--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
--  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
--  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice (including the next paragraph) shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
--  THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.
--------------------------------------------------

pragma Check_Float_Overflow;

with Prunt.Motion_Planner.Stereographic_Curves.Geometry;

package body Prunt.Motion_Planner.Planner.Corner_Blender is

   pragma Unsuppress (Overflow_Check);
   pragma Unsuppress (Range_Check);
   pragma Unsuppress (Division_Check);

   pragma Extensions_Allowed (On);
   pragma Assert (Dimensionless'Machine_Rounds);

   use Prunt.Motion_Planner.Stereographic_Curves.Geometry;

   procedure Run
     (Block       : aliased in out Execution_Block;
      Motor_Map   : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace   : not null access Planning_Workspace;
      Force_Stops : Boolean := False)
   is
      Original_E              : Block_Segment_Lengths renames
        Workspace.Original_Extrusion_Positions (Block.Corners'Range);
      Spatial_Map             : Prunt.Motion_Planner.Planner.Motor_Position_Map := Motor_Map;
      Extrusion_Reduction     : Length := 0.0 * mm;
      Reduction_Roundoff      : Length := 0.0 * mm;
      Stop_At_Density_Changes : constant Boolean :=
        Block.Params.Extrusion_Cornering.Velocity_Change_Max = 0.0 * mm / s;
   begin
      for I in Block.Corners'Range loop
         Original_E (I) := Block.Corners (I) (E_Axis);
      end loop;
      --  Extrusion_Density_Normalizer has already assigned the authoritative densities. In particular, a geometry
      --  retry must retain their exact values instead of reconstructing nearly equal ratios from the corners.
      for I in Block.Corners'Range loop
         Block.Corners (I) (E_Axis) := 0.0 * mm;
      end loop;
      for M in Motor_Name loop
         Spatial_Map (E_Axis, M) := 0.0 / mm;
      end loop;
      if Force_Stops then
         for I in Block.Corners'Range loop
            Block.Corner_Transitions (I) := To_Evaluator (Stop_At (Block.Corners (I)));
            Workspace.Corner_Derivative_Bounds (I) := Derivative_Bounds (Block.Corner_Transitions (I));
         end loop;
         for I in Block.Primitives'Range loop
            Block.Primitive_Start_Distances (I) := 0.0 * mm;
            Block.Primitive_Distances (I) := Primitive_Length (Block'Access, I);
         end loop;
      else
         Runner.Run (Block, Spatial_Map, Workspace, Stop_At_Density_Changes);
      end if;
      for I in Block.Corners'Range loop
         Block.Corners (I) (E_Axis) := Original_E (I);
      end loop;
      for I in Block.Primitives'Range loop
         if Block.Primitive_Distances (I) = 0.0 * mm
           and then Policy (Block.Corner_Transitions (I - 1)) = Hard_Stop
           and then Policy (Block.Corner_Transitions (I)) = Hard_Stop
         then
            Block.Primitive_Distances (I) := Primitive_Length (Block'Access, I);
         end if;
      end loop;
      Block.Extrusion_Reference_Positions (1) := Block.Corners (1) (E_Axis);
      for I in Block.Primitives'Range loop
         declare
            Reduction     : constant Length :=
              (if Force_Stops
               then 0.0 * mm
               else
                 Block.Extrusion_Densities (I)
                 * (Workspace.Unblended_Segment_Lengths (I) - Segment_Total_Distance (Block'Access, I)));
            Adjusted      : constant Length := Reduction - Reduction_Roundoff;
            New_Reduction : constant Length := Extrusion_Reduction + Adjusted;
         begin
            Reduction_Roundoff := (New_Reduction - Extrusion_Reduction) - Adjusted;
            Extrusion_Reduction := New_Reduction;
            Block.Extrusion_Reference_Positions (I) :=
              Workspace.Unblended_Extrusion_Positions (I) - Extrusion_Reduction;
         end;
      end loop;

      --  Bounds bypasses are used by homing. If all commanded E coordinates are in bounds, shortening must not
      --  create an out-of-bounds E reference (for example printing followed by a retraction to the lower bound).
      --  Restore the original path with stops if shortening would make the corrected reference infeasible.
      if (for all I in Block.Corners'Range =>
            Block.Corners (I) (E_Axis) in Block.Params.Bounds.Lower_E .. Block.Params.Bounds.Upper_E)
      then
         for I in Block.Extrusion_Reference_Positions'Range loop
            if Block.Extrusion_Reference_Positions (I)
               not in Block.Params.Bounds.Lower_E .. Block.Params.Bounds.Upper_E
            then
               --  The normalized unshortened reference stays between each run's commanded endpoints.
               --  The recursive call is bounded: Force_Stops uses that reference without shortening it.
               Run (Block, Motor_Map, Workspace, Force_Stops => True);
               return;
            end if;
         end loop;
      end if;
      --  Early_Kinematic_Limiter sizes the transitions once the feedrates are known.
      Block.Extrusion_Transitions := [others => <>];
   exception
      when others =>
         for I in Block.Corners'Range loop
            Block.Corners (I) (E_Axis) := Original_E (I);
         end loop;
         raise;
   end Run;

   protected body Runner is
      procedure Run
        (Block                   : aliased in out Execution_Block;
         Motor_Map               : Prunt.Motion_Planner.Planner.Motor_Position_Map;
         Workspace               : not null access Planning_Workspace;
         Stop_At_Density_Changes : Boolean)
      is
         Transition_Samples   : constant Positive := 65;
         Coarse_Sample_Span   : constant Positive := 16;
         --  Validation first examines every sixteenth sample, where the chord between two neighbouring samples is
         --  compared with the limits, and evaluates the samples in between only where that leaves a limit unproved.
         --  The span is a power of two because an unproved interval is halved.
         Primitive_Samples    : constant Positive := 17;
         Max_Repair_Attempts  : constant Positive := 3;
         Repair_Shrink_Factor : constant Dimensionless := 0.55;
         Repair_Safety        : constant Dimensionless := 0.95;
         --  A failed transition is rebuilt at this fraction of the width at which its measured excess predicts that
         --  it meets the limit. Deviation is proportional to width for a corner between two lines.
         Width_Model_Safety   : constant Dimensionless := 0.97;
         --  The first width of a corner aims at this fraction of the modelled limit.
         Helix_Snap_Allowance : constant Length := 2.0E-6 * mm;
         --  A helix is accepted with endpoint radii up to 1.0E-6 mm apart and returns its commanded endpoints
         --  exactly, so an evaluated point can be that far from the ideal helix. Twice that covers the step from an
         --  evaluated point to the ideal helix and back from it to the path as evaluated.
         Motor_Error_Fraction : constant Dimensionless := 0.25;
         Straight_Tolerance   : constant Dimensionless := 1.0E-9;

         function Extrusion_Stop (I : Corners_Index) return Boolean
         is (Stop_At_Density_Changes
             and then I > Block.Corners'First
             and then I < Block.Corners'Last
             and then Block.Extrusion_Densities (I) /= Block.Extrusion_Densities (I + 1)
             and then
               (Block.Params.Extrusion_Cornering.Kind = Instantaneous_Velocity_Change
                or else not Density_Change_Is_Negligible (Block'Access, I))
             and then not Smooths_Density_Change (Block'Access, I));
         --  A change that may be smoothed keeps its curve; Extrusion_Junction_Ceiling stops it if no transition fits.
         --  Smooth_Deviation lets roundoff step E's velocity, as Extrusion_Junction_Ceiling does.

         type Blend_Settings is record
            Deviation   : Axial_Deviation_Limits := [others => 0.0 * mm];
            Miss        : Length := 0.0 * mm;
            Circularity : Dimensionless := 0.0;
            Bias        : Dimensionless := 0.0;
            Trim_Max    : Length := 0.0 * mm;
         end record;

         function Settings_For_Block return Blend_Settings;

         function Settings_For_Block return Blend_Settings is
         begin
            case Block.Params.Cornering.Kind is
               when Stereographic =>
                  return
                    (Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes,
                     Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max,
                     Block.Params.Cornering.Stereographic_Params.Circularity,
                     Block.Params.Cornering.Stereographic_Params.Shape_Bias,
                     0.0 * mm);

               when Circular      =>
                  return
                    (Block.Params.Cornering.Circular_Params.Axial_Deviation_Maxes,
                     Block.Params.Cornering.Circular_Params.Corner_Miss_Distance_Max,
                     0.0,
                     0.0,
                     0.0 * mm);

               when Biarc         =>
                  return
                    (Block.Params.Cornering.Biarc_Params.Axial_Deviation_Maxes,
                     Block.Params.Cornering.Biarc_Params.Corner_Miss_Distance_Max,
                     0.0,
                     Block.Params.Cornering.Biarc_Params.Shape_Bias,
                     Block.Params.Cornering.Biarc_Params.Trim_Max);

               when Sharp_SCV     =>
                  return (others => <>);
            end case;
         end Settings_For_Block;

         Settings : constant Blend_Settings := Settings_For_Block;
         type Transition_Points is array (Natural range 0 .. Transition_Samples - 1) of Position;
         type Transition_Flags is array (Natural range 0 .. Transition_Samples - 1) of Boolean;

         type Transition_Sampler is record
            Points : Transition_Points := [others => [others => 0.0 * mm]];
            Known  : Transition_Flags := [others => False];
         end record;
         --  Points of one transition at equally spaced arc distances, evaluated on first use.

         type Section_Distance is record
            Distance : Length := Length'Last;
            --  Upper bound on the distance from the examined point to the reference point at Foot.
            Foot     : Length := 0.0 * mm;
            --  Path distance of that reference point along its primitive.
         end record;

         type Reference_Section is record
            Finishing_Corner : Finishing_Corners_Index := Finishing_Corners_Index'First;
            Primitive        : Derived_Path_Primitive;
            Start_Distance   : Length := 0.0 * mm;
            End_Distance     : Length := 0.0 * mm;
            Start_Point      : Position := [others => 0.0 * mm];
            End_Point        : Position := [others => 0.0 * mm];
            Bend             : Curvature := 0.0 / mm;
            --  Upper bound on the second path-distance derivative of the primitive.
            Allowance        : Length := 0.0 * mm;
            --  Upper bound on the distance from an evaluated point of the primitive to the ideal primitive.
         end record;
         --  The part of one commanded primitive which a transition replaces.

         function Required_Stop (I : Corners_Index) return Boolean
         is (I = Block.Corners'First
             or else I = Block.Corners'Last
             or else Extrusion_Stop (I)
             or else Primitive_Length (Block'Access, I) <= 0.0 * mm
             or else Primitive_Length (Block'Access, I + 1) <= 0.0 * mm
             or else Block.Corner_Dwell_Times (I) /= 0.0 * s);

         function Clamp_Unit_Dot (Value : Dimensionless) return Dimensionless;
         function Add_Length_Upper (Left, Right : Length) return Length;
         function Chord_Sag_Upper (Span : Length; Bend : Curvature) return Length;
         --  Upper bound on Span² · Bend / 8: the most by which a curve can leave the chord between two of its points
         --  whose parameters are Span apart, when its second derivative in that parameter is at most Bend.
         function Transition_Position_Error return Length;
         function Divide_Length_Upper (Value : Length; Divisor : Dimensionless) return Length;
         function Effective_Axial_Deviation
           (Incoming_Tangent, Outgoing_Tangent : Position_Scale; Reserve : Length := 0.0 * mm) return Length;
         --  The offset from the corner along the turn direction which reaches an axial limit less Reserve.
         function Executed_Bend_Upper (Curve : Corner_Transition) return Curvature;
         function Executed_Speed_Upper (Curve : Corner_Transition) return Dimensionless;
         function Flat_Blend_Miss_Ratio (Cos_Half_Theta : Dimensionless) return Dimensionless;
         --  The distance from the corner to the nearest point of an equal-trim Stereographic blend between two lines,
         --  divided by that trim.
         function Hard_Anchor (I : Corners_Index) return Boolean;
         function Is_Passthrough_Corner (I : Corners_Index) return Boolean;
         function Modelled_Base_Width
           (I                                  : Corners_Index;
            Incoming_Tangent, Outgoing_Tangent : Position_Scale;
            Cos_Half_Theta, Trim_Balance       : Dimensionless) return Length;
         --  The base width at which the blend between two lines with these tangents is predicted to reach the first
         --  of its deviation and miss limits, less a safety margin. Trim_Balance is the geometric mean of the two
         --  trim multipliers. This only chooses the first attempt: Validate_Transition decides what is retained.
         function Multiply_Length_Upper (Value : Length; Factor : Dimensionless) return Length;
         function Point_Distance_Upper (Left, Right : Position) return Length
         renames Prunt.Motion_Planner.Stereographic_Curves.Geometry.Point_To_Point_Distance_Upper;
         function Predicted_Shrink (Limit, Achieved : Length; Fallback, Safety : Dimensionless) return Dimensionless;
         function Round_Down (Value : Dimensionless) return Dimensionless
         renames Prunt.Motion_Planner.Stereographic_Curves.Geometry.Round_Scalar_Down;
         function Round_Up (Value : Dimensionless) return Dimensionless
         renames Prunt.Motion_Planner.Stereographic_Curves.Geometry.Round_Scalar_Up;
         function Round_Length_Down_Nonnegative (Value : Length) return Length
         renames Prunt.Motion_Planner.Stereographic_Curves.Geometry.Round_Down_Nonnegative;
         function Round_Length_Up (Value : Length) return Length
         renames Prunt.Motion_Planner.Stereographic_Curves.Geometry.Round_Up;

         function Distance_To_Section (Point : Position; Section : Reference_Section) return Section_Distance;
         --  The distance from Point to one evaluated point of Section, chosen close to the nearest one. Whichever
         --  point the search selects, its distance is an upper bound on the distance from Point to Section.
         function Line_Corner_Axial_Deviation
           (Curve          : Corner_Transition;
            Sampler        : in out Transition_Sampler;
            Start_Point    : Position;
            Corner_Point   : Position;
            Finish_Point   : Position;
            Limits         : Axial_Deviation_Limits;
            Position_Error : Length) return Axial_Deviation_Check;
         function Primitive_Deviation_Upper_Bound
           (Curve              : Corner_Transition;
            Sampler            : in out Transition_Sampler;
            Incoming, Outgoing : Reference_Section;
            Position_Error     : Length;
            Limit              : Length) return Length;
         --  Upper bound on the distance from the executed curve to the nearer of the two sections. The bound is
         --  refined only as far as proving that it is within Limit requires.
         function Sample
           (Curve : Corner_Transition; Sampler : in out Transition_Sampler; Index : Natural) return Position;
         function Sample_Span_Upper (Curve : Corner_Transition; Span : Positive) return Length;
         --  Upper bound on the arc distance between two samples Span indices apart, at their evaluated positions.
         function Section_Of
           (Finishing_Corner : Finishing_Corners_Index;
            Primitive        : Derived_Path_Primitive;
            Start_Distance   : Length;
            Distance         : Length) return Reference_Section;
         function Validate_Transition
           (I                  : Corners_Index;
            Curve              : Corner_Transition;
            Incoming, Outgoing : Derived_Path_Primitive;
            Trim_In            : Length;
            Trim_Out           : Length;
            Position_Error     : Length) return Corner_Transition_Attempt;
         function Try_Build_Corner (I : Corners_Index) return Corner_Transition_Attempt;
         function Zero_Corner_Transition (I : Corners_Index) return Corner_Transition;
         procedure Allocate_Segment_Trims;
         procedure Assign_Target_Widths;
         procedure Clear_Cached_Attempt (I : Corners_Index);
         procedure Store_Sharp_Transitions;
         procedure Store_Transition (I : Corners_Index; Transition : Corner_Transition);
         procedure Store_Final_Transitions;

         function Clamp_Unit_Dot (Value : Dimensionless) return Dimensionless is
         begin
            return Dimensionless'Max (-1.0, Dimensionless'Min (1.0, Value));
         end Clamp_Unit_Dot;

         function Add_Length_Upper (Left, Right : Length) return Length is
         begin
            if Left < 0.0 * mm or else Right < 0.0 * mm or else Left >= Length'Last or else Right >= Length'Last then
               return Length'Last;
            elsif Left = 0.0 * mm then
               return Right;
            elsif Right = 0.0 * mm then
               return Left;
            elsif Right > Length'Last - Left then
               return Length'Last;
            else
               return Round_Length_Up (Left + Right);
            end if;
         exception
            when Constraint_Error =>
               return Length'Last;
         end Add_Length_Upper;

         function Chord_Sag_Upper (Span : Length; Bend : Curvature) return Length is
         begin
            if not (Span >= 0.0 * mm and then Span < Length'Last)
              or else not (Bend >= 0.0 / mm and then Bend < Curvature'Last)
            then
               return Length'Last;
            elsif Span = 0.0 * mm or else Bend = 0.0 / mm then
               return 0.0 * mm;
            else
               --  Three rounded operations on positive values; the factor repays them many times over.
               return Round_Length_Up ((Span * Span) * Bend * (0.125 * (1.0 + 2.0**(-40))));
            end if;
         exception
            when Constraint_Error =>
               return Length'Last;
         end Chord_Sag_Upper;

         function Divide_Length_Upper (Value : Length; Divisor : Dimensionless) return Length is
         begin
            if Value < 0.0 * mm or else Divisor <= 0.0 then
               return Length'Last;
            elsif Value = 0.0 * mm then
               return 0.0 * mm;
            else
               return Round_Length_Up (Value / Divisor);
            end if;
         exception
            when Constraint_Error =>
               return Length'Last;
         end Divide_Length_Upper;

         function Executed_Bend_Upper (Curve : Corner_Transition) return Curvature is
            Bounds     : constant Unit_Speed_Axial_Acceleration_Bounds := Derivative_Bounds (Curve).Acceleration;
            Square_Sum : Dimensionless := 0.0;
         begin
            for Axis in Axis_Name loop
               if Bounds (Axis) < 0.0 / mm or else Bounds (Axis) >= Curvature'Last then
                  return Curvature'Last;
               end if;
               Square_Sum := Round_Up (Square_Sum + Round_Up (Dimensionless (Bounds (Axis) * mm)**2));
            end loop;
            return Round_Curvature_Up (Round_Up (Dimensionless_Math.Sqrt (Square_Sum)) / mm);
         exception
            when Constraint_Error =>
               return Curvature'Last;
         end Executed_Bend_Upper;

         function Executed_Speed_Upper (Curve : Corner_Transition) return Dimensionless is
            Bounds     : constant Unit_Speed_Axial_Velocity_Bounds := Derivative_Bounds (Curve).Velocity;
            Square_Sum : Dimensionless := 0.0;
         begin
            for Axis in Axis_Name loop
               if Bounds (Axis) < 0.0 or else Bounds (Axis) >= Dimensionless'Last then
                  return Dimensionless'Last;
               end if;
               Square_Sum := Round_Up (Square_Sum + Round_Up (Bounds (Axis)**2));
            end loop;
            return Round_Up (Dimensionless_Math.Sqrt (Square_Sum));
         exception
            when Constraint_Error =>
               return Dimensionless'Last;
         end Executed_Speed_Upper;

         function Effective_Axial_Deviation
           (Incoming_Tangent, Outgoing_Tangent : Position_Scale; Reserve : Length := 0.0 * mm) return Length
         is
            Turn        : Position_Scale;
            Turn_Norm   : Dimensionless := 0.0;
            Result      : Length := Length'Last;
            Constrained : Boolean := False;
         begin
            Turn := Incoming_Tangent - Outgoing_Tangent;
            for Axis in Axis_Name loop
               Turn_Norm := Turn_Norm + Turn (Axis)**2;
            end loop;
            Turn_Norm := Dimensionless_Math.Sqrt (Turn_Norm);
            if Turn_Norm <= 128.0 * Dimensionless'Model_Epsilon then
               return Length'Last;
            end if;
            for Axis in Axis_Name when Axis /= E_Axis loop
               if abs Turn (Axis) > 128.0 * Dimensionless'Model_Epsilon * Turn_Norm then
                  Constrained := True;
                  Result :=
                    Length'Min
                      (Result,
                       (if Reserve > 0.0 * mm
                        then Length'Max (0.0 * mm, Settings.Deviation (Axis) - Reserve)
                        else Settings.Deviation (Axis))
                       * (Turn_Norm / abs Turn (Axis)));
               end if;
            end loop;
            return (if Constrained then Result else Length'Last);
         exception
            when Constraint_Error =>
               return 0.0 * mm;
         end Effective_Axial_Deviation;

         function Multiply_Length_Upper (Value : Length; Factor : Dimensionless) return Length is
         begin
            if Value < 0.0 * mm or else Factor < 0.0 then
               return Length'Last;
            elsif Value = 0.0 * mm or else Factor = 0.0 then
               return 0.0 * mm;
            else
               return Round_Length_Up (Value * Factor);
            end if;
         exception
            when Constraint_Error =>
               return Length'Last;
         end Multiply_Length_Upper;

         function Transition_Position_Error return Length is
            Result             : Length := Corner_Transition_Max_Computational_Error;
            Has_Relevant_Motor : Boolean := False;
         begin
            for M in Motor_Name loop
               declare
                  Norm_Squared_Upper : Curvature_To_2 := 0.0 / mm**2;
               begin
                  for A in Axis_Name loop
                     if Motor_Map (A, M) /= 0.0 / mm then
                        Has_Relevant_Motor := True;
                        declare
                           Coefficient_Upper : constant Curvature := Round_Curvature_Up (abs Motor_Map (A, M));
                           Square_Upper      : constant Curvature_To_2 :=
                             Curvature_To_2'Adjacent (Coefficient_Upper**2, Curvature_To_2'Last);
                        begin
                           if Square_Upper > Curvature_To_2'Last - Norm_Squared_Upper then
                              return 0.0 * mm;
                           end if;
                           Norm_Squared_Upper :=
                             Curvature_To_2'Adjacent (Norm_Squared_Upper + Square_Upper, Curvature_To_2'Last);
                        end;
                     end if;
                  end loop;

                  if Norm_Squared_Upper > 0.0 / mm**2 then
                     declare
                        Norm_Upper : constant Curvature := Round_Curvature_Up (Norm_Squared_Upper**(1 / 2));
                        Allowable  : constant Length := Motor_Error_Fraction / Norm_Upper;
                     begin
                        Result := Length'Min (Result, Round_Length_Down_Nonnegative (Allowable));
                     end;
                  end if;
               end;
            end loop;

            --  A Euclidean scaled-coordinate error no greater than Result changes every relevant motor coordinate by
            --  at most one quarter of a microstep.  The generic tolerance remains an independent upper cap and is
            --  also the fallback for a deliberately motorless planner instance.
            return (if Has_Relevant_Motor then Result else Corner_Transition_Max_Computational_Error);
         exception
            when Constraint_Error =>
               return 0.0 * mm;
         end Transition_Position_Error;

         Position_Error_Limit : constant Length := Transition_Position_Error;
         --  The motor map is fixed for the block, so every corner shares this evaluator error budget.

         function Predicted_Shrink (Limit, Achieved : Length; Fallback, Safety : Dimensionless) return Dimensionless is
         begin
            if Limit > 0.0 * mm and then Achieved > Limit and then Achieved < 1.0E80 * mm then
               return Dimensionless'Max (0.1, Safety * (Limit / Achieved));
            else
               return Fallback;
            end if;
         end Predicted_Shrink;

         function Flat_Blend_Miss_Ratio (Cos_Half_Theta : Dimensionless) return Dimensionless is
            --  With flat endpoint jets and equal trims the chart is t·(2·σ(U) - 1), where t = tan (θ/4) and σ is
            --  the degree-seven smoothstep, and no closure mode is needed. Its tangent makes the angle φ with the
            --  chord, where cos φ = (1 - y²)/(1 + y²) and sin φ = 2·y/(1 + y²), so integrating both over half the
            --  curve locates its middle, which is its nearest point to the corner:
            --
            --     miss/trim = sin (θ/2) - cos (θ/2) · ∫ sin φ dU / ∫ cos φ dU   over [0, 1/2].
            --
            --  Both integrands are smooth and an eight-point Gauss-Legendre rule resolves the ratio to seven digits.
            Nodes   : constant array (1 .. 4) of Dimensionless :=
              [0.183_434_642_495_649_8, 0.525_532_409_916_329_0, 0.796_666_477_413_626_7, 0.960_289_856_497_536_3];
            Weights : constant array (1 .. 4) of Dimensionless :=
              [0.362_683_783_378_362_0, 0.313_706_645_877_887_3, 0.222_381_034_453_374_5, 0.101_228_536_290_376_3];
            Cosine  : constant Dimensionless := Dimensionless'Max (0.0, Dimensionless'Min (1.0, Cos_Half_Theta));
            Sine    : constant Dimensionless := Dimensionless_Math.Sqrt (1.0 - Cosine * Cosine);
            Chart   : constant Dimensionless := Sine / (1.0 + Cosine);
            Rise    : Dimensionless := 0.0;
            Advance : Dimensionless := 0.0;
         begin
            for Index in Nodes'Range loop
               for Upper_Half in Boolean loop
                  declare
                     U    : constant Dimensionless := 0.25 + (if Upper_Half then 0.25 else -0.25) * Nodes (Index);
                     Step : constant Dimensionless := U**4 * (35.0 + U * (-84.0 + U * (70.0 - 20.0 * U)));
                     Y    : constant Dimensionless := Chart * (1.0 - 2.0 * Step);
                  begin
                     Rise := Rise + Weights (Index) * (2.0 * Y / (1.0 + Y * Y));
                     Advance := Advance + Weights (Index) * ((1.0 - Y * Y) / (1.0 + Y * Y));
                  end;
               end loop;
            end loop;
            if Advance > 0.0 then
               return Dimensionless'Max (0.0, Sine - Cosine * (Rise / Advance));
            else
               return 1.0;
            end if;
         exception
            when Constraint_Error =>
               return 1.0;
         end Flat_Blend_Miss_Ratio;

         function Modelled_Base_Width
           (I                                  : Corners_Index;
            Incoming_Tangent, Outgoing_Tangent : Position_Scale;
            Cos_Half_Theta, Trim_Balance       : Dimensionless) return Length
         is
            Reserve   : constant Length := Multiply_Length_Upper (Position_Error_Limit, 2.0);
            Line_Line : constant Boolean :=
              Block.Primitives (I).Kind = Line_Primitive_Kind
              and then Block.Primitives (I + 1).Kind = Line_Primitive_Kind;
            Ratio     : constant Dimensionless := Flat_Blend_Miss_Ratio (Cos_Half_Theta) * Trim_Balance;
            Offset    : Length;
            --  The largest admissible distance from the corner to the nearest point of the curve.
         begin
            if Line_Line then
               --  That point is offset from the corner along the turn direction and has to stay within the axial
               --  limits of the corner itself, which is conservative: a point further along either line can be
               --  nearer on the limiting axis.
               Offset := Effective_Axial_Deviation (Incoming_Tangent, Outgoing_Tangent, Reserve);
            else
               --  A corner with a helix is held to the tightest axial limit as a Euclidean distance from the
               --  primitives, which the nearest point exceeds first, at Offset · cos (θ/2).
               declare
                  Tightest : Length := Length'Last;
               begin
                  for Axis in Axis_Name when Axis /= E_Axis loop
                     if Axis in X_Axis | Y_Axis
                       or else Block.Corners (I - 1) (Axis) /= Block.Corners (I) (Axis)
                       or else Block.Corners (I + 1) (Axis) /= Block.Corners (I) (Axis)
                     then
                        Tightest := Length'Min (Tightest, Settings.Deviation (Axis));
                     end if;
                  end loop;
                  if Cos_Half_Theta > 0.0 and then Tightest < Length'Last then
                     Offset := Length'Max (0.0 * mm, Tightest - Reserve) / Cos_Half_Theta;
                  else
                     Offset := Length'Last;
                  end if;
               end;
            end if;
            Offset := Length'Min (Offset, Length'Max (0.0 * mm, Settings.Miss - Position_Error_Limit));
            if Ratio > 0.0 and then Offset < Length'Last then
               return Width_Model_Safety * Offset / Ratio;
            else
               return Length'Last;
            end if;
         exception
            when Constraint_Error =>
               return Length'Last;
         end Modelled_Base_Width;

         function Hard_Anchor (I : Corners_Index) return Boolean is
            Any_Deviation_Allowed : Boolean := False;
         begin
            for Axis in Axis_Name loop
               if Axis /= E_Axis then
                  Any_Deviation_Allowed := Any_Deviation_Allowed or else Settings.Deviation (Axis) > 0.0 * mm;
               end if;
            end loop;
            if Required_Stop (I) then
               return True;
            elsif Is_Passthrough_Corner (I) then
               return True;
            elsif not Any_Deviation_Allowed then
               return True;
            elsif Block.Params.Cornering.Kind = Circular
              and then
                (Block.Primitives (I).Kind /= Line_Primitive_Kind
                 or else Block.Primitives (I + 1).Kind /= Line_Primitive_Kind)
            then
               return True;
            else
               declare
                  Incoming_Tangent : constant Position_Scale :=
                    Primitive_Direction_At_Distance (Block'Access, I, Primitive_Length (Block'Access, I));
                  Outgoing_Tangent : constant Position_Scale :=
                    Primitive_Direction_At_Distance (Block'Access, I + 1, 0.0 * mm);
                  Dot_Tangents     : constant Dimensionless :=
                    Clamp_Unit_Dot (Dot (Incoming_Tangent, Outgoing_Tangent));
                  Secondary_Sine   : constant Dimensionless := ((1.0 - Dot_Tangents) / 2.0)**(1 / 2);
               begin
                  return
                    Angle_Elementary_Functions.Sin (90.0 * deg - 0.5 * Corner_Transition_Min_Corner_Angle)
                    < Secondary_Sine;
               end;
            end if;
         end Hard_Anchor;

         function Is_Passthrough_Corner (I : Corners_Index) return Boolean is
            function Matches (Left, Right : Position_Scale) return Boolean;

            function Matches (Left, Right : Position_Scale) return Boolean is
               Magnitude : Dimensionless := 0.0;
            begin
               for Axis in Axis_Name loop
                  Magnitude := Dimensionless'Max (Magnitude, Dimensionless'Max (abs Left (Axis), abs Right (Axis)));
               end loop;
               --  A relative allowance absorbs evaluation roundoff on two pieces of the same helix.
               --  In particular, a zero derivative must not match a small but nonzero curvature.
               return
                 (for all Axis in Axis_Name =>
                    abs (Left (Axis) - Right (Axis)) <= 128.0 * Dimensionless'Model_Epsilon * Magnitude);
            end Matches;
         begin
            if Required_Stop (I) then
               return False;
            end if;

            if 1.0
              - Clamp_Unit_Dot
                  (Dot
                     (Primitive_Direction_At_Distance (Block'Access, I, Primitive_Length (Block'Access, I)),
                      Primitive_Direction_At_Distance (Block'Access, I + 1, 0.0 * mm)))
              > Straight_Tolerance
            then
               return False;
            elsif Block.Params.Cornering.Kind /= Stereographic then
               return True;
            end if;

            --  Equal tangents alone only establish C1 continuity. The bounded-crackle family also needs
            --  matching distance derivatives through order four; otherwise it must blend or stop.
            declare
               Incoming : constant Endpoint_Tangent_Jet :=
                 Primitive_Derivative_Jets_At_Distance (Block'Access, I, Primitive_Length (Block'Access, I));
               Outgoing : constant Endpoint_Tangent_Jet :=
                 Primitive_Derivative_Jets_At_Distance (Block'Access, I + 1, 0.0 * mm);
            begin
               return
                 Matches
                   ([for Axis in Axis_Name => Incoming.Tangent_Derivative_1 (Axis) * mm],
                    [for Axis in Axis_Name => Outgoing.Tangent_Derivative_1 (Axis) * mm])
                 and then
                   Matches
                     ([for Axis in Axis_Name => Incoming.Tangent_Derivative_2 (Axis) * mm**2],
                      [for Axis in Axis_Name => Outgoing.Tangent_Derivative_2 (Axis) * mm**2])
                 and then
                   Matches
                     ([for Axis in Axis_Name => Incoming.Tangent_Derivative_3 (Axis) * mm**3],
                      [for Axis in Axis_Name => Outgoing.Tangent_Derivative_3 (Axis) * mm**3]);
            end;
         exception
            when Constraint_Error =>
               return False;
         end Is_Passthrough_Corner;

         function Zero_Corner_Transition (I : Corners_Index) return Corner_Transition is
         begin
            return Stop_At (Block.Corners (I));
         end Zero_Corner_Transition;

         procedure Store_Transition (I : Corners_Index; Transition : Corner_Transition) is
         begin
            Block.Corner_Transitions (I) := To_Evaluator (Transition);
            Workspace.Corner_Derivative_Bounds (I) := Derivative_Bounds (Transition);
         end Store_Transition;

         procedure Store_Sharp_Transitions is
         begin
            for I in Block.Corners'Range loop
               if Required_Stop (I) then
                  Store_Transition (I, Stop_At (Block.Corners (I)));
               else
                  declare
                     Incoming_Tangent : constant Position_Scale :=
                       Primitive_Direction_At_Distance (Block'Access, I, Primitive_Length (Block'Access, I));
                     Outgoing_Tangent : constant Position_Scale :=
                       Primitive_Direction_At_Distance (Block'Access, I + 1, 0.0 * mm);
                     Result           : constant SCV_Result :=
                       Compute_Sharp_SCV_Limit
                         (Incoming_Tangent,
                          Outgoing_Tangent,
                          Block.Params.Cornering.Sharp_SCV_Params.Square_Corner_Velocity,
                          Block.Params.Ignore_E_In_XYZE);
                  begin
                     case Result.Status is
                        when SCV_Passthrough                                          =>
                           Store_Transition (I, Passthrough_At (Block.Corners (I)));

                        when SCV_Success                                              =>
                           Store_Transition (I, Sharp_At (Block.Corners (I), Result.Velocity_Limit));

                        when SCV_Reversal_Stop | SCV_Mixed_Pure_E | SCV_Invalid_Input =>
                           Store_Transition (I, Stop_At (Block.Corners (I)));
                     end case;
                  end;
               end if;
            end loop;

            for I in Block.Primitives'Range loop
               Block.Primitive_Start_Distances (I) := 0.0 * mm;
               Block.Primitive_Distances (I) := Primitive_Length (Block'Access, I);
            end loop;
         end Store_Sharp_Transitions;

         procedure Clear_Cached_Attempt (I : Corners_Index) is
         begin
            Cached_Attempts (I) := (others => <>);
            Cached_Attempt_Valid (I) := False;
         end Clear_Cached_Attempt;

         function Sample
           (Curve : Corner_Transition; Sampler : in out Transition_Sampler; Index : Natural) return Position is
         begin
            if not Sampler.Known (Index) then
               Sampler.Points (Index) :=
                 Point_At_Parameter
                   (Curve, Transition_Parameter (Dimensionless (Index) / Dimensionless (Transition_Samples - 1)));
               Sampler.Known (Index) := True;
            end if;
            return Sampler.Points (Index);
         end Sample;

         function Sample_Span_Upper (Curve : Corner_Transition; Span : Positive) return Length is
         begin
            --  A sample is evaluated within a few units in the last place of its nominal parameter, which the
            --  second factor covers.
            return
              Multiply_Length_Upper
                (Multiply_Length_Upper
                   (Arc_Length (Curve), Round_Up (Dimensionless (Span) / Dimensionless (Transition_Samples - 1))),
                 1.0 + 2.0**(-40));
         end Sample_Span_Upper;

         function Section_Of
           (Finishing_Corner : Finishing_Corners_Index;
            Primitive        : Derived_Path_Primitive;
            Start_Distance   : Length;
            Distance         : Length) return Reference_Section
         is
            Result : Reference_Section;
         begin
            Result.Finishing_Corner := Finishing_Corner;
            Result.Primitive := Primitive;
            Result.Start_Distance := Start_Distance;
            Result.End_Distance := Start_Distance + Length'Max (0.0 * mm, Distance);
            Result.Start_Point :=
              Derived_Primitive_Point_At_Distance (Block'Access, Finishing_Corner, Primitive, Result.Start_Distance);
            Result.End_Point :=
              Derived_Primitive_Point_At_Distance (Block'Access, Finishing_Corner, Primitive, Result.End_Distance);
            case Primitive.Kind is
               when Line_Primitive_Kind  =>
                  null;

               when Helix_Primitive_Kind =>
                  --  The allowance covers the distance between an evaluated point and the ideal helix in either
                  --  direction.
                  Result.Allowance := Helix_Snap_Allowance;
                  begin
                     --  Path distance turns the phase at 1/Length_Per_Radian, so the second derivative is the
                     --  centripetal Radius/Length_Per_Radian²; the rise is linear in path distance.
                     Result.Bend :=
                       Round_Curvature_Up
                         (Primitive.Radius
                          / (Primitive.Length_Per_Radian * Primitive.Length_Per_Radian)
                          * (1.0 + 2.0**(-40)));
                  exception
                     when Constraint_Error =>
                        --  Without a bend the chord certificate is unavailable and the step certificate decides.
                        Result.Bend := Curvature'Last;
                  end;
            end case;
            return Result;
         end Section_Of;

         function Distance_To_Section (Point : Position; Section : Reference_Section) return Section_Distance is
            --  Each validation attempt examines many points against the same two sections, so the section ends are
            --  evaluated once in Section_Of rather than once per point.
            Distance : constant Length := Section.End_Distance - Section.Start_Distance;
            To_Start : constant Length := Point_Distance_Upper (Point, Section.Start_Point);

            procedure Line_Candidate (Candidate : out Length; Found : out Boolean);
            procedure Helix_Candidate (Candidate : out Length; Found : out Boolean);
            --  Propose the path distance of an interior point of the section near the closest one.

            procedure Line_Candidate (Candidate : out Length; Found : out Boolean) is
               Offset : constant Position_Offset := Section.End_Point - Section.Start_Point;
               Den    : constant Area := Dot (Offset, Offset);
            begin
               Candidate := Section.Start_Distance;
               Found := Den > 0.0 * mm**2;
               if Found then
                  --  The rounded projection parameter is used only to select one actual point in the reference
                  --  primitive section.  Its distance is therefore an upper bound on the section minimum even if
                  --  the projection arithmetic itself is poorly conditioned.
                  Candidate :=
                    Length'Max
                      (Section.Start_Distance,
                       Length'Min
                         (Section.End_Distance,
                          Section.Start_Distance
                          + Dimensionless'Max
                              (0.0, Dimensionless'Min (1.0, Dot (Point - Section.Start_Point, Offset) / Den))
                            * Distance));
               end if;
            end Line_Candidate;

            procedure Helix_Candidate (Candidate : out Length; Found : out Boolean) is
               Primitive    : Derived_Path_Primitive renames Section.Primitive;
               Center       : constant Position := Block.Primitives (Section.Finishing_Corner).Center;
               Sign         : constant Dimensionless := (if Primitive.Theta_Delta < 0.0 then -1.0 else 1.0);
               --  Travel is the phase swept from the primitive's start, so the axial offset at Travel is
               --  Rise * Travel. Rise has no X or Y component.
               Rise         : constant Position_Offset := Primitive.Axial_Per_Phase * Sign;
               Rise_Squared : constant Area := Dot (Rise, Rise);
               Axial_Target : constant Area := Dot (Rise, Point - Block.Corners (Section.Finishing_Corner - 1));
               DX           : constant Length := Point (X_Axis) - Center (X_Axis);
               DY           : constant Length := Point (Y_Axis) - Center (Y_Axis);
               First_Travel : constant Dimensionless := Section.Start_Distance / Primitive.Length_Per_Radian;
               Last_Travel  : constant Dimensionless := Section.End_Distance / Primitive.Length_Per_Radian;
               Two_Pi       : constant Dimensionless := 2.0 * Ada.Numerics.Pi;
               Travel       : Dimensionless;
            begin
               Candidate := Section.Start_Distance;
               Found := False;
               if Rise_Squared <= 0.0 * mm**2 then
                  --  A planar arc has one distance minimum per turn, at the phase of Point. The section minimum is
                  --  there when the section contains that phase and at an endpoint otherwise.
                  if DX = 0.0 * mm and then DY = 0.0 * mm then
                     return;
                  end if;

                  declare
                     Ahead : Dimensionless :=
                       Sign * (Dimensionless_Math.Arctan (DY / mm, DX / mm) - Primitive.Theta_Start) - First_Travel;
                  begin
                     Ahead := Ahead - Two_Pi * Dimensionless'Floor (Ahead / Two_Pi);
                     if First_Travel + Ahead > Last_Travel then
                        return;
                     end if;
                     Travel := First_Travel + Ahead;
                  end;
               else
                  --  The rise can give the squared distance several local minima on a long, steep section. It is
                  --  sampled in plain arithmetic, then the best sample is refined by Newton steps on its derivative,
                  --  falling back to bisection inside the bracket formed by the neighbouring samples.
                  declare
                     function Phase_At (T : Dimensionless) return Dimensionless
                     is (Primitive.Theta_Start + Sign * T);

                     --  The squared distance from Point at travel T, less a constant.
                     function Reduced_Squared_Distance (T : Dimensionless) return Area
                     is (Rise_Squared * T * T - 2.0 * Axial_Target * T
                         - 2.0
                           * Primitive.Radius
                           * (DX * Dimensionless_Math.Cos (Phase_At (T))
                              + DY * Dimensionless_Math.Sin (Phase_At (T))));

                     Sample_Step : constant Dimensionless :=
                       (Last_Travel - First_Travel) / Dimensionless (Primitive_Samples - 1);
                     Best_Index  : Natural := 0;
                     Best_Value  : Area := Area'Last;
                     Lower       : Dimensionless;
                     Upper       : Dimensionless;
                  begin
                     for S in 0 .. Primitive_Samples - 1 loop
                        declare
                           Value : constant Area :=
                             Reduced_Squared_Distance (First_Travel + Sample_Step * Dimensionless (S));
                        begin
                           if Value < Best_Value then
                              Best_Value := Value;
                              Best_Index := S;
                           end if;
                        end;
                     end loop;

                     Travel := First_Travel + Sample_Step * Dimensionless (Best_Index);
                     Lower := First_Travel + Sample_Step * Dimensionless (Natural'Max (Best_Index, 1) - 1);
                     Upper :=
                       Dimensionless'Min
                         (Last_Travel,
                          First_Travel
                          + Sample_Step * Dimensionless (Natural'Min (Best_Index + 1, Primitive_Samples - 1)));

                     for Iteration in 1 .. 8 loop
                        declare
                           Sin_Phase : constant Dimensionless := Dimensionless_Math.Sin (Phase_At (Travel));
                           Cos_Phase : constant Dimensionless := Dimensionless_Math.Cos (Phase_At (Travel));
                           Slope     : constant Area :=
                             Rise_Squared * Travel - Axial_Target
                             + Sign * Primitive.Radius * (DX * Sin_Phase - DY * Cos_Phase);
                           Bend      : constant Area :=
                             Rise_Squared + Primitive.Radius * (DX * Cos_Phase + DY * Sin_Phase);
                           Next      : Dimensionless := 0.5 * (Lower + Upper);
                        begin
                           exit when Slope = 0.0 * mm**2;
                           if Slope > 0.0 * mm**2 then
                              Upper := Travel;
                           else
                              Lower := Travel;
                           end if;

                           if Bend > 0.0 * mm**2 then
                              Next := Travel - Slope / Bend;
                           end if;
                           --  A converged Newton step rounds onto the bracket end just set to Travel, so it is kept
                           --  rather than replaced by a bisection away from the minimum.
                           if Next < Lower or else Next > Upper then
                              Next := 0.5 * (Lower + Upper);
                           end if;
                           exit when Next = Travel;
                           Travel := Next;
                        end;
                     end loop;

                     if Reduced_Squared_Distance (Travel) > Best_Value then
                        Travel := First_Travel + Sample_Step * Dimensionless (Best_Index);
                     end if;
                  end;
               end if;

               --  Like the line projection, Travel only selects an actual point in the section, so its distance is an
               --  upper bound on the section minimum however well the search above converged.
               Candidate :=
                 Length'Max
                   (Section.Start_Distance, Length'Min (Section.End_Distance, Travel * Primitive.Length_Per_Radian));
               Found := True;
            end Helix_Candidate;
         begin
            if Distance <= 0.0 * mm then
               return (Distance => To_Start, Foot => Section.Start_Distance);
            end if;

            declare
               To_End    : constant Length := Point_Distance_Upper (Point, Section.End_Point);
               Best      : Section_Distance :=
                 (if To_Start <= To_End
                  then (Distance => To_Start, Foot => Section.Start_Distance)
                  else (Distance => To_End, Foot => Section.End_Distance));
               Candidate : Length;
               Found     : Boolean;
            begin
               case Section.Primitive.Kind is
                  when Line_Primitive_Kind  =>
                     Line_Candidate (Candidate, Found);

                  when Helix_Primitive_Kind =>
                     Helix_Candidate (Candidate, Found);
               end case;

               if Found then
                  declare
                     Candidate_Distance : constant Length :=
                       Point_Distance_Upper
                         (Point,
                          Derived_Primitive_Point_At_Distance
                            (Block'Access, Section.Finishing_Corner, Section.Primitive, Candidate));
                  begin
                     if Candidate_Distance < Best.Distance then
                        Best := (Distance => Candidate_Distance, Foot => Candidate);
                     end if;
                  end;
               end if;
               return Best;
            end;
         exception
            when Constraint_Error =>
               return (Distance => Length'Last, Foot => Section.Start_Distance);
         end Distance_To_Section;

         function Primitive_Deviation_Upper_Bound
           (Curve              : Corner_Transition;
            Sampler            : in out Transition_Sampler;
            Incoming, Outgoing : Reference_Section;
            Position_Error     : Length;
            Limit              : Length) return Length
         is
            --  Every sample is an actual represented point and each of its two section distances belongs to an
            --  actual reference point, the foot. Two certificates bound the executed curve between neighbouring
            --  samples A and B, and the smaller one stands:
            --
            --    * By chord. The curve stays within the chord sag of its certified second derivative from the
            --      chord A-B. On one section, the reference stays within the section's own chord sag from the chord
            --      between the two feet, and all of it between the feet belongs to the section. Corresponding
            --      points of the two chords are no further apart than the larger of the two sample distances.
            --
            --    * By step. Each sample covers its own half of the interval, where the curve moves by at most
            --      half the interval times its certified speed, and may use whichever section is nearer.
            --
            --  An interval is halved while its bound exceeds both Limit and the largest bound already found, down
            --  to neighbouring samples. Halving an interval whose bound is at most the larger of those two could
            --  change neither the result's comparison with Limit nor, beyond it, the result.
            type Sample_Distances is record
               Incoming, Outgoing : Section_Distance;
            end record;

            Distances       : array (Transition_Flags'Range) of Sample_Distances;
            Measured        : Transition_Flags := [others => False];
            Error_Allowance : constant Length := Multiply_Length_Upper (Position_Error, 2.0);
            --  The two-sided ideal-to-executed allowance covers both locally built and imported V1 curves.
            Target          : constant Length :=
              (if Limit > Error_Allowance and then Limit < Length'Last then Limit - Error_Allowance else 0.0 * mm);
            Speed           : constant Dimensionless := Executed_Speed_Upper (Curve);
            Bend            : constant Curvature := Executed_Bend_Upper (Curve);
            Intervals       : constant Positive := (Transition_Samples - 1) / Coarse_Sample_Span;
            Result          : Length := 0.0 * mm;

            procedure Measure (Index : Natural);
            function Along (Section : Reference_Section; A, B : Section_Distance) return Length;
            function Interval_Bound (First_Index : Natural; Span : Positive; Floor : Length) return Length;

            procedure Measure (Index : Natural) is
            begin
               if not Measured (Index) then
                  declare
                     Point : constant Position := Sample (Curve, Sampler, Index);
                  begin
                     Distances (Index) :=
                       (Incoming => Distance_To_Section (Point, Incoming),
                        Outgoing => Distance_To_Section (Point, Outgoing));
                     Measured (Index) := True;
                  end;
               end if;
            end Measure;

            function Along (Section : Reference_Section; A, B : Section_Distance) return Length is
            begin
               return
                 Add_Length_Upper
                   (Add_Length_Upper
                      (Length'Max (A.Distance, B.Distance), Chord_Sag_Upper (abs (B.Foot - A.Foot), Section.Bend)),
                    Section.Allowance);
            end Along;

            function Interval_Bound (First_Index : Natural; Span : Positive; Floor : Length) return Length is
               Last_Index : constant Natural := First_Index + Span;
            begin
               Measure (First_Index);
               Measure (Last_Index);
               declare
                  A           : Sample_Distances renames Distances (First_Index);
                  B           : Sample_Distances renames Distances (Last_Index);
                  Span_Length : constant Length := Sample_Span_Upper (Curve, Span);
                  By_Chord    : constant Length :=
                    Add_Length_Upper
                      (Length'Min (Along (Incoming, A.Incoming, B.Incoming), Along (Outgoing, A.Outgoing, B.Outgoing)),
                       Chord_Sag_Upper (Span_Length, Bend));
                  By_Step     : constant Length :=
                    Add_Length_Upper
                      (Length'Max
                         (Length'Min (A.Incoming.Distance, A.Outgoing.Distance),
                          Length'Min (B.Incoming.Distance, B.Outgoing.Distance)),
                       Multiply_Length_Upper (Multiply_Length_Upper (Span_Length, 0.5), Speed));
                  Best        : constant Length := Length'Min (By_Chord, By_Step);
               begin
                  if Span = 1 or else Best <= Length'Max (Target, Floor) then
                     return Best;
                  end if;
                  declare
                     First_Half : constant Length := Interval_Bound (First_Index, Span / 2, Floor);
                  begin
                     return
                       Length'Max
                         (First_Half,
                          Interval_Bound (First_Index + Span / 2, Span / 2, Length'Max (Floor, First_Half)));
                  end;
               end;
            end Interval_Bound;
         begin
            if Arc_Length (Curve) <= 0.0 * mm then
               return Length'Last;
            end if;

            --  The deviation peaks near the middle, so starting there settles the largest bound early and leaves
            --  the outer intervals of a failing curve unrefined.
            for Step in 0 .. Intervals - 1 loop
               declare
                  Interval : constant Natural :=
                    (if Step mod 2 = 0 then Intervals / 2 + Step / 2 else Intervals / 2 - 1 - Step / 2);
               begin
                  Result :=
                    Length'Max (Result, Interval_Bound (Interval * Coarse_Sample_Span, Coarse_Sample_Span, Result));
               end;
            end loop;
            return Add_Length_Upper (Result, Error_Allowance);
         end Primitive_Deviation_Upper_Bound;

         function Line_Corner_Axial_Deviation
           (Curve          : Corner_Transition;
            Sampler        : in out Transition_Sampler;
            Start_Point    : Position;
            Corner_Point   : Position;
            Finish_Point   : Position;
            Limits         : Axial_Deviation_Limits;
            Position_Error : Length) return Axial_Deviation_Check
         is
            --  The points within the axial limits of one line form a convex set, the sum of a segment and a box.
            --  Between two samples the executed curve leaves their chord, on each axis, by at most the chord sag of
            --  its certified second derivative plus the error allowance. Two samples inside one line's set shrunk
            --  by that margin therefore prove the whole interval between them.
            --
            --  An interval which its chord does not prove is halved. Between neighbouring samples each sample may
            --  instead cover its own half of the interval, where the curve moves on each axis by at most half the
            --  interval times its certified velocity, and may use either line.
            Bounds          : constant Unit_Speed_Axial_Derivative_Bounds := Derivative_Bounds (Curve);
            Error_Allowance : constant Length := Multiply_Length_Upper (Position_Error, 2.0);
            --  The two-sided ideal-to-executed allowance covers both locally built and imported V1 curves.
            Arc_Gap         : constant Length :=
              Divide_Length_Upper (Arc_Length (Curve), 2.0 * Dimensionless (Transition_Samples - 1));
            Intervals       : constant Positive := (Transition_Samples - 1) / Coarse_Sample_Span;
            Chord_Margins   : array (1 .. Coarse_Sample_Span) of Axial_Deviation_Limits :=
              [others => [others => 0.0 * mm]];
            --  Indexed by the number of sample steps which the chord spans.
            Step_Margins    : Axial_Deviation_Limits := [others => 0.0 * mm];

            function On_Line
              (Point, First, Last : Position; Margins : Axial_Deviation_Limits; Factor : Dimensionless) return Boolean;
            function Within (Point : Position; Margins : Axial_Deviation_Limits; Factor : Dimensionless) return Boolean
            is (On_Line (Point, Start_Point, Corner_Point, Margins, Factor)
                or else On_Line (Point, Corner_Point, Finish_Point, Margins, Factor));
            function Proved (First_Index : Natural; Span : Positive) return Boolean;
            function Sample_Ratio (Point : Position; Margins : Axial_Deviation_Limits) return Dimensionless;

            function On_Line
              (Point, First, Last : Position; Margins : Axial_Deviation_Limits; Factor : Dimensionless) return Boolean
            is
               Q_Lower : Dimensionless := 0.0;
               Q_Upper : Dimensionless := 1.0;
            begin
               for Axis in Axis_Name when Axis /= E_Axis loop
                  declare
                     Budget     : constant Length := Factor * Limits (Axis) - Margins (Axis);
                     Offset     : constant Length := Point (Axis) - First (Axis);
                     Line_Delta : constant Length := Last (Axis) - First (Axis);
                  begin
                     if Budget < 0.0 * mm then
                        return False;
                     elsif Line_Delta = 0.0 * mm then
                        if abs Offset > Budget then
                           return False;
                        end if;
                     else
                        declare
                           First_Q    : constant Dimensionless := (Offset - Budget) / Line_Delta;
                           Last_Q     : constant Dimensionless := (Offset + Budget) / Line_Delta;
                           Axis_Lower : constant Dimensionless := Round_Up (Dimensionless'Min (First_Q, Last_Q));
                           Axis_Upper : constant Dimensionless := Round_Down (Dimensionless'Max (First_Q, Last_Q));
                        begin
                           Q_Lower := Dimensionless'Max (Q_Lower, Axis_Lower);
                           Q_Upper := Dimensionless'Min (Q_Upper, Axis_Upper);
                           if Q_Lower > Q_Upper then
                              return False;
                           end if;
                        end;
                     end if;
                  end;
               end loop;
               return Q_Lower <= Q_Upper;
            exception
               when Constraint_Error =>
                  return False;
            end On_Line;

            function Proved (First_Index : Natural; Span : Positive) return Boolean is
               A : constant Position := Sample (Curve, Sampler, First_Index);
               B : constant Position := Sample (Curve, Sampler, First_Index + Span);
            begin
               if (On_Line (A, Start_Point, Corner_Point, Chord_Margins (Span), 1.0)
                   and then On_Line (B, Start_Point, Corner_Point, Chord_Margins (Span), 1.0))
                 or else
                   (On_Line (A, Corner_Point, Finish_Point, Chord_Margins (Span), 1.0)
                    and then On_Line (B, Corner_Point, Finish_Point, Chord_Margins (Span), 1.0))
               then
                  return True;
               elsif Span > 1 then
                  return Proved (First_Index, Span / 2) and then Proved (First_Index + Span / 2, Span / 2);
               else
                  return Within (A, Step_Margins, 1.0) and then Within (B, Step_Margins, 1.0);
               end if;
            end Proved;

            function Sample_Ratio (Point : Position; Margins : Axial_Deviation_Limits) return Dimensionless is
               Lower : Dimensionless := 0.0;
               Upper : Dimensionless := 1.0;
            begin
               if Within (Point, Margins, 1.0) then
                  return 1.0;
               end if;
               while Upper < 1.0E12 and then not Within (Point, Margins, Upper) loop
                  Lower := Upper;
                  Upper := 2.0 * Upper;
               end loop;
               if not Within (Point, Margins, Upper) then
                  return Dimensionless'Last;
               end if;
               --  The ratio only sizes the next attempt, so twenty halvings resolve it amply.
               for Iteration in 1 .. 20 loop
                  declare
                     Middle : constant Dimensionless := 0.5 * (Lower + Upper);
                  begin
                     if Within (Point, Margins, Middle) then
                        Upper := Middle;
                     else
                        Lower := Middle;
                     end if;
                  end;
               end loop;
               return Round_Up (Upper);
            end Sample_Ratio;

            Pass  : Boolean := True;
            Worst : Dimensionless := 0.0;
         begin
            if Arc_Length (Curve) <= 0.0 * mm then
               return (Pass => False, Worst_Ratio => Dimensionless'Last);
            end if;
            for Axis in Axis_Name when Axis /= E_Axis loop
               if not Axis_Is_Structurally_Constant (Curve, Axis) then
                  declare
                     Span : Positive := Coarse_Sample_Span;
                  begin
                     --  Only the spans which halving can reach are used.
                     loop
                        Chord_Margins (Span) (Axis) :=
                          Add_Length_Upper
                            (Chord_Sag_Upper (Sample_Span_Upper (Curve, Span), Bounds.Acceleration (Axis)),
                             Error_Allowance);
                        exit when Span = 1;
                        Span := Span / 2;
                     end loop;
                  end;
                  Step_Margins (Axis) :=
                    Add_Length_Upper (Multiply_Length_Upper (Arc_Gap, Bounds.Velocity (Axis)), Error_Allowance);
               end if;
            end loop;

            --  The deviation peaks near the middle, so a curve which cannot pass is recognised there first.
            for Step in 0 .. Intervals - 1 loop
               declare
                  Interval : constant Natural :=
                    (if Step mod 2 = 0 then Intervals / 2 + Step / 2 else Intervals / 2 - 1 - Step / 2);
               begin
                  Pass := Proved (Interval * Coarse_Sample_Span, Coarse_Sample_Span);
                  exit when not Pass;
               end;
            end loop;
            if Pass then
               return (Pass => True, Worst_Ratio => 1.0);
            end if;

            --  Repair needs the worst ratio over the entire curve. The coarse samples and whichever others the
            --  failed proof evaluated estimate it; only the pass above is a certificate. The margins are those of
            --  neighbouring samples, the smallest which a proof of the rebuilt curve can come to need.
            for Index in Transition_Flags'Range loop
               if Index mod Coarse_Sample_Span = 0 or else Sampler.Known (Index) then
                  Worst := Dimensionless'Max (Worst, Sample_Ratio (Sample (Curve, Sampler, Index), Chord_Margins (1)));
                  exit when Worst >= Dimensionless'Last;
               end if;
            end loop;
            --  Samples which each lie within the limits can still leave an interval unproved. Ask for a slightly
            --  smaller curve rather than the fallback shrink of an unmeasured failure.
            return (Pass => False, Worst_Ratio => Dimensionless'Max (Worst, 1.02));
         exception
            when Constraint_Error =>
               return (Pass => False, Worst_Ratio => Dimensionless'Last);
         end Line_Corner_Axial_Deviation;

         procedure Assign_Target_Widths is
         begin
            for I in Block.Corners'Range loop
               Hard_Anchors (I) := Hard_Anchor (I);
               Target_Incoming_Trims (I) := 0.0 * mm;
               Target_Outgoing_Trims (I) := 0.0 * mm;
               Cached_Attempts (I) := (others => <>);
               Cached_Attempt_Valid (I) := False;
               Accepted_Incoming_Trims (I) := 0.0 * mm;
               Accepted_Outgoing_Trims (I) := 0.0 * mm;
            end loop;

            for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
               if not Hard_Anchors (I) then
                  declare
                     Incoming_Tangent    : constant Position_Scale :=
                       Primitive_Direction_At_Distance (Block'Access, I, Primitive_Length (Block'Access, I));
                     Outgoing_Tangent    : constant Position_Scale :=
                       Primitive_Direction_At_Distance (Block'Access, I + 1, 0.0 * mm);
                     Dot_Tangents        : constant Dimensionless :=
                       Clamp_Unit_Dot (Dot (Incoming_Tangent, Outgoing_Tangent));
                     In_Bias             : constant Dimensionless := Dimensionless'Max (0.0, -Settings.Bias);
                     Out_Bias            : constant Dimensionless := Dimensionless'Max (0.0, Settings.Bias);
                     In_Mult             : constant Dimensionless := 1.0 + 0.75 * In_Bias;
                     Out_Mult            : constant Dimensionless := 1.0 + 0.75 * Out_Bias;
                     Adjacent_Length     : constant Length :=
                       Length'Min (Primitive_Length (Block'Access, I), Primitive_Length (Block'Access, I + 1));
                     Effective_Deviation : constant Length :=
                       Effective_Axial_Deviation (Incoming_Tangent, Outgoing_Tangent);
                     Base_Width          : Length;
                  begin
                     case Block.Params.Cornering.Kind is
                        when Stereographic =>
                           declare
                              Cos_Half_Theta : constant Dimensionless :=
                                Dimensionless_Math.Sqrt
                                  (Dimensionless'Min (1.0, Dimensionless'Max (0.0, 0.5 + 0.5 * Dot_Tangents)));
                              Bend_Factor    : constant Dimensionless :=
                                Dimensionless'Max (0.025, 1.0 - Cos_Half_Theta);
                           begin
                              if Effective_Deviation = Length'Last then
                                 --  Matching tangents give no angle-based width estimate, but differing
                                 --  curvature can still need a blend. Start within the adjacent primitives;
                                 --  construction and deviation checks determine whether it can be retained.
                                 Base_Width := Adjacent_Length / Dimensionless'Max (In_Mult, Out_Mult);
                                 declare
                                    Incoming_Jet : constant Endpoint_Tangent_Jet :=
                                      Primitive_Derivative_Jets_At_Distance
                                        (Block'Access, I, Primitive_Length (Block'Access, I));
                                    Outgoing_Jet : constant Endpoint_Tangent_Jet :=
                                      Primitive_Derivative_Jets_At_Distance (Block'Access, I + 1, 0.0 * mm);
                                    Jump_Squared : Dimensionless := 0.0;
                                    Tightest     : Length := Settings.Miss;
                                 begin
                                    for Axis in Axis_Name when Axis /= E_Axis loop
                                       Jump_Squared :=
                                         Jump_Squared
                                         + Dimensionless
                                             ((Outgoing_Jet.Tangent_Derivative_1 (Axis)
                                               - Incoming_Jet.Tangent_Derivative_1 (Axis))
                                              * mm)**2;
                                       if Settings.Deviation (Axis) > 0.0 * mm then
                                          Tightest := Length'Min (Tightest, Settings.Deviation (Axis));
                                       end if;
                                    end loop;
                                    --  A curvature ramp of half-width w across a jump Δκ shifts the path by about
                                    --  Δκ·w²/6, the clothoid shift. Size w so that shift meets the tightest limit;
                                    --  the repair loop corrects the estimate.
                                    if Jump_Squared > 0.0 and then Tightest > 0.0 * mm then
                                       Base_Width :=
                                         Length'Min
                                           (Base_Width,
                                            Dimensionless_Math.Sqrt
                                              (6.0
                                               * Dimensionless (Tightest / mm)
                                               / Dimensionless_Math.Sqrt (Jump_Squared))
                                            * mm);
                                    end if;
                                 end;
                              else
                                 --  The first expression is the long-standing width policy. It overshoots the
                                 --  limits at shallow corners, where the model of the actual blend takes over so
                                 --  that the first attempt is usually the one retained.
                                 Base_Width :=
                                   Length'Min
                                     ((1.35 - 0.49 * Settings.Circularity)
                                      * Length'Min (Effective_Deviation, Settings.Miss * Cos_Half_Theta)
                                      / Bend_Factor,
                                      Modelled_Base_Width
                                        (I,
                                         Incoming_Tangent,
                                         Outgoing_Tangent,
                                         Cos_Half_Theta,
                                         Dimensionless_Math.Sqrt (In_Mult * Out_Mult)));
                              end if;
                              Target_Incoming_Trims (I) := Base_Width * In_Mult;
                              Target_Outgoing_Trims (I) := Base_Width * Out_Mult;
                           end;

                        when Circular      =>
                           declare
                              Sin_Half : constant Dimensionless :=
                                Dimensionless_Math.Sqrt
                                  (Dimensionless'Min (1.0, Dimensionless'Max (0.0, 0.5 - 0.5 * Dot_Tangents)));
                              Cos_Half : constant Dimensionless :=
                                Dimensionless_Math.Sqrt
                                  (Dimensionless'Min (1.0, Dimensionless'Max (0.0, 0.5 + 0.5 * Dot_Tangents)));
                              Tan_Half : constant Dimensionless :=
                                (if Cos_Half > 0.0 then Sin_Half / Cos_Half else Dimensionless'Last);
                              Radius   : constant Length := Block.Params.Cornering.Circular_Params.Radius_Max;
                           begin
                              if Tan_Half <= 0.0 or else Radius <= 0.0 * mm then
                                 Base_Width := 0.0 * mm;
                              else
                                 Base_Width := Length'Min (Adjacent_Length, Multiply_Length_Upper (Radius, Tan_Half));
                              end if;
                              Target_Incoming_Trims (I) := Base_Width;
                              Target_Outgoing_Trims (I) := Base_Width;
                           end;

                        when Biarc         =>
                           declare
                              Trim_Max : constant Length := Settings.Trim_Max;
                           begin
                              Base_Width := Length'Min (Adjacent_Length, Trim_Max);
                              Target_Incoming_Trims (I) :=
                                Length'Min (Trim_Max, Multiply_Length_Upper (Base_Width, In_Mult));
                              Target_Outgoing_Trims (I) :=
                                Length'Min (Trim_Max, Multiply_Length_Upper (Base_Width, Out_Mult));
                           end;

                        when Sharp_SCV     =>
                           raise Program_Error with "sharp SCV does not allocate corner trims";
                     end case;
                  end;
               end if;
            end loop;
         end Assign_Target_Widths;

         procedure Allocate_Segment_Trims is
            function Segment_Scale (Finishing_Corner : Finishing_Corners_Index) return Dimensionless;

            function Segment_Scale (Finishing_Corner : Finishing_Corners_Index) return Dimensionless is
               Demand      : constant Length :=
                 Add_Length_Upper
                   (Target_Outgoing_Trims (Finishing_Corner - 1), Target_Incoming_Trims (Finishing_Corner));
               Path_Length : constant Length := Primitive_Length (Block'Access, Finishing_Corner);
            begin
               if Path_Length <= 0.0 * mm then
                  return 0.0;
               elsif Demand = 0.0 * mm then
                  return 1.0;
               elsif Demand > Path_Length then
                  return Dimensionless'Max (0.0, Round_Down (Path_Length / Demand));
               else
                  return 1.0;
               end if;
            end Segment_Scale;
         begin
            for I in Block.Corners'Range loop
               Allocated_Incoming_Trims (I) := 0.0 * mm;
               Allocated_Outgoing_Trims (I) := 0.0 * mm;
            end loop;

            for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
               if not Hard_Anchors (I) then
                  declare
                     Scale : constant Dimensionless := Dimensionless'Min (Segment_Scale (I), Segment_Scale (I + 1));
                  begin
                     --  A single corner scale is applied to both sides.  This
                     --  preserves the requested asymmetry exactly while both
                     --  adjacent segment constraints are satisfied in O(N).
                     Allocated_Incoming_Trims (I) := Target_Incoming_Trims (I) * Scale;
                     Allocated_Outgoing_Trims (I) := Target_Outgoing_Trims (I) * Scale;
                     --  Keep the constructor out of pathological boundary
                     --  layers even if a future width policy requests a much
                     --  larger imbalance.  Reducing only the long side keeps
                     --  both segment-capacity inequalities satisfied.
                     if Allocated_Outgoing_Trims (I) > Maximum_Trim_Asymmetry * Allocated_Incoming_Trims (I) then
                        Allocated_Outgoing_Trims (I) := Maximum_Trim_Asymmetry * Allocated_Incoming_Trims (I);
                     elsif Allocated_Incoming_Trims (I) > Maximum_Trim_Asymmetry * Allocated_Outgoing_Trims (I) then
                        Allocated_Incoming_Trims (I) := Maximum_Trim_Asymmetry * Allocated_Outgoing_Trims (I);
                     end if;
                  end;
               end if;
            end loop;
         end Allocate_Segment_Trims;

         function Validate_Transition
           (I                  : Corners_Index;
            Curve              : Corner_Transition;
            Incoming, Outgoing : Derived_Path_Primitive;
            Trim_In            : Length;
            Trim_Out           : Length;
            Position_Error     : Length) return Corner_Transition_Attempt
         is
            function Finite_Length (Value : Length) return Boolean
            is (Value >= Length'First and then Value <= Length'Last);

            function Envelope_Is_In_Bounds (Envelope : Position_Envelope; Numeric_Passes : out Boolean) return Boolean;

            function Envelope_Is_In_Bounds (Envelope : Position_Envelope; Numeric_Passes : out Boolean) return Boolean
            is
               Expanded_Envelope : Position_Envelope;
               Error             : constant Length := Position_Error_Bound (Curve);
            begin
               Numeric_Passes := False;
               if not Finite_Length (Error) or else Error < 0.0 * mm or else Error >= Length'Last then
                  return False;
               end if;

               for Axis in Axis_Name loop
                  if not Finite_Length (Envelope (Axis).Lower)
                    or else not Finite_Length (Envelope (Axis).Upper)
                    or else Envelope (Axis).Lower > Envelope (Axis).Upper
                  then
                     return False;
                  end if;

                  declare
                     Constant_Axis  : constant Boolean := Axis_Is_Structurally_Constant (Curve, Axis);
                     Expanded_Lower : constant Length :=
                       (if Constant_Axis
                        then Envelope (Axis).Lower
                        else Length'Adjacent (Envelope (Axis).Lower, Length'First));
                     Expanded_Upper : constant Length :=
                       (if Constant_Axis
                        then Envelope (Axis).Upper
                        else Length'Adjacent (Envelope (Axis).Upper, Length'Last));
                     Outside_Bounds : constant Boolean :=
                       (case Axis is
                          when X_Axis =>
                            (case Block.Params.Bounds.Kind is
                               when Rectangular_Workspace =>
                                 Expanded_Lower < Block.Params.Bounds.Lower_X
                                 or else Expanded_Upper > Block.Params.Bounds.Upper_X,
                               when Circular_Workspace    => False),
                          when Y_Axis =>
                            (case Block.Params.Bounds.Kind is
                               when Rectangular_Workspace =>
                                 Expanded_Lower < Block.Params.Bounds.Lower_Y
                                 or else Expanded_Upper > Block.Params.Bounds.Upper_Y,
                               when Circular_Workspace    => False),
                          when Z_Axis =>
                            Expanded_Lower < Block.Params.Bounds.Lower_Z
                            or else Expanded_Upper > Block.Params.Bounds.Upper_Z,
                          when E_Axis => False);
                     --  E's shortened-path reference and time-domain correction are checked separately.
                  begin
                     Expanded_Envelope (Axis) := (Lower => Expanded_Lower, Upper => Expanded_Upper);
                     if not Finite_Length (Expanded_Lower)
                       or else not Finite_Length (Expanded_Upper)
                       or else Expanded_Lower > Expanded_Upper
                     then
                        return False;
                     elsif Outside_Bounds then
                        Numeric_Passes := True;
                        return False;
                     end if;
                  end;
               end loop;

               if Block.Params.Bounds.Kind = Circular_Workspace then
                  for Use_Upper_X in Boolean loop
                     for Use_Upper_Y in Boolean loop
                        declare
                           Pos : Position := Block.Corners (I);
                        begin
                           Pos (X_Axis) :=
                             (if Use_Upper_X
                              then Expanded_Envelope (X_Axis).Upper
                              else Expanded_Envelope (X_Axis).Lower);
                           Pos (Y_Axis) :=
                             (if Use_Upper_Y
                              then Expanded_Envelope (Y_Axis).Upper
                              else Expanded_Envelope (Y_Axis).Lower);
                           if not XY_Position_Is_In_Bounds (Pos, Block.Params) then
                              Numeric_Passes := True;
                              return False;
                           end if;
                        end;
                     end loop;
                  end loop;
               end if;

               Numeric_Passes := True;
               return True;
            exception
               when Constraint_Error =>
                  Numeric_Passes := False;
                  return False;
            end Envelope_Is_In_Bounds;
         begin
            if not (Arc_Length (Curve) > 0.0 * mm and then Arc_Length (Curve) < Length'Last)
              or else not (Split_Distance (Curve) >= 0.0 * mm and then Split_Distance (Curve) <= Arc_Length (Curve))
              or else not (Trim_In >= 0.0 * mm and then Trim_In < Length'Last)
              or else not (Trim_Out >= 0.0 * mm and then Trim_Out < Length'Last)
              or else not (Position_Error >= 0.0 * mm and then Position_Error < Length'Last)
            then
               return
                 (Accepted             => False,
                  Requires_Hard_Anchor => True,
                  Trim_In              => Trim_In,
                  Trim_Out             => Trim_Out,
                  Failure_Limit        => 0.0 * mm,
                  Failure_Upper        => Length'Last);
            end if;

            --  Keep evaluation after the validity guard and within the exception handler below.
            --  An exception during a function's own declarations would bypass its handler.
            declare
               Sampler             : Transition_Sampler;
               Incoming_Section    : constant Reference_Section :=
                 Section_Of (I, Incoming, Incoming.Length - Trim_In, Trim_In);
               Outgoing_Section    : constant Reference_Section := Section_Of (I + 1, Outgoing, 0.0 * mm, Trim_Out);
               Start_Point         : Position renames Incoming_Section.Start_Point;
               End_Point           : Position renames Outgoing_Section.End_Point;
               Deviation_Limits    : constant Axial_Deviation_Limits := Settings.Deviation;
               Deviation_Proxy     : constant Length :=
                 Effective_Axial_Deviation
                   (Derived_Primitive_Direction_At_Distance (Incoming, Incoming.Length),
                    Derived_Primitive_Direction_At_Distance (Outgoing, 0.0 * mm));
               Miss_Limit          : constant Length := Settings.Miss;
               Line_Line           : constant Boolean :=
                 Block.Primitives (I).Kind = Line_Primitive_Kind
                 and then Block.Primitives (I + 1).Kind = Line_Primitive_Kind;
               Error_Allowance     : constant Length := Multiply_Length_Upper (Position_Error, 2.0);
               Axial_Check         : Axial_Deviation_Check := (Pass => False, Worst_Ratio => Dimensionless'Last);
               Primitive_Limit     : Length := Length'Last;
               --  The tightest axial limit which a corner with a helix has to meet.
               Primitive_Deviation : Length := 0.0 * mm;
               Deviation_Passes    : Boolean;
               Corner_Distance     : Length := Length'Last;
               Nearest             : Natural := 0;
               Corner_Passes       : Boolean;
               Envelope_Passes     : Boolean := True;
               Envelope_Numeric    : Boolean := True;
               Failure_Limit       : Length := Deviation_Proxy;
               Failure_Upper       : Length := Length'Last;
               Accepted            : Boolean;

               function Corner_Distance_At (Index : Natural) return Length
               is (Add_Length_Upper
                     (Point_Distance_Upper (Sample (Curve, Sampler, Index), Block.Corners (I)),
                      Position_Error_Bound (Curve)));
            begin
               if Line_Line then
                  Axial_Check :=
                    Line_Corner_Axial_Deviation
                      (Curve, Sampler, Start_Point, Block.Corners (I), End_Point, Deviation_Limits, Position_Error);
                  Deviation_Passes := Axial_Check.Pass;
                  if Axial_Check.Worst_Ratio < Dimensionless'Last then
                     --  The ratio scales the limits about fixed error allowances, which do not shrink with the
                     --  curve. Size the repair from the part of the tightest limit which does.
                     declare
                        Tightest : Length := Length'Last;
                        Reserved : Dimensionless := 0.0;
                     begin
                        for Axis in Axis_Name when Axis /= E_Axis loop
                           if Deviation_Limits (Axis) > 0.0 * mm then
                              Tightest := Length'Min (Tightest, Deviation_Limits (Axis));
                           end if;
                        end loop;
                        if Tightest < Length'Last and then Tightest > Error_Allowance then
                           Reserved := Error_Allowance / Tightest;
                        end if;
                        Failure_Limit := Multiply_Length_Upper (Deviation_Proxy, 1.0 - Reserved);
                        Failure_Upper := Multiply_Length_Upper (Deviation_Proxy, Axial_Check.Worst_Ratio - Reserved);
                     end;
                  end if;
               else
                  for Axis in Axis_Name when Axis /= E_Axis loop
                     --  Helices rotate in XY. Equal Z endpoints prove constant Z on each primitive;
                     --  equal XY endpoints alone would not prove that for a full or partial arc.
                     if not (Axis_Is_Structurally_Constant (Curve, Axis)
                             and then Axis not in X_Axis | Y_Axis
                             and then Block.Corners (I - 1) (Axis) = Block.Corners (I) (Axis)
                             and then Block.Corners (I + 1) (Axis) = Block.Corners (I) (Axis))
                     then
                        Primitive_Limit := Length'Min (Primitive_Limit, Deviation_Limits (Axis));
                     end if;
                  end loop;
                  Primitive_Deviation :=
                    Primitive_Deviation_Upper_Bound
                      (Curve, Sampler, Incoming_Section, Outgoing_Section, Position_Error, Primitive_Limit);
                  Deviation_Passes := Primitive_Deviation <= Primitive_Limit;
                  Failure_Limit := Primitive_Limit;
                  Failure_Upper := Primitive_Deviation;
               end if;

               --  Every sampled value is an actual represented point.  Its distance plus the certified evaluator error
               --  is consequently a safe upper bound on the transition's minimum distance from the commanded corner;
               --  no assumption about where that minimum occurs is needed.
               for Index in Transition_Flags'Range loop
                  if Index mod Coarse_Sample_Span = 0 or else Sampler.Known (Index) then
                     declare
                        Distance : constant Length := Corner_Distance_At (Index);
                     begin
                        if Distance < Corner_Distance then
                           Corner_Distance := Distance;
                           Nearest := Index;
                        end if;
                     end;
                  end if;
               end loop;
               --  Any sample bounds the minimum, so the samples beside the nearest one are only worth evaluating
               --  when that one alone does not meet the limit.
               while Corner_Distance > Miss_Limit loop
                  declare
                     Before : constant Length :=
                       (if Nearest > 0 then Corner_Distance_At (Nearest - 1) else Length'Last);
                     After  : constant Length :=
                       (if Nearest < Transition_Samples - 1 then Corner_Distance_At (Nearest + 1) else Length'Last);
                  begin
                     exit when Length'Min (Before, After) >= Corner_Distance;
                     if Before <= After then
                        Corner_Distance := Before;
                        Nearest := Nearest - 1;
                     else
                        Corner_Distance := After;
                        Nearest := Nearest + 1;
                     end if;
                  end;
               end loop;
               Corner_Passes := Corner_Distance <= Miss_Limit;

               Envelope_Passes := Envelope_Is_In_Bounds (Certified_Position_Envelope (Curve), Envelope_Numeric);
               if not Envelope_Passes then
                  declare
                     Incoming_Numeric : Boolean;
                     Outgoing_Numeric : Boolean;
                     Incoming_Passes  : constant Boolean :=
                       Envelope_Is_In_Bounds
                         (Certified_Position_Envelope (Curve, 0.0 * mm, Split_Distance (Curve)), Incoming_Numeric);
                     Outgoing_Passes  : constant Boolean :=
                       Envelope_Is_In_Bounds
                         (Certified_Position_Envelope (Curve, Split_Distance (Curve), Arc_Length (Curve)),
                          Outgoing_Numeric);
                  begin
                     Envelope_Passes := Incoming_Passes and Outgoing_Passes;
                     Envelope_Numeric := Incoming_Numeric and Outgoing_Numeric;
                  end;
               end if;
               Accepted := Deviation_Passes and Corner_Passes and Envelope_Passes;

               --  Likewise for the Euclidean bound of a corner with a helix.
               if not Line_Line
                 and then Failure_Limit > Error_Allowance
                 and then Failure_Upper > Error_Allowance
                 and then Failure_Upper < Length'Last
               then
                  Failure_Limit := Failure_Limit - Error_Allowance;
                  Failure_Upper := Failure_Upper - Error_Allowance;
               end if;
               if Deviation_Passes
                 or else
                   (not Corner_Passes
                    and then
                      Predicted_Shrink (Miss_Limit, Corner_Distance, Repair_Shrink_Factor, Repair_Safety)
                      < Predicted_Shrink (Failure_Limit, Failure_Upper, Repair_Shrink_Factor, Repair_Safety))
               then
                  Failure_Limit := Miss_Limit;
                  Failure_Upper := Corner_Distance;
               end if;

               if Accepted then
                  Store_Transition (I, Curve);
               end if;

               return
                 (Accepted             => Accepted,
                  Requires_Hard_Anchor => not Envelope_Numeric,
                  Trim_In              => Trim_In,
                  Trim_Out             => Trim_Out,
                  Failure_Limit        => Failure_Limit,
                  Failure_Upper        => Failure_Upper);
            end;
         exception
            when Constraint_Error =>
               return
                 (Accepted             => False,
                  Requires_Hard_Anchor => True,
                  Trim_In              => Trim_In,
                  Trim_Out             => Trim_Out,
                  Failure_Limit        => 0.0 * mm,
                  Failure_Upper        => Length'Last);
         end Validate_Transition;

         function Try_Build_Corner (I : Corners_Index) return Corner_Transition_Attempt is
            Trim_In  : constant Length := Allocated_Incoming_Trims (I);
            Trim_Out : constant Length := Allocated_Outgoing_Trims (I);
         begin
            if Hard_Anchors (I) then
               return
                 (Accepted             => True,
                  Requires_Hard_Anchor => False,
                  Trim_In              => 0.0 * mm,
                  Trim_Out             => 0.0 * mm,
                  Failure_Limit        => 0.0 * mm,
                  Failure_Upper        => 0.0 * mm);
            elsif Trim_In <= 0.0 * mm or else Trim_Out <= 0.0 * mm then
               return
                 (Accepted             => False,
                  Requires_Hard_Anchor => True,
                  Trim_In              => Trim_In,
                  Trim_Out             => Trim_Out,
                  Failure_Limit        => 0.0 * mm,
                  Failure_Upper        => Length'Last);
            end if;

            declare
               --  Every query below concerns these two primitives, so their geometry is derived once.
               Incoming         : constant Derived_Path_Primitive := Spatial_Primitive (Block'Access, I);
               Outgoing         : constant Derived_Path_Primitive := Spatial_Primitive (Block'Access, I + 1);
               Incoming_Length  : constant Length := Incoming.Length;
               Start_Distance   : constant Length := Incoming_Length - Trim_In;
               Start_Point      : constant Position :=
                 Derived_Primitive_Point_At_Distance (Block'Access, I, Incoming, Start_Distance);
               End_Point        : constant Position :=
                 Derived_Primitive_Point_At_Distance (Block'Access, I + 1, Outgoing, Trim_Out);
               Deviation_Proxy  : constant Length :=
                 Effective_Axial_Deviation
                   (Derived_Primitive_Direction_At_Distance (Incoming, Incoming_Length),
                    Derived_Primitive_Direction_At_Distance (Outgoing, 0.0 * mm));
               Incoming_Tangent : constant Position_Scale :=
                 Derived_Primitive_Direction_At_Distance (Incoming, Start_Distance);
               Outgoing_Tangent : constant Position_Scale :=
                 Derived_Primitive_Direction_At_Distance (Outgoing, Trim_Out);
               --  Biarc tangents belong to the trimmed endpoints. On a helix they differ from the tangents
               --  at the commanded corner; using those would introduce a velocity jump at either join.
               Position_Error   : constant Length := Position_Error_Limit;
               Result           : Construction_Result;
            begin
               case Block.Params.Cornering.Kind is
                  when Stereographic =>
                     Result :=
                       Create_Stereographic
                         ((Start                  =>
                             (Point => Start_Point,
                              Jet   => Derived_Primitive_Derivative_Jets_At_Distance (Incoming, Start_Distance)),
                           Finish                 =>
                             (Point => End_Point,
                              Jet   => Derived_Primitive_Derivative_Jets_At_Distance (Outgoing, Trim_Out)),
                           Maximum_Position_Error => Position_Error,
                           Maximum_Arc_Length     => Trim_In + Trim_Out,
                           Bulge                  => When_Unavoidable));

                  when Circular      =>
                     Result :=
                       Create_Circular
                         (Start_Point,
                          Block.Corners (I),
                          End_Point,
                          Block.Params.Cornering.Circular_Params.Radius_Max);

                  when Biarc         =>
                     Result :=
                       Create_Biarc
                         (Start_Point,
                          End_Point,
                          Incoming_Tangent,
                          Outgoing_Tangent,
                          Maximum_Length       => Trim_In + Trim_Out,
                          Preferred_Trim_Ratio => Dimensionless (Trim_Out / Trim_In));

                  when Sharp_SCV     =>
                     raise Program_Error with "sharp SCV transitions are built by Store_Sharp_Transitions";
               end case;

               case Result.Status is
                  when Construction_Success                 =>
                     return
                       Validate_Transition
                         (I,
                          Result.Transition,
                          Incoming,
                          Outgoing,
                          Trim_In,
                          Trim_Out,
                          Length'Max (Position_Error, Position_Error_Bound (Result.Transition)));

                  when Invalid_Input | Unsupported_Geometry =>
                     return
                       (Accepted             => False,
                        Requires_Hard_Anchor => True,
                        Trim_In              => Trim_In,
                        Trim_Out             => Trim_Out,
                        Failure_Limit        => Deviation_Proxy,
                        Failure_Upper        => Length'Last);

                  when Numerically_Unsafe
                     | Radius_Limit_Exceeded
                     | Length_Limit_Exceeded
                     | Stereographic_Construction_Failed    =>
                     return
                       (Accepted             => False,
                        Requires_Hard_Anchor => False,
                        Trim_In              => Trim_In,
                        Trim_Out             => Trim_Out,
                        Failure_Limit        => Deviation_Proxy,
                        Failure_Upper        => Length'Last);
               end case;
            end;
         end Try_Build_Corner;

         procedure Store_Final_Transitions is
         begin
            for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
               if Hard_Anchors (I) then
                  Allocated_Incoming_Trims (I) := 0.0 * mm;
                  Allocated_Outgoing_Trims (I) := 0.0 * mm;
                  Store_Transition
                    (I,
                     (if Is_Passthrough_Corner (I)
                      then Passthrough_At (Block.Corners (I))
                      else Zero_Corner_Transition (I)));
               else
                  if Cached_Attempt_Valid (I) and then Cached_Attempts (I).Accepted then
                     Allocated_Incoming_Trims (I) := Accepted_Incoming_Trims (I);
                     Allocated_Outgoing_Trims (I) := Accepted_Outgoing_Trims (I);
                  else
                     Hard_Anchors (I) := True;
                     Target_Incoming_Trims (I) := 0.0 * mm;
                     Target_Outgoing_Trims (I) := 0.0 * mm;
                     Allocated_Incoming_Trims (I) := 0.0 * mm;
                     Allocated_Outgoing_Trims (I) := 0.0 * mm;
                     Store_Transition (I, Zero_Corner_Transition (I));
                  end if;
               end if;
            end loop;

            for I in Block.Primitives'Range loop
               declare
                  Full_Primitive_Length : constant Length := Primitive_Length (Block'Access, I);
                  Primitive_Start       : constant Length :=
                    Length'Min (Full_Primitive_Length, Allocated_Outgoing_Trims (I - 1));
                  Primitive_End_Trim    : constant Length :=
                    Length'Min (Full_Primitive_Length - Primitive_Start, Allocated_Incoming_Trims (I));
               begin
                  Block.Primitive_Start_Distances (I) := Primitive_Start;
                  Block.Primitive_Distances (I) := Full_Primitive_Length - Primitive_Start - Primitive_End_Trim;
               end;
            end loop;

            for I in Block.Corners'Range loop
               Clear_Cached_Attempt (I);
            end loop;
         end Store_Final_Transitions;
      begin
         if Block.Params.Cornering.Kind = Sharp_SCV then
            Store_Sharp_Transitions;
            return;
         end if;

         Assign_Target_Widths;

         --  Boundary corners are hard anchors in every allocation mode.
         Store_Transition (Block.Corners'First, Zero_Corner_Transition (Block.Corners'First));
         if Block.Corners'Last /= Block.Corners'First then
            Store_Transition (Block.Corners'Last, Zero_Corner_Transition (Block.Corners'Last));
         end if;

         declare
            Repairs_Used : Natural := 0;
         begin
            Repair_Loop : loop
               declare
                  Needs_Repair      : Boolean := False;
                  Hard_Anchor_Added : Boolean := False;
               begin
                  Allocate_Segment_Trims;

                  for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
                     if not Hard_Anchors (I)
                       and then
                         not (Cached_Attempt_Valid (I)
                              and then Cached_Attempts (I).Trim_In = Allocated_Incoming_Trims (I)
                              and then Cached_Attempts (I).Trim_Out = Allocated_Outgoing_Trims (I))
                     then
                        declare
                           Attempt : Corner_Transition_Attempt := Try_Build_Corner (I);
                        begin
                           if Attempt.Accepted then
                              Accepted_Incoming_Trims (I) := Attempt.Trim_In;
                              Accepted_Outgoing_Trims (I) := Attempt.Trim_Out;
                           elsif Accepted_Incoming_Trims (I) > 0.0 * mm
                             and then Accepted_Incoming_Trims (I) <= Allocated_Incoming_Trims (I)
                             and then Accepted_Outgoing_Trims (I) <= Allocated_Outgoing_Trims (I)
                           then
                              --  A larger allocation can fail even though the previous transition remains valid.
                              --  Reuse it only when both trims fit this pass's capacity allocation. Its evaluator
                              --  and derivative certificate are still stored: failed attempts never overwrite them.
                              --  Keep the attempted allocation as the cache key so unchanged inputs are not retried.
                              --  Store_Final_Transitions uses the separate accepted trims for the retained evaluator.
                              Attempt.Accepted := True;
                              Attempt.Requires_Hard_Anchor := False;
                           end if;
                           Clear_Cached_Attempt (I);
                           Cached_Attempts (I) := Attempt;
                           Cached_Attempt_Valid (I) := True;
                        end;
                     end if;
                  end loop;

                  for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
                     if not Hard_Anchors (I) then
                        declare
                           Attempt : Corner_Transition_Attempt;
                        begin
                           Attempt := Cached_Attempts (I);

                           if not Attempt.Accepted then
                              if Attempt.Requires_Hard_Anchor then
                                 Hard_Anchors (I) := True;
                                 Target_Incoming_Trims (I) := 0.0 * mm;
                                 Target_Outgoing_Trims (I) := 0.0 * mm;
                                 Hard_Anchor_Added := True;
                                 Clear_Cached_Attempt (I);
                              else
                                 Needs_Repair := True;
                              end if;
                           end if;
                        end;
                     end if;
                  end loop;

                  if Hard_Anchor_Added then
                     --  Re-run the global allocator without charging a repair attempt.  Releasing this corner's
                     --  trims can change either adjacent corner's allocation.  Preserve all other cached attempts:
                     --  the next pass already compares both allocated trims bit-for-bit and rebuilds only entries
                     --  whose actual inputs changed.
                     null;
                  else
                     exit Repair_Loop when not Needs_Repair;

                     if Repairs_Used >= Max_Repair_Attempts then
                        for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
                           if not Hard_Anchors (I)
                             and then Cached_Attempt_Valid (I)
                             and then not Cached_Attempts (I).Accepted
                             and then not Cached_Attempts (I).Requires_Hard_Anchor
                           then
                              Hard_Anchors (I) := True;
                              Target_Incoming_Trims (I) := 0.0 * mm;
                              Target_Outgoing_Trims (I) := 0.0 * mm;
                              Clear_Cached_Attempt (I);
                           end if;
                        end loop;

                        exit Repair_Loop;
                     end if;

                     for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
                        if not Hard_Anchors (I)
                          and then Cached_Attempt_Valid (I)
                          and then not Cached_Attempts (I).Accepted
                          and then not Cached_Attempts (I).Requires_Hard_Anchor
                        then
                           declare
                              Shrink : constant Dimensionless :=
                                Predicted_Shrink
                                  (Cached_Attempts (I).Failure_Limit,
                                   Cached_Attempts (I).Failure_Upper,
                                   Repair_Shrink_Factor,
                                   Repair_Safety);
                           begin
                              Target_Incoming_Trims (I) := Target_Incoming_Trims (I) * Shrink;
                              Target_Outgoing_Trims (I) := Target_Outgoing_Trims (I) * Shrink;
                              Clear_Cached_Attempt (I);
                           end;

                           if Target_Incoming_Trims (I) <= 1.0E-9 * mm or else Target_Outgoing_Trims (I) <= 1.0E-9 * mm
                           then
                              Hard_Anchors (I) := True;
                              Target_Incoming_Trims (I) := 0.0 * mm;
                              Target_Outgoing_Trims (I) := 0.0 * mm;
                           end if;
                        end if;
                     end loop;

                     Repairs_Used := Repairs_Used + 1;
                  end if;
               end;
            end loop Repair_Loop;
         end;

         Store_Final_Transitions;
      end Run;
   end Runner;

end Prunt.Motion_Planner.Planner.Corner_Blender;
