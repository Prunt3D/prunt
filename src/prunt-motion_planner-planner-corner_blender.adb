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

with Prunt.Motion_Planner.Stereographic_Curves.Geometry;

package body Prunt.Motion_Planner.Planner.Corner_Blender is

   pragma Extensions_Allowed (On);
   pragma Assert (Dimensionless'Machine_Rounds);

   use Prunt.Motion_Planner.Stereographic_Curves.Geometry;

   procedure Run
     (Block     : aliased in out Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace : not null access Planning_Workspace) is
   begin
      Runner.Run (Block, Motor_Map, Workspace);
   end Run;

   protected body Runner is
      procedure Run
        (Block     : aliased in out Execution_Block;
         Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
         Workspace : not null access Planning_Workspace)
      is
         Transition_Samples   : constant Positive := 65;
         Primitive_Samples    : constant Positive := 17;
         Max_Repair_Attempts  : constant Positive := 3;
         Repair_Shrink_Factor : constant Dimensionless := 0.55;
         Motor_Error_Fraction : constant Dimensionless := 0.25;
         Straight_Tolerance   : constant Dimensionless := 1.0E-9;

         function Active_Axial_Deviation_Maxes return Axial_Deviation_Limits;
         function Active_Circularity return Dimensionless;
         function Active_Shape_Bias return Dimensionless;
         function Clamp_Unit_Dot (Value : Dimensionless) return Dimensionless;
         function Add_Length_Upper (Left, Right : Length) return Length;
         function Transition_Position_Error return Length;
         function Corner_Miss_Limit return Length;
         function Divide_Length_Upper (Value : Length; Divisor : Dimensionless) return Length;
         function Effective_Axial_Deviation (Incoming_Tangent, Outgoing_Tangent : Position_Scale) return Length;
         function Executed_Speed_Upper (Curve : Corner_Transition) return Dimensionless;
         function Hard_Anchor (I : Corners_Index) return Boolean;
         function Is_Finite (Value : Dimensionless) return Boolean;
         function Is_Passthrough_Corner (I : Corners_Index) return Boolean;
         function Multiply_Length_Upper (Value : Length; Factor : Dimensionless) return Length;
         function Point_Distance_Upper (Left, Right : Position) return Length;
         function Predicted_Shrink (Limit, Achieved : Length; Fallback, Safety : Dimensionless) return Dimensionless;
         function Round_Curvature_Up (Value : Curvature) return Curvature;
         function Round_Down (Value : Dimensionless) return Dimensionless;
         function Round_Length_Down_Nonnegative (Value : Length) return Length;
         function Round_Length_Up (Value : Length) return Length;
         function Round_Up (Value : Dimensionless) return Dimensionless;
         function Helix_Primitive_Deviation_Upper_Bound
           (Corner         : Corners_Index;
            Curve          : Corner_Transition;
            Trim_In        : Length;
            Trim_Out       : Length;
            Position_Error : Length) return Length;
         function Line_Corner_Axial_Deviation
           (Curve          : Corner_Transition;
            Start_Point    : Position;
            Corner_Point   : Position;
            Finish_Point   : Position;
            Limits         : Axial_Deviation_Limits;
            Position_Error : Length) return Axial_Deviation_Check;
         function Validate_Transition
           (I : Corners_Index; Curve : Corner_Transition; Trim_In : Length; Trim_Out : Length; Position_Error : Length)
            return Corner_Transition_Attempt;
         function Try_Build_Corner (I : Corners_Index) return Corner_Transition_Attempt;
         function Zero_Corner_Transition (I : Corners_Index) return Corner_Transition;
         procedure Allocate_Segment_Trims;
         procedure Assign_Target_Widths;
         procedure Clear_Cached_Attempt (I : Corners_Index);
         procedure Store_Sharp_Transitions;
         procedure Store_Transition (I : Corners_Index; Transition : Corner_Transition);
         procedure Store_Final_Transitions;

         function Active_Axial_Deviation_Maxes return Axial_Deviation_Limits is
         begin
            case Block.Params.Cornering.Kind is
               when Stereographic =>
                  return Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes;

               when Circular      =>
                  return Block.Params.Cornering.Circular_Params.Axial_Deviation_Maxes;

               when Parabolic     =>
                  return Block.Params.Cornering.Parabolic_Params.Axial_Deviation_Maxes;

               when Biarc         =>
                  return Block.Params.Cornering.Biarc_Params.Axial_Deviation_Maxes;

               when Sharp_SCV     =>
                  return [others => 0.0 * mm];
            end case;
         end Active_Axial_Deviation_Maxes;

         function Active_Circularity return Dimensionless is
         begin
            return
              (case Block.Params.Cornering.Kind is
                 when Stereographic => Block.Params.Cornering.Stereographic_Params.Circularity,
                 when others        => 0.0);
         end Active_Circularity;

         function Active_Shape_Bias return Dimensionless is
         begin
            return
              (case Block.Params.Cornering.Kind is
                 when Stereographic => Block.Params.Cornering.Stereographic_Params.Shape_Bias,
                 when Parabolic     => Block.Params.Cornering.Parabolic_Params.Shape_Bias,
                 when Biarc         => Block.Params.Cornering.Biarc_Params.Shape_Bias,
                 when others        => 0.0);
         end Active_Shape_Bias;

         function Clamp_Unit_Dot (Value : Dimensionless) return Dimensionless is
         begin
            return Dimensionless'Max (-1.0, Dimensionless'Min (1.0, Value));
         end Clamp_Unit_Dot;

         function Is_Finite (Value : Dimensionless) return Boolean is
         begin
            return Value >= -Dimensionless'Last and then Value <= Dimensionless'Last;
         end Is_Finite;

         function Round_Down (Value : Dimensionless) return Dimensionless is
            Inflation : Dimensionless;
            Candidate : Dimensionless;
         begin
            if not Is_Finite (Value) then
               raise Constraint_Error with "non-finite outward-rounding input";
            elsif Value <= -Dimensionless'Last then
               return -Dimensionless'Last;
            else
               Inflation :=
                 Dimensionless'Max (Dimensionless'Model_Small, 2.0 * Dimensionless'Model_Epsilon * abs Value);
               Candidate := Value - Inflation;
               return (if Candidate < Value then Candidate else Dimensionless'Adjacent (Value, -Dimensionless'Last));
            end if;
         end Round_Down;

         function Round_Up (Value : Dimensionless) return Dimensionless is
            Inflation : Dimensionless;
            Candidate : Dimensionless;
         begin
            if not Is_Finite (Value) then
               raise Constraint_Error with "non-finite outward-rounding input";
            elsif Value >= Dimensionless'Last then
               return Dimensionless'Last;
            else
               Inflation :=
                 Dimensionless'Max (Dimensionless'Model_Small, 2.0 * Dimensionless'Model_Epsilon * abs Value);
               Candidate := Value + Inflation;
               return (if Candidate > Value then Candidate else Dimensionless'Adjacent (Value, Dimensionless'Last));
            end if;
         end Round_Up;

         function Round_Length_Down_Nonnegative (Value : Length) return Length is
         begin
            if not (Value >= -Length'Last and then Value <= Length'Last) or else Value <= 0.0 * mm then
               return 0.0 * mm;
            elsif Value >= Length'Last then
               return Length'Last;
            else
               return Length'Max (0.0 * mm, Length'Adjacent (Value, Length'First));
            end if;
         exception
            when Constraint_Error =>
               return 0.0 * mm;
         end Round_Length_Down_Nonnegative;

         function Round_Length_Up (Value : Length) return Length is
         begin
            if not (Value >= -Length'Last and then Value <= Length'Last) or else Value < 0.0 * mm then
               return Length'Last;
            elsif Value = 0.0 * mm then
               return 0.0 * mm;
            elsif Value >= Length'Last then
               return Length'Last;
            else
               return Length'Adjacent (Value, Length'Last);
            end if;
         exception
            when Constraint_Error =>
               return Length'Last;
         end Round_Length_Up;

         function Round_Curvature_Up (Value : Curvature) return Curvature is
            Inflation : Curvature;
            Candidate : Curvature;
         begin
            if not (Value >= -Curvature'Last and then Value <= Curvature'Last) then
               return Curvature'Last;
            elsif Value >= Curvature'Last then
               return Curvature'Last;
            else
               Inflation :=
                 Curvature'Max (Dimensionless'Model_Small / mm, 2.0 * Dimensionless'Model_Epsilon * abs Value);
               Candidate := Value + Inflation;
               return (if Candidate > Value then Candidate else Curvature'Adjacent (Value, Curvature'Last));
            end if;
         exception
            when Constraint_Error =>
               return Curvature'Last;
         end Round_Curvature_Up;

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

         function Executed_Speed_Upper (Curve : Corner_Transition) return Dimensionless is
            Bounds     : constant Unit_Speed_Axial_Velocity_Bounds := Derivative_Bounds (Curve).Velocity;
            Square_Sum : Dimensionless := 0.0;
         begin
            for Axis in Axis_Name loop
               if Bounds (Axis) < 0.0 or else Bounds (Axis) >= Dimensionless'Last then
                  return Dimensionless'Last;
               end if;
               Square_Sum := Round_Up (Square_Sum + Round_Up (Bounds (Axis) ** 2));
            end loop;
            return Round_Up (Dimensionless_Math.Sqrt (Square_Sum));
         exception
            when Constraint_Error =>
               return Dimensionless'Last;
         end Executed_Speed_Upper;

         function Effective_Axial_Deviation (Incoming_Tangent, Outgoing_Tangent : Position_Scale) return Length is
            Turn        : constant Position_Scale := Incoming_Tangent - Outgoing_Tangent;
            Turn_Norm   : Dimensionless := 0.0;
            Result      : Length := Length'Last;
            Constrained : Boolean := False;
         begin
            for Axis in Axis_Name loop
               Turn_Norm := Turn_Norm + Turn (Axis) ** 2;
            end loop;
            Turn_Norm := Dimensionless_Math.Sqrt (Turn_Norm);
            if Turn_Norm <= 128.0 * Dimensionless'Model_Epsilon then
               return Length'Last;
            end if;
            for Axis in Axis_Name loop
               if abs Turn (Axis) > 128.0 * Dimensionless'Model_Epsilon * Turn_Norm then
                  Constrained := True;
                  Result := Length'Min (Result, Active_Axial_Deviation_Maxes (Axis) * (Turn_Norm / abs Turn (Axis)));
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

         function Point_Distance_Upper (Left, Right : Position) return Length is
         begin
            return Point_To_Point_Distance (Left, Right).Upper;
         exception
            when Constraint_Error =>
               return Length'Last;
         end Point_Distance_Upper;

         function Transition_Position_Error return Length is
            Result             : Length := Corner_Transition_Max_Computational_Error;
            Has_Relevant_Motor : Boolean := False;
         begin
            for M in Motor_Name loop
               declare
                  Norm_Squared_Upper : Curvature_To_2 := 0.0 / mm ** 2;
               begin
                  for A in Axis_Name loop
                     if Motor_Map (A, M) = 0.0 * mm then
                        return 0.0 * mm;
                     elsif Motor_Map (A, M) /= Length'Last then
                        Has_Relevant_Motor := True;
                        declare
                           Coefficient_Upper : constant Curvature := Round_Curvature_Up (abs (1.0 / Motor_Map (A, M)));
                           Square_Upper      : constant Curvature_To_2 :=
                             Curvature_To_2'Adjacent (Coefficient_Upper ** 2, Curvature_To_2'Last);
                        begin
                           if Square_Upper > Curvature_To_2'Last - Norm_Squared_Upper then
                              return 0.0 * mm;
                           end if;
                           Norm_Squared_Upper :=
                             Curvature_To_2'Adjacent (Norm_Squared_Upper + Square_Upper, Curvature_To_2'Last);
                        end;
                     end if;
                  end loop;

                  if Norm_Squared_Upper > 0.0 / mm ** 2 then
                     declare
                        Norm_Upper : constant Curvature := Round_Curvature_Up (Norm_Squared_Upper ** (1 / 2));
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

         function Corner_Miss_Limit return Length is
         begin
            case Block.Params.Cornering.Kind is
               when Stereographic =>
                  return Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max;

               when Circular      =>
                  return Block.Params.Cornering.Circular_Params.Corner_Miss_Distance_Max;

               when Parabolic     =>
                  return Block.Params.Cornering.Parabolic_Params.Corner_Miss_Distance_Max;

               when Biarc         =>
                  return Block.Params.Cornering.Biarc_Params.Corner_Miss_Distance_Max;

               when Sharp_SCV     =>
                  return 0.0 * mm;
            end case;
         end Corner_Miss_Limit;

         function Predicted_Shrink (Limit, Achieved : Length; Fallback, Safety : Dimensionless) return Dimensionless is
         begin
            if Limit > 0.0 * mm and then Achieved > Limit and then Achieved < 1.0E80 * mm then
               return Dimensionless'Min (0.90, Dimensionless'Max (0.25, Safety * (Limit / Achieved)));
            else
               return Fallback;
            end if;
         end Predicted_Shrink;

         function Hard_Anchor (I : Corners_Index) return Boolean is
            Any_Deviation_Allowed : Boolean := False;
         begin
            for Axis in Axis_Name loop
               Any_Deviation_Allowed := Any_Deviation_Allowed or else Active_Axial_Deviation_Maxes (Axis) > 0.0 * mm;
            end loop;
            if I = Block.Corners'First or else I = Block.Corners'Last then
               return True;
            elsif Is_Passthrough_Corner (I) then
               return True;
            elsif not Any_Deviation_Allowed then
               return True;
            elsif Primitive_Length (Block'Access, I) <= 0.0 * mm
              or else Primitive_Length (Block'Access, I + 1) <= 0.0 * mm
            then
               return True;
            elsif Block.Corner_Dwell_Times (I) /= 0.0 * s then
               return True;
            elsif Block.Params.Cornering.Kind in Circular | Parabolic
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
                  Secondary_Sine   : constant Dimensionless := ((1.0 - Dot_Tangents) / 2.0) ** (1 / 2);
               begin
                  return
                    Angle_Elementary_Functions.Sin (90.0 * deg - 0.5 * Corner_Transition_Min_Corner_Angle)
                    < Secondary_Sine;
               end;
            end if;
         end Hard_Anchor;

         function Is_Passthrough_Corner (I : Corners_Index) return Boolean is
         begin
            if I = Block.Corners'First
              or else I = Block.Corners'Last
              or else Primitive_Length (Block'Access, I) <= 0.0 * mm
              or else Primitive_Length (Block'Access, I + 1) <= 0.0 * mm
              or else Block.Corner_Dwell_Times (I) /= 0.0 * s
            then
               return False;
            end if;

            return
              1.0
              - Clamp_Unit_Dot
                  (Dot
                     (Primitive_Direction_At_Distance (Block'Access, I, Primitive_Length (Block'Access, I)),
                      Primitive_Direction_At_Distance (Block'Access, I + 1, 0.0 * mm)))
              <= Straight_Tolerance;
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
               if I = Block.Corners'First
                 or else I = Block.Corners'Last
                 or else Primitive_Length (Block'Access, I) <= 0.0 * mm
                 or else Primitive_Length (Block'Access, I + 1) <= 0.0 * mm
                 or else Block.Corner_Dwell_Times (I) /= 0.0 * s
               then
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

         function Helix_Primitive_Deviation_Upper_Bound
           (Corner         : Corners_Index;
            Curve          : Corner_Transition;
            Trim_In        : Length;
            Trim_Out       : Length;
            Position_Error : Length) return Length
         is
            Incoming_Length : constant Length := Primitive_Length (Block'Access, Corner);
            Result          : Length := 0.0 * mm;

            function Distance_To_Line_Section
              (Point            : Position;
               Finishing_Corner : Finishing_Corners_Index;
               Start_Distance   : Length;
               Distance         : Length) return Length;
            function Distance_To_Primitive_Section
              (Point            : Position;
               Finishing_Corner : Finishing_Corners_Index;
               Start_Distance   : Length;
               Distance         : Length) return Length;

            function Distance_To_Line_Section
              (Point            : Position;
               Finishing_Corner : Finishing_Corners_Index;
               Start_Distance   : Length;
               Distance         : Length) return Length
            is
               Section_End_Distance : constant Length := Start_Distance + Distance;
               Start_Point          : constant Position :=
                 Primitive_Point_At_Distance (Block'Access, Finishing_Corner, Start_Distance);
               End_Point            : constant Position :=
                 Primitive_Point_At_Distance (Block'Access, Finishing_Corner, Section_End_Distance);
               Offset               : constant Position_Offset := End_Point - Start_Point;
               Den                  : constant Area := Dot (Offset, Offset);
               Endpoint_Upper       : constant Length :=
                 Length'Min (Point_Distance_Upper (Point, Start_Point), Point_Distance_Upper (Point, End_Point));
            begin
               if Den <= 0.0 * mm ** 2 then
                  return Endpoint_Upper;
               else
                  declare
                     U                  : constant Dimensionless :=
                       Dimensionless'Max (0.0, Dimensionless'Min (1.0, Dot (Point - Start_Point, Offset) / Den));
                     Candidate_Distance : constant Length :=
                       Length'Max (Start_Distance, Length'Min (Section_End_Distance, Start_Distance + U * Distance));
                  begin
                     --  The rounded projection parameter is used only to select one actual point in the reference
                     --  primitive section.  Its distance is therefore an upper bound on the section minimum even if
                     --  the projection arithmetic itself is poorly conditioned.
                     return
                       Length'Min
                         (Endpoint_Upper,
                          Point_Distance_Upper
                            (Point, Primitive_Point_At_Distance (Block'Access, Finishing_Corner, Candidate_Distance)));
                  end;
               end if;
            exception
               when Constraint_Error =>
                  return Length'Last;
            end Distance_To_Line_Section;

            function Distance_To_Primitive_Section
              (Point            : Position;
               Finishing_Corner : Finishing_Corners_Index;
               Start_Distance   : Length;
               Distance         : Length) return Length
            is
               Best       : Length := Length'Last;
               Best_Index : Natural := 0;

               function Distance_At_Local (Local_Distance : Length) return Length;

               function Distance_At_Local (Local_Distance : Length) return Length is
                  Clamped_Local : constant Length := Length'Max (0.0 * mm, Length'Min (Distance, Local_Distance));
               begin
                  return
                    Point_Distance_Upper
                      (Point,
                       Primitive_Point_At_Distance (Block'Access, Finishing_Corner, Start_Distance + Clamped_Local));
               end Distance_At_Local;
            begin
               if Distance <= 0.0 * mm then
                  return
                    Point_Distance_Upper
                      (Point, Primitive_Point_At_Distance (Block'Access, Finishing_Corner, Start_Distance));
               elsif Block.Primitives (Finishing_Corner).Kind = Line_Primitive_Kind then
                  return Distance_To_Line_Section (Point, Finishing_Corner, Start_Distance, Distance);
               end if;

               for S in 0 .. Primitive_Samples - 1 loop
                  declare
                     Local_Distance  : constant Length :=
                       Distance * Dimensionless (S) / Dimensionless (Primitive_Samples - 1);
                     Sample_Distance : constant Length := Distance_At_Local (Local_Distance);
                  begin
                     if Sample_Distance < Best then
                        Best := Sample_Distance;
                        Best_Index := S;
                     end if;
                  end;
               end loop;

               declare
                  Left_Index  : constant Natural := (if Best_Index = 0 then 0 else Best_Index - 1);
                  Right_Index : constant Natural :=
                    (if Best_Index >= Primitive_Samples - 1 then Primitive_Samples - 1 else Best_Index + 1);
                  Left        : Length :=
                    Distance * Dimensionless (Left_Index) / Dimensionless (Primitive_Samples - 1);
                  Right       : Length :=
                    Distance * Dimensionless (Right_Index) / Dimensionless (Primitive_Samples - 1);
               begin
                  for Step in 1 .. 24 loop
                     declare
                        First  : constant Length := Left + (Right - Left) / 3.0;
                        Second : constant Length := Right - (Right - Left) / 3.0;
                     begin
                        if Distance_At_Local (First) < Distance_At_Local (Second) then
                           Right := Second;
                        else
                           Left := First;
                        end if;
                     end;
                  end loop;

                  Best := Length'Min (Best, Distance_At_Local (0.5 * (Left + Right)));
               end;

               return Best;
            end Distance_To_Primitive_Section;
         begin
            if Arc_Length (Curve) <= 0.0 * mm then
               return Length'Last;
            end if;

            for B in 0 .. Transition_Samples - 1 loop
               declare
                  Parameter         : constant Transition_Parameter :=
                    Transition_Parameter (Dimensionless (B) / Dimensionless (Transition_Samples - 1));
                  Point             : constant Position := Point_At_Parameter (Curve, Parameter);
                  Incoming_Distance : constant Length :=
                    Distance_To_Primitive_Section (Point, Corner, Incoming_Length - Trim_In, Trim_In);
                  Outgoing_Distance : constant Length :=
                    Distance_To_Primitive_Section (Point, Corner + 1, 0.0 * mm, Trim_Out);
               begin
                  Result := Length'Max (Result, Length'Min (Incoming_Distance, Outgoing_Distance));
               end;
            end loop;

            declare
               Curve_Sample_Gap        : constant Length :=
                 Multiply_Length_Upper
                   (Divide_Length_Upper (Arc_Length (Curve), 2.0 * Dimensionless (Transition_Samples - 1)),
                    Executed_Speed_Upper (Curve));
               Primitive_Sample_Gap    : constant Length :=
                 Divide_Length_Upper (Length'Max (Trim_In, Trim_Out), 2.0 * Dimensionless (Primitive_Samples - 1));
               --  The recomputed component bounds supply Curve_Sample_Gap directly for the executed evaluator. The
               --  two-sided ideal-to-executed allowance below covers both locally built and imported V1 curves.
               Executed_Position_Error : constant Length := Multiply_Length_Upper (Position_Error, 2.0);
            begin
               return
                 Add_Length_Upper
                   (Add_Length_Upper (Add_Length_Upper (Result, Curve_Sample_Gap), Primitive_Sample_Gap),
                    Executed_Position_Error);
            end;
         end Helix_Primitive_Deviation_Upper_Bound;

         function Line_Corner_Axial_Deviation
           (Curve          : Corner_Transition;
            Start_Point    : Position;
            Corner_Point   : Position;
            Finish_Point   : Position;
            Limits         : Axial_Deviation_Limits;
            Position_Error : Length) return Axial_Deviation_Check
         is
            Bounds  : constant Unit_Speed_Axial_Derivative_Bounds := Derivative_Bounds (Curve);
            Arc_Gap : constant Length :=
              Divide_Length_Upper (Arc_Length (Curve), 2.0 * Dimensionless (Transition_Samples - 1));

            function On_Line_With_Factor (Point, First, Last : Position; Factor : Dimensionless) return Boolean;

            function On_Line_With_Factor (Point, First, Last : Position; Factor : Dimensionless) return Boolean is
               Q_Lower : Dimensionless := 0.0;
               Q_Upper : Dimensionless := 1.0;
            begin
               for Axis in Axis_Name loop
                  declare
                     Structurally_Constant : constant Boolean := Axis_Is_Structurally_Constant (Curve, Axis);
                     Margin                : constant Length :=
                       (if Structurally_Constant
                        then 0.0 * mm
                        else
                          Add_Length_Upper
                            (Multiply_Length_Upper (Arc_Gap, Bounds.Velocity (Axis)),
                             Multiply_Length_Upper (Position_Error, 2.0)));
                     Budget                : constant Length := Factor * Limits (Axis) - Margin;
                     Offset                : constant Length := Point (Axis) - First (Axis);
                     Line_Delta            : constant Length := Last (Axis) - First (Axis);
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
            end On_Line_With_Factor;

            function Sample_Ratio (Point : Position) return Dimensionless;

            function Sample_Ratio (Point : Position) return Dimensionless is
               function Feasible (Factor : Dimensionless) return Boolean
               is (On_Line_With_Factor (Point, Start_Point, Corner_Point, Factor)
                   or else On_Line_With_Factor (Point, Corner_Point, Finish_Point, Factor));

               Lower : Dimensionless := 0.0;
               Upper : Dimensionless := 1.0;
            begin
               if Feasible (1.0) then
                  return 1.0;
               end if;
               while Upper < 1.0E12 and then not Feasible (Upper) loop
                  Lower := Upper;
                  Upper := 2.0 * Upper;
               end loop;
               if not Feasible (Upper) then
                  return Dimensionless'Last;
               end if;
               for Iteration in 1 .. 40 loop
                  declare
                     Middle : constant Dimensionless := 0.5 * (Lower + Upper);
                  begin
                     if Feasible (Middle) then
                        Upper := Middle;
                     else
                        Lower := Middle;
                     end if;
                  end;
               end loop;
               return Round_Up (Upper);
            end Sample_Ratio;

            Worst : Dimensionless := 0.0;
         begin
            if Arc_Length (Curve) <= 0.0 * mm then
               return (Pass => False, Worst_Ratio => Dimensionless'Last);
            end if;
            for I in 0 .. Transition_Samples - 1 loop
               declare
                  Parameter : constant Transition_Parameter :=
                    Transition_Parameter (Dimensionless (I) / Dimensionless (Transition_Samples - 1));
                  Ratio     : constant Dimensionless := Sample_Ratio (Point_At_Parameter (Curve, Parameter));
               begin
                  Worst := Dimensionless'Max (Worst, Ratio);
                  if Worst > 1.0 then
                     return (Pass => False, Worst_Ratio => Worst);
                  end if;
               end;
            end loop;
            return (Pass => True, Worst_Ratio => Worst);
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
                     In_Bias             : constant Dimensionless := Dimensionless'Max (0.0, -Active_Shape_Bias);
                     Out_Bias            : constant Dimensionless := Dimensionless'Max (0.0, Active_Shape_Bias);
                     In_Mult             : constant Dimensionless := 1.0 + 0.75 * In_Bias;
                     Out_Mult            : constant Dimensionless := 1.0 + 0.75 * Out_Bias;
                     Adjacent_Length     : constant Length :=
                       Length'Min (Primitive_Length (Block'Access, I), Primitive_Length (Block'Access, I + 1));
                     Effective_Deviation : constant Length :=
                       Effective_Axial_Deviation (Incoming_Tangent, Outgoing_Tangent);
                     Base_Width          : Length;
                  begin
                     case Block.Params.Cornering.Kind is
                        when Stereographic     =>
                           declare
                              Cos_Half_Theta : constant Dimensionless :=
                                Dimensionless_Math.Sqrt
                                  (Dimensionless'Min (1.0, Dimensionless'Max (0.0, 0.5 + 0.5 * Dot_Tangents)));
                              Bend_Factor    : constant Dimensionless :=
                                Dimensionless'Max (0.025, 1.0 - Cos_Half_Theta);
                           begin
                              Base_Width := (1.35 - 0.49 * Active_Circularity) * Effective_Deviation / Bend_Factor;
                              Target_Incoming_Trims (I) := Base_Width * In_Mult;
                              Target_Outgoing_Trims (I) := Base_Width * Out_Mult;
                           end;

                        when Circular          =>
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

                        when Parabolic | Biarc =>
                           declare
                              Trim_Max : constant Length :=
                                (case Block.Params.Cornering.Kind is
                                   when Parabolic => Block.Params.Cornering.Parabolic_Params.Trim_Max,
                                   when Biarc     => Block.Params.Cornering.Biarc_Params.Trim_Max,
                                   when others    => 0.0 * mm);
                           begin
                              Base_Width := Length'Min (Adjacent_Length, Trim_Max);
                              Target_Incoming_Trims (I) :=
                                Length'Min (Trim_Max, Multiply_Length_Upper (Base_Width, In_Mult));
                              Target_Outgoing_Trims (I) :=
                                Length'Min (Trim_Max, Multiply_Length_Upper (Base_Width, Out_Mult));
                           end;

                        when Sharp_SCV         =>
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
           (I : Corners_Index; Curve : Corner_Transition; Trim_In : Length; Trim_Out : Length; Position_Error : Length)
            return Corner_Transition_Attempt
         is
            Incoming_Length     : constant Length := Primitive_Length (Block'Access, I);
            Start_Distance      : constant Length := Incoming_Length - Trim_In;
            Start_Point         : constant Position := Primitive_Point_At_Distance (Block'Access, I, Start_Distance);
            End_Point           : constant Position := Primitive_Point_At_Distance (Block'Access, I + 1, Trim_Out);
            Deviation_Limits    : constant Axial_Deviation_Limits := Active_Axial_Deviation_Maxes;
            Deviation_Proxy     : constant Length :=
              Effective_Axial_Deviation
                (Primitive_Direction_At_Distance (Block'Access, I, Incoming_Length),
                 Primitive_Direction_At_Distance (Block'Access, I + 1, 0.0 * mm));
            Miss_Limit          : constant Length := Corner_Miss_Limit;
            Line_Line           : constant Boolean :=
              Block.Primitives (I).Kind = Line_Primitive_Kind
              and then Block.Primitives (I + 1).Kind = Line_Primitive_Kind;
            Axial_Check         : constant Axial_Deviation_Check :=
              (if Line_Line
               then
                 Line_Corner_Axial_Deviation
                   (Curve, Start_Point, Block.Corners (I), End_Point, Deviation_Limits, Position_Error)
               else (Pass => False, Worst_Ratio => Dimensionless'Last));
            Primitive_Deviation : constant Length :=
              (if Line_Line
               then 0.0 * mm
               else Helix_Primitive_Deviation_Upper_Bound (I, Curve, Trim_In, Trim_Out, Position_Error));
            Deviation_Passes    : Boolean := Axial_Check.Pass;
            Corner_Distance     : Length := Length'Last;
            Corner_Passes       : Boolean;
            Envelope_Passes     : Boolean := True;
            Envelope_Numeric    : Boolean := True;
            Failure_Limit       : Length := Deviation_Proxy;
            Failure_Upper       : Length :=
              (if Axial_Check.Worst_Ratio >= Dimensionless'Last
               then Length'Last
               else Multiply_Length_Upper (Deviation_Proxy, Axial_Check.Worst_Ratio));
            Accepted            : Boolean;

            function Finite_Length (Value : Length) return Boolean
            is (Value >= Length'First and then Value <= Length'Last);

            function Side_Envelope_Passes
              (First_Distance, Last_Distance : Length; Numeric_Passes : out Boolean) return Boolean;

            function Side_Envelope_Passes
              (First_Distance, Last_Distance : Length; Numeric_Passes : out Boolean) return Boolean
            is
               Envelope : constant Position_Envelope :=
                 Certified_Position_Envelope (Curve, First_Distance, Last_Distance);
               Error    : constant Length := Position_Error_Bound (Curve);
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
                  begin
                     if not Finite_Length (Expanded_Lower)
                       or else not Finite_Length (Expanded_Upper)
                       or else Expanded_Lower > Expanded_Upper
                     then
                        return False;
                     elsif Expanded_Lower < Block.Params.Lower_Pos_Limit (Axis)
                       or else Expanded_Upper > Block.Params.Upper_Pos_Limit (Axis)
                     then
                        Numeric_Passes := True;
                        return False;
                     end if;
                  end;
               end loop;
               Numeric_Passes := True;
               return True;
            exception
               when Constraint_Error =>
                  Numeric_Passes := False;
                  return False;
            end Side_Envelope_Passes;
         begin
            if Arc_Length (Curve) <= 0.0 * mm
              or else Split_Distance (Curve) < 0.0 * mm
              or else Split_Distance (Curve) > Arc_Length (Curve)
            then
               return
                 (Accepted             => False,
                  Requires_Hard_Anchor => True,
                  Trim_In              => Trim_In,
                  Trim_Out             => Trim_Out,
                  Failure_Limit        => 0.0 * mm,
                  Failure_Upper        => Length'Last);
            end if;

            --  Every sampled value is an actual represented point.  Its distance plus the certified evaluator error
            --  is consequently a safe upper bound on the transition's minimum distance from the commanded corner;
            --  no assumption about where that minimum occurs is needed.
            for Sample in 0 .. Transition_Samples - 1 loop
               declare
                  Parameter : constant Transition_Parameter :=
                    Transition_Parameter (Dimensionless (Sample) / Dimensionless (Transition_Samples - 1));
               begin
                  Corner_Distance :=
                    Length'Min
                      (Corner_Distance,
                       Add_Length_Upper
                         (Point_Distance_Upper (Point_At_Parameter (Curve, Parameter), Block.Corners (I)),
                          Position_Error_Bound (Curve)));
               end;
            end loop;
            Corner_Passes := Corner_Distance <= Miss_Limit;

            if not Line_Line then
               Deviation_Passes := True;
               for Axis in Axis_Name loop
                  Deviation_Passes := Deviation_Passes and then Primitive_Deviation <= Deviation_Limits (Axis);
               end loop;
               Failure_Upper := Primitive_Deviation;
            end if;

            declare
               Incoming_Numeric : Boolean;
               Outgoing_Numeric : Boolean;
               Incoming_Passes  : constant Boolean :=
                 Side_Envelope_Passes (0.0 * mm, Split_Distance (Curve), Incoming_Numeric);
               Outgoing_Passes  : constant Boolean :=
                 Side_Envelope_Passes (Split_Distance (Curve), Arc_Length (Curve), Outgoing_Numeric);
            begin
               Envelope_Passes := Incoming_Passes and Outgoing_Passes;
               Envelope_Numeric := Incoming_Numeric and Outgoing_Numeric;
            end;
            Accepted := Deviation_Passes and Corner_Passes and Envelope_Passes;

            if Deviation_Passes
              or else
                (not Corner_Passes
                 and then
                   Predicted_Shrink (Miss_Limit, Corner_Distance, Repair_Shrink_Factor, 0.85)
                   < Predicted_Shrink (Failure_Limit, Failure_Upper, Repair_Shrink_Factor, 0.85))
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
               Incoming_Length  : constant Length := Primitive_Length (Block'Access, I);
               Start_Distance   : constant Length := Incoming_Length - Trim_In;
               Start_Point      : constant Position := Primitive_Point_At_Distance (Block'Access, I, Start_Distance);
               End_Point        : constant Position := Primitive_Point_At_Distance (Block'Access, I + 1, Trim_Out);
               Deviation_Proxy  : constant Length :=
                 Effective_Axial_Deviation
                   (Primitive_Direction_At_Distance (Block'Access, I, Incoming_Length),
                    Primitive_Direction_At_Distance (Block'Access, I + 1, 0.0 * mm));
               Incoming_Tangent : constant Position_Scale :=
                 Primitive_Direction_At_Distance (Block'Access, I, Incoming_Length);
               Outgoing_Tangent : constant Position_Scale :=
                 Primitive_Direction_At_Distance (Block'Access, I + 1, 0.0 * mm);
               Position_Error   : constant Length := Transition_Position_Error;
               Result           : Construction_Result;
            begin
               case Block.Params.Cornering.Kind is
                  when Stereographic =>
                     Result :=
                       Create_Stereographic
                         ((Start                  =>
                             (Point => Start_Point,
                              Jet   => Primitive_Derivative_Jets_At_Distance (Block'Access, I, Start_Distance)),
                           Finish                 =>
                             (Point => End_Point,
                              Jet   => Primitive_Derivative_Jets_At_Distance (Block'Access, I + 1, Trim_Out)),
                           Maximum_Position_Error => Position_Error,
                           Maximum_Arc_Length     => Trim_In + Trim_Out,
                           Allow_Bulge            => False));

                  when Circular      =>
                     Result :=
                       Create_Circular
                         (Start_Point,
                          Block.Corners (I),
                          End_Point,
                          Block.Params.Cornering.Circular_Params.Radius_Max);

                  when Parabolic     =>
                     Result :=
                       Create_Parabolic
                         (Start_Point, Block.Corners (I), End_Point, Maximum_Length => Trim_In + Trim_Out);

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
                     Allocated_Incoming_Trims (I) := Cached_Attempts (I).Trim_In;
                     Allocated_Outgoing_Trims (I) := Cached_Attempts (I).Trim_Out;
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

         for I in Block.Corners'Range loop
            Cached_Attempts (I) := (others => <>);
            Cached_Attempt_Valid (I) := False;
         end loop;

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
                           Attempt : constant Corner_Transition_Attempt := Try_Build_Corner (I);
                        begin
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
                                   0.85);
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
