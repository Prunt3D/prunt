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

package body Prunt.Motion_Planner.Planner.Kinematic_Limiter is

   pragma Extensions_Allowed (On);

   procedure Run
     (Block     : aliased in out Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace : not null access Planning_Workspace;
      Result    : out Profile_Planning_Result)
   is
      Velocity_Change_Tolerance : constant Velocity := 1.0E-3 * mm / s;
      Cleanup_Iterations        : constant Positive := 4;
      Deviation                 : constant Length := Extrusion_Deviation (Block'Access);

      procedure Clamp_Corner (Corner : Corners_Index; Limit : Velocity; Changed : in out Boolean);
      --  Clamp Limit according to the selected transition's explicit junction policy.

      procedure Apply_Corner_Transition_Limits (Corner : Corners_Index; Limit : in out Velocity);
      procedure Forward_Pass (Monotone : Boolean; Changed : in out Boolean);
      procedure Fill_Static_Caches;
      function Reachable_Cached_Profile_Window_Velocity
        (Finishing_Corner : Finishing_Corners_Index; Fixed_Vel : Velocity; Forward : Boolean) return Velocity;
      procedure Reverse_Pass (Changed : in out Boolean);
      function Static_Corner_Limit (Corner : Corners_Index) return Velocity;

      Static_Corner_Limits : Block_Corner_Velocity_Limits renames
        Workspace.Static_Corner_Limits (Block.Corner_Velocity_Limits'Range);
      Window_Evaluations   : Profile_Window_Evaluation_Cache renames
        Workspace.Window_Evaluations (Block.Limited_Segment_Feedrates'Range);

      procedure Apply_Corner_Transition_Limits (Corner : Corners_Index; Limit : in out Velocity) is
         Transition : constant Corner_Transition_Evaluator := Block.Corner_Transitions (Corner);
         Bounds     : constant Unit_Speed_Axial_Derivative_Bounds := Workspace.Corner_Derivative_Bounds (Corner);
      begin
         case Policy (Transition) is
            when Hard_Stop              =>
               Limit := 0.0 * mm / s;

            when Passthrough            =>
               null;

            when Square_Corner_Velocity =>
               Limit := Velocity'Min (Limit, Junction_Velocity_Limit (Transition));

            when Derivative_Bounded     =>
               if Arc_Length (Transition) <= 0.0 * mm then
                  Limit := 0.0 * mm / s;
               else
                  Limit := Constant_Speed_Axial_Ceiling (Block.Params, Bounds, Limit);
                  Limit := Motor_Delta_Ceiling_For_Projection (Block.Params, Spatial_Motor_Map (Motor_Map), Limit);
               end if;
         end case;
         if Workspace.Extrusion_Stops (Corner) then
            Limit := 0.0 * mm / s;
         else
            --  Bound both possible junction coasts, including motor deltas. Unrelated density transitions at the far
            --  ends of these segments must not constrain this corner's velocity.
            for I in Corner .. Corner + 1 loop
               declare
                  Total  : constant Length := Segment_Total_Distance (Block'Access, I);
                  Width  : constant Length :=
                    Length'Min
                      (Total,
                       Length'Max
                         (Workspace.Extrusion_Widths (Corner),
                          (if I = Corner
                           then Segment_End_Transition_Distance (Block'Access, I)
                           else Segment_Start_Transition_Distance (Block'Access, I))));
                  Window : constant Profile_Window :=
                    (Start_Distance => (if I = Corner then Total - Width else 0.0 * mm), Distance => Width);
               begin
                  Limit :=
                    Constant_Speed_Axial_Ceiling
                      (Block.Params,
                       Extrusion_Bounds (Block'Access, Workspace, I, Window.Start_Distance, Window.Distance),
                       Limit);
                  Limit := Motor_Delta_Ceiling_For_Window (Block'Access, Workspace, Motor_Map, I, Window, Limit);
               end;
            end loop;
         end if;
      end Apply_Corner_Transition_Limits;

      procedure Clamp_Corner (Corner : Corners_Index; Limit : Velocity; Changed : in out Boolean) is
      begin
         if Limit < Block.Corner_Velocity_Limits (Corner) then
            if Block.Corner_Velocity_Limits (Corner) - Limit > Velocity_Change_Tolerance then
               Changed := True;
            end if;
            Block.Corner_Velocity_Limits (Corner) := Limit;
         end if;
      end Clamp_Corner;

      function Static_Corner_Limit (Corner : Corners_Index) return Velocity is
         Limit : Velocity :=
           Velocity'Min (Block.Limited_Segment_Feedrates (Corner), Block.Limited_Segment_Feedrates (Corner + 1));
      begin
         Apply_Corner_Transition_Limits (Corner, Limit);
         Limit := Velocity'Min (Limit, Workspace.Extrusion_Ceilings (Corner));

         --  A junction speed must be admissible for a profile on both sides. Otherwise the forward pass can arrive
         --  faster than every next-window ceiling, report zero reachability, and permanently clamp the following
         --  junction to a stop even though lowering this junction slightly gives a through profile.
         for I in Corner .. Corner + 1 loop
            declare
               Ceiling : Velocity := 0.0 * mm / s;
            begin
               for Eval of Window_Evaluations (I) loop
                  if Eval.Valid then
                     Ceiling := Velocity'Max (Ceiling, Eval.Max_Vel);
                  end if;
               end loop;
               Limit := Velocity'Min (Limit, Ceiling);
            end;
         end loop;

         if Block.Corner_Dwell_Times (Corner) /= 0.0 * s then
            pragma Assert (Limit = 0.0 * mm / s);
         end if;

         return Limit;
      end Static_Corner_Limit;

      procedure Forward_Pass (Monotone : Boolean; Changed : in out Boolean) is
      begin
         for I in Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
            declare
               Limit     : constant Velocity := Static_Corner_Limits (I);
               Reachable : constant Velocity :=
                 Velocity'Min
                   (Limit,
                    Reachable_Cached_Profile_Window_Velocity
                      (I, Block.Corner_Velocity_Limits (I - 1), Forward => True));
            begin
               if Monotone then
                  Clamp_Corner (I, Reachable, Changed);
               else
                  Block.Corner_Velocity_Limits (I) := Reachable;
               end if;
            end;
         end loop;
      end Forward_Pass;

      procedure Fill_Static_Caches is
      begin
         Static_Corner_Limits := [others => 0.0 * mm / s];

         for I in Block.Limited_Segment_Feedrates'Range loop
            declare
               Windows : constant Profile_Window_Candidates :=
                 Segment_Profile_Window_Candidates (Block'Access, Workspace, I);
            begin
               for W in Profile_Window_Candidate_Index loop
                  if (for all Previous in Profile_Window_Candidate_Index'First .. W - 1 =>
                        Windows (Previous) /= Windows (W))
                  then
                     Window_Evaluations (I) (W) :=
                       Evaluate_Profile_Window
                         (Block'Access, Workspace, Motor_Map, I, Windows (W), Block.Limited_Segment_Feedrates (I));
                  else
                     --  Geometry and E cuts often coincide. Do not repeat certification or reachability solves.
                     Window_Evaluations (I) (W) := (others => <>);
                  end if;
               end loop;
            end;
         end loop;

         for I in Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
            Static_Corner_Limits (I) := Static_Corner_Limit (I);
         end loop;
      end Fill_Static_Caches;

      function Reachable_Cached_Profile_Window_Velocity
        (Finishing_Corner : Finishing_Corners_Index; Fixed_Vel : Velocity; Forward : Boolean) return Velocity
      is
         Best  : Velocity := 0.0 * mm / s;
         Total : constant Length := Segment_Total_Distance (Block'Access, Finishing_Corner);
      begin
         for W in Profile_Window_Candidate_Index loop
            declare
               Eval              : Profile_Window_Evaluation renames Window_Evaluations (Finishing_Corner) (W);
               Constant_Distance : constant Length :=
                 (if Forward
                  then Eval.Window.Start_Distance
                  else Total - Eval.Window.Start_Distance - Eval.Window.Distance);
               Reachable         : Velocity;
            begin
               if Eval.Valid
                 and then Eval.Max_Vel >= Fixed_Vel
                 and then (Constant_Distance <= 0.0 * mm or else Fixed_Vel > 0.0 * mm / s)
               then
                  Reachable := Reachable_Velocity (Fixed_Vel, Eval.Max_Vel, Eval.Window.Distance, Eval.Limits);
                  Best := Velocity'Max (Best, Reachable);
               end if;
            end;
         end loop;

         return Best;
      end Reachable_Cached_Profile_Window_Velocity;

      procedure Reverse_Pass (Changed : in out Boolean) is
      begin
         for I in reverse Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
            Clamp_Corner
              (I,
               Reachable_Cached_Profile_Window_Velocity
                 (I + 1, Block.Corner_Velocity_Limits (I + 1), Forward => False),
               Changed);
         end loop;
      end Reverse_Pass;

      Changed : Boolean := False;

   begin
      Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'First) := 0.0 * mm / s;
      Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'Last) := 0.0 * mm / s;
      --  Reset junction state for each solve, including replans after homing feedrate adjustments.
      Workspace.Extrusion_Widths (Block.Corners'Range) := [others => 0.0 * mm];
      Workspace.Check_Extrusion_Position :=
        (for all I in Block.Corners'Range =>
           Block.Corners (I) (E_Axis) in Block.Params.Bounds.Lower_E .. Block.Params.Bounds.Upper_E);
      Workspace.Extrusion_Stops (Block.Corners'Range) := [others => False];
      Workspace.Extrusion_Ceilings (Block.Corners'Range) := [others => 0.0 * mm / s];
      Block.Extrusion_Junction_Corrections := [others => <>];
      Workspace.Extrusion_Deviations (Block.Corners'Range) := [others => 0.0 * mm];
      for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
         declare
            Change : constant Dimensionless := abs (Block.Extrusion_Densities (I + 1) - Block.Extrusion_Densities (I));
         begin
            if Block.Params.Extrusion_Cornering.Kind = Smooth_Deviation
              and then Change > 0.0
              and then Policy (Block.Corner_Transitions (I)) /= Hard_Stop
            then
               --  Reserve a nonoverlapping extent for each side of the junction. The joint speed solver will reduce
               --  this to the extent of E's generated velocity transition, enforcing its exact deviation.
               Workspace.Extrusion_Widths (I) :=
                 0.5
                 * Length'Min (Segment_Total_Distance (Block'Access, I), Segment_Total_Distance (Block'Access, I + 1));
               Workspace.Extrusion_Stops (I) :=
                 Deviation <= 0.0 * mm or else Workspace.Extrusion_Widths (I) <= 1.0E-12 * mm;
               if Workspace.Extrusion_Stops (I) then
                  Workspace.Extrusion_Widths (I) := 0.0 * mm;
               end if;
            end if;
         end;
      end loop;

      for I in Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
         declare
            Ceiling : Velocity :=
              Velocity'Min (Block.Limited_Segment_Feedrates (I), Block.Limited_Segment_Feedrates (I + 1));
         begin
            Apply_Corner_Transition_Limits (I, Ceiling);
            Plan_Extrusion_Junction (Block, Workspace, I, Ceiling);
         end;
      end loop;
      Fill_Static_Caches;

      --  Forward pass: Iterate from the second corner to the second-to-last corner. This pass calculates the maximum
      --  corner velocity based on:
      --
      --  1. The corner transition's explicit policy: a hard stop, an unconstrained straight passthrough, a Sharp_SCV
      --     angular velocity cap, or the certified derivative bounds of a curved transition. Discontinuities waived by
      --     a C1 or C0 family are intentionally not treated as finite higher derivatives at the junction.
      --
      --  2. The maximum velocity achievable by accelerating from the previous corner's velocity over the length of the
      --     segment.
      --
      --  3. The maximum velocity allowed in the segment leading in to and out of the corner.
      --
      --  In this pass we are only concerned about the maximum reachable velocity under the above constraints.
      --  Deceleration constraints are added in the reverse pass.
      Forward_Pass (Monotone => False, Changed => Changed);

      --  Reverse pass: Iterate in reverse from the second-to-last corner to the second corner. For each corner, check
      --  if it's possible to decelerate from its current velocity limit to the next corner's velocity limit over the
      --  length of the connecting segment, if it is not then reduce the corner velocity so that it is possible.
      Reverse_Pass (Changed);

      for Iteration in 1 .. Cleanup_Iterations loop
         Changed := False;
         Forward_Pass (Monotone => True, Changed => Changed);
         Reverse_Pass (Changed);
         exit when not Changed;
      end loop;
      for I in Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
         declare
            Transition : constant Extrusion_Transition_Result :=
              Extrusion_Transition (Block'Access, Workspace, I, Block.Corner_Velocity_Limits (I));
         begin
            if not Transition.Valid or else Transition.Half_Distance > Workspace.Extrusion_Widths (I) then
               Result := (Valid => False, Failed_Segment => I);
               return;
            end if;
            Block.Extrusion_Junction_Corrections (I) := Transition.Profile;
            Workspace.Extrusion_Deviations (I) := Transition.Deviation;
         end;
      end loop;
      Result := (Valid => True);
   end Run;

end Prunt.Motion_Planner.Planner.Kinematic_Limiter;
