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

package body Prunt.Motion_Planner.Planner.Kinematic_Limiter is

   pragma Unsuppress (Overflow_Check);
   pragma Unsuppress (Range_Check);
   pragma Unsuppress (Division_Check);

   pragma Extensions_Allowed (On);

   procedure Run
     (Block           : aliased in out Execution_Block;
      Motor_Map       : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace       : not null access Planning_Workspace;
      Fixed_Ends      : Boolean := False;
      Forced_Junction : Corners_Index'Base := 0)
   is
      type Finishing_Corners_Index_Pair is array (1 .. 2) of Finishing_Corners_Index;

      Velocity_Change_Tolerance : constant Velocity := 1.0E-3 * mm / s;
      Cleanup_Iterations        : constant Positive := 4;

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
      Reachable_Memos      : Reachable_Velocity_Memo_Cache renames
        Workspace.Reachable_Velocity_Memos (Block.Limited_Segment_Feedrates'Range);

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
         --  Bound both possible junction coasts, including motor deltas.
         for I in Corner .. Corner + 1 loop
            declare
               Total  : constant Length := Segment_Total_Distance (Block'Access, I);
               Width  : constant Length :=
                 Length'Min
                   (Total,
                    (if I = Corner
                     then Segment_End_Transition_Distance (Block'Access, I)
                     else Segment_Start_Transition_Distance (Block'Access, I)));
               Window : constant Profile_Window :=
                 (Start_Distance => (if I = Corner then Total - Width else 0.0 * mm), Distance => Width);
            begin
               Limit := Constant_Speed_Axial_Ceiling (Block.Params, Extrusion_Bounds (Block'Access, I), Limit);
               Limit := Motor_Delta_Ceiling_For_Window (Block'Access, Motor_Map, I, Window, Limit);
            end;
         end loop;
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
         Incoming : constant Finishing_Corners_Index := Timing_Run_First (Block'Access, Corner);
         Limit    : Velocity :=
           Velocity'Min (Block.Limited_Segment_Feedrates (Corner), Block.Limited_Segment_Feedrates (Corner + 1));
      begin
         Apply_Corner_Transition_Limits (Corner, Limit);
         Limit := Extrusion_Junction_Ceiling (Block'Access, Corner, Limit);

         --  A junction speed must be admissible for a profile on both sides. Otherwise the forward pass can arrive
         --  faster than every next-window ceiling, report zero reachability, and permanently clamp the following
         --  junction to a stop even though lowering this junction slightly gives a through profile. The junction
         --  ends the incoming run and starts the outgoing one.
         for I of Finishing_Corners_Index_Pair'[Incoming, Corner + 1] loop
            declare
               Ceiling : Velocity := 0.0 * mm / s;
            begin
               for Eval of Window_Evaluations (I) loop
                  if Eval.Valid then
                     Ceiling :=
                       Velocity'Max (Ceiling, (if I = Incoming then Eval.End_Max_Vel else Eval.Start_Max_Vel));
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
            if Is_Timing_Junction (Block, I) then
               declare
                  Run_First : constant Finishing_Corners_Index := Timing_Run_First (Block'Access, I);
                  Limit     : constant Velocity := Static_Corner_Limits (I);
                  Reachable : constant Velocity :=
                    Velocity'Min
                      (Limit,
                       Reachable_Cached_Profile_Window_Velocity
                         (Run_First, Block.Corner_Velocity_Limits (Run_First - 1), Forward => True));
               begin
                  if Monotone then
                     Clamp_Corner (I, Reachable, Changed);
                  else
                     Block.Corner_Velocity_Limits (I) := Reachable;
                  end if;
               end;
            end if;
         end loop;
      end Forward_Pass;

      procedure Fill_Static_Caches is
      begin
         Static_Corner_Limits := [others => 0.0 * mm / s];

         for I in Block.Limited_Segment_Feedrates'Range loop
            if Timing_Run_First (Block'Access, I) /= I then
               --  Only a run's first segment holds its windows.
               Window_Evaluations (I) := [others => (others => <>)];
            else
               declare
                  Windows : constant Profile_Window_Candidates :=
                    Segment_Profile_Window_Candidates (Block'Access, I);
               begin
                  for W in Profile_Window_Candidate_Index loop
                     if (for all Previous in Profile_Window_Candidate_Index'First .. W - 1 =>
                           Windows (Previous) /= Windows (W))
                     then
                        Window_Evaluations (I) (W) :=
                          Evaluate_Timing_Window
                            (Block'Access, Motor_Map, I, Windows (W), Timing_Feedrate (Block'Access, I));
                     else
                        --  Absent transitions make cuts coincide. Do not repeat certification or reachability solves.
                        Window_Evaluations (I) (W) := (others => <>);
                     end if;
                  end loop;
               end;
            end if;
         end loop;

         for I in Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
            if Is_Timing_Junction (Block, I) then
               Static_Corner_Limits (I) := Static_Corner_Limit (I);
            end if;
         end loop;
      end Fill_Static_Caches;

      function Reachable_Cached_Profile_Window_Velocity
        (Finishing_Corner : Finishing_Corners_Index; Fixed_Vel : Velocity; Forward : Boolean) return Velocity
      is
         Memo  : Reachable_Velocity_Memo renames Reachable_Memos (Finishing_Corner) (Forward);
         Best  : Velocity := 0.0 * mm / s;
         Total : constant Length := Timing_Total_Distance (Block'Access, Finishing_Corner);
      begin
         --  The cached evaluations are fixed for this run, so the result depends only on Fixed_Vel.
         if Memo.Valid and then Identical (Memo.Fixed_Vel, Fixed_Vel) then
            return Memo.Reachable;
         end if;

         declare
            Cells : constant Path_Cells :=
              (if Is_Cellular_Run (Block'Access, Finishing_Corner)
               then
                 Timing_Run_Cells
                   (Block'Access, Motor_Map, Finishing_Corner, Timing_Feedrate (Block'Access, Finishing_Corner))
               else [1 .. 0 => <>]);
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
                    and then (if Forward then Eval.Start_Max_Vel else Eval.End_Max_Vel) >= Fixed_Vel
                    and then (Constant_Distance <= 0.0 * mm or else Fixed_Vel > 0.0 * mm / s)
                  then
                     Reachable :=
                       (if Eval.Cellular
                        then Reachable_Cellular_Velocity (Block.Params, Cells, Eval, Fixed_Vel, Forward)
                        else Reachable_Window_Velocity (Block.Params, Eval, Fixed_Vel));
                     Best := Velocity'Max (Best, Reachable);
                  end if;
               end;
            end loop;
         end;

         Memo := (Fixed_Vel => Fixed_Vel, Reachable => Best, Valid => True);
         return Best;
      end Reachable_Cached_Profile_Window_Velocity;

      procedure Reverse_Pass (Changed : in out Boolean) is
      begin
         for I in reverse Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
            if Is_Timing_Junction (Block, I) then
               Clamp_Corner
                 (I,
                  Reachable_Cached_Profile_Window_Velocity
                    (I + 1, Block.Corner_Velocity_Limits (Timing_Run_Last (Block'Access, I + 1)), Forward => False),
                  Changed);
            end if;
         end loop;
      end Reverse_Pass;

      Changed : Boolean := False;

   begin
      Workspace.Timing_Joins (Block.Corners'Range) := [others => False];
      if Block.Params.Continuous_Timing then
         for I in Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
            Workspace.Timing_Joins (I) :=
              I /= Forced_Junction
              and then not Workspace.Timing_Join_Vetoes (I)
              and then Joins_Timing_Run (Block'Access, I);
         end loop;
      end if;
      Assign_Timing_Runs (Block, Workspace.Timing_Joins (Block.Corners'Range));
      if not Fixed_Ends then
         Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'First) := 0.0 * mm / s;
         Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'Last) := 0.0 * mm / s;
      end if;
      Workspace.Check_Extrusion_Position :=
        (for all I in Block.Corners'Range =>
           Block.Corners (I) (E_Axis) in Block.Params.Bounds.Lower_E .. Block.Params.Bounds.Upper_E);
      Fill_Static_Caches;
      Reachable_Memos := [others => [others => (others => <>)]];

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
      --  4. Extrusion_Velocity_Change_Max, since E's velocity jumps by the density change times the corner velocity.
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
   end Run;

end Prunt.Motion_Planner.Planner.Kinematic_Limiter;
