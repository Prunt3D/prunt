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

package body Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator is

   pragma Unsuppress (Overflow_Check);
   pragma Unsuppress (Range_Check);
   pragma Unsuppress (Division_Check);

   pragma Extensions_Allowed (On);

   procedure Select_Feedrate_Profile_Window
     (Block            : not null access Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Finishing_Corner : Finishing_Corners_Index;
      Result           : out Profile_Planning_Result;
      Limit_Scale      : Dimensionless;
      Split_Profile    : Boolean := True)
   is
      Start_Vel : constant Velocity := Block.Corner_Velocity_Limits (Finishing_Corner - 1);
      End_Vel   : constant Velocity := Block.Corner_Velocity_Limits (Timing_Run_Last (Block, Finishing_Corner));
      Total     : constant Length := Timing_Total_Distance (Block, Finishing_Corner);
      Feedrate  : constant Velocity := Timing_Feedrate (Block, Finishing_Corner);

      Best_Found         : Boolean := False;
      Best_Time          : Time := 1.0E100 * s;
      Best_Profile       : Feedrate_Profile :=
        (Accel => [others => 0.0 * s], Coast => 0.0 * s, Decel => [others => 0.0 * s]);
      Best_Eval          : Profile_Window_Evaluation;
      Best_Decel_Crackle : Crackle := 0.0 * mm / s**5;

      function Scaled_Cells return Path_Cells;
      --  The run's cells with their ceilings reduced by Limit_Scale, like every window's Max_Vel.

      function Scaled_Cells return Path_Cells is
      begin
         if not Is_Cellular_Run (Block, Finishing_Corner) then
            return [1 .. 0 => <>];
         end if;
         return Result : Path_Cells := Timing_Run_Cells (Block, Motor_Map, Finishing_Corner, Feedrate) do
            for Cell of Result loop
               Cell.Ceiling := Cell.Ceiling * Limit_Scale;
            end loop;
         end return;
      end Scaled_Cells;

      Cells : constant Path_Cells := Scaled_Cells;

      type Candidate_Info is record
         Valid    : Boolean := False;
         Eval     : Profile_Window_Evaluation;
         Duration : Time := 1.0E100 * s;
         Solution : Window_Profile_Result;
      end record;

      type Candidate_Info_Array is array (Profile_Window_Candidate_Index) of Candidate_Info;

      Windows    : constant Profile_Window_Candidates := Segment_Profile_Window_Candidates (Block, Finishing_Corner);
      Candidates : Candidate_Info_Array;

      function Conservative_Evaluation (Eval : Profile_Window_Evaluation) return Profile_Window_Evaluation;
      function Conservative_Profile
        (Eval : Profile_Window_Evaluation; First, Last : Velocity) return Window_Profile_Result;
      function Generate (Eval : Profile_Window_Evaluation; First, Last : Velocity) return Window_Profile_Result;
      function Reachable (Eval : Profile_Window_Evaluation; Fixed_Vel : Velocity; Forward : Boolean) return Velocity;
      --  The window or cellular certificate, whichever Eval needs.
      procedure Consider_Split_Profile (Conservative : Boolean; Accepted : out Boolean);
      procedure Try_Conservative_Candidate (I : Profile_Window_Candidate_Index);

      function Conservative_Evaluation (Eval : Profile_Window_Evaluation) return Profile_Window_Evaluation is
         Mixed  : constant Mixed_Derivative_Limit_Result :=
           Mixed_Derivative_Limits (Block.Params, Eval.Bounds, Feedrate);
         Result : Profile_Window_Evaluation := Eval;
      begin
         --  Eval.Limits contains tangent-only budgets. Reconstruct the full mixed-limit certificate;
         --  merely passing those budgets to Optimal_Full_Profile would not certify a curved window.
         Result.Valid := Eval.Valid and then Mixed.Valid;
         Result.Max_Vel := Velocity'Min (Eval.Max_Vel, Mixed.Max_Vel * Limit_Scale);
         if Eval.Cellular then
            --  A cellular Max_Vel omits the motor-delta and E ceilings, which only its cells hold.
            for Cell of Cells loop
               if Cell_Overlaps (Cell, Eval.Window.Start_Distance, Eval.Window.Start_Distance + Eval.Window.Distance)
               then
                  Result.Max_Vel := Velocity'Min (Result.Max_Vel, Cell.Ceiling);
               end if;
            end loop;
         end if;
         --  The conservative profile caps its whole window, including both ends, at Max_Vel.
         Result.Start_Max_Vel := Result.Max_Vel;
         Result.End_Max_Vel := Result.Max_Vel;
         Result.Limits :=
           (Mixed.Limits.Acceleration_Max * Limit_Scale,
            Mixed.Limits.Jerk_Max * Limit_Scale,
            Mixed.Limits.Snap_Max * Limit_Scale,
            Mixed.Limits.Crackle_Max * Limit_Scale);
         return Result;
      end Conservative_Evaluation;

      function Conservative_Profile
        (Eval : Profile_Window_Evaluation; First, Last : Velocity) return Window_Profile_Result
      is
         Result : Window_Profile_Result;
      begin
         if not Eval.Valid or else Eval.Max_Vel < Velocity'Max (First, Last) then
            return Result;
         end if;
         Result.Profile :=
           Optimal_Full_Profile
             (First,
              Eval.Max_Vel,
              Last,
              Eval.Window.Distance,
              Eval.Limits.Acceleration_Max,
              Eval.Limits.Jerk_Max,
              Eval.Limits.Snap_Max,
              Eval.Limits.Crackle_Max);
         Result.Max_Crackle := Eval.Limits.Crackle_Max;
         Result.Decel_Crackle := Eval.Limits.Crackle_Max;
         Result.Valid := True;
         return Result;
      exception
         when Constraint_Error =>
            return (others => <>);
      end Conservative_Profile;

      function Generate (Eval : Profile_Window_Evaluation; First, Last : Velocity) return Window_Profile_Result
      is (if Eval.Cellular
          then Generate_Cellular_Profile (Block.Params, Cells, Eval, First, Last)
          else Generate_Window_Profile (Block.Params, Eval, First, Last));

      function Reachable (Eval : Profile_Window_Evaluation; Fixed_Vel : Velocity; Forward : Boolean) return Velocity
      is (if Eval.Cellular
          then Reachable_Cellular_Velocity (Block.Params, Cells, Eval, Fixed_Vel, Forward)
          else Reachable_Window_Velocity (Block.Params, Eval, Fixed_Vel));

      procedure Consider_Split_Profile (Conservative : Boolean; Accepted : out Boolean) is
         Split_Windows : constant Split_Profile_Windows := Timing_Split_Windows (Block, Finishing_Corner);
         Evals         : array (Split_Windows'Range) of Profile_Window_Evaluation;
         Profiles      : array (Split_Windows'Range) of Feedrate_Profile;
         Decels        : array (Split_Windows'Range) of Crackle;
         Speeds        : array (Natural range 0 .. 3) of Velocity;
         Split_Time    : Time := 0.0 * s;
      begin
         Accepted := False;
         if Split_Windows (1).Distance = 0.0 * mm and then Split_Windows (3).Distance = 0.0 * mm then
            Accepted := True;
            return;
         end if;
         --  Each geometric portion gets its own exact full profile. Their joins have zero acceleration,
         --  jerk and snap, and a common velocity solved in both directions. Curvature bounds then constrain
         --  only the portion where they apply, instead of imposing a tiny curve's ceiling on a long straight.
         for I in Split_Windows'Range loop
            Evals (I) := Evaluate_Timing_Window (Block, Motor_Map, Finishing_Corner, Split_Windows (I), Feedrate);
            if not Evals (I).Valid then
               return;
            end if;
            Evals (I).Max_Vel := @ * Limit_Scale;
            Evals (I).Start_Max_Vel := @ * Limit_Scale;
            Evals (I).End_Max_Vel := @ * Limit_Scale;
            Evals (I).Limits.Acceleration_Max := @ * Limit_Scale;
            Evals (I).Limits.Jerk_Max := @ * Limit_Scale;
            Evals (I).Limits.Snap_Max := @ * Limit_Scale;
            Evals (I).Limits.Crackle_Max := @ * Limit_Scale;
            if Conservative then
               Evals (I) := Conservative_Evaluation (Evals (I));
               if not Evals (I).Valid then
                  return;
               end if;
            end if;
         end loop;
         if Start_Vel > Evals (1).Start_Max_Vel or else End_Vel > Evals (3).End_Max_Vel then
            return;
         end if;
         Speeds :=
           [Start_Vel,
            Velocity'Min (Evals (1).End_Max_Vel, Evals (2).Start_Max_Vel),
            Velocity'Min (Evals (2).End_Max_Vel, Evals (3).Start_Max_Vel),
            End_Vel];
         for Iteration in 1 .. 3 loop
            declare
               Previous : constant array (1 .. 2) of Velocity := [Speeds (1), Speeds (2)];
            begin
               for I in 1 .. 2 loop
                  Speeds (I) :=
                    Velocity'Min
                      (Speeds (I),
                       (if Conservative
                        then
                          Reachable_Velocity
                            (Speeds (I - 1), Evals (I).Max_Vel, Split_Windows (I).Distance, Evals (I).Limits)
                        else Reachable (Evals (I), Speeds (I - 1), Forward => True)));
               end loop;
               for I in reverse 2 .. 3 loop
                  Speeds (I - 1) :=
                    Velocity'Min
                      (Speeds (I - 1),
                       (if Conservative
                        then
                          Reachable_Velocity
                            (Speeds (I), Evals (I).Max_Vel, Split_Windows (I).Distance, Evals (I).Limits)
                        else Reachable (Evals (I), Speeds (I), Forward => False)));
               end loop;
               --  A sweep depends only on Speeds, so once one leaves them unchanged every later sweep would too.
               exit when Identical (Speeds (1), Previous (1)) and then Identical (Speeds (2), Previous (2));
            end;
         end loop;
         for I in Split_Windows'Range loop
            declare
               Solution : constant Window_Profile_Result :=
                 (if Conservative
                  then Conservative_Profile (Evals (I), Speeds (I - 1), Speeds (I))
                  else Generate (Evals (I), Speeds (I - 1), Speeds (I)));
            begin
               if not Solution.Valid then
                  return;
               end if;
               Profiles (I) := Solution.Profile;
               Evals (I).Limits.Crackle_Max := Solution.Max_Crackle;
               Decels (I) := Solution.Decel_Crackle;
               Split_Time := @ + Total_Time (Profiles (I));
               if Conservative and then Split_Time >= Best_Time then
                  --  Valid profile times are nonnegative, so this split can no longer be selected. Only the
                  --  conservative attempt can stop here: a later failure of the first attempt enables the retry.
                  Accepted := True;
                  return;
               end if;
            end;
         end loop;
         Accepted := True;
         if Split_Time < Best_Time then
            Best_Found := True;
            Best_Time := Split_Time;
            Best_Profile := Profiles (2);
            Best_Eval := Evals (2);
            Best_Decel_Crackle := Decels (2);
            Block.Profile_Ends (Finishing_Corner) :=
              (Enabled => True,
               Prefix  => (Profiles (1), Evals (1).Limits.Crackle_Max, Decels (1), Speeds (1)),
               Suffix  => (Profiles (3), Evals (3).Limits.Crackle_Max, Decels (3), Speeds (2)));
         end if;
      exception
         when Constraint_Error =>
            null;
      end Consider_Split_Profile;

      procedure Try_Conservative_Candidate (I : Profile_Window_Candidate_Index) is
         C : Candidate_Info renames Candidates (I);
      begin
         if not C.Eval.Curved then
            return;
         end if;
         C.Valid := False;
         C.Eval := Conservative_Evaluation (C.Eval);
         C.Solution := Conservative_Profile (C.Eval, Start_Vel, End_Vel);
         if C.Solution.Valid then
            C.Valid := True;
            C.Duration :=
              Constant_Speed_Time (C.Eval.Window.Start_Distance, Start_Vel)
              + Total_Time (C.Solution.Profile)
              + Constant_Speed_Time (Total - C.Eval.Window.Start_Distance - C.Eval.Window.Distance, End_Vel);
         end if;
      end Try_Conservative_Candidate;
   begin
      Block.Profile_Ends (Finishing_Corner) := (others => <>);
      for I in Profile_Window_Candidate_Index loop
         if (for all Previous in Profile_Window_Candidate_Index'First .. I - 1 => Windows (Previous) /= Windows (I))
         then
            declare
               Eval        : Profile_Window_Evaluation :=
                 Evaluate_Timing_Window (Block, Motor_Map, Finishing_Corner, Windows (I), Feedrate);
               Prefix_Dist : constant Length := Windows (I).Start_Distance;
               Suffix_Dist : constant Length := Total - Windows (I).Start_Distance - Windows (I).Distance;
            begin
               Eval.Max_Vel := Eval.Max_Vel * Limit_Scale;
               Eval.Start_Max_Vel := Eval.Start_Max_Vel * Limit_Scale;
               Eval.End_Max_Vel := Eval.End_Max_Vel * Limit_Scale;
               Eval.Limits.Acceleration_Max := Eval.Limits.Acceleration_Max * Limit_Scale;
               Eval.Limits.Jerk_Max := Eval.Limits.Jerk_Max * Limit_Scale;
               Eval.Limits.Snap_Max := Eval.Limits.Snap_Max * Limit_Scale;
               Eval.Limits.Crackle_Max := Eval.Limits.Crackle_Max * Limit_Scale;
               if Eval.Valid
                 and then Eval.Start_Max_Vel >= Start_Vel
                 and then Eval.End_Max_Vel >= End_Vel
                 and then (Prefix_Dist <= 0.0 * mm or else Start_Vel > 0.0 * mm / s)
                 and then (Suffix_Dist <= 0.0 * mm or else End_Vel > 0.0 * mm / s)
                 and then (Windows (I).Distance > 0.0 * mm or else Start_Vel = End_Vel)
                 and then (Windows (I).Distance <= 0.0 * mm or else Eval.Max_Vel > 0.0 * mm / s)
               then
                  declare
                     Solution : constant Window_Profile_Result := Generate (Eval, Start_Vel, End_Vel);
                  begin
                     Candidates (I) :=
                       (Valid    => Solution.Valid,
                        Eval     => Eval,
                        Solution => Solution,
                        Duration =>
                          Constant_Speed_Time (Prefix_Dist, Start_Vel)
                          + Total_Time (Solution.Profile)
                          + Constant_Speed_Time (Suffix_Dist, End_Vel));
                     if not Solution.Valid then
                        Try_Conservative_Candidate (I);
                     end if;
                  end;
               end if;
            end;
         end if;
      end loop;

      for C of Candidates loop
         if C.Valid and then C.Duration < Best_Time then
            Best_Found := True;
            Best_Time := C.Duration;
            Best_Profile := C.Solution.Profile;
            Best_Eval := C.Eval;
            Best_Eval.Limits.Crackle_Max := C.Solution.Max_Crackle;
            Best_Decel_Crackle := C.Solution.Decel_Crackle;
         end if;
      end loop;

      if Split_Profile then
         declare
            Accepted : Boolean;
         begin
            Consider_Split_Profile (Conservative => False, Accepted => Accepted);
            if not Accepted then
               --  The gentler split needs its own internal join velocities as well as its own ramps.
               Consider_Split_Profile (Conservative => True, Accepted => Accepted);
            end if;
         end;
      end if;

      if not Best_Found then
         Result := (Valid => False, Failed_Segment => Finishing_Corner);
         return;
      end if;

      Result := (Valid => True);
      Block.Profile_Windows (Finishing_Corner) := Best_Eval.Window;
      Block.Feedrate_Profiles (Finishing_Corner) := Best_Profile;
      Block.Profile_Crackles (Finishing_Corner) := Best_Eval.Limits.Crackle_Max;
      Block.Profile_Decel_Crackles (Finishing_Corner) := Best_Decel_Crackle;
   end Select_Feedrate_Profile_Window;

   procedure Run
     (Block       : aliased in out Execution_Block;
      Motor_Map   : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Result      : out Profile_Planning_Result;
      Limit_Scale : Dimensionless := 1.0) is
   begin
      Result := (Valid => True);
      for I in Block.Feedrate_Profiles'Range loop
         if Timing_Run_First (Block'Access, I) = I then
            Select_Feedrate_Profile_Window (Block'Access, Motor_Map, I, Result, Limit_Scale);
            exit when not Result.Valid;
            if Timing_Run_Last (Block'Access, I) /= I then
               Assign_Timing_Members (Block'Access, I);
            end if;
         end if;
      end loop;
   end Run;

end Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;
