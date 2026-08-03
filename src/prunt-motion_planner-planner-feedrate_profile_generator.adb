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

package body Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator is

   pragma Extensions_Allowed (On);

   function Profile_Window_Time
     (Start_Vel       : Velocity;
      End_Vel         : Velocity;
      Distance        : Length;
      Max_Vel         : Velocity;
      Limits          : Scalar_Derivative_Limits;
      Prefix_Distance : Length;
      Suffix_Distance : Length) return Time is
   begin
      declare
         Profile : constant Feedrate_Profile :=
           Optimal_Full_Profile
             (Start_Vel        => Start_Vel,
              Max_Vel          => Max_Vel,
              End_Vel          => End_Vel,
              Distance         => Distance,
              Acceleration_Max => Limits.Acceleration_Max,
              Jerk_Max         => Limits.Jerk_Max,
              Snap_Max         => Limits.Snap_Max,
              Crackle_Max      => Limits.Crackle_Max);
      begin
         return
           Constant_Speed_Time (Prefix_Distance, Start_Vel)
           + Total_Time (Profile)
           + Constant_Speed_Time (Suffix_Distance, End_Vel);
      end;
   exception
      when Constraint_Error =>
         return 1.0E100 * s;
   end Profile_Window_Time;

   procedure Select_Feedrate_Profile_Window
     (Block            : not null access Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace        : not null access constant Planning_Workspace;
      Finishing_Corner : Finishing_Corners_Index)
   is
      Start_Vel : constant Velocity := Block.Corner_Velocity_Limits (Finishing_Corner - 1);
      End_Vel   : constant Velocity := Block.Corner_Velocity_Limits (Finishing_Corner);
      Total     : constant Length := Segment_Total_Distance (Block, Finishing_Corner);

      Best_Found   : Boolean := False;
      Best_Profile : Feedrate_Profile :=
        (Accel => [others => 0.0 * s], Coast => 0.0 * s, Decel => [others => 0.0 * s]);
      Best_Eval    : Profile_Window_Evaluation;
      Candidate    : Feedrate_Profile;

      type Candidate_Info is record
         Valid    : Boolean := False;
         Tried    : Boolean := False;
         Eval     : Profile_Window_Evaluation;
         Duration : Time := 1.0E100 * s;
      end record;

      type Candidate_Info_Array is array (Profile_Window_Candidate_Index) of Candidate_Info;

      Windows    : constant Profile_Window_Candidates := Segment_Profile_Window_Candidates (Block, Finishing_Corner);
      Candidates : Candidate_Info_Array;
   begin
      for I in Profile_Window_Candidate_Index loop
         declare
            Eval        : constant Profile_Window_Evaluation :=
              Evaluate_Profile_Window
                (Block,
                 Workspace,
                 Motor_Map,
                 Finishing_Corner,
                 Windows (I),
                 Block.Limited_Segment_Feedrates (Finishing_Corner));
            Prefix_Dist : constant Length := Windows (I).Start_Distance;
            Suffix_Dist : constant Length := Total - Windows (I).Start_Distance - Windows (I).Distance;
         begin
            if Eval.Valid
              and then Eval.Max_Vel >= Start_Vel
              and then Eval.Max_Vel >= End_Vel
              and then (Prefix_Dist <= 0.0 * mm or else Start_Vel > 0.0 * mm / s)
              and then (Suffix_Dist <= 0.0 * mm or else End_Vel > 0.0 * mm / s)
              and then (Windows (I).Distance > 0.0 * mm or else Start_Vel = End_Vel)
              and then (Windows (I).Distance <= 0.0 * mm or else Eval.Max_Vel > 0.0 * mm / s)
              and then Endpoint_Delta_V_Distance (Start_Vel, End_Vel, Eval.Limits) <= Windows (I).Distance
            then
               Candidates (I) :=
                 (Valid    => True,
                  Tried    => False,
                  Eval     => Eval,
                  Duration =>
                    Profile_Window_Time
                      (Start_Vel, End_Vel, Windows (I).Distance, Eval.Max_Vel, Eval.Limits, Prefix_Dist, Suffix_Dist));
            end if;
         end;
      end loop;

      loop
         declare
            Candidate_Found : Boolean := False;
            Best_Index      : Profile_Window_Candidate_Index := Profile_Window_Candidate_Index'First;
         begin
            for I in Profile_Window_Candidate_Index loop
               if Candidates (I).Valid
                 and then not Candidates (I).Tried
                 and then (not Candidate_Found or else Candidates (I).Duration < Candidates (Best_Index).Duration)
               then
                  Candidate_Found := True;
                  Best_Index := I;
               end if;
            end loop;

            exit when not Candidate_Found;

            Candidates (Best_Index).Tried := True;

            begin
               Candidate :=
                 Optimal_Full_Profile
                   (Start_Vel        => Start_Vel,
                    Max_Vel          => Candidates (Best_Index).Eval.Max_Vel,
                    End_Vel          => End_Vel,
                    Distance         => Candidates (Best_Index).Eval.Window.Distance,
                    Acceleration_Max => Candidates (Best_Index).Eval.Limits.Acceleration_Max,
                    Jerk_Max         => Candidates (Best_Index).Eval.Limits.Jerk_Max,
                    Snap_Max         => Candidates (Best_Index).Eval.Limits.Snap_Max,
                    Crackle_Max      => Candidates (Best_Index).Eval.Limits.Crackle_Max);

               Best_Found := True;
               Best_Profile := Candidate;
               Best_Eval := Candidates (Best_Index).Eval;
               Block.Profile_Window_Selections (Finishing_Corner) := Stored_Profile_Window_Selection (Best_Index);
               exit;
            exception
               when Constraint_Error =>
                  null;
            end;
         end;
      end loop;

      if not Best_Found then
         raise Constraint_Error with "No valid feedrate profile window.";
      end if;

      Block.Feedrate_Profiles (Finishing_Corner) := Best_Profile;
      Block.Profile_Crackles (Finishing_Corner) := Best_Eval.Limits.Crackle_Max;
   end Select_Feedrate_Profile_Window;

   procedure Run
     (Block     : aliased in out Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace : not null access constant Planning_Workspace) is
   begin
      for I in Block.Feedrate_Profiles'Range loop
         Select_Feedrate_Profile_Window (Block'Access, Motor_Map, Workspace, I);
      end loop;
   end Run;

end Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;
