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
      Finishing_Corner : Finishing_Corners_Index;
      Result           : out Profile_Planning_Result;
      Limit_Scale      : Dimensionless)
   is
      Start_Vel : constant Velocity := Block.Corner_Velocity_Limits (Finishing_Corner - 1);
      End_Vel   : constant Velocity := Block.Corner_Velocity_Limits (Finishing_Corner);
      Total     : constant Length := Segment_Total_Distance (Block, Finishing_Corner);

      Best_Found   : Boolean := False;
      Best_Time    : Time := 1.0E100 * s;
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

      Windows    : constant Profile_Window_Candidates :=
        Segment_Profile_Window_Candidates (Block, Workspace, Finishing_Corner);
      Candidates : Candidate_Info_Array;

      procedure Consider_Split_Profile;

      procedure Consider_Split_Profile is
         Start_Curve   : constant Length := Segment_Start_Transition_Distance (Block, Finishing_Corner);
         Middle        : constant Length := Segment_Straight_Distance (Block, Finishing_Corner);
         Split_Windows : constant array (Positive range 1 .. 3) of Profile_Window :=
           [(Start_Distance => 0.0 * mm, Distance => Start_Curve),
            (Start_Distance => Start_Curve, Distance => Middle),
            (Start_Distance => Start_Curve + Middle, Distance => Total - (Start_Curve + Middle))];
         Evals         : array (Split_Windows'Range) of Profile_Window_Evaluation;
         Profiles      : array (Split_Windows'Range) of Feedrate_Profile;
         Speeds        : array (Natural range 0 .. 3) of Velocity;
         Ends          : Profile_End_Pair := (Enabled => True, others => <>);
         Split_Time    : Time := 0.0 * s;
         Maximum_Speed : Velocity := 0.0 * mm / s;

         procedure Consider_Current;
         procedure Improve_Curve_Profile (I : Positive);

         procedure Consider_Current is
         begin
            if Split_Time >= Best_Time then
               return;
            end if;
            Ends.Prefix := (Profiles (1), Evals (1).Limits.Crackle_Max, Speeds (1));
            Ends.Suffix := (Profiles (3), Evals (3).Limits.Crackle_Max, Speeds (2));
            if Extrusion_Segment_Valid
                 (Block,
                  Workspace,
                  Finishing_Corner,
                  Profiles (2),
                  Split_Windows (2),
                  Evals (2).Limits.Crackle_Max,
                  Maximum_Speed,
                  Motor_Map,
                  Ends)
            then
               Best_Found := True;
               Best_Time := Split_Time;
               Best_Profile := Profiles (2);
               Best_Eval := Evals (2);
               Block.Profile_Ends (Finishing_Corner) := Ends;
            end if;
         end Consider_Current;

         procedure Improve_Curve_Profile (I : Positive) is
            Bounds : Unit_Speed_Axial_Derivative_Bounds :=
              Window_Axial_Derivative_Bounds (Block, Workspace, Finishing_Corner, Split_Windows (I));
            Base   : Scalar_Derivative_Limits;
            Lower  : Dimensionless := 0.0;
            Upper  : Dimensionless := 1.0;
            type Ramp_Array is array (1 .. 2) of Feedrate_Profile_Times;
         begin
            if Split_Windows (I).Distance <= 0.0 * mm
              or else
                (for all Axis in Axis_Name =>
                   Bounds.Acceleration (Axis) = 0.0 / mm
                   and then Bounds.Jerk (Axis) = 0.0 / mm ** 2
                   and then Bounds.Snap (Axis) = 0.0 / mm ** 3
                   and then Bounds.Crackle (Axis) = 0.0 / mm ** 4)
            then
               return;
            end if;
            Bounds.Velocity (E_Axis) := abs Block.Extrusion_Densities (Finishing_Corner);
            Base :=
              Mixed_Derivative_Limits (Block.Params, (Velocity => Bounds.Velocity, others => <>), Evals (I).Max_Vel)
                .Limits;
            --  Scaling all configured derivative limits by one factor can starve acceleration to reserve
            --  jerk/snap budgets a short profile never reaches. Generate candidate trajectories using a
            --  time scale, then certify their exact derivative peaks against the mixed chain-rule bounds.
            for Iteration in 1 .. 12 loop
               declare
                  Scale      : constant Dimensionless := (Lower + Upper) / 2.0;
                  C          : constant Crackle := Base.Crackle_Max * Scale ** 5;
                  Candidate  : Feedrate_Profile;
                  Peaks      : Scalar_Derivative_Limits :=
                    (0.0 * mm / s ** 2, 0.0 * mm / s ** 3, 0.0 * mm / s ** 4, 0.0 * mm / s ** 5);
                  Budget     : Scalar_Derivative_Limits;
                  Check      : Mixed_Derivative_Limit_Result;
                  Peak_Speed : Velocity;
               begin
                  Candidate :=
                    Optimal_Full_Profile
                      (Speeds (I - 1),
                       Evals (I).Max_Vel,
                       Speeds (I),
                       Split_Windows (I).Distance,
                       Base.Acceleration_Max * Scale ** 2,
                       Base.Jerk_Max * Scale ** 3,
                       Base.Snap_Max * Scale ** 4,
                       C);
                  for Times of Ramp_Array'[Candidate.Accel, Candidate.Decel] loop
                     --  Exact maxima of the symmetric constant-crackle velocity transition.
                     Peaks.Acceleration_Max :=
                       Acceleration'Max
                         (Peaks.Acceleration_Max,
                          C * Times (1) * (Times (1) + Times (2)) * (2.0 * Times (1) + Times (2) + Times (3)));
                     Peaks.Jerk_Max := Jerk'Max (Peaks.Jerk_Max, C * Times (1) * (Times (1) + Times (2)));
                     Peaks.Snap_Max := Snap'Max (Peaks.Snap_Max, C * Times (1));
                     if Times (1) > 0.0 * s then
                        Peaks.Crackle_Max := C;
                     end if;
                  end loop;
                  Peak_Speed := Fast_Velocity_At_Max_Time (Candidate.Accel, C, Speeds (I - 1));
                  --  Leave room for the mixed-limit solver's numerical safety margin. Acceptance below
                  --  still requires its certified budgets to contain the uninflated trajectory peaks.
                  Budget :=
                    (Peaks.Acceleration_Max * 1.002,
                     Peaks.Jerk_Max * 1.002,
                     Peaks.Snap_Max * 1.002,
                     Peaks.Crackle_Max * 1.002);
                  Check := Mixed_Derivative_Limits (Block.Params, Bounds, Peak_Speed, Scalar_Maxes => Budget);
                  if Check.Valid
                    and then Check.Max_Vel >= Peak_Speed
                    and then Check.Limits.Acceleration_Max >= Peaks.Acceleration_Max
                    and then Check.Limits.Jerk_Max >= Peaks.Jerk_Max
                    and then Check.Limits.Snap_Max >= Peaks.Snap_Max
                    and then Check.Limits.Crackle_Max >= Peaks.Crackle_Max
                  then
                     Lower := Scale;
                     if Total_Time (Candidate) < Total_Time (Profiles (I)) then
                        Profiles (I) := Candidate;
                        Evals (I).Limits.Crackle_Max := C;
                     end if;
                  else
                     Upper := Scale;
                  end if;
               exception
                  when Constraint_Error =>
                     --  Slower limits may be unable to connect the fixed boundary velocities in this distance.
                     Lower := Scale;
               end;
            end loop;
         end Improve_Curve_Profile;
      begin
         if Start_Curve = 0.0 * mm and then Split_Windows (3).Distance = 0.0 * mm then
            return;
         end if;
         --  Each geometric portion gets its own exact full profile. Their joins have zero acceleration,
         --  jerk and snap, and a common velocity solved in both directions. Curvature bounds then constrain
         --  only the portion where they apply, instead of imposing a tiny curve's ceiling on a long straight.
         for I in Split_Windows'Range loop
            Evals (I) :=
              Evaluate_Profile_Window
                (Block,
                 Workspace,
                 Motor_Map,
                 Finishing_Corner,
                 Split_Windows (I),
                 Block.Limited_Segment_Feedrates (Finishing_Corner),
                 Allow_Extrusion_Overlap => True);
            if not Evals (I).Valid then
               return;
            end if;
            Evals (I).Max_Vel := @ * Limit_Scale;
            Evals (I).Limits.Acceleration_Max := @ * Limit_Scale;
            Evals (I).Limits.Jerk_Max := @ * Limit_Scale;
            Evals (I).Limits.Snap_Max := @ * Limit_Scale;
            Evals (I).Limits.Crackle_Max := @ * Limit_Scale;
            Maximum_Speed := Velocity'Max (Maximum_Speed, Evals (I).Max_Vel);
         end loop;
         if Start_Vel > Evals (1).Max_Vel or else End_Vel > Evals (3).Max_Vel then
            return;
         end if;
         Speeds :=
           [Start_Vel,
            Velocity'Min (Evals (1).Max_Vel, Evals (2).Max_Vel),
            Velocity'Min (Evals (2).Max_Vel, Evals (3).Max_Vel),
            End_Vel];
         for Iteration in 1 .. 3 loop
            for I in 1 .. 2 loop
               Speeds (I) :=
                 Velocity'Min
                   (Speeds (I),
                    Reachable_Velocity
                      (Speeds (I - 1), Evals (I).Max_Vel, Split_Windows (I).Distance, Evals (I).Limits));
            end loop;
            for I in reverse 2 .. 3 loop
               Speeds (I - 1) :=
                 Velocity'Min
                   (Speeds (I - 1),
                    Reachable_Velocity (Speeds (I), Evals (I).Max_Vel, Split_Windows (I).Distance, Evals (I).Limits));
            end loop;
         end loop;
         for I in Split_Windows'Range loop
            if Endpoint_Delta_V_Distance (Speeds (I - 1), Speeds (I), Evals (I).Limits) > Split_Windows (I).Distance
            then
               return;
            end if;
            Profiles (I) :=
              Optimal_Full_Profile
                (Speeds (I - 1),
                 Evals (I).Max_Vel,
                 Speeds (I),
                 Split_Windows (I).Distance,
                 Evals (I).Limits.Acceleration_Max,
                 Evals (I).Limits.Jerk_Max,
                 Evals (I).Limits.Snap_Max,
                 Evals (I).Limits.Crackle_Max);
            Split_Time := @ + Total_Time (Profiles (I));
         end loop;
         --  Keep the conservative split if the faster ramps fail the combined E certificate.
         Consider_Current;
         Split_Time := 0.0 * s;
         for I in Split_Windows'Range loop
            Improve_Curve_Profile (I);
            Split_Time := @ + Total_Time (Profiles (I));
         end loop;
         Consider_Current;
      exception
         when Constraint_Error =>
            null;
      end Consider_Split_Profile;
   begin
      Block.Profile_Ends (Finishing_Corner) := (others => <>);
      for I in Profile_Window_Candidate_Index loop
         if (for all Previous in Profile_Window_Candidate_Index'First .. I - 1 => Windows (Previous) /= Windows (I))
         then
            declare
               Eval        : Profile_Window_Evaluation :=
                 Evaluate_Profile_Window
                   (Block,
                    Workspace,
                    Motor_Map,
                    Finishing_Corner,
                    Windows (I),
                    Block.Limited_Segment_Feedrates (Finishing_Corner),
                    Allow_Extrusion_Overlap => True);
               Prefix_Dist : constant Length := Windows (I).Start_Distance;
               Suffix_Dist : constant Length := Total - Windows (I).Start_Distance - Windows (I).Distance;
            begin
               Eval.Max_Vel := Eval.Max_Vel * Limit_Scale;
               Eval.Limits.Acceleration_Max := Eval.Limits.Acceleration_Max * Limit_Scale;
               Eval.Limits.Jerk_Max := Eval.Limits.Jerk_Max * Limit_Scale;
               Eval.Limits.Snap_Max := Eval.Limits.Snap_Max * Limit_Scale;
               Eval.Limits.Crackle_Max := Eval.Limits.Crackle_Max * Limit_Scale;
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
                         (Start_Vel,
                          End_Vel,
                          Windows (I).Distance,
                          Eval.Max_Vel,
                          Eval.Limits,
                          Prefix_Dist,
                          Suffix_Dist));
               end if;
            end;
         end if;
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

               if Extrusion_Segment_Valid
                    (Block,
                     Workspace,
                     Finishing_Corner,
                     Candidate,
                     Candidates (Best_Index).Eval.Window,
                     Candidates (Best_Index).Eval.Limits.Crackle_Max,
                     Candidates (Best_Index).Eval.Max_Vel,
                     Motor_Map)
               then
                  Best_Found := True;
                  Best_Time := Candidates (Best_Index).Duration;
                  Best_Profile := Candidate;
                  Best_Eval := Candidates (Best_Index).Eval;
                  exit;
               end if;
            exception
               when Constraint_Error =>
                  null;
            end;
         end;
      end loop;

      Consider_Split_Profile;

      if not Best_Found then
         Result := (Valid => False, Failed_Segment => Finishing_Corner);
         return;
      end if;

      Result := (Valid => True);
      Block.Profile_Windows (Finishing_Corner) := Best_Eval.Window;
      Block.Feedrate_Profiles (Finishing_Corner) := Best_Profile;
      Block.Profile_Crackles (Finishing_Corner) := Best_Eval.Limits.Crackle_Max;
   end Select_Feedrate_Profile_Window;

   procedure Run
     (Block       : aliased in out Execution_Block;
      Motor_Map   : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace   : not null access constant Planning_Workspace;
      Result      : out Profile_Planning_Result;
      Limit_Scale : Dimensionless := 1.0) is
   begin
      Result := (Valid => True);
      for I in Block.Feedrate_Profiles'Range loop
         Select_Feedrate_Profile_Window (Block'Access, Motor_Map, Workspace, I, Result, Limit_Scale);
         exit when not Result.Valid;
      end loop;
   end Run;

end Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;
