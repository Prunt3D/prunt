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

with Trendy_Test; use Trendy_Test;

package body Prunt.Motion_Planner.Test is

   pragma Extensions_Allowed (On);

   procedure Check_Profile_For_Distance
     (Start_Vel   : Velocity;
      Distance    : Length;
      Max_Accel   : Acceleration;
      Max_Jerk    : Jerk;
      Max_Snap    : Snap;
      Max_Crackle : Crackle;
      Region      : Constraint_Region;
      Index       : Integer;
      Name        : String;
      T           : in out Trendy_Test.Operation'Class)
   is
      Result : constant Internal_Profile_Result :=
        Optimal_Profile_For_Distance_Internal (Start_Vel, Distance, Max_Accel, Max_Jerk, Max_Snap, Max_Crackle);
   begin
      T.Assert (Result.Region = Region, Name & ": Incorrect region");
      T.Assert (Result.Index = Index, Name & ": Incorrect index");

      for I in 0 .. N_Kinematic_Check_Steps loop
         T_Val : constant Time :=
           Dimensionless (I) / Dimensionless (N_Kinematic_Check_Steps) * Total_Time (Result.Profile);
         Step_Velocity : constant Velocity := Velocity_At_Time (Result.Profile, T_Val, Max_Crackle, Start_Vel);
         Step_Accel : constant Acceleration := Acceleration_At_Time (Result.Profile, T_Val, Max_Crackle);
         Step_Jerk : constant Jerk := Jerk_At_Time (Result.Profile, T_Val, Max_Crackle);
         Step_Snap : constant Snap := Snap_At_Time (Result.Profile, T_Val, Max_Crackle);
         Step_Crackle : constant Crackle := Crackle_At_Time (Result.Profile, T_Val, Max_Crackle);

         T.Assert
           (abs Step_Velocity > -Tolerance_Epsilon * mm / s,
            Name & ": Negative velocity at " & T_Val'Image & " (" & Step_Velocity'Image & ")");
         T.Assert
           (abs Step_Accel <= Max_Accel + Max_Accel * Tolerance_Epsilon,
            Name & ": Max acceleration exceeded at " & T_Val'Image & " (" & Step_Accel'Image & ")");
         T.Assert
           (abs Step_Jerk <= Max_Jerk + Max_Jerk * Tolerance_Epsilon,
            Name & ": Max jerk exceeded at " & T_Val'Image & " (" & Step_Jerk'Image & ")");
         T.Assert
           (abs Step_Snap <= Max_Snap + Max_Snap * Tolerance_Epsilon,
            Name & ": Max snap exceeded at " & T_Val'Image & " (" & Step_Snap'Image & ")");
         T.Assert
           (abs Step_Crackle <= Max_Crackle + Max_Crackle * Tolerance_Epsilon,
            Name & ": Max crackle exceeded at " & T_Val'Image & " (" & Step_Crackle'Image & ")");
      end loop;

      End_Distance : constant Length :=
        Distance_At_Time (Result.Profile, Total_Time (Result.Profile), Max_Crackle, Start_Vel);

      T.Assert
        (abs (End_Distance - Distance) <= abs Distance * Tolerance_Epsilon,
         Name & ": Incorrect end distance " & End_Distance'Image & " vs " & Distance'Image);
   end Check_Profile_For_Distance;

   procedure Check_Profile_For_Delta_V
     (Delta_V     : Velocity;
      Max_Accel   : Acceleration;
      Max_Jerk    : Jerk;
      Max_Snap    : Snap;
      Max_Crackle : Crackle;
      Region      : Constraint_Region;
      Index       : Integer;
      Name        : String;
      T           : in out Trendy_Test.Operation'Class)
   is
      Result : constant Internal_Profile_Result :=
        Optimal_Profile_For_Delta_V_Internal (Delta_V, Max_Accel, Max_Jerk, Max_Snap, Max_Crackle);
   begin
      T.Assert (Result.Region = Region, Name & ": Incorrect region");
      T.Assert (Result.Index = Index, Name & ": Incorrect index");

      for I in 0 .. N_Kinematic_Check_Steps loop
         T_Val : constant Time :=
           Dimensionless (I) / Dimensionless (N_Kinematic_Check_Steps) * Total_Time (Result.Profile);
         Step_Velocity : constant Velocity := Velocity_At_Time (Result.Profile, T_Val, Max_Crackle, 0.0 * mm / s);
         Step_Accel : constant Acceleration := Acceleration_At_Time (Result.Profile, T_Val, Max_Crackle);
         Step_Jerk : constant Jerk := Jerk_At_Time (Result.Profile, T_Val, Max_Crackle);
         Step_Snap : constant Snap := Snap_At_Time (Result.Profile, T_Val, Max_Crackle);
         Step_Crackle : constant Crackle := Crackle_At_Time (Result.Profile, T_Val, Max_Crackle);

         T.Assert
           (abs Step_Velocity <= Delta_V + Delta_V * Tolerance_Epsilon,
            Name & ": Max velocity exceeded at " & T_Val'Image & " (" & Step_Velocity'Image & ")");
         T.Assert
           (abs Step_Velocity > -Tolerance_Epsilon * mm / s,
            Name & ": Negative velocity at " & T_Val'Image & " (" & Step_Velocity'Image & ")");
         T.Assert
           (abs Step_Accel <= Max_Accel + Max_Accel * Tolerance_Epsilon,
            Name & ": Max acceleration exceeded at " & T_Val'Image & " (" & Step_Accel'Image & ")");
         T.Assert
           (abs Step_Jerk <= Max_Jerk + Max_Jerk * Tolerance_Epsilon,
            Name & ": Max jerk exceeded at " & T_Val'Image & " (" & Step_Jerk'Image & ")");
         T.Assert
           (abs Step_Snap <= Max_Snap + Max_Snap * Tolerance_Epsilon,
            Name & ": Max snap exceeded at " & T_Val'Image & " (" & Step_Snap'Image & ")");
         T.Assert
           (abs Step_Crackle <= Max_Crackle + Max_Crackle * Tolerance_Epsilon,
            Name & ": Max crackle exceeded at " & T_Val'Image & " (" & Step_Crackle'Image & ")");
      end loop;

      End_Velocity : constant Velocity :=
        Velocity_At_Time (Result.Profile, Total_Time (Result.Profile), Max_Crackle, 0.0 * mm / s);

      T.Assert
        (abs (End_Velocity - Delta_V) <= abs Delta_V * Tolerance_Epsilon,
         Name & ": Incorrect end velocity delta " & End_Velocity'Image & " vs " & Delta_V'Image);
   end Check_Profile_For_Delta_V;

   procedure Test_Distance_At_Time_Is_Past_Accel (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      Profile : constant Feedrate_Profile :=
        (Accel => [Sm / Cm, Jm / Sm - Sm / Cm, Am / Jm - Jm / Sm - Sm / Cm, 0.0 * s],
         Coast => 1.0 * s,
         Decel => [Sm / Cm, Jm / Sm - Sm / Cm, Am / Jm - Jm / Sm - Sm / Cm, 0.0 * s]);

      Is_Past_Accel : Boolean;
      Dist : Length;
      Dist_Check : Length;

      Dist := Distance_At_Time (Profile, 0.5 * Total_Time (Profile.Accel), Cm, Vs, Is_Past_Accel);
      T.Assert (not Is_Past_Accel, "Is_Past_Accel should be False in Accel phase");
      Dist_Check := Distance_At_Time (Profile, 0.5 * Total_Time (Profile.Accel), Cm, Vs);
      T.Assert (Dist = Dist_Check, "Distance should match for Accel phase");

      Dist := Distance_At_Time (Profile, Total_Time (Profile.Accel) + 0.5 * Profile.Coast, Cm, Vs, Is_Past_Accel);
      T.Assert (Is_Past_Accel, "Is_Past_Accel should be True in Coast phase");
      Dist_Check := Distance_At_Time (Profile, Total_Time (Profile.Accel) + 0.5 * Profile.Coast, Cm, Vs);
      T.Assert (Dist = Dist_Check, "Distance should match for Coast phase");

      Dist :=
        Distance_At_Time
          (Profile,
           Total_Time (Profile.Accel) + Profile.Coast + 0.5 * Total_Time (Profile.Decel),
           Cm,
           Vs,
           Is_Past_Accel);
      T.Assert (Is_Past_Accel, "Is_Past_Accel should be True in Decel phase");
      Dist_Check :=
        Distance_At_Time
          (Profile, Total_Time (Profile.Accel) + Profile.Coast + 0.5 * Total_Time (Profile.Decel), Cm, Vs);
      T.Assert (Dist = Dist_Check, "Distance should match for Decel phase");
   end Test_Distance_At_Time_Is_Past_Accel;

   procedure Test_Distance_At_Time_Phases (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      Profile : constant Feedrate_Profile :=
        (Accel => [Sm / Cm, Jm / Sm - Sm / Cm, Am / Jm - Jm / Sm - Sm / Cm, 0.0 * s],
         Coast => 1.0 * s,
         Decel => [Sm / Cm, Jm / Sm - Sm / Cm, Am / Jm - Jm / Sm - Sm / Cm, 0.0 * s]);

      Dist_Accel : Length;
      Dist_Coast : Length;

      Dist_Accel := Distance_At_Time (Profile, 0.5 * Total_Time (Profile.Accel), Cm, Vs);
      T.Assert (Dist_Accel > 0.0 * mm, "Distance in Accel should be positive");

      Dist_Coast := Distance_At_Time (Profile, Total_Time (Profile.Accel) + 0.5 * Profile.Coast, Cm, Vs);
      T.Assert
        (Dist_Coast > Distance_At_Time (Profile.Accel, Total_Time (Profile.Accel), Cm, Vs),
         "Distance in Coast should be > Accel distance");
   end Test_Distance_At_Time_Phases;

   procedure Test_Fast_Vs_Slow (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Start_Vel : constant Velocity := 10.0 * mm / s;
      Dist : constant Length := 100.0 * mm;
      Profile : constant Feedrate_Profile_Times := Optimal_Profile_For_Distance (Start_Vel, Dist, Am, Jm, Sm, Cm);

      Total_T : constant Time := Total_Time (Profile);

      Fast_Dist : constant Length := Fast_Distance_At_Max_Time (Profile, Cm, Start_Vel);
      Slow_Dist : constant Length := Distance_At_Time (Profile, Total_T, Cm, Start_Vel);

      Fast_Vel : constant Velocity := Fast_Velocity_At_Max_Time (Profile, Cm, Start_Vel);
      Slow_Vel : constant Velocity := Velocity_At_Time (Profile, Total_T, Cm, Start_Vel);


      T.Assert
        (abs (Fast_Dist - Slow_Dist) < 0.001 * mm,
         "Fast vs Slow Distance Mismatch: " & Fast_Dist'Image & " vs " & Slow_Dist'Image);
      T.Assert
        (abs (Fast_Vel - Slow_Vel) < 0.001 * mm / s,
         "Fast vs Slow Velocity Mismatch: " & Fast_Vel'Image & " vs " & Slow_Vel'Image);
   end Test_Fast_Vs_Slow;

   procedure Test_Integration_Acceleration_To_Velocity (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Profile : constant Feedrate_Profile_Times := [4.0 * s, 5.0 * s, 6.0 * s, 7.0 * s];
      Start_Vel : constant Velocity := 0.0 * mm / s;
      Dt : constant Time := 0.00001 * s;
      Num_Steps : constant Integer := Integer (Float (Total_Time (Profile) / Dt));
      Current_Vel : Velocity := Velocity_At_Time (Profile, 0.0 * s, Cm, Start_Vel);
      Step_Time : Time;
      Step_Accel : Acceleration;
      Expected_Vel : Velocity;

      for I in 1 .. Num_Steps loop
         Step_Time := Dimensionless (I) * Dt;

         Step_Accel := Acceleration_At_Time (Profile, Step_Time, Cm);
         Expected_Vel := Velocity_At_Time (Profile, Step_Time, Cm, Start_Vel);
         Current_Vel := Current_Vel + Step_Accel * Dt;

         T.Assert
           (abs (Current_Vel - Expected_Vel) <= 1.0E-4 * abs Expected_Vel + 1.0E-5 * mm / s,
            "Velocity integration failed at "
            & Step_Time'Image
            & ": "
            & Current_Vel'Image
            & " vs "
            & Expected_Vel'Image);
      end loop;
   end Test_Integration_Acceleration_To_Velocity;

   procedure Test_Integration_Crackle_To_Snap (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Profile : constant Feedrate_Profile_Times := [4.0 * s, 5.0 * s, 6.0 * s, 7.0 * s];
      Dt : constant Time := 0.00001 * s;
      Num_Steps : constant Integer := Integer (Float (Total_Time (Profile) / Dt));
      Current_Snap : Snap := Snap_At_Time (Profile, 0.0 * s, Cm);
      Step_Time : Time;
      Step_Crackle : Crackle;
      Expected_Snap : Snap;

      for I in 1 .. Num_Steps loop
         Step_Time := Dimensionless (I) * Dt;

         Step_Crackle := Crackle_At_Time (Profile, Step_Time, Cm);
         Expected_Snap := Snap_At_Time (Profile, Step_Time, Cm);
         Current_Snap := Current_Snap + Step_Crackle * Dt;

         T.Assert
           (abs (Current_Snap - Expected_Snap) <= Snap'Max (1.0E-7 * abs Expected_Snap, 1.0 * mm / s ** 4),
            "Snap integration failed at "
            & Step_Time'Image
            & ": "
            & Current_Snap'Image
            & " vs "
            & Expected_Snap'Image);
      end loop;
   end Test_Integration_Crackle_To_Snap;

   procedure Test_Integration_Jerk_To_Acceleration (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Profile : constant Feedrate_Profile_Times := [4.0 * s, 5.0 * s, 6.0 * s, 7.0 * s];
      Dt : constant Time := 0.00001 * s;
      Total_T : constant Time := Total_Time (Profile);
      Num_Steps : constant Integer := Integer (Float (Total_T / Dt));
      Current_Accel : Acceleration := Acceleration_At_Time (Profile, 0.0 * s, Cm);
      Time_T : Time;
      Step_Jerk : Jerk;
      Expected_Accel : Acceleration;

      for I in 1 .. Num_Steps loop
         Time_T := Dimensionless (I) * Dt;

         Step_Jerk := Jerk_At_Time (Profile, Time_T, Cm);
         Expected_Accel := Acceleration_At_Time (Profile, Time_T, Cm);
         Current_Accel := Current_Accel + Step_Jerk * Dt;

         T.Assert
           (abs (Current_Accel - Expected_Accel) <= 1.0E-5 * abs Expected_Accel + 1.0E-2 * mm / s ** 2,
            "Acceleration integration failed at "
            & Time_T'Image
            & ": "
            & Current_Accel'Image
            & " vs "
            & Expected_Accel'Image);
      end loop;
   end Test_Integration_Jerk_To_Acceleration;

   procedure Test_Integration_Snap_To_Jerk (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Profile : constant Feedrate_Profile_Times := [4.0 * s, 5.0 * s, 6.0 * s, 7.0 * s];
      Dt : constant Time := 0.00001 * s;
      Num_Steps : constant Integer := Integer (Float (Total_Time (Profile) / Dt)) - 1;
      Current_Jerk : Jerk := Jerk_At_Time (Profile, 0.0 * s, Cm);
      Step_Time : Time;
      Step_Snap : Snap;
      Expected_Jerk : Jerk;

      for I in 1 .. Num_Steps loop
         Step_Time := Dimensionless (I) * Dt;

         Step_Snap := Snap_At_Time (Profile, Step_Time, Cm);
         Expected_Jerk := Jerk_At_Time (Profile, Step_Time, Cm);
         Current_Jerk := Current_Jerk + Step_Snap * Dt;

         T.Assert
           (abs (Current_Jerk - Expected_Jerk) <= Jerk'Max (1.0E-10 * abs Expected_Jerk, 1.0 * mm / s ** 3),
            "Jerk integration failed at "
            & Step_Time'Image
            & ": "
            & Current_Jerk'Image
            & " vs "
            & Expected_Jerk'Image);
      end loop;
   end Test_Integration_Snap_To_Jerk;

   procedure Test_Integration_Velocity_To_Distance (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Profile : constant Feedrate_Profile_Times := [4.0 * s, 5.0 * s, 6.0 * s, 7.0 * s];
      Start_Vel : constant Velocity := 0.0 * mm / s;
      Dt : constant Time := 0.00001 * s;
      Num_Steps : constant Integer := Integer (Float (Total_Time (Profile) / Dt));
      Current_Dist : Length := Distance_At_Time (Profile, 0.0 * s, Cm, Start_Vel);
      Step_Time : Time;
      Step_Vel : Velocity;
      Expected_Dist : Length;

      for I in 1 .. Num_Steps loop
         Step_Time := Dimensionless (I) * Dt;

         Step_Vel := Velocity_At_Time (Profile, Step_Time, Cm, Start_Vel);
         Expected_Dist := Distance_At_Time (Profile, Step_Time, Cm, Start_Vel);
         Current_Dist := Current_Dist + Step_Vel * Dt;

         T.Assert
           (abs (Current_Dist - Expected_Dist) <= 1.0E-3 * abs Expected_Dist + 1.0E-5 * mm,
            "Distance integration failed at "
            & Step_Time'Image
            & ": "
            & Current_Dist'Image
            & " vs "
            & Expected_Dist'Image);
      end loop;
   end Test_Integration_Velocity_To_Distance;

   procedure Test_Optimal_Full_Profile_Constraint_Errors (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vm : constant Velocity := 100.0 * mm / s;
      Vs : constant Velocity := 10.0 * mm / s;
      Ve : constant Velocity := 20.0 * mm / s;
      Dist : constant Length := 0.001 * mm; --  Too short to decel

      Profile : constant Feedrate_Profile := Optimal_Full_Profile (Vs, Vm, Ve, Dist, Am, Jm, Sm, Cm)
      with Unreferenced;

      T.Fail ("Should have raised Constraint_Error for too short distance");
   exception
      when Constraint_Error =>
         null;
   end Test_Optimal_Full_Profile_Constraint_Errors;

   procedure Test_Optimal_Full_Profile_Errors (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;

      Profile : Feedrate_Profile;

      begin
         Profile := Optimal_Full_Profile (0.0 * mm / s, 10.0 * mm / s, 20.0 * mm / s, 100.0 * mm, Am, Jm, Sm, Cm);
         T.Fail ("Should have raised Constraint_Error (Max_Vel < End_Vel)");
      exception
         when Constraint_Error =>
            null;
      end;

      Profile := Optimal_Full_Profile (0.0 * mm / s, 10.0 * mm / s, 0.0 * mm / s, 0.0 * mm, Am, Jm, Sm, Cm);
      T.Assert (Total_Time (Profile) = 0.0 * s, "Zero distance should result in zero time profile");
   end Test_Optimal_Full_Profile_Errors;

   procedure Test_Optimal_Full_Profile_Impossible (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vm : constant Velocity := 10.0 * mm / s;
      Vs : constant Velocity := 100.0 * mm / s; --  > Vm
      Ve : constant Velocity := 0.0 * mm / s;
      Dist : constant Length := 100.0 * mm;

      Profile : constant Feedrate_Profile := Optimal_Full_Profile (Vs, Vm, Ve, Dist, Am, Jm, Sm, Cm)
      with Unreferenced;

      T.Fail ("Should have raised Constraint_Error");
   exception
      when Constraint_Error =>
         null;
   end Test_Optimal_Full_Profile_Impossible;

   procedure Check_Full_Profile
     (Start_Vel : Velocity;
      Max_Vel   : Velocity;
      End_Vel   : Velocity;
      Distance  : Length;
      Am        : Acceleration;
      Jm        : Jerk;
      Sm        : Snap;
      Cm        : Crackle;
      Name      : String;
      T         : in out Trendy_Test.Operation'Class)
   is
      Profile : constant Feedrate_Profile :=
        Optimal_Full_Profile (Start_Vel, Max_Vel, End_Vel, Distance, Am, Jm, Sm, Cm);

      Total_T : constant Time := Total_Time (Profile);
      Steps   : constant Integer := 2000;
      Dt      : constant Time := Total_T / Dimensionless (Steps);
      Time_T  : Time;
      Acc     : Acceleration;
      Jrk     : Jerk;
      Snp     : Snap;
      Crk     : Crackle;
      V       : Velocity;
      D       : Length;
      Epsilon : constant := 0.0001;
   begin
      for I in 0 .. Steps loop
         Time_T := Dimensionless (I) * Dt;
         if Time_T > Total_T then
            Time_T := Total_T;
         end if;

         Acc := Acceleration_At_Time (Profile, Time_T, Cm);
         Jrk := Jerk_At_Time (Profile, Time_T, Cm);
         Snp := Snap_At_Time (Profile, Time_T, Cm);
         Crk := Crackle_At_Time (Profile, Time_T, Cm);
         V := Velocity_At_Time (Profile, Time_T, Cm, Start_Vel);

         T.Assert
           (abs Acc <= Am + Am * Epsilon,
            Name & ": Max Acceleration exceeded at " & Time_T'Image & " (" & Acc'Image & ")");
         T.Assert
           (abs Jrk <= Jm + Jm * Epsilon, Name & ": Max Jerk exceeded at " & Time_T'Image & " (" & Jrk'Image & ")");
         T.Assert
           (abs Snp <= Sm + Sm * Epsilon, Name & ": Max Snap exceeded at " & Time_T'Image & " (" & Snp'Image & ")");
         T.Assert
           (abs Crk <= Cm + Cm * Epsilon, Name & ": Max Crackle exceeded at " & Time_T'Image & " (" & Crk'Image & ")");
         T.Assert
           (abs V <= Max_Vel + Max_Vel * Epsilon,
            Name & ": Max Velocity exceeded at " & Time_T'Image & " (" & V'Image & ")");
         T.Assert
           (V >= -1.0 * Epsilon * mm / s, Name & ": Negative Velocity at " & Time_T'Image & " (" & V'Image & ")");
      end loop;

      D := Distance_At_Time (Profile, Total_T, Cm, Start_Vel);
      T.Assert
        (abs (D - Distance) < 0.001 * mm, Name & ": Incorrect end distance " & D'Image & " vs " & Distance'Image);

      V := Velocity_At_Time (Profile, Total_T, Cm, Start_Vel);
      T.Assert
        (abs (V - End_Vel) < 0.001 * mm / s, Name & ": Incorrect end velocity " & V'Image & " vs " & End_Vel'Image);
   end Check_Full_Profile;

   procedure Test_Optimal_Full_Profile_Reach_Max_Vel (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vm : constant Velocity := 20.0 * mm / s;
      Vs : constant Velocity := 0.0 * mm / s;
      Ve : constant Velocity := 0.0 * mm / s;
      Dist : constant Length := 1000.0 * mm;

      Check_Full_Profile (Vs, Vm, Ve, Dist, Am, Jm, Sm, Cm, "Reach Max Vel", T);
   end Test_Optimal_Full_Profile_Reach_Max_Vel;

   procedure Test_Optimal_Full_Profile_Start_End_Constraints (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vm : constant Velocity := 50.0 * mm / s;
      Vs : constant Velocity := 10.0 * mm / s;
      Ve : constant Velocity := 20.0 * mm / s;
      Dist : constant Length := 500.0 * mm;

      Check_Full_Profile (Vs, Vm, Ve, Dist, Am, Jm, Sm, Cm, "Start/End Constraints", T);
   end Test_Optimal_Full_Profile_Start_End_Constraints;

   procedure Test_Optimal_Full_Profile_Triangle (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vm : constant Velocity := 1000.0 * mm / s; -- Unreachable
      Vs : constant Velocity := 0.0 * mm / s;
      Ve : constant Velocity := 0.0 * mm / s;
      Dist : constant Length := 10.0 * mm;

      Check_Full_Profile (Vs, Vm, Ve, Dist, Am, Jm, Sm, Cm, "Triangle", T);
   end Test_Optimal_Full_Profile_Triangle;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V1_1 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;

      V_Bound_Bot : constant Velocity := 8.0 * Sm ** 4 / Cm ** 3;

      for I in 0 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     => V_Bound_Bot * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_1,
            Index       => 1,
            Name        => "Case V1.1 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 2,
         Name        => "Case V1.1 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Delta_V_Case_V1_1;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V1_2 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;

      V_Bound_Top : constant Velocity := 2.0 * Jm * (Jm / Sm + Sm / Cm) ** 2;
      V_Bound_Bot : constant Velocity := 8.0 * Sm ** 4 / Cm ** 3;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot + (V_Bound_Top - V_Bound_Bot) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 1,
         Name        => "Case V1.2 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     =>
              V_Bound_Bot + (V_Bound_Top - V_Bound_Bot) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_1,
            Index       => 2,
            Name        => "Case V1.2 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot
           + (V_Bound_Top - V_Bound_Bot)
             * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 3,
         Name        => "Case V1.2 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Delta_V_Case_V1_2;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V1_3 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;

      V_Bound_Top : constant Velocity := Am * (Am / Jm + Jm / Sm + Sm / Cm);
      V_Bound_Bot : constant Velocity := 2.0 * Jm * (Jm / Sm + Sm / Cm) ** 2;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot + (V_Bound_Top - V_Bound_Bot) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 2,
         Name        => "Case V1.3 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     =>
              V_Bound_Bot + (V_Bound_Top - V_Bound_Bot) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_1,
            Index       => 3,
            Name        => "Case V1.3 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot
           + (V_Bound_Top - V_Bound_Bot)
             * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 4,
         Name        => "Case V1.3 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Delta_V_Case_V1_3;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V1_4 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;

      V_Bound : constant Velocity := Am * (Am / Jm + Jm / Sm + Sm / Cm);
      Min_V : constant Velocity := V_Bound;
      Max_V : constant Velocity := V_Bound + 1.0 * mm / s;

      Check_Profile_For_Delta_V
        (Delta_V     => Min_V + (Max_V - Min_V) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 3,
         Name        => "Case V1.4 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     => Min_V + (Max_V - Min_V) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_1,
            Index       => 4,
            Name        => "Case V1.4 (" & I'Image & ")",
            T           => T);
      end loop;
   end Test_Optimal_Profile_For_Delta_V_Case_V1_4;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V2_1 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 0.01 * mm / s ** 2;

      V_Bound_Bot : constant Velocity := 8.0 * Sm ** 4 / Cm ** 3;

      for I in 0 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     => V_Bound_Bot * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_2,
            Index       => 1,
            Name        => "Case V2.1 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_2,
         Index       => 2,
         Name        => "Case V2.1 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Delta_V_Case_V2_1;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V2_2 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 0.01 * mm / s ** 2;

      V_Bound_Top : constant Velocity := Am * (2.0 * (0.25 * Sm ** 2 / Cm ** 2 + Am / Sm) ** (1 / 2) + Sm / Cm);
      V_Bound_Bot : constant Velocity := 8.0 * Sm ** 4 / Cm ** 3;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot + (V_Bound_Top - V_Bound_Bot) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_2,
         Index       => 1,
         Name        => "Case V2.2 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     =>
              V_Bound_Bot + (V_Bound_Top - V_Bound_Bot) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_2,
            Index       => 2,
            Name        => "Case V2.2 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot
           + (V_Bound_Top - V_Bound_Bot)
             * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_2,
         Index       => 4,
         Name        => "Case V2.2 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Delta_V_Case_V2_2;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V2_4 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 0.01 * mm / s ** 2;

      V_Bound : constant Velocity := Am * (2.0 * (0.25 * Sm ** 2 / Cm ** 2 + Am / Sm) ** (1 / 2) + Sm / Cm);
      Min_V : constant Velocity := V_Bound;
      Max_V : constant Velocity := V_Bound + 1.0 * mm / s;

      Check_Profile_For_Delta_V
        (Delta_V     => Min_V + (Max_V - Min_V) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_2,
         Index       => 2,
         Name        => "Case V2.4 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     => Min_V + (Max_V - Min_V) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_2,
            Index       => 4,
            Name        => "Case V2.4 (" & I'Image & ")",
            T           => T);
      end loop;
   end Test_Optimal_Profile_For_Delta_V_Case_V2_4;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V3_1 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 0.001 * mm / s ** 2;

      V_Bound : constant Velocity := 8.0 * Cm * (0.5 * Am / Cm) ** (4 / 3);

      for I in 0 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     => V_Bound * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_3,
            Index       => 1,
            Name        => "Case V3.1 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Delta_V
        (Delta_V     => V_Bound * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_3,
         Index       => 4,
         Name        => "Case V3.1 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Delta_V_Case_V3_1;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V3_4 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 0.001 * mm / s ** 2;

      V_Bound : constant Velocity := 8.0 * Cm * (0.5 * Am / Cm) ** (4 / 3);
      Min_V : constant Velocity := V_Bound;
      Max_V : constant Velocity := V_Bound + 0.1 * mm / s;

      Check_Profile_For_Delta_V
        (Delta_V     => Min_V + (Max_V - Min_V) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_3,
         Index       => 1,
         Name        => "Case V3.4 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     => Min_V + (Max_V - Min_V) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_3,
            Index       => 4,
            Name        => "Case V3.4 (" & I'Image & ")",
            T           => T);
      end loop;
   end Test_Optimal_Profile_For_Delta_V_Case_V3_4;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V4_1 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 10.0 * mm / s ** 3;
      Am : constant Acceleration := 100.0 * mm / s ** 2;

      V_Bound_Bot : constant Velocity := 8.0 * Jm ** 2 / Cm;

      for I in 0 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     => V_Bound_Bot * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_4,
            Index       => 1,
            Name        => "Case V4.1 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_4,
         Index       => 3,
         Name        => "Case V4.1 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Delta_V_Case_V4_1;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V4_3 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 10.0 * mm / s ** 3;
      Am : constant Acceleration := 100.0 * mm / s ** 2;

      V_Bound_Top : constant Velocity := Am * (Am / Jm + 2.0 * (Jm / Cm) ** (1 / 2));
      V_Bound_Bot : constant Velocity := 8.0 * Jm ** 2 / Cm;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot + (V_Bound_Top - V_Bound_Bot) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_4,
         Index       => 1,
         Name        => "Case V4.3 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     =>
              V_Bound_Bot + (V_Bound_Top - V_Bound_Bot) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_4,
            Index       => 3,
            Name        => "Case V4.3 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Delta_V
        (Delta_V     =>
           V_Bound_Bot
           + (V_Bound_Top - V_Bound_Bot)
             * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_4,
         Index       => 4,
         Name        => "Case V4.3 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Delta_V_Case_V4_3;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V4_4 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 10.0 * mm / s ** 3;
      Am : constant Acceleration := 100.0 * mm / s ** 2;

      V_Bound : constant Velocity := Am * (Am / Jm + 2.0 * (Jm / Cm) ** (1 / 2));
      Min_V : constant Velocity := V_Bound;
      Max_V : constant Velocity := V_Bound + 1.0 * mm / s;

      Check_Profile_For_Delta_V
        (Delta_V     => Min_V + (Max_V - Min_V) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_4,
         Index       => 3,
         Name        => "Case V4.4 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     => Min_V + (Max_V - Min_V) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_4,
            Index       => 4,
            Name        => "Case V4.4 (" & I'Image & ")",
            T           => T);
      end loop;
   end Test_Optimal_Profile_For_Delta_V_Case_V4_4;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V5_1 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 10.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;

      V_Bound : constant Velocity := 8.0 * Cm * (0.5 * Am / Cm) ** (4 / 3);

      for I in 0 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     => V_Bound * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_5,
            Index       => 1,
            Name        => "Case V5.1 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Delta_V
        (Delta_V     => V_Bound * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_5,
         Index       => 4,
         Name        => "Case V5.1 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Delta_V_Case_V5_1;

   procedure Test_Optimal_Profile_For_Delta_V_Case_V5_4 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 10.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;

      V_Bound : constant Velocity := 8.0 * Cm * (0.5 * Am / Cm) ** (4 / 3);
      Min_V : constant Velocity := V_Bound;
      Max_V : constant Velocity := V_Bound + 0.1 * mm / s;

      Check_Profile_For_Delta_V
        (Delta_V     => Min_V + (Max_V - Min_V) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_5,
         Index       => 1,
         Name        => "Case V5.4 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Delta_V
           (Delta_V     => Min_V + (Max_V - Min_V) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_5,
            Index       => 4,
            Name        => "Case V5.4 (" & I'Image & ")",
            T           => T);
      end loop;
   end Test_Optimal_Profile_For_Delta_V_Case_V5_4;

   procedure Test_Optimal_Profile_For_Distance_Case_D1_1 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P2 : constant Feedrate_Profile_Times := [Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s];
      Dist2 : constant Length := Fast_Distance_At_Max_Time (P2, Cm, Vs);

      for I in 0 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    => Dist2 * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_1,
            Index       => 1,
            Name        => "Case D1.1 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Dist2 * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 2,
         Name        => "Case D1.1 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Distance_Case_D1_1;

   procedure Test_Optimal_Profile_For_Distance_Case_D1_2 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P3 : constant Feedrate_Profile_Times := [Sm / Cm, Jm / Sm - Sm / Cm, 0.0 * s, 0.0 * s];
      Dist3 : constant Length := Fast_Distance_At_Max_Time (P3, Cm, Vs);

      P2 : constant Feedrate_Profile_Times := [Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s];
      Dist2 : constant Length := Fast_Distance_At_Max_Time (P2, Cm, Vs);

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Dist2 + (Dist3 - Dist2) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 1,
         Name        => "Case D1.2 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    => Dist2 + (Dist3 - Dist2) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_1,
            Index       => 2,
            Name        => "Case D1.2 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Distance
        (Vs,
         Dist2 + (Dist3 - Dist2) * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Am,
         Jm,
         Sm,
         Cm,
         Region_1,
         3,
         "Case D1.2 Boundary High (I=N+1)",
         T);
   end Test_Optimal_Profile_For_Distance_Case_D1_2;

   procedure Test_Optimal_Profile_For_Distance_Case_D1_3 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P4 : constant Feedrate_Profile_Times := [Sm / Cm, Jm / Sm - Sm / Cm, Am / Jm - Jm / Sm - Sm / Cm, 0.0 * s];
      Dist4 : constant Length := Fast_Distance_At_Max_Time (P4, Cm, Vs);

      P3 : constant Feedrate_Profile_Times := [Sm / Cm, Jm / Sm - Sm / Cm, 0.0 * s, 0.0 * s];
      Dist3 : constant Length := Fast_Distance_At_Max_Time (P3, Cm, Vs);

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Dist3 + (Dist4 - Dist3) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 2,
         Name        => "Case D1.3 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    => Dist3 + (Dist4 - Dist3) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_1,
            Index       => 3,
            Name        => "Case D1.3 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    =>
           Dist3 + (Dist4 - Dist3) * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 4,
         Name        => "Case D1.3 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Distance_Case_D1_3;

   procedure Test_Optimal_Profile_For_Distance_Case_D1_4 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P4 : constant Feedrate_Profile_Times := [Sm / Cm, Jm / Sm - Sm / Cm, Am / Jm - Jm / Sm - Sm / Cm, 0.0 * s];
      Dist : constant Length := Fast_Distance_At_Max_Time (P4, Cm, Vs);

      Min_Dist : constant Length := Dist;
      Max_Dist : constant Length := Dist + 10.0 * mm;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Min_Dist + (Max_Dist - Min_Dist) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_1,
         Index       => 3,
         Name        => "Case D1.4 Boundary Low",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    =>
              Min_Dist + (Max_Dist - Min_Dist) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_1,
            Index       => 4,
            Name        => "Case D1.4 (" & I'Image & ")",
            T           => T);
      end loop;
   end Test_Optimal_Profile_For_Distance_Case_D1_4;

   procedure Test_Optimal_Profile_For_Distance_Case_D2_1 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 0.01 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P2 : constant Feedrate_Profile_Times := [Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s];
      Dist2 : constant Length := Fast_Distance_At_Max_Time (P2, Cm, Vs);

      for I in 0 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    => Dist2 * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_2,
            Index       => 1,
            Name        => "Case D2.1 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Dist2 * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_2,
         Index       => 2,
         Name        => "Case D2.1 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Distance_Case_D2_1;

   procedure Test_Optimal_Profile_For_Distance_Case_D2_2 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 0.01 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P4 : constant Feedrate_Profile_Times :=
        [Sm / Cm, (0.25 * Sm ** 2 / Cm ** 2 + Am / Sm) ** (1 / 2) - 1.5 * Sm / Cm, 0.0 * s, 0.0 * s];
      Dist4 : constant Length := Fast_Distance_At_Max_Time (P4, Cm, Vs);

      P2 : constant Feedrate_Profile_Times := [Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s];
      Dist2 : constant Length := Fast_Distance_At_Max_Time (P2, Cm, Vs);

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Dist2 + (Dist4 - Dist2) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_2,
         Index       => 1,
         Name        => "Case D2.2 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    => Dist2 + (Dist4 - Dist2) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_2,
            Index       => 2,
            Name        => "Case D2.2 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    =>
           Dist2 + (Dist4 - Dist2) * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_2,
         Index       => 4,
         Name        => "Case D2.2 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Distance_Case_D2_2;

   procedure Test_Optimal_Profile_For_Distance_Case_D2_4 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 0.01 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P4 : constant Feedrate_Profile_Times :=
        [Sm / Cm, (0.25 * Sm ** 2 / Cm ** 2 + Am / Sm) ** (1 / 2) - 1.5 * Sm / Cm, 0.0 * s, 0.0 * s];
      Dist4 : constant Length := Fast_Distance_At_Max_Time (P4, Cm, Vs);

      Min_Dist : constant Length := Dist4;
      Max_Dist : constant Length := Dist4 + 10.0 * mm;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Min_Dist + (Max_Dist - Min_Dist) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_2,
         Index       => 2,
         Name        => "Case D2.4 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    =>
              Min_Dist + (Max_Dist - Min_Dist) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_2,
            Index       => 4,
            Name        => "Case D2.4 (" & I'Image & ")",
            T           => T);
      end loop;
   end Test_Optimal_Profile_For_Distance_Case_D2_4;

   procedure Test_Optimal_Profile_For_Distance_Case_D3_1 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 0.001 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P4 : constant Feedrate_Profile_Times := [(0.5 * Am / Cm) ** (1 / 3), 0.0 * s, 0.0 * s, 0.0 * s];
      Dist4 : constant Length := Fast_Distance_At_Max_Time (P4, Cm, Vs);

      for I in 0 .. N_Boundary_Intervals - 1 loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    => Dist4 * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_3,
            Index       => 1,
            Name        => "Case D3.1 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Dist4 * (Dimensionless (N_Boundary_Intervals) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_3,
         Index       => 4,
         Name        => "Case D3.1 Boundary High (I=N)",
         T           => T);
   end Test_Optimal_Profile_For_Distance_Case_D3_1;

   procedure Test_Optimal_Profile_For_Distance_Case_D3_4 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1000.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 1.0 * mm / s ** 3;
      Am : constant Acceleration := 0.001 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P4 : constant Feedrate_Profile_Times := [(0.5 * Am / Cm) ** (1 / 3), 0.0 * s, 0.0 * s, 0.0 * s];
      Dist4 : constant Length := Fast_Distance_At_Max_Time (P4, Cm, Vs);

      Min_Dist : constant Length := Dist4;
      Max_Dist : constant Length := Dist4 + 1.0 * mm;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => 0.0 * mm,
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_3,
         Index       => 1,
         Name        => "Case D3.4 Boundary Low (I=-1)",
         T           => T);

      for I in 0 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    =>
              Min_Dist + (Max_Dist - Min_Dist) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_3,
            Index       => 4,
            Name        => "Case D3.4 (" & I'Image & ")",
            T           => T);
      end loop;
   end Test_Optimal_Profile_For_Distance_Case_D3_4;

   procedure Test_Optimal_Profile_For_Distance_Case_D4_1 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 10.0 * mm / s ** 3;
      Am : constant Acceleration := 100.0 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P3 : constant Feedrate_Profile_Times := [(Jm / Cm) ** (1 / 2), 0.0 * s, 0.0 * s, 0.0 * s];
      Dist3 : constant Length := Fast_Distance_At_Max_Time (P3, Cm, Vs);

      for I in 0 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    => Dist3 * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_4,
            Index       => 1,
            Name        => "Case D4.1 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Dist3 * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_4,
         Index       => 3,
         Name        => "Case D4.1 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Distance_Case_D4_1;

   procedure Test_Optimal_Profile_For_Distance_Case_D4_3 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 10.0 * mm / s ** 3;
      Am : constant Acceleration := 100.0 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P4 : constant Feedrate_Profile_Times :=
        [(Jm / Cm) ** (1 / 2), 0.0 * s, Am / Jm - 2.0 * (Jm / Cm) ** (1 / 2), 0.0 * s];
      Dist4 : constant Length := Fast_Distance_At_Max_Time (P4, Cm, Vs);

      P3 : constant Feedrate_Profile_Times := [(Jm / Cm) ** (1 / 2), 0.0 * s, 0.0 * s, 0.0 * s];
      Dist3 : constant Length := Fast_Distance_At_Max_Time (P3, Cm, Vs);

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Dist3 + (Dist4 - Dist3) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_4,
         Index       => 1,
         Name        => "Case D4.3 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    => Dist3 + (Dist4 - Dist3) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_4,
            Index       => 3,
            Name        => "Case D4.3 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    =>
           Dist3 + (Dist4 - Dist3) * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_4,
         Index       => 4,
         Name        => "Case D4.3 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Distance_Case_D4_3;

   procedure Test_Optimal_Profile_For_Distance_Case_D4_4 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 10.0 * mm / s ** 3;
      Am : constant Acceleration := 100.0 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P4 : constant Feedrate_Profile_Times :=
        [(Jm / Cm) ** (1 / 2), 0.0 * s, Am / Jm - 2.0 * (Jm / Cm) ** (1 / 2), 0.0 * s];

      Dist4 : constant Length := Fast_Distance_At_Max_Time (P4, Cm, Vs);

      Min_Dist : constant Length := Dist4;
      Max_Dist : constant Length := Dist4 + 10.0 * mm;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Min_Dist + (Max_Dist - Min_Dist) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_4,
         Index       => 3,
         Name        => "Case D4.4 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    =>
              Min_Dist + (Max_Dist - Min_Dist) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_4,
            Index       => 4,
            Name        => "Case D4.4 (" & I'Image & ")",
            T           => T);
      end loop;
   end Test_Optimal_Profile_For_Distance_Case_D4_4;

   procedure Test_Optimal_Profile_For_Distance_Case_D5_1 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 10.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;

      Vs : constant Velocity := 0.0 * mm / s;

      P4 : constant Feedrate_Profile_Times := [(Am / (2.0 * Cm)) ** (1 / 3), 0.0 * s, 0.0 * s, 0.0 * s];
      Dist4 : constant Length := Fast_Distance_At_Max_Time (P4, Cm, Vs);

      for I in 0 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    => Dist4 * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_5,
            Index       => 1,
            Name        => "Case D5.1 (" & I'Image & ")",
            T           => T);
      end loop;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Dist4 * (Dimensionless (N_Boundary_Intervals + 1) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_5,
         Index       => 4,
         Name        => "Case D5.1 Boundary High (I=N+1)",
         T           => T);
   end Test_Optimal_Profile_For_Distance_Case_D5_1;

   procedure Test_Optimal_Profile_For_Distance_Case_D5_4 (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Cm : constant Crackle := 1.0 * mm / s ** 5;
      Sm : constant Snap := 10.0 * mm / s ** 4;
      Jm : constant Jerk := 10.0 * mm / s ** 3;
      Am : constant Acceleration := 10.0 * mm / s ** 2;
      Vs : constant Velocity := 0.0 * mm / s;

      P4 : constant Feedrate_Profile_Times := [(Am / (2.0 * Cm)) ** (1 / 3), 0.0 * s, 0.0 * s, 0.0 * s];
      Dist4 : constant Length := Fast_Distance_At_Max_Time (P4, Cm, Vs);

      Min_Dist : constant Length := Dist4;
      Max_Dist : constant Length := Dist4 + 1.0 * mm;

      Check_Profile_For_Distance
        (Start_Vel   => Vs,
         Distance    => Min_Dist + (Max_Dist - Min_Dist) * (Dimensionless (0) / Dimensionless (N_Boundary_Intervals)),
         Max_Accel   => Am,
         Max_Jerk    => Jm,
         Max_Snap    => Sm,
         Max_Crackle => Cm,
         Region      => Region_5,
         Index       => 1,
         Name        => "Case D5.4 Boundary Low (I=0)",
         T           => T);

      for I in 1 .. N_Boundary_Intervals loop
         Check_Profile_For_Distance
           (Start_Vel   => Vs,
            Distance    =>
              Min_Dist + (Max_Dist - Min_Dist) * (Dimensionless (I) / Dimensionless (N_Boundary_Intervals)),
            Max_Accel   => Am,
            Max_Jerk    => Jm,
            Max_Snap    => Sm,
            Max_Crackle => Cm,
            Region      => Region_5,
            Index       => 4,
            Name        => "Case D5.4 (" & I'Image & ")",
            T           => T);
      end loop;
   end Test_Optimal_Profile_For_Distance_Case_D5_4;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Distance_At_Time_Is_Past_Accel'Access,
         Test_Distance_At_Time_Phases'Access,
         Test_Fast_Vs_Slow'Access,
         Test_Integration_Acceleration_To_Velocity'Access,
         Test_Integration_Crackle_To_Snap'Access,
         Test_Integration_Jerk_To_Acceleration'Access,
         Test_Integration_Snap_To_Jerk'Access,
         Test_Integration_Velocity_To_Distance'Access,
         Test_Optimal_Full_Profile_Constraint_Errors'Access,
         Test_Optimal_Full_Profile_Errors'Access,
         Test_Optimal_Full_Profile_Impossible'Access,
         Test_Optimal_Full_Profile_Reach_Max_Vel'Access,
         Test_Optimal_Full_Profile_Start_End_Constraints'Access,
         Test_Optimal_Full_Profile_Triangle'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V1_1'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V1_2'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V1_3'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V1_4'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V2_1'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V2_2'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V2_4'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V3_1'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V3_4'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V4_1'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V4_3'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V4_4'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V5_1'Access,
         Test_Optimal_Profile_For_Delta_V_Case_V5_4'Access,
         Test_Optimal_Profile_For_Distance_Case_D1_1'Access,
         Test_Optimal_Profile_For_Distance_Case_D1_2'Access,
         Test_Optimal_Profile_For_Distance_Case_D1_3'Access,
         Test_Optimal_Profile_For_Distance_Case_D1_4'Access,
         Test_Optimal_Profile_For_Distance_Case_D2_1'Access,
         Test_Optimal_Profile_For_Distance_Case_D2_2'Access,
         Test_Optimal_Profile_For_Distance_Case_D2_4'Access,
         Test_Optimal_Profile_For_Distance_Case_D3_1'Access,
         Test_Optimal_Profile_For_Distance_Case_D3_4'Access,
         Test_Optimal_Profile_For_Distance_Case_D4_1'Access,
         Test_Optimal_Profile_For_Distance_Case_D4_3'Access,
         Test_Optimal_Profile_For_Distance_Case_D4_4'Access,
         Test_Optimal_Profile_For_Distance_Case_D5_1'Access,
         Test_Optimal_Profile_For_Distance_Case_D5_4'Access];
   end All_Tests;

end Prunt.Motion_Planner.Test;
