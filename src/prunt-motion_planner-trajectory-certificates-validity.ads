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

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

--  What Profile_Is_Valid establishes, in exact arithmetic.
--
--  Profile_Is_Valid compares, in floating point, the travel and the final speed of a profile with the distance and
--  the end speed it was asked for, to one part in 10**9. The theorem below says what that means for the exact
--  profile which the phase times denote: its acceleration ramp from the starting speed, a coast at the speed which
--  that ramp ends at, and its deceleration ramp from there, each as Trajectory.Profile_Model defines a ramp.
--  That profile covers the distance and ends at the end speed to within the tolerance of the check and the proved
--  rounding of the check's arithmetic.

with Prunt.Motion_Planner.Trajectory.Solvers; use Prunt.Motion_Planner.Trajectory.Solvers;

package Prunt.Motion_Planner.Trajectory.Certificates.Validity
  with SPARK_Mode => On, Ghost, Always_Terminates
is

   Certified_Validity_Tolerance : constant := 1.01E-9;
   --  How far, relative to the distance asked for or to the highest speed of the profile, the exact profile is
   --  from what was asked: the tolerance of the check, 1.0E-9, and the rounding of its arithmetic.

   Certified_Distance_Residue : constant := 2.0**(-500);
   Certified_Speed_Residue    : constant := 2.0**(-500);
   --  How far it is beyond that, in millimetres and in millimetres per second, where a product underflows.

   function Is_Certifiable
     (Profile : Feedrate_Profile; Distance : Length; Start_Vel, End_Vel : Velocity; Max_Crackle : Crackle)
      return Boolean
   is (Rounding_Is_Bounded (Profile.Accel, Max_Crackle, Start_Vel)
       and then Rounding_Is_Bounded (Profile.Decel, Max_Crackle, Start_Vel)
       and then Profile.Coast >= No_Time
       and then Profile.Coast <= Rounding_Time_Max
       and then Max_Crackle >= 0.0 * mm / s**5
       and then Distance >= No_Distance
       and then Distance <= Distance_Max
       and then Start_Vel >= No_Speed
       and then Start_Vel <= Speed_Max
       and then End_Vel >= No_Speed
       and then End_Vel <= Speed_Max);
   --  A profile and a request small enough for the rounding of the check's arithmetic to be bounded: times up to
   --  2**100 s, crackle up to 2**80, speeds and distance up to 2**80.

   function Accel_Ramp (Profile : Feedrate_Profile; Max_Crackle : Crackle; Start_Vel : Velocity) return Ramp
   is (To_Ramp (Profile.Accel, Max_Crackle, Start_Vel));
   --  The exact acceleration ramp of a profile.

   function Exact_Peak (Profile : Feedrate_Profile; Max_Crackle : Crackle; Start_Vel : Velocity) return Real
   is (R (Start_Vel) + Velocity_Gain (Accel_Ramp (Profile, Max_Crackle, Start_Vel)));
   --  The speed at which the exact acceleration ramp ends, and at which the profile coasts.

   function Decel_Ramp (Profile : Feedrate_Profile; Max_Crackle : Crackle; Start_Vel : Velocity) return Ramp
   is ((T1 => R (Profile.Decel (1)),
        T2 => R (Profile.Decel (2)),
        T3 => R (Profile.Decel (3)),
        T4 => R (Profile.Decel (4)),
        C  => 0.0 - R (Max_Crackle),
        Vs => Exact_Peak (Profile, Max_Crackle, Start_Vel)));
   --  The exact deceleration ramp of a profile, which starts at the speed of the coast.

   function Exact_Finish (Profile : Feedrate_Profile; Max_Crackle : Crackle; Start_Vel : Velocity) return Real
   is (Exact_Peak (Profile, Max_Crackle, Start_Vel) + Velocity_Gain (Decel_Ramp (Profile, Max_Crackle, Start_Vel)));
   --  The speed at which the exact profile ends.

   function Exact_Distance (Profile : Feedrate_Profile; Max_Crackle : Crackle; Start_Vel : Velocity) return Real
   is (Travel (Accel_Ramp (Profile, Max_Crackle, Start_Vel))
       + Exact_Peak (Profile, Max_Crackle, Start_Vel) * R (Profile.Coast)
       + Travel (Decel_Ramp (Profile, Max_Crackle, Start_Vel)));
   --  The distance which the exact profile covers.

   function Exact_Top_Speed
     (Profile : Feedrate_Profile; Max_Crackle : Crackle; Start_Vel, End_Vel : Velocity) return Real
   is (if Exact_Peak (Profile, Max_Crackle, Start_Vel) >= R (End_Vel)
       then Exact_Peak (Profile, Max_Crackle, Start_Vel)
       else R (End_Vel));
   --  The larger of the coasting speed and the end speed asked for. The coasting speed is never below the
   --  starting speed.

   procedure Lemma_Valid_Profile_Is_Exact
     (Profile : Feedrate_Profile; Distance : Length; Start_Vel, End_Vel : Velocity; Max_Crackle : Crackle)
   with
     Pre  =>
       Is_Certifiable (Profile, Distance, Start_Vel, End_Vel, Max_Crackle)
       and then Profile_Is_Valid (Profile, Distance, Start_Vel, End_Vel, Max_Crackle),
     Post =>
       Is_Ramp (Accel_Ramp (Profile, Max_Crackle, Start_Vel))
       and then Is_Ramp (Decel_Ramp (Profile, Max_Crackle, Start_Vel))
       and then R (Profile.Coast) >= 0.0
       and then Exact_Peak (Profile, Max_Crackle, Start_Vel) >= R (Start_Vel)
       and then Exact_Finish (Profile, Max_Crackle, Start_Vel) <= Exact_Peak (Profile, Max_Crackle, Start_Vel)
       and then
         abs (Exact_Distance (Profile, Max_Crackle, Start_Vel) - R (Distance))
         <= Certified_Validity_Tolerance * R (Distance) + Certified_Distance_Residue
       and then
         abs (Exact_Finish (Profile, Max_Crackle, Start_Vel) - R (End_Vel))
         <= Certified_Validity_Tolerance
            * Exact_Top_Speed (Profile, Max_Crackle, Start_Vel, End_Vel)
            + Certified_Speed_Residue;
   --  A profile which Profile_Is_Valid accepts is made of two ramps and a coast of nonnegative times; it speeds up
   --  to its coasting speed and slows down from it; it covers the distance asked for to within 1.01 parts in
   --  10**9; and it ends at the end speed asked for to within 1.01 parts in 10**9 of its highest speed.

   procedure Lemma_Speed_At_Every_Instant
     (Profile : Feedrate_Profile; Max_Crackle : Crackle; Start_Vel : Velocity; Stage : Stage_Index; DT : Real)
   with
     Pre  =>
       Is_Ramp (Accel_Ramp (Profile, Max_Crackle, Start_Vel))
       and then Is_Ramp (Decel_Ramp (Profile, Max_Crackle, Start_Vel))
       and then Max_Crackle >= 0.0 * mm / s**5
       and then DT >= 0.0,
     Post =>
       (if In_Stage (Accel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT)
        then
          State_In (Accel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT).Vel
          <= Exact_Peak (Profile, Max_Crackle, Start_Vel))
       and then
         (if In_Stage (Decel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT)
          then
            State_In (Decel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT).Vel
            <= Exact_Peak (Profile, Max_Crackle, Start_Vel));
   --  At no instant of either ramp does the exact profile move faster than it coasts. With the postcondition of
   --  Solvers.Try_Optimal_Full_Profile, which bounds that speed by Max_Vel, no instant of the exact profile of a
   --  reachable result is faster than Max_Vel, to Solvers.Top_Speed_Tolerance.

   procedure Lemma_Limits_At_Every_Instant_Of_Profile
     (Profile          : Feedrate_Profile;
      Max_Crackle      : Crackle;
      Start_Vel        : Velocity;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Stage            : Stage_Index;
      DT               : Real)
   with
     Pre  =>
       Is_Ramp (Accel_Ramp (Profile, Max_Crackle, Start_Vel))
       and then Is_Ramp (Decel_Ramp (Profile, Max_Crackle, Start_Vel))
       and then Max_Crackle >= 0.0 * mm / s**5
       and then
         Peaks_Are_Limited
           (Profile.Accel (1), Profile.Accel (2), Profile.Accel (3), Acceleration_Max, Jerk_Max, Snap_Max, Max_Crackle)
       and then
         Peaks_Are_Limited
           (Profile.Decel (1),
            Profile.Decel (2),
            Profile.Decel (3),
            Acceleration_Max,
            Jerk_Max,
            Snap_Max,
            Max_Crackle),
     Post =>
       (if In_Stage (Accel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT)
        then
          abs Stage_Crackle (Accel_Ramp (Profile, Max_Crackle, Start_Vel), Stage) <= R (Max_Crackle)
          and then
            abs State_In (Accel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT).Snap
            <= R (Snap_Max) * (1.0 + Certified_Tolerance)
          and then
            abs State_In (Accel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT).Jerk
            <= R (Jerk_Max) * (1.0 + Certified_Tolerance)
          and then
            abs State_In (Accel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT).Accel
            <= R (Acceleration_Max) * (1.0 + Certified_Tolerance))
       and then
         (if In_Stage (Decel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT)
          then
            abs Stage_Crackle (Decel_Ramp (Profile, Max_Crackle, Start_Vel), Stage) <= R (Max_Crackle)
            and then
              abs State_In (Decel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT).Snap
              <= R (Snap_Max) * (1.0 + Certified_Tolerance)
            and then
              abs State_In (Decel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT).Jerk
              <= R (Jerk_Max) * (1.0 + Certified_Tolerance)
            and then
              abs State_In (Decel_Ramp (Profile, Max_Crackle, Start_Vel), Stage, DT).Accel
              <= R (Acceleration_Max) * (1.0 + Certified_Tolerance));
   --  With the peaks of both ramps limited, which is the postcondition of Solvers.Try_Optimal_Full_Profile for a
   --  reachable result with positive limits within the proved domain: at every instant of either ramp of the
   --  exact profile the crackle is within its limit, and snap, jerk and acceleration are within theirs to the
   --  certified tolerance. The coast has none of them.

end Prunt.Motion_Planner.Trajectory.Certificates.Validity;
