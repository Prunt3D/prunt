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
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

--  Certificates which tie the floating-point phase times of a ramp to the exact ramp of
--  Prunt.Motion_Planner.Trajectory.Profile_Model.
--
--  Nothing in Prunt calls these: the solvers respect the derivative limits by construction, up to rounding. The
--  check below makes that an independent, proved statement about any ramp, whoever produced it: if it accepts the
--  phase times, then at every instant of the exact ramp which they denote, crackle, snap, jerk and acceleration are
--  within the limits, to within one part in 2**29.

with Prunt.Motion_Planner.Trajectory.Profile_Model; use Prunt.Motion_Planner.Trajectory.Profile_Model;
with Prunt.Motion_Planner.Trajectory.Profiles;      use Prunt.Motion_Planner.Trajectory.Profiles;
with Prunt.Motion_Planner.Reals;         use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Trajectory.Certificates
  with SPARK_Mode => On, Always_Terminates
is
   use type Prunt.Motion_Planner.Reals.Real;

   Certified_Time_Max : constant Time := Time (2#1.0#E100);
   --  The largest phase time of a ramp which can be certified.

   Certified_Crackle_Max : constant Crackle := Crackle (2#1.0#E100);
   --  The largest crackle magnitude of a ramp which can be certified.

   Certified_Acceleration_Min : constant Acceleration := Acceleration (2#1.0#E-100);
   Certified_Acceleration_Max : constant Acceleration := Acceleration (2#1.0#E100);
   Certified_Jerk_Min         : constant Jerk := Jerk (2#1.0#E-100);
   Certified_Jerk_Max         : constant Jerk := Jerk (2#1.0#E100);
   Certified_Snap_Min         : constant Snap := Snap (2#1.0#E-100);
   Certified_Snap_Max         : constant Snap := Snap (2#1.0#E100);
   --  The limits which a ramp can be certified against.

   Checked_Tolerance : constant := 2.0**(-30);
   --  How far above a limit the floating-point check lets a computed peak be. The solvers reach a limit to within
   --  a few units in the last place, or to within the accuracy of the library's cube root, which is far closer.

   Certified_Tolerance : constant := 2.0**(-29);
   --  How far above a limit the exact peak of an accepted ramp can be: the tolerance of the check and the rounding
   --  of the check's own arithmetic.

   function Limits_Are_Respected
     (Exact : Ramp; Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max : Real) return Boolean
   is (abs Exact.C <= Crackle_Max
       and then abs Peak_Snap (Exact) <= Snap_Max * (1.0 + Certified_Tolerance)
       and then abs Peak_Jerk (Exact) <= Jerk_Max * (1.0 + Certified_Tolerance)
       and then abs Peak_Accel (Exact) <= Acceleration_Max * (1.0 + Certified_Tolerance))
   with Ghost;
   --  The peaks which the phase times of an exact ramp imply are within the limits, to the certified tolerance.

   function Ramp_Respects_Limits
     (Times            : Feedrate_Profile_Times;
      Max_Crackle      : Crackle;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Boolean
   with
     Global => null,
     Pre    =>
       Times (1) >= No_Time
       and then Times (1) <= Certified_Time_Max
       and then Times (2) >= No_Time
       and then Times (2) <= Certified_Time_Max
       and then Times (3) >= No_Time
       and then Times (3) <= Certified_Time_Max
       and then Times (4) >= No_Time
       and then Times (4) <= Certified_Time_Max
       and then abs Max_Crackle <= Certified_Crackle_Max
       and then Crackle_Max >= 0.0 * mm / s**5
       and then Crackle_Max <= Certified_Crackle_Max
       and then Snap_Max >= Certified_Snap_Min
       and then Snap_Max <= Certified_Snap_Max
       and then Jerk_Max >= Certified_Jerk_Min
       and then Jerk_Max <= Certified_Jerk_Max
       and then Acceleration_Max >= Certified_Acceleration_Min
       and then Acceleration_Max <= Certified_Acceleration_Max,
     Post   =>
       (if Ramp_Respects_Limits'Result
        then
          Limits_Are_Respected
            (To_Ramp (Times, Max_Crackle, No_Velocity),
             R (Acceleration_Max),
             R (Jerk_Max),
             R (Snap_Max),
             R (Crackle_Max)));
   --  Whether the peaks of the ramp, computed in floating point as |Max_Crackle| * T1, that times (T1 + T2), and
   --  that times (2 * T1 + T2 + T3), are each at most the limit times (1 + Checked_Tolerance), and |Max_Crackle| at
   --  most Crackle_Max. Max_Crackle may be negative, as for a deceleration ramp.

   procedure Lemma_Limits_At_Every_Instant
     (Exact : Ramp; Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max : Real; Stage : Stage_Index; DT : Real)
   with
     Ghost,
     Pre  =>
       Is_Ramp (Exact)
       and then Limits_Are_Respected (Exact, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
       and then In_Stage (Exact, Stage, DT),
     Post =>
       abs Stage_Crackle (Exact, Stage) <= Crackle_Max
       and then abs State_In (Exact, Stage, DT).Snap <= Snap_Max * (1.0 + Certified_Tolerance)
       and then abs State_In (Exact, Stage, DT).Jerk <= Jerk_Max * (1.0 + Certified_Tolerance)
       and then abs State_In (Exact, Stage, DT).Accel <= Acceleration_Max * (1.0 + Certified_Tolerance);
   --  For a ramp which Ramp_Respects_Limits accepts, at every instant: crackle is within its limit, and snap, jerk
   --  and acceleration are within theirs to the certified tolerance.

end Prunt.Motion_Planner.Trajectory.Certificates;
