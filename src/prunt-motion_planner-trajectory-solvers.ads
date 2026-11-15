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

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

pragma Extensions_Allowed (On);

--  The solvers which find feedrate profiles: the fastest ramp over a distance or through a velocity change under
--  acceleration, jerk, snap and crackle limits, and the fastest complete profile between two velocities over a
--  distance.

with Prunt.Motion_Planner.Trajectory.Certificates;
with Prunt.Motion_Planner.Trajectory.Checked;
with Prunt.Motion_Planner.Trajectory.Machine_Numbers; use Prunt.Motion_Planner.Trajectory.Machine_Numbers;
with Prunt.Motion_Planner.Trajectory.Profiles;        use Prunt.Motion_Planner.Trajectory.Profiles;
with Prunt.Motion_Planner.Reals;
with Prunt.Motion_Planner.Trajectory.Rounding.Relative;

package Prunt.Motion_Planner.Trajectory.Solvers
  with SPARK_Mode => On
is

   type Constraint_Region is (Region_1, Region_2, Region_3, Region_4, Region_5);

   type Internal_Profile_Result is record
      Profile : Feedrate_Profile_Times;
      Region  : Constraint_Region;
      Index   : Integer;
   end record;

   type Delta_V_Solver is record
      Limits_Valid      : Boolean := False;
      Zero_Limit        : Boolean := False;
      Unclassifiable    : Boolean := False;
      Threshold_Raises  : Boolean := False;
      Region            : Constraint_Region := Region_5;
      Am                : Acceleration := 0.0 * mm / s**2;
      Jm                : Jerk := 0.0 * mm / s**3;
      Cm                : Crackle := 0.0 * mm / s**5;
      T1, T2, T3        : Time := 0.0 * s;
      Accel_Over_Jerk   : Time := 0.0 * s;
      Jerk_Over_Snap    : Time := 0.0 * s;
      Snap_Over_Crackle : Time := 0.0 * s;
      Snap_Root         : Time := 0.0 * s;
      Jerk_Over_Crackle : Time_Squared := 0.0 * s**2;
      Snap_Only_Above   : Velocity := 0.0 * mm / s;
   end record;
   --  Everything in Optimal_Profile_For_Delta_V that depends only on the derivative limits: the constraint region,
   --  the phase times of its fully saturated case (T1 to T3) and the quotients and roots that the partially
   --  saturated cases combine with the velocity change. A search over velocity changes under fixed limits prepares
   --  this once. Every value is formed by the same operations, in the same order, as a direct solve, so a prepared
   --  solve returns identical profiles.
   --
   --  Unclassifiable records that selecting the region raised Constraint_Error, and Threshold_Raises that the
   --  snap-only velocity threshold did. A solve raises the same error at the point where it would need the value.

   type Full_Profile_Result is record
      Reachable : Boolean := False;
      Profile   : Feedrate_Profile := (Accel => [others => 0.0 * s], Coast => 0.0 * s, Decel => [others => 0.0 * s]);
   end record;

   ------------------------------------------------------------------------------------------------------------------
   --  The proved domain
   --
   --  The solvers are proved for derivative limits, speeds and distances which are zero or within 2**-80 to 2**80
   --  of their units, about 8e-25 to 1.2e24. The preconditions are not checked when the program runs: outside
   --  them the solvers compute exactly what Prunt's do, including raising Constraint_Error where an operation
   --  overflows, but nothing is proved about that.
   ------------------------------------------------------------------------------------------------------------------

   Acceleration_Limit_Min : constant Acceleration := Acceleration (2#1.0#E-80);
   Acceleration_Limit_Max : constant Acceleration := Acceleration (2#1.0#E80);
   Jerk_Limit_Min         : constant Jerk := Jerk (2#1.0#E-80);
   Jerk_Limit_Max         : constant Jerk := Jerk (2#1.0#E80);
   Snap_Limit_Min         : constant Snap := Snap (2#1.0#E-80);
   Snap_Limit_Max         : constant Snap := Snap (2#1.0#E80);
   Crackle_Limit_Min      : constant Crackle := Crackle (2#1.0#E-80);
   Crackle_Limit_Max      : constant Crackle := Crackle (2#1.0#E80);

   No_Speed  : constant Velocity := 0.0;
   Speed_Min : constant Velocity := Velocity (2#1.0#E-80);
   Speed_Max : constant Velocity := Velocity (2#1.0#E80);
   --  The smallest positive speed which the solvers divide by, and the largest speed or change of speed.

   No_Distance  : constant Length := 0.0;
   Distance_Min : constant Length := Length (2#1.0#E-80);
   Distance_Max : constant Length := Length (2#1.0#E80);

   Solver_Time_Max : constant Time := Time (2#1.0#E163);
   --  No quotient or root of limits which a prepared solver holds is larger than this.

   Solver_Time_Squared_Max : constant Time_Squared := Time_Squared (2#1.0#E160);

   Snap_Only_Speed_Max : constant Velocity := Velocity (2#1.0#E563);

   Search_Time_Cap : constant Time := Time (86_400.0);
   --  The longest phase time which the two bisections of a phase time consider: 24 hours.

   Moderate_Time_Max : constant Time := Time (2#1.0#E99);
   --  The largest quotient of two successive limits for which the distance solver is proved to respect them.

   function Limits_In_Domain (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Am >= Acceleration_Limit_Min
       and then Am <= Acceleration_Limit_Max
       and then Jm >= Jerk_Limit_Min
       and then Jm <= Jerk_Limit_Max
       and then Sm >= Snap_Limit_Min
       and then Sm <= Snap_Limit_Max
       and then Cm >= Crackle_Limit_Min
       and then Cm <= Crackle_Limit_Max);
   --  Every limit is positive and within the proved domain.

   function Limits_Are_Moderate (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Limits_In_Domain (Am, Jm, Sm, Cm)
       and then Am / Jm <= Moderate_Time_Max
       and then Jm / Sm <= Moderate_Time_Max
       and then Sm / Cm <= Moderate_Time_Max)
   with Ghost;
   --  Limits within the proved domain whose saturated ramp has no phase longer than 2**100 s, which is as long
   --  as a ramp may be for the rounding of its travel to be bounded relatively.

   function Any_Limit_Is_Zero (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Am = 0.0 * mm / s**2 or else Jm = 0.0 * mm / s**3 or else Sm = 0.0 * mm / s**4 or else Cm = 0.0 * mm / s**5);
   --  With a zero limit the only motion possible is at constant speed.

   function Derivative_Limits_Are_Valid (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Am >= 0.0 * mm / s**2
       and then Am <= Acceleration'Last
       and then Jm >= 0.0 * mm / s**3
       and then Jm <= Jerk'Last
       and then Sm >= 0.0 * mm / s**4
       and then Sm <= Snap'Last
       and then Cm >= 0.0 * mm / s**5
       and then Cm <= Crackle'Last);
   --  Zero limits permit coasting, but negative or nonfinite limits are never meaningful.

   function Limits_Are_Provable (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (not Derivative_Limits_Are_Valid (Am, Jm, Sm, Cm)
       or else Any_Limit_Is_Zero (Am, Jm, Sm, Cm)
       or else Limits_In_Domain (Am, Jm, Sm, Cm));
   --  Limits for which the solvers are proved: invalid ones, which they reject, ones with a zero, and positive
   --  ones within the proved domain.

   function Solver_In_Domain (Solver : Delta_V_Solver) return Boolean
   is (Solver.Limits_Valid
       and then not Solver.Zero_Limit
       and then not Solver.Unclassifiable
       and then not Solver.Threshold_Raises
       and then Solver.Am >= Acceleration_Limit_Min
       and then Solver.Am <= Acceleration_Limit_Max
       and then Solver.Jm >= Jerk_Limit_Min
       and then Solver.Jm <= Jerk_Limit_Max
       and then Solver.Cm >= Crackle_Limit_Min
       and then Solver.Cm <= Crackle_Limit_Max
       and then Solver.T1 >= No_Time
       and then Solver.T1 <= Solver_Time_Max
       and then Solver.T2 >= No_Time
       and then Solver.T2 <= Solver_Time_Max
       and then Solver.T3 >= No_Time
       and then Solver.T3 <= Solver_Time_Max
       and then Solver.Accel_Over_Jerk >= No_Time
       and then Solver.Accel_Over_Jerk <= Solver_Time_Max
       and then Solver.Jerk_Over_Snap >= No_Time
       and then Solver.Jerk_Over_Snap <= Solver_Time_Max
       and then Solver.Snap_Over_Crackle >= No_Time
       and then Solver.Snap_Over_Crackle <= Solver_Time_Max
       and then Solver.Snap_Root >= No_Time
       and then Solver.Snap_Root <= Solver_Time_Max
       and then Solver.Jerk_Over_Crackle >= 0.0 * s**2
       and then Solver.Jerk_Over_Crackle <= Solver_Time_Squared_Max
       and then Solver.Snap_Only_Above >= No_Speed
       and then Solver.Snap_Only_Above <= Snap_Only_Speed_Max);
   --  What Prepare_Delta_V establishes for positive limits within the proved domain: no failure is recorded, and
   --  every quotient and root which the solves combine is nonnegative and bounded.

   function Solver_Is_Ready (Solver : Delta_V_Solver; Delta_V : Velocity) return Boolean
   is (Solver.Limits_Valid
       and then abs Delta_V <= Speed_Max
       and then (if Solver.Zero_Limit then Delta_V = No_Speed else Solver_In_Domain (Solver)));
   --  A solve of Delta_V with Solver returns phase times instead of raising.

   function Ramp_Request_Is_Provable
     (Delta_V : Velocity; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Derivative_Limits_Are_Valid (Am, Jm, Sm, Cm)
       and then abs Delta_V <= Speed_Max
       and then
         (if Any_Limit_Is_Zero (Am, Jm, Sm, Cm) then Delta_V = No_Speed else Limits_In_Domain (Am, Jm, Sm, Cm)));

   function Distance_Request_Is_Provable
     (Start_Vel : Velocity; Distance : Length; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Derivative_Limits_Are_Valid (Am, Jm, Sm, Cm)
       and then Distance >= No_Distance
       and then Distance <= Distance_Max
       and then Start_Vel >= No_Speed
       and then Start_Vel <= Speed_Max
       and then
         (if Any_Limit_Is_Zero (Am, Jm, Sm, Cm)
          then Distance = No_Distance or else (Start_Vel >= Speed_Min and then Distance >= Distance_Min)
          else Limits_In_Domain (Am, Jm, Sm, Cm)));

   function Times_Are_Bounded (Times : Feedrate_Profile_Times) return Boolean
   is (abs Times (1) <= Phase_Time_Max
       and then abs Times (2) <= Phase_Time_Max
       and then abs Times (3) <= Phase_Time_Max
       and then abs Times (4) <= Phase_Time_Max);

   function Validity_Inputs_Are_Bounded
     (Distance : Length; Start_Vel, End_Vel : Velocity; Max_Crackle : Crackle) return Boolean
   is (abs Distance <= Distance_Max
       and then abs Start_Vel <= Speed_Max
       and then abs End_Vel <= Speed_Max
       and then abs Max_Crackle <= Crackle_Limit_Max);

   function Full_Profile_Request_Is_Provable
     (Start_Vel, Max_Vel, End_Vel : Velocity; Distance : Length; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
      return Boolean
   is (abs Distance <= Distance_Max
       and then abs Start_Vel <= Speed_Max
       and then abs End_Vel <= Speed_Max
       and then abs Max_Vel <= Speed_Max
       and then Limits_Are_Provable (Am, Jm, Sm, Cm)
       and then (if Derivative_Limits_Are_Valid (Am, Jm, Sm, Cm) then Cm <= Crackle_Limit_Max)
       and then (Start_Vel <= No_Speed or else Start_Vel >= Speed_Min)
       and then (Max_Vel <= No_Speed or else Max_Vel >= Speed_Min));
   --  A request for which Try_Optimal_Full_Profile is proved. Speeds which it divides by are zero or not tiny. The
   --  crackle limit is within the proved domain even when another limit is zero, because the constant-speed
   --  profile of that case is still checked by Profile_Is_Valid under the crackle limit.

   function Passes_Validity_Checks
     (Profile                        : Feedrate_Profile;
      Distance                       : Length;
      Start_Vel, End_Vel             : Velocity;
      Max_Crackle                    : Crackle;
      Peak                           : Velocity;
      Accel_Distance, Decel_Distance : Length) return Boolean
   is (Distance >= No_Distance
       and then Start_Vel >= No_Speed
       and then End_Vel >= No_Speed
       and then Max_Crackle >= 0.0 * mm / s**5
       and then Peak >= No_Speed
       and then
         abs (Checked.Bounded_Travel (Accel_Distance, Peak, Profile.Coast, Decel_Distance) - Distance)
         <= 1.0E-9 * Distance
       and then
         abs (Fast_Velocity_At_Max_Time (Profile.Decel, -Max_Crackle, Peak) - End_Vel)
         <= 1.0E-9 * Velocity'Max (Start_Vel, Velocity'Max (Peak, End_Vel)))
   with
     Ghost,
     Pre =>
       Times_In_Range (Profile.Accel)
       and then Times_In_Range (Profile.Decel)
       and then Profile.Coast >= No_Time
       and then Profile.Coast <= Phase_Time_Max
       and then Validity_Inputs_Are_Bounded (Distance, Start_Vel, End_Vel, Max_Crackle)
       and then abs Peak <= Ramp_Velocity_Max
       and then abs Accel_Distance <= Ramp_Distance_Max
       and then abs Decel_Distance <= Ramp_Distance_Max;
   --  The comparisons which Profile_Is_Valid makes on the travel and the final speed, as it computes them in
   --  floating point from the speed at the end of the acceleration ramp and the distances of the two ramps.

   ------------------------------------------------------------------------------------------------------------------
   --  What the two bisections of a phase time find
   --
   --  Where no closed form gives a phase time, the distance solver and the snap-limited case of the speed-change
   --  solver bisect the machine numbers from zero to Search_Time_Cap. The predicates below are the certificate
   --  which each result carries, in floating point: the distance or the final speed of the result, as the end
   --  formulas of Trajectory.Profiles compute it, does not exceed what was asked for, and with the next
   --  machine number as the phase time it does, unless that machine number is the cap. A solve whose answer lies
   --  beyond the cap therefore ends just below it, and falls short.
   ------------------------------------------------------------------------------------------------------------------

   function Next_Time (T : Time) return Time
   is (To_Time (To_Bits (T) + 1))
   with Ghost, Pre => To_Bits (T) < Last_Pattern;
   --  The machine number after T, for a T which is not negative.

   function Smallest_Time return Time
   is (To_Time (1))
   with Ghost;
   --  The machine number after zero.

   function Distance_With
     (Times       : Feedrate_Profile_Times;
      Variable    : Feedrate_Profile_Times_Index;
      T           : Time;
      Max_Crackle : Crackle;
      Start_Vel   : Velocity) return Length
   is (Distance_Formula
         ((if Variable = 1 then T else Times (1)),
          (if Variable = 2 then T else Times (2)),
          (if Variable = 3 then T else Times (3)),
          (if Variable = 4 then T else Times (4)),
          Max_Crackle,
          Start_Vel))
   with Ghost, Pre => Ramp_In_Range (Times, Max_Crackle, Start_Vel) and then T >= No_Time and then T <= Phase_Time_Max;
   --  The distance of the ramp of Times with phase time Variable replaced by T, as Fast_Distance_At_Max_Time
   --  computes it.

   function Velocity_With
     (Times       : Feedrate_Profile_Times;
      Variable    : Feedrate_Profile_Times_Index;
      T           : Time;
      Max_Crackle : Crackle;
      Start_Vel   : Velocity) return Velocity
   is (Velocity_Formula
         ((if Variable = 1 then T else Times (1)),
          (if Variable = 2 then T else Times (2)),
          (if Variable = 3 then T else Times (3)),
          (if Variable = 4 then T else Times (4)),
          Max_Crackle,
          Start_Vel))
   with Ghost, Pre => Ramp_In_Range (Times, Max_Crackle, Start_Vel) and then T >= No_Time and then T <= Phase_Time_Max;
   --  The final speed of the ramp of Times with phase time Variable replaced by T, as Fast_Velocity_At_Max_Time
   --  computes it.

   function Distance_Is_Bisected
     (Times       : Feedrate_Profile_Times;
      Variable    : Feedrate_Profile_Times_Index;
      Start_Vel   : Velocity;
      Distance    : Length;
      Max_Crackle : Crackle) return Boolean
   is (Times (Variable) < Search_Time_Cap
       and then To_Bits (Times (Variable)) < Last_Pattern
       and then
         (Times (Variable) = No_Time or else Fast_Distance_At_Max_Time (Times, Max_Crackle, Start_Vel) <= Distance)
       and then Next_Time (Times (Variable)) <= Search_Time_Cap
       and then
         (Next_Time (Times (Variable)) = Search_Time_Cap
          or else Distance_With (Times, Variable, Next_Time (Times (Variable)), Max_Crackle, Start_Vel) > Distance))
   with Ghost, Pre => Ramp_In_Range (Times, Max_Crackle, Start_Vel);
   --  Phase time Variable of Times is zero or a time below the cap for which the ramp, as
   --  Fast_Distance_At_Max_Time computes it, does not exceed Distance, and with the next machine number in its
   --  place the ramp exceeds Distance, unless that machine number is the cap.

   function Gain_Is_Bisected
     (Times : Feedrate_Profile_Times; Speed_Change : Velocity; Max_Crackle : Crackle) return Boolean
   is (Times (3) = No_Time
       and then Times (4) = No_Time
       and then Times (2) < Search_Time_Cap
       and then (Times (2) = No_Time or else Fast_Velocity_At_Max_Time (Times, Max_Crackle, No_Speed) <= Speed_Change)
       and then
         (if Times (2) > No_Time
          then
            To_Bits (Times (2)) < Last_Pattern
            and then Next_Time (Times (2)) <= Search_Time_Cap
            and then
              (Next_Time (Times (2)) = Search_Time_Cap
               or else Velocity_With (Times, 2, Next_Time (Times (2)), Max_Crackle, No_Speed) > Speed_Change)
          else
            Smallest_Time < Search_Time_Cap
            and then Velocity_With (Times, 2, Smallest_Time, Max_Crackle, No_Speed) > Speed_Change))
   with Ghost, Pre => Ramp_In_Range (Times, Max_Crackle, No_Speed);
   --  The same for the second phase time of a ramp from rest whose third and fourth are zero, and the speed
   --  which it gains: the snap-limited case. Where that phase time is zero the next machine number is the
   --  smallest one, which a profile time cannot name through its own bit pattern.

   ------------------------------------------------------------------------------------------------------------------
   --  What the phase times of a solver mean for the exact ramp
   --
   --  The peaks of the exact ramp which phase times denote are products of the crackle and of three sums of phase
   --  times, the spans: the snap peaks at C * T1, the jerk at that times T1 + T2, and the acceleration at that
   --  times 2 * T1 + T2 + T3 (Trajectory.Profile_Model.Derivatives.Lemma_Limits). The predicates below say,
   --  in exact arithmetic, how the phase times which a solver computes in floating point stand to the limits.
   --
   --  A prepared solver holds the phase times T1 to T3 of the ramp which reaches every limit that its region lets
   --  it reach: the saturated ramp. Snap_Is_Within and Solver_Is_Consistent say that the peaks of the saturated
   --  ramp are within the limits, to Prepared_Tolerance, and that the quotients and roots which the solver keeps
   --  are the ones of that ramp. The snap limit is not kept in a solver, so the first of the two names it. A
   --  solve returns phase times whose spans do not exceed those of the saturated ramp, to Solved_Tolerance
   --  (Spans_Are_Within), which is why its ramp is within the limits too.
   ------------------------------------------------------------------------------------------------------------------

   use type Prunt.Motion_Planner.Reals.Real;

   Prepared_Tolerance : constant := 2.0**(-32);
   --  How far, relatively, a peak of the saturated ramp can be above its limit.

   Peak_Tolerance : constant := 2.0**(-33);
   --  How far, relatively, the acceleration peak of the saturated ramp can be from the acceleration limit.

   Region_Tolerance : constant := 2.0**(-40);
   --  How far, relatively, the values which a solver keeps can be from the spans and peaks of the saturated ramp
   --  which they stand for.

   Snap_Ceiling : constant := 2.0**81;
   --  No saturated ramp of the proved domain has a larger snap peak.

   Solved_Tolerance : constant := 2.0**(-32);
   --  How far, relatively, a span of a solved ramp can be above the span of the saturated ramp, and its change of
   --  speed above the one asked for.

   Gain_Residue : constant := 2.0**(-700);
   --  How far it can be above that besides, in millimetres per second, where the change of speed asked for is so
   --  small that the quotient which gives the phase time is not a normal number.

   Span_Floor : constant := 2.0**(-170);
   --  The first phase time of a saturated ramp is at least this long, in seconds.

   function Snap_Peak (Cm : Crackle; T1 : Time) return Reals.Real
   is (Reals.R (Cm) * Reals.R (T1))
   with Ghost;

   function Jerk_Span (T1, T2 : Time) return Reals.Real
   is (Reals.R (T1) + Reals.R (T2))
   with Ghost;

   function Acceleration_Span (T1, T2, T3 : Time) return Reals.Real
   is (2.0 * Reals.R (T1) + Reals.R (T2) + Reals.R (T3))
   with Ghost;

   function Jerk_Peak (Cm : Crackle; T1, T2 : Time) return Reals.Real
   is (Snap_Peak (Cm, T1) * Jerk_Span (T1, T2))
   with Ghost;

   function Acceleration_Peak (Cm : Crackle; T1, T2, T3 : Time) return Reals.Real
   is (Jerk_Peak (Cm, T1, T2) * Acceleration_Span (T1, T2, T3))
   with Ghost;
   --  The peaks of snap, jerk and acceleration of the exact ramp with crackle Cm and these phase times.

   function Peaks_Are_Within (Cm : Crackle; T1, T2, T3 : Time; Am : Acceleration; Jm : Jerk; Sm : Snap) return Boolean
   is (Rounding.Relative.At_Most (Snap_Peak (Cm, T1), Reals.R (Sm), Prepared_Tolerance)
       and then Rounding.Relative.At_Most (Jerk_Peak (Cm, T1, T2), Reals.R (Jm), Prepared_Tolerance)
       and then Rounding.Relative.At_Most (Acceleration_Peak (Cm, T1, T2, T3), Reals.R (Am), Prepared_Tolerance))
   with Ghost;
   --  The peaks of that ramp are within the limits, to Prepared_Tolerance.

   function Threshold_Is_Consistent (Solver : Delta_V_Solver) return Boolean
   is (Rounding.Relative.About
         (Reals.R (Solver.Snap_Only_Above),
          8.0 * Snap_Peak (Solver.Cm, Solver.T1) * Reals.R (Solver.T1) * Reals.R (Solver.T1) * Reals.R (Solver.T1),
          Region_Tolerance))
   with Ghost;
   --  The speed above which snap is reached is the change of speed of the ramp of four equal phases of the
   --  saturated first phase time.

   function Region_Is_Consistent (Solver : Delta_V_Solver) return Boolean
   is (case Solver.Region is
         when Region_1            =>
           Rounding.Relative.About (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2), Reals.R (Solver.Jm), Region_Tolerance)
           and then
             Rounding.Relative.About
               (Acceleration_Span (Solver.T1, Solver.T2, Solver.T3),
                Reals.R (Solver.Accel_Over_Jerk),
                Region_Tolerance)
           and then
             Rounding.Relative.About
               (Reals.R (Solver.T1) + Jerk_Span (Solver.T1, Solver.T2),
                Reals.R (Solver.Jerk_Over_Snap) + Reals.R (Solver.Snap_Over_Crackle),
                Region_Tolerance),
         when Region_2            =>
           Reals.R (Solver.T3) = 0.0
           and then
             Rounding.Relative.About
               (2.0 * (Reals.R (Solver.T1) + Jerk_Span (Solver.T1, Solver.T2)),
                Reals.R (Solver.Snap_Over_Crackle) + 2.0 * Reals.R (Solver.Snap_Root),
                Region_Tolerance),
         when Region_3 | Region_5 => Reals.R (Solver.T2) = 0.0 and then Reals.R (Solver.T3) = 0.0,
         when Region_4            =>
           Reals.R (Solver.T2) = 0.0
           and then
             Rounding.Relative.About
               (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2), Reals.R (Solver.Jm), Region_Tolerance)
           and then
             Rounding.Relative.About
               (Acceleration_Span (Solver.T1, Solver.T2, Solver.T3),
                Reals.R (Solver.Accel_Over_Jerk),
                Region_Tolerance)
           and then
             Rounding.Relative.About
               (Reals.R (Solver.T1) * Reals.R (Solver.T1), Reals.R (Solver.Jerk_Over_Crackle), Region_Tolerance))
   with Ghost;
   --  The values which a solve combines with a change of speed are the ones of the saturated ramp: in Region_1
   --  and Region_4 that ramp reaches the jerk limit and its last span is the quotient of the acceleration and jerk
   --  limits, and so on region by region.

   function Core_Is_Consistent (Solver : Delta_V_Solver) return Boolean
   is (Reals.R (Solver.T1) >= Span_Floor
       and then Reals.R (Solver.T2) >= 0.0
       and then Reals.R (Solver.T3) >= 0.0
       and then Snap_Peak (Solver.Cm, Solver.T1) <= Snap_Ceiling
       and then
         Rounding.Relative.At_Most
           (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2), Reals.R (Solver.Jm), Prepared_Tolerance)
       and then
         Rounding.Relative.About
           (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3), Reals.R (Solver.Am), Peak_Tolerance)
       and then Region_Is_Consistent (Solver))
   with Ghost;
   --  The saturated ramp of Solver reaches the acceleration limit and is within the jerk limit, and
   --  Region_Is_Consistent.

   function Solver_Is_Consistent (Solver : Delta_V_Solver) return Boolean
   is (Core_Is_Consistent (Solver)
       and then (if Solver.Region in Region_1 | Region_2 then Threshold_Is_Consistent (Solver)))
   with Ghost;
   --  What Prepare_Delta_V establishes of the solver which it returns, for positive limits within the proved
   --  domain, besides Snap_Is_Within. Nothing here names the snap limit.

   function Snap_Is_Within (Solver : Delta_V_Solver; Sm : Snap) return Boolean
   is (Rounding.Relative.At_Most (Snap_Peak (Solver.Cm, Solver.T1), Reals.R (Sm), Prepared_Tolerance))
   with Ghost;
   --  The saturated ramp of Solver is within the snap limit which Solver was prepared with.

   function Spans_Are_Within (Solver : Delta_V_Solver; T1, T2, T3 : Time) return Boolean
   is (Rounding.Relative.At_Most (Reals.R (T1), Reals.R (Solver.T1), Solved_Tolerance)
       and then Rounding.Relative.At_Most (Jerk_Span (T1, T2), Jerk_Span (Solver.T1, Solver.T2), Solved_Tolerance)
       and then
         Rounding.Relative.At_Most
           (Acceleration_Span (T1, T2, T3), Acceleration_Span (Solver.T1, Solver.T2, Solver.T3), Solved_Tolerance))
   with Ghost;
   --  No span of the ramp of T1 to T3 exceeds the span of the saturated ramp of Solver, to Solved_Tolerance.

   function Gain_Is_At_Most (Cm : Crackle; T1, T2, T3, T4 : Time; Speed_Change : Velocity) return Boolean
   is (Exact_Gain (Reals.R (Cm), Reals.R (T1), Reals.R (T2), Reals.R (T3), Reals.R (T4))
       <= Reals.R (Speed_Change) + Solved_Tolerance * Reals.R (Speed_Change) + Gain_Residue)
   with Ghost;
   --  The exact ramp changes speed by no more than Speed_Change, to Solved_Tolerance.

   function Is_Capped (T : Time) return Boolean
   is (T >= No_Time and then To_Bits (T) < Last_Pattern and then Next_Time (T) = Search_Time_Cap)
   with Ghost;
   --  T is the machine number below Search_Time_Cap, where the bisection of a second phase time stops when 24
   --  hours are not enough for the change of speed asked for.

   function Gain_Is_At_Least (Cm : Crackle; T1, T2, T3, T4 : Time; Speed_Change : Velocity) return Boolean
   is (Exact_Gain (Reals.R (Cm), Reals.R (T1), Reals.R (T2), Reals.R (T3), Reals.R (T4))
       >= Reals.R (Speed_Change) - Solved_Tolerance * Reals.R (Speed_Change) - Gain_Residue)
   with Ghost;
   --  The exact ramp changes speed by no less than Speed_Change, to Solved_Tolerance.

   function Gain_Is_Reached (Cm : Crackle; T1, T2, T3, T4 : Time; Speed_Change : Velocity) return Boolean
   is (if not Is_Capped (T2) then Gain_Is_At_Least (Cm, T1, T2, T3, T4, Speed_Change))
   with Ghost;
   --  And so it does unless its second phase time is the one at which the bisection gives up.

   Limit_Tolerance : constant := 2.0**(-29);
   --  Certificates.Certified_Tolerance, under a name which this unit may use in exact arithmetic.

   function Peaks_Are_Limited (T1, T2, T3 : Time; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Snap_Peak (Cm, T1) <= Reals.R (Sm) * (1.0 + Limit_Tolerance)
       and then Jerk_Peak (Cm, T1, T2) <= Reals.R (Jm) * (1.0 + Limit_Tolerance)
       and then Acceleration_Peak (Cm, T1, T2, T3) <= Reals.R (Am) * (1.0 + Limit_Tolerance))
   with Ghost;
   --  The peaks of the exact ramp of phase times T1 to T3 are within the limits, to Limit_Tolerance. The ramp may
   --  run either way and have any last phase time: for both ramps of a profile this is the condition of
   --  Certificates.Validity.Lemma_Limits_At_Every_Instant_Of_Profile.

   Top_Speed_Tolerance : constant := 2.0**(-31);
   Top_Speed_Residue   : constant := 2.0**(-699);
   --  How far the highest speed of an exact profile may be above the highest speed asked for: as a fraction of
   --  it, and beyond that in millimetres per second.

   function Top_Speed_Is_At_Most (T1, T2, T3, T4 : Time; Start_Vel, Top : Velocity; Cm : Crackle) return Boolean
   is (Reals.R (Start_Vel) + Exact_Gain (Reals.R (Cm), Reals.R (T1), Reals.R (T2), Reals.R (T3), Reals.R (T4))
       <= Reals.R (Top) + Top_Speed_Tolerance * Reals.R (Top) + Top_Speed_Residue)
   with Ghost;
   --  The exact acceleration ramp of phase times T1 to T4 from Start_Vel ends at no more than Top, to
   --  Top_Speed_Tolerance. That speed is Certificates.Validity.Exact_Peak, which no instant of the exact profile
   --  exceeds (Certificates.Validity.Lemma_Speed_At_Every_Instant).

   function Solve_Is_Sound (Solver : Delta_V_Solver; Speed_Change : Velocity; T1, T2, T3, T4 : Time) return Boolean
   is (if Solver_Is_Consistent (Solver)
       then
         Spans_Are_Within (Solver, T1, T2, T3)
         and then Gain_Is_At_Most (Solver.Cm, T1, T2, T3, T4, Speed_Change)
         and then Gain_Is_Reached (Solver.Cm, T1, T2, T3, T4, Speed_Change))
   with Ghost;
   --  What a solve with a consistent solver establishes of the phase times which it returns: the ramp stays
   --  within the saturated ramp, and its change of speed is the one asked for, to Solved_Tolerance.

   function Ramp_Is_Within_Limits
     (Times : Feedrate_Profile_Times; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Certificates.Limits_Are_Respected
         (To_Ramp (Times, Cm, No_Velocity), Reals.R (Am), Reals.R (Jm), Reals.R (Sm), Reals.R (Cm)))
   with Ghost;
   --  At every instant of the exact ramp of Times, crackle, snap, jerk and acceleration are within the limits, to
   --  Certificates.Certified_Tolerance (Certificates.Lemma_Limits_At_Every_Instant).

   function Ramp_Is_Sound
     (Times : Feedrate_Profile_Times; Speed_Change : Velocity; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
      return Boolean
   is (Ramp_Is_Within_Limits (Times, Am, Jm, Sm, Cm)
       and then Gain_Is_At_Most (Cm, Times (1), Times (2), Times (3), Times (4), Speed_Change)
       and then Gain_Is_Reached (Cm, Times (1), Times (2), Times (3), Times (4), Speed_Change))
   with Ghost;
   --  What the solvers for a speed change establish of the phase times which they return: the exact ramp is
   --  within the limits at every instant, and it changes speed by what was asked for, to Solved_Tolerance, or by
   --  less where 24 hours of the second phase are not enough.

   function Is_Empty (Profile : Feedrate_Profile) return Boolean
   is (Times_Are_Zero (Profile.Accel) and then Profile.Coast = No_Time and then Times_Are_Zero (Profile.Decel));
   --  Every time of Profile is zero, as in a Full_Profile_Result which was given no profile.

   function Optimal_Profile_For_Distance
     (Start_Vel        : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Feedrate_Profile_Times
   with
     Global => null,
     Pre    => Distance_Request_Is_Provable (Start_Vel, Distance, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max),
     Post   =>
       Times_In_Range (Optimal_Profile_For_Distance'Result)
       and then
         (if Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
            and then Limits_Are_Moderate (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
          then
            Ramp_Is_Within_Limits
              (Optimal_Profile_For_Distance'Result, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max));
   --  Compute the acceleration part of a feedrate profile that has the lowest total time to travel the given distance
   --  without violating any of the given constraints. Note that there is no velocity limit here.
   --
   --  For moderate limits the exact ramp of the result is within the limits at every instant, to
   --  Certificates.Certified_Tolerance.

   function Optimal_Profile_For_Delta_V
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
      return Feedrate_Profile_Times
   with
     Global => null,
     Pre    => Ramp_Request_Is_Provable (Delta_V, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max),
     Post   =>
       Times_In_Range (Optimal_Profile_For_Delta_V'Result)
       and then
         (if Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
          then
            Ramp_Is_Sound
              (Optimal_Profile_For_Delta_V'Result, abs Delta_V, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max));
   --  Compute the acceleration part of a feedrate profile that achieves the given change in velocity in the lowest
   --  time without violating any of the given constraints. Note that there is no distance limit here.
   --
   --  For limits within the proved domain the exact ramp of the result is within the limits at every instant, to
   --  Certificates.Certified_Tolerance, and changes speed by abs Delta_V, to Solved_Tolerance: never by more,
   --  and by less only where its second phase time is the longest which the bisection considers.

   function Optimal_Full_Profile
     (Start_Vel        : Velocity;
      Max_Vel          : Velocity;
      End_Vel          : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Feedrate_Profile
   with
     Side_Effects,
     Exceptional_Cases => (Constraint_Error => True),
     Global            => null,
     Pre               =>
       Full_Profile_Request_Is_Provable
         (Start_Vel, Max_Vel, End_Vel, Distance, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max),
     Post              =>
       Times_In_Range (Optimal_Full_Profile'Result.Accel)
       and then Times_In_Range (Optimal_Full_Profile'Result.Decel)
       and then Profile_Is_Valid (Optimal_Full_Profile'Result, Distance, Start_Vel, End_Vel, Crackle_Max);
   --  Compute the feedrate profile with the minimal time without violating the given constraints. Raises
   --  Constraint_Error if there is no legal feedrate profile which can meet the given constraints, specifically
   --  regarding End_Vel being reachable. Also raises Constraint_Error if Start_Vel or End_Vel are higher than
   --  Max_Vel.

   function Direct_Ramp_Distance
     (Start_Vel, End_Vel : Velocity;
      Acceleration_Max   : Acceleration;
      Jerk_Max           : Jerk;
      Snap_Max           : Snap;
      Crackle_Max        : Crackle) return Length
   with
     Ghost,
     Global => null,
     Pre    =>
       Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
       and then Start_Vel >= No_Speed
       and then Start_Vel <= Speed_Max
       and then End_Vel >= No_Speed
       and then End_Vel <= Speed_Max;
   --  The distance, as Fast_Distance_At_Max_Time computes it, of the ramp which the speed-change solver gives
   --  for going straight from Start_Vel to End_Vel. No profile between those speeds is shorter, so
   --  Try_Optimal_Full_Profile reports a distance below it as unreachable. Its expression is at the end of this
   --  specification, after the functions which it calls.

   function Try_Optimal_Full_Profile
     (Start_Vel        : Velocity;
      Max_Vel          : Velocity;
      End_Vel          : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Full_Profile_Result
   with
     Side_Effects,
     Exceptional_Cases => (Constraint_Error => True),
     Global            => null,
     Pre               =>
       Full_Profile_Request_Is_Provable
         (Start_Vel, Max_Vel, End_Vel, Distance, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max),
     Post              =>
       Derivative_Limits_Are_Valid (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
       and then
         (if Try_Optimal_Full_Profile'Result.Reachable
          then
            Times_In_Range (Try_Optimal_Full_Profile'Result.Profile.Accel)
            and then Times_In_Range (Try_Optimal_Full_Profile'Result.Profile.Decel)
            and then
              Profile_Is_Valid (Try_Optimal_Full_Profile'Result.Profile, Distance, Start_Vel, End_Vel, Crackle_Max)
            and then
              Top_Speed_Is_At_Most
                (Try_Optimal_Full_Profile'Result.Profile.Accel (1),
                 Try_Optimal_Full_Profile'Result.Profile.Accel (2),
                 Try_Optimal_Full_Profile'Result.Profile.Accel (3),
                 Try_Optimal_Full_Profile'Result.Profile.Accel (4),
                 Start_Vel,
                 Max_Vel,
                 Crackle_Max)
            and then
              (if Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
               then
                 Peaks_Are_Limited
                   (Try_Optimal_Full_Profile'Result.Profile.Accel (1),
                    Try_Optimal_Full_Profile'Result.Profile.Accel (2),
                    Try_Optimal_Full_Profile'Result.Profile.Accel (3),
                    Acceleration_Max,
                    Jerk_Max,
                    Snap_Max,
                    Crackle_Max)
                 and then
                   Peaks_Are_Limited
                     (Try_Optimal_Full_Profile'Result.Profile.Decel (1),
                      Try_Optimal_Full_Profile'Result.Profile.Decel (2),
                      Try_Optimal_Full_Profile'Result.Profile.Decel (3),
                      Acceleration_Max,
                      Jerk_Max,
                      Snap_Max,
                      Crackle_Max))
          else
            Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
            and then Start_Vel >= No_Speed
            and then End_Vel >= No_Speed
            and then
              Distance < Direct_Ramp_Distance (Start_Vel, End_Vel, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max));
   --  As Optimal_Full_Profile, except that when Distance is too short to reach End_Vel from Start_Vel the result has
   --  Reachable set to False and an empty profile instead of Constraint_Error being raised. That outcome is ordinary
   --  for a caller trying candidate limits, and propagating an exception costs far more than returning. Every other
   --  error still raises Constraint_Error.
   --
   --  Unreachable is only reported with positive limits, and means that Distance is less than the direct ramp
   --  from Start_Vel to End_Vel covers.
   --
   --  The exact profile of a reachable result never moves faster than Max_Vel, to Top_Speed_Tolerance, and with
   --  positive limits within the proved domain both of its ramps are within the limits at every instant, to
   --  Limit_Tolerance.

   function Profile_Is_Valid
     (Profile : Feedrate_Profile; Distance : Length; Start_Vel, End_Vel : Velocity; Max_Crackle : Crackle)
      return Boolean
   with
     Global => null,
     Pre    =>
       Times_In_Range (Profile.Accel)
       and then Times_In_Range (Profile.Decel)
       and then Validity_Inputs_Are_Bounded (Distance, Start_Vel, End_Vel, Max_Crackle),
     Post   =>
       (if Profile_Is_Valid'Result then Profile.Coast >= No_Time and then Max_Crackle >= 0.0 * mm / s**5)
       and then
         (if Is_Empty (Profile)
            and then Distance = No_Distance
            and then Start_Vel >= No_Speed
            and then End_Vel = Start_Vel
            and then Max_Crackle >= 0.0 * mm / s**5
          then Profile_Is_Valid'Result)
       and then
         (if Profile_Is_Valid'Result and then Profile.Coast <= Phase_Time_Max
          then
            Passes_Validity_Checks
              (Profile,
               Distance,
               Start_Vel,
               End_Vel,
               Max_Crackle,
               Fast_Velocity_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel),
               Fast_Distance_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel),
               Fast_Distance_At_Max_Time
                 (Profile.Decel, -Max_Crackle, Fast_Velocity_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel))));
   --  Reject nonfinite or negative times, empty motion, and profiles that miss their distance or endpoint velocity.
   --  The empty profile is valid for no distance at an unchanged speed.

   function Profile_Is_Valid
     (Profile                        : Feedrate_Profile;
      Distance                       : Length;
      Start_Vel, End_Vel             : Velocity;
      Max_Crackle                    : Crackle;
      Peak                           : Velocity;
      Accel_Distance, Decel_Distance : Length) return Boolean
   with
     Global => null,
     Pre    =>
       Times_Are_Bounded (Profile.Accel)
       and then Times_Are_Bounded (Profile.Decel)
       and then Validity_Inputs_Are_Bounded (Distance, Start_Vel, End_Vel, Max_Crackle)
       and then abs Peak <= Ramp_Velocity_Max
       and then abs Accel_Distance <= Ramp_Distance_Max
       and then abs Decel_Distance <= Ramp_Distance_Max,
     Post   =>
       (if Profile_Is_Valid'Result
        then
          Times_In_Range (Profile.Accel)
          and then Times_In_Range (Profile.Decel)
          and then Profile.Coast >= No_Time
          and then Max_Crackle >= 0.0 * mm / s**5)
       and then
         (if Is_Empty (Profile)
            and then Distance = No_Distance
            and then Start_Vel >= No_Speed
            and then End_Vel = Start_Vel
            and then Max_Crackle >= 0.0 * mm / s**5
            and then Peak = Start_Vel
            and then Accel_Distance = No_Distance
            and then Decel_Distance = No_Distance
          then Profile_Is_Valid'Result)
       and then
         (if Profile_Is_Valid'Result and then Profile.Coast <= Phase_Time_Max
          then
            Passes_Validity_Checks
              (Profile, Distance, Start_Vel, End_Vel, Max_Crackle, Peak, Accel_Distance, Decel_Distance));
   --  Reuse the checked ramp evaluations when the caller already needed them to calculate coast time.

   function Optimal_Profile_For_Distance_Internal
     (Start_Vel        : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Internal_Profile_Result
   with
     Global => null,
     Pre    => Distance_Request_Is_Provable (Start_Vel, Distance, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max),
     Post   =>
       Times_In_Range (Optimal_Profile_For_Distance_Internal'Result.Profile)
       and then Optimal_Profile_For_Distance_Internal'Result.Index in 1 .. 4
       and then
         (if Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
          then
            Distance_Is_Bisected
              (Optimal_Profile_For_Distance_Internal'Result.Profile,
               Feedrate_Profile_Times_Index (Optimal_Profile_For_Distance_Internal'Result.Index),
               Start_Vel,
               Distance,
               Crackle_Max))
       and then
         (if Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
            and then Limits_Are_Moderate (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
          then
            Ramp_Is_Within_Limits
              (Optimal_Profile_For_Distance_Internal'Result.Profile,
               Acceleration_Max,
               Jerk_Max,
               Snap_Max,
               Crackle_Max));
   --  Solve the minimum-time zero-endpoint-velocity profile for Distance and retain its active constraint region.
   --  With positive limits the phase time which Index names is the one bisected, and carries its certificate.

   function Optimal_Profile_For_Delta_V_Internal
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
      return Internal_Profile_Result
   with
     Global => null,
     Pre    => Ramp_Request_Is_Provable (Delta_V, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max),
     Post   =>
       Times_In_Range (Optimal_Profile_For_Delta_V_Internal'Result.Profile)
       and then Optimal_Profile_For_Delta_V_Internal'Result.Index in 1 .. 4
       and then
         (if Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
          then
            Ramp_Is_Sound
              (Optimal_Profile_For_Delta_V_Internal'Result.Profile,
               abs Delta_V,
               Acceleration_Max,
               Jerk_Max,
               Snap_Max,
               Crackle_Max));
   --  Solve the minimum-time derivative-limited transition for Delta_V and retain its active constraint region.

   function Prepare_Delta_V
     (Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle) return Delta_V_Solver
   with
     Global => null,
     Pre    => Limits_Are_Provable (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max),
     Post   =>
       Prepare_Delta_V'Result.Limits_Valid
       = Derivative_Limits_Are_Valid (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
       and then
         (if Prepare_Delta_V'Result.Limits_Valid
          then
            Prepare_Delta_V'Result.Zero_Limit = Any_Limit_Is_Zero (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
            and then
              (if not Prepare_Delta_V'Result.Zero_Limit
               then
                 Solver_In_Domain (Prepare_Delta_V'Result)
                 and then Prepare_Delta_V'Result.Am = Acceleration_Max
                 and then Prepare_Delta_V'Result.Jm = Jerk_Max
                 and then Prepare_Delta_V'Result.Cm = Crackle_Max
                 and then Solver_Is_Consistent (Prepare_Delta_V'Result)
                 and then Snap_Is_Within (Prepare_Delta_V'Result, Snap_Max)));
   --  Never raises: invalid limits and arithmetic failures are recorded for the solves that use the result. For
   --  positive limits within the proved domain the saturated ramp of the result is within the limits, and its
   --  kept values are consistent with it.

   function Optimal_Profile_For_Delta_V (Solver : Delta_V_Solver; Delta_V : Velocity) return Feedrate_Profile_Times
   with
     Global => null,
     Pre    => Solver_Is_Ready (Solver, Delta_V),
     Post   =>
       Times_In_Range (Optimal_Profile_For_Delta_V'Result)
       and then
         (if Solver_In_Domain (Solver)
          then
            Solve_Is_Sound
              (Solver,
               abs Delta_V,
               Optimal_Profile_For_Delta_V'Result (1),
               Optimal_Profile_For_Delta_V'Result (2),
               Optimal_Profile_For_Delta_V'Result (3),
               Optimal_Profile_For_Delta_V'Result (4)));
   --  Identical to Optimal_Profile_For_Delta_V for the limits that Solver was prepared with. With a Solver from
   --  Prepare_Delta_V, which is consistent and within its snap limit, Accuracy.Lemma_Prepared_Is_Within_Limits
   --  turns the postcondition into the one of that function.

   function Optimal_Profile_For_Delta_V_Internal
     (Solver : Delta_V_Solver; Delta_V : Velocity) return Internal_Profile_Result
   with
     Global => null,
     Pre    => Solver_Is_Ready (Solver, Delta_V),
     Post   =>
       Times_In_Range (Optimal_Profile_For_Delta_V_Internal'Result.Profile)
       and then Optimal_Profile_For_Delta_V_Internal'Result.Index in 1 .. 4
       and then
         (if Optimal_Profile_For_Delta_V_Internal'Result.Index = 2
          then
            Solver_In_Domain (Solver)
            and then Gain_Is_Bisected (Optimal_Profile_For_Delta_V_Internal'Result.Profile, abs Delta_V, Solver.Cm))
       and then
         (if Solver_In_Domain (Solver)
          then
            Solve_Is_Sound
              (Solver,
               abs Delta_V,
               Optimal_Profile_For_Delta_V_Internal'Result.Profile (1),
               Optimal_Profile_For_Delta_V_Internal'Result.Profile (2),
               Optimal_Profile_For_Delta_V_Internal'Result.Profile (3),
               Optimal_Profile_For_Delta_V_Internal'Result.Profile (4)));
   --  An Index of two is the snap-limited case, whose second phase time is bisected and carries its certificate.
   --  Whatever the case, the ramp stays within the saturated ramp of a consistent Solver and gains no more speed
   --  than was asked for.

   function Direct_Ramp_Distance
     (Start_Vel, End_Vel : Velocity;
      Acceleration_Max   : Acceleration;
      Jerk_Max           : Jerk;
      Snap_Max           : Snap;
      Crackle_Max        : Crackle) return Length
   is (Fast_Distance_At_Max_Time
         (Optimal_Profile_For_Delta_V
            (Prepare_Delta_V (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max), Start_Vel - End_Vel),
          (if Start_Vel < End_Vel then Crackle_Max / (mm / s**5) else -Crackle_Max / (mm / s**5)) * (mm / s**5),
          Start_Vel));
   --  As Try_Optimal_Full_Profile writes it.

end Prunt.Motion_Planner.Trajectory.Solvers;
