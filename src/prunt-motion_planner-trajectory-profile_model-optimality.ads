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

--  Which ramp of the 15-stage family is the fastest, as exact mathematics.
--
--  The solvers of Trajectory.Solvers build a ramp greedily: the first phase time is as long as the limits
--  allow, a later phase time is nonzero only once the limit below it is reached, and the last nonzero phase time
--  is what the change of speed or the distance asked for requires. Is_Saturating below describes those ramps by
--  their defining equations, and the theorems say that no ramp of the family within the same limits reaches the
--  same change of speed, or covers the same distance from the same starting speed, in less time.
--
--  The proofs rest on a change of variables. A ramp with phase times T1 to T4 has the four spans
--
--     a = T1,   b = T1 + T2,   c = 2 T1 + T2 + T3,   d = 4 T1 + 2 T2 + T3 + T4,
--
--  in terms of which its peaks are C a, C a b and C a b c, its change of speed is C a b c d and its duration is
--  a + b + c + d, and the phase times are nonnegative exactly when b >= a, c >= a + b and d >= a + b + c. The
--  theorems then follow from one lemma about four ordered numbers, proved by taking the spans away one at a time
--  from the last.
--
--  These are theorems about the family: the ramps of fifteen stages of constant crackle in the pattern of
--  Feedrate_Profile_Times. They do not say that no other motion with bounded crackle is faster. They are about
--  exact phase times, not about the floating-point phase times which the solvers return, and they do not say that
--  a saturating ramp exists for every change of speed.

package Prunt.Motion_Planner.Trajectory.Profile_Model.Optimality
  with SPARK_Mode => On, Ghost, Always_Terminates
is

   ------------------------------------------------------------------------------------------------------------------
   --  The change of variables
   ------------------------------------------------------------------------------------------------------------------

   function Snap_Span (R : Ramp) return Real
   is (R.T1);
   --  a: the peak of snap is the crackle acting for this long.

   function Jerk_Span (R : Ramp) return Real
   is (R.T1 + R.T2);
   --  b: the peak of jerk is the peak of snap acting for this long.

   function Acceleration_Span (R : Ramp) return Real
   is (2.0 * R.T1 + R.T2 + R.T3);
   --  c: the peak of acceleration is the peak of jerk acting for this long.

   function Velocity_Span (R : Ramp) return Real
   is (4.0 * R.T1 + 2.0 * R.T2 + R.T3 + R.T4);
   --  d: the change of speed is the peak of acceleration acting for this long.

   procedure Lemma_Spans (R : Ramp)
   with
     Post =>
       Peak_Snap (R) = R.C * Snap_Span (R)
       and then Peak_Jerk (R) = R.C * Snap_Span (R) * Jerk_Span (R)
       and then Peak_Accel (R) = R.C * Snap_Span (R) * Jerk_Span (R) * Acceleration_Span (R)
       and then Velocity_Gain (R) = R.C * Snap_Span (R) * Jerk_Span (R) * Acceleration_Span (R) * Velocity_Span (R)
       and then Total_Time (R) = Snap_Span (R) + Jerk_Span (R) + Acceleration_Span (R) + Velocity_Span (R);
   --  The peaks, the change of speed and the duration of any ramp in terms of its spans.

   function Spans_Are_Ordered (A, B, C, D : Real) return Boolean
   is (A >= 0.0 and then B >= A and then C >= A + B and then D >= A + B + C);

   procedure Lemma_Ordered_Spans (R : Ramp)
   with
     Post => Is_Ramp (R) = Spans_Are_Ordered (Snap_Span (R), Jerk_Span (R), Acceleration_Span (R), Velocity_Span (R));
   --  The phase times are nonnegative exactly when each span is at least the sum of the spans before it.

   function Ramp_With_Spans (A, B, C, D, Crackle, Start : Real) return Ramp
   is ((T1 => A, T2 => B - A, T3 => C - A - B, T4 => D - A - B - C, C => Crackle, Vs => Start));

   procedure Lemma_Ramp_With_Spans (A, B, C, D, Crackle, Start : Real)
   with
     Post =>
       Snap_Span (Ramp_With_Spans (A, B, C, D, Crackle, Start)) = A
       and then Jerk_Span (Ramp_With_Spans (A, B, C, D, Crackle, Start)) = B
       and then Acceleration_Span (Ramp_With_Spans (A, B, C, D, Crackle, Start)) = C
       and then Velocity_Span (Ramp_With_Spans (A, B, C, D, Crackle, Start)) = D
       and then Is_Ramp (Ramp_With_Spans (A, B, C, D, Crackle, Start)) = Spans_Are_Ordered (A, B, C, D);
   --  Every four numbers are the spans of one ramp, so the family is the set of ordered spans.

   ------------------------------------------------------------------------------------------------------------------
   --  One ramp against another
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_No_Faster_Ramp (Star, Other : Ramp)
   with
     Pre  =>
       Is_Ramp (Star)
       and then Star.C > 0.0
       and then Star.T1 > 0.0
       and then Is_Ramp (Other)
       and then Other.C >= 0.0
       and then Other.C <= Star.C
       and then (Star.T2 = 0.0 or else Peak_Snap (Other) <= Peak_Snap (Star))
       and then (Star.T3 = 0.0 or else Peak_Jerk (Other) <= Peak_Jerk (Star))
       and then (Star.T4 = 0.0 or else Peak_Accel (Other) <= Peak_Accel (Star))
       and then Velocity_Gain (Other) >= Velocity_Gain (Star),
     Post => Total_Time (Other) >= Total_Time (Star);
   --  The lemma behind the theorems, with no limits in it. Star spends time in its second, third and fourth
   --  phases only where Other has no higher a peak of snap, of jerk and of acceleration, and Other has no more
   --  crackle. Then Other, if it changes the speed by as much, takes at least as long.

   ------------------------------------------------------------------------------------------------------------------
   --  The fastest ramp within limits
   ------------------------------------------------------------------------------------------------------------------

   function Is_Within_Limits (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real) return Boolean
   is (abs R.C <= Crackle_Max
       and then abs Peak_Snap (R) <= Snap_Max
       and then abs Peak_Jerk (R) <= Jerk_Max
       and then abs Peak_Accel (R) <= Acceleration_Max);
   --  The crackle and the peaks which the phase times imply are within the limits. By
   --  Trajectory.Profile_Model.Derivatives.Lemma_Limits, snap, jerk and acceleration are then within them at
   --  every instant, and each peak is the value at a stage boundary.

   function Is_Saturating (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real) return Boolean
   is (Is_Ramp (R)
       and then Is_Within_Limits (R, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then abs R.C = Crackle_Max
       and then (R.T2 = 0.0 or else abs Peak_Snap (R) = Snap_Max)
       and then (R.T3 = 0.0 or else abs Peak_Jerk (R) = Jerk_Max)
       and then (R.T4 = 0.0 or else abs Peak_Accel (R) = Acceleration_Max));
   --  A ramp within the limits, at the crackle limit, in which the second phase is empty unless snap reaches its
   --  limit, the third unless jerk does and the fourth unless acceleration does. These are the equations which
   --  define each case of Optimal_Profile_For_Delta_V_Internal, written T2, T3, T4 for the phase times and S, J, A
   --  for the limit which a peak equals:
   --
   --     no limit reached (V1.1, V2.1, V3.1, V4.1, V5.1)      T2 = 0, T3 = 0, T4 = 0
   --     snap (V1.2, V2.2)                                     S,      T3 = 0, T4 = 0
   --     snap and jerk (V1.3)                                  S,      J,      T4 = 0
   --     snap, jerk and acceleration (V1.4)                    S,      J,      A
   --     snap and acceleration (V2.4)                          S,      T3 = 0, A
   --     acceleration (V3.4, V5.4)                             T2 = 0, T3 = 0, A
   --     jerk (V4.3)                                           T2 = 0, J,      T4 = 0
   --     jerk and acceleration (V4.4)                          T2 = 0, J,      A
   --
   --  and, with the distance in place of the change of speed, of Optimal_Profile_For_Distance_Internal. The
   --  remaining equation of a case, that the ramp reaches the change of speed or the distance asked for, is the
   --  hypothesis of the theorems which compares the two ramps. That each case gives a ramp which is within the
   --  limits is proved from the conditions which select it, in the last two sections below.

   procedure Lemma_Saturating_Is_Fastest (Star, Other : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Saturating (Star, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Is_Ramp (Other)
       and then Is_Within_Limits (Other, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then abs Velocity_Gain (Other) >= abs Velocity_Gain (Star),
     Post => Total_Time (Other) >= Total_Time (Star);
   --  No ramp of the family within the limits changes the speed by as much as a saturating ramp in less time.
   --  Either ramp may speed up or slow down.

   procedure Lemma_Saturating_Times_Agree
     (Star_1, Star_2 : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Saturating (Star_1, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Is_Saturating (Star_2, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then abs Velocity_Gain (Star_1) = abs Velocity_Gain (Star_2),
     Post => Total_Time (Star_1) = Total_Time (Star_2);
   --  The least time for a change of speed is one number: any two saturating ramps for it take equally long.

   procedure Lemma_Saturating_Gains_Most (Star, Other : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Saturating (Star, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Is_Ramp (Other)
       and then Is_Within_Limits (Other, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Total_Time (Other) <= Total_Time (Star),
     Post => abs Velocity_Gain (Other) <= abs Velocity_Gain (Star);
   --  The same, the other way round: for a fixed duration the saturating ramp maximises the change of speed, which
   --  is the product of the spans, within the limits.

   procedure Lemma_Saturating_Is_Fastest_Over_Distance
     (Star, Other : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Saturating (Star, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Star.C >= 0.0
       and then Star.Vs >= 0.0
       and then Is_Ramp (Other)
       and then Is_Within_Limits (Other, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Other.C >= 0.0
       and then Other.Vs = Star.Vs
       and then Travel (Other) >= Travel (Star),
     Post => Total_Time (Other) >= Total_Time (Star);
   --  No accelerating ramp of the family within the limits covers the distance of a saturating one, from the same
   --  starting speed, in less time: the distance is the duration times the mean of the starting and final speeds.

   procedure Lemma_Saturating_Travels_Farthest
     (Star, Other : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Saturating (Star, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Star.C >= 0.0
       and then Star.Vs >= 0.0
       and then Is_Ramp (Other)
       and then Is_Within_Limits (Other, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Other.C >= 0.0
       and then Other.Vs = Star.Vs
       and then Total_Time (Other) <= Total_Time (Star),
     Post => Travel (Other) <= Travel (Star);
   --  The same, the other way round: for a fixed duration the saturating ramp covers the greatest distance.

   ------------------------------------------------------------------------------------------------------------------
   --  The solvers, in exact arithmetic
   --
   --  Each solver first forms, from the limits alone, the ramp of its region in which every limit that the region
   --  can reach is reached: Is_Fully_Saturating below. The lemmas named after the regions say that the equations
   --  and the conditions which define each region give such a ramp. The solver then returns that ramp cut short:
   --  it keeps the phases before one of them, gives that one the time which the change of speed or the distance
   --  requires and leaves the later ones empty, and it selects the phase by comparing what was asked for with what
   --  the ramp cut short at full length would give. Cut_Short is that ramp. A ramp so selected is saturating, and
   --  so, by the theorems above, the fastest of the family.
   ------------------------------------------------------------------------------------------------------------------

   subtype Phase_Index is Integer range 1 .. 4;

   function Phase_Time (R : Ramp; Phase : Phase_Index) return Real
   is (case Phase is
         when 1 => R.T1,
         when 2 => R.T2,
         when 3 => R.T3,
         when 4 => R.T4);

   function Cut_Short (Full : Ramp; Last : Phase_Index; Time : Real) return Ramp
   is (case Last is
         when 1 => (T1 => Time, T2 => 0.0, T3 => 0.0, T4 => 0.0, C => Full.C, Vs => Full.Vs),
         when 2 => (T1 => Full.T1, T2 => Time, T3 => 0.0, T4 => 0.0, C => Full.C, Vs => Full.Vs),
         when 3 => (T1 => Full.T1, T2 => Full.T2, T3 => Time, T4 => 0.0, C => Full.C, Vs => Full.Vs),
         when 4 => (T1 => Full.T1, T2 => Full.T2, T3 => Full.T3, T4 => Time, C => Full.C, Vs => Full.Vs));
   --  The ramp which keeps the phases of Full before Last, lasts Time in phase Last and has no later phase. Case K
   --  of Optimal_Profile_For_Delta_V_Internal, and the result of Optimal_Profile_For_Distance_Internal for index
   --  K, have this form with Last = K, where Full is the ramp of the region. A case which the solvers mark as
   --  impossible is one where the phase of Full is empty.

   function Is_Fully_Saturating (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real) return Boolean
   is (Is_Saturating (R, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then abs Peak_Accel (R) = Acceleration_Max);
   --  A saturating ramp which reaches the acceleration limit, so that it stays saturating however long its fourth
   --  phase.

   procedure Lemma_Cut_Short_Is_Saturating
     (Full : Ramp; Last : Phase_Index; Time : Real; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Is_Fully_Saturating (Full, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Time >= 0.0
       and then (Last = 4 or else Time <= Phase_Time (Full, Last)),
     Post => Is_Saturating (Cut_Short (Full, Last, Time), Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
   --  A fully saturating ramp cut short at any phase, within the time of that phase, is saturating.

   procedure Lemma_Gain_Selects_Phase (Full : Ramp; Last : Phase_Index; Time : Real)
   with
     Pre  =>
       Is_Ramp (Full)
       and then Full.C /= 0.0
       and then Full.T1 > 0.0
       and then Last <= 3
       and then Time >= 0.0
       and then
         abs Velocity_Gain (Cut_Short (Full, Last, Time))
         <= abs Velocity_Gain (Cut_Short (Full, Last, Phase_Time (Full, Last))),
     Post => Time <= Phase_Time (Full, Last);
   --  The change of speed of a ramp cut short grows with the time of its last phase. So where the ramp cut short at
   --  the full length of a phase would change the speed by at least as much as is asked for, which is the test by
   --  which the solver for a change of speed does not go on to the next phase, the time which the change of speed
   --  requires is within the phase.

   procedure Lemma_Travel_Selects_Phase (Full : Ramp; Last : Phase_Index; Time : Real)
   with
     Pre  =>
       Is_Ramp (Full)
       and then Full.C > 0.0
       and then Full.T1 > 0.0
       and then Full.Vs >= 0.0
       and then Last <= 3
       and then Time >= 0.0
       and then Travel (Cut_Short (Full, Last, Time)) <= Travel (Cut_Short (Full, Last, Phase_Time (Full, Last))),
     Post => Time <= Phase_Time (Full, Last);
   --  The same for the distance of an accelerating ramp, which is the test of the solver for a distance.

   procedure Lemma_Delta_V_Case_Is_Fastest
     (Full                                              : Ramp;
      Last                                              : Phase_Index;
      Time                                              : Real;
      Other                                             : Ramp;
      Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Fully_Saturating (Full, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Time >= 0.0
       and then
         (Last = 4
          or else
            abs Velocity_Gain (Cut_Short (Full, Last, Time))
            <= abs Velocity_Gain (Cut_Short (Full, Last, Phase_Time (Full, Last))))
       and then Is_Ramp (Other)
       and then Is_Within_Limits (Other, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then abs Velocity_Gain (Other) >= abs Velocity_Gain (Cut_Short (Full, Last, Time)),
     Post => Total_Time (Other) >= Total_Time (Cut_Short (Full, Last, Time));
   --  What the solver for a change of speed returns, were its arithmetic exact, is the fastest ramp of the family
   --  for that change of speed: the ramp of the region cut short at the phase which the solver selects, with the
   --  time there which the change of speed requires.

   procedure Lemma_Distance_Case_Is_Fastest
     (Full                                              : Ramp;
      Last                                              : Phase_Index;
      Time                                              : Real;
      Other                                             : Ramp;
      Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Fully_Saturating (Full, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Full.C > 0.0
       and then Full.Vs >= 0.0
       and then Time >= 0.0
       and then
         (Last = 4
          or else Travel (Cut_Short (Full, Last, Time)) <= Travel (Cut_Short (Full, Last, Phase_Time (Full, Last))))
       and then Is_Ramp (Other)
       and then Is_Within_Limits (Other, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max)
       and then Other.C >= 0.0
       and then Other.Vs = Full.Vs
       and then Travel (Other) >= Travel (Cut_Short (Full, Last, Time)),
     Post => Total_Time (Other) >= Total_Time (Cut_Short (Full, Last, Time));
   --  What the solver for a distance returns, were its arithmetic exact, is the fastest accelerating ramp of the
   --  family over that distance from that starting speed.

   ------------------------------------------------------------------------------------------------------------------
   --  The ramp of each region
   --
   --  Each lemma takes the equations which the solvers solve for the phase times of a region, and the conditions
   --  by which they select it as the comments beside their tests state them, and concludes that the ramp is fully
   --  saturating. Region_1 is given by quotients, so its ramp is written out. The others are given by square and
   --  cube roots, which exact rationals do not have, so their ramps are described by the equations which the
   --  roots solve.
   ------------------------------------------------------------------------------------------------------------------

   function Region_1_Ramp (Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max, T4, Start : Real) return Ramp
   is ((T1 => Snap_Max / Crackle_Max,
        T2 => Jerk_Max / Snap_Max - Snap_Max / Crackle_Max,
        T3 => Acceleration_Max / Jerk_Max - Jerk_Max / Snap_Max - Snap_Max / Crackle_Max,
        T4 => T4,
        C  => Crackle_Max,
        Vs => Start))
   with Pre => Crackle_Max > 0.0 and then Snap_Max > 0.0 and then Jerk_Max > 0.0;
   --  The exact ramp of the closed forms which Region_1 of the solvers evaluates in floating point.

   procedure Lemma_Region_1_Is_Saturating (Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max, T4, Start : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Jerk_Max / Snap_Max - Snap_Max / Crackle_Max >= 0.0
       and then Acceleration_Max / Jerk_Max - Jerk_Max / Snap_Max - Snap_Max / Crackle_Max >= 0.0
       and then T4 >= 0.0,
     Post =>
       Peak_Snap (Region_1_Ramp (Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max, T4, Start)) = Snap_Max
       and then Peak_Jerk (Region_1_Ramp (Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max, T4, Start)) = Jerk_Max
       and then
         Peak_Accel (Region_1_Ramp (Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max, T4, Start)) = Acceleration_Max
       and then
         Is_Fully_Saturating
           (Region_1_Ramp (Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max, T4, Start),
            Crackle_Max,
            Snap_Max,
            Jerk_Max,
            Acceleration_Max);
   --  Where the two differences are not negative, which is what selects Region_1, the closed forms give a ramp
   --  whose peaks are exactly the three limits.

   procedure Lemma_Region_2_Is_Saturating (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Ramp (R)
       and then R.C = Crackle_Max
       and then Peak_Snap (R) = Snap_Max
       and then R.T3 = 0.0
       and then Peak_Accel (R) = Acceleration_Max
       and then Acceleration_Max <= Jerk_Max * (Jerk_Max / Snap_Max + Snap_Max / Crackle_Max),
     Post => Is_Fully_Saturating (R, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
   --  Region_2: snap and acceleration reach their limits with no third phase, where the acceleration limit is
   --  below the one which Region_1 needs. Jerk then stays within its limit.

   procedure Lemma_Region_3_Is_Saturating (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Ramp (R)
       and then R.C = Crackle_Max
       and then R.T2 = 0.0
       and then R.T3 = 0.0
       and then Peak_Accel (R) = Acceleration_Max
       and then Snap_Max * Snap_Max <= Jerk_Max * Crackle_Max
       and then Acceleration_Max * Crackle_Max * Crackle_Max <= 2.0 * Snap_Max * Snap_Max * Snap_Max,
     Post => Is_Fully_Saturating (R, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
   --  Region_3: acceleration reaches its limit with no second or third phase, where the acceleration limit is
   --  below the one which Region_2 needs. Snap and jerk then stay within their limits.

   procedure Lemma_Region_4_Is_Saturating (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Ramp (R)
       and then R.C = Crackle_Max
       and then R.T2 = 0.0
       and then Peak_Jerk (R) = Jerk_Max
       and then Peak_Accel (R) = Acceleration_Max
       and then Jerk_Max * Crackle_Max <= Snap_Max * Snap_Max,
     Post => Is_Fully_Saturating (R, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
   --  Region_4: jerk and acceleration reach their limits with no second phase, where the snap limit is too high
   --  to be reached. Snap then stays within its limit.

   procedure Lemma_Region_5_Is_Saturating (R : Ramp; Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max : Real)
   with
     Pre  =>
       Crackle_Max > 0.0
       and then Snap_Max > 0.0
       and then Jerk_Max > 0.0
       and then Acceleration_Max > 0.0
       and then Is_Ramp (R)
       and then R.C = Crackle_Max
       and then R.T2 = 0.0
       and then R.T3 = 0.0
       and then Peak_Accel (R) = Acceleration_Max
       and then Jerk_Max * Crackle_Max <= Snap_Max * Snap_Max
       and then Acceleration_Max * Acceleration_Max * Crackle_Max <= 4.0 * Jerk_Max * Jerk_Max * Jerk_Max,
     Post => Is_Fully_Saturating (R, Crackle_Max, Snap_Max, Jerk_Max, Acceleration_Max);
   --  Region_5: acceleration reaches its limit with no second or third phase, where the snap limit is too high to
   --  be reached and the acceleration limit is below the one which Region_4 needs. Snap and jerk then stay within
   --  their limits.

end Prunt.Motion_Planner.Trajectory.Profile_Model.Optimality;
