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

with Prunt.Motion_Planner.Trajectory.Elementary; use Prunt.Motion_Planner.Trajectory.Elementary;
with Prunt.Motion_Planner.Trajectory.Searches;
with Prunt.Motion_Planner.Trajectory.Solvers.Accuracy;
with Prunt.Motion_Planner.Trajectory.Solvers.Estimates;

package body Prunt.Motion_Planner.Trajectory.Solvers
  with SPARK_Mode => On
is

   pragma Extensions_Allowed (On);

   ------------------------------------------------------------------------------------------------------------------
   --  Proof support
   --
   --  Everything from here to the first solver is ghost code: bounds of the floating-point expressions which the
   --  solvers evaluate, each stated for the expression exactly as a solver writes it, so that a call of the lemma
   --  beside the statement gives the provers the bound of that very term. The parameters have no dimension, so
   --  that quantities of any dimension can be passed, and the bounds are therefore written as numbers: 2#1.0#E80
   --  and 2#1.0#E-80 are the magnitudes of the proved domain, and 2#1.0#E163 is Solver_Time_Max.
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Quotient (N, D : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => N >= 0.0 and then N <= 2#1.0#E80 and then D >= 2#1.0#E-80,
     Post   => N / D >= 0.0 and then N / D <= 2#1.0#E160;
   --  A speed or a limit over a limit.

   procedure Lemma_Quotient (N, D : Dimensioned_Float) is null;

   procedure Lemma_Positive_Quotient (N, D : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => N >= 2#1.0#E-80 and then N <= 2#1.0#E80 and then D >= 2#1.0#E-80 and then D <= 2#1.0#E80,
     Post   => N / D >= 2#1.0#E-160 and then N / D <= 2#1.0#E160;
   --  A positive limit over a limit.

   procedure Lemma_Positive_Quotient (N, D : Dimensioned_Float) is null;

   procedure Lemma_Less_One (X, A : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => X >= 0.0 and then X <= 2#1.0#E200 and then A >= 0.0 and then A <= 2#1.0#E200,
     Post   => X - A <= X and then X - A >= -2#1.0#E200;
   --  Taking a nonnegative time from a nonnegative one leaves no more than there was.

   procedure Lemma_Less_One (X, A : Dimensioned_Float) is null;

   procedure Lemma_Less_Two (X, A, B : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    =>
       X >= 0.0
       and then X <= 2#1.0#E200
       and then A >= 0.0
       and then A <= 2#1.0#E200
       and then B >= 0.0
       and then B <= 2#1.0#E200,
     Post   => X - A >= -2#1.0#E200 and then X - A <= X and then X - A - B <= X;

   procedure Lemma_Less_Two (X, A, B : Dimensioned_Float) is
   begin
      pragma Assert (X - A >= -2#1.0#E200 and then X - A <= X);
      pragma Assert (X - A - B <= X - A);
   end Lemma_Less_Two;

   procedure Lemma_Less_Three (X, A, B, C : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    =>
       X >= 0.0
       and then X <= 2#1.0#E200
       and then A >= 0.0
       and then A <= 2#1.0#E200
       and then B >= 0.0
       and then B <= 2#1.0#E200
       and then C >= 0.0
       and then C <= 2#1.0#E200,
     Post   =>
       X - A >= -2#1.0#E200
       and then X - A <= X
       and then X - A - B >= -2#1.0#E201
       and then X - A - B <= X
       and then X - A - B - C <= X;

   procedure Lemma_Less_Three (X, A, B, C : Dimensioned_Float) is
   begin
      pragma Assert (X - A >= -2#1.0#E200 and then X - A <= X);
      pragma Assert (X - A - B >= -2#1.0#E201 and then X - A - B <= X - A);
      pragma Assert (X - A - B - C <= X - A - B);
   end Lemma_Less_Three;

   procedure Lemma_Root_At_Most (X, Square, Root : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    =>
       X >= 0.0 and then X <= Square and then Root >= 0.0 and then Reals.R (Square) <= Reals.R (Root) * Reals.R (Root),
     Post   => Dimensionless_Math.Sqrt (X) >= 0.0 and then Dimensionless_Math.Sqrt (X) <= Root;
   --  The square root of a number which is at most the square of Root is at most Root.

   procedure Lemma_Root_At_Most (X, Square, Root : Dimensioned_Float) is
   begin
      pragma Assert (Reals.R (X) <= Reals.R (Square));
      Lemma_Square_Root_Is_Faithful (X, 0.0, Root);
   end Lemma_Root_At_Most;

   procedure Lemma_Snap_Root_Argument (A, S, C : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    =>
       A >= 2#1.0#E-80
       and then A <= 2#1.0#E80
       and then S >= 2#1.0#E-80
       and then S <= 2#1.0#E80
       and then C >= 2#1.0#E-80
       and then C <= 2#1.0#E80,
     Post   =>
       S**2 >= 0.0
       and then S**2 <= 2#1.0#E160
       and then 0.25 * S**2 >= 0.0
       and then 0.25 * S**2 <= 2#1.0#E158
       and then C**2 >= 2#1.0#E-160
       and then 0.25 * S**2 / C**2 >= 0.0
       and then 0.25 * S**2 / C**2 <= 2#1.0#E318
       and then A / S >= 0.0
       and then A / S <= 2#1.0#E160
       and then 0.25 * S**2 / C**2 + A / S >= 0.0
       and then 0.25 * S**2 / C**2 + A / S <= 2#1.0#E319;
   --  The argument of the square root of the snap-limited regions.

   procedure Lemma_Snap_Root_Argument (A, S, C : Dimensioned_Float) is
   begin
      pragma Assert (S**2 >= 0.0 and then S**2 <= 2#1.0#E160);
      pragma Assert (0.25 * S**2 >= 0.0 and then 0.25 * S**2 <= 2#1.0#E158);
      pragma Assert (C**2 >= 2#1.0#E-160);
      pragma Assert (0.25 * S**2 / C**2 >= 0.0 and then 0.25 * S**2 / C**2 <= 2#1.0#E318);
      Lemma_Quotient (A, S);
   end Lemma_Snap_Root_Argument;

   procedure Lemma_Scaled_Quotient (S, C : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => S >= 0.0 and then S <= 2#1.0#E80 and then C >= 2#1.0#E-80,
     Post   =>
       1.5 * S >= 0.0 and then 1.5 * S <= 2#1.0#E81 and then 1.5 * S / C >= 0.0 and then 1.5 * S / C <= 2#1.0#E161;

   procedure Lemma_Scaled_Quotient (S, C : Dimensioned_Float) is
   begin
      pragma Assert (1.5 * S >= 0.0 and then 1.5 * S <= 2#1.0#E81);
      pragma Assert (1.5 * S / C >= 0.0);
   end Lemma_Scaled_Quotient;

   procedure Lemma_Cube_Root_At_Most (X : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => X >= 0.0 and then X <= 2#1.0#E160,
     Post   =>
       X / 1.0 = X
       and then Cube_Root (X / 1.0) >= 0.0
       and then Cube_Root (X / 1.0) <= 2#1.0#E55
       and then Cube_Root (X / 1.0) * 1.0 >= 0.0
       and then Cube_Root (X / 1.0) * 1.0 <= 2#1.0#E55;
   --  The time of the crackle-only regions, with the unit conversions around the root that the solvers write.

   procedure Lemma_Cube_Root_At_Most (X : Dimensioned_Float) is
   begin
      pragma Assert (X / 1.0 = X);
      Lemma_Cube_Root_Is_Close (X / 1.0, 0.0, 2#1.0#E54);
   end Lemma_Cube_Root_At_Most;

   procedure Lemma_Crackle_Only_Argument (A, C : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => A >= 0.0 and then A <= 2#1.0#E80 and then C >= 2#1.0#E-80 and then C <= 2#1.0#E80,
     Post   =>
       0.5 * A >= 0.0
       and then 0.5 * A <= 2#1.0#E80
       and then 0.5 * A / C >= 0.0
       and then 0.5 * A / C <= 2#1.0#E160
       and then 2.0 * C >= 2#1.0#E-80
       and then 2.0 * C <= 2#1.0#E81
       and then A / (2.0 * C) >= 0.0
       and then A / (2.0 * C) <= 2#1.0#E160;
   --  The two ways in which the solvers write the argument of the cube root.

   procedure Lemma_Crackle_Only_Argument (A, C : Dimensioned_Float) is
   begin
      pragma Assert (0.5 * A >= 0.0 and then 0.5 * A <= 2#1.0#E80);
      Lemma_Quotient (0.5 * A, C);
      pragma Assert (2.0 * C >= 2#1.0#E-80 and then 2.0 * C <= 2#1.0#E81);
      Lemma_Quotient (A, 2.0 * C);
   end Lemma_Crackle_Only_Argument;

   procedure Lemma_Threshold_Quotient (N, D : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => N >= 0.0 and then N <= 2#1.0#E323 and then D >= 2#1.0#E-240,
     Post   => N / D >= 0.0 and then N / D <= 2#1.0#E563;

   procedure Lemma_Threshold_Quotient (N, D : Dimensioned_Float) is null;

   procedure Lemma_Snap_Only_Threshold (S, C : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => S >= 2#1.0#E-80 and then S <= 2#1.0#E80 and then C >= 2#1.0#E-80 and then C <= 2#1.0#E80,
     Post   =>
       S**2 >= 0.0
       and then S**2 <= 2#1.0#E160
       and then (S**2)**2 >= 0.0
       and then (S**2)**2 <= 2#1.0#E320
       and then 8.0 * (S**2)**2 >= 0.0
       and then 8.0 * (S**2)**2 <= 2#1.0#E323
       and then C**3 >= 2#1.0#E-240
       and then 8.0 * (S**2)**2 / C**3 >= 0.0
       and then 8.0 * (S**2)**2 / C**3 <= 2#1.0#E563;

   procedure Lemma_Snap_Only_Threshold (S, C : Dimensioned_Float) is
   begin
      pragma Assert (S**2 >= 0.0 and then S**2 <= 2#1.0#E160);
      pragma Assert ((S**2)**2 >= 0.0 and then (S**2)**2 <= 2#1.0#E320);
      pragma Assert (8.0 * (S**2)**2 >= 0.0 and then 8.0 * (S**2)**2 <= 2#1.0#E323);
      pragma Assert (C**3 >= 2#1.0#E-240);
      Lemma_Threshold_Quotient (8.0 * (S**2)**2, C**3);
   end Lemma_Snap_Only_Threshold;

   procedure Lemma_Jerk_Limited_Time (V, J, A, B : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    =>
       V >= 0.0
       and then V <= 2#1.0#E80
       and then J >= 2#1.0#E-80
       and then A >= 0.0
       and then A <= 2#1.0#E163
       and then B >= 0.0
       and then B <= 2#1.0#E163,
     Post   =>
       A + B >= 0.0
       and then A + B <= 2#1.0#E164
       and then (A + B)**2 >= 0.0
       and then (A + B)**2 <= 2#1.0#E328
       and then 4.0 * V >= 0.0
       and then 4.0 * V <= 2#1.0#E82
       and then 4.0 * V / J >= 0.0
       and then 4.0 * V / J <= 2#1.0#E162
       and then (A + B)**2 + 4.0 * V / J >= 0.0
       and then (A + B)**2 + 4.0 * V / J <= 2#1.0#E329
       and then Dimensionless_Math.Sqrt ((A + B)**2 + 4.0 * V / J) >= 0.0
       and then Dimensionless_Math.Sqrt ((A + B)**2 + 4.0 * V / J) <= 2#1.0#E165
       and then 0.5 * Dimensionless_Math.Sqrt ((A + B)**2 + 4.0 * V / J) >= 0.0
       and then 0.5 * Dimensionless_Math.Sqrt ((A + B)**2 + 4.0 * V / J) <= 2#1.0#E164
       and then 1.5 * (A + B) >= 0.0
       and then 1.5 * (A + B) <= 2#1.0#E165
       and then
         0.5 * Dimensionless_Math.Sqrt ((A + B)**2 + 4.0 * V / J) - 1.5 * (A + B)
         <= 0.5 * Dimensionless_Math.Sqrt ((A + B)**2 + 4.0 * V / J);
   --  The third phase time where jerk is the highest limit reached in Region_1.

   procedure Lemma_Jerk_Limited_Time (V, J, A, B : Dimensioned_Float) is
   begin
      pragma Assert (A + B >= 0.0 and then A + B <= 2#1.0#E164);
      pragma Assert ((A + B)**2 >= 0.0 and then (A + B)**2 <= 2#1.0#E328);
      pragma Assert (4.0 * V >= 0.0 and then 4.0 * V <= 2#1.0#E82);
      pragma Assert (4.0 * V / J >= 0.0 and then 4.0 * V / J <= 2#1.0#E162);
      pragma Assert ((A + B)**2 + 4.0 * V / J >= 0.0 and then (A + B)**2 + 4.0 * V / J <= 2#1.0#E329);
      Lemma_Root_At_Most ((A + B)**2 + 4.0 * V / J, 2#1.0#E330, 2#1.0#E165);
      pragma
        Assert
          (0.5 * Dimensionless_Math.Sqrt ((A + B)**2 + 4.0 * V / J) >= 0.0
           and then 0.5 * Dimensionless_Math.Sqrt ((A + B)**2 + 4.0 * V / J) <= 2#1.0#E164);
      pragma Assert (1.5 * (A + B) >= 0.0 and then 1.5 * (A + B) <= 2#1.0#E165);
      Lemma_Less_One (0.5 * Dimensionless_Math.Sqrt ((A + B)**2 + 4.0 * V / J), 1.5 * (A + B));
   end Lemma_Jerk_Limited_Time;

   procedure Lemma_Jerk_Only_Time (V, J, Q : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => V >= 0.0 and then V <= 2#1.0#E80 and then J >= 2#1.0#E-80 and then Q >= 0.0 and then Q <= 2#1.0#E160,
     Post   =>
       V / J >= 0.0
       and then V / J <= 2#1.0#E160
       and then Q + V / J >= 0.0
       and then Q + V / J <= 2#1.0#E161
       and then Dimensionless_Math.Sqrt (Q + V / J) >= 0.0
       and then Dimensionless_Math.Sqrt (Q + V / J) <= 2#1.0#E81;
   --  The root of the third phase time where jerk is the highest limit reached in Region_4.

   procedure Lemma_Jerk_Only_Time (V, J, Q : Dimensioned_Float) is
   begin
      Lemma_Quotient (V, J);
      pragma Assert (Q + V / J >= 0.0 and then Q + V / J <= 2#1.0#E161);
      Lemma_Root_At_Most (Q + V / J, 2#1.0#E162, 2#1.0#E81);
   end Lemma_Jerk_Only_Time;

   procedure Lemma_Crackle_Limited_Time (V, C : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => V >= 0.0 and then V <= 2#1.0#E80 and then C >= 2#1.0#E-80,
     Post   =>
       0.125 * V >= 0.0
       and then 0.125 * V <= 2#1.0#E80
       and then 0.125 * V / C >= 0.0
       and then 0.125 * V / C <= 2#1.0#E160
       and then Dimensionless_Math.Sqrt (0.125 * V / C) >= 0.0
       and then Dimensionless_Math.Sqrt (0.125 * V / C) <= 2#1.0#E80
       and then Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * V / C)) >= 0.0
       and then Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * V / C)) <= 2#1.0#E40;
   --  The only phase time where no limit other than crackle is reached.

   procedure Lemma_Crackle_Limited_Time (V, C : Dimensioned_Float) is
   begin
      pragma Assert (0.125 * V >= 0.0 and then 0.125 * V <= 2#1.0#E80);
      Lemma_Quotient (0.125 * V, C);
      Lemma_Root_At_Most (0.125 * V / C, 2#1.0#E160, 2#1.0#E80);
      Lemma_Root_At_Most (Dimensionless_Math.Sqrt (0.125 * V / C), 2#1.0#E80, 2#1.0#E40);
   end Lemma_Crackle_Limited_Time;

   procedure Lemma_Twice (X : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => X >= 0.0 and then X <= 2#1.0#E163,
     Post   => 2.0 * X >= 0.0 and then 2.0 * X <= 2#1.0#E164;
   --  This and the next two bound the small multiples of a prepared time which the solves subtract.

   procedure Lemma_Twice (X : Dimensioned_Float) is null;

   procedure Lemma_Thrice (X : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => X >= 0.0 and then X <= 2#1.0#E163,
     Post   => 3.0 * X >= 0.0 and then 3.0 * X <= 2#1.0#E165;

   procedure Lemma_Thrice (X : Dimensioned_Float) is null;

   procedure Lemma_Unit_Speed (V : Dimensioned_Float)
   with Ghost, Global => null, Always_Terminates, Post => V / 1.0 = V and then V * 1.0 = V and then V / 1.0 * 1.0 = V;
   --  A speed as a number of millimetres per second, and back.

   procedure Lemma_Unit_Speed (V : Dimensioned_Float) is null;

   procedure Lemma_Four_Times (X : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => X >= 0.0 and then X <= 2#1.0#E163,
     Post   => 4.0 * X >= 0.0 and then 4.0 * X <= 2#1.0#E165;

   procedure Lemma_Four_Times (X : Dimensioned_Float) is null;

   --  The lemmas below state, for expressions of Optimal_Profile_For_Delta_V_Internal as it writes them, exactly
   --  what is asked of the phase times which they give, so that the checks which follow a call find the fact they
   --  need as it stands and need not derive it from the arithmetic.

   function Crackle_Limited_Time_Fits (Vd : Velocity; Cm : Crackle) return Boolean
   is (Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)) <= Phase_Time_Max)
   with Ghost, Pre => Vd >= No_Speed and then Vd <= Speed_Max and then Cm >= Crackle_Limit_Min;
   --  The only phase time where no limit other than crackle is reached is within the proved domain of Profiles.
   --  Crackle_Limited takes this as a precondition, under a name so that its callers need not look at the roots.

   procedure Lemma_Crackle_Limited_In_Range (Vd : Velocity; Cm : Crackle)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    =>
       Vd >= No_Speed and then Vd <= Speed_Max and then Cm >= Crackle_Limit_Min and then Cm <= Crackle_Limit_Max,
     Post   =>
       Crackle_Limited_Time_Fits (Vd, Cm)
       and then
         Accuracy.Crackle_Time_Fact (Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)), Vd, Cm);
   --  And what the phase time means in exact arithmetic, for the conclusion of the case.

   procedure Lemma_Crackle_Limited_In_Range (Vd : Velocity; Cm : Crackle) is
   begin
      Lemma_Crackle_Limited_Time (Vd, Cm);
      Accuracy.Lemma_Crackle_Time (Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)), Vd, Cm);
   end Lemma_Crackle_Limited_In_Range;

   procedure Lemma_Jerk_Limited_In_Range (Solver : Delta_V_Solver; Vd : Velocity)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => Solver_In_Domain (Solver) and then Vd >= No_Speed and then Vd <= Speed_Max,
     Post   =>
       0.5 * Dimensionless_Math.Sqrt ((Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle)**2 + 4.0 * Vd / Solver.Jm)
       - 1.5 * (Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle)
       <= Phase_Time_Max
       and then
         (if 0.5
            * Dimensionless_Math.Sqrt ((Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle)**2 + 4.0 * Vd / Solver.Jm)
            - 1.5 * (Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle)
            > 0.0 * s
          then
            0.5
            * Dimensionless_Math.Sqrt ((Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle)**2 + 4.0 * Vd / Solver.Jm)
            - 1.5 * (Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle)
            >= No_Time);
   --  The third phase time of Region_1 where jerk is the highest limit reached.

   procedure Lemma_Jerk_Limited_In_Range (Solver : Delta_V_Solver; Vd : Velocity) is
   begin
      Lemma_Jerk_Limited_Time (Vd, Solver.Jm, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle);
   end Lemma_Jerk_Limited_In_Range;

   procedure Lemma_Jerk_Only_In_Range (Solver : Delta_V_Solver; Vd : Velocity)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => Solver_In_Domain (Solver) and then Vd >= No_Speed and then Vd <= Speed_Max,
     Post   =>
       Dimensionless_Math.Sqrt (Solver.Jerk_Over_Crackle + Vd / Solver.Jm) - 3.0 * Solver.T1 <= Phase_Time_Max
       and then
         (if Dimensionless_Math.Sqrt (Solver.Jerk_Over_Crackle + Vd / Solver.Jm) - 3.0 * Solver.T1 > 0.0 * s
          then Dimensionless_Math.Sqrt (Solver.Jerk_Over_Crackle + Vd / Solver.Jm) - 3.0 * Solver.T1 >= No_Time);
   --  The third phase time of Region_4 where jerk is the highest limit reached.

   procedure Lemma_Jerk_Only_In_Range (Solver : Delta_V_Solver; Vd : Velocity) is
   begin
      Lemma_Jerk_Only_Time (Vd, Solver.Jm, Solver.Jerk_Over_Crackle);
      Lemma_Thrice (Solver.T1);
      Lemma_Less_One (Dimensionless_Math.Sqrt (Solver.Jerk_Over_Crackle + Vd / Solver.Jm), 3.0 * Solver.T1);
   end Lemma_Jerk_Only_In_Range;

   function Same (X, Y : Dimensioned_Float) return Boolean
   is (X = Y)
   with Ghost;
   --  Equality of two values whatever their dimensions, such as a time and the number of seconds in it.

   procedure Lemma_No_Travel (Accel_Distance, Peak, Coast, Decel_Distance : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Post   =>
       (if Accel_Distance = 0.0 and then Coast = 0.0 and then Decel_Distance = 0.0
        then
          Peak * Coast = 0.0
          and then Accel_Distance + Peak * Coast = 0.0
          and then Accel_Distance + Peak * Coast + Decel_Distance = 0.0);
   --  This and the next two evaluate the checks of Profile_Is_Valid for a profile in which nothing moves. Each
   --  states its case as a condition, because it is called where that case is only one of those possible.

   procedure Lemma_No_Travel (Accel_Distance, Peak, Coast, Decel_Distance : Dimensioned_Float) is null;

   procedure Lemma_No_Distance_Error (Travel, Distance : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Post   => (if Travel = 0.0 and then Distance = 0.0 then abs (Travel - Distance) <= 1.0E-9 * Distance);

   procedure Lemma_No_Distance_Error (Travel, Distance : Dimensioned_Float) is null;

   procedure Lemma_No_Speed_Error (Finish, End_Speed, Start_Speed, Peak : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Post   =>
       (if Start_Speed >= 0.0
          and then Start_Speed <= 2#1.0#E80
          and then Finish = Start_Speed
          and then End_Speed = Start_Speed
          and then Peak = Start_Speed
        then
          abs (Finish - End_Speed)
          <= 1.0E-9 * Dimensioned_Float'Max (Start_Speed, Dimensioned_Float'Max (Peak, End_Speed)));

   procedure Lemma_No_Speed_Error (Finish, End_Speed, Start_Speed, Peak : Dimensioned_Float) is null;

   ------------------------------------------------------------------------------------------------------------------
   --  The solvers
   ------------------------------------------------------------------------------------------------------------------

   function Profile_Is_Valid
     (Profile : Feedrate_Profile; Distance : Length; Start_Vel, End_Vel : Velocity; Max_Crackle : Crackle)
      return Boolean
   is
      Peak : Velocity;
   begin
      Peak := Fast_Velocity_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel);
      return
        Profile_Is_Valid
          (Profile,
           Distance,
           Start_Vel,
           End_Vel,
           Max_Crackle,
           Peak,
           Fast_Distance_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel),
           Fast_Distance_At_Max_Time (Profile.Decel, -Max_Crackle, Peak));
   exception
      when Constraint_Error =>
         pragma
           Warnings
             (GNATprove,
              Off,
              "this statement is never reached",
              Reason =>
                "Within the proved domain neither ramp evaluation overflows, so the handler only serves "
                & "arguments outside it");
         return False;
   end Profile_Is_Valid;
   pragma Warnings (GNATprove, On, "this statement is never reached");

   function Profile_Is_Valid
     (Profile                        : Feedrate_Profile;
      Distance                       : Length;
      Start_Vel, End_Vel             : Velocity;
      Max_Crackle                    : Crackle;
      Peak                           : Velocity;
      Accel_Distance, Decel_Distance : Length) return Boolean
   is
      Motion_Time : Time;
      Finish      : Velocity;
      Travel      : Length;

      Nothing_Moves : constant Boolean :=
        Is_Empty (Profile)
        and then Distance = No_Distance
        and then Start_Vel >= No_Speed
        and then End_Vel = Start_Vel
        and then Max_Crackle >= 0.0 * mm / s**5
        and then Peak = Start_Vel
        and then Accel_Distance = No_Distance
        and then Decel_Distance = No_Distance
      with Ghost;
      --  The case which the postcondition states is valid. The assertions below follow it through the checks.
   begin
      pragma
        Assert
          (if Nothing_Moves
           then
             (for all T of Profile.Accel => T >= 0.0 * s and then T <= Time'Last)
             and then (for all T of Profile.Decel => T >= 0.0 * s and then T <= Time'Last));
      pragma
        Assert
          (if Nothing_Moves
           then
             Distance >= 0.0 * mm
             and then Distance <= Length'Last
             and then Start_Vel >= 0.0 * mm / s
             and then Start_Vel <= Velocity'Last
             and then End_Vel >= 0.0 * mm / s
             and then End_Vel <= Velocity'Last
             and then Max_Crackle >= 0.0 * mm / s**5
             and then Max_Crackle <= Crackle'Last
             and then Profile.Coast >= 0.0 * s
             and then Profile.Coast <= Time'Last);
      if not (Distance >= 0.0 * mm
              and then Distance <= Length'Last
              and then Start_Vel >= 0.0 * mm / s
              and then Start_Vel <= Velocity'Last
              and then End_Vel >= 0.0 * mm / s
              and then End_Vel <= Velocity'Last
              and then Max_Crackle >= 0.0 * mm / s**5
              and then Max_Crackle <= Crackle'Last
              and then Profile.Coast >= 0.0 * s
              and then Profile.Coast <= Time'Last)
        or else (for some T of Profile.Accel => not (T >= 0.0 * s and then T <= Time'Last))
        or else (for some T of Profile.Decel => not (T >= 0.0 * s and then T <= Time'Last))
      then
         return False;
      end if;
      pragma Assert (Times_In_Range (Profile.Accel) and then Times_In_Range (Profile.Decel));
      Checked.Total_Time (Profile, Motion_Time);
      pragma
        Assert
          (if Nothing_Moves
           then Motion_Time >= 0.0 * s and then Motion_Time <= Time'Last and then not (Distance > 0.0 * mm));
      if not (Motion_Time >= 0.0 * s and then Motion_Time <= Time'Last)
        or else (Distance > 0.0 * mm and then Motion_Time = 0.0 * s)
      then
         return False;
      end if;
      Finish := Fast_Velocity_At_Max_Time (Profile.Decel, -Max_Crackle, Peak);
      pragma Assert (if Nothing_Moves then Finish = Start_Vel);
      Checked.Travel (Accel_Distance, Peak, Profile.Coast, Decel_Distance, Travel);
      Lemma_No_Travel (Accel_Distance, Peak, Profile.Coast, Decel_Distance);
      pragma Assert (if Nothing_Moves then Travel = No_Distance);
      Lemma_No_Distance_Error (Travel, Distance);
      Lemma_No_Speed_Error (Finish, End_Vel, Start_Vel, Peak);
      pragma
        Assert
          (if Nothing_Moves
           then
             Peak >= 0.0 * mm / s
             and then Peak <= Velocity'Last
             and then Finish >= -Velocity'Last
             and then Finish <= Velocity'Last
             and then Travel >= 0.0 * mm
             and then Travel <= Length'Last
             and then (Distance = 0.0 * mm or else Travel > 0.0 * mm)
             and then abs (Travel - Distance) <= 1.0E-9 * Distance
             and then abs (Finish - End_Vel) <= 1.0E-9 * Velocity'Max (Start_Vel, Velocity'Max (Peak, End_Vel)));
      --  The analytic ramp solutions and endpoint evaluation round independently. Use relative tolerances so tiny
      --  motion cannot disappear beneath an absolute tolerance. A positive distance must actually be traversed.
      return
        Peak >= 0.0 * mm / s
        and then Peak <= Velocity'Last
        and then Finish >= -Velocity'Last
        and then Finish <= Velocity'Last
        and then Travel >= 0.0 * mm
        and then Travel <= Length'Last
        and then (Distance = 0.0 * mm or else Travel > 0.0 * mm)
        and then abs (Travel - Distance) <= 1.0E-9 * Distance
        and then abs (Finish - End_Vel) <= 1.0E-9 * Velocity'Max (Start_Vel, Velocity'Max (Peak, End_Vel));
   exception
      when Constraint_Error =>
         pragma Assert (not Nothing_Moves);
         return False;
   end Profile_Is_Valid;

   procedure Lemma_Moderate (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => Limits_In_Domain (Am, Jm, Sm, Cm),
     Post   =>
       (if Limits_Are_Moderate (Am, Jm, Sm, Cm)
        then Am / Jm <= Moderate_Time_Max and then Jm / Sm <= Moderate_Time_Max and then Sm / Cm <= Moderate_Time_Max);
   --  What Limits_Are_Moderate says, for the distance solver, which hides it.

   procedure Lemma_Moderate (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) is null;

   --  The quotients of the limits which the region tests of the distance solver combine, as functions: the solver
   --  hides their expressions and states what the lemmas of Accuracy say about them without an operation. A
   --  lemma gives the expressions back at the points where the code computes with them.

   function Quotient_JS (Jm : Jerk; Sm : Snap) return Time
   is (Jm / Sm)
   with
     Ghost,
     Pre =>
       Jm >= Jerk_Limit_Min and then Jm <= Jerk_Limit_Max and then Sm >= Snap_Limit_Min and then Sm <= Snap_Limit_Max;

   function Quotient_SC (Sm : Snap; Cm : Crackle) return Time
   is (Sm / Cm)
   with
     Ghost,
     Pre =>
       Sm >= Snap_Limit_Min
       and then Sm <= Snap_Limit_Max
       and then Cm >= Crackle_Limit_Min
       and then Cm <= Crackle_Limit_Max;

   function Quotient_AJ (Am : Acceleration; Jm : Jerk) return Time
   is (Am / Jm)
   with
     Ghost,
     Pre =>
       Am >= Acceleration_Limit_Min
       and then Am <= Acceleration_Limit_Max
       and then Jm >= Jerk_Limit_Min
       and then Jm <= Jerk_Limit_Max;

   function Quotient_JC (Jm : Jerk; Cm : Crackle) return Time_Squared
   is (Jm / Cm)
   with
     Ghost,
     Pre =>
       Jm >= Jerk_Limit_Min
       and then Jm <= Jerk_Limit_Max
       and then Cm >= Crackle_Limit_Min
       and then Cm <= Crackle_Limit_Max;

   function Root_JC (Jm : Jerk; Cm : Crackle) return Time
   is (Dimensionless_Math.Sqrt (Jm / Cm))
   with
     Ghost,
     Pre =>
       Jm >= Jerk_Limit_Min
       and then Jm <= Jerk_Limit_Max
       and then Cm >= Crackle_Limit_Min
       and then Cm <= Crackle_Limit_Max;

   procedure Lemma_Snap_Quotients (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => Limits_In_Domain (Am, Jm, Sm, Cm),
     Post   =>
       Quotient_JS (Jm, Sm) = Jm / Sm
       and then Quotient_SC (Sm, Cm) = Sm / Cm
       and then Quotient_AJ (Am, Jm) = Am / Jm
       and then Quotient_JS (Jm, Sm) >= Time (2#1.0#E-160)
       and then Quotient_SC (Sm, Cm) >= Time (2#1.0#E-160)
       and then Quotient_AJ (Am, Jm) >= Time (2#1.0#E-160);

   procedure Lemma_Snap_Quotients (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) is
   begin
      Lemma_Positive_Quotient (Jm, Sm);
      Lemma_Positive_Quotient (Sm, Cm);
      Lemma_Positive_Quotient (Am, Jm);
   end Lemma_Snap_Quotients;

   procedure Lemma_Jerk_Quotients (Am : Acceleration; Jm : Jerk; Cm : Crackle)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    =>
       Am >= Acceleration_Limit_Min
       and then Am <= Acceleration_Limit_Max
       and then Jm >= Jerk_Limit_Min
       and then Jm <= Jerk_Limit_Max
       and then Cm >= Crackle_Limit_Min
       and then Cm <= Crackle_Limit_Max,
     Post   =>
       Quotient_AJ (Am, Jm) = Am / Jm
       and then Quotient_JC (Jm, Cm) = Jm / Cm
       and then Root_JC (Jm, Cm) = Dimensionless_Math.Sqrt (Jm / Cm)
       and then Root_JC (Jm, Cm) = Dimensionless_Math.Sqrt (Quotient_JC (Jm, Cm))
       and then Quotient_AJ (Am, Jm) >= Time (2#1.0#E-160)
       and then Quotient_JC (Jm, Cm) >= Time_Squared (2#1.0#E-160);

   procedure Lemma_Jerk_Quotients (Am : Acceleration; Jm : Jerk; Cm : Crackle) is
   begin
      Lemma_Positive_Quotient (Am, Jm);
      Lemma_Positive_Quotient (Jm, Cm);
   end Lemma_Jerk_Quotients;

   function Optimal_Profile_For_Distance_Internal
     (Start_Vel        : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Internal_Profile_Result
   is
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Quotient_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Gap_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Gap_Fact_2);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Scaled_Gap_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Twice_Gap_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Below);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Below_Sum);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Below_Scaled);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Below_Twice);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Snap_Root_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Square_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Cube_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Prepared_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Limits_Are_Moderate);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Ramp_Is_Within_Limits);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Quotient_JS);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Quotient_SC);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Quotient_AJ);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Quotient_JC);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Root_JC);
      --  As in Prepare_Delta_V, what the lemmas of Accuracy say of each computed value is passed on from lemma
      --  to lemma, and nothing here needs the expressions.

      D      : constant Length := Distance;
      Vs     : constant Velocity := Start_Vel;
      Am     : constant Acceleration := Acceleration_Max;
      Jm     : constant Jerk := Jerk_Max;
      Sm     : constant Snap := Snap_Max;
      Cm     : constant Crackle := Crackle_Max;
      Cases  : array (Feedrate_Profile_Times_Index) of Feedrate_Profile_Times;
      Region : Constraint_Region;
      T2, T3 : Time;

      Moderate : constant Boolean := Limits_In_Domain (Am, Jm, Sm, Cm) and then Limits_Are_Moderate (Am, Jm, Sm, Cm)
      with Ghost;
      --  The limits for which the result is proved to respect them.

      Saturated : Delta_V_Solver
      with Ghost;
      --  The saturated ramp of the limits and the quotients and roots which it was formed from, as
      --  Prepare_Delta_V would return them: the lemmas about a prepared solver then apply to it. It is set once
      --  the region is known.

      Snap_Root : Time := 0.0 * s
      with Ghost;
      --  The square root which the test for Region_2 uses.

      function Cases_In_Range return Boolean
      is (Times_In_Range (Cases (1))
          and then Times_In_Range (Cases (2))
          and then Times_In_Range (Cases (3))
          and then Times_In_Range (Cases (4)))
      with Ghost;
      --  Every candidate may be evaluated. This is all that the search below needs of the region analysis.

      function Tracked return Boolean
      is (Accuracy.Prepared_Fact (Saturated, Am, Jm, Sm, Cm)
          and then Accuracy.Is_Candidate (Saturated, Cases (4), 4)
          and then Accuracy.Is_Candidate (Saturated, Cases (3), 3)
          and then Accuracy.Is_Candidate (Saturated, Cases (2), 2)
          and then Accuracy.Is_Candidate (Saturated, Cases (1), 1)
          and then (if Moderate then Accuracy.Is_Moderate (Saturated)))
      with Ghost;
      --  The candidates are cuts of the saturated ramp, whose peaks are within the limits.

      function Solve_Distance_At_Time
        (Profile : Feedrate_Profile_Times; Variable : Feedrate_Profile_Times_Index) return Internal_Profile_Result
      with
        Pre  => Times_In_Range (Profile) and then abs Cm <= Crackle_Magnitude_Max and then abs Vs <= Ramp_Velocity_Max,
        Post =>
          Times_In_Range (Solve_Distance_At_Time'Result.Profile)
          and then Solve_Distance_At_Time'Result.Index = Integer (Variable)
          and then Distance_Is_Bisected (Solve_Distance_At_Time'Result.Profile, Variable, Vs, D, Cm)
          and then Accuracy.Is_Solved (Profile, Solve_Distance_At_Time'Result.Profile, Variable);
      --  Solve one profile time variable so the profile covers the requested distance.

      function Solve_Distance_At_Time
        (Profile : Feedrate_Profile_Times; Variable : Feedrate_Profile_Times_Index) return Internal_Profile_Result
      is
         Result : Feedrate_Profile_Times := Profile;

         Lower : Time := 0.0 * s;
         Upper : Time := 86_400.0 * s;
         --  A maximum of 24 hours should be more than enough.

      begin
         --  The midpoint of the bit patterns of two nonnegative machine numbers is the pattern of a machine number
         --  between them, so this bisects the machine numbers themselves. See Machine_Numbers.
         Lemma_Zero_Pattern;
         Lemma_Positive_Pattern (Upper);
         Lemma_Order (Lower, Upper);
         loop
            pragma Loop_Invariant (Lower >= 0.0 * s and then Lower < Upper and then Upper <= 86_400.0 * s);
            pragma Loop_Invariant (To_Bits (Lower) < To_Bits (Upper) and then To_Bits (Upper) <= Last_Pattern);
            pragma Loop_Invariant (Times_In_Range (Result));
            pragma
              Loop_Invariant
                (for all J in Feedrate_Profile_Times_Index => (if J /= Variable then Result (J) = Profile (J)));
            pragma Loop_Invariant (Lower = 0.0 * s or else Distance_With (Result, Variable, Lower, Cm, Vs) <= D);
            pragma Loop_Invariant (Upper = 86_400.0 * s or else Distance_With (Result, Variable, Upper, Cm, Vs) > D);
            --  The certificate, which does not depend on the phase time being searched: the ramp with the lower
            --  end in its place was found to fit, and the ramp with the upper end in its place not to.
            pragma Loop_Variant (Decreases => To_Bits (Upper) - To_Bits (Lower));
            pragma
              Assert
                (To_Bits (Lower) + (To_Bits (Upper) - To_Bits (Lower)) / 2 >= To_Bits (Lower)
                 and then To_Bits (Lower) + (To_Bits (Upper) - To_Bits (Lower)) / 2 < To_Bits (Upper));
            Result (Variable) := To_Time (To_Bits (Lower) + (To_Bits (Upper) - To_Bits (Lower)) / 2);
            Lemma_Order (Lower, Result (Variable));
            Lemma_Order (Result (Variable), Upper);
            pragma Assert (Lower <= Result (Variable) and then Result (Variable) < Upper);
            pragma Assert (Times_In_Range (Result));
            pragma Assert (Lower = 0.0 * s or else Distance_With (Result, Variable, Lower, Cm, Vs) <= D);
            pragma Assert (Upper = 86_400.0 * s or else Distance_With (Result, Variable, Upper, Cm, Vs) > D);
            exit when Lower = Result (Variable) or else Upper = Result (Variable);
            pragma
              Assert
                (Fast_Distance_At_Max_Time (Result, Cm, Vs)
                 = Distance_With (Result, Variable, Result (Variable), Cm, Vs));
            if Fast_Distance_At_Max_Time (Result, Cm, Vs) <= D then
               Lower := Result (Variable);
            else
               Upper := Result (Variable);
            end if;
         end loop;

         --  The search ends with the lower end as the phase time and the upper end as the next machine number.
         pragma Assert (To_Bits (Result (Variable)) = To_Bits (Lower));
         pragma Assert (To_Bits (Upper) = To_Bits (Result (Variable)) + 1);
         Lemma_Order (Next_Time (Result (Variable)), Upper);
         pragma Assert (Next_Time (Result (Variable)) = Upper);
         pragma
           Assert
             (Upper = 86_400.0 * s
              or else Distance_With (Result, Variable, Next_Time (Result (Variable)), Cm, Vs) > D);
         pragma
           Assert
             (Fast_Distance_At_Max_Time (Result, Cm, Vs)
              = Distance_With (Result, Variable, Result (Variable), Cm, Vs));
         pragma Assert (Result (Variable) = 0.0 * s or else Fast_Distance_At_Max_Time (Result, Cm, Vs) <= D);
         pragma Assert (Distance_Is_Bisected (Result, Variable, Vs, D, Cm));
         pragma Assert (Result (Variable) >= 0.0 * s and then Result (Variable) <= Search_Time_Cap);
         pragma Assert (Accuracy.Is_Solved (Profile, Result, Variable));
         return (Result, Region, Integer (Variable));
      end Solve_Distance_At_Time;

      JS        : constant Time := (if Limits_In_Domain (Am, Jm, Sm, Cm) then Quotient_JS (Jm, Sm) else 0.0 * s)
      with Ghost;
      SC        : constant Time := (if Limits_In_Domain (Am, Jm, Sm, Cm) then Quotient_SC (Sm, Cm) else 0.0 * s)
      with Ghost;
      AJ        : constant Time := (if Limits_In_Domain (Am, Jm, Sm, Cm) then Quotient_AJ (Am, Jm) else 0.0 * s)
      with Ghost;
      JC        : constant Time_Squared :=
        (if Limits_In_Domain (Am, Jm, Sm, Cm) then Quotient_JC (Jm, Cm) else 0.0 * s**2)
      with Ghost;
      Jerk_Root : constant Time := (if Limits_In_Domain (Am, Jm, Sm, Cm) then Root_JC (Jm, Cm) else 0.0 * s)
      with Ghost;
      --  The quotients of the limits and the root which the region tests combine, under names whose expressions
      --  are hidden here. What the lemmas of Accuracy say of them is then stated without an operation, and a
      --  constant keeps its value across the cuts below. Lemma_Snap_Quotients and Lemma_Jerk_Quotients say what
      --  they are, where the code computes with the same quotients.

      function Quotients return Boolean
      is (Accuracy.Quotient_Fact (JS, Jm, Sm)
          and then Accuracy.Quotient_Fact (SC, Sm, Cm)
          and then Accuracy.Quotient_Fact (AJ, Am, Jm))
      with Ghost;
      --  What Accuracy.Lemma_Quotient says of the three quotients of successive limits.

      procedure Set_Saturated (Region : Constraint_Region; T1, T2, T3, Root : Time; Jerk_Over_Crackle : Time_Squared)
      with Ghost;
      --  Set Saturated for a region, its phase times and the roots which the region formed.

      procedure Set_Saturated (Region : Constraint_Region; T1, T2, T3, Root : Time; Jerk_Over_Crackle : Time_Squared)
      is
      begin
         Saturated :=
           (Limits_Valid      => True,
            Zero_Limit        => False,
            Unclassifiable    => False,
            Threshold_Raises  => False,
            Region            => Region,
            Am                => Am,
            Jm                => Jm,
            Cm                => Cm,
            T1                => T1,
            T2                => T2,
            T3                => T3,
            Accel_Over_Jerk   => AJ,
            Jerk_Over_Snap    => JS,
            Snap_Over_Crackle => SC,
            Snap_Root         => Root,
            Jerk_Over_Crackle => Jerk_Over_Crackle,
            Snap_Only_Above   => 0.0 * mm / s);
      end Set_Saturated;

   begin
      if not (D >= 0.0 * mm and then D <= Length'Last and then Vs >= 0.0 * mm / s and then Vs <= Velocity'Last)
        or else not Derivative_Limits_Are_Valid (Am, Jm, Sm, Cm)
      then
         raise Constraint_Error with "Invalid distance-profile inputs.";
      elsif Am = 0.0 * mm / s**2 or else Jm = 0.0 * mm / s**3 or else Sm = 0.0 * mm / s**4 or else Cm = 0.0 * mm / s**5
      then
         if D = 0.0 * mm then
            return ([others => 0.0 * s], Region_5, 1);
         elsif Vs > 0.0 * mm / s then
            Lemma_Positive_Quotient (D, Vs);
            declare
               Coast_Time : constant Time := D / Vs;
            begin
               if not (Coast_Time > 0.0 * s and then Coast_Time <= Time'Last) then
                  raise Constraint_Error with "Unrepresentable constant-speed duration.";
               end if;
               return ([0.0 * s, 0.0 * s, 0.0 * s, Coast_Time], Region_5, 4);
            end;
         else
            raise Constraint_Error with "Positive distance from rest requires positive derivative limits.";
         end if;
      end if;
      --  Each region test is algebraically equivalent to a phase time being nonnegative. Test the computed phase time
      --  itself, since a rearranged test rounds differently and can select a region whose phase time is negative.
      --
      --  The cuts keep the sign and bound of the latest phase time and, once a region is chosen, that its
      --  candidates may be evaluated. They drop how each value was formed, so that no check has more than a few
      --  floating-point operations to consider.
      pragma Assert (Limits_In_Domain (Am, Jm, Sm, Cm));
      pragma
        Assert
          (Accuracy.In_Domain (Am)
           and then Accuracy.In_Domain (Jm)
           and then Accuracy.In_Domain (Sm)
           and then Accuracy.In_Domain (Cm));
      Lemma_Positive_Quotient (Jm, Sm);
      Lemma_Positive_Quotient (Sm, Cm);
      Lemma_Less_One (Jm / Sm, Sm / Cm);
      T2 := Jm / Sm - Sm / Cm;
      Lemma_Moderate (Am, Jm, Sm, Cm);
      Lemma_Snap_Quotients (Am, Jm, Sm, Cm);
      Accuracy.Lemma_Quotient (JS, Jm, Sm);
      Accuracy.Lemma_Quotient (SC, Sm, Cm);
      Accuracy.Lemma_Quotient (AJ, Am, Jm);
      Accuracy.Lemma_Quotient_Is_Operand (JS, Jm, Sm);
      Accuracy.Lemma_Quotient_Is_Operand (SC, Sm, Cm);
      Accuracy.Lemma_Gap (T2, JS, SC);
      pragma
        Assert_And_Cut
          (Limits_In_Domain (Am, Jm, Sm, Cm)
           and then T2 <= Solver_Time_Max
           and then Quotients
           and then Accuracy.Gap_Fact (T2, JS, SC)
           and then (if T2 > 0.0 * s then Accuracy.Below (SC, JS) else Accuracy.Below (JS, SC))
           and then (if Moderate then T2 <= Moderate_Time_Max));
      if T2 > 0.0 * s then
         --  Sm ** 2 < Jm * Cm
         Lemma_Positive_Quotient (Am, Jm);
         Lemma_Positive_Quotient (Jm, Sm);
         Lemma_Positive_Quotient (Sm, Cm);
         Lemma_Less_Two (Am / Jm, Jm / Sm, Sm / Cm);
         T3 := Am / Jm - Jm / Sm - Sm / Cm;
         Lemma_Moderate (Am, Jm, Sm, Cm);
         Lemma_Snap_Quotients (Am, Jm, Sm, Cm);
         Accuracy.Lemma_Quotient_Is_Operand (AJ, Am, Jm);
         Accuracy.Lemma_Quotient_Is_Operand (JS, Jm, Sm);
         Accuracy.Lemma_Quotient_Is_Operand (SC, Sm, Cm);
         Accuracy.Lemma_Gap_2 (T3, AJ, JS, SC);
         pragma
           Assert_And_Cut
             (T2 > 0.0 * s
              and then T2 <= Solver_Time_Max
              and then T3 <= Solver_Time_Max
              and then Quotients
              and then Accuracy.Gap_Fact (T2, JS, SC)
              and then Accuracy.Below (SC, JS)
              and then Accuracy.Gap_Fact_2 (T3, AJ, JS, SC)
              and then (if not (T3 >= 0.0 * s) then Accuracy.Below_Sum (AJ, JS, SC))
              and then (if Moderate then T2 <= Moderate_Time_Max and then T3 <= Moderate_Time_Max));
         if T3 >= 0.0 * s then
            --  Am >= Jm * (Jm / Sm + Sm / Cm)
            Region := Region_1;
            Lemma_Positive_Quotient (Sm, Cm);
            Cases :=
              [
               --  Reachable: Sm, Jm, Am (Test Case D1.4)
               4 => [Sm / Cm, T2, T3, 0.0 * s],
               --  Reachable: Sm, Jm (Test Case D1.3)
               3 => [Sm / Cm, T2, 0.0 * s, 0.0 * s],
               --  Reachable: Sm (Test Case D1.2)
               2 => [Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None (Test Case D1.1)
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]];
            Lemma_Moderate (Am, Jm, Sm, Cm);
            Lemma_Snap_Quotients (Am, Jm, Sm, Cm);
            Set_Saturated (Region_1, SC, T2, T3, 0.0 * s, 0.0 * s**2);
            Accuracy.Lemma_Prepared_1 (Saturated, Am, Jm, Sm, Cm);
            pragma Assert_And_Cut (Cases_In_Range and then Tracked);
         else
            Lemma_Snap_Root_Argument (Am, Sm, Cm);
            Lemma_Root_At_Most (0.25 * Sm**2 / Cm**2 + Am / Sm, 2#1.0#E320, 2#1.0#E160);
            Lemma_Scaled_Quotient (Sm, Cm);
            Lemma_Less_One (Dimensionless_Math.Sqrt (0.25 * Sm**2 / Cm**2 + Am / Sm), 1.5 * Sm / Cm);
            T2 := Dimensionless_Math.Sqrt (0.25 * Sm**2 / Cm**2 + Am / Sm) - 1.5 * Sm / Cm;
            Snap_Root := Dimensionless_Math.Sqrt (0.25 * Sm**2 / Cm**2 + Am / Sm);
            Accuracy.Lemma_Snap_Root (Snap_Root, Am, Sm, Cm);
            Accuracy.Lemma_Snap_Root_Is_Minuend (Snap_Root, Am, Sm, Cm);
            Accuracy.Lemma_Scaled_Gap (T2, Snap_Root, Sm, Cm, SC);
            pragma
              Assert_And_Cut
                (T2 <= Solver_Time_Max
                 and then Quotients
                 and then Accuracy.Below (SC, JS)
                 and then Accuracy.Below_Sum (AJ, JS, SC)
                 and then Accuracy.Snap_Root_Fact (Snap_Root, Am, Sm, Cm)
                 and then Accuracy.Scaled_Gap_Fact (T2, Snap_Root, SC)
                 and then (if not (T2 >= 0.0 * s) then Accuracy.Below_Scaled (Snap_Root, SC)));
            if T2 >= 0.0 * s then
               --  Am >= 2.0 * Sm ** 3 / Cm ** 2
               Region := Region_2;
               Lemma_Positive_Quotient (Sm, Cm);
               Cases :=
                 [
                  --  Reachable: Sm, Am (Test Case D2.4)
                  4 => [Sm / Cm, T2, 0.0 * s, 0.0 * s],
                  --  Impossible case.
                  3 => [Sm / Cm, T2, 0.0 * s, 0.0 * s],
                  --  Reachable: Sm (Test Case D2.2)
                  2 => [Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s],
                  --  Reachable: None (Test Case D2.1)
                  1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]];
               Lemma_Moderate (Am, Jm, Sm, Cm);
               Lemma_Snap_Quotients (Am, Jm, Sm, Cm);
               Set_Saturated (Region_2, SC, T2, 0.0 * s, Snap_Root, 0.0 * s**2);
               Accuracy.Lemma_Prepared_2 (Saturated, Am, Jm, Sm, Cm);
               Accuracy.Lemma_Moderate_2 (Saturated, Am, Jm, Sm, Cm, Moderate);
               pragma Assert_And_Cut (Cases_In_Range and then Tracked);
            else
               Region := Region_3;
               Lemma_Crackle_Only_Argument (Am, Cm);
               Lemma_Cube_Root_At_Most (0.5 * Am / Cm);
               Cases :=
                 [
                  --  Reachable: Am (Test Case D3.4)
                  4 => [Cube_Root ((0.5 * Am / Cm) / s**3) * s, 0.0 * s, 0.0 * s, 0.0 * s],
                  --  Impossible case.
                  3 => [Cube_Root ((0.5 * Am / Cm) / s**3) * s, 0.0 * s, 0.0 * s, 0.0 * s],
                  --  Impossible case.
                  2 => [Cube_Root ((0.5 * Am / Cm) / s**3) * s, 0.0 * s, 0.0 * s, 0.0 * s],
                  --  Reachable: None (Test Case D3.1)
                  1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]];
               Set_Saturated
                 (Region_3, Cube_Root ((0.5 * Am / Cm) / s**3) * s, 0.0 * s, 0.0 * s, Snap_Root, 0.0 * s**2);
               Accuracy.Lemma_Crackle_Root (Saturated.T1, Am, Cm);
               Accuracy.Lemma_Prepared_3 (Saturated, Am, Jm, Sm, Cm);
               pragma Assert_And_Cut (Cases_In_Range and then Tracked);
            end if;
         end if;
      else
         Lemma_Positive_Quotient (Am, Jm);
         Lemma_Positive_Quotient (Jm, Cm);
         Lemma_Root_At_Most (Jm / Cm, 2#1.0#E160, 2#1.0#E80);
         Lemma_Twice (Dimensionless_Math.Sqrt (Jm / Cm));
         Lemma_Less_One (Am / Jm, 2.0 * Dimensionless_Math.Sqrt (Jm / Cm));
         T3 := Am / Jm - 2.0 * Dimensionless_Math.Sqrt (Jm / Cm);
         Lemma_Moderate (Am, Jm, Sm, Cm);
         Lemma_Jerk_Quotients (Am, Jm, Cm);
         Accuracy.Lemma_Quotient (JC, Jm, Cm);
         Accuracy.Lemma_Quotient_Is_Operand (AJ, Am, Jm);
         Accuracy.Lemma_Jerk_Root (Jerk_Root, JC, Jm, Cm);
         Accuracy.Lemma_Twice_Gap (T3, AJ, Jerk_Root);
         pragma
           Assert_And_Cut
             (T3 <= Solver_Time_Max
              and then Quotients
              and then Accuracy.Quotient_Fact (JC, Jm, Cm)
              and then Accuracy.Square_Fact (Jerk_Root, JC)
              and then Accuracy.Below (JS, SC)
              and then Accuracy.Twice_Gap_Fact (T3, AJ, Jerk_Root)
              and then (if not (T3 > 0.0 * s) then Accuracy.Below_Twice (AJ, Jerk_Root))
              and then (if Moderate then T3 <= Moderate_Time_Max));
         if T3 > 0.0 * s then
            --  Am > 2.0 * Jm * Dimensionless_Math.Sqrt (Jm / Cm)
            Region := Region_4;
            Lemma_Positive_Quotient (Jm, Cm);
            Lemma_Root_At_Most (Jm / Cm, 2#1.0#E160, 2#1.0#E80);
            Cases :=
              [
               --  Reachable: Jm, Am (Test Case D4.4)
               4 => [Dimensionless_Math.Sqrt (Jm / Cm), 0.0 * s, T3, 0.0 * s],
               --  Reachable: Jm (Test Case D4.3)
               3 => [Dimensionless_Math.Sqrt (Jm / Cm), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               2 => [Dimensionless_Math.Sqrt (Jm / Cm), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None (Test Case D4.1)
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]];
            Lemma_Jerk_Quotients (Am, Jm, Cm);
            Set_Saturated (Region_4, Jerk_Root, 0.0 * s, T3, 0.0 * s, JC);
            Accuracy.Lemma_Prepared_4 (Saturated, Am, Jm, Sm, Cm, Jerk_Root);
            pragma Assert_And_Cut (Cases_In_Range and then Tracked);
         else
            Region := Region_5;
            Lemma_Crackle_Only_Argument (Am, Cm);
            Lemma_Cube_Root_At_Most (Am / (2.0 * Cm));
            Cases :=
              [
               --  Reachable: Am (Test Case D5.4)
               4 => [Cube_Root ((Am / (2.0 * Cm)) / s**3) * s, 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               3 => [Cube_Root ((Am / (2.0 * Cm)) / s**3) * s, 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               2 => [Cube_Root ((Am / (2.0 * Cm)) / s**3) * s, 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None (Test Case D5.1)
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]];
            Set_Saturated (Region_5, Cube_Root ((Am / (2.0 * Cm)) / s**3) * s, 0.0 * s, 0.0 * s, 0.0 * s, JC);
            Accuracy.Lemma_Crackle_Root_2 (Saturated.T1, Am, Cm);
            Accuracy.Lemma_Prepared_5 (Saturated, Am, Jm, Sm, Cm, Jerk_Root);
            pragma Assert_And_Cut (Cases_In_Range and then Tracked);
         end if;
      end if;
      pragma Assert_And_Cut (Cases_In_Range and then Limits_In_Domain (Am, Jm, Sm, Cm) and then Tracked);

      for I in reverse Cases'Range loop
         pragma Loop_Invariant (Cases_In_Range and then Limits_In_Domain (Am, Jm, Sm, Cm) and then Tracked);
         pragma Loop_Invariant (if I < 4 then not (D > Fast_Distance_At_Max_Time (Cases (I + 1), Cm, Vs)));
         --  The candidate after this one was found long enough. With an invariant each candidate is proved on its
         --  own, with one evaluation of a candidate before it to consider instead of all of them.
         if I = Cases'First or else D > Fast_Distance_At_Max_Time (Cases (I), Cm, Vs) then
            Accuracy.Lemma_Distance_Solve
              (Saturated,
               Cases ((if I < 4 then I + 1 else I)),
               Cases (I),
               Solve_Distance_At_Time (Cases (I), I).Profile,
               I,
               Vs,
               D,
               Am,
               Jm,
               Sm,
               Cm,
               Moderate);
            pragma Assert (Solve_Distance_At_Time (Cases (I), I).Index = Integer (I));
            pragma Assert (Distance_Is_Bisected (Solve_Distance_At_Time (Cases (I), I).Profile, I, Vs, D, Cm));
            return Solve_Distance_At_Time (Cases (I), I);
            --  There are simple analytical solutions for a lot of these, but this is already fast so there is no
            --  reason to optimise it.

         end if;
      end loop;

      raise Program_Error;
   end Optimal_Profile_For_Distance_Internal;

   function Optimal_Profile_For_Distance
     (Start_Vel        : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Feedrate_Profile_Times is
   begin
      return
        Optimal_Profile_For_Distance_Internal (Start_Vel, Distance, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
          .Profile;
   end Optimal_Profile_For_Distance;

   function Prepare_Delta_V
     (Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle) return Delta_V_Solver
   is
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Quotient_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Gap_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Gap_Fact_2);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Scaled_Gap_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Twice_Gap_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Below);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Below_Sum);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Below_Scaled);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Below_Twice);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Snap_Root_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Square_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Cube_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Threshold_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Prepared_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Is_Sound);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Solver_Is_Consistent);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Snap_Is_Within);
      --  The facts which the lemmas of Accuracy give about each computed value are passed on from lemma to
      --  lemma. Nothing below needs what they say, so their expressions are hidden here: the checks of this
      --  function then have nothing more to consider than they had without them.

      Am     : constant Acceleration := Acceleration_Max;
      Jm     : constant Jerk := Jerk_Max;
      Sm     : constant Snap := Snap_Max;
      Cm     : constant Crackle := Crackle_Max;
      Solver : Delta_V_Solver;
      T2, T3 : Time;

      Jerk_Root : Time := 0.0 * s
      with Ghost;
      --  The square root which the test for Region_4 uses, kept for Region_5, which goes on to another first
      --  phase time.

      function Kept return Boolean
      is (Accuracy.Holds_Limits (Solver, Am, Jm, Cm))
      with Ghost;
      --  The solver still holds the limits which it was given.

      function Unset return Boolean
      is (Solver.T2 = 0.0 * s and then Solver.T3 = 0.0 * s)
      with Ghost;
      --  The second and third phase times are still the zeros which a solver starts with.

      function Quotients return Boolean
      is (Kept
          and then Unset
          and then Accuracy.Quotient_Fact (Solver.Jerk_Over_Snap, Jm, Sm)
          and then Accuracy.Quotient_Fact (Solver.Snap_Over_Crackle, Sm, Cm))
      with Ghost;
      --  What every region keeps of the first two quotients.

      function Snap_Regions return Boolean
      is (Quotients and then Accuracy.Below (Solver.Snap_Over_Crackle, Solver.Jerk_Over_Snap))
      with Ghost;
      --  Regions 1 to 3, where the first test found a positive time.

      function Jerk_Regions return Boolean
      is (Quotients
          and then Accuracy.Below (Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
          and then Accuracy.Quotient_Fact (Solver.Accel_Over_Jerk, Am, Jm))
      with Ghost;
      --  Regions 4 and 5, where it did not.
   begin
      Solver.Limits_Valid := Derivative_Limits_Are_Valid (Am, Jm, Sm, Cm);
      if not Solver.Limits_Valid then
         return Solver;
      end if;
      Solver.Am := Am;
      Solver.Jm := Jm;
      Solver.Cm := Cm;
      Solver.Zero_Limit :=
        Am = 0.0 * mm / s**2 or else Jm = 0.0 * mm / s**3 or else Sm = 0.0 * mm / s**4 or else Cm = 0.0 * mm / s**5;
      if Solver.Zero_Limit then
         return Solver;
      end if;

      --  Each case is selected by testing the sign of the computed phase time it would use, with the equivalent
      --  algebraic condition noted beside each test. A rearranged test rounds differently and can select a region
      --  whose phase time is negative.
      pragma Assert (Limits_In_Domain (Am, Jm, Sm, Cm));
      pragma Assert (Solver_In_Domain (Solver));
      pragma Assert (Kept and then Unset);
      pragma
        Assert
          (Accuracy.In_Domain (Am)
           and then Accuracy.In_Domain (Jm)
           and then Accuracy.In_Domain (Sm)
           and then Accuracy.In_Domain (Cm));
      --  The solver is within its domain from here on: every time is zero until it is given a value within it. The
      --  cuts below keep that and the sign and bound of the latest phase time, and drop how each value was formed,
      --  so that no check has more than a few floating-point operations to consider. What a value means for the
      --  exact ramp is kept as a fact which a lemma of Accuracy gives when the value is computed.
      pragma
        Warnings
          (GNATprove,
           Off,
           "this statement is never reached",
           Reason => "Within the proved domain no operation overflows, so the handlers only serve limits outside it");
      begin
         Lemma_Positive_Quotient (Jm, Sm);
         Solver.Jerk_Over_Snap := Jm / Sm;
         Accuracy.Lemma_Quotient (Solver.Jerk_Over_Snap, Jm, Sm);
         pragma
           Assert_And_Cut
             (Solver_In_Domain (Solver)
              and then Kept
              and then Unset
              and then Accuracy.Quotient_Fact (Solver.Jerk_Over_Snap, Jm, Sm));
         Lemma_Positive_Quotient (Sm, Cm);
         Solver.Snap_Over_Crackle := Sm / Cm;
         Accuracy.Lemma_Quotient (Solver.Snap_Over_Crackle, Sm, Cm);
         pragma Assert_And_Cut (Solver_In_Domain (Solver) and then Quotients);
         Lemma_Less_One (Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle);
         T2 := Solver.Jerk_Over_Snap - Solver.Snap_Over_Crackle;
         Accuracy.Lemma_Quotient_Is_Operand (Solver.Jerk_Over_Snap, Jm, Sm);
         Accuracy.Lemma_Quotient_Is_Operand (Solver.Snap_Over_Crackle, Sm, Cm);
         Accuracy.Lemma_Gap (T2, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle);
         pragma
           Assert_And_Cut
             (Solver_In_Domain (Solver)
              and then T2 <= Solver_Time_Max
              and then Quotients
              and then Accuracy.Gap_Fact (T2, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
              and then
                (if T2 > 0.0 * s
                 then Accuracy.Below (Solver.Snap_Over_Crackle, Solver.Jerk_Over_Snap)
                 else Accuracy.Below (Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)));
         if T2 > 0.0 * s then
            --  Sm ** 2 < Jm * Cm
            Lemma_Positive_Quotient (Am, Jm);
            Solver.Accel_Over_Jerk := Am / Jm;
            Accuracy.Lemma_Quotient (Solver.Accel_Over_Jerk, Am, Jm);
            pragma
              Assert_And_Cut
                (Solver_In_Domain (Solver)
                 and then T2 > 0.0 * s
                 and then T2 <= Solver_Time_Max
                 and then Snap_Regions
                 and then Accuracy.Gap_Fact (T2, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
                 and then Accuracy.Quotient_Fact (Solver.Accel_Over_Jerk, Am, Jm));
            Lemma_Less_Two (Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle);
            T3 := Solver.Accel_Over_Jerk - Solver.Jerk_Over_Snap - Solver.Snap_Over_Crackle;
            Accuracy.Lemma_Quotient_Is_Operand (Solver.Accel_Over_Jerk, Am, Jm);
            Accuracy.Lemma_Quotient_Is_Operand (Solver.Jerk_Over_Snap, Jm, Sm);
            Accuracy.Lemma_Quotient_Is_Operand (Solver.Snap_Over_Crackle, Sm, Cm);
            Accuracy.Lemma_Gap_2 (T3, Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle);
            pragma
              Assert_And_Cut
                (Solver_In_Domain (Solver)
                 and then T2 > 0.0 * s
                 and then T2 <= Solver_Time_Max
                 and then T3 <= Solver_Time_Max
                 and then Snap_Regions
                 and then Accuracy.Gap_Fact (T2, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
                 and then Accuracy.Quotient_Fact (Solver.Accel_Over_Jerk, Am, Jm)
                 and then
                   Accuracy.Gap_Fact_2 (T3, Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
                 and then
                   (if not (T3 >= 0.0 * s)
                    then
                      Accuracy.Below_Sum (Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)));
            if T3 >= 0.0 * s then
               --  Am >= Jm * (Jm / Sm + Sm / Cm)
               Solver.Region := Region_1;
               Solver.T1 := Solver.Snap_Over_Crackle;
               Solver.T2 := T2;
               Solver.T3 := T3;
               Accuracy.Lemma_Prepared_1 (Solver, Am, Jm, Sm, Cm);
               pragma
                 Assert_And_Cut (Solver_In_Domain (Solver) and then Accuracy.Prepared_Fact (Solver, Am, Jm, Sm, Cm));
            else
               Lemma_Snap_Root_Argument (Am, Sm, Cm);
               Lemma_Root_At_Most (0.25 * Sm**2 / Cm**2 + Am / Sm, 2#1.0#E320, 2#1.0#E160);
               Solver.Snap_Root := Dimensionless_Math.Sqrt (0.25 * Sm**2 / Cm**2 + Am / Sm);
               Accuracy.Lemma_Snap_Root (Solver.Snap_Root, Am, Sm, Cm);
               pragma
                 Assert_And_Cut
                   (Solver_In_Domain (Solver)
                    and then Snap_Regions
                    and then Accuracy.Quotient_Fact (Solver.Accel_Over_Jerk, Am, Jm)
                    and then
                      Accuracy.Below_Sum (Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
                    and then Accuracy.Snap_Root_Fact (Solver.Snap_Root, Am, Sm, Cm));
               Lemma_Scaled_Quotient (Sm, Cm);
               Lemma_Less_One (Solver.Snap_Root, 1.5 * Sm / Cm);
               T2 := Solver.Snap_Root - 1.5 * Sm / Cm;
               Accuracy.Lemma_Snap_Root_Is_Minuend (Solver.Snap_Root, Am, Sm, Cm);
               Accuracy.Lemma_Scaled_Gap (T2, Solver.Snap_Root, Sm, Cm, Solver.Snap_Over_Crackle);
               pragma
                 Assert_And_Cut
                   (Solver_In_Domain (Solver)
                    and then T2 <= Solver_Time_Max
                    and then Snap_Regions
                    and then Accuracy.Quotient_Fact (Solver.Accel_Over_Jerk, Am, Jm)
                    and then
                      Accuracy.Below_Sum (Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
                    and then Accuracy.Snap_Root_Fact (Solver.Snap_Root, Am, Sm, Cm)
                    and then Accuracy.Scaled_Gap_Fact (T2, Solver.Snap_Root, Solver.Snap_Over_Crackle)
                    and then
                      (if not (T2 >= 0.0 * s)
                       then Accuracy.Below_Scaled (Solver.Snap_Root, Solver.Snap_Over_Crackle)));
               if T2 >= 0.0 * s then
                  --  Am >= 2.0 * Sm ** 3 / Cm ** 2
                  Solver.Region := Region_2;
                  Solver.T1 := Solver.Snap_Over_Crackle;
                  Solver.T2 := T2;
                  Accuracy.Lemma_Prepared_2 (Solver, Am, Jm, Sm, Cm);
                  pragma
                    Assert_And_Cut
                      (Solver_In_Domain (Solver) and then Accuracy.Prepared_Fact (Solver, Am, Jm, Sm, Cm));
               else
                  Solver.Region := Region_3;
                  Lemma_Crackle_Only_Argument (Am, Cm);
                  Lemma_Cube_Root_At_Most (0.5 * Am / Cm);
                  Solver.T1 := Cube_Root ((0.5 * Am / Cm) / s**3) * s;
                  Accuracy.Lemma_Crackle_Root (Solver.T1, Am, Cm);
                  Accuracy.Lemma_Prepared_3 (Solver, Am, Jm, Sm, Cm);
                  pragma
                    Assert_And_Cut
                      (Solver_In_Domain (Solver) and then Accuracy.Prepared_Fact (Solver, Am, Jm, Sm, Cm));
               end if;
            end if;
         else
            Lemma_Positive_Quotient (Am, Jm);
            Solver.Accel_Over_Jerk := Am / Jm;
            Accuracy.Lemma_Quotient (Solver.Accel_Over_Jerk, Am, Jm);
            pragma Assert_And_Cut (Solver_In_Domain (Solver) and then Jerk_Regions);
            Lemma_Positive_Quotient (Jm, Cm);
            Solver.Jerk_Over_Crackle := Jm / Cm;
            Accuracy.Lemma_Quotient (Solver.Jerk_Over_Crackle, Jm, Cm);
            pragma
              Assert_And_Cut
                (Solver_In_Domain (Solver)
                 and then Jerk_Regions
                 and then Accuracy.Quotient_Fact (Solver.Jerk_Over_Crackle, Jm, Cm));
            Lemma_Root_At_Most (Solver.Jerk_Over_Crackle, 2#1.0#E160, 2#1.0#E80);
            Solver.T1 := Dimensionless_Math.Sqrt (Solver.Jerk_Over_Crackle);
            Jerk_Root := Solver.T1;
            Accuracy.Lemma_Jerk_Root (Jerk_Root, Solver.Jerk_Over_Crackle, Jm, Cm);
            pragma
              Assert_And_Cut
                (Solver_In_Domain (Solver)
                 and then Jerk_Regions
                 and then Accuracy.Quotient_Fact (Solver.Jerk_Over_Crackle, Jm, Cm)
                 and then Accuracy.Same (Solver.T1, Jerk_Root)
                 and then Accuracy.Is_Doubled (Jerk_Root)
                 and then Accuracy.Square_Fact (Jerk_Root, Solver.Jerk_Over_Crackle));
            Lemma_Twice (Solver.T1);
            Lemma_Less_One (Solver.Accel_Over_Jerk, 2.0 * Solver.T1);
            T3 := Solver.Accel_Over_Jerk - 2.0 * Solver.T1;
            Accuracy.Lemma_Quotient_Is_Operand (Solver.Accel_Over_Jerk, Am, Jm);
            Accuracy.Lemma_Twice_Gap (T3, Solver.Accel_Over_Jerk, Jerk_Root);
            pragma
              Assert_And_Cut
                (Solver_In_Domain (Solver)
                 and then T3 <= Solver_Time_Max
                 and then Jerk_Regions
                 and then Accuracy.Quotient_Fact (Solver.Jerk_Over_Crackle, Jm, Cm)
                 and then Accuracy.Same (Solver.T1, Jerk_Root)
                 and then Accuracy.Square_Fact (Jerk_Root, Solver.Jerk_Over_Crackle)
                 and then Accuracy.Twice_Gap_Fact (T3, Solver.Accel_Over_Jerk, Jerk_Root)
                 and then (if not (T3 > 0.0 * s) then Accuracy.Below_Twice (Solver.Accel_Over_Jerk, Jerk_Root)));
            if T3 > 0.0 * s then
               --  Am > 2.0 * Jm * Dimensionless_Math.Sqrt (Jm / Cm)
               Solver.Region := Region_4;
               Solver.T3 := T3;
               Accuracy.Lemma_Prepared_4 (Solver, Am, Jm, Sm, Cm, Jerk_Root);
               pragma
                 Assert_And_Cut (Solver_In_Domain (Solver) and then Accuracy.Prepared_Fact (Solver, Am, Jm, Sm, Cm));
            else
               Solver.Region := Region_5;
               Lemma_Crackle_Only_Argument (Am, Cm);
               Lemma_Cube_Root_At_Most (0.5 * Am / Cm);
               Solver.T1 := Cube_Root ((0.5 * Am / Cm) / s**3) * s;
               Accuracy.Lemma_Crackle_Root (Solver.T1, Am, Cm);
               Accuracy.Lemma_Prepared_5 (Solver, Am, Jm, Sm, Cm, Jerk_Root);
               pragma
                 Assert_And_Cut (Solver_In_Domain (Solver) and then Accuracy.Prepared_Fact (Solver, Am, Jm, Sm, Cm));
            end if;
         end if;
         pragma Assert_And_Cut (Solver_In_Domain (Solver) and then Accuracy.Prepared_Fact (Solver, Am, Jm, Sm, Cm));
      exception
         when Constraint_Error =>
            Solver.Unclassifiable := True;
            return Solver;
      end;
      pragma Assert (Solver_In_Domain (Solver));

      if Solver.Region in Region_1 | Region_2 then
         declare
            Before : constant Delta_V_Solver := Solver
            with Ghost;
            --  The solver before the speed is set, which what was established of it carries over from.
         begin
            Lemma_Snap_Only_Threshold (Sm, Cm);
            Solver.Snap_Only_Above := 8.0 * (Sm**2)**2 / Cm**3;
            --  Sm**4, written as GNAT evaluates it, because GNATprove has no model of a fourth power.
            Accuracy.Lemma_Threshold_Value (Solver.Snap_Only_Above, Before, Am, Jm, Sm, Cm);
            Accuracy.Lemma_Threshold_Set (Before, Solver, Am, Jm, Sm, Cm);
         exception
            when Constraint_Error =>
               Solver.Threshold_Raises := True;
         end;
      else
         Accuracy.Lemma_No_Threshold (Solver, Am, Jm, Sm, Cm);
      end if;
      pragma Warnings (GNATprove, On, "this statement is never reached");
      Accuracy.Lemma_Is_Sound (Solver, Am, Jm, Sm, Cm);
      return Solver;
   end Prepare_Delta_V;

   function Optimal_Profile_For_Delta_V_Internal
     (Solver : Delta_V_Solver; Delta_V : Velocity) return Internal_Profile_Result
   is
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Last_Time_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Jerk_Limited_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Accuracy.Crackle_Time_Fact);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Solve_Is_Sound);
      --  What the lemmas of Accuracy say of each tested phase time is passed on to the lemma which concludes the
      --  case, and what that concludes is the postcondition. Nothing here needs the expressions of either.

      Vd     : constant Velocity := abs Delta_V;
      Am     : Acceleration renames Solver.Am;
      Jm     : Jerk renames Solver.Jm;
      Cm     : Crackle renames Solver.Cm;
      T1     : Time renames Solver.T1;
      Region : Constraint_Region renames Solver.Region;
      T3, T4 : Time;

      function Crackle_Limited return Internal_Profile_Result
      is ([Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)), 0.0 * s, 0.0 * s, 0.0 * s], Region, 1)
      with
        Pre  =>
          Vd >= No_Speed
          and then Vd <= Speed_Max
          and then Cm >= Crackle_Limit_Min
          and then Crackle_Limited_Time_Fits (Vd, Cm),
        Post => Times_In_Range (Crackle_Limited'Result.Profile) and then Crackle_Limited'Result.Index = 1;
      --  No limit other than crackle is reached. The bound of the roots comes from Lemma_Crackle_Limited_In_Range,
      --  which an expression function cannot call for itself, so a caller does and passes it on.

      function Snap_Limited return Internal_Profile_Result
      with
        Pre  => Solver_In_Domain (Solver) and then Vd >= No_Speed and then Vd <= Speed_Max,
        Post =>
          Times_In_Range (Snap_Limited'Result.Profile)
          and then Snap_Limited'Result.Index = 2
          and then Snap_Limited'Result.Profile (1) = T1
          and then Gain_Is_Bisected (Snap_Limited'Result.Profile, Vd, Cm);
      --  Snap is the only derivative limit reached, so the second phase time solves
      --
      --     Vd = Cm*T1*(T1 + T2)*(2*T1 + T2)*(4*T1 + 2*T2)
      --
      --  with T1 = Sm/Cm. The analytical solution that Mathematica outputs involves a Cm**18, which is far outside
      --  the range of Dimensioned_Float for reasonable values of Cm. For reference:
      --  ToRadicals[
      --    Solve[
      --      With[
      --        {T1 = Sm/Cm, T3 = 0, T4 = 0},
      --        v == Cm*T1*(T1 + T2)*(2*T1 + T2 + T3)*(4*T1 + 2*T2 + T3 + T4)
      --      ],
      --      T2,
      --      NonNegativeReals
      --    ]
      --  ]
      --
      --  The result is the largest time below 24 hours whose profile, as Fast_Velocity_At_Max_Time evaluates it,
      --  does not exceed Vd. That evaluation is a product of sums of nonnegative terms, so it is monotone in machine
      --  arithmetic and the search returns what bisecting every machine number would.

      function Snap_Limited return Internal_Profile_Result is
         function Excess (T2_Seconds : Dimensionless) return Dimensionless
         is ((Fast_Velocity_At_Max_Time ([T1, T2_Seconds * s, 0.0 * s, 0.0 * s], Cm, 0.0 * mm / s) - Vd) / (mm / s))
         with
           Pre  =>
             T2_Seconds >= 0.0
             and then T2_Seconds <= 86_400.0
             and then Solver_In_Domain (Solver)
             and then Vd >= No_Speed
             and then Vd <= Speed_Max,
           Post =>
             abs Excess'Result <= Searches.Residual_Max
             and then
               (Excess'Result <= 0.0)
               = (Velocity_Formula (T1, T2_Seconds * s, 0.0 * s, 0.0 * s, Cm, 0.0 * mm / s) <= Vd);
         pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Excess);
         --  The search is proved from the contract of Excess alone. Its expression, repeated at every point where
         --  the search evaluates it, only burdens the provers. The contract says what the sign of the excess
         --  means, with the end formula as a function of the phase times, so that the certificate of the search
         --  can be stated for the profile returned.

         function Evaluable (Lower, Upper : Dimensionless) return Boolean
         is (Lower >= 0.0
             and then Upper <= 86_400.0
             and then Solver_In_Domain (Solver)
             and then Vd >= No_Speed
             and then Vd <= Speed_Max);
         --  The brackets on which Excess may be evaluated. Only the precondition of Solve calls this.

         function Solve is new Searches.Largest_Nonpositive (Excess, Evaluable);

         Guess : constant Dimensionless := Estimates.Snap_Limited_Guess (Vd, Cm, T1);

         Solved_Seconds : constant Dimensionless :=
           Solve
             (Lower          => 0.0,
              Upper          => 86_400.0,
              --  A maximum of 24 hours should be more than enough.
              Lower_Residual => 0.0,
              Upper_Residual => 0.0,
              Guess          => Guess,
              Use_Model      => False);

         Next_Seconds : constant Dimensionless := To_Real (To_Bits (Solved_Seconds) + 1)
         with Ghost;
         --  The machine number after the result of the search, which the search's certificate speaks of.
      begin
         --  The certificate of the search is about a number of seconds. What follows restates it about the phase
         --  time: a positive number of seconds times the unit is the same number, with the same bit pattern.
         pragma Assert (Solved_Seconds >= 0.0 and then Solved_Seconds < 86_400.0);
         pragma Assert (Same (Solved_Seconds * s, Solved_Seconds));
         pragma Assert (Same (Next_Seconds * s, Next_Seconds));
         pragma Assert (Next_Seconds > 0.0 and then Next_Seconds <= 86_400.0);
         pragma
           Assert
             (Solved_Seconds = 0.0
              or else Velocity_Formula (T1, Solved_Seconds * s, 0.0 * s, 0.0 * s, Cm, 0.0 * mm / s) <= Vd);
         if Solved_Seconds > 0.0 then
            pragma Assert (To_Bits (Solved_Seconds * s) = To_Bits (Solved_Seconds));
            Lemma_Order (Next_Time (Solved_Seconds * s), Next_Seconds);
            pragma Assert (Same (Next_Time (Solved_Seconds * s), Next_Seconds));
            pragma Assert (Next_Time (Solved_Seconds * s) = Next_Seconds * s);
            pragma
              Assert
                (Next_Seconds = 86_400.0
                 or else Velocity_Formula (T1, Next_Seconds * s, 0.0 * s, 0.0 * s, Cm, 0.0 * mm / s) > Vd);
            pragma
              Assert
                (Next_Time (Solved_Seconds * s) = Search_Time_Cap
                 or else
                   Velocity_Formula (T1, Next_Time (Solved_Seconds * s), 0.0 * s, 0.0 * s, Cm, 0.0 * mm / s) > Vd);
         else
            --  The smallest machine number is below one, which is below the cap.
            pragma Assert (To_Bits (Solved_Seconds) = 0);
            Lemma_Positive_Pattern (1.0);
            Lemma_Positive_Pattern (86_400.0);
            Lemma_Order (1.0, 86_400.0);
            Lemma_Order (Next_Seconds, 86_400.0);
            pragma Assert (Next_Seconds < 86_400.0);
            Lemma_Order (Smallest_Time, Next_Seconds);
            pragma Assert (Same (Smallest_Time, Next_Seconds * s));
            pragma Assert (Smallest_Time < Search_Time_Cap);
            pragma Assert (Velocity_Formula (T1, Next_Seconds * s, 0.0 * s, 0.0 * s, Cm, 0.0 * mm / s) > Vd);
            pragma Assert (Velocity_Formula (T1, Smallest_Time, 0.0 * s, 0.0 * s, Cm, 0.0 * mm / s) > Vd);
         end if;
         return ([T1, Solved_Seconds * s, 0.0 * s, 0.0 * s], Region, 2);
      end Snap_Limited;
   begin
      if not (Vd <= Velocity'Last) or else not Solver.Limits_Valid then
         raise Constraint_Error with "Invalid velocity-profile inputs.";
      elsif Solver.Zero_Limit then
         if Vd = 0.0 * mm / s then
            return ([others => 0.0 * s], Region_5, 1);
         end if;
         raise Constraint_Error with "A velocity change requires positive derivative limits.";
      elsif Solver.Unclassifiable then
         raise Constraint_Error with "Unrepresentable phase times for these derivative limits.";
      end if;
      --  This function is called a lot more than Optimal_Profile_For_Distance, so we use simple analytical solutions
      --  where they exist, and the limit-only parts of those come prepared in Solver.
      --
      --  As in Optimal_Profile_For_Distance_Internal, each case is selected by testing the sign of the computed phase
      --  time it would use, with the equivalent algebraic condition noted beside each test.
      pragma
        Assert
          (T1 >= No_Time
           and then T1 <= Phase_Time_Max
           and then Solver.T2 >= No_Time
           and then Solver.T2 <= Phase_Time_Max
           and then Solver.T3 >= No_Time
           and then Solver.T3 <= Phase_Time_Max);
      --  The prepared times, as the postcondition bounds them: stated here so that each result below finds them
      --  as they stand.
      --
      --  The cuts keep the bound of the latest phase time and drop how it was formed, so that the postcondition,
      --  which every result reaches at once, has no floating-point operation to consider. A cut which names a
      --  variable also stops GNAT from substituting the region of a case alternative for Region in the results
      --  after it: where the compiler does not find that for itself, the code reads the region from Solver where
      --  it loaded a constant. That is the only trace which the proof leaves in the code generated for this
      --  function, and the postcondition is not provable within the time limit without these cuts.
      case Region is
         when Region_1            =>
            Lemma_Quotient (Vd, Am);
            Lemma_Less_Three (Vd / Am, Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle);
            T4 := Vd / Am - Solver.Accel_Over_Jerk - Solver.Jerk_Over_Snap - Solver.Snap_Over_Crackle;
            Accuracy.Lemma_Last_Time_1 (T4, Vd / Am, Vd, Solver);
            pragma Assert_And_Cut (T4 <= Phase_Time_Max and then Accuracy.Last_Time_Fact (T4, Vd, Solver));
            if T4 > 0.0 * s then
               --  Vd > Am * (Am / Jm + Jm / Sm + Sm / Cm)
               --  Reachable: Sm, Jm, Am (Test Case V1.4)
               Accuracy.Lemma_Case_Full (T4, Vd, Solver);
               return ([T1, Solver.T2, Solver.T3, T4], Region, 4);
            end if;
            Lemma_Jerk_Limited_In_Range (Solver, Vd);
            Lemma_Jerk_Limited_Time (Vd, Jm, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle);
            T3 :=
              0.5 * Dimensionless_Math.Sqrt ((Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle)**2 + 4.0 * Vd / Jm)
              - 1.5 * (Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle);
            Accuracy.Lemma_Jerk_Limited_1 (T3, Vd, Solver);
            pragma
              Assert_And_Cut
                (T3 <= Phase_Time_Max
                 and then Accuracy.Last_Time_Fact (T4, Vd, Solver)
                 and then not (T4 > 0.0 * s)
                 and then Accuracy.Jerk_Limited_Fact (T3, Vd, Solver));
            if T3 > 0.0 * s then
               --  Vd > 2.0 * Jm * (Jm / Sm + Sm / Cm) ** 2
               --  Reachable: Sm, Jm (Test Case V1.3)
               Accuracy.Lemma_Case_Jerk_Limited (T3, T4, Vd, Solver);
               return ([T1, Solver.T2, T3, 0.0 * s], Region, 3);
            elsif Solver.Threshold_Raises then
               raise Constraint_Error with "Unrepresentable snap-limited velocity threshold.";
            elsif Vd > Solver.Snap_Only_Above then
               --  Vd > 8.0 * Sm ** 4 / Cm ** 3
               --  Reachable: Sm (Test Case V1.2)
               Accuracy.Lemma_Snap_Limited_Is_Sound (Snap_Limited.Profile, T3, Vd, Solver);
               return Snap_Limited;
            else
               --  Reachable: None (Test Case V1.1)
               Lemma_Crackle_Limited_In_Range (Vd, Cm);
               Accuracy.Lemma_Case_Crackle_Limited_Snap (Crackle_Limited.Profile (1), Vd, Solver);
               return Crackle_Limited;
            end if;

         when Region_2            =>
            Lemma_Quotient (Vd, Am);
            Lemma_Twice (Solver.Snap_Root);
            Lemma_Less_Two (Vd / Am, Solver.Snap_Over_Crackle, 2.0 * Solver.Snap_Root);
            T4 := Vd / Am - Solver.Snap_Over_Crackle - 2.0 * Solver.Snap_Root;
            Accuracy.Lemma_Last_Time_2 (T4, Vd / Am, Vd, Solver);
            pragma Assert_And_Cut (T4 <= Phase_Time_Max and then Accuracy.Last_Time_Fact (T4, Vd, Solver));
            if T4 > 0.0 * s then
               --  Vd > Am * (2.0 * Dimensionless_Math.Sqrt (0.25 * Sm ** 2 / Cm ** 2 + Am / Sm) + Sm / Cm)
               --  Reachable: Sm, Am (Test Case V2.4)
               Accuracy.Lemma_Case_Full (T4, Vd, Solver);
               return ([T1, Solver.T2, 0.0 * s, T4], Region, 4);
            elsif Solver.Threshold_Raises then
               raise Constraint_Error with "Unrepresentable snap-limited velocity threshold.";
            elsif Vd > Solver.Snap_Only_Above then
               --  Vd > 8.0 * Sm ** 4 / Cm ** 3
               --  Reachable: Sm (Test Case V2.2)
               Accuracy.Lemma_Snap_Limited_Is_Sound (Snap_Limited.Profile, T4, Vd, Solver);
               return Snap_Limited;
            else
               --  Reachable: None (Test Case V2.1)
               Lemma_Crackle_Limited_In_Range (Vd, Cm);
               Accuracy.Lemma_Case_Crackle_Limited_Snap (Crackle_Limited.Profile (1), Vd, Solver);
               return Crackle_Limited;
            end if;

         when Region_3 | Region_5 =>
            Lemma_Quotient (Vd, Am);
            Lemma_Four_Times (T1);
            Lemma_Less_One (Vd / Am, 4.0 * T1);
            T4 := Vd / Am - 4.0 * T1;
            Accuracy.Lemma_Last_Time_3 (T4, Vd / Am, Vd, Solver);
            pragma Assert_And_Cut (T4 <= Phase_Time_Max and then Accuracy.Last_Time_Fact (T4, Vd, Solver));
            if T4 > 0.0 * s then
               --  Vd > 8.0 * Cm * (0.5 * Am / Cm) ** (4 / 3)
               --  Reachable: Am (Test Cases V3.4 and V5.4)
               Accuracy.Lemma_Case_Full (T4, Vd, Solver);
               return ([T1, 0.0 * s, 0.0 * s, T4], Region, 4);
            else
               --  Reachable: None (Test Cases V3.1 and V5.1)
               Lemma_Crackle_Limited_In_Range (Vd, Cm);
               Accuracy.Lemma_Case_Crackle_Limited_Single (Crackle_Limited.Profile (1), T4, Vd, Solver);
               return Crackle_Limited;
            end if;

         when Region_4            =>
            Lemma_Quotient (Vd, Am);
            Lemma_Twice (T1);
            Lemma_Less_Two (Vd / Am, Solver.Accel_Over_Jerk, 2.0 * T1);
            T4 := Vd / Am - Solver.Accel_Over_Jerk - 2.0 * T1;
            Accuracy.Lemma_Last_Time_4 (T4, Vd / Am, Vd, Solver);
            pragma Assert_And_Cut (T4 <= Phase_Time_Max and then Accuracy.Last_Time_Fact (T4, Vd, Solver));
            if T4 > 0.0 * s then
               --  Vd > Am * (Am / Jm + 2.0 * Dimensionless_Math.Sqrt (Jm / Cm))
               --  Reachable: Jm, Am (Test Case V4.4)
               Accuracy.Lemma_Case_Full (T4, Vd, Solver);
               return ([T1, 0.0 * s, Solver.T3, T4], Region, 4);
            end if;
            Lemma_Jerk_Only_In_Range (Solver, Vd);
            Lemma_Jerk_Only_Time (Vd, Jm, Solver.Jerk_Over_Crackle);
            Lemma_Thrice (T1);
            Lemma_Less_One (Dimensionless_Math.Sqrt (Solver.Jerk_Over_Crackle + Vd / Jm), 3.0 * T1);
            T3 := Dimensionless_Math.Sqrt (Solver.Jerk_Over_Crackle + Vd / Jm) - 3.0 * T1;
            Accuracy.Lemma_Jerk_Limited_4 (T3, Vd, Solver);
            pragma
              Assert_And_Cut
                (T3 <= Phase_Time_Max
                 and then Accuracy.Last_Time_Fact (T4, Vd, Solver)
                 and then not (T4 > 0.0 * s)
                 and then Accuracy.Jerk_Limited_Fact (T3, Vd, Solver));
            if T3 > 0.0 * s then
               --  Vd > 8.0 * Jm ** 2 / Cm
               --  Reachable: Jm (Test Case V4.3)
               Accuracy.Lemma_Case_Jerk_Limited (T3, T4, Vd, Solver);
               return ([T1, 0.0 * s, T3, 0.0 * s], Region, 3);
            else
               --  Reachable: None (Test Case V4.1)
               Lemma_Crackle_Limited_In_Range (Vd, Cm);
               Accuracy.Lemma_Case_Crackle_Limited_Jerk (Crackle_Limited.Profile (1), T3, Vd, Solver);
               return Crackle_Limited;
            end if;
      end case;
   end Optimal_Profile_For_Delta_V_Internal;

   procedure Lemma_Prepared_Solve
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
   with
     Ghost,
     Global => null,
     Always_Terminates,
     Pre    => Ramp_Request_Is_Provable (Delta_V, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max),
     Post   =>
       (if Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
        then
          Ramp_Is_Sound
            (Optimal_Profile_For_Delta_V_Internal
               (Prepare_Delta_V (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max), Delta_V)
               .Profile,
             abs Delta_V,
             Acceleration_Max,
             Jerk_Max,
             Snap_Max,
             Crackle_Max));
   --  A solve with the solver which was prepared for the limits returns a ramp within those limits.

   procedure Lemma_Prepared_Solve
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
   is
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Solve_Is_Sound);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Solver_Is_Consistent);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Snap_Is_Within);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Ramp_Is_Sound);

      Solver : constant Delta_V_Solver := Prepare_Delta_V (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
      Times  : constant Feedrate_Profile_Times := Optimal_Profile_For_Delta_V_Internal (Solver, Delta_V).Profile;
   begin
      if Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max) then
         Accuracy.Lemma_Prepared_Is_Within_Limits
           (Solver, Times, abs Delta_V, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
      end if;
   end Lemma_Prepared_Solve;

   function Optimal_Profile_For_Delta_V_Internal
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
      return Internal_Profile_Result
   is
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Ramp_Is_Sound);
   begin
      Lemma_Prepared_Solve (Delta_V, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
      return
        Optimal_Profile_For_Delta_V_Internal
          (Prepare_Delta_V (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max), Delta_V);
   end Optimal_Profile_For_Delta_V_Internal;

   function Optimal_Profile_For_Delta_V (Solver : Delta_V_Solver; Delta_V : Velocity) return Feedrate_Profile_Times is
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Solve_Is_Sound);
   begin
      if Delta_V = 0.0 * mm / s and then Solver.Limits_Valid then
         --  Reachability certificates put the peak at one endpoint, so one ramp is usually empty.
         Accuracy.Lemma_Empty_Is_Sound (abs Delta_V, Solver);
         return [others => 0.0 * s];
      end if;
      return Optimal_Profile_For_Delta_V_Internal (Solver, Delta_V).Profile;
   end Optimal_Profile_For_Delta_V;

   function Optimal_Profile_For_Delta_V
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
      return Feedrate_Profile_Times is
   begin
      if Delta_V = 0.0 * mm / s
        and then Derivative_Limits_Are_Valid (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
      then
         --  Reachability certificates put the peak at one endpoint, so one ramp is usually empty.
         Accuracy.Lemma_Empty_Is_Within_Limits
           ([others => 0.0 * s], abs Delta_V, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
         return [others => 0.0 * s];
      end if;
      return Optimal_Profile_For_Delta_V_Internal (Delta_V, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max).Profile;
   end Optimal_Profile_For_Delta_V;

   function Try_Optimal_Full_Profile
     (Start_Vel        : Velocity;
      Max_Vel          : Velocity;
      End_Vel          : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Full_Profile_Result
   is
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Solve_Is_Sound);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Solver_Is_Consistent);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Top_Speed_Is_At_Most);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Peaks_Are_Limited);
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Snap_Is_Within);
      --  The bounds of the peaks and of the highest speed are passed from the solves of the ramps to the
      --  postcondition by the lemmas of Accuracy. Nothing here needs what they say.

      Profile                        : Feedrate_Profile;
      Solver                         : Delta_V_Solver;
      Direct_Distance                : Length;
      --  The distance of the ramp straight from Start_Vel to End_Vel.
      Accel_Distance, Decel_Distance : Length;

      type Ramp_Pair is record
         Accel, Decel                   : Feedrate_Profile_Times;
         Accel_Distance, Decel_Distance : Length;
      end record;

      function Ramps_Are_Provable return Boolean
      is (Solver_In_Domain (Solver)
          and then Start_Vel >= No_Speed
          and then Start_Vel <= Speed_Max
          and then End_Vel >= No_Speed
          and then End_Vel <= Speed_Max
          and then Distance >= No_Distance
          and then Distance <= Distance_Max
          and then Crackle_Max >= Crackle_Limit_Min
          and then Crackle_Max <= Crackle_Limit_Max)
      with Ghost;
      --  Solver is prepared for a request within the proved domain, so the ramps to a peak may be formed.

      function Request_In_Domain return Boolean
      is (Ramps_Are_Provable
          and then Derivative_Limits_Are_Valid (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
          and then Max_Vel >= Speed_Min
          and then Max_Vel <= Speed_Max
          and then Max_Vel >= Start_Vel
          and then Max_Vel >= End_Vel
          and then Solver_Is_Consistent (Solver)
          and then Snap_Is_Within (Solver, Snap_Max)
          and then Accuracy.Holds_Limits (Solver, Acceleration_Max, Jerk_Max, Crackle_Max)
          and then Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max))
      with Ghost;
      --  What holds from the point where Solver is prepared.

      function Peaks_Are_Bounded (Accel, Decel : Feedrate_Profile_Times) return Boolean
      is (Peaks_Are_Limited (Accel (1), Accel (2), Accel (3), Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
          and then
            Peaks_Are_Limited (Decel (1), Decel (2), Decel (3), Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max))
      with Ghost;
      --  Both ramps are within the limits.

      function Top_Speed_Is_Bounded (Accel : Feedrate_Profile_Times) return Boolean
      is (Top_Speed_Is_At_Most (Accel (1), Accel (2), Accel (3), Accel (4), Start_Vel, Max_Vel, Crackle_Max))
      with Ghost;
      --  The acceleration ramp Accel ends at no more than Max_Vel.

      function Ramps_To (Peak : Velocity) return Ramp_Pair
      with
        Pre  => Ramps_Are_Provable and then Peak >= No_Speed and then Peak <= Speed_Max,
        Post =>
          Times_In_Range (Ramps_To'Result.Accel)
          and then Times_In_Range (Ramps_To'Result.Decel)
          and then abs Ramps_To'Result.Accel_Distance <= Ramp_Distance_Max
          and then abs Ramps_To'Result.Decel_Distance <= Ramp_Distance_Max
          and then
            (if Solver_Is_Consistent (Solver) and then Solver.Cm = Crackle_Max and then Peak >= Start_Vel
             then
               Top_Speed_Is_At_Most
                 (Ramps_To'Result.Accel (1),
                  Ramps_To'Result.Accel (2),
                  Ramps_To'Result.Accel (3),
                  Ramps_To'Result.Accel (4),
                  Start_Vel,
                  Peak,
                  Crackle_Max))
          and then
            (if Solver_Is_Consistent (Solver)
               and then Snap_Is_Within (Solver, Snap_Max)
               and then Accuracy.Holds_Limits (Solver, Acceleration_Max, Jerk_Max, Crackle_Max)
               and then Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
             then Peaks_Are_Bounded (Ramps_To'Result.Accel, Ramps_To'Result.Decel));
      --  Both ramps of a profile that peaks at Peak, and their distances. The acceleration ramp ends at no more
      --  than Peak, and both ramps are within the limits.

      procedure Set_Peak (Peak : Velocity);
      --  Form both ramps for a profile that peaks at Peak, and their distances.

      function Ramps_To (Peak : Velocity) return Ramp_Pair is
         Accel : constant Feedrate_Profile_Times := Optimal_Profile_For_Delta_V (Solver, Start_Vel - Peak);
         Decel : constant Feedrate_Profile_Times :=
           (if Start_Vel = End_Vel then Accel else Optimal_Profile_For_Delta_V (Solver, End_Vel - Peak));
      begin
         Accuracy.Lemma_Top_Speed (Solver, Accel, Start_Vel, Peak, Crackle_Max);
         Accuracy.Lemma_Peaks
           (Solver, Accel, abs (Start_Vel - Peak), Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
         if Start_Vel /= End_Vel then
            Accuracy.Lemma_Peaks
              (Solver, Decel, abs (End_Vel - Peak), Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
         end if;
         return
           (Accel          => Accel,
            Decel          => Decel,
            Accel_Distance => Fast_Distance_At_Max_Time (Accel, Crackle_Max, Start_Vel),
            Decel_Distance => Fast_Distance_At_Max_Time (Decel, -Crackle_Max, Peak));
      end Ramps_To;

      procedure Set_Peak (Peak : Velocity) is
         Ramps : constant Ramp_Pair := Ramps_To (Peak);
      begin
         Profile.Accel := Ramps.Accel;
         Profile.Decel := Ramps.Decel;
         Accel_Distance := Ramps.Accel_Distance;
         Decel_Distance := Ramps.Decel_Distance;
      end Set_Peak;
   begin
      if not (Distance >= 0.0 * mm
              and then Distance <= Length'Last
              and then Start_Vel >= 0.0 * mm / s
              and then Start_Vel <= Velocity'Last
              and then End_Vel >= 0.0 * mm / s
              and then End_Vel <= Velocity'Last
              and then Max_Vel >= 0.0 * mm / s
              and then Max_Vel <= Velocity'Last)
        or else not Derivative_Limits_Are_Valid (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
      then
         raise Constraint_Error with "Invalid full-profile inputs.";
      end if;
      if Max_Vel < Start_Vel then
         raise Constraint_Error with "Max_Vel can not be smaller than Start_Vel.";
      end if;

      if Max_Vel < End_Vel then
         raise Constraint_Error with "Max_Vel can not be smaller than End_Vel.";
      end if;

      if Distance = 0.0 * mm then
         if Start_Vel /= End_Vel then
            raise Constraint_Error with "A zero-distance profile cannot change velocity.";
         end if;
         pragma Assert (Validity_Inputs_Are_Bounded (Distance, Start_Vel, End_Vel, Crackle_Max));
         pragma
           Assert
             (Distance = No_Distance
              and then Start_Vel >= No_Speed
              and then End_Vel = Start_Vel
              and then Crackle_Max >= 0.0 * mm / s**5);
         --  The precondition of Profile_Is_Valid and the case in which it accepts an empty profile, each as its
         --  contract states it, so that the postcondition for the profile returned here is left with only the
         --  times of that profile to look at.
         Accuracy.Lemma_Top_Speed_Of_Rest (0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s, Start_Vel, Max_Vel, Crackle_Max);
         Accuracy.Lemma_Peaks_Of_Rest (0.0 * s, 0.0 * s, 0.0 * s, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
         return (Reachable => True, Profile => <>);
      end if;

      if Acceleration_Max = 0.0 * mm / s**2
        or else Jerk_Max = 0.0 * mm / s**3
        or else Snap_Max = 0.0 * mm / s**4
        or else Crackle_Max = 0.0 * mm / s**5
      then
         if Start_Vel /= End_Vel or else Start_Vel = 0.0 * mm / s then
            raise Constraint_Error with "This motion requires positive derivative limits.";
         end if;
         Lemma_Quotient (Distance, Start_Vel);
         Profile := (Accel => [others => 0.0 * s], Coast => Distance / Start_Vel, Decel => [others => 0.0 * s]);
         pragma Assert_And_Cut (Times_Are_Zero (Profile.Accel) and then Times_Are_Zero (Profile.Decel));
         if not Profile_Is_Valid (Profile, Distance, Start_Vel, End_Vel, Crackle_Max) then
            raise Constraint_Error with "Unrepresentable constant-speed profile.";
         end if;
         Accuracy.Lemma_Top_Speed_Of_Rest
           (Profile.Accel (1),
            Profile.Accel (2),
            Profile.Accel (3),
            Profile.Accel (4),
            Start_Vel,
            Max_Vel,
            Crackle_Max);
         return (Reachable => True, Profile => Profile);
      end if;

      if Max_Vel = 0.0 * mm / s then
         raise Constraint_Error with "Positive distance requires positive velocity.";
      end if;

      Solver := Prepare_Delta_V (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
      pragma Assert (Request_In_Domain);
      pragma Assert (Limits_In_Domain (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max));
      --  The cuts below keep what the rest of the function needs and drop how it came about, so that no check
      --  has more than a few floating-point operations to consider. The first of them comes after the direct
      --  ramp, which is stated in the postcondition as the ramp of the solver prepared here.

      declare
         Profile : constant Feedrate_Profile_Times := Optimal_Profile_For_Delta_V (Solver, Start_Vel - End_Vel);
      begin
         Direct_Distance :=
           Fast_Distance_At_Max_Time
             (Profile,
              (if Start_Vel < End_Vel then Crackle_Max / (mm / s**5) else -Crackle_Max / (mm / s**5))
              * (mm / s**5), --  Temporarily drop the dimension otherwise gnatcov breaks it.
              Start_Vel);
         pragma
           Assert
             (Direct_Distance
              = Direct_Ramp_Distance (Start_Vel, End_Vel, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max));
         if Distance < Direct_Distance then
            return (Reachable => False, Profile => <>);
         end if;
      end;
      pragma Assert_And_Cut (Request_In_Domain and then abs Direct_Distance <= Ramp_Distance_Max);

      Set_Peak (Max_Vel);
      pragma
        Assert_And_Cut
          (Request_In_Domain
           and then abs Direct_Distance <= Ramp_Distance_Max
           and then Times_In_Range (Profile.Accel)
           and then Times_In_Range (Profile.Decel)
           and then abs Accel_Distance <= Ramp_Distance_Max
           and then abs Decel_Distance <= Ramp_Distance_Max
           and then Top_Speed_Is_Bounded (Profile.Accel)
           and then Peaks_Are_Bounded (Profile.Accel, Profile.Decel));

      if Accel_Distance + Decel_Distance <= Distance then
         Checked.Divide ((Distance - Accel_Distance - Decel_Distance), Max_Vel, Profile.Coast);
      else
         Profile.Coast := 0.0 * s;
         declare
            function Excess (Peak : Dimensionless) return Dimensionless
            with
              Pre  => Ramps_Are_Provable and then Peak >= 0.0 and then Peak <= 2#1.0#E80,
              Post => abs Excess'Result <= Searches.Residual_Max;
            --  How far the ramps to and from Peak overrun Distance.

            function Excess (Peak : Dimensionless) return Dimensionless is
               Ramps : constant Ramp_Pair := Ramps_To (Peak * (mm / s));
            begin
               return (Ramps.Accel_Distance + Ramps.Decel_Distance - Distance) / mm;
            end Excess;

            function Evaluable (Lower, Upper : Dimensionless) return Boolean
            is (Solver_In_Domain (Solver)
                and then Start_Vel >= No_Speed
                and then Start_Vel <= Speed_Max
                and then End_Vel >= No_Speed
                and then End_Vel <= Speed_Max
                and then Distance >= No_Distance
                and then Distance <= Distance_Max
                and then Crackle_Max >= Crackle_Limit_Min
                and then Crackle_Max <= Crackle_Limit_Max
                and then Lower >= 0.0
                and then Upper <= 2#1.0#E80);
            --  The brackets of peak speeds, in mm/s, on which Excess may be evaluated: Ramps_Are_Provable, spelled
            --  out because this cannot be a ghost function, and peaks of at most Speed_Max. Only the precondition
            --  of Highest_Peak calls this, so it never runs. It names nothing which Ramps_To and Excess do not
            --  name already, and so leaves the frame which they share as it was.

            function Highest_Peak is new Searches.Largest_Nonpositive (Excess, Evaluable);

            Peak_Tolerance : constant Dimensionless := 2.0**(-40);
            --  How closely the search brackets the highest peak, as a fraction of it. The duration is not stationary
            --  at that peak: a peak this fraction below it lengthens the profile by at most the same fraction, and
            --  for a shortfall this small by at most about 3/8 of it, so about 3.4E-13 of the duration. Settling
            --  the last machine number as well would take most of the evaluations of the search.

            --  The ramp distances grow with the peak, so the fastest profile has the highest peak whose ramps fit
            --  in Distance. Whatever distance the peak found leaves over is coasted. The ramps for the lowest peak
            --  are the direct ramp checked above, which fits.
            --
            --  Between equal speeds the two ramps mirror each other, so each covers half of Distance and the
            --  peak is the speed that one ramp gains over that half. That estimate only steers the search.
            Symmetric_Gain : constant Velocity :=
              (if Start_Vel = End_Vel
               then Estimates.Delta_V_Over_Distance (Solver, Start_Vel, Distance / 2.0)
               else -1.0 * mm / s);
            Lower          : constant Velocity :=
              Highest_Peak
                (Lower              => Velocity'Max (Start_Vel, End_Vel) / (mm / s),
                 Upper              => Max_Vel / (mm / s),
                 Lower_Residual     => (Direct_Distance - Distance) / mm,
                 Upper_Residual     => (Accel_Distance + Decel_Distance - Distance) / mm,
                 Relative_Tolerance => Peak_Tolerance,
                 Guess              =>
                   (if Symmetric_Gain >= 0.0 * mm / s then (Start_Vel + Symmetric_Gain) / (mm / s) else -1.0),
                 Use_Model          => Symmetric_Gain < 0.0 * mm / s)
              * (mm / s);
         begin
            pragma Assert (Lower >= No_Speed and then Lower <= Speed_Max);
            Lemma_Unit_Speed (Max_Vel);
            Lemma_Unit_Speed (Lower);
            pragma Assert (Lower >= Start_Vel);
            pragma Assert (Lower / (mm / s) <= Max_Vel / (mm / s));
            pragma Assert (Lower <= Max_Vel);
            Set_Peak (Lower);
            Accuracy.Lemma_Top_Speed_Weaken
              (Profile.Accel (1),
               Profile.Accel (2),
               Profile.Accel (3),
               Profile.Accel (4),
               Start_Vel,
               Lower,
               Max_Vel,
               Crackle_Max);
            pragma
              Assert_And_Cut
                (Request_In_Domain
                 and then Times_In_Range (Profile.Accel)
                 and then Times_In_Range (Profile.Decel)
                 and then abs Accel_Distance <= Ramp_Distance_Max
                 and then abs Decel_Distance <= Ramp_Distance_Max
                 and then Top_Speed_Is_Bounded (Profile.Accel)
                 and then Peaks_Are_Bounded (Profile.Accel, Profile.Decel));

            if Lower > 0.0 * mm / s and then Accel_Distance + Decel_Distance < Distance then
               Checked.Divide ((Distance - Accel_Distance - Decel_Distance), Lower, Profile.Coast);
            end if;
         end;
         pragma
           Assert_And_Cut
             (Request_In_Domain
              and then Times_In_Range (Profile.Accel)
              and then Times_In_Range (Profile.Decel)
              and then Top_Speed_Is_Bounded (Profile.Accel)
              and then Peaks_Are_Bounded (Profile.Accel, Profile.Decel));
      end if;
      pragma
        Assert_And_Cut
          (Request_In_Domain
           and then Times_In_Range (Profile.Accel)
           and then Times_In_Range (Profile.Decel)
           and then Top_Speed_Is_Bounded (Profile.Accel)
           and then Peaks_Are_Bounded (Profile.Accel, Profile.Decel));

      if not Profile_Is_Valid (Profile, Distance, Start_Vel, End_Vel, Crackle_Max) then
         raise Constraint_Error with "The generated profile cannot represent the requested motion.";
      end if;
      return (Reachable => True, Profile => Profile);
   end Try_Optimal_Full_Profile;

   function Optimal_Full_Profile
     (Start_Vel        : Velocity;
      Max_Vel          : Velocity;
      End_Vel          : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Feedrate_Profile
   is
      Result : Full_Profile_Result;
   begin
      Result :=
        Try_Optimal_Full_Profile
          (Start_Vel, Max_Vel, End_Vel, Distance, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
      if not Result.Reachable then
         raise Constraint_Error with "End_Vel is not reachable under given constraints.";
      end if;
      return Result.Profile;
   end Optimal_Full_Profile;

end Prunt.Motion_Planner.Trajectory.Solvers;
