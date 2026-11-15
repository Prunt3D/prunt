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
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile. The
--  ghost code among them computes with exact rationals and would be far too slow to run.

--  The rounding analysis of the ramp solvers: what the floating-point quotients, differences and roots which they
--  compute say about the exact ramp.
--
--  The solvers call a lemma of this package after each value which they compute. The lemma's precondition is the
--  floating-point expression as the solver writes it, and its postcondition a fact about the reals which the
--  operands and the result denote, with no floating-point operation in it. The solvers keep those facts across
--  their cuts with the expressions hidden, so that the checks of the solvers themselves have nothing more to
--  consider, and a lemma for each region or case then combines the facts, in real arithmetic alone, into what
--  Prunt.Motion_Planner.Trajectory.Solvers states of the result.
--
--  The parameters have no dimension, so that quantities of any dimension can be passed.

with Prunt.Motion_Planner.Trajectory.Elementary;        use Prunt.Motion_Planner.Trajectory.Elementary;
with Prunt.Motion_Planner.Trajectory.Rounding.Relative; use Prunt.Motion_Planner.Trajectory.Rounding.Relative;

package Prunt.Motion_Planner.Trajectory.Solvers.Accuracy
  with SPARK_Mode => On, Ghost, Always_Terminates
is
   use Prunt.Motion_Planner.Reals;

   subtype Raw is Dimensioned_Float;
   --  A value of any dimension.

   subtype Real is Prunt.Motion_Planner.Reals.Real;

   Limit_Min : constant Raw := 2#1.0#E-80;
   Limit_Max : constant Raw := 2#1.0#E80;
   --  The limits of the proved domain of the solvers.

   Minuend_Min : constant Raw := 2#1.0#E-400;
   Operand_Top : constant Raw := 2#1.0#E200;
   --  The least value which a remainder is taken from, and the largest operand of one.

   function In_Domain (X : Raw) return Boolean
   is (X >= Limit_Min and then X <= Limit_Max);

   N_1     : constant := 1.0 * Unit;
   N_2     : constant := 2.0 * Unit;
   N_3     : constant := 3.0 * Unit;
   N_4     : constant := 4.0 * Unit;
   N_6     : constant := 6.0 * Unit;
   N_8     : constant := 8.0 * Unit;
   N_10    : constant := 10.0 * Unit;
   N_12    : constant := 12.0 * Unit;
   N_16    : constant := 16.0 * Unit;
   N_20    : constant := 20.0 * Unit;
   N_24    : constant := 24.0 * Unit;
   N_32    : constant := 32.0 * Unit;
   N_48    : constant := 48.0 * Unit;
   N_64    : constant := 64.0 * Unit;
   N_128   : constant := 128.0 * Unit;
   N_256   : constant := 256.0 * Unit;
   N_512   : constant := 512.0 * Unit;
   N_1024  : constant := 1024.0 * Unit;
   N_2048  : constant := 2048.0 * Unit;
   N_4096  : constant := 4096.0 * Unit;
   N_8192  : constant := 8192.0 * Unit;
   N_16384 : constant := 16384.0 * Unit;
   --  Relative bounds of so many times Relative.Unit.

   Cube_Tolerance : constant := 2.0**(-34);
   --  The relative bound of the cube of a computed cube root: Relative.Cubed_Step and two roundings.

   Cubic_Tolerance : constant := 2.0**(-34) + N_4096;
   --  The relative bound of a first phase time which is a cube root, against the time which a limit allows.

   Twice_Cubic_Tolerance : constant := 2.0**(-33) + N_16384;
   --  The relative bound of the jerk of such a ramp, which has that phase time twice.

   Root_Floor : constant := 2.0**(-90);
   --  A square root of a quotient of limits, and the root of the snap-limited regions, are at least this.

   Cube_Floor : constant := 2.0**(-60);
   --  A cube root of a quotient of limits is at least this.

   Quotient_Floor : constant := 2.0**(-161);
   Quotient_Top   : constant := 2.0**161;
   --  A quotient of two limits is between these.

   ------------------------------------------------------------------------------------------------------------------
   --  Facts about one computed value
   ------------------------------------------------------------------------------------------------------------------

   function Quotient_Fact (Q, N, D : Raw) return Boolean
   is (Q >= 0.0
       and then About (R (D) * R (Q), R (N), N_1)
       and then R (Q) >= Quotient_Floor
       and then R (Q) <= Quotient_Top);
   --  Q is the quotient of the limits N and D: times D it is N, to within one rounding.

   function Remainder_Fact (X : Raw; P, Q, U : Real) return Boolean
   is ((if X >= 0.0 then abs (R (X) - (P - Q)) <= U * P and then Q <= P + U * P)
       and then (if X <= 0.0 then P <= Q + U * Q));
   --  X is what is left of the exact value P after the exact value Q is taken from it, as a solver computes it: if
   --  X is not negative it is that remainder, to within the fraction U of P, and if it is not positive then P does
   --  not exceed Q, to within the fraction U. This is what the sign of a phase time which a solver tests means.

   procedure Lemma_Quotient (Q, N, D : Raw)
   with Pre => In_Domain (N) and then In_Domain (D) and then Q = N / D, Post => Quotient_Fact (Q, N, D);

   procedure Lemma_Remainder_1 (X, P, Q : Raw)
   with
     Pre  => P >= Minuend_Min and then P <= Operand_Top and then Q >= 0.0 and then Q <= Operand_Top and then X = P - Q,
     Post => Remainder_Fact (X, R (P), R (Q), N_4);

   procedure Lemma_Remainder_2 (X, P, Q1, Q2 : Raw)
   with
     Pre  =>
       P >= Minuend_Min
       and then P <= Operand_Top
       and then Q1 >= 0.0
       and then Q1 <= Operand_Top
       and then Q2 >= 0.0
       and then Q2 <= Operand_Top
       and then X = P - Q1 - Q2,
     Post => Remainder_Fact (X, R (P), R (Q1) + R (Q2), N_4);

   procedure Lemma_Quotient_Bounds (Q, N, D : Raw)
   with Pre => Quotient_Fact (Q, N, D), Post => Q >= Minuend_Min and then Q <= Operand_Top;
   --  A quotient of limits may be a minuend or a subtrahend of the lemmas above.

   procedure Lemma_Remainder_Sign (X : Raw; P, Q, U : Real)
   with
     Pre  => Remainder_Fact (X, P, Q, U) and then P >= 0.0 and then Q >= 0.0,
     Post => (if X >= 0.0 then At_Most (Q, P, U)) and then (if X <= 0.0 then At_Most (P, Q, U));
   --  What the sign of a remainder says of the two exact values, which is all that is left to say of a remainder
   --  whose variable a solver goes on to use for something else.

   procedure Lemma_Remainder_Rebase (X : Raw; P, Q, Q2, U, V, W : Real)
   with
     Pre  =>
       Remainder_Fact (X, P, Q, U)
       and then About (Q, Q2, V)
       and then P >= 0.0
       and then U >= 0.0
       and then U <= Relative_Max
       and then V >= 0.0
       and then V <= Relative_Max
       and then W >= U + 2.0 * V + 2.0 * Second_Order,
     Post => Remainder_Fact (X, P, Q2, W);
   --  The exact value taken away may be replaced by one which it is near.

   Doubled_Top : constant Raw := 2#1.0#E198;

   procedure Lemma_Remainder_Twice (X, P, T : Raw)
   with
     Pre  =>
       P >= Minuend_Min
       and then P <= Operand_Top
       and then T >= Minuend_Min
       and then T <= Doubled_Top
       and then X = P - 2.0 * T,
     Post => Remainder_Fact (X, R (P), 2.0 * R (T), N_8);

   ------------------------------------------------------------------------------------------------------------------
   --  The roots of the regions, and the speed above which snap is reached
   ------------------------------------------------------------------------------------------------------------------

   function Snap_Root_Argument_OK (A, S, C : Raw) return Boolean
   is (S**2 >= 0.0
       and then S**2 <= 2#1.0#E160
       and then 0.25 * S**2 >= 0.0
       and then 0.25 * S**2 <= 2#1.0#E158
       and then C**2 >= 2#1.0#E-160
       and then 0.25 * S**2 / C**2 >= 0.0
       and then 0.25 * S**2 / C**2 <= 2#1.0#E318
       and then A / S >= 0.0
       and then A / S <= 2#1.0#E160
       and then 0.25 * S**2 / C**2 + A / S >= 0.0
       and then 0.25 * S**2 / C**2 + A / S <= 2#1.0#E319)
   with Pre => In_Domain (A) and then In_Domain (S) and then In_Domain (C);
   --  The bounds of the argument of the square root of the snap-limited regions, operation by operation, as
   --  the solvers establish them before they compute it.

   function Snap_Root_Fact (SR, Am, Sm, Cm : Raw) return Boolean
   is (SR >= 0.0
       and then R (SR) >= Root_Floor
       and then
         About
           (R (SR) * R (SR) * (R (Cm) * R (Cm) * R (Sm)),
            0.25 * (R (Sm) * R (Sm) * R (Sm)) + R (Am) * (R (Cm) * R (Cm)),
            N_128));
   --  SR is the root of the snap-limited regions: its square is Sm**2 / (4 * Cm**2) + Am / Sm, which is stated
   --  here times Cm**2 * Sm so that no quotient is needed.

   procedure Lemma_Snap_Root (SR, Am, Sm, Cm : Raw)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Snap_Root_Argument_OK (Am, Sm, Cm)
       and then SR = Dimensionless_Math.Sqrt (0.25 * Sm**2 / Cm**2 + Am / Sm),
     Post => Snap_Root_Fact (SR, Am, Sm, Cm) and then SR >= Minuend_Min;

   function Scaled_Quotient_OK (S, C : Raw) return Boolean
   is (1.5 * S >= 0.0 and then 1.5 * S <= 2#1.0#E81 and then 1.5 * S / C >= 0.0 and then 1.5 * S / C <= 2#1.0#E161)
   with Pre => In_Domain (S) and then In_Domain (C);

   procedure Lemma_Scaled_Remainder (X, SR, Sm, Cm, SC : Raw)
   with
     Pre  =>
       In_Domain (Sm)
       and then In_Domain (Cm)
       and then Quotient_Fact (SC, Sm, Cm)
       and then SR >= Minuend_Min
       and then SR <= Operand_Top
       and then Scaled_Quotient_OK (Sm, Cm)
       and then X = SR - 1.5 * Sm / Cm,
     Post => Remainder_Fact (X, R (SR), 1.5 * R (SC), N_20);
   --  The second phase time of Region_2: the root less one and a half times the first phase time.

   function Cube_Fact (T, Am, Cm : Raw) return Boolean
   is (T >= 0.0
       and then R (T) >= Cube_Floor
       and then About (2.0 * R (Cm) * (R (T) * R (T) * R (T)), R (Am), Cube_Tolerance));
   --  T is the first phase time of the crackle-only regions: twice the crackle times its cube is the
   --  acceleration limit.

   function Crackle_Root_Argument_OK (A, C : Raw) return Boolean
   is (0.5 * A >= 0.0 and then 0.5 * A <= 2#1.0#E80 and then 0.5 * A / C >= 0.0 and then 0.5 * A / C <= 2#1.0#E160)
   with Pre => In_Domain (A) and then In_Domain (C);

   procedure Lemma_Crackle_Root (T, Am, Cm : Raw)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Cm)
       and then Crackle_Root_Argument_OK (Am, Cm)
       and then T = Cube_Root ((0.5 * Am / Cm) / 1.0) * 1.0,
     Post => Cube_Fact (T, Am, Cm);

   function Square_Fact (T, X : Raw) return Boolean
   is (T >= 0.0 and then R (T) >= Root_Floor and then About (R (T) * R (T), R (X), N_64));
   --  T is the square root of X.

   procedure Lemma_Jerk_Root (T, JC, Jm, Cm : Raw)
   with
     Pre  => Quotient_Fact (JC, Jm, Cm) and then T = Dimensionless_Math.Sqrt (JC),
     Post => Square_Fact (T, JC) and then T >= Minuend_Min and then T <= Doubled_Top;

   function Threshold_Argument_OK (S, C : Raw) return Boolean
   is (S**2 >= 0.0
       and then S**2 <= 2#1.0#E160
       and then (S**2)**2 >= 0.0
       and then (S**2)**2 <= 2#1.0#E320
       and then 8.0 * (S**2)**2 >= 0.0
       and then 8.0 * (S**2)**2 <= 2#1.0#E323
       and then C**3 >= 2#1.0#E-240)
   with Pre => In_Domain (S) and then In_Domain (C);

   function Threshold_Fact (SOA, Cm, SC : Raw) return Boolean
   is (About (R (SOA), 8.0 * (R (Cm) * R (SC)) * R (SC) * R (SC) * R (SC), N_32));
   --  SOA is the change of speed of the ramp of four equal phases of SC.

   procedure Lemma_Threshold (SOA, Sm, Cm, SC : Raw)
   with
     Pre  =>
       In_Domain (Sm)
       and then In_Domain (Cm)
       and then Quotient_Fact (SC, Sm, Cm)
       and then Threshold_Argument_OK (Sm, Cm)
       and then SOA = 8.0 * (Sm**2)**2 / Cm**3,
     Post => Threshold_Fact (SOA, Cm, SC);

   ------------------------------------------------------------------------------------------------------------------
   --  The saturated ramp of each region
   ------------------------------------------------------------------------------------------------------------------

   function Region_1_Is_Sound (Am, Jm, Sm, Cm, JS, SC, AJ, T2, T3 : Raw) return Boolean
   is (R (SC) >= Quotient_Floor
       and then R (T2) >= 0.0
       and then R (T3) >= 0.0
       and then About (R (Cm) * R (SC), R (Sm), N_32)
       and then About (R (SC) + R (T2), R (JS), N_32)
       and then About (R (Cm) * R (SC) * (R (SC) + R (T2)), R (Jm), N_32)
       and then About (2.0 * R (SC) + R (T2) + R (T3), R (AJ), N_32)
       and then About (R (Cm) * R (SC) * (R (SC) + R (T2)) * (2.0 * R (SC) + R (T2) + R (T3)), R (Am), N_32));
   --  The ramp of phase times SC, T2 and T3 reaches the snap, jerk and acceleration limits, and its spans are the
   --  quotients of the limits.

   procedure Lemma_Region_1 (Am, Jm, Sm, Cm, JS, SC, AJ, T2, T3 : Raw)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Jm)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Quotient_Fact (JS, Jm, Sm)
       and then Quotient_Fact (SC, Sm, Cm)
       and then Quotient_Fact (AJ, Am, Jm)
       and then Remainder_Fact (T2, R (JS), R (SC), N_4)
       and then T2 > 0.0
       and then Remainder_Fact (T3, R (AJ), R (JS) + R (SC), N_4)
       and then T3 >= 0.0,
     Post => Region_1_Is_Sound (Am, Jm, Sm, Cm, JS, SC, AJ, T2, T3);

   function Region_2_Is_Sound (Am, Jm, Sm, Cm, SC, SR, T2 : Raw) return Boolean
   is (R (SC) >= Quotient_Floor
       and then R (T2) >= 0.0
       and then About (R (Cm) * R (SC), R (Sm), N_4096)
       and then At_Most (R (Cm) * R (SC) * (R (SC) + R (T2)), R (Jm), N_4096)
       and then About (R (Cm) * R (SC) * (R (SC) + R (T2)) * (2.0 * R (SC) + R (T2)), R (Am), N_4096)
       and then About (2.0 * (R (SC) + (R (SC) + R (T2))), R (SC) + 2.0 * R (SR), N_4096));
   --  The ramp of phase times SC and T2 reaches the snap and acceleration limits and is within the jerk limit,
   --  and twice the sum of its first two spans is SC and twice the root.

   procedure Lemma_Region_2 (Am, Jm, Sm, Cm, JS, SC, AJ, SR, T2 : Raw)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Jm)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Quotient_Fact (JS, Jm, Sm)
       and then Quotient_Fact (SC, Sm, Cm)
       and then Quotient_Fact (AJ, Am, Jm)
       and then At_Most (R (SC), R (JS), N_4)
       and then At_Most (R (AJ), R (JS) + R (SC), N_4)
       and then Snap_Root_Fact (SR, Am, Sm, Cm)
       and then Remainder_Fact (T2, R (SR), 1.5 * R (SC), N_20)
       and then T2 >= 0.0,
     Post => Region_2_Is_Sound (Am, Jm, Sm, Cm, SC, SR, T2);

   function Region_3_Is_Sound (Am, Jm, Sm, Cm, T1 : Raw) return Boolean
   is (R (T1) >= Cube_Floor
       and then At_Most (R (Cm) * R (T1), R (Sm), Cubic_Tolerance)
       and then At_Most (R (Cm) * R (T1) * R (T1), R (Jm), Twice_Cubic_Tolerance)
       and then About (2.0 * R (Cm) * (R (T1) * R (T1) * R (T1)), R (Am), Cube_Tolerance));
   --  The ramp of the single phase time T1 reaches the acceleration limit and is within the snap and jerk
   --  limits. Region_5 is the same.

   procedure Lemma_Region_3 (Am, Jm, Sm, Cm, JS, SC, SR, T1 : Raw)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Jm)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Quotient_Fact (JS, Jm, Sm)
       and then Quotient_Fact (SC, Sm, Cm)
       and then At_Most (R (SC), R (JS), N_4)
       and then Snap_Root_Fact (SR, Am, Sm, Cm)
       and then At_Most (R (SR), 1.5 * R (SC), N_20)
       and then Cube_Fact (T1, Am, Cm),
     Post => Region_3_Is_Sound (Am, Jm, Sm, Cm, T1);

   function Region_4_Is_Sound (Am, Jm, Sm, Cm, AJ, JC, T1, T3 : Raw) return Boolean
   is (R (T1) >= Root_Floor
       and then R (T3) >= 0.0
       and then At_Most (R (Cm) * R (T1), R (Sm), N_4096)
       and then About (R (Cm) * R (T1) * R (T1), R (Jm), N_4096)
       and then About (2.0 * R (T1) + R (T3), R (AJ), N_4096)
       and then About (R (Cm) * R (T1) * R (T1) * (2.0 * R (T1) + R (T3)), R (Am), N_4096)
       and then About (R (T1) * R (T1), R (JC), N_4096));
   --  The ramp of phase times T1 and T3 (and no second) reaches the jerk and acceleration limits and is within
   --  the snap limit.

   procedure Lemma_Region_4 (Am, Jm, Sm, Cm, JS, SC, AJ, JC, T1, T3 : Raw)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Jm)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Quotient_Fact (JS, Jm, Sm)
       and then Quotient_Fact (SC, Sm, Cm)
       and then Quotient_Fact (AJ, Am, Jm)
       and then Quotient_Fact (JC, Jm, Cm)
       and then At_Most (R (JS), R (SC), N_4)
       and then Square_Fact (T1, JC)
       and then Remainder_Fact (T3, R (AJ), 2.0 * R (T1), N_8)
       and then T3 > 0.0,
     Post => Region_4_Is_Sound (Am, Jm, Sm, Cm, AJ, JC, T1, T3);

   procedure Lemma_Region_5 (Am, Jm, Sm, Cm, JS, SC, AJ, JC, Q4, T1 : Raw)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Jm)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Quotient_Fact (JS, Jm, Sm)
       and then Quotient_Fact (SC, Sm, Cm)
       and then Quotient_Fact (AJ, Am, Jm)
       and then Quotient_Fact (JC, Jm, Cm)
       and then At_Most (R (JS), R (SC), N_4)
       and then Square_Fact (Q4, JC)
       and then At_Most (R (AJ), 2.0 * R (Q4), N_8)
       and then Cube_Fact (T1, Am, Cm),
     Post => Region_3_Is_Sound (Am, Jm, Sm, Cm, T1);
   --  Q4 is the square root which the test for Region_4 used.

   ------------------------------------------------------------------------------------------------------------------
   --  The facts as the solvers keep them
   --
   --  The same facts with floating-point values only as their arguments, so that a solver names no real number,
   --  and what Prepare_Delta_V concludes from them for the solver which it returns. Prepare_Delta_V hides the
   --  expressions of all of these, and passes the facts from the lemma which gives one to the lemma which takes
   --  it.
   ------------------------------------------------------------------------------------------------------------------

   function Is_Minuend (X : Raw) return Boolean
   is (X >= Minuend_Min and then X <= Operand_Top);

   function Is_Subtrahend (X : Raw) return Boolean
   is (X >= 0.0 and then X <= Operand_Top);

   function Is_Doubled (X : Raw) return Boolean
   is (X >= Minuend_Min and then X <= Doubled_Top);

   function Gap_Fact (X, P, Q : Raw) return Boolean
   is (Remainder_Fact (X, R (P), R (Q), N_4));
   --  X is P less Q.

   function Below (X, Y : Raw) return Boolean
   is (At_Most (R (X), R (Y), N_4));
   --  X does not exceed Y, to within the rounding of a difference.

   function Gap_Fact_2 (X, P, Q1, Q2 : Raw) return Boolean
   is (Remainder_Fact (X, R (P), R (Q1) + R (Q2), N_4));
   --  X is P less Q1 and Q2.

   function Below_Sum (X, Y1, Y2 : Raw) return Boolean
   is (At_Most (R (X), R (Y1) + R (Y2), N_4));

   function Scaled_Gap_Fact (X, SR, SC : Raw) return Boolean
   is (Remainder_Fact (X, R (SR), 1.5 * R (SC), N_20));
   --  X is the root SR less one and a half times the first phase time SC.

   function Below_Scaled (SR, SC : Raw) return Boolean
   is (At_Most (R (SR), 1.5 * R (SC), N_20));

   function Twice_Gap_Fact (X, P, T : Raw) return Boolean
   is (Remainder_Fact (X, R (P), 2.0 * R (T), N_8));
   --  X is P less twice T.

   function Below_Twice (P, T : Raw) return Boolean
   is (At_Most (R (P), 2.0 * R (T), N_8));

   procedure Lemma_Quotient_Is_Operand (Q, N, D : Raw)
   with Pre => Quotient_Fact (Q, N, D), Post => Is_Minuend (Q) and then Is_Subtrahend (Q);

   procedure Lemma_Gap (X, P, Q : Raw)
   with
     Pre  => Is_Minuend (P) and then Is_Subtrahend (Q) and then X = P - Q,
     Post => Gap_Fact (X, P, Q) and then (if X >= 0.0 then Below (Q, P)) and then (if X <= 0.0 then Below (P, Q));

   procedure Lemma_Gap_2 (X, P, Q1, Q2 : Raw)
   with
     Pre  => Is_Minuend (P) and then Is_Subtrahend (Q1) and then Is_Subtrahend (Q2) and then X = P - Q1 - Q2,
     Post => Gap_Fact_2 (X, P, Q1, Q2) and then (if X <= 0.0 then Below_Sum (P, Q1, Q2));

   procedure Lemma_Scaled_Gap (X, SR, Sm, Cm, SC : Raw)
   with
     Pre  =>
       In_Domain (Sm)
       and then In_Domain (Cm)
       and then Quotient_Fact (SC, Sm, Cm)
       and then Is_Minuend (SR)
       and then Scaled_Quotient_OK (Sm, Cm)
       and then X = SR - 1.5 * Sm / Cm,
     Post => Scaled_Gap_Fact (X, SR, SC) and then (if X <= 0.0 then Below_Scaled (SR, SC));

   procedure Lemma_Twice_Gap (X, P, T : Raw)
   with
     Pre  => Is_Minuend (P) and then Is_Doubled (T) and then X = P - 2.0 * T,
     Post => Twice_Gap_Fact (X, P, T) and then (if X <= 0.0 then Below_Twice (P, T));

   procedure Lemma_Snap_Root_Is_Minuend (SR, Am, Sm, Cm : Raw)
   with Pre => Snap_Root_Fact (SR, Am, Sm, Cm) and then SR <= Operand_Top, Post => Is_Minuend (SR);

   procedure Lemma_Square_Root_Is_Doubled (T, X : Raw)
   with Pre => Square_Fact (T, X) and then T <= Doubled_Top, Post => Is_Doubled (T);

   function Same (X, Y : Raw) return Boolean
   is (X = Y);
   --  Equality of two values whatever their dimensions.

   function Holds_Limits (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Cm : Crackle) return Boolean
   is (Same (Solver.Am, Am) and then Same (Solver.Jm, Jm) and then Same (Solver.Cm, Cm));

   function Prepared_Fact
     (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Holds_Limits (Solver, Am, Jm, Cm)
       and then Core_Is_Consistent (Solver)
       and then Snap_Is_Within (Solver, Sm)
       and then
         (if Solver.Region in Region_1 | Region_2
          then Solver.T1 = Solver.Snap_Over_Crackle and then Quotient_Fact (Solver.Snap_Over_Crackle, Sm, Cm)));
   --  What Prepare_Delta_V has established of its solver once the region is chosen, before the speed above
   --  which snap is reached is computed.

   function Is_Sound (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Holds_Limits (Solver, Am, Jm, Cm) and then Solver_Is_Consistent (Solver) and then Snap_Is_Within (Solver, Sm));
   --  What it has established of the solver which it returns.

   procedure Lemma_Prepared_1 (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Jm)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Solver.Region = Region_1
       and then Holds_Limits (Solver, Am, Jm, Cm)
       and then Solver.T1 = Solver.Snap_Over_Crackle
       and then Quotient_Fact (Solver.Jerk_Over_Snap, Jm, Sm)
       and then Quotient_Fact (Solver.Snap_Over_Crackle, Sm, Cm)
       and then Quotient_Fact (Solver.Accel_Over_Jerk, Am, Jm)
       and then Gap_Fact (Solver.T2, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
       and then Solver.T2 > 0.0 * s
       and then Gap_Fact_2 (Solver.T3, Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
       and then Solver.T3 >= 0.0 * s,
     Post => Prepared_Fact (Solver, Am, Jm, Sm, Cm);

   procedure Lemma_Prepared_2 (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Jm)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Solver.Region = Region_2
       and then Holds_Limits (Solver, Am, Jm, Cm)
       and then Solver.T1 = Solver.Snap_Over_Crackle
       and then Solver.T3 = 0.0 * s
       and then Quotient_Fact (Solver.Jerk_Over_Snap, Jm, Sm)
       and then Quotient_Fact (Solver.Snap_Over_Crackle, Sm, Cm)
       and then Quotient_Fact (Solver.Accel_Over_Jerk, Am, Jm)
       and then Below (Solver.Snap_Over_Crackle, Solver.Jerk_Over_Snap)
       and then Below_Sum (Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
       and then Snap_Root_Fact (Solver.Snap_Root, Am, Sm, Cm)
       and then Scaled_Gap_Fact (Solver.T2, Solver.Snap_Root, Solver.Snap_Over_Crackle)
       and then Solver.T2 >= 0.0 * s,
     Post => Prepared_Fact (Solver, Am, Jm, Sm, Cm);

   procedure Lemma_Prepared_3 (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Jm)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Solver.Region = Region_3
       and then Holds_Limits (Solver, Am, Jm, Cm)
       and then Solver.T2 = 0.0 * s
       and then Solver.T3 = 0.0 * s
       and then Quotient_Fact (Solver.Jerk_Over_Snap, Jm, Sm)
       and then Quotient_Fact (Solver.Snap_Over_Crackle, Sm, Cm)
       and then Below (Solver.Snap_Over_Crackle, Solver.Jerk_Over_Snap)
       and then Snap_Root_Fact (Solver.Snap_Root, Am, Sm, Cm)
       and then Below_Scaled (Solver.Snap_Root, Solver.Snap_Over_Crackle)
       and then Cube_Fact (Solver.T1, Am, Cm),
     Post => Prepared_Fact (Solver, Am, Jm, Sm, Cm);

   procedure Lemma_Prepared_4
     (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle; Q4 : Raw)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Jm)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Solver.Region = Region_4
       and then Holds_Limits (Solver, Am, Jm, Cm)
       and then Solver.T2 = 0.0 * s
       and then Same (Solver.T1, Q4)
       and then Quotient_Fact (Solver.Jerk_Over_Snap, Jm, Sm)
       and then Quotient_Fact (Solver.Snap_Over_Crackle, Sm, Cm)
       and then Quotient_Fact (Solver.Accel_Over_Jerk, Am, Jm)
       and then Quotient_Fact (Solver.Jerk_Over_Crackle, Jm, Cm)
       and then Below (Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
       and then Square_Fact (Q4, Solver.Jerk_Over_Crackle)
       and then Twice_Gap_Fact (Solver.T3, Solver.Accel_Over_Jerk, Q4)
       and then Solver.T3 > 0.0 * s,
     Post => Prepared_Fact (Solver, Am, Jm, Sm, Cm);
   --  Q4 is the first phase time as it was computed, which the solver holds.

   procedure Lemma_Prepared_5
     (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle; Q4 : Raw)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Jm)
       and then In_Domain (Sm)
       and then In_Domain (Cm)
       and then Solver.Region = Region_5
       and then Holds_Limits (Solver, Am, Jm, Cm)
       and then Solver.T2 = 0.0 * s
       and then Solver.T3 = 0.0 * s
       and then Quotient_Fact (Solver.Jerk_Over_Snap, Jm, Sm)
       and then Quotient_Fact (Solver.Snap_Over_Crackle, Sm, Cm)
       and then Quotient_Fact (Solver.Accel_Over_Jerk, Am, Jm)
       and then Quotient_Fact (Solver.Jerk_Over_Crackle, Jm, Cm)
       and then Below (Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
       and then Square_Fact (Q4, Solver.Jerk_Over_Crackle)
       and then Below_Twice (Solver.Accel_Over_Jerk, Q4)
       and then Cube_Fact (Solver.T1, Am, Cm),
     Post => Prepared_Fact (Solver, Am, Jm, Sm, Cm);
   --  Q4 is the square root which the test for Region_4 used.

   function Same_But_Threshold (Before, After : Delta_V_Solver) return Boolean
   is (After.Region = Before.Region
       and then After.Am = Before.Am
       and then After.Jm = Before.Jm
       and then After.Cm = Before.Cm
       and then After.T1 = Before.T1
       and then After.T2 = Before.T2
       and then After.T3 = Before.T3
       and then After.Accel_Over_Jerk = Before.Accel_Over_Jerk
       and then After.Jerk_Over_Snap = Before.Jerk_Over_Snap
       and then After.Snap_Over_Crackle = Before.Snap_Over_Crackle
       and then After.Snap_Root = Before.Snap_Root
       and then After.Jerk_Over_Crackle = Before.Jerk_Over_Crackle);
   --  After is Before with another speed above which snap is reached.

   procedure Lemma_Threshold_Value
     (SOA : Velocity; Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Pre  =>
       In_Domain (Sm)
       and then In_Domain (Cm)
       and then Prepared_Fact (Solver, Am, Jm, Sm, Cm)
       and then Solver.Region in Region_1 | Region_2
       and then Threshold_Argument_OK (Sm, Cm)
       and then SOA = 8.0 * (Sm**2)**2 / Cm**3,
     Post => Threshold_Fact (SOA, Cm, Solver.Snap_Over_Crackle);

   procedure Lemma_Threshold_Set
     (Before, After : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Pre  =>
       Prepared_Fact (Before, Am, Jm, Sm, Cm)
       and then Before.Region in Region_1 | Region_2
       and then Same_But_Threshold (Before, After)
       and then Threshold_Fact (After.Snap_Only_Above, Cm, Before.Snap_Over_Crackle),
     Post => Is_Sound (After, Am, Jm, Sm, Cm);

   procedure Lemma_No_Threshold (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Pre  => Prepared_Fact (Solver, Am, Jm, Sm, Cm) and then Solver.Region in Region_3 | Region_4 | Region_5,
     Post => Is_Sound (Solver, Am, Jm, Sm, Cm);

   procedure Lemma_Is_Sound (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Pre  => Is_Sound (Solver, Am, Jm, Sm, Cm),
     Post =>
       Same (Solver.Am, Am)
       and then Same (Solver.Jm, Jm)
       and then Same (Solver.Cm, Cm)
       and then Solver_Is_Consistent (Solver)
       and then Snap_Is_Within (Solver, Sm);
   --  What Is_Sound says, for a caller which hides it.

   ------------------------------------------------------------------------------------------------------------------
   --  A solve: the values which it computes
   --
   --  The last phase time of a full case is the time the speed change takes at the acceleration limit, less the
   --  spans of the saturated ramp; the third phase time of the jerk-limited cases is the root of a quadratic; and
   --  the single phase time of the crackle-limited case is a fourth root. Each fact below says what the value, or
   --  its sign, means in exact arithmetic, with the quotient or root which it was computed through eliminated.
   ------------------------------------------------------------------------------------------------------------------

   function Speed_Remainder_Fact (X : Raw; V, A, Q, U : Real) return Boolean
   is ((if X >= 0.0 then abs (R (X) * A - (V - A * Q)) <= U * V and then A * Q <= V + U * V)
       and then (if X <= 0.0 then V <= A * Q + U * (A * Q)));
   --  X is the time which the speed change V takes at the acceleration A, less the time Q: if X is not negative
   --  then X * A is V - A * Q to within the fraction U of V, and if it is not positive then V does not exceed
   --  A * Q, to within the fraction U.

   function Gap_OK (P, Q1, Q2 : Raw) return Boolean
   is (P - Q1 >= -2#1.0#E200 and then P - Q1 <= P and then P - Q1 - Q2 >= -2#1.0#E201 and then P - Q1 - Q2 <= P)
   with Pre => Is_Subtrahend (P) and then Is_Subtrahend (Q1) and then Is_Subtrahend (Q2);
   --  The bounds of the differences on the way to a last phase time, as the solvers establish them.

   Speed_Top : constant Raw := 2#1.0#E80;
   --  The largest speed change of the proved domain.

   Span_Sum_Floor : constant := 2.0**(-200);
   --  The time which is taken away is at least this.

   Radicand_Floor : constant := 2.0**(-410);
   --  The part of a radicand which does not depend on the speed change is at least this.

   procedure Lemma_Speed_Gap (X, P, Vd, Am, Q1, Q2, Q3 : Raw)
   with
     Pre  =>
       Vd >= 0.0
       and then Vd <= Speed_Top
       and then In_Domain (Am)
       and then P = Vd / Am
       and then Is_Subtrahend (P)
       and then Is_Subtrahend (Q1)
       and then Is_Subtrahend (Q2)
       and then Is_Subtrahend (Q3)
       and then Gap_OK (P, Q1, Q2)
       and then R (Q1) + R (Q2) + R (Q3) >= Span_Sum_Floor
       and then X = P - Q1 - Q2 - Q3,
     Post => Speed_Remainder_Fact (X, R (Vd), R (Am), R (Q1) + R (Q2) + R (Q3), N_16);

   procedure Lemma_Speed_Rebase (X : Raw; V, A, Q, Q2, U, W, U2 : Real)
   with
     Pre  =>
       Speed_Remainder_Fact (X, V, A, Q, U)
       and then About (Q, Q2, W)
       and then V >= 0.0
       and then A >= 0.0
       and then U >= 0.0
       and then U <= Relative_Max
       and then W >= 0.0
       and then W <= Relative_Max
       and then U2 >= U + 2.0 * W + 2.0 * Second_Order,
     Post => Speed_Remainder_Fact (X, V, A, Q2, U2);

   function Jerk_Time_Argument_OK (V, J, A, B : Raw) return Boolean
   is (A + B >= 0.0
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
       and then 1.5 * (A + B) <= 2#1.0#E165)
   with
     Pre =>
       V >= 0.0
       and then V <= Speed_Top
       and then J >= Limit_Min
       and then A >= 0.0
       and then A <= 2#1.0#E163
       and then B >= 0.0
       and then B <= 2#1.0#E163;
   --  The bounds of the third phase time where jerk is the highest limit reached in Region_1, operation by
   --  operation, as the solver establishes them.

   function Jerk_Time_Fact_1 (T3 : Raw; V, J, G : Real) return Boolean
   is ((if T3 > 0.0 then About ((2.0 * R (T3) + 3.0 * G) * (2.0 * R (T3) + 3.0 * G) * J, G * G * J + 4.0 * V, N_1024))
       and then (if T3 <= 0.0 then At_Most (G * G * J + 4.0 * V, 9.0 * (G * G) * J, N_1024)));
   --  T3 is half the root of G**2 + 4 * V / J less one and a half times G: where it is positive, twice T3 and
   --  three times G is that root, and where it is not, the root is at most three times G. Both are stated with
   --  the root squared, times J.

   procedure Lemma_Jerk_Time (T3, Vd, Jm, JS, SC : Raw)
   with
     Pre  =>
       Vd >= 0.0
       and then Vd <= Speed_Top
       and then In_Domain (Jm)
       and then JS >= 0.0
       and then JS <= 2#1.0#E163
       and then SC >= 0.0
       and then SC <= 2#1.0#E163
       and then R (JS) + R (SC) >= Span_Sum_Floor
       and then Jerk_Time_Argument_OK (Vd, Jm, JS, SC)
       and then T3 = 0.5 * Dimensionless_Math.Sqrt ((JS + SC)**2 + 4.0 * Vd / Jm) - 1.5 * (JS + SC),
     Post => Jerk_Time_Fact_1 (T3, R (Vd), R (Jm), R (JS) + R (SC));

   function Jerk_Only_Argument_OK (V, J, Q, T : Raw) return Boolean
   is (V / J >= 0.0
       and then V / J <= 2#1.0#E160
       and then Q + V / J >= 0.0
       and then Q + V / J <= 2#1.0#E161
       and then Dimensionless_Math.Sqrt (Q + V / J) >= 0.0
       and then Dimensionless_Math.Sqrt (Q + V / J) <= 2#1.0#E81
       and then 3.0 * T >= 0.0
       and then 3.0 * T <= 2#1.0#E165)
   with
     Pre =>
       V >= 0.0
       and then V <= Speed_Top
       and then J >= Limit_Min
       and then Q >= 0.0
       and then Q <= 2#1.0#E160
       and then T >= 0.0
       and then T <= 2#1.0#E163;

   function Jerk_Time_Fact_4 (T3 : Raw; V, J, Gam, T : Real) return Boolean
   is ((if T3 > 0.0 then About ((R (T3) + 3.0 * T) * (R (T3) + 3.0 * T) * J, Gam * J + V, N_1024))
       and then (if T3 <= 0.0 then At_Most (Gam * J + V, 9.0 * (T * T) * J, N_1024)));
   --  T3 is the root of Gam + V / J less three times T: the same, for Region_4.

   procedure Lemma_Jerk_Only_Time (T3, Vd, Jm, JC, T1 : Raw)
   with
     Pre  =>
       Vd >= 0.0
       and then Vd <= Speed_Top
       and then In_Domain (Jm)
       and then JC >= 0.0
       and then JC <= 2#1.0#E160
       and then R (JC) >= Radicand_Floor
       and then T1 >= 0.0
       and then T1 <= 2#1.0#E163
       and then R (T1) >= Span_Sum_Floor
       and then Jerk_Only_Argument_OK (Vd, Jm, JC, T1)
       and then T3 = Dimensionless_Math.Sqrt (JC + Vd / Jm) - 3.0 * T1,
     Post => Jerk_Time_Fact_4 (T3, R (Vd), R (Jm), R (JC), R (T1));

   Tiny_Speed : constant Raw := 2#1.0#E-870;
   --  A speed change below this is too small for its eighth, over the crackle, to be a normal number.

   Tiny_Time : constant := 2.0**(-197);
   --  The phase time of such a speed change is no longer than this.

   function Crackle_Time_Argument_OK (V, C : Raw) return Boolean
   is (0.125 * V >= 0.0
       and then 0.125 * V <= 2#1.0#E80
       and then 0.125 * V / C >= 0.0
       and then 0.125 * V / C <= 2#1.0#E160
       and then Dimensionless_Math.Sqrt (0.125 * V / C) >= 0.0
       and then Dimensionless_Math.Sqrt (0.125 * V / C) <= 2#1.0#E80)
   with Pre => V >= 0.0 and then V <= Speed_Top and then C >= Limit_Min;

   function Crackle_Time_Fact (T, Vd, Cm : Raw) return Boolean
   is (T >= 0.0
       and then
         (if Vd >= Tiny_Speed
          then About (8.0 * R (Cm) * ((R (T) * R (T)) * (R (T) * R (T))), R (Vd), N_512)
          else R (T) <= Tiny_Time));
   --  T is the fourth root of an eighth of the speed change over the crackle: eight times the crackle times its
   --  fourth power is the speed change, unless the speed change is tiny, and then so is T.

   procedure Lemma_Crackle_Time (T, Vd, Cm : Raw)
   with
     Pre  =>
       Vd >= 0.0
       and then Vd <= Speed_Top
       and then In_Domain (Cm)
       and then Crackle_Time_Argument_OK (Vd, Cm)
       and then T = Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)),
     Post => Crackle_Time_Fact (T, Vd, Cm);

   ------------------------------------------------------------------------------------------------------------------
   --  A solve: the facts as the solver keeps them, and what each case concludes
   --
   --  Each fact is conditional on the solver being consistent, which a solve does not know of its solver: the
   --  lemmas which give the facts prove them under that condition, and so does every conclusion.
   ------------------------------------------------------------------------------------------------------------------

   function Full_Span (Solver : Delta_V_Solver) return Real
   is (case Solver.Region is
         when Region_1            =>
           R (Solver.Accel_Over_Jerk) + R (Solver.Jerk_Over_Snap) + R (Solver.Snap_Over_Crackle),
         when Region_2            => R (Solver.Snap_Over_Crackle) + 2.0 * R (Solver.Snap_Root),
         when Region_3 | Region_5 => 4.0 * R (Solver.T1),
         when Region_4            => R (Solver.Accel_Over_Jerk) + 2.0 * R (Solver.T1));
   --  The time which the last phase time of a full case is the speed time less.

   function Last_Time_Fact (T4 : Time; Vd : Velocity; Solver : Delta_V_Solver) return Boolean
   is (if Solver_Is_Consistent (Solver)
       then Speed_Remainder_Fact (T4, R (Vd), R (Solver.Am), Full_Span (Solver), N_64));

   function Jerk_Limited_Fact (T3 : Time; Vd : Velocity; Solver : Delta_V_Solver) return Boolean
   is (if Solver_Is_Consistent (Solver)
       then
         (case Solver.Region is
            when Region_1 =>
              Jerk_Time_Fact_1 (T3, R (Vd), R (Solver.Jm), R (Solver.Jerk_Over_Snap) + R (Solver.Snap_Over_Crackle)),
            when Region_4 => Jerk_Time_Fact_4 (T3, R (Vd), R (Solver.Jm), R (Solver.Jerk_Over_Crackle), R (Solver.T1)),
            when others   => True));

   Bisected_Slack : constant := 2.0**(-45);
   --  How far, relatively, the gain of a bisected ramp can be below the speed change.

   function Snap_Gain_Fact (T2 : Time; Vd : Velocity; Solver : Delta_V_Solver) return Boolean
   is (if Solver_Is_Consistent (Solver)
       then
         (R (T2) = 0.0 or else At_Most (Exact_Gain (R (Solver.Cm), R (Solver.T1), R (T2), 0.0, 0.0), R (Vd), N_64))
         and then
           (if not Is_Capped (T2)
            then Exact_Gain (R (Solver.Cm), R (Solver.T1), R (T2), 0.0, 0.0) >= R (Vd) - Bisected_Slack * R (Vd)));
   --  The second phase time of the snap-limited case is zero, or the ramp gains no more than the speed change;
   --  and unless the bisection stopped at its cap, the ramp gains no less.

   function Gap_1_OK (P, Q1 : Raw) return Boolean
   is (P - Q1 >= -2#1.0#E200 and then P - Q1 <= P)
   with Pre => Is_Subtrahend (P) and then Is_Subtrahend (Q1);

   function Speed_OK (Vd : Velocity; Solver : Delta_V_Solver) return Boolean
   is (Solver_In_Domain (Solver) and then Vd >= No_Speed and then Vd <= Speed_Max);

   procedure Lemma_Last_Time_1 (T4, P : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region = Region_1
       and then P = Vd / Solver.Am
       and then Is_Subtrahend (P)
       and then Gap_OK (P, Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap)
       and then T4 = P - Solver.Accel_Over_Jerk - Solver.Jerk_Over_Snap - Solver.Snap_Over_Crackle,
     Post => Last_Time_Fact (T4, Vd, Solver);

   procedure Lemma_Last_Time_2 (T4, P : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region = Region_2
       and then P = Vd / Solver.Am
       and then Is_Subtrahend (P)
       and then Gap_1_OK (P, Solver.Snap_Over_Crackle)
       and then 2.0 * Solver.Snap_Root >= 0.0 * s
       and then Is_Subtrahend (2.0 * Solver.Snap_Root)
       and then T4 = P - Solver.Snap_Over_Crackle - 2.0 * Solver.Snap_Root,
     Post => Last_Time_Fact (T4, Vd, Solver);

   procedure Lemma_Last_Time_3 (T4, P : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region in Region_3 | Region_5
       and then P = Vd / Solver.Am
       and then Is_Subtrahend (P)
       and then 4.0 * Solver.T1 >= 0.0 * s
       and then Is_Subtrahend (4.0 * Solver.T1)
       and then T4 = P - 4.0 * Solver.T1,
     Post => Last_Time_Fact (T4, Vd, Solver);

   procedure Lemma_Last_Time_4 (T4, P : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region = Region_4
       and then P = Vd / Solver.Am
       and then Is_Subtrahend (P)
       and then Gap_1_OK (P, Solver.Accel_Over_Jerk)
       and then 2.0 * Solver.T1 >= 0.0 * s
       and then Is_Subtrahend (2.0 * Solver.T1)
       and then T4 = P - Solver.Accel_Over_Jerk - 2.0 * Solver.T1,
     Post => Last_Time_Fact (T4, Vd, Solver);

   procedure Lemma_Jerk_Limited_1 (T3 : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region = Region_1
       and then Jerk_Time_Argument_OK (Vd, Solver.Jm, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle)
       and then
         T3
         = 0.5 * Dimensionless_Math.Sqrt ((Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle)**2 + 4.0 * Vd / Solver.Jm)
           - 1.5 * (Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle),
     Post => Jerk_Limited_Fact (T3, Vd, Solver);

   procedure Lemma_Jerk_Limited_4 (T3 : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region = Region_4
       and then Jerk_Only_Argument_OK (Vd, Solver.Jm, Solver.Jerk_Over_Crackle, Solver.T1)
       and then T3 = Dimensionless_Math.Sqrt (Solver.Jerk_Over_Crackle + Vd / Solver.Jm) - 3.0 * Solver.T1,
     Post => Jerk_Limited_Fact (T3, Vd, Solver);

   procedure Lemma_Snap_Gain (Times : Feedrate_Profile_Times; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Times_In_Range (Times)
       and then Times (1) = Solver.T1
       and then Gain_Is_Bisected (Times, Vd, Solver.Cm),
     Post => Snap_Gain_Fact (Times (2), Vd, Solver) and then Times (3) = No_Time and then Times (4) = No_Time;

   procedure Lemma_Case_Full (T4 : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  => Speed_OK (Vd, Solver) and then Last_Time_Fact (T4, Vd, Solver) and then T4 > 0.0 * s,
     Post =>
       (case Solver.Region is
          when Region_1            => Solve_Is_Sound (Solver, Vd, Solver.T1, Solver.T2, Solver.T3, T4),
          when Region_2            => Solve_Is_Sound (Solver, Vd, Solver.T1, Solver.T2, 0.0 * s, T4),
          when Region_3 | Region_5 => Solve_Is_Sound (Solver, Vd, Solver.T1, 0.0 * s, 0.0 * s, T4),
          when Region_4            => Solve_Is_Sound (Solver, Vd, Solver.T1, 0.0 * s, Solver.T3, T4));
   --  Every limit which the region lets a ramp reach is reached.

   procedure Lemma_Case_Jerk_Limited (T3, T4 : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region in Region_1 | Region_4
       and then Last_Time_Fact (T4, Vd, Solver)
       and then not (T4 > 0.0 * s)
       and then Jerk_Limited_Fact (T3, Vd, Solver)
       and then T3 > 0.0 * s,
     Post =>
       (if Solver.Region = Region_1
        then Solve_Is_Sound (Solver, Vd, Solver.T1, Solver.T2, T3, 0.0 * s)
        else Solve_Is_Sound (Solver, Vd, Solver.T1, 0.0 * s, T3, 0.0 * s));
   --  Jerk is the highest limit reached.

   procedure Lemma_Case_Snap_Limited_1 (T2, T3 : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region = Region_1
       and then Jerk_Limited_Fact (T3, Vd, Solver)
       and then not (T3 > 0.0 * s)
       and then Vd > Solver.Snap_Only_Above
       and then T2 >= 0.0 * s
       and then Snap_Gain_Fact (T2, Vd, Solver),
     Post => Solve_Is_Sound (Solver, Vd, Solver.T1, T2, 0.0 * s, 0.0 * s);

   procedure Lemma_Case_Snap_Limited_2 (T2, T4 : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region = Region_2
       and then Last_Time_Fact (T4, Vd, Solver)
       and then not (T4 > 0.0 * s)
       and then Vd > Solver.Snap_Only_Above
       and then T2 >= 0.0 * s
       and then Snap_Gain_Fact (T2, Vd, Solver),
     Post => Solve_Is_Sound (Solver, Vd, Solver.T1, T2, 0.0 * s, 0.0 * s);
   --  Snap is the only limit reached.

   procedure Lemma_Case_Crackle_Limited_Snap (T : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region in Region_1 | Region_2
       and then not (Vd > Solver.Snap_Only_Above)
       and then Crackle_Time_Fact (T, Vd, Solver.Cm),
     Post => Solve_Is_Sound (Solver, Vd, T, 0.0 * s, 0.0 * s, 0.0 * s);

   procedure Lemma_Case_Crackle_Limited_Single (T, T4 : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region in Region_3 | Region_5
       and then Last_Time_Fact (T4, Vd, Solver)
       and then not (T4 > 0.0 * s)
       and then Crackle_Time_Fact (T, Vd, Solver.Cm),
     Post => Solve_Is_Sound (Solver, Vd, T, 0.0 * s, 0.0 * s, 0.0 * s);

   procedure Lemma_Case_Crackle_Limited_Jerk (T, T3 : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region = Region_4
       and then Jerk_Limited_Fact (T3, Vd, Solver)
       and then not (T3 > 0.0 * s)
       and then Crackle_Time_Fact (T, Vd, Solver.Cm),
     Post => Solve_Is_Sound (Solver, Vd, T, 0.0 * s, 0.0 * s, 0.0 * s);
   --  No limit other than crackle is reached.

   procedure Lemma_Snap_Limited_Is_Sound
     (Times : Feedrate_Profile_Times; T : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver.Region in Region_1 | Region_2
       and then
         (if Solver.Region = Region_1 then Jerk_Limited_Fact (T, Vd, Solver) else Last_Time_Fact (T, Vd, Solver))
       and then not (T > 0.0 * s)
       and then Vd > Solver.Snap_Only_Above
       and then Times_In_Range (Times)
       and then Times (1) = Solver.T1
       and then Gain_Is_Bisected (Times, Vd, Solver.Cm),
     Post => Solve_Is_Sound (Solver, Vd, Times (1), Times (2), Times (3), Times (4));
   --  The two snap-limited cases, for the phase times as the bisection returns them. T is the phase time whose
   --  sign was tested last: the third of Region_1, the fourth of Region_2.

   procedure Lemma_Empty_Is_Sound (Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  => Vd = No_Speed,
     Post => (if Solver_In_Domain (Solver) then Solve_Is_Sound (Solver, Vd, 0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s));
   --  The ramp which takes no time, for no change of speed.

   procedure Lemma_Sound_Is_Within_Limits
     (Solver : Delta_V_Solver; Sm : Snap; Vd : Velocity; Times : Feedrate_Profile_Times)
   with
     Pre  =>
       Solver_In_Domain (Solver)
       and then Sm >= Snap_Limit_Min
       and then Sm <= Snap_Limit_Max
       and then Solver_Is_Consistent (Solver)
       and then Snap_Is_Within (Solver, Sm)
       and then Times_In_Range (Times)
       and then Solve_Is_Sound (Solver, Vd, Times (1), Times (2), Times (3), Times (4)),
     Post =>
       Ramp_Is_Within_Limits (Times, Solver.Am, Solver.Jm, Sm, Solver.Cm)
       and then Gain_Is_At_Most (Solver.Cm, Times (1), Times (2), Times (3), Times (4), Vd)
       and then Gain_Is_Reached (Solver.Cm, Times (1), Times (2), Times (3), Times (4), Vd);
   --  The ramp which a solve with a prepared solver returns is within the limits which the solver was prepared
   --  with, at every instant, to Certificates.Certified_Tolerance.

   procedure Lemma_Prepared_Is_Within_Limits
     (Solver : Delta_V_Solver;
      Times  : Feedrate_Profile_Times;
      Vd     : Velocity;
      Am     : Acceleration;
      Jm     : Jerk;
      Sm     : Snap;
      Cm     : Crackle)
   with
     Pre  =>
       Solver_In_Domain (Solver)
       and then Limits_In_Domain (Am, Jm, Sm, Cm)
       and then Solver.Am = Am
       and then Solver.Jm = Jm
       and then Solver.Cm = Cm
       and then Solver_Is_Consistent (Solver)
       and then Snap_Is_Within (Solver, Sm)
       and then Times_In_Range (Times)
       and then Solve_Is_Sound (Solver, Vd, Times (1), Times (2), Times (3), Times (4)),
     Post => Ramp_Is_Sound (Times, Vd, Am, Jm, Sm, Cm);
   --  The same, with the limits themselves in place of the copies which the solver keeps.

   procedure Lemma_Empty_Is_Within_Limits
     (Times : Feedrate_Profile_Times; Vd : Velocity; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Pre  => Times_Are_Zero (Times) and then Vd = No_Speed,
     Post => (if Limits_In_Domain (Am, Jm, Sm, Cm) then Ramp_Is_Sound (Times, Vd, Am, Jm, Sm, Cm));
   --  The ramp which takes no time is within any limits, and changes no speed.

   ------------------------------------------------------------------------------------------------------------------
   --  The distance solver
   --
   --  It forms the saturated ramp of the limits as Prepare_Delta_V does. Its candidates are that ramp without its
   --  last phase, then cut after its second and after its first phase, and the ramp of no time. It bisects one
   --  phase time of the longest candidate which is too short for the distance. The candidate above that one was
   --  not too short, so the bisected ramp travels no further than it, and so the span which the bisected phase
   --  time lengthens is no longer than the saturated one.
   ------------------------------------------------------------------------------------------------------------------

   function Crackle_Root_Argument_2_OK (A, C : Raw) return Boolean
   is (2.0 * C >= 2#1.0#E-80
       and then 2.0 * C <= 2#1.0#E81
       and then A / (2.0 * C) >= 0.0
       and then A / (2.0 * C) <= 2#1.0#E160)
   with Pre => In_Domain (A) and then In_Domain (C);

   procedure Lemma_Crackle_Root_2 (T, Am, Cm : Raw)
   with
     Pre  =>
       In_Domain (Am)
       and then In_Domain (Cm)
       and then Crackle_Root_Argument_2_OK (Am, Cm)
       and then T = Cube_Root ((Am / (2.0 * Cm)) / 1.0) * 1.0,
     Post => Cube_Fact (T, Am, Cm);
   --  The first phase time of Region_5 as the distance solver writes it.

   function Is_Candidate
     (Solver : Delta_V_Solver; Times : Feedrate_Profile_Times; Index : Feedrate_Profile_Times_Index) return Boolean
   is (Times (1) = (if Index >= 2 then Solver.T1 else No_Time)
       and then Times (2) = (if Index >= 3 then Solver.T2 else No_Time)
       and then Times (3) = (if Index >= 4 then Solver.T3 else No_Time)
       and then Times (4) = No_Time);
   --  Times is candidate Index of the distance solver, for the saturated ramp which Solver holds.

   function Is_Moderate (Solver : Delta_V_Solver) return Boolean
   is (Solver.T1 >= No_Time
       and then Solver.T1 <= Rounding_Time_Max
       and then Solver.T2 >= No_Time
       and then Solver.T2 <= Rounding_Time_Max
       and then Solver.T3 >= No_Time
       and then Solver.T3 <= Rounding_Time_Max);
   --  The saturated ramp is short enough for the rounding of its travel to be bounded relatively.

   function Is_Solved (Base, Times : Feedrate_Profile_Times; Variable : Feedrate_Profile_Times_Index) return Boolean
   is ((for all J in Feedrate_Profile_Times_Index => (if J /= Variable then Times (J) = Base (J)))
       and then Times (Variable) >= No_Time
       and then Times (Variable) <= Search_Time_Cap);
   --  Times is the candidate Base with phase time Variable bisected.

   procedure Lemma_Distance_Solve
     (Solver             : Delta_V_Solver;
      Above, Base, Times : Feedrate_Profile_Times;
      Variable           : Feedrate_Profile_Times_Index;
      Vs                 : Velocity;
      D                  : Length;
      Am                 : Acceleration;
      Jm                 : Jerk;
      Sm                 : Snap;
      Cm                 : Crackle;
      Moderate           : Boolean)
   with
     Pre  =>
       Limits_In_Domain (Am, Jm, Sm, Cm)
       and then Prepared_Fact (Solver, Am, Jm, Sm, Cm)
       and then (if Moderate then Is_Moderate (Solver))
       and then Vs >= No_Speed
       and then Vs <= Speed_Max
       and then Times_In_Range (Times)
       and then Times_In_Range (Above)
       and then Is_Candidate (Solver, Base, Variable)
       and then Is_Solved (Base, Times, Variable)
       and then (Times (Variable) = No_Time or else Fast_Distance_At_Max_Time (Times, Cm, Vs) <= D)
       and then
         (if Variable < 4
          then
            Is_Candidate (Solver, Above, Variable + 1) and then not (D > Fast_Distance_At_Max_Time (Above, Cm, Vs))),
     Post => (if Moderate then Ramp_Is_Within_Limits (Times, Am, Jm, Sm, Cm));
   --  The ramp which the distance solver returns is within the limits, at every instant, to
   --  Certificates.Certified_Tolerance. Above is the candidate after Base, which was found long enough.
   --  Moderate is whether the saturated ramp is known to be moderate, which the caller cannot test in ghost code
   --  of its own.

   procedure Lemma_Moderate_2
     (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle; Moderate : Boolean)
   with
     Pre  =>
       Limits_In_Domain (Am, Jm, Sm, Cm)
       and then Prepared_Fact (Solver, Am, Jm, Sm, Cm)
       and then Solver.Region = Region_2
       and then Quotient_Fact (Solver.Jerk_Over_Snap, Jm, Sm)
       and then Solver.T1 >= No_Time
       and then Solver.T2 >= No_Time
       and then Solver.T3 = No_Time
       and then (if Moderate then Solver.Jerk_Over_Snap <= Moderate_Time_Max and then Solver.T1 <= Moderate_Time_Max),
     Post => (if Moderate then Is_Moderate (Solver));
   --  The second phase time of Region_2 is a difference with a root, which the quotients of the limits bound
   --  through the jerk peak of the saturated ramp.

   ------------------------------------------------------------------------------------------------------------------
   --  The peaks and the highest speed of a profile
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Peaks
     (Solver : Delta_V_Solver;
      Times  : Feedrate_Profile_Times;
      Vd     : Velocity;
      Am     : Acceleration;
      Jm     : Jerk;
      Sm     : Snap;
      Cm     : Crackle)
   with
     Pre  =>
       Solver_In_Domain (Solver)
       and then Times_In_Range (Times)
       and then Solve_Is_Sound (Solver, Vd, Times (1), Times (2), Times (3), Times (4)),
     Post =>
       (if Solver_Is_Consistent (Solver)
          and then Snap_Is_Within (Solver, Sm)
          and then Holds_Limits (Solver, Am, Jm, Cm)
          and then Limits_In_Domain (Am, Jm, Sm, Cm)
        then Peaks_Are_Limited (Times (1), Times (2), Times (3), Am, Jm, Sm, Cm));
   --  The ramp which a consistent solver returns has its peaks within the limits which the solver was prepared
   --  with.

   procedure Lemma_Peaks_Of_Rest (T1, T2, T3 : Time; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Pre  => T1 = No_Time and then T2 = No_Time and then T3 = No_Time,
     Post => (if Limits_In_Domain (Am, Jm, Sm, Cm) then Peaks_Are_Limited (T1, T2, T3, Am, Jm, Sm, Cm));
   --  A ramp which takes no time has no peaks.

   procedure Lemma_Top_Speed
     (Solver : Delta_V_Solver; Times : Feedrate_Profile_Times; Start_Vel, Peak : Velocity; Cm : Crackle)
   with
     Pre  =>
       Solver_In_Domain (Solver)
       and then Times_In_Range (Times)
       and then Start_Vel >= No_Speed
       and then Start_Vel <= Speed_Max
       and then Peak >= No_Speed
       and then Peak <= Speed_Max
       and then Solve_Is_Sound (Solver, abs (Start_Vel - Peak), Times (1), Times (2), Times (3), Times (4)),
     Post =>
       (if Solver_Is_Consistent (Solver) and then Solver.Cm = Cm and then Peak >= Start_Vel
        then Top_Speed_Is_At_Most (Times (1), Times (2), Times (3), Times (4), Start_Vel, Peak, Cm));
   --  The acceleration ramp which a consistent solver returns for the speed change from Start_Vel to Peak ends
   --  at no more than Peak.

   procedure Lemma_Top_Speed_Weaken (T1, T2, T3, T4 : Time; Start_Vel, Peak, Top : Velocity; Cm : Crackle)
   with
     Pre  => Top_Speed_Is_At_Most (T1, T2, T3, T4, Start_Vel, Peak, Cm) and then Peak >= No_Speed and then Peak <= Top,
     Post => Top_Speed_Is_At_Most (T1, T2, T3, T4, Start_Vel, Top, Cm);

   procedure Lemma_Top_Speed_Of_Rest (T1, T2, T3, T4 : Time; Start_Vel, Top : Velocity; Cm : Crackle)
   with
     Pre  =>
       T1 = No_Time
       and then T2 = No_Time
       and then T3 = No_Time
       and then T4 = No_Time
       and then Start_Vel >= No_Speed
       and then Start_Vel <= Top,
     Post => Top_Speed_Is_At_Most (T1, T2, T3, T4, Start_Vel, Top, Cm);
   --  A profile without an acceleration ramp coasts at its starting speed.

end Prunt.Motion_Planner.Trajectory.Solvers.Accuracy;
