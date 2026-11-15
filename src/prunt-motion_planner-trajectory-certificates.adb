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

pragma Style_Checks ("-s");
--  Lemmas are declared by their bodies, where they are proved: a separate declaration would only repeat them.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

with Prunt.Motion_Planner.Trajectory.Profile_Model.Derivatives;

package body Prunt.Motion_Planner.Trajectory.Certificates
  with SPARK_Mode => On
is

   ------------------------------------------------------------------------------------------------------------------
   --  How the proof goes
   --
   --  Each floating-point value F of the check is computed for an exact real X which is not negative: a peak, or a
   --  sum of phase times. Bounds (X, F, U, A) says that X is at most F * U + A, where U is a little more than one
   --  and A a little more than zero. Each rounded operation costs one more factor of 1 + 2 * Epsilon in U and one
   --  more Eta in A, and a product carries the absolute part of each factor's bound through the magnitude of the
   --  other factor. Lemma_Product_Bound takes such bounds through a product and Lemma_Sum_Bound through a sum.
   --  Lemma_Accepted then turns the floating-point comparison of F with a limit into the comparison of X with the
   --  exact limit: the limits are large enough for A to be negligible, and the difference between the checked and
   --  the certified tolerance pays for U and for the rounding of the limit's own product.
   ------------------------------------------------------------------------------------------------------------------

   Tolerance_Factor : constant Dimensionless := 1.0 + Checked_Tolerance;
   --  The factor of the check. Both the sum and its conversion are exact.

   Checked_Factor : constant := 1.0 + Checked_Tolerance;
   --  The real number which Tolerance_Factor denotes.

   Round_Up : constant := 1.0 + 2.0**(-52);
   --  One and twice Epsilon. An exact value is at most its rounding, plus Eta, times this: 1 / (1 - Epsilon) is
   --  smaller.

   One_Less_Epsilon : constant := 1.0 - 2.0**(-53);
   One_More_Epsilon : constant := 1.0 + 2.0**(-53);
   --  Spelled out because a named number cannot refer to Epsilon, which is ghost.

   Slack_1 : constant := Round_Up;
   Slack_2 : constant := Slack_1 * Round_Up;
   Slack_3 : constant := Slack_2 * Round_Up;
   Slack_7 : constant := Slack_3 * Slack_3 * Round_Up;
   --  The relative part of a bound after so many rounded operations in a row.

   Slack_Max : constant := 1.0 + 2.0**(-40);
   --  The largest relative part which Lemma_Accepted takes.

   Absolute_Max : constant := 2.0**(-700);
   --  The largest absolute part which Lemma_Accepted takes.

   Limit_Min : constant Dimensioned_Float := 2#1.0#E-100;
   Limit_Max : constant Dimensioned_Float := 2#1.0#E100;
   --  The limits which Lemma_Accepted takes, as in the specification.

   Real_Limit_Min : constant := 2.0**(-100);
   --  The real number which Limit_Min denotes.

   ------------------------------------------------------------------------------------------------------------------
   --  Real arithmetic
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Product_Monotone (A, B, C, D : Real)
   with Ghost, Pre => A >= 0.0 and then A <= B and then C >= 0.0 and then C <= D, Post => A * C <= B * D
   is
   begin
      pragma Assert (A * C <= B * C);
      pragma Assert (B * C <= B * D);
   end Lemma_Product_Monotone;

   procedure Lemma_Abs_Product (X, Y : Real) with Ghost, Pre => Y >= 0.0, Post => abs (X * Y) = abs X * Y is
   begin
      null;
   end Lemma_Abs_Product;

   procedure Lemma_Expand_Bounds (F1, F2, U1, U2, A1, A2 : Real)
   with
     Ghost,
     Post => (F1 * U1 + A1) * (F2 * U2 + A2) = (F1 * F2) * (U1 * U2) + A1 * (F2 * U2) + A2 * (F1 * U1) + A1 * A2
   is
   begin
      null;
   end Lemma_Expand_Bounds;

   ------------------------------------------------------------------------------------------------------------------
   --  One rounded operation, over reals
   --
   --  P is the exact result of an operation on two floating-point values and F its rounding, as the lemmas of
   --  Prunt.Motion_Planner.Reals.Float_Lemmas relate them.
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Exact_Below_Rounded (P, F : Real)
   with
     Ghost,
     Pre  => P >= 0.0 and then abs (F - P) <= Epsilon * abs P + Eta,
     Post => F + Eta >= 0.0 and then P <= (F + Eta) * Round_Up
   is
   begin
      pragma Assert (P * One_Less_Epsilon <= F + Eta);
      pragma Assert (P <= P * One_Less_Epsilon * Round_Up);
   end Lemma_Exact_Below_Rounded;

   procedure Lemma_Rounded_Below_Exact (P, F : Real)
   with Ghost, Pre => P >= 0.0 and then abs (F - P) <= Epsilon * abs P + Eta, Post => F <= P * One_More_Epsilon + Eta
   is
   begin
      null;
   end Lemma_Rounded_Below_Exact;

   procedure Lemma_Absorb_Rounding (X, P, F, V, B, U, A : Real)
   with
     Ghost,
     Pre  =>
       P >= 0.0
       and then abs (F - P) <= Epsilon * abs P + Eta
       and then V >= 0.0
       and then X <= P * V + B
       and then U >= Round_Up * V
       and then A >= Eta * U + B,
     Post => X <= F * U + A
   is
      Q : constant Real := F + Eta;
   begin
      Lemma_Exact_Below_Rounded (P, F);
      Lemma_Product_Monotone (P, Q * Round_Up, V, V);
      pragma Assert (P * V <= Q * (Round_Up * V));
      Lemma_Product_Monotone (Q, Q, Round_Up * V, U);
      pragma Assert (P * V <= Q * U);
      pragma Assert (Q * U = F * U + Eta * U);
   end Lemma_Absorb_Rounding;
   --  If X is at most V times the exact result, plus B, then it is at most U times the rounded result, plus A.

   procedure Lemma_Real_Product_Bound (X1, X2, F1, F2, U1, U2, A1, A2, M1, M2, B : Real)
   with
     Ghost,
     Pre  =>
       X1 >= 0.0
       and then X2 >= 0.0
       and then F1 >= 0.0
       and then F1 <= M1
       and then F2 >= 0.0
       and then F2 <= M2
       and then U1 >= 0.0
       and then U2 >= 0.0
       and then A1 >= 0.0
       and then A2 >= 0.0
       and then X1 <= F1 * U1 + A1
       and then X2 <= F2 * U2 + A2
       and then B = A1 * (M2 * U2) + A2 * (M1 * U1) + A1 * A2,
     Post => X1 * X2 <= (F1 * F2) * (U1 * U2) + B
   is
   begin
      Lemma_Product_Monotone (X1, F1 * U1 + A1, X2, F2 * U2 + A2);
      Lemma_Expand_Bounds (F1, F2, U1, U2, A1, A2);
      Lemma_Product_Monotone (F2, M2, U2, U2);
      Lemma_Product_Monotone (A1, A1, F2 * U2, M2 * U2);
      Lemma_Product_Monotone (F1, M1, U1, U1);
      Lemma_Product_Monotone (A2, A2, F1 * U1, M1 * U1);
   end Lemma_Real_Product_Bound;

   procedure Lemma_Real_Sum_Bound (X1, X2, F1, F2, U1, U2, A1, A2, V : Real)
   with
     Ghost,
     Pre  =>
       F1 >= 0.0
       and then F2 >= 0.0
       and then U1 >= 0.0
       and then U2 >= 0.0
       and then X1 <= F1 * U1 + A1
       and then X2 <= F2 * U2 + A2
       and then V >= U1
       and then V >= U2,
     Post => X1 + X2 <= (F1 + F2) * V + (A1 + A2)
   is
   begin
      Lemma_Product_Monotone (F1, F1, U1, V);
      Lemma_Product_Monotone (F2, F2, U2, V);
      pragma Assert ((F1 + F2) * V = F1 * V + F2 * V);
   end Lemma_Real_Sum_Bound;

   procedure Lemma_Real_Accepted (X, F, L, FL, U, A : Real)
   with
     Ghost,
     Pre  =>
       X <= F * U + A
       and then F <= FL
       and then L >= Real_Limit_Min
       and then abs (FL - L * Checked_Factor) <= Epsilon * abs (L * Checked_Factor) + Eta
       and then U >= 0.0
       and then U <= Slack_Max
       and then A <= Absolute_Max,
     Post => X <= L * (1.0 + Certified_Tolerance)
   is
      Top : constant Real := L * Checked_Factor * One_More_Epsilon + Eta;
   begin
      Lemma_Rounded_Below_Exact (L * Checked_Factor, FL);
      pragma Assert (FL <= Top);
      pragma Assert (Top >= 0.0);
      pragma Assert (F * U <= Top * U);
      Lemma_Product_Monotone (Top, Top, U, Slack_Max);
      pragma Assert (X <= Top * Slack_Max + Absolute_Max);
      pragma Assert (Top * Slack_Max + Absolute_Max <= L * (1.0 + Certified_Tolerance));
   end Lemma_Real_Accepted;
   --  FL is the rounding of the limit L times the factor of the check, and F did not exceed it.

   ------------------------------------------------------------------------------------------------------------------
   --  Floating-point values and the reals which they denote
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Order (F1, F2 : Dimensioned_Float) with Ghost, Post => (if F1 <= F2 then R (F1) <= R (F2)) is
   begin
      null;
   end Lemma_Order;

   procedure Lemma_Nonnegative (F : Dimensioned_Float) with Ghost, Pre => F >= 0.0, Post => R (F) >= 0.0 is
   begin
      null;
   end Lemma_Nonnegative;

   procedure Lemma_Magnitude (F : Dimensioned_Float) with Ghost, Post => R (abs F) = abs R (F) is
   begin
      null;
   end Lemma_Magnitude;

   procedure Lemma_Tolerance_Factor with Ghost, Post => R (Tolerance_Factor) = Checked_Factor is
   begin
      null;
   end Lemma_Tolerance_Factor;

   procedure Lemma_Nonnegative_Product (F1, F2 : Dimensioned_Float)
   with
     Ghost,
     Pre  => F1 >= 0.0 and then F1 <= Square_Root_Of_Last and then F2 >= 0.0 and then F2 <= Square_Root_Of_Last,
     Post => F1 * F2 >= 0.0
   is
   begin
      null;
   end Lemma_Nonnegative_Product;

   procedure Lemma_Nonnegative_Sum (F1, F2 : Dimensioned_Float)
   with
     Ghost,
     Pre  =>
       F1 >= 0.0
       and then F1 <= Dimensioned_Float'Last / 2.0
       and then F2 >= 0.0
       and then F2 <= Dimensioned_Float'Last / 2.0,
     Post => F1 + F2 >= 0.0
   is
   begin
      null;
   end Lemma_Nonnegative_Sum;

   ------------------------------------------------------------------------------------------------------------------
   --  Bounds of exact values by the floating-point values computed for them
   ------------------------------------------------------------------------------------------------------------------

   function Bounds (X : Real; F : Dimensioned_Float; U, A : Real) return Boolean
   is (X >= 0.0 and then F >= 0.0 and then U >= 1.0 and then A >= 0.0 and then X <= R (F) * U + A)
   with Ghost;
   --  The exact value X, which is not negative, is at most U times the floating-point value F, plus A.

   procedure Lemma_Exact (F : Dimensioned_Float) with Ghost, Pre => F >= 0.0, Post => Bounds (R (F), F, 1.0, 0.0) is
   begin
      Lemma_Nonnegative (F);
   end Lemma_Exact;
   --  A floating-point value bounds the real which it denotes.

   procedure Lemma_Product_Bound (X1, X2 : Real; F1, F2, M1, M2 : Dimensioned_Float; U1, U2, A1, A2, U, A : Real)
   with
     Ghost,
     Pre  =>
       Bounds (X1, F1, U1, A1)
       and then Bounds (X2, F2, U2, A2)
       and then F1 <= M1
       and then M1 <= Square_Root_Of_Last
       and then F2 <= M2
       and then M2 <= Square_Root_Of_Last
       and then U >= Round_Up * (U1 * U2)
       and then A >= Eta * U + (A1 * (R (M2) * U2) + A2 * (R (M1) * U1) + A1 * A2),
     Post => Bounds (X1 * X2, F1 * F2, U, A)
   is
      B : constant Real := A1 * (R (M2) * U2) + A2 * (R (M1) * U1) + A1 * A2;
   begin
      Lemma_Nonnegative (F1);
      Lemma_Nonnegative (F2);
      Lemma_Order (F1, M1);
      Lemma_Order (F2, M2);
      Lemma_Nonnegative_Product (F1, F2);
      Float_Lemmas.Lemma_Rounding_Error_Mul (F1, F2);
      Lemma_Real_Product_Bound (X1, X2, R (F1), R (F2), U1, U2, A1, A2, R (M1), R (M2), B);
      Lemma_Product_Monotone (0.0, R (F1), 0.0, R (F2));
      Lemma_Product_Monotone (0.0, U1, 0.0, U2);
      Lemma_Absorb_Rounding (X1 * X2, R (F1) * R (F2), R (F1 * F2), U1 * U2, B, U, A);
      Lemma_Product_Monotone (0.0, X1, 0.0, X2);
      pragma Assert (U >= 1.0);
      pragma Assert (A >= 0.0);
   end Lemma_Product_Bound;
   --  The product of two bounded values is bounded by the floating-point product. M1 and M2 are magnitudes which
   --  the floating-point factors do not exceed: they carry the absolute part of each bound through the other
   --  factor.

   procedure Lemma_Sum_Bound (X1, X2 : Real; F1, F2 : Dimensioned_Float; U1, U2, A1, A2, U, A : Real)
   with
     Ghost,
     Pre  =>
       Bounds (X1, F1, U1, A1)
       and then Bounds (X2, F2, U2, A2)
       and then F1 <= Dimensioned_Float'Last / 2.0
       and then F2 <= Dimensioned_Float'Last / 2.0
       and then U >= Round_Up * U1
       and then U >= Round_Up * U2
       and then A >= Eta * U + (A1 + A2),
     Post => Bounds (X1 + X2, F1 + F2, U, A)
   is
      V : constant Real := (if U1 >= U2 then U1 else U2);
   begin
      Lemma_Nonnegative (F1);
      Lemma_Nonnegative (F2);
      Lemma_Nonnegative_Sum (F1, F2);
      Float_Lemmas.Lemma_Rounding_Error_Add (F1, F2);
      Lemma_Real_Sum_Bound (X1, X2, R (F1), R (F2), U1, U2, A1, A2, V);
      Lemma_Absorb_Rounding (X1 + X2, R (F1) + R (F2), R (F1 + F2), V, A1 + A2, U, A);
      pragma Assert (U >= 1.0);
      pragma Assert (A >= 0.0);
   end Lemma_Sum_Bound;
   --  The sum of two bounded values is bounded by the floating-point sum.

   procedure Lemma_Accepted (X : Real; F, Limit : Dimensioned_Float; U, A : Real)
   with
     Ghost,
     Pre  =>
       Bounds (X, F, U, A)
       and then U <= Slack_Max
       and then A <= Absolute_Max
       and then Limit >= Limit_Min
       and then Limit <= Limit_Max,
     Post => (if F <= Limit * Tolerance_Factor then X <= R (Limit) * (1.0 + Certified_Tolerance))
   is
   begin
      Lemma_Order (F, Limit * Tolerance_Factor);
      Lemma_Order (Limit_Min, Limit);
      pragma Assert (R (Limit_Min) = Real_Limit_Min);
      Lemma_Tolerance_Factor;
      Float_Lemmas.Lemma_Rounding_Error_Mul (Limit, Tolerance_Factor);
      if F <= Limit * Tolerance_Factor then
         Lemma_Real_Accepted (X, R (F), R (Limit), R (Limit * Tolerance_Factor), U, A);
      end if;
   end Lemma_Accepted;
   --  A bounded value whose floating-point value passes the check against a limit is within the certified
   --  tolerance of the exact limit.

   ------------------------------------------------------------------------------------------------------------------
   --  The certificate
   ------------------------------------------------------------------------------------------------------------------

   No_Snap : constant Snap := 0.0;
   No_Jerk : constant Jerk := 0.0;

   Time_Sum_Max  : constant Time := Time (2#1.0#E102);
   Peak_Snap_Max : constant Snap := Snap (2#1.0#E200);
   Peak_Jerk_Max : constant Jerk := Jerk (2#1.0#E302);
   --  No sum of phase times, and no peak, which the check computes within its precondition is larger.

   Absolute_1 : constant := 2.0**(-1074);
   Absolute_2 : constant := 2.0**(-1073);
   Absolute_3 : constant := 2.0**(-1072);
   --  The absolute part of a bound after so many rounded operations in a row on exact values.

   Absolute_Jerk         : constant := 2.0**(-870);
   Absolute_Acceleration : constant := 2.0**(-765);
   --  The absolute parts of the bounds of the peaks of jerk and acceleration: more than the absolute parts of the
   --  factors' bounds times the magnitudes above.

   procedure Lemma_Peak_Magnitudes (C, T1, T2, T3 : Real)
   with
     Ghost,
     Pre  => T1 >= 0.0 and then T2 >= 0.0 and then T3 >= 0.0,
     Post =>
       abs (C * T1) = abs C * T1
       and then abs (C * T1 * (T1 + T2)) = abs C * T1 * (T1 + T2)
       and then abs (C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3)) = abs C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3)
   is
   begin
      Lemma_Abs_Product (C, T1);
      Lemma_Abs_Product (C * T1, T1 + T2);
      Lemma_Abs_Product (C * T1 * (T1 + T2), 2.0 * T1 + T2 + T3);
   end Lemma_Peak_Magnitudes;

   procedure Lemma_Exact_Peaks (Exact : Ramp; C, T1, T2, T3 : Real)
   with
     Ghost,
     Pre  =>
       T1 >= 0.0
       and then T2 >= 0.0
       and then T3 >= 0.0
       and then C = abs Exact.C
       and then Exact.T1 = T1
       and then Exact.T2 = T2
       and then Exact.T3 = T3,
     Post =>
       abs Peak_Snap (Exact) = C * T1
       and then abs Peak_Jerk (Exact) = C * T1 * (T1 + T2)
       and then abs Peak_Accel (Exact) = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3)
   is
   begin
      Lemma_Peak_Magnitudes (Exact.C, T1, T2, T3);
      pragma Assert (Peak_Snap (Exact) = Exact.C * T1);
      pragma Assert (Peak_Jerk (Exact) = Exact.C * T1 * (T1 + T2));
      pragma Assert (Peak_Accel (Exact) = Exact.C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3));
   end Lemma_Exact_Peaks;
   --  The magnitudes of the peaks of a ramp are the peaks of the ramp with the magnitude of its crackle, C.

   function Ramp_Respects_Limits
     (Times            : Feedrate_Profile_Times;
      Max_Crackle      : Crackle;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Boolean
   is
      Magnitude : constant Crackle := abs Max_Crackle
      with Ghost;
      Peak_S    : constant Snap := abs Max_Crackle * Times (1);
      pragma Assert (Peak_S >= No_Snap and then Peak_S <= Peak_Snap_Max);

      Sum_12 : constant Time := Times (1) + Times (2)
      with Ghost;
      pragma Assert (Sum_12 >= No_Time and then Sum_12 <= Time_Sum_Max);
      Peak_J : constant Jerk := Peak_S * (Times (1) + Times (2));
      pragma Assert (Peak_J >= No_Jerk and then Peak_J <= Peak_Jerk_Max);

      Twice_1  : constant Time := 2.0 * Times (1)
      with Ghost;
      pragma Assert (Twice_1 >= No_Time and then Twice_1 <= Time_Sum_Max);
      Sum_112  : constant Time := Twice_1 + Times (2)
      with Ghost;
      pragma Assert (Sum_112 >= No_Time and then Sum_112 <= Time_Sum_Max);
      Sum_1123 : constant Time := Sum_112 + Times (3)
      with Ghost;
      pragma Assert (Sum_1123 >= No_Time and then Sum_1123 <= Time_Sum_Max);
      Peak_A   : constant Acceleration := Peak_J * (2.0 * Times (1) + Times (2) + Times (3));
      --  The check computes the three peaks only. The ghost constants name the intermediate values of their
      --  expressions, and the assertions give the range of each value before it is used, which is what the proofs
      --  of absence of overflow need.

      C  : constant Real := R (Magnitude)
      with Ghost;
      T1 : constant Real := R (Times (1))
      with Ghost;
      T2 : constant Real := R (Times (2))
      with Ghost;
      T3 : constant Real := R (Times (3))
      with Ghost;
      --  The reals which the inputs denote.

      Exact_Snap         : constant Real := C * T1
      with Ghost;
      Exact_Jerk         : constant Real := Exact_Snap * (T1 + T2)
      with Ghost;
      Exact_Acceleration : constant Real := Exact_Jerk * (2.0 * T1 + T2 + T3)
      with Ghost;
      --  The magnitudes of the exact peaks.

      Exact : constant Ramp := To_Ramp (Times, Max_Crackle, No_Velocity)
      with Ghost;
   begin
      --  The inputs are exact.
      Lemma_Exact (Magnitude);
      Lemma_Exact (Times (1));
      Lemma_Exact (Times (2));
      Lemma_Exact (Times (3));
      Lemma_Exact (2.0);

      --  Snap: one rounded product.
      Lemma_Product_Bound
        (X1 => C,
         X2 => T1,
         F1 => Magnitude,
         F2 => Times (1),
         M1 => Certified_Crackle_Max,
         M2 => Certified_Time_Max,
         U1 => 1.0,
         U2 => 1.0,
         A1 => 0.0,
         A2 => 0.0,
         U  => Slack_1,
         A  => Absolute_1);
      pragma Assert (Bounds (Exact_Snap, Peak_S, Slack_1, Absolute_1));

      --  Jerk: a rounded sum, and the rounded product of it and the peak of snap.
      Lemma_Sum_Bound
        (X1 => T1,
         X2 => T2,
         F1 => Times (1),
         F2 => Times (2),
         U1 => 1.0,
         U2 => 1.0,
         A1 => 0.0,
         A2 => 0.0,
         U  => Slack_1,
         A  => Absolute_1);
      pragma Assert (Bounds (T1 + T2, Sum_12, Slack_1, Absolute_1));
      Lemma_Product_Bound
        (X1 => Exact_Snap,
         X2 => T1 + T2,
         F1 => Peak_S,
         F2 => Sum_12,
         M1 => Peak_Snap_Max,
         M2 => Time_Sum_Max,
         U1 => Slack_1,
         U2 => Slack_1,
         A1 => Absolute_1,
         A2 => Absolute_1,
         U  => Slack_3,
         A  => Absolute_Jerk);
      pragma Assert (Bounds (Exact_Jerk, Peak_J, Slack_3, Absolute_Jerk));

      --  Acceleration: a rounded product and two rounded sums, and the rounded product of the last sum and the
      --  peak of jerk.
      Lemma_Product_Bound
        (X1 => 2.0,
         X2 => T1,
         F1 => 2.0,
         F2 => Times (1),
         M1 => 2.0,
         M2 => Certified_Time_Max,
         U1 => 1.0,
         U2 => 1.0,
         A1 => 0.0,
         A2 => 0.0,
         U  => Slack_1,
         A  => Absolute_1);
      pragma Assert (Bounds (2.0 * T1, Twice_1, Slack_1, Absolute_1));
      Lemma_Sum_Bound
        (X1 => 2.0 * T1,
         X2 => T2,
         F1 => Twice_1,
         F2 => Times (2),
         U1 => Slack_1,
         U2 => 1.0,
         A1 => Absolute_1,
         A2 => 0.0,
         U  => Slack_2,
         A  => Absolute_2);
      pragma Assert (Bounds (2.0 * T1 + T2, Sum_112, Slack_2, Absolute_2));
      Lemma_Sum_Bound
        (X1 => 2.0 * T1 + T2,
         X2 => T3,
         F1 => Sum_112,
         F2 => Times (3),
         U1 => Slack_2,
         U2 => 1.0,
         A1 => Absolute_2,
         A2 => 0.0,
         U  => Slack_3,
         A  => Absolute_3);
      pragma Assert (Bounds (2.0 * T1 + T2 + T3, Sum_1123, Slack_3, Absolute_3));
      Lemma_Product_Bound
        (X1 => Exact_Jerk,
         X2 => 2.0 * T1 + T2 + T3,
         F1 => Peak_J,
         F2 => Sum_1123,
         M1 => Peak_Jerk_Max,
         M2 => Time_Sum_Max,
         U1 => Slack_3,
         U2 => Slack_3,
         A1 => Absolute_Jerk,
         A2 => Absolute_3,
         U  => Slack_7,
         A  => Absolute_Acceleration);
      pragma Assert (Bounds (Exact_Acceleration, Peak_A, Slack_7, Absolute_Acceleration));

      --  The peaks of the exact ramp are, in magnitude, the values which have just been bounded.
      Lemma_Magnitude (Max_Crackle);
      Lemma_Exact_Peaks (Exact, C, T1, T2, T3);
      pragma Assert (abs Peak_Snap (Exact) = Exact_Snap);
      pragma Assert (abs Peak_Jerk (Exact) = Exact_Jerk);
      pragma Assert (abs Peak_Accel (Exact) = Exact_Acceleration);

      --  What each comparison of the check implies.
      Lemma_Order (Magnitude, Crackle_Max);
      Lemma_Accepted (Exact_Snap, Peak_S, Snap_Max, Slack_1, Absolute_1);
      Lemma_Accepted (Exact_Jerk, Peak_J, Jerk_Max, Slack_3, Absolute_Jerk);
      Lemma_Accepted (Exact_Acceleration, Peak_A, Acceleration_Max, Slack_7, Absolute_Acceleration);

      return
        abs Max_Crackle <= Crackle_Max
        and then Peak_S <= Snap_Max * Tolerance_Factor
        and then Peak_J <= Jerk_Max * Tolerance_Factor
        and then Peak_A <= Acceleration_Max * Tolerance_Factor;
   end Ramp_Respects_Limits;

   procedure Lemma_Limits_At_Every_Instant
     (Exact : Ramp; Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max : Real; Stage : Stage_Index; DT : Real) is
   begin
      Derivatives.Lemma_Limits (Exact, Stage, DT);
      pragma Assert (abs Stage_Crackle (Exact, Stage) <= abs Exact.C);
      pragma Assert (abs Stage_Crackle (Exact, Stage) <= Crackle_Max);
      pragma Assert (abs State_In (Exact, Stage, DT).Snap <= Snap_Max * (1.0 + Certified_Tolerance));
      pragma Assert (abs State_In (Exact, Stage, DT).Jerk <= Jerk_Max * (1.0 + Certified_Tolerance));
      pragma Assert (abs State_In (Exact, Stage, DT).Accel <= Acceleration_Max * (1.0 + Certified_Tolerance));
   end Lemma_Limits_At_Every_Instant;

end Prunt.Motion_Planner.Trajectory.Certificates;
