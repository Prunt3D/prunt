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

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

package body Prunt.Motion_Planner.Trajectory.Rounding
  with SPARK_Mode => On
is

   ------------------------------------------------------------------------------------------------------------------
   --  How the proofs go
   --
   --  The lemmas for the operations each call the rounding lemma of Prunt.Motion_Planner.Reals for their operation
   --  and then a lemma over reals only, which combines the rounding with what is known about the operands. The
   --  lemmas over reals have a few scalar parameters and one algebraic step each, which is what the provers
   --  manage.
   --
   --  Lemma_Sharp_Sum is the exception. SPARKlib bounds the rounding of a sum S of X and Y by Epsilon * (X + Y)
   --  + Eta. Where S is at least Small that Eta is a small multiple of Epsilon * (X + Y). Where S is below Small,
   --  X, Y and S are not normal and the sum is exact, but the provers cannot see that the real which S denotes is
   --  the sum of the reals which X and Y denote. So the proof goes through a scale at which Eta does not matter:
   --  X, Y and S are multiplied by Scale, a power of two. SPARKlib bounds the rounding of those products, and of
   --  the sum Q of the scaled X and Y, relative to them, their Eta being negligible there. The scaled values are
   --  all multiples of Gap, the scaled spacing of values which are not normal, which the provers see by
   --  bit-blasting (Lemma_Grid and Lemma_Gap, in floating point only). The scaled S and Q are within less than
   --  Gap of each other, by the bounds, so they are equal, and that makes S relative to X + Y as closely as Q is
   --  to the scaled X and Y.
   ------------------------------------------------------------------------------------------------------------------

   One_More_Epsilon : constant := 1.0 + 2.0**(-53);
   One_More_Slack   : constant := 1.0 + Slack_Max;
   Slack_Squared    : constant := Slack_Max * Slack_Max;
   Twice_Epsilon    : constant := 2.0**(-52);
   Real_Root        : constant := 2.0**511;

   Half_Magnitude_Max : constant := 2.0**508;

   ------------------------------------------------------------------------------------------------------------------
   --  Real arithmetic
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Scale_Monotone (A, B, C : Real) with Pre => A <= B and then C >= 0.0, Post => A * C <= B * C is
   begin
      null;
   end Lemma_Scale_Monotone;

   procedure Lemma_Product_Monotone (A, B, C, D : Real)
   with Pre => A >= 0.0 and then A <= B and then C >= 0.0 and then C <= D, Post => A * C <= B * D
   is
   begin
      pragma Assert (A * C <= B * C);
      pragma Assert (B * C <= B * D);
   end Lemma_Product_Monotone;

   procedure Lemma_Abs_Product (X, Y : Real) with Post => abs (X * Y) = abs X * abs Y is
   begin
      null;
   end Lemma_Abs_Product;

   procedure Lemma_Abs_Product_Bound (X, Y, P, Q : Real)
   with Pre => abs X <= P and then abs Y <= Q, Post => abs (X * Y) <= P * Q
   is
   begin
      Lemma_Abs_Product (X, Y);
      Lemma_Product_Monotone (abs X, P, abs Y, Q);
   end Lemma_Abs_Product_Bound;

   ------------------------------------------------------------------------------------------------------------------
   --  A sum, over reals
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Sum (X1, X2, F1, F2, FS, M1, M2, U1, U2, A1, A2, W, D, U, A : Real)
   with
     Pre  =>
       abs (F1 - X1) <= U1 * M1 + A1
       and then abs X1 <= M1
       and then abs (F2 - X2) <= U2 * M2 + A2
       and then abs X2 <= M2
       and then U1 >= 0.0
       and then U1 <= W
       and then U2 >= 0.0
       and then U2 <= W
       and then W <= Slack_Max
       and then A1 >= 0.0
       and then A2 >= 0.0
       and then abs (FS - (F1 + F2)) <= Sharp * abs (F1 + F2) + D
       and then U >= W + Step
       and then A >= 2.0 * (A1 + A2) + D,
     Post => abs (FS - (X1 + X2)) <= U * (M1 + M2) + A and then abs (X1 + X2) <= M1 + M2
   is
      M : constant Real := M1 + M2;
   begin
      Lemma_Scale_Monotone (U1, W, M1);
      Lemma_Scale_Monotone (U2, W, M2);
      pragma Assert (W * M = W * M1 + W * M2);
      pragma Assert (abs (F1 - X1) + abs (F2 - X2) <= W * M + (A1 + A2));
      Lemma_Scale_Monotone (W, Slack_Max, M);
      pragma Assert (abs (F1 + F2) <= M + Slack_Max * M + (A1 + A2));
      pragma Assert (abs (FS - (X1 + X2)) <= Sharp * (M + Slack_Max * M + (A1 + A2)) + D + W * M + (A1 + A2));
      pragma Assert (abs (FS - (X1 + X2)) <= W * M + Step * M + 2.0 * (A1 + A2) + D);
      Lemma_Scale_Monotone (W + Step, U, M);
      pragma Assert ((W + Step) * M = W * M + Step * M);
   end Lemma_Real_Sum;
   --  FS is the rounding of F1 + F2, to within Sharp relatively and D absolutely. W is the larger of the
   --  relative parts of the operands.

   ------------------------------------------------------------------------------------------------------------------
   --  A product, over reals
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Product_Error (X1, X2, F1, F2, M1, M2, E1, E2 : Real)
   with
     Pre  => abs (F1 - X1) <= E1 and then abs X1 <= M1 and then abs (F2 - X2) <= E2 and then abs X2 <= M2,
     Post => abs (F1 * F2 - X1 * X2) <= E1 * M2 + M1 * E2 + E1 * E2 and then abs (X1 * X2) <= M1 * M2
   is
      D1 : constant Real := F1 - X1;
      D2 : constant Real := F2 - X2;
   begin
      Lemma_Abs_Product_Bound (D1, X2, E1, M2);
      Lemma_Abs_Product_Bound (X1, D2, M1, E2);
      Lemma_Abs_Product_Bound (D1, D2, E1, E2);
      Lemma_Abs_Product_Bound (X1, X2, M1, M2);
      pragma Assert (F1 * F2 - X1 * X2 = D1 * X2 + X1 * D2 + D1 * D2);
   end Lemma_Product_Error;
   --  The error of a product of two values with errors of at most E1 and E2.

   procedure Lemma_Expand_Errors (M1, M2, U1, U2, A1, A2 : Real)
   with
     Post =>
       (U1 * M1 + A1) * M2 + M1 * (U2 * M2 + A2) + (U1 * M1 + A1) * (U2 * M2 + A2)
       = (U1 + U2 + U1 * U2) * (M1 * M2) + A1 * (M2 * (1.0 + U2)) + A2 * (M1 * (1.0 + U1)) + A1 * A2
   is
   begin
      null;
   end Lemma_Expand_Errors;

   procedure Lemma_Error_Terms (M1, M2, B1, B2, U1, U2, A1, A2 : Real)
   with
     Pre  =>
       M1 >= 0.0
       and then M1 <= B1
       and then M2 >= 0.0
       and then M2 <= B2
       and then U1 >= 0.0
       and then U1 <= Slack_Max
       and then U2 >= 0.0
       and then U2 <= Slack_Max
       and then A1 >= 0.0
       and then A2 >= 0.0,
     Post =>
       (U1 * M1 + A1) * M2 + M1 * (U2 * M2 + A2) + (U1 * M1 + A1) * (U2 * M2 + A2)
       <= (U1 + U2 + Slack_Squared) * (M1 * M2) + One_More_Slack * (A1 * B2 + A2 * B1) + A1 * A2
       and then M1 * M2 >= 0.0
       and then M1 * M2 <= B1 * B2
       and then A1 * B2 + A2 * B1 >= 0.0
       and then A1 * A2 >= 0.0
   is
      P : constant Real := M1 * M2;
   begin
      Lemma_Expand_Errors (M1, M2, U1, U2, A1, A2);
      Lemma_Product_Monotone (0.0, M1, 0.0, M2);
      Lemma_Product_Monotone (M1, B1, M2, B2);
      Lemma_Product_Monotone (U1, Slack_Max, U2, Slack_Max);
      Lemma_Scale_Monotone (U1 + U2 + U1 * U2, U1 + U2 + Slack_Squared, P);
      Lemma_Product_Monotone (M2, B2, 1.0 + U2, One_More_Slack);
      Lemma_Scale_Monotone (M2 * (1.0 + U2), B2 * One_More_Slack, A1);
      Lemma_Product_Monotone (M1, B1, 1.0 + U1, One_More_Slack);
      Lemma_Scale_Monotone (M1 * (1.0 + U1), B1 * One_More_Slack, A2);
      pragma Assert (A1 * (M2 * (1.0 + U2)) <= One_More_Slack * (A1 * B2));
      pragma Assert (A2 * (M1 * (1.0 + U1)) <= One_More_Slack * (A2 * B1));
      Lemma_Product_Monotone (0.0, A1, 0.0, B2);
      Lemma_Product_Monotone (0.0, A2, 0.0, B1);
      Lemma_Product_Monotone (0.0, A1, 0.0, A2);
   end Lemma_Error_Terms;
   --  The error of a product in terms of the relative and absolute parts of the bounds of its factors.

   procedure Lemma_Product_Rounding (FP, FF, XX, P, UU, T, U, A : Real)
   with
     Pre  =>
       abs (FP - FF) <= Epsilon * abs FF + Eta
       and then abs (FF - XX) <= UU * P + T
       and then abs XX <= P
       and then UU >= 0.0
       and then UU <= 1.0
       and then T >= 0.0
       and then U >= UU + Twice_Epsilon
       and then A >= One_More_Epsilon * T + Eta,
     Post => abs (FP - XX) <= U * P + A
   is
   begin
      pragma Assert (P >= 0.0);
      Lemma_Scale_Monotone (0.0, UU, P);
      Lemma_Scale_Monotone (UU, 1.0, P);
      pragma Assert (abs FF <= P + UU * P + T);
      pragma Assert (abs (FP - XX) <= Epsilon * (P + UU * P + T) + Eta + UU * P + T);
      pragma Assert (abs (FP - XX) <= UU * P + Twice_Epsilon * P + One_More_Epsilon * T + Eta);
      Lemma_Scale_Monotone (UU + Twice_Epsilon, U, P);
      pragma Assert ((UU + Twice_Epsilon) * P = UU * P + Twice_Epsilon * P);
   end Lemma_Product_Rounding;
   --  FP is the rounding of the product FF of the floating-point factors, which is within UU * P + T of the
   --  exact product XX.

   procedure Lemma_Real_Product (X1, X2, F1, F2, FP, M1, M2, B1, B2, U1, U2, A1, A2, U, A : Real)
   with
     Pre  =>
       abs (F1 - X1) <= U1 * M1 + A1
       and then abs X1 <= M1
       and then M1 <= B1
       and then abs (F2 - X2) <= U2 * M2 + A2
       and then abs X2 <= M2
       and then M2 <= B2
       and then U1 >= 0.0
       and then U1 <= Slack_Max
       and then U2 >= 0.0
       and then U2 <= Slack_Max
       and then A1 >= 0.0
       and then A2 >= 0.0
       and then abs (FP - F1 * F2) <= Epsilon * abs (F1 * F2) + Eta
       and then U >= U1 + U2 + Step
       and then A >= 2.0 * (A1 * B2 + A2 * B1 + A1 * A2) + Eta,
     Post => abs (FP - X1 * X2) <= U * (M1 * M2) + A and then abs (X1 * X2) <= M1 * M2 and then M1 * M2 <= B1 * B2
   is
      E1 : constant Real := U1 * M1 + A1;
      E2 : constant Real := U2 * M2 + A2;
      UU : constant Real := U1 + U2 + Slack_Squared;
      S  : constant Real := A1 * B2 + A2 * B1;
      T  : constant Real := One_More_Slack * S + A1 * A2;
   begin
      Lemma_Product_Error (X1, X2, F1, F2, M1, M2, E1, E2);
      Lemma_Error_Terms (M1, M2, B1, B2, U1, U2, A1, A2);
      pragma Assert (abs (F1 * F2 - X1 * X2) <= UU * (M1 * M2) + T);
      pragma Assert (One_More_Epsilon * T + Eta <= 2.0 * (S + A1 * A2) + Eta);
      Lemma_Product_Rounding (FP, F1 * F2, X1 * X2, M1 * M2, UU, T, U, A);
   end Lemma_Real_Product;

   ------------------------------------------------------------------------------------------------------------------
   --  A half, over reals
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Half (X, F, FH, M, U, A, UH, AH : Real)
   with
     Pre  =>
       abs (F - X) <= U * M + A
       and then abs X <= M
       and then U >= 0.0
       and then U <= Slack_Max
       and then A >= 0.0
       and then abs (FH - F / 2.0) <= Epsilon * abs (F / 2.0) + Eta
       and then UH >= U + Step
       and then AH >= A + Eta,
     Post => abs (FH - X / 2.0) <= UH * (M / 2.0) + AH and then abs (X / 2.0) <= M / 2.0
   is
      H : constant Real := M / 2.0;
   begin
      Lemma_Scale_Monotone (U, Slack_Max, M);
      pragma Assert (abs F <= M + Slack_Max * M + A);
      pragma Assert (U * M = 2.0 * (U * H));
      pragma Assert (abs (FH - X / 2.0) <= Epsilon * (M + Slack_Max * M + A) / 2.0 + Eta + U * H + A / 2.0);
      pragma Assert (abs (FH - X / 2.0) <= U * H + Step * H + A + Eta);
      Lemma_Scale_Monotone (U + Step, UH, H);
      pragma Assert ((U + Step) * H = U * H + Step * H);
   end Lemma_Real_Half;

   ------------------------------------------------------------------------------------------------------------------
   --  Floating-point values and the reals which they denote
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Order (F1, F2 : Dimensioned_Float) is
   begin
      null;
   end Lemma_Order;

   procedure Lemma_Nonnegative (F : Dimensioned_Float) is
   begin
      null;
   end Lemma_Nonnegative;

   procedure Lemma_Magnitude (F : Dimensioned_Float) is
   begin
      null;
   end Lemma_Magnitude;

   procedure Lemma_Bounded (F, Limit : Dimensioned_Float) is
   begin
      Lemma_Magnitude (F);
      Lemma_Order (abs F, Limit);
   end Lemma_Bounded;

   procedure Lemma_Within_Root (F : Dimensioned_Float) with Pre => abs R (F) <= Real_Root, Post => In_Range (F) is
   begin
      null;
   end Lemma_Within_Root;

   procedure Lemma_Two with Post => R (2.0) = 2.0 is
   begin
      null;
   end Lemma_Two;

   procedure Lemma_Same (F1, F2 : Dimensioned_Float) is
   begin
      Lemma_Order (F1 => F1, F2 => F2);
      Lemma_Order (F1 => F2, F2 => F1);
   end Lemma_Same;

   ------------------------------------------------------------------------------------------------------------------
   --  Floating-point arithmetic which the provers see by bit-blasting
   ------------------------------------------------------------------------------------------------------------------

   Small : constant Dimensioned_Float := 2#1.0#E-1025;
   --  A sum below this is exact, and SPARKlib's Eta is a small multiple of Epsilon times a sum which is not.

   Subnormal_Max : constant Dimensioned_Float := 2#1.0#E-1022;
   --  Values below this are not normal: they are the multiples of 2**(-1074).

   Scale : constant Dimensioned_Float := 2#1.0#E500;
   --  What Lemma_Sharp_Sum multiplies values which are not normal by.

   Unscale : constant Dimensioned_Float := 2#1.0#E-500;

   Gap : constant Dimensioned_Float := 2#1.0#E-574;
   --  Scale times 2**(-1074): the scaled values are its multiples.

   Real_Small : constant := 2.0**(-1025);
   Real_Scale : constant := 2.0**500;
   Real_Gap   : constant := 2.0**(-574);
   --  The reals which Small, Scale and Gap denote.

   procedure Lemma_Constants
   with Post => R (Small) = Real_Small and then R (Scale) = Real_Scale and then R (Gap) = Real_Gap
   is
   begin
      null;
   end Lemma_Constants;

   procedure Lemma_Sum_Above_Terms (F1, F2 : Dimensioned_Float)
   with
     Pre  => F1 >= 0.0 and then F1 <= Root and then F2 >= 0.0 and then F2 <= Root,
     Post => F1 + F2 >= 0.0 and then F1 <= F1 + F2 and then F2 <= F1 + F2
   is
   begin
      null;
   end Lemma_Sum_Above_Terms;

   function Is_Scaled_Grid_Sum (F1, F2 : Dimensioned_Float) return Boolean
   is ((F1 * Scale + F2 * Scale) * Unscale >= 0.0
       and then (F1 * Scale + F2 * Scale) * Unscale < Subnormal_Max
       and then ((F1 * Scale + F2 * Scale) * Unscale) * Scale = F1 * Scale + F2 * Scale)
   with Pre => F1 >= 0.0 and then F1 < Small and then F2 >= 0.0 and then F2 < Small;
   --  The sum of the scaled F1 and F2 is itself the scaling of a value which is not normal.

   Low_Split  : constant Dimensioned_Float := 2#1.0#E-1050;
   High_Split : constant Dimensioned_Float := 2#1.0#E-1038;
   --  Lemma_Grid is proved in three ranges of its larger operand, each of which the provers manage in under a
   --  second.

   procedure Lemma_Grid_Low (F1, F2 : Dimensioned_Float)
   with
     Pre  => F1 >= 0.0 and then F1 < Low_Split and then F2 >= 0.0 and then F2 < Low_Split,
     Post => Is_Scaled_Grid_Sum (F1, F2)
   is
   begin
      null;
   end Lemma_Grid_Low;

   procedure Lemma_Grid_Middle (F1, F2 : Dimensioned_Float)
   with
     Pre  =>
       F1 >= 0.0
       and then F1 < High_Split
       and then F2 >= 0.0
       and then F2 < High_Split
       and then (F1 >= Low_Split or else F2 >= Low_Split),
     Post => Is_Scaled_Grid_Sum (F1, F2)
   is
   begin
      null;
   end Lemma_Grid_Middle;

   procedure Lemma_Grid_High (F1, F2 : Dimensioned_Float)
   with
     Pre  =>
       F1 >= 0.0
       and then F1 < Small
       and then F2 >= 0.0
       and then F2 < Small
       and then (F1 >= High_Split or else F2 >= High_Split),
     Post => Is_Scaled_Grid_Sum (F1, F2)
   is
   begin
      null;
   end Lemma_Grid_High;

   procedure Lemma_Grid (F1, F2 : Dimensioned_Float)
   with
     Pre  => F1 >= 0.0 and then F1 < Small and then F2 >= 0.0 and then F2 < Small,
     Post => Is_Scaled_Grid_Sum (F1, F2)
   is
   begin
      if F1 >= High_Split or else F2 >= High_Split then
         Lemma_Grid_High (F1, F2);
      elsif F1 >= Low_Split or else F2 >= Low_Split then
         Lemma_Grid_Middle (F1, F2);
      else
         Lemma_Grid_Low (F1, F2);
      end if;
   end Lemma_Grid;
   --  The sum of two scaled values which are not normal is a scaled value which is not normal.

   procedure Lemma_Gap (F1, F2 : Dimensioned_Float)
   with
     Pre  => F1 >= 0.0 and then F1 < Subnormal_Max and then F2 >= 0.0 and then F2 < Subnormal_Max,
     Post => (if F1 * Scale < F2 * Scale then F1 * Scale + Gap <= F2 * Scale)
   is
   begin
      null;
   end Lemma_Gap;
   --  Two scaled values which are not normal are the same or at least Gap apart.

   procedure Lemma_Scaled_Range (F : Dimensioned_Float)
   with
     Pre  => F >= 0.0 and then F < Subnormal_Max,
     Post => F * Scale >= 0.0 and then F * Scale <= Root and then F * Scale + Gap <= Root
   is
   begin
      null;
   end Lemma_Scaled_Range;

   procedure Lemma_Double_Is_Sum (F : Dimensioned_Float)
   with Pre => F >= 0.0 and then F <= Root, Post => 2.0 * F = F + F
   is
   begin
      null;
   end Lemma_Double_Is_Sum;

   procedure Lemma_Quadruple_Is_Double (F : Dimensioned_Float)
   with Pre => F >= 0.0 and then F <= Root, Post => 4.0 * F = 2.0 * (2.0 * F)
   is
   begin
      null;
   end Lemma_Quadruple_Is_Double;

   procedure Lemma_Octuple_Is_Double (F : Dimensioned_Float)
   with Pre => F >= 0.0 and then F <= Root, Post => 8.0 * F = 2.0 * (4.0 * F)
   is
   begin
      null;
   end Lemma_Octuple_Is_Double;

   ------------------------------------------------------------------------------------------------------------------
   --  A sum of values which are not negative, over reals
   --
   --  X and Y are the terms and S the rounded sum. XK, YK and P are the rounded products of X, Y and S and the
   --  scale, and Q is the rounded sum of XK and YK.
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Large_Sum (X, Y, S : Real)
   with
     Pre  => X >= 0.0 and then Y >= 0.0 and then abs (S - (X + Y)) <= Epsilon * (X + Y) + Eta and then S >= Real_Small,
     Post => abs (S - (X + Y)) <= Sharp * (X + Y)
   is
   begin
      pragma Assert (One_More_Epsilon * (X + Y) + Eta >= Real_Small);
      pragma Assert (Eta <= 9.0 * Epsilon * (X + Y));
   end Lemma_Real_Large_Sum;

   procedure Lemma_Real_No_Gap_Below (X, Y, S, XK, YK, Q, P, G : Real)
   with
     Pre  =>
       X >= 0.0
       and then Y >= 0.0
       and then S >= 0.0
       and then S <= Real_Small
       and then abs (S - (X + Y)) <= Epsilon * (X + Y) + Eta
       and then abs (XK - Real_Scale * X) <= Epsilon * (Real_Scale * X) + Eta
       and then abs (YK - Real_Scale * Y) <= Epsilon * (Real_Scale * Y) + Eta
       and then abs (Q - (XK + YK)) <= Epsilon * abs (XK + YK) + Eta
       and then abs (P - Real_Scale * S) <= Epsilon * (Real_Scale * S) + Eta
       and then abs (G - (P + Real_Gap)) <= Epsilon * abs (P + Real_Gap) + Eta
       and then G <= Q,
     Post => False
   is
      W : constant Real := X + Y;
   begin
      pragma Assert (W <= Real_Small + 2.0 * Eta);
      pragma Assert (abs (XK + YK) <= One_More_Epsilon * (Real_Scale * W) + 2.0 * Eta);
      pragma Assert (Q <= Real_Scale * W + 3.0 * Epsilon * (Real_Scale * W) + 4.0 * Eta);
      pragma Assert (P >= Real_Scale * W - 3.0 * Epsilon * (Real_Scale * W) - Real_Scale * Eta - Eta);
      pragma Assert (P + Real_Gap >= 0.0);
      pragma Assert (G >= P + Real_Gap - Epsilon * (P + Real_Gap) - Eta);
   end Lemma_Real_No_Gap_Below;
   --  The scaled sum P is not Gap or more below Q: G is the rounding of P + Gap.

   procedure Lemma_Real_No_Gap_Above (X, Y, S, XK, YK, Q, P, G : Real)
   with
     Pre  =>
       X >= 0.0
       and then Y >= 0.0
       and then S >= 0.0
       and then S <= Real_Small
       and then abs (S - (X + Y)) <= Epsilon * (X + Y) + Eta
       and then abs (XK - Real_Scale * X) <= Epsilon * (Real_Scale * X) + Eta
       and then abs (YK - Real_Scale * Y) <= Epsilon * (Real_Scale * Y) + Eta
       and then abs (Q - (XK + YK)) <= Epsilon * abs (XK + YK) + Eta
       and then abs (P - Real_Scale * S) <= Epsilon * (Real_Scale * S) + Eta
       and then abs (G - (Q + Real_Gap)) <= Epsilon * abs (Q + Real_Gap) + Eta
       and then G <= P,
     Post => False
   is
      W : constant Real := X + Y;
   begin
      pragma Assert (W <= Real_Small + 2.0 * Eta);
      pragma Assert (abs (XK + YK) <= One_More_Epsilon * (Real_Scale * W) + 2.0 * Eta);
      pragma Assert (XK + YK >= Real_Scale * W - Epsilon * (Real_Scale * W) - 2.0 * Eta);
      pragma Assert (Q >= Real_Scale * W - 3.0 * Epsilon * (Real_Scale * W) - 4.0 * Eta);
      pragma Assert (P <= Real_Scale * W + 3.0 * Epsilon * (Real_Scale * W) + Real_Scale * Eta + 2.0 * Eta);
      pragma Assert (Q + Real_Gap >= 0.0);
      pragma Assert (G >= Q + Real_Gap - Epsilon * (Q + Real_Gap) - Eta);
   end Lemma_Real_No_Gap_Above;
   --  The scaled sum P is not Gap or more above Q: G is the rounding of Q + Gap.

   procedure Lemma_Real_Scaled_Sum (X, Y, S, XK, YK, Q : Real)
   with
     Pre  =>
       X >= 0.0
       and then Y >= 0.0
       and then S >= 0.0
       and then abs (XK - Real_Scale * X) <= Epsilon * (Real_Scale * X) + Eta
       and then abs (YK - Real_Scale * Y) <= Epsilon * (Real_Scale * Y) + Eta
       and then abs (Q - (XK + YK)) <= Epsilon * abs (XK + YK) + Eta
       and then abs (Q - Real_Scale * S) <= Epsilon * (Real_Scale * S) + Eta,
     Post => abs (S - (X + Y)) <= Sharp * (X + Y) + Sum_Residue
   is
      W : constant Real := X + Y;
   begin
      pragma Assert (abs (XK + YK) <= One_More_Epsilon * (Real_Scale * W) + 2.0 * Eta);
      pragma Assert (XK + YK >= Real_Scale * W - Epsilon * (Real_Scale * W) - 2.0 * Eta);
      pragma Assert (Q <= Real_Scale * W + 3.0 * Epsilon * (Real_Scale * W) + 4.0 * Eta);
      pragma Assert (Q >= Real_Scale * W - 3.0 * Epsilon * (Real_Scale * W) - 4.0 * Eta);
      pragma Assert (Real_Scale * S <= Real_Scale * W + 5.0 * Epsilon * (Real_Scale * W) + 6.0 * Eta);
      pragma Assert (Real_Scale * S >= Real_Scale * W - 5.0 * Epsilon * (Real_Scale * W) - 6.0 * Eta);
      pragma Assert (Real_Scale * Sum_Residue >= 6.0 * Eta);
   end Lemma_Real_Scaled_Sum;
   --  The scaled sum is Q: that makes the sum relative to its terms.

   ------------------------------------------------------------------------------------------------------------------
   --  One rounded sum of values which are not negative
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Sharp_Sum (F1, F2 : Dimensioned_Float) is
      S : constant Dimensioned_Float := F1 + F2;
   begin
      Lemma_Sum_Above_Terms (F1, F2);
      Lemma_Nonnegative (F1);
      Lemma_Nonnegative (F2);
      Lemma_Nonnegative (S);
      Lemma_Constants;
      Float_Lemmas.Lemma_Rounding_Error_Add (F1, F2);
      Lemma_Order (Small, S);

      if S >= Small then
         Lemma_Real_Large_Sum (R (F1), R (F2), R (S));
      else
         declare
            XK : constant Dimensioned_Float := F1 * Scale;
            YK : constant Dimensioned_Float := F2 * Scale;
            Q  : constant Dimensioned_Float := XK + YK;
            Z  : constant Dimensioned_Float := Q * Unscale;
            P  : constant Dimensioned_Float := S * Scale;
         begin
            Lemma_Grid (F1, F2);
            Lemma_Scaled_Range (F1);
            Lemma_Scaled_Range (F2);
            Lemma_Scaled_Range (S);
            Lemma_Scaled_Range (Z);
            Lemma_Gap (S, Z);
            Lemma_Gap (Z, S);
            pragma Assert (Z * Scale = Q);
            pragma Assert (if P < Q then P + Gap <= Q);
            pragma Assert (if Q < P then Q + Gap <= P);
            Float_Lemmas.Lemma_Rounding_Error_Mul (F1, Scale);
            Float_Lemmas.Lemma_Rounding_Error_Mul (F2, Scale);
            Float_Lemmas.Lemma_Rounding_Error_Mul (S, Scale);
            Float_Lemmas.Lemma_Rounding_Error_Add (XK, YK);

            if P < Q then
               Float_Lemmas.Lemma_Rounding_Error_Add (P, Gap);
               Lemma_Order (P + Gap, Q);
               Lemma_Real_No_Gap_Below (R (F1), R (F2), R (S), R (XK), R (YK), R (Q), R (P), R (P + Gap));
            elsif Q < P then
               Float_Lemmas.Lemma_Rounding_Error_Add (Q, Gap);
               Lemma_Order (Q + Gap, P);
               Lemma_Real_No_Gap_Above (R (F1), R (F2), R (S), R (XK), R (YK), R (Q), R (P), R (Q + Gap));
            end if;

            pragma Assert (P = Q);
            Lemma_Same (P, Q);
            Lemma_Real_Scaled_Sum (R (F1), R (F2), R (S), R (XK), R (YK), R (Q));
         end;
      end if;
   end Lemma_Sharp_Sum;

   ------------------------------------------------------------------------------------------------------------------
   --  From the operands of an operation to its result
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Exact (F : Dimensioned_Float; M : Real) is
   begin
      null;
   end Lemma_Exact;

   procedure Lemma_In_Range (X : Real; F : Dimensioned_Float; M, B, U, A : Real) is
   begin
      Lemma_Scale_Monotone (U, Slack_Max, M);
      pragma Assert (abs X <= B);
      pragma Assert (abs R (F) <= abs X + Slack_Max * M + A);
      pragma Assert (abs R (F) <= Magnitude_Max + Slack_Max * Magnitude_Max + Residue_Max);
      Lemma_Within_Root (F);
   end Lemma_In_Range;

   procedure Lemma_Sum (X1, X2 : Real; F1, F2 : Dimensioned_Float; M1, M2, B1, B2, U1, U2, A1, A2, U, A : Real) is
      W : constant Real := (if U1 >= U2 then U1 else U2);
   begin
      Lemma_In_Range (X1, F1, M1, B1, U1, A1);
      Lemma_In_Range (X2, F2, M2, B2, U2, A2);
      Float_Lemmas.Lemma_Rounding_Error_Add (F1, F2);
      Lemma_Real_Sum (X1, X2, R (F1), R (F2), R (F1 + F2), M1, M2, U1, U2, A1, A2, W, Eta, U, A);

      if Within_Limits (M1 + M2, B1 + B2, U, A) then
         Lemma_In_Range (X1 + X2, F1 + F2, M1 + M2, B1 + B2, U, A);
      end if;
   end Lemma_Sum;

   procedure Lemma_Sum_Nonnegative
     (X1, X2 : Real; F1, F2 : Dimensioned_Float; M1, M2, B1, B2, U1, U2, A1, A2, U, A : Real)
   is
      W : constant Real := (if U1 >= U2 then U1 else U2);
   begin
      Lemma_In_Range (X1, F1, M1, B1, U1, A1);
      Lemma_In_Range (X2, F2, M2, B2, U2, A2);
      Lemma_Nonnegative (F1);
      Lemma_Nonnegative (F2);
      Lemma_Sharp_Sum (F1, F2);
      Lemma_Real_Sum (X1, X2, R (F1), R (F2), R (F1 + F2), M1, M2, U1, U2, A1, A2, W, Sum_Residue, U, A);

      if Within_Limits (M1 + M2, B1 + B2, U, A) then
         Lemma_In_Range (X1 + X2, F1 + F2, M1 + M2, B1 + B2, U, A);
      end if;
   end Lemma_Sum_Nonnegative;

   procedure Lemma_Double_Close (X : Real; F : Dimensioned_Float; M, B, U, A, UD, AD : Real)
   with
     Pre  =>
       F >= 0.0
       and then Close (X, F, M, U, A)
       and then Within_Limits (M, B, U, A)
       and then B <= Half_Magnitude_Max
       and then UD >= U + Step
       and then UD <= Slack_Max
       and then AD >= 4.0 * A + Sum_Residue
       and then AD <= Residue_Max,
     Post =>
       2.0 * F >= 0.0
       and then Close (2.0 * X, 2.0 * F, 2.0 * M, UD, AD)
       and then Within_Limits (2.0 * M, 2.0 * B, UD, AD)
       and then In_Range (2.0 * F)
   is
   begin
      Lemma_In_Range (X, F, M, B, U, A);
      Lemma_Sum_Nonnegative (X, X, F, F, M, M, B, B, U, U, A, A, UD, AD);
      Lemma_Double_Is_Sum (F);
      Lemma_Same (2.0 * F, F + F);
      pragma Assert (X + X = 2.0 * X);
      pragma Assert (M + M = 2.0 * M);
      pragma Assert (B + B = 2.0 * B);
   end Lemma_Double_Close;
   --  Twice a value which is not negative, as the sum which it is.

   procedure Lemma_Double (F : Dimensioned_Float; B : Real) is
      X : constant Real := R (F);
   begin
      Lemma_Nonnegative (F);
      Lemma_Exact (F, X);
      Lemma_Double_Close (X, F, X, B, 0.0, 0.0, Slack_1, Scale_Residue);
   end Lemma_Double;

   procedure Lemma_Quadruple (F : Dimensioned_Float; B : Real) is
      X : constant Real := R (F);
   begin
      Lemma_Nonnegative (F);
      Lemma_Exact (F, X);
      Lemma_Double_Close (X, F, X, B, 0.0, 0.0, Slack_1, Sum_Residue);
      Lemma_Double_Close (2.0 * X, 2.0 * F, 2.0 * X, 2.0 * B, Slack_1, Sum_Residue, Slack_2, Scale_Residue);
      Lemma_In_Range (X, F, X, B, 0.0, 0.0);
      Lemma_Quadruple_Is_Double (F);
      Lemma_Same (4.0 * F, 2.0 * (2.0 * F));
      pragma Assert (2.0 * (2.0 * X) = 4.0 * X);
   end Lemma_Quadruple;

   procedure Lemma_Octuple (F : Dimensioned_Float; B : Real) is
      X         : constant Real := R (F);
      Residue_2 : constant := 2.0**(-1569);
   begin
      Lemma_Nonnegative (F);
      Lemma_Exact (F, X);
      Lemma_Double_Close (X, F, X, B, 0.0, 0.0, Slack_1, Sum_Residue);
      Lemma_Double_Close (2.0 * X, 2.0 * F, 2.0 * X, 2.0 * B, Slack_1, Sum_Residue, Slack_2, Residue_2);
      Lemma_In_Range (X, F, X, B, 0.0, 0.0);
      Lemma_Quadruple_Is_Double (F);
      Lemma_Same (4.0 * F, 2.0 * (2.0 * F));
      pragma Assert (2.0 * (2.0 * X) = 4.0 * X);
      pragma Assert (2.0 * (2.0 * B) = 4.0 * B);
      pragma Assert (Close (4.0 * X, 4.0 * F, 4.0 * X, Slack_2, Residue_2));
      Lemma_Double_Close (4.0 * X, 4.0 * F, 4.0 * X, 4.0 * B, Slack_2, Residue_2, Slack_3, Scale_Residue);
      Lemma_Octuple_Is_Double (F);
      Lemma_Same (8.0 * F, 2.0 * (4.0 * F));
      pragma Assert (2.0 * (4.0 * X) = 8.0 * X);
   end Lemma_Octuple;

   procedure Lemma_Product (X1, X2 : Real; F1, F2 : Dimensioned_Float; M1, M2, B1, B2, U1, U2, A1, A2, U, A : Real) is
   begin
      Lemma_In_Range (X1, F1, M1, B1, U1, A1);
      Lemma_In_Range (X2, F2, M2, B2, U2, A2);
      Float_Lemmas.Lemma_Rounding_Error_Mul (F1, F2);
      Lemma_Real_Product (X1, X2, R (F1), R (F2), R (F1 * F2), M1, M2, B1, B2, U1, U2, A1, A2, U, A);

      if Within_Limits (M1 * M2, B1 * B2, U, A) then
         Lemma_In_Range (X1 * X2, F1 * F2, M1 * M2, B1 * B2, U, A);
      end if;
   end Lemma_Product;

   procedure Lemma_Half (X : Real; F : Dimensioned_Float; M, B, U, A, UH, AH : Real) is
   begin
      Lemma_In_Range (X, F, M, B, U, A);
      Lemma_Two;
      Float_Lemmas.Lemma_Rounding_Error_Div (F, 2.0);
      Lemma_Real_Half (X, R (F), R (F / 2.0), M, U, A, UH, AH);

      if Within_Limits (M / 2.0, B / 2.0, UH, AH) then
         Lemma_In_Range (X / 2.0, F / 2.0, M / 2.0, B / 2.0, UH, AH);
      end if;
   end Lemma_Half;

   procedure Lemma_Meets (X : Real; F : Dimensioned_Float; M, U, A, Slack, Residue : Real) is
   begin
      Lemma_Scale_Monotone (U, Slack, M);
      pragma Assert (abs (R (F) - X) <= Slack * M + A);
      pragma Assert (A <= Residue);
   end Lemma_Meets;

end Prunt.Motion_Planner.Trajectory.Rounding;
