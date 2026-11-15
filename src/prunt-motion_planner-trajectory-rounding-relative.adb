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
pragma Warnings ("F");
--  A lemma can take a parameter only for its contract, which is not compiled.

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile. The
--  ghost code among them computes with exact rationals and would be far too slow to run.

package body Prunt.Motion_Planner.Trajectory.Rounding.Relative
  with SPARK_Mode => On
is

   ------------------------------------------------------------------------------------------------------------------
   --  How the proofs go
   --
   --  As in the parent package: each lemma calls the rounding lemma of Prunt.Motion_Planner.Reals for its operation
   --  and then a lemma over reals only, with a few scalar parameters and one algebraic step at a time. G1 and G2
   --  are the reals which the floating-point operands denote and GP, GQ, GS the real which the rounded result
   --  denotes.
   ------------------------------------------------------------------------------------------------------------------

   --  Powers of two which the proofs below name. A real is only made from a literal or from a named number.
   Two_M1000 : constant := 2.0**(-1000);
   Two_M600  : constant := 2.0**(-600);
   Two_M500  : constant := 2.0**(-500);
   Two_M400  : constant := 2.0**(-400);
   Two_M335  : constant := 2.0**(-335);
   Two_M334  : constant := 2.0**(-334);
   Two_M78   : constant := 2.0**(-78);
   Two_M75   : constant := 2.0**(-75);
   Two_M53   : constant := 2.0**(-53);
   Two_M52   : constant := 2.0**(-52);
   Two_M48   : constant := 2.0**(-48);
   Two_M37   : constant := 2.0**(-37);
   Two_M35   : constant := 2.0**(-35);
   Two_M28   : constant := 2.0**(-28);
   Two_M26   : constant := 2.0**(-26);
   Two_P171  : constant := 2.0**171;
   Two_P172  : constant := 2.0**172;
   Two_P511  : constant := 2.0**511;

   Two_P500 : constant := 2.0**500;

   Relative_Squared : constant := Relative_Max * Relative_Max;

   Negligible : constant := 2.0**(-115);
   --  Eta is at most this fraction of Normal_Min.

   One_More : constant := 1.0 + 2.0**(-26);
   --  A value within its bound is at most this many times its exact value.

   ------------------------------------------------------------------------------------------------------------------
   --  Real arithmetic
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Scale (A, B, C : Real) with Pre => A <= B and then C >= 0.0, Post => A * C <= B * C is
   begin
      null;
   end Lemma_Scale;

   procedure Lemma_Strict_Scale (A, B, C : Real) with Pre => A < B and then C > 0.0, Post => A * C < B * C is
   begin
      null;
   end Lemma_Strict_Scale;

   procedure Lemma_Monotone (A, B, C, D : Real)
   with Pre => A >= 0.0 and then A <= B and then C >= 0.0 and then C <= D, Post => A * C <= B * D
   is
   begin
      pragma Assert (A * C <= B * C);
      pragma Assert (B * C <= B * D);
   end Lemma_Monotone;

   procedure Lemma_Abs_Bound (X, Y, P, Q : Real)
   with Pre => abs X <= P and then abs Y <= Q, Post => abs (X * Y) <= P * Q
   is
   begin
      pragma Assert (abs (X * Y) = abs X * abs Y);
      Lemma_Monotone (abs X, P, abs Y, Q);
   end Lemma_Abs_Bound;

   procedure Lemma_Cancel (A, B, C : Real) with Pre => A * C <= B * C and then C > 0.0, Post => A <= B is
   begin
      if A > B then
         Lemma_Strict_Scale (A => B, B => A, C => C);
      end if;
   end Lemma_Cancel;
   --  A common positive factor of both sides of an inequality may be dropped.

   procedure Lemma_Tiny (P : Real) with Pre => P >= Normal_Min, Post => Eta <= Negligible * P is
   begin
      null;
   end Lemma_Tiny;

   procedure Lemma_Square_Order (A, B : Real)
   with Pre => A >= 0.0 and then B >= 0.0 and then A * A <= B * B, Post => A <= B
   is
   begin
      if A > B then
         Lemma_Strict_Scale (B, A, A);
         Lemma_Scale (B, A, B);
      end if;
   end Lemma_Square_Order;

   procedure Lemma_Cube_Order (A, B : Real)
   with Pre => A >= 0.0 and then B >= 0.0 and then A * A * A <= B * B * B, Post => A <= B
   is
   begin
      if A > B then
         Lemma_Monotone (B, A, B, A);
         pragma Assert (B * B <= A * A);
         pragma Assert (A * A > 0.0);
         Lemma_Strict_Scale (B, A, A * A);
         Lemma_Scale (B * B, A * A, B);
         pragma Assert (B * B * B <= A * A * B);
         pragma Assert (A * A * B < A * A * A);
      end if;
   end Lemma_Cube_Order;

   procedure Lemma_Cube_Monotone (A, B : Real) with Pre => A >= 0.0 and then A <= B, Post => A * A * A <= B * B * B is
   begin
      Lemma_Monotone (A, B, A, B);
      Lemma_Monotone (A * A, B * B, A, B);
   end Lemma_Cube_Monotone;

   ------------------------------------------------------------------------------------------------------------------
   --  Floating-point values and the reals which they denote
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Positive (F : Dimensioned_Float) with Pre => R (F) >= 0.0, Post => F >= 0.0 is
   begin
      null;
   end Lemma_Positive;

   procedure Lemma_Strictly_Below (F1, F2 : Dimensioned_Float) with Pre => R (F1) < R (F2), Post => F1 < F2 is
   begin
      Rounding.Lemma_Order (F1 => F2, F2 => F1);
   end Lemma_Strictly_Below;

   procedure Lemma_Nonnegative_Product (F1, F2 : Dimensioned_Float)
   with
     Pre  => F1 >= 0.0 and then F1 <= Operand_Max and then F2 >= 0.0 and then F2 <= Operand_Max,
     Post => F1 * F2 >= 0.0
   is
   begin
      null;
   end Lemma_Nonnegative_Product;

   procedure Lemma_Nonnegative_Quotient (F1, F2 : Dimensioned_Float)
   with Pre => F1 >= 0.0 and then F1 <= Operand_Max and then F2 >= Divisor_Min, Post => F1 / F2 >= 0.0
   is
   begin
      null;
   end Lemma_Nonnegative_Quotient;

   procedure Lemma_Nonnegative_Sum (F1, F2 : Dimensioned_Float)
   with
     Pre  => F1 >= 0.0 and then F1 <= Operand_Max and then F2 >= 0.0 and then F2 <= Operand_Max,
     Post => F1 + F2 >= 0.0
   is
   begin
      null;
   end Lemma_Nonnegative_Sum;

   ------------------------------------------------------------------------------------------------------------------
   --  The simple lemmas
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Near_Exact (F : Dimensioned_Float) is
   begin
      Rounding.Lemma_Nonnegative (F);
   end Lemma_Near_Exact;

   procedure Lemma_Near_Weaken (X : Real; F : Dimensioned_Float; U, V : Real) is
   begin
      Lemma_Scale (U, V, X);
   end Lemma_Near_Weaken;

   procedure Lemma_Near_Same (X : Real; F, G : Dimensioned_Float; U : Real) is
   begin
      Rounding.Lemma_Same (F, G);
   end Lemma_Near_Same;

   procedure Lemma_Near_Order (X1, X2 : Real; F1, F2 : Dimensioned_Float; U1, U2 : Real) is
   begin
      Rounding.Lemma_Order (F1, F2);
   end Lemma_Near_Order;

   ------------------------------------------------------------------------------------------------------------------
   --  A product
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Product_Error (X1, X2, G1, G2, U1, U2 : Real)
   with
     Pre  =>
       X1 >= 0.0
       and then X2 >= 0.0
       and then abs (G1 - X1) <= U1 * X1
       and then abs (G2 - X2) <= U2 * X2
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max,
     Post =>
       X1 * X2 >= 0.0
       and then abs (G1 * G2 - X1 * X2) <= (U1 + U2 + Relative_Squared) * (X1 * X2)
       and then abs (G1 * G2) <= One_More * (X1 * X2)
   is
      D1 : constant Real := G1 - X1;
      D2 : constant Real := G2 - X2;
      P  : constant Real := X1 * X2;
   begin
      Lemma_Monotone (0.0, X1, 0.0, X2);
      Lemma_Abs_Bound (D1, X2, U1 * X1, X2);
      Lemma_Abs_Bound (X1, D2, X1, U2 * X2);
      Lemma_Abs_Bound (D1, D2, U1 * X1, U2 * X2);
      pragma Assert (G1 * G2 - P = D1 * X2 + X1 * D2 + D1 * D2);
      pragma Assert ((U1 * X1) * X2 = U1 * P);
      pragma Assert (X1 * (U2 * X2) = U2 * P);
      pragma Assert ((U1 * X1) * (U2 * X2) = (U1 * U2) * P);
      Lemma_Monotone (U1, Relative_Max, U2, Relative_Max);
      Lemma_Scale (U1 * U2, Relative_Squared, P);
      pragma Assert (abs (G1 * G2 - P) <= U1 * P + U2 * P + Relative_Squared * P);
      Lemma_Scale (U1 + U2 + Relative_Squared, Two_M26, P);
      pragma Assert ((U1 + U2 + Relative_Squared) * P = U1 * P + U2 * P + Relative_Squared * P);
   end Lemma_Real_Product_Error;
   --  The product of two values within their bounds, before it is rounded.

   procedure Lemma_Real_Rounded (GP, GG, P, W, U : Real)
   with
     Pre  =>
       abs (GP - GG) <= Epsilon * abs GG + Eta
       and then P >= Normal_Min
       and then abs (GG - P) <= W * P
       and then abs GG <= One_More * P
       and then W >= 0.0
       and then U >= W + Two_M53 + Two_M78,
     Post => abs (GP - P) <= U * P
   is
   begin
      Lemma_Tiny (P);
      pragma Assert (Epsilon * abs GG <= Epsilon * (One_More * P));
      pragma Assert (abs (GP - P) <= Epsilon * (One_More * P) + Negligible * P + W * P);
      Lemma_Scale (W + Two_M53 + Two_M78, U, P);
      pragma Assert ((W + Two_M53 + Two_M78) * P = W * P + Two_M53 * P + Two_M78 * P);
   end Lemma_Real_Rounded;
   --  GP is the rounding of GG, which is within the fraction W of an exact value P that is not tiny.

   procedure Lemma_Near_Product (X1, X2 : Real; F1, F2 : Dimensioned_Float; U1, U2, U : Real) is
   begin
      Lemma_Nonnegative_Product (F1, F2);
      Float_Lemmas.Lemma_Rounding_Error_Mul (F1, F2);
      Lemma_Real_Product_Error (X1, X2, R (F1), R (F2), U1, U2);
      Lemma_Real_Rounded (R (F1 * F2), R (F1) * R (F2), X1 * X2, U1 + U2 + Relative_Squared, U);
   end Lemma_Near_Product;

   ------------------------------------------------------------------------------------------------------------------
   --  A quotient
   ------------------------------------------------------------------------------------------------------------------

   Quotient_Slack : constant := 2.0**(-54);
   --  More than the second-order terms of a quotient of two values within Relative_Max.

   procedure Lemma_Real_Quotient_Above (X1, X2, Q, G1, G2, H, U1, U2 : Real)
   with
     Pre  =>
       X1 >= 0.0
       and then X2 > 0.0
       and then Q * X2 = X1
       and then abs (G1 - X1) <= U1 * X1
       and then abs (G2 - X2) <= U2 * X2
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then G2 > 0.0
       and then H * G2 = G1,
     Post => Q >= 0.0 and then H <= Q + (U1 + U2 + Quotient_Slack) * Q
   is
      K : constant Real := 1.0 + U1 + U2 + Quotient_Slack;
   begin
      Lemma_Cancel (0.0, Q, X2);
      Lemma_Monotone (U1, Relative_Max, U2, Relative_Max);
      Lemma_Monotone (U2, Relative_Max, U2, Relative_Max);
      Lemma_Scale (U2, Relative_Max, Quotient_Slack);
      Lemma_Monotone (0.0, Q, 0.0, K);
      pragma Assert (K * (1.0 - U2) = 1.0 + U1 + Quotient_Slack - U1 * U2 - U2 * U2 - Quotient_Slack * U2);
      pragma Assert (1.0 + U1 <= K * (1.0 - U2));
      pragma Assert (G1 <= X1 + U1 * X1);
      pragma Assert (X1 + U1 * X1 = (Q * X2) * (1.0 + U1));
      Lemma_Monotone (0.0, Q, 0.0, X2);
      Lemma_Scale (1.0 + U1, K * (1.0 - U2), Q * X2);
      pragma Assert (G1 <= (K * (1.0 - U2)) * (Q * X2));
      pragma Assert ((K * (1.0 - U2)) * (Q * X2) = (Q * K) * (X2 - U2 * X2));
      pragma Assert (X2 - U2 * X2 <= G2);
      Lemma_Scale (X2 - U2 * X2, G2, Q * K);
      pragma Assert (H * G2 <= (Q * K) * G2);
      Lemma_Cancel (H, Q * K, G2);
      pragma Assert (Q * K = Q + (U1 + U2 + Quotient_Slack) * Q);
   end Lemma_Real_Quotient_Above;

   procedure Lemma_Real_Quotient_Below (X1, X2, Q, G1, G2, H, U1, U2 : Real)
   with
     Pre  =>
       X1 >= 0.0
       and then X2 > 0.0
       and then Q * X2 = X1
       and then Q >= 0.0
       and then abs (G1 - X1) <= U1 * X1
       and then abs (G2 - X2) <= U2 * X2
       and then U1 >= 0.0
       and then U1 <= Relative_Max
       and then U2 >= 0.0
       and then U2 <= Relative_Max
       and then G2 > 0.0
       and then H * G2 = G1,
     Post => H >= Q - (U1 + U2 + Quotient_Slack) * Q
   is
      K : constant Real := 1.0 - U1 - U2 - Quotient_Slack;
   begin
      Lemma_Monotone (U1, Relative_Max, U2, Relative_Max);
      Lemma_Monotone (U2, Relative_Max, U2, Relative_Max);
      Lemma_Scale (U2, Relative_Max, Quotient_Slack);
      Lemma_Monotone (0.0, U1, 0.0, U2);
      Lemma_Monotone (0.0, U2, 0.0, U2);
      Lemma_Scale (0.0, U2, Quotient_Slack);
      pragma Assert (K >= 0.0);
      pragma Assert (K * (1.0 + U2) = 1.0 - U1 - Quotient_Slack - U1 * U2 - U2 * U2 - Quotient_Slack * U2);
      pragma Assert (K * (1.0 + U2) <= 1.0 - U1);
      pragma Assert (G1 >= X1 - U1 * X1);
      pragma Assert (X1 - U1 * X1 = (Q * X2) * (1.0 - U1));
      Lemma_Monotone (0.0, Q, 0.0, X2);
      Lemma_Scale (K * (1.0 + U2), 1.0 - U1, Q * X2);
      pragma Assert (G1 >= (K * (1.0 + U2)) * (Q * X2));
      pragma Assert ((K * (1.0 + U2)) * (Q * X2) = (Q * K) * (X2 + U2 * X2));
      pragma Assert (G2 <= X2 + U2 * X2);
      Lemma_Monotone (0.0, Q, 0.0, K);
      Lemma_Scale (G2, X2 + U2 * X2, Q * K);
      pragma Assert ((Q * K) * G2 <= H * G2);
      Lemma_Cancel (Q * K, H, G2);
      pragma Assert (Q * K = Q - (U1 + U2 + Quotient_Slack) * Q);
   end Lemma_Real_Quotient_Below;

   procedure Lemma_Real_Divisor (X2, G2, U2 : Real)
   with
     Pre  => X2 > 0.0 and then abs (G2 - X2) <= U2 * X2 and then U2 >= 0.0 and then U2 <= Relative_Max,
     Post => G2 > 0.0
   is
   begin
      Lemma_Scale (U2, Relative_Max, X2);
   end Lemma_Real_Divisor;

   procedure Lemma_Near_Quotient (X1, X2, Q : Real; F1, F2 : Dimensioned_Float; U1, U2, U : Real) is
      G1 : constant Real := R (F1);
      G2 : constant Real := R (F2);
   begin
      Lemma_Nonnegative_Quotient (F1, F2);
      Float_Lemmas.Lemma_Rounding_Error_Div (F1, F2);
      Lemma_Real_Divisor (X2, G2, U2);
      declare
         H : constant Real := G1 / G2;
         W : constant Real := U1 + U2 + Quotient_Slack;
      begin
         pragma Assert (H * G2 = G1);
         Lemma_Real_Quotient_Above (X1, X2, Q, G1, G2, H, U1, U2);
         Lemma_Real_Quotient_Below (X1, X2, Q, G1, G2, H, U1, U2);
         pragma Assert (abs (H - Q) <= W * Q);
         Lemma_Scale (W, Two_M26, Q);
         pragma Assert (abs H <= One_More * Q);
         Lemma_Real_Rounded (R (F1 / F2), H, Q, W, U);
      end;
   end Lemma_Near_Quotient;

   ------------------------------------------------------------------------------------------------------------------
   --  A sum, and bounds relative to a reference magnitude
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Sum_Error (X1, X2, G1, G2, U1, U2, W : Real)
   with
     Pre  =>
       X1 >= 0.0
       and then X2 >= 0.0
       and then abs (G1 - X1) <= U1 * X1
       and then abs (G2 - X2) <= U2 * X2
       and then U1 >= 0.0
       and then U1 <= W
       and then U2 >= 0.0
       and then U2 <= W
       and then W <= Relative_Max,
     Post => abs (G1 + G2 - (X1 + X2)) <= W * (X1 + X2) and then abs (G1 + G2) <= One_More * (X1 + X2)
   is
   begin
      Lemma_Scale (U1, W, X1);
      Lemma_Scale (U2, W, X2);
      pragma Assert (W * (X1 + X2) = W * X1 + W * X2);
      Lemma_Scale (W, Two_M26, X1 + X2);
   end Lemma_Real_Sum_Error;

   procedure Lemma_Near_Sum (X1, X2 : Real; F1, F2 : Dimensioned_Float; U1, U2, U : Real) is
      W : constant Real := (if U1 >= U2 then U1 else U2);
   begin
      Lemma_Nonnegative_Sum (F1, F2);
      Float_Lemmas.Lemma_Rounding_Error_Add (F1, F2);
      Lemma_Real_Sum_Error (X1, X2, R (F1), R (F2), U1, U2, W);
      Lemma_Real_Rounded (R (F1 + F2), R (F1) + R (F2), X1 + X2, W, U);
   end Lemma_Near_Sum;

   procedure Lemma_Almost_From_Near (X : Real; F : Dimensioned_Float; U : Real) is
   begin
      null;
   end Lemma_Almost_From_Near;

   procedure Lemma_Scaled_Limits with Post => R (Scaled_Max) = Two_P500 and then R (Operand_Max) = Two_P511 is
   begin
      null;
   end Lemma_Scaled_Limits;

   procedure Lemma_Almost_Double (F : Dimensioned_Float) is
   begin
      Lemma_Scaled_Limits;
      Rounding.Lemma_Order (F, Scaled_Max);
      Rounding.Lemma_Nonnegative (F);
      Rounding.Lemma_Double (F, Two_P500);
      pragma Assert (R (2.0 * F) <= 2.0 * R (F) + Slack_1 * (2.0 * R (F)) + Scale_Residue);
      pragma Assert (R (2.0 * F) <= Two_P511);
      Rounding.Lemma_Order (2.0 * F, Operand_Max);
   end Lemma_Almost_Double;

   procedure Lemma_Almost_Quadruple (F : Dimensioned_Float) is
   begin
      Lemma_Scaled_Limits;
      Rounding.Lemma_Order (F, Scaled_Max);
      Rounding.Lemma_Nonnegative (F);
      Rounding.Lemma_Quadruple (F, Two_P500);
      pragma Assert (R (4.0 * F) <= 4.0 * R (F) + Slack_2 * (4.0 * R (F)) + Scale_Residue);
      pragma Assert (R (4.0 * F) <= Two_P511);
      Rounding.Lemma_Order (4.0 * F, Operand_Max);
   end Lemma_Almost_Quadruple;

   procedure Lemma_Real_Almost_Sum (X1, X2, G1, G2, U1, U2, W : Real)
   with
     Pre  =>
       X1 >= 0.0
       and then X2 >= 0.0
       and then X1 + X2 >= Normal_Min
       and then abs (G1 - X1) <= U1 * X1 + Residue
       and then abs (G2 - X2) <= U2 * X2 + Residue
       and then U1 >= 0.0
       and then U1 <= W
       and then U2 >= 0.0
       and then U2 <= W
       and then W <= Relative_Max,
     Post => abs (G1 + G2 - (X1 + X2)) <= (W + Two_M500) * (X1 + X2) and then abs (G1 + G2) <= One_More * (X1 + X2)
   is
      S : constant Real := X1 + X2;
   begin
      Lemma_Scale (U1, W, X1);
      Lemma_Scale (U2, W, X2);
      pragma Assert (W * S = W * X1 + W * X2);
      pragma Assert (2.0 * Residue <= Two_M500 * S);
      pragma Assert ((W + Two_M500) * S = W * S + Two_M500 * S);
      Lemma_Scale (W + Two_M500, Two_M26, S);
   end Lemma_Real_Almost_Sum;

   procedure Lemma_Near_Sum_Almost (X1, X2 : Real; F1, F2 : Dimensioned_Float; U1, U2, U : Real) is
      W : constant Real := (if U1 >= U2 then U1 else U2);
   begin
      Lemma_Nonnegative_Sum (F1, F2);
      Float_Lemmas.Lemma_Rounding_Error_Add (F1, F2);
      Lemma_Real_Almost_Sum (X1, X2, R (F1), R (F2), U1, U2, W);
      Lemma_Real_Rounded (R (F1 + F2), R (F1) + R (F2), X1 + X2, W + Two_M500, U);
   end Lemma_Near_Sum_Almost;

   procedure Lemma_Near_Bounded (X : Real; F, Limit : Dimensioned_Float; U : Real) is
   begin
      Lemma_Scale (U, 1.0, X);
      Rounding.Lemma_Order (F, Limit);
   end Lemma_Near_Bounded;

   procedure Lemma_Near_Bounds (X : Real; F : Dimensioned_Float; U : Real) is
   begin
      Rounding.Lemma_Nonnegative (F);
   end Lemma_Near_Bounds;

   procedure Lemma_Scale_Monotone (A, B, C : Real) is
   begin
      null;
   end Lemma_Scale_Monotone;

   procedure Lemma_Product_Monotone (A, B, C, D : Real) is
   begin
      Lemma_Monotone (A, B, C, D);
   end Lemma_Product_Monotone;

   procedure Lemma_Product_Cancel (A, B, C : Real) is
   begin
      Lemma_Cancel (A, B, C);
   end Lemma_Product_Cancel;

   procedure Lemma_Squares_Ordered (A, B : Real) is
   begin
      Lemma_Square_Order (A, B);
   end Lemma_Squares_Ordered;

   procedure Lemma_Cubes_Ordered (A, B : Real) is
   begin
      Lemma_Cube_Order (A, B);
   end Lemma_Cubes_Ordered;

   procedure Lemma_Within_From_Near (X : Real; F : Dimensioned_Float; M, U : Real) is
   begin
      Lemma_Scale (X, M, U);
   end Lemma_Within_From_Near;

   procedure Lemma_Real_Reference (GD, GG, XX, M, W, U : Real)
   with
     Pre  =>
       abs (GD - GG) <= Epsilon * abs GG + Eta
       and then M >= Normal_Min
       and then abs (GG - XX) <= W * M
       and then abs XX <= M
       and then W >= 0.0
       and then W <= Two_M26
       and then U >= W + 2.0 * Unit,
     Post => abs (GD - XX) <= U * M
   is
   begin
      Lemma_Tiny (M);
      Lemma_Scale (W, Two_M26, M);
      pragma Assert (abs GG <= One_More * M);
      pragma Assert (abs (GD - XX) <= Epsilon * (One_More * M) + Negligible * M + W * M);
      Lemma_Scale (W + 2.0 * Unit, U, M);
      pragma Assert ((W + 2.0 * Unit) * M = W * M + 2.0 * Unit * M);
   end Lemma_Real_Reference;
   --  GD is the rounding of GG, which is within the fraction W of the reference magnitude M from XX.

   procedure Lemma_Within_Difference (X1, X2 : Real; F1, F2 : Dimensioned_Float; M, U1, U2, U : Real) is
      W : constant Real := U1 + U2;
   begin
      Float_Lemmas.Lemma_Rounding_Error_Sub (F1, F2);
      pragma Assert (W * M = U1 * M + U2 * M);
      pragma Assert (abs (R (F1) - R (F2) - (X1 - X2)) <= W * M);
      pragma Assert (abs (X1 - X2) <= M);
      Lemma_Real_Reference (R (F1 - F2), R (F1) - R (F2), X1 - X2, M, W, U);
   end Lemma_Within_Difference;

   procedure Lemma_Within_Sum (X1, X2 : Real; F1, F2 : Dimensioned_Float; M, U1, U2, U : Real) is
      W : constant Real := U1 + U2;
   begin
      Float_Lemmas.Lemma_Rounding_Error_Add (F1, F2);
      pragma Assert (W * M = U1 * M + U2 * M);
      pragma Assert (abs (R (F1) + R (F2) - (X1 + X2)) <= W * M);
      Lemma_Real_Reference (R (F1 + F2), R (F1) + R (F2), X1 + X2, M, W, U);
   end Lemma_Within_Sum;

   procedure Lemma_Near_From_Within (X : Real; F : Dimensioned_Float; M, U, K, UK : Real) is
   begin
      Lemma_Scale (M, K * X, U);
      pragma Assert (U * M <= (K * U) * X);
      Lemma_Scale (K * U, UK, X);
   end Lemma_Near_From_Within;

   ------------------------------------------------------------------------------------------------------------------
   --  A slightly smaller and a slightly larger machine number
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Scaled (GS, GP, D, Low, High : Real)
   with
     Pre  =>
       GS >= Two_M600
       and then D > 0.0
       and then D <= 1.5
       and then abs (GP - GS * D) <= Epsilon * abs (GS * D) + Eta
       and then Low <= D - Two_M52
       and then High >= D + Two_M52,
     Post => GP >= Low * GS and then GP <= High * GS
   is
   begin
      pragma Assert (Eta <= Two_M400 * GS);
      pragma Assert (GS * D >= 0.0);
      Lemma_Scale (D, 1.5, GS);
      pragma Assert (Epsilon * (GS * D) <= Two_M53 * (1.5 * GS));
      pragma Assert (GP <= GS * D + Two_M53 * (1.5 * GS) + Two_M400 * GS);
      pragma Assert (GP >= GS * D - Two_M53 * (1.5 * GS) - Two_M400 * GS);
      pragma Assert (High * GS >= (D + Two_M52) * GS);
      pragma Assert (Low * GS <= (D - Two_M52) * GS);
   end Lemma_Real_Scaled;
   --  GP is the rounding of GS times D, for a GS which is not tiny: it is GS times D to within a little more
   --  than Epsilon. Low and High are named numbers below and above D by that much.

   ------------------------------------------------------------------------------------------------------------------
   --  A square root
   ------------------------------------------------------------------------------------------------------------------

   Root_Low  : constant Dimensioned_Float := 2#1.0#E-500;
   Root_High : constant Dimensioned_Float := 2#1.0#E511;
   --  The square root of a value from 2**-1000 to 2**1022 is between these.

   Shrink : constant Dimensioned_Float := 1.0 - 2#1.0#E-50;
   Grow   : constant Dimensioned_Float := 1.0 + 2#1.0#E-50;
   --  What the square root is multiplied by to give the brackets.

   Real_Shrink : constant := 1.0 - 2.0**(-50);
   Real_Grow   : constant := 1.0 + 2.0**(-50);

   Shrunk_Low  : constant := 1.0 - 2.0**(-49);
   Shrunk_High : constant := 1.0 - 2.0**(-51);
   Grown_Low   : constant := 1.0 + 2.0**(-51);
   Grown_High  : constant := 1.0 + 2.0**(-49);
   --  The rounded products are within these multiples of the square root.

   Square_Slack : constant := 2.0**(-47);
   --  The square of the square root is within this fraction of the radicand.

   procedure Lemma_Root_Constants
   with
     Post =>
       R (Root_Low) = Two_M500
       and then R (Root_High) = Two_P511
       and then R (Shrink) = Real_Shrink
       and then R (Grow) = Real_Grow
       and then R (0.0) = 0.0
   is
   begin
      null;
   end Lemma_Root_Constants;

   procedure Lemma_Real_Square_Above (GS, GP, G : Real)
   with
     Pre  => GS >= 0.0 and then GP >= Shrunk_Low * GS and then GP >= 0.0 and then GP * GP <= G,
     Post => GS * GS <= G + Square_Slack * G
   is
      Q : constant Real := GS * GS;
   begin
      Lemma_Monotone (Shrunk_Low * GS, GP, Shrunk_Low * GS, GP);
      pragma Assert ((Shrunk_Low * GS) * (Shrunk_Low * GS) = (Shrunk_Low * Shrunk_Low) * Q);
      pragma Assert ((Shrunk_Low * Shrunk_Low) * Q <= G);
      pragma Assert (Q >= 0.0);
      pragma Assert ((1.0 - Two_M48) * Q <= (Shrunk_Low * Shrunk_Low) * Q);
      pragma Assert (Q <= (1.0 + Square_Slack) * ((1.0 - Two_M48) * Q));
   end Lemma_Real_Square_Above;

   procedure Lemma_Real_Square_Below (GS, GQ, G : Real)
   with
     Pre  => GS >= 0.0 and then GQ <= Grown_High * GS and then GQ >= 0.0 and then GQ * GQ >= G and then G >= 0.0,
     Post => GS * GS >= G - Square_Slack * G
   is
      Q : constant Real := GS * GS;
   begin
      Lemma_Monotone (GQ, Grown_High * GS, GQ, Grown_High * GS);
      pragma Assert ((Grown_High * GS) * (Grown_High * GS) = (Grown_High * Grown_High) * Q);
      pragma Assert (G <= (Grown_High * Grown_High) * Q);
      pragma Assert (Q >= 0.0);
      pragma Assert ((Grown_High * Grown_High) * Q <= (1.0 + Square_Slack) * Q);
      pragma Assert ((1.0 - Square_Slack) * G <= (1.0 - Square_Slack) * ((1.0 + Square_Slack) * Q));
   end Lemma_Real_Square_Below;

   procedure Lemma_Square_Of_Root (F : Dimensioned_Float)
   with
     Pre  => F >= 0.0 and then F <= Operand_Max and then R (F) >= Two_M1000,
     Post =>
       Dimensionless_Math.Sqrt (F) >= 0.0
       and then R (Dimensionless_Math.Sqrt (F)) >= 0.0
       and then R (Dimensionless_Math.Sqrt (F)) * R (Dimensionless_Math.Sqrt (F)) <= R (F) + Square_Slack * R (F)
       and then R (Dimensionless_Math.Sqrt (F)) * R (Dimensionless_Math.Sqrt (F)) >= R (F) - Square_Slack * R (F)
   is
      G     : constant Real := R (F);
      Fs    : constant Dimensioned_Float := Dimensionless_Math.Sqrt (F);
      Gs    : constant Real := R (Fs);
      Below : Dimensioned_Float;
      Above : Dimensioned_Float;
   begin
      Lemma_Root_Constants;
      Rounding.Lemma_Order (F, Operand_Max);
      pragma Assert (R (Operand_Max) = Two_P511);
      Lemma_Square_Root_Is_Faithful (F, Root_Low, Root_High);
      Rounding.Lemma_Order (Root_Low, Fs);
      Rounding.Lemma_Order (Fs, Root_High);
      pragma Assert (Gs >= Two_M500 and then Gs <= Two_P511);

      --  A machine number a little below the root has a square below the radicand: were its square at least the
      --  radicand, the root could not be above it.
      Below := Fs * Shrink;
      Float_Lemmas.Lemma_Rounding_Error_Mul (Fs, Shrink);
      Lemma_Real_Scaled (Gs, R (Below), Real_Shrink, Shrunk_Low, Shrunk_High);
      pragma Assert (R (Below) < Gs);
      pragma Assert (R (Below) >= 0.0);
      Lemma_Positive (Below);
      if R (Below) * R (Below) >= G then
         Lemma_Square_Root_Is_Faithful (F, 0.0, Below);
         Rounding.Lemma_Order (Fs, Below);
         pragma Assert (False);
      end if;
      Lemma_Real_Square_Above (Gs, R (Below), G);

      --  And one a little above it has a square above the radicand.
      Above := Fs * Grow;
      Float_Lemmas.Lemma_Rounding_Error_Mul (Fs, Grow);
      Lemma_Real_Scaled (Gs, R (Above), Real_Grow, Grown_Low, Grown_High);
      pragma Assert (R (Above) > Gs);
      Lemma_Positive (Above);
      if R (Above) * R (Above) <= G then
         Lemma_Square_Root_Is_Faithful (F, Above, Root_High);
         Rounding.Lemma_Order (Above, Fs);
         pragma Assert (False);
      end if;
      Lemma_Real_Square_Below (Gs, R (Above), G);
   end Lemma_Square_Of_Root;
   --  The square of the library's square root is within one part in 2**47 of the radicand.

   procedure Lemma_Real_Root (X, S, G, GS, U, US : Real)
   with
     Pre  =>
       X >= 0.0
       and then S >= 0.0
       and then S * S = X
       and then abs (G - X) <= U * X
       and then U >= 0.0
       and then U <= Relative_Max
       and then GS >= 0.0
       and then GS * GS <= G + Square_Slack * G
       and then GS * GS >= G - Square_Slack * G
       and then US >= U + Root_Slack,
     Post => abs (GS - S) <= US * S
   is
      W : constant Real := U + Root_Slack;
   begin
      Lemma_Scale (U, Relative_Max, X);
      pragma Assert (G >= 0.0);
      pragma Assert (G <= X + U * X);
      pragma Assert (G >= X - U * X);
      pragma Assert (GS * GS <= (1.0 + Square_Slack) * (X + U * X));
      pragma Assert ((1.0 + Square_Slack) * (X + U * X) = X + (U + Square_Slack + Square_Slack * U) * X);
      Lemma_Scale (U, Relative_Max, Square_Slack);
      Lemma_Scale (U + Square_Slack + Square_Slack * U, W, X);
      pragma Assert (GS * GS <= X + W * X);
      pragma Assert (GS * GS >= (1.0 - Square_Slack) * (X - U * X));
      pragma Assert ((1.0 - Square_Slack) * (X - U * X) = X - (U + Square_Slack - Square_Slack * U) * X);
      Lemma_Scale (U + Square_Slack - Square_Slack * U, W, X);
      pragma Assert (GS * GS >= X - W * X);

      --  Above: the square of S * (1 + W) is at least X * (1 + W).
      pragma Assert ((S + W * S) * (S + W * S) = X + W * X + (W + W * W) * X);
      pragma Assert (W >= 0.0);
      Lemma_Scale (0.0, W + W * W, X);
      Lemma_Square_Order (GS, S + W * S);

      --  Below: the square of S * (1 - W) is at most X * (1 - W).
      pragma Assert (W <= 1.0);
      Lemma_Scale (W, 1.0, W);
      pragma Assert ((S - W * S) * (S - W * S) = X - W * X - (W - W * W) * X);
      pragma Assert (W - W * W >= 0.0);
      Lemma_Scale (0.0, W - W * W, X);
      Lemma_Scale (0.0, 1.0 - W, S);
      pragma Assert (S - W * S >= 0.0);
      Lemma_Square_Order (S - W * S, GS);

      Lemma_Scale (W, US, S);
   end Lemma_Real_Root;

   procedure Lemma_Real_Squared (X, G, Q, U, U2 : Real)
   with
     Pre  =>
       X >= 0.0
       and then abs (G - X) <= U * X
       and then U >= 0.0
       and then U <= Relative_Max
       and then Q <= G + Square_Slack * G
       and then Q >= G - Square_Slack * G
       and then U2 >= U + Root_Slack,
     Post => abs (Q - X) <= U2 * X
   is
      W : constant Real := U + Root_Slack;
   begin
      Lemma_Scale (U, Relative_Max, X);
      pragma Assert (G >= 0.0);
      pragma Assert (Q <= (1.0 + Square_Slack) * (X + U * X));
      pragma Assert ((1.0 + Square_Slack) * (X + U * X) = X + (U + Square_Slack + Square_Slack * U) * X);
      Lemma_Scale (U, Relative_Max, Square_Slack);
      Lemma_Scale (U + Square_Slack + Square_Slack * U, W, X);
      pragma Assert (Q >= (1.0 - Square_Slack) * (X - U * X));
      pragma Assert ((1.0 - Square_Slack) * (X - U * X) = X - (U + Square_Slack - Square_Slack * U) * X);
      Lemma_Scale (0.0, U, Square_Slack);
      Lemma_Scale (U + Square_Slack - Square_Slack * U, W, X);
      Lemma_Scale (W, U2, X);
   end Lemma_Real_Squared;
   --  Q is the square of a root of G, which is near X.

   procedure Lemma_Root_Squared (X : Real; F : Dimensioned_Float; U, U2 : Real) is
   begin
      Lemma_Scale (U, Relative_Max, X);
      pragma Assert (R (F) >= X - Relative_Max * X);
      pragma Assert (R (F) >= Two_M1000);
      Lemma_Square_Of_Root (F);
      Lemma_Real_Squared (X, R (F), R (Dimensionless_Math.Sqrt (F)) * R (Dimensionless_Math.Sqrt (F)), U, U2);
   end Lemma_Root_Squared;

   procedure Lemma_Near_Square_Root (X, S : Real; F : Dimensioned_Float; U, US : Real) is
   begin
      Lemma_Scale (U, Relative_Max, X);
      pragma Assert (R (F) >= X - Relative_Max * X);
      pragma Assert (R (F) >= Two_M1000);
      Lemma_Square_Of_Root (F);
      Lemma_Real_Root (X, S, R (F), R (Dimensionless_Math.Sqrt (F)), U, US);
   end Lemma_Near_Square_Root;

   ------------------------------------------------------------------------------------------------------------------
   --  A cube root
   ------------------------------------------------------------------------------------------------------------------

   Cube_Low  : constant Dimensioned_Float := 2#1.0#E-334;
   Cube_High : constant Dimensioned_Float := 2#1.0#E171;
   --  Machine numbers whose cubes are below 2**-1000 and above 2**511.

   Cube_Shrink : constant Dimensioned_Float := 1.0 - 2#1.0#E-39;
   Cube_Grow   : constant Dimensioned_Float := 1.0 + 2#1.0#E-39;
   --  What the cube root is multiplied by to give the brackets: a little further than the 2**-40 which is taken
   --  on trust about the library's cube root.

   Real_Cube_Shrink : constant := 1.0 - 2.0**(-39);
   Real_Cube_Grow   : constant := 1.0 + 2.0**(-39);

   Cube_Shrunk_Low  : constant := 1.0 - 2.0**(-39) - 2.0**(-52);
   Cube_Shrunk_High : constant := 1.0 - 2.0**(-39) + 2.0**(-52);
   Cube_Grown_Low   : constant := 1.0 + 2.0**(-39) - 2.0**(-52);
   Cube_Grown_High  : constant := 1.0 + 2.0**(-39) + 2.0**(-52);

   Cubed_Slack : constant := 2.0**(-36);
   --  The cube of the cube root is within this fraction of the radicand.

   procedure Lemma_Cube_Constants
   with
     Post =>
       R (Cube_Low) = Two_M334
       and then R (Cube_High) = Two_P171
       and then R (Cube_Shrink) = Real_Cube_Shrink
       and then R (Cube_Grow) = Real_Cube_Grow
       and then R (0.0) = 0.0
   is
   begin
      null;
   end Lemma_Cube_Constants;

   procedure Lemma_Real_Cube_Above (GS, GP, G : Real)
   with
     Pre  => GS >= 0.0 and then GP >= Cube_Shrunk_Low * GS and then GP >= 0.0 and then GP * GP * GP <= G,
     Post => GS * GS * GS <= G + Cubed_Slack * G
   is
      Q : constant Real := GS * GS * GS;
      L : constant Real := Cube_Shrunk_Low * GS;
   begin
      Lemma_Scale (0.0, Cube_Shrunk_Low, GS);
      Lemma_Cube_Monotone (L, GP);
      pragma Assert (L * L * L = (Cube_Shrunk_Low * Cube_Shrunk_Low * Cube_Shrunk_Low) * Q);
      pragma Assert ((Cube_Shrunk_Low * Cube_Shrunk_Low * Cube_Shrunk_Low) * Q <= G);
      Lemma_Monotone (0.0, GS, 0.0, GS);
      Lemma_Monotone (0.0, GS * GS, 0.0, GS);
      pragma Assert (Q >= 0.0);
      pragma Assert ((1.0 - Two_M37) * Q <= (Cube_Shrunk_Low * Cube_Shrunk_Low * Cube_Shrunk_Low) * Q);
      pragma Assert (Q <= (1.0 + Cubed_Slack) * ((1.0 - Two_M37) * Q));
   end Lemma_Real_Cube_Above;

   procedure Lemma_Real_Cube_Below (GS, GQ, G : Real)
   with
     Pre  =>
       GS >= 0.0 and then GQ <= Cube_Grown_High * GS and then GQ >= 0.0 and then GQ * GQ * GQ >= G and then G >= 0.0,
     Post => GS * GS * GS >= G - Cubed_Slack * G
   is
      Q : constant Real := GS * GS * GS;
      H : constant Real := Cube_Grown_High * GS;
   begin
      Lemma_Cube_Monotone (GQ, H);
      pragma Assert (H * H * H = (Cube_Grown_High * Cube_Grown_High * Cube_Grown_High) * Q);
      pragma Assert (G <= (Cube_Grown_High * Cube_Grown_High * Cube_Grown_High) * Q);
      Lemma_Monotone (0.0, GS, 0.0, GS);
      Lemma_Monotone (0.0, GS * GS, 0.0, GS);
      pragma Assert (Q >= 0.0);
      pragma Assert ((Cube_Grown_High * Cube_Grown_High * Cube_Grown_High) * Q <= (1.0 + Cubed_Slack) * Q);
      pragma Assert ((1.0 - Cubed_Slack) * G <= (1.0 - Cubed_Slack) * ((1.0 + Cubed_Slack) * Q));
   end Lemma_Real_Cube_Below;

   procedure Lemma_Real_Not_Above (GS, GP : Real)
   with
     Pre  => GS >= Two_M600 and then GP <= Cube_Shrunk_High * GS and then GS <= GP + Cube_Root_Slack * GP,
     Post => False
   is
   begin
      pragma Assert (GP >= 0.0);
      pragma Assert (GP + Cube_Root_Slack * GP <= (1.0 + Cube_Root_Slack) * (Cube_Shrunk_High * GS));
      pragma Assert ((1.0 + Cube_Root_Slack) * (Cube_Shrunk_High * GS) < GS);
   end Lemma_Real_Not_Above;
   --  A cube root cannot be within the trusted accuracy below a bracket which is that far below it.

   procedure Lemma_Real_Not_Below (GS, GQ : Real)
   with
     Pre  => GS >= Two_M600 and then GQ >= Cube_Grown_Low * GS and then GS >= GQ - Cube_Root_Slack * GQ,
     Post => False
   is
   begin
      pragma Assert (GQ >= 0.0);
      pragma Assert (GQ - Cube_Root_Slack * GQ >= (1.0 - Cube_Root_Slack) * (Cube_Grown_Low * GS));
      pragma Assert ((1.0 - Cube_Root_Slack) * (Cube_Grown_Low * GS) > GS);
   end Lemma_Real_Not_Below;

   procedure Lemma_Cube_Of_Root (F : Dimensioned_Float)
   with
     Pre  => F >= 0.0 and then F <= Operand_Max and then R (F) >= Two_M1000,
     Post =>
       Cube_Root (F) >= 0.0
       and then R (Cube_Root (F)) >= 0.0
       and then R (Cube_Root (F)) * R (Cube_Root (F)) * R (Cube_Root (F)) <= R (F) + Cubed_Slack * R (F)
       and then R (Cube_Root (F)) * R (Cube_Root (F)) * R (Cube_Root (F)) >= R (F) - Cubed_Slack * R (F)
   is
      G     : constant Real := R (F);
      Fs    : constant Dimensioned_Float := Cube_Root (F);
      Gs    : constant Real := R (Fs);
      Below : Dimensioned_Float;
      Above : Dimensioned_Float;
   begin
      Lemma_Cube_Constants;
      Rounding.Lemma_Order (F, Operand_Max);
      pragma Assert (R (Operand_Max) = Two_P511);
      pragma Assert (R (Cube_Low) * R (Cube_Low) * R (Cube_Low) <= G);
      pragma Assert (G <= R (Cube_High) * R (Cube_High) * R (Cube_High));
      Lemma_Cube_Root_Is_Close (F, Cube_Low, Cube_High);
      Rounding.Lemma_Nonnegative (Fs);
      pragma Assert (Gs >= Two_M335 and then Gs <= Two_P172);
      Lemma_Strictly_Below (Fs, Operand_Max);

      Below := Fs * Cube_Shrink;
      Float_Lemmas.Lemma_Rounding_Error_Mul (Fs, Cube_Shrink);
      Lemma_Real_Scaled (Gs, R (Below), Real_Cube_Shrink, Cube_Shrunk_Low, Cube_Shrunk_High);
      pragma Assert (R (Below) >= 0.0);
      Lemma_Positive (Below);
      if R (Below) * R (Below) * R (Below) >= G then
         Lemma_Cube_Root_Is_Close (F, 0.0, Below);
         Lemma_Real_Not_Above (Gs, R (Below));
      end if;
      Lemma_Real_Cube_Above (Gs, R (Below), G);

      Above := Fs * Cube_Grow;
      Float_Lemmas.Lemma_Rounding_Error_Mul (Fs, Cube_Grow);
      Lemma_Real_Scaled (Gs, R (Above), Real_Cube_Grow, Cube_Grown_Low, Cube_Grown_High);
      pragma Assert (R (Above) >= 0.0);
      Lemma_Positive (Above);
      if R (Above) * R (Above) * R (Above) <= G then
         Lemma_Cube_Root_Is_Close (F, Above, Cube_High);
         Lemma_Real_Not_Below (Gs, R (Above));
      end if;
      Lemma_Real_Cube_Below (Gs, R (Above), G);
   end Lemma_Cube_Of_Root;
   --  The cube of the library's cube root is within one part in 2**36 of the radicand.

   procedure Lemma_Real_Cube_Root (X, S, G, GS, U, US : Real)
   with
     Pre  =>
       X >= 0.0
       and then S >= 0.0
       and then S * S * S = X
       and then abs (G - X) <= U * X
       and then U >= 0.0
       and then U <= Relative_Max
       and then GS >= 0.0
       and then GS * GS * GS <= G + Cubed_Slack * G
       and then GS * GS * GS >= G - Cubed_Slack * G
       and then US >= U + Cube_Root_Step,
     Post => abs (GS - S) <= US * S
   is
      W  : constant Real := U + Two_M35;
      H  : constant Real := W / 2.0;
      S2 : constant Real := S * S;
   begin
      Lemma_Scale (U, Relative_Max, X);
      pragma Assert (G >= 0.0);
      pragma Assert (GS * GS * GS <= (1.0 + Cubed_Slack) * (X + U * X));
      pragma Assert ((1.0 + Cubed_Slack) * (X + U * X) = X + (U + Cubed_Slack + Cubed_Slack * U) * X);
      Lemma_Scale (U, Relative_Max, Cubed_Slack);
      Lemma_Scale (U + Cubed_Slack + Cubed_Slack * U, W, X);
      pragma Assert (GS * GS * GS <= X + W * X);
      pragma Assert (GS * GS * GS >= (1.0 - Cubed_Slack) * (X - U * X));
      pragma Assert ((1.0 - Cubed_Slack) * (X - U * X) = X - (U + Cubed_Slack - Cubed_Slack * U) * X);
      Lemma_Scale (U + Cubed_Slack - Cubed_Slack * U, W, X);
      pragma Assert (GS * GS * GS >= X - W * X);

      --  Above: the cube of S * (1 + H) is at least X * (1 + 3 * H), which is more than X * (1 + W).
      Lemma_Monotone (0.0, S, 0.0, S);
      pragma Assert (S2 >= 0.0);
      pragma Assert ((S + H * S) * (S + H * S) * (S + H * S) = X + (3.0 * H) * X + (3.0 * H * H + H * H * H) * X);
      pragma Assert (H >= 0.0);
      Lemma_Monotone (0.0, H, 0.0, H);
      Lemma_Monotone (0.0, H * H, 0.0, H);
      Lemma_Scale (0.0, 3.0 * H * H + H * H * H, X);
      pragma Assert ((3.0 * H) * X >= W * X);
      Lemma_Scale (0.0, H, S);
      Lemma_Cube_Order (GS, S + H * S);

      --  Below: the cube of S * (1 - H) is at most X * (1 - W).
      pragma Assert (H <= Two_M28);
      pragma Assert ((S - H * S) * (S - H * S) * (S - H * S) = X - (3.0 * H) * X + (3.0 * H * H - H * H * H) * X);
      Lemma_Scale (H, Two_M28, H);
      pragma Assert (3.0 * H * H - H * H * H <= H);
      Lemma_Scale (3.0 * H * H - H * H * H, H, X);
      pragma Assert ((3.0 * H) * X = W * X + H * X);
      pragma Assert ((S - H * S) * (S - H * S) * (S - H * S) <= X - W * X);
      Lemma_Scale (0.0, 1.0 - H, S);
      pragma Assert (S - H * S >= 0.0);
      Lemma_Cube_Order (S - H * S, GS);

      Lemma_Scale (H, US, S);
   end Lemma_Real_Cube_Root;

   procedure Lemma_Real_Cubed (X, G, Q, U, U3 : Real)
   with
     Pre  =>
       X >= 0.0
       and then abs (G - X) <= U * X
       and then U >= 0.0
       and then U <= Relative_Max
       and then Q <= G + Cubed_Slack * G
       and then Q >= G - Cubed_Slack * G
       and then U3 >= U + Cubed_Step,
     Post => abs (Q - X) <= U3 * X
   is
      W : constant Real := U + Cubed_Step;
   begin
      Lemma_Scale (U, Relative_Max, X);
      pragma Assert (G >= 0.0);
      pragma Assert (Q <= (1.0 + Cubed_Slack) * (X + U * X));
      pragma Assert ((1.0 + Cubed_Slack) * (X + U * X) = X + (U + Cubed_Slack + Cubed_Slack * U) * X);
      Lemma_Scale (U, Relative_Max, Cubed_Slack);
      Lemma_Scale (U + Cubed_Slack + Cubed_Slack * U, W, X);
      pragma Assert (Q >= (1.0 - Cubed_Slack) * (X - U * X));
      pragma Assert ((1.0 - Cubed_Slack) * (X - U * X) = X - (U + Cubed_Slack - Cubed_Slack * U) * X);
      Lemma_Scale (0.0, U, Cubed_Slack);
      Lemma_Scale (U + Cubed_Slack - Cubed_Slack * U, W, X);
      Lemma_Scale (W, U3, X);
   end Lemma_Real_Cubed;

   procedure Lemma_Root_Cubed (X : Real; F : Dimensioned_Float; U, U3 : Real) is
   begin
      Lemma_Scale (U, Relative_Max, X);
      pragma Assert (R (F) >= X - Relative_Max * X);
      pragma Assert (R (F) >= Two_M1000);
      Lemma_Cube_Of_Root (F);
      Lemma_Real_Cubed (X, R (F), R (Cube_Root (F)) * R (Cube_Root (F)) * R (Cube_Root (F)), U, U3);
   end Lemma_Root_Cubed;

   procedure Lemma_Near_Cube_Root (X, S : Real; F : Dimensioned_Float; U, US : Real) is
   begin
      Lemma_Scale (U, Relative_Max, X);
      pragma Assert (R (F) >= X - Relative_Max * X);
      pragma Assert (R (F) >= Two_M1000);
      Lemma_Cube_Of_Root (F);
      Lemma_Real_Cube_Root (X, S, R (F), R (Cube_Root (F)), U, US);
   end Lemma_Near_Cube_Root;

   ------------------------------------------------------------------------------------------------------------------
   --  Relative bounds between reals
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Fractions (U1, U2 : Real)
   with
     Pre  => U1 >= 0.0 and then U1 <= Relative_Max and then U2 >= 0.0 and then U2 <= Relative_Max,
     Post => U1 * U2 >= 0.0 and then U1 * U2 <= Relative_Squared
   is
   begin
      Lemma_Monotone (0.0, U1, 0.0, U2);
      Lemma_Monotone (U1, Relative_Max, U2, Relative_Max);
   end Lemma_Fractions;

   procedure Lemma_About_From_Near (X : Real; F : Dimensioned_Float; U : Real) is
   begin
      Rounding.Lemma_Nonnegative (F);
   end Lemma_About_From_Near;

   procedure Lemma_About_Same (X : Real) is
   begin
      null;
   end Lemma_About_Same;

   procedure Lemma_About_Weaken (X, Y, U, V : Real) is
   begin
      Lemma_Scale (U, V, Y);
   end Lemma_About_Weaken;

   procedure Lemma_About_Bounds (X, Y, U : Real) is
   begin
      Lemma_Scale (U, Relative_Max, Y);
   end Lemma_About_Bounds;

   procedure Lemma_About_Symmetric (X, Y, U, V : Real) is
   begin
      Lemma_Scale (U, Relative_Max, Y);
      pragma Assert (X >= Y - Relative_Max * Y);
      pragma Assert (X >= 0.0);
      --  Y is at most X * (1 + 2 * U), so U * Y is at most (U + 2 * U * U) * X.
      Lemma_Fractions (U, U);
      Lemma_Scale (U, Relative_Max, U);
      pragma Assert ((1.0 + 2.0 * U) * (Y - U * Y) = Y + (U - 2.0 * (U * U)) * Y);
      pragma Assert (U - 2.0 * (U * U) >= 0.0);
      Lemma_Scale (0.0, U - 2.0 * (U * U), Y);
      Lemma_Scale (Y - U * Y, X, 1.0 + 2.0 * U);
      pragma Assert (Y <= (1.0 + 2.0 * U) * X);
      Lemma_Scale (Y, (1.0 + 2.0 * U) * X, U);
      pragma Assert (U * Y <= (U + 2.0 * (U * U)) * X);
      Lemma_Scale (U + 2.0 * (U * U), V, X);
   end Lemma_About_Symmetric;

   procedure Lemma_About_Chain (X, Y, Z, U, V, W : Real) is
   begin
      Lemma_Fractions (U, V);
      Lemma_Scale (0.0, U, Z);
      Lemma_Scale (0.0, V, Z);
      pragma Assert (Y <= Z + V * Z);
      Lemma_Scale (Y, Z + V * Z, U);
      pragma Assert (U * Y <= U * Z + (U * V) * Z);
      Lemma_Scale (U * V, Relative_Squared, Z);
      pragma Assert (abs (X - Z) <= U * Z + V * Z + Relative_Squared * Z);
      Lemma_Scale (U + V + Relative_Squared, W, Z);
      pragma Assert ((U + V + Relative_Squared) * Z = U * Z + V * Z + Relative_Squared * Z);
   end Lemma_About_Chain;

   procedure Lemma_About_Product (X1, Y1, X2, Y2, U1, U2, U : Real) is
      D1 : constant Real := X1 - Y1;
      D2 : constant Real := X2 - Y2;
      P  : constant Real := Y1 * Y2;
   begin
      Lemma_Monotone (0.0, Y1, 0.0, Y2);
      Lemma_Abs_Bound (D1, Y2, U1 * Y1, Y2);
      Lemma_Abs_Bound (Y1, D2, Y1, U2 * Y2);
      Lemma_Abs_Bound (D1, D2, U1 * Y1, U2 * Y2);
      pragma Assert (X1 * X2 - P = D1 * Y2 + Y1 * D2 + D1 * D2);
      pragma Assert ((U1 * Y1) * Y2 = U1 * P);
      pragma Assert (Y1 * (U2 * Y2) = U2 * P);
      pragma Assert ((U1 * Y1) * (U2 * Y2) = (U1 * U2) * P);
      Lemma_Fractions (U1, U2);
      Lemma_Scale (U1 * U2, Relative_Squared, P);
      pragma Assert (abs (X1 * X2 - P) <= U1 * P + U2 * P + Relative_Squared * P);
      Lemma_Scale (U1 + U2 + Relative_Squared, U, P);
      pragma Assert ((U1 + U2 + Relative_Squared) * P = U1 * P + U2 * P + Relative_Squared * P);
   end Lemma_About_Product;

   procedure Lemma_About_Sum (X1, Y1, X2, Y2, U1, U2, U : Real) is
   begin
      Lemma_Scale (U1, U, Y1);
      Lemma_Scale (U2, U, Y2);
      pragma Assert (U * (Y1 + Y2) = U * Y1 + U * Y2);
   end Lemma_About_Sum;

   procedure Lemma_About_Scaled (X, Y, K, U : Real) is
   begin
      Lemma_Abs_Bound (K, X - Y, K, U * Y);
      pragma Assert (K * X - K * Y = K * (X - Y));
      pragma Assert (K * (U * Y) = U * (K * Y));
      Lemma_Monotone (0.0, K, 0.0, Y);
   end Lemma_About_Scaled;

   procedure Lemma_About_Cancel (X, Y, K, U : Real) is
   begin
      pragma Assert (abs (K * X - K * Y) = K * abs (X - Y));
      pragma Assert (U * (K * Y) = (U * Y) * K);
      Lemma_Cancel (abs (X - Y), U * Y, K);
   end Lemma_About_Cancel;

   procedure Lemma_About_Quotient (X1, Y1, X2, Y2, QX, QY, U1, U2, U : Real) is
      W : constant Real := U1 + U2 + Quotient_Slack;
   begin
      Lemma_Scale (0.0, U1, Y1);
      pragma Assert (Y1 >= 0.0);
      Lemma_Real_Divisor (Y2, X2, U2);
      Lemma_Real_Quotient_Above (Y1, Y2, QY, X1, X2, QX, U1, U2);
      Lemma_Real_Quotient_Below (Y1, Y2, QY, X1, X2, QX, U1, U2);
      pragma Assert (abs (QX - QY) <= W * QY);
      Lemma_Scale (W, U, QY);
   end Lemma_About_Quotient;

   procedure Lemma_At_Most_Chain (X, Y, Z, U, V, W : Real) is
   begin
      Lemma_Fractions (U, V);
      Lemma_Scale (0.0, V, Z);
      pragma Assert (Y <= Z + V * Z);
      Lemma_Scale (Y, Z + V * Z, U);
      pragma Assert (U * Y <= U * Z + (U * V) * Z);
      Lemma_Scale (U * V, Relative_Squared, Z);
      pragma Assert (X <= Z + V * Z + U * Z + Relative_Squared * Z);
      Lemma_Scale (U + V + Relative_Squared, W, Z);
      pragma Assert ((U + V + Relative_Squared) * Z = U * Z + V * Z + Relative_Squared * Z);
   end Lemma_At_Most_Chain;

   procedure Lemma_At_Most_Product (X1, Y1, X2, Y2, U1, U2, U : Real) is
      P : constant Real := Y1 * Y2;
   begin
      Lemma_Monotone (0.0, Y1, 0.0, Y2);
      Lemma_Monotone (X1, Y1 + U1 * Y1, X2, Y2 + U2 * Y2);
      pragma Assert ((Y1 + U1 * Y1) * (Y2 + U2 * Y2) = P + U1 * P + U2 * P + (U1 * U2) * P);
      Lemma_Fractions (U1, U2);
      Lemma_Scale (U1 * U2, Relative_Squared, P);
      pragma Assert (X1 * X2 <= P + U1 * P + U2 * P + Relative_Squared * P);
      Lemma_Scale (U1 + U2 + Relative_Squared, U, P);
      pragma Assert ((U1 + U2 + Relative_Squared) * P = U1 * P + U2 * P + Relative_Squared * P);
   end Lemma_At_Most_Product;

   procedure Lemma_At_Most_Sum (X1, Y1, X2, Y2, U1, U2, U : Real) is
   begin
      Lemma_Scale (U1, U, Y1);
      Lemma_Scale (U2, U, Y2);
      pragma Assert (U * (Y1 + Y2) = U * Y1 + U * Y2);
   end Lemma_At_Most_Sum;

   procedure Lemma_At_Most_Cancel (X, Y, K, U : Real) is
   begin
      pragma Assert (K * Y + U * (K * Y) = (Y + U * Y) * K);
      Lemma_Cancel (X, Y + U * Y, K);
   end Lemma_At_Most_Cancel;

   procedure Lemma_At_Most_Scaled (X, Y, K, U : Real) is
   begin
      Lemma_Scale (X, Y + U * Y, K);
      pragma Assert (K * Y + U * (K * Y) = (Y + U * Y) * K);
      Lemma_Monotone (0.0, K, 0.0, Y);
   end Lemma_At_Most_Scaled;

   procedure Lemma_At_Most_From_About (X, Y, U : Real) is
   begin
      null;
   end Lemma_At_Most_From_About;

   procedure Lemma_At_Most_Weaken (X, Y, U, V : Real) is
   begin
      Lemma_Scale (U, V, Y);
   end Lemma_At_Most_Weaken;

   procedure Lemma_At_Most_Root (X, Y, U : Real) is
      Z : constant Real := Y + U * Y;
   begin
      Lemma_Scale (0.0, U, Y);
      Lemma_Monotone (0.0, Y, 0.0, Y);
      pragma Assert (Z * Z = Y * Y + U * (Y * Y) + (U + U * U) * (Y * Y));
      Lemma_Monotone (0.0, U, 0.0, U);
      Lemma_Scale (0.0, U + U * U, Y * Y);
      Lemma_Square_Order (X, Z);
   end Lemma_At_Most_Root;

   procedure Lemma_At_Most_Cube_Root (X, Y, U : Real) is
      Z  : constant Real := Y + U * Y;
      Y3 : constant Real := Y * Y * Y;
   begin
      Lemma_Scale (0.0, U, Y);
      Lemma_Monotone (0.0, Y, 0.0, Y);
      Lemma_Monotone (0.0, Y * Y, 0.0, Y);
      pragma Assert (Z * Z * Z = Y3 + U * Y3 + (2.0 * U + 3.0 * U * U + U * U * U) * Y3);
      Lemma_Monotone (0.0, U, 0.0, U);
      Lemma_Monotone (0.0, U * U, 0.0, U);
      Lemma_Scale (0.0, 2.0 * U + 3.0 * U * U + U * U * U, Y3);
      Lemma_Cube_Order (X, Z);
   end Lemma_At_Most_Cube_Root;

   procedure Lemma_At_Most_Fourth_Root (X, Y, U : Real) is
   begin
      Lemma_Monotone (0.0, X, 0.0, X);
      Lemma_Monotone (0.0, Y, 0.0, Y);
      Lemma_At_Most_Root (X * X, Y * Y, U);
      Lemma_At_Most_Root (X, Y, U);
   end Lemma_At_Most_Fourth_Root;

   ------------------------------------------------------------------------------------------------------------------
   --  The next machine number
   ------------------------------------------------------------------------------------------------------------------

   Tiny : constant Dimensioned_Float := 2#1.0#E-1000;

   Step_Up : constant Dimensioned_Float := 1.0 + 2#1.0#E-52;
   --  The machine number after one.

   Real_Step_Up : constant := 1.0 + 2.0**(-52);

   procedure Lemma_Next_Constants with Post => R (Tiny) = Next_Residue and then R (Step_Up) = Real_Step_Up is
   begin
      null;
   end Lemma_Next_Constants;

   procedure Lemma_Real_Stepped (GX, GY : Real)
   with
     Pre  => GX >= Next_Residue and then abs (GY - GX * Real_Step_Up) <= Epsilon * abs (GX * Real_Step_Up) + Eta,
     Post => GY > GX and then GY <= GX + Next_Slack * GX
   is
   begin
      pragma Assert (Eta <= Two_M75 * GX);
      pragma Assert (GX * Real_Step_Up >= 0.0);
      pragma Assert (GY >= GX * Real_Step_Up - Epsilon * (GX * Real_Step_Up) - Two_M75 * GX);
      pragma Assert (GY <= GX * Real_Step_Up + Epsilon * (GX * Real_Step_Up) + Two_M75 * GX);
   end Lemma_Real_Stepped;

   procedure Lemma_Next_Is_Close (X : Dimensioned_Float) is
      Next : constant Dimensioned_Float := To_Real (To_Bits (X) + 1);
   begin
      Lemma_Next_Constants;
      Machine_Numbers.Lemma_Order (X, Next);
      pragma Assert (Next > X);
      Rounding.Lemma_Nonnegative (X);
      if X < Tiny then
         Lemma_Positive_Pattern (Tiny);
         Machine_Numbers.Lemma_Order (X, Tiny);
         Machine_Numbers.Lemma_Order (Next, Tiny);
         pragma Assert (Next <= Tiny);
         Rounding.Lemma_Order (Next, Tiny);
      else
         declare
            Above : constant Dimensioned_Float := X * Step_Up;
         begin
            Rounding.Lemma_Order (Tiny, X);
            Float_Lemmas.Lemma_Rounding_Error_Mul (X, Step_Up);
            Lemma_Real_Stepped (R (X), R (Above));
            Lemma_Strictly_Below (X, Above);
            Lemma_Positive_Pattern (Above);
            Machine_Numbers.Lemma_Order (X, Above);
            Machine_Numbers.Lemma_Order (Next, Above);
            pragma Assert (Next <= Above);
            Rounding.Lemma_Order (Next, Above);
         end;
      end if;
   end Lemma_Next_Is_Close;

end Prunt.Motion_Planner.Trajectory.Rounding.Relative;
