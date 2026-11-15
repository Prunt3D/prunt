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

with Prunt.Motion_Planner.Trajectory.Profile_Model;
with Prunt.Motion_Planner.Trajectory.Rounding;

package body Prunt.Motion_Planner.Trajectory.Solvers.Accuracy
  with SPARK_Mode => On
is

   ------------------------------------------------------------------------------------------------------------------
   --  How the proofs go
   --
   --  A lemma about a computed value calls the rounding lemma of its operation and then a lemma over reals only.
   --  A lemma about a region is over reals only: its parameters are the reals which the limits and the computed
   --  values denote, and each step is one call of a lemma of Trajectory.Rounding.Relative, with the relative
   --  bound which the step gives as a named number.
   ------------------------------------------------------------------------------------------------------------------

   Two_M162 : constant := 2.0**(-162);
   Two_M48  : constant := 2.0**(-48);
   Two_M50  : constant := 2.0**(-50);
   Two_M52  : constant := 2.0**(-52);
   Two_M163 : constant := 2.0**(-163);
   Two_M180 : constant := 2.0**(-180);
   Two_M322 : constant := 2.0**(-322);
   Two_M557 : constant := 2.0**(-557);
   Two_P162 : constant := 2.0**162;
   Two_P198 : constant := 2.0**198;
   Two_P396 : constant := 2.0**396;
   Two_M10  : constant := 2.0**(-10);
   Two_M170 : constant := 2.0**(-170);
   Two_M190 : constant := 2.0**(-190);
   Two_M340 : constant := 2.0**(-340);
   Two_P163 : constant := 2.0**163;
   Two_M197 : constant := 2.0**(-197);
   Two_M201 : constant := 2.0**(-201);
   Two_M202 : constant := 2.0**(-202);
   Two_M210 : constant := 2.0**(-210);
   Two_M394 : constant := 2.0**(-394);
   Two_M410 : constant := 2.0**(-410);
   Two_M478 : constant := 2.0**(-478);
   Two_M788 : constant := 2.0**(-788);
   Two_M800 : constant := 2.0**(-800);
   Two_M870 : constant := 2.0**(-870);
   Two_M872 : constant := 2.0**(-872);
   Two_M953 : constant := 2.0**(-953);
   Two_M954 : constant := 2.0**(-954);
   Two_M956 : constant := 2.0**(-956);
   Two_M990 : constant := 2.0**(-990);
   Two_M995 : constant := 2.0**(-995);
   Two_M80  : constant := 2.0**(-80);
   Two_P80  : constant := 2.0**80;
   Two_M160 : constant := 2.0**(-160);
   Two_P160 : constant := 2.0**160;
   Two_M400 : constant := 2.0**(-400);
   Two_P200 : constant := 2.0**200;
   Two_M600 : constant := 2.0**(-600);

   Sign_Top : constant Raw := 2#1.0#E600;

   procedure Lemma_Domain_Constants
   with
     Post =>
       R (Limit_Min) = Two_M80
       and then R (Limit_Max) = Two_P80
       and then R (Minuend_Min) = Two_M400
       and then R (Operand_Top) = Two_P200
       and then R (0.0) = 0.0
       and then R (Doubled_Top) = Two_P198
       and then R (0.25) = 0.25
       and then R (0.5) = 0.5
       and then R (1.5) = 1.5
       and then R (2.0) = 2.0
       and then R (8.0) = 8.0
   is
   begin
      null;
   end Lemma_Domain_Constants;

   procedure Lemma_Quotient_Times_Divisor (X, Y : Real) with Pre => Y > 0.0, Post => (X / Y) * Y = X is
   begin
      null;
   end Lemma_Quotient_Times_Divisor;

   procedure Lemma_Strict_Scale (A, B, C : Real) with Pre => A < B and then C > 0.0, Post => A * C < B * C is
   begin
      null;
   end Lemma_Strict_Scale;

   procedure Lemma_Floor (X, Square, Floor, Floor_Squared : Real)
   with
     Pre  =>
       X >= 0.0
       and then Floor >= 0.0
       and then Floor * Floor = Floor_Squared
       and then Square > Floor_Squared
       and then X * X >= Square,
     Post => X >= Floor
   is
   begin
      if X < Floor then
         Lemma_Product_Monotone (X, Floor, X, Floor);
      end if;
   end Lemma_Floor;
   --  A value whose square exceeds the square of Floor is at least Floor.

   procedure Lemma_Cube_Floor (X, Cube, Floor, Floor_Cubed : Real)
   with
     Pre  =>
       X >= 0.0
       and then Floor >= 0.0
       and then Floor * Floor * Floor = Floor_Cubed
       and then Cube > Floor_Cubed
       and then X * X * X >= Cube,
     Post => X >= Floor
   is
   begin
      if X < Floor then
         Lemma_Product_Monotone (X, Floor, X, Floor);
         Lemma_Product_Monotone (X * X, Floor * Floor, X, Floor);
      end if;
   end Lemma_Cube_Floor;

   procedure Lemma_Limit (X : Raw) with Pre => In_Domain (X), Post => R (X) >= Two_M80 and then R (X) <= Two_P80 is
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Limit_Min, X);
      Rounding.Lemma_Order (X, Limit_Max);
   end Lemma_Limit;

   procedure Lemma_Difference_Sign (X, Y : Raw)
   with
     Pre  => abs X <= Sign_Top and then abs Y <= Sign_Top,
     Post => (X - Y > 0.0) = (X > Y) and then (X - Y >= 0.0) = (X >= Y)
   is
   begin
      null;
   end Lemma_Difference_Sign;
   --  A floating-point difference has the sign of the exact one: it is zero only for equal operands.

   ------------------------------------------------------------------------------------------------------------------
   --  A quotient of two limits
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Quotient_Range (X1, X2, E : Real)
   with
     Pre  => X1 >= Two_M80 and then X1 <= Two_P80 and then X2 >= Two_M80 and then X2 <= Two_P80 and then E * X2 = X1,
     Post => E >= Two_M160 and then E <= Two_P160
   is
   begin
      pragma Assert (Two_M160 * X2 <= Two_M160 * Two_P80);
      Lemma_Product_Cancel (Two_M160, E, X2);
      pragma Assert (Two_P160 * X2 >= Two_P160 * Two_M80);
      Lemma_Product_Cancel (E, Two_P160, X2);
   end Lemma_Real_Quotient_Range;

   procedure Lemma_Quotient (Q, N, D : Raw) is
      X1 : constant Real := R (N);
      X2 : constant Real := R (D);
   begin
      Lemma_Limit (N);
      Lemma_Limit (D);
      declare
         E : constant Real := X1 / X2;
      begin
         pragma Assert (E * X2 = X1);
         Lemma_Real_Quotient_Range (X1, X2, E);
         Lemma_Near_Exact (N);
         Lemma_Near_Exact (D);
         Lemma_Near_Quotient (X1, X2, E, N, D, 0.0, 0.0, N_1);
         Lemma_About_From_Near (E, N / D, N_1);
         Lemma_About_Scaled (R (N / D), E, X2, N_1);
         Lemma_About_Bounds (R (N / D), E, N_1);
         Rounding.Lemma_Same (Q, N / D);
         pragma Assert (X2 * E = X1);
         pragma Assert (R (Q) >= Quotient_Floor and then R (Q) <= Quotient_Top);
      end;
   end Lemma_Quotient;

   ------------------------------------------------------------------------------------------------------------------
   --  What is left after one or two values are taken away
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Left (X, P, Q : Real)
   with
     Pre  => P >= Two_M400 and then Q >= 0.0 and then Q <= P and then abs (X - (P - Q)) <= Epsilon * abs (P - Q) + Eta,
     Post => abs (X - (P - Q)) <= N_4 * P
   is
   begin
      pragma Assert (Eta <= Two_M600 * P);
      pragma Assert (Epsilon * abs (P - Q) <= Epsilon * P);
   end Lemma_Real_Left;

   procedure Lemma_Remainder_1 (X, P, Q : Raw) is
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Minuend_Min, P);
      Rounding.Lemma_Nonnegative (Q);
      Lemma_Difference_Sign (P, Q);
      Float_Lemmas.Lemma_Rounding_Error_Sub (P, Q);
      Rounding.Lemma_Same (X, P - Q);
      if X >= 0.0 then
         Rounding.Lemma_Order (Q, P);
         Lemma_Real_Left (R (X), R (P), R (Q));
         Lemma_Scale_Monotone (0.0, N_4, R (P));
      end if;
      if X <= 0.0 then
         Rounding.Lemma_Order (P, Q);
         Lemma_Scale_Monotone (0.0, N_4, R (Q));
      end if;
   end Lemma_Remainder_1;

   procedure Lemma_Real_Left_2 (X, P, Q1, Q2, R1 : Real)
   with
     Pre  =>
       P >= Two_M400
       and then Q1 >= 0.0
       and then Q2 >= 0.0
       and then Q1 <= P
       and then Q2 <= R1
       and then abs (R1 - (P - Q1)) <= Epsilon * abs (P - Q1) + Eta
       and then abs (X - (R1 - Q2)) <= Epsilon * abs (R1 - Q2) + Eta,
     Post => abs (X - (P - (Q1 + Q2))) <= N_4 * P and then Q1 + Q2 <= P + N_4 * P
   is
   begin
      pragma Assert (Eta <= Two_M600 * P);
      pragma Assert (Epsilon * abs (P - Q1) <= Epsilon * P);
      pragma Assert (R1 <= P + Epsilon * P + Two_M600 * P);
      pragma Assert (abs (R1 - Q2) <= R1);
      pragma Assert (Epsilon * abs (R1 - Q2) <= Epsilon * (P + Epsilon * P + Two_M600 * P));
      pragma
        Assert
          (abs (X - (P - (Q1 + Q2)))
           <= Epsilon * P + Two_M600 * P + Epsilon * (P + Epsilon * P + Two_M600 * P) + Two_M600 * P);
   end Lemma_Real_Left_2;

   procedure Lemma_Real_Short_2 (P, Q1, Q2, R1 : Real)
   with
     Pre  =>
       P >= Two_M400
       and then Q1 >= 0.0
       and then Q2 >= 0.0
       and then Q1 <= P
       and then R1 <= Q2
       and then abs (R1 - (P - Q1)) <= Epsilon * abs (P - Q1) + Eta,
     Post => P <= (Q1 + Q2) + N_4 * (Q1 + Q2)
   is
      S : constant Real := Q1 + Q2;
      D : constant Real := P - Q1;
   begin
      pragma Assert (D - Epsilon * D <= Q2 + Eta);
      pragma Assert ((1.0 + N_1) * (D - Epsilon * D) >= D);
      pragma Assert (D <= (1.0 + N_1) * (Q2 + Eta));
      pragma Assert (P <= S + N_1 * S + 2.0 * Eta);
      pragma Assert (2.0 * Eta <= Two_M600 * P);
      pragma Assert (P - Two_M600 * P <= S + N_1 * S);
      pragma Assert (2.0 * Eta <= N_1 * S);
   end Lemma_Real_Short_2;

   procedure Lemma_Remainder_2 (X, P, Q1, Q2 : Raw) is
      E1 : constant Raw := P - Q1;
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Minuend_Min, P);
      Rounding.Lemma_Nonnegative (Q1);
      Rounding.Lemma_Nonnegative (Q2);
      Lemma_Difference_Sign (P, Q1);
      Lemma_Difference_Sign (E1, Q2);
      Float_Lemmas.Lemma_Rounding_Error_Sub (P, Q1);
      Float_Lemmas.Lemma_Rounding_Error_Sub (E1, Q2);
      Rounding.Lemma_Same (X, E1 - Q2);
      if X >= 0.0 then
         pragma Assert (E1 >= Q2);
         pragma Assert (P >= Q1);
         Rounding.Lemma_Order (Q2, E1);
         Rounding.Lemma_Order (Q1, P);
         Lemma_Real_Left_2 (R (X), R (P), R (Q1), R (Q2), R (E1));
      end if;
      if X <= 0.0 then
         if P <= Q1 then
            Rounding.Lemma_Order (P, Q1);
            Lemma_Scale_Monotone (0.0, N_4, R (Q1) + R (Q2));
         else
            pragma Assert (E1 <= Q2);
            Rounding.Lemma_Order (E1, Q2);
            Rounding.Lemma_Order (Q1, P);
            Lemma_Real_Short_2 (R (P), R (Q1), R (Q2), R (E1));
         end if;
      end if;
   end Lemma_Remainder_2;

   procedure Lemma_Quotient_Bounds (Q, N, D : Raw) is
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Minuend_Min, Q);
      Rounding.Lemma_Order (Q, Operand_Top);
   end Lemma_Quotient_Bounds;

   procedure Lemma_Remainder_Sign (X : Raw; P, Q, U : Real) is
   begin
      null;
   end Lemma_Remainder_Sign;

   procedure Lemma_Remainder_Rebase (X : Raw; P, Q, Q2, U, V, W : Real) is
   begin
      Lemma_Scale_Monotone (V, Relative_Max, Q2);
      Lemma_Scale_Monotone (0.0, V, Q2);
      if X >= 0.0 then
         --  Q2 is at most P, to within U + 2 * V and a second-order term.
         pragma Assert (Q2 - V * Q2 <= P + U * P);
         pragma Assert ((1.0 + 2.0 * V) * (Q2 - V * Q2) = Q2 + (V - 2.0 * (V * V)) * Q2);
         Lemma_Product_Monotone (V, Relative_Max, V, Relative_Max);
         Lemma_Scale_Monotone (V, Relative_Max, V);
         pragma Assert (V - 2.0 * (V * V) >= 0.0);
         Lemma_Scale_Monotone (0.0, V - 2.0 * (V * V), Q2);
         Lemma_Scale_Monotone (Q2 - V * Q2, P + U * P, 1.0 + 2.0 * V);
         pragma Assert (Q2 <= (1.0 + 2.0 * V) * (P + U * P));
         pragma Assert ((1.0 + 2.0 * V) * (P + U * P) = P + (U + 2.0 * V + 2.0 * (V * U)) * P);
         Lemma_Product_Monotone (0.0, V, 0.0, U);
         Lemma_Product_Monotone (V, Relative_Max, U, Relative_Max);
         Lemma_Scale_Monotone (U + 2.0 * V + 2.0 * (V * U), W, P);
         pragma Assert (Q2 <= P + W * P);
         --  The remainder moves by the difference of the two values.
         Lemma_Scale_Monotone (Q2, 2.0 * P, V);
         pragma Assert (V * Q2 <= (2.0 * V) * P);
         pragma Assert (abs (R (X) - (P - Q2)) <= U * P + (2.0 * V) * P);
         Lemma_Scale_Monotone (U + 2.0 * V, W, P);
      end if;
      if X <= 0.0 then
         pragma Assert (Q <= Q2 + V * Q2);
         Lemma_Scale_Monotone (Q, Q2 + V * Q2, 1.0 + U);
         pragma Assert (P <= (1.0 + U) * (Q2 + V * Q2));
         pragma Assert ((1.0 + U) * (Q2 + V * Q2) = Q2 + (U + V + U * V) * Q2);
         Lemma_Product_Monotone (0.0, U, 0.0, V);
         Lemma_Product_Monotone (U, Relative_Max, V, Relative_Max);
         Lemma_Scale_Monotone (U + V + U * V, W, Q2);
      end if;
   end Lemma_Remainder_Rebase;

   procedure Lemma_Remainder_Twice (X, P, T : Raw) is
      Twice : constant Raw := 2.0 * T;
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Minuend_Min, T);
      Rounding.Lemma_Order (T, Doubled_Top);
      Rounding.Lemma_Order (Minuend_Min, P);
      Lemma_Near_Exact (2.0);
      Lemma_Near_Exact (T);
      Lemma_Near_Product (2.0, R (T), 2.0, T, 0.0, 0.0, N_1);
      Lemma_About_From_Near (2.0 * R (T), Twice, N_1);
      Lemma_Near_Bounded (2.0 * R (T), Twice, Operand_Top, N_1);
      Lemma_Remainder_1 (X, P, Twice);
      Lemma_Remainder_Rebase (X, R (P), R (Twice), 2.0 * R (T), N_4, N_1, N_8);
   end Lemma_Remainder_Twice;

   ------------------------------------------------------------------------------------------------------------------
   --  The roots of the regions, and the speed above which snap is reached
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Powers (X : Raw) with Pre => In_Domain (X), Post => X**2 = X * X and then X**3 = X * X * X is
   begin
      null;
   end Lemma_Powers;
   --  The second and third powers are the products which they are computed as.

   procedure Lemma_Real_Snap_Root (S, A, C, Q1, Q2, Rr : Real)
   with
     Pre  =>
       S >= Two_M80
       and then S <= Two_P80
       and then A >= Two_M80
       and then C >= Two_M80
       and then C <= Two_P80
       and then Q1 * (C * C) = 0.25 * (S * S)
       and then Q1 >= 0.0
       and then Q2 * S = A
       and then Q2 >= Two_M160
       and then Rr >= 0.0
       and then About (Rr * Rr, Q1 + Q2, N_128),
     Post => About (Rr * Rr * (C * C * S), 0.25 * (S * S * S) + A * (C * C), N_128) and then Rr >= Root_Floor
   is
      K : constant Real := C * C * S;
      W : constant Real := Q1 + Q2;
   begin
      Lemma_Product_Monotone (0.0, C, 0.0, C);
      Lemma_Product_Monotone (0.0, C * C, 0.0, S);
      Lemma_About_Scaled (Rr * Rr, W, K, N_128);
      pragma Assert (K * Q1 = S * (Q1 * (C * C)));
      pragma Assert (K * Q2 = (C * C) * (Q2 * S));
      pragma Assert (K * W = 0.25 * (S * S * S) + A * (C * C));
      pragma Assert (K * (Rr * Rr) = Rr * Rr * (C * C * S));
      Lemma_About_Bounds (Rr * Rr, W, N_128);
      pragma Assert (Rr * Rr >= W - N_128 * W);
      pragma Assert (W - N_128 * W >= Two_M162);
      Lemma_Floor (Rr, Two_M162, Root_Floor, Two_M180);
   end Lemma_Real_Snap_Root;

   procedure Lemma_Snap_Root (SR, Am, Sm, Cm : Raw) is
      S : constant Real := R (Sm);
      C : constant Real := R (Cm);
      A : constant Real := R (Am);

      F_S2 : constant Raw := Sm**2;
      F_Q4 : constant Raw := 0.25 * F_S2;
      F_C2 : constant Raw := Cm**2;
      F_Q1 : constant Raw := F_Q4 / F_C2;
      F_Q2 : constant Raw := Am / Sm;
      F_W  : constant Raw := F_Q1 + F_Q2;
      --  The values which the argument of the root is computed through.
   begin
      Lemma_Domain_Constants;
      Lemma_Limit (Am);
      Lemma_Limit (Sm);
      Lemma_Limit (Cm);
      Lemma_Powers (Sm);
      Lemma_Powers (Cm);
      Lemma_Near_Exact (Am);
      Lemma_Near_Exact (Sm);
      Lemma_Near_Exact (Cm);
      Lemma_Near_Exact (0.25);

      Lemma_Product_Monotone (Two_M80, S, Two_M80, S);
      Lemma_Near_Product (S, S, Sm, Sm, 0.0, 0.0, N_1);
      Lemma_Near_Same (S * S, Sm * Sm, F_S2, N_1);
      Lemma_Near_Product (0.25, S * S, 0.25, F_S2, 0.0, N_1, N_2);
      pragma Assert (Near (0.25 * (S * S), F_Q4, N_2));
      Lemma_Product_Monotone (Two_M80, C, Two_M80, C);
      Lemma_Product_Monotone (C, Two_P80, C, Two_P80);
      Lemma_Near_Product (C, C, Cm, Cm, 0.0, 0.0, N_1);
      Lemma_Near_Same (C * C, Cm * Cm, F_C2, N_1);
      declare
         Q1 : constant Real := (0.25 * (S * S)) / (C * C);
         Q2 : constant Real := A / S;
      begin
         Lemma_Quotient_Times_Divisor (0.25 * (S * S), C * C);
         pragma Assert (Q1 * (C * C) = 0.25 * (S * S));
         Lemma_Quotient_Times_Divisor (A, S);
         pragma Assert (Q2 * S = A);
         pragma Assert (Two_M322 * (C * C) <= Q1 * (C * C));
         Lemma_Product_Cancel (Two_M322, Q1, C * C);
         Lemma_Near_Quotient (0.25 * (S * S), C * C, Q1, F_Q4, F_C2, N_2, N_1, N_4);
         Lemma_Real_Quotient_Range (A, S, Q2);
         Lemma_Near_Quotient (A, S, Q2, Am, Sm, 0.0, 0.0, N_1);
         Lemma_Near_Sum (Q1, Q2, F_Q1, F_Q2, N_4, N_1, N_6);
         Lemma_Root_Squared (Q1 + Q2, F_W, N_6, N_128);
         Rounding.Lemma_Same (SR, Dimensionless_Math.Sqrt (F_W));
         pragma Assert (About (R (SR) * R (SR), Q1 + Q2, N_128));
         Lemma_Real_Snap_Root (S, A, C, Q1, Q2, R (SR));
         Rounding.Lemma_Order (Minuend_Min, SR);
      end;
   end Lemma_Snap_Root;

   procedure Lemma_Scaled_Remainder (X, SR, Sm, Cm, SC : Raw) is
      S   : constant Real := R (Sm);
      C   : constant Real := R (Cm);
      Sig : constant Real := R (SC);

      F_H0 : constant Raw := 1.5 * Sm;
      F_H  : constant Raw := F_H0 / Cm;
   begin
      Lemma_Domain_Constants;
      Lemma_Limit (Sm);
      Lemma_Limit (Cm);
      Lemma_Near_Exact (1.5);
      Lemma_Near_Exact (Sm);
      Lemma_Near_Exact (Cm);
      Lemma_Near_Product (1.5, S, 1.5, Sm, 0.0, 0.0, N_1);
      declare
         Q : constant Real := (1.5 * S) / C;
      begin
         Lemma_Quotient_Times_Divisor (1.5 * S, C);
         pragma Assert (Q * C = 1.5 * S);
         pragma Assert (Two_M160 * C <= Q * C);
         Lemma_Product_Cancel (Two_M160, Q, C);
         Lemma_Near_Quotient (1.5 * S, C, Q, F_H0, Cm, N_1, 0.0, N_2);
         Lemma_About_From_Near (Q, F_H, N_2);

         --  Q is one and a half times the first phase time, to within the rounding of that time.
         Lemma_About_Scaled (C * Sig, S, 1.5, N_1);
         pragma Assert (1.5 * (C * Sig) = C * (1.5 * Sig));
         pragma Assert (1.5 * S = C * Q);
         Lemma_About_Cancel (1.5 * Sig, Q, C, N_1);
         Lemma_About_Symmetric (1.5 * Sig, Q, N_1, N_2);
         Lemma_About_Chain (R (F_H), Q, 1.5 * Sig, N_2, N_2, N_6);

         Rounding.Lemma_Order (Minuend_Min, SR);
         Lemma_Remainder_1 (X, SR, F_H);
         Lemma_Remainder_Rebase (X, R (SR), R (F_H), 1.5 * Sig, N_4, N_6, N_20);
      end;
   end Lemma_Scaled_Remainder;

   procedure Lemma_Real_Cube_Fact (A, C, Q, T : Real)
   with
     Pre  =>
       A >= Two_M80
       and then C >= Two_M80
       and then Q * C = 0.5 * A
       and then Q >= Two_M162
       and then T >= 0.0
       and then About (T * T * T, Q, Cube_Tolerance),
     Post => About (2.0 * C * (T * T * T), A, Cube_Tolerance) and then T >= Cube_Floor
   is
   begin
      Lemma_About_Scaled (T * T * T, Q, 2.0 * C, Cube_Tolerance);
      pragma Assert ((2.0 * C) * Q = A);
      pragma Assert ((2.0 * C) * (T * T * T) = 2.0 * C * (T * T * T));
      Lemma_About_Bounds (T * T * T, Q, Cube_Tolerance);
      pragma Assert (T * T * T >= Two_M163);
      Lemma_Cube_Floor (T, Two_M163, Cube_Floor, Two_M180);
   end Lemma_Real_Cube_Fact;

   procedure Lemma_Unit_Operations (X : Raw) with Post => X / 1.0 = X and then X * 1.0 = X is
   begin
      null;
   end Lemma_Unit_Operations;

   procedure Lemma_Crackle_Root (T, Am, Cm : Raw) is
      A : constant Real := R (Am);
      C : constant Real := R (Cm);

      F_H : constant Raw := 0.5 * Am;
      F_Q : constant Raw := F_H / Cm;
      F_X : constant Raw := F_Q / 1.0;
      F_T : constant Raw := Cube_Root (F_X);
   begin
      Lemma_Domain_Constants;
      Lemma_Limit (Am);
      Lemma_Limit (Cm);
      Lemma_Near_Exact (0.5);
      Lemma_Near_Exact (Am);
      Lemma_Near_Exact (Cm);
      Lemma_Near_Product (0.5, A, 0.5, Am, 0.0, 0.0, N_1);
      declare
         Q : constant Real := (0.5 * A) / C;
      begin
         Lemma_Quotient_Times_Divisor (0.5 * A, C);
         pragma Assert (Q * C = 0.5 * A);
         pragma Assert (Two_M162 * C <= Q * C);
         Lemma_Product_Cancel (Two_M162, Q, C);
         Lemma_Near_Quotient (0.5 * A, C, Q, F_H, Cm, N_1, 0.0, N_2);
         Lemma_Unit_Operations (F_Q);
         Lemma_Near_Same (Q, F_Q, F_X, N_2);
         Lemma_Root_Cubed (Q, F_X, N_2, Cube_Tolerance);
         Lemma_Unit_Operations (F_T);
         Rounding.Lemma_Same (T, F_T);
         Lemma_Real_Cube_Fact (A, C, Q, R (T));
      end;
   end Lemma_Crackle_Root;

   procedure Lemma_Jerk_Root (T, JC, Jm, Cm : Raw) is
      G  : constant Real := R (JC);
      Rt : constant Real := R (Dimensionless_Math.Sqrt (JC));
   begin
      Lemma_Domain_Constants;
      Lemma_Quotient_Bounds (JC, Jm, Cm);
      Lemma_Near_Exact (JC);
      Lemma_Root_Squared (G, JC, 0.0, N_64);
      Rounding.Lemma_Same (T, Dimensionless_Math.Sqrt (JC));
      Lemma_About_Bounds (Rt * Rt, G, N_64);
      pragma Assert (Rt * Rt >= Two_M162);
      Lemma_Floor (Rt, Two_M162, Root_Floor, Two_M180);
      pragma Assert (Rt * Rt <= Two_P162);
      if Rt > Two_P198 then
         Lemma_Product_Monotone (Two_P198, Rt, Two_P198, Rt);
         pragma Assert (Two_P198 * Two_P198 = Two_P396);
         pragma Assert (False);
      end if;
      Rounding.Lemma_Order (Minuend_Min, T);
      Rounding.Lemma_Order (T, Doubled_Top);
   end Lemma_Jerk_Root;

   procedure Lemma_Threshold_Identity (C, Sig : Real)
   with
     Post =>
       8.0 * (((C * Sig) * (C * Sig)) * ((C * Sig) * (C * Sig))) = ((C * C) * C) * (8.0 * (C * Sig) * Sig * Sig * Sig)
   is
   begin
      null;
   end Lemma_Threshold_Identity;

   procedure Lemma_Real_Threshold (S, C, Sig, E, G : Real)
   with
     Pre  =>
       S >= 0.0
       and then C >= Two_M80
       and then Sig >= 0.0
       and then About (C * Sig, S, N_1)
       and then E >= 0.0
       and then E * ((C * C) * C) = 8.0 * ((S * S) * (S * S))
       and then About (G, E, N_8),
     Post => About (G, 8.0 * (C * Sig) * Sig * Sig * Sig, N_32)
   is
      P : constant Real := C * Sig;
      Y : constant Real := 8.0 * (C * Sig) * Sig * Sig * Sig;
      K : constant Real := (C * C) * C;
   begin
      Lemma_Product_Monotone (0.0, C, 0.0, C);
      Lemma_Product_Monotone (Two_M160, C * C, Two_M80, C);
      pragma Assert (K > 0.0);
      Lemma_About_Product (P, S, P, S, N_1, N_1, N_3);
      Lemma_About_Product (P * P, S * S, P * P, S * S, N_3, N_3, N_8);
      Lemma_About_Scaled ((P * P) * (P * P), (S * S) * (S * S), 8.0, N_8);
      Lemma_Threshold_Identity (C, Sig);
      pragma Assert (8.0 * ((P * P) * (P * P)) = K * Y);
      pragma Assert (8.0 * ((S * S) * (S * S)) = K * E);
      Lemma_About_Cancel (Y, E, K, N_8);
      Lemma_About_Symmetric (Y, E, N_8, N_10);
      Lemma_About_Chain (G, E, Y, N_8, N_10, N_20);
      Lemma_About_Weaken (G, Y, N_20, N_32);
   end Lemma_Real_Threshold;

   procedure Lemma_Threshold (SOA, Sm, Cm, SC : Raw) is
      S : constant Real := R (Sm);
      C : constant Real := R (Cm);

      pragma Assert (About (C * R (SC), S, N_1));
      --  From the precondition, before the context fills with the values below.

      F_S2 : constant Raw := Sm**2;
      F_S4 : constant Raw := F_S2**2;
      F_E  : constant Raw := 8.0 * F_S4;
      F_C2 : constant Raw := Cm * Cm;
      F_C3 : constant Raw := Cm**3;
   begin
      Lemma_Domain_Constants;
      Lemma_Limit (Sm);
      Lemma_Limit (Cm);
      Lemma_Powers (Sm);
      Lemma_Powers (Cm);
      pragma Assert (F_S4 = F_S2 * F_S2);
      Lemma_Near_Exact (Sm);
      Lemma_Near_Exact (Cm);
      Lemma_Near_Exact (8.0);

      Lemma_Product_Monotone (Two_M80, S, Two_M80, S);
      Lemma_Near_Product (S, S, Sm, Sm, 0.0, 0.0, N_1);
      Lemma_Near_Same (S * S, Sm * Sm, F_S2, N_1);
      Lemma_Product_Monotone (Two_M160, S * S, Two_M160, S * S);
      Lemma_Near_Product (S * S, S * S, F_S2, F_S2, N_1, N_1, N_3);
      Lemma_Near_Same ((S * S) * (S * S), F_S2 * F_S2, F_S4, N_3);
      Lemma_Near_Product (8.0, (S * S) * (S * S), 8.0, F_S4, 0.0, N_3, N_4);

      Lemma_Product_Monotone (Two_M80, C, Two_M80, C);
      Lemma_Product_Monotone (C, Two_P80, C, Two_P80);
      Lemma_Near_Product (C, C, Cm, Cm, 0.0, 0.0, N_1);
      Lemma_Near_Bounded (C * C, F_C2, Operand_Top, N_1);
      Lemma_Product_Monotone (Two_M160, C * C, Two_M80, C);
      Lemma_Product_Monotone (C * C, Two_P160, C, Two_P80);
      Lemma_Near_Product (C * C, C, F_C2, Cm, N_1, 0.0, N_2);
      Lemma_Near_Same ((C * C) * C, F_C2 * Cm, F_C3, N_2);
      declare
         E : constant Real := (8.0 * ((S * S) * (S * S))) / ((C * C) * C);
      begin
         Lemma_Quotient_Times_Divisor (8.0 * ((S * S) * (S * S)), (C * C) * C);
         pragma Assert (E * ((C * C) * C) = 8.0 * ((S * S) * (S * S)));
         pragma Assert (Two_M557 * ((C * C) * C) <= E * ((C * C) * C));
         Lemma_Product_Cancel (Two_M557, E, (C * C) * C);
         Lemma_Near_Quotient (8.0 * ((S * S) * (S * S)), (C * C) * C, E, F_E, F_C3, N_4, N_2, N_8);
         Rounding.Lemma_Same (SOA, F_E / F_C3);
         Lemma_About_From_Near (E, F_E / F_C3, N_8);
         pragma Assert (R (SOA) = R (F_E / F_C3));
         pragma Assert (About (R (SOA), E, N_8));
         Rounding.Lemma_Nonnegative (SC);
         Lemma_Real_Threshold (S, C, R (SC), E, R (SOA));
      end;
   end Lemma_Threshold;

   ------------------------------------------------------------------------------------------------------------------
   --  Region 1: snap, jerk and acceleration are all reached
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Region_1 (S, J, A, C, Sig, Kap, Alp, T2, T3 : Real)
   with
     Pre  =>
       Sig >= 0.0
       and then T2 >= 0.0
       and then T3 >= 0.0
       and then About (C * Sig, S, N_1)
       and then About (S * Kap, J, N_1)
       and then About (J * Alp, A, N_1)
       and then Kap >= 0.0
       and then Alp >= 0.0
       and then abs (T2 - (Kap - Sig)) <= N_4 * Kap
       and then abs (T3 - (Alp - (Kap + Sig))) <= N_4 * Alp
       and then Kap + Sig <= Alp + N_4 * Alp,
     Post =>
       About (Sig + T2, Kap, N_4)
       and then About (C * Sig * (Sig + T2), J, N_8)
       and then About (2.0 * Sig + T2 + T3, Alp, N_10)
       and then About (C * Sig * (Sig + T2) * (2.0 * Sig + T2 + T3), A, N_24)
   is
      B  : constant Real := Sig + T2;
      Cc : constant Real := 2.0 * Sig + T2 + T3;
   begin
      pragma Assert (About (B, Kap, N_4));
      Lemma_About_Product (C * Sig, S, B, Kap, N_1, N_4, N_6);
      Lemma_About_Chain (C * Sig * B, S * Kap, J, N_6, N_1, N_8);

      pragma Assert (Cc - Alp = (T2 - (Kap - Sig)) + (T3 - (Alp - (Kap + Sig))));
      pragma Assert (Kap <= Alp + N_4 * Alp);
      Lemma_Scale_Monotone (Kap, Alp + N_4 * Alp, N_4);
      pragma Assert (abs (Cc - Alp) <= N_4 * (Alp + N_4 * Alp) + N_4 * Alp);
      pragma Assert (N_4 * (Alp + N_4 * Alp) + N_4 * Alp <= N_10 * Alp);
      pragma Assert (About (Cc, Alp, N_10));

      Lemma_About_Product (C * Sig * B, J, Cc, Alp, N_8, N_10, N_20);
      Lemma_About_Chain (C * Sig * B * Cc, J * Alp, A, N_20, N_1, N_24);
   end Lemma_Real_Region_1;

   procedure Lemma_Region_1 (Am, Jm, Sm, Cm, JS, SC, AJ, T2, T3 : Raw) is
      S   : constant Real := R (Sm);
      J   : constant Real := R (Jm);
      A   : constant Real := R (Am);
      C   : constant Real := R (Cm);
      Sig : constant Real := R (SC);
      Kap : constant Real := R (JS);
      Alp : constant Real := R (AJ);
   begin
      Rounding.Lemma_Nonnegative (T2);
      Rounding.Lemma_Nonnegative (T3);
      Lemma_Real_Region_1 (S, J, A, C, Sig, Kap, Alp, R (T2), R (T3));
      Lemma_About_Weaken (C * Sig, S, N_1, N_32);
      Lemma_About_Weaken (Sig + R (T2), Kap, N_4, N_32);
      Lemma_About_Weaken (C * Sig * (Sig + R (T2)), J, N_8, N_32);
      Lemma_About_Weaken (2.0 * Sig + R (T2) + R (T3), Alp, N_10, N_32);
      Lemma_About_Weaken (C * Sig * (Sig + R (T2)) * (2.0 * Sig + R (T2) + R (T3)), A, N_24, N_32);
   end Lemma_Region_1;

   ------------------------------------------------------------------------------------------------------------------
   --  Real arithmetic which several regions share
   ------------------------------------------------------------------------------------------------------------------

   Cubic_Inner : constant := 2.0**(-34) + N_2048;
   --  The relative bound of a first phase time which is a cube root, before it is compared with a limit.

   Twice_Inner      : constant := 2.0**(-33) + N_8192;
   Twice_Inner_More : constant := Twice_Inner + N_128;
   --  The relative bounds of its square.

   procedure Lemma_Solve_Product_Sum (X, Y, G, U : Real)
   with
     Pre  =>
       X >= 0.0 and then Y >= 0.0 and then G >= 0.0 and then U >= 0.0 and then At_Most (X * (G + X), Y * (G + Y), U),
     Post => At_Most (X, Y, U)
   is
      Z : constant Real := Y + U * Y;
   begin
      Lemma_Scale_Monotone (0.0, U, Y);
      if X > Z then
         Lemma_Strict_Scale (Z, X, G + X);
         Lemma_Scale_Monotone (G + Y, G + X, Z);
         pragma Assert (Z * (G + Y) = Y * (G + Y) + U * (Y * (G + Y)));
         pragma Assert (False);
      end if;
   end Lemma_Solve_Product_Sum;
   --  The product of a value and its sum with G grows with the value, so an order of two such products, to
   --  within a fraction, is an order of the values to within it.

   procedure Lemma_Real_Jerk_Below (S, J, C, Sig, Kap : Real)
   with
     Pre  =>
       S >= 0.0
       and then C >= 0.0
       and then Sig >= 0.0
       and then Kap >= 0.0
       and then About (C * Sig, S, N_1)
       and then About (S * Kap, J, N_1)
       and then At_Most (Kap, Sig, N_4),
     Post => At_Most (C * J, S * S, N_16)
   is
   begin
      Lemma_About_Symmetric (S * Kap, J, N_1, N_2);
      Lemma_At_Most_From_About (J, S * Kap, N_2);
      Lemma_At_Most_Scaled (Kap, Sig, S, N_4);
      Lemma_Product_Monotone (0.0, S, 0.0, Sig);
      Lemma_At_Most_Chain (J, S * Kap, S * Sig, N_2, N_4, N_8);
      Lemma_At_Most_Scaled (J, S * Sig, C, N_8);
      pragma Assert (C * (S * Sig) = S * (C * Sig));
      Lemma_At_Most_From_About (C * Sig, S, N_1);
      Lemma_At_Most_Scaled (C * Sig, S, S, N_1);
      Lemma_Product_Monotone (0.0, S, 0.0, S);
      Lemma_At_Most_Chain (C * J, S * (C * Sig), S * S, N_8, N_1, N_16);
   end Lemma_Real_Jerk_Below;
   --  Where the quotient of jerk and snap does not exceed that of snap and crackle, the product of the crackle
   --  and jerk limits does not exceed the square of the snap limit.

   procedure Lemma_Real_Jerk_Above (S, J, C, Sig, Kap : Real)
   with
     Pre  =>
       S >= 0.0
       and then C >= 0.0
       and then Sig >= 0.0
       and then Kap >= 0.0
       and then J >= 0.0
       and then About (C * Sig, S, N_1)
       and then About (S * Kap, J, N_1)
       and then At_Most (Sig, Kap, N_4),
     Post => At_Most (S * S, C * J, N_16)
   is
   begin
      Lemma_Product_Monotone (0.0, C, 0.0, Sig);
      Lemma_About_Symmetric (C * Sig, S, N_1, N_2);
      Lemma_At_Most_From_About (S, C * Sig, N_2);
      Lemma_At_Most_Scaled (Sig, Kap, C, N_4);
      Lemma_Product_Monotone (0.0, C, 0.0, Kap);
      Lemma_At_Most_Chain (S, C * Sig, C * Kap, N_2, N_4, N_8);
      Lemma_At_Most_Scaled (S, C * Kap, S, N_8);
      pragma Assert (S * (C * Kap) = C * (S * Kap));
      Lemma_At_Most_From_About (S * Kap, J, N_1);
      Lemma_At_Most_Scaled (S * Kap, J, C, N_1);
      Lemma_Product_Monotone (0.0, C, 0.0, J);
      Lemma_At_Most_Chain (S * S, C * (S * Kap), C * J, N_8, N_1, N_16);
   end Lemma_Real_Jerk_Above;

   ------------------------------------------------------------------------------------------------------------------
   --  Region 2: snap and acceleration are reached, jerk is not
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Region_2_Spans (Sig, Rr, T2 : Real)
   with
     Pre  =>
       Sig >= 0.0
       and then Rr >= 0.0
       and then T2 >= 0.0
       and then abs (T2 - (Rr - 1.5 * Sig)) <= N_20 * Rr
       and then 1.5 * Sig <= Rr + N_20 * Rr,
     Post =>
       Rr - 0.5 * Sig >= 0.0
       and then About (Sig + T2, Rr - 0.5 * Sig, N_64)
       and then About (2.0 * Sig + T2, Rr + 0.5 * Sig, N_20)
       and then About (2.0 * (Sig + (Sig + T2)), Sig + 2.0 * Rr, N_20)
   is
      B  : constant Real := Sig + T2;
      Bx : constant Real := Rr - 0.5 * Sig;
      Cx : constant Real := Rr + 0.5 * Sig;
   begin
      pragma Assert (0.5 * Sig <= 0.34 * Rr);
      pragma Assert (Bx >= 0.66 * Rr);
      pragma Assert (B - Bx = T2 - (Rr - 1.5 * Sig));
      pragma Assert (N_20 * Rr <= N_64 * Bx);
      pragma Assert (About (B, Bx, N_64));
      pragma Assert ((2.0 * Sig + T2) - Cx = T2 - (Rr - 1.5 * Sig));
      pragma Assert (N_20 * Rr <= N_20 * Cx);
      pragma Assert (2.0 * (Sig + B) - (Sig + 2.0 * Rr) = 2.0 * (T2 - (Rr - 1.5 * Sig)));
      pragma Assert (2.0 * (N_20 * Rr) <= N_20 * (Sig + 2.0 * Rr));
   end Lemma_Real_Region_2_Spans;
   --  The first two spans of Region_2 are the root less and plus half of the first phase time.

   procedure Lemma_Real_Region_2_Condition (S, A, C, Sig, Rr : Real)
   with
     Pre  =>
       S >= 0.0
       and then A >= 0.0
       and then C >= 0.0
       and then Sig >= 0.0
       and then Rr >= 0.0
       and then About (C * Sig, S, N_1)
       and then About (Rr * Rr * (C * C * S), 0.25 * (S * S * S) + A * (C * C), N_128)
       and then 1.5 * Sig <= Rr + N_20 * Rr,
     Post => S * S * S <= A * (C * C)
   is
      K  : constant Real := C * C * S;
      X1 : constant Real := (C * Sig) * (C * Sig);
      M  : constant Real := 0.25 * (S * S * S) + A * (C * C);
      S3 : constant Real := S * S * S;
      AC : constant Real := A * (C * C);
   begin
      Lemma_Product_Monotone (0.0, C, 0.0, C);
      Lemma_Product_Monotone (0.0, C * C, 0.0, S);
      Lemma_Product_Monotone (0.0, S, 0.0, S);
      Lemma_Product_Monotone (0.0, S * S, 0.0, S);
      Lemma_Product_Monotone (0.0, A, 0.0, C * C);
      Lemma_About_Product (C * Sig, S, C * Sig, S, N_1, N_1, N_3);
      Lemma_About_Bounds (X1, S * S, N_3);
      pragma Assert (X1 >= S * S - N_3 * (S * S));
      Lemma_Scale_Monotone (S * S - N_3 * (S * S), X1, S);
      pragma Assert (X1 * S >= S3 - N_3 * S3);
      pragma Assert (Sig * Sig * K = X1 * S);
      Lemma_Product_Monotone (1.5 * Sig, Rr + N_20 * Rr, 1.5 * Sig, Rr + N_20 * Rr);
      pragma Assert ((1.5 * Sig) * (1.5 * Sig) = 2.25 * (Sig * Sig));
      pragma Assert ((Rr + N_20 * Rr) * (Rr + N_20 * Rr) = (1.0 + N_20) * (1.0 + N_20) * (Rr * Rr));
      Lemma_Product_Monotone (0.0, Rr, 0.0, Rr);
      pragma Assert ((1.0 + N_20) * (1.0 + N_20) * (Rr * Rr) <= (1.0 + N_48) * (Rr * Rr));
      Lemma_Scale_Monotone (2.25 * (Sig * Sig), (1.0 + N_48) * (Rr * Rr), K);
      pragma Assert (2.25 * (X1 * S) <= (1.0 + N_48) * (Rr * Rr * K));
      Lemma_About_Bounds (Rr * Rr * K, M, N_128);
      pragma Assert (Rr * Rr * K <= M + N_128 * M);
      pragma Assert (M >= 0.0);
      pragma Assert (2.25 * (S3 - N_3 * S3) <= (1.0 + N_48) * (M + N_128 * M));
      pragma Assert ((1.0 + N_48) * (M + N_128 * M) <= 1.01 * M);
      pragma Assert (2.2 * S3 <= 1.01 * (0.25 * S3 + AC));
   end Lemma_Real_Region_2_Condition;
   --  In Region_2 the cube of the snap limit is no more than the acceleration limit times the square of the
   --  crackle limit: the root is at least one and a half times the first phase time.

   procedure Lemma_Real_Region_2_Acceleration (S, A, C, Sig, Rr, B, Cc : Real)
   with
     Pre  =>
       S > 0.0
       and then A >= 0.0
       and then C > 0.0
       and then Sig >= 0.0
       and then Rr >= 0.0
       and then About (C * Sig, S, N_1)
       and then About (Rr * Rr * (C * C * S), 0.25 * (S * S * S) + A * (C * C), N_128)
       and then S * S * S <= A * (C * C)
       and then Rr - 0.5 * Sig >= 0.0
       and then About (B, Rr - 0.5 * Sig, N_64)
       and then About (Cc, Rr + 0.5 * Sig, N_20),
     Post => About (S * (B * Cc), A, N_512) and then About (C * Sig * B * Cc, A, N_1024)
   is
      K  : constant Real := C * C * S;
      X1 : constant Real := (C * Sig) * (C * Sig);
      M  : constant Real := 0.25 * (S * S * S) + A * (C * C);
      S3 : constant Real := S * S * S;
      AC : constant Real := A * (C * C);
      Bx : constant Real := Rr - 0.5 * Sig;
      Cx : constant Real := Rr + 0.5 * Sig;
   begin
      Lemma_Product_Monotone (0.0, C, 0.0, C);
      pragma Assert (C * C > 0.0);
      Lemma_Product_Monotone (0.0, C * C, 0.0, S);
      Lemma_Product_Monotone (0.0, S, 0.0, S);
      Lemma_Product_Monotone (0.0, S * S, 0.0, S);
      Lemma_Product_Monotone (0.0, A, 0.0, C * C);
      Lemma_About_Product (B, Bx, Cc, Cx, N_64, N_20, N_128);
      pragma Assert (Bx * Cx = Rr * Rr - 0.25 * (Sig * Sig));

      --  The exact product of the two spans, times Cm**2 * Sm, is Am * Cm**2 to within the bound of the root.
      Lemma_About_Product (C * Sig, S, C * Sig, S, N_1, N_1, N_3);
      Lemma_About_Scaled (X1, S * S, S, N_3);
      pragma Assert (abs (S * X1 - S3) <= N_3 * S3);
      pragma Assert (Sig * Sig * K = S * X1);
      pragma Assert (Bx * Cx * K = Rr * Rr * K - 0.25 * (S * X1));
      pragma Assert (abs (Rr * Rr * K - M) <= N_128 * M);
      pragma Assert (M <= 1.25 * AC);
      pragma Assert (abs (Bx * Cx * K - AC) <= N_128 * (1.25 * AC) + 0.25 * (N_3 * AC));
      pragma Assert (About (Bx * Cx * K, AC, N_256));

      Lemma_About_Scaled (B * Cc, Bx * Cx, K, N_128);
      pragma Assert (K * (Bx * Cx) = Bx * Cx * K);
      Lemma_About_Chain (K * (B * Cc), Bx * Cx * K, AC, N_128, N_256, N_512);
      pragma Assert (K * (B * Cc) = (C * C) * (S * (B * Cc)));
      pragma Assert (AC = (C * C) * A);
      Lemma_About_Cancel (S * (B * Cc), A, C * C, N_512);

      Lemma_About_Bounds (B, Bx, N_64);
      Lemma_About_Bounds (Cc, Cx, N_20);
      Lemma_Product_Monotone (0.0, B, 0.0, Cc);
      Lemma_About_Scaled (C * Sig, S, B * Cc, N_1);
      Lemma_About_Chain ((B * Cc) * (C * Sig), (B * Cc) * S, A, N_1, N_512, N_1024);
      pragma Assert ((B * Cc) * S = S * (B * Cc));
      pragma Assert ((B * Cc) * (C * Sig) = C * Sig * B * Cc);
   end Lemma_Real_Region_2_Acceleration;

   procedure Lemma_Real_Region_2_Jerk (S, J, A, C, Sig, Kap, Alp, B : Real)
   with
     Pre  =>
       S > 0.0
       and then J >= 0.0
       and then A >= 0.0
       and then C >= 0.0
       and then Sig >= 0.0
       and then Kap >= 0.0
       and then Alp >= 0.0
       and then B >= 0.0
       and then About (C * Sig, S, N_1)
       and then About (S * Kap, J, N_1)
       and then About (J * Alp, A, N_1)
       and then At_Most (Alp, Kap + Sig, N_4)
       and then About (S * (B * (Sig + B)), A, N_512),
     Post => At_Most (B, Kap, N_1024) and then At_Most (C * Sig * B, J, N_4096)
   is
   begin
      Lemma_Product_Monotone (0.0, S, 0.0, Kap);
      Lemma_Product_Monotone (0.0, J, 0.0, Alp);
      Lemma_About_Symmetric (J * Alp, A, N_1, N_2);
      Lemma_At_Most_From_About (A, J * Alp, N_2);
      Lemma_About_Symmetric (S * Kap, J, N_1, N_2);
      Lemma_At_Most_From_About (J, S * Kap, N_2);
      Lemma_At_Most_Product (J, S * Kap, Alp, Kap + Sig, N_2, N_4, N_8);
      Lemma_Product_Monotone (0.0, S * Kap, 0.0, Kap + Sig);
      Lemma_At_Most_Chain (A, J * Alp, (S * Kap) * (Kap + Sig), N_2, N_8, N_16);
      Lemma_At_Most_From_About (S * (B * (Sig + B)), A, N_512);
      Lemma_At_Most_Chain (S * (B * (Sig + B)), A, (S * Kap) * (Kap + Sig), N_512, N_16, N_1024);
      pragma Assert ((S * Kap) * (Kap + Sig) = S * (Kap * (Sig + Kap)));
      Lemma_Product_Monotone (0.0, Kap, 0.0, Sig + Kap);
      Lemma_At_Most_Cancel (B * (Sig + B), Kap * (Sig + Kap), S, N_1024);
      Lemma_Solve_Product_Sum (B, Kap, Sig, N_1024);

      Lemma_Product_Monotone (0.0, C, 0.0, Sig);
      Lemma_At_Most_From_About (C * Sig, S, N_1);
      Lemma_At_Most_Product (C * Sig, S, B, Kap, N_1, N_1024, N_2048);
      Lemma_At_Most_From_About (S * Kap, J, N_1);
      Lemma_At_Most_Chain (C * Sig * B, S * Kap, J, N_2048, N_1, N_4096);
   end Lemma_Real_Region_2_Jerk;

   procedure Lemma_Region_2 (Am, Jm, Sm, Cm, JS, SC, AJ, SR, T2 : Raw) is
      S   : constant Real := R (Sm);
      J   : constant Real := R (Jm);
      A   : constant Real := R (Am);
      C   : constant Real := R (Cm);
      Sig : constant Real := R (SC);
      Kap : constant Real := R (JS);
      Alp : constant Real := R (AJ);
      Rr  : constant Real := R (SR);
      B   : constant Real := Sig + R (T2);
      Cc  : constant Real := 2.0 * Sig + R (T2);
   begin
      Lemma_Limit (Am);
      Lemma_Limit (Jm);
      Lemma_Limit (Sm);
      Lemma_Limit (Cm);
      Rounding.Lemma_Nonnegative (T2);
      Lemma_Real_Region_2_Spans (Sig, Rr, R (T2));
      Lemma_Real_Region_2_Condition (S, A, C, Sig, Rr);
      Lemma_Real_Region_2_Acceleration (S, A, C, Sig, Rr, B, Cc);
      pragma Assert (Cc = Sig + B);
      Lemma_Real_Region_2_Jerk (S, J, A, C, Sig, Kap, Alp, B);
      Lemma_About_Weaken (C * Sig, S, N_1, N_4096);
      Lemma_About_Weaken (C * Sig * B * Cc, A, N_1024, N_4096);
      Lemma_About_Weaken (2.0 * (Sig + B), Sig + 2.0 * Rr, N_20, N_4096);
   end Lemma_Region_2;

   ------------------------------------------------------------------------------------------------------------------
   --  Regions 3 and 5: acceleration is reached with a single phase time
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Cubic (S, J, A, C, T, Bound, U : Real)
   with
     Pre  =>
       S >= 0.0
       and then J >= 0.0
       and then A >= 0.0
       and then C > 0.0
       and then T >= 0.0
       and then Bound >= 0.0
       and then About (2.0 * C * (T * T * T), A, Cube_Tolerance)
       and then At_Most (A, 2.0 * C * (Bound * Bound * Bound), U)
       and then U >= 0.0
       and then U <= N_1024,
     Post => At_Most (T, Bound, Cubic_Inner)
   is
   begin
      Lemma_Product_Monotone (0.0, Bound, 0.0, Bound);
      Lemma_Product_Monotone (0.0, Bound * Bound, 0.0, Bound);
      Lemma_Product_Monotone (0.0, 2.0 * C, 0.0, Bound * Bound * Bound);
      Lemma_At_Most_From_About (2.0 * C * (T * T * T), A, Cube_Tolerance);
      Lemma_At_Most_Chain
        (2.0 * C * (T * T * T), A, 2.0 * C * (Bound * Bound * Bound), Cube_Tolerance, U, Cubic_Inner);
      pragma Assert (2.0 * C * (T * T * T) = (2.0 * C) * (T * T * T));
      pragma Assert (2.0 * C * (Bound * Bound * Bound) = (2.0 * C) * (Bound * Bound * Bound));
      Lemma_At_Most_Cancel (T * T * T, Bound * Bound * Bound, 2.0 * C, Cubic_Inner);
      Lemma_At_Most_Cube_Root (T, Bound, Cubic_Inner);
   end Lemma_Real_Cubic;
   --  A first phase time whose cube gives the acceleration limit is no more than a time whose cube gives at
   --  least that limit.

   procedure Lemma_Real_Region_3 (S, J, A, C, Sig, Kap, Rr, T : Real)
   with
     Pre  =>
       S > 0.0
       and then J >= 0.0
       and then A >= 0.0
       and then C > 0.0
       and then Sig >= 0.0
       and then Kap >= 0.0
       and then Rr >= 0.0
       and then T >= 0.0
       and then About (C * Sig, S, N_1)
       and then About (S * Kap, J, N_1)
       and then At_Most (Sig, Kap, N_4)
       and then About (Rr * Rr * (C * C * S), 0.25 * (S * S * S) + A * (C * C), N_128)
       and then At_Most (Rr, 1.5 * Sig, N_20)
       and then About (2.0 * C * (T * T * T), A, Cube_Tolerance),
     Post => At_Most (C * T, S, Cubic_Tolerance) and then At_Most (C * T * T, J, Twice_Cubic_Tolerance)
   is
      K  : constant Real := C * C * S;
      X1 : constant Real := (C * Sig) * (C * Sig);
      M  : constant Real := 0.25 * (S * S * S) + A * (C * C);
      S3 : constant Real := S * S * S;
      AC : constant Real := A * (C * C);
      CT : constant Real := C * T;
   begin
      Lemma_Product_Monotone (0.0, C, 0.0, C);
      Lemma_Product_Monotone (0.0, C * C, 0.0, S);
      Lemma_Product_Monotone (0.0, S, 0.0, S);
      Lemma_Product_Monotone (0.0, S * S, 0.0, S);
      Lemma_Product_Monotone (0.0, A, 0.0, C * C);
      Lemma_Product_Monotone (0.0, C, 0.0, T);

      --  The square of the root, times Cm**2 * Sm, is at most nine quarters of the cube of the snap limit.
      Lemma_At_Most_Product (Rr, 1.5 * Sig, Rr, 1.5 * Sig, N_20, N_20, N_48);
      Lemma_At_Most_Scaled (Rr * Rr, (1.5 * Sig) * (1.5 * Sig), K, N_48);
      Lemma_About_Product (C * Sig, S, C * Sig, S, N_1, N_1, N_3);
      Lemma_At_Most_From_About (X1, S * S, N_3);
      Lemma_At_Most_Scaled (X1, S * S, 2.25 * S, N_3);
      pragma Assert (K * ((1.5 * Sig) * (1.5 * Sig)) = (2.25 * S) * X1);
      pragma Assert ((2.25 * S) * (S * S) = 2.25 * S3);
      Lemma_At_Most_Chain (K * (Rr * Rr), (2.25 * S) * X1, 2.25 * S3, N_48, N_3, N_64);

      --  So the acceleration limit times Cm**2 is at most twice that cube.
      Lemma_About_Symmetric (Rr * Rr * K, M, N_128, N_256);
      Lemma_At_Most_From_About (M, Rr * Rr * K, N_256);
      pragma Assert (Rr * Rr * K = K * (Rr * Rr));
      Lemma_At_Most_Chain (M, K * (Rr * Rr), 2.25 * S3, N_256, N_64, N_512);
      pragma Assert (AC <= 2.0 * S3 + N_512 * (2.25 * S3));
      pragma Assert (N_512 * (2.25 * S3) <= N_1024 * (2.0 * S3));
      pragma Assert (At_Most (AC, 2.0 * S3, N_1024));

      --  The cube of the peak snap is therefore at most the cube of the snap limit.
      Lemma_At_Most_From_About (2.0 * C * (T * T * T), A, Cube_Tolerance);
      Lemma_At_Most_Scaled (2.0 * C * (T * T * T), A, C * C, Cube_Tolerance);
      pragma Assert ((C * C) * A = AC);
      Lemma_At_Most_Chain ((C * C) * (2.0 * C * (T * T * T)), AC, 2.0 * S3, Cube_Tolerance, N_1024, Cubic_Inner);
      pragma Assert ((C * C) * (2.0 * C * (T * T * T)) = 2.0 * (CT * CT * CT));
      pragma Assert (2.0 * S3 = 2.0 * (S * S * S));
      Lemma_At_Most_Cancel (CT * CT * CT, S * S * S, 2.0, Cubic_Inner);
      Lemma_At_Most_Cube_Root (CT, S, Cubic_Inner);
      Lemma_At_Most_Weaken (CT, S, Cubic_Inner, Cubic_Tolerance);

      --  And the peak jerk, which is that snap squared over the crackle, at most the jerk limit.
      Lemma_At_Most_Product (CT, S, CT, S, Cubic_Inner, Cubic_Inner, Twice_Inner);
      Lemma_Real_Jerk_Above (S, J, C, Sig, Kap);
      Lemma_Product_Monotone (0.0, C, 0.0, J);
      Lemma_At_Most_Chain (CT * CT, S * S, C * J, Twice_Inner, N_16, Twice_Cubic_Tolerance);
      pragma Assert (CT * CT = C * (C * T * T));
      Lemma_At_Most_Cancel (C * T * T, J, C, Twice_Cubic_Tolerance);
   end Lemma_Real_Region_3;

   procedure Lemma_Region_3 (Am, Jm, Sm, Cm, JS, SC, SR, T1 : Raw) is
   begin
      Lemma_Limit (Am);
      Lemma_Limit (Jm);
      Lemma_Limit (Sm);
      Lemma_Limit (Cm);
      Lemma_Real_Region_3 (R (Sm), R (Jm), R (Am), R (Cm), R (SC), R (JS), R (SR), R (T1));
   end Lemma_Region_3;

   ------------------------------------------------------------------------------------------------------------------
   --  Region 4: jerk and acceleration are reached, snap is not
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Region_4 (S, J, A, C, Sig, Kap, Alp, Gam, T, T3 : Real)
   with
     Pre  =>
       S >= 0.0
       and then J >= 0.0
       and then A >= 0.0
       and then C >= 0.0
       and then Sig >= 0.0
       and then Kap >= 0.0
       and then Alp >= 0.0
       and then Gam >= 0.0
       and then T >= 0.0
       and then T3 >= 0.0
       and then About (C * Sig, S, N_1)
       and then About (S * Kap, J, N_1)
       and then About (J * Alp, A, N_1)
       and then About (C * Gam, J, N_1)
       and then At_Most (Kap, Sig, N_4)
       and then About (T * T, Gam, N_64)
       and then abs (T3 - (Alp - 2.0 * T)) <= N_8 * Alp,
     Post =>
       About (C * T * T, J, N_128)
       and then About (2.0 * T + T3, Alp, N_8)
       and then About (C * T * T * (2.0 * T + T3), A, N_512)
       and then At_Most (C * T, S, N_256)
   is
      CT : constant Real := C * T;
   begin
      Lemma_About_Scaled (T * T, Gam, C, N_64);
      Lemma_About_Chain (C * (T * T), C * Gam, J, N_64, N_1, N_128);
      pragma Assert (C * (T * T) = C * T * T);
      pragma Assert (About (2.0 * T + T3, Alp, N_8));
      Lemma_About_Product (C * T * T, J, 2.0 * T + T3, Alp, N_128, N_8, N_256);
      Lemma_About_Chain (C * T * T * (2.0 * T + T3), J * Alp, A, N_256, N_1, N_512);

      Lemma_At_Most_From_About (C * T * T, J, N_128);
      Lemma_At_Most_Scaled (C * T * T, J, C, N_128);
      Lemma_Real_Jerk_Below (S, J, C, Sig, Kap);
      Lemma_Product_Monotone (0.0, S, 0.0, S);
      Lemma_At_Most_Chain (C * (C * T * T), C * J, S * S, N_128, N_16, N_256);
      pragma Assert (C * (C * T * T) = CT * CT);
      Lemma_Product_Monotone (0.0, C, 0.0, T);
      Lemma_At_Most_Root (CT, S, N_256);
   end Lemma_Real_Region_4;

   procedure Lemma_Region_4 (Am, Jm, Sm, Cm, JS, SC, AJ, JC, T1, T3 : Raw) is
      S : constant Real := R (Sm);
      J : constant Real := R (Jm);
      A : constant Real := R (Am);
      C : constant Real := R (Cm);
      T : constant Real := R (T1);
   begin
      Lemma_Limit (Am);
      Lemma_Limit (Jm);
      Lemma_Limit (Sm);
      Lemma_Limit (Cm);
      Rounding.Lemma_Nonnegative (T3);
      Lemma_Real_Region_4 (S, J, A, C, R (SC), R (JS), R (AJ), R (JC), T, R (T3));
      Lemma_At_Most_Weaken (C * T, S, N_256, N_4096);
      Lemma_About_Weaken (C * T * T, J, N_128, N_4096);
      Lemma_About_Weaken (2.0 * T + R (T3), R (AJ), N_8, N_4096);
      Lemma_About_Weaken (C * T * T * (2.0 * T + R (T3)), A, N_512, N_4096);
      Lemma_About_Weaken (T * T, R (JC), N_64, N_4096);
   end Lemma_Region_4;

   ------------------------------------------------------------------------------------------------------------------
   --  Region 5: as Region 3, where the jerk limit is what bounds the first phase time
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Region_5 (S, J, A, C, Sig, Kap, Alp, Gam, Q4, T : Real)
   with
     Pre  =>
       S >= 0.0
       and then J >= 0.0
       and then A >= 0.0
       and then C > 0.0
       and then Sig >= 0.0
       and then Kap >= 0.0
       and then Alp >= 0.0
       and then Gam >= 0.0
       and then Q4 >= 0.0
       and then T >= 0.0
       and then About (C * Sig, S, N_1)
       and then About (S * Kap, J, N_1)
       and then About (J * Alp, A, N_1)
       and then About (C * Gam, J, N_1)
       and then At_Most (Kap, Sig, N_4)
       and then About (Q4 * Q4, Gam, N_64)
       and then At_Most (Alp, 2.0 * Q4, N_8)
       and then About (2.0 * C * (T * T * T), A, Cube_Tolerance),
     Post => At_Most (C * T, S, Cubic_Tolerance) and then At_Most (C * T * T, J, Twice_Cubic_Tolerance)
   is
      CQ : constant Real := C * Q4;
   begin
      Lemma_Product_Monotone (0.0, Q4, 0.0, Q4);
      Lemma_Product_Monotone (0.0, C, 0.0, Q4 * Q4);
      Lemma_Product_Monotone (0.0, C, 0.0, Q4);
      Lemma_Product_Monotone (0.0, J, 0.0, Alp);

      --  The acceleration limit is at most what the square root, as a first phase time, would give.
      Lemma_About_Symmetric (J * Alp, A, N_1, N_2);
      Lemma_At_Most_From_About (A, J * Alp, N_2);
      Lemma_About_Symmetric (C * Gam, J, N_1, N_2);
      Lemma_At_Most_From_About (J, C * Gam, N_2);
      Lemma_About_Symmetric (Q4 * Q4, Gam, N_64, N_128);
      Lemma_At_Most_From_About (Gam, Q4 * Q4, N_128);
      Lemma_At_Most_Scaled (Gam, Q4 * Q4, C, N_128);
      Lemma_At_Most_Chain (J, C * Gam, C * (Q4 * Q4), N_2, N_128, N_256);
      Lemma_At_Most_Product (J, C * (Q4 * Q4), Alp, 2.0 * Q4, N_256, N_8, N_512);
      Lemma_Product_Monotone (0.0, C * (Q4 * Q4), 0.0, 2.0 * Q4);
      Lemma_At_Most_Chain (A, J * Alp, (C * (Q4 * Q4)) * (2.0 * Q4), N_2, N_512, N_1024);
      pragma Assert ((C * (Q4 * Q4)) * (2.0 * Q4) = 2.0 * C * (Q4 * Q4 * Q4));
      Lemma_Real_Cubic (S, J, A, C, T, Q4, N_1024);

      --  The peak jerk.
      Lemma_At_Most_Product (T, Q4, T, Q4, Cubic_Inner, Cubic_Inner, Twice_Inner);
      Lemma_At_Most_From_About (Q4 * Q4, Gam, N_64);
      Lemma_At_Most_Chain (T * T, Q4 * Q4, Gam, Twice_Inner, N_64, Twice_Inner_More);
      Lemma_At_Most_Scaled (T * T, Gam, C, Twice_Inner_More);
      Lemma_At_Most_From_About (C * Gam, J, N_1);
      Lemma_At_Most_Chain (C * (T * T), C * Gam, J, Twice_Inner_More, N_1, Twice_Cubic_Tolerance);
      pragma Assert (C * (T * T) = C * T * T);

      --  The peak snap: the square root as a first phase time is within the snap limit.
      Lemma_At_Most_Scaled (Q4 * Q4, Gam, C, N_64);
      Lemma_At_Most_Chain (C * (Q4 * Q4), C * Gam, J, N_64, N_1, N_128);
      Lemma_At_Most_Scaled (C * (Q4 * Q4), J, C, N_128);
      Lemma_Real_Jerk_Below (S, J, C, Sig, Kap);
      Lemma_Product_Monotone (0.0, S, 0.0, S);
      Lemma_At_Most_Chain (C * (C * (Q4 * Q4)), C * J, S * S, N_128, N_16, N_256);
      pragma Assert (C * (C * (Q4 * Q4)) = CQ * CQ);
      Lemma_At_Most_Root (CQ, S, N_256);
      Lemma_At_Most_Scaled (T, Q4, C, Cubic_Inner);
      Lemma_At_Most_Chain (C * T, CQ, S, Cubic_Inner, N_256, Cubic_Tolerance);
   end Lemma_Real_Region_5;

   procedure Lemma_Region_5 (Am, Jm, Sm, Cm, JS, SC, AJ, JC, Q4, T1 : Raw) is
   begin
      Lemma_Limit (Am);
      Lemma_Limit (Jm);
      Lemma_Limit (Sm);
      Lemma_Limit (Cm);
      Lemma_Real_Region_5 (R (Sm), R (Jm), R (Am), R (Cm), R (SC), R (JS), R (AJ), R (JC), R (Q4), R (T1));
   end Lemma_Region_5;

   ------------------------------------------------------------------------------------------------------------------
   --  The facts as the solvers keep them
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Quotient_Is_Operand (Q, N, D : Raw) is
   begin
      Lemma_Quotient_Bounds (Q, N, D);
   end Lemma_Quotient_Is_Operand;

   procedure Lemma_Gap (X, P, Q : Raw) is
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Minuend_Min, P);
      Rounding.Lemma_Nonnegative (Q);
      Lemma_Remainder_1 (X, P, Q);
      Lemma_Remainder_Sign (X, R (P), R (Q), N_4);
   end Lemma_Gap;

   procedure Lemma_Gap_2 (X, P, Q1, Q2 : Raw) is
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Minuend_Min, P);
      Rounding.Lemma_Nonnegative (Q1);
      Rounding.Lemma_Nonnegative (Q2);
      Lemma_Remainder_2 (X, P, Q1, Q2);
      Lemma_Remainder_Sign (X, R (P), R (Q1) + R (Q2), N_4);
   end Lemma_Gap_2;

   procedure Lemma_Scaled_Gap (X, SR, Sm, Cm, SC : Raw) is
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Minuend_Min, SR);
      Rounding.Lemma_Nonnegative (SC);
      Lemma_Scaled_Remainder (X, SR, Sm, Cm, SC);
      Lemma_Remainder_Sign (X, R (SR), 1.5 * R (SC), N_20);
   end Lemma_Scaled_Gap;

   procedure Lemma_Twice_Gap (X, P, T : Raw) is
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Minuend_Min, P);
      Rounding.Lemma_Order (Minuend_Min, T);
      Lemma_Remainder_Twice (X, P, T);
      Lemma_Remainder_Sign (X, R (P), 2.0 * R (T), N_8);
   end Lemma_Twice_Gap;

   procedure Lemma_Snap_Root_Is_Minuend (SR, Am, Sm, Cm : Raw) is
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Minuend_Min, SR);
   end Lemma_Snap_Root_Is_Minuend;

   procedure Lemma_Square_Root_Is_Doubled (T, X : Raw) is
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Order (Minuend_Min, T);
   end Lemma_Square_Root_Is_Doubled;

   procedure Lemma_Holds (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Cm : Crackle)
   with
     Pre  => Holds_Limits (Solver, Am, Jm, Cm),
     Post => R (Solver.Am) = R (Am) and then R (Solver.Jm) = R (Jm) and then R (Solver.Cm) = R (Cm)
   is
   begin
      Rounding.Lemma_Same (Solver.Am, Am);
      Rounding.Lemma_Same (Solver.Jm, Jm);
      Rounding.Lemma_Same (Solver.Cm, Cm);
   end Lemma_Holds;

   procedure Lemma_Snap_Ceiling (Peak : Real; Sm : Raw)
   with Pre => In_Domain (Sm) and then At_Most (Peak, R (Sm), Prepared_Tolerance), Post => Peak <= Snap_Ceiling
   is
   begin
      Lemma_Limit (Sm);
   end Lemma_Snap_Ceiling;
   --  A snap peak within the snap limit is within the ceiling of the proved domain.

   procedure Lemma_No_Time (T : Raw) with Pre => T = 0.0, Post => R (T) = 0.0 is
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Same (T, 0.0);
   end Lemma_No_Time;

   procedure Lemma_Prepared_1 (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) is
      C   : constant Real := R (Cm);
      Sig : constant Real := R (Solver.Snap_Over_Crackle);
      Kap : constant Real := R (Solver.Jerk_Over_Snap);
      B   : constant Real := Sig + R (Solver.T2);
      Cc  : constant Real := 2.0 * Sig + R (Solver.T2) + R (Solver.T3);
   begin
      Lemma_Holds (Solver, Am, Jm, Cm);
      Rounding.Lemma_Same (Solver.T1, Solver.Snap_Over_Crackle);
      Lemma_Region_1
        (Am,
         Jm,
         Sm,
         Cm,
         Solver.Jerk_Over_Snap,
         Solver.Snap_Over_Crackle,
         Solver.Accel_Over_Jerk,
         Solver.T2,
         Solver.T3);
      Lemma_About_Weaken (C * Sig, R (Sm), N_32, Prepared_Tolerance);
      Lemma_At_Most_From_About (C * Sig, R (Sm), Prepared_Tolerance);
      Lemma_About_Weaken (C * Sig * B, R (Jm), N_32, Region_Tolerance);
      Lemma_At_Most_From_About (C * Sig * B, R (Jm), Region_Tolerance);
      Lemma_At_Most_Weaken (C * Sig * B, R (Jm), Region_Tolerance, Prepared_Tolerance);
      Lemma_About_Weaken (C * Sig * B * Cc, R (Am), N_32, Peak_Tolerance);
      Lemma_About_Weaken (Cc, R (Solver.Accel_Over_Jerk), N_32, Region_Tolerance);
      Lemma_About_Same (Sig);
      Lemma_About_Sum (Sig, Sig, B, Kap, 0.0, N_32, Region_Tolerance);
      Lemma_Snap_Ceiling (C * Sig, Sm);
      pragma Assert (Sig + Kap = Kap + Sig);
      pragma Assert (Snap_Peak (Solver.Cm, Solver.T1) = C * Sig);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = C * Sig * B);
      pragma Assert (Acceleration_Span (Solver.T1, Solver.T2, Solver.T3) = Cc);
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = C * Sig * B * Cc);
      pragma Assert (Region_Is_Consistent (Solver));
      pragma Assert (Core_Is_Consistent (Solver));
      pragma Assert (Snap_Is_Within (Solver, Sm));
   end Lemma_Prepared_1;

   procedure Lemma_Prepared_2 (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) is
      C   : constant Real := R (Cm);
      Sig : constant Real := R (Solver.Snap_Over_Crackle);
      B   : constant Real := Sig + R (Solver.T2);
      Cc  : constant Real := 2.0 * Sig + R (Solver.T2);
   begin
      Lemma_Holds (Solver, Am, Jm, Cm);
      Rounding.Lemma_Same (Solver.T1, Solver.Snap_Over_Crackle);
      Lemma_No_Time (Solver.T3);
      Lemma_Region_2
        (Am,
         Jm,
         Sm,
         Cm,
         Solver.Jerk_Over_Snap,
         Solver.Snap_Over_Crackle,
         Solver.Accel_Over_Jerk,
         Solver.Snap_Root,
         Solver.T2);
      Lemma_About_Weaken (C * Sig, R (Sm), N_4096, Prepared_Tolerance);
      Lemma_At_Most_From_About (C * Sig, R (Sm), Prepared_Tolerance);
      Lemma_At_Most_Weaken (C * Sig * B, R (Jm), N_4096, Prepared_Tolerance);
      Lemma_About_Weaken (C * Sig * B * Cc, R (Am), N_4096, Peak_Tolerance);
      Lemma_About_Weaken (2.0 * (Sig + B), Sig + 2.0 * R (Solver.Snap_Root), N_4096, Region_Tolerance);
      Lemma_Snap_Ceiling (C * Sig, Sm);
      pragma Assert (Snap_Peak (Solver.Cm, Solver.T1) = C * Sig);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = C * Sig * B);
      pragma Assert (Acceleration_Span (Solver.T1, Solver.T2, Solver.T3) = Cc);
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = C * Sig * B * Cc);
      pragma Assert (Region_Is_Consistent (Solver));
      pragma Assert (Core_Is_Consistent (Solver));
      pragma Assert (Snap_Is_Within (Solver, Sm));
   end Lemma_Prepared_2;

   procedure Lemma_Single_Phase (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   with
     Pre  =>
       In_Domain (Sm)
       and then Solver.Region in Region_3 | Region_5
       and then Holds_Limits (Solver, Am, Jm, Cm)
       and then Solver.T2 = 0.0 * s
       and then Solver.T3 = 0.0 * s
       and then Region_3_Is_Sound (Am, Jm, Sm, Cm, Solver.T1),
     Post => Prepared_Fact (Solver, Am, Jm, Sm, Cm)
   is
      C : constant Real := R (Cm);
      T : constant Real := R (Solver.T1);
   begin
      Lemma_Holds (Solver, Am, Jm, Cm);
      Lemma_No_Time (Solver.T2);
      Lemma_No_Time (Solver.T3);
      Lemma_At_Most_Weaken (C * T, R (Sm), Cubic_Tolerance, Prepared_Tolerance);
      Lemma_At_Most_Weaken (C * T * T, R (Jm), Twice_Cubic_Tolerance, Prepared_Tolerance);
      Lemma_About_Weaken (2.0 * C * (T * T * T), R (Am), Cube_Tolerance, Peak_Tolerance);
      Lemma_Snap_Ceiling (C * T, Sm);
      pragma Assert (Snap_Peak (Solver.Cm, Solver.T1) = C * T);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = C * T * T);
      pragma Assert (Acceleration_Span (Solver.T1, Solver.T2, Solver.T3) = 2.0 * T);
      pragma Assert (C * T * T * (2.0 * T) = 2.0 * C * (T * T * T));
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = 2.0 * C * (T * T * T));
      pragma Assert (Region_Is_Consistent (Solver));
      pragma Assert (Core_Is_Consistent (Solver));
      pragma Assert (Snap_Is_Within (Solver, Sm));
   end Lemma_Single_Phase;
   --  Regions 3 and 5 alike.

   procedure Lemma_Prepared_3 (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) is
   begin
      Lemma_Region_3 (Am, Jm, Sm, Cm, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle, Solver.Snap_Root, Solver.T1);
      Lemma_Single_Phase (Solver, Am, Jm, Sm, Cm);
   end Lemma_Prepared_3;

   procedure Lemma_Prepared_4
     (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle; Q4 : Raw)
   is
      C  : constant Real := R (Cm);
      T  : constant Real := R (Solver.T1);
      Cc : constant Real := 2.0 * T + R (Solver.T3);
   begin
      Lemma_Holds (Solver, Am, Jm, Cm);
      Lemma_No_Time (Solver.T2);
      Rounding.Lemma_Same (Solver.T1, Q4);
      Lemma_Region_4
        (Am,
         Jm,
         Sm,
         Cm,
         Solver.Jerk_Over_Snap,
         Solver.Snap_Over_Crackle,
         Solver.Accel_Over_Jerk,
         Solver.Jerk_Over_Crackle,
         Q4,
         Solver.T3);
      Lemma_At_Most_Weaken (C * T, R (Sm), N_4096, Prepared_Tolerance);
      Lemma_About_Weaken (C * T * T, R (Jm), N_4096, Region_Tolerance);
      Lemma_At_Most_From_About (C * T * T, R (Jm), Region_Tolerance);
      Lemma_At_Most_Weaken (C * T * T, R (Jm), Region_Tolerance, Prepared_Tolerance);
      Lemma_About_Weaken (Cc, R (Solver.Accel_Over_Jerk), N_4096, Region_Tolerance);
      Lemma_About_Weaken (C * T * T * Cc, R (Am), N_4096, Peak_Tolerance);
      Lemma_About_Weaken (T * T, R (Solver.Jerk_Over_Crackle), N_4096, Region_Tolerance);
      Lemma_Snap_Ceiling (C * T, Sm);
      pragma Assert (Snap_Peak (Solver.Cm, Solver.T1) = C * T);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = C * T * T);
      pragma Assert (Acceleration_Span (Solver.T1, Solver.T2, Solver.T3) = Cc);
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = C * T * T * Cc);
      pragma Assert (Region_Is_Consistent (Solver));
      pragma Assert (Core_Is_Consistent (Solver));
      pragma Assert (Snap_Is_Within (Solver, Sm));
   end Lemma_Prepared_4;

   procedure Lemma_Prepared_5
     (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle; Q4 : Raw) is
   begin
      Lemma_Region_5
        (Am,
         Jm,
         Sm,
         Cm,
         Solver.Jerk_Over_Snap,
         Solver.Snap_Over_Crackle,
         Solver.Accel_Over_Jerk,
         Solver.Jerk_Over_Crackle,
         Q4,
         Solver.T1);
      Lemma_Single_Phase (Solver, Am, Jm, Sm, Cm);
   end Lemma_Prepared_5;

   procedure Lemma_Threshold_Value
     (SOA : Velocity; Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) is
   begin
      Lemma_Threshold (SOA, Sm, Cm, Solver.Snap_Over_Crackle);
   end Lemma_Threshold_Value;

   procedure Lemma_Same_Reals (Before, After : Delta_V_Solver)
   with
     Pre  => Same_But_Threshold (Before, After),
     Post =>
       R (After.Am) = R (Before.Am)
       and then R (After.Jm) = R (Before.Jm)
       and then R (After.Cm) = R (Before.Cm)
       and then R (After.T1) = R (Before.T1)
       and then R (After.T2) = R (Before.T2)
       and then R (After.T3) = R (Before.T3)
       and then R (After.Accel_Over_Jerk) = R (Before.Accel_Over_Jerk)
       and then R (After.Jerk_Over_Snap) = R (Before.Jerk_Over_Snap)
       and then R (After.Snap_Over_Crackle) = R (Before.Snap_Over_Crackle)
       and then R (After.Snap_Root) = R (Before.Snap_Root)
       and then R (After.Jerk_Over_Crackle) = R (Before.Jerk_Over_Crackle)
   is
   begin
      Rounding.Lemma_Same (After.Am, Before.Am);
      Rounding.Lemma_Same (After.Jm, Before.Jm);
      Rounding.Lemma_Same (After.Cm, Before.Cm);
      Rounding.Lemma_Same (After.T1, Before.T1);
      Rounding.Lemma_Same (After.T2, Before.T2);
      Rounding.Lemma_Same (After.T3, Before.T3);
      Rounding.Lemma_Same (After.Accel_Over_Jerk, Before.Accel_Over_Jerk);
      Rounding.Lemma_Same (After.Jerk_Over_Snap, Before.Jerk_Over_Snap);
      Rounding.Lemma_Same (After.Snap_Over_Crackle, Before.Snap_Over_Crackle);
      Rounding.Lemma_Same (After.Snap_Root, Before.Snap_Root);
      Rounding.Lemma_Same (After.Jerk_Over_Crackle, Before.Jerk_Over_Crackle);
   end Lemma_Same_Reals;

   procedure Lemma_Threshold_Set
     (Before, After : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   is
      T : constant Real := R (After.T1);
   begin
      Lemma_Same_Reals (Before, After);
      Lemma_Holds (Before, Am, Jm, Cm);
      Rounding.Lemma_Same (Before.T1, Before.Snap_Over_Crackle);
      pragma Assert (Snap_Peak (After.Cm, After.T1) = Snap_Peak (Before.Cm, Before.T1));
      pragma Assert (Jerk_Span (After.T1, After.T2) = Jerk_Span (Before.T1, Before.T2));
      pragma
        Assert
          (Acceleration_Span (After.T1, After.T2, After.T3) = Acceleration_Span (Before.T1, Before.T2, Before.T3));
      pragma Assert (Jerk_Peak (After.Cm, After.T1, After.T2) = Jerk_Peak (Before.Cm, Before.T1, Before.T2));
      pragma
        Assert
          (Acceleration_Peak (After.Cm, After.T1, After.T2, After.T3)
           = Acceleration_Peak (Before.Cm, Before.T1, Before.T2, Before.T3));
      pragma Assert (Region_Is_Consistent (After));
      pragma Assert (Core_Is_Consistent (After));
      pragma Assert (Snap_Is_Within (After, Sm));
      Lemma_About_Weaken (R (After.Snap_Only_Above), 8.0 * (R (Cm) * T) * T * T * T, N_32, Region_Tolerance);
      pragma Assert (Threshold_Is_Consistent (After));
   end Lemma_Threshold_Set;

   procedure Lemma_No_Threshold (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) is
   begin
      null;
   end Lemma_No_Threshold;

   procedure Lemma_Is_Sound (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) is
   begin
      null;
   end Lemma_Is_Sound;

   ------------------------------------------------------------------------------------------------------------------
   --  A solve: the last phase time
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Real_Step_Down (D, Rr, Qb : Real)
   with
     Pre  => D >= 0.0 and then abs (Rr - D) <= Epsilon * abs D + Eta and then Rr <= Qb and then Qb >= 0.0,
     Post => D <= Qb + N_1 * Qb + 2.0 * Eta
   is
   begin
      pragma Assert (D - Epsilon * D <= Qb + Eta);
      pragma Assert ((1.0 + N_1) * (D - Epsilon * D) >= D);
      pragma Assert (D <= (1.0 + N_1) * (Qb + Eta));
   end Lemma_Real_Step_Down;
   --  D was rounded to at most Qb, so D is at most Qb and a little.

   procedure Lemma_Real_Left_3 (X, P, Q1, Q2, Q3, R1, R2 : Real)
   with
     Pre  =>
       P >= 0.0
       and then Q1 >= 0.0
       and then Q2 >= 0.0
       and then Q3 >= 0.0
       and then Q1 <= P
       and then Q2 <= R1
       and then Q3 <= R2
       and then Q1 + Q2 + Q3 >= Span_Sum_Floor
       and then abs (R1 - (P - Q1)) <= Epsilon * abs (P - Q1) + Eta
       and then abs (R2 - (R1 - Q2)) <= Epsilon * abs (R1 - Q2) + Eta
       and then abs (X - (R2 - Q3)) <= Epsilon * abs (R2 - Q3) + Eta,
     Post => abs (X - (P - (Q1 + Q2 + Q3))) <= N_8 * P and then Q1 + Q2 + Q3 <= P + N_8 * P
   is
   begin
      pragma Assert (R1 <= P + Epsilon * P + Eta);
      pragma Assert (R2 <= R1 + Epsilon * R1 + Eta);
      pragma Assert (Q1 + Q2 + Q3 <= P + R1 + R2);
      pragma Assert (P >= Two_M210);
      pragma Assert (Eta <= Two_M600 * P);
      pragma Assert (R1 <= 1.01 * P);
      pragma Assert (R2 <= 1.03 * P);
      pragma Assert (abs (R1 - (P - Q1)) <= Epsilon * P + Two_M600 * P);
      pragma Assert (abs (R2 - (R1 - Q2)) <= Epsilon * (1.01 * P) + Two_M600 * P);
      pragma Assert (abs (X - (R2 - Q3)) <= Epsilon * (1.03 * P) + Two_M600 * P);
      pragma
        Assert
          (abs (X - (P - (Q1 + Q2 + Q3)))
           <= Epsilon * P + Epsilon * (1.01 * P) + Epsilon * (1.03 * P) + 3.0 * (Two_M600 * P));
      pragma Assert (Q3 <= R1 - Q2 + Epsilon * (1.01 * P) + Two_M600 * P);
   end Lemma_Real_Left_3;

   procedure Lemma_Remainder_3 (X, P, Q1, Q2, Q3 : Raw)
   with
     Pre  =>
       Is_Subtrahend (P)
       and then Is_Subtrahend (Q1)
       and then Is_Subtrahend (Q2)
       and then Is_Subtrahend (Q3)
       and then Gap_OK (P, Q1, Q2)
       and then R (Q1) + R (Q2) + R (Q3) >= Span_Sum_Floor
       and then X = P - Q1 - Q2 - Q3,
     Post => Remainder_Fact (X, R (P), R (Q1) + R (Q2) + R (Q3), N_8)
   is
      E1 : constant Raw := P - Q1;
      E2 : constant Raw := E1 - Q2;
      Q  : constant Real := R (Q1) + R (Q2) + R (Q3);
   begin
      Lemma_Domain_Constants;
      Rounding.Lemma_Nonnegative (P);
      Rounding.Lemma_Nonnegative (Q1);
      Rounding.Lemma_Nonnegative (Q2);
      Rounding.Lemma_Nonnegative (Q3);
      Lemma_Difference_Sign (P, Q1);
      Lemma_Difference_Sign (E1, Q2);
      Lemma_Difference_Sign (E2, Q3);
      Float_Lemmas.Lemma_Rounding_Error_Sub (P, Q1);
      Float_Lemmas.Lemma_Rounding_Error_Sub (E1, Q2);
      Float_Lemmas.Lemma_Rounding_Error_Sub (E2, Q3);
      Rounding.Lemma_Same (X, E2 - Q3);
      pragma Assert (Two_M600 * Q >= 3.0 * Eta);
      if X >= 0.0 then
         pragma Assert (E2 >= Q3);
         pragma Assert (E1 >= Q2);
         pragma Assert (P >= Q1);
         Rounding.Lemma_Order (Q3, E2);
         Rounding.Lemma_Order (Q2, E1);
         Rounding.Lemma_Order (Q1, P);
         Lemma_Real_Left_3 (R (X), R (P), R (Q1), R (Q2), R (Q3), R (E1), R (E2));
      end if;
      if X <= 0.0 then
         if P <= Q1 then
            Rounding.Lemma_Order (P, Q1);
            Lemma_Scale_Monotone (0.0, N_8, Q);
         elsif E1 <= Q2 then
            Rounding.Lemma_Order (Q1, P);
            Rounding.Lemma_Order (E1, Q2);
            Lemma_Real_Step_Down (R (P) - R (Q1), R (E1), R (Q2));
            pragma Assert (R (P) <= Q + N_1 * Q + 2.0 * Eta);
         else
            pragma Assert (E2 <= Q3);
            Rounding.Lemma_Order (Q1, P);
            Rounding.Lemma_Order (Q2, E1);
            Rounding.Lemma_Order (E2, Q3);
            Lemma_Real_Step_Down (R (E1) - R (Q2), R (E2), R (Q3));
            Lemma_Real_Step_Down (R (P) - R (Q1), R (E1), R (Q2) + R (Q3) + N_1 * R (Q3) + 2.0 * Eta);
            pragma Assert (R (P) <= Q + N_3 * Q + 5.0 * Eta);
         end if;
      end if;
   end Lemma_Remainder_3;
   --  What is left after three values are taken away, where the minuend may be tiny but what is taken is not.

   procedure Lemma_Real_Speed (X, P, V, A, Q, E : Real; Nonnegative, Nonpositive : Boolean)
   with
     Pre  =>
       P >= 0.0
       and then V >= 0.0
       and then A >= Two_M80
       and then A <= Two_P80
       and then Q >= Span_Sum_Floor
       and then E * A = V
       and then abs (P - E) <= Epsilon * abs E + Eta
       and then (if Nonnegative then abs (X - (P - Q)) <= N_8 * P and then Q <= P + N_8 * P)
       and then (if Nonpositive then P <= Q + N_8 * Q),
     Post =>
       (if Nonnegative then abs (X * A - (V - A * Q)) <= N_16 * V and then A * Q <= V + N_16 * V)
       and then (if Nonpositive then V <= A * Q + N_16 * (A * Q))
   is
   begin
      Lemma_Product_Cancel (0.0, E, A);
      if Nonnegative then
         pragma Assert (P >= Two_M201);
         pragma Assert (E >= Two_M202);
         pragma Assert (Eta <= Two_M800 * E);
         pragma Assert (abs (P - E) <= N_1 * E);
         --  The speed time times the acceleration is the speed change.
         Lemma_Scale_Monotone (P - E, N_1 * E, A);
         Lemma_Scale_Monotone (E - P, N_1 * E, A);
         pragma Assert ((N_1 * E) * A = N_1 * V);
         pragma Assert (abs (P * A - V) <= N_1 * V);
         --  And what is left of it, times the acceleration, is what is left of the speed change.
         Lemma_Scale_Monotone (X - (P - Q), N_8 * P, A);
         Lemma_Scale_Monotone ((P - Q) - X, N_8 * P, A);
         pragma Assert ((N_8 * P) * A = N_8 * (P * A));
         pragma Assert (X * A - (V - A * Q) = (X - (P - Q)) * A + (P * A - V));
         pragma Assert (abs (X * A - (V - A * Q)) <= N_8 * (V + N_1 * V) + N_1 * V);
         Lemma_Scale_Monotone (Q, P + N_8 * P, A);
         pragma Assert ((P + N_8 * P) * A = (1.0 + N_8) * (P * A));
         pragma Assert (A * Q <= (1.0 + N_8) * (V + N_1 * V));
      end if;
      if Nonpositive then
         pragma Assert (E - Epsilon * E <= P + Eta);
         pragma Assert ((1.0 + N_1) * (E - Epsilon * E) >= E);
         pragma Assert (E <= (1.0 + N_1) * (P + Eta));
         pragma Assert (Eta <= Two_M800 * Q);
         pragma Assert (E <= (1.0 + N_1) * (Q + N_8 * Q + Two_M800 * Q));
         pragma Assert (E <= Q + N_16 * Q);
         Lemma_Scale_Monotone (E, Q + N_16 * Q, A);
         pragma Assert ((Q + N_16 * Q) * A = A * Q + N_16 * (A * Q));
      end if;
   end Lemma_Real_Speed;

   procedure Lemma_Speed_Gap (X, P, Vd, Am, Q1, Q2, Q3 : Raw) is
      V : constant Real := R (Vd);
      A : constant Real := R (Am);
   begin
      Lemma_Domain_Constants;
      Lemma_Limit (Am);
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Nonnegative (P);
      Lemma_Remainder_3 (X, P, Q1, Q2, Q3);
      Float_Lemmas.Lemma_Rounding_Error_Div (Vd, Am);
      Rounding.Lemma_Same (P, Vd / Am);
      Lemma_Quotient_Times_Divisor (V, A);
      Lemma_Real_Speed (R (X), R (P), V, A, R (Q1) + R (Q2) + R (Q3), V / A, X >= 0.0, X <= 0.0);
   end Lemma_Speed_Gap;

   procedure Lemma_Speed_Rebase (X : Raw; V, A, Q, Q2, U, W, U2 : Real) is
   begin
      Lemma_Scale_Monotone (W, Relative_Max, Q2);
      Lemma_Scale_Monotone (0.0, W, Q2);
      Lemma_Product_Monotone (0.0, A, 0.0, Q2);
      Lemma_Product_Monotone (0.0, U, 0.0, W);
      Lemma_Product_Monotone (U, Relative_Max, W, Relative_Max);
      if X >= 0.0 then
         --  A * Q2 is at most V, to within U + 2 * W and a second-order term.
         pragma Assert (Q2 - W * Q2 <= Q);
         pragma Assert ((1.0 + 2.0 * W) * (Q2 - W * Q2) = Q2 + (W - 2.0 * (W * W)) * Q2);
         Lemma_Product_Monotone (W, Relative_Max, W, Relative_Max);
         Lemma_Scale_Monotone (W, Relative_Max, W);
         pragma Assert (W - 2.0 * (W * W) >= 0.0);
         Lemma_Scale_Monotone (0.0, W - 2.0 * (W * W), Q2);
         pragma Assert (Q >= 0.0);
         Lemma_Scale_Monotone (Q2 - W * Q2, Q, 1.0 + 2.0 * W);
         pragma Assert (Q2 <= (1.0 + 2.0 * W) * Q);
         Lemma_Scale_Monotone (Q2, (1.0 + 2.0 * W) * Q, A);
         pragma Assert (A * Q2 <= (1.0 + 2.0 * W) * (A * Q));
         Lemma_Scale_Monotone (A * Q, V + U * V, 1.0 + 2.0 * W);
         pragma Assert ((1.0 + 2.0 * W) * (V + U * V) = V + (U + 2.0 * W + 2.0 * (W * U)) * V);
         Lemma_Scale_Monotone (U + 2.0 * W + 2.0 * (W * U), U2, V);
         pragma Assert (A * Q2 <= V + U2 * V);
         --  The remainder moves by the acceleration times the difference of the two times.
         Lemma_Scale_Monotone (Q - Q2, W * Q2, A);
         Lemma_Scale_Monotone (Q2 - Q, W * Q2, A);
         pragma Assert (abs (A * Q - A * Q2) <= W * (A * Q2));
         Lemma_Scale_Monotone (U + 2.0 * W + 2.0 * (W * U), 1.0, V);
         pragma Assert (A * Q2 <= 2.0 * V);
         Lemma_Scale_Monotone (A * Q2, 2.0 * V, W);
         pragma Assert (abs (R (X) * A - (V - A * Q2)) <= U * V + W * (2.0 * V));
         Lemma_Scale_Monotone (U + 2.0 * W, U2, V);
      end if;
      if X <= 0.0 then
         pragma Assert (Q <= Q2 + W * Q2);
         pragma Assert (Q >= 0.0);
         Lemma_Scale_Monotone (Q, Q2 + W * Q2, A);
         Lemma_Product_Monotone (0.0, A, 0.0, Q);
         Lemma_Scale_Monotone (A * Q, A * (Q2 + W * Q2), 1.0 + U);
         pragma Assert (V <= (1.0 + U) * (A * (Q2 + W * Q2)));
         pragma Assert ((1.0 + U) * (A * (Q2 + W * Q2)) = A * Q2 + (U + W + U * W) * (A * Q2));
         Lemma_Scale_Monotone (U + W + U * W, U2, A * Q2);
      end if;
   end Lemma_Speed_Rebase;

   ------------------------------------------------------------------------------------------------------------------
   --  A solve: the third phase time where jerk is the highest limit reached
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Square_Is_Product (X : Raw) with Pre => abs X <= Operand_Top, Post => X**2 = X * X is
   begin
      null;
   end Lemma_Square_Is_Product;

   procedure Lemma_Real_Radicand (Base, V, J, K, Q4, Br, Vr, Qd, Qr, Wr : Real)
   with
     Pre  =>
       Base >= Two_M410
       and then V >= 0.0
       and then J >= Two_M80
       and then J <= Two_P80
       and then K >= 1.0
       and then K <= 4.0
       and then Q4 * J = K * V
       and then abs (Br - Base) <= N_1 * Base
       and then abs (Vr - K * V) <= Epsilon * abs (K * V) + Eta
       and then Qd * J = Vr
       and then abs (Qr - Qd) <= Epsilon * abs Qd + Eta
       and then abs (Wr - (Br + Qr)) <= Epsilon * abs (Br + Qr) + Eta,
     Post => Q4 >= 0.0 and then abs (Wr - (Base + Q4)) <= N_6 * (Base + Q4)
   is
   begin
      Lemma_Product_Cancel (0.0, Q4, J);
      --  The quotient of the rounded numerator is the exact quotient to within the rounding of the numerator.
      pragma Assert ((Qd - Q4) * J = Vr - K * V);
      pragma Assert ((Epsilon * Q4 + Two_M995) * J >= Epsilon * (K * V) + Eta);
      Lemma_Product_Cancel (Qd - Q4, Epsilon * Q4 + Two_M995, J);
      Lemma_Product_Cancel (Q4 - Qd, Epsilon * Q4 + Two_M995, J);
      pragma Assert (abs (Qd - Q4) <= Epsilon * Q4 + Two_M995);
      pragma Assert (abs Qd <= Q4 + Epsilon * Q4 + Two_M995);
      pragma Assert (abs (Qr - Q4) <= N_2 * Q4 + Two_M990);
      pragma Assert (abs (Br + Qr - (Base + Q4)) <= N_2 * (Base + Q4) + Two_M990);
      pragma Assert (abs (Br + Qr) <= 2.0 * (Base + Q4) + Two_M990);
      pragma Assert (abs (Wr - (Base + Q4)) <= N_4 * (Base + Q4) + 4.0 * Two_M990);
      pragma Assert (4.0 * Two_M990 <= N_1 * Base);
   end Lemma_Real_Radicand;
   --  The radicand of a jerk-limited time: a base which is not tiny, and K times the speed change over the jerk
   --  limit, which may be. Br, Vr, Qr and Wr are the rounded base, numerator, quotient and sum.

   procedure Lemma_Real_Jerk_Positive (T3, Hh, Kk, Rho, Gf, G, X, J, JX, Target : Real)
   with
     Pre  =>
       Rho >= 0.0
       and then Gf >= 0.0
       and then J >= 0.0
       and then X >= 0.0
       and then About (Hh, 0.5 * Rho, N_1)
       and then About (Kk, 1.5 * Gf, N_1)
       and then About (Gf, G, N_1)
       and then abs (Rho * Rho - X) <= N_128 * X
       and then abs (T3 - (Hh - Kk)) <= N_4 * Hh
       and then Kk <= Hh + N_4 * Hh
       and then JX = J * X
       and then About (JX, Target, N_4),
     Post => About ((2.0 * T3 + 3.0 * G) * (2.0 * T3 + 3.0 * G) * J, Target, N_1024)
   is
      Y : constant Real := 2.0 * T3 + 3.0 * G;
   begin
      Lemma_About_Bounds (Hh, 0.5 * Rho, N_1);
      Lemma_About_Bounds (Kk, 1.5 * Gf, N_1);
      Lemma_About_Bounds (Gf, G, N_1);
      pragma Assert (1.5 * Gf <= 0.51 * Rho);
      pragma Assert (abs (T3 - (0.5 * Rho - 1.5 * Gf)) <= N_4 * Rho);
      pragma Assert (G <= 0.35 * Rho);
      pragma Assert (abs (Y - Rho) <= N_16 * Rho);
      pragma Assert (About (Y, Rho, N_16));
      Lemma_About_Product (Y, Rho, Y, Rho, N_16, N_16, N_48);
      pragma Assert (About (Rho * Rho, X, N_128));
      Lemma_About_Chain (Y * Y, Rho * Rho, X, N_48, N_128, N_256);
      Lemma_About_Scaled (Y * Y, X, J, N_256);
      Lemma_About_Chain (J * (Y * Y), J * X, Target, N_256, N_4, N_1024);
      pragma Assert (J * (Y * Y) = Y * Y * J);
   end Lemma_Real_Jerk_Positive;
   --  Where the third phase time is positive: twice it and three times the base is the root.

   procedure Lemma_Real_Jerk_Nonpositive (Hh, Kk, Rho, Gf, G, X, J, JX, Target : Real)
   with
     Pre  =>
       Rho >= 0.0
       and then Gf >= 0.0
       and then G >= 0.0
       and then J >= 0.0
       and then X >= 0.0
       and then About (Hh, 0.5 * Rho, N_1)
       and then About (Kk, 1.5 * Gf, N_1)
       and then About (Gf, G, N_1)
       and then abs (Rho * Rho - X) <= N_128 * X
       and then Hh <= Kk + N_4 * Kk
       and then JX = J * X
       and then About (Target, JX, N_4),
     Post => At_Most (Target, 9.0 * (G * G) * J, N_1024)
   is
   begin
      Lemma_About_Bounds (Hh, 0.5 * Rho, N_1);
      Lemma_About_Bounds (Kk, 1.5 * Gf, N_1);
      Lemma_About_Bounds (Gf, G, N_1);
      pragma Assert (0.5 * Rho - N_1 * (0.5 * Rho) <= (1.0 + N_4) * (1.5 * Gf + N_1 * (1.5 * Gf)));
      pragma Assert (Rho <= 3.0 * G + N_16 * (3.0 * G));
      pragma Assert (At_Most (Rho, 3.0 * G, N_16));
      Lemma_At_Most_Product (Rho, 3.0 * G, Rho, 3.0 * G, N_16, N_16, N_48);
      pragma Assert ((3.0 * G) * (3.0 * G) = 9.0 * (G * G));
      pragma Assert (About (Rho * Rho, X, N_128));
      Lemma_About_Symmetric (Rho * Rho, X, N_128, N_256);
      Lemma_At_Most_From_About (X, Rho * Rho, N_256);
      Lemma_Product_Monotone (0.0, G, 0.0, G);
      Lemma_At_Most_Chain (X, Rho * Rho, 9.0 * (G * G), N_256, N_48, N_512);
      Lemma_At_Most_Scaled (X, 9.0 * (G * G), J, N_512);
      Lemma_At_Most_From_About (Target, JX, N_4);
      Lemma_Product_Monotone (0.0, J, 0.0, 9.0 * (G * G));
      Lemma_At_Most_Chain (Target, J * X, J * (9.0 * (G * G)), N_4, N_512, N_1024);
      pragma Assert (J * (9.0 * (G * G)) = 9.0 * (G * G) * J);
   end Lemma_Real_Jerk_Nonpositive;
   --  Where it is not positive: the root is at most three times the base.

   procedure Lemma_Jerk_Time (T3, Vd, Jm, JS, SC : Raw) is
      V : constant Real := R (Vd);
      J : constant Real := R (Jm);
      G : constant Real := R (JS) + R (SC);

      F_G  : constant Raw := JS + SC;
      F_G2 : constant Raw := F_G**2;
      F_V4 : constant Raw := 4.0 * Vd;
      F_Q  : constant Raw := F_V4 / Jm;
      F_W  : constant Raw := F_G2 + F_Q;
      F_R  : constant Raw := Dimensionless_Math.Sqrt (F_W);
      F_H  : constant Raw := 0.5 * F_R;
      F_K  : constant Raw := 1.5 * F_G;
      --  The values which the phase time is computed through.

      Gf  : constant Real := R (F_G);
      Rho : constant Real := R (F_R);
   begin
      Lemma_Domain_Constants;
      Lemma_Limit (Jm);
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Nonnegative (JS);
      Rounding.Lemma_Nonnegative (SC);
      pragma Assert (R (4.0) = 4.0);

      --  The base: the sum of the two quotients, and its square.
      Lemma_Near_Exact (JS);
      Lemma_Near_Exact (SC);
      Lemma_Near_Sum (R (JS), R (SC), JS, SC, 0.0, 0.0, N_1);
      Lemma_About_From_Near (G, F_G, N_1);
      Lemma_About_Bounds (Gf, G, N_1);
      pragma Assert (Gf >= Two_M201);
      Lemma_Square_Is_Product (F_G);
      Lemma_Near_Exact (F_G);
      Lemma_Product_Monotone (Two_M201, Gf, Two_M201, Gf);
      Lemma_Near_Product (Gf, Gf, F_G, F_G, 0.0, 0.0, N_1);
      Rounding.Lemma_Same (F_G2, F_G * F_G);

      --  The radicand.
      Float_Lemmas.Lemma_Rounding_Error_Mul (4.0, Vd);
      Float_Lemmas.Lemma_Rounding_Error_Div (F_V4, Jm);
      Float_Lemmas.Lemma_Rounding_Error_Add (F_G2, F_Q);
      Lemma_Quotient_Times_Divisor (4.0 * V, J);
      Lemma_Quotient_Times_Divisor (R (F_V4), J);
      declare
         Q4 : constant Real := (4.0 * V) / J;
         X  : constant Real := Gf * Gf + Q4;
      begin
         Lemma_Real_Radicand (Gf * Gf, V, J, 4.0, Q4, R (F_G2), R (F_V4), R (F_V4) / J, R (F_Q), R (F_W));
         pragma Assert (X >= Two_M410);
         pragma Assert (Near (X, F_W, N_6));
         Lemma_Root_Squared (X, F_W, N_6, N_128);
         pragma Assert (Rho * Rho >= X - N_128 * X);
         Lemma_Floor (Rho, Two_M410 - N_128 * Two_M410, Two_M210, Two_M410 * Two_M10);

         --  Half of the root, and one and a half times the base.
         Lemma_Near_Exact (0.5);
         Lemma_Near_Exact (F_R);
         Lemma_Near_Product (0.5, Rho, 0.5, F_R, 0.0, 0.0, N_1);
         Lemma_About_From_Near (0.5 * Rho, F_H, N_1);
         Lemma_Near_Exact (1.5);
         Lemma_Near_Product (1.5, Gf, 1.5, F_G, 0.0, 0.0, N_1);
         Lemma_About_From_Near (1.5 * Gf, F_K, N_1);

         --  The phase time is their difference.
         Rounding.Lemma_Order (Minuend_Min, F_H);
         Lemma_Remainder_1 (T3, F_H, F_K);
         pragma Assert (J * X = Gf * Gf * J + 4.0 * V);
         Lemma_About_Product (Gf, G, Gf, G, N_1, N_1, N_3);
         Lemma_About_Scaled (Gf * Gf, G * G, J, N_3);
         Lemma_Product_Monotone (0.0, G, 0.0, G);
         Lemma_Product_Monotone (0.0, J, 0.0, G * G);
         pragma Assert (abs (J * X - (G * G * J + 4.0 * V)) <= N_3 * (G * G * J + 4.0 * V));
         pragma Assert (About (J * X, G * G * J + 4.0 * V, N_3));
         if T3 > 0.0 then
            Lemma_About_Weaken (J * X, G * G * J + 4.0 * V, N_3, N_4);
            Lemma_Real_Jerk_Positive (R (T3), R (F_H), R (F_K), Rho, Gf, G, X, J, J * X, G * G * J + 4.0 * V);
         end if;
         if T3 <= 0.0 then
            Lemma_About_Symmetric (J * X, G * G * J + 4.0 * V, N_3, N_4);
            Lemma_Real_Jerk_Nonpositive (R (F_H), R (F_K), Rho, Gf, G, X, J, J * X, G * G * J + 4.0 * V);
         end if;
      end;
   end Lemma_Jerk_Time;

   procedure Lemma_Real_Only_Positive (T3, Kk, Rho, T, X, J, JX : Real)
   with
     Pre  =>
       Rho >= 0.0
       and then T >= 0.0
       and then J >= 0.0
       and then X >= 0.0
       and then About (Kk, 3.0 * T, N_1)
       and then abs (Rho * Rho - X) <= N_128 * X
       and then abs (T3 - (Rho - Kk)) <= N_4 * Rho
       and then Kk <= Rho + N_4 * Rho
       and then JX = J * X,
     Post => About ((T3 + 3.0 * T) * (T3 + 3.0 * T) * J, JX, N_1024)
   is
      Y : constant Real := T3 + 3.0 * T;
   begin
      Lemma_About_Bounds (Kk, 3.0 * T, N_1);
      pragma Assert (3.0 * T <= 1.01 * Rho);
      pragma Assert (abs (Y - Rho) <= N_8 * Rho);
      pragma Assert (About (Y, Rho, N_8));
      Lemma_About_Product (Y, Rho, Y, Rho, N_8, N_8, N_32);
      pragma Assert (About (Rho * Rho, X, N_128));
      Lemma_About_Chain (Y * Y, Rho * Rho, X, N_32, N_128, N_256);
      Lemma_About_Scaled (Y * Y, X, J, N_256);
      Lemma_About_Weaken (J * (Y * Y), J * X, N_256, N_1024);
      pragma Assert (J * (Y * Y) = Y * Y * J);
   end Lemma_Real_Only_Positive;

   procedure Lemma_Real_Only_Nonpositive (Kk, Rho, T, X, J, JX : Real)
   with
     Pre  =>
       Rho >= 0.0
       and then T >= 0.0
       and then J >= 0.0
       and then X >= 0.0
       and then About (Kk, 3.0 * T, N_1)
       and then abs (Rho * Rho - X) <= N_128 * X
       and then Rho <= Kk + N_4 * Kk
       and then JX = J * X,
     Post => At_Most (JX, 9.0 * (T * T) * J, N_1024)
   is
   begin
      Lemma_About_Bounds (Kk, 3.0 * T, N_1);
      pragma Assert (Rho <= 3.0 * T + N_8 * (3.0 * T));
      pragma Assert (At_Most (Rho, 3.0 * T, N_8));
      Lemma_At_Most_Product (Rho, 3.0 * T, Rho, 3.0 * T, N_8, N_8, N_32);
      pragma Assert ((3.0 * T) * (3.0 * T) = 9.0 * (T * T));
      pragma Assert (About (Rho * Rho, X, N_128));
      Lemma_About_Symmetric (Rho * Rho, X, N_128, N_256);
      Lemma_At_Most_From_About (X, Rho * Rho, N_256);
      Lemma_Product_Monotone (0.0, T, 0.0, T);
      Lemma_At_Most_Chain (X, Rho * Rho, 9.0 * (T * T), N_256, N_32, N_512);
      Lemma_At_Most_Scaled (X, 9.0 * (T * T), J, N_512);
      Lemma_At_Most_Weaken (J * X, J * (9.0 * (T * T)), N_512, N_1024);
      pragma Assert (J * (9.0 * (T * T)) = 9.0 * (T * T) * J);
   end Lemma_Real_Only_Nonpositive;

   procedure Lemma_Jerk_Only_Time (T3, Vd, Jm, JC, T1 : Raw) is
      V   : constant Real := R (Vd);
      J   : constant Real := R (Jm);
      Gam : constant Real := R (JC);
      T   : constant Real := R (T1);

      F_Q : constant Raw := Vd / Jm;
      F_W : constant Raw := JC + F_Q;
      F_R : constant Raw := Dimensionless_Math.Sqrt (F_W);
      F_K : constant Raw := 3.0 * T1;

      Rho : constant Real := R (F_R);
   begin
      Lemma_Domain_Constants;
      Lemma_Limit (Jm);
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Nonnegative (JC);
      Rounding.Lemma_Nonnegative (T1);
      pragma Assert (R (3.0) = 3.0 and then R (1.0) = 1.0);

      --  The radicand: the kept quotient and the speed change over the jerk limit.
      Float_Lemmas.Lemma_Rounding_Error_Mul (1.0, Vd);
      Lemma_Unit_Operations (Vd);
      Float_Lemmas.Lemma_Rounding_Error_Div (Vd, Jm);
      Float_Lemmas.Lemma_Rounding_Error_Add (JC, F_Q);
      Lemma_Quotient_Times_Divisor (V, J);
      declare
         Q1 : constant Real := V / J;
         X  : constant Real := Gam + Q1;
      begin
         pragma Assert (Q1 * J = 1.0 * V);
         Lemma_Real_Radicand (Gam, V, J, 1.0, Q1, Gam, V, Q1, R (F_Q), R (F_W));
         pragma Assert (Near (X, F_W, N_6));
         Lemma_Root_Squared (X, F_W, N_6, N_128);
         pragma Assert (Rho * Rho >= X - N_128 * X);
         Lemma_Floor (Rho, Two_M410 - N_128 * Two_M410, Two_M210, Two_M410 * Two_M10);

         Lemma_Near_Exact (3.0);
         Lemma_Near_Exact (T1);
         Lemma_Near_Product (3.0, T, 3.0, T1, 0.0, 0.0, N_1);
         Lemma_About_From_Near (3.0 * T, F_K, N_1);

         Rounding.Lemma_Order (Minuend_Min, F_R);
         Lemma_Remainder_1 (T3, F_R, F_K);
         pragma Assert (J * X = Gam * J + V);
         if T3 > 0.0 then
            Lemma_Real_Only_Positive (R (T3), R (F_K), Rho, T, X, J, J * X);
         end if;
         if T3 <= 0.0 then
            Lemma_Real_Only_Nonpositive (R (F_K), Rho, T, X, J, J * X);
         end if;
      end;
   end Lemma_Jerk_Only_Time;

   ------------------------------------------------------------------------------------------------------------------
   --  A solve: the phase time where no limit other than crackle is reached
   ------------------------------------------------------------------------------------------------------------------

   Tiny_Quotient : constant Raw := 2#1.0#E-788;
   Tiny_Root     : constant Raw := 2#1.0#E-394;
   Tiny_Fourth   : constant Raw := 2#1.0#E-197;

   procedure Lemma_Tiny_Constants
   with
     Post =>
       R (Tiny_Speed) = Two_M870
       and then R (Tiny_Quotient) = Two_M788
       and then R (Tiny_Root) = Two_M394
       and then R (Tiny_Fourth) = Two_M197
       and then R (0.125) = 0.125
   is
   begin
      null;
   end Lemma_Tiny_Constants;

   procedure Lemma_Real_Tiny_Quotient (V, C, Er, Qd, Qr : Real)
   with
     Pre  =>
       V >= 0.0
       and then V <= Two_M870
       and then C >= Two_M80
       and then abs (Er - 0.125 * V) <= Epsilon * abs (0.125 * V) + Eta
       and then Qd * C = Er
       and then abs (Qr - Qd) <= Epsilon * abs Qd + Eta,
     Post => Qr <= Two_M788
   is
   begin
      pragma Assert (Er <= Two_M872);
      pragma Assert (Two_M788 * C >= 2.0 * Two_M872);
      Lemma_Product_Cancel (Qd, 0.5 * Two_M788, C);
      if Qd >= 0.0 then
         pragma Assert (Qr <= Qd + Epsilon * Qd + Eta);
      else
         pragma Assert (Qr <= Eta);
      end if;
   end Lemma_Real_Tiny_Quotient;

   procedure Lemma_Real_Fourth (C, V, Q, S1, S2 : Real)
   with
     Pre  =>
       C >= 0.0
       and then Q >= 0.0
       and then Q * C = 0.125 * V
       and then S1 >= 0.0
       and then abs (S1 * S1 - Q) <= N_128 * Q
       and then S2 >= 0.0
       and then abs (S2 * S2 - S1) <= N_64 * S1,
     Post => About (8.0 * C * ((S2 * S2) * (S2 * S2)), V, N_512)
   is
   begin
      pragma Assert (About (S2 * S2, S1, N_64));
      Lemma_About_Product (S2 * S2, S1, S2 * S2, S1, N_64, N_64, N_256);
      pragma Assert (About (S1 * S1, Q, N_128));
      Lemma_About_Chain ((S2 * S2) * (S2 * S2), S1 * S1, Q, N_256, N_128, N_512);
      Lemma_About_Scaled ((S2 * S2) * (S2 * S2), Q, 8.0 * C, N_512);
      pragma Assert ((8.0 * C) * Q = V);
      pragma Assert ((8.0 * C) * ((S2 * S2) * (S2 * S2)) = 8.0 * C * ((S2 * S2) * (S2 * S2)));
   end Lemma_Real_Fourth;

   procedure Lemma_Crackle_Time (T, Vd, Cm : Raw) is
      V : constant Real := R (Vd);
      C : constant Real := R (Cm);

      F_E : constant Raw := 0.125 * Vd;
      F_Q : constant Raw := F_E / Cm;
      F_S : constant Raw := Dimensionless_Math.Sqrt (F_Q);
   begin
      Lemma_Domain_Constants;
      Lemma_Tiny_Constants;
      Lemma_Limit (Cm);
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Same (T, Dimensionless_Math.Sqrt (F_S));
      if Vd >= Tiny_Speed then
         Rounding.Lemma_Order (Tiny_Speed, Vd);
         Lemma_Near_Exact (0.125);
         Lemma_Near_Exact (Vd);
         Lemma_Near_Exact (Cm);
         Lemma_Near_Product (0.125, V, 0.125, Vd, 0.0, 0.0, N_1);
         Lemma_Quotient_Times_Divisor (0.125 * V, C);
         declare
            Q : constant Real := (0.125 * V) / C;
         begin
            pragma Assert (Two_M953 * C <= Q * C);
            Lemma_Product_Cancel (Two_M953, Q, C);
            Lemma_Near_Quotient (0.125 * V, C, Q, F_E, Cm, N_1, 0.0, N_2);
            Lemma_Root_Squared (Q, F_Q, N_2, N_128);
            pragma Assert (R (F_S) * R (F_S) >= Q - N_128 * Q);
            Lemma_Floor (R (F_S), Two_M954, Two_M478, Two_M956);
            Lemma_Near_Exact (F_S);
            Lemma_Root_Squared (R (F_S), F_S, 0.0, N_64);
            Lemma_Real_Fourth (C, V, Q, R (F_S), R (T));
         end;
      else
         Rounding.Lemma_Order (Vd, Tiny_Speed);
         Float_Lemmas.Lemma_Rounding_Error_Mul (0.125, Vd);
         Float_Lemmas.Lemma_Rounding_Error_Div (F_E, Cm);
         Lemma_Quotient_Times_Divisor (R (F_E), C);
         Lemma_Real_Tiny_Quotient (V, C, R (F_E), R (F_E) / C, R (F_Q));
         Rounding.Lemma_Order (F_Q, Tiny_Quotient);
         pragma Assert (R (Tiny_Root) * R (Tiny_Root) = Two_M788);
         Lemma_Square_Root_Is_Faithful (F_Q, 0.0, Tiny_Root);
         Rounding.Lemma_Order (F_S, Tiny_Root);
         pragma Assert (R (Tiny_Fourth) * R (Tiny_Fourth) = Two_M394);
         Lemma_Square_Root_Is_Faithful (F_S, 0.0, Tiny_Fourth);
         Rounding.Lemma_Order (T, Tiny_Fourth);
      end if;
   end Lemma_Crackle_Time;

   ------------------------------------------------------------------------------------------------------------------
   --  A solve: the facts as the solver keeps them
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_About_Floor (X, Y, U, Low : Real)
   with Pre => About (X, Y, U) and then U >= 0.0 and then U <= Relative_Max and then X >= 2.0 * Low, Post => Y >= Low
   is
   begin
      Lemma_Scale_Monotone (U, Relative_Max, Y);
   end Lemma_About_Floor;
   --  A value near one which is at least twice Low is at least Low.

   procedure Lemma_Speed_Weaken (X : Raw; V, A, Q, U, U2 : Real)
   with
     Pre  => Speed_Remainder_Fact (X, V, A, Q, U) and then U <= U2 and then V >= 0.0 and then A * Q >= 0.0,
     Post => Speed_Remainder_Fact (X, V, A, Q, U2)
   is
   begin
      Lemma_Scale_Monotone (U, U2, V);
      Lemma_Scale_Monotone (U, U2, A * Q);
   end Lemma_Speed_Weaken;

   procedure Lemma_First_Gap (P, Q1 : Raw)
   with Pre => Is_Subtrahend (P) and then Is_Subtrahend (Q1), Post => Gap_1_OK (P, Q1)
   is
   begin
      null;
   end Lemma_First_Gap;

   procedure Lemma_Second_Gap (P, Q1, Q2 : Raw)
   with
     Pre  => Is_Subtrahend (P) and then Is_Subtrahend (Q1) and then Is_Subtrahend (Q2) and then Gap_1_OK (P, Q1),
     Post => Gap_OK (P, Q1, Q2)
   is
   begin
      null;
   end Lemma_Second_Gap;

   procedure Lemma_Minus_Zero (X : Raw) with Post => X - 0.0 = X is
   begin
      null;
   end Lemma_Minus_Zero;

   procedure Lemma_Real_Scaled_Term (Base, Y, Yr, K, Slack, W : Real)
   with
     Pre  =>
       Base >= 0.0
       and then Y >= 0.0
       and then K >= 1.0
       and then Slack >= 0.0
       and then Slack <= N_16
       and then abs (Yr - K * Y) <= Slack * (K * Y) + Residue
       and then Base + K * Y >= Two_M190
       and then W >= Slack + N_1,
     Post => About (Base + Yr + 0.0, Base + K * Y, W) and then Base + Yr + 0.0 >= Span_Sum_Floor
   is
      Q2 : constant Real := Base + K * Y;
   begin
      pragma Assert (Residue <= N_1 * Q2);
      Lemma_Scale_Monotone (K * Y, Q2, Slack);
      pragma Assert (abs (Base + Yr - Q2) <= Slack * Q2 + N_1 * Q2);
      Lemma_Scale_Monotone (Slack + N_1, W, Q2);
      pragma Assert ((Slack + N_1) * Q2 = Slack * Q2 + N_1 * Q2);
      pragma Assert (Base + Yr >= Q2 - N_32 * Q2);
   end Lemma_Real_Scaled_Term;
   --  A sum whose second term is the rounding of K times Y is near the exact sum, and is not tiny.

   procedure Lemma_Time_Top (Y : Raw) with Pre => Y <= 2#1.0#E163, Post => R (Y) <= Two_P163 is
   begin
      null;
   end Lemma_Time_Top;

   procedure Lemma_Raw_Last_Time_Twice (T4, P, Vd, Am, Q1, Y : Raw)
   with
     Pre  =>
       Vd >= 0.0
       and then Vd <= Speed_Top
       and then In_Domain (Am)
       and then P = Vd / Am
       and then Is_Subtrahend (P)
       and then Is_Subtrahend (Q1)
       and then Y >= 0.0
       and then Y <= Doubled_Top
       and then Gap_1_OK (P, Q1)
       and then R (Q1) + 2.0 * R (Y) >= Two_M190
       and then T4 = P - Q1 - 2.0 * Y,
     Post => Speed_Remainder_Fact (T4, R (Vd), R (Am), R (Q1) + 2.0 * R (Y), N_64)
   is
      Twice : constant Raw := 2.0 * Y;
   begin
      Lemma_Domain_Constants;
      Lemma_Limit (Am);
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Nonnegative (Q1);
      Rounding.Lemma_Nonnegative (Y);
      Rounding.Lemma_Order (Y, Doubled_Top);
      Lemma_Almost_Double (Y);
      Lemma_Real_Scaled_Term (R (Q1), R (Y), R (Twice), 2.0, N_8, N_10);
      pragma Assert (R (Twice) <= Two_P200);
      Rounding.Lemma_Order (Twice, Operand_Top);
      Lemma_Second_Gap (P, Q1, Twice);
      Lemma_Minus_Zero (P - Q1 - Twice);
      Lemma_Speed_Gap (T4, P, Vd, Am, Q1, Twice, 0.0);
      Lemma_Speed_Rebase (T4, R (Vd), R (Am), R (Q1) + R (Twice) + R (0.0), R (Q1) + 2.0 * R (Y), N_16, N_10, N_64);
   end Lemma_Raw_Last_Time_Twice;
   --  The last phase time of Region_2 and Region_4: the speed time less one time and twice another.

   procedure Lemma_Raw_Last_Time_Four (T4, P, Vd, Am, Y : Raw)
   with
     Pre  =>
       Vd >= 0.0
       and then Vd <= Speed_Top
       and then In_Domain (Am)
       and then P = Vd / Am
       and then Is_Subtrahend (P)
       and then Y >= 0.0
       and then Y <= 2#1.0#E163
       and then R (Y) >= Two_M190
       and then T4 = P - 4.0 * Y,
     Post => Speed_Remainder_Fact (T4, R (Vd), R (Am), 4.0 * R (Y), N_64)
   is
      Four : constant Raw := 4.0 * Y;
   begin
      Lemma_Domain_Constants;
      Lemma_Limit (Am);
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Nonnegative (Y);
      Lemma_Time_Top (Y);
      Lemma_Almost_Quadruple (Y);
      Lemma_Real_Scaled_Term (0.0, R (Y), R (Four), 4.0, N_16, N_20);
      pragma Assert (R (Four) <= Two_P200);
      Rounding.Lemma_Order (Four, Operand_Top);
      Lemma_First_Gap (P, Four);
      Lemma_Second_Gap (P, Four, 0.0);
      Lemma_Minus_Zero (P - Four);
      Lemma_Minus_Zero (P - Four - 0.0);
      Lemma_Speed_Gap (T4, P, Vd, Am, Four, 0.0, 0.0);
      pragma Assert (R (Four) + R (0.0) + R (0.0) = 0.0 + R (Four) + 0.0);
      pragma Assert (0.0 + 4.0 * R (Y) = 4.0 * R (Y));
      Lemma_Speed_Rebase (T4, R (Vd), R (Am), R (Four) + R (0.0) + R (0.0), 4.0 * R (Y), N_16, N_20, N_64);
   end Lemma_Raw_Last_Time_Four;
   --  The last phase time of Region_3 and Region_5.

   procedure Lemma_Solver_Reals (Solver : Delta_V_Solver)
   with
     Pre  => Solver_In_Domain (Solver),
     Post =>
       R (Solver.Am) >= Two_M80
       and then R (Solver.Am) <= Two_P80
       and then R (Solver.Jm) >= Two_M80
       and then R (Solver.Jm) <= Two_P80
       and then R (Solver.Cm) >= Two_M80
       and then R (Solver.Cm) <= Two_P80
       and then R (Solver.T1) >= 0.0
       and then R (Solver.T2) >= 0.0
       and then R (Solver.T3) >= 0.0
       and then R (Solver.Accel_Over_Jerk) >= 0.0
       and then R (Solver.Jerk_Over_Snap) >= 0.0
       and then R (Solver.Snap_Over_Crackle) >= 0.0
       and then R (Solver.Snap_Root) >= 0.0
       and then R (Solver.Jerk_Over_Crackle) >= 0.0
       and then R (Solver.Snap_Only_Above) >= 0.0
       and then In_Domain (Solver.Am)
       and then In_Domain (Solver.Jm)
       and then In_Domain (Solver.Cm)
   is
   begin
      Lemma_Limit (Solver.Am);
      Lemma_Limit (Solver.Jm);
      Lemma_Limit (Solver.Cm);
      Rounding.Lemma_Nonnegative (Solver.T1);
      Rounding.Lemma_Nonnegative (Solver.T2);
      Rounding.Lemma_Nonnegative (Solver.T3);
      Rounding.Lemma_Nonnegative (Solver.Accel_Over_Jerk);
      Rounding.Lemma_Nonnegative (Solver.Jerk_Over_Snap);
      Rounding.Lemma_Nonnegative (Solver.Snap_Over_Crackle);
      Rounding.Lemma_Nonnegative (Solver.Snap_Root);
      Rounding.Lemma_Nonnegative (Solver.Jerk_Over_Crackle);
      Rounding.Lemma_Nonnegative (Solver.Snap_Only_Above);
   end Lemma_Solver_Reals;
   --  The reals which the fields of a solver within its domain denote.

   procedure Lemma_Last_Time_1 (T4, P : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      A1 : constant Real := R (Solver.T1);
      Cs : constant Real := Acceleration_Span (Solver.T1, Solver.T2, Solver.T3);
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      pragma Assert (Cs >= 2.0 * A1);
      Lemma_About_Floor (Cs, R (Solver.Accel_Over_Jerk), Region_Tolerance, Two_M170);
      Lemma_Speed_Gap (T4, P, Vd, Solver.Am, Solver.Accel_Over_Jerk, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle);
      Lemma_Product_Monotone (0.0, R (Solver.Am), 0.0, Full_Span (Solver));
      Rounding.Lemma_Nonnegative (Vd);
      Lemma_Speed_Weaken (T4, R (Vd), R (Solver.Am), Full_Span (Solver), N_16, N_64);
   end Lemma_Last_Time_1;

   procedure Lemma_Last_Time_2 (T4, P : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      A1 : constant Real := R (Solver.T1);
      Bs : constant Real := Jerk_Span (Solver.T1, Solver.T2);
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      pragma Assert (2.0 * (A1 + Bs) >= 4.0 * A1);
      Lemma_About_Floor (2.0 * (A1 + Bs), Full_Span (Solver), Region_Tolerance, Two_M190);
      Lemma_Raw_Last_Time_Twice (T4, P, Vd, Solver.Am, Solver.Snap_Over_Crackle, Solver.Snap_Root);
   end Lemma_Last_Time_2;

   procedure Lemma_Last_Time_3 (T4, P : Time; Vd : Velocity; Solver : Delta_V_Solver) is
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Lemma_Raw_Last_Time_Four (T4, P, Vd, Solver.Am, Solver.T1);
   end Lemma_Last_Time_3;

   procedure Lemma_Last_Time_4 (T4, P : Time; Vd : Velocity; Solver : Delta_V_Solver) is
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Lemma_Raw_Last_Time_Twice (T4, P, Vd, Solver.Am, Solver.Accel_Over_Jerk, Solver.T1);
   end Lemma_Last_Time_4;

   procedure Lemma_Jerk_Limited_1 (T3 : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      A1 : constant Real := R (Solver.T1);
      Bs : constant Real := Jerk_Span (Solver.T1, Solver.T2);
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      pragma Assert (A1 + Bs >= 2.0 * A1);
      Lemma_About_Floor
        (A1 + Bs, R (Solver.Jerk_Over_Snap) + R (Solver.Snap_Over_Crackle), Region_Tolerance, Span_Sum_Floor);
      Lemma_Jerk_Time (T3, Vd, Solver.Jm, Solver.Jerk_Over_Snap, Solver.Snap_Over_Crackle);
   end Lemma_Jerk_Limited_1;

   procedure Lemma_Jerk_Limited_4 (T3 : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      A1 : constant Real := R (Solver.T1);
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Lemma_Product_Monotone (Two_M170, A1, Two_M170, A1);
      pragma Assert (A1 * A1 >= Two_M340);
      Lemma_About_Floor (A1 * A1, R (Solver.Jerk_Over_Crackle), Region_Tolerance, Radicand_Floor);
      Lemma_Jerk_Only_Time (T3, Vd, Solver.Jm, Solver.Jerk_Over_Crackle, Solver.T1);
   end Lemma_Jerk_Limited_4;

   procedure Lemma_Normal_Constants
   with
     Post =>
       R (Normal_Time_Min) = Two_M170
       and then R (Phase_Time_Max) >= Two_P163
       and then R (No_Time) = 0.0
       and then R (No_Speed) = 0.0
       and then R (No_Velocity) = 0.0
   is
   begin
      null;
   end Lemma_Normal_Constants;

   procedure Lemma_Real_Gain_Below (Exact, F, V : Real)
   with
     Pre  => Exact >= 0.0 and then abs (F - Exact) <= Gain_Slack * Exact and then F <= V,
     Post => At_Most (Exact, V, N_64)
   is
   begin
      pragma Assert (Exact - Gain_Slack * Exact <= V);
      pragma Assert (V >= 0.0);
      pragma Assert ((1.0 + N_64) * (Exact - Gain_Slack * Exact) >= Exact);
   end Lemma_Real_Gain_Below;

   procedure Lemma_Snap_Gain_Identity (C, A1, X : Real)
   with Post => Exact_Gain (C, A1, X, 0.0, 0.0) = 2.0 * (C * A1) * (A1 + X) * ((A1 + (A1 + X)) * (A1 + (A1 + X)))
   is
   begin
      null;
   end Lemma_Snap_Gain_Identity;

   procedure Lemma_Real_Next_Gain (Sp, A1, X, N, F, V : Real)
   with
     Pre  =>
       Sp >= 0.0
       and then A1 >= Two_M170
       and then X >= 0.0
       and then N >= 0.0
       and then N <= X + Next_Slack * X + Next_Residue
       and then V >= 0.0
       and then F > V
       and then
         abs (F - 2.0 * Sp * (A1 + N) * ((A1 + (A1 + N)) * (A1 + (A1 + N))))
         <= Gain_Slack * (2.0 * Sp * (A1 + N) * ((A1 + (A1 + N)) * (A1 + (A1 + N)))),
     Post => 2.0 * Sp * (A1 + X) * ((A1 + (A1 + X)) * (A1 + (A1 + X))) >= V - Bisected_Slack * V
   is
      B  : constant Real := A1 + X;
      E  : constant Real := A1 + B;
      BN : constant Real := A1 + N;
      EN : constant Real := A1 + BN;
      G  : constant Real := 2.0 * Sp * B * (E * E);
      GN : constant Real := 2.0 * Sp * BN * (EN * EN);
   begin
      --  The next machine number lengthens each span by no more than one part in 2**50.
      pragma Assert (Next_Residue <= Two_M52 * A1);
      pragma Assert (BN <= B + Two_M50 * B);
      pragma Assert (EN <= E + Two_M50 * E);
      Lemma_Product_Monotone (0.0, EN, 0.0, EN);
      Lemma_Product_Monotone (EN, E + Two_M50 * E, EN, E + Two_M50 * E);
      Lemma_Product_Monotone (BN, B + Two_M50 * B, EN * EN, (E + Two_M50 * E) * (E + Two_M50 * E));
      pragma
        Assert
          ((B + Two_M50 * B) * ((E + Two_M50 * E) * (E + Two_M50 * E))
           = (1.0 + Two_M50) * (1.0 + Two_M50) * (1.0 + Two_M50) * (B * (E * E)));
      Lemma_Product_Monotone (0.0, E, 0.0, E);
      Lemma_Product_Monotone (0.0, B, 0.0, E * E);
      Lemma_Scale_Monotone ((1.0 + Two_M50) * (1.0 + Two_M50) * (1.0 + Two_M50), 1.0 + Two_M48, B * (E * E));
      pragma Assert (BN * (EN * EN) <= (1.0 + Two_M48) * (B * (E * E)));
      Lemma_Scale_Monotone (BN * (EN * EN), (1.0 + Two_M48) * (B * (E * E)), 2.0 * Sp);
      pragma Assert (GN <= G + Two_M48 * G);
      Lemma_Product_Monotone (0.0, 2.0 * Sp, 0.0, B * (E * E));
      pragma Assert (G >= 0.0);

      --  So the ramp found gains nearly what its successor does, which is more than the speed change.
      pragma Assert (V < GN + Gain_Slack * GN);
      pragma Assert (V <= (1.0 + Gain_Slack) * (1.0 + Two_M48) * G);
      pragma Assert (V <= G + Bisected_Slack * G);
      if G <= V then
         pragma Assert (G >= V - Bisected_Slack * V);
      end if;
   end Lemma_Real_Next_Gain;
   --  Sp is the snap peak, A1 the first phase time, X the second phase time found, N the machine number after
   --  it and F the gain computed with N in its place, which exceeds the speed change V.

   procedure Lemma_Next_Gain (T1, Start, Next, T3, T4 : Time; Vd : Velocity; Solver : Delta_V_Solver)
   with
     Pre  =>
       Speed_OK (Vd, Solver)
       and then Solver_Is_Consistent (Solver)
       and then T1 = Solver.T1
       and then T1 <= Phase_Time_Max
       and then T3 = No_Time
       and then T4 = No_Time
       and then Start >= No_Time
       and then Start <= Search_Time_Cap
       and then To_Bits (Start) < Last_Pattern
       and then Next = To_Time (To_Bits (Start) + 1)
       and then Next <= Search_Time_Cap
       and then Velocity_Formula (T1, Next, T3, T4, Solver.Cm, No_Speed) > Vd,
     Post => Exact_Gain (R (Solver.Cm), R (Solver.T1), R (Start), 0.0, 0.0) >= R (Vd) - Bisected_Slack * R (Vd)
   is
      C  : constant Real := R (Solver.Cm);
      A1 : constant Real := R (Solver.T1);
      X  : constant Real := R (Start);
      N  : constant Real := R (Next);
      F  : constant Velocity := Velocity_Formula (T1, Next, T3, T4, Solver.Cm, No_Speed);
   begin
      Lemma_Solver_Reals (Solver);
      Lemma_Normal_Constants;
      Rounding.Lemma_Same (T1, Solver.T1);
      Lemma_No_Time (T3);
      Lemma_No_Time (T4);
      Rounding.Lemma_Nonnegative (Start);
      Rounding.Lemma_Nonnegative (Next);
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Order (Normal_Time_Min, T1);
      pragma Assert (Snap_Peak (Solver.Cm, Solver.T1) = C * A1);
      pragma Assert (C * R (T1) <= Normal_Snap_Max);

      --  The machine number after the phase time found is close to it.
      Lemma_Next_Is_Close (Start);
      Machine_Numbers.Lemma_Order (Next, To_Real (To_Bits (Start) + 1));
      Rounding.Lemma_Same (Next, To_Real (To_Bits (Start) + 1));

      --  The gain computed with it is close to the exact one, and exceeds the speed change.
      pragma Assert (Gain_Is_Normal (T1, Next, T3, T4, Solver.Cm));
      Lemma_Gain_Is_Near (T1, Next, T3, T4, Solver.Cm);
      Rounding.Lemma_Order (F, Vd);
      pragma Assert (R (F) > R (Vd));
      Lemma_Snap_Gain_Identity (C, A1, N);
      Lemma_Snap_Gain_Identity (C, A1, X);
      pragma Assert (Exact_Gain (C, R (T1), N, R (T3), R (T4)) = Exact_Gain (C, A1, N, 0.0, 0.0));
      Lemma_Product_Monotone (0.0, C, 0.0, A1);
      Lemma_Real_Next_Gain (C * A1, A1, X, N, R (F), R (Vd));
   end Lemma_Next_Gain;
   --  The half of the certificate of the bisection which says that the phase time found is not too short.

   procedure Lemma_Snap_Gain (Times : Feedrate_Profile_Times; Vd : Velocity; Solver : Delta_V_Solver) is
      C  : constant Real := R (Solver.Cm);
      A1 : constant Real := R (Solver.T1);
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Lemma_Normal_Constants;
      Rounding.Lemma_Same (Times (1), Solver.T1);
      Lemma_No_Time (Times (3));
      Lemma_No_Time (Times (4));
      if Times (2) > No_Time then
         if not Is_Capped (Times (2)) then
            Lemma_Next_Gain (Times (1), Times (2), Next_Time (Times (2)), Times (3), Times (4), Vd, Solver);
         end if;
      else
         --  The machine number after zero is the smallest one, whatever the sign of the zero found.
         Lemma_Zero_Pattern;
         Lemma_No_Time (Times (2));
         Lemma_No_Time (No_Time);
         Lemma_Next_Gain (Times (1), No_Time, Smallest_Time, Times (3), Times (4), Vd, Solver);
      end if;
      if Times (2) = No_Time then
         Lemma_No_Time (Times (2));
      else
         Rounding.Lemma_Order (Normal_Time_Min, Times (1));
         pragma Assert (C * R (Times (1)) <= Normal_Snap_Max);
         pragma Assert (Gain_Is_Normal (Times (1), Times (2), Times (3), Times (4), Solver.Cm));
         Lemma_Gain_Is_Near (Times (1), Times (2), Times (3), Times (4), Solver.Cm);
         Rounding.Lemma_Order
           (Velocity_Formula (Times (1), Times (2), Times (3), Times (4), Solver.Cm, No_Velocity), Vd);
         Lemma_Real_Gain_Below
           (Exact_Gain (C, R (Times (1)), R (Times (2)), R (Times (3)), R (Times (4))),
            R (Velocity_Formula (Times (1), Times (2), Times (3), Times (4), Solver.Cm, No_Velocity)),
            R (Vd));
         pragma
           Assert
             (Exact_Gain (C, R (Times (1)), R (Times (2)), R (Times (3)), R (Times (4)))
              = Exact_Gain (C, A1, R (Times (2)), 0.0, 0.0));
      end if;
   end Lemma_Snap_Gain;

   ------------------------------------------------------------------------------------------------------------------
   --  A solve: what each case concludes, in real arithmetic
   --
   --  A1, Bs and Cs are the spans of the saturated ramp, Sp, JP and AP its peaks, and J, A and V the jerk and
   --  acceleration limits and the speed change.
   ------------------------------------------------------------------------------------------------------------------

   R_39 : constant := 2.0**(-39);
   --  More than Region_Tolerance and a second-order term: the bound of a kept value taken the other way round.

   R_38 : constant := 2.0**(-38);
   --  More than two such bounds.

   T_A : constant := 2.0**(-36);
   --  The bound of the speed gained in a jerk-limited or snap-limited case, against the speed change.

   T_P : constant := 2.0**(-33) + R_39;
   --  More than Peak_Tolerance and a second-order term.

   T_C : constant := 2.0**(-33) + 2.0**(-35);
   --  The bound of a span where the acceleration limit is what bounds it.

   T_35 : constant := 2.0**(-35);
   T_P2 : constant := 2.0**(-33) + R_38;
   T_P3 : constant := 2.0**(-33) + 2.0**(-37);
   T_P4 : constant := 2.0**(-33) + 2.0**(-37) + 2.0**(-38);
   T_Q  : constant := 2.0**(-33) + 2.0**(-38) + 2.0**(-40);
   --  Steps on the way to T_C.

   L_1 : constant := 2.0**(-30);
   L_2 : constant := 2.0**(-30) + 2.0**(-31);
   L_3 : constant := 2.0**(-29);
   --  The bounds of the three peaks of a solved ramp against the limits.

   procedure Lemma_Solve_Cubic (X, Y, G, U : Real)
   with
     Pre  =>
       X >= 0.0
       and then Y >= 0.0
       and then G >= 0.0
       and then U >= 0.0
       and then At_Most (X * ((G + X) * (G + X)), Y * ((G + Y) * (G + Y)), U),
     Post => At_Most (X, Y, U)
   is
      Z : constant Real := Y + U * Y;
   begin
      Lemma_Scale_Monotone (0.0, U, Y);
      if X > Z then
         Lemma_Product_Monotone (G + Y, G + X, G + Y, G + X);
         pragma Assert ((G + X) * (G + X) > 0.0);
         Lemma_Strict_Scale (Z, X, (G + X) * (G + X));
         Lemma_Product_Monotone (0.0, G + Y, 0.0, G + Y);
         Lemma_Scale_Monotone ((G + Y) * (G + Y), (G + X) * (G + X), Z);
         pragma Assert (Z * ((G + Y) * (G + Y)) = Y * ((G + Y) * (G + Y)) + U * (Y * ((G + Y) * (G + Y))));
         pragma Assert (False);
      end if;
   end Lemma_Solve_Cubic;
   --  The product of a value and the square of its sum with G grows with the value.

   procedure Lemma_Real_Full (AP, A, D0, Q, T4, V : Real)
   with
     Pre  =>
       A >= 0.0
       and then V >= 0.0
       and then D0 >= 0.0
       and then T4 >= 0.0
       and then About (AP, A, Peak_Tolerance)
       and then About (D0, Q, Region_Tolerance)
       and then abs (T4 * A - (V - A * Q)) <= N_64 * V
       and then A * Q <= V + N_64 * V,
     Post => AP * (D0 + T4) <= V + Solved_Tolerance * V and then AP * (D0 + T4) >= V - Solved_Tolerance * V
   is
      X : constant Real := (D0 + T4) * A;
   begin
      --  No less than the speed change.
      pragma Assert (D0 >= Q - Region_Tolerance * Q);
      Lemma_Scale_Monotone (Q - Region_Tolerance * Q, D0, A);
      pragma Assert (D0 * A >= A * Q - Region_Tolerance * (A * Q));
      pragma Assert (X = D0 * A + T4 * A);
      pragma Assert (X >= V - N_64 * V - Region_Tolerance * (V + N_64 * V));
      pragma Assert (AP >= A - Peak_Tolerance * A);
      Lemma_Scale_Monotone (A - Peak_Tolerance * A, AP, D0 + T4);
      pragma Assert ((A - Peak_Tolerance * A) * (D0 + T4) = X - Peak_Tolerance * X);
      pragma
        Assert
          ((1.0 - Peak_Tolerance) * (V - N_64 * V - Region_Tolerance * (V + N_64 * V)) >= V - Solved_Tolerance * V);
      pragma Assert (AP * (D0 + T4) >= V - Solved_Tolerance * V);

      --  And no more.
      pragma Assert (D0 <= Q + Region_Tolerance * Q);
      Lemma_Scale_Monotone (D0, Q + Region_Tolerance * Q, A);
      pragma Assert (D0 * A <= A * Q + Region_Tolerance * (A * Q));
      pragma Assert (A * Q >= 0.0);
      pragma Assert ((D0 + T4) * A = D0 * A + T4 * A);
      pragma Assert ((D0 + T4) * A <= V + N_64 * V + Region_Tolerance * (V + N_64 * V));
      pragma Assert (AP <= A + Peak_Tolerance * A);
      Lemma_Scale_Monotone (AP, A + Peak_Tolerance * A, D0 + T4);
      pragma Assert ((A + Peak_Tolerance * A) * (D0 + T4) = (1.0 + Peak_Tolerance) * ((D0 + T4) * A));
      pragma
        Assert
          ((1.0 + Peak_Tolerance) * (V + N_64 * V + Region_Tolerance * (V + N_64 * V)) <= V + Solved_Tolerance * V);
   end Lemma_Real_Full;
   --  A full case: the last span times the acceleration peak is the speed change.

   procedure Lemma_Real_Jerk_Tail (J, A, V, G0, H0, Cs, AJr, JP, C : Real)
   with
     Pre  =>
       J > 0.0
       and then A >= 0.0
       and then V >= 0.0
       and then G0 >= 0.0
       and then H0 >= 0.0
       and then Cs >= 0.0
       and then AJr >= 0.0
       and then JP >= 0.0
       and then C >= 0.0
       and then About (C * (H0 + C) * J, V, T_A)
       and then At_Most (V, A * (AJr + G0), N_64)
       and then About (JP * Cs, A, Peak_Tolerance)
       and then About (JP, J, Region_Tolerance)
       and then About (Cs, AJr, Region_Tolerance)
       and then About (H0, G0, Region_Tolerance),
     Post =>
       At_Most (C, Cs, T_C)
       and then JP * (C * (H0 + C)) <= V + Solved_Tolerance * V
       and then JP * (C * (H0 + C)) >= V - Solved_Tolerance * V
   is
      CD : constant Real := C * (H0 + C);
   begin
      Lemma_Product_Monotone (0.0, C, 0.0, H0 + C);
      Lemma_Product_Monotone (0.0, J, 0.0, Cs);
      Lemma_Product_Monotone (0.0, J * Cs, 0.0, Cs + H0);
      Lemma_Product_Monotone (0.0, Cs, 0.0, H0 + Cs);

      --  The speed gained.
      Lemma_About_Scaled (JP, J, CD, Region_Tolerance);
      pragma Assert (CD * J = C * (H0 + C) * J);
      Lemma_About_Chain (CD * JP, CD * J, V, Region_Tolerance, T_A, T_35);
      Lemma_About_Bounds (CD * JP, V, T_35);
      pragma Assert (JP * CD <= V + Solved_Tolerance * V);
      pragma Assert (JP * CD >= V - Solved_Tolerance * V);

      --  The acceleration limit is the jerk limit times the last span of the saturated ramp.
      Lemma_About_Symmetric (JP * Cs, A, Peak_Tolerance, T_P);
      Lemma_At_Most_From_About (A, JP * Cs, T_P);
      Lemma_At_Most_From_About (JP, J, Region_Tolerance);
      Lemma_At_Most_Scaled (JP, J, Cs, Region_Tolerance);
      pragma Assert (Cs * JP = JP * Cs and then Cs * J = J * Cs);
      Lemma_At_Most_Chain (A, JP * Cs, J * Cs, T_P, Region_Tolerance, T_P2);

      --  And the time which the speed change is compared with is the last two spans.
      Lemma_About_Symmetric (Cs, AJr, Region_Tolerance, R_39);
      Lemma_At_Most_From_About (AJr, Cs, R_39);
      Lemma_About_Symmetric (H0, G0, Region_Tolerance, R_39);
      Lemma_At_Most_From_About (G0, H0, R_39);
      Lemma_At_Most_Sum (AJr, Cs, G0, H0, R_39, R_39, R_39);
      Lemma_Product_Monotone (0.0, A, 0.0, AJr + G0);
      Lemma_At_Most_Product (A, J * Cs, AJr + G0, Cs + H0, T_P2, R_39, T_P3);
      Lemma_At_Most_Chain (V, A * (AJr + G0), (J * Cs) * (Cs + H0), N_64, T_P3, T_P4);
      Lemma_At_Most_From_About (CD * J, V, T_A);
      Lemma_At_Most_Chain (CD * J, V, (J * Cs) * (Cs + H0), T_A, T_P4, T_C);
      pragma Assert (CD * J = J * (C * (H0 + C)));
      pragma Assert ((J * Cs) * (Cs + H0) = J * (Cs * (H0 + Cs)));
      Lemma_At_Most_Cancel (C * (H0 + C), Cs * (H0 + Cs), J, T_C);
      Lemma_Solve_Product_Sum (C, Cs, H0, T_C);
   end Lemma_Real_Jerk_Tail;
   --  What the two jerk-limited cases share. C is the last span of the result but one, C + H0 the last, and their
   --  product times the jerk limit is the speed change; the speed change is at most what the acceleration limit
   --  allows, so C is at most the span Cs of the saturated ramp.

   procedure Lemma_Real_Jerk_Limited (J, V, G, H, T3 : Real)
   with
     Pre  =>
       J >= 0.0
       and then V >= 0.0
       and then G >= 0.0
       and then H >= 0.0
       and then T3 >= 0.0
       and then About (H, G, Region_Tolerance)
       and then About ((2.0 * T3 + 3.0 * G) * (2.0 * T3 + 3.0 * G) * J, G * G * J + 4.0 * V, N_1024),
     Post => About ((H + T3) * (H + (H + T3)) * J, V, T_A)
   is
      E  : constant Real := (T3 + G) * (T3 + 2.0 * G);
      GJ : constant Real := G * G * J;
   begin
      Lemma_Product_Monotone (0.0, G, 0.0, G);
      Lemma_Product_Monotone (0.0, G * G, 0.0, J);
      Lemma_Product_Monotone (0.0, T3 + G, 0.0, T3 + 2.0 * G);
      pragma Assert ((2.0 * T3 + 3.0 * G) * (2.0 * T3 + 3.0 * G) - G * G = 4.0 * E);
      pragma Assert ((2.0 * T3 + 3.0 * G) * (2.0 * T3 + 3.0 * G) * J - GJ = 4.0 * (E * J));
      pragma Assert (abs (4.0 * (E * J) - 4.0 * V) <= N_1024 * (GJ + 4.0 * V));
      --  The base is small beside the speed change: the product is at least twice its square.
      Lemma_Product_Monotone (G, T3 + G, 2.0 * G, T3 + 2.0 * G);
      pragma Assert (E >= 2.0 * (G * G));
      Lemma_Scale_Monotone (2.0 * (G * G), E, J);
      pragma Assert (E * J >= 2.0 * GJ);
      pragma Assert (GJ <= V);
      pragma Assert (abs (E * J - V) <= N_2048 * V);
      pragma Assert (About (E * J, V, N_2048));

      Lemma_About_Same (T3);
      Lemma_About_Sum (H, G, T3, T3, Region_Tolerance, 0.0, Region_Tolerance);
      Lemma_About_Scaled (H, G, 2.0, Region_Tolerance);
      Lemma_About_Sum (2.0 * H, 2.0 * G, T3, T3, Region_Tolerance, 0.0, Region_Tolerance);
      pragma Assert (H + (H + T3) = 2.0 * H + T3);
      pragma Assert (H + T3 = T3 + H and then G + T3 = T3 + G);
      Lemma_About_Product (H + T3, G + T3, 2.0 * H + T3, 2.0 * G + T3, Region_Tolerance, Region_Tolerance, R_38);
      pragma Assert ((G + T3) * (2.0 * G + T3) = E);
      Lemma_About_Scaled ((H + T3) * (2.0 * H + T3), E, J, R_38);
      Lemma_About_Chain (J * ((H + T3) * (2.0 * H + T3)), J * E, V, R_38, N_2048, T_A);
      pragma Assert (J * ((H + T3) * (2.0 * H + T3)) = (H + T3) * (H + (H + T3)) * J);
   end Lemma_Real_Jerk_Limited;
   --  Region_1: the last two spans, times the jerk limit, are the speed change.

   procedure Lemma_Real_Jerk_Only (J, V, Gam, T, T3 : Real)
   with
     Pre  =>
       J >= 0.0
       and then V >= 0.0
       and then Gam >= 0.0
       and then T >= 0.0
       and then T3 >= 0.0
       and then About (T * T, Gam, Region_Tolerance)
       and then About ((T3 + 3.0 * T) * (T3 + 3.0 * T) * J, Gam * J + V, N_1024),
     Post => About ((2.0 * T + T3) * (2.0 * T + (2.0 * T + T3)) * J, V, T_A)
   is
      CD : constant Real := (2.0 * T + T3) * (4.0 * T + T3);
      TJ : constant Real := T * T * J;
   begin
      Lemma_Product_Monotone (0.0, T, 0.0, T);
      Lemma_Product_Monotone (0.0, T * T, 0.0, J);
      Lemma_Product_Monotone (0.0, Gam, 0.0, J);
      pragma Assert ((T3 + 3.0 * T) * (T3 + 3.0 * T) - T * T = CD);
      pragma Assert ((T3 + 3.0 * T) * (T3 + 3.0 * T) * J - TJ = CD * J);
      Lemma_About_Scaled (T * T, Gam, J, Region_Tolerance);
      pragma Assert (abs (TJ - Gam * J) <= Region_Tolerance * (Gam * J));
      pragma Assert (abs (CD * J - V) <= N_1024 * (Gam * J + V) + Region_Tolerance * (Gam * J));
      Lemma_Product_Monotone (2.0 * T, 2.0 * T + T3, 4.0 * T, 4.0 * T + T3);
      pragma Assert (CD >= 8.0 * (T * T));
      Lemma_Scale_Monotone (8.0 * (T * T), CD, J);
      pragma Assert (CD * J >= 8.0 * TJ);
      pragma Assert (Gam * J <= 1.01 * TJ);
      pragma Assert (Gam * J <= V);
      pragma Assert (abs (CD * J - V) <= T_A * V);
      pragma Assert (2.0 * T + (2.0 * T + T3) = 4.0 * T + T3);
   end Lemma_Real_Jerk_Only;
   --  Region_4: the same.

   procedure Lemma_Real_Snap_Span (Sp, A1, B, Bs, U : Real)
   with
     Pre  =>
       Sp > 0.0
       and then A1 >= 0.0
       and then B >= 0.0
       and then Bs >= 0.0
       and then U >= 0.0
       and then At_Most (2.0 * Sp * B * ((A1 + B) * (A1 + B)), 2.0 * Sp * Bs * ((A1 + Bs) * (A1 + Bs)), U),
     Post => At_Most (B, Bs, U) and then At_Most (A1 + B, A1 + Bs, U)
   is
   begin
      Lemma_Product_Monotone (0.0, A1 + Bs, 0.0, A1 + Bs);
      Lemma_Product_Monotone (0.0, Bs, 0.0, (A1 + Bs) * (A1 + Bs));
      pragma Assert (2.0 * Sp * B * ((A1 + B) * (A1 + B)) = (2.0 * Sp) * (B * ((A1 + B) * (A1 + B))));
      pragma Assert (2.0 * Sp * Bs * ((A1 + Bs) * (A1 + Bs)) = (2.0 * Sp) * (Bs * ((A1 + Bs) * (A1 + Bs))));
      Lemma_At_Most_Cancel (B * ((A1 + B) * (A1 + B)), Bs * ((A1 + Bs) * (A1 + Bs)), 2.0 * Sp, U);
      Lemma_Solve_Cubic (B, Bs, A1, U);
      Lemma_Scale_Monotone (0.0, U, A1);
   end Lemma_Real_Snap_Span;
   --  A snap-limited case which gains no more than the saturated ramp cut after its second phase has no longer
   --  a second span.

   procedure Lemma_Real_Fourth_Span (C, T, A1, V, Bound, U, W : Real)
   with
     Pre  =>
       C > 0.0
       and then T >= 0.0
       and then A1 >= 0.0
       and then About (8.0 * C * ((T * T) * (T * T)), V, N_512)
       and then At_Most (V, Bound, U)
       and then Bound = 8.0 * C * ((A1 * A1) * (A1 * A1))
       and then U >= 0.0
       and then U <= Relative_Max
       and then W >= U + N_1024,
     Post => At_Most (T, A1, W)
   is
   begin
      Lemma_Product_Monotone (0.0, A1, 0.0, A1);
      Lemma_Product_Monotone (0.0, A1 * A1, 0.0, A1 * A1);
      Lemma_At_Most_From_About (8.0 * C * ((T * T) * (T * T)), V, N_512);
      Lemma_At_Most_Chain (8.0 * C * ((T * T) * (T * T)), V, Bound, N_512, U, W);
      pragma Assert (8.0 * C * ((T * T) * (T * T)) = (8.0 * C) * ((T * T) * (T * T)));
      pragma Assert (Bound = (8.0 * C) * ((A1 * A1) * (A1 * A1)));
      Lemma_At_Most_Cancel ((T * T) * (T * T), (A1 * A1) * (A1 * A1), 8.0 * C, W);
      Lemma_At_Most_Fourth_Root (T, A1, W);
   end Lemma_Real_Fourth_Span;
   --  A crackle-limited case whose speed change is at most what four equal phases of A1 gain has a phase time of
   --  at most A1.

   procedure Lemma_Real_Limits (C, S, J, A, T1, B, Cc, A1, Bs, Cs : Real)
   with
     Pre  =>
       C >= 0.0
       and then S >= 0.0
       and then J >= 0.0
       and then A >= 0.0
       and then T1 >= 0.0
       and then B >= 0.0
       and then Cc >= 0.0
       and then A1 >= 0.0
       and then Bs >= 0.0
       and then Cs >= 0.0
       and then At_Most (T1, A1, Solved_Tolerance)
       and then At_Most (B, Bs, Solved_Tolerance)
       and then At_Most (Cc, Cs, Solved_Tolerance)
       and then At_Most (C * A1, S, Prepared_Tolerance)
       and then At_Most (C * A1 * Bs, J, Prepared_Tolerance)
       and then About (C * A1 * Bs * Cs, A, Peak_Tolerance),
     Post =>
       C * T1 <= S * (1.0 + L_3) and then C * T1 * B <= J * (1.0 + L_3) and then C * T1 * B * Cc <= A * (1.0 + L_3)
   is
   begin
      Lemma_Product_Monotone (0.0, C, 0.0, A1);
      Lemma_Product_Monotone (0.0, C * A1, 0.0, Bs);
      Lemma_Product_Monotone (0.0, C * A1 * Bs, 0.0, Cs);
      Lemma_Product_Monotone (0.0, C, 0.0, T1);
      Lemma_Product_Monotone (0.0, C * T1, 0.0, B);
      Lemma_At_Most_Scaled (T1, A1, C, Solved_Tolerance);
      Lemma_At_Most_Chain (C * T1, C * A1, S, Solved_Tolerance, Prepared_Tolerance, L_1);
      Lemma_At_Most_Product (C * T1, C * A1, B, Bs, Solved_Tolerance, Solved_Tolerance, L_1);
      Lemma_At_Most_Chain (C * T1 * B, C * A1 * Bs, J, L_1, Prepared_Tolerance, L_2);
      Lemma_At_Most_Product (C * T1 * B, C * A1 * Bs, Cc, Cs, L_1, Solved_Tolerance, L_2);
      Lemma_At_Most_From_About (C * A1 * Bs * Cs, A, Peak_Tolerance);
      Lemma_At_Most_Chain (C * T1 * B * Cc, C * A1 * Bs * Cs, A, L_2, Peak_Tolerance, L_3);
      Lemma_Scale_Monotone (L_1, L_3, S);
      Lemma_Scale_Monotone (L_2, L_3, J);
   end Lemma_Real_Limits;
   --  A ramp whose spans are within those of a saturated ramp which is within the limits is within the limits,
   --  to the tolerance which Prunt.Motion_Planner.Trajectory.Certificates certifies.

   ------------------------------------------------------------------------------------------------------------------
   --  A solve: what each case concludes
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Same_Span (X : Real) with Pre => X >= 0.0, Post => At_Most (X, X, Solved_Tolerance) is
   begin
      null;
   end Lemma_Same_Span;

   procedure Lemma_Zero_Times with Post => R (0.0 * s) = 0.0 is
   begin
      null;
   end Lemma_Zero_Times;

   procedure Lemma_Case_Full (T4 : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      C  : constant Real := R (Solver.Cm);
      A1 : constant Real := R (Solver.T1);
      A2 : constant Real := R (Solver.T2);
      A3 : constant Real := R (Solver.T3);
      Bs : constant Real := A1 + A2;
      Cs : constant Real := 2.0 * A1 + A2 + A3;
      AP : constant Real := C * A1 * Bs * Cs;
      D0 : constant Real := 4.0 * A1 + 2.0 * A2 + A3;
      A  : constant Real := R (Solver.Am);
      V  : constant Real := R (Vd);
      Q  : constant Real := Full_Span (Solver);
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Lemma_Zero_Times;
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Nonnegative (T4);
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = AP);
      Lemma_Same_Span (A1);
      Lemma_Same_Span (Bs);
      Lemma_Same_Span (Cs);
      --  The spans up to the last, without the last phase time, are the time which was taken from the speed
      --  time.
      case Solver.Region is
         when Region_1            =>
            Lemma_About_Sum
              (Cs,
               R (Solver.Accel_Over_Jerk),
               A1 + Bs,
               R (Solver.Jerk_Over_Snap) + R (Solver.Snap_Over_Crackle),
               Region_Tolerance,
               Region_Tolerance,
               Region_Tolerance);
            pragma Assert (D0 = Cs + (A1 + Bs));

         when Region_2            =>
            pragma Assert (D0 = 2.0 * (A1 + Bs));

         when Region_3 | Region_5 =>
            pragma Assert (D0 = 4.0 * A1);
            Lemma_About_Same (D0);
            Lemma_About_Weaken (D0, D0, 0.0, Region_Tolerance);

         when Region_4            =>
            Lemma_About_Same (2.0 * A1);
            Lemma_About_Sum
              (Cs, R (Solver.Accel_Over_Jerk), 2.0 * A1, 2.0 * A1, Region_Tolerance, 0.0, Region_Tolerance);
            pragma Assert (D0 = Cs + 2.0 * A1);
      end case;
      pragma Assert (About (D0, Q, Region_Tolerance));
      Lemma_Real_Full (AP, A, D0, Q, R (T4), V);
      pragma Assert (Exact_Gain (C, A1, A2, A3, R (T4)) = AP * (D0 + R (T4)));
   end Lemma_Case_Full;

   procedure Lemma_Case_Jerk_Limited (T3, T4 : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      C   : constant Real := R (Solver.Cm);
      A1  : constant Real := R (Solver.T1);
      A2  : constant Real := R (Solver.T2);
      A3  : constant Real := R (Solver.T3);
      Bs  : constant Real := A1 + A2;
      Cs  : constant Real := 2.0 * A1 + A2 + A3;
      JP  : constant Real := C * A1 * Bs;
      A   : constant Real := R (Solver.Am);
      J   : constant Real := R (Solver.Jm);
      V   : constant Real := R (Vd);
      AJr : constant Real := R (Solver.Accel_Over_Jerk);
      X3  : constant Real := R (T3);
      H   : constant Real := A1 + Bs;
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Lemma_Zero_Times;
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Nonnegative (T3);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = JP);
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = JP * Cs);
      Lemma_Product_Monotone (0.0, C, 0.0, A1);
      Lemma_Product_Monotone (0.0, C * A1, 0.0, Bs);
      Lemma_Product_Monotone (0.0, A, 0.0, Full_Span (Solver));
      Lemma_Same_Span (A1);
      Lemma_Same_Span (Bs);
      if Solver.Region = Region_1 then
         declare
            G : constant Real := R (Solver.Jerk_Over_Snap) + R (Solver.Snap_Over_Crackle);
         begin
            pragma Assert (Full_Span (Solver) = AJr + G);
            Lemma_Real_Jerk_Limited (J, V, G, H, X3);
            Lemma_Real_Jerk_Tail (J, A, V, G, H, Cs, AJr, JP, H + X3);
            pragma Assert (Acceleration_Span (Solver.T1, Solver.T2, T3) = H + X3);
            pragma Assert (Exact_Gain (C, A1, A2, X3, 0.0) = JP * ((H + X3) * (H + (H + X3))));
         end;
      else
         pragma Assert (A2 = 0.0);
         pragma Assert (Full_Span (Solver) = AJr + 2.0 * A1);
         Lemma_Real_Jerk_Only (J, V, R (Solver.Jerk_Over_Crackle), A1, X3);
         Lemma_About_Same (2.0 * A1);
         Lemma_About_Weaken (2.0 * A1, 2.0 * A1, 0.0, Region_Tolerance);
         Lemma_Real_Jerk_Tail (J, A, V, 2.0 * A1, 2.0 * A1, Cs, AJr, JP, 2.0 * A1 + X3);
         pragma Assert (Acceleration_Span (Solver.T1, 0.0 * s, T3) = 2.0 * A1 + X3);
         pragma Assert (Exact_Gain (C, A1, 0.0, X3, 0.0) = JP * ((2.0 * A1 + X3) * (2.0 * A1 + (2.0 * A1 + X3))));
      end if;
   end Lemma_Case_Jerk_Limited;

   procedure Lemma_Snap_Limited_Tail (Solver : Delta_V_Solver; T2 : Time; V, Bound, U : Real)
   with
     Pre  =>
       Solver_In_Domain (Solver)
       and then Solver.Region in Region_1 | Region_2
       and then Solver_Is_Consistent (Solver)
       and then T2 >= 0.0 * s
       and then V >= 0.0
       and then (R (T2) = 0.0 or else At_Most (Exact_Gain (R (Solver.Cm), R (Solver.T1), R (T2), 0.0, 0.0), V, N_64))
       and then
         (if not Is_Capped (T2)
          then Exact_Gain (R (Solver.Cm), R (Solver.T1), R (T2), 0.0, 0.0) >= V - Bisected_Slack * V)
       and then
         Bound
         = 2.0
           * (R (Solver.Cm) * R (Solver.T1))
           * Jerk_Span (Solver.T1, Solver.T2)
           * ((R (Solver.T1) + Jerk_Span (Solver.T1, Solver.T2)) * (R (Solver.T1) + Jerk_Span (Solver.T1, Solver.T2)))
       and then At_Most (V, Bound, U)
       and then U >= 0.0
       and then U <= T_P3
       and then R (Solver.Snap_Only_Above) <= V,
     Post =>
       Spans_Are_Within (Solver, Solver.T1, T2, 0.0 * s)
       and then Exact_Gain (R (Solver.Cm), R (Solver.T1), R (T2), 0.0, 0.0) <= V + Solved_Tolerance * V
       and then
         (if not Is_Capped (T2)
          then Exact_Gain (R (Solver.Cm), R (Solver.T1), R (T2), 0.0, 0.0) >= V - Solved_Tolerance * V)
   is
      C  : constant Real := R (Solver.Cm);
      A1 : constant Real := R (Solver.T1);
      A2 : constant Real := R (Solver.T2);
      A3 : constant Real := R (Solver.T3);
      Bs : constant Real := A1 + A2;
      Cs : constant Real := 2.0 * A1 + A2 + A3;
      X  : constant Real := R (T2);
      B  : constant Real := A1 + X;
      Sp : constant Real := C * A1;
   begin
      Lemma_Solver_Reals (Solver);
      Lemma_Zero_Times;
      Rounding.Lemma_Nonnegative (T2);
      Lemma_Same_Span (A1);
      Lemma_Scale_Monotone (Bisected_Slack, Solved_Tolerance, V);
      pragma Assert (Sp > 0.0);
      pragma Assert (Cs >= A1 + Bs);
      if X = 0.0 then
         --  The ramp is the one of four equal phases, which gains the speed above which snap is reached.
         pragma Assert (At_Most (B, Bs, Solved_Tolerance));
         pragma Assert (At_Most (A1 + B, Cs, Solved_Tolerance));
         Lemma_Product_Monotone (0.0, Sp, 0.0, A1);
         Lemma_Product_Monotone (0.0, Sp * A1, 0.0, A1);
         Lemma_Product_Monotone (0.0, Sp * A1 * A1, 0.0, A1);
         Lemma_About_Symmetric (R (Solver.Snap_Only_Above), 8.0 * Sp * A1 * A1 * A1, Region_Tolerance, R_39);
         Lemma_About_Bounds (8.0 * Sp * A1 * A1 * A1, R (Solver.Snap_Only_Above), R_39);
         pragma Assert (Exact_Gain (C, A1, 0.0, 0.0, 0.0) = 8.0 * Sp * A1 * A1 * A1);
         Lemma_Scale_Monotone (R (Solver.Snap_Only_Above), V, 1.0 + R_39);
      else
         Lemma_Snap_Gain_Identity (C, A1, X);
         Lemma_At_Most_Chain (Exact_Gain (C, A1, X, 0.0, 0.0), V, Bound, N_64, U, T_P3 + N_128);
         Lemma_Real_Snap_Span (Sp, A1, B, Bs, T_P3 + N_128);
         Lemma_At_Most_Weaken (B, Bs, T_P3 + N_128, Solved_Tolerance);
         Lemma_Scale_Monotone (A1 + Bs, Cs, 1.0 + (T_P3 + N_128));
         pragma Assert (At_Most (A1 + B, Cs, Solved_Tolerance));
         Lemma_Scale_Monotone (N_64, Solved_Tolerance, V);
      end if;
      pragma Assert (Jerk_Span (Solver.T1, T2) = B);
      pragma Assert (Acceleration_Span (Solver.T1, T2, 0.0 * s) = A1 + B);
   end Lemma_Snap_Limited_Tail;
   --  What the two snap-limited cases share: Bound is what the saturated ramp cut after its second phase gains,
   --  and the speed change is at most that.

   procedure Lemma_Case_Snap_Limited_1 (T2, T3 : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      C  : constant Real := R (Solver.Cm);
      A1 : constant Real := R (Solver.T1);
      Bs : constant Real := Jerk_Span (Solver.T1, Solver.T2);
      J  : constant Real := R (Solver.Jm);
      V  : constant Real := R (Vd);
      G  : constant Real := R (Solver.Jerk_Over_Snap) + R (Solver.Snap_Over_Crackle);
      H  : constant Real := A1 + Bs;
      JP : constant Real := C * A1 * Bs;
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Order (Solver.Snap_Only_Above, Vd);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = JP);
      Lemma_Product_Monotone (0.0, C, 0.0, A1);
      Lemma_Product_Monotone (0.0, C * A1, 0.0, Bs);
      Lemma_Product_Monotone (0.0, G, 0.0, G);
      Lemma_Product_Monotone (0.0, G * G, 0.0, J);
      Lemma_Product_Monotone (0.0, H, 0.0, H);
      Lemma_Product_Monotone (0.0, H * H, 0.0, JP);

      --  The speed change is at most twice the square of the base times the jerk limit.
      pragma Assert (G * G * J + 4.0 * V <= 9.0 * (G * G) * J + N_1024 * (9.0 * (G * G) * J));
      pragma Assert (V <= 2.0 * (G * G * J) + N_2048 * (2.0 * (G * G * J)));
      pragma Assert (At_Most (V, 2.0 * (G * G * J), N_2048));

      --  Which is what the saturated ramp cut after its second phase gains.
      Lemma_About_Symmetric (H, G, Region_Tolerance, R_39);
      Lemma_At_Most_From_About (G, H, R_39);
      Lemma_At_Most_Product (G, H, G, H, R_39, R_39, R_38 + N_1);
      Lemma_About_Symmetric (JP, J, Region_Tolerance, R_39);
      Lemma_At_Most_From_About (J, JP, R_39);
      Lemma_At_Most_Product (G * G, H * H, J, JP, R_38 + N_1, R_39, 2.0 * R_38);
      Lemma_At_Most_Scaled (G * G * J, H * H * JP, 2.0, 2.0 * R_38);
      Lemma_At_Most_Chain (V, 2.0 * (G * G * J), 2.0 * (H * H * JP), N_2048, 2.0 * R_38, T_A);
      pragma Assert (2.0 * (H * H * JP) = 2.0 * (C * A1) * Bs * ((A1 + Bs) * (A1 + Bs)));
      Lemma_Snap_Limited_Tail (Solver, T2, V, 2.0 * (H * H * JP), T_A);
   end Lemma_Case_Snap_Limited_1;

   procedure Lemma_Case_Snap_Limited_2 (T2, T4 : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      C  : constant Real := R (Solver.Cm);
      A1 : constant Real := R (Solver.T1);
      A3 : constant Real := R (Solver.T3);
      Bs : constant Real := Jerk_Span (Solver.T1, Solver.T2);
      A  : constant Real := R (Solver.Am);
      V  : constant Real := R (Vd);
      Q  : constant Real := Full_Span (Solver);
      H  : constant Real := A1 + Bs;
      JP : constant Real := C * A1 * Bs;
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Order (Solver.Snap_Only_Above, Vd);
      pragma Assert (A3 = 0.0);
      pragma Assert (Acceleration_Span (Solver.T1, Solver.T2, Solver.T3) = H);
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = JP * H);
      Lemma_Product_Monotone (0.0, C, 0.0, A1);
      Lemma_Product_Monotone (0.0, C * A1, 0.0, Bs);
      Lemma_Product_Monotone (0.0, JP, 0.0, H);
      Lemma_Product_Monotone (0.0, JP * H, 0.0, 2.0 * H);
      pragma Assert (Q >= 0.0);
      Lemma_Product_Monotone (0.0, A, 0.0, Q);

      --  The speed change is at most the acceleration limit times twice the sum of the first two spans.
      pragma Assert (At_Most (V, A * Q, N_64));
      Lemma_About_Symmetric (JP * H, A, Peak_Tolerance, T_P);
      Lemma_At_Most_From_About (A, JP * H, T_P);
      Lemma_About_Symmetric (2.0 * H, Q, Region_Tolerance, R_39);
      Lemma_At_Most_From_About (Q, 2.0 * H, R_39);
      Lemma_At_Most_Product (A, JP * H, Q, 2.0 * H, T_P, R_39, T_Q);
      Lemma_At_Most_Chain (V, A * Q, (JP * H) * (2.0 * H), N_64, T_Q, T_P3);
      pragma Assert ((JP * H) * (2.0 * H) = 2.0 * (C * A1) * Bs * ((A1 + Bs) * (A1 + Bs)));
      Lemma_Snap_Limited_Tail (Solver, T2, V, (JP * H) * (2.0 * H), T_P3);
   end Lemma_Case_Snap_Limited_2;

   Tiny_Fourth_Power : constant := 2.0**(-788);

   function Is_Tiny (Vd : Raw) return Boolean
   is (not (Vd >= Tiny_Speed));

   procedure Lemma_Crackle_Limited_Tail (Solver : Delta_V_Solver; T : Time; Vd : Velocity; Bound, U : Real)
   with
     Pre  =>
       Solver_In_Domain (Solver)
       and then Solver_Is_Consistent (Solver)
       and then Vd >= No_Speed
       and then Crackle_Time_Fact (T, Vd, Solver.Cm)
       and then Bound = 8.0 * R (Solver.Cm) * ((R (Solver.T1) * R (Solver.T1)) * (R (Solver.T1) * R (Solver.T1)))
       and then At_Most (R (Vd), Bound, U)
       and then U >= 0.0
       and then U <= T_P3,
     Post =>
       Spans_Are_Within (Solver, T, 0.0 * s, 0.0 * s)
       and then Gain_Is_At_Most (Solver.Cm, T, 0.0 * s, 0.0 * s, 0.0 * s, Vd)
       and then Gain_Is_At_Least (Solver.Cm, T, 0.0 * s, 0.0 * s, 0.0 * s, Vd)
   is
      C  : constant Real := R (Solver.Cm);
      A1 : constant Real := R (Solver.T1);
      A2 : constant Real := R (Solver.T2);
      A3 : constant Real := R (Solver.T3);
      X  : constant Real := R (T);
      V  : constant Real := R (Vd);
   begin
      Lemma_Solver_Reals (Solver);
      Lemma_Zero_Times;
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Nonnegative (T);
      pragma Assert (Exact_Gain (C, X, 0.0, 0.0, 0.0) = 8.0 * C * ((X * X) * (X * X)));
      if not Is_Tiny (Vd) then
         Lemma_Real_Fourth_Span (C, X, A1, V, Bound, U, T_P3 + N_1024);
         Lemma_At_Most_Weaken (X, A1, T_P3 + N_1024, Solved_Tolerance);
         Lemma_About_Bounds (8.0 * C * ((X * X) * (X * X)), V, N_512);
         Lemma_Scale_Monotone (N_512, Solved_Tolerance, V);
      else
         --  A tiny speed change: the phase time is far below any saturated one, and gains next to nothing.
         pragma Assert (X <= A1);
         Lemma_Product_Monotone (X, Tiny_Time, X, Tiny_Time);
         Lemma_Product_Monotone (X * X, Tiny_Time * Tiny_Time, X * X, Tiny_Time * Tiny_Time);
         pragma Assert ((X * X) * (X * X) <= Tiny_Fourth_Power);
         Lemma_Product_Monotone (0.0, X, 0.0, X);
         Lemma_Product_Monotone (0.0, X * X, 0.0, X * X);
         Lemma_Scale_Monotone ((X * X) * (X * X), Tiny_Fourth_Power, 8.0 * C);
         pragma Assert (8.0 * C * ((X * X) * (X * X)) <= Gain_Residue);
         Lemma_Scale_Monotone (0.0, Solved_Tolerance, V);
         --  And a speed change this small is itself below the residue.
         Lemma_Tiny_Constants;
         Rounding.Lemma_Order (Vd, Tiny_Speed);
         Lemma_Product_Monotone (0.0, 8.0 * C, 0.0, (X * X) * (X * X));
         pragma Assert (V <= Gain_Residue);
      end if;
      pragma Assert (At_Most (X, A1, Solved_Tolerance));
      Lemma_Scale_Monotone (A1, A1 + A2, 1.0 + Solved_Tolerance);
      Lemma_Scale_Monotone (2.0 * A1, 2.0 * A1 + A2 + A3, 1.0 + Solved_Tolerance);
      pragma Assert (Jerk_Span (T, 0.0 * s) = X);
      pragma Assert (Acceleration_Span (T, 0.0 * s, 0.0 * s) = 2.0 * X);
   end Lemma_Crackle_Limited_Tail;
   --  What the crackle-limited cases share: Bound is what four equal phases of the saturated first phase time
   --  gain, and the speed change is at most that.

   procedure Lemma_Case_Crackle_Limited_Snap (T : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      C  : constant Real := R (Solver.Cm);
      A1 : constant Real := R (Solver.T1);
      V  : constant Real := R (Vd);
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Rounding.Lemma_Nonnegative (Vd);
      Rounding.Lemma_Order (Vd, Solver.Snap_Only_Above);
      Lemma_At_Most_From_About (R (Solver.Snap_Only_Above), 8.0 * (C * A1) * A1 * A1 * A1, Region_Tolerance);
      pragma Assert (8.0 * (C * A1) * A1 * A1 * A1 = 8.0 * C * ((A1 * A1) * (A1 * A1)));
      pragma Assert (At_Most (V, 8.0 * C * ((A1 * A1) * (A1 * A1)), Region_Tolerance));
      Lemma_Crackle_Limited_Tail (Solver, T, Vd, 8.0 * C * ((A1 * A1) * (A1 * A1)), Region_Tolerance);
   end Lemma_Case_Crackle_Limited_Snap;

   procedure Lemma_Case_Crackle_Limited_Single (T, T4 : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      C  : constant Real := R (Solver.Cm);
      A1 : constant Real := R (Solver.T1);
      A  : constant Real := R (Solver.Am);
      V  : constant Real := R (Vd);
      AP : constant Real := C * A1 * A1 * (2.0 * A1);
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Rounding.Lemma_Nonnegative (Vd);
      pragma Assert (R (Solver.T2) = 0.0 and then R (Solver.T3) = 0.0);
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = AP);
      Lemma_Product_Monotone (0.0, C, 0.0, A1);
      Lemma_Product_Monotone (0.0, C * A1, 0.0, A1);
      Lemma_Product_Monotone (0.0, C * A1 * A1, 0.0, 2.0 * A1);
      Lemma_Product_Monotone (0.0, A, 0.0, 4.0 * A1);
      Lemma_Product_Monotone (0.0, AP, 0.0, 4.0 * A1);
      pragma Assert (Full_Span (Solver) = 4.0 * A1);
      pragma Assert (At_Most (V, A * (4.0 * A1), N_64));
      Lemma_About_Symmetric (AP, A, Peak_Tolerance, T_P);
      Lemma_At_Most_From_About (A, AP, T_P);
      Lemma_At_Most_Scaled (A, AP, 4.0 * A1, T_P);
      Lemma_At_Most_Chain (V, (4.0 * A1) * A, (4.0 * A1) * AP, N_64, T_P, T_P2);
      pragma Assert ((4.0 * A1) * AP = 8.0 * C * ((A1 * A1) * (A1 * A1)));
      Lemma_Crackle_Limited_Tail (Solver, T, Vd, (4.0 * A1) * AP, T_P2);
   end Lemma_Case_Crackle_Limited_Single;

   procedure Lemma_Case_Crackle_Limited_Jerk (T, T3 : Time; Vd : Velocity; Solver : Delta_V_Solver) is
      C   : constant Real := R (Solver.Cm);
      A1  : constant Real := R (Solver.T1);
      J   : constant Real := R (Solver.Jm);
      V   : constant Real := R (Vd);
      Gam : constant Real := R (Solver.Jerk_Over_Crackle);
      JP  : constant Real := C * A1 * A1;
      TJ  : constant Real := A1 * A1 * J;
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Rounding.Lemma_Nonnegative (Vd);
      pragma Assert (R (Solver.T2) = 0.0);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = JP);
      Lemma_Product_Monotone (0.0, C, 0.0, A1);
      Lemma_Product_Monotone (0.0, C * A1, 0.0, A1);
      Lemma_Product_Monotone (0.0, A1, 0.0, A1);
      Lemma_Product_Monotone (0.0, A1 * A1, 0.0, J);
      Lemma_Product_Monotone (0.0, A1 * A1, 0.0, JP);

      --  The speed change is at most eight times the square of the first phase time times the jerk limit.
      Lemma_About_Scaled (A1 * A1, Gam, J, Region_Tolerance);
      pragma Assert (abs (TJ - Gam * J) <= Region_Tolerance * (Gam * J));
      pragma Assert (Gam * J + V <= 9.0 * (A1 * A1) * J + N_1024 * (9.0 * (A1 * A1) * J));
      pragma Assert (Gam * J >= TJ - R_39 * TJ);
      pragma Assert (V <= 8.0 * TJ + N_2048 * (8.0 * TJ) + R_39 * TJ);
      pragma Assert (At_Most (V, 8.0 * TJ, R_39));

      --  Which is what four equal phases of that time gain.
      Lemma_About_Symmetric (JP, J, Region_Tolerance, R_39);
      Lemma_At_Most_From_About (J, JP, R_39);
      Lemma_At_Most_Scaled (J, JP, 8.0 * (A1 * A1), R_39);
      Lemma_At_Most_Chain (V, (8.0 * (A1 * A1)) * J, (8.0 * (A1 * A1)) * JP, R_39, R_39, R_38 + N_1);
      pragma Assert ((8.0 * (A1 * A1)) * JP = 8.0 * C * ((A1 * A1) * (A1 * A1)));
      Lemma_Crackle_Limited_Tail (Solver, T, Vd, (8.0 * (A1 * A1)) * JP, R_38 + N_1);
   end Lemma_Case_Crackle_Limited_Jerk;

   procedure Lemma_Snap_Limited_Is_Sound
     (Times : Feedrate_Profile_Times; T : Time; Vd : Velocity; Solver : Delta_V_Solver) is
   begin
      if not Solver_Is_Consistent (Solver) then
         return;
      end if;
      Lemma_Snap_Gain (Times, Vd, Solver);
      if Solver.Region = Region_1 then
         Lemma_Case_Snap_Limited_1 (Times (2), T, Vd, Solver);
      else
         Lemma_Case_Snap_Limited_2 (Times (2), T, Vd, Solver);
      end if;
      Rounding.Lemma_Same (Times (1), Solver.T1);
      Lemma_No_Time (Times (3));
      Lemma_No_Time (Times (4));
      Lemma_Zero_Times;
      pragma Assert (Spans_Are_Within (Solver, Solver.T1, Times (2), 0.0 * s));
      pragma Assert (Jerk_Span (Times (1), Times (2)) = Jerk_Span (Solver.T1, Times (2)));
      pragma
        Assert
          (Acceleration_Span (Times (1), Times (2), Times (3)) = Acceleration_Span (Solver.T1, Times (2), 0.0 * s));
      pragma Assert (Spans_Are_Within (Solver, Times (1), Times (2), Times (3)));
      pragma Assert (Gain_Is_At_Most (Solver.Cm, Solver.T1, Times (2), 0.0 * s, 0.0 * s, Vd));
      pragma Assert (Gain_Is_At_Most (Solver.Cm, Times (1), Times (2), Times (3), Times (4), Vd));
      pragma Assert (Gain_Is_Reached (Solver.Cm, Solver.T1, Times (2), 0.0 * s, 0.0 * s, Vd));
      pragma Assert (Gain_Is_Reached (Solver.Cm, Times (1), Times (2), Times (3), Times (4), Vd));
   end Lemma_Snap_Limited_Is_Sound;

   procedure Lemma_Empty_Is_Sound (Vd : Velocity; Solver : Delta_V_Solver) is
   begin
      if not Solver_In_Domain (Solver) then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Lemma_Zero_Times;
      Lemma_No_Time (Vd);
   end Lemma_Empty_Is_Sound;

   procedure Lemma_Sound_Is_Within_Limits
     (Solver : Delta_V_Solver; Sm : Snap; Vd : Velocity; Times : Feedrate_Profile_Times)
   is
      C     : constant Real := R (Solver.Cm);
      A1    : constant Real := R (Solver.T1);
      Bs    : constant Real := Jerk_Span (Solver.T1, Solver.T2);
      Cs    : constant Real := Acceleration_Span (Solver.T1, Solver.T2, Solver.T3);
      T1    : constant Real := R (Times (1));
      B     : constant Real := Jerk_Span (Times (1), Times (2));
      Cc    : constant Real := Acceleration_Span (Times (1), Times (2), Times (3));
      Exact : constant Profile_Model.Ramp := To_Ramp (Times, Solver.Cm, No_Velocity);
   begin
      Lemma_Solver_Reals (Solver);
      Lemma_Limit (Sm);
      Rounding.Lemma_Nonnegative (Times (1));
      Rounding.Lemma_Nonnegative (Times (2));
      Rounding.Lemma_Nonnegative (Times (3));
      pragma Assert (Snap_Peak (Solver.Cm, Solver.T1) = C * A1);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = C * A1 * Bs);
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = C * A1 * Bs * Cs);
      Lemma_Real_Limits (C, R (Sm), R (Solver.Jm), R (Solver.Am), T1, B, Cc, A1, Bs, Cs);
      Lemma_Product_Monotone (0.0, C, 0.0, T1);
      Lemma_Product_Monotone (0.0, C * T1, 0.0, B);
      Lemma_Product_Monotone (0.0, C * T1 * B, 0.0, Cc);
      pragma Assert (Profile_Model.Peak_Snap (Exact) = C * T1);
      pragma Assert (Profile_Model.Peak_Jerk (Exact) = C * T1 * B);
      pragma Assert (Profile_Model.Peak_Accel (Exact) = C * T1 * B * Cc);
      pragma Assert (Exact.C = C);
   end Lemma_Sound_Is_Within_Limits;

   procedure Lemma_Empty_Is_Within_Limits
     (Times : Feedrate_Profile_Times; Vd : Velocity; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle)
   is
      Exact : constant Profile_Model.Ramp := To_Ramp (Times, Cm, No_Velocity);
   begin
      if not Limits_In_Domain (Am, Jm, Sm, Cm) then
         return;
      end if;
      Lemma_Limit (Am);
      Lemma_Limit (Jm);
      Lemma_Limit (Sm);
      Lemma_Limit (Cm);
      Lemma_No_Time (Times (1));
      Lemma_No_Time (Times (2));
      Lemma_No_Time (Times (3));
      Lemma_No_Time (Times (4));
      Lemma_No_Time (Vd);
      pragma Assert (Profile_Model.Peak_Snap (Exact) = 0.0);
      pragma Assert (Profile_Model.Peak_Jerk (Exact) = 0.0);
      pragma Assert (Profile_Model.Peak_Accel (Exact) = 0.0);
      pragma Assert (Exact.C = R (Cm));
      pragma Assert (Exact_Gain (R (Cm), 0.0, 0.0, 0.0, 0.0) = 0.0);
   end Lemma_Empty_Is_Within_Limits;

   procedure Lemma_Prepared_Is_Within_Limits
     (Solver : Delta_V_Solver;
      Times  : Feedrate_Profile_Times;
      Vd     : Velocity;
      Am     : Acceleration;
      Jm     : Jerk;
      Sm     : Snap;
      Cm     : Crackle) is
   begin
      Lemma_Sound_Is_Within_Limits (Solver, Sm, Vd, Times);
      Rounding.Lemma_Same (Solver.Am, Am);
      Rounding.Lemma_Same (Solver.Jm, Jm);
      Rounding.Lemma_Same (Solver.Cm, Cm);
      pragma Assert (Profile_Model."=" (To_Ramp (Times, Solver.Cm, No_Velocity), To_Ramp (Times, Cm, No_Velocity)));
   end Lemma_Prepared_Is_Within_Limits;

   ------------------------------------------------------------------------------------------------------------------
   --  The distance solver
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Crackle_Root_2 (T, Am, Cm : Raw) is
      A : constant Real := R (Am);
      C : constant Real := R (Cm);

      F_D : constant Raw := 2.0 * Cm;
      F_Q : constant Raw := Am / F_D;
      F_X : constant Raw := F_Q / 1.0;
      F_T : constant Raw := Cube_Root (F_X);
   begin
      Lemma_Domain_Constants;
      Lemma_Limit (Am);
      Lemma_Limit (Cm);
      Lemma_Near_Exact (2.0);
      Lemma_Near_Exact (Am);
      Lemma_Near_Exact (Cm);
      Lemma_Near_Product (2.0, C, 2.0, Cm, 0.0, 0.0, N_1);
      declare
         Q : constant Real := A / (2.0 * C);
      begin
         Lemma_Quotient_Times_Divisor (A, 2.0 * C);
         pragma Assert (Q * (2.0 * C) = A);
         pragma Assert (Q * C = 0.5 * A);
         pragma Assert (Two_M162 * C <= Q * C);
         Lemma_Product_Cancel (Two_M162, Q, C);
         Lemma_Near_Quotient (A, 2.0 * C, Q, Am, F_D, 0.0, N_1, N_2);
         Lemma_Unit_Operations (F_Q);
         Lemma_Near_Same (Q, F_Q, F_X, N_2);
         Lemma_Root_Cubed (Q, F_X, N_2, Cube_Tolerance);
         Lemma_Unit_Operations (F_T);
         Rounding.Lemma_Same (T, F_T);
         Lemma_Real_Cube_Fact (A, C, Q, R (T));
      end;
   end Lemma_Crackle_Root_2;

   Order_Slack : constant := 2.0**(-43);
   --  How far, relatively, the exact travel of a ramp can be above that of another whose computed travel is no
   --  less: a little more than four times Travel_Slack.

   Span_Slack      : constant := 2.0**(-41);
   Half_Span_Slack : constant := 2.0**(-42);
   --  How far, relatively, a span of the first can then be above that span of the other.

   Day : constant := 86_400.0;

   procedure Lemma_Distance_Constants
   with
     Post =>
       R (Search_Time_Cap) = Day
       and then R (Rounding_Time_Max) >= Day
       and then Search_Time_Cap <= Rounding_Time_Max
       and then Search_Time_Cap <= Phase_Time_Max
       and then Rounding_Time_Max <= Phase_Time_Max
       and then Two_P80 * Day <= Normal_Snap_Max
       and then Snap_Ceiling <= Normal_Snap_Max
       and then Speed_Max <= Normal_Start_Max
   is
   begin
      null;
   end Lemma_Distance_Constants;

   procedure Lemma_Real_Evaluations (W1, W2, F1, F2 : Real)
   with
     Pre  =>
       W1 >= 0.0
       and then W2 >= 0.0
       and then abs (F1 - W1) <= Travel_Slack * W1
       and then abs (F2 - W2) <= Travel_Slack * W2
       and then F1 <= F2,
     Post => W1 <= W2 + Order_Slack * W2
   is
      Top : constant Real := W2 + Travel_Slack * W2;
   begin
      Lemma_Scale_Monotone (Travel_Slack, 0.5, W1);
      pragma Assert (W1 - Travel_Slack * W1 <= Top);
      pragma Assert (W1 <= 2.0 * Top);
      Lemma_Scale_Monotone (Travel_Slack, 1.0, W2);
      pragma Assert (Top <= 2.0 * W2);
      Lemma_Scale_Monotone (W1, 4.0 * W2, Travel_Slack);
      pragma Assert (W1 <= W2 + Travel_Slack * W2 + 4.0 * W2 * Travel_Slack);
      Lemma_Scale_Monotone (5.0 * Travel_Slack, Order_Slack, W2);
   end Lemma_Real_Evaluations;
   --  W1 and W2 are exact travels and F1 and F2 what was computed for them.

   procedure Lemma_Travels_Ordered (T, U : Feedrate_Profile_Times; Cm : Crackle; Vs : Velocity; D : Length)
   with
     Pre  =>
       Times_In_Range (T)
       and then Times_In_Range (U)
       and then Travel_Is_Normal (T (1), T (2), T (3), T (4), Cm, Vs)
       and then Travel_Is_Normal (U (1), U (2), U (3), U (4), Cm, Vs)
       and then Fast_Distance_At_Max_Time (T, Cm, Vs) <= D
       and then not (D > Fast_Distance_At_Max_Time (U, Cm, Vs)),
     Post =>
       Exact_Travel (R (Cm), R (Vs), R (T (1)), R (T (2)), R (T (3)), R (T (4)))
       <= Exact_Travel (R (Cm), R (Vs), R (U (1)), R (U (2)), R (U (3)), R (U (4)))
          + Order_Slack * Exact_Travel (R (Cm), R (Vs), R (U (1)), R (U (2)), R (U (3)), R (U (4)))
   is
      FT : constant Length := Fast_Distance_At_Max_Time (T, Cm, Vs);
      FU : constant Length := Fast_Distance_At_Max_Time (U, Cm, Vs);
   begin
      Lemma_Travel_Is_Near (T (1), T (2), T (3), T (4), Cm, Vs);
      Lemma_Travel_Is_Near (U (1), U (2), U (3), U (4), Cm, Vs);
      Rounding.Lemma_Order (FT, D);
      Rounding.Lemma_Order (D, FU);
      Rounding.Lemma_Same (FT, Distance_Formula (T (1), T (2), T (3), T (4), Cm, Vs));
      Rounding.Lemma_Same (FU, Distance_Formula (U (1), U (2), U (3), U (4), Cm, Vs));
      Lemma_Real_Evaluations
        (Exact_Travel (R (Cm), R (Vs), R (T (1)), R (T (2)), R (T (3)), R (T (4))),
         Exact_Travel (R (Cm), R (Vs), R (U (1)), R (U (2)), R (U (3)), R (U (4))),
         R (FT),
         R (FU));
   end Lemma_Travels_Ordered;
   --  A ramp which was computed to fit in D travels no further than one which was computed not to fall short of
   --  D, to Order_Slack.

   procedure Lemma_Travel_Identity_3 (C, V, A1, A2, X, K, P, Cc : Real)
   with
     Pre  => K = C * A1 * (A1 + A2) and then P = 2.0 * A1 + A2 and then Cc = P + X,
     Post => Exact_Travel (C, V, A1, A2, X, 0.0) = 2.0 * V * (P + Cc) + K * Cc * ((P + Cc) * (P + Cc))
   is
   begin
      null;
   end Lemma_Travel_Identity_3;

   procedure Lemma_Travel_Identity_2 (C, V, A1, X, Sp, B : Real)
   with
     Pre  => Sp = C * A1 and then B = A1 + X,
     Post =>
       Exact_Travel (C, V, A1, X, 0.0, 0.0) = 4.0 * V * (A1 + B) + 4.0 * Sp * B * ((A1 + B) * (A1 + B) * (A1 + B))
   is
   begin
      null;
   end Lemma_Travel_Identity_2;

   procedure Lemma_Travel_Identity_1 (C, V, X : Real)
   with Post => Exact_Travel (C, V, X, 0.0, 0.0, 0.0) = 8.0 * V * X + 32.0 * C * ((X * X) * (X * X) * X)
   is
   begin
      null;
   end Lemma_Travel_Identity_1;
   --  The travel of a ramp without its last phase, without its last two, and of four equal phases.

   procedure Lemma_Real_Longer_Span (K, V, P, C1, C2 : Real)
   with
     Pre  =>
       K > 0.0
       and then V >= 0.0
       and then P > 0.0
       and then C1 >= P
       and then C2 >= P
       and then
         2.0 * V * (P + C1) + K * C1 * ((P + C1) * (P + C1))
         <= (2.0 * V * (P + C2) + K * C2 * ((P + C2) * (P + C2)))
            + Order_Slack * (2.0 * V * (P + C2) + K * C2 * ((P + C2) * (P + C2))),
     Post => C1 <= C2 + Span_Slack * C2
   is
      CD : constant Real := C2 + Span_Slack * C2;
      U1 : constant Real := P + C1;
      U2 : constant Real := P + C2;
      L2 : constant Real := 2.0 * V * U2;
      Q2 : constant Real := K * C2 * (U2 * U2);
      F2 : constant Real := L2 + Q2;
   begin
      if C1 > CD then
         pragma Assert (U1 >= U2 + Half_Span_Slack * U2);
         Lemma_Scale_Monotone (U2 + Half_Span_Slack * U2, U1, 2.0 * V);
         pragma Assert (2.0 * V * U1 >= L2 + Half_Span_Slack * L2);
         Lemma_Product_Monotone (U2, U1, U2, U1);
         Lemma_Product_Monotone (0.0, U2, 0.0, U2);
         Lemma_Product_Monotone (CD, C1, U2 * U2, U1 * U1);
         Lemma_Scale_Monotone (CD * (U2 * U2), C1 * (U1 * U1), K);
         pragma Assert (K * C1 * (U1 * U1) >= K * (CD * (U2 * U2)));
         pragma Assert (K * (CD * (U2 * U2)) = Q2 + Span_Slack * Q2);
         pragma Assert (K * C2 > 0.0);
         pragma Assert (U2 * U2 > 0.0);
         pragma Assert (Q2 > 0.0);
         pragma Assert (K * C1 * (U1 * U1) >= Q2 + Half_Span_Slack * Q2);
         Lemma_Product_Monotone (0.0, V, 0.0, U2);
         pragma Assert (L2 >= 0.0);
         pragma Assert (2.0 * V * U1 + K * C1 * (U1 * U1) >= F2 + Half_Span_Slack * F2);
         Lemma_Strict_Scale (Order_Slack, Half_Span_Slack, F2);
         pragma Assert (False);
      end if;
   end Lemma_Real_Longer_Span;
   --  The travel of a ramp without its last phase, as a function of its acceleration span C with the jerk peak
   --  K and the sum P of the first two spans fixed: a span longer by the fraction Span_Slack travels further by
   --  at least half that fraction, because C is at least P.

   procedure Lemma_Real_Longer_Jerk_Span (Sp, V, A, B1, B2 : Real)
   with
     Pre  =>
       Sp > 0.0
       and then V >= 0.0
       and then A > 0.0
       and then B1 >= A
       and then B2 >= A
       and then
         4.0 * V * (A + B1) + 4.0 * Sp * B1 * ((A + B1) * (A + B1) * (A + B1))
         <= (4.0 * V * (A + B2) + 4.0 * Sp * B2 * ((A + B2) * (A + B2) * (A + B2)))
            + Order_Slack * (4.0 * V * (A + B2) + 4.0 * Sp * B2 * ((A + B2) * (A + B2) * (A + B2))),
     Post => B1 <= B2 + Span_Slack * B2
   is
      BD : constant Real := B2 + Span_Slack * B2;
      U1 : constant Real := A + B1;
      U2 : constant Real := A + B2;
      L2 : constant Real := 4.0 * V * U2;
      Q2 : constant Real := 4.0 * Sp * B2 * (U2 * U2 * U2);
      F2 : constant Real := L2 + Q2;
   begin
      if B1 > BD then
         pragma Assert (U1 >= U2 + Half_Span_Slack * U2);
         Lemma_Scale_Monotone (U2 + Half_Span_Slack * U2, U1, 4.0 * V);
         pragma Assert (4.0 * V * U1 >= L2 + Half_Span_Slack * L2);
         Lemma_Product_Monotone (U2, U1, U2, U1);
         Lemma_Product_Monotone (0.0, U2, 0.0, U2);
         Lemma_Product_Monotone (U2 * U2, U1 * U1, U2, U1);
         Lemma_Product_Monotone (0.0, U2 * U2, 0.0, U2);
         Lemma_Product_Monotone (BD, B1, U2 * U2 * U2, U1 * U1 * U1);
         Lemma_Scale_Monotone (BD * (U2 * U2 * U2), B1 * (U1 * U1 * U1), 4.0 * Sp);
         pragma Assert (4.0 * Sp * B1 * (U1 * U1 * U1) >= 4.0 * Sp * (BD * (U2 * U2 * U2)));
         pragma Assert (4.0 * Sp * (BD * (U2 * U2 * U2)) = Q2 + Span_Slack * Q2);
         pragma Assert (Sp * B2 > 0.0);
         pragma Assert (U2 * U2 > 0.0);
         pragma Assert (U2 * U2 * U2 > 0.0);
         pragma Assert (Q2 > 0.0);
         pragma Assert (4.0 * Sp * B1 * (U1 * U1 * U1) >= Q2 + Half_Span_Slack * Q2);
         Lemma_Product_Monotone (0.0, V, 0.0, U2);
         pragma Assert (L2 >= 0.0);
         pragma Assert (4.0 * V * U1 + 4.0 * Sp * B1 * (U1 * U1 * U1) >= F2 + Half_Span_Slack * F2);
         Lemma_Strict_Scale (Order_Slack, Half_Span_Slack, F2);
         pragma Assert (False);
      end if;
   end Lemma_Real_Longer_Jerk_Span;
   --  The same for a ramp without its last two phases, as a function of its jerk span B with the snap peak Sp
   --  and the first phase time A fixed.

   procedure Lemma_Real_Longer_Phase (C, V, X1, X2 : Real)
   with
     Pre  =>
       C > 0.0
       and then V >= 0.0
       and then X1 >= 0.0
       and then X2 > 0.0
       and then
         8.0 * V * X1 + 32.0 * C * ((X1 * X1) * (X1 * X1) * X1)
         <= (8.0 * V * X2 + 32.0 * C * ((X2 * X2) * (X2 * X2) * X2))
            + Order_Slack * (8.0 * V * X2 + 32.0 * C * ((X2 * X2) * (X2 * X2) * X2)),
     Post => X1 <= X2 + Span_Slack * X2
   is
      XD : constant Real := X2 + Span_Slack * X2;
      P1 : constant Real := (X1 * X1) * (X1 * X1);
      P2 : constant Real := (X2 * X2) * (X2 * X2);
      L2 : constant Real := 8.0 * V * X2;
      Q2 : constant Real := 32.0 * C * (P2 * X2);
      F2 : constant Real := L2 + Q2;
   begin
      if X1 > XD then
         Lemma_Scale_Monotone (XD, X1, 8.0 * V);
         pragma Assert (8.0 * V * X1 >= L2 + Half_Span_Slack * L2);
         Lemma_Product_Monotone (X2, X1, X2, X1);
         Lemma_Product_Monotone (0.0, X2, 0.0, X2);
         Lemma_Product_Monotone (X2 * X2, X1 * X1, X2 * X2, X1 * X1);
         Lemma_Product_Monotone (0.0, X2 * X2, 0.0, X2 * X2);
         Lemma_Product_Monotone (P2, P1, XD, X1);
         Lemma_Scale_Monotone (P2 * XD, P1 * X1, 32.0 * C);
         pragma Assert (32.0 * C * (P1 * X1) >= 32.0 * C * (P2 * XD));
         pragma Assert (32.0 * C * (P2 * XD) = Q2 + Span_Slack * Q2);
         pragma Assert (X2 * X2 > 0.0);
         pragma Assert (P2 > 0.0);
         pragma Assert (P2 * X2 > 0.0);
         pragma Assert (Q2 > 0.0);
         pragma Assert (32.0 * C * (P1 * X1) >= Q2 + Half_Span_Slack * Q2);
         Lemma_Product_Monotone (0.0, V, 0.0, X2);
         pragma Assert (L2 >= 0.0);
         pragma Assert (8.0 * V * X1 + 32.0 * C * (P1 * X1) >= F2 + Half_Span_Slack * F2);
         Lemma_Strict_Scale (Order_Slack, Half_Span_Slack, F2);
         pragma Assert (False);
      end if;
   end Lemma_Real_Longer_Phase;
   --  The same for a ramp of four equal phases, as a function of its phase time.

   function Shares_Start (Solver : Delta_V_Solver; Cm : Crackle; Vs : Velocity) return Boolean
   is (In_Domain (Cm)
       and then Solver.Cm = Cm
       and then Core_Is_Consistent (Solver)
       and then Is_Moderate (Solver)
       and then Vs >= No_Speed
       and then Vs <= Speed_Max);
   --  What the lemmas about one bisected phase time take of the saturated ramp and of the request.

   procedure Lemma_Saturated_Reals (Solver : Delta_V_Solver; Cm : Crackle; Vs : Velocity)
   with
     Pre  => Shares_Start (Solver, Cm, Vs),
     Post =>
       R (Cm) >= Two_M80
       and then R (Cm) <= Two_P80
       and then R (Solver.Cm) = R (Cm)
       and then R (Vs) >= 0.0
       and then R (Solver.T1) >= Two_M170
       and then R (Solver.T2) >= 0.0
       and then R (Solver.T3) >= 0.0
       and then R (Cm) * R (Solver.T1) <= Normal_Snap_Max
       and then Solver.T1 >= Normal_Time_Min
       and then Solver.T1 <= Phase_Time_Max
       and then Solver.T2 <= Phase_Time_Max
       and then Solver.T3 <= Phase_Time_Max
       and then Cm >= Normal_Crackle_Min
       and then Cm <= Crackle_Magnitude_Max
       and then Vs <= Normal_Start_Max
   is
   begin
      Lemma_Limit (Cm);
      Lemma_Normal_Constants;
      Lemma_Distance_Constants;
      Rounding.Lemma_Same (Solver.Cm, Cm);
      Rounding.Lemma_Nonnegative (Vs);
      Rounding.Lemma_Nonnegative (Solver.T2);
      Rounding.Lemma_Nonnegative (Solver.T3);
      Rounding.Lemma_Order (Normal_Time_Min, Solver.T1);
      pragma Assert (Snap_Peak (Solver.Cm, Solver.T1) = R (Cm) * R (Solver.T1));
   end Lemma_Saturated_Reals;

   procedure Lemma_Third_Phase
     (Solver : Delta_V_Solver; Above, Times : Feedrate_Profile_Times; Cm : Crackle; Vs : Velocity; D : Length)
   with
     Pre  =>
       Shares_Start (Solver, Cm, Vs)
       and then Times_In_Range (Times)
       and then Times_In_Range (Above)
       and then Is_Candidate (Solver, Above, 4)
       and then Times (1) = Solver.T1
       and then Times (2) = Solver.T2
       and then Times (3) > No_Time
       and then Times (3) <= Search_Time_Cap
       and then Times (4) = No_Time
       and then Fast_Distance_At_Max_Time (Times, Cm, Vs) <= D
       and then not (D > Fast_Distance_At_Max_Time (Above, Cm, Vs)),
     Post =>
       At_Most
         (Acceleration_Span (Times (1), Times (2), Times (3)),
          Acceleration_Span (Solver.T1, Solver.T2, Solver.T3),
          Span_Slack)
   is
      C  : constant Real := R (Cm);
      V  : constant Real := R (Vs);
      A1 : constant Real := R (Solver.T1);
      A2 : constant Real := R (Solver.T2);
      A3 : constant Real := R (Solver.T3);
      X  : constant Real := R (Times (3));
      K  : constant Real := C * A1 * (A1 + A2);
      P  : constant Real := 2.0 * A1 + A2;
   begin
      Lemma_Saturated_Reals (Solver, Cm, Vs);
      Lemma_Distance_Constants;
      Rounding.Lemma_Same (Times (1), Solver.T1);
      Rounding.Lemma_Same (Times (2), Solver.T2);
      Rounding.Lemma_Nonnegative (Times (3));
      Lemma_No_Time (Times (4));
      Rounding.Lemma_Same (Above (1), Solver.T1);
      Rounding.Lemma_Same (Above (2), Solver.T2);
      Rounding.Lemma_Same (Above (3), Solver.T3);
      Lemma_No_Time (Above (4));
      pragma Assert (Travel_Is_Normal (Times (1), Times (2), Times (3), Times (4), Cm, Vs));
      pragma Assert (Travel_Is_Normal (Above (1), Above (2), Above (3), Above (4), Cm, Vs));
      Lemma_Travels_Ordered (Times, Above, Cm, Vs, D);
      Lemma_Travel_Identity_3 (C, V, A1, A2, X, K, P, P + X);
      Lemma_Travel_Identity_3 (C, V, A1, A2, A3, K, P, P + A3);
      pragma Assert (C * A1 > 0.0);
      pragma Assert (K > 0.0);
      Lemma_Real_Longer_Span (K, V, P, P + X, P + A3);
      pragma Assert (Acceleration_Span (Times (1), Times (2), Times (3)) = P + X);
      pragma Assert (Acceleration_Span (Solver.T1, Solver.T2, Solver.T3) = P + A3);
   end Lemma_Third_Phase;

   procedure Lemma_Second_Phase
     (Solver : Delta_V_Solver; Above, Times : Feedrate_Profile_Times; Cm : Crackle; Vs : Velocity; D : Length)
   with
     Pre  =>
       Shares_Start (Solver, Cm, Vs)
       and then Times_In_Range (Times)
       and then Times_In_Range (Above)
       and then Is_Candidate (Solver, Above, 3)
       and then Times (1) = Solver.T1
       and then Times (2) > No_Time
       and then Times (2) <= Search_Time_Cap
       and then Times (3) = No_Time
       and then Times (4) = No_Time
       and then Fast_Distance_At_Max_Time (Times, Cm, Vs) <= D
       and then not (D > Fast_Distance_At_Max_Time (Above, Cm, Vs)),
     Post => At_Most (Jerk_Span (Times (1), Times (2)), Jerk_Span (Solver.T1, Solver.T2), Span_Slack)
   is
      C  : constant Real := R (Cm);
      V  : constant Real := R (Vs);
      A1 : constant Real := R (Solver.T1);
      A2 : constant Real := R (Solver.T2);
      X  : constant Real := R (Times (2));
      Sp : constant Real := C * A1;
   begin
      Lemma_Saturated_Reals (Solver, Cm, Vs);
      Lemma_Distance_Constants;
      Rounding.Lemma_Same (Times (1), Solver.T1);
      Rounding.Lemma_Nonnegative (Times (2));
      Lemma_No_Time (Times (3));
      Lemma_No_Time (Times (4));
      Rounding.Lemma_Same (Above (1), Solver.T1);
      Rounding.Lemma_Same (Above (2), Solver.T2);
      Lemma_No_Time (Above (3));
      Lemma_No_Time (Above (4));
      pragma Assert (Travel_Is_Normal (Times (1), Times (2), Times (3), Times (4), Cm, Vs));
      pragma Assert (Travel_Is_Normal (Above (1), Above (2), Above (3), Above (4), Cm, Vs));
      Lemma_Travels_Ordered (Times, Above, Cm, Vs, D);
      Lemma_Travel_Identity_2 (C, V, A1, X, Sp, A1 + X);
      Lemma_Travel_Identity_2 (C, V, A1, A2, Sp, A1 + A2);
      pragma Assert (Sp > 0.0);
      Lemma_Real_Longer_Jerk_Span (Sp, V, A1, A1 + X, A1 + A2);
      pragma Assert (Jerk_Span (Times (1), Times (2)) = A1 + X);
      pragma Assert (Jerk_Span (Solver.T1, Solver.T2) = A1 + A2);
   end Lemma_Second_Phase;

   procedure Lemma_First_Phase
     (Solver : Delta_V_Solver; Above, Times : Feedrate_Profile_Times; Cm : Crackle; Vs : Velocity; D : Length)
   with
     Pre  =>
       Shares_Start (Solver, Cm, Vs)
       and then Times_In_Range (Times)
       and then Times_In_Range (Above)
       and then Is_Candidate (Solver, Above, 2)
       and then Times (1) > No_Time
       and then Times (1) <= Search_Time_Cap
       and then Times (2) = No_Time
       and then Times (3) = No_Time
       and then Times (4) = No_Time
       and then Fast_Distance_At_Max_Time (Times, Cm, Vs) <= D
       and then not (D > Fast_Distance_At_Max_Time (Above, Cm, Vs)),
     Post => At_Most (R (Times (1)), R (Solver.T1), Span_Slack)
   is
      C  : constant Real := R (Cm);
      V  : constant Real := R (Vs);
      A1 : constant Real := R (Solver.T1);
      X  : constant Real := R (Times (1));
   begin
      Lemma_Saturated_Reals (Solver, Cm, Vs);
      Lemma_Distance_Constants;
      Rounding.Lemma_Nonnegative (Times (1));
      Lemma_Scale_Monotone (0.0, Span_Slack, A1);
      if Times (1) < Normal_Time_Min then
         --  A phase time too short for its travel to be bounded relatively is shorter than any saturated one.
         Rounding.Lemma_Order (Times (1), Normal_Time_Min);
      else
         Rounding.Lemma_Order (Times (1), Search_Time_Cap);
         Lemma_Product_Monotone (0.0, C, 0.0, X);
         Lemma_Product_Monotone (C, Two_P80, X, Day);
         Lemma_No_Time (Times (2));
         Lemma_No_Time (Times (3));
         Lemma_No_Time (Times (4));
         Rounding.Lemma_Same (Above (1), Solver.T1);
         Lemma_No_Time (Above (2));
         Lemma_No_Time (Above (3));
         Lemma_No_Time (Above (4));
         pragma Assert (Travel_Is_Normal (Times (1), Times (2), Times (3), Times (4), Cm, Vs));
         pragma Assert (Travel_Is_Normal (Above (1), Above (2), Above (3), Above (4), Cm, Vs));
         Lemma_Travels_Ordered (Times, Above, Cm, Vs, D);
         Lemma_Travel_Identity_1 (C, V, X);
         Lemma_Travel_Identity_1 (C, V, A1);
         Lemma_Real_Longer_Phase (C, V, X, A1);
      end if;
   end Lemma_First_Phase;

   procedure Lemma_Distance_Spans
     (Solver             : Delta_V_Solver;
      Above, Base, Times : Feedrate_Profile_Times;
      Variable           : Feedrate_Profile_Times_Index;
      Cm                 : Crackle;
      Vs                 : Velocity;
      D                  : Length)
   with
     Pre  =>
       Shares_Start (Solver, Cm, Vs)
       and then Times_In_Range (Times)
       and then Times_In_Range (Above)
       and then Is_Candidate (Solver, Base, Variable)
       and then Is_Solved (Base, Times, Variable)
       and then (Times (Variable) = No_Time or else Fast_Distance_At_Max_Time (Times, Cm, Vs) <= D)
       and then
         (if Variable < 4
          then
            Is_Candidate (Solver, Above, Variable + 1) and then not (D > Fast_Distance_At_Max_Time (Above, Cm, Vs))),
     Post =>
       Spans_Are_Within (Solver, Times (1), Times (2), Times (3))
       and then R (Times (1)) >= 0.0
       and then R (Times (2)) >= 0.0
       and then R (Times (3)) >= 0.0
   is
      A1 : constant Real := R (Solver.T1);
      A2 : constant Real := R (Solver.T2);
      A3 : constant Real := R (Solver.T3);
      Bs : constant Real := A1 + A2;
      Cs : constant Real := 2.0 * A1 + A2 + A3;
      X1 : constant Real := R (Times (1));
      X2 : constant Real := R (Times (2));
      X3 : constant Real := R (Times (3));
   begin
      Lemma_Saturated_Reals (Solver, Cm, Vs);
      Rounding.Lemma_Nonnegative (Times (1));
      Rounding.Lemma_Nonnegative (Times (2));
      Rounding.Lemma_Nonnegative (Times (3));
      Lemma_Same_Span (A1);
      Lemma_Same_Span (Bs);
      Lemma_Same_Span (Cs);
      Lemma_Scale_Monotone (0.0, Solved_Tolerance, A1);
      Lemma_Scale_Monotone (0.0, Solved_Tolerance, Bs);
      Lemma_Scale_Monotone (0.0, Solved_Tolerance, Cs);
      pragma Assert (Jerk_Span (Solver.T1, Solver.T2) = Bs);
      pragma Assert (Acceleration_Span (Solver.T1, Solver.T2, Solver.T3) = Cs);
      pragma Assert (Jerk_Span (Times (1), Times (2)) = X1 + X2);
      pragma Assert (Acceleration_Span (Times (1), Times (2), Times (3)) = 2.0 * X1 + X2 + X3);
      case Variable is
         when 4 =>
            Rounding.Lemma_Same (Times (1), Solver.T1);
            Rounding.Lemma_Same (Times (2), Solver.T2);
            Rounding.Lemma_Same (Times (3), Solver.T3);

         when 3 =>
            Rounding.Lemma_Same (Times (1), Solver.T1);
            Rounding.Lemma_Same (Times (2), Solver.T2);
            Rounding.Lemma_Nonnegative (Times (3));
            if Times (3) = No_Time then
               Lemma_No_Time (Times (3));
            else
               Lemma_Third_Phase (Solver, Above, Times, Cm, Vs, D);
               Lemma_At_Most_Weaken (2.0 * X1 + X2 + X3, Cs, Span_Slack, Solved_Tolerance);
            end if;

         when 2 =>
            Rounding.Lemma_Same (Times (1), Solver.T1);
            Lemma_No_Time (Times (3));
            Rounding.Lemma_Nonnegative (Times (2));
            if Times (2) = No_Time then
               Lemma_No_Time (Times (2));
            else
               Lemma_Second_Phase (Solver, Above, Times, Cm, Vs, D);
               Lemma_At_Most_Weaken (X1 + X2, Bs, Span_Slack, Solved_Tolerance);
               --  The acceleration span is the first phase time and the jerk span.
               Lemma_Scale_Monotone (A1 + Bs, Cs, 1.0 + Span_Slack);
               pragma Assert (2.0 * X1 + X2 + X3 <= Cs + Span_Slack * Cs);
               Lemma_Scale_Monotone (Span_Slack, Solved_Tolerance, Cs);
            end if;

         when 1 =>
            Lemma_No_Time (Times (2));
            Lemma_No_Time (Times (3));
            Rounding.Lemma_Nonnegative (Times (1));
            if Times (1) = No_Time then
               Lemma_No_Time (Times (1));
            else
               Lemma_First_Phase (Solver, Above, Times, Cm, Vs, D);
               Lemma_At_Most_Weaken (X1, A1, Span_Slack, Solved_Tolerance);
               Lemma_Scale_Monotone (A1, Bs, 1.0 + Span_Slack);
               Lemma_Scale_Monotone (2.0 * A1, Cs, 1.0 + Span_Slack);
               pragma Assert (X1 + X2 <= Bs + Span_Slack * Bs);
               pragma Assert (2.0 * X1 + X2 + X3 <= Cs + Span_Slack * Cs);
               Lemma_Scale_Monotone (Span_Slack, Solved_Tolerance, Bs);
               Lemma_Scale_Monotone (Span_Slack, Solved_Tolerance, Cs);
            end if;
      end case;
   end Lemma_Distance_Spans;

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
   is
      C     : constant Real := R (Cm);
      A1    : constant Real := R (Solver.T1);
      Bs    : constant Real := Jerk_Span (Solver.T1, Solver.T2);
      Cs    : constant Real := Acceleration_Span (Solver.T1, Solver.T2, Solver.T3);
      T1    : constant Real := R (Times (1));
      B     : constant Real := Jerk_Span (Times (1), Times (2));
      Cc    : constant Real := Acceleration_Span (Times (1), Times (2), Times (3));
      Exact : constant Profile_Model.Ramp := To_Ramp (Times, Cm, No_Velocity);
   begin
      if not Moderate then
         return;
      end if;
      Lemma_Limit (Am);
      Lemma_Limit (Jm);
      Lemma_Limit (Sm);
      Lemma_Limit (Cm);
      Rounding.Lemma_Same (Solver.Am, Am);
      Rounding.Lemma_Same (Solver.Jm, Jm);
      Rounding.Lemma_Same (Solver.Cm, Cm);
      Lemma_Distance_Spans (Solver, Above, Base, Times, Variable, Cm, Vs, D);
      Lemma_Saturated_Reals (Solver, Cm, Vs);
      pragma Assert (Snap_Peak (Solver.Cm, Solver.T1) = C * A1);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = C * A1 * Bs);
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = C * A1 * Bs * Cs);
      Lemma_Real_Limits (C, R (Sm), R (Jm), R (Am), T1, B, Cc, A1, Bs, Cs);
      Lemma_Product_Monotone (0.0, C, 0.0, T1);
      Lemma_Product_Monotone (0.0, C * T1, 0.0, B);
      Lemma_Product_Monotone (0.0, C * T1 * B, 0.0, Cc);
      pragma Assert (Profile_Model.Peak_Snap (Exact) = C * T1);
      pragma Assert (Profile_Model.Peak_Jerk (Exact) = C * T1 * B);
      pragma Assert (Profile_Model.Peak_Accel (Exact) = C * T1 * B * Cc);
      pragma Assert (Exact.C = C);
   end Lemma_Distance_Solve;

   Two_P99  : constant := 2.0**99;
   Two_P100 : constant := 2.0**100;

   procedure Lemma_Moderate_Constants
   with
     Post =>
       R (Moderate_Time_Max) = Two_P99
       and then R (Rounding_Time_Max) = Two_P100
       and then Moderate_Time_Max <= Rounding_Time_Max
   is
   begin
      null;
   end Lemma_Moderate_Constants;

   procedure Lemma_Real_Moderate (S, J, Kap, Sp, Bs : Real)
   with
     Pre  =>
       S > 0.0
       and then J >= 0.0
       and then Bs >= 0.0
       and then Kap <= Two_P99
       and then Sp >= S - N_1 * S
       and then S * Kap >= J - N_1 * J
       and then Sp * Bs <= J + Prepared_Tolerance * J,
     Post => Bs <= Two_P100
   is
   begin
      Lemma_Scale_Monotone (S - N_1 * S, Sp, Bs);
      Lemma_Scale_Monotone (Kap, Two_P99, S);
      pragma Assert (J - N_1 * J <= Two_P99 * S);
      pragma Assert ((S - N_1 * S) * Bs = S * Bs - N_1 * (S * Bs));
      pragma Assert (S * Bs - N_1 * (S * Bs) <= J + Prepared_Tolerance * J);
      pragma Assert (S * Bs <= Two_P100 * S);
      Lemma_Product_Cancel (Bs, Two_P100, S);
   end Lemma_Real_Moderate;
   --  S and J are the snap and jerk limits, Kap their quotient as it was computed, Sp the snap peak and Bs the
   --  jerk span of the saturated ramp.

   procedure Lemma_Moderate_2
     (Solver : Delta_V_Solver; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle; Moderate : Boolean)
   is
      C   : constant Real := R (Cm);
      S   : constant Real := R (Sm);
      J   : constant Real := R (Jm);
      A1  : constant Real := R (Solver.T1);
      A2  : constant Real := R (Solver.T2);
      Kap : constant Real := R (Solver.Jerk_Over_Snap);
   begin
      if not Moderate then
         return;
      end if;
      Lemma_Limit (Jm);
      Lemma_Limit (Sm);
      Lemma_Limit (Cm);
      Lemma_Moderate_Constants;
      Rounding.Lemma_Same (Solver.Cm, Cm);
      Rounding.Lemma_Same (Solver.Jm, Jm);
      Rounding.Lemma_Same (Solver.T1, Solver.Snap_Over_Crackle);
      Rounding.Lemma_Nonnegative (Solver.T1);
      Rounding.Lemma_Nonnegative (Solver.T2);
      Rounding.Lemma_Order (Solver.Jerk_Over_Snap, Moderate_Time_Max);
      Lemma_About_Bounds (C * A1, S, N_1);
      Lemma_About_Bounds (S * Kap, J, N_1);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = C * A1 * (A1 + A2));
      Lemma_Real_Moderate (S, J, Kap, C * A1, A1 + A2);
      Rounding.Lemma_Order (Solver.T2, Rounding_Time_Max);
   end Lemma_Moderate_2;

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
   is
      C  : constant Real := R (Solver.Cm);
      A1 : constant Real := R (Solver.T1);
      Bs : constant Real := Jerk_Span (Solver.T1, Solver.T2);
      Cs : constant Real := Acceleration_Span (Solver.T1, Solver.T2, Solver.T3);
      T1 : constant Real := R (Times (1));
      B  : constant Real := Jerk_Span (Times (1), Times (2));
      Cc : constant Real := Acceleration_Span (Times (1), Times (2), Times (3));
   begin
      if not (Solver_Is_Consistent (Solver)
              and then Snap_Is_Within (Solver, Sm)
              and then Holds_Limits (Solver, Am, Jm, Cm)
              and then Limits_In_Domain (Am, Jm, Sm, Cm))
      then
         return;
      end if;
      Lemma_Solver_Reals (Solver);
      Lemma_Limit (Sm);
      Rounding.Lemma_Same (Solver.Am, Am);
      Rounding.Lemma_Same (Solver.Jm, Jm);
      Rounding.Lemma_Same (Solver.Cm, Cm);
      Rounding.Lemma_Nonnegative (Times (1));
      Rounding.Lemma_Nonnegative (Times (2));
      Rounding.Lemma_Nonnegative (Times (3));
      pragma Assert (Snap_Peak (Solver.Cm, Solver.T1) = C * A1);
      pragma Assert (Jerk_Peak (Solver.Cm, Solver.T1, Solver.T2) = C * A1 * Bs);
      pragma Assert (Acceleration_Peak (Solver.Cm, Solver.T1, Solver.T2, Solver.T3) = C * A1 * Bs * Cs);
      Lemma_Real_Limits (C, R (Sm), R (Jm), R (Am), T1, B, Cc, A1, Bs, Cs);
      pragma Assert (Snap_Peak (Cm, Times (1)) = C * T1);
      pragma Assert (Jerk_Peak (Cm, Times (1), Times (2)) = C * T1 * B);
      pragma Assert (Acceleration_Peak (Cm, Times (1), Times (2), Times (3)) = C * T1 * B * Cc);
   end Lemma_Peaks;

   procedure Lemma_Peaks_Of_Rest (T1, T2, T3 : Time; Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) is
   begin
      if not Limits_In_Domain (Am, Jm, Sm, Cm) then
         return;
      end if;
      Lemma_Limit (Am);
      Lemma_Limit (Jm);
      Lemma_Limit (Sm);
      Lemma_No_Time (T1);
      Lemma_No_Time (T2);
      Lemma_No_Time (T3);
      pragma Assert (Snap_Peak (Cm, T1) = 0.0);
      pragma Assert (Jerk_Peak (Cm, T1, T2) = 0.0);
      pragma Assert (Acceleration_Peak (Cm, T1, T2, T3) = 0.0);
   end Lemma_Peaks_Of_Rest;

   procedure Lemma_Real_Top_Speed (S, P, D, G : Real)
   with
     Pre  =>
       S >= 0.0
       and then P >= S
       and then D >= 0.0
       and then D <= (P - S) + Epsilon * (P - S) + Eta
       and then G <= D + Solved_Tolerance * D + Gain_Residue,
     Post => S + G <= P + Top_Speed_Tolerance * P + Top_Speed_Residue
   is
      W : constant Real := P - S;
   begin
      Lemma_Scale_Monotone (D, W + Epsilon * W + Eta, 1.0 + Solved_Tolerance);
      pragma
        Assert
          ((W + Epsilon * W + Eta) * (1.0 + Solved_Tolerance)
           = W + (Epsilon + Solved_Tolerance + Epsilon * Solved_Tolerance) * W + Eta * (1.0 + Solved_Tolerance));
      Lemma_Scale_Monotone (Epsilon + Solved_Tolerance + Epsilon * Solved_Tolerance, Top_Speed_Tolerance, W);
      Lemma_Scale_Monotone (W, P, Top_Speed_Tolerance);
      pragma Assert (Eta * (1.0 + Solved_Tolerance) + Gain_Residue <= Top_Speed_Residue);
   end Lemma_Real_Top_Speed;
   --  S is the starting speed, P the peak asked for, D the speed change as it was computed and G the gain.

   procedure Lemma_Top_Speed
     (Solver : Delta_V_Solver; Times : Feedrate_Profile_Times; Start_Vel, Peak : Velocity; Cm : Crackle)
   is
      Change : constant Velocity := abs (Start_Vel - Peak);
   begin
      if not (Solver_Is_Consistent (Solver) and then Solver.Cm = Cm and then Peak >= Start_Vel) then
         return;
      end if;
      Rounding.Lemma_Same (Solver.Cm, Cm);
      Rounding.Lemma_Nonnegative (Start_Vel);
      Rounding.Lemma_Order (Start_Vel, Peak);
      Float_Lemmas.Lemma_Rounding_Error_Sub (Start_Vel, Peak);
      Rounding.Lemma_Magnitude (Start_Vel - Peak);
      Rounding.Lemma_Nonnegative (Change);
      pragma Assert (R (Change) <= (R (Peak) - R (Start_Vel)) + Epsilon * (R (Peak) - R (Start_Vel)) + Eta);
      Lemma_Real_Top_Speed
        (R (Start_Vel),
         R (Peak),
         R (Change),
         Exact_Gain (R (Cm), R (Times (1)), R (Times (2)), R (Times (3)), R (Times (4))));
   end Lemma_Top_Speed;

   procedure Lemma_Top_Speed_Weaken (T1, T2, T3, T4 : Time; Start_Vel, Peak, Top : Velocity; Cm : Crackle) is
   begin
      Rounding.Lemma_Nonnegative (Peak);
      Rounding.Lemma_Order (Peak, Top);
      Lemma_Scale_Monotone (R (Peak), R (Top), Top_Speed_Tolerance);
   end Lemma_Top_Speed_Weaken;

   procedure Lemma_Top_Speed_Of_Rest (T1, T2, T3, T4 : Time; Start_Vel, Top : Velocity; Cm : Crackle) is
   begin
      Lemma_No_Time (T1);
      Lemma_No_Time (T2);
      Lemma_No_Time (T3);
      Lemma_No_Time (T4);
      Rounding.Lemma_Nonnegative (Start_Vel);
      Rounding.Lemma_Order (Start_Vel, Top);
      pragma Assert (Exact_Gain (R (Cm), R (T1), R (T2), R (T3), R (T4)) = 0.0);
      Lemma_Scale_Monotone (0.0, Top_Speed_Tolerance, R (Top));
   end Lemma_Top_Speed_Of_Rest;

end Prunt.Motion_Planner.Trajectory.Solvers.Accuracy;
