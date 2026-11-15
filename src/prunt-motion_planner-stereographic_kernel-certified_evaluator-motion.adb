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

pragma Warnings ("F");
--  A lemma can take a parameter only for its contract, which is not compiled.

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

package body Prunt.Motion_Planner.Stereographic_Kernel.Certified_Evaluator.Motion
  with SPARK_Mode => On
is
   package Unit_Speed renames Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed;

   Largest_Rate : constant := Finish_Correction.Largest_Rate;

   Rate_Square : constant := Largest_Rate * Largest_Rate;

   ---------------------------------------------------------------------------
   --  Real arithmetic
   ---------------------------------------------------------------------------

   procedure Lemma_Square_Monotone (A, B : Real)
   with Pre => 0.0 <= A and then A <= B, Post => A * A <= B * B;

   procedure Lemma_Square_Monotone (A, B : Real) is
   begin
      pragma Assert (A * A <= A * B);
      pragma Assert (A * B <= B * B);
   end Lemma_Square_Monotone;

   procedure Lemma_Root_Order (P, Q : Real)
   with Pre => Q >= 0.0 and then P * P <= Q * Q, Post => P <= Q and then 0.0 - Q <= P;
   --  |P| ≤ Q, without a square root.

   procedure Lemma_Root_Order (P, Q : Real) is
   begin
      if P > Q then
         pragma Assert (P * (P - Q) > 0.0);
         pragma Assert (Q * (P - Q) >= 0.0);
         pragma Assert (P * P > Q * Q);
      elsif 0.0 - P > Q then
         pragma Assert ((0.0 - P) * (0.0 - P - Q) > 0.0);
         pragma Assert (Q * (0.0 - P - Q) >= 0.0);
         pragma Assert ((0.0 - P) * (0.0 - P) > Q * Q);
         pragma Assert ((0.0 - P) * (0.0 - P) = P * P);
      end if;
   end Lemma_Root_Order;

   procedure Lemma_Sum_Of_Bounded (X, Y, M, A, B : Real)
   with
     Pre  => M >= 0.0 and then A >= 0.0 and then B >= 0.0 and then X * X <= M * (A * A) and then Y * Y <= M * (B * B),
     Post => (X + Y) * (X + Y) <= M * ((A + B) * (A + B));
   --  The triangle inequality for two numbers bounded through their squares: (X·Y)² ≤ (M·A·B)² gives
   --  X·Y ≤ M·A·B, the cross term of the square of the sum.

   procedure Lemma_Sum_Of_Bounded (X, Y, M, A, B : Real) is
      X_Square : constant Real := X * X;
      Y_Square : constant Real := Y * Y;
      A_Bound  : constant Real := M * (A * A);
      B_Bound  : constant Real := M * (B * B);
      Cross    : constant Real := X * Y;
      Bound    : constant Real := M * (A * B);
   begin
      pragma Assert (X_Square >= 0.0 and then Y_Square >= 0.0);
      pragma Assert (A_Bound >= 0.0 and then B_Bound >= 0.0);
      pragma Assert (X_Square * Y_Square <= A_Bound * Y_Square);
      pragma Assert (A_Bound * Y_Square <= A_Bound * B_Bound);
      pragma Assert (Cross * Cross = X_Square * Y_Square);
      pragma Assert (Bound * Bound = A_Bound * B_Bound);
      pragma Assert (A * B >= 0.0);
      pragma Assert (Bound >= 0.0);
      Lemma_Root_Order (Cross, Bound);
      pragma Assert ((X + Y) * (X + Y) = X_Square + 2.0 * Cross + Y_Square);
      pragma Assert (M * ((A + B) * (A + B)) = A_Bound + 2.0 * Bound + B_Bound);
   end Lemma_Sum_Of_Bounded;

   procedure Lemma_Lagrange (N1, N2, N3, N4, E1, E2, E3, E4 : Real)
   with
     Post =>
       (N1 * E1 + N2 * E2 + N3 * E3 + N4 * E4) * (N1 * E1 + N2 * E2 + N3 * E3 + N4 * E4)
       <= (N1 * N1 + N2 * N2 + N3 * N3 + N4 * N4) * (E1 * E1 + E2 * E2 + E3 * E3 + E4 * E4);
   --  The Cauchy-Schwarz inequality in four dimensions, by Lagrange's identity: the difference of the two sides
   --  is the sum of the squares of the six two-by-two determinants.

   procedure Lemma_Lagrange (N1, N2, N3, N4, E1, E2, E3, E4 : Real) is
      D12 : constant Real := N1 * E2 - N2 * E1;
      D13 : constant Real := N1 * E3 - N3 * E1;
      D14 : constant Real := N1 * E4 - N4 * E1;
      D23 : constant Real := N2 * E3 - N3 * E2;
      D24 : constant Real := N2 * E4 - N4 * E2;
      D34 : constant Real := N3 * E4 - N4 * E3;
   begin
      pragma Assert (D12 * D12 >= 0.0);
      pragma Assert (D13 * D13 >= 0.0);
      pragma Assert (D14 * D14 >= 0.0);
      pragma Assert (D23 * D23 >= 0.0);
      pragma Assert (D24 * D24 >= 0.0);
      pragma Assert (D34 * D34 >= 0.0);
      pragma
        Assert
          ((N1 * N1 + N2 * N2 + N3 * N3 + N4 * N4) * (E1 * E1 + E2 * E2 + E3 * E3 + E4 * E4)
           - (N1 * E1 + N2 * E2 + N3 * E3 + N4 * E4) * (N1 * E1 + N2 * E2 + N3 * E3 + N4 * E4)
           = D12 * D12 + D13 * D13 + D14 * D14 + D23 * D23 + D24 * D24 + D34 * D34);
   end Lemma_Lagrange;

   procedure Lemma_Cauchy_Schwarz (N, E : Real_Vector)
   with Post => Dot (N, E) * Dot (N, E) <= Dot (N, N) * Dot (E, E) and then Dot (N, N) >= 0.0;

   procedure Lemma_Cauchy_Schwarz (N, E : Real_Vector) is
   begin
      Lemma_Lagrange (N (X_Axis), N (Y_Axis), N (Z_Axis), N (E_Axis), E (X_Axis), E (Y_Axis), E (Z_Axis), E (E_Axis));
      pragma Assert (N (X_Axis) * N (X_Axis) >= 0.0);
      pragma Assert (N (Y_Axis) * N (Y_Axis) >= 0.0);
      pragma Assert (N (Z_Axis) * N (Z_Axis) >= 0.0);
      pragma Assert (N (E_Axis) * N (E_Axis) >= 0.0);
   end Lemma_Cauchy_Schwarz;

   procedure Lemma_Dot_Of_Sum (N, D, E, C : Real_Vector)
   with
     Pre  =>
       D (X_Axis) = E (X_Axis) + C (X_Axis)
       and then D (Y_Axis) = E (Y_Axis) + C (Y_Axis)
       and then D (Z_Axis) = E (Z_Axis) + C (Z_Axis)
       and then D (E_Axis) = E (E_Axis) + C (E_Axis),
     Post => Dot (N, D) = Dot (N, E) + Dot (N, C);

   procedure Lemma_Dot_Of_Sum (N, D, E, C : Real_Vector) is
   begin
      pragma Assert (N (X_Axis) * D (X_Axis) = N (X_Axis) * E (X_Axis) + N (X_Axis) * C (X_Axis));
      pragma Assert (N (Y_Axis) * D (Y_Axis) = N (Y_Axis) * E (Y_Axis) + N (Y_Axis) * C (Y_Axis));
      pragma Assert (N (Z_Axis) * D (Z_Axis) = N (Z_Axis) * E (Z_Axis) + N (Z_Axis) * C (Z_Axis));
      pragma Assert (N (E_Axis) * D (E_Axis) = N (E_Axis) * E (E_Axis) + N (E_Axis) * C (E_Axis));
   end Lemma_Dot_Of_Sum;

   procedure Lemma_Dot_Of_Difference (N, D, Left, Right : Real_Vector)
   with
     Pre  =>
       D (X_Axis) = Left (X_Axis) - Right (X_Axis)
       and then D (Y_Axis) = Left (Y_Axis) - Right (Y_Axis)
       and then D (Z_Axis) = Left (Z_Axis) - Right (Z_Axis)
       and then D (E_Axis) = Left (E_Axis) - Right (E_Axis),
     Post => Dot (N, D) = Dot (N, Left) - Dot (N, Right);

   procedure Lemma_Dot_Of_Difference (N, D, Left, Right : Real_Vector) is
   begin
      pragma Assert (N (X_Axis) * D (X_Axis) = N (X_Axis) * Left (X_Axis) - N (X_Axis) * Right (X_Axis));
      pragma Assert (N (Y_Axis) * D (Y_Axis) = N (Y_Axis) * Left (Y_Axis) - N (Y_Axis) * Right (Y_Axis));
      pragma Assert (N (Z_Axis) * D (Z_Axis) = N (Z_Axis) * Left (Z_Axis) - N (Z_Axis) * Right (Z_Axis));
      pragma Assert (N (E_Axis) * D (E_Axis) = N (E_Axis) * Left (E_Axis) - N (E_Axis) * Right (E_Axis));
   end Lemma_Dot_Of_Difference;

   procedure Lemma_Combined_Error (N, D, E, C : Real_Vector; Tangent_Bound, Total_Bound : Real)
   with
     Pre  =>
       0.0 <= Tangent_Bound
       and then Tangent_Bound <= Total_Bound
       and then Dot (E, E) <= Tangent_Bound * Tangent_Bound
       and then Dot (C, C) <= (Total_Bound - Tangent_Bound) * (Total_Bound - Tangent_Bound)
       and then D (X_Axis) = E (X_Axis) + C (X_Axis)
       and then D (Y_Axis) = E (Y_Axis) + C (Y_Axis)
       and then D (Z_Axis) = E (Z_Axis) + C (Z_Axis)
       and then D (E_Axis) = E (E_Axis) + C (E_Axis),
     Post => Dot (N, D) * Dot (N, D) <= Dot (N, N) * (Total_Bound * Total_Bound);
   --  A vector which is the sum of one within Tangent_Bound and one within the rest of Total_Bound has a
   --  component of at most Total_Bound times |N| along N.

   procedure Lemma_Combined_Error (N, D, E, C : Real_Vector; Tangent_Bound, Total_Bound : Real) is
      Norm_Square : constant Real := Dot (N, N);
      Along_E     : constant Real := Dot (N, E);
      Along_C     : constant Real := Dot (N, C);
      Rest        : constant Real := Total_Bound - Tangent_Bound;
   begin
      Lemma_Dot_Of_Sum (N, D, E, C);
      Lemma_Cauchy_Schwarz (N, E);
      Lemma_Cauchy_Schwarz (N, C);
      pragma Assert (Norm_Square >= 0.0);
      pragma Assert (Norm_Square * Dot (E, E) <= Norm_Square * (Tangent_Bound * Tangent_Bound));
      pragma Assert (Along_E * Along_E <= Norm_Square * (Tangent_Bound * Tangent_Bound));
      pragma Assert (Norm_Square * Dot (C, C) <= Norm_Square * (Rest * Rest));
      pragma Assert (Along_C * Along_C <= Norm_Square * (Rest * Rest));
      Lemma_Sum_Of_Bounded (Along_E, Along_C, Norm_Square, Tangent_Bound, Rest);
      pragma Assert (Tangent_Bound + Rest = Total_Bound);
      pragma Assert (Dot (N, D) = Along_E + Along_C);
   end Lemma_Combined_Error;

   procedure Lemma_Rate_Scaled (Rate_Value, Difference, Length : Real)
   with
     Pre  => 0.0 <= Rate_Value and then Rate_Value <= Largest_Rate and then Length > 0.0,
     Post =>
       (Rate_Value * Difference / Length) * (Rate_Value * Difference / Length)
       <= (Largest_Rate * Difference / Length) * (Largest_Rate * Difference / Length);
   --  The correction's tangent on one axis is largest where its rate is.

   procedure Lemma_Rate_Scaled (Rate_Value, Difference, Length : Real) is
      Quotient : constant Real := Difference / Length;
   begin
      pragma Assert (Rate_Value * Difference / Length = Rate_Value * Quotient);
      pragma Assert (Largest_Rate * Difference / Length = Largest_Rate * Quotient);
      Lemma_Square_Monotone (Rate_Value, Largest_Rate);
      pragma Assert (Rate_Value * Rate_Value <= Rate_Square);
      pragma Assert (Quotient * Quotient >= 0.0);
      pragma Assert ((Rate_Value * Rate_Value) * (Quotient * Quotient) <= Rate_Square * (Quotient * Quotient));
      pragma
        Assert ((Rate_Value * Quotient) * (Rate_Value * Quotient) = (Rate_Value * Rate_Value) * (Quotient * Quotient));
      pragma Assert ((Largest_Rate * Quotient) * (Largest_Rate * Quotient) = Rate_Square * (Quotient * Quotient));
   end Lemma_Rate_Scaled;

   procedure Lemma_Regroup (N, Combination, F0, F1, F2, F3 : Real_Vector; M0, M1, M2, M3 : Real)
   with
     Pre  =>
       Combination (X_Axis) = M0 * F0 (X_Axis) + M1 * F1 (X_Axis) + M2 * F2 (X_Axis) + M3 * F3 (X_Axis)
       and then Combination (Y_Axis) = M0 * F0 (Y_Axis) + M1 * F1 (Y_Axis) + M2 * F2 (Y_Axis) + M3 * F3 (Y_Axis)
       and then Combination (Z_Axis) = M0 * F0 (Z_Axis) + M1 * F1 (Z_Axis) + M2 * F2 (Z_Axis) + M3 * F3 (Z_Axis)
       and then Combination (E_Axis) = M0 * F0 (E_Axis) + M1 * F1 (E_Axis) + M2 * F2 (E_Axis) + M3 * F3 (E_Axis),
     Post => Dot (N, Combination) = M0 * Dot (N, F0) + M1 * Dot (N, F1) + M2 * Dot (N, F2) + M3 * Dot (N, F3);
   --  The component along N of a combination of four vectors is the combination of their components.

   procedure Lemma_Regroup (N, Combination, F0, F1, F2, F3 : Real_Vector; M0, M1, M2, M3 : Real) is
   begin
      pragma
        Assert
          (N (X_Axis) * Combination (X_Axis)
           = N (X_Axis) * (M0 * F0 (X_Axis)) + N (X_Axis) * (M1 * F1 (X_Axis)) + N (X_Axis) * (M2 * F2 (X_Axis))
             + N (X_Axis) * (M3 * F3 (X_Axis)));
      pragma
        Assert
          (N (Y_Axis) * Combination (Y_Axis)
           = N (Y_Axis) * (M0 * F0 (Y_Axis)) + N (Y_Axis) * (M1 * F1 (Y_Axis)) + N (Y_Axis) * (M2 * F2 (Y_Axis))
             + N (Y_Axis) * (M3 * F3 (Y_Axis)));
      pragma
        Assert
          (N (Z_Axis) * Combination (Z_Axis)
           = N (Z_Axis) * (M0 * F0 (Z_Axis)) + N (Z_Axis) * (M1 * F1 (Z_Axis)) + N (Z_Axis) * (M2 * F2 (Z_Axis))
             + N (Z_Axis) * (M3 * F3 (Z_Axis)));
      pragma
        Assert
          (N (E_Axis) * Combination (E_Axis)
           = N (E_Axis) * (M0 * F0 (E_Axis)) + N (E_Axis) * (M1 * F1 (E_Axis)) + N (E_Axis) * (M2 * F2 (E_Axis))
             + N (E_Axis) * (M3 * F3 (E_Axis)));
      pragma
        Assert
          (M0 * Dot (N, F0)
           = N (X_Axis) * (M0 * F0 (X_Axis)) + N (Y_Axis) * (M0 * F0 (Y_Axis)) + N (Z_Axis) * (M0 * F0 (Z_Axis))
             + N (E_Axis) * (M0 * F0 (E_Axis)));
      pragma
        Assert
          (M1 * Dot (N, F1)
           = N (X_Axis) * (M1 * F1 (X_Axis)) + N (Y_Axis) * (M1 * F1 (Y_Axis)) + N (Z_Axis) * (M1 * F1 (Z_Axis))
             + N (E_Axis) * (M1 * F1 (E_Axis)));
      pragma
        Assert
          (M2 * Dot (N, F2)
           = N (X_Axis) * (M2 * F2 (X_Axis)) + N (Y_Axis) * (M2 * F2 (Y_Axis)) + N (Z_Axis) * (M2 * F2 (Z_Axis))
             + N (E_Axis) * (M2 * F2 (E_Axis)));
      pragma
        Assert
          (M3 * Dot (N, F3)
           = N (X_Axis) * (M3 * F3 (X_Axis)) + N (Y_Axis) * (M3 * F3 (Y_Axis)) + N (Z_Axis) * (M3 * F3 (Z_Axis))
             + N (E_Axis) * (M3 * F3 (E_Axis)));
   end Lemma_Regroup;

   procedure Lemma_Drop_Terms (M0, M1, M2, M3, A0, A1, A2, A3 : Real)
   with Pre => A2 = 0.0 and then A3 = 0.0, Post => M0 * A0 + M1 * A1 + M2 * A2 + M3 * A3 = M0 * A0 + M1 * A1;

   procedure Lemma_Drop_Terms (M0, M1, M2, M3, A0, A1, A2, A3 : Real) is
   begin
      pragma Assert (M2 * A2 = 0.0);
      pragma Assert (M3 * A3 = 0.0);
   end Lemma_Drop_Terms;

   procedure Lemma_Same_Combination (M0, M1, M2, M3, A0, A1, A2, A3, B0, B1, B2, B3 : Real)
   with
     Pre  => A0 = B0 and then A1 = B1 and then A2 = B2 and then A3 = B3,
     Post => M0 * A0 + M1 * A1 + M2 * A2 + M3 * A3 = M0 * B0 + M1 * B1 + M2 * B2 + M3 * B3;

   procedure Lemma_Same_Combination (M0, M1, M2, M3, A0, A1, A2, A3, B0, B1, B2, B3 : Real) is null;

   procedure Lemma_Same_Squares (A1, A2, A3, A4, B1, B2, B3, B4 : Real)
   with
     Pre  => A1 = B1 and then A2 = B2 and then A3 = B3 and then A4 = B4,
     Post => A1 * A1 + A2 * A2 + A3 * A3 + A4 * A4 = B1 * B1 + B2 * B2 + B3 * B3 + B4 * B4;

   procedure Lemma_Same_Squares (A1, A2, A3, A4, B1, B2, B3, B4 : Real) is null;

   procedure Lemma_Scaled_Square (Quotients, Products : Real_Vector; Divisor : Real)
   with
     Pre  =>
       Divisor >= 1.0
       and then Quotients (X_Axis) * Divisor = Products (X_Axis)
       and then Quotients (Y_Axis) * Divisor = Products (Y_Axis)
       and then Quotients (Z_Axis) * Divisor = Products (Z_Axis)
       and then Quotients (E_Axis) * Divisor = Products (E_Axis),
     Post =>
       Divisor * Divisor > 0.0 and then Dot (Quotients, Quotients) * (Divisor * Divisor) = Dot (Products, Products);

   procedure Lemma_Scaled_Square (Quotients, Products : Real_Vector; Divisor : Real) is
   begin
      pragma Assert (Divisor * Divisor >= 1.0 * Divisor);
      pragma
        Assert
          ((Quotients (X_Axis) * Quotients (X_Axis)) * (Divisor * Divisor)
           = (Quotients (X_Axis) * Divisor) * (Quotients (X_Axis) * Divisor));
      pragma
        Assert
          ((Quotients (Y_Axis) * Quotients (Y_Axis)) * (Divisor * Divisor)
           = (Quotients (Y_Axis) * Divisor) * (Quotients (Y_Axis) * Divisor));
      pragma
        Assert
          ((Quotients (Z_Axis) * Quotients (Z_Axis)) * (Divisor * Divisor)
           = (Quotients (Z_Axis) * Divisor) * (Quotients (Z_Axis) * Divisor));
      pragma
        Assert
          ((Quotients (E_Axis) * Quotients (E_Axis)) * (Divisor * Divisor)
           = (Quotients (E_Axis) * Divisor) * (Quotients (E_Axis) * Divisor));
      pragma
        Assert
          (Dot (Quotients, Quotients) * (Divisor * Divisor)
           = (Quotients (X_Axis) * Quotients (X_Axis)) * (Divisor * Divisor)
             + (Quotients (Y_Axis) * Quotients (Y_Axis)) * (Divisor * Divisor)
             + (Quotients (Z_Axis) * Quotients (Z_Axis)) * (Divisor * Divisor)
             + (Quotients (E_Axis) * Quotients (E_Axis)) * (Divisor * Divisor));
   end Lemma_Scaled_Square;

   procedure Lemma_Scale_Dot (N, Quotients, Products : Real_Vector; Divisor : Real)
   with
     Pre  =>
       Quotients (X_Axis) * Divisor = Products (X_Axis)
       and then Quotients (Y_Axis) * Divisor = Products (Y_Axis)
       and then Quotients (Z_Axis) * Divisor = Products (Z_Axis)
       and then Quotients (E_Axis) * Divisor = Products (E_Axis),
     Post => Dot (N, Quotients) * Divisor = Dot (N, Products);

   procedure Lemma_Scale_Dot (N, Quotients, Products : Real_Vector; Divisor : Real) is
   begin
      pragma
        Assert
          (Dot (N, Quotients) * Divisor
           = N (X_Axis) * (Quotients (X_Axis) * Divisor) + N (Y_Axis) * (Quotients (Y_Axis) * Divisor)
             + N (Z_Axis) * (Quotients (Z_Axis) * Divisor)
             + N (E_Axis) * (Quotients (E_Axis) * Divisor));
   end Lemma_Scale_Dot;

   procedure Lemma_Sign_Of_Quotient (Value, Product, Divisor : Real)
   with Pre => Divisor > 0.0 and then Value * Divisor = Product and then Product >= 0.0, Post => Value >= 0.0;

   procedure Lemma_Sign_Of_Quotient (Value, Product, Divisor : Real) is
   begin
      if Value < 0.0 then
         pragma Assert (Value * Divisor < 0.0);
      end if;
   end Lemma_Sign_Of_Quotient;

   procedure Lemma_Cone_Combination (Start_Weight, Finish_Weight, S0, S1, F0, F1, G0, G1, Along_0, Along_1 : Real)
   with
     Pre  =>
       Start_Weight >= 0.0
       and then Finish_Weight >= 0.0
       and then G0 = Start_Weight * S0 + Finish_Weight * F0
       and then G1 = Start_Weight * S1 + Finish_Weight * F1
       and then S0 * Along_0 + S1 * Along_1 >= 0.0
       and then F0 * Along_0 + F1 * Along_1 >= 0.0,
     Post => G0 * Along_0 + G1 * Along_1 >= 0.0;
   --  A combination with nonnegative weights of two vectors on the inner side of a half-plane is on that side.

   procedure Lemma_Cone_Combination (Start_Weight, Finish_Weight, S0, S1, F0, F1, G0, G1, Along_0, Along_1 : Real) is
      Start_Part  : constant Real := S0 * Along_0 + S1 * Along_1;
      Finish_Part : constant Real := F0 * Along_0 + F1 * Along_1;
   begin
      pragma Assert (Start_Weight * Start_Part >= 0.0);
      pragma Assert (Finish_Weight * Finish_Part >= 0.0);
      pragma Assert (Start_Weight * Start_Part = Start_Weight * S0 * Along_0 + Start_Weight * S1 * Along_1);
      pragma Assert (Finish_Weight * Finish_Part = Finish_Weight * F0 * Along_0 + Finish_Weight * F1 * Along_1);
      pragma Assert (G0 * Along_0 = Start_Weight * S0 * Along_0 + Finish_Weight * F0 * Along_0);
      pragma Assert (G1 * Along_1 = Start_Weight * S1 * Along_1 + Finish_Weight * F1 * Along_1);
      pragma Assert (G0 * Along_0 + G1 * Along_1 = Start_Weight * Start_Part + Finish_Weight * Finish_Part);
   end Lemma_Cone_Combination;

   procedure Lemma_Normalized_Speed (Speed_Square, Numerator_Square, Divisor_Square, Bound : Real)
   with
     Pre  =>
       Divisor_Square > 0.0
       and then Speed_Square * Divisor_Square = Numerator_Square
       and then abs (Numerator_Square - Divisor_Square) <= Bound * Divisor_Square,
     Post => abs (Speed_Square - 1.0) <= Bound;
   --  Divide the unit-speed statement by the square of the denominator.

   procedure Lemma_Normalized_Speed (Speed_Square, Numerator_Square, Divisor_Square, Bound : Real) is
   begin
      pragma Assert ((Speed_Square - 1.0) * Divisor_Square = Numerator_Square - Divisor_Square);
      if Speed_Square - 1.0 > Bound then
         pragma Assert ((Speed_Square - 1.0) * Divisor_Square > Bound * Divisor_Square);
      end if;
      if 1.0 - Speed_Square > Bound then
         pragma Assert ((1.0 - Speed_Square) * Divisor_Square > Bound * Divisor_Square);
      end if;
   end Lemma_Normalized_Speed;

   procedure Lemma_Self_Bounded (Square, Bound_Square : Real)
   with
     Pre  => Square >= 0.0 and then Bound_Square >= 0.0 and then Square * Square <= Square * Bound_Square,
     Post => Square <= Bound_Square;
   --  A vector whose component along itself is within a bound is within that bound.

   procedure Lemma_Self_Bounded (Square, Bound_Square : Real) is
   begin
      if Square > Bound_Square then
         pragma Assert (Square > 0.0);
         pragma Assert (Square * (Square - Bound_Square) > 0.0);
         pragma Assert (Square * Square > Square * Bound_Square);
      end if;
   end Lemma_Self_Bounded;

   procedure Lemma_Speed (Ideal_Square, Cross, Error_Square, Defect, Total_Bound : Real)
   with
     Pre  =>
       abs (Ideal_Square - 1.0) <= 4.0 * Defect
       and then Total_Bound >= 0.0
       and then Cross * Cross <= Ideal_Square * (Total_Bound * Total_Bound)
       and then Error_Square >= 0.0
       and then Error_Square <= Total_Bound * Total_Bound,
     Post =>
       abs (Ideal_Square + 2.0 * Cross + Error_Square - 1.0)
       <= 4.0 * Defect + 2.0 * (1.0 + 2.0 * Defect) * Total_Bound + Total_Bound * Total_Bound;
   --  |I + D|² = |I|² + 2·I·D + |D|², with (I·D)² ≤ |I|²·|D|² ≤ (1 + 4·δ)·ε² ≤ ((1 + 2·δ)·ε)².

   procedure Lemma_Speed (Ideal_Square, Cross, Error_Square, Defect, Total_Bound : Real) is
      Total_Square : constant Real := Total_Bound * Total_Bound;
      Cross_Bound  : constant Real := (1.0 + 2.0 * Defect) * Total_Bound;
   begin
      pragma Assert (Defect >= 0.0);
      pragma Assert (Total_Square >= 0.0);
      pragma Assert (Ideal_Square <= 1.0 + 4.0 * Defect);
      pragma Assert (Ideal_Square * Total_Square <= (1.0 + 4.0 * Defect) * Total_Square);
      pragma Assert (Defect * Defect >= 0.0);
      pragma Assert (Cross_Bound * Cross_Bound = (1.0 + 4.0 * Defect + 4.0 * (Defect * Defect)) * Total_Square);
      pragma Assert ((Defect * Defect) * Total_Square >= 0.0);
      pragma
        Assert
          ((1.0 + 4.0 * Defect + 4.0 * (Defect * Defect)) * Total_Square
           = (1.0 + 4.0 * Defect) * Total_Square + 4.0 * ((Defect * Defect) * Total_Square));
      pragma Assert (Cross * Cross <= Cross_Bound * Cross_Bound);
      pragma Assert (Cross_Bound >= 0.0);
      Lemma_Root_Order (Cross, Cross_Bound);
      pragma Assert (2.0 * Cross_Bound = 2.0 * (1.0 + 2.0 * Defect) * Total_Bound);
      pragma Assert (abs (2.0 * Cross) <= 2.0 * Cross_Bound);
   end Lemma_Speed;

   procedure Lemma_Lower_Bound (Executed, Ideal, Norm_Square, Total_Bound, Length : Real)
   with
     Pre  =>
       (Executed - Ideal) * (Executed - Ideal) <= Norm_Square * (Total_Bound * Total_Bound)
       and then Norm_Square <= Length * Length
       and then Length >= 0.0
       and then Total_Bound >= 0.0
       and then Ideal >= 0.0,
     Post => Executed >= 0.0 - Length * Total_Bound;
   --  A component within Total_Bound times the direction's length of a nonnegative one is not below minus that.

   procedure Lemma_Lower_Bound (Executed, Ideal, Norm_Square, Total_Bound, Length : Real) is
      Total_Square : constant Real := Total_Bound * Total_Bound;
      Bound        : constant Real := Length * Total_Bound;
   begin
      pragma Assert (Total_Square >= 0.0);
      pragma Assert (Norm_Square * Total_Square <= (Length * Length) * Total_Square);
      pragma Assert (Bound * Bound = (Length * Length) * Total_Square);
      pragma Assert (Bound >= 0.0);
      pragma Assert ((Executed - Ideal) * (Executed - Ideal) <= Bound * Bound);
      Lemma_Root_Order (Executed - Ideal, Bound);
   end Lemma_Lower_Bound;

   ---------------------------------------------------------------------------
   --  The vectors which the theorems are about, as functions of the plain data in the certificates
   ---------------------------------------------------------------------------

   --  The lemmas of this part take the frame and the chart rather than the certificate which holds them: what
   --  is known of a certificate is large, and the provers do better without it.

   function Frame_Row (Frame : Frame_Vector_Array; Component : Frame_Component_Index) return Real_Vector
   is ([X_Axis => R (Frame (Component) (X_Axis)),
        Y_Axis => R (Frame (Component) (Y_Axis)),
        Z_Axis => R (Frame (Component) (Z_Axis)),
        E_Axis => R (Frame (Component) (E_Axis))]);
   --  One frame vector, as the real numbers which it denotes.

   function Chord_Vector (Chord : Position_Scale) return Real_Vector
   is ([X_Axis => R (Chord (X_Axis)),
        Y_Axis => R (Chord (Y_Axis)),
        Z_Axis => R (Chord (Z_Axis)),
        E_Axis => R (Chord (E_Axis))]);
   --  A stored direction, likewise.

   function Numerator_Vector (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real) return Real_Vector
   is ([X_Axis => Unit_Speed.Tangent_Numerator (Frame, Coefficients, X_Axis, V),
        Y_Axis => Unit_Speed.Tangent_Numerator (Frame, Coefficients, Y_Axis, V),
        Z_Axis => Unit_Speed.Tangent_Numerator (Frame, Coefficients, Z_Axis, V),
        E_Axis => Unit_Speed.Tangent_Numerator (Frame, Coefficients, E_Axis, V)]);
   --  The ideal tangent at chart coordinate V, times its denominator.

   function Tangent_Vector (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real) return Real_Vector
   is ([X_Axis => Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, X_Axis, V),
        Y_Axis => Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Y_Axis, V),
        Z_Axis => Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Z_Axis, V),
        E_Axis => Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, E_Axis, V)]);
   --  The ideal tangent at chart coordinate V.

   function Frame_Combination
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Real_Vector; V : Real) return Real
   is (Numerator (Coefficients, 0, V) * Dot (Direction, Frame_Row (Frame, 0))
       + Numerator (Coefficients, 1, V) * Dot (Direction, Frame_Row (Frame, 1))
       + Numerator (Coefficients, 2, V) * Dot (Direction, Frame_Row (Frame, 2))
       + Numerator (Coefficients, 3, V) * Dot (Direction, Frame_Row (Frame, 3)));
   --  The component along Direction of the tangent numerator at V, through its frame components.

   function Endpoint_Numerator (Value : Certificate; At_Finish : Boolean) return Real_Vector
   is (Numerator_Vector
         (Certified_Geometry.Frame (Geometry (Value)),
          Certified_Geometry.Coefficients (Geometry (Value)),
          (if At_Finish then Real'(1.0) else Real'(0.0))));

   --  Each of the next lemmas unfolds one definition and is proved on its own. The lemmas after them combine
   --  those facts through lemmas about plain vectors and never unfold a definition themselves: with the chart's
   --  polynomials in view the provers do not get through even the simplest arithmetic.

   procedure Lemma_Numerator_Is_Tangent_Numerator
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real)
   with
     Post =>
       Numerator_Vector (Frame, Coefficients, V) (Axis) = Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, V);

   procedure Lemma_Numerator_Is_Tangent_Numerator
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real)
   is null;

   procedure Lemma_Tangent_Numerator
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real)
   with
     Post =>
       Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, V)
       = Numerator (Coefficients, 0, V) * R (Frame (0) (Axis)) + Numerator (Coefficients, 1, V) * R (Frame (1) (Axis))
         + Numerator (Coefficients, 2, V) * R (Frame (2) (Axis))
         + Numerator (Coefficients, 3, V) * R (Frame (3) (Axis));

   procedure Lemma_Tangent_Numerator
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real)
   is null;

   procedure Lemma_Frame_Row (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Axis : Axis_Name)
   with Post => Frame_Row (Frame, Component) (Axis) = R (Frame (Component) (Axis));

   procedure Lemma_Frame_Row (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Axis : Axis_Name) is null;

   procedure Lemma_Numerator_Component
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real)
   with
     Post =>
       Numerator_Vector (Frame, Coefficients, V) (Axis)
       = Numerator (Coefficients, 0, V) * Frame_Row (Frame, 0) (Axis)
         + Numerator (Coefficients, 1, V) * Frame_Row (Frame, 1) (Axis)
         + Numerator (Coefficients, 2, V) * Frame_Row (Frame, 2) (Axis)
         + Numerator (Coefficients, 3, V) * Frame_Row (Frame, 3) (Axis);

   procedure Lemma_Numerator_Component
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real) is
   begin
      Lemma_Numerator_Is_Tangent_Numerator (Frame, Coefficients, Axis, V);
      Lemma_Tangent_Numerator (Frame, Coefficients, Axis, V);
      Lemma_Frame_Row (Frame, 0, Axis);
      Lemma_Frame_Row (Frame, 1, Axis);
      Lemma_Frame_Row (Frame, 2, Axis);
      Lemma_Frame_Row (Frame, 3, Axis);
      Lemma_Same_Combination
        (Numerator (Coefficients, 0, V),
         Numerator (Coefficients, 1, V),
         Numerator (Coefficients, 2, V),
         Numerator (Coefficients, 3, V),
         R (Frame (0) (Axis)),
         R (Frame (1) (Axis)),
         R (Frame (2) (Axis)),
         R (Frame (3) (Axis)),
         Frame_Row (Frame, 0) (Axis),
         Frame_Row (Frame, 1) (Axis),
         Frame_Row (Frame, 2) (Axis),
         Frame_Row (Frame, 3) (Axis));
   end Lemma_Numerator_Component;

   procedure Lemma_Numerator_Components (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real)
   with
     Post =>
       Numerator_Vector (Frame, Coefficients, V) (X_Axis)
       = Numerator (Coefficients, 0, V) * Frame_Row (Frame, 0) (X_Axis)
         + Numerator (Coefficients, 1, V) * Frame_Row (Frame, 1) (X_Axis)
         + Numerator (Coefficients, 2, V) * Frame_Row (Frame, 2) (X_Axis)
         + Numerator (Coefficients, 3, V) * Frame_Row (Frame, 3) (X_Axis)
       and then
         Numerator_Vector (Frame, Coefficients, V) (Y_Axis)
         = Numerator (Coefficients, 0, V) * Frame_Row (Frame, 0) (Y_Axis)
           + Numerator (Coefficients, 1, V) * Frame_Row (Frame, 1) (Y_Axis)
           + Numerator (Coefficients, 2, V) * Frame_Row (Frame, 2) (Y_Axis)
           + Numerator (Coefficients, 3, V) * Frame_Row (Frame, 3) (Y_Axis)
       and then
         Numerator_Vector (Frame, Coefficients, V) (Z_Axis)
         = Numerator (Coefficients, 0, V) * Frame_Row (Frame, 0) (Z_Axis)
           + Numerator (Coefficients, 1, V) * Frame_Row (Frame, 1) (Z_Axis)
           + Numerator (Coefficients, 2, V) * Frame_Row (Frame, 2) (Z_Axis)
           + Numerator (Coefficients, 3, V) * Frame_Row (Frame, 3) (Z_Axis)
       and then
         Numerator_Vector (Frame, Coefficients, V) (E_Axis)
         = Numerator (Coefficients, 0, V) * Frame_Row (Frame, 0) (E_Axis)
           + Numerator (Coefficients, 1, V) * Frame_Row (Frame, 1) (E_Axis)
           + Numerator (Coefficients, 2, V) * Frame_Row (Frame, 2) (E_Axis)
           + Numerator (Coefficients, 3, V) * Frame_Row (Frame, 3) (E_Axis);
   --  The tangent numerator is the combination of the frame vectors by the frame components.

   procedure Lemma_Numerator_Components (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real) is
   begin
      Lemma_Numerator_Component (Frame, Coefficients, X_Axis, V);
      Lemma_Numerator_Component (Frame, Coefficients, Y_Axis, V);
      Lemma_Numerator_Component (Frame, Coefficients, Z_Axis, V);
      Lemma_Numerator_Component (Frame, Coefficients, E_Axis, V);
   end Lemma_Numerator_Components;

   procedure Lemma_Tangent_Component
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real)
   with
     Post =>
       Tangent_Vector (Frame, Coefficients, V) (Axis) * Denominator (Coefficients, V)
       = Numerator_Vector (Frame, Coefficients, V) (Axis);

   procedure Lemma_Tangent_Component
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real)
   is null;

   procedure Lemma_Tangent_Times_Denominator (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real)
   with
     Post =>
       Denominator (Coefficients, V) >= 1.0
       and then
         Tangent_Vector (Frame, Coefficients, V) (X_Axis) * Denominator (Coefficients, V)
         = Numerator_Vector (Frame, Coefficients, V) (X_Axis)
       and then
         Tangent_Vector (Frame, Coefficients, V) (Y_Axis) * Denominator (Coefficients, V)
         = Numerator_Vector (Frame, Coefficients, V) (Y_Axis)
       and then
         Tangent_Vector (Frame, Coefficients, V) (Z_Axis) * Denominator (Coefficients, V)
         = Numerator_Vector (Frame, Coefficients, V) (Z_Axis)
       and then
         Tangent_Vector (Frame, Coefficients, V) (E_Axis) * Denominator (Coefficients, V)
         = Numerator_Vector (Frame, Coefficients, V) (E_Axis);
   --  The tangent is its numerator over the denominator, which is at least one.

   procedure Lemma_Tangent_Times_Denominator (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real) is
   begin
      Lemma_Tangent_Component (Frame, Coefficients, X_Axis, V);
      Lemma_Tangent_Component (Frame, Coefficients, Y_Axis, V);
      Lemma_Tangent_Component (Frame, Coefficients, Z_Axis, V);
      Lemma_Tangent_Component (Frame, Coefficients, E_Axis, V);
   end Lemma_Tangent_Times_Denominator;

   procedure Lemma_Speed_Numerator_Definition (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real)
   with
     Post =>
       Unit_Speed.Speed_Numerator_Square (Frame, Coefficients, V)
       = Unit_Speed.Tangent_Numerator (Frame, Coefficients, X_Axis, V)
         * Unit_Speed.Tangent_Numerator (Frame, Coefficients, X_Axis, V)
         + Unit_Speed.Tangent_Numerator (Frame, Coefficients, Y_Axis, V)
           * Unit_Speed.Tangent_Numerator (Frame, Coefficients, Y_Axis, V)
         + Unit_Speed.Tangent_Numerator (Frame, Coefficients, Z_Axis, V)
           * Unit_Speed.Tangent_Numerator (Frame, Coefficients, Z_Axis, V)
         + Unit_Speed.Tangent_Numerator (Frame, Coefficients, E_Axis, V)
           * Unit_Speed.Tangent_Numerator (Frame, Coefficients, E_Axis, V);

   procedure Lemma_Speed_Numerator_Definition (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real)
   is null;

   procedure Lemma_Speed_Numerator (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real)
   with
     Post =>
       Unit_Speed.Speed_Numerator_Square (Frame, Coefficients, V)
       = Dot (Numerator_Vector (Frame, Coefficients, V), Numerator_Vector (Frame, Coefficients, V));

   procedure Lemma_Speed_Numerator (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real) is
   begin
      Lemma_Speed_Numerator_Definition (Frame, Coefficients, V);
      Lemma_Numerator_Is_Tangent_Numerator (Frame, Coefficients, X_Axis, V);
      Lemma_Numerator_Is_Tangent_Numerator (Frame, Coefficients, Y_Axis, V);
      Lemma_Numerator_Is_Tangent_Numerator (Frame, Coefficients, Z_Axis, V);
      Lemma_Numerator_Is_Tangent_Numerator (Frame, Coefficients, E_Axis, V);
      Lemma_Same_Squares
        (Numerator_Vector (Frame, Coefficients, V) (X_Axis),
         Numerator_Vector (Frame, Coefficients, V) (Y_Axis),
         Numerator_Vector (Frame, Coefficients, V) (Z_Axis),
         Numerator_Vector (Frame, Coefficients, V) (E_Axis),
         Unit_Speed.Tangent_Numerator (Frame, Coefficients, X_Axis, V),
         Unit_Speed.Tangent_Numerator (Frame, Coefficients, Y_Axis, V),
         Unit_Speed.Tangent_Numerator (Frame, Coefficients, Z_Axis, V),
         Unit_Speed.Tangent_Numerator (Frame, Coefficients, E_Axis, V));
   end Lemma_Speed_Numerator;

   procedure Lemma_Frame_Combination
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Real_Vector; V : Real)
   with
     Post =>
       Frame_Combination (Frame, Coefficients, Direction, V)
       = Numerator (Coefficients, 0, V) * Dot (Direction, Frame_Row (Frame, 0))
         + Numerator (Coefficients, 1, V) * Dot (Direction, Frame_Row (Frame, 1))
         + Numerator (Coefficients, 2, V) * Dot (Direction, Frame_Row (Frame, 2))
         + Numerator (Coefficients, 3, V) * Dot (Direction, Frame_Row (Frame, 3));

   procedure Lemma_Frame_Combination
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Real_Vector; V : Real)
   is null;

   procedure Lemma_Projected_Numerator
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Chord : Position_Scale; V : Real)
   with
     Post =>
       Projected_Numerator (Frame, Coefficients, Chord, V)
       = Numerator (Coefficients, 0, V) * Dot_Real (Frame (0), Chord)
         + Numerator (Coefficients, 1, V) * Dot_Real (Frame (1), Chord)
         + Numerator (Coefficients, 2, V) * Dot_Real (Frame (2), Chord)
         + Numerator (Coefficients, 3, V) * Dot_Real (Frame (3), Chord);

   procedure Lemma_Projected_Numerator
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Chord : Position_Scale; V : Real)
   is null;

   procedure Lemma_Chord_Along_Frame
     (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Chord : Position_Scale)
   with Post => Dot (Chord_Vector (Chord), Frame_Row (Frame, Component)) = Dot_Real (Frame (Component), Chord);

   procedure Lemma_Chord_Along_Frame
     (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Chord : Position_Scale)
   is null;

   procedure Lemma_Chord_Square (Chord : Position_Scale)
   with Post => Dot (Chord_Vector (Chord), Chord_Vector (Chord)) = Dot_Real (Chord, Chord);

   procedure Lemma_Chord_Square (Chord : Position_Scale) is null;

   procedure Lemma_Dot_Numerator
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Real_Vector; V : Real)
   with
     Post =>
       Dot (Direction, Numerator_Vector (Frame, Coefficients, V))
       = Frame_Combination (Frame, Coefficients, Direction, V);

   procedure Lemma_Dot_Numerator
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Real_Vector; V : Real) is
   begin
      Lemma_Numerator_Components (Frame, Coefficients, V);
      Lemma_Regroup
        (Direction,
         Numerator_Vector (Frame, Coefficients, V),
         Frame_Row (Frame, 0),
         Frame_Row (Frame, 1),
         Frame_Row (Frame, 2),
         Frame_Row (Frame, 3),
         Numerator (Coefficients, 0, V),
         Numerator (Coefficients, 1, V),
         Numerator (Coefficients, 2, V),
         Numerator (Coefficients, 3, V));
      Lemma_Frame_Combination (Frame, Coefficients, Direction, V);
   end Lemma_Dot_Numerator;

   procedure Lemma_Dot_Tangent
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Real_Vector; V : Real)
   with
     Post =>
       Denominator (Coefficients, V) >= 1.0
       and then
         Dot (Direction, Tangent_Vector (Frame, Coefficients, V)) * Denominator (Coefficients, V)
         = Frame_Combination (Frame, Coefficients, Direction, V);
   --  The component of the ideal tangent along Direction, times the denominator, through its frame components.

   procedure Lemma_Dot_Tangent
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Real_Vector; V : Real) is
   begin
      Lemma_Tangent_Times_Denominator (Frame, Coefficients, V);
      Lemma_Scale_Dot
        (Direction,
         Tangent_Vector (Frame, Coefficients, V),
         Numerator_Vector (Frame, Coefficients, V),
         Denominator (Coefficients, V));
      Lemma_Dot_Numerator (Frame, Coefficients, Direction, V);
   end Lemma_Dot_Tangent;

   procedure Lemma_Tangent_Speed (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real; Bound : Real)
   with
     Pre  =>
       abs (Unit_Speed.Speed_Numerator_Square (Frame, Coefficients, V)
            - Denominator (Coefficients, V) * Denominator (Coefficients, V))
       <= Bound * (Denominator (Coefficients, V) * Denominator (Coefficients, V)),
     Post =>
       abs (Dot (Tangent_Vector (Frame, Coefficients, V), Tangent_Vector (Frame, Coefficients, V)) - 1.0) <= Bound;
   --  The unit-speed statement of the geometry certificate, divided by the square of the denominator.

   procedure Lemma_Tangent_Speed (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real; Bound : Real)
   is
   begin
      Lemma_Tangent_Times_Denominator (Frame, Coefficients, V);
      Lemma_Speed_Numerator (Frame, Coefficients, V);
      Lemma_Scaled_Square
        (Tangent_Vector (Frame, Coefficients, V),
         Numerator_Vector (Frame, Coefficients, V),
         Denominator (Coefficients, V));
      Lemma_Normalized_Speed
        (Speed_Square     => Dot (Tangent_Vector (Frame, Coefficients, V), Tangent_Vector (Frame, Coefficients, V)),
         Numerator_Square => Unit_Speed.Speed_Numerator_Square (Frame, Coefficients, V),
         Divisor_Square   => Denominator (Coefficients, V) * Denominator (Coefficients, V),
         Bound            => Bound);
   end Lemma_Tangent_Speed;

   ---------------------------------------------------------------------------
   --  The theorems, for plain data
   ---------------------------------------------------------------------------

   procedure Lemma_Tangent_Core
     (Direction       : Real_Vector;
      Ideal           : Real_Vector;
      Retained        : Real_Vector;
      Executed        : Real_Vector;
      Finish          : Dimensionless_Axis_Vector;
      Uncorrected     : Dimensionless_Axis_Vector;
      Length_Value    : Dimensionless;
      Rate_Value      : Real;
      Tangent_Bound   : Real;
      Total_Bound     : Real;
      Distance_Square : Real;
      Executed_Along  : Real;
      Ideal_Along     : Real)
   with
     Pre  =>
       Length_Value > 0.0
       and then 0.0 <= Rate_Value
       and then Rate_Value <= Largest_Rate
       and then 0.0 <= Tangent_Bound
       and then Tangent_Bound <= Total_Bound
       and then
         Distance_Square
         = (Ideal (X_Axis) - Retained (X_Axis)) * (Ideal (X_Axis) - Retained (X_Axis))
           + (Ideal (Y_Axis) - Retained (Y_Axis)) * (Ideal (Y_Axis) - Retained (Y_Axis))
           + (Ideal (Z_Axis) - Retained (Z_Axis)) * (Ideal (Z_Axis) - Retained (Z_Axis))
           + (Ideal (E_Axis) - Retained (E_Axis)) * (Ideal (E_Axis) - Retained (E_Axis))
       and then Distance_Square <= Tangent_Bound * Tangent_Bound
       and then
         Executed (X_Axis)
         = Retained (X_Axis) + Rate_Value * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value)
       and then
         Executed (Y_Axis)
         = Retained (Y_Axis) + Rate_Value * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value)
       and then
         Executed (Z_Axis)
         = Retained (Z_Axis) + Rate_Value * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value)
       and then
         Executed (E_Axis)
         = Retained (E_Axis) + Rate_Value * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value)
       and then
         (Largest_Rate * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
         * (Largest_Rate * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value))
         <= (Total_Bound - Tangent_Bound) * (Total_Bound - Tangent_Bound)
       and then Executed_Along = Dot (Direction, Executed)
       and then Ideal_Along = Dot (Direction, Ideal),
     Post =>
       (Executed_Along - Ideal_Along) * (Executed_Along - Ideal_Along)
       <= Dot (Direction, Direction) * (Total_Bound * Total_Bound);
   --  With the retained tangent within Tangent_Bound of the ideal one, and the largest tangent which the
   --  correction can add within the rest of Total_Bound, the retained tangent plus the correction's tangent is
   --  within Total_Bound of the ideal tangent along every direction.

   procedure Lemma_Tangent_Core
     (Direction       : Real_Vector;
      Ideal           : Real_Vector;
      Retained        : Real_Vector;
      Executed        : Real_Vector;
      Finish          : Dimensionless_Axis_Vector;
      Uncorrected     : Dimensionless_Axis_Vector;
      Length_Value    : Dimensionless;
      Rate_Value      : Real;
      Tangent_Bound   : Real;
      Total_Bound     : Real;
      Distance_Square : Real;
      Executed_Along  : Real;
      Ideal_Along     : Real)
   is
      --  The retained tangent's error, the correction's tangent, and their sum.
      E : constant Real_Vector :=
        [X_Axis => Retained (X_Axis) - Ideal (X_Axis),
         Y_Axis => Retained (Y_Axis) - Ideal (Y_Axis),
         Z_Axis => Retained (Z_Axis) - Ideal (Z_Axis),
         E_Axis => Retained (E_Axis) - Ideal (E_Axis)];
      C : constant Real_Vector :=
        [X_Axis => Rate_Value * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value),
         Y_Axis => Rate_Value * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value),
         Z_Axis => Rate_Value * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value),
         E_Axis => Rate_Value * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value)];
      D : constant Real_Vector :=
        [X_Axis => Executed (X_Axis) - Ideal (X_Axis),
         Y_Axis => Executed (Y_Axis) - Ideal (Y_Axis),
         Z_Axis => Executed (Z_Axis) - Ideal (Z_Axis),
         E_Axis => Executed (E_Axis) - Ideal (E_Axis)];
   begin
      pragma Assert (R (Length_Value) > 0.0);

      --  The retained tangent is within the certified error of the ideal one.
      pragma
        Assert (E (X_Axis) * E (X_Axis) = (Ideal (X_Axis) - Retained (X_Axis)) * (Ideal (X_Axis) - Retained (X_Axis)));
      pragma
        Assert (E (Y_Axis) * E (Y_Axis) = (Ideal (Y_Axis) - Retained (Y_Axis)) * (Ideal (Y_Axis) - Retained (Y_Axis)));
      pragma
        Assert (E (Z_Axis) * E (Z_Axis) = (Ideal (Z_Axis) - Retained (Z_Axis)) * (Ideal (Z_Axis) - Retained (Z_Axis)));
      pragma
        Assert (E (E_Axis) * E (E_Axis) = (Ideal (E_Axis) - Retained (E_Axis)) * (Ideal (E_Axis) - Retained (E_Axis)));
      pragma Assert (Dot (E, E) = Distance_Square);
      pragma Assert (Dot (E, E) <= Tangent_Bound * Tangent_Bound);

      --  The correction adds at most what the total bound allows for it.
      Lemma_Rate_Scaled (Rate_Value, R (Finish (X_Axis)) - R (Uncorrected (X_Axis)), R (Length_Value));
      Lemma_Rate_Scaled (Rate_Value, R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis)), R (Length_Value));
      Lemma_Rate_Scaled (Rate_Value, R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis)), R (Length_Value));
      Lemma_Rate_Scaled (Rate_Value, R (Finish (E_Axis)) - R (Uncorrected (E_Axis)), R (Length_Value));
      pragma Assert (Dot (C, C) <= (Total_Bound - Tangent_Bound) * (Total_Bound - Tangent_Bound));

      pragma Assert (D (X_Axis) = E (X_Axis) + C (X_Axis));
      pragma Assert (D (Y_Axis) = E (Y_Axis) + C (Y_Axis));
      pragma Assert (D (Z_Axis) = E (Z_Axis) + C (Z_Axis));
      pragma Assert (D (E_Axis) = E (E_Axis) + C (E_Axis));
      Lemma_Combined_Error (Direction, D, E, C, Tangent_Bound, Total_Bound);

      --  The difference of the two components along Direction is the component of the difference.
      Lemma_Dot_Of_Difference (Direction, D, Executed, Ideal);
      pragma Assert (Dot (Direction, D) = Executed_Along - Ideal_Along);
   end Lemma_Tangent_Core;

   procedure Lemma_Cone_Core (Geometry : Certified_Geometry.Certificate; Normal : Real_Vector; V : Real)
   with
     Pre  =>
       Certified_Geometry.Kind (Geometry) = Certified_Geometry.In_Cone
       and then 0.0 <= V
       and then V <= 1.0
       and then Dot (Normal, Frame_Row (Certified_Geometry.Frame (Geometry), 2)) = 0.0
       and then Dot (Normal, Frame_Row (Certified_Geometry.Frame (Geometry), 3)) = 0.0
       and then
         Dot
           (Normal,
            Numerator_Vector (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), 0.0))
         >= 0.0
       and then
         Dot
           (Normal,
            Numerator_Vector (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), 1.0))
         >= 0.0,
     Post =>
       Dot
         (Normal, Tangent_Vector (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), V))
       >= 0.0;
   --  The ideal tangent's part in the frame plane is a combination of the endpoint tangents with nonnegative
   --  weights, and an inward normal of their cone sees only that part: its component along it is nonnegative.

   procedure Lemma_Cone_Weights
     (Geometry : Certified_Geometry.Certificate; V : Real; Start_Share, Finish_Share : out Real)
   with
     Pre  => Certified_Geometry.Kind (Geometry) = Certified_Geometry.In_Cone and then 0.0 <= V and then V <= 1.0,
     Post =>
       Start_Share >= 0.0
       and then Finish_Share >= 0.0
       and then
         Numerator (Certified_Geometry.Coefficients (Geometry), 0, V)
         = Start_Share * Numerator (Certified_Geometry.Coefficients (Geometry), 0, 0.0)
           + Finish_Share * Numerator (Certified_Geometry.Coefficients (Geometry), 0, 1.0)
       and then
         Numerator (Certified_Geometry.Coefficients (Geometry), 1, V)
         = Start_Share * Numerator (Certified_Geometry.Coefficients (Geometry), 1, 0.0)
           + Finish_Share * Numerator (Certified_Geometry.Coefficients (Geometry), 1, 1.0);
   --  The weights of the geometry certificate's cone theorem, as two numbers: what else that theorem states
   --  would only burden the provers where the weights are used.

   procedure Lemma_Cone_Weights
     (Geometry : Certified_Geometry.Certificate; V : Real; Start_Share, Finish_Share : out Real) is
   begin
      Certified_Geometry.Lemma_In_Cone (Geometry, V);
      Start_Share := Start_Weight (Certified_Geometry.Coefficients (Geometry), V);
      Finish_Share := Finish_Weight (Certified_Geometry.Coefficients (Geometry), V);
   end Lemma_Cone_Weights;

   procedure Lemma_Cone_Core (Geometry : Certified_Geometry.Certificate; Normal : Real_Vector; V : Real) is
      Along_0      : constant Real := Dot (Normal, Frame_Row (Certified_Geometry.Frame (Geometry), 0));
      Along_1      : constant Real := Dot (Normal, Frame_Row (Certified_Geometry.Frame (Geometry), 1));
      Along_2      : constant Real := Dot (Normal, Frame_Row (Certified_Geometry.Frame (Geometry), 2));
      Along_3      : constant Real := Dot (Normal, Frame_Row (Certified_Geometry.Frame (Geometry), 3));
      Start_Share  : Real;
      Finish_Share : Real;
   begin
      Lemma_Cone_Weights (Geometry, V, Start_Share, Finish_Share);

      --  Normal sees only the first two frame components, at the start, at the finish and at V.
      Lemma_Dot_Numerator
        (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Normal, 0.0);
      Lemma_Frame_Combination
        (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Normal, 0.0);
      Lemma_Drop_Terms
        (Numerator (Certified_Geometry.Coefficients (Geometry), 0, 0.0),
         Numerator (Certified_Geometry.Coefficients (Geometry), 1, 0.0),
         Numerator (Certified_Geometry.Coefficients (Geometry), 2, 0.0),
         Numerator (Certified_Geometry.Coefficients (Geometry), 3, 0.0),
         Along_0,
         Along_1,
         Along_2,
         Along_3);
      Lemma_Dot_Numerator
        (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Normal, 1.0);
      Lemma_Frame_Combination
        (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Normal, 1.0);
      Lemma_Drop_Terms
        (Numerator (Certified_Geometry.Coefficients (Geometry), 0, 1.0),
         Numerator (Certified_Geometry.Coefficients (Geometry), 1, 1.0),
         Numerator (Certified_Geometry.Coefficients (Geometry), 2, 1.0),
         Numerator (Certified_Geometry.Coefficients (Geometry), 3, 1.0),
         Along_0,
         Along_1,
         Along_2,
         Along_3);
      Lemma_Dot_Tangent (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Normal, V);
      Lemma_Frame_Combination
        (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Normal, V);
      Lemma_Drop_Terms
        (Numerator (Certified_Geometry.Coefficients (Geometry), 0, V),
         Numerator (Certified_Geometry.Coefficients (Geometry), 1, V),
         Numerator (Certified_Geometry.Coefficients (Geometry), 2, V),
         Numerator (Certified_Geometry.Coefficients (Geometry), 3, V),
         Along_0,
         Along_1,
         Along_2,
         Along_3);

      --  There it is the same combination of the endpoint tangents as the tangent is.
      Lemma_Cone_Combination
        (Start_Weight  => Start_Share,
         Finish_Weight => Finish_Share,
         S0            => Numerator (Certified_Geometry.Coefficients (Geometry), 0, 0.0),
         S1            => Numerator (Certified_Geometry.Coefficients (Geometry), 1, 0.0),
         F0            => Numerator (Certified_Geometry.Coefficients (Geometry), 0, 1.0),
         F1            => Numerator (Certified_Geometry.Coefficients (Geometry), 1, 1.0),
         G0            => Numerator (Certified_Geometry.Coefficients (Geometry), 0, V),
         G1            => Numerator (Certified_Geometry.Coefficients (Geometry), 1, V),
         Along_0       => Along_0,
         Along_1       => Along_1);
      Lemma_Sign_Of_Quotient
        (Dot
           (Normal,
            Tangent_Vector (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), V)),
         Frame_Combination
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Normal, V),
         Denominator (Certified_Geometry.Coefficients (Geometry), V));
   end Lemma_Cone_Core;

   procedure Lemma_Chord_Core (Geometry : Certified_Geometry.Certificate; V : Real)
   with
     Pre  =>
       Certified_Geometry.Kind (Geometry) = Certified_Geometry.Chord_Progress and then 0.0 <= V and then V <= 1.0,
     Post =>
       Dot
         (Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)),
          Tangent_Vector (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), V))
       >= 0.0;
   --  The ideal tangent never travels backwards along the chord direction.

   procedure Lemma_Chord_Core (Geometry : Certified_Geometry.Certificate; V : Real) is
   begin
      Certified_Geometry.Lemma_Chord_Progress (Geometry, V);
      Lemma_Projected_Numerator
        (Certified_Geometry.Frame (Geometry),
         Certified_Geometry.Coefficients (Geometry),
         Certified_Geometry.Chord_Direction (Geometry),
         V);
      Lemma_Chord_Along_Frame (Certified_Geometry.Frame (Geometry), 0, Certified_Geometry.Chord_Direction (Geometry));
      Lemma_Chord_Along_Frame (Certified_Geometry.Frame (Geometry), 1, Certified_Geometry.Chord_Direction (Geometry));
      Lemma_Chord_Along_Frame (Certified_Geometry.Frame (Geometry), 2, Certified_Geometry.Chord_Direction (Geometry));
      Lemma_Chord_Along_Frame (Certified_Geometry.Frame (Geometry), 3, Certified_Geometry.Chord_Direction (Geometry));
      Lemma_Frame_Combination
        (Certified_Geometry.Frame (Geometry),
         Certified_Geometry.Coefficients (Geometry),
         Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)),
         V);
      Lemma_Same_Combination
        (Numerator (Certified_Geometry.Coefficients (Geometry), 0, V),
         Numerator (Certified_Geometry.Coefficients (Geometry), 1, V),
         Numerator (Certified_Geometry.Coefficients (Geometry), 2, V),
         Numerator (Certified_Geometry.Coefficients (Geometry), 3, V),
         Dot
           (Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)),
            Frame_Row (Certified_Geometry.Frame (Geometry), 0)),
         Dot
           (Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)),
            Frame_Row (Certified_Geometry.Frame (Geometry), 1)),
         Dot
           (Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)),
            Frame_Row (Certified_Geometry.Frame (Geometry), 2)),
         Dot
           (Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)),
            Frame_Row (Certified_Geometry.Frame (Geometry), 3)),
         Dot_Real (Certified_Geometry.Frame (Geometry) (0), Certified_Geometry.Chord_Direction (Geometry)),
         Dot_Real (Certified_Geometry.Frame (Geometry) (1), Certified_Geometry.Chord_Direction (Geometry)),
         Dot_Real (Certified_Geometry.Frame (Geometry) (2), Certified_Geometry.Chord_Direction (Geometry)),
         Dot_Real (Certified_Geometry.Frame (Geometry) (3), Certified_Geometry.Chord_Direction (Geometry)));
      Lemma_Dot_Tangent
        (Certified_Geometry.Frame (Geometry),
         Certified_Geometry.Coefficients (Geometry),
         Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)),
         V);
      Lemma_Sign_Of_Quotient
        (Dot
           (Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)),
            Tangent_Vector (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), V)),
         Frame_Combination
           (Certified_Geometry.Frame (Geometry),
            Certified_Geometry.Coefficients (Geometry),
            Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)),
            V),
         Denominator (Certified_Geometry.Coefficients (Geometry), V));
   end Lemma_Chord_Core;

   procedure Lemma_Ideal_Speed (Geometry : Certified_Geometry.Certificate; V : Real)
   with
     Pre  =>
       Certified_Geometry.Kind (Geometry) /= Certified_Geometry.Not_Certified
       and then Certified_Geometry.Frame_Defect (Geometry) < Dimensionless'Last,
     Post =>
       abs (Dot
              (Tangent_Vector (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), V),
               Tangent_Vector (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), V))
            - 1.0)
       <= 4.0 * (R (Certified_Geometry.Frame_Defect (Geometry)) + Defect_Allowance);
   --  The ideal tangent has unit speed up to the frame's defect.

   procedure Lemma_Ideal_Speed (Geometry : Certified_Geometry.Certificate; V : Real) is
   begin
      Certified_Geometry.Lemma_Unit_Speed (Geometry, V);
      Lemma_Tangent_Speed
        (Certified_Geometry.Frame (Geometry),
         Certified_Geometry.Coefficients (Geometry),
         V,
         4.0 * (R (Certified_Geometry.Frame_Defect (Geometry)) + Defect_Allowance));
   end Lemma_Ideal_Speed;

   procedure Lemma_Speed_Core
     (Ideal                     : Real_Vector;
      Executed                  : Real_Vector;
      Difference                : Real_Vector;
      Defect                    : Real;
      Total_Bound               : Real;
      Executed_Along_Difference : Real;
      Ideal_Along_Difference    : Real;
      Executed_Along_Ideal      : Real;
      Ideal_Along_Ideal         : Real)
   with
     Pre  =>
       Difference (X_Axis) = Executed (X_Axis) - Ideal (X_Axis)
       and then Difference (Y_Axis) = Executed (Y_Axis) - Ideal (Y_Axis)
       and then Difference (Z_Axis) = Executed (Z_Axis) - Ideal (Z_Axis)
       and then Difference (E_Axis) = Executed (E_Axis) - Ideal (E_Axis)
       and then Total_Bound >= 0.0
       and then abs (Dot (Ideal, Ideal) - 1.0) <= 4.0 * Defect
       and then Executed_Along_Difference = Dot (Difference, Executed)
       and then Ideal_Along_Difference = Dot (Difference, Ideal)
       and then Executed_Along_Ideal = Dot (Ideal, Executed)
       and then Ideal_Along_Ideal = Dot (Ideal, Ideal)
       and then
         (Executed_Along_Difference - Ideal_Along_Difference) * (Executed_Along_Difference - Ideal_Along_Difference)
         <= Dot (Difference, Difference) * (Total_Bound * Total_Bound)
       and then
         (Executed_Along_Ideal - Ideal_Along_Ideal) * (Executed_Along_Ideal - Ideal_Along_Ideal)
         <= Dot (Ideal, Ideal) * (Total_Bound * Total_Bound),
     Post =>
       abs (Dot (Executed, Executed) - 1.0)
       <= 4.0 * Defect + 2.0 * (1.0 + 2.0 * Defect) * Total_Bound + Total_Bound * Total_Bound;
   --  A vector within Total_Bound, along itself and along the other, of one whose squared length is within
   --  4·δ of one has a squared length within 4·δ + 2·(1 + 2·δ)·ε + ε² of one.

   procedure Lemma_Speed_Core
     (Ideal                     : Real_Vector;
      Executed                  : Real_Vector;
      Difference                : Real_Vector;
      Defect                    : Real;
      Total_Bound               : Real;
      Executed_Along_Difference : Real;
      Ideal_Along_Difference    : Real;
      Executed_Along_Ideal      : Real;
      Ideal_Along_Ideal         : Real)
   is
      Total_Square : constant Real := Total_Bound * Total_Bound;
      Ideal_Square : constant Real := Dot (Ideal, Ideal);
      Cross        : constant Real := Dot (Ideal, Difference);
      Error_Square : constant Real := Dot (Difference, Difference);
   begin
      pragma Assert (Total_Square >= 0.0);

      --  The difference is within Total_Bound along itself, hence within Total_Bound.
      Lemma_Dot_Of_Difference (Difference, Difference, Executed, Ideal);
      pragma Assert (Executed_Along_Difference - Ideal_Along_Difference = Error_Square);
      pragma Assert (Error_Square * Error_Square <= Error_Square * Total_Square);
      pragma Assert (Difference (X_Axis) * Difference (X_Axis) >= 0.0);
      pragma Assert (Difference (Y_Axis) * Difference (Y_Axis) >= 0.0);
      pragma Assert (Difference (Z_Axis) * Difference (Z_Axis) >= 0.0);
      pragma Assert (Difference (E_Axis) * Difference (E_Axis) >= 0.0);
      pragma Assert (Error_Square >= 0.0);
      Lemma_Self_Bounded (Error_Square, Total_Square);

      --  It is within Total_Bound times the other's length along the other.
      Lemma_Dot_Of_Difference (Ideal, Difference, Executed, Ideal);
      pragma Assert (Executed_Along_Ideal - Ideal_Along_Ideal = Cross);
      pragma Assert (Cross * Cross <= Ideal_Square * Total_Square);

      Lemma_Speed (Ideal_Square, Cross, Error_Square, Defect, Total_Bound);
      pragma
        Assert
          (Executed (X_Axis) * Executed (X_Axis)
           = Ideal (X_Axis) * Ideal (X_Axis) + 2.0 * (Ideal (X_Axis) * Difference (X_Axis))
             + Difference (X_Axis) * Difference (X_Axis));
      pragma
        Assert
          (Executed (Y_Axis) * Executed (Y_Axis)
           = Ideal (Y_Axis) * Ideal (Y_Axis) + 2.0 * (Ideal (Y_Axis) * Difference (Y_Axis))
             + Difference (Y_Axis) * Difference (Y_Axis));
      pragma
        Assert
          (Executed (Z_Axis) * Executed (Z_Axis)
           = Ideal (Z_Axis) * Ideal (Z_Axis) + 2.0 * (Ideal (Z_Axis) * Difference (Z_Axis))
             + Difference (Z_Axis) * Difference (Z_Axis));
      pragma
        Assert
          (Executed (E_Axis) * Executed (E_Axis)
           = Ideal (E_Axis) * Ideal (E_Axis) + 2.0 * (Ideal (E_Axis) * Difference (E_Axis))
             + Difference (E_Axis) * Difference (E_Axis));
      pragma Assert (Dot (Executed, Executed) = Ideal_Square + 2.0 * Cross + Error_Square);
   end Lemma_Speed_Core;

   ---------------------------------------------------------------------------
   --  The same, in the terms of the theorems
   ---------------------------------------------------------------------------

   --  The theorems are about a certificate. In this child unit a certificate's invariant is known only inside
   --  the subprograms which the specification declares, so no subprogram which is local to this body takes a
   --  certificate; and where the invariant is known, the provers are told all that it holds, which leaves them
   --  time for little else. So the theorems only call the lemmas of this part, which take plain data and are
   --  stated in the very terms which the definitions in the specification unfold to.

   procedure Lemma_Same_Weights (A0, A1, A2, A3, B0, B1, B2, B3, M0, M1, M2, M3 : Real)
   with
     Pre  => A0 = B0 and then A1 = B1 and then A2 = B2 and then A3 = B3,
     Post => A0 * M0 + A1 * M1 + A2 * M2 + A3 * M3 = B0 * M0 + B1 * M1 + B2 * M2 + B3 * M3;

   procedure Lemma_Same_Weights (A0, A1, A2, A3, B0, B1, B2, B3, M0, M1, M2, M3 : Real) is null;

   procedure Lemma_Chord_Vector (Chord : Position_Scale)
   with
     Post =>
       Chord_Vector (Chord) (X_Axis) = R (Chord (X_Axis))
       and then Chord_Vector (Chord) (Y_Axis) = R (Chord (Y_Axis))
       and then Chord_Vector (Chord) (Z_Axis) = R (Chord (Z_Axis))
       and then Chord_Vector (Chord) (E_Axis) = R (Chord (E_Axis));

   procedure Lemma_Chord_Vector (Chord : Position_Scale) is null;

   procedure Lemma_Tangent_Vector_Component
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real)
   with
     Post =>
       Tangent_Vector (Frame, Coefficients, V) (Axis)
       = Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Axis, V);

   procedure Lemma_Tangent_Vector_Component
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real)
   is null;

   procedure Lemma_Frame_Dot_Explicit
     (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Direction : Real_Vector)
   with
     Post =>
       Dot (Direction, Frame_Row (Frame, Component))
       = Direction (X_Axis) * R (Frame (Component) (X_Axis)) + Direction (Y_Axis) * R (Frame (Component) (Y_Axis))
         + Direction (Z_Axis) * R (Frame (Component) (Z_Axis))
         + Direction (E_Axis) * R (Frame (Component) (E_Axis));

   procedure Lemma_Frame_Dot_Explicit
     (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Direction : Real_Vector) is
   begin
      Lemma_Frame_Row (Frame, Component, X_Axis);
      Lemma_Frame_Row (Frame, Component, Y_Axis);
      Lemma_Frame_Row (Frame, Component, Z_Axis);
      Lemma_Frame_Row (Frame, Component, E_Axis);
      Lemma_Same_Combination
        (Direction (X_Axis),
         Direction (Y_Axis),
         Direction (Z_Axis),
         Direction (E_Axis),
         Frame_Row (Frame, Component) (X_Axis),
         Frame_Row (Frame, Component) (Y_Axis),
         Frame_Row (Frame, Component) (Z_Axis),
         Frame_Row (Frame, Component) (E_Axis),
         R (Frame (Component) (X_Axis)),
         R (Frame (Component) (Y_Axis)),
         R (Frame (Component) (Z_Axis)),
         R (Frame (Component) (E_Axis)));
   end Lemma_Frame_Dot_Explicit;

   procedure Lemma_Tangent_Dot_Explicit
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Real_Vector; V : Real)
   with
     Post =>
       Dot (Direction, Tangent_Vector (Frame, Coefficients, V))
       = Direction (X_Axis) * Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, X_Axis, V)
         + Direction (Y_Axis) * Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Y_Axis, V)
         + Direction (Z_Axis) * Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Z_Axis, V)
         + Direction (E_Axis) * Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, E_Axis, V);

   procedure Lemma_Tangent_Dot_Explicit
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Real_Vector; V : Real) is
   begin
      Lemma_Tangent_Vector_Component (Frame, Coefficients, X_Axis, V);
      Lemma_Tangent_Vector_Component (Frame, Coefficients, Y_Axis, V);
      Lemma_Tangent_Vector_Component (Frame, Coefficients, Z_Axis, V);
      Lemma_Tangent_Vector_Component (Frame, Coefficients, E_Axis, V);
      Lemma_Same_Combination
        (Direction (X_Axis),
         Direction (Y_Axis),
         Direction (Z_Axis),
         Direction (E_Axis),
         Tangent_Vector (Frame, Coefficients, V) (X_Axis),
         Tangent_Vector (Frame, Coefficients, V) (Y_Axis),
         Tangent_Vector (Frame, Coefficients, V) (Z_Axis),
         Tangent_Vector (Frame, Coefficients, V) (E_Axis),
         Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, X_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Y_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Z_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, E_Axis, V));
   end Lemma_Tangent_Dot_Explicit;

   procedure Lemma_Tangent_Square_Explicit (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real)
   with
     Post =>
       Dot (Tangent_Vector (Frame, Coefficients, V), Tangent_Vector (Frame, Coefficients, V))
       = Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, X_Axis, V)
         * Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, X_Axis, V)
         + Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Y_Axis, V)
           * Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Y_Axis, V)
         + Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Z_Axis, V)
           * Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Z_Axis, V)
         + Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, E_Axis, V)
           * Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, E_Axis, V);

   procedure Lemma_Tangent_Square_Explicit (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real) is
   begin
      Lemma_Tangent_Vector_Component (Frame, Coefficients, X_Axis, V);
      Lemma_Tangent_Vector_Component (Frame, Coefficients, Y_Axis, V);
      Lemma_Tangent_Vector_Component (Frame, Coefficients, Z_Axis, V);
      Lemma_Tangent_Vector_Component (Frame, Coefficients, E_Axis, V);
      Lemma_Same_Squares
        (Tangent_Vector (Frame, Coefficients, V) (X_Axis),
         Tangent_Vector (Frame, Coefficients, V) (Y_Axis),
         Tangent_Vector (Frame, Coefficients, V) (Z_Axis),
         Tangent_Vector (Frame, Coefficients, V) (E_Axis),
         Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, X_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Y_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, Z_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent (Frame, Coefficients, E_Axis, V));
   end Lemma_Tangent_Square_Explicit;

   procedure Lemma_Total_Bound_Unfolded
     (Tangent_Error : Dimensionless;
      Finish        : Dimensionless_Axis_Vector;
      Uncorrected   : Dimensionless_Axis_Vector;
      Length_Value  : Dimensionless;
      Total         : Dimensionless)
   with
     Pre  => Finish_Correction.Is_Total_Bound (Tangent_Error, Finish, Uncorrected, Length_Value, Total),
     Post =>
       Length_Value > 0.0
       and then 0.0 <= R (Tangent_Error)
       and then R (Tangent_Error) <= R (Total)
       and then
         (Largest_Rate * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
         * (Largest_Rate * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value))
         <= (R (Total) - R (Tangent_Error)) * (R (Total) - R (Tangent_Error));

   procedure Lemma_Total_Bound_Unfolded
     (Tangent_Error : Dimensionless;
      Finish        : Dimensionless_Axis_Vector;
      Uncorrected   : Dimensionless_Axis_Vector;
      Length_Value  : Dimensionless;
      Total         : Dimensionless)
   is null;

   procedure Lemma_Endpoint_Dot
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Direction    : Real_Vector;
      Endpoint     : Real_Vector;
      V            : Real)
   with
     Pre  =>
       (for all Axis in Axis_Name => Endpoint (Axis) = Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, V)),
     Post => Dot (Direction, Numerator_Vector (Frame, Coefficients, V)) = Dot (Direction, Endpoint);
   --  A vector with the components of the tangent numerator has its component along a direction.

   procedure Lemma_Endpoint_Dot
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Direction    : Real_Vector;
      Endpoint     : Real_Vector;
      V            : Real) is
   begin
      Lemma_Numerator_Is_Tangent_Numerator (Frame, Coefficients, X_Axis, V);
      Lemma_Numerator_Is_Tangent_Numerator (Frame, Coefficients, Y_Axis, V);
      Lemma_Numerator_Is_Tangent_Numerator (Frame, Coefficients, Z_Axis, V);
      Lemma_Numerator_Is_Tangent_Numerator (Frame, Coefficients, E_Axis, V);
      pragma Assert (Endpoint (X_Axis) = Unit_Speed.Tangent_Numerator (Frame, Coefficients, X_Axis, V));
      pragma Assert (Endpoint (Y_Axis) = Unit_Speed.Tangent_Numerator (Frame, Coefficients, Y_Axis, V));
      pragma Assert (Endpoint (Z_Axis) = Unit_Speed.Tangent_Numerator (Frame, Coefficients, Z_Axis, V));
      pragma Assert (Endpoint (E_Axis) = Unit_Speed.Tangent_Numerator (Frame, Coefficients, E_Axis, V));
      Lemma_Same_Combination
        (Direction (X_Axis),
         Direction (Y_Axis),
         Direction (Z_Axis),
         Direction (E_Axis),
         Numerator_Vector (Frame, Coefficients, V) (X_Axis),
         Numerator_Vector (Frame, Coefficients, V) (Y_Axis),
         Numerator_Vector (Frame, Coefficients, V) (Z_Axis),
         Numerator_Vector (Frame, Coefficients, V) (E_Axis),
         Endpoint (X_Axis),
         Endpoint (Y_Axis),
         Endpoint (Z_Axis),
         Endpoint (E_Axis));
   end Lemma_Endpoint_Dot;

   procedure Lemma_Tangent_Explicit
     (Direction       : Real_Vector;
      Ideal_X         : Real;
      Ideal_Y         : Real;
      Ideal_Z         : Real;
      Ideal_E         : Real;
      Retained_X      : Real;
      Retained_Y      : Real;
      Retained_Z      : Real;
      Retained_E      : Real;
      Finish          : Dimensionless_Axis_Vector;
      Uncorrected     : Dimensionless_Axis_Vector;
      Length_Value    : Dimensionless;
      Rate_Value      : Real;
      Tangent_Bound   : Real;
      Total_Bound     : Real;
      Distance_Square : Real;
      Executed_Along  : Real;
      Ideal_Along     : Real)
   with
     Pre  =>
       Length_Value > 0.0
       and then 0.0 <= Rate_Value
       and then Rate_Value <= Largest_Rate
       and then 0.0 <= Tangent_Bound
       and then Tangent_Bound <= Total_Bound
       and then
         Distance_Square
         = (Ideal_X - Retained_X) * (Ideal_X - Retained_X) + (Ideal_Y - Retained_Y) * (Ideal_Y - Retained_Y)
           + (Ideal_Z - Retained_Z) * (Ideal_Z - Retained_Z)
           + (Ideal_E - Retained_E) * (Ideal_E - Retained_E)
       and then Distance_Square <= Tangent_Bound * Tangent_Bound
       and then
         (Largest_Rate * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
         * (Largest_Rate * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value))
         <= (Total_Bound - Tangent_Bound) * (Total_Bound - Tangent_Bound)
       and then
         Executed_Along
         = Direction (X_Axis)
           * (Retained_X + Rate_Value * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
           + Direction (Y_Axis)
             * (Retained_Y + Rate_Value * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
           + Direction (Z_Axis)
             * (Retained_Z + Rate_Value * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
           + Direction (E_Axis)
             * (Retained_E + Rate_Value * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value))
       and then
         Ideal_Along
         = Direction (X_Axis) * Ideal_X + Direction (Y_Axis) * Ideal_Y + Direction (Z_Axis) * Ideal_Z
           + Direction (E_Axis) * Ideal_E,
     Post =>
       (Executed_Along - Ideal_Along) * (Executed_Along - Ideal_Along)
       <= Dot (Direction, Direction) * (Total_Bound * Total_Bound);
   --  Lemma_Tangent_Core for the four components of each tangent, as Squared_Distance, Dot_Executed and
   --  Dot_Ideal state them.

   procedure Lemma_Tangent_Explicit
     (Direction       : Real_Vector;
      Ideal_X         : Real;
      Ideal_Y         : Real;
      Ideal_Z         : Real;
      Ideal_E         : Real;
      Retained_X      : Real;
      Retained_Y      : Real;
      Retained_Z      : Real;
      Retained_E      : Real;
      Finish          : Dimensionless_Axis_Vector;
      Uncorrected     : Dimensionless_Axis_Vector;
      Length_Value    : Dimensionless;
      Rate_Value      : Real;
      Tangent_Bound   : Real;
      Total_Bound     : Real;
      Distance_Square : Real;
      Executed_Along  : Real;
      Ideal_Along     : Real)
   is
      Ideal    : constant Real_Vector := [X_Axis => Ideal_X, Y_Axis => Ideal_Y, Z_Axis => Ideal_Z, E_Axis => Ideal_E];
      Retained : constant Real_Vector :=
        [X_Axis => Retained_X, Y_Axis => Retained_Y, Z_Axis => Retained_Z, E_Axis => Retained_E];
      Executed : constant Real_Vector :=
        [X_Axis => Retained_X + Rate_Value * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value),
         Y_Axis => Retained_Y + Rate_Value * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value),
         Z_Axis => Retained_Z + Rate_Value * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value),
         E_Axis => Retained_E + Rate_Value * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value)];
   begin
      pragma Assert (R (Length_Value) > 0.0);
      pragma Assert (Executed_Along = Dot (Direction, Executed));
      pragma Assert (Ideal_Along = Dot (Direction, Ideal));
      Lemma_Tangent_Core
        (Direction       => Direction,
         Ideal           => Ideal,
         Retained        => Retained,
         Executed        => Executed,
         Finish          => Finish,
         Uncorrected     => Uncorrected,
         Length_Value    => Length_Value,
         Rate_Value      => Rate_Value,
         Tangent_Bound   => Tangent_Bound,
         Total_Bound     => Total_Bound,
         Distance_Square => Distance_Square,
         Executed_Along  => Executed_Along,
         Ideal_Along     => Ideal_Along);
   end Lemma_Tangent_Explicit;

   procedure Lemma_Cone_Explicit
     (Geometry         : Certified_Geometry.Certificate;
      Normal           : Real_Vector;
      Start_Numerator  : Real_Vector;
      Finish_Numerator : Real_Vector;
      V                : Real;
      Tangent_X        : Real;
      Tangent_Y        : Real;
      Tangent_Z        : Real;
      Tangent_E        : Real;
      Ideal_Along      : Real)
   with
     Pre  =>
       Certified_Geometry.Kind (Geometry) = Certified_Geometry.In_Cone
       and then 0.0 <= V
       and then V <= 1.0
       and then
         Normal (X_Axis) * R (Certified_Geometry.Frame (Geometry) (2) (X_Axis))
         + Normal (Y_Axis) * R (Certified_Geometry.Frame (Geometry) (2) (Y_Axis))
         + Normal (Z_Axis) * R (Certified_Geometry.Frame (Geometry) (2) (Z_Axis))
         + Normal (E_Axis) * R (Certified_Geometry.Frame (Geometry) (2) (E_Axis))
         = 0.0
       and then
         Normal (X_Axis) * R (Certified_Geometry.Frame (Geometry) (3) (X_Axis))
         + Normal (Y_Axis) * R (Certified_Geometry.Frame (Geometry) (3) (Y_Axis))
         + Normal (Z_Axis) * R (Certified_Geometry.Frame (Geometry) (3) (Z_Axis))
         + Normal (E_Axis) * R (Certified_Geometry.Frame (Geometry) (3) (E_Axis))
         = 0.0
       and then
         (for all Axis in Axis_Name =>
            Start_Numerator (Axis)
            = Unit_Speed.Tangent_Numerator
                (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Axis, 0.0))
       and then
         (for all Axis in Axis_Name =>
            Finish_Numerator (Axis)
            = Unit_Speed.Tangent_Numerator
                (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Axis, 1.0))
       and then Dot (Normal, Start_Numerator) >= 0.0
       and then Dot (Normal, Finish_Numerator) >= 0.0
       and then
         Tangent_X
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), X_Axis, V)
       and then
         Tangent_Y
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Y_Axis, V)
       and then
         Tangent_Z
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Z_Axis, V)
       and then
         Tangent_E
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), E_Axis, V)
       and then
         Ideal_Along
         = Normal (X_Axis) * Tangent_X + Normal (Y_Axis) * Tangent_Y + Normal (Z_Axis) * Tangent_Z
           + Normal (E_Axis) * Tangent_E,
     Post => Ideal_Along >= 0.0;
   --  Lemma_Cone_Core as Dot_Frame, Endpoint_Numerator, Ideal_Tangent and Dot_Ideal state its terms.

   procedure Lemma_Cone_Explicit
     (Geometry         : Certified_Geometry.Certificate;
      Normal           : Real_Vector;
      Start_Numerator  : Real_Vector;
      Finish_Numerator : Real_Vector;
      V                : Real;
      Tangent_X        : Real;
      Tangent_Y        : Real;
      Tangent_Z        : Real;
      Tangent_E        : Real;
      Ideal_Along      : Real) is
   begin
      Lemma_Frame_Dot_Explicit (Certified_Geometry.Frame (Geometry), 2, Normal);
      Lemma_Frame_Dot_Explicit (Certified_Geometry.Frame (Geometry), 3, Normal);
      Lemma_Endpoint_Dot
        (Certified_Geometry.Frame (Geometry),
         Certified_Geometry.Coefficients (Geometry),
         Normal,
         Start_Numerator,
         0.0);
      Lemma_Endpoint_Dot
        (Certified_Geometry.Frame (Geometry),
         Certified_Geometry.Coefficients (Geometry),
         Normal,
         Finish_Numerator,
         1.0);
      Lemma_Cone_Core (Geometry, Normal, V);
      Lemma_Tangent_Dot_Explicit
        (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Normal, V);
      Lemma_Same_Combination
        (Normal (X_Axis),
         Normal (Y_Axis),
         Normal (Z_Axis),
         Normal (E_Axis),
         Tangent_X,
         Tangent_Y,
         Tangent_Z,
         Tangent_E,
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), X_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Y_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Z_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), E_Axis, V));
   end Lemma_Cone_Explicit;

   procedure Lemma_Chord_Explicit
     (Geometry    : Certified_Geometry.Certificate;
      V           : Real;
      Tangent_X   : Real;
      Tangent_Y   : Real;
      Tangent_Z   : Real;
      Tangent_E   : Real;
      Ideal_Along : Real)
   with
     Pre  =>
       Certified_Geometry.Kind (Geometry) = Certified_Geometry.Chord_Progress
       and then 0.0 <= V
       and then V <= 1.0
       and then
         Tangent_X
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), X_Axis, V)
       and then
         Tangent_Y
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Y_Axis, V)
       and then
         Tangent_Z
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Z_Axis, V)
       and then
         Tangent_E
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), E_Axis, V)
       and then
         Ideal_Along
         = Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)) (X_Axis) * Tangent_X
           + Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)) (Y_Axis) * Tangent_Y
           + Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)) (Z_Axis) * Tangent_Z
           + Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)) (E_Axis) * Tangent_E,
     Post => Ideal_Along >= 0.0;
   --  Lemma_Chord_Core as Ideal_Tangent and Dot_Ideal state its terms.

   procedure Lemma_Chord_Explicit
     (Geometry    : Certified_Geometry.Certificate;
      V           : Real;
      Tangent_X   : Real;
      Tangent_Y   : Real;
      Tangent_Z   : Real;
      Tangent_E   : Real;
      Ideal_Along : Real) is
   begin
      Lemma_Chord_Core (Geometry, V);
      Lemma_Tangent_Dot_Explicit
        (Certified_Geometry.Frame (Geometry),
         Certified_Geometry.Coefficients (Geometry),
         Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)),
         V);
      Lemma_Same_Combination
        (Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)) (X_Axis),
         Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)) (Y_Axis),
         Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)) (Z_Axis),
         Chord_Vector (Certified_Geometry.Chord_Direction (Geometry)) (E_Axis),
         Tangent_X,
         Tangent_Y,
         Tangent_Z,
         Tangent_E,
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), X_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Y_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Z_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), E_Axis, V));
   end Lemma_Chord_Explicit;

   procedure Lemma_Ideal_Speed_Explicit
     (Geometry  : Certified_Geometry.Certificate;
      V         : Real;
      Tangent_X : Real;
      Tangent_Y : Real;
      Tangent_Z : Real;
      Tangent_E : Real)
   with
     Pre  =>
       Certified_Geometry.Kind (Geometry) /= Certified_Geometry.Not_Certified
       and then Certified_Geometry.Frame_Defect (Geometry) < Dimensionless'Last
       and then
         Tangent_X
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), X_Axis, V)
       and then
         Tangent_Y
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Y_Axis, V)
       and then
         Tangent_Z
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Z_Axis, V)
       and then
         Tangent_E
         = Sampled_Tangent.Model.Ideal_Tangent
             (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), E_Axis, V),
     Post =>
       abs (Tangent_X * Tangent_X + Tangent_Y * Tangent_Y + Tangent_Z * Tangent_Z + Tangent_E * Tangent_E - 1.0)
       <= 4.0 * (R (Certified_Geometry.Frame_Defect (Geometry)) + Defect_Allowance);
   --  Lemma_Ideal_Speed for the four components of the tangent, as Ideal_Tangent states them.

   procedure Lemma_Ideal_Speed_Explicit
     (Geometry  : Certified_Geometry.Certificate;
      V         : Real;
      Tangent_X : Real;
      Tangent_Y : Real;
      Tangent_Z : Real;
      Tangent_E : Real) is
   begin
      Lemma_Ideal_Speed (Geometry, V);
      Lemma_Tangent_Square_Explicit
        (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), V);
      Lemma_Same_Squares
        (Tangent_X,
         Tangent_Y,
         Tangent_Z,
         Tangent_E,
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), X_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Y_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), Z_Axis, V),
         Sampled_Tangent.Model.Ideal_Tangent
           (Certified_Geometry.Frame (Geometry), Certified_Geometry.Coefficients (Geometry), E_Axis, V));
   end Lemma_Ideal_Speed_Explicit;

   procedure Lemma_Speed_Explicit
     (Ideal                     : Real_Vector;
      Difference                : Real_Vector;
      Ideal_X                   : Real;
      Ideal_Y                   : Real;
      Ideal_Z                   : Real;
      Ideal_E                   : Real;
      Executed_X                : Real;
      Executed_Y                : Real;
      Executed_Z                : Real;
      Executed_E                : Real;
      Defect                    : Real;
      Total_Bound               : Real;
      Executed_Along_Difference : Real;
      Ideal_Along_Difference    : Real;
      Executed_Along_Ideal      : Real;
      Ideal_Along_Ideal         : Real)
   with
     Pre  =>
       Ideal (X_Axis) = Ideal_X
       and then Ideal (Y_Axis) = Ideal_Y
       and then Ideal (Z_Axis) = Ideal_Z
       and then Ideal (E_Axis) = Ideal_E
       and then Difference (X_Axis) = Executed_X - Ideal_X
       and then Difference (Y_Axis) = Executed_Y - Ideal_Y
       and then Difference (Z_Axis) = Executed_Z - Ideal_Z
       and then Difference (E_Axis) = Executed_E - Ideal_E
       and then Total_Bound >= 0.0
       and then
         abs (Ideal_X * Ideal_X + Ideal_Y * Ideal_Y + Ideal_Z * Ideal_Z + Ideal_E * Ideal_E - 1.0) <= 4.0 * Defect
       and then
         Executed_Along_Difference
         = Difference (X_Axis) * Executed_X + Difference (Y_Axis) * Executed_Y + Difference (Z_Axis) * Executed_Z
           + Difference (E_Axis) * Executed_E
       and then
         Ideal_Along_Difference
         = Difference (X_Axis) * Ideal_X + Difference (Y_Axis) * Ideal_Y + Difference (Z_Axis) * Ideal_Z
           + Difference (E_Axis) * Ideal_E
       and then
         Executed_Along_Ideal
         = Ideal (X_Axis) * Executed_X + Ideal (Y_Axis) * Executed_Y + Ideal (Z_Axis) * Executed_Z
           + Ideal (E_Axis) * Executed_E
       and then
         Ideal_Along_Ideal
         = Ideal (X_Axis) * Ideal_X + Ideal (Y_Axis) * Ideal_Y + Ideal (Z_Axis) * Ideal_Z + Ideal (E_Axis) * Ideal_E
       and then
         (Executed_Along_Difference - Ideal_Along_Difference) * (Executed_Along_Difference - Ideal_Along_Difference)
         <= Dot (Difference, Difference) * (Total_Bound * Total_Bound)
       and then
         (Executed_Along_Ideal - Ideal_Along_Ideal) * (Executed_Along_Ideal - Ideal_Along_Ideal)
         <= Dot (Ideal, Ideal) * (Total_Bound * Total_Bound),
     Post =>
       abs (Executed_X * Executed_X + Executed_Y * Executed_Y + Executed_Z * Executed_Z
            + Executed_E * Executed_E
            - 1.0)
       <= 4.0 * Defect + 2.0 * (1.0 + 2.0 * Defect) * Total_Bound + Total_Bound * Total_Bound;
   --  Lemma_Speed_Core for the four components of each tangent, as Dot_Executed and Dot_Ideal state them.

   procedure Lemma_Speed_Explicit
     (Ideal                     : Real_Vector;
      Difference                : Real_Vector;
      Ideal_X                   : Real;
      Ideal_Y                   : Real;
      Ideal_Z                   : Real;
      Ideal_E                   : Real;
      Executed_X                : Real;
      Executed_Y                : Real;
      Executed_Z                : Real;
      Executed_E                : Real;
      Defect                    : Real;
      Total_Bound               : Real;
      Executed_Along_Difference : Real;
      Ideal_Along_Difference    : Real;
      Executed_Along_Ideal      : Real;
      Ideal_Along_Ideal         : Real)
   is
      Executed : constant Real_Vector :=
        [X_Axis => Executed_X, Y_Axis => Executed_Y, Z_Axis => Executed_Z, E_Axis => Executed_E];
   begin
      pragma Assert (Executed (X_Axis) = Executed_X);
      pragma Assert (Executed (Y_Axis) = Executed_Y);
      pragma Assert (Executed (Z_Axis) = Executed_Z);
      pragma Assert (Executed (E_Axis) = Executed_E);
      Lemma_Same_Squares
        (Ideal (X_Axis), Ideal (Y_Axis), Ideal (Z_Axis), Ideal (E_Axis), Ideal_X, Ideal_Y, Ideal_Z, Ideal_E);
      pragma Assert (abs (Dot (Ideal, Ideal) - 1.0) <= 4.0 * Defect);
      Lemma_Same_Combination
        (Difference (X_Axis),
         Difference (Y_Axis),
         Difference (Z_Axis),
         Difference (E_Axis),
         Executed_X,
         Executed_Y,
         Executed_Z,
         Executed_E,
         Executed (X_Axis),
         Executed (Y_Axis),
         Executed (Z_Axis),
         Executed (E_Axis));
      pragma Assert (Executed_Along_Difference = Dot (Difference, Executed));
      Lemma_Same_Combination
        (Difference (X_Axis),
         Difference (Y_Axis),
         Difference (Z_Axis),
         Difference (E_Axis),
         Ideal_X,
         Ideal_Y,
         Ideal_Z,
         Ideal_E,
         Ideal (X_Axis),
         Ideal (Y_Axis),
         Ideal (Z_Axis),
         Ideal (E_Axis));
      pragma Assert (Ideal_Along_Difference = Dot (Difference, Ideal));
      Lemma_Same_Combination
        (Ideal (X_Axis),
         Ideal (Y_Axis),
         Ideal (Z_Axis),
         Ideal (E_Axis),
         Executed_X,
         Executed_Y,
         Executed_Z,
         Executed_E,
         Executed (X_Axis),
         Executed (Y_Axis),
         Executed (Z_Axis),
         Executed (E_Axis));
      pragma Assert (Executed_Along_Ideal = Dot (Ideal, Executed));
      Lemma_Same_Combination
        (Ideal (X_Axis),
         Ideal (Y_Axis),
         Ideal (Z_Axis),
         Ideal (E_Axis),
         Ideal_X,
         Ideal_Y,
         Ideal_Z,
         Ideal_E,
         Ideal (X_Axis),
         Ideal (Y_Axis),
         Ideal (Z_Axis),
         Ideal (E_Axis));
      pragma Assert (Ideal_Along_Ideal = Dot (Ideal, Ideal));
      Lemma_Speed_Core
        (Ideal                     => Ideal,
         Executed                  => Executed,
         Difference                => Difference,
         Defect                    => Defect,
         Total_Bound               => Total_Bound,
         Executed_Along_Difference => Executed_Along_Difference,
         Ideal_Along_Difference    => Ideal_Along_Difference,
         Executed_Along_Ideal      => Executed_Along_Ideal,
         Ideal_Along_Ideal         => Ideal_Along_Ideal);
      Lemma_Same_Squares
        (Executed (X_Axis),
         Executed (Y_Axis),
         Executed (Z_Axis),
         Executed (E_Axis),
         Executed_X,
         Executed_Y,
         Executed_Z,
         Executed_E);
      pragma
        Assert
          (Dot (Executed, Executed)
           = Executed_X * Executed_X + Executed_Y * Executed_Y + Executed_Z * Executed_Z + Executed_E * Executed_E);
   end Lemma_Speed_Explicit;

   ---------------------------------------------------------------------------
   --  The theorems
   ---------------------------------------------------------------------------

   procedure Lemma_Executed_Tangent
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Total        : Dimensionless;
      Direction    : Real_Vector;
      U            : Real) is
   begin
      Lemma_Tangent_Error (Value, U);
      Finish_Correction.Lemma_Rate (U);
      Lemma_Total_Bound_Unfolded (Tangent_Error (Value), Finish, Uncorrected, Length_Value, Total);
      Lemma_Tangent_Explicit
        (Direction       => Direction,
         Ideal_X         => Ideal_Tangent (Value, X_Axis, U),
         Ideal_Y         => Ideal_Tangent (Value, Y_Axis, U),
         Ideal_Z         => Ideal_Tangent (Value, Z_Axis, U),
         Ideal_E         => Ideal_Tangent (Value, E_Axis, U),
         Retained_X      => Retained_Value (Value, X_Axis, U),
         Retained_Y      => Retained_Value (Value, Y_Axis, U),
         Retained_Z      => Retained_Value (Value, Z_Axis, U),
         Retained_E      => Retained_Value (Value, E_Axis, U),
         Finish          => Finish,
         Uncorrected     => Uncorrected,
         Length_Value    => Length_Value,
         Rate_Value      => Finish_Correction.Rate (U),
         Tangent_Bound   => R (Tangent_Error (Value)),
         Total_Bound     => R (Total),
         Distance_Square => Squared_Distance (Value, U),
         Executed_Along  => Dot_Executed (Value, Finish, Uncorrected, Length_Value, Direction, U),
         Ideal_Along     => Dot_Ideal (Value, Direction, U));
   end Lemma_Executed_Tangent;

   procedure Lemma_Executed_In_Cone
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Total        : Dimensionless;
      Normal       : Real_Vector;
      U            : Real) is
   begin
      Lemma_Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Total, Normal, U);
      Lemma_Total_Bound_Unfolded (Tangent_Error (Value), Finish, Uncorrected, Length_Value, Total);
      Lemma_Warped_Is_In_Unit_Interval (U, R (Warp (Value)));
      Lemma_Cone_Explicit
        (Geometry         => Geometry (Value),
         Normal           => Normal,
         Start_Numerator  => Endpoint_Numerator (Value, At_Finish => False),
         Finish_Numerator => Endpoint_Numerator (Value, At_Finish => True),
         V                => Warped (U, R (Warp (Value))),
         Tangent_X        => Ideal_Tangent (Value, X_Axis, U),
         Tangent_Y        => Ideal_Tangent (Value, Y_Axis, U),
         Tangent_Z        => Ideal_Tangent (Value, Z_Axis, U),
         Tangent_E        => Ideal_Tangent (Value, E_Axis, U),
         Ideal_Along      => Dot_Ideal (Value, Normal, U));
      Lemma_Lower_Bound
        (Executed    => Dot_Executed (Value, Finish, Uncorrected, Length_Value, Normal, U),
         Ideal       => Dot_Ideal (Value, Normal, U),
         Norm_Square => Dot (Normal, Normal),
         Total_Bound => R (Total),
         Length      => 1.0);
   end Lemma_Executed_In_Cone;

   procedure Lemma_Executed_Chord_Progress
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Total        : Dimensionless;
      Chord_Length : Real;
      U            : Real) is
   begin
      Lemma_Executed_Tangent
        (Value,
         Finish,
         Uncorrected,
         Length_Value,
         Total,
         Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value))),
         U);
      Lemma_Total_Bound_Unfolded (Tangent_Error (Value), Finish, Uncorrected, Length_Value, Total);
      Lemma_Warped_Is_In_Unit_Interval (U, R (Warp (Value)));
      Lemma_Chord_Explicit
        (Geometry    => Geometry (Value),
         V           => Warped (U, R (Warp (Value))),
         Tangent_X   => Ideal_Tangent (Value, X_Axis, U),
         Tangent_Y   => Ideal_Tangent (Value, Y_Axis, U),
         Tangent_Z   => Ideal_Tangent (Value, Z_Axis, U),
         Tangent_E   => Ideal_Tangent (Value, E_Axis, U),
         Ideal_Along => Dot_Ideal (Value, Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value))), U));
      Lemma_Chord_Square (Certified_Geometry.Chord_Direction (Geometry (Value)));
      Lemma_Lower_Bound
        (Executed    =>
           Dot_Executed
             (Value,
              Finish,
              Uncorrected,
              Length_Value,
              Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value))),
              U),
         Ideal       => Dot_Ideal (Value, Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value))), U),
         Norm_Square =>
           Dot
             (Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value))),
              Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value)))),
         Total_Bound => R (Total),
         Length      => Chord_Length);
      Lemma_Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value)));
      Lemma_Same_Weights
        (Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value))) (X_Axis),
         Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value))) (Y_Axis),
         Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value))) (Z_Axis),
         Chord_Vector (Certified_Geometry.Chord_Direction (Geometry (Value))) (E_Axis),
         R (Certified_Geometry.Chord_Direction (Geometry (Value)) (X_Axis)),
         R (Certified_Geometry.Chord_Direction (Geometry (Value)) (Y_Axis)),
         R (Certified_Geometry.Chord_Direction (Geometry (Value)) (Z_Axis)),
         R (Certified_Geometry.Chord_Direction (Geometry (Value)) (E_Axis)),
         Executed_Tangent (Value, Finish, Uncorrected, Length_Value, X_Axis, U),
         Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Y_Axis, U),
         Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Z_Axis, U),
         Executed_Tangent (Value, Finish, Uncorrected, Length_Value, E_Axis, U));
   end Lemma_Executed_Chord_Progress;

   procedure Lemma_Executed_Speed
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Total        : Dimensionless;
      U            : Real) is
   begin
      Lemma_Tangent_Error (Value, U);
      Lemma_Total_Bound_Unfolded (Tangent_Error (Value), Finish, Uncorrected, Length_Value, Total);
      declare
         Ideal      : constant Real_Vector :=
           [X_Axis => Ideal_Tangent (Value, X_Axis, U),
            Y_Axis => Ideal_Tangent (Value, Y_Axis, U),
            Z_Axis => Ideal_Tangent (Value, Z_Axis, U),
            E_Axis => Ideal_Tangent (Value, E_Axis, U)];
         Difference : constant Real_Vector :=
           [X_Axis =>
              Executed_Tangent (Value, Finish, Uncorrected, Length_Value, X_Axis, U)
              - Ideal_Tangent (Value, X_Axis, U),
            Y_Axis =>
              Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Y_Axis, U)
              - Ideal_Tangent (Value, Y_Axis, U),
            Z_Axis =>
              Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Z_Axis, U)
              - Ideal_Tangent (Value, Z_Axis, U),
            E_Axis =>
              Executed_Tangent (Value, Finish, Uncorrected, Length_Value, E_Axis, U)
              - Ideal_Tangent (Value, E_Axis, U)];
      begin
         --  The executed tangent is within Total of the ideal one along their difference and along the ideal
         --  tangent itself, and the ideal tangent has unit speed up to the frame's defect.
         Lemma_Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Total, Difference, U);
         Lemma_Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Total, Ideal, U);
         Lemma_Ideal_Speed_Explicit
           (Geometry  => Geometry (Value),
            V         => Warped (U, R (Warp (Value))),
            Tangent_X => Ideal_Tangent (Value, X_Axis, U),
            Tangent_Y => Ideal_Tangent (Value, Y_Axis, U),
            Tangent_Z => Ideal_Tangent (Value, Z_Axis, U),
            Tangent_E => Ideal_Tangent (Value, E_Axis, U));
         Lemma_Speed_Explicit
           (Ideal                     => Ideal,
            Difference                => Difference,
            Ideal_X                   => Ideal_Tangent (Value, X_Axis, U),
            Ideal_Y                   => Ideal_Tangent (Value, Y_Axis, U),
            Ideal_Z                   => Ideal_Tangent (Value, Z_Axis, U),
            Ideal_E                   => Ideal_Tangent (Value, E_Axis, U),
            Executed_X                => Executed_Tangent (Value, Finish, Uncorrected, Length_Value, X_Axis, U),
            Executed_Y                => Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Y_Axis, U),
            Executed_Z                => Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Z_Axis, U),
            Executed_E                => Executed_Tangent (Value, Finish, Uncorrected, Length_Value, E_Axis, U),
            Defect                    => R (Certified_Geometry.Frame_Defect (Geometry (Value))) + Defect_Allowance,
            Total_Bound               => R (Total),
            Executed_Along_Difference => Dot_Executed (Value, Finish, Uncorrected, Length_Value, Difference, U),
            Ideal_Along_Difference    => Dot_Ideal (Value, Difference, U),
            Executed_Along_Ideal      => Dot_Executed (Value, Finish, Uncorrected, Length_Value, Ideal, U),
            Ideal_Along_Ideal         => Dot_Ideal (Value, Ideal, U));
      end;
   end Lemma_Executed_Speed;

end Prunt.Motion_Planner.Stereographic_Kernel.Certified_Evaluator.Motion;
