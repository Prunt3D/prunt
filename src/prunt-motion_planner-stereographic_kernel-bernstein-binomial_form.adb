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
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile. The
--  ghost code among them computes with exact rationals and would be far too slow to run.

package body Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Powers and quotients
   ---------------------------------------------------------------------------

   procedure Lemma_Division_Undone (Quotient, Numerator, Divisor : Real)
   with Pre => Divisor /= 0.0 and then Quotient = Numerator / Divisor, Post => Quotient * Divisor = Numerator;
   --  Controls are specified as quotients by binomial coefficients; the proofs use them multiplied out.

   procedure Lemma_Division_Undone (Quotient, Numerator, Divisor : Real) is null;

   procedure Lemma_Power_Is_Nonnegative (Base : Real; Exponent : Natural) is
   begin
      pragma Assert (Power (Base, 0) = 1.0);
      for Lower in 1 .. Exponent loop
         pragma Assert (Power (Base, Lower) = Base * Power (Base, Lower - 1));
         pragma Loop_Invariant (Power (Base, Lower) >= 0.0);
      end loop;
   end Lemma_Power_Is_Nonnegative;

   procedure Lemma_Power_Of_Sum (Base : Real; Left, Right : Natural) is
   begin
      if Left = 0 then
         pragma Assert (Power (Base, Left) = 1.0);
      else
         Lemma_Power_Of_Sum (Base, Left - 1, Right);
         declare
            Lower : constant Real := Power (Base, Left - 1);
            Other : constant Real := Power (Base, Right);
         begin
            pragma Assert (Power (Base, Left) = Base * Lower);
            pragma Assert (Power (Base, Left + Right) = Base * Power (Base, Left - 1 + Right));
            pragma Assert (Power (Base, Left + Right) = Base * (Lower * Other));
            pragma Assert (Base * (Lower * Other) = (Base * Lower) * Other);
         end;
      end if;
   end Lemma_Power_Of_Sum;

   ---------------------------------------------------------------------------
   --  Pascal's rule for the basis polynomials
   ---------------------------------------------------------------------------

   procedure Lemma_Pascal_Algebra (A, B, T, U, X, Y : Real)
   with Post => (A + B) * (T * X) * (U * Y) = U * (B * (T * X) * Y) + T * (A * X * (U * Y));
   --  The rule with the coefficients A and B of the lower degree and the powers X and Y of T and of U = 1 - T which
   --  all three basis polynomials share.

   procedure Lemma_Pascal_Algebra (A, B, T, U, X, Y : Real) is
      W : constant Real := X * Y;
   begin
      pragma Assert ((T * X) * (U * Y) = T * U * W);
      pragma Assert ((A + B) * (T * X) * (U * Y) = (A + B) * (T * U * W));
      pragma Assert ((A + B) * (T * U * W) = A * (T * U * W) + B * (T * U * W));
      pragma Assert (B * (T * X) * Y = B * (T * W));
      pragma Assert (U * (B * (T * X) * Y) = B * (T * U * W));
      pragma Assert (A * X * (U * Y) = A * (U * W));
      pragma Assert (T * (A * X * (U * Y)) = A * (T * U * W));
   end Lemma_Pascal_Algebra;

   procedure Lemma_Basis_Pascal (N, I : Natural; T : Real) is
      U : constant Real := 1.0 - T;
   begin
      if I = 0 then
         pragma Assert (Choose (N, 0) = 1.0 and then Choose (N - 1, 0) = 1.0);
         pragma Assert (Power (T, 0) = 1.0);
         pragma Assert (Power (U, N) = U * Power (U, N - 1));
         pragma Assert (Basis (N, 0, T) = U * Power (U, N - 1));
         pragma Assert (Basis (N - 1, 0, T) = Power (U, N - 1));
         pragma Assert (Basis (N, I, T) = U * Basis (N - 1, I, T));
      elsif I = N then
         pragma Assert (Choose (N, N) = 1.0 and then Choose (N - 1, N - 1) = 1.0);
         pragma Assert (Power (U, 0) = 1.0);
         pragma Assert (Power (T, N) = T * Power (T, N - 1));
         pragma Assert (Basis (N, N, T) = T * Power (T, N - 1));
         pragma Assert (Basis (N - 1, N - 1, T) = Power (T, N - 1));
         pragma Assert (Basis (N, I, T) = T * Basis (N - 1, I - 1, T));
      else
         declare
            A : constant Real := Choose (N - 1, I - 1);
            B : constant Real := Choose (N - 1, I);
            X : constant Real := Power (T, I - 1);
            Y : constant Real := Power (U, N - I - 1);
         begin
            pragma Assert (Choose (N, I) = A + B);
            pragma Assert (Power (T, I) = T * X);
            pragma Assert (Power (U, N - I) = U * Y);
            pragma Assert (Basis (N, I, T) = (A + B) * (T * X) * (U * Y));
            pragma Assert (Basis (N - 1, I, T) = B * (T * X) * Y);
            pragma Assert (Basis (N - 1, I - 1, T) = A * X * (U * Y));
            Lemma_Pascal_Algebra (A, B, T, U, X, Y);
            pragma Assert (Basis (N, I, T) = U * Basis (N - 1, I, T) + T * Basis (N - 1, I - 1, T));
         end;
      end if;
   end Lemma_Basis_Pascal;

   ---------------------------------------------------------------------------
   --  The Bernstein sum of controls of any degree
   ---------------------------------------------------------------------------

   --  Products reach twice the Maximum_Degree, so the working sum is defined for any controls indexed from zero.
   --  Partial_Sum is the same sum under the narrower precondition.

   function Zero_Based (P : Controls) return Boolean
   is (P'First = 0 and then P'Last < Natural'Last);

   function Sum (P : Controls; Count : Natural; T : Real) return Real
   is (if Count = 0 then 0.0 else Sum (P, Count - 1, T) + P (Count - 1) * Basis (P'Last, Count - 1, T))
   with Pre => Zero_Based (P) and then Count <= P'Length, Subprogram_Variant => (Decreases => Count);

   procedure Lemma_Partial_Sum_Is_Sum (P : Controls; Count : Natural; T : Real)
   with
     Pre                => Is_Polynomial (P) and then Count <= P'Length,
     Post               => Partial_Sum (P, Count, T) = Sum (P, Count, T),
     Subprogram_Variant => (Decreases => Count);

   procedure Lemma_Partial_Sum_Is_Sum (P : Controls; Count : Natural; T : Real) is
   begin
      if Count > 0 then
         Lemma_Partial_Sum_Is_Sum (P, Count - 1, T);
      end if;
   end Lemma_Partial_Sum_Is_Sum;

   procedure Lemma_Sum_Split (P, Left, Right : Controls; Count : Positive; T : Real)
   with
     Pre                =>
       Zero_Based (P)
       and then P'Last >= 1
       and then Left'First = 0
       and then Left'Last = P'Last - 1
       and then Right'First = 0
       and then Right'Last = P'Last - 1
       and then (for all I in Left'Range => Left (I) = P (I))
       and then (for all I in Right'Range => Right (I) = P (I + 1))
       and then Count <= P'Last,
     Post               => Sum (P, Count, T) = (1.0 - T) * Sum (Left, Count, T) + T * Sum (Right, Count - 1, T),
     Subprogram_Variant => (Decreases => Count);
   --  Pascal's rule applied to each term: the sum of P is 1 - T times the sum of all its controls but the last plus
   --  T times the sum of all but the first, both one degree lower. The second lags by one term.

   procedure Lemma_Sum_Split (P, Left, Right : Controls; Count : Positive; T : Real) is
      N : constant Positive := P'Last;
      I : constant Natural := Count - 1;
      U : constant Real := 1.0 - T;
   begin
      Lemma_Basis_Pascal (N, I, T);
      pragma Assert (Sum (P, Count, T) = Sum (P, Count - 1, T) + P (I) * Basis (N, I, T));
      pragma Assert (Sum (Left, Count, T) = Sum (Left, Count - 1, T) + Left (I) * Basis (N - 1, I, T));
      pragma Assert (Left (I) = P (I));
      if Count = 1 then
         pragma Assert (Sum (P, 0, T) = 0.0);
         pragma Assert (Sum (Left, 0, T) = 0.0);
         pragma Assert (Sum (Right, 0, T) = 0.0);
         pragma Assert (Basis (N, 0, T) = U * Basis (N - 1, 0, T));
         pragma Assert (P (0) * (U * Basis (N - 1, 0, T)) = U * (P (0) * Basis (N - 1, 0, T)));
      else
         Lemma_Sum_Split (P, Left, Right, Count - 1, T);
         declare
            Lower_Left  : constant Real := Sum (Left, Count - 1, T);
            Lower_Right : constant Real := Sum (Right, I - 1, T);
            Basis_Left  : constant Real := Basis (N - 1, I, T);
            Basis_Right : constant Real := Basis (N - 1, I - 1, T);
            Control     : constant Real := P (I);
         begin
            pragma Assert (Right (I - 1) = Control);
            pragma Assert (Sum (Right, I, T) = Sum (Right, I - 1, T) + Right (I - 1) * Basis (Right'Last, I - 1, T));
            pragma Assert (Sum (Right, Count - 1, T) = Lower_Right + Control * Basis_Right);
            pragma Assert (Sum (Left, Count, T) = Lower_Left + Control * Basis_Left);
            pragma Assert (Sum (P, Count - 1, T) = U * Lower_Left + T * Lower_Right);
            pragma Assert (Basis (N, I, T) = U * Basis_Left + T * Basis_Right);
            pragma
              Assert
                (Control * (U * Basis_Left + T * Basis_Right)
                 = U * (Control * Basis_Left) + T * (Control * Basis_Right));
            pragma Assert (U * (Lower_Left + Control * Basis_Left) = U * Lower_Left + U * (Control * Basis_Left));
            pragma Assert (T * (Lower_Right + Control * Basis_Right) = T * Lower_Right + T * (Control * Basis_Right));
         end;
      end if;
   end Lemma_Sum_Split;

   procedure Lemma_Sum_Split_Whole (P, Left, Right : Controls; T : Real)
   with
     Pre  =>
       Zero_Based (P)
       and then P'Last >= 1
       and then Left'First = 0
       and then Left'Last = P'Last - 1
       and then Right'First = 0
       and then Right'Last = P'Last - 1
       and then (for all I in Left'Range => Left (I) = P (I))
       and then (for all I in Right'Range => Right (I) = P (I + 1)),
     Post => Sum (P, P'Length, T) = (1.0 - T) * Sum (Left, Left'Length, T) + T * Sum (Right, Right'Length, T);
   --  The same for the whole sums, where the last term of P supplies the term by which the second sum lagged.

   procedure Lemma_Sum_Split_Whole (P, Left, Right : Controls; T : Real) is
      N    : constant Positive := P'Last;
      U    : constant Real := 1.0 - T;
      Last : constant Real := Basis (N - 1, N - 1, T);
   begin
      Lemma_Sum_Split (P, Left, Right, N, T);
      Lemma_Basis_Pascal (N, N, T);
      pragma Assert (Basis (N, N, T) = T * Last);
      pragma Assert (Right (N - 1) = P (N));
      pragma Assert (Sum (Right, N, T) = Sum (Right, N - 1, T) + P (N) * Last);
      pragma Assert (Sum (P, N + 1, T) = Sum (P, N, T) + P (N) * Basis (N, N, T));
      pragma Assert (Sum (P, N + 1, T) = Sum (P, N, T) + P (N) * (T * Last));
      pragma Assert (P (N) * (T * Last) = T * (P (N) * Last));
      pragma Assert (T * Sum (Right, N, T) = T * Sum (Right, N - 1, T) + T * (P (N) * Last));
      pragma Assert (Sum (P, N + 1, T) = U * Sum (Left, N, T) + T * Sum (Right, N, T));
      pragma Assert (P'Length = N + 1 and then Left'Length = N and then Right'Length = N);
   end Lemma_Sum_Split_Whole;

   procedure Lemma_Value_Is_Sum (P : Controls; T : Real)
   with
     Pre                => Zero_Based (P) and then P'Length >= 1,
     Post               => Value (P, T) = Sum (P, P'Length, T),
     Subprogram_Variant => (Decreases => P'Length);
   --  De Casteljau's recurrence evaluates the Bernstein sum, at any degree.

   procedure Lemma_Value_Is_Sum (P : Controls; T : Real) is
   begin
      if P'Last = 0 then
         pragma Assert (Value (P, T) = P (0));
         pragma Assert (Choose (0, 0) = 1.0);
         pragma Assert (Power (T, 0) = 1.0);
         pragma Assert (Power (1.0 - T, 0) = 1.0);
         pragma Assert (Basis (0, 0, T) = 1.0);
         pragma Assert (Sum (P, 0, T) = 0.0);
         pragma Assert (Sum (P, 1, T) = Sum (P, 0, T) + P (0) * Basis (0, 0, T));
      else
         declare
            N     : constant Positive := P'Last;
            Left  : constant Controls (0 .. N - 1) := P (0 .. N - 1);
            Right : constant Controls (0 .. N - 1) := P (1 .. N);
         begin
            pragma Assert (for all I in Right'Range => Right (I) = P (I + 1));
            Lemma_Value_Ignores_Offset (P (1 .. N), Right, T);
            Lemma_Value_Is_Sum (Left, T);
            Lemma_Value_Is_Sum (Right, T);
            Lemma_Sum_Split_Whole (P, Left, Right, T);
            pragma Assert (Value (P, T) = (1.0 - T) * Value (P (0 .. N - 1), T) + T * Value (P (1 .. N), T));
         end;
      end if;
   end Lemma_Value_Is_Sum;

   procedure Lemma_Value_Is_Bernstein_Sum (P : Controls; T : Real) is
   begin
      Lemma_Value_Is_Sum (P, T);
      Lemma_Partial_Sum_Is_Sum (P, P'Length, T);
   end Lemma_Value_Is_Bernstein_Sum;

   ---------------------------------------------------------------------------
   --  Multiplication by a power of T or of 1 - T
   ---------------------------------------------------------------------------

   procedure Lemma_Scaled_Term (X, C, Y, D, Scale, U : Real)
   with Pre => X * C = Y * D, Post => X * (C * (Scale * U)) = Scale * (Y * (D * U));
   --  One term of a raised polynomial against the term it came from: the controls X and Y are in the inverse ratio
   --  of the binomial coefficients C and D, and the rest of the basis differs by the factor Scale.

   procedure Lemma_Scaled_Term (X, C, Y, D, Scale, U : Real) is
   begin
      pragma Assert (X * (C * (Scale * U)) = (X * C) * (Scale * U));
      pragma Assert ((Y * D) * (Scale * U) = Scale * (Y * (D * U)));
   end Lemma_Scaled_Term;

   --  The two raisings as relations between controls of any degree, with the divisions multiplied out. The functions
   --  in the specification are the instances below the Maximum_Degree. The relation at one control is a function of
   --  its own, and not an expression function, so that the provers see quantified facts about it without the
   --  products inside: GNATprove inlines expression functions in the assertions it sets out to prove.

   function Fits_Raised (P, Raised : Controls; By : Natural) return Boolean
   is (Zero_Based (P)
       and then Zero_Based (Raised)
       and then P'Last >= 0
       and then By <= Raised'Last
       and then Raised'Last - By = P'Last);

   function Start_Control_Agrees (P, Raised : Controls; By, J : Natural) return Boolean
   with
     Pre  => Fits_Raised (P, Raised, By) and then J <= Raised'Last,
     Post =>
       Start_Control_Agrees'Result
       = (if J < By
          then Raised (J) = 0.0
          else Raised (J) * Choose (Raised'Last, J) = P (J - By) * Choose (P'Last, J - By));

   function Start_Control_Agrees (P, Raised : Controls; By, J : Natural) return Boolean is
   begin
      if J < By then
         return Raised (J) = 0.0;
      else
         return Raised (J) * Choose (Raised'Last, J) = P (J - By) * Choose (P'Last, J - By);
      end if;
   end Start_Control_Agrees;

   function Finish_Control_Agrees (P, Raised : Controls; By, J : Natural) return Boolean
   with
     Pre  => Fits_Raised (P, Raised, By) and then J <= Raised'Last,
     Post =>
       Finish_Control_Agrees'Result
       = (if J > P'Last then Raised (J) = 0.0 else Raised (J) * Choose (Raised'Last, J) = P (J) * Choose (P'Last, J));

   function Finish_Control_Agrees (P, Raised : Controls; By, J : Natural) return Boolean is
   begin
      if J > P'Last then
         return Raised (J) = 0.0;
      else
         return Raised (J) * Choose (Raised'Last, J) = P (J) * Choose (P'Last, J);
      end if;
   end Finish_Control_Agrees;

   function Is_Raised_At_Start (P, Raised : Controls; By : Natural) return Boolean
   is (Fits_Raised (P, Raised, By) and then (for all J in Raised'Range => Start_Control_Agrees (P, Raised, By, J)));

   function Is_Raised_At_Finish (P, Raised : Controls; By : Natural) return Boolean
   is (Fits_Raised (P, Raised, By) and then (for all J in Raised'Range => Finish_Control_Agrees (P, Raised, By, J)));

   procedure Lemma_Term_Raised_At_Start (P, Raised : Controls; By, I : Natural; T : Real)
   with
     Pre  => Is_Raised_At_Start (P, Raised, By) and then I <= P'Last,
     Post => Raised (By + I) * Basis (Raised'Last, By + I, T) = Power (T, By) * (P (I) * Basis (P'Last, I, T));
   --  Term By + I of the raised sum is T**By times term I of the sum of P.

   procedure Lemma_Term_Raised_At_Start (P, Raised : Controls; By, I : Natural; T : Real) is
      J     : constant Natural := By + I;
      Scale : constant Real := Power (T, By);
      T_I   : constant Real := Power (T, I);
      Rest  : constant Real := Power (1.0 - T, P'Last - I);
      Shape : constant Real := T_I * Rest;
      C     : constant Real := Choose (Raised'Last, J);
      D     : constant Real := Choose (P'Last, I);
   begin
      Lemma_Power_Of_Sum (T, By, I);
      pragma Assert (Power (T, J) = Scale * T_I);
      pragma Assert (Raised'Last - J = P'Last - I);
      pragma Assert (Power (1.0 - T, Raised'Last - J) = Rest);
      pragma Assert (Basis (Raised'Last, J, T) = C * Power (T, J) * Power (1.0 - T, Raised'Last - J));
      pragma Assert (Basis (Raised'Last, J, T) = C * (Scale * T_I) * Rest);
      pragma Assert (C * (Scale * T_I) * Rest = C * (Scale * Shape));
      pragma Assert (Basis (Raised'Last, J, T) = C * (Scale * Shape));
      pragma Assert (Basis (P'Last, I, T) = D * T_I * Rest);
      pragma Assert (D * T_I * Rest = D * Shape);
      pragma Assert (Basis (P'Last, I, T) = D * Shape);
      pragma Assert (Start_Control_Agrees (P, Raised, By, J));
      pragma Assert (Raised (J) * C = P (I) * D);
      Lemma_Scaled_Term (Raised (J), C, P (I), D, Scale, Shape);
      pragma Assert (Raised (J) * (C * (Scale * Shape)) = Scale * (P (I) * (D * Shape)));
   end Lemma_Term_Raised_At_Start;

   procedure Lemma_Term_Raised_At_Finish (P, Raised : Controls; By, I : Natural; T : Real)
   with
     Pre  => Is_Raised_At_Finish (P, Raised, By) and then I <= P'Last,
     Post => Raised (I) * Basis (Raised'Last, I, T) = Power (1.0 - T, By) * (P (I) * Basis (P'Last, I, T));
   --  Term I of the raised sum is (1 - T)**By times term I of the sum of P.

   procedure Lemma_Term_Raised_At_Finish (P, Raised : Controls; By, I : Natural; T : Real) is
      Scale : constant Real := Power (1.0 - T, By);
      T_I   : constant Real := Power (T, I);
      Rest  : constant Real := Power (1.0 - T, P'Last - I);
      Shape : constant Real := T_I * Rest;
      C     : constant Real := Choose (Raised'Last, I);
      D     : constant Real := Choose (P'Last, I);
   begin
      Lemma_Power_Of_Sum (1.0 - T, By, P'Last - I);
      pragma Assert (Raised'Last - I = By + (P'Last - I));
      pragma Assert (Power (1.0 - T, Raised'Last - I) = Scale * Rest);
      pragma Assert (Basis (Raised'Last, I, T) = C * Power (T, I) * Power (1.0 - T, Raised'Last - I));
      pragma Assert (Basis (Raised'Last, I, T) = C * T_I * (Scale * Rest));
      pragma Assert (C * T_I * (Scale * Rest) = C * (Scale * Shape));
      pragma Assert (Basis (Raised'Last, I, T) = C * (Scale * Shape));
      pragma Assert (Basis (P'Last, I, T) = D * T_I * Rest);
      pragma Assert (D * T_I * Rest = D * Shape);
      pragma Assert (Basis (P'Last, I, T) = D * Shape);
      pragma Assert (Finish_Control_Agrees (P, Raised, By, I));
      pragma Assert (Raised (I) * C = P (I) * D);
      Lemma_Scaled_Term (Raised (I), C, P (I), D, Scale, Shape);
      pragma Assert (Raised (I) * (C * (Scale * Shape)) = Scale * (P (I) * (D * Shape)));
   end Lemma_Term_Raised_At_Finish;

   procedure Lemma_Sum_Raised_At_Start (P, Raised : Controls; By, Count : Natural; T : Real)
   with
     Pre                => Is_Raised_At_Start (P, Raised, By) and then Count <= Raised'Length,
     Post               => Sum (Raised, Count, T) = Power (T, By) * Sum (P, (if Count > By then Count - By else 0), T),
     Subprogram_Variant => (Decreases => Count);
   --  The first By terms of the raised sum are zero, and the rest are T**By times the terms of the sum of P.

   procedure Lemma_Sum_Raised_At_Start (P, Raised : Controls; By, Count : Natural; T : Real) is
      Scale   : constant Real := Power (T, By);
      Covered : constant Natural := (if Count > By then Count - By else 0);
      --  How many terms of the sum of P the first Count terms of the raised sum hold.
   begin
      if Count = 0 then
         pragma Assert (Sum (Raised, 0, T) = 0.0);
         pragma Assert (Sum (P, 0, T) = 0.0);
      else
         Lemma_Sum_Raised_At_Start (P, Raised, By, Count - 1, T);
         pragma
           Assert
             (Sum (Raised, Count, T)
              = Sum (Raised, Count - 1, T) + Raised (Count - 1) * Basis (Raised'Last, Count - 1, T));
         if Count <= By then
            pragma Assert (Start_Control_Agrees (P, Raised, By, Count - 1));
            pragma Assert (Raised (Count - 1) = 0.0);
            pragma Assert (Sum (Raised, Count - 1, T) = Scale * Sum (P, 0, T));
         else
            declare
               I     : constant Natural := Count - 1 - By;
               Lower : constant Real := Sum (P, I, T);
               Term  : constant Real := P (I) * Basis (P'Last, I, T);
            begin
               Lemma_Term_Raised_At_Start (P, Raised, By, I, T);
               pragma Assert (Sum (Raised, Count, T) = Sum (Raised, Count - 1, T) + Scale * Term);
               pragma Assert (Sum (Raised, Count - 1, T) = Scale * Lower);
               pragma Assert (Sum (P, I + 1, T) = Lower + Term);
               pragma Assert (Scale * (Lower + Term) = Scale * Lower + Scale * Term);
               pragma Assert (Covered = I + 1);
            end;
         end if;
      end if;
      pragma Assert (Sum (Raised, Count, T) = Scale * Sum (P, Covered, T));
   end Lemma_Sum_Raised_At_Start;

   procedure Lemma_Sum_Raised_At_Finish (P, Raised : Controls; By, Count : Natural; T : Real)
   with
     Pre                => Is_Raised_At_Finish (P, Raised, By) and then Count <= Raised'Length,
     Post               =>
       Sum (Raised, Count, T) = Power (1.0 - T, By) * Sum (P, (if Count <= P'Length then Count else P'Length), T),
     Subprogram_Variant => (Decreases => Count);
   --  The terms of the raised sum are (1 - T)**By times the terms of the sum of P, and the last By terms are zero.

   procedure Lemma_Sum_Raised_At_Finish (P, Raised : Controls; By, Count : Natural; T : Real) is
      Scale   : constant Real := Power (1.0 - T, By);
      Covered : constant Natural := (if Count <= P'Length then Count else P'Length);
      --  How many terms of the sum of P the first Count terms of the raised sum hold.
   begin
      if Count = 0 then
         pragma Assert (Sum (Raised, 0, T) = 0.0);
         pragma Assert (Sum (P, 0, T) = 0.0);
      else
         Lemma_Sum_Raised_At_Finish (P, Raised, By, Count - 1, T);
         pragma
           Assert
             (Sum (Raised, Count, T)
              = Sum (Raised, Count - 1, T) + Raised (Count - 1) * Basis (Raised'Last, Count - 1, T));
         if Count > P'Length then
            pragma Assert (Finish_Control_Agrees (P, Raised, By, Count - 1));
            pragma Assert (Raised (Count - 1) = 0.0);
            pragma Assert (Sum (Raised, Count - 1, T) = Scale * Sum (P, P'Length, T));
            pragma Assert (Covered = P'Length);
         else
            declare
               I     : constant Natural := Count - 1;
               Lower : constant Real := Sum (P, I, T);
               Term  : constant Real := P (I) * Basis (P'Last, I, T);
            begin
               Lemma_Term_Raised_At_Finish (P, Raised, By, I, T);
               pragma Assert (Sum (Raised, Count, T) = Sum (Raised, Count - 1, T) + Scale * Term);
               pragma Assert (Sum (Raised, Count - 1, T) = Scale * Lower);
               pragma Assert (Sum (P, Count, T) = Lower + Term);
               pragma Assert (Scale * (Lower + Term) = Scale * Lower + Scale * Term);
               pragma Assert (Covered = Count);
            end;
         end if;
      end if;
      pragma Assert (Sum (Raised, Count, T) = Scale * Sum (P, Covered, T));
   end Lemma_Sum_Raised_At_Finish;

   procedure Lemma_Value_Raised_At_Start (P, Raised : Controls; By : Natural; T : Real)
   with Pre => Is_Raised_At_Start (P, Raised, By), Post => Value (Raised, T) = Power (T, By) * Value (P, T);
   --  Controls raised at the start are those of T**By times the polynomial, at any degree.

   procedure Lemma_Value_Raised_At_Start (P, Raised : Controls; By : Natural; T : Real) is
   begin
      Lemma_Sum_Raised_At_Start (P, Raised, By, Raised'Length, T);
      pragma Assert (Raised'Length - By = P'Length);
      Lemma_Value_Is_Sum (P, T);
      Lemma_Value_Is_Sum (Raised, T);
   end Lemma_Value_Raised_At_Start;

   procedure Lemma_Value_Raised_At_Finish (P, Raised : Controls; By : Natural; T : Real)
   with Pre => Is_Raised_At_Finish (P, Raised, By), Post => Value (Raised, T) = Power (1.0 - T, By) * Value (P, T);
   --  Controls raised at the finish are those of (1 - T)**By times the polynomial, at any degree.

   procedure Lemma_Value_Raised_At_Finish (P, Raised : Controls; By : Natural; T : Real) is
   begin
      Lemma_Sum_Raised_At_Finish (P, Raised, By, Raised'Length, T);
      pragma Assert (Raised'Length >= P'Length);
      Lemma_Value_Is_Sum (P, T);
      Lemma_Value_Is_Sum (Raised, T);
   end Lemma_Value_Raised_At_Finish;

   function Raised_At_Start (P : Controls; By : Natural) return Controls is
      Result : Controls (0 .. P'Last + By) := [others => 0.0];
   begin
      for J in Result'Range loop
         if J >= By then
            Result (J) := P (J - By) * Choose (P'Last, J - By) / Choose (P'Last + By, J);
         end if;
         pragma
           Loop_Invariant
             (for all K in 0 .. J =>
                Result (K) = (if K < By then 0.0 else P (K - By) * Choose (P'Last, K - By) / Choose (P'Last + By, K)));
      end loop;
      return Result;
   end Raised_At_Start;

   procedure Lemma_Raised_At_Start (P : Controls; By : Natural; T : Real) is
      Raised : constant Controls := Raised_At_Start (P, By);
   begin
      --  Multiply out the division in each control.
      for J in Raised'Range loop
         if J >= By then
            pragma Assert (Choose (Raised'Last, J) >= 1.0);
            Lemma_Division_Undone (Raised (J), P (J - By) * Choose (P'Last, J - By), Choose (Raised'Last, J));
         end if;
         pragma Assert (Start_Control_Agrees (P, Raised, By, J));
         pragma Loop_Invariant (for all K in 0 .. J => Start_Control_Agrees (P, Raised, By, K));
      end loop;
      pragma Assert (Is_Raised_At_Start (P, Raised, By));
      Lemma_Value_Raised_At_Start (P, Raised, By, T);
   end Lemma_Raised_At_Start;

   function Raised_At_Finish (P : Controls; By : Natural) return Controls is
      Result : Controls (0 .. P'Last + By) := [others => 0.0];
   begin
      for J in Result'Range loop
         if J <= P'Last then
            Result (J) := P (J) * Choose (P'Last, J) / Choose (P'Last + By, J);
         end if;
         pragma
           Loop_Invariant
             (for all K in 0 .. J =>
                Result (K) = (if K > P'Last then 0.0 else P (K) * Choose (P'Last, K) / Choose (P'Last + By, K)));
      end loop;
      return Result;
   end Raised_At_Finish;

   procedure Lemma_Raised_At_Finish (P : Controls; By : Natural; T : Real) is
      Raised : constant Controls := Raised_At_Finish (P, By);
   begin
      --  Multiply out the division in each control.
      for J in Raised'Range loop
         if J <= P'Last then
            pragma Assert (Choose (Raised'Last, J) >= 1.0);
            Lemma_Division_Undone (Raised (J), P (J) * Choose (P'Last, J), Choose (Raised'Last, J));
         end if;
         pragma Assert (Finish_Control_Agrees (P, Raised, By, J));
         pragma Loop_Invariant (for all K in 0 .. J => Finish_Control_Agrees (P, Raised, By, K));
      end loop;
      pragma Assert (Is_Raised_At_Finish (P, Raised, By));
      Lemma_Value_Raised_At_Finish (P, Raised, By, T);
   end Lemma_Raised_At_Finish;

   ---------------------------------------------------------------------------
   --  Division by an endpoint root
   ---------------------------------------------------------------------------

   procedure Lemma_Value_Of_Constant (P : Controls; Level, T : Real) is
   begin
      --  Every convex combination in the recurrence has weights which sum to one.
      if P'Length > 1 then
         Lemma_Value_Of_Constant (P (P'First .. P'Last - 1), Level, T);
         Lemma_Value_Of_Constant (P (P'First + 1 .. P'Last), Level, T);
         pragma Assert (Value (P, T) = (1.0 - T) * Level + T * Level);
      end if;
   end Lemma_Value_Of_Constant;

   --  Subtracting the end control leaves controls which are the quotient's raised at that end, so the raising
   --  lemmas read backwards give the division.

   function Start_Quotient (P : Controls; Order : Positive) return Controls is
      Result : Controls (0 .. P'Last - Order) := [others => 0.0];
   begin
      for J in Result'Range loop
         Result (J) := (P (J + Order) - P (0)) * Choose (P'Last, J + Order) / Choose (P'Last - Order, J);
         pragma
           Loop_Invariant
             (for all K in 0 .. J =>
                Result (K) = (P (K + Order) - P (0)) * Choose (P'Last, K + Order) / Choose (P'Last - Order, K));
      end loop;
      return Result;
   end Start_Quotient;

   procedure Lemma_Start_Quotient (P : Controls; Order : Positive; T : Real) is
      Quotient : constant Controls := Start_Quotient (P, Order);
      Level    : constant Controls (P'Range) := [others => P (0)];
      Offset   : Controls (P'Range) := [others => 0.0];
   begin
      --  The controls P (J) - P (0) are those of the quotient raised at the start, and P is their sum with a
      --  constant.
      for J in P'Range loop
         Offset (J) := P (J) - P (0);
         pragma Loop_Invariant (for all K in 0 .. J => Offset (K) = P (K) - P (0));
      end loop;
      for J in P'Range loop
         if J >= Order then
            pragma Assert (Choose (Quotient'Last, J - Order) >= 1.0);
            Lemma_Division_Undone
              (Quotient (J - Order), (P (J) - P (0)) * Choose (P'Last, J), Choose (Quotient'Last, J - Order));
            pragma Assert (Offset (J) * Choose (P'Last, J) = Quotient (J - Order) * Choose (Quotient'Last, J - Order));
         else
            pragma Assert (P (J) = P (0));
            pragma Assert (Offset (J) = 0.0);
         end if;
         pragma Assert (Start_Control_Agrees (Quotient, Offset, Order, J));
         pragma Loop_Invariant (for all K in 0 .. J => Start_Control_Agrees (Quotient, Offset, Order, K));
      end loop;
      pragma Assert (Is_Raised_At_Start (Quotient, Offset, Order));
      Lemma_Value_Raised_At_Start (Quotient, Offset, Order, T);
      Lemma_Value_Of_Constant (Level, P (0), T);
      Lemma_Value_Is_Additive (Offset, Level, P, T);
   end Lemma_Start_Quotient;

   function Finish_Quotient (P : Controls; Order : Positive) return Controls is
      Result : Controls (0 .. P'Last - Order) := [others => 0.0];
   begin
      for J in Result'Range loop
         Result (J) := (P (P'Last) - P (J)) * Choose (P'Last, J) / Choose (P'Last - Order, J);
         pragma
           Loop_Invariant
             (for all K in 0 .. J =>
                Result (K) = (P (P'Last) - P (K)) * Choose (P'Last, K) / Choose (P'Last - Order, K));
      end loop;
      return Result;
   end Finish_Quotient;

   procedure Lemma_Finish_Quotient (P : Controls; Order : Positive; T : Real) is
      Quotient : constant Controls := Finish_Quotient (P, Order);
      Level    : constant Controls (P'Range) := [others => P (P'Last)];
      Offset   : Controls (P'Range) := [others => 0.0];
   begin
      --  The controls P (P'Last) - P (J) are those of the quotient raised at the finish, and their sum with P is
      --  a constant.
      for J in P'Range loop
         Offset (J) := P (P'Last) - P (J);
         pragma Loop_Invariant (for all K in 0 .. J => Offset (K) = P (P'Last) - P (K));
      end loop;
      for J in P'Range loop
         if J <= Quotient'Last then
            pragma Assert (Choose (Quotient'Last, J) >= 1.0);
            Lemma_Division_Undone (Quotient (J), (P (P'Last) - P (J)) * Choose (P'Last, J), Choose (Quotient'Last, J));
            pragma Assert (Offset (J) * Choose (P'Last, J) = Quotient (J) * Choose (Quotient'Last, J));
         else
            pragma Assert (P (J) = P (P'Last));
            pragma Assert (Offset (J) = 0.0);
         end if;
         pragma Assert (Finish_Control_Agrees (Quotient, Offset, Order, J));
         pragma Loop_Invariant (for all K in 0 .. J => Finish_Control_Agrees (Quotient, Offset, Order, K));
      end loop;
      pragma Assert (Is_Raised_At_Finish (Quotient, Offset, Order));
      Lemma_Value_Raised_At_Finish (Quotient, Offset, Order, T);
      Lemma_Value_Of_Constant (Level, P (P'Last), T);
      Lemma_Value_Is_Additive (Offset, P, Level, T);
   end Lemma_Finish_Quotient;

   ---------------------------------------------------------------------------
   --  Products
   ---------------------------------------------------------------------------

   --  The product is built one control of A at a time. Row I is A (I) times basis polynomial I of A's degree times
   --  the polynomial B, whose controls at the degree of the product are a shifted and rescaled copy of B's: B
   --  raised I times at the start and the remaining degrees of A at the finish. The first Count rows add up to the
   --  controls Scaled_Convolution (A, B, K, Count) / C(m + n, K), so all of them add up to the product.

   function Product_Row (B : Controls; Degree, Shift : Natural) return Controls
   with
     Pre  => Is_Polynomial (B) and then Shift <= Degree and then Degree <= Maximum_Degree,
     Post =>
       Product_Row'Result'First = 0
       and then Product_Row'Result'Last = Degree + B'Last
       and then
         (for all K in Product_Row'Result'Range =>
            Product_Row'Result (K)
            = (if K >= Shift and then K - Shift <= B'Last
               then Choose (B'Last, K - Shift) * B (K - Shift) / Choose (Degree + B'Last, K)
               else 0.0));
   --  The controls, Degree degrees higher, of T**Shift * (1 - T)**(Degree - Shift) times the polynomial B.

   function Product_Row (B : Controls; Degree, Shift : Natural) return Controls is
      Result : Controls (0 .. Degree + B'Last) := [others => 0.0];
   begin
      for K in Result'Range loop
         if K >= Shift and then K - Shift <= B'Last then
            Result (K) := Choose (B'Last, K - Shift) * B (K - Shift) / Choose (Degree + B'Last, K);
         end if;
         pragma
           Loop_Invariant
             (for all J in 0 .. K =>
                Result (J)
                = (if J >= Shift and then J - Shift <= B'Last
                   then Choose (B'Last, J - Shift) * B (J - Shift) / Choose (Degree + B'Last, J)
                   else 0.0));
      end loop;
      return Result;
   end Product_Row;

   procedure Lemma_Product_Row (B : Controls; Degree, Shift : Natural; T : Real)
   with
     Pre  => Is_Polynomial (B) and then Shift <= Degree and then Degree <= Maximum_Degree,
     Post =>
       Value (Product_Row (B, Degree, Shift), T) = Power (T, Shift) * Power (1.0 - T, Degree - Shift) * Value (B, T);
   --  The intermediate controls, B raised at the start only, may already be above the Maximum_Degree. This is
   --  where the relations between controls of any degree are needed.

   procedure Lemma_Product_Row (B : Controls; Degree, Shift : Natural; T : Real) is
      Row    : constant Controls := Product_Row (B, Degree, Shift);
      Lifted : constant Controls := Raised_At_Start (B, Shift);
   begin
      Lemma_Raised_At_Start (B, Shift, T);
      --  The row is B raised at the start, raised again at the finish: both hold the same scaled control of B over
      --  their own binomial coefficient.
      for J in Row'Range loop
         if J >= Shift and then J - Shift <= B'Last then
            pragma Assert (Choose (Row'Last, J) >= 1.0);
            pragma Assert (Choose (Lifted'Last, J) >= 1.0);
            pragma Assert (Row (J) = Choose (B'Last, J - Shift) * B (J - Shift) / Choose (Row'Last, J));
            Lemma_Division_Undone (Row (J), Choose (B'Last, J - Shift) * B (J - Shift), Choose (Row'Last, J));
            pragma Assert (Lifted (J) = B (J - Shift) * Choose (B'Last, J - Shift) / Choose (Lifted'Last, J));
            Lemma_Division_Undone (Lifted (J), B (J - Shift) * Choose (B'Last, J - Shift), Choose (Lifted'Last, J));
            pragma Assert (Row (J) * Choose (Row'Last, J) = Lifted (J) * Choose (Lifted'Last, J));
         else
            pragma Assert (Row (J) = 0.0);
            pragma Assert (if J < Shift then Lifted (J) = 0.0);
         end if;
         pragma Assert (Finish_Control_Agrees (Lifted, Row, Degree - Shift, J));
         pragma Loop_Invariant (for all K in 0 .. J => Finish_Control_Agrees (Lifted, Row, Degree - Shift, K));
      end loop;
      pragma Assert (Is_Raised_At_Finish (Lifted, Row, Degree - Shift));
      Lemma_Value_Raised_At_Finish (Lifted, Row, Degree - Shift, T);
      pragma Assert (Value (Row, T) = Power (1.0 - T, Degree - Shift) * (Power (T, Shift) * Value (B, T)));
   end Lemma_Product_Row;

   function Partial_Product (A, B : Controls; Count : Natural) return Controls
   with
     Pre  => Is_Polynomial (A) and then Is_Polynomial (B) and then Count <= A'Length,
     Post =>
       Partial_Product'Result'First = 0
       and then Partial_Product'Result'Last = A'Last + B'Last
       and then
         (for all K in Partial_Product'Result'Range =>
            Partial_Product'Result (K) = Scaled_Convolution (A, B, K, Count) / Choose (A'Last + B'Last, K));
   --  The controls of the product of B with the first Count terms of the Bernstein sum of A.

   function Partial_Product (A, B : Controls; Count : Natural) return Controls is
      Result : Controls (0 .. A'Last + B'Last) := [others => 0.0];
   begin
      for K in Result'Range loop
         Result (K) := Scaled_Convolution (A, B, K, Count) / Choose (A'Last + B'Last, K);
         pragma
           Loop_Invariant
             (for all J in 0 .. K => Result (J) = Scaled_Convolution (A, B, J, Count) / Choose (A'Last + B'Last, J));
      end loop;
      return Result;
   end Partial_Product;

   procedure Lemma_Division_Distributes (X, Factor, G, C : Real)
   with Pre => C /= 0.0, Post => (X + Factor * G) / C = X / C + Factor * (G / C);

   procedure Lemma_Division_Distributes (X, Factor, G, C : Real) is
      Q_X : constant Real := X / C;
      Q_G : constant Real := G / C;
   begin
      pragma Assert (Q_X * C = X);
      pragma Assert (Q_G * C = G);
      pragma Assert ((Q_X + Factor * Q_G) * C = Q_X * C + Factor * (Q_G * C));
      pragma Assert ((Q_X + Factor * Q_G) * C = X + Factor * G);
   end Lemma_Division_Distributes;

   procedure Lemma_Partial_Product_Control (A, B : Controls; Count : Positive; K : Natural)
   with
     Pre  => Is_Polynomial (A) and then Is_Polynomial (B) and then Count <= A'Length and then K <= A'Last + B'Last,
     Post =>
       Partial_Product (A, B, Count) (K)
       = Partial_Product (A, B, Count - 1) (K)
         + (Choose (A'Last, Count - 1) * A (Count - 1)) * Product_Row (B, A'Last, Count - 1) (K);
   --  Each control of a partial product is that of the partial product one row shorter plus the scaled control of
   --  the new row.

   procedure Lemma_Partial_Product_Control (A, B : Controls; Count : Positive; K : Natural) is
      I        : constant Natural := Count - 1;
      Current  : constant Controls := Partial_Product (A, B, Count);
      Previous : constant Controls := Partial_Product (A, B, Count - 1);
      Row      : constant Controls := Product_Row (B, A'Last, I);
      Factor   : constant Real := Choose (A'Last, I) * A (I);
      C        : constant Real := Choose (A'Last + B'Last, K);
      X        : constant Real := Scaled_Convolution (A, B, K, Count - 1);
   begin
      pragma Assert (C >= 1.0);
      pragma Assert (Previous (K) = X / C);
      pragma Assert (Current (K) = Scaled_Convolution (A, B, K, Count) / C);
      if K >= I and then K - I <= B'Last then
         declare
            G : constant Real := Choose (B'Last, K - I) * B (K - I);
         begin
            pragma Assert (Scaled_Convolution (A, B, K, Count) = X + Factor * G);
            pragma Assert (Row (K) = G / C);
            Lemma_Division_Distributes (X, Factor, G, C);
            pragma Assert (Current (K) = Previous (K) + Factor * Row (K));
         end;
      else
         pragma Assert (Scaled_Convolution (A, B, K, Count) = X);
         pragma Assert (Row (K) = 0.0);
         pragma Assert (Current (K) = Previous (K) + Factor * Row (K));
      end if;
   end Lemma_Partial_Product_Control;

   procedure Lemma_Row_Accumulates (Lower, Term, Other, Factor, Weight, Row, Scaled_Row, Previous, Current : Real)
   with
     Pre  =>
       Row = Weight * Other
       and then Scaled_Row = Factor * Row
       and then Factor * Weight = Term
       and then Previous = Lower * Other
       and then Current = Previous + Scaled_Row,
     Post => Current = (Lower + Term) * Other;
   --  The values in the step below: a row worth Weight times the other polynomial, scaled by Factor, joins the
   --  rows worth Lower times the other polynomial.

   procedure Lemma_Row_Accumulates (Lower, Term, Other, Factor, Weight, Row, Scaled_Row, Previous, Current : Real) is
   begin
      pragma Assert (Scaled_Row = Factor * (Weight * Other));
      pragma Assert (Factor * (Weight * Other) = (Factor * Weight) * Other);
      pragma Assert (Scaled_Row = Term * Other);
      pragma Assert ((Lower + Term) * Other = Lower * Other + Term * Other);
   end Lemma_Row_Accumulates;

   procedure Lemma_Partial_Product (A, B : Controls; Count : Natural; T : Real)
   with
     Pre                => Is_Polynomial (A) and then Is_Polynomial (B) and then Count <= A'Length,
     Post               => Value (Partial_Product (A, B, Count), T) = Partial_Sum (A, Count, T) * Value (B, T),
     Subprogram_Variant => (Decreases => Count);
   --  The rows accumulate: the first Count of them make the first Count terms of the sum of A, times B.

   procedure Lemma_Partial_Product (A, B : Controls; Count : Natural; T : Real) is
      Current : constant Controls := Partial_Product (A, B, Count);
   begin
      if Count = 0 then
         pragma Assert (for all K in Current'Range => Scaled_Convolution (A, B, K, 0) = 0.0);
         pragma Assert (for all K in Current'Range => Current (K) = 0.0);
         Lemma_Value_Of_Constant (Current, 0.0, T);
         pragma Assert (Partial_Sum (A, 0, T) = 0.0);
      else
         declare
            I          : constant Natural := Count - 1;
            Previous   : constant Controls := Partial_Product (A, B, Count - 1);
            Row        : constant Controls := Product_Row (B, A'Last, I);
            Factor     : constant Real := Choose (A'Last, I) * A (I);
            Scaled_Row : Controls (Row'Range) := [others => 0.0];
            Weight     : constant Real := Power (T, I) * Power (1.0 - T, A'Last - I);
            Term       : constant Real := A (I) * Basis (A'Last, I, T);
            Lower      : constant Real := Partial_Sum (A, Count - 1, T);
            Other      : constant Real := Value (B, T);
         begin
            for K in Row'Range loop
               Scaled_Row (K) := Factor * Row (K);
               Lemma_Partial_Product_Control (A, B, Count, K);
               pragma
                 Loop_Invariant
                   (for all J in 0 .. K =>
                      Scaled_Row (J) = Factor * Row (J) and then Current (J) = Previous (J) + Scaled_Row (J));
            end loop;
            Lemma_Partial_Product (A, B, Count - 1, T);
            Lemma_Product_Row (B, A'Last, I, T);
            Lemma_Value_Is_Homogeneous (Factor, Row, Scaled_Row, T);
            Lemma_Value_Is_Additive (Previous, Scaled_Row, Current, T);
            pragma Assert (Value (Row, T) = Weight * Other);
            pragma Assert (Basis (A'Last, I, T) = Choose (A'Last, I) * Power (T, I) * Power (1.0 - T, A'Last - I));
            pragma Assert (Basis (A'Last, I, T) = Choose (A'Last, I) * Weight);
            pragma Assert (Factor * Weight = Term);
            pragma Assert (Value (Previous, T) = Lower * Other);
            Lemma_Row_Accumulates
              (Lower      => Lower,
               Term       => Term,
               Other      => Other,
               Factor     => Factor,
               Weight     => Weight,
               Row        => Value (Row, T),
               Scaled_Row => Value (Scaled_Row, T),
               Previous   => Value (Previous, T),
               Current    => Value (Current, T));
            pragma Assert (Partial_Sum (A, Count, T) = Lower + Term);
            pragma Assert (Value (Current, T) = Partial_Sum (A, Count, T) * Other);
         end;
      end if;
   end Lemma_Partial_Product;

   function Product (A, B : Controls) return Controls
   is (Partial_Product (A, B, A'Length));

   procedure Lemma_Product (A, B : Controls; T : Real) is
   begin
      Lemma_Partial_Product (A, B, A'Length, T);
      Lemma_Value_Ignores_Offset (Product (A, B), Partial_Product (A, B, A'Length), T);
      Lemma_Value_Is_Bernstein_Sum (A, T);
   end Lemma_Product;

end Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;
