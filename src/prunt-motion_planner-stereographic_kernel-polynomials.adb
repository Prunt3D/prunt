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

package body Prunt.Motion_Planner.Stereographic_Kernel.Polynomials
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Coefficients and Horner's rule past the last coefficient
   ---------------------------------------------------------------------------

   function Coefficient (P : Polynomial; K : Natural) return Real
   is (if K in P'Range then P (K) else 0.0);
   --  The coefficient of the K-th power, which is zero past the last one stored.

   function Tail (P : Polynomial; From : Natural; X : Real) return Real
   is (if From > P'Last then 0.0 else Eval_From (P, From, X))
   with Pre => Is_Polynomial (P);
   --  Horner's rule from From upward, which is zero when there is no coefficient left.

   procedure Lemma_Tail_Step (P : Polynomial; From : Natural; X : Real)
   with
     Pre  => Is_Polynomial (P) and then From <= Maximum_Length,
     Post => Tail (P, From, X) = Coefficient (P, From) + X * Tail (P, From + 1, X);
   --  One step of Horner's rule, wherever From is.

   procedure Lemma_Tail_Step (P : Polynomial; From : Natural; X : Real) is
   begin
      if From > P'Last then
         pragma Assert (Tail (P, From, X) = 0.0 and then Tail (P, From + 1, X) = 0.0);
         pragma Assert (Coefficient (P, From) = 0.0);
      elsif From = P'Last then
         pragma Assert (Eval_From (P, From, X) = P (From));
         pragma Assert (Tail (P, From + 1, X) = 0.0);
      else
         pragma Assert (Eval_From (P, From, X) = P (From) + X * Eval_From (P, From + 1, X));
         pragma Assert (Tail (P, From + 1, X) = Eval_From (P, From + 1, X));
      end if;
   end Lemma_Tail_Step;

   ---------------------------------------------------------------------------
   --  Construction
   ---------------------------------------------------------------------------

   function Constant_Polynomial (Value : Real) return Polynomial is
      Result : constant Polynomial (0 .. 0) := [others => Value];
   begin
      return Result;
   end Constant_Polynomial;

   function Linear (Constant_Term, Slope : Real) return Polynomial is
      Result : Polynomial (0 .. 1) := [others => Constant_Term];
   begin
      Result (1) := Slope;
      return Result;
   end Linear;

   function Sum (P, Q : Polynomial) return Polynomial
   with
     Refined_Post =>
       Is_Polynomial (Sum'Result)
       and then Sum'Result'Last = Natural'Max (P'Last, Q'Last)
       and then (for all K in Sum'Result'Range => Sum'Result (K) = Coefficient (P, K) + Coefficient (Q, K))
   is
      Result : Polynomial (0 .. Natural'Max (P'Last, Q'Last)) := [others => 0.0];
   begin
      for K in Result'Range loop
         Result (K) := Coefficient (P, K) + Coefficient (Q, K);
         pragma Loop_Invariant (for all J in 0 .. K => Result (J) = Coefficient (P, J) + Coefficient (Q, J));
      end loop;
      return Result;
   end Sum;

   function Scaled (Factor : Real; P : Polynomial) return Polynomial
   with
     Refined_Post =>
       Is_Polynomial (Scaled'Result)
       and then Scaled'Result'Last = P'Last
       and then (for all K in P'Range => Scaled'Result (K) = Factor * P (K))
   is
      Result : Polynomial (0 .. P'Last) := [others => 0.0];
   begin
      for K in Result'Range loop
         Result (K) := Factor * P (K);
         pragma Loop_Invariant (for all J in 0 .. K => Result (J) = Factor * P (J));
      end loop;
      return Result;
   end Scaled;

   function Difference (P, Q : Polynomial) return Polynomial
   is (Sum (P, Scaled (-1.0, Q)));
   --  P plus Q with every coefficient negated.

   function Shifted (P : Polynomial) return Polynomial
   with
     Pre  => Is_Polynomial (P) and then P'Last < Maximum_Length - 1,
     Post =>
       Is_Polynomial (Shifted'Result)
       and then Shifted'Result'Last = P'Last + 1
       and then Shifted'Result (0) = 0.0
       and then (for all K in P'Range => Shifted'Result (K + 1) = P (K));
   --  P times X: every coefficient moves up by one power.

   function Shifted (P : Polynomial) return Polynomial is
      Result : Polynomial (0 .. P'Last + 1) := [others => 0.0];
   begin
      for K in P'Range loop
         Result (K + 1) := P (K);
         pragma Loop_Invariant (Result (0) = 0.0);
         pragma Loop_Invariant (for all J in 0 .. K => Result (J + 1) = P (J));
      end loop;
      return Result;
   end Shifted;

   function Product_From (P, Q : Polynomial; From : Natural) return Polynomial
   is (if From = P'Last
       then Scaled (P (From), Q)
       else Sum (Scaled (P (From), Q), Shifted (Product_From (P, Q, From + 1))))
   with
     Subprogram_Variant => (Decreases => P'Last - From),
     Pre                =>
       Is_Polynomial (P) and then Is_Polynomial (Q) and then P'Last + Q'Last < Maximum_Length and then From <= P'Last,
     Post               =>
       Is_Polynomial (Product_From'Result) and then Product_From'Result'Last = P'Last - From + Q'Last;
   --  Q times the polynomial whose coefficients are those of P from From upward, by Horner's rule on P: the
   --  coefficient at From times Q, plus X times the product for the coefficients above it.

   function Product (P, Q : Polynomial) return Polynomial
   is (Product_From (P, Q, 0));

   function Bernstein_Polynomial (Controls : Bernstein.Controls) return Polynomial
   is (if Controls'Length = 1
       then Constant_Polynomial (Controls (Controls'First))
       else
         Sum
           (Bernstein_Polynomial (Controls (Controls'First .. Controls'Last - 1)),
            Shifted
              (Difference
                 (Bernstein_Polynomial (Controls (Controls'First + 1 .. Controls'Last)),
                  Bernstein_Polynomial (Controls (Controls'First .. Controls'Last - 1))))))
   with
     Subprogram_Variant => (Decreases => Controls'Length),
     Pre                => Controls'Length >= 1 and then Controls'Last < Maximum_Length,
     Post               =>
       Is_Polynomial (Bernstein_Polynomial'Result) and then Bernstein_Polynomial'Result'Last = Controls'Length - 1;
   --  De Casteljau's recurrence on polynomials. With A for the controls without the last and B for the controls
   --  without the first, (1 - X)·A + X·B is A + X·(B - A).

   function From_Bernstein (Controls : Bernstein.Controls) return Polynomial
   is (Bernstein_Polynomial (Controls));

   ---------------------------------------------------------------------------
   --  Values
   ---------------------------------------------------------------------------

   procedure Lemma_Eval_Constant (Value, X : Real) is
      P : constant Polynomial := Constant_Polynomial (Value);
   begin
      pragma Assert (Eval_From (P, 0, X) = P (0));
   end Lemma_Eval_Constant;

   procedure Lemma_Eval_Linear (Constant_Term, Slope, X : Real) is
      P : constant Polynomial := Linear (Constant_Term, Slope);
   begin
      pragma Assert (Eval_From (P, 1, X) = P (1));
      pragma Assert (Eval_From (P, 0, X) = P (0) + X * Eval_From (P, 1, X));
      pragma Assert (X * Slope = Slope * X);
   end Lemma_Eval_Linear;

   procedure Lemma_Sum_From (P, Q : Polynomial; From : Natural; X : Real)
   with
     Subprogram_Variant => (Decreases => Natural'Max (P'Last, Q'Last) - From),
     Pre                => Is_Polynomial (P) and then Is_Polynomial (Q) and then From <= Natural'Max (P'Last, Q'Last),
     Post               => Eval_From (Sum (P, Q), From, X) = Tail (P, From, X) + Tail (Q, From, X);
   --  Horner's rule on the sum, from any coefficient upward, is the sum of Horner's rule on each.

   procedure Lemma_Sum_From (P, Q : Polynomial; From : Natural; X : Real) is
      S      : constant Polynomial := Sum (P, Q);
      Rest_P : constant Real := Tail (P, From + 1, X);
      Rest_Q : constant Real := Tail (Q, From + 1, X);
   begin
      Lemma_Tail_Step (P, From, X);
      Lemma_Tail_Step (Q, From, X);
      pragma Assert (S (From) = Coefficient (P, From) + Coefficient (Q, From));
      if From = S'Last then
         pragma Assert (Rest_P = 0.0 and then Rest_Q = 0.0);
         pragma Assert (Eval_From (S, From, X) = S (From));
      else
         Lemma_Sum_From (P, Q, From + 1, X);
         pragma Assert (Eval_From (S, From + 1, X) = Rest_P + Rest_Q);
         pragma Assert (Eval_From (S, From, X) = S (From) + X * Eval_From (S, From + 1, X));
         pragma Assert (X * (Rest_P + Rest_Q) = X * Rest_P + X * Rest_Q);
      end if;
   end Lemma_Sum_From;

   procedure Lemma_Eval_Sum (P, Q : Polynomial; X : Real) is
   begin
      Lemma_Sum_From (P, Q, 0, X);
   end Lemma_Eval_Sum;

   procedure Lemma_Scaled_From (Factor : Real; P : Polynomial; From : Natural; X : Real)
   with
     Subprogram_Variant => (Decreases => P'Last - From),
     Pre                => Is_Polynomial (P) and then From <= P'Last,
     Post               => Eval_From (Scaled (Factor, P), From, X) = Factor * Eval_From (P, From, X);
   --  Horner's rule on the scaled coefficients, from any coefficient upward.

   procedure Lemma_Scaled_From (Factor : Real; P : Polynomial; From : Natural; X : Real) is
      S : constant Polynomial := Scaled (Factor, P);
   begin
      pragma Assert (S (From) = Factor * P (From));
      if From = P'Last then
         pragma Assert (Eval_From (S, From, X) = S (From));
         pragma Assert (Eval_From (P, From, X) = P (From));
      else
         declare
            Rest : constant Real := Eval_From (P, From + 1, X);
         begin
            Lemma_Scaled_From (Factor, P, From + 1, X);
            pragma Assert (Eval_From (S, From + 1, X) = Factor * Rest);
            pragma Assert (Eval_From (S, From, X) = S (From) + X * Eval_From (S, From + 1, X));
            pragma Assert (Eval_From (P, From, X) = P (From) + X * Rest);
            pragma Assert (X * (Factor * Rest) = Factor * (X * Rest));
            pragma Assert (Factor * (P (From) + X * Rest) = Factor * P (From) + Factor * (X * Rest));
         end;
      end if;
   end Lemma_Scaled_From;

   procedure Lemma_Eval_Scaled (Factor : Real; P : Polynomial; X : Real) is
   begin
      Lemma_Scaled_From (Factor, P, 0, X);
   end Lemma_Eval_Scaled;

   procedure Lemma_Eval_Difference (P, Q : Polynomial; X : Real) is
   begin
      Lemma_Eval_Scaled (-1.0, Q, X);
      Lemma_Eval_Sum (P, Scaled (-1.0, Q), X);
      pragma Assert (Eval (Difference (P, Q), X) = Eval (P, X) + (-1.0) * Eval (Q, X));
   end Lemma_Eval_Difference;

   procedure Lemma_Shifted_From (P : Polynomial; From : Natural; X : Real)
   with
     Subprogram_Variant => (Decreases => P'Last - From),
     Pre                => Is_Polynomial (P) and then P'Last < Maximum_Length - 1 and then From <= P'Last,
     Post               => Eval_From (Shifted (P), From + 1, X) = Eval_From (P, From, X);
   --  Horner's rule sees the same coefficients one place higher.

   procedure Lemma_Shifted_From (P : Polynomial; From : Natural; X : Real) is
      S : constant Polynomial := Shifted (P);
   begin
      pragma Assert (S (From + 1) = P (From));
      if From = P'Last then
         pragma Assert (Eval_From (S, From + 1, X) = S (From + 1));
         pragma Assert (Eval_From (P, From, X) = P (From));
      else
         Lemma_Shifted_From (P, From + 1, X);
         pragma Assert (Eval_From (S, From + 1, X) = S (From + 1) + X * Eval_From (S, From + 2, X));
         pragma Assert (Eval_From (P, From, X) = P (From) + X * Eval_From (P, From + 1, X));
      end if;
   end Lemma_Shifted_From;

   procedure Lemma_Eval_Shifted (P : Polynomial; X : Real)
   with Pre => Is_Polynomial (P) and then P'Last < Maximum_Length - 1, Post => Eval (Shifted (P), X) = X * Eval (P, X);
   --  Moving every coefficient up by one power multiplies by X.

   procedure Lemma_Eval_Shifted (P : Polynomial; X : Real) is
      S : constant Polynomial := Shifted (P);
   begin
      Lemma_Shifted_From (P, 0, X);
      pragma Assert (Eval_From (S, 0, X) = S (0) + X * Eval_From (S, 1, X));
   end Lemma_Eval_Shifted;

   procedure Lemma_Product_From (P, Q : Polynomial; From : Natural; X : Real)
   with
     Subprogram_Variant => (Decreases => P'Last - From),
     Pre                =>
       Is_Polynomial (P) and then Is_Polynomial (Q) and then P'Last + Q'Last < Maximum_Length and then From <= P'Last,
     Post               => Eval (Product_From (P, Q, From), X) = Eval_From (P, From, X) * Eval (Q, X);
   --  The product built by Horner's rule on P has the product of the values, from any coefficient of P upward.

   procedure Lemma_Product_From (P, Q : Polynomial; From : Natural; X : Real) is
      Value_Q : constant Real := Eval (Q, X);
   begin
      Lemma_Eval_Scaled (P (From), Q, X);
      if From = P'Last then
         pragma Assert (Eval_From (P, From, X) = P (From));
         pragma Assert (Eval (Product_From (P, Q, From), X) = Eval (Scaled (P (From), Q), X));
      else
         declare
            Rest   : constant Polynomial := Product_From (P, Q, From + 1);
            Rest_P : constant Real := Eval_From (P, From + 1, X);
         begin
            Lemma_Product_From (P, Q, From + 1, X);
            pragma Assert (Eval (Rest, X) = Rest_P * Value_Q);
            Lemma_Eval_Shifted (Rest, X);
            Lemma_Eval_Sum (Scaled (P (From), Q), Shifted (Rest), X);
            pragma Assert (Eval (Product_From (P, Q, From), X) = Eval (Sum (Scaled (P (From), Q), Shifted (Rest)), X));
            pragma Assert (Eval (Product_From (P, Q, From), X) = P (From) * Value_Q + X * (Rest_P * Value_Q));
            pragma Assert (Eval_From (P, From, X) = P (From) + X * Rest_P);
            pragma Assert ((P (From) + X * Rest_P) * Value_Q = P (From) * Value_Q + (X * Rest_P) * Value_Q);
            pragma Assert ((X * Rest_P) * Value_Q = X * (Rest_P * Value_Q));
         end;
      end if;
   end Lemma_Product_From;

   procedure Lemma_Eval_Product (P, Q : Polynomial; X : Real) is
   begin
      Lemma_Product_From (P, Q, 0, X);
   end Lemma_Eval_Product;

   procedure Lemma_Recurrence_Weights (A, B, T : Real)
   with Post => A + T * (B - A) = (1.0 - T) * A + T * B;
   --  The form of de Casteljau's recurrence which Bernstein_Polynomial builds is the form which defines the value.

   procedure Lemma_Recurrence_Weights (A, B, T : Real) is
   begin
      pragma Assert (T * (B - A) = T * B - T * A);
      pragma Assert ((1.0 - T) * A = A - T * A);
   end Lemma_Recurrence_Weights;

   procedure Lemma_Eval_Bernstein_Polynomial (Controls : Bernstein.Controls; T : Real)
   with
     Subprogram_Variant => (Decreases => Controls'Length),
     Pre                => Controls'Length >= 1 and then Controls'Last < Maximum_Length,
     Post               => Eval (Bernstein_Polynomial (Controls), T) = Bernstein.Value (Controls, T);
   --  By induction on the number of controls, wherever their index range starts.

   procedure Lemma_Eval_Bernstein_Polynomial (Controls : Bernstein.Controls; T : Real) is
   begin
      if Controls'Length = 1 then
         Lemma_Eval_Constant (Controls (Controls'First), T);
         pragma
           Assert
             (Eval (Bernstein_Polynomial (Controls), T) = Eval (Constant_Polynomial (Controls (Controls'First)), T));
         pragma Assert (Bernstein.Value (Controls, T) = Controls (Controls'First));
      else
         declare
            Low   : constant Bernstein.Controls := Controls (Controls'First .. Controls'Last - 1);
            High  : constant Bernstein.Controls := Controls (Controls'First + 1 .. Controls'Last);
            A     : constant Polynomial := Bernstein_Polynomial (Low);
            B     : constant Polynomial := Bernstein_Polynomial (High);
            Slope : constant Polynomial := Difference (B, A);
            Left  : constant Real := Bernstein.Value (Low, T);
            Right : constant Real := Bernstein.Value (High, T);
         begin
            Lemma_Eval_Bernstein_Polynomial (Low, T);
            Lemma_Eval_Bernstein_Polynomial (High, T);
            pragma Assert (Eval (A, T) = Left and then Eval (B, T) = Right);
            Lemma_Eval_Difference (B, A, T);
            Lemma_Eval_Shifted (Slope, T);
            Lemma_Eval_Sum (A, Shifted (Slope), T);
            pragma Assert (Eval (Sum (A, Shifted (Slope)), T) = Left + T * (Right - Left));
            pragma Assert (Eval (Bernstein_Polynomial (Controls), T) = Eval (Sum (A, Shifted (Slope)), T));
            pragma Assert (Bernstein.Value (Controls, T) = (1.0 - T) * Left + T * Right);
            Lemma_Recurrence_Weights (Left, Right, T);
         end;
      end if;
   end Lemma_Eval_Bernstein_Polynomial;

   procedure Lemma_Eval_From_Bernstein (Controls : Bernstein.Controls; T : Real) is
   begin
      Lemma_Eval_Bernstein_Polynomial (Controls, T);
   end Lemma_Eval_From_Bernstein;

   ---------------------------------------------------------------------------
   --  Division by a linear factor
   ---------------------------------------------------------------------------

   function Quotient (P : Polynomial; Root : Real) return Polynomial
   with
     Refined_Post =>
       Is_Polynomial (Quotient'Result)
       and then Quotient'Result'Last = P'Last - 1
       and then (for all K in 0 .. P'Last - 1 => Quotient'Result (K) = Eval_From (P, K + 1, Root))
   is
      Result : Polynomial (0 .. P'Last - 1) := [others => 0.0];
   begin
      --  Synthetic division: each coefficient of the quotient is Horner's rule at the root over the coefficients
      --  of P above it.
      for K in Result'Range loop
         Result (K) := Eval_From (P, K + 1, Root);
         pragma Loop_Invariant (for all J in 0 .. K => Result (J) = Eval_From (P, J + 1, Root));
      end loop;
      return Result;
   end Quotient;

   procedure Lemma_Quotient_Start (C, A, Root, X : Real)
   with Post => C + X * A = (C + Root * A) + (X - Root) * A;
   --  The algebra of the first step of Lemma_Quotient_From, at the last coefficient of the quotient, which is the
   --  last coefficient A of P.

   procedure Lemma_Quotient_Start (C, A, Root, X : Real) is
   begin
      pragma Assert ((X - Root) * A = X * A - Root * A);
   end Lemma_Quotient_Start;

   procedure Lemma_Quotient_Step (C, A, B, Root, X : Real)
   with Post => C + X * (A + (X - Root) * B) = (C + Root * A) + (X - Root) * (A + X * B);
   --  The algebra of one step of Lemma_Quotient_From: C is the coefficient of P, A the value at the root of the
   --  coefficients of P above it, and B the value at X of the coefficients of the quotient above it.

   procedure Lemma_Quotient_Step (C, A, B, Root, X : Real) is
      D : constant Real := X - Root;
   begin
      pragma Assert (X * (A + D * B) = X * A + X * (D * B));
      pragma Assert (D * (A + X * B) = D * A + D * (X * B));
      pragma Assert (D * A = X * A - Root * A);
      pragma Assert (X * (D * B) = D * (X * B));
   end Lemma_Quotient_Step;

   procedure Lemma_Quotient_From (P : Polynomial; Root, X : Real; From : Natural)
   with
     Subprogram_Variant => (Decreases => P'Last - From),
     Pre                => Is_Polynomial (P) and then P'Last >= 1 and then From < P'Last,
     Post               =>
       Eval_From (P, From, X) = Eval_From (P, From, Root) + (X - Root) * Eval_From (Quotient (P, Root), From, X);
   --  The division holds for Horner's rule from any coefficient upward.

   procedure Lemma_Quotient_From (P : Polynomial; Root, X : Real; From : Natural) is
      Q : constant Polynomial := Quotient (P, Root);
      C : constant Real := P (From);
      A : constant Real := Eval_From (P, From + 1, Root);
   begin
      pragma Assert (Q (From) = A);
      pragma Assert (Eval_From (P, From, X) = C + X * Eval_From (P, From + 1, X));
      pragma Assert (Eval_From (P, From, Root) = C + Root * A);
      if From = P'Last - 1 then
         pragma Assert (Eval_From (P, From + 1, X) = P (From + 1));
         pragma Assert (A = P (From + 1));
         pragma Assert (Eval_From (P, From, X) = C + X * A);
         pragma Assert (Eval_From (Q, From, X) = A);
         Lemma_Quotient_Start (C, A, Root, X);
      else
         declare
            B : constant Real := Eval_From (Q, From + 1, X);
         begin
            Lemma_Quotient_From (P, Root, X, From + 1);
            pragma Assert (Eval_From (P, From + 1, X) = A + (X - Root) * B);
            pragma Assert (Eval_From (P, From, X) = C + X * (A + (X - Root) * B));
            pragma Assert (Eval_From (Q, From, X) = A + X * B);
            Lemma_Quotient_Step (C, A, B, Root, X);
         end;
      end if;
   end Lemma_Quotient_From;

   procedure Lemma_Quotient (P : Polynomial; Root, X : Real) is
   begin
      Lemma_Quotient_From (P, Root, X, 0);
   end Lemma_Quotient;

end Prunt.Motion_Planner.Stereographic_Kernel.Polynomials;
