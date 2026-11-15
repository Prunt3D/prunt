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

--  The Lagrange identity is proved by induction on the number of nodes, without counting roots. Write xᵢ for the
--  nodes and Lᵢⁿ for the basis polynomial of node i over the first n nodes. For a polynomial P with at most n
--  coefficients and n >= 2, let W = Lₙⁿ as a polynomial and R = P - P(xₙ)·W. R vanishes at xₙ, so synthetic
--  division gives R(X) = (X - xₙ)·Q(X) with Q of at most n - 1 coefficients, and R agrees with P at the earlier
--  nodes because W vanishes there. By induction Q(X) = Σ Q(xᵢ)·Lᵢⁿ⁻¹(X) over i < n, and as
--  Lᵢⁿ(X)·(xᵢ - xₙ) = Lᵢⁿ⁻¹(X)·(X - xₙ), multiplying by X - xₙ gives R(X) = Σ P(xᵢ)·Lᵢⁿ(X) over i < n. Adding
--  P(xₙ)·Lₙⁿ(X) to both sides is the identity for P.

package body Prunt.Motion_Planner.Stereographic_Kernel.Polynomials.Interpolation
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Real arithmetic
   ---------------------------------------------------------------------------

   procedure Lemma_Product_Is_Nonzero (A, B : Real)
   with Pre => A /= 0.0 and then B /= 0.0, Post => A * B /= 0.0;
   --  The reals have no zero divisors.

   procedure Lemma_Product_Is_Nonzero (A, B : Real) is null;

   procedure Lemma_Ratio_Step (Num, Den, A, E : Real)
   with
     Pre  => Den /= 0.0 and then E /= 0.0 and then Den * E /= 0.0,
     Post => ((Num * A) / (Den * E)) * E = (Num / Den) * A;
   --  Extending a ratio by one factor above and one below, then cancelling the one below.

   procedure Lemma_Ratio_Step (Num, Den, A, E : Real) is
      Long  : constant Real := (Num * A) / (Den * E);
      Short : constant Real := Num / Den;
   begin
      pragma Assert (Long * (Den * E) = Num * A);
      pragma Assert (Short * Den = Num);
      pragma Assert ((Long * E) * Den = Long * (Den * E));
      pragma Assert ((Short * A) * Den = (Short * Den) * A);
      pragma Assert ((Long * E - Short * A) * Den = (Long * E) * Den - (Short * A) * Den);
      pragma Assert ((Long * E - Short * A) * Den = 0.0);
      pragma Assert (Long * E - Short * A = 0.0);
   end Lemma_Ratio_Step;

   procedure Lemma_Reciprocal_Scales (Num, Den : Real)
   with Pre => Den /= 0.0, Post => (1.0 / Den) * Num = Num / Den;
   --  Multiplying by a reciprocal divides.

   procedure Lemma_Reciprocal_Scales (Num, Den : Real) is
      Inverse : constant Real := 1.0 / Den;
      Ratio   : constant Real := Num / Den;
   begin
      pragma Assert (Inverse * Den = 1.0);
      pragma Assert (Ratio * Den = Num);
      pragma Assert ((Inverse * Num) * Den = (Inverse * Den) * Num);
      pragma Assert ((Inverse * Num - Ratio) * Den = (Inverse * Num) * Den - Ratio * Den);
      pragma Assert ((Inverse * Num - Ratio) * Den = 0.0);
      pragma Assert (Inverse * Num - Ratio = 0.0);
   end Lemma_Reciprocal_Scales;

   procedure Lemma_Term_Step (Value, Reduced_Value, Basis_New, Basis_Old, A, E : Real)
   with
     Pre  => Value = E * Reduced_Value and then Basis_New * E = Basis_Old * A,
     Post => Value * Basis_New = A * (Reduced_Value * Basis_Old);
   --  One term of the interpolant over n nodes is X - xₙ times the matching term of the interpolant of the
   --  reduced polynomial over n - 1 nodes.

   procedure Lemma_Term_Step (Value, Reduced_Value, Basis_New, Basis_Old, A, E : Real) is
   begin
      pragma Assert (Value * Basis_New = (E * Reduced_Value) * Basis_New);
      pragma Assert ((E * Reduced_Value) * Basis_New = Reduced_Value * (Basis_New * E));
      pragma Assert (Reduced_Value * (Basis_New * E) = Reduced_Value * (Basis_Old * A));
      pragma Assert (Reduced_Value * (Basis_Old * A) = A * (Reduced_Value * Basis_Old));
   end Lemma_Term_Step;

   procedure Lemma_Scaled_Magnitude (Value, Weight, Largest : Real)
   with Pre => abs Value <= Largest, Post => abs (Value * Weight) <= Largest * abs Weight;
   --  One term of the triangle inequality: a weighted value is bounded by the weight's magnitude times a bound on
   --  the value's.

   procedure Lemma_Scaled_Magnitude (Value, Weight, Largest : Real) is
   begin
      pragma Assert (abs (Value * Weight) = abs Value * abs Weight);
      pragma Assert (0.0 <= (Largest - abs Value) * abs Weight);
      pragma Assert ((Largest - abs Value) * abs Weight = Largest * abs Weight - abs Value * abs Weight);
   end Lemma_Scaled_Magnitude;

   procedure Lemma_Scaling_Keeps_Order (Factor, Low, High : Real)
   with Pre => 0.0 <= Factor and then Low <= High, Post => Factor * Low <= Factor * High;
   --  Multiplying both sides of an inequality by a factor which is not negative keeps the inequality.

   procedure Lemma_Scaling_Keeps_Order (Factor, Low, High : Real) is
   begin
      pragma Assert (0.0 <= Factor * (High - Low));
      pragma Assert (Factor * (High - Low) = Factor * High - Factor * Low);
   end Lemma_Scaling_Keeps_Order;

   ---------------------------------------------------------------------------
   --  Nodes and the products over them
   ---------------------------------------------------------------------------

   procedure Lemma_Nodes_Are_Ordered (Nodes : Node_Array; Low, High : Positive)
   with
     Subprogram_Variant => (Decreases => High),
     Pre                => Is_Increasing (Nodes) and then Low < High and then High <= Nodes'Last,
     Post               => Nodes (Low) < Nodes (High);
   --  Nodes which each exceed the one before are in order however far apart.

   procedure Lemma_Nodes_Are_Ordered (Nodes : Node_Array; Low, High : Positive) is
   begin
      pragma Assert (Nodes (High - 1) < Nodes (High - 1 + 1));
      if Low < High - 1 then
         Lemma_Nodes_Are_Ordered (Nodes, Low, High - 1);
      end if;
   end Lemma_Nodes_Are_Ordered;

   procedure Lemma_Numerator_Step (Nodes : Node_Array; I : Positive; X : Real; Count : Positive)
   with
     Pre  => Nodes'First = 1 and then Count <= Nodes'Last,
     Post =>
       Numerator (Nodes, I, X, Count)
       = (if Count = I
          then Numerator (Nodes, I, X, Count - 1)
          else Numerator (Nodes, I, X, Count - 1) * (X - Nodes (Count)));
   --  The definition of Numerator, for one more node.

   procedure Lemma_Numerator_Step (Nodes : Node_Array; I : Positive; X : Real; Count : Positive) is null;

   procedure Lemma_Numerator_Is_Nonzero (Nodes : Node_Array; I : Positive; Count : Natural)
   with
     Subprogram_Variant => (Decreases => Count),
     Pre                => Is_Increasing (Nodes) and then I <= Nodes'Last and then Count <= Nodes'Last,
     Post               => Numerator (Nodes, I, Nodes (I), Count) /= 0.0;
   --  No factor of the product at its own node is zero, as the nodes are distinct.

   procedure Lemma_Numerator_Is_Nonzero (Nodes : Node_Array; I : Positive; Count : Natural) is
   begin
      if Count = 0 then
         pragma Assert (Numerator (Nodes, I, Nodes (I), Count) = 1.0);
      else
         Lemma_Numerator_Is_Nonzero (Nodes, I, Count - 1);
         Lemma_Numerator_Step (Nodes, I, Nodes (I), Count);
         if Count < I then
            Lemma_Nodes_Are_Ordered (Nodes, Count, I);
            Lemma_Product_Is_Nonzero (Numerator (Nodes, I, Nodes (I), Count - 1), Nodes (I) - Nodes (Count));
         elsif Count > I then
            Lemma_Nodes_Are_Ordered (Nodes, I, Count);
            Lemma_Product_Is_Nonzero (Numerator (Nodes, I, Nodes (I), Count - 1), Nodes (I) - Nodes (Count));
         end if;
      end if;
   end Lemma_Numerator_Is_Nonzero;

   procedure Lemma_Weight_Denominator_Is_Nonzero (Nodes : Node_Array; I : Positive) is
   begin
      Lemma_Numerator_Is_Nonzero (Nodes, I, Nodes'Last);
   end Lemma_Weight_Denominator_Is_Nonzero;

   procedure Lemma_Numerator_Vanishes (Nodes : Node_Array; I, K : Positive; Count : Natural)
   with
     Subprogram_Variant => (Decreases => Count),
     Pre                => Nodes'First = 1 and then Count <= Nodes'Last and then K <= Count and then K /= I,
     Post               => Numerator (Nodes, I, Nodes (K), Count) = 0.0;
   --  The product is zero at any other node which it covers.

   procedure Lemma_Numerator_Vanishes (Nodes : Node_Array; I, K : Positive; Count : Natural) is
      Before : constant Real := Numerator (Nodes, I, Nodes (K), Count - 1);
   begin
      Lemma_Numerator_Step (Nodes, I, Nodes (K), Count);
      if Count = K then
         pragma Assert (Nodes (K) - Nodes (Count) = 0.0);
         pragma Assert (Before * 0.0 = 0.0);
      else
         Lemma_Numerator_Vanishes (Nodes, I, K, Count - 1);
         pragma Assert (Before = 0.0);
         pragma Assert (0.0 * (Nodes (K) - Nodes (Count)) = 0.0);
      end if;
   end Lemma_Numerator_Vanishes;

   ---------------------------------------------------------------------------
   --  The basis over the first nodes, and the interpolant
   ---------------------------------------------------------------------------

   function Partial_Basis (Nodes : Node_Array; Size, I : Positive; X : Real) return Real
   is (if Numerator (Nodes, I, Nodes (I), Size) = 0.0
       then 0.0
       else Numerator (Nodes, I, X, Size) / Numerator (Nodes, I, Nodes (I), Size))
   with Pre => Nodes'First = 1 and then Size <= Nodes'Last and then I <= Nodes'Last;
   --  The Lagrange basis polynomial of node I over the first Size nodes, at X. Over all the nodes it is Basis.

   function Interpolant (P : Polynomial; Nodes : Node_Array; Size : Positive; X : Real; Count : Natural) return Real
   is (if Count = 0
       then 0.0
       else
         Interpolant (P, Nodes, Size, X, Count - 1) + Eval (P, Nodes (Count)) * Partial_Basis (Nodes, Size, Count, X))
   with
     Subprogram_Variant => (Decreases => Count),
     Pre                =>
       Is_Polynomial (P) and then Nodes'First = 1 and then Size <= Nodes'Last and then Count <= Size;
   --  The first Count terms of the polynomial which interpolates P at the first Size nodes, at X.

   procedure Lemma_Basis_Step (Nodes : Node_Array; Size, I : Positive; X : Real)
   with
     Pre  => Is_Increasing (Nodes) and then Size <= Nodes'Last and then I < Size,
     Post =>
       Partial_Basis (Nodes, Size, I, X) * (Nodes (I) - Nodes (Size))
       = Partial_Basis (Nodes, Size - 1, I, X) * (X - Nodes (Size));
   --  Taking one more node into the basis polynomial of an earlier node I multiplies it by
   --  (X - x) / (xᵢ - x) for the new node x.

   procedure Lemma_Basis_Step (Nodes : Node_Array; Size, I : Positive; X : Real) is
      Num : constant Real := Numerator (Nodes, I, X, Size - 1);
      Den : constant Real := Numerator (Nodes, I, Nodes (I), Size - 1);
      A   : constant Real := X - Nodes (Size);
      E   : constant Real := Nodes (I) - Nodes (Size);
   begin
      Lemma_Numerator_Is_Nonzero (Nodes, I, Size - 1);
      Lemma_Numerator_Is_Nonzero (Nodes, I, Size);
      Lemma_Nodes_Are_Ordered (Nodes, I, Size);
      pragma Assert (Den /= 0.0 and then E /= 0.0);
      Lemma_Numerator_Step (Nodes, I, X, Size);
      Lemma_Numerator_Step (Nodes, I, Nodes (I), Size);
      pragma Assert (Numerator (Nodes, I, X, Size) = Num * A);
      pragma Assert (Numerator (Nodes, I, Nodes (I), Size) = Den * E);
      pragma Assert (Den * E /= 0.0);
      Lemma_Ratio_Step (Num, Den, A, E);
      pragma Assert (Partial_Basis (Nodes, Size, I, X) = (Num * A) / (Den * E));
      pragma Assert (Partial_Basis (Nodes, Size - 1, I, X) = Num / Den);
   end Lemma_Basis_Step;

   ---------------------------------------------------------------------------
   --  The basis polynomial of the last node, as a polynomial
   ---------------------------------------------------------------------------

   function Node_Polynomial (Nodes : Node_Array; Count : Natural) return Polynomial
   is (if Count = 0
       then Constant_Polynomial (1.0)
       else Product (Node_Polynomial (Nodes, Count - 1), Linear (-Nodes (Count), 1.0)))
   with
     Subprogram_Variant => (Decreases => Count),
     Pre                => Nodes'First = 1 and then Count <= Nodes'Last and then Nodes'Last < Maximum_Length,
     Post               => Is_Polynomial (Node_Polynomial'Result) and then Node_Polynomial'Result'Last = Count;
   --  The product of X - Nodes (J) over J in 1 .. Count, as a polynomial in X.

   procedure Lemma_Eval_Node_Polynomial (Nodes : Node_Array; Count : Natural; I : Positive; X : Real)
   with
     Subprogram_Variant => (Decreases => Count),
     Pre                =>
       Nodes'First = 1 and then Count <= Nodes'Last and then Nodes'Last < Maximum_Length and then Count < I,
     Post               => Eval (Node_Polynomial (Nodes, Count), X) = Numerator (Nodes, I, X, Count);
   --  That polynomial is the numerator of any node past the ones it covers.

   procedure Lemma_Eval_Node_Polynomial (Nodes : Node_Array; Count : Natural; I : Positive; X : Real) is
   begin
      if Count = 0 then
         Lemma_Eval_Constant (1.0, X);
         pragma Assert (Eval (Node_Polynomial (Nodes, Count), X) = Eval (Constant_Polynomial (1.0), X));
         pragma Assert (Numerator (Nodes, I, X, Count) = 1.0);
      else
         declare
            Before : constant Polynomial := Node_Polynomial (Nodes, Count - 1);
            Factor : constant Polynomial := Linear (-Nodes (Count), 1.0);
            Value  : constant Real := Numerator (Nodes, I, X, Count - 1);
         begin
            Lemma_Eval_Node_Polynomial (Nodes, Count - 1, I, X);
            pragma Assert (Eval (Before, X) = Value);
            Lemma_Eval_Linear (-Nodes (Count), 1.0, X);
            pragma Assert (Eval (Factor, X) = X - Nodes (Count));
            Lemma_Eval_Product (Before, Factor, X);
            pragma Assert (Eval (Node_Polynomial (Nodes, Count), X) = Eval (Product (Before, Factor), X));
            pragma Assert (Eval (Node_Polynomial (Nodes, Count), X) = Value * (X - Nodes (Count)));
            Lemma_Numerator_Step (Nodes, I, X, Count);
         end;
      end if;
   end Lemma_Eval_Node_Polynomial;

   function Last_Weight (Nodes : Node_Array; Size : Positive) return Real
   is (Numerator (Nodes, Size, Nodes (Size), Size - 1))
   with Pre => Nodes'First = 1 and then Size <= Nodes'Last;
   --  The product of the last node's distances from the earlier ones.

   function Reciprocal (Value : Real) return Real
   is (if Value = 0.0 then 0.0 else 1.0 / Value);
   --  One over Value. Value is never zero where this is used: the zero only keeps the function total.

   function Last_Basis_Polynomial (Nodes : Node_Array; Size : Positive) return Polynomial
   is (Scaled (Reciprocal (Last_Weight (Nodes, Size)), Node_Polynomial (Nodes, Size - 1)))
   with
     Pre  => Nodes'First = 1 and then Size <= Nodes'Last and then Nodes'Last < Maximum_Length,
     Post => Is_Polynomial (Last_Basis_Polynomial'Result) and then Last_Basis_Polynomial'Result'Last = Size - 1;
   --  The basis polynomial of node Size over the first Size nodes, as a polynomial.

   procedure Lemma_Eval_Last_Basis_Scaled (Nodes : Node_Array; Size : Positive; X : Real)
   with
     Pre  => Is_Increasing (Nodes) and then Size <= Nodes'Last,
     Post =>
       Last_Weight (Nodes, Size) /= 0.0
       and then
         Eval (Last_Basis_Polynomial (Nodes, Size), X)
         = (1.0 / Last_Weight (Nodes, Size)) * Numerator (Nodes, Size, X, Size - 1);
   --  The value of that polynomial, as it is built: the product over the earlier nodes, scaled.

   procedure Lemma_Eval_Last_Basis_Scaled (Nodes : Node_Array; Size : Positive; X : Real) is
   begin
      Lemma_Numerator_Is_Nonzero (Nodes, Size, Size - 1);
      Lemma_Eval_Node_Polynomial (Nodes, Size - 1, Size, X);
      Lemma_Eval_Scaled (Reciprocal (Last_Weight (Nodes, Size)), Node_Polynomial (Nodes, Size - 1), X);
   end Lemma_Eval_Last_Basis_Scaled;

   procedure Lemma_Eval_Last_Basis (Nodes : Node_Array; Size : Positive; X : Real)
   with
     Pre  => Is_Increasing (Nodes) and then Size <= Nodes'Last,
     Post => Eval (Last_Basis_Polynomial (Nodes, Size), X) = Partial_Basis (Nodes, Size, Size, X);
   --  Its value is that of the basis polynomial of node Size over the first Size nodes.

   procedure Lemma_Eval_Last_Basis (Nodes : Node_Array; Size : Positive; X : Real) is
      Weight : constant Real := Last_Weight (Nodes, Size);
      Value  : constant Real := Numerator (Nodes, Size, X, Size - 1);
   begin
      Lemma_Eval_Last_Basis_Scaled (Nodes, Size, X);
      Lemma_Numerator_Step (Nodes, Size, X, Size);
      Lemma_Numerator_Step (Nodes, Size, Nodes (Size), Size);
      pragma Assert (Numerator (Nodes, Size, X, Size) = Value);
      pragma Assert (Numerator (Nodes, Size, Nodes (Size), Size) = Weight);
      pragma Assert (Partial_Basis (Nodes, Size, Size, X) = Value / Weight);
      Lemma_Reciprocal_Scales (Value, Weight);
   end Lemma_Eval_Last_Basis;

   procedure Lemma_Last_Basis_At_Last_Node (Nodes : Node_Array; Size : Positive)
   with
     Pre  => Is_Increasing (Nodes) and then Size <= Nodes'Last,
     Post => Eval (Last_Basis_Polynomial (Nodes, Size), Nodes (Size)) = 1.0;
   --  It is one at its own node.

   procedure Lemma_Last_Basis_At_Last_Node (Nodes : Node_Array; Size : Positive) is
      Weight : constant Real := Last_Weight (Nodes, Size);
   begin
      Lemma_Eval_Last_Basis_Scaled (Nodes, Size, Nodes (Size));
      pragma Assert ((1.0 / Weight) * Weight = 1.0);
   end Lemma_Last_Basis_At_Last_Node;

   procedure Lemma_Last_Basis_At_Earlier_Node (Nodes : Node_Array; Size, I : Positive)
   with
     Pre  => Is_Increasing (Nodes) and then Size <= Nodes'Last and then I < Size,
     Post => Eval (Last_Basis_Polynomial (Nodes, Size), Nodes (I)) = 0.0;
   --  It is zero at every earlier node.

   procedure Lemma_Last_Basis_At_Earlier_Node (Nodes : Node_Array; Size, I : Positive) is
      Weight : constant Real := Last_Weight (Nodes, Size);
   begin
      Lemma_Eval_Last_Basis_Scaled (Nodes, Size, Nodes (I));
      Lemma_Numerator_Vanishes (Nodes, Size, I, Size - 1);
      pragma Assert ((1.0 / Weight) * 0.0 = 0.0);
   end Lemma_Last_Basis_At_Earlier_Node;

   ---------------------------------------------------------------------------
   --  Reduction to one node fewer
   ---------------------------------------------------------------------------

   function Remainder (P : Polynomial; Nodes : Node_Array; Size : Positive) return Polynomial
   is (Difference (P, Scaled (Eval (P, Nodes (Size)), Last_Basis_Polynomial (Nodes, Size))))
   with
     Pre  =>
       Is_Polynomial (P)
       and then Nodes'First = 1
       and then Size <= Nodes'Last
       and then Nodes'Last < Maximum_Length
       and then P'Last < Size,
     Post => Is_Polynomial (Remainder'Result) and then Remainder'Result'Last = Size - 1;
   --  P less its value at node Size times the basis polynomial of that node: what the earlier nodes must
   --  interpolate.

   procedure Lemma_Eval_Remainder (P : Polynomial; Nodes : Node_Array; Size : Positive; X : Real)
   with
     Pre  =>
       Is_Polynomial (P)
       and then Nodes'First = 1
       and then Size <= Nodes'Last
       and then Nodes'Last < Maximum_Length
       and then P'Last < Size,
     Post =>
       Eval (Remainder (P, Nodes, Size), X)
       = Eval (P, X) - Eval (P, Nodes (Size)) * Eval (Last_Basis_Polynomial (Nodes, Size), X);
   --  The value of the remainder, as it is built.

   procedure Lemma_Eval_Remainder (P : Polynomial; Nodes : Node_Array; Size : Positive; X : Real) is
      Value : constant Real := Eval (P, Nodes (Size));
      Last  : constant Polynomial := Last_Basis_Polynomial (Nodes, Size);
   begin
      Lemma_Eval_Scaled (Value, Last, X);
      Lemma_Eval_Difference (P, Scaled (Value, Last), X);
   end Lemma_Eval_Remainder;

   function Reduced (P : Polynomial; Nodes : Node_Array; Size : Positive) return Polynomial
   is (Quotient (Remainder (P, Nodes, Size), Nodes (Size)))
   with
     Pre  =>
       Is_Polynomial (P)
       and then Nodes'First = 1
       and then Size >= 2
       and then Size <= Nodes'Last
       and then Nodes'Last < Maximum_Length
       and then P'Last < Size,
     Post => Is_Polynomial (Reduced'Result) and then Reduced'Result'Last = Size - 2;
   --  The remainder divided by X less node Size, at which it vanishes.

   procedure Lemma_Remainder_Factors (P : Polynomial; Nodes : Node_Array; Size : Positive; X : Real)
   with
     Pre  => Is_Polynomial (P) and then Is_Increasing (Nodes) and then Size in 2 .. Nodes'Last and then P'Last < Size,
     Post => Eval (Remainder (P, Nodes, Size), X) = (X - Nodes (Size)) * Eval (Reduced (P, Nodes, Size), X);
   --  The remainder vanishes at node Size, so the division by X less that node is exact.

   procedure Lemma_Remainder_Factors (P : Polynomial; Nodes : Node_Array; Size : Positive; X : Real) is
      Value : constant Real := Eval (P, Nodes (Size));
   begin
      Lemma_Eval_Remainder (P, Nodes, Size, Nodes (Size));
      Lemma_Last_Basis_At_Last_Node (Nodes, Size);
      pragma Assert (Value * 1.0 = Value);
      pragma Assert (Eval (Remainder (P, Nodes, Size), Nodes (Size)) = 0.0);
      Lemma_Quotient (Remainder (P, Nodes, Size), Nodes (Size), X);
   end Lemma_Remainder_Factors;

   procedure Lemma_Value_At_Earlier_Node (P : Polynomial; Nodes : Node_Array; Size, I : Positive)
   with
     Pre  =>
       Is_Polynomial (P)
       and then Is_Increasing (Nodes)
       and then Size in 2 .. Nodes'Last
       and then P'Last < Size
       and then I < Size,
     Post => Eval (P, Nodes (I)) = (Nodes (I) - Nodes (Size)) * Eval (Reduced (P, Nodes, Size), Nodes (I));
   --  At an earlier node the remainder is P itself, as the basis polynomial of node Size vanishes there.

   procedure Lemma_Value_At_Earlier_Node (P : Polynomial; Nodes : Node_Array; Size, I : Positive) is
      Value : constant Real := Eval (P, Nodes (Size));
   begin
      Lemma_Eval_Remainder (P, Nodes, Size, Nodes (I));
      Lemma_Last_Basis_At_Earlier_Node (Nodes, Size, I);
      pragma Assert (Value * 0.0 = 0.0);
      pragma Assert (Eval (Remainder (P, Nodes, Size), Nodes (I)) = Eval (P, Nodes (I)));
      Lemma_Remainder_Factors (P, Nodes, Size, Nodes (I));
   end Lemma_Value_At_Earlier_Node;

   procedure Lemma_Lift (P : Polynomial; Nodes : Node_Array; Size : Positive; X : Real; Count : Natural)
   with
     Subprogram_Variant => (Decreases => Count),
     Pre                =>
       Is_Polynomial (P)
       and then Is_Increasing (Nodes)
       and then Size in 2 .. Nodes'Last
       and then P'Last < Size
       and then Count < Size,
     Post               =>
       Interpolant (P, Nodes, Size, X, Count)
       = (X - Nodes (Size)) * Interpolant (Reduced (P, Nodes, Size), Nodes, Size - 1, X, Count);
   --  Over the earlier nodes, the interpolant of P is X less the last node times the interpolant of the reduced
   --  polynomial over those nodes alone.

   procedure Lemma_Lift (P : Polynomial; Nodes : Node_Array; Size : Positive; X : Real; Count : Natural) is
      Q : constant Polynomial := Reduced (P, Nodes, Size);
      A : constant Real := X - Nodes (Size);
   begin
      if Count = 0 then
         pragma Assert (Interpolant (P, Nodes, Size, X, Count) = 0.0);
         pragma Assert (Interpolant (Q, Nodes, Size - 1, X, Count) = 0.0);
         pragma Assert (A * 0.0 = 0.0);
      else
         declare
            Value         : constant Real := Eval (P, Nodes (Count));
            Reduced_Value : constant Real := Eval (Q, Nodes (Count));
            Basis_New     : constant Real := Partial_Basis (Nodes, Size, Count, X);
            Basis_Old     : constant Real := Partial_Basis (Nodes, Size - 1, Count, X);
            Before_New    : constant Real := Interpolant (P, Nodes, Size, X, Count - 1);
            Before_Old    : constant Real := Interpolant (Q, Nodes, Size - 1, X, Count - 1);
         begin
            Lemma_Lift (P, Nodes, Size, X, Count - 1);
            pragma Assert (Before_New = A * Before_Old);
            Lemma_Value_At_Earlier_Node (P, Nodes, Size, Count);
            Lemma_Basis_Step (Nodes, Size, Count, X);
            Lemma_Term_Step (Value, Reduced_Value, Basis_New, Basis_Old, A, Nodes (Count) - Nodes (Size));
            pragma Assert (Value * Basis_New = A * (Reduced_Value * Basis_Old));
            pragma Assert (Interpolant (P, Nodes, Size, X, Count) = Before_New + Value * Basis_New);
            pragma Assert (Interpolant (Q, Nodes, Size - 1, X, Count) = Before_Old + Reduced_Value * Basis_Old);
            pragma
              Assert (A * (Before_Old + Reduced_Value * Basis_Old) = A * Before_Old + A * (Reduced_Value * Basis_Old));
         end;
      end if;
   end Lemma_Lift;

   ---------------------------------------------------------------------------
   --  The Lagrange identity
   ---------------------------------------------------------------------------

   procedure Lemma_Lagrange (P : Polynomial; Nodes : Node_Array; Size : Positive; X : Real)
   with
     Subprogram_Variant => (Decreases => Size),
     Pre                =>
       Is_Polynomial (P) and then Is_Increasing (Nodes) and then Size <= Nodes'Last and then P'Last < Size,
     Post               => Eval (P, X) = Interpolant (P, Nodes, Size, X, Size);
   --  A polynomial with at most Size coefficients is its own interpolant at the first Size nodes.

   procedure Lemma_Lagrange (P : Polynomial; Nodes : Node_Array; Size : Positive; X : Real) is
   begin
      if Size = 1 then
         --  A constant, and the only basis polynomial is one.
         pragma Assert (Eval_From (P, 0, X) = P (0));
         pragma Assert (Eval_From (P, 0, Nodes (1)) = P (0));
         Lemma_Numerator_Step (Nodes, 1, X, 1);
         Lemma_Numerator_Step (Nodes, 1, Nodes (1), 1);
         pragma Assert (Numerator (Nodes, 1, X, 0) = 1.0);
         pragma Assert (Numerator (Nodes, 1, Nodes (1), 0) = 1.0);
         pragma Assert (Partial_Basis (Nodes, 1, 1, X) = 1.0);
         pragma Assert (Interpolant (P, Nodes, 1, X, 0) = 0.0);
         pragma Assert (Interpolant (P, Nodes, 1, X, 1) = 0.0 + Eval (P, Nodes (1)) * Partial_Basis (Nodes, 1, 1, X));
      else
         declare
            Q          : constant Polynomial := Reduced (P, Nodes, Size);
            A          : constant Real := X - Nodes (Size);
            Value      : constant Real := Eval (P, Nodes (Size));
            Last_Basis : constant Real := Partial_Basis (Nodes, Size, Size, X);
            Before     : constant Real := Interpolant (P, Nodes, Size, X, Size - 1);
         begin
            Lemma_Lagrange (Q, Nodes, Size - 1, X);
            Lemma_Lift (P, Nodes, Size, X, Size - 1);
            pragma Assert (Before = A * Eval (Q, X));
            Lemma_Remainder_Factors (P, Nodes, Size, X);
            pragma Assert (Eval (Remainder (P, Nodes, Size), X) = Before);
            Lemma_Eval_Remainder (P, Nodes, Size, X);
            Lemma_Eval_Last_Basis (Nodes, Size, X);
            pragma Assert (Before = Eval (P, X) - Value * Last_Basis);
            pragma Assert (Interpolant (P, Nodes, Size, X, Size) = Before + Value * Last_Basis);
         end;
      end if;
   end Lemma_Lagrange;

   ---------------------------------------------------------------------------
   --  The bound
   ---------------------------------------------------------------------------

   procedure Lemma_Partial_Bound (P : Polynomial; Nodes : Node_Array; X, Largest : Real; Count : Natural)
   with
     Subprogram_Variant => (Decreases => Count),
     Pre                =>
       Is_Polynomial (P)
       and then Nodes'First = 1
       and then Nodes'Last >= 1
       and then Count <= Nodes'Last
       and then (for all I in Nodes'Range => abs Eval (P, Nodes (I)) <= Largest),
     Post               =>
       abs Interpolant (P, Nodes, Nodes'Last, X, Count) <= Largest * Lebesgue_Partial (Nodes, X, Count);
   --  The triangle inequality over the terms of the interpolant.

   procedure Lemma_Partial_Bound (P : Polynomial; Nodes : Node_Array; X, Largest : Real; Count : Natural) is
   begin
      if Count = 0 then
         pragma Assert (Interpolant (P, Nodes, Nodes'Last, X, Count) = 0.0);
         pragma Assert (Lebesgue_Partial (Nodes, X, Count) = 0.0);
         pragma Assert (Largest * 0.0 = 0.0);
      else
         declare
            Value         : constant Real := Eval (P, Nodes (Count));
            Weight        : constant Real := Basis (Nodes, Count, X);
            Before        : constant Real := Interpolant (P, Nodes, Nodes'Last, X, Count - 1);
            Before_Weight : constant Real := Lebesgue_Partial (Nodes, X, Count - 1);
         begin
            Lemma_Partial_Bound (P, Nodes, X, Largest, Count - 1);
            pragma Assert (abs Before <= Largest * Before_Weight);
            pragma Assert (abs Value <= Largest);
            Lemma_Scaled_Magnitude (Value, Weight, Largest);
            pragma Assert (Partial_Basis (Nodes, Nodes'Last, Count, X) = Weight);
            pragma Assert (Interpolant (P, Nodes, Nodes'Last, X, Count) = Before + Value * Weight);
            pragma Assert (Lebesgue_Partial (Nodes, X, Count) = Before_Weight + abs Weight);
            pragma Assert (abs (Before + Value * Weight) <= abs Before + abs (Value * Weight));
            pragma Assert (Largest * (Before_Weight + abs Weight) = Largest * Before_Weight + Largest * abs Weight);
         end;
      end if;
   end Lemma_Partial_Bound;

   procedure Lemma_Interpolation_Bound (P : Polynomial; Nodes : Node_Array; X, Largest, Lebesgue : Real) is
      Sum : constant Real := Lebesgue_Sum (Nodes, X);
   begin
      Lemma_Lagrange (P, Nodes, Nodes'Last, X);
      Lemma_Partial_Bound (P, Nodes, X, Largest, Nodes'Last);
      pragma Assert (abs Eval (P, X) <= Largest * Sum);
      pragma Assert (abs Eval (P, Nodes (1)) <= Largest);
      pragma Assert (0.0 <= Largest);
      Lemma_Scaling_Keeps_Order (Largest, Sum, Lebesgue);
      pragma Assert (Largest * Lebesgue = Lebesgue * Largest);
   end Lemma_Interpolation_Bound;

end Prunt.Motion_Planner.Stereographic_Kernel.Polynomials.Interpolation;
