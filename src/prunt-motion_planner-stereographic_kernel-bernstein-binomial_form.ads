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

--  The Bernstein sum behind de Casteljau's recurrence, and the identities which rest on its binomial weights:
--  products, multiplication by a power of T or of 1 - T, and division by an endpoint root. For specification and
--  proof only. Every polynomial here has controls indexed from zero, so that its degree is its last index.

package Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form
  with SPARK_Mode => On, Ghost, Always_Terminates
is

   Maximum_Degree : constant := 1_000;
   --  Far above any degree in use; it only keeps index arithmetic away from Natural'Last.

   function Is_Polynomial (P : Controls) return Boolean
   is (P'First = 0 and then P'Last in 0 .. Maximum_Degree);
   --  There is at least one control: the empty array indexed 0 .. -1 is not a polynomial.

   ---------------------------------------------------------------------------
   --  The Bernstein sum
   ---------------------------------------------------------------------------

   function Choose (N, K : Natural) return Real
   is (if K > N then 0.0 elsif K = 0 or else K = N then 1.0 else Choose (N - 1, K - 1) + Choose (N - 1, K))
   with Subprogram_Variant => (Decreases => N), Post => (if K <= N then Choose'Result >= 1.0 else Choose'Result = 0.0);
   --  The binomial coefficient "N choose K", by Pascal's rule.

   function Power (Base : Real; Exponent : Natural) return Real
   is (if Exponent = 0 then 1.0 else Base * Power (Base, Exponent - 1))
   with Subprogram_Variant => (Decreases => Exponent);

   function Basis (N, I : Natural; T : Real) return Real
   is (Choose (N, I) * Power (T, I) * Power (1.0 - T, N - I))
   with Pre => I <= N;
   --  The Bernstein basis polynomial C(N,I)·Tᴵ·(1 - T)ᴺ⁻ᴵ.

   function Partial_Sum (P : Controls; Count : Natural; T : Real) return Real
   is (if Count = 0 then 0.0 else Partial_Sum (P, Count - 1, T) + P (Count - 1) * Basis (P'Last, Count - 1, T))
   with Pre => Is_Polynomial (P) and then Count <= P'Length, Subprogram_Variant => (Decreases => Count);
   --  The first Count terms of the Bernstein sum of P.

   procedure Lemma_Value_Is_Bernstein_Sum (P : Controls; T : Real)
   with Pre => Is_Polynomial (P), Post => Value (P, T) = Partial_Sum (P, P'Length, T);
   --  De Casteljau's recurrence evaluates the Bernstein sum.

   procedure Lemma_Power_Is_Nonnegative (Base : Real; Exponent : Natural)
   with Pre => Base >= 0.0, Post => Power (Base, Exponent) >= 0.0;

   procedure Lemma_Power_Of_Sum (Base : Real; Left, Right : Natural)
   with
     Pre                => Left <= Natural'Last - Right,
     Post               => Power (Base, Left + Right) = Power (Base, Left) * Power (Base, Right),
     Subprogram_Variant => (Decreases => Left);
   --  Exponents add when powers of one base are multiplied.

   procedure Lemma_Basis_Pascal (N, I : Natural; T : Real)
   with
     Pre  => N >= 1 and then I <= N,
     Post =>
       Basis (N, I, T)
       = (if I < N then (1.0 - T) * Basis (N - 1, I, T) else 0.0)
         + (if I > 0 then T * Basis (N - 1, I - 1, T) else 0.0);
   --  Pascal's rule C(N,I) = C(N-1,I) + C(N-1,I-1) carried over to the basis polynomials, where a term whose lower
   --  index would leave 0 .. N - 1 is absent.

   procedure Lemma_Value_Of_Constant (P : Controls; Level, T : Real)
   with
     Pre                =>
       P'Length >= 1 and then P'Last < Natural'Last and then (for all I in P'Range => P (I) = Level),
     Post               => Value (P, T) = Level,
     Subprogram_Variant => (Decreases => P'Length);
   --  Equal controls give a constant polynomial. Unlike Lemma_Convex_Hull with equal bounds, this holds at any T.

   ---------------------------------------------------------------------------
   --  Multiplication by a power of T or of 1 - T
   ---------------------------------------------------------------------------

   function Raised_At_Start (P : Controls; By : Natural) return Controls
   with
     Pre  => Is_Polynomial (P) and then By <= Maximum_Degree,
     Post =>
       Raised_At_Start'Result'First = 0
       and then Raised_At_Start'Result'Last = P'Last + By
       and then
         (for all J in Raised_At_Start'Result'Range =>
            Raised_At_Start'Result (J)
            = (if J < By then 0.0 else P (J - By) * Choose (P'Last, J - By) / Choose (P'Last + By, J)));
   --  The controls, By degrees higher, of T**By times the polynomial P.

   procedure Lemma_Raised_At_Start (P : Controls; By : Natural; T : Real)
   with
     Pre  => Is_Polynomial (P) and then By <= Maximum_Degree,
     Post => Value (Raised_At_Start (P, By), T) = Power (T, By) * Value (P, T);

   function Raised_At_Finish (P : Controls; By : Natural) return Controls
   with
     Pre  => Is_Polynomial (P) and then By <= Maximum_Degree,
     Post =>
       Raised_At_Finish'Result'First = 0
       and then Raised_At_Finish'Result'Last = P'Last + By
       and then
         (for all J in Raised_At_Finish'Result'Range =>
            Raised_At_Finish'Result (J)
            = (if J > P'Last then 0.0 else P (J) * Choose (P'Last, J) / Choose (P'Last + By, J)));
   --  The controls, By degrees higher, of (1 - T)**By times the polynomial P.

   procedure Lemma_Raised_At_Finish (P : Controls; By : Natural; T : Real)
   with
     Pre  => Is_Polynomial (P) and then By <= Maximum_Degree,
     Post => Value (Raised_At_Finish (P, By), T) = Power (1.0 - T, By) * Value (P, T);

   ---------------------------------------------------------------------------
   --  Division by an endpoint root
   ---------------------------------------------------------------------------

   function Start_Quotient (P : Controls; Order : Positive) return Controls
   with
     Pre  => Is_Polynomial (P) and then Order <= P'Last and then (for all I in 1 .. Order - 1 => P (I) = P (0)),
     Post =>
       Start_Quotient'Result'First = 0
       and then Start_Quotient'Result'Last = P'Last - Order
       and then
         (for all J in Start_Quotient'Result'Range =>
            Start_Quotient'Result (J)
            = (P (J + Order) - P (0)) * Choose (P'Last, J + Order) / Choose (P'Last - Order, J));
   --  The controls of (P - P(0)) / T**Order. The leading controls being equal is what gives P - P(0) a root of
   --  that order at zero.

   procedure Lemma_Start_Quotient (P : Controls; Order : Positive; T : Real)
   with
     Pre  => Is_Polynomial (P) and then Order <= P'Last and then (for all I in 1 .. Order - 1 => P (I) = P (0)),
     Post => Value (P, T) - P (0) = Power (T, Order) * Value (Start_Quotient (P, Order), T);

   function Finish_Quotient (P : Controls; Order : Positive) return Controls
   with
     Pre  =>
       Is_Polynomial (P)
       and then Order <= P'Last
       and then (for all I in P'Last - Order + 1 .. P'Last - 1 => P (I) = P (P'Last)),
     Post =>
       Finish_Quotient'Result'First = 0
       and then Finish_Quotient'Result'Last = P'Last - Order
       and then
         (for all J in Finish_Quotient'Result'Range =>
            Finish_Quotient'Result (J) = (P (P'Last) - P (J)) * Choose (P'Last, J) / Choose (P'Last - Order, J));
   --  The controls of (P(1) - P) / (1 - T)**Order.

   procedure Lemma_Finish_Quotient (P : Controls; Order : Positive; T : Real)
   with
     Pre  =>
       Is_Polynomial (P)
       and then Order <= P'Last
       and then (for all I in P'Last - Order + 1 .. P'Last - 1 => P (I) = P (P'Last)),
     Post => P (P'Last) - Value (P, T) = Power (1.0 - T, Order) * Value (Finish_Quotient (P, Order), T);

   ---------------------------------------------------------------------------
   --  Products
   ---------------------------------------------------------------------------

   function Scaled_Convolution (A, B : Controls; K : Natural; Count : Natural) return Real
   is (if Count = 0
       then 0.0
       else
         Scaled_Convolution (A, B, K, Count - 1)
         + (if K >= Count - 1 and then K - (Count - 1) <= B'Last
            then
              (Choose (A'Last, Count - 1) * A (Count - 1)) * (Choose (B'Last, K - (Count - 1)) * B (K - (Count - 1)))
            else 0.0))
   with
     Pre                => Is_Polynomial (A) and then Is_Polynomial (B) and then Count <= A'Length,
     Subprogram_Variant => (Decreases => Count);
   --  The sum, over I below Count with K - I a control of B, of (C(m,I)·A(I))·(C(n,K-I)·B(K-I)): the binomially
   --  scaled controls of A and B convolved, where m and n are their degrees.

   function Product (A, B : Controls) return Controls
   with
     Pre  => Is_Polynomial (A) and then Is_Polynomial (B),
     Post =>
       Product'Result'First = 0
       and then Product'Result'Last = A'Last + B'Last
       and then
         (for all K in Product'Result'Range =>
            Product'Result (K) = Scaled_Convolution (A, B, K, A'Length) / Choose (A'Last + B'Last, K));
   --  The controls of the product of the polynomials A and B.

   procedure Lemma_Product (A, B : Controls; T : Real)
   with
     Pre  => Is_Polynomial (A) and then Is_Polynomial (B),
     Post => Value (Product (A, B), T) = Value (A, T) * Value (B, T);

end Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;
