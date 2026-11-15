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

--  Real polynomials by their coefficients, lowest power first: the algebra which shows that a function built from
--  sums and products of polynomials is a polynomial of a known degree. Everything here is ghost code.

with Prunt.Motion_Planner.Stereographic_Kernel.Bernstein;
with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Polynomials
  with SPARK_Mode => On, Ghost, Always_Terminates
is
   use type Prunt.Motion_Planner.Reals.Real;

   type Polynomial is array (Natural range <>) of Real;
   --  The coefficients of one polynomial: P (K) multiplies the K-th power.

   Maximum_Length : constant := 1024;

   function Is_Polynomial (P : Polynomial) return Boolean
   is (P'First = 0 and then P'Last >= 0 and then P'Last < Maximum_Length);
   --  There is at least one coefficient. P'Last bounds the degree: leading coefficients may be zero.

   function Eval_From (P : Polynomial; From : Natural; X : Real) return Real
   is (if From = P'Last then P (From) else P (From) + X * Eval_From (P, From + 1, X))
   with Pre => From in P'Range, Subprogram_Variant => (Decreases => P'Last - From);
   --  Horner's rule over the coefficients from From upward.

   function Eval (P : Polynomial; X : Real) return Real
   is (Eval_From (P, 0, X))
   with Pre => Is_Polynomial (P);
   --  The value of P at X.

   ---------------------------------------------------------------------------
   --  Construction
   ---------------------------------------------------------------------------

   function Constant_Polynomial (Value : Real) return Polynomial
   with
     Post =>
       Is_Polynomial (Constant_Polynomial'Result)
       and then Constant_Polynomial'Result'Last = 0
       and then Constant_Polynomial'Result (0) = Value;

   function Linear (Constant_Term, Slope : Real) return Polynomial
   with
     Post =>
       Is_Polynomial (Linear'Result)
       and then Linear'Result'Last = 1
       and then Linear'Result (0) = Constant_Term
       and then Linear'Result (1) = Slope;

   function Sum (P, Q : Polynomial) return Polynomial
   with
     Pre  => Is_Polynomial (P) and then Is_Polynomial (Q),
     Post => Is_Polynomial (Sum'Result) and then Sum'Result'Last = Natural'Max (P'Last, Q'Last);

   function Difference (P, Q : Polynomial) return Polynomial
   with
     Pre  => Is_Polynomial (P) and then Is_Polynomial (Q),
     Post => Is_Polynomial (Difference'Result) and then Difference'Result'Last = Natural'Max (P'Last, Q'Last);

   function Scaled (Factor : Real; P : Polynomial) return Polynomial
   with Pre => Is_Polynomial (P), Post => Is_Polynomial (Scaled'Result) and then Scaled'Result'Last = P'Last;

   function Product (P, Q : Polynomial) return Polynomial
   with
     Pre  => Is_Polynomial (P) and then Is_Polynomial (Q) and then P'Last + Q'Last < Maximum_Length,
     Post => Is_Polynomial (Product'Result) and then Product'Result'Last = P'Last + Q'Last;

   function From_Bernstein (Controls : Bernstein.Controls) return Polynomial
   with
     Pre  => Controls'First = 0 and then Controls'Last >= 0 and then Controls'Last < Maximum_Length,
     Post => Is_Polynomial (From_Bernstein'Result) and then From_Bernstein'Result'Last = Controls'Last;
   --  The same polynomial as the one with these Bernstein controls.

   ---------------------------------------------------------------------------
   --  Values
   ---------------------------------------------------------------------------

   procedure Lemma_Eval_Constant (Value, X : Real)
   with Post => Eval (Constant_Polynomial (Value), X) = Value;

   procedure Lemma_Eval_Linear (Constant_Term, Slope, X : Real)
   with Post => Eval (Linear (Constant_Term, Slope), X) = Constant_Term + Slope * X;

   procedure Lemma_Eval_Sum (P, Q : Polynomial; X : Real)
   with Pre => Is_Polynomial (P) and then Is_Polynomial (Q), Post => Eval (Sum (P, Q), X) = Eval (P, X) + Eval (Q, X);

   procedure Lemma_Eval_Difference (P, Q : Polynomial; X : Real)
   with
     Pre  => Is_Polynomial (P) and then Is_Polynomial (Q),
     Post => Eval (Difference (P, Q), X) = Eval (P, X) - Eval (Q, X);

   procedure Lemma_Eval_Scaled (Factor : Real; P : Polynomial; X : Real)
   with Pre => Is_Polynomial (P), Post => Eval (Scaled (Factor, P), X) = Factor * Eval (P, X);

   procedure Lemma_Eval_Product (P, Q : Polynomial; X : Real)
   with
     Pre  => Is_Polynomial (P) and then Is_Polynomial (Q) and then P'Last + Q'Last < Maximum_Length,
     Post => Eval (Product (P, Q), X) = Eval (P, X) * Eval (Q, X);

   procedure Lemma_Eval_From_Bernstein (Controls : Bernstein.Controls; T : Real)
   with
     Pre  => Controls'First = 0 and then Controls'Last >= 0 and then Controls'Last < Maximum_Length,
     Post => Eval (From_Bernstein (Controls), T) = Bernstein.Value (Controls, T);

   ---------------------------------------------------------------------------
   --  Division by a linear factor
   ---------------------------------------------------------------------------

   function Quotient (P : Polynomial; Root : Real) return Polynomial
   with
     Pre  => Is_Polynomial (P) and then P'Last >= 1,
     Post => Is_Polynomial (Quotient'Result) and then Quotient'Result'Last = P'Last - 1;
   --  The quotient of P by X - Root, by synthetic division.

   procedure Lemma_Quotient (P : Polynomial; Root, X : Real)
   with
     Pre  => Is_Polynomial (P) and then P'Last >= 1,
     Post => Eval (P, X) = Eval (P, Root) + (X - Root) * Eval (Quotient (P, Root), X);
   --  The remainder of that division is the value of P at Root.

end Prunt.Motion_Planner.Stereographic_Kernel.Polynomials;
