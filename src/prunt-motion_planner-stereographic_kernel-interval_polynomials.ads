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

--  Polynomials of degree at most sixteen in Bernstein form whose controls are outward-rounded intervals: the
--  arithmetic behind the certificates of the ideal tangent's geometry.
--
--  These are the operations of Stereographic_Curves restricted to the degrees which those certificates use. Every
--  binomial coefficient of such a degree is an exact floating-point number, so nothing here needs the 128-bit
--  integers of the general routines. An operation performs the same floating-point steps in the same order as the
--  general one, so that the two agree bit for bit; the one difference is that where the general product falls back
--  to a slower formula because a scaled control exceeds 2**500, this one returns invalid controls instead.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Prunt.Motion_Planner.Stereographic_Kernel.Bernstein;
with Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;
with Prunt.Motion_Planner.Stereographic_Kernel.Intervals; use Prunt.Motion_Planner.Stereographic_Kernel.Intervals;
with Prunt.Motion_Planner.Reals;     use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   Maximum_Degree : constant := 16;

   subtype Degree_Range is Natural range 0 .. Maximum_Degree;

   type Interval_Controls is array (Degree_Range) of Interval;

   type Polynomial is record
      Degree : Degree_Range := 0;
      --  The controls from zero to Degree are those of the polynomial. The rest are unused and exactly zero.

      Controls : Interval_Controls := [others => (Lower => 0.0, Upper => 0.0, Valid => True)];
   end record;
   --  A polynomial in Bernstein form with interval controls.

   function All_Valid (Value : Polynomial) return Boolean
   is (for all K in 0 .. Value.Degree => Value.Controls (K).Valid);

   function Encloses (Value : Polynomial; Exact : Bernstein.Controls) return Boolean
   is (Exact'First = 0
       and then Exact'Last = Value.Degree
       and then (for all K in 0 .. Value.Degree => Contains (Value.Controls (K), Exact (K))))
   with Ghost;
   --  Whether Exact is the control vector of a real polynomial each of whose controls lies in the matching
   --  interval of Value. Every control of Value is then valid.

   function Invalid_Polynomial (Degree : Degree_Range) return Polynomial
   with
     Post =>
       Invalid_Polynomial'Result.Degree = Degree
       and then (for all K in 0 .. Degree => not Invalid_Polynomial'Result.Controls (K).Valid);
   --  What an operation returns when it cannot enclose its result.

   ---------------------------------------------------------------------------
   --  Binomial coefficients
   ---------------------------------------------------------------------------

   type Binomial_Table is array (Degree_Range, Degree_Range) of Dimensionless;

   Binomials : constant Binomial_Table :=
     [[1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 3.0, 3.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 4.0, 6.0, 4.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 5.0, 10.0, 10.0, 5.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 6.0, 15.0, 20.0, 15.0, 6.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 7.0, 21.0, 35.0, 35.0, 21.0, 7.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 8.0, 28.0, 56.0, 70.0, 56.0, 28.0, 8.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 9.0, 36.0, 84.0, 126.0, 126.0, 84.0, 36.0, 9.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 10.0, 45.0, 120.0, 210.0, 252.0, 210.0, 120.0, 45.0, 10.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 11.0, 55.0, 165.0, 330.0, 462.0, 462.0, 330.0, 165.0, 55.0, 11.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 12.0, 66.0, 220.0, 495.0, 792.0, 924.0, 792.0, 495.0, 220.0, 66.0, 12.0, 1.0, 0.0, 0.0, 0.0, 0.0],
      [1.0, 13.0, 78.0, 286.0, 715.0, 1287.0, 1716.0, 1716.0, 1287.0, 715.0, 286.0, 78.0, 13.0, 1.0, 0.0, 0.0, 0.0],
      [1.0,
       14.0,
       91.0,
       364.0,
       1001.0,
       2002.0,
       3003.0,
       3432.0,
       3003.0,
       2002.0,
       1001.0,
       364.0,
       91.0,
       14.0,
       1.0,
       0.0,
       0.0],
      [1.0,
       15.0,
       105.0,
       455.0,
       1365.0,
       3003.0,
       5005.0,
       6435.0,
       6435.0,
       5005.0,
       3003.0,
       1365.0,
       455.0,
       105.0,
       15.0,
       1.0,
       0.0],
      [1.0,
       16.0,
       120.0,
       560.0,
       1820.0,
       4368.0,
       8008.0,
       11440.0,
       12870.0,
       11440.0,
       8008.0,
       4368.0,
       1820.0,
       560.0,
       120.0,
       16.0,
       1.0]];
   --  Binomials (N, K) is "N choose K", and zero above the diagonal. Every entry is an exact integer.

   Largest_Binomial : constant Dimensionless := 12_870.0;

   procedure Lemma_Binomials_Are_Exact (N, K : Degree_Range)
   with
     Ghost,
     Post =>
       R (Binomials (N, K)) = Bernstein.Binomial_Form.Choose (N, K)
       and then (if K <= N then Binomials (N, K) >= 1.0 and then Binomials (N, K) <= Largest_Binomial);
   --  The table holds the binomial coefficients themselves.

   function Binomial (N, K : Natural) return Dimensionless
   is (if N <= Maximum_Degree and then K <= N then Binomials (N, K) else 0.0);
   --  The binomial coefficient, or zero outside the table, which simplifies bounded polynomial summations.

   type Positive_Endpoints is record
      Lower, Upper : Dimensionless;
   end record;
   --  A strictly positive enclosure, which needs no validity flag.

   type Reciprocal_Table is array (Degree_Range, Degree_Range) of Positive_Endpoints;

   function Build_Reciprocals return Reciprocal_Table;
   --  Enclose the reciprocal of every binomial coefficient, once, during elaboration.

   ---------------------------------------------------------------------------
   --  Arithmetic
   ---------------------------------------------------------------------------

   function Positive_Ratio (Numerator, Denominator : Dimensionless) return Interval
   with
     Post =>
       Is_Ordered (Positive_Ratio'Result)
       and then
         (if Positive_Ratio'Result.Valid
          then
            Numerator >= 0.0
            and then Denominator >= 1.0
            and then Contains (Positive_Ratio'Result, R (Numerator) / R (Denominator)));
   --  Enclose a ratio whose numerator is nonnegative and whose denominator is at least one. Any other operands
   --  give the invalid interval.

   function Multiply (Left, Right : Polynomial) return Polynomial
   with
     Pre  => Left.Degree >= 1 and then Right.Degree >= 1 and then Left.Degree + Right.Degree <= Maximum_Degree,
     Post =>
       Multiply'Result.Degree = Left.Degree + Right.Degree
       and then (if All_Valid (Multiply'Result) then All_Valid (Left) and then All_Valid (Right));
   --  Enclose the product of two polynomials. Each control of a product in Bernstein form is
   --
   --     C(m+n,k)⁻¹·Σ (C(m,i)·Leftᵢ)·(C(n,k-i)·Rightₖ₋ᵢ),
   --
   --  with every scale, product, sum and final quotient enclosed independently. The result has only invalid
   --  controls if any control of an operand is invalid, or if a scaled control exceeds Scaled_Control_Limit.

   Scaled_Control_Limit : constant Dimensionless := 2.0**500;

   procedure Lemma_Multiply_Encloses (Left, Right : Polynomial; A, B : Bernstein.Controls)
   with
     Ghost,
     Pre  =>
       Left.Degree >= 1
       and then Right.Degree >= 1
       and then Left.Degree + Right.Degree <= Maximum_Degree
       and then Encloses (Left, A)
       and then Encloses (Right, B)
       and then All_Valid (Multiply (Left, Right)),
     Post => Encloses (Multiply (Left, Right), Bernstein.Binomial_Form.Product (A, B));
   --  A valid product encloses the product of any two real polynomials which its operands enclose.

end Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials;
