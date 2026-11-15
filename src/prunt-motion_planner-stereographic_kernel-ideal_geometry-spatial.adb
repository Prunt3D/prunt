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
pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;
with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Polynomials;
use Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Polynomials;
with Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials;
use Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials;
with Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials.Nonnegativity;
with Prunt.Motion_Planner.Stereographic_Kernel.Intervals;
use Prunt.Motion_Planner.Stereographic_Kernel.Intervals;

use Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials.Nonnegativity;

package body Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Spatial
  with SPARK_Mode => On
is

   package Binomial_Form renames Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;

   --  How Lemma_Certified is proved, at the start; the finish mirrors it in 1 - V.
   --
   --  Let k be the order of the root of Y₁ - Yₛ at the start, m the lower of the orders of the roots of Y₂ and Y₃
   --  there, and s the lesser of k and 2·m. With Q₁ = (Y₁ - Yₛ)/Vᵏ, Q₂ = Y₂/Vᵐ and Q₃ = Y₃/Vᵐ, the certificate
   --  tests the polynomial
   --
   --     E = Sign·(Vᵏ⁻ˢ·Q₁·(1 + Yₛ·Y₁) + V²ᵐ⁻ˢ·Yₛ·(Q₂² + Q₃²)),
   --
   --  for which Vˢ·E is Sign times the start determinant (Y₁ - Yₛ)·(1 + Yₛ·Y₁) + Yₛ·(Y₂² + Y₃²).
   --
   --  1. The interval polynomial which End_Polynomial returns, when all of its controls are valid, encloses the
   --     controls of E as a real polynomial (Exact_End). Validity walks backwards through the postcondition of each
   --     operation to the quotients and the linear factor, and their enclosures then walk forwards through the
   --     enclosure lemma of each operation (Lemma_Assembled_Encloses, Lemma_End_Encloses).
   --  2. The value of that real polynomial at V is the expression above, by the value lemma of each operation
   --     (Lemma_Exact_Assembled_Value, Lemma_Exact_End_Value), and it is nonnegative because the test passed
   --     (Lemma_Exact_End_Is_Nonnegative).
   --  3. The quotients times the powers which they divided out are the chart coordinates' distances from their
   --     start values (Lemma_Start_Roots), so that Vˢ·E is the oriented determinant (Lemma_Shared_Root), which is
   --     therefore nonnegative as well.
   --  4. The sign which orients both determinants is the proven sign of the turn, which puts the tangent in the
   --     cone.
   --
   --  Negations of real numbers are written as differences from zero throughout: this version of the prover fails
   --  to translate the unary minus of a real number in some contexts.

   ---------------------------------------------------------------------------
   --  Real algebra
   ---------------------------------------------------------------------------

   procedure Lemma_Common_Factor (Shared, Rest, Whole, Quotient_Value, Difference, Other : Real)
   with
     Ghost,
     Pre  => Whole = Shared * Rest and then Difference = Whole * Quotient_Value,
     Post => Shared * (Rest * (Quotient_Value * Other)) = Difference * Other;
   --  The in-plane term: the shared power restores the whole root of Y₁'s distance from its endpoint value.

   procedure Lemma_Common_Factor (Shared, Rest, Whole, Quotient_Value, Difference, Other : Real) is
      Tail : constant Real := Quotient_Value * Other;
   begin
      pragma Assert (Shared * (Rest * Tail) = (Shared * Rest) * Tail);
      pragma Assert (Whole * Tail = (Whole * Quotient_Value) * Other);
      pragma Assert (Whole * Tail = Difference * Other);
   end Lemma_Common_Factor;

   procedure Lemma_Square_Of_Product (Single, Quotient_Value, Whole : Real)
   with
     Ghost,
     Pre  => Whole = Single * Quotient_Value,
     Post => Whole * Whole = (Single * Single) * (Quotient_Value * Quotient_Value);

   --  (a·b)² = a²·b².

   procedure Lemma_Square_Of_Product (Single, Quotient_Value, Whole : Real) is null;

   procedure Lemma_Common_Square_Factor (Shared, Rest, Single, Scale, Quotient_2, Quotient_3, Whole_2, Whole_3 : Real)
   with
     Ghost,
     Pre  =>
       Single * Single = Shared * Rest and then Whole_2 = Single * Quotient_2 and then Whole_3 = Single * Quotient_3,
     Post =>
       Shared * (Rest * (Scale * (Quotient_2 * Quotient_2 + Quotient_3 * Quotient_3)))
       = Scale * (Whole_2 * Whole_2 + Whole_3 * Whole_3);
   --  The normal term: each normal coordinate carries its root once, so the squares carry it twice, which the
   --  shared power restores.

   procedure Lemma_Common_Square_Factor (Shared, Rest, Single, Scale, Quotient_2, Quotient_3, Whole_2, Whole_3 : Real)
   is
      Double  : constant Real := Single * Single;
      Squares : constant Real := Quotient_2 * Quotient_2 + Quotient_3 * Quotient_3;
   begin
      Lemma_Square_Of_Product (Single, Quotient_2, Whole_2);
      Lemma_Square_Of_Product (Single, Quotient_3, Whole_3);
      pragma Assert (Whole_2 * Whole_2 + Whole_3 * Whole_3 = Double * Squares);
      pragma Assert (Shared * (Rest * (Scale * Squares)) = (Shared * Rest) * (Scale * Squares));
      pragma Assert (Double * (Scale * Squares) = Scale * (Double * Squares));
   end Lemma_Common_Square_Factor;

   procedure Lemma_Shared_Root
     (Sign,
      Shared,
      Tangent_Rest,
      Normal_Rest,
      Tangent,
      Normal,
      Quotient_1,
      Factor,
      Quotient_2,
      Quotient_3,
      Scale,
      Difference,
      Whole_2,
      Whole_3,
      Certified,
      Determinant : Real)
   with
     Ghost,
     Pre  =>
       Certified
       = Sign
         * (Tangent_Rest * (Quotient_1 * Factor)
            + Normal_Rest * (Scale * (Quotient_2 * Quotient_2 + Quotient_3 * Quotient_3)))
       and then Tangent = Shared * Tangent_Rest
       and then Normal * Normal = Shared * Normal_Rest
       and then Difference = Tangent * Quotient_1
       and then Whole_2 = Normal * Quotient_2
       and then Whole_3 = Normal * Quotient_3
       and then Determinant = Difference * Factor + Scale * (Whole_2 * Whole_2 + Whole_3 * Whole_3)
       and then Shared >= 0.0
       and then Certified >= 0.0,
     Post => Sign * Determinant >= 0.0;
   --  The certified polynomial times the shared power of V, or of 1 - V, is the oriented cone determinant, which
   --  is therefore nonnegative wherever the certified polynomial is. Tangent and Normal are the powers which the
   --  quotients of Y₁ and of Y₂, Y₃ divided out, Shared is the power common to every term of the determinant, and
   --  Tangent_Rest and Normal_Rest are the powers which the certificate multiplied back in.

   procedure Lemma_Shared_Root
     (Sign,
      Shared,
      Tangent_Rest,
      Normal_Rest,
      Tangent,
      Normal,
      Quotient_1,
      Factor,
      Quotient_2,
      Quotient_3,
      Scale,
      Difference,
      Whole_2,
      Whole_3,
      Certified,
      Determinant : Real)
   is
      In_Plane    : constant Real := Tangent_Rest * (Quotient_1 * Factor);
      Normal_Term : constant Real := Normal_Rest * (Scale * (Quotient_2 * Quotient_2 + Quotient_3 * Quotient_3));
   begin
      Lemma_Common_Factor (Shared, Tangent_Rest, Tangent, Quotient_1, Difference, Factor);
      Lemma_Common_Square_Factor (Shared, Normal_Rest, Normal, Scale, Quotient_2, Quotient_3, Whole_2, Whole_3);
      pragma Assert (Shared * In_Plane = Difference * Factor);
      pragma Assert (Shared * Normal_Term = Scale * (Whole_2 * Whole_2 + Whole_3 * Whole_3));
      pragma Assert (Shared * (In_Plane + Normal_Term) = Determinant);
      pragma Assert (Certified = Sign * (In_Plane + Normal_Term));
      pragma Assert (Shared * Certified = Sign * (Shared * (In_Plane + Normal_Term)));
      pragma Assert (Shared * Certified >= 0.0);
   end Lemma_Shared_Root;

   procedure Lemma_Start_Algebra
     (Sign,
      Shared,
      Tangent_Rest,
      Normal_Rest,
      Tangent,
      Normal,
      Quotient_1,
      Factor,
      Quotient_2,
      Quotient_3,
      Scale,
      Start,
      Y_1,
      Y_2,
      Y_3,
      Certified,
      Determinant : Real)
   with
     Ghost,
     Pre  =>
       Certified
       = Sign
         * (Tangent_Rest * (Quotient_1 * Factor)
            + Normal_Rest * (Scale * (Quotient_2 * Quotient_2 + Quotient_3 * Quotient_3)))
       and then Tangent = Shared * Tangent_Rest
       and then Normal * Normal = Shared * Normal_Rest
       and then Y_1 - Start = Tangent * Quotient_1
       and then Y_2 = Normal * Quotient_2
       and then Y_3 = Normal * Quotient_3
       and then Factor = 1.0 + Start * Y_1
       and then Scale = Start
       and then Determinant = (Y_1 - Start) * (1.0 + Start * Y_1) + Start * (Y_2 * Y_2 + Y_3 * Y_3)
       and then Shared >= 0.0
       and then Certified >= 0.0,
     Post => Sign * Determinant >= 0.0;
   --  Lemma_Shared_Root for the start determinant (Y₁ - Yₛ)·(1 + Yₛ·Y₁) + Yₛ·(Y₂² + Y₃²).

   procedure Lemma_Start_Algebra
     (Sign,
      Shared,
      Tangent_Rest,
      Normal_Rest,
      Tangent,
      Normal,
      Quotient_1,
      Factor,
      Quotient_2,
      Quotient_3,
      Scale,
      Start,
      Y_1,
      Y_2,
      Y_3,
      Certified,
      Determinant : Real) is
   begin
      pragma Assert (Determinant = (Y_1 - Start) * Factor + Scale * (Y_2 * Y_2 + Y_3 * Y_3));
      Lemma_Shared_Root
        (Sign         => Sign,
         Shared       => Shared,
         Tangent_Rest => Tangent_Rest,
         Normal_Rest  => Normal_Rest,
         Tangent      => Tangent,
         Normal       => Normal,
         Quotient_1   => Quotient_1,
         Factor       => Factor,
         Quotient_2   => Quotient_2,
         Quotient_3   => Quotient_3,
         Scale        => Scale,
         Difference   => Y_1 - Start,
         Whole_2      => Y_2,
         Whole_3      => Y_3,
         Certified    => Certified,
         Determinant  => Determinant);
   end Lemma_Start_Algebra;

   procedure Lemma_Finish_Algebra
     (Sign,
      Shared,
      Tangent_Rest,
      Normal_Rest,
      Tangent,
      Normal,
      Quotient_1,
      Factor,
      Quotient_2,
      Quotient_3,
      Scale,
      Finish,
      Y_1,
      Y_2,
      Y_3,
      Certified,
      Determinant : Real)
   with
     Ghost,
     Pre  =>
       Certified
       = Sign
         * (Tangent_Rest * (Quotient_1 * Factor)
            + Normal_Rest * (Scale * (Quotient_2 * Quotient_2 + Quotient_3 * Quotient_3)))
       and then Tangent = Shared * Tangent_Rest
       and then Normal * Normal = Shared * Normal_Rest
       and then Finish - Y_1 = Tangent * Quotient_1
       and then 0.0 - Y_2 = Normal * Quotient_2
       and then 0.0 - Y_3 = Normal * Quotient_3
       and then Factor = 1.0 + Finish * Y_1
       and then Scale = 0.0 - Finish
       and then Determinant = (Finish - Y_1) * (1.0 + Finish * Y_1) - Finish * (Y_2 * Y_2 + Y_3 * Y_3)
       and then Shared >= 0.0
       and then Certified >= 0.0,
     Post => Sign * Determinant >= 0.0;
   --  Lemma_Shared_Root for the finish determinant (Yꜰ - Y₁)·(1 + Yꜰ·Y₁) - Yꜰ·(Y₂² + Y₃²). The normal coordinates
   --  enter squared, so the sign of their quotients does not matter, and their scale is the negated finish value.

   procedure Lemma_Finish_Algebra
     (Sign,
      Shared,
      Tangent_Rest,
      Normal_Rest,
      Tangent,
      Normal,
      Quotient_1,
      Factor,
      Quotient_2,
      Quotient_3,
      Scale,
      Finish,
      Y_1,
      Y_2,
      Y_3,
      Certified,
      Determinant : Real)
   is
      Whole_2 : constant Real := 0.0 - Y_2;
      Whole_3 : constant Real := 0.0 - Y_3;
      Squares : constant Real := Y_2 * Y_2 + Y_3 * Y_3;
   begin
      pragma Assert (Whole_2 * Whole_2 = Y_2 * Y_2);
      pragma Assert (Whole_3 * Whole_3 = Y_3 * Y_3);
      pragma Assert (Scale * Squares = 0.0 - Finish * Squares);
      pragma Assert (Determinant = (Finish - Y_1) * Factor + Scale * (Whole_2 * Whole_2 + Whole_3 * Whole_3));
      Lemma_Shared_Root
        (Sign         => Sign,
         Shared       => Shared,
         Tangent_Rest => Tangent_Rest,
         Normal_Rest  => Normal_Rest,
         Tangent      => Tangent,
         Normal       => Normal,
         Quotient_1   => Quotient_1,
         Factor       => Factor,
         Quotient_2   => Quotient_2,
         Quotient_3   => Quotient_3,
         Scale        => Scale,
         Difference   => Finish - Y_1,
         Whole_2      => Whole_2,
         Whole_3      => Whole_3,
         Certified    => Certified,
         Determinant  => Determinant);
   end Lemma_Finish_Algebra;

   procedure Lemma_Oriented_Cone (Sign, Turn_Value, Start, Finish : Real)
   with
     Ghost,
     Pre  =>
       ((Sign = 1.0 and then Turn_Value > 0.0) or else (Sign + 1.0 = 0.0 and then Turn_Value < 0.0))
       and then Sign * Start >= 0.0
       and then Sign * Finish >= 0.0,
     Post =>
       (Turn_Value > 0.0 and then Start >= 0.0 and then Finish >= 0.0)
       or else (Turn_Value < 0.0 and then Start <= 0.0 and then Finish <= 0.0);
   --  Determinants which are nonnegative once oriented by the sign of the turn have the sign of the turn.

   procedure Lemma_Oriented_Cone (Sign, Turn_Value, Start, Finish : Real) is null;

   ---------------------------------------------------------------------------
   --  Exact scalars
   ---------------------------------------------------------------------------

   procedure Lemma_Exact_Contains (Value : Dimensionless)
   with Ghost, Post => (if Interval_Exact (Value).Valid then Contains (Interval_Exact (Value), R (Value)));
   --  The enclosure of one stored value holds the real number which that value denotes.

   procedure Lemma_Exact_Contains (Value : Dimensionless) is null;

   procedure Lemma_Negation_Is_Exact (Value : Dimensionless)
   with Ghost, Post => R (-Value) = 0.0 - R (Value);
   --  Negating a floating-point number does not round.

   procedure Lemma_Negation_Is_Exact (Value : Dimensionless) is null;

   procedure Lemma_Unit_Is_Exact (Value : Dimensionless)
   with Ghost, Post => (if Value = 1.0 then R (Value) = 1.0) and then (if Value = -1.0 then R (Value) + 1.0 = 0.0);
   --  The two values which a proven orientation sign takes denote plus and minus one.

   procedure Lemma_Unit_Is_Exact (Value : Dimensionless) is null;

   ---------------------------------------------------------------------------
   --  The real polynomial which the assembled intervals enclose
   ---------------------------------------------------------------------------

   function Sum_Controls (Left, Right : Bernstein.Controls) return Bernstein.Controls
   with
     Ghost,
     Pre  => Bernstein.Same_Range (Left, Right),
     Post =>
       Bernstein.Same_Range (Sum_Controls'Result, Left)
       and then (for all K in Left'Range => Sum_Controls'Result (K) = Left (K) + Right (K));
   --  The controlwise sum.

   function Sum_Controls (Left, Right : Bernstein.Controls) return Bernstein.Controls is
      Result : Bernstein.Controls (Left'Range) := [others => 0.0];
   begin
      for K in Left'Range loop
         Result (K) := Left (K) + Right (K);
         pragma Loop_Invariant (for all J in Left'First .. K => Result (J) = Left (J) + Right (J));
      end loop;
      return Result;
   end Sum_Controls;

   function Scaled_Controls (Factor : Real; Exact : Bernstein.Controls) return Bernstein.Controls
   with
     Ghost,
     Post =>
       Bernstein.Same_Range (Scaled_Controls'Result, Exact)
       and then (for all K in Exact'Range => Scaled_Controls'Result (K) = Factor * Exact (K));
   --  Every control multiplied by Factor.

   function Scaled_Controls (Factor : Real; Exact : Bernstein.Controls) return Bernstein.Controls is
      Result : Bernstein.Controls (Exact'Range) := [others => 0.0];
   begin
      for K in Exact'Range loop
         Result (K) := Factor * Exact (K);
         pragma Loop_Invariant (for all J in Exact'First .. K => Result (J) = Factor * Exact (J));
      end loop;
      return Result;
   end Scaled_Controls;

   function Raised_Controls (Exact : Bernstein.Controls; By : Natural; At_Start : Boolean) return Bernstein.Controls
   is (if At_Start then Binomial_Form.Raised_At_Start (Exact, By) else Binomial_Form.Raised_At_Finish (Exact, By))
   with
     Ghost,
     Pre  => Binomial_Form.Is_Polynomial (Exact) and then By <= Maximum_Degree,
     Post => Raised_Controls'Result'First = 0 and then Raised_Controls'Result'Last = Exact'Last + By;
   --  The controls of the polynomial times V**By at the start, or times (1 - V)**By at the finish.

   function Is_Assembly
     (Tangent_Quotient, Factor, Normal_2, Normal_3 : Bernstein.Controls; Tangent_Power, Normal_Power : Natural)
      return Boolean
   is (Tangent_Quotient'First = 0
       and then Factor'First = 0
       and then Normal_2'First = 0
       and then Normal_3'First = 0
       and then Tangent_Quotient'Last in 1 .. Maximum_Degree
       and then Factor'Last in 1 .. Maximum_Degree
       and then Normal_2'Last in 1 .. Maximum_Degree
       and then Normal_3'Last = Normal_2'Last
       and then Tangent_Power <= Maximum_Degree
       and then Normal_Power <= Maximum_Degree
       and then Tangent_Quotient'Last + Factor'Last + Tangent_Power <= Maximum_Degree
       and then 2 * Normal_2'Last + Normal_Power = Tangent_Quotient'Last + Factor'Last + Tangent_Power)
   with Ghost;
   --  Whether four real polynomials and two powers have the degrees of an assembly: the two raised terms reach one
   --  degree, which is within the largest that an interval polynomial holds.

   function Exact_Assembled
     (Tangent_Quotient, Factor, Normal_2, Normal_3 : Bernstein.Controls;
      Inner, Outer                                 : Real;
      Tangent_Power, Normal_Power                  : Natural;
      At_Start                                     : Boolean) return Bernstein.Controls
   is (Scaled_Controls
         (Outer,
          Sum_Controls
            (Raised_Controls (Binomial_Form.Product (Tangent_Quotient, Factor), Tangent_Power, At_Start),
             Raised_Controls
               (Scaled_Controls
                  (Inner,
                   Sum_Controls
                     (Binomial_Form.Product (Normal_2, Normal_2), Binomial_Form.Product (Normal_3, Normal_3))),
                Normal_Power,
                At_Start))))
   with
     Ghost,
     Pre  => Is_Assembly (Tangent_Quotient, Factor, Normal_2, Normal_3, Tangent_Power, Normal_Power),
     Post =>
       Exact_Assembled'Result'First = 0
       and then Exact_Assembled'Result'Last = Tangent_Quotient'Last + Factor'Last + Tangent_Power;
   --  The controls of
   --
   --     Outer·(Bᵃ·Tangent_Quotient·Factor + Bᵇ·Inner·(Normal_2² + Normal_3²)),
   --
   --  where B is V at the start and 1 - V at the finish, and a, b are the two powers. It is built by the real
   --  counterpart of each interval operation, in the order in which End_Polynomial applies them.

   procedure Lemma_Substitute
     (Result,
      Total,
      Raised_Tangent,
      Raised_Normal,
      In_Plane,
      Normal,
      Squares,
      Square_2,
      Square_3,
      Tangent_Value,
      Factor_Value,
      Value_2,
      Value_3,
      Inner,
      Outer,
      Tangent_Scale,
      Normal_Scale : Real)
   with
     Ghost,
     Pre  =>
       Result = Outer * Total
       and then Total = Raised_Tangent + Raised_Normal
       and then Raised_Tangent = Tangent_Scale * In_Plane
       and then In_Plane = Tangent_Value * Factor_Value
       and then Raised_Normal = Normal_Scale * Normal
       and then Normal = Inner * Squares
       and then Squares = Square_2 + Square_3
       and then Square_2 = Value_2 * Value_2
       and then Square_3 = Value_3 * Value_3,
     Post =>
       Result
       = Outer
         * (Tangent_Scale * (Tangent_Value * Factor_Value)
            + Normal_Scale * (Inner * (Value_2 * Value_2 + Value_3 * Value_3)));

   procedure Lemma_Substitute
     (Result,
      Total,
      Raised_Tangent,
      Raised_Normal,
      In_Plane,
      Normal,
      Squares,
      Square_2,
      Square_3,
      Tangent_Value,
      Factor_Value,
      Value_2,
      Value_3,
      Inner,
      Outer,
      Tangent_Scale,
      Normal_Scale : Real)
   is null;
   --  The value of each stage of the assembly substituted into the next.

   procedure Lemma_Exact_Assembled_Value
     (Tangent_Quotient, Factor, Normal_2, Normal_3 : Bernstein.Controls;
      Inner, Outer                                 : Real;
      Tangent_Power, Normal_Power                  : Natural;
      At_Start                                     : Boolean;
      V, Base                                      : Real)
   with
     Ghost,
     Pre  =>
       Is_Assembly (Tangent_Quotient, Factor, Normal_2, Normal_3, Tangent_Power, Normal_Power)
       and then (if At_Start then Base = V else Base = 1.0 - V),
     Post =>
       Bernstein.Value
         (Exact_Assembled
            (Tangent_Quotient, Factor, Normal_2, Normal_3, Inner, Outer, Tangent_Power, Normal_Power, At_Start),
          V)
       = Outer
         * (Binomial_Form.Power (Base, Tangent_Power)
            * (Bernstein.Value (Tangent_Quotient, V) * Bernstein.Value (Factor, V))
            + Binomial_Form.Power (Base, Normal_Power)
              * (Inner
                 * (Bernstein.Value (Normal_2, V) * Bernstein.Value (Normal_2, V)
                    + Bernstein.Value (Normal_3, V) * Bernstein.Value (Normal_3, V))));
   --  The value of the assembled polynomial, from those of its four parts. Base is what the raised terms were
   --  multiplied by powers of.

   procedure Lemma_Exact_Assembled_Value
     (Tangent_Quotient, Factor, Normal_2, Normal_3 : Bernstein.Controls;
      Inner, Outer                                 : Real;
      Tangent_Power, Normal_Power                  : Natural;
      At_Start                                     : Boolean;
      V, Base                                      : Real)
   is
      In_Plane       : constant Bernstein.Controls := Binomial_Form.Product (Tangent_Quotient, Factor);
      Square_2       : constant Bernstein.Controls := Binomial_Form.Product (Normal_2, Normal_2);
      Square_3       : constant Bernstein.Controls := Binomial_Form.Product (Normal_3, Normal_3);
      Squares        : constant Bernstein.Controls := Sum_Controls (Square_2, Square_3);
      Normal         : constant Bernstein.Controls := Scaled_Controls (Inner, Squares);
      Raised_Tangent : constant Bernstein.Controls := Raised_Controls (In_Plane, Tangent_Power, At_Start);
      Raised_Normal  : constant Bernstein.Controls := Raised_Controls (Normal, Normal_Power, At_Start);
      Total          : constant Bernstein.Controls := Sum_Controls (Raised_Tangent, Raised_Normal);
      Result         : constant Bernstein.Controls := Scaled_Controls (Outer, Total);
   begin
      Binomial_Form.Lemma_Product (Tangent_Quotient, Factor, V);
      Binomial_Form.Lemma_Product (Normal_2, Normal_2, V);
      Binomial_Form.Lemma_Product (Normal_3, Normal_3, V);
      Bernstein.Lemma_Value_Is_Additive (Square_2, Square_3, Squares, V);
      Bernstein.Lemma_Value_Is_Homogeneous (Inner, Squares, Normal, V);
      if At_Start then
         Binomial_Form.Lemma_Raised_At_Start (In_Plane, Tangent_Power, V);
         Binomial_Form.Lemma_Raised_At_Start (Normal, Normal_Power, V);
      else
         Binomial_Form.Lemma_Raised_At_Finish (In_Plane, Tangent_Power, V);
         Binomial_Form.Lemma_Raised_At_Finish (Normal, Normal_Power, V);
      end if;
      pragma
        Assert
          (Bernstein.Value (Raised_Tangent, V)
           = Binomial_Form.Power (Base, Tangent_Power) * Bernstein.Value (In_Plane, V));
      pragma
        Assert
          (Bernstein.Value (Raised_Normal, V)
           = Binomial_Form.Power (Base, Normal_Power) * Bernstein.Value (Normal, V));
      Bernstein.Lemma_Value_Is_Additive (Raised_Tangent, Raised_Normal, Total, V);
      Bernstein.Lemma_Value_Is_Homogeneous (Outer, Total, Result, V);
      Lemma_Substitute
        (Result         => Bernstein.Value (Result, V),
         Total          => Bernstein.Value (Total, V),
         Raised_Tangent => Bernstein.Value (Raised_Tangent, V),
         Raised_Normal  => Bernstein.Value (Raised_Normal, V),
         In_Plane       => Bernstein.Value (In_Plane, V),
         Normal         => Bernstein.Value (Normal, V),
         Squares        => Bernstein.Value (Squares, V),
         Square_2       => Bernstein.Value (Square_2, V),
         Square_3       => Bernstein.Value (Square_3, V),
         Tangent_Value  => Bernstein.Value (Tangent_Quotient, V),
         Factor_Value   => Bernstein.Value (Factor, V),
         Value_2        => Bernstein.Value (Normal_2, V),
         Value_3        => Bernstein.Value (Normal_3, V),
         Inner          => Inner,
         Outer          => Outer,
         Tangent_Scale  => Binomial_Form.Power (Base, Tangent_Power),
         Normal_Scale   => Binomial_Form.Power (Base, Normal_Power));
   end Lemma_Exact_Assembled_Value;

   ---------------------------------------------------------------------------
   --  The assembled intervals
   ---------------------------------------------------------------------------

   function Assembled
     (Tangent_Quotient, Factor, Normal_2, Normal_3 : Polynomial;
      Inner, Outer                                 : Interval;
      Tangent_Power, Normal_Power                  : Natural;
      At_Start                                     : Boolean) return Polynomial
   is (Scaled
         (Outer,
          Sum
            (Raised (Multiply (Tangent_Quotient, Factor), Tangent_Power, At_Start),
             Raised
               (Scaled (Inner, Sum (Multiply (Normal_2, Normal_2), Multiply (Normal_3, Normal_3))),
                Normal_Power,
                At_Start))))
   with
     Ghost,
     Pre =>
       Tangent_Quotient.Degree >= 1
       and then Factor.Degree >= 1
       and then Normal_2.Degree >= 1
       and then Normal_3.Degree = Normal_2.Degree
       and then Tangent_Power <= Maximum_Degree
       and then Normal_Power <= Maximum_Degree
       and then Tangent_Quotient.Degree + Factor.Degree + Tangent_Power <= Maximum_Degree
       and then 2 * Normal_2.Degree + Normal_Power = Tangent_Quotient.Degree + Factor.Degree + Tangent_Power;
   --  The interval polynomial which End_Polynomial builds from the two quotients of the normal coordinates, the
   --  quotient of the first coordinate, the linear factor, the two scales and the two powers.

   procedure Lemma_Assembled_Encloses
     (Tangent_Quotient, Factor, Normal_2, Normal_3                : Polynomial;
      Inner, Outer                                                : Interval;
      Tangent_Power, Normal_Power                                 : Natural;
      At_Start                                                    : Boolean;
      Exact_Tangent, Exact_Factor, Exact_Normal_2, Exact_Normal_3 : Bernstein.Controls;
      Exact_Inner, Exact_Outer                                    : Real)
   with
     Ghost,
     Pre  =>
       Tangent_Quotient.Degree >= 1
       and then Factor.Degree >= 1
       and then Normal_2.Degree >= 1
       and then Normal_3.Degree = Normal_2.Degree
       and then Tangent_Power <= Maximum_Degree
       and then Normal_Power <= Maximum_Degree
       and then Tangent_Quotient.Degree + Factor.Degree + Tangent_Power <= Maximum_Degree
       and then 2 * Normal_2.Degree + Normal_Power = Tangent_Quotient.Degree + Factor.Degree + Tangent_Power
       and then Exact_Tangent'First = 0
       and then Exact_Tangent'Last = Tangent_Quotient.Degree
       and then Exact_Factor'First = 0
       and then Exact_Factor'Last = Factor.Degree
       and then Exact_Normal_2'First = 0
       and then Exact_Normal_2'Last = Normal_2.Degree
       and then Exact_Normal_3'First = 0
       and then Exact_Normal_3'Last = Normal_3.Degree
       and then (if All_Valid (Tangent_Quotient) then Encloses (Tangent_Quotient, Exact_Tangent))
       and then (if All_Valid (Factor) then Encloses (Factor, Exact_Factor))
       and then (if All_Valid (Normal_2) then Encloses (Normal_2, Exact_Normal_2))
       and then (if All_Valid (Normal_3) then Encloses (Normal_3, Exact_Normal_3))
       and then (if Inner.Valid then Contains (Inner, Exact_Inner))
       and then (if Outer.Valid then Contains (Outer, Exact_Outer)),
     Post =>
       (if All_Valid
             (Assembled
                (Tangent_Quotient, Factor, Normal_2, Normal_3, Inner, Outer, Tangent_Power, Normal_Power, At_Start))
        then
          Encloses
            (Assembled
               (Tangent_Quotient, Factor, Normal_2, Normal_3, Inner, Outer, Tangent_Power, Normal_Power, At_Start),
             Exact_Assembled
               (Exact_Tangent,
                Exact_Factor,
                Exact_Normal_2,
                Exact_Normal_3,
                Exact_Inner,
                Exact_Outer,
                Tangent_Power,
                Normal_Power,
                At_Start)));
   --  A valid assembly encloses the assembly of the real polynomials and scales which its parts enclose, where
   --  a part need only enclose its real counterpart if it is itself valid.

   procedure Lemma_Assembled_Encloses
     (Tangent_Quotient, Factor, Normal_2, Normal_3                : Polynomial;
      Inner, Outer                                                : Interval;
      Tangent_Power, Normal_Power                                 : Natural;
      At_Start                                                    : Boolean;
      Exact_Tangent, Exact_Factor, Exact_Normal_2, Exact_Normal_3 : Bernstein.Controls;
      Exact_Inner, Exact_Outer                                    : Real)
   is
      In_Plane       : constant Polynomial := Multiply (Tangent_Quotient, Factor);
      Square_2       : constant Polynomial := Multiply (Normal_2, Normal_2);
      Square_3       : constant Polynomial := Multiply (Normal_3, Normal_3);
      Squares        : constant Polynomial := Sum (Square_2, Square_3);
      Normal         : constant Polynomial := Scaled (Inner, Squares);
      Raised_Tangent : constant Polynomial := Raised (In_Plane, Tangent_Power, At_Start);
      Raised_Normal  : constant Polynomial := Raised (Normal, Normal_Power, At_Start);
      Total          : constant Polynomial := Sum (Raised_Tangent, Raised_Normal);
      Result         : constant Polynomial := Scaled (Outer, Total);

      Exact_In_Plane       : constant Bernstein.Controls := Binomial_Form.Product (Exact_Tangent, Exact_Factor);
      Exact_Square_2       : constant Bernstein.Controls := Binomial_Form.Product (Exact_Normal_2, Exact_Normal_2);
      Exact_Square_3       : constant Bernstein.Controls := Binomial_Form.Product (Exact_Normal_3, Exact_Normal_3);
      Exact_Squares        : constant Bernstein.Controls := Sum_Controls (Exact_Square_2, Exact_Square_3);
      Exact_Normal         : constant Bernstein.Controls := Scaled_Controls (Exact_Inner, Exact_Squares);
      Exact_Raised_Tangent : constant Bernstein.Controls := Raised_Controls (Exact_In_Plane, Tangent_Power, At_Start);
      Exact_Raised_Normal  : constant Bernstein.Controls := Raised_Controls (Exact_Normal, Normal_Power, At_Start);
      Exact_Total          : constant Bernstein.Controls := Sum_Controls (Exact_Raised_Tangent, Exact_Raised_Normal);
      Exact_Result         : constant Bernstein.Controls := Scaled_Controls (Exact_Outer, Exact_Total);
   begin
      if All_Valid (Result) then
         --  Validity walks backwards: an operation with a valid result had valid operands.
         pragma Assert (Outer.Valid and then All_Valid (Total));
         pragma Assert (All_Valid (Raised_Tangent) and then All_Valid (Raised_Normal));
         pragma Assert (All_Valid (In_Plane) and then All_Valid (Normal));
         pragma Assert (All_Valid (Tangent_Quotient) and then All_Valid (Factor));
         pragma Assert (Inner.Valid and then All_Valid (Squares));
         pragma Assert (All_Valid (Square_2) and then All_Valid (Square_3));
         pragma Assert (All_Valid (Normal_2) and then All_Valid (Normal_3));

         --  The enclosures walk forwards: each operation encloses the real operation on what its operands enclose.
         Lemma_Multiply_Encloses (Tangent_Quotient, Factor, Exact_Tangent, Exact_Factor);
         Lemma_Multiply_Encloses (Normal_2, Normal_2, Exact_Normal_2, Exact_Normal_2);
         Lemma_Multiply_Encloses (Normal_3, Normal_3, Exact_Normal_3, Exact_Normal_3);
         Lemma_Sum_Encloses (Square_2, Square_3, Exact_Square_2, Exact_Square_3, Exact_Squares);
         Lemma_Scaled_Encloses (Inner, Squares, Exact_Inner, Exact_Squares, Exact_Normal);
         Lemma_Raised_Encloses (In_Plane, Tangent_Power, At_Start, Exact_In_Plane);
         Lemma_Raised_Encloses (Normal, Normal_Power, At_Start, Exact_Normal);
         pragma Assert (Encloses (Raised_Tangent, Exact_Raised_Tangent));
         pragma Assert (Encloses (Raised_Normal, Exact_Raised_Normal));
         Lemma_Sum_Encloses (Raised_Tangent, Raised_Normal, Exact_Raised_Tangent, Exact_Raised_Normal, Exact_Total);
         Lemma_Scaled_Encloses (Outer, Total, Exact_Outer, Exact_Total, Exact_Result);
         pragma Assert (Encloses (Result, Exact_Result));
      end if;
   end Lemma_Assembled_Encloses;

   ---------------------------------------------------------------------------
   --  The certified polynomial of one end
   ---------------------------------------------------------------------------

   function End_Value (Coefficients : Bernstein_Chart; At_Start : Boolean) return Dimensionless
   is (Coefficients ((if At_Start then 0 else Fixed_Chart_Degree), 1))
   with Ghost;
   --  The first chart coordinate at one end.

   function Inner_Scale (Coefficients : Bernstein_Chart; At_Start : Boolean) return Dimensionless
   is (if At_Start then End_Value (Coefficients, At_Start) else -End_Value (Coefficients, At_Start))
   with Ghost;
   --  What scales the normal square in the cone determinant: Yₛ at the start and -Yꜰ at the finish.

   function Tangent_Root_Order (Coefficients : Bernstein_Chart; At_Start : Boolean) return Root_Order_Range
   is (Root_Order (Coefficients, 1, At_Start))
   with Ghost, Post => Has_Root_Order (Coefficients, 1, Tangent_Root_Order'Result, At_Start);
   --  The order of the root of the first chart coordinate's distance from its endpoint value which the certificate
   --  divides out.

   function Normal_Root_Order (Coefficients : Bernstein_Chart; At_Start : Boolean) return Root_Order_Range
   is (Positive'Min (Root_Order (Coefficients, 2, At_Start), Root_Order (Coefficients, 3, At_Start)))
   with
     Ghost,
     Post =>
       Has_Root_Order (Coefficients, 2, Normal_Root_Order'Result, At_Start)
       and then Has_Root_Order (Coefficients, 3, Normal_Root_Order'Result, At_Start);
   --  The order of the root which the certificate divides out of both normal coordinates. A root of some order is
   --  also a root of every lower order, so both have one of the lower of their two orders.

   function Shared_Root_Order (Coefficients : Bernstein_Chart; At_Start : Boolean) return Root_Order_Range
   is (Positive'Min (Tangent_Root_Order (Coefficients, At_Start), 2 * Normal_Root_Order (Coefficients, At_Start)))
   with Ghost;
   --  The order of the root which the in-plane term and the squared normal term share.

   function Tangent_Rest_Order (Coefficients : Bernstein_Chart; At_Start : Boolean) return Natural
   is (Tangent_Root_Order (Coefficients, At_Start) - Shared_Root_Order (Coefficients, At_Start))
   with Ghost;
   --  The power which the certificate multiplies the in-plane term by.

   function Normal_Rest_Order (Coefficients : Bernstein_Chart; At_Start : Boolean) return Natural
   is (2 * Normal_Root_Order (Coefficients, At_Start) - Shared_Root_Order (Coefficients, At_Start))
   with Ghost;
   --  The power which the certificate multiplies the squared normal term by.

   function Exact_End
     (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless) return Bernstein.Controls
   is (Exact_Assembled
         (Tangent_Quotient =>
            Quotient_Controls (Coefficients, 1, Tangent_Root_Order (Coefficients, At_Start), At_Start),
          Factor           => Linear_Factor_Controls (Coefficients, End_Value (Coefficients, At_Start)),
          Normal_2         =>
            Quotient_Controls (Coefficients, 2, Normal_Root_Order (Coefficients, At_Start), At_Start),
          Normal_3         =>
            Quotient_Controls (Coefficients, 3, Normal_Root_Order (Coefficients, At_Start), At_Start),
          Inner            => R (Inner_Scale (Coefficients, At_Start)),
          Outer            => R (Sign),
          Tangent_Power    => Tangent_Rest_Order (Coefficients, At_Start),
          Normal_Power     => Normal_Rest_Order (Coefficients, At_Start),
          At_Start         => At_Start))
   with
     Ghost,
     Post =>
       Exact_End'Result'First = 0
       and then Exact_End'Result'Last = 2 * Fixed_Chart_Degree - Shared_Root_Order (Coefficients, At_Start);
   --  The real polynomial which End_Polynomial encloses.

   procedure Lemma_End_Encloses
     (Coefficients                                 : Bernstein_Chart;
      At_Start                                     : Boolean;
      Sign                                         : Dimensionless;
      Tangent_Quotient, Factor, Normal_2, Normal_3 : Polynomial;
      Inner, Outer                                 : Interval;
      Tangent_Power, Normal_Power                  : Natural)
   with
     Ghost,
     Pre  =>
       Tangent_Quotient.Degree = Fixed_Chart_Degree - Tangent_Root_Order (Coefficients, At_Start)
       and then Factor.Degree = Fixed_Chart_Degree
       and then Normal_2.Degree = Fixed_Chart_Degree - Normal_Root_Order (Coefficients, At_Start)
       and then Normal_3.Degree = Normal_2.Degree
       and then Tangent_Power = Tangent_Rest_Order (Coefficients, At_Start)
       and then Normal_Power = Normal_Rest_Order (Coefficients, At_Start)
       and then
         (if All_Valid (Tangent_Quotient)
          then
            Encloses
              (Tangent_Quotient,
               Quotient_Controls (Coefficients, 1, Tangent_Root_Order (Coefficients, At_Start), At_Start)))
       and then
         (if All_Valid (Factor)
          then Encloses (Factor, Linear_Factor_Controls (Coefficients, End_Value (Coefficients, At_Start))))
       and then
         (if All_Valid (Normal_2)
          then
            Encloses
              (Normal_2, Quotient_Controls (Coefficients, 2, Normal_Root_Order (Coefficients, At_Start), At_Start)))
       and then
         (if All_Valid (Normal_3)
          then
            Encloses
              (Normal_3, Quotient_Controls (Coefficients, 3, Normal_Root_Order (Coefficients, At_Start), At_Start)))
       and then (if Inner.Valid then Contains (Inner, R (Inner_Scale (Coefficients, At_Start))))
       and then (if Outer.Valid then Contains (Outer, R (Sign))),
     Post =>
       (if All_Valid
             (Assembled
                (Tangent_Quotient, Factor, Normal_2, Normal_3, Inner, Outer, Tangent_Power, Normal_Power, At_Start))
        then
          Encloses
            (Assembled
               (Tangent_Quotient, Factor, Normal_2, Normal_3, Inner, Outer, Tangent_Power, Normal_Power, At_Start),
             Exact_End (Coefficients, At_Start, Sign)));
   --  The assembly of enclosures of the quotients, of the linear factor and of the two scales of one end encloses
   --  the real polynomial of that end.

   procedure Lemma_End_Encloses
     (Coefficients                                 : Bernstein_Chart;
      At_Start                                     : Boolean;
      Sign                                         : Dimensionless;
      Tangent_Quotient, Factor, Normal_2, Normal_3 : Polynomial;
      Inner, Outer                                 : Interval;
      Tangent_Power, Normal_Power                  : Natural) is
   begin
      Lemma_Assembled_Encloses
        (Tangent_Quotient => Tangent_Quotient,
         Factor           => Factor,
         Normal_2         => Normal_2,
         Normal_3         => Normal_3,
         Inner            => Inner,
         Outer            => Outer,
         Tangent_Power    => Tangent_Power,
         Normal_Power     => Normal_Power,
         At_Start         => At_Start,
         Exact_Tangent    =>
           Quotient_Controls (Coefficients, 1, Tangent_Root_Order (Coefficients, At_Start), At_Start),
         Exact_Factor     => Linear_Factor_Controls (Coefficients, End_Value (Coefficients, At_Start)),
         Exact_Normal_2   => Quotient_Controls (Coefficients, 2, Normal_Root_Order (Coefficients, At_Start), At_Start),
         Exact_Normal_3   => Quotient_Controls (Coefficients, 3, Normal_Root_Order (Coefficients, At_Start), At_Start),
         Exact_Inner      => R (Inner_Scale (Coefficients, At_Start)),
         Exact_Outer      => R (Sign));
   end Lemma_End_Encloses;

   function End_Polynomial (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless) return Polynomial
   with
     Post =>
       (if All_Valid (End_Polynomial'Result)
        then Encloses (End_Polynomial'Result, Exact_End (Coefficients, At_Start, Sign)));
   --  The oriented cone determinant at one end with the root which all of its terms share divided out.

   function End_Polynomial (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless) return Polynomial
   is
      Value            : constant Dimensionless := Coefficients ((if At_Start then 0 else Fixed_Chart_Degree), 1);
      Tangent_Order    : constant Root_Order_Range := Root_Order (Coefficients, 1, At_Start);
      Normal_Order     : constant Root_Order_Range :=
        Positive'Min (Root_Order (Coefficients, 2, At_Start), Root_Order (Coefficients, 3, At_Start));
      Shared_Order     : constant Root_Order_Range := Positive'Min (Tangent_Order, 2 * Normal_Order);
      Normal_2         : constant Polynomial := Quotient (Coefficients, 2, Normal_Order, At_Start);
      Normal_3         : constant Polynomial := Quotient (Coefficients, 3, Normal_Order, At_Start);
      In_Plane_Product : constant Polynomial :=
        Multiply (Quotient (Coefficients, 1, Tangent_Order, At_Start), Linear_Factor (Coefficients, Value));
      Normal_Square    : constant Polynomial :=
        Scaled
          (Interval_Exact (if At_Start then Value else -Value),
           Sum (Multiply (Normal_2, Normal_2), Multiply (Normal_3, Normal_3)));
   begin
      --  The two scales enclose the real numbers which they were loaded from, and each quotient and the linear
      --  factor enclose their real polynomials when valid, so a valid result encloses the real polynomial of this
      --  end.
      Lemma_Exact_Contains (Inner_Scale (Coefficients, At_Start));
      Lemma_Exact_Contains (Sign);
      Lemma_End_Encloses
        (Coefficients     => Coefficients,
         At_Start         => At_Start,
         Sign             => Sign,
         Tangent_Quotient => Quotient (Coefficients, 1, Tangent_Order, At_Start),
         Factor           => Linear_Factor (Coefficients, Value),
         Normal_2         => Normal_2,
         Normal_3         => Normal_3,
         Inner            => Interval_Exact (if At_Start then Value else -Value),
         Outer            => Interval_Exact (Sign),
         Tangent_Power    => Tangent_Order - Shared_Order,
         Normal_Power     => 2 * Normal_Order - Shared_Order);
      return
        Scaled
          (Interval_Exact (Sign),
           Sum
             (Raised (In_Plane_Product, Tangent_Order - Shared_Order, At_Start),
              Raised (Normal_Square, 2 * Normal_Order - Shared_Order, At_Start)));
   end End_Polynomial;

   function Is_Certified (Coefficients : Bernstein_Chart) return Boolean
   with
     Refined_Post =>
       (if Is_Certified'Result
        then
          Endpoints_Are_In_Frame_Plane (Coefficients)
          and then Orientation_Sign (Coefficients) /= 0.0
          and then Is_Nonnegative (End_Polynomial (Coefficients, True, Orientation_Sign (Coefficients)))
          and then Is_Nonnegative (End_Polynomial (Coefficients, False, Orientation_Sign (Coefficients))))
   is
      Sign : Dimensionless;
   begin
      for C in 2 .. 3 loop
         if Coefficients (0, C) /= 0.0 or else Coefficients (Fixed_Chart_Degree, C) /= 0.0 then
            --  This frame plane does not contain both endpoint tangents.
            return False;
         end if;
      end loop;
      Sign := Orientation_Sign (Coefficients);
      if Sign = 0.0 then
         return False;
      end if;
      return
        Is_Nonnegative (End_Polynomial (Coefficients, True, Sign))
        and then Is_Nonnegative (End_Polynomial (Coefficients, False, Sign));
   end Is_Certified;

   ---------------------------------------------------------------------------
   --  What the certificate establishes
   ---------------------------------------------------------------------------

   function Tangent_Quotient_Value (Coefficients : Bernstein_Chart; At_Start : Boolean; V : Real) return Real
   is (Bernstein.Value (Quotient_Controls (Coefficients, 1, Tangent_Root_Order (Coefficients, At_Start), At_Start), V))
   with Ghost;
   --  The value at V of the quotient of the first chart coordinate.

   function Normal_Quotient_Value
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; At_Start : Boolean; V : Real) return Real
   is (Bernstein.Value
         (Quotient_Controls (Coefficients, Component, Normal_Root_Order (Coefficients, At_Start), At_Start), V))
   with Ghost, Pre => Component in 2 .. 3;
   --  The value at V of the quotient of one normal coordinate.

   function Factor_Value (Coefficients : Bernstein_Chart; At_Start : Boolean; V : Real) return Real
   is (Bernstein.Value (Linear_Factor_Controls (Coefficients, End_Value (Coefficients, At_Start)), V))
   with Ghost;
   --  The value at V of the linear factor of one end.

   procedure Lemma_Exact_End_Is_Nonnegative
     (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless; V : Real)
   with
     Ghost,
     Pre  => Is_Nonnegative (End_Polynomial (Coefficients, At_Start, Sign)) and then 0.0 <= V and then V <= 1.0,
     Post => Bernstein.Value (Exact_End (Coefficients, At_Start, Sign), V) >= 0.0;
   --  A polynomial which passes the test has only valid controls, so it encloses the real polynomial of its end,
   --  which the test then keeps nonnegative.

   procedure Lemma_Exact_End_Is_Nonnegative
     (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless; V : Real) is
   begin
      Lemma_Is_Nonnegative
        (End_Polynomial (Coefficients, At_Start, Sign), Exact_End (Coefficients, At_Start, Sign), V);
   end Lemma_Exact_End_Is_Nonnegative;

   procedure Lemma_Exact_End_Value
     (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless; V, Base : Real)
   with
     Ghost,
     Pre  => (if At_Start then Base = V else Base = 1.0 - V),
     Post =>
       Bernstein.Value (Exact_End (Coefficients, At_Start, Sign), V)
       = R (Sign)
         * (Binomial_Form.Power (Base, Tangent_Rest_Order (Coefficients, At_Start))
            * (Tangent_Quotient_Value (Coefficients, At_Start, V) * Factor_Value (Coefficients, At_Start, V))
            + Binomial_Form.Power (Base, Normal_Rest_Order (Coefficients, At_Start))
              * (R (Inner_Scale (Coefficients, At_Start))
                 * (Normal_Quotient_Value (Coefficients, 2, At_Start, V)
                    * Normal_Quotient_Value (Coefficients, 2, At_Start, V)
                    + Normal_Quotient_Value (Coefficients, 3, At_Start, V)
                      * Normal_Quotient_Value (Coefficients, 3, At_Start, V))));
   --  The value of the real polynomial of one end, from those of the quotients and of the linear factor.

   procedure Lemma_Exact_End_Value
     (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless; V, Base : Real) is
   begin
      Lemma_Exact_Assembled_Value
        (Tangent_Quotient =>
           Quotient_Controls (Coefficients, 1, Tangent_Root_Order (Coefficients, At_Start), At_Start),
         Factor           => Linear_Factor_Controls (Coefficients, End_Value (Coefficients, At_Start)),
         Normal_2         => Quotient_Controls (Coefficients, 2, Normal_Root_Order (Coefficients, At_Start), At_Start),
         Normal_3         => Quotient_Controls (Coefficients, 3, Normal_Root_Order (Coefficients, At_Start), At_Start),
         Inner            => R (Inner_Scale (Coefficients, At_Start)),
         Outer            => R (Sign),
         Tangent_Power    => Tangent_Rest_Order (Coefficients, At_Start),
         Normal_Power     => Normal_Rest_Order (Coefficients, At_Start),
         At_Start         => At_Start,
         V                => V,
         Base             => Base);
   end Lemma_Exact_End_Value;

   procedure Lemma_Root_Powers (Base : Real; Tangent, Normal, Shared : Root_Order_Range)
   with
     Ghost,
     Pre  => Base >= 0.0 and then Shared <= Tangent and then Shared <= 2 * Normal,
     Post =>
       Binomial_Form.Power (Base, Tangent)
       = Binomial_Form.Power (Base, Shared) * Binomial_Form.Power (Base, Tangent - Shared)
       and then
         Binomial_Form.Power (Base, Normal) * Binomial_Form.Power (Base, Normal)
         = Binomial_Form.Power (Base, Shared) * Binomial_Form.Power (Base, 2 * Normal - Shared)
       and then Binomial_Form.Power (Base, Shared) >= 0.0;
   --  The shared power divides the power of the in-plane term and the squared power of the normal coordinates.

   procedure Lemma_Root_Powers (Base : Real; Tangent, Normal, Shared : Root_Order_Range) is
   begin
      Binomial_Form.Lemma_Power_Of_Sum (Base, Shared, Tangent - Shared);
      Binomial_Form.Lemma_Power_Of_Sum (Base, Normal, Normal);
      Binomial_Form.Lemma_Power_Of_Sum (Base, Shared, 2 * Normal - Shared);
      Binomial_Form.Lemma_Power_Is_Nonnegative (Base, Shared);
      pragma Assert (Normal + Normal = Shared + (2 * Normal - Shared));
   end Lemma_Root_Powers;

   procedure Lemma_Start_Roots (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Pre  => Endpoints_Are_In_Frame_Plane (Coefficients),
     Post =>
       Chart_Value (Coefficients, 1, V) - Start_Value (Coefficients)
       = Binomial_Form.Power (V, Tangent_Root_Order (Coefficients, True))
         * Tangent_Quotient_Value (Coefficients, True, V)
       and then
         Chart_Value (Coefficients, 2, V)
         = Binomial_Form.Power (V, Normal_Root_Order (Coefficients, True))
           * Normal_Quotient_Value (Coefficients, 2, True, V)
       and then
         Chart_Value (Coefficients, 3, V)
         = Binomial_Form.Power (V, Normal_Root_Order (Coefficients, True))
           * Normal_Quotient_Value (Coefficients, 3, True, V)
       and then
         Factor_Value (Coefficients, True, V) = 1.0 + Start_Value (Coefficients) * Chart_Value (Coefficients, 1, V)
       and then R (Inner_Scale (Coefficients, True)) = Start_Value (Coefficients);
   --  At the start, each quotient times the power of V which it divided out is its chart coordinate's distance
   --  from the start value, which is zero for the normal coordinates.

   procedure Lemma_Start_Roots (Coefficients : Bernstein_Chart; V : Real) is
   begin
      Lemma_Quotient_Value (Coefficients, 1, Tangent_Root_Order (Coefficients, True), True, V);
      Lemma_Quotient_Value (Coefficients, 2, Normal_Root_Order (Coefficients, True), True, V);
      Lemma_Quotient_Value (Coefficients, 3, Normal_Root_Order (Coefficients, True), True, V);
      Lemma_Linear_Factor_Value (Coefficients, End_Value (Coefficients, True), V);
      pragma Assert (R (Coefficients (0, 2)) = 0.0 and then R (Coefficients (0, 3)) = 0.0);
   end Lemma_Start_Roots;

   procedure Lemma_Finish_Roots (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Pre  => Endpoints_Are_In_Frame_Plane (Coefficients),
     Post =>
       Finish_Value (Coefficients) - Chart_Value (Coefficients, 1, V)
       = Binomial_Form.Power (1.0 - V, Tangent_Root_Order (Coefficients, False))
         * Tangent_Quotient_Value (Coefficients, False, V)
       and then
         0.0 - Chart_Value (Coefficients, 2, V)
         = Binomial_Form.Power (1.0 - V, Normal_Root_Order (Coefficients, False))
           * Normal_Quotient_Value (Coefficients, 2, False, V)
       and then
         0.0 - Chart_Value (Coefficients, 3, V)
         = Binomial_Form.Power (1.0 - V, Normal_Root_Order (Coefficients, False))
           * Normal_Quotient_Value (Coefficients, 3, False, V)
       and then
         Factor_Value (Coefficients, False, V) = 1.0 + Finish_Value (Coefficients) * Chart_Value (Coefficients, 1, V)
       and then R (Inner_Scale (Coefficients, False)) = 0.0 - Finish_Value (Coefficients);
   --  Likewise at the finish, in powers of 1 - V and with the distances measured from the finish value.

   procedure Lemma_Finish_Roots (Coefficients : Bernstein_Chart; V : Real) is
   begin
      Lemma_Quotient_Value (Coefficients, 1, Tangent_Root_Order (Coefficients, False), False, V);
      Lemma_Quotient_Value (Coefficients, 2, Normal_Root_Order (Coefficients, False), False, V);
      Lemma_Quotient_Value (Coefficients, 3, Normal_Root_Order (Coefficients, False), False, V);
      Lemma_Linear_Factor_Value (Coefficients, End_Value (Coefficients, False), V);
      Lemma_Negation_Is_Exact (End_Value (Coefficients, False));
      pragma
        Assert
          (R (Coefficients (Fixed_Chart_Degree, 2)) = 0.0 and then R (Coefficients (Fixed_Chart_Degree, 3)) = 0.0);
   end Lemma_Finish_Roots;

   procedure Lemma_Determinants (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Post =>
       Start_Determinant (Coefficients, V)
       = (Chart_Value (Coefficients, 1, V) - Start_Value (Coefficients))
         * (1.0 + Start_Value (Coefficients) * Chart_Value (Coefficients, 1, V))
         + Start_Value (Coefficients)
           * (Chart_Value (Coefficients, 2, V) * Chart_Value (Coefficients, 2, V)
              + Chart_Value (Coefficients, 3, V) * Chart_Value (Coefficients, 3, V))
       and then
         Finish_Determinant (Coefficients, V)
         = (Finish_Value (Coefficients) - Chart_Value (Coefficients, 1, V))
           * (1.0 + Finish_Value (Coefficients) * Chart_Value (Coefficients, 1, V))
           - Finish_Value (Coefficients)
             * (Chart_Value (Coefficients, 2, V) * Chart_Value (Coefficients, 2, V)
                + Chart_Value (Coefficients, 3, V) * Chart_Value (Coefficients, 3, V));
   --  The two cone determinants written out in the chart coordinates.

   procedure Lemma_Determinants (Coefficients : Bernstein_Chart; V : Real) is null;

   procedure Lemma_Start (Coefficients : Bernstein_Chart; Sign : Dimensionless; V : Real)
   with
     Ghost,
     Pre  =>
       Endpoints_Are_In_Frame_Plane (Coefficients)
       and then Is_Nonnegative (End_Polynomial (Coefficients, True, Sign))
       and then 0.0 <= V
       and then V <= 1.0,
     Post => R (Sign) * Start_Determinant (Coefficients, V) >= 0.0;
   --  A nonnegative certified polynomial makes the oriented start determinant nonnegative at V.

   procedure Lemma_Start (Coefficients : Bernstein_Chart; Sign : Dimensionless; V : Real) is
      Tangent : constant Root_Order_Range := Tangent_Root_Order (Coefficients, True);
      Normal  : constant Root_Order_Range := Normal_Root_Order (Coefficients, True);
      Shared  : constant Root_Order_Range := Shared_Root_Order (Coefficients, True);
   begin
      Lemma_Exact_End_Is_Nonnegative (Coefficients, True, Sign, V);
      Lemma_Exact_End_Value (Coefficients, True, Sign, V, V);
      Lemma_Root_Powers (V, Tangent, Normal, Shared);
      Lemma_Start_Roots (Coefficients, V);
      Lemma_Determinants (Coefficients, V);
      Lemma_Start_Algebra
        (Sign         => R (Sign),
         Shared       => Binomial_Form.Power (V, Shared),
         Tangent_Rest => Binomial_Form.Power (V, Tangent_Rest_Order (Coefficients, True)),
         Normal_Rest  => Binomial_Form.Power (V, Normal_Rest_Order (Coefficients, True)),
         Tangent      => Binomial_Form.Power (V, Tangent),
         Normal       => Binomial_Form.Power (V, Normal),
         Quotient_1   => Tangent_Quotient_Value (Coefficients, True, V),
         Factor       => Factor_Value (Coefficients, True, V),
         Quotient_2   => Normal_Quotient_Value (Coefficients, 2, True, V),
         Quotient_3   => Normal_Quotient_Value (Coefficients, 3, True, V),
         Scale        => R (Inner_Scale (Coefficients, True)),
         Start        => Start_Value (Coefficients),
         Y_1          => Chart_Value (Coefficients, 1, V),
         Y_2          => Chart_Value (Coefficients, 2, V),
         Y_3          => Chart_Value (Coefficients, 3, V),
         Certified    => Bernstein.Value (Exact_End (Coefficients, True, Sign), V),
         Determinant  => Start_Determinant (Coefficients, V));
   end Lemma_Start;

   procedure Lemma_Finish (Coefficients : Bernstein_Chart; Sign : Dimensionless; V : Real)
   with
     Ghost,
     Pre  =>
       Endpoints_Are_In_Frame_Plane (Coefficients)
       and then Is_Nonnegative (End_Polynomial (Coefficients, False, Sign))
       and then 0.0 <= V
       and then V <= 1.0,
     Post => R (Sign) * Finish_Determinant (Coefficients, V) >= 0.0;
   --  A nonnegative certified polynomial makes the oriented finish determinant nonnegative at V.

   procedure Lemma_Finish (Coefficients : Bernstein_Chart; Sign : Dimensionless; V : Real) is
      Tangent : constant Root_Order_Range := Tangent_Root_Order (Coefficients, False);
      Normal  : constant Root_Order_Range := Normal_Root_Order (Coefficients, False);
      Shared  : constant Root_Order_Range := Shared_Root_Order (Coefficients, False);
      Base    : constant Real := 1.0 - V;
   begin
      Lemma_Exact_End_Is_Nonnegative (Coefficients, False, Sign, V);
      Lemma_Exact_End_Value (Coefficients, False, Sign, V, Base);
      Lemma_Root_Powers (Base, Tangent, Normal, Shared);
      Lemma_Finish_Roots (Coefficients, V);
      Lemma_Determinants (Coefficients, V);
      Lemma_Finish_Algebra
        (Sign         => R (Sign),
         Shared       => Binomial_Form.Power (Base, Shared),
         Tangent_Rest => Binomial_Form.Power (Base, Tangent_Rest_Order (Coefficients, False)),
         Normal_Rest  => Binomial_Form.Power (Base, Normal_Rest_Order (Coefficients, False)),
         Tangent      => Binomial_Form.Power (Base, Tangent),
         Normal       => Binomial_Form.Power (Base, Normal),
         Quotient_1   => Tangent_Quotient_Value (Coefficients, False, V),
         Factor       => Factor_Value (Coefficients, False, V),
         Quotient_2   => Normal_Quotient_Value (Coefficients, 2, False, V),
         Quotient_3   => Normal_Quotient_Value (Coefficients, 3, False, V),
         Scale        => R (Inner_Scale (Coefficients, False)),
         Finish       => Finish_Value (Coefficients),
         Y_1          => Chart_Value (Coefficients, 1, V),
         Y_2          => Chart_Value (Coefficients, 2, V),
         Y_3          => Chart_Value (Coefficients, 3, V),
         Certified    => Bernstein.Value (Exact_End (Coefficients, False, Sign), V),
         Determinant  => Finish_Determinant (Coefficients, V));
   end Lemma_Finish;

   procedure Lemma_Certified (Coefficients : Bernstein_Chart; V : Real) is
      Sign : constant Dimensionless := Orientation_Sign (Coefficients);
   begin
      Lemma_Start (Coefficients, Sign, V);
      Lemma_Finish (Coefficients, Sign, V);
      Lemma_Unit_Is_Exact (Sign);
      Lemma_Oriented_Cone
        (R (Sign), Turn (Coefficients), Start_Determinant (Coefficients, V), Finish_Determinant (Coefficients, V));
   end Lemma_Certified;

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Spatial;
