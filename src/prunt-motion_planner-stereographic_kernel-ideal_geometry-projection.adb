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

package body Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Projection
  with SPARK_Mode => On
is

   Numerator_Degree : constant := 2 * Fixed_Chart_Degree;
   --  The degree of the numerator, which is that of the square of a chart coordinate.

   subtype Term_Count is Natural range 0 .. 3;
   --  How many of the three chart coordinates a partial sum has taken in.

   ---------------------------------------------------------------------------
   --  Real polynomials, control by control
   ---------------------------------------------------------------------------

   function Constant_Controls (Level : Real; Degree : Degree_Range) return Bernstein.Controls
   with
     Ghost,
     Post =>
       Constant_Controls'Result'First = 0
       and then Constant_Controls'Result'Last = Degree
       and then (for all K in 0 .. Degree => Constant_Controls'Result (K) = Level);
   --  The controls, at the given degree, of the polynomial which is Level everywhere.

   function Constant_Controls (Level : Real; Degree : Degree_Range) return Bernstein.Controls is
      Result : constant Bernstein.Controls (0 .. Degree) := [others => Level];
   begin
      return Result;
   end Constant_Controls;

   function Scaled_Controls (Factor : Real; P : Bernstein.Controls) return Bernstein.Controls
   with
     Ghost,
     Post =>
       Bernstein.Same_Range (Scaled_Controls'Result, P)
       and then (for all K in P'Range => Scaled_Controls'Result (K) = Factor * P (K));
   --  The controls of Factor times the polynomial P.

   function Scaled_Controls (Factor : Real; P : Bernstein.Controls) return Bernstein.Controls is
      Result : Bernstein.Controls (P'Range) := P;
   begin
      for K in P'Range loop
         Result (K) := Factor * P (K);
         pragma Loop_Invariant (for all J in P'First .. K => Result (J) = Factor * P (J));
      end loop;
      return Result;
   end Scaled_Controls;

   function Sum_Controls (P, Q : Bernstein.Controls) return Bernstein.Controls
   with
     Ghost,
     Pre  => Bernstein.Same_Range (P, Q),
     Post =>
       Bernstein.Same_Range (Sum_Controls'Result, P)
       and then (for all K in P'Range => Sum_Controls'Result (K) = P (K) + Q (K));
   --  The controls of the sum of the polynomials P and Q.

   function Sum_Controls (P, Q : Bernstein.Controls) return Bernstein.Controls is
      Result : Bernstein.Controls (P'Range) := P;
   begin
      for K in P'Range loop
         Result (K) := P (K) + Q (K);
         pragma Loop_Invariant (for all J in P'First .. K => Result (J) = P (J) + Q (J));
      end loop;
      return Result;
   end Sum_Controls;

   ---------------------------------------------------------------------------
   --  The real numerator
   ---------------------------------------------------------------------------

   function Square_Controls (Coefficients : Bernstein_Chart; C : Chart_Component_Index) return Bernstein.Controls
   is (Bernstein.Binomial_Form.Product (Chart_Controls (Coefficients, C), Chart_Controls (Coefficients, C)))
   with Ghost, Post => Square_Controls'Result'First = 0 and then Square_Controls'Result'Last = Numerator_Degree;
   --  The controls of Y꜀².

   function Squares_Controls (Coefficients : Bernstein_Chart; Count : Term_Count) return Bernstein.Controls
   is (if Count = 0
       then Constant_Controls (0.0, Numerator_Degree)
       else Sum_Controls (Squares_Controls (Coefficients, Count - 1), Square_Controls (Coefficients, Count)))
   with
     Ghost,
     Subprogram_Variant => (Decreases => Count),
     Post               => Squares_Controls'Result'First = 0 and then Squares_Controls'Result'Last = Numerator_Degree;
   --  The controls of the sum of the squares of the first Count chart coordinates, taken in from zero in the order
   --  in which Numerator_Polynomial adds them. With all three this is Y·Y.

   function Elevated_Controls (Coefficients : Bernstein_Chart; C : Chart_Component_Index) return Bernstein.Controls
   is (Bernstein.Binomial_Form.Product (Chart_Controls (Coefficients, C), Constant_Controls (1.0, Fixed_Chart_Degree)))
   with Ghost, Post => Elevated_Controls'Result'First = 0 and then Elevated_Controls'Result'Last = Numerator_Degree;
   --  The controls of Y꜀ times the constant one of degree eight, which is Y꜀ at the numerator's degree.

   function Base_Controls
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale) return Bernstein.Controls
   is (Sum_Controls
         (Constant_Controls (Dot_Real (Frame (0), Direction), Numerator_Degree),
          Scaled_Controls (0.0 - Dot_Real (Frame (0), Direction), Squares_Controls (Coefficients, 3))))
   with
     Ghost,
     Post =>
       Base_Controls'Result'First = 0
       and then Base_Controls'Result'Last = Numerator_Degree
       and then
         (for all K in 0 .. Numerator_Degree =>
            Base_Controls'Result (K)
            = Dot_Real (Frame (0), Direction)
              - Dot_Real (Frame (0), Direction) * Squares_Controls (Coefficients, 3) (K));
   --  The controls of (Frame₀·Direction)·(1 - Y·Y), each in the form in which its enclosure is computed. The
   --  negated factor is written as a difference from zero because GNATprove 16.1 fails to translate the unary
   --  minus of a Big_Real.

   function Term_Controls
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Direction    : Position_Scale;
      C            : Chart_Component_Index) return Bernstein.Controls
   is (Scaled_Controls (2.0, Scaled_Controls (Dot_Real (Frame (C), Direction), Elevated_Controls (Coefficients, C))))
   with
     Ghost,
     Post =>
       Term_Controls'Result'First = 0
       and then Term_Controls'Result'Last = Numerator_Degree
       and then
         (for all K in 0 .. Numerator_Degree =>
            Term_Controls'Result (K)
            = 2.0 * (Dot_Real (Frame (C), Direction) * Elevated_Controls (Coefficients, C) (K)));
   --  The controls of 2·(Frame꜀·Direction)·Y꜀.

   function Numerator_Controls
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale; Count : Term_Count)
      return Bernstein.Controls
   is (if Count = 0
       then Base_Controls (Frame, Coefficients, Direction)
       else
         Sum_Controls
           (Numerator_Controls (Frame, Coefficients, Direction, Count - 1),
            Term_Controls (Frame, Coefficients, Direction, Count)))
   with
     Ghost,
     Subprogram_Variant => (Decreases => Count),
     Post               =>
       Numerator_Controls'Result'First = 0 and then Numerator_Controls'Result'Last = Numerator_Degree;
   --  The controls of the projected numerator with the terms of the first Count chart coordinates added to
   --  (Frame₀·Direction)·(1 - Y·Y), in the order in which Numerator_Polynomial adds them.

   procedure Lemma_Numerator_Control_Step
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Direction    : Position_Scale;
      Count        : Chart_Component_Index;
      K            : Degree_Range)
   with
     Ghost,
     Post =>
       Numerator_Controls (Frame, Coefficients, Direction, Count) (K)
       = Numerator_Controls (Frame, Coefficients, Direction, Count - 1) (K)
         + 2.0 * (Dot_Real (Frame (Count), Direction) * Elevated_Controls (Coefficients, Count) (K));
   --  Each control with one more term in is the control before plus that term's control.

   procedure Lemma_Numerator_Control_Step
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Direction    : Position_Scale;
      Count        : Chart_Component_Index;
      K            : Degree_Range)
   is
      Previous : constant Bernstein.Controls := Numerator_Controls (Frame, Coefficients, Direction, Count - 1);
      Term     : constant Bernstein.Controls := Term_Controls (Frame, Coefficients, Direction, Count);
   begin
      pragma Assert (Numerator_Controls (Frame, Coefficients, Direction, Count) (K) = Previous (K) + Term (K));
   end Lemma_Numerator_Control_Step;

   ---------------------------------------------------------------------------
   --  The value of the real numerator
   ---------------------------------------------------------------------------

   procedure Lemma_Squares_Value_Step (Coefficients : Bernstein_Chart; Count : Chart_Component_Index; V : Real)
   with
     Ghost,
     Post =>
       Bernstein.Value (Squares_Controls (Coefficients, Count), V)
       = Bernstein.Value (Squares_Controls (Coefficients, Count - 1), V)
         + Chart_Value (Coefficients, Count, V) * Chart_Value (Coefficients, Count, V);
   --  Taking in one more chart coordinate adds its square, by the product and the sum of polynomials.

   procedure Lemma_Squares_Value_Step (Coefficients : Bernstein_Chart; Count : Chart_Component_Index; V : Real) is
   begin
      Bernstein.Binomial_Form.Lemma_Product
        (Chart_Controls (Coefficients, Count), Chart_Controls (Coefficients, Count), V);
      Bernstein.Lemma_Value_Is_Additive
        (Squares_Controls (Coefficients, Count - 1),
         Square_Controls (Coefficients, Count),
         Squares_Controls (Coefficients, Count),
         V);
   end Lemma_Squares_Value_Step;

   procedure Lemma_Squares_Value (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Post =>
       Bernstein.Value (Squares_Controls (Coefficients, 3), V)
       = Chart_Value (Coefficients, 1, V) * Chart_Value (Coefficients, 1, V)
         + Chart_Value (Coefficients, 2, V) * Chart_Value (Coefficients, 2, V)
         + Chart_Value (Coefficients, 3, V) * Chart_Value (Coefficients, 3, V);
   --  The sum of all three squares is Y·Y.

   procedure Lemma_Squares_Value (Coefficients : Bernstein_Chart; V : Real) is
   begin
      Bernstein.Binomial_Form.Lemma_Value_Of_Constant (Squares_Controls (Coefficients, 0), 0.0, V);
      Lemma_Squares_Value_Step (Coefficients, 1, V);
      Lemma_Squares_Value_Step (Coefficients, 2, V);
      Lemma_Squares_Value_Step (Coefficients, 3, V);
   end Lemma_Squares_Value;

   procedure Lemma_Base_Value
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale; V : Real)
   with
     Ghost,
     Post =>
       Bernstein.Value (Base_Controls (Frame, Coefficients, Direction), V)
       = Dot_Real (Frame (0), Direction)
         - Dot_Real (Frame (0), Direction) * Bernstein.Value (Squares_Controls (Coefficients, 3), V);
   --  The value is linear in the controls: a constant, plus a multiple of the squares.

   procedure Lemma_Base_Value
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale; V : Real)
   is
      F      : constant Real := Dot_Real (Frame (0), Direction);
      Level  : constant Bernstein.Controls := Constant_Controls (F, Numerator_Degree);
      Scaled : constant Bernstein.Controls := Scaled_Controls (0.0 - F, Squares_Controls (Coefficients, 3));
   begin
      Bernstein.Binomial_Form.Lemma_Value_Of_Constant (Level, F, V);
      Bernstein.Lemma_Value_Is_Homogeneous (0.0 - F, Squares_Controls (Coefficients, 3), Scaled, V);
      Bernstein.Lemma_Value_Is_Additive (Level, Scaled, Base_Controls (Frame, Coefficients, Direction), V);
   end Lemma_Base_Value;

   procedure Lemma_Elevated_Value (Coefficients : Bernstein_Chart; C : Chart_Component_Index; V : Real)
   with Ghost, Post => Bernstein.Value (Elevated_Controls (Coefficients, C), V) = Chart_Value (Coefficients, C, V);
   --  Multiplying by the constant one changes the degree and not the polynomial.

   procedure Lemma_Elevated_Value (Coefficients : Bernstein_Chart; C : Chart_Component_Index; V : Real) is
   begin
      Bernstein.Binomial_Form.Lemma_Value_Of_Constant (Constant_Controls (1.0, Fixed_Chart_Degree), 1.0, V);
      Bernstein.Binomial_Form.Lemma_Product
        (Chart_Controls (Coefficients, C), Constant_Controls (1.0, Fixed_Chart_Degree), V);
   end Lemma_Elevated_Value;

   procedure Lemma_Term_Value
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Direction    : Position_Scale;
      C            : Chart_Component_Index;
      V            : Real)
   with
     Ghost,
     Post =>
       Bernstein.Value (Term_Controls (Frame, Coefficients, Direction, C), V)
       = 2.0 * (Dot_Real (Frame (C), Direction) * Chart_Value (Coefficients, C, V));
   --  Scaling the controls, once by the projection and once by two, scales the value.

   procedure Lemma_Term_Value
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Direction    : Position_Scale;
      C            : Chart_Component_Index;
      V            : Real)
   is
      F         : constant Real := Dot_Real (Frame (C), Direction);
      Elevated  : constant Bernstein.Controls := Elevated_Controls (Coefficients, C);
      Projected : constant Bernstein.Controls := Scaled_Controls (F, Elevated);
   begin
      Lemma_Elevated_Value (Coefficients, C, V);
      Bernstein.Lemma_Value_Is_Homogeneous (F, Elevated, Projected, V);
      Bernstein.Lemma_Value_Is_Homogeneous (2.0, Projected, Term_Controls (Frame, Coefficients, Direction, C), V);
   end Lemma_Term_Value;

   procedure Lemma_Numerator_Value_Step
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Direction    : Position_Scale;
      Count        : Chart_Component_Index;
      V            : Real)
   with
     Ghost,
     Post =>
       Bernstein.Value (Numerator_Controls (Frame, Coefficients, Direction, Count), V)
       = Bernstein.Value (Numerator_Controls (Frame, Coefficients, Direction, Count - 1), V)
         + 2.0 * (Dot_Real (Frame (Count), Direction) * Chart_Value (Coefficients, Count, V));
   --  Adding the term of one more chart coordinate adds its value.

   procedure Lemma_Numerator_Value_Step
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Direction    : Position_Scale;
      Count        : Chart_Component_Index;
      V            : Real) is
   begin
      Lemma_Term_Value (Frame, Coefficients, Direction, Count, V);
      Bernstein.Lemma_Value_Is_Additive
        (Numerator_Controls (Frame, Coefficients, Direction, Count - 1),
         Term_Controls (Frame, Coefficients, Direction, Count),
         Numerator_Controls (Frame, Coefficients, Direction, Count),
         V);
   end Lemma_Numerator_Value_Step;

   procedure Lemma_Projection_Algebra (F0, F1, F2, F3, Y1, Y2, Y3, S, N0, N1, N2, N3, Projected : Real)
   with
     Ghost,
     Pre  =>
       S = Y1 * Y1 + Y2 * Y2 + Y3 * Y3
       and then N0 = F0 - F0 * S
       and then N1 = N0 + 2.0 * (F1 * Y1)
       and then N2 = N1 + 2.0 * (F2 * Y2)
       and then N3 = N2 + 2.0 * (F3 * Y3)
       and then
         Projected = (1.0 - Y1 * Y1 - (Y2 * Y2 + Y3 * Y3)) * F0 + (2.0 * Y1) * F1 + (2.0 * Y2) * F2 + (2.0 * Y3) * F3,
     Post => N3 = Projected;
   --  The numerator as it is accumulated is the numerator as Projected_Numerator states it.

   procedure Lemma_Projection_Algebra (F0, F1, F2, F3, Y1, Y2, Y3, S, N0, N1, N2, N3, Projected : Real) is
   begin
      pragma Assert (1.0 - Y1 * Y1 - (Y2 * Y2 + Y3 * Y3) = 1.0 - S);
      pragma Assert ((1.0 - S) * F0 = N0);
      pragma Assert ((2.0 * Y1) * F1 = N1 - N0);
      pragma Assert ((2.0 * Y2) * F2 = N2 - N1);
      pragma Assert ((2.0 * Y3) * F3 = N3 - N2);
      pragma Assert (Projected = (1.0 - S) * F0 + (N1 - N0) + (N2 - N1) + (N3 - N2));
   end Lemma_Projection_Algebra;

   procedure Lemma_Numerator_Value
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale; V : Real)
   with
     Ghost,
     Post =>
       Bernstein.Value (Numerator_Controls (Frame, Coefficients, Direction, 3), V)
       = Projected_Numerator (Frame, Coefficients, Direction, V);
   --  The real polynomial which Numerator_Polynomial encloses is the projected numerator.

   procedure Lemma_Numerator_Value
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale; V : Real) is
   begin
      Lemma_Squares_Value (Coefficients, V);
      Lemma_Base_Value (Frame, Coefficients, Direction, V);
      Lemma_Numerator_Value_Step (Frame, Coefficients, Direction, 1, V);
      Lemma_Numerator_Value_Step (Frame, Coefficients, Direction, 2, V);
      Lemma_Numerator_Value_Step (Frame, Coefficients, Direction, 3, V);
      Lemma_Projection_Algebra
        (F0        => Dot_Real (Frame (0), Direction),
         F1        => Dot_Real (Frame (1), Direction),
         F2        => Dot_Real (Frame (2), Direction),
         F3        => Dot_Real (Frame (3), Direction),
         Y1        => Chart_Value (Coefficients, 1, V),
         Y2        => Chart_Value (Coefficients, 2, V),
         Y3        => Chart_Value (Coefficients, 3, V),
         S         => Bernstein.Value (Squares_Controls (Coefficients, 3), V),
         N0        => Bernstein.Value (Numerator_Controls (Frame, Coefficients, Direction, 0), V),
         N1        => Bernstein.Value (Numerator_Controls (Frame, Coefficients, Direction, 1), V),
         N2        => Bernstein.Value (Numerator_Controls (Frame, Coefficients, Direction, 2), V),
         N3        => Bernstein.Value (Numerator_Controls (Frame, Coefficients, Direction, 3), V),
         Projected => Projected_Numerator (Frame, Coefficients, Direction, V));
   end Lemma_Numerator_Value;

   ---------------------------------------------------------------------------
   --  What each interval control encloses
   ---------------------------------------------------------------------------

   --  An interval polynomial is known to enclose a real one only once all of its controls are known to be valid,
   --  and for the operands of the numerator that is known only at the end, from the validity of every control of
   --  the numerator. Until then each control carries the two halves of the argument separately: that it is
   --  invalid unless the matching controls of its operands are valid, and that it holds the matching real control
   --  if they hold theirs.
   --
   --  Numerator_Polynomial sees the three predicates below only through the lemmas which relate them. What they
   --  mean is stated on their bodies, so that the real polynomials, which are defined by recursion, stay out of
   --  the proof of its loops.

   package Soundness
     with Ghost
   is

      function Elevated_Polynomial (Coefficients : Bernstein_Chart; C : Chart_Component_Index) return Polynomial
      is (Multiply
            (Chart_Polynomial (Coefficients, C), Constant_Polynomial (Interval_Exact (1.0), Fixed_Chart_Degree)))
      with Post => Elevated_Polynomial'Result.Degree = Numerator_Degree;
      --  One chart coordinate at the numerator's degree, as Numerator_Polynomial computes it.

      function Squares_Are_Enclosed
        (Squares : Polynomial; Coefficients : Bernstein_Chart; Count : Term_Count) return Boolean
      with Global => null;
      --  What is known of the sum of the squares of the first Count chart coordinates: if all of its controls
      --  are valid, it encloses Squares_Controls (Coefficients, Count).

      function Control_Is_Sound
        (Control      : Interval;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale;
         Count        : Term_Count;
         K            : Degree_Range) return Boolean
      with Global => null;
      --  What is known of control K of the numerator once the terms of the first Count chart coordinates are in:
      --  if it is valid, then so is control K of the sum of squares and of each of those elevated coordinates,
      --  and if each of them holds control K of its real polynomial, then Control holds control K of
      --  Numerator_Controls (Frame, Coefficients, Direction, Count).

      function Numerator_Is_Enclosed
        (Value : Polynomial; Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale)
         return Boolean
      with Global => null;
      --  What is known of the whole numerator: if all of its controls are valid, it encloses
      --  Numerator_Controls (Frame, Coefficients, Direction, 3).

      procedure Lemma_No_Squares (Coefficients : Bernstein_Chart)
      with
        Post => Squares_Are_Enclosed (Constant_Polynomial (Interval_Exact (0.0), Numerator_Degree), Coefficients, 0);
      --  The sum of squares starts as the zero polynomial, which encloses the empty sum.

      procedure Lemma_Add_Square (Squares : Polynomial; Coefficients : Bernstein_Chart; C : Chart_Component_Index)
      with
        Pre  => Squares.Degree = Numerator_Degree and then Squares_Are_Enclosed (Squares, Coefficients, C - 1),
        Post =>
          Squares_Are_Enclosed
            (Sum (Squares, Multiply (Chart_Polynomial (Coefficients, C), Chart_Polynomial (Coefficients, C))),
             Coefficients,
             C);
      --  Adding the square of one more chart coordinate keeps the sum of squares enclosed. A valid sum has valid
      --  operands, and a valid product encloses the product of what its operands enclose.

      procedure Lemma_Base_Is_Sound
        (Projection   : Interval;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale;
         K            : Degree_Range)
      with
        Pre  => (if Projection.Valid then Contains (Projection, Dot_Real (Frame (0), Direction))),
        Post =>
          Control_Is_Sound
            (Interval_Subtract (Projection, Interval_Multiply (Projection, Squares.Controls (K))),
             Squares,
             Frame,
             Coefficients,
             Direction,
             0,
             K);
      --  The control F₀ - F₀·Sₖ as the numerator starts with it. A valid difference and a valid product have
      --  valid operands, and each holds the difference or the product of what its operands hold.

      procedure Lemma_Term_Is_Sound
        (Previous     : Interval;
         Projection   : Interval;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale;
         Count        : Chart_Component_Index;
         K            : Degree_Range)
      with
        Pre  =>
          Control_Is_Sound (Previous, Squares, Frame, Coefficients, Direction, Count - 1, K)
          and then (if Projection.Valid then Contains (Projection, Dot_Real (Frame (Count), Direction))),
        Post =>
          Control_Is_Sound
            (Interval_Add
               (Previous,
                Interval_Multiply
                  (Interval_Exact (2.0),
                   Interval_Multiply (Projection, Elevated_Polynomial (Coefficients, Count).Controls (K)))),
             Squares,
             Frame,
             Coefficients,
             Direction,
             Count,
             K);
      --  The control after the term 2·F꜀·Eₖ of one more chart coordinate is added to it. A valid sum and a valid
      --  product have valid operands, and each holds the sum or the product of what its operands hold.

      procedure Lemma_Sound_Controls_Enclose
        (Value        : Polynomial;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale)
      with
        Pre  =>
          Value.Degree = Numerator_Degree
          and then Squares.Degree = Numerator_Degree
          and then Squares_Are_Enclosed (Squares, Coefficients, 3)
          and then
            (for all K in Degree_Range =>
               Control_Is_Sound (Value.Controls (K), Squares, Frame, Coefficients, Direction, 3, K)),
        Post => Numerator_Is_Enclosed (Value, Frame, Coefficients, Direction);
      --  With every control of the numerator valid, every control of each operand is valid, so each operand
      --  encloses its real polynomial, and therefore so does the numerator.

      procedure Lemma_Numerator_Encloses
        (Value : Polynomial; Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale)
      with
        Pre  => All_Valid (Value) and then Numerator_Is_Enclosed (Value, Frame, Coefficients, Direction),
        Post => Encloses (Value, Numerator_Controls (Frame, Coefficients, Direction, 3));
      --  What Numerator_Is_Enclosed means for a numerator whose controls are all valid.

   end Soundness;

   package body Soundness is

      function Operands_Are_Valid
        (Squares : Polynomial; Coefficients : Bernstein_Chart; Count : Term_Count; K : Degree_Range) return Boolean
      is (Squares.Controls (K).Valid
          and then (for all D in 1 .. Count => Elevated_Polynomial (Coefficients, D).Controls (K).Valid));
      --  Whether control K of the sum of squares and of each of the first Count elevated chart coordinates is
      --  valid.

      function Operands_Are_Enclosed
        (Squares : Polynomial; Coefficients : Bernstein_Chart; Count : Term_Count; K : Degree_Range) return Boolean
      is (Contains (Squares.Controls (K), Squares_Controls (Coefficients, 3) (K))
          and then
            (for all D in 1 .. Count =>
               Contains
                 (Elevated_Polynomial (Coefficients, D).Controls (K), Elevated_Controls (Coefficients, D) (K))));
      --  Whether each of those controls holds the matching control of the real polynomial.

      function Squares_Are_Enclosed
        (Squares : Polynomial; Coefficients : Bernstein_Chart; Count : Term_Count) return Boolean
      with
        Refined_Post =>
          Squares_Are_Enclosed'Result
          = (if All_Valid (Squares) then Encloses (Squares, Squares_Controls (Coefficients, Count)))
      is
      begin
         return (if All_Valid (Squares) then Encloses (Squares, Squares_Controls (Coefficients, Count)));
      end Squares_Are_Enclosed;

      function Control_Is_Sound
        (Control      : Interval;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale;
         Count        : Term_Count;
         K            : Degree_Range) return Boolean
      with
        Refined_Post =>
          Control_Is_Sound'Result
          = (if Control.Valid
             then
               Operands_Are_Valid (Squares, Coefficients, Count, K)
               and then
                 (if Operands_Are_Enclosed (Squares, Coefficients, Count, K)
                  then Contains (Control, Numerator_Controls (Frame, Coefficients, Direction, Count) (K))))
      is
      begin
         return
           (if Control.Valid
            then
              Operands_Are_Valid (Squares, Coefficients, Count, K)
              and then
                (if Operands_Are_Enclosed (Squares, Coefficients, Count, K)
                 then Contains (Control, Numerator_Controls (Frame, Coefficients, Direction, Count) (K))));
      end Control_Is_Sound;

      function Numerator_Is_Enclosed
        (Value : Polynomial; Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale)
         return Boolean
      with
        Refined_Post =>
          Numerator_Is_Enclosed'Result
          = (if All_Valid (Value) then Encloses (Value, Numerator_Controls (Frame, Coefficients, Direction, 3)))
      is
      begin
         return (if All_Valid (Value) then Encloses (Value, Numerator_Controls (Frame, Coefficients, Direction, 3)));
      end Numerator_Is_Enclosed;

      procedure Lemma_Exact_Contains (Value : Dimensionless)
      with Pre => Is_Finite (Value), Post => Contains (Interval_Exact (Value), R (Value));
      --  The interval of one finite value holds that value.

      procedure Lemma_Exact_Contains (Value : Dimensionless) is null;

      procedure Lemma_No_Squares (Coefficients : Bernstein_Chart) is
      begin
         Lemma_Exact_Contains (0.0);
         Lemma_Constant_Encloses (Interval_Exact (0.0), Numerator_Degree, 0.0, Squares_Controls (Coefficients, 0));
      end Lemma_No_Squares;

      procedure Lemma_Add_Square (Squares : Polynomial; Coefficients : Bernstein_Chart; C : Chart_Component_Index) is
         Chart  : constant Polynomial := Chart_Polynomial (Coefficients, C);
         Square : constant Polynomial := Multiply (Chart, Chart);
      begin
         if All_Valid (Sum (Squares, Square)) then
            Lemma_Multiply_Encloses (Chart, Chart, Chart_Controls (Coefficients, C), Chart_Controls (Coefficients, C));
            Lemma_Sum_Encloses
              (Squares,
               Square,
               Squares_Controls (Coefficients, C - 1),
               Square_Controls (Coefficients, C),
               Squares_Controls (Coefficients, C));
         end if;
      end Lemma_Add_Square;

      procedure Lemma_Elevated_Encloses (Coefficients : Bernstein_Chart; C : Chart_Component_Index)
      with
        Post =>
          (if All_Valid (Elevated_Polynomial (Coefficients, C))
           then Encloses (Elevated_Polynomial (Coefficients, C), Elevated_Controls (Coefficients, C)));
      --  A valid elevated chart coordinate encloses the real one: the chart coordinate encloses its controls,
      --  and the constant-one polynomial the real controls which all equal one.

      procedure Lemma_Elevated_Encloses (Coefficients : Bernstein_Chart; C : Chart_Component_Index) is
         Chart : constant Polynomial := Chart_Polynomial (Coefficients, C);
         Ones  : constant Polynomial := Constant_Polynomial (Interval_Exact (1.0), Fixed_Chart_Degree);
      begin
         Lemma_Exact_Contains (1.0);
         Lemma_Constant_Encloses
           (Interval_Exact (1.0), Fixed_Chart_Degree, 1.0, Constant_Controls (1.0, Fixed_Chart_Degree));
         if All_Valid (Multiply (Chart, Ones)) then
            Lemma_Multiply_Encloses
              (Chart, Ones, Chart_Controls (Coefficients, C), Constant_Controls (1.0, Fixed_Chart_Degree));
         end if;
      end Lemma_Elevated_Encloses;

      procedure Lemma_Base_Is_Sound
        (Projection   : Interval;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale;
         K            : Degree_Range)
      is
         F       : constant Real := Dot_Real (Frame (0), Direction);
         S       : constant Real := Squares_Controls (Coefficients, 3) (K);
         Square  : constant Interval := Squares.Controls (K);
         Product : constant Interval := Interval_Multiply (Projection, Square);
         Control : constant Interval := Interval_Subtract (Projection, Product);
      begin
         if Control.Valid then
            pragma Assert (Projection.Valid and then Product.Valid);
            pragma Assert (Square.Valid);
            pragma Assert (Operands_Are_Valid (Squares, Coefficients, 0, K));
            if Operands_Are_Enclosed (Squares, Coefficients, 0, K) then
               pragma Assert (Contains (Square, S));
               Lemma_Multiply_Contains (Projection, Square, F, S);
               Lemma_Subtract_Contains (Projection, Product, F, F * S);
               pragma Assert (Numerator_Controls (Frame, Coefficients, Direction, 0) (K) = F - F * S);
            end if;
         end if;
      end Lemma_Base_Is_Sound;

      procedure Lemma_Term_Is_Sound
        (Previous     : Interval;
         Projection   : Interval;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale;
         Count        : Chart_Component_Index;
         K            : Degree_Range)
      is
         F        : constant Real := Dot_Real (Frame (Count), Direction);
         E        : constant Real := Elevated_Controls (Coefficients, Count) (K);
         Exact    : constant Real := Numerator_Controls (Frame, Coefficients, Direction, Count - 1) (K);
         Elevated : constant Interval := Elevated_Polynomial (Coefficients, Count).Controls (K);
         Two      : constant Interval := Interval_Exact (2.0);
         Inner    : constant Interval := Interval_Multiply (Projection, Elevated);
         Doubled  : constant Interval := Interval_Multiply (Two, Inner);
         Control  : constant Interval := Interval_Add (Previous, Doubled);
      begin
         if Control.Valid then
            pragma Assert (Previous.Valid and then Doubled.Valid);
            pragma Assert (Inner.Valid);
            pragma Assert (Projection.Valid and then Elevated.Valid);
            pragma Assert (Operands_Are_Valid (Squares, Coefficients, Count - 1, K));
            pragma Assert (Operands_Are_Valid (Squares, Coefficients, Count, K));
            if Operands_Are_Enclosed (Squares, Coefficients, Count, K) then
               pragma Assert (Operands_Are_Enclosed (Squares, Coefficients, Count - 1, K));
               pragma Assert (Contains (Previous, Exact));
               pragma Assert (Contains (Elevated, E));
               Lemma_Exact_Contains (2.0);
               Lemma_Multiply_Contains (Projection, Elevated, F, E);
               Lemma_Multiply_Contains (Two, Inner, 2.0, F * E);
               Lemma_Add_Contains (Previous, Doubled, Exact, 2.0 * (F * E));
               Lemma_Numerator_Control_Step (Frame, Coefficients, Direction, Count, K);
            end if;
         end if;
      end Lemma_Term_Is_Sound;

      procedure Lemma_Sound_Control_Operands
        (Control      : Interval;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale;
         K            : Degree_Range)
      with
        Pre  => Control.Valid and then Control_Is_Sound (Control, Squares, Frame, Coefficients, Direction, 3, K),
        Post =>
          Squares.Controls (K).Valid
          and then Elevated_Polynomial (Coefficients, 1).Controls (K).Valid
          and then Elevated_Polynomial (Coefficients, 2).Controls (K).Valid
          and then Elevated_Polynomial (Coefficients, 3).Controls (K).Valid;
      --  A valid control of the finished numerator has valid operands.

      procedure Lemma_Sound_Control_Operands
        (Control      : Interval;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale;
         K            : Degree_Range) is
      begin
         pragma Assert (Control_Is_Sound (Control, Squares, Frame, Coefficients, Direction, 3, K));
         pragma Assert (Operands_Are_Valid (Squares, Coefficients, 3, K));
      end Lemma_Sound_Control_Operands;

      procedure Lemma_Sound_Control_Contains
        (Control      : Interval;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale;
         K            : Degree_Range)
      with
        Pre  =>
          Control.Valid
          and then Control_Is_Sound (Control, Squares, Frame, Coefficients, Direction, 3, K)
          and then Contains (Squares.Controls (K), Squares_Controls (Coefficients, 3) (K))
          and then
            Contains (Elevated_Polynomial (Coefficients, 1).Controls (K), Elevated_Controls (Coefficients, 1) (K))
          and then
            Contains (Elevated_Polynomial (Coefficients, 2).Controls (K), Elevated_Controls (Coefficients, 2) (K))
          and then
            Contains (Elevated_Polynomial (Coefficients, 3).Controls (K), Elevated_Controls (Coefficients, 3) (K)),
        Post => Contains (Control, Numerator_Controls (Frame, Coefficients, Direction, 3) (K));
      --  A valid control of the finished numerator whose operands hold their real controls holds its own.

      procedure Lemma_Sound_Control_Contains
        (Control      : Interval;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale;
         K            : Degree_Range) is
      begin
         pragma
           Assert
             (for all D in 1 .. 3 =>
                Contains
                  (Elevated_Polynomial (Coefficients, D).Controls (K), Elevated_Controls (Coefficients, D) (K)));
         pragma Assert (Operands_Are_Enclosed (Squares, Coefficients, 3, K));
         pragma Assert (Control_Is_Sound (Control, Squares, Frame, Coefficients, Direction, 3, K));
      end Lemma_Sound_Control_Contains;

      procedure Lemma_Sound_Controls_Enclose
        (Value        : Polynomial;
         Squares      : Polynomial;
         Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Direction    : Position_Scale) is
      begin
         if All_Valid (Value) then
            for K in Degree_Range loop
               Lemma_Sound_Control_Operands (Value.Controls (K), Squares, Frame, Coefficients, Direction, K);
               pragma
                 Loop_Invariant
                   (for all J in 0 .. K =>
                      Squares.Controls (J).Valid
                      and then Elevated_Polynomial (Coefficients, 1).Controls (J).Valid
                      and then Elevated_Polynomial (Coefficients, 2).Controls (J).Valid
                      and then Elevated_Polynomial (Coefficients, 3).Controls (J).Valid);
            end loop;
            pragma Assert (All_Valid (Squares));
            pragma Assert (Encloses (Squares, Squares_Controls (Coefficients, 3)));
            pragma Assert (All_Valid (Elevated_Polynomial (Coefficients, 1)));
            Lemma_Elevated_Encloses (Coefficients, 1);
            pragma Assert (All_Valid (Elevated_Polynomial (Coefficients, 2)));
            Lemma_Elevated_Encloses (Coefficients, 2);
            pragma Assert (All_Valid (Elevated_Polynomial (Coefficients, 3)));
            Lemma_Elevated_Encloses (Coefficients, 3);
            for K in Degree_Range loop
               Lemma_Sound_Control_Contains (Value.Controls (K), Squares, Frame, Coefficients, Direction, K);
               pragma
                 Loop_Invariant
                   (for all J in 0 .. K =>
                      Contains (Value.Controls (J), Numerator_Controls (Frame, Coefficients, Direction, 3) (J)));
            end loop;
            pragma Assert (Encloses (Value, Numerator_Controls (Frame, Coefficients, Direction, 3)));
         end if;
      end Lemma_Sound_Controls_Enclose;

      procedure Lemma_Numerator_Encloses
        (Value : Polynomial; Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale)
      is null;

   end Soundness;

   use Soundness;

   ---------------------------------------------------------------------------
   --  The certificate
   ---------------------------------------------------------------------------

   function Numerator_Polynomial
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale) return Polynomial
   with
     Post =>
       Numerator_Polynomial'Result.Degree = Numerator_Degree
       and then Numerator_Is_Enclosed (Numerator_Polynomial'Result, Frame, Coefficients, Direction);
   --  The tangent numerator projected onto Direction, at degree sixteen.

   function Numerator_Polynomial
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale) return Polynomial
   is
      Ones               : constant Polynomial := Constant_Polynomial (Interval_Exact (1.0), Fixed_Chart_Degree);
      Frame_0_Projection : constant Interval := Dot_Interval (Frame (0), Direction);
      Squares            : Polynomial := Constant_Polynomial (Interval_Exact (0.0), 2 * Fixed_Chart_Degree);
      Result             : Polynomial := (Degree => 2 * Fixed_Chart_Degree, Controls => <>);
   begin
      Lemma_No_Squares (Coefficients);
      for C in Chart_Component_Index loop
         pragma Loop_Invariant (Squares.Degree = 2 * Fixed_Chart_Degree);
         pragma Loop_Invariant (Squares_Are_Enclosed (Squares, Coefficients, C - 1));
         Lemma_Add_Square (Squares, Coefficients, C);
         Squares := Sum (Squares, Multiply (Chart_Polynomial (Coefficients, C), Chart_Polynomial (Coefficients, C)));
      end loop;
      pragma Assert (Squares_Are_Enclosed (Squares, Coefficients, 3));
      for K in 0 .. 2 * Fixed_Chart_Degree loop
         Result.Controls (K) :=
           Interval_Subtract (Frame_0_Projection, Interval_Multiply (Frame_0_Projection, Squares.Controls (K)));
         Lemma_Base_Is_Sound (Frame_0_Projection, Squares, Frame, Coefficients, Direction, K);
         pragma
           Loop_Invariant
             (for all J in 0 .. K - 1 =>
                Control_Is_Sound (Result.Controls (J), Squares, Frame, Coefficients, Direction, 0, J));
         pragma Loop_Invariant (Control_Is_Sound (Result.Controls (K), Squares, Frame, Coefficients, Direction, 0, K));
      end loop;
      for C in Chart_Component_Index loop
         pragma Loop_Invariant (Result.Degree = 2 * Fixed_Chart_Degree);
         pragma
           Loop_Invariant
             (for all J in Degree_Range =>
                Control_Is_Sound (Result.Controls (J), Squares, Frame, Coefficients, Direction, C - 1, J));
         Add_Term : declare
            Frame_Projection : constant Interval := Dot_Interval (Frame (C), Direction);
            Elevated         : constant Polynomial := Multiply (Chart_Polynomial (Coefficients, C), Ones);
         begin
            for K in 0 .. 2 * Fixed_Chart_Degree loop
               --  The controls below K already hold this term. That is stated before control K changes, so that
               --  all the invariant has left to show for them is that the assignment leaves them alone.
               pragma
                 Assert
                   (for all J in 0 .. K - 1 =>
                      Control_Is_Sound (Result.Controls (J), Squares, Frame, Coefficients, Direction, C, J));
               Lemma_Term_Is_Sound
                 (Result.Controls (K), Frame_Projection, Squares, Frame, Coefficients, Direction, C, K);
               Result.Controls (K) :=
                 Interval_Add
                   (Result.Controls (K),
                    Interval_Multiply
                      (Interval_Exact (2.0), Interval_Multiply (Frame_Projection, Elevated.Controls (K))));
               pragma
                 Loop_Invariant
                   (for all J in 0 .. K - 1 =>
                      Control_Is_Sound (Result.Controls (J), Squares, Frame, Coefficients, Direction, C, J));
               pragma
                 Loop_Invariant
                   (Control_Is_Sound (Result.Controls (K), Squares, Frame, Coefficients, Direction, C, K));
               pragma
                 Loop_Invariant
                   (for all J in K + 1 .. 2 * Fixed_Chart_Degree =>
                      Control_Is_Sound (Result.Controls (J), Squares, Frame, Coefficients, Direction, C - 1, J));
            end loop;
         end Add_Term;
      end loop;
      --  Every control is now sound with all three terms in, which is all that enclosure needs.
      Lemma_Sound_Controls_Enclose (Result, Squares, Frame, Coefficients, Direction);
      return Result;
   end Numerator_Polynomial;

   function Is_Certified
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale) return Boolean
   with Refined_Post => Is_Certified'Result = Is_Nonnegative (Numerator_Polynomial (Frame, Coefficients, Direction))
   is
   begin
      return Is_Nonnegative (Numerator_Polynomial (Frame, Coefficients, Direction));
   end Is_Certified;

   procedure Lemma_Certified
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale; V : Real)
   is
      Value : constant Polynomial := Numerator_Polynomial (Frame, Coefficients, Direction);
   begin
      --  The test passed, so every control of the numerator is valid, and it encloses the real numerator. That
      --  real polynomial is therefore nonnegative at V, and its value there is the projected numerator.
      pragma Assert (Is_Nonnegative (Value));
      pragma Assert (All_Valid (Value));
      Lemma_Numerator_Encloses (Value, Frame, Coefficients, Direction);
      Lemma_Is_Nonnegative (Value, Numerator_Controls (Frame, Coefficients, Direction, 3), V);
      Lemma_Numerator_Value (Frame, Coefficients, Direction, V);
   end Lemma_Certified;

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Projection;
