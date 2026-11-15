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

with Prunt.Motion_Planner.Stereographic_Kernel.Intervals; use Prunt.Motion_Planner.Stereographic_Kernel.Intervals;
with Prunt.Motion_Planner.Stereographic_Kernel.Polynomials.Interpolation;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Structure;
with Prunt.Motion_Planner.Stereographic_Kernel.Square_Roots;

package body Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Bound
  with SPARK_Mode => On
is

   --  The proof has three parts.
   --
   --  Conclude is floating-point arithmetic. Each of its rounded operations has a lemma which bounds the
   --  magnitude of the result and relates its padded value to the exact one; Lemma_Axis_Floats chains those for
   --  one pass of the loop; and Lemma_Minimum, Lemma_Axis and Lemma_Conclusion state the three steps of
   --  Conclude over predicates which keep the real arithmetic out of it.
   --
   --  Error keeps, for every node so far, that it gave bounds within the largest ones (Is_Covered), and
   --  Lemma_Sound turns that and what Conclude returned into Is_Witnessed.
   --
   --  Lemma_Error is the mathematics of the comment which heads Sampled_Tangent. The nodal bounds of a witness
   --  bound the two polynomials at the nodes (Lemma_Denominator_Value, Lemma_Numerator_Value), interpolation
   --  bounds them on the complete curve (Lemma_Denominator_Bound, Lemma_Numerator_Bound), and the algebra of
   --  one axis (Lemma_Axis_Tangent_Error) and of the sum over the axes (Lemma_Real_Total) concludes.

   ---------------------------------------------------------------------------
   --  How far the outward padding moves a value
   ---------------------------------------------------------------------------

   Pad_Scale : constant := 2.0**(-51);
   --  Two model epsilons, as a real number.

   Smallest_Pad : constant := 2.0**(-1022);
   --  Model_Small, as a real number.

   Reach_Factor : constant := 2.0**(-50);
   Reach_Floor  : constant := 2.0**(-1021);

   function Reach (Value : Dimensionless) return Real
   is (Reach_Factor * abs (R (Value)) + Reach_Floor)
   with Ghost;
   --  More than the distance which the outward padding moves Value.

   procedure Lemma_Scale (Value : Dimensionless)
   with
     Ghost,
     Pre  => Value >= 0.0 and then Value <= Half_Last,
     Post =>
       abs (R (2.0 * Dimensionless'Model_Epsilon * Value) - Pad_Scale * R (Value))
       <= Epsilon * Pad_Scale * R (Value) + Eta;
   --  Scaling by two model epsilons is one rounded multiplication.

   procedure Lemma_Scale (Value : Dimensionless) is null;

   procedure Lemma_Inflate_Reach (Value : Dimensionless)
   with
     Ghost,
     Pre  => abs Value <= Half_Last,
     Post =>
       R (Value) - Reach (Value) <= R (Inflate_Finite (Value, False))
       and then R (Inflate_Finite (Value, False)) <= R (Value)
       and then R (Value) <= R (Inflate_Finite (Value, True))
       and then R (Inflate_Finite (Value, True)) <= R (Value) + Reach (Value);
   --  The padding is at most a little over two model epsilons of Value, or Model_Small, and adding it is one more
   --  rounded operation.

   procedure Lemma_Inflate_Reach (Value : Dimensionless) is
      Pad : constant Dimensionless := Roundoff_Padding (Value);
   begin
      Lemma_Scale (abs Value);
      pragma Assert (R (Pad) >= 0.0);
      pragma
        Assert (R (Pad) <= Pad_Scale * abs (R (Value)) + Epsilon * Pad_Scale * abs (R (Value)) + Eta + Smallest_Pad);
      Float_Lemmas.Lemma_Rounding_Error_Add (Value, Pad);
      Float_Lemmas.Lemma_Rounding_Error_Sub (Value, Pad);
      pragma Assert (Value + Pad = Inflate_Finite (Value, True));
      pragma Assert (Value - Pad = Inflate_Finite (Value, False));
      pragma Assert (R (Value + Pad) <= R (Value) + R (Pad) + Epsilon * (abs (R (Value)) + R (Pad)) + Eta);
      pragma Assert (R (Value - Pad) >= R (Value) - R (Pad) - Epsilon * (abs (R (Value)) + R (Pad)) - Eta);
      pragma Assert (R (Value + Pad) <= R (Value) + Reach (Value));
      pragma Assert (R (Value - Pad) >= R (Value) - Reach (Value));
   end Lemma_Inflate_Reach;

   procedure Lemma_Same_Real (X, Y : Dimensionless)
   with Ghost, Pre => X = Y, Post => R (X) = R (Y);
   --  Floating-point numbers which compare equal denote the same real number.

   procedure Lemma_Same_Real (X, Y : Dimensionless) is null;

   procedure Lemma_Up (Rounded : Dimensionless; Exact : Real)
   with
     Ghost,
     Pre  => abs Rounded <= Half_Last and then Is_Rounding_Of (Rounded, Exact),
     Post =>
       Exact <= R (Up (Rounded))
       and then R (Rounded) <= R (Up (Rounded))
       and then R (Up (Rounded)) <= R (Rounded) + Reach (Rounded);
   --  A rounded result padded upward lies above the exact one, and not far above the rounded one.

   procedure Lemma_Up (Rounded : Dimensionless; Exact : Real) is
   begin
      Lemma_Inflate_Encloses (Rounded, Exact);
      Lemma_Inflate_Reach (Rounded);
      Lemma_Same_Real (Up (Rounded), Inflate_Finite (Rounded, True));
   end Lemma_Up;

   procedure Lemma_Down (Rounded : Dimensionless; Exact : Real)
   with
     Ghost,
     Pre  => abs Rounded <= Half_Last and then Is_Rounding_Of (Rounded, Exact),
     Post =>
       R (Down (Rounded)) <= Exact
       and then R (Down (Rounded)) <= R (Rounded)
       and then R (Rounded) - Reach (Rounded) <= R (Down (Rounded));
   --  A rounded result padded downward lies below the exact one, and not far below the rounded one.

   procedure Lemma_Down (Rounded : Dimensionless; Exact : Real) is
   begin
      Lemma_Inflate_Encloses (Rounded, Exact);
      Lemma_Inflate_Reach (Rounded);
      Lemma_Same_Real (Down (Rounded), Inflate_Finite (Rounded, False));
   end Lemma_Down;

   ---------------------------------------------------------------------------
   --  The rounded operations of Conclude, one at a time
   ---------------------------------------------------------------------------

   --  Magnitudes are bounded by powers of two, which are the same numbers in floating point and in the reals.
   --  Magnitude_Limit is below 2**200.

   Nodal_Limit    : constant := 2.0**200;
   Scaled_Limit   : constant := 2.0**202;
   Padded_Limit   : constant := 2.0**203;
   Quotient_Limit : constant := 2.0**205;
   Error_Limit    : constant := 2.0**206;
   Square_Limit   : constant := 2.0**412;
   Term_Limit     : constant := 2.0**413;
   Largest_Before : constant := 2.0**417;
   Largest_Sum    : constant := 2.0**418;

   Smallest_Minimum_Q : constant := 0.25;

   procedure Lemma_Scaled (Largest : Dimensionless)
   with
     Ghost,
     Pre  => Largest >= 0.0 and then Largest <= Magnitude_Limit,
     Post =>
       Sample_Nodes.Lebesgue_Bound * Largest >= 0.0
       and then Sample_Nodes.Lebesgue_Bound * Largest <= Scaled_Limit
       and then Up (Sample_Nodes.Lebesgue_Bound * Largest) >= 0.0
       and then Up (Sample_Nodes.Lebesgue_Bound * Largest) <= Padded_Limit
       and then 0.0 <= Lebesgue * R (Largest)
       and then Lebesgue * R (Largest) <= R (Up (Sample_Nodes.Lebesgue_Bound * Largest));
   --  The padded product of the Lebesgue bound with a nodal bound is at least the exact product.

   procedure Lemma_Scaled (Largest : Dimensionless) is
      Product : constant Dimensionless := Sample_Nodes.Lebesgue_Bound * Largest;
      Exact   : constant Real := Lebesgue * R (Largest);
   begin
      pragma Assert (Largest <= Nodal_Limit);
      pragma Assert (R (Largest) <= Nodal_Limit);
      pragma Assert (R (Sample_Nodes.Lebesgue_Bound) = Lebesgue);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Sample_Nodes.Lebesgue_Bound, Largest);
      pragma Assert (Is_Rounding_Of (Product, Exact));
      pragma Assert (Product >= 0.0);
      pragma Assert (Exact <= Lebesgue * Nodal_Limit);
      pragma Assert (R (Product) <= Scaled_Limit);
      pragma Assert (Product <= Scaled_Limit);
      Lemma_Up (Product, Exact);
      pragma Assert (Up (Product) >= 0.0);
      pragma Assert (R (Up (Product)) <= Padded_Limit);
      pragma Assert (Up (Product) <= Padded_Limit);
   end Lemma_Scaled;

   procedure Lemma_Minimum_Q (Denominator_Slack : Dimensionless)
   with
     Ghost,
     Pre  => Denominator_Slack >= 0.0 and then Denominator_Slack <= 0.5,
     Post =>
       Down (1.0 - Denominator_Slack) >= Smallest_Minimum_Q
       and then Down (1.0 - Denominator_Slack) <= 1.0
       and then R (Down (1.0 - Denominator_Slack)) <= 1.0 - R (Denominator_Slack);
   --  The padded difference is at most the exact one, and still well above zero.

   procedure Lemma_Minimum_Q (Denominator_Slack : Dimensionless) is
      Difference : constant Dimensionless := 1.0 - Denominator_Slack;
      Exact      : constant Real := 1.0 - R (Denominator_Slack);
   begin
      Float_Lemmas.Lemma_Rounding_Error_Sub (1.0, Denominator_Slack);
      pragma Assert (Is_Rounding_Of (Difference, Exact));
      pragma Assert (Difference >= 0.5);
      pragma Assert (Difference <= 1.0);
      pragma Assert (R (Difference) >= 0.5);
      pragma Assert (R (Difference) <= 1.0);
      Lemma_Down (Difference, Exact);
      pragma Assert (Reach (Difference) <= 0.25);
      pragma Assert (R (Down (Difference)) >= 0.25);
      pragma Assert (Down (Difference) >= Smallest_Minimum_Q);
   end Lemma_Minimum_Q;

   procedure Lemma_Real_Quotient (Numerator, Divisor, Limit, Smallest : Real)
   with
     Ghost,
     Pre  => 0.0 <= Numerator and then Numerator <= Limit and then Smallest > 0.0 and then Divisor >= Smallest,
     Post =>
       0.0 <= Numerator / Divisor
       and then Numerator / Divisor <= Limit / Smallest
       and then Numerator / Divisor * Divisor = Numerator;

   procedure Lemma_Real_Quotient (Numerator, Divisor, Limit, Smallest : Real) is
      Quotient : constant Real := Numerator / Divisor;
   begin
      pragma Assert (Quotient * Divisor = Numerator);
      pragma Assert (Quotient >= 0.0);
      pragma Assert (Quotient * Smallest <= Quotient * Divisor);
      pragma Assert (Limit / Smallest * Smallest = Limit);
      if Quotient > Limit / Smallest then
         pragma Assert (Quotient * Smallest > Limit / Smallest * Smallest);
      end if;
   end Lemma_Real_Quotient;

   procedure Lemma_Axis_Error (Numerator, Minimum_Q : Dimensionless)
   with
     Ghost,
     Pre  =>
       Numerator >= 0.0
       and then Numerator <= Padded_Limit
       and then Minimum_Q >= Smallest_Minimum_Q
       and then Minimum_Q <= 1.0,
     Post =>
       Numerator / Minimum_Q >= 0.0
       and then Numerator / Minimum_Q <= Error_Limit
       and then Up (Numerator / Minimum_Q) >= 0.0
       and then Up (Numerator / Minimum_Q) <= Error_Limit
       and then R (Numerator) <= R (Up (Numerator / Minimum_Q)) * R (Minimum_Q);
   --  The padded quotient is at least the exact one, which is stated multiplied through by the divisor.

   procedure Lemma_Axis_Error (Numerator, Minimum_Q : Dimensionless) is
      Exact : constant Real := R (Numerator) / R (Minimum_Q);
   begin
      pragma Assert (R (Numerator) >= 0.0);
      pragma Assert (R (Numerator) <= Padded_Limit);
      pragma Assert (R (Minimum_Q) >= Smallest_Minimum_Q);
      Lemma_Real_Quotient (R (Numerator), R (Minimum_Q), Padded_Limit, Smallest_Minimum_Q);
      pragma Assert (Exact >= 0.0);
      pragma Assert (Exact <= Quotient_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Div (Numerator, Minimum_Q);
      declare
         Quotient : constant Dimensionless := Numerator / Minimum_Q;
      begin
         pragma Assert (Is_Rounding_Of (Quotient, Exact));
         pragma Assert (Quotient >= 0.0);
         pragma Assert (R (Quotient) <= Quotient_Limit + Epsilon * Quotient_Limit + Eta);
         pragma Assert (R (Quotient) <= Error_Limit);
         pragma Assert (Quotient <= Error_Limit);
         Lemma_Up (Quotient, Exact);
         pragma Assert (Up (Quotient) >= 0.0);
         pragma
           Assert (Reach (Quotient) <= Reach_Factor * (Quotient_Limit + Epsilon * Quotient_Limit + Eta) + Reach_Floor);
         pragma Assert (R (Up (Quotient)) <= Error_Limit);
         pragma Assert (Up (Quotient) <= Error_Limit);
         pragma Assert (Exact * R (Minimum_Q) <= R (Up (Quotient)) * R (Minimum_Q));
      end;
   end Lemma_Axis_Error;

   procedure Lemma_Real_Square (X, Limit : Real)
   with Ghost, Pre => 0.0 <= X and then X <= Limit, Post => 0.0 <= X * X and then X * X <= Limit * Limit;

   procedure Lemma_Real_Square (X, Limit : Real) is
   begin
      pragma Assert (X * X <= Limit * X);
      pragma Assert (Limit * X <= Limit * Limit);
   end Lemma_Real_Square;

   procedure Lemma_Square (Axis_Error : Dimensionless)
   with
     Ghost,
     Pre  => Axis_Error >= 0.0 and then Axis_Error <= Error_Limit,
     Post =>
       Axis_Error * Axis_Error >= 0.0
       and then Axis_Error * Axis_Error <= Term_Limit
       and then Up (Axis_Error * Axis_Error) >= 0.0
       and then Up (Axis_Error * Axis_Error) <= Term_Limit
       and then R (Axis_Error) * R (Axis_Error) <= R (Up (Axis_Error * Axis_Error));
   --  The padded square is at least the exact one.

   procedure Lemma_Square (Axis_Error : Dimensionless) is
      Exact : constant Real := R (Axis_Error) * R (Axis_Error);
   begin
      pragma Assert (R (Axis_Error) >= 0.0);
      pragma Assert (R (Axis_Error) <= Error_Limit);
      Lemma_Real_Square (R (Axis_Error), Error_Limit);
      pragma Assert (Exact <= Square_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Axis_Error, Axis_Error);
      declare
         Square : constant Dimensionless := Axis_Error * Axis_Error;
      begin
         pragma Assert (Is_Rounding_Of (Square, Exact));
         pragma Assert (Square >= 0.0);
         pragma Assert (R (Square) <= Square_Limit + Epsilon * Square_Limit + Eta);
         pragma Assert (R (Square) <= Term_Limit);
         pragma Assert (Square <= Term_Limit);
         Lemma_Up (Square, Exact);
         pragma Assert (Up (Square) >= 0.0);
         pragma Assert (Reach (Square) <= Reach_Factor * (Square_Limit + Epsilon * Square_Limit + Eta) + Reach_Floor);
         pragma Assert (R (Up (Square)) <= Term_Limit);
         pragma Assert (Up (Square) <= Term_Limit);
      end;
   end Lemma_Square;

   function Limit_Before (Axis : Axis_Name) return Dimensionless
   is (case Axis is
         when X_Axis => 2.0**414,
         when Y_Axis => 2.0**415,
         when Z_Axis => 2.0**416,
         when E_Axis => 2.0**417)
   with Ghost;
   --  Bounds Squared_Error_Sum before the term of an axis is added,

   function Limit_After (Axis : Axis_Name) return Dimensionless
   is (case Axis is
         when X_Axis => 2.0**415,
         when Y_Axis => 2.0**416,
         when Z_Axis => 2.0**417,
         when E_Axis => 2.0**418)
   with Ghost;
   --  and afterwards: each term is small enough for the sum to double at most.

   procedure Lemma_Limits (Axis : Axis_Name)
   with
     Ghost,
     Post =>
       Limit_Before (Axis) <= Limit_After (Axis)
       and then (if Axis = X_Axis then Limit_Before (Axis) >= 0.0)
       and then (if Axis /= X_Axis then Limit_After (Axis_Name'Pred (Axis)) <= Limit_Before (Axis));
   --  The limit after one axis is the limit before the next.

   procedure Lemma_Limits (Axis : Axis_Name) is null;

   procedure Lemma_Sum (Sum, Term : Dimensionless; Axis : Axis_Name)
   with
     Ghost,
     Pre  => Sum >= 0.0 and then Sum <= Limit_Before (Axis) and then Term >= 0.0 and then Term <= Term_Limit,
     Post =>
       Sum + Term >= 0.0
       and then Sum + Term <= Largest_Sum
       and then Sum + Term <= Limit_After (Axis)
       and then Up (Sum + Term) >= 0.0
       and then Up (Sum + Term) <= Limit_After (Axis)
       and then R (Sum) + R (Term) <= R (Up (Sum + Term));
   --  The padded sum is at least the exact one.

   procedure Lemma_Sum (Sum, Term : Dimensionless; Axis : Axis_Name) is
      Exact  : constant Real := R (Sum) + R (Term);
      Before : constant Real := R (Limit_Before (Axis));
      After  : constant Real := R (Limit_After (Axis));
   begin
      pragma Assert (After = 2.0 * Before);
      pragma Assert (Before >= 2.0 * Term_Limit);
      pragma Assert (Before <= Largest_Before);
      pragma Assert (R (Sum) >= 0.0 and then R (Sum) <= Before);
      pragma Assert (R (Term) >= 0.0 and then R (Term) <= Term_Limit);
      pragma Assert (Exact <= 1.5 * Before);
      Float_Lemmas.Lemma_Rounding_Error_Add (Sum, Term);
      declare
         Total : constant Dimensionless := Sum + Term;
      begin
         pragma Assert (Is_Rounding_Of (Total, Exact));
         pragma Assert (Total >= 0.0);
         pragma Assert (R (Total) <= 1.5 * Before + Epsilon * (1.5 * Before) + Eta);
         pragma Assert (R (Total) <= 1.75 * Before);
         pragma Assert (R (Total) <= After);
         pragma Assert (After <= Largest_Sum);
         pragma Assert (Total <= Largest_Sum);
         pragma Assert (Total <= Limit_After (Axis));
         Lemma_Up (Total, Exact);
         pragma Assert (Up (Total) >= 0.0);
         pragma Assert (Reach (Total) <= Reach_Factor * (1.75 * Before) + Reach_Floor);
         pragma Assert (R (Up (Total)) <= After);
         pragma Assert (Up (Total) <= Limit_After (Axis));
      end;
   end Lemma_Sum;

   ---------------------------------------------------------------------------
   --  The exact sum of squares which Conclude bounds
   ---------------------------------------------------------------------------

   function Term (Active : Axis_Flags; Largest_G : Axis_Values; Axis : Axis_Name) return Real
   is (if Active (Axis) then Lebesgue * R (Largest_G (Axis)) * (Lebesgue * R (Largest_G (Axis))) else 0.0)
   with Ghost;
   --  (Λ·G)² on an active axis.

   function Sum_Through (Active : Axis_Flags; Largest_G : Axis_Values; Axis : Axis_Name) return Real
   is ((if Axis = X_Axis then 0.0 else Sum_Through (Active, Largest_G, Axis_Name'Pred (Axis)))
       + Term (Active, Largest_G, Axis))
   with Ghost, Subprogram_Variant => (Decreases => Axis_Name'Pos (Axis));
   --  The sum of those over the axes up to Axis.

   procedure Lemma_Sum_Through (Active : Axis_Flags; Largest_G : Axis_Values; Bound : Real)
   with
     Ghost,
     Pre  => Sum_Through (Active, Largest_G, E_Axis) <= Bound,
     Post =>
       (if Active (X_Axis) then Lebesgue * R (Largest_G (X_Axis)) * (Lebesgue * R (Largest_G (X_Axis))) else 0.0)
       + (if Active (Y_Axis) then Lebesgue * R (Largest_G (Y_Axis)) * (Lebesgue * R (Largest_G (Y_Axis))) else 0.0)
       + (if Active (Z_Axis) then Lebesgue * R (Largest_G (Z_Axis)) * (Lebesgue * R (Largest_G (Z_Axis))) else 0.0)
       + (if Active (E_Axis) then Lebesgue * R (Largest_G (E_Axis)) * (Lebesgue * R (Largest_G (E_Axis))) else 0.0)
       <= Bound;
   --  The sum over all four axes, written out as Is_Combined has it.

   procedure Lemma_Sum_Through (Active : Axis_Flags; Largest_G : Axis_Values; Bound : Real) is
      X : constant Real := Term (Active, Largest_G, X_Axis);
      Y : constant Real := Term (Active, Largest_G, Y_Axis);
      Z : constant Real := Term (Active, Largest_G, Z_Axis);
      E : constant Real := Term (Active, Largest_G, E_Axis);
   begin
      pragma Assert (Sum_Through (Active, Largest_G, X_Axis) = X);
      pragma Assert (Sum_Through (Active, Largest_G, Y_Axis) = X + Y);
      pragma Assert (Sum_Through (Active, Largest_G, Z_Axis) = X + Y + Z);
      pragma Assert (Sum_Through (Active, Largest_G, E_Axis) = X + Y + Z + E);
   end Lemma_Sum_Through;

   procedure Lemma_Accumulate (Before, Scaled, Axis_Error, Square, Old_Sum, New_Sum, Minimum_Q : Real)
   with
     Ghost,
     Pre  =>
       Before <= Old_Sum * (Minimum_Q * Minimum_Q)
       and then 0.0 <= Scaled
       and then Scaled <= Axis_Error * Minimum_Q
       and then Axis_Error * Axis_Error <= Square
       and then Old_Sum + Square <= New_Sum
       and then Minimum_Q > 0.0,
     Post => Before + Scaled * Scaled <= New_Sum * (Minimum_Q * Minimum_Q);
   --  One more axis: (Λ·G)² ≤ (Axis_Error·Minimum_Q)² ≤ Square·Minimum_Q².

   procedure Lemma_Accumulate (Before, Scaled, Axis_Error, Square, Old_Sum, New_Sum, Minimum_Q : Real) is
      Product : constant Real := Axis_Error * Minimum_Q;
      Q2      : constant Real := Minimum_Q * Minimum_Q;
   begin
      pragma Assert (Q2 >= 0.0);
      pragma Assert (Scaled * Scaled <= Product * Scaled);
      pragma Assert (Product * Scaled <= Product * Product);
      pragma Assert (Product * Product = Axis_Error * Axis_Error * Q2);
      pragma Assert (Axis_Error * Axis_Error * Q2 <= Square * Q2);
      pragma Assert (Scaled * Scaled <= Square * Q2);
      pragma Assert ((Old_Sum + Square) * Q2 = Old_Sum * Q2 + Square * Q2);
      pragma Assert ((Old_Sum + Square) * Q2 <= New_Sum * Q2);
   end Lemma_Accumulate;

   procedure Lemma_Combine (Total, Sum, Error_Bound, Minimum_Q, Slack : Real)
   with
     Ghost,
     Pre  =>
       Total <= Sum * (Minimum_Q * Minimum_Q)
       and then Sum <= Error_Bound * Error_Bound
       and then 0.0 < Minimum_Q
       and then Minimum_Q <= 1.0 - Slack,
     Post => Total <= Error_Bound * Error_Bound * ((1.0 - Slack) * (1.0 - Slack));
   --  The sum of squares is at most (Error_Bound·Minimum_Q)², and Minimum_Q is at most 1 - Λ·E.

   procedure Lemma_Combine (Total, Sum, Error_Bound, Minimum_Q, Slack : Real) is
      Q2 : constant Real := Minimum_Q * Minimum_Q;
      M  : constant Real := 1.0 - Slack;
      E2 : constant Real := Error_Bound * Error_Bound;
   begin
      pragma Assert (Q2 >= 0.0);
      pragma Assert (E2 >= 0.0);
      pragma Assert (Sum * Q2 <= E2 * Q2);
      pragma Assert (Minimum_Q * Minimum_Q <= M * Minimum_Q);
      pragma Assert (M * Minimum_Q <= M * M);
      pragma Assert (Q2 <= M * M);
      pragma Assert (E2 * Q2 <= E2 * (M * M));
   end Lemma_Combine;

   ---------------------------------------------------------------------------
   --  The rounded operations of Conclude for one axis, together
   ---------------------------------------------------------------------------

   procedure Lemma_Axis_Floats (Largest_G, Minimum_Q, Sum : Dimensionless; Axis : Axis_Name)
   with
     Ghost,
     Pre  =>
       Largest_G >= 0.0
       and then Largest_G <= Magnitude_Limit
       and then Minimum_Q >= Smallest_Minimum_Q
       and then Minimum_Q <= 1.0
       and then Sum >= 0.0
       and then Sum <= Limit_Before (Axis),
     Post =>
       Sample_Nodes.Lebesgue_Bound * Largest_G >= 0.0
       and then Sample_Nodes.Lebesgue_Bound * Largest_G <= Scaled_Limit
       and then Up (Sample_Nodes.Lebesgue_Bound * Largest_G) >= 0.0
       and then Up (Sample_Nodes.Lebesgue_Bound * Largest_G) <= Padded_Limit
       and then Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q >= 0.0
       and then Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q <= Error_Limit
       and then Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q) >= 0.0
       and then Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q) <= Error_Limit
       and then
         Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
         * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
         >= 0.0
       and then
         Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
         * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
         <= Term_Limit
       and then
         Up
           (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
            * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q))
         >= 0.0
       and then
         Up
           (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
            * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q))
         <= Term_Limit
       and then Sum >= 0.0
       and then Sum <= Largest_Before
       and then
         Sum
         + Up
             (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
              * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q))
         >= 0.0
       and then
         Sum
         + Up
             (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
              * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q))
         <= Largest_Sum
       and then
         Up
           (Sum
            + Up
                (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
                 * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)))
         >= 0.0
       and then
         Up
           (Sum
            + Up
                (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
                 * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)))
         <= Limit_After (Axis)
       and then
         R (Sum) * (R (Minimum_Q) * R (Minimum_Q)) + Lebesgue * R (Largest_G) * (Lebesgue * R (Largest_G))
         <= R
              (Up
                 (Sum
                  + Up
                      (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q)
                       * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q))))
            * (R (Minimum_Q) * R (Minimum_Q));
   --  No operation of one pass of Conclude's loop overflows, and with S the sum before, S' the sum after and q
   --  the divisor, S·q² + (Λ·G)² ≤ S'·q².

   procedure Lemma_Axis_Floats (Largest_G, Minimum_Q, Sum : Dimensionless; Axis : Axis_Name) is
   begin
      Lemma_Scaled (Largest_G);
      Lemma_Axis_Error (Up (Sample_Nodes.Lebesgue_Bound * Largest_G), Minimum_Q);
      declare
         Axis_Error : constant Dimensionless := Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G) / Minimum_Q);
      begin
         Lemma_Square (Axis_Error);
         Lemma_Sum (Sum, Up (Axis_Error * Axis_Error), Axis);
         pragma Assert (Limit_Before (Axis) <= Largest_Before);
         pragma Assert (Sum <= Largest_Before);
         pragma Assert (R (Minimum_Q) > 0.0);
         Lemma_Accumulate
           (Before     => R (Sum) * (R (Minimum_Q) * R (Minimum_Q)),
            Scaled     => Lebesgue * R (Largest_G),
            Axis_Error => R (Axis_Error),
            Square     => R (Up (Axis_Error * Axis_Error)),
            Old_Sum    => R (Sum),
            New_Sum    => R (Up (Sum + Up (Axis_Error * Axis_Error))),
            Minimum_Q  => R (Minimum_Q));
      end;
   end Lemma_Axis_Floats;

   ---------------------------------------------------------------------------
   --  The bound from the largest nodal bounds
   ---------------------------------------------------------------------------

   --  The predicates below are ordinary functions with a postcondition, rather than expression functions, to
   --  have the tool keep each as one term. Conclude then states no real arithmetic of its own, only these terms
   --  and the magnitudes of its floating-point numbers, and one lemma for each of its steps does the rest: the
   --  provers are slow with the floating-point operations of the whole function at once, and each further fact
   --  about them costs time. Conclude also has several return statements, and the postcondition of such a
   --  function is proved for all of them at once: the provers must find which statement returned before they can
   --  use what was known there, which they do for one term and do not for the nonlinear inequality of
   --  Is_Combined.

   function Is_Minimum (Largest_E, Minimum_Q : Dimensionless) return Boolean
   with
     Ghost,
     Post =>
       Is_Minimum'Result
       = (Minimum_Q >= Smallest_Minimum_Q
          and then Minimum_Q <= 1.0
          and then R (Minimum_Q) <= 1.0 - Lebesgue * R (Largest_E));
   --  Whether Minimum_Q is a positive number, at most 1 - Λ·E.

   function Is_Minimum (Largest_E, Minimum_Q : Dimensionless) return Boolean is
   begin
      return
        Minimum_Q >= Smallest_Minimum_Q
        and then Minimum_Q <= 1.0
        and then R (Minimum_Q) <= 1.0 - Lebesgue * R (Largest_E);
   end Is_Minimum;

   function Is_Partial
     (Active : Axis_Flags; Largest_G : Axis_Values; Axis : Axis_Name; Minimum_Q, Sum : Dimensionless) return Boolean
   with
     Ghost,
     Post =>
       Is_Partial'Result
       = (Sum >= 0.0
          and then Sum <= Limit_After (Axis)
          and then Sum_Through (Active, Largest_G, Axis) <= R (Sum) * (R (Minimum_Q) * R (Minimum_Q)));
   --  Whether Sum bounds the squares of the axes up to Axis: Σ (Λ·G)² ≤ Sum·Minimum_Q² over the active ones.

   function Is_Partial
     (Active : Axis_Flags; Largest_G : Axis_Values; Axis : Axis_Name; Minimum_Q, Sum : Dimensionless) return Boolean is
   begin
      return
        Sum >= 0.0
        and then Sum <= Limit_After (Axis)
        and then Sum_Through (Active, Largest_G, Axis) <= R (Sum) * (R (Minimum_Q) * R (Minimum_Q));
   end Is_Partial;

   function Is_Concluded
     (Active : Axis_Flags; Largest_E : Dimensionless; Largest_G : Axis_Values; Value : Result) return Boolean
   with
     Ghost,
     Post =>
       Is_Concluded'Result
       = (Is_Combined (Active, Value)
          and then Value.Largest_E >= Largest_E
          and then (for all Axis in Axis_Name => Value.Largest_G (Axis) >= Largest_G (Axis)));
   --  Whether the bound of Value follows from nodal bounds which are not below the given ones.

   function Is_Concluded
     (Active : Axis_Flags; Largest_E : Dimensionless; Largest_G : Axis_Values; Value : Result) return Boolean is
   begin
      return
        Is_Combined (Active, Value)
        and then Value.Largest_E >= Largest_E
        and then (for all Axis in Axis_Name => Value.Largest_G (Axis) >= Largest_G (Axis));
   end Is_Concluded;

   procedure Lemma_Is_Partial
     (Active : Axis_Flags; Largest_G : Axis_Values; Axis : Axis_Name; Minimum_Q, Sum : Dimensionless)
   with
     Ghost,
     Pre  =>
       Sum >= 0.0
       and then Sum <= Limit_After (Axis)
       and then Sum_Through (Active, Largest_G, Axis) <= R (Sum) * (R (Minimum_Q) * R (Minimum_Q)),
     Post => Is_Partial (Active, Largest_G, Axis, Minimum_Q, Sum);

   procedure Lemma_Is_Partial
     (Active : Axis_Flags; Largest_G : Axis_Values; Axis : Axis_Name; Minimum_Q, Sum : Dimensionless)
   is null;

   procedure Lemma_Minimum (Largest_E : Dimensionless)
   with
     Ghost,
     Pre  => Largest_E >= 0.0 and then Largest_E <= 1.0 and then Up (Sample_Nodes.Lebesgue_Bound * Largest_E) <= 0.5,
     Post =>
       1.0 - Up (Sample_Nodes.Lebesgue_Bound * Largest_E) >= 0.0
       and then 1.0 - Up (Sample_Nodes.Lebesgue_Bound * Largest_E) <= 1.0
       and then Is_Minimum (Largest_E, Down (1.0 - Up (Sample_Nodes.Lebesgue_Bound * Largest_E)));
   --  The first step of Conclude: Minimum_Q ≤ 1 - Denominator_Slack ≤ 1 - Λ·E.

   procedure Lemma_Minimum (Largest_E : Dimensionless) is
   begin
      Lemma_Scaled (Largest_E);
      Lemma_Minimum_Q (Up (Sample_Nodes.Lebesgue_Bound * Largest_E));
      pragma Assert (R (Down (1.0 - Up (Sample_Nodes.Lebesgue_Bound * Largest_E))) <= 1.0 - Lebesgue * R (Largest_E));
      pragma Assert (Is_Minimum (Largest_E, Down (1.0 - Up (Sample_Nodes.Lebesgue_Bound * Largest_E))));
   end Lemma_Minimum;

   procedure Lemma_Axis
     (Active    : Axis_Flags;
      Largest_E : Dimensionless;
      Largest_G : Axis_Values;
      Axis      : Axis_Name;
      Minimum_Q : Dimensionless;
      Sum       : Dimensionless)
   with
     Ghost,
     Pre  =>
       Largest_G (Axis) >= 0.0
       and then Largest_G (Axis) <= Magnitude_Limit
       and then Is_Minimum (Largest_E, Minimum_Q)
       and then
         (if Axis = X_Axis then Sum = 0.0 else Is_Partial (Active, Largest_G, Axis_Name'Pred (Axis), Minimum_Q, Sum)),
     Post =>
       Minimum_Q >= Smallest_Minimum_Q
       and then
         (if Active (Axis)
          then
            Sample_Nodes.Lebesgue_Bound * Largest_G (Axis) >= 0.0
            and then Sample_Nodes.Lebesgue_Bound * Largest_G (Axis) <= Scaled_Limit
            and then Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) >= 0.0
            and then Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) <= Padded_Limit
            and then Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q >= 0.0
            and then Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q <= Error_Limit
            and then Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q) >= 0.0
            and then Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q) <= Error_Limit
            and then
              Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
              * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
              >= 0.0
            and then
              Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
              * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
              <= Term_Limit
            and then
              Up
                (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
                 * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q))
              >= 0.0
            and then
              Up
                (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
                 * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q))
              <= Term_Limit
            and then Sum >= 0.0
            and then Sum <= Largest_Before
            and then
              Sum
              + Up
                  (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
                   * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q))
              >= 0.0
            and then
              Sum
              + Up
                  (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
                   * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q))
              <= Largest_Sum
            and then
              Is_Partial
                (Active,
                 Largest_G,
                 Axis,
                 Minimum_Q,
                 Up
                   (Sum
                    + Up
                        (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
                         * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q))))
          else Is_Partial (Active, Largest_G, Axis, Minimum_Q, Sum));
   --  One pass of Conclude's loop: nothing overflows, and the sum which it leaves bounds the squares of the axes
   --  up to this one.

   procedure Lemma_Axis
     (Active    : Axis_Flags;
      Largest_E : Dimensionless;
      Largest_G : Axis_Values;
      Axis      : Axis_Name;
      Minimum_Q : Dimensionless;
      Sum       : Dimensionless)
   is
      Before : constant Real :=
        (if Axis = X_Axis then 0.0 else Sum_Through (Active, Largest_G, Axis_Name'Pred (Axis)));
      Q2     : constant Real := R (Minimum_Q) * R (Minimum_Q);
   begin
      pragma Assert (Minimum_Q >= Smallest_Minimum_Q and then Minimum_Q <= 1.0);
      Lemma_Limits (Axis);
      pragma Assert (Sum >= 0.0);
      pragma Assert (Sum <= Limit_Before (Axis));
      pragma Assert (Before <= R (Sum) * Q2);
      pragma Assert (Sum_Through (Active, Largest_G, Axis) = Before + Term (Active, Largest_G, Axis));
      if Active (Axis) then
         Lemma_Axis_Floats (Largest_G (Axis), Minimum_Q, Sum, Axis);
         pragma
           Assert
             (Term (Active, Largest_G, Axis) = Lebesgue * R (Largest_G (Axis)) * (Lebesgue * R (Largest_G (Axis))));
         pragma
           Assert
             (Sum_Through (Active, Largest_G, Axis)
              <= R (Sum) * Q2 + Lebesgue * R (Largest_G (Axis)) * (Lebesgue * R (Largest_G (Axis))));
         pragma
           Assert
             (Sum_Through (Active, Largest_G, Axis)
              <= R
                   (Up
                      (Sum
                       + Up
                           (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
                            * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q))))
                 * (R (Minimum_Q) * R (Minimum_Q)));
         Lemma_Is_Partial
           (Active,
            Largest_G,
            Axis,
            Minimum_Q,
            Up
              (Sum
               + Up
                   (Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q)
                    * Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q))));
      else
         pragma Assert (Term (Active, Largest_G, Axis) = 0.0);
         pragma Assert (Sum_Through (Active, Largest_G, Axis) <= R (Sum) * (R (Minimum_Q) * R (Minimum_Q)));
         pragma Assert (Sum <= Limit_After (Axis));
         Lemma_Is_Partial (Active, Largest_G, Axis, Minimum_Q, Sum);
      end if;
   end Lemma_Axis;

   procedure Lemma_Conclusion
     (Active      : Axis_Flags;
      Largest_E   : Dimensionless;
      Largest_G   : Axis_Values;
      Minimum_Q   : Dimensionless;
      Sum         : Dimensionless;
      Error_Bound : Dimensionless)
   with
     Ghost,
     Pre  =>
       Largest_E >= 0.0
       and then (for all Axis in Axis_Name => Largest_G (Axis) >= 0.0)
       and then Is_Minimum (Largest_E, Minimum_Q)
       and then Is_Partial (Active, Largest_G, E_Axis, Minimum_Q, Sum)
       and then Error_Bound >= 0.0
       and then Error_Bound < Dimensionless'Last
       and then R (Error_Bound) * R (Error_Bound) >= R (Sum),
     Post =>
       Is_Concluded
         (Active,
          Largest_E,
          Largest_G,
          (Outcome => Certified, Error_Bound => Error_Bound, Largest_E => Largest_E, Largest_G => Largest_G));
   --  The last step of Conclude: the sum of squares over the active axes is at most Sum·Minimum_Q², Sum is at
   --  most Error_Bound², and Minimum_Q is at most 1 - Λ·E.

   procedure Lemma_Conclusion
     (Active      : Axis_Flags;
      Largest_E   : Dimensionless;
      Largest_G   : Axis_Values;
      Minimum_Q   : Dimensionless;
      Sum         : Dimensionless;
      Error_Bound : Dimensionless)
   is
      Value : constant Result :=
        (Outcome => Certified, Error_Bound => Error_Bound, Largest_E => Largest_E, Largest_G => Largest_G);
   begin
      Lemma_Same_Real (Value.Error_Bound, Error_Bound);
      Lemma_Same_Real (Value.Largest_E, Largest_E);
      pragma Assert (Minimum_Q >= Smallest_Minimum_Q);
      pragma Assert (R (Minimum_Q) > 0.0);
      pragma Assert (R (Minimum_Q) <= 1.0 - Lebesgue * R (Largest_E));
      pragma Assert (Sum_Through (Active, Largest_G, E_Axis) <= R (Sum) * (R (Minimum_Q) * R (Minimum_Q)));
      Lemma_Combine
        (Total       => Sum_Through (Active, Largest_G, E_Axis),
         Sum         => R (Sum),
         Error_Bound => R (Error_Bound),
         Minimum_Q   => R (Minimum_Q),
         Slack       => Lebesgue * R (Largest_E));
      pragma Assert (Lebesgue * R (Value.Largest_E) < 1.0);
      Lemma_Sum_Through
        (Active,
         Value.Largest_G,
         R (Value.Error_Bound)
         * R (Value.Error_Bound)
         * ((1.0 - Lebesgue * R (Value.Largest_E)) * (1.0 - Lebesgue * R (Value.Largest_E))));
      pragma Assert (Is_Combined (Active, Value));
      pragma Assert (Value.Largest_E >= Largest_E);
      pragma Assert (for all Axis in Axis_Name => Value.Largest_G (Axis) >= Largest_G (Axis));
      pragma Assert (Is_Concluded (Active, Largest_E, Largest_G, Value));
   end Lemma_Conclusion;

   function Conclude (Active : Axis_Flags; Largest_E : Dimensionless; Largest_G : Axis_Values) return Result
   with
     Pre  =>
       Largest_E >= 0.0
       and then Largest_E <= 1.0
       and then (for all Axis in Axis_Name => Largest_G (Axis) >= 0.0 and then Largest_G (Axis) <= Magnitude_Limit),
     Post =>
       (if Conclude'Result.Outcome = Certified then Is_Concluded (Active, Largest_E, Largest_G, Conclude'Result));
   --  The bound which follows from the largest nodal bounds.

   function Conclude (Active : Axis_Flags; Largest_E : Dimensionless; Largest_G : Axis_Values) return Result is
      Denominator_Slack : constant Dimensionless := Up (Sample_Nodes.Lebesgue_Bound * Largest_E);
      Minimum_Q         : Dimensionless;
      Squared_Error_Sum : Dimensionless := 0.0;
   begin
      if not (Denominator_Slack <= 0.5) then
         return (Outcome => Not_Certified, others => <>);
      end if;
      Lemma_Minimum (Largest_E);
      Minimum_Q := Down (1.0 - Denominator_Slack);
      for Axis in Axis_Name loop
         --  The invariant is what Lemma_Axis requires. It comes first in the loop to have the checks of one
         --  pass proved from that pass alone.

         pragma
           Loop_Invariant
             (if Axis = X_Axis
              then Squared_Error_Sum = 0.0
              else Is_Partial (Active, Largest_G, Axis_Name'Pred (Axis), Minimum_Q, Squared_Error_Sum));
         Lemma_Axis (Active, Largest_E, Largest_G, Axis, Minimum_Q, Squared_Error_Sum);
         if Active (Axis) then
            declare
               Axis_Error : constant Dimensionless :=
                 Up (Up (Sample_Nodes.Lebesgue_Bound * Largest_G (Axis)) / Minimum_Q);
            begin
               Squared_Error_Sum := Up (Squared_Error_Sum + Up (Axis_Error * Axis_Error));
            end;
         end if;
         pragma Assert (Is_Partial (Active, Largest_G, Axis, Minimum_Q, Squared_Error_Sum));
      end loop;
      pragma Assert (Is_Partial (Active, Largest_G, E_Axis, Minimum_Q, Squared_Error_Sum));
      declare
         Error_Bound : constant Dimensionless := Square_Roots.Certified_Upper_Square_Root (Squared_Error_Sum);
      begin
         if Error_Bound = Dimensionless'Last or else not Is_Finite (Error_Bound) then
            return (Outcome => Unsafe, others => <>);
         else
            Lemma_Conclusion (Active, Largest_E, Largest_G, Minimum_Q, Squared_Error_Sum, Error_Bound);
            return (Outcome => Certified, Error_Bound => Error_Bound, Largest_E => Largest_E, Largest_G => Largest_G);
         end if;
      end;
   end Conclude;

   ---------------------------------------------------------------------------
   --  The largest nodal bounds
   ---------------------------------------------------------------------------

   function Is_Covered
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Index             : Sample_Nodes.Sample_Index;
      Largest_E         : Dimensionless;
      Largest_G         : Axis_Values) return Boolean
   with
     Ghost,
     Pre  => Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Outcome = Certified,
     Post =>
       Is_Covered'Result
       = (Nodes.Bounds_At_Node
            (Frame,
             Coefficients,
             Warp,
             Cache,
             Denominator_Scale,
             Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
             Sample_Nodes.Table.Samples (Index))
            .Good
          and then
            Nodes.Bounds_At_Node
              (Frame,
               Coefficients,
               Warp,
               Cache,
               Denominator_Scale,
               Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
               Sample_Nodes.Table.Samples (Index))
              .Nodal_E
            <= Largest_E
          and then
            (for all Axis in Axis_Name =>
               (if Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active (Axis)
                then
                  Nodes.Bounds_At_Node
                    (Frame,
                     Coefficients,
                     Warp,
                     Cache,
                     Denominator_Scale,
                     Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
                     Sample_Nodes.Table.Samples (Index))
                    .Nodal_G (Axis)
                  <= Largest_G (Axis))));
   --  Whether the sample node Index gave nodal bounds, none above the given ones. This is what Is_Witnessed asks
   --  of every node. It is an ordinary function, rather than an expression function, to have the tool keep it
   --  as one term under the quantifiers of the loop invariants, where the provers need that term to instantiate.

   function Is_Covered
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Index             : Sample_Nodes.Sample_Index;
      Largest_E         : Dimensionless;
      Largest_G         : Axis_Values) return Boolean is
   begin
      return
        (Nodes.Bounds_At_Node
           (Frame,
            Coefficients,
            Warp,
            Cache,
            Denominator_Scale,
            Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
            Sample_Nodes.Table.Samples (Index))
           .Good
         and then
           Nodes.Bounds_At_Node
             (Frame,
              Coefficients,
              Warp,
              Cache,
              Denominator_Scale,
              Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
              Sample_Nodes.Table.Samples (Index))
             .Nodal_E
           <= Largest_E
         and then
           (for all Axis in Axis_Name =>
              (if Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active (Axis)
               then
                 Nodes.Bounds_At_Node
                   (Frame,
                    Coefficients,
                    Warp,
                    Cache,
                    Denominator_Scale,
                    Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
                    Sample_Nodes.Table.Samples (Index))
                   .Nodal_G (Axis)
                 <= Largest_G (Axis))));
   end Is_Covered;

   procedure Lemma_Still_Covered
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Count             : Natural;
      Old_E, New_E      : Dimensionless;
      Old_G, New_G      : Axis_Values)
   with
     Ghost,
     Pre  =>
       Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Outcome = Certified
       and then Count <= Sample_Nodes.Sample_Count
       and then
         (for all I in 1 .. Count => Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, I, Old_E, Old_G))
       and then Old_E <= New_E
       and then (for all Axis in Axis_Name => Old_G (Axis) <= New_G (Axis)),
     Post =>
       (for all I in 1 .. Count => Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, I, New_E, New_G));
   --  Nodes which were covered remain so when the largest bounds grow.

   procedure Lemma_Still_Covered
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Count             : Natural;
      Old_E, New_E      : Dimensionless;
      Old_G, New_G      : Axis_Values) is
   begin
      for I in 1 .. Count loop
         pragma Assert (Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, I, Old_E, Old_G));
         pragma Assert (Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, I, New_E, New_G));
         pragma
           Loop_Invariant
             (for all J in 1 .. I =>
                Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, J, New_E, New_G));
      end loop;
   end Lemma_Still_Covered;

   procedure Lemma_Uncovered
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Index             : Sample_Nodes.Sample_Index;
      Largest_E         : Dimensionless;
      Largest_G         : Axis_Values)
   with
     Ghost,
     Pre  =>
       Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Outcome = Certified
       and then Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, Index, Largest_E, Largest_G),
     Post =>
       Nodes.Bounds_At_Node
         (Frame,
          Coefficients,
          Warp,
          Cache,
          Denominator_Scale,
          Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
          Sample_Nodes.Table.Samples (Index))
         .Good
       and then
         Nodes.Bounds_At_Node
           (Frame,
            Coefficients,
            Warp,
            Cache,
            Denominator_Scale,
            Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
            Sample_Nodes.Table.Samples (Index))
           .Nodal_E
         <= Largest_E
       and then
         (for all Axis in Axis_Name =>
            (if Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active (Axis)
             then
               Nodes.Bounds_At_Node
                 (Frame,
                  Coefficients,
                  Warp,
                  Cache,
                  Denominator_Scale,
                  Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
                  Sample_Nodes.Table.Samples (Index))
                 .Nodal_G (Axis)
               <= Largest_G (Axis)));
   --  Is_Covered, written out.

   procedure Lemma_Uncovered
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Index             : Sample_Nodes.Sample_Index;
      Largest_E         : Dimensionless;
      Largest_G         : Axis_Values)
   is null;

   procedure Lemma_Written_Out
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Largest_E         : Dimensionless;
      Largest_G         : Axis_Values)
   with
     Ghost,
     Pre  =>
       Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Outcome = Certified
       and then
         (for all Index in Sample_Nodes.Sample_Index =>
            Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, Index, Largest_E, Largest_G)),
     Post =>
       (for all Index in Sample_Nodes.Sample_Index =>
          Nodes.Bounds_At_Node
            (Frame,
             Coefficients,
             Warp,
             Cache,
             Denominator_Scale,
             Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
             Sample_Nodes.Table.Samples (Index))
            .Good
          and then
            Nodes.Bounds_At_Node
              (Frame,
               Coefficients,
               Warp,
               Cache,
               Denominator_Scale,
               Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
               Sample_Nodes.Table.Samples (Index))
              .Nodal_E
            <= Largest_E
          and then
            (for all Axis in Axis_Name =>
               (if Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active (Axis)
                then
                  Nodes.Bounds_At_Node
                    (Frame,
                     Coefficients,
                     Warp,
                     Cache,
                     Denominator_Scale,
                     Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
                     Sample_Nodes.Table.Samples (Index))
                    .Nodal_G (Axis)
                  <= Largest_G (Axis))));
   --  The covering of every node, written out as Is_Witnessed has it.

   procedure Lemma_Written_Out
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Largest_E         : Dimensionless;
      Largest_G         : Axis_Values) is
   begin
      for Index in Sample_Nodes.Sample_Index loop
         Lemma_Uncovered (Frame, Coefficients, Warp, Cache, Denominator_Scale, Index, Largest_E, Largest_G);
         pragma
           Loop_Invariant
             (for all I in 1 .. Index =>
                Nodes.Bounds_At_Node
                  (Frame,
                   Coefficients,
                   Warp,
                   Cache,
                   Denominator_Scale,
                   Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
                   Sample_Nodes.Table.Samples (I))
                  .Good
                and then
                  Nodes.Bounds_At_Node
                    (Frame,
                     Coefficients,
                     Warp,
                     Cache,
                     Denominator_Scale,
                     Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
                     Sample_Nodes.Table.Samples (I))
                    .Nodal_E
                  <= Largest_E
                and then
                  (for all Axis in Axis_Name =>
                     (if Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active (Axis)
                      then
                        Nodes.Bounds_At_Node
                          (Frame,
                           Coefficients,
                           Warp,
                           Cache,
                           Denominator_Scale,
                           Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
                           Sample_Nodes.Table.Samples (I))
                          .Nodal_G (Axis)
                        <= Largest_G (Axis))));
      end loop;
   end Lemma_Written_Out;

   function Is_Sound
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result) return Boolean
   with Ghost, Post => Is_Sound'Result = Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value);
   --  Is_Witnessed, as one term: see the predicates of Conclude. Error has three return statements.

   function Is_Sound
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result) return Boolean is
   begin
      return Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value);
   end Is_Sound;

   procedure Lemma_Sound
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Largest_E         : Dimensionless;
      Largest_G         : Axis_Values;
      Value             : Result)
   with
     Ghost,
     Pre  =>
       Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Outcome = Certified
       and then
         (for all Index in Sample_Nodes.Sample_Index =>
            Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, Index, Largest_E, Largest_G))
       and then
         (if Value.Outcome = Certified
          then
            Is_Concluded
              (Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active,
               Largest_E,
               Largest_G,
               Value)),
     Post => (if Value.Outcome = Certified then Is_Sound (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value));
   --  A result concluded from largest nodal bounds which cover every node is a certificate.

   procedure Lemma_Sound
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Largest_E         : Dimensionless;
      Largest_G         : Axis_Values;
      Value             : Result) is
   begin
      if Value.Outcome = Certified then
         pragma
           Assert (Is_Combined (Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active, Value));
         pragma Assert (Largest_E <= Value.Largest_E);
         pragma Assert (for all Axis in Axis_Name => Largest_G (Axis) <= Value.Largest_G (Axis));
         Lemma_Still_Covered
           (Frame,
            Coefficients,
            Warp,
            Cache,
            Denominator_Scale,
            Sample_Nodes.Sample_Count,
            Largest_E,
            Value.Largest_E,
            Largest_G,
            Value.Largest_G);

         Lemma_Written_Out (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value.Largest_E, Value.Largest_G);
         pragma Assert (Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value));
         pragma Assert (Is_Sound (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value));
      end if;
   end Lemma_Sound;

   function Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless) return Result
   with
     Refined_Post =>
       (if Error'Result.Outcome = Certified
        then Is_Sound (Frame, Coefficients, Warp, Cache, Denominator_Scale, Error'Result))
   is
      Prepared  : constant Preparation := Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale);
      Largest_E : Dimensionless := 0.0;
      Largest_G : Axis_Values := [others => 0.0];
   begin
      if Prepared.Outcome /= Certified then
         return (Outcome => Prepared.Outcome, others => <>);
      end if;

      --  The nodes are visited by index, which the invariant needs to say which of them have been covered.

      for Index in Sample_Nodes.Sample_Index loop
         pragma Loop_Invariant (Largest_E >= 0.0 and then Largest_E <= 1.0);
         pragma
           Loop_Invariant
             (for all Axis in Axis_Name => Largest_G (Axis) >= 0.0 and then Largest_G (Axis) <= Magnitude_Limit);
         pragma
           Loop_Invariant
             (for all I in 1 .. Index - 1 =>
                Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, I, Largest_E, Largest_G));
         declare
            Node : constant Node_Bounds :=
              Nodes.Bounds_At_Node
                (Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared, Sample_Nodes.Table.Samples (Index));

            Old_E : constant Dimensionless := Largest_E
            with Ghost;
            Old_G : constant Axis_Values := Largest_G
            with Ghost;
         begin
            if not Node.Good then
               return (Outcome => Not_Certified, others => <>);
            end if;
            Largest_E := Dimensionless'Max (Largest_E, Node.Nodal_E);
            for Axis in Axis_Name loop
               if Prepared.Active (Axis) then
                  Largest_G (Axis) := Dimensionless'Max (Largest_G (Axis), Node.Nodal_G (Axis));
               end if;
               pragma
                 Loop_Invariant
                   (for all Other in Axis_Name =>
                      Largest_G (Other) >= Old_G (Other)
                      and then Largest_G (Other) >= 0.0
                      and then Largest_G (Other) <= Magnitude_Limit);
               pragma
                 Loop_Invariant
                   (for all Other in X_Axis .. Axis =>
                      (if Prepared.Active (Other) then Largest_G (Other) >= Node.Nodal_G (Other)));
            end loop;
            Lemma_Still_Covered
              (Frame, Coefficients, Warp, Cache, Denominator_Scale, Index - 1, Old_E, Largest_E, Old_G, Largest_G);
            pragma
              Assert (Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, Index, Largest_E, Largest_G));
            pragma
              Assert
                (for all I in 1 .. Index =>
                   Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, I, Largest_E, Largest_G));
         end;
      end loop;
      pragma
        Assert
          (for all I in Sample_Nodes.Sample_Index =>
             Is_Covered (Frame, Coefficients, Warp, Cache, Denominator_Scale, I, Largest_E, Largest_G));
      Lemma_Sound
        (Frame,
         Coefficients,
         Warp,
         Cache,
         Denominator_Scale,
         Largest_E,
         Largest_G,
         Conclude (Prepared.Active, Largest_E, Largest_G));
      return Conclude (Prepared.Active, Largest_E, Largest_G);
   end Error;

   ---------------------------------------------------------------------------
   --  The theorem: what a certificate guarantees
   ---------------------------------------------------------------------------

   procedure Lemma_Witness_Is_Regular
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result)
   with
     Ghost,
     Pre  => Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value),
     Post =>
       Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Outcome = Certified
       and then
         Poles.Is_Prepared
           (Frame,
            Coefficients,
            Warp,
            Cache,
            Denominator_Scale,
            Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale))
       and then Warp > 0.0
       and then Has_Layout (Cache)
       and then Poles_Are_Off_The_Curve (Cache)
       and then Structure.Is_Regular (Warp, Cache)
       and then Is_Combined (Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active, Value)
       and then Lebesgue * R (Value.Largest_E) < 1.0;
   --  A certificate exists only for a prepared cache, which has a positive warp and no pole on the curve.

   procedure Lemma_Witness_Is_Regular
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result) is
   begin
      pragma Assert (Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Outcome = Certified);
      pragma
        Assert
          (Poles.Is_Prepared
             (Frame,
              Coefficients,
              Warp,
              Cache,
              Denominator_Scale,
              Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale)));
      pragma Assert (Warp >= Smallest_Warp);
      pragma Assert (Warp > 0.0);
      pragma Assert (Is_Combined (Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active, Value));
   end Lemma_Witness_Is_Regular;

   procedure Lemma_Sample_Is_Accurate (Index : Sample_Nodes.Sample_Index)
   with
     Ghost,
     Pre  => Sample_Nodes.Table.Is_Certified,
     Post =>
       Sample_Nodes.Is_In_Range (Sample_Nodes.Table.Samples (Index))
       and then Sample_Nodes.Is_Accurate (Sample_Nodes.Table.Samples (Index))
       and then 0.0 <= R (Sample_Nodes.Table.Samples (Index).Node)
       and then R (Sample_Nodes.Table.Samples (Index).Node) <= 1.0;
   --  Once the nodes are certified they are inside their margins, where Build is accurate.

   procedure Lemma_Sample_Is_Accurate (Index : Sample_Nodes.Sample_Index) is
   begin
      pragma Assert (Sample_Nodes.Certify (Sample_Nodes.Table.Samples));
      pragma Assert (Sample_Nodes.Is_In_Range (Sample_Nodes.Table.Samples (Index)));
      pragma Assert (Sample_Nodes.Has_Margins (Sample_Nodes.Table.Samples (Index)));
      pragma Assert (Sample_Nodes.Is_Accurate (Sample_Nodes.Table.Samples (Index)));
      pragma Assert (Sample_Nodes.Table.Samples (Index).Node >= 0.0);
      pragma Assert (Sample_Nodes.Table.Samples (Index).Node <= 1.0);
   end Lemma_Sample_Is_Accurate;

   procedure Lemma_Nodal_E
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Index             : Sample_Nodes.Sample_Index)
   with
     Ghost,
     Pre  =>
       Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value)
       and then Sample_Nodes.Table.Is_Certified,
     Post =>
       Has_Layout (Cache)
       and then
         abs Model.Denominator_Error
               (Coefficients, Cache, Denominator_Scale, Warp, R (Sample_Nodes.Table.Samples (Index).Node))
         <= R (Value.Largest_E);
   --  At every node, e is within the largest nodal bound of a certificate,

   procedure Lemma_Nodal_E
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Index             : Sample_Nodes.Sample_Index)
   is
      X : constant Real := R (Sample_Nodes.Table.Samples (Index).Node);
   begin
      Lemma_Witness_Is_Regular (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value);
      Lemma_Sample_Is_Accurate (Index);
      declare
         Node : constant Node_Bounds :=
           Nodes.Bounds_At_Node
             (Frame,
              Coefficients,
              Warp,
              Cache,
              Denominator_Scale,
              Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
              Sample_Nodes.Table.Samples (Index));
      begin
         pragma Assert (Node.Good);
         pragma Assert (Node.Nodal_E <= Value.Largest_E);
         pragma Assert (R (Node.Nodal_E) <= R (Value.Largest_E));
         pragma
           Assert (abs Model.Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, X) <= R (Node.Nodal_E));
      end;
   end Lemma_Nodal_E;

   procedure Lemma_Nodal_G
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Axis              : Axis_Name;
      Index             : Sample_Nodes.Sample_Index)
   with
     Ghost,
     Pre  =>
       Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value)
       and then Sample_Nodes.Table.Is_Certified
       and then Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active (Axis),
     Post =>
       Has_Layout (Cache)
       and then
         abs Model.Numerator_Error
               (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, R (Sample_Nodes.Table.Samples (Index).Node))
         <= R (Value.Largest_G (Axis));
   --  and so is g on an active axis.

   procedure Lemma_Nodal_G
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Axis              : Axis_Name;
      Index             : Sample_Nodes.Sample_Index)
   is
      X : constant Real := R (Sample_Nodes.Table.Samples (Index).Node);
   begin
      Lemma_Witness_Is_Regular (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value);
      Lemma_Sample_Is_Accurate (Index);
      declare
         Node : constant Node_Bounds :=
           Nodes.Bounds_At_Node
             (Frame,
              Coefficients,
              Warp,
              Cache,
              Denominator_Scale,
              Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
              Sample_Nodes.Table.Samples (Index));
      begin
         pragma Assert (Node.Good);
         pragma Assert (Node.Nodal_G (Axis) <= Value.Largest_G (Axis));
         pragma Assert (R (Node.Nodal_G (Axis)) <= R (Value.Largest_G (Axis)));
         pragma
           Assert
             (abs Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, X)
              <= R (Node.Nodal_G (Axis)));
      end;
   end Lemma_Nodal_G;

   procedure Lemma_Denominator_Value
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Index             : Sample_Nodes.Sample_Index)
   with
     Ghost,
     Pre  =>
       Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value)
       and then Sample_Nodes.Table.Is_Certified,
     Post =>
       Structure.Is_Regular (Warp, Cache)
       and then
         abs Polynomials.Eval
               (Structure.Denominator_Error_Polynomial (Coefficients, Cache, Denominator_Scale, Warp),
                Sample_Nodes.Node_Reals (Sample_Nodes.Table.Samples) (Index))
         <= R (Value.Largest_E);
   --  The same for the polynomials which e and g are, at the nodes as real numbers.

   procedure Lemma_Denominator_Value
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Index             : Sample_Nodes.Sample_Index)
   is
      X : constant Real := R (Sample_Nodes.Table.Samples (Index).Node);
   begin
      Lemma_Witness_Is_Regular (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value);
      Lemma_Sample_Is_Accurate (Index);
      Lemma_Nodal_E (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value, Index);
      pragma Assert (Sample_Nodes.Node_Reals (Sample_Nodes.Table.Samples) (Index) = X);
      Structure.Lemma_Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, X);
   end Lemma_Denominator_Value;

   procedure Lemma_Numerator_Value
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Axis              : Axis_Name;
      Index             : Sample_Nodes.Sample_Index)
   with
     Ghost,
     Pre  =>
       Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value)
       and then Sample_Nodes.Table.Is_Certified
       and then Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active (Axis),
     Post =>
       Structure.Is_Regular (Warp, Cache)
       and then
         abs Polynomials.Eval
               (Structure.Numerator_Error_Polynomial (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis),
                Sample_Nodes.Node_Reals (Sample_Nodes.Table.Samples) (Index))
         <= R (Value.Largest_G (Axis));

   procedure Lemma_Numerator_Value
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Axis              : Axis_Name;
      Index             : Sample_Nodes.Sample_Index)
   is
      X : constant Real := R (Sample_Nodes.Table.Samples (Index).Node);
   begin
      Lemma_Witness_Is_Regular (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value);
      Lemma_Sample_Is_Accurate (Index);
      Lemma_Nodal_G (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value, Axis, Index);
      pragma Assert (Sample_Nodes.Node_Reals (Sample_Nodes.Table.Samples) (Index) = X);
      Structure.Lemma_Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, X);
   end Lemma_Numerator_Value;

   procedure Lemma_Denominator_Bound
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      V                 : Real)
   with
     Ghost,
     Pre  =>
       Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value)
       and then Sample_Nodes.Table.Is_Certified
       and then 0.0 <= V
       and then V <= 1.0,
     Post =>
       Has_Layout (Cache)
       and then
         abs Model.Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, V)
         <= Lebesgue * R (Value.Largest_E);
   --  |e| ≤ Λ·E on the whole curve, by interpolation at the nodes.

   procedure Lemma_Denominator_Bound
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      V                 : Real) is
   begin
      Lemma_Witness_Is_Regular (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value);
      declare
         P           : constant Polynomials.Polynomial :=
           Structure.Denominator_Error_Polynomial (Coefficients, Cache, Denominator_Scale, Warp);
         Node_Values : constant Polynomials.Interpolation.Node_Array :=
           Sample_Nodes.Node_Reals (Sample_Nodes.Table.Samples);
      begin
         for Index in Sample_Nodes.Sample_Index loop
            Lemma_Denominator_Value (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value, Index);
            pragma
              Loop_Invariant
                (for all I in 1 .. Index => abs Polynomials.Eval (P, Node_Values (I)) <= R (Value.Largest_E));
         end loop;
         pragma
           Assert (for all I in Node_Values'Range => abs Polynomials.Eval (P, Node_Values (I)) <= R (Value.Largest_E));
         Sample_Nodes.Lemma_Lebesgue (Sample_Nodes.Table.Samples, V);
         Polynomials.Interpolation.Lemma_Interpolation_Bound (P, Node_Values, V, R (Value.Largest_E), Lebesgue);
         Structure.Lemma_Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, V);
      end;
   end Lemma_Denominator_Bound;

   procedure Lemma_Numerator_Bound
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Axis              : Axis_Name;
      V                 : Real)
   with
     Ghost,
     Pre  =>
       Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value)
       and then Sample_Nodes.Table.Is_Certified
       and then 0.0 <= V
       and then V <= 1.0
       and then Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active (Axis),
     Post =>
       Has_Layout (Cache)
       and then
         abs Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, V)
         <= Lebesgue * R (Value.Largest_G (Axis));
   --  |g| ≤ Λ·G on the whole curve, on an active axis.

   procedure Lemma_Numerator_Bound
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Axis              : Axis_Name;
      V                 : Real) is
   begin
      Lemma_Witness_Is_Regular (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value);
      declare
         P           : constant Polynomials.Polynomial :=
           Structure.Numerator_Error_Polynomial (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis);
         Node_Values : constant Polynomials.Interpolation.Node_Array :=
           Sample_Nodes.Node_Reals (Sample_Nodes.Table.Samples);
      begin
         for Index in Sample_Nodes.Sample_Index loop
            Lemma_Numerator_Value (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value, Axis, Index);
            pragma
              Loop_Invariant
                (for all I in 1 .. Index => abs Polynomials.Eval (P, Node_Values (I)) <= R (Value.Largest_G (Axis)));
         end loop;
         pragma
           Assert
             (for all I in Node_Values'Range =>
                abs Polynomials.Eval (P, Node_Values (I)) <= R (Value.Largest_G (Axis)));
         Sample_Nodes.Lemma_Lebesgue (Sample_Nodes.Table.Samples, V);
         Polynomials.Interpolation.Lemma_Interpolation_Bound (P, Node_Values, V, R (Value.Largest_G (Axis)), Lebesgue);
         Structure.Lemma_Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, V);
      end;
   end Lemma_Numerator_Bound;

   procedure Lemma_Real_Axis (T, Q, D, G, Bound, M : Real)
   with
     Ghost,
     Pre  => G = Q * D * T and then abs G <= Bound and then M > 0.0 and then Q >= M and then D >= 1.0,
     Post => T * T * (M * M) <= Bound * Bound;
   --  |T|·M ≤ |T|·Q ≤ |T|·Q·D = |G| ≤ Bound, squared.

   procedure Lemma_Real_Axis (T, Q, D, G, Bound, M : Real) is
      A : constant Real := abs T;
      S : constant Real := A * M;
   begin
      pragma Assert (A >= 0.0);
      pragma Assert (A * M <= A * Q);
      pragma Assert (A * Q >= 0.0);
      pragma Assert (A * Q * 1.0 <= A * Q * D);
      pragma Assert (Q * D > 0.0);
      pragma Assert (abs (Q * D * T) = Q * D * A);
      pragma Assert (A * Q * D = Q * D * A);
      pragma Assert (S <= Bound);
      pragma Assert (S >= 0.0);
      pragma Assert (S * S <= Bound * S);
      pragma Assert (Bound * S <= Bound * Bound);
      pragma Assert (A * A = T * T);
      pragma Assert (S * S = A * A * (M * M));
   end Lemma_Real_Axis;

   procedure Lemma_Axis_Tangent_Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Axis              : Axis_Name;
      V                 : Real)
   with
     Ghost,
     Pre  =>
       Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value)
       and then Sample_Nodes.Table.Is_Certified
       and then 0.0 <= V
       and then V <= 1.0,
     Post =>
       Has_Layout (Cache)
       and then
         Model.Tangent_Error (Frame, Coefficients, Warp, Cache, Axis, V)
         * Model.Tangent_Error (Frame, Coefficients, Warp, Cache, Axis, V)
         * ((1.0 - Lebesgue * R (Value.Largest_E)) * (1.0 - Lebesgue * R (Value.Largest_E)))
         <= Term (Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active, Value.Largest_G, Axis);
   --  The squared tangent error on one axis, times (1 - Λ·E)², is at most (Λ·G)², and zero on an axis which is
   --  not active.

   procedure Lemma_Axis_Tangent_Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      Axis              : Axis_Name;
      V                 : Real) is
   begin
      Lemma_Witness_Is_Regular (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value);
      declare
         T : constant Real := Model.Tangent_Error (Frame, Coefficients, Warp, Cache, Axis, V);
         Q : constant Real := Model.Cached_Denominator (Cache, Denominator_Scale, Warp, V);
         D : constant Real := Denominator (Coefficients, V);
         G : constant Real := Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, V);
         M : constant Real := 1.0 - Lebesgue * R (Value.Largest_E);
      begin
         pragma Assert (M > 0.0);
         if Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active (Axis) then
            Lemma_Denominator_Bound (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value, V);
            Lemma_Numerator_Bound (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value, Axis, V);
            Structure.Lemma_Tangent_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, V);
            pragma Assert (G = Q * D * T);
            pragma Assert (Model.Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, V) = Q - D);
            pragma Assert (D >= 1.0);
            pragma Assert (Q - D >= -(Lebesgue * R (Value.Largest_E)));
            pragma Assert (Q >= M);
            pragma Assert (abs G <= Lebesgue * R (Value.Largest_G (Axis)));
            Lemma_Real_Axis (T, Q, D, G, Lebesgue * R (Value.Largest_G (Axis)), M);
            pragma
              Assert
                (T * T * (M * M)
                 <= Term
                      (Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active,
                       Value.Largest_G,
                       Axis));
         else
            pragma Assert (Structure.Vanishes_On_Axis (Frame, Coefficients, Cache, Axis));
            Structure.Lemma_Inactive_Axis (Frame, Coefficients, Warp, Cache, Axis, V);
            pragma Assert (T = 0.0);
            pragma Assert (T * T * (M * M) = 0.0);
            pragma
              Assert
                (Term
                   (Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active, Value.Largest_G, Axis)
                 = 0.0);
         end if;
      end;
   end Lemma_Axis_Tangent_Error;

   procedure Lemma_Real_Total (TX, TY, TZ, TE, BX, BY, BZ, BE, Error_Square, M2 : Real)
   with
     Ghost,
     Pre  =>
       TX * M2 <= BX
       and then TY * M2 <= BY
       and then TZ * M2 <= BZ
       and then TE * M2 <= BE
       and then BX + BY + BZ + BE <= Error_Square * M2
       and then M2 > 0.0,
     Post => TX + TY + TZ + TE <= Error_Square;
   --  Sum over the axes and divide by the positive (1 - Λ·E)².

   procedure Lemma_Real_Total (TX, TY, TZ, TE, BX, BY, BZ, BE, Error_Square, M2 : Real) is
      S : constant Real := TX + TY + TZ + TE;
   begin
      pragma Assert (S * M2 = TX * M2 + TY * M2 + TZ * M2 + TE * M2);
      pragma Assert (S * M2 <= Error_Square * M2);
      if S > Error_Square then
         pragma Assert (S * M2 > Error_Square * M2);
      end if;
   end Lemma_Real_Total;

   procedure Lemma_Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      V                 : Real) is
   begin
      Lemma_Witness_Is_Regular (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value);
      Lemma_Axis_Tangent_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value, X_Axis, V);
      Lemma_Axis_Tangent_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value, Y_Axis, V);
      Lemma_Axis_Tangent_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value, Z_Axis, V);
      Lemma_Axis_Tangent_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value, E_Axis, V);
      declare
         Active : constant Axis_Flags := Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active;
         M2     : constant Real := (1.0 - Lebesgue * R (Value.Largest_E)) * (1.0 - Lebesgue * R (Value.Largest_E));
         TX     : constant Real := Model.Tangent_Error (Frame, Coefficients, Warp, Cache, X_Axis, V);
         TY     : constant Real := Model.Tangent_Error (Frame, Coefficients, Warp, Cache, Y_Axis, V);
         TZ     : constant Real := Model.Tangent_Error (Frame, Coefficients, Warp, Cache, Z_Axis, V);
         TE     : constant Real := Model.Tangent_Error (Frame, Coefficients, Warp, Cache, E_Axis, V);
      begin
         pragma Assert (Is_Combined (Active, Value));
         pragma Assert (1.0 - Lebesgue * R (Value.Largest_E) > 0.0);
         pragma Assert (M2 > 0.0);
         pragma
           Assert
             (Term (Active, Value.Largest_G, X_Axis)
              + Term (Active, Value.Largest_G, Y_Axis)
              + Term (Active, Value.Largest_G, Z_Axis)
              + Term (Active, Value.Largest_G, E_Axis)
              <= R (Value.Error_Bound) * R (Value.Error_Bound) * M2);
         Lemma_Real_Total
           (TX * TX,
            TY * TY,
            TZ * TZ,
            TE * TE,
            Term (Active, Value.Largest_G, X_Axis),
            Term (Active, Value.Largest_G, Y_Axis),
            Term (Active, Value.Largest_G, Z_Axis),
            Term (Active, Value.Largest_G, E_Axis),
            R (Value.Error_Bound) * R (Value.Error_Bound),
            M2);
         pragma
           Assert
             (Model.Squared_Tangent_Error (Frame, Coefficients, Warp, Cache, V)
              = TX * TX + TY * TY + TZ * TZ + TE * TE);
      end;
   end Lemma_Error;

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Bound;
