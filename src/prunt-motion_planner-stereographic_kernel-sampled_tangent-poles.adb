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

package body Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Poles
  with SPARK_Mode => On
is
   use all type Prunt.Motion_Planner.Reals.Real;
   --  For To_Real, by which a count of poles becomes a real number.

   --  The executable code of this unit is Append and Prepare. The rest of the unit is the proof, laid out as
   --  follows.
   --
   --    * Facts about single floating-point operations and small facts of real arithmetic come first, then the
   --      error analysis of each computation of Append as a lemma: the squared distance from the curve
   --      (Lemma_Square), the reciprocals (Lemma_Real_Reciprocal, Lemma_Pair_Reciprocal) and one pole's share
   --      of Slope (Lemma_Slope_Is_Finite, Lemma_Slope_Is_Grown).
   --    * Append promises Is_Appended: the earlier terms stay, the new term is the pole of the given slots, every
   --      Slope has grown by its share, and an axis stays inactive only if the pole has no residue on it.
   --      Lemma_Appended turns that into Is_Partly_Prepared, which is Is_Cache_Prepared for the poles gathered
   --      so far, and Prepare carries it through its loops.
   --    * Three things keep the checks within reach of the provers. Facts which are only carried along are
   --      functions whose bodies are hidden in nested packages (Carried, Appending, Progress), so that the
   --      provers see them as single facts. That an operation does not overflow is stated as membership of its
   --      result in the type, which is how the check itself reads. And pragma Assert_And_Cut drops the
   --      arithmetic of one part of Append before the next begins.
   --
   --  A multiplication by a power of two is exact unless it underflows, but nothing which the provers are given
   --  says that a multiple of a floating-point number by a power of two is again one: they know a rounded
   --  product only to within its relative error. The doubled residue of a pair and the allowance Delta_Y are
   --  therefore stated, here as in the specification, with that error: Lemma_Doubling_Is_Rounded and
   --  Lemma_Chart_Scale_Is_Rounded.

   Overflowing_Divisor : constant Dimensionless := 2.0**(-1024);
   --  The reciprocal of a positive number no larger than this overflows.

   Unusable_Divisor : constant Dimensionless := 2.0**(-511);
   --  The reciprocal of a positive number smaller than this is beyond Magnitude_Limit.

   function Usable (Value : Dimensionless) return Boolean
   is (abs Value <= Magnitude_Limit);

   ---------------------------------------------------------------------------
   --  Facts about single floating-point operations
   ---------------------------------------------------------------------------

   Limit_Bound : constant := 3.0 * 2.0**198;
   --  More than Magnitude_Limit, and less than 2**200.

   Tiny : constant := 2.0**(-511);
   --  Unusable_Divisor, as a real number.

   Operand_Limit : constant Dimensionless := 2.0**200
   with Ghost;
   --  More than Magnitude_Limit: no pole component and no distance from [0, 1] is larger.

   Operand_Bound : constant := 2.0**200;

   procedure Lemma_Order (Low, High : Dimensionless)
   with Ghost, Pre => Low <= High, Post => R (Low) <= R (High);
   --  The order of two floating-point numbers is that of the real numbers which they denote.

   procedure Lemma_Order (Low, High : Dimensionless) is null;

   procedure Lemma_Same (Left, Right : Dimensionless)
   with Ghost, Pre => Left = Right, Post => R (Left) = R (Right);
   --  Equal floating-point numbers denote the same real number.

   procedure Lemma_Same (Left, Right : Dimensionless) is
   begin
      Lemma_Order (Left, Right);
      Lemma_Order (Right, Left);
   end Lemma_Same;

   procedure Lemma_Same_Float (Left, Right : Dimensionless)
   with Ghost, Pre => R (Left) = R (Right), Post => Left = Right;
   --  Floating-point numbers which denote the same real number are equal.

   procedure Lemma_Same_Float (Left, Right : Dimensionless) is null;

   procedure Lemma_Abs_Is_Exact (Value : Dimensionless)
   with Ghost, Post => R (abs Value) = abs (R (Value));
   --  The floating-point absolute value is not rounded.

   procedure Lemma_Abs_Is_Exact (Value : Dimensionless) is null;

   procedure Lemma_Negation_Is_Exact (Value : Dimensionless)
   with Ghost, Post => R (-Value) = 0.0 - R (Value);
   --  Nor is the negation.

   procedure Lemma_Negation_Is_Exact (Value : Dimensionless) is null;

   procedure Lemma_Usable_Bound (Value : Dimensionless)
   with
     Ghost,
     Pre  => abs Value <= Magnitude_Limit,
     Post => abs (R (Value)) <= Limit_Bound and then abs Value <= Operand_Limit;
   --  Magnitude_Limit, as a bound on the real number.

   procedure Lemma_Usable_Bound (Value : Dimensionless) is null;

   procedure Lemma_Doubling_Is_Rounded (Value : Dimensionless)
   with
     Ghost,
     Pre  => abs Value <= Dimensionless'Last / 2.0,
     Post => abs (R (2.0 * Value) - 2.0 * R (Value)) <= 2.0 * Epsilon * abs (R (Value)) + Eta;
   --  Doubling is one rounded multiplication. In fact it is exact, but the provers know a rounded product only
   --  to within its relative error, and Is_Pair_Term asks no more of the doubled residues of a pair.

   procedure Lemma_Doubling_Is_Rounded (Value : Dimensionless) is null;

   procedure Lemma_Doubling_Keeps_Zero (Value : Dimensionless)
   with
     Ghost,
     Pre  => abs Value <= Dimensionless'Last / 2.0,
     Post => (2.0 * Value) in Dimensionless and then (if 2.0 * Value = 0.0 then Value = 0.0);
   --  Doubling cannot overflow within half the largest number, and never underflows to zero.

   procedure Lemma_Doubling_Keeps_Zero (Value : Dimensionless) is null;

   procedure Lemma_Unit_Is_Exact (Value : Dimensionless)
   with
     Ghost,
     Post =>
       R (1.0 * Value) = R (Value)
       and then (1.0 * Value) in Dimensionless
       and then (if 1.0 * Value = 0.0 then Value = 0.0);
   --  Multiplying by one changes nothing.

   procedure Lemma_Unit_Is_Exact (Value : Dimensionless) is null;

   procedure Lemma_Chart_Scale_Range (Value : Dimensionless)
   with
     Ghost,
     Pre  => Value >= 0.0 and then Value <= Magnitude_Limit,
     Post =>
       (Chart_Allowance * Value) in Dimensionless
       and then Chart_Allowance * Value >= 0.0
       and then Chart_Allowance * Value <= Chart_Allowance * Magnitude_Limit;
   --  Multiplying by a positive constant keeps the order.

   procedure Lemma_Chart_Scale_Range (Value : Dimensionless) is null;

   procedure Lemma_Chart_Scale_Is_Rounded (Value : Dimensionless)
   with
     Ghost,
     Pre  => Value >= 0.0 and then Value <= Magnitude_Limit,
     Post => R (Chart_Allowance * Value) >= Chart_Allowance_Real * R (Value) * (1.0 - Epsilon) - Eta;
   --  Multiplying by Chart_Allowance is one rounded multiplication, which is what Is_Chart_Prepared asks of
   --  Delta_Y, for the reason given at Lemma_Doubling_Is_Rounded.

   procedure Lemma_Chart_Scale_Is_Rounded (Value : Dimensionless) is null;

   procedure Lemma_Maximum (A, B : Dimensionless)
   with
     Ghost,
     Pre  => A >= 0.0 and then B >= 0.0,
     Post =>
       Dimensionless'Max (A, B) >= A
       and then Dimensionless'Max (A, B) >= B
       and then (Dimensionless'Max (A, B) = 0.0) = (A = 0.0 and then B = 0.0);
   --  The floating-point maximum is at least both operands, and zero only if both are.

   procedure Lemma_Maximum (A, B : Dimensionless) is null;

   ---------------------------------------------------------------------------
   --  Small facts of real arithmetic
   ---------------------------------------------------------------------------

   procedure Lemma_Real_Product_Bound (X, Y, Limit : Real)
   with Ghost, Pre => abs X <= Limit and then abs Y <= Limit, Post => abs (X * Y) <= Limit * Limit;
   --  |X| ≤ L and |Y| ≤ L give |X·Y| ≤ L².

   procedure Lemma_Real_Product_Bound (X, Y, Limit : Real) is
   begin
      pragma Assert (abs X * abs Y <= Limit * abs Y);
      pragma Assert (Limit * abs Y <= Limit * Limit);
   end Lemma_Real_Product_Bound;

   procedure Lemma_Product_Is_Monotonic (X, Y, Bound_X, Bound_Y : Real)
   with
     Ghost,
     Pre  => 0.0 <= X and then X <= Bound_X and then 0.0 <= Y and then Y <= Bound_Y,
     Post => X * Y <= Bound_X * Bound_Y;
   --  A product of nonnegative factors grows with each of them.

   procedure Lemma_Product_Is_Monotonic (X, Y, Bound_X, Bound_Y : Real) is
   begin
      pragma Assert (X * Y <= Bound_X * Y);
      pragma Assert (Bound_X * Y <= Bound_X * Bound_Y);
   end Lemma_Product_Is_Monotonic;

   procedure Lemma_Square_Is_Monotonic (A, B : Real)
   with Ghost, Pre => 0.0 <= A and then A <= B, Post => A * A <= B * B;

   procedure Lemma_Square_Is_Monotonic (A, B : Real) is
   begin
      Lemma_Product_Is_Monotonic (A, A, B, B);
   end Lemma_Square_Is_Monotonic;

   procedure Lemma_Reciprocal_Is_Large (X, Bound : Real)
   with
     Ghost,
     Pre  => X /= 0.0 and then abs X <= Bound,
     Post => abs (1.0 / X) * Bound >= 1.0 and then (1.0 / X) * X = 1.0;
   --  The reciprocal of a small number is large: |1/X| ≥ 1/Bound.

   procedure Lemma_Reciprocal_Is_Large (X, Bound : Real) is
      Y : constant Real := 1.0 / X;
   begin
      pragma Assert (Y * X = 1.0);
      pragma Assert (abs Y * abs X = 1.0);
      pragma Assert (abs Y * abs X <= abs Y * Bound);
   end Lemma_Reciprocal_Is_Large;

   procedure Lemma_Reciprocal_Is_Small (X, Bound : Real)
   with
     Ghost,
     Pre  => Bound > 0.0 and then X >= Bound,
     Post => 1.0 / X > 0.0 and then (1.0 / X) * Bound <= 1.0 and then (1.0 / X) * X = 1.0;
   --  The reciprocal of a large number is small: 1/X ≤ 1/Bound.

   procedure Lemma_Reciprocal_Is_Small (X, Bound : Real) is
      Y : constant Real := 1.0 / X;
   begin
      pragma Assert (Y * X = 1.0);
      pragma Assert (Y > 0.0);
      pragma Assert (Y * Bound <= Y * X);
   end Lemma_Reciprocal_Is_Small;

   procedure Lemma_Quotient_Is_Monotonic (X1, X2, Y1, Y2 : Real)
   with
     Ghost,
     Pre  => 0.0 <= X1 and then X1 <= X2 and then 0.0 < Y2 and then Y2 <= Y1,
     Post => X1 / Y1 <= X2 / Y2 and then X2 / Y2 >= 0.0;
   --  A quotient of positive numbers grows with its numerator and falls with its denominator.

   procedure Lemma_Quotient_Is_Monotonic (X1, X2, Y1, Y2 : Real) is
      A : constant Real := X1 / Y1;
      B : constant Real := X2 / Y2;
   begin
      pragma Assert (A * Y1 = X1);
      pragma Assert (B * Y2 = X2);
      pragma Assert (B >= 0.0);
      pragma Assert (B * Y2 <= B * Y1);
      pragma Assert ((A - B) * Y1 <= 0.0);
   end Lemma_Quotient_Is_Monotonic;

   ---------------------------------------------------------------------------
   --  How far Up and Down move a value
   ---------------------------------------------------------------------------

   Pad_Scale : constant := 2.0**(-51);
   --  Two model epsilons, as a real number.

   Smallest_Pad : constant := 2.0**(-1022);
   --  Model_Small, as a real number.

   Pad_Reach : constant := 2.0**(-50);
   Pad_Floor : constant := 2.0**(-1021);
   --  Up and Down move a value by no more than Pad_Reach times its magnitude, plus Pad_Floor.

   procedure Lemma_Scale (Value : Dimensionless)
   with
     Ghost,
     Pre  => Value >= 0.0 and then Value <= Half_Last,
     Post =>
       abs (R (2.0 * Dimensionless'Model_Epsilon * Value) - Pad_Scale * R (Value))
       <= Epsilon * Pad_Scale * R (Value) + Eta;
   --  Scaling by two model epsilons is one rounded multiplication.

   procedure Lemma_Scale (Value : Dimensionless) is null;

   procedure Lemma_Padding_Bounds (Value : Dimensionless)
   with
     Ghost,
     Pre  => abs Value <= Half_Last,
     Post =>
       R (Roundoff_Padding (Value)) >= Smallest_Pad
       and then
         R (Roundoff_Padding (Value))
         <= Pad_Scale * abs (R (Value)) + Epsilon * Pad_Scale * abs (R (Value)) + Eta + Smallest_Pad;

   procedure Lemma_Padding_Bounds (Value : Dimensionless) is
   begin
      Lemma_Scale (abs Value);
   end Lemma_Padding_Bounds;

   procedure Lemma_Pad_Reach (V, P, T : Real)
   with
     Ghost,
     Pre  =>
       P >= 0.0
       and then P <= Pad_Scale * abs V + Epsilon * Pad_Scale * abs V + Eta + Smallest_Pad
       and then
         (abs (T - (V + P)) <= Epsilon * abs (V + P) + Eta or else abs (T - (V - P)) <= Epsilon * abs (V - P) + Eta),
     Post => abs (T - V) <= Pad_Reach * abs V + Pad_Floor;
   --  T is V moved by the padding P, as that sum or difference rounds.

   procedure Lemma_Pad_Reach (V, P, T : Real) is
   begin
      pragma Assert (abs (V + P) <= abs V + P and then abs (V - P) <= abs V + P);
      pragma Assert (abs (T - V) <= P + Epsilon * (abs V + P) + Eta);
   end Lemma_Pad_Reach;

   procedure Lemma_Raised (Value : Dimensionless; Exact : Real)
   with
     Ghost,
     Pre  => abs Value <= Half_Last and then Is_Rounding_Of (Value, Exact),
     Post =>
       R (Up (Value)) >= Exact
       and then R (Up (Value)) >= R (Value)
       and then R (Up (Value)) <= R (Value) + Pad_Reach * abs (R (Value)) + Pad_Floor;
   --  Up covers the rounding of the operation which gave Value, and moves it no further than the reach.

   procedure Lemma_Raised (Value : Dimensionless; Exact : Real) is
      Pad : constant Dimensionless := Roundoff_Padding (Value);
   begin
      Lemma_Padding_Bounds (Value);
      Float_Lemmas.Lemma_Rounding_Error_Add (Value, Pad);
      Lemma_Inflate_Encloses (Value, Exact);
      Lemma_Same (Up (Value), Inflate_Finite (Value, True));
      Lemma_Same (Inflate_Finite (Value, True), Value + Pad);
      Lemma_Order (Value, Up (Value));
      Lemma_Pad_Reach (R (Value), R (Pad), R (Value + Pad));
   end Lemma_Raised;

   procedure Lemma_Lowered (Value : Dimensionless)
   with
     Ghost,
     Pre  => abs Value <= Half_Last,
     Post =>
       R (Down (Value)) <= R (Value) - Intervals.Slack (Value)
       and then R (Down (Value)) >= R (Value) - Pad_Reach * abs (R (Value)) - Pad_Floor;
   --  Down moves Value by more than its slack, and no further than the reach.

   procedure Lemma_Lowered (Value : Dimensionless) is
      Pad : constant Dimensionless := Roundoff_Padding (Value);
   begin
      Lemma_Padding_Bounds (Value);
      Float_Lemmas.Lemma_Rounding_Error_Sub (Value, Pad);
      Lemma_Inflate_Margin (Value);
      Lemma_Same (Down (Value), Inflate_Finite (Value, False));
      Lemma_Same (Inflate_Finite (Value, False), Value - Pad);
      Lemma_Pad_Reach (R (Value), R (Pad), R (Value - Pad));
   end Lemma_Lowered;

   ---------------------------------------------------------------------------
   --  The squared distance from the curve, rounded toward zero
   ---------------------------------------------------------------------------

   Shrink : constant := 1.0 - 3.0 * 2.0**(-54);
   --  A value moved down by its slack is at most this times the exact result of the operation which gave it.

   Shrink_Square : constant := Shrink * Shrink;

   Lowered_Limit : constant Dimensionless := 2.0**401
   with Ghost;
   --  No lowered square of an operand within Operand_Limit is larger in magnitude.

   Lowered_Bound : constant := 2.0**401;

   procedure Lemma_Shrunk (V, E, D : Real)
   with
     Ghost,
     Pre  => abs (V - E) <= Epsilon * abs E + Eta and then D <= V - (Slack_Factor * abs V + Twice_Eta),
     Post => (if E >= 0.0 then D <= E * Shrink else D < 0.0);
   --  V is the rounded E and D is V less its slack. The slack is two and a half Epsilon of V, of which the
   --  rounding of V uses one.

   procedure Lemma_Shrunk (V, E, D : Real) is
   begin
      if E >= 0.0 then
         if V >= 0.0 then
            pragma Assert (V <= E * (1.0 + Epsilon) + Eta);
            pragma Assert (D <= V * (1.0 - Slack_Factor) - Twice_Eta);
            pragma Assert (D <= E * ((1.0 + Epsilon) * (1.0 - Slack_Factor)));
         else
            pragma Assert (E <= Eta + Epsilon * E);
            pragma Assert (D < 0.0);
            pragma Assert (E * Shrink >= 0.0);
         end if;
      else
         pragma Assert (V <= Eta);
      end if;
   end Lemma_Shrunk;

   procedure Lemma_Lowered_Square (X : Dimensionless)
   with
     Ghost,
     Pre  => abs X <= Operand_Limit,
     Post =>
       abs (X * X) <= Half_Last
       and then abs (Down (X * X)) <= Lowered_Limit
       and then R (Down (X * X)) <= R (X) * R (X) * Shrink;
   --  The lowered square of a number is below its exact square, with margin.

   procedure Lemma_Lowered_Square (X : Dimensionless) is
   begin
      Lemma_Order (abs X, Operand_Limit);
      Lemma_Abs_Is_Exact (X);
      Lemma_Real_Product_Bound (R (X), R (X), Operand_Bound);
      Lemma_Product_Is_Bounded (X, X);
      Lemma_Lowered (X * X);
      pragma Assert (R (X) * R (X) >= 0.0);
      Lemma_Shrunk (R (X * X), R (X) * R (X), R (Down (X * X)));
      pragma Assert (abs (R (X * X)) <= Operand_Bound * Operand_Bound * (1.0 + Epsilon) + Eta);
      pragma Assert (abs (R (Down (X * X))) <= Lowered_Bound);
   end Lemma_Lowered_Square;

   procedure Lemma_Lowered_Sum (A, B : Dimensionless)
   with
     Ghost,
     Pre  => abs A <= Lowered_Limit and then abs B <= Lowered_Limit,
     Post =>
       abs (A + B) <= Half_Last
       and then
         (if Down (A + B) > 0.0 then R (A) + R (B) >= 0.0 and then R (Down (A + B)) <= (R (A) + R (B)) * Shrink);
   --  A positive lowered sum is below the exact sum, with margin.

   procedure Lemma_Lowered_Sum (A, B : Dimensionless) is
   begin
      Float_Lemmas.Lemma_Rounding_Error_Add (A, B);
      Lemma_Lowered (A + B);
      Lemma_Shrunk (R (A + B), R (A) + R (B), R (Down (A + B)));
   end Lemma_Lowered_Sum;

   Small_Distance : constant := 2.0**(-300);
   --  Below this distance the square is compared in absolute terms, above it in relative terms.

   Eta_Share : constant := 2.0**(-775);
   --  Eta divided by Small_Distance.

   Growth : constant := 1.0 + 2.0**(-53) + 2.0**(-775);
   --  A rounded difference of at least Small_Distance is at most this times the exact one.

   Growth_Square : constant := Growth * Growth;

   Gap_Bound   : constant := 2.0**(-352);
   Sum_Bound   : constant := 2.0**(-298);
   Square_Dust : constant := 2.0**(-650);
   --  A rounded difference below Small_Distance has a square within Square_Dust of the exact one.

   procedure Lemma_Near_Square (Rounded, Exact : Real)
   with
     Ghost,
     Pre  => Exact >= Small_Distance and then Rounded >= 0.0 and then Rounded <= Exact + Epsilon * Exact + Eta,
     Post => Rounded * Rounded <= Exact * Exact * Growth_Square;

   procedure Lemma_Near_Square (Rounded, Exact : Real) is
   begin
      pragma Assert (Eta <= Eta_Share * Exact);
      pragma Assert (Rounded <= Exact * Growth);
      Lemma_Square_Is_Monotonic (Rounded, Exact * Growth);
      pragma Assert ((Exact * Growth) * (Exact * Growth) = Exact * Exact * Growth_Square);
   end Lemma_Near_Square;

   procedure Lemma_Tiny_Square (Rounded, Exact : Real)
   with
     Ghost,
     Pre  =>
       Exact >= 0.0
       and then Exact < Small_Distance
       and then Rounded >= 0.0
       and then Rounded <= Exact + Epsilon * Exact + Eta,
     Post => Rounded * Rounded <= Exact * Exact + Square_Dust;

   procedure Lemma_Tiny_Square (Rounded, Exact : Real) is
   begin
      if Rounded <= Exact then
         Lemma_Square_Is_Monotonic (Rounded, Exact);
      else
         pragma Assert (Rounded - Exact <= Gap_Bound);
         pragma Assert (Rounded + Exact <= Sum_Bound);
         Lemma_Product_Is_Monotonic (Rounded - Exact, Rounded + Exact, Gap_Bound, Sum_Bound);
         pragma Assert ((Rounded - Exact) * (Rounded + Exact) = Rounded * Rounded - Exact * Exact);
      end if;
   end Lemma_Tiny_Square;

   procedure Lemma_Distance (Rounded, Exact, Vertical, First, Second, Square : Real)
   with
     Ghost,
     Pre  =>
       Exact >= 0.0
       and then Rounded >= 0.0
       and then abs (Rounded - Exact) <= Epsilon * Exact + Eta
       and then Vertical >= 0.0
       and then First <= Rounded * Rounded * Shrink
       and then Second <= Vertical * Shrink
       and then First + Second >= 0.0
       and then Square <= (First + Second) * Shrink
       and then Square >= Tiny,
     Post => Square <= Exact * Exact + Vertical;
   --  Rounded is the horizontal distance as the code computes it and Exact the true one; Vertical is the square
   --  of the imaginary part; First and Second are the two lowered squares and Square the lowered sum. The two
   --  levels of lowering leave a margin of three Epsilon, of which the rounding of the distance, squared, uses two.
   --  Below Small_Distance, where Eta matters to that rounding, its square is negligible against Tiny.

   procedure Lemma_Distance (Rounded, Exact, Vertical, First, Second, Square : Real) is
      Total  : constant Real := Rounded * Rounded + Vertical;
      Wanted : constant Real := Exact * Exact;
   begin
      pragma Assert (Rounded * Rounded >= 0.0);
      pragma Assert (First + Second <= Total * Shrink);
      pragma Assert ((First + Second) * Shrink <= Total * Shrink * Shrink);
      pragma Assert (Square <= Total * Shrink_Square);
      pragma Assert (Total >= Tiny);
      if Exact >= Small_Distance then
         Lemma_Near_Square (Rounded, Exact);
         pragma Assert (Total <= Wanted * Growth_Square + Vertical);
         pragma Assert (Total * Shrink_Square <= (Wanted * Growth_Square + Vertical) * Shrink_Square);
         pragma Assert (Wanted >= 0.0);
         pragma Assert ((Wanted * Growth_Square + Vertical) * Shrink_Square <= Wanted + Vertical);
      else
         Lemma_Tiny_Square (Rounded, Exact);
         pragma Assert (Total <= Wanted + Vertical + Square_Dust);
         pragma Assert (Total * Shrink_Square <= Total - Square_Dust);
      end if;
   end Lemma_Distance;

   ---------------------------------------------------------------------------
   --  The reciprocals
   ---------------------------------------------------------------------------

   Reciprocal_Floor : constant := 2.0**(-200);
   --  The reciprocal of a number within Operand_Limit is at least this in magnitude.

   Half_Tiny : constant := 2.0**(-512);

   Norm_Error : constant := 3.0 * 2.0**(-53);
   --  The relative error of the computed |π|²: one rounding in each square and one in their sum.

   Norm_Bound : constant := 2.0**401;
   --  No |π|² of a usable pole is larger.

   Norm_Floor : constant := 2.0**(-401);

   procedure Lemma_Real_Reciprocal (Pole : Dimensionless)
   with
     Ghost,
     Pre  => abs Pole >= Unusable_Divisor and then abs Pole <= Magnitude_Limit,
     Post =>
       R (Pole) /= 0.0
       and then (1.0 / Pole) in Dimensionless
       and then abs (R (1.0 / Pole) - 1.0 / R (Pole)) <= 2.0 * Epsilon * abs (1.0 / R (Pole));
   --  The reciprocal of a real pole is one rounded division, whose result is too large for Eta to matter.

   procedure Lemma_Real_Reciprocal (Pole : Dimensionless) is
   begin
      Lemma_Usable_Bound (Pole);
      Lemma_Abs_Is_Exact (Pole);
      Lemma_Order (Unusable_Divisor, abs Pole);
      pragma Assert (abs (R (Pole)) >= Tiny);
      Float_Lemmas.Lemma_Rounding_Error_Div (1.0, Pole);
      Lemma_Reciprocal_Is_Large (R (Pole), Operand_Bound);
      pragma Assert (abs (1.0 / R (Pole)) >= Reciprocal_Floor);
      pragma Assert (Eta <= Epsilon * abs (1.0 / R (Pole)));
   end Lemma_Real_Reciprocal;

   procedure Lemma_Norm_Is_Close (First, Second, First_Rounded, Second_Rounded, Sum : Real)
   with
     Ghost,
     Pre  =>
       First >= 0.0
       and then Second >= 0.0
       and then abs (First_Rounded - First) <= Epsilon * First + Eta
       and then abs (Second_Rounded - Second) <= Epsilon * Second + Eta
       and then abs (Sum - (First_Rounded + Second_Rounded)) <= Epsilon * abs (First_Rounded + Second_Rounded) + Eta
       and then Sum >= Tiny,
     Post => First + Second >= Half_Tiny and then abs (Sum - (First + Second)) <= Norm_Error * (First + Second);
   --  Two rounded squares and their rounded sum. The sum is at least Tiny, so Eta is negligible.

   procedure Lemma_Norm_Is_Close (First, Second, First_Rounded, Second_Rounded, Sum : Real) is
      Exact : constant Real := First + Second;
      Inner : constant Real := First_Rounded + Second_Rounded;
   begin
      pragma Assert (abs (Inner - Exact) <= Epsilon * Exact + 2.0 * Eta);
      pragma Assert (abs Inner <= Exact * (1.0 + Epsilon) + 2.0 * Eta);
      pragma Assert (abs (Sum - Exact) <= Exact * (2.0 * Epsilon + Epsilon * Epsilon) + 4.0 * Eta);
      pragma Assert (Exact >= Half_Tiny);
      pragma Assert (4.0 * Eta <= (Epsilon - Epsilon * Epsilon) * Exact);
   end Lemma_Norm_Is_Close;

   procedure Lemma_Reciprocal_Bracket (Exact, Rounded, Low, High : Real)
   with
     Ghost,
     Pre  => Exact > 0.0 and then Low > 0.0 and then Rounded >= Exact * Low and then Rounded <= Exact * High,
     Post =>
       1.0 / Rounded > 0.0
       and then 1.0 / Exact > 0.0
       and then (1.0 / Rounded) * Low <= 1.0 / Exact
       and then (1.0 / Rounded) * High >= 1.0 / Exact;
   --  A number between Low and High times another has its reciprocal between 1/High and 1/Low times the other's.

   procedure Lemma_Reciprocal_Bracket (Exact, Rounded, Low, High : Real) is
      U : constant Real := 1.0 / Rounded;
      W : constant Real := 1.0 / Exact;
   begin
      pragma Assert (Exact * Low > 0.0);
      pragma Assert (Rounded > 0.0);
      pragma Assert (U * Rounded = 1.0);
      pragma Assert (W * Exact = 1.0);
      pragma Assert (U > 0.0 and then W > 0.0);
      pragma Assert (U * (Exact * Low) <= U * Rounded);
      pragma Assert ((U * Low - W) * Exact = U * (Exact * Low) - W * Exact);
      pragma Assert ((U * Low - W) * Exact <= 0.0);
      pragma Assert (U * Low <= W);
      pragma Assert (U * (Exact * High) >= U * Rounded);
      pragma Assert ((U * High - W) * Exact = U * (Exact * High) - W * Exact);
      pragma Assert ((U * High - W) * Exact >= 0.0);
      pragma Assert (U * High >= W);
   end Lemma_Reciprocal_Bracket;

   procedure Lemma_Reciprocal_Is_Close (Exact, Rounded, Quotient : Real)
   with
     Ghost,
     Pre  =>
       Exact >= Half_Tiny
       and then Exact <= Norm_Bound
       and then abs (Rounded - Exact) <= Norm_Error * Exact
       and then Rounded /= 0.0
       and then abs (Quotient - 1.0 / Rounded) <= Epsilon * abs (1.0 / Rounded) + Eta,
     Post => abs (Quotient - 1.0 / Exact) <= 8.0 * Epsilon * (1.0 / Exact);
   --  Rounded is |π|² as computed, Exact as it is, and Quotient the rounded reciprocal of the former: three
   --  Epsilon from the norm, one from the division, and second-order terms.

   procedure Lemma_Reciprocal_Is_Close (Exact, Rounded, Quotient : Real) is
      U : constant Real := 1.0 / Rounded;
      W : constant Real := 1.0 / Exact;
   begin
      Lemma_Reciprocal_Bracket (Exact, Rounded, 1.0 - Norm_Error, 1.0 + Norm_Error);
      Lemma_Reciprocal_Is_Large (Exact, Norm_Bound);
      pragma Assert (W >= Norm_Floor);
      pragma Assert (U * (1.0 - Norm_Error) <= W and then U * (1.0 + Norm_Error) >= W);
      pragma Assert (abs (U - W) <= Norm_Error * U);
      pragma Assert (U <= W * (1.0 + 4.0 * Epsilon));
      pragma Assert (Eta <= Epsilon * W);
      pragma Assert (abs (Quotient - U) <= Epsilon * U + Epsilon * W);
   end Lemma_Reciprocal_Is_Close;

   procedure Lemma_Pair_Reciprocal (A, B : Dimensionless)
   with
     Ghost,
     Pre  => abs A <= Magnitude_Limit and then abs B <= Magnitude_Limit and then A * A + B * B >= Unusable_Divisor,
     Post =>
       R (A) * R (A) + R (B) * R (B) > 0.0
       and then (1.0 / (A * A + B * B)) in Dimensionless
       and then
         abs (R (1.0 / (A * A + B * B)) - 1.0 / (R (A) * R (A) + R (B) * R (B)))
         <= 8.0 * Epsilon * (1.0 / (R (A) * R (A) + R (B) * R (B)));
   --  The reciprocal of |π|² as the code computes it: two squares, a sum and a division.

   procedure Lemma_Pair_Reciprocal (A, B : Dimensionless) is
      Norm : constant Dimensionless := A * A + B * B;
   begin
      Lemma_Usable_Bound (A);
      Lemma_Usable_Bound (B);
      Lemma_Real_Product_Bound (R (A), R (A), Operand_Bound);
      Lemma_Real_Product_Bound (R (B), R (B), Operand_Bound);
      Lemma_Product_Is_Bounded (A, A);
      Lemma_Product_Is_Bounded (B, B);
      Float_Lemmas.Lemma_Rounding_Error_Add (A * A, B * B);
      Lemma_Order (Unusable_Divisor, Norm);
      pragma Assert (R (A) * R (A) >= 0.0 and then R (B) * R (B) >= 0.0);
      Lemma_Norm_Is_Close (R (A) * R (A), R (B) * R (B), R (A * A), R (B * B), R (Norm));
      Float_Lemmas.Lemma_Rounding_Error_Div (1.0, Norm);
      Lemma_Reciprocal_Is_Close (R (A) * R (A) + R (B) * R (B), R (Norm), R (1.0 / Norm));
   end Lemma_Pair_Reciprocal;

   procedure Lemma_Accepted_Square (Square : Dimensionless)
   with
     Ghost,
     Pre  => Square >= Unusable_Divisor and then abs (1.0 / Square) <= Magnitude_Limit,
     Post => R (Square) >= Smallest_Distance_Square and then R (Square) >= Tiny;
   --  A squared distance whose rounded reciprocal is within Magnitude_Limit is at least Smallest_Distance_Square.

   procedure Lemma_Accepted_Square (Square : Dimensionless) is
   begin
      Lemma_Order (Unusable_Divisor, Square);
      Float_Lemmas.Lemma_Rounding_Error_Div (1.0, Square);
      Lemma_Usable_Bound (1.0 / Square);
      Lemma_Reciprocal_Is_Small (R (Square), Tiny);
      pragma Assert (1.0 / R (Square) <= Operand_Bound);
      pragma Assert ((1.0 / R (Square)) * R (Square) <= Operand_Bound * R (Square));
   end Lemma_Accepted_Square;

   ---------------------------------------------------------------------------
   --  One pole's share of the largest derivative
   ---------------------------------------------------------------------------

   Numerator_Limit : constant Dimensionless := 2.0**202
   with Ghost;
   Numerator_Bound : constant := 2.0**202;
   --  No raised sum of the magnitudes of two usable residues is larger.

   Quotient_Bound : constant := 2.0**402;
   --  Numerator_Bound divided by Smallest_Distance_Square.

   Increment_Limit : constant Dimensionless := 2.0**404
   with Ghost;
   Increment_Bound : constant := 2.0**404;
   --  No pole adds more to Slope, before the rounding of that addition.

   Slope_Step : constant := 2.0**405;
   --  No pole adds more to Slope.

   Slope_Ceiling : constant Dimensionless := 2.0**411
   with Ghost;
   Slope_Bound   : constant := 2.0**411;
   --  More than sixteen times Slope_Step, and less than Largest_Slope.

   procedure Lemma_Slope_Numerator (Real_Residue, Imaginary_Residue : Dimensionless)
   with
     Ghost,
     Pre  => abs Real_Residue <= Magnitude_Limit and then abs Imaginary_Residue <= Magnitude_Limit,
     Post =>
       abs (abs Real_Residue + abs Imaginary_Residue) <= Numerator_Limit
       and then Up (abs Real_Residue + abs Imaginary_Residue) >= 0.0
       and then Up (abs Real_Residue + abs Imaginary_Residue) <= Numerator_Limit
       and then
         R (Up (abs Real_Residue + abs Imaginary_Residue)) >= abs (R (Real_Residue)) + abs (R (Imaginary_Residue));
   --  The raised sum of the two magnitudes is at least their exact sum.

   procedure Lemma_Slope_Numerator (Real_Residue, Imaginary_Residue : Dimensionless) is
      Sum : constant Dimensionless := abs Real_Residue + abs Imaginary_Residue;
   begin
      Lemma_Usable_Bound (Real_Residue);
      Lemma_Usable_Bound (Imaginary_Residue);
      Lemma_Abs_Is_Exact (Real_Residue);
      Lemma_Abs_Is_Exact (Imaginary_Residue);
      Float_Lemmas.Lemma_Rounding_Error_Add (abs Real_Residue, abs Imaginary_Residue);
      pragma Assert (abs (R (Sum)) <= 2.0 * Limit_Bound * (1.0 + Epsilon) + Eta);
      pragma Assert (abs Sum <= Numerator_Limit);
      Lemma_Raised (Sum, abs (R (Real_Residue)) + abs (R (Imaginary_Residue)));
      pragma Assert (R (Up (Sum)) >= 0.0);
      pragma Assert (R (Up (Sum)) <= Numerator_Bound);
   end Lemma_Slope_Numerator;

   procedure Lemma_Slope_Quotient (Numerator, Square : Dimensionless; Exact, Distance : Real)
   with
     Ghost,
     Pre  =>
       Numerator >= 0.0
       and then Numerator <= Numerator_Limit
       and then Square >= Unusable_Divisor
       and then R (Square) >= Smallest_Distance_Square
       and then R (Square) <= Distance
       and then Exact >= 0.0
       and then Exact <= R (Numerator),
     Post =>
       abs (Numerator / Square) <= Increment_Limit
       and then Up (Numerator / Square) >= 0.0
       and then Up (Numerator / Square) <= Increment_Limit
       and then R (Up (Numerator / Square)) >= Exact / Distance;
   --  The raised quotient by a squared distance which is not too large is at least the exact quotient by the
   --  true one.

   procedure Lemma_Slope_Quotient (Numerator, Square : Dimensionless; Exact, Distance : Real) is
   begin
      Lemma_Order (0.0, Numerator);
      Lemma_Order (Numerator, Numerator_Limit);
      Lemma_Quotient_Is_Monotonic (R (Numerator), Numerator_Bound, R (Square), Smallest_Distance_Square);
      pragma Assert (Numerator_Bound / Smallest_Distance_Square = Quotient_Bound);
      pragma Assert (R (Numerator) / R (Square) <= Quotient_Bound);
      Lemma_Quotient_Is_Monotonic (Exact, R (Numerator), Distance, R (Square));
      pragma Assert (R (Numerator) / R (Square) >= 0.0);
      Float_Lemmas.Lemma_Rounding_Error_Div (Numerator, Square);
      pragma Assert (abs (R (Numerator / Square)) <= Quotient_Bound * (1.0 + Epsilon) + Eta);
      pragma Assert (abs (Numerator / Square) <= Increment_Limit);
      Lemma_Raised (Numerator / Square, R (Numerator) / R (Square));
      pragma Assert (R (Up (Numerator / Square)) >= 0.0);
      pragma Assert (R (Up (Numerator / Square)) <= Increment_Bound);
   end Lemma_Slope_Quotient;

   procedure Lemma_Slope_Increment (Slope, Increment : Dimensionless)
   with
     Ghost,
     Pre  =>
       Slope >= 0.0 and then Slope <= Slope_Ceiling and then Increment >= 0.0 and then Increment <= Increment_Limit,
     Post =>
       abs (Slope + Increment) <= Half_Last
       and then Up (Slope + Increment) >= 0.0
       and then R (Up (Slope + Increment)) >= R (Slope) + R (Increment)
       and then R (Up (Slope + Increment)) <= R (Slope) + Slope_Step;
   --  Adding one pole's share to Slope, rounded upward, adds at least the share and at most Slope_Step.

   procedure Lemma_Slope_Increment (Slope, Increment : Dimensionless) is
   begin
      Lemma_Order (0.0, Slope);
      Lemma_Order (Slope, Slope_Ceiling);
      Lemma_Order (0.0, Increment);
      Lemma_Order (Increment, Increment_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Add (Slope, Increment);
      pragma Assert (abs (R (Slope + Increment)) <= (Slope_Bound + Increment_Bound) * (1.0 + Epsilon) + Eta);
      pragma Assert (abs (Slope + Increment) <= Half_Last);
      Lemma_Raised (Slope + Increment, R (Slope) + R (Increment));
      pragma Assert (R (Up (Slope + Increment)) >= 0.0);
   end Lemma_Slope_Increment;

   procedure Lemma_Scaled (Factor, Value : Dimensionless)
   with
     Ghost,
     Pre  => Factor = 1.0 or else (Factor = 2.0 and then abs Value <= Dimensionless'Last / 2.0),
     Post =>
       (Factor * Value) in Dimensionless
       and then (if Factor * Value = 0.0 then Value = 0.0)
       and then
         (if Factor = 1.0
          then R (Factor * Value) = R (Value)
          else abs (R (Factor * Value) - 2.0 * R (Value)) <= 2.0 * Epsilon * abs (R (Value)) + Eta);
   --  A residue times one or two: finite, zero only if the residue is, and the residue itself or its double as
   --  one multiplication rounds it.

   procedure Lemma_Scaled (Factor, Value : Dimensionless) is
   begin
      if Factor = 1.0 then
         Lemma_Unit_Is_Exact (Value);
      else
         Lemma_Doubling_Is_Rounded (Value);
         Lemma_Doubling_Keeps_Zero (Value);
      end if;
   end Lemma_Scaled;

   procedure Lemma_Horizontal (Pole, Horizontal : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs Pole <= Magnitude_Limit
       and then
         (if Pole < 0.0
          then R (Horizontal) = 0.0 - R (Pole)
          elsif Pole > 1.0
          then R (Horizontal) = R (Pole - 1.0)
          else R (Horizontal) = 0.0),
     Post =>
       Horizontal >= 0.0
       and then Horizontal <= Operand_Limit
       and then
         abs (R (Horizontal) - Horizontal_Distance (R (Pole))) <= Epsilon * Horizontal_Distance (R (Pole)) + Eta;
   --  The distance of a real part from [0, 1] as the code computes it: exact below zero, one rounded subtraction
   --  above one.

   procedure Lemma_Horizontal (Pole, Horizontal : Dimensionless) is
   begin
      Lemma_Usable_Bound (Pole);
      if Pole < 0.0 then
         Lemma_Order (Pole, 0.0);
         pragma Assert (R (Horizontal) = Horizontal_Distance (R (Pole)));
      elsif Pole > 1.0 then
         Float_Lemmas.Lemma_Rounding_Error_Sub (Pole, 1.0);
         Lemma_Order (1.0, Pole);
         pragma Assert (Horizontal_Distance (R (Pole)) = R (Pole) - 1.0);
         pragma Assert (R (Horizontal) >= 0.0);
      else
         Lemma_Order (0.0, Pole);
         Lemma_Order (Pole, 1.0);
         pragma Assert (Horizontal_Distance (R (Pole)) = 0.0);
      end if;
      pragma Assert (R (Horizontal) >= 0.0);
      pragma Assert (R (Horizontal) <= Operand_Bound);
   end Lemma_Horizontal;

   ---------------------------------------------------------------------------
   --  One gathered pole
   ---------------------------------------------------------------------------

   function Slope_Cap (Count : Natural) return Real
   is (To_Real (Count) * Slope_Step)
   with Ghost;
   --  No Slope is larger after Count poles.

   procedure Lemma_Slope_Cap (Count : Natural)
   with
     Ghost,
     Pre  => Count <= Maximum_Rational_Degree,
     Post =>
       Slope_Cap (Count) >= 0.0
       and then Slope_Cap (Count) <= 16.0 * Slope_Step
       and then (if Count < Maximum_Rational_Degree then Slope_Cap (Count) <= 15.0 * Slope_Step)
       and then (if Count < Maximum_Rational_Degree then Slope_Cap (Count + 1) = Slope_Cap (Count) + Slope_Step);

   procedure Lemma_Slope_Cap (Count : Natural) is
   begin
      pragma Assert (To_Real (Count) >= 0.0 and then To_Real (Count) <= 16.0);
      if Count < Maximum_Rational_Degree then
         pragma Assert (To_Real (Count) <= 15.0);
         pragma Assert (To_Real (Count + 1) = To_Real (Count) + 1.0);
      end if;
   end Lemma_Slope_Cap;

   procedure Lemma_Slope_Is_Within_Ceiling (Slope : Dimensionless)
   with
     Ghost,
     Pre  => Slope >= 0.0 and then R (Slope) <= 16.0 * Slope_Step,
     Post => Slope <= Slope_Ceiling and then Slope <= Largest_Slope;

   procedure Lemma_Slope_Is_Within_Ceiling (Slope : Dimensionless) is null;

   function Is_Residue_Gathered
     (Cache                           : Rational_Antiderivative;
      Real_Slot                       : Rational_Degree_Slot;
      Imaginary_Slot                  : Natural;
      Real_Residue, Imaginary_Residue : Dimensionless;
      Axis                            : Axis_Name) return Boolean
   is (abs Real_Residue <= Magnitude_Limit
       and then abs Imaginary_Residue <= Magnitude_Limit
       and then
         (if Imaginary_Slot = 0
          then
            R (Real_Residue) = R (Cache.Pole_Slots (Real_Slot).Residue_Component (Axis))
            and then Imaginary_Residue = 0.0
          else
            abs (R (Real_Residue) - 2.0 * R (Cache.Pole_Slots (Real_Slot).Residue_Component (Axis)))
            <= 2.0 * Epsilon * abs R (Cache.Pole_Slots (Real_Slot).Residue_Component (Axis)) + Eta
            and then
              abs (R (Imaginary_Residue) - 2.0 * R (Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Axis)))
              <= 2.0 * Epsilon * abs R (Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Axis)) + Eta))
   with Ghost, Pre => Imaginary_Slot <= Maximum_Rational_Degree;
   --  The two numbers are a term's residue on one axis: as stored for a real pole, and doubled, as that
   --  multiplication rounds, for a pair.

   function Is_Idle_Residue_Zero
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Active         : Boolean;
      Axis           : Axis_Name) return Boolean
   is (if not Active
       then
         Cache.Pole_Slots (Real_Slot).Residue_Component (Axis) = 0.0
         and then (if Imaginary_Slot /= 0 then Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Axis) = 0.0))
   with Ghost, Pre => Imaginary_Slot <= Maximum_Rational_Degree;
   --  The pole has no residue on an axis which is not active.

   function Pole_Distance_Square (Real_Part, Imaginary_Part : Dimensionless) return Real
   is (Horizontal_Distance (R (Real_Part)) * Horizontal_Distance (R (Real_Part))
       + R (Imaginary_Part) * R (Imaginary_Part))
   with Ghost;
   --  Distance_Square of a term with this pole.

   function Is_Reciprocal_Gathered
     (Imaginary_Slot : Natural; Real_Part, Imaginary_Part, Reciprocal_Norm : Dimensionless) return Boolean
   is (if Imaginary_Slot = 0
       then
         R (Real_Part) /= 0.0
         and then abs (R (Reciprocal_Norm) - 1.0 / R (Real_Part)) <= 2.0 * Epsilon * abs (1.0 / R (Real_Part))
       else
         R (Real_Part) * R (Real_Part) + R (Imaginary_Part) * R (Imaginary_Part) > 0.0
         and then
           abs (R (Reciprocal_Norm) - 1.0 / (R (Real_Part) * R (Real_Part) + R (Imaginary_Part) * R (Imaginary_Part)))
           <= 8.0 * Epsilon * (1.0 / (R (Real_Part) * R (Real_Part) + R (Imaginary_Part) * R (Imaginary_Part))))
   with Ghost;
   --  The third number is the reciprocal of a real pole, or of the squared modulus of a pair, as its few
   --  operations round it.

   function Is_Pole_Gathered
     (Cache                                      : Rational_Antiderivative;
      Real_Slot                                  : Rational_Degree_Slot;
      Imaginary_Slot                             : Natural;
      Real_Part, Imaginary_Part, Reciprocal_Norm : Dimensionless) return Boolean
   is (R (Real_Part) = R (Cache.Pole_Slots (Real_Slot).Pole_Component)
       and then
         (if Imaginary_Slot = 0
          then Imaginary_Part = 0.0 and then (Real_Part < 0.0 or else Real_Part > 1.0)
          else R (Imaginary_Part) = R (Cache.Pole_Slots (Imaginary_Slot).Pole_Component) and then Imaginary_Part > 0.0)
       and then Is_Reciprocal_Gathered (Imaginary_Slot, Real_Part, Imaginary_Part, Reciprocal_Norm))
   with Ghost, Pre => Imaginary_Slot <= Maximum_Rational_Degree;
   --  The three numbers are the pole of the given slots and the reciprocal of its norm: what Is_Real_Term and
   --  Is_Pair_Term say of them, with the slots given outright.

   function Is_Term_Gathered
     (Cache : Rational_Antiderivative; Real_Slot : Rational_Degree_Slot; Imaginary_Slot : Natural; Term : Pole_Term)
      return Boolean
   is (Is_Term_Bounded (Term)
       and then
         Is_Pole_Gathered (Cache, Real_Slot, Imaginary_Slot, Term.Real_Part, Term.Imaginary_Part, Term.Reciprocal_Norm)
       and then
         (for all Axis in Axis_Name =>
            Is_Residue_Gathered
              (Cache, Real_Slot, Imaginary_Slot, Term.Residue_Real (Axis), Term.Residue_Imaginary (Axis), Axis)))
   with Ghost, Pre => Imaginary_Slot <= Maximum_Rational_Degree;
   --  Term is the pole of the given slots: bounded, and as Is_Real_Term or Is_Pair_Term describe it.

   function Is_Slope_Grown (Before, After : Dimensionless; Term : Pole_Term; Axis : Axis_Name) return Boolean
   is (After >= 0.0
       and then R (After) <= R (Before) + Slope_Step
       and then R (After) >= R (Before) + Slope_Term (Term, Axis))
   with Ghost;
   --  Slope has grown by at least the term's share on one axis, and by at most Slope_Step.

   function Is_Continuation (Before, After : Preparation) return Boolean
   is ((for all Axis in Axis_Name => (if Before.Active (Axis) then After.Active (Axis)))
       and then
         (for all Component in Chart_Component_Index =>
            R (After.Delta_Y (Component)) = R (Before.Delta_Y (Component))
            and then After.Vanishes (Component) = Before.Vanishes (Component)))
   with Ghost;
   --  What Append leaves alone: it only ever switches an axis on, and does not touch the chart's data.

   function Same_Term (Left, Right : Pole_Term) return Boolean
   is (R (Left.Real_Part) = R (Right.Real_Part)
       and then R (Left.Imaginary_Part) = R (Right.Imaginary_Part)
       and then R (Left.Reciprocal_Norm) = R (Right.Reciprocal_Norm)
       and then
         (for all Axis in Axis_Name =>
            R (Left.Residue_Real (Axis)) = R (Right.Residue_Real (Axis))
            and then R (Left.Residue_Imaginary (Axis)) = R (Right.Residue_Imaginary (Axis))))
   with Ghost;
   --  Whether two terms denote the same numbers. Stated over the real numbers, because equality of
   --  floating-point numbers is of little use to the provers.

   function Same_Terms (Left, Right : Pole_Terms; Count : Natural) return Boolean
   is (for all Index in 1 .. Count => Same_Term (Left (Index), Right (Index)))
   with Ghost, Pre => Count <= Maximum_Rational_Degree;
   --  Whether the first Count terms denote the same poles and residues.

   function Is_Appended
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Before, After  : Preparation) return Boolean
   is (After.Count = Before.Count + 1
       and then Same_Terms (Before.Terms, After.Terms, Before.Count)
       and then Is_Continuation (Before, After)
       and then Is_Term_Gathered (Cache, Real_Slot, Imaginary_Slot, After.Terms (After.Count))
       and then
         (for all Axis in Axis_Name =>
            Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, After.Active (Axis), Axis)
            and then Is_Slope_Grown (Before.Slope (Axis), After.Slope (Axis), After.Terms (After.Count), Axis)))
   with Ghost, Pre => Imaginary_Slot <= Maximum_Rational_Degree and then Before.Count < Maximum_Rational_Degree;
   --  What one successful call of Append does: the earlier terms stay, the pole of the given slots becomes the
   --  next term, every Slope grows by its share, and an axis stays inactive only if the pole has no residue on it.

   function Is_Axis_Untouched (Before_Active, Active : Boolean; Before, After : Dimensionless) return Boolean
   is (Active = Before_Active
       and then After >= 0.0
       and then R (After) = R (Before)
       and then R (After) <= 15.0 * Slope_Step)
   with Ghost;
   --  What holds of an axis which Append has not come to yet: its flag and its Slope are as they were, and
   --  Slope leaves room for one more pole.

   function Is_Axis_Gathered
     (Cache                           : Rational_Antiderivative;
      Real_Slot                       : Rational_Degree_Slot;
      Imaginary_Slot                  : Natural;
      Real_Residue, Imaginary_Residue : Dimensionless;
      Before_Active, Active           : Boolean;
      Before, After                   : Dimensionless;
      Distance                        : Real;
      Axis                            : Axis_Name) return Boolean
   is (Is_Residue_Gathered (Cache, Real_Slot, Imaginary_Slot, Real_Residue, Imaginary_Residue, Axis)
       and then Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, Active, Axis)
       and then (if Before_Active then Active)
       and then After >= 0.0
       and then R (After) <= R (Before) + Slope_Step
       and then R (After) >= R (Before) + (abs (R (Real_Residue)) + abs (R (Imaginary_Residue))) / Distance)
   with Ghost, Pre => Imaginary_Slot <= Maximum_Rational_Degree and then Distance > 0.0;
   --  What Append has done for one axis: the residue is in the term, the axis is active unless the residue is
   --  zero, and Slope, which was Before, is now After.

   package Carried
     with Ghost
   is
      --  What Append carries through its loop over the axes without looking inside. The definitions are in the
      --  body of this package, where only the lemmas below see them: stated outright in Append, they would be
      --  part of every check of the loop, which the provers do not withstand.

      function Pole_Fact
        (Cache                                      : Rational_Antiderivative;
         Real_Slot                                  : Rational_Degree_Slot;
         Imaginary_Slot                             : Natural;
         Real_Part, Imaginary_Part, Reciprocal_Norm : Dimensionless;
         Distance                                   : Real) return Boolean
      with Pre => Imaginary_Slot <= Maximum_Rational_Degree;
      --  The pole is usable, it is the pole of the given slots, and Distance is its squared distance from the
      --  curve, which is at least Smallest_Distance_Square.

      procedure Lemma_Fold_Pole
        (Cache                                      : Rational_Antiderivative;
         Real_Slot                                  : Rational_Degree_Slot;
         Imaginary_Slot                             : Natural;
         Real_Part, Imaginary_Part, Reciprocal_Norm : Dimensionless;
         Distance                                   : Real)
      with
        Pre  =>
          Imaginary_Slot <= Maximum_Rational_Degree
          and then abs Real_Part <= Magnitude_Limit
          and then Imaginary_Part >= 0.0
          and then Imaginary_Part <= Magnitude_Limit
          and then abs Reciprocal_Norm <= Magnitude_Limit
          and then Is_Pole_Gathered (Cache, Real_Slot, Imaginary_Slot, Real_Part, Imaginary_Part, Reciprocal_Norm)
          and then Distance = Pole_Distance_Square (Real_Part, Imaginary_Part)
          and then Distance >= Smallest_Distance_Square,
        Post => Pole_Fact (Cache, Real_Slot, Imaginary_Slot, Real_Part, Imaginary_Part, Reciprocal_Norm, Distance);

      procedure Lemma_Unfold_Pole
        (Cache                                      : Rational_Antiderivative;
         Real_Slot                                  : Rational_Degree_Slot;
         Imaginary_Slot                             : Natural;
         Real_Part, Imaginary_Part, Reciprocal_Norm : Dimensionless;
         Distance                                   : Real)
      with
        Pre  =>
          Imaginary_Slot <= Maximum_Rational_Degree
          and then Pole_Fact (Cache, Real_Slot, Imaginary_Slot, Real_Part, Imaginary_Part, Reciprocal_Norm, Distance),
        Post =>
          abs Real_Part <= Magnitude_Limit
          and then Imaginary_Part >= 0.0
          and then Imaginary_Part <= Magnitude_Limit
          and then abs Reciprocal_Norm <= Magnitude_Limit
          and then Is_Pole_Gathered (Cache, Real_Slot, Imaginary_Slot, Real_Part, Imaginary_Part, Reciprocal_Norm)
          and then Distance = Pole_Distance_Square (Real_Part, Imaginary_Part)
          and then Distance >= Smallest_Distance_Square;

      function Axis_Fact
        (Cache                           : Rational_Antiderivative;
         Real_Slot                       : Rational_Degree_Slot;
         Imaginary_Slot                  : Natural;
         Real_Residue, Imaginary_Residue : Dimensionless;
         Before_Active, Active           : Boolean;
         Before, After                   : Dimensionless;
         Distance                        : Real;
         Axis                            : Axis_Name;
         Done                            : Boolean) return Boolean
      with Pre => Imaginary_Slot <= Maximum_Rational_Degree and then Distance > 0.0;
      --  One axis during Append's loop: Is_Axis_Gathered once the loop has come to it, Is_Axis_Untouched until
      --  then.

      procedure Lemma_Fold_Untouched
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Term           : Pole_Term;
         Before         : Preparation;
         Distance       : Real)
      with
        Pre  =>
          Imaginary_Slot <= Maximum_Rational_Degree
          and then Distance > 0.0
          and then
            (for all A in Axis_Name => Before.Slope (A) >= 0.0 and then R (Before.Slope (A)) <= 15.0 * Slope_Step),
        Post =>
          (for all A in Axis_Name =>
             Axis_Fact
               (Cache,
                Real_Slot,
                Imaginary_Slot,
                Term.Residue_Real (A),
                Term.Residue_Imaginary (A),
                Before.Active (A),
                Before.Active (A),
                Before.Slope (A),
                Before.Slope (A),
                Distance,
                A,
                False));
      --  Before the loop no axis has been touched.

      procedure Lemma_Unfold_Untouched
        (Cache                           : Rational_Antiderivative;
         Real_Slot                       : Rational_Degree_Slot;
         Imaginary_Slot                  : Natural;
         Real_Residue, Imaginary_Residue : Dimensionless;
         Before_Active, Active           : Boolean;
         Before, After                   : Dimensionless;
         Distance                        : Real;
         Axis                            : Axis_Name)
      with
        Pre  =>
          Imaginary_Slot <= Maximum_Rational_Degree
          and then Distance > 0.0
          and then
            Axis_Fact
              (Cache,
               Real_Slot,
               Imaginary_Slot,
               Real_Residue,
               Imaginary_Residue,
               Before_Active,
               Active,
               Before,
               After,
               Distance,
               Axis,
               False),
        Post => Is_Axis_Untouched (Before_Active, Active, Before, After);

      procedure Lemma_Fold_Gathered
        (Cache                             : Rational_Antiderivative;
         Real_Slot                         : Rational_Degree_Slot;
         Imaginary_Slot                    : Natural;
         Real_Residue, Imaginary_Residue   : Dimensionless;
         Before_Active, Old_Active, Active : Boolean;
         Before, Old_Slope, After          : Dimensionless;
         Distance                          : Real;
         Axis                              : Axis_Name)
      with
        Pre  =>
          Imaginary_Slot <= Maximum_Rational_Degree
          and then Distance > 0.0
          and then Is_Residue_Gathered (Cache, Real_Slot, Imaginary_Slot, Real_Residue, Imaginary_Residue, Axis)
          and then Is_Axis_Untouched (Before_Active, Old_Active, Before, Old_Slope)
          and then
            (if Real_Residue = 0.0 and then Imaginary_Residue = 0.0
             then Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, False, Axis))
          and then
            (if Real_Residue /= 0.0 or else Imaginary_Residue /= 0.0
             then
               Active
               and then After >= 0.0
               and then R (After) <= R (Old_Slope) + Slope_Step
               and then R (After) >= R (Old_Slope) + (abs (R (Real_Residue)) + abs (R (Imaginary_Residue))) / Distance
             else Active = Old_Active and then After >= 0.0 and then R (After) = R (Old_Slope)),
        Post =>
          Axis_Fact
            (Cache,
             Real_Slot,
             Imaginary_Slot,
             Real_Residue,
             Imaginary_Residue,
             Before_Active,
             Active,
             Before,
             After,
             Distance,
             Axis,
             True);
      --  What one pass of Append's loop does for its axis. Old_Active and Old_Slope are the axis' flag and Slope
      --  when the pass began, Active and After when it ended.

      procedure Lemma_Step
        (Cache                          : Rational_Antiderivative;
         Real_Slot                      : Rational_Degree_Slot;
         Imaginary_Slot                 : Natural;
         Old_Term, Term                 : Pole_Term;
         Before, Old_Prepared, Prepared : Preparation;
         Distance                       : Real;
         Axis                           : Axis_Name)
      with
        Pre  =>
          Imaginary_Slot <= Maximum_Rational_Degree
          and then Distance > 0.0
          and then
            (for all A in Axis_Name =>
               Axis_Fact
                 (Cache,
                  Real_Slot,
                  Imaginary_Slot,
                  Old_Term.Residue_Real (A),
                  Old_Term.Residue_Imaginary (A),
                  Before.Active (A),
                  Old_Prepared.Active (A),
                  Before.Slope (A),
                  Old_Prepared.Slope (A),
                  Distance,
                  A,
                  A < Axis))
          and then
            Axis_Fact
              (Cache,
               Real_Slot,
               Imaginary_Slot,
               Term.Residue_Real (Axis),
               Term.Residue_Imaginary (Axis),
               Before.Active (Axis),
               Prepared.Active (Axis),
               Before.Slope (Axis),
               Prepared.Slope (Axis),
               Distance,
               Axis,
               True)
          and then
            (for all A in Axis_Name =>
               (if A /= Axis
                then
                  R (Term.Residue_Real (A)) = R (Old_Term.Residue_Real (A))
                  and then R (Term.Residue_Imaginary (A)) = R (Old_Term.Residue_Imaginary (A))
                  and then Prepared.Active (A) = Old_Prepared.Active (A)
                  and then R (Prepared.Slope (A)) = R (Old_Prepared.Slope (A)))),
        Post =>
          (for all A in Axis_Name =>
             Axis_Fact
               (Cache,
                Real_Slot,
                Imaginary_Slot,
                Term.Residue_Real (A),
                Term.Residue_Imaginary (A),
                Before.Active (A),
                Prepared.Active (A),
                Before.Slope (A),
                Prepared.Slope (A),
                Distance,
                A,
                A <= Axis));
      --  One pass of the loop: the term and the preparation were Old_Term and Old_Prepared before it, and it
      --  changed them on its own axis only.

      procedure Lemma_Unfold_Gathered
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Term           : Pole_Term;
         Before, After  : Preparation;
         Distance       : Real)
      with
        Pre  =>
          Imaginary_Slot <= Maximum_Rational_Degree
          and then Distance > 0.0
          and then
            (for all A in Axis_Name =>
               Axis_Fact
                 (Cache,
                  Real_Slot,
                  Imaginary_Slot,
                  Term.Residue_Real (A),
                  Term.Residue_Imaginary (A),
                  Before.Active (A),
                  After.Active (A),
                  Before.Slope (A),
                  After.Slope (A),
                  Distance,
                  A,
                  True)),
        Post =>
          (for all A in Axis_Name =>
             Is_Axis_Gathered
               (Cache,
                Real_Slot,
                Imaginary_Slot,
                Term.Residue_Real (A),
                Term.Residue_Imaginary (A),
                Before.Active (A),
                After.Active (A),
                Before.Slope (A),
                After.Slope (A),
                Distance,
                A));
      --  After the loop every axis has been gathered.

   end Carried;

   package body Carried is

      function Pole_Fact
        (Cache                                      : Rational_Antiderivative;
         Real_Slot                                  : Rational_Degree_Slot;
         Imaginary_Slot                             : Natural;
         Real_Part, Imaginary_Part, Reciprocal_Norm : Dimensionless;
         Distance                                   : Real) return Boolean
      is (abs Real_Part <= Magnitude_Limit
          and then Imaginary_Part >= 0.0
          and then Imaginary_Part <= Magnitude_Limit
          and then abs Reciprocal_Norm <= Magnitude_Limit
          and then Is_Pole_Gathered (Cache, Real_Slot, Imaginary_Slot, Real_Part, Imaginary_Part, Reciprocal_Norm)
          and then Distance = Pole_Distance_Square (Real_Part, Imaginary_Part)
          and then Distance >= Smallest_Distance_Square);

      procedure Lemma_Fold_Pole
        (Cache                                      : Rational_Antiderivative;
         Real_Slot                                  : Rational_Degree_Slot;
         Imaginary_Slot                             : Natural;
         Real_Part, Imaginary_Part, Reciprocal_Norm : Dimensionless;
         Distance                                   : Real)
      is null;

      procedure Lemma_Unfold_Pole
        (Cache                                      : Rational_Antiderivative;
         Real_Slot                                  : Rational_Degree_Slot;
         Imaginary_Slot                             : Natural;
         Real_Part, Imaginary_Part, Reciprocal_Norm : Dimensionless;
         Distance                                   : Real)
      is null;

      function Axis_Fact
        (Cache                           : Rational_Antiderivative;
         Real_Slot                       : Rational_Degree_Slot;
         Imaginary_Slot                  : Natural;
         Real_Residue, Imaginary_Residue : Dimensionless;
         Before_Active, Active           : Boolean;
         Before, After                   : Dimensionless;
         Distance                        : Real;
         Axis                            : Axis_Name;
         Done                            : Boolean) return Boolean
      is (if Done
          then
            Is_Axis_Gathered
              (Cache,
               Real_Slot,
               Imaginary_Slot,
               Real_Residue,
               Imaginary_Residue,
               Before_Active,
               Active,
               Before,
               After,
               Distance,
               Axis)
          else Is_Axis_Untouched (Before_Active, Active, Before, After));

      procedure Lemma_Fold_Untouched
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Term           : Pole_Term;
         Before         : Preparation;
         Distance       : Real)
      is null;

      procedure Lemma_Unfold_Untouched
        (Cache                           : Rational_Antiderivative;
         Real_Slot                       : Rational_Degree_Slot;
         Imaginary_Slot                  : Natural;
         Real_Residue, Imaginary_Residue : Dimensionless;
         Before_Active, Active           : Boolean;
         Before, After                   : Dimensionless;
         Distance                        : Real;
         Axis                            : Axis_Name)
      is null;

      procedure Lemma_Fold_Gathered
        (Cache                             : Rational_Antiderivative;
         Real_Slot                         : Rational_Degree_Slot;
         Imaginary_Slot                    : Natural;
         Real_Residue, Imaginary_Residue   : Dimensionless;
         Before_Active, Old_Active, Active : Boolean;
         Before, Old_Slope, After          : Dimensionless;
         Distance                          : Real;
         Axis                              : Axis_Name) is
      begin
         if Real_Residue /= 0.0 or else Imaginary_Residue /= 0.0 then
            pragma Assert (Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, Active, Axis));
         else
            pragma Assert (R (Real_Residue) = 0.0 and then R (Imaginary_Residue) = 0.0);
            pragma Assert (abs (R (Real_Residue)) + abs (R (Imaginary_Residue)) = 0.0);
            pragma Assert ((abs (R (Real_Residue)) + abs (R (Imaginary_Residue))) / Distance = 0.0);
            pragma Assert (Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, Active, Axis));
         end if;
         pragma Assert (R (After) <= R (Before) + Slope_Step);
         pragma Assert (R (After) >= R (Before) + (abs (R (Real_Residue)) + abs (R (Imaginary_Residue))) / Distance);
         pragma
           Assert
             (Is_Axis_Gathered
                (Cache,
                 Real_Slot,
                 Imaginary_Slot,
                 Real_Residue,
                 Imaginary_Residue,
                 Before_Active,
                 Active,
                 Before,
                 After,
                 Distance,
                 Axis));
      end Lemma_Fold_Gathered;

      procedure Lemma_Step
        (Cache                          : Rational_Antiderivative;
         Real_Slot                      : Rational_Degree_Slot;
         Imaginary_Slot                 : Natural;
         Old_Term, Term                 : Pole_Term;
         Before, Old_Prepared, Prepared : Preparation;
         Distance                       : Real;
         Axis                           : Axis_Name) is
      begin
         for A in Axis_Name loop
            if A /= Axis then
               Lemma_Same_Float (Term.Residue_Real (A), Old_Term.Residue_Real (A));
               Lemma_Same_Float (Term.Residue_Imaginary (A), Old_Term.Residue_Imaginary (A));
               Lemma_Same_Float (Prepared.Slope (A), Old_Prepared.Slope (A));
               pragma
                 Assert
                   (Axis_Fact
                      (Cache,
                       Real_Slot,
                       Imaginary_Slot,
                       Old_Term.Residue_Real (A),
                       Old_Term.Residue_Imaginary (A),
                       Before.Active (A),
                       Old_Prepared.Active (A),
                       Before.Slope (A),
                       Old_Prepared.Slope (A),
                       Distance,
                       A,
                       A < Axis));
            end if;
            pragma
              Assert
                (Axis_Fact
                   (Cache,
                    Real_Slot,
                    Imaginary_Slot,
                    Term.Residue_Real (A),
                    Term.Residue_Imaginary (A),
                    Before.Active (A),
                    Prepared.Active (A),
                    Before.Slope (A),
                    Prepared.Slope (A),
                    Distance,
                    A,
                    A <= Axis));
            pragma
              Loop_Invariant
                (for all B in Axis_Name =>
                   (if B <= A
                    then
                      Axis_Fact
                        (Cache,
                         Real_Slot,
                         Imaginary_Slot,
                         Term.Residue_Real (B),
                         Term.Residue_Imaginary (B),
                         Before.Active (B),
                         Prepared.Active (B),
                         Before.Slope (B),
                         Prepared.Slope (B),
                         Distance,
                         B,
                         B <= Axis)));
         end loop;
      end Lemma_Step;

      procedure Lemma_Unfold_Gathered
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Term           : Pole_Term;
         Before, After  : Preparation;
         Distance       : Real)
      is null;

   end Carried;

   ---------------------------------------------------------------------------
   --  The steps of Append
   ---------------------------------------------------------------------------

   procedure Lemma_Reciprocal_Is_Finite (Square : Dimensionless)
   with Ghost, Post => (if Square >= Unusable_Divisor then (1.0 / Square) in Dimensionless);
   --  The reciprocal of a number which is not tiny does not overflow.

   procedure Lemma_Reciprocal_Is_Finite (Square : Dimensionless) is
   begin
      if Square >= Unusable_Divisor then
         Lemma_Order (Unusable_Divisor, Square);
         Lemma_Reciprocal_Is_Small (R (Square), Tiny);
         pragma Assert (1.0 / R (Square) <= Square_Root_Of_Last);
      end if;
   end Lemma_Reciprocal_Is_Finite;

   procedure Lemma_Is_Outside (Real_Part, Imaginary_Part : Dimensionless; Distance : Real)
   with
     Ghost,
     Pre  => Distance > 0.0 and then Distance = Pole_Distance_Square (Real_Part, Imaginary_Part),
     Post => (if Imaginary_Part = 0.0 then Real_Part < 0.0 or else Real_Part > 1.0);
   --  A real pole at a positive distance from [0, 1] is outside it.

   procedure Lemma_Is_Outside (Real_Part, Imaginary_Part : Dimensionless; Distance : Real) is
   begin
      if Imaginary_Part = 0.0 then
         pragma Assert (R (Imaginary_Part) = 0.0);
         pragma Assert (Horizontal_Distance (R (Real_Part)) /= 0.0);
         pragma Assert (R (Real_Part) < 0.0 or else R (Real_Part) > 1.0);
      end if;
   end Lemma_Is_Outside;

   procedure Lemma_Term_Is_Gathered
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Term           : Pole_Term;
      Distance       : Real)
   with
     Ghost,
     Pre  =>
       Imaginary_Slot <= Maximum_Rational_Degree
       and then abs Term.Real_Part <= Magnitude_Limit
       and then Term.Imaginary_Part >= 0.0
       and then Term.Imaginary_Part <= Magnitude_Limit
       and then abs Term.Reciprocal_Norm <= Magnitude_Limit
       and then Distance = Pole_Distance_Square (Term.Real_Part, Term.Imaginary_Part)
       and then Distance >= Smallest_Distance_Square
       and then
         Is_Pole_Gathered (Cache, Real_Slot, Imaginary_Slot, Term.Real_Part, Term.Imaginary_Part, Term.Reciprocal_Norm)
       and then
         (for all A in Axis_Name =>
            Is_Residue_Gathered
              (Cache, Real_Slot, Imaginary_Slot, Term.Residue_Real (A), Term.Residue_Imaginary (A), A)),
     Post => Is_Term_Gathered (Cache, Real_Slot, Imaginary_Slot, Term);
   --  The pieces which Append establishes one by one make up Is_Term_Gathered.

   procedure Lemma_Term_Is_Gathered
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Term           : Pole_Term;
      Distance       : Real) is
   begin
      pragma
        Assert
          (for all A in Axis_Name =>
             abs Term.Residue_Real (A) <= Magnitude_Limit and then abs Term.Residue_Imaginary (A) <= Magnitude_Limit);
      pragma Assert (Distance_Square (Term) = Distance);
      pragma Assert (Is_Term_Bounded (Term));
   end Lemma_Term_Is_Gathered;

   procedure Lemma_Axes_Are_Gathered
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Term           : Pole_Term;
      Before, After  : Preparation;
      Distance       : Real)
   with
     Ghost,
     Pre  =>
       Imaginary_Slot <= Maximum_Rational_Degree
       and then Distance > 0.0
       and then Distance = Pole_Distance_Square (Term.Real_Part, Term.Imaginary_Part)
       and then
         (for all A in Axis_Name =>
            Is_Axis_Gathered
              (Cache,
               Real_Slot,
               Imaginary_Slot,
               Term.Residue_Real (A),
               Term.Residue_Imaginary (A),
               Before.Active (A),
               After.Active (A),
               Before.Slope (A),
               After.Slope (A),
               Distance,
               A)),
     Post =>
       (for all A in Axis_Name =>
          Is_Residue_Gathered (Cache, Real_Slot, Imaginary_Slot, Term.Residue_Real (A), Term.Residue_Imaginary (A), A)
          and then Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, After.Active (A), A)
          and then (if Before.Active (A) then After.Active (A))
          and then Is_Slope_Grown (Before.Slope (A), After.Slope (A), Term, A));
   --  The share which Append added to each Slope is the term's Slope_Term.

   procedure Lemma_Axes_Are_Gathered
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Term           : Pole_Term;
      Before, After  : Preparation;
      Distance       : Real) is
   begin
      pragma Assert (Distance_Square (Term) = Distance);
      for Axis in Axis_Name loop
         pragma
           Assert
             (Is_Axis_Gathered
                (Cache,
                 Real_Slot,
                 Imaginary_Slot,
                 Term.Residue_Real (Axis),
                 Term.Residue_Imaginary (Axis),
                 Before.Active (Axis),
                 After.Active (Axis),
                 Before.Slope (Axis),
                 After.Slope (Axis),
                 Distance,
                 Axis));
         pragma
           Assert
             (Slope_Term (Term, Axis)
              = (abs (R (Term.Residue_Real (Axis))) + abs (R (Term.Residue_Imaginary (Axis)))) / Distance);
         pragma Assert (Is_Slope_Grown (Before.Slope (Axis), After.Slope (Axis), Term, Axis));
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                (if A <= Axis
                 then
                   Is_Residue_Gathered
                     (Cache, Real_Slot, Imaginary_Slot, Term.Residue_Real (A), Term.Residue_Imaginary (A), A)
                   and then Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, After.Active (A), A)
                   and then (if Before.Active (A) then After.Active (A))
                   and then Is_Slope_Grown (Before.Slope (A), After.Slope (A), Term, A)));
      end loop;
   end Lemma_Axes_Are_Gathered;

   procedure Lemma_Square (Pole, Imaginary, Horizontal : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs Pole <= Magnitude_Limit
       and then abs Imaginary <= Magnitude_Limit
       and then
         (if Pole < 0.0
          then R (Horizontal) = 0.0 - R (Pole)
          elsif Pole > 1.0
          then R (Horizontal) = R (Pole - 1.0)
          else R (Horizontal) = 0.0),
     Post =>
       (Horizontal * Horizontal) in Dimensionless
       and then (Imaginary * Imaginary) in Dimensionless
       and then (Down (Horizontal * Horizontal) + Down (Imaginary * Imaginary)) in Dimensionless
       and then
         (if Down (Down (Horizontal * Horizontal) + Down (Imaginary * Imaginary)) >= Unusable_Divisor
          then
            R (Down (Down (Horizontal * Horizontal) + Down (Imaginary * Imaginary)))
            <= Pole_Distance_Square (Pole, Imaginary));
   --  The squared distance of a pole from [0, 1] as the code computes it, rounded toward zero at each step, is
   --  at most the exact one whenever it is large enough to be accepted. Nothing in it overflows.

   procedure Lemma_Square (Pole, Imaginary, Horizontal : Dimensionless) is
   begin
      Lemma_Horizontal (Pole, Horizontal);
      Lemma_Usable_Bound (Imaginary);
      Lemma_Lowered_Square (Horizontal);
      Lemma_Lowered_Square (Imaginary);
      Lemma_Lowered_Sum (Down (Horizontal * Horizontal), Down (Imaginary * Imaginary));
      if Down (Down (Horizontal * Horizontal) + Down (Imaginary * Imaginary)) >= Unusable_Divisor then
         Lemma_Order (Unusable_Divisor, Down (Down (Horizontal * Horizontal) + Down (Imaginary * Imaginary)));
         pragma Assert (R (Imaginary) * R (Imaginary) >= 0.0);
         Lemma_Distance
           (Rounded  => R (Horizontal),
            Exact    => Horizontal_Distance (R (Pole)),
            Vertical => R (Imaginary) * R (Imaginary),
            First    => R (Down (Horizontal * Horizontal)),
            Second   => R (Down (Imaginary * Imaginary)),
            Square   => R (Down (Down (Horizontal * Horizontal) + Down (Imaginary * Imaginary))));
      end if;
   end Lemma_Square;

   procedure Lemma_Slope_Is_Finite (Slope, Real_Residue, Imaginary_Residue, Square : Dimensionless)
   with
     Ghost,
     Pre  =>
       Slope >= 0.0
       and then R (Slope) <= 15.0 * Slope_Step
       and then abs Real_Residue <= Magnitude_Limit
       and then abs Imaginary_Residue <= Magnitude_Limit
       and then Square >= Unusable_Divisor
       and then R (Square) >= Smallest_Distance_Square,
     Post =>
       (abs Real_Residue + abs Imaginary_Residue) in Dimensionless
       and then (Up (abs Real_Residue + abs Imaginary_Residue) / Square) in Dimensionless
       and then (Slope + Up (Up (abs Real_Residue + abs Imaginary_Residue) / Square)) in Dimensionless;
   --  One pole's share added to Slope, with every rounding upward: no step overflows. Membership in the type
   --  is how a check of overflow reads to the provers, so that Append's statement, which performs these very
   --  operations, needs no arithmetic for its checks; anything else is too slow there.

   procedure Lemma_Slope_Is_Finite (Slope, Real_Residue, Imaginary_Residue, Square : Dimensionless) is
   begin
      Lemma_Slope_Is_Within_Ceiling (Slope);
      Lemma_Slope_Numerator (Real_Residue, Imaginary_Residue);
      Lemma_Slope_Quotient
        (Numerator => Up (abs Real_Residue + abs Imaginary_Residue),
         Square    => Square,
         Exact     => abs (R (Real_Residue)) + abs (R (Imaginary_Residue)),
         Distance  => R (Square));
      Lemma_Slope_Increment (Slope, Up (Up (abs Real_Residue + abs Imaginary_Residue) / Square));
   end Lemma_Slope_Is_Finite;

   procedure Lemma_Slope_Is_Grown (Slope, Real_Residue, Imaginary_Residue, Square : Dimensionless; Distance : Real)
   with
     Ghost,
     Pre  =>
       Slope >= 0.0
       and then R (Slope) <= 15.0 * Slope_Step
       and then abs Real_Residue <= Magnitude_Limit
       and then abs Imaginary_Residue <= Magnitude_Limit
       and then Square >= Unusable_Divisor
       and then R (Square) >= Smallest_Distance_Square
       and then R (Square) <= Distance,
     Post =>
       Up (Slope + Up (Up (abs Real_Residue + abs Imaginary_Residue) / Square)) >= 0.0
       and then R (Up (Slope + Up (Up (abs Real_Residue + abs Imaginary_Residue) / Square))) <= R (Slope) + Slope_Step
       and then
         R (Up (Slope + Up (Up (abs Real_Residue + abs Imaginary_Residue) / Square)))
         >= R (Slope) + (abs (R (Real_Residue)) + abs (R (Imaginary_Residue))) / Distance;
   --  And Slope grows by at least the exact share and by at most Slope_Step.

   procedure Lemma_Slope_Is_Grown (Slope, Real_Residue, Imaginary_Residue, Square : Dimensionless; Distance : Real) is
   begin
      Lemma_Slope_Is_Within_Ceiling (Slope);
      Lemma_Slope_Numerator (Real_Residue, Imaginary_Residue);
      Lemma_Slope_Quotient
        (Numerator => Up (abs Real_Residue + abs Imaginary_Residue),
         Square    => Square,
         Exact     => abs (R (Real_Residue)) + abs (R (Imaginary_Residue)),
         Distance  => Distance);
      Lemma_Slope_Increment (Slope, Up (Up (abs Real_Residue + abs Imaginary_Residue) / Square));
   end Lemma_Slope_Is_Grown;

   procedure Lemma_Norm_Is_Finite (A, B : Dimensionless)
   with
     Ghost,
     Pre  => abs A <= Magnitude_Limit and then abs B <= Magnitude_Limit,
     Post => (A * A) in Dimensionless and then (B * B) in Dimensionless and then (A * A + B * B) in Dimensionless;
   --  The squared modulus of a usable pole does not overflow.

   procedure Lemma_Norm_Is_Finite (A, B : Dimensionless) is
   begin
      Lemma_Usable_Bound (A);
      Lemma_Usable_Bound (B);
      Lemma_Real_Product_Bound (R (A), R (A), Operand_Bound);
      Lemma_Real_Product_Bound (R (B), R (B), Operand_Bound);
      Lemma_Product_Is_Bounded (A, A);
      Lemma_Product_Is_Bounded (B, B);
      pragma Assert (abs (R (A * A)) <= Operand_Bound * Operand_Bound * (1.0 + Epsilon) + Eta);
      pragma Assert (abs (R (B * B)) <= Operand_Bound * Operand_Bound * (1.0 + Epsilon) + Eta);
      Float_Lemmas.Lemma_Rounding_Error_Add (A * A, B * B);
      pragma Assert (abs (R (A * A + B * B)) <= Lowered_Bound * 4.0);
   end Lemma_Norm_Is_Finite;

   package Appending
     with Ghost
   is
      --  What Append has established once its loop is over, and what it promises. Like the facts of Carried,
      --  these are carried rather than looked into: by Append to its end, and by Prepare to Lemma_Appended.

      function Term_Fact
        (Cache                       : Rational_Antiderivative;
         Real_Slot                   : Rational_Degree_Slot;
         Imaginary_Slot              : Natural;
         Term                        : Pole_Term;
         Before_Active, After_Active : Axis_Flags;
         Before_Slope, After_Slope   : Axis_Values) return Boolean
      with Pre => Imaginary_Slot <= Maximum_Rational_Degree;
      --  Term is the pole of the given slots, every Slope has grown by its share, and an axis is inactive only
      --  if it was before and the pole has no residue on it.

      procedure Lemma_Fold_Term
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Term           : Pole_Term;
         Before, After  : Preparation;
         Distance       : Real)
      with
        Pre  =>
          Imaginary_Slot <= Maximum_Rational_Degree
          and then Distance > 0.0
          and then
            Carried.Pole_Fact
              (Cache, Real_Slot, Imaginary_Slot, Term.Real_Part, Term.Imaginary_Part, Term.Reciprocal_Norm, Distance)
          and then
            (for all A in Axis_Name =>
               Carried.Axis_Fact
                 (Cache,
                  Real_Slot,
                  Imaginary_Slot,
                  Term.Residue_Real (A),
                  Term.Residue_Imaginary (A),
                  Before.Active (A),
                  After.Active (A),
                  Before.Slope (A),
                  After.Slope (A),
                  Distance,
                  A,
                  True)),
        Post =>
          Term_Fact (Cache, Real_Slot, Imaginary_Slot, Term, Before.Active, After.Active, Before.Slope, After.Slope);
      --  What the loop over the axes has gathered makes up the term.

      function Fact
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Before, After  : Preparation) return Boolean
      with Pre => Imaginary_Slot <= Maximum_Rational_Degree and then Before.Count < Maximum_Rational_Degree;
      --  Is_Appended.

      procedure Lemma_Fold
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Before, After  : Preparation)
      with
        Pre  =>
          Imaginary_Slot <= Maximum_Rational_Degree
          and then Before.Count < Maximum_Rational_Degree
          and then After.Count = Before.Count + 1
          and then Same_Terms (Before.Terms, After.Terms, Before.Count)
          and then
            (for all Component in Chart_Component_Index =>
               R (After.Delta_Y (Component)) = R (Before.Delta_Y (Component))
               and then After.Vanishes (Component) = Before.Vanishes (Component))
          and then
            Term_Fact
              (Cache,
               Real_Slot,
               Imaginary_Slot,
               After.Terms (After.Count),
               Before.Active,
               After.Active,
               Before.Slope,
               After.Slope),
        Post => Fact (Cache, Real_Slot, Imaginary_Slot, Before, After);
      --  With the term stored and counted, the preparation is the earlier one with that pole appended.

      procedure Lemma_Unfold
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Before, After  : Preparation)
      with
        Pre  =>
          Imaginary_Slot <= Maximum_Rational_Degree
          and then Before.Count < Maximum_Rational_Degree
          and then Fact (Cache, Real_Slot, Imaginary_Slot, Before, After),
        Post => Is_Appended (Cache, Real_Slot, Imaginary_Slot, Before, After);

   end Appending;

   package body Appending is

      function Term_Fact
        (Cache                       : Rational_Antiderivative;
         Real_Slot                   : Rational_Degree_Slot;
         Imaginary_Slot              : Natural;
         Term                        : Pole_Term;
         Before_Active, After_Active : Axis_Flags;
         Before_Slope, After_Slope   : Axis_Values) return Boolean
      is (Is_Term_Gathered (Cache, Real_Slot, Imaginary_Slot, Term)
          and then
            (for all A in Axis_Name =>
               Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, After_Active (A), A)
               and then (if Before_Active (A) then After_Active (A))
               and then Is_Slope_Grown (Before_Slope (A), After_Slope (A), Term, A)));

      procedure Lemma_Fold_Term
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Term           : Pole_Term;
         Before, After  : Preparation;
         Distance       : Real) is
      begin
         Carried.Lemma_Unfold_Gathered (Cache, Real_Slot, Imaginary_Slot, Term, Before, After, Distance);
         Carried.Lemma_Unfold_Pole
           (Cache, Real_Slot, Imaginary_Slot, Term.Real_Part, Term.Imaginary_Part, Term.Reciprocal_Norm, Distance);
         Lemma_Axes_Are_Gathered (Cache, Real_Slot, Imaginary_Slot, Term, Before, After, Distance);
         Lemma_Term_Is_Gathered (Cache, Real_Slot, Imaginary_Slot, Term, Distance);
      end Lemma_Fold_Term;

      function Fact
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Before, After  : Preparation) return Boolean
      is (Is_Appended (Cache, Real_Slot, Imaginary_Slot, Before, After));

      procedure Lemma_Fold
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Before, After  : Preparation) is
      begin
         pragma Assert (Is_Continuation (Before, After));
         pragma Assert (Is_Term_Gathered (Cache, Real_Slot, Imaginary_Slot, After.Terms (After.Count)));
         pragma
           Assert
             (for all Axis in Axis_Name =>
                Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, After.Active (Axis), Axis)
                and then Is_Slope_Grown (Before.Slope (Axis), After.Slope (Axis), After.Terms (After.Count), Axis));
      end Lemma_Fold;

      procedure Lemma_Unfold
        (Cache          : Rational_Antiderivative;
         Real_Slot      : Rational_Degree_Slot;
         Imaginary_Slot : Natural;
         Before, After  : Preparation)
      is null;

   end Appending;

   ---------------------------------------------------------------------------
   --  Gathering one pole
   ---------------------------------------------------------------------------

   procedure Append
     (Cache                     : Rational_Antiderivative;
      Real_Slot, Imaginary_Slot : Natural;
      Prepared                  : in out Preparation;
      Outcome                   : out Status)
   with
     Pre  =>
       Real_Slot in Rational_Degree_Slot
       and then Imaginary_Slot <= Maximum_Rational_Degree
       and then Prepared.Count < Maximum_Rational_Degree
       and then
         (for all Axis in Axis_Name =>
            Prepared.Slope (Axis) >= 0.0 and then R (Prepared.Slope (Axis)) <= 15.0 * Slope_Step),
     Post =>
       Prepared.Outcome = Prepared.Outcome'Old
       and then
         (if Outcome = Certified then Appending.Fact (Cache, Real_Slot, Imaginary_Slot, Prepared'Old, Prepared));
   --  Record one pole and add its share of the retained tangent's largest derivative to Slope. Imaginary_Slot is
   --  zero for a real pole.

   procedure Append
     (Cache                     : Rational_Antiderivative;
      Real_Slot, Imaginary_Slot : Natural;
      Prepared                  : in out Preparation;
      Outcome                   : out Status)
   is
      Term       : Pole_Term;
      Factor     : constant Dimensionless := (if Imaginary_Slot = 0 then 1.0 else 2.0);
      Horizontal : Dimensionless;
      Square     : Dimensionless;

      Before : constant Preparation := Prepared
      with Ghost;
      --  Prepared as it was on entry.

      Distance : Real
      with Ghost;
      --  The exact squared distance of the pole from the curve.

      Old_Term     : Pole_Term
      with Ghost;
      Old_Prepared : Preparation
      with Ghost;
      --  Term and Prepared as they were when the current pass of the loop over the axes began.
   begin
      Outcome := Not_Certified;
      Term.Real_Part := Cache.Pole_Slots (Real_Slot).Pole_Component;
      Term.Imaginary_Part := (if Imaginary_Slot = 0 then 0.0 else Cache.Pole_Slots (Imaginary_Slot).Pole_Component);
      if not (Usable (Term.Real_Part) and then Usable (Term.Imaginary_Part)) then
         return;
      end if;
      Horizontal :=
        (if Term.Real_Part < 0.0 then -Term.Real_Part elsif Term.Real_Part > 1.0 then Term.Real_Part - 1.0 else 0.0);
      Lemma_Negation_Is_Exact (Term.Real_Part);
      Lemma_Square (Term.Real_Part, Term.Imaginary_Part, Horizontal);
      --  The squared distance from [0, 1], rounded toward zero.
      Square := Down (Down (Horizontal * Horizontal) + Down (Term.Imaginary_Part * Term.Imaginary_Part));
      Distance := Pole_Distance_Square (Term.Real_Part, Term.Imaginary_Part);

      --  Of the arithmetic so far, only this matters to the rest.
      pragma
        Assert_And_Cut
          (Outcome = Not_Certified
           and then R (Term.Real_Part) = R (Cache.Pole_Slots (Real_Slot).Pole_Component)
           and then abs Term.Real_Part <= Magnitude_Limit
           and then abs Term.Imaginary_Part <= Magnitude_Limit
           and then
             (if Imaginary_Slot = 0
              then Term.Imaginary_Part = 0.0
              else R (Term.Imaginary_Part) = R (Cache.Pole_Slots (Imaginary_Slot).Pole_Component))
           and then Distance = Pole_Distance_Square (Term.Real_Part, Term.Imaginary_Part)
           and then (if Square >= Unusable_Divisor then R (Square) <= Distance));
      if Imaginary_Slot = 0 then
         if Term.Real_Part = 0.0 then
            return;
         elsif abs Term.Real_Part <= Overflowing_Divisor then
            Outcome := Unsafe;
            return;
         elsif abs Term.Real_Part < Unusable_Divisor then
            return;
         end if;
         Lemma_Real_Reciprocal (Term.Real_Part);
         Term.Reciprocal_Norm := 1.0 / Term.Real_Part;
      else
         Lemma_Norm_Is_Finite (Term.Real_Part, Term.Imaginary_Part);
         declare
            Norm : constant Dimensionless :=
              Term.Real_Part * Term.Real_Part + Term.Imaginary_Part * Term.Imaginary_Part;
         begin
            if not (Term.Imaginary_Part > 0.0 and then Norm > 0.0) then
               return;
            elsif Norm <= Overflowing_Divisor then
               Outcome := Unsafe;
               return;
            elsif Norm < Unusable_Divisor then
               return;
            end if;
            Lemma_Pair_Reciprocal (Term.Real_Part, Term.Imaginary_Part);
            Term.Reciprocal_Norm := 1.0 / Norm;
         end;
      end if;
      pragma
        Assert (Is_Reciprocal_Gathered (Imaginary_Slot, Term.Real_Part, Term.Imaginary_Part, Term.Reciprocal_Norm));
      Lemma_Reciprocal_Is_Finite (Square);
      if not (Square > 0.0 and then Usable (Term.Reciprocal_Norm)) then
         return;
      elsif Square <= Overflowing_Divisor then
         Outcome := Unsafe;
         return;
      elsif Square < Unusable_Divisor or else not Usable (1.0 / Square) then
         return;
      end if;

      --  The accepted Square is at least Smallest_Distance_Square and at most the exact squared distance. A real
      --  pole at this distance is outside [0, 1].
      Lemma_Accepted_Square (Square);
      pragma Assert (R (Square) <= Distance);
      pragma Assert (Distance >= Smallest_Distance_Square);
      pragma Assert (Distance = Pole_Distance_Square (Term.Real_Part, Term.Imaginary_Part));
      Lemma_Is_Outside (Term.Real_Part, Term.Imaginary_Part, Distance);
      pragma Assert (if Imaginary_Slot = 0 then Term.Imaginary_Part >= 0.0 else Term.Imaginary_Part > 0.0);
      pragma
        Assert
          (Is_Pole_Gathered
             (Cache, Real_Slot, Imaginary_Slot, Term.Real_Part, Term.Imaginary_Part, Term.Reciprocal_Norm));
      Carried.Lemma_Fold_Pole
        (Cache, Real_Slot, Imaginary_Slot, Term.Real_Part, Term.Imaginary_Part, Term.Reciprocal_Norm, Distance);

      --  This is all that the rest needs to know of the pole.
      pragma
        Assert_And_Cut
          (Outcome = Not_Certified
           and then
             Carried.Pole_Fact
               (Cache, Real_Slot, Imaginary_Slot, Term.Real_Part, Term.Imaginary_Part, Term.Reciprocal_Norm, Distance)
           and then Distance >= Smallest_Distance_Square
           and then Square >= Unusable_Divisor
           and then R (Square) >= Smallest_Distance_Square
           and then R (Square) <= Distance);
      Carried.Lemma_Fold_Untouched (Cache, Real_Slot, Imaginary_Slot, Term, Before, Distance);

      for Axis in Axis_Name loop
         declare
            Real_Stored       : constant Dimensionless := Cache.Pole_Slots (Real_Slot).Residue_Component (Axis);
            Imaginary_Stored  : constant Dimensionless :=
              (if Imaginary_Slot = 0 then 0.0 else Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Axis));
            Real_Residue      : Dimensionless;
            Imaginary_Residue : Dimensionless;

            Old_Active : constant Boolean := Prepared.Active (Axis)
            with Ghost;
            Old_Slope  : constant Dimensionless := Prepared.Slope (Axis)
            with Ghost;
            --  The axis' flag and Slope before this pass.
         begin
            Carried.Lemma_Unfold_Untouched
              (Cache,
               Real_Slot,
               Imaginary_Slot,
               Term.Residue_Real (Axis),
               Term.Residue_Imaginary (Axis),
               Before.Active (Axis),
               Old_Active,
               Before.Slope (Axis),
               Old_Slope,
               Distance,
               Axis);
            --  Doubling a pair's residue is exact unless it overflows.
            if Imaginary_Slot /= 0
              and then
                (abs Real_Stored > Dimensionless'Last / 2.0 or else abs Imaginary_Stored > Dimensionless'Last / 2.0)
            then
               Outcome := Unsafe;
               return;
            end if;
            Lemma_Scaled (Factor, Real_Stored);
            Lemma_Scaled (Factor, Imaginary_Stored);
            Real_Residue := Factor * Real_Stored;
            Imaginary_Residue := (if Imaginary_Slot = 0 then 0.0 else Factor * Imaginary_Stored);
            if not (Usable (Real_Residue) and then Usable (Imaginary_Residue)) then
               return;
            end if;

            --  The products themselves are of no further interest.
            pragma
              Assert_And_Cut
                (Outcome = Not_Certified
                 and then abs Real_Residue <= Magnitude_Limit
                 and then abs Imaginary_Residue <= Magnitude_Limit
                 and then Old_Slope >= 0.0
                 and then R (Old_Slope) <= 15.0 * Slope_Step
                 and then Is_Axis_Untouched (Before.Active (Axis), Old_Active, Before.Slope (Axis), Old_Slope)
                 and then Is_Residue_Gathered (Cache, Real_Slot, Imaginary_Slot, Real_Residue, Imaginary_Residue, Axis)
                 and then
                   (if Real_Residue = 0.0 and then Imaginary_Residue = 0.0
                    then Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, False, Axis)));
            Old_Term := Term;
            Old_Prepared := Prepared;
            Term.Residue_Real (Axis) := Real_Residue;
            Term.Residue_Imaginary (Axis) := Imaginary_Residue;
            if Real_Residue /= 0.0 or else Imaginary_Residue /= 0.0 then
               Prepared.Active (Axis) := True;
               Lemma_Slope_Is_Finite (Prepared.Slope (Axis), Real_Residue, Imaginary_Residue, Square);
               --  |d/dU r/(U - p)| ≤ |r|/d².
               Prepared.Slope (Axis) :=
                 Up (Prepared.Slope (Axis) + Up (Up (abs Real_Residue + abs Imaginary_Residue) / Square));
               Lemma_Slope_Is_Grown (Old_Slope, Real_Residue, Imaginary_Residue, Square, Distance);
            end if;
            Carried.Lemma_Fold_Gathered
              (Cache,
               Real_Slot,
               Imaginary_Slot,
               Real_Residue,
               Imaginary_Residue,
               Before.Active (Axis),
               Old_Active,
               Prepared.Active (Axis),
               Before.Slope (Axis),
               Old_Slope,
               Prepared.Slope (Axis),
               Distance,
               Axis);
            Carried.Lemma_Step
              (Cache, Real_Slot, Imaginary_Slot, Old_Term, Term, Before, Old_Prepared, Prepared, Distance, Axis);
         end;
         pragma Loop_Invariant (Outcome = Not_Certified);
         pragma
           Loop_Invariant
             (Carried.Pole_Fact
                (Cache,
                 Real_Slot,
                 Imaginary_Slot,
                 Term.Real_Part,
                 Term.Imaginary_Part,
                 Term.Reciprocal_Norm,
                 Distance));
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                Carried.Axis_Fact
                  (Cache,
                   Real_Slot,
                   Imaginary_Slot,
                   Term.Residue_Real (A),
                   Term.Residue_Imaginary (A),
                   Before.Active (A),
                   Prepared.Active (A),
                   Before.Slope (A),
                   Prepared.Slope (A),
                   Distance,
                   A,
                   A <= Axis));
      end loop;

      Appending.Lemma_Fold_Term (Cache, Real_Slot, Imaginary_Slot, Term, Before, Prepared, Distance);

      --  And this is all that is left to know of the pole and its residues.
      pragma
        Assert_And_Cut
          (Prepared.Count = Before.Count
           and then Prepared.Outcome = Before.Outcome
           and then Same_Terms (Before.Terms, Prepared.Terms, Before.Count)
           and then
             (for all Component in Chart_Component_Index =>
                R (Prepared.Delta_Y (Component)) = R (Before.Delta_Y (Component))
                and then Prepared.Vanishes (Component) = Before.Vanishes (Component))
           and then
             Appending.Term_Fact
               (Cache, Real_Slot, Imaginary_Slot, Term, Before.Active, Prepared.Active, Before.Slope, Prepared.Slope));
      Prepared.Count := Prepared.Count + 1;
      Prepared.Terms (Prepared.Count) := Term;
      Appending.Lemma_Fold (Cache, Real_Slot, Imaginary_Slot, Before, Prepared);
      Outcome := Certified;
   end Append;

   ---------------------------------------------------------------------------
   --  What has been gathered so far
   ---------------------------------------------------------------------------

   function Slots_Done (Cache : Rational_Antiderivative; Count : Natural) return Natural
   is (if Count <= Cache.Real_Pole_Count then Count else Cache.Real_Pole_Count + 2 * (Count - Cache.Real_Pole_Count))
   with Ghost, Pre => Count <= Maximum_Rational_Degree;
   --  The number of leading slots which the first Count terms come from.

   function Is_Next_Pole
     (Cache : Rational_Antiderivative; Count : Natural; Real_Slot, Imaginary_Slot : Natural) return Boolean
   is (Count < Cache.Real_Pole_Count + Cache.Pair_Count
       and then
         (if Count < Cache.Real_Pole_Count
          then Real_Slot = Count + 1 and then Imaginary_Slot = 0
          else
            Real_Slot = Cache.Real_Pole_Count + 2 * (Count - Cache.Real_Pole_Count) + 1
            and then Imaginary_Slot = Real_Slot + 1))
   with Ghost, Pre => Has_Layout (Cache) and then Count <= Maximum_Rational_Degree;
   --  Whether the slots are those of the pole which follows the first Count: the real poles come first, in
   --  the order of their slots, and then the pairs.

   function Is_Bounded_Element (Term : Pole_Term) return Boolean
   with Ghost, Post => Is_Bounded_Element'Result = Is_Term_Bounded (Term);

   function Is_Bounded_Element (Term : Pole_Term) return Boolean is
   begin
      return Is_Term_Bounded (Term);
   end Is_Bounded_Element;

   function Is_Real_Element
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Term : Pole_Term) return Boolean
   with
     Ghost,
     Post =>
       Is_Real_Element'Result
       = (Is_Real_Term (Cache, Slot, Term)
          and then
            (Cache.Pole_Slots (Slot).Pole_Component < 0.0 or else Cache.Pole_Slots (Slot).Pole_Component > 1.0));

   function Is_Real_Element
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Term : Pole_Term) return Boolean is
   begin
      return
        Is_Real_Term (Cache, Slot, Term)
        and then (Cache.Pole_Slots (Slot).Pole_Component < 0.0 or else Cache.Pole_Slots (Slot).Pole_Component > 1.0);
   end Is_Real_Element;

   function Is_Pair_Element
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term) return Boolean
   with
     Ghost,
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count,
     Post =>
       Is_Pair_Element'Result
       = (Is_Pair_Term (Cache, Pair, Term)
          and then Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair).Pole_Component > 0.0);

   function Is_Pair_Element
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term) return Boolean is
   begin
      return
        Is_Pair_Term (Cache, Pair, Term)
        and then Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair).Pole_Component > 0.0;
   end Is_Pair_Element;
   --  Is_Term_Bounded, Is_Real_Term and Is_Pair_Term, the latter two with what Poles_Are_Off_The_Curve says of
   --  the slots. They are functions with a postcondition rather than expression functions: an expression
   --  function is expanded wherever a check mentions it, and expanded below a quantifier, with their divisions,
   --  these are more than the provers can take.

   function Are_Terms_Gathered (Cache : Rational_Antiderivative; Prepared : Preparation) return Boolean
   is (Prepared.Count <= Cache.Real_Pole_Count + Cache.Pair_Count
       and then (for all Index in 1 .. Prepared.Count => Is_Bounded_Element (Prepared.Terms (Index)))
       and then
         (for all Slot in 1 .. Cache.Real_Pole_Count =>
            (if Slot <= Prepared.Count then Is_Real_Element (Cache, Slot, Prepared.Terms (Slot))))
       and then
         (for all Pair in 1 .. Cache.Pair_Count =>
            (if Cache.Real_Pole_Count + Pair <= Prepared.Count
             then Is_Pair_Element (Cache, Pair, Prepared.Terms (Cache.Real_Pole_Count + Pair)))))
   with Ghost, Pre => Has_Layout (Cache);
   --  Is_Cache_Prepared for the terms gathered so far, with what Poles_Are_Off_The_Curve says of their slots.

   function Are_Slopes_Gathered (Prepared : Preparation) return Boolean
   is (for all Axis in Axis_Name =>
         Prepared.Slope (Axis) >= 0.0
         and then R (Prepared.Slope (Axis)) <= Slope_Cap (Prepared.Count)
         and then R (Prepared.Slope (Axis)) >= Slope_Sum (Prepared, Prepared.Count, Axis))
   with Ghost;
   --  Slope covers the terms gathered so far, and has grown by at most Slope_Step with each.

   function Are_Idle_Axes_Empty (Cache : Rational_Antiderivative; Prepared : Preparation) return Boolean
   is (for all Axis in Axis_Name =>
         (if not Prepared.Active (Axis)
          then
            (for all Slot in Rational_Degree_Slot =>
               (if Slot <= Slots_Done (Cache, Prepared.Count)
                then Cache.Pole_Slots (Slot).Residue_Component (Axis) = 0.0))))
   with Ghost;
   --  On an axis which is not active, every residue seen so far is zero.

   function Is_Partly_Prepared (Cache : Rational_Antiderivative; Prepared : Preparation) return Boolean
   is (Are_Terms_Gathered (Cache, Prepared)
       and then Are_Slopes_Gathered (Prepared)
       and then Are_Idle_Axes_Empty (Cache, Prepared))
   with Ghost, Pre => Has_Layout (Cache);
   --  What holds of the terms gathered so far, and is Is_Cache_Prepared once all are.

   ---------------------------------------------------------------------------
   --  Terms which denote the same numbers
   ---------------------------------------------------------------------------

   procedure Lemma_Same_Term_Floats (Left, Right : Pole_Term)
   with
     Ghost,
     Pre  => Same_Term (Left, Right),
     Post =>
       Left.Real_Part = Right.Real_Part
       and then Left.Imaginary_Part = Right.Imaginary_Part
       and then Left.Reciprocal_Norm = Right.Reciprocal_Norm
       and then
         (for all Axis in Axis_Name =>
            Left.Residue_Real (Axis) = Right.Residue_Real (Axis)
            and then Left.Residue_Imaginary (Axis) = Right.Residue_Imaginary (Axis))
       and then Distance_Square (Left) = Distance_Square (Right);

   procedure Lemma_Same_Term_Floats (Left, Right : Pole_Term) is
   begin
      Lemma_Same_Float (Left.Real_Part, Right.Real_Part);
      Lemma_Same_Float (Left.Imaginary_Part, Right.Imaginary_Part);
      Lemma_Same_Float (Left.Reciprocal_Norm, Right.Reciprocal_Norm);
      for Axis in Axis_Name loop
         Lemma_Same_Float (Left.Residue_Real (Axis), Right.Residue_Real (Axis));
         Lemma_Same_Float (Left.Residue_Imaginary (Axis), Right.Residue_Imaginary (Axis));
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                (if A <= Axis
                 then
                   Left.Residue_Real (A) = Right.Residue_Real (A)
                   and then Left.Residue_Imaginary (A) = Right.Residue_Imaginary (A)));
      end loop;
   end Lemma_Same_Term_Floats;

   procedure Lemma_Same_Term_Is_Bounded (Old_Term, New_Term : Pole_Term)
   with
     Ghost,
     Pre  => Same_Term (Old_Term, New_Term) and then Is_Term_Bounded (Old_Term),
     Post => Is_Term_Bounded (New_Term);

   procedure Lemma_Same_Term_Is_Bounded (Old_Term, New_Term : Pole_Term) is
   begin
      Lemma_Same_Term_Floats (Old_Term, New_Term);
      pragma
        Assert
          (for all Axis in Axis_Name =>
             abs New_Term.Residue_Real (Axis) <= Magnitude_Limit
             and then abs New_Term.Residue_Imaginary (Axis) <= Magnitude_Limit);
   end Lemma_Same_Term_Is_Bounded;

   procedure Lemma_Same_Term_Is_Real
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Old_Term, New_Term : Pole_Term)
   with
     Ghost,
     Pre  => Same_Term (Old_Term, New_Term) and then Is_Real_Term (Cache, Slot, Old_Term),
     Post => Is_Real_Term (Cache, Slot, New_Term);

   procedure Lemma_Same_Term_Is_Real
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Old_Term, New_Term : Pole_Term) is
   begin
      Lemma_Same_Term_Floats (Old_Term, New_Term);
      pragma
        Assert
          (for all Axis in Axis_Name =>
             R (New_Term.Residue_Real (Axis)) = R (Cache.Pole_Slots (Slot).Residue_Component (Axis))
             and then New_Term.Residue_Imaginary (Axis) = 0.0);
   end Lemma_Same_Term_Is_Real;

   procedure Lemma_Same_Term_Is_Pair
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Old_Term, New_Term : Pole_Term)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Pair <= Cache.Pair_Count
       and then Same_Term (Old_Term, New_Term)
       and then Is_Pair_Term (Cache, Pair, Old_Term),
     Post => Is_Pair_Term (Cache, Pair, New_Term);

   procedure Lemma_Same_Term_Is_Pair
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Old_Term, New_Term : Pole_Term) is
   begin
      Lemma_Same_Term_Floats (Old_Term, New_Term);
      pragma
        Assert
          (for all Axis in Axis_Name =>
             abs (R (New_Term.Residue_Real (Axis))
                  - 2.0 * R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis)))
             <= 2.0 * Epsilon * abs R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis)) + Eta
             and then
               abs (R (New_Term.Residue_Imaginary (Axis))
                    - 2.0 * R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis)))
               <= 2.0
                  * Epsilon
                  * abs R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis))
                  + Eta);
   end Lemma_Same_Term_Is_Pair;

   procedure Lemma_Same_Bounded_Element (Old_Term, New_Term : Pole_Term)
   with
     Ghost,
     Pre  => Same_Term (Old_Term, New_Term) and then Is_Bounded_Element (Old_Term),
     Post => Is_Bounded_Element (New_Term);

   procedure Lemma_Same_Bounded_Element (Old_Term, New_Term : Pole_Term) is
   begin
      Lemma_Same_Term_Is_Bounded (Old_Term, New_Term);
   end Lemma_Same_Bounded_Element;

   procedure Lemma_Same_Real_Element
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Old_Term, New_Term : Pole_Term)
   with
     Ghost,
     Pre  => Same_Term (Old_Term, New_Term) and then Is_Real_Element (Cache, Slot, Old_Term),
     Post => Is_Real_Element (Cache, Slot, New_Term);

   procedure Lemma_Same_Real_Element
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Old_Term, New_Term : Pole_Term) is
   begin
      Lemma_Same_Term_Is_Real (Cache, Slot, Old_Term, New_Term);
   end Lemma_Same_Real_Element;

   procedure Lemma_Same_Pair_Element
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Old_Term, New_Term : Pole_Term)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Pair <= Cache.Pair_Count
       and then Same_Term (Old_Term, New_Term)
       and then Is_Pair_Element (Cache, Pair, Old_Term),
     Post => Is_Pair_Element (Cache, Pair, New_Term);

   procedure Lemma_Same_Pair_Element
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Old_Term, New_Term : Pole_Term) is
   begin
      Lemma_Same_Term_Is_Pair (Cache, Pair, Old_Term, New_Term);
   end Lemma_Same_Pair_Element;
   --  The same three facts in the form in which Are_Terms_Gathered holds them, so that where these lemmas are
   --  used nothing has to be expanded.

   procedure Lemma_New_Real_Term (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Term : Pole_Term)
   with
     Ghost,
     Pre  => Is_Term_Gathered (Cache, Slot, 0, Term),
     Post =>
       Is_Real_Term (Cache, Slot, Term)
       and then (Cache.Pole_Slots (Slot).Pole_Component < 0.0 or else Cache.Pole_Slots (Slot).Pole_Component > 1.0);
   --  A term gathered from one slot is the real pole of that slot, which is outside [0, 1].

   procedure Lemma_New_Real_Term (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Term : Pole_Term) is
   begin
      pragma Assert (Is_Pole_Gathered (Cache, Slot, 0, Term.Real_Part, Term.Imaginary_Part, Term.Reciprocal_Norm));
      Lemma_Same_Float (Term.Real_Part, Cache.Pole_Slots (Slot).Pole_Component);
      for Axis in Axis_Name loop
         pragma
           Assert
             (Is_Residue_Gathered (Cache, Slot, 0, Term.Residue_Real (Axis), Term.Residue_Imaginary (Axis), Axis));
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                (if A <= Axis
                 then
                   R (Term.Residue_Real (A)) = R (Cache.Pole_Slots (Slot).Residue_Component (A))
                   and then Term.Residue_Imaginary (A) = 0.0));
      end loop;
   end Lemma_New_Real_Term;

   procedure Lemma_New_Pair_Term (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Pair <= Cache.Pair_Count
       and then Is_Term_Gathered (Cache, Pair_Real_Slot (Cache, Pair), Pair_Imaginary_Slot (Cache, Pair), Term),
     Post =>
       Is_Pair_Term (Cache, Pair, Term)
       and then Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair).Pole_Component > 0.0;
   --  A term gathered from the two slots of a pair is that pair, which is off the real axis.

   procedure Lemma_New_Pair_Term (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term) is
      Real_Slot      : constant Rational_Degree_Slot := Pair_Real_Slot (Cache, Pair);
      Imaginary_Slot : constant Rational_Degree_Slot := Pair_Imaginary_Slot (Cache, Pair);
   begin
      pragma
        Assert
          (Is_Pole_Gathered
             (Cache, Real_Slot, Imaginary_Slot, Term.Real_Part, Term.Imaginary_Part, Term.Reciprocal_Norm));
      Lemma_Same_Float (Term.Imaginary_Part, Cache.Pole_Slots (Imaginary_Slot).Pole_Component);
      for Axis in Axis_Name loop
         pragma
           Assert
             (Is_Residue_Gathered
                (Cache, Real_Slot, Imaginary_Slot, Term.Residue_Real (Axis), Term.Residue_Imaginary (Axis), Axis));
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                (if A <= Axis
                 then
                   abs (R (Term.Residue_Real (A)) - 2.0 * R (Cache.Pole_Slots (Real_Slot).Residue_Component (A)))
                   <= 2.0 * Epsilon * abs R (Cache.Pole_Slots (Real_Slot).Residue_Component (A)) + Eta
                   and then
                     abs (R (Term.Residue_Imaginary (A))
                          - 2.0 * R (Cache.Pole_Slots (Imaginary_Slot).Residue_Component (A)))
                     <= 2.0 * Epsilon * abs R (Cache.Pole_Slots (Imaginary_Slot).Residue_Component (A)) + Eta));
      end loop;
   end Lemma_New_Pair_Term;

   ---------------------------------------------------------------------------
   --  One more pole
   ---------------------------------------------------------------------------

   procedure Lemma_Slope_Sum_Frame (Left, Right : Preparation; Count : Natural; Axis : Axis_Name)
   with
     Ghost,
     Subprogram_Variant => (Decreases => Count),
     Pre                => Count <= Maximum_Rational_Degree and then Same_Terms (Left.Terms, Right.Terms, Count),
     Post               => Slope_Sum (Left, Count, Axis) = Slope_Sum (Right, Count, Axis);
   --  Slope_Sum depends on the first Count terms only.

   procedure Lemma_Slope_Sum_Frame (Left, Right : Preparation; Count : Natural; Axis : Axis_Name) is
   begin
      if Count > 0 then
         Lemma_Slope_Sum_Frame (Left, Right, Count - 1, Axis);
         pragma Assert (Same_Term (Left.Terms (Count), Right.Terms (Count)));
         pragma Assert (Distance_Square (Left.Terms (Count)) = Distance_Square (Right.Terms (Count)));
         pragma Assert (Slope_Term (Left.Terms (Count), Axis) = Slope_Term (Right.Terms (Count), Axis));
         pragma
           Assert
             (Slope_Sum (Left, Count, Axis)
              = Slope_Sum (Left, Count - 1, Axis) + Slope_Term (Left.Terms (Count), Axis));
         pragma
           Assert
             (Slope_Sum (Right, Count, Axis)
              = Slope_Sum (Right, Count - 1, Axis) + Slope_Term (Right.Terms (Count), Axis));
      end if;
   end Lemma_Slope_Sum_Frame;

   procedure Lemma_Slope_Sum_Append (Before, After : Preparation)
   with
     Ghost,
     Pre  =>
       Before.Count < Maximum_Rational_Degree
       and then After.Count = Before.Count + 1
       and then Same_Terms (Before.Terms, After.Terms, Before.Count),
     Post =>
       (for all Axis in Axis_Name =>
          Slope_Sum (After, After.Count, Axis)
          = Slope_Sum (Before, Before.Count, Axis) + Slope_Term (After.Terms (After.Count), Axis));
   --  One more term adds its Slope_Term to every sum.

   procedure Lemma_Slope_Sum_Append (Before, After : Preparation) is
   begin
      for Axis in Axis_Name loop
         Lemma_Slope_Sum_Frame (Before, After, Before.Count, Axis);
         pragma
           Assert
             (Slope_Sum (After, After.Count, Axis)
              = Slope_Sum (After, After.Count - 1, Axis) + Slope_Term (After.Terms (After.Count), Axis));
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                (if A <= Axis
                 then
                   Slope_Sum (After, After.Count, A)
                   = Slope_Sum (Before, Before.Count, A) + Slope_Term (After.Terms (After.Count), A)));
      end loop;
   end Lemma_Slope_Sum_Append;

   procedure Lemma_Slopes_Have_Room (Prepared : Preparation)
   with
     Ghost,
     Pre  => Prepared.Count < Maximum_Rational_Degree and then Are_Slopes_Gathered (Prepared),
     Post =>
       (for all Axis in Axis_Name =>
          Prepared.Slope (Axis) >= 0.0 and then R (Prepared.Slope (Axis)) <= 15.0 * Slope_Step);
   --  Before the last pole, Slope is at least one Slope_Step below its limit: what Append needs.

   procedure Lemma_Slopes_Have_Room (Prepared : Preparation) is
   begin
      Lemma_Slope_Cap (Prepared.Count);
   end Lemma_Slopes_Have_Room;

   procedure Lemma_Terms_Extend
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Before, After  : Preparation)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Imaginary_Slot <= Maximum_Rational_Degree
       and then Before.Count < Maximum_Rational_Degree
       and then Are_Terms_Gathered (Cache, Before)
       and then Is_Next_Pole (Cache, Before.Count, Real_Slot, Imaginary_Slot)
       and then After.Count = Before.Count + 1
       and then Same_Terms (Before.Terms, After.Terms, Before.Count)
       and then Is_Term_Gathered (Cache, Real_Slot, Imaginary_Slot, After.Terms (After.Count)),
     Post => Are_Terms_Gathered (Cache, After);
   --  The earlier terms denote the same numbers as before, and the new one is the next pole.

   procedure Lemma_Terms_Extend
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Before, After  : Preparation) is
   begin
      for Index in 1 .. Before.Count loop
         pragma Assert (Same_Term (Before.Terms (Index), After.Terms (Index)));
         pragma Assert (Is_Bounded_Element (Before.Terms (Index)));
         Lemma_Same_Bounded_Element (Before.Terms (Index), After.Terms (Index));
         pragma Loop_Invariant (for all I in 1 .. Index => Is_Bounded_Element (After.Terms (I)));
      end loop;
      pragma Assert (Is_Bounded_Element (After.Terms (After.Count)));
      pragma Assert (for all I in 1 .. After.Count => Is_Bounded_Element (After.Terms (I)));

      for Slot in 1 .. Cache.Real_Pole_Count loop
         if Slot <= Before.Count then
            pragma Assert (Same_Term (Before.Terms (Slot), After.Terms (Slot)));
            pragma Assert (Is_Real_Element (Cache, Slot, Before.Terms (Slot)));
            Lemma_Same_Real_Element (Cache, Slot, Before.Terms (Slot), After.Terms (Slot));
         elsif Slot = After.Count then
            pragma Assert (Real_Slot = Slot and then Imaginary_Slot = 0);
            Lemma_New_Real_Term (Cache, Slot, After.Terms (Slot));
            pragma Assert (Is_Real_Element (Cache, Slot, After.Terms (Slot)));
         end if;
         pragma
           Loop_Invariant
             (for all S in 1 .. Slot => (if S <= After.Count then Is_Real_Element (Cache, S, After.Terms (S))));
      end loop;

      for Pair in 1 .. Cache.Pair_Count loop
         if Cache.Real_Pole_Count + Pair <= Before.Count then
            pragma
              Assert
                (Same_Term (Before.Terms (Cache.Real_Pole_Count + Pair), After.Terms (Cache.Real_Pole_Count + Pair)));
            pragma Assert (Is_Pair_Element (Cache, Pair, Before.Terms (Cache.Real_Pole_Count + Pair)));
            Lemma_Same_Pair_Element
              (Cache, Pair, Before.Terms (Cache.Real_Pole_Count + Pair), After.Terms (Cache.Real_Pole_Count + Pair));
         elsif Cache.Real_Pole_Count + Pair = After.Count then
            pragma
              Assert
                (Real_Slot = Pair_Real_Slot (Cache, Pair) and then Imaginary_Slot = Pair_Imaginary_Slot (Cache, Pair));
            Lemma_New_Pair_Term (Cache, Pair, After.Terms (Cache.Real_Pole_Count + Pair));
            pragma Assert (Is_Pair_Element (Cache, Pair, After.Terms (Cache.Real_Pole_Count + Pair)));
         end if;
         pragma
           Loop_Invariant
             (for all P in 1 .. Pair =>
                (if Cache.Real_Pole_Count + P <= After.Count
                 then Is_Pair_Element (Cache, P, After.Terms (Cache.Real_Pole_Count + P))));
      end loop;
   end Lemma_Terms_Extend;

   procedure Lemma_Slopes_Extend (Before, After : Preparation)
   with
     Ghost,
     Pre  =>
       Before.Count < Maximum_Rational_Degree
       and then After.Count = Before.Count + 1
       and then Same_Terms (Before.Terms, After.Terms, Before.Count)
       and then Are_Slopes_Gathered (Before)
       and then
         (for all Axis in Axis_Name =>
            Is_Slope_Grown (Before.Slope (Axis), After.Slope (Axis), After.Terms (After.Count), Axis)),
     Post => Are_Slopes_Gathered (After);
   --  Every Slope has grown by at least the new term's share, which is what its sum has grown by.

   procedure Lemma_Slopes_Extend (Before, After : Preparation) is
   begin
      Lemma_Slope_Sum_Append (Before, After);
      Lemma_Slope_Cap (Before.Count);
      for Axis in Axis_Name loop
         pragma Assert (Is_Slope_Grown (Before.Slope (Axis), After.Slope (Axis), After.Terms (After.Count), Axis));
         pragma Assert (R (Before.Slope (Axis)) <= Slope_Cap (Before.Count));
         pragma Assert (R (Before.Slope (Axis)) >= Slope_Sum (Before, Before.Count, Axis));
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                (if A <= Axis
                 then
                   After.Slope (A) >= 0.0
                   and then R (After.Slope (A)) <= Slope_Cap (After.Count)
                   and then R (After.Slope (A)) >= Slope_Sum (After, After.Count, A)));
      end loop;
   end Lemma_Slopes_Extend;

   procedure Lemma_Idle_Extend
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Before, After  : Preparation)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Imaginary_Slot <= Maximum_Rational_Degree
       and then Before.Count < Maximum_Rational_Degree
       and then Is_Next_Pole (Cache, Before.Count, Real_Slot, Imaginary_Slot)
       and then After.Count = Before.Count + 1
       and then Are_Idle_Axes_Empty (Cache, Before)
       and then (for all Axis in Axis_Name => (if Before.Active (Axis) then After.Active (Axis)))
       and then
         (for all Axis in Axis_Name =>
            Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, After.Active (Axis), Axis)),
     Post => Are_Idle_Axes_Empty (Cache, After);
   --  An axis which is still inactive was inactive before, and the new pole has no residue on it.

   procedure Lemma_Idle_Extend
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Before, After  : Preparation) is
   begin
      pragma Assert (Slots_Done (Cache, Before.Count) = Real_Slot - 1);
      pragma Assert (Slots_Done (Cache, After.Count) = (if Imaginary_Slot = 0 then Real_Slot else Imaginary_Slot));
      for Axis in Axis_Name loop
         pragma Assert (Is_Idle_Residue_Zero (Cache, Real_Slot, Imaginary_Slot, After.Active (Axis), Axis));
         pragma
           Assert
             (if not After.Active (Axis)
              then
                (for all Slot in Rational_Degree_Slot =>
                   (if Slot <= Slots_Done (Cache, Before.Count)
                    then Cache.Pole_Slots (Slot).Residue_Component (Axis) = 0.0)));
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                (if A <= Axis and then not After.Active (A)
                 then
                   (for all Slot in Rational_Degree_Slot =>
                      (if Slot <= Slots_Done (Cache, After.Count)
                       then Cache.Pole_Slots (Slot).Residue_Component (A) = 0.0))));
      end loop;
   end Lemma_Idle_Extend;

   procedure Lemma_Appended
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Before, After  : Preparation)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Imaginary_Slot <= Maximum_Rational_Degree
       and then Before.Count < Maximum_Rational_Degree
       and then Is_Partly_Prepared (Cache, Before)
       and then Is_Next_Pole (Cache, Before.Count, Real_Slot, Imaginary_Slot)
       and then Appending.Fact (Cache, Real_Slot, Imaginary_Slot, Before, After),
     Post => Is_Partly_Prepared (Cache, After) and then Is_Continuation (Before, After);
   --  What Append does to a partial preparation, when it is given the next pole.

   procedure Lemma_Appended
     (Cache          : Rational_Antiderivative;
      Real_Slot      : Rational_Degree_Slot;
      Imaginary_Slot : Natural;
      Before, After  : Preparation) is
   begin
      Appending.Lemma_Unfold (Cache, Real_Slot, Imaginary_Slot, Before, After);
      Lemma_Terms_Extend (Cache, Real_Slot, Imaginary_Slot, Before, After);
      Lemma_Slopes_Extend (Before, After);
      Lemma_Idle_Extend (Cache, Real_Slot, Imaginary_Slot, Before, After);
   end Lemma_Appended;

   ---------------------------------------------------------------------------
   --  The chart and the frame
   ---------------------------------------------------------------------------

   function Component_Holds
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Allowance : Dimensionless; Vanishes : Boolean)
      return Boolean
   is (Allowance >= 0.0
       and then Allowance <= Chart_Allowance * Magnitude_Limit
       and then Vanishes = (for all K in Chart_Coefficient_Index => Coefficients (K, Component) = 0.0)
       and then
         (for all K in Chart_Coefficient_Index =>
            abs Coefficients (K, Component) <= Magnitude_Limit
            and then
              R (Allowance) >= Chart_Allowance_Real * abs R (Coefficients (K, Component)) * (1.0 - Epsilon) - Eta))
   with Ghost;
   --  What Is_Chart_Prepared says of one component, whose Delta_Y and Vanishes are Allowance and Vanishes.

   function Is_Component_Prepared
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Allowance : Dimensionless; Vanishes : Boolean)
      return Boolean
   with Ghost, Post => Is_Component_Prepared'Result = Component_Holds (Coefficients, Component, Allowance, Vanishes);
   --  Component_Holds, as a function which is not expanded wherever it is mentioned.

   function Is_Component_Prepared
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Allowance : Dimensionless; Vanishes : Boolean)
      return Boolean is
   begin
      return Component_Holds (Coefficients, Component, Allowance, Vanishes);
   end Is_Component_Prepared;

   function Are_Components_Prepared
     (Coefficients : Bernstein_Chart; Delta_Y : Component_Values; Vanishes : Component_Flags) return Boolean
   is (Is_Component_Prepared (Coefficients, 1, Delta_Y (1), Vanishes (1))
       and then Is_Component_Prepared (Coefficients, 2, Delta_Y (2), Vanishes (2))
       and then Is_Component_Prepared (Coefficients, 3, Delta_Y (3), Vanishes (3)))
   with Ghost;
   --  Is_Chart_Prepared, with the three components written out: the provers handle that far better than a
   --  quantified statement whose body is itself quantified.

   procedure Lemma_Component_Is_Covered
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Largest : Dimensionless)
   with
     Ghost,
     Pre  =>
       Largest >= 0.0
       and then Largest <= Magnitude_Limit
       and then (for all K in Chart_Coefficient_Index => abs Coefficients (K, Component) <= Largest)
       and then R (Chart_Allowance * Largest) >= Chart_Allowance_Real * R (Largest) * (1.0 - Epsilon) - Eta,
     Post =>
       (for all K in Chart_Coefficient_Index =>
          abs Coefficients (K, Component) <= Magnitude_Limit
          and then
            R (Chart_Allowance * Largest)
            >= Chart_Allowance_Real * abs R (Coefficients (K, Component)) * (1.0 - Epsilon) - Eta);
   --  The allowance of the largest magnitude covers every coefficient.

   procedure Lemma_Component_Is_Covered
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Largest : Dimensionless) is
   begin
      for K in Chart_Coefficient_Index loop
         Lemma_Order (abs Coefficients (K, Component), Largest);
         Lemma_Abs_Is_Exact (Coefficients (K, Component));
         pragma
           Assert
             (Chart_Allowance_Real * abs R (Coefficients (K, Component)) * (1.0 - Epsilon)
              <= Chart_Allowance_Real * R (Largest) * (1.0 - Epsilon));
         pragma
           Loop_Invariant
             (for all J in Chart_Coefficient_Index =>
                (if J <= K
                 then
                   abs Coefficients (J, Component) <= Magnitude_Limit
                   and then
                     R (Chart_Allowance * Largest)
                     >= Chart_Allowance_Real * abs R (Coefficients (J, Component)) * (1.0 - Epsilon) - Eta));
      end loop;
   end Lemma_Component_Is_Covered;

   function Is_Axis_Checked
     (Frame    : Frame_Vector_Array;
      Cache    : Rational_Antiderivative;
      Active   : Boolean;
      Vanishes : Component_Flags;
      Axis     : Axis_Name) return Boolean
   with
     Ghost,
     Post =>
       Is_Axis_Checked'Result
       = (abs Cache.Constant_Tangent (Axis) <= Magnitude_Limit
          and then
            (if not Active
             then
               Cache.Constant_Tangent (Axis) = 0.0
               and then Frame (0) (Axis) = 0.0
               and then (Frame (1) (Axis) = 0.0 or else Vanishes (1))
               and then (Frame (2) (Axis) = 0.0 or else Vanishes (2))
               and then (Frame (3) (Axis) = 0.0 or else Vanishes (3))));
   --  What the first look at an axis establishes: its cached constant is usable, and if the axis is left
   --  inactive then that constant and every frame column which could matter are zero on it.

   function Is_Axis_Checked
     (Frame    : Frame_Vector_Array;
      Cache    : Rational_Antiderivative;
      Active   : Boolean;
      Vanishes : Component_Flags;
      Axis     : Axis_Name) return Boolean is
   begin
      return
        abs Cache.Constant_Tangent (Axis) <= Magnitude_Limit
        and then
          (if not Active
           then
             Cache.Constant_Tangent (Axis) = 0.0
             and then Frame (0) (Axis) = 0.0
             and then (Frame (1) (Axis) = 0.0 or else Vanishes (1))
             and then (Frame (2) (Axis) = 0.0 or else Vanishes (2))
             and then (Frame (3) (Axis) = 0.0 or else Vanishes (3)));
   end Is_Axis_Checked;

   procedure Lemma_Continuation_Is_Transitive (First, Second, Third : Preparation)
   with
     Ghost,
     Pre  => Is_Continuation (First, Second) and then Is_Continuation (Second, Third),
     Post => Is_Continuation (First, Third);

   procedure Lemma_Continuation_Is_Transitive (First, Second, Third : Preparation) is null;

   procedure Lemma_Component_Transfer
     (Coefficients                 : Bernstein_Chart;
      Component                    : Chart_Component_Index;
      Old_Allowance, New_Allowance : Dimensionless;
      Vanishes                     : Boolean)
   with
     Ghost,
     Pre  =>
       Is_Component_Prepared (Coefficients, Component, Old_Allowance, Vanishes)
       and then R (New_Allowance) = R (Old_Allowance),
     Post => Is_Component_Prepared (Coefficients, Component, New_Allowance, Vanishes);
   --  An allowance which denotes the same number serves as well.

   procedure Lemma_Component_Transfer
     (Coefficients                 : Bernstein_Chart;
      Component                    : Chart_Component_Index;
      Old_Allowance, New_Allowance : Dimensionless;
      Vanishes                     : Boolean) is
   begin
      Lemma_Same_Float (New_Allowance, Old_Allowance);
      pragma Assert (Component_Holds (Coefficients, Component, Old_Allowance, Vanishes));
      pragma Assert (Component_Holds (Coefficients, Component, New_Allowance, Vanishes));
   end Lemma_Component_Transfer;

   procedure Lemma_Chart_Is_Prepared (Coefficients : Bernstein_Chart; Before, After : Preparation)
   with
     Ghost,
     Pre  =>
       Are_Components_Prepared (Coefficients, Before.Delta_Y, Before.Vanishes)
       and then Is_Continuation (Before, After),
     Post => Is_Chart_Prepared (Coefficients, After);
   --  The chart's data denote the same numbers after the poles were gathered.

   procedure Lemma_Chart_Is_Prepared (Coefficients : Bernstein_Chart; Before, After : Preparation) is
   begin
      Lemma_Component_Transfer (Coefficients, 1, Before.Delta_Y (1), After.Delta_Y (1), Before.Vanishes (1));
      Lemma_Component_Transfer (Coefficients, 2, Before.Delta_Y (2), After.Delta_Y (2), Before.Vanishes (2));
      Lemma_Component_Transfer (Coefficients, 3, Before.Delta_Y (3), After.Delta_Y (3), Before.Vanishes (3));
      pragma Assert (Component_Holds (Coefficients, 1, After.Delta_Y (1), After.Vanishes (1)));
      pragma Assert (Component_Holds (Coefficients, 2, After.Delta_Y (2), After.Vanishes (2)));
      pragma Assert (Component_Holds (Coefficients, 3, After.Delta_Y (3), After.Vanishes (3)));
      pragma
        Assert
          (for all Component in Chart_Component_Index =>
             Component_Holds (Coefficients, Component, After.Delta_Y (Component), After.Vanishes (Component)));
   end Lemma_Chart_Is_Prepared;

   procedure Lemma_Frame_Is_Prepared
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Cache        : Rational_Antiderivative;
      Before       : Preparation;
      After        : Preparation)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then
         (for all Component in Frame_Component_Index =>
            (for all Axis in Axis_Name => abs Frame (Component) (Axis) <= Magnitude_Limit))
       and then
         (for all Axis in Axis_Name => Is_Axis_Checked (Frame, Cache, Before.Active (Axis), Before.Vanishes, Axis))
       and then Are_Components_Prepared (Coefficients, Before.Delta_Y, Before.Vanishes)
       and then Is_Continuation (Before, After)
       and then After.Count = Cache.Real_Pole_Count + Cache.Pair_Count
       and then Are_Idle_Axes_Empty (Cache, After),
     Post => Is_Frame_Prepared (Frame, Coefficients, Cache, After);
   --  An axis which is still inactive after the poles were gathered was inactive before, and has no residue.

   procedure Lemma_Frame_Is_Prepared
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Cache        : Rational_Antiderivative;
      Before       : Preparation;
      After        : Preparation) is
   begin
      pragma Assert (Slots_Done (Cache, After.Count) = Cache.Real_Pole_Count + 2 * Cache.Pair_Count);
      pragma Assert (Component_Holds (Coefficients, 1, Before.Delta_Y (1), Before.Vanishes (1)));
      pragma Assert (Component_Holds (Coefficients, 2, Before.Delta_Y (2), Before.Vanishes (2)));
      pragma Assert (Component_Holds (Coefficients, 3, Before.Delta_Y (3), Before.Vanishes (3)));
      for Axis in Axis_Name loop
         pragma Assert (Is_Axis_Checked (Frame, Cache, Before.Active (Axis), Before.Vanishes, Axis));
         if not After.Active (Axis) then
            pragma Assert (not Before.Active (Axis));
            pragma
              Assert
                (Frame (1) (Axis) = 0.0 or else (for all K in Chart_Coefficient_Index => Coefficients (K, 1) = 0.0));
            pragma
              Assert
                (Frame (2) (Axis) = 0.0 or else (for all K in Chart_Coefficient_Index => Coefficients (K, 2) = 0.0));
            pragma
              Assert
                (Frame (3) (Axis) = 0.0 or else (for all K in Chart_Coefficient_Index => Coefficients (K, 3) = 0.0));
            pragma
              Assert
                (for all Component in Chart_Component_Index =>
                   Frame (Component) (Axis) = 0.0
                   or else (for all K in Chart_Coefficient_Index => Coefficients (K, Component) = 0.0));
            pragma
              Assert
                (for all Slot in Rational_Degree_Slot =>
                   (if Slot <= Cache.Real_Pole_Count + 2 * Cache.Pair_Count
                    then Cache.Pole_Slots (Slot).Residue_Component (Axis) = 0.0));
         end if;
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                (if A <= Axis
                 then
                   abs Cache.Constant_Tangent (A) <= Magnitude_Limit
                   and then
                     (if not After.Active (A)
                      then
                        Cache.Constant_Tangent (A) = 0.0
                        and then Frame (0) (A) = 0.0
                        and then
                          (for all Component in Chart_Component_Index =>
                             Frame (Component) (A) = 0.0
                             or else (for all K in Chart_Coefficient_Index => Coefficients (K, Component) = 0.0))
                        and then
                          (for all Slot in Rational_Degree_Slot =>
                             (if Slot <= Cache.Real_Pole_Count + 2 * Cache.Pair_Count
                              then Cache.Pole_Slots (Slot).Residue_Component (A) = 0.0)))));
      end loop;
   end Lemma_Frame_Is_Prepared;

   procedure Lemma_Pair_Is_Off_The_Axis (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term)
   with
     Ghost,
     Pre  => Has_Layout (Cache) and then Pair <= Cache.Pair_Count and then Is_Pair_Element (Cache, Pair, Term),
     Post => Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair).Pole_Component > 0.0;

   procedure Lemma_Pair_Is_Off_The_Axis (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term)
   is null;

   procedure Lemma_Terms_Are_Prepared (Cache : Rational_Antiderivative; Prepared : Preparation)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Are_Terms_Gathered (Cache, Prepared)
       and then Prepared.Count = Cache.Real_Pole_Count + Cache.Pair_Count,
     Post =>
       (for all Index in 1 .. Prepared.Count => Is_Term_Bounded (Prepared.Terms (Index)))
       and then (for all Slot in 1 .. Cache.Real_Pole_Count => Is_Real_Term (Cache, Slot, Prepared.Terms (Slot)))
       and then
         (for all Pair in 1 .. Cache.Pair_Count =>
            Is_Pair_Term (Cache, Pair, Prepared.Terms (Cache.Real_Pole_Count + Pair)))
       and then Poles_Are_Off_The_Curve (Cache);
   --  Once every pole is gathered, the terms are those of Is_Cache_Prepared.

   procedure Lemma_Terms_Are_Prepared (Cache : Rational_Antiderivative; Prepared : Preparation) is
   begin
      pragma
        Assert (for all Slot in 1 .. Cache.Real_Pole_Count => Is_Real_Element (Cache, Slot, Prepared.Terms (Slot)));
      pragma
        Assert
          (for all Pair in 1 .. Cache.Pair_Count =>
             Is_Pair_Element (Cache, Pair, Prepared.Terms (Cache.Real_Pole_Count + Pair)));
      pragma
        Assert
          (for all Slot in 1 .. Cache.Real_Pole_Count =>
             Cache.Pole_Slots (Slot).Pole_Component < 0.0 or else Cache.Pole_Slots (Slot).Pole_Component > 1.0);
      for Pair in 1 .. Cache.Pair_Count loop
         Lemma_Pair_Is_Off_The_Axis (Cache, Pair, Prepared.Terms (Cache.Real_Pole_Count + Pair));
         pragma
           Loop_Invariant
             (for all P in 1 .. Pair => Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * P).Pole_Component > 0.0);
      end loop;
      pragma Assert (Poles_Are_Off_The_Curve (Cache));
   end Lemma_Terms_Are_Prepared;

   procedure Lemma_Cache_Is_Prepared (Cache : Rational_Antiderivative; Prepared : Preparation)
   with
     Ghost,
     Pre  =>
       Has_Layout (Cache)
       and then Is_Partly_Prepared (Cache, Prepared)
       and then Prepared.Count = Cache.Real_Pole_Count + Cache.Pair_Count,
     Post => Is_Cache_Prepared (Cache, Prepared) and then Poles_Are_Off_The_Curve (Cache);
   --  Once every pole is gathered, the partial preparation is the whole one.

   procedure Lemma_Cache_Is_Prepared (Cache : Rational_Antiderivative; Prepared : Preparation) is
   begin
      Lemma_Slope_Cap (Prepared.Count);
      for Axis in Axis_Name loop
         Lemma_Slope_Is_Within_Ceiling (Prepared.Slope (Axis));
         pragma Loop_Invariant (for all A in Axis_Name => (if A <= Axis then Prepared.Slope (A) <= Largest_Slope));
      end loop;
      Lemma_Terms_Are_Prepared (Cache, Prepared);
   end Lemma_Cache_Is_Prepared;

   function Certified_Copy (Prepared : Preparation) return Preparation
   is ((Prepared with delta Outcome => Certified))
   with Ghost;
   --  Prepared as Prepare returns it once nothing has failed.

   procedure Lemma_Is_Prepared
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Checked, Complete : Preparation)
   with
     Ghost,
     Pre  =>
       Denominator_Scale > 0.0
       and then Denominator_Scale <= Magnitude_Limit
       and then Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then
         (for all Component in Frame_Component_Index =>
            (for all Axis in Axis_Name => abs Frame (Component) (Axis) <= Magnitude_Limit))
       and then
         (for all Axis in Axis_Name => Is_Axis_Checked (Frame, Cache, Checked.Active (Axis), Checked.Vanishes, Axis))
       and then Are_Components_Prepared (Coefficients, Checked.Delta_Y, Checked.Vanishes)
       and then Is_Continuation (Checked, Complete)
       and then Complete.Count = Cache.Real_Pole_Count + Cache.Pair_Count
       and then Is_Partly_Prepared (Cache, Complete),
     Post => Is_Prepared (Frame, Coefficients, Warp, Cache, Denominator_Scale, Certified_Copy (Complete));
   --  Checked is the preparation before any pole was gathered and Complete the one after the last. Setting the
   --  outcome changes nothing which the predicates look at.

   procedure Lemma_Is_Prepared
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Checked, Complete : Preparation)
   is
      Final : constant Preparation := Certified_Copy (Complete);
   begin
      pragma Assert (Is_Continuation (Checked, Final));
      pragma Assert (Are_Terms_Gathered (Cache, Final));
      pragma Assert (Are_Idle_Axes_Empty (Cache, Final));
      for Axis in Axis_Name loop
         Lemma_Slope_Sum_Frame (Complete, Final, Final.Count, Axis);
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                (if A <= Axis then Slope_Sum (Final, Final.Count, A) = Slope_Sum (Complete, Complete.Count, A)));
      end loop;
      pragma Assert (Are_Slopes_Gathered (Final));
      Lemma_Chart_Is_Prepared (Coefficients, Checked, Final);
      Lemma_Frame_Is_Prepared (Frame, Coefficients, Cache, Checked, Final);
      Lemma_Cache_Is_Prepared (Cache, Final);
   end Lemma_Is_Prepared;

   ---------------------------------------------------------------------------
   --  Checking magnitudes and gathering every pole
   ---------------------------------------------------------------------------

   package Progress
     with Ghost
   is
      --  What Prepare carries through its loops over the poles without looking inside, for the reason given at
      --  Carried.

      function Checked_Fact
        (Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Cache        : Rational_Antiderivative;
         Checked      : Preparation) return Boolean;
      --  The chart and the axes have been looked at: Is_Component_Prepared of every component and
      --  Is_Axis_Checked of every axis.

      procedure Lemma_Fold_Checked
        (Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Cache        : Rational_Antiderivative;
         Checked      : Preparation)
      with
        Pre  =>
          Are_Components_Prepared (Coefficients, Checked.Delta_Y, Checked.Vanishes)
          and then
            (for all Axis in Axis_Name =>
               Is_Axis_Checked (Frame, Cache, Checked.Active (Axis), Checked.Vanishes, Axis)),
        Post => Checked_Fact (Frame, Coefficients, Cache, Checked);

      function Poles_Fact (Cache : Rational_Antiderivative; Prepared : Preparation) return Boolean
      with Pre => Has_Layout (Cache);
      --  Is_Partly_Prepared.

      function Kept_Fact (Before, After : Preparation) return Boolean;
      --  Is_Continuation.

      procedure Lemma_Start (Cache : Rational_Antiderivative; Prepared : Preparation)
      with
        Pre  =>
          Has_Layout (Cache)
          and then Prepared.Count = 0
          and then (for all Axis in Axis_Name => Prepared.Slope (Axis) = 0.0),
        Post => Poles_Fact (Cache, Prepared) and then Kept_Fact (Prepared, Prepared);
      --  Before any pole is gathered.

      procedure Lemma_Room (Cache : Rational_Antiderivative; Prepared : Preparation)
      with
        Pre  =>
          Has_Layout (Cache) and then Prepared.Count < Maximum_Rational_Degree and then Poles_Fact (Cache, Prepared),
        Post =>
          (for all Axis in Axis_Name =>
             Prepared.Slope (Axis) >= 0.0 and then R (Prepared.Slope (Axis)) <= 15.0 * Slope_Step);
      --  What Append requires of Slope.

      procedure Lemma_Next
        (Cache                  : Rational_Antiderivative;
         Real_Slot              : Rational_Degree_Slot;
         Imaginary_Slot         : Natural;
         Checked, Before, After : Preparation)
      with
        Pre  =>
          Has_Layout (Cache)
          and then Imaginary_Slot <= Maximum_Rational_Degree
          and then Before.Count < Maximum_Rational_Degree
          and then Poles_Fact (Cache, Before)
          and then Kept_Fact (Checked, Before)
          and then Is_Next_Pole (Cache, Before.Count, Real_Slot, Imaginary_Slot)
          and then Appending.Fact (Cache, Real_Slot, Imaginary_Slot, Before, After),
        Post => After.Count = Before.Count + 1 and then Poles_Fact (Cache, After) and then Kept_Fact (Checked, After);
      --  One more pole: what Append promises, given the next pole, carries the two facts over.

      procedure Lemma_Finish
        (Frame             : Frame_Vector_Array;
         Coefficients      : Bernstein_Chart;
         Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Checked, Complete : Preparation)
      with
        Pre  =>
          Denominator_Scale > 0.0
          and then Denominator_Scale <= Magnitude_Limit
          and then Warp >= Smallest_Warp
          and then Warp <= Largest_Warp
          and then Has_Layout (Cache)
          and then
            (for all Component in Frame_Component_Index =>
               (for all Axis in Axis_Name => abs Frame (Component) (Axis) <= Magnitude_Limit))
          and then Checked_Fact (Frame, Coefficients, Cache, Checked)
          and then Kept_Fact (Checked, Complete)
          and then Complete.Count = Cache.Real_Pole_Count + Cache.Pair_Count
          and then Poles_Fact (Cache, Complete),
        Post => Is_Prepared (Frame, Coefficients, Warp, Cache, Denominator_Scale, Certified_Copy (Complete));
      --  With every pole gathered and every frame component usable, the preparation is complete but for its
      --  outcome.

   end Progress;

   package body Progress is

      function Checked_Fact
        (Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Cache        : Rational_Antiderivative;
         Checked      : Preparation) return Boolean
      is (Are_Components_Prepared (Coefficients, Checked.Delta_Y, Checked.Vanishes)
          and then
            (for all Axis in Axis_Name =>
               Is_Axis_Checked (Frame, Cache, Checked.Active (Axis), Checked.Vanishes, Axis)));

      procedure Lemma_Fold_Checked
        (Frame        : Frame_Vector_Array;
         Coefficients : Bernstein_Chart;
         Cache        : Rational_Antiderivative;
         Checked      : Preparation)
      is null;

      function Poles_Fact (Cache : Rational_Antiderivative; Prepared : Preparation) return Boolean
      is (Is_Partly_Prepared (Cache, Prepared));

      function Kept_Fact (Before, After : Preparation) return Boolean
      is (Is_Continuation (Before, After));

      procedure Lemma_Start (Cache : Rational_Antiderivative; Prepared : Preparation) is
      begin
         pragma Assert (Are_Terms_Gathered (Cache, Prepared));
         pragma Assert (Slope_Cap (0) = 0.0);
         pragma
           Assert
             (for all Axis in Axis_Name =>
                R (Prepared.Slope (Axis)) = 0.0 and then Slope_Sum (Prepared, 0, Axis) = 0.0);
         pragma Assert (Are_Slopes_Gathered (Prepared));
         pragma Assert (Are_Idle_Axes_Empty (Cache, Prepared));
      end Lemma_Start;

      procedure Lemma_Room (Cache : Rational_Antiderivative; Prepared : Preparation) is
      begin
         Lemma_Slopes_Have_Room (Prepared);
      end Lemma_Room;

      procedure Lemma_Next
        (Cache                  : Rational_Antiderivative;
         Real_Slot              : Rational_Degree_Slot;
         Imaginary_Slot         : Natural;
         Checked, Before, After : Preparation) is
      begin
         Lemma_Appended (Cache, Real_Slot, Imaginary_Slot, Before, After);
         Appending.Lemma_Unfold (Cache, Real_Slot, Imaginary_Slot, Before, After);
         Lemma_Continuation_Is_Transitive (Checked, Before, After);
      end Lemma_Next;

      procedure Lemma_Finish
        (Frame             : Frame_Vector_Array;
         Coefficients      : Bernstein_Chart;
         Warp              : Dimensionless;
         Cache             : Rational_Antiderivative;
         Denominator_Scale : Dimensionless;
         Checked, Complete : Preparation) is
      begin
         Lemma_Is_Prepared (Frame, Coefficients, Warp, Cache, Denominator_Scale, Checked, Complete);
      end Lemma_Finish;

   end Progress;

   function Prepare
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless) return Preparation
   is
      Prepared : Preparation;
      Appended : Status;

      Checked : Preparation
      with Ghost;
      --  Prepared as it was once the chart and the axes had been looked at, before any pole was gathered.

      Previous : Preparation
      with Ghost;
      --  Prepared as it was before the latest pole was gathered.

      Complete : Preparation
      with Ghost;
      --  Prepared as it was just before its outcome was set.
   begin
      if not (Denominator_Scale > 0.0 and then Denominator_Scale <= Magnitude_Limit)
        or else not (Warp >= Smallest_Warp and then Warp <= Largest_Warp)
        or else not Has_Layout (Cache)
      then
         Prepared.Outcome := Unsafe;
         return Prepared;
      end if;

      for Component in Chart_Component_Index loop
         declare
            Largest : Dimensionless := 0.0;
         begin
            for K in Chart_Coefficient_Index loop
               Lemma_Maximum (Largest, abs Coefficients (K, Component));
               Largest := Dimensionless'Max (Largest, abs Coefficients (K, Component));
               pragma Loop_Invariant (Largest >= 0.0);
               pragma
                 Loop_Invariant
                   (for all J in Chart_Coefficient_Index =>
                      (if J <= K then abs Coefficients (J, Component) <= Largest));
               pragma
                 Loop_Invariant
                   ((Largest = 0.0)
                    = (for all J in Chart_Coefficient_Index => (if J <= K then Coefficients (J, Component) = 0.0)));
            end loop;
            if not Usable (Largest) then
               return Prepared;
            end if;
            Lemma_Chart_Scale_Range (Largest);
            Lemma_Chart_Scale_Is_Rounded (Largest);
            Lemma_Component_Is_Covered (Coefficients, Component, Largest);
            Prepared.Delta_Y (Component) := Chart_Allowance * Largest;
            Prepared.Vanishes (Component) := Largest = 0.0;
            pragma
              Assert
                (Prepared.Vanishes (Component)
                 = (for all K in Chart_Coefficient_Index => Coefficients (K, Component) = 0.0));
            pragma
              Assert
                (Component_Holds
                   (Coefficients, Component, Prepared.Delta_Y (Component), Prepared.Vanishes (Component)));
            pragma
              Assert
                (Is_Component_Prepared
                   (Coefficients, Component, Prepared.Delta_Y (Component), Prepared.Vanishes (Component)));
         end;
         pragma Warnings (Off, "condition is always True");
         --  The first of the conditions below always holds; they are written alike for each component.
         pragma
           Loop_Invariant
             ((if Component >= 1
               then Is_Component_Prepared (Coefficients, 1, Prepared.Delta_Y (1), Prepared.Vanishes (1)))
              and then
                (if Component >= 2
                 then Is_Component_Prepared (Coefficients, 2, Prepared.Delta_Y (2), Prepared.Vanishes (2)))
              and then
                (if Component >= 3
                 then Is_Component_Prepared (Coefficients, 3, Prepared.Delta_Y (3), Prepared.Vanishes (3))));
         pragma Warnings (On, "condition is always True");
      end loop;
      pragma Assert (Are_Components_Prepared (Coefficients, Prepared.Delta_Y, Prepared.Vanishes));

      --  An axis takes part unless both tangents vanish on it identically: no cached constant or residue, and no
      --  frame column which multiplies a nonzero local numerator. Such an axis contributes exactly zero error.
      for Axis in Axis_Name loop
         if not Usable (Cache.Constant_Tangent (Axis)) then
            return Prepared;
         end if;
         Prepared.Active (Axis) := Cache.Constant_Tangent (Axis) /= 0.0 or else Frame (0) (Axis) /= 0.0;
         for Component in Chart_Component_Index loop
            if Frame (Component) (Axis) /= 0.0 and then not Prepared.Vanishes (Component) then
               Prepared.Active (Axis) := True;
            end if;
            pragma
              Loop_Invariant
                (for all A in Axis_Name => (if A /= Axis then Prepared.Active (A) = Prepared.Active'Loop_Entry (A)));
            pragma
              Loop_Invariant
                (if not Prepared.Active (Axis)
                 then
                   Cache.Constant_Tangent (Axis) = 0.0
                   and then Frame (0) (Axis) = 0.0
                   and then
                     (for all C in Chart_Component_Index =>
                        (if C <= Component then Frame (C) (Axis) = 0.0 or else Prepared.Vanishes (C))));
         end loop;
         pragma Assert (Is_Axis_Checked (Frame, Cache, Prepared.Active (Axis), Prepared.Vanishes, Axis));
         pragma
           Loop_Invariant
             (for all A in Axis_Name =>
                (if A <= Axis then Is_Axis_Checked (Frame, Cache, Prepared.Active (A), Prepared.Vanishes, A)));
         pragma Loop_Invariant (Are_Components_Prepared (Coefficients, Prepared.Delta_Y, Prepared.Vanishes));
      end loop;

      Checked := Prepared;
      Progress.Lemma_Fold_Checked (Frame, Coefficients, Cache, Checked);
      Progress.Lemma_Start (Cache, Prepared);

      --  The chart and the axes are done with: this is what the gathering of the poles starts from.
      pragma
        Assert_And_Cut
          (Denominator_Scale > 0.0
           and then Denominator_Scale <= Magnitude_Limit
           and then Warp >= Smallest_Warp
           and then Warp <= Largest_Warp
           and then Has_Layout (Cache)
           and then Prepared.Count = 0
           and then Prepared.Outcome = Not_Certified
           and then Progress.Checked_Fact (Frame, Coefficients, Cache, Checked)
           and then Progress.Poles_Fact (Cache, Prepared)
           and then Progress.Kept_Fact (Checked, Prepared));

      for Index in 1 .. Cache.Real_Pole_Count loop
         Previous := Prepared;
         Progress.Lemma_Room (Cache, Prepared);
         Append (Cache, Index, 0, Prepared, Appended);
         if Appended /= Certified then
            Prepared.Outcome := Appended;
            return Prepared;
         end if;
         Progress.Lemma_Next (Cache, Index, 0, Checked, Previous, Prepared);
         pragma Loop_Invariant (Prepared.Count = Index);
         pragma Loop_Invariant (Prepared.Outcome = Not_Certified);
         pragma Loop_Invariant (Progress.Poles_Fact (Cache, Prepared));
         pragma Loop_Invariant (Progress.Kept_Fact (Checked, Prepared));
      end loop;
      pragma Assert (Prepared.Count = Cache.Real_Pole_Count);
      for Pair in 1 .. Cache.Pair_Count loop
         Previous := Prepared;
         Progress.Lemma_Room (Cache, Prepared);
         Append
           (Cache,
            Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)),
            Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)),
            Prepared,
            Appended);
         if Appended /= Certified then
            Prepared.Outcome := Appended;
            return Prepared;
         end if;
         Progress.Lemma_Next
           (Cache,
            Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)),
            Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)),
            Checked,
            Previous,
            Prepared);
         pragma Loop_Invariant (Prepared.Count = Cache.Real_Pole_Count + Pair);
         pragma Loop_Invariant (Prepared.Outcome = Not_Certified);
         pragma Loop_Invariant (Progress.Poles_Fact (Cache, Prepared));
         pragma Loop_Invariant (Progress.Kept_Fact (Checked, Prepared));
      end loop;
      pragma Assert (Prepared.Count = Cache.Real_Pole_Count + Cache.Pair_Count);

      for Component in Frame_Component_Index loop
         for Axis in Axis_Name loop
            if not Usable (Frame (Component) (Axis)) then
               return Prepared;
            end if;
            pragma
              Loop_Invariant
                (for all A in Axis_Name => (if A <= Axis then abs Frame (Component) (A) <= Magnitude_Limit));
         end loop;
         pragma
           Loop_Invariant
             (for all C in Frame_Component_Index =>
                (if C <= Component then (for all A in Axis_Name => abs Frame (C) (A) <= Magnitude_Limit)));
      end loop;
      Complete := Prepared;
      Progress.Lemma_Finish (Frame, Coefficients, Warp, Cache, Denominator_Scale, Checked, Complete);
      Prepared.Outcome := Certified;
      return Prepared;
   end Prepare;

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Poles;
