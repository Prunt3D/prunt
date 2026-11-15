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

--  Outward-rounded interval arithmetic. Every operation here is proved free of run-time errors, and proved to
--  enclose the exact real result whenever it reports a valid interval. The contracts state the enclosure for the
--  endpoints, in real arithmetic, which is all a caller needs to carry it over to every member of the operands.
--
--  No operation handles Constraint_Error. Each decides beforehand whether it could overflow: an operand whose
--  magnitude is beyond the stated limit gives the invalid interval. The limits are within a small factor of the
--  largest floating-point number, except that a product fails closed where an operand exceeds 2**511.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Intervals
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   type Interval is record
      Lower : Dimensionless := 0.0;
      --  Inclusive lower endpoint, rounded toward negative infinity.

      Upper : Dimensionless := 0.0;
      --  Inclusive upper endpoint, rounded toward positive infinity.

      Valid : Boolean := True;
      --  False when finite enclosing arithmetic could not be established.
   end record;
   --  A conservative floating-point enclosure used to certify bounds without trusting ordinary roundoff direction.

   Invalid_Interval : constant Interval := (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
   --  What every operation returns when it cannot enclose its result.

   Half_Last : constant Dimensionless := Dimensionless'Last / 2.0;
   --  Largest magnitude which the outward padding can be added to without overflow.

   Sum_Operand_Limit : constant Dimensionless := Dimensionless'Last / 4.0;
   --  Two operands within this magnitude have a sum within Half_Last.

   Product_Operand_Limit : constant Dimensionless := 2.0**511;
   --  Two operands within this magnitude have a product within Half_Last.

   Saturation_Limit : constant Dimensionless := Dimensionless'Last * (1.0 - 2.0**(-50));
   --  Up and Down saturate at the largest magnitude from here on, since their padding could overflow beyond it.

   ---------------------------------------------------------------------------
   --  Specification
   ---------------------------------------------------------------------------

   function Is_Finite (Value : Dimensionless) return Boolean
   is (Value >= -Dimensionless'Last and then Value <= Dimensionless'Last);
   --  Return True only for an ordinary finite floating-point value. The ordered comparisons reject both infinities
   --  and NaNs without invoking the heavier runtime validity-classification helper.

   function Is_Ordered (Value : Interval) return Boolean
   is (not Value.Valid or else Value.Lower <= Value.Upper);
   --  Every interval which the operations below return is ordered.

   function Contains (Value : Interval; Point : Real) return Boolean
   is (Value.Valid and then R (Value.Lower) <= Point and then Point <= R (Value.Upper))
   with Ghost;
   --  Whether the real number Point is a member of a valid interval.

   function Is_Zero (Value : Interval) return Boolean
   is (Value.Lower = 0.0 and then Value.Upper = 0.0);

   function Is_Usable (Value : Interval) return Boolean
   is (Value.Valid
       and then Value.Lower > -Dimensionless'Last
       and then Value.Upper < Dimensionless'Last
       and then Value.Lower <= Value.Upper);
   --  Valid, ordered, and with neither endpoint saturated.

   ---------------------------------------------------------------------------
   --  Outward rounding of one value
   ---------------------------------------------------------------------------

   function Roundoff_Padding (Value : Dimensionless) return Dimensionless
   is (if 2.0 * Dimensionless'Model_Epsilon * abs Value > Dimensionless'Model_Small
       then 2.0 * Dimensionless'Model_Epsilon * abs Value
       else Dimensionless'Model_Small)
   with Inline, Post => Roundoff_Padding'Result >= Dimensionless'Model_Small;
   --  Two model epsilons of Value, or Model_Small near zero: more than the rounding error of one operation whose
   --  result is Value.

   function Inflate_Finite (Value : Dimensionless; Upward : Boolean) return Dimensionless
   with
     Inline,
     Pre  => abs Value <= Half_Last,
     Post =>
       Inflate_Finite'Result = (if Upward then Value + Roundoff_Padding (Value) else Value - Roundoff_Padding (Value))
       and then (if Upward then Inflate_Finite'Result >= Value else Inflate_Finite'Result <= Value)
       and then Is_Finite (Inflate_Finite'Result);
   --  Move Value outward by its padding. Within Half_Last this needs neither an overflow check nor a fallback to
   --  the adjacent machine number.

   procedure Inflate_Finite_Endpoints (Lower, Upper : in out Dimensionless)
   with
     Inline,
     Pre  => abs Lower <= Half_Last and then abs Upper <= Half_Last,
     Post => Lower = Inflate_Finite (Lower'Old, False) and then Upper = Inflate_Finite (Upper'Old, True);

   function Down (Value : Dimensionless) return Dimensionless
   with
     Post =>
       Down'Result <= Value
       and then Is_Finite (Down'Result)
       and then (if abs Value <= Half_Last then Down'Result = Inflate_Finite (Value, False));
   --  Move a finite value far enough downward to conservatively cover one rounded arithmetic operation.

   function Up (Value : Dimensionless) return Dimensionless
   with
     Post =>
       Up'Result >= Value
       and then Is_Finite (Up'Result)
       and then (if abs Value <= Half_Last then Up'Result = Inflate_Finite (Value, True));
   --  Move a finite value far enough upward to conservatively cover one rounded arithmetic operation.

   function Is_Rounding_Of (Value : Dimensionless; Exact : Real) return Boolean
   is (abs (R (Value) - Exact) <= Epsilon * abs Exact + Eta)
   with Ghost;
   --  Whether Value can be the result of one correctly rounded operation whose exact result is Exact.

   function Slack (Value : Dimensionless) return Real
   is (Slack_Factor * abs (R (Value)) + Twice_Eta)
   with Ghost;
   --  More than the distance from Value to the exact result of any one rounded operation which gave it, and of
   --  any two in succession, yet less than the outward padding moves Value.

   procedure Lemma_Rounding_Slack (Value : Dimensionless; Exact : Real)
   with Ghost, Pre => Is_Rounding_Of (Value, Exact), Post => abs (R (Value) - Exact) <= Slack (Value);

   procedure Lemma_Inflate_Margin (Value : Dimensionless)
   with
     Ghost,
     Pre  => abs Value <= Half_Last,
     Post =>
       R (Inflate_Finite (Value, False)) <= R (Value) - Slack (Value)
       and then R (Value) + Slack (Value) <= R (Inflate_Finite (Value, True));
   --  The padding moves Value by more than its slack, after the rounding of the padding itself.

   procedure Lemma_Inflate_Encloses (Value : Dimensionless; Exact : Real)
   with
     Ghost,
     Pre  => abs Value <= Half_Last and then Is_Rounding_Of (Value, Exact),
     Post => R (Inflate_Finite (Value, False)) <= Exact and then Exact <= R (Inflate_Finite (Value, True));
   --  The padding exceeds the rounding error of the operation which produced Value.

   ---------------------------------------------------------------------------
   --  Intervals
   ---------------------------------------------------------------------------

   function Checked_Interval (Lower, Upper : Dimensionless; Valid : Boolean := True) return Interval
   with
     Post =>
       Checked_Interval'Result
       = (if Valid and then Is_Finite (Lower) and then Is_Finite (Upper) and then Lower <= Upper
          then (Lower => Lower, Upper => Upper, Valid => True)
          else Invalid_Interval)
       and then Is_Ordered (Checked_Interval'Result)
       and then
         Checked_Interval'Result.Valid
         = (Valid and then Is_Finite (Lower) and then Is_Finite (Upper) and then Lower <= Upper)
       and then
         (if Checked_Interval'Result.Valid
          then Checked_Interval'Result.Lower = Lower and then Checked_Interval'Result.Upper = Upper
          else
            Checked_Interval'Result.Lower = -Dimensionless'Last
            and then Checked_Interval'Result.Upper = Dimensionless'Last);
   --  Construct an interval when its endpoints are finite and ordered; otherwise return the invalid full-range
   --  marker.

   function Interval_Exact (Value : Dimensionless) return Interval
   with
     Post =>
       Interval_Exact'Result
       = (if Is_Finite (Value) then (Lower => Value, Upper => Value, Valid => True) else Invalid_Interval)
       and then Is_Ordered (Interval_Exact'Result)
       and then Interval_Exact'Result.Valid = Is_Finite (Value)
       and then
         (if Interval_Exact'Result.Valid
          then Interval_Exact'Result.Lower = Value and then Interval_Exact'Result.Upper = Value);
   --  Enclose one already-represented input value without adding uncertainty merely for loading it.

   function Interval_Add (Left, Right : Interval) return Interval
   with
     Post =>
       Is_Ordered (Interval_Add'Result)
       and then (if not Interval_Add'Result.Valid then Interval_Add'Result = Invalid_Interval)
       and then
         (if Interval_Add'Result.Valid
          then
            Left.Valid
            and then Right.Valid
            and then R (Interval_Add'Result.Lower) <= R (Left.Lower) + R (Right.Lower)
            and then R (Left.Upper) + R (Right.Upper) <= R (Interval_Add'Result.Upper));
   --  Enclose every sum formed by choosing one value from Left and one from Right.

   function Interval_Negate (Value : Interval) return Interval
   with
     Post =>
       Is_Ordered (Interval_Negate'Result)
       and then (if not Interval_Negate'Result.Valid then Interval_Negate'Result = Invalid_Interval)
       and then
         (if Interval_Negate'Result.Valid
          then
            Value.Valid
            and then Interval_Negate'Result.Lower = -Value.Upper
            and then Interval_Negate'Result.Upper = -Value.Lower);
   --  Enclose the additive inverse of every member of Value.

   function Interval_Subtract (Left, Right : Interval) return Interval
   with
     Post =>
       Is_Ordered (Interval_Subtract'Result)
       and then (if not Interval_Subtract'Result.Valid then Interval_Subtract'Result = Invalid_Interval)
       and then
         (if Interval_Subtract'Result.Valid
          then
            Left.Valid
            and then Right.Valid
            and then R (Interval_Subtract'Result.Lower) <= R (Left.Lower) - R (Right.Upper)
            and then R (Left.Upper) - R (Right.Lower) <= R (Interval_Subtract'Result.Upper));
   --  Enclose every difference formed by choosing one value from Left and one from Right.

   function Encloses_Products (Result, Left, Right : Interval) return Boolean
   is (R (Result.Lower) <= R (Left.Lower) * R (Right.Lower)
       and then R (Result.Lower) <= R (Left.Lower) * R (Right.Upper)
       and then R (Result.Lower) <= R (Left.Upper) * R (Right.Lower)
       and then R (Result.Lower) <= R (Left.Upper) * R (Right.Upper)
       and then R (Left.Lower) * R (Right.Lower) <= R (Result.Upper)
       and then R (Left.Lower) * R (Right.Upper) <= R (Result.Upper)
       and then R (Left.Upper) * R (Right.Lower) <= R (Result.Upper)
       and then R (Left.Upper) * R (Right.Upper) <= R (Result.Upper))
   with Ghost;
   --  Whether Result reaches from below the least to above the greatest product of an endpoint of Left with an
   --  endpoint of Right. A product of members of two ordered intervals lies between those extremes.

   function Below_Products (Bound : Real; Left, Right : Interval) return Boolean
   is (Bound <= R (Left.Lower) * R (Right.Lower)
       and then Bound <= R (Left.Lower) * R (Right.Upper)
       and then Bound <= R (Left.Upper) * R (Right.Lower)
       and then Bound <= R (Left.Upper) * R (Right.Upper))
   with Ghost;

   function Above_Products (Bound : Real; Left, Right : Interval) return Boolean
   is (Bound >= R (Left.Lower) * R (Right.Lower)
       and then Bound >= R (Left.Lower) * R (Right.Upper)
       and then Bound >= R (Left.Upper) * R (Right.Lower)
       and then Bound >= R (Left.Upper) * R (Right.Upper))
   with Ghost;

   procedure Lemma_Slack_Is_Monotonic (Low, High : Dimensionless)
   with
     Ghost,
     Pre  => Low <= High,
     Post =>
       R (Low) - Slack (Low) <= R (High) - Slack (High) and then R (Low) + Slack (Low) <= R (High) + Slack (High);
   --  Moving a value outward by its slack keeps the order of two values.

   procedure Lemma_Product_Is_Bounded (A, B : Dimensionless)
   with
     Ghost,
     Pre  => abs A <= Product_Operand_Limit and then abs B <= Product_Operand_Limit,
     Post => abs (A * B) <= Half_Last and then Is_Rounding_Of (A * B, R (A) * R (B));
   --  A product of two operands within the limit cannot overflow, and is one rounded operation.

   procedure Interval_Product_Extrema (Left, Right : Interval; Lower_Product, Upper_Product : out Dimensionless)
   with
     Inline,
     Pre  =>
       abs Left.Lower <= Product_Operand_Limit
       and then abs Left.Upper <= Product_Operand_Limit
       and then abs Right.Lower <= Product_Operand_Limit
       and then abs Right.Upper <= Product_Operand_Limit,
     Post =>
       abs Lower_Product <= Half_Last
       and then abs Upper_Product <= Half_Last
       and then
         (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
          then
            Below_Products (R (Lower_Product) - Slack (Lower_Product), Left, Right)
            and then Above_Products (R (Upper_Product) + Slack (Upper_Product), Left, Right));
   --  Form the extremal floating-point corner products. Each is within its slack of the least or the greatest
   --  exact corner product.

   function Interval_Multiply (Left, Right : Interval) return Interval
   with
     Post =>
       Is_Ordered (Interval_Multiply'Result)
       and then (if not Interval_Multiply'Result.Valid then Interval_Multiply'Result = Invalid_Interval)
       and then
         (if Interval_Multiply'Result.Valid
          then
            Left.Valid
            and then Right.Valid
            and then
              (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
               then Encloses_Products (Interval_Multiply'Result, Left, Right)));
   --  Enclose every product formed by choosing one value from each input interval.

   function Interval_Abs_Max (Value : Interval) return Dimensionless
   with
     Post =>
       Interval_Abs_Max'Result >= 0.0
       and then
         (if Value.Valid
          then Interval_Abs_Max'Result >= abs Value.Lower and then Interval_Abs_Max'Result >= abs Value.Upper
          else Interval_Abs_Max'Result = Dimensionless'Last);
   --  Return a conservative upper bound on the absolute value of every member of Value.

   ---------------------------------------------------------------------------
   --  Membership carried through each operation
   ---------------------------------------------------------------------------

   procedure Lemma_Product_Between_Corners (X, A, B, Y, C, D, Low, High : Real)
   with
     Ghost,
     Pre  =>
       A <= X
       and then X <= B
       and then C <= Y
       and then Y <= D
       and then Low <= A * C
       and then Low <= A * D
       and then Low <= B * C
       and then Low <= B * D
       and then A * C <= High
       and then A * D <= High
       and then B * C <= High
       and then B * D <= High,
     Post => Low <= X * Y and then X * Y <= High;
   --  A product is bilinear, so over a rectangle it takes its extremes at the corners.

   procedure Lemma_Add_Contains (Left, Right : Interval; X, Y : Real)
   with
     Ghost,
     Pre  => Contains (Left, X) and then Contains (Right, Y) and then Interval_Add (Left, Right).Valid,
     Post => Contains (Interval_Add (Left, Right), X + Y);

   procedure Lemma_Subtract_Contains (Left, Right : Interval; X, Y : Real)
   with
     Ghost,
     Pre  => Contains (Left, X) and then Contains (Right, Y) and then Interval_Subtract (Left, Right).Valid,
     Post => Contains (Interval_Subtract (Left, Right), X - Y);

   procedure Lemma_Multiply_Contains (Left, Right : Interval; X, Y : Real)
   with
     Ghost,
     Pre  => Contains (Left, X) and then Contains (Right, Y) and then Interval_Multiply (Left, Right).Valid,
     Post => Contains (Interval_Multiply (Left, Right), X * Y);

end Prunt.Motion_Planner.Stereographic_Kernel.Intervals;
