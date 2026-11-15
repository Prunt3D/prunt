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
pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

package body Prunt.Motion_Planner.Stereographic_Kernel.Intervals
  with SPARK_Mode => On
is
   Pad_Scale : constant := 2.0**(-51);
   --  Two model epsilons, as a real number.

   Smallest_Pad : constant := 2.0**(-1022);
   --  Model_Small, as a real number.

   Padding_Crossover : constant := 2.0**(-971);
   --  The magnitude at which two model epsilons of a value reach Model_Small.

   ---------------------------------------------------------------------------
   --  Facts about the padding
   ---------------------------------------------------------------------------

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
         R (Roundoff_Padding (Value)) >= Pad_Scale * abs (R (Value)) - Epsilon * Pad_Scale * abs (R (Value)) - Eta
       and then
         R (Roundoff_Padding (Value))
         <= Pad_Scale * abs (R (Value)) + Epsilon * Pad_Scale * abs (R (Value)) + Eta + Smallest_Pad;

   procedure Lemma_Padding_Bounds (Value : Dimensionless) is
   begin
      Lemma_Scale (abs Value);
   end Lemma_Padding_Bounds;

   ---------------------------------------------------------------------------
   --  Outward rounding of one value
   ---------------------------------------------------------------------------

   function Inflate_Finite (Value : Dimensionless; Upward : Boolean) return Dimensionless is
      pragma Suppress (Overflow_Check);
      --  Roundoff_Padding's formula without its overflow check. Not 'Max: GNAT guards that with a NaN test, which
      --  measurably slows the interval kernels this is inlined into. Both forms give Model_Small for NaN.
      Relative : constant Dimensionless := 2.0 * Dimensionless'Model_Epsilon * abs Value;
      Padding  : constant Dimensionless :=
        (if Relative > Dimensionless'Model_Small then Relative else Dimensionless'Model_Small);
   begin
      return (if Upward then Value + Padding else Value - Padding);
   end Inflate_Finite;

   procedure Inflate_Finite_Endpoints (Lower, Upper : in out Dimensionless) is
   begin
      Lower := Inflate_Finite (Lower, False);
      Upper := Inflate_Finite (Upper, True);
   end Inflate_Finite_Endpoints;

   function Down (Value : Dimensionless) return Dimensionless is
      Candidate : Dimensionless;
   begin
      if Value >= -Half_Last and then Value <= Half_Last then
         return Inflate_Finite (Value, False);
      elsif Value <= -Saturation_Limit then
         return -Dimensionless'Last;
      end if;
      --  Move by at least two model epsilons, falling back to the adjacent number when subtraction rounds back to
      --  Value.
      Candidate := Value - Roundoff_Padding (Value);
      return (if Candidate < Value then Candidate else Dimensionless'Pred (Value));
   end Down;

   function Up (Value : Dimensionless) return Dimensionless is
      Candidate : Dimensionless;
   begin
      if Value >= -Half_Last and then Value <= Half_Last then
         return Inflate_Finite (Value, True);
      elsif Value >= Saturation_Limit then
         return Dimensionless'Last;
      end if;
      --  Move by at least two model epsilons, falling back to the adjacent number when addition rounds back to
      --  Value.
      Candidate := Value + Roundoff_Padding (Value);
      return (if Candidate > Value then Candidate else Dimensionless'Succ (Value));
   end Up;

   procedure Lemma_Rounding_Slack (Value : Dimensionless; Exact : Real) is
   begin
      --  |Exact| <= (|Value| + Eta) / (1 - Epsilon), so the error is below twice Epsilon of Value and twice Eta.
      pragma Assert (abs Exact <= abs (R (Value)) + Epsilon * abs Exact + Eta);
      pragma Assert (abs Exact <= 2.0 * abs (R (Value)) + 2.0 * Eta);
      pragma Assert (Epsilon * abs Exact <= Slack_Factor * abs (R (Value)) + Eta);
   end Lemma_Rounding_Slack;

   procedure Lemma_Inflate_Margin (Value : Dimensionless) is
      Pad : constant Dimensionless := Roundoff_Padding (Value);
   begin
      Lemma_Padding_Bounds (Value);
      Float_Lemmas.Lemma_Rounding_Error_Add (Value, Pad);
      Float_Lemmas.Lemma_Rounding_Error_Sub (Value, Pad);
      pragma Assert (Value + Pad = Inflate_Finite (Value, True));
      pragma Assert (Value - Pad = Inflate_Finite (Value, False));
      pragma Assert (R (Value + Pad) >= R (Value) + R (Pad) - Epsilon * abs (R (Value) + R (Pad)) - Eta);
      pragma Assert (R (Value - Pad) <= R (Value) - R (Pad) + Epsilon * abs (R (Value) - R (Pad)) + Eta);
      if abs (R (Value)) >= Padding_Crossover then
         --  The padding is two model epsilons of Value, which is four times Epsilon.
         pragma Assert (R (Pad) >= Pad_Scale * abs (R (Value)) - Epsilon * Pad_Scale * abs (R (Value)) - Eta);
      else
         --  The padding is Model_Small, which dwarfs both Eta and Epsilon of so small a Value.
         pragma Assert (R (Pad) >= Smallest_Pad);
      end if;
      pragma Assert (R (Value + Pad) >= R (Value) + Slack (Value));
      pragma Assert (R (Value - Pad) <= R (Value) - Slack (Value));
   end Lemma_Inflate_Margin;

   procedure Lemma_Inflate_Encloses (Value : Dimensionless; Exact : Real) is
   begin
      Lemma_Rounding_Slack (Value, Exact);
      Lemma_Inflate_Margin (Value);
   end Lemma_Inflate_Encloses;

   ---------------------------------------------------------------------------
   --  Intervals
   ---------------------------------------------------------------------------

   function Checked_Interval (Lower, Upper : Dimensionless; Valid : Boolean := True) return Interval is
   begin
      if Valid and then Is_Finite (Lower) and then Is_Finite (Upper) and then Lower <= Upper then
         return (Lower => Lower, Upper => Upper, Valid => True);
      else
         return Invalid_Interval;
      end if;
   end Checked_Interval;

   function Interval_Exact (Value : Dimensionless) return Interval is
   begin
      --  Values already stored in the curve, exact integers, and dyadic cell boundaries are the authoritative inputs
      --  to the implemented formula. They do not acquire uncertainty merely by being loaded. Arithmetic involving
      --  them is still expanded after every primitive operation below.
      return Checked_Interval (Value, Value, Is_Finite (Value));
   end Interval_Exact;

   function Interval_Add (Left, Right : Interval) return Interval is
   begin
      if not Left.Valid or else not Right.Valid then
         return Invalid_Interval;
      elsif Left.Lower = 0.0 and then Left.Upper = 0.0 then
         return (if Right.Lower <= Right.Upper then Right else Invalid_Interval);
      elsif Right.Lower = 0.0 and then Right.Upper = 0.0 then
         return (if Left.Lower <= Left.Upper then Left else Invalid_Interval);
      elsif not (abs Left.Lower <= Sum_Operand_Limit)
        or else not (abs Left.Upper <= Sum_Operand_Limit)
        or else not (abs Right.Lower <= Sum_Operand_Limit)
        or else not (abs Right.Upper <= Sum_Operand_Limit)
      then
         --  The sums could overflow. The comparisons fail for a NaN as well.
         return Invalid_Interval;
      end if;
      declare
         pragma Suppress (Overflow_Check);
         Lower_Sum : constant Dimensionless := Left.Lower + Right.Lower;
         Upper_Sum : constant Dimensionless := Left.Upper + Right.Upper;
      begin
         Float_Lemmas.Lemma_Rounding_Error_Add (Left.Lower, Right.Lower);
         Float_Lemmas.Lemma_Rounding_Error_Add (Left.Upper, Right.Upper);
         Lemma_Inflate_Encloses (Lower_Sum, R (Left.Lower) + R (Right.Lower));
         Lemma_Inflate_Encloses (Upper_Sum, R (Left.Upper) + R (Right.Upper));
         return Checked_Interval (Inflate_Finite (Lower_Sum, False), Inflate_Finite (Upper_Sum, True));
      end;
   end Interval_Add;

   function Interval_Negate (Value : Interval) return Interval is
   begin
      if not Value.Valid then
         return Invalid_Interval;
      elsif Value.Lower = 0.0 and then Value.Upper = 0.0 then
         return Value;
      end if;
      --  Unary negation is exact for every finite model number; only reverse the interval endpoints. Inflating here
      --  makes the later subtraction of nearby stored coordinates scale with their absolute coordinates instead of
      --  with the small correction being enclosed.
      return Checked_Interval (-Value.Upper, -Value.Lower, Value.Valid);
   end Interval_Negate;

   function Interval_Subtract (Left, Right : Interval) return Interval is
   begin
      return Interval_Add (Left, Interval_Negate (Right));
   end Interval_Subtract;

   procedure Lemma_Real_Product_Bound (X, Y, Limit : Real)
   with Ghost, Pre => abs X <= Limit and then abs Y <= Limit, Post => abs (X * Y) <= Limit * Limit;

   procedure Lemma_Real_Product_Bound (X, Y, Limit : Real) is
   begin
      pragma Assert (abs X * abs Y <= Limit * abs Y);
      pragma Assert (Limit * abs Y <= Limit * Limit);
   end Lemma_Real_Product_Bound;

   procedure Lemma_Product_Is_Bounded (A, B : Dimensionless) is
   begin
      Lemma_Real_Product_Bound (R (A), R (B), Square_Root_Of_Last);
      Float_Lemmas.Lemma_Rounding_Error_Mul (A, B);
   end Lemma_Product_Is_Bounded;

   procedure Lemma_Slack_Is_Monotonic (Low, High : Dimensionless) is null;

   procedure Interval_Product_Extrema (Left, Right : Interval; Lower_Product, Upper_Product : out Dimensionless) is
      pragma Suppress (Overflow_Check);

      procedure Prove_Rounded (A, B : Dimensionless)
      with
        Ghost,
        Pre  => abs A <= Product_Operand_Limit and then abs B <= Product_Operand_Limit,
        Post =>
          abs (A * B) <= Half_Last
          and then R (A * B) - Slack (A * B) <= R (A) * R (B)
          and then R (A) * R (B) <= R (A * B) + Slack (A * B);

      procedure Prove_Rounded (A, B : Dimensionless) is
      begin
         Lemma_Product_Is_Bounded (A, B);
         Lemma_Rounding_Slack (A * B, R (A) * R (B));
      end Prove_Rounded;

      procedure Prove_Ordered (Low, High, Factor : Real)
      with
        Ghost,
        Pre  => Low <= High,
        Post => (if Factor >= 0.0 then Low * Factor <= High * Factor else High * Factor <= Low * Factor);
      --  Multiplying by a factor keeps or reverses an order according to the factor's sign.

      procedure Prove_Ordered (Low, High, Factor : Real) is null;

      A : constant Real := R (Left.Lower)
      with Ghost;
      B : constant Real := R (Left.Upper)
      with Ghost;
      C : constant Real := R (Right.Lower)
      with Ghost;
      D : constant Real := R (Right.Upper)
      with Ghost;
   begin
      Prove_Rounded (Left.Lower, Right.Lower);
      Prove_Rounded (Left.Lower, Right.Upper);
      Prove_Rounded (Left.Upper, Right.Lower);
      Prove_Rounded (Left.Upper, Right.Upper);
      if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper then
         Prove_Ordered (A, B, C);
         Prove_Ordered (A, B, D);
         Prove_Ordered (C, D, A);
         Prove_Ordered (C, D, B);
         pragma Assert (if C >= 0.0 then A * C <= B * C else B * C <= A * C);
         pragma Assert (if D >= 0.0 then A * D <= B * D else B * D <= A * D);
         pragma Assert (if A >= 0.0 then A * C <= A * D else A * D <= A * C);
         pragma Assert (if B >= 0.0 then B * C <= B * D else B * D <= B * C);
      end if;
      --  Except when both intervals span zero, their signs identify the two corner products which attain the
      --  product interval's extrema. Avoid forming the other two products and the four-way min/max in the common
      --  cases; all binomial weights and denominator reciprocals take one of these two-product paths.
      if Left.Lower >= 0.0 then
         if Right.Lower >= 0.0 then
            Lower_Product := Left.Lower * Right.Lower;
            Upper_Product := Left.Upper * Right.Upper;
            pragma
              Assert
                (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
                 then
                   A * C <= A * C
                   and then A * C <= A * D
                   and then A * C <= B * C
                   and then A * C <= B * D
                   and then B * D >= A * C
                   and then B * D >= A * D
                   and then B * D >= B * C
                   and then B * D >= B * D);
         elsif Right.Upper <= 0.0 then
            Lower_Product := Left.Upper * Right.Lower;
            Upper_Product := Left.Lower * Right.Upper;
            pragma
              Assert
                (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
                 then
                   B * C <= A * C
                   and then B * C <= A * D
                   and then B * C <= B * C
                   and then B * C <= B * D
                   and then A * D >= A * C
                   and then A * D >= A * D
                   and then A * D >= B * C
                   and then A * D >= B * D);
         else
            Lower_Product := Left.Upper * Right.Lower;
            Upper_Product := Left.Upper * Right.Upper;
            pragma
              Assert
                (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
                 then
                   B * C <= A * C
                   and then B * C <= A * D
                   and then B * C <= B * C
                   and then B * C <= B * D
                   and then B * D >= A * C
                   and then B * D >= A * D
                   and then B * D >= B * C
                   and then B * D >= B * D);
         end if;
      elsif Left.Upper <= 0.0 then
         if Right.Lower >= 0.0 then
            Lower_Product := Left.Lower * Right.Upper;
            Upper_Product := Left.Upper * Right.Lower;
            pragma
              Assert
                (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
                 then
                   A * D <= A * C
                   and then A * D <= A * D
                   and then A * D <= B * C
                   and then A * D <= B * D
                   and then B * C >= A * C
                   and then B * C >= A * D
                   and then B * C >= B * C
                   and then B * C >= B * D);
         elsif Right.Upper <= 0.0 then
            Lower_Product := Left.Upper * Right.Upper;
            Upper_Product := Left.Lower * Right.Lower;
            pragma
              Assert
                (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
                 then
                   B * D <= A * C
                   and then B * D <= A * D
                   and then B * D <= B * C
                   and then B * D <= B * D
                   and then A * C >= A * C
                   and then A * C >= A * D
                   and then A * C >= B * C
                   and then A * C >= B * D);
         else
            Lower_Product := Left.Lower * Right.Upper;
            Upper_Product := Left.Lower * Right.Lower;
            pragma
              Assert
                (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
                 then
                   A * D <= A * C
                   and then A * D <= A * D
                   and then A * D <= B * C
                   and then A * D <= B * D
                   and then A * C >= A * C
                   and then A * C >= A * D
                   and then A * C >= B * C
                   and then A * C >= B * D);
         end if;
      elsif Right.Lower >= 0.0 then
         Lower_Product := Left.Lower * Right.Upper;
         Upper_Product := Left.Upper * Right.Upper;
         pragma
           Assert
             (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
              then
                A * D <= A * C
                and then A * D <= A * D
                and then A * D <= B * C
                and then A * D <= B * D
                and then B * D >= A * C
                and then B * D >= A * D
                and then B * D >= B * C
                and then B * D >= B * D);
      elsif Right.Upper <= 0.0 then
         Lower_Product := Left.Upper * Right.Lower;
         Upper_Product := Left.Lower * Right.Lower;
         pragma
           Assert
             (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
              then
                B * C <= A * C
                and then B * C <= A * D
                and then B * C <= B * C
                and then B * C <= B * D
                and then A * C >= A * C
                and then A * C >= A * D
                and then A * C >= B * C
                and then A * C >= B * D);
      else
         Lower_Product := Dimensionless'Min (Left.Lower * Right.Upper, Left.Upper * Right.Lower);
         Upper_Product := Dimensionless'Max (Left.Lower * Right.Lower, Left.Upper * Right.Upper);
         Lemma_Slack_Is_Monotonic (Lower_Product, Left.Lower * Right.Upper);
         Lemma_Slack_Is_Monotonic (Lower_Product, Left.Upper * Right.Lower);
         Lemma_Slack_Is_Monotonic (Left.Lower * Right.Lower, Upper_Product);
         Lemma_Slack_Is_Monotonic (Left.Upper * Right.Upper, Upper_Product);
         pragma
           Assert
             (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
              then A * D <= A * C and then A * D <= B * D and then B * C <= A * C and then B * C <= B * D);
         pragma
           Assert
             (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
              then
                Below_Products (R (Lower_Product) - Slack (Lower_Product), Left, Right)
                and then Above_Products (R (Upper_Product) + Slack (Upper_Product), Left, Right));
      end if;
   end Interval_Product_Extrema;

   function Interval_Multiply (Left, Right : Interval) return Interval is
   begin
      if not Left.Valid or else not Right.Valid then
         return Invalid_Interval;
      elsif (Left.Lower = 0.0 and then Left.Upper = 0.0) or else (Right.Lower = 0.0 and then Right.Upper = 0.0) then
         pragma Assert (Encloses_Products (Interval_Exact (0.0), Left, Right));
         pragma Assert (Is_Ordered (Interval_Exact (0.0)));
         return Interval_Exact (0.0);
      elsif Left.Lower = 1.0 and then Left.Upper = 1.0 then
         pragma Assert (if Right.Lower <= Right.Upper then Encloses_Products (Right, Left, Right));
         return (if Right.Lower <= Right.Upper then Right else Invalid_Interval);
      elsif Right.Lower = 1.0 and then Right.Upper = 1.0 then
         pragma Assert (if Left.Lower <= Left.Upper then Encloses_Products (Left, Left, Right));
         return (if Left.Lower <= Left.Upper then Left else Invalid_Interval);
      elsif Left.Lower = -1.0 and then Left.Upper = -1.0 then
         pragma
           Assert
             (if Right.Lower <= Right.Upper and then Interval_Negate (Right).Valid
              then Encloses_Products (Interval_Negate (Right), Left, Right));
         return Interval_Negate (Right);
      elsif Right.Lower = -1.0 and then Right.Upper = -1.0 then
         pragma
           Assert
             (if Left.Lower <= Left.Upper and then Interval_Negate (Left).Valid
              then Encloses_Products (Interval_Negate (Left), Left, Right));
         return Interval_Negate (Left);
      elsif not (abs Left.Lower <= Product_Operand_Limit)
        or else not (abs Left.Upper <= Product_Operand_Limit)
        or else not (abs Right.Lower <= Product_Operand_Limit)
        or else not (abs Right.Upper <= Product_Operand_Limit)
      then
         --  The products could overflow. The comparisons fail for a NaN as well.
         return Invalid_Interval;
      end if;
      declare
         Lower_Product : Dimensionless;
         Upper_Product : Dimensionless;
      begin
         Interval_Product_Extrema (Left, Right, Lower_Product, Upper_Product);
         Lemma_Inflate_Margin (Lower_Product);
         Lemma_Inflate_Margin (Upper_Product);
         Lower_Product := Inflate_Finite (Lower_Product, False);
         Upper_Product := Inflate_Finite (Upper_Product, True);
         pragma
           Assert
             (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
              then
                Below_Products (R (Lower_Product), Left, Right)
                and then Above_Products (R (Upper_Product), Left, Right));
         declare
            Result : constant Interval := Checked_Interval (Lower_Product, Upper_Product);
         begin
            pragma
              Assert
                (if Result.Valid and then Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
                 then Encloses_Products (Result, Left, Right));
            return Result;
         end;
      end;
   end Interval_Multiply;

   function Interval_Abs_Max (Value : Interval) return Dimensionless is
   begin
      if not Value.Valid then
         return Dimensionless'Last;
      else
         return Dimensionless'Max (abs Value.Lower, abs Value.Upper);
      end if;
   end Interval_Abs_Max;

   ---------------------------------------------------------------------------
   --  Membership carried through each operation
   ---------------------------------------------------------------------------

   procedure Lemma_Product_Between_Corners (X, A, B, Y, C, D, Low, High : Real) is
   begin
      --  For fixed Y the product is linear in X, so it lies between A * Y and B * Y; each of those is linear in Y.
      if Y >= 0.0 then
         pragma Assert (A * Y <= X * Y and then X * Y <= B * Y);
      else
         pragma Assert (B * Y <= X * Y and then X * Y <= A * Y);
      end if;
      if A >= 0.0 then
         pragma Assert (A * C <= A * Y and then A * Y <= A * D);
      else
         pragma Assert (A * D <= A * Y and then A * Y <= A * C);
      end if;
      if B >= 0.0 then
         pragma Assert (B * C <= B * Y and then B * Y <= B * D);
      else
         pragma Assert (B * D <= B * Y and then B * Y <= B * C);
      end if;
   end Lemma_Product_Between_Corners;

   procedure Lemma_Add_Contains (Left, Right : Interval; X, Y : Real) is null;

   procedure Lemma_Subtract_Contains (Left, Right : Interval; X, Y : Real) is null;

   procedure Lemma_Multiply_Contains (Left, Right : Interval; X, Y : Real) is
      Result : constant Interval := Interval_Multiply (Left, Right);
   begin
      Lemma_Product_Between_Corners
        (X    => X,
         A    => R (Left.Lower),
         B    => R (Left.Upper),
         Y    => Y,
         C    => R (Right.Lower),
         D    => R (Right.Upper),
         Low  => R (Result.Lower),
         High => R (Result.Upper));
   end Lemma_Multiply_Contains;

end Prunt.Motion_Planner.Stereographic_Kernel.Intervals;
