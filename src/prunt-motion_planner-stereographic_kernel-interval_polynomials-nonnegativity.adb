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

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package body Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials.Nonnegativity
  with SPARK_Mode => On
is
   use all type Prunt.Motion_Planner.Reals.Real;

   --  A nonnegative lower bound on every control certifies p(U) ≥ 0 by the Bernstein convex-hull property. If the
   --  full hull straddles zero, bisect with de Casteljau and retry each half.
   --
   --  Bisection only averages neighbouring controls, and an average increases with each of its two terms. The lower
   --  bounds of the controls of a half are therefore the averages of lower bounds, and likewise for the upper
   --  bounds, so the two sets are carried as plain arrays rather than as intervals. One rounded sum is correct to
   --  half a unit in its last place and halving it is exact, or errs by less than Model_Small for a subnormal;
   --  Inflate_Finite moves each result outward by more than either.

   type Bound_Array is array (Degree_Range) of Dimensionless;

   type Split_Bounds is record
      Left_Lower, Left_Upper, Right_Lower, Right_Upper : Bound_Array;
   end record;

   Magnitude_Limit : constant Dimensionless := Dimensionless'Last / 8.0;
   --  Averages never exceed the larger of their terms, and the outward padding compounds by less than a factor of
   --  two over every level, so no value below this limit can leave Inflate_Finite's range.

   ---------------------------------------------------------------------------
   --  Constants of the proof
   ---------------------------------------------------------------------------

   --  Everything from here to Midpoint_Split, and everything between the subprograms after it, is ghost code. The
   --  proof has two parts.
   --
   --  No operation overflows, because magnitudes grow slowly: the bounds start within Magnitude_Limit, one row of
   --  de Casteljau's triangle adds less than Step to the largest magnitude, one bisection of at most sixteen rows
   --  less than Split_Growth, and after eight bisections everything is still within Top, where two values can be
   --  added and their mean padded.
   --
   --  A True result is sound, because each lower bound which a bisection finds is below the matching control of
   --  the exact half of the polynomial whose controls are the lower bounds. Exact bisection of that polynomial
   --  then reaches pieces without a negative control wherever Decide does, so it is not negative on the unit
   --  interval, and neither is any polynomial with controls no lower.
   --
   --  The ghost expression functions carry No_Inline. The compiler takes an expression function for inlinable even
   --  when ghost code is disabled, and keeps whatever such a function could reference visible to other units; that
   --  stopped it from merging Midpoint_Split into Decide. With the aspect the generated code is that of the body
   --  without its ghost code.

   Half : constant := 0.5;

   Pad_Scale : constant := 2.0**(-51);
   --  Two model epsilons, as a real number.

   Smallest_Pad : constant := 2.0**(-1022);
   --  Model_Small, as a real number.

   Growth_Factor : constant := 1.0 + 2.0**(-50);
   Growth_Offset : constant := 2.0**(-1021);
   --  Inflate_Finite takes a magnitude M to at most Growth_Factor * M + Growth_Offset.

   Middle_Factor : constant := 1.0 + 2.0**(-51);
   Middle_Offset : constant := 2.0**(-1073);
   --  The rounded average of two values within M is within Middle_Factor * M + Middle_Offset.

   Base : constant := 2.0**1021;
   --  No smaller than Magnitude_Limit.

   Step : constant := 2.0**1013;
   --  Far more than one level of de Casteljau's triangle can add to the largest magnitude.

   Split_Growth : constant := 2.0**1017;
   --  Sixteen steps: far more than one bisection can add to the largest magnitude.

   Split_Input_Limit : constant := 2.0**1021 + 7.0 * 2.0**1017;
   --  Base and seven bisections' growth: the largest magnitude which the last bisection can be given.

   Top : constant := 2.0**1021 + 8.0 * 2.0**1017;
   --  Base and eight bisections' growth, which is still below Sum_Operand_Limit.

   Wide_Limit : constant := 2.0**1022;
   --  Above Top with room for one average, and below Half_Last.

   ---------------------------------------------------------------------------
   --  Floating-point facts
   ---------------------------------------------------------------------------

   procedure Lemma_Sum_Operand (Value : Dimensionless)
   with Ghost, Always_Terminates, Pre => abs (R (Value)) <= Top, Post => abs Value <= Sum_Operand_Limit;
   --  The three lemmas which follow carry a bound between its real and its floating-point statement.

   procedure Lemma_Sum_Operand (Value : Dimensionless) is null;

   procedure Lemma_Half_Last (Value : Dimensionless)
   with Ghost, Always_Terminates, Pre => abs (R (Value)) <= Wide_Limit, Post => abs Value <= Half_Last;

   procedure Lemma_Half_Last (Value : Dimensionless) is null;

   procedure Lemma_Magnitude_Limit (Value : Dimensionless)
   with Ghost, Always_Terminates, Pre => abs Value <= Magnitude_Limit, Post => abs (R (Value)) <= Base;

   procedure Lemma_Magnitude_Limit (Value : Dimensionless) is null;

   procedure Lemma_Halving (Value : Dimensionless)
   with
     Ghost,
     Always_Terminates,
     Post => abs (R (0.5 * Value) - Half * R (Value)) <= Epsilon * Half * abs (R (Value)) + Eta;
   --  Halving is one rounded multiplication (and exact unless the result is subnormal).

   procedure Lemma_Halving (Value : Dimensionless) is null;

   procedure Lemma_Scale (Value : Dimensionless)
   with
     Ghost,
     Always_Terminates,
     Pre  => Value >= 0.0 and then Value <= Half_Last,
     Post =>
       abs (R (2.0 * Dimensionless'Model_Epsilon * Value) - Pad_Scale * R (Value))
       <= Epsilon * Pad_Scale * R (Value) + Eta;
   --  Scaling by two model epsilons is one rounded multiplication.

   procedure Lemma_Scale (Value : Dimensionless) is null;

   procedure Lemma_Padding_Upper (Value : Dimensionless)
   with
     Ghost,
     Always_Terminates,
     Pre  => abs Value <= Half_Last,
     Post =>
       Roundoff_Padding (Value) >= 0.0
       and then Roundoff_Padding (Value) <= Half_Last
       and then R (Roundoff_Padding (Value)) >= 0.0
       and then
         R (Roundoff_Padding (Value))
         <= Pad_Scale * abs (R (Value)) + Epsilon * Pad_Scale * abs (R (Value)) + Eta + Smallest_Pad;
   --  The padding is at most a rounded two model epsilons of Value, or Model_Small.

   procedure Lemma_Padding_Upper (Value : Dimensionless) is
   begin
      Lemma_Scale (abs Value);
   end Lemma_Padding_Upper;

   procedure Lemma_Inflate_Growth (Value : Dimensionless; Upward : Boolean)
   with
     Ghost,
     Always_Terminates,
     Pre  => abs Value <= Half_Last,
     Post => abs (R (Inflate_Finite (Value, Upward))) <= Growth_Factor * abs (R (Value)) + Growth_Offset;
   --  The padding adds little more than two model epsilons of Value to its magnitude.

   procedure Lemma_Inflate_Growth (Value : Dimensionless; Upward : Boolean) is
      Pad : constant Dimensionless := Roundoff_Padding (Value);
   begin
      Lemma_Padding_Upper (Value);
      Float_Lemmas.Lemma_Rounding_Error_Add (Value, Pad);
      Float_Lemmas.Lemma_Rounding_Error_Sub (Value, Pad);
      pragma Assert (Inflate_Finite (Value, Upward) = (if Upward then Value + Pad else Value - Pad));
      pragma Assert (abs (R (Value) + R (Pad)) <= abs (R (Value)) + R (Pad));
      pragma Assert (abs (R (Value) - R (Pad)) <= abs (R (Value)) + R (Pad));
      pragma Assert (abs (R (Value + Pad)) <= abs (R (Value)) + R (Pad) + Epsilon * (abs (R (Value)) + R (Pad)) + Eta);
      pragma Assert (abs (R (Value - Pad)) <= abs (R (Value)) + R (Pad) + Epsilon * (abs (R (Value)) + R (Pad)) + Eta);
   end Lemma_Inflate_Growth;

   procedure Lemma_Two_Roundings (Exact, Sum, Middle : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       abs (Sum - Exact) <= Epsilon * abs Exact + Eta
       and then abs (Middle - Half * Sum) <= Epsilon * Half * abs Sum + Eta,
     Post => abs (Middle - Half * Exact) <= Slack_Factor * abs Middle + Twice_Eta;
   --  A rounded sum which is then halved is within the halved result's slack of half the exact sum.

   procedure Lemma_Two_Roundings (Exact, Sum, Middle : Real) is null;

   procedure Lemma_Middle_Magnitude (Exact, Sum, Middle, Limit : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       abs Exact <= 2.0 * Limit
       and then abs (Sum - Exact) <= Epsilon * abs Exact + Eta
       and then abs (Middle - Half * Sum) <= Epsilon * Half * abs Sum + Eta,
     Post => abs Middle <= Middle_Factor * Limit + Middle_Offset;
   --  The rounded mean of two values within Limit exceeds Limit by two rounding errors at most.

   procedure Lemma_Middle_Magnitude (Exact, Sum, Middle, Limit : Real) is null;

   procedure Lemma_Growth_Fits (Limit, Middle, Inflated : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       0.0 <= Limit
       and then Limit <= Top - Step
       and then abs Middle <= Middle_Factor * Limit + Middle_Offset
       and then abs Inflated <= Growth_Factor * abs Middle + Growth_Offset,
     Post => abs Middle <= Wide_Limit and then abs Inflated <= Limit + Step;
   --  Step is far more than rounding and padding add to a mean of values within Limit.

   procedure Lemma_Growth_Fits (Limit, Middle, Inflated : Real) is null;

   procedure Lemma_Average (A, B : Dimensionless; Limit : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  => abs (R (A)) <= Limit and then abs (R (B)) <= Limit and then Limit <= Top - Step,
     Post =>
       abs A <= Sum_Operand_Limit
       and then abs B <= Sum_Operand_Limit
       and then abs (0.5 * (A + B)) <= Half_Last
       and then R (Inflate_Finite (0.5 * (A + B), False)) <= Half * R (A) + Half * R (B)
       and then Half * R (A) + Half * R (B) <= R (Inflate_Finite (0.5 * (A + B), True))
       and then abs (R (Inflate_Finite (0.5 * (A + B), False))) <= Limit + Step
       and then abs (R (Inflate_Finite (0.5 * (A + B), True))) <= Limit + Step;
   --  One entry of de Casteljau's triangle: the average of two values within Limit can be formed and padded, the
   --  two paddings enclose the exact average, and neither exceeds Limit by more than Step.

   procedure Lemma_Average (A, B : Dimensionless; Limit : Real) is
   begin
      Lemma_Sum_Operand (A);
      Lemma_Sum_Operand (B);
      declare
         Sum    : constant Dimensionless := A + B;
         Middle : constant Dimensionless := 0.5 * Sum;
         Exact  : constant Real := R (A) + R (B);
      begin
         Float_Lemmas.Lemma_Rounding_Error_Add (A, B);
         Lemma_Halving (Sum);
         Lemma_Two_Roundings (Exact, R (Sum), R (Middle));
         Lemma_Middle_Magnitude (Exact, R (Sum), R (Middle), Limit);
         pragma Assert (abs (R (Middle)) <= Wide_Limit);
         Lemma_Half_Last (Middle);
         Lemma_Inflate_Margin (Middle);
         Lemma_Inflate_Growth (Middle, False);
         Lemma_Inflate_Growth (Middle, True);
         Lemma_Growth_Fits (Limit, R (Middle), R (Inflate_Finite (Middle, False)));
         Lemma_Growth_Fits (Limit, R (Middle), R (Inflate_Finite (Middle, True)));
         pragma Assert (R (Inflate_Finite (Middle, False)) <= Half * Exact);
         pragma Assert (Half * Exact <= R (Inflate_Finite (Middle, True)));
      end;
   end Lemma_Average;

   ---------------------------------------------------------------------------
   --  Real views of the bounds
   ---------------------------------------------------------------------------

   function To_Reals (Bounds : Bound_Array; Degree : Degree_Range) return Bernstein.Controls
   with
     Ghost,
     Post =>
       To_Reals'Result'First = 0
       and then To_Reals'Result'Last = Degree
       and then (for all K in 0 .. Degree => To_Reals'Result (K) = R (Bounds (K)));
   --  The controls from zero to Degree, as real numbers.

   function To_Reals (Bounds : Bound_Array; Degree : Degree_Range) return Bernstein.Controls is
      Result : Bernstein.Controls (0 .. Degree) := [others => 0.0];
   begin
      for K in 0 .. Degree loop
         Result (K) := R (Bounds (K));
         pragma Loop_Invariant (for all J in 0 .. K => Result (J) = R (Bounds (J)));
      end loop;
      return Result;
   end To_Reals;

   function Within (Bounds : Bound_Array; Degree : Degree_Range; Limit : Real) return Boolean
   is (for all K in 0 .. Degree => abs (R (Bounds (K))) <= Limit)
   with Ghost, No_Inline;
   --  Whether no control up to Degree exceeds Limit in magnitude.

   function Splittable (Lower, Upper : Bound_Array; Degree : Degree_Range) return Boolean
   is (Within (Lower, Degree, Split_Input_Limit) and then Within (Upper, Degree, Split_Input_Limit))
   with Ghost, No_Inline;
   --  Whether the bounds are small enough for every average of one bisection to be formed and padded.

   function Max_Magnitude (Lower, Upper : Bound_Array; Degree : Degree_Range) return Real
   with
     Ghost,
     Post =>
       Within (Lower, Degree, Max_Magnitude'Result)
       and then Within (Upper, Degree, Max_Magnitude'Result)
       and then
         (for some K in 0 .. Degree =>
            Max_Magnitude'Result = abs (R (Lower (K))) or else Max_Magnitude'Result = abs (R (Upper (K))));
   --  The largest magnitude among the bounds up to Degree.

   function Max_Magnitude (Lower, Upper : Bound_Array; Degree : Degree_Range) return Real is
      Result : Real := abs (R (Lower (0)));
   begin
      for K in 0 .. Degree loop
         if abs (R (Lower (K))) > Result then
            Result := abs (R (Lower (K)));
         end if;
         if abs (R (Upper (K))) > Result then
            Result := abs (R (Upper (K)));
         end if;
         pragma
           Loop_Invariant
             (for all J in 0 .. K => abs (R (Lower (J))) <= Result and then abs (R (Upper (J))) <= Result);
         pragma
           Loop_Invariant (for some J in 0 .. K => Result = abs (R (Lower (J))) or else Result = abs (R (Upper (J))));
      end loop;
      return Result;
   end Max_Magnitude;

   procedure Lemma_Max_Is_Least (Lower, Upper : Bound_Array; Degree : Degree_Range; Limit : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (Lower, Degree, Limit) and then Within (Upper, Degree, Limit),
     Post => Max_Magnitude (Lower, Upper, Degree) <= Limit and then Max_Magnitude (Lower, Upper, Degree) >= 0.0;
   --  The largest magnitude is one of the magnitudes, so any limit on them all is a limit on it.

   procedure Lemma_Max_Is_Least (Lower, Upper : Bound_Array; Degree : Degree_Range; Limit : Real) is null;

   function Level_Limit (Start : Real; Level : Degree_Range) return Real
   is (Start + To_Real (Level) * Step)
   with Ghost, No_Inline;
   --  A bound on the magnitudes in row Level of de Casteljau's triangle, when Start bounds those of row zero.

   procedure Lemma_Level_Limit (Start : Real; Level : Degree_Range)
   with
     Ghost,
     Always_Terminates,
     Pre  => Level >= 1 and then 0.0 <= Start and then Start <= Split_Input_Limit,
     Post =>
       Level_Limit (Start, Level) = Level_Limit (Start, Level - 1) + Step
       and then Start <= Level_Limit (Start, Level - 1)
       and then Level_Limit (Start, Level - 1) <= Top - Step
       and then Level_Limit (Start, Level) <= Start + Split_Growth;
   --  The limit grows by Step from one row to the next, and over the sixteen rows at most by Split_Growth.

   procedure Lemma_Level_Limit (Start : Real; Level : Degree_Range) is null;

   procedure Lemma_Slice_Recurrence (P : Bernstein.Controls; First, Last : Natural)
   with
     Ghost,
     Always_Terminates,
     Pre  => P'First <= First and then First < Last and then Last <= P'Last and then P'Last < Natural'Last,
     Post =>
       Bernstein.Value (P (First .. Last), Half)
       = Half * Bernstein.Value (P (First .. Last - 1), Half) + Half * Bernstein.Value (P (First + 1 .. Last), Half);
   --  De Casteljau's recurrence at one half, for a run of the controls of P.

   procedure Lemma_Slice_Recurrence (P : Bernstein.Controls; First, Last : Natural) is null;

   ---------------------------------------------------------------------------
   --  De Casteljau's triangle over the reals
   ---------------------------------------------------------------------------

   type Real_Triangle is array (Degree_Range, Degree_Range) of Real with Ghost;

   function Exact_Triangle (Bounds : Bound_Array; Degree : Degree_Range) return Real_Triangle
   with
     Ghost,
     Post =>
       (for all J in 0 .. Degree => Exact_Triangle'Result (0, J) = R (Bounds (J)))
       and then
         (for all L in 1 .. Degree =>
            (for all J in 0 .. Degree - L =>
               Exact_Triangle'Result (L, J)
               = Half * Exact_Triangle'Result (L - 1, J) + Half * Exact_Triangle'Result (L - 1, J + 1)));
   --  De Casteljau's triangle at one half, in exact arithmetic, of the controls from zero to Degree: row zero holds
   --  the controls, and each entry of a later row is the mean of the two entries above it.

   function Exact_Triangle (Bounds : Bound_Array; Degree : Degree_Range) return Real_Triangle is
      Result : Real_Triangle := [others => [others => 0.0]];
   begin
      for J in 0 .. Degree loop
         Result (0, J) := R (Bounds (J));
         pragma Loop_Invariant (for all I in 0 .. J => Result (0, I) = R (Bounds (I)));
      end loop;
      for L in 1 .. Degree loop
         for J in 0 .. Degree - L loop
            Result (L, J) := Half * Result (L - 1, J) + Half * Result (L - 1, J + 1);
            pragma
              Loop_Invariant
                (for all I in 0 .. J => Result (L, I) = Half * Result (L - 1, I) + Half * Result (L - 1, I + 1));
            pragma
              Loop_Invariant
                (for all M in 0 .. L - 1 => (for all I in Degree_Range => Result (M, I) = Result'Loop_Entry (M, I)));
         end loop;
         pragma Loop_Invariant (for all I in 0 .. Degree => Result (0, I) = R (Bounds (I)));
         pragma
           Loop_Invariant
             (for all M in 1 .. L =>
                (for all I in 0 .. Degree - M =>
                   Result (M, I) = Half * Result (M - 1, I) + Half * Result (M - 1, I + 1)));
      end loop;
      return Result;
   end Exact_Triangle;

   procedure Lemma_Triangle_Value (Bounds : Bound_Array; Degree : Degree_Range; L, J : Degree_Range)
   with
     Ghost,
     Always_Terminates,
     Subprogram_Variant => (Decreases => L),
     Pre                => L <= Degree and then J <= Degree - L,
     Post               =>
       Exact_Triangle (Bounds, Degree) (L, J) = Bernstein.Value (To_Reals (Bounds, Degree) (J .. J + L), Half);
   --  The entry J of row L is the value at one half of the polynomial with the L + 1 controls from J on.

   procedure Lemma_Triangle_Value (Bounds : Bound_Array; Degree : Degree_Range; L, J : Degree_Range) is
      P : constant Bernstein.Controls := To_Reals (Bounds, Degree);
   begin
      if L = 0 then
         pragma Assert (Bernstein.Value (P (J .. J), Half) = P (J));
      else
         Lemma_Triangle_Value (Bounds, Degree, L - 1, J);
         Lemma_Triangle_Value (Bounds, Degree, L - 1, J + 1);
         Lemma_Slice_Recurrence (P, J, J + L);
      end if;
   end Lemma_Triangle_Value;

   ---------------------------------------------------------------------------
   --  Bisection
   ---------------------------------------------------------------------------

   procedure Lemma_Triangle_Step (Bounds : Bound_Array; Degree : Degree_Range; L, J : Degree_Range)
   with
     Ghost,
     Always_Terminates,
     Pre  => L >= 1 and then L <= Degree and then J <= Degree - L,
     Post =>
       Exact_Triangle (Bounds, Degree) (L, J)
       = Half * Exact_Triangle (Bounds, Degree) (L - 1, J) + Half * Exact_Triangle (Bounds, Degree) (L - 1, J + 1);
   --  One entry of the exact triangle, from the function's postcondition.

   procedure Lemma_Triangle_Step (Bounds : Bound_Array; Degree : Degree_Range; L, J : Degree_Range) is null;

   --  The loops of Midpoint_Split keep the facts below about their arrays. Each is a named predicate, and each
   --  step of a loop is a lemma over the array before and after the step, so that the provers meet the
   --  floating-point operations and the quantified facts in separate, small contexts. The predicates are functions
   --  with postconditions rather than expression functions, so that a loop invariant is one opaque fact which a
   --  lemma's postcondition supplies as it stands.

   function Sided
     (Work : Bound_Array; Exact : Real_Triangle; Upward : Boolean; Level : Degree_Range; First, Last : Integer)
      return Boolean
   with
     Ghost,
     Pre  => First >= 0 and then Last <= Maximum_Degree,
     Post =>
       Sided'Result
       = (for all J in First .. Last =>
            (if Upward then Exact (Level, J) <= R (Work (J)) else R (Work (J)) <= Exact (Level, J)));
   --  Whether the elements from First to Last are on the outward side of row Level of the exact triangle.

   function Sided
     (Work : Bound_Array; Exact : Real_Triangle; Upward : Boolean; Level : Degree_Range; First, Last : Integer)
      return Boolean is
   begin
      return
        (for all J in First .. Last =>
           (if Upward then Exact (Level, J) <= R (Work (J)) else R (Work (J)) <= Exact (Level, J)));
   end Sided;

   function Bounded (Work : Bound_Array; First, Last : Integer; Limit : Real) return Boolean
   with
     Ghost,
     Pre  => First >= 0 and then Last <= Maximum_Degree,
     Post => Bounded'Result = (for all J in First .. Last => abs (R (Work (J))) <= Limit);
   --  Whether the elements from First to Last are within Limit in magnitude.

   function Bounded (Work : Bound_Array; First, Last : Integer; Limit : Real) return Boolean is
   begin
      return (for all J in First .. Last => abs (R (Work (J))) <= Limit);
   end Bounded;

   function Left_Sided
     (Bounds : Bound_Array; Exact : Real_Triangle; Upward : Boolean; Last : Integer; Limit : Real) return Boolean
   with
     Ghost,
     Pre  => Last <= Maximum_Degree,
     Post =>
       Left_Sided'Result
       = (for all K in 0 .. Last =>
            (if Upward then Exact (K, 0) <= R (Bounds (K)) else R (Bounds (K)) <= Exact (K, 0))
            and then abs (R (Bounds (K))) <= Limit);
   --  Whether the elements up to Last are within Limit, and on the outward side of the first entries of the rows
   --  of the exact triangle, which are the controls of the left half.

   function Left_Sided
     (Bounds : Bound_Array; Exact : Real_Triangle; Upward : Boolean; Last : Integer; Limit : Real) return Boolean is
   begin
      return
        (for all K in 0 .. Last =>
           (if Upward then Exact (K, 0) <= R (Bounds (K)) else R (Bounds (K)) <= Exact (K, 0))
           and then abs (R (Bounds (K))) <= Limit);
   end Left_Sided;

   function Right_Sided
     (Bounds : Bound_Array;
      Exact  : Real_Triangle;
      Upward : Boolean;
      Degree : Degree_Range;
      First  : Integer;
      Limit  : Real) return Boolean
   with
     Ghost,
     Pre  => First >= 0,
     Post =>
       Right_Sided'Result
       = (for all K in First .. Degree =>
            (if Upward then Exact (Degree - K, K) <= R (Bounds (K)) else R (Bounds (K)) <= Exact (Degree - K, K))
            and then abs (R (Bounds (K))) <= Limit);
   --  Likewise for the elements from First to Degree and the last entries of the rows, which are the controls of
   --  the right half.

   function Right_Sided
     (Bounds : Bound_Array;
      Exact  : Real_Triangle;
      Upward : Boolean;
      Degree : Degree_Range;
      First  : Integer;
      Limit  : Real) return Boolean is
   begin
      return
        (for all K in First .. Degree =>
           (if Upward then Exact (Degree - K, K) <= R (Bounds (K)) else R (Bounds (K)) <= Exact (Degree - K, K))
           and then abs (R (Bounds (K))) <= Limit);
   end Right_Sided;

   procedure Lemma_Entry
     (Work   : Bound_Array;
      Exact  : Real_Triangle;
      Upward : Boolean;
      Degree : Degree_Range;
      Level  : Degree_Range;
      Index  : Degree_Range;
      Limit  : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Level >= 1
       and then Level <= Degree
       and then Index <= Degree - Level
       and then Sided (Work, Exact, Upward, Level - 1, Index, Degree - Level + 1)
       and then Bounded (Work, Index, Degree, Limit)
       and then Limit <= Top - Step
       and then Exact (Level, Index) = Half * Exact (Level - 1, Index) + Half * Exact (Level - 1, Index + 1),
     Post =>
       abs Work (Index) <= Sum_Operand_Limit
       and then abs Work (Index + 1) <= Sum_Operand_Limit
       and then abs (0.5 * (Work (Index) + Work (Index + 1))) <= Half_Last
       and then
         (if Upward
          then Exact (Level, Index) <= R (Inflate_Finite (0.5 * (Work (Index) + Work (Index + 1)), True))
          else R (Inflate_Finite (0.5 * (Work (Index) + Work (Index + 1)), False)) <= Exact (Level, Index))
       and then abs (R (Inflate_Finite (0.5 * (Work (Index) + Work (Index + 1)), Upward))) <= Limit + Step;
   --  One new entry of row Level can be formed from elements Index and Index + 1, which are still of the row
   --  before; it is on the outward side of the exact entry, which is the mean of the two exact entries above it,
   --  and exceeds Limit by no more than Step.

   procedure Lemma_Entry
     (Work   : Bound_Array;
      Exact  : Real_Triangle;
      Upward : Boolean;
      Degree : Degree_Range;
      Level  : Degree_Range;
      Index  : Degree_Range;
      Limit  : Real) is
   begin
      pragma Assert (Index + 1 <= Degree - Level + 1 and then Index + 1 <= Degree);
      pragma Assert (abs (R (Work (Index))) <= Limit and then abs (R (Work (Index + 1))) <= Limit);
      pragma
        Assert
          (if Upward
           then
             Exact (Level - 1, Index) <= R (Work (Index)) and then Exact (Level - 1, Index + 1) <= R (Work (Index + 1))
           else
             R (Work (Index)) <= Exact (Level - 1, Index)
             and then R (Work (Index + 1)) <= Exact (Level - 1, Index + 1));
      Lemma_Average (Work (Index), Work (Index + 1), Limit);
   end Lemma_Entry;

   procedure Lemma_Row_Step
     (Before, After : Bound_Array;
      Exact         : Real_Triangle;
      Upward        : Boolean;
      Degree        : Degree_Range;
      Level         : Degree_Range;
      Index         : Degree_Range;
      Limit         : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Level >= 1
       and then Level <= Degree
       and then Index <= Degree - Level
       and then Sided (Before, Exact, Upward, Level, 0, Index - 1)
       and then Sided (Before, Exact, Upward, Level - 1, Index, Degree - Level + 1)
       and then Bounded (Before, 0, Index - 1, Limit + Step)
       and then Bounded (Before, Index, Degree, Limit)
       and then (for all J in Degree_Range => (if J /= Index then R (After (J)) = R (Before (J))))
       and then
         (if Upward then Exact (Level, Index) <= R (After (Index)) else R (After (Index)) <= Exact (Level, Index))
       and then abs (R (After (Index))) <= Limit + Step,
     Post =>
       Sided (After, Exact, Upward, Level, 0, Index)
       and then Sided (After, Exact, Upward, Level - 1, Index + 1, Degree - Level + 1)
       and then Bounded (After, 0, Index, Limit + Step)
       and then Bounded (After, Index + 1, Degree, Limit);
   --  Replacing element Index by an entry of row Level extends the part of the array which holds that row.

   procedure Lemma_Row_Step
     (Before, After : Bound_Array;
      Exact         : Real_Triangle;
      Upward        : Boolean;
      Degree        : Degree_Range;
      Level         : Degree_Range;
      Index         : Degree_Range;
      Limit         : Real)
   is null;

   procedure Lemma_Row_Done (Work : Bound_Array; Degree : Degree_Range; Last : Degree_Range; Low, High : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Last <= Degree
       and then Bounded (Work, 0, Last, High)
       and then Bounded (Work, Last + 1, Degree, Low)
       and then Low <= High,
     Post => Bounded (Work, 0, Degree, High);
   --  After a row is complete, the larger limit holds for the whole array.

   procedure Lemma_Row_Done (Work : Bound_Array; Degree : Degree_Range; Last : Degree_Range; Low, High : Real) is null;

   procedure Lemma_Left_Step
     (Before, After : Bound_Array;
      Work          : Bound_Array;
      Exact         : Real_Triangle;
      Upward        : Boolean;
      Degree        : Degree_Range;
      Level         : Degree_Range;
      Row_Limit     : Real;
      Limit         : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Level >= 1
       and then Level <= Degree
       and then Left_Sided (Before, Exact, Upward, Level - 1, Limit)
       and then Sided (Work, Exact, Upward, Level, 0, Degree - Level)
       and then Bounded (Work, 0, Degree, Row_Limit)
       and then Row_Limit <= Limit
       and then (for all K in Degree_Range => (if K /= Level then R (After (K)) = R (Before (K))))
       and then R (After (Level)) = R (Work (0)),
     Post => Left_Sided (After, Exact, Upward, Level, Limit);
   --  The first entry of row Level is the next control of the left half.

   procedure Lemma_Left_Step
     (Before, After : Bound_Array;
      Work          : Bound_Array;
      Exact         : Real_Triangle;
      Upward        : Boolean;
      Degree        : Degree_Range;
      Level         : Degree_Range;
      Row_Limit     : Real;
      Limit         : Real)
   is null;

   procedure Lemma_Right_Step
     (Before, After : Bound_Array;
      Work          : Bound_Array;
      Exact         : Real_Triangle;
      Upward        : Boolean;
      Degree        : Degree_Range;
      Level         : Degree_Range;
      Row_Limit     : Real;
      Limit         : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Level >= 1
       and then Level <= Degree
       and then Right_Sided (Before, Exact, Upward, Degree, Degree - Level + 1, Limit)
       and then Sided (Work, Exact, Upward, Level, 0, Degree - Level)
       and then Bounded (Work, 0, Degree, Row_Limit)
       and then Row_Limit <= Limit
       and then (for all K in Degree_Range => (if K /= Degree - Level then R (After (K)) = R (Before (K))))
       and then R (After (Degree - Level)) = R (Work (Degree - Level)),
     Post => Right_Sided (After, Exact, Upward, Degree, Degree - Level, Limit);
   --  The last entry of row Level is the next control, counting downward, of the right half.

   procedure Lemma_Right_Step
     (Before, After : Bound_Array;
      Work          : Bound_Array;
      Exact         : Real_Triangle;
      Upward        : Boolean;
      Degree        : Degree_Range;
      Level         : Degree_Range;
      Row_Limit     : Real;
      Limit         : Real)
   is null;

   procedure Lemma_Split_Start
     (Bounds : Bound_Array; Other : Bound_Array; Upward : Boolean; Degree : Degree_Range; Limit, Final_Limit : Real)
   with
     Ghost,
     Always_Terminates,
     Pre  => Within (Bounds, Degree, Limit) and then Limit <= Final_Limit,
     Post =>
       Sided (Bounds, Exact_Triangle (Bounds, Degree), Upward, 0, 0, Degree)
       and then Bounded (Bounds, 0, Degree, Level_Limit (Limit, 0))
       and then
         (if R (Other (0)) = R (Bounds (0))
          then Left_Sided (Other, Exact_Triangle (Bounds, Degree), Upward, 0, Final_Limit))
       and then
         (if R (Other (Degree)) = R (Bounds (Degree))
          then Right_Sided (Other, Exact_Triangle (Bounds, Degree), Upward, Degree, Degree, Final_Limit));
   --  Before any row is formed the array holds row zero of its own exact triangle, and the first and the last
   --  control of the bounds are those of the left and of the right half.

   procedure Lemma_Split_Start
     (Bounds : Bound_Array; Other : Bound_Array; Upward : Boolean; Degree : Degree_Range; Limit, Final_Limit : Real)
   is null;

   function Midpoint_Split (Lower, Upper : Bound_Array; Degree : Degree_Range) return Split_Bounds
   with
     Pre  => Splittable (Lower, Upper, Degree),
     Post =>
       Left_Sided
         (Midpoint_Split'Result.Left_Lower,
          Exact_Triangle (Lower, Degree),
          False,
          Degree,
          Max_Magnitude (Lower, Upper, Degree) + Split_Growth)
       and then
         Left_Sided
           (Midpoint_Split'Result.Left_Upper,
            Exact_Triangle (Upper, Degree),
            True,
            Degree,
            Max_Magnitude (Lower, Upper, Degree) + Split_Growth)
       and then
         Right_Sided
           (Midpoint_Split'Result.Right_Lower,
            Exact_Triangle (Lower, Degree),
            False,
            Degree,
            0,
            Max_Magnitude (Lower, Upper, Degree) + Split_Growth)
       and then
         Right_Sided
           (Midpoint_Split'Result.Right_Upper,
            Exact_Triangle (Upper, Degree),
            True,
            Degree,
            0,
            Max_Magnitude (Lower, Upper, Degree) + Split_Growth);
   --  The bounds of the controls of each half, by de Casteljau's triangle with every average rounded outward.
   --  The controls of the left half of a polynomial are the first entries of the rows of its exact triangle, and
   --  those of the right half the last entries; the bounds found are below those of the triangle of the lower
   --  bounds and above those of the triangle of the upper bounds. No magnitude grows by more than Split_Growth.

   function Midpoint_Split (Lower, Upper : Bound_Array; Degree : Degree_Range) return Split_Bounds is
      pragma Suppress (Overflow_Check);
      Work_Lower : Bound_Array := Lower;
      Work_Upper : Bound_Array := Upper;
      Result     : Split_Bounds := (Left_Lower | Left_Upper | Right_Lower | Right_Upper => [others => 0.0]);

      Lower_Exact : constant Real_Triangle := Exact_Triangle (Lower, Degree)
      with Ghost;
      Upper_Exact : constant Real_Triangle := Exact_Triangle (Upper, Degree)
      with Ghost;
      Entry_Limit : constant Real := Max_Magnitude (Lower, Upper, Degree)
      with Ghost;
      Final_Limit : constant Real := Entry_Limit + Split_Growth
      with Ghost;

      Before_Lower  : Bound_Array
      with Ghost;
      Before_Upper  : Bound_Array
      with Ghost;
      Before_Result : Split_Bounds
      with Ghost;
      --  The arrays as they were before the latest step.
   begin
      Lemma_Max_Is_Least (Lower, Upper, Degree, Split_Input_Limit);
      Result.Left_Lower (0) := Work_Lower (0);
      Result.Left_Upper (0) := Work_Upper (0);
      Result.Right_Lower (Degree) := Work_Lower (Degree);
      Result.Right_Upper (Degree) := Work_Upper (Degree);
      Lemma_Split_Start (Lower, Result.Left_Lower, False, Degree, Entry_Limit, Final_Limit);
      Lemma_Split_Start (Lower, Result.Right_Lower, False, Degree, Entry_Limit, Final_Limit);
      Lemma_Split_Start (Upper, Result.Left_Upper, True, Degree, Entry_Limit, Final_Limit);
      Lemma_Split_Start (Upper, Result.Right_Upper, True, Degree, Entry_Limit, Final_Limit);
      for Level in 1 .. Degree loop
         --  On entry the arrays hold row Level - 1 of the triangle, and beyond its end what is left of earlier rows.
         pragma Loop_Invariant (Sided (Work_Lower, Lower_Exact, False, Level - 1, 0, Degree - Level + 1));
         pragma Loop_Invariant (Sided (Work_Upper, Upper_Exact, True, Level - 1, 0, Degree - Level + 1));
         pragma Loop_Invariant (Bounded (Work_Lower, 0, Degree, Level_Limit (Entry_Limit, Level - 1)));
         pragma Loop_Invariant (Bounded (Work_Upper, 0, Degree, Level_Limit (Entry_Limit, Level - 1)));
         pragma Loop_Invariant (Left_Sided (Result.Left_Lower, Lower_Exact, False, Level - 1, Final_Limit));
         pragma Loop_Invariant (Left_Sided (Result.Left_Upper, Upper_Exact, True, Level - 1, Final_Limit));
         pragma
           Loop_Invariant
             (Right_Sided (Result.Right_Lower, Lower_Exact, False, Degree, Degree - Level + 1, Final_Limit));
         pragma
           Loop_Invariant
             (Right_Sided (Result.Right_Upper, Upper_Exact, True, Degree, Degree - Level + 1, Final_Limit));
         Lemma_Level_Limit (Entry_Limit, Level);
         for Index in 0 .. Degree - Level loop
            --  The elements before Index are of row Level, and those from Index on are as the row began.
            pragma Loop_Invariant (Sided (Work_Lower, Lower_Exact, False, Level, 0, Index - 1));
            pragma Loop_Invariant (Sided (Work_Lower, Lower_Exact, False, Level - 1, Index, Degree - Level + 1));
            pragma Loop_Invariant (Bounded (Work_Lower, 0, Index - 1, Level_Limit (Entry_Limit, Level - 1) + Step));
            pragma Loop_Invariant (Bounded (Work_Lower, Index, Degree, Level_Limit (Entry_Limit, Level - 1)));
            pragma Loop_Invariant (Sided (Work_Upper, Upper_Exact, True, Level, 0, Index - 1));
            pragma Loop_Invariant (Sided (Work_Upper, Upper_Exact, True, Level - 1, Index, Degree - Level + 1));
            pragma Loop_Invariant (Bounded (Work_Upper, 0, Index - 1, Level_Limit (Entry_Limit, Level - 1) + Step));
            pragma Loop_Invariant (Bounded (Work_Upper, Index, Degree, Level_Limit (Entry_Limit, Level - 1)));
            Before_Lower := Work_Lower;
            Before_Upper := Work_Upper;
            Lemma_Triangle_Step (Lower, Degree, Level, Index);
            Lemma_Triangle_Step (Upper, Degree, Level, Index);
            Lemma_Entry (Work_Lower, Lower_Exact, False, Degree, Level, Index, Level_Limit (Entry_Limit, Level - 1));
            Lemma_Entry (Work_Upper, Upper_Exact, True, Degree, Level, Index, Level_Limit (Entry_Limit, Level - 1));
            Work_Lower (Index) :=
              Inflate_Finite (0.5 * (Work_Lower (Index) + Work_Lower (Index + 1)), Upward => False);
            Work_Upper (Index) := Inflate_Finite (0.5 * (Work_Upper (Index) + Work_Upper (Index + 1)), Upward => True);
            Lemma_Row_Step
              (Before_Lower,
               Work_Lower,
               Lower_Exact,
               False,
               Degree,
               Level,
               Index,
               Level_Limit (Entry_Limit, Level - 1));
            Lemma_Row_Step
              (Before_Upper,
               Work_Upper,
               Upper_Exact,
               True,
               Degree,
               Level,
               Index,
               Level_Limit (Entry_Limit, Level - 1));
         end loop;
         Lemma_Row_Done
           (Work_Lower,
            Degree,
            Degree - Level,
            Level_Limit (Entry_Limit, Level - 1),
            Level_Limit (Entry_Limit, Level - 1) + Step);
         Lemma_Row_Done
           (Work_Upper,
            Degree,
            Degree - Level,
            Level_Limit (Entry_Limit, Level - 1),
            Level_Limit (Entry_Limit, Level - 1) + Step);
         Before_Result := Result;
         Result.Left_Lower (Level) := Work_Lower (0);
         Result.Left_Upper (Level) := Work_Upper (0);
         Result.Right_Lower (Degree - Level) := Work_Lower (Degree - Level);
         Result.Right_Upper (Degree - Level) := Work_Upper (Degree - Level);
         Lemma_Left_Step
           (Before_Result.Left_Lower,
            Result.Left_Lower,
            Work_Lower,
            Lower_Exact,
            False,
            Degree,
            Level,
            Level_Limit (Entry_Limit, Level - 1) + Step,
            Final_Limit);
         Lemma_Left_Step
           (Before_Result.Left_Upper,
            Result.Left_Upper,
            Work_Upper,
            Upper_Exact,
            True,
            Degree,
            Level,
            Level_Limit (Entry_Limit, Level - 1) + Step,
            Final_Limit);
         Lemma_Right_Step
           (Before_Result.Right_Lower,
            Result.Right_Lower,
            Work_Lower,
            Lower_Exact,
            False,
            Degree,
            Level,
            Level_Limit (Entry_Limit, Level - 1) + Step,
            Final_Limit);
         Lemma_Right_Step
           (Before_Result.Right_Upper,
            Result.Right_Upper,
            Work_Upper,
            Upper_Exact,
            True,
            Degree,
            Level,
            Level_Limit (Entry_Limit, Level - 1) + Step,
            Final_Limit);
      end loop;
      --  Either there was one control and no row to form, or the last row formed was row Degree.
      pragma Assert (if Degree = 0 then Left_Sided (Result.Left_Lower, Lower_Exact, False, Degree, Final_Limit));
      pragma Assert (if Degree > 0 then Left_Sided (Result.Left_Lower, Lower_Exact, False, Degree, Final_Limit));
      pragma Assert (if Degree = 0 then Left_Sided (Result.Left_Upper, Upper_Exact, True, Degree, Final_Limit));
      pragma Assert (if Degree > 0 then Left_Sided (Result.Left_Upper, Upper_Exact, True, Degree, Final_Limit));
      pragma Assert (if Degree = 0 then Right_Sided (Result.Right_Lower, Lower_Exact, False, Degree, 0, Final_Limit));
      pragma Assert (if Degree > 0 then Right_Sided (Result.Right_Lower, Lower_Exact, False, Degree, 0, Final_Limit));
      pragma Assert (if Degree = 0 then Right_Sided (Result.Right_Upper, Upper_Exact, True, Degree, 0, Final_Limit));
      pragma Assert (if Degree > 0 then Right_Sided (Result.Right_Upper, Upper_Exact, True, Degree, 0, Final_Limit));
      return Result;
   end Midpoint_Split;

   ---------------------------------------------------------------------------
   --  The decision
   ---------------------------------------------------------------------------

   function No_Negative_Bound (Lower : Bound_Array; Degree : Degree_Range) return Boolean
   is (for all K in 0 .. Degree => Lower (K) >= 0.0)
   with Ghost, No_Inline;
   --  Whether the convex-hull test passes.

   function Depth_Bound (Remaining : Bisection_Depth) return Real
   is (Base + To_Real (Nonnegativity_Depth - Remaining) * Split_Growth)
   with Ghost, No_Inline;
   --  A bound on every magnitude which the decision can meet with Remaining bisections left.

   procedure Lemma_Depth_Bound (Remaining : Bisection_Depth)
   with
     Ghost,
     Always_Terminates,
     Post =>
       Depth_Bound (Remaining) >= Base
       and then Depth_Bound (Remaining) <= Top
       and then (if Remaining = Nonnegativity_Depth then Depth_Bound (Remaining) = Base)
       and then
         (if Remaining >= 1
          then
            Depth_Bound (Remaining) <= Split_Input_Limit
            and then Depth_Bound (Remaining - 1) = Depth_Bound (Remaining) + Split_Growth);
   --  The bound starts at Base, grows by Split_Growth with each bisection, and never exceeds Top; while a
   --  bisection is left it is small enough for Midpoint_Split.

   procedure Lemma_Depth_Bound (Remaining : Bisection_Depth) is null;

   function Certified (Lower, Upper : Bound_Array; Degree : Degree_Range; Remaining : Bisection_Depth) return Boolean
   is (No_Negative_Bound (Lower, Degree)
       or else
         (Remaining > 0
          and then Splittable (Lower, Upper, Degree)
          and then
            Certified
              (Midpoint_Split (Lower, Upper, Degree).Left_Lower,
               Midpoint_Split (Lower, Upper, Degree).Left_Upper,
               Degree,
               Remaining - 1)
          and then
            Certified
              (Midpoint_Split (Lower, Upper, Degree).Right_Lower,
               Midpoint_Split (Lower, Upper, Degree).Right_Upper,
               Degree,
               Remaining - 1)))
   with Ghost, No_Inline, Subprogram_Variant => (Decreases => Remaining);
   --  What a True result of Decide rests on: no lower bound is negative, or else both halves are certified in turn.

   function Decide (Lower, Upper : Bound_Array; Degree : Degree_Range; Remaining : Bisection_Depth) return Boolean
   with
     Subprogram_Variant => (Decreases => Remaining),
     Pre                =>
       Within (Lower, Degree, Depth_Bound (Remaining)) and then Within (Upper, Degree, Depth_Bound (Remaining)),
     Post               => (if Decide'Result then Certified (Lower, Upper, Degree, Remaining));
   --  Whether the bounds certify, within Remaining bisections, that no polynomial between them is negative.

   function Decide (Lower, Upper : Bound_Array; Degree : Degree_Range; Remaining : Bisection_Depth) return Boolean is
      All_Nonnegative : Boolean := True;
      All_Negative    : Boolean := True;
   begin
      for Index in 0 .. Degree loop
         All_Nonnegative := All_Nonnegative and then Lower (Index) >= 0.0;
         All_Negative := All_Negative and then Upper (Index) < 0.0;
         pragma Loop_Invariant (All_Nonnegative = (for all K in 0 .. Index => Lower (K) >= 0.0));
      end loop;
      if All_Nonnegative then
         return True;
      elsif Upper (0) < 0.0 or else Upper (Degree) < 0.0 then
         --  The end controls are the values of the polynomial at the ends of this piece, so it is negative there and
         --  no further bisection can certify it.
         return False;
      elsif All_Negative or else Remaining = 0 or else Degree = 0 then
         return False;
      end if;
      Lemma_Depth_Bound (Remaining);
      Lemma_Max_Is_Least (Lower, Upper, Degree, Depth_Bound (Remaining));
      declare
         Halves : constant Split_Bounds := Midpoint_Split (Lower, Upper, Degree);
      begin
         return
           Decide (Halves.Left_Lower, Halves.Left_Upper, Degree, Remaining - 1)
           and then Decide (Halves.Right_Lower, Halves.Right_Upper, Degree, Remaining - 1);
      end;
   end Decide;

   ---------------------------------------------------------------------------
   --  Soundness of the decision
   ---------------------------------------------------------------------------

   --  Only the lower bounds matter to what a True result means. Take them as the controls of a real polynomial:
   --  a certificate for the bounds is one for that polynomial by exact bisection, because each lower bound of a
   --  half is below the matching control of the exact half; such a certificate carries over to every polynomial
   --  with controls no lower; and a polynomial which has one is not negative on the unit interval.

   procedure Lemma_Nonnegative_Hull (P : Bernstein.Controls; T : Real)
   with
     Ghost,
     Always_Terminates,
     Subprogram_Variant => (Decreases => P'Length),
     Pre                =>
       P'Length >= 1
       and then P'Last < Natural'Last
       and then 0.0 <= T
       and then T <= 1.0
       and then (for all I in P'Range => P (I) >= 0.0),
     Post               => Bernstein.Value (P, T) >= 0.0;
   --  On the unit interval a polynomial without a negative control is not negative.

   procedure Lemma_Nonnegative_Hull (P : Bernstein.Controls; T : Real) is
   begin
      if P'Length > 1 then
         Lemma_Nonnegative_Hull (P (P'First .. P'Last - 1), T);
         Lemma_Nonnegative_Hull (P (P'First + 1 .. P'Last), T);
         declare
            Left  : constant Real := Bernstein.Value (P (P'First .. P'Last - 1), T);
            Right : constant Real := Bernstein.Value (P (P'First + 1 .. P'Last), T);
         begin
            pragma Assert ((1.0 - T) * Left >= 0.0 and then T * Right >= 0.0);
         end;
      end if;
   end Lemma_Nonnegative_Hull;

   function No_Negative_Control (P : Bernstein.Controls) return Boolean
   is (for all K in P'Range => P (K) >= 0.0)
   with Ghost, No_Inline;

   function Real_Certified (P : Bernstein.Controls; Remaining : Bisection_Depth) return Boolean
   is (No_Negative_Control (P)
       or else
         (Remaining > 0
          and then Real_Certified (Bernstein.Left_Half (P), Remaining - 1)
          and then Real_Certified (Bernstein.Right_Half (P), Remaining - 1)))
   with
     Ghost,
     No_Inline,
     Pre                => P'Length >= 1 and then P'Last < Natural'Last,
     Subprogram_Variant => (Decreases => Remaining);
   --  Whether exact bisection to at most Remaining levels reaches pieces without a negative control.

   procedure Lemma_Halves_Are_Monotonic (P, Q : Bernstein.Controls; K : Natural)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       P'Length >= 1
       and then P'Last < Natural'Last
       and then Bernstein.Same_Range (P, Q)
       and then (for all I in P'Range => P (I) <= Q (I))
       and then K in P'Range,
     Post =>
       Bernstein.Left_Half (P) (K) <= Bernstein.Left_Half (Q) (K)
       and then Bernstein.Right_Half (P) (K) <= Bernstein.Right_Half (Q) (K);
   --  Raising controls cannot lower a control of either half.

   procedure Lemma_Halves_Are_Monotonic (P, Q : Bernstein.Controls; K : Natural) is
   begin
      Bernstein.Lemma_Value_Is_Monotonic (P (P'First .. K), Q (Q'First .. K), Half);
      Bernstein.Lemma_Value_Is_Monotonic (P (K .. P'Last), Q (K .. Q'Last), Half);
      pragma Assert (Bernstein.Left_Half (P) (K) = Bernstein.Value (P (P'First .. K), Half));
      pragma Assert (Bernstein.Left_Half (Q) (K) = Bernstein.Value (Q (Q'First .. K), Half));
      pragma Assert (Bernstein.Right_Half (P) (K) = Bernstein.Value (P (K .. P'Last), Half));
      pragma Assert (Bernstein.Right_Half (Q) (K) = Bernstein.Value (Q (K .. Q'Last), Half));
   end Lemma_Halves_Are_Monotonic;

   procedure Lemma_Real_Certified_Is_Monotonic (P, Q : Bernstein.Controls; Remaining : Bisection_Depth)
   with
     Ghost,
     Always_Terminates,
     Subprogram_Variant => (Decreases => Remaining),
     Pre                =>
       P'Length >= 1
       and then P'Last < Natural'Last
       and then Bernstein.Same_Range (P, Q)
       and then (for all I in P'Range => P (I) <= Q (I))
       and then Real_Certified (P, Remaining),
     Post               => Real_Certified (Q, Remaining);
   --  A certificate for a polynomial is one for every polynomial with controls no lower.

   procedure Lemma_Real_Certified_Is_Monotonic (P, Q : Bernstein.Controls; Remaining : Bisection_Depth) is
   begin
      if No_Negative_Control (P) then
         pragma Assert (for all K in Q'Range => Q (K) >= 0.0);
      else
         declare
            P_Left  : constant Bernstein.Controls := Bernstein.Left_Half (P);
            Q_Left  : constant Bernstein.Controls := Bernstein.Left_Half (Q);
            P_Right : constant Bernstein.Controls := Bernstein.Right_Half (P);
            Q_Right : constant Bernstein.Controls := Bernstein.Right_Half (Q);
         begin
            for K in P'Range loop
               Lemma_Halves_Are_Monotonic (P, Q, K);
               pragma
                 Loop_Invariant
                   (for all J in P'First .. K => P_Left (J) <= Q_Left (J) and then P_Right (J) <= Q_Right (J));
            end loop;
            Lemma_Real_Certified_Is_Monotonic (P_Left, Q_Left, Remaining - 1);
            Lemma_Real_Certified_Is_Monotonic (P_Right, Q_Right, Remaining - 1);
         end;
      end if;
   end Lemma_Real_Certified_Is_Monotonic;

   procedure Lemma_Real_Certified_Is_Sound (P : Bernstein.Controls; Remaining : Bisection_Depth; T : Real)
   with
     Ghost,
     Always_Terminates,
     Subprogram_Variant => (Decreases => Remaining),
     Pre                =>
       P'Length >= 1
       and then P'Last < Natural'Last
       and then Real_Certified (P, Remaining)
       and then 0.0 <= T
       and then T <= 1.0,
     Post               => Bernstein.Value (P, T) >= 0.0;
   --  A polynomial with a certificate is not negative on the unit interval.

   procedure Lemma_Real_Certified_Is_Sound (P : Bernstein.Controls; Remaining : Bisection_Depth; T : Real) is
   begin
      if No_Negative_Control (P) then
         Lemma_Nonnegative_Hull (P, T);
      elsif T <= Half then
         --  T lies in the left half, where P is its left half at twice T.
         Bernstein.Lemma_Left_Half (P, 2.0 * T);
         pragma Assert (Half * (2.0 * T) = T);
         Lemma_Real_Certified_Is_Sound (Bernstein.Left_Half (P), Remaining - 1, 2.0 * T);
      else
         --  T lies in the right half, where P is its right half at twice T less one.
         Bernstein.Lemma_Right_Half (P, 2.0 * T - 1.0);
         pragma Assert (Half + Half * (2.0 * T - 1.0) = T);
         Lemma_Real_Certified_Is_Sound (Bernstein.Right_Half (P), Remaining - 1, 2.0 * T - 1.0);
      end if;
   end Lemma_Real_Certified_Is_Sound;

   procedure Lemma_Split_Below_Halves (Lower, Upper : Bound_Array; Degree : Degree_Range; K : Degree_Range)
   with
     Ghost,
     Always_Terminates,
     Pre  => Splittable (Lower, Upper, Degree) and then K <= Degree,
     Post =>
       R (Midpoint_Split (Lower, Upper, Degree).Left_Lower (K)) <= Bernstein.Left_Half (To_Reals (Lower, Degree)) (K)
       and then
         R (Midpoint_Split (Lower, Upper, Degree).Right_Lower (K))
         <= Bernstein.Right_Half (To_Reals (Lower, Degree)) (K);
   --  A lower bound which the split finds for a control of a half is below that control of the exact half of the
   --  lower bounds: the control is an entry of the exact triangle, which is the value at one half of a leading or
   --  of a trailing run of the controls.

   procedure Lemma_Split_Below_Halves (Lower, Upper : Bound_Array; Degree : Degree_Range; K : Degree_Range) is
      P : constant Bernstein.Controls := To_Reals (Lower, Degree);
   begin
      pragma
        Assert (R (Midpoint_Split (Lower, Upper, Degree).Left_Lower (K)) <= Exact_Triangle (Lower, Degree) (K, 0));
      pragma
        Assert
          (R (Midpoint_Split (Lower, Upper, Degree).Right_Lower (K))
           <= Exact_Triangle (Lower, Degree) (Degree - K, K));
      Lemma_Triangle_Value (Lower, Degree, K, 0);
      Lemma_Triangle_Value (Lower, Degree, Degree - K, K);
      pragma Assert (Bernstein.Left_Half (P) (K) = Bernstein.Value (P (0 .. K), Half));
      pragma Assert (Bernstein.Right_Half (P) (K) = Bernstein.Value (P (K .. Degree), Half));
   end Lemma_Split_Below_Halves;

   procedure Lemma_Nonnegative_Reals (Lower : Bound_Array; Degree : Degree_Range)
   with
     Ghost,
     Always_Terminates,
     Pre  => No_Negative_Bound (Lower, Degree),
     Post => No_Negative_Control (To_Reals (Lower, Degree));

   procedure Lemma_Nonnegative_Reals (Lower : Bound_Array; Degree : Degree_Range) is
      P : constant Bernstein.Controls := To_Reals (Lower, Degree);
   begin
      pragma Assert (for all K in 0 .. Degree => R (Lower (K)) >= 0.0);
      pragma Assert (for all K in P'Range => P (K) >= 0.0);
   end Lemma_Nonnegative_Reals;

   procedure Lemma_Certified_Is_Real (Lower, Upper : Bound_Array; Degree : Degree_Range; Remaining : Bisection_Depth)
   with
     Ghost,
     Always_Terminates,
     Subprogram_Variant => (Decreases => Remaining),
     Pre                => Certified (Lower, Upper, Degree, Remaining),
     Post               => Real_Certified (To_Reals (Lower, Degree), Remaining);
   --  What Decide certifies, exact bisection of the lower bounds certifies.

   procedure Lemma_Certified_Is_Real (Lower, Upper : Bound_Array; Degree : Degree_Range; Remaining : Bisection_Depth)
   is
      P : constant Bernstein.Controls := To_Reals (Lower, Degree);
   begin
      if No_Negative_Bound (Lower, Degree) then
         Lemma_Nonnegative_Reals (Lower, Degree);
      else
         declare
            Halves : constant Split_Bounds := Midpoint_Split (Lower, Upper, Degree);
            Left   : constant Bernstein.Controls := To_Reals (Halves.Left_Lower, Degree);
            Right  : constant Bernstein.Controls := To_Reals (Halves.Right_Lower, Degree);
         begin
            Lemma_Certified_Is_Real (Halves.Left_Lower, Halves.Left_Upper, Degree, Remaining - 1);
            Lemma_Certified_Is_Real (Halves.Right_Lower, Halves.Right_Upper, Degree, Remaining - 1);
            for K in 0 .. Degree loop
               Lemma_Split_Below_Halves (Lower, Upper, Degree, K);
               pragma
                 Loop_Invariant
                   (for all J in 0 .. K =>
                      Left (J) <= Bernstein.Left_Half (P) (J) and then Right (J) <= Bernstein.Right_Half (P) (J));
            end loop;
            Lemma_Real_Certified_Is_Monotonic (Left, Bernstein.Left_Half (P), Remaining - 1);
            Lemma_Real_Certified_Is_Monotonic (Right, Bernstein.Right_Half (P), Remaining - 1);
         end;
      end if;
   end Lemma_Certified_Is_Real;

   ---------------------------------------------------------------------------
   --  The test
   ---------------------------------------------------------------------------

   function Lower_Reals (Value : Polynomial) return Bernstein.Controls
   with
     Ghost,
     Post =>
       Lower_Reals'Result'First = 0
       and then Lower_Reals'Result'Last = Value.Degree
       and then (for all K in 0 .. Value.Degree => Lower_Reals'Result (K) = R (Value.Controls (K).Lower));
   --  The lower endpoints of the controls of Value, as the controls of a real polynomial.

   function Lower_Reals (Value : Polynomial) return Bernstein.Controls is
      Result : Bernstein.Controls (0 .. Value.Degree) := [others => 0.0];
   begin
      for K in 0 .. Value.Degree loop
         Result (K) := R (Value.Controls (K).Lower);
         pragma Loop_Invariant (for all J in 0 .. K => Result (J) = R (Value.Controls (J).Lower));
      end loop;
      return Result;
   end Lower_Reals;

   procedure Lemma_Decision (Lower, Upper : Bound_Array; Value : Polynomial)
   with
     Ghost,
     Always_Terminates,
     Pre  =>
       Within (Lower, Value.Degree, Base)
       and then Within (Upper, Value.Degree, Base)
       and then (for all K in 0 .. Value.Degree => R (Lower (K)) = R (Value.Controls (K).Lower)),
     Post =>
       Within (Lower, Value.Degree, Depth_Bound (Nonnegativity_Depth))
       and then Within (Upper, Value.Degree, Depth_Bound (Nonnegativity_Depth))
       and then
         (if Decide (Lower, Upper, Value.Degree, Nonnegativity_Depth)
          then Real_Certified (Lower_Reals (Value), Nonnegativity_Depth));
   --  The decision can be taken for bounds within Base, and what it certifies for an array with the values of
   --  the lower endpoints of Value is certified for the real polynomial with those controls.

   procedure Lemma_Decision (Lower, Upper : Bound_Array; Value : Polynomial) is
   begin
      Lemma_Depth_Bound (Nonnegativity_Depth);
      if Decide (Lower, Upper, Value.Degree, Nonnegativity_Depth) then
         Lemma_Certified_Is_Real (Lower, Upper, Value.Degree, Nonnegativity_Depth);
         Lemma_Real_Certified_Is_Monotonic (To_Reals (Lower, Value.Degree), Lower_Reals (Value), Nonnegativity_Depth);
      end if;
   end Lemma_Decision;

   function Is_Nonnegative (Value : Polynomial) return Boolean
   with
     Refined_Post =>
       (if Is_Nonnegative'Result
        then All_Valid (Value) and then Real_Certified (Lower_Reals (Value), Nonnegativity_Depth))
   is
      Lower, Upper : Bound_Array := [others => 0.0];
   begin
      for Index in 0 .. Value.Degree loop
         pragma
           Loop_Invariant
             (for all K in 0 .. Index - 1 =>
                Value.Controls (K).Valid
                and then R (Lower (K)) = R (Value.Controls (K).Lower)
                and then abs (R (Lower (K))) <= Base
                and then abs (R (Upper (K))) <= Base);
         declare
            Coefficient : constant Interval := Value.Controls (Index);
         begin
            --  The comparisons fail for a NaN as well.
            if not Coefficient.Valid
              or else not (abs Coefficient.Lower <= Magnitude_Limit)
              or else not (abs Coefficient.Upper <= Magnitude_Limit)
              or else not (Coefficient.Lower <= Coefficient.Upper)
            then
               return False;
            end if;
            Lower (Index) := Coefficient.Lower;
            Upper (Index) := Coefficient.Upper;
            Lemma_Magnitude_Limit (Coefficient.Lower);
            Lemma_Magnitude_Limit (Coefficient.Upper);
         end;
      end loop;
      Lemma_Decision (Lower, Upper, Value);
      return Decide (Lower, Upper, Value.Degree, Nonnegativity_Depth);
   end Is_Nonnegative;

   procedure Lemma_Is_Nonnegative (Value : Polynomial; Exact : Bernstein.Controls; T : Real) is
   begin
      --  Each control of Exact is at least the lower endpoint of its interval.
      Lemma_Real_Certified_Is_Monotonic (Lower_Reals (Value), Exact, Nonnegativity_Depth);
      Lemma_Real_Certified_Is_Sound (Exact, Nonnegativity_Depth, T);
   end Lemma_Is_Nonnegative;

end Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials.Nonnegativity;
