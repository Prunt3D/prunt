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

with Prunt.Motion_Planner.Stereographic_Kernel.Square_Roots;

package body Prunt.Motion_Planner.Stereographic_Kernel.Finish_Correction
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Constants of the error analysis
   ---------------------------------------------------------------------------

   Last_Real : constant := 2.0**1024 - 2.0**971;
   --  Dimensionless'Last, as a real number.

   Half_Last_Real : constant := 2.0**1023 - 2.0**970;
   --  Half_Last, as a real number.

   Quarter_Last_Real : constant := 2.0**1022 - 2.0**969;
   --  Dimensionless'Last / 4.0, as a real number.

   Three_Eighths_Last_Real : constant := 3.0 * (2.0**1021 - 2.0**968);
   --  Between the two: more than a rounded sum within a quarter can be, and still rounding within Half_Last.

   Smallest_Positive : constant := 2.0**(-1074);
   --  The smallest positive floating-point number.

   Half_Eta : constant := 2.0**(-1076);

   Three_Eta : constant := 3.0 * 2.0**(-1075);

   Six_Eta : constant := 6.0 * 2.0**(-1075);

   Half_Epsilon : constant := 2.0**(-54);

   Successor_Of_One : constant := 1.0 + 2.0**(-52);
   --  The floating-point number after one.

   One_Less : constant := 1.0 - 2.0**(-53);
   --  A rounded operation keeps at least this fraction of a nonnegative exact result, up to Eta.

   One_Less_Square : constant := One_Less * One_Less;

   Gain : constant := (1.0 - 2.0**(-53)) * (1.0 + 5.0 * 2.0**(-54));
   --  Up of a rounded nonnegative result is at least this multiple of the exact result: the rounding loses at
   --  most Epsilon of it and the padding adds at least Slack_Factor of what is left. It is 1 + 1.5·Epsilon to
   --  first order, and that surplus is what covers the one operation of the computation which is not padded,
   --  the quotient by Scale.

   Gain_Square : constant := Gain * Gain;

   Squared_Ratio_Gain : constant := Gain_Square * One_Less_Square;
   --  At least 1 + Epsilon / 2.

   Rate_Square : constant := Largest_Rate * Largest_Rate;

   ---------------------------------------------------------------------------
   --  Real arithmetic
   ---------------------------------------------------------------------------

   procedure Lemma_Square_Monotone (A, B : Real)
   with Ghost, Pre => 0.0 <= A and then A <= B, Post => A * A <= B * B;

   procedure Lemma_Square_Monotone (A, B : Real) is
   begin
      pragma Assert (A * A <= A * B);
      pragma Assert (A * B <= B * B);
   end Lemma_Square_Monotone;

   procedure Lemma_Square_Bound (D, B : Real)
   with Ghost, Pre => abs D <= B, Post => D * D <= B * B;

   procedure Lemma_Square_Bound (D, B : Real) is
   begin
      if D >= 0.0 then
         Lemma_Square_Monotone (D, B);
      else
         Lemma_Square_Monotone (0.0 - D, B);
         pragma Assert ((0.0 - D) * (0.0 - D) = D * D);
      end if;
   end Lemma_Square_Bound;

   procedure Lemma_Unit_Quotient (B, S : Real)
   with Ghost, Pre => S > 0.0 and then 0.0 <= B and then B <= S, Post => 0.0 <= B / S and then B / S <= 1.0;

   procedure Lemma_Unit_Quotient (B, S : Real) is
      Quotient : constant Real := B / S;
   begin
      pragma Assert (Quotient * S = B);
      if Quotient < 0.0 then
         pragma Assert (Quotient * S < 0.0);
      end if;
      if Quotient > 1.0 then
         pragma Assert (Quotient * S > 1.0 * S);
      end if;
   end Lemma_Unit_Quotient;

   procedure Lemma_Padded (Exact, Rounded, Padded : Real)
   with
     Ghost,
     Pre  =>
       Exact >= 0.0
       and then Rounded >= 0.0
       and then abs (Rounded - Exact) <= Epsilon * Exact + Eta
       and then Padded >= Rounded + (Slack_Factor * Rounded + Twice_Eta),
     Post => Padded >= Gain * Exact + Half_Eta and then Padded >= Exact;
   --  What an outward padding by the slack leaves of an exact nonnegative result after one rounding:
   --  (1 + Slack_Factor)·((1 - Epsilon)·Exact - Eta) + 2·Eta ≥ Gain·Exact + Eta/2.

   procedure Lemma_Padded (Exact, Rounded, Padded : Real) is
   begin
      pragma Assert (Rounded >= One_Less * Exact - Eta);
      pragma Assert (Padded >= (1.0 + Slack_Factor) * Rounded + Twice_Eta);
      pragma Assert (Padded >= (1.0 + Slack_Factor) * (One_Less * Exact - Eta) + Twice_Eta);
      pragma Assert ((1.0 + Slack_Factor) * (One_Less * Exact - Eta) + Twice_Eta >= Gain * Exact + Half_Eta);
      pragma Assert (Gain * Exact >= Exact);
   end Lemma_Padded;

   procedure Lemma_Rounded_Ratio_Square (Rho, Q : Real)
   with
     Ghost,
     Pre  => 0.0 <= Rho and then Rho <= 1.0 and then Q >= 0.0 and then abs (Q - Rho) <= Epsilon * Rho + Eta,
     Post => Gain_Square * (Q * Q) + Half_Eta >= Rho * Rho;
   --  The quotient by Scale is rounded and not padded, so its square can be below the exact one by twice
   --  Epsilon. Two gains make up for it: Gain²·(1 - Epsilon)² ≥ 1 + Epsilon/2. A quotient small enough to
   --  underflow errs by Eta instead, which costs at most 3·Eta·Rho; that is below Eta/2 while
   --  Epsilon·Rho < 6·Eta and below the surplus Epsilon/2·Rho² from there on.

   procedure Lemma_Rounded_Ratio_Square (Rho, Q : Real) is
      X : constant Real := One_Less * Rho;
   begin
      pragma Assert (X >= 0.0);
      pragma Assert (Q >= X - Eta);
      if X >= Eta then
         Lemma_Square_Monotone (X - Eta, Q);
         pragma Assert ((X - Eta) * (X - Eta) = X * X - Twice_Eta * X + Eta * Eta);
         pragma Assert (Q * Q >= X * X - Twice_Eta * X);
      else
         pragma Assert (X * X <= Eta * X);
         pragma Assert (Q * Q >= 0.0);
         pragma Assert (Q * Q >= X * X - Twice_Eta * X);
      end if;
      pragma Assert (X * X = One_Less_Square * (Rho * Rho));
      pragma Assert (Rho * Rho >= 0.0);
      pragma Assert (Gain_Square * (Q * Q) >= Gain_Square * (X * X) - Gain_Square * (Twice_Eta * X));
      pragma Assert (Gain_Square * (X * X) = Squared_Ratio_Gain * (Rho * Rho));
      pragma Assert (Gain_Square * (Twice_Eta * X) <= Three_Eta * Rho);
      pragma Assert (Squared_Ratio_Gain * (Rho * Rho) >= Rho * Rho + Half_Epsilon * (Rho * Rho));
      pragma Assert (Gain_Square * (Q * Q) >= Rho * Rho + Half_Epsilon * (Rho * Rho) - Three_Eta * Rho);
      if Epsilon * Rho >= Six_Eta then
         pragma Assert (Rho * (Epsilon * Rho) >= Rho * Six_Eta);
         pragma Assert (Half_Epsilon * (Rho * Rho) >= Three_Eta * Rho);
      else
         pragma Assert (Three_Eta * Rho <= Half_Eta);
      end if;
   end Lemma_Rounded_Ratio_Square;

   procedure Lemma_Step (Old_Sum, Term, New_Sum, Rounded_Square, Exact_Square : Real)
   with
     Ghost,
     Pre  =>
       Old_Sum >= 0.0
       and then Rounded_Square >= 0.0
       and then Term >= Gain * Rounded_Square + Half_Eta
       and then New_Sum >= Gain * (Old_Sum + Term) + Half_Eta
       and then Gain_Square * Rounded_Square + Half_Eta >= Exact_Square,
     Post => New_Sum >= Old_Sum + Exact_Square;
   --  One step of the sum of squares: the two paddings give the two gains.

   procedure Lemma_Step (Old_Sum, Term, New_Sum, Rounded_Square, Exact_Square : Real) is
   begin
      pragma Assert (Gain * Old_Sum >= Old_Sum);
      pragma Assert (Gain * Term >= Gain * (Gain * Rounded_Square + Half_Eta));
      pragma Assert (Gain * (Gain * Rounded_Square + Half_Eta) >= Gain_Square * Rounded_Square + Half_Eta);
      pragma Assert (New_Sum >= Gain * Old_Sum + Gain * Term);
   end Lemma_Step;

   procedure Lemma_Quotient_Bound (Difference, Magnitude, Length, Bound : Real)
   with
     Ghost,
     Pre  => Length > 0.0 and then abs Difference <= Magnitude and then Bound >= Magnitude / Length,
     Post => abs Difference <= Bound * Length;

   procedure Lemma_Quotient_Bound (Difference, Magnitude, Length, Bound : Real) is
      Quotient : constant Real := Magnitude / Length;
   begin
      pragma Assert (Quotient * Length = Magnitude);
      pragma Assert (Bound * Length >= Quotient * Length);
   end Lemma_Quotient_Bound;

   procedure Lemma_Rate_Product (Exact_Norm, Norm, Product, Correction : Real)
   with
     Ghost,
     Pre  =>
       Exact_Norm >= 0.0
       and then Norm >= Gain * Exact_Norm + Half_Eta
       and then abs (Product - Largest_Rate * Norm) <= Epsilon * (Largest_Rate * Norm) + Eta
       and then Correction >= Product,
     Post => Correction >= Largest_Rate * Exact_Norm;
   --  The gain of the padded norm covers the rounding of its product with the largest rate, whether or not that
   --  product is padded in turn: (1 - Epsilon)·693/256·(Gain·X + Eta/2) - Eta ≥ 693/256·X.

   procedure Lemma_Rate_Product (Exact_Norm, Norm, Product, Correction : Real) is
   begin
      pragma Assert (Product >= One_Less * (Largest_Rate * Norm) - Eta);
      pragma Assert (One_Less * (Largest_Rate * Norm) >= One_Less * (Largest_Rate * (Gain * Exact_Norm + Half_Eta)));
      pragma Assert (One_Less * (Largest_Rate * (Gain * Exact_Norm + Half_Eta)) - Eta >= Largest_Rate * Exact_Norm);
   end Lemma_Rate_Product;

   procedure Lemma_Unscale (B, S : Real)
   with Ghost, Pre => S > 0.0, Post => S * S * ((B / S) * (B / S)) = B * B;

   procedure Lemma_Unscale (B, S : Real) is
      Quotient : constant Real := B / S;
   begin
      pragma Assert (Quotient * S = B);
      pragma Assert (S * S * (Quotient * Quotient) = (Quotient * S) * (Quotient * S));
   end Lemma_Unscale;

   procedure Lemma_Ratio_Squares (B1, B2, B3, B4, S, Sum, N : Real)
   with
     Ghost,
     Pre  =>
       S > 0.0
       and then Sum >= (B1 / S) * (B1 / S) + (B2 / S) * (B2 / S) + (B3 / S) * (B3 / S) + (B4 / S) * (B4 / S)
       and then N * N >= Sum,
     Post => (S * N) * (S * N) >= B1 * B1 + B2 * B2 + B3 * B3 + B4 * B4;
   --  Undo the scaling: with ρ = B/S, S²·N² ≥ S²·Σ ρ² = Σ B².

   procedure Lemma_Ratio_Squares (B1, B2, B3, B4, S, Sum, N : Real) is
      Scale_Square : constant Real := S * S;
      R1           : constant Real := (B1 / S) * (B1 / S);
      R2           : constant Real := (B2 / S) * (B2 / S);
      R3           : constant Real := (B3 / S) * (B3 / S);
      R4           : constant Real := (B4 / S) * (B4 / S);
   begin
      Lemma_Unscale (B1, S);
      Lemma_Unscale (B2, S);
      Lemma_Unscale (B3, S);
      Lemma_Unscale (B4, S);
      pragma Assert (Scale_Square >= 0.0);
      pragma Assert (Scale_Square * (N * N) >= Scale_Square * Sum);
      pragma Assert (Scale_Square * Sum >= Scale_Square * (R1 + R2 + R3 + R4));
      pragma
        Assert
          (Scale_Square * (R1 + R2 + R3 + R4)
           = Scale_Square * R1 + Scale_Square * R2 + Scale_Square * R3 + Scale_Square * R4);
      pragma Assert ((S * N) * (S * N) = Scale_Square * (N * N));
   end Lemma_Ratio_Squares;

   procedure Lemma_Scaled_Square (Exact_Norm, Correction, Squares : Real)
   with
     Ghost,
     Pre  =>
       Exact_Norm >= 0.0 and then Correction >= Largest_Rate * Exact_Norm and then Exact_Norm * Exact_Norm >= Squares,
     Post => Correction * Correction >= Rate_Square * Squares;

   procedure Lemma_Scaled_Square (Exact_Norm, Correction, Squares : Real) is
   begin
      Lemma_Square_Monotone (Largest_Rate * Exact_Norm, Correction);
      pragma
        Assert ((Largest_Rate * Exact_Norm) * (Largest_Rate * Exact_Norm) = Rate_Square * (Exact_Norm * Exact_Norm));
      pragma Assert (Rate_Square * (Exact_Norm * Exact_Norm) >= Rate_Square * Squares);
   end Lemma_Scaled_Square;

   procedure Lemma_Axis_Term (Difference, Length, Bound : Real)
   with
     Ghost,
     Pre  => Length > 0.0 and then abs Difference <= Bound * Length,
     Post =>
       (Largest_Rate * Difference / Length) * (Largest_Rate * Difference / Length) <= Rate_Square * (Bound * Bound);
   --  One axis of the correction's largest tangent is within 693/256 times the axis bound.

   procedure Lemma_Axis_Term (Difference, Length, Bound : Real) is
      Quotient : constant Real := Difference / Length;
   begin
      pragma Assert (Quotient * Length = Difference);
      pragma Assert ((Bound - Quotient) * Length >= 0.0);
      pragma Assert ((Bound + Quotient) * Length >= 0.0);
      if Bound - Quotient < 0.0 then
         pragma Assert ((Bound - Quotient) * Length < 0.0);
      end if;
      if Bound + Quotient < 0.0 then
         pragma Assert ((Bound + Quotient) * Length < 0.0);
      end if;
      pragma Assert (abs Quotient <= Bound);
      Lemma_Square_Bound (Quotient, Bound);
      pragma Assert (Largest_Rate * Difference / Length = Largest_Rate * Quotient);
      pragma Assert ((Largest_Rate * Quotient) * (Largest_Rate * Quotient) = Rate_Square * (Quotient * Quotient));
      pragma Assert (Rate_Square * (Quotient * Quotient) <= Rate_Square * (Bound * Bound));
   end Lemma_Axis_Term;

   procedure Lemma_Total_Real (Tangent, Correction, Total, Squares : Real)
   with
     Ghost,
     Pre  => Correction >= 0.0 and then Total >= Tangent + Correction and then Squares <= Correction * Correction,
     Post => Total >= Tangent and then Squares <= (Total - Tangent) * (Total - Tangent);

   procedure Lemma_Total_Real (Tangent, Correction, Total, Squares : Real) is
   begin
      Lemma_Square_Monotone (Correction, Total - Tangent);
   end Lemma_Total_Real;

   ---------------------------------------------------------------------------
   --  The rate
   ---------------------------------------------------------------------------

   procedure Lemma_Fifth_Power (P : Real)
   with
     Ghost,
     Pre  => 0.0 <= P and then P <= 0.25,
     Post => 0.0 <= P * P * P * P * P and then 1024.0 * (P * P * P * P * P) <= 1.0;

   procedure Lemma_Fifth_Power (P : Real) is
      Square : constant Real := P * P;
      Fourth : constant Real := Square * Square;
   begin
      pragma Assert (Square >= 0.0 and then Square <= 0.25 * P);
      pragma Assert (Square <= 0.0625);
      pragma Assert (Fourth >= 0.0 and then Fourth <= 0.0625 * Square);
      pragma Assert (Fourth <= 0.00390625);
      pragma Assert (Fourth * P >= 0.0 and then Fourth * P <= 0.00390625 * P);
      pragma Assert (P * P * P * P * P = Fourth * P);
   end Lemma_Fifth_Power;

   procedure Lemma_Product_Of_Fifth_Powers (A, B : Real)
   with Ghost, Post => (A * A * A * A * A) * (B * B * B * B * B) = (A * B) * (A * B) * (A * B) * (A * B) * (A * B);

   procedure Lemma_Product_Of_Fifth_Powers (A, B : Real) is null;

   procedure Lemma_Rate (U : Real) is
      Complement : constant Real := 1.0 - U;
      Product    : constant Real := U * Complement;
   begin
      --  U·(1 - U) = 1/4 - (U - 1/2)² is between zero and a quarter, and the rate is 2772 times its fifth power.
      pragma Assert (Product >= 0.0);
      pragma Assert ((U - 0.5) * (U - 0.5) >= 0.0);
      pragma Assert (Product = 0.25 - (U - 0.5) * (U - 0.5));
      Lemma_Fifth_Power (Product);
      Lemma_Product_Of_Fifth_Powers (U, Complement);
      pragma Assert (Rate (U) = 2772.0 * (Product * Product * Product * Product * Product));
   end Lemma_Rate;

   ---------------------------------------------------------------------------
   --  One rounded operation at a time
   ---------------------------------------------------------------------------

   procedure Lemma_Smallest_Positive (Value : Dimensionless)
   with Ghost, Pre => Value > 0.0, Post => R (Value) >= Smallest_Positive;

   procedure Lemma_Smallest_Positive (Value : Dimensionless) is null;

   procedure Lemma_Half_Last_Product (Length_Value : Dimensionless)
   with
     Ghost,
     Pre  => Length_Value > 0.0 and then Length_Value < 1.0,
     Post =>
       R ((Dimensionless'Last / 2.0) * Length_Value)
       <= Half_Last_Real * R (Length_Value) + Epsilon * (Half_Last_Real * R (Length_Value)) + Eta;

   procedure Lemma_Half_Last_Product (Length_Value : Dimensionless) is null;

   procedure Lemma_Guard (Magnitude, Length_Value : Dimensionless)
   with
     Ghost,
     Pre  =>
       Length_Value > 0.0
       and then Magnitude >= 0.0
       and then (Length_Value >= 1.0 or else Magnitude <= (Dimensionless'Last / 2.0) * Length_Value),
     Post => R (Magnitude) <= Last_Real * R (Length_Value);
   --  What the test before the quotient leaves: the exact quotient is within the largest number.

   procedure Lemma_Guard (Magnitude, Length_Value : Dimensionless) is
   begin
      if Length_Value >= 1.0 then
         pragma Assert (R (Length_Value) >= 1.0);
         pragma Assert (R (Magnitude) <= Last_Real);
      else
         Lemma_Half_Last_Product (Length_Value);
         Lemma_Smallest_Positive (Length_Value);
         pragma Assert (R (Magnitude) <= R ((Dimensionless'Last / 2.0) * Length_Value));
         pragma Assert (Eta <= 0.5 * R (Length_Value));
         pragma
           Assert
             (R (Magnitude)
              <= Half_Last_Real * R (Length_Value) + Epsilon * (Half_Last_Real * R (Length_Value))
                 + 0.5 * R (Length_Value));
      end if;
   end Lemma_Guard;

   procedure Lemma_Quotient_Rounds (Numerator, Denominator : Dimensionless)
   with
     Ghost,
     Pre  => Numerator >= 0.0 and then Denominator > 0.0 and then R (Numerator) <= Last_Real * R (Denominator),
     Post =>
       Numerator / Denominator >= 0.0
       and then Is_Rounding_Of (Numerator / Denominator, R (Numerator) / R (Denominator));

   procedure Lemma_Quotient_Rounds (Numerator, Denominator : Dimensionless) is null;

   procedure Lemma_Below_Successor_Of_One (Ratio : Dimensionless)
   with Ghost, Pre => Ratio < 1.0 + 2.0**(-52), Post => Ratio <= 1.0;
   --  No floating-point number lies strictly between one and its successor.

   procedure Lemma_Below_Successor_Of_One (Ratio : Dimensionless) is null;

   procedure Lemma_Real_Below_Successor_Of_One (Ratio : Dimensionless)
   with Ghost, Pre => R (Ratio) < Successor_Of_One, Post => Ratio < 1.0 + 2.0**(-52);

   procedure Lemma_Real_Below_Successor_Of_One (Ratio : Dimensionless) is null;

   procedure Lemma_Ratio (Bound, Scale : Dimensionless)
   with
     Ghost,
     Pre  => Scale > 0.0 and then Bound >= 0.0 and then Bound <= Scale,
     Post =>
       Bound / Scale >= 0.0
       and then Bound / Scale <= 1.0
       and then Is_Rounding_Of (Bound / Scale, R (Bound) / R (Scale));
   --  The quotient of an axis bound by the largest of them rounds to at most one.

   procedure Lemma_Ratio (Bound, Scale : Dimensionless) is
   begin
      pragma Assert (R (Bound) <= R (Scale));
      pragma Assert (R (Bound) <= Last_Real * R (Scale));
      Lemma_Quotient_Rounds (Bound, Scale);
      Lemma_Unit_Quotient (R (Bound), R (Scale));
      pragma Assert (R (Bound / Scale) <= 1.0 + Epsilon + Eta);
      Lemma_Real_Below_Successor_Of_One (Bound / Scale);
      Lemma_Below_Successor_Of_One (Bound / Scale);
   end Lemma_Ratio;

   procedure Lemma_Square_Rounds (Ratio : Dimensionless)
   with
     Ghost,
     Pre  => Ratio >= 0.0 and then Ratio <= 1.0,
     Post => Ratio**2 >= 0.0 and then Ratio**2 <= 1.0 and then Is_Rounding_Of (Ratio**2, R (Ratio) * R (Ratio));

   procedure Lemma_Square_Rounds (Ratio : Dimensionless) is null;

   procedure Lemma_Rate_Product_Rounds (Norm : Dimensionless)
   with
     Ghost,
     Pre  => Norm >= 0.0 and then Norm <= Dimensionless'Last / 4.0,
     Post =>
       Largest_Rate * R (Norm) >= 0.0
       and then Largest_Rate * R (Norm) <= Last_Real
       and then (693.0 / 256.0) * Norm >= 0.0
       and then Is_Rounding_Of ((693.0 / 256.0) * Norm, Largest_Rate * R (Norm));

   procedure Lemma_Rate_Product_Rounds (Norm : Dimensionless) is null;

   procedure Lemma_Up_Bound (Value : Dimensionless)
   with
     Ghost,
     Pre  => Value >= 0.0 and then Value <= 14.0,
     Post =>
       Up (Value) >= 0.0
       and then Up (Value) <= 16.0
       and then (if Value <= 10.0 then Up (Value) <= 12.0)
       and then (if Value <= 6.0 then Up (Value) <= 8.0)
       and then (if Value <= 2.0 then Up (Value) <= 4.0)
       and then (if Value <= 1.0 then Up (Value) <= 2.0);
   --  The padding of a small number is small.

   procedure Lemma_Up_Bound (Value : Dimensionless) is null;

   procedure Lemma_Up_Gain (Value : Dimensionless; Exact : Real)
   with
     Ghost,
     Pre  => Value >= 0.0 and then Value <= Half_Last and then Exact >= 0.0 and then Is_Rounding_Of (Value, Exact),
     Post => R (Up (Value)) >= Gain * Exact + Half_Eta and then R (Up (Value)) >= Exact;
   --  Up of a rounded result, within the range where the padding is known, has the gain.

   procedure Lemma_Up_Gain (Value : Dimensionless; Exact : Real) is
   begin
      Lemma_Inflate_Margin (Value);
      pragma Assert (R (Up (Value)) >= R (Value) + Slack (Value));
      Lemma_Padded (Exact, R (Value), R (Up (Value)));
   end Lemma_Up_Gain;

   procedure Lemma_Max (Left, Right : Dimensionless)
   with
     Ghost,
     Post =>
       R (Dimensionless'Max (Left, Right)) >= R (Left)
       and then R (Dimensionless'Max (Left, Right)) >= R (Right)
       and then
         (R (Dimensionless'Max (Left, Right)) = R (Left) or else R (Dimensionless'Max (Left, Right)) = R (Right));
   --  The provers know little about the larger of two numbers, so main states what it needs of it over the
   --  reals.

   procedure Lemma_Max (Left, Right : Dimensionless) is
      Larger : constant Dimensionless := Dimensionless'Max (Left, Right);
   begin
      if Left >= Right then
         pragma Assert (Larger = Left);
         pragma Assert (R (Larger) = R (Left));
      else
         pragma Assert (Larger = Right);
         pragma Assert (R (Larger) = R (Right));
      end if;
   end Lemma_Max;

   procedure Lemma_Half_Last_Quotient (Divisor : Dimensionless)
   with
     Ghost,
     Pre  => Divisor >= 1.0,
     Post =>
       Half_Last_Real / R (Divisor) >= 0.0
       and then Half_Last_Real / R (Divisor) <= Half_Last_Real
       and then (Dimensionless'Last / 2.0) / Divisor >= 0.0
       and then (Dimensionless'Last / 2.0) / Divisor <= Dimensionless'Last;

   procedure Lemma_Half_Last_Quotient (Divisor : Dimensionless) is
      Quotient : constant Real := Half_Last_Real / R (Divisor);
   begin
      pragma Assert (R (Divisor) >= 1.0);
      pragma Assert (Quotient * R (Divisor) = Half_Last_Real);
      if Quotient < 0.0 then
         pragma Assert (Quotient * R (Divisor) < 0.0);
      end if;
      if Quotient > Half_Last_Real then
         pragma Assert (Quotient * R (Divisor) >= Quotient * 1.0);
      end if;
   end Lemma_Half_Last_Quotient;

   ---------------------------------------------------------------------------
   --  What the loops maintain, one axis at a time
   ---------------------------------------------------------------------------

   function Correction_Square
     (Finish : Dimensionless_Axis_Vector; Uncorrected : Dimensionless_Axis_Vector; Length_Value : Dimensionless)
      return Real
   is ((Largest_Rate * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
       * (Largest_Rate * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
       + (Largest_Rate * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
         * (Largest_Rate * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
       + (Largest_Rate * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
         * (Largest_Rate * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
       + (Largest_Rate * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value))
         * (Largest_Rate * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value)))
   with Ghost, Pre => Length_Value > 0.0;
   --  The squared norm of the largest tangent which the correction can add: the left side of Is_Total_Bound.

   function Is_Axis_Bound
     (Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Bounds       : Dimensionless_Axis_Vector;
      Axis         : Axis_Name) return Boolean
   is (abs (R (Finish (Axis)) - R (Uncorrected (Axis))) <= R (Bounds (Axis)) * R (Length_Value))
   with Ghost;
   --  Whether the bound of one axis is at least |Finish - Uncorrected| / Length there.

   function Are_Axis_Bounds
     (Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Bounds       : Dimensionless_Axis_Vector;
      Count        : Natural) return Boolean
   is ((if Count >= 1 then Is_Axis_Bound (Finish, Uncorrected, Length_Value, Bounds, X_Axis))
       and then (if Count >= 2 then Is_Axis_Bound (Finish, Uncorrected, Length_Value, Bounds, Y_Axis))
       and then (if Count >= 3 then Is_Axis_Bound (Finish, Uncorrected, Length_Value, Bounds, Z_Axis))
       and then (if Count >= 4 then Is_Axis_Bound (Finish, Uncorrected, Length_Value, Bounds, E_Axis)))
   with Ghost;
   --  The same of the first Count axes. The facts about the axes are stated one axis at a time, without
   --  quantifiers, which is what keeps them cheap for the provers.

   function Are_Within (Bounds : Dimensionless_Axis_Vector; Scale : Dimensionless) return Boolean
   is (0.0 <= R (Bounds (X_Axis))
       and then R (Bounds (X_Axis)) <= R (Scale)
       and then 0.0 <= R (Bounds (Y_Axis))
       and then R (Bounds (Y_Axis)) <= R (Scale)
       and then 0.0 <= R (Bounds (Z_Axis))
       and then R (Bounds (Z_Axis)) <= R (Scale)
       and then 0.0 <= R (Bounds (E_Axis))
       and then R (Bounds (E_Axis)) <= R (Scale))
   with Ghost;
   --  Whether every axis bound is between zero and Scale. This and the next are stated over the reals, where
   --  the provers need not consider whether a number is finite.

   function Rest_Is_Zero (Bounds : Dimensionless_Axis_Vector; Count : Natural) return Boolean
   is ((if Count < 1 then R (Bounds (X_Axis)) = 0.0)
       and then (if Count < 2 then R (Bounds (Y_Axis)) = 0.0)
       and then (if Count < 3 then R (Bounds (Z_Axis)) = 0.0)
       and then (if Count < 4 then R (Bounds (E_Axis)) = 0.0))
   with Ghost;
   --  Whether the bounds after the first Count axes are still zero.

   function Is_Attained (Bounds : Dimensionless_Axis_Vector; Scale : Dimensionless) return Boolean
   is (R (Bounds (X_Axis)) = R (Scale)
       or else R (Bounds (Y_Axis)) = R (Scale)
       or else R (Bounds (Z_Axis)) = R (Scale)
       or else R (Bounds (E_Axis)) = R (Scale))
   with Ghost;
   --  Whether Scale is one of the axis bounds.

   function Ratio_Square (Bound, Scale : Dimensionless) return Real
   is ((R (Bound) / R (Scale)) * (R (Bound) / R (Scale)))
   with Ghost, Pre => Scale > 0.0;

   function Ratio_Squares (Bounds : Dimensionless_Axis_Vector; Scale : Dimensionless; Count : Natural) return Real
   is (case Count is
         when 0      => 0.0,
         when 1      => Ratio_Square (Bounds (X_Axis), Scale),
         when 2      => Ratio_Square (Bounds (X_Axis), Scale) + Ratio_Square (Bounds (Y_Axis), Scale),
         when 3      =>
           Ratio_Square (Bounds (X_Axis), Scale)
           + Ratio_Square (Bounds (Y_Axis), Scale)
           + Ratio_Square (Bounds (Z_Axis), Scale),
         when others =>
           Ratio_Square (Bounds (X_Axis), Scale)
           + Ratio_Square (Bounds (Y_Axis), Scale)
           + Ratio_Square (Bounds (Z_Axis), Scale)
           + Ratio_Square (Bounds (E_Axis), Scale))
   with Ghost, Pre => Scale > 0.0;
   --  The exact squares of the first Count axis bounds over Scale.

   function Sum_Limit (Count : Natural) return Real
   is (case Count is
         when 0      => 0.0,
         when 1      => 4.0,
         when 2      => 8.0,
         when 3      => 12.0,
         when others => 16.0)
   with Ghost;
   --  Each term of the sum is at most one before its padding.

   ---------------------------------------------------------------------------
   --  The stages of the bound
   ---------------------------------------------------------------------------

   procedure Lemma_Magnitude (Correction : Interval; Magnitude : Dimensionless; Difference : Real)
   with
     Ghost,
     Pre  =>
       R (Correction.Lower) <= Difference
       and then Difference <= R (Correction.Upper)
       and then Magnitude >= abs Correction.Lower
       and then Magnitude >= abs Correction.Upper,
     Post => Magnitude >= 0.0 and then abs Difference <= R (Magnitude);
   --  The largest magnitude of an interval's endpoints bounds the magnitude of every member.

   procedure Lemma_Magnitude (Correction : Interval; Magnitude : Dimensionless; Difference : Real) is
   begin
      pragma Assert (R (Magnitude) >= abs (R (Correction.Lower)));
      pragma Assert (R (Magnitude) >= abs (R (Correction.Upper)));
   end Lemma_Magnitude;

   procedure Lemma_Axis_Bound (Magnitude, Length_Value : Dimensionless; Difference : Real)
   with
     Ghost,
     Pre  =>
       Length_Value > 0.0
       and then Magnitude >= 0.0
       and then (Length_Value >= 1.0 or else Magnitude <= (Dimensionless'Last / 2.0) * Length_Value)
       and then abs Difference <= R (Magnitude),
     Post =>
       R (Magnitude) / R (Length_Value) >= 0.0
       and then R (Magnitude) / R (Length_Value) <= Last_Real
       and then R (Up (Magnitude / Length_Value)) >= 0.0
       and then
         (R (Up (Magnitude / Length_Value)) > Half_Last_Real
          or else abs Difference <= R (Up (Magnitude / Length_Value)) * R (Length_Value));
   --  The bound of one axis is at least |Difference| / Length, unless it is too large for the padding to be
   --  known, in which case it is beyond Half_Last.

   procedure Lemma_Axis_Bound (Magnitude, Length_Value : Dimensionless; Difference : Real) is
   begin
      Lemma_Guard (Magnitude, Length_Value);
      Lemma_Quotient_Rounds (Magnitude, Length_Value);
      declare
         Quotient : constant Dimensionless := Magnitude / Length_Value;
         Exact    : constant Real := R (Magnitude) / R (Length_Value);
      begin
         pragma Assert (R (Length_Value) > 0.0);
         pragma Assert (R (Magnitude) >= 0.0);
         pragma Assert (Exact * R (Length_Value) = R (Magnitude));
         if Exact < 0.0 then
            pragma Assert (Exact * R (Length_Value) < 0.0);
         end if;
         pragma Assert (Exact >= 0.0);
         if Exact > Last_Real then
            pragma Assert (Exact * R (Length_Value) > Last_Real * R (Length_Value));
         end if;
         pragma Assert (Exact <= Last_Real);
         pragma Assert (Up (Quotient) >= 0.0);
         if Quotient <= Half_Last then
            Lemma_Up_Gain (Quotient, Exact);
            Lemma_Quotient_Bound (Difference, R (Magnitude), R (Length_Value), R (Up (Quotient)));
         else
            pragma Assert (Up (Quotient) > Half_Last);
         end if;
      end;
   end Lemma_Axis_Bound;

   procedure Lemma_Sum_Step (Bound, Scale, Sum : Dimensionless)
   with
     Ghost,
     Pre  =>
       Scale > 0.0
       and then 0.0 <= R (Bound)
       and then R (Bound) <= R (Scale)
       and then 0.0 <= R (Sum)
       and then R (Sum) <= 12.0,
     Post =>
       0.0 <= R (Bound) / R (Scale)
       and then R (Bound) / R (Scale) <= 1.0
       and then 0.0 <= R (Bound / Scale) * R (Bound / Scale)
       and then R (Bound / Scale) * R (Bound / Scale) <= 1.0
       and then 0.0 <= R (Sum) + R (Up ((Bound / Scale)**2))
       and then R (Sum) + R (Up ((Bound / Scale)**2)) <= 14.0
       and then 0.0 <= R (Up (Sum + Up ((Bound / Scale)**2)))
       and then R (Up (Sum + Up ((Bound / Scale)**2))) <= 16.0
       and then (if R (Sum) <= 8.0 then R (Up (Sum + Up ((Bound / Scale)**2))) <= 12.0)
       and then (if R (Sum) <= 4.0 then R (Up (Sum + Up ((Bound / Scale)**2))) <= 8.0)
       and then (if R (Sum) <= 0.0 then R (Up (Sum + Up ((Bound / Scale)**2))) <= 4.0)
       and then R (Up (Sum + Up ((Bound / Scale)**2))) >= R (Sum) + Ratio_Square (Bound, Scale);
   --  One step of the sum stays small and adds at least the exact square of the axis bound over Scale. The
   --  exact results of its three operations are bounded here too, which is what shows main that none overflows.

   procedure Lemma_Sum_Step (Bound, Scale, Sum : Dimensionless) is
      Ratio : constant Dimensionless := Bound / Scale;
      Rho   : constant Real := R (Bound) / R (Scale);
   begin
      pragma Assert (Bound >= 0.0 and then Bound <= Scale and then Sum >= 0.0 and then Sum <= 12.0);
      Lemma_Ratio (Bound, Scale);
      Lemma_Square_Rounds (Ratio);
      Lemma_Unit_Quotient (R (Bound), R (Scale));
      Lemma_Square_Monotone (R (Ratio), 1.0);
      declare
         Square : constant Dimensionless := Ratio**2;
      begin
         pragma Assert (R (Ratio) * R (Ratio) >= 0.0);
         Lemma_Up_Bound (Square);
         Lemma_Up_Gain (Square, R (Ratio) * R (Ratio));
         declare
            Term : constant Dimensionless := Up (Square);
         begin
            pragma Assert (Term >= 0.0 and then Term <= 2.0);
            Float_Lemmas.Lemma_Rounding_Error_Add (Sum, Term);
            declare
               Total : constant Dimensionless := Sum + Term;
            begin
               pragma Assert (Total >= 0.0 and then Total <= 14.0);
               pragma Assert (if Sum <= 8.0 then Total <= 10.0);
               pragma Assert (if Sum <= 4.0 then Total <= 6.0);
               pragma Assert (if Sum <= 0.0 then Total <= 2.0);
               pragma Assert (R (Sum) + R (Term) >= 0.0);
               pragma Assert (Is_Rounding_Of (Total, R (Sum) + R (Term)));
               Lemma_Up_Gain (Total, R (Sum) + R (Term));
               Lemma_Up_Bound (Total);
               Lemma_Rounded_Ratio_Square (Rho, R (Ratio));
               Lemma_Step (R (Sum), R (Term), R (Up (Total)), R (Ratio) * R (Ratio), Rho * Rho);
            end;
         end;
      end;
   end Lemma_Sum_Step;

   procedure Lemma_Unit_Norm (Bounds : Dimensionless_Axis_Vector; Scale, Sum, Unit_Norm : Dimensionless)
   with
     Ghost,
     Pre  =>
       Scale > 0.0
       and then Is_Attained (Bounds, Scale)
       and then R (Sum) >= Ratio_Squares (Bounds, Scale, 4)
       and then
         (Unit_Norm = Dimensionless'Last or else (Unit_Norm >= 0.0 and then R (Unit_Norm) * R (Unit_Norm) >= R (Sum))),
     Post => Unit_Norm >= 1.0 and then R (Unit_Norm) >= 1.0;
   --  The largest axis bound is Scale itself, so the sum of squares is at least one, and so is its root.

   procedure Lemma_Unit_Norm (Bounds : Dimensionless_Axis_Vector; Scale, Sum, Unit_Norm : Dimensionless) is
      SX : constant Real := Ratio_Square (Bounds (X_Axis), Scale);
      SY : constant Real := Ratio_Square (Bounds (Y_Axis), Scale);
      SZ : constant Real := Ratio_Square (Bounds (Z_Axis), Scale);
      SE : constant Real := Ratio_Square (Bounds (E_Axis), Scale);
   begin
      pragma Assert (R (Scale) > 0.0);
      pragma Assert (R (Scale) / R (Scale) = 1.0);
      pragma Assert (SX >= 0.0 and then SY >= 0.0 and then SZ >= 0.0 and then SE >= 0.0);
      pragma Assert (if R (Bounds (X_Axis)) = R (Scale) then SX = 1.0);
      pragma Assert (if R (Bounds (Y_Axis)) = R (Scale) then SY = 1.0);
      pragma Assert (if R (Bounds (Z_Axis)) = R (Scale) then SZ = 1.0);
      pragma Assert (if R (Bounds (E_Axis)) = R (Scale) then SE = 1.0);
      pragma Assert (SX + SY + SZ + SE >= 1.0);
      pragma Assert (R (Sum) >= 1.0);
      if Unit_Norm < Dimensionless'Last then
         if R (Unit_Norm) < 1.0 then
            pragma Assert (R (Unit_Norm) * R (Unit_Norm) <= 1.0 * R (Unit_Norm));
            pragma Assert (False);
         end if;
         pragma Assert (R (Unit_Norm) >= 1.0);
      end if;
   end Lemma_Unit_Norm;

   procedure Lemma_Product_Rounds (Left, Right : Dimensionless)
   with
     Ghost,
     Pre  => Left >= 0.0 and then Right >= 0.0 and then R (Left) * R (Right) <= Last_Real,
     Post => Left * Right >= 0.0 and then Is_Rounding_Of (Left * Right, R (Left) * R (Right));

   procedure Lemma_Product_Rounds (Left, Right : Dimensionless) is null;

   procedure Lemma_Norm_Product (Scale, Unit_Norm : Dimensionless)
   with
     Ghost,
     Pre  => R (Scale) >= 0.0 and then R (Unit_Norm) >= 1.0 and then R (Scale) * R (Unit_Norm) <= Last_Real,
     Post =>
       R (Scale) * R (Unit_Norm) >= 0.0
       and then
         (if R (Up (Scale * Unit_Norm)) <= Quarter_Last_Real
          then
            R (Up (Scale * Unit_Norm)) >= Gain * (R (Scale) * R (Unit_Norm)) + Half_Eta
            and then R (Up (Scale * Unit_Norm)) >= 0.0
            and then R (Scale) <= Half_Last_Real);
   --  The padded product of Scale and the root has the gain over the exact product, where it is within a
   --  quarter of the largest number. Scale is then within Half_Last, so that every axis bound was padded.

   procedure Lemma_Norm_Product (Scale, Unit_Norm : Dimensionless) is
      Exact : constant Real := R (Scale) * R (Unit_Norm);
   begin
      pragma Assert (Scale >= 0.0 and then Unit_Norm >= 1.0);
      pragma Assert (R (Scale) * (R (Unit_Norm) - 1.0) >= 0.0);
      pragma Assert (Exact >= R (Scale));
      Lemma_Product_Rounds (Scale, Unit_Norm);
      declare
         Product : constant Dimensionless := Scale * Unit_Norm;
      begin
         if R (Up (Product)) <= Quarter_Last_Real then
            pragma Assert (Product <= Up (Product));
            pragma Assert (R (Product) <= Quarter_Last_Real);
            pragma Assert (Product <= Half_Last);
            Lemma_Up_Gain (Product, Exact);
            pragma Assert (One_Less * Exact <= R (Product) + Eta);
            pragma Assert (One_Less * R (Scale) <= Quarter_Last_Real + Eta);
            pragma Assert (R (Scale) <= Half_Last_Real);
         end if;
      end;
   end Lemma_Norm_Product;

   procedure Lemma_No_Correction
     (Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Bounds       : Dimensionless_Axis_Vector;
      Scale        : Dimensionless)
   with
     Ghost,
     Pre  =>
       Length_Value > 0.0
       and then Are_Within (Bounds, Scale)
       and then (R (Scale) > Half_Last_Real or else Are_Axis_Bounds (Finish, Uncorrected, Length_Value, Bounds, 4)),
     Post => (if R (Scale) <= 0.0 then Correction_Square (Finish, Uncorrected, Length_Value) = 0.0);
   --  A zero Scale means that the finish points coincide on every axis.

   procedure Lemma_No_Correction
     (Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Bounds       : Dimensionless_Axis_Vector;
      Scale        : Dimensionless) is
   begin
      if R (Scale) <= 0.0 then
         pragma Assert (Are_Axis_Bounds (Finish, Uncorrected, Length_Value, Bounds, 4));
         pragma Assert (R (Bounds (X_Axis)) = 0.0);
         pragma Assert (R (Bounds (Y_Axis)) = 0.0);
         pragma Assert (R (Bounds (Z_Axis)) = 0.0);
         pragma Assert (R (Bounds (E_Axis)) = 0.0);
         pragma Assert (R (Finish (X_Axis)) - R (Uncorrected (X_Axis)) = 0.0);
         pragma Assert (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis)) = 0.0);
         pragma Assert (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis)) = 0.0);
         pragma Assert (R (Finish (E_Axis)) - R (Uncorrected (E_Axis)) = 0.0);
      end if;
   end Lemma_No_Correction;

   procedure Lemma_Correction_Bound
     (Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Bounds       : Dimensionless_Axis_Vector;
      Scale        : Dimensionless;
      Sum          : Dimensionless;
      Unit_Norm    : Dimensionless;
      Norm         : Dimensionless;
      Rate_Product : Dimensionless;
      Correction   : Dimensionless)
   with
     Ghost,
     Pre  =>
       Length_Value > 0.0
       and then Scale > 0.0
       and then Are_Axis_Bounds (Finish, Uncorrected, Length_Value, Bounds, 4)
       and then R (Sum) >= Ratio_Squares (Bounds, Scale, 4)
       and then R (Unit_Norm) >= 0.0
       and then R (Unit_Norm) * R (Unit_Norm) >= R (Sum)
       and then R (Norm) >= 0.0
       and then R (Norm) >= Gain * (R (Scale) * R (Unit_Norm)) + Half_Eta
       and then Is_Rounding_Of (Rate_Product, Largest_Rate * R (Norm))
       and then Correction >= Rate_Product,
     Post =>
       R (Correction) >= 0.0
       and then Correction_Square (Finish, Uncorrected, Length_Value) <= R (Correction) * R (Correction);
   --  The correction bound is at least 693/256 times the Euclidean norm of the axis bounds, each of which is at
   --  least the magnitude of its axis of (Finish - Uncorrected) / Length.

   procedure Lemma_Correction_Bound
     (Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Bounds       : Dimensionless_Axis_Vector;
      Scale        : Dimensionless;
      Sum          : Dimensionless;
      Unit_Norm    : Dimensionless;
      Norm         : Dimensionless;
      Rate_Product : Dimensionless;
      Correction   : Dimensionless)
   is
      Exact_Norm : constant Real := R (Scale) * R (Unit_Norm);
      Squares    : constant Real :=
        R (Bounds (X_Axis)) * R (Bounds (X_Axis)) + R (Bounds (Y_Axis)) * R (Bounds (Y_Axis))
        + R (Bounds (Z_Axis)) * R (Bounds (Z_Axis))
        + R (Bounds (E_Axis)) * R (Bounds (E_Axis));
   begin
      pragma Assert (R (Scale) > 0.0 and then R (Unit_Norm) >= 0.0);
      pragma Assert (Exact_Norm >= 0.0);
      pragma Assert (R (Norm) >= 0.0);
      pragma Assert (abs (Largest_Rate * R (Norm)) = Largest_Rate * R (Norm));
      Lemma_Rate_Product (Exact_Norm, R (Norm), R (Rate_Product), R (Correction));
      Lemma_Ratio_Squares
        (R (Bounds (X_Axis)),
         R (Bounds (Y_Axis)),
         R (Bounds (Z_Axis)),
         R (Bounds (E_Axis)),
         R (Scale),
         R (Sum),
         R (Unit_Norm));
      Lemma_Scaled_Square (Exact_Norm, R (Correction), Squares);
      Lemma_Axis_Term (R (Finish (X_Axis)) - R (Uncorrected (X_Axis)), R (Length_Value), R (Bounds (X_Axis)));
      Lemma_Axis_Term (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis)), R (Length_Value), R (Bounds (Y_Axis)));
      Lemma_Axis_Term (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis)), R (Length_Value), R (Bounds (Z_Axis)));
      Lemma_Axis_Term (R (Finish (E_Axis)) - R (Uncorrected (E_Axis)), R (Length_Value), R (Bounds (E_Axis)));
      pragma
        Assert
          (Rate_Square * Squares
           = Rate_Square * (R (Bounds (X_Axis)) * R (Bounds (X_Axis)))
             + Rate_Square * (R (Bounds (Y_Axis)) * R (Bounds (Y_Axis)))
             + Rate_Square * (R (Bounds (Z_Axis)) * R (Bounds (Z_Axis)))
             + Rate_Square * (R (Bounds (E_Axis)) * R (Bounds (E_Axis))));
      pragma Assert (Correction_Square (Finish, Uncorrected, Length_Value) <= Rate_Square * Squares);
   end Lemma_Correction_Bound;

   procedure Lemma_Sum_Rounds (Left, Right : Dimensionless)
   with
     Ghost,
     Pre  => Left >= 0.0 and then Right >= 0.0 and then R (Left) + R (Right) <= Last_Real,
     Post => Left + Right >= 0.0 and then Is_Rounding_Of (Left + Right, R (Left) + R (Right));

   procedure Lemma_Sum_Rounds (Left, Right : Dimensionless) is null;

   procedure Lemma_Total
     (Tangent_Error : Dimensionless;
      Correction    : Dimensionless;
      Finish        : Dimensionless_Axis_Vector;
      Uncorrected   : Dimensionless_Axis_Vector;
      Length_Value  : Dimensionless)
   with
     Ghost,
     Pre  =>
       Tangent_Error >= 0.0
       and then R (Correction) >= 0.0
       and then Length_Value > 0.0
       and then Correction_Square (Finish, Uncorrected, Length_Value) <= R (Correction) * R (Correction)
       and then R (Tangent_Error) + R (Correction) <= Three_Eighths_Last_Real,
     Post =>
       R (Tangent_Error) + R (Correction) >= 0.0
       and then Up (Tangent_Error + Correction) >= 0.0
       and then Is_Total_Bound (Tangent_Error, Finish, Uncorrected, Length_Value, Up (Tangent_Error + Correction));
   --  The padded sum of the tangent error and the correction bound is a total bound. The sum is to round
   --  within Half_Last, where the padding of Up is known.

   procedure Lemma_Total
     (Tangent_Error : Dimensionless;
      Correction    : Dimensionless;
      Finish        : Dimensionless_Axis_Vector;
      Uncorrected   : Dimensionless_Axis_Vector;
      Length_Value  : Dimensionless) is
   begin
      pragma Assert (Correction >= 0.0);
      pragma Assert (R (Tangent_Error) + R (Correction) <= Last_Real);
      Lemma_Sum_Rounds (Tangent_Error, Correction);
      declare
         Sum : constant Dimensionless := Tangent_Error + Correction;
      begin
         Lemma_Rounding_Slack (Sum, R (Tangent_Error) + R (Correction));
         pragma Assert (R (Sum) <= Half_Last_Real);
         pragma Assert (Sum <= Half_Last);
         Lemma_Inflate_Margin (Sum);
         pragma Assert (R (Up (Sum)) >= R (Sum) + Slack (Sum));
         pragma Assert (Up (Sum) >= 0.0);
         pragma Assert (R (Up (Sum)) >= R (Tangent_Error) + R (Correction));
         Lemma_Total_Real
           (R (Tangent_Error), R (Correction), R (Up (Sum)), Correction_Square (Finish, Uncorrected, Length_Value));
         pragma Assert (Is_Total_Bound (Tangent_Error, Finish, Uncorrected, Length_Value, Up (Sum)));
      end;
   end Lemma_Total;

   ---------------------------------------------------------------------------
   --  What the tests before the product and the sum establish
   ---------------------------------------------------------------------------

   --  Both tests compare with a rounded bound, which can exceed the exact one. Half and a quarter of the largest
   --  number leave room for that.

   procedure Lemma_Halved_Product_Real (Scale, Unit_Norm, Exact : Real)
   with
     Ghost,
     Pre  =>
       1.0 <= Unit_Norm
       and then Unit_Norm <= Last_Real
       and then Exact * Unit_Norm = Half_Last_Real
       and then Scale <= Exact + Epsilon * Exact + Eta,
     Post => Scale * Unit_Norm <= Last_Real;

   procedure Lemma_Halved_Product_Real (Scale, Unit_Norm, Exact : Real) is
      Bound : constant Real := Exact + Epsilon * Exact + Eta;
   begin
      pragma Assert ((Bound - Scale) * Unit_Norm >= 0.0);
      pragma Assert (Scale * Unit_Norm <= Bound * Unit_Norm);
      pragma Assert (Bound * Unit_Norm = Exact * Unit_Norm + Epsilon * (Exact * Unit_Norm) + Eta * Unit_Norm);
      pragma Assert (Eta * Unit_Norm <= 1.0);
   end Lemma_Halved_Product_Real;

   procedure Lemma_Halved_Product_Test (Scale, Unit_Norm : Dimensionless)
   with
     Ghost,
     Pre  => Scale >= 0.0 and then Unit_Norm >= 1.0 and then Scale <= (Dimensionless'Last / 2.0) / Unit_Norm,
     Post => R (Scale) * R (Unit_Norm) <= Last_Real;
   --  A Scale within the rounded quotient of half the largest number by the root has an exact product with the
   --  root within the largest number, so the rounded product does not overflow.

   procedure Lemma_Halved_Product_Test (Scale, Unit_Norm : Dimensionless) is
      Exact : constant Real := Half_Last_Real / R (Unit_Norm);
   begin
      pragma Assert (R (Unit_Norm) >= 1.0 and then R (Unit_Norm) <= Last_Real);
      pragma Assert (Exact * R (Unit_Norm) = Half_Last_Real);
      if Exact < 0.0 then
         pragma Assert (Exact * R (Unit_Norm) < 0.0);
      end if;
      pragma Assert (Exact >= 0.0);
      pragma Assert (Half_Last_Real <= Last_Real * R (Unit_Norm));
      Lemma_Quotient_Rounds (Dimensionless'Last / 2.0, Unit_Norm);
      pragma Assert (R (Scale) <= R ((Dimensionless'Last / 2.0) / Unit_Norm));
      pragma Assert (R (Scale) <= Exact + Epsilon * Exact + Eta);
      Lemma_Halved_Product_Real (R (Scale), R (Unit_Norm), Exact);
   end Lemma_Halved_Product_Test;

   procedure Lemma_Quarter_Difference_Rounds (Value : Dimensionless)
   with
     Ghost,
     Pre  => R (Value) >= 0.0,
     Post =>
       Is_Rounding_Of (Dimensionless'Last / 4.0 - Value, Quarter_Last_Real - R (Value))
       and then R (Value) <= Last_Real;

   procedure Lemma_Quarter_Difference_Rounds (Value : Dimensionless) is null;

   procedure Lemma_Quartered_Sum_Test (Tangent_Error, Correction : Dimensionless)
   with
     Ghost,
     Pre  =>
       Tangent_Error >= 0.0
       and then R (Correction) >= 0.0
       and then Tangent_Error <= Dimensionless'Last / 4.0 - Correction,
     Post => R (Tangent_Error) + R (Correction) <= Three_Eighths_Last_Real;
   --  A Tangent_Error within the rounded difference of a quarter of the largest number and the correction bound
   --  has an exact sum with it of little more than that quarter.

   procedure Lemma_Quartered_Sum_Test (Tangent_Error, Correction : Dimensionless) is
      Exact : constant Real := Quarter_Last_Real - R (Correction);
   begin
      Lemma_Quarter_Difference_Rounds (Correction);
      pragma Assert (R (Tangent_Error) <= R (Dimensionless'Last / 4.0 - Correction));
      pragma Assert (abs Exact <= Last_Real);
      pragma Assert (R (Tangent_Error) <= Exact + Epsilon * Last_Real + Eta);
   end Lemma_Quartered_Sum_Test;

   ---------------------------------------------------------------------------
   --  The outcome
   ---------------------------------------------------------------------------

   --  The computation of the bound returns from nine places. A postcondition is one check over every return, in
   --  which the provers know the result only case by case, and with the squares of Is_Total_Bound in view they
   --  take seconds over that. So the computation is a function of its own, Bound_Or_Last, whose postcondition
   --  is Is_Outcome, completed in the body of this package where the computation does not see what it means:
   --  each return establishes it by a lemma, and Total_Tangent_Error_Bound, which returns from one place, has
   --  it revealed.

   package Outcome
     with Ghost
   is
      function Is_Outcome
        (Tangent_Error : Dimensionless;
         Finish        : Dimensionless_Axis_Vector;
         Uncorrected   : Dimensionless_Axis_Vector;
         Length_Value  : Dimensionless;
         Total         : Dimensionless) return Boolean;
      --  The postcondition of Total_Tangent_Error_Bound, of Total.

      procedure Lemma_No_Bound
        (Tangent_Error : Dimensionless;
         Finish        : Dimensionless_Axis_Vector;
         Uncorrected   : Dimensionless_Axis_Vector;
         Length_Value  : Dimensionless)
      with Post => Is_Outcome (Tangent_Error, Finish, Uncorrected, Length_Value, Dimensionless'Last);
      --  Nothing is promised of Dimensionless'Last.

      procedure Lemma_Bound
        (Tangent_Error : Dimensionless;
         Finish        : Dimensionless_Axis_Vector;
         Uncorrected   : Dimensionless_Axis_Vector;
         Length_Value  : Dimensionless;
         Total         : Dimensionless)
      with
        Pre  => Total >= 0.0 and then Is_Total_Bound (Tangent_Error, Finish, Uncorrected, Length_Value, Total),
        Post => Is_Outcome (Tangent_Error, Finish, Uncorrected, Length_Value, Total);

      procedure Lemma_Outcome
        (Tangent_Error : Dimensionless;
         Finish        : Dimensionless_Axis_Vector;
         Uncorrected   : Dimensionless_Axis_Vector;
         Length_Value  : Dimensionless;
         Total         : Dimensionless)
      with
        Pre  => Is_Outcome (Tangent_Error, Finish, Uncorrected, Length_Value, Total),
        Post =>
          (if Total < Dimensionless'Last
           then Total >= 0.0 and then Is_Total_Bound (Tangent_Error, Finish, Uncorrected, Length_Value, Total));
      --  What Is_Outcome means.

   end Outcome;

   package body Outcome is

      function Is_Outcome
        (Tangent_Error : Dimensionless;
         Finish        : Dimensionless_Axis_Vector;
         Uncorrected   : Dimensionless_Axis_Vector;
         Length_Value  : Dimensionless;
         Total         : Dimensionless) return Boolean
      is (if Total < Dimensionless'Last
          then Total >= 0.0 and then Is_Total_Bound (Tangent_Error, Finish, Uncorrected, Length_Value, Total));

      procedure Lemma_No_Bound
        (Tangent_Error : Dimensionless;
         Finish        : Dimensionless_Axis_Vector;
         Uncorrected   : Dimensionless_Axis_Vector;
         Length_Value  : Dimensionless)
      is null;

      procedure Lemma_Bound
        (Tangent_Error : Dimensionless;
         Finish        : Dimensionless_Axis_Vector;
         Uncorrected   : Dimensionless_Axis_Vector;
         Length_Value  : Dimensionless;
         Total         : Dimensionless)
      is null;

      procedure Lemma_Outcome
        (Tangent_Error : Dimensionless;
         Finish        : Dimensionless_Axis_Vector;
         Uncorrected   : Dimensionless_Axis_Vector;
         Length_Value  : Dimensionless;
         Total         : Dimensionless)
      is null;

   end Outcome;

   ---------------------------------------------------------------------------
   --  The executable subprograms
   ---------------------------------------------------------------------------

   function Correction_Interval (Finish, Uncorrected : Dimensionless) return Interval is
   begin
      if Finish = Uncorrected then
         return Interval_Exact (0.0);
      else
         return Interval_Subtract (Interval_Exact (Finish), Interval_Exact (Uncorrected));
      end if;
   end Correction_Interval;

   function Bound_Or_Last
     (Tangent_Error : Dimensionless;
      Finish        : Dimensionless_Axis_Vector;
      Uncorrected   : Dimensionless_Axis_Vector;
      Length_Value  : Dimensionless) return Dimensionless
   with
     Pre  => Tangent_Error >= 0.0,
     Post => Outcome.Is_Outcome (Tangent_Error, Finish, Uncorrected, Length_Value, Bound_Or_Last'Result);
   --  Total_Tangent_Error_Bound, with what its result means left to Outcome.

   function Bound_Or_Last
     (Tangent_Error : Dimensionless;
      Finish        : Dimensionless_Axis_Vector;
      Uncorrected   : Dimensionless_Axis_Vector;
      Length_Value  : Dimensionless) return Dimensionless
   is
      Axis_Correction_Bounds : Dimensionless_Axis_Vector := [others => 0.0];
      Scale                  : Dimensionless := 0.0;
      Sum                    : Dimensionless := 0.0;
      Correction_Bound       : Dimensionless := 0.0;
   begin
      --  The postcondition is one check over every return below, with whatever led to each of them in view.
      --  Nothing is promised of a result of Dimensionless'Last, so each such return forgets how it was reached
      --  first.
      if Length_Value <= 0.0 or else not Is_Finite (Length_Value) then
         Outcome.Lemma_No_Bound (Tangent_Error, Finish, Uncorrected, Length_Value);
         return Dimensionless'Last;
      end if;

      for Axis in Axis_Name loop
         --  What holds of the axes before this one. The assertions which end the loop body state the same of the
         --  axes up to this one.
         pragma Loop_Invariant (R (Scale) >= 0.0);
         pragma Loop_Invariant (Are_Within (Axis_Correction_Bounds, Scale));
         pragma Loop_Invariant (Rest_Is_Zero (Axis_Correction_Bounds, Axis_Name'Pos (Axis)));
         pragma Loop_Invariant (if R (Scale) > 0.0 then Is_Attained (Axis_Correction_Bounds, Scale));
         pragma
           Loop_Invariant
             (R (Scale) > Half_Last_Real
              or else
                Are_Axis_Bounds (Finish, Uncorrected, Length_Value, Axis_Correction_Bounds, Axis_Name'Pos (Axis)));

         declare
            Correction : constant Interval := Correction_Interval (Finish (Axis), Uncorrected (Axis));
            Magnitude  : Dimensionless;
         begin
            if not Correction.Valid then
               pragma Assert_And_Cut (not Correction.Valid);
               Outcome.Lemma_No_Bound (Tangent_Error, Finish, Uncorrected, Length_Value);
               return Dimensionless'Last;
            end if;
            Magnitude := Interval_Abs_Max (Correction);
            --  The quotient below would overflow.
            if Length_Value < 1.0 and then Magnitude > (Dimensionless'Last / 2.0) * Length_Value then
               pragma Assert_And_Cut (Length_Value < 1.0);
               Outcome.Lemma_No_Bound (Tangent_Error, Finish, Uncorrected, Length_Value);
               return Dimensionless'Last;
            end if;
            Lemma_Magnitude (Correction, Magnitude, R (Finish (Axis)) - R (Uncorrected (Axis)));
            Lemma_Axis_Bound (Magnitude, Length_Value, R (Finish (Axis)) - R (Uncorrected (Axis)));
            Axis_Correction_Bounds (Axis) := Up (Magnitude / Length_Value);
            if not Is_Finite (Axis_Correction_Bounds (Axis)) then
               pragma Assert_And_Cut (not Is_Finite (Axis_Correction_Bounds (Axis)));
               Outcome.Lemma_No_Bound (Tangent_Error, Finish, Uncorrected, Length_Value);
               return Dimensionless'Last;
            end if;
            Lemma_Max (Scale, Axis_Correction_Bounds (Axis));
            Scale := Dimensionless'Max (Scale, Axis_Correction_Bounds (Axis));
         end;

         pragma Assert (R (Scale) >= 0.0);
         pragma Assert (Are_Within (Axis_Correction_Bounds, Scale));
         pragma Assert (Rest_Is_Zero (Axis_Correction_Bounds, Axis_Name'Pos (Axis) + 1));
         pragma Assert (if R (Scale) > 0.0 then Is_Attained (Axis_Correction_Bounds, Scale));
         pragma
           Assert
             (R (Scale) > Half_Last_Real
              or else Is_Axis_Bound (Finish, Uncorrected, Length_Value, Axis_Correction_Bounds, Axis));
         pragma
           Assert
             (R (Scale) > Half_Last_Real
              or else
                Are_Axis_Bounds (Finish, Uncorrected, Length_Value, Axis_Correction_Bounds, Axis_Name'Pos (Axis)));
         pragma
           Assert
             (R (Scale) > Half_Last_Real
              or else
                Are_Axis_Bounds (Finish, Uncorrected, Length_Value, Axis_Correction_Bounds, Axis_Name'Pos (Axis) + 1));
      end loop;

      --  From here on only this much of the first loop matters.
      pragma
        Assert_And_Cut
          (Tangent_Error >= 0.0
           and then Length_Value > 0.0
           and then R (Scale) >= 0.0
           and then R (Sum) = 0.0
           and then R (Correction_Bound) = 0.0
           and then Are_Within (Axis_Correction_Bounds, Scale)
           and then (if R (Scale) > 0.0 then Is_Attained (Axis_Correction_Bounds, Scale))
           and then
             (R (Scale) > Half_Last_Real
              or else Are_Axis_Bounds (Finish, Uncorrected, Length_Value, Axis_Correction_Bounds, 4)));

      if Scale > 0.0 then
         for Axis in Axis_Name loop
            --  Stated of the axes before this one, as in the first loop, so that the provers see one iteration
            --  at a time.
            pragma Loop_Invariant (0.0 <= R (Sum) and then R (Sum) <= Sum_Limit (Axis_Name'Pos (Axis)));
            pragma Loop_Invariant (R (Sum) >= Ratio_Squares (Axis_Correction_Bounds, Scale, Axis_Name'Pos (Axis)));

            Lemma_Sum_Step (Axis_Correction_Bounds (Axis), Scale, Sum);
            Sum := Up (Sum + Up ((Axis_Correction_Bounds (Axis) / Scale)**2));

            pragma Assert (0.0 <= R (Sum) and then R (Sum) <= Sum_Limit (Axis_Name'Pos (Axis) + 1));
            pragma Assert (R (Sum) >= Ratio_Squares (Axis_Correction_Bounds, Scale, Axis_Name'Pos (Axis) + 1));
         end loop;

         --  And only this much of the second.
         pragma
           Assert_And_Cut
             (Scale > 0.0
              and then R (Scale) > 0.0
              and then 0.0 <= R (Sum)
              and then R (Sum) >= Ratio_Squares (Axis_Correction_Bounds, Scale, 4));

         if not Is_Finite (Sum) then
            Outcome.Lemma_No_Bound (Tangent_Error, Finish, Uncorrected, Length_Value);
            return Dimensionless'Last;
         end if;
         declare
            Unit_Norm_Bound : constant Dimensionless := Square_Roots.Certified_Upper_Square_Root (Sum);
            Norm_Bound      : Dimensionless;
         begin
            Lemma_Unit_Norm (Axis_Correction_Bounds, Scale, Sum, Unit_Norm_Bound);
            Lemma_Half_Last_Quotient (Unit_Norm_Bound);
            --  The product below would overflow. The test is against half the largest number because the
            --  quotient is rounded; a product beyond a quarter of it is turned away below in any case.
            if Unit_Norm_Bound >= Dimensionless'Last or else Scale > (Dimensionless'Last / 2.0) / Unit_Norm_Bound then
               pragma Assert_And_Cut (Scale > 0.0);
               Outcome.Lemma_No_Bound (Tangent_Error, Finish, Uncorrected, Length_Value);
               return Dimensionless'Last;
            end if;
            Lemma_Halved_Product_Test (Scale, Unit_Norm_Bound);
            Lemma_Norm_Product (Scale, Unit_Norm_Bound);
            Norm_Bound := Up (Scale * Unit_Norm_Bound);
            --  Likewise for the product below.
            if Norm_Bound > Dimensionless'Last / 4.0 then
               pragma Assert_And_Cut (Norm_Bound > Dimensionless'Last / 4.0);
               Outcome.Lemma_No_Bound (Tangent_Error, Finish, Uncorrected, Length_Value);
               return Dimensionless'Last;
            end if;
            Lemma_Rate_Product_Rounds (Norm_Bound);
            Correction_Bound := Up ((693.0 / 256.0) * Norm_Bound);
            Lemma_Correction_Bound
              (Finish,
               Uncorrected,
               Length_Value,
               Axis_Correction_Bounds,
               Scale,
               Sum,
               Unit_Norm_Bound,
               Norm_Bound,
               (693.0 / 256.0) * Norm_Bound,
               Correction_Bound);
         end;
      end if;

      --  From here on only this much of the correction bound matters. Without a positive Scale there is none.
      Lemma_No_Correction (Finish, Uncorrected, Length_Value, Axis_Correction_Bounds, Scale);
      pragma
        Assert_And_Cut
          (Tangent_Error >= 0.0
           and then Length_Value > 0.0
           and then R (Correction_Bound) >= 0.0
           and then
             Correction_Square (Finish, Uncorrected, Length_Value) <= R (Correction_Bound) * R (Correction_Bound));

      --  The sum below would overflow, or leave Half_Last, beyond which Up promises no padding. A quarter of
      --  the largest number keeps it within both, whatever the rounding of the difference.
      if not Is_Finite (Correction_Bound) or else Tangent_Error > Dimensionless'Last / 4.0 - Correction_Bound then
         Outcome.Lemma_No_Bound (Tangent_Error, Finish, Uncorrected, Length_Value);
         return Dimensionless'Last;
      else
         Lemma_Quartered_Sum_Test (Tangent_Error, Correction_Bound);
         Lemma_Total (Tangent_Error, Correction_Bound, Finish, Uncorrected, Length_Value);
         Outcome.Lemma_Bound (Tangent_Error, Finish, Uncorrected, Length_Value, Up (Tangent_Error + Correction_Bound));
         return Up (Tangent_Error + Correction_Bound);
      end if;
   end Bound_Or_Last;

   function Total_Tangent_Error_Bound
     (Tangent_Error : Dimensionless;
      Finish        : Dimensionless_Axis_Vector;
      Uncorrected   : Dimensionless_Axis_Vector;
      Length_Value  : Dimensionless) return Dimensionless
   is
      Total : constant Dimensionless := Bound_Or_Last (Tangent_Error, Finish, Uncorrected, Length_Value);
   begin
      Outcome.Lemma_Outcome (Tangent_Error, Finish, Uncorrected, Length_Value, Total);
      return Total;
   end Total_Tangent_Error_Bound;

end Prunt.Motion_Planner.Stereographic_Kernel.Finish_Correction;
