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

package body Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials
  with SPARK_Mode => On
is
   use Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;

   use all type Prunt.Motion_Planner.Reals.Real;
   --  For To_Real, by which a count of terms becomes a real number.

   Zero : constant Interval := (Lower => 0.0, Upper => 0.0, Valid => True);

   function Invalid_Polynomial (Degree : Degree_Range) return Polynomial
   with
     Refined_Post =>
       Invalid_Polynomial'Result.Degree = Degree
       and then (for all K in 0 .. Degree => not Invalid_Polynomial'Result.Controls (K).Valid)
       and then not Invalid_Polynomial'Result.Controls (0).Valid
   is
      --  The refined postcondition names the first control, which gives the provers the instance they need to see
      --  that the result is not All_Valid.
      Result : Polynomial := (Degree => Degree, Controls => <>);
   begin
      for K in 0 .. Degree loop
         Result.Controls (K) := Invalid_Interval;
         pragma Loop_Invariant (for all J in 0 .. K => not Result.Controls (J).Valid);
      end loop;
      return Result;
   end Invalid_Polynomial;

   ---------------------------------------------------------------------------
   --  Real arithmetic
   ---------------------------------------------------------------------------

   procedure Lemma_Real_Product_Bound (X, Y, X_Limit, Y_Limit : Real)
   with Ghost, Pre => abs X <= X_Limit and then abs Y <= Y_Limit, Post => abs (X * Y) <= X_Limit * Y_Limit;
   --  A product is no larger than the product of bounds on its factors.

   procedure Lemma_Real_Product_Bound (X, Y, X_Limit, Y_Limit : Real) is
   begin
      pragma Assert (abs X * abs Y <= X_Limit * abs Y);
      pragma Assert (X_Limit * abs Y <= X_Limit * Y_Limit);
   end Lemma_Real_Product_Bound;

   procedure Lemma_Ratio_Bound (Numerator, Denominator : Real)
   with
     Ghost,
     Pre  => Numerator >= 0.0 and then Denominator >= 1.0,
     Post => Numerator / Denominator >= 0.0 and then Numerator / Denominator <= Numerator;
   --  Dividing a nonnegative number by one which is at least one cannot increase it.

   procedure Lemma_Ratio_Bound (Numerator, Denominator : Real) is
      Quotient : constant Real := Numerator / Denominator;
   begin
      pragma Assert (Quotient * Denominator = Numerator);
      if Quotient < 0.0 then
         pragma Assert (Quotient * Denominator < 0.0);
      end if;
      pragma Assert (Quotient * 1.0 <= Quotient * Denominator);
   end Lemma_Ratio_Bound;

   procedure Lemma_Times_Reciprocal (X, Divisor : Real)
   with
     Ghost,
     Pre  => Divisor > 0.0,
     Post => (if X <= 1.0 / Divisor then X * Divisor <= 1.0) and then (if X >= 1.0 / Divisor then X * Divisor >= 1.0);
   --  A bound on a reciprocal, multiplied through.

   procedure Lemma_Times_Reciprocal (X, Divisor : Real) is
      Reciprocal : constant Real := 1.0 / Divisor;
   begin
      pragma Assert (Reciprocal * Divisor = 1.0);
      if X <= Reciprocal then
         pragma Assert (X * Divisor <= Reciprocal * Divisor);
      end if;
      if X >= Reciprocal then
         pragma Assert (X * Divisor >= Reciprocal * Divisor);
      end if;
   end Lemma_Times_Reciprocal;

   Smallest_Reciprocal : constant := 1.0 / 12_870.0;
   --  The reciprocal of Largest_Binomial.

   procedure Lemma_Reciprocal_Bounds (Divisor : Real)
   with
     Ghost,
     Pre  => Divisor >= 2.0 and then Divisor <= 12_870.0,
     Post => 1.0 / Divisor >= Smallest_Reciprocal and then 1.0 / Divisor <= 0.5;

   procedure Lemma_Reciprocal_Bounds (Divisor : Real) is
      Reciprocal : constant Real := 1.0 / Divisor;
   begin
      pragma Assert (Reciprocal * Divisor = 1.0);
      if Reciprocal > 0.5 then
         pragma Assert (Reciprocal * Divisor > 0.5 * Divisor);
      end if;
      if Reciprocal < Smallest_Reciprocal then
         pragma Assert (Reciprocal * Divisor <= Smallest_Reciprocal * Divisor);
         pragma Assert (Smallest_Reciprocal * Divisor <= 1.0);
      end if;
   end Lemma_Reciprocal_Bounds;

   procedure Lemma_Quotient_Is_Between (Low, High, Value, Divisor : Real)
   with
     Ghost,
     Pre  => Divisor > 0.0 and then Low * Divisor <= Value and then Value <= High * Divisor,
     Post => Low <= Value / Divisor and then Value / Divisor <= High;
   --  Bounds which hold multiplied through by a positive divisor hold of the quotient.

   procedure Lemma_Quotient_Is_Between (Low, High, Value, Divisor : Real) is
      Quotient : constant Real := Value / Divisor;
   begin
      pragma Assert (Quotient * Divisor = Value);
      if Low > Quotient then
         pragma Assert (Low * Divisor > Quotient * Divisor);
      end if;
      if Quotient > High then
         pragma Assert (Quotient * Divisor > High * Divisor);
      end if;
   end Lemma_Quotient_Is_Between;

   procedure Lemma_Scaling_Keeps_Order (Factor, Low, Middle, High : Real)
   with
     Ghost,
     Pre  => Factor >= 0.0 and then Low <= Middle and then Middle <= High,
     Post => Factor * Low <= Factor * Middle and then Factor * Middle <= Factor * High;

   procedure Lemma_Scaling_Keeps_Order (Factor, Low, Middle, High : Real) is null;

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
   --  More than the distance which the outward padding moves Value. Intervals.Slack is less than that distance.

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

   procedure Lemma_Padded_Below (Rounded, Padded : Dimensionless; Exact : Real)
   with
     Ghost,
     Pre  =>
       abs Rounded <= Half_Last
       and then Is_Rounding_Of (Rounded, Exact)
       and then Padded = Inflate_Finite (Rounded, False),
     Post => R (Padded) <= Exact;
   --  A rounded result padded downward lies below the exact one. Padded is a parameter, rather than the padding
   --  being written out by the caller, so that the step from equal floating-point numbers to equal real numbers
   --  is taken here, where there is little else for the provers to consider.

   procedure Lemma_Padded_Below (Rounded, Padded : Dimensionless; Exact : Real) is
   begin
      Lemma_Inflate_Encloses (Rounded, Exact);
      Lemma_Same_Real (Padded, Inflate_Finite (Rounded, False));
   end Lemma_Padded_Below;

   procedure Lemma_Padded_Above (Rounded, Padded : Dimensionless; Exact : Real)
   with
     Ghost,
     Pre  =>
       abs Rounded <= Half_Last
       and then Is_Rounding_Of (Rounded, Exact)
       and then Padded = Inflate_Finite (Rounded, True),
     Post => Exact <= R (Padded);

   procedure Lemma_Padded_Above (Rounded, Padded : Dimensionless; Exact : Real) is
   begin
      Lemma_Inflate_Encloses (Rounded, Exact);
      Lemma_Same_Real (Padded, Inflate_Finite (Rounded, True));
   end Lemma_Padded_Above;

   ---------------------------------------------------------------------------
   --  Binomial coefficients
   ---------------------------------------------------------------------------

   function Has_Table_Shape (N, K : Degree_Range) return Boolean
   is (if K > N
       then Binomials (N, K) = 0.0
       elsif K = 0 or else K = N
       then Binomials (N, K) = 1.0
       else Binomials (N, K) >= 2.0 and then Binomials (N, K) <= Largest_Binomial)
   with Ghost;
   --  Zero above the diagonal, one at either end of a row, and from two to Largest_Binomial in between.

   procedure Lemma_Table_Shape (N, K : Degree_Range)
   with
     Ghost,
     Post =>
       Has_Table_Shape (N, K)
       and then (if K <= N then Binomials (N, K) >= 1.0 and then Binomials (N, K) <= Largest_Binomial);

   procedure Lemma_Table_Shape (N, K : Degree_Range) is null;

   procedure Lemma_Entry_Is_Exact (N, K : Degree_Range; Above_Left, Above, Here : Real)
   with
     Ghost,
     Pre  =>
       (if K > N
        then Here = 0.0
        elsif K = 0 or else K = N
        then Here = 1.0
        else Above_Left = Choose (N - 1, K - 1) and then Above = Choose (N - 1, K) and then Here = Above_Left + Above),
     Post => Here = Choose (N, K);
   --  Pascal's rule, by which Choose is defined: a number which follows the rule from the two binomial
   --  coefficients above it is the binomial coefficient. This lemma does not mention the table, whose literal
   --  aggregate is more than the provers can carry through anything but the simplest step.

   procedure Lemma_Entry_Is_Exact (N, K : Degree_Range; Above_Left, Above, Here : Real) is
   begin
      if K > 0 and then K < N then
         pragma Assert (Choose (N, K) = Choose (N - 1, K - 1) + Choose (N - 1, K));
      end if;
   end Lemma_Entry_Is_Exact;

   function Entry_Above_Left (N, K : Degree_Range) return Real
   is (if K > 0 and then K < N then R (Binomials (N - 1, K - 1)) else 0.0)
   with Ghost;
   --  The table's entry above and to the left of an entry inside a row.

   function Entry_Above (N, K : Degree_Range) return Real
   is (if K > 0 and then K < N then R (Binomials (N - 1, K)) else 0.0)
   with Ghost;

   --  That a row of the table is exact is stated below entry by entry, and without a function to name it: the
   --  provers do not survive a quantified statement about the table, and are slow to unfold a named one.

   generic
      Row : Degree_Range;
   procedure Prove_Row_Is_Exact
   with
     Ghost,
     Pre  =>
       Row = 0
       or else
         (R (Binomials (Row - 1, 0)) = Choose (Row - 1, 0)
          and then R (Binomials (Row - 1, 1)) = Choose (Row - 1, 1)
          and then R (Binomials (Row - 1, 2)) = Choose (Row - 1, 2)
          and then R (Binomials (Row - 1, 3)) = Choose (Row - 1, 3)
          and then R (Binomials (Row - 1, 4)) = Choose (Row - 1, 4)
          and then R (Binomials (Row - 1, 5)) = Choose (Row - 1, 5)
          and then R (Binomials (Row - 1, 6)) = Choose (Row - 1, 6)
          and then R (Binomials (Row - 1, 7)) = Choose (Row - 1, 7)
          and then R (Binomials (Row - 1, 8)) = Choose (Row - 1, 8)
          and then R (Binomials (Row - 1, 9)) = Choose (Row - 1, 9)
          and then R (Binomials (Row - 1, 10)) = Choose (Row - 1, 10)
          and then R (Binomials (Row - 1, 11)) = Choose (Row - 1, 11)
          and then R (Binomials (Row - 1, 12)) = Choose (Row - 1, 12)
          and then R (Binomials (Row - 1, 13)) = Choose (Row - 1, 13)
          and then R (Binomials (Row - 1, 14)) = Choose (Row - 1, 14)
          and then R (Binomials (Row - 1, 15)) = Choose (Row - 1, 15)
          and then R (Binomials (Row - 1, 16)) = Choose (Row - 1, 16)),
     Post =>
       R (Binomials (Row, 0)) = Choose (Row, 0)
       and then R (Binomials (Row, 1)) = Choose (Row, 1)
       and then R (Binomials (Row, 2)) = Choose (Row, 2)
       and then R (Binomials (Row, 3)) = Choose (Row, 3)
       and then R (Binomials (Row, 4)) = Choose (Row, 4)
       and then R (Binomials (Row, 5)) = Choose (Row, 5)
       and then R (Binomials (Row, 6)) = Choose (Row, 6)
       and then R (Binomials (Row, 7)) = Choose (Row, 7)
       and then R (Binomials (Row, 8)) = Choose (Row, 8)
       and then R (Binomials (Row, 9)) = Choose (Row, 9)
       and then R (Binomials (Row, 10)) = Choose (Row, 10)
       and then R (Binomials (Row, 11)) = Choose (Row, 11)
       and then R (Binomials (Row, 12)) = Choose (Row, 12)
       and then R (Binomials (Row, 13)) = Choose (Row, 13)
       and then R (Binomials (Row, 14)) = Choose (Row, 14)
       and then R (Binomials (Row, 15)) = Choose (Row, 15)
       and then R (Binomials (Row, 16)) = Choose (Row, 16);
   --  One row of the table is exact when the row above it is. This is generic in the row, and instantiated for each
   --  one, because the provers can evaluate the table's literals only where both indices are literals as well.

   procedure Prove_Row_Is_Exact is
   begin
      --  This loop has static bounds and no invariant, so that the tool unrolls it and gives the provers each entry
      --  of the row as a separate goal: the entry follows Pascal's rule from the two above it.
      for Column in Degree_Range loop
         Lemma_Entry_Is_Exact
           (Row, Column, Entry_Above_Left (Row, Column), Entry_Above (Row, Column), R (Binomials (Row, Column)));
      end loop;
   end Prove_Row_Is_Exact;

   procedure Prove_Row_0 is new Prove_Row_Is_Exact (0);
   procedure Prove_Row_1 is new Prove_Row_Is_Exact (1);
   procedure Prove_Row_2 is new Prove_Row_Is_Exact (2);
   procedure Prove_Row_3 is new Prove_Row_Is_Exact (3);
   procedure Prove_Row_4 is new Prove_Row_Is_Exact (4);
   procedure Prove_Row_5 is new Prove_Row_Is_Exact (5);
   procedure Prove_Row_6 is new Prove_Row_Is_Exact (6);
   procedure Prove_Row_7 is new Prove_Row_Is_Exact (7);
   procedure Prove_Row_8 is new Prove_Row_Is_Exact (8);
   procedure Prove_Row_9 is new Prove_Row_Is_Exact (9);
   procedure Prove_Row_10 is new Prove_Row_Is_Exact (10);
   procedure Prove_Row_11 is new Prove_Row_Is_Exact (11);
   procedure Prove_Row_12 is new Prove_Row_Is_Exact (12);
   procedure Prove_Row_13 is new Prove_Row_Is_Exact (13);
   procedure Prove_Row_14 is new Prove_Row_Is_Exact (14);
   procedure Prove_Row_15 is new Prove_Row_Is_Exact (15);
   procedure Prove_Row_16 is new Prove_Row_Is_Exact (16);

   procedure Lemma_Row_Is_Exact (N : Degree_Range)
   with
     Ghost,
     Subprogram_Variant => (Decreases => N),
     Post               =>
       R (Binomials (N, 0)) = Choose (N, 0)
       and then R (Binomials (N, 1)) = Choose (N, 1)
       and then R (Binomials (N, 2)) = Choose (N, 2)
       and then R (Binomials (N, 3)) = Choose (N, 3)
       and then R (Binomials (N, 4)) = Choose (N, 4)
       and then R (Binomials (N, 5)) = Choose (N, 5)
       and then R (Binomials (N, 6)) = Choose (N, 6)
       and then R (Binomials (N, 7)) = Choose (N, 7)
       and then R (Binomials (N, 8)) = Choose (N, 8)
       and then R (Binomials (N, 9)) = Choose (N, 9)
       and then R (Binomials (N, 10)) = Choose (N, 10)
       and then R (Binomials (N, 11)) = Choose (N, 11)
       and then R (Binomials (N, 12)) = Choose (N, 12)
       and then R (Binomials (N, 13)) = Choose (N, 13)
       and then R (Binomials (N, 14)) = Choose (N, 14)
       and then R (Binomials (N, 15)) = Choose (N, 15)
       and then R (Binomials (N, 16)) = Choose (N, 16);
   --  Row N of the table holds the binomial coefficients, by induction on N.

   procedure Lemma_Row_Is_Exact (N : Degree_Range) is
   begin
      --  The row above is named by its literal in each alternative, to match the instance which uses it.
      case N is
         when 0  =>
            Prove_Row_0;

         when 1  =>
            Lemma_Row_Is_Exact (0);
            Prove_Row_1;

         when 2  =>
            Lemma_Row_Is_Exact (1);
            Prove_Row_2;

         when 3  =>
            Lemma_Row_Is_Exact (2);
            Prove_Row_3;

         when 4  =>
            Lemma_Row_Is_Exact (3);
            Prove_Row_4;

         when 5  =>
            Lemma_Row_Is_Exact (4);
            Prove_Row_5;

         when 6  =>
            Lemma_Row_Is_Exact (5);
            Prove_Row_6;

         when 7  =>
            Lemma_Row_Is_Exact (6);
            Prove_Row_7;

         when 8  =>
            Lemma_Row_Is_Exact (7);
            Prove_Row_8;

         when 9  =>
            Lemma_Row_Is_Exact (8);
            Prove_Row_9;

         when 10 =>
            Lemma_Row_Is_Exact (9);
            Prove_Row_10;

         when 11 =>
            Lemma_Row_Is_Exact (10);
            Prove_Row_11;

         when 12 =>
            Lemma_Row_Is_Exact (11);
            Prove_Row_12;

         when 13 =>
            Lemma_Row_Is_Exact (12);
            Prove_Row_13;

         when 14 =>
            Lemma_Row_Is_Exact (13);
            Prove_Row_14;

         when 15 =>
            Lemma_Row_Is_Exact (14);
            Prove_Row_15;

         when 16 =>
            Lemma_Row_Is_Exact (15);
            Prove_Row_16;
      end case;
   end Lemma_Row_Is_Exact;

   procedure Lemma_Table_Is_Exact (N, K : Degree_Range)
   with Ghost, Post => R (Binomials (N, K)) = Choose (N, K);
   --  One entry of an exact row.

   procedure Lemma_Table_Is_Exact (N, K : Degree_Range) is
   begin
      Lemma_Row_Is_Exact (N);
      --  Unrolled by the tool. Each pass gives the provers the column as a literal, with which the assertion is
      --  one of the entries just established; left to find the entry themselves, they come within a second of the
      --  time limit.
      for Column in Degree_Range loop
         if Column = K then
            pragma Assert (R (Binomials (N, K)) = Choose (N, K));
            return;
         end if;
      end loop;
   end Lemma_Table_Is_Exact;

   procedure Lemma_Binomials_Are_Exact (N, K : Degree_Range) is
   begin
      Lemma_Table_Is_Exact (N, K);
      Lemma_Table_Shape (N, K);
   end Lemma_Binomials_Are_Exact;

   function Binomial_Weight (N, K : Degree_Range) return Dimensionless
   with
     Inline,
     Pre  => K <= N,
     Post =>
       Binomial_Weight'Result >= 1.0
       and then Binomial_Weight'Result <= Largest_Binomial
       and then (Binomial_Weight'Result = 1.0 or else Binomial_Weight'Result >= 2.0)
       and then R (Binomial_Weight'Result) = Choose (N, K);
   --  Binomials (N, K), read through a contract. A direct read brings the table's literal aggregate into every
   --  proof of the reading subprogram, which the provers do not survive in the presence of loop invariants.

   function Binomial_Weight (N, K : Degree_Range) return Dimensionless is
   begin
      Lemma_Binomials_Are_Exact (N, K);
      Lemma_Table_Shape (N, K);
      return Binomials (N, K);
   end Binomial_Weight;

   function Is_Reciprocal_Enclosure (Factor : Positive_Endpoints; N, K : Degree_Range) return Boolean
   with
     Ghost,
     Post =>
       Is_Reciprocal_Enclosure'Result
       = (Factor.Lower > 0.0
          and then Factor.Lower <= Factor.Upper
          and then Factor.Upper <= 1.0
          and then R (Factor.Lower) * Choose (N, K) <= 1.0
          and then 1.0 <= R (Factor.Upper) * Choose (N, K));
   --  Whether Factor is a positive enclosure, within one, of the reciprocal of "N choose K". The enclosure is
   --  stated multiplied through by the binomial coefficient.
   --
   --  This and the other predicates below which are given by a postcondition, rather than as expression functions,
   --  are so written to have the tool keep each as one term in loop invariants and postconditions instead of
   --  expanding it: the provers match the term at once, and are slow with its expansion.

   function Is_Reciprocal_Enclosure (Factor : Positive_Endpoints; N, K : Degree_Range) return Boolean is
   begin
      return
        Factor.Lower > 0.0
        and then Factor.Lower <= Factor.Upper
        and then Factor.Upper <= 1.0
        and then R (Factor.Lower) * Choose (N, K) <= 1.0
        and then 1.0 <= R (Factor.Upper) * Choose (N, K);
   end Is_Reciprocal_Enclosure;

   function Is_Unit (Factor : Positive_Endpoints) return Boolean
   is (Factor.Lower = 1.0 and then Factor.Upper = 1.0)
   with Ghost;

   procedure Lemma_Quotient_Is_Enclosed (Value : Dimensionless)
   with
     Ghost,
     Pre  => Value >= 2.0 and then Value <= Largest_Binomial,
     Post =>
       abs (1.0 / Value) <= 1.0
       and then Down (1.0 / Value) > 0.0
       and then Down (1.0 / Value) <= Up (1.0 / Value)
       and then Up (1.0 / Value) <= 1.0
       and then R (Down (1.0 / Value)) * R (Value) <= 1.0
       and then 1.0 <= R (Up (1.0 / Value)) * R (Value);
   --  The padded quotient of one by a binomial coefficient encloses the reciprocal, and stays within (0, 1].

   procedure Lemma_Quotient_Is_Enclosed (Value : Dimensionless) is
      Quotient : constant Dimensionless := 1.0 / Value;
      Exact    : constant Real := 1.0 / R (Value);
   begin
      Lemma_Reciprocal_Bounds (R (Value));
      Float_Lemmas.Lemma_Rounding_Error_Div (1.0, Value);
      pragma Assert (Is_Rounding_Of (Quotient, Exact));
      pragma Assert (R (Quotient) <= 0.5 + Epsilon * 0.5 + Eta);
      pragma Assert (R (Quotient) >= Smallest_Reciprocal - Epsilon * Smallest_Reciprocal - Eta);
      pragma Assert (abs Quotient <= 1.0);
      Lemma_Inflate_Encloses (Quotient, Exact);
      Lemma_Inflate_Reach (Quotient);
      pragma Assert (Down (Quotient) = Inflate_Finite (Quotient, False));
      pragma Assert (Up (Quotient) = Inflate_Finite (Quotient, True));
      pragma Assert (R (Down (Quotient)) > 0.0);
      pragma Assert (R (Up (Quotient)) <= 1.0);
      Lemma_Times_Reciprocal (R (Down (Quotient)), R (Value));
      Lemma_Times_Reciprocal (R (Up (Quotient)), R (Value));
   end Lemma_Quotient_Is_Enclosed;

   procedure Lemma_Reciprocal_Entry (N, K : Degree_Range; Value : Dimensionless; Stored : Positive_Endpoints)
   with
     Ghost,
     Pre  =>
       Value >= 1.0
       and then Value <= Largest_Binomial
       and then (Value = 1.0 or else Value >= 2.0)
       and then R (Value) = Choose (N, K)
       and then Is_Unit (Stored),
     Post =>
       (if Value > 1.0
        then
          abs (1.0 / Value) <= 1.0
          and then Is_Reciprocal_Enclosure ((Lower => Down (1.0 / Value), Upper => Up (1.0 / Value)), N, K)
        else Is_Reciprocal_Enclosure (Stored, N, K));
   --  What Build_Reciprocals stores for one binomial coefficient encloses its reciprocal, and so does the initial
   --  entry which it leaves in place for a binomial coefficient of one.

   procedure Lemma_Reciprocal_Entry (N, K : Degree_Range; Value : Dimensionless; Stored : Positive_Endpoints) is
   begin
      if Value > 1.0 then
         Lemma_Quotient_Is_Enclosed (Value);
      else
         pragma Assert (R (Value) = 1.0);
         pragma Assert (R (Stored.Lower) = 1.0 and then R (Stored.Upper) = 1.0);
         pragma Assert (R (Stored.Lower) * Choose (N, K) = 1.0);
         pragma Assert (R (Stored.Upper) * Choose (N, K) = 1.0);
      end if;
   end Lemma_Reciprocal_Entry;

   function Build_Reciprocals return Reciprocal_Table
   with
     Refined_Post =>
       (for all N in Degree_Range =>
          (for all K in 0 .. N => Is_Reciprocal_Enclosure (Build_Reciprocals'Result (N, K), N, K)))
   is
      Result : Reciprocal_Table := [others => [others => (Lower => 1.0, Upper => 1.0)]];
      --  The entries above the diagonal are never read.
   begin
      for N in Degree_Range loop
         for K in 0 .. N loop
            declare
               Value : constant Dimensionless := Binomial_Weight (N, K);
            begin
               Lemma_Reciprocal_Entry (N, K, Value, Result (N, K));
               if Value > 1.0 and then Value <= Largest_Binomial then
                  Result (N, K) := (Lower => Down (1.0 / Value), Upper => Up (1.0 / Value));
               end if;
            end;
            pragma
              Loop_Invariant
                (for all M in N + 1 .. Maximum_Degree => (for all J in Degree_Range => Is_Unit (Result (M, J))));
            pragma Loop_Invariant (for all J in K + 1 .. Maximum_Degree => Is_Unit (Result (N, J)));
            pragma
              Loop_Invariant
                (for all M in 0 .. N - 1 => (for all J in 0 .. M => Is_Reciprocal_Enclosure (Result (M, J), M, J)));
            pragma Loop_Invariant (for all J in 0 .. K => Is_Reciprocal_Enclosure (Result (N, J), N, J));
         end loop;
         pragma
           Loop_Invariant
             (for all M in N + 1 .. Maximum_Degree => (for all J in Degree_Range => Is_Unit (Result (M, J))));
         pragma
           Loop_Invariant
             (for all M in 0 .. N => (for all J in 0 .. M => Is_Reciprocal_Enclosure (Result (M, J), M, J)));
      end loop;
      return Result;
   end Build_Reciprocals;

   Reciprocals : constant Reciprocal_Table := Build_Reciprocals;
   --  These positive enclosures depend only on degree and index, so they are built once.

   ---------------------------------------------------------------------------
   --  Arithmetic
   ---------------------------------------------------------------------------

   function Positive_Ratio (Numerator, Denominator : Dimensionless) return Interval is
      Value : Dimensionless;
   begin
      if not (Numerator >= 0.0)
        or else not (Numerator <= Product_Operand_Limit)
        or else not (Denominator >= 1.0)
        or else not (Denominator <= Product_Operand_Limit)
      then
         return Invalid_Interval;
      end if;
      --  The quotient is one rounded operation on a ratio which is no larger than its numerator.
      Lemma_Ratio_Bound (R (Numerator), R (Denominator));
      Float_Lemmas.Lemma_Rounding_Error_Div (Numerator, Denominator);
      Value := Numerator / Denominator;
      pragma Assert (Is_Rounding_Of (Value, R (Numerator) / R (Denominator)));
      pragma Assert (abs Value <= Half_Last);
      Lemma_Inflate_Encloses (Value, R (Numerator) / R (Denominator));
      return Checked_Interval (Down (Value), Up (Value));
   end Positive_Ratio;

   ---------------------------------------------------------------------------
   --  Magnitudes within a product
   ---------------------------------------------------------------------------

   Control_Bound : constant := 2.0**500;
   --  Scaled_Control_Limit, as a real number.

   Product_Bound : constant := 2.0**1001;
   Product_Limit : constant Dimensionless := Product_Bound
   with Ghost;
   --  No rounded product of two scaled controls exceeds this.

   Padded_Bound : constant := 2.0**1002;
   Padded_Limit : constant Dimensionless := Padded_Bound
   with Ghost;
   --  Nor does such a product exceed this once it is padded.

   Term_Growth : constant := 2.0**1003;
   --  Adding one padded product, and padding the sum, moves a partial sum by no more than this.

   Sum_Bound : constant := 2.0**1008;
   Sum_Limit : constant Dimensionless := Sum_Bound
   with Ghost;
   --  More than seventeen times Term_Growth, so that no partial sum of a control exceeds it, yet far below
   --  Half_Last.

   function Terms (Count : Natural) return Real
   is (To_Real (Count))
   with Ghost;
   --  A number of terms, as a real number.

   procedure Lemma_Sum_Is_Within_Limit (Value : Dimensionless; Count : Natural)
   with
     Ghost,
     Pre  => Count <= Maximum_Degree + 1 and then abs (R (Value)) <= Terms (Count) * Term_Growth,
     Post => abs Value <= Sum_Limit;
   --  At most seventeen terms contribute to a control.

   procedure Lemma_Sum_Is_Within_Limit (Value : Dimensionless; Count : Natural) is
   begin
      pragma Assert (Terms (Count) <= 17.0);
      pragma Assert (abs (R (Value)) <= 17.0 * Term_Growth);
      pragma Assert (abs (R (Value)) <= Sum_Bound);
   end Lemma_Sum_Is_Within_Limit;

   ---------------------------------------------------------------------------
   --  Corner products
   ---------------------------------------------------------------------------

   function Least (X, Y : Real) return Real
   is (if X <= Y then X else Y)
   with Ghost;

   function Greatest (X, Y : Real) return Real
   is (if X >= Y then X else Y)
   with Ghost;

   function Least_Corner (A, B, C, D : Real) return Real
   is (Least (Least (A * C, A * D), Least (B * C, B * D)))
   with Ghost;
   --  The least product of one of A and B with one of C and D. When A <= B and C <= D it is the least product of a
   --  member of [A, B] with a member of [C, D].

   function Greatest_Corner (A, B, C, D : Real) return Real
   is (Greatest (Greatest (A * C, A * D), Greatest (B * C, B * D)))
   with Ghost;

   function Low_Product (Left, Right : Interval) return Real
   is (Least_Corner (R (Left.Lower), R (Left.Upper), R (Right.Lower), R (Right.Upper)))
   with Ghost;
   --  The least product of an endpoint of Left with an endpoint of Right.

   function High_Product (Left, Right : Interval) return Real
   is (Greatest_Corner (R (Left.Lower), R (Left.Upper), R (Right.Lower), R (Right.Upper)))
   with Ghost;
   --  The greatest such product.

   procedure Lemma_Zero_Corners (A, B, C, D : Real)
   with
     Ghost,
     Pre  => (A = 0.0 and then B = 0.0) or else (C = 0.0 and then D = 0.0),
     Post => Least_Corner (A, B, C, D) = 0.0 and then Greatest_Corner (A, B, C, D) = 0.0;

   procedure Lemma_Zero_Corners (A, B, C, D : Real) is
   begin
      pragma Assert (A * C = 0.0 and then A * D = 0.0 and then B * C = 0.0 and then B * D = 0.0);
   end Lemma_Zero_Corners;

   procedure Lemma_Nested_Corners (Outer_Left, Outer_Right : Interval; A, B, C, D : Real)
   with
     Ghost,
     Pre  =>
       R (Outer_Left.Lower) <= A
       and then A <= R (Outer_Left.Upper)
       and then R (Outer_Left.Lower) <= B
       and then B <= R (Outer_Left.Upper)
       and then R (Outer_Right.Lower) <= C
       and then C <= R (Outer_Right.Upper)
       and then R (Outer_Right.Lower) <= D
       and then D <= R (Outer_Right.Upper),
     Post =>
       Low_Product (Outer_Left, Outer_Right) <= Least_Corner (A, B, C, D)
       and then Greatest_Corner (A, B, C, D) <= High_Product (Outer_Left, Outer_Right);
   --  Corner products of points within two intervals lie between the extreme corner products of the intervals.

   procedure Lemma_Nested_Corners (Outer_Left, Outer_Right : Interval; A, B, C, D : Real) is
      Low  : constant Real := Low_Product (Outer_Left, Outer_Right);
      High : constant Real := High_Product (Outer_Left, Outer_Right);

      procedure Prove_Corner (X, Y : Real)
      with
        Pre  =>
          R (Outer_Left.Lower) <= X
          and then X <= R (Outer_Left.Upper)
          and then R (Outer_Right.Lower) <= Y
          and then Y <= R (Outer_Right.Upper)
          and then Low = Low_Product (Outer_Left, Outer_Right)
          and then High = High_Product (Outer_Left, Outer_Right),
        Post => Low <= X * Y and then X * Y <= High;

      procedure Prove_Corner (X, Y : Real) is
      begin
         Lemma_Product_Between_Corners
           (X    => X,
            A    => R (Outer_Left.Lower),
            B    => R (Outer_Left.Upper),
            Y    => Y,
            C    => R (Outer_Right.Lower),
            D    => R (Outer_Right.Upper),
            Low  => Low,
            High => High);
      end Prove_Corner;
   begin
      Prove_Corner (A, C);
      Prove_Corner (A, D);
      Prove_Corner (B, C);
      Prove_Corner (B, D);
   end Lemma_Nested_Corners;

   procedure Lemma_Corner_Extremes (A, B, C, D : Real)
   with
     Ghost,
     Pre  => A <= B and then C <= D,
     Post =>
       (if A >= 0.0 and then C >= 0.0
        then Least_Corner (A, B, C, D) = A * C and then Greatest_Corner (A, B, C, D) = B * D)
       and then
         (if A >= 0.0 and then D <= 0.0
          then Least_Corner (A, B, C, D) = B * C and then Greatest_Corner (A, B, C, D) = A * D)
       and then
         (if A >= 0.0 and then C < 0.0 and then D > 0.0
          then Least_Corner (A, B, C, D) = B * C and then Greatest_Corner (A, B, C, D) = B * D)
       and then
         (if B <= 0.0 and then C >= 0.0
          then Least_Corner (A, B, C, D) = A * D and then Greatest_Corner (A, B, C, D) = B * C)
       and then
         (if B <= 0.0 and then D <= 0.0
          then Least_Corner (A, B, C, D) = B * D and then Greatest_Corner (A, B, C, D) = A * C)
       and then
         (if B <= 0.0 and then C < 0.0 and then D > 0.0
          then Least_Corner (A, B, C, D) = A * D and then Greatest_Corner (A, B, C, D) = A * C)
       and then
         (if A < 0.0 and then B > 0.0 and then C >= 0.0
          then Least_Corner (A, B, C, D) = A * D and then Greatest_Corner (A, B, C, D) = B * D)
       and then
         (if A < 0.0 and then B > 0.0 and then D <= 0.0
          then Least_Corner (A, B, C, D) = B * C and then Greatest_Corner (A, B, C, D) = A * C)
       and then
         (if A < 0.0 and then B > 0.0 and then C < 0.0 and then D > 0.0
          then
            Least_Corner (A, B, C, D) = Least (A * D, B * C)
            and then Greatest_Corner (A, B, C, D) = Greatest (A * C, B * D));
   --  Which corner products of [A, B] and [C, D] are extreme, by the signs of the endpoints. These are the cases
   --  which Interval_Product_Extrema distinguishes.

   procedure Lemma_Corner_Extremes (A, B, C, D : Real) is

      procedure Prove_Ordered (Low, High, Factor : Real)
      with
        Pre  => Low <= High,
        Post => (if Factor >= 0.0 then Low * Factor <= High * Factor else High * Factor <= Low * Factor);
      --  Multiplying by a factor keeps or reverses an order according to the factor's sign.

      procedure Prove_Ordered (Low, High, Factor : Real) is null;

      AC : constant Real := A * C;
      AD : constant Real := A * D;
      BC : constant Real := B * C;
      BD : constant Real := B * D;
   begin
      Prove_Ordered (A, B, C);
      Prove_Ordered (A, B, D);
      Prove_Ordered (C, D, A);
      Prove_Ordered (C, D, B);
      pragma Assert (if C >= 0.0 then AC <= BC else BC <= AC);
      pragma Assert (if D >= 0.0 then AD <= BD else BD <= AD);
      pragma Assert (if A >= 0.0 then AC <= AD else AD <= AC);
      pragma Assert (if B >= 0.0 then BC <= BD else BD <= BC);
      pragma Assert (Least_Corner (A, B, C, D) = Least (Least (AC, AD), Least (BC, BD)));
      pragma Assert (Greatest_Corner (A, B, C, D) = Greatest (Greatest (AC, AD), Greatest (BC, BD)));
   end Lemma_Corner_Extremes;

   procedure Lemma_Corners_Are (A, B, C, D, Low, High : Real)
   with
     Ghost,
     Pre  =>
       A <= B
       and then C <= D
       and then
         ((A >= 0.0 and then C >= 0.0 and then Low = A * C and then High = B * D)
          or else (A >= 0.0 and then D <= 0.0 and then Low = B * C and then High = A * D)
          or else (A >= 0.0 and then C < 0.0 and then D > 0.0 and then Low = B * C and then High = B * D)
          or else (B <= 0.0 and then C >= 0.0 and then Low = A * D and then High = B * C)
          or else (B <= 0.0 and then D <= 0.0 and then Low = B * D and then High = A * C)
          or else (B <= 0.0 and then C < 0.0 and then D > 0.0 and then Low = A * D and then High = A * C)
          or else (A < 0.0 and then B > 0.0 and then C >= 0.0 and then Low = A * D and then High = B * D)
          or else (A < 0.0 and then B > 0.0 and then D <= 0.0 and then Low = B * C and then High = A * C)
          or else
            (A < 0.0
             and then B > 0.0
             and then C < 0.0
             and then D > 0.0
             and then Low = Least (A * D, B * C)
             and then High = Greatest (A * C, B * D))),
     Post => Least_Corner (A, B, C, D) = Low and then Greatest_Corner (A, B, C, D) = High;
   --  The same, for the one case which the caller is in: it names the corner products which that case makes
   --  extreme, and learns nothing of the other cases.

   procedure Lemma_Corners_Are (A, B, C, D, Low, High : Real) is
   begin
      Lemma_Corner_Extremes (A, B, C, D);
   end Lemma_Corners_Are;

   ---------------------------------------------------------------------------
   --  Scaling a control by its binomial coefficient
   ---------------------------------------------------------------------------

   procedure Lemma_Scaled_Endpoints
     (Low, High : Real; Low_Scaled, High_Scaled, Low_Padded, High_Padded : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs Low_Scaled <= Half_Last
       and then abs High_Scaled <= Half_Last
       and then Low_Scaled <= High_Scaled
       and then Is_Rounding_Of (Low_Scaled, Low)
       and then Is_Rounding_Of (High_Scaled, High)
       and then Low_Padded = Inflate_Finite (Low_Scaled, False)
       and then High_Padded = Inflate_Finite (High_Scaled, True),
     Post =>
       Low_Padded <= High_Padded
       and then R (Low_Padded) <= Low
       and then R (Low_Padded) <= High
       and then Low <= R (High_Padded)
       and then High <= R (High_Padded);
   --  When the rounded values of two exact products are in order, the lesser padded downward lies below both exact
   --  products and the greater padded upward lies above both, whatever the order of the exact products.

   procedure Lemma_Scaled_Endpoints
     (Low, High : Real; Low_Scaled, High_Scaled, Low_Padded, High_Padded : Dimensionless) is
   begin
      Lemma_Rounding_Slack (Low_Scaled, Low);
      Lemma_Rounding_Slack (High_Scaled, High);
      Lemma_Inflate_Margin (Low_Scaled);
      Lemma_Inflate_Margin (High_Scaled);
      Lemma_Slack_Is_Monotonic (Low_Scaled, High_Scaled);
      Lemma_Same_Real (Low_Padded, Inflate_Finite (Low_Scaled, False));
      Lemma_Same_Real (High_Padded, Inflate_Finite (High_Scaled, True));
      pragma Assert (R (Low_Scaled) - Slack (Low_Scaled) <= Low);
      pragma Assert (R (High_Scaled) - Slack (High_Scaled) <= High);
      pragma Assert (Low <= R (Low_Scaled) + Slack (Low_Scaled));
      pragma Assert (High <= R (High_Scaled) + Slack (High_Scaled));
      pragma Assert (R (Low_Padded) <= R (Low_Scaled) - Slack (Low_Scaled));
      pragma Assert (R (High_Scaled) + Slack (High_Scaled) <= R (High_Padded));
   end Lemma_Scaled_Endpoints;

   function Is_Binomial_Scaling (Result, Value : Interval; Factor : Dimensionless) return Boolean
   with
     Ghost,
     Post =>
       Is_Binomial_Scaling'Result
       = (if Result.Valid
          then
            Value.Valid
            and then Result.Lower <= Result.Upper
            and then Contains (Result, R (Factor) * R (Value.Lower))
            and then Contains (Result, R (Factor) * R (Value.Upper)));
   --  What Scale_By_Binomial promises: a valid result is ordered and contains both scaled endpoints of Value,
   --  which is then valid as well.

   function Is_Binomial_Scaling (Result, Value : Interval; Factor : Dimensionless) return Boolean is
   begin
      return
        (if Result.Valid
         then
           Value.Valid
           and then Result.Lower <= Result.Upper
           and then Contains (Result, R (Factor) * R (Value.Lower))
           and then Contains (Result, R (Factor) * R (Value.Upper)));
   end Is_Binomial_Scaling;

   procedure Lemma_Exact_Scalings (Value : Interval; Factor : Dimensionless)
   with
     Ghost,
     Pre  => Factor >= 1.0 and then Factor <= Largest_Binomial,
     Post =>
       Is_Binomial_Scaling (Invalid_Interval, Value, Factor)
       and then (if not Value.Valid or else Is_Zero (Value) then Is_Binomial_Scaling (Value, Value, Factor))
       and then (if Factor = 1.0 and then Is_Usable (Value) then Is_Binomial_Scaling (Value, Value, Factor))
       and then
         (if Value.Valid and then Value.Lower = 1.0 and then Value.Upper = 1.0
          then Is_Binomial_Scaling ((Lower => Factor, Upper => Factor, Valid => True), Value, Factor))
       and then
         (if Value.Valid and then Value.Lower = -1.0 and then Value.Upper = -1.0
          then Is_Binomial_Scaling ((Lower => -Factor, Upper => -Factor, Valid => True), Value, Factor));
   --  The results which Scale_By_Binomial gives without rounding.

   procedure Lemma_Exact_Scalings (Value : Interval; Factor : Dimensionless) is
      Weight : constant Real := R (Factor);
   begin
      if Value.Valid and then Is_Zero (Value) then
         pragma Assert (R (Value.Lower) = 0.0 and then R (Value.Upper) = 0.0);
         pragma Assert (Weight * R (Value.Lower) = 0.0 and then Weight * R (Value.Upper) = 0.0);
      end if;
      if Factor = 1.0 and then Is_Usable (Value) then
         pragma Assert (Weight = 1.0);
         pragma Assert (Weight * R (Value.Lower) = R (Value.Lower));
         pragma Assert (Weight * R (Value.Upper) = R (Value.Upper));
         pragma Assert (R (Value.Lower) <= R (Value.Upper));
      end if;
      if Value.Lower = 1.0 and then Value.Upper = 1.0 then
         pragma Assert (R (Value.Lower) = 1.0 and then R (Value.Upper) = 1.0);
         pragma Assert (Weight * R (Value.Lower) = Weight and then Weight * R (Value.Upper) = Weight);
      end if;
      if Value.Lower = -1.0 and then Value.Upper = -1.0 then
         pragma Assert (R (Value.Lower) = 0.0 - 1.0 and then R (Value.Upper) = 0.0 - 1.0);
         pragma Assert (R (-Factor) = 0.0 - Weight);
         pragma Assert (Weight * R (Value.Lower) = 0.0 - Weight and then Weight * R (Value.Upper) = 0.0 - Weight);
      end if;
   end Lemma_Exact_Scalings;

   procedure Lemma_Rounded_Scaling
     (Value : Interval; Factor, Rounded_Lower, Rounded_Upper, Lower, Upper : Dimensionless)
   with
     Ghost,
     Pre  =>
       Value.Valid
       and then abs Rounded_Lower <= Half_Last
       and then abs Rounded_Upper <= Half_Last
       and then Rounded_Lower <= Rounded_Upper
       and then Is_Rounding_Of (Rounded_Lower, R (Value.Lower) * R (Factor))
       and then Is_Rounding_Of (Rounded_Upper, R (Value.Upper) * R (Factor))
       and then Lower = Inflate_Finite (Rounded_Lower, False)
       and then Upper = Inflate_Finite (Rounded_Upper, True),
     Post => Is_Binomial_Scaling ((Lower => Lower, Upper => Upper, Valid => True), Value, Factor);
   --  The rounded products, when they are in order, padded outward.

   procedure Lemma_Rounded_Scaling
     (Value : Interval; Factor, Rounded_Lower, Rounded_Upper, Lower, Upper : Dimensionless)
   is
      Result : constant Interval := (Lower => Lower, Upper => Upper, Valid => True);
   begin
      Lemma_Scaled_Endpoints
        (R (Value.Lower) * R (Factor), R (Value.Upper) * R (Factor), Rounded_Lower, Rounded_Upper, Lower, Upper);
      pragma Assert (Result.Lower <= Result.Upper);
      pragma Assert (R (Lower) <= R (Factor) * R (Value.Lower) and then R (Factor) * R (Value.Lower) <= R (Upper));
      pragma Assert (R (Lower) <= R (Factor) * R (Value.Upper) and then R (Factor) * R (Value.Upper) <= R (Upper));
      pragma Assert (Contains (Result, R (Factor) * R (Value.Lower)));
      pragma Assert (Contains (Result, R (Factor) * R (Value.Upper)));
      pragma Assert (Is_Binomial_Scaling (Result, Value, Factor));
   end Lemma_Rounded_Scaling;

   function Scale_By_Binomial (Value : Interval; Factor : Dimensionless) return Interval
   with
     Inline,
     Pre  => Factor >= 1.0 and then Factor <= Largest_Binomial,
     Post => Is_Binomial_Scaling (Scale_By_Binomial'Result, Value, Factor);
   --  Enclose Value times an exact binomial coefficient.

   function Scale_By_Binomial (Value : Interval; Factor : Dimensionless) return Interval is
      Lower, Upper : Dimensionless;

      Rounded_Lower, Rounded_Upper : Dimensionless
      with Ghost;
      --  The products before they are padded.
   begin
      Lemma_Exact_Scalings (Value, Factor);
      if not Value.Valid or else (Value.Lower = 0.0 and then Value.Upper = 0.0) then
         return Value;
      elsif Factor = 1.0 then
         if Is_Usable (Value) then
            return Value;
         else
            return Invalid_Interval;
         end if;
      elsif Value.Lower = 1.0 and then Value.Upper = 1.0 then
         return (Lower => Factor, Upper => Factor, Valid => True);
      elsif Value.Lower = -1.0 and then Value.Upper = -1.0 then
         return (Lower => -Factor, Upper => -Factor, Valid => True);
      elsif not (abs Value.Lower <= Product_Operand_Limit) or else not (abs Value.Upper <= Product_Operand_Limit) then
         --  The general product can still enclose a larger control. This one fails closed.
         return Invalid_Interval;
      end if;
      --  Each product is one rounded operation, which the padding then covers.
      Lemma_Product_Is_Bounded (Value.Lower, Factor);
      Lemma_Product_Is_Bounded (Value.Upper, Factor);
      Lower := Value.Lower * Factor;
      Upper := Value.Upper * Factor;
      if Lower > -Dimensionless'Last / 2.0 and then Upper < Dimensionless'Last / 2.0 and then Lower <= Upper then
         Rounded_Lower := Lower;
         Rounded_Upper := Upper;
         Inflate_Finite_Endpoints (Lower, Upper);
         Lemma_Rounded_Scaling (Value, Factor, Rounded_Lower, Rounded_Upper, Lower, Upper);
         return (Lower => Lower, Upper => Upper, Valid => True);
      else
         return Invalid_Interval;
      end if;
   end Scale_By_Binomial;

   ---------------------------------------------------------------------------
   --  The exact bounds which a product control must enclose
   ---------------------------------------------------------------------------

   function Scaled_Lower (Value : Polynomial; Index : Degree_Range) return Real
   is (Choose (Value.Degree, Index) * R (Value.Controls (Index).Lower))
   with Ghost;
   --  The lower endpoint of a control, scaled exactly by its binomial coefficient.

   function Scaled_Upper (Value : Polynomial; Index : Degree_Range) return Real
   is (Choose (Value.Degree, Index) * R (Value.Controls (Index).Upper))
   with Ghost;

   function Is_Scaled_Control (Scaled : Interval; Value : Polynomial; Index : Degree_Range) return Boolean
   with
     Ghost,
     Post =>
       Is_Scaled_Control'Result
       = (Value.Controls (Index).Valid
          and then Scaled.Lower <= Scaled.Upper
          and then Contains (Scaled, Scaled_Lower (Value, Index))
          and then Contains (Scaled, Scaled_Upper (Value, Index)));
   --  What Multiply knows of each scaled control which it keeps: it is ordered, and contains the exactly scaled
   --  endpoints of a valid control. This is not an expression function, so that the tool keeps it as one term in
   --  loop invariants rather than expanding it; the provers do far better matching the term than its expansion.

   function Is_Scaled_Control (Scaled : Interval; Value : Polynomial; Index : Degree_Range) return Boolean is
   begin
      return
        Value.Controls (Index).Valid
        and then Scaled.Lower <= Scaled.Upper
        and then Contains (Scaled, Scaled_Lower (Value, Index))
        and then Contains (Scaled, Scaled_Upper (Value, Index));
   end Is_Scaled_Control;

   procedure Lemma_Scaled_Control (Scaled : Interval; Factor : Dimensionless; Value : Polynomial; Index : Degree_Range)
   with
     Ghost,
     Pre  =>
       Scaled.Valid
       and then R (Factor) = Choose (Value.Degree, Index)
       and then Is_Binomial_Scaling (Scaled, Value.Controls (Index), Factor),
     Post =>
       Is_Scaled_Control (Scaled, Value, Index)
       and then Value.Controls (Index).Valid
       and then Scaled.Lower <= Scaled.Upper;
   --  What Scale_By_Binomial returns for an exact binomial coefficient, when valid, is a scaled control.

   procedure Lemma_Scaled_Control (Scaled : Interval; Factor : Dimensionless; Value : Polynomial; Index : Degree_Range)
   is
   begin
      pragma Assert (Contains (Scaled, R (Factor) * R (Value.Controls (Index).Lower)));
      pragma Assert (Contains (Scaled, R (Factor) * R (Value.Controls (Index).Upper)));
      pragma Assert (Contains (Scaled, Scaled_Lower (Value, Index)));
      pragma Assert (Contains (Scaled, Scaled_Upper (Value, Index)));
   end Lemma_Scaled_Control;

   function Low_Term (Left, Right : Polynomial; K, I : Degree_Range) return Real
   is (if I <= Left.Degree and then K >= I and then K - I <= Right.Degree
       then
         Least_Corner
           (Scaled_Lower (Left, I), Scaled_Upper (Left, I), Scaled_Lower (Right, K - I), Scaled_Upper (Right, K - I))
       else 0.0)
   with Ghost;
   --  The least value of term I of control K of the product, over the exactly scaled controls: zero unless both
   --  controls exist.

   function High_Term (Left, Right : Polynomial; K, I : Degree_Range) return Real
   is (if I <= Left.Degree and then K >= I and then K - I <= Right.Degree
       then
         Greatest_Corner
           (Scaled_Lower (Left, I), Scaled_Upper (Left, I), Scaled_Lower (Right, K - I), Scaled_Upper (Right, K - I))
       else 0.0)
   with Ghost;

   function Low_Sum (Left, Right : Polynomial; K : Degree_Range; Count : Natural) return Real
   is (if Count = 0 then 0.0 else Low_Sum (Left, Right, K, Count - 1) + Low_Term (Left, Right, K, Count - 1))
   with Ghost, Pre => Count <= Maximum_Degree + 1, Subprogram_Variant => (Decreases => Count);
   --  The sum of the least values of the first Count terms of control K, over the same terms as
   --  Binomial_Form.Scaled_Convolution.

   function High_Sum (Left, Right : Polynomial; K : Degree_Range; Count : Natural) return Real
   is (if Count = 0 then 0.0 else High_Sum (Left, Right, K, Count - 1) + High_Term (Left, Right, K, Count - 1))
   with Ghost, Pre => Count <= Maximum_Degree + 1, Subprogram_Variant => (Decreases => Count);

   function Encloses_Sums (Control : Interval; Left, Right : Polynomial; K : Degree_Range) return Boolean
   with
     Ghost,
     Pre  => Left.Degree + Right.Degree <= Maximum_Degree,
     Post =>
       Encloses_Sums'Result
       = (R (Control.Lower) * Choose (Left.Degree + Right.Degree, K) <= Low_Sum (Left, Right, K, Left.Degree + 1)
          and then
            High_Sum (Left, Right, K, Left.Degree + 1) <= R (Control.Upper) * Choose (Left.Degree + Right.Degree, K));
   --  Whether Control, times the binomial coefficient of the product's degree, reaches from below the least to
   --  above the greatest sum of the terms of control K. Like Is_Scaled_Control, this is kept as one term.

   function Encloses_Sums (Control : Interval; Left, Right : Polynomial; K : Degree_Range) return Boolean is
   begin
      return
        R (Control.Lower) * Choose (Left.Degree + Right.Degree, K) <= Low_Sum (Left, Right, K, Left.Degree + 1)
        and then
          High_Sum (Left, Right, K, Left.Degree + 1) <= R (Control.Upper) * Choose (Left.Degree + Right.Degree, K);
   end Encloses_Sums;

   procedure Lemma_Leading_Terms (Left, Right : Polynomial; K : Degree_Range; Count : Natural)
   with
     Ghost,
     Subprogram_Variant => (Decreases => Count),
     Pre                => Count <= Maximum_Degree + 1 and then (Count = 0 or else Count + Right.Degree <= K),
     Post               => Low_Sum (Left, Right, K, Count) = 0.0 and then High_Sum (Left, Right, K, Count) = 0.0;
   --  The terms whose right control would lie beyond the right operand's degree are zero.

   procedure Lemma_Leading_Terms (Left, Right : Polynomial; K : Degree_Range; Count : Natural) is
   begin
      if Count > 0 then
         Lemma_Leading_Terms (Left, Right, K, Count - 1);
         pragma Assert (Low_Term (Left, Right, K, Count - 1) = 0.0);
         pragma Assert (High_Term (Left, Right, K, Count - 1) = 0.0);
      end if;
   end Lemma_Leading_Terms;

   procedure Lemma_Trailing_Terms (Left, Right : Polynomial; K : Degree_Range; Count : Natural)
   with
     Ghost,
     Subprogram_Variant => (Decreases => Count),
     Pre                => Count <= Maximum_Degree + 1,
     Post               =>
       Low_Sum (Left, Right, K, Count) = Low_Sum (Left, Right, K, Integer'Min (Count, K + 1))
       and then High_Sum (Left, Right, K, Count) = High_Sum (Left, Right, K, Integer'Min (Count, K + 1));
   --  The terms whose left index exceeds K are zero.

   procedure Lemma_Trailing_Terms (Left, Right : Polynomial; K : Degree_Range; Count : Natural) is
   begin
      if Count > K + 1 then
         Lemma_Trailing_Terms (Left, Right, K, Count - 1);
         pragma Assert (Low_Term (Left, Right, K, Count - 1) = 0.0);
         pragma Assert (High_Term (Left, Right, K, Count - 1) = 0.0);
      end if;
   end Lemma_Trailing_Terms;

   function Is_Zero_Polynomial (Value : Polynomial) return Boolean
   is (for all I in 0 .. Value.Degree => Scaled_Lower (Value, I) = 0.0 and then Scaled_Upper (Value, I) = 0.0)
   with Ghost;

   procedure Lemma_Zero_Sums (Left, Right : Polynomial; K : Degree_Range; Count : Natural)
   with
     Ghost,
     Subprogram_Variant => (Decreases => Count),
     Pre                =>
       Count <= Maximum_Degree + 1 and then (Is_Zero_Polynomial (Left) or else Is_Zero_Polynomial (Right)),
     Post               => Low_Sum (Left, Right, K, Count) = 0.0 and then High_Sum (Left, Right, K, Count) = 0.0;
   --  Every term has a zero factor when all the scaled controls of one operand are zero.

   procedure Lemma_Zero_Sums (Left, Right : Polynomial; K : Degree_Range; Count : Natural) is
   begin
      if Count > 0 then
         Lemma_Zero_Sums (Left, Right, K, Count - 1);
         if Count - 1 <= Left.Degree and then K >= Count - 1 and then K - (Count - 1) <= Right.Degree then
            Lemma_Zero_Corners
              (Scaled_Lower (Left, Count - 1),
               Scaled_Upper (Left, Count - 1),
               Scaled_Lower (Right, K - (Count - 1)),
               Scaled_Upper (Right, K - (Count - 1)));
         end if;
         pragma Assert (Low_Term (Left, Right, K, Count - 1) = 0.0);
         pragma Assert (High_Term (Left, Right, K, Count - 1) = 0.0);
      end if;
   end Lemma_Zero_Sums;

   function Is_Zero_Control (Control : Interval) return Boolean
   is (Control.Valid and then R (Control.Lower) = 0.0 and then R (Control.Upper) = 0.0)
   with Ghost;

   procedure Lemma_Zero_Product (Left, Right : Polynomial; Result : Polynomial)
   with
     Ghost,
     Pre  =>
       Left.Degree + Right.Degree <= Maximum_Degree
       and then (Is_Zero_Polynomial (Left) or else Is_Zero_Polynomial (Right))
       and then (for all K in 0 .. Left.Degree + Right.Degree => Is_Zero_Control (Result.Controls (K))),
     Post => (for all K in 0 .. Left.Degree + Right.Degree => Encloses_Sums (Result.Controls (K), Left, Right, K));
   --  Zero controls are the product when an operand is zero.

   procedure Lemma_Zero_Product (Left, Right : Polynomial; Result : Polynomial) is
   begin
      for K in 0 .. Left.Degree + Right.Degree loop
         Lemma_Zero_Sums (Left, Right, K, Left.Degree + 1);
         pragma Assert (R (Result.Controls (K).Lower) * Choose (Left.Degree + Right.Degree, K) = 0.0);
         pragma Assert (R (Result.Controls (K).Upper) * Choose (Left.Degree + Right.Degree, K) = 0.0);
         pragma Loop_Invariant (for all J in 0 .. K => Encloses_Sums (Result.Controls (J), Left, Right, J));
      end loop;
   end Lemma_Zero_Product;

   procedure Lemma_Term_Is_Enclosed
     (Scaled_Left, Scaled_Right : Interval; Left, Right : Polynomial; K, I : Degree_Range)
   with
     Ghost,
     Pre  =>
       I <= Left.Degree
       and then K >= I
       and then K - I <= Right.Degree
       and then Is_Scaled_Control (Scaled_Left, Left, I)
       and then Is_Scaled_Control (Scaled_Right, Right, K - I),
     Post =>
       Low_Product (Scaled_Left, Scaled_Right) <= Low_Term (Left, Right, K, I)
       and then High_Term (Left, Right, K, I) <= High_Product (Scaled_Left, Scaled_Right)
       and then
         (if Is_Zero (Scaled_Left)
          then Low_Term (Left, Right, K, I) = 0.0 and then High_Term (Left, Right, K, I) = 0.0);
   --  The corner products of two scaled controls bound the term over the exactly scaled controls, which is zero
   --  when the left control is.

   procedure Lemma_Term_Is_Enclosed
     (Scaled_Left, Scaled_Right : Interval; Left, Right : Polynomial; K, I : Degree_Range) is
   begin
      Lemma_Nested_Corners
        (Scaled_Left,
         Scaled_Right,
         Scaled_Lower (Left, I),
         Scaled_Upper (Left, I),
         Scaled_Lower (Right, K - I),
         Scaled_Upper (Right, K - I));
      if Is_Zero (Scaled_Left) then
         Lemma_Zero_Corners
           (Scaled_Lower (Left, I), Scaled_Upper (Left, I), Scaled_Lower (Right, K - I), Scaled_Upper (Right, K - I));
      end if;
   end Lemma_Term_Is_Enclosed;

   ---------------------------------------------------------------------------
   --  One term of a control
   ---------------------------------------------------------------------------

   function Is_Scaled_Operand (Value : Interval) return Boolean
   is (abs Value.Lower <= Scaled_Control_Limit
       and then abs Value.Upper <= Scaled_Control_Limit
       and then Value.Lower <= Value.Upper)
   with Ghost;
   --  What Multiply has established of every scaled control before it forms a product.

   procedure Scaled_Product_Extrema (Left, Right : Interval; Lower_Product, Upper_Product : out Dimensionless)
   with
     Inline,
     Pre  => Is_Scaled_Operand (Left) and then Is_Scaled_Operand (Right),
     Post =>
       abs Lower_Product <= Product_Limit
       and then abs Upper_Product <= Product_Limit
       and then R (Lower_Product) - Slack (Lower_Product) <= Low_Product (Left, Right)
       and then High_Product (Left, Right) <= R (Upper_Product) + Slack (Upper_Product);
   --  Form the extremal floating-point corner products, by the very steps of Intervals.Interval_Product_Extrema.
   --  That procedure's contract bounds each product on one side only, which cannot keep the partial sums of a
   --  control from overflowing; this one also states that the products are no larger than rounding allows.

   procedure Scaled_Product_Extrema (Left, Right : Interval; Lower_Product, Upper_Product : out Dimensionless) is
      pragma Suppress (Overflow_Check);

      procedure Prove_Rounded (A, B : Dimensionless)
      with
        Ghost,
        Pre  => abs A <= Scaled_Control_Limit and then abs B <= Scaled_Control_Limit,
        Post =>
          abs (A * B) <= Product_Limit
          and then R (A * B) - Slack (A * B) <= R (A) * R (B)
          and then R (A) * R (B) <= R (A * B) + Slack (A * B);

      procedure Prove_Rounded (A, B : Dimensionless) is
      begin
         Lemma_Real_Product_Bound (R (A), R (B), Control_Bound, Control_Bound);
         Lemma_Product_Is_Bounded (A, B);
         Lemma_Rounding_Slack (A * B, R (A) * R (B));
      end Prove_Rounded;

      A : constant Real := R (Left.Lower)
      with Ghost;
      B : constant Real := R (Left.Upper)
      with Ghost;
      C : constant Real := R (Right.Lower)
      with Ghost;
      D : constant Real := R (Right.Upper)
      with Ghost;
   begin
      --  Except when both intervals span zero, their signs identify the two corner products which attain the
      --  product interval's extrema. Avoid forming the other two products and the four-way min/max in the common
      --  cases; all binomial weights and denominator reciprocals take one of these two-product paths.
      if Left.Lower >= 0.0 then
         if Right.Lower >= 0.0 then
            Prove_Rounded (Left.Lower, Right.Lower);
            Prove_Rounded (Left.Upper, Right.Upper);
            Lemma_Corners_Are (A, B, C, D, A * C, B * D);
            Lower_Product := Left.Lower * Right.Lower;
            Upper_Product := Left.Upper * Right.Upper;
            pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= Low_Product (Left, Right));
            pragma Assert (High_Product (Left, Right) <= R (Upper_Product) + Slack (Upper_Product));
         elsif Right.Upper <= 0.0 then
            Prove_Rounded (Left.Upper, Right.Lower);
            Prove_Rounded (Left.Lower, Right.Upper);
            Lemma_Corners_Are (A, B, C, D, B * C, A * D);
            Lower_Product := Left.Upper * Right.Lower;
            Upper_Product := Left.Lower * Right.Upper;
            pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= Low_Product (Left, Right));
            pragma Assert (High_Product (Left, Right) <= R (Upper_Product) + Slack (Upper_Product));
         else
            Prove_Rounded (Left.Upper, Right.Lower);
            Prove_Rounded (Left.Upper, Right.Upper);
            Lemma_Corners_Are (A, B, C, D, B * C, B * D);
            Lower_Product := Left.Upper * Right.Lower;
            Upper_Product := Left.Upper * Right.Upper;
            pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= Low_Product (Left, Right));
            pragma Assert (High_Product (Left, Right) <= R (Upper_Product) + Slack (Upper_Product));
         end if;
      elsif Left.Upper <= 0.0 then
         if Right.Lower >= 0.0 then
            Prove_Rounded (Left.Lower, Right.Upper);
            Prove_Rounded (Left.Upper, Right.Lower);
            Lemma_Corners_Are (A, B, C, D, A * D, B * C);
            Lower_Product := Left.Lower * Right.Upper;
            Upper_Product := Left.Upper * Right.Lower;
            pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= Low_Product (Left, Right));
            pragma Assert (High_Product (Left, Right) <= R (Upper_Product) + Slack (Upper_Product));
         elsif Right.Upper <= 0.0 then
            Prove_Rounded (Left.Upper, Right.Upper);
            Prove_Rounded (Left.Lower, Right.Lower);
            Lemma_Corners_Are (A, B, C, D, B * D, A * C);
            Lower_Product := Left.Upper * Right.Upper;
            Upper_Product := Left.Lower * Right.Lower;
            pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= Low_Product (Left, Right));
            pragma Assert (High_Product (Left, Right) <= R (Upper_Product) + Slack (Upper_Product));
         else
            Prove_Rounded (Left.Lower, Right.Upper);
            Prove_Rounded (Left.Lower, Right.Lower);
            Lemma_Corners_Are (A, B, C, D, A * D, A * C);
            Lower_Product := Left.Lower * Right.Upper;
            Upper_Product := Left.Lower * Right.Lower;
            pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= Low_Product (Left, Right));
            pragma Assert (High_Product (Left, Right) <= R (Upper_Product) + Slack (Upper_Product));
         end if;
      elsif Right.Lower >= 0.0 then
         Prove_Rounded (Left.Lower, Right.Upper);
         Prove_Rounded (Left.Upper, Right.Upper);
         Lemma_Corners_Are (A, B, C, D, A * D, B * D);
         Lower_Product := Left.Lower * Right.Upper;
         Upper_Product := Left.Upper * Right.Upper;
         pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= Low_Product (Left, Right));
         pragma Assert (High_Product (Left, Right) <= R (Upper_Product) + Slack (Upper_Product));
      elsif Right.Upper <= 0.0 then
         Prove_Rounded (Left.Upper, Right.Lower);
         Prove_Rounded (Left.Lower, Right.Lower);
         Lemma_Corners_Are (A, B, C, D, B * C, A * C);
         Lower_Product := Left.Upper * Right.Lower;
         Upper_Product := Left.Lower * Right.Lower;
         pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= Low_Product (Left, Right));
         pragma Assert (High_Product (Left, Right) <= R (Upper_Product) + Slack (Upper_Product));
      else
         Prove_Rounded (Left.Lower, Right.Upper);
         Prove_Rounded (Left.Upper, Right.Lower);
         Prove_Rounded (Left.Lower, Right.Lower);
         Prove_Rounded (Left.Upper, Right.Upper);
         Lemma_Corners_Are (A, B, C, D, Least (A * D, B * C), Greatest (A * C, B * D));
         Lower_Product := Dimensionless'Min (Left.Lower * Right.Upper, Left.Upper * Right.Lower);
         Upper_Product := Dimensionless'Max (Left.Lower * Right.Lower, Left.Upper * Right.Upper);
         Lemma_Slack_Is_Monotonic (Lower_Product, Left.Lower * Right.Upper);
         Lemma_Slack_Is_Monotonic (Lower_Product, Left.Upper * Right.Lower);
         Lemma_Slack_Is_Monotonic (Left.Lower * Right.Lower, Upper_Product);
         Lemma_Slack_Is_Monotonic (Left.Upper * Right.Upper, Upper_Product);
         pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= A * D);
         pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= B * C);
         pragma Assert (A * C <= R (Upper_Product) + Slack (Upper_Product));
         pragma Assert (B * D <= R (Upper_Product) + Slack (Upper_Product));
         pragma Assert (R (Lower_Product) - Slack (Lower_Product) <= Low_Product (Left, Right));
         pragma Assert (High_Product (Left, Right) <= R (Upper_Product) + Slack (Upper_Product));
      end if;
   end Scaled_Product_Extrema;

   procedure Lemma_Padded_Product (Product : Dimensionless)
   with
     Ghost,
     Pre  => abs Product <= Product_Limit,
     Post =>
       abs Inflate_Finite (Product, False) <= Padded_Limit
       and then abs Inflate_Finite (Product, True) <= Padded_Limit
       and then R (Inflate_Finite (Product, False)) <= R (Product) - Slack (Product)
       and then R (Product) + Slack (Product) <= R (Inflate_Finite (Product, True));
   --  Padding a product keeps it within the next power of two, and moves it by more than its slack.

   procedure Lemma_Padded_Product (Product : Dimensionless) is
   begin
      Lemma_Inflate_Margin (Product);
      Lemma_Inflate_Reach (Product);
      pragma Assert (abs (R (Product)) <= Product_Bound);
      pragma Assert (Reach (Product) <= Reach_Factor * Product_Bound + Reach_Floor);
      pragma Assert (abs (R (Inflate_Finite (Product, False))) <= Padded_Bound);
      pragma Assert (abs (R (Inflate_Finite (Product, True))) <= Padded_Bound);
   end Lemma_Padded_Product;

   function Is_Term (Lower_Product, Upper_Product : Dimensionless; Left, Right : Interval) return Boolean
   is (abs Lower_Product <= Padded_Limit
       and then abs Upper_Product <= Padded_Limit
       and then R (Lower_Product) <= Low_Product (Left, Right)
       and then High_Product (Left, Right) <= R (Upper_Product))
   with Ghost;
   --  Whether two numbers enclose every product of members of Left and Right, and are small enough to be added to
   --  a partial sum.

   procedure Lemma_Padded_Term
     (Rounded_Lower, Rounded_Upper, Lower_Product, Upper_Product : Dimensionless; Left, Right : Interval)
   with
     Ghost,
     Pre  =>
       abs Rounded_Lower <= Product_Limit
       and then abs Rounded_Upper <= Product_Limit
       and then R (Rounded_Lower) - Slack (Rounded_Lower) <= Low_Product (Left, Right)
       and then High_Product (Left, Right) <= R (Rounded_Upper) + Slack (Rounded_Upper)
       and then Lower_Product = Inflate_Finite (Rounded_Lower, False)
       and then Upper_Product = Inflate_Finite (Rounded_Upper, True),
     Post => Is_Term (Lower_Product, Upper_Product, Left, Right);
   --  The padded extremal products are a term.

   procedure Lemma_Padded_Term
     (Rounded_Lower, Rounded_Upper, Lower_Product, Upper_Product : Dimensionless; Left, Right : Interval) is
   begin
      Lemma_Padded_Product (Rounded_Lower);
      Lemma_Padded_Product (Rounded_Upper);
      Lemma_Same_Real (Lower_Product, Inflate_Finite (Rounded_Lower, False));
      Lemma_Same_Real (Upper_Product, Inflate_Finite (Rounded_Upper, True));
   end Lemma_Padded_Term;

   procedure Lemma_Exact_Terms (Left, Right : Interval)
   with
     Ghost,
     Pre  => Is_Scaled_Operand (Left) and then Is_Scaled_Operand (Right),
     Post =>
       (if Is_Zero (Right) then Low_Product (Left, Right) = 0.0 and then High_Product (Left, Right) = 0.0)
       and then (if Left.Lower = 1.0 and then Left.Upper = 1.0 then Is_Term (Right.Lower, Right.Upper, Left, Right))
       and then (if Right.Lower = 1.0 and then Right.Upper = 1.0 then Is_Term (Left.Lower, Left.Upper, Left, Right))
       and then
         (if Left.Lower = -1.0 and then Left.Upper = -1.0 then Is_Term (-Right.Upper, -Right.Lower, Left, Right))
       and then
         (if Right.Lower = -1.0 and then Right.Upper = -1.0 then Is_Term (-Left.Upper, -Left.Lower, Left, Right));
   --  The products which Accumulate_Product takes without rounding: with an exact zero, one or minus one.

   procedure Lemma_Exact_Terms (Left, Right : Interval) is
      A : constant Real := R (Left.Lower);
      B : constant Real := R (Left.Upper);
      C : constant Real := R (Right.Lower);
      D : constant Real := R (Right.Upper);
   begin
      pragma Assert (A <= B and then C <= D);
      if Is_Zero (Right) then
         Lemma_Zero_Corners (A, B, C, D);
      end if;
      if Left.Lower = 1.0 and then Left.Upper = 1.0 then
         pragma Assert (A = 1.0 and then B = 1.0);
         pragma Assert (A * C = C and then A * D = D and then B * C = C and then B * D = D);
         pragma Assert (Least_Corner (A, B, C, D) = C and then Greatest_Corner (A, B, C, D) = D);
      end if;
      if Right.Lower = 1.0 and then Right.Upper = 1.0 then
         pragma Assert (C = 1.0 and then D = 1.0);
         pragma Assert (A * C = A and then A * D = A and then B * C = B and then B * D = B);
         pragma Assert (Least_Corner (A, B, C, D) = A and then Greatest_Corner (A, B, C, D) = B);
      end if;
      if Left.Lower = -1.0 and then Left.Upper = -1.0 then
         pragma Assert (A = 0.0 - 1.0 and then B = 0.0 - 1.0);
         pragma Assert (A * C = 0.0 - C and then A * D = 0.0 - D and then B * C = 0.0 - C and then B * D = 0.0 - D);
         pragma Assert (Least_Corner (A, B, C, D) = 0.0 - D and then Greatest_Corner (A, B, C, D) = 0.0 - C);
         pragma Assert (R (-Right.Upper) = 0.0 - D and then R (-Right.Lower) = 0.0 - C);
      end if;
      if Right.Lower = -1.0 and then Right.Upper = -1.0 then
         pragma Assert (C = 0.0 - 1.0 and then D = 0.0 - 1.0);
         pragma Assert (A * C = 0.0 - A and then A * D = 0.0 - A and then B * C = 0.0 - B and then B * D = 0.0 - B);
         pragma Assert (Least_Corner (A, B, C, D) = 0.0 - B and then Greatest_Corner (A, B, C, D) = 0.0 - A);
         pragma Assert (R (-Left.Upper) = 0.0 - B and then R (-Left.Lower) = 0.0 - A);
      end if;
   end Lemma_Exact_Terms;

   procedure Lemma_Padded_Sum (Sum, Term : Dimensionless)
   with
     Ghost,
     Pre  => abs Sum <= Sum_Limit and then abs Term <= Padded_Limit,
     Post =>
       abs (Sum + Term) <= Half_Last
       and then R (Inflate_Finite (Sum + Term, False)) <= R (Sum) + R (Term)
       and then R (Sum) + R (Term) <= R (Inflate_Finite (Sum + Term, True))
       and then abs (R (Inflate_Finite (Sum + Term, False))) <= abs (R (Sum)) + Term_Growth
       and then abs (R (Inflate_Finite (Sum + Term, True))) <= abs (R (Sum)) + Term_Growth;
   --  Adding a padded product to a partial sum and padding the result encloses the exact sum, and moves the
   --  partial sum by no more than Term_Growth.

   procedure Lemma_Padded_Sum (Sum, Term : Dimensionless) is
      Rounded : constant Dimensionless := Sum + Term;
      Exact   : constant Real := R (Sum) + R (Term);
   begin
      Float_Lemmas.Lemma_Rounding_Error_Add (Sum, Term);
      pragma Assert (abs (R (Sum)) <= Sum_Bound);
      pragma Assert (abs (R (Term)) <= Padded_Bound);
      pragma Assert (abs Exact <= abs (R (Sum)) + Padded_Bound);
      pragma Assert (abs (R (Rounded)) <= abs Exact + Epsilon * abs Exact + Eta);
      pragma Assert (abs (R (Rounded)) <= abs (R (Sum)) + Padded_Bound + Epsilon * (Sum_Bound + Padded_Bound) + Eta);
      pragma Assert (abs Rounded <= Half_Last);
      Lemma_Inflate_Encloses (Rounded, Exact);
      Lemma_Inflate_Reach (Rounded);
      pragma Assert (Reach (Rounded) <= Reach_Factor * (Sum_Bound + Padded_Bound + Padded_Bound) + Reach_Floor);
      pragma Assert (abs (R (Inflate_Finite (Rounded, False))) <= abs (R (Rounded)) + Reach (Rounded));
      pragma Assert (abs (R (Inflate_Finite (Rounded, True))) <= abs (R (Rounded)) + Reach (Rounded));
   end Lemma_Padded_Sum;

   function Is_Accumulated
     (Old_Lower, Old_Upper, New_Lower, New_Upper : Dimensionless; Left, Right : Interval) return Boolean
   is (R (New_Lower) <= R (Old_Lower) + Low_Product (Left, Right)
       and then R (Old_Upper) + High_Product (Left, Right) <= R (New_Upper)
       and then abs (R (New_Lower)) <= abs (R (Old_Lower)) + Term_Growth
       and then abs (R (New_Upper)) <= abs (R (Old_Upper)) + Term_Growth)
   with Ghost;
   --  What adding the product of Left and Right does to the endpoints of a partial sum.

   procedure Lemma_No_Term (Old_Lower, Old_Upper : Dimensionless; Left, Right : Interval)
   with
     Ghost,
     Pre  => Low_Product (Left, Right) = 0.0 and then High_Product (Left, Right) = 0.0,
     Post => Is_Accumulated (Old_Lower, Old_Upper, Old_Lower, Old_Upper, Left, Right);
   --  A zero product leaves the sum as it is.

   procedure Lemma_No_Term (Old_Lower, Old_Upper : Dimensionless; Left, Right : Interval) is null;

   procedure Lemma_First_Term
     (Old_Lower, Old_Upper, Lower_Product, Upper_Product : Dimensionless; Left, Right : Interval)
   with
     Ghost,
     Pre  => Old_Lower = 0.0 and then Old_Upper = 0.0 and then Is_Term (Lower_Product, Upper_Product, Left, Right),
     Post => Is_Accumulated (Old_Lower, Old_Upper, Lower_Product, Upper_Product, Left, Right);
   --  A term replaces an exactly zero sum.

   procedure Lemma_First_Term
     (Old_Lower, Old_Upper, Lower_Product, Upper_Product : Dimensionless; Left, Right : Interval) is
   begin
      pragma Assert (R (Old_Lower) = 0.0 and then R (Old_Upper) = 0.0);
      pragma Assert (abs (R (Lower_Product)) <= Padded_Bound and then abs (R (Upper_Product)) <= Padded_Bound);
   end Lemma_First_Term;

   procedure Lemma_Next_Term
     (Old_Lower, Old_Upper, Lower_Product, Upper_Product, New_Lower, New_Upper : Dimensionless; Left, Right : Interval)
   with
     Ghost,
     Pre  =>
       abs Old_Lower <= Sum_Limit
       and then abs Old_Upper <= Sum_Limit
       and then Is_Term (Lower_Product, Upper_Product, Left, Right)
       and then abs (Old_Lower + Lower_Product) <= Half_Last
       and then abs (Old_Upper + Upper_Product) <= Half_Last
       and then New_Lower = Inflate_Finite (Old_Lower + Lower_Product, False)
       and then New_Upper = Inflate_Finite (Old_Upper + Upper_Product, True),
     Post => Is_Accumulated (Old_Lower, Old_Upper, New_Lower, New_Upper, Left, Right);
   --  A term is added to a sum, and the result padded.

   procedure Lemma_Next_Term
     (Old_Lower, Old_Upper, Lower_Product, Upper_Product, New_Lower, New_Upper : Dimensionless; Left, Right : Interval)
   is
   begin
      Lemma_Padded_Sum (Old_Lower, Lower_Product);
      Lemma_Padded_Sum (Old_Upper, Upper_Product);
      Lemma_Same_Real (New_Lower, Inflate_Finite (Old_Lower + Lower_Product, False));
      Lemma_Same_Real (New_Upper, Inflate_Finite (Old_Upper + Upper_Product, True));
   end Lemma_Next_Term;

   procedure Accumulate_Product (Sum : in out Interval; Left, Right : Interval)
   with
     Inline,
     Pre  =>
       Is_Scaled_Operand (Left)
       and then Is_Scaled_Operand (Right)
       and then abs Sum.Lower <= Sum_Limit
       and then abs Sum.Upper <= Sum_Limit,
     Post =>
       Sum.Valid = Sum'Old.Valid
       and then Is_Accumulated (Sum'Old.Lower, Sum'Old.Upper, Sum.Lower, Sum.Upper, Left, Right);
   --  Add an enclosure of the product of Left and Right to Sum. Exact identities are kept exact, and the first
   --  term replaces an exactly zero Sum without rounding.

   procedure Accumulate_Product (Sum : in out Interval; Left, Right : Interval) is
      pragma Suppress (Overflow_Check);
      Lower_Product, Upper_Product : Dimensionless;

      Old_Lower : constant Dimensionless := Sum.Lower
      with Ghost;
      Old_Upper : constant Dimensionless := Sum.Upper
      with Ghost;
      --  The sum on entry.

      Rounded_Lower, Rounded_Upper : Dimensionless
      with Ghost;
      --  The extremal products before they are padded.
   begin
      Lemma_Exact_Terms (Left, Right);
      --  The caller already skips exact-zero left controls.
      if Right.Lower = 0.0 and then Right.Upper = 0.0 then
         Lemma_No_Term (Old_Lower, Old_Upper, Left, Right);
         return;
      elsif Left.Lower = 1.0 and then Left.Upper = 1.0 then
         Lower_Product := Right.Lower;
         Upper_Product := Right.Upper;
      elsif Right.Lower = 1.0 and then Right.Upper = 1.0 then
         Lower_Product := Left.Lower;
         Upper_Product := Left.Upper;
      elsif Left.Lower = -1.0 and then Left.Upper = -1.0 then
         Lower_Product := -Right.Upper;
         Upper_Product := -Right.Lower;
      elsif Right.Lower = -1.0 and then Right.Upper = -1.0 then
         Lower_Product := -Left.Upper;
         Upper_Product := -Left.Lower;
      else
         Scaled_Product_Extrema (Left, Right, Lower_Product, Upper_Product);
         Rounded_Lower := Lower_Product;
         Rounded_Upper := Upper_Product;
         Inflate_Finite_Endpoints (Lower_Product, Upper_Product);
         Lemma_Padded_Term (Rounded_Lower, Rounded_Upper, Lower_Product, Upper_Product, Left, Right);
      end if;
      pragma Assert (Is_Term (Lower_Product, Upper_Product, Left, Right));
      if Sum.Lower = 0.0 and then Sum.Upper = 0.0 then
         Lemma_First_Term (Old_Lower, Old_Upper, Lower_Product, Upper_Product, Left, Right);
         Sum.Lower := Lower_Product;
         Sum.Upper := Upper_Product;
      else
         Lemma_Padded_Sum (Sum.Lower, Lower_Product);
         Lemma_Padded_Sum (Sum.Upper, Upper_Product);
         Sum.Lower := Sum.Lower + Lower_Product;
         Sum.Upper := Sum.Upper + Upper_Product;
         Inflate_Finite_Endpoints (Sum.Lower, Sum.Upper);
         Lemma_Next_Term (Old_Lower, Old_Upper, Lower_Product, Upper_Product, Sum.Lower, Sum.Upper, Left, Right);
      end if;
   end Accumulate_Product;

   ---------------------------------------------------------------------------
   --  The final scaling of a control
   ---------------------------------------------------------------------------

   procedure Lemma_Shrinking_Rounding (Value, Factor : Dimensionless)
   with
     Ghost,
     Pre  => Value in -Sum_Limit .. Sum_Limit and then Factor in -1.0 .. 1.0,
     Post => abs (R (Value * Factor) - R (Value) * R (Factor)) <= Epsilon * abs (R (Value) * R (Factor)) + Eta;
   --  A product with a factor within one cannot overflow, and is one rounded operation, although its other operand
   --  may exceed the 2**511 at which SPARKlib's lemma stops.

   procedure Lemma_Shrinking_Rounding (Value, Factor : Dimensionless) is null;

   procedure Lemma_Shrinking_Product (Value, Factor : Dimensionless)
   with
     Ghost,
     Pre  => abs Value <= Sum_Limit and then Factor >= 0.0 and then Factor <= 1.0,
     Post => abs (Value * Factor) <= Half_Last and then Is_Rounding_Of (Value * Factor, R (Value) * R (Factor));

   procedure Lemma_Shrinking_Product (Value, Factor : Dimensionless) is
   begin
      Lemma_Shrinking_Rounding (Value, Factor);
      Lemma_Real_Product_Bound (R (Value), R (Factor), Sum_Bound, 1.0);
      pragma Assert (abs (R (Value) * R (Factor)) <= Sum_Bound);
      pragma Assert (abs (R (Value * Factor)) <= Sum_Bound + Epsilon * Sum_Bound + Eta);
   end Lemma_Shrinking_Product;

   function Lower_Factor (Value : Interval; Factor : Positive_Endpoints) return Dimensionless
   is (if Value.Lower < 0.0 then Factor.Upper else Factor.Lower)
   with Ghost;
   --  The end of Factor which scales the lower endpoint of Value furthest downward.

   function Upper_Factor (Value : Interval; Factor : Positive_Endpoints) return Dimensionless
   is (if Value.Upper < 0.0 then Factor.Lower else Factor.Upper)
   with Ghost;

   function Is_Reciprocal_Scaling (Result, Value : Interval; Factor : Positive_Endpoints) return Boolean
   with
     Ghost,
     Post =>
       Is_Reciprocal_Scaling'Result
       = (Result.Valid
          and then R (Result.Lower) <= R (Value.Lower) * R (Lower_Factor (Value, Factor))
          and then R (Value.Upper) * R (Upper_Factor (Value, Factor)) <= R (Result.Upper));
   --  Whether Result reaches from below the lower endpoint of Value to above its upper endpoint, each scaled by
   --  the end of Factor which moves it outward.

   function Is_Reciprocal_Scaling (Result, Value : Interval; Factor : Positive_Endpoints) return Boolean is
   begin
      return
        Result.Valid
        and then R (Result.Lower) <= R (Value.Lower) * R (Lower_Factor (Value, Factor))
        and then R (Value.Upper) * R (Upper_Factor (Value, Factor)) <= R (Result.Upper);
   end Is_Reciprocal_Scaling;

   procedure Lemma_Exact_Reciprocal_Scalings (Value : Interval; Factor : Positive_Endpoints)
   with
     Ghost,
     Pre  => Value.Valid and then Factor.Lower > 0.0 and then Factor.Lower <= Factor.Upper,
     Post =>
       (if Is_Unit (Factor) or else Is_Zero (Value) then Is_Reciprocal_Scaling (Value, Value, Factor))
       and then
         (if Value.Lower = 1.0 and then Value.Upper = 1.0
          then Is_Reciprocal_Scaling ((Lower => Factor.Lower, Upper => Factor.Upper, Valid => True), Value, Factor))
       and then
         (if Value.Lower = -1.0 and then Value.Upper = -1.0
          then Is_Reciprocal_Scaling ((Lower => -Factor.Upper, Upper => -Factor.Lower, Valid => True), Value, Factor));
   --  The results which Scale_By_Reciprocal gives without rounding.

   procedure Lemma_Exact_Reciprocal_Scalings (Value : Interval; Factor : Positive_Endpoints) is
      Low  : constant Real := R (Value.Lower);
      High : constant Real := R (Value.Upper);
   begin
      if Is_Unit (Factor) then
         pragma Assert (R (Factor.Lower) = 1.0 and then R (Factor.Upper) = 1.0);
         pragma Assert (R (Lower_Factor (Value, Factor)) = 1.0 and then R (Upper_Factor (Value, Factor)) = 1.0);
         pragma Assert (Low * R (Lower_Factor (Value, Factor)) = Low);
         pragma Assert (High * R (Upper_Factor (Value, Factor)) = High);
      end if;
      if Is_Zero (Value) then
         pragma Assert (Low = 0.0 and then High = 0.0);
         pragma Assert (Low * R (Lower_Factor (Value, Factor)) = 0.0);
         pragma Assert (High * R (Upper_Factor (Value, Factor)) = 0.0);
      end if;
      if Value.Lower = 1.0 and then Value.Upper = 1.0 then
         pragma Assert (Low = 1.0 and then High = 1.0);
         pragma Assert (Low * R (Lower_Factor (Value, Factor)) = R (Factor.Lower));
         pragma Assert (High * R (Upper_Factor (Value, Factor)) = R (Factor.Upper));
      end if;
      if Value.Lower = -1.0 and then Value.Upper = -1.0 then
         pragma Assert (Low = 0.0 - 1.0 and then High = 0.0 - 1.0);
         pragma Assert (Low * R (Lower_Factor (Value, Factor)) = 0.0 - R (Factor.Upper));
         pragma Assert (High * R (Upper_Factor (Value, Factor)) = 0.0 - R (Factor.Lower));
         pragma Assert (R (-Factor.Upper) = 0.0 - R (Factor.Upper));
         pragma Assert (R (-Factor.Lower) = 0.0 - R (Factor.Lower));
      end if;
   end Lemma_Exact_Reciprocal_Scalings;

   procedure Lemma_Rounded_Reciprocal_Scaling
     (Value : Interval; Factor : Positive_Endpoints; Rounded_Lower, Rounded_Upper, Lower, Upper : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs Rounded_Lower <= Half_Last
       and then abs Rounded_Upper <= Half_Last
       and then Is_Rounding_Of (Rounded_Lower, R (Value.Lower) * R (Lower_Factor (Value, Factor)))
       and then Is_Rounding_Of (Rounded_Upper, R (Value.Upper) * R (Upper_Factor (Value, Factor)))
       and then Lower = Inflate_Finite (Rounded_Lower, False)
       and then Upper = Inflate_Finite (Rounded_Upper, True),
     Post => Is_Reciprocal_Scaling ((Lower => Lower, Upper => Upper, Valid => True), Value, Factor);
   --  The rounded products, padded outward.

   procedure Lemma_Rounded_Reciprocal_Scaling
     (Value : Interval; Factor : Positive_Endpoints; Rounded_Lower, Rounded_Upper, Lower, Upper : Dimensionless) is
   begin
      Lemma_Padded_Below (Rounded_Lower, Lower, R (Value.Lower) * R (Lower_Factor (Value, Factor)));
      Lemma_Padded_Above (Rounded_Upper, Upper, R (Value.Upper) * R (Upper_Factor (Value, Factor)));
   end Lemma_Rounded_Reciprocal_Scaling;

   function Scale_By_Reciprocal (Value : Interval; Factor : Positive_Endpoints) return Interval
   with
     Inline,
     Pre  =>
       Value.Valid
       and then abs Value.Lower <= Sum_Limit
       and then abs Value.Upper <= Sum_Limit
       and then Factor.Lower > 0.0
       and then Factor.Lower <= Factor.Upper
       and then Factor.Upper <= 1.0,
     Post => Is_Reciprocal_Scaling (Scale_By_Reciprocal'Result, Value, Factor);
   --  Enclose Value times the reciprocal of a binomial coefficient, which lies in (0, 1] and so cannot overflow or
   --  invalidate an interval. Each endpoint is scaled by whichever end of Factor moves it outward.

   function Scale_By_Reciprocal (Value : Interval; Factor : Positive_Endpoints) return Interval is
      pragma Suppress (Overflow_Check);
      Lower, Upper : Dimensionless;

      Rounded_Lower, Rounded_Upper : Dimensionless
      with Ghost;
      --  The products before they are padded.
   begin
      Lemma_Exact_Reciprocal_Scalings (Value, Factor);
      if (Factor.Lower = 1.0 and then Factor.Upper = 1.0) or else (Value.Lower = 0.0 and then Value.Upper = 0.0) then
         return Value;
      elsif Value.Lower = 1.0 and then Value.Upper = 1.0 then
         return (Lower => Factor.Lower, Upper => Factor.Upper, Valid => True);
      elsif Value.Lower = -1.0 and then Value.Upper = -1.0 then
         return (Lower => -Factor.Upper, Upper => -Factor.Lower, Valid => True);
      end if;
      Lemma_Shrinking_Product (Value.Lower, Lower_Factor (Value, Factor));
      Lemma_Shrinking_Product (Value.Upper, Upper_Factor (Value, Factor));
      Lower := Value.Lower * (if Value.Lower < 0.0 then Factor.Upper else Factor.Lower);
      Upper := Value.Upper * (if Value.Upper < 0.0 then Factor.Lower else Factor.Upper);
      Rounded_Lower := Lower;
      Rounded_Upper := Upper;
      Inflate_Finite_Endpoints (Lower, Upper);
      Lemma_Rounded_Reciprocal_Scaling (Value, Factor, Rounded_Lower, Rounded_Upper, Lower, Upper);
      return (Lower => Lower, Upper => Upper, Valid => True);
   end Scale_By_Reciprocal;

   procedure Lemma_Bound_Times_Coefficient (Bound, Value, Factor, Coefficient : Real)
   with
     Ghost,
     Pre  =>
       Coefficient >= 0.0
       and then (if Value >= 0.0 then Factor * Coefficient <= 1.0 else Factor * Coefficient >= 1.0)
       and then Bound <= Value * Factor,
     Post => Bound * Coefficient <= Value;
   --  A lower bound on Value times an end of the reciprocal's enclosure, the end being chosen by the sign of Value
   --  so that the product lies below Value over the coefficient, is a lower bound on Value once multiplied through.

   procedure Lemma_Bound_Times_Coefficient (Bound, Value, Factor, Coefficient : Real) is
   begin
      pragma Assert (Bound * Coefficient <= (Value * Factor) * Coefficient);
      pragma Assert ((Value * Factor) * Coefficient = Value * (Factor * Coefficient));
      pragma Assert (Value * (Factor * Coefficient) <= Value * 1.0);
   end Lemma_Bound_Times_Coefficient;

   procedure Lemma_Coefficient_Times_Bound (Bound, Value, Factor, Coefficient : Real)
   with
     Ghost,
     Pre  =>
       Coefficient >= 0.0
       and then (if Value >= 0.0 then Factor * Coefficient >= 1.0 else Factor * Coefficient <= 1.0)
       and then Value * Factor <= Bound,
     Post => Value <= Bound * Coefficient;
   --  Likewise for an upper bound.

   procedure Lemma_Coefficient_Times_Bound (Bound, Value, Factor, Coefficient : Real) is
   begin
      pragma Assert ((Value * Factor) * Coefficient <= Bound * Coefficient);
      pragma Assert ((Value * Factor) * Coefficient = Value * (Factor * Coefficient));
      pragma Assert (Value * 1.0 <= Value * (Factor * Coefficient));
   end Lemma_Coefficient_Times_Bound;

   procedure Lemma_Control_Encloses_Sums
     (Control, Sum : Interval; Factor : Positive_Endpoints; Left, Right : Polynomial; K : Degree_Range)
   with
     Ghost,
     Pre  =>
       Left.Degree + Right.Degree <= Maximum_Degree
       and then K <= Left.Degree + Right.Degree
       and then Is_Reciprocal_Enclosure (Factor, Left.Degree + Right.Degree, K)
       and then Is_Reciprocal_Scaling (Control, Sum, Factor)
       and then R (Sum.Lower) <= Low_Sum (Left, Right, K, Left.Degree + 1)
       and then High_Sum (Left, Right, K, Left.Degree + 1) <= R (Sum.Upper),
     Post => Control.Valid and then Encloses_Sums (Control, Left, Right, K);
   --  Scaling an enclosure of the sums by an enclosure of the reciprocal of the binomial coefficient gives a
   --  control which encloses the sums once multiplied through.

   procedure Lemma_Control_Encloses_Sums
     (Control, Sum : Interval; Factor : Positive_Endpoints; Left, Right : Polynomial; K : Degree_Range)
   is
      Coefficient : constant Real := Choose (Left.Degree + Right.Degree, K);
   begin
      Lemma_Bound_Times_Coefficient
        (Bound       => R (Control.Lower),
         Value       => R (Sum.Lower),
         Factor      => R (Lower_Factor (Sum, Factor)),
         Coefficient => Coefficient);
      Lemma_Coefficient_Times_Bound
        (Bound       => R (Control.Upper),
         Value       => R (Sum.Upper),
         Factor      => R (Upper_Factor (Sum, Factor)),
         Coefficient => Coefficient);
   end Lemma_Control_Encloses_Sums;

   ---------------------------------------------------------------------------
   --  Products
   ---------------------------------------------------------------------------

   procedure Lemma_Reciprocal_Is_Enclosure (N, K : Degree_Range)
   with
     Ghost,
     Pre  => K <= N,
     Post =>
       Is_Reciprocal_Enclosure (Reciprocals (N, K), N, K)
       and then Reciprocals (N, K).Lower > 0.0
       and then Reciprocals (N, K).Lower <= Reciprocals (N, K).Upper
       and then Reciprocals (N, K).Upper <= 1.0;
   --  The table of reciprocals is what Build_Reciprocals returned.

   procedure Lemma_Reciprocal_Is_Enclosure (N, K : Degree_Range) is null;

   procedure Lemma_Zero_Scaled (Scaled : Interval; X, Y : Real)
   with
     Ghost,
     Pre  =>
       abs Scaled.Lower <= 0.0
       and then abs Scaled.Upper <= 0.0
       and then Contains (Scaled, X)
       and then Contains (Scaled, Y),
     Post => X = 0.0 and then Y = 0.0;

   procedure Lemma_Zero_Scaled (Scaled : Interval; X, Y : Real) is
   begin
      pragma Assert (R (Scaled.Lower) = 0.0 and then R (Scaled.Upper) = 0.0);
   end Lemma_Zero_Scaled;

   procedure Lemma_Zero_Operand (Scaled : Interval_Controls; Value : Polynomial; Maximum : Dimensionless)
   with
     Ghost,
     Pre  =>
       (for all I in 0 .. Value.Degree => abs Scaled (I).Lower <= Maximum)
       and then (for all I in 0 .. Value.Degree => abs Scaled (I).Upper <= Maximum)
       and then (for all I in 0 .. Value.Degree => Is_Scaled_Control (Scaled (I), Value, I)),
     Post => (if Maximum = 0.0 then Is_Zero_Polynomial (Value));
   --  When the largest magnitude among the scaled controls of an operand is zero, so are the exactly scaled
   --  endpoints which they contain.

   procedure Lemma_Zero_Operand (Scaled : Interval_Controls; Value : Polynomial; Maximum : Dimensionless) is
   begin
      if Maximum = 0.0 then
         for I in 0 .. Value.Degree loop
            Lemma_Zero_Scaled (Scaled (I), Scaled_Lower (Value, I), Scaled_Upper (Value, I));
            pragma
              Loop_Invariant
                (for all J in 0 .. I => Scaled_Lower (Value, J) = 0.0 and then Scaled_Upper (Value, J) = 0.0);
         end loop;
      end if;
   end Lemma_Zero_Operand;

   function Encloses_Product (Result, Left, Right : Polynomial) return Boolean
   with
     Ghost,
     Pre  => Left.Degree + Right.Degree <= Maximum_Degree,
     Post =>
       Encloses_Product'Result
       = (if All_Valid (Result)
          then
            All_Valid (Left)
            and then All_Valid (Right)
            and then
              (for all K in 0 .. Left.Degree + Right.Degree => Encloses_Sums (Result.Controls (K), Left, Right, K)));
   --  What Multiply promises within this body: a result with only valid controls comes of operands with only
   --  valid controls, and each of its controls encloses the exact sums of its terms.

   function Encloses_Product (Result, Left, Right : Polynomial) return Boolean is
   begin
      return
        (if All_Valid (Result)
         then
           All_Valid (Left)
           and then All_Valid (Right)
           and then
             (for all K in 0 .. Left.Degree + Right.Degree => Encloses_Sums (Result.Controls (K), Left, Right, K)));
   end Encloses_Product;

   procedure Lemma_Invalid_Product (Left, Right : Polynomial; Degree : Degree_Range)
   with
     Ghost,
     Pre  => Left.Degree + Right.Degree <= Maximum_Degree,
     Post => Encloses_Product (Invalid_Polynomial (Degree), Left, Right);
   --  An invalid result promises nothing.

   procedure Lemma_Invalid_Product (Left, Right : Polynomial; Degree : Degree_Range) is
   begin
      pragma Assert (not Invalid_Polynomial (Degree).Controls (0).Valid);
      pragma Assert (not All_Valid (Invalid_Polynomial (Degree)));
   end Lemma_Invalid_Product;

   procedure Lemma_Valid_Product (Result, Left, Right : Polynomial)
   with
     Ghost,
     Pre  =>
       Left.Degree + Right.Degree <= Maximum_Degree
       and then All_Valid (Left)
       and then All_Valid (Right)
       and then (for all K in 0 .. Left.Degree + Right.Degree => Encloses_Sums (Result.Controls (K), Left, Right, K)),
     Post => Encloses_Product (Result, Left, Right);

   procedure Lemma_Valid_Product (Result, Left, Right : Polynomial) is null;

   procedure Lemma_Sum_Step
     (Old_Lower, Old_Upper, New_Lower, New_Upper : Dimensionless;
      Scaled_Left, Scaled_Right                  : Interval;
      Left, Right                                : Polynomial;
      K, I                                       : Degree_Range)
   with
     Ghost,
     Pre  =>
       I <= Left.Degree
       and then K >= I
       and then K - I <= Right.Degree
       and then Is_Scaled_Control (Scaled_Left, Left, I)
       and then Is_Scaled_Control (Scaled_Right, Right, K - I)
       and then R (Old_Lower) <= Low_Sum (Left, Right, K, I)
       and then High_Sum (Left, Right, K, I) <= R (Old_Upper)
       and then abs (R (Old_Lower)) <= Terms (I) * Term_Growth
       and then abs (R (Old_Upper)) <= Terms (I) * Term_Growth
       and then
         (if Is_Zero (Scaled_Left)
          then R (New_Lower) = R (Old_Lower) and then R (New_Upper) = R (Old_Upper)
          else Is_Accumulated (Old_Lower, Old_Upper, New_Lower, New_Upper, Scaled_Left, Scaled_Right)),
     Post =>
       R (New_Lower) <= Low_Sum (Left, Right, K, I + 1)
       and then High_Sum (Left, Right, K, I + 1) <= R (New_Upper)
       and then abs (R (New_Lower)) <= Terms (I + 1) * Term_Growth
       and then abs (R (New_Upper)) <= Terms (I + 1) * Term_Growth;
   --  One pass of the loop which sums the terms of a control keeps the partial sum around the exact partial sums,
   --  whether it adds a product or skips an exactly zero left control.

   procedure Lemma_Sum_Step
     (Old_Lower, Old_Upper, New_Lower, New_Upper : Dimensionless;
      Scaled_Left, Scaled_Right                  : Interval;
      Left, Right                                : Polynomial;
      K, I                                       : Degree_Range) is
   begin
      Lemma_Term_Is_Enclosed (Scaled_Left, Scaled_Right, Left, Right, K, I);
      pragma Assert (Low_Sum (Left, Right, K, I + 1) = Low_Sum (Left, Right, K, I) + Low_Term (Left, Right, K, I));
      pragma Assert (High_Sum (Left, Right, K, I + 1) = High_Sum (Left, Right, K, I) + High_Term (Left, Right, K, I));
      pragma Assert (Terms (I + 1) = Terms (I) + 1.0);
      pragma Assert (Terms (I + 1) * Term_Growth = Terms (I) * Term_Growth + Term_Growth);
   end Lemma_Sum_Step;

   function Multiply (Left, Right : Polynomial) return Polynomial
   with
     Refined_Post =>
       Multiply'Result.Degree = Left.Degree + Right.Degree and then Encloses_Product (Multiply'Result, Left, Right)
   is
      Degree       : constant Degree_Range := Left.Degree + Right.Degree;
      Scaled_Left  : Interval_Controls := [others => Zero];
      Scaled_Right : Interval_Controls := [others => Zero];
      Result       : Polynomial := (Degree => Degree, Controls => <>);
      Left_Max     : Dimensionless := 0.0;
      Right_Max    : Dimensionless := 0.0;
   begin
      for I in 0 .. Left.Degree loop
         Scaled_Left (I) := Scale_By_Binomial (Left.Controls (I), Binomial_Weight (Left.Degree, I));
         if not Scaled_Left (I).Valid then
            Lemma_Invalid_Product (Left, Right, Degree);
            return Invalid_Polynomial (Degree);
         end if;
         Lemma_Scaled_Control (Scaled_Left (I), Binomial_Weight (Left.Degree, I), Left, I);
         Left_Max := Dimensionless'Max (Left_Max, Interval_Abs_Max (Scaled_Left (I)));
         pragma Loop_Invariant (Left_Max >= 0.0);
         pragma Loop_Invariant (for all J in 0 .. I => Left.Controls (J).Valid);
         pragma Loop_Invariant (for all J in 0 .. I => Scaled_Left (J).Lower <= Scaled_Left (J).Upper);
         pragma Loop_Invariant (for all J in 0 .. I => abs Scaled_Left (J).Lower <= Left_Max);
         pragma Loop_Invariant (for all J in 0 .. I => abs Scaled_Left (J).Upper <= Left_Max);
         pragma Loop_Invariant (for all J in 0 .. I => Is_Scaled_Control (Scaled_Left (J), Left, J));
      end loop;
      pragma Assert (All_Valid (Left));
      for J in 0 .. Right.Degree loop
         Scaled_Right (J) := Scale_By_Binomial (Right.Controls (J), Binomial_Weight (Right.Degree, J));
         if not Scaled_Right (J).Valid then
            Lemma_Invalid_Product (Left, Right, Degree);
            return Invalid_Polynomial (Degree);
         end if;
         Lemma_Scaled_Control (Scaled_Right (J), Binomial_Weight (Right.Degree, J), Right, J);
         Right_Max := Dimensionless'Max (Right_Max, Interval_Abs_Max (Scaled_Right (J)));
         pragma Loop_Invariant (Right_Max >= 0.0);
         pragma Loop_Invariant (for all I in 0 .. J => Right.Controls (I).Valid);
         pragma Loop_Invariant (for all I in 0 .. J => Scaled_Right (I).Lower <= Scaled_Right (I).Upper);
         pragma Loop_Invariant (for all I in 0 .. J => abs Scaled_Right (I).Lower <= Right_Max);
         pragma Loop_Invariant (for all I in 0 .. J => abs Scaled_Right (I).Upper <= Right_Max);
         pragma Loop_Invariant (for all I in 0 .. J => Is_Scaled_Control (Scaled_Right (I), Right, I));
      end loop;
      pragma Assert (All_Valid (Right));
      --  Both operands have been validated before skipping zero terms. In particular, zero must never conceal an
      --  invalid coefficient in the other operand. The maxima also identify all-zero polynomials without rescanning.
      if Left_Max = 0.0 or else Right_Max = 0.0 then
         Lemma_Zero_Operand (Scaled_Left, Left, Left_Max);
         Lemma_Zero_Operand (Scaled_Right, Right, Right_Max);
         Lemma_Zero_Product (Left, Right, Result);
         Lemma_Valid_Product (Result, Left, Right);
         return Result;
      elsif not (Left_Max <= Scaled_Control_Limit) or else not (Right_Max <= Scaled_Control_Limit) then
         --  No product or partial sum below can overflow within the limit. The general product goes on to a
         --  slower formula beyond it. This one fails closed.
         Lemma_Invalid_Product (Left, Right, Degree);
         return Invalid_Polynomial (Degree);
      end if;
      for K in 0 .. Degree loop
         pragma Loop_Invariant (Result.Degree = Degree);
         pragma
           Loop_Invariant
             (for all J in 0 .. K - 1 =>
                Result.Controls (J).Valid and then Encloses_Sums (Result.Controls (J), Left, Right, J));
         declare
            Sum : Interval := Zero;

            Old_Lower, Old_Upper : Dimensionless
            with Ghost;
            --  The partial sum before each term.
         begin
            Lemma_Leading_Terms (Left, Right, K, Integer'Max (0, K - Right.Degree));
            --  The terms of one control are taken in order of the left index, as the general product takes them.
            for I in Integer'Max (0, K - Right.Degree) .. Integer'Min (Left.Degree, K) loop
               Old_Lower := Sum.Lower;
               Old_Upper := Sum.Upper;
               Lemma_Sum_Is_Within_Limit (Sum.Lower, I);
               Lemma_Sum_Is_Within_Limit (Sum.Upper, I);
               if Scaled_Left (I).Lower /= 0.0 or else Scaled_Left (I).Upper /= 0.0 then
                  Accumulate_Product (Sum, Scaled_Left (I), Scaled_Right (K - I));
               end if;
               Lemma_Sum_Step
                 (Old_Lower,
                  Old_Upper,
                  Sum.Lower,
                  Sum.Upper,
                  Scaled_Left (I),
                  Scaled_Right (K - I),
                  Left,
                  Right,
                  K,
                  I);
               pragma Loop_Invariant (Sum.Valid);
               pragma Loop_Invariant (R (Sum.Lower) <= Low_Sum (Left, Right, K, I + 1));
               pragma Loop_Invariant (High_Sum (Left, Right, K, I + 1) <= R (Sum.Upper));
               pragma Loop_Invariant (abs (R (Sum.Lower)) <= Terms (I + 1) * Term_Growth);
               pragma Loop_Invariant (abs (R (Sum.Upper)) <= Terms (I + 1) * Term_Growth);
            end loop;
            Lemma_Trailing_Terms (Left, Right, K, Left.Degree + 1);
            Lemma_Sum_Is_Within_Limit (Sum.Lower, Integer'Min (Left.Degree, K) + 1);
            Lemma_Sum_Is_Within_Limit (Sum.Upper, Integer'Min (Left.Degree, K) + 1);
            Lemma_Reciprocal_Is_Enclosure (Degree, K);
            Result.Controls (K) := Scale_By_Reciprocal (Sum, Reciprocals (Degree, K));
            Lemma_Control_Encloses_Sums (Result.Controls (K), Sum, Reciprocals (Degree, K), Left, Right, K);
         end;
      end loop;
      pragma
        Assert
          (for all J in 0 .. Degree =>
             Result.Controls (J).Valid and then Encloses_Sums (Result.Controls (J), Left, Right, J));
      Lemma_Valid_Product (Result, Left, Right);
      return Result;
   end Multiply;

   ---------------------------------------------------------------------------
   --  A product encloses the product of what its operands enclose
   ---------------------------------------------------------------------------

   procedure Lemma_Scaled_Contains (Value : Polynomial; Exact : Bernstein.Controls; Index : Degree_Range)
   with
     Ghost,
     Pre  => Encloses (Value, Exact) and then Index <= Value.Degree,
     Post =>
       Scaled_Lower (Value, Index) <= Choose (Value.Degree, Index) * Exact (Index)
       and then Choose (Value.Degree, Index) * Exact (Index) <= Scaled_Upper (Value, Index);
   --  The binomially scaled control of a real polynomial which Value encloses lies between the exactly scaled
   --  endpoints of the control of Value.

   procedure Lemma_Scaled_Contains (Value : Polynomial; Exact : Bernstein.Controls; Index : Degree_Range) is
   begin
      pragma Assert (Contains (Value.Controls (Index), Exact (Index)));
      Lemma_Scaling_Keeps_Order
        (Choose (Value.Degree, Index),
         R (Value.Controls (Index).Lower),
         Exact (Index),
         R (Value.Controls (Index).Upper));
   end Lemma_Scaled_Contains;

   procedure Lemma_Sums_Enclose
     (Left, Right : Polynomial; A, B : Bernstein.Controls; K : Degree_Range; Count : Natural)
   with
     Ghost,
     Subprogram_Variant => (Decreases => Count),
     Pre                => Encloses (Left, A) and then Encloses (Right, B) and then Count <= Left.Degree + 1,
     Post               =>
       Low_Sum (Left, Right, K, Count) <= Scaled_Convolution (A, B, K, Count)
       and then Scaled_Convolution (A, B, K, Count) <= High_Sum (Left, Right, K, Count);
   --  Term by term, the convolution of the binomially scaled controls of A and B lies between the least and the
   --  greatest corner products of the exactly scaled controls which contain them.

   procedure Lemma_Sums_Enclose
     (Left, Right : Polynomial; A, B : Bernstein.Controls; K : Degree_Range; Count : Natural)
   is
      I : constant Integer := Count - 1;
   begin
      if Count = 0 then
         return;
      end if;
      Lemma_Sums_Enclose (Left, Right, A, B, K, Count - 1);
      if K >= I and then K - I <= Right.Degree then
         Lemma_Scaled_Contains (Left, A, I);
         Lemma_Scaled_Contains (Right, B, K - I);
         Lemma_Product_Between_Corners
           (X    => Choose (A'Last, I) * A (I),
            A    => Scaled_Lower (Left, I),
            B    => Scaled_Upper (Left, I),
            Y    => Choose (B'Last, K - I) * B (K - I),
            C    => Scaled_Lower (Right, K - I),
            D    => Scaled_Upper (Right, K - I),
            Low  => Low_Term (Left, Right, K, I),
            High => High_Term (Left, Right, K, I));
         pragma
           Assert
             (Scaled_Convolution (A, B, K, Count)
              = Scaled_Convolution (A, B, K, Count - 1)
                + (Choose (A'Last, I) * A (I)) * (Choose (B'Last, K - I) * B (K - I)));
      else
         pragma Assert (Low_Term (Left, Right, K, I) = 0.0 and then High_Term (Left, Right, K, I) = 0.0);
         pragma Assert (Scaled_Convolution (A, B, K, Count) = Scaled_Convolution (A, B, K, Count - 1));
      end if;
   end Lemma_Sums_Enclose;

   procedure Lemma_Multiply_Encloses (Left, Right : Polynomial; A, B : Bernstein.Controls) is
      Degree : constant Degree_Range := Left.Degree + Right.Degree;
      Result : constant Polynomial := Multiply (Left, Right);
      Exact  : constant Bernstein.Controls := Product (A, B);
   begin
      pragma Assert (Encloses_Product (Result, Left, Right));
      pragma Assert (for all K in 0 .. Degree => Encloses_Sums (Result.Controls (K), Left, Right, K));
      for K in 0 .. Degree loop
         Lemma_Sums_Enclose (Left, Right, A, B, K, Left.Degree + 1);
         pragma Assert (Result.Controls (K).Valid);
         pragma Assert (Encloses_Sums (Result.Controls (K), Left, Right, K));
         pragma Assert (Exact (K) = Scaled_Convolution (A, B, K, A'Length) / Choose (Degree, K));
         Lemma_Quotient_Is_Between
           (Low     => R (Result.Controls (K).Lower),
            High    => R (Result.Controls (K).Upper),
            Value   => Scaled_Convolution (A, B, K, Left.Degree + 1),
            Divisor => Choose (Degree, K));
         pragma Loop_Invariant (for all J in 0 .. K => Contains (Result.Controls (J), Exact (J)));
      end loop;
   end Lemma_Multiply_Encloses;

end Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials;
