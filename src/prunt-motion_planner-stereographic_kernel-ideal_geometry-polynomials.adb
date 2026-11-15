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

package body Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Polynomials
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Real arithmetic, and the floating-point values which it describes
   ---------------------------------------------------------------------------

   procedure Lemma_Ratio_Shape (X, Top, Bottom : Real)
   with Ghost, Pre => Bottom /= 0.0, Post => X * (Top / Bottom) = X * Top / Bottom;
   --  A product with a ratio is the ratio of the product.

   procedure Lemma_Ratio_Shape (X, Top, Bottom : Real) is
      Ratio : constant Real := Top / Bottom;
   begin
      pragma Assert (Ratio * Bottom = Top);
      pragma Assert (X * Top = (X * Ratio) * Bottom);
   end Lemma_Ratio_Shape;

   procedure Lemma_Ratio_Cancels (X, Weight : Real)
   with Ghost, Pre => Weight /= 0.0, Post => X * Weight / Weight = X;

   procedure Lemma_Ratio_Cancels (X, Weight : Real) is null;

   procedure Lemma_Equal_Reals (Left, Right : Dimensionless)
   with Ghost, Pre => Left = Right, Post => R (Left) = R (Right);
   --  Equal floating-point values denote one real number.

   procedure Lemma_Equal_Reals (Left, Right : Dimensionless) is null;

   procedure Lemma_Order (Low, High : Dimensionless)
   with Ghost, Pre => R (Low) <= R (High), Post => Low <= High;
   --  Floating-point values compare as the real numbers which they denote.

   procedure Lemma_Order (Low, High : Dimensionless) is null;

   procedure Lemma_Exact (Value : Dimensionless)
   with
     Ghost,
     Post =>
       Interval_Exact (Value).Valid
       and then R (Interval_Exact (Value).Lower) = R (Value)
       and then R (Interval_Exact (Value).Upper) = R (Value);
   --  Every stored value is finite, so its exact enclosure is valid and is that one point.

   procedure Lemma_Exact (Value : Dimensionless) is null;

   procedure Lemma_Exact_Product (Left, Right : Dimensionless)
   with
     Ghost,
     Post =>
       (if Interval_Multiply (Interval_Exact (Left), Interval_Exact (Right)).Valid
        then Contains (Interval_Multiply (Interval_Exact (Left), Interval_Exact (Right)), R (Left) * R (Right)));

   procedure Lemma_Exact_Product (Left, Right : Dimensionless) is
   begin
      Lemma_Exact (Left);
      Lemma_Exact (Right);
      if Interval_Multiply (Interval_Exact (Left), Interval_Exact (Right)).Valid then
         Lemma_Multiply_Contains (Interval_Exact (Left), Interval_Exact (Right), R (Left), R (Right));
      end if;
   end Lemma_Exact_Product;

   procedure Lemma_One_Plus_Product (Left, Right : Dimensionless)
   with
     Ghost,
     Post =>
       (if Interval_Add (Interval_Exact (1.0), Interval_Multiply (Interval_Exact (Left), Interval_Exact (Right))).Valid
        then
          Contains
            (Interval_Add (Interval_Exact (1.0), Interval_Multiply (Interval_Exact (Left), Interval_Exact (Right))),
             1.0 + R (Left) * R (Right)));
   --  The enclosure of 1 + Left·Right, as the linear factor and the orientation form it.

   procedure Lemma_One_Plus_Product (Left, Right : Dimensionless) is
      Product : constant Interval := Interval_Multiply (Interval_Exact (Left), Interval_Exact (Right));
   begin
      Lemma_Exact (1.0);
      Lemma_Exact_Product (Left, Right);
      pragma Assert (R (1.0) = 1.0);
      if Interval_Add (Interval_Exact (1.0), Product).Valid then
         Lemma_Add_Contains (Interval_Exact (1.0), Product, 1.0, R (Left) * R (Right));
      end if;
   end Lemma_One_Plus_Product;

   ---------------------------------------------------------------------------
   --  Ratios of binomial coefficients
   ---------------------------------------------------------------------------

   procedure Lemma_Ratio_Of_Exact (Top_Value, Bottom_Value : Dimensionless; Top, Bottom : Real)
   with
     Ghost,
     Pre  => R (Top_Value) = Top and then R (Bottom_Value) = Bottom and then Bottom >= 1.0,
     Post =>
       (if Positive_Ratio (Top_Value, Bottom_Value).Valid
        then Contains (Positive_Ratio (Top_Value, Bottom_Value), Top / Bottom));
   --  The enclosed ratio of two floating-point values, in terms of the real numbers which they denote.

   procedure Lemma_Ratio_Of_Exact (Top_Value, Bottom_Value : Dimensionless; Top, Bottom : Real) is null;

   function Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K : Natural) return Interval
   with
     Inline,
     Pre  =>
       Top_N <= Maximum_Degree
       and then Top_K <= Top_N
       and then Bottom_N <= Maximum_Degree
       and then Bottom_K <= Bottom_N,
     Post =>
       Is_Ordered (Binomial_Ratio'Result)
       and then
         (if Binomial_Ratio'Result.Valid
          then
            Contains
              (Binomial_Ratio'Result,
               Bernstein.Binomial_Form.Choose (Top_N, Top_K) / Bernstein.Binomial_Form.Choose (Bottom_N, Bottom_K)));
   --  Enclose C(Top_N,Top_K)/C(Bottom_N,Bottom_K). This is the one place which reads the table of binomial
   --  coefficients, so that only its own proof has to carry the table.

   function Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K : Natural) return Interval is
   begin
      Lemma_Binomials_Are_Exact (Top_N, Top_K);
      Lemma_Binomials_Are_Exact (Bottom_N, Bottom_K);
      Lemma_Ratio_Of_Exact
        (Binomial (Top_N, Top_K),
         Binomial (Bottom_N, Bottom_K),
         Bernstein.Binomial_Form.Choose (Top_N, Top_K),
         Bernstein.Binomial_Form.Choose (Bottom_N, Bottom_K));
      return Positive_Ratio (Binomial (Top_N, Top_K), Binomial (Bottom_N, Bottom_K));
   end Binomial_Ratio;

   ---------------------------------------------------------------------------
   --  What one control of a controlwise operation encloses
   ---------------------------------------------------------------------------

   --  These predicates are ordinary functions with a postcondition, not expression functions, so that a loop
   --  invariant which states one of every control is carried from one iteration to the next as it stands.

   function Is_Product_Enclosure (Result, Left, Right : Interval) return Boolean
   with
     Ghost,
     Post =>
       Is_Product_Enclosure'Result
       = (if Result.Valid
          then
            Left.Valid
            and then Right.Valid
            and then
              (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
               then Encloses_Products (Result, Left, Right)));
   --  What Interval_Multiply promises of its result.

   function Is_Product_Enclosure (Result, Left, Right : Interval) return Boolean is
   begin
      return
        (if Result.Valid
         then
           Left.Valid
           and then Right.Valid
           and then
             (if Left.Lower <= Left.Upper and then Right.Lower <= Right.Upper
              then Encloses_Products (Result, Left, Right)));
   end Is_Product_Enclosure;

   function Is_Sum_Enclosure (Result, Left, Right : Interval) return Boolean
   with
     Ghost,
     Post =>
       Is_Sum_Enclosure'Result
       = (if Result.Valid
          then
            Left.Valid
            and then Right.Valid
            and then R (Result.Lower) <= R (Left.Lower) + R (Right.Lower)
            and then R (Left.Upper) + R (Right.Upper) <= R (Result.Upper));
   --  What Interval_Add promises of its result.

   function Is_Sum_Enclosure (Result, Left, Right : Interval) return Boolean is
   begin
      return
        (if Result.Valid
         then
           Left.Valid
           and then Right.Valid
           and then R (Result.Lower) <= R (Left.Lower) + R (Right.Lower)
           and then R (Left.Upper) + R (Right.Upper) <= R (Result.Upper));
   end Is_Sum_Enclosure;

   function Is_Copy (Result, Source : Interval) return Boolean
   with
     Ghost,
     Post =>
       Is_Copy'Result
       = (Result.Valid = Source.Valid
          and then R (Result.Lower) = R (Source.Lower)
          and then R (Result.Upper) = R (Source.Upper));
   --  Whether two intervals have the same members and validity.

   function Is_Copy (Result, Source : Interval) return Boolean is
   begin
      return
        Result.Valid = Source.Valid
        and then R (Result.Lower) = R (Source.Lower)
        and then R (Result.Upper) = R (Source.Upper);
   end Is_Copy;

   function Is_Exact_Zero (Value : Interval) return Boolean
   with
     Ghost,
     Post => Is_Exact_Zero'Result = (Value.Valid and then R (Value.Lower) = 0.0 and then R (Value.Upper) = 0.0);

   function Is_Exact_Zero (Value : Interval) return Boolean is
   begin
      return Value.Valid and then R (Value.Lower) = 0.0 and then R (Value.Upper) = 0.0;
   end Is_Exact_Zero;

   function Is_Weighted_Enclosure
     (Result, Source : Interval; Top_N, Top_K, Bottom_N, Bottom_K : Degree_Range) return Boolean
   with
     Ghost,
     Pre  => Top_K <= Top_N and then Bottom_K <= Bottom_N,
     Post =>
       Is_Weighted_Enclosure'Result
       = Is_Product_Enclosure (Result, Source, Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K));
   --  What a control of Raised or of Quotient is to the interval it came from: the enclosure of its products with
   --  the enclosed ratio of two binomial coefficients.

   function Is_Weighted_Enclosure
     (Result, Source : Interval; Top_N, Top_K, Bottom_N, Bottom_K : Degree_Range) return Boolean is
   begin
      return Is_Product_Enclosure (Result, Source, Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K));
   end Is_Weighted_Enclosure;

   procedure Lemma_Multiply_Is_Enclosure (Left, Right : Interval)
   with
     Ghost,
     Post =>
       Is_Product_Enclosure (Interval_Multiply (Left, Right), Left, Right)
       and then (if Interval_Multiply (Left, Right).Valid then Left.Valid and then Right.Valid);

   procedure Lemma_Multiply_Is_Enclosure (Left, Right : Interval) is null;

   procedure Lemma_Add_Is_Enclosure (Left, Right : Interval)
   with
     Ghost,
     Post =>
       Is_Sum_Enclosure (Interval_Add (Left, Right), Left, Right)
       and then (if Interval_Add (Left, Right).Valid then Left.Valid and then Right.Valid);

   procedure Lemma_Add_Is_Enclosure (Left, Right : Interval) is null;

   procedure Lemma_Multiply_Is_Weighted (Source : Interval; Top_N, Top_K, Bottom_N, Bottom_K : Degree_Range)
   with
     Ghost,
     Pre  => Top_K <= Top_N and then Bottom_K <= Bottom_N,
     Post =>
       Is_Weighted_Enclosure
         (Interval_Multiply (Source, Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K)),
          Source,
          Top_N,
          Top_K,
          Bottom_N,
          Bottom_K)
       and then
         (if Interval_Multiply (Source, Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K)).Valid then Source.Valid);

   procedure Lemma_Multiply_Is_Weighted (Source : Interval; Top_N, Top_K, Bottom_N, Bottom_K : Degree_Range) is
   begin
      Lemma_Multiply_Is_Enclosure (Source, Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K));
   end Lemma_Multiply_Is_Weighted;

   procedure Lemma_Copy_Contains (Result, Source : Interval; X : Real)
   with Ghost, Pre => Is_Copy (Result, Source) and then Contains (Source, X), Post => Contains (Result, X);

   procedure Lemma_Copy_Contains (Result, Source : Interval; X : Real) is null;

   procedure Lemma_Zero_Contains (Result : Interval)
   with Ghost, Pre => Is_Exact_Zero (Result), Post => Contains (Result, 0.0);

   procedure Lemma_Zero_Contains (Result : Interval) is null;

   procedure Lemma_Product_Contains (Result, Left, Right : Interval; X, Y : Real)
   with
     Ghost,
     Pre  =>
       Result.Valid
       and then Is_Product_Enclosure (Result, Left, Right)
       and then Contains (Left, X)
       and then Contains (Right, Y),
     Post => Contains (Result, X * Y);
   --  A product of members is a member of the enclosure of the corner products.

   procedure Lemma_Product_Contains (Result, Left, Right : Interval; X, Y : Real) is
   begin
      Lemma_Order (Left.Lower, Left.Upper);
      Lemma_Order (Right.Lower, Right.Upper);
      Lemma_Product_Between_Corners
        (X    => X,
         A    => R (Left.Lower),
         B    => R (Left.Upper),
         Y    => Y,
         C    => R (Right.Lower),
         D    => R (Right.Upper),
         Low  => R (Result.Lower),
         High => R (Result.Upper));
   end Lemma_Product_Contains;

   procedure Lemma_Sum_Contains (Result, Left, Right : Interval; X, Y : Real)
   with
     Ghost,
     Pre  =>
       Result.Valid
       and then Is_Sum_Enclosure (Result, Left, Right)
       and then Contains (Left, X)
       and then Contains (Right, Y),
     Post => Contains (Result, X + Y);

   procedure Lemma_Sum_Contains (Result, Left, Right : Interval; X, Y : Real) is null;

   procedure Lemma_Weighted_Contains
     (Result, Source : Interval; Top_N, Top_K, Bottom_N, Bottom_K : Degree_Range; X : Real)
   with
     Ghost,
     Pre  =>
       Top_K <= Top_N
       and then Bottom_K <= Bottom_N
       and then Result.Valid
       and then Is_Weighted_Enclosure (Result, Source, Top_N, Top_K, Bottom_N, Bottom_K)
       and then Contains (Source, X),
     Post =>
       Contains
         (Result,
          X * Bernstein.Binomial_Form.Choose (Top_N, Top_K) / Bernstein.Binomial_Form.Choose (Bottom_N, Bottom_K));
   --  An enclosure of the products of Source with the enclosed ratio of two binomial coefficients holds every
   --  member of Source weighted by that ratio.

   procedure Lemma_Weighted_Contains
     (Result, Source : Interval; Top_N, Top_K, Bottom_N, Bottom_K : Degree_Range; X : Real)
   is
      Ratio  : constant Interval := Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K);
      Top    : constant Real := Bernstein.Binomial_Form.Choose (Top_N, Top_K);
      Bottom : constant Real := Bernstein.Binomial_Form.Choose (Bottom_N, Bottom_K);
   begin
      pragma Assert (Is_Product_Enclosure (Result, Source, Ratio));
      pragma Assert (Ratio.Valid);
      pragma Assert (Contains (Ratio, Top / Bottom));
      Lemma_Product_Contains (Result, Source, Ratio, X, Top / Bottom);
      Lemma_Ratio_Shape (X, Top, Bottom);
   end Lemma_Weighted_Contains;

   ---------------------------------------------------------------------------
   --  The chart
   ---------------------------------------------------------------------------

   function Chart_Polynomial (Coefficients : Bernstein_Chart; Component : Chart_Component_Index) return Polynomial is
      Result : Polynomial := (Degree => Fixed_Chart_Degree, Controls => <>);
   begin
      for I in Chart_Coefficient_Index loop
         Result.Controls (I) := Interval_Exact (Coefficients (I, Component));
         Lemma_Exact (Coefficients (I, Component));
         pragma
           Loop_Invariant (for all K in 0 .. I => Contains (Result.Controls (K), R (Coefficients (K, Component))));
      end loop;
      return Result;
   end Chart_Polynomial;

   ---------------------------------------------------------------------------
   --  Endpoint roots and their quotients
   ---------------------------------------------------------------------------

   function Root_Order
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; At_Start : Boolean) return Root_Order_Range
   is
      Endpoint : constant Chart_Coefficient_Index := (if At_Start then 0 else Fixed_Chart_Degree);
      Order    : Root_Order_Range := 1;
   begin
      while Order < 4
        and then
          Coefficients ((if At_Start then Order else Fixed_Chart_Degree - Order), Component)
          = Coefficients (Endpoint, Component)
      loop
         Order := Order + 1;
         pragma Loop_Invariant (Has_Root_Order (Coefficients, Component, Order, At_Start));
         pragma Loop_Variant (Increases => Order);
      end loop;
      return Order;
   end Root_Order;

   function Planar_Factor_Order (Coefficients : Bernstein_Chart; At_Start : Boolean) return Root_Order_Range is
   begin
      if At_Start then
         return (if (for all I in 1 .. 3 => Coefficients (I, 1) = Coefficients (0, 1)) then 4 else 1);
      else
         --  The controls 5 .. 7 are those at distances 3 .. 1 from the finish.
         pragma
           Assert
             (if (for all I in 5 .. Fixed_Chart_Degree - 1 =>
                    Coefficients (I, 1) = Coefficients (Fixed_Chart_Degree, 1))
              then
                Coefficients (Fixed_Chart_Degree - 1, 1) = Coefficients (Fixed_Chart_Degree, 1)
                and then Coefficients (Fixed_Chart_Degree - 2, 1) = Coefficients (Fixed_Chart_Degree, 1)
                and then Coefficients (Fixed_Chart_Degree - 3, 1) = Coefficients (Fixed_Chart_Degree, 1));
         return
           (if (for all I in 5 .. Fixed_Chart_Degree - 1 => Coefficients (I, 1) = Coefficients (Fixed_Chart_Degree, 1))
            then 4
            else 1);
      end if;
   end Planar_Factor_Order;

   function Quotient_Control
     (Coefficients : Bernstein_Chart;
      Component    : Chart_Component_Index;
      Order        : Root_Order_Range;
      At_Start     : Boolean;
      J            : Natural) return Real
   is (if At_Start
       then
         (R (Coefficients (J + Order, Component)) - R (Coefficients (0, Component)))
         * Bernstein.Binomial_Form.Choose (Fixed_Chart_Degree, J + Order)
         / Bernstein.Binomial_Form.Choose (Fixed_Chart_Degree - Order, J)
       else
         (R (Coefficients (Fixed_Chart_Degree, Component)) - R (Coefficients (J, Component)))
         * Bernstein.Binomial_Form.Choose (Fixed_Chart_Degree, J)
         / Bernstein.Binomial_Form.Choose (Fixed_Chart_Degree - Order, J))
   with Ghost, Pre => J <= Fixed_Chart_Degree - Order;
   --  Control J of Quotient_Controls, in terms of the stored coefficients.

   procedure Lemma_Root_Order_Controls
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; At_Start : Boolean)
   with
     Ghost,
     Pre  => Has_Root_Order (Coefficients, Component, Order, At_Start),
     Post =>
       (if At_Start
        then
          (for all I in 1 .. Order - 1 =>
             Chart_Controls (Coefficients, Component) (I) = Chart_Controls (Coefficients, Component) (0))
        else
          (for all I in Fixed_Chart_Degree - Order + 1 .. Fixed_Chart_Degree - 1 =>
             Chart_Controls (Coefficients, Component) (I)
             = Chart_Controls (Coefficients, Component) (Fixed_Chart_Degree)));
   --  Equal stored coefficients are equal real controls, which is what the real quotients require.

   procedure Lemma_Root_Order_Controls
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; At_Start : Boolean)
   is
      Chart : constant Bernstein.Controls := Chart_Controls (Coefficients, Component);
   begin
      for I in 1 .. Order - 1 loop
         pragma
           Assert
             (Coefficients ((if At_Start then I else Fixed_Chart_Degree - I), Component)
              = Coefficients ((if At_Start then 0 else Fixed_Chart_Degree), Component));
         if At_Start then
            Lemma_Equal_Reals (Coefficients (I, Component), Coefficients (0, Component));
         else
            Lemma_Equal_Reals
              (Coefficients (Fixed_Chart_Degree - I, Component), Coefficients (Fixed_Chart_Degree, Component));
         end if;
         pragma
           Loop_Invariant
             (if At_Start
              then (for all K in 1 .. I => Chart (K) = Chart (0))
              else
                (for all K in Fixed_Chart_Degree - I .. Fixed_Chart_Degree - 1 =>
                   Chart (K) = Chart (Fixed_Chart_Degree)));
      end loop;
   end Lemma_Root_Order_Controls;

   function Quotient_Controls
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; At_Start : Boolean)
      return Bernstein.Controls
   with
     Refined_Post =>
       Quotient_Controls'Result'First = 0
       and then Quotient_Controls'Result'Last = Fixed_Chart_Degree - Order
       and then
         (for all J in 0 .. Fixed_Chart_Degree - Order =>
            Quotient_Controls'Result (J) = Quotient_Control (Coefficients, Component, Order, At_Start, J))
   is
   begin
      Lemma_Root_Order_Controls (Coefficients, Component, Order, At_Start);
      if At_Start then
         return Bernstein.Binomial_Form.Start_Quotient (Chart_Controls (Coefficients, Component), Order);
      else
         return Bernstein.Binomial_Form.Finish_Quotient (Chart_Controls (Coefficients, Component), Order);
      end if;
   end Quotient_Controls;

   procedure Lemma_Quotient_Value
     (Coefficients : Bernstein_Chart;
      Component    : Chart_Component_Index;
      Order        : Root_Order_Range;
      At_Start     : Boolean;
      V            : Real)
   is
      Chart : constant Bernstein.Controls := Chart_Controls (Coefficients, Component);
      Exact : constant Bernstein.Controls := Quotient_Controls (Coefficients, Component, Order, At_Start);
   begin
      Lemma_Root_Order_Controls (Coefficients, Component, Order, At_Start);
      if At_Start then
         declare
            Divided : constant Bernstein.Controls := Bernstein.Binomial_Form.Start_Quotient (Chart, Order);
         begin
            for J in 0 .. Fixed_Chart_Degree - Order loop
               pragma Assert (Exact (J) = Quotient_Control (Coefficients, Component, Order, At_Start, J));
               pragma Loop_Invariant (for all K in 0 .. J => Exact (K) = Divided (K));
            end loop;
            Bernstein.Lemma_Value_Ignores_Offset (Exact, Divided, V);
            Bernstein.Binomial_Form.Lemma_Start_Quotient (Chart, Order, V);
         end;
      else
         declare
            Divided : constant Bernstein.Controls := Bernstein.Binomial_Form.Finish_Quotient (Chart, Order);
         begin
            for J in 0 .. Fixed_Chart_Degree - Order loop
               pragma Assert (Exact (J) = Quotient_Control (Coefficients, Component, Order, At_Start, J));
               pragma Loop_Invariant (for all K in 0 .. J => Exact (K) = Divided (K));
            end loop;
            Bernstein.Lemma_Value_Ignores_Offset (Exact, Divided, V);
            Bernstein.Binomial_Form.Lemma_Finish_Quotient (Chart, Order, V);
         end;
      end if;
   end Lemma_Quotient_Value;

   procedure Lemma_Quotient_Step (Minuend, Subtrahend : Dimensionless; Top_N, Top_K, Bottom_N, Bottom_K : Degree_Range)
   with
     Ghost,
     Pre  => Top_K <= Top_N and then Bottom_K <= Bottom_N,
     Post =>
       (if Interval_Multiply
             (Interval_Subtract (Interval_Exact (Minuend), Interval_Exact (Subtrahend)),
              Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K))
             .Valid
        then
          Contains
            (Interval_Multiply
               (Interval_Subtract (Interval_Exact (Minuend), Interval_Exact (Subtrahend)),
                Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K)),
             (R (Minuend) - R (Subtrahend))
             * Bernstein.Binomial_Form.Choose (Top_N, Top_K)
             / Bernstein.Binomial_Form.Choose (Bottom_N, Bottom_K)));
   --  One control of Quotient, when valid, holds the difference of the two stored coefficients weighted by the
   --  ratio of the two binomial coefficients.

   procedure Lemma_Quotient_Step (Minuend, Subtrahend : Dimensionless; Top_N, Top_K, Bottom_N, Bottom_K : Degree_Range)
   is
      Difference : constant Interval := Interval_Subtract (Interval_Exact (Minuend), Interval_Exact (Subtrahend));
      Ratio      : constant Interval := Binomial_Ratio (Top_N, Top_K, Bottom_N, Bottom_K);
      Result     : constant Interval := Interval_Multiply (Difference, Ratio);
   begin
      if Result.Valid then
         Lemma_Exact (Minuend);
         Lemma_Exact (Subtrahend);
         Lemma_Subtract_Contains (Interval_Exact (Minuend), Interval_Exact (Subtrahend), R (Minuend), R (Subtrahend));
         Lemma_Multiply_Is_Weighted (Difference, Top_N, Top_K, Bottom_N, Bottom_K);
         Lemma_Weighted_Contains (Result, Difference, Top_N, Top_K, Bottom_N, Bottom_K, R (Minuend) - R (Subtrahend));
      end if;
   end Lemma_Quotient_Step;

   function Encloses_Quotient_Control
     (Control      : Interval;
      Coefficients : Bernstein_Chart;
      Component    : Chart_Component_Index;
      Order        : Root_Order_Range;
      At_Start     : Boolean;
      J            : Natural) return Boolean
   with
     Ghost,
     Pre  => J <= Fixed_Chart_Degree - Order,
     Post =>
       Encloses_Quotient_Control'Result
       = (if Control.Valid then Contains (Control, Quotient_Control (Coefficients, Component, Order, At_Start, J)));
   --  Whether a control of Quotient, when valid, holds the matching real control.

   function Encloses_Quotient_Control
     (Control      : Interval;
      Coefficients : Bernstein_Chart;
      Component    : Chart_Component_Index;
      Order        : Root_Order_Range;
      At_Start     : Boolean;
      J            : Natural) return Boolean is
   begin
      return
        (if Control.Valid then Contains (Control, Quotient_Control (Coefficients, Component, Order, At_Start, J)));
   end Encloses_Quotient_Control;

   procedure Lemma_Quotient_Step_At_Start
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; J : Natural)
   with
     Ghost,
     Pre  => J <= Fixed_Chart_Degree - Order,
     Post =>
       Encloses_Quotient_Control
         (Interval_Multiply
            (Interval_Subtract
               (Interval_Exact (Coefficients (J + Order, Component)), Interval_Exact (Coefficients (0, Component))),
             Binomial_Ratio (Fixed_Chart_Degree, J + Order, Fixed_Chart_Degree - Order, J)),
          Coefficients,
          Component,
          Order,
          True,
          J);

   procedure Lemma_Quotient_Step_At_Start
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; J : Natural) is
   begin
      Lemma_Quotient_Step
        (Coefficients (J + Order, Component),
         Coefficients (0, Component),
         Fixed_Chart_Degree,
         J + Order,
         Fixed_Chart_Degree - Order,
         J);
   end Lemma_Quotient_Step_At_Start;

   procedure Lemma_Quotient_Step_At_Finish
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; J : Natural)
   with
     Ghost,
     Pre  => J <= Fixed_Chart_Degree - Order,
     Post =>
       Encloses_Quotient_Control
         (Interval_Multiply
            (Interval_Subtract
               (Interval_Exact (Coefficients (Fixed_Chart_Degree, Component)),
                Interval_Exact (Coefficients (J, Component))),
             Binomial_Ratio (Fixed_Chart_Degree, J, Fixed_Chart_Degree - Order, J)),
          Coefficients,
          Component,
          Order,
          False,
          J);

   procedure Lemma_Quotient_Step_At_Finish
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; J : Natural) is
   begin
      Lemma_Quotient_Step
        (Coefficients (Fixed_Chart_Degree, Component),
         Coefficients (J, Component),
         Fixed_Chart_Degree,
         J,
         Fixed_Chart_Degree - Order,
         J);
   end Lemma_Quotient_Step_At_Finish;

   procedure Lemma_Quotient_Encloses
     (Result       : Polynomial;
      Coefficients : Bernstein_Chart;
      Component    : Chart_Component_Index;
      Order        : Root_Order_Range;
      At_Start     : Boolean)
   with
     Ghost,
     Pre  =>
       Result.Degree = Fixed_Chart_Degree - Order
       and then
         (for all K in 0 .. Fixed_Chart_Degree - Order =>
            Encloses_Quotient_Control (Result.Controls (K), Coefficients, Component, Order, At_Start, K)),
     Post =>
       (if All_Valid (Result) and then Has_Root_Order (Coefficients, Component, Order, At_Start)
        then Encloses (Result, Quotient_Controls (Coefficients, Component, Order, At_Start)));

   procedure Lemma_Quotient_Encloses
     (Result       : Polynomial;
      Coefficients : Bernstein_Chart;
      Component    : Chart_Component_Index;
      Order        : Root_Order_Range;
      At_Start     : Boolean) is
   begin
      if All_Valid (Result) and then Has_Root_Order (Coefficients, Component, Order, At_Start) then
         declare
            Exact : constant Bernstein.Controls := Quotient_Controls (Coefficients, Component, Order, At_Start);
         begin
            for K in 0 .. Fixed_Chart_Degree - Order loop
               pragma Assert (Exact (K) = Quotient_Control (Coefficients, Component, Order, At_Start, K));
               pragma
                 Assert (Encloses_Quotient_Control (Result.Controls (K), Coefficients, Component, Order, At_Start, K));
               pragma Loop_Invariant (for all J in 0 .. K => Contains (Result.Controls (J), Exact (J)));
            end loop;
         end;
      end if;
   end Lemma_Quotient_Encloses;

   function Quotient
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; At_Start : Boolean)
      return Polynomial
   is
      Result : Polynomial := (Degree => Fixed_Chart_Degree - Order, Controls => <>);
   begin
      for J in 0 .. Fixed_Chart_Degree - Order loop
         if At_Start then
            Result.Controls (J) :=
              Interval_Multiply
                (Interval_Subtract
                   (Interval_Exact (Coefficients (J + Order, Component)),
                    Interval_Exact (Coefficients (0, Component))),
                 Binomial_Ratio (Fixed_Chart_Degree, J + Order, Fixed_Chart_Degree - Order, J));
            Lemma_Quotient_Step_At_Start (Coefficients, Component, Order, J);
            --  The invariant is first stated inside each branch, where the one assignment made is known.
            pragma
              Assert
                (for all K in 0 .. J =>
                   Encloses_Quotient_Control (Result.Controls (K), Coefficients, Component, Order, At_Start, K));
         else
            Result.Controls (J) :=
              Interval_Multiply
                (Interval_Subtract
                   (Interval_Exact (Coefficients (Fixed_Chart_Degree, Component)),
                    Interval_Exact (Coefficients (J, Component))),
                 Binomial_Ratio (Fixed_Chart_Degree, J, Fixed_Chart_Degree - Order, J));
            Lemma_Quotient_Step_At_Finish (Coefficients, Component, Order, J);
            pragma
              Assert
                (for all K in 0 .. J =>
                   Encloses_Quotient_Control (Result.Controls (K), Coefficients, Component, Order, At_Start, K));
         end if;
         pragma
           Loop_Invariant
             (for all K in 0 .. J =>
                Encloses_Quotient_Control (Result.Controls (K), Coefficients, Component, Order, At_Start, K));
      end loop;
      Lemma_Quotient_Encloses (Result, Coefficients, Component, Order, At_Start);
      return Result;
   end Quotient;

   ---------------------------------------------------------------------------
   --  The factor 1 + Value·Y₁
   ---------------------------------------------------------------------------

   function Linear_Factor_Controls (Coefficients : Bernstein_Chart; Value : Dimensionless) return Bernstein.Controls is
      Result : Bernstein.Controls (0 .. Fixed_Chart_Degree) := [others => 0.0];
   begin
      for I in Chart_Coefficient_Index loop
         Result (I) := 1.0 + R (Value) * R (Coefficients (I, 1));
         pragma Loop_Invariant (for all J in 0 .. I => Result (J) = 1.0 + R (Value) * R (Coefficients (J, 1)));
      end loop;
      return Result;
   end Linear_Factor_Controls;

   procedure Lemma_Linear_Factor_Value (Coefficients : Bernstein_Chart; Value : Dimensionless; V : Real) is
      Chart        : constant Bernstein.Controls := Chart_Controls (Coefficients, 1);
      Linear       : constant Bernstein.Controls := Linear_Factor_Controls (Coefficients, Value);
      Ones         : constant Bernstein.Controls (0 .. Fixed_Chart_Degree) := [others => 1.0];
      Scaled_Chart : Bernstein.Controls (0 .. Fixed_Chart_Degree) := [others => 0.0];
   begin
      --  The factor is the constant one plus Value times the chart coordinate, control by control.
      for I in Chart_Coefficient_Index loop
         Scaled_Chart (I) := R (Value) * Chart (I);
         pragma Loop_Invariant (for all J in 0 .. I => Scaled_Chart (J) = R (Value) * Chart (J));
      end loop;
      Bernstein.Binomial_Form.Lemma_Value_Of_Constant (Ones, 1.0, V);
      Bernstein.Lemma_Value_Is_Homogeneous (R (Value), Chart, Scaled_Chart, V);
      pragma Assert (for all I in Chart_Coefficient_Index => Linear (I) = Ones (I) + Scaled_Chart (I));
      Bernstein.Lemma_Value_Is_Additive (Ones, Scaled_Chart, Linear, V);
   end Lemma_Linear_Factor_Value;

   function Encloses_Linear_Control
     (Control : Interval; Coefficients : Bernstein_Chart; Value : Dimensionless; K : Chart_Coefficient_Index)
      return Boolean
   with
     Ghost,
     Post =>
       Encloses_Linear_Control'Result
       = (if Control.Valid then Contains (Control, 1.0 + R (Value) * R (Coefficients (K, 1))));
   --  Whether a control of Linear_Factor, when valid, holds the matching real control.

   function Encloses_Linear_Control
     (Control : Interval; Coefficients : Bernstein_Chart; Value : Dimensionless; K : Chart_Coefficient_Index)
      return Boolean is
   begin
      return (if Control.Valid then Contains (Control, 1.0 + R (Value) * R (Coefficients (K, 1))));
   end Encloses_Linear_Control;

   procedure Lemma_Linear_Step (Coefficients : Bernstein_Chart; Value : Dimensionless; K : Chart_Coefficient_Index)
   with
     Ghost,
     Post =>
       Encloses_Linear_Control
         (Interval_Add
            (Interval_Exact (1.0), Interval_Multiply (Interval_Exact (Value), Interval_Exact (Coefficients (K, 1)))),
          Coefficients,
          Value,
          K);

   procedure Lemma_Linear_Step (Coefficients : Bernstein_Chart; Value : Dimensionless; K : Chart_Coefficient_Index) is
   begin
      Lemma_One_Plus_Product (Value, Coefficients (K, 1));
   end Lemma_Linear_Step;

   procedure Lemma_Linear_Factor_Encloses (Result : Polynomial; Coefficients : Bernstein_Chart; Value : Dimensionless)
   with
     Ghost,
     Pre  =>
       Result.Degree = Fixed_Chart_Degree
       and then
         (for all K in Chart_Coefficient_Index =>
            Encloses_Linear_Control (Result.Controls (K), Coefficients, Value, K)),
     Post => (if All_Valid (Result) then Encloses (Result, Linear_Factor_Controls (Coefficients, Value)));

   procedure Lemma_Linear_Factor_Encloses (Result : Polynomial; Coefficients : Bernstein_Chart; Value : Dimensionless)
   is
   begin
      if All_Valid (Result) then
         declare
            Exact : constant Bernstein.Controls := Linear_Factor_Controls (Coefficients, Value);
         begin
            for K in Chart_Coefficient_Index loop
               pragma Assert (Encloses_Linear_Control (Result.Controls (K), Coefficients, Value, K));
               pragma Assert (Exact (K) = 1.0 + R (Value) * R (Coefficients (K, 1)));
               pragma Loop_Invariant (for all J in 0 .. K => Contains (Result.Controls (J), Exact (J)));
            end loop;
         end;
      end if;
   end Lemma_Linear_Factor_Encloses;

   function Linear_Factor (Coefficients : Bernstein_Chart; Value : Dimensionless) return Polynomial is
      Result : Polynomial := (Degree => Fixed_Chart_Degree, Controls => <>);
   begin
      for I in Chart_Coefficient_Index loop
         Result.Controls (I) :=
           Interval_Add
             (Interval_Exact (1.0), Interval_Multiply (Interval_Exact (Value), Interval_Exact (Coefficients (I, 1))));
         Lemma_Linear_Step (Coefficients, Value, I);
         pragma
           Loop_Invariant
             (for all K in 0 .. I => Encloses_Linear_Control (Result.Controls (K), Coefficients, Value, K));
      end loop;
      Lemma_Linear_Factor_Encloses (Result, Coefficients, Value);
      return Result;
   end Linear_Factor;

   ---------------------------------------------------------------------------
   --  Controlwise operations
   ---------------------------------------------------------------------------

   function Scaled (Factor : Interval; Value : Polynomial) return Polynomial
   with
     Refined_Post =>
       Scaled'Result.Degree = Value.Degree
       and then (if All_Valid (Scaled'Result) then Factor.Valid and then All_Valid (Value))
       and then
         (for all K in 0 .. Value.Degree =>
            Is_Product_Enclosure (Scaled'Result.Controls (K), Factor, Value.Controls (K)))
   is
      Result : Polynomial := (Degree => Value.Degree, Controls => <>);
   begin
      for K in 0 .. Value.Degree loop
         Result.Controls (K) := Interval_Multiply (Factor, Value.Controls (K));
         Lemma_Multiply_Is_Enclosure (Factor, Value.Controls (K));
         pragma
           Loop_Invariant
             (for all J in 0 .. K => Is_Product_Enclosure (Result.Controls (J), Factor, Value.Controls (J)));
         pragma
           Loop_Invariant
             (for all J in 0 .. K =>
                (if Result.Controls (J).Valid then Factor.Valid and then Value.Controls (J).Valid));
      end loop;
      pragma Assert (if Result.Controls (0).Valid then Factor.Valid);
      return Result;
   end Scaled;

   procedure Lemma_Scaled_Encloses
     (Factor : Interval; Value : Polynomial; Exact_Factor : Real; Exact, Product : Bernstein.Controls)
   is
      Result : constant Polynomial := Scaled (Factor, Value);
   begin
      for K in 0 .. Value.Degree loop
         Lemma_Product_Contains (Result.Controls (K), Factor, Value.Controls (K), Exact_Factor, Exact (K));
         pragma Loop_Invariant (for all J in 0 .. K => Contains (Result.Controls (J), Product (J)));
      end loop;
   end Lemma_Scaled_Encloses;

   function Sum (Left, Right : Polynomial) return Polynomial
   with
     Refined_Post =>
       Sum'Result.Degree = Left.Degree
       and then (if All_Valid (Sum'Result) then All_Valid (Left) and then All_Valid (Right))
       and then
         (for all K in 0 .. Left.Degree =>
            Is_Sum_Enclosure (Sum'Result.Controls (K), Left.Controls (K), Right.Controls (K)))
   is
      Result : Polynomial := (Degree => Left.Degree, Controls => <>);
   begin
      for K in 0 .. Left.Degree loop
         Result.Controls (K) := Interval_Add (Left.Controls (K), Right.Controls (K));
         Lemma_Add_Is_Enclosure (Left.Controls (K), Right.Controls (K));
         pragma
           Loop_Invariant
             (for all J in 0 .. K => Is_Sum_Enclosure (Result.Controls (J), Left.Controls (J), Right.Controls (J)));
         pragma
           Loop_Invariant
             (for all J in 0 .. K =>
                (if Result.Controls (J).Valid then Left.Controls (J).Valid and then Right.Controls (J).Valid));
      end loop;
      return Result;
   end Sum;

   procedure Lemma_Sum_Encloses (Left, Right : Polynomial; Exact_Left, Exact_Right, Total : Bernstein.Controls) is
      Result : constant Polynomial := Sum (Left, Right);
   begin
      for K in 0 .. Left.Degree loop
         Lemma_Sum_Contains
           (Result.Controls (K), Left.Controls (K), Right.Controls (K), Exact_Left (K), Exact_Right (K));
         pragma Loop_Invariant (for all J in 0 .. K => Contains (Result.Controls (J), Total (J)));
      end loop;
   end Lemma_Sum_Encloses;

   procedure Lemma_Raised_Validity (Result, Value : Polynomial; Power : Positive; At_Start : Boolean)
   with
     Ghost,
     Pre  =>
       Power <= Maximum_Degree - Value.Degree
       and then Result.Degree = Value.Degree + Power
       and then
         (if At_Start
          then
            (for all K in Power .. Value.Degree + Power =>
               (if Result.Controls (K).Valid then Value.Controls (K - Power).Valid))
          else (for all K in 0 .. Value.Degree => (if Result.Controls (K).Valid then Value.Controls (K).Valid))),
     Post => (if All_Valid (Result) then All_Valid (Value));
   --  Every control of the operand has a control of the result which is valid only if it is.

   procedure Lemma_Raised_Validity (Result, Value : Polynomial; Power : Positive; At_Start : Boolean) is
   begin
      if All_Valid (Result) then
         for J in 0 .. Value.Degree loop
            if At_Start then
               pragma Assert (Result.Controls (J + Power).Valid);
               pragma Assert (Value.Controls (J + Power - Power).Valid);
            else
               pragma Assert (Result.Controls (J).Valid);
            end if;
            pragma Loop_Invariant (for all I in 0 .. J => Value.Controls (I).Valid);
         end loop;
      end if;
   end Lemma_Raised_Validity;

   function Raised (Value : Polynomial; Power : Natural; At_Start : Boolean) return Polynomial
   with
     Refined_Post =>
       Raised'Result.Degree = Value.Degree + Power
       and then (if All_Valid (Raised'Result) then All_Valid (Value))
       and then
         (if Power = 0
          then (for all K in 0 .. Value.Degree => Is_Copy (Raised'Result.Controls (K), Value.Controls (K)))
          elsif At_Start
          then
            (for all K in 0 .. Power - 1 => Is_Exact_Zero (Raised'Result.Controls (K)))
            and then
              (for all K in Power .. Value.Degree + Power =>
                 Is_Weighted_Enclosure
                   (Raised'Result.Controls (K),
                    Value.Controls (K - Power),
                    Value.Degree,
                    K - Power,
                    Value.Degree + Power,
                    K))
          else
            (for all K in Value.Degree + 1 .. Value.Degree + Power => Is_Exact_Zero (Raised'Result.Controls (K)))
            and then
              (for all K in 0 .. Value.Degree =>
                 Is_Weighted_Enclosure
                   (Raised'Result.Controls (K), Value.Controls (K), Value.Degree, K, Value.Degree + Power, K)))
   is
      Degree : constant Degree_Range := Value.Degree;
      Result : Polynomial := (Degree => Degree + Power, Controls => <>);
   begin
      if Power = 0 then
         return Value;
      end if;
      --  The controls which the loop leaves alone are the exact zeros which Result starts with.
      pragma Assert (for all K in Degree_Range => Is_Exact_Zero (Result.Controls (K)));
      for J in 0 .. Degree loop
         if At_Start then
            Result.Controls (J + Power) :=
              Interval_Multiply (Value.Controls (J), Binomial_Ratio (Degree, J, Degree + Power, J + Power));
            Lemma_Multiply_Is_Weighted (Value.Controls (J), Degree, J, Degree + Power, J + Power);
            --  The invariant runs over the controls of the result, so the operand's index is restated from there.
            pragma
              Assert
                (Is_Weighted_Enclosure
                   (Result.Controls (J + Power),
                    Value.Controls (J + Power - Power),
                    Degree,
                    J + Power - Power,
                    Degree + Power,
                    J + Power));
            pragma Assert (if Result.Controls (J + Power).Valid then Value.Controls (J + Power - Power).Valid);
         else
            Result.Controls (J) :=
              Interval_Multiply (Value.Controls (J), Binomial_Ratio (Degree, J, Degree + Power, J));
            Lemma_Multiply_Is_Weighted (Value.Controls (J), Degree, J, Degree + Power, J);
         end if;
         pragma
           Loop_Invariant
             (if At_Start
              then
                (for all K in 0 .. Power - 1 => Is_Exact_Zero (Result.Controls (K)))
                and then
                  (for all K in Power .. J + Power =>
                     Is_Weighted_Enclosure
                       (Result.Controls (K), Value.Controls (K - Power), Degree, K - Power, Degree + Power, K))
                and then
                  (for all K in Power .. J + Power =>
                     (if Result.Controls (K).Valid then Value.Controls (K - Power).Valid))
              else
                (for all K in Degree + 1 .. Degree + Power => Is_Exact_Zero (Result.Controls (K)))
                and then
                  (for all K in 0 .. J =>
                     Is_Weighted_Enclosure (Result.Controls (K), Value.Controls (K), Degree, K, Degree + Power, K))
                and then (for all K in 0 .. J => (if Result.Controls (K).Valid then Value.Controls (K).Valid)));
      end loop;
      Lemma_Raised_Validity (Result, Value, Power, At_Start);
      return Result;
   end Raised;

   procedure Lemma_Raised_By_Zero_Encloses (Value : Polynomial; At_Start : Boolean; Exact : Bernstein.Controls)
   with
     Ghost,
     Pre  => Encloses (Value, Exact),
     Post =>
       Encloses
         (Raised (Value, 0, At_Start),
          (if At_Start
           then Bernstein.Binomial_Form.Raised_At_Start (Exact, 0)
           else Bernstein.Binomial_Form.Raised_At_Finish (Exact, 0)));
   --  Raising by no power keeps every control, and both real forms then weigh each control by C(n,j)/C(n,j) = 1.

   procedure Lemma_Raised_By_Zero_Encloses (Value : Polynomial; At_Start : Boolean; Exact : Bernstein.Controls) is
      Result : constant Polynomial := Raised (Value, 0, At_Start);
      Target : constant Bernstein.Controls :=
        (if At_Start
         then Bernstein.Binomial_Form.Raised_At_Start (Exact, 0)
         else Bernstein.Binomial_Form.Raised_At_Finish (Exact, 0));
   begin
      for K in 0 .. Value.Degree loop
         Lemma_Ratio_Cancels (Exact (K), Bernstein.Binomial_Form.Choose (Value.Degree, K));
         pragma Assert (Target (K) = Exact (K));
         Lemma_Copy_Contains (Result.Controls (K), Value.Controls (K), Exact (K));
         pragma Loop_Invariant (for all J in 0 .. K => Contains (Result.Controls (J), Target (J)));
      end loop;
   end Lemma_Raised_By_Zero_Encloses;

   procedure Lemma_Raised_At_Start_Encloses (Value : Polynomial; Power : Positive; Exact : Bernstein.Controls)
   with
     Ghost,
     Pre  =>
       Power <= Maximum_Degree - Value.Degree
       and then Encloses (Value, Exact)
       and then All_Valid (Raised (Value, Power, True)),
     Post => Encloses (Raised (Value, Power, True), Bernstein.Binomial_Form.Raised_At_Start (Exact, Power));

   procedure Lemma_Raised_At_Start_Encloses (Value : Polynomial; Power : Positive; Exact : Bernstein.Controls) is
      Result : constant Polynomial := Raised (Value, Power, True);
      Target : constant Bernstein.Controls := Bernstein.Binomial_Form.Raised_At_Start (Exact, Power);
   begin
      for K in 0 .. Value.Degree + Power loop
         if K >= Power then
            Lemma_Weighted_Contains
              (Result.Controls (K),
               Value.Controls (K - Power),
               Value.Degree,
               K - Power,
               Value.Degree + Power,
               K,
               Exact (K - Power));
         else
            Lemma_Zero_Contains (Result.Controls (K));
         end if;
         pragma Loop_Invariant (for all J in 0 .. K => Contains (Result.Controls (J), Target (J)));
      end loop;
   end Lemma_Raised_At_Start_Encloses;

   procedure Lemma_Raised_At_Finish_Encloses (Value : Polynomial; Power : Positive; Exact : Bernstein.Controls)
   with
     Ghost,
     Pre  =>
       Power <= Maximum_Degree - Value.Degree
       and then Encloses (Value, Exact)
       and then All_Valid (Raised (Value, Power, False)),
     Post => Encloses (Raised (Value, Power, False), Bernstein.Binomial_Form.Raised_At_Finish (Exact, Power));

   procedure Lemma_Raised_At_Finish_Encloses (Value : Polynomial; Power : Positive; Exact : Bernstein.Controls) is
      Result : constant Polynomial := Raised (Value, Power, False);
      Target : constant Bernstein.Controls := Bernstein.Binomial_Form.Raised_At_Finish (Exact, Power);
   begin
      for K in 0 .. Value.Degree + Power loop
         if K <= Value.Degree then
            Lemma_Weighted_Contains
              (Result.Controls (K), Value.Controls (K), Value.Degree, K, Value.Degree + Power, K, Exact (K));
         else
            Lemma_Zero_Contains (Result.Controls (K));
         end if;
         pragma Loop_Invariant (for all J in 0 .. K => Contains (Result.Controls (J), Target (J)));
      end loop;
   end Lemma_Raised_At_Finish_Encloses;

   procedure Lemma_Raised_Encloses
     (Value : Polynomial; Power : Natural; At_Start : Boolean; Exact : Bernstein.Controls) is
   begin
      if Power = 0 then
         Lemma_Raised_By_Zero_Encloses (Value, At_Start, Exact);
      elsif At_Start then
         Lemma_Raised_At_Start_Encloses (Value, Power, Exact);
      else
         Lemma_Raised_At_Finish_Encloses (Value, Power, Exact);
      end if;
   end Lemma_Raised_Encloses;

   function Constant_Polynomial (Value : Interval; Degree : Degree_Range) return Polynomial
   with
     Refined_Post =>
       Constant_Polynomial'Result.Degree = Degree
       and then (for all K in 0 .. Degree => Is_Copy (Constant_Polynomial'Result.Controls (K), Value))
   is
      Result : Polynomial := (Degree => Degree, Controls => <>);
   begin
      for K in 0 .. Degree loop
         Result.Controls (K) := Value;
         pragma Loop_Invariant (for all J in 0 .. K => Is_Copy (Result.Controls (J), Value));
      end loop;
      return Result;
   end Constant_Polynomial;

   procedure Lemma_Constant_Encloses
     (Value : Interval; Degree : Degree_Range; Level : Real; Exact : Bernstein.Controls)
   is
      Result : constant Polynomial := Constant_Polynomial (Value, Degree);
   begin
      for K in 0 .. Degree loop
         Lemma_Copy_Contains (Result.Controls (K), Value, Level);
         pragma Loop_Invariant (for all J in 0 .. K => Contains (Result.Controls (J), Exact (J)));
      end loop;
   end Lemma_Constant_Encloses;

   ---------------------------------------------------------------------------
   --  Scalars
   ---------------------------------------------------------------------------

   procedure Lemma_Orientation (Start, Finish : Dimensionless)
   with
     Ghost,
     Post =>
       (if Interval_Multiply
             (Interval_Subtract (Interval_Exact (Finish), Interval_Exact (Start)),
              Interval_Add (Interval_Exact (1.0), Interval_Multiply (Interval_Exact (Start), Interval_Exact (Finish))))
             .Valid
        then
          Contains
            (Interval_Multiply
               (Interval_Subtract (Interval_Exact (Finish), Interval_Exact (Start)),
                Interval_Add
                  (Interval_Exact (1.0), Interval_Multiply (Interval_Exact (Start), Interval_Exact (Finish)))),
             (R (Finish) - R (Start)) * (1.0 + R (Start) * R (Finish))));
   --  The orientation's enclosure, when valid, holds (Yꜰ - Yₛ)·(1 + Yₛ·Yꜰ).

   procedure Lemma_Orientation (Start, Finish : Dimensionless) is
      Difference : constant Interval := Interval_Subtract (Interval_Exact (Finish), Interval_Exact (Start));
      Cross      : constant Interval :=
        Interval_Add (Interval_Exact (1.0), Interval_Multiply (Interval_Exact (Start), Interval_Exact (Finish)));
   begin
      if Interval_Multiply (Difference, Cross).Valid then
         Lemma_Exact (Finish);
         Lemma_Exact (Start);
         Lemma_Subtract_Contains (Interval_Exact (Finish), Interval_Exact (Start), R (Finish), R (Start));
         Lemma_One_Plus_Product (Start, Finish);
         Lemma_Multiply_Contains (Difference, Cross, R (Finish) - R (Start), 1.0 + R (Start) * R (Finish));
      end if;
   end Lemma_Orientation;

   function Orientation (Coefficients : Bernstein_Chart) return Interval
   with
     Refined_Post =>
       (if Orientation'Result.Valid
        then Contains (Orientation'Result, Turn (Coefficients))
        else Orientation'Result.Lower < 0.0 and then Orientation'Result.Upper > 0.0)
   is
      Start_Value  : constant Dimensionless := Coefficients (0, 1);
      Finish_Value : constant Dimensionless := Coefficients (Fixed_Chart_Degree, 1);
   begin
      Lemma_Orientation (Start_Value, Finish_Value);
      return
        Interval_Multiply
          (Interval_Subtract (Interval_Exact (Finish_Value), Interval_Exact (Start_Value)),
           Interval_Add
             (Interval_Exact (1.0), Interval_Multiply (Interval_Exact (Start_Value), Interval_Exact (Finish_Value))));
   end Orientation;

   function Orientation_Sign (Coefficients : Bernstein_Chart) return Dimensionless is
      Turn_Enclosure : constant Interval := Orientation (Coefficients);
   begin
      if Turn_Enclosure.Lower > 0.0 then
         return 1.0;
      elsif Turn_Enclosure.Upper < 0.0 then
         return -1.0;
      else
         return 0.0;
      end if;
   end Orientation_Sign;

   function Partial_Dot (Left, Right : Position_Scale; Last : Axis_Name) return Real
   is ((if Last = X_Axis then 0.0 else Partial_Dot (Left, Right, Axis_Name'Pred (Last)))
       + R (Left (Last)) * R (Right (Last)))
   with Ghost, Subprogram_Variant => (Decreases => Last);
   --  The dot product's terms up to the axis Last, summed in the order in which Dot_Interval adds them.

   procedure Lemma_Partial_Dot_Is_Dot (Left, Right : Position_Scale)
   with Ghost, Post => Partial_Dot (Left, Right, E_Axis) = Dot_Real (Left, Right);

   procedure Lemma_Partial_Dot_Is_Dot (Left, Right : Position_Scale) is
   begin
      pragma Assert (Partial_Dot (Left, Right, X_Axis) = R (Left (X_Axis)) * R (Right (X_Axis)));
      pragma
        Assert
          (Partial_Dot (Left, Right, Y_Axis)
           = Partial_Dot (Left, Right, X_Axis) + R (Left (Y_Axis)) * R (Right (Y_Axis)));
      pragma
        Assert
          (Partial_Dot (Left, Right, Z_Axis)
           = Partial_Dot (Left, Right, Y_Axis) + R (Left (Z_Axis)) * R (Right (Z_Axis)));
      pragma
        Assert
          (Partial_Dot (Left, Right, E_Axis)
           = Partial_Dot (Left, Right, Z_Axis) + R (Left (E_Axis)) * R (Right (E_Axis)));
   end Lemma_Partial_Dot_Is_Dot;

   function Encloses_Partial_Dot (Sum_So_Far : Interval; Left, Right : Position_Scale; Last : Axis_Name) return Boolean
   with
     Ghost,
     Post =>
       Encloses_Partial_Dot'Result = (if Sum_So_Far.Valid then Contains (Sum_So_Far, Partial_Dot (Left, Right, Last)));
   --  Whether the running sum of Dot_Interval, when valid, holds the terms up to the axis Last.

   function Encloses_Partial_Dot (Sum_So_Far : Interval; Left, Right : Position_Scale; Last : Axis_Name) return Boolean
   is
   begin
      return (if Sum_So_Far.Valid then Contains (Sum_So_Far, Partial_Dot (Left, Right, Last)));
   end Encloses_Partial_Dot;

   procedure Lemma_Dot_Step (Sum_So_Far : Interval; Left, Right : Position_Scale; Axis : Axis_Name)
   with
     Ghost,
     Pre  =>
       (if Axis = X_Axis
        then Contains (Sum_So_Far, 0.0)
        else Encloses_Partial_Dot (Sum_So_Far, Left, Right, Axis_Name'Pred (Axis))),
     Post =>
       Encloses_Partial_Dot
         (Interval_Add (Sum_So_Far, Interval_Multiply (Interval_Exact (Left (Axis)), Interval_Exact (Right (Axis)))),
          Left,
          Right,
          Axis);
   --  Adding the enclosure of one more term keeps the enclosure of the partial dot product.

   procedure Lemma_Dot_Step (Sum_So_Far : Interval; Left, Right : Position_Scale; Axis : Axis_Name) is
      Product : constant Interval := Interval_Multiply (Interval_Exact (Left (Axis)), Interval_Exact (Right (Axis)));
      Partial : constant Real := (if Axis = X_Axis then 0.0 else Partial_Dot (Left, Right, Axis_Name'Pred (Axis)));
   begin
      Lemma_Exact_Product (Left (Axis), Right (Axis));
      if Interval_Add (Sum_So_Far, Product).Valid then
         pragma Assert (Contains (Sum_So_Far, Partial));
         Lemma_Add_Contains (Sum_So_Far, Product, Partial, R (Left (Axis)) * R (Right (Axis)));
         pragma Assert (Partial_Dot (Left, Right, Axis) = Partial + R (Left (Axis)) * R (Right (Axis)));
      end if;
   end Lemma_Dot_Step;

   function Dot_Interval (Left, Right : Position_Scale) return Interval is
      Result : Interval := Interval_Exact (0.0);
   begin
      Lemma_Exact (0.0);
      pragma Assert (R (0.0) = 0.0);
      pragma Assert (Contains (Result, 0.0));
      for Axis in Axis_Name loop
         Lemma_Dot_Step (Result, Left, Right, Axis);
         Result :=
           Interval_Add (Result, Interval_Multiply (Interval_Exact (Left (Axis)), Interval_Exact (Right (Axis))));
         pragma Loop_Invariant (Encloses_Partial_Dot (Result, Left, Right, Axis));
      end loop;
      Lemma_Partial_Dot_Is_Dot (Left, Right);
      return Result;
   end Dot_Interval;

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Polynomials;
