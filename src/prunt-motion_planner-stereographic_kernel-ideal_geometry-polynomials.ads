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

--  The interval polynomials which the geometry certificates are assembled from, each with the real polynomial it
--  encloses.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;
with Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials;
use Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials;
with Prunt.Motion_Planner.Stereographic_Kernel.Intervals;
use Prunt.Motion_Planner.Stereographic_Kernel.Intervals;

package Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Polynomials
  with SPARK_Mode => On
is

   subtype Root_Order_Range is Positive range 1 .. 4;
   --  The order of an endpoint root which a quotient divides out.

   ---------------------------------------------------------------------------
   --  The chart
   ---------------------------------------------------------------------------

   function Chart_Polynomial (Coefficients : Bernstein_Chart; Component : Chart_Component_Index) return Polynomial
   with
     Post =>
       Chart_Polynomial'Result.Degree = Fixed_Chart_Degree
       and then Encloses (Chart_Polynomial'Result, Chart_Controls (Coefficients, Component));
   --  One chart coordinate, with each stored control enclosed exactly.

   ---------------------------------------------------------------------------
   --  Endpoint roots and their quotients
   ---------------------------------------------------------------------------

   function Has_Root_Order
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; At_Start : Boolean)
      return Boolean
   is (for all I in 1 .. Order - 1 =>
         Coefficients ((if At_Start then I else Fixed_Chart_Degree - I), Component)
         = Coefficients ((if At_Start then 0 else Fixed_Chart_Degree), Component));
   --  Whether the Order - 1 controls next to an endpoint equal the endpoint control, so that Y꜀ minus its endpoint
   --  value has a root of that order there.

   function Root_Order
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; At_Start : Boolean) return Root_Order_Range
   with Post => Has_Root_Order (Coefficients, Component, Root_Order'Result, At_Start);
   --  Return k such that the endpoint root of Y꜀ - Y꜀(endpoint) has order at least k. Equal leading controls expose
   --  higher orders, as for structurally flat endpoint jets.

   function Planar_Factor_Order (Coefficients : Bernstein_Chart; At_Start : Boolean) return Root_Order_Range
   with
     Post =>
       Planar_Factor_Order'Result in 1 | 4
       and then Has_Root_Order (Coefficients, 1, Planar_Factor_Order'Result, At_Start);
   --  Four when the three controls next to an endpoint equal it, which is how structurally flat endpoint jets
   --  appear, and otherwise one.

   function Quotient_Controls
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; At_Start : Boolean)
      return Bernstein.Controls
   with
     Ghost,
     Pre  => Has_Root_Order (Coefficients, Component, Order, At_Start),
     Post => Quotient_Controls'Result'First = 0 and then Quotient_Controls'Result'Last = Fixed_Chart_Degree - Order;
   --  The real controls of (Y꜀ - Y꜀(0))/Vᵏ at the start, or of (Y꜀(1) - Y꜀)/(1 - V)ᵏ at the finish:
   --  Binomial_Form.Start_Quotient or Finish_Quotient of the chart coordinate's controls.

   procedure Lemma_Quotient_Value
     (Coefficients : Bernstein_Chart;
      Component    : Chart_Component_Index;
      Order        : Root_Order_Range;
      At_Start     : Boolean;
      V            : Real)
   with
     Ghost,
     Pre  => Has_Root_Order (Coefficients, Component, Order, At_Start),
     Post =>
       (if At_Start
        then
          Chart_Value (Coefficients, Component, V) - R (Coefficients (0, Component))
          = Bernstein.Binomial_Form.Power (V, Order)
            * Bernstein.Value (Quotient_Controls (Coefficients, Component, Order, At_Start), V)
        else
          R (Coefficients (Fixed_Chart_Degree, Component)) - Chart_Value (Coefficients, Component, V)
          = Bernstein.Binomial_Form.Power (1.0 - V, Order)
            * Bernstein.Value (Quotient_Controls (Coefficients, Component, Order, At_Start), V));
   --  The quotient times the power of V, or of 1 - V, which it divided out, is the chart coordinate's distance
   --  from its endpoint value.

   function Quotient
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Order : Root_Order_Range; At_Start : Boolean)
      return Polynomial
   with
     Post =>
       Quotient'Result.Degree = Fixed_Chart_Degree - Order
       and then
         (if All_Valid (Quotient'Result) and then Has_Root_Order (Coefficients, Component, Order, At_Start)
          then Encloses (Quotient'Result, Quotient_Controls (Coefficients, Component, Order, At_Start)));
   --  Divide out the exact endpoint root in Bernstein form: (Y꜀ - Y꜀(0))/Vᵏ at the start, or (Y꜀(1) - Y꜀)/(1 - V)ᵏ
   --  at the finish. The result has degree eight minus Order.

   ---------------------------------------------------------------------------
   --  The factor 1 + Value·Y₁
   ---------------------------------------------------------------------------

   function Linear_Factor_Controls (Coefficients : Bernstein_Chart; Value : Dimensionless) return Bernstein.Controls
   with
     Ghost,
     Post =>
       Linear_Factor_Controls'Result'First = 0
       and then Linear_Factor_Controls'Result'Last = Fixed_Chart_Degree
       and then
         (for all I in Chart_Coefficient_Index =>
            Linear_Factor_Controls'Result (I) = 1.0 + R (Value) * R (Coefficients (I, 1)));

   procedure Lemma_Linear_Factor_Value (Coefficients : Bernstein_Chart; Value : Dimensionless; V : Real)
   with
     Ghost,
     Post =>
       Bernstein.Value (Linear_Factor_Controls (Coefficients, Value), V)
       = 1.0 + R (Value) * Chart_Value (Coefficients, 1, V);

   function Linear_Factor (Coefficients : Bernstein_Chart; Value : Dimensionless) return Polynomial
   with
     Post =>
       Linear_Factor'Result.Degree = Fixed_Chart_Degree
       and then
         (if All_Valid (Linear_Factor'Result)
          then Encloses (Linear_Factor'Result, Linear_Factor_Controls (Coefficients, Value)));
   --  The polynomial 1 + Value·Y₁.

   ---------------------------------------------------------------------------
   --  Controlwise operations
   ---------------------------------------------------------------------------

   function Scaled (Factor : Interval; Value : Polynomial) return Polynomial
   with
     Post =>
       Scaled'Result.Degree = Value.Degree
       and then (if All_Valid (Scaled'Result) then Factor.Valid and then All_Valid (Value));
   --  Every control multiplied by Factor.

   procedure Lemma_Scaled_Encloses
     (Factor : Interval; Value : Polynomial; Exact_Factor : Real; Exact, Product : Bernstein.Controls)
   with
     Ghost,
     Pre  =>
       Contains (Factor, Exact_Factor)
       and then Encloses (Value, Exact)
       and then All_Valid (Scaled (Factor, Value))
       and then Bernstein.Same_Range (Exact, Product)
       and then (for all K in Exact'Range => Product (K) = Exact_Factor * Exact (K)),
     Post => Encloses (Scaled (Factor, Value), Product);

   function Sum (Left, Right : Polynomial) return Polynomial
   with
     Pre  => Left.Degree = Right.Degree,
     Post =>
       Sum'Result.Degree = Left.Degree
       and then (if All_Valid (Sum'Result) then All_Valid (Left) and then All_Valid (Right));
   --  The controlwise sum of two polynomials of one degree.

   procedure Lemma_Sum_Encloses (Left, Right : Polynomial; Exact_Left, Exact_Right, Total : Bernstein.Controls)
   with
     Ghost,
     Pre  =>
       Left.Degree = Right.Degree
       and then Encloses (Left, Exact_Left)
       and then Encloses (Right, Exact_Right)
       and then All_Valid (Sum (Left, Right))
       and then Bernstein.Same_Range (Exact_Left, Total)
       and then (for all K in Total'Range => Total (K) = Exact_Left (K) + Exact_Right (K)),
     Post => Encloses (Sum (Left, Right), Total);

   function Raised (Value : Polynomial; Power : Natural; At_Start : Boolean) return Polynomial
   with
     Pre  => Power <= Maximum_Degree - Value.Degree,
     Post =>
       Raised'Result.Degree = Value.Degree + Power and then (if All_Valid (Raised'Result) then All_Valid (Value));
   --  Multiply by Vᵖ at the start, or by (1 - V)ᵖ at the finish. The result has Power more degrees:
   --
   --     Vᵖ·Bⱼ,ₙ = C(n,j)/C(n+p,j+p)·Bⱼ₊ₚ,ₙ₊ₚ   and   (1 - V)ᵖ·Bⱼ,ₙ = C(n,j)/C(n+p,j)·Bⱼ,ₙ₊ₚ.

   procedure Lemma_Raised_Encloses
     (Value : Polynomial; Power : Natural; At_Start : Boolean; Exact : Bernstein.Controls)
   with
     Ghost,
     Pre  =>
       Power <= Maximum_Degree - Value.Degree
       and then Encloses (Value, Exact)
       and then All_Valid (Raised (Value, Power, At_Start)),
     Post =>
       Encloses
         (Raised (Value, Power, At_Start),
          (if At_Start
           then Bernstein.Binomial_Form.Raised_At_Start (Exact, Power)
           else Bernstein.Binomial_Form.Raised_At_Finish (Exact, Power)));

   function Constant_Polynomial (Value : Interval; Degree : Degree_Range) return Polynomial
   with Post => Constant_Polynomial'Result.Degree = Degree;
   --  The polynomial whose controls all equal Value.

   procedure Lemma_Constant_Encloses
     (Value : Interval; Degree : Degree_Range; Level : Real; Exact : Bernstein.Controls)
   with
     Ghost,
     Pre  =>
       Contains (Value, Level)
       and then Exact'First = 0
       and then Exact'Last = Degree
       and then (for all K in Exact'Range => Exact (K) = Level),
     Post => Encloses (Constant_Polynomial (Value, Degree), Exact);

   ---------------------------------------------------------------------------
   --  Scalars
   ---------------------------------------------------------------------------

   function Orientation (Coefficients : Bernstein_Chart) return Interval
   with Post => (if Orientation'Result.Valid then Contains (Orientation'Result, Turn (Coefficients)));
   --  Enclose (Yꜰ - Yₛ)·(1 + Yₛ·Yꜰ), whose sign is that of the turn from the start tangent to the finish tangent.

   function Orientation_Sign (Coefficients : Bernstein_Chart) return Dimensionless
   with
     Post =>
       (Orientation_Sign'Result = 1.0 and then Turn (Coefficients) > 0.0)
       or else (Orientation_Sign'Result = -1.0 and then Turn (Coefficients) < 0.0)
       or else Orientation_Sign'Result = 0.0;
   --  The proven sign of the turn, or zero when its enclosure does not exclude zero.

   function Dot_Interval (Left, Right : Position_Scale) return Interval
   with Post => (if Dot_Interval'Result.Valid then Contains (Dot_Interval'Result, Dot_Real (Left, Right)));
   --  Enclose the dot product of two exact vectors.

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Polynomials;
