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

package body Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Planar
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  The real polynomials which the certificate encloses
   ---------------------------------------------------------------------------

   function Quotient_Factor (Coefficients : Bernstein_Chart; At_Start : Boolean) return Bernstein.Controls
   is (Quotient_Controls (Coefficients, 1, Planar_Factor_Order (Coefficients, At_Start), At_Start))
   with
     Ghost,
     Post =>
       Quotient_Factor'Result'First = 0
       and then Quotient_Factor'Result'Last = Fixed_Chart_Degree - Planar_Factor_Order (Coefficients, At_Start);
   --  The controls of (Y₁ - Yₛ)/Vᵏ at the start, or of (Yꜰ - Y₁)/(1 - V)ᵏ at the finish, for the order k which
   --  the certificate divides out.

   function Cross_Factor (Coefficients : Bernstein_Chart; At_Start : Boolean) return Bernstein.Controls
   is (Linear_Factor_Controls (Coefficients, Coefficients ((if At_Start then 0 else Fixed_Chart_Degree), 1)))
   with Ghost, Post => Cross_Factor'Result'First = 0 and then Cross_Factor'Result'Last = Fixed_Chart_Degree;
   --  The controls of 1 + Yₛ·Y₁ at the start, or of 1 + Yꜰ·Y₁ at the finish.

   function Signed_Controls (Factor : Real; Exact : Bernstein.Controls) return Bernstein.Controls
   with
     Ghost,
     Pre  => Exact'First = 0 and then Exact'Last in 0 .. Maximum_Degree,
     Post =>
       Bernstein.Same_Range (Signed_Controls'Result, Exact)
       and then (for all K in Exact'Range => Signed_Controls'Result (K) = Factor * Exact (K));
   --  Every control multiplied by Factor.

   function Signed_Controls (Factor : Real; Exact : Bernstein.Controls) return Bernstein.Controls is
      Result : Bernstein.Controls (Exact'Range) := [others => 0.0];
   begin
      for K in Exact'Range loop
         Result (K) := Factor * Exact (K);
         pragma Loop_Invariant (for all J in Exact'First .. K => Result (J) = Factor * Exact (J));
      end loop;
      return Result;
   end Signed_Controls;

   function Exact_Product
     (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless) return Bernstein.Controls
   is (Signed_Controls
         (R (Sign),
          Bernstein.Binomial_Form.Product
            (Quotient_Factor (Coefficients, At_Start), Cross_Factor (Coefficients, At_Start))))
   with Ghost;
   --  The controls of the real polynomial which Product encloses: the sign times the quotient times the cross
   --  factor.

   ---------------------------------------------------------------------------
   --  The certificate
   ---------------------------------------------------------------------------

   procedure Lemma_Sign_Is_Exact (Sign : Dimensionless)
   with
     Ghost,
     Pre  => Sign = 1.0 or else Sign = -1.0,
     Post =>
       Contains (Interval_Exact (Sign), R (Sign)) and then (if Sign = 1.0 then R (Sign) = 1.0 else R (Sign) = -1.0);
   --  A sign is enclosed by its own point interval.

   procedure Lemma_Sign_Is_Exact (Sign : Dimensionless) is null;

   procedure Lemma_Signed_Product_Encloses (Sign : Dimensionless; Left, Right : Polynomial; A, B : Bernstein.Controls)
   with
     Ghost,
     Pre  =>
       Left.Degree >= 1
       and then Right.Degree >= 1
       and then Left.Degree + Right.Degree <= Maximum_Degree
       and then A'First = 0
       and then A'Last = Left.Degree
       and then B'First = 0
       and then B'Last = Right.Degree
       and then (if All_Valid (Left) then Encloses (Left, A))
       and then (if All_Valid (Right) then Encloses (Right, B)),
     Post =>
       (if (Sign = 1.0 or else Sign = -1.0) and then All_Valid (Scaled (Interval_Exact (Sign), Multiply (Left, Right)))
        then
          Encloses
            (Scaled (Interval_Exact (Sign), Multiply (Left, Right)),
             Signed_Controls (R (Sign), Bernstein.Binomial_Form.Product (A, B))));
   --  Validity walks backwards from the scaled product to its operands, and the enclosures walk forwards again:
   --  operands which enclose A and B whenever they are valid give a signed product which, when valid, encloses
   --  the sign times the product of A and B.

   procedure Lemma_Signed_Product_Encloses (Sign : Dimensionless; Left, Right : Polynomial; A, B : Bernstein.Controls)
   is
   begin
      if (Sign = 1.0 or else Sign = -1.0) and then All_Valid (Scaled (Interval_Exact (Sign), Multiply (Left, Right)))
      then
         pragma Assert (All_Valid (Multiply (Left, Right)));
         pragma Assert (All_Valid (Left) and then All_Valid (Right));
         Lemma_Multiply_Encloses (Left, Right, A, B);
         Lemma_Sign_Is_Exact (Sign);
         Lemma_Scaled_Encloses
           (Factor       => Interval_Exact (Sign),
            Value        => Multiply (Left, Right),
            Exact_Factor => R (Sign),
            Exact        => Bernstein.Binomial_Form.Product (A, B),
            Product      => Signed_Controls (R (Sign), Bernstein.Binomial_Form.Product (A, B)));
      end if;
   end Lemma_Signed_Product_Encloses;

   function Product (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless) return Polynomial
   with
     Post =>
       (if (Sign = 1.0 or else Sign = -1.0) and then All_Valid (Product'Result)
        then Encloses (Product'Result, Exact_Product (Coefficients, At_Start, Sign)));
   --  The oriented cone determinant at one end with its endpoint root divided out. Scaled by a proven sign, a
   --  valid result encloses that sign times the real quotient times the real cross factor.

   function Product (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless) return Polynomial is
      Order : constant Root_Order_Range := Planar_Factor_Order (Coefficients, At_Start);
      Value : constant Dimensionless := Coefficients ((if At_Start then 0 else Fixed_Chart_Degree), 1);
   begin
      --  Proof only: the operands enclose the real quotient and cross factor whenever they are valid.
      Lemma_Signed_Product_Encloses
        (Sign,
         Quotient (Coefficients, 1, Order, At_Start),
         Linear_Factor (Coefficients, Value),
         Quotient_Factor (Coefficients, At_Start),
         Cross_Factor (Coefficients, At_Start));
      return
        Scaled
          (Interval_Exact (Sign),
           Multiply (Quotient (Coefficients, 1, Order, At_Start), Linear_Factor (Coefficients, Value)));
   end Product;

   function Is_Certified (Coefficients : Bernstein_Chart) return Boolean
   with
     Refined_Post =>
       Is_Certified'Result
       = (Orientation_Sign (Coefficients) /= 0.0
          and then Is_Nonnegative (Product (Coefficients, True, Orientation_Sign (Coefficients)))
          and then Is_Nonnegative (Product (Coefficients, False, Orientation_Sign (Coefficients))))
   is
      Sign : constant Dimensionless := Orientation_Sign (Coefficients);
   begin
      if Sign = 0.0 then
         return False;
      end if;
      return
        Is_Nonnegative (Product (Coefficients, True, Sign))
        and then Is_Nonnegative (Product (Coefficients, False, Sign));
   end Is_Certified;

   ---------------------------------------------------------------------------
   --  What the certificate establishes
   ---------------------------------------------------------------------------

   procedure Lemma_Planar_Chart_Is_Flat (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Pre  => Is_Structurally_Planar (Coefficients) and then 0.0 <= V and then V <= 1.0,
     Post => Normal_Square (Coefficients, V) = 0.0;
   --  Chart coordinates whose controls all vanish are zero.

   procedure Lemma_Planar_Chart_Is_Flat (Coefficients : Bernstein_Chart; V : Real) is
   begin
      for C in 2 .. 3 loop
         pragma Assert (Bernstein.All_Within (Chart_Controls (Coefficients, C), 0.0, 0.0));
         Bernstein.Lemma_Convex_Hull (Chart_Controls (Coefficients, C), V, 0.0, 0.0);
         pragma Loop_Invariant (for all D in 2 .. C => Chart_Value (Coefficients, D, V) = 0.0);
      end loop;
   end Lemma_Planar_Chart_Is_Flat;

   procedure Lemma_Restore_Root (S, P, Qv, Fv, D, N : Real)
   with
     Ghost,
     Pre  => P >= 0.0 and then D = P * Qv and then N = S * (Qv * Fv) and then N >= 0.0,
     Post => S * (D * Fv) >= 0.0;
   --  Multiplying the certified product N = S·Q·F by the nonnegative power P which was divided out of D = P·Q
   --  keeps it nonnegative.

   procedure Lemma_Restore_Root (S, P, Qv, Fv, D, N : Real) is
   begin
      pragma Assert (S * (D * Fv) = S * ((P * Qv) * Fv));
      pragma Assert (S * ((P * Qv) * Fv) = P * (S * (Qv * Fv)));
      pragma Assert (P * N >= 0.0);
   end Lemma_Restore_Root;

   procedure Lemma_Signed_End (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless; V : Real)
   with
     Ghost,
     Pre  =>
       (Sign = 1.0 or else Sign = -1.0)
       and then Is_Nonnegative (Product (Coefficients, At_Start, Sign))
       and then 0.0 <= V
       and then V <= 1.0,
     Post =>
       (if At_Start
        then
          R (Sign)
          * ((Chart_Value (Coefficients, 1, V) - Start_Value (Coefficients))
             * (1.0 + Start_Value (Coefficients) * Chart_Value (Coefficients, 1, V)))
          >= 0.0
        else
          R (Sign)
          * ((Finish_Value (Coefficients) - Chart_Value (Coefficients, 1, V))
             * (1.0 + Finish_Value (Coefficients) * Chart_Value (Coefficients, 1, V)))
          >= 0.0);
   --  A nonnegative product at one end makes the planar cone determinant at that end, times the sign,
   --  nonnegative on the unit interval.

   procedure Lemma_Signed_End (Coefficients : Bernstein_Chart; At_Start : Boolean; Sign : Dimensionless; V : Real) is
      Order : constant Root_Order_Range := Planar_Factor_Order (Coefficients, At_Start);
      Value : constant Dimensionless := Coefficients ((if At_Start then 0 else Fixed_Chart_Degree), 1);
      Q     : constant Bernstein.Controls := Quotient_Factor (Coefficients, At_Start);
      F     : constant Bernstein.Controls := Cross_Factor (Coefficients, At_Start);
      U     : constant Bernstein.Controls := Bernstein.Binomial_Form.Product (Q, F);
      E     : constant Bernstein.Controls := Signed_Controls (R (Sign), U);
      S     : constant Real := R (Sign);
      Y     : constant Real := Chart_Value (Coefficients, 1, V);
      Base  : constant Real := (if At_Start then V else 1.0 - V);
      P     : constant Real := Bernstein.Binomial_Form.Power (Base, Order);
      Qv    : constant Real := Bernstein.Value (Q, V);
      Fv    : constant Real := Bernstein.Value (F, V);
      D     : constant Real := (if At_Start then Y - R (Value) else R (Value) - Y);
   begin
      --  The product is valid, so it encloses the signed real product, which is then nonnegative at V.
      pragma Assert (All_Valid (Product (Coefficients, At_Start, Sign)));
      pragma Assert (Encloses (Product (Coefficients, At_Start, Sign), E));
      Lemma_Is_Nonnegative (Product (Coefficients, At_Start, Sign), E, V);
      pragma Assert (Bernstein.Value (E, V) >= 0.0);

      --  Its value is the sign times the values of the quotient and of the cross factor.
      Bernstein.Lemma_Value_Is_Homogeneous (S, U, E, V);
      Bernstein.Binomial_Form.Lemma_Product (Q, F, V);
      pragma Assert (Bernstein.Value (E, V) = S * (Qv * Fv));
      Lemma_Linear_Factor_Value (Coefficients, Value, V);
      pragma Assert (Fv = 1.0 + R (Value) * Y);

      --  The quotient times the power which it divided out is the distance from the endpoint value.
      Lemma_Quotient_Value (Coefficients, 1, Order, At_Start, V);
      pragma Assert (D = P * Qv);
      Bernstein.Binomial_Form.Lemma_Power_Is_Nonnegative (Base, Order);
      Lemma_Restore_Root (S, P, Qv, Fv, D, Bernstein.Value (E, V));
      pragma Assert (S * (D * Fv) >= 0.0);
   end Lemma_Signed_End;

   procedure Lemma_Signed_Cone (S, T, Ys, Yf, Y, W, Sd, Fd : Real)
   with
     Ghost,
     Pre  =>
       W = 0.0
       and then Sd = (Y - Ys) * (1.0 + Ys * Y) + Ys * W
       and then Fd = (Yf - Y) * (1.0 + Yf * Y) - Yf * W
       and then ((S = 1.0 and then T > 0.0) or else (S = -1.0 and then T < 0.0))
       and then S * ((Y - Ys) * (1.0 + Ys * Y)) >= 0.0
       and then S * ((Yf - Y) * (1.0 + Yf * Y)) >= 0.0,
     Post => (T > 0.0 and then Sd >= 0.0 and then Fd >= 0.0) or else (T < 0.0 and then Sd <= 0.0 and then Fd <= 0.0);
   --  In the plane the cone determinants are the two factored products, and their common sign is that of the
   --  turn.

   procedure Lemma_Signed_Cone (S, T, Ys, Yf, Y, W, Sd, Fd : Real) is
   begin
      --  With W = 0 the determinants are the factored products themselves.
      pragma Assert (Ys * W = 0.0 and then Yf * W = 0.0);
      pragma Assert (Sd = (Y - Ys) * (1.0 + Ys * Y));
      pragma Assert (Fd = (Yf - Y) * (1.0 + Yf * Y));

      --  The sign is that of the turn, so multiplying by it is the identity or a negation.
      pragma Assert (if T > 0.0 then S = 1.0 else (S = -1.0 and then T < 0.0));
   end Lemma_Signed_Cone;

   procedure Lemma_Certified (Coefficients : Bernstein_Chart; V : Real) is
      Sign : constant Dimensionless := Orientation_Sign (Coefficients);
   begin
      --  The certificate passed, so the sign is the proven sign of the turn and both products are nonnegative.
      pragma Assert (Sign = 1.0 or else Sign = -1.0);
      Lemma_Sign_Is_Exact (Sign);

      --  Each product makes the factored determinant at its end, times the sign, nonnegative at V; and in the
      --  plane those factored products are the cone determinants.
      Lemma_Signed_End (Coefficients, True, Sign, V);
      Lemma_Signed_End (Coefficients, False, Sign, V);
      Lemma_Planar_Chart_Is_Flat (Coefficients, V);
      Lemma_Signed_Cone
        (S  => R (Sign),
         T  => Turn (Coefficients),
         Ys => Start_Value (Coefficients),
         Yf => Finish_Value (Coefficients),
         Y  => Chart_Value (Coefficients, 1, V),
         W  => Normal_Square (Coefficients, V),
         Sd => Start_Determinant (Coefficients, V),
         Fd => Finish_Determinant (Coefficients, V));
   end Lemma_Certified;

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Planar;
