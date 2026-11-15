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

with Ada.Numerics;

with Prunt.Motion_Planner.Stereographic_Kernel.Elementary; use Prunt.Motion_Planner.Stereographic_Kernel.Elementary;
with Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials;
with Prunt.Motion_Planner.Stereographic_Kernel.Intervals;  use Prunt.Motion_Planner.Stereographic_Kernel.Intervals;

package body Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes
  with SPARK_Mode => On
is
   use Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;
   use Prunt.Motion_Planner.Stereographic_Kernel.Polynomials.Interpolation;
   use all type Prunt.Motion_Planner.Reals.Real;
   --  For To_Real, by which a count of rounded operations becomes a real number.

   ---------------------------------------------------------------------------
   --  What Build establishes
   ---------------------------------------------------------------------------
   --
   --  Build is annotated with predicates which mean nothing outside this package, and with one lemma for each of
   --  its steps, so that its own proof obligations need no arithmetic. What a predicate means is a refined
   --  postcondition, which only the lemmas' proofs can see.

   package Accuracy
     with Ghost
   is

      function Header_Is_Built (Node, Complement : Dimensionless) return Boolean;
      --  Whether Node and Complement are in [0, 1] with a sum of at least a half, and Complement is 1 - Node,
      --  rounded.

      function Is_Copy (Stored, Original : Dimensionless) return Boolean;
      --  Whether the two denote the same real number.

      function Is_Binomial (Value : Dimensionless; K : Chart_Coefficient_Index) return Boolean;
      --  Whether Value is the binomial coefficient of K in degree eight, exactly.

      function Is_Tracking
        (Value, Node : Dimensionless; K : Chart_Coefficient_Index; By_Node, By_Complement : Natural) return Boolean;
      --  Whether Value is in the range of a basis value, and, if Node has margins, is the binomial coefficient of K
      --  times By_Node factors of the node and By_Complement factors of its exact complement, to within one Unit
      --  of relative error for each factor of the first kind and two for each of the second.

      function Basis_Is_Built (Value, Node : Dimensionless; K : Chart_Coefficient_Index) return Boolean;
      --  Whether Value is in the range of a basis value, and accurate as basis polynomial K if Node has margins.

      function Is_Built (Value : Sample) return Boolean;
      --  The postcondition of Build for one sample.

      function Node_Is_Built (Angle : Dimensionless) return Boolean
      with
        Post =>
          Node_Is_Built'Result
          and then 1.0 - Dimensionless_Math.Cos (Angle) > -1.0
          and then 1.0 - Dimensionless_Math.Cos (Angle) < 3.0
          and then 0.5 * (1.0 - Dimensionless_Math.Cos (Angle)) >= 0.0
          and then 0.5 * (1.0 - Dimensionless_Math.Cos (Angle)) <= 1.0
          and then 1.0 - 0.5 * (1.0 - Dimensionless_Math.Cos (Angle)) > -1.0
          and then 1.0 - 0.5 * (1.0 - Dimensionless_Math.Cos (Angle)) < 2.0
          and then
            Header_Is_Built
              (0.5 * (1.0 - Dimensionless_Math.Cos (Angle)), 1.0 - 0.5 * (1.0 - Dimensionless_Math.Cos (Angle)));
      --  Always True. Its postcondition is what the library's cosine, which is only known to be within [-1, 1],
      --  gives for the node and its complement as Build computes them. A function, so that Build can state it
      --  among its declarations, before the node is computed. The strict bounds are there for the overflow checks
      --  of those declarations: a value strictly between two numbers is finite by one axiom of the provers.

      procedure Lemma_Copy (Node : Dimensionless)
      with Post => Is_Copy (Node, Node);

      procedure Lemma_Binomial (K : Chart_Coefficient_Index)
      with Post => Is_Binomial (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K), K);
      --  The table holds the binomial coefficients of degree eight exactly.

      procedure Lemma_Start (Value, Node : Dimensionless; K : Chart_Coefficient_Index)
      with Pre => Is_Binomial (Value, K), Post => Is_Tracking (Value, Node, K, 0, 0);
      --  An exact binomial coefficient tracks itself.

      procedure Lemma_Node_Step (Value, Node : Dimensionless; K : Chart_Coefficient_Index; Done : Natural)
      with
        Pre  => Node >= 0.0 and then Node <= 1.0 and then Done < K and then Is_Tracking (Value, Node, K, Done, 0),
        Post =>
          Value * Node > -1.0
          and then Value * Node < 2.0 * Largest_Basis_Value
          and then Is_Tracking (Value * Node, Node, K, Done + 1, 0);
      --  One multiplication by the node. The strict bounds are for its overflow check.

      procedure Lemma_Complement_Step
        (Value, Node, Complement : Dimensionless; K : Chart_Coefficient_Index; Done : Natural)
      with
        Pre  =>
          Header_Is_Built (Node, Complement)
          and then Done < Fixed_Chart_Degree - K
          and then Is_Tracking (Value, Node, K, K, Done),
        Post =>
          Value * Complement > -1.0
          and then Value * Complement < 2.0 * Largest_Basis_Value
          and then Is_Tracking (Value * Complement, Node, K, K, Done + 1);
      --  One multiplication by the rounded complement.

      procedure Lemma_Basis_Is_Built (Value, Node : Dimensionless; K : Chart_Coefficient_Index)
      with Pre => Is_Tracking (Value, Node, K, K, Fixed_Chart_Degree - K), Post => Basis_Is_Built (Value, Node, K);
      --  After its eight multiplications a value is the basis polynomial to within sixteen Units.

      procedure Lemma_Sample_Is_Built (Value : Sample; Node : Dimensionless)
      with
        Pre  =>
          Is_Copy (Value.Node, Node)
          and then Header_Is_Built (Value.Node, Value.Complement)
          and then (for all K in Chart_Coefficient_Index => Basis_Is_Built (Value.Basis (K), Node, K)),
        Post => Is_Built (Value);
      --  A built header and nine built basis values make a built sample.

      procedure Lemma_Samples_Are_Built (Samples : Sample_Array)
      with
        Pre  => (for all Index in Sample_Index => Is_Built (Samples (Index))),
        Post =>
          (for all Index in Sample_Index =>
             Is_In_Range (Samples (Index))
             and then (if Has_Margins (Samples (Index)) then Is_Accurate (Samples (Index))));
      --  What Is_Built means, for every sample at once.

   end Accuracy;

   package body Accuracy is

      --  A basis value is a binomial coefficient, which is exact, multiplied eight times: K times by the node and
      --  8 - K times by its rounded complement. Each product is rounded, by a relative Epsilon and, as far as the
      --  rounding lemmas tell, an absolute Eta. With the node inside its margins every exact partial product is
      --  at least 2**-160, which dwarfs Eta, so one multiplication by the node adds less than Unit to the relative
      --  error and one by the complement, itself rounded, less than twice Unit. Sixteen Units are
      --  Basis_Relative_Error.

      Unit : constant := 2.0**(-52);
      --  Twice Epsilon.

      Margin_Real : constant := 2.0**(-20);
      --  Node_Margin, as a real number.

      Largest_Allowance : constant := 2.0**(-48);
      --  Sixteen Units, which no relative error here exceeds.

      Smallest_Exact : constant := 2.0**(-160);
      --  Eight factors of Margin_Real: no exact partial product is smaller.

      Tiny_Ratio : constant := 2.0**(-900);
      --  Far more than Eta is of Smallest_Exact.

      function Has_Real_Margins (X : Real) return Boolean
      is (X >= Margin_Real and then X <= 1.0 - Margin_Real);
      --  Has_Margins, of the real number which a node denotes.

      function Allowance (Units : Natural) return Real
      is (To_Real (Units) * Unit);

      function Tracks (Value : Dimensionless; Exact : Real; Steps, Units : Natural) return Boolean
      is (Exact >= Bernstein.Binomial_Form.Power (Margin_Real, Steps)
          and then abs (R (Value) - Exact) <= Allowance (Units) * Exact);
      --  Whether Value is within Units relative Units of the exact product Exact of Steps factors, which is not
      --  small.

      function Exact_Product (K : Chart_Coefficient_Index; By_Node, By_Complement : Natural; X : Real) return Real
      is (Choose (Fixed_Chart_Degree, K)
          * Bernstein.Binomial_Form.Power (X, By_Node)
          * Bernstein.Binomial_Form.Power (1.0 - X, By_Complement));
      --  What a value approximates after so many of its multiplications.

      ------------------------------------------------------------------------
      --  The predicates
      ------------------------------------------------------------------------

      function Header_Is_Built (Node, Complement : Dimensionless) return Boolean
      with
        Refined_Post =>
          Header_Is_Built'Result
          = (Node >= 0.0
             and then Node <= 1.0
             and then Complement >= 0.0
             and then Complement <= 1.0
             and then Node + Complement >= 0.5
             and then Is_Rounding_Of (Complement, 1.0 - R (Node)))
      is
      begin
         return
           Node >= 0.0
           and then Node <= 1.0
           and then Complement >= 0.0
           and then Complement <= 1.0
           and then Node + Complement >= 0.5
           and then Is_Rounding_Of (Complement, 1.0 - R (Node));
      end Header_Is_Built;

      function Is_Copy (Stored, Original : Dimensionless) return Boolean
      with Refined_Post => Is_Copy'Result = (R (Stored) = R (Original))
      is
      begin
         return R (Stored) = R (Original);
      end Is_Copy;

      function Is_Binomial (Value : Dimensionless; K : Chart_Coefficient_Index) return Boolean
      with
        Refined_Post =>
          Is_Binomial'Result
          = (Value >= 1.0 and then Value <= Largest_Basis_Value and then R (Value) = Choose (Fixed_Chart_Degree, K))
      is
      begin
         return Value >= 1.0 and then Value <= Largest_Basis_Value and then R (Value) = Choose (Fixed_Chart_Degree, K);
      end Is_Binomial;

      function Is_Tracking
        (Value, Node : Dimensionless; K : Chart_Coefficient_Index; By_Node, By_Complement : Natural) return Boolean
      with
        Refined_Post =>
          Is_Tracking'Result
          = (By_Node <= Fixed_Chart_Degree
             and then By_Complement <= Fixed_Chart_Degree
             and then Value >= 0.0
             and then Value <= Largest_Basis_Value
             and then
               (if Has_Real_Margins (R (Node))
                then
                  Tracks
                    (Value,
                     Exact_Product (K, By_Node, By_Complement, R (Node)),
                     By_Node + By_Complement,
                     By_Node + 2 * By_Complement)))
      is
      begin
         return
           By_Node <= Fixed_Chart_Degree
           and then By_Complement <= Fixed_Chart_Degree
           and then Value >= 0.0
           and then Value <= Largest_Basis_Value
           and then
             (if Has_Real_Margins (R (Node))
              then
                Tracks
                  (Value,
                   Exact_Product (K, By_Node, By_Complement, R (Node)),
                   By_Node + By_Complement,
                   By_Node + 2 * By_Complement));
      end Is_Tracking;

      function Basis_Is_Built (Value, Node : Dimensionless; K : Chart_Coefficient_Index) return Boolean
      with
        Refined_Post =>
          Basis_Is_Built'Result
          = (Value >= 0.0
             and then Value <= Largest_Basis_Value
             and then
               (if Has_Real_Margins (R (Node))
                then
                  abs (R (Value) - Basis (Fixed_Chart_Degree, K, R (Node)))
                  <= Basis_Relative_Error * Basis (Fixed_Chart_Degree, K, R (Node))))
      is
      begin
         return
           Value >= 0.0
           and then Value <= Largest_Basis_Value
           and then
             (if Has_Real_Margins (R (Node))
              then
                abs (R (Value) - Basis (Fixed_Chart_Degree, K, R (Node)))
                <= Basis_Relative_Error * Basis (Fixed_Chart_Degree, K, R (Node)));
      end Basis_Is_Built;

      function Is_Built (Value : Sample) return Boolean
      with
        Refined_Post =>
          Is_Built'Result = (Is_In_Range (Value) and then (if Has_Margins (Value) then Is_Accurate (Value)))
      is
      begin
         return Is_In_Range (Value) and then (if Has_Margins (Value) then Is_Accurate (Value));
      end Is_Built;

      ------------------------------------------------------------------------
      --  Real facts
      ------------------------------------------------------------------------

      procedure Lemma_Floor (Steps : Natural)
      with
        Pre  => Steps <= Fixed_Chart_Degree,
        Post => Bernstein.Binomial_Form.Power (Margin_Real, Steps) >= Smallest_Exact;
      --  At most eight factors of Margin_Real.

      procedure Lemma_Floor (Steps : Natural) is
      begin
         pragma Assert (Bernstein.Binomial_Form.Power (Margin_Real, 0) = 1.0);
         pragma
           Assert
             (Bernstein.Binomial_Form.Power (Margin_Real, 1)
              = Margin_Real * Bernstein.Binomial_Form.Power (Margin_Real, 0));
         pragma
           Assert
             (Bernstein.Binomial_Form.Power (Margin_Real, 2)
              = Margin_Real * Bernstein.Binomial_Form.Power (Margin_Real, 1));
         pragma
           Assert
             (Bernstein.Binomial_Form.Power (Margin_Real, 3)
              = Margin_Real * Bernstein.Binomial_Form.Power (Margin_Real, 2));
         pragma
           Assert
             (Bernstein.Binomial_Form.Power (Margin_Real, 4)
              = Margin_Real * Bernstein.Binomial_Form.Power (Margin_Real, 3));
         pragma
           Assert
             (Bernstein.Binomial_Form.Power (Margin_Real, 5)
              = Margin_Real * Bernstein.Binomial_Form.Power (Margin_Real, 4));
         pragma
           Assert
             (Bernstein.Binomial_Form.Power (Margin_Real, 6)
              = Margin_Real * Bernstein.Binomial_Form.Power (Margin_Real, 5));
         pragma
           Assert
             (Bernstein.Binomial_Form.Power (Margin_Real, 7)
              = Margin_Real * Bernstein.Binomial_Form.Power (Margin_Real, 6));
         pragma
           Assert
             (Bernstein.Binomial_Form.Power (Margin_Real, 8)
              = Margin_Real * Bernstein.Binomial_Form.Power (Margin_Real, 7));
      end Lemma_Floor;

      procedure Lemma_Floor_Step (Exact, X : Real; Steps : Natural)
      with
        Pre  =>
          Steps < Fixed_Chart_Degree
          and then Exact >= Bernstein.Binomial_Form.Power (Margin_Real, Steps)
          and then X >= Margin_Real,
        Post =>
          Exact > 0.0
          and then Exact * X >= Bernstein.Binomial_Form.Power (Margin_Real, Steps + 1)
          and then Exact * X >= Smallest_Exact;
      --  One more factor of at least Margin_Real.

      procedure Lemma_Floor_Step (Exact, X : Real; Steps : Natural) is
         Floor : constant Real := Bernstein.Binomial_Form.Power (Margin_Real, Steps);
      begin
         Lemma_Floor (Steps);
         Lemma_Floor (Steps + 1);
         pragma Assert (Bernstein.Binomial_Form.Power (Margin_Real, Steps + 1) = Margin_Real * Floor);
         pragma Assert (Exact * X >= Floor * X);
         pragma Assert (Floor * X >= Floor * Margin_Real);
      end Lemma_Floor_Step;

      procedure Lemma_Node_Step_Real (F, V, P, X, T : Real)
      with
        Pre  =>
          P > 0.0
          and then X > 0.0
          and then P * X >= Smallest_Exact
          and then T >= 0.0
          and then T <= Largest_Allowance
          and then abs (V - P) <= T * P
          and then abs (F - V * X) <= Epsilon * abs (V * X) + Eta,
        Post => abs (F - P * X) <= (T + Unit) * (P * X);
      --  F is the rounded product of V, which is within T of the exact P, with the exact factor X.

      procedure Lemma_Node_Step_Real (F, V, P, X, T : Real) is
         PX  : constant Real := P * X;
         VX  : constant Real := V * X;
         TPX : constant Real := T * PX;
      begin
         pragma Assert ((V - P) * X = VX - PX);
         pragma Assert (abs (V - P) * X <= (T * P) * X);
         pragma Assert ((T * P) * X = TPX);
         pragma Assert (abs ((V - P) * X) = abs (V - P) * X);
         pragma Assert (abs (VX - PX) <= TPX);
         pragma Assert (TPX <= Largest_Allowance * PX);
         pragma Assert (Eta <= Tiny_Ratio * PX);
         pragma Assert (abs VX <= PX + TPX);
         pragma Assert (abs (F - VX) <= Epsilon * (PX + TPX) + Tiny_Ratio * PX);
         pragma Assert (abs (F - PX) <= TPX + Unit * PX);
         pragma Assert ((T + Unit) * PX = TPX + Unit * PX);
      end Lemma_Node_Step_Real;

      procedure Lemma_Complement_Step_Real (F, V, P, C, Y, T : Real)
      with
        Pre  =>
          P > 0.0
          and then Y > 0.0
          and then P * Y >= Smallest_Exact
          and then T >= 0.0
          and then T <= Largest_Allowance
          and then abs (V - P) <= T * P
          and then abs (C - Y) <= Unit * Y
          and then abs (F - V * C) <= Epsilon * abs (V * C) + Eta,
        Post => abs (F - P * Y) <= (T + 2.0 * Unit) * (P * Y);
      --  F is the rounded product of V, which is within T of the exact P, with C, which is within Unit of the
      --  exact factor Y.

      procedure Lemma_Complement_Step_Real (F, V, P, C, Y, T : Real) is
         PY  : constant Real := P * Y;
         VC  : constant Real := V * C;
         TPY : constant Real := T * PY;
      begin
         pragma Assert (C >= 0.0 and then C <= Y + Unit * Y);
         --  The error which V brings.
         pragma Assert (abs ((V - P) * C) = abs (V - P) * C);
         pragma Assert (abs (V - P) * C <= (T * P) * C);
         pragma Assert ((T * P) * C <= (T * P) * (Y + Unit * Y));
         pragma Assert ((T * P) * (Y + Unit * Y) = TPY + Unit * TPY);
         pragma Assert (abs ((V - P) * C) <= TPY + Unit * TPY);
         --  The error which C brings.
         pragma Assert (abs (P * (C - Y)) = P * abs (C - Y));
         pragma Assert (P * abs (C - Y) <= P * (Unit * Y));
         pragma Assert (P * (Unit * Y) = Unit * PY);
         pragma Assert (abs (P * (C - Y)) <= Unit * PY);
         --  Together, and the rounding of the product.
         pragma Assert (VC - PY = (V - P) * C + P * (C - Y));
         pragma Assert (abs (VC - PY) <= TPY + Unit * TPY + Unit * PY);
         pragma Assert (TPY <= Largest_Allowance * PY);
         pragma Assert (Eta <= Tiny_Ratio * PY);
         pragma Assert (abs VC <= PY + TPY + Unit * TPY + Unit * PY);
         pragma Assert (abs (F - VC) <= Epsilon * (PY + TPY + Unit * TPY + Unit * PY) + Tiny_Ratio * PY);
         pragma Assert (abs (F - PY) <= TPY + 2.0 * Unit * PY);
         pragma Assert ((T + 2.0 * Unit) * PY = TPY + 2.0 * Unit * PY);
      end Lemma_Complement_Step_Real;

      procedure Lemma_Allowance (Units : Natural)
      with
        Pre  => Units <= 2 * Fixed_Chart_Degree,
        Post =>
          Allowance (Units) >= 0.0
          and then Allowance (Units) <= Largest_Allowance
          and then Allowance (Units + 1) = Allowance (Units) + Unit
          and then Allowance (Units + 2) = Allowance (Units) + 2.0 * Unit;

      procedure Lemma_Allowance (Units : Natural) is
      begin
         pragma Assert (To_Real (Units) >= 0.0 and then To_Real (Units) <= 16.0);
         pragma Assert (To_Real (Units + 1) = To_Real (Units) + 1.0);
         pragma Assert (To_Real (Units + 2) = To_Real (Units) + 2.0);
      end Lemma_Allowance;

      ------------------------------------------------------------------------
      --  One rounded multiplication
      ------------------------------------------------------------------------

      procedure Lemma_Node_Product (Value, Node : Dimensionless; Exact : Real; Steps, Units : Natural)
      with
        Pre  =>
          Value >= 0.0
          and then Value <= Largest_Basis_Value
          and then Node >= 0.0
          and then Node <= 1.0
          and then Steps < Fixed_Chart_Degree
          and then Units < 2 * Fixed_Chart_Degree,
        Post =>
          Value * Node >= 0.0
          and then Value * Node <= Largest_Basis_Value
          and then
            (if Has_Real_Margins (R (Node)) and then Tracks (Value, Exact, Steps, Units)
             then Tracks (Value * Node, Exact * R (Node), Steps + 1, Units + 1));
      --  The product stays in range, whatever the node, and with margins it tracks the exact product with one
      --  more Unit of relative error.

      procedure Lemma_Node_Product (Value, Node : Dimensionless; Exact : Real; Steps, Units : Natural) is
         X : constant Real := R (Node);
      begin
         if Has_Real_Margins (X) and then Tracks (Value, Exact, Steps, Units) then
            Float_Lemmas.Lemma_Rounding_Error_Mul (Value, Node);
            Lemma_Floor_Step (Exact, X, Steps);
            Lemma_Allowance (Units);
            Lemma_Node_Step_Real (R (Value * Node), R (Value), Exact, X, Allowance (Units));
         end if;
      end Lemma_Node_Product;

      procedure Lemma_Complement_Product
        (Value, Node, Complement : Dimensionless; Exact : Real; Steps, Units : Natural)
      with
        Pre  =>
          Value >= 0.0
          and then Value <= Largest_Basis_Value
          and then Node >= 0.0
          and then Node <= 1.0
          and then Complement >= 0.0
          and then Complement <= 1.0
          and then Is_Rounding_Of (Complement, 1.0 - R (Node))
          and then Steps < Fixed_Chart_Degree
          and then Units <= 2 * Fixed_Chart_Degree - 2,
        Post =>
          Value * Complement >= 0.0
          and then Value * Complement <= Largest_Basis_Value
          and then
            (if Has_Real_Margins (R (Node)) and then Tracks (Value, Exact, Steps, Units)
             then Tracks (Value * Complement, Exact * (1.0 - R (Node)), Steps + 1, Units + 2));
      --  As for the node, but the rounded complement adds two Units.

      procedure Lemma_Complement_Product
        (Value, Node, Complement : Dimensionless; Exact : Real; Steps, Units : Natural)
      is
         Y : constant Real := 1.0 - R (Node);
      begin
         if Has_Real_Margins (R (Node)) and then Tracks (Value, Exact, Steps, Units) then
            Float_Lemmas.Lemma_Rounding_Error_Mul (Value, Complement);
            Lemma_Floor_Step (Exact, Y, Steps);
            Lemma_Allowance (Units);
            pragma Assert (abs (R (Complement) - Y) <= Epsilon * Y + Eta);
            pragma Assert (Eta <= Epsilon * Y);
            pragma Assert (abs (R (Complement) - Y) <= Unit * Y);
            Lemma_Complement_Step_Real
              (R (Value * Complement), R (Value), Exact, R (Complement), Y, Allowance (Units));
         end if;
      end Lemma_Complement_Product;

      procedure Lemma_Final_Product (Value : Dimensionless; K : Chart_Coefficient_Index; X, Exact : Real)
      with
        Pre  =>
          Exact = Exact_Product (K, K, Fixed_Chart_Degree - K, X)
          and then
            (if Has_Real_Margins (X) then Tracks (Value, Exact, Fixed_Chart_Degree, 2 * Fixed_Chart_Degree - K)),
        Post =>
          (if Has_Real_Margins (X)
           then
             abs (R (Value) - Basis (Fixed_Chart_Degree, K, X))
             <= Basis_Relative_Error * Basis (Fixed_Chart_Degree, K, X));

      procedure Lemma_Final_Product (Value : Dimensionless; K : Chart_Coefficient_Index; X, Exact : Real) is
      begin
         pragma Assert (Exact = Basis (Fixed_Chart_Degree, K, X));
         if Has_Real_Margins (X) then
            Lemma_Floor (Fixed_Chart_Degree);
            Lemma_Allowance (2 * Fixed_Chart_Degree - K);
            pragma Assert (Exact > 0.0);
            pragma Assert (Allowance (2 * Fixed_Chart_Degree - K) * Exact <= Largest_Allowance * Exact);
            pragma Assert (abs (R (Value) - Exact) <= Basis_Relative_Error * Exact);
         end if;
      end Lemma_Final_Product;

      procedure Lemma_Header (Cosine : Dimensionless)
      with
        Pre  => Cosine >= -1.0 and then Cosine <= 1.0,
        Post =>
          1.0 - Cosine >= 0.0
          and then 1.0 - Cosine <= 2.0
          and then 0.5 * (1.0 - Cosine) >= 0.0
          and then 0.5 * (1.0 - Cosine) <= 1.0
          and then 1.0 - 0.5 * (1.0 - Cosine) >= 0.0
          and then 1.0 - 0.5 * (1.0 - Cosine) <= 1.0
          and then 0.5 * (1.0 - Cosine) + (1.0 - 0.5 * (1.0 - Cosine)) >= 0.5
          and then Is_Rounding_Of (1.0 - 0.5 * (1.0 - Cosine), 1.0 - R (0.5 * (1.0 - Cosine)));
      --  The node is in [0, 1] for any cosine, its complement likewise, and the complement is one rounded
      --  subtraction.

      procedure Lemma_Header (Cosine : Dimensionless) is
         Node : constant Dimensionless := 0.5 * (1.0 - Cosine);
      begin
         pragma Assert (1.0 - Cosine >= 0.0 and then 1.0 - Cosine <= 2.0);
         pragma Assert (Node >= 0.0 and then Node <= 1.0);
         Float_Lemmas.Lemma_Rounding_Error_Sub (1.0, Node);
         pragma Assert (1.0 - Node >= 0.0 and then 1.0 - Node <= 1.0);
         pragma Assert (Node + (1.0 - Node) >= 0.5);
      end Lemma_Header;

      procedure Lemma_Is_Binomial (Value : Dimensionless; K : Chart_Coefficient_Index)
      with
        Pre  => Value >= 1.0 and then Value <= Largest_Basis_Value and then R (Value) = Choose (Fixed_Chart_Degree, K),
        Post => Is_Binomial (Value, K);
      --  What Is_Binomial means, away from the table.

      procedure Lemma_Is_Binomial (Value : Dimensionless; K : Chart_Coefficient_Index) is null;

      procedure Lemma_Binomial_Low (K : Chart_Coefficient_Index)
      with Pre => K in 0 .. 2, Post => Is_Binomial (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K), K);
      --  The table at one of three columns. The loop is unrolled by the tool, so that each pass reads the table
      --  at a literal column; and the columns are taken three at a time, here and in the two lemmas which follow,
      --  because the table's literal aggregate is among the premises and makes every further case costly.

      procedure Lemma_Binomial_Low (K : Chart_Coefficient_Index) is
      begin
         Interval_Polynomials.Lemma_Binomials_Are_Exact (Fixed_Chart_Degree, K);
         for Column in 0 .. 2 loop
            if Column = K then
               pragma Assert (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K) >= 1.0);
               pragma Assert (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K) <= Largest_Basis_Value);
               Lemma_Is_Binomial (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K), K);
               return;
            end if;
         end loop;
      end Lemma_Binomial_Low;

      procedure Lemma_Binomial_Middle (K : Chart_Coefficient_Index)
      with Pre => K in 3 .. 5, Post => Is_Binomial (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K), K);

      procedure Lemma_Binomial_Middle (K : Chart_Coefficient_Index) is
      begin
         Interval_Polynomials.Lemma_Binomials_Are_Exact (Fixed_Chart_Degree, K);
         for Column in 3 .. 5 loop
            if Column = K then
               pragma Assert (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K) >= 1.0);
               pragma Assert (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K) <= Largest_Basis_Value);
               Lemma_Is_Binomial (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K), K);
               return;
            end if;
         end loop;
      end Lemma_Binomial_Middle;

      procedure Lemma_Binomial_High (K : Chart_Coefficient_Index)
      with Pre => K in 6 .. 8, Post => Is_Binomial (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K), K);

      procedure Lemma_Binomial_High (K : Chart_Coefficient_Index) is
      begin
         Interval_Polynomials.Lemma_Binomials_Are_Exact (Fixed_Chart_Degree, K);
         for Column in 6 .. 8 loop
            if Column = K then
               pragma Assert (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K) >= 1.0);
               pragma Assert (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K) <= Largest_Basis_Value);
               Lemma_Is_Binomial (Interval_Polynomials.Binomials (Fixed_Chart_Degree, K), K);
               return;
            end if;
         end loop;
      end Lemma_Binomial_High;

      procedure Lemma_Binomial (K : Chart_Coefficient_Index) is
      begin
         if K <= 2 then
            Lemma_Binomial_Low (K);
         elsif K <= 5 then
            Lemma_Binomial_Middle (K);
         else
            Lemma_Binomial_High (K);
         end if;
      end Lemma_Binomial;

      procedure Lemma_Start (Value, Node : Dimensionless; K : Chart_Coefficient_Index) is
         X : constant Real := R (Node);
      begin
         pragma
           Assert
             (Bernstein.Binomial_Form.Power (X, 0) = 1.0 and then Bernstein.Binomial_Form.Power (1.0 - X, 0) = 1.0);
         pragma Assert (Exact_Product (K, 0, 0, X) = R (Value));
         pragma Assert (Bernstein.Binomial_Form.Power (Margin_Real, 0) = 1.0);
         pragma Assert (Allowance (0) = 0.0);
         pragma Assert (Tracks (Value, Exact_Product (K, 0, 0, X), 0, 0));
      end Lemma_Start;

      ------------------------------------------------------------------------
      --  The steps of Build
      ------------------------------------------------------------------------

      function Node_Is_Built (Angle : Dimensionless) return Boolean is
      begin
         Lemma_Header (Dimensionless_Math.Cos (Angle));
         return True;
      end Node_Is_Built;

      procedure Lemma_Copy (Node : Dimensionless) is null;

      procedure Lemma_Node_Step (Value, Node : Dimensionless; K : Chart_Coefficient_Index; Done : Natural) is
         X : constant Real := R (Node);
      begin
         Lemma_Node_Product (Value, Node, Exact_Product (K, Done, 0, X), Done, Done);
         pragma Assert (Bernstein.Binomial_Form.Power (X, Done + 1) = X * Bernstein.Binomial_Form.Power (X, Done));
         pragma Assert (Exact_Product (K, Done + 1, 0, X) = Exact_Product (K, Done, 0, X) * X);
      end Lemma_Node_Step;

      procedure Lemma_Complement_Step
        (Value, Node, Complement : Dimensionless; K : Chart_Coefficient_Index; Done : Natural)
      is
         X : constant Real := R (Node);
      begin
         Lemma_Complement_Product (Value, Node, Complement, Exact_Product (K, K, Done, X), K + Done, K + 2 * Done);
         pragma
           Assert
             (Bernstein.Binomial_Form.Power (1.0 - X, Done + 1)
              = (1.0 - X) * Bernstein.Binomial_Form.Power (1.0 - X, Done));
         pragma Assert (Exact_Product (K, K, Done + 1, X) = Exact_Product (K, K, Done, X) * (1.0 - X));
      end Lemma_Complement_Step;

      procedure Lemma_Basis_Is_Built (Value, Node : Dimensionless; K : Chart_Coefficient_Index) is
      begin
         Lemma_Final_Product (Value, K, R (Node), Exact_Product (K, K, Fixed_Chart_Degree - K, R (Node)));
      end Lemma_Basis_Is_Built;

      procedure Lemma_Margins (Value : Sample)
      with Post => Has_Margins (Value) = Has_Real_Margins (R (Value.Node));
      --  The margins of a node are those of the real number which it denotes.

      procedure Lemma_Margins (Value : Sample) is null;

      procedure Lemma_Sample_Is_Built (Value : Sample; Node : Dimensionless) is
      begin
         Lemma_Margins (Value);
         pragma Assert (R (Value.Node) = R (Node));
         pragma
           Assert
             (for all K in Chart_Coefficient_Index =>
                Value.Basis (K) >= 0.0 and then Value.Basis (K) <= Largest_Basis_Value);
         pragma Assert (Is_In_Range (Value));
         if Has_Margins (Value) then
            pragma
              Assert (abs (R (Value.Complement) - (1.0 - R (Value.Node))) <= Epsilon * (1.0 - R (Value.Node)) + Eta);
            pragma
              Assert
                (for all K in Chart_Coefficient_Index =>
                   abs (R (Value.Basis (K)) - Basis (Fixed_Chart_Degree, K, R (Value.Node)))
                   <= Basis_Relative_Error * Basis (Fixed_Chart_Degree, K, R (Value.Node)));
            pragma Assert (Is_Accurate (Value));
         end if;
      end Lemma_Sample_Is_Built;

      procedure Lemma_Samples_Are_Built (Samples : Sample_Array) is
      begin
         for Index in Sample_Index loop
            pragma Assert (Is_Built (Samples (Index)));
            pragma
              Loop_Invariant
                (for all Earlier in 1 .. Index =>
                   Is_In_Range (Samples (Earlier))
                   and then (if Has_Margins (Samples (Earlier)) then Is_Accurate (Samples (Earlier))));
         end loop;
      end Lemma_Samples_Are_Built;

   end Accuracy;

   use Accuracy;

   function Binomial_Coefficient (K : Chart_Coefficient_Index) return Dimensionless
   with Inline, Post => Is_Binomial (Binomial_Coefficient'Result, K);
   --  Interval_Polynomials.Binomials (Fixed_Chart_Degree, K), read through a contract. A direct read brings the
   --  table's literal aggregate into every proof obligation of the reading subprogram, and with it among their
   --  premises the provers fail on obligations of Build which they otherwise prove at once.

   function Binomial_Coefficient (K : Chart_Coefficient_Index) return Dimensionless is
   begin
      Lemma_Binomial (K);
      return Interval_Polynomials.Binomials (Fixed_Chart_Degree, K);
   end Binomial_Coefficient;

   function Build return Sample_Array is
      Result : Sample_Array := [others => (Node => 0.0, Complement => 1.0, Basis => [others => 0.0])];
   begin
      for Index in Sample_Index loop
         declare
            Angle : constant Dimensionless :=
              Dimensionless (2 * Index - 1) * Ada.Numerics.Pi / Dimensionless (2 * Sample_Count);

            Checked : constant Boolean := Node_Is_Built (Angle)
            with Ghost;
            --  Stated here for its postcondition, which the two declarations below rest on.

            Node       : constant Dimensionless := 0.5 * (1.0 - Dimensionless_Math.Cos (Angle));
            Complement : constant Dimensionless := 1.0 - Node;
         begin
            pragma Assert (Checked);
            Lemma_Copy (Node);
            Result (Index).Node := Node;
            Result (Index).Complement := Complement;
            for K in Chart_Coefficient_Index loop
               declare
                  Value : Dimensionless := Binomial_Coefficient (K);
               begin
                  Lemma_Start (Value, Node, K);
                  for Factor in 1 .. K loop
                     Lemma_Node_Step (Value, Node, K, Factor - 1);
                     Value := Value * Node;
                     pragma Loop_Invariant (Is_Tracking (Value, Node, K, Factor, 0));
                  end loop;
                  for Factor in 1 .. Fixed_Chart_Degree - K loop
                     Lemma_Complement_Step (Value, Node, Complement, K, Factor - 1);
                     Value := Value * Complement;
                     pragma Loop_Invariant (Is_Tracking (Value, Node, K, K, Factor));
                  end loop;
                  Lemma_Basis_Is_Built (Value, Node, K);
                  Result (Index).Basis (K) := Value;
                  --  The new value and the earlier ones separately, which is how the provers find the invariant.
                  pragma Assert (Basis_Is_Built (Result (Index).Basis (K), Node, K));
                  pragma Assert (for all J in 0 .. K - 1 => Basis_Is_Built (Result (Index).Basis (J), Node, J));
               end;
               pragma Loop_Invariant (for all Earlier in 1 .. Index - 1 => Is_Built (Result (Earlier)));
               pragma Loop_Invariant (Is_Copy (Result (Index).Node, Node));
               pragma Loop_Invariant (Header_Is_Built (Result (Index).Node, Result (Index).Complement));
               pragma Loop_Invariant (for all J in 0 .. K => Basis_Is_Built (Result (Index).Basis (J), Node, J));
            end loop;
            Lemma_Sample_Is_Built (Result (Index), Node);
         end;
         pragma Loop_Invariant (for all Earlier in 1 .. Index => Is_Built (Result (Earlier)));
      end loop;
      Lemma_Samples_Are_Built (Result);
      return Result;
   end Build;

   ---------------------------------------------------------------------------
   --  The Lebesgue function
   ---------------------------------------------------------------------------
   --
   --  With ℓᵢ(X) = Πⱼ≠ᵢ (X - xⱼ)/(xᵢ - xⱼ) the Lagrange basis of the nodes, the Lebesgue function is Σᵢ |ℓᵢ(X)|.
   --  On a cell [A, B] every |X - xⱼ| is at most max(|A - xⱼ|, |B - xⱼ|), so
   --
   --     Σᵢ |ℓᵢ(X)| ≤ Σᵢ wᵢ·Πⱼ≠ᵢ max(|A - xⱼ|, |B - xⱼ|),     wᵢ = 1/Πⱼ≠ᵢ |xᵢ - xⱼ|.
   --
   --  Every quantity is positive, so rounding each operation upward, or downward under the reciprocal, keeps the
   --  bound. A cell whose bound is too large is halved. About nine hundred cells are examined, once, when the
   --  program starts.

   type Weight_Array is array (Sample_Index) of Dimensionless;

   type Weight_Table is record
      Weights : Weight_Array;
      Valid   : Boolean;
   end record;
   --  The weights, and whether they could be bounded.

   Largest_Weight : constant Dimensionless := 2.0**200;
   --  Weights are near 1.0E18. A bound on them keeps every product below finite.

   Largest_Product : constant Dimensionless := 2.0;
   --  A product of distances within [0, 1] is at most one before rounding.

   Maximum_Depth : constant := 16;

   ---------------------------------------------------------------------------
   --  What the check establishes
   ---------------------------------------------------------------------------
   --
   --  As for Build, the functions of the check are annotated with predicates which mean nothing outside this
   --  package, and with one lemma for each of their steps, so that their own proof obligations need no arithmetic.
   --  Inside the package the proof has three layers. Upper_Sum is the right-hand side above as a real number, for
   --  weights which are floating-point numbers: the lemmas for Cell_Bound show that it returns no less. The lemmas
   --  for Build_Weights show that its weights are no less than the wᵢ. For such weights Upper_Sum is no less
   --  than the Lebesgue function anywhere in the cell, term by term.

   package Bounds
     with Ghost
   is

      function Nodes_Are_In_Unit (Samples : Sample_Array) return Boolean;
      --  Whether every node is in [0, 1].

      function Is_Denominator_Bound
        (Samples : Sample_Array; I : Sample_Index; Count : Natural; Product : Dimensionless) return Boolean;
      --  Whether Product is a lower bound, at least the reciprocal of Largest_Weight and at most one, on the
      --  product of |xᵢ - xⱼ| over the first Count nodes j other than I.

      function Weight_Is_Sound (Samples : Sample_Array; I : Sample_Index; Weight : Dimensionless) return Boolean;
      --  Whether Weight is within [0, Largest_Weight] and at least wᵢ.

      function Weights_Are_Sound (Samples : Sample_Array; Weights : Weight_Array) return Boolean;
      --  Whether every weight is sound.

      function Reach_Is_Sound (Samples : Sample_Array; J : Sample_Index; A, B, Reach : Dimensionless) return Boolean;
      --  Whether Reach is within [0, Largest_Product] and at least the distance from node J to any point of
      --  [A, B].

      function Is_Product_Bound
        (Samples : Sample_Array; I : Sample_Index; A, B : Dimensionless; Count : Natural; Product : Dimensionless)
         return Boolean;
      --  Whether Product is within [0, Largest_Product] and at least the product of the reaches of the first
      --  Count nodes other than I.

      function Is_Sum_Bound
        (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless; Count : Natural; Sum : Dimensionless)
         return Boolean;
      --  Whether Sum is within [0, Largest_Weight] and at least the first Count terms of the bound over [A, B].

      function Is_Cell_Bound
        (Samples : Sample_Array; Weights : Weight_Array; A, B, Bound : Dimensionless) return Boolean;
      --  Whether Bound is at least the whole bound over [A, B].

      procedure Lemma_Nodes_Are_In_Unit (Samples : Sample_Array)
      with
        Pre  => (for all Index in Sample_Index => Is_In_Range (Samples (Index))),
        Post => Nodes_Are_In_Unit (Samples);

      --  The weights

      procedure Lemma_Denominator_Start (Samples : Sample_Array; I : Sample_Index)
      with Post => Is_Denominator_Bound (Samples, I, 0, 1.0);

      procedure Lemma_Denominator_Step (Samples : Sample_Array; I, J : Sample_Index; Product : Dimensionless)
      with
        Pre  => Nodes_Are_In_Unit (Samples) and then J /= I and then Is_Denominator_Bound (Samples, I, J - 1, Product),
        Post =>
          Samples (I).Node - Samples (J).Node > -2.0
          and then Samples (I).Node - Samples (J).Node < 2.0
          and then Product * Down (abs (Samples (I).Node - Samples (J).Node)) > -Dimensionless'Last
          and then Product * Down (abs (Samples (I).Node - Samples (J).Node)) < Dimensionless'Last
          and then
            (if Down (Product * Down (abs (Samples (I).Node - Samples (J).Node))) >= 1.0 / Largest_Weight
             then
               Is_Denominator_Bound
                 (Samples, I, J, Down (Product * Down (abs (Samples (I).Node - Samples (J).Node)))));
      --  One more factor: the two paddings take the product below the exact one. The strict bounds are for the
      --  overflow checks of the difference and of the product.

      procedure Lemma_Denominator_Skip (Samples : Sample_Array; I : Sample_Index; Product : Dimensionless)
      with
        Pre  => Is_Denominator_Bound (Samples, I, I - 1, Product),
        Post => Is_Denominator_Bound (Samples, I, I, Product);
      --  Node I itself contributes no factor.

      procedure Lemma_Weight (Samples : Sample_Array; I : Sample_Index; Product : Dimensionless)
      with
        Pre  => Is_Denominator_Bound (Samples, I, Sample_Count, Product),
        Post =>
          Product >= 1.0 / Largest_Weight
          and then Product <= 1.0
          and then (if Up (1.0 / Product) <= Largest_Weight then Weight_Is_Sound (Samples, I, Up (1.0 / Product)));
      --  The padded reciprocal of a lower bound on the denominator is at least wᵢ.

      procedure Lemma_Weights_Are_Sound (Samples : Sample_Array; Weights : Weight_Array)
      with
        Pre  => (for all I in Sample_Index => Weight_Is_Sound (Samples, I, Weights (I))),
        Post => Weights_Are_Sound (Samples, Weights);

      --  The bound over one cell

      procedure Lemma_Reach (Samples : Sample_Array; J : Sample_Index; A, B : Dimensionless)
      with
        Pre  => Nodes_Are_In_Unit (Samples) and then A >= 0.0 and then A <= B and then B <= 1.0,
        Post =>
          abs (A - Samples (J).Node) <= 1.0
          and then abs (B - Samples (J).Node) <= 1.0
          and then
            (if Up (Dimensionless'Max (abs (A - Samples (J).Node), abs (B - Samples (J).Node))) <= Largest_Product
             then
               Reach_Is_Sound
                 (Samples, J, A, B, Up (Dimensionless'Max (abs (A - Samples (J).Node), abs (B - Samples (J).Node)))));
      --  The padded larger distance from a node to the ends of a cell is at least its distance to any point of
      --  the cell.

      procedure Lemma_Product_Start (Samples : Sample_Array; I : Sample_Index; A, B : Dimensionless)
      with Post => Is_Product_Bound (Samples, I, A, B, 0, 1.0);

      procedure Lemma_Product_Step (Samples : Sample_Array; I, J : Sample_Index; A, B, Product, Reach : Dimensionless)
      with
        Pre  =>
          J /= I
          and then Is_Product_Bound (Samples, I, A, B, J - 1, Product)
          and then Reach_Is_Sound (Samples, J, A, B, Reach),
        Post =>
          abs (Product * Reach) <= Half_Last
          and then
            (if Up (Product * Reach) <= Largest_Product
             then Is_Product_Bound (Samples, I, A, B, J, Up (Product * Reach)));
      --  One more reach.

      procedure Lemma_Product_Skip (Samples : Sample_Array; I : Sample_Index; A, B, Product : Dimensionless)
      with
        Pre  => Is_Product_Bound (Samples, I, A, B, I - 1, Product),
        Post => Is_Product_Bound (Samples, I, A, B, I, Product);

      procedure Lemma_Sum_Start (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless)
      with Post => Is_Sum_Bound (Samples, Weights, A, B, 0, 0.0);

      procedure Lemma_Sum_Step
        (Samples : Sample_Array; Weights : Weight_Array; I : Sample_Index; A, B, Sum, Product : Dimensionless)
      with
        Pre  =>
          Is_Sum_Bound (Samples, Weights, A, B, I - 1, Sum)
          and then Is_Product_Bound (Samples, I, A, B, Sample_Count, Product)
          and then Weights (I) >= 0.0
          and then Weights (I) <= Largest_Weight,
        Post =>
          abs (Product * Weights (I)) <= Half_Last
          and then abs (Sum + Up (Product * Weights (I))) <= Half_Last
          and then
            (if Up (Sum + Up (Product * Weights (I))) <= Largest_Weight
             then Is_Sum_Bound (Samples, Weights, A, B, I, Up (Sum + Up (Product * Weights (I)))));
      --  One more term.

      procedure Lemma_Cell_Bound (Samples : Sample_Array; Weights : Weight_Array; A, B, Sum : Dimensionless)
      with
        Pre  => Is_Sum_Bound (Samples, Weights, A, B, Sample_Count, Sum),
        Post => Is_Cell_Bound (Samples, Weights, A, B, Sum);

      --  What the bound is a bound on

      procedure Lemma_Cell_Bound_Is_Sound
        (Samples : Sample_Array; Weights : Weight_Array; A, B, Bound : Dimensionless; X : Real)
      with
        Pre  =>
          Weights_Are_Sound (Samples, Weights)
          and then Is_Cell_Bound (Samples, Weights, A, B, Bound)
          and then R (A) <= X
          and then X <= R (B),
        Post => Lebesgue_Sum (Node_Reals (Samples), X) <= R (Bound);
      --  With sound weights the bound over a cell is at least the Lebesgue function at every point of the cell.

      procedure Lemma_Nodes_Increase (Samples : Sample_Array)
      with
        Pre  => (for all Index in 1 .. Sample_Count - 1 => Samples (Index).Node < Samples (Index + 1).Node),
        Post => Is_Increasing (Node_Reals (Samples));

   end Bounds;

   package body Bounds is

      Weight_Limit : constant := 2.0**200;
      --  Largest_Weight, as a real number.

      Weight_Floor : constant := 2.0**(-200);
      --  Its reciprocal, below which no product behind a weight may fall.

      Padded_Bound : constant := 2.0**202;
      Padded_Limit : constant Dimensionless := Padded_Bound;
      --  No padded product of a factor within Largest_Product and one within Largest_Weight exceeds this.

      Pad_Scale : constant := 2.0**(-51);
      --  Two model epsilons, as a real number.

      Smallest_Pad : constant := 2.0**(-1022);
      --  Model_Small, as a real number.

      Growth_Factor : constant := 1.0 + 2.0**(-50);
      Growth_Offset : constant := 2.0**(-1021);
      --  Inflate_Finite takes a magnitude M to at most Growth_Factor * M + Growth_Offset.

      ------------------------------------------------------------------------
      --  The bound as a real number
      ------------------------------------------------------------------------

      function Farthest (Node, A, B : Real) return Real
      is (if abs (A - Node) >= abs (B - Node) then abs (A - Node) else abs (B - Node));
      --  The largest distance from Node to a point of [A, B].

      function Upper_Product (Nodes : Node_Array; I : Positive; A, B : Real; Count : Natural) return Real
      is (if Count = 0
          then 1.0
          elsif Count = I
          then Upper_Product (Nodes, I, A, B, Count - 1)
          else Upper_Product (Nodes, I, A, B, Count - 1) * Farthest (Nodes (Count), A, B))
      with Pre => Nodes'First = 1 and then Count <= Nodes'Last, Subprogram_Variant => (Decreases => Count);
      --  The product of Farthest (Nodes (J), A, B) over J in 1 .. Count other than I, which bounds the magnitude
      --  of Numerator (Nodes, I, X, Count) for X in [A, B].

      function Upper_Sum (Nodes : Node_Array; Weights : Weight_Array; A, B : Real; Count : Natural) return Real
      is (if Count = 0
          then 0.0
          else
            Upper_Sum (Nodes, Weights, A, B, Count - 1)
            + R (Weights (Count)) * Upper_Product (Nodes, Count, A, B, Sample_Count))
      with
        Pre                => Nodes'First = 1 and then Nodes'Last = Sample_Count and then Count <= Sample_Count,
        Subprogram_Variant => (Decreases => Count);
      --  The first Count terms of the bound on the Lebesgue function over [A, B].

      function Is_Sound_For (Nodes : Node_Array; I : Sample_Index; Weight : Dimensionless) return Boolean
      is (Weight >= 0.0
          and then Weight <= Largest_Weight
          and then R (Weight) * abs (Weight_Denominator (Nodes, I)) >= 1.0)
      with Pre => Nodes'First = 1 and then Nodes'Last = Sample_Count;
      --  Whether Weight is at least wᵢ, stated multiplied through by the denominator, which it shows to be
      --  nonzero.

      ------------------------------------------------------------------------
      --  The predicates
      ------------------------------------------------------------------------

      function Nodes_Are_In_Unit (Samples : Sample_Array) return Boolean
      with
        Refined_Post =>
          Nodes_Are_In_Unit'Result
          = (for all Index in Sample_Index => Samples (Index).Node >= 0.0 and then Samples (Index).Node <= 1.0)
      is
      begin
         return (for all Index in Sample_Index => Samples (Index).Node >= 0.0 and then Samples (Index).Node <= 1.0);
      end Nodes_Are_In_Unit;

      function Is_Denominator_Bound
        (Samples : Sample_Array; I : Sample_Index; Count : Natural; Product : Dimensionless) return Boolean
      with
        Refined_Post =>
          Is_Denominator_Bound'Result
          = (Count <= Sample_Count
             and then Product >= 1.0 / Largest_Weight
             and then Product <= 1.0
             and then R (Product) <= abs (Numerator (Node_Reals (Samples), I, R (Samples (I).Node), Count)))
      is
      begin
         return
           Count <= Sample_Count
           and then Product >= 1.0 / Largest_Weight
           and then Product <= 1.0
           and then R (Product) <= abs (Numerator (Node_Reals (Samples), I, R (Samples (I).Node), Count));
      end Is_Denominator_Bound;

      function Weight_Is_Sound (Samples : Sample_Array; I : Sample_Index; Weight : Dimensionless) return Boolean
      with Refined_Post => Weight_Is_Sound'Result = Is_Sound_For (Node_Reals (Samples), I, Weight)
      is
      begin
         return Is_Sound_For (Node_Reals (Samples), I, Weight);
      end Weight_Is_Sound;

      function Weights_Are_Sound (Samples : Sample_Array; Weights : Weight_Array) return Boolean
      with
        Refined_Post =>
          Weights_Are_Sound'Result = (for all I in Sample_Index => Is_Sound_For (Node_Reals (Samples), I, Weights (I)))
      is
      begin
         return (for all I in Sample_Index => Is_Sound_For (Node_Reals (Samples), I, Weights (I)));
      end Weights_Are_Sound;

      function Reach_Is_Sound (Samples : Sample_Array; J : Sample_Index; A, B, Reach : Dimensionless) return Boolean
      with
        Refined_Post =>
          Reach_Is_Sound'Result
          = (Reach >= 0.0
             and then Reach <= Largest_Product
             and then R (Reach) >= Farthest (R (Samples (J).Node), R (A), R (B)))
      is
      begin
         return
           Reach >= 0.0
           and then Reach <= Largest_Product
           and then R (Reach) >= Farthest (R (Samples (J).Node), R (A), R (B));
      end Reach_Is_Sound;

      function Is_Product_Bound
        (Samples : Sample_Array; I : Sample_Index; A, B : Dimensionless; Count : Natural; Product : Dimensionless)
         return Boolean
      with
        Refined_Post =>
          Is_Product_Bound'Result
          = (Count <= Sample_Count
             and then Product >= 0.0
             and then Product <= Largest_Product
             and then Upper_Product (Node_Reals (Samples), I, R (A), R (B), Count) >= 0.0
             and then R (Product) >= Upper_Product (Node_Reals (Samples), I, R (A), R (B), Count))
      is
      begin
         return
           Count <= Sample_Count
           and then Product >= 0.0
           and then Product <= Largest_Product
           and then Upper_Product (Node_Reals (Samples), I, R (A), R (B), Count) >= 0.0
           and then R (Product) >= Upper_Product (Node_Reals (Samples), I, R (A), R (B), Count);
      end Is_Product_Bound;

      function Is_Sum_Bound
        (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless; Count : Natural; Sum : Dimensionless)
         return Boolean
      with
        Refined_Post =>
          Is_Sum_Bound'Result
          = (Count <= Sample_Count
             and then Sum >= 0.0
             and then Sum <= Largest_Weight
             and then R (Sum) >= Upper_Sum (Node_Reals (Samples), Weights, R (A), R (B), Count))
      is
      begin
         return
           Count <= Sample_Count
           and then Sum >= 0.0
           and then Sum <= Largest_Weight
           and then R (Sum) >= Upper_Sum (Node_Reals (Samples), Weights, R (A), R (B), Count);
      end Is_Sum_Bound;

      function Is_Cell_Bound
        (Samples : Sample_Array; Weights : Weight_Array; A, B, Bound : Dimensionless) return Boolean
      with
        Refined_Post =>
          Is_Cell_Bound'Result = (R (Bound) >= Upper_Sum (Node_Reals (Samples), Weights, R (A), R (B), Sample_Count))
      is
      begin
         return R (Bound) >= Upper_Sum (Node_Reals (Samples), Weights, R (A), R (B), Sample_Count);
      end Is_Cell_Bound;

      ------------------------------------------------------------------------
      --  Floating-point facts
      ------------------------------------------------------------------------

      procedure Lemma_Scale (Value : Dimensionless)
      with
        Pre  => Value >= 0.0 and then Value <= Half_Last,
        Post =>
          abs (R (2.0 * Dimensionless'Model_Epsilon * Value) - Pad_Scale * R (Value))
          <= Epsilon * Pad_Scale * R (Value) + Eta;
      --  Scaling by two model epsilons is one rounded multiplication.

      procedure Lemma_Scale (Value : Dimensionless) is null;

      procedure Lemma_Padding_Upper (Value : Dimensionless)
      with
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
         pragma
           Assert (abs (R (Value + Pad)) <= abs (R (Value)) + R (Pad) + Epsilon * (abs (R (Value)) + R (Pad)) + Eta);
         pragma
           Assert (abs (R (Value - Pad)) <= abs (R (Value)) + R (Pad) + Epsilon * (abs (R (Value)) + R (Pad)) + Eta);
      end Lemma_Inflate_Growth;

      procedure Lemma_Real_Product (X, Y, X_Limit, Y_Limit : Real)
      with
        Pre  => X >= 0.0 and then X <= X_Limit and then Y >= 0.0 and then Y <= Y_Limit,
        Post => X * Y >= 0.0 and then X * Y <= X_Limit * Y_Limit;

      procedure Lemma_Real_Product (X, Y, X_Limit, Y_Limit : Real) is
      begin
         pragma Assert (X * Y <= X_Limit * Y);
         pragma Assert (X_Limit * Y <= X_Limit * Y_Limit);
      end Lemma_Real_Product;

      procedure Lemma_Distance (Left, Right : Dimensionless)
      with
        Pre  => Left >= 0.0 and then Left <= 1.0 and then Right >= 0.0 and then Right <= 1.0,
        Post =>
          abs (Left - Right) <= 1.0
          and then Down (abs (Left - Right)) >= -2.0
          and then Down (abs (Left - Right)) <= 1.0
          and then R (Down (abs (Left - Right))) <= abs (R (Left) - R (Right));
      --  The padded distance of two nodes is no more than their distance.

      procedure Lemma_Distance (Left, Right : Dimensionless) is
         Distance : constant Dimensionless := abs (Left - Right);
      begin
         Float_Lemmas.Lemma_Rounding_Error_Sub (Left, Right);
         pragma Assert (Distance >= 0.0 and then Distance <= 1.0);
         pragma Assert (Is_Rounding_Of (Distance, abs (R (Left) - R (Right))));
         Lemma_Inflate_Encloses (Distance, abs (R (Left) - R (Right)));
         Lemma_Inflate_Growth (Distance, False);
         pragma Assert (abs (R (Down (Distance))) <= 2.0);
      end Lemma_Distance;

      procedure Lemma_Scaled_Down (Product, Distance : Dimensionless)
      with
        Pre  => Product >= 0.0 and then Product <= 1.0 and then Distance >= -2.0 and then Distance <= 1.0,
        Post =>
          abs (Product * Distance) <= Half_Last
          and then Down (Product * Distance) <= 1.0
          and then R (Down (Product * Distance)) <= R (Product) * R (Distance);
      --  A padded product with a factor of at most one is no more than the exact product, and so than one.

      procedure Lemma_Scaled_Down (Product, Distance : Dimensionless) is
      begin
         Lemma_Product_Is_Bounded (Product, Distance);
         Lemma_Inflate_Encloses (Product * Distance, R (Product) * R (Distance));
         pragma Assert (R (Down (Product * Distance)) <= R (Product) * R (Distance));
         pragma Assert (R (Product) * R (Distance) <= R (Product) * 1.0);
         pragma Assert (R (Product) * 1.0 <= 1.0);
         pragma Assert (R (Down (Product * Distance)) <= 1.0);
      end Lemma_Scaled_Down;

      procedure Lemma_Reciprocal (X, Low : Real)
      with Pre => Low > 0.0 and then X >= Low, Post => 1.0 / X > 0.0 and then (1.0 / X) * Low <= 1.0;

      procedure Lemma_Reciprocal (X, Low : Real) is
      begin
         pragma Assert ((1.0 / X) * X = 1.0);
         pragma Assert (1.0 / X > 0.0);
         pragma Assert ((1.0 / X) * Low <= (1.0 / X) * X);
      end Lemma_Reciprocal;

      procedure Lemma_Up_Is_Inflated (Value : Dimensionless)
      with Pre => abs Value <= Half_Last, Post => R (Up (Value)) = R (Inflate_Finite (Value, True));
      --  Up pads whatever is within Half_Last.

      procedure Lemma_Up_Is_Inflated (Value : Dimensionless) is null;

      procedure Lemma_Reciprocal_Bound (Padded, Exact, Product, Denominator : Real)
      with
        Pre  =>
          Exact > 0.0
          and then Padded >= Exact
          and then Product > 0.0
          and then Exact * Product = 1.0
          and then Product <= Denominator,
        Post => Padded * Denominator >= 1.0;
      --  An upper bound on the reciprocal of a lower bound on a denominator is at least its reciprocal.

      procedure Lemma_Reciprocal_Bound (Padded, Exact, Product, Denominator : Real) is
      begin
         pragma Assert (Padded * Product >= Exact * Product);
         pragma Assert (Padded * Denominator >= Padded * Product);
      end Lemma_Reciprocal_Bound;

      procedure Lemma_Padded_Reciprocal (Product : Dimensionless; Denominator : Real)
      with
        Pre  => Product >= 1.0 / Largest_Weight and then Product <= 1.0 and then R (Product) <= abs Denominator,
        Post => Up (1.0 / Product) >= 0.0 and then R (Up (1.0 / Product)) * abs Denominator >= 1.0;
      --  The padded reciprocal of a lower bound on the magnitude of a denominator is at least the reciprocal of
      --  that magnitude.

      procedure Lemma_Padded_Reciprocal (Product : Dimensionless; Denominator : Real) is
         Exact    : constant Real := 1.0 / R (Product);
         Quotient : constant Dimensionless := 1.0 / Product;
      begin
         Lemma_Reciprocal (R (Product), Weight_Floor);
         pragma Assert (Exact > 0.0 and then Exact <= Weight_Limit);
         pragma Assert (Exact * R (Product) = 1.0);
         Float_Lemmas.Lemma_Rounding_Error_Div (1.0, Product);
         pragma Assert (Is_Rounding_Of (Quotient, Exact));
         pragma Assert (abs (R (Quotient)) <= 2.0 * Weight_Limit);
         pragma Assert (abs Quotient <= Half_Last);
         Lemma_Inflate_Encloses (Quotient, Exact);
         Lemma_Up_Is_Inflated (Quotient);
         Lemma_Reciprocal_Bound (R (Up (Quotient)), Exact, R (Product), abs Denominator);
      end Lemma_Padded_Reciprocal;

      procedure Lemma_Padded_Reach (A, B, Node : Dimensionless)
      with
        Pre  =>
          A >= 0.0 and then A <= 1.0 and then B >= 0.0 and then B <= 1.0 and then Node >= 0.0 and then Node <= 1.0,
        Post =>
          abs (A - Node) <= 1.0
          and then abs (B - Node) <= 1.0
          and then Up (Dimensionless'Max (abs (A - Node), abs (B - Node))) >= 0.0
          and then R (Up (Dimensionless'Max (abs (A - Node), abs (B - Node)))) >= Farthest (R (Node), R (A), R (B));
      --  The padded larger distance from a node to the ends of a cell is at least either distance.

      procedure Lemma_Padded_Reach (A, B, Node : Dimensionless) is
         From_A  : constant Dimensionless := abs (A - Node);
         From_B  : constant Dimensionless := abs (B - Node);
         Longest : constant Dimensionless := Dimensionless'Max (From_A, From_B);
      begin
         Float_Lemmas.Lemma_Rounding_Error_Sub (A, Node);
         Float_Lemmas.Lemma_Rounding_Error_Sub (B, Node);
         pragma Assert (From_A >= 0.0 and then From_A <= 1.0 and then From_B >= 0.0 and then From_B <= 1.0);
         pragma Assert (Is_Rounding_Of (From_A, abs (R (A) - R (Node))));
         pragma Assert (Is_Rounding_Of (From_B, abs (R (B) - R (Node))));
         Lemma_Rounding_Slack (From_A, abs (R (A) - R (Node)));
         Lemma_Rounding_Slack (From_B, abs (R (B) - R (Node)));
         pragma Assert (Longest >= From_A and then Longest >= From_B and then Longest <= 1.0);
         Lemma_Slack_Is_Monotonic (From_A, Longest);
         Lemma_Slack_Is_Monotonic (From_B, Longest);
         Lemma_Inflate_Margin (Longest);
         pragma Assert (R (Up (Longest)) >= R (Longest) + Slack (Longest));
         pragma Assert (R (Up (Longest)) >= abs (R (A) - R (Node)));
         pragma Assert (R (Up (Longest)) >= abs (R (B) - R (Node)));
      end Lemma_Padded_Reach;

      procedure Lemma_Padded_Product (Left, Right : Dimensionless)
      with
        Pre  => Left >= 0.0 and then Left <= Largest_Product and then Right >= 0.0 and then Right <= Largest_Weight,
        Post =>
          abs (Left * Right) <= Half_Last
          and then Up (Left * Right) >= 0.0
          and then Up (Left * Right) <= Padded_Limit
          and then R (Up (Left * Right)) >= R (Left) * R (Right);
      --  A padded product of two bounded nonnegative factors is at least the exact product, and bounded.

      procedure Lemma_Padded_Product (Left, Right : Dimensionless) is
         Exact : constant Real := R (Left) * R (Right);
      begin
         Lemma_Real_Product (R (Left), R (Right), 2.0, Weight_Limit);
         Lemma_Product_Is_Bounded (Left, Right);
         Lemma_Inflate_Encloses (Left * Right, Exact);
         Lemma_Inflate_Growth (Left * Right, True);
         pragma Assert (abs (R (Left * Right)) <= Exact + Epsilon * Exact + Eta);
         pragma Assert (abs (R (Left * Right)) <= 3.0 * Weight_Limit);
         pragma Assert (abs (R (Up (Left * Right))) <= Padded_Bound);
         pragma Assert (R (Up (Left * Right)) >= 0.0);
      end Lemma_Padded_Product;

      procedure Lemma_Padded_Sum (Sum, Term : Dimensionless)
      with
        Pre  => Sum >= 0.0 and then Sum <= Largest_Weight and then Term >= 0.0 and then Term <= Padded_Limit,
        Post =>
          abs (Sum + Term) <= Half_Last
          and then Up (Sum + Term) >= 0.0
          and then R (Up (Sum + Term)) >= R (Sum) + R (Term);
      --  A padded sum of two bounded nonnegative terms is at least the exact sum.

      procedure Lemma_Padded_Sum (Sum, Term : Dimensionless) is
      begin
         Float_Lemmas.Lemma_Rounding_Error_Add (Sum, Term);
         pragma Assert (R (Sum) + R (Term) <= Weight_Limit + Padded_Bound);
         pragma Assert (abs (R (Sum + Term)) <= 2.0 * Padded_Bound);
         pragma Assert (abs (Sum + Term) <= Half_Last);
         Lemma_Inflate_Encloses (Sum + Term, R (Sum) + R (Term));
         pragma Assert (R (Up (Sum + Term)) >= 0.0);
      end Lemma_Padded_Sum;

      ------------------------------------------------------------------------
      --  Steps of the bound, as real numbers
      ------------------------------------------------------------------------

      procedure Lemma_Numerator_Step
        (Nodes : Node_Array; I, J : Sample_Index; Old_Product, Distance, New_Product : Real)
      with
        Pre  =>
          Nodes'First = 1
          and then Nodes'Last = Sample_Count
          and then I /= J
          and then Old_Product > 0.0
          and then Old_Product <= abs (Numerator (Nodes, I, Nodes (I), J - 1))
          and then Distance <= abs (Nodes (I) - Nodes (J))
          and then New_Product <= Old_Product * Distance,
        Post => (if New_Product > 0.0 then New_Product <= abs (Numerator (Nodes, I, Nodes (I), J)));
      --  One more factor of the weight denominator, bounded from below.

      procedure Lemma_Numerator_Step
        (Nodes : Node_Array; I, J : Sample_Index; Old_Product, Distance, New_Product : Real)
      is
         Before : constant Real := Numerator (Nodes, I, Nodes (I), J - 1);
         Gap    : constant Real := Nodes (I) - Nodes (J);
      begin
         pragma Assert (Numerator (Nodes, I, Nodes (I), J) = Before * Gap);
         pragma Assert (abs (Before * Gap) = abs Before * abs Gap);
         if New_Product > 0.0 then
            pragma Assert (if Distance <= 0.0 then Old_Product * Distance <= 0.0);
            pragma Assert (Distance > 0.0);
            pragma Assert (Old_Product * Distance <= abs Before * Distance);
            pragma Assert (abs Before * Distance <= abs Before * abs Gap);
         end if;
      end Lemma_Numerator_Step;

      procedure Lemma_Upper_Product_Step
        (Nodes : Node_Array; I, J : Sample_Index; A, B, Old_Product, Reach, New_Product : Real)
      with
        Pre  =>
          Nodes'First = 1
          and then Nodes'Last = Sample_Count
          and then I /= J
          and then Upper_Product (Nodes, I, A, B, J - 1) >= 0.0
          and then Old_Product >= Upper_Product (Nodes, I, A, B, J - 1)
          and then Reach >= Farthest (Nodes (J), A, B)
          and then New_Product >= Old_Product * Reach,
        Post => Upper_Product (Nodes, I, A, B, J) >= 0.0 and then New_Product >= Upper_Product (Nodes, I, A, B, J);
      --  One more factor of the bound on a numerator, bounded from above.

      procedure Lemma_Upper_Product_Step
        (Nodes : Node_Array; I, J : Sample_Index; A, B, Old_Product, Reach, New_Product : Real)
      is
         Before : constant Real := Upper_Product (Nodes, I, A, B, J - 1);
         Far    : constant Real := Farthest (Nodes (J), A, B);
      begin
         pragma Assert (Far >= 0.0);
         pragma Assert (Upper_Product (Nodes, I, A, B, J) = Before * Far);
         pragma Assert (Old_Product * Reach >= Before * Reach);
         pragma Assert (Before * Reach >= Before * Far);
      end Lemma_Upper_Product_Step;

      procedure Lemma_Upper_Sum_Step
        (Nodes : Node_Array; Weights : Weight_Array; I : Sample_Index; A, B, Old_Sum, Product, New_Sum : Real)
      with
        Pre  =>
          Nodes'First = 1
          and then Nodes'Last = Sample_Count
          and then Old_Sum >= Upper_Sum (Nodes, Weights, A, B, I - 1)
          and then Weights (I) >= 0.0
          and then Product >= Upper_Product (Nodes, I, A, B, Sample_Count)
          and then New_Sum >= Old_Sum + Product * R (Weights (I)),
        Post => New_Sum >= Upper_Sum (Nodes, Weights, A, B, I);
      --  One more term of the bound.

      procedure Lemma_Upper_Sum_Step
        (Nodes : Node_Array; Weights : Weight_Array; I : Sample_Index; A, B, Old_Sum, Product, New_Sum : Real)
      is
         Term : constant Real := Upper_Product (Nodes, I, A, B, Sample_Count);
      begin
         pragma
           Assert
             (Upper_Sum (Nodes, Weights, A, B, I) = Upper_Sum (Nodes, Weights, A, B, I - 1) + R (Weights (I)) * Term);
         pragma Assert (R (Weights (I)) >= 0.0);
         pragma Assert (Product * R (Weights (I)) >= Term * R (Weights (I)));
      end Lemma_Upper_Sum_Step;

      ------------------------------------------------------------------------
      --  The bound is one on the Lebesgue function
      ------------------------------------------------------------------------

      procedure Lemma_Farthest (Node, A, B, X : Real)
      with
        Pre  => A <= X and then X <= B,
        Post => Farthest (Node, A, B) >= 0.0 and then abs (X - Node) <= Farthest (Node, A, B);
      --  A point of a cell is no farther from a node than the farther end of the cell.

      procedure Lemma_Farthest (Node, A, B, X : Real) is null;

      procedure Lemma_Bounded_Product (N, D, U, F : Real)
      with Pre => abs N <= U and then abs D <= F, Post => abs (N * D) <= U * F;

      procedure Lemma_Bounded_Product (N, D, U, F : Real) is
      begin
         pragma Assert (abs (N * D) = abs N * abs D);
         pragma Assert (abs N * abs D <= U * abs D);
         pragma Assert (U * abs D <= U * F);
      end Lemma_Bounded_Product;

      procedure Lemma_Numerator_Is_Bounded (Nodes : Node_Array; I : Positive; A, B, X : Real; Count : Natural)
      with
        Always_Terminates,
        Subprogram_Variant => (Decreases => Count),
        Pre                => Nodes'First = 1 and then Count <= Nodes'Last and then A <= X and then X <= B,
        Post               => abs (Numerator (Nodes, I, X, Count)) <= Upper_Product (Nodes, I, A, B, Count);
      --  The numerator of a basis polynomial is bounded over a cell by the product of the farthest distances.

      procedure Lemma_Numerator_Is_Bounded (Nodes : Node_Array; I : Positive; A, B, X : Real; Count : Natural) is
      begin
         if Count = 0 then
            return;
         end if;
         Lemma_Numerator_Is_Bounded (Nodes, I, A, B, X, Count - 1);
         if Count /= I then
            Lemma_Farthest (Nodes (Count), A, B, X);
            Lemma_Bounded_Product
              (Numerator (Nodes, I, X, Count - 1),
               X - Nodes (Count),
               Upper_Product (Nodes, I, A, B, Count - 1),
               Farthest (Nodes (Count), A, B));
         end if;
      end Lemma_Numerator_Is_Bounded;

      procedure Lemma_Bounded_Quotient (N, D, W, U : Real)
      with
        Pre  => W >= 0.0 and then W * abs D >= 1.0 and then abs N <= U,
        Post => D /= 0.0 and then abs (N / D) <= W * U;
      --  Dividing by D is no more than multiplying by a W which is at least the reciprocal of its magnitude.

      procedure Lemma_Bounded_Quotient (N, D, W, U : Real) is
         Q : constant Real := (if D = 0.0 then 0.0 else abs (N / D));
      begin
         pragma Assert (D /= 0.0);
         pragma Assert ((N / D) * D = N);
         pragma Assert (Q * abs D = abs N);
         pragma Assert (U >= 0.0);
         pragma Assert (U * 1.0 <= U * (W * abs D));
         pragma Assert (U * (W * abs D) = (W * U) * abs D);
         pragma Assert (Q * abs D <= (W * U) * abs D);
         pragma Assert (if Q > W * U then Q * abs D > (W * U) * abs D);
      end Lemma_Bounded_Quotient;

      procedure Lemma_Basis_Is_Bounded (Nodes : Node_Array; Weights : Weight_Array; I : Sample_Index; A, B, X : Real)
      with
        Pre  =>
          Nodes'First = 1
          and then Nodes'Last = Sample_Count
          and then A <= X
          and then X <= B
          and then Is_Sound_For (Nodes, I, Weights (I)),
        Post => abs (Basis (Nodes, I, X)) <= R (Weights (I)) * Upper_Product (Nodes, I, A, B, Sample_Count);
      --  One term of the Lebesgue function is at most the matching term of the bound.

      procedure Lemma_Basis_Is_Bounded (Nodes : Node_Array; Weights : Weight_Array; I : Sample_Index; A, B, X : Real)
      is
      begin
         Lemma_Numerator_Is_Bounded (Nodes, I, A, B, X, Sample_Count);
         Lemma_Bounded_Quotient
           (Numerator (Nodes, I, X, Sample_Count),
            Weight_Denominator (Nodes, I),
            R (Weights (I)),
            Upper_Product (Nodes, I, A, B, Sample_Count));
         pragma Assert (Basis (Nodes, I, X) = Numerator (Nodes, I, X, Sample_Count) / Weight_Denominator (Nodes, I));
      end Lemma_Basis_Is_Bounded;

      procedure Lemma_Upper_Sum_Is_Above (Nodes : Node_Array; Weights : Weight_Array; A, B, X : Real; Count : Natural)
      with
        Always_Terminates,
        Subprogram_Variant => (Decreases => Count),
        Pre                =>
          Nodes'First = 1
          and then Nodes'Last = Sample_Count
          and then Count <= Sample_Count
          and then A <= X
          and then X <= B
          and then (for all I in Sample_Index => Is_Sound_For (Nodes, I, Weights (I))),
        Post               => Lebesgue_Partial (Nodes, X, Count) <= Upper_Sum (Nodes, Weights, A, B, Count);
      --  The Lebesgue function is at most the bound, term by term.

      procedure Lemma_Upper_Sum_Is_Above (Nodes : Node_Array; Weights : Weight_Array; A, B, X : Real; Count : Natural)
      is
      begin
         if Count > 0 then
            Lemma_Upper_Sum_Is_Above (Nodes, Weights, A, B, X, Count - 1);
            Lemma_Basis_Is_Bounded (Nodes, Weights, Count, A, B, X);
         end if;
      end Lemma_Upper_Sum_Is_Above;

      ------------------------------------------------------------------------
      --  The steps of the check
      ------------------------------------------------------------------------

      procedure Lemma_Node_Is_In_Unit (Samples : Sample_Array; J : Sample_Index)
      with
        Pre  => Nodes_Are_In_Unit (Samples),
        Post =>
          Samples (J).Node >= 0.0
          and then Samples (J).Node <= 1.0
          and then Node_Reals (Samples) (J) = R (Samples (J).Node);
      --  One node, and the real number which stands for it.

      procedure Lemma_Node_Is_In_Unit (Samples : Sample_Array; J : Sample_Index) is null;

      procedure Lemma_Nodes_Are_In_Unit (Samples : Sample_Array) is null;

      procedure Lemma_Denominator_Start (Samples : Sample_Array; I : Sample_Index) is
      begin
         pragma Assert (Numerator (Node_Reals (Samples), I, R (Samples (I).Node), 0) = 1.0);
      end Lemma_Denominator_Start;

      procedure Lemma_Denominator_Step (Samples : Sample_Array; I, J : Sample_Index; Product : Dimensionless) is
         Nodes : constant Node_Array := Node_Reals (Samples);
      begin
         Lemma_Node_Is_In_Unit (Samples, I);
         Lemma_Node_Is_In_Unit (Samples, J);
         Lemma_Distance (Samples (I).Node, Samples (J).Node);
         Lemma_Scaled_Down (Product, Down (abs (Samples (I).Node - Samples (J).Node)));
         Lemma_Numerator_Step
           (Nodes,
            I,
            J,
            R (Product),
            R (Down (abs (Samples (I).Node - Samples (J).Node))),
            R (Down (Product * Down (abs (Samples (I).Node - Samples (J).Node)))));
      end Lemma_Denominator_Step;

      procedure Lemma_Denominator_Skip (Samples : Sample_Array; I : Sample_Index; Product : Dimensionless) is
         Nodes : constant Node_Array := Node_Reals (Samples);
      begin
         pragma
           Assert (Numerator (Nodes, I, R (Samples (I).Node), I) = Numerator (Nodes, I, R (Samples (I).Node), I - 1));
      end Lemma_Denominator_Skip;

      procedure Lemma_Weight (Samples : Sample_Array; I : Sample_Index; Product : Dimensionless) is
         Nodes : constant Node_Array := Node_Reals (Samples);
      begin
         pragma Assert (Nodes (I) = R (Samples (I).Node));
         pragma Assert (Weight_Denominator (Nodes, I) = Numerator (Nodes, I, R (Samples (I).Node), Sample_Count));
         Lemma_Padded_Reciprocal (Product, Weight_Denominator (Nodes, I));
      end Lemma_Weight;

      procedure Lemma_Weights_Are_Sound (Samples : Sample_Array; Weights : Weight_Array) is null;

      procedure Lemma_Reach (Samples : Sample_Array; J : Sample_Index; A, B : Dimensionless) is
      begin
         Lemma_Node_Is_In_Unit (Samples, J);
         Lemma_Padded_Reach (A, B, Samples (J).Node);
      end Lemma_Reach;

      procedure Lemma_Product_Start (Samples : Sample_Array; I : Sample_Index; A, B : Dimensionless) is
      begin
         pragma Assert (Upper_Product (Node_Reals (Samples), I, R (A), R (B), 0) = 1.0);
      end Lemma_Product_Start;

      procedure Lemma_Product_Step (Samples : Sample_Array; I, J : Sample_Index; A, B, Product, Reach : Dimensionless)
      is
         Nodes : constant Node_Array := Node_Reals (Samples);
      begin
         pragma Assert (Nodes (J) = R (Samples (J).Node));
         Lemma_Padded_Product (Product, Reach);
         Lemma_Upper_Product_Step (Nodes, I, J, R (A), R (B), R (Product), R (Reach), R (Up (Product * Reach)));
      end Lemma_Product_Step;

      procedure Lemma_Product_Skip (Samples : Sample_Array; I : Sample_Index; A, B, Product : Dimensionless) is
         Nodes : constant Node_Array := Node_Reals (Samples);
      begin
         pragma Assert (Upper_Product (Nodes, I, R (A), R (B), I) = Upper_Product (Nodes, I, R (A), R (B), I - 1));
      end Lemma_Product_Skip;

      procedure Lemma_Sum_Start (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless) is
      begin
         pragma Assert (Upper_Sum (Node_Reals (Samples), Weights, R (A), R (B), 0) = 0.0);
      end Lemma_Sum_Start;

      procedure Lemma_Sum_Step
        (Samples : Sample_Array; Weights : Weight_Array; I : Sample_Index; A, B, Sum, Product : Dimensionless)
      is
         Nodes : constant Node_Array := Node_Reals (Samples);
      begin
         Lemma_Padded_Product (Product, Weights (I));
         Lemma_Padded_Sum (Sum, Up (Product * Weights (I)));
         Lemma_Upper_Sum_Step
           (Nodes, Weights, I, R (A), R (B), R (Sum), R (Product), R (Up (Sum + Up (Product * Weights (I)))));
      end Lemma_Sum_Step;

      procedure Lemma_Cell_Bound (Samples : Sample_Array; Weights : Weight_Array; A, B, Sum : Dimensionless) is null;

      procedure Lemma_Cell_Bound_Is_Sound
        (Samples : Sample_Array; Weights : Weight_Array; A, B, Bound : Dimensionless; X : Real)
      is
         Nodes : constant Node_Array := Node_Reals (Samples);
      begin
         Lemma_Upper_Sum_Is_Above (Nodes, Weights, R (A), R (B), X, Sample_Count);
         pragma Assert (Lebesgue_Sum (Nodes, X) = Lebesgue_Partial (Nodes, X, Sample_Count));
      end Lemma_Cell_Bound_Is_Sound;

      procedure Lemma_Nodes_Increase (Samples : Sample_Array) is
         Nodes : constant Node_Array := Node_Reals (Samples);
      begin
         for I in 1 .. Sample_Count - 1 loop
            pragma Assert (Nodes (I) = R (Samples (I).Node));
            pragma Assert (Nodes (I + 1) = R (Samples (I + 1).Node));
            pragma Assert (Samples (I).Node < Samples (I + 1).Node);
            pragma Assert (Nodes (I) < Nodes (I + 1));
            pragma Loop_Invariant (for all Earlier in 1 .. I => Nodes (Earlier) < Nodes (Earlier + 1));
         end loop;
         pragma Assert (Is_Increasing (Nodes));
      end Lemma_Nodes_Increase;

   end Bounds;

   use Bounds;

   ---------------------------------------------------------------------------
   --  The weights
   ---------------------------------------------------------------------------

   function Build_Weights (Samples : Sample_Array) return Weight_Table
   with
     Pre  => Nodes_Are_In_Unit (Samples),
     Post => (if Build_Weights'Result.Valid then Weights_Are_Sound (Samples, Build_Weights'Result.Weights));
   --  Upper bounds wᵢ, or an invalid table if the nodes do not allow them.

   function Build_Weights (Samples : Sample_Array) return Weight_Table is
      Weights : Weight_Array := [others => Largest_Weight];
   begin
      for I in Sample_Index loop
         declare
            Product : Dimensionless := 1.0;
            --  A lower bound on Πⱼ≠ᵢ |xᵢ - xⱼ| over the nodes visited so far.
         begin
            Lemma_Denominator_Start (Samples, I);
            for J in Sample_Index loop
               if J /= I then
                  Lemma_Denominator_Step (Samples, I, J, Product);
                  Product := Down (Product * Down (abs (Samples (I).Node - Samples (J).Node)));
                  if not (Product >= 1.0 / Largest_Weight) then
                     return (Weights => Weights, Valid => False);
                  end if;
               else
                  Lemma_Denominator_Skip (Samples, I, Product);
               end if;
               pragma Loop_Invariant (Is_Denominator_Bound (Samples, I, J, Product));
            end loop;
            Lemma_Weight (Samples, I, Product);
            Weights (I) := Up (1.0 / Product);
            if not (Weights (I) <= Largest_Weight) then
               return (Weights => Weights, Valid => False);
            end if;
         end;
         pragma Loop_Invariant (for all Earlier in 1 .. I => Weight_Is_Sound (Samples, Earlier, Weights (Earlier)));
      end loop;
      Lemma_Weights_Are_Sound (Samples, Weights);
      return (Weights => Weights, Valid => True);
   end Build_Weights;

   ---------------------------------------------------------------------------
   --  The bound over one cell
   ---------------------------------------------------------------------------

   function Cell_Bound (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless) return Dimensionless
   with
     Pre  => Nodes_Are_In_Unit (Samples) and then A >= 0.0 and then A <= B and then B <= 1.0,
     Post => (if Cell_Bound'Result <= Largest_Weight then Is_Cell_Bound (Samples, Weights, A, B, Cell_Bound'Result));
   --  An upper bound on the Lebesgue function over [A, B], or Dimensionless'Last.

   function Cell_Bound (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless) return Dimensionless is
      Reach : Weight_Array := [others => 1.0];
      --  Upper bounds on |X - xⱼ| over the cell.

      Sum : Dimensionless := 0.0;
   begin
      for J in Sample_Index loop
         Lemma_Reach (Samples, J, A, B);
         Reach (J) := Up (Dimensionless'Max (abs (A - Samples (J).Node), abs (B - Samples (J).Node)));
         if not (Reach (J) <= Largest_Product) then
            return Dimensionless'Last;
         end if;
         pragma Loop_Invariant (for all Earlier in 1 .. J => Reach_Is_Sound (Samples, Earlier, A, B, Reach (Earlier)));
      end loop;
      Lemma_Sum_Start (Samples, Weights, A, B);
      for I in Sample_Index loop
         declare
            Product : Dimensionless := 1.0;
         begin
            Lemma_Product_Start (Samples, I, A, B);
            for J in Sample_Index loop
               if J /= I then
                  Lemma_Product_Step (Samples, I, J, A, B, Product, Reach (J));
                  Product := Up (Product * Reach (J));
                  if not (Product <= Largest_Product) then
                     return Dimensionless'Last;
                  end if;
               else
                  Lemma_Product_Skip (Samples, I, A, B, Product);
               end if;
               pragma Loop_Invariant (Is_Product_Bound (Samples, I, A, B, J, Product));
            end loop;
            if not (Weights (I) <= Largest_Weight) or else not (Weights (I) >= 0.0) then
               return Dimensionless'Last;
            end if;
            Lemma_Sum_Step (Samples, Weights, I, A, B, Sum, Product);
            Sum := Up (Sum + Up (Product * Weights (I)));
            if not (Sum <= Largest_Weight) then
               return Dimensionless'Last;
            end if;
         end;
         pragma Loop_Invariant (Is_Sum_Bound (Samples, Weights, A, B, I, Sum));
      end loop;
      Lemma_Cell_Bound (Samples, Weights, A, B, Sum);
      return Sum;
   end Cell_Bound;

   ---------------------------------------------------------------------------
   --  The decision
   ---------------------------------------------------------------------------

   function Certified
     (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless; Depth : Natural) return Boolean
   is (Cell_Bound (Samples, Weights, A, B) <= Lebesgue_Bound
       or else
         (Depth < Maximum_Depth
          and then 0.5 * (A + B) >= A
          and then 0.5 * (A + B) <= B
          and then Certified (Samples, Weights, A, 0.5 * (A + B), Depth + 1)
          and then Certified (Samples, Weights, 0.5 * (A + B), B, Depth + 1)))
   with
     Ghost,
     Pre                =>
       Nodes_Are_In_Unit (Samples) and then A >= 0.0 and then A <= B and then B <= 1.0 and then Depth <= Maximum_Depth,
     Subprogram_Variant => (Increases => Depth);
   --  What a True result of Decide rests on: the bound over the cell is small enough, or else both halves are
   --  certified in turn.

   function Decide
     (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless; Depth : Natural) return Boolean
   with
     Pre                =>
       Nodes_Are_In_Unit (Samples) and then A >= 0.0 and then A <= B and then B <= 1.0 and then Depth <= Maximum_Depth,
     Post               => (if Decide'Result then Certified (Samples, Weights, A, B, Depth)),
     Subprogram_Variant => (Increases => Depth);
   --  Whether the Lebesgue function is at most Lebesgue_Bound over [A, B].

   function Decide
     (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless; Depth : Natural) return Boolean is
   begin
      if Cell_Bound (Samples, Weights, A, B) <= Lebesgue_Bound then
         return True;
      elsif Depth = Maximum_Depth then
         return False;
      end if;
      declare
         Middle : constant Dimensionless := 0.5 * (A + B);
      begin
         if not (Middle >= A and then Middle <= B) then
            return False;
         end if;
         return
           Decide (Samples, Weights, A, Middle, Depth + 1) and then Decide (Samples, Weights, Middle, B, Depth + 1);
      end;
   end Decide;

   function Certify (Samples : Sample_Array) return Boolean
   with
     Refined_Post =>
       (if Certify'Result
        then
          (for all Index in Sample_Index => Is_In_Range (Samples (Index)) and then Has_Margins (Samples (Index)))
          and then (for all Index in 1 .. Sample_Count - 1 => Samples (Index).Node < Samples (Index + 1).Node)
          and then Nodes_Are_In_Unit (Samples)
          and then Build_Weights (Samples).Valid
          and then Certified (Samples, Build_Weights (Samples).Weights, 0.0, 1.0, 0))
   is
      Table : Weight_Table;
   begin
      for Index in Sample_Index loop
         if not (Is_In_Range (Samples (Index)) and then Has_Margins (Samples (Index))) then
            return False;
         end if;
         pragma
           Loop_Invariant
             (for all Earlier in 1 .. Index =>
                Is_In_Range (Samples (Earlier)) and then Has_Margins (Samples (Earlier)));
      end loop;
      for Index in 1 .. Sample_Count - 1 loop
         if not (Samples (Index).Node < Samples (Index + 1).Node) then
            return False;
         end if;
         pragma Loop_Invariant (for all Earlier in 1 .. Index => Samples (Earlier).Node < Samples (Earlier + 1).Node);
      end loop;
      Lemma_Nodes_Are_In_Unit (Samples);
      Table := Build_Weights (Samples);
      return Table.Valid and then Decide (Samples, Table.Weights, 0.0, 1.0, 0);
   end Certify;

   function Node_Reals (Samples : Sample_Array) return Polynomials.Interpolation.Node_Array is
      Result : Polynomials.Interpolation.Node_Array (1 .. Sample_Count) := [others => 0.0];
   begin
      for Index in Sample_Index loop
         Result (Index) := R (Samples (Index).Node);
         pragma Loop_Invariant (for all Earlier in 1 .. Index => Result (Earlier) = R (Samples (Earlier).Node));
      end loop;
      return Result;
   end Node_Reals;

   ---------------------------------------------------------------------------
   --  Soundness of the decision
   ---------------------------------------------------------------------------

   procedure Lemma_Certified_Is_Sound
     (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless; Depth : Natural; X : Real)
   with
     Ghost,
     Always_Terminates,
     Subprogram_Variant => (Increases => Depth),
     Pre                =>
       Nodes_Are_In_Unit (Samples)
       and then Weights_Are_Sound (Samples, Weights)
       and then A >= 0.0
       and then A <= B
       and then B <= 1.0
       and then Depth <= Maximum_Depth
       and then Certified (Samples, Weights, A, B, Depth)
       and then R (A) <= X
       and then X <= R (B),
     Post               => Lebesgue_Sum (Node_Reals (Samples), X) <= Lebesgue_Bound_Real;
   --  A certified cell has no point where the Lebesgue function exceeds the bound: the point is in a cell whose
   --  own bound is small enough, found by following the halves which contain it.

   procedure Lemma_Certified_Is_Sound
     (Samples : Sample_Array; Weights : Weight_Array; A, B : Dimensionless; Depth : Natural; X : Real) is
   begin
      if Cell_Bound (Samples, Weights, A, B) <= Lebesgue_Bound then
         Lemma_Cell_Bound_Is_Sound (Samples, Weights, A, B, Cell_Bound (Samples, Weights, A, B), X);
         pragma Assert (R (Cell_Bound (Samples, Weights, A, B)) <= Lebesgue_Bound_Real);
      else
         declare
            Middle : constant Dimensionless := 0.5 * (A + B);
         begin
            if X <= R (Middle) then
               Lemma_Certified_Is_Sound (Samples, Weights, A, Middle, Depth + 1, X);
            else
               Lemma_Certified_Is_Sound (Samples, Weights, Middle, B, Depth + 1, X);
            end if;
         end;
      end if;
   end Lemma_Certified_Is_Sound;

   procedure Lemma_Complement_Is_Relative (Value : Sample) is
      Margin : constant := 2.0**(-20);
      --  Node_Margin, as a real number.

      Eta_Share : constant := 2.0**(-1055);
      --  Eta over Margin.
   begin
      pragma Assert (R (Value.Node) <= 1.0 - Margin);
      pragma Assert (Eta <= Eta_Share * (1.0 - R (Value.Node)));
   end Lemma_Complement_Is_Relative;

   procedure Lemma_Lebesgue (Samples : Sample_Array; X : Real) is
      Table : constant Weight_Table := Build_Weights (Samples);
   begin
      Lemma_Certified_Is_Sound (Samples, Table.Weights, 0.0, 1.0, 0, X);
      Lemma_Nodes_Increase (Samples);
   end Lemma_Lebesgue;

end Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes;
