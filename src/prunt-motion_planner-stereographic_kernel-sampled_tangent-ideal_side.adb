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

with Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;
with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed;

package body Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Ideal_Side
  with SPARK_Mode => On
is

   --  The proofs below are a rounding-error analysis. With u = Epsilon and η = Eta, every rounded operation
   --  errs by at most u times the magnitude of its exact result plus η. A sum of terms which is accumulated in
   --  floating point is tracked against the exact sum by a relative factor, which grows by Step with every
   --  addition, and an absolute dust of underflow.

   Step : constant := 2.0**(-53) * (1.0 + 2.0**(-7));
   --  Slightly more than Epsilon: what one more rounded addition adds to the relative error of a running sum.

   Two_Steps   : constant := 2.0 * Step;
   Four_Steps  : constant := 4.0 * Step;
   Five_Steps  : constant := 5.0 * Step;
   Seven_Steps : constant := 7.0 * Step;

   Factor_Limit : constant := 2.0**(-7);
   --  No relative factor comes anywhere near this.

   Three_Eta : constant := 3.0 * 2.0**(-1075);
   Five_Eta  : constant := 5.0 * 2.0**(-1075);
   Seven_Eta : constant := 7.0 * 2.0**(-1075);

   Dust_Limit : constant := 100.0 * 2.0**(-1075);
   --  No dust of At_Node's sums comes near this.

   ---------------------------------------------------------------------------
   --  Real arithmetic
   ---------------------------------------------------------------------------

   procedure Lemma_Scale (A, B, C : Real)
   with Ghost, Pre => A <= B and then C >= 0.0, Post => A * C <= B * C and then C * A <= C * B;

   procedure Lemma_Scale (A, B, C : Real) is null;

   procedure Lemma_Sign (A, B : Real)
   with Ghost, Pre => A >= 0.0 and then B >= 0.0, Post => A * B >= 0.0;

   procedure Lemma_Sign (A, B : Real) is null;

   procedure Lemma_Distribute (A, B, C : Real)
   with Ghost, Post => A * (B + C) = A * B + A * C and then (B + C) * A = B * A + C * A;

   procedure Lemma_Distribute (A, B, C : Real) is null;

   procedure Lemma_Product_Bound (X, Y, A, B : Real)
   with Ghost, Pre => abs X <= A and then abs Y <= B, Post => abs (X * Y) <= A * B;
   --  |X| ≤ A and |Y| ≤ B give |X·Y| ≤ A·B.

   procedure Lemma_Product_Bound (X, Y, A, B : Real) is
   begin
      pragma Assert (abs (X * Y) = abs X * abs Y);
      Lemma_Scale (abs X, A, abs Y);
      Lemma_Scale (abs Y, B, A);
   end Lemma_Product_Bound;

   procedure Lemma_Accumulate (S, P, T, Exact, Size, Term, Factor, Next_Factor, Dust, Term_Dust, Unit : Real)
   with
     Ghost,
     Pre  =>
       Size >= 0.0
       and then abs Exact <= Size
       and then Factor >= 0.0
       and then Factor <= Factor_Limit
       and then Next_Factor >= Factor + Step
       and then Dust >= 0.0
       and then Term_Dust >= 0.0
       and then Epsilon * (Dust + Term_Dust) + Eta <= Unit
       and then abs (S - Exact) <= Factor * Size + Dust
       and then abs (P - Term) <= Factor * abs Term + Term_Dust
       and then abs (T - (S + P)) <= Epsilon * abs (S + P) + Eta,
     Post => abs (T - (Exact + Term)) <= Next_Factor * (Size + abs Term) + Dust + Term_Dust + Unit;
   --  One rounded addition to a running sum. S approximates Exact, whose terms have magnitudes summing to Size,
   --  within Factor of Size and Dust; P approximates the next term likewise; T is their rounded sum. Then T
   --  approximates the longer exact sum within Factor + Step of its size, the two dusts, and Unit, which covers
   --  the underflow of the addition and the rounding of the dusts.

   procedure Lemma_Accumulate (S, P, T, Exact, Size, Term, Factor, Next_Factor, Dust, Term_Dust, Unit : Real) is
      Scaled_Size : constant Real := Factor * Size;
      Scaled_Term : constant Real := Factor * abs Term;
      Total       : constant Real := Size + abs Term;
   begin
      Lemma_Sign (Factor, Size);
      Lemma_Sign (Factor, abs Term);
      Lemma_Scale (Factor, Factor_Limit, Size);
      Lemma_Scale (Factor, Factor_Limit, abs Term);
      pragma Assert (Scaled_Size >= 0.0 and then Scaled_Size <= Factor_Limit * Size);
      pragma Assert (Scaled_Term >= 0.0 and then Scaled_Term <= Factor_Limit * abs Term);
      pragma Assert (abs S <= Size + Scaled_Size + Dust);
      pragma Assert (abs P <= abs Term + Scaled_Term + Term_Dust);
      pragma Assert (abs (S + P) <= Total + Scaled_Size + Scaled_Term + Dust + Term_Dust);
      pragma Assert (abs (T - (Exact + Term)) <= abs (T - (S + P)) + abs (S - Exact) + abs (P - Term));
      pragma
        Assert
          (abs (T - (Exact + Term))
           <= Epsilon
              * (Total + Scaled_Size + Scaled_Term + Dust + Term_Dust)
              + Eta
              + Scaled_Size
              + Dust
              + Scaled_Term
              + Term_Dust);
      pragma Assert (Epsilon * (Total + Scaled_Size + Scaled_Term) <= Step * Total);
      pragma Assert (abs (T - (Exact + Term)) <= Scaled_Size + Scaled_Term + Step * Total + Dust + Term_Dust + Unit);
      Lemma_Distribute (Factor, Size, abs Term);
      pragma Assert ((Factor + Step) * Total = Scaled_Size + Scaled_Term + Step * Total);
      Lemma_Scale (Factor + Step, Next_Factor, Total);
   end Lemma_Accumulate;

   function Tally (Count : Natural) return Real
   is (case Count is
         when 0      => Real'(0.0),
         when 1      => Real'(1.0),
         when 2      => Real'(2.0),
         when 3      => Real'(3.0),
         when 4      => Real'(4.0),
         when 5      => Real'(5.0),
         when 6      => Real'(6.0),
         when 7      => Real'(7.0),
         when 8      => Real'(8.0),
         when others => Real'(9.0))
   with Ghost, Pre => Count <= 9;
   --  Count as a real number.

   procedure Lemma_Tally_Step (Count : Natural)
   with
     Ghost,
     Pre  => Count < 9,
     Post => Tally (Count + 1) = Tally (Count) + 1.0 and then Tally (Count) >= 0.0 and then Tally (Count) <= 8.0;

   procedure Lemma_Tally_Step (Count : Natural) is null;

   procedure Lemma_Small_Tally (Count : Natural)
   with
     Ghost,
     Pre  => Count <= 2,
     Post => Tally (Count + 1) = Tally (Count) + 1.0 and then Tally (Count) >= 0.0 and then Tally (Count) <= 2.0;
   --  The same for the three components of a chart.

   procedure Lemma_Small_Tally (Count : Natural) is null;

   ---------------------------------------------------------------------------
   --  Floating-point facts which need no rounding analysis
   ---------------------------------------------------------------------------

   Sign_Limit : constant Dimensionless := 2.0**430
   with Ghost;
   --  Operands within this magnitude have products which do not overflow.

   procedure Lemma_Square_Sign (Value : Dimensionless)
   with Ghost, Pre => abs Value <= Sign_Limit, Post => Value * Value >= 0.0;

   procedure Lemma_Square_Sign (Value : Dimensionless) is null;

   procedure Lemma_Product_Sign (Left, Right : Dimensionless)
   with
     Ghost,
     Pre  => Left >= 0.0 and then Left <= Sign_Limit and then Right >= 0.0 and then Right <= Sign_Limit,
     Post => Left * Right >= 0.0;

   procedure Lemma_Product_Sign (Left, Right : Dimensionless) is null;

   Sum_Sign_Limit : constant Dimensionless := 2.0**600
   with Ghost;
   --  The same for sums alone.

   procedure Lemma_Sum_Sign (Left, Right : Dimensionless)
   with
     Ghost,
     Pre  => Left >= 0.0 and then Left <= Sum_Sign_Limit and then Right >= 0.0 and then Right <= Sum_Sign_Limit,
     Post => Left + Right >= 0.0;

   procedure Lemma_Sum_Sign (Left, Right : Dimensionless) is null;

   procedure Lemma_One_More (Value : Dimensionless)
   with Ghost, Pre => Value >= 0.0 and then Value <= Sign_Limit, Post => 1.0 + Value >= 1.0;

   procedure Lemma_One_More (Value : Dimensionless) is null;

   procedure Lemma_One_Less (Value : Dimensionless)
   with
     Ghost,
     Pre  => Value >= 0.0 and then Value <= Sign_Limit and then 1.0 + Value <= Largest_Ideal_Denominator,
     Post => abs (1.0 - Value) <= Largest_Ideal_Denominator;
   --  Rounding is monotonic and symmetric, and -(1 + V) ≤ 1 - V ≤ 1 + V for a nonnegative V.

   procedure Lemma_One_Less (Value : Dimensionless) is null;

   procedure Lemma_Abs_Is_Exact (Value : Dimensionless)
   with Ghost, Post => R (abs Value) = abs R (Value);

   procedure Lemma_Abs_Is_Exact (Value : Dimensionless) is null;

   ---------------------------------------------------------------------------
   --  The chart sum
   ---------------------------------------------------------------------------

   Coefficient_Limit : constant := 2.0**200;
   --  More than Magnitude_Limit.

   Basis_Limit : constant := 70.0;

   Product_Limit_Real : constant := 2.0**207;
   Product_Limit      : constant Dimensionless := 2.0**207
   with Ghost;
   --  More than the product of a coefficient and a basis value, as it rounds.

   Step_Limit : constant := 2.0**208;
   --  A running chart sum grows by at most this in magnitude with every term.

   Chart_Limit_Real : constant := 2.0**212;
   Chart_Limit      : constant Dimensionless := 2.0**212
   with Ghost;
   --  More than nine times Step_Limit: no chart coordinate is larger, whatever the basis values.

   procedure Lemma_Within_Chart_Limit (Value : Dimensionless)
   with Ghost, Pre => abs R (Value) <= Chart_Limit_Real, Post => abs Value <= Chart_Limit;

   procedure Lemma_Within_Chart_Limit (Value : Dimensionless) is null;

   procedure Lemma_Magnitude_Step (Sum, B, Beta : Dimensionless; Count : Real)
   with
     Ghost,
     Pre  =>
       abs Sum <= Chart_Limit
       and then abs B <= Magnitude_Limit
       and then Beta >= 0.0
       and then Beta <= Sample_Nodes.Largest_Basis_Value
       and then Count >= 0.0
       and then Count <= 8.0
       and then abs R (Sum) <= Count * Step_Limit,
     Post =>
       abs (B * Beta) <= Product_Limit
       and then abs (Sum + B * Beta) <= Chart_Limit
       and then abs R (Sum + B * Beta) <= (Count + 1.0) * Step_Limit
       and then abs (R (B * Beta) - R (B) * R (Beta)) <= Epsilon * abs (R (B) * R (Beta)) + Eta
       and then abs (R (Sum + B * Beta) - (R (Sum) + R (B * Beta))) <= Epsilon * abs (R (Sum) + R (B * Beta)) + Eta;
   --  One term of the chart sum: neither the product nor the sum overflows, and the sum stays within Step_Limit
   --  times the number of terms.

   procedure Lemma_Magnitude_Step (Sum, B, Beta : Dimensionless; Count : Real) is
   begin
      pragma Assert (abs R (B) <= Coefficient_Limit);
      pragma Assert (abs R (Beta) <= Basis_Limit);
      Lemma_Product_Bound (R (B), R (Beta), Coefficient_Limit, Basis_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Mul (B, Beta);
      pragma Assert (abs R (B * Beta) <= Product_Limit_Real);
      pragma Assert (abs (B * Beta) <= Product_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Add (Sum, B * Beta);
      pragma Assert (abs (R (Sum) + R (B * Beta)) <= Count * Step_Limit + Product_Limit_Real);
      pragma Assert (abs R (Sum + B * Beta) <= (Count + 1.0) * Step_Limit);
      pragma Assert (abs R (Sum + B * Beta) <= Chart_Limit_Real);
      Lemma_Within_Chart_Limit (Sum + B * Beta);
   end Lemma_Magnitude_Step;

   function Basis_At (K : Chart_Coefficient_Index; X : Real) return Real
   is (Bernstein.Binomial_Form.Basis (Fixed_Chart_Degree, K, X))
   with Ghost;
   --  The exact Bernstein basis value Bₖ(X).

   function Basis_Sum (Count : Natural; X : Real) return Real
   is (if Count = 0 then 0.0 else Basis_Sum (Count - 1, X) + Basis_At (Count - 1, X))
   with Ghost, Pre => Count <= Fixed_Chart_Degree + 1, Subprogram_Variant => (Decreases => Count);
   --  The sum of the first Count basis values.

   function Chart_Sum
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; Count : Natural; X : Real) return Real
   is (Bernstein.Binomial_Form.Partial_Sum (Chart_Controls (Coefficients, Component), Count, X))
   with Ghost, Pre => Count <= Fixed_Chart_Degree + 1;
   --  The first Count terms Σ bₖ·Bₖ(X) of a chart coordinate.

   Allowance_Scale : constant := 2.0**(-46) * (1.0 - 2.0**(-53));
   --  Chart_Allowance times 1 - Epsilon: Is_Chart_Allowance promises this much of every coefficient magnitude,
   --  less Eta, because the scaling which forms the allowance is a rounded multiplication.

   Coefficient_Scale : constant := 2.0**46 * (1.0 + 2.0**(-52));
   --  More than the reciprocal of Allowance_Scale: 1/(1 - u) ≤ 1 + 2u.

   function Coefficient_Bound (Allowance : Dimensionless) return Real
   is (Coefficient_Scale * (R (Allowance) + Eta))
   with Ghost;
   --  No coefficient of a component is larger in magnitude than this, by Is_Chart_Allowance.

   Basis_Error   : constant := 2.0**(-48);
   Product_Error : constant := 2.0**(-47);
   Sum_Error     : constant := 2.0**(-52);
   --  A stored basis value is within Basis_Error of the exact one, relatively; a rounded product with it within
   --  Product_Error; and one rounded addition to the chart sum adds at most Sum_Error of the coefficient bound.

   function Chart_Error (Allowance : Dimensionless; Count : Natural; X : Real) return Real
   is (Product_Error * (Coefficient_Bound (Allowance) * Basis_Sum (Count, X))
       + Tally (Count) * (Sum_Error * Coefficient_Bound (Allowance) + Three_Eta))
   with Ghost, Pre => Count <= Fixed_Chart_Degree + 1;
   --  More than the error of the first Count terms of the floating-point chart sum.

   Loose_Error : constant := 2.0**(-40);
   Loose_Dust  : constant := 30.0 * 2.0**(-1075);
   --  A crude bound on Chart_Error: Loose_Error of the coefficient bound, and Loose_Dust.

   procedure Lemma_Chart_Step_Error (S, P, T, B, Beta, Basis, Exact, Bound, Error : Real)
   with
     Ghost,
     Pre  =>
       abs B <= Bound
       and then Basis >= 0.0
       and then Basis <= 1.0
       and then abs (Beta - Basis) <= Basis_Error * Basis
       and then abs (S - Exact) <= Error
       and then Error <= Loose_Error * Bound + Loose_Dust
       and then abs (Exact + B * Basis) <= Bound
       and then abs (P - B * Beta) <= Epsilon * abs (B * Beta) + Eta
       and then abs (T - (S + P)) <= Epsilon * abs (S + P) + Eta,
     Post => abs (T - (Exact + B * Basis)) <= Error + Product_Error * (Bound * Basis) + Sum_Error * Bound + Three_Eta;
   --  One term of the chart sum, over the reals. The stored basis value Beta is within 2⁻⁴⁸ of Basis and the
   --  product is rounded, so P is within 2⁻⁴⁷·Bound·Basis + η of B·Basis. The running sum is within a little
   --  more than Bound in magnitude, so its rounding adds at most 2⁻⁵²·Bound + 2η.

   procedure Lemma_Chart_Step_Error (S, P, T, B, Beta, Basis, Exact, Bound, Error : Real) is
      Weight : constant Real := Bound * Basis;
   begin
      Lemma_Sign (Bound, Basis);
      Lemma_Scale (Basis, 1.0, Bound);
      pragma Assert (Weight >= 0.0 and then Weight <= Bound);
      Lemma_Product_Bound (B, Basis, Bound, Basis);
      Lemma_Product_Bound (B, Beta - Basis, Bound, Basis_Error * Basis);
      pragma Assert (B * Beta - B * Basis = B * (Beta - Basis));
      pragma Assert (Bound * (Basis_Error * Basis) = Basis_Error * Weight);
      pragma Assert (abs (B * Beta - B * Basis) <= Basis_Error * Weight);
      pragma Assert (abs (B * Basis) <= Weight);
      pragma Assert (abs (B * Beta) <= Weight + Basis_Error * Weight);
      pragma Assert (abs (P - B * Basis) <= Product_Error * Weight + Eta);
      pragma Assert (abs ((S + P) - (Exact + B * Basis)) <= Error + Product_Error * Weight + Eta);
      pragma Assert (abs (S + P) <= Bound + Error + Product_Error * Weight + Eta);
      pragma Assert (abs (T - (S + P)) <= Sum_Error * Bound + Twice_Eta);
   end Lemma_Chart_Step_Error;

   procedure Lemma_Basis_Sum_Is_One (X : Real)
   with Ghost, Post => Basis_Sum (Fixed_Chart_Degree + 1, X) = 1.0;
   --  The basis values sum to one: they are the Bernstein sum of the polynomial whose controls are all one.

   procedure Lemma_Basis_Sum_Is_One (X : Real) is
      Ones : constant Bernstein.Controls (0 .. Fixed_Chart_Degree) := [others => 1.0];
   begin
      for Count in 1 .. Fixed_Chart_Degree + 1 loop
         pragma
           Assert
             (Bernstein.Binomial_Form.Partial_Sum (Ones, Count, X)
              = Bernstein.Binomial_Form.Partial_Sum (Ones, Count - 1, X)
                + Ones (Count - 1) * Bernstein.Binomial_Form.Basis (Fixed_Chart_Degree, Count - 1, X));
         pragma Loop_Invariant (Bernstein.Binomial_Form.Partial_Sum (Ones, Count, X) = Basis_Sum (Count, X));
      end loop;
      Bernstein.Binomial_Form.Lemma_Value_Is_Bernstein_Sum (Ones, X);
      Bernstein.Binomial_Form.Lemma_Value_Of_Constant (Ones, 1.0, X);
   end Lemma_Basis_Sum_Is_One;

   procedure Lemma_Basis_Is_Accurate (Sample : Sample_Nodes.Sample; K : Chart_Coefficient_Index)
   with
     Ghost,
     Pre  => Sample_Nodes.Is_Accurate (Sample),
     Post =>
       Basis_At (K, R (Sample.Node)) >= 0.0
       and then
         abs (R (Sample.Basis (K)) - Basis_At (K, R (Sample.Node))) <= Basis_Error * Basis_At (K, R (Sample.Node));
   --  One instance of Is_Accurate. The exact basis value is nonnegative because its multiple bounds a magnitude.

   procedure Lemma_Basis_Is_Accurate (Sample : Sample_Nodes.Sample; K : Chart_Coefficient_Index) is null;

   procedure Lemma_Coefficient_Is_Bounded
     (Coefficients : Bernstein_Chart;
      Delta_Y      : Component_Values;
      Component    : Chart_Component_Index;
      K            : Chart_Coefficient_Index)
   with
     Ghost,
     Pre  => Is_Chart_Allowance (Coefficients, Delta_Y),
     Post =>
       Coefficient_Bound (Delta_Y (Component)) >= 0.0
       and then R (Delta_Y (Component)) >= 0.0
       and then abs R (Coefficients (K, Component)) <= Coefficient_Bound (Delta_Y (Component));
   --  One instance of Is_Chart_Allowance, solved for the coefficient.

   procedure Lemma_Coefficient_Is_Bounded
     (Coefficients : Bernstein_Chart;
      Delta_Y      : Component_Values;
      Component    : Chart_Component_Index;
      K            : Chart_Coefficient_Index) is
   begin
      pragma Assert (Delta_Y (Component) >= 0.0);
      pragma
        Assert
          (R (Delta_Y (Component))
           >= Chart_Allowance_Real * abs R (Coefficients (K, Component)) * (1.0 - Epsilon) - Eta);
      pragma Assert (R (Delta_Y (Component)) >= Allowance_Scale * abs R (Coefficients (K, Component)) - Eta);
   end Lemma_Coefficient_Is_Bounded;

   procedure Lemma_Chart_Sum_Step
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; K : Chart_Coefficient_Index; X : Real)
   with
     Ghost,
     Post =>
       Chart_Sum (Coefficients, Component, K + 1, X)
       = Chart_Sum (Coefficients, Component, K, X) + R (Coefficients (K, Component)) * Basis_At (K, X);
   --  The definition of the partial Bernstein sum, for the chart's controls.

   procedure Lemma_Chart_Sum_Step
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; K : Chart_Coefficient_Index; X : Real)
   is
      P : constant Bernstein.Controls := Chart_Controls (Coefficients, Component);
   begin
      pragma Assert (P'First = 0 and then P'Last = Fixed_Chart_Degree);
      pragma Assert (P (K) = R (Coefficients (K, Component)));
      pragma
        Assert
          (Bernstein.Binomial_Form.Partial_Sum (P, K + 1, X)
           = Bernstein.Binomial_Form.Partial_Sum (P, K, X) + P (K) * Bernstein.Binomial_Form.Basis (P'Last, K, X));
   end Lemma_Chart_Sum_Step;

   procedure Lemma_Chart_Sum_Is_Bounded
     (Coefficients : Bernstein_Chart;
      Delta_Y      : Component_Values;
      Sample       : Sample_Nodes.Sample;
      Component    : Chart_Component_Index;
      Count        : Natural)
   with
     Ghost,
     Pre                =>
       Is_Chart_Allowance (Coefficients, Delta_Y)
       and then Sample_Nodes.Is_Accurate (Sample)
       and then Count <= Fixed_Chart_Degree + 1,
     Post               =>
       Basis_Sum (Count, R (Sample.Node)) >= 0.0
       and then
         abs Chart_Sum (Coefficients, Component, Count, R (Sample.Node))
         <= Coefficient_Bound (Delta_Y (Component)) * Basis_Sum (Count, R (Sample.Node)),
     Subprogram_Variant => (Decreases => Count);
   --  A partial chart sum is at most the coefficient bound times the partial sum of the basis values, which
   --  are nonnegative.

   procedure Lemma_Chart_Sum_Is_Bounded
     (Coefficients : Bernstein_Chart;
      Delta_Y      : Component_Values;
      Sample       : Sample_Nodes.Sample;
      Component    : Chart_Component_Index;
      Count        : Natural)
   is
      X     : constant Real := R (Sample.Node);
      Bound : constant Real := Coefficient_Bound (Delta_Y (Component));
   begin
      if Count = 0 then
         pragma Assert (Chart_Sum (Coefficients, Component, 0, X) = 0.0);
         pragma Assert (Basis_Sum (0, X) = 0.0);
      else
         Lemma_Chart_Sum_Is_Bounded (Coefficients, Delta_Y, Sample, Component, Count - 1);
         Lemma_Basis_Is_Accurate (Sample, Count - 1);
         Lemma_Coefficient_Is_Bounded (Coefficients, Delta_Y, Component, Count - 1);
         Lemma_Chart_Sum_Step (Coefficients, Component, Count - 1, X);
         Lemma_Product_Bound
           (R (Coefficients (Count - 1, Component)), Basis_At (Count - 1, X), Bound, Basis_At (Count - 1, X));
         pragma Assert (Basis_Sum (Count, X) = Basis_Sum (Count - 1, X) + Basis_At (Count - 1, X));
         Lemma_Distribute (Bound, Basis_Sum (Count - 1, X), Basis_At (Count - 1, X));
      end if;
   end Lemma_Chart_Sum_Is_Bounded;

   procedure Lemma_Basis_Sum_Is_Bounded (Sample : Sample_Nodes.Sample; Count : Natural)
   with
     Ghost,
     Pre                => Sample_Nodes.Is_Accurate (Sample) and then Count <= Fixed_Chart_Degree + 1,
     Post               => Basis_Sum (Count, R (Sample.Node)) <= 1.0,
     Subprogram_Variant => (Decreases => Fixed_Chart_Degree + 1 - Count);
   --  The later basis values are nonnegative and all of them sum to one.

   procedure Lemma_Basis_Sum_Is_Bounded (Sample : Sample_Nodes.Sample; Count : Natural) is
   begin
      if Count = Fixed_Chart_Degree + 1 then
         Lemma_Basis_Sum_Is_One (R (Sample.Node));
      else
         Lemma_Basis_Sum_Is_Bounded (Sample, Count + 1);
         Lemma_Basis_Is_Accurate (Sample, Count);
         pragma
           Assert
             (Basis_Sum (Count + 1, R (Sample.Node))
              = Basis_Sum (Count, R (Sample.Node)) + Basis_At (Count, R (Sample.Node)));
      end if;
   end Lemma_Basis_Sum_Is_Bounded;

   procedure Lemma_Accurate_Step
     (Coefficients : Bernstein_Chart;
      Delta_Y      : Component_Values;
      Sample       : Sample_Nodes.Sample;
      Component    : Chart_Component_Index;
      K            : Chart_Coefficient_Index;
      S, P, T      : Real)
   with
     Ghost,
     Pre  =>
       Is_Chart_Allowance (Coefficients, Delta_Y)
       and then Sample_Nodes.Is_Accurate (Sample)
       and then
         abs (S - Chart_Sum (Coefficients, Component, K, R (Sample.Node)))
         <= Chart_Error (Delta_Y (Component), K, R (Sample.Node))
       and then
         abs (P - R (Coefficients (K, Component)) * R (Sample.Basis (K)))
         <= Epsilon * abs (R (Coefficients (K, Component)) * R (Sample.Basis (K))) + Eta
       and then abs (T - (S + P)) <= Epsilon * abs (S + P) + Eta,
     Post =>
       abs (T - Chart_Sum (Coefficients, Component, K + 1, R (Sample.Node)))
       <= Chart_Error (Delta_Y (Component), K + 1, R (Sample.Node));
   --  The error bound of the floating-point chart sum, carried over one more term.

   procedure Lemma_Accurate_Step
     (Coefficients : Bernstein_Chart;
      Delta_Y      : Component_Values;
      Sample       : Sample_Nodes.Sample;
      Component    : Chart_Component_Index;
      K            : Chart_Coefficient_Index;
      S, P, T      : Real)
   is
      X     : constant Real := R (Sample.Node);
      Bound : constant Real := Coefficient_Bound (Delta_Y (Component));
      Basis : constant Real := Basis_At (K, X);
      B     : constant Real := R (Coefficients (K, Component));
      Exact : constant Real := Chart_Sum (Coefficients, Component, K, X);
      Error : constant Real := Chart_Error (Delta_Y (Component), K, X);
      Unit  : constant Real := Sum_Error * Bound + Three_Eta;
   begin
      Lemma_Tally_Step (K);
      Lemma_Basis_Is_Accurate (Sample, K);
      Lemma_Coefficient_Is_Bounded (Coefficients, Delta_Y, Component, K);
      Lemma_Chart_Sum_Step (Coefficients, Component, K, X);
      Lemma_Chart_Sum_Is_Bounded (Coefficients, Delta_Y, Sample, Component, K);
      Lemma_Chart_Sum_Is_Bounded (Coefficients, Delta_Y, Sample, Component, K + 1);
      Lemma_Basis_Sum_Is_Bounded (Sample, K);
      Lemma_Basis_Sum_Is_Bounded (Sample, K + 1);
      pragma Assert (Basis_Sum (K + 1, X) = Basis_Sum (K, X) + Basis);
      pragma Assert (Basis <= 1.0);
      Lemma_Scale (Basis_Sum (K, X), 1.0, Bound);
      Lemma_Scale (Basis_Sum (K + 1, X), 1.0, Bound);
      Lemma_Sign (Bound, Basis_Sum (K, X));
      pragma Assert (Bound * Basis_Sum (K, X) <= Bound);
      pragma Assert (abs (Exact + B * Basis) <= Bound);
      pragma Assert (Unit >= 0.0);
      Lemma_Scale (Tally (K), 8.0, Unit);
      pragma Assert (Error <= Product_Error * Bound + 8.0 * Unit);
      pragma Assert (Error <= Loose_Error * Bound + Loose_Dust);
      Lemma_Chart_Step_Error (S, P, T, B, R (Sample.Basis (K)), Basis, Exact, Bound, Error);
      Lemma_Distribute (Bound, Basis_Sum (K, X), Basis);
      Lemma_Distribute (Unit, Tally (K), 1.0);
      pragma
        Assert
          (Chart_Error (Delta_Y (Component), K + 1, X)
           = Error + Product_Error * (Bound * Basis) + Sum_Error * Bound + Three_Eta);
   end Lemma_Accurate_Step;

   Node_Dust : constant := 28.0 * 2.0**(-1075);
   --  The dust of a chart coordinate as At_Node computes it. It is far below Chart_Dust, which matters for the
   --  squares: Chart_Dust times a chart coordinate near Largest_Chart_Value would exceed Denominator_Dust.

   procedure Lemma_Chart_Is_Accurate
     (Coefficients : Bernstein_Chart;
      Delta_Y      : Component_Values;
      Sample       : Sample_Nodes.Sample;
      Component    : Chart_Component_Index;
      Value        : Real)
   with
     Ghost,
     Pre  =>
       Is_Chart_Allowance (Coefficients, Delta_Y)
       and then Sample_Nodes.Is_Accurate (Sample)
       and then
         abs (Value - Chart_Sum (Coefficients, Component, Fixed_Chart_Degree + 1, R (Sample.Node)))
         <= Chart_Error (Delta_Y (Component), Fixed_Chart_Degree + 1, R (Sample.Node)),
     Post =>
       abs (Value - Chart_Value (Coefficients, Component, R (Sample.Node))) <= R (Delta_Y (Component)) + Node_Dust;
   --  The complete sum is the chart coordinate, and its error bound is within the allowance: the basis values
   --  sum to one, so the bound is (2⁻⁴⁷ + 9·2⁻⁵²)·2⁴⁶·(1 + 2u)·(δ + η) + 27η, less than two thirds of δ plus
   --  28η.

   procedure Lemma_Chart_Is_Accurate
     (Coefficients : Bernstein_Chart;
      Delta_Y      : Component_Values;
      Sample       : Sample_Nodes.Sample;
      Component    : Chart_Component_Index;
      Value        : Real)
   is
      X     : constant Real := R (Sample.Node);
      Bound : constant Real := Coefficient_Bound (Delta_Y (Component));
   begin
      pragma Assert (Delta_Y (Component) >= 0.0);
      pragma Assert (R (Delta_Y (Component)) >= 0.0);
      Bernstein.Binomial_Form.Lemma_Value_Is_Bernstein_Sum (Chart_Controls (Coefficients, Component), X);
      pragma
        Assert
          (Chart_Value (Coefficients, Component, X) = Chart_Sum (Coefficients, Component, Fixed_Chart_Degree + 1, X));
      Lemma_Basis_Sum_Is_One (X);
      pragma Assert (Tally (Fixed_Chart_Degree + 1) = 9.0);
      pragma
        Assert
          (Chart_Error (Delta_Y (Component), Fixed_Chart_Degree + 1, X)
           = Product_Error * Bound + 9.0 * (Sum_Error * Bound + Three_Eta));
   end Lemma_Chart_Is_Accurate;

   ---------------------------------------------------------------------------
   --  The sums of squares and of allowances
   ---------------------------------------------------------------------------

   Square_Cap : constant := 2.0**424;
   --  No square of a chart coordinate is larger.

   Square_Limit_Real : constant := 2.0**426;
   Square_Limit      : constant Dimensionless := 2.0**426
   with Ghost;
   --  More than three times Square_Cap, with room for the rounding.

   Delta_Cap : constant := 2.0**154;
   --  More than Chart_Allowance times Magnitude_Limit, the largest chart allowance.

   Inner_Limit_Real : constant := 2.0**214;
   Inner_Limit      : constant Dimensionless := 2.0**214
   with Ghost;
   --  More than twice a chart coordinate plus its allowance.

   Allowance_Cap : constant := 2.0**368;
   --  No term δ·(2·|Ŷ| + δ) is larger.

   Allowance_Limit_Real : constant := 2.0**370;
   Allowance_Limit      : constant Dimensionless := 2.0**370
   with Ghost;
   --  More than three times Allowance_Cap, with room for the rounding.

   procedure Lemma_Within_Square_Limit (Value : Dimensionless)
   with Ghost, Pre => R (Value) <= Square_Limit_Real, Post => Value <= Square_Limit;

   procedure Lemma_Within_Square_Limit (Value : Dimensionless) is null;

   procedure Lemma_Within_Allowance_Limit (Value : Dimensionless)
   with Ghost, Pre => R (Value) <= Allowance_Limit_Real, Post => Value <= Allowance_Limit;

   procedure Lemma_Within_Allowance_Limit (Value : Dimensionless) is null;

   function Square_Factor (Count : Natural) return Real
   is ((Tally (Count) + 1.0) * Step)
   with Ghost, Pre => Count <= 3;
   --  The relative error of Square_Sum after Count components: one Step for each square and each addition.

   function Allowance_Factor (Count : Natural) return Real
   is ((Tally (Count) + 4.0) * Step)
   with Ghost, Pre => Count <= 3;
   --  The relative error of Square_Allowance after Count components. A term takes four rounded operations.

   procedure Lemma_Square_Step (Sum, Y : Dimensionless; Exact, Factor, Next_Factor, Dust : Real)
   with
     Ghost,
     Pre  =>
       Sum >= 0.0
       and then Sum <= Square_Limit
       and then abs Y <= Chart_Limit
       and then Exact >= 0.0
       and then Exact <= 2.0 * Square_Cap
       and then Factor >= Epsilon
       and then Factor <= Factor_Limit
       and then Next_Factor >= Factor + Step
       and then Next_Factor <= Factor_Limit
       and then Dust >= 0.0
       and then Dust <= Dust_Limit
       and then abs (R (Sum) - Exact) <= Factor * Exact + Dust,
     Post =>
       Y * Y >= 0.0
       and then Y * Y <= Square_Limit
       and then Sum + Y * Y >= 0.0
       and then Sum + Y * Y <= Square_Limit
       and then R (Y) * R (Y) >= 0.0
       and then R (Y) * R (Y) <= Square_Cap
       and then
         abs (R (Sum + Y * Y) - (Exact + R (Y) * R (Y))) <= Next_Factor * (Exact + R (Y) * R (Y)) + Dust + Three_Eta;
   --  One more square added to Square_Sum: nothing overflows, and the sum's error bound is carried along.

   procedure Lemma_Square_Step (Sum, Y : Dimensionless; Exact, Factor, Next_Factor, Dust : Real) is
      Term : constant Real := R (Y) * R (Y);
   begin
      Lemma_Square_Sign (Y);
      pragma Assert (abs R (Y) <= Chart_Limit_Real);
      Lemma_Product_Bound (R (Y), R (Y), Chart_Limit_Real, Chart_Limit_Real);
      pragma Assert (Term >= 0.0);
      pragma Assert (Term <= Square_Cap);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Y, Y);
      pragma Assert (abs (R (Y * Y) - Term) <= Epsilon * Term + Eta);
      pragma Assert (R (Y * Y) <= 2.0 * Square_Cap);
      Lemma_Within_Square_Limit (Y * Y);
      Lemma_Sum_Sign (Sum, Y * Y);
      Float_Lemmas.Lemma_Rounding_Error_Add (Sum, Y * Y);
      Lemma_Scale (Epsilon, Factor, Term);
      Lemma_Accumulate
        (S           => R (Sum),
         P           => R (Y * Y),
         T           => R (Sum + Y * Y),
         Exact       => Exact,
         Size        => Exact,
         Term        => Term,
         Factor      => Factor,
         Next_Factor => Next_Factor,
         Dust        => Dust,
         Term_Dust   => Eta,
         Unit        => Twice_Eta);
      Lemma_Scale (Next_Factor, Factor_Limit, Exact + Term);
      pragma Assert (R (Sum + Y * Y) <= Square_Limit_Real);
      Lemma_Within_Square_Limit (Sum + Y * Y);
   end Lemma_Square_Step;

   Loose_Factor : constant := 2.01 * 2.0**(-53);
   --  A little more than the relative error of two rounded operations in succession.

   procedure Lemma_Dust_Scale (D, Y : Real)
   with Ghost, Pre => D >= 0.0 and then Y >= 0.0, Post => D <= 1.0 + D * (2.0 * Y + D);
   --  D ≤ 1, or else D ≤ D². An underflow error which is multiplied by D is therefore either still dust or
   --  negligible beside the term D·(2·Y + D).

   procedure Lemma_Dust_Scale (D, Y : Real) is
   begin
      Lemma_Sign (D, Y);
      Lemma_Sign (D, D);
      pragma Assert (D * (2.0 * Y + D) = 2.0 * (D * Y) + D * D);
      if D > 1.0 then
         Lemma_Scale (1.0, D, D);
      end if;
   end Lemma_Dust_Scale;

   procedure Lemma_Allowance_Term (D, Y, Doubled, Inner, Product : Real)
   with
     Ghost,
     Pre  =>
       D >= 0.0
       and then Y >= 0.0
       and then abs (Doubled - 2.0 * Y) <= Epsilon * (2.0 * Y) + Eta
       and then abs (Inner - (Doubled + D)) <= Epsilon * abs (Doubled + D) + Eta
       and then abs (Product - D * Inner) <= Epsilon * abs (D * Inner) + Eta,
     Post => abs (Product - D * (2.0 * Y + D)) <= Four_Steps * (D * (2.0 * Y + D)) + Five_Eta;
   --  The three rounded operations of one term δ·(2·|Ŷ| + δ) of Square_Allowance: within a little more than 3u
   --  of the exact term, and 5η.

   procedure Lemma_Allowance_Term (D, Y, Doubled, Inner, Product : Real) is
      Base : constant Real := 2.0 * Y + D;
      Term : constant Real := D * Base;
   begin
      Lemma_Sign (D, Base);
      pragma Assert (Term >= 0.0);
      pragma Assert (abs (Doubled + D - Base) <= Epsilon * Base + Eta);
      pragma Assert (abs (Doubled + D) <= Base + Epsilon * Base + Eta);
      pragma Assert (abs (Inner - Base) <= Loose_Factor * Base + Three_Eta);
      Lemma_Product_Bound (D, Inner - Base, D, Loose_Factor * Base + Three_Eta);
      pragma Assert (D * Inner - Term = D * (Inner - Base));
      pragma Assert (D * (Loose_Factor * Base + Three_Eta) = Loose_Factor * Term + Three_Eta * D);
      pragma Assert (abs (D * Inner - Term) <= Loose_Factor * Term + Three_Eta * D);
      Lemma_Dust_Scale (D, Y);
      pragma Assert (Three_Eta * D <= Three_Eta + Three_Eta * Term);
      pragma Assert (abs (D * Inner) <= Term + Loose_Factor * Term + Three_Eta * D);
      pragma
        Assert
          (abs (Product - Term)
           <= Epsilon * (Term + Loose_Factor * Term + Three_Eta * D) + Eta + Loose_Factor * Term + Three_Eta * D);
   end Lemma_Allowance_Term;

   procedure Lemma_Allowance_Step (Sum, D, Y : Dimensionless; Exact, Factor, Next_Factor, Dust : Real)
   with
     Ghost,
     Pre  =>
       Sum >= 0.0
       and then Sum <= Allowance_Limit
       and then D >= 0.0
       and then D <= Chart_Allowance * Magnitude_Limit
       and then abs Y <= Chart_Limit
       and then Exact >= 0.0
       and then Exact <= 2.0 * Allowance_Cap
       and then Factor >= Four_Steps
       and then Factor <= Factor_Limit
       and then Next_Factor >= Factor + Step
       and then Next_Factor <= Factor_Limit
       and then Dust >= 0.0
       and then Dust <= Dust_Limit
       and then abs (R (Sum) - Exact) <= Factor * Exact + Dust,
     Post =>
       2.0 * abs Y >= 0.0
       and then 2.0 * abs Y <= Inner_Limit
       and then 2.0 * abs Y + D >= 0.0
       and then 2.0 * abs Y + D <= Inner_Limit
       and then D * (2.0 * abs Y + D) >= 0.0
       and then D * (2.0 * abs Y + D) <= Allowance_Limit
       and then Sum + D * (2.0 * abs Y + D) >= 0.0
       and then Sum + D * (2.0 * abs Y + D) <= Allowance_Limit
       and then R (D) * (2.0 * abs R (Y) + R (D)) >= 0.0
       and then R (D) * (2.0 * abs R (Y) + R (D)) <= Allowance_Cap
       and then
         abs (R (Sum + D * (2.0 * abs Y + D)) - (Exact + R (D) * (2.0 * abs R (Y) + R (D))))
         <= Next_Factor * (Exact + R (D) * (2.0 * abs R (Y) + R (D))) + Dust + Seven_Eta;
   --  One more term added to Square_Allowance: nothing overflows, and the sum's error bound is carried along.

   procedure Lemma_Allowance_Step (Sum, D, Y : Dimensionless; Exact, Factor, Next_Factor, Dust : Real) is
      Size : constant Real := abs R (Y);
      Base : constant Real := 2.0 * Size + R (D);
      Term : constant Real := R (D) * Base;
   begin
      Lemma_Abs_Is_Exact (Y);
      pragma Assert (Size <= Chart_Limit_Real);
      pragma Assert (R (D) >= 0.0 and then R (D) <= Delta_Cap);
      Lemma_Product_Bound (R (D), Base, Delta_Cap, 2.0 * Chart_Limit_Real + Delta_Cap);
      Lemma_Sign (R (D), Base);
      pragma Assert (Term >= 0.0 and then Term <= Allowance_Cap);

      Lemma_Product_Sign (2.0, abs Y);
      Float_Lemmas.Lemma_Rounding_Error_Mul (2.0, abs Y);
      pragma Assert (abs (R (2.0 * abs Y) - 2.0 * Size) <= Epsilon * (2.0 * Size) + Eta);
      pragma Assert (R (2.0 * abs Y) <= Inner_Limit_Real);
      pragma Assert (2.0 * abs Y <= Inner_Limit);

      Lemma_Sum_Sign (2.0 * abs Y, D);
      Float_Lemmas.Lemma_Rounding_Error_Add (2.0 * abs Y, D);
      pragma Assert (R (2.0 * abs Y + D) <= Inner_Limit_Real);
      pragma Assert (2.0 * abs Y + D <= Inner_Limit);

      Lemma_Product_Sign (D, 2.0 * abs Y + D);
      Float_Lemmas.Lemma_Rounding_Error_Mul (D, 2.0 * abs Y + D);
      Lemma_Allowance_Term (R (D), Size, R (2.0 * abs Y), R (2.0 * abs Y + D), R (D * (2.0 * abs Y + D)));
      pragma Assert (abs (R (D * (2.0 * abs Y + D)) - Term) <= Four_Steps * Term + Five_Eta);
      pragma Assert (R (D * (2.0 * abs Y + D)) <= 2.0 * Allowance_Cap);
      Lemma_Within_Allowance_Limit (D * (2.0 * abs Y + D));

      Lemma_Sum_Sign (Sum, D * (2.0 * abs Y + D));
      Float_Lemmas.Lemma_Rounding_Error_Add (Sum, D * (2.0 * abs Y + D));
      Lemma_Scale (Four_Steps, Factor, Term);
      Lemma_Accumulate
        (S           => R (Sum),
         P           => R (D * (2.0 * abs Y + D)),
         T           => R (Sum + D * (2.0 * abs Y + D)),
         Exact       => Exact,
         Size        => Exact,
         Term        => Term,
         Factor      => Factor,
         Next_Factor => Next_Factor,
         Dust        => Dust,
         Term_Dust   => Five_Eta,
         Unit        => Twice_Eta);
      Lemma_Scale (Next_Factor, Factor_Limit, Exact + Term);
      pragma Assert (R (Sum + D * (2.0 * abs Y + D)) <= Allowance_Limit_Real);
      Lemma_Within_Allowance_Limit (Sum + D * (2.0 * abs Y + D));
   end Lemma_Allowance_Step;

   procedure Lemma_Allowance_Is_Bounded (D, Y : Real)
   with
     Ghost,
     Pre  => D >= 0.0 and then D <= Delta_Cap,
     Post => D * (2.0 * abs Y + D) >= 0.0 and then D * (2.0 * abs Y + D) <= Delta_Cap * (1.0 + Delta_Cap + Y * Y);
   --  2·|Y| ≤ 1 + Y², so the allowances are bounded through the sum of squares.

   procedure Lemma_Allowance_Is_Bounded (D, Y : Real) is
      Size : constant Real := abs Y;
   begin
      pragma Assert ((Size - 1.0) * (Size - 1.0) >= 0.0);
      pragma Assert ((Size - 1.0) * (Size - 1.0) = Size * Size - 2.0 * Size + 1.0);
      pragma Assert (Size * Size = Y * Y);
      pragma Assert (2.0 * Size + D <= 1.0 + Delta_Cap + Y * Y);
      Lemma_Sign (D, 2.0 * Size + D);
      Lemma_Scale (2.0 * Size + D, 1.0 + Delta_Cap + Y * Y, D);
      Lemma_Scale (D, Delta_Cap, 1.0 + Delta_Cap + Y * Y);
   end Lemma_Allowance_Is_Bounded;

   ---------------------------------------------------------------------------
   --  The squares against the squares of the exact chart coordinates
   ---------------------------------------------------------------------------

   function True_Squares (Coefficients : Bernstein_Chart; X : Real; Count : Natural) return Real
   is (case Count is
         when 0      => Real'(0.0),
         when 1      => Chart_Value (Coefficients, 1, X) * Chart_Value (Coefficients, 1, X),
         when 2      =>
           Chart_Value (Coefficients, 1, X) * Chart_Value (Coefficients, 1, X)
           + Chart_Value (Coefficients, 2, X) * Chart_Value (Coefficients, 2, X),
         when others =>
           Chart_Value (Coefficients, 1, X) * Chart_Value (Coefficients, 1, X)
           + Chart_Value (Coefficients, 2, X) * Chart_Value (Coefficients, 2, X)
           + Chart_Value (Coefficients, 3, X) * Chart_Value (Coefficients, 3, X))
   with Ghost, Pre => Count <= 3;
   --  The sum of the squares of the first Count exact chart coordinates.

   procedure Lemma_True_Squares_Step (Coefficients : Bernstein_Chart; X : Real; Component : Chart_Component_Index)
   with
     Ghost,
     Post =>
       True_Squares (Coefficients, X, Component)
       = True_Squares (Coefficients, X, Component - 1)
         + Chart_Value (Coefficients, Component, X) * Chart_Value (Coefficients, Component, X);

   procedure Lemma_True_Squares_Step (Coefficients : Bernstein_Chart; X : Real; Component : Chart_Component_Index) is
      Square_1 : constant Real := Chart_Value (Coefficients, 1, X) * Chart_Value (Coefficients, 1, X);
      Square_2 : constant Real := Chart_Value (Coefficients, 2, X) * Chart_Value (Coefficients, 2, X);
      Square_3 : constant Real := Chart_Value (Coefficients, 3, X) * Chart_Value (Coefficients, 3, X);
   begin
      case Component is
         when 1 =>
            pragma Assert (True_Squares (Coefficients, X, 0) = 0.0);
            pragma Assert (True_Squares (Coefficients, X, 1) = Square_1);

         when 2 =>
            pragma Assert (True_Squares (Coefficients, X, 1) = Square_1);
            pragma Assert (True_Squares (Coefficients, X, 2) = Square_1 + Square_2);

         when 3 =>
            pragma Assert (True_Squares (Coefficients, X, 2) = Square_1 + Square_2);
            pragma Assert (True_Squares (Coefficients, X, 3) = Square_1 + Square_2 + Square_3);
      end case;
   end Lemma_True_Squares_Step;

   Two_To_50    : constant := 2.0**50;
   Weight_Unit  : constant := 2.0**50 + 2.0;
   Weight_Scale : constant := 2.0**(-50);
   Root_Scale   : constant := 2.0**(-25);
   Root_Unit    : constant := 2.0**25;

   procedure Lemma_Square_Error (Computed, Exact, D : Real)
   with
     Ghost,
     Pre  => D >= 0.0 and then abs (Computed - Exact) <= D + Node_Dust,
     Post =>
       abs (Computed * Computed - Exact * Exact)
       <= D * (2.0 * abs Computed + D)
          + Node_Dust * (Weight_Unit + Weight_Scale * (Computed * Computed) + D * (2.0 * abs Computed + D));
   --  |Ŷ² - Y²| = |Ŷ - Y|·|Ŷ + Y| ≤ (δ + ρ)·(2·|Ŷ| + δ + ρ) = δ·(2·|Ŷ| + δ) + ρ·(2·|Ŷ| + 2·δ + ρ) with ρ the
   --  dust. In the second term 2·|Ŷ| ≤ 2⁵⁰ + 2⁻⁵⁰·Ŷ² and 2·δ ≤ 1 + δ², which the first term exceeds: the dust
   --  is multiplied by quantities which the final sums bound.

   procedure Lemma_Square_Error (Computed, Exact, D : Real) is
      Size : constant Real := abs Computed;
      Term : constant Real := D * (2.0 * Size + D);
   begin
      pragma Assert (Computed * Computed - Exact * Exact = (Computed - Exact) * (Computed + Exact));
      pragma Assert (abs (Computed + Exact) <= 2.0 * Size + D + Node_Dust);
      Lemma_Product_Bound (Computed - Exact, Computed + Exact, D + Node_Dust, 2.0 * Size + D + Node_Dust);
      pragma
        Assert
          ((D + Node_Dust) * (2.0 * Size + D + Node_Dust) = Term + Node_Dust * (2.0 * Size + 2.0 * D + Node_Dust));
      pragma Assert ((Root_Scale * Size - Root_Unit) * (Root_Scale * Size - Root_Unit) >= 0.0);
      pragma
        Assert
          ((Root_Scale * Size - Root_Unit) * (Root_Scale * Size - Root_Unit)
           = Weight_Scale * (Size * Size) - 2.0 * Size + Two_To_50);
      pragma Assert (Size * Size = Computed * Computed);
      pragma Assert (2.0 * Size <= Two_To_50 + Weight_Scale * (Computed * Computed));
      pragma Assert ((D - 1.0) * (D - 1.0) >= 0.0);
      pragma Assert ((D - 1.0) * (D - 1.0) = D * D - 2.0 * D + 1.0);
      Lemma_Sign (D, Size);
      pragma Assert (Term = 2.0 * (D * Size) + D * D);
      pragma Assert (2.0 * D <= 1.0 + Term);
      pragma Assert (2.0 * Size + 2.0 * D + Node_Dust <= Weight_Unit + Weight_Scale * (Computed * Computed) + Term);
   end Lemma_Square_Error;

   procedure Lemma_Component_Error
     (Coefficients                    : Bernstein_Chart;
      Sample                          : Sample_Nodes.Sample;
      Component                       : Chart_Component_Index;
      Computed, D, Squares, Allowance : Real)
   with
     Ghost,
     Pre  =>
       D >= 0.0
       and then
         (if Sample_Nodes.Is_Accurate (Sample)
          then
            abs (Computed - Chart_Value (Coefficients, Component, R (Sample.Node))) <= D + Node_Dust
            and then
              abs (Squares - True_Squares (Coefficients, R (Sample.Node), Component - 1))
              <= Allowance + Node_Dust * (Tally (Component - 1) * Weight_Unit + Weight_Scale * Squares + Allowance)),
     Post =>
       (if Sample_Nodes.Is_Accurate (Sample)
        then
          abs (Squares + Computed * Computed - True_Squares (Coefficients, R (Sample.Node), Component))
          <= Allowance + D * (2.0 * abs Computed + D)
             + Node_Dust
               * (Tally (Component) * Weight_Unit
                  + Weight_Scale * (Squares + Computed * Computed)
                  + (Allowance + D * (2.0 * abs Computed + D))));
   --  The sum of the computed squares is within the sum of the allowance terms of the exact one, and a dust.

   procedure Lemma_Component_Error
     (Coefficients                    : Bernstein_Chart;
      Sample                          : Sample_Nodes.Sample;
      Component                       : Chart_Component_Index;
      Computed, D, Squares, Allowance : Real)
   is
      X     : constant Real := R (Sample.Node);
      Exact : constant Real := Chart_Value (Coefficients, Component, X);
   begin
      if Sample_Nodes.Is_Accurate (Sample) then
         Lemma_Tally_Step (Component - 1);
         Lemma_Square_Error (Computed, Exact, D);
         Lemma_True_Squares_Step (Coefficients, X, Component);
      end if;
   end Lemma_Component_Error;

   ---------------------------------------------------------------------------
   --  The denominator, the first numerator and their allowance
   ---------------------------------------------------------------------------

   Finish_Limit_Real : constant := 2.0**427;
   Finish_Limit      : constant Dimensionless := 2.0**427
   with Ghost;
   --  More than 1 + Square_Limit, and than the complete allowance.

   Denominator_Cap : constant := 2.0**100;
   --  More than Largest_Ideal_Denominator.

   Root_Cap : constant := 2.0**101;
   --  More than the sum of squares where the denominator is within Denominator_Cap, and less than the square
   --  of Largest_Chart_Value.

   Allowance_Bound_Real : constant := 2.0**312;
   --  More than the complete allowance where the denominator is within Denominator_Cap, and less than
   --  Largest_Allowance.

   Exact_Allowance_Cap : constant := 2.0**310;
   --  More than the exact sum of allowance terms where the sum of squares is within Root_Cap.

   Nine_Eta       : constant := 9.0 * 2.0**(-1075);
   Twenty_One_Eta : constant := 21.0 * 2.0**(-1075);

   function Is_Finish_Rounded (S, A, Q, L, W, Total : Real) return Boolean
   is (abs (Q - (1.0 + S)) <= Epsilon * (1.0 + S) + Eta
       and then abs (L - (1.0 - S)) <= Epsilon * (1.0 + S) + Eta
       and then abs (W - Small_Allowance_Real * Q) <= Epsilon * (Small_Allowance_Real * Q) + Eta
       and then abs (Total - (A + W)) <= Epsilon * (A + W) + Eta)
   with Ghost;
   --  The four rounded operations after the loop, for S = Square_Sum ≥ 0 and A = Square_Allowance ≥ 0:
   --  Q = fl (1 + S), L = fl (1 - S), W = fl (2⁻⁵⁰·Q) and Total = fl (A + W).

   procedure Lemma_Finish (Square_Sum, Square_Allowance : Dimensionless)
   with
     Ghost,
     Pre  =>
       Square_Sum >= 0.0
       and then Square_Sum <= Square_Limit
       and then Square_Allowance >= 0.0
       and then Square_Allowance <= Allowance_Limit,
     Post =>
       1.0 + Square_Sum >= 1.0
       and then 1.0 + Square_Sum <= Finish_Limit
       and then abs (1.0 - Square_Sum) <= Finish_Limit
       and then Small_Allowance * (1.0 + Square_Sum) >= 0.0
       and then Small_Allowance * (1.0 + Square_Sum) <= Finish_Limit
       and then Square_Allowance + Small_Allowance * (1.0 + Square_Sum) >= 0.0
       and then Square_Allowance + Small_Allowance * (1.0 + Square_Sum) <= Finish_Limit
       and then R (Small_Allowance * (1.0 + Square_Sum)) >= 0.0
       and then R (Square_Sum) >= 0.0
       and then R (Square_Allowance) >= 0.0
       and then
         (if 1.0 + Square_Sum <= Largest_Ideal_Denominator
          then abs (1.0 - Square_Sum) <= Largest_Ideal_Denominator and then R (1.0 + Square_Sum) <= Denominator_Cap)
       and then
         Is_Finish_Rounded
           (R (Square_Sum),
            R (Square_Allowance),
            R (1.0 + Square_Sum),
            R (1.0 - Square_Sum),
            R (Small_Allowance * (1.0 + Square_Sum)),
            R (Square_Allowance + Small_Allowance * (1.0 + Square_Sum)));
   --  The operations after the loop: none overflows, the signs are as expected, and each is rounded.

   procedure Lemma_Finish (Square_Sum, Square_Allowance : Dimensionless) is
   begin
      pragma Assert (R (Square_Sum) >= 0.0 and then R (Square_Sum) <= Square_Limit_Real);
      pragma Assert (R (Square_Allowance) >= 0.0 and then R (Square_Allowance) <= Allowance_Limit_Real);

      Lemma_One_More (Square_Sum);
      Float_Lemmas.Lemma_Rounding_Error_Add (1.0, Square_Sum);
      pragma Assert (abs (R (1.0 + Square_Sum) - (1.0 + R (Square_Sum))) <= Epsilon * (1.0 + R (Square_Sum)) + Eta);
      pragma Assert (R (1.0 + Square_Sum) <= Finish_Limit_Real);
      pragma Assert (1.0 + Square_Sum <= Finish_Limit);

      Float_Lemmas.Lemma_Rounding_Error_Sub (1.0, Square_Sum);
      pragma Assert (abs (R (1.0 - Square_Sum) - (1.0 - R (Square_Sum))) <= Epsilon * (1.0 + R (Square_Sum)) + Eta);
      pragma Assert (abs R (1.0 - Square_Sum) <= Finish_Limit_Real);
      pragma Assert (abs (1.0 - Square_Sum) <= Finish_Limit);

      Lemma_Product_Sign (Small_Allowance, 1.0 + Square_Sum);
      Float_Lemmas.Lemma_Rounding_Error_Mul (Small_Allowance, 1.0 + Square_Sum);
      pragma Assert (R (Small_Allowance) = Small_Allowance_Real);
      pragma Assert (R (1.0 + Square_Sum) >= 1.0);
      pragma
        Assert
          (abs (R (Small_Allowance * (1.0 + Square_Sum)) - Small_Allowance_Real * R (1.0 + Square_Sum))
           <= Epsilon * (Small_Allowance_Real * R (1.0 + Square_Sum)) + Eta);
      pragma Assert (R (Small_Allowance * (1.0 + Square_Sum)) >= 0.0);
      pragma Assert (R (Small_Allowance * (1.0 + Square_Sum)) <= Finish_Limit_Real);
      pragma Assert (Small_Allowance * (1.0 + Square_Sum) <= Finish_Limit);

      Lemma_Sum_Sign (Square_Allowance, Small_Allowance * (1.0 + Square_Sum));
      Float_Lemmas.Lemma_Rounding_Error_Add (Square_Allowance, Small_Allowance * (1.0 + Square_Sum));
      pragma Assert (R (Square_Allowance + Small_Allowance * (1.0 + Square_Sum)) <= Finish_Limit_Real);
      pragma Assert (Square_Allowance + Small_Allowance * (1.0 + Square_Sum) <= Finish_Limit);

      if 1.0 + Square_Sum <= Largest_Ideal_Denominator then
         Lemma_One_Less (Square_Sum);
         pragma Assert (R (1.0 + Square_Sum) <= Denominator_Cap);
      end if;
   end Lemma_Finish;

   procedure Lemma_Finish_Bounds (S, A, Q, L, W, Total, Squares, Allowance : Real)
   with
     Ghost,
     Pre  =>
       S >= 0.0
       and then A >= 0.0
       and then W >= 0.0
       and then Squares >= 0.0
       and then Allowance >= 0.0
       and then Is_Finish_Rounded (S, A, Q, L, W, Total)
       and then abs (S - Squares) <= Four_Steps * Squares + Nine_Eta
       and then abs (A - Allowance) <= Seven_Steps * Allowance + Twenty_One_Eta
       and then Allowance <= Delta_Cap * (3.0 * (1.0 + Delta_Cap) + Squares),
     Post => (if Q <= Denominator_Cap then Squares <= Root_Cap and then Total <= Allowance_Bound_Real);
   --  Where the denominator is within its limit, so is the sum of squares, and through it the allowance.

   procedure Lemma_Finish_Bounds (S, A, Q, L, W, Total, Squares, Allowance : Real) is
   begin
      if Q <= Denominator_Cap then
         pragma Assert (S <= 2.0 * Denominator_Cap);
         pragma Assert (Squares <= Root_Cap);
         pragma Assert (Allowance <= Exact_Allowance_Cap);
         pragma Assert (A <= 2.0 * Exact_Allowance_Cap);
         pragma Assert (W <= Denominator_Cap);
      end if;
   end Lemma_Finish_Bounds;

   Dust_Bound : constant := 2.0**58 * 2.0**(-1075);
   --  More than Node_Dust times what multiplies it in the final sums.

   procedure Lemma_Finish_Error (S, A, Q, L, W, Total, Squares, Allowance, Exact : Real)
   with
     Ghost,
     Pre  =>
       S >= 0.0
       and then A >= 0.0
       and then W >= 0.0
       and then Squares >= 0.0
       and then Allowance >= 0.0
       and then Is_Finish_Rounded (S, A, Q, L, W, Total)
       and then abs (S - Squares) <= Four_Steps * Squares + Nine_Eta
       and then abs (A - Allowance) <= Seven_Steps * Allowance + Twenty_One_Eta
       and then
         abs (Squares - Exact) <= Allowance + Node_Dust * (3.0 * Weight_Unit + Weight_Scale * Squares + Allowance)
       and then Q <= Denominator_Cap
       and then Total <= 1.0,
     Post =>
       abs (Q - (1.0 + Exact)) <= Total * (1.0 + Node_Slack) + Denominator_Dust
       and then abs (L - (1.0 - Exact)) <= Total * (1.0 + Node_Slack) + Denominator_Dust;
   --  The error of the denominator and of the first numerator. The squares and their sum round by at most
   --  5.1u·Q in all, and the exact sum of allowance terms is at most (1 + 8u) times the computed one; the
   --  allowance's own term 8u·Q and the slack 32u cover both. The complete allowance being at most one keeps
   --  what multiplies the dust small.

   procedure Lemma_Finish_Error (S, A, Q, L, W, Total, Squares, Allowance, Exact : Real) is
      Rounding : constant Real := Epsilon * (1.0 + S) + Four_Steps * Squares;
   begin
      pragma Assert (A + W <= 1.01);
      pragma Assert (Allowance <= 1.02);
      pragma Assert (S <= 2.0 * Denominator_Cap);
      pragma Assert (Squares <= Root_Cap);
      pragma Assert (Node_Dust * (3.0 * Weight_Unit + Weight_Scale * Squares + Allowance) <= Dust_Bound);
      pragma Assert (abs (Q - (1.0 + Exact)) <= Rounding + 10.0 * Eta + Allowance + Dust_Bound);
      pragma Assert (abs (L - (1.0 - Exact)) <= Rounding + 10.0 * Eta + Allowance + Dust_Bound);
      pragma Assert (Four_Steps * Squares <= 4.04 * Epsilon * S + Eta);
      pragma Assert (Rounding <= 5.04 * Epsilon * (1.0 + S) + Eta);
      pragma Assert (Rounding <= 5.1 * Epsilon * Q + 2.0 * Eta);
      pragma Assert (Allowance <= A + 8.0 * Epsilon * A + 22.0 * Eta);
      pragma Assert (W >= 8.0 * Epsilon * Q - 8.0 * Epsilon * Epsilon * Q - Eta);
      pragma Assert (Total >= A + W - Epsilon * (A + W) - Eta);
      pragma Assert (Total * (1.0 + Node_Slack) >= A + 8.0 * Epsilon * A + 5.1 * Epsilon * Q - 4.0 * Eta);
   end Lemma_Finish_Error;

   procedure Lemma_Finish_Accuracy
     (Coefficients : Bernstein_Chart; Sample : Sample_Nodes.Sample; S, A, Q, L, W, Total, Squares, Allowance : Real)
   with
     Ghost,
     Pre  =>
       S >= 0.0
       and then A >= 0.0
       and then W >= 0.0
       and then Squares >= 0.0
       and then Allowance >= 0.0
       and then Is_Finish_Rounded (S, A, Q, L, W, Total)
       and then abs (S - Squares) <= Four_Steps * Squares + Nine_Eta
       and then abs (A - Allowance) <= Seven_Steps * Allowance + Twenty_One_Eta
       and then
         (if Sample_Nodes.Is_Accurate (Sample)
          then
            abs (Squares - True_Squares (Coefficients, R (Sample.Node), 3))
            <= Allowance + Node_Dust * (3.0 * Weight_Unit + Weight_Scale * Squares + Allowance)),
     Post =>
       (if Sample_Nodes.Is_Accurate (Sample) and then Q <= Denominator_Cap and then Total <= 1.0
        then
          abs (Q - Denominator (Coefficients, R (Sample.Node))) <= Total * (1.0 + Node_Slack) + Denominator_Dust
          and then
            abs (L - Numerator (Coefficients, 0, R (Sample.Node))) <= Total * (1.0 + Node_Slack) + Denominator_Dust
          and then abs (Q - Denominator (Coefficients, R (Sample.Node))) <= Total * (1.0 + Slack) + Denominator_Dust
          and then abs (L - Numerator (Coefficients, 0, R (Sample.Node))) <= Total * (1.0 + Slack) + Denominator_Dust);
   --  Lemma_Finish_Error for the chart's denominator and first numerator, at an accurate sample.

   procedure Lemma_Finish_Accuracy
     (Coefficients : Bernstein_Chart; Sample : Sample_Nodes.Sample; S, A, Q, L, W, Total, Squares, Allowance : Real)
   is
      X     : constant Real := R (Sample.Node);
      Exact : constant Real := True_Squares (Coefficients, X, 3);
   begin
      if Sample_Nodes.Is_Accurate (Sample) and then Q <= Denominator_Cap and then Total <= 1.0 then
         Lemma_Finish_Error (S, A, Q, L, W, Total, Squares, Allowance, Exact);
         pragma Assert (Denominator (Coefficients, X) = 1.0 + Exact);
         pragma Assert (Numerator (Coefficients, 0, X) = 1.0 - Exact);
         pragma Assert (Total >= 0.0);
         pragma Assert (Total * (1.0 + Node_Slack) <= Total * (1.0 + Slack));
      end if;
   end Lemma_Finish_Accuracy;

   procedure Lemma_Chart_Value_Is_Bounded (Value : Dimensionless; Squares : Real)
   with
     Ghost,
     Pre  => R (Value) * R (Value) <= Squares,
     Post => (if Squares <= Root_Cap then abs Value <= Largest_Chart_Value);
   --  A chart coordinate whose square is at most 2¹⁰¹ is within 2·10¹⁵, whose square is 4·10³⁰.

   procedure Lemma_Chart_Value_Is_Bounded (Value : Dimensionless; Squares : Real) is
      Size : constant Real := abs R (Value);
   begin
      if Squares <= Root_Cap then
         pragma Assert (Size * Size = R (Value) * R (Value));
         if Size > 2.0E15 then
            Lemma_Scale (2.0E15, Size, Size);
            Lemma_Scale (2.0E15, Size, 2.0E15);
            pragma Assert (Size * Size >= 4.0E30);
            pragma Assert (False);
         end if;
         pragma Assert (abs R (Value) <= 2.0E15);
      end if;
   end Lemma_Chart_Value_Is_Bounded;

   procedure Lemma_Allowance_Fits (Value : Dimensionless)
   with Ghost, Post => (if R (Value) <= Allowance_Bound_Real then Value <= Largest_Allowance);

   procedure Lemma_Allowance_Fits (Value : Dimensionless) is null;

   procedure Lemma_At_Most_One (Value : Dimensionless)
   with Ghost, Post => (if Value <= 1.0 then R (Value) <= 1.0);

   procedure Lemma_At_Most_One (Value : Dimensionless) is null;

   procedure Lemma_Chart_Values_Are_Bounded (Y : Component_Values; Squares : Real)
   with
     Ghost,
     Pre  =>
       R (Y (1)) * R (Y (1)) <= Squares
       and then R (Y (2)) * R (Y (2)) <= Squares
       and then R (Y (3)) * R (Y (3)) <= Squares,
     Post => (if Squares <= Root_Cap then (for all J in Chart_Component_Index => abs Y (J) <= Largest_Chart_Value));
   --  The same for all three chart coordinates.

   procedure Lemma_Chart_Values_Are_Bounded (Y : Component_Values; Squares : Real) is
   begin
      Lemma_Chart_Value_Is_Bounded (Y (1), Squares);
      Lemma_Chart_Value_Is_Bounded (Y (2), Squares);
      Lemma_Chart_Value_Is_Bounded (Y (3), Squares);
   end Lemma_Chart_Values_Are_Bounded;

   procedure Lemma_Current_Is_Zero (Y : Component_Values; Component : Chart_Component_Index)
   with
     Ghost,
     Pre  => (if Component <= 1 then Y (1) = 0.0) and then (if Component <= 2 then Y (2) = 0.0) and then Y (3) = 0.0,
     Post => Y (Component) = 0.0;
   --  The chart coordinates which At_Node has not reached are still zero, the next one among them.

   procedure Lemma_Current_Is_Zero (Y : Component_Values; Component : Chart_Component_Index) is null;

   procedure Lemma_Extend_Squares
     (Y : Component_Values; Component : Chart_Component_Index; Squares, Longer_Squares : Real)
   with
     Ghost,
     Pre  =>
       Squares >= 0.0
       and then R (Y (Component)) * R (Y (Component)) >= 0.0
       and then Longer_Squares = Squares + R (Y (Component)) * R (Y (Component))
       and then (if Component > 1 then R (Y (1)) * R (Y (1)) <= Squares)
       and then (if Component > 2 then R (Y (2)) * R (Y (2)) <= Squares),
     Post =>
       (if Component >= 1 then R (Y (1)) * R (Y (1)) <= Longer_Squares)
       and then (if Component >= 2 then R (Y (2)) * R (Y (2)) <= Longer_Squares)
       and then (if Component >= 3 then R (Y (3)) * R (Y (3)) <= Longer_Squares);
   --  Every square is at most the sum of the squares so far, which one more square only raises.

   procedure Lemma_Extend_Squares
     (Y : Component_Values; Component : Chart_Component_Index; Squares, Longer_Squares : Real) is
   begin
      case Component is
         when 1 =>
            pragma Assert (R (Y (1)) * R (Y (1)) <= Longer_Squares);

         when 2 =>
            pragma Assert (R (Y (1)) * R (Y (1)) <= Longer_Squares);
            pragma Assert (R (Y (2)) * R (Y (2)) <= Longer_Squares);

         when 3 =>
            pragma Assert (R (Y (1)) * R (Y (1)) <= Longer_Squares);
            pragma Assert (R (Y (2)) * R (Y (2)) <= Longer_Squares);
            pragma Assert (R (Y (3)) * R (Y (3)) <= Longer_Squares);
      end case;
   end Lemma_Extend_Squares;

   ---------------------------------------------------------------------------
   --  What At_Node carries along
   ---------------------------------------------------------------------------

   --  The facts about the exact chart rest on recursive definitions, which the provers unfold without end
   --  wherever they see them, and the error bounds of the running sums are nonlinear. At_Node only has to carry
   --  these facts from one statement to the next, so it sees them as opaque predicates: their definitions are
   --  in the body of Tracking, and so are the proofs which need the definitions.

   package Tracking
     with Ghost
   is

      function Is_Tracked
        (Coefficients : Bernstein_Chart;
         Delta_Y      : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index;
         Count        : Natural;
         Sum          : Real) return Boolean
      with Pre => Count <= Fixed_Chart_Degree + 1;
      --  At an accurate sample, Sum is within Chart_Error of the first Count terms of the chart coordinate.

      function Is_Close
        (Coefficients : Bernstein_Chart;
         Delta_Y      : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index;
         Value        : Real) return Boolean;
      --  At an accurate sample, Value is within the allowance, and Node_Dust, of the chart coordinate.

      function Squares_Are_Tracked (S, Squares : Real; Count : Natural) return Boolean
      with Pre => Count <= 3;
      --  S, the real value of Square_Sum after Count components, against the exact sum Squares of the squares
      --  of the computed chart coordinates.

      function Allowance_Is_Tracked (A, Allowance, Squares : Real; Count : Natural) return Boolean
      with Pre => Count <= 3;
      --  A, the real value of Square_Allowance after Count components, against the exact sum Allowance of
      --  its terms, which in turn is bounded through Squares.

      function Squares_Are_Close
        (Coefficients : Bernstein_Chart; Sample : Sample_Nodes.Sample; Count : Natural; Squares, Allowance : Real)
         return Boolean
      with Pre => Count <= 3;
      --  At an accurate sample, Squares is within Allowance, and a dust, of the sum of the squares of the first
      --  Count exact chart coordinates.

      procedure Lemma_Nothing_Tracked
        (Coefficients : Bernstein_Chart;
         Delta_Y      : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index;
         Sum          : Dimensionless)
      with Pre => Sum = 0.0, Post => Is_Tracked (Coefficients, Delta_Y, Sample, Component, 0, R (Sum));

      procedure Lemma_Track
        (Coefficients : Bernstein_Chart;
         Delta_Y      : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index;
         K            : Chart_Coefficient_Index;
         S, P, T      : Real)
      with
        Pre  =>
          Is_Chart_Allowance (Coefficients, Delta_Y)
          and then Is_Tracked (Coefficients, Delta_Y, Sample, Component, K, S)
          and then
            abs (P - R (Coefficients (K, Component)) * R (Sample.Basis (K)))
            <= Epsilon * abs (R (Coefficients (K, Component)) * R (Sample.Basis (K))) + Eta
          and then abs (T - (S + P)) <= Epsilon * abs (S + P) + Eta,
        Post => Is_Tracked (Coefficients, Delta_Y, Sample, Component, K + 1, T);
      --  One more term: P is the rounded product and T the rounded sum.

      procedure Lemma_Chart_Component
        (Coefficients : Bernstein_Chart;
         Delta_Y      : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index;
         Value        : Real)
      with
        Pre  =>
          Is_Chart_Allowance (Coefficients, Delta_Y)
          and then Is_Tracked (Coefficients, Delta_Y, Sample, Component, Fixed_Chart_Degree + 1, Value),
        Post =>
          Is_Close (Coefficients, Delta_Y, Sample, Component, Value)
          and then Delta_Y (Component) >= 0.0
          and then Delta_Y (Component) <= Chart_Allowance * Magnitude_Limit
          and then R (Delta_Y (Component)) >= 0.0;
      --  The complete sum is within the allowance of the chart coordinate.

      procedure Lemma_Extend_Close
        (Coefficients : Bernstein_Chart;
         Delta_Y, Y   : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index)
      with
        Pre  =>
          Is_Close (Coefficients, Delta_Y, Sample, Component, R (Y (Component)))
          and then (if Component > 1 then Is_Close (Coefficients, Delta_Y, Sample, 1, R (Y (1))))
          and then (if Component > 2 then Is_Close (Coefficients, Delta_Y, Sample, 2, R (Y (2)))),
        Post =>
          (if Component >= 1 then Is_Close (Coefficients, Delta_Y, Sample, 1, R (Y (1))))
          and then (if Component >= 2 then Is_Close (Coefficients, Delta_Y, Sample, 2, R (Y (2))))
          and then (if Component >= 3 then Is_Close (Coefficients, Delta_Y, Sample, 3, R (Y (3))));
      --  One more chart coordinate is close.

      procedure Lemma_Nothing_Summed
        (Coefficients : Bernstein_Chart; Sample : Sample_Nodes.Sample; Square_Sum, Square_Allowance : Dimensionless)
      with
        Pre  => Square_Sum = 0.0 and then Square_Allowance = 0.0,
        Post =>
          Squares_Are_Tracked (R (Square_Sum), 0.0, 0)
          and then Allowance_Is_Tracked (R (Square_Allowance), 0.0, 0.0, 0)
          and then Squares_Are_Close (Coefficients, Sample, 0, 0.0, 0.0);

      procedure Lemma_Square_Component
        (Component : Chart_Component_Index; Y, Square_Sum : Dimensionless; Squares : Real)
      with
        Pre  =>
          abs Y <= Chart_Limit
          and then Square_Sum >= 0.0
          and then Square_Sum <= Square_Limit
          and then Squares_Are_Tracked (R (Square_Sum), Squares, Component - 1),
        Post =>
          Y * Y >= 0.0
          and then Y * Y <= Square_Limit
          and then Square_Sum + Y * Y >= 0.0
          and then Square_Sum + Y * Y <= Square_Limit
          and then Squares >= 0.0
          and then R (Y) * R (Y) >= 0.0
          and then Squares_Are_Tracked (R (Square_Sum + Y * Y), Squares + R (Y) * R (Y), Component);
      --  One more square added to Square_Sum.

      procedure Lemma_Allowance_Component
        (Component : Chart_Component_Index; Y, D, Square_Allowance : Dimensionless; Allowance, Squares : Real)
      with
        Pre  =>
          abs Y <= Chart_Limit
          and then D >= 0.0
          and then D <= Chart_Allowance * Magnitude_Limit
          and then Square_Allowance >= 0.0
          and then Square_Allowance <= Allowance_Limit
          and then Allowance_Is_Tracked (R (Square_Allowance), Allowance, Squares, Component - 1),
        Post =>
          2.0 * abs Y >= 0.0
          and then 2.0 * abs Y <= Inner_Limit
          and then 2.0 * abs Y + D >= 0.0
          and then 2.0 * abs Y + D <= Inner_Limit
          and then D * (2.0 * abs Y + D) >= 0.0
          and then D * (2.0 * abs Y + D) <= Allowance_Limit
          and then Square_Allowance + D * (2.0 * abs Y + D) >= 0.0
          and then Square_Allowance + D * (2.0 * abs Y + D) <= Allowance_Limit
          and then
            Allowance_Is_Tracked
              (R (Square_Allowance + D * (2.0 * abs Y + D)),
               Allowance + R (D) * (2.0 * abs R (Y) + R (D)),
               Squares + R (Y) * R (Y),
               Component);
      --  One more term added to Square_Allowance.

      procedure Lemma_Extend_Squares_Close
        (Coefficients                 : Bernstein_Chart;
         Delta_Y                      : Component_Values;
         Sample                       : Sample_Nodes.Sample;
         Component                    : Chart_Component_Index;
         Computed, Squares, Allowance : Real)
      with
        Pre  =>
          R (Delta_Y (Component)) >= 0.0
          and then Is_Close (Coefficients, Delta_Y, Sample, Component, Computed)
          and then Squares_Are_Close (Coefficients, Sample, Component - 1, Squares, Allowance),
        Post =>
          Squares_Are_Close
            (Coefficients,
             Sample,
             Component,
             Squares + Computed * Computed,
             Allowance + R (Delta_Y (Component)) * (2.0 * abs Computed + R (Delta_Y (Component))));
      --  One more square, whose error is at most its term of the allowance and a dust.

      procedure Lemma_Chart_Clause
        (Coefficients : Bernstein_Chart; Delta_Y, Y : Component_Values; Sample : Sample_Nodes.Sample)
      with
        Pre  =>
          Is_Close (Coefficients, Delta_Y, Sample, 1, R (Y (1)))
          and then Is_Close (Coefficients, Delta_Y, Sample, 2, R (Y (2)))
          and then Is_Close (Coefficients, Delta_Y, Sample, 3, R (Y (3))),
        Post =>
          (if Sample_Nodes.Is_Accurate (Sample)
           then
             abs (R (Y (1)) - Chart_Value (Coefficients, 1, R (Sample.Node))) <= R (Delta_Y (1)) + Chart_Dust
             and then abs (R (Y (2)) - Chart_Value (Coefficients, 2, R (Sample.Node))) <= R (Delta_Y (2)) + Chart_Dust
             and then abs (R (Y (3)) - Chart_Value (Coefficients, 3, R (Sample.Node))) <= R (Delta_Y (3)) + Chart_Dust
             and then
               (for all Component in Chart_Component_Index =>
                  abs (R (Y (Component)) - Chart_Value (Coefficients, Component, R (Sample.Node)))
                  <= R (Delta_Y (Component)) + Chart_Dust));
      --  The first clause of Is_Node_Accurate, for the chart coordinates themselves.

      procedure Lemma_Node
        (Coefficients                 : Bernstein_Chart;
         Sample                       : Sample_Nodes.Sample;
         Square_Sum, Square_Allowance : Dimensionless;
         Squares, Allowance           : Real)
      with
        Pre  =>
          Square_Sum >= 0.0
          and then Square_Sum <= Square_Limit
          and then Square_Allowance >= 0.0
          and then Square_Allowance <= Allowance_Limit
          and then Squares_Are_Tracked (R (Square_Sum), Squares, 3)
          and then Allowance_Is_Tracked (R (Square_Allowance), Allowance, Squares, 3)
          and then Squares_Are_Close (Coefficients, Sample, 3, Squares, Allowance),
        Post =>
          1.0 + Square_Sum >= 1.0
          and then 1.0 + Square_Sum <= Finish_Limit
          and then abs (1.0 - Square_Sum) <= Finish_Limit
          and then Small_Allowance * (1.0 + Square_Sum) >= 0.0
          and then Small_Allowance * (1.0 + Square_Sum) <= Finish_Limit
          and then Square_Allowance + Small_Allowance * (1.0 + Square_Sum) >= 0.0
          and then Square_Allowance + Small_Allowance * (1.0 + Square_Sum) <= Finish_Limit
          and then
            (if 1.0 + Square_Sum <= Largest_Ideal_Denominator
             then
               abs (1.0 - Square_Sum) <= Largest_Ideal_Denominator
               and then Square_Allowance + Small_Allowance * (1.0 + Square_Sum) <= Largest_Allowance
               and then Squares <= Root_Cap)
          and then
            (if Sample_Nodes.Is_Accurate (Sample)
               and then 1.0 + Square_Sum <= Largest_Ideal_Denominator
               and then Square_Allowance + Small_Allowance * (1.0 + Square_Sum) <= 1.0
             then
               abs (R (1.0 + Square_Sum) - Denominator (Coefficients, R (Sample.Node)))
               <= R (Square_Allowance + Small_Allowance * (1.0 + Square_Sum)) * (1.0 + Slack) + Denominator_Dust
               and then
                 abs (R (1.0 - Square_Sum) - Numerator (Coefficients, 0, R (Sample.Node)))
                 <= R (Square_Allowance + Small_Allowance * (1.0 + Square_Sum)) * (1.0 + Slack) + Denominator_Dust
               and then
                 abs (R (1.0 + Square_Sum) - Denominator (Coefficients, R (Sample.Node)))
                 <= R (Square_Allowance + Small_Allowance * (1.0 + Square_Sum)) * (1.0 + Node_Slack) + Denominator_Dust
               and then
                 abs (R (1.0 - Square_Sum) - Numerator (Coefficients, 0, R (Sample.Node)))
                 <= R (Square_Allowance + Small_Allowance * (1.0 + Square_Sum))
                    * (1.0 + Node_Slack)
                    + Denominator_Dust);
      --  The operations after the loop, and what At_Node promises of their results: no overflow, the signs,
      --  the limits where the denominator is within its own, and the other clauses of Is_Node_Accurate and of
      --  Is_Node_Tight, each stated for the very expressions which At_Node returns.

   end Tracking;

   package body Tracking is

      function Is_Tracked
        (Coefficients : Bernstein_Chart;
         Delta_Y      : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index;
         Count        : Natural;
         Sum          : Real) return Boolean
      is (if Sample_Nodes.Is_Accurate (Sample)
          then
            abs (Sum - Chart_Sum (Coefficients, Component, Count, R (Sample.Node)))
            <= Chart_Error (Delta_Y (Component), Count, R (Sample.Node)));

      function Is_Close
        (Coefficients : Bernstein_Chart;
         Delta_Y      : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index;
         Value        : Real) return Boolean
      is (if Sample_Nodes.Is_Accurate (Sample)
          then
            abs (Value - Chart_Value (Coefficients, Component, R (Sample.Node)))
            <= R (Delta_Y (Component)) + Node_Dust);

      function Squares_Are_Tracked (S, Squares : Real; Count : Natural) return Boolean
      is (Squares >= 0.0
          and then Squares <= Tally (Count) * Square_Cap
          and then abs (S - Squares) <= Square_Factor (Count) * Squares + Tally (Count) * Three_Eta);

      function Allowance_Is_Tracked (A, Allowance, Squares : Real; Count : Natural) return Boolean
      is (Allowance >= 0.0
          and then Allowance <= Tally (Count) * Allowance_Cap
          and then Allowance <= Delta_Cap * (Tally (Count) * (1.0 + Delta_Cap) + Squares)
          and then abs (A - Allowance) <= Allowance_Factor (Count) * Allowance + Tally (Count) * Seven_Eta);

      function Squares_Are_Close
        (Coefficients : Bernstein_Chart; Sample : Sample_Nodes.Sample; Count : Natural; Squares, Allowance : Real)
         return Boolean
      is (if Sample_Nodes.Is_Accurate (Sample)
          then
            abs (Squares - True_Squares (Coefficients, R (Sample.Node), Count))
            <= Allowance + Node_Dust * (Tally (Count) * Weight_Unit + Weight_Scale * Squares + Allowance));

      procedure Lemma_Nothing_Tracked
        (Coefficients : Bernstein_Chart;
         Delta_Y      : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index;
         Sum          : Dimensionless) is
      begin
         pragma Assert (R (Sum) = 0.0);
         pragma Assert (Chart_Sum (Coefficients, Component, 0, R (Sample.Node)) = 0.0);
         pragma Assert (Basis_Sum (0, R (Sample.Node)) = 0.0);
         pragma Assert (Tally (0) = 0.0);
         pragma Assert (Chart_Error (Delta_Y (Component), 0, R (Sample.Node)) = 0.0);
      end Lemma_Nothing_Tracked;

      procedure Lemma_Track
        (Coefficients : Bernstein_Chart;
         Delta_Y      : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index;
         K            : Chart_Coefficient_Index;
         S, P, T      : Real) is
      begin
         if Sample_Nodes.Is_Accurate (Sample) then
            Lemma_Accurate_Step (Coefficients, Delta_Y, Sample, Component, K, S, P, T);
         end if;
      end Lemma_Track;

      procedure Lemma_Chart_Component
        (Coefficients : Bernstein_Chart;
         Delta_Y      : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index;
         Value        : Real) is
      begin
         pragma Assert (Delta_Y (Component) >= 0.0);
         pragma Assert (Delta_Y (Component) <= Chart_Allowance * Magnitude_Limit);
         pragma Assert (R (Delta_Y (Component)) >= 0.0);
         if Sample_Nodes.Is_Accurate (Sample) then
            Lemma_Chart_Is_Accurate (Coefficients, Delta_Y, Sample, Component, Value);
         end if;
      end Lemma_Chart_Component;

      procedure Lemma_Extend_Close
        (Coefficients : Bernstein_Chart;
         Delta_Y, Y   : Component_Values;
         Sample       : Sample_Nodes.Sample;
         Component    : Chart_Component_Index) is
      begin
         case Component is
            when 1 =>
               pragma Assert (Is_Close (Coefficients, Delta_Y, Sample, 1, R (Y (1))));

            when 2 =>
               pragma Assert (Is_Close (Coefficients, Delta_Y, Sample, 2, R (Y (2))));

            when 3 =>
               pragma Assert (Is_Close (Coefficients, Delta_Y, Sample, 3, R (Y (3))));
         end case;
      end Lemma_Extend_Close;

      procedure Lemma_Nothing_Summed
        (Coefficients : Bernstein_Chart; Sample : Sample_Nodes.Sample; Square_Sum, Square_Allowance : Dimensionless) is
      begin
         pragma Assert (R (Square_Sum) = 0.0);
         pragma Assert (R (Square_Allowance) = 0.0);
         pragma Assert (Tally (0) = 0.0);
         pragma Assert (True_Squares (Coefficients, R (Sample.Node), 0) = 0.0);
      end Lemma_Nothing_Summed;

      procedure Lemma_Square_Component
        (Component : Chart_Component_Index; Y, Square_Sum : Dimensionless; Squares : Real) is
      begin
         Lemma_Small_Tally (Component - 1);
         Lemma_Square_Step
           (Sum         => Square_Sum,
            Y           => Y,
            Exact       => Squares,
            Factor      => Square_Factor (Component - 1),
            Next_Factor => Square_Factor (Component),
            Dust        => Tally (Component - 1) * Three_Eta);
      end Lemma_Square_Component;

      procedure Lemma_Allowance_Component
        (Component : Chart_Component_Index; Y, D, Square_Allowance : Dimensionless; Allowance, Squares : Real) is
      begin
         Lemma_Small_Tally (Component - 1);
         pragma Assert (R (D) >= 0.0 and then R (D) <= Delta_Cap);
         Lemma_Allowance_Step
           (Sum         => Square_Allowance,
            D           => D,
            Y           => Y,
            Exact       => Allowance,
            Factor      => Allowance_Factor (Component - 1),
            Next_Factor => Allowance_Factor (Component),
            Dust        => Tally (Component - 1) * Seven_Eta);
         Lemma_Allowance_Is_Bounded (R (D), R (Y));
      end Lemma_Allowance_Component;

      procedure Lemma_Extend_Squares_Close
        (Coefficients                 : Bernstein_Chart;
         Delta_Y                      : Component_Values;
         Sample                       : Sample_Nodes.Sample;
         Component                    : Chart_Component_Index;
         Computed, Squares, Allowance : Real) is
      begin
         Lemma_Component_Error
           (Coefficients => Coefficients,
            Sample       => Sample,
            Component    => Component,
            Computed     => Computed,
            D            => R (Delta_Y (Component)),
            Squares      => Squares,
            Allowance    => Allowance);
      end Lemma_Extend_Squares_Close;

      procedure Lemma_Chart_Clause
        (Coefficients : Bernstein_Chart; Delta_Y, Y : Component_Values; Sample : Sample_Nodes.Sample)
      is
         X : constant Real := R (Sample.Node);
      begin
         if Sample_Nodes.Is_Accurate (Sample) then
            pragma Assert (abs (R (Y (1)) - Chart_Value (Coefficients, 1, X)) <= R (Delta_Y (1)) + Chart_Dust);
            pragma Assert (abs (R (Y (2)) - Chart_Value (Coefficients, 2, X)) <= R (Delta_Y (2)) + Chart_Dust);
            pragma Assert (abs (R (Y (3)) - Chart_Value (Coefficients, 3, X)) <= R (Delta_Y (3)) + Chart_Dust);
            pragma
              Assert
                (for all Component in Chart_Component_Index =>
                   abs (R (Y (Component)) - Chart_Value (Coefficients, Component, X))
                   <= R (Delta_Y (Component)) + Chart_Dust);
         end if;
      end Lemma_Chart_Clause;

      procedure Lemma_Node
        (Coefficients                 : Bernstein_Chart;
         Sample                       : Sample_Nodes.Sample;
         Square_Sum, Square_Allowance : Dimensionless;
         Squares, Allowance           : Real) is
      begin
         pragma Assert (Tally (3) = 3.0);
         pragma Assert (abs (R (Square_Sum) - Squares) <= Four_Steps * Squares + Nine_Eta);
         pragma Assert (abs (R (Square_Allowance) - Allowance) <= Seven_Steps * Allowance + Twenty_One_Eta);
         pragma Assert (Allowance <= Delta_Cap * (3.0 * (1.0 + Delta_Cap) + Squares));
         Lemma_Finish (Square_Sum, Square_Allowance);
         Lemma_Finish_Bounds
           (S         => R (Square_Sum),
            A         => R (Square_Allowance),
            Q         => R (1.0 + Square_Sum),
            L         => R (1.0 - Square_Sum),
            W         => R (Small_Allowance * (1.0 + Square_Sum)),
            Total     => R (Square_Allowance + Small_Allowance * (1.0 + Square_Sum)),
            Squares   => Squares,
            Allowance => Allowance);
         Lemma_Allowance_Fits (Square_Allowance + Small_Allowance * (1.0 + Square_Sum));
         Lemma_Finish_Accuracy
           (Coefficients => Coefficients,
            Sample       => Sample,
            S            => R (Square_Sum),
            A            => R (Square_Allowance),
            Q            => R (1.0 + Square_Sum),
            L            => R (1.0 - Square_Sum),
            W            => R (Small_Allowance * (1.0 + Square_Sum)),
            Total        => R (Square_Allowance + Small_Allowance * (1.0 + Square_Sum)),
            Squares      => Squares,
            Allowance    => Allowance);
         Lemma_At_Most_One (Square_Allowance + Small_Allowance * (1.0 + Square_Sum));
      end Lemma_Node;

   end Tracking;

   procedure Lemma_Chart_Iteration
     (Coefficients : Bernstein_Chart;
      Delta_Y      : Component_Values;
      Sample       : Sample_Nodes.Sample;
      Component    : Chart_Component_Index;
      K            : Chart_Coefficient_Index;
      Sum          : Dimensionless)
   with
     Ghost,
     Pre  =>
       Is_Chart_Bounded (Coefficients)
       and then Is_Chart_Allowance (Coefficients, Delta_Y)
       and then Sample_Nodes.Is_In_Range (Sample)
       and then abs Sum <= Chart_Limit
       and then abs R (Sum) <= Tally (K) * Step_Limit
       and then Tracking.Is_Tracked (Coefficients, Delta_Y, Sample, Component, K, R (Sum)),
     Post =>
       abs (Coefficients (K, Component) * Sample.Basis (K)) <= Product_Limit
       and then abs (Sum + Coefficients (K, Component) * Sample.Basis (K)) <= Chart_Limit
       and then abs R (Sum + Coefficients (K, Component) * Sample.Basis (K)) <= Tally (K + 1) * Step_Limit
       and then
         Tracking.Is_Tracked
           (Coefficients, Delta_Y, Sample, Component, K + 1, R (Sum + Coefficients (K, Component) * Sample.Basis (K)));
   --  What one iteration of the chart loop preserves: the magnitude limit of the running sum and, at an
   --  accurate sample, its error bound.

   procedure Lemma_Chart_Iteration
     (Coefficients : Bernstein_Chart;
      Delta_Y      : Component_Values;
      Sample       : Sample_Nodes.Sample;
      Component    : Chart_Component_Index;
      K            : Chart_Coefficient_Index;
      Sum          : Dimensionless)
   is
      B    : constant Dimensionless := Coefficients (K, Component);
      Beta : constant Dimensionless := Sample.Basis (K);
   begin
      pragma Assert (abs B <= Magnitude_Limit);
      pragma Assert (Beta >= 0.0 and then Beta <= Sample_Nodes.Largest_Basis_Value);
      Lemma_Tally_Step (K);
      Lemma_Magnitude_Step (Sum, B, Beta, Tally (K));
      Tracking.Lemma_Track (Coefficients, Delta_Y, Sample, Component, K, R (Sum), R (B * Beta), R (Sum + B * Beta));
   end Lemma_Chart_Iteration;

   ---------------------------------------------------------------------------
   --  The node
   ---------------------------------------------------------------------------

   function At_Node
     (Coefficients : Bernstein_Chart; Delta_Y : Component_Values; Sample : Sample_Nodes.Sample) return Ideal_Node
   is
      Y                : Component_Values := [others => 0.0];
      Square_Sum       : Dimensionless := 0.0;
      Square_Allowance : Dimensionless := 0.0;
      Ideal_Q          : Dimensionless;

      Exact_Squares : Real := 0.0
      with Ghost;
      --  Σ Ŷ꜀² over the components done, exactly.

      Exact_Allowance : Real := 0.0
      with Ghost;
      --  Σ δ꜀·(2·|Ŷ꜀| + δ꜀) over the components done, exactly.
   begin
      Tracking.Lemma_Nothing_Summed (Coefficients, Sample, Square_Sum, Square_Allowance);
      for Component in Chart_Component_Index loop
         pragma Loop_Invariant (Square_Sum >= 0.0 and then Square_Sum <= Square_Limit);
         pragma Loop_Invariant (Square_Allowance >= 0.0 and then Square_Allowance <= Allowance_Limit);
         pragma Loop_Invariant (if Component <= 1 then Y (1) = 0.0);
         pragma Loop_Invariant (if Component <= 2 then Y (2) = 0.0);
         pragma Loop_Invariant (Y (3) = 0.0);
         pragma Loop_Invariant (if Component > 1 then R (Y (1)) * R (Y (1)) <= Exact_Squares);
         pragma Loop_Invariant (if Component > 2 then R (Y (2)) * R (Y (2)) <= Exact_Squares);
         pragma Loop_Invariant (if Component > 1 then Tracking.Is_Close (Coefficients, Delta_Y, Sample, 1, R (Y (1))));
         pragma Loop_Invariant (if Component > 2 then Tracking.Is_Close (Coefficients, Delta_Y, Sample, 2, R (Y (2))));
         pragma Loop_Invariant (Tracking.Squares_Are_Tracked (R (Square_Sum), Exact_Squares, Component - 1));
         pragma
           Loop_Invariant
             (Tracking.Allowance_Is_Tracked (R (Square_Allowance), Exact_Allowance, Exact_Squares, Component - 1));
         pragma
           Loop_Invariant
             (Tracking.Squares_Are_Close (Coefficients, Sample, Component - 1, Exact_Squares, Exact_Allowance));

         Lemma_Current_Is_Zero (Y, Component);
         Tracking.Lemma_Nothing_Tracked (Coefficients, Delta_Y, Sample, Component, Y (Component));
         for K in Chart_Coefficient_Index loop
            pragma Loop_Invariant (if Component < 2 then Y (2) = 0.0);
            pragma Loop_Invariant (if Component < 3 then Y (3) = 0.0);
            pragma Loop_Invariant (if Component > 1 then R (Y (1)) * R (Y (1)) <= Exact_Squares);
            pragma Loop_Invariant (if Component > 2 then R (Y (2)) * R (Y (2)) <= Exact_Squares);
            pragma
              Loop_Invariant (if Component > 1 then Tracking.Is_Close (Coefficients, Delta_Y, Sample, 1, R (Y (1))));
            pragma
              Loop_Invariant (if Component > 2 then Tracking.Is_Close (Coefficients, Delta_Y, Sample, 2, R (Y (2))));
            pragma Loop_Invariant (abs Y (Component) <= Chart_Limit);
            pragma Loop_Invariant (abs R (Y (Component)) <= Tally (K) * Step_Limit);
            pragma
              Loop_Invariant (Tracking.Is_Tracked (Coefficients, Delta_Y, Sample, Component, K, R (Y (Component))));
            Lemma_Chart_Iteration (Coefficients, Delta_Y, Sample, Component, K, Y (Component));
            Y (Component) := Y (Component) + Coefficients (K, Component) * Sample.Basis (K);
         end loop;

         Tracking.Lemma_Chart_Component (Coefficients, Delta_Y, Sample, Component, R (Y (Component)));
         Tracking.Lemma_Square_Component (Component, Y (Component), Square_Sum, Exact_Squares);
         Tracking.Lemma_Allowance_Component
           (Component        => Component,
            Y                => Y (Component),
            D                => Delta_Y (Component),
            Square_Allowance => Square_Allowance,
            Allowance        => Exact_Allowance,
            Squares          => Exact_Squares);
         Tracking.Lemma_Extend_Squares_Close
           (Coefficients => Coefficients,
            Delta_Y      => Delta_Y,
            Sample       => Sample,
            Component    => Component,
            Computed     => R (Y (Component)),
            Squares      => Exact_Squares,
            Allowance    => Exact_Allowance);
         Tracking.Lemma_Extend_Close (Coefficients, Delta_Y, Y, Sample, Component);
         Lemma_Extend_Squares (Y, Component, Exact_Squares, Exact_Squares + R (Y (Component)) * R (Y (Component)));
         Exact_Allowance :=
           Exact_Allowance + R (Delta_Y (Component)) * (2.0 * abs R (Y (Component)) + R (Delta_Y (Component)));
         Exact_Squares := Exact_Squares + R (Y (Component)) * R (Y (Component));

         Square_Sum := Square_Sum + Y (Component) * Y (Component);
         Square_Allowance := Square_Allowance + Delta_Y (Component) * (2.0 * abs Y (Component) + Delta_Y (Component));

         pragma Assert (Tracking.Squares_Are_Tracked (R (Square_Sum), Exact_Squares, Component));
         pragma
           Assert (Tracking.Allowance_Is_Tracked (R (Square_Allowance), Exact_Allowance, Exact_Squares, Component));
         pragma Assert (Tracking.Squares_Are_Close (Coefficients, Sample, Component, Exact_Squares, Exact_Allowance));
      end loop;

      Tracking.Lemma_Node (Coefficients, Sample, Square_Sum, Square_Allowance, Exact_Squares, Exact_Allowance);
      Ideal_Q := 1.0 + Square_Sum;
      Lemma_Chart_Values_Are_Bounded (Y, Exact_Squares);
      Tracking.Lemma_Chart_Clause (Coefficients, Delta_Y, Y, Sample);
      return
        (Y               => Y,
         Ideal_Q         => Ideal_Q,
         Local_0         => 1.0 - Square_Sum,
         Ideal_Allowance => Square_Allowance + Small_Allowance * Ideal_Q);
   end At_Node;

   ---------------------------------------------------------------------------
   --  One axis: the terms of the numerator and of its allowances
   ---------------------------------------------------------------------------

   Frame_Cap : constant := 2.0**200;
   --  More than Magnitude_Limit.

   Allowance_Value_Cap : constant := 2.0**333;
   --  More than Largest_Allowance.

   First_Term_Cap : constant := 2.0**300;
   Later_Term_Cap : constant := 2.0**252;
   Size_Cap       : constant := 2.0**301;
   --  More than the first term F₀·N̂₀ of the numerator, than a later term F꜀·2·Ŷ꜀, and than all four magnitudes.

   First_Slip_Cap : constant := 2.0**533;
   Later_Slip_Cap : constant := 2.0**355;
   Slip_Cap       : constant := 2.0**534;
   --  The same for the terms |F₀|·allowance and |F꜀|·2·δ꜀ of the slip.

   Numerator_Limit_Real : constant := 2.0**302;
   Numerator_Limit      : constant Dimensionless := 2.0**302
   with Ghost;

   Slip_Limit_Real : constant := 2.0**535;
   Slip_Limit      : constant Dimensionless := 2.0**535
   with Ghost;

   Quotient_Limit_Real : constant := 2.0**303;
   Quotient_Limit      : constant Dimensionless := 2.0**303
   with Ghost;

   Doubled_Cap   : constant := 2.0**53;
   Doubled_Limit : constant Dimensionless := 2.0**53
   with Ghost;
   --  More than twice Largest_Chart_Value.

   Doubled_Delta_Cap   : constant := 2.0**156;
   Doubled_Delta_Limit : constant Dimensionless := 2.0**156
   with Ghost;
   --  More than twice the largest chart allowance.

   Axis_Dust : constant := 2.0**(-870);
   --  More than the underflow of a doubling, 2⁻¹⁰⁷⁵, times a frame component, and a few more underflows.

   Frame_Dust : constant := 2.0**200 * 2.0**(-1075);

   Seven_Dusts : constant := 7.0 * 2.0**(-870);

   Quotient_Factor : constant := 1.01 * 2.0**(-53);
   Quotient_Dust   : constant := 1.01 * 2.0**(-1075);
   Quotient_Slack  : constant := 0.01 * 2.0**(-1075);
   --  The quotient is within Quotient_Factor of itself, and Quotient_Dust, of the exact quotient.

   procedure Lemma_Within_Numerator_Limit (Value : Dimensionless)
   with Ghost, Pre => abs R (Value) <= Numerator_Limit_Real, Post => abs Value <= Numerator_Limit;

   procedure Lemma_Within_Numerator_Limit (Value : Dimensionless) is null;

   procedure Lemma_Within_Slip_Limit (Value : Dimensionless)
   with Ghost, Pre => abs R (Value) <= Slip_Limit_Real, Post => abs Value <= Slip_Limit;

   procedure Lemma_Within_Slip_Limit (Value : Dimensionless) is null;

   procedure Lemma_Within_Quotient_Limit (Value : Dimensionless)
   with Ghost, Pre => abs R (Value) <= Quotient_Limit_Real, Post => abs Value <= Quotient_Limit;

   procedure Lemma_Within_Quotient_Limit (Value : Dimensionless) is null;

   function Numerator_Term
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Node : Ideal_Node; Component : Frame_Component_Index) return Real
   is (if Component = 0
       then R (Frame (0) (Axis)) * R (Node.Local_0)
       else R (Frame (Component) (Axis)) * (2.0 * R (Node.Y (Component))))
   with Ghost;
   --  One term of the numerator as At_Axis forms it, exactly: F₀·N̂₀, then F꜀·2·Ŷ꜀.

   function Slip_Term
     (Frame     : Frame_Vector_Array;
      Axis      : Axis_Name;
      Delta_Y   : Component_Values;
      Node      : Ideal_Node;
      Component : Frame_Component_Index) return Real
   is (if Component = 0
       then abs R (Frame (0) (Axis)) * R (Node.Ideal_Allowance)
       else abs R (Frame (Component) (Axis)) * (2.0 * R (Delta_Y (Component))))
   with Ghost;
   --  One term of the slip, exactly: |F₀| times the node's allowance, then |F꜀|·2·δ꜀.

   function Partial_Numerator
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Node : Ideal_Node; Count : Natural) return Real
   is (case Count is
         when 0      => Numerator_Term (Frame, Axis, Node, 0),
         when 1      => Numerator_Term (Frame, Axis, Node, 0) + Numerator_Term (Frame, Axis, Node, 1),
         when 2      =>
           Numerator_Term (Frame, Axis, Node, 0)
           + Numerator_Term (Frame, Axis, Node, 1)
           + Numerator_Term (Frame, Axis, Node, 2),
         when others =>
           Numerator_Term (Frame, Axis, Node, 0)
           + Numerator_Term (Frame, Axis, Node, 1)
           + Numerator_Term (Frame, Axis, Node, 2)
           + Numerator_Term (Frame, Axis, Node, 3))
   with Ghost, Pre => Count <= 3;
   --  The exact sum of the first term and Count more.

   function Partial_Size (Frame : Frame_Vector_Array; Axis : Axis_Name; Node : Ideal_Node; Count : Natural) return Real
   is (case Count is
         when 0      => abs Numerator_Term (Frame, Axis, Node, 0),
         when 1      => abs Numerator_Term (Frame, Axis, Node, 0) + abs Numerator_Term (Frame, Axis, Node, 1),
         when 2      =>
           abs Numerator_Term (Frame, Axis, Node, 0)
           + abs Numerator_Term (Frame, Axis, Node, 1)
           + abs Numerator_Term (Frame, Axis, Node, 2),
         when others =>
           abs Numerator_Term (Frame, Axis, Node, 0)
           + abs Numerator_Term (Frame, Axis, Node, 1)
           + abs Numerator_Term (Frame, Axis, Node, 2)
           + abs Numerator_Term (Frame, Axis, Node, 3))
   with Ghost, Pre => Count <= 3;
   --  The exact sum of their magnitudes.

   function Partial_Slip
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node; Count : Natural)
      return Real
   is (case Count is
         when 0      => Slip_Term (Frame, Axis, Delta_Y, Node, 0),
         when 1      => Slip_Term (Frame, Axis, Delta_Y, Node, 0) + Slip_Term (Frame, Axis, Delta_Y, Node, 1),
         when 2      =>
           Slip_Term (Frame, Axis, Delta_Y, Node, 0)
           + Slip_Term (Frame, Axis, Delta_Y, Node, 1)
           + Slip_Term (Frame, Axis, Delta_Y, Node, 2),
         when others =>
           Slip_Term (Frame, Axis, Delta_Y, Node, 0)
           + Slip_Term (Frame, Axis, Delta_Y, Node, 1)
           + Slip_Term (Frame, Axis, Delta_Y, Node, 2)
           + Slip_Term (Frame, Axis, Delta_Y, Node, 3))
   with Ghost, Pre => Count <= 3;
   --  The exact sum of the first term of the slip and Count more.

   procedure Lemma_Partial_Step
     (Frame     : Frame_Vector_Array;
      Axis      : Axis_Name;
      Delta_Y   : Component_Values;
      Node      : Ideal_Node;
      Component : Chart_Component_Index)
   with
     Ghost,
     Post =>
       Partial_Numerator (Frame, Axis, Node, Component)
       = Partial_Numerator (Frame, Axis, Node, Component - 1) + Numerator_Term (Frame, Axis, Node, Component)
       and then
         Partial_Size (Frame, Axis, Node, Component)
         = Partial_Size (Frame, Axis, Node, Component - 1) + abs Numerator_Term (Frame, Axis, Node, Component)
       and then
         Partial_Slip (Frame, Axis, Delta_Y, Node, Component)
         = Partial_Slip (Frame, Axis, Delta_Y, Node, Component - 1) + Slip_Term (Frame, Axis, Delta_Y, Node, Component)
       and then
         abs Partial_Numerator (Frame, Axis, Node, Component - 1) <= Partial_Size (Frame, Axis, Node, Component - 1)
       and then Partial_Size (Frame, Axis, Node, Component - 1) >= 0.0;
   --  One more term extends each sum, and a sum is no larger than the sum of the magnitudes of its terms.

   procedure Lemma_Partial_Step
     (Frame     : Frame_Vector_Array;
      Axis      : Axis_Name;
      Delta_Y   : Component_Values;
      Node      : Ideal_Node;
      Component : Chart_Component_Index)
   is null;

   function Axis_Factor (Count : Natural) return Real
   is ((Tally (Count) + 2.0) * Step)
   with Ghost, Pre => Count <= 3;
   --  The relative error of the three running sums after Count additions. A term takes two rounded operations.

   function Axis_Dust_At (Count : Natural) return Real
   is ((2.0 * Tally (Count) + 1.0) * Axis_Dust)
   with Ghost, Pre => Count <= 3;
   --  Their dust.

   procedure Lemma_First_Terms (F, L, A : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs F <= Magnitude_Limit
       and then abs L <= Largest_Ideal_Denominator
       and then A >= 0.0
       and then A <= Largest_Allowance,
     Post =>
       abs (F * L) <= Numerator_Limit
       and then abs F * A >= 0.0
       and then abs F * A <= Slip_Limit
       and then abs (R (F) * R (L)) <= First_Term_Cap
       and then abs R (F) * R (A) >= 0.0
       and then abs R (F) * R (A) <= First_Slip_Cap
       and then abs (R (F * L) - R (F) * R (L)) <= Two_Steps * abs (R (F) * R (L)) + Axis_Dust
       and then abs (R (abs (F * L)) - abs (R (F) * R (L))) <= Two_Steps * abs (R (F) * R (L)) + Axis_Dust
       and then abs (R (abs F * A) - abs R (F) * R (A)) <= Two_Steps * (abs R (F) * R (A)) + Axis_Dust;
   --  The two products before the loop: neither overflows, and each is rounded once.

   procedure Lemma_First_Terms (F, L, A : Dimensionless) is
   begin
      pragma Assert (abs R (F) <= Frame_Cap);
      pragma Assert (abs R (L) <= Denominator_Cap);
      pragma Assert (R (A) >= 0.0 and then R (A) <= Allowance_Value_Cap);
      Lemma_Product_Bound (R (F), R (L), Frame_Cap, Denominator_Cap);
      Float_Lemmas.Lemma_Rounding_Error_Mul (F, L);
      pragma Assert (abs R (F * L) <= Numerator_Limit_Real);
      Lemma_Within_Numerator_Limit (F * L);
      Lemma_Abs_Is_Exact (F * L);
      Lemma_Abs_Is_Exact (F);
      Lemma_Product_Bound (R (abs F), R (A), Frame_Cap, Allowance_Value_Cap);
      Lemma_Sign (abs R (F), R (A));
      Lemma_Product_Sign (abs F, A);
      Float_Lemmas.Lemma_Rounding_Error_Mul (abs F, A);
      pragma Assert (abs R (abs F * A) <= Slip_Limit_Real);
      Lemma_Within_Slip_Limit (abs F * A);
   end Lemma_First_Terms;

   procedure Lemma_Doubled_Product (F, V, Doubled, Product : Real)
   with
     Ghost,
     Pre  =>
       abs F <= Frame_Cap
       and then abs (Doubled - 2.0 * V) <= Epsilon * abs (2.0 * V) + Eta
       and then abs (Product - F * Doubled) <= Epsilon * abs (F * Doubled) + Eta,
     Post => abs (Product - F * (2.0 * V)) <= Two_Steps * abs (F * (2.0 * V)) + Axis_Dust;
   --  A doubling and a multiplication, both rounded: within a little more than 2u of F·2·V, and a dust which is
   --  the doubling's underflow times |F|.

   procedure Lemma_Doubled_Product (F, V, Doubled, Product : Real) is
      Size : constant Real := abs (F * (2.0 * V));
   begin
      pragma Assert (Size = abs F * abs (2.0 * V));
      Lemma_Product_Bound (F, Doubled - 2.0 * V, abs F, Epsilon * abs (2.0 * V) + Eta);
      pragma Assert (F * Doubled - F * (2.0 * V) = F * (Doubled - 2.0 * V));
      Lemma_Distribute (abs F, Epsilon * abs (2.0 * V), Eta);
      pragma Assert (abs F * (Epsilon * abs (2.0 * V)) = Epsilon * Size);
      pragma Assert (abs F * Eta <= Frame_Dust);
      pragma Assert (abs (F * Doubled - F * (2.0 * V)) <= Epsilon * Size + Frame_Dust);
      pragma Assert (abs (F * Doubled) <= Size + Epsilon * Size + Frame_Dust);
      pragma
        Assert
          (abs (Product - F * (2.0 * V))
           <= Epsilon * (Size + Epsilon * Size + Frame_Dust) + Eta + Epsilon * Size + Frame_Dust);
   end Lemma_Doubled_Product;

   procedure Lemma_Later_Terms (F, Y, D : Dimensionless)
   with
     Ghost,
     Pre  =>
       abs F <= Magnitude_Limit
       and then abs Y <= Largest_Chart_Value
       and then D >= 0.0
       and then D <= Chart_Allowance * Magnitude_Limit,
     Post =>
       abs (2.0 * Y) <= Doubled_Limit
       and then abs (F * (2.0 * Y)) <= Numerator_Limit
       and then 2.0 * D >= 0.0
       and then 2.0 * D <= Doubled_Delta_Limit
       and then abs F * (2.0 * D) >= 0.0
       and then abs F * (2.0 * D) <= Slip_Limit
       and then abs (R (F) * (2.0 * R (Y))) <= Later_Term_Cap
       and then abs R (F) * (2.0 * R (D)) >= 0.0
       and then abs R (F) * (2.0 * R (D)) <= Later_Slip_Cap
       and then abs (R (F * (2.0 * Y)) - R (F) * (2.0 * R (Y))) <= Two_Steps * abs (R (F) * (2.0 * R (Y))) + Axis_Dust
       and then
         abs (R (abs (F * (2.0 * Y))) - abs (R (F) * (2.0 * R (Y))))
         <= Two_Steps * abs (R (F) * (2.0 * R (Y))) + Axis_Dust
       and then
         abs (R (abs F * (2.0 * D)) - abs R (F) * (2.0 * R (D)))
         <= Two_Steps * (abs R (F) * (2.0 * R (D))) + Axis_Dust;
   --  The products of one component inside the loop: none overflows, and each is a rounded doubling followed
   --  by a rounded multiplication.

   procedure Lemma_Later_Terms (F, Y, D : Dimensionless) is
   begin
      pragma Assert (abs R (F) <= Frame_Cap);
      pragma Assert (abs R (Y) <= 2.0E15);
      pragma Assert (R (D) >= 0.0 and then R (D) <= Delta_Cap);

      Float_Lemmas.Lemma_Rounding_Error_Mul (2.0, Y);
      pragma Assert (abs (R (2.0 * Y) - 2.0 * R (Y)) <= Epsilon * abs (2.0 * R (Y)) + Eta);
      pragma Assert (abs R (2.0 * Y) <= Doubled_Cap);
      pragma Assert (abs (2.0 * Y) <= Doubled_Limit);
      Float_Lemmas.Lemma_Rounding_Error_Mul (F, 2.0 * Y);
      Lemma_Product_Bound (R (F), 2.0 * R (Y), Frame_Cap, 4.0E15);
      Lemma_Doubled_Product (R (F), R (Y), R (2.0 * Y), R (F * (2.0 * Y)));
      pragma Assert (abs (R (F) * (2.0 * R (Y))) <= Later_Term_Cap);
      pragma Assert (abs R (F * (2.0 * Y)) <= Numerator_Limit_Real);
      Lemma_Within_Numerator_Limit (F * (2.0 * Y));
      Lemma_Abs_Is_Exact (F * (2.0 * Y));

      Lemma_Abs_Is_Exact (F);
      Lemma_Product_Sign (2.0, D);
      Float_Lemmas.Lemma_Rounding_Error_Mul (2.0, D);
      pragma Assert (abs (R (2.0 * D) - 2.0 * R (D)) <= Epsilon * abs (2.0 * R (D)) + Eta);
      pragma Assert (R (2.0 * D) <= Doubled_Delta_Cap);
      pragma Assert (2.0 * D <= Doubled_Delta_Limit);
      Lemma_Product_Sign (abs F, 2.0 * D);
      Float_Lemmas.Lemma_Rounding_Error_Mul (abs F, 2.0 * D);
      Lemma_Product_Bound (R (abs F), 2.0 * R (D), Frame_Cap, 2.0 * Delta_Cap);
      Lemma_Sign (abs R (F), 2.0 * R (D));
      Lemma_Doubled_Product (abs R (F), R (D), R (2.0 * D), R (abs F * (2.0 * D)));
      pragma Assert (abs R (F) * (2.0 * R (D)) <= Later_Slip_Cap);
      pragma Assert (abs R (abs F * (2.0 * D)) <= Slip_Limit_Real);
      Lemma_Within_Slip_Limit (abs F * (2.0 * D));
   end Lemma_Later_Terms;

   procedure Lemma_Axis_Sum
     (Sum, Term                             : Dimensionless;
      Exact, Size, Exact_Term, Factor, Dust : Real;
      Next_Exact, Next_Size, Next_Factor    : Real;
      Next_Dust, Cap                        : Real)
   with
     Ghost,
     Pre  =>
       abs Sum <= Slip_Limit
       and then abs Term <= Slip_Limit
       and then Size >= 0.0
       and then abs Exact <= Size
       and then Factor >= Two_Steps
       and then Factor <= Factor_Limit
       and then Next_Factor >= Factor + Step
       and then Next_Factor <= Factor_Limit
       and then Dust >= 0.0
       and then Dust <= Seven_Dusts
       and then abs (R (Sum) - Exact) <= Factor * Size + Dust
       and then abs (R (Term) - Exact_Term) <= Two_Steps * abs Exact_Term + Axis_Dust
       and then Next_Exact = Exact + Exact_Term
       and then Next_Size = Size + abs Exact_Term
       and then Next_Dust >= Dust + Axis_Dust + Axis_Dust
       and then Cap >= 1.0
       and then Next_Size <= Cap,
     Post =>
       abs (R (Sum + Term) - Next_Exact) <= Next_Factor * Next_Size + Next_Dust
       and then abs R (Sum + Term) <= 2.0 * Cap;
   --  One rounded addition to one of the three running sums of At_Axis: from the error bound of the sum so
   --  far, against Exact, to that of the longer sum, against Next_Exact.

   procedure Lemma_Axis_Sum
     (Sum, Term                             : Dimensionless;
      Exact, Size, Exact_Term, Factor, Dust : Real;
      Next_Exact, Next_Size, Next_Factor    : Real;
      Next_Dust, Cap                        : Real) is
   begin
      Float_Lemmas.Lemma_Rounding_Error_Add (Sum, Term);
      Lemma_Scale (Two_Steps, Factor, abs Exact_Term);
      Lemma_Accumulate
        (S           => R (Sum),
         P           => R (Term),
         T           => R (Sum + Term),
         Exact       => Exact,
         Size        => Size,
         Term        => Exact_Term,
         Factor      => Factor,
         Next_Factor => Next_Factor,
         Dust        => Dust,
         Term_Dust   => Axis_Dust,
         Unit        => Axis_Dust);
      Lemma_Scale (Next_Factor, Factor_Limit, Size + abs Exact_Term);
      pragma Assert (abs (Exact + Exact_Term) <= Size + abs Exact_Term);
   end Lemma_Axis_Sum;

   procedure Lemma_Quotient_Error (N, Q, I : Real)
   with
     Ghost,
     Pre  => Q >= 1.0 and then abs N <= Numerator_Limit_Real and then abs (I - N / Q) <= Epsilon * abs (N / Q) + Eta,
     Post =>
       abs (N / Q) <= Numerator_Limit_Real
       and then abs I <= Quotient_Limit_Real
       and then abs (I * Q - N) <= Q * (Quotient_Factor * abs I + Quotient_Dust);
   --  The rounded quotient I of N by Q ≥ 1, without the division: I·Q is within Q·(1.01u·|I| + 1.01η) of N.

   procedure Lemma_Quotient_Error (N, Q, I : Real) is
      V : constant Real := N / Q;
   begin
      pragma Assert (V * Q = N);
      pragma Assert (abs V * Q = abs N);
      Lemma_Scale (1.0, Q, abs V);
      pragma Assert (abs V <= abs N);
      pragma Assert (abs V <= abs I + Epsilon * abs V + Eta);
      pragma Assert (Epsilon * abs V <= Quotient_Factor * abs I + Quotient_Slack);
      pragma Assert (abs (I - V) <= Quotient_Factor * abs I + Quotient_Dust);
      pragma Assert (I * Q - N = (I - V) * Q);
      Lemma_Product_Bound (I - V, Q, Quotient_Factor * abs I + Quotient_Dust, Q);
   end Lemma_Quotient_Error;

   procedure Lemma_Quotient (N, Q : Dimensionless; Exact, Size : Real)
   with
     Ghost,
     Pre  =>
       abs N <= Numerator_Limit and then Q >= 1.0 and then abs (R (N) - Exact) <= Five_Steps * Size + Seven_Dusts,
     Post =>
       abs (N / Q) <= Quotient_Limit
       and then R (Q) >= 1.0
       and then abs (R (N) / R (Q)) <= Numerator_Limit_Real
       and then
         abs (R (N / Q) * R (Q) - Exact)
         <= R (Q) * (Quotient_Factor * abs R (N / Q) + Quotient_Dust) + Five_Steps * Size + Seven_Dusts;
   --  The division which ends At_Axis. The exact quotient is bounded too, which is what shows most directly
   --  that the division does not overflow.

   procedure Lemma_Quotient (N, Q : Dimensionless; Exact, Size : Real) is
   begin
      Float_Lemmas.Lemma_Rounding_Error_Div (N, Q);
      pragma Assert (R (Q) >= 1.0);
      pragma Assert (abs R (N) <= Numerator_Limit_Real);
      Lemma_Quotient_Error (R (N), R (Q), R (N / Q));
      Lemma_Within_Quotient_Limit (N / Q);
   end Lemma_Quotient;

   function Is_Axis_Rounded
     (Frame             : Frame_Vector_Array;
      Axis              : Axis_Name;
      Delta_Y           : Component_Values;
      Node              : Ideal_Node;
      Ideal, Size, Slip : Dimensionless) return Boolean
   is (abs (R (Ideal) * R (Node.Ideal_Q) - Partial_Numerator (Frame, Axis, Node, 3))
       <= R (Node.Ideal_Q) * (Quotient_Factor * abs R (Ideal) + Quotient_Dust)
          + Five_Steps * Partial_Size (Frame, Axis, Node, 3)
          + Seven_Dusts
       and then
         abs (R (Size) - Partial_Size (Frame, Axis, Node, 3))
         <= Five_Steps * Partial_Size (Frame, Axis, Node, 3) + Seven_Dusts
       and then
         abs (R (Slip) - Partial_Slip (Frame, Axis, Delta_Y, Node, 3))
         <= Five_Steps * Partial_Slip (Frame, Axis, Delta_Y, Node, 3) + Seven_Dusts)
   with Ghost;
   --  What the rounding analysis of At_Axis establishes about its result: the quotient against the exact
   --  numerator of the computed node, and the two allowances against their exact sums.

   procedure Lemma_Component_Is_Bounded
     (Frame     : Frame_Vector_Array;
      Axis      : Axis_Name;
      Delta_Y   : Component_Values;
      Node      : Ideal_Node;
      Component : Chart_Component_Index)
   with
     Ghost,
     Pre  => Is_Axis_Bounded (Frame, Axis) and then Is_Node_Bounded (Delta_Y, Node),
     Post =>
       abs Frame (Component) (Axis) <= Magnitude_Limit
       and then abs Node.Y (Component) <= Largest_Chart_Value
       and then Delta_Y (Component) >= 0.0
       and then Delta_Y (Component) <= Chart_Allowance * Magnitude_Limit;
   --  The instances of the two preconditions of At_Axis for one component.

   procedure Lemma_Component_Is_Bounded
     (Frame     : Frame_Vector_Array;
      Axis      : Axis_Name;
      Delta_Y   : Component_Values;
      Node      : Ideal_Node;
      Component : Chart_Component_Index)
   is null;

   procedure Lemma_First_Term_Bounds
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node)
   with
     Ghost,
     Pre  => Is_Axis_Bounded (Frame, Axis) and then Is_Node_Bounded (Delta_Y, Node),
     Post =>
       abs Numerator_Term (Frame, Axis, Node, 0) <= First_Term_Cap
       and then Slip_Term (Frame, Axis, Delta_Y, Node, 0) >= 0.0
       and then Slip_Term (Frame, Axis, Delta_Y, Node, 0) <= First_Slip_Cap;

   procedure Lemma_First_Term_Bounds
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node) is
   begin
      pragma Assert (abs Frame (0) (Axis) <= Magnitude_Limit);
      pragma Assert (abs Node.Local_0 <= Largest_Ideal_Denominator);
      pragma Assert (Node.Ideal_Allowance >= 0.0 and then Node.Ideal_Allowance <= Largest_Allowance);
      Lemma_First_Terms (Frame (0) (Axis), Node.Local_0, Node.Ideal_Allowance);
   end Lemma_First_Term_Bounds;

   procedure Lemma_Later_Term_Bounds
     (Frame     : Frame_Vector_Array;
      Axis      : Axis_Name;
      Delta_Y   : Component_Values;
      Node      : Ideal_Node;
      Component : Chart_Component_Index)
   with
     Ghost,
     Pre  => Is_Axis_Bounded (Frame, Axis) and then Is_Node_Bounded (Delta_Y, Node),
     Post =>
       abs Numerator_Term (Frame, Axis, Node, Component) <= Later_Term_Cap
       and then Slip_Term (Frame, Axis, Delta_Y, Node, Component) >= 0.0
       and then Slip_Term (Frame, Axis, Delta_Y, Node, Component) <= Later_Slip_Cap;

   procedure Lemma_Later_Term_Bounds
     (Frame     : Frame_Vector_Array;
      Axis      : Axis_Name;
      Delta_Y   : Component_Values;
      Node      : Ideal_Node;
      Component : Chart_Component_Index) is
   begin
      Lemma_Component_Is_Bounded (Frame, Axis, Delta_Y, Node, Component);
      Lemma_Later_Terms (Frame (Component) (Axis), Node.Y (Component), Delta_Y (Component));
   end Lemma_Later_Term_Bounds;

   procedure Lemma_Sums_Of_Bounded_Terms
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node; Count : Natural)
   with
     Ghost,
     Pre  =>
       Count <= 3
       and then abs Numerator_Term (Frame, Axis, Node, 0) <= First_Term_Cap
       and then abs Numerator_Term (Frame, Axis, Node, 1) <= Later_Term_Cap
       and then abs Numerator_Term (Frame, Axis, Node, 2) <= Later_Term_Cap
       and then abs Numerator_Term (Frame, Axis, Node, 3) <= Later_Term_Cap
       and then Slip_Term (Frame, Axis, Delta_Y, Node, 0) >= 0.0
       and then Slip_Term (Frame, Axis, Delta_Y, Node, 0) <= First_Slip_Cap
       and then Slip_Term (Frame, Axis, Delta_Y, Node, 1) >= 0.0
       and then Slip_Term (Frame, Axis, Delta_Y, Node, 1) <= Later_Slip_Cap
       and then Slip_Term (Frame, Axis, Delta_Y, Node, 2) >= 0.0
       and then Slip_Term (Frame, Axis, Delta_Y, Node, 2) <= Later_Slip_Cap
       and then Slip_Term (Frame, Axis, Delta_Y, Node, 3) >= 0.0
       and then Slip_Term (Frame, Axis, Delta_Y, Node, 3) <= Later_Slip_Cap,
     Post =>
       Partial_Size (Frame, Axis, Node, Count) >= 0.0
       and then Partial_Size (Frame, Axis, Node, Count) <= Size_Cap
       and then abs Partial_Numerator (Frame, Axis, Node, Count) <= Partial_Size (Frame, Axis, Node, Count)
       and then Partial_Slip (Frame, Axis, Delta_Y, Node, Count) >= 0.0
       and then Partial_Slip (Frame, Axis, Delta_Y, Node, Count) <= Slip_Cap;
   --  Sums of up to four terms within their caps are within the caps of the sums.

   procedure Lemma_Sums_Of_Bounded_Terms
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node; Count : Natural)
   is null;

   procedure Lemma_Partial_Bounds
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node; Count : Natural)
   with
     Ghost,
     Pre  => Is_Axis_Bounded (Frame, Axis) and then Is_Node_Bounded (Delta_Y, Node) and then Count <= 3,
     Post =>
       Partial_Size (Frame, Axis, Node, Count) >= 0.0
       and then Partial_Size (Frame, Axis, Node, Count) <= Size_Cap
       and then abs Partial_Numerator (Frame, Axis, Node, Count) <= Partial_Size (Frame, Axis, Node, Count)
       and then Partial_Slip (Frame, Axis, Delta_Y, Node, Count) >= 0.0
       and then Partial_Slip (Frame, Axis, Delta_Y, Node, Count) <= Slip_Cap;
   --  The exact sums are within their caps, whatever the number of terms.

   procedure Lemma_Partial_Bounds
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node; Count : Natural) is
   begin
      Lemma_First_Term_Bounds (Frame, Axis, Delta_Y, Node);
      Lemma_Later_Term_Bounds (Frame, Axis, Delta_Y, Node, 1);
      Lemma_Later_Term_Bounds (Frame, Axis, Delta_Y, Node, 2);
      Lemma_Later_Term_Bounds (Frame, Axis, Delta_Y, Node, 3);
      Lemma_Sums_Of_Bounded_Terms (Frame, Axis, Delta_Y, Node, Count);
   end Lemma_Partial_Bounds;

   procedure Lemma_Within_Sum_Limits (Value : Dimensionless)
   with Ghost, Pre => abs Value <= Numerator_Limit, Post => abs Value <= Slip_Limit;

   procedure Lemma_Within_Sum_Limits (Value : Dimensionless) is null;

   Finite_Limit : constant Dimensionless := 2.0**536
   with Ghost;

   procedure Lemma_Sum_Is_Finite (Left, Right : Dimensionless)
   with
     Ghost,
     Pre  => abs Left <= Slip_Limit and then abs Right <= Slip_Limit,
     Post => abs (Left + Right) <= Finite_Limit;
   --  The three running sums of At_Axis do not overflow.

   procedure Lemma_Sum_Is_Finite (Left, Right : Dimensionless) is null;

   ---------------------------------------------------------------------------
   --  What At_Axis carries along
   ---------------------------------------------------------------------------

   --  As for At_Node: the error bounds of the three running sums are opaque to At_Axis, which only passes
   --  them from one statement to the next.

   package Axis_Tracking
     with Ghost
   is

      function Numerator_Is_Tracked
        (Frame : Frame_Vector_Array; Axis : Axis_Name; Node : Ideal_Node; Count : Natural; Sum : Real) return Boolean
      with Pre => Count <= 3;
      --  Sum, the real value of Numerator after Count additions, against the exact sum of its terms.

      function Size_Is_Tracked
        (Frame : Frame_Vector_Array; Axis : Axis_Name; Node : Ideal_Node; Count : Natural; Sum : Real) return Boolean
      with Pre => Count <= 3;
      --  The same for Numerator_Size.

      function Slip_Is_Tracked
        (Frame   : Frame_Vector_Array;
         Axis    : Axis_Name;
         Delta_Y : Component_Values;
         Node    : Ideal_Node;
         Count   : Natural;
         Sum     : Real) return Boolean
      with Pre => Count <= 3;
      --  The same for Numerator_Slip.

      procedure Lemma_Start
        (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node)
      with
        Pre  => Is_Axis_Bounded (Frame, Axis) and then Is_Node_Bounded (Delta_Y, Node),
        Post =>
          abs (Frame (0) (Axis) * Node.Local_0) <= Numerator_Limit
          and then abs Frame (0) (Axis) * Node.Ideal_Allowance >= 0.0
          and then abs Frame (0) (Axis) * Node.Ideal_Allowance <= Slip_Limit
          and then Numerator_Is_Tracked (Frame, Axis, Node, 0, R (Frame (0) (Axis) * Node.Local_0))
          and then Size_Is_Tracked (Frame, Axis, Node, 0, R (abs (Frame (0) (Axis) * Node.Local_0)))
          and then Slip_Is_Tracked (Frame, Axis, Delta_Y, Node, 0, R (abs Frame (0) (Axis) * Node.Ideal_Allowance));
      --  The initial values of the three sums.

      procedure Lemma_Numerator_Step
        (Frame     : Frame_Vector_Array;
         Axis      : Axis_Name;
         Delta_Y   : Component_Values;
         Node      : Ideal_Node;
         Component : Chart_Component_Index;
         Sum       : Dimensionless)
      with
        Pre  =>
          Is_Axis_Bounded (Frame, Axis)
          and then Is_Node_Bounded (Delta_Y, Node)
          and then abs Sum <= Numerator_Limit
          and then Numerator_Is_Tracked (Frame, Axis, Node, Component - 1, R (Sum)),
        Post =>
          abs (2.0 * Node.Y (Component)) <= Doubled_Limit
          and then abs (Frame (Component) (Axis) * (2.0 * Node.Y (Component))) <= Numerator_Limit
          and then abs (Sum + Frame (Component) (Axis) * (2.0 * Node.Y (Component))) <= Numerator_Limit
          and then
            Numerator_Is_Tracked
              (Frame, Axis, Node, Component, R (Sum + Frame (Component) (Axis) * (2.0 * Node.Y (Component))));
      --  One more term added to Numerator.

      procedure Lemma_Size_Step
        (Frame     : Frame_Vector_Array;
         Axis      : Axis_Name;
         Delta_Y   : Component_Values;
         Node      : Ideal_Node;
         Component : Chart_Component_Index;
         Sum       : Dimensionless)
      with
        Pre  =>
          Is_Axis_Bounded (Frame, Axis)
          and then Is_Node_Bounded (Delta_Y, Node)
          and then Sum >= 0.0
          and then Sum <= Numerator_Limit
          and then Size_Is_Tracked (Frame, Axis, Node, Component - 1, R (Sum)),
        Post =>
          abs (2.0 * Node.Y (Component)) <= Doubled_Limit
          and then abs (Frame (Component) (Axis) * (2.0 * Node.Y (Component))) <= Numerator_Limit
          and then Sum + abs (Frame (Component) (Axis) * (2.0 * Node.Y (Component))) >= 0.0
          and then Sum + abs (Frame (Component) (Axis) * (2.0 * Node.Y (Component))) <= Numerator_Limit
          and then
            Size_Is_Tracked
              (Frame, Axis, Node, Component, R (Sum + abs (Frame (Component) (Axis) * (2.0 * Node.Y (Component)))));
      --  One more magnitude added to Numerator_Size.

      procedure Lemma_Slip_Step
        (Frame     : Frame_Vector_Array;
         Axis      : Axis_Name;
         Delta_Y   : Component_Values;
         Node      : Ideal_Node;
         Component : Chart_Component_Index;
         Sum       : Dimensionless)
      with
        Pre  =>
          Is_Axis_Bounded (Frame, Axis)
          and then Is_Node_Bounded (Delta_Y, Node)
          and then Sum >= 0.0
          and then Sum <= Slip_Limit
          and then Slip_Is_Tracked (Frame, Axis, Delta_Y, Node, Component - 1, R (Sum)),
        Post =>
          2.0 * Delta_Y (Component) >= 0.0
          and then 2.0 * Delta_Y (Component) <= Doubled_Delta_Limit
          and then abs Frame (Component) (Axis) * (2.0 * Delta_Y (Component)) >= 0.0
          and then abs Frame (Component) (Axis) * (2.0 * Delta_Y (Component)) <= Slip_Limit
          and then Sum + abs Frame (Component) (Axis) * (2.0 * Delta_Y (Component)) >= 0.0
          and then Sum + abs Frame (Component) (Axis) * (2.0 * Delta_Y (Component)) <= Slip_Limit
          and then
            Slip_Is_Tracked
              (Frame,
               Axis,
               Delta_Y,
               Node,
               Component,
               R (Sum + abs Frame (Component) (Axis) * (2.0 * Delta_Y (Component))));
      --  One more term added to Numerator_Slip.

      function Is_Rounded
        (Frame             : Frame_Vector_Array;
         Axis              : Axis_Name;
         Delta_Y           : Component_Values;
         Node              : Ideal_Node;
         Ideal, Size, Slip : Dimensionless) return Boolean;
      --  Is_Axis_Rounded, opaque to At_Axis.

      procedure Lemma_Finish
        (Frame                 : Frame_Vector_Array;
         Axis                  : Axis_Name;
         Delta_Y               : Component_Values;
         Node                  : Ideal_Node;
         Numerator, Size, Slip : Dimensionless)
      with
        Pre  =>
          Is_Node_Bounded (Delta_Y, Node)
          and then abs Numerator <= Numerator_Limit
          and then Numerator_Is_Tracked (Frame, Axis, Node, 3, R (Numerator))
          and then Size_Is_Tracked (Frame, Axis, Node, 3, R (Size))
          and then Slip_Is_Tracked (Frame, Axis, Delta_Y, Node, 3, R (Slip)),
        Post =>
          Node.Ideal_Q >= 1.0
          and then R (Node.Ideal_Q) >= 1.0
          and then abs (R (Numerator) / R (Node.Ideal_Q)) <= Numerator_Limit_Real
          and then abs (Numerator / Node.Ideal_Q) <= Quotient_Limit
          and then Is_Rounded (Frame, Axis, Delta_Y, Node, Numerator / Node.Ideal_Q, Size, Slip);
      --  The division, and what the complete sums amount to.

      procedure Lemma_Rounded
        (Frame             : Frame_Vector_Array;
         Axis              : Axis_Name;
         Delta_Y           : Component_Values;
         Node              : Ideal_Node;
         Ideal, Size, Slip : Dimensionless)
      with
        Pre  => Is_Rounded (Frame, Axis, Delta_Y, Node, Ideal, Size, Slip),
        Post => Is_Axis_Rounded (Frame, Axis, Delta_Y, Node, Ideal, Size, Slip);
      --  What Is_Rounded means.

   end Axis_Tracking;

   package body Axis_Tracking is

      function Numerator_Is_Tracked
        (Frame : Frame_Vector_Array; Axis : Axis_Name; Node : Ideal_Node; Count : Natural; Sum : Real) return Boolean
      is (abs (Sum - Partial_Numerator (Frame, Axis, Node, Count))
          <= Axis_Factor (Count) * Partial_Size (Frame, Axis, Node, Count) + Axis_Dust_At (Count));

      function Size_Is_Tracked
        (Frame : Frame_Vector_Array; Axis : Axis_Name; Node : Ideal_Node; Count : Natural; Sum : Real) return Boolean
      is (abs (Sum - Partial_Size (Frame, Axis, Node, Count))
          <= Axis_Factor (Count) * Partial_Size (Frame, Axis, Node, Count) + Axis_Dust_At (Count));

      function Slip_Is_Tracked
        (Frame   : Frame_Vector_Array;
         Axis    : Axis_Name;
         Delta_Y : Component_Values;
         Node    : Ideal_Node;
         Count   : Natural;
         Sum     : Real) return Boolean
      is (abs (Sum - Partial_Slip (Frame, Axis, Delta_Y, Node, Count))
          <= Axis_Factor (Count) * Partial_Slip (Frame, Axis, Delta_Y, Node, Count) + Axis_Dust_At (Count));

      function Is_Rounded
        (Frame             : Frame_Vector_Array;
         Axis              : Axis_Name;
         Delta_Y           : Component_Values;
         Node              : Ideal_Node;
         Ideal, Size, Slip : Dimensionless) return Boolean
      is (Is_Axis_Rounded (Frame, Axis, Delta_Y, Node, Ideal, Size, Slip));

      procedure Lemma_Rounded
        (Frame             : Frame_Vector_Array;
         Axis              : Axis_Name;
         Delta_Y           : Component_Values;
         Node              : Ideal_Node;
         Ideal, Size, Slip : Dimensionless)
      is null;

      procedure Lemma_Start
        (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node) is
      begin
         pragma Assert (abs Frame (0) (Axis) <= Magnitude_Limit);
         pragma Assert (abs Node.Local_0 <= Largest_Ideal_Denominator);
         pragma Assert (Node.Ideal_Allowance >= 0.0 and then Node.Ideal_Allowance <= Largest_Allowance);
         Lemma_First_Terms (Frame (0) (Axis), Node.Local_0, Node.Ideal_Allowance);
         pragma Assert (Tally (0) = 0.0);
         pragma Assert (Axis_Factor (0) = Two_Steps);
         pragma Assert (Axis_Dust_At (0) = Axis_Dust);
      end Lemma_Start;

      procedure Lemma_Numerator_Step
        (Frame     : Frame_Vector_Array;
         Axis      : Axis_Name;
         Delta_Y   : Component_Values;
         Node      : Ideal_Node;
         Component : Chart_Component_Index;
         Sum       : Dimensionless) is
      begin
         Lemma_Small_Tally (Component - 1);
         Lemma_Partial_Step (Frame, Axis, Delta_Y, Node, Component);
         Lemma_Partial_Bounds (Frame, Axis, Delta_Y, Node, Component - 1);
         Lemma_Partial_Bounds (Frame, Axis, Delta_Y, Node, Component);
         Lemma_Component_Is_Bounded (Frame, Axis, Delta_Y, Node, Component);
         Lemma_Later_Terms (Frame (Component) (Axis), Node.Y (Component), Delta_Y (Component));
         Lemma_Within_Sum_Limits (Sum);
         Lemma_Within_Sum_Limits (Frame (Component) (Axis) * (2.0 * Node.Y (Component)));
         Lemma_Sum_Is_Finite (Sum, Frame (Component) (Axis) * (2.0 * Node.Y (Component)));
         Lemma_Axis_Sum
           (Sum         => Sum,
            Term        => Frame (Component) (Axis) * (2.0 * Node.Y (Component)),
            Exact       => Partial_Numerator (Frame, Axis, Node, Component - 1),
            Size        => Partial_Size (Frame, Axis, Node, Component - 1),
            Exact_Term  => Numerator_Term (Frame, Axis, Node, Component),
            Factor      => Axis_Factor (Component - 1),
            Dust        => Axis_Dust_At (Component - 1),
            Next_Exact  => Partial_Numerator (Frame, Axis, Node, Component),
            Next_Size   => Partial_Size (Frame, Axis, Node, Component),
            Next_Factor => Axis_Factor (Component),
            Next_Dust   => Axis_Dust_At (Component),
            Cap         => Size_Cap);
         Lemma_Within_Numerator_Limit (Sum + Frame (Component) (Axis) * (2.0 * Node.Y (Component)));
      end Lemma_Numerator_Step;

      procedure Lemma_Size_Step
        (Frame     : Frame_Vector_Array;
         Axis      : Axis_Name;
         Delta_Y   : Component_Values;
         Node      : Ideal_Node;
         Component : Chart_Component_Index;
         Sum       : Dimensionless) is
      begin
         Lemma_Small_Tally (Component - 1);
         Lemma_Partial_Step (Frame, Axis, Delta_Y, Node, Component);
         Lemma_Partial_Bounds (Frame, Axis, Delta_Y, Node, Component - 1);
         Lemma_Partial_Bounds (Frame, Axis, Delta_Y, Node, Component);
         Lemma_Component_Is_Bounded (Frame, Axis, Delta_Y, Node, Component);
         Lemma_Later_Terms (Frame (Component) (Axis), Node.Y (Component), Delta_Y (Component));
         Lemma_Within_Sum_Limits (Sum);
         Lemma_Within_Sum_Limits (abs (Frame (Component) (Axis) * (2.0 * Node.Y (Component))));
         Lemma_Sum_Is_Finite (Sum, abs (Frame (Component) (Axis) * (2.0 * Node.Y (Component))));
         Lemma_Axis_Sum
           (Sum         => Sum,
            Term        => abs (Frame (Component) (Axis) * (2.0 * Node.Y (Component))),
            Exact       => Partial_Size (Frame, Axis, Node, Component - 1),
            Size        => Partial_Size (Frame, Axis, Node, Component - 1),
            Exact_Term  => abs Numerator_Term (Frame, Axis, Node, Component),
            Factor      => Axis_Factor (Component - 1),
            Dust        => Axis_Dust_At (Component - 1),
            Next_Exact  => Partial_Size (Frame, Axis, Node, Component),
            Next_Size   => Partial_Size (Frame, Axis, Node, Component),
            Next_Factor => Axis_Factor (Component),
            Next_Dust   => Axis_Dust_At (Component),
            Cap         => Size_Cap);
         Lemma_Sum_Sign (Sum, abs (Frame (Component) (Axis) * (2.0 * Node.Y (Component))));
         Lemma_Within_Numerator_Limit (Sum + abs (Frame (Component) (Axis) * (2.0 * Node.Y (Component))));
      end Lemma_Size_Step;

      procedure Lemma_Slip_Step
        (Frame     : Frame_Vector_Array;
         Axis      : Axis_Name;
         Delta_Y   : Component_Values;
         Node      : Ideal_Node;
         Component : Chart_Component_Index;
         Sum       : Dimensionless) is
      begin
         Lemma_Small_Tally (Component - 1);
         Lemma_Partial_Step (Frame, Axis, Delta_Y, Node, Component);
         Lemma_Partial_Bounds (Frame, Axis, Delta_Y, Node, Component - 1);
         Lemma_Partial_Bounds (Frame, Axis, Delta_Y, Node, Component);
         Lemma_Component_Is_Bounded (Frame, Axis, Delta_Y, Node, Component);
         Lemma_Later_Terms (Frame (Component) (Axis), Node.Y (Component), Delta_Y (Component));
         Lemma_Sum_Is_Finite (Sum, abs Frame (Component) (Axis) * (2.0 * Delta_Y (Component)));
         Lemma_Axis_Sum
           (Sum         => Sum,
            Term        => abs Frame (Component) (Axis) * (2.0 * Delta_Y (Component)),
            Exact       => Partial_Slip (Frame, Axis, Delta_Y, Node, Component - 1),
            Size        => Partial_Slip (Frame, Axis, Delta_Y, Node, Component - 1),
            Exact_Term  => Slip_Term (Frame, Axis, Delta_Y, Node, Component),
            Factor      => Axis_Factor (Component - 1),
            Dust        => Axis_Dust_At (Component - 1),
            Next_Exact  => Partial_Slip (Frame, Axis, Delta_Y, Node, Component),
            Next_Size   => Partial_Slip (Frame, Axis, Delta_Y, Node, Component),
            Next_Factor => Axis_Factor (Component),
            Next_Dust   => Axis_Dust_At (Component),
            Cap         => Slip_Cap);
         Lemma_Sum_Sign (Sum, abs Frame (Component) (Axis) * (2.0 * Delta_Y (Component)));
         Lemma_Within_Slip_Limit (Sum + abs Frame (Component) (Axis) * (2.0 * Delta_Y (Component)));
      end Lemma_Slip_Step;

      procedure Lemma_Finish
        (Frame                 : Frame_Vector_Array;
         Axis                  : Axis_Name;
         Delta_Y               : Component_Values;
         Node                  : Ideal_Node;
         Numerator, Size, Slip : Dimensionless) is
      begin
         pragma Assert (Node.Ideal_Q >= 1.0);
         pragma Assert (Tally (3) = 3.0);
         pragma Assert (Axis_Factor (3) = Five_Steps);
         pragma Assert (Axis_Dust_At (3) = Seven_Dusts);
         pragma
           Assert
             (abs (R (Numerator) - Partial_Numerator (Frame, Axis, Node, 3))
              <= Five_Steps * Partial_Size (Frame, Axis, Node, 3) + Seven_Dusts);
         pragma
           Assert
             (abs (R (Size) - Partial_Size (Frame, Axis, Node, 3))
              <= Five_Steps * Partial_Size (Frame, Axis, Node, 3) + Seven_Dusts);
         pragma
           Assert
             (abs (R (Slip) - Partial_Slip (Frame, Axis, Delta_Y, Node, 3))
              <= Five_Steps * Partial_Slip (Frame, Axis, Delta_Y, Node, 3) + Seven_Dusts);
         Lemma_Quotient
           (Numerator, Node.Ideal_Q, Partial_Numerator (Frame, Axis, Node, 3), Partial_Size (Frame, Axis, Node, 3));
         pragma Assert (Is_Axis_Rounded (Frame, Axis, Delta_Y, Node, Numerator / Node.Ideal_Q, Size, Slip));
      end Lemma_Finish;

   end Axis_Tracking;

   function At_Axis
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node) return Ideal_Axis
   with
     Refined_Post =>
       abs At_Axis'Result.Ideal <= Largest_Axis_Value
       and then At_Axis'Result.Numerator_Slip >= 0.0
       and then At_Axis'Result.Numerator_Slip <= Largest_Axis_Value
       and then At_Axis'Result.Numerator_Size >= 0.0
       and then At_Axis'Result.Numerator_Size <= Largest_Axis_Value
       and then
         Axis_Tracking.Is_Rounded
           (Frame,
            Axis,
            Delta_Y,
            Node,
            At_Axis'Result.Ideal,
            At_Axis'Result.Numerator_Size,
            At_Axis'Result.Numerator_Slip)
   is
      Numerator      : Dimensionless := Frame (0) (Axis) * Node.Local_0;
      Numerator_Size : Dimensionless := abs Numerator;
      Numerator_Slip : Dimensionless := abs Frame (0) (Axis) * Node.Ideal_Allowance;
   begin
      Axis_Tracking.Lemma_Start (Frame, Axis, Delta_Y, Node);
      for Component in Chart_Component_Index loop
         pragma Loop_Invariant (abs Numerator <= Numerator_Limit);
         pragma Loop_Invariant (Numerator_Size >= 0.0 and then Numerator_Size <= Numerator_Limit);
         pragma Loop_Invariant (Numerator_Slip >= 0.0 and then Numerator_Slip <= Slip_Limit);
         pragma Loop_Invariant (Axis_Tracking.Numerator_Is_Tracked (Frame, Axis, Node, Component - 1, R (Numerator)));
         pragma Loop_Invariant (Axis_Tracking.Size_Is_Tracked (Frame, Axis, Node, Component - 1, R (Numerator_Size)));
         pragma
           Loop_Invariant
             (Axis_Tracking.Slip_Is_Tracked (Frame, Axis, Delta_Y, Node, Component - 1, R (Numerator_Slip)));

         Axis_Tracking.Lemma_Numerator_Step (Frame, Axis, Delta_Y, Node, Component, Numerator);
         Axis_Tracking.Lemma_Size_Step (Frame, Axis, Delta_Y, Node, Component, Numerator_Size);
         Axis_Tracking.Lemma_Slip_Step (Frame, Axis, Delta_Y, Node, Component, Numerator_Slip);
         declare
            Product : constant Dimensionless := Frame (Component) (Axis) * (2.0 * Node.Y (Component));
         begin
            Numerator := Numerator + Product;
            Numerator_Size := Numerator_Size + abs Product;
            Numerator_Slip := Numerator_Slip + abs Frame (Component) (Axis) * (2.0 * Delta_Y (Component));
         end;
      end loop;

      Axis_Tracking.Lemma_Finish (Frame, Axis, Delta_Y, Node, Numerator, Numerator_Size, Numerator_Slip);
      return (Ideal => Numerator / Node.Ideal_Q, Numerator_Slip => Numerator_Slip, Numerator_Size => Numerator_Size);
   end At_Axis;

   ---------------------------------------------------------------------------
   --  One axis against the ideal tangent
   ---------------------------------------------------------------------------

   --  With T = P/D the exact tangent component, D ≥ 1, I the computed one, Q the computed denominator and N the
   --  exact numerator of the computed node,
   --
   --     (T - I)·D = (P - N) + (N - I·Q) + I·(Q - D).
   --
   --  The first difference is what the chart allowances and the node's allowance permit, the second is the
   --  rounding of At_Axis, and the third is the node's allowance again.

   Node_Factor : constant := 1.0 + 2.0**(-48);
   --  1 + Node_Slack.

   Error_Cap : constant := 1.1;
   --  More than the error which Is_Node_Accurate permits for a node whose allowance is at most one.

   Size_Scale : constant := 9.8 * 2.0**200;
   --  The sum of the magnitudes of the numerator's terms is at most this times Q, plus the slip.

   Ideal_Scale : constant := 10.1 * 2.0**200;
   --  The computed component is at most this, plus 1.1 times the slip.

   Gap_Dust : constant := 2.0**200 * 2.0**(-1010) + 6.0 * 2.0**200 * 2.0**(-1060);
   --  Frame_Cap times Denominator_Dust, and three times Frame_Cap times twice Chart_Dust.

   procedure Lemma_Cancel (A, B, C : Real)
   with Ghost, Pre => C > 0.0 and then A * C <= B * C, Post => A <= B;

   procedure Lemma_Cancel (A, B, C : Real) is
   begin
      if A > B then
         pragma Assert ((A - B) * C > 0.0);
         pragma Assert ((A - B) * C = A * C - B * C);
      end if;
   end Lemma_Cancel;

   procedure Lemma_Division (P, D : Real)
   with Ghost, Pre => D >= 1.0, Post => (P / D) * D = P;

   procedure Lemma_Division (P, D : Real) is null;

   procedure Lemma_Double_Root (C : Real)
   with Ghost, Post => C * C >= 0.0 and then 2.0 * abs C <= 1.0 + C * C;
   --  (|C| - 1)² ≥ 0.

   procedure Lemma_Double_Root (C : Real) is
      Size : constant Real := abs C;
   begin
      pragma Assert ((Size - 1.0) * (Size - 1.0) >= 0.0);
      pragma Assert ((Size - 1.0) * (Size - 1.0) = Size * Size - 2.0 * Size + 1.0);
      pragma Assert (Size * Size = C * C);
   end Lemma_Double_Root;

   procedure Lemma_Chart_Facts (C1, C2, C3, N0, Dt : Real)
   with
     Ghost,
     Pre  => N0 = 1.0 - C1 * C1 - (C2 * C2 + C3 * C3) and then Dt = 1.0 + C1 * C1 + (C2 * C2 + C3 * C3),
     Post =>
       Dt >= 1.0
       and then abs N0 <= Dt
       and then 2.0 * abs C1 <= Dt
       and then 2.0 * abs C2 <= Dt
       and then 2.0 * abs C3 <= Dt;
   --  Neither the first numerator nor twice a chart coordinate exceeds the denominator.

   procedure Lemma_Chart_Facts (C1, C2, C3, N0, Dt : Real) is
   begin
      Lemma_Double_Root (C1);
      Lemma_Double_Root (C2);
      Lemma_Double_Root (C3);
   end Lemma_Chart_Facts;

   procedure Lemma_Numerator_Gap (F0, F1, F2, F3, L0, N0, Y1, Y2, Y3, C1, C2, C3, E, G1, G2, G3 : Real)
   with
     Ghost,
     Pre  => abs (L0 - N0) <= E and then abs (Y1 - C1) <= G1 and then abs (Y2 - C2) <= G2 and then abs (Y3 - C3) <= G3,
     Post =>
       abs ((N0 * F0 + (2.0 * C1) * F1 + (2.0 * C2) * F2 + (2.0 * C3) * F3)
            - (F0 * L0 + F1 * (2.0 * Y1) + F2 * (2.0 * Y2) + F3 * (2.0 * Y3)))
       <= abs F0 * E + abs F1 * (2.0 * G1) + abs F2 * (2.0 * G2) + abs F3 * (2.0 * G3);
   --  P - N, term by term.

   procedure Lemma_Numerator_Gap (F0, F1, F2, F3, L0, N0, Y1, Y2, Y3, C1, C2, C3, E, G1, G2, G3 : Real) is
   begin
      Lemma_Product_Bound (F0, N0 - L0, abs F0, E);
      Lemma_Product_Bound (F1, 2.0 * C1 - 2.0 * Y1, abs F1, 2.0 * G1);
      Lemma_Product_Bound (F2, 2.0 * C2 - 2.0 * Y2, abs F2, 2.0 * G2);
      Lemma_Product_Bound (F3, 2.0 * C3 - 2.0 * Y3, abs F3, 2.0 * G3);
      pragma Assert (N0 * F0 - F0 * L0 = F0 * (N0 - L0));
      pragma Assert ((2.0 * C1) * F1 - F1 * (2.0 * Y1) = F1 * (2.0 * C1 - 2.0 * Y1));
      pragma Assert ((2.0 * C2) * F2 - F2 * (2.0 * Y2) = F2 * (2.0 * C2 - 2.0 * Y2));
      pragma Assert ((2.0 * C3) * F3 - F3 * (2.0 * Y3) = F3 * (2.0 * C3 - 2.0 * Y3));
   end Lemma_Numerator_Gap;

   procedure Lemma_Node_Magnitudes (L0, N0, Q, Dt, E, Y, C, D : Real)
   with
     Ghost,
     Pre  =>
       Q >= 1.0
       and then E >= 0.0
       and then E <= Error_Cap
       and then abs (Q - Dt) <= E
       and then abs (L0 - N0) <= E
       and then abs N0 <= Dt
       and then 2.0 * abs C <= Dt
       and then D >= 0.0
       and then abs (Y - C) <= D + Chart_Dust,
     Post => abs L0 <= 3.2 * Q and then abs (2.0 * Y) <= 2.2 * Q + 2.0 * D;
   --  The computed first numerator and chart coordinates are bounded through the computed denominator.

   procedure Lemma_Node_Magnitudes (L0, N0, Q, Dt, E, Y, C, D : Real) is null;

   procedure Lemma_Later_Size (F, Y, D, Q : Real)
   with
     Ghost,
     Pre  => abs F <= Frame_Cap and then Q >= 0.0 and then D >= 0.0 and then abs (2.0 * Y) <= 2.2 * Q + 2.0 * D,
     Post => abs (F * (2.0 * Y)) <= Frame_Cap * (2.2 * Q) + abs F * (2.0 * D);

   procedure Lemma_Later_Size (F, Y, D, Q : Real) is
   begin
      Lemma_Product_Bound (F, 2.0 * Y, abs F, 2.2 * Q + 2.0 * D);
      Lemma_Distribute (abs F, 2.2 * Q, 2.0 * D);
      Lemma_Scale (abs F, Frame_Cap, 2.2 * Q);
   end Lemma_Later_Size;

   procedure Lemma_Size_Bound (F0, F1, F2, F3, L0, Q, Y1, Y2, Y3, D1, D2, D3 : Real)
   with
     Ghost,
     Pre  =>
       abs F0 <= Frame_Cap
       and then abs F1 <= Frame_Cap
       and then abs F2 <= Frame_Cap
       and then abs F3 <= Frame_Cap
       and then Q >= 1.0
       and then D1 >= 0.0
       and then D2 >= 0.0
       and then D3 >= 0.0
       and then abs L0 <= 3.2 * Q
       and then abs (2.0 * Y1) <= 2.2 * Q + 2.0 * D1
       and then abs (2.0 * Y2) <= 2.2 * Q + 2.0 * D2
       and then abs (2.0 * Y3) <= 2.2 * Q + 2.0 * D3,
     Post =>
       abs (F0 * L0) + abs (F1 * (2.0 * Y1)) + abs (F2 * (2.0 * Y2)) + abs (F3 * (2.0 * Y3))
       <= Size_Scale * Q + (abs F1 * (2.0 * D1) + abs F2 * (2.0 * D2) + abs F3 * (2.0 * D3));
   --  The magnitudes of the numerator's terms: a multiple of Q, and the later terms of the slip.

   procedure Lemma_Size_Bound (F0, F1, F2, F3, L0, Q, Y1, Y2, Y3, D1, D2, D3 : Real) is
   begin
      Lemma_Product_Bound (F0, L0, Frame_Cap, 3.2 * Q);
      Lemma_Later_Size (F1, Y1, D1, Q);
      Lemma_Later_Size (F2, Y2, D2, Q);
      Lemma_Later_Size (F3, Y3, D3, Q);
   end Lemma_Size_Bound;

   procedure Lemma_Ideal_Magnitude (I, Q, PN, PZ, PS : Real)
   with
     Ghost,
     Pre  =>
       Q >= 1.0
       and then PS >= 0.0
       and then PZ >= 0.0
       and then abs PN <= PZ
       and then PZ <= Size_Scale * Q + PS
       and then abs (I * Q - PN) <= Q * (Quotient_Factor * abs I + Quotient_Dust) + Five_Steps * PZ + Seven_Dusts,
     Post => abs I <= Ideal_Scale + 1.1 * PS;
   --  The computed component is about the numerator over Q, so a multiple of the frame limit plus the slip.

   procedure Lemma_Ideal_Magnitude (I, Q, PN, PZ, PS : Real) is
      Scaled_Ideal : constant Real := abs I * Q;
      Scaled_Slip  : constant Real := PS * Q;
   begin
      pragma Assert (abs (I * Q) = Scaled_Ideal);
      Lemma_Scale (1.0, Q, PS);
      pragma Assert (PS <= Scaled_Slip);
      Lemma_Distribute (Q, Quotient_Factor * abs I, Quotient_Dust);
      pragma Assert (Q * (Quotient_Factor * abs I) = Quotient_Factor * Scaled_Ideal);
      pragma
        Assert
          (Scaled_Ideal
           <= PZ + Quotient_Factor * Scaled_Ideal + Quotient_Dust * Q + Five_Steps * PZ + Seven_Dusts * Q);
      pragma Assert (Scaled_Ideal <= Ideal_Scale * Q + 1.1 * Scaled_Slip);
      Lemma_Distribute (Q, Ideal_Scale, 1.1 * PS);
      pragma Assert ((Ideal_Scale + 1.1 * PS) * Q = Ideal_Scale * Q + 1.1 * Scaled_Slip);
      Lemma_Cancel (abs I, Ideal_Scale + 1.1 * PS, Q);
   end Lemma_Ideal_Magnitude;

   procedure Lemma_Tangent_Split (T, I, P, PN, Dt, Q : Real)
   with Ghost, Pre => T * Dt = P, Post => (T - I) * Dt = (P - PN) + (PN - I * Q) + I * (Q - Dt);

   procedure Lemma_Tangent_Split (T, I, P, PN, Dt, Q : Real) is
   begin
      pragma Assert ((T - I) * Dt = T * Dt - I * Dt);
      pragma Assert (I * (Q - Dt) = I * Q - I * Dt);
   end Lemma_Tangent_Split;

   procedure Lemma_Tangent_Error (T, I, Dt, Q, Gap, Per_Q, Rest, E : Real)
   with
     Ghost,
     Pre  =>
       Dt >= 1.0
       and then Q >= 1.0
       and then E >= 0.0
       and then abs (Q - Dt) <= E
       and then Gap >= 0.0
       and then Per_Q >= 0.0
       and then Rest >= 0.0
       and then abs ((T - I) * Dt) <= Gap + (Q * Per_Q + Rest) + abs I * E,
     Post => abs (T - I) <= Gap + Rest + (1.0 + E) * Per_Q + abs I * E;
   --  Divide the split by D ≥ 1. The one part which carries a factor Q keeps a factor Q/D ≤ 1 + E.

   procedure Lemma_Tangent_Error (T, I, Dt, Q, Gap, Per_Q, Rest, E : Real) is
      Weighted : constant Real := abs I * E;
      Scaled   : constant Real := (1.0 + E) * Per_Q;
      Bound    : constant Real := Gap + Rest + Scaled + Weighted;
   begin
      Lemma_Scale (1.0, Dt, E);
      Lemma_Distribute (Dt, 1.0, E);
      pragma Assert (Q <= Dt * (1.0 + E));
      Lemma_Scale (Q, Dt * (1.0 + E), Per_Q);
      pragma Assert ((Dt * (1.0 + E)) * Per_Q = Scaled * Dt);
      pragma Assert (Q * Per_Q <= Scaled * Dt);
      Lemma_Sign (abs I, E);
      Lemma_Scale (1.0, Dt, Gap);
      Lemma_Scale (1.0, Dt, Rest);
      Lemma_Scale (1.0, Dt, Weighted);
      pragma Assert (abs ((T - I) * Dt) = abs (T - I) * Dt);
      pragma Assert (Bound * Dt = Gap * Dt + Rest * Dt + Scaled * Dt + Weighted * Dt);
      pragma Assert (abs (T - I) * Dt <= Bound * Dt);
      Lemma_Cancel (abs (T - I), Bound, Dt);
   end Lemma_Tangent_Error;

   procedure Lemma_Axis_Core
     (F0, F1, F2, F3, L0, Q, A, E, Y1, Y2, Y3, D1, D2, D3, C1, C2, C3, N0, Dt, P, T, I, PN, PZ, PS : Real)
   with
     Ghost,
     Pre  =>
       abs F0 <= Frame_Cap
       and then abs F1 <= Frame_Cap
       and then abs F2 <= Frame_Cap
       and then abs F3 <= Frame_Cap
       and then Q >= 1.0
       and then A >= 0.0
       and then E >= 0.0
       and then E <= Error_Cap
       and then D1 >= 0.0
       and then D2 >= 0.0
       and then D3 >= 0.0
       and then N0 = 1.0 - C1 * C1 - (C2 * C2 + C3 * C3)
       and then Dt = 1.0 + C1 * C1 + (C2 * C2 + C3 * C3)
       and then P = N0 * F0 + (2.0 * C1) * F1 + (2.0 * C2) * F2 + (2.0 * C3) * F3
       and then T * Dt = P
       and then PN = F0 * L0 + F1 * (2.0 * Y1) + F2 * (2.0 * Y2) + F3 * (2.0 * Y3)
       and then PZ = abs (F0 * L0) + abs (F1 * (2.0 * Y1)) + abs (F2 * (2.0 * Y2)) + abs (F3 * (2.0 * Y3))
       and then PS = abs F0 * A + abs F1 * (2.0 * D1) + abs F2 * (2.0 * D2) + abs F3 * (2.0 * D3)
       and then abs (Y1 - C1) <= D1 + Chart_Dust
       and then abs (Y2 - C2) <= D2 + Chart_Dust
       and then abs (Y3 - C3) <= D3 + Chart_Dust
       and then abs (Q - Dt) <= E
       and then abs (L0 - N0) <= E
       and then abs (I * Q - PN) <= Q * (Quotient_Factor * abs I + Quotient_Dust) + Five_Steps * PZ + Seven_Dusts,
     Post =>
       PZ >= 0.0
       and then PS >= 0.0
       and then abs I <= Ideal_Scale + 1.1 * PS
       and then
         abs (I - T)
         <= (abs F0 * E + abs F1 * (2.0 * (D1 + Chart_Dust)) + abs F2 * (2.0 * (D2 + Chart_Dust))
             + abs F3 * (2.0 * (D3 + Chart_Dust)))
            + (Five_Steps * PZ + Seven_Dusts)
            + 2.1 * (Quotient_Factor * abs I + Quotient_Dust)
            + abs I * E;
   --  The comparison over the reals, for a node whose denominator and first numerator are within E of the exact
   --  ones: the frame components F, the computed node (L0, Q, A, Y), the chart allowances D, the exact chart
   --  coordinates C with their numerator N0, denominator Dt and tangent T = P/Dt, and At_Axis's quotient I with
   --  the exact sums PN, PZ and PS of the terms which it adds.

   procedure Lemma_Axis_Core
     (F0, F1, F2, F3, L0, Q, A, E, Y1, Y2, Y3, D1, D2, D3, C1, C2, C3, N0, Dt, P, T, I, PN, PZ, PS : Real)
   is
      G1    : constant Real := D1 + Chart_Dust;
      G2    : constant Real := D2 + Chart_Dust;
      G3    : constant Real := D3 + Chart_Dust;
      Gap   : constant Real := abs F0 * E + abs F1 * (2.0 * G1) + abs F2 * (2.0 * G2) + abs F3 * (2.0 * G3);
      Per_Q : constant Real := Quotient_Factor * abs I + Quotient_Dust;
      Rest  : constant Real := Five_Steps * PZ + Seven_Dusts;
   begin
      Lemma_Chart_Facts (C1, C2, C3, N0, Dt);
      Lemma_Numerator_Gap (F0, F1, F2, F3, L0, N0, Y1, Y2, Y3, C1, C2, C3, E, G1, G2, G3);
      pragma Assert (abs (P - PN) <= Gap);
      Lemma_Sign (abs F0, E);
      Lemma_Sign (abs F1, 2.0 * G1);
      Lemma_Sign (abs F2, 2.0 * G2);
      Lemma_Sign (abs F3, 2.0 * G3);
      pragma Assert (Gap >= 0.0);
      Lemma_Sign (abs F0, A);
      Lemma_Sign (abs F1, 2.0 * D1);
      Lemma_Sign (abs F2, 2.0 * D2);
      Lemma_Sign (abs F3, 2.0 * D3);
      pragma Assert (PS >= 0.0);
      pragma Assert (PZ >= 0.0);
      pragma Assert (abs PN <= PZ);

      Lemma_Node_Magnitudes (L0, N0, Q, Dt, E, Y1, C1, D1);
      Lemma_Node_Magnitudes (L0, N0, Q, Dt, E, Y2, C2, D2);
      Lemma_Node_Magnitudes (L0, N0, Q, Dt, E, Y3, C3, D3);
      Lemma_Size_Bound (F0, F1, F2, F3, L0, Q, Y1, Y2, Y3, D1, D2, D3);
      pragma Assert (PZ <= Size_Scale * Q + PS);
      Lemma_Ideal_Magnitude (I, Q, PN, PZ, PS);

      Lemma_Tangent_Split (T, I, P, PN, Dt, Q);
      Lemma_Product_Bound (I, Q - Dt, abs I, E);
      pragma Assert (abs (PN - I * Q) <= Q * Per_Q + Rest);
      pragma Assert (abs ((T - I) * Dt) <= Gap + (Q * Per_Q + Rest) + abs I * E);
      Lemma_Tangent_Error (T, I, Dt, Q, Gap, Per_Q, Rest, E);
      Lemma_Scale (1.0 + E, 2.1, Per_Q);
   end Lemma_Axis_Core;

   procedure Lemma_Frame_Is_Bounded (Frame : Frame_Vector_Array; Axis : Axis_Name)
   with
     Ghost,
     Pre  => Is_Axis_Bounded (Frame, Axis),
     Post =>
       abs R (Frame (0) (Axis)) <= Frame_Cap
       and then abs R (Frame (1) (Axis)) <= Frame_Cap
       and then abs R (Frame (2) (Axis)) <= Frame_Cap
       and then abs R (Frame (3) (Axis)) <= Frame_Cap;
   --  Is_Axis_Bounded over the reals.

   procedure Lemma_Frame_Is_Bounded (Frame : Frame_Vector_Array; Axis : Axis_Name) is
   begin
      pragma Assert (abs Frame (0) (Axis) <= Magnitude_Limit);
      pragma Assert (abs Frame (1) (Axis) <= Magnitude_Limit);
      pragma Assert (abs Frame (2) (Axis) <= Magnitude_Limit);
      pragma Assert (abs Frame (3) (Axis) <= Magnitude_Limit);
      pragma Assert (abs R (Frame (0) (Axis)) <= Frame_Cap);
      pragma Assert (abs R (Frame (1) (Axis)) <= Frame_Cap);
      pragma Assert (abs R (Frame (2) (Axis)) <= Frame_Cap);
      pragma Assert (abs R (Frame (3) (Axis)) <= Frame_Cap);
   end Lemma_Frame_Is_Bounded;

   procedure Lemma_Node_Is_Bounded (Delta_Y : Component_Values; Node : Ideal_Node)
   with
     Ghost,
     Pre  => Is_Node_Bounded (Delta_Y, Node),
     Post =>
       R (Node.Ideal_Q) >= 1.0
       and then R (Node.Ideal_Allowance) >= 0.0
       and then R (Delta_Y (1)) >= 0.0
       and then R (Delta_Y (2)) >= 0.0
       and then R (Delta_Y (3)) >= 0.0;
   --  What the comparison needs of Is_Node_Bounded, over the reals.

   procedure Lemma_Node_Is_Bounded (Delta_Y : Component_Values; Node : Ideal_Node) is
   begin
      pragma Assert (Node.Ideal_Q >= 1.0);
      pragma Assert (Node.Ideal_Allowance >= 0.0);
      pragma Assert (Delta_Y (1) >= 0.0 and then Delta_Y (2) >= 0.0 and then Delta_Y (3) >= 0.0);
   end Lemma_Node_Is_Bounded;

   procedure Lemma_Result_Is_Nonnegative (Result : Ideal_Axis)
   with
     Ghost,
     Pre  => Result.Numerator_Size >= 0.0 and then Result.Numerator_Slip >= 0.0,
     Post => R (Result.Numerator_Size) >= 0.0 and then R (Result.Numerator_Slip) >= 0.0;

   procedure Lemma_Result_Is_Nonnegative (Result : Ideal_Axis) is null;

   procedure Lemma_Allowance_Is_Small (Node : Ideal_Node)
   with
     Ghost,
     Pre  => Node.Ideal_Allowance >= 0.0 and then Node.Ideal_Allowance <= 1.0,
     Post => R (Node.Ideal_Allowance) >= 0.0 and then R (Node.Ideal_Allowance) <= 1.0;

   procedure Lemma_Allowance_Is_Small (Node : Ideal_Node) is null;

   procedure Lemma_Exact_Tangent
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; X : Real)
   with
     Ghost,
     Post =>
       Numerator (Coefficients, 0, X)
       = 1.0
         - Chart_Value (Coefficients, 1, X) * Chart_Value (Coefficients, 1, X)
         - (Chart_Value (Coefficients, 2, X) * Chart_Value (Coefficients, 2, X)
            + Chart_Value (Coefficients, 3, X) * Chart_Value (Coefficients, 3, X))
       and then
         Denominator (Coefficients, X)
         = 1.0
           + Chart_Value (Coefficients, 1, X) * Chart_Value (Coefficients, 1, X)
           + (Chart_Value (Coefficients, 2, X) * Chart_Value (Coefficients, 2, X)
              + Chart_Value (Coefficients, 3, X) * Chart_Value (Coefficients, 3, X))
       and then
         Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, X)
         = Numerator (Coefficients, 0, X) * R (Frame (0) (Axis))
           + (2.0 * Chart_Value (Coefficients, 1, X)) * R (Frame (1) (Axis))
           + (2.0 * Chart_Value (Coefficients, 2, X)) * R (Frame (2) (Axis))
           + (2.0 * Chart_Value (Coefficients, 3, X)) * R (Frame (3) (Axis))
       and then
         Model.Ideal_Tangent (Frame, Coefficients, Axis, X) * Denominator (Coefficients, X)
         = Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, X);
   --  The definitions of the exact numerator, denominator and tangent, written out.

   procedure Lemma_Exact_Tangent
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; X : Real) is
   begin
      Lemma_Division
        (Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, X), Denominator (Coefficients, X));
   end Lemma_Exact_Tangent;

   function Gap_Allowance
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; E : Real) return Real
   is (abs R (Frame (0) (Axis)) * E + abs R (Frame (1) (Axis)) * (2.0 * (R (Delta_Y (1)) + Chart_Dust))
       + abs R (Frame (2) (Axis)) * (2.0 * (R (Delta_Y (2)) + Chart_Dust))
       + abs R (Frame (3) (Axis)) * (2.0 * (R (Delta_Y (3)) + Chart_Dust)))
   with Ghost;
   --  What the errors of the node contribute to the error of the numerator: E for the first local numerator,
   --  and the chart allowances with their dust for the others.

   procedure Lemma_General_Axis
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Axis         : Axis_Name;
      Delta_Y      : Component_Values;
      Node         : Ideal_Node;
      X, E         : Real)
   with
     Ghost,
     Pre  =>
       Is_Axis_Bounded (Frame, Axis)
       and then Is_Node_Bounded (Delta_Y, Node)
       and then E >= 0.0
       and then E <= Error_Cap
       and then
         (for all Component in Chart_Component_Index =>
            abs (R (Node.Y (Component)) - Chart_Value (Coefficients, Component, X))
            <= R (Delta_Y (Component)) + Chart_Dust)
       and then abs (R (Node.Ideal_Q) - Denominator (Coefficients, X)) <= E
       and then abs (R (Node.Local_0) - Numerator (Coefficients, 0, X)) <= E,
     Post =>
       Partial_Size (Frame, Axis, Node, 3) >= 0.0
       and then Partial_Slip (Frame, Axis, Delta_Y, Node, 3) >= 0.0
       and then
         abs R (At_Axis (Frame, Axis, Delta_Y, Node).Ideal)
         <= Ideal_Scale + 1.1 * Partial_Slip (Frame, Axis, Delta_Y, Node, 3)
       and then
         abs (R (At_Axis (Frame, Axis, Delta_Y, Node).Ideal) - Model.Ideal_Tangent (Frame, Coefficients, Axis, X))
         <= Gap_Allowance (Frame, Axis, Delta_Y, E) + (Five_Steps * Partial_Size (Frame, Axis, Node, 3) + Seven_Dusts)
            + 2.1 * (Quotient_Factor * abs R (At_Axis (Frame, Axis, Delta_Y, Node).Ideal) + Quotient_Dust)
            + abs R (At_Axis (Frame, Axis, Delta_Y, Node).Ideal) * E
       and then abs R (Frame (0) (Axis)) <= Frame_Cap
       and then abs R (Frame (1) (Axis)) <= Frame_Cap
       and then abs R (Frame (2) (Axis)) <= Frame_Cap
       and then abs R (Frame (3) (Axis)) <= Frame_Cap
       and then R (Delta_Y (1)) >= 0.0
       and then R (Delta_Y (2)) >= 0.0
       and then R (Delta_Y (3)) >= 0.0
       and then R (At_Axis (Frame, Axis, Delta_Y, Node).Numerator_Size) >= 0.0
       and then R (At_Axis (Frame, Axis, Delta_Y, Node).Numerator_Slip) >= 0.0
       and then
         Is_Axis_Rounded
           (Frame,
            Axis,
            Delta_Y,
            Node,
            At_Axis (Frame, Axis, Delta_Y, Node).Ideal,
            At_Axis (Frame, Axis, Delta_Y, Node).Numerator_Size,
            At_Axis (Frame, Axis, Delta_Y, Node).Numerator_Slip);
   --  Lemma_Axis_Core for the stored data and At_Axis's result, whatever bound E the node's denominator and
   --  first numerator meet.

   procedure Lemma_General_Axis
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Axis         : Axis_Name;
      Delta_Y      : Component_Values;
      Node         : Ideal_Node;
      X, E         : Real)
   is
      Result : constant Ideal_Axis := At_Axis (Frame, Axis, Delta_Y, Node);
      C1     : constant Real := Chart_Value (Coefficients, 1, X);
      C2     : constant Real := Chart_Value (Coefficients, 2, X);
      C3     : constant Real := Chart_Value (Coefficients, 3, X);
      N0     : constant Real := Numerator (Coefficients, 0, X);
      Dt     : constant Real := Denominator (Coefficients, X);
      P      : constant Real := Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, X);
   begin
      Lemma_Frame_Is_Bounded (Frame, Axis);
      Lemma_Node_Is_Bounded (Delta_Y, Node);
      Lemma_Result_Is_Nonnegative (Result);
      Lemma_Exact_Tangent (Frame, Coefficients, Axis, X);
      pragma Assert (abs (R (Node.Y (1)) - C1) <= R (Delta_Y (1)) + Chart_Dust);
      pragma Assert (abs (R (Node.Y (2)) - C2) <= R (Delta_Y (2)) + Chart_Dust);
      pragma Assert (abs (R (Node.Y (3)) - C3) <= R (Delta_Y (3)) + Chart_Dust);
      Axis_Tracking.Lemma_Rounded
        (Frame, Axis, Delta_Y, Node, Result.Ideal, Result.Numerator_Size, Result.Numerator_Slip);
      Lemma_Axis_Core
        (F0 => R (Frame (0) (Axis)),
         F1 => R (Frame (1) (Axis)),
         F2 => R (Frame (2) (Axis)),
         F3 => R (Frame (3) (Axis)),
         L0 => R (Node.Local_0),
         Q  => R (Node.Ideal_Q),
         A  => R (Node.Ideal_Allowance),
         E  => E,
         Y1 => R (Node.Y (1)),
         Y2 => R (Node.Y (2)),
         Y3 => R (Node.Y (3)),
         D1 => R (Delta_Y (1)),
         D2 => R (Delta_Y (2)),
         D3 => R (Delta_Y (3)),
         C1 => C1,
         C2 => C2,
         C3 => C3,
         N0 => N0,
         Dt => Dt,
         P  => P,
         T  => Model.Ideal_Tangent (Frame, Coefficients, Axis, X),
         I  => R (Result.Ideal),
         PN => Partial_Numerator (Frame, Axis, Node, 3),
         PZ => Partial_Size (Frame, Axis, Node, 3),
         PS => Partial_Slip (Frame, Axis, Delta_Y, Node, 3));
   end Lemma_General_Axis;

   procedure Lemma_Tight_Finish (W0, W1, W2, W3, A, D1, D2, D3, Error, Size, Slip, Extent, PZ, PS : Real)
   with
     Ghost,
     Pre  =>
       W0 >= 0.0
       and then W0 <= Frame_Cap
       and then W1 >= 0.0
       and then W1 <= Frame_Cap
       and then W2 >= 0.0
       and then W2 <= Frame_Cap
       and then W3 >= 0.0
       and then W3 <= Frame_Cap
       and then A >= 0.0
       and then D1 >= 0.0
       and then D2 >= 0.0
       and then D3 >= 0.0
       and then Size >= 0.0
       and then Slip >= 0.0
       and then Extent >= 0.0
       and then PZ >= 0.0
       and then PS = W0 * A + W1 * (2.0 * D1) + W2 * (2.0 * D2) + W3 * (2.0 * D3)
       and then Size <= Ideal_Scale + 1.1 * PS
       and then abs (Extent - PZ) <= Five_Steps * PZ + Seven_Dusts
       and then abs (Slip - PS) <= Five_Steps * PS + Seven_Dusts
       and then
         Error
         <= (W0 * (A * (1.0 + Node_Slack) + Denominator_Dust) + W1 * (2.0 * (D1 + Chart_Dust))
             + W2 * (2.0 * (D2 + Chart_Dust))
             + W3 * (2.0 * (D3 + Chart_Dust)))
            + (Five_Steps * PZ + Seven_Dusts)
            + 2.1 * (Quotient_Factor * Size + Quotient_Dust)
            + Size * (A * (1.0 + Node_Slack) + Denominator_Dust),
     Post =>
       Error
       <= (Slip + Small_Allowance_Real * Extent + Size * (A + Small_Allowance_Real)) * (1.0 + Slack) + Tangent_Dust;
   --  From Lemma_Axis_Core's bound to the conclusion of Lemma_Axis, for a node which meets Is_Node_Tight.
   --  The slip and the node's allowance appear with the factor 1 + Node_Slack, and the computed slip may be
   --  6u below the exact one: together still within 1 + Slack.

   procedure Lemma_Tight_Finish (W0, W1, W2, W3, A, D1, D2, D3, Error, Size, Slip, Extent, PZ, PS : Real) is
   begin
      Lemma_Sign (W0, A);
      Lemma_Sign (W1, 2.0 * D1);
      Lemma_Sign (W2, 2.0 * D2);
      Lemma_Sign (W3, 2.0 * D3);
      Lemma_Sign (Size, A);
      pragma
        Assert (W0 * (A * (1.0 + Node_Slack) + Denominator_Dust) = Node_Factor * (W0 * A) + Denominator_Dust * W0);
      pragma Assert (W1 * (2.0 * (D1 + Chart_Dust)) = W1 * (2.0 * D1) + 2.0 * Chart_Dust * W1);
      pragma Assert (W2 * (2.0 * (D2 + Chart_Dust)) = W2 * (2.0 * D2) + 2.0 * Chart_Dust * W2);
      pragma Assert (W3 * (2.0 * (D3 + Chart_Dust)) = W3 * (2.0 * D3) + 2.0 * Chart_Dust * W3);
      pragma
        Assert
          (Size * (A * (1.0 + Node_Slack) + Denominator_Dust) = Node_Factor * (Size * A) + Denominator_Dust * Size);
      pragma Assert (PZ <= Extent + 6.0 * Epsilon * Extent + 8.0 * Axis_Dust);
      pragma Assert (PS <= Slip + 6.0 * Epsilon * Slip + 8.0 * Axis_Dust);
      pragma Assert (Denominator_Dust * Size <= Denominator_Dust * Ideal_Scale + 1.1 * Denominator_Dust * PS);
      pragma
        Assert
          (Error
           <= (Node_Factor + 1.1 * Denominator_Dust) * PS + Five_Steps * PZ + 2.1 * Quotient_Factor * Size
              + Node_Factor * (Size * A)
              + (Gap_Dust + Seven_Dusts + 2.1 * Quotient_Dust + Denominator_Dust * Ideal_Scale));
      pragma
        Assert
          (Error
           <= (Slip + Small_Allowance_Real * Extent + (Size * A + Small_Allowance_Real * Size))
              * (1.0 + Slack)
              + Tangent_Dust);
      Lemma_Distribute (Size, A, Small_Allowance_Real);
   end Lemma_Tight_Finish;

   procedure Lemma_Axis
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Axis         : Axis_Name;
      Delta_Y      : Component_Values;
      Node         : Ideal_Node;
      X            : Real)
   is
      Result : constant Ideal_Axis := At_Axis (Frame, Axis, Delta_Y, Node);
      A      : constant Real := R (Node.Ideal_Allowance);
      E      : constant Real := A * (1.0 + Node_Slack) + Denominator_Dust;
   begin
      Lemma_Allowance_Is_Small (Node);
      pragma Assert (A >= 0.0 and then A <= 1.0);
      pragma Assert (E >= 0.0 and then E <= Error_Cap);
      Lemma_General_Axis (Frame, Coefficients, Axis, Delta_Y, Node, X, E);
      Lemma_Tight_Finish
        (W0     => abs R (Frame (0) (Axis)),
         W1     => abs R (Frame (1) (Axis)),
         W2     => abs R (Frame (2) (Axis)),
         W3     => abs R (Frame (3) (Axis)),
         A      => A,
         D1     => R (Delta_Y (1)),
         D2     => R (Delta_Y (2)),
         D3     => R (Delta_Y (3)),
         Error  => abs (R (Result.Ideal) - Model.Ideal_Tangent (Frame, Coefficients, Axis, X)),
         Size   => abs R (Result.Ideal),
         Slip   => R (Result.Numerator_Slip),
         Extent => R (Result.Numerator_Size),
         PZ     => Partial_Size (Frame, Axis, Node, 3),
         PS     => Partial_Slip (Frame, Axis, Delta_Y, Node, 3));
   end Lemma_Axis;

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Ideal_Side;
