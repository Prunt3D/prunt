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

--  The sampled tangent certificate: a bound on the difference between the ideal tangent of a chart and the
--  retained tangent which the realtime evaluator integrates, over the complete curve, from their values at the
--  sample nodes alone.
--
--  Let V be the chart coordinate, U = V/h(V) with h(V) = W·(1 - V) + V the normalized distance, P/D the ideal
--  tangent with D = 1 + Y·Y ≥ 1, and R(U) = c + Σ r/(U - p) the retained one. Define
--
--     Q(V) = s·Π (V - p·h(V))/(-p·W),
--
--  one linear factor per real pole and one conjugate product per pair, each equal to one at V = 0. Every
--  V - p·h(V) = h(V)·(U - p) is linear in V, so Q has degree at most sixteen, and so has Q·R(U(V)): multiplying
--  r/(U - p) = r·h(V)/(V - p·h(V)) by Q replaces that pole's factor with r·h(V)/(-p·W). Consequently
--
--     g(V) = Q(V)·D(V)·(P(V)/D(V) - R(U(V))),     e(V) = Q(V) - D(V)
--
--  are polynomials of degree at most thirty-two and sixteen. A polynomial of degree below N is its own
--  interpolant at any N nodes, so on [0, 1] it is bounded by Λ times its largest nodal magnitude, where Λ bounds
--  the Lebesgue function of the nodes. Therefore, with E and G the largest nodal magnitudes of e and g,
--
--     Q ≥ D - |e| ≥ 1 - Λ·E > 0,     |P/D - R| = |g|/(Q·D) ≤ Λ·G/(1 - Λ·E)
--
--  over the complete curve. Only sixty-six point evaluations are needed, instead of reconstructing and
--  multiplying the polynomials themselves.
--
--  The nodal values are computed in ordinary rounded arithmetic. With u = 2⁻⁵³, each allowance below exceeds the
--  accumulated error it covers:
--
--    * Basis values B(X) = C(8,k)·Xᵏ·(1 - X)⁸⁻ᵏ take at most seventeen rounded operations, counting the rounded
--      1 - X in every factor: relative error below 20u. The nine-term chart sum adds 9u of the sum of
--      magnitudes, and the basis values sum to one, so |Ŷ - Y| ≤ 2⁻⁴⁶·max|b| per component.
--    * D and the local numerators inherit that through ε_S = Σ δ·(2·|Ŷ| + δ), plus 5u·D for their own
--      operations. The frame combination adds 8u of its sum of magnitudes, and the quotient by D ≥ 1 adds
--      |T|·ε_D and 2u·|T|.
--    * h, a sum of nonnegative terms, and Û = X/h err by at most 6u·U ≤ 6u. The retained tangent is evaluated at
--      the stored number Û, where Û - p is correct to u; each term then errs by at most 16u of its magnitude,
--      and seventeen terms accumulate at most 17u more. Moving from Û to U changes it by at most 6u times the
--      largest derivative on [0, 1], which Σ |r|/d² bounds with d the pole's distance from [0, 1].
--    * A factor |Û - π|²/|π|² of Q errs by at most 8u in its own operations and, through Û, by
--      12u·(|Û - A| + 6u)/|Û - π|² ≤ 13u·max(1, 1/|Û - π|²). The power of h/W adds at most 100u.
--
--  Nothing in the node loop can overflow: the magnitude limits bound every quantity except the running product
--  for Q, which is kept between Smallest_Partial_Product and Largest_Partial_Product, and the certificate fails
--  closed outside that range. The finished product must be near D, so only absurd later factors could bring a
--  partial product back from beyond it, and one small enough to be subnormal would not keep its relative-error
--  allowance.

with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
use Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
with Prunt.Motion_Planner.Stereographic_Kernel.Retained_Tangent;
use Prunt.Motion_Planner.Stereographic_Kernel.Retained_Tangent;

package Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent
  with SPARK_Mode => On
is
   --  The certificate is computed in stages, each a child of this package: Poles gathers the poles, Ideal_Side
   --  and Retained_Side evaluate the two tangents at a node, Nodes turns those into the two nodal bounds, and
   --  Bound takes their maxima over the nodes and concludes. Model defines the functions they are about, and
   --  Structure shows that the sampled ones are polynomials.

   type Status is
     (Certified,
      --  Error_Bound encloses the tangent error on the complete curve.

      Not_Certified,
      --  Finite arithmetic completed, but positivity or accuracy was not established.

      Unsafe
      --  The certificate arithmetic did not produce trustworthy finite data.
     );

   Magnitude_Limit : constant Dimensionless := 1.0E60;
   --  Largest magnitude of a pole, residue, frame component or chart coefficient which is certified.

   Smallest_Warp : constant Dimensionless := 2.0**(-24);
   Largest_Warp  : constant Dimensionless := 2.0**24;
   --  The distance warp factors of every curve lie well inside this range.

   Smallest_Partial_Product : constant Dimensionless := 1.0E-100;
   Largest_Partial_Product  : constant Dimensionless := 1.0E100;
   --  Limits of the running product for Q at a node.

   Largest_Ideal_Denominator  : constant Dimensionless := 1.0E30;
   Largest_Cached_Denominator : constant Dimensionless := 1.0E30;
   --  Limits of D and Q at a node.

   Chart_Allowance  : constant Dimensionless := 2.0**(-46);
   Term_Allowance   : constant Dimensionless := 2.0**(-47);
   Small_Allowance  : constant Dimensionless := 2.0**(-50);
   Factor_Allowance : constant Dimensionless := 2.0**(-45);
   --  The allowances for rounding which the nodal bounds include. The comment above derives them.

   Rounding_Factor : constant Dimensionless := 1.0 + 2.0**(-40);
   --  Covers the rounding of the allowance arithmetic itself, which sums a few dozen nonnegative terms.

   Underflow_Padding : constant Dimensionless := 2.0**(-500);
   --  An underflowing operation errs by less than 2⁻¹⁰⁷⁵ rather than relatively. The magnitude limits keep every
   --  later amplification of such an error far below this padding.

   Largest_Factor_Error : constant Dimensionless := 2.0**(-10);
   --  The largest accumulated relative error of Q at a node which is accepted.

   Largest_Amplification : constant Dimensionless := 2.0**35;
   --  Largest_Factor_Error divided by Factor_Allowance.

   --  The same allowances as real numbers, and the slack with which the stages state their accuracy. A stage
   --  shows that an exact error is at most (1 + Slack) times the allowance which the floating-point code
   --  computes for it, plus a dust of underflow; the final Rounding_Factor and Underflow_Padding cover both.

   Chart_Allowance_Real : constant := 2.0**(-46);
   Small_Allowance_Real : constant := 2.0**(-50);

   Slack : constant := 2.0**(-44);

   Chart_Dust       : constant := 2.0**(-1060);
   Denominator_Dust : constant := 2.0**(-1010);
   Tangent_Dust     : constant := 2.0**(-800);
   Retained_Dust    : constant := 2.0**(-969);

   Factor_Slack : constant := 2.0**(-47);
   --  The relative error of the computed Q at a node is at most this times its amplification.

   Term_Slack : constant := 40.0 * 2.0**(-53);
   --  The computed retained tangent errs by at most this times the sum of the magnitudes of its terms,

   Slope_Slack : constant := 7.0 * 2.0**(-53);
   --  and, for being evaluated at the rounded distance, by at most this times its largest derivative.

   type Axis_Values is array (Axis_Name) of Dimensionless;
   type Axis_Flags is array (Axis_Name) of Boolean;
   type Component_Values is array (Chart_Component_Index) of Dimensionless;
   type Component_Flags is array (Chart_Component_Index) of Boolean;

   type Pole_Term is record
      Real_Part, Imaginary_Part : Dimensionless := 0.0;
      --  Imaginary_Part is zero for a real pole.

      Reciprocal_Norm : Dimensionless := 0.0;
      --  1/p for a real pole and 1/|π|² for a pair.

      Residue_Real, Residue_Imaginary : Axis_Values := [others => 0.0];
      --  A pair's residue is stored doubled, which is exact.
   end record;

   type Pole_Terms is array (Rational_Degree_Slot) of Pole_Term;

   type Preparation is record
      Outcome : Status := Not_Certified;
      --  Certified here means only that the node loop may run.

      Terms : Pole_Terms;
      Count : Natural range 0 .. Maximum_Rational_Degree := 0;
      --  One term per real pole, then one per pair.

      Active : Axis_Flags := [others => False];
      --  An axis takes part unless both tangents vanish on it identically.

      Slope : Axis_Values := [others => 0.0];
      --  Σ |r|/d²: bounds the derivative of the retained tangent on [0, 1].

      Delta_Y : Component_Values := [others => 0.0];
      --  Allowance for the rounding of each chart coordinate at a node.

      Vanishes : Component_Flags := [others => True];
      --  Whether every coefficient of the chart component is exactly zero.
   end record;

   type Node_Bounds is record
      Good : Boolean := False;

      Nodal_E : Dimensionless := 0.0;
      --  At least |e| at the node.

      Nodal_G : Axis_Values := [others => 0.0];
      --  At least |g| at the node, for every active axis.
   end record;

   type Result is record
      Outcome : Status := Not_Certified;

      Error_Bound : Dimensionless := 0.0;
      --  Uniform Euclidean tangent-error bound. Meaningful only when Outcome is Certified.

      Largest_E : Dimensionless := 0.0;
      Largest_G : Axis_Values := [others => 0.0];
      --  The largest nodal bounds, from which Error_Bound follows.
   end record;

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent;
