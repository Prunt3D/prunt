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

--  The nodes at which the sampled tangent certificate compares the ideal tangent with the retained one, and the
--  evidence that sampling there is enough.
--
--  A polynomial of degree below the number of nodes is its own interpolant there, so on [0, 1] its magnitude is at
--  most the Lebesgue constant of the nodes times its largest magnitude at a node. The nodes are Chebyshev nodes as
--  the library's cosine rounds them, so their Lebesgue constant is not a known number. Certify bounds it for the
--  nodes which were actually built, by enclosing the Lebesgue function on a subdivision of [0, 1].

with Prunt.Motion_Planner.Stereographic_Kernel.Bernstein.Binomial_Form;
with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
use Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
with Prunt.Motion_Planner.Stereographic_Kernel.Polynomials.Interpolation;
with Prunt.Motion_Planner.Reals;
use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes
  with SPARK_Mode => On
is
   pragma Elaborate_Body;
   --  Table, a child, calls Build and Certify while it is elaborated.

   use type Prunt.Motion_Planner.Reals.Real;

   Sample_Count : constant := 33;
   --  A polynomial of degree at most thirty-two is determined by this many values.

   subtype Sample_Index is Positive range 1 .. Sample_Count;

   Lebesgue_Bound : constant Dimensionless := 3.5;
   --  Upper bound on the Lebesgue constant of the nodes, which is 3.1885 for thirty-three Chebyshev nodes of the
   --  first kind.

   Lebesgue_Bound_Real : constant := 3.5;

   type Basis_Values is array (Chart_Coefficient_Index) of Dimensionless;

   type Sample is record
      Node : Dimensionless;
      --  A Chebyshev node of [0, 1] in the chart coordinate.

      Complement : Dimensionless;
      --  1 - Node, rounded.

      Basis : Basis_Values;
      --  The degree-eight Bernstein basis at Node.
   end record;

   type Sample_Array is array (Sample_Index) of Sample;

   Largest_Basis_Value : constant Dimensionless := 70.0;
   --  The largest binomial coefficient of degree eight.

   function Is_In_Range (Value : Sample) return Boolean
   is (Value.Node >= 0.0
       and then Value.Node <= 1.0
       and then Value.Complement >= 0.0
       and then Value.Complement <= 1.0
       and then Value.Node + Value.Complement >= 0.5
       and then
         (for all K in Chart_Coefficient_Index =>
            Value.Basis (K) >= 0.0 and then Value.Basis (K) <= Largest_Basis_Value));
   --  What keeps the arithmetic at a node finite, whatever the cosine returned.

   Node_Margin : constant Dimensionless := 2.0**(-20);

   function Has_Margins (Value : Sample) return Boolean
   is (Value.Node >= Node_Margin and then Value.Node <= 1.0 - Node_Margin);
   --  Whether the node is far enough inside [0, 1] for no basis value to underflow. The outermost Chebyshev
   --  nodes are near 0.0006 from the ends.

   Basis_Relative_Error : constant := 2.0**(-48);

   function Is_Accurate (Value : Sample) return Boolean
   is (Is_In_Range (Value)
       and then Has_Margins (Value)
       and then abs (R (Value.Complement) - (1.0 - R (Value.Node))) <= Epsilon * (1.0 - R (Value.Node)) + Eta
       and then
         (for all K in Chart_Coefficient_Index =>
            abs (R (Value.Basis (K)) - Bernstein.Binomial_Form.Basis (Fixed_Chart_Degree, K, R (Value.Node)))
            <= Basis_Relative_Error * Bernstein.Binomial_Form.Basis (Fixed_Chart_Degree, K, R (Value.Node))))
   with Ghost;
   --  Whether the stored complement and basis values are those of the node, to within the rounding of the few
   --  operations which compute them.

   Complement_Relative_Error : constant := 2.0**(-53) + 2.0**(-1055);
   --  Epsilon, and what Eta is of a complement no smaller than Node_Margin.

   procedure Lemma_Complement_Is_Relative (Value : Sample)
   with
     Ghost,
     Pre  => Is_Accurate (Value),
     Post =>
       1.0 - R (Value.Node) > 0.0
       and then
         abs (R (Value.Complement) - (1.0 - R (Value.Node))) <= Complement_Relative_Error * (1.0 - R (Value.Node));
   --  The error of the stored complement as a purely relative one. Is_Accurate states it as one rounded
   --  subtraction, with the absolute Eta which the rounding lemmas carry for every operation; inside the margins
   --  that Eta is a negligible share of the complement.

   function Build return Sample_Array
   with
     Post =>
       (for all Index in Sample_Index =>
          Is_In_Range (Build'Result (Index))
          and then (if Has_Margins (Build'Result (Index)) then Is_Accurate (Build'Result (Index))));
   --  The Chebyshev nodes of the first kind on [0, 1], in increasing order, with the basis values there.

   function Certify (Samples : Sample_Array) return Boolean
   with
     Post =>
       (if Certify'Result
        then
          (for all Index in Sample_Index => Is_In_Range (Samples (Index)) and then Has_Margins (Samples (Index)))
          and then (for all Index in 1 .. Sample_Count - 1 => Samples (Index).Node < Samples (Index + 1).Node));
   --  Whether the nodes are strictly increasing, inside their margins, and their Lebesgue function is at most
   --  Lebesgue_Bound everywhere on [0, 1].

   function Node_Reals (Samples : Sample_Array) return Polynomials.Interpolation.Node_Array
   with
     Ghost,
     Post =>
       Node_Reals'Result'First = 1
       and then Node_Reals'Result'Last = Sample_Count
       and then (for all Index in Sample_Index => Node_Reals'Result (Index) = R (Samples (Index).Node));
   --  The nodes as the real numbers which they denote.

   procedure Lemma_Lebesgue (Samples : Sample_Array; X : Real)
   with
     Ghost,
     Pre  => Certify (Samples) and then 0.0 <= X and then X <= 1.0,
     Post =>
       Polynomials.Interpolation.Is_Increasing (Node_Reals (Samples))
       and then Polynomials.Interpolation.Lebesgue_Sum (Node_Reals (Samples), X) <= Lebesgue_Bound_Real;
   --  What Certify establishes: at every point of [0, 1], the magnitudes of the Lagrange basis polynomials of
   --  the nodes sum to at most Lebesgue_Bound.

end Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes;
