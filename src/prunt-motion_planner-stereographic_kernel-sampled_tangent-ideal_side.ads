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

--  The ideal tangent at one sample node, in ordinary rounded arithmetic, with the allowances which cover its
--  rounding.

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;
with Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Model;

package Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Ideal_Side
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   Largest_Chart_Value : constant Dimensionless := 2.0E15;
   --  No chart coordinate is larger where the ideal denominator is within Largest_Ideal_Denominator.

   Largest_Allowance : constant Dimensionless := 1.0E100;

   type Ideal_Node is record
      Y : Component_Values;
      --  The chart coordinates Ŷ.

      Ideal_Q : Dimensionless;
      --  1 + Ŷ·Ŷ, for the ideal denominator D.

      Local_0 : Dimensionless;
      --  1 - Ŷ·Ŷ, for the first local numerator.

      Ideal_Allowance : Dimensionless;
      --  Covers the error of Ideal_Q and of Local_0.
   end record;

   function Is_Chart_Bounded (Coefficients : Bernstein_Chart) return Boolean
   is (for all Component in Chart_Component_Index =>
         (for all K in Chart_Coefficient_Index => abs Coefficients (K, Component) <= Magnitude_Limit));

   function Is_Chart_Allowance (Coefficients : Bernstein_Chart; Delta_Y : Component_Values) return Boolean
   is (for all Component in Chart_Component_Index =>
         Delta_Y (Component) >= 0.0
         and then Delta_Y (Component) <= Chart_Allowance * Magnitude_Limit
         and then
           (for all K in Chart_Coefficient_Index =>
              R (Delta_Y (Component))
              >= Chart_Allowance_Real * abs R (Coefficients (K, Component)) * (1.0 - Epsilon) - Eta))
   with Ghost;
   --  Delta_Y is Chart_Allowance times the largest coefficient magnitude of each component, as it rounds.

   function Is_Node_Accurate
     (Coefficients : Bernstein_Chart; Delta_Y : Component_Values; Node : Ideal_Node; X : Real) return Boolean
   is ((for all Component in Chart_Component_Index =>
          abs (R (Node.Y (Component)) - Chart_Value (Coefficients, Component, X))
          <= R (Delta_Y (Component)) + Chart_Dust)
       and then
         abs (R (Node.Ideal_Q) - Denominator (Coefficients, X))
         <= R (Node.Ideal_Allowance) * (1.0 + Slack) + Denominator_Dust
       and then
         abs (R (Node.Local_0) - Numerator (Coefficients, 0, X))
         <= R (Node.Ideal_Allowance) * (1.0 + Slack) + Denominator_Dust)
   with Ghost;
   --  The chart coordinates are within their allowances of the chart at X, and the denominator and the first
   --  local numerator within theirs, up to the slack and dust which the final bounds absorb.

   Node_Slack : constant := 2.0**(-48);
   --  The slack with which At_Node itself meets its allowance. It is sixteen times smaller than Slack, which
   --  leaves At_Axis room to round its own allowances low within Slack.

   function Is_Node_Tight
     (Coefficients : Bernstein_Chart; Delta_Y : Component_Values; Node : Ideal_Node; X : Real) return Boolean
   is ((for all Component in Chart_Component_Index =>
          abs (R (Node.Y (Component)) - Chart_Value (Coefficients, Component, X))
          <= R (Delta_Y (Component)) + Chart_Dust)
       and then
         abs (R (Node.Ideal_Q) - Denominator (Coefficients, X))
         <= R (Node.Ideal_Allowance) * (1.0 + Node_Slack) + Denominator_Dust
       and then
         abs (R (Node.Local_0) - Numerator (Coefficients, 0, X))
         <= R (Node.Ideal_Allowance) * (1.0 + Node_Slack) + Denominator_Dust)
   with Ghost;
   --  Is_Node_Accurate with Node_Slack in place of Slack. Lemma_Axis needs this tighter form: Is_Node_Accurate
   --  spends the whole factor 1 + Slack on the node, which leaves nothing to cover At_Axis rounding its own
   --  allowances low. Is_Node_Accurate remains what the denominator's nodal bound uses.

   function At_Node
     (Coefficients : Bernstein_Chart; Delta_Y : Component_Values; Sample : Sample_Nodes.Sample) return Ideal_Node
   with
     Inline,
     Pre  =>
       Is_Chart_Bounded (Coefficients)
       and then Is_Chart_Allowance (Coefficients, Delta_Y)
       and then Sample_Nodes.Is_In_Range (Sample),
     Post =>
       At_Node'Result.Ideal_Q >= 1.0
       and then At_Node'Result.Ideal_Allowance >= 0.0
       and then
         (if At_Node'Result.Ideal_Q <= Largest_Ideal_Denominator
          then
            abs At_Node'Result.Local_0 <= Largest_Ideal_Denominator
            and then At_Node'Result.Ideal_Allowance <= Largest_Allowance
            and then
              (for all Component in Chart_Component_Index => abs At_Node'Result.Y (Component) <= Largest_Chart_Value))
       and then
         (if Sample_Nodes.Is_Accurate (Sample)
            and then At_Node'Result.Ideal_Q <= Largest_Ideal_Denominator
            and then At_Node'Result.Ideal_Allowance <= 1.0
          then
            Is_Node_Accurate (Coefficients, Delta_Y, At_Node'Result, R (Sample.Node))
            and then Is_Node_Tight (Coefficients, Delta_Y, At_Node'Result, R (Sample.Node)));
   --  Evaluate the chart, the ideal denominator and the first local numerator at a node.

   type Ideal_Axis is record
      Ideal : Dimensionless;
      --  One machine-axis component of the ideal tangent.

      Numerator_Slip : Dimensionless;
      --  Covers what the chart allowances contribute to the error of the numerator.

      Numerator_Size : Dimensionless;
      --  The sum of the magnitudes of the terms of the numerator.
   end record;

   function Is_Axis_Bounded (Frame : Frame_Vector_Array; Axis : Axis_Name) return Boolean
   is (for all Component in Frame_Component_Index => abs Frame (Component) (Axis) <= Magnitude_Limit);

   function Is_Node_Bounded (Delta_Y : Component_Values; Node : Ideal_Node) return Boolean
   is (Node.Ideal_Q >= 1.0
       and then Node.Ideal_Q <= Largest_Ideal_Denominator
       and then abs Node.Local_0 <= Largest_Ideal_Denominator
       and then Node.Ideal_Allowance >= 0.0
       and then Node.Ideal_Allowance <= Largest_Allowance
       and then
         (for all Component in Chart_Component_Index =>
            abs Node.Y (Component) <= Largest_Chart_Value
            and then Delta_Y (Component) >= 0.0
            and then Delta_Y (Component) <= Chart_Allowance * Magnitude_Limit));

   Largest_Axis_Value : constant Dimensionless := 1.0E170;

   function At_Axis
     (Frame : Frame_Vector_Array; Axis : Axis_Name; Delta_Y : Component_Values; Node : Ideal_Node) return Ideal_Axis
   with
     Inline,
     Pre  => Is_Axis_Bounded (Frame, Axis) and then Is_Node_Bounded (Delta_Y, Node),
     Post =>
       abs At_Axis'Result.Ideal <= Largest_Axis_Value
       and then At_Axis'Result.Numerator_Slip >= 0.0
       and then At_Axis'Result.Numerator_Slip <= Largest_Axis_Value
       and then At_Axis'Result.Numerator_Size >= 0.0
       and then At_Axis'Result.Numerator_Size <= Largest_Axis_Value;
   --  Combine the local numerators into one machine-axis component of the ideal tangent.

   procedure Lemma_Axis
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Axis         : Axis_Name;
      Delta_Y      : Component_Values;
      Node         : Ideal_Node;
      X            : Real)
   with
     Ghost,
     Pre  =>
       Is_Axis_Bounded (Frame, Axis)
       and then Is_Node_Bounded (Delta_Y, Node)
       and then Node.Ideal_Allowance <= 1.0
       and then Is_Node_Tight (Coefficients, Delta_Y, Node, X),
     Post =>
       abs (R (At_Axis (Frame, Axis, Delta_Y, Node).Ideal) - Model.Ideal_Tangent (Frame, Coefficients, Axis, X))
       <= (R (At_Axis (Frame, Axis, Delta_Y, Node).Numerator_Slip)
           + Small_Allowance_Real * R (At_Axis (Frame, Axis, Delta_Y, Node).Numerator_Size)
           + abs R (At_Axis (Frame, Axis, Delta_Y, Node).Ideal) * (R (Node.Ideal_Allowance) + Small_Allowance_Real))
          * (1.0 + Slack)
          + Tangent_Dust;
   --  The computed component is within its allowances of the ideal tangent at X: the three which the nodal
   --  bound for g includes, up to the slack and dust which that bound absorbs. The hypothesis on the node is
   --  the tighter accuracy which At_Node establishes for its own result.

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Ideal_Side;
