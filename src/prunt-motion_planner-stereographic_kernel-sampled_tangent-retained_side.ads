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

--  The retained tangent and the cached denominator at one sample node, in ordinary rounded arithmetic.

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;
with Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Model;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Poles;

package Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Retained_Side
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   Largest_Retained_Value : constant Dimensionless := 1.0E185;

   Largest_Amplification_Value : constant Dimensionless := 1.0E62;

   type Retained_Node is record
      Good : Boolean := False;
      --  False if the running product for Q left its limits.

      Retained : Axis_Values := [others => 0.0];
      --  The retained tangent without its constant, on every active axis.

      Retained_Size : Axis_Values := [others => 0.0];
      --  The sum of the magnitudes of its terms.

      Cached_Q : Dimensionless := 0.0;
      --  The cached denominator Q.

      Amplification : Dimensionless := 16.0;
      --  Sixteen plus, for every pole, the larger of one and the reciprocal of its distance from the node
      --  (squared for a pair): how far the rounding of the distance is amplified in Q.
   end record;

   function Is_Node_Accurate
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Node              : Retained_Node;
      X                 : Real) return Boolean
   is (abs (R (Node.Cached_Q) - Model.Cached_Denominator (Cache, Denominator_Scale, Warp, X))
       <= Factor_Slack * R (Node.Amplification) * R (Node.Cached_Q)
       and then
         (for all Axis in Axis_Name =>
            (if Prepared.Active (Axis)
             then
               abs (R (Node.Retained (Axis))
                    + R (Cache.Constant_Tangent (Axis))
                    - Retained_Tangent.Value (Cache, Axis, Model.Distance (Warp, X)))
               <= Term_Slack * R (Node.Retained_Size (Axis))
                  + Slope_Slack * R (Prepared.Slope (Axis))
                  + Retained_Dust)))
   with Ghost, Pre => Has_Layout (Cache);
   --  The computed Q is within a relative Factor_Slack times its amplification of Q at X, and the computed
   --  tangent plus the cached constant within its allowances of the retained tangent at the distance of X.

   function At_Node
     (Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample) return Retained_Node
   with
     Inline,
     Pre  =>
       Prepared.Outcome = Certified
       and then Denominator_Scale > 0.0
       and then Denominator_Scale <= Magnitude_Limit
       and then Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Poles_Are_Off_The_Curve (Cache)
       and then Poles.Is_Cache_Prepared (Cache, Prepared)
       and then Sample_Nodes.Is_In_Range (Sample),
     Post =>
       (if At_Node'Result.Good
        then
          At_Node'Result.Cached_Q > 0.0
          and then
            (if Prepared.Count > 0 or else Denominator_Scale >= Smallest_Partial_Product
             then At_Node'Result.Cached_Q >= Smallest_Partial_Product)
          and then At_Node'Result.Cached_Q <= Largest_Partial_Product
          and then At_Node'Result.Amplification >= 16.0
          and then At_Node'Result.Amplification <= Largest_Amplification_Value
          and then
            (for all Axis in Axis_Name =>
               abs At_Node'Result.Retained (Axis) <= Largest_Retained_Value
               and then At_Node'Result.Retained_Size (Axis) >= 0.0
               and then At_Node'Result.Retained_Size (Axis) <= Largest_Retained_Value))
       and then
         (if At_Node'Result.Good
            and then Sample_Nodes.Is_Accurate (Sample)
            and then At_Node'Result.Amplification <= Largest_Amplification
          then Is_Node_Accurate (Warp, Cache, Denominator_Scale, Prepared, At_Node'Result, R (Sample.Node)));
   --  Evaluate the retained tangent and the cached denominator at the distance of a node. Cache is here for
   --  what the result is stated to approximate: the computation reads the poles which Prepared holds. Without a
   --  pole Q is the denominator scale itself, which nothing here keeps above Smallest_Partial_Product.

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Retained_Side;
