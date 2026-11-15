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

--  The two nodal bounds of the sampled tangent certificate at one sample node.

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;
with Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Model;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Poles;

package Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Nodes
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   function Bounds_At_Node
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation;
      Sample            : Sample_Nodes.Sample) return Node_Bounds
   with
     Inline,
     Pre  =>
       Poles.Is_Prepared (Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepared)
       and then Sample_Nodes.Is_In_Range (Sample),
     Post =>
       (if Bounds_At_Node'Result.Good
        then
          Bounds_At_Node'Result.Nodal_E >= 0.0
          and then Bounds_At_Node'Result.Nodal_E <= 1.0
          and then
            (for all Axis in Axis_Name =>
               Bounds_At_Node'Result.Nodal_G (Axis) >= 0.0
               and then Bounds_At_Node'Result.Nodal_G (Axis) <= Magnitude_Limit))
       and then
         (if Bounds_At_Node'Result.Good and then Sample_Nodes.Is_Accurate (Sample)
          then
            abs Model.Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, R (Sample.Node))
            <= R (Bounds_At_Node'Result.Nodal_E)
            and then
              (for all Axis in Axis_Name =>
                 (if Prepared.Active (Axis)
                  then
                    abs Model.Numerator_Error
                          (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, R (Sample.Node))
                    <= R (Bounds_At_Node'Result.Nodal_G (Axis)))));
   --  Nodal_E is at least |e| at the node and Nodal_G at least |g| there, on every active axis: the rounding
   --  allowances which they include cover the rounding of everything computed on the way.

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Nodes;
