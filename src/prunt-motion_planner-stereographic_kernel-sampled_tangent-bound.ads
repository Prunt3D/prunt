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

--  The sampled tangent certificate: the largest nodal bounds over the sample nodes, and the bound on the tangent
--  error over the complete curve which follows from them.

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;
with Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes.Table;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Model;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Nodes;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Poles;

package Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Bound
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   Lebesgue : constant := Sample_Nodes.Lebesgue_Bound_Real;

   function Is_Combined (Active : Axis_Flags; Value : Result) return Boolean
   is (Value.Error_Bound >= 0.0
       and then Value.Error_Bound < Dimensionless'Last
       and then Value.Largest_E >= 0.0
       and then Lebesgue * R (Value.Largest_E) < 1.0
       and then (for all Axis in Axis_Name => Value.Largest_G (Axis) >= 0.0)
       and then
         (if Active (X_Axis)
          then Lebesgue * R (Value.Largest_G (X_Axis)) * (Lebesgue * R (Value.Largest_G (X_Axis)))
          else 0.0)
         + (if Active (Y_Axis)
            then Lebesgue * R (Value.Largest_G (Y_Axis)) * (Lebesgue * R (Value.Largest_G (Y_Axis)))
            else 0.0)
         + (if Active (Z_Axis)
            then Lebesgue * R (Value.Largest_G (Z_Axis)) * (Lebesgue * R (Value.Largest_G (Z_Axis)))
            else 0.0)
         + (if Active (E_Axis)
            then Lebesgue * R (Value.Largest_G (E_Axis)) * (Lebesgue * R (Value.Largest_G (E_Axis)))
            else 0.0)
         <= R (Value.Error_Bound)
            * R (Value.Error_Bound)
            * ((1.0 - Lebesgue * R (Value.Largest_E)) * (1.0 - Lebesgue * R (Value.Largest_E))))
   with Ghost;
   --  Whether Error_Bound follows from the largest nodal bounds: with E and G those bounds and Λ the Lebesgue
   --  bound, Λ·E < 1 and the squares of Λ·G over the active axes sum to at most (Error_Bound·(1 - Λ·E))².

   function Is_Witnessed
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result) return Boolean
   is (Value.Outcome = Certified
       and then Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Outcome = Certified
       and then
         (for all Index in Sample_Nodes.Sample_Index =>
            Nodes.Bounds_At_Node
              (Frame,
               Coefficients,
               Warp,
               Cache,
               Denominator_Scale,
               Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
               Sample_Nodes.Table.Samples (Index))
              .Good
            and then
              Nodes.Bounds_At_Node
                (Frame,
                 Coefficients,
                 Warp,
                 Cache,
                 Denominator_Scale,
                 Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
                 Sample_Nodes.Table.Samples (Index))
                .Nodal_E
              <= Value.Largest_E
            and then
              (for all Axis in Axis_Name =>
                 (if Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active (Axis)
                  then
                    Nodes.Bounds_At_Node
                      (Frame,
                       Coefficients,
                       Warp,
                       Cache,
                       Denominator_Scale,
                       Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale),
                       Sample_Nodes.Table.Samples (Index))
                      .Nodal_G (Axis)
                    <= Value.Largest_G (Axis))))
       and then Is_Combined (Poles.Prepare (Frame, Coefficients, Warp, Cache, Denominator_Scale).Active, Value))
   with Ghost;
   --  Whether Value is a certificate for these inputs: every sample node gave nodal bounds, none above the
   --  largest ones which Value records, and its Error_Bound follows from those.

   function Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless) return Result
   with
     Post =>
       (if Error'Result.Outcome = Certified
        then Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Error'Result));
   --  Bound the Euclidean difference between the ideal tangent of the chart and the retained tangent of Cache
   --  over the complete curve. Denominator_Scale is the ideal denominator at the start of the chart.

   procedure Lemma_Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Value             : Result;
      V                 : Real)
   with
     Ghost,
     Pre  =>
       Is_Witnessed (Frame, Coefficients, Warp, Cache, Denominator_Scale, Value)
       and then Sample_Nodes.Table.Is_Certified
       and then 0.0 <= V
       and then V <= 1.0,
     Post =>
       Warp > 0.0
       and then Has_Layout (Cache)
       and then Poles_Are_Off_The_Curve (Cache)
       and then
         Model.Squared_Tangent_Error (Frame, Coefficients, Warp, Cache, V)
         <= R (Value.Error_Bound) * R (Value.Error_Bound);
   --  What a certificate guarantees, provided the sample nodes passed their own check: at every chart
   --  coordinate of the curve, the retained tangent at the same point of the curve is within Error_Bound of
   --  the ideal tangent, and no pole of the retained tangent lies on the curve.

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Bound;
