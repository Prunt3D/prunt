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

--  The endpoint correction of the realtime evaluator, and what it adds to the tangent.
--
--  The evaluator integrates the retained tangent from the start point. Its value at the finish, Uncorrected, is
--  within rounding of the requested finish point but need not be it, so the evaluator adds
--
--     H(U)·(Finish - Uncorrected),     H(U) = 462·U⁶ - 1980·U⁷ + 3465·U⁸ - 3080·U⁹ + 1386·U¹⁰ - 252·U¹¹,
--
--  a smootherstep which is zero at the start and one at the finish with five vanishing derivatives at both. With
--  respect to physical distance, which is the curve length times U, this adds H′(U)·(Finish - Uncorrected)/Length
--  to the tangent, where H′(U) = 2772·U⁵·(1 - U)⁵ is at most 693/256, its value at one half. That H′ is the
--  derivative of H is calculus, which is outside what is stated here.

with Prunt.Motion_Planner.Stereographic_Kernel.Intervals;
use Prunt.Motion_Planner.Stereographic_Kernel.Intervals;
with Prunt.Motion_Planner.Reals;
use Prunt.Motion_Planner.Reals;
with Prunt.Motion_Planner.Stereographic_Kernel.Retained_Tangent;
use Prunt.Motion_Planner.Stereographic_Kernel.Retained_Tangent;

package Prunt.Motion_Planner.Stereographic_Kernel.Finish_Correction
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   Largest_Rate : constant := 693.0 / 256.0;

   function Rate (U : Real) return Real
   is (2772.0 * (U * U * U * U * U) * ((1.0 - U) * (1.0 - U) * (1.0 - U) * (1.0 - U) * (1.0 - U)))
   with Ghost;
   --  H′(U).

   procedure Lemma_Rate (U : Real)
   with Ghost, Pre => 0.0 <= U and then U <= 1.0, Post => 0.0 <= Rate (U) and then Rate (U) <= Largest_Rate;
   --  On the curve the correction's rate is between zero and 693/256.

   function Correction_Interval (Finish, Uncorrected : Dimensionless) return Interval
   with
     Post =>
       (if Correction_Interval'Result.Valid
        then
          R (Correction_Interval'Result.Lower) <= R (Finish) - R (Uncorrected)
          and then R (Finish) - R (Uncorrected) <= R (Correction_Interval'Result.Upper));
   --  Enclose Finish - Uncorrected for one axis without rounding two potentially extreme coordinates into one.

   function Is_Total_Bound
     (Tangent_Error : Dimensionless;
      Finish        : Dimensionless_Axis_Vector;
      Uncorrected   : Dimensionless_Axis_Vector;
      Length_Value  : Dimensionless;
      Total         : Dimensionless) return Boolean
   is (Tangent_Error >= 0.0
       and then Length_Value > 0.0
       and then R (Total) >= R (Tangent_Error)
       and then
         (Largest_Rate * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
         * (Largest_Rate * (R (Finish (X_Axis)) - R (Uncorrected (X_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (Y_Axis)) - R (Uncorrected (Y_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (Z_Axis)) - R (Uncorrected (Z_Axis))) / R (Length_Value))
         + (Largest_Rate * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value))
           * (Largest_Rate * (R (Finish (E_Axis)) - R (Uncorrected (E_Axis))) / R (Length_Value))
         <= (R (Total) - R (Tangent_Error)) * (R (Total) - R (Tangent_Error)))
   with Ghost;
   --  Whether Total is at least Tangent_Error plus the Euclidean norm of the largest tangent which the
   --  correction can add, 693/256·|Finish - Uncorrected|/Length.

   function Total_Tangent_Error_Bound
     (Tangent_Error : Dimensionless;
      Finish        : Dimensionless_Axis_Vector;
      Uncorrected   : Dimensionless_Axis_Vector;
      Length_Value  : Dimensionless) return Dimensionless
   with
     Pre  => Tangent_Error >= 0.0,
     Post =>
       (if Total_Tangent_Error_Bound'Result < Dimensionless'Last
        then
          Total_Tangent_Error_Bound'Result >= 0.0
          and then
            Is_Total_Bound (Tangent_Error, Finish, Uncorrected, Length_Value, Total_Tangent_Error_Bound'Result));
   --  A bound on the distance between the ideal tangent and the tangent of the corrected evaluator, given one,
   --  Tangent_Error, on its distance from the retained tangent. The points and the length are in millimetres.
   --  Dimensionless'Last means that no bound was established.

end Prunt.Motion_Planner.Stereographic_Kernel.Finish_Correction;
