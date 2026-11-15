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

--  The certificate that the ideal tangent never travels backwards along a direction: its numerator
--
--     Frame₀·(1 - Y·Y) + 2·Σ꜀Frame꜀·Y꜀,
--
--  projected onto the direction, is nonnegative. The numerator is formed directly from the authoritative
--  degree-eight Bernstein chart. Multiplication by the degree-eight constant-one polynomial both elevates Y to
--  degree sixteen and keeps every conversion outward-rounded.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

package Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Projection
  with SPARK_Mode => On
is

   function Is_Certified
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale) return Boolean;
   --  Whether the projected tangent numerator is proved nonnegative.

   procedure Lemma_Certified
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale; V : Real)
   with
     Ghost,
     Pre  => Is_Certified (Frame, Coefficients, Direction) and then 0.0 <= V and then V <= 1.0,
     Post => Projected_Numerator (Frame, Coefficients, Direction, V) >= 0.0;
   --  A certified projection is nonnegative everywhere on the curve.

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Projection;
