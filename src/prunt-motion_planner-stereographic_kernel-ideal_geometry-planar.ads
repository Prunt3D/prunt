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

--  The planar no-bulge certificate, for a chart whose last two coordinates vanish identically.
--
--  For the planar inverse-stereographic tangent
--
--     S(Y) = ((1 - Y²), 2Y)/(1 + Y²),
--
--  the two endpoint-cone determinants factor as
--
--     (Y - Yₛ)·(1 + Yₛ·Y),   (Yꜰ - Y)·(1 + Yꜰ·Y).
--
--  Every Bernstein chart has the exact endpoint factors Y - Yₛ = V·Qₛ and Yꜰ - Y = (1 - V)·Qꜰ. Structurally flat
--  endpoint controls expose fourth-order factors instead. The strongest available quotient is formed directly in
--  Bernstein form, and the remaining products are certified with outward-rounded intervals.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

package Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Planar
  with SPARK_Mode => On
is

   function Is_Certified (Coefficients : Bernstein_Chart) return Boolean
   with Pre => Is_Structurally_Planar (Coefficients);
   --  Whether both oriented cone determinants, with their endpoint roots divided out, are proved nonnegative.

   procedure Lemma_Certified (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Pre  =>
       Is_Structurally_Planar (Coefficients) and then Is_Certified (Coefficients) and then 0.0 <= V and then V <= 1.0,
     Post => Is_In_Cone (Coefficients, V);
   --  A certified planar chart keeps its tangent in the cone of its endpoint tangents.

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Planar;
