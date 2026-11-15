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

--  The spatial no-bulge certificate, for a chart whose endpoint tangents lie in the plane of the first two frame
--  vectors but which may leave that plane in between.
--
--  The frame-plane projection of the tangent numerator is (1 - |Y|², 2·Y₁). With Y₂ = Y₃ = 0 at both ends and
--  W = Y₂² + Y₃², the cone determinants are
--
--     (Y₁ - Yₛ)·(1 + Yₛ·Y₁) + Yₛ·W,   (Yꜰ - Y₁)·(1 + Yꜰ·Y₁) - Yꜰ·W.
--
--  They reduce to the planar factors when W = 0. Every term has an exact endpoint root: Y₁ - Yₛ = Vᵏ·Q and
--  Y₂, Y₃ = Vᵐ·P at the start, and likewise in 1 - V at the finish. The common power is divided out so that a curve
--  which leaves an endpoint along the cone boundary still certifies.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

package Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Spatial
  with SPARK_Mode => On
is

   function Is_Certified (Coefficients : Bernstein_Chart) return Boolean;
   --  Whether both endpoint tangents lie in the frame plane and both oriented cone determinants, with their shared
   --  endpoint roots divided out, are proved nonnegative.

   procedure Lemma_Certified (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Pre  => Is_Certified (Coefficients) and then 0.0 <= V and then V <= 1.0,
     Post => Endpoints_Are_In_Frame_Plane (Coefficients) and then Is_In_Cone (Coefficients, V);
   --  A certified chart keeps the frame-plane part of its tangent in the cone of its endpoint tangents.

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Spatial;
