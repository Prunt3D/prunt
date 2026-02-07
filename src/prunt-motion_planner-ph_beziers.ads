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

pragma Extensions_Allowed (On);

package Prunt.Motion_Planner.PH_Beziers is

   type PH_Bezier is private;
   --  C⁴ continuous Pythagorean-hodograph curve as specified in https://doi.org/10.1007/s00170-022-09463-y.

   subtype Curve_Parameter is Dimensionless range 0.0 .. 1.0;

   function Distance_At_T (Bez : PH_Bezier; T : Curve_Parameter) return Length;
   --  Returns the distance along the curve at T. T = 0 will return the start point, T = 1 will return the end point,
   --  and T=0.5 will return the midpoint. This function is monotonic but is not linear.

   function T_At_Distance (Bez : PH_Bezier; Distance : Length) return Curve_Parameter;
   --  Returns a value T where Distance_At_T (Bez, T) = Distance.

   function Inverse_Curvature (Bez : PH_Bezier) return Length;
   --  Returns the inverse of the curvature at the midpoint, which is the point with the highest curvature.

   function Midpoint (Bez : PH_Bezier) return Scaled_Position;
   --  Returns the midpoint of the curve. Equivalent to Point_At_T (Bez, 0.5).

   function Point_At_T (Bez : PH_Bezier; T : Curve_Parameter) return Scaled_Position;
   --  Return the point on the curve at parameter T.

   --  function Tangent_At_T (Bez : PH_Bezier; T : Curve_Parameter) return Scaled_Position_Offset;
   --
   --  TODO: Need to validate that this function is correct. LLM says it is 1/15 of what it should be and that logic
   --  looks sound at a glance but proper investigation is required. We do not actually use this so it is not a high
   --  priority, it used to be part of pressure advance shaping.
   --
   --  Return a vector tangent to the curve at parameter T. This vector is not normalised and will be zero is the
   --  curve has no length.

   function Point_At_Distance (Bez : PH_Bezier; Distance : Length) return Scaled_Position;
   --  Return the point that is a given distance along the curve. Equivalent to
   --  Point_At_T (Bez, T_At_Distance (Bez, Distance)). This vector is not normalised and will be zero is the curve has
   --  no length.

   --  function Tangent_At_Distance (Bez : PH_Bezier; Distance : Length) return Scaled_Position_Offset;
   --
   --  TODO: Relies on Tangent_At_T, see related TODO comment.
   --
   --  Return a vector tangent to the curve at at the given distance along the curve. Equivalent to
   --  Tangent_At_T (Bez, T_At_Distance (Bez, Distance)).

   function Create_Bezier (Start, Corner, Finish : Scaled_Position; Deviation_Limit : Length) return PH_Bezier;
   --  Creates a C4 continuous Pythagorean-hodograph (PH) curve that smoothly blends a corner.
   --
   --  The curve is generated based on the method described in https://doi.org/10.1007/s00170-022-09463-y, but is
   --  simplified to always be symmetrical.
   --
   --  Start, Corner, and Finish define the two lines that form the corner to be blended. The generated curve
   --  will be tangent to the lines Start - Corner and Finish - Corner at its endpoints.
   --
   --  Deviation_Limit constraints how far the midpoint of the curve can be from Corner. The start and end points
   --  of the curve lie on the segments between Start and Corner, and Finish and Corner respectively. They are
   --  positioned to not go beyond the halfway point of these segments.
private

   type Control_Points_Index is range 0 .. 15;
   type PH_Control_Points is array (Control_Points_Index) of Scaled_Position;

   function Point_At_T_V2 (Bez : PH_Bezier; T : Curve_Parameter) return Scaled_Position;
   --  This method is slower than Point_At_T on most CPUs, but may be useful if this code is ported to a GPU or
   --  FPGA. It may also be faster for cases where T is known at compile time, but I am not aware of any methods to
   --  detect that with GCC.
   --
   --  The details of this implementation are here:
   --  https://github.com/Prunt3D/prunt_notebooks/blob/master/Pythagorean-Hodograph%20Splines.ipynb

   type PH_Bezier is record
      Control_Points    : PH_Control_Points;
      Inverse_Curvature : Length;
   end record;

end Prunt.Motion_Planner.PH_Beziers;
