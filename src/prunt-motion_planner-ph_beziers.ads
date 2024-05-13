-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

private package Prunt.Motion_Planner.PH_Beziers is

   type PH_Bezier is private;
   --  C⁴ continuous Pythagorean-hodograph curve as specified in https://doi.org/10.1007/s00170-022-09463-y.

   subtype Curve_Parameter is Dimensionless range 0.0..1.0;

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
   --  Return the point on the curve at T.

   function Tangent_At_T (Bez : PH_Bezier; T : Curve_Parameter) return Scaled_Position_Offset;
   --  Return a vector tangent to the curve at T. This vector is not normalised and will be zero is the curve has no
   --  length.

   function Point_At_Distance (Bez : PH_Bezier; Distance : Length) return Scaled_Position;
   --  Return the point that is a given distance along the curve. Equivalent to
   --  Point_At_T (Bez, T_At_Distance (Bez, Distance)). This vector is not normalised and will be zero is the curve has
   --  no length.

   function Tangent_At_Distance (Bez : PH_Bezier; Distance : Length) return Scaled_Position_Offset;
   --  Return a vector tangent to the curve at at the given distance along the curve. Equivalent to
   --  Tangent_At_T (Bez, T_At_Distance (Bez, Distance)).

   function Create_Bezier (Start, Corner, Finish : Scaled_Position; Deviation_Limit : Length) return PH_Bezier;
   --  Create a C⁴ continuous Pythagorean-hodograph curve as specified in https://doi.org/10.1007/s00170-022-09463-y.
   --
   --  The curve will always be symmetrical rather than the asymmetrical curves shown in the paper. The distance from
   --  the midpoint to Corner is limited by Deviation_Limit. The start and end of the curve will be on the vectors
   --  between Corner and Start/Finish and will not further away from Corner than the midpoint between Corner and
   --  Start/Finish.

private

   type Control_Points_Index is range 0 .. 15;
   type PH_Control_Points is array (Control_Points_Index) of Scaled_Position;

   type PH_Bezier is record
      Control_Points    : PH_Control_Points;
      Inverse_Curvature : Length;
   end record;

end Prunt.Motion_Planner.PH_Beziers;
