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

private generic
package Prunt.Motion_Planner.Planner.Corner_Blender is

   procedure Run (Block : in out Execution_Block);
   --  Fills Block.Beziers based on the block parameters.
   --
   --  Deviation of the curve path is limited by Chord_Error_Max.
   --
   --  If Shift_Blended_Corners is true then corners that are not within Chord_Error_Max of the work area and are not
   --  outside of the work area will be shifted so that the midpoint of the curve is where the original corner was.
   --  Block.Corners will not be modified. The accuracy for this shifting is controlled by the generic parameter
   --  Corner_Blender_Max_Computational_Error.

private

   pragma Warnings (Off, "use of an anonymous access type allocator");

   type Shifted_Corner_Error_Limits_Type is array (Corners_Index) of Length;
   type Shifted_Corners_Type is array (Corners_Index) of Scaled_Position;

   Shifted_Corner_Error_Limits : access Shifted_Corner_Error_Limits_Type := new Shifted_Corner_Error_Limits_Type;
   Shifted_Corners             : access Shifted_Corners_Type := new Shifted_Corners_Type;

   pragma Warnings (On, "use of an anonymous access type allocator");

   function Sine_Secondary_Angle (Start, Corner, Finish : Scaled_Position) return Dimensionless;
   --  Compute sin(x) where x is one of the two identical angles of the triangle formed by the normalised vectors
   --  (Start, Corner) and (Corner, Finish). The input vectors do not need to be normalised, this is done by the
   --  function.

   function Unit_Bisector (Start, Corner, Finish : Scaled_Position) return Position_Scale;
   --  Compute the unit vector that bisects the given corner.

end Prunt.Motion_Planner.Planner.Corner_Blender;
