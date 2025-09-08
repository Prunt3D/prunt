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
package Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter is

   procedure Run (Block : in out Execution_Block);
   --  Apply early kinematic limitations to the execution block. The programmed feed-rate is adjusted if
   --  `Ignore_E_In_XYZE` is set so that it is equal to the desired feedrate when the E axis movement is included.
   --  After this the total time of each move is adjusted such that no move will be less than `Interpolation_Time`.
   --  Finally the axial limits defined in `Axial_Velocity_Maxes` are applied.

end Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter;
