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

with GNAT.Sockets;
with System.Multiprocessors;
with Prunt.Motion_Planner;

package Prunt.Command_Line_Arguments is

   function Web_Server_Port return GNAT.Sockets.Port_Type;
   function Motion_Planner_CPU return System.Multiprocessors.CPU_Range;
   function Step_Generator_CPU return System.Multiprocessors.CPU_Range;
   function Max_Planner_Block_Corners return Motion_Planner.Max_Corners_Type;
   function Enable_Documentation_Dev_Mode return Boolean;

end Prunt.Command_Line_Arguments;
