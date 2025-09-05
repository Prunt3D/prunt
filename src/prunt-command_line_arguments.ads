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

   Duplicate_Argument_Error : exception;

   function Argument_Value (Switch, Default : String) return String;
   --  Returns the value of the argument beginning with Switch, excluding the prefix equal to Switch. If the argument
   --  is not found then Default is returned. If the argument is provided more than once then Duplicate_Argument_Error
   --  is raised.

   function Web_Server_Port return GNAT.Sockets.Port_Type;
   --  Returns the argument supplied to --prunt-web-server-port=, or 8080 if no argument is provided. If the
   --  argument is provided more than once then Duplicate_Argument_Error is raised.

   function Motion_Planner_CPU return System.Multiprocessors.CPU_Range;
   --  Returns the argument supplied to --prunt-motion-planner-cpu=, or 0 if no argument is provided. If the
   --  argument is provided more than once then Duplicate_Argument_Error is raised.

   function Step_Generator_CPU return System.Multiprocessors.CPU_Range;
   --  Returns the argument supplied to --prunt-step-generator-cpu=, or 0 if no argument is provided. If the
   --  argument is provided more than once then Duplicate_Argument_Error is raised.

   function Max_Planner_Block_Corners return Motion_Planner.Max_Corners_Type;
   --  Returns the argument supplied to --prunt-max-planner-block-corners=, or 50000 if no argument is provided.
   --  If the argument is provided more than once then Duplicate_Argument_Error is raised.

   function Enable_Documentation_Dev_Mode return Boolean;
   --  Returns the argument supplied to --enable-documentation-dev-mode=, or False if no argument is provided. If
   --  the argument is provided more than once then Duplicate_Argument_Error is raised.

end Prunt.Command_Line_Arguments;
