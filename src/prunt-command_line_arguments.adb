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

with Ada.Command_Line;

package body Prunt.Command_Line_Arguments is

   function Argument_Value (Switch, Default : String) return String is
      use Ada.Command_Line;
   begin
      --  The last argument takes priority in case of duplicates.
      for Arg in reverse 1 .. Argument_Count loop
         if Argument (Arg)'Length > Switch'Length
           and then Argument (Arg) (Argument (Arg)'First .. Argument (Arg)'First + Switch'Length - 1) = Switch
         then
            return Argument (Arg) (Argument (Arg)'First + Switch'Length .. Argument (Arg)'Last);
         end if;
      end loop;
      return Default;
   end Argument_Value;

   function Web_Server_Port return GNAT.Sockets.Port_Type
   is (GNAT.Sockets.Port_Type'Value (Argument_Value ("--prunt-web-server-port=", "8080")));
   function Motion_Planner_CPU return System.Multiprocessors.CPU_Range
   is (System.Multiprocessors.CPU_Range'Value (Argument_Value ("--prunt-motion-planner-cpu=", "0")));
   function Step_Generator_CPU return System.Multiprocessors.CPU_Range
   is (System.Multiprocessors.CPU_Range'Value (Argument_Value ("--prunt-step-generator-cpu=", "0")));
   function Max_Planner_Block_Corners return Motion_Planner.Max_Corners_Type
   is (Motion_Planner.Max_Corners_Type'Value (Argument_Value ("--prunt-max-planner-block-corners=", "50000")));

end Prunt.Command_Line_Arguments;
