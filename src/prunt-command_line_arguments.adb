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

with Ada.Command_Line;

package body Prunt.Command_Line_Arguments is

   pragma Extensions_Allowed (On);

   function Argument_Value (Switch, Default : String) return String is
      use Ada.Command_Line;
      Found_Index : Natural := 0;
   begin
      for Arg in 1 .. Argument_Count loop
         if Argument (Arg)'Length > Switch'Length
           and then Argument (Arg) (Argument (Arg)'First .. Argument (Arg)'First + Switch'Length - 1) = Switch
         then
            if Found_Index /= 0 then
               raise Duplicate_Argument_Error with "Duplicate command line argument: " & Switch;
            end if;
            Found_Index := Arg;
         end if;
      end loop;

      if Found_Index /= 0 then
         return Argument (Found_Index) (Argument (Found_Index)'First + Switch'Length .. Argument (Found_Index)'Last);
      else
         return Default;
      end if;
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
