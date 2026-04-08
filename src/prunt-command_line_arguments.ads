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

with GNAT.Sockets;
with Prunt.Motion_Planner;
with System.Multiprocessors;

--  TODO: We should get these from the implementation instead of getting them directly as a given implementation might
--  have multiple Prunt.Controller instances.

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

end Prunt.Command_Line_Arguments;
