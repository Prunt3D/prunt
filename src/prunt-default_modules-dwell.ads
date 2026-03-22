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

with Ada.Tags;
with Prunt.Config;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
package Prunt.Default_Modules.Dwell is

   type Module is new My_Modules.Module with null record;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

private

   protected type Module_Instance is new My_Modules.Module_Instance with
      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure No_Operation (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "G4");
      --  Does nothing. This command is provided for compatibility with Marlin g-code.

      procedure Dwell_Seconds
        (Planner : Planner_Interface'Class;
         S       : Dimensionless
         --  Amount of time to dwell in seconds.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "G4");
      --  Dwell for a specified time in seconds.
      --
      --  Negative times will cause an error to be emitted.
      --
      --  This command differs from Marlin in that `S` and `P` can not be specified at the same time. In Marlin if both
      --  `S` and `P` are specified then `P` will be silently ignored, however in Prunt it is an error to specify both.

      procedure Dwell_Milliseconds
        (Planner : Planner_Interface'Class;
         P       : Dimensionless
         --  Amount of time to dwell in milliseconds.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "G4");
      --  Dwell for a specified time in milliseconds.
      --
      --  Negative times will cause an error to be emitted.
      --
      --  This command differs from Marlin in that `S` and `P` can not be specified at the same time. In Marlin if both
      --  `S` and `P` are specified then `P` will be silently ignored, however in Prunt it is an error to specify both.
   private
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
   end Module_Instance;

end Prunt.Default_Modules.Dwell;
