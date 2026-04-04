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
with Prunt.Controller_Generic_Types;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
   with package My_Controller_Generic_Types is new Controller_Generic_Types (<>);
   --  TODO
package Prunt.Default_Modules.Tachometers is

   use My_Controller_Generic_Types;

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

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
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

private

   procedure Report_Tachometers (Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M123");
   --  Report tachometer readings to the log immediately. This will not interrupt readings that are being reported
   --  on an interval.

   procedure Report_Tachometers
     (Planner : Planner_Interface'Class;
      S       : Dimensionless
      --  Interval in seconds between reports.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M123");
   --  Report tachometer readings to the log repeatedly with a given interval. If this command has been called
   --  previously then this will override the previous interval rather than using both.
   --
   --  This command differs from Marlin in that the `S` parameter may be a real number instead of just an integer.

   --  type User_Config is record
   --  end record
   --  with Annotate => (Prunt_Config, Root_User_Config);

   --  TODO: Tachometer settings.

   --  function Build_Schema return Config.Config_Property_Maps.Map;

   --  function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   --  procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   protected type Module_Instance is new My_Modules.Module_Instance with
      --  procedure Initialize (Config_In : User_Config);

      overriding
      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

   private
      --  Config   : User_Config;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
   end Module_Instance;

end Prunt.Default_Modules.Tachometers;
