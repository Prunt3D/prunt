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
with Prunt.Status_Manager;

private with Ada.Finalization;
private with Prunt.Limited_Shared_Pointers;

generic
   with function Get_Position return Prunt.Position;
   with function Get_File_Name return Virtual_String;
   with function Get_Line return File_Line_Count;
   with function Stepgen_Paused return Boolean;
   with function Axis_Is_Homed (Axis : Axis_Name) return Boolean;
package Prunt.Default_Modules.Internal_Status_Reporter is

   type Module is new My_Modules.Module with null record;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;
   --  Return the supported G-code commands.

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;
   --  Create a module instance.

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map;
   --  Return the status schema.

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);
   --  Dispatch a G-code command.

private

   Status_Report_Period : constant Duration := 0.5;

   task type Status_Updater is
      entry Start (Status_Emitter : Status_Manager.Status_Emitter);
      entry Set_Position_Auto_Report_Interval (Value : Duration);
      entry Stop;
   end Status_Updater;

   type Status_Updater_Wrapper is new Ada.Finalization.Limited_Controlled with record
      Updater : Status_Updater;
   end record;

   overriding
   procedure Finalize (Object : in out Status_Updater_Wrapper);
   --  Stop the status updater.

   package Status_Updater_Wrapper_Pointers is new Limited_Shared_Pointers (Status_Updater_Wrapper);
   --  TODO: GCC bug in comment below?
   --
   --  The task sometimes does not start if we do not wrap it in a shared pointer. I have no idea why. I do not just
   --  mean that it does not start during the BIP (which it should not do), I mean that it does not start after the
   --  assignment is complete.

   type Position_Report_Event is new Extra_Block_Resetting_Data with null record;

   overriding
   procedure Process_After_Block (This : Position_Report_Event; Context : Block_End_Context'Class);
   --  Log the current position.

   type Position_Auto_Report_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Interval            : Duration;
   end record;

   overriding
   procedure Process_After_Block (This : Position_Auto_Report_Event; Context : Block_End_Context'Class);
   --  Update periodic position reporting.

   function Current_Position_Report (Pos : Position) return Virtual_String;
   --  Format a position report.

   procedure Log_Position;
   --  Log the current position.

   procedure Report_Current_Position (Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M114");
   --  Report the current position to the logger.
   --
   --  THE `DER` parameters from Marlin are not present.

   procedure Set_Position_Auto_Report
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Dimensionless
      --  Interval in seconds between reports. `S0` disables auto-reporting.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M154");
   --  Configure automatic position reporting to the logger.
   --
   --  This command differs from Marlin in that the `S` parameter is not optional.

   protected type Module_Instance is new My_Modules.Module_Instance with
      procedure Initialize (Status_Emitter_In : Status_Manager.Status_Emitter);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Set_Position_Auto_Report_Interval (Value : Duration);
   private
      Status_Emitter : Status_Manager.Status_Emitter;
      Updater        : Status_Updater_Wrapper_Pointers.Ref;
   end Module_Instance;

end Prunt.Default_Modules.Internal_Status_Reporter;
