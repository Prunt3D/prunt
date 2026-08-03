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

private with Ada.Finalization;
private with Prunt.Limited_Shared_Pointers;

generic
   with package My_Controller_Generic_Types is new Controller_Generic_Types (<>);
   Tachometer_Hardware : My_Controller_Generic_Types.Tachometer_Hardware_Parameters_Array_Type;
package Prunt.Default_Modules.Tachometers is

   use My_Controller_Generic_Types;

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;
   --  Return the configuration schema.

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;
   --  Return the supported G-code commands.

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map;
   --  Return the status schema.

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;
   --  Create a module instance.

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

   type User_Config_Tachometer is record
      Enabled : Boolean := False;
      --  Enable this tachometer. Disabled tachometers remain available in the schema but report 0 RPM.

      Pulses_Per_Revolution : Dimensionless range 1.0E-100 .. 1.0E100 := 1.0;
      --  Number of tachometer pulses produced per full revolution.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Tachometer_Array is array (Tachometer_Name) of User_Config_Tachometer
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Tachometers : User_Config_Tachometer_Array := [others => <>];
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Current_Speed
     (Config         : User_Config;
      Tachometer     : Tachometer_Name;
      Requires_Fresh : Boolean) return Dimensionless;
   --  Return Tachometer's speed.

   procedure Log_Tachometers (Config : User_Config; Requires_Fresh : Boolean);
   --  Log enabled tachometer speeds.

   type Tachometer_Report_Event is new Extra_Block_Resetting_Data with record
      Config : User_Config;
   end record;

   overriding
   procedure Process_After_Block (This : Tachometer_Report_Event; Context : Block_End_Context'Class);
   --  Log a tachometer report.

   type Tachometer_Auto_Report_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Interval            : Duration;
   end record;

   overriding
   procedure Process_After_Block (This : Tachometer_Auto_Report_Event; Context : Block_End_Context'Class);
   --  Update periodic tachometer reporting.

   function Build_Schema return Config.Config_Property_Maps.Map;
   --  Build the configuration schema.

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;
   --  Convert validated configuration data.

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);
   --  Store the configuration in Data.

   type Tachometer_Speed_Status_Setters is array (Tachometer_Name) of Status_Manager.Lock_Free_Dimensionless_Setter;

   procedure Report_Tachometers (This : Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M123");
   --  Report tachometer readings to the log immediately. This will not interrupt readings that are being reported
   --  on an interval.

   procedure Set_Tachometer_Auto_Report
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Dimensionless
      --  Interval in seconds between reports. `S0` disables auto-reporting.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M123");
   --  Report tachometer readings to the log repeatedly with a given interval. If this command has been called
   --  previously then this will override the previous interval rather than using both.
   --
   --  This command differs from Marlin in that the `S` parameter may be a real number instead of just an integer.

   task type Status_Updater is
      entry Start (Config : User_Config; Speed_Status_Setters_In : Tachometer_Speed_Status_Setters);
      entry Stop;
      entry Set_Auto_Report_Interval (Value : Duration);
   end Status_Updater;

   type Status_Updater_Wrapper is new Ada.Finalization.Limited_Controlled with record
      Updater : Status_Updater;
   end record;

   overriding
   procedure Finalize (Object : in out Status_Updater_Wrapper);
   --  Stop the status updater.

   package Status_Updater_Wrapper_Pointers is new Limited_Shared_Pointers (Status_Updater_Wrapper);

   protected type Module_Instance is new My_Modules.Module_Instance with
      procedure Initialize (Config_In : User_Config; Status_Emitter_In : Status_Manager.Status_Emitter);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Set_Auto_Report_Interval (Value : Duration);

      function Get_Config return User_Config;
   private
      Config               : User_Config;
      Speed_Status_Setters : Tachometer_Speed_Status_Setters;
      Self_Ref             : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
      Updater              : Status_Updater_Wrapper_Pointers.Ref;
   end Module_Instance;

end Prunt.Default_Modules.Tachometers;
