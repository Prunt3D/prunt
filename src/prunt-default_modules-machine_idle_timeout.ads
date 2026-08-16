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
with Prunt.Default_Modules.Config_Saving;
with Prunt.Default_Modules.Idle_Emitter;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;

private with Ada.Finalization;
private with Prunt.Limited_Shared_Pointers;

generic
   with package Config_Saving_Module is new Default_Modules.Config_Saving;
   with package Idle_Emitter_Module is new Default_Modules.Idle_Emitter;
   with procedure Request_Shutdown (Message : String);
   --  Report a recoverable error because the inactivity timeout expired.
package Prunt.Default_Modules.Machine_Idle_Timeout is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class;
   --  Return the configuration schema.

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;
   --  Return the supported G-code commands.

   type Module_Instance (<>) is synchronized
     new My_Modules.Module_Instance
     and Idle_Emitter_Module.Idle_Notification_Receiver with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path'Class; Message : Virtual_String);
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

   type User_Config_Machine_Idle_Timeout is record
      --  Configure the inactivity shutdown timer.

      Timeout : Time range 0.0 * s .. 1.0E12 * s := 0.0 * s;
      --  Maximum time without queued or executing moves before Prunt reports a recoverable error. Set this to zero to
      --  disable inactivity shutdown. M85 S changes this value at runtime, and M500 persists the change.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Machine_Idle_Timeout : User_Config_Machine_Idle_Timeout := (others => <>);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;
   --  Build the configuration schema.

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;
   --  Convert validated configuration data.

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);
   --  Store the configuration in Data.

   type Inactivity_Shutdown_Update is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Timeout             : Duration;
   end record;

   overriding
   procedure Process_After_Block (This : Inactivity_Shutdown_Update; Context : Block_End_Context'Class);
   --  Apply an inactivity-timeout change.

   type Inactivity_Shutdown_Report_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
   end record;

   overriding
   procedure Process_After_Block (This : Inactivity_Shutdown_Report_Event; Context : Block_End_Context'Class);
   --  Log the inactivity timeout.

   procedure Set_Inactivity_Shutdown
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Gcode_Arguments.Argument_Integer
      --  Maximum motion-idle time in seconds. `S0` disables the timeout.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M85");
   --  Set the maximum time the machine may remain without queued or executing moves. When the timeout expires, Prunt
   --  reports a recoverable error. Motion restarts the timer. Use `S0` to disable the timeout. Saved by M500.

   procedure Report_Inactivity_Shutdown
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M85");
   --  Report the current inactivity timeout to the log.

   task type Inactivity_Watchdog is
      entry Start;
      entry Stop;
      entry Set_Timeout (Value : Duration);
      entry Idle_Start;
      entry Idle_End;
   end Inactivity_Watchdog;

   type Inactivity_Watchdog_Wrapper is new Ada.Finalization.Limited_Controlled with record
      Watchdog : Inactivity_Watchdog;
   end record;

   overriding
   procedure Finalize (Object : in out Inactivity_Watchdog_Wrapper);
   --  Stop the watchdog before releasing the module instance.

   package Inactivity_Watchdog_Wrapper_Pointers is new Prunt.Limited_Shared_Pointers (Inactivity_Watchdog_Wrapper);

   protected type Module_Instance is new My_Modules.Module_Instance
   and Idle_Emitter_Module.Idle_Notification_Receiver with
      procedure Initialize
        (Config_In       : User_Config;
         Config_Data_In  : Prunt.Config.Config_Data;
         Idle_Emitter_In : My_Modules.Module_Instance_Shared_Pointers.Ref);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Apply_Runtime_Timeout (Value : Duration);

      function Get_Timeout return Duration;

      function Get_Watchdog return Inactivity_Watchdog_Wrapper_Pointers.Ref;

      overriding
      procedure Idle_Start;

      overriding
      procedure Idle_End;
   private
      Timeout               : Duration := 0.0;
      Config                : User_Config;
      Config_Data           : Prunt.Config.Config_Data;
      Watchdog              : Inactivity_Watchdog_Wrapper_Pointers.Ref;
      Idle_Emitter_Instance : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
   end Module_Instance;

end Prunt.Default_Modules.Machine_Idle_Timeout;
