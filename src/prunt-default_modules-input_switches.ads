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
   Input_Switch_Hardware : My_Controller_Generic_Types.Input_Switch_Hardware_Parameters_Array_Type;
package Prunt.Default_Modules.Input_Switches is

   --  TODO: Need to receive homing events and then run the setup for homing/conditional moves. Alternatively homing
   --  module could check the switch state and do the conditional part?

   use My_Controller_Generic_Types;

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map;

   type Module_Instance_Interface is synchronized interface;

   function Switch_Is_Enabled_In_Config (This : Module_Instance_Interface; Switch : Input_Switch_Name) return Boolean
   is abstract;

   function Switch_Is_Normally_Closed (This : Module_Instance_Interface; Switch : Input_Switch_Name) return Boolean
   is abstract;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Module_Instance_Interface with private;

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

   Status_Report_Period : constant Duration := 0.5;

   type User_Config_Switch_Kind is (Normally_Open, Normally_Closed) with Annotate => (Prunt_Config, User_Config);

   type User_Config_Input_Switch is record
      Enabled : Boolean := False;
      --  Enable this input switch. Disabled switches remain unavailable to modules such as homing even if the
      --  hardware exposes them.

      Kind : User_Config_Switch_Kind := Normally_Open;
      --  Select whether the switch is normally open or normally closed in its resting state.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Input_Switch_Array is array (Input_Switch_Name) of User_Config_Input_Switch
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Switches : User_Config_Input_Switch_Array := [others => <>];
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   type Endstop_Report_Event is new Extra_Block_Resetting_Data with record
      Config : User_Config;
   end record;

   overriding
   procedure Process_After_Block (This : Endstop_Report_Event; Context : Block_End_Context'Class);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   procedure Report_Endstop_States (This : Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M119");
   --  Report configured input switch states to the logger.

   task type Status_Updater is
      entry Start (Config : User_Config; Status_Emitter : Status_Manager.Status_Emitter);
      entry Stop;
   end Status_Updater;

   type Status_Updater_Wrapper is new Ada.Finalization.Limited_Controlled with record
      Updater : Status_Updater;
   end record;

   overriding
   procedure Finalize (Object : in out Status_Updater_Wrapper);

   package Status_Updater_Wrapper_Pointers is new Limited_Shared_Pointers (Status_Updater_Wrapper);

   function Switch_Is_Triggered (Config : User_Config; Switch : Input_Switch_Name) return Boolean;

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize (Config_In : User_Config; Status_Emitter_In : Status_Manager.Status_Emitter);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      overriding
      function Switch_Is_Enabled_In_Config (Switch : Input_Switch_Name) return Boolean;

      overriding
      function Switch_Is_Normally_Closed (Switch : Input_Switch_Name) return Boolean;

      function Get_Config return User_Config;
   private
      Config         : User_Config;
      Status_Emitter : Status_Manager.Status_Emitter;
      Updater        : Status_Updater_Wrapper_Pointers.Ref;
   end Module_Instance;

end Prunt.Default_Modules.Input_Switches;
