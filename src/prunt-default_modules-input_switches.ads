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
   type Input_Switch_Name is (<>);
package Prunt.Default_Modules.Input_Switches is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

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
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

private

   type User_Config_Switch_Kind is (Normally_Open, Normally_Closed)
   with Annotate => (Prunt_Config, User_Config);

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

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize (Config_In : User_Config);

      overriding
      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Report_Endstop_States (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M119");
      --  Report configured input switch states to the logger.

      procedure Enable_Endstops (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M120");
      --  Enable endstops. Currently a documented no-op placeholder.

      procedure Disable_Endstops (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M121");
      --  Disable endstops. Currently a documented no-op placeholder.

      overriding
      function Switch_Is_Enabled_In_Config (Switch : Input_Switch_Name) return Boolean;

      overriding
      function Switch_Is_Normally_Closed (Switch : Input_Switch_Name) return Boolean;
   private
      Config   : User_Config;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
   end Module_Instance;

end Prunt.Default_Modules.Input_Switches;
