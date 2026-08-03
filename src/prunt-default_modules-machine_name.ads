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
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
   with package Config_Saving_Module is new Default_Modules.Config_Saving;
package Prunt.Default_Modules.Machine_Name is

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

   type User_Config_Machine_Name is record
      --  This section contains identity information for your machine.

      Name : Virtual_String := "";
      --  The machine name reported by M550 and used by M16 checks.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Machine_Name : User_Config_Machine_Name := (others => <>);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;
   --  Build the configuration schema.

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;
   --  Convert validated configuration data.

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);
   --  Store the configuration in Data.

   type Machine_Name_Update is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Name                : Virtual_String;
   end record;

   overriding
   procedure Process_After_Block (This : Machine_Name_Update; Context : Block_End_Context'Class);
   --  Apply a machine-name change.

   type Machine_Name_Report_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
   end record;

   overriding
   procedure Process_After_Block (This : Machine_Name_Report_Event; Context : Block_End_Context'Class);
   --  Log the machine name.

   procedure Expected_Printer_Check
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      P       : Virtual_String
      --  Expected machine name.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M16");
   --  Halt if the machine name does not match the provided string. The machine name can be set in the configuration
   --  page or via M550.
   --
   --  This command has the same function as M16 in Marlin but the format is slightly different. Specifically, the
   --  string to match against must be wrapped in quotation marks and must come after the `P` parameter letter.

   procedure Set_Machine_Name
     (This    : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner : Planner_Interface'Class;
      P       : Virtual_String
      --  Machine name to set.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M550");
   --  Set the machine name. Saved by M500. This can also be set in the configuration page.
   --
   --  This command has the same function as M550 in Marlin but the format is slightly different. Specifically,
   --  there is no loose string form.

   procedure Report_Machine_Name
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M550");
   --  Report the current machine name to the log.

   protected type Module_Instance is new My_Modules.Module_Instance with
      procedure Initialize
        (Config_In         : User_Config;
         Config_Data_In    : Prunt.Config.Config_Data;
         Status_Emitter_In : Status_Manager.Status_Emitter);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Apply_Runtime_Name (Value : Virtual_String);

      function Get_Current_Name return Virtual_String;
   private
      Self_Ref       : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
      Config         : User_Config;
      Config_Data    : Prunt.Config.Config_Data;
      Status_Emitter : Status_Manager.Status_Emitter;
   end Module_Instance;

end Prunt.Default_Modules.Machine_Name;
