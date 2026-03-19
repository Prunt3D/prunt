-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

pragma Extensions_Allowed (On);

with Ada.Tags;
with Prunt.Config;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;

private with Ada.Containers.Ordered_Maps;

generic
package Prunt.Default_Modules.Config_Saving is

   type Module is new My_Modules.Module with null record;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Config_Saver is synchronized interface;

   procedure Register_For_Saving (This : in out Config_Saver; Config_Data : Config.Config_Data) is abstract;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Config_Saver with private;

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

   function Return_False (Left, Right : Config.Config_Data) return Boolean
   is (False);

   package Config_Data_Maps is new
     Ada.Containers.Ordered_Maps (Virtual_String, Config.Config_Data, "=" => Return_False);

   type Config_Save_Event is new Extra_Block_Resetting_Data with record
      Config_To_Save : Config.Config_Data;
   end record;

   type Config_List_Event is new Extra_Block_Resetting_Data with record
      Config_List : Virtual_String;
   end record;

   protected type Module_Instance is new My_Modules.Module_Instance and Config_Saver with
      overriding
      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref);

      overriding
      procedure Register_For_Saving (Config_Data : Config.Config_Data);

      procedure Save_Settings (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M500");
      --  Save all configurable settings for all modules that have been temporarily set as a result of g-code commands.
      --  Settings and g-code commands which use this functionality make a note of this in their own descriptions.

      procedure Save_Settings
        (Planner : Planner_Interface'Class;
         I       : Virtual_String
         --  The name of the module to save.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M500");
      --  Save all configurable settings for a specific module that have been temporarily set as a result of g-code
      --  commands. Settings and g-code commands which use this functionality make a note of this in their own
      --  descriptions.

      procedure Save_Settings
        (Planner : Planner_Interface'Class;
         I       : Gcode_No_Value
         --  When providing no value a listing of modules with savable settings will be emitted.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M500");
      --  List modules with savable settings.

   private
      Self_Ref        : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
      Configs_To_Save : Config_Data_Maps.Map;
   end Module_Instance;

end Prunt.Default_Modules.Config_Saving;
