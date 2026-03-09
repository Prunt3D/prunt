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

generic
package Prunt.Default_Modules.Basic_Config is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   type Module_Instance_Interface is synchronized interface;

   procedure Disable_Prunt (This : in out Module_Instance_Interface) is abstract;

   function Prunt_Is_Disabled (This : Module_Instance_Interface) return Boolean is abstract;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Module_Instance_Interface with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

private

   type User_Config_Prunt is record
      --  This section contains general settings for Prunt.

      Enabled : Boolean := False;
      --  This is the main switch to enable or disable all functionality of your machine. Enable this after configuring
      --  all other settings.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Prunt : User_Config_Prunt := (others => <>);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config);

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      overriding
      procedure Start;

      overriding
      procedure Gcode_Dispatch
        (Args               : in out Gcode_Arguments.Arguments;
         Planner            : Planner_Interface'Class;
         Command_Identifier : Gcode_Command_Identifier);

      procedure Initialize (Config_Data_In : My_Modules.Config_Data_Shared_Pointers.Ref);

      overriding
      procedure Disable_Prunt;

      overriding
      function Prunt_Is_Disabled return Boolean;
   private
      Config      : User_Config;
      Config_Data : My_Modules.Config_Data_Shared_Pointers.Ref;
   end Module_Instance;

end Prunt.Default_Modules.Basic_Config;
