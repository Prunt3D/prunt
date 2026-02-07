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

package body Prunt.Default_Modules.Motion is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema is
   begin
      return (Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
   begin
      return Module_Instance'(My_Modules.Module_Instance with Config => Config_Data_To_User_Config (Config_Data.Get));
   end Initialize;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   procedure Rapid_Linear_Move
     (This : in out Module_Instance; Planner : Planner_Interface'Class; X, Y, Z, E, F : Gcode_Optional_Float) is
   begin
      if This.Config.Prunt.Replace_G0_With_G1 then
         Linear_Move (This, Planner, X => X, Y => Y, Z => Z, E => E, F => F);
      else
         null;
         --  TODO
      end if;
   end Rapid_Linear_Move;

   procedure Linear_Move
     (This : in out Module_Instance; Planner : Planner_Interface'Class; X, Y, Z, E, F : Gcode_Optional_Float) is
   begin
      null;
      --  TODO
   end Linear_Move;

   procedure Retract (This : in out Module_Instance; Planner : Planner_Interface'Class) is
   begin
      null;
      --  TODO
   end Retract;

   procedure Recover (This : in out Module_Instance; Planner : Planner_Interface'Class) is
   begin
      null;
      --  TODO
   end Recover;

   procedure Millimeter_Units (This : in out Module_Instance; Planner : Planner_Interface'Class) is
   begin
      null;
      --  TODO
   end Millimeter_Units;

   procedure Absolute_Positioning (This : in out Module_Instance; Planner : Planner_Interface'Class) is
   begin
      null;
      --  TODO
   end Absolute_Positioning;

   procedure Relative_Positioning (This : in out Module_Instance; Planner : Planner_Interface'Class) is
   begin
      null;
      --  TODO
   end Relative_Positioning;

   procedure Set_Virtual_Position
     (This : in out Module_Instance; Planner : Planner_Interface'Class; X, Y, Z, E : Gcode_Optional_Float) is
   begin
      null;
      --  TODO
   end Set_Virtual_Position;

   procedure E_Axis_Absolute (This : in out Module_Instance; Planner : Planner_Interface'Class) is
   begin
      null;
      --  TODO
   end E_Axis_Absolute;

   procedure E_Axis_Relative (This : in out Module_Instance; Planner : Planner_Interface'Class) is
   begin
      null;
      --  TODO
   end E_Axis_Relative;

   procedure Retraction_Settings
     (This : in out Module_Instance; Planner : Planner_Interface'Class; F, E, Z : Gcode_Optional_Float) is
   begin
      null;
      --  TODO
   end Retraction_Settings;

   procedure Recover_Settings
     (This : in out Module_Instance; Planner : Planner_Interface'Class; F, S : Gcode_Optional_Float) is
   begin
      null;
      --  TODO
   end Recover_Settings;

end Prunt.Default_Modules.Motion;
