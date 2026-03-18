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

package body Prunt.Default_Modules.Input_Shapers is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   function Build_Shaper_Parameters
     (Method : User_Config_Input_Shaping_Method) return Prunt.Input_Shapers.Shaper_Parameters is
   begin
      case Method.Kind is
         when No_Shaper        =>
            return (Kind => Prunt.Input_Shapers.No_Shaper);

         when ZV               =>
            return
              (Kind                         => Prunt.Input_Shapers.Zero_Vibration,
               Zero_Vibration_Frequency     => Method.ZV.Shaper_Frequency,
               Zero_Vibration_Damping_Ratio => Method.ZV.Damping_Ratio,
               Zero_Vibration_Deriviatives  => Method.ZV.Number_Of_Derivatives);

         when EI               =>
            return
              (Kind                                 => Prunt.Input_Shapers.Extra_Insensitive,
               Extra_Insensitive_Frequency          => Method.EI.Shaper_Frequency,
               Extra_Insensitive_Damping_Ratio      => Method.EI.Damping_Ratio,
               Extra_Insensitive_Humps              => Method.EI.Number_Of_Humps,
               Extra_Insensitive_Residual_Vibration => Method.EI.Residual_Vibration_Level);

         when Pressure_Advance =>
            return
              (Kind                                    => Prunt.Input_Shapers.Pressure_Advance,
               Pressure_Advance_Time                   => Method.Pressure_Advance.Pressure_Advance_Time,
               Pressure_Advance_Smooth_Time            => Method.Pressure_Advance.Pressure_Advance_Smooth_Time,
               Pressure_Advance_Smooth_Added_Part_Only => Method.Pressure_Advance.Smooth_Added_Part_Only,
               Pressure_Advance_Smooth_Levels          => Positive (Method.Pressure_Advance.Smoothing_Levels));
      end case;
   end Build_Shaper_Parameters;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema is
   begin
      return (Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Report_Config_Error, Status_Emitter, Get_Other_Instance);
   begin
      return Result : Module_Instance do
         Result.Initialize (Config_Data_To_User_Config (Config_Data.Get));
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config) is
      begin
         Config := Config_In;
      end Initialize;

      procedure Start is null;

      procedure Configure_Input_Shaping
        (Planner : Planner_Interface'Class;
         P       : Virtual_String;
         X       : Gcode_Optional_String;
         Y       : Gcode_Optional_String;
         Z       : Gcode_Optional_String;
         E       : Gcode_Optional_String) is
      begin
         pragma Unreferenced (Planner, P, X, Y, Z, E);
         raise Constraint_Error with "M493 is not implemented yet.";
      end Configure_Input_Shaping;

      function Get_Default_Axial_Shapers return Prunt.Input_Shapers.Axial_Shaper_Parameters is
      begin
         return [for Axis in Axis_Name => Build_Shaper_Parameters (Config.Input_Shaping (Axis))];
      end Get_Default_Axial_Shapers;
   end Module_Instance;

end Prunt.Default_Modules.Input_Shapers;
