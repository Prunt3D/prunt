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

package body Prunt.Default_Modules.Heaters is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config) is separate;

   function Thermistor_Path (Heater : Heater_Name) return Config.Config_Data_Paths.Vector
   is (["Heaters", +Heater'Image, "Thermistor"]);

   function To_Heater_Parameters (Config : User_Config_Heater) return Heater_Parameters is
   begin
      case Config.Control_Method.Kind is
         when Disabled =>
            return
              (Kind                         => Disabled_Kind,
               Check_Max_Cumulative_Error   => Config.Check_Maximum_Cumulative_Error,
               Check_Gain_Time              => Config.Check_Gain_Time,
               Check_Minimum_Gain           => Config.Check_Minimum_Gain,
               Check_Hysteresis             => Config.Check_Hysteresis);

         when PID =>
            return
              (Kind                         => PID_Kind,
               Check_Max_Cumulative_Error   => Config.Check_Maximum_Cumulative_Error,
               Check_Gain_Time              => Config.Check_Gain_Time,
               Check_Minimum_Gain           => Config.Check_Minimum_Gain,
               Check_Hysteresis             => Config.Check_Hysteresis,
               Proportional_Scale           => Config.Control_Method.PID.Proportional_Scale,
               Integral_Scale               => Config.Control_Method.PID.Integral_Scale,
               Derivative_Scale             => Config.Control_Method.PID.Derivative_Scale);

         when Bang_Bang =>
            return
              (Kind                         => Bang_Bang_Kind,
               Check_Max_Cumulative_Error   => Config.Check_Maximum_Cumulative_Error,
               Check_Gain_Time              => Config.Check_Gain_Time,
               Check_Minimum_Gain           => Config.Check_Minimum_Gain,
               Check_Hysteresis             => Config.Check_Hysteresis,
               Bang_Bang_Hysteresis         => Config.Control_Method.Bang_Bang.Hysteresis);
      end case;
   end To_Heater_Parameters;

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
      pragma Unreferenced (This, Status_Emitter);
      use type My_Modules.Module_Instance_Shared_Pointers.Ref;

      Parsed_Config                  : constant User_Config := Config_Data_To_User_Config (Config_Data.Get);
      Thermistors_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Thermistors_Module.Module_Instance'Tag);
   begin
      if Thermistors_Module_Instance_Ref = My_Modules.Module_Instance_Shared_Pointers.Null_Ref then
         raise Program_Error with "Thermistors module instance not found.";
      end if;

      return Result : Module_Instance do
         declare
            Thermistors_Module_Instance : Thermistors_Module.Module_Instance_Interface'Class renames
              Thermistors_Module.Module_Instance_Interface'Class (Thermistors_Module_Instance_Ref.Get.Element.all);
         begin
            Result.Initialize (Parsed_Config);

            for H in Heater_Name loop
               if Parsed_Config.Heaters (H).Control_Method.Kind /= Disabled
                 and then
                   not
                     Thermistors_Module_Instance.Thermistor_Is_Enabled_In_Config
                       (Parsed_Config.Heaters (H).Thermistor)
               then
                  Report_Config_Error (Thermistor_Path (H), "This thermistor is disabled in Thermistors.");
               end if;
            end loop;
         end;
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config) is
      begin
         Config := Config_In;
      end Initialize;

      procedure Start is
      begin
         for H in Heater_Name loop
            Heater_Hardware (H).Reconfigure (H, To_Heater_Parameters (Config.Heaters (H)), Config.Heaters (H).Thermistor);
         end loop;
      end Start;

      procedure Gcode_Dispatch
        (Args               : in out Gcode_Arguments.Arguments;
         Planner            : Planner_Interface'Class;
         Command_Identifier : Gcode_Command_Identifier) is
      begin
         pragma Unreferenced (Args, Planner, Command_Identifier);
         raise Constraint_Error with "Not implemented.";
      end Gcode_Dispatch;

      function Heater_Is_Enabled_In_Config (Heater : Heater_Name) return Boolean is
      begin
         return Config.Heaters (Heater).Control_Method.Kind /= Disabled;
      end Heater_Is_Enabled_In_Config;

      function Assigned_Thermistor (Heater : Heater_Name) return Thermistor_Name is
      begin
         return Config.Heaters (Heater).Thermistor;
      end Assigned_Thermistor;

      function Get_Heater_Parameters (Heater : Heater_Name) return Heater_Parameters is
      begin
         return To_Heater_Parameters (Config.Heaters (Heater));
      end Get_Heater_Parameters;
   end Module_Instance;

end Prunt.Default_Modules.Heaters;
