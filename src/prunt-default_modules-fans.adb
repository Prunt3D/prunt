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

package body Prunt.Default_Modules.Fans is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   function Maximum_PWM_Frequency
     (Fan : Fan_Name; Use_High_Side_Switching : Boolean) return Frequency
   is
   begin
      case Fan_Hardware (Fan).Kind is
         when Fixed_Switching_Kind =>
            return Fan_Hardware (Fan).Maximum_PWM_Frequency;

         when Low_Or_High_Side_Switching_Kind =>
            return
              (if Use_High_Side_Switching
               then Fan_Hardware (Fan).Maximum_High_Side_PWM_Frequency
               else Fan_Hardware (Fan).Maximum_Low_Side_PWM_Frequency);
      end case;
   end Maximum_PWM_Frequency;

   function PWM_Frequency_Path (Fan : Fan_Name) return Config.Config_Data_Paths.Vector is (["Fans", +Fan'Image, "PWM_Frequency"]);

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
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Status_Emitter, Get_Other_Instance);

      Parsed_Config : constant User_Config := Config_Data_To_User_Config (Config_Data);
   begin
      return Result : Module_Instance do
         Result.Initialize (Parsed_Config);

         for F in Fan_Name loop
            if Parsed_Config.Fans (F).PWM_Frequency > Maximum_PWM_Frequency
                 (F, Parsed_Config.Fans (F).Use_High_Side_Switching)
            then
               Report_Config_Error
                 (PWM_Frequency_Path (F),
                  "This PWM frequency exceeds the maximum supported by the selected fan hardware mode.");
            end if;
         end loop;
      end return;
   end Initialize;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config) is
      begin
         Config := Config_In;
      end Initialize;

      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref) is
      begin
         Self_Ref := Self_Ref_In;

         --  The current fan hardware API only exposes PWM frequency and switching mode.
         for F in Fan_Name loop
            case Fan_Hardware (F).Kind is
               when Fixed_Switching_Kind =>
                  Fan_Hardware (F).Reconfigure_Fixed_Switching_Fan (F, Config.Fans (F).PWM_Frequency);

               when Low_Or_High_Side_Switching_Kind =>
                  Fan_Hardware (F).Reconfigure_Low_Or_High_Side_Switching_Fan
                    (F,
                     Config.Fans (F).PWM_Frequency,
                     Config.Fans (F).Use_High_Side_Switching);
            end case;
         end loop;
      end Start;

      procedure Set_Fan_Speed
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, P, S);
         raise Constraint_Error with "Runtime fan duty control is not available through Fan_Hardware yet.";
      end Set_Fan_Speed;

      procedure Set_Fan_Speed
        (Planner : Planner_Interface'Class;
         P       : Virtual_String;
         S       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, P, S);
         raise Constraint_Error with "Runtime fan duty control by name is not available through Fan_Hardware yet.";
      end Set_Fan_Speed;

      procedure Turn_Off_Fan (Planner : Planner_Interface'Class; P : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, P);
         raise Constraint_Error with "Runtime fan duty control is not available through Fan_Hardware yet.";
      end Turn_Off_Fan;

      procedure Turn_Off_Fan (Planner : Planner_Interface'Class; P : Virtual_String) is
      begin
         pragma Unreferenced (Planner, P);
         raise Constraint_Error with "Runtime fan duty control by name is not available through Fan_Hardware yet.";
      end Turn_Off_Fan;

      procedure Report_Fan_Tachometers
        (Planner : Planner_Interface'Class;
         S       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, S);
         My_Logger.Log ("M123 fan tachometer reporting is not implemented yet.");
      end Report_Fan_Tachometers;
   end Module_Instance;

end Prunt.Default_Modules.Fans;
