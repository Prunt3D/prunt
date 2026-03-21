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
            case Fan_Hardware (Fan).Kind is
               when Fixed_Switching_Kind            =>
                  if Parsed_Config.Fans (F).PWM_Frequency > Fan_Hardware (F).Maximum_PWM_Frequency then
                     --  TODO: We should propagate this to the client in the schema and raise a constraint error if we
                     --  somehow get a bad value here.
                     Report_Config_Error
                       (["Fans", +F'Image, "PWM_Frequency"],
                        "This frequency exceeds the maximum supported by this fan output. The maximum frequency is "
                        & Dimensionless'Image (Fan_Hardware (F).Maximum_PWM_Frequency / hertz)
                        & " Hz.");
                  end if;

               when Low_Or_High_Side_Switching_Kind =>
                  if Parsed_Config.Fans (F).PWM_Frequency
                    > (if Parsed_Config.Fans (F).Use_High_Side_Switching
                       then Fan_Hardware (F).Maximum_High_Side_PWM_Frequency
                       else Fan_Hardware (F).Maximum_Low_Side_PWM_Frequency)
                  then
                     --  TODO: We should propagate the largest value to the client in the schema and have a more
                     --  friendly error message here.
                     Report_Config_Error
                       (["Fans", +F'Image, "PWM_Frequency"],
                        "This frequency exceeds the maximum supported by this fan output. The maximum frequency is "
                        & Dimensionless'Image (Fan_Hardware (F).Maximum_Low_Side_PWM_Frequency / hertz)
                        & " Hz in low side switching mode or "
                        & Dimensionless'Image (Fan_Hardware (F).Maximum_High_Side_PWM_Frequency / hertz)
                        & " Hz in high side switching mode.");
                  end if;
            end case;
         end loop;
      end return;
   end Initialize;

   procedure Process (This : Fan_Speed_Change; Last_Command_Index : Command_Index) is
   begin
      Fan_Hardware (This.Fan).Set_Duty_Cycle ((if This.Invert then 1.0 - This.Duty_Cycle else This.Duty_Cycle));
      This.Speeds_Array.Get (This.Fan) := This.Duty_Cycle;
   end Process;

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
         --  TODO: Need to pass in status manager and set up the status schema.
      end Initialize;

      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref) is
      begin
         Self_Ref := Self_Ref_In;

         for F in Fan_Name loop
            case Fan_Hardware (F).Kind is
               when Fixed_Switching_Kind            =>
                  Fan_Hardware (F).Reconfigure_Fixed_Switching_Fan (F, Config.Fans (F).PWM_Frequency);

               when Low_Or_High_Side_Switching_Kind =>
                  Fan_Hardware (F).Reconfigure_Low_Or_High_Side_Switching_Fan
                    (F, Config.Fans (F).PWM_Frequency, Config.Fans (F).Use_High_Side_Switching);
            end case;
         end loop;
         --  TODO: Set fixed speed values.
      end Start;

      procedure Set_Fan_Speed_Internal (Planner : Planner_Interface'Class; Fan : Fan_Name; Speed : Dimensionless) is
      begin
         if Config.Fans (Fan).Control_Method.Kind /= Dynamic_Duty_Cycle then
            raise Gcode_Bad_Inputs_Error
              with "Fan " & Fan'Image & " is not configured for dynamic duty cycle operation.";
         end if;

         if Speed < 0.0 then
            raise Gcode_Bad_Inputs_Error with "Speed must not be less than 0.";
         end if;

         if Speed > 255.0 then
            raise Gcode_Bad_Inputs_Error with "Speed must not be greater than 255.";
         end if;

         declare
            Duty_Cycle : PWM_Scale :=
              Speed / 255.0 * Config.Fans (Fan).Control_Method.Dynamic_Duty_Cycle.Maximum_Duty_Cycle;
         begin
            Planner.Add_Corner_Data
              (Fan_Speed_Change'
                 (Fan          => Fan,
                  Invert       => Config.Fans (Fan).Invert_PWM_Output,
                  Duty_Cycle   => Duty_Cycle,
                  Speeds_Array => Speeds_Array));
         end;
      end Set_Fan_Speed_Internal;

      procedure Set_Fan_Speed_For_Default_Fan (Planner : Planner_Interface'Class; S : Dimensionless := 255.0) is
      begin
         Set_Fan_Speed_Internal (Planner, Config.Gcode_Defaults.Default_Fan, S);
      end Set_Fan_Speed_For_Default_Fan;

      procedure Set_Fan_Speed
        (Planner : Planner_Interface'Class; P : Gcode_Arguments.Argument_Integer; S : Dimensionless := 255.0) is
      begin
         --  TODO: Need a fan lookup array in controller generic types.
         Set_Fan_Speed_Internal (Planner, P, S);
      end Set_Fan_Speed;

      procedure Set_Fan_Speed (Planner : Planner_Interface'Class; P : Virtual_String; S : Dimensionless := 255.0) is
         Fan : Fan_Name;
      begin
         begin
            Fan := Fan_Name'Value (Conversions.To_UTF_8_String (P));
         exception
            when Constraint_Error =>
               --  TODO: Emit a list of valid fans here.
               raise Gcode_Bad_Inputs_Error with "Fan name not known.";
         end;

         Set_Fan_Speed_Internal (Planner, Fan, S);
      end Set_Fan_Speed;

      procedure Turn_Off_Default_Fan (Planner : Planner_Interface'Class) is
      begin
         Set_Fan_Speed_Internal (Planner, Config.Gcode_Defaults.Default_Fan, 0.0);
      end Turn_Off_Default_Fan;

      procedure Turn_Off_Fan (Planner : Planner_Interface'Class; P : Gcode_Arguments.Argument_Integer) is
      begin
         --  TODO: Need a fan lookup array in controller generic types.
         Set_Fan_Speed_Internal (Planner, P, 0.0);
      end Turn_Off_Fan;

      procedure Turn_Off_Fan (Planner : Planner_Interface'Class; P : Virtual_String) is
      begin
         begin
            Fan := Fan_Name'Value (Conversions.To_UTF_8_String (P));
         exception
            when Constraint_Error =>
               --  TODO: Emit a list of valid fans here.
               raise Gcode_Bad_Inputs_Error with "Fan name not known.";
         end;

         Set_Fan_Speed_Internal (Planner, Fan, 0.0);
      end Turn_Off_Fan;
   end Module_Instance;

end Prunt.Default_Modules.Fans;
