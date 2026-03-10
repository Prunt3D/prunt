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

package body Prunt.Default_Modules.Motor_Drivers is

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
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
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

      procedure Start is
      begin
         for M in Motor_Name loop
            if not Motor_Configs_Provided (M) then
               raise Program_Error with "Motor configuration not provided for " & M'Image;
            end if;
         end loop;
      end Start;

      procedure Gcode_Dispatch
        (Args               : in out Gcode_Arguments.Arguments;
         Planner            : Planner_Interface'Class;
         Command_Identifier : Gcode_Command_Identifier) is
      begin
         raise Constraint_Error with "Not implemented.";
      end Gcode_Dispatch;

      procedure Provide_Motor_Configuration
        (Motor : Motor_Name; Configuration : Motor_Configuration; Handler : Motor_Handler'Class) is
      begin
         if Motor_Configs_Provided (Motor) then
            raise Program_Error with "Motor configuration already provided for " & Motor'Image;
         end if;

         Motor_Configs (Motor) := Configuration;
         Motor_Configs_Provided (Motor) := True;
      end Provide_Motor_Configuration;

      function Motor_Is_Enabled_In_Config (Motor : Motor_Name) return Boolean is
      begin
         return Config.Motors (Motor).Enabled;
      end Motor_Is_Enabled_In_Config;

      function Distance_Per_Rotation (Motor : Motor_Name) return Length is
         Motor_Config         : constant User_Config_Motion_Units := Config.Motors (Motor).Motion_Units;
         Direction_Multiplier : constant Dimensionless := (if Motor_Config.Reverse_Direction then -1.0 else 1.0);
      begin
         case Motor_Config.Kind is
            when Direct_Entry                    =>
               return Direction_Multiplier * Motor_Config.Direct_Entry.Distance_Per_Rotation;

            when Lead_Screw                      =>
               return
                 Direction_Multiplier
                 * Motor_Config.Lead_Screw.Lead
                 / (Motor_Config.Lead_Screw.Gear_Ratio.Numerator / Motor_Config.Lead_Screw.Gear_Ratio.Denominator);

            when Gear_With_Circumference         =>
               return
                 Direction_Multiplier
                 * Motor_Config.Gear_With_Circumference.Circumference
                 / (Motor_Config.Gear_With_Circumference.Gear_Ratio.Numerator
                    / Motor_Config.Gear_With_Circumference.Gear_Ratio.Denominator);

            when Gear_With_Tooth_Count_And_Pitch =>
               return
                 Direction_Multiplier
                 * (Motor_Config.Gear_With_Tooth_Count_And_Pitch.Tooth_Count
                    * Motor_Config.Gear_With_Tooth_Count_And_Pitch.Tooth_Pitch)
                 / (Motor_Config.Gear_With_Tooth_Count_And_Pitch.Gear_Ratio.Numerator
                    / Motor_Config.Gear_With_Tooth_Count_And_Pitch.Gear_Ratio.Denominator);
         end case;
      end Distance_Per_Rotation;

      function Distance_Per_Unit (Motor : Motor_Name; Microsteps : Dimensionless) return Length is
      begin
         return (Distance_Per_Rotation (Motor) / Config.Motors (Motor).Motion_Units.Units_Per_Rotation) / Microsteps;
      end Distance_Per_Unit;

      function Distance_Per_Unit (Motor : Motor_Name) return Length is
      begin
         if not Motor_Configs_Provided (Motor) then
            raise Program_Error with "Motor configuration not yet provided for " & Motor'Image;
         end if;

         return Distance_Per_Unit (Motor, Motor_Configs (Motor).Microsteps);
      end Distance_Per_Unit;
   end Module_Instance;

end Prunt.Default_Modules.Motor_Drivers;
