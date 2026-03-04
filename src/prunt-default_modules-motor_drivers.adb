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
      return My_Modules.Module_Instance'Class
   is
      My_Config : constant User_Config := Config_Data_To_User_Config (Config_Data.Get);
   begin
      --  TODO
      return
        Module_Instance'
          (My_Modules.Module_Instance
           with Config => My_Config, Motor_Configs => <>, Motor_Configs_Provided => [others => False]);
   end Initialize;

   overriding
   procedure Start (This : in out Module_Instance) is
   begin
      for M in Motor_Name loop
         if not This.Motor_Configs_Provided (M) then
            raise Program_Error with "Motor configuration not provided for " & M'Image;
         end if;
      end loop;
   end Start;

   procedure Provide_Motor_Configuration
     (This          : in out Module_Instance;
      Motor         : Motor_Name;
      Configuration : Motor_Configuration;
      Handler       : Motor_Handler'Class) is
   begin
      if This.Motor_Configs_Provided (Motor) then
         raise Program_Error with "Motor configuration already provided for " & Motor'Image;
      end if;

      This.Motor_Configs (Motor) := Configuration;
      This.Motor_Configs_Provided (Motor) := True;
   end Provide_Motor_Configuration;

   function Motor_Is_Enabled_In_Config (This : Module_Instance; Motor : Motor_Name) return Boolean is
   begin
      return This.Config.Motors (Motor).Enabled;
   end Motor_Is_Enabled_In_Config;

   function Distance_Per_Rotation (This : Module_Instance; Motor : Motor_Name) return Length is
      Config               : constant User_Config_Motion_Units := This.Config.Motors (Motor).Motion_Units;
      Direction_Multiplier : constant Dimensionless := (if Config.Reverse_Direction then -1.0 else 1.0);
   begin
      case Config.Kind is
         when Direct_Entry                    =>
            return Direction_Multiplier * Config.Direct_Entry.Distance_Per_Rotation;

         when Lead_Screw                      =>
            return
              Direction_Multiplier
              * Config.Lead_Screw.Lead
              / (Config.Lead_Screw.Gear_Ratio.Numerator / Config.Lead_Screw.Gear_Ratio.Denominator);

         when Gear_With_Circumference         =>
            return
              Direction_Multiplier
              * Config.Gear_With_Circumference.Circumference
              / (Config.Gear_With_Circumference.Gear_Ratio.Numerator
                 / Config.Gear_With_Circumference.Gear_Ratio.Denominator);

         when Gear_With_Tooth_Count_And_Pitch =>
            return
              Direction_Multiplier
              * (Config.Gear_With_Tooth_Count_And_Pitch.Tooth_Count
                 * Config.Gear_With_Tooth_Count_And_Pitch.Tooth_Pitch)
              / (Config.Gear_With_Tooth_Count_And_Pitch.Gear_Ratio.Numerator
                 / Config.Gear_With_Tooth_Count_And_Pitch.Gear_Ratio.Denominator);
      end case;
   end Distance_Per_Rotation;

   function Distance_Per_Unit (This : Module_Instance; Motor : Motor_Name) return Length is
   begin
      if not This.Motor_Configs_Provided (Motor) then
         raise Program_Error with "Motor configuration not yet provided for " & Motor'Image;
      end if;

      return
        (This.Distance_Per_Rotation (Motor) / This.Config.Motors (Motor).Motion_Units.Units_Per_Rotation)
        / This.Motor_Configs (Motor).Microsteps;
   end Distance_Per_Unit;

end Prunt.Default_Modules.Motor_Drivers;
