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

package body Prunt.Default_Modules.TMC_Drivers is

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
      My_Config                         : constant User_Config := Config_Data_To_User_Config (Config_Data.Get);
      Motor_Drivers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Motor_Drivers_Module.Module_Instance'Tag);
      Motor_Drivers_Module_Instance     : Motor_Drivers_Module.Module_Instance renames
        Motor_Drivers_Module.Module_Instance (Motor_Drivers_Module_Instance_Ref.Get.Element.all);

      Motor_Enabled_In_Config : Motor_Enabled_Map :=
        (for M in My_Controller_Generic_Types.Motor_Name =>
           Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M));
      --  We need to hold on to this so we know which motors to set registers on in the `Start` procedure, but we do
      --  not need to hold on to the entire base motor module instance as we do not use anything else from it.
   begin
      for M in My_Controller_Generic_Types.Motor_Name when Motor_Enabled_In_Config (M) loop
         case My_Config.Motors (M).Fixed_Kind is
            when TMC2240_UART_Kind =>
               --  TODO: Check params in procedure.
               null;

            when others            =>
               null;
         end case;
      end loop;

      return
        Module_Instance'
          (My_Modules.Module_Instance with Config => My_Config, Motor_Enabled_In_Config => Motor_Enabled_In_Config);
   end Initialize;

   overriding
   procedure Start (This : in out Module_Instance) is
   begin
      --  TODO
      null;
   end Start;

end Prunt.Default_Modules.TMC_Drivers;
