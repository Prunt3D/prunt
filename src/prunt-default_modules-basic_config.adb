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

package body Prunt.Default_Modules.Basic_Config is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema is
   begin
      return (Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   procedure Disable_Prunt (This : in out Module_Instance) is
   begin
      This.Config.Prunt.Enabled := False;
      User_Config_To_Config_Data (This.Config_Data.Get, This.Config);
   end Disable_Prunt;

   function Prunt_Is_Disabled (This : Module_Instance) return Boolean is
   begin
      return This.Config.Prunt.Enabled;
   end Prunt_Is_Disabled;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
   begin
      return
         Result : constant Module_Instance :=
           (My_Modules.Module_Instance
            with Config => Config_Data_To_User_Config (Config_Data.Get), Config_Data => Config_Data)
      do
         if not Result.Config.Prunt.Enabled then
            Report_Config_Error
              (["Prunt", "Enabled"], "Prunt is currently disabled. Enable after configuring all other settings.");
         end if;
      end return;
   end Initialize;

end Prunt.Default_Modules.Basic_Config;
