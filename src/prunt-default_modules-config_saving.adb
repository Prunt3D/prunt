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

package body Prunt.Default_Modules.Config_Saving is

   pragma Extensions_Allowed (On);

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
   begin
      return Result : Module_Instance;
   end Initialize;

   protected body Module_Instance is
      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref) is
      begin
         Self_Ref := Self_Ref_In;
      end Start;

      procedure Register_For_Saving (Config_Data : Config.Config_Data) is
      begin
         Configs_To_Save.Insert (Config_Data.Module_Name, Config_Data);
      end Register_For_Saving;

      procedure Save_Settings (Planner : Planner_Interface'Class) is
      begin
         for C of Configs_To_Save loop
            Planner.Flush (Config_Save_Event'(Config_To_Save => C));
         end loop;
      end Save_Settings;

      procedure Save_Settings (Planner : Planner_Interface'Class; I : Virtual_String) is
      begin
         if not Configs_To_Save.Contains (I) then
            raise Gcode_Bad_Inputs_Error
              with "Module """ & Conversions.To_UTF_8_String (I) & """ not known or does not have savable settings.";
         end if;

         Planner.Flush (Config_Save_Event'(Config_To_Save => Configs_To_Save (I)));
      end Save_Settings;

      procedure Save_Settings (Planner : Planner_Interface'Class; I : Gcode_No_Value) is
         Module_List : Virtual_String := "Modules with savable settings: ";
      begin
         for C in Configs_To_Save.Iterate loop
            Module_List := @ & C.Key & (if C.Key = Configs_To_Save.Last_Key then +"" else +", ");
         end loop;

         Planner.Flush (Config_List_Event'(Config_List => Module_List));
      end Save_Settings;
   end Module_Instance;

end Prunt.Default_Modules.Config_Saving;
