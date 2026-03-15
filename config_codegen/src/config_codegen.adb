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

with Ada.Text_IO;         use Ada.Text_IO;
with Config_Generator;
with Config_Parser;
with Config_Types;        use Config_Types;
with Libadalang.Analysis; use Libadalang.Analysis;
with VSS.Strings;         use VSS.Strings;
with VSS.Strings.Conversions;
with Libadalang.Helpers;

procedure Config_Codegen is
   pragma Extensions_Allowed (On);

   Modules : Module_Data_Vectors.Vector := [];
   Config : Config_Maps.Map :=
     ["Standard.Boolean"                                                           => (Boolean_Kind, (null record)),
      "Prunt.Dimensionless_Ratio"                                                  => (Ratio_Kind, (Unit => "")),
      "Prunt.Default_Modules.Homing.My_Controller_Generic_Types.Input_Switch_Name" => (Enum_Kind, (Present_When => [])),
      "Prunt.Default_Modules.Homing.My_Controller_Generic_Types.Motor_Name"        => (Enum_Kind, (Present_When => []))];
   --  TODO: We should properly resolve these.

   procedure Process_Unit (Context : Libadalang.Helpers.App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);
      Filename : constant String := Unit.Get_Filename;
   begin
      if Filename'Length > 4 and then Filename (Filename'Last - 3 .. Filename'Last) = ".ads" then
         Put_Line ("Parsing " & Filename);
         declare
            Data : constant Module_Data := Config_Parser.Parse (Unit.Context, Filename);
         begin
            if Data.Name /= "" then
               Modules.Append (Data);
               for C in Data.Config.Iterate loop
                  if not Config.Contains (Config_Maps.Key (C)) then
                     Config.Insert (Config_Maps.Key (C), Config_Maps.Element (C));
                  end if;
               end loop;
            end if;
         end;
      end if;
   end Process_Unit;

   package App is new
     Libadalang.Helpers.App
       (Name         => "config_codegen",
        Description  => "Generates Prunt configuration code from Ada specs.",
        Process_Unit => Process_Unit);
begin
   App.Run;

   for Mod_Data of Modules loop
      Put_Line ("Generating for " & VSS.Strings.Conversions.To_UTF_8_String (Mod_Data.Name));
      Config_Generator.Generate_Files (VSS.Strings.Conversions.To_UTF_8_String (Mod_Data.Filename), Mod_Data, Config);
   end loop;
end Config_Codegen;
