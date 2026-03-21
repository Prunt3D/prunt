--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
--  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
--  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice (including the next paragraph) shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
--  THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.

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
     ["Standard.Boolean"          => (Boolean_Kind, (null record)),
      "Prunt.Dimensionless_Ratio" => (Ratio_Kind, (Unit => (Conversion => "", Display => ""))),
      "Prunt.Default_Modules.Fans.My_Controller_Generic_Types.Fan_Name"
      => (Enum_Kind, (Present_When => [])),
      "Prunt.Default_Modules.Heaters.My_Controller_Generic_Types.Thermistor_Name"
      => (Enum_Kind, (Present_When => [])),
      "Prunt.Default_Modules.Homing.My_Controller_Generic_Types.Input_Switch_Name"
      => (Enum_Kind, (Present_When => [])),
      "Prunt.Default_Modules.Homing.My_Controller_Generic_Types.Motor_Name"
      => (Enum_Kind, (Present_When => []))];
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
