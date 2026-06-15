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
--------------------------------------------------

with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Prunt.Integration_Test_Catalog;

procedure Integration_Tests is
   use Ada.Command_Line;

   List_Only : Boolean := False;
   Filter    : Unbounded_String;
   Scenario  : Unbounded_String;

   Scenario_Prefix : constant String := "--scenario=";

   function Starts_With (Source, Prefix : String) return Boolean;

   function Starts_With (Source, Prefix : String) return Boolean is
   begin
      return
        Source'Length >= Prefix'Length
        and then Source (Source'First .. Source'First + Prefix'Length - 1) = Prefix;
   end Starts_With;
begin
   for I in 1 .. Argument_Count loop
      declare
         Arg : constant String := Argument (I);
      begin
         if Arg = "--list" then
            List_Only := True;
         elsif Arg = "xcov_dump" then
            null;
         elsif Starts_With (Arg, Scenario_Prefix) then
            Scenario := To_Unbounded_String (Arg (Arg'First + Scenario_Prefix'Length .. Arg'Last));
         elsif Starts_With (Arg, "--prunt-") then
            null;
         elsif Arg /= "" then
            Filter := To_Unbounded_String (Arg);
         end if;
      end;
   end loop;

   if List_Only then
      for I in 1 .. Prunt.Integration_Test_Catalog.Scenario_Count loop
         declare
            Name : constant String := Prunt.Integration_Test_Catalog.Scenario_Name (I);
         begin
            if Prunt.Integration_Test_Catalog.Matches_Filter (Name, To_String (Filter)) then
               Ada.Text_IO.Put_Line (Name);
            end if;
         end;
      end loop;
   elsif Scenario /= Null_Unbounded_String then
      raise Program_Error with "Use integration_scenario_runner to execute integration scenarios.";
   else
      for I in 1 .. Prunt.Integration_Test_Catalog.Scenario_Count loop
         declare
            Name : constant String := Prunt.Integration_Test_Catalog.Scenario_Name (I);
         begin
            if Prunt.Integration_Test_Catalog.Matches_Filter (Name, To_String (Filter)) then
               Ada.Text_IO.Put_Line (Name);
            end if;
         end;
      end loop;
   end if;
end Integration_Tests;
