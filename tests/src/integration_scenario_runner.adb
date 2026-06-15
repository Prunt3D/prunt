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
with Prunt.Integration_Test_Harness;

procedure Integration_Scenario_Runner is
   use Ada.Command_Line;

   Scenario : Unbounded_String;
   Xcov_Dump : Boolean := False;

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
         if Arg = "xcov_dump" then
            Xcov_Dump := True;
         elsif Starts_With (Arg, Scenario_Prefix) then
            Scenario := To_Unbounded_String (Arg (Arg'First + Scenario_Prefix'Length .. Arg'Last));
         elsif Starts_With (Arg, "--prunt-") then
            null;
         elsif Arg /= "" then
            Scenario := To_Unbounded_String (Arg);
         end if;
      end;
   end loop;

   if Scenario = Null_Unbounded_String then
      raise Program_Error with "Missing --scenario=<name>.";
   end if;

   Prunt.Integration_Test_Harness.Run_Scenario (To_String (Scenario), Xcov_Dump);
end Integration_Scenario_Runner;
