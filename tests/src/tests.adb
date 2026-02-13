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

with Ada.Command_Line;
with Prunt.Bounded_Indefinite_Vectors_Test;
with Prunt.Config.Test;
with Prunt.Gcode_Arguments.Test;
with Prunt.Gcode_Queues.Test;
with Prunt.Motion_Planner.Test;
with Prunt.Moving_Averages.Test;
with Prunt.Thermistors.Test;
with Trendy_Test.Reports;

procedure Tests is
   package Moving_Averages_Float is new Prunt.Moving_Averages (Float);
   package Moving_Averages_Float_Test is new Moving_Averages_Float.Test;
   package Moving_Averages_Long_Float is new Prunt.Moving_Averages (Long_Float);
   package Moving_Averages_Long_Float_Test is new Moving_Averages_Long_Float.Test;

   procedure Xcov_Dump (Name : String) is
   begin
      pragma Annotate (Xcov, Dump_Buffers, "individual_test-" & Name);
      pragma Annotate (Xcov, Reset_Buffers);
   end Xcov_Dump;
begin
   Trendy_Test.Register (Moving_Averages_Float_Test.All_Tests);
   Trendy_Test.Register (Moving_Averages_Long_Float_Test.All_Tests);
   Trendy_Test.Register (Prunt.Bounded_Indefinite_Vectors_Test.All_Tests);
   Trendy_Test.Register (Prunt.Config.Test.All_Tests);
   Trendy_Test.Register (Prunt.Gcode_Arguments.Test.All_Tests);
   Trendy_Test.Register (Prunt.Gcode_Queues.Test.All_Tests);
   Trendy_Test.Register (Prunt.Motion_Planner.Test.All_Tests);
   Trendy_Test.Register (Prunt.Thermistors.Test.All_Tests);

   if Ada.Command_Line.Argument_Count = 1 and then Ada.Command_Line.Argument (1) = "xcov_dump" then
      pragma Annotate (Xcov, Dump_Buffers, "individual_test-tests.(startup)");
      pragma Annotate (Xcov, Reset_Buffers);
      Trendy_Test.Reports.Print_Basic_Report (Trendy_Test.Run (Xcov_Dump'Access));
   else
      Trendy_Test.Reports.Print_Basic_Report (Trendy_Test.Run (null));
      pragma Annotate (Xcov, Dump_Buffers, "all_tests");
   end if;
end Tests;
