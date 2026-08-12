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
with Prunt.Bounded_Indefinite_Queues_Test;
with Prunt.Bounded_Indefinite_Vectors_Test;
with Prunt.Config.Test;
with Prunt.Controller_Generic_Types;
with Prunt.Default_Modules;
with Prunt.Default_Modules.Config_Saving;
with Prunt.Default_Modules.Idle_Emitter;
with Prunt.Default_Modules.Machine_Idle_Timeout;
with Prunt.Default_Modules.Machine_Idle_Timeout.Test;
with Prunt.Dummy_Allocator.Test;
with Prunt.Exception_Occurrence_Holders.Test;
with Prunt.Gcode_Arguments.Test;
with Prunt.Gcode_Queues.Test;
with Prunt.Generic_Lock.Test;
with Prunt.Indefinite_Ordered_Maps_With_Insertion_Order_Test;
with Prunt.Integration_Config_Overlays.Test;
with Prunt.Logger;
with Prunt.Logger.Test_Control;
with Prunt.Motion_Planner.Corner_Transitions.Test;
with Prunt.Motion_Planner.Test;
with Prunt.Motion_Planner.Stereographic_Curves.Test;
with Prunt.Motion_Planner.Planner_Primitive_Jet_Test;
with Prunt.Moving_Averages.Test;
with Prunt.Thermistors.Test;
with Trendy_Test.Reports;

procedure Tests is
   type Machine_Idle_Timeout_Test_Name is (Only_Item);
   pragma Unreferenced (Only_Item);

   package Machine_Idle_Timeout_Test_Controller_Types is new
     Prunt.Controller_Generic_Types
       (Motor_Name                   => Machine_Idle_Timeout_Test_Name,
        Heater_Name                  => Machine_Idle_Timeout_Test_Name,
        Thermistor_Name              => Machine_Idle_Timeout_Test_Name,
        Board_Temperature_Probe_Name => Machine_Idle_Timeout_Test_Name,
        Fan_Name                     => Machine_Idle_Timeout_Test_Name,
        Tachometer_Name              => Machine_Idle_Timeout_Test_Name,
        Input_Switch_Name            => Machine_Idle_Timeout_Test_Name);
   package Machine_Idle_Timeout_Test_Logger is new Prunt.Logger;
   package Machine_Idle_Timeout_Test_Logger_Control is new Machine_Idle_Timeout_Test_Logger.Test_Control;
   package Machine_Idle_Timeout_Test_Default_Modules is new
     Prunt.Default_Modules
       (My_Modules => Machine_Idle_Timeout_Test_Controller_Types.My_Modules,
        My_Logger  => Machine_Idle_Timeout_Test_Logger);
   package Machine_Idle_Timeout_Test_Config_Saving is new Machine_Idle_Timeout_Test_Default_Modules.Config_Saving;
   Timeout_Report_Count : Natural := 0 with Atomic, Volatile;

   function Get_Timeout_Report_Count return Natural is (Timeout_Report_Count);

   procedure Request_Machine_Idle_Timeout_Shutdown (Message : String) is
      pragma Unreferenced (Message);
   begin
      Timeout_Report_Count := @ + 1;
   end Request_Machine_Idle_Timeout_Shutdown;

   procedure Reset_Timeout_Report_Count is
   begin
      Timeout_Report_Count := 0;
   end Reset_Timeout_Report_Count;

   package Machine_Idle_Timeout_Test_Idle_Emitter is new Machine_Idle_Timeout_Test_Default_Modules.Idle_Emitter;
   package Machine_Idle_Timeout_Test_Module is new
     Machine_Idle_Timeout_Test_Default_Modules.Machine_Idle_Timeout
       (Config_Saving_Module => Machine_Idle_Timeout_Test_Config_Saving,
        Idle_Emitter_Module  => Machine_Idle_Timeout_Test_Idle_Emitter,
        Request_Shutdown     => Request_Machine_Idle_Timeout_Shutdown);
   package Machine_Idle_Timeout_Test is new
     Machine_Idle_Timeout_Test_Module.Test
       (Get_Report_Count => Get_Timeout_Report_Count, Reset_Report_Count => Reset_Timeout_Report_Count);

   package Moving_Averages_Float is new Prunt.Moving_Averages (Float);
   package Moving_Averages_Float_Test is new Moving_Averages_Float.Test;
   package Moving_Averages_Long_Float is new Prunt.Moving_Averages (Long_Float);
   package Moving_Averages_Long_Float_Test is new Moving_Averages_Long_Float.Test;

   package Generic_Lock is new Prunt.Generic_Lock;
   package Generic_Lock_Test is new Generic_Lock.Test;

   function Is_Xcov_Dump return Boolean;

   function Filter return String is
   begin
      if Is_Xcov_Dump then
         if Ada.Command_Line.Argument_Count >= 2 then
            return Ada.Command_Line.Argument (2);
         else
            return "";
         end if;
      elsif Ada.Command_Line.Argument_Count >= 1 then
         return Ada.Command_Line.Argument (1);
      else
         return "";
      end if;
   end Filter;

   function Is_Xcov_Dump return Boolean is
     (Ada.Command_Line.Argument_Count >= 1 and then Ada.Command_Line.Argument (1) = "xcov_dump");

   procedure Xcov_Dump (Name : String) is
   begin
      pragma Annotate (Xcov, Dump_Buffers, "individual_test-" & Name);
      pragma Annotate (Xcov, Reset_Buffers);
   end Xcov_Dump;
begin
   Trendy_Test.Register (Generic_Lock_Test.All_Tests);
   Trendy_Test.Register (Machine_Idle_Timeout_Test.All_Tests);
   Trendy_Test.Register (Moving_Averages_Float_Test.All_Tests);
   Trendy_Test.Register (Moving_Averages_Long_Float_Test.All_Tests);
   Trendy_Test.Register (Prunt.Bounded_Indefinite_Queues_Test.All_Tests);
   Trendy_Test.Register (Prunt.Bounded_Indefinite_Vectors_Test.All_Tests);
   Trendy_Test.Register (Prunt.Config.Test.All_Tests);
   Trendy_Test.Register (Prunt.Dummy_Allocator.Test.All_Tests);
   Trendy_Test.Register (Prunt.Exception_Occurrence_Holders.Test.All_Tests);
   Trendy_Test.Register (Prunt.Gcode_Arguments.Test.All_Tests);
   Trendy_Test.Register (Prunt.Gcode_Queues.Test.All_Tests);
   Trendy_Test.Register (Prunt.Indefinite_Ordered_Maps_With_Insertion_Order_Test.All_Tests);
   Trendy_Test.Register (Prunt.Integration_Config_Overlays.Test.All_Tests);
   Trendy_Test.Register (Prunt.Motion_Planner.Corner_Transitions.Test.All_Tests);
   Trendy_Test.Register (Prunt.Motion_Planner.Stereographic_Curves.Test.All_Tests);
   Trendy_Test.Register (Prunt.Motion_Planner.Planner_Primitive_Jet_Test.All_Tests);
   Trendy_Test.Register (Prunt.Motion_Planner.Test.All_Tests);
   Trendy_Test.Register (Prunt.Thermistors.Test.All_Tests);

   if Is_Xcov_Dump then
      pragma Annotate (Xcov, Dump_Buffers, "individual_test-tests.(startup)");
      pragma Annotate (Xcov, Reset_Buffers);
      Trendy_Test.Reports.Print_Basic_Report (Trendy_Test.Run (Xcov_Dump'Access, Filter));
   else
      Trendy_Test.Reports.Print_Basic_Report (Trendy_Test.Run (Filter => Filter));
      pragma Annotate (Xcov, Dump_Buffers, "all_tests");
   end if;
   Machine_Idle_Timeout_Test_Logger_Control.Stop;
exception
   when others =>
      Machine_Idle_Timeout_Test_Logger_Control.Stop;
      raise;
end Tests;
