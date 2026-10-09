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

with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Real_Time; use Ada.Real_Time;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Trendy_Test; use Trendy_Test;
with VSS.Strings.Conversions;

package body Prunt.Smart_Plugs.Test is

   package Commands is new Ada.Containers.Indefinite_Vectors (Positive, String);
   use type Commands.Vector;
   --  Tests sharing this mock transport register for sequential execution.
   Calls : Commands.Vector;
   Device_On : Boolean := False;
   Indexed : Boolean := False;
   Fail_Command : Virtual_String;
   Bad_Boot : Boolean := False;
   Bad_Timer : Boolean := False;
   Fail_After_On : Boolean := False;
   Switch_Off_During_Feed : Boolean := False;

   Test_Config : constant Plug_Config :=
     (Enabled => True, Host => "10.1.1.29", Watchdog_Seconds => 60, others => <>);

   procedure Test_Boot_Off_Must_Be_Confirmed (T : in out Operation'Class);
   procedure Test_Expired_Lease_Never_Repowers (T : in out Operation'Class);
   procedure Test_External_Off_Is_Respected (T : in out Operation'Class);
   procedure Test_Failed_Feed_Disarms (T : in out Operation'Class);
   procedure Test_Failed_Off_Is_Retried (T : in out Operation'Class);
   procedure Test_Invalid_Or_Disabled_Config (T : in out Operation'Class);
   procedure Test_Lost_On_Response_Retries_Only_Off (T : in out Operation'Class);
   procedure Test_Manual_Off_Stops_Refresh (T : in out Operation'Class);
   procedure Test_Manual_On_Arms_Before_Power (T : in out Operation'Class);
   procedure Test_Startup_Only_Observes (T : in out Operation'Class);
   procedure Test_Timeout_Encoding (T : in out Operation'Class);
   procedure Test_Timer_Must_Be_Confirmed (T : in out Operation'Class);

   procedure Test_Unconfigured_Controller (T : in out Operation'Class);
   procedure Test_Reconfiguration_Discards_Pending_Commands (T : in out Operation'Class);
   procedure Test_Disabled_Worker_Shutdown (T : in out Operation'Class);

   function All_Tests return Test_Group is
   begin
      return [Test_Manual_On_Arms_Before_Power'Unrestricted_Access, Test_Startup_Only_Observes'Unrestricted_Access,
              Test_Manual_Off_Stops_Refresh'Unrestricted_Access, Test_Timer_Must_Be_Confirmed'Unrestricted_Access,
              Test_Boot_Off_Must_Be_Confirmed'Unrestricted_Access,
              Test_Lost_On_Response_Retries_Only_Off'Unrestricted_Access,
              Test_Failed_Feed_Disarms'Unrestricted_Access, Test_Expired_Lease_Never_Repowers'Unrestricted_Access,
              Test_External_Off_Is_Respected'Unrestricted_Access, Test_Failed_Off_Is_Retried'Unrestricted_Access,
              Test_Timeout_Encoding'Unrestricted_Access, Test_Invalid_Or_Disabled_Config'Unrestricted_Access,
              Test_Unconfigured_Controller'Unrestricted_Access,
              Test_Reconfiguration_Discards_Pending_Commands'Unrestricted_Access,
              Test_Disabled_Worker_Shutdown'Unrestricted_Access];
   end All_Tests;

   function Handle_Command (Host : Virtual_String; Command : String) return JSON_Value is
      pragma Unreferenced (Host);
      Reply : constant JSON_Value := Create_Object;
   begin
      Calls.Append (Command);
      if +Command = Fail_Command then
         raise Constraint_Error with "Simulated network timeout";
      end if;
      if Command = "PowerOnState 0" then
         Reply.Set_Field ("PowerOnState", Integer'(if Bad_Boot then 3 else 0));
      elsif Command = "PulseTime1 160" or else Command = "PulseTime1 130" or else Command = "PulseTime1 3700" then
         declare
            Timer : constant JSON_Value := Create_Object;
         begin
            Timer.Set_Field
              ("Set", (if Bad_Timer then 0 else Integer'Value (Command (Command'First + 11 .. Command'Last))));
            Reply.Set_Field ("PulseTime1", Timer);
            if Switch_Off_During_Feed then
               Device_On := False;
            end if;
         end;
      elsif Command = "Power1" or else Command = "Power1 ON" or else Command = "Power1 OFF" then
         if Command = "Power1 ON" then
            Device_On := True;
            if Fail_After_On then
               raise Constraint_Error with "Reply lost after switching on";
            end if;
         elsif Command = "Power1 OFF" then
            Device_On := False;
         end if;
         Reply.Set_Field ((if Indexed then "POWER1" else "POWER"), (if Device_On then "ON" else "OFF"));
      else
         raise Constraint_Error with "Unexpected command: " & Command;
      end if;
      return Reply;
   end Handle_Command;

   function Request (Host : Virtual_String; Path : String) return JSON_Value is
      use Ada.Strings.Unbounded;
      Decoded : Unbounded_String := To_Unbounded_String (Path);
      Space : Natural;
   begin
      if Ada.Strings.Fixed.Index (Path, "/cm?cmnd=") /= Path'First then
         raise Constraint_Error with "Unexpected command path";
      end if;
      Delete (Decoded, 1, 9);
      loop
         Space := Index (Decoded, "%20");
         exit when Space = 0;
         Replace_Slice (Decoded, Space, Space + 2, " ");
      end loop;
      return Handle_Command (Host, To_String (Decoded));
   end Request;

   procedure Poll_Plug is new Poll (Request);

   procedure Reset is
   begin
      Calls.Clear;
      Device_On := False;
      Indexed := False;
      Fail_Command := "";
      Bad_Boot := False;
      Bad_Timer := False;
      Fail_After_On := False;
      Switch_Off_During_Feed := False;
   end Reset;

   procedure Test_Boot_Off_Must_Be_Confirmed (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Bad_Boot := True;
      Poll_Plug (Test_Config, State, Turn_On);
      T.Assert (not State.Armed and then not Device_On and then not State.Error.Is_Empty);
      T.Assert (Calls = Commands.Vector'["PowerOnState 0"]);
   end Test_Boot_Off_Must_Be_Confirmed;

   procedure Test_Disabled_Worker_Shutdown (T : in out Operation'Class) is
   begin
      T.Register;
      declare
         Control : aliased Controller;
         Worker_Task : Worker (Control'Access);
         Accepted : Boolean;
      begin
         Control.Configure ((others => <>));
         Control.Switch (True, Accepted);
         T.Assert (not Accepted);
         T.Assert (not Boolean'(Read (VSS.Strings.Conversions.To_UTF_8_String (Control.Status_JSON)).Get ("Enabled")));
         Worker_Task.Stop;
      exception
         when others =>
            abort Worker_Task;
            raise;
      end;
   end Test_Disabled_Worker_Shutdown;

   procedure Test_Expired_Lease_Never_Repowers (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Poll_Plug (Test_Config, State, Turn_On);
      State.Last_Feed := Clock - Seconds (61);
      Calls.Clear;
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (Calls = Commands.Vector'["Power1 OFF"]);
      T.Assert (not Device_On and then not State.Armed and then not State.Error.Is_Empty);
   end Test_Expired_Lease_Never_Repowers;

   procedure Test_External_Off_Is_Respected (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Poll_Plug (Test_Config, State, Turn_On);
      Device_On := False;
      Calls.Clear;
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (not State.Armed and then Calls = Commands.Vector'["Power1"]);
      Poll_Plug (Test_Config, State, Turn_On);
      Switch_Off_During_Feed := True;
      Calls.Clear;
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (not Device_On and then State.Error.Is_Empty);
      T.Assert (Calls = Commands.Vector'["Power1", "PulseTime1 160"]);
   end Test_External_Off_Is_Respected;

   procedure Test_Failed_Feed_Disarms (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Poll_Plug (Test_Config, State, Turn_On);
      Fail_Command := "PulseTime1 160";
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (not State.Armed and then State.Power = Unknown and then State.Retry_Off);
      Calls.Clear;
      Fail_Command := "";
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (Calls = Commands.Vector'["Power1 OFF"]);
      T.Assert (not Device_On and then not State.Armed);
   end Test_Failed_Feed_Disarms;

   procedure Test_Failed_Off_Is_Retried (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Poll_Plug (Test_Config, State, Turn_On);
      Fail_Command := "Power1 OFF";
      Poll_Plug (Test_Config, State, Turn_Off);
      T.Assert (not State.Armed and then State.Retry_Off);
      Fail_Command := "";
      Calls.Clear;
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (Calls = Commands.Vector'["Power1 OFF"]);
      T.Assert (not Device_On and then not State.Retry_Off);
   end Test_Failed_Off_Is_Retried;

   procedure Test_Invalid_Or_Disabled_Config (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      T.Assert (Valid_Host ("10.1.1.29"));
      T.Assert (Valid_Host ("plug.local:8080"));
      T.Assert (not Valid_Host (""));
      T.Assert (not Valid_Host ("10.1.1.29/cm?cmnd=Power1"));
      T.Assert (not Valid_Host ("user@10.1.1.29"));
      T.Assert (not Valid_Host ("10.1.1.29:65536"));
      T.Assert (not Valid_Host ("10.1.1.29:"));
      T.Assert (not Valid_Host ("10.1.1.29:0"));
      Poll_Plug ((Test_Config with delta Enabled => False), State, Turn_On);
      T.Assert (Calls.Is_Empty and then not State.Error.Is_Empty);
      Poll_Plug ((Test_Config with delta Host => "http://10.1.1.29"), State, Turn_On);
      T.Assert (Calls.Is_Empty and then not State.Error.Is_Empty);
   end Test_Invalid_Or_Disabled_Config;

   procedure Test_Lost_On_Response_Retries_Only_Off (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Fail_After_On := True;
      Poll_Plug (Test_Config, State, Turn_On);
      T.Assert (not State.Armed and then Device_On and then State.Retry_Off);
      Calls.Clear;
      Poll_Plug (Test_Config, State, No_Request);
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (not Device_On and then not State.Armed);
      T.Assert (Calls = Commands.Vector'["Power1 OFF", "Power1"]);
   end Test_Lost_On_Response_Retries_Only_Off;

   procedure Test_Manual_Off_Stops_Refresh (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Poll_Plug (Test_Config, State, Turn_On);
      Calls.Clear;
      Poll_Plug (Test_Config, State, Turn_Off);
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (not State.Armed and then State.Power = Off);
      T.Assert (Calls = Commands.Vector'["Power1 OFF", "Power1"]);
   end Test_Manual_Off_Stops_Refresh;

   procedure Test_Manual_On_Arms_Before_Power (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Poll_Plug (Test_Config, State, Turn_On);
      T.Assert (State.Armed and then State.Power = On and then State.Error.Is_Empty);
      T.Assert (Calls = Commands.Vector'["PowerOnState 0", "PulseTime1 160", "Power1 ON"]);
      Calls.Clear;
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (Calls = Commands.Vector'["Power1", "PulseTime1 160"]);
      T.Assert (State.Armed and then Device_On);
   end Test_Manual_On_Arms_Before_Power;

   procedure Test_Reconfiguration_Discards_Pending_Commands (T : in out Operation'Class) is
      State : Shared_State;
      Accepted : Boolean;
      Config : Plug_Config;
      Action : Power_Request;
      Replacement : constant Plug_Config := (Test_Config with delta Host => "replacement.local");
   begin
      T.Register;
      State.Configure (Test_Config);
      State.Publish ((Config => Test_Config, Power => Off, others => <>));
      State.Submit (Turn_On, Accepted);
      T.Assert (Accepted);
      State.Configure (Replacement);
      State.Take (Config, Action);
      T.Assert (Config = Replacement and then Action = No_Request,
                "a queued command for the old device must not operate its replacement");
      State.Submit (Turn_On, Accepted);
      T.Assert (not Accepted, "commands wait until changed settings have been applied");
      State.Publish ((Config => Test_Config, Power => On, Watchdog_Active => True, others => <>));
      T.Assert (State.Snapshot.Busy and then not State.Snapshot.Watchdog_Active,
                "a result for the previous device must not appear as the replacement's status");
      State.Publish ((Config => Replacement, Power => Off, others => <>));
      State.Submit (Turn_Off, Accepted);
      T.Assert (Accepted);
      State.Configure ((others => <>));
      State.Submit (Turn_On, Accepted);
      T.Assert (not Accepted and then not State.Snapshot.Config.Enabled);
   end Test_Reconfiguration_Discards_Pending_Commands;

   procedure Test_Startup_Only_Observes (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Device_On := True;
      Indexed := True;
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (State.Power = On and then not State.Armed);
      T.Assert (Calls = Commands.Vector'["Power1"]);
   end Test_Startup_Only_Observes;

   procedure Test_Timeout_Encoding (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Poll_Plug ((Test_Config with delta Watchdog_Seconds => 30), State, Turn_On);
      T.Assert (State.Armed and then Calls (2) = "PulseTime1 130");
      Calls.Clear;
      Poll_Plug ((Test_Config with delta Watchdog_Seconds => 3600), State, Turn_On);
      T.Assert (State.Armed and then Calls (2) = "PulseTime1 3700");
   end Test_Timeout_Encoding;

   procedure Test_Timer_Must_Be_Confirmed (T : in out Operation'Class) is
      State : Runtime_State;
   begin
      T.Register (Parallelize => False);
      Reset;
      Bad_Timer := True;
      Poll_Plug (Test_Config, State, Turn_On);
      T.Assert (not State.Armed and then not Device_On and then not State.Error.Is_Empty);
      T.Assert (Calls = Commands.Vector'["PowerOnState 0", "PulseTime1 160"]);
      Poll_Plug (Test_Config, State, No_Request);
      T.Assert (Calls.Last_Element = "Power1 OFF" and then State.Power = Off);
   end Test_Timer_Must_Be_Confirmed;

   procedure Test_Unconfigured_Controller (T : in out Operation'Class) is
      Control : Controller;
      Accepted : Boolean;
   begin
      T.Register;
      Control.Switch (True, Accepted);
      T.Assert (not Accepted, "an absent configuration module cannot switch power");
      T.Assert (not Boolean'(Read (VSS.Strings.Conversions.To_UTF_8_String (Control.Status_JSON)).Get ("Enabled")));
   end Test_Unconfigured_Controller;

end Prunt.Smart_Plugs.Test;
