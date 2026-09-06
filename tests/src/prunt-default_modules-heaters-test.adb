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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Prunt.Thermistors;
with Prunt.Motion_Planner;

package body Prunt.Default_Modules.Heaters.Test is

   type Mock_Planner is new Planner_Interface with record
      Reject_Queue : Boolean := False;
   end record;

   overriding
   function Get_Last_Position (This : Mock_Planner) return Position is (raise Program_Error);
   overriding
   function Get_Last_Kinematic_Parameters (This : Mock_Planner) return Motion_Planner.Kinematic_Parameters
   is (raise Program_Error);
   overriding
   function Get_State_Anchor_Corner_ID (This : Mock_Planner) return Planner_Corner_ID is (raise Program_Error);
   overriding
   function Get_Last_Executed_Corner_ID (This : Mock_Planner) return Planner_Corner_ID is (raise Program_Error);
   overriding
   procedure Mark_Axis_Homed (This : Mock_Planner; Axis : Axis_Name) is null;
   overriding
   procedure Mark_Axis_Unhomed (This : Mock_Planner; Axis : Axis_Name) is null;
   overriding
   function Axis_Is_Homed (This : Mock_Planner; Axis : Axis_Name) return Boolean is (raise Program_Error);
   overriding
   function Cancellation_Is_Active (This : Mock_Planner) return Boolean is (False);
   overriding
   procedure Add_Corner
     (This : Mock_Planner; Pos : Position; Feedrate : Velocity; Dwell_After : Time := 0.0 * s;
      Require_Homed : Boolean := True) is null;
   overriding
   procedure Add_Helix
     (This : Mock_Planner; Center : Position; Pos : Position; Clockwise : Boolean; Feedrate : Velocity;
      Dwell_After : Time := 0.0 * s; Require_Homed : Boolean := True) is null;
   overriding
   procedure Add_Corner_Data (This : Mock_Planner; Corner_Data : Extra_Corner_Data'Class);
   overriding
   procedure Flush
     (This : Mock_Planner;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record));
   overriding
   procedure Resolve_Homing_Move (This : Mock_Planner; Stopped_Position : Position) is null;
   overriding
   procedure Flush_And_Change_Kinematic_Parameters
     (This : Mock_Planner; Params : Motion_Planner.Kinematic_Parameters;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record)) is null;
   overriding
   procedure Flush_And_Reset_Position
     (This : Mock_Planner; New_Position : Position;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record)) is null;

   overriding
   procedure Add_Corner_Data (This : Mock_Planner; Corner_Data : Extra_Corner_Data'Class) is
   begin
      if This.Reject_Queue then
         raise Gcode_Temporarily_Rejected_Error;
      elsif Corner_Data not in Heater_Target_Command then
         raise Program_Error with "Expected a heater target command.";
      end if;
   end Add_Corner_Data;

   overriding
   procedure Flush
     (This : Mock_Planner;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record)) is
   begin
      if This.Reject_Queue then
         raise Gcode_Temporarily_Rejected_Error;
      elsif Extra_Data not in Heater_Temperature_Wait then
         raise Program_Error with "Expected a heater temperature wait.";
      end if;
   end Flush;

   protected type Mock_Thermistors is new My_Modules.Module_Instance_Parent
     and Thermistors_Module.Module_Instance_Interface with
      procedure Set_Temperature (Value : Temperature);
      procedure Set_Failed (Value : Boolean);
      overriding
      function Thermistor_Is_Enabled_In_Config (Thermistor : Thermistor_Name) return Boolean;
      overriding
      function Get_Thermistor_Parameters (Thermistor : Thermistor_Name) return Prunt.Thermistors.Thermistor_Parameters;
      overriding
      function Get_Temperature (Thermistor : Thermistor_Name; Requires_Fresh : Boolean) return Temperature;
   private
      Current : Temperature := 20.0 * celsius;
      Failed : Boolean := False;
   end Mock_Thermistors;

   protected body Mock_Thermistors is
      procedure Set_Temperature (Value : Temperature) is
      begin
         Current := Value;
      end Set_Temperature;

      procedure Set_Failed (Value : Boolean) is
      begin
         Failed := Value;
      end Set_Failed;

      function Thermistor_Is_Enabled_In_Config (Thermistor : Thermistor_Name) return Boolean is
         pragma Unreferenced (Thermistor);
      begin
         return True;
      end Thermistor_Is_Enabled_In_Config;

      function Get_Thermistor_Parameters
        (Thermistor : Thermistor_Name) return Prunt.Thermistors.Thermistor_Parameters is
         pragma Unreferenced (Thermistor);
      begin
         return (Kind => Prunt.Thermistors.Disabled_Kind, Maximum_Temperature => 300.0 * celsius, others => <>);
      end Get_Thermistor_Parameters;

      function Get_Temperature (Thermistor : Thermistor_Name; Requires_Fresh : Boolean) return Temperature is
         pragma Unreferenced (Thermistor);
      begin
         if not Requires_Fresh then
            raise Program_Error with "Extrusion checks must require a fresh reading.";
         elsif Failed then
            raise Constraint_Error with "Thermistor reading is stale.";
         end if;
         return Current;
      end Get_Temperature;
   end Mock_Thermistors;

   procedure Test_Cold_Extrusion (T : in out Trendy_Test.Operation'Class) is
      use type Ada.Real_Time.Time;
      Sensor_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Settings : User_Config;

      function Create_Sensor return My_Modules.Module_Instance_Parent'Class;
      procedure Wait_For_Permission (Expected : Boolean);

      function Create_Sensor return My_Modules.Module_Instance_Parent'Class is
      begin
         return Result : Mock_Thermistors;
      end Create_Sensor;

      procedure Wait_For_Permission (Expected : Boolean) is
         Timeout : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (1.0);
      begin
         while Extrusion_Is_Allowed /= Expected and then Ada.Real_Time.Clock < Timeout loop
            delay 0.01;
         end loop;
         T.Assert (Extrusion_Is_Allowed = Expected, "background sampler publishes the expected permission");
      end Wait_For_Permission;
   begin
      T.Register;
      Sensor_Ref.Set (Create_Sensor'Access);
      declare
         Sensor : Mock_Thermistors renames Mock_Thermistors (Sensor_Ref.Get.Element.all);
      begin
         Sensor.Set_Failed (True);
         T.Assert (Extrusion_Temperature_Is_Safe (Settings, Sensor_Ref), "disabled heaters do not read sensors");
         Settings.Heaters (Heater_Name'First) := (Kind => Enabled, others => <>);
         T.Assert (Extrusion_Temperature_Is_Safe (Settings, Sensor_Ref), "disabled protection does not read sensors");

         Settings.Heaters (Heater_Name'First).Cold_Extrusion_Prevention :=
           (Kind => Enabled, Minimum_Temperature => 170.0 * celsius);
         declare
            Raised : Boolean := False;
            Safe : Boolean;
         begin
            begin
               Safe := Extrusion_Temperature_Is_Safe (Settings, Sensor_Ref);
            exception
               when E : Constraint_Error =>
                  Raised := Ada.Strings.Fixed.Index (Ada.Exceptions.Exception_Message (E), "stale") /= 0;
            end;
            T.Assert (Raised, "the background sample requires fresh temperature data");
         end;
         Sensor.Set_Failed (False);
         T.Assert (not Extrusion_Temperature_Is_Safe (Settings, Sensor_Ref), "cold extrusion is denied");
         Sensor.Set_Temperature (169.9 * celsius);
         T.Assert (not Extrusion_Temperature_Is_Safe (Settings, Sensor_Ref), "below the threshold is denied");
         Sensor.Set_Temperature (170.0 * celsius);
         T.Assert (Extrusion_Temperature_Is_Safe (Settings, Sensor_Ref), "equality is allowed");
         Sensor.Set_Temperature (200.0 * celsius);
         T.Assert (Extrusion_Temperature_Is_Safe (Settings, Sensor_Ref), "above the threshold is allowed");
         Sensor.Set_Temperature (160.0 * celsius);
         T.Assert (not Extrusion_Temperature_Is_Safe (Settings, Sensor_Ref), "cooling revokes permission");
         Settings.Heaters (Heater_Name'First).Cold_Extrusion_Prevention.Minimum_Temperature := 150.0 * celsius;
         T.Assert (Extrusion_Temperature_Is_Safe (Settings, Sensor_Ref), "the configured minimum is used");

         Sensor.Set_Failed (True);
         Extrusion_Allowed_Until := Ada.Real_Time.Time_Last;
         T.Assert (Extrusion_Is_Allowed, "disabled cached guard does not read the sensor");
         Extrusion_Allowed_Until := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (10.0);
         T.Assert (Extrusion_Is_Allowed, "valid cached permission does not read the sensor");
         Extrusion_Allowed_Until := Ada.Real_Time.Clock - Ada.Real_Time.To_Time_Span (1.0);
         T.Assert (not Extrusion_Is_Allowed, "expired permission fails closed even if the sampler stalls");
         Extrusion_Allowed_Until := Ada.Real_Time.Time_First;
         T.Assert (not Extrusion_Is_Allowed, "uninitialized sampling denies extrusion");
         Sensor.Set_Failed (False);

         declare
            Monitor : Extrusion_Temperature_Monitor_Pointers.Ref;

            function Make_Monitor return Extrusion_Temperature_Monitor_Wrapper;

            function Make_Monitor return Extrusion_Temperature_Monitor_Wrapper is
            begin
               return Result : Extrusion_Temperature_Monitor_Wrapper;
            end Make_Monitor;
         begin
            Monitor.Set (Make_Monitor'Access);
            Sensor.Set_Temperature (20.0 * celsius);
            Monitor.Get.Monitor.Start (Settings, Sensor_Ref);
            T.Assert (not Extrusion_Is_Allowed, "starting the sampler denies extrusion until a successful sample");
            --  Simulate a non-waiting heat command followed by other moves before extrusion is executed.
            Sensor.Set_Temperature (200.0 * celsius);
            Wait_For_Permission (True);
            Sensor.Set_Temperature (20.0 * celsius);
            Wait_For_Permission (False);
            Sensor.Set_Temperature (200.0 * celsius);
            Wait_For_Permission (True);
         end;
         T.Assert (not Extrusion_Is_Allowed, "stopping the sampler revokes permission");
         Extrusion_Allowed_Until := Ada.Real_Time.Time_Last;
      end;
   end Test_Cold_Extrusion;

   procedure Test_Cold_Extrusion_Setpoints (T : in out Trendy_Test.Operation'Class) is
      Device : constant Module := (My_Modules.Module with null record);
      Status : constant Status_Manager.Status_Data_Collection :=
        Status_Manager.Build_Collection (["Heaters" => Device.Status_Schema]);
      Settings : User_Config;
      Sensor_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Heater_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      H : constant Heater_Name := Heater_Name'First;
      Planner : Mock_Planner;

      function Create_Sensor return My_Modules.Module_Instance_Parent'Class;
      function Create_Heater return My_Modules.Module_Instance_Parent'Class;
      procedure Queue_Target (Value : Temperature);
      procedure Queue_Wait (Value : Temperature);
      procedure Expect_Rejection;

      function Create_Sensor return My_Modules.Module_Instance_Parent'Class is
      begin
         return Result : Mock_Thermistors do
            Result.Set_Failed (True);
         end return;
      end Create_Sensor;

      function Create_Heater return My_Modules.Module_Instance_Parent'Class is
      begin
         return Result : Module_Instance do
            Result.Initialize
              (Settings, Status.Get_Emitter ("Heaters"), Sensor_Ref,
               My_Modules.Module_Instance_Shared_Pointers.Null_Ref);
         end return;
      end Create_Heater;

      procedure Queue_Target (Value : Temperature) is
      begin
         Set_Hotend_Temperature (Heater_Ref, Planner, Value / celsius);
      end Queue_Target;

      procedure Queue_Wait (Value : Temperature) is
      begin
         Wait_For_Hotend_Temperature_Heat (Heater_Ref, Planner, Value / celsius);
      end Queue_Wait;

      procedure Expect_Rejection is
         Rejected : Boolean := False;
      begin
         begin
            Module_Instance (Heater_Ref.Get.Element.all).Validate_Extrusion_Setpoints;
         exception
            when E : Gcode_Bad_Inputs_Error =>
               Rejected :=
                 Ada.Strings.Fixed.Index (Ada.Exceptions.Exception_Message (E), "queued setpoint") /= 0
                 and then Ada.Strings.Fixed.Index (Ada.Exceptions.Exception_Message (E), H'Image) /= 0;
         end;
         T.Assert (Rejected, "low queued setpoints produce a user-facing rejection identifying the heater");
      end Expect_Rejection;
   begin
      T.Register;
      Sensor_Ref.Set (Create_Sensor'Access);
      Heater_Ref.Set (Create_Heater'Access);
      Module_Instance (Heater_Ref.Get.Element.all).Validate_Extrusion_Setpoints;
      Settings.Gcode_Defaults := (others => (Kind => Enabled, Heater => H));
      Settings.Heaters (H) := (Kind => Enabled, others => <>);
      Heater_Ref.Set (Create_Heater'Access);
      Module_Instance (Heater_Ref.Get.Element.all).Validate_Extrusion_Setpoints;

      Settings.Heaters (H).Cold_Extrusion_Prevention := (Kind => Enabled, Minimum_Temperature => 170.0 * celsius);
      Heater_Ref.Set (Create_Heater'Access);
      declare
         Instance : Module_Instance renames Module_Instance (Heater_Ref.Get.Element.all);
      begin
         Expect_Rejection;
         --  Exercise every command handler, including queue failure before its planned target is recorded.
         for Command in 1 .. 9 loop
            for Reject in Boolean loop
               Instance.Record_Planned_Target (H, 0.0 * celsius);
               Planner.Reject_Queue := Reject;
               declare
                  Rejected : Boolean := False;
               begin
                  begin
                     case Command is
                        when 1 => Set_Hotend_Temperature (Heater_Ref, Planner, 200.0);
                        when 2 => Set_Bed_Temperature (Heater_Ref, Planner, 200.0);
                        when 3 => Set_Chamber_Temperature (Heater_Ref, Planner, 200.0);
                        when 4 => Wait_For_Hotend_Temperature_Heat (Heater_Ref, Planner, 200.0);
                        when 5 => Wait_For_Hotend_Temperature_Heat_Or_Cool (Heater_Ref, Planner, 200.0);
                        when 6 => Wait_For_Bed_Temperature_Heat (Heater_Ref, Planner, 200.0);
                        when 7 => Wait_For_Bed_Temperature_Heat_Or_Cool (Heater_Ref, Planner, 200.0);
                        when 8 => Wait_For_Chamber_Temperature_Heat (Heater_Ref, Planner, 200.0);
                        when 9 => Wait_For_Chamber_Temperature_Heat_Or_Cool (Heater_Ref, Planner, 200.0);
                     end case;
                  exception
                     when Gcode_Temporarily_Rejected_Error => Rejected := True;
                  end;
                  T.Assert (Rejected = Reject, "handler propagates queue rejection");
                  if Reject then
                     Expect_Rejection;
                  else
                     Instance.Validate_Extrusion_Setpoints;
                  end if;
               end;
            end loop;
         end loop;
         Planner.Reject_Queue := False;
         Queue_Target (200.0 * celsius);
         Instance.Validate_Extrusion_Setpoints;
         Instance.Record_Heater_Target (H, 0.0 * celsius);
         Instance.Validate_Extrusion_Setpoints;
         --  An earlier command executing must not overwrite a newer queued M104 target.
         Queue_Target (150.0 * celsius);
         Instance.Record_Heater_Target (H, 200.0 * celsius);
         Expect_Rejection;
         Queue_Target (170.0 * celsius);
         Instance.Validate_Extrusion_Setpoints;
         Queue_Wait (200.0 * celsius);
         Instance.Validate_Extrusion_Setpoints;
         Queue_Wait (169.9 * celsius);
         Expect_Rejection;

         Instance.Handle_Cancel (0, 0, [others => 0.0 * mm]);
         Instance.Validate_Extrusion_Setpoints;
         --  Cancelling the queued low target restores the applied 200 C target.
         Instance.Record_Heater_Target (H, 0.0 * celsius);
         Queue_Target (200.0 * celsius);
         Instance.Handle_Cancel (0, 0, [others => 0.0 * mm]);
         Expect_Rejection;

         Instance.Record_Heater_Target (H, 200.0 * celsius);
         Instance.Save_Pause_Targets;
         Instance.Record_Heater_Target (H, 0.0 * celsius);
         Queue_Target (150.0 * celsius);
         Instance.Handle_Cancel (0, 0, [others => 0.0 * mm]);
         Instance.Validate_Extrusion_Setpoints;
         --  Resume restores the pre-pause target, not the temporary pause override.
         Instance.Clear_Pause_Targets;
         Instance.Handle_Cancel (0, 0, [others => 0.0 * mm]);
         Expect_Rejection;
      end;
   end Test_Cold_Extrusion_Setpoints;

   function All_Tests return Trendy_Test.Test_Group is
     (Trendy_Test.Test_Group'
        [Test_Cold_Extrusion'Unrestricted_Access, Test_Cold_Extrusion_Setpoints'Unrestricted_Access]);

end Prunt.Default_Modules.Heaters.Test;
