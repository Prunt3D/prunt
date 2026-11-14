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

with Trendy_Test; use Trendy_Test;
with Ada.Containers.Indefinite_Vectors;
with Prunt.Motion_Planner;
with VSS.Strings.Conversions;

package body Prunt.Default_Modules.Motor_Drivers.Test is

   Trace : Virtual_String;
   --  Receives planner calls, block-end waits and motor handler calls in the order they happen.

   package Event_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Extra_Block_Resetting_Data'Class);

   Pending : Event_Vectors.Vector;
   --  Block-end events flushed to the mock planner that the executor has not reached yet.

   Fail_Flush : Boolean := False;
   --  Makes the mock planner's Flush raise, as it does when cancellation or shutdown releases a blocked planner call.

   type Line_Array is array (Positive range <>) of Virtual_String;

   type Mock_Planner is new Planner_Interface with null record;

   overriding
   function Get_Last_Position (This : Mock_Planner) return Position
   is ([others => 0.0 * mm]);
   overriding
   function Get_Last_Kinematic_Parameters (This : Mock_Planner) return Motion_Planner.Kinematic_Parameters
   is ((others => <>));
   overriding
   function Get_State_Anchor_Corner_ID (This : Mock_Planner) return Planner_Corner_ID
   is (0);
   overriding
   function Get_Last_Executed_Corner_ID (This : Mock_Planner) return Planner_Corner_ID
   is (0);
   overriding
   procedure Mark_Axis_Homed (This : Mock_Planner; Axis : Axis_Name);
   overriding
   procedure Mark_Axis_Unhomed (This : Mock_Planner; Axis : Axis_Name);
   overriding
   function Axis_Is_Homed (This : Mock_Planner; Axis : Axis_Name) return Boolean
   is (True);
   overriding
   function Cancellation_Is_Active (This : Mock_Planner) return Boolean
   is (False);
   overriding
   procedure Add_Corner
     (This          : Mock_Planner;
      Pos           : Position;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Require_Homed : Boolean := True;
      Ignore_Bounds : Boolean := False)
   is null;
   overriding
   procedure Add_Helix
     (This          : Mock_Planner;
      Center        : Position;
      Pos           : Position;
      Clockwise     : Boolean;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Require_Homed : Boolean := True)
   is null;
   overriding
   procedure Add_Corner_Data (This : Mock_Planner; Corner_Data : Extra_Corner_Data'Class) is null;
   overriding
   procedure Flush
     (This : Mock_Planner; Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record));
   overriding
   procedure Resolve_Homing_Move (This : Mock_Planner; Stopped_Position : Position) is null;
   overriding
   procedure Flush_And_Change_Kinematic_Parameters
     (This       : Mock_Planner;
      Params     : Motion_Planner.Kinematic_Parameters;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record))
   is null;
   overriding
   procedure Change_Move_Limits (This : Mock_Planner; Limits : Motion_Planner.Motion_Limits) is null;
   overriding
   function Get_Move_Limits (This : Mock_Planner) return Motion_Planner.Motion_Limits
   is (Motion_Planner.No_Motion_Limits);
   overriding
   procedure Flush_And_Reset_Position
     (This          : Mock_Planner;
      New_Position  : Position;
      Extra_Data    : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Ignore_Bounds : Boolean := False)
   is null;

   overriding
   procedure Mark_Axis_Homed (This : Mock_Planner; Axis : Axis_Name) is
      pragma Unreferenced (This);
   begin
      Trace.Append ("homed " & VSS.Strings.Conversions.To_Virtual_String (Axis'Image) & ";");
   end Mark_Axis_Homed;

   overriding
   procedure Mark_Axis_Unhomed (This : Mock_Planner; Axis : Axis_Name) is
      pragma Unreferenced (This);
   begin
      Trace.Append ("unhomed " & VSS.Strings.Conversions.To_Virtual_String (Axis'Image) & ";");
   end Mark_Axis_Unhomed;

   overriding
   procedure Flush
     (This : Mock_Planner; Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record))
   is
      pragma Unreferenced (This);
   begin
      if Fail_Flush then
         raise Program_Error with "Planner flush released.";
      end if;

      Trace.Append ("flush;");
      Pending.Append (Extra_Data);
   end Flush;

   type Mock_Context is new Block_End_Context with null record;

   overriding
   function Get_Last_Command_Index (This : Mock_Context) return Command_Index
   is (0);
   overriding
   procedure Wait_For_Idle (This : Mock_Context);
   overriding
   procedure Catch_Up_Planner_State (This : Mock_Context) is null;
   overriding
   procedure Prepare_Config_For_Save (This : Mock_Context) is null;
   overriding
   procedure Log (This : Mock_Context; Message : Virtual_String) is null;
   overriding
   procedure Log_If_Interactive (This : Mock_Context; Message : Virtual_String) is null;
   overriding
   function Cancellation_Is_Active (This : Mock_Context) return Boolean
   is (False);

   overriding
   procedure Wait_For_Idle (This : Mock_Context) is
      pragma Unreferenced (This);
   begin
      Trace.Append ("idle;");
   end Wait_For_Idle;

   type Mock_Motor_Handler is new Motor_Handler with null record;

   overriding
   procedure Enable_Motor (This : in out Mock_Motor_Handler);
   overriding
   procedure Disable_Motor (This : in out Mock_Motor_Handler);

   overriding
   procedure Enable_Motor (This : in out Mock_Motor_Handler) is
      pragma Unreferenced (This);
   begin
      Trace.Append ("enable;");
   end Enable_Motor;

   overriding
   procedure Disable_Motor (This : in out Mock_Motor_Handler) is
      pragma Unreferenced (This);
   begin
      Trace.Append ("disable;");
   end Disable_Motor;

   function Create_Instance (Motor_Axis : Axis_Name) return My_Modules.Module_Instance_Shared_Pointers.Ref;
   --  Create an instance where every motor is enabled and drives only Motor_Axis. Pending is shared and the test
   --  order varies, so this also discards any events an earlier test left queued.

   function Create_Instance (Motor_Axis : Axis_Name) return My_Modules.Module_Instance_Shared_Pointers.Ref is
      function Create return My_Modules.Module_Instance_Parent'Class;

      function Create return My_Modules.Module_Instance_Parent'Class is
         Map : Motor_Axis_Map := [others => [others => False]];
      begin
         for Motor in Motor_Name loop
            Map (Motor_Axis, Motor) := True;
         end loop;

         return Result : Module_Instance do
            Result.Initialize ((Motors => [others => (Enabled => True, others => <>)]));
            for Motor in Motor_Name loop
               Result.Provide_Motor_Configuration (Motor, (others => <>), Mock_Motor_Handler'(null record));
            end loop;
            Result.Set_Motor_Axis_Map (Map);
         end return;
      end Create;
   begin
      Pending.Clear;
      return Result : My_Modules.Module_Instance_Shared_Pointers.Ref do
         Result.Set (Create'Access);
      end return;
   end Create_Instance;

   function Dispatch_Trace (Instance : My_Modules.Module_Instance_Shared_Pointers.Ref; Line : Virtual_String)
   return Virtual_String;
   --  Dispatch an M17, M18 or M84 line and return what it traced before the executor reaches any queued event.

   function Dispatch_Trace (Instance : My_Modules.Module_Instance_Shared_Pointers.Ref; Line : Virtual_String)
   return Virtual_String
   is
      Planner : constant Mock_Planner := (null record);
      Args    : Gcode_Arguments.Arguments := Gcode_Arguments.Parse_Arguments (Line);
      Number  : constant Gcode_Arguments.Argument_Integer := Args.Consume_Integer ('M');
   begin
      Trace := "";
      Gcode_Dispatch (Module_Instance (Instance.Get.Element.all), Instance, Args, Planner, ('M', Number));
      Gcode_Arguments.Validate_All_Consumed (Args);
      return Trace;
   end Dispatch_Trace;

   function Process_Pending_Trace return Virtual_String;
   --  Process the queued block-end events in order, as the executor does, and return what they traced.

   function Process_Pending_Trace return Virtual_String is
      Context : constant Mock_Context := (null record);
   begin
      Trace := "";
      for Event of Pending loop
         Event.Process_After_Block (Context);
      end loop;
      Pending.Clear;
      return Trace;
   end Process_Pending_Trace;

   procedure Test_Disabling_Extruder_Keeps_E_Homed (T : in out Trendy_Test.Operation'Class);

   procedure Test_Disabling_Extruder_Keeps_E_Homed (T : in out Trendy_Test.Operation'Class) is
      Extruder : constant My_Modules.Module_Instance_Shared_Pointers.Ref := Create_Instance (E_Axis);
   begin
      T.Register (Parallelize => False);

      --  E has an arbitrary origin and is only marked homed when the homing module starts, so unhoming it here would
      --  block extrusion until the controller restarts. With no axis to unhome, the queued event is the only thing
      --  holding the release back until preceding motion has stopped.
      for Line of Line_Array'["M84", "M18", "M84 E", "M18 E"] loop
         declare
            Command : constant String := VSS.Strings.Conversions.To_UTF_8_String (Line);
         begin
            T.Assert
              (Dispatch_Trace (Extruder, Line) = "flush;",
               Command & " must not mark E unhomed or disable the extruder motor before preceding motion stops");
            T.Assert
              (Process_Pending_Trace = "idle;disable;",
               Command & " must disable the extruder motor once preceding motion has stopped");
         end;
      end loop;
   end Test_Disabling_Extruder_Keeps_E_Homed;

   procedure Test_Disabling_Positioning_Motor_Unhomes_Axis (T : in out Trendy_Test.Operation'Class);

   procedure Test_Disabling_Positioning_Motor_Unhomes_Axis (T : in out Trendy_Test.Operation'Class) is
      X_Motors : constant My_Modules.Module_Instance_Shared_Pointers.Ref := Create_Instance (X_Axis);
   begin
      T.Register (Parallelize => False);

      for Line of Line_Array'["M84", "M18", "M84 X", "M18 X"] loop
         declare
            Command : constant String := VSS.Strings.Conversions.To_UTF_8_String (Line);
         begin
            T.Assert
              (Dispatch_Trace (X_Motors, Line) = "unhomed X_AXIS;flush;",
               Command & " must mark X unhomed before queueing the release of its motor");
            T.Assert
              (Process_Pending_Trace = "idle;disable;",
               Command & " must disable the X motor once preceding motion has stopped");
         end;
      end loop;

      T.Assert (Dispatch_Trace (X_Motors, "M84 E") = "flush;", "M84 E must not mark X unhomed");
      T.Assert (Process_Pending_Trace = "idle;", "M84 E must leave motors that do not drive E alone");
   end Test_Disabling_Positioning_Motor_Unhomes_Axis;

   procedure Test_Enable_Follows_Queued_Disable (T : in out Trendy_Test.Operation'Class);

   procedure Test_Enable_Follows_Queued_Disable (T : in out Trendy_Test.Operation'Class) is
      Extruder : constant My_Modules.Module_Instance_Shared_Pointers.Ref := Create_Instance (E_Axis);
   begin
      T.Register (Parallelize => False);

      T.Assert (Dispatch_Trace (Extruder, "M84 E") = "flush;", "M84 E must queue the extruder release");
      T.Assert (Dispatch_Trace (Extruder, "M17 E") = "flush;", "M17 E must queue behind the pending M84 E");
      T.Assert
        (Process_Pending_Trace = "idle;disable;enable;",
         "M17 E after M84 E must leave the extruder motor enabled");
   end Test_Enable_Follows_Queued_Disable;

   function Prepare_Trace
     (Instance : My_Modules.Module_Instance_Shared_Pointers.Ref; Pause_Motion : Boolean := False) return Virtual_String;
   --  Prepare a move of every motor and return what it traced before the executor reaches any queued event.

   function Prepare_Trace
     (Instance : My_Modules.Module_Instance_Shared_Pointers.Ref; Pause_Motion : Boolean := False) return Virtual_String
   is
      Planner : constant Mock_Planner := (null record);
   begin
      Trace := "";
      Prepare_For_Motion
        (Module_Instance (Instance.Get.Element.all),
         Instance,
         Planner,
         Moving_Motors => [others => True],
         Pause_Motion  => Pause_Motion);
      return Trace;
   end Prepare_Trace;

   procedure Cancel (Instance : My_Modules.Module_Instance_Shared_Pointers.Ref);
   --  Discard the queued events and run the cancellation handler, as a cancellation barrier does.

   procedure Cancel (Instance : My_Modules.Module_Instance_Shared_Pointers.Ref) is
      Target : Module_Instance renames Module_Instance (Instance.Get.Element.all);
   begin
      Pending.Clear;
      Target.Handle_Cancel
        (Executed_Corner_ID => 0, Cancellation_Barrier_ID => 0, Current_Position => [others => 0.0 * mm]);
   end Cancel;

   procedure Test_Motion_Turns_Motors_On (T : in out Trendy_Test.Operation'Class);

   procedure Test_Motion_Turns_Motors_On (T : in out Trendy_Test.Operation'Class) is
      Extruder : constant My_Modules.Module_Instance_Shared_Pointers.Ref := Create_Instance (E_Axis);
   begin
      T.Register (Parallelize => False);

      --  The startup state is not known, so the first move turns the motors on.
      T.Assert (Prepare_Trace (Extruder) = "flush;", "the first move must queue its motors to be turned on");
      T.Assert (Prepare_Trace (Extruder) = "", "a move after a queued enable must not queue another one");
      T.Assert
        (Process_Pending_Trace = "enable;",
         "turning motors on must not wait for idle, as nothing queued can step a motor that is off");
      T.Assert (Prepare_Trace (Extruder) = "", "a move must not turn on motors that are already on");

      T.Assert (Dispatch_Trace (Extruder, "M84 E") = "flush;", "M84 E must queue the extruder release");
      T.Assert (Prepare_Trace (Extruder) = "flush;", "a move after a queued M84 E must queue its own enable");
      T.Assert
        (Process_Pending_Trace = "idle;disable;enable;",
         "the enable must run after the release queued in front of it");

      T.Assert (Dispatch_Trace (Extruder, "M17 E") = "flush;", "M17 E must queue an enable");
      T.Assert (Prepare_Trace (Extruder) = "", "a move after M17 E must rely on the enable it queued");
      T.Assert (Process_Pending_Trace = "enable;", "M17 E must turn the extruder on");
   end Test_Motion_Turns_Motors_On;

   procedure Test_Pause_Motion_Uses_Current_State (T : in out Trendy_Test.Operation'Class);

   procedure Test_Pause_Motion_Uses_Current_State (T : in out Trendy_Test.Operation'Class) is
      Extruder : constant My_Modules.Module_Instance_Shared_Pointers.Ref := Create_Instance (E_Axis);
   begin
      T.Register (Parallelize => False);

      T.Assert (Prepare_Trace (Extruder) = "flush;", "the first move must queue its motors to be turned on");
      T.Assert (Process_Pending_Trace = "enable;", "the queued enable must turn the extruder on");

      --  Pause motion runs before queued primary work, so a release that has not run yet still leaves the motor on.
      T.Assert (Dispatch_Trace (Extruder, "M84 E") = "flush;", "M84 E must queue the extruder release");
      T.Assert
        (Prepare_Trace (Extruder, Pause_Motion => True) = "",
         "pause motion must not turn on a motor whose queued release has not run yet");
      T.Assert (Process_Pending_Trace = "idle;disable;", "the queued release must turn the extruder off");

      T.Assert
        (Prepare_Trace (Extruder, Pause_Motion => True) = "flush;",
         "pause motion must turn on a motor that is currently off");
      T.Assert
        (Prepare_Trace (Extruder, Pause_Motion => True) = "",
         "pause motion must not queue a second enable while one is pending");
      T.Assert (Process_Pending_Trace = "enable;", "the pause enable must turn the extruder on");
      T.Assert
        (Prepare_Trace (Extruder, Pause_Motion => True) = "",
         "pause motion must not turn on a motor that is already on");
   end Test_Pause_Motion_Uses_Current_State;

   procedure Test_Cancel_Restores_Current_State (T : in out Trendy_Test.Operation'Class);

   procedure Test_Cancel_Restores_Current_State (T : in out Trendy_Test.Operation'Class) is
      Extruder : constant My_Modules.Module_Instance_Shared_Pointers.Ref := Create_Instance (E_Axis);
   begin
      T.Register (Parallelize => False);

      --  A cancelled enable never ran, so the motor is still off and the next move must turn it on.
      T.Assert (Dispatch_Trace (Extruder, "M17 E") = "flush;", "M17 E must queue an enable");
      Cancel (Extruder);
      T.Assert (Prepare_Trace (Extruder) = "flush;", "a cancelled M17 E must not count as turning the motor on");
      T.Assert (Process_Pending_Trace = "enable;", "the move's own enable must turn the extruder on");

      --  A cancelled release never ran either, so the motor is still on.
      T.Assert (Dispatch_Trace (Extruder, "M84 E") = "flush;", "M84 E must queue the extruder release");
      Cancel (Extruder);
      T.Assert (Prepare_Trace (Extruder) = "", "a cancelled M84 E must leave the motor counted as on");

      --  A pending pause enable is discarded with the rest of the queue.
      T.Assert (Dispatch_Trace (Extruder, "M84 E") = "flush;", "M84 E must queue the extruder release");
      T.Assert (Process_Pending_Trace = "idle;disable;", "the queued release must turn the extruder off");
      T.Assert (Prepare_Trace (Extruder, Pause_Motion => True) = "flush;", "pause motion must queue an enable");
      Cancel (Extruder);
      T.Assert
        (Prepare_Trace (Extruder, Pause_Motion => True) = "flush;",
         "a cancelled pause enable must not count as turning the motor on");
   end Test_Cancel_Restores_Current_State;

   procedure Test_Failed_Flush_Releases_Claim (T : in out Trendy_Test.Operation'Class);

   procedure Test_Failed_Flush_Releases_Claim (T : in out Trendy_Test.Operation'Class) is
      Extruder : constant My_Modules.Module_Instance_Shared_Pointers.Ref := Create_Instance (E_Axis);
   begin
      T.Register (Parallelize => False);

      for Pause_Motion in Boolean loop
         Fail_Flush := True;
         begin
            declare
               Unused : constant Virtual_String := Prepare_Trace (Extruder, Pause_Motion);
            begin
               T.Fail ("a failed flush must propagate");
            end;
         exception
            when Program_Error =>
               null;
         end;
         Fail_Flush := False;

         T.Assert
           (Prepare_Trace (Extruder, Pause_Motion) = "flush;",
            "an enable that could not be queued must not count as queued");
         Pending.Clear;
         Cancel (Extruder);
      end loop;
   end Test_Failed_Flush_Releases_Claim;

   function All_Tests return Test_Group is
   begin
      return
        [Test_Disabling_Extruder_Keeps_E_Homed'Unrestricted_Access,
         Test_Disabling_Positioning_Motor_Unhomes_Axis'Unrestricted_Access,
         Test_Enable_Follows_Queued_Disable'Unrestricted_Access,
         Test_Motion_Turns_Motors_On'Unrestricted_Access,
         Test_Pause_Motion_Uses_Current_State'Unrestricted_Access,
         Test_Cancel_Restores_Current_State'Unrestricted_Access,
         Test_Failed_Flush_Releases_Claim'Unrestricted_Access];
   end All_Tests;

end Prunt.Default_Modules.Motor_Drivers.Test;
