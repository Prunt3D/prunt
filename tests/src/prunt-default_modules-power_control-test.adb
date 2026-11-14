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
with Ada.Containers.Indefinite_Holders;
with Prunt.Motion_Planner;
with VSS.Strings.Conversions;

package body Prunt.Default_Modules.Power_Control.Test is

   package Events is new Ada.Containers.Indefinite_Holders (Extra_Block_Resetting_Data'Class);

   type Mock_Planner is new Planner_Interface with record
      Pending : access Events.Holder;
   end record;

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
   begin
      Trace.Append ("flush;");
      This.Pending.Replace_Element (Extra_Data);
   end Flush;

   type Mock_Context is new Block_End_Context with null record;

   overriding
   function Get_Last_Command_Index (This : Mock_Context) return Command_Index
   is (42);
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

   procedure Test_Power_Off_Unhomes_Axes (T : in out Trendy_Test.Operation'Class);

   procedure Test_Power_Off_Unhomes_Axes (T : in out Trendy_Test.Operation'Class) is
      Instance : Module_Instance;
      Pending  : aliased Events.Holder;
      Planner  : constant Mock_Planner := (Pending => Pending'Unchecked_Access);
      Context  : Mock_Context;
      Ref      : My_Modules.Module_Instance_Shared_Pointers.Ref;

      procedure Dispatch (Line : Virtual_String);

      procedure Dispatch (Line : Virtual_String) is
         Args   : Gcode_Arguments.Arguments := Gcode_Arguments.Parse_Arguments (Line);
         Number : constant Gcode_Arguments.Argument_Integer := Args.Consume_Integer ('M');
      begin
         Instance.Gcode_Dispatch (Ref, Args, Planner, ('M', Number));
         Gcode_Arguments.Validate_All_Consumed (Args);
      end Dispatch;
   begin
      T.Register (Parallelize => False);

      Trace := "";
      Dispatch ("M81");
      T.Assert
        (Trace = "unhomed X_AXIS;unhomed Y_AXIS;unhomed Z_AXIS;flush;",
         "M81 must mark every physically homed axis unhomed before queueing the power-off block");
      Trace := "";
      Pending.Element.Process_After_Block (Context);
      T.Assert (Trace = "idle;off;", "power must be removed only after physical idle");

      Trace := "";
      Dispatch ("M80");
      T.Assert (Trace = "flush;", "M80 must not change the homed state");
      Trace := "";
      Pending.Element.Process_After_Block (Context);
      T.Assert (Trace = "idle;on;", "power must be restored only after physical idle");
   end Test_Power_Off_Unhomes_Axes;

   function All_Tests return Test_Group is
   begin
      return [Test_Power_Off_Unhomes_Axes'Unrestricted_Access];
   end All_Tests;

end Prunt.Default_Modules.Power_Control.Test;
