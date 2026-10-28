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

package body Prunt.Default_Modules.Smart_Plugs.Test is

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
   procedure Mark_Axis_Homed (This : Mock_Planner; Axis : Axis_Name) is null;
   overriding
   procedure Mark_Axis_Unhomed (This : Mock_Planner; Axis : Axis_Name) is null;
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
      Require_Homed : Boolean := True)
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
     (This         : Mock_Planner;
      New_Position : Position;
      Extra_Data   : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record))
   is null;

   overriding
   procedure Flush
     (This : Mock_Planner; Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record))
   is
   begin
      This.Pending.Replace_Element (Extra_Data);
   end Flush;

   type Mock_Context is new Block_End_Context with record
      Fail_Idle : Boolean := False;
   end record;

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
   procedure Log (This : Mock_Context; Message : Virtual_String);
   overriding
   procedure Log_If_Interactive (This : Mock_Context; Message : Virtual_String) is null;

   overriding
   procedure Wait_For_Idle (This : Mock_Context) is
   begin
      Trace.Append ("idle;");
      if This.Fail_Idle then
         raise Hardware_Communication_Error;
      end if;
   end Wait_For_Idle;

   overriding
   procedure Log (This : Mock_Context; Message : Virtual_String) is
      pragma Unreferenced (This);
   begin
      Trace.Append (Message);
   end Log;

   type Command_Lines is array (Positive range <>) of Virtual_String;

   procedure Test_Smart_Plug_Shutdown (T : in out Trendy_Test.Operation'Class);

   procedure Test_Smart_Plug_Shutdown (T : in out Trendy_Test.Operation'Class) is
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
      Dispatch ("M81 P");
      T.Assert (Trace.Is_Empty, "parsing must not shut down or switch power ahead of queued motion");
      T.Assert (not Pending.Is_Empty, "M81 P must enqueue a block boundary");
      begin
         Pending.Element.Process_After_Block (Context);
         T.Fail ("shutdown must leave the current step-generator block");
      exception
         when Controller_Shutdown_Requested =>
            null;
      end;
      T.Assert (Trace = "idle;shutdown;", "shutdown must follow confirmed physical idle");

      Trace := "";
      Context.Fail_Idle := True;
      begin
         Pending.Element.Process_After_Block (Context);
         T.Fail ("a failed idle wait must propagate");
      exception
         when Hardware_Communication_Error =>
            null;
      end;
      T.Assert (Trace = "idle;", "failed idle must propagate to the controller's emergency shutdown path");
      Context.Fail_Idle := False;

      for Line of Command_Lines'["M81 P1", "M81 P0", "M81 P S", "M81", "M80", "M80 S"] loop
         Pending.Clear;
         begin
            Dispatch (Line);
            T.Fail ("only bare P may select smart plug shutdown");
         exception
            when Gcode_Bad_Inputs_Error =>
               null;
         end;
         T.Assert (Pending.Is_Empty, "invalid variants must not enqueue work");
      end loop;
   end Test_Smart_Plug_Shutdown;

   procedure Test_Shutdown_Without_Plug (T : in out Operation'Class);

   procedure Test_Shutdown_Without_Plug (T : in out Operation'Class) is
      procedure Report_Config_Error (Path : Config.Config_Path; Message : Virtual_String) is
         pragma Unreferenced (Path);
      begin
         T.Fail ("disabled plug configuration must be valid: " & VSS.Strings.Conversions.To_UTF_8_String (Message));
      end Report_Config_Error;

      function Get_Other_Instance (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref is
         pragma Unreferenced (Tag);
      begin
         raise Program_Error with "Smart plug shutdown must not depend on another module.";
         return My_Modules.Module_Instance_Shared_Pointers.Null_Ref;
      end Get_Other_Instance;
   begin
      T.Register (Parallelize => False);
      declare
         Plug_Module : Module;
         File        : constant Config.Config_File :=
           Config.Create ("smart_plug_disabled_shutdown.json", ["Smart Plug" => Plug_Module.Config_Schema]);
         Status      : Status_Manager.Status_Emitter;
         Instance    : My_Modules.Module_Instance'Class :=
           Plug_Module.Initialize
             (File.Get_Data ("Smart Plug"), Report_Config_Error'Access, Status, Get_Other_Instance'Access);
         Pending     : aliased Events.Holder;
         Planner     : constant Mock_Planner := (Pending => Pending'Unchecked_Access);
         Context     : Mock_Context;
         Ref         : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Args        : Gcode_Arguments.Arguments := Gcode_Arguments.Parse_Arguments ("M81 P");
         Number      : constant Gcode_Arguments.Argument_Integer := Args.Consume_Integer ('M');
      begin
         T.Assert (not Read_Configuration (File.Get_Data ("Smart Plug")).Enabled, "plug must be disabled");
         Trace := "";
         Instance.Gcode_Dispatch (Ref, Args, Planner, ('M', Number));
         Args.Validate_All_Consumed;
         T.Assert (not Pending.Is_Empty, "a disabled plug must still allow board shutdown");
         T.Assert (Trace.Is_Empty, "shutdown must wait for queued motion even without a plug");
         begin
            Pending.Element.Process_After_Block (Context);
            T.Fail ("shutdown must leave the current step-generator block even without a plug");
         exception
            when Controller_Shutdown_Requested =>
               null;
         end;
         T.Assert (Trace = "idle;shutdown;", "a disabled plug must still shut down after physical idle");
      end;
   end Test_Shutdown_Without_Plug;

   procedure Test_Saved_Changes_Wait_For_Restart (T : in out Operation'Class);

   function All_Tests return Test_Group is
   begin
      return
        [Test_Saved_Changes_Wait_For_Restart'Unrestricted_Access,
         Test_Smart_Plug_Shutdown'Unrestricted_Access,
         Test_Shutdown_Without_Plug'Unrestricted_Access];
   end All_Tests;

   procedure Test_Saved_Changes_Wait_For_Restart (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         Plug_Module : Module;
         File        : constant Config.Config_File :=
           Config.Create ("smart_plug_saved_changes.json", ["Smart Plug" => Plug_Module.Config_Schema]);
         Result      : Virtual_String;
         Errors      : Config.Config_Error_Vectors.Vector;
         pragma Unreferenced (Result);
      begin
         T.Assert
           (not Read_Configuration (File.Get_Data ("Smart Plug")).Enabled, "startup loads the disabled default");

         File.Apply_Untrusted_Patch
           ("{""Prunt config version"":1,""Config"":{""Smart Plug"":{""Version"":1,""Config"":{""Smart_Plug"":{"
            & """Kind"":{""Selected"":""Enabled"",""Children"":{""Enabled"":{""Host"":""plug.local""}}}}}}}}",
            Result,
            Errors);
         T.Assert (Errors.Is_Empty, "saved enable patch is valid");
         T.Assert
           (not Read_Configuration (File.Get_Data ("Smart Plug")).Enabled,
            "saving Enabled does not activate the plug before restart");

         File.Reset_Live_To_Stored;
         T.Assert
           (Read_Configuration (File.Get_Data ("Smart Plug")).Enabled
            and then Read_Configuration (File.Get_Data ("Smart Plug")).Host = "plug.local",
            "restart applies saved settings even before the board connects");

         File.Apply_Untrusted_Patch
           ("{""Prunt config version"":1,""Config"":{""Smart Plug"":{""Version"":1,""Config"":{""Smart_Plug"":{"
            & """Kind"":{""Children"":{""Enabled"":{""Host"":""replacement.local"",""Watchdog_Seconds"":120}}}}}}}}",
            Result,
            Errors);
         T.Assert (Errors.Is_Empty, "saved host and timeout patch is valid");
         T.Assert
           (Read_Configuration (File.Get_Data ("Smart Plug")).Host = "plug.local"
            and then Read_Configuration (File.Get_Data ("Smart Plug")).Watchdog_Seconds = 60,
            "saving host and watchdog changes preserves the active settings until restart");

         File.Reset_Live_To_Stored;
         T.Assert
           (Read_Configuration (File.Get_Data ("Smart Plug")).Host = "replacement.local"
            and then Read_Configuration (File.Get_Data ("Smart Plug")).Watchdog_Seconds = 120,
            "restart applies host and watchdog changes together");

         File.Apply_Untrusted_Patch
           ("{""Prunt config version"":1,""Config"":{""Smart Plug"":{""Version"":1,""Config"":{""Smart_Plug"":{"
            & """Kind"":{""Selected"":""Disabled""}}}}}}",
            Result,
            Errors);
         T.Assert (Errors.Is_Empty, "saved disable patch is valid");
         T.Assert
           (Read_Configuration (File.Get_Data ("Smart Plug")).Enabled,
            "saving Disabled leaves the banner and worker enabled until restart");

         File.Reset_Live_To_Stored;
         T.Assert
           (not Read_Configuration (File.Get_Data ("Smart Plug")).Enabled,
            "restart applies saved disabling through the normal live configuration reset");
      end;
   end Test_Saved_Changes_Wait_For_Restart;

end Prunt.Default_Modules.Smart_Plugs.Test;
