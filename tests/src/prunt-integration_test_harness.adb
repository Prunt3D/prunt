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

pragma Extensions_Allowed (On);
pragma Style_Checks (Off);

with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Prunt.Config;
with Prunt.Controller;
with Prunt.Controller_Generic_Types;
with Prunt.Integration_Config_Overlays;
with Prunt.Integration_Test_Catalog;
with Prunt.JSON; use Prunt.JSON;
with Prunt.Mockable.Text_IO;
with Prunt.Thermistors;
with VSS.Strings.Conversions;

package body Prunt.Integration_Test_Harness is

   use type Ada.Containers.Count_Type;
   use type Ada.Calendar.Time;

   type Motor_Name is (X_Motor, Y_Motor, Z_Motor, E_Motor);
   type Heater_Name is (Dummy_Heater);
   type Thermistor_Name is (Dummy_Thermistor);
   type Board_Temperature_Probe_Name is (Dummy_Board_Temperature_Probe);
   type Fan_Name is (Dummy_Fan, Low_Side_Dummy_Fan, High_Side_Dummy_Fan);
   type Tachometer_Name is (Dummy_Tachometer);
   type Input_Switch_Name is (Dummy_Input_Switch);

   package Generic_Types is new
     Prunt.Controller_Generic_Types
       (Motor_Name                   => Motor_Name,
        Heater_Name                  => Heater_Name,
        Thermistor_Name              => Thermistor_Name,
        Board_Temperature_Probe_Name => Board_Temperature_Probe_Name,
        Fan_Name                     => Fan_Name,
        Tachometer_Name              => Tachometer_Name,
        Input_Switch_Name            => Input_Switch_Name);

   subtype Queued_Command is Generic_Types.Queued_Command;
   subtype Motor_Position is Generic_Types.Motor_Position;

   type Axis_Name is (X_Axis, Y_Axis, Z_Axis, E_Axis);
   type Axis_Position is array (Axis_Name) of Long_Float;

   Base_Sample_Rate_Hz       : constant Positive := 2_000;
   Sample_Rate_Hz            : constant Positive := 10_000;
   Sample_Rate_Scale         : constant Positive := Sample_Rate_Hz / Base_Sample_Rate_Hz;
   Sample_Period_S           : constant Long_Float := 1.0 / Long_Float (Sample_Rate_Hz);
   Command_Queue_Capacity    : constant Positive := 128;
   Short_Empty_Wait_Limit    : constant Natural := 50;
   Submit_Empty_Wait_Limit   : constant Natural := 200 * Sample_Rate_Scale;
   Long_Empty_Wait_Limit     : constant Natural := 2_000 * Sample_Rate_Scale;
   Cancel_Empty_Wait_Limit   : constant Natural := 30_000 * Sample_Rate_Scale;
   Idle_Stable_Position_Count : constant Natural := 20;
   Submit_Timeout_S          : constant Duration := 300.0;

   type Sample is record
      T            : Long_Float := 0.0;
      Position     : Axis_Position := [others => 0.0];
      Velocity     : Axis_Position := [others => 0.0];
      Acceleration : Axis_Position := [others => 0.0];
      Jerk         : Axis_Position := [others => 0.0];
      Snap         : Axis_Position := [others => 0.0];
      Crackle      : Axis_Position := [others => 0.0];
      Command      : Command_Index := 0;
   end record;

   package Sample_Vectors is new Ada.Containers.Vectors (Positive, Sample);

   type Event is record
      T       : Long_Float := 0.0;
      Kind    : Unbounded_String;
      Label   : Unbounded_String;
      Target  : Unbounded_String;
      Value   : Unbounded_String;
      Command : Command_Index := 0;
   end record;

   package Event_Vectors is new Ada.Containers.Vectors (Positive, Event);

   type Machine_Queue_Item_Kind is (Command_Item, Reset_Position_Item);

   type Machine_Queue_Item (Kind : Machine_Queue_Item_Kind := Command_Item) is record
      case Kind is
         when Command_Item         =>
            Command : Queued_Command :=
              (Index => 0, Pos => [others => 0.0], Safe_Stop_After => True, Loop_Until_Hit => False);
         when Reset_Position_Item  =>
            Reset_Pos : Motor_Position := [others => 0.0];
      end case;
   end record;

   type Machine_Queue_Array is array (Positive range <>) of Machine_Queue_Item;

   protected Machine is
      entry Enqueue (Command : Queued_Command);
      entry Record_Reset_Position (Pos : Motor_Position);
      procedure Clear;
      procedure Clear_Trace;
      procedure Dequeue (Item : out Machine_Queue_Item; Found : out Boolean);
      entry Wait_Until_Started;
      procedure Record_Event
        (Kind : String; Label : String; Target : String := ""; Value : String := ""; Command : Command_Index := 0);
      procedure Record_Executed_Command (Command : Queued_Command);
      procedure Record_Executed_Reset_Position (Pos : Motor_Position);
      function Current_Time return Long_Float;
      function Current_Position return Axis_Position;
      function Last_Executed return Command_Index;
      function Sample_Count return Natural;
      function Event_Count return Natural;
      procedure Snapshot (Samples_Out : out Sample_Vectors.Vector; Events_Out : out Event_Vectors.Vector);
   private
      Queue         : Machine_Queue_Array (1 .. Command_Queue_Capacity);
      Count         : Natural := 0;
      Head          : Positive := 1;
      Tail          : Positive := 1;
      Started       : Boolean := False;
      Now_S         : Long_Float := 0.0;
      Last_Pos      : Axis_Position := [others => 0.0];
      Last_Vel      : Axis_Position := [others => 0.0];
      Last_Accel    : Axis_Position := [others => 0.0];
      Last_Jerk     : Axis_Position := [others => 0.0];
      Last_Snap     : Axis_Position := [others => 0.0];
      Last_Command  : Command_Index := 0;
      Samples       : Sample_Vectors.Vector;
      Events        : Event_Vectors.Vector;
   end Machine;

   protected Controller_Task_State is
      procedure Clear;
      function Message return String;
      procedure Mark_Stopped (Why : String);
      function Stopped return Boolean;
   private
      Has_Stopped : Boolean := False;
      Stop_Message : Unbounded_String;
   end Controller_Task_State;

   procedure Advance_By (Delta_S : Long_Float; Empty_Wait_Limit : Natural := Short_Empty_Wait_Limit);
   procedure Advance_To (Target_S : Long_Float; Empty_Wait_Limit : Natural := Short_Empty_Wait_Limit);
   procedure Advance_Until_Idle (Timeout_S : Long_Float);
   procedure Assert (Condition : Boolean; Message : String);
   function Axis_Label (Axis : Axis_Name) return String;
   function Axis_Value (Position : Axis_Position; Name : String) return Long_Float;
   function Build_Config_Overrides return Config.Config_Override_Vectors.Vector;
   procedure Enqueue_Command (Command : Queued_Command);
   function Event_Kind_Count (Kind : String) return Natural;
   function Expected_Controller_Failure_Contains (Scenario : JSON_Value) return String;
   function Find_Scenario_Path (Name : String) return String;
   function Get_Board_Specific_Documentation (Key : Virtual_String) return Virtual_String;
   function Get_Extra_HTTP_Content (Name : Virtual_String) return access constant Ada.Streams.Stream_Element_Array;
   function Image (Value : Long_Float) return String;
   function Reset_Position_Value (Pos : Motor_Position) return String;
   procedure Log_Event
     (Kind : String; Label : String; Target : String := ""; Value : String := ""; Command : Command_Index := 0);
   procedure Record_Final_Trace (Scenario : JSON_Value; Name : String);
   procedure Report_Command_Executed (Index : Command_Index);
   procedure Reset_Position (Pos : Motor_Position);
   procedure Run_Action
     (Action       : JSON_Value;
      Submit       : access procedure
        (Command : Virtual_String; Succeeded : out Boolean; Command_ID : out Gcode_Command_ID);
      Submit_File  : access procedure (Path : Virtual_String; Succeeded : out Boolean);
      Cancel       : access procedure (Succeeded : out Boolean));
   procedure Run_Cancel_At
     (Time_S : Long_Float; Cancel : access procedure (Succeeded : out Boolean));
   procedure Run_Expectations (Expect : JSON_Value);
   function To_Axis_Position (Pos : Motor_Position) return Axis_Position;
   procedure Wait_For_Gcode_Ready;
   procedure Wait_For_Motion (Minimum_New_Samples : Natural; Timeout_S : Long_Float);
   procedure Wait_Until_Idle (Last_Command_Index : Command_Index);

   procedure Enable_Motor (Motor : Motor_Name);
   procedure Disable_Motor (Motor : Motor_Name);
   procedure Set_Fan_Duty_Cycle (Fan : Fan_Name; Duty_Cycle : PWM_Scale);
   procedure Reconfigure_Fan (Fan : Fan_Name; PWM_Frequency : Frequency);
   procedure Reconfigure_Low_Or_High_Side_Fan
     (Fan : Fan_Name; PWM_Frequency : Frequency; Use_High_Side_Switching : Boolean);
   function Get_Tachometer_Frequency (Tachometer : Tachometer_Name; Requires_Fresh : Boolean) return Frequency;
   function Get_Input_Switch_State (Switch : Input_Switch_Name) return Boolean;
   procedure Reconfigure_Heater
     (Heater : Heater_Name; Params : Heater_Parameters; Assigned_Thermistor : Thermistor_Name);
   procedure Set_Heater_Temperature (Heater : Heater_Name; Target : Temperature);
   procedure Autotune_Heater
     (Heater : Heater_Name; Params : Heater_Parameters; Assigned_Thermistor : Thermistor_Name);
   procedure Reconfigure_Thermistor
     (Thermistor : Thermistor_Name; Params : Thermistors.Thermistor_Parameters);
   function Get_Thermistor_Temperature (Thermistor : Thermistor_Name; Requires_Fresh : Boolean) return Temperature;
   function Get_Board_Temperature
     (Probe : Board_Temperature_Probe_Name; Requires_Fresh : Boolean) return Temperature;

   Hardware : constant Generic_Types.Hardware_Parameters :=
     (Motor_Hardware                   =>
        [others =>
           (Kind                      => Basic_Motor_Kind,
            Maximum_Delta_Per_Command => 1.0,
            Enable                    => Enable_Motor'Access,
            Disable                   => Disable_Motor'Access)],
      Fan_Hardware                     =>
        [Dummy_Fan =>
           (Kind                            => Fixed_Switching_Kind,
            Set_Duty_Cycle                  => Set_Fan_Duty_Cycle'Access,
            Gcode_Index                     => 0,
            Reconfigure_Fixed_Switching_Fan => Reconfigure_Fan'Access,
            Maximum_PWM_Frequency           => 25_000.0 * hertz),
         Low_Side_Dummy_Fan =>
           (Kind                                     => Low_Or_High_Side_Switching_Kind,
            Set_Duty_Cycle                           => Set_Fan_Duty_Cycle'Access,
            Gcode_Index                              => 1,
            Reconfigure_Low_Or_High_Side_Switching_Fan => Reconfigure_Low_Or_High_Side_Fan'Access,
            Maximum_Low_Side_PWM_Frequency           => 20_000.0 * hertz,
            Maximum_High_Side_PWM_Frequency          => 25_000.0 * hertz),
         High_Side_Dummy_Fan =>
           (Kind                                     => Low_Or_High_Side_Switching_Kind,
            Set_Duty_Cycle                           => Set_Fan_Duty_Cycle'Access,
            Gcode_Index                              => 2,
            Reconfigure_Low_Or_High_Side_Switching_Fan => Reconfigure_Low_Or_High_Side_Fan'Access,
            Maximum_Low_Side_PWM_Frequency           => 20_000.0 * hertz,
            Maximum_High_Side_PWM_Frequency          => 25_000.0 * hertz)],
      Tachometer_Hardware              =>
        [others => (Get_Pulse_Frequency => Get_Tachometer_Frequency'Access)],
      Input_Switch_Hardware            =>
        [others => (Visible_To_User => False, Get_State => Get_Input_Switch_State'Access)],
      Heater_Hardware                  =>
        [others =>
           (Reconfigure     => Reconfigure_Heater'Access,
            Set_Temperature => Set_Heater_Temperature'Access,
            Autotune        => Autotune_Heater'Access)],
      Thermistor_Hardware              =>
        [others =>
           (Reconfigure     => Reconfigure_Thermistor'Access,
            Get_Temperature => Get_Thermistor_Temperature'Access)],
      Board_Temperature_Probe_Hardware =>
        [others => (Get_Temperature => Get_Board_Temperature'Access)]);

   protected body Machine is
      entry Enqueue (Command : Queued_Command) when Count < Command_Queue_Capacity is
      begin
         Queue (Tail) := (Kind => Command_Item, Command => Command);
         Tail := (if Tail = Command_Queue_Capacity then 1 else Tail + 1);
         Count := Count + 1;
      end Enqueue;

      entry Record_Reset_Position (Pos : Motor_Position) when Count < Command_Queue_Capacity or else not Started is
      begin
         if not Started and then Count = 0 then
            Last_Pos := To_Axis_Position (Pos);
            Started := True;
            Record_Event ("machine", "reset_position", Value => Reset_Position_Value (Pos), Command => Last_Command);
         else
            Queue (Tail) := (Kind => Reset_Position_Item, Reset_Pos => Pos);
            Tail := (if Tail = Command_Queue_Capacity then 1 else Tail + 1);
            Count := Count + 1;
         end if;
      end Record_Reset_Position;

      procedure Clear is
      begin
         Count := 0;
         Head := 1;
         Tail := 1;
         Started := False;
         Now_S := 0.0;
         Last_Pos := [others => 0.0];
         Last_Vel := [others => 0.0];
         Last_Accel := [others => 0.0];
         Last_Jerk := [others => 0.0];
         Last_Snap := [others => 0.0];
         Last_Command := 0;
         Samples.Clear;
         Events.Clear;
      end Clear;

      procedure Clear_Trace is
      begin
         Now_S := 0.0;
         Last_Vel := [others => 0.0];
         Last_Accel := [others => 0.0];
         Last_Jerk := [others => 0.0];
         Last_Snap := [others => 0.0];
         Last_Command := 0;
         Samples.Clear;
         Events.Clear;
      end Clear_Trace;

      procedure Dequeue (Item : out Machine_Queue_Item; Found : out Boolean) is
      begin
         if Count = 0 then
            Item :=
              (Kind    => Command_Item,
               Command => (Index => 0, Pos => [others => 0.0], Safe_Stop_After => True, Loop_Until_Hit => False));
            Found := False;
         else
            Item := Queue (Head);
            Head := (if Head = Command_Queue_Capacity then 1 else Head + 1);
            Count := Count - 1;
            Found := True;
         end if;
      end Dequeue;

      entry Wait_Until_Started when Started is
      begin
         null;
      end Wait_Until_Started;

      procedure Record_Event
        (Kind : String; Label : String; Target : String := ""; Value : String := ""; Command : Command_Index := 0) is
      begin
         Events.Append
           (Event'
              (T       => Now_S,
               Kind    => To_Unbounded_String (Kind),
               Label   => To_Unbounded_String (Label),
               Target  => To_Unbounded_String (Target),
               Value   => To_Unbounded_String (Value),
               Command => Command));
      end Record_Event;

      procedure Record_Executed_Command (Command : Queued_Command) is
         New_Pos : constant Axis_Position := To_Axis_Position (Command.Pos);
         New_Vel : Axis_Position;
         New_Accel : Axis_Position;
         New_Jerk : Axis_Position;
         New_Snap : Axis_Position;
         New_Crackle : Axis_Position;
      begin
         Now_S := Now_S + Sample_Period_S;

         for Axis in Axis_Name loop
            New_Vel (Axis) := (New_Pos (Axis) - Last_Pos (Axis)) / Sample_Period_S;
            New_Accel (Axis) := (New_Vel (Axis) - Last_Vel (Axis)) / Sample_Period_S;
            New_Jerk (Axis) := (New_Accel (Axis) - Last_Accel (Axis)) / Sample_Period_S;
            New_Snap (Axis) := (New_Jerk (Axis) - Last_Jerk (Axis)) / Sample_Period_S;
            New_Crackle (Axis) := (New_Snap (Axis) - Last_Snap (Axis)) / Sample_Period_S;
         end loop;

         Last_Pos := New_Pos;
         Last_Vel := New_Vel;
         Last_Accel := New_Accel;
         Last_Jerk := New_Jerk;
         Last_Snap := New_Snap;
         Last_Command := Command.Index;

         Samples.Append
           (Sample'
              (T            => Now_S,
               Position     => New_Pos,
               Velocity     => New_Vel,
               Acceleration => New_Accel,
               Jerk         => New_Jerk,
               Snap         => New_Snap,
               Crackle      => New_Crackle,
               Command      => Command.Index));
      end Record_Executed_Command;

      procedure Record_Executed_Reset_Position (Pos : Motor_Position) is
      begin
         Last_Pos := To_Axis_Position (Pos);
         Last_Vel := [others => 0.0];
         Last_Accel := [others => 0.0];
         Last_Jerk := [others => 0.0];
         Last_Snap := [others => 0.0];
         Record_Event ("machine", "reset_position", Value => Reset_Position_Value (Pos), Command => Last_Command);
      end Record_Executed_Reset_Position;

      function Current_Time return Long_Float is
      begin
         return Now_S;
      end Current_Time;

      function Current_Position return Axis_Position is
      begin
         return Last_Pos;
      end Current_Position;

      function Last_Executed return Command_Index is
      begin
         return Last_Command;
      end Last_Executed;

      function Sample_Count return Natural is
      begin
         return Natural (Samples.Length);
      end Sample_Count;

      function Event_Count return Natural is
      begin
         return Natural (Events.Length);
      end Event_Count;

      procedure Snapshot (Samples_Out : out Sample_Vectors.Vector; Events_Out : out Event_Vectors.Vector) is
      begin
         Samples_Out := Samples;
         Events_Out := Events;
      end Snapshot;
   end Machine;

   protected body Controller_Task_State is
      procedure Clear is
      begin
         Has_Stopped := False;
         Stop_Message := Null_Unbounded_String;
      end Clear;

      function Message return String is
      begin
         return To_String (Stop_Message);
      end Message;

      procedure Mark_Stopped (Why : String) is
      begin
         if not Has_Stopped then
            Has_Stopped := True;
            Stop_Message := To_Unbounded_String (Why);
         end if;
      end Mark_Stopped;

      function Stopped return Boolean is
      begin
         return Has_Stopped;
      end Stopped;
   end Controller_Task_State;

   function Scenario_Count return Natural is
   begin
      return Integration_Test_Catalog.Scenario_Count;
   end Scenario_Count;

   function Scenario_Name (Index : Positive) return String is
   begin
      return Integration_Test_Catalog.Scenario_Name (Index);
   end Scenario_Name;

   function Matches_Filter (Name, Filter : String) return Boolean is
   begin
      return Integration_Test_Catalog.Matches_Filter (Name, Filter);
   end Matches_Filter;

   function Axis_Label (Axis : Axis_Name) return String is
   begin
      case Axis is
         when X_Axis =>
            return "X";
         when Y_Axis =>
            return "Y";
         when Z_Axis =>
            return "Z";
         when E_Axis =>
            return "E";
      end case;
   end Axis_Label;

   function Axis_Value (Position : Axis_Position; Name : String) return Long_Float is
   begin
      if Name = "X" then
         return Position (X_Axis);
      elsif Name = "Y" then
         return Position (Y_Axis);
      elsif Name = "Z" then
         return Position (Z_Axis);
      elsif Name = "E" then
         return Position (E_Axis);
      else
         raise Constraint_Error with "Unknown axis name: " & Name;
      end if;
   end Axis_Value;

   function Image (Value : Long_Float) return String is
   begin
      return Ada.Strings.Fixed.Trim (Long_Float'Image (Value), Ada.Strings.Both);
   end Image;

   function Reset_Position_Value (Pos : Motor_Position) return String is
   begin
      return
        "X="
        & Image (Long_Float (Pos (X_Motor)))
        & " Y="
        & Image (Long_Float (Pos (Y_Motor)))
        & " Z="
        & Image (Long_Float (Pos (Z_Motor)))
        & " E="
        & Image (Long_Float (Pos (E_Motor)));
   end Reset_Position_Value;

   function To_Axis_Position (Pos : Motor_Position) return Axis_Position is
   begin
      return
        [X_Axis => Long_Float (Pos (X_Motor)),
         Y_Axis => Long_Float (Pos (Y_Motor)),
         Z_Axis => Long_Float (Pos (Z_Motor)),
         E_Axis => Long_Float (Pos (E_Motor))];
   end To_Axis_Position;

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      if Controller_Task_State.Stopped then
         raise Program_Error with Controller_Task_State.Message;
      end if;

      if not Condition then
         raise Program_Error with Message;
      end if;
   end Assert;

   procedure Log_Event
     (Kind : String; Label : String; Target : String := ""; Value : String := ""; Command : Command_Index := 0) is
   begin
      Machine.Record_Event (Kind, Label, Target, Value, Command);
   end Log_Event;

   procedure Advance_By (Delta_S : Long_Float; Empty_Wait_Limit : Natural := Short_Empty_Wait_Limit) is
   begin
      Advance_To (Machine.Current_Time + Delta_S, Empty_Wait_Limit);
   end Advance_By;

   procedure Advance_To (Target_S : Long_Float; Empty_Wait_Limit : Natural := Short_Empty_Wait_Limit) is
      Item        : Machine_Queue_Item;
      Found       : Boolean;
      Empty_Waits : Natural := 0;
   begin
      while Machine.Current_Time < Target_S loop
         Assert (not Controller_Task_State.Stopped, Controller_Task_State.Message);
         Machine.Dequeue (Item, Found);

         if Found then
            case Item.Kind is
               when Command_Item         =>
                  Machine.Record_Executed_Command (Item.Command);
                  Report_Command_Executed (Item.Command.Index);

               when Reset_Position_Item  =>
                  Machine.Record_Executed_Reset_Position (Item.Reset_Pos);
            end case;
            Empty_Waits := 0;
         else
            Empty_Waits := Empty_Waits + 1;
            exit when Empty_Waits > Empty_Wait_Limit;
            delay 0.001;
         end if;
      end loop;
   end Advance_To;

   procedure Advance_Until_Idle (Timeout_S : Long_Float) is
      Sim_Deadline : constant Long_Float := Machine.Current_Time + Timeout_S;
      Wall_Deadline : constant Ada.Calendar.Time := Ada.Calendar."+" (Ada.Calendar.Clock, Duration (120.0));
      Last_Pos : Axis_Position := Machine.Current_Position;
      Start_Event_Count : constant Natural := Machine.Event_Count;
      Start_Sample_Count : constant Natural := Machine.Sample_Count;
      Stable_Position_Count : Natural := 0;

      function Same_Position (Left, Right : Axis_Position) return Boolean;

      function Same_Position (Left, Right : Axis_Position) return Boolean is
      begin
         for Axis in Axis_Name loop
            if abs (Left (Axis) - Right (Axis)) > 1.0E-5 then
               return False;
            end if;
         end loop;

         return True;
      end Same_Position;
   begin
      loop
         Advance_By (0.01);

         declare
            Current_Event_Count : constant Natural := Machine.Event_Count;
            Had_Activity : constant Boolean :=
              Machine.Sample_Count > Start_Sample_Count or else Current_Event_Count > Start_Event_Count;
         begin
            if
              Had_Activity
              and then Same_Position (Machine.Current_Position, Last_Pos)
            then
               Stable_Position_Count := Stable_Position_Count + 1;
            else
               Stable_Position_Count := 0;
            end if;
         end;

         Last_Pos := Machine.Current_Position;

         exit when Stable_Position_Count >= Idle_Stable_Position_Count;
         Assert (Machine.Current_Time <= Sim_Deadline, "Timed out waiting for deterministic machine to become idle.");
         Assert (Ada.Calendar.Clock < Wall_Deadline, "Wall-clock guard expired waiting for deterministic machine.");
      end loop;
   end Advance_Until_Idle;
   procedure Enqueue_Command (Command : Queued_Command) is
   begin
      Machine.Enqueue (Command);
   end Enqueue_Command;

   procedure Reset_Position (Pos : Motor_Position) is
   begin
      Machine.Record_Reset_Position (Pos);
   end Reset_Position;

   procedure Wait_Until_Idle (Last_Command_Index : Command_Index) is
      Deadline : constant Ada.Calendar.Time := Ada.Calendar."+" (Ada.Calendar.Clock, Duration (10.0));
   begin
      while Machine.Last_Executed < Last_Command_Index loop
         Assert (Ada.Calendar.Clock < Deadline, "Timed out waiting for command" & Last_Command_Index'Image);
         delay 0.001;
      end loop;
   end Wait_Until_Idle;

   function Event_Kind_Count (Kind : String) return Natural is
      Samples : Sample_Vectors.Vector;
      Events  : Event_Vectors.Vector;
      Count   : Natural := 0;
   begin
      Machine.Snapshot (Samples, Events);
      for E of Events loop
         if To_String (E.Kind) = Kind then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Event_Kind_Count;

   function Event_Match_Count
     (Kind : String; Label : String := ""; Target : String := ""; Value : String := "") return Natural
   is
      Samples : Sample_Vectors.Vector;
      Events  : Event_Vectors.Vector;
      Count   : Natural := 0;
   begin
      Machine.Snapshot (Samples, Events);
      for E of Events loop
         if
           To_String (E.Kind) = Kind
           and then To_String (E.Label) = Label
           and then To_String (E.Target) = Target
           and then To_String (E.Value) = Value
         then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Event_Match_Count;

   function Expected_Controller_Failure_Contains (Scenario : JSON_Value) return String is
   begin
      if Scenario.Has_Field ("expect") then
         declare
            Expect : constant JSON_Value := Scenario.Get ("expect");
         begin
            if Expect.Has_Field ("controller_failure_contains") then
               return
                 VSS.Strings.Conversions.To_UTF_8_String
                   (Virtual_String'(Expect.Get ("controller_failure_contains").Get));
            end if;
         end;
      end if;

      return "";
   end Expected_Controller_Failure_Contains;

   function Expected_Event_Field (Expected_Event : JSON_Value; Field : String) return String is
   begin
      if Expected_Event.Has_Field (+Field) then
         return VSS.Strings.Conversions.To_UTF_8_String (Virtual_String'(Expected_Event.Get (+Field)));
      else
         return "";
      end if;
   end Expected_Event_Field;

   function Event_Tuple_Image (Kind, Label, Target, Value : String) return String is
   begin
      return Kind & "/" & Label & "/" & Target & "/" & Value;
   end Event_Tuple_Image;

   function Matches_Event_Tuple (Actual : Event; Expected_Event : JSON_Value) return Boolean is
   begin
      return
        To_String (Actual.Kind) = Expected_Event_Field (Expected_Event, "kind")
        and then To_String (Actual.Label) = Expected_Event_Field (Expected_Event, "label")
        and then To_String (Actual.Target) = Expected_Event_Field (Expected_Event, "target")
        and then To_String (Actual.Value) = Expected_Event_Field (Expected_Event, "value");
   end Matches_Event_Tuple;

   function Expected_Event_Tuple_Image (Expected_Event : JSON_Value) return String is
   begin
      return
        Event_Tuple_Image
          (Expected_Event_Field (Expected_Event, "kind"),
           Expected_Event_Field (Expected_Event, "label"),
           Expected_Event_Field (Expected_Event, "target"),
           Expected_Event_Field (Expected_Event, "value"));
   end Expected_Event_Tuple_Image;

   function Find_Scenario_Path (Name : String) return String is
   begin
      return Integration_Test_Catalog.Find_Scenario_Path (Name);
   end Find_Scenario_Path;

   function Get_Board_Specific_Documentation (Key : Virtual_String) return Virtual_String is
   begin
      pragma Unreferenced (Key);
      return +"";
   end Get_Board_Specific_Documentation;

   function Get_Extra_HTTP_Content (Name : Virtual_String) return access constant Ada.Streams.Stream_Element_Array is
   begin
      pragma Unreferenced (Name);
      return null;
   end Get_Extra_HTTP_Content;

   procedure Add_Override
     (Result : in out Config.Config_Override_Vectors.Vector;
      Owner  : String;
      Path   : Config.Config_Data_Paths.Vector;
      Value  : JSON_Value) is
   begin
      Result.Append (Config.Config_Override'(Owner => +Owner, Path => Path, Value => Value));
   end Add_Override;

   function Build_Config_Overrides return Config.Config_Override_Vectors.Vector is
      use Config;
      Result : Config_Override_Vectors.Vector;
      type Virtual_String_Array is array (Positive range <>) of Virtual_String;
      Axes   : constant Virtual_String_Array := [+"X_AXIS", +"Y_AXIS", +"Z_AXIS"];
      Motors : constant Virtual_String_Array := [+"X_MOTOR", +"Y_MOTOR", +"Z_MOTOR", +"E_MOTOR"];
   begin
      Add_Override (Result, "Basic Config", ["Prunt", "Enabled"], Create (True));

      for Axis of Axes loop
         Add_Override
           (Result,
            "Homing",
            ["Homing", Axis, "Homing_Method", "Kind", "Selected"],
            Create (+"Set_To_Value"));
      end loop;

      for Motor of Motors loop
         Add_Override (Result, "Motor Drivers", ["Motors", Motor, "Enabled"], Create (True));
         Add_Override
           (Result,
            "Motor Drivers",
            ["Motors",
             Motor,
             "Motion_Units",
             "Kind",
             "Children",
             "Direct_Entry",
             "Direct_Entry",
             "Distance_Per_Rotation"],
            Create (Long_Float'(1.0)));
      end loop;

      Add_Override (Result, "Kinematics", ["Kinematics", "Lower_Position_Limit", "E_AXIS"], Create (Long_Float'(-1.0E100)));
      Add_Override (Result, "Kinematics", ["Kinematics", "Upper_Position_Limit", "X_AXIS"], Create (Long_Float'(300.0)));
      Add_Override (Result, "Kinematics", ["Kinematics", "Upper_Position_Limit", "Y_AXIS"], Create (Long_Float'(300.0)));
      Add_Override (Result, "Kinematics", ["Kinematics", "Upper_Position_Limit", "Z_AXIS"], Create (Long_Float'(300.0)));
      Add_Override (Result, "Kinematics", ["Kinematics", "Upper_Position_Limit", "E_AXIS"], Create (Long_Float'(1.0E100)));
      Add_Override (Result, "Kinematics", ["Kinematics", "Maximum_Tangential_Velocity"], Create (Long_Float'(250.0)));
      Add_Override (Result, "Kinematics", ["Kinematics", "Axial_Velocity_Limits", "X_AXIS"], Create (Long_Float'(250.0)));
      Add_Override (Result, "Kinematics", ["Kinematics", "Axial_Velocity_Limits", "Y_AXIS"], Create (Long_Float'(250.0)));
      Add_Override (Result, "Kinematics", ["Kinematics", "Axial_Velocity_Limits", "Z_AXIS"], Create (Long_Float'(25.0)));
      Add_Override (Result, "Kinematics", ["Kinematics", "Axial_Velocity_Limits", "E_AXIS"], Create (Long_Float'(80.0)));
      for Axis of Axes loop
         Add_Override
           (Result,
            "Kinematics",
            ["Kinematics",
             "Cornering",
             "Kind",
             "Children",
             "Stereographic",
             "Stereographic_Params",
             "Axial_Deviation_Limits",
             Axis],
            Create (Long_Float'(0.02)));
      end loop;
      Add_Override
        (Result,
         "Kinematics",
         ["Kinematics",
          "Cornering",
          "Kind",
          "Children",
          "Stereographic",
          "Stereographic_Params",
          "Maximum_Corner_Miss_Distance"],
         Create (Long_Float'(0.02)));
      for Axis of Axes loop
         Add_Override
           (Result, "Kinematics", ["Kinematics", "Axial_Acceleration_Limits", Axis], Create (Long_Float'(5_000.0)));
         Add_Override
           (Result, "Kinematics", ["Kinematics", "Axial_Jerk_Limits", Axis], Create (Long_Float'(500_000.0)));
         Add_Override
           (Result, "Kinematics", ["Kinematics", "Axial_Snap_Limits", Axis], Create (Long_Float'(500_000_000.0)));
         Add_Override
           (Result,
            "Kinematics",
            ["Kinematics", "Axial_Crackle_Limits", Axis],
            Create (Long_Float'(500_000_000_000.0)));
      end loop;
      Add_Override
        (Result,
         "Kinematics",
         ["Kinematics", "Kinematics_Kind", "Kind", "Children", "Cartesian", "Cartesian", "X_MOTOR"],
         Create (+"X_AXIS"));
      Add_Override
        (Result,
         "Kinematics",
         ["Kinematics", "Kinematics_Kind", "Kind", "Children", "Cartesian", "Cartesian", "Y_MOTOR"],
         Create (+"Y_AXIS"));
      Add_Override
        (Result,
         "Kinematics",
         ["Kinematics", "Kinematics_Kind", "Kind", "Children", "Cartesian", "Cartesian", "Z_MOTOR"],
         Create (+"Z_AXIS"));
      Add_Override
        (Result,
         "Kinematics",
         ["Kinematics", "Kinematics_Kind", "Kind", "Children", "Cartesian", "Cartesian", "E_MOTOR"],
         Create (+"E_AXIS"));

      Add_Override
        (Result,
         "Thermistors",
         ["Thermistors", "DUMMY_THERMISTOR", "Minimum_Temperature"],
         Create (Long_Float'(-20.0)));
      Add_Override
        (Result,
         "Thermistors",
         ["Thermistors", "DUMMY_THERMISTOR", "Maximum_Temperature"],
         Create (Long_Float'(300.0)));

      return Result;
   end Build_Config_Overrides;

   Integration_Config_Path : constant String := "integration_test_config.json";
   Integration_Interpolation_Time : constant Time := Dimensionless (Sample_Period_S) * s;

   package Controller_Instance is new
     Prunt.Controller
       (Generic_Types                           => Generic_Types,
        Hardware                                => Hardware,
        Interpolation_Time                      => Integration_Interpolation_Time,
        Enqueue_Command                         => Enqueue_Command,
        Reset_Position                          => Reset_Position,
        Wait_Until_Idle                         => Wait_Until_Idle,
        Reset_Hardware                          => Machine.Clear,
        Config_Path                             => Integration_Config_Path,
        Config_Overrides                        => Build_Config_Overrides,
        Get_Extra_HTTP_Content                  => Get_Extra_HTTP_Content,
        Get_Board_Specific_Documentation        => Get_Board_Specific_Documentation,
        Executed_Command_Position_Ring_Capacity => 65_536);

   procedure Report_Command_Executed (Index : Command_Index) is
   begin
      Controller_Instance.Report_Last_Command_Executed (Index);
   end Report_Command_Executed;

   procedure Wait_For_Gcode_Ready is
      Deadline : constant Ada.Calendar.Time := Ada.Calendar."+" (Ada.Calendar.Clock, Duration (20.0));
   begin
      while not Controller_Instance.Ready_For_Gcode loop
         Advance_By (0.005, 20);
         delay 0.001;
         Assert (Ada.Calendar.Clock < Deadline, "Timed out waiting for controller G-code processor.");
      end loop;
   end Wait_For_Gcode_Ready;

   procedure Wait_For_Motion (Minimum_New_Samples : Natural; Timeout_S : Long_Float) is
      Start_Sample_Count : constant Natural := Machine.Sample_Count;
      Target_Samples     : constant Natural := Start_Sample_Count + Minimum_New_Samples;
      Deadline           : constant Ada.Calendar.Time := Ada.Calendar."+" (Ada.Calendar.Clock, Duration (Timeout_S));
   begin
      while Machine.Sample_Count < Target_Samples loop
         Advance_By (0.002, Submit_Empty_Wait_Limit);
         delay 0.001;
         Assert (Ada.Calendar.Clock < Deadline, "Timed out waiting for deterministic motion samples.");
      end loop;
   end Wait_For_Motion;

   procedure Run_Cancel_At
     (Time_S : Long_Float; Cancel : access procedure (Succeeded : out Boolean))
   is
   begin
      Advance_To (Time_S, Cancel_Empty_Wait_Limit);
      Assert
        (Machine.Current_Time >= Time_S,
         "Timed out waiting for motion before cancellation time " & Image (Time_S) & "s.");
      Log_Event ("cancel", "cancel_requested");

      declare
         protected Cancel_Result is
            procedure Set (Value : Boolean);
            function Done return Boolean;
            function Succeeded return Boolean;
         private
            Is_Done : Boolean := False;
            Success : Boolean := False;
         end Cancel_Result;

         protected body Cancel_Result is
            procedure Set (Value : Boolean) is
            begin
               Success := Value;
               Is_Done := True;
            end Set;

            function Done return Boolean is
            begin
               return Is_Done;
            end Done;

            function Succeeded return Boolean is
            begin
               return Success;
            end Succeeded;
         end Cancel_Result;

         task Canceller;

         task body Canceller is
            Success : Boolean := False;
         begin
            Cancel (Success);
            Cancel_Result.Set (Success);
         exception
            when others =>
               Cancel_Result.Set (False);
         end Canceller;

         Deadline : constant Ada.Calendar.Time := Ada.Calendar."+" (Ada.Calendar.Clock, Duration (10.0));
      begin
         while not Cancel_Result.Done loop
            Advance_By (0.002);
            delay 0.001;
            Assert (Ada.Calendar.Clock < Deadline, "Timed out waiting for cancellation to complete.");
         end loop;

         Assert (Cancel_Result.Succeeded, "Controller rejected cancellation.");
      end;
      Log_Event ("cancel", "cancel_completed");
   end Run_Cancel_At;

   procedure Run_Action
     (Action       : JSON_Value;
      Submit       : access procedure
        (Command : Virtual_String; Succeeded : out Boolean; Command_ID : out Gcode_Command_ID);
      Submit_File  : access procedure (Path : Virtual_String; Succeeded : out Boolean);
      Cancel       : access procedure (Succeeded : out Boolean))
   is
      Kind : constant String := VSS.Strings.Conversions.To_UTF_8_String (Virtual_String'(Action.Get ("kind").Get));
   begin
      if Kind = "submit_gcode" then
         declare
            Command    : constant Virtual_String := Action.Get ("command").Get;
            Success    : Boolean := False;
            Command_ID : Gcode_Command_ID;
            Deadline   : constant Ada.Calendar.Time := Ada.Calendar."+" (Ada.Calendar.Clock, Submit_Timeout_S);
         begin
            Log_Event ("gcode", "submit_gcode", Value => VSS.Strings.Conversions.To_UTF_8_String (Command));

            while not Success loop
               Submit (Command, Success, Command_ID);
               exit when Success;
               Advance_By (0.002, Submit_Empty_Wait_Limit);
               Assert (Ada.Calendar.Clock < Deadline, "Timed out submitting G-code command.");
            end loop;

            Assert
              (Success,
               "Controller rejected G-code command: " & VSS.Strings.Conversions.To_UTF_8_String (Command));
            Assert (Command_ID /= 0, "Controller accepted a G-code command without assigning an ID.");
            Advance_By (0.002, Submit_Empty_Wait_Limit);
         end;
      elsif Kind = "submit_gcode_file" then
         declare
            Path    : Virtual_String := Prunt.Next_Test_Filename;
            Success : Boolean := False;
         begin
            if Action.Has_Field ("path") then
               Path := Action.Get ("path").Get;
            end if;

            if Action.Has_Field ("commands") then
               declare
                  File : Prunt.Mockable.Text_IO.File_Type;
               begin
                  Prunt.Mockable.Text_IO.Create
                    (File, Prunt.Mockable.Text_IO.Out_File, VSS.Strings.Conversions.To_UTF_8_String (Path));
                  for Command of Action.Get_Array ("commands") loop
                     declare
                        Line : constant String :=
                          VSS.Strings.Conversions.To_UTF_8_String (Virtual_String'(Command.Get));
                     begin
                        Log_Event ("gcode", "write_gcode_file_line", Value => Line);
                        Prunt.Mockable.Text_IO.Put_Line (File, Line);
                     end;
                  end loop;
                  Prunt.Mockable.Text_IO.Close (File);
               end;
            end if;

            Log_Event ("gcode", "submit_gcode_file", Value => VSS.Strings.Conversions.To_UTF_8_String (Path));
            declare
               Deadline : constant Ada.Calendar.Time := Ada.Calendar."+" (Ada.Calendar.Clock, Submit_Timeout_S);
            begin
               while not Success loop
                  Submit_File (Path, Success);
                  exit when Success;
                  Advance_By (0.002, Submit_Empty_Wait_Limit);
                  Assert (Ada.Calendar.Clock < Deadline, "Timed out submitting G-code file.");
               end loop;
            end;
            Assert
              (Success,
               "Controller rejected G-code file: " & VSS.Strings.Conversions.To_UTF_8_String (Path));
         end;
      elsif Kind = "advance_to_s" then
         Advance_To (Long_Float'(Action.Get ("time_s").Get), Long_Empty_Wait_Limit);
      elsif Kind = "advance_by_s" then
         Advance_By (Long_Float'(Action.Get ("duration_s").Get), Long_Empty_Wait_Limit);
      elsif Kind = "wait_for_motion" then
         declare
            Sample_Count : Natural := 1;
            Timeout      : Long_Float := 60.0;
         begin
            if Action.Has_Field ("sample_count") then
               Sample_Count := Natural'(Action.Get ("sample_count").Get);
            end if;
            if Action.Has_Field ("timeout_s") then
               Timeout := Action.Get ("timeout_s").Get;
            end if;

            Wait_For_Motion (Sample_Count, Timeout);
         end;
      elsif Kind = "cancel_at_s" then
         Run_Cancel_At (Long_Float'(Action.Get ("time_s").Get), Cancel);
      elsif Kind = "pause" then
         declare
            Deadline : constant Ada.Calendar.Time := Ada.Calendar."+" (Ada.Calendar.Clock, Duration (20.0));
         begin
            Log_Event ("pause", "pause_requested");
            Controller_Instance.Pause_Stepgen;

            while not Controller_Instance.Stepgen_Paused loop
               Advance_By (0.01, Long_Empty_Wait_Limit);
               delay 0.001;
               Assert (Ada.Calendar.Clock < Deadline, "Timed out waiting for pause.");
            end loop;

            Log_Event ("pause", "paused");
         end;
      elsif Kind = "pause_until_controller_stops" then
         declare
            Deadline : constant Ada.Calendar.Time := Ada.Calendar."+" (Ada.Calendar.Clock, Duration (20.0));
         begin
            Log_Event ("pause", "pause_requested");
            Controller_Instance.Pause_Stepgen;

            while not Controller_Task_State.Stopped loop
               begin
                  Advance_By (0.01, Long_Empty_Wait_Limit);
               exception
                  when others =>
                     if not Controller_Task_State.Stopped then
                        raise;
                     end if;
               end;
               delay 0.001;
               if Ada.Calendar.Clock >= Deadline then
                  raise Program_Error with "Timed out waiting for controller to stop.";
               end if;
            end loop;
         end;
      elsif Kind = "resume" then
         declare
            Deadline : constant Ada.Calendar.Time := Ada.Calendar."+" (Ada.Calendar.Clock, Duration (20.0));
         begin
            Log_Event ("pause", "resume_requested");
            Controller_Instance.Resume_Stepgen;

            while Controller_Instance.Stepgen_Paused loop
               Advance_By (0.01, Long_Empty_Wait_Limit);
               delay 0.001;
               Assert (Ada.Calendar.Clock < Deadline, "Timed out waiting for resume.");
            end loop;

            Log_Event ("pause", "resumed");
         end;
      elsif Kind = "wait_idle" then
         declare
            Timeout : Long_Float := 10.0;
         begin
            if Action.Has_Field ("timeout_s") then
               Timeout := Action.Get ("timeout_s").Get;
            end if;
            Advance_Until_Idle (Timeout);
         end;
      else
         raise Constraint_Error with "Unknown integration action kind: " & Kind;
      end if;
   end Run_Action;

   procedure Run_Expectations (Expect : JSON_Value) is
      Pos : constant Axis_Position := Machine.Current_Position;
   begin
      if Expect.Has_Field ("final_position") then
         declare
            Tolerance : Long_Float := 0.01;

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value);

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value) is
               Name_String : constant String := VSS.Strings.Conversions.To_UTF_8_String (Name);
               Expected    : constant Long_Float := Value.Get;
               Actual      : constant Long_Float := Axis_Value (Pos, Name_String);
            begin
               Assert
                 (abs (Actual - Expected) <= Tolerance,
                  "Axis "
                  & Name_String
                  & " final position was "
                  & Image (Actual)
                  & ", expected "
                  & Image (Expected));
            end Check_Axis;
         begin
            if Expect.Has_Field ("position_tolerance") then
               Tolerance := Expect.Get ("position_tolerance").Get;
            end if;
            Expect.Get ("final_position").Map_JSON_Object (Check_Axis'Access);
         end;
      end if;

      if Expect.Has_Field ("max_position") then
         declare
            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value);

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value) is
               Name_String : constant String := VSS.Strings.Conversions.To_UTF_8_String (Name);
               Max_Value   : constant Long_Float := Value.Get;
               Actual      : constant Long_Float := Axis_Value (Pos, Name_String);
            begin
               Assert
                 (Actual <= Max_Value,
                  "Axis " & Name_String & " final position exceeded " & Image (Max_Value) & ": " & Image (Actual));
            end Check_Axis;
         begin
            Expect.Get ("max_position").Map_JSON_Object (Check_Axis'Access);
         end;
      end if;

      if Expect.Has_Field ("max_sample_position") then
         declare
            Samples : Sample_Vectors.Vector;
            Events  : Event_Vectors.Vector;

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value);

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value) is
               Name_String : constant String := VSS.Strings.Conversions.To_UTF_8_String (Name);
               Max_Value   : constant Long_Float := Value.Get;
               Actual      : Long_Float := Long_Float'First;
            begin
               for S of Samples loop
                  Actual := Long_Float'Max (Actual, Axis_Value (S.Position, Name_String));
               end loop;

               Assert
                 (Actual <= Max_Value,
                  "Axis "
                  & Name_String
                  & " sampled position exceeded "
                  & Image (Max_Value)
                  & ": "
                  & Image (Actual));
            end Check_Axis;
         begin
            Machine.Snapshot (Samples, Events);
            pragma Unreferenced (Events);
            Expect.Get ("max_sample_position").Map_JSON_Object (Check_Axis'Access);
         end;
      end if;

      if Expect.Has_Field ("sample_position_reaches") then
         declare
            Samples : Sample_Vectors.Vector;
            Events  : Event_Vectors.Vector;

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value);

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value) is
               Name_String : constant String := VSS.Strings.Conversions.To_UTF_8_String (Name);
               Min_Value   : constant Long_Float := Value.Get;
               Actual      : Long_Float := Long_Float'First;
            begin
               for S of Samples loop
                  Actual := Long_Float'Max (Actual, Axis_Value (S.Position, Name_String));
               end loop;

               Assert
                 (Actual >= Min_Value,
                  "Axis "
                  & Name_String
                  & " sampled position did not reach "
                  & Image (Min_Value)
                  & ": "
                  & Image (Actual));
            end Check_Axis;
         begin
            Machine.Snapshot (Samples, Events);
            pragma Unreferenced (Events);
            Expect.Get ("sample_position_reaches").Map_JSON_Object (Check_Axis'Access);
         end;
      end if;

      if Expect.Has_Field ("sample_position_drops_below") then
         declare
            Samples : Sample_Vectors.Vector;
            Events  : Event_Vectors.Vector;

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value);

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value) is
               Name_String : constant String := VSS.Strings.Conversions.To_UTF_8_String (Name);
               Max_Value   : constant Long_Float := Value.Get;
               Actual      : Long_Float := Long_Float'Last;
            begin
               for S of Samples loop
                  Actual := Long_Float'Min (Actual, Axis_Value (S.Position, Name_String));
               end loop;

               Assert
                 (Actual <= Max_Value,
                  "Axis "
                  & Name_String
                  & " sampled position did not drop below "
                  & Image (Max_Value)
                  & ": "
                  & Image (Actual));
            end Check_Axis;
         begin
            Machine.Snapshot (Samples, Events);
            pragma Unreferenced (Events);
            Expect.Get ("sample_position_drops_below").Map_JSON_Object (Check_Axis'Access);
         end;
      end if;

      if Expect.Has_Field ("max_sample_delta") then
         declare
            Samples : Sample_Vectors.Vector;
            Events  : Event_Vectors.Vector;

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value);

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value) is
               Name_String : constant String := VSS.Strings.Conversions.To_UTF_8_String (Name);
               Max_Value   : constant Long_Float := Value.Get;
               Actual      : Long_Float := 0.0;
               Previous    : Long_Float := 0.0;
               First       : Boolean := True;
            begin
               for S of Samples loop
                  declare
                     Current : constant Long_Float := Axis_Value (S.Position, Name_String);
                  begin
                     if First then
                        First := False;
                     else
                        Actual := Long_Float'Max (Actual, abs (Current - Previous));
                     end if;
                     Previous := Current;
                  end;
               end loop;

               Assert
                 (Actual <= Max_Value,
                  "Axis "
                  & Name_String
                  & " sampled delta exceeded "
                  & Image (Max_Value)
                  & ": "
                  & Image (Actual));
            end Check_Axis;
         begin
            Machine.Snapshot (Samples, Events);
            pragma Unreferenced (Events);
            Expect.Get ("max_sample_delta").Map_JSON_Object (Check_Axis'Access);
         end;
      end if;

      if Expect.Has_Field ("sample_delta_reaches") then
         declare
            Samples : Sample_Vectors.Vector;
            Events  : Event_Vectors.Vector;

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value);

            procedure Check_Axis (Name : Virtual_String; Value : JSON_Value) is
               Name_String : constant String := VSS.Strings.Conversions.To_UTF_8_String (Name);
               Min_Value   : constant Long_Float := Value.Get;
               Actual      : Long_Float := 0.0;
               Previous    : Long_Float := 0.0;
               First       : Boolean := True;
            begin
               for S of Samples loop
                  declare
                     Current : constant Long_Float := Axis_Value (S.Position, Name_String);
                  begin
                     if First then
                        First := False;
                     else
                        Actual := Long_Float'Max (Actual, abs (Current - Previous));
                     end if;
                     Previous := Current;
                  end;
               end loop;

               Assert
                 (Actual >= Min_Value,
                  "Axis "
                  & Name_String
                  & " sampled delta did not reach "
                  & Image (Min_Value)
                  & ": "
                  & Image (Actual));
            end Check_Axis;
         begin
            Machine.Snapshot (Samples, Events);
            pragma Unreferenced (Events);
            Expect.Get ("sample_delta_reaches").Map_JSON_Object (Check_Axis'Access);
         end;
      end if;

      if Expect.Has_Field ("min_samples") then
         Assert
           (Machine.Sample_Count >= Natural'(Expect.Get ("min_samples").Get),
            "Trace did not contain enough samples.");
      end if;

      if Expect.Has_Field ("min_events") then
         Assert
           (Machine.Event_Count >= Natural'(Expect.Get ("min_events").Get), "Trace did not contain enough events.");
      end if;

      if Expect.Has_Field ("event_counts") then
         declare
            procedure Check_Event_Count (Name : Virtual_String; Value : JSON_Value);

            procedure Check_Event_Count (Name : Virtual_String; Value : JSON_Value) is
               Kind : constant String := VSS.Strings.Conversions.To_UTF_8_String (Name);
               Expected : constant Natural := Natural'(Value.Get);
               Actual   : constant Natural := Event_Kind_Count (Kind);
            begin
               Assert
                 (Actual = Expected,
                  "Event kind " & Kind & " occurred" & Actual'Image & " times, expected exactly" & Expected'Image);
            end Check_Event_Count;
         begin
            Expect.Get ("event_counts").Map_JSON_Object (Check_Event_Count'Access);
         end;
      end if;

      if Expect.Has_Field ("events") then
         for Expected_Event of Expect.Get_Array ("events") loop
            declare
               Kind      : constant String :=
                 Expected_Event_Field (Expected_Event, "kind");
               Label     : constant String :=
                 Expected_Event_Field (Expected_Event, "label");
               Target    : constant String :=
                 Expected_Event_Field (Expected_Event, "target");
               Value     : constant String :=
                 Expected_Event_Field (Expected_Event, "value");
               Expected  : constant Natural :=
                 (if Expected_Event.Has_Field ("count")
                  then Natural'(Expected_Event.Get ("count").Get)
                  elsif Expected_Event.Has_Field ("min_count")
                  then Natural'(Expected_Event.Get ("min_count").Get)
                  else 1);
               Actual    : constant Natural := Event_Match_Count (Kind, Label, Target, Value);
               Is_Minimum : constant Boolean :=
                 Expected_Event.Has_Field ("min_count") and then not Expected_Event.Has_Field ("count");
            begin
               Assert
                 ((if Is_Minimum then Actual >= Expected else Actual = Expected),
                  "Event "
                  & Event_Tuple_Image (Kind, Label, Target, Value)
                  & " occurred"
                  & Actual'Image
                  & " times, expected "
                  & (if Is_Minimum then "at least" else "exactly")
                  & Expected'Image);
            end;
         end loop;
      end if;

      if Expect.Has_Field ("event_order") then
         declare
            Samples : Sample_Vectors.Vector;
            Events  : Event_Vectors.Vector;
            Search_Start : Natural := 1;
         begin
            Machine.Snapshot (Samples, Events);
            pragma Unreferenced (Samples);

            for Expected_Event of Expect.Get_Array ("event_order") loop
               declare
                  Found : Boolean := False;
               begin
                  while Search_Start <= Natural (Events.Length) loop
                     if Matches_Event_Tuple (Events.Element (Positive (Search_Start)), Expected_Event) then
                        Found := True;
                        Search_Start := Search_Start + 1;
                        exit;
                     end if;

                     Search_Start := Search_Start + 1;
                  end loop;

                  Assert
                    (Found,
                     "Expected event "
                     & Expected_Event_Tuple_Image (Expected_Event)
                     & " was not found in the required order.");
               end;
            end loop;
         end;
      end if;
   end Run_Expectations;

   procedure Record_Final_Trace (Scenario : JSON_Value; Name : String) is
      Samples : Sample_Vectors.Vector;
      Events  : Event_Vectors.Vector;
      Trace   : constant JSON_Value := Create_Object;
      Axes    : JSON_Array := Empty_Array;
      T       : JSON_Array := Empty_Array;
      Commands : JSON_Array := Empty_Array;
      Position : JSON_Array := Empty_Array;
      Velocity : JSON_Array := Empty_Array;
      Accel    : JSON_Array := Empty_Array;
      Jerk     : JSON_Array := Empty_Array;
      Snap     : JSON_Array := Empty_Array;
      Crackle  : JSON_Array := Empty_Array;
      Event_Array : JSON_Array := Empty_Array;
      Output_Dir : constant String := "integration_traces";
      Output_Path : constant String := Output_Dir & "/" & Name & ".json";
      File : Ada.Text_IO.File_Type;
   begin
      Machine.Snapshot (Samples, Events);
      Ada.Directories.Create_Path (Output_Dir);

      for Axis in Axis_Name loop
         Axes.Append (Create (+Axis_Label (Axis)));
      end loop;

      for Axis in Axis_Name loop
         declare
            Pos_Axis : JSON_Array := Empty_Array;
            Vel_Axis : JSON_Array := Empty_Array;
            Acc_Axis : JSON_Array := Empty_Array;
            Jerk_Axis : JSON_Array := Empty_Array;
            Snap_Axis : JSON_Array := Empty_Array;
            Crackle_Axis : JSON_Array := Empty_Array;
         begin
            for S of Samples loop
               if Axis = Axis_Name'First then
                  T.Append (Create (S.T));
                  Commands.Append (Create (Long_Long_Integer (S.Command)));
               end if;
               Pos_Axis.Append (Create (S.Position (Axis)));
               Vel_Axis.Append (Create (S.Velocity (Axis)));
               Acc_Axis.Append (Create (S.Acceleration (Axis)));
               Jerk_Axis.Append (Create (S.Jerk (Axis)));
               Snap_Axis.Append (Create (S.Snap (Axis)));
               Crackle_Axis.Append (Create (S.Crackle (Axis)));
            end loop;
            Position.Append (Create (Pos_Axis));
            Velocity.Append (Create (Vel_Axis));
            Accel.Append (Create (Acc_Axis));
            Jerk.Append (Create (Jerk_Axis));
            Snap.Append (Create (Snap_Axis));
            Crackle.Append (Create (Crackle_Axis));
         end;
      end loop;

      for E of Events loop
         declare
            Event_Object : constant JSON_Value := Create_Object;
         begin
            Event_Object.Set_Field_Long_Float ("t_s", E.T);
            Event_Object.Set_Field ("kind", +To_String (E.Kind));
            Event_Object.Set_Field ("label", +To_String (E.Label));
            Event_Object.Set_Field ("target", +To_String (E.Target));
            Event_Object.Set_Field ("value", +To_String (E.Value));
            Event_Object.Set_Field ("command_index", Create (Long_Long_Integer (E.Command)));
            Event_Array.Append (Event_Object);
         end;
      end loop;

      Trace.Set_Field ("name", +Name);
      if Scenario.Has_Field ("description") then
         declare
            Description : constant JSON_Value := Scenario.Get ("description");
         begin
            Trace.Set_Field (+"description", Description);
         end;
      end if;
      Trace.Set_Field ("axes", Axes);
      Trace.Set_Field_Long_Float ("dt_s", Sample_Period_S);
      Trace.Set_Field (+"samples", Create (Long_Long_Integer (Natural (Samples.Length))));
      Trace.Set_Field ("events", Event_Array);
      Trace.Set_Field ("t", T);
      Trace.Set_Field ("command_index", Commands);
      Trace.Set_Field ("position", Position);
      Trace.Set_Field ("velocity", Velocity);
      Trace.Set_Field ("acceleration", Accel);
      Trace.Set_Field ("jerk", Jerk);
      Trace.Set_Field ("snap", Snap);
      Trace.Set_Field ("crackle", Crackle);

      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Output_Path);
      Ada.Text_IO.Put (File, VSS.Strings.Conversions.To_UTF_8_String (Write (Trace, Compact => False)));
      Ada.Text_IO.Close (File);
      pragma Unreferenced (File);
   end Record_Final_Trace;

   procedure Run_Scenario (Name : String; Xcov_Dump : Boolean) is
      Scenario : constant JSON_Value := Read_File (Find_Scenario_Path (Name)).Value;
      Expected_Controller_Failure : constant String := Expected_Controller_Failure_Contains (Scenario);
      Expecting_Controller_Failure : constant Boolean := Expected_Controller_Failure /= "";
      Observed_Controller_Failure  : Boolean := False;
      task type Controller_Task_Type;

      procedure Check_Expected_Controller_Failure;

      procedure Check_Expected_Controller_Failure is
         Message : constant String := Controller_Task_State.Message;
      begin
         if not Controller_Task_State.Stopped then
            raise Program_Error
              with "Expected controller task failure containing: " & Expected_Controller_Failure;
         end if;

         if Ada.Strings.Fixed.Index (Message, Expected_Controller_Failure) = 0 then
            raise Program_Error
              with "Controller task failure did not contain """
                   & Expected_Controller_Failure
                   & """: "
                   & Message;
         end if;

         Observed_Controller_Failure := True;
      end Check_Expected_Controller_Failure;

      task body Controller_Task_Type is
      begin
         Controller_Instance.Run;
         declare
            Last_Error : constant String := Controller_Instance.Last_Error_Message;
         begin
            if Last_Error = "" then
               Controller_Task_State.Mark_Stopped ("Controller task stopped before the integration scenario completed.");
            else
               Controller_Task_State.Mark_Stopped ("Controller task stopped after error: " & Last_Error);
            end if;
         end;
      exception
         when E : others =>
            Controller_Task_State.Mark_Stopped ("Controller task failed: " & Ada.Exceptions.Exception_Information (E));
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Controller task failed: " & Ada.Exceptions.Exception_Information (E));
      end Controller_Task_Type;

      type Controller_Task_Access is access Controller_Task_Type;
      Runner : Controller_Task_Access;
      pragma Unreferenced (Runner);
   begin
      Machine.Clear;
      Controller_Task_State.Clear;
      Ada.Directories.Create_Path ("integration_traces");

      if Scenario.Has_Field ("config") then
         Prunt.Integration_Config_Overlays.Apply_Config_Overlay
           (Controller_Instance.Get_Config_Schema_String,
            Scenario.Get ("config"),
            Controller_Instance.Apply_Untrusted_Config_Patch'Access);
         Controller_Instance.Reset_Live_Config_To_Stored;
      end if;

      Runner := new Controller_Task_Type;
      Machine.Wait_Until_Started;
      Wait_For_Gcode_Ready;
      Machine.Clear_Trace;
      Log_Event ("machine", "scenario_start");

      if Scenario.Has_Field ("actions") then
         for Action of Scenario.Get_Array ("actions") loop
            begin
               Run_Action
                 (Action,
                  Controller_Instance.Submit_Gcode_Command'Access,
                  Controller_Instance.Submit_Gcode_File'Access,
                  Controller_Instance.Cancel_Gcode'Access);
            exception
               when others =>
                  if Expecting_Controller_Failure and then Controller_Task_State.Stopped then
                     Check_Expected_Controller_Failure;
                     exit;
                  else
                     raise;
                  end if;
            end;

            if Expecting_Controller_Failure and then Controller_Task_State.Stopped then
               Check_Expected_Controller_Failure;
               exit;
            end if;
         end loop;
      end if;

      if Expecting_Controller_Failure then
         if not Observed_Controller_Failure then
            Check_Expected_Controller_Failure;
         end if;
      else
         Advance_Until_Idle (10.0);

         if Scenario.Has_Field ("expect") then
            Run_Expectations (Scenario.Get ("expect"));
         end if;
      end if;

      Record_Final_Trace (Scenario, Name);

      if Xcov_Dump then
         pragma Annotate (Xcov, Dump_Buffers, "individual_test-" & Name);
         pragma Annotate (Xcov, Reset_Buffers);
      else
         pragma Annotate (Xcov, Dump_Buffers, "integration");
      end if;

      GNAT.OS_Lib.OS_Exit (0);
   exception
      when E : others =>
         begin
            Record_Final_Trace (Scenario, Name);
         exception
            when others =>
               null;
         end;
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Ada.Exceptions.Exception_Information (E));
         Controller_Instance.Report_External_Error ("Integration scenario failed.", Is_Fatal => True);
         GNAT.OS_Lib.OS_Exit (1);
   end Run_Scenario;

   procedure Enable_Motor (Motor : Motor_Name) is
   begin
      Log_Event ("motor", "enable", Motor'Image);
   end Enable_Motor;

   procedure Disable_Motor (Motor : Motor_Name) is
   begin
      Log_Event ("motor", "disable", Motor'Image);
   end Disable_Motor;

   procedure Set_Fan_Duty_Cycle (Fan : Fan_Name; Duty_Cycle : PWM_Scale) is
   begin
      Log_Event ("fan", "set_duty_cycle", Fan'Image, Image (Long_Float (Duty_Cycle)));
   end Set_Fan_Duty_Cycle;

   procedure Reconfigure_Fan (Fan : Fan_Name; PWM_Frequency : Frequency) is
   begin
      Log_Event ("fan", "reconfigure", Fan'Image, Image (Long_Float (PWM_Frequency / hertz)));
   end Reconfigure_Fan;

   procedure Reconfigure_Low_Or_High_Side_Fan
     (Fan : Fan_Name; PWM_Frequency : Frequency; Use_High_Side_Switching : Boolean) is
   begin
      Log_Event
        ("fan",
         "reconfigure_low_or_high_side",
         Fan'Image,
         Image (Long_Float (PWM_Frequency / hertz))
         & " "
         & Boolean'Image (Use_High_Side_Switching));
   end Reconfigure_Low_Or_High_Side_Fan;

   function Get_Tachometer_Frequency (Tachometer : Tachometer_Name; Requires_Fresh : Boolean) return Frequency is
   begin
      Log_Event ("tachometer", "read_frequency", Tachometer'Image, Boolean'Image (Requires_Fresh));
      return 0.0 * hertz;
   end Get_Tachometer_Frequency;

   function Get_Input_Switch_State (Switch : Input_Switch_Name) return Boolean is
   begin
      Log_Event ("input_switch", "read_state", Switch'Image);
      return False;
   end Get_Input_Switch_State;

   procedure Reconfigure_Heater
     (Heater : Heater_Name; Params : Heater_Parameters; Assigned_Thermistor : Thermistor_Name) is
   begin
      pragma Unreferenced (Params);
      Log_Event ("heater", "reconfigure", Heater'Image, Assigned_Thermistor'Image);
   end Reconfigure_Heater;

   procedure Set_Heater_Temperature (Heater : Heater_Name; Target : Temperature) is
   begin
      Log_Event ("heater", "set_temperature", Heater'Image, Image (Long_Float (Target / celsius)));
   end Set_Heater_Temperature;

   procedure Autotune_Heater
     (Heater : Heater_Name; Params : Heater_Parameters; Assigned_Thermistor : Thermistor_Name) is
   begin
      pragma Unreferenced (Params);
      Log_Event ("heater", "autotune", Heater'Image, Assigned_Thermistor'Image);
   end Autotune_Heater;

   procedure Reconfigure_Thermistor
     (Thermistor : Thermistor_Name; Params : Thermistors.Thermistor_Parameters) is
   begin
      pragma Unreferenced (Params);
      Log_Event ("thermistor", "reconfigure", Thermistor'Image);
   end Reconfigure_Thermistor;

   function Get_Thermistor_Temperature (Thermistor : Thermistor_Name; Requires_Fresh : Boolean) return Temperature is
   begin
      Log_Event ("thermistor", "read_temperature", Thermistor'Image, Boolean'Image (Requires_Fresh));
      return 25.0 * celsius;
   end Get_Thermistor_Temperature;

   function Get_Board_Temperature
     (Probe : Board_Temperature_Probe_Name; Requires_Fresh : Boolean) return Temperature is
   begin
      Log_Event ("board_temperature", "read_temperature", Probe'Image, Boolean'Image (Requires_Fresh));
      return 25.0 * celsius;
   end Get_Board_Temperature;

end Prunt.Integration_Test_Harness;
