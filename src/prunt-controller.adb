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

with Ada.Containers.Ordered_Sets;
with Ada.Tags;
with Ada.Task_Identification;
with Ada.Task_Termination;
with Prunt.Controller_Interfaces;
with Prunt.Gcode_Arguments;
with VSS.Strings.Conversions;

package body Prunt.Controller is

   pragma Extensions_Allowed (On);

   protected body Idle_Notification_State is
      procedure Reset is
      begin
         Phase := Active;
         Generation := @ + 1;
         Active_Activity_Count := 0;
         Latest_Completion_Serial := @ + 1;
         Completion_Pending := False;
      end Reset;

      entry Begin_Activity (Notify : out Boolean) when Phase /= Starting_Idle is
      begin
         Generation := @ + 1;
         Active_Activity_Count := @ + 1;
         Notify := Phase = Idle;
         if Notify then
            Phase := Ending_Idle;
         end if;
      end Begin_Activity;

      procedure Finish_Idle_End is
      begin
         if Phase = Ending_Idle then
            Phase := Active;
         end if;
      end Finish_Idle_End;

      procedure Complete_Activity (Last_Command_Index : Command_Index) is
      begin
         if Active_Activity_Count = 0 then
            raise Constraint_Error with "Completed idle-notification activity without a matching start.";
         end if;

         Active_Activity_Count := @ - 1;
         if Active_Activity_Count = 0 then
            Publish_Completion_When_Inactive (Last_Command_Index);
         end if;
      end Complete_Activity;

      entry Abandon_Activities (Last_Command_Index : Command_Index) when Phase in Active | Idle is
      begin
         Generation := @ + 1;
         Active_Activity_Count := 0;
         Completion_Pending := False;

         if Phase = Active then
            Publish_Completion_When_Inactive (Last_Command_Index);
         end if;
      end Abandon_Activities;

      procedure Publish_Completion_When_Inactive (Last_Command_Index : Command_Index) is
      begin
         if Active_Activity_Count = 0 then
            Latest_Completion_Serial := @ + 1;
            Pending_Completion :=
              (Generation         => Generation,
               Completion_Serial  => Latest_Completion_Serial,
               Last_Command_Index => Last_Command_Index);
            Completion_Pending := True;
         end if;
      end Publish_Completion_When_Inactive;

      entry Wait_For_Completion (Completion : out Idle_Activity_Completion) when Completion_Pending is
      begin
         Completion := Pending_Completion;
         Completion_Pending := False;
      end Wait_For_Completion;

      procedure Begin_Idle (Completion : Idle_Activity_Completion; Notify : out Boolean) is
      begin
         Notify :=
           Phase = Active
           and then Active_Activity_Count = 0
           and then Completion.Generation = Generation
           and then Completion.Completion_Serial = Latest_Completion_Serial;
         if Notify then
            Phase := Starting_Idle;
         end if;
      end Begin_Idle;

      procedure Finish_Idle is
      begin
         if Phase = Starting_Idle then
            Phase := Idle;
         end if;
      end Finish_Idle;
   end Idle_Notification_State;

   protected body Handler_Instances is
      procedure Load (New_Handlers : Module_Instance_Vectors.Vector) is
      begin
         Handlers := New_Handlers;
      end Load;

      procedure Clear is
      begin
         Handlers.Clear;
      end Clear;

      procedure Snapshot (Result : out Module_Instance_Vectors.Vector) is
      begin
         Result := Handlers;
      end Snapshot;
   end Handler_Instances;

   procedure Notify_Activity_Start is
      Handlers : Module_Instance_Vectors.Vector;
      Notify   : Boolean;
   begin
      Idle_Notification_State.Begin_Activity (Notify);
      if not Notify then
         return;
      end if;

      Idle_Notification_Instances.Snapshot (Handlers);
      for Instance of Handlers loop
         Controller_Interfaces.Idle_Notification_Receiver'Class (Instance.Get.Element.all).Idle_End;
      end loop;
      Idle_Notification_State.Finish_Idle_End;
   exception
      when others =>
         Idle_Notification_State.Finish_Idle_End;
         raise;
   end Notify_Activity_Start;

   procedure Notify_Idle_Start (Completion : Idle_Activity_Completion) is
      Handlers : Module_Instance_Vectors.Vector;
      Notify   : Boolean;
   begin
      Idle_Notification_State.Begin_Idle (Completion, Notify);
      if not Notify then
         return;
      end if;

      Idle_Notification_Instances.Snapshot (Handlers);
      for Instance of Handlers loop
         Controller_Interfaces.Idle_Notification_Receiver'Class (Instance.Get.Element.all).Idle_Start;
      end loop;
      Idle_Notification_State.Finish_Idle;
   exception
      when others =>
         Idle_Notification_State.Finish_Idle;
         raise;
   end Notify_Idle_Start;

   protected body Gcode_Cancellation_Barrier is
      entry Start_Cancellation when not Cancellation_Active is
      begin
         Cancellation_Count := @ + 1;
         Cancellation_Active := True;
      end Start_Cancellation;

      procedure Finish_Cancellation is
      begin
         Cancellation_Active := False;
      end Finish_Cancellation;

      entry Start_Submission when not Cancellation_Active is
      begin
         Active_Submissions := @ + 1;
      end Start_Submission;

      procedure Finish_Submission is
      begin
         Active_Submissions := @ - 1;
      end Finish_Submission;

      entry Start_Line when not Cancellation_Active is
      begin
         Processing_Line := True;
      end Start_Line;

      procedure Finish_Line is
      begin
         Processing_Line := False;
      end Finish_Line;

      entry Wait_Until_Not_Processing when not Processing_Line and then Active_Submissions = 0 is
      begin
         null;
      end Wait_Until_Not_Processing;

      entry Wait_Until_Not_Submitting when Active_Submissions = 0 is
      begin
         null;
      end Wait_Until_Not_Submitting;

      entry Wait_Until_Not_Cancelling when not Cancellation_Active is
      begin
         null;
      end Wait_Until_Not_Cancelling;

      function Cancellation_Generation return Cancellation_Generation_Type is
      begin
         return Cancellation_Count;
      end Cancellation_Generation;
   end Gcode_Cancellation_Barrier;

   protected body Planner_State_Type is
      procedure Reset is
      begin
         Last_Position := [others => 0.0 * mm];
         Last_Kinematic_Parameters := (others => <>);
      end Reset;

      function Get_Last_Position return Position is
      begin
         return Last_Position;
      end Get_Last_Position;

      function Get_Last_Kinematic_Parameters return Motion_Planner.Kinematic_Parameters is
      begin
         return Last_Kinematic_Parameters;
      end Get_Last_Kinematic_Parameters;

      procedure Set_Last_Position (Pos : Position) is
      begin
         Last_Position := Pos;
      end Set_Last_Position;

      procedure Set_Last_Kinematic_Parameters (Params : Motion_Planner.Kinematic_Parameters) is
      begin
         Last_Kinematic_Parameters := Params;
      end Set_Last_Kinematic_Parameters;
   end Planner_State_Type;

   overriding
   function Get_Last_Position (This : Planner_Wrapper) return Position is
   begin
      case This.Target is
         when Primary_Planner_Target =>
            return Primary_Planner_State.Get_Last_Position;

         when Pause_Planner_Target   =>
            return Pause_Planner_State.Get_Last_Position;
      end case;
   end Get_Last_Position;

   overriding
   function Get_Last_Kinematic_Parameters (This : Planner_Wrapper) return Motion_Planner.Kinematic_Parameters is
   begin
      case This.Target is
         when Primary_Planner_Target =>
            return Primary_Planner_State.Get_Last_Kinematic_Parameters;

         when Pause_Planner_Target   =>
            return Pause_Planner_State.Get_Last_Kinematic_Parameters;
      end case;
   end Get_Last_Kinematic_Parameters;

   overriding
   function Get_State_Anchor_Corner_ID (This : Planner_Wrapper) return Planner_Corner_ID is
   begin
      case This.Target is
         when Primary_Planner_Target =>
            return My_Motion_Planner.Get_Last_Assigned_Corner_ID;

         when Pause_Planner_Target   =>
            return My_Step_Generator.Get_Last_Executed_Primary_Corner_ID;
      end case;
   end Get_State_Anchor_Corner_ID;

   overriding
   function Get_Last_Executed_Corner_ID (This : Planner_Wrapper) return Planner_Corner_ID is
      pragma Unreferenced (This);
   begin
      return My_Step_Generator.Get_Last_Executed_Primary_Corner_ID;
   end Get_Last_Executed_Corner_ID;

   overriding
   procedure Mark_Axis_Homed (This : Planner_Wrapper; Axis : Axis_Name) is
   begin
      pragma Unreferenced (This, Axis); --  TODO
      null;
   end Mark_Axis_Homed;

   overriding
   function Get_First_Accel_Distance (This : Planner_Block_End_Context) return Length is
   begin
      return This.First_Accel_Distance;
   end Get_First_Accel_Distance;

   overriding
   function Get_Last_Command_Index (This : Planner_Block_End_Context) return Command_Index is
   begin
      return This.Last_Command_Index;
   end Get_Last_Command_Index;

   overriding
   function Get_Loop_Move_Offset (This : Planner_Block_End_Context) return Position_Offset is
   begin
      return This.Loop_Move_Offset;
   end Get_Loop_Move_Offset;

   overriding
   procedure Wait_For_Idle (This : Planner_Block_End_Context) is
   begin
      Wait_Until_Idle (This.Last_Command_Index);
   end Wait_For_Idle;

   overriding
   procedure Catch_Up_Planner_State (This : Planner_Block_End_Context) is
   begin
      Catch_Up_Planner_State_Handlers (This.State_Catch_Up_Corner_ID);
   end Catch_Up_Planner_State;

   overriding
   procedure Prepare_Config_For_Save (This : Planner_Block_End_Context) is
      pragma Unreferenced (This);
   begin
      Prepare_Config_For_Save_Handlers;
   end Prepare_Config_For_Save;

   overriding
   procedure Mark_Axis_Unhomed (This : Planner_Wrapper; Axis : Axis_Name) is
   begin
      pragma Unreferenced (This, Axis); --  TODO
      null;
   end Mark_Axis_Unhomed;

   overriding
   function Axis_Is_Homed (This : Planner_Wrapper; Axis : Axis_Name) return Boolean is
   begin
      --  TODO
      return False;
   end Axis_Is_Homed;

   overriding
   procedure Add_Corner
     (This          : Planner_Wrapper;
      Pos           : Position;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Require_Homed : Boolean := True)
   is
      pragma Unreferenced (Require_Homed);
   begin
      if This.Startup_Mode and then Pos /= This.Get_Last_Position then
         raise Constraint_Error with "Motion not allowed during startup.";
      end if;

      case This.Target is
         when Primary_Planner_Target =>
            My_Motion_Planner.Enqueue_Move (Pos => Pos, Feedrate => Feedrate, Dwell_After => Dwell_After);
            Primary_Planner_State.Set_Last_Position (Pos);

         when Pause_Planner_Target   =>
            My_Pause_Motion_Planner.Enqueue_Move (Pos => Pos, Feedrate => Feedrate, Dwell_After => Dwell_After);
            Pause_Planner_State.Set_Last_Position (Pos);
      end case;
   end Add_Corner;

   overriding
   procedure Add_Helix
     (This          : Planner_Wrapper;
      Center        : Position;
      Pos           : Position;
      Clockwise     : Boolean;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Require_Homed : Boolean := True)
   is
      pragma Unreferenced (Require_Homed);
   begin
      if This.Startup_Mode and then Pos /= This.Get_Last_Position then
         raise Constraint_Error with "Motion not allowed during startup.";
      end if;

      case This.Target is
         when Primary_Planner_Target =>
            My_Motion_Planner.Enqueue_Helix
              (Pos => Pos, Center => Center, Clockwise => Clockwise, Feedrate => Feedrate, Dwell_After => Dwell_After);
            Primary_Planner_State.Set_Last_Position (Pos);

         when Pause_Planner_Target   =>
            My_Pause_Motion_Planner.Enqueue_Helix
              (Pos => Pos, Center => Center, Clockwise => Clockwise, Feedrate => Feedrate, Dwell_After => Dwell_After);
            Pause_Planner_State.Set_Last_Position (Pos);
      end case;
   end Add_Helix;

   overriding
   procedure Add_Corner_Data (This : Planner_Wrapper; Corner_Data : Extra_Corner_Data'Class) is
   begin
      if Corner_Data not in Extra_Corner_Data then
         case This.Target is
            when Primary_Planner_Target =>
               My_Motion_Planner.Enqueue_Corner_Extra_Data (Corner_Data);

            when Pause_Planner_Target   =>
               My_Pause_Motion_Planner.Enqueue_Corner_Extra_Data (Corner_Data);
         end case;
      end if;
   end Add_Corner_Data;

   overriding
   procedure Flush
     (This           : Planner_Wrapper;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False) is
   begin
      if This.Target = Pause_Planner_Target and then Is_Homing_Move then
         raise Constraint_Error with "Homing moves are not allowed in pause plans.";
      end if;

      case This.Target is
         when Primary_Planner_Target =>
            My_Motion_Planner.Enqueue_Flush
              (Extra_Block_Resetting_Data_Holders.To_Holder (Extra_Data), Is_Homing_Move => Is_Homing_Move);

         when Pause_Planner_Target   =>
            My_Pause_Motion_Planner.Enqueue_Flush
              (Extra_Block_Resetting_Data_Holders.To_Holder (Extra_Data), Is_Homing_Move => Is_Homing_Move);
      end case;
   end Flush;

   overriding
   procedure Flush_And_Change_Kinematic_Parameters
     (This           : Planner_Wrapper;
      Params         : Motion_Planner.Kinematic_Parameters;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False) is
   begin
      if This.Target = Pause_Planner_Target and then Is_Homing_Move then
         raise Constraint_Error with "Homing moves are not allowed in pause plans.";
      end if;

      case This.Target is
         when Primary_Planner_Target =>
            My_Motion_Planner.Enqueue_Flush_And_Change_Kinematic_Parameters
              (Extra_Block_Resetting_Data_Holders.To_Holder (Extra_Data), Params, Is_Homing_Move => Is_Homing_Move);
            Primary_Planner_State.Set_Last_Kinematic_Parameters (Params);

         when Pause_Planner_Target   =>
            My_Pause_Motion_Planner.Enqueue_Flush_And_Change_Kinematic_Parameters
              (Extra_Block_Resetting_Data_Holders.To_Holder (Extra_Data), Params, Is_Homing_Move => Is_Homing_Move);
            Pause_Planner_State.Set_Last_Kinematic_Parameters (Params);
      end case;
   end Flush_And_Change_Kinematic_Parameters;

   overriding
   procedure Flush_And_Reset_Position
     (This           : Planner_Wrapper;
      New_Position   : Position;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False) is
   begin
      if This.Target = Pause_Planner_Target and then Is_Homing_Move then
         raise Constraint_Error with "Homing moves are not allowed in pause plans.";
      end if;

      case This.Target is
         when Primary_Planner_Target =>
            My_Motion_Planner.Enqueue_Flush_And_Reset_Position
              (Data           => Extra_Block_Resetting_Data_Holders.To_Holder (Extra_Data),
               Pos            => New_Position,
               Is_Homing_Move => Is_Homing_Move);
            Primary_Planner_State.Set_Last_Position (New_Position);

         when Pause_Planner_Target   =>
            My_Pause_Motion_Planner.Enqueue_Flush_And_Reset_Position
              (Data           => Extra_Block_Resetting_Data_Holders.To_Holder (Extra_Data),
               Pos            => New_Position,
               Is_Homing_Move => Is_Homing_Move);
            Pause_Planner_State.Set_Last_Position (New_Position);
      end case;
   end Flush_And_Reset_Position;

   procedure Prompt_For_Update is
   begin
      null; --  TODO
   end Prompt_For_Update;

   procedure Setup_Planner_Runners
     (Params : Motion_Planner.Kinematic_Parameters; Map : My_Default_Modules_Children.Kinematics.Motor_Position_Map) is
   begin
      My_Motion_Planner.Runner.Setup (Params, Map);
      My_Pause_Motion_Planner.Runner.Setup (Params, Map);
      My_Step_Generator.Runner.Setup (My_Step_Generator.Motor_Pos_Map (Map));
   end Setup_Planner_Runners;

   procedure Catch_Up_Planner_State_Handlers (Executed_Corner_ID : Planner_Corner_ID) is
      Handlers : Module_Instance_Vectors.Vector;
   begin
      Planner_State_Handler_Instances.Snapshot (Handlers);

      for Instance of Handlers loop
         Planner_State_Handler'Class (Instance.Get.Element.all).Catch_Up_Planner_State (Executed_Corner_ID);
      end loop;
   end Catch_Up_Planner_State_Handlers;

   procedure Prepare_Config_For_Save_Handlers is
      Handlers : Module_Instance_Vectors.Vector;
   begin
      Config_Save_Preparer_Instances.Snapshot (Handlers);

      for Instance of Handlers loop
         Config_Save_Preparer'Class (Instance.Get.Element.all).Prepare_Config_For_Save;
      end loop;
   end Prepare_Config_For_Save_Handlers;

   procedure Handle_Cancellation_Handlers
     (Executed_Corner_ID : Planner_Corner_ID; Cancellation_Barrier_ID : Planner_Corner_ID; Current_Position : Position)
   is
      Handlers : Module_Instance_Vectors.Vector;
   begin
      Cancellation_Handler_Instances.Snapshot (Handlers);

      for Instance of Handlers loop
         Cancellation_Handler'Class (Instance.Get.Element.all).Handle_Cancel
           (Executed_Corner_ID, Cancellation_Barrier_ID, Current_Position);
      end loop;
   end Handle_Cancellation_Handlers;

   procedure Run is
      Active_Module_Instances : Module_Instance_Maps.Map := [];

      procedure Reset_Runtime_State;
      --  Reset runtime queues, planner state, and execution tracking for a fresh controller run.

      procedure Setup_Runtime_Pipeline;
      --  Configure the motion planner and step generator using the active kinematics module.

      procedure Attempt_Start (Started : out Boolean);
      --  Initialize configured modules and start them if no configuration errors are reported.

      procedure Clear_Active_Modules;
      --  Stop and release all active module instances in reverse startup order.

      procedure Process_Gcode_Queue;
      --  Process queued G-code items until the surrounding task is aborted.

      procedure Handle_Reload_Request (Modules_Started : Boolean);
      --  Reset runtime and configuration state after a reload request.

      procedure Handle_Fatal_Error (Modules_Started : Boolean);
      --  Wait for a fatal error, then tear runtime state down for shutdown.

      procedure Wait_For_Reload_Or_Fatal (Modules_Started : Boolean; Exit_Main : out Boolean);
      --  Wait for either a reload request or a fatal error.

      procedure Wait_For_Run_End (Started : Boolean; Exit_Main : out Boolean);
      --  Wait for the current controller run to end, starting the G-code processor when needed.

      procedure Reset_Runtime_State is
      begin
         if Pipeline_Is_Set_Up then
            My_Motion_Planner.Reset;
            My_Pause_Motion_Planner.Reset;
            My_Step_Generator.Pause;
            My_Step_Generator.Reset;
            Pipeline_Is_Set_Up := False;
         end if;

         Gcode_Processor_Is_Running := False;
         Primary_Planner_State.Reset;
         Pause_Planner_State.Reset;
         Pause_Default_State.Reset;
         Last_Command_Executed.Reset (Startup_Position);
         Idle_Notification_State.Reset;
         My_Gcode_Queue.Cancel_All;
      end Reset_Runtime_State;

      procedure Setup_Runtime_Pipeline is
         Kinematics_Instance_Ref    : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
           Active_Module_Instances ("Kinematics");
         Kinematics_Instance        : My_Default_Modules_Children.Kinematics.Module_Instance_Interface'Class renames
           My_Default_Modules_Children.Kinematics.Module_Instance_Interface'Class
             (Kinematics_Instance_Ref.Get.Element.all);
         Input_Shapers_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
           Active_Module_Instances ("Input Shapers");
         Input_Shapers_Instance     : My_Default_Modules_Children.Input_Shapers.Module_Instance_Interface'Class renames
           My_Default_Modules_Children.Input_Shapers.Module_Instance_Interface'Class
             (Input_Shapers_Instance_Ref.Get.Element.all);
         Startup_Configuration      : constant My_Default_Modules_Children.Kinematics.Motion_Planner_Configuration :=
           Kinematics_Instance.Get_Default_Motion_Planner_Configuration;
         Startup_Parameters         : constant Motion_Planner.Kinematic_Parameters :=
           (Startup_Configuration.Parameters
            with delta Axial_Shapers => Input_Shapers_Instance.Get_Current_Axial_Shapers);
      begin
         Current_Motor_Position_Map := Startup_Configuration.Motors_To_Position;
         Setup_Planner_Runners (Startup_Parameters, Current_Motor_Position_Map);

         Primary_Planner_State.Set_Last_Kinematic_Parameters (Startup_Parameters);
         Primary_Planner_State.Set_Last_Position (Startup_Position);
         Pause_Planner_State.Set_Last_Kinematic_Parameters (Startup_Parameters);
         Pause_Planner_State.Set_Last_Position (Startup_Position);
         Pause_Default_State.Set_Last_Kinematic_Parameters (Startup_Parameters);
         Pause_Default_State.Set_Last_Position (Startup_Position);
         Last_Command_Executed.Reset (Startup_Position);
         Reset_Position ([others => 0.0]);
         Pipeline_Is_Set_Up := True;
      end Setup_Runtime_Pipeline;

      procedure Attempt_Start (Started : out Boolean) is
         Had_Error : Boolean := False;

         procedure Report_Config_Error (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
         --  Log a startup configuration error and mark the current start attempt as failed.

         procedure Report_Config_Error (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String) is
         begin
            My_Logger.Log ("Startup error: " & Conversions.To_Virtual_String (Path'Image) & ": " & Message);
            Had_Error := True;
         end Report_Config_Error;

         Startup_Planner : constant Planner_Wrapper := (Startup_Mode => True, Target => Primary_Planner_Target);
      begin
         Active_Module_Instances :=
           Recursive_Module_Initialization
             (Report_Config_Error'Access, Active_Config_File, Log_Dependency_Tree => False);

         if Had_Error then
            My_Logger.Log ("Prunt could not start due to configuration errors.");
            Clear_Active_Modules;
            Started := False;
         else
            Setup_Runtime_Pipeline;
            Pause_Handler_Instances.Load
              (Module_Instance_Vectors.Vector'
                 [for Instance of Active_Module_Instances when Instance.Get.Element.all in Pause_Handler'Class =>
                    Instance]);
            Planner_State_Handler_Instances.Load
              (Module_Instance_Vectors.Vector'
                 [for Instance of
                      Active_Module_Instances
                      when Instance.Get.Element.all in Planner_State_Handler'Class =>
                    Instance]);
            Config_Save_Preparer_Instances.Load
              (Module_Instance_Vectors.Vector'
                 [for Instance of
                      Active_Module_Instances
                      when Instance.Get.Element.all in Config_Save_Preparer'Class =>
                    Instance]);
            Cancellation_Handler_Instances.Load
              (Module_Instance_Vectors.Vector'
                 [for Instance of
                      Active_Module_Instances
                      when Instance.Get.Element.all in Cancellation_Handler'Class =>
                    Instance]);
            Idle_Notification_Instances.Load
              (Module_Instance_Vectors.Vector'
                 [for Instance of
                      Active_Module_Instances
                      when Instance.Get.Element.all in Controller_Interfaces.Idle_Notification_Receiver'Class =>
                    Instance]);

            for M of Active_Module_Instances loop
               My_Modules.Module_Instance'Class (M.Get.Element.all).Start (M.Weak, Startup_Planner);
            end loop;

            Idle_Notification_State.Publish_Completion_When_Inactive (Last_Command_Executed.Get);

            Started := True;
         end if;
      exception
         when others =>
            Reset_Runtime_State;
            Clear_Active_Modules;
            raise;
      end Attempt_Start;

      procedure Clear_Active_Modules is
      begin
         Pause_Handler_Instances.Clear;
         Planner_State_Handler_Instances.Clear;
         Config_Save_Preparer_Instances.Clear;
         Cancellation_Handler_Instances.Clear;
         Idle_Notification_Instances.Clear;
         Active_Module_Instances.Reverse_Clear;
      end Clear_Active_Modules;

      procedure Process_Gcode_Queue is
         use type Gcode_Arguments.Argument_Kind;

         Active_Planner              : constant Planner_Wrapper :=
           (Startup_Mode => False, Target => Primary_Planner_Target);
         Gcode_Rejection_Retry_Delay : constant Duration := 0.1;

         function Line_Is_Empty (Args : Gcode_Arguments.Arguments) return Boolean;

         function Extract_Integer_Command_Identifier
           (Args : in out Gcode_Arguments.Arguments; Argument : Gcode_Identifier_Argument_Index)
            return Gcode_Command_Identifier;

         function Extract_Command_Identifier (Args : in out Gcode_Arguments.Arguments) return Gcode_Command_Identifier;

         procedure Dispatch_Gcode_Command (Args : in out Gcode_Arguments.Arguments);

         procedure Process_Gcode_Line (Line : Virtual_String);

         procedure Process_Next_Gcode_Item (Stopped : out Boolean);

         function Line_Is_Empty (Args : Gcode_Arguments.Arguments) return Boolean is
         begin
            for Index in Gcode_Arguments.Arguments_Index loop
               if Gcode_Arguments.Kind (Args, Index) /= Gcode_Arguments.Non_Existent_Kind then
                  return False;
               end if;
            end loop;

            return True;
         end Line_Is_Empty;

         function Extract_Integer_Command_Identifier
           (Args : in out Gcode_Arguments.Arguments; Argument : Gcode_Identifier_Argument_Index)
            return Gcode_Command_Identifier is
         begin
            if Gcode_Arguments.Kind (Args, Argument) /= Gcode_Arguments.Integer_Kind then
               if Argument = 'G' then
                  raise Gcode_Arguments.Parse_Error with "G-code G identifier must be an integer.";
               end if;

               raise Gcode_Arguments.Parse_Error with "G-code M identifier must be an integer.";
            end if;

            return (Argument => Argument, Number => Gcode_Arguments.Consume_Integer (Args, Argument));
         end Extract_Integer_Command_Identifier;

         function Extract_Command_Identifier (Args : in out Gcode_Arguments.Arguments) return Gcode_Command_Identifier
         is
            G_Present : constant Boolean := Gcode_Arguments.Kind (Args, 'G') /= Gcode_Arguments.Non_Existent_Kind;
            M_Present : constant Boolean := Gcode_Arguments.Kind (Args, 'M') /= Gcode_Arguments.Non_Existent_Kind;
         begin
            if G_Present = M_Present then
               raise Gcode_Arguments.Parse_Error
                 with "Each G-code line must contain exactly one G or M command identifier.";
            end if;

            if G_Present then
               return Extract_Integer_Command_Identifier (Args, 'G');
            end if;

            return Extract_Integer_Command_Identifier (Args, 'M');
         end Extract_Command_Identifier;

         procedure Dispatch_Gcode_Command (Args : in out Gcode_Arguments.Arguments) is
            Command_Identifier : constant Gcode_Command_Identifier := Extract_Command_Identifier (Args);
            Module_Name        : constant Virtual_String :=
              Find_Module_Name (Active_Module_Gcode_Dispatch_Map, Command_Identifier, Args);
         begin
            if Module_Name.Is_Empty then
               raise Constraint_Error with "No active module can handle the requested G-code command.";
            end if;

            My_Modules.Module_Instance'Class (Active_Module_Instances (Module_Name).Get.Element.all).Gcode_Dispatch
              (Self_Ref           => Active_Module_Instances (Module_Name),
               Args               => Args,
               Planner            => Active_Planner,
               Command_Identifier => Command_Identifier);
         end Dispatch_Gcode_Command;

         procedure Process_Gcode_Line (Line : Virtual_String) is
            Args : Gcode_Arguments.Arguments := Gcode_Arguments.Parse_Arguments (Line);
         begin
            if Line_Is_Empty (Args) then
               return;
            end if;

            Dispatch_Gcode_Command (Args);

            Gcode_Arguments.Validate_All_Consumed (Args);
         end Process_Gcode_Line;

         procedure Process_Next_Gcode_Item (Stopped : out Boolean) is
            use type Gcode_Queues.Queue_Item_Kind;

            Line                            : Virtual_String;
            Item_Kind                       : Gcode_Queues.Queue_Item_Kind;
            End_Of_Item                     : Boolean;
            Queue_Stopped                   : Boolean;
            Initial_Cancellation_Generation : Cancellation_Generation_Type;
         begin
            My_Gcode_Queue.Get_Next_Line (Line, Item_Kind, End_Of_Item, Queue_Stopped);

            if Queue_Stopped then
               Stopped := True;
               return;
            end if;

            Initial_Cancellation_Generation := Gcode_Cancellation_Barrier.Cancellation_Generation;

            Retry_Line : loop
               select
                  Gcode_Cancellation_Barrier.Start_Line;
               else
                  Gcode_Cancellation_Barrier.Wait_Until_Not_Cancelling;
                  Stopped := False;
                  return;
               end select;

               begin
                  if Gcode_Cancellation_Barrier.Cancellation_Generation /= Initial_Cancellation_Generation then
                     Gcode_Cancellation_Barrier.Finish_Line;
                     Stopped := False;
                     return;
                  end if;

                  Process_Gcode_Line (Line);

                  if End_Of_Item then
                     Active_Planner.Flush;
                  end if;

                  Gcode_Cancellation_Barrier.Finish_Line;
                  exit Retry_Line;
               exception
                  when Gcode_Temporarily_Rejected_Error =>
                     --  The line was not accepted. Force already-accepted work toward execution, then retry after
                     --  leaving the cancellation barrier so cancellation can interrupt the polling delay.
                     Active_Planner.Flush;
                     Gcode_Cancellation_Barrier.Finish_Line;
                     delay Gcode_Rejection_Retry_Delay;

                  when E : Gcode_Bad_Inputs_Error =>
                     My_Logger.Log
                       (Conversions.To_Virtual_String ("Rejected G-code line: ")
                        & Line
                        & Conversions.To_Virtual_String (" (")
                        & Conversions.To_Virtual_String (Ada.Exceptions.Exception_Message (E))
                        & Conversions.To_Virtual_String (")"));
                     if Item_Kind = Gcode_Queues.File_Item then
                        My_Gcode_Queue.Cancel_File;
                        Active_Planner.Flush;
                     end if;
                     Gcode_Cancellation_Barrier.Finish_Line;
                     exit Retry_Line;

                  when others =>
                     Gcode_Cancellation_Barrier.Finish_Line;
                     raise;
               end;
            end loop Retry_Line;

            Stopped := False;
         end Process_Next_Gcode_Item;
      begin
         loop
            declare
               Stopped : Boolean;
            begin
               Process_Next_Gcode_Item (Stopped);
               exit when Stopped;
            end;
         end loop;
      end Process_Gcode_Queue;

      procedure Handle_Reload_Request (Modules_Started : Boolean) is
      begin
         My_Logger.Log ("Reload requested. Resetting...");

         if Modules_Started then
            My_Gcode_Queue.Stop_Waiting;
         end if;

         Reset_Runtime_State;

         if Modules_Started then
            Clear_Active_Modules;
         end if;

         Exception_Occurrence_Holder.Reset;
         Active_Config_File.Reset_Live_To_Stored;
         Reset_Hardware;
         My_Web_Server.Reset;
      end Handle_Reload_Request;

      procedure Handle_Fatal_Error (Modules_Started : Boolean) is
      begin
         Exception_Occurrence_Holder.Enter_When_Fatal_Set;

         if Modules_Started then
            My_Gcode_Queue.Stop_Waiting;
         end if;

         Reset_Runtime_State;

         if Modules_Started then
            Clear_Active_Modules;
         end if;
      end Handle_Fatal_Error;

      procedure Wait_For_Reload_Or_Fatal (Modules_Started : Boolean; Exit_Main : out Boolean) is
      begin
         select
            Reload_Signal.Wait;
            Handle_Reload_Request (Modules_Started);
            Exit_Main := False;
         then abort
            Handle_Fatal_Error (Modules_Started);
            Exit_Main := True;
         end select;
      end Wait_For_Reload_Or_Fatal;

      procedure Wait_For_Run_End (Started : Boolean; Exit_Main : out Boolean) is
      begin
         if Started then
            declare
               protected Catch_Up_Stop is
                  procedure Stop;
                  function Stopped return Boolean;
                  entry Wait_Until_Stopped;
               private
                  Stop_Requested : Boolean := False;
               end Catch_Up_Stop;

               protected body Catch_Up_Stop is
                  procedure Stop is
                  begin
                     Stop_Requested := True;
                  end Stop;

                  function Stopped return Boolean is
                  begin
                     return Stop_Requested;
                  end Stopped;

                  entry Wait_Until_Stopped when Stop_Requested is
                  begin
                     null;
                  end Wait_Until_Stopped;
               end Catch_Up_Stop;

               task Gcode_Processor;
               task Idle_Notification_Worker;
               task Planner_State_Catch_Up;

               task body Gcode_Processor is
               begin
                  Gcode_Processor_Is_Running := True;
                  Process_Gcode_Queue;
                  Gcode_Processor_Is_Running := False;
               exception
                  when E : others =>
                     Gcode_Processor_Is_Running := False;
                     Exception_Occurrence_Holder.all.Set_Fatal
                       (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, E);
               end Gcode_Processor;

               task body Idle_Notification_Worker is
                  Completion : Idle_Activity_Completion;
               begin
                  loop
                     select
                        Catch_Up_Stop.Wait_Until_Stopped;
                        exit;
                     then abort
                        Idle_Notification_State.Wait_For_Completion (Completion);
                     end select;

                     select
                        Catch_Up_Stop.Wait_Until_Stopped;
                        exit;
                     then abort
                        Wait_Until_Idle (Completion.Last_Command_Index);
                     end select;

                     Notify_Idle_Start (Completion);
                  end loop;
               exception
                  when E : others =>
                     Exception_Occurrence_Holder.all.Set_Fatal
                       (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, E);
               end Idle_Notification_Worker;

               task body Planner_State_Catch_Up is
               begin
                  loop
                     exit when Catch_Up_Stop.Stopped;
                     Catch_Up_Planner_State_Handlers (My_Step_Generator.Get_Last_Executed_Primary_Corner_ID);
                     delay 0.1;
                  end loop;
               exception
                  when E : others =>
                     Exception_Occurrence_Holder.all.Set_Fatal
                       (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, E);
               end Planner_State_Catch_Up;
            begin
               Wait_For_Reload_Or_Fatal (Modules_Started => True, Exit_Main => Exit_Main);
               Catch_Up_Stop.Stop;
            end;
         else
            Wait_For_Reload_Or_Fatal (Modules_Started => False, Exit_Main => Exit_Main);
         end if;
      end Wait_For_Run_End;

   begin
      Reset_Runtime_State;
      Reload_Signal.Mark_Startup_Done;

      Main : loop
         declare
            Started   : Boolean;
            Exit_Main : Boolean;
         begin
            Attempt_Start (Started);
            Wait_For_Run_End (Started, Exit_Main);
            exit Main when Exit_Main;
         end;
      end loop Main;
   end Run;

   procedure Report_External_Error (Message : String; Is_Fatal : Boolean := True) is
   begin
      begin
         raise Program_Error with Message;
      exception
         when E : others =>
            Report_External_Error (E, Is_Fatal);
      end;
   end Report_External_Error;

   procedure Report_External_Error (Occurrence : Ada.Exceptions.Exception_Occurrence; Is_Fatal : Boolean := True) is
      use type Ada.Task_Termination.Cause_Of_Termination;
   begin
      if Is_Fatal then
         Exception_Occurrence_Holder.all.Set_Fatal
           (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Occurrence);
      else
         Exception_Occurrence_Holder.all.Set_Recoverable
           (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Occurrence);
      end if;
   end Report_External_Error;

   procedure Request_Machine_Idle_Timeout_Shutdown (Message : String) is
   begin
      begin
         raise Machine_Idle_Timeout_Error with Message;
      exception
         when E : Machine_Idle_Timeout_Error =>
            Exception_Occurrence_Holder.all.Set_Fatal
              (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, E);
      end;
   end Request_Machine_Idle_Timeout_Shutdown;

   function Last_Error_Message return String is
      Occurrence : Ada.Exceptions.Exception_Occurrence;
      Is_Fatal   : Boolean;
      pragma Unreferenced (Is_Fatal);
   begin
      if not Exception_Occurrence_Holder.all.Is_Set then
         return "";
      end if;

      Exception_Occurrence_Holder.all.Get (Occurrence, Is_Fatal);
      return Ada.Exceptions.Exception_Information (Occurrence);
   end Last_Error_Message;

   procedure Log (Message : String) is
   begin
      My_Logger.Log (+Message);
   end Log;

   function Recursive_Module_Initialization
     (Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      My_Config_File      : Config.Config_File;
      Log_Dependency_Tree : Boolean := False) return Module_Instance_Maps.Map
   is
      use Module_Maps;
      use Module_Instance_Maps;
      use type Ada.Tags.Tag;
      use type My_Modules.Module_Instance_Shared_Pointers.Ref;

      type Dependency_Request is record
         Requester     : Virtual_String;
         Requested_Tag : Ada.Tags.Tag;
      end record;

      package Dependency_Request_Vectors is new Ada.Containers.Vectors (Positive, Dependency_Request);
      package String_Sets is new Ada.Containers.Ordered_Sets (Virtual_String);
      package String_Vectors is new Ada.Containers.Vectors (Positive, Virtual_String);

      Result               : Module_Instance_Maps.Map := [];
      Initializing         : String_Sets.Set := [];
      Initialization_Stack : String_Vectors.Vector := [];
      Dependency_Requests  : Dependency_Request_Vectors.Vector := [];

      function Find_Existing_Instance (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref;
      --  Return the initialized module instance with Tag, or Null_Ref if none has been initialized.

      function Find_Existing_Instance (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref is
      begin
         for I of Result loop
            if I.Get.Element'Tag = Tag then
               return I;
            end if;
         end loop;

         return My_Modules.Module_Instance_Shared_Pointers.Null_Ref;
      end Find_Existing_Instance;

      function Find_Module_By_Tag (Tag : Ada.Tags.Tag) return Virtual_String;
      --  Return the configured module name for an initialized instance tag.

      function Find_Module_By_Tag (Tag : Ada.Tags.Tag) return Virtual_String is
      begin
         for I in Result.Iterate loop
            if Element (I).Get.Element'Tag = Tag then
               return Key (I);
            end if;
         end loop;

         raise Constraint_Error;
      end Find_Module_By_Tag;

      procedure Record_Dependency_Request (Requester : Virtual_String; Requested_Tag : Ada.Tags.Tag);
      --  Record a dependency request unless the same requester and requested tag have already been recorded.

      procedure Record_Dependency_Request (Requester : Virtual_String; Requested_Tag : Ada.Tags.Tag) is
      begin
         for Request of Dependency_Requests loop
            if Request.Requester = Requester and then Request.Requested_Tag = Requested_Tag then
               return;
            end if;
         end loop;

         Dependency_Requests.Append (Dependency_Request'(Requester => Requester, Requested_Tag => Requested_Tag));
      end Record_Dependency_Request;

      function Direct_Dependencies (Module_Name : Virtual_String) return String_Vectors.Vector;
      --  Return the unique module names directly requested by Module_Name during initialization.

      function Direct_Dependencies (Module_Name : Virtual_String) return String_Vectors.Vector is
         Dependencies : String_Vectors.Vector := [];
         Seen         : String_Sets.Set := [];
      begin
         for Request of Dependency_Requests loop
            if Request.Requester = Module_Name then
               declare
                  Dependency_Name : constant Virtual_String := Find_Module_By_Tag (Request.Requested_Tag);
               begin
                  if not Seen.Contains (Dependency_Name) then
                     Dependencies.Append (Dependency_Name);
                     Seen.Insert (Dependency_Name);
                  end if;
               end;
            end if;
         end loop;

         return Dependencies;
      end Direct_Dependencies;

      function Has_Incoming_Dependency (Module_Name : Virtual_String) return Boolean;
      --  Return True when another active module directly depends on Module_Name.

      function Has_Incoming_Dependency (Module_Name : Virtual_String) return Boolean is
      begin
         for C in Active_Modules.Iterate loop
            for Dependency_Name of Direct_Dependencies (Key (C)) loop
               if Dependency_Name = Module_Name then
                  return True;
               end if;
            end loop;
         end loop;

         return False;
      end Has_Incoming_Dependency;

      procedure Log_Module_Dependency_Tree;
      --  Log the module dependency tree using the dependency requests recorded during initialization.

      procedure Log_Module_Dependency_Tree is
         Expanded : Config.Discrete_String_Sets.Set := [];
         Path     : Config.Discrete_String_Sets.Set := [];

         procedure Log_Subtree (Module_Name : Virtual_String; Prefix : Virtual_String);

         procedure Log_Subtree (Module_Name : Virtual_String; Prefix : Virtual_String) is
         begin
            if Path.Contains (Module_Name) then
               My_Logger.Log (Prefix & Module_Name & " (cycle)");
               return;
            end if;

            if Expanded.Contains (Module_Name) then
               My_Logger.Log (Prefix & Module_Name & " (already shown)");
               return;
            end if;

            My_Logger.Log (Prefix & Module_Name);
            Expanded.Insert (Module_Name);
            Path.Insert (Module_Name);

            for Dependency_Name of Direct_Dependencies (Module_Name) loop
               Log_Subtree (Dependency_Name, "  " & Prefix);
            end loop;

            Path.Delete (Module_Name);
         end Log_Subtree;
      begin
         My_Logger.Log ("Module dependency tree:");

         for C in Active_Modules.Iterate loop
            if not Has_Incoming_Dependency (Key (C)) then
               Log_Subtree (Key (C), "- ");
            end if;
         end loop;

         for C in Active_Modules.Iterate loop
            if not Expanded.Contains (Key (C)) then
               Log_Subtree (Key (C), "- ");
            end if;
         end loop;
      end Log_Module_Dependency_Tree;

      function Recurse return Natural;
      --  Initialize all currently unblocked modules and return the number initialized in this pass.

      function Recurse return Natural is
         function Get_Other_Instance (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref;
         --  Resolve a requested module dependency, recursively initializing modules if needed.

         function Get_Other_Instance (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref is
            Requester : constant Virtual_String := Initialization_Stack.Last_Element;
            Existing  : My_Modules.Module_Instance_Shared_Pointers.Ref :=
              My_Modules.Module_Instance_Shared_Pointers.Null_Ref;
         begin
            Record_Dependency_Request (Requester, Tag);

            loop
               Existing := Find_Existing_Instance (Tag);
               exit when Existing /= My_Modules.Module_Instance_Shared_Pointers.Null_Ref;
               exit when Recurse = 0;
            end loop;

            if Existing = My_Modules.Module_Instance_Shared_Pointers.Null_Ref then
               My_Logger.Log
                 ("Module dependency could not be resolved: "
                  & (+Ada.Tags.Expanded_Name (Tag))
                  & " requested by "
                  & Requester
                  & ". Attempted initialization chain: "
                  & (+Initialization_Stack'Image)
                  & ". If this tag belongs to one of these modules then there is a dependency loop, otherwise no "
                  & "module with the requested tag is in the initialization set.");

               raise Program_Error with "Module dependency resolution error, refer to log.";
            end if;

            return Existing;
         end Get_Other_Instance;

         Modules_Initialized : Natural := 0;
      begin
         for C in Active_Modules.Iterate loop
            if not Result.Contains (Key (C)) and then not Initializing.Contains (Key (C)) then
               declare
                  Module_Name : constant Virtual_String := Key (C);

                  function Get_Data return My_Modules.Module_Instance_Parent'Class
                  with Post => Get_Data'Result in My_Modules.Module_Instance'Class;
                  --  Initialize the module selected by the current cursor and return its instance data.

                  procedure Report_Config_Error_With_Module
                    (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
                  --  Prefix a configuration error path with the module location before reporting it.

                  procedure Report_Config_Error_With_Module
                    (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String)
                  is
                     use type Config.Config_Data_Paths.Vector;
                  begin
                     Report_Config_Error (["Config", Module_Name, "Config"] & Path, Message);
                  end Report_Config_Error_With_Module;

                  function Get_Data return My_Modules.Module_Instance_Parent'Class is
                     Config_Data : constant Config.Config_Data := My_Config_File.Get_Data (Module_Name);
                  begin
                     return
                       Element (C).Initialize
                         (Config_Data,
                          Report_Config_Error_With_Module'Access,
                          Status_Manager.Get_Emitter (My_Status_Data, Module_Name),
                          Get_Other_Instance'Access);
                  end Get_Data;

                  Ref : My_Modules.Module_Instance_Shared_Pointers.Ref :=
                    My_Modules.Module_Instance_Shared_Pointers.Null_Ref;
               begin
                  Initializing.Insert (Module_Name);
                  Initialization_Stack.Append (Module_Name);

                  begin
                     Ref.Set (Get_Data'Access);
                  exception
                     when others =>
                        Initialization_Stack.Delete_Last;
                        Initializing.Delete (Module_Name);
                        raise;
                  end;

                  Initialization_Stack.Delete_Last;
                  Initializing.Delete (Module_Name);

                  Result.Insert (Module_Name, Ref);

                  for Other in Result.Iterate loop
                     if Key (Other) /= Module_Name
                       and then Element (Other).Get.Element'Tag = Result (Module_Name).Get.Element'Tag
                     then
                        raise Program_Error
                          with "Duplicate module tag: " & Module_Name'Image & " and " & Key (Other)'Image;
                     end if;
                  end loop;

                  Modules_Initialized := @ + 1;
               end;
            end if;
         end loop;

         return Modules_Initialized;
      end Recurse;

      Ignored : Natural := Recurse;
   begin
      if Log_Dependency_Tree then
         Log_Module_Dependency_Tree;
      end if;

      return Result;
   end Recursive_Module_Initialization;

   protected body Patch_Processor is
      procedure Apply
        (Patch : Virtual_String; Result : out Virtual_String; Errors : out Config.Config_Error_Vectors.Vector)
      is
         use type Config.Save_Counter;

         procedure Report_Config_Error (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);

         procedure Report_Config_Error (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String) is
         begin
            Errors.Append (Config.Config_Error'(Path, Message));
         end Report_Config_Error;
      begin
         if Has_Cache and then Patch.Is_Empty and then Cached_Save_Counter = Active_Config_File.Last_Save then
            Result := Cached_Result;
            Errors := Cached_Errors;
         else
            Active_Config_File.Apply_Untrusted_Patch (Patch, Result, Errors);
            --  We apply the patch to the active config file so it will be used by the next reset and won't be
            --  overwritten by modules but have the temporary modules use a new config file since we want them to
            --  check the new values.
            if Errors.Is_Empty then
               declare
                  --  There is no point trying to load the modules if there are errors when testing against the schema
                  --  as no patch is applied in that case.
                  My_Config_File             : constant Config.Config_File :=
                    Config.Create (Config_Path, Active_Module_Config_Schemas, Config_Overrides);
                  Temporary_Module_Instances : Module_Instance_Maps.Map :=
                    Recursive_Module_Initialization (Report_Config_Error'Access, My_Config_File);
               begin
                  Temporary_Module_Instances.Reverse_Clear;
               end;
            end if;

            Cached_Result := Result;
            Cached_Errors := Errors;
            Cached_Save_Counter := Active_Config_File.Last_Save;
            Has_Cache := True;
         end if;
      end Apply;
   end Patch_Processor;

   procedure Apply_Untrusted_Config_Patch
     (Patch : Virtual_String; Result : out Virtual_String; Errors : out Config.Config_Error_Vectors.Vector) is
   begin
      Patch_Processor.Apply (Patch, Result, Errors);
   end Apply_Untrusted_Config_Patch;

   function Get_Config_Schema_String return Virtual_String is
   begin
      return Active_Config_File.Get_Schema_String;
   end Get_Config_Schema_String;

   procedure Reset_Live_Config_To_Stored is
   begin
      Active_Config_File.Reset_Live_To_Stored;
   end Reset_Live_Config_To_Stored;

   procedure Submit_Gcode_Command (Command : Virtual_String; Succeeded : out Boolean) is
   begin
      select
         Gcode_Cancellation_Barrier.Start_Submission;
      else
         Succeeded := False;
         return;
      end select;

      begin
         My_Gcode_Queue.Try_Set_Command (Command, Succeeded);
         Gcode_Cancellation_Barrier.Finish_Submission;
      exception
         when others =>
            Gcode_Cancellation_Barrier.Finish_Submission;
            raise;
      end;
   end Submit_Gcode_Command;

   procedure Submit_Gcode_File (Path : Virtual_String; Succeeded : out Boolean) is
   begin
      select
         Gcode_Cancellation_Barrier.Start_Submission;
      else
         Succeeded := False;
         return;
      end select;

      begin
         My_Gcode_Queue.Try_Set_File (Path, Succeeded);
         Gcode_Cancellation_Barrier.Finish_Submission;
      exception
         when others =>
            Gcode_Cancellation_Barrier.Finish_Submission;
            raise;
      end;
   end Submit_Gcode_File;

   procedure Cancel_Gcode (Succeeded : out Boolean) is
      Executed_Corner_ID      : Planner_Corner_ID;
      Cancellation_Barrier_ID : Planner_Corner_ID;
      Current_Position        : Position;
      Params                  : Motion_Planner.Kinematic_Parameters;
   begin
      if not Pipeline_Is_Set_Up then
         Succeeded := False;
         return;
      end if;

      select
         Gcode_Cancellation_Barrier.Start_Cancellation;
      else
         Succeeded := False;
         return;
      end select;

      --  TODO: We need protection against a reload happening here.

      while not My_Step_Generator.Is_Paused loop
         My_Step_Generator.Pause;
         delay 0.1;
      end loop;

      --  TODO: We need to stop the user from being able to unpause the step generator while we're trying to cancel.

      begin
         Gcode_Cancellation_Barrier.Wait_Until_Not_Submitting;
         My_Gcode_Queue.Cancel_All;
         Gcode_Cancellation_Barrier.Wait_Until_Not_Processing;

         Executed_Corner_ID := My_Step_Generator.Get_Last_Executed_Primary_Corner_ID;
         Cancellation_Barrier_ID := My_Motion_Planner.Get_Last_Assigned_Corner_ID;
         Current_Position := Last_Command_Executed.Get_Current_Position;
         Handle_Cancellation_Handlers (Executed_Corner_ID, Cancellation_Barrier_ID, Current_Position);

         Params := Primary_Planner_State.Get_Last_Kinematic_Parameters;

         My_Step_Generator.Reset;
         My_Motion_Planner.Reset;
         My_Pause_Motion_Planner.Reset;

         Last_Command_Executed.Reset (Current_Position);
         Idle_Notification_State.Abandon_Activities (Last_Command_Executed.Get);

         Setup_Planner_Runners (Params, Current_Motor_Position_Map);

         Primary_Planner_State.Set_Last_Position (Current_Position);
         Pause_Planner_State.Set_Last_Position (Current_Position);
         Pause_Default_State.Set_Last_Position (Current_Position);
         Reset_Position
           (My_Step_Generator.To_Motor_Position
              (Current_Position, My_Step_Generator.Motor_Pos_Map (Current_Motor_Position_Map)));

         Succeeded := True;
         Gcode_Cancellation_Barrier.Finish_Cancellation;
      exception
         when others =>
            Gcode_Cancellation_Barrier.Finish_Cancellation;
            raise;
      end;
   end Cancel_Gcode;

   procedure Pause_Stepgen is
   begin
      My_Step_Generator.Pause;
   end Pause_Stepgen;

   procedure Resume_Stepgen is
   begin
      My_Step_Generator.Resume;
   end Resume_Stepgen;

   function Ready_For_Gcode return Boolean is
   begin
      return Gcode_Processor_Is_Running;
   end Ready_For_Gcode;

   procedure Reload_Server is
   begin
      null; --  TODO
   end Reload_Server;

   overriding
   procedure Process_After_Block (This : Pause_Plan_End_Event; Context : Block_End_Context'Class) is
   begin
      pragma Unreferenced (This, Context);
      null;
   end Process_After_Block;

   overriding
   function Get_Pause_Position (This : Pause_Context_Data) return Position is
   begin
      return This.Pause_Position;
   end Get_Pause_Position;

   overriding
   function Get_Last_Command_Index (This : Pause_Context_Data) return Command_Index is
   begin
      return This.Last_Command_Index;
   end Get_Last_Command_Index;

   procedure Handle_Pause (Pause_Position : Position; Last_Command_Index : Command_Index) is
      Pause_Planner : constant Planner_Wrapper := (Startup_Mode => False, Target => Pause_Planner_Target);
      Context       : constant Pause_Context_Data :=
        (Pause_Position => Pause_Position, Last_Command_Index => Last_Command_Index);
      Params        : constant Motion_Planner.Kinematic_Parameters :=
        Pause_Default_State.Get_Last_Kinematic_Parameters;
      Handlers      : Module_Instance_Vectors.Vector;
   begin
      Pause_Planner.Flush_And_Change_Kinematic_Parameters (Params);
      Pause_Planner.Flush_And_Reset_Position (Pause_Position);

      Pause_Handler_Instances.Snapshot (Handlers);

      for Instance of Handlers loop
         Pause_Handler'Class (Instance.Get.Element.all).Handle_Pause (Pause_Planner, Context);
      end loop;

      Pause_Planner.Flush (Pause_Plan_End_Event'(null record));
   end Handle_Pause;

   procedure Handle_Resume (Pause_Position : Position; Last_Command_Index : Command_Index) is
      Pause_Planner : constant Planner_Wrapper := (Startup_Mode => False, Target => Pause_Planner_Target);
      Context       : constant Pause_Context_Data :=
        (Pause_Position => Pause_Position, Last_Command_Index => Last_Command_Index);
      Handlers      : Module_Instance_Vectors.Vector;
   begin
      Pause_Handler_Instances.Snapshot (Handlers);

      for C in reverse Handlers.Iterate loop
         declare
            Instance : constant My_Modules.Module_Instance_Shared_Pointers.Ref := Module_Instance_Vectors.Element (C);
         begin
            Pause_Handler'Class (Instance.Get.Element.all).Handle_Resume (Pause_Planner, Context);
         end;
      end loop;

      Pause_Planner.Flush (Pause_Plan_End_Event'(null record));
   end Handle_Resume;

   function Is_Pause_Plan_Done (Resetting_Data : Extra_Block_Resetting_Data_Holders.Holder) return Boolean is
   begin
      return not Resetting_Data.Is_Empty and then Resetting_Data.Element in Pause_Plan_End_Event;
   end Is_Pause_Plan_Done;

   procedure Start_Planner_Block
     (Resetting_Data : Extra_Block_Resetting_Data_Holders.Holder; Last_Command_Index : Command_Index)
   is
      pragma Unreferenced (Resetting_Data, Last_Command_Index);
   begin
      Notify_Activity_Start;
   end Start_Planner_Block;

   procedure Enqueue_Command_Internal
     (Pos             : Position;
      Motor_Pos       : Motor_Position;
      Index           : Command_Index;
      Loop_Until_Hit  : Boolean;
      Safe_Stop_After : Boolean;
      Vel_Ratio       : Dimensionless)
   is
      pragma Unreferenced (Vel_Ratio);
      --  TODO: Vel_Ratio is for the laser module so we can modulate the output power based of the actual speed over
      --  the target speed.
   begin
      Last_Command_Executed.Record_Queued_Position (Index, Pos);
      Enqueue_Command
        ((Index => Index, Pos => Motor_Pos, Safe_Stop_After => Safe_Stop_After, Loop_Until_Hit => Loop_Until_Hit));
   end Enqueue_Command_Internal;

   procedure Start_Corner (Last_Command_Index : Command_Index; Data : Module_Types.Extra_Corner_Data'Class) is
   begin
      Data.Process (Last_Command_Index);
   end Start_Corner;

   procedure Finish_Planner_Block
     (Resetting_Data       : Extra_Block_Resetting_Data_Holders.Holder;
      Next_Block_Pos       : Motor_Position;
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index;
      Loop_Move_Offset     : Position_Offset)
   is
      Context : constant Planner_Block_End_Context :=
        (First_Accel_Distance     => First_Accel_Distance,
         Last_Command_Index       => Last_Command_Index,
         Loop_Move_Offset         => Loop_Move_Offset,
         State_Catch_Up_Corner_ID => My_Step_Generator.Get_Last_Executed_Primary_Corner_ID);
   begin
      if not Resetting_Data.Is_Empty then
         Resetting_Data.Element.Process_After_Block (Context);
      end if;

      Reset_Position (Next_Block_Pos);
      Idle_Notification_State.Complete_Activity (Last_Command_Index);
   end Finish_Planner_Block;

   procedure Report_Last_Command_Executed (Index : Command_Index) is
   begin
      Last_Command_Executed.Report (Index);
   end Report_Last_Command_Executed;

   procedure Report_Loop_Move_Cycles (Index : Command_Index; Cycles : Dimensionless) is
   begin
      Loop_Move_Cycles.Report (Index, Cycles);
   end Report_Loop_Move_Cycles;

   package body Last_Command_Executed is
      function Next_Slot (Slot : Pending_Position_Ring_Index) return Pending_Position_Ring_Index is
      begin
         if Slot = Pending_Position_Ring_Index'Last then
            return Pending_Position_Ring_Index'First;
         else
            return Slot + 1;
         end if;
      end Next_Slot;

      function Previous_Slot (Slot : Pending_Position_Ring_Index) return Pending_Position_Ring_Index is
      begin
         if Slot = Pending_Position_Ring_Index'First then
            return Pending_Position_Ring_Index'Last;
         else
            return Slot - 1;
         end if;
      end Previous_Slot;

      function Advance_Slot (Slot : Pending_Position_Ring_Index; Count : Natural) return Pending_Position_Ring_Index is
         First_Pos : constant Natural := Pending_Position_Ring_Index'Pos (Pending_Position_Ring_Index'First);
         Span      : constant Natural :=
           Pending_Position_Ring_Index'Pos (Pending_Position_Ring_Index'Last) - First_Pos + 1;
      begin
         return
           Pending_Position_Ring_Index'Val
             (First_Pos + (Pending_Position_Ring_Index'Pos (Slot) - First_Pos + Count) mod Span);
      end Advance_Slot;

      function Slot_Distance (From_Slot, To_Slot : Pending_Position_Ring_Index) return Natural is
         First_Pos : constant Natural := Pending_Position_Ring_Index'Pos (Pending_Position_Ring_Index'First);
         Span      : constant Natural :=
           Pending_Position_Ring_Index'Pos (Pending_Position_Ring_Index'Last) - First_Pos + 1;
         From_Pos  : constant Natural := Pending_Position_Ring_Index'Pos (From_Slot) - First_Pos;
         To_Pos    : constant Natural := Pending_Position_Ring_Index'Pos (To_Slot) - First_Pos;
      begin
         if To_Pos >= From_Pos then
            return To_Pos - From_Pos;
         else
            return Span - (From_Pos - To_Pos);
         end if;
      end Slot_Distance;

      procedure Write_Position (Target : out Atomic_Position_Array; Pos : Position) is
      begin
         for A in Axis_Name loop
            Target (A) := Atomic_Length (Pos (A));
         end loop;
      end Write_Position;

      function Read_Position (Source : Atomic_Position_Array) return Position is
      begin
         return Pos : Position do
            for A in Axis_Name loop
               Pos (A) := Length (Source (A));
            end loop;
         end return;
      end Read_Position;

      procedure Reset (Pos : Position) is
      begin
         if Executed_Command_Position_Ring_Capacity < 2 then
            raise Constraint_Error with "Executed_Command_Position_Ring_Capacity must be at least 2.";
         end if;

         Pending_Position_Write_Slot := Pending_Position_Ring_Index'First;
         Pending_Position_Read_Slot := Pending_Position_Ring_Index'First;

         Last_Command_Executed_Index := 0;
         Write_Position (Current_Position_Data, Pos);
      end Reset;

      procedure Record_Queued_Position (Index : Command_Index; Pos : Position) is
         Write_Slot      : constant Pending_Position_Ring_Index := Pending_Position_Write_Slot;
         Next_Write_Slot : constant Pending_Position_Ring_Index := Next_Slot (Write_Slot);
         Slot            : Pending_Position_Slot renames Pending_Positions (Write_Slot);
      begin
         while Next_Write_Slot = Pending_Position_Read_Slot loop
            --  We can delay here since the implementation should have a very full queue at this point.
            delay 0.01;
         end loop;

         Slot.Index := Atomic_Command_Index (Index);
         Write_Position (Slot.Pos, Pos);
         Pending_Position_Write_Slot := Next_Write_Slot;
      end Record_Queued_Position;

      procedure Report (Index : Command_Index) is
         Current_Report_Index : constant Command_Index := Command_Index (Last_Command_Executed_Index);
         Write_Slot_Snapshot  : constant Pending_Position_Ring_Index := Pending_Position_Write_Slot;
      begin
         if Index < Current_Report_Index then
            raise Constraint_Error with "Executed command index moved backwards.";
         end if;

         if Index = Current_Report_Index then
            return;
         end if;

         if Pending_Position_Read_Slot /= Write_Slot_Snapshot then
            --  Perform an optimistic jump to the expected position of the target indext and then move backwards or
            --  forwards for there as required.
            declare
               Initial_Read_Slot       : constant Pending_Position_Ring_Index := Pending_Position_Read_Slot;
               Pending_Count           : constant Natural := Slot_Distance (Initial_Read_Slot, Write_Slot_Snapshot);
               Expected_Consumed_Count : constant Natural :=
                 (if Index - Current_Report_Index >= Command_Index (Pending_Count)
                  then Pending_Count
                  else Natural (Index - Current_Report_Index));
               Search_Read_Slot        : Pending_Position_Ring_Index :=
                 Advance_Slot (Initial_Read_Slot, Expected_Consumed_Count);
            begin
               while Search_Read_Slot /= Initial_Read_Slot
                 and then Command_Index (Pending_Positions (Previous_Slot (Search_Read_Slot)).Index) > Index
               loop
                  Search_Read_Slot := Previous_Slot (Search_Read_Slot);
               end loop;

               while Search_Read_Slot /= Write_Slot_Snapshot
                 and then Command_Index (Pending_Positions (Search_Read_Slot).Index) <= Index
               loop
                  Search_Read_Slot := Next_Slot (Search_Read_Slot);
               end loop;

               if Search_Read_Slot /= Initial_Read_Slot then
                  Current_Position_Data := Pending_Positions (Previous_Slot (Search_Read_Slot)).Pos;
               end if;

               Pending_Position_Read_Slot := Search_Read_Slot;
            end;
         end if;

         Last_Command_Executed_Index := Atomic_Command_Index (Index);
      end Report;

      function Get return Command_Index is
      begin
         return Command_Index (Last_Command_Executed_Index);
      end Get;

      function Is_Idle return Boolean is
      begin
         return Pending_Position_Read_Slot = Pending_Position_Write_Slot;
      end Is_Idle;

      function Get_Current_Position return Position is
      begin
         return Read_Position (Current_Position_Data);
      end Get_Current_Position;
   end Last_Command_Executed;

   protected body Loop_Move_Cycles is
      procedure Report (Index : Command_Index; Cycles : Dimensionless) is
      begin
         if Has_Data then
            raise Constraint_Error with "Attempted to report loop move cycles while result is already pending.";
         end if;

         Current_Index := Index;
         Current_Cycles := Cycles;
         Has_Data := True;
      end Report;

      entry Wait (Index : Command_Index; Cycles : out Dimensionless) when Has_Data is
      begin
         if Current_Index /= Index then
            raise Constraint_Error
              with
                "Reported loop move cycles index mismatch. Expected "
                & Index'Image
                & " but got "
                & Current_Index'Image;
         end if;

         Cycles := Current_Cycles;
         Has_Data := False;
      end Wait;
   end Loop_Move_Cycles;

   protected body Reload_Signal is
      entry Wait when Reload_Requested is
      begin
         Reload_Requested := False;
      end Wait;

      procedure Signal is
      begin
         if Startup_Done then
            Reload_Requested := True;
         else
            --  Nothing has actually started yet, so there's nothing to restart. We reload the web server anyway to
            --  prevent any confusion when the reload button does nothing.
            My_Web_Server.Reset;
            null;
         end if;
      end Signal;

      procedure Mark_Startup_Done is
      begin
         Startup_Done := True;
         My_Web_Server.Notify_Startup_Done;
      end Mark_Startup_Done;
   end Reload_Signal;

   procedure Signal_Reload is
   begin
      Reload_Signal.Signal;
   end Signal_Reload;

   function Get_Current_Position return Position is
   begin
      return Last_Command_Executed.Get_Current_Position;
   end Get_Current_Position;

   function Get_Current_File_Name return Virtual_String is
   begin
      return My_Gcode_Queue.Get_Current_File;
   end Get_Current_File_Name;

   function Get_Current_File_Line return File_Line_Count is
   begin
      return My_Gcode_Queue.Get_Current_Line_Number;
   end Get_Current_File_Line;

   function Stepgen_Paused return Boolean is
   begin
      return My_Step_Generator.Is_Paused;
   end Stepgen_Paused;

begin
   Ada.Task_Termination.Set_Dependents_Fallback_Handler (Exception_Occurrence_Holder.all.Set_Fatal'Access);
end Prunt.Controller;
