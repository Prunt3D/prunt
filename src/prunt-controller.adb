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
with Ada.Containers.Vectors;
with Ada.Tags;
with Ada.Task_Identification;
with Ada.Task_Termination;
with VSS.Strings.Conversions;

package body Prunt.Controller is

   pragma Extensions_Allowed (On);

   protected body Planner_State is
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
   end Planner_State;

   overriding
   function Get_Last_Position (This : Planner_Wrapper) return Position is
      pragma Unreferenced (This);
   begin
      return Planner_State.Get_Last_Position;
   end Get_Last_Position;

   overriding
   function Get_Last_Kinematic_Parameters (This : Planner_Wrapper) return Motion_Planner.Kinematic_Parameters is
      pragma Unreferenced (This);
   begin
      return Planner_State.Get_Last_Kinematic_Parameters;
   end Get_Last_Kinematic_Parameters;

   overriding
   procedure Mark_Axis_Homed (This : Planner_Wrapper; Axis : Axis_Name) is
   begin
      pragma Unreferenced (This, Axis); --  TODO
      null;
   end Mark_Axis_Homed;

   type Planner_Block_End_Context is limited new Module_Types.Block_End_Context with record
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index;
      Loop_Move_Offset     : Position_Offset;
   end record;

   overriding
   function Get_First_Accel_Distance (This : Planner_Block_End_Context) return Length;
   overriding
   function Get_Last_Command_Index (This : Planner_Block_End_Context) return Command_Index;
   overriding
   function Get_Loop_Move_Offset (This : Planner_Block_End_Context) return Position_Offset;
   overriding
   procedure Wait_For_Idle (This : Planner_Block_End_Context);

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
      loop
         exit when Last_Command_Executed.Get >= This.Last_Command_Index;
         delay 0.001;
      end loop;
   end Wait_For_Idle;

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
      Require_Homed : Boolean := True;
      Corner_Data   : Extra_Corner_Data'Class := Extra_Corner_Data'(null record))
   is
      pragma Unreferenced (Require_Homed);
   begin
      if This.Startup_Mode and then Pos /= Planner_State.Get_Last_Position then
         raise Constraint_Error with "Motion not allowed during startup.";
      end if;

      Planner_State.Set_Last_Position (Pos);

      My_Motion_Planner.Enqueue_Move (Pos => Pos, Feedrate => Feedrate, Dwell_After => Dwell_After);

      if Corner_Data not in Extra_Corner_Data then
         My_Motion_Planner.Enqueue_Corner_Extra_Data (Corner_Data);
      end if;
   end Add_Corner;

   overriding
   procedure Add_Corner_Data (This : Planner_Wrapper; Corner_Data : Extra_Corner_Data'Class) is
   begin
      if Corner_Data not in Extra_Corner_Data then
         My_Motion_Planner.Enqueue_Corner_Extra_Data (Corner_Data);
      end if;
   end Add_Corner_Data;

   overriding
   procedure Flush
     (This           : Planner_Wrapper;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False)
   is
      pragma Unreferenced (This);
   begin
      My_Motion_Planner.Enqueue_Flush
        (Extra_Block_Resetting_Data_Holders.To_Holder (Extra_Data), Is_Homing_Move => Is_Homing_Move);
   end Flush;

   overriding
   procedure Flush_And_Change_Kinematic_Parameters
     (This           : Planner_Wrapper;
      Params         : Motion_Planner.Kinematic_Parameters;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False)
   is
      pragma Unreferenced (This);
   begin
      Planner_State.Set_Last_Kinematic_Parameters (Params);

      My_Motion_Planner.Enqueue_Flush_And_Change_Kinematic_Parameters
        (Extra_Block_Resetting_Data_Holders.To_Holder (Extra_Data), Params, Is_Homing_Move => Is_Homing_Move);
   end Flush_And_Change_Kinematic_Parameters;

   overriding
   procedure Flush_And_Reset_Position
     (This           : Planner_Wrapper;
      New_Position   : Position;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False)
   is
      pragma Unreferenced (This);
   begin
      Planner_State.Set_Last_Position (New_Position);

      My_Motion_Planner.Enqueue_Flush_And_Reset_Position
        (Data           => Extra_Block_Resetting_Data_Holders.To_Holder (Extra_Data),
         Pos            => New_Position,
         Is_Homing_Move => Is_Homing_Move);
   end Flush_And_Reset_Position;

   procedure Prompt_For_Update is
   begin
      null; --  TODO
   end Prompt_For_Update;

   procedure Run is
      Active_Module_Instances : Module_Instance_Maps.Map := [];

      procedure Attempt_Start is
         Had_Error : Boolean := False;

         procedure Report_Config_Error (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String) is
         begin
            My_Logger.Log ("Startup error: " & Conversions.To_Virtual_String (Path'Image) & ": " & Message);
            Had_Error := True;
         end Report_Config_Error;

         Startup_Planner : constant Planner_Wrapper := (Startup_Mode => True);
      begin
         Planner_State.Reset;

         Active_Module_Instances :=
           Recursive_Module_Initialization
             (Report_Config_Error'Access, Active_Config_File, Log_Dependency_Tree => True);

         if Had_Error then
            My_Logger.Log ("Prunt could not start due to configuration errors.");
            Active_Module_Instances.Reverse_Clear;
         else
            for M of Active_Module_Instances loop
               My_Modules.Module_Instance'Class (M.Get.Element.all).Start (M.Weak, Startup_Planner);
            end loop;
         end if;
      end Attempt_Start;

   begin
      Reload_Signal.Mark_Startup_Done;

      Main : loop
         Attempt_Start;

         select
            Reload_Signal.Wait;
            My_Logger.Log ("Reload requested. Resetting...");
            Exception_Occurrence_Holder.Reset;
            Active_Config_File.Reset_Live_To_Stored;
            Reset_Hardware;
            My_Web_Server.Reset;
         then abort
            Exception_Occurrence_Holder.Enter_When_Fatal_Set;
            exit Main;
         end select;
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

      function Find_Existing_Instance (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref is
      begin
         for I of Result loop
            if I.Get.Element'Tag = Tag then
               return I;
            end if;
         end loop;

         return My_Modules.Module_Instance_Shared_Pointers.Null_Ref;
      end Find_Existing_Instance;

      function Find_Module_By_Tag (Tag : Ada.Tags.Tag) return Virtual_String is
      begin
         for I in Result.Iterate loop
            if Element (I).Get.Element'Tag = Tag then
               return Key (I);
            end if;
         end loop;

         raise Constraint_Error;
      end Find_Module_By_Tag;

      procedure Record_Dependency_Request (Requester : Virtual_String; Requested_Tag : Ada.Tags.Tag) is
      begin
         for Request of Dependency_Requests loop
            if Request.Requester = Requester and then Request.Requested_Tag = Requested_Tag then
               return;
            end if;
         end loop;

         Dependency_Requests.Append (Dependency_Request'(Requester => Requester, Requested_Tag => Requested_Tag));
      end Record_Dependency_Request;

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

      function Recurse return Natural is
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
                    Config.Create (Config_Path, Active_Module_Config_Schemas);
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

   procedure Submit_Gcode_Command (Command : Virtual_String; Succeeded : out Boolean) is
   begin
      My_Gcode_Queue.Try_Set_Command (Command, Succeeded);
   end Submit_Gcode_Command;

   procedure Submit_Gcode_File (Path : Virtual_String; Succeeded : out Boolean) is
   begin
      My_Gcode_Queue.Try_Set_File (Path, Succeeded);
   end Submit_Gcode_File;

   procedure Reload_Server is
   begin
      null; --  TODO
   end Reload_Server;

   procedure Start_Planner_Block
     (Resetting_Data : Extra_Block_Resetting_Data_Holders.Holder; Last_Command_Index : Command_Index) is
   begin
      null;
      --  if not Resetting_Data.Is_Empty then
      --     Resetting_Data.Element.Process_Before_Block (Last_Command_Index);
      --  end if;
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
      null; --  TODO
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
        (First_Accel_Distance => First_Accel_Distance,
         Last_Command_Index   => Last_Command_Index,
         Loop_Move_Offset     => Loop_Move_Offset);
   begin
      if not Resetting_Data.Is_Empty then
         Resetting_Data.Element.Process_After_Block (Context);
         Reset_Position (Next_Block_Pos);
      end if;
   end Finish_Planner_Block;

   procedure Report_Last_Command_Executed (Index : Command_Index) is
   begin
      Last_Command_Executed.Report (Index);
   end Report_Last_Command_Executed;

   procedure Report_Loop_Move_Cycles (Index : Command_Index; Cycles : Dimensionless) is
   begin
      Loop_Move_Cycles.Report (Index, Cycles);
   end Report_Loop_Move_Cycles;

   protected body Last_Command_Executed is
      procedure Report (Index : Command_Index) is
      begin
         Current_Index := Index;
      end Report;

      function Get return Command_Index is
      begin
         return Current_Index;
      end Get;
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
      --  TODO
      return [others => 0.0 * mm];
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
