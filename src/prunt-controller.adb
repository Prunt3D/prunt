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

with Ada.Tags;
with Ada.Task_Identification;
with Ada.Task_Termination;
with VSS.Strings.Conversions;

package body Prunt.Controller is

   pragma Extensions_Allowed (On);

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
      begin
         Active_Module_Instances := Recursive_Module_Initialization (Report_Config_Error'Access, Active_Config_File);

         if Had_Error then
            My_Logger.Log ("Prunt could not start due to configuration errors.");
            Active_Module_Instances.Reverse_Clear;
         else
            for M of Active_Module_Instances loop
               M.Get.Start;
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
      My_Config_File      : Config.Config_File) return Module_Instance_Maps.Map
   is
      use Module_Maps;
      use Module_Instance_Maps;
      use type Ada.Tags.Tag;
      use type My_Modules.Module_Instance_Shared_Pointers.Ref;

      Result : Module_Instance_Maps.Map := [];

      function Recurse return Natural is
         function Get_Other_Instance (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref is
         begin
            loop
               for I of Result loop
                  if I /= My_Modules.Module_Instance_Shared_Pointers.Null_Ref and then I.Get.Element'Tag = Tag then
                     return I;
                  end if;
               end loop;

               exit when Recurse = 0;
               --  If the requested module instance is not found then keep instantiating modules until it is found
               --  or until all other modules are trying to get another instance.
            end loop;

            return My_Modules.Module_Instance_Shared_Pointers.Null_Ref;
         end Get_Other_Instance;

         Modules_Initialized : Natural := 0;
      begin
         for C in Active_Modules.Iterate loop
            if not Result.Contains (Key (C)) then
               Result.Insert (Key (C), My_Modules.Module_Instance_Shared_Pointers.Null_Ref);
               --  We insert a null reference to avoid an infinite loop when circular dependencies are present. One
               --  of the modules in the dependency loop will receive the null reference.
               declare
                  procedure Report_Config_Error_With_Module
                    (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String)
                  is
                     use type Config.Config_Data_Paths.Vector;
                  begin
                     Report_Config_Error (["Config", Key (C), "Config"] & Path, Message);
                  end Report_Config_Error_With_Module;

                  function Get_Data return My_Modules.Module_Instance'Class is
                     Emitter_Ref : My_Modules.Status_Emitter_Shared_Pointers.Ref :=
                       My_Modules.Status_Emitter_Shared_Pointers.Null_Ref;
                     Config_Ref  : My_Modules.Config_Data_Shared_Pointers.Ref :=
                       My_Modules.Config_Data_Shared_Pointers.Null_Ref;
                  begin
                     Emitter_Ref.Set (Status_Manager.Get_Emitter (My_Status_Data, Key (C)));
                     Config_Ref.Set (My_Config_File.Get_Data (Key (C)));
                     return
                       Element (C).Initialize
                         (Config_Ref, Report_Config_Error_With_Module'Access, Emitter_Ref, Get_Other_Instance'Access);
                  end Get_Data;

                  Ref : My_Modules.Module_Instance_Shared_Pointers.Ref :=
                    My_Modules.Module_Instance_Shared_Pointers.Null_Ref;
               begin
                  Result.Delete (Key (C));
                  --  Remove the previously inserted null reference.

                  Ref.Set (Get_Data'Access);
                  Result.Insert (Key (C), Ref);
               end;

               for Other in Result.Iterate loop
                  if Key (Other) /= Key (C)
                    and then Element (Other) /= My_Modules.Module_Instance_Shared_Pointers.Null_Ref
                    and then Element (Other).Get.Element'Tag = Result (Key (C)).Get.Element'Tag
                  then
                     --  Duplicate module tags are not supported as module instances retrieve other module instances
                     --  solely by tag and not by name.
                     raise Program_Error with "Duplicate module tag: " & Key (C)'Image & " and " & Key (Other)'Image;
                  end if;
               end loop;

               Modules_Initialized := @ + 1;
            end if;
         end loop;

         return Modules_Initialized;
      end Recurse;

      Ignored : Natural := Recurse;
   begin
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
      if not Resetting_Data.Is_Empty then
         Resetting_Data.Element.Process_Before_Block (Last_Command_Index);
      end if;
   end Start_Planner_Block;

   procedure Enqueue_Command_Internal
     (Pos             : Position;
      Stepper_Pos     : Stepper_Position;
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
      Next_Block_Pos       : Stepper_Position;
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index;
      Loop_Move_Offset     : Position_Offset) is
   begin
      if not Resetting_Data.Is_Empty then
         Resetting_Data.Element.Process_After_Block
           (First_Accel_Distance => First_Accel_Distance,
            Last_Command_Index   => Last_Command_Index,
            Loop_Move_Offset     => Loop_Move_Offset);
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

   My_Logger.Log
     (Conversions.To_Virtual_String ("Gcode dispatch map size: " & Active_Module_Gcode_Dispatch_Map.Length'Image));
end Prunt.Controller;
