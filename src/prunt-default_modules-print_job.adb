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

with Ada.Strings;
with Ada.Strings.Fixed;

package body Prunt.Default_Modules.Print_Job is

   pragma Extensions_Allowed (On);

   use type Prunt.Gcode_Arguments.Argument_Integer;

   use Ada.Real_Time;

   function Trimmed_Image (Value : Integer) return String is
     (Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Both));

   function Trimmed_Image (Value : Gcode_Arguments.Argument_Integer) return String is
     (Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Both));

   function Format_Duration (Value : Duration) return String is
      Total_Seconds : constant Integer := Integer (Long_Long_Integer'Max (0, Long_Long_Integer (Value)));
      Hours         : constant Integer := Total_Seconds / 3_600;
      Minutes       : constant Integer := (Total_Seconds mod 3_600) / 60;
      Seconds       : constant Integer := Total_Seconds mod 60;
   begin
      if Hours > 0 then
         return Trimmed_Image (Hours) & "h " & Trimmed_Image (Minutes) & "m " & Trimmed_Image (Seconds) & "s";
      elsif Minutes > 0 then
         return Trimmed_Image (Minutes) & "m " & Trimmed_Image (Seconds) & "s";
      else
         return Trimmed_Image (Seconds) & "s";
      end if;
   end Format_Duration;

   function Timer_State_Image (State : Print_Job_Timer_State) return Virtual_String is
     (case State is
         when Stopped => "Stopped",
         when Running => "Running",
         when Paused  => "Paused");

   procedure Log_Progress_Value
     (Label       : String;
      Value       : Gcode_Optional_Integer;
      Suffix      : String := "";
      Unset_Label : String := "not set")
   is
   begin
      if Value.Present then
         My_Logger.Log (+(Label & ": " & Trimmed_Image (Value.Value) & Suffix));
      else
         My_Logger.Log (+(Label & ": " & Unset_Label));
      end if;
   end Log_Progress_Value;

   procedure Log_Snapshot (Snapshot : Print_Job_Snapshot; Include_State : Boolean) is
   begin
      if Include_State then
         My_Logger.Log ("Print job timer state: " & Timer_State_Image (Snapshot.Timer_State));
      end if;

      My_Logger.Log ("Elapsed print time: " & (+Format_Duration (Snapshot.Elapsed_Time)));
      Log_Progress_Value ("Print progress", Snapshot.Progress_Percentage, "%");
      Log_Progress_Value ("Remaining time", Snapshot.Remaining_Time_In_Minutes, " min");
      Log_Progress_Value ("Next interaction countdown", Snapshot.Interaction_Countdown, " min");
   end Log_Snapshot;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Config_Data, Report_Config_Error, Status_Emitter, Get_Other_Instance);
   begin
      return Result : Module_Instance do
         Result.Initialize;
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize is
      begin
         null;
      end Initialize;

      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Planner);
         Self_Ref := Self_Ref_In;
      end Start;

      procedure Apply_Timer_Command (Command : Print_Job_Timer_Command) is
      begin
         case Command is
            when Start_Timer =>
               if Timer_State = Stopped then
                  Accumulated_Elapsed_Time := 0.0;
                  Last_Start_Time := Clock;
                  Timer_State := Running;
                  My_Logger.Log ("Print job timer started.");
               elsif Timer_State = Paused then
                  Last_Start_Time := Clock;
                  Timer_State := Running;
                  My_Logger.Log ("Print job timer resumed.");
               else
                  My_Logger.Log ("Print job timer is already running.");
               end if;

            when Pause_Timer =>
               if Timer_State = Running then
                  Accumulated_Elapsed_Time :=
                    Accumulated_Elapsed_Time + To_Duration (Clock - Last_Start_Time);
                  Timer_State := Paused;
                  My_Logger.Log ("Print job timer paused.");
               elsif Timer_State = Paused then
                  My_Logger.Log ("Print job timer is already paused.");
               else
                  My_Logger.Log ("Print job timer is not running.");
               end if;

            when Stop_Timer =>
               if Timer_State = Running then
                  Accumulated_Elapsed_Time :=
                    Accumulated_Elapsed_Time + To_Duration (Clock - Last_Start_Time);
               end if;

               if Timer_State /= Stopped then
                  Timer_State := Stopped;
                  My_Logger.Log ("Print job timer stopped.");
               else
                  My_Logger.Log ("Print job timer is already stopped.");
               end if;
         end case;
      end Apply_Timer_Command;

      procedure Apply_Progress_Update
        (C : Gcode_Optional_Integer;
         P : Gcode_Optional_Integer;
         R : Gcode_Optional_Integer)
      is
      begin
         if C.Present then
            Interaction_Countdown := C;
            My_Logger.Log (+("Next interaction countdown: " & Trimmed_Image (C.Value) & " min"));
         end if;

         if P.Present then
            Progress_Percentage := P;
            My_Logger.Log (+("Print progress: " & Trimmed_Image (P.Value) & "%"));
         end if;

         if R.Present then
            Remaining_Time_In_Minutes := R;
            My_Logger.Log (+("Remaining time: " & Trimmed_Image (R.Value) & " min"));
         end if;
      end Apply_Progress_Update;

      function Snapshot return Print_Job_Snapshot is
         Elapsed_Time : Duration := Accumulated_Elapsed_Time;
      begin
         if Timer_State = Running then
            Elapsed_Time := Elapsed_Time + To_Duration (Clock - Last_Start_Time);
         end if;

         return
           (Timer_State               => Timer_State,
            Elapsed_Time              => Elapsed_Time,
            Interaction_Countdown     => Interaction_Countdown,
            Progress_Percentage       => Progress_Percentage,
            Remaining_Time_In_Minutes => Remaining_Time_In_Minutes);
      end Snapshot;
   end Module_Instance;

   overriding
   procedure Process_After_Block (This : Print_Job_Timer_Event; Context : Block_End_Context'Class) is
   begin
      Context.Wait_For_Idle;
      Module_Instance (This.Module_Instance_Ref.Get.Element.all).Apply_Timer_Command (This.Command);
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Print_Job_Progress_Update; Context : Block_End_Context'Class) is
   begin
      Context.Wait_For_Idle;
      Module_Instance (This.Module_Instance_Ref.Get.Element.all).Apply_Progress_Update (This.C, This.P, This.R);
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Print_Job_Report_Event; Context : Block_End_Context'Class) is
      Snapshot : Print_Job_Snapshot;
   begin
      Context.Wait_For_Idle;
      Snapshot := Module_Instance (This.Module_Instance_Ref.Get.Element.all).Snapshot;

      case This.Kind is
         when Time_Report =>
            My_Logger.Log (+("Print time: " & Format_Duration (Snapshot.Elapsed_Time)));

         when Progress_Report =>
            Log_Progress_Value ("Print progress", Snapshot.Progress_Percentage, "%");
            Log_Progress_Value ("Remaining time", Snapshot.Remaining_Time_In_Minutes, " min");
            Log_Progress_Value ("Next interaction countdown", Snapshot.Interaction_Countdown, " min");

         when Stats_Report =>
            Log_Snapshot (Snapshot, Include_State => True);
      end case;
   end Process_After_Block;

   procedure Report_Print_Time
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class) is
   begin
      Planner.Flush (Print_Job_Report_Event'(Module_Instance_Ref => Self_Ref, Kind => Time_Report));
   end Report_Print_Time;

   procedure Set_Print_Progress
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      C       : Gcode_Optional_Integer;
      P       : Gcode_Optional_Integer;
      R       : Gcode_Optional_Integer) is
   begin
      if C.Present and then C.Value < 0 then
         raise Gcode_Bad_Inputs_Error with "Interaction countdown must be 0 or greater.";
      end if;

      if P.Present and then (P.Value < 0 or else P.Value > 100) then
         raise Gcode_Bad_Inputs_Error with "Print progress percentage must be between 0 and 100.";
      end if;

      if R.Present and then R.Value < 0 then
         raise Gcode_Bad_Inputs_Error with "Remaining time must be 0 or greater.";
      end if;

      if not C.Present and then not P.Present and then not R.Present then
         Planner.Flush (Print_Job_Report_Event'(Module_Instance_Ref => Self_Ref, Kind => Progress_Report));
      else
         Planner.Flush (Print_Job_Progress_Update'(Module_Instance_Ref => Self_Ref, C => C, P => P, R => R));
      end if;
   end Set_Print_Progress;

   procedure Start_Print_Job_Timer
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class) is
   begin
      Planner.Flush (Print_Job_Timer_Event'(Module_Instance_Ref => Self_Ref, Command => Start_Timer));
   end Start_Print_Job_Timer;

   procedure Pause_Print_Job_Timer
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class) is
   begin
      Planner.Flush (Print_Job_Timer_Event'(Module_Instance_Ref => Self_Ref, Command => Pause_Timer));
   end Pause_Print_Job_Timer;

   procedure Stop_Print_Job_Timer
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class) is
   begin
      Planner.Flush (Print_Job_Timer_Event'(Module_Instance_Ref => Self_Ref, Command => Stop_Timer));
   end Stop_Print_Job_Timer;

   procedure Report_Print_Job_Stats
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class) is
   begin
      Planner.Flush (Print_Job_Report_Event'(Module_Instance_Ref => Self_Ref, Kind => Stats_Report));
   end Report_Print_Job_Stats;

end Prunt.Default_Modules.Print_Job;
