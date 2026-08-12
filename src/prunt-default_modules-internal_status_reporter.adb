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

with Ada.Real_Time;
with Ada.Strings;
with Ada.Strings.Fixed;
with VSS.Strings.Conversions;

package body Prunt.Default_Modules.Internal_Status_Reporter is

   pragma Extensions_Allowed (On);

   use type Ada.Real_Time.Time;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   function Current_Position_Report (Pos : Position) return Virtual_String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      return
        +("X:"
          & Trim (Dimensionless'Image (Pos (X_Axis) / mm), Both)
          & " Y:"
          & Trim (Dimensionless'Image (Pos (Y_Axis) / mm), Both)
          & " Z:"
          & Trim (Dimensionless'Image (Pos (Z_Axis) / mm), Both)
          & " E:"
          & Trim (Dimensionless'Image (Pos (E_Axis) / mm), Both));
   end Current_Position_Report;

   procedure Log_Position is
      Pos : constant Position := Get_Position;
   begin
      My_Logger.Log (Current_Position_Report (Pos));
   end Log_Position;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
   begin
      return Result : Module_Instance do
         Result.Initialize (Status_Emitter);
      end return;
   end Initialize;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map is
   begin
      return
        ["Homed"        =>
           [for A in Axis_Name use Conversions.To_Virtual_String (A'Image) =>
              (Kind        => Status_Manager.Boolean_Kind,
               Unit        => "",
               Description => "True if axis " & Conversions.To_Virtual_String (A'Image) & " is currently homed.",
               Condition   => "")],
         "Position"     =>
           [for A in Axis_Name use Conversions.To_Virtual_String (A'Image) =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm",
               Description => "Position of axis " & Conversions.To_Virtual_String (A'Image),
               Condition   => "")],
         "Print status" =>
           ["File name"    =>
              (Kind        => Status_Manager.String_Kind,
               Unit        => "",
               Description => "Name of current g-code file.",
               Condition   => ""),
            "Current line" =>
              (Kind        => Status_Manager.Integer_Kind,
               Unit        => "",
               Description => "Current line in g-code file.",
               Condition   => ""),
            "Paused"       =>
              (Kind        => Status_Manager.Boolean_Kind,
               Unit        => "",
               Description => "True if printer is currently paused.",
               Condition   => "")]];
   end Status_Schema;

   task body Status_Updater is
      Status_Ref                   : Status_Manager.Status_Emitter;
      Stop_Received                : Boolean := False;
      Next_Status_Report           : Ada.Real_Time.Time;
      Current_Auto_Report_Interval : Duration := 0.0;
      Next_Position_Auto_Report    : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
   begin
      select
         accept Stop;
         Stop_Received := True;
      or
         accept Start (Status_Emitter : Status_Manager.Status_Emitter) do
            Status_Ref := Status_Emitter;
            Next_Status_Report := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Status_Report_Period);
         end Start;
      end select;

      while not Stop_Received loop
         select
            accept Stop;
            Stop_Received := True;
         or
            accept Set_Position_Auto_Report_Interval (Value : Duration) do
               Current_Auto_Report_Interval := Value;

               if Current_Auto_Report_Interval > 0.0 then
                  Next_Position_Auto_Report :=
                    Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Current_Auto_Report_Interval);
               end if;
            end Set_Position_Auto_Report_Interval;
         or
            delay until Next_Status_Report;

            declare
               Pos : constant Position := Get_Position;
            begin
               for A in Axis_Name loop
                  Status_Ref.Set_Value ("Homed", +A'Image, Axis_Is_Homed (A));
                  Status_Ref.Set_Value ("Position", +A'Image, Pos (A) / mm);
               end loop;
               Status_Ref.Set_Value ("Print status", "File name", Get_File_Name);
               Status_Ref.Set_Value ("Print status", "Current line", Long_Long_Integer (File_Line_Count'(Get_Line)));
               Status_Ref.Set_Value ("Print status", "Paused", Stepgen_Paused);
            end;

            Next_Status_Report := Next_Status_Report + Ada.Real_Time.To_Time_Span (Status_Report_Period);
            if Next_Status_Report > Ada.Real_Time.Clock then
               Next_Status_Report := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Status_Report_Period);
            end if;
         or
            when Current_Auto_Report_Interval > 0.0 =>
            delay until Next_Position_Auto_Report;
            Log_Position;

            Next_Position_Auto_Report :=
              Next_Position_Auto_Report + Ada.Real_Time.To_Time_Span (Current_Auto_Report_Interval);
            if Next_Position_Auto_Report > Ada.Real_Time.Clock then
               Next_Position_Auto_Report :=
                 Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Current_Auto_Report_Interval);
            end if;
         end select;
      end loop;
   end Status_Updater;

   overriding
   procedure Finalize (Object : in out Status_Updater_Wrapper) is
   begin
      Object.Updater.Stop;
      --  This is valid since this wrapper will always exist inside a smart pointer. Specifically, we do not have to
      --  worry about the fact that 7.6.1(4) says tasks are stopped before finalization of a master since finalization
      --  here is not happening via finalization of a master.
   end Finalize;

   protected body Module_Instance is
      procedure Initialize (Status_Emitter_In : Status_Manager.Status_Emitter) is
         function Make_Updater_Task return Status_Updater_Wrapper;

         function Make_Updater_Task return Status_Updater_Wrapper is
         begin
            return Result : Status_Updater_Wrapper;
         end Make_Updater_Task;
      begin
         Status_Emitter := Status_Emitter_In;
         Updater.Set (Make_Updater_Task'Access);
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Self_Ref_In, Planner);
         Updater.Get.Updater.Start (Status_Emitter);
      end Start;

      procedure Set_Position_Auto_Report_Interval (Value : Duration) is
      begin
         Updater.Get.Updater.Set_Position_Auto_Report_Interval (Value);
      end Set_Position_Auto_Report_Interval;

   end Module_Instance;

   overriding
   procedure Process_After_Block (This : Position_Report_Event; Context : Block_End_Context'Class) is
      pragma Unreferenced (This);
   begin
      Context.Wait_For_Idle;

      Log_Position;
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Position_Auto_Report_Event; Context : Block_End_Context'Class) is
   begin
      Context.Wait_For_Idle;

      Module_Instance (This.Module_Instance_Ref.Get.Element.all).Set_Position_Auto_Report_Interval (This.Interval);

      if This.Interval = 0.0 then
         My_Logger.Log ("Position auto-reporting disabled.");
      else
         My_Logger.Log
           (+("Position auto-reporting every "
              & Ada.Strings.Fixed.Trim (This.Interval'Image, Ada.Strings.Both)
              & " seconds."));
      end if;
   end Process_After_Block;

   procedure Report_Current_Position (Planner : Planner_Interface'Class) is
   begin
      Planner.Flush (Position_Report_Event'(null record));
   end Report_Current_Position;

   procedure Set_Position_Auto_Report
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref; Planner : Planner_Interface'Class; S : Dimensionless)
   is
      Interval : Duration;
   begin
      if S < 0.1 then
         raise Gcode_Bad_Inputs_Error with "Position auto-report interval must be 0.1 or higher.";
      end if;

      if S > Dimensionless (Duration'Last) then
         raise Gcode_Bad_Inputs_Error
           with "Position auto-report interval must not be greater than " & Duration'Last'Image;
      end if;

      begin
         Interval := Duration (S);
      exception
         when Constraint_Error =>
            raise Gcode_Bad_Inputs_Error with "Position auto-report interval is out of range.";
      end;

      Planner.Flush (Position_Auto_Report_Event'(Module_Instance_Ref => Self_Ref, Interval => Interval));
   end Set_Position_Auto_Report;

end Prunt.Default_Modules.Internal_Status_Reporter;
