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

with Ada.Real_Time;
with Ada.Tags;
with Prunt.Config;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
package Prunt.Default_Modules.Print_Job is

   type Module is new My_Modules.Module with null record;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

private

   type Print_Job_Timer_State is (Stopped, Running, Paused);

   type Print_Job_Timer_Command is (Start_Timer, Pause_Timer, Stop_Timer);

   type Print_Job_Snapshot is record
      Timer_State                : Print_Job_Timer_State := Stopped;
      Elapsed_Time               : Duration := 0.0;
      Interaction_Countdown      : Gcode_Optional_Integer := (Present => False);
      Progress_Percentage        : Gcode_Optional_Integer := (Present => False);
      Remaining_Time_In_Minutes  : Gcode_Optional_Integer := (Present => False);
   end record;

   type Print_Job_Timer_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Command             : Print_Job_Timer_Command;
   end record;

   overriding
   procedure Process_After_Block (This : Print_Job_Timer_Event; Context : Block_End_Context'Class);

   type Print_Job_Progress_Update is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      C                   : Gcode_Optional_Integer;
      P                   : Gcode_Optional_Integer;
      R                   : Gcode_Optional_Integer;
   end record;

   overriding
   procedure Process_After_Block (This : Print_Job_Progress_Update; Context : Block_End_Context'Class);

   type Print_Job_Report_Kind is (Time_Report, Progress_Report, Stats_Report);

   type Print_Job_Report_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Kind                : Print_Job_Report_Kind;
   end record;

   overriding
   procedure Process_After_Block (This : Print_Job_Report_Event; Context : Block_End_Context'Class);

   procedure Report_Print_Time
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M31");
   --  Report elapsed print time.

   procedure Set_Print_Progress
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      C       : Gcode_Optional_Integer;
      P       : Gcode_Optional_Integer;
      R       : Gcode_Optional_Integer)
   with Annotate => (Prunt_Config, Gcode_Command, "M73");
   --  Set or report print progress data.

   procedure Start_Print_Job_Timer
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M75");
   --  Start the print job timer.

   procedure Pause_Print_Job_Timer
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M76");
   --  Pause the print job timer.

   procedure Stop_Print_Job_Timer
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M77");
   --  Stop the print job timer.

   procedure Report_Print_Job_Stats
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M78");
   --  Report print job statistics.

   protected type Module_Instance is new My_Modules.Module_Instance with
      procedure Initialize;

      overriding
      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Apply_Timer_Command (Command : Print_Job_Timer_Command);

      procedure Apply_Progress_Update
        (C : Gcode_Optional_Integer;
         P : Gcode_Optional_Integer;
         R : Gcode_Optional_Integer);

      function Snapshot return Print_Job_Snapshot;
   private
      Self_Ref                    : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
      Timer_State                 : Print_Job_Timer_State := Stopped;
      Last_Start_Time             : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
      Accumulated_Elapsed_Time    : Duration := 0.0;
      Interaction_Countdown       : Gcode_Optional_Integer := (Present => False);
      Progress_Percentage         : Gcode_Optional_Integer := (Present => False);
      Remaining_Time_In_Minutes   : Gcode_Optional_Integer := (Present => False);
   end Module_Instance;

end Prunt.Default_Modules.Print_Job;
