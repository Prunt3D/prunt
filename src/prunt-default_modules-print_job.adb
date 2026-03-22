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

package body Prunt.Default_Modules.Print_Job is

   pragma Extensions_Allowed (On);

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
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
      return My_Modules.Module_Instance'Class is
   begin
      return Result : Module_Instance;
   end Initialize;

   protected body Module_Instance is
      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         Self_Ref := Self_Ref_In;
      end Start;

      procedure Report_Print_Time (Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Planner);
         My_Logger.Log ("M31 print-time reporting is not implemented yet.");
      end Report_Print_Time;

      procedure Set_Print_Progress
        (Planner : Planner_Interface'Class;
         C       : Gcode_Optional_Integer;
         P       : Gcode_Optional_Integer;
         R       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, C, P, R);
         My_Logger.Log ("M73 print progress reporting is not implemented yet.");
      end Set_Print_Progress;

      procedure Start_Print_Job_Timer (Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Planner);
         raise Constraint_Error with "M75 is not implemented yet.";
      end Start_Print_Job_Timer;

      procedure Pause_Print_Job_Timer (Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Planner);
         raise Constraint_Error with "M76 is not implemented yet.";
      end Pause_Print_Job_Timer;

      procedure Stop_Print_Job_Timer (Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Planner);
         raise Constraint_Error with "M77 is not implemented yet.";
      end Stop_Print_Job_Timer;

      procedure Report_Print_Job_Stats (Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Planner);
         My_Logger.Log ("M78 print-job statistics are not implemented yet.");
      end Report_Print_Job_Stats;
   end Module_Instance;

end Prunt.Default_Modules.Print_Job;
