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
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
   begin
      return Result : Module_Instance;
   end Initialize;

   protected body Module_Instance is
      procedure Start is null;

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
