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

pragma Extensions_Allowed (On);

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
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

private

   protected type Module_Instance is new My_Modules.Module_Instance with
      overriding
      procedure Start;

      procedure Report_Print_Time (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M31");
      --  Report elapsed print time.

      procedure Set_Print_Progress
        (Planner : Planner_Interface'Class;
         C       : Gcode_Optional_Integer;
         P       : Gcode_Optional_Integer;
         R       : Gcode_Optional_Integer)
      with Annotate => (Prunt_Config, Gcode_Command, "M73");
      --  Set or report print progress data.

      procedure Start_Print_Job_Timer (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M75");
      --  Start the print job timer.

      procedure Pause_Print_Job_Timer (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M76");
      --  Pause the print job timer.

      procedure Stop_Print_Job_Timer (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M77");
      --  Stop the print job timer.

      procedure Report_Print_Job_Stats (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M78");
      --  Report print job statistics.
   end Module_Instance;

end Prunt.Default_Modules.Print_Job;
