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

with System.Multiprocessors;
with Prunt.Motion_Planner.Planner;
with Prunt.Input_Shapers;

generic
   with package Planner is new Motion_Planner.Planner (<>);

   type Stepper_Name is (<>);

   type Stepper_Position is array (Stepper_Name) of Dimensionless;

   with
     procedure Start_Planner_Block
       (Resetting_Data : Planner.Flush_Resetting_Data_Type; Last_Command_Index : Command_Index);

   with
     procedure Enqueue_Command
       (Pos             : Position;
        Stepper_Pos     : Stepper_Position;
        Index           : Command_Index;
        Loop_Until_Hit  : Boolean;
        Safe_Stop_After : Boolean;
        Vel_Ratio       : Dimensionless);

   with procedure Start_Corner (Last_Command_Index : Command_Index; Data : Planner.Corner_Extra_Data_Type);
   --  Called when we start moving towards a corner for each extra data element.
   --
   --  This is not included in `Enqueue_Command` as floating point inaccuracy could potentially cause a very short
   --  segment to not contain any command even though segments have a minimum length.

   with
     procedure Finish_Planner_Block
       (Resetting_Data       : Planner.Flush_Resetting_Data_Type;
        Next_Block_Pos       : Stepper_Position;
        First_Accel_Distance : Length;
        Last_Command_Index   : Command_Index;
        Loop_Move_Offset     : Position_Offset);
   --  `First_Accel_Distance` is the distance length of the acceleration part of the first move. This is used to
   --  determine the position after a homing move as the loop move starts as soon as possible after the acceleration
   --  part.

   Loop_Cycle_Reporter : access Loop_Cycle_Reporter_Interface'Class;

   Interpolation_Time : Time;

   Runner_CPU : System.Multiprocessors.CPU_Range;
package Prunt.Step_Generator is
   use Planner;

   type Stepper_Pos_Map is array (Axis_Name, Stepper_Name) of Length;

   task Runner
     with
       CPU          => Runner_CPU,
       Storage_Size => 32 * 1024 * 1024
       --  Allows for very large shapers and shaper buffers to be allocated.
   is
      entry Setup (Map : Stepper_Pos_Map);
      --  Configure the step generator with the stepper position map. This must be called before any steps can be
      --  generated.
      entry Reset;
      --  Reset the step generator state. This should be called when the machine is disabled or reset.
   end Runner;

   procedure Pause;
   --  Request the step generator to pause execution. This call is non-blocking. Use Is_Paused to check if the pause
   --  has taken effect.
   procedure Resume;
   --  Request the step generator to resume execution from a paused state.
   function Is_Paused return Boolean;
   --  Returns True if the step generator is currently fully paused.

private

   type Pause_Slew_Index is new Integer range 0 .. Integer (3.0 * s / Interpolation_Time);
   --  Max at paused end of slew.

   function Pause_Slew_Interpolation_Time (Index : Pause_Slew_Index) return Time;
   --  Calculates the interpolation time scaling factor for the pause/resume slew.

   function To_Stepper_Position (Pos : Position; Map : Stepper_Pos_Map) return Stepper_Position;
   --  Converts a cartesian position to stepper positions using the provided map.

end Prunt.Step_Generator;
