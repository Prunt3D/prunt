-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
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

with System.Multiprocessors;
with Prunt.Motion_Planner.Planner;
with Prunt.Input_Shapers;

generic
   with package Planner is new Motion_Planner.Planner (<>);
   use Planner;

   type Stepper_Name is (<>);

   type Stepper_Position is array (Stepper_Name) of Dimensionless;

   with procedure Start_Planner_Block (Data : Flush_Resetting_Data_Type; Last_Command_Index : Command_Index);
   with procedure Enqueue_Command
     (Pos             : Position;
      Stepper_Pos     : Stepper_Position;
      Data            : Corner_Extra_Data_Type;
      Index           : Command_Index;
      Loop_Until_Hit  : Boolean;
      Safe_Stop_After : Boolean);
   with procedure Finish_Planner_Block
     (Data                 : Flush_Resetting_Data_Type;
      Next_Block_Pos       : Stepper_Position;
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index);
   --  First_Accel_Distance is the distance length of the acceleration part of the first move. This is used to
   --  determine the position after a homing move as the loop move starts as soon as possible after the acceleration
   --  part.

   with function Get_Axial_Shaper_Parameters
     (Data : Block_Persistent_Data_Type) return Input_Shapers.Axial_Shaper_Parameters;

   Interpolation_Time : Time;
   Loop_Interpolation_Time : Time;

   Runner_CPU : System.Multiprocessors.CPU_Range;
package Prunt.Step_Generator.Generator is

   type Stepper_Pos_Map is array (Axis_Name, Stepper_Name) of Length;

   task Runner with
     CPU => Runner_CPU,
     Storage_Size => 32 * 1024 * 1024
     --  Allows for very large shapers and shaper buffers to be allocated.
   is
      entry Setup (Map : Stepper_Pos_Map);
      entry Finish;
   end Runner;

   procedure Pause;
   procedure Resume;
   function Is_Paused return Boolean;

private

   type Pause_Slew_Index is new Integer range 0 .. Integer (10.0 * s / Interpolation_Time);
   --  Max at paused end of slew.

   function Pause_Slew_Interpolation_Time (Index : Pause_Slew_Index) return Time;

   function To_Stepper_Position (Pos : Position; Map : Stepper_Pos_Map) return Stepper_Position;

end Prunt.Step_Generator.Generator;
