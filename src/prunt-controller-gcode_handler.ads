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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

private generic
package Prunt.Controller.Gcode_Handler is

   procedure Try_Set_File (Path : String; Succeeded : out Boolean);
   --  Try to enqueue a file to be executed. Succeeded will be set False if there is already a file running.

   procedure Try_Queue_Command (Command : String; Succeeded : out Boolean);
   --  Try to enqueue a command to be executed. Succeeded will be set False if there is already a file or command
   --  running.

   procedure Finished_Block (Data : Flush_Extra_Data; First_Segment_Accel_Distance : Length);
   --  Report that a block has finished executing. First_Segment_Accel_Distance should be set to the distance of the
   --  first acceleration profile, this is used to determine at what point a homing loop move started in homing blocks.

   task Runner is
      entry Start (Initial_Data : Corner_Extra_Data);
   end Runner;

private

   protected Gcode_Queue is
      --  TODO: Currently we use a delay loop for getting files and commands. We should use entries here instead.
      procedure Try_Set_File (In_File : String; Succeeded : out Boolean);
      procedure Clear_File;
      function Get_File return String;
      procedure Try_Set_Command (In_Command : String; Succeeded : out Boolean);
      procedure Clear_Command;
      function Get_Command return String;
   private
      File    : Unbounded_String := To_Unbounded_String ("");
      Command : Unbounded_String := To_Unbounded_String ("");
   end Gcode_Queue;

   protected Finished_Block_Queue is
      procedure Push (In_Data : Flush_Extra_Data; In_First_Segment_Accel_Distance : Length);
      entry Pop (Out_Data : out Flush_Extra_Data; Out_First_Segment_Accel_Distance : out Length);
   private
      Has_Item                     : Boolean := False;
      Data                         : Flush_Extra_Data;
      First_Segment_Accel_Distance : Length;
   end Finished_Block_Queue;

end Prunt.Controller.Gcode_Handler;
