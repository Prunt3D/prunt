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

private with Ada.Containers.Synchronized_Queue_Interfaces;
private with Ada.Containers.Bounded_Synchronized_Queues;

private generic
package Prunt.Motion_Planner.Planner.Preprocessor is

   procedure Setup (Initial_Parameters : Kinematic_Parameters);

   procedure Enqueue (Comm : Command; Ignore_Bounds : Boolean := False);

   procedure Run (Block : aliased out Execution_Block);

private

   pragma Warnings (Off, "use of an anonymous access type allocator");

   package Command_Queues_Interface is new Ada.Containers.Synchronized_Queue_Interfaces (Command);
   package Command_Queues is new Ada.Containers.Bounded_Synchronized_Queues
     (Command_Queues_Interface, Input_Queue_Length);

   Command_Queue : access Command_Queues.Queue := new Command_Queues.Queue;

   Params   : Kinematic_Parameters := (others => <>);
   Last_Pos : Position             := [others => Length (0.0)];

   Setup_Done : Boolean := False;

   Corners            : access Block_Plain_Corners      := new Block_Plain_Corners (1 .. Corners_Index'Last);
   Segment_Feedrates  : access Block_Segment_Feedrates  := new Block_Segment_Feedrates (2 .. Corners_Index'Last);
   Corners_Extra_Data : access Block_Corners_Extra_Data := new Block_Corners_Extra_Data (2 .. Corners_Index'Last);

   pragma Warnings (On, "use of an anonymous access type allocator");

   procedure Check_Bounds (Pos : Position);

end Prunt.Motion_Planner.Planner.Preprocessor;
