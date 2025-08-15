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

with Ada.Containers; use Ada.Containers;

private generic
package Prunt.Motion_Planner.Planner.Preprocessor is

   procedure Setup (Initial_Parameters : Kinematic_Parameters);

   procedure Enqueue (Comm : Command; Ignore_Bounds : Boolean := False);

   procedure Reset;
   --  Cause Run to immediately return with Reset_Called set to True and resets the planner back to its initial state.

   procedure Run (Block : aliased out Execution_Block; Reset_Called : out Boolean);

private

   type Command_Queue_Array_Type is array (1 .. Input_Queue_Length) of Command;

   protected Command_Queue is
      procedure Setup (Initial_Parameters : Kinematic_Parameters);
      entry Enqueue (Comm : Command; Ignore_Bounds : Boolean := False);
      entry Dequeue (Comm : out Command; Reset_Called : out Boolean);
      --  Reset_Called should not be part of Dequeue, but GNAT does not seem to work correctly when using a
      --  select-then-abort with two entries to the same object, as you would with a separate Enter_When_Reset entry.
      procedure Reset;
   private
      Setup_Done            : Boolean := False;
      Is_Full               : Boolean := False;
      Next_Read, Next_Write : Count_Type := Command_Queue_Array_Type'First;
      Elements              : Command_Queue_Array_Type;
      Current_Params        : Kinematic_Parameters;
   end Command_Queue;

   protected Runner is
      procedure Setup (Initial_Parameters : Kinematic_Parameters);
      procedure Run (Block : aliased out Execution_Block; Reset_Called : out Boolean);
      procedure Reset;
   private
      Setup_Done            : Boolean := False;
      Last_Pos              : Position := Initial_Position;
      Current_Params        : Kinematic_Parameters;
      Block_Persistent_Data : Block_Persistent_Data_Type := Block_Persistent_Data_Default;
      pragma Warnings (Off, "use of an anonymous access type allocator");
      Corners               : access Block_Plain_Corners := new Block_Plain_Corners (1 .. Corners_Index'Last);
      Corner_Dwell_Times    : access Block_Corner_Dwell_Times :=
        new Block_Corner_Dwell_Times (1 .. Corners_Index'Last);
      Segment_Feedrates     : access Block_Segment_Feedrates := new Block_Segment_Feedrates (2 .. Corners_Index'Last);
      Corners_Extra_Data    : access Block_Corners_Extra_Data :=
        new Block_Corners_Extra_Data (2 .. Corners_Index'Last);
      pragma Warnings (On, "use of an anonymous access type allocator");
   end Runner;

   procedure Check_Bounds (Pos : Position; Params : Kinematic_Parameters);

   function Limit_Higher_Order_Params (Params : Kinematic_Parameters) return Kinematic_Parameters;
   --  Limit the higher order kinematic limits to those reachable within a single interpolation period. This may be
   --  useful if the user chooses to enter an extremely large value.

end Prunt.Motion_Planner.Planner.Preprocessor;
