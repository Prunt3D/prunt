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

with Ada.Containers; use Ada.Containers;

private with System.Pool_Local;
private with Prunt.Bounded_Indefinite_Queues;

private generic
package Prunt.Motion_Planner.Planner.Preprocessor is

   procedure Setup (Initial_Parameters : Kinematic_Parameters);
   --  Initialise the preprocessor with the initial kinematic parameters. This must be called before any other
   --  operations.

   procedure Enqueue
     (Comm : Command; Ignore_Bounds : Boolean := False; Extra : access constant Corner_Extra_Data_Type := null);
   --  Add a new command to the processing queue. Commands are processed in FIFO order. If Ignore_Bounds is True,
   --  position bounds checking is bypassed for this command (useful for homing operations). May block if the queue is
   --  full.

   procedure Reset;
   --  Cause Run to immediately return with Reset_Called set to True and resets the planner back to its initial
   --  state. This clears the command queue and resets position tracking to the initial values.

   procedure Run (Block : aliased out Execution_Block; Reset_Called : out Boolean);
   --  Process queued commands and assemble them into an execution block. This procedure blocks until either a complete
   --  block is assembled or a reset is requested. Reset_Called indicates whether the operation was terminated by a
   --  reset request.

private

   type Command_Queue_Array_Type is array (1 .. Input_Queue_Length) of Command;

   package Corner_Extra_Data_Queues is new
     Bounded_Indefinite_Queues
       (Element_Type => Corner_Extra_Data_Type,
        Storage_Size => Max_Corners_Extra_Data_Storage);

   protected Command_Queue is
      procedure Setup (Initial_Parameters : Kinematic_Parameters);
      entry Enqueue
        (Comm : Command; Ignore_Bounds : Boolean := False; Extra : access constant Corner_Extra_Data_Type := null)
      with Pre => (if Comm.Kind = Corner_Extra_Data_Kind then Extra /= null else Extra = null);
      entry High_Priority_Enqueue
        (Comm : Command; Ignore_Bounds : Boolean := False; Extra : access constant Corner_Extra_Data_Type := null)
      with Pre => (if Comm.Kind = Corner_Extra_Data_Kind then Extra /= null else Extra = null);
      entry Dequeue (Comm : out Command; Reset_Called : out Boolean);
      function Dequeue_Extra_Data return Corner_Extra_Data_Type;
      procedure Finish_Dequeue;
      procedure Cancel_Dequeue;
      procedure Reset;
   private
      procedure Append_To_Queue (Comm : Command);

      Setup_Done            : Boolean := False;
      In_Dequeue            : Boolean := False;
      Is_Full               : Boolean := False;
      Next_Read, Next_Write : Count_Type := Command_Queue_Array_Type'First;
      Elements              : Command_Queue_Array_Type;
      Current_Params        : Kinematic_Parameters;
      Extra_Data_Storage    : Corner_Extra_Data_Queues.Queue;
      Retry_High_Priority   : Boolean := True;
   end Command_Queue;

   Pool : System.Pool_Local.Unbounded_Reclaim_Pool;

   type Block_Plain_Corners_Access is access Block_Plain_Corners with Storage_Pool => Pool;
   type Block_Corner_Dwell_Times_Access is access Block_Corner_Dwell_Times with Storage_Pool => Pool;
   type Block_Segment_Feedrates_Access is access Block_Segment_Feedrates with Storage_Pool => Pool;
   type Block_Corners_Extra_Data_Access is access Corner_Extra_Data_Vectors.Vector with Storage_Pool => Pool;
   type Block_Corners_Extra_Data_End_Indices_Access is access Block_Corners_Extra_Data_End_Indices
   with Storage_Pool => Pool;

   protected Runner is
      procedure Setup (Initial_Parameters : Kinematic_Parameters);
      procedure Run (Block : aliased out Execution_Block; Reset_Called : out Boolean);
      procedure Reset;
   private
      Setup_Done                     : Boolean := False;
      Last_Pos                       : Position := Initial_Position;
      Current_Params                 : Kinematic_Parameters;
      Corners                        : Block_Plain_Corners_Access := new Block_Plain_Corners (1 .. Corners_Index'Last);
      Corner_Dwell_Times             : Block_Corner_Dwell_Times_Access :=
        new Block_Corner_Dwell_Times (2 .. Corners_Index'Last);
      Segment_Feedrates              : Block_Segment_Feedrates_Access :=
        new Block_Segment_Feedrates (2 .. Corners_Index'Last);
      Corners_Extra_Data             : Block_Corners_Extra_Data_Access := new Corner_Extra_Data_Vectors.Vector;
      Corners_Extra_Data_End_Indices : Block_Corners_Extra_Data_End_Indices_Access :=
        new Block_Corners_Extra_Data_End_Indices (1 .. Corners_Index'Last);
   end Runner;

   procedure Check_Bounds (Pos : Position; Params : Kinematic_Parameters);
   --  Check if a given position is within the machine's boundaries defined in Params. Raises Out_Of_Bounds_Error
   --  if the check fails.

   function Limit_Higher_Order_Params (Params : Kinematic_Parameters) return Kinematic_Parameters;
   --  Limit the higher order kinematic limits to those reachable within a single interpolation period. This may be
   --  useful if the user chooses to enter an extremely large value.

end Prunt.Motion_Planner.Planner.Preprocessor;
