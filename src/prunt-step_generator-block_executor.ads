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

with Prunt.Motion_Planner.Planner;

private generic
   with package Active_Planner is new Motion_Planner.Planner (<>);

   Allow_Homing : Boolean := False;
   --  Whether this executor may run planner homing blocks. Pause-plan executors should leave this False.

   with procedure Check_Reset (Reset_Requested : out Boolean);
   --  Report whether the step generator has been asked to reset.

   with procedure Step_Rate_Limiter_Stalled;
   --  Called during dequeue timeouts while the step-rate limiter is preventing a block from becoming ready.

   with
     procedure Start_Block_Callback
       (Resetting_Data : Active_Planner.Flush_Resetting_Data_Type; Last_Command_Index : Command_Index);
   --  Runs immediately before the block's first corner data is processed.

   with
     procedure Start_Corner_Callback
       (Last_Command_Index : Command_Index; Data : Active_Planner.Corner_Extra_Data_Type);
   --  Handles corner extra data using the command index current after that corner's final stepgen command is queued.

   with
     procedure Finish_Block_Callback
       (Resetting_Data       : Active_Planner.Flush_Resetting_Data_Type;
        Next_Block_Pos       : Motor_Position;
        First_Accel_Distance : Length;
        Last_Command_Index   : Command_Index;
        Loop_Move_Offset     : Position_Offset);
   --  Runs after all commands for the block have been queued.

   with procedure Publish_Corner_ID (Corner_ID : Planner_Corner_ID);
   --  Publishes a corner after any motion and extra data associated with it has been queued or processed.

   with function Pause_Requested return Boolean is No_Pause_Requested;
   --  True when execution should slew to a stopped pause point.

   with procedure Handle_Pause (Pause_Position : Position; Reset_Requested : out Boolean) is No_Pause_Handler;
   --  Runs once the executor has reached a stopped position and drained the input shapers.

package Prunt.Step_Generator.Block_Executor
is

   procedure Dequeue_Block
     (Block : out Active_Planner.Execution_Block; Commands : in out Command_State; Reset_Requested : out Boolean);
   --  Wait for a planner block to become available while polling reset and between-block policy hooks.

   procedure Execute_Block
     (Block           : Active_Planner.Execution_Block;
      Map             : Motor_Pos_Map;
      Commands        : in out Command_State;
      Reset_Requested : out Boolean);
   --  Queue all step-generator commands for Block unless reset is requested.

end Prunt.Step_Generator.Block_Executor;
