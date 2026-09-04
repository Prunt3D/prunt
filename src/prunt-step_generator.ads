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

with System.Multiprocessors;
with Prunt.Motion_Planner.Planner;

generic
   with package Planner is new Motion_Planner.Planner (<>);
   with package Pause_Planner is new Motion_Planner.Planner (<>);

   type Motor_Name is (<>);

   type Motor_Position is array (Motor_Name) of Dimensionless;

   type Kinematic_Transform is private;

   with procedure Setup_Loop_Move (Resetting_Data : Planner.Flush_Resetting_Data_Type);
   --  Configure the hardware immediately before the repeated command is enqueued.

   with
     function Pin_Motor_To_Block_Start
       (Resetting_Data : Planner.Flush_Resetting_Data_Type; Transform : Kinematic_Transform; Motor : Motor_Name)
        return Boolean;
   --  Return True when the step generator must override Motor's transformed positions and hold it at its block-start
   --  position throughout the primary-planner block.

   with function Transform_To_Motor_Position (Pos : Position; Transform : Kinematic_Transform) return Motor_Position;
   with
     function Transform_Motor_Affects_Axis
       (Transform : Kinematic_Transform; Motor : Motor_Name; Axis : Axis_Name) return Boolean;

   type Motor_Delta_Limits is array (Motor_Name) of Dimensionless;

   Maximum_Deltas_Per_Command : Motor_Delta_Limits;

   with
     procedure Start_Planner_Block
       (Resetting_Data : Planner.Flush_Resetting_Data_Type; Last_Command_Index : Command_Index);

   with
     procedure Start_Pause_Planner_Block
       (Resetting_Data : Pause_Planner.Flush_Resetting_Data_Type; Last_Command_Index : Command_Index);

   with
     procedure Enqueue_Command
       (Pos             : Position;
        Motor_Pos       : Motor_Position;
        Index           : Command_Index;
        Safe_Stop_After : Boolean;
        Vel_Ratio       : Dimensionless);

   with procedure Start_Corner (Last_Command_Index : Command_Index; Data : Planner.Corner_Extra_Data_Type);

   with procedure Start_Pause_Corner (Last_Command_Index : Command_Index; Data : Pause_Planner.Corner_Extra_Data_Type);
   --  Called for each extra data element after the final stepgen command reaching that corner has been queued. For the
   --  first corner in a block, this happens at block start because that corner is already reached.
   --
   --  This is not included in Enqueue_Command as floating point inaccuracy could potentially cause a very short
   --  segment to not contain any command even though segments have a minimum length.

   with
     procedure Finish_Planner_Block
       (Resetting_Data     : Planner.Flush_Resetting_Data_Type;
        Next_Block_Pos     : Motor_Position;
        Last_Command_Index : Command_Index);

   with
     procedure Finish_Pause_Planner_Block
       (Resetting_Data     : Pause_Planner.Flush_Resetting_Data_Type;
        Next_Block_Pos     : Motor_Position;
        Last_Command_Index : Command_Index);

   with function Is_Pause_Plan_Done (Resetting_Data : Pause_Planner.Flush_Resetting_Data_Type) return Boolean;

   with procedure Handle_Pause (Pause_Position : Position; Last_Command_Index : Command_Index);

   with procedure Handle_Resume (Pause_Position : Position; Last_Command_Index : Command_Index);

   with procedure Wait_Until_Idle (Last_Command_Index : Command_Index);
   --  Block until the hardware has executed through Last_Command_Index.

   pragma Warnings (Off, "formal object ""Interpolation_Time"" is not referenced");
   Interpolation_Time : Time;
   pragma Warnings (On, "formal object ""Interpolation_Time"" is not referenced");

   Runner_CPU : System.Multiprocessors.CPU_Range;
package Prunt.Step_Generator is
   use Planner;

   task Runner
     with
       CPU          => Runner_CPU,
       Storage_Size => 32 * 1024 * 1024
       --  Allows for very large shapers and shaper buffers to be allocated.
   is
      entry Setup (Transform : Kinematic_Transform);
      --  Configure the step generator with the motor transform. This must be called before any steps can be
      --  generated.
   end Runner;

   procedure Reset;
   --  Reset the step generator state. This should be called when the machine is disabled or reset.

   procedure Soft_Halt;
   --  Slew to a safe stop, wait for the hardware to execute the stop, and reset the step generator. This blocks until
   --  the halt is complete.

   procedure Pause;
   --  Request the step generator to pause execution. This call is non-blocking. Use Is_Paused to check if the pause
   --  has taken effect.
   procedure Resume;
   --  Request the step generator to resume execution from a paused state. Ignored while a soft halt is active.
   function Is_Paused return Boolean;
   --  Returns True if the step generator is currently fully paused.

   function Get_Last_Executed_Primary_Corner_ID return Planner_Corner_ID;
   --  Returns the last primary planner corner whose final stepgen command has been queued.

private

   type Motor_Pin_Selection is array (Motor_Name) of Boolean;

   type Command_State is record
      Current_Command_Index : Command_Index := 0;
      Last_Queued_Position  : Position := [others => 0.0 * mm];
   end record;
   --  Tracks the shared command-index stream and last queued position across primary and pause execution.

   type Catch_Up_Axis_Set is array (Axis_Name) of Boolean;

   procedure Queue_Command
     (State              : in out Command_State;
      Pos                : Position;
      Transform          : Kinematic_Transform;
      Safe_Stop_After    : Boolean;
      Vel_Ratio          : Dimensionless;
      Catch_Up_Axes      : Catch_Up_Axis_Set;
      Pin_To_Block_Start : Motor_Pin_Selection;
      Stationary_Pos     : Motor_Position);
   --  Append one shaped command to the shared step queue and update State.

   function No_Pause_Requested return Boolean;
   --  Default pause-policy hook that always reports no pending pause request.

   procedure No_Pause_Handler (Pause_Position : Position; Reset_Requested : out Boolean);
   --  Default pause handler that performs no motion and reports that no reset was requested.

   type Axis_Fractions is array (Axis_Name) of Dimensionless;

   function Command_Fractions
     (Start_Pos, Target_Pos : Position; Transform : Kinematic_Transform; Catch_Up_Axes : Catch_Up_Axis_Set)
      return Axis_Fractions;
   --  Return per-axis fractions that keep pressure-advance catch-up within every motor's per-command delta limit.

end Prunt.Step_Generator;
