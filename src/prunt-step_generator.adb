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

with Prunt.Step_Generator.Block_Executor;
with System.Pool_Local;

package body Prunt.Step_Generator is

   pragma Extensions_Allowed (On);

   --  Keep rounded PA catch-up commands below the exact hardware limit.
   Catch_Up_Numerical_Safety_Factor : constant Dimensionless := 0.999;

   protected Reset_Control is
      procedure Request;
      --  Request that the runner stop at the next reset check. If the runner is idle, acknowledge immediately.

      function Requested return Boolean;
      --  True while an active runner has not yet acknowledged the current reset request.

      procedure Mark_Running;
      --  Mark the runner as active after setup has completed.

      procedure Acknowledge;
      --  Mark the runner as stopped and clear any pending reset request.

      entry Wait_For_Acknowledgement;
      --  Block until Request has been acknowledged or there was no active runner to reset.

   private
      Reset_Requested    : Boolean := False;
      Reset_Acknowledged : Boolean := True;
      Runner_Running     : Boolean := False;
   end Reset_Control;

   protected body Reset_Control is
      procedure Request is
      begin
         if Runner_Running then
            Reset_Requested := True;
            Reset_Acknowledged := False;
         else
            Reset_Requested := False;
            Reset_Acknowledged := True;
         end if;
      end Request;

      function Requested return Boolean is
      begin
         return Reset_Requested;
      end Requested;

      procedure Mark_Running is
      begin
         Runner_Running := True;
         Reset_Requested := False;
         Reset_Acknowledged := True;
      end Mark_Running;

      procedure Acknowledge is
      begin
         Runner_Running := False;
         Reset_Requested := False;
         Reset_Acknowledged := True;
      end Acknowledge;

      entry Wait_For_Acknowledgement when Reset_Acknowledged is
      begin
         null;
      end Wait_For_Acknowledgement;
   end Reset_Control;

   protected Halt_Control is
      procedure Request;
      procedure Acknowledge;
      entry Wait_For_Acknowledgement;
   private
      Halt_Acknowledged : Boolean := False;
   end Halt_Control;

   protected body Halt_Control is
      procedure Request is
      begin
         Halt_Acknowledged := False;
      end Request;

      procedure Acknowledge is
      begin
         Halt_Acknowledged := True;
      end Acknowledge;

      entry Wait_For_Acknowledgement when Halt_Acknowledged is
      begin
         null;
      end Wait_For_Acknowledgement;
   end Halt_Control;

   Do_Pause : Boolean := False
   with Atomic, Volatile;
   Do_Halt  : Boolean := False
   with Atomic, Volatile;
   Paused   : Boolean := False
   with Atomic, Volatile;

   Last_Executed_Primary_Corner_ID : Planner_Corner_ID := 0
   with Atomic, Volatile;

   procedure Reset is
   begin
      Reset_Control.Request;

      loop
         select
            Reset_Control.Wait_For_Acknowledgement;
            exit;
         or
            delay 0.1;

            if Runner'Terminated then
               Reset_Control.Acknowledge;
               exit;
            end if;
         end select;
      end loop;
   end Reset;

   procedure Soft_Halt is
   begin
      Halt_Control.Request;
      Do_Halt := True;
      Do_Pause := True;

      loop
         select
            Halt_Control.Wait_For_Acknowledgement;
            exit;
         or
            delay 0.1;

            if Runner'Terminated then
               exit;
            end if;
         end select;
      end loop;

      Reset;
   end Soft_Halt;

   procedure Pause is
   begin
      Do_Pause := True;
   end Pause;

   procedure Resume is
   begin
      if not Do_Halt then
         Do_Pause := False;
      end if;
   end Resume;

   function Is_Paused return Boolean is
   begin
      return Paused;
   end Is_Paused;

   function Get_Last_Executed_Primary_Corner_ID return Planner_Corner_ID is
   begin
      return Last_Executed_Primary_Corner_ID;
   end Get_Last_Executed_Primary_Corner_ID;

   function To_Motor_Position (Pos : Position; Map : Motor_Pos_Map) return Motor_Position is
      Ret : Motor_Position := [others => 0.0];
   begin
      for M in Motor_Name loop
         for A in Axis_Name loop
            --  TODO: Use multiplication for the map instead of division so we don't need this check.
            if Map (A, M) /= Length'Last then
               Ret (M) := Ret (M) + Pos (A) / Map (A, M);
            end if;
         end loop;
      end loop;

      return Ret;
   end To_Motor_Position;

   function Command_Fractions
     (Start_Pos, Target_Pos : Position; Map : Motor_Pos_Map; Catch_Up_Axes : Catch_Up_Axis_Set) return Axis_Fractions
   is
      Start_Motor_Pos  : constant Motor_Position := To_Motor_Position (Start_Pos, Map);
      Target_Motor_Pos : constant Motor_Position := To_Motor_Position (Target_Pos, Map);
      Result           : Axis_Fractions := [others => 1.0];
   begin
      for Motor in Motor_Name loop
         declare
            Motor_Delta       : constant Dimensionless := Target_Motor_Pos (Motor) - Start_Motor_Pos (Motor);
            Limit             : constant Dimensionless := abs Maximum_Deltas_Per_Command (Motor);
            Safe_Limit        : constant Dimensionless := Catch_Up_Numerical_Safety_Factor * Limit;
            Catch_Up_Axis     : Axis_Name := Axis_Name'First;
            Has_Catch_Up_Axis : Boolean := False;
         begin
            for Axis in Axis_Name loop
               if Map (Axis, Motor) /= Length'Last then
                  if Catch_Up_Axes (Axis) then
                     Catch_Up_Axis := Axis;
                     Has_Catch_Up_Axis := True;
                  end if;
               end if;
            end loop;

            if abs Motor_Delta > Limit then
               if not Has_Catch_Up_Axis then
                  raise Constraint_Error with "Maximum_Delta_Per_Command exceeded without pressure advance catch-up.";
               elsif Limit <= 0.0 then
                  Result (Catch_Up_Axis) := 0.0;
               else
                  Result (Catch_Up_Axis) := Dimensionless'Min (Result (Catch_Up_Axis), Safe_Limit / abs Motor_Delta);
               end if;
            end if;
         end;
      end loop;

      return Result;
   end Command_Fractions;

   procedure Queue_Command
     (State           : in out Command_State;
      Pos             : Position;
      Map             : Motor_Pos_Map;
      Loop_Until_Hit  : Boolean;
      Safe_Stop_After : Boolean;
      Vel_Ratio       : Dimensionless;
      Catch_Up_Axes   : Catch_Up_Axis_Set := [others => False])
   is
      procedure Emit (Emit_Pos : Position; Emit_Safe_Stop_After : Boolean);

      procedure Emit (Emit_Pos : Position; Emit_Safe_Stop_After : Boolean) is
      begin
         State.Current_Command_Index := @ + 1;
         State.Last_Queued_Position := Emit_Pos;
         Enqueue_Command
           (Pos             => Emit_Pos,
            Motor_Pos       => To_Motor_Position (Emit_Pos, Map),
            Index           => State.Current_Command_Index,
            Loop_Until_Hit  => Loop_Until_Hit,
            Safe_Stop_After => Emit_Safe_Stop_After,
            Vel_Ratio       => Vel_Ratio);
      end Emit;

      Target_Pos : constant Position := Pos;
   begin
      loop
         declare
            Fractions : constant Axis_Fractions :=
              Command_Fractions
                (Start_Pos     => State.Last_Queued_Position,
                 Target_Pos    => Target_Pos,
                 Map           => Map,
                 Catch_Up_Axes => Catch_Up_Axes);
            Complete  : Boolean := True;
         begin
            for Axis in Axis_Name loop
               if Fractions (Axis) < 1.0 then
                  Complete := False;
                  if Fractions (Axis) <= 0.0 then
                     raise Constraint_Error with "Maximum_Delta_Per_Command catch-up can not make progress.";
                  end if;
               end if;
            end loop;

            if Complete then
               Emit (Target_Pos, Safe_Stop_After);
               exit;
            end if;

            declare
               Intermediate_Pos : Position := State.Last_Queued_Position;
            begin
               for Axis in Axis_Name loop
                  Intermediate_Pos (Axis) :=
                    State.Last_Queued_Position (Axis)
                    + (Target_Pos (Axis) - State.Last_Queued_Position (Axis)) * Fractions (Axis);
               end loop;

               if Intermediate_Pos = State.Last_Queued_Position then
                  raise Constraint_Error with "Maximum_Delta_Per_Command catch-up stalled.";
               end if;

               Emit (Intermediate_Pos, False);
            end;

            exit when not Safe_Stop_After;
         end;
      end loop;
   end Queue_Command;

   function No_Pause_Requested return Boolean is
   begin
      return False;
   end No_Pause_Requested;

   procedure No_Pause_Handler (Pause_Position : Position; Reset_Requested : out Boolean) is
      pragma Unreferenced (Pause_Position);
   begin
      Reset_Requested := False;
   end No_Pause_Handler;

   task body Runner is
      Commands : Command_State;
      Pos_Map  : Motor_Pos_Map;

      type Block_Wrapper is record
         Block : aliased Execution_Block;
      end record;

      type Pause_Block_Wrapper is record
         Block : aliased Pause_Planner.Execution_Block;
      end record;

      Pool : System.Pool_Local.Unbounded_Reclaim_Pool;

      type Block_Wrapper_Access is access Block_Wrapper with Storage_Pool => Pool;
      type Pause_Block_Wrapper_Access is access Pause_Block_Wrapper with Storage_Pool => Pool;

      Working_Block_Wrapper       : constant Block_Wrapper_Access := new Block_Wrapper;
      Block renames Working_Block_Wrapper.Block;
      Working_Pause_Block_Wrapper : constant Pause_Block_Wrapper_Access := new Pause_Block_Wrapper;
      Pause_Block renames Working_Pause_Block_Wrapper.Block;

      Zero_Length : constant Length := 0.0 * mm;

      procedure Check_Reset (Reset_Requested : out Boolean);

      procedure Run_Pause_Cycle (Pause_Position : Position; Reset_Requested : out Boolean);

      function Primary_Pause_Requested return Boolean;

      procedure Publish_Primary_Corner_ID (Corner_ID : Planner_Corner_ID);

      procedure Ignore_Corner_ID_Publication (Corner_ID : Planner_Corner_ID);

      procedure Check_Reset (Reset_Requested : out Boolean) is
      begin
         Reset_Requested := Reset_Control.Requested;
      end Check_Reset;

      function Primary_Pause_Requested return Boolean is
      begin
         return Do_Pause or else Do_Halt;
      end Primary_Pause_Requested;

      procedure Publish_Primary_Corner_ID (Corner_ID : Planner_Corner_ID) is
         Last_Executed : constant Planner_Corner_ID := Last_Executed_Primary_Corner_ID;
      begin
         if Corner_ID < Last_Executed then
            raise Constraint_Error
              with "Corner ID publication moved backwards from" & Last_Executed'Image & " to" & Corner_ID'Image & ".";
         end if;

         Last_Executed_Primary_Corner_ID := Corner_ID;
      end Publish_Primary_Corner_ID;

      procedure Ignore_Corner_ID_Publication (Corner_ID : Planner_Corner_ID) is
         pragma Unreferenced (Corner_ID);
      begin
         null;
      end Ignore_Corner_ID_Publication;

      package Primary_Block_Executor is new
        Block_Executor
          (Active_Planner        => Planner,
           Allow_Homing          => True,
           Check_Reset           => Check_Reset,
           Start_Block_Callback  => Start_Planner_Block,
           Start_Corner_Callback => Start_Corner,
           Finish_Block_Callback => Finish_Planner_Block,
           Publish_Corner_ID     => Publish_Primary_Corner_ID,
           Pause_Requested       => Primary_Pause_Requested,
           Handle_Pause          => Run_Pause_Cycle);

      package Pause_Block_Executor is new
        Block_Executor
          (Active_Planner        => Pause_Planner,
           Allow_Homing          => False,
           Check_Reset           => Check_Reset,
           Start_Block_Callback  => Start_Pause_Planner_Block,
           Start_Corner_Callback => Start_Pause_Corner,
           Finish_Block_Callback => Finish_Pause_Planner_Block,
           Publish_Corner_ID     => Ignore_Corner_ID_Publication);

      procedure Run_Pause_Cycle (Pause_Position : Position; Reset_Requested : out Boolean) is
         Stable_Pause_Position : constant Position := Pause_Position;
         --  Implicit pass-by-reference was a mistake.

         procedure Enter_Halted_State;
         procedure Execute_Pause_Plan (Resume_Plan : Boolean);
         procedure Wait_For_Resume;

         procedure Enter_Halted_State is
         begin
            Wait_Until_Idle (Commands.Current_Command_Index);
            Halt_Control.Acknowledge;

            loop
               Check_Reset (Reset_Requested);
               exit when Reset_Requested;
               delay 0.1;
            end loop;
         end Enter_Halted_State;

         procedure Execute_Pause_Plan (Resume_Plan : Boolean) is
         begin
            if Resume_Plan then
               Handle_Resume (Stable_Pause_Position, Commands.Current_Command_Index);
            else
               Handle_Pause (Stable_Pause_Position, Commands.Current_Command_Index);
            end if;

            loop
               Pause_Block_Executor.Dequeue_Block (Pause_Block, Commands, Reset_Requested);
               if Reset_Requested then
                  return;
               end if;
               Pause_Block_Executor.Execute_Block (Pause_Block'Access, Pos_Map, Commands, Reset_Requested);
               if Reset_Requested then
                  return;
               end if;
               exit when Is_Pause_Plan_Done (Pause_Planner.Flush_Resetting_Data (Pause_Block'Access));
            end loop;
         end Execute_Pause_Plan;

         procedure Wait_For_Resume is
         begin
            loop
               Check_Reset (Reset_Requested);
               if Reset_Requested then
                  return;
               end if;

               exit when Do_Halt or else not Do_Pause;

               delay 0.1;
            end loop;
         end Wait_For_Resume;
      begin
         Reset_Requested := False;
         Paused := True;

         if Do_Halt then
            Enter_Halted_State;
            return;
         end if;

         Execute_Pause_Plan (Resume_Plan => False);
         if Reset_Requested then
            return;
         end if;

         if Do_Halt then
            Enter_Halted_State;
            return;
         end if;

         Wait_For_Resume;
         if Reset_Requested then
            return;
         end if;

         if Do_Halt then
            Enter_Halted_State;
            return;
         end if;

         Execute_Pause_Plan (Resume_Plan => True);
         if Reset_Requested then
            return;
         end if;

         if Commands.Last_Queued_Position /= Stable_Pause_Position then
            raise Constraint_Error with "Pause resume plan did not return to the pause position.";
         end if;

         Paused := False;
      exception
         when others =>
            Paused := False;
            raise;
      end Run_Pause_Cycle;
   begin
      loop
         Paused := False;
         Do_Pause := False;
         Do_Halt := False;
         Reset_Control.Acknowledge;
         Commands.Last_Queued_Position := [others => Zero_Length];

         accept Setup (Map : Motor_Pos_Map) do
            Pos_Map := Map;
            Reset_Control.Mark_Running;
         end Setup;

         Main : loop
            declare
               Reset_Requested : Boolean;
            begin
               Primary_Block_Executor.Dequeue_Block (Block, Commands, Reset_Requested);
               exit Main when Reset_Requested;

               Primary_Block_Executor.Execute_Block (Block'Access, Pos_Map, Commands, Reset_Requested);
               exit Main when Reset_Requested;
            end;
         end loop Main;
      end loop;
   end Runner;

end Prunt.Step_Generator;
