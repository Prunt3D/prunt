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

   Do_Pause : Boolean := False
   with Atomic, Volatile;
   Paused   : Boolean := False
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

   procedure Pause is
   begin
      Do_Pause := True;
   end Pause;

   procedure Resume is
   begin
      Do_Pause := False;
   end Resume;

   function Is_Paused return Boolean is
   begin
      return Paused;
   end Is_Paused;

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

   procedure Queue_Command
     (State           : in out Command_State;
      Pos             : Position;
      Map             : Motor_Pos_Map;
      Loop_Until_Hit  : Boolean;
      Safe_Stop_After : Boolean;
      Vel_Ratio       : Dimensionless) is
   begin
      State.Current_Command_Index := @ + 1;
      State.Last_Queued_Position := Pos;
      Enqueue_Command
        (Pos             => Pos,
         Motor_Pos       => To_Motor_Position (Pos, Map),
         Index           => State.Current_Command_Index,
         Loop_Until_Hit  => Loop_Until_Hit,
         Safe_Stop_After => Safe_Stop_After,
         Vel_Ratio       => Vel_Ratio);
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

      procedure Log_Primary_Waiting_For_Step_Rate_Limiter;

      procedure Log_Pause_Waiting_For_Step_Rate_Limiter;

      function Primary_Pause_Requested return Boolean;

      procedure Check_Reset (Reset_Requested : out Boolean) is
      begin
         Reset_Requested := Reset_Control.Requested;
      end Check_Reset;

      procedure Log_Primary_Waiting_For_Step_Rate_Limiter is
      begin
         Log
           ("The step command generator is waiting for the step rate limiter to complete. This can take "
            & "a long time if the G-code contains multiple very long moves. In a future version this "
            & "will be improved.");
      end Log_Primary_Waiting_For_Step_Rate_Limiter;

      procedure Log_Pause_Waiting_For_Step_Rate_Limiter is
      begin
         Log
           ("The pause step command generator is waiting for the step rate limiter to complete. This can "
            & "take a long time if the pause plan contains multiple very long moves. In a future version "
            & "this will be improved.");
      end Log_Pause_Waiting_For_Step_Rate_Limiter;

      function Primary_Pause_Requested return Boolean is
      begin
         return Do_Pause;
      end Primary_Pause_Requested;

      package Primary_Block_Executor is new
        Block_Executor
          (Active_Planner            => Planner,
           Allow_Homing              => True,
           Check_Reset               => Check_Reset,
           Step_Rate_Limiter_Stalled => Log_Primary_Waiting_For_Step_Rate_Limiter,
           Start_Block_Callback      => Start_Planner_Block,
           Start_Corner_Callback     => Start_Corner,
           Finish_Block_Callback     => Finish_Planner_Block,
           Pause_Requested           => Primary_Pause_Requested,
           Handle_Pause              => Run_Pause_Cycle);

      package Pause_Block_Executor is new
        Block_Executor
          (Active_Planner            => Pause_Planner,
           Allow_Homing              => False,
           Check_Reset               => Check_Reset,
           Step_Rate_Limiter_Stalled => Log_Pause_Waiting_For_Step_Rate_Limiter,
           Start_Block_Callback      => Start_Pause_Planner_Block,
           Start_Corner_Callback     => Start_Pause_Corner,
           Finish_Block_Callback     => Finish_Pause_Planner_Block);

      procedure Run_Pause_Cycle (Pause_Position : Position; Reset_Requested : out Boolean) is
         procedure Execute_Pause_Plan (Resume_Plan : Boolean);
         procedure Wait_For_Resume;

         procedure Execute_Pause_Plan (Resume_Plan : Boolean) is
         begin
            if Resume_Plan then
               Handle_Resume (Pause_Position, Commands.Current_Command_Index);
            else
               Handle_Pause (Pause_Position, Commands.Current_Command_Index);
            end if;

            loop
               Pause_Block_Executor.Dequeue_Block (Pause_Block, Commands, Reset_Requested);
               if Reset_Requested then
                  return;
               end if;
               Pause_Block_Executor.Execute_Block (Pause_Block, Pos_Map, Commands, Reset_Requested);
               if Reset_Requested then
                  return;
               end if;
               exit when Is_Pause_Plan_Done (Pause_Planner.Flush_Resetting_Data (Pause_Block));
            end loop;
         end Execute_Pause_Plan;

         procedure Wait_For_Resume is
         begin
            loop
               Check_Reset (Reset_Requested);
               if Reset_Requested then
                  return;
               end if;

               exit when not Do_Pause;

               delay 0.1;
            end loop;
         end Wait_For_Resume;
      begin
         Reset_Requested := False;
         Paused := True;
         Execute_Pause_Plan (Resume_Plan => False);
         if Reset_Requested then
            return;
         end if;

         Wait_For_Resume;
         if Reset_Requested then
            return;
         end if;

         Execute_Pause_Plan (Resume_Plan => True);
         if Reset_Requested then
            return;
         end if;

         if Commands.Last_Queued_Position /= Pause_Position then
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

               Primary_Block_Executor.Execute_Block (Block, Pos_Map, Commands, Reset_Requested);
               exit Main when Reset_Requested;
            end;
         end loop Main;
      end loop;
   end Runner;

end Prunt.Step_Generator;
