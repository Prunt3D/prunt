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

with System.Multiprocessors;
with Prunt.Motion_Planner.Planner;
with Prunt.Step_Generator;

package body Prunt.Step_Generator_Restart_Test is

   pragma Extensions_Allowed (On);

   procedure Test_Command_Indices_After_Restart (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         type Motor_Name is (X_Motor);
         type Motor_Position is array (Motor_Name) of Dimensionless;
         type Motor_Position_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Delta_Limits is array (Motor_Name) of Dimensionless;

         package Planner is new Motion_Planner.Planner
           (Motor_Name                        => Motor_Name,
            Motor_Position_Map                => Motor_Position_Map,
            Motor_Delta_Limits                => Motor_Delta_Limits,
            Maximum_Deltas_Per_Command         => [others => 1.0],
            Flush_Resetting_Data_Type         => Boolean,
            Flush_Resetting_Data_Type_Default => False,
            Corner_Extra_Data_Type            => Boolean,
            Home_Move_Minimum_Coast_Time       => 0.005 * s,
            Home_Move_Maximum_Tail_Time        => 1.0 * s,
            Interpolation_Time                => 0.001 * s,
            Max_Corners                       => 10,
            Max_Corners_Extra_Data_Count       => 10,
            Max_Corners_Extra_Data_Storage     => 1_024,
            Input_Queue_Length                => 10);

         package Pause_Planner is new Motion_Planner.Planner
           (Motor_Name                        => Motor_Name,
            Motor_Position_Map                => Motor_Position_Map,
            Motor_Delta_Limits                => Motor_Delta_Limits,
            Maximum_Deltas_Per_Command         => [others => 1.0],
            Flush_Resetting_Data_Type         => Boolean,
            Flush_Resetting_Data_Type_Default => False,
            Corner_Extra_Data_Type            => Boolean,
            Home_Move_Minimum_Coast_Time       => 0.005 * s,
            Home_Move_Maximum_Tail_Time        => 1.0 * s,
            Interpolation_Time                => 0.001 * s,
            Max_Corners                       => 10,
            Max_Corners_Extra_Data_Count       => 10,
            Max_Corners_Extra_Data_Storage     => 1_024,
            Input_Queue_Length                => 10);

         protected Hardware is
            procedure Check_Idle (Index : Command_Index);
            procedure Complete_Block (Index : Command_Index);
            procedure Execute (Index : Command_Index);
            procedure Reset;
            procedure Request_Shutdown;
            procedure Fail_Next_Idle;
            function Accessed_After_Shutdown return Boolean;
            function Last_Command return Command_Index;
            entry Wait_For_Shutdown;
            entry Wait_For_Idle_Failure;
            entry Wait_For_Block (Index : out Command_Index; Invalid_Wait : out Boolean);
         private
            Shut_Down : Boolean := False;
            Unexpected_Access : Boolean := False;
            Fail_Idle : Boolean := False;
            Idle_Failed : Boolean := False;
            Last_Executed              : Command_Index := 0;
            Last_Block                 : Command_Index := 0;
            Block_Complete             : Boolean := False;
            Waited_For_Missing_Command : Boolean := False;
         end Hardware;

         protected body Hardware is
            procedure Check_Idle (Index : Command_Index) is
            begin
               Unexpected_Access := @ or else Shut_Down;
               --  Record the impossible wait instead of hanging the test like real hardware would.
               Waited_For_Missing_Command := @ or else Index > Last_Executed;
               if Fail_Idle then
                  Idle_Failed := True;
                  raise Hardware_Communication_Error;
               end if;
            end Check_Idle;

            procedure Complete_Block (Index : Command_Index) is
            begin
               Unexpected_Access := @ or else Shut_Down;
               Last_Block := Index;
               Block_Complete := True;
            end Complete_Block;

            procedure Execute (Index : Command_Index) is
            begin
               Unexpected_Access := @ or else Shut_Down;
               Last_Executed := Index;
            end Execute;

            procedure Reset is
            begin
               Last_Executed := 0;
               Shut_Down := False;
               Unexpected_Access := False;
               Fail_Idle := False;
               Idle_Failed := False;
            end Reset;

            procedure Request_Shutdown is
            begin
               Shut_Down := True;
            end Request_Shutdown;

            procedure Fail_Next_Idle is
            begin
               Fail_Idle := True;
            end Fail_Next_Idle;

            entry Wait_For_Idle_Failure when Idle_Failed is
            begin
               null;
            end Wait_For_Idle_Failure;

            function Accessed_After_Shutdown return Boolean is (Unexpected_Access);

            function Last_Command return Command_Index is (Last_Executed);

            entry Wait_For_Shutdown when Shut_Down is
            begin
               null;
            end Wait_For_Shutdown;

            entry Wait_For_Block (Index : out Command_Index; Invalid_Wait : out Boolean) when Block_Complete is
            begin
               Index := Last_Block;
               Invalid_Wait := Waited_For_Missing_Command;
               Block_Complete := False;
               Waited_For_Missing_Command := False;
            end Wait_For_Block;
         end Hardware;

         procedure Wait_Until_Idle (Index : Command_Index);

         function Affects_Axis (Transform : Boolean; Motor : Motor_Name; Axis : Axis_Name) return Boolean is
            pragma Unreferenced (Transform, Motor);
         begin
            return Axis = X_Axis;
         end Affects_Axis;

         procedure Enqueue_Command
           (Pos             : Position;
            Motor_Pos       : Motor_Position;
            Index           : Command_Index;
            Safe_Stop_After : Boolean;
            Vel_Ratio       : Dimensionless)
         is
            pragma Unreferenced (Pos, Motor_Pos, Safe_Stop_After, Vel_Ratio);
         begin
            Hardware.Execute (Index);
         end Enqueue_Command;

         function Extrusion_Is_Allowed return Boolean is (True);

         procedure Finish_Block (Data : Boolean; Pos : Motor_Position; Index : Command_Index) is
            pragma Unreferenced (Pos);
         begin
            --  Homing and other blocking commands wait here, before any following block can execute.
            Wait_Until_Idle (Index);
            if Data then
               Hardware.Request_Shutdown;
               raise Controller_Shutdown_Requested;
            end if;
            Hardware.Complete_Block (Index);
         end Finish_Block;

         protected Pause_Gate is
            procedure Enter_Block;
            entry Wait_For_Block;
            procedure Release;
            entry Wait_For_Release;
         private
            Entered : Boolean := False;
            Released : Boolean := False;
         end Pause_Gate;

         protected body Pause_Gate is
            procedure Enter_Block is
            begin
               Entered := True;
            end Enter_Block;
            entry Wait_For_Block when Entered is
            begin
               null;
            end Wait_For_Block;
            procedure Release is
            begin
               Released := True;
            end Release;
            entry Wait_For_Release when Released is
            begin
               null;
            end Wait_For_Release;
         end Pause_Gate;

         procedure Start_Pause_Block (Data : Boolean; Index : Command_Index) is
            pragma Unreferenced (Index);
         begin
            if not Data then
               Pause_Gate.Enter_Block;
               Pause_Gate.Wait_For_Release;
            end if;
         end Start_Pause_Block;

         procedure Handle_Pause (Pos : Position; Index : Command_Index) is
            pragma Unreferenced (Pos, Index);
         begin
            Pause_Planner.Enqueue_Flush (False);
            Pause_Planner.Enqueue_Flush (True);
         end Handle_Pause;

         procedure Finish_Pause_Block (Data : Boolean; Pos : Motor_Position; Index : Command_Index) is
            pragma Unreferenced (Data, Pos);
         begin
            Wait_Until_Idle (Index);
         end Finish_Pause_Block;

         function Is_Pause_Plan_Done (Data : Boolean) return Boolean is (Data);

         function Pin_Motor (Data, Transform : Boolean; Motor : Motor_Name) return Boolean is
            pragma Unreferenced (Data, Transform, Motor);
         begin
            return False;
         end Pin_Motor;

         procedure Setup_Loop_Move (Data : Boolean) is null;
         procedure Start_Block (Data : Boolean; Index : Command_Index) is null;
         procedure Start_Corner (Index : Command_Index; Data : Boolean) is null;

         function To_Motor (Pos : Position; Transform : Boolean) return Motor_Position is
            pragma Unreferenced (Transform);
         begin
            return [X_Motor => Pos (X_Axis) / mm];
         end To_Motor;

         procedure Wait_Until_Idle (Index : Command_Index) is
         begin
            Hardware.Check_Idle (Index);
         end Wait_Until_Idle;

         package Stepgen is new Step_Generator
           (Planner                       => Planner,
            Pause_Planner                 => Pause_Planner,
            Motor_Name                    => Motor_Name,
            Motor_Position                => Motor_Position,
            Kinematic_Transform           => Boolean,
            Setup_Loop_Move                => Setup_Loop_Move,
            Pin_Motor_To_Block_Start       => Pin_Motor,
            Transform_To_Motor_Position    => To_Motor,
            Transform_Motor_Affects_Axis    => Affects_Axis,
            Motor_Delta_Limits             => Motor_Delta_Limits,
            Maximum_Deltas_Per_Command     => [others => 1.0],
            Start_Planner_Block            => Start_Block,
            Start_Pause_Planner_Block      => Start_Pause_Block,
            Enqueue_Command                => Enqueue_Command,
            Extrusion_Is_Allowed           => Extrusion_Is_Allowed,
            Start_Corner                   => Start_Corner,
            Start_Pause_Corner             => Start_Corner,
            Finish_Planner_Block           => Finish_Block,
            Finish_Pause_Planner_Block     => Finish_Pause_Block,
            Is_Pause_Plan_Done             => Is_Pause_Plan_Done,
            Handle_Pause                   => Handle_Pause,
            Handle_Resume                  => Handle_Pause,
            Wait_Until_Idle                => Wait_Until_Idle,
            Interpolation_Time            => 0.001 * s,
            Runner_CPU                    => System.Multiprocessors.Not_A_Specific_CPU);

         Params : constant Motion_Planner.Kinematic_Parameters :=
           (Tangential_Velocity_Max  => 10.0 * mm / s,
            Axial_Velocity_Maxes     => [others => 10.0 * mm / s],
            Axial_Acceleration_Maxes => [others => 100.0 * mm / s ** 2],
            Axial_Jerk_Maxes         => [others => 1_000.0 * mm / s ** 3],
            Axial_Snap_Maxes         => [others => 10_000.0 * mm / s ** 4],
            Axial_Crackle_Maxes      => [others => 100_000.0 * mm / s ** 5],
            others                  => <>);
         Motor_Map : constant Motor_Position_Map :=
           [X_Axis => [others => 1.0 / mm], others => [others => 0.0 / mm]];
         Previous_Index : Command_Index;
         Current_Index  : Command_Index;
         Idle_Confirmed : Boolean;
         Halt_Index : Command_Index;
         Primary_Running : Boolean := False;
         Pause_Running : Boolean := False;
         Runners_Stopped : Boolean := False;

         procedure Move is
         begin
            Planner.Enqueue_Move ([X_Axis => 1.0 * mm, others => 0.0 * mm], 10.0 * mm / s, Ignore_Bounds => True);
            Planner.Enqueue_Flush (False);
         end Move;

         procedure Setup (Reset_Command_Index : Boolean := False) is
         begin
            Planner.Runner.Setup (Params, Motor_Map);
            Primary_Running := True;
            Pause_Planner.Runner.Setup (Params, Motor_Map);
            Pause_Running := True;
            Stepgen.Runner.Setup (False, Reset_Command_Index);
         end Setup;

         procedure Reset_Planners (Include_Pause : Boolean := True) is
         begin
            if Primary_Running then
               Planner.Reset;
               Primary_Running := False;
            end if;
            if Include_Pause and Pause_Running then
               Pause_Planner.Reset;
               Pause_Running := False;
            end if;
         end Reset_Planners;

         procedure Stop_Runners is
         begin
            if Runners_Stopped then
               return;
            end if;
            Runners_Stopped := True;
            Pause_Gate.Release;
            --  Release preprocessing before stopping the step generator, and do not reset planners already
            --  awaiting Setup after a halt. This also permits cleanup after a failed assertion.
            Reset_Planners;
            Stepgen.Reset;
            abort Stepgen.Runner, Planner.Runner, Pause_Planner.Runner;
         end Stop_Runners;

         procedure Wait_For_Block (Index : out Command_Index) is
            Invalid_Wait : Boolean;
         begin
            select
               Hardware.Wait_For_Block (Index, Invalid_Wait);
            or
               delay 5.0;
               T.Fail ("Timed out waiting for step generator block completion");
               return;
            end select;
            T.Assert (not Invalid_Wait, "Block waited for a command from before the hardware restart");
         end Wait_For_Block;

      begin
         Setup (Reset_Command_Index => True);
         Move;
         Wait_For_Block (Previous_Index);
         T.Assert (Previous_Index > 0, "Initial motion must execute commands");

         --  Cancellation resets the planners without resetting the hardware command stream.
         Stepgen.Reset;
         Reset_Planners;
         Setup;
         Planner.Enqueue_Flush (False);
         Wait_For_Block (Current_Index);
         T.Assert (Current_Index = Previous_Index, "Cancellation must preserve the command index");
         Move;
         Wait_For_Block (Current_Index);
         T.Assert (Current_Index > Previous_Index, "Commands after cancellation must keep increasing");

         for Restart in 1 .. 2 loop
            --  Match the controller's web-restart sequence, including the hardware losing its command history.
            Reset_Planners (Include_Pause => False);
            Stepgen.Soft_Halt (Idle_Confirmed, Halt_Index);
            Reset_Planners;
            T.Assert (Idle_Confirmed, "normal halt must confirm physical idle");
            T.Assert (Halt_Index = Hardware.Last_Command, "halt must return the last emitted command index");
            Wait_Until_Idle (Halt_Index);
            Hardware.Reset;
            Setup (Reset_Command_Index => True);

            --  Startup marks the extruder homed and waits for an empty block before starting the G-code worker.
            Planner.Enqueue_Flush (False);
            Wait_For_Block (Current_Index);
            T.Assert (Current_Index = 0, "A fresh hardware run must begin at command index zero");
            Move;
            Wait_For_Block (Current_Index);
            T.Assert (Current_Index > 0, "Motion must execute after each restart");
         end loop;

         --  A shutdown block must prevent the next queued block and the normal block-completion callback from
         --  touching hardware, even before the controller gets around to halting the pipeline.
         Planner.Enqueue_Flush (True);
         Move;
         select
            Hardware.Wait_For_Shutdown;
         or
            delay 5.0;
            T.Fail ("Timed out waiting for shutdown block");
         end select;
         delay 0.05;
         Reset_Planners (Include_Pause => False);
         select
            delay 5.0;
            T.Fail ("Shutdown must allow soft halt to finish without waiting for hardware");
         then abort
            Stepgen.Soft_Halt (Idle_Confirmed, Halt_Index);
         end select;
         Reset_Planners;
         T.Assert (not Hardware.Accessed_After_Shutdown, "No hardware calls may follow a shutdown block");
         T.Assert (Idle_Confirmed, "the shutdown block already confirmed physical idle");
         T.Assert
           (not Stepgen.Runner'Terminated, "Orderly shutdown must keep the step generator available for reload");
         Hardware.Reset;
         Setup (Reset_Command_Index => True);
         Move;
         Wait_For_Block (Current_Index);
         T.Assert (Current_Index > 0, "Motion must work after reloading a shut down controller");

         Hardware.Fail_Next_Idle;
         Planner.Enqueue_Flush (False);
         select
            Hardware.Wait_For_Idle_Failure;
         or
            delay 5.0;
            T.Fail ("Timed out waiting for simulated hardware failure");
         end select;
         Reset_Planners (Include_Pause => False);
         Stepgen.Soft_Halt (Idle_Confirmed, Halt_Index);
         Reset_Planners;
         T.Assert (not Idle_Confirmed, "hardware failure must not masquerade as confirmed idle");
         T.Assert (Halt_Index = Current_Index, "failure must retain the last submitted command index");
         Hardware.Reset;
         Setup (Reset_Command_Index => True);
         Move;
         Wait_For_Block (Current_Index);
         T.Assert (Current_Index > 0, "communication failure must leave the worker available for reload");

         --  Keep the pause completion marker available until the step generator acknowledges the halt.
         Stepgen.Pause;
         select
            Pause_Gate.Wait_For_Block;
         or
            delay 5.0;
            T.Fail ("Timed out entering the pause block");
         end select;
         Reset_Planners (Include_Pause => False);
         Pause_Gate.Release;
         select
            delay 1.0;
            T.Fail ("Soft halt deadlocked while executing a pause plan");
         then abort
            Stepgen.Soft_Halt (Idle_Confirmed, Halt_Index);
            T.Assert (Idle_Confirmed, "halting a pause plan must confirm physical idle");
         end select;
         Reset_Planners;
         Hardware.Reset;
         Setup (Reset_Command_Index => True);

         abort Stepgen.Runner;
         Stepgen.Soft_Halt (Idle_Confirmed, Halt_Index);
         T.Assert (not Idle_Confirmed, "a terminated worker must not be reported as physically idle");
         begin
            Stepgen.Soft_Halt;
            T.Fail ("the legacy halt API must also report a failed stop");
         exception
            when Hardware_Communication_Error => null;
         end;

         Stop_Runners;
      exception
         when others =>
            Stop_Runners;
            raise;
      end;
   end Test_Command_Indices_After_Restart;

   function All_Tests return Trendy_Test.Test_Group is
     ([Test_Command_Indices_After_Restart'Access]);

end Prunt.Step_Generator_Restart_Test;
