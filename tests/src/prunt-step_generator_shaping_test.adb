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

with Ada.Synchronous_Task_Control;
with System.Multiprocessors;
with Prunt.Input_Shapers;
with Prunt.Motion_Planner.Planner;
with Prunt.Step_Generator;

package body Prunt.Step_Generator_Shaping_Test is

   pragma Extensions_Allowed (On);

   function Block_Allows_Blended_Entry (Data : Boolean) return Boolean is
      pragma Unreferenced (Data);
   begin
      return True;
   end Block_Allows_Blended_Entry;

   type Pause_Timing is (During_Motion, Near_Block_End, During_Drain);

   procedure Check_Shaped_Pause
     (T                      : in out Trendy_Test.Operation'Class;
      Smooth_Added_Part_Only : Boolean;
      Timing                 : Pause_Timing;
      Across_Blocks          : Boolean := False;
      Reset_Instead          : Boolean := False)
   is
   begin
      declare
         type Motor_Name is (X_Motor, Y_Motor, Z_Motor, E_Motor);
         type Motor_Position is array (Motor_Name) of Dimensionless;
         type Motor_Position_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Delta_Limits is array (Motor_Name) of Dimensionless;

         Stream_Blocks : constant Positive := 20;
         Policy_Calls : Natural := 0 with Atomic;
         Publication_Complete, Producer_Failed : Boolean := False with Atomic;
         Stream_Gate : Positive := 1;
         function Allow_Stream_Entry (Data : Boolean) return Boolean is
            pragma Unreferenced (Data);
         begin
            Policy_Calls := @ + 1;
            return True;
         end Allow_Stream_Entry;

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
            Input_Queue_Length                => 10,
            Allow_Blended_Entry               => Allow_Stream_Entry);

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
            Input_Queue_Length                => 10,
            Allow_Blended_Entry               => Block_Allows_Blended_Entry);

         End_Pos           : constant Position := [others => 100.123 * mm];
         Request_Pause     : access procedure;
         Request_Resume    : access procedure;
         In_Pause_Plan     : Boolean := False;
         Pause_Count       : Natural := 0;
         Primary_Count     : Natural := 0;
         Pause_Sent        : Boolean := False;
         Last_Pos          : Position := [others => 0.0 * mm];
         Previous_Pos      : Position := Last_Pos;
         Last_Safe         : Boolean := False;
         Last_Index        : Command_Index := 0;
         Safe_Stops        : Natural := 0;
         Block_Starts      : Natural := 0;
         Block_Finishes    : Natural := 0;
         Reset_Gate        : Ada.Synchronous_Task_Control.Suspension_Object;
         Valid_Pause       : Boolean := True;
         Valid_Deltas      : Boolean := True;
         Valid_Indices     : Boolean := True;
         Resuming_Plan     : Boolean := False;
         Timed_Out         : Boolean := False;
         Runner_Terminated : Boolean;
         Runners_Stopped   : Boolean := False;

         protected Completion is
            procedure Finish;
            entry Wait;
         private
            Done : Boolean := False;
         end Completion;

         protected body Completion is
            procedure Finish is
            begin
               Done := True;
            end Finish;

            entry Wait when Done is
            begin
               null;
            end Wait;
         end Completion;

         function Affects_Axis (Transform : Boolean; Motor : Motor_Name; Axis : Axis_Name) return Boolean is
            pragma Unreferenced (Transform);
         begin
            return Motor_Name'Pos (Motor) = Axis_Name'Pos (Axis);
         end Affects_Axis;

         task Stream_Producer is
            entry Start;
         end Stream_Producer;
         task body Stream_Producer is
            Tail : Position_Offset;
         begin
            accept Start;
            for I in 1 .. Stream_Blocks loop
               Planner.Enqueue_Move
                 ((if I = Stream_Blocks then End_Pos else
                    Position'[others => End_Pos (X_Axis) * Dimensionless (I) / Dimensionless (Stream_Blocks)]),
                  10.0 * mm / s, Ignore_Bounds => True);
               Planner.Enqueue_Flush (False);
            end loop;
            Planner.Enqueue_Move (End_Pos + Position_Offset'[others => 10.0 * mm], 10.0 * mm / s,
                                  Ignore_Bounds => True);
            Tail := Planner.Enqueue_Homing_Flush (False);
            Publication_Complete := abs Tail > 0.0 * mm;
         exception
            when others => Producer_Failed := True;
         end Stream_Producer;

         procedure Enqueue_Command
           (Pos             : Position;
            Motor_Pos       : Motor_Position;
            Index           : Command_Index;
            Safe_Stop_After : Boolean;
            Vel_Ratio       : Dimensionless)
         is
            pragma Unreferenced (Motor_Pos, Vel_Ratio);
         begin
            if Across_Blocks and then Stream_Gate < Stream_Blocks
              and then Pos (X_Axis) >= End_Pos (X_Axis) * (Dimensionless (Stream_Gate) - 0.5)
                / Dimensionless (Stream_Blocks)
            then
               --  Simulate hardware backpressure using only atomic publication witnesses. Keep planning one
               --  boundary ahead of the next block's decision point while exercising real moving handoffs.
               select
                  delay 5.0;
                  raise Program_Error with "Streaming planner failed to stay ahead of the simulated hardware";
               then abort
                  loop
                     exit when Producer_Failed or else
                       (if Stream_Gate < Stream_Blocks - 2 then Policy_Calls > Stream_Gate + 1
                        else Publication_Complete);
                     delay 0.001;
                  end loop;
               end select;
               Stream_Gate := @ + 1;
            end if;
            Previous_Pos := Last_Pos;
            Last_Pos := Pos;
            Last_Safe := Safe_Stop_After;
            Valid_Indices := @ and then Index = Last_Index + 1;
            Last_Index := Index;
            for Axis in Axis_Name loop
               Valid_Deltas := @ and then abs (Pos (Axis) - Previous_Pos (Axis)) <= 1.0 * mm;
            end loop;
            if Safe_Stop_After then
               Safe_Stops := @ + 1;
            end if;
            if not In_Pause_Plan then
               Primary_Count := @ + 1;
               if not Pause_Sent
                 and then
                   (case Timing is
                      when During_Motion => Primary_Count = 500,
                      when Near_Block_End => Pos (X_Axis) >= 99.0 * mm,
                      when During_Drain => Pos (X_Axis) = End_Pos (X_Axis) and then not Safe_Stop_After)
               then
                  Pause_Sent := True;
                  if Reset_Instead then
                     Ada.Synchronous_Task_Control.Set_True (Reset_Gate);
                  else
                     Request_Pause.all;
                  end if;
               end if;
            end if;
         end Enqueue_Command;

         function Extrusion_Is_Allowed return Boolean is (True);

         procedure Finish_Block (Data : Boolean; Pos : Motor_Position; Index : Command_Index) is
            pragma Unreferenced (Data, Pos, Index);
         begin
            Block_Finishes := @ + 1;
            --  A pause requested during the final drain is serviced by the next dequeue.
            if Timing = During_Motion and then not Reset_Instead then
               Completion.Finish;
            end if;
         end Finish_Block;

         procedure Finish_Pause_Block (Data : Boolean; Pos : Motor_Position; Index : Command_Index) is
            pragma Unreferenced (Pos, Index);
         begin
            if Data and then Resuming_Plan and then Timing /= During_Motion then
               Completion.Finish;
            end if;
         end Finish_Pause_Block;

         procedure Handle_Pause (Pos : Position; Index : Command_Index) is
         begin
            Pause_Count := @ + 1;
            Valid_Pause := @ and then Last_Safe and then Pos = Last_Pos and then Index = Last_Index;
            for Axis in Axis_Name loop
               --  All axes follow the same primary path. Their different delays and pressure advance must be gone.
               Valid_Pause := @ and then Pos (Axis) = Pos (X_Axis);
               Valid_Pause := @ and then abs (Pos (Axis) - Previous_Pos (Axis)) < 1.0E-8 * mm;
            end loop;
            if Timing /= During_Motion then
               Valid_Pause := @ and then Pos = End_Pos;
            end if;
            Pause_Planner.Enqueue_Flush_And_Reset_Position (False, Pos, Ignore_Bounds => True);
            Pause_Planner.Enqueue_Move
              (Pos + Position_Offset'[others => 3.14159 * mm], 10.0 * mm / s, Ignore_Bounds => True);
            Pause_Planner.Enqueue_Flush (True);
            Request_Resume.all;
         end Handle_Pause;

         procedure Handle_Resume (Pos : Position; Index : Command_Index) is
            pragma Unreferenced (Index);
         begin
            Resuming_Plan := True;
            Pause_Planner.Enqueue_Move (Pos, 10.0 * mm / s, Ignore_Bounds => True);
            Pause_Planner.Enqueue_Flush (True);
         end Handle_Resume;

         function Is_Pause_Plan_Done (Data : Boolean) return Boolean is (Data);

         function Pin_Motor (Data, Transform : Boolean; Motor : Motor_Name) return Boolean is
            pragma Unreferenced (Data, Transform, Motor);
         begin
            return False;
         end Pin_Motor;

         procedure Setup_Loop_Move (Data : Boolean) is null;
         procedure Start_Block (Data : Boolean; Index : Command_Index) is
            pragma Unreferenced (Data, Index);
         begin
            Block_Starts := @ + 1;
         end Start_Block;
         procedure Start_Corner (Index : Command_Index; Data : Boolean) is null;

         procedure Start_Pause_Block (Data : Boolean; Index : Command_Index) is
            pragma Unreferenced (Data, Index);
         begin
            In_Pause_Plan := True;
         end Start_Pause_Block;

         function To_Motor (Pos : Position; Transform : Boolean) return Motor_Position;
         procedure Wait_Until_Idle (Index : Command_Index);

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
            Handle_Resume                  => Handle_Resume,
            Wait_Until_Idle                => Wait_Until_Idle,
            Interpolation_Time            => 0.001 * s,
            Runner_CPU                    => System.Multiprocessors.Not_A_Specific_CPU);

         Params : Motion_Planner.Kinematic_Parameters :=
           (Tangential_Velocity_Max  => 10.0 * mm / s,
            Axial_Velocity_Maxes     => [others => 10.0 * mm / s],
            Axial_Acceleration_Maxes => [others => 100.0 * mm / s ** 2],
            Axial_Jerk_Maxes         => [others => 1_000.0 * mm / s ** 3],
            Axial_Snap_Maxes         => [others => 10_000.0 * mm / s ** 4],
            Axial_Crackle_Maxes      => [others => 100_000.0 * mm / s ** 5],
            others                  => <>);
         Motor_Map : constant Motor_Position_Map :=
           [for Axis in Axis_Name =>
              [for Motor in Motor_Name =>
                 (if Axis_Name'Pos (Axis) = Motor_Name'Pos (Motor) then 1.0 / mm else 0.0 / mm)]];

         task Resetter;
         task body Resetter is
         begin
            Ada.Synchronous_Task_Control.Suspend_Until_True (Reset_Gate);
            --  Reset waits for Runner to acknowledge, so it can not be called from Runner's own callbacks.
            Stepgen.Reset;
            Completion.Finish;
         end Resetter;

         procedure Stop_Runners is
         begin
            if Runners_Stopped then
               return;
            end if;
            Runners_Stopped := True;
            abort Stream_Producer, Resetter;
            Stepgen.Reset;
            Planner.Reset;
            Pause_Planner.Reset;
            abort Stepgen.Runner, Planner.Runner, Pause_Planner.Runner;
         end Stop_Runners;

         function To_Motor (Pos : Position; Transform : Boolean) return Motor_Position is
            pragma Unreferenced (Transform);
         begin
            return
              [X_Motor => Pos (X_Axis) / mm,
               Y_Motor => Pos (Y_Axis) / mm,
               Z_Motor => Pos (Z_Axis) / mm,
               E_Motor => Pos (E_Axis) / mm];
         end To_Motor;

         procedure Wait_Until_Idle (Index : Command_Index) is null;
      begin
         Params.Axial_Shapers (Y_Axis) :=
           (Kind                         => Input_Shapers.Zero_Vibration,
            Zero_Vibration_Frequency     => 7.3 * hertz,
            Zero_Vibration_Damping_Ratio => 0.1,
            Zero_Vibration_Deriviatives  => 3);
         Params.Axial_Shapers (Z_Axis) :=
           (Kind                                 => Input_Shapers.Extra_Insensitive,
            Extra_Insensitive_Frequency          => 5.7 * hertz,
            Extra_Insensitive_Damping_Ratio      => 0.1,
            Extra_Insensitive_Humps              => 3,
            Extra_Insensitive_Residual_Vibration => 0.05);
         Params.Axial_Shapers (E_Axis) :=
           (Kind                                    => Input_Shapers.Pressure_Advance,
            Pressure_Advance_Time                   => 0.5 * s,
            Pressure_Advance_Smooth_Time            => 0.123 * s,
            Pressure_Advance_Smooth_Added_Part_Only => Smooth_Added_Part_Only,
            Pressure_Advance_Smooth_Levels          => 3);
         if Across_Blocks then
            Params.Bounds :=
              (Kind => Motion_Planner.Rectangular_Workspace,
               Lower_X | Lower_Y | Lower_Z | Lower_E => -1000.0 * mm,
               Upper_X | Upper_Y | Upper_Z | Upper_E => 1000.0 * mm);
         end if;
         Request_Pause := Stepgen.Pause'Access;
         Request_Resume := Stepgen.Resume'Access;
         Planner.Runner.Setup (Params, Motor_Map);
         Pause_Planner.Runner.Setup (Params, Motor_Map);
         if Across_Blocks then
            Stream_Producer.Start;
            select
               delay 5.0;
               T.Fail ("The initial streaming lookahead was not published");
            then abort
               while Policy_Calls < 2 and then not Producer_Failed loop
                  delay 0.001;
               end loop;
            end select;
         else
            Planner.Enqueue_Move (End_Pos, 10.0 * mm / s, Ignore_Bounds => True);
            Planner.Enqueue_Flush (False);
         end if;
         Stepgen.Runner.Setup (False, True);
         select
            Completion.Wait;
         or
            delay 10.0;
            Timed_Out := True;
         end select;
         Runner_Terminated := Stepgen.Runner'Terminated;
         Stop_Runners;
         T.Assert
           (not Timed_Out,
            "Timed out completing shaped pause/park/resume motion; terminated=" & Runner_Terminated'Image
            & ", commands=" & Last_Index'Image & ", pauses=" & Pause_Count'Image
            & ", stops=" & Safe_Stops'Image & ", last=" & Last_Pos'Image);
         if Across_Blocks then
            --  A reset stops consuming the stream, so its producer may still be waiting for a free slot.
            T.Assert
              (not Producer_Failed and then (Reset_Instead or else Publication_Complete),
               "The complete shaped stream was published");
            T.Assert (Block_Starts = 1, "Twenty streamed blocks start once and do not run callbacks at moving seams");
         end if;
         T.Assert (Valid_Deltas, "All shaped commands must respect the motor delta limits");
         T.Assert (Valid_Indices, "Pause and primary motion must share consecutive command indices");
         if Reset_Instead then
            T.Assert (Pause_Sent and then Pause_Count = 0, "A reset must stop without running the pause handler");
            T.Assert (Block_Finishes = 0, "A reset stop must not run the block's end callback");
            T.Assert (Last_Safe and then Safe_Stops = 1, "A reset must end at its only safe stop");
            T.Assert (Last_Pos (X_Axis) < End_Pos (X_Axis), "A reset must stop mid-stream, not at the stream's end");
            for Axis in Axis_Name loop
               T.Assert
                 (Last_Pos (Axis) = Last_Pos (X_Axis)
                  and then abs (Last_Pos (Axis) - Previous_Pos (Axis)) < 1.0E-8 * mm,
                  "A reset must slew to a standstill and drain all shapers before stopping");
            end loop;
         else
            T.Assert (Pause_Sent and then Pause_Count = 1, "The requested pause must be handled once");
            T.Assert (Valid_Pause, "Pause must begin at the exact stationary position after all shapers drain");
            T.Assert (Last_Pos = End_Pos, "Resumed motion must finish at the exact planned position");
            T.Assert
              (Safe_Stops = (if Timing = During_Motion then 4 else 3),
               "Only drained stops in the primary, park and return plans may be marked safe");
         end if;
      exception
         when others =>
            Stop_Runners;
            raise;
      end;
   end Check_Shaped_Pause;

   procedure Test_Shaped_Pause_During_Drain (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Shaped_Pause (T, Smooth_Added_Part_Only => False, Timing => During_Drain);
   end Test_Shaped_Pause_During_Drain;

   procedure Test_Shaped_Pause_Near_Block_End (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Shaped_Pause (T, Smooth_Added_Part_Only => True, Timing => Near_Block_End);
   end Test_Shaped_Pause_Near_Block_End;

   procedure Test_Shaped_Pause_Smooth_Added (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Shaped_Pause (T, Smooth_Added_Part_Only => True, Timing => During_Motion);
   end Test_Shaped_Pause_Smooth_Added;

   procedure Test_Shaped_Pause_Smooth_All (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Shaped_Pause (T, Smooth_Added_Part_Only => False, Timing => During_Motion);
   end Test_Shaped_Pause_Smooth_All;

   procedure Test_Inter_Block_Shaped_Pause (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Shaped_Pause (T, Smooth_Added_Part_Only => False, Timing => During_Motion, Across_Blocks => True);
   end Test_Inter_Block_Shaped_Pause;

   procedure Test_Inter_Block_Reset_Pauses (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Shaped_Pause
        (T, Smooth_Added_Part_Only => False, Timing => During_Motion, Across_Blocks => True, Reset_Instead => True);
   end Test_Inter_Block_Reset_Pauses;

   function All_Tests return Trendy_Test.Test_Group is
     ([Test_Inter_Block_Shaped_Pause'Access,
       Test_Inter_Block_Reset_Pauses'Access,
       Test_Shaped_Pause_Smooth_All'Access,
       Test_Shaped_Pause_Smooth_Added'Access,
       Test_Shaped_Pause_During_Drain'Access,
       Test_Shaped_Pause_Near_Block_End'Access]);

end Prunt.Step_Generator_Shaping_Test;
