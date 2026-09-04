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

with Prunt.Input_Shapers.Shapers;

package body Prunt.Step_Generator.Block_Executor is

   pragma Extensions_Allowed (On);

   use type Active_Planner.Corners_Index;
   use type Input_Shapers.Cycle_Count;
   use type Input_Shapers.Shaper_Kind;

   function Pause_Slew_Interpolation_Time (Value : Pause_Slew_Index) return Time is
   begin
      if Value = Pause_Slew_Index'First then
         return Interpolation_Time;
      elsif Value = Pause_Slew_Index'Last then
         return 0.0 * s;
      else
         declare
            X : constant Dimensionless := Dimensionless (Value) / Dimensionless (Pause_Slew_Index'Last);
            Y : constant Dimensionless := 1.0 - X;
         begin
            return Y ** 5 / (X ** 5 + Y ** 5) * Interpolation_Time;
         end;
      end if;
   end Pause_Slew_Interpolation_Time;

   type Pausing_State_Kind is (Running_Kind, Pausing_Kind, Paused_Kind, Resuming_Kind);
   procedure Dequeue_Block
     (Block : out Active_Planner.Execution_Block; Commands : in out Command_State; Reset_Requested : out Boolean)
   is
      Timed_Out : Boolean;
   begin
      Reset_Requested := False;

      loop
         Active_Planner.Dequeue (Block, Timed_Out);

         Check_Reset (Reset_Requested);
         if Reset_Requested then
            return;
         end if;

         if Pause_Requested then
            Handle_Pause (Commands.Last_Queued_Position, Reset_Requested);
            if Reset_Requested then
               return;
            end if;
         end if;

         exit when not Timed_Out;
      end loop;
   end Dequeue_Block;

   procedure Execute_Block
     (Block           : access constant Active_Planner.Execution_Block;
      Transform       : Kinematic_Transform;
      Commands        : in out Command_State;
      Reset_Requested : out Boolean)
   is
      Pausing_State      : Pausing_State_Kind := Running_Kind;
      Pause_Slew_Cursor  : Pause_Slew_Index := Pause_Slew_Index'First;
      Current_Time       : Time := 0.0 * s;
      Shapers            : Input_Shapers.Shapers.Axial_Shapers;
      Has_Loop_Move      : Boolean := False;
      Catch_Up_Axes      : Catch_Up_Axis_Set := [others => False];
      Pin_To_Block_Start : constant Motor_Pin_Selection :=
        [for Motor in Motor_Name =>
           Pin_Motor_To_Block_Start_Callback (Active_Planner.Flush_Resetting_Data (Block), Transform, Motor)];
      Stationary_Pos     : constant Motor_Position :=
        Transform_To_Motor_Position (Active_Planner.Block_Start_Pos (Block), Transform);
      Use_Input_Shapers  : constant Boolean :=
        not Active_Planner.Is_Homing_Move (Block) and then not (for some Is_Pinned of Pin_To_Block_Start => Is_Pinned);

      procedure Process_Corner_Extra_Data (Data : in out Active_Planner.Corner_Extra_Data_Type);
      procedure Publish_Block_Corner_ID (Corner : Active_Planner.Corners_Index);
      procedure Queue_Block_Command
        (Pos             : Position;
         Safe_Stop_After : Boolean;
         Vel_Ratio       : Dimensionless;
         Catch_Up_Axes   : Catch_Up_Axis_Set := [others => False]);
      procedure Queue_Loop_Command (Pos : Position; Safe_Stop_After : Boolean; Vel_Ratio : Dimensionless);

      procedure Process_Corner_Extra_Data (Data : in out Active_Planner.Corner_Extra_Data_Type) is
      begin
         Start_Corner_Callback (Commands.Current_Command_Index, Data);
      end Process_Corner_Extra_Data;

      procedure Publish_Block_Corner_ID (Corner : Active_Planner.Corners_Index) is
         Corner_ID : constant Planner_Corner_ID := Active_Planner.Corner_ID (Block, Corner);
      begin
         if Corner = Block.N_Corners and then Active_Planner.Has_Associated_Overflow_Block (Block) then
            return;
         end if;

         Publish_Corner_ID (Corner_ID);
      end Publish_Block_Corner_ID;

      procedure Queue_Block_Command
        (Pos             : Position;
         Safe_Stop_After : Boolean;
         Vel_Ratio       : Dimensionless;
         Catch_Up_Axes   : Catch_Up_Axis_Set := [others => False]) is
      begin
         Queue_Command
           (State              => Commands,
            Pos                => Pos,
            Transform          => Transform,
            Safe_Stop_After    => Safe_Stop_After,
            Vel_Ratio          => Vel_Ratio,
            Catch_Up_Axes      => Catch_Up_Axes,
            Pin_To_Block_Start => Pin_To_Block_Start,
            Stationary_Pos     => Stationary_Pos);
      end Queue_Block_Command;

      procedure Queue_Loop_Command (Pos : Position; Safe_Stop_After : Boolean; Vel_Ratio : Dimensionless) is
         Fractions  : constant Axis_Fractions :=
           Command_Fractions
             (Start_Pos     => Commands.Last_Queued_Position,
              Target_Pos    => Pos,
              Transform     => Transform,
              Catch_Up_Axes => [others => False]);
         Motor_Pos  : Motor_Position := Transform_To_Motor_Position (Pos, Transform);
         Next_Index : constant Command_Index := Commands.Current_Command_Index + 1;
      begin
         for Axis in Axis_Name loop
            if Fractions (Axis) < 1.0 then
               raise Constraint_Error with "A loop command unexpectedly requires subdivision.";
            end if;
         end loop;

         for Motor in Motor_Name loop
            if Pin_To_Block_Start (Motor) then
               Motor_Pos (Motor) := Stationary_Pos (Motor);
            end if;
         end loop;

         Setup_Loop_Move_Callback (Active_Planner.Flush_Resetting_Data (Block));
         Enqueue_Command
           (Pos             => Pos,
            Motor_Pos       => Motor_Pos,
            Index           => Next_Index,
            Safe_Stop_After => Safe_Stop_After,
            Vel_Ratio       => Vel_Ratio);
         Commands.Current_Command_Index := Next_Index;
         Commands.Last_Queued_Position := Pos;
      end Queue_Loop_Command;
   begin
      Reset_Requested := False;
      Commands.Last_Queued_Position := Active_Planner.Block_Start_Pos (Block);

      if Active_Planner.Is_Homing_Move (Block) then
         if not Allow_Homing then
            raise Constraint_Error with "Homing moves are not allowed in pause plans.";
         end if;

         if Block.N_Corners /= 2 then
            raise Constraint_Error with "Homing move must have exactly 2 corners.";
         end if;
      end if;

      if Use_Input_Shapers then
         Shapers :=
           Input_Shapers.Shapers.Create
             (Active_Planner.Block_Kinematic_Parameters (Block).Axial_Shapers,
              Interpolation_Time,
              Active_Planner.Block_Start_Pos (Block));

         for A in Axis_Name loop
            if Active_Planner.Block_Kinematic_Parameters (Block).Axial_Shapers (A).Kind
              = Input_Shapers.Pressure_Advance
            then
               Catch_Up_Axes (A) := True;
            end if;
         end loop;
      else
         Shapers :=
           Input_Shapers.Shapers.Create
             ([others => (Kind => Input_Shapers.No_Shaper)],
              Interpolation_Time,
              Active_Planner.Block_Start_Pos (Block));
      end if;

      Start_Block_Callback (Active_Planner.Flush_Resetting_Data (Block), Commands.Current_Command_Index);

      Active_Planner.Corner_Extra_Data (Block, Active_Planner.Corners_Index'First, Process_Corner_Extra_Data'Access);
      Publish_Block_Corner_ID (Active_Planner.Corners_Index'First);

      for I in 2 .. Block.N_Corners loop
         declare
            Finishing_Corner : constant Active_Planner.Finishing_Corners_Index := I;
         begin
            loop
               case Pausing_State is
                  when Running_Kind  =>
                     if Pause_Requested and then not Active_Planner.Is_Homing_Move (Block) then
                        Pausing_State := Pausing_Kind;
                     end if;

                  when Pausing_Kind  =>
                     if Pause_Slew_Cursor = Pause_Slew_Index'Last then
                        Pausing_State := Paused_Kind;
                     else
                        Pause_Slew_Cursor := @ + 1;
                        if Pause_Slew_Cursor = Pause_Slew_Index'Last then
                           Pausing_State := Paused_Kind;
                        end if;
                     end if;

                  when Paused_Kind   =>
                     null;

                  when Resuming_Kind =>
                     if Pause_Slew_Cursor = Pause_Slew_Index'First then
                        Pausing_State := Running_Kind;
                     else
                        Pause_Slew_Cursor := @ - 1;
                     end if;
               end case;

               declare
                  Segment_Time               : constant Time := Active_Planner.Segment_Time (Block, Finishing_Corner);
                  Current_Interpolation_Time : constant Time :=
                    (if Pausing_State = Running_Kind
                     then Interpolation_Time
                     else Pause_Slew_Interpolation_Time (Pause_Slew_Cursor));
                  Pause_Stopped              : constant Boolean := Pausing_State = Paused_Kind;
               begin
                  if Current_Time <= Segment_Time then
                     declare
                        Unshaped_Pos : constant Position :=
                          Active_Planner.Segment_Pos_At_Time (Block, Finishing_Corner, Current_Time);
                        Shaped_Pos   : Position := Unshaped_Pos;
                        Vel_Ratio    : constant Dimensionless :=
                          Active_Planner.Segment_Vel_Ratio_At_Time (Block, Finishing_Corner, Current_Time);
                        At_Stop      : constant Boolean :=
                          Pause_Stopped
                          or else (Finishing_Corner = Block.N_Corners and then Current_Time >= Segment_Time);
                     begin
                        if Use_Input_Shapers then
                           Shaped_Pos := Input_Shapers.Shapers.Do_Step (Shapers, Unshaped_Pos);
                        end if;

                        if Use_Input_Shapers and then At_Stop then
                           declare
                              Extra_Loops_Required : constant Input_Shapers.Cycle_Count :=
                                Input_Shapers.Cycle_Count'Max
                                  (0, Input_Shapers.Shapers.Extra_End_Steps_Required (Shapers));
                           begin
                              for J in 0 .. Extra_Loops_Required loop
                                 Queue_Block_Command
                                   (Pos             => Shaped_Pos,
                                    Safe_Stop_After => J = Extra_Loops_Required,
                                    Vel_Ratio       => Vel_Ratio,
                                    Catch_Up_Axes   => Catch_Up_Axes);

                                 Shaped_Pos := Input_Shapers.Shapers.Do_Step (Shapers, Unshaped_Pos);
                              end loop;
                           end;
                        else
                           if Active_Planner.Is_Homing_Move (Block)
                             and then not Has_Loop_Move
                             and then Current_Time >= Active_Planner.Loop_Move_Minimum_Time (Block)
                           then
                              Has_Loop_Move := True;
                              Queue_Loop_Command
                                (Pos => Shaped_Pos, Safe_Stop_After => At_Stop, Vel_Ratio => Vel_Ratio);
                           else
                              Queue_Block_Command
                                (Pos             => Shaped_Pos,
                                 Safe_Stop_After => At_Stop,
                                 Vel_Ratio       => Vel_Ratio,
                                 Catch_Up_Axes   => Catch_Up_Axes);
                           end if;
                        end if;

                        if Pause_Stopped then
                           Handle_Pause (Commands.Last_Queued_Position, Reset_Requested);
                           if Reset_Requested then
                              return;
                           end if;

                           Pausing_State := Resuming_Kind;
                        end if;
                     end;
                  end if;

                  if Current_Time /= Segment_Time and then not Pause_Stopped then
                     Current_Time := Current_Time + Current_Interpolation_Time;
                  end if;

                  if Finishing_Corner = Block.N_Corners and then Current_Time > Segment_Time then
                     --  Ensure that the last corner is always enqueued at least once and we always finish on the exact
                     --  final position. Having the wrong interpolation time here is fine because the final bit of an
                     --  execution block has very low velocity.
                     Current_Time := Segment_Time;
                  else
                     exit when Current_Time >= Segment_Time;
                  end if;
               end;
            end loop;

            Active_Planner.Corner_Extra_Data (Block, I, Process_Corner_Extra_Data'Access);
            Publish_Block_Corner_ID (I);

            Current_Time := Current_Time - Active_Planner.Segment_Time (Block, Finishing_Corner);
         end;
      end loop;

      if Active_Planner.Is_Homing_Move (Block) and then not Has_Loop_Move then
         raise Constraint_Error with "Homing block ended before its loop command was emitted.";
      end if;

      if Has_Loop_Move then
         Wait_Until_Idle (Commands.Current_Command_Index);
      end if;

      Finish_Block_Callback
        (Resetting_Data     => Active_Planner.Flush_Resetting_Data (Block),
         Next_Block_Pos     => Transform_To_Motor_Position (Active_Planner.Next_Block_Pos (Block), Transform),
         Last_Command_Index => Commands.Current_Command_Index);

   end Execute_Block;

end Prunt.Step_Generator.Block_Executor;
