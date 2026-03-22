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

with Ada.Numerics.Generic_Elementary_Functions;
with System.Pool_Local;
with Prunt.Input_Shapers.Shapers;
use type Prunt.Input_Shapers.Cycle_Count;
use type Prunt.Input_Shapers.Axial_Shaper_Parameters;

package body Prunt.Step_Generator is

   pragma Extensions_Allowed (On);

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   Do_Pause : Boolean := False
   with Atomic, Volatile;
   Paused   : Boolean := False
   with Atomic, Volatile;

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

   function Pause_Slew_Interpolation_Time (Index : Pause_Slew_Index) return Time is
   begin
      return Math.Cos (Dimensionless (Index), 4.0 * Dimensionless (Pause_Slew_Index'Last)) * Interpolation_Time;
   end Pause_Slew_Interpolation_Time;

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

   task body Runner is
      Current_Command_Index : Command_Index := 0;
      Current_Time          : Time;
      Pos_Map               : Motor_Pos_Map;

      type Homing_Move_When_Kind is (Not_Pending_Kind, This_Block_Kind, This_Move_Kind);
      Homing_Move_When : Homing_Move_When_Kind;

      type Pausing_State_Kind is (Running_Kind, Pausing_Kind, Paused_Kind, Resuming_Kind);
      Pausing_State : Pausing_State_Kind;
      Pause_Slew    : Pause_Slew_Index;

      type Block_Wrapper is record
         Block : aliased Execution_Block;
      end record;

      Pool : System.Pool_Local.Unbounded_Reclaim_Pool;

      type Block_Wrapper_Access is access Block_Wrapper with Storage_Pool => Pool;

      Working_Block_Wrapper : constant Block_Wrapper_Access := new Block_Wrapper;
      Block renames Working_Block_Wrapper.Block;

      Current_Shapers : Input_Shapers.Shapers.Axial_Shapers;

      Loop_Move_Offset        : Position_Offset;
      Loop_Move_Command_Index : Command_Index;
      Previous_Position       : Position;

      Zero_Length : constant Length := 0.0 * mm;

      procedure Process_Corner_Extra_Data (Data : in out Planner.Corner_Extra_Data_Type) is
      begin
         Start_Corner (Current_Command_Index, Data);
      end Process_Corner_Extra_Data;
   begin
      loop
         Current_Time := 0.0 * s;
         Homing_Move_When := Not_Pending_Kind;
         Pausing_State := Running_Kind;
         Pause_Slew := Pause_Slew_Index'First;
         Paused := False;
         Do_Pause := False;
         Previous_Position := [others => Zero_Length];

         accept Setup (Map : Motor_Pos_Map) do
            Pos_Map := Map;
         end Setup;

         Main : loop
            Loop_Move_Offset := [others => Zero_Length];
            Loop_Move_Command_Index := 0;

            declare
               Timed_Out                     : Boolean;
               Waiting_For_Step_Rate_Limiter : Boolean;
            begin
               loop
                  Dequeue (Block, Timed_Out, Waiting_For_Step_Rate_Limiter);

                  if Timed_Out and then Waiting_For_Step_Rate_Limiter then
                     Log
                       ("The step command generator is waiting for the step rate limiter to complete. This can take "
                        & "a long time if the G-code contains multiple very long moves. In a future version this "
                        & "will be improved.");
                  end if;

                  select
                     accept Reset;
                     exit Main;
                  else
                     null;
                  end select;

                  if Do_Pause then
                     Paused := True;
                     loop
                        select
                           accept Reset;
                           exit Main;
                        else
                           null;
                        end select;

                        delay 0.1;

                        exit when not Do_Pause;
                     end loop;
                     Paused := False;
                  end if;
                  Pausing_State := Running_Kind;
                  Pause_Slew := Pause_Slew_Index'First;

                  exit when not Timed_Out;
               end loop;
            end;

            if Block.Is_Homing_Move then
               if Block.N_Corners /= 2 then
                  raise Constraint_Error with "Homing move must have exactly 2 corners.";
               end if;
               Homing_Move_When := This_Block_Kind;

               --  Shapers are disabled during homing as the interpolation time changes in the middle of the block.
               pragma
                 Assert
                   (Block.Block_Kinematic_Parameters.Axial_Shapers
                    = Input_Shapers.Axial_Shaper_Parameters'(others => (Kind => Input_Shapers.No_Shaper)));
            end if;

            Current_Shapers :=
              Input_Shapers.Shapers.Create
                (Block.Block_Kinematic_Parameters.Axial_Shapers, Interpolation_Time, Block_Start_Pos (Block));

            Start_Planner_Block (Block.Flush_Resetting_Data, Current_Command_Index);

            Block.Corner_Extra_Data (Planner.Corners_Index'First, Process_Corner_Extra_Data'Access);

            for I in 2 .. Block.N_Corners loop
               Block.Corner_Extra_Data (I, Process_Corner_Extra_Data'Access);

               loop
                  Current_Command_Index := @ + 1;

                  case Pausing_State is
                     when Running_Kind  =>
                        if Do_Pause and then Homing_Move_When = Not_Pending_Kind then
                           Pausing_State := Pausing_Kind;
                        end if;

                     when Pausing_Kind  =>
                        if Pause_Slew = Pause_Slew_Index'Last then
                           Pausing_State := Paused_Kind;
                        else
                           Pause_Slew := @ + 1;
                        end if;

                     when Paused_Kind   =>
                        Paused := True;
                        loop
                           select
                              accept Reset;
                              exit Main;
                           else
                              null;
                           end select;

                           delay 0.1;

                           exit when not Do_Pause;
                        end loop;
                        Paused := False;
                        Pausing_State := Resuming_Kind;

                     when Resuming_Kind =>
                        if Pause_Slew = Pause_Slew_Index'First then
                           Pausing_State := Running_Kind;
                        else
                           Pause_Slew := @ - 1;
                        end if;
                  end case;

                  if Current_Time <= Block.Segment_Time (I) then
                     declare
                        Is_Past_Accel_Part : Boolean;
                        Unshaped_Pos       : constant Position :=
                          Block.Segment_Pos_At_Time (I, Current_Time, Is_Past_Accel_Part);
                        Shaped_Pos         : Position := Input_Shapers.Shapers.Do_Step (Current_Shapers, Unshaped_Pos);
                        Vel_Ratio          : constant Dimensionless :=
                          Block.Segment_Vel_Ratio_At_Time (I, Current_Time);
                     begin
                        if Pausing_State = Paused_Kind
                          or else (I = Block.N_Corners and then Current_Time >= Block.Segment_Time (I))
                        then
                           declare
                              Extra_Loops_Required : constant Input_Shapers.Cycle_Count :=
                                Input_Shapers.Cycle_Count'Max
                                  (0, Input_Shapers.Shapers.Extra_End_Steps_Required (Current_Shapers));
                           begin
                              for J in 0 .. Extra_Loops_Required loop
                                 if J /= 0 then
                                    Current_Command_Index := @ + 1;
                                 end if;

                                 Enqueue_Command
                                   (Pos             => Shaped_Pos,
                                    Motor_Pos       => To_Motor_Position (Shaped_Pos, Pos_Map),
                                    Index           => Current_Command_Index,
                                    Loop_Until_Hit  => Homing_Move_When = This_Move_Kind and then J = 0,
                                    Safe_Stop_After => J = Extra_Loops_Required,
                                    Vel_Ratio       => Vel_Ratio);

                                 if Homing_Move_When = This_Move_Kind and then J = 0 then
                                    Loop_Move_Offset := Shaped_Pos - Previous_Position;
                                 end if;

                                 Shaped_Pos := Input_Shapers.Shapers.Do_Step (Current_Shapers, Unshaped_Pos);
                              end loop;
                           end;
                        else
                           if Homing_Move_When = This_Move_Kind then
                              Loop_Move_Offset := Shaped_Pos - Previous_Position;
                              Loop_Move_Command_Index := Current_Command_Index;
                           end if;

                           Enqueue_Command
                             (Pos             => Shaped_Pos,
                              Motor_Pos       => To_Motor_Position (Shaped_Pos, Pos_Map),
                              Index           => Current_Command_Index,
                              Loop_Until_Hit  => Homing_Move_When = This_Move_Kind,
                              Safe_Stop_After => False,
                              Vel_Ratio       => Vel_Ratio);
                        end if;

                        Previous_Position := Shaped_Pos;

                        case Homing_Move_When is
                           when This_Block_Kind  =>
                              if Is_Past_Accel_Part then
                                 Homing_Move_When := This_Move_Kind; --  Next loop iteration, not this one.

                              end if;

                           when Not_Pending_Kind =>
                              null;

                           when This_Move_Kind   =>
                              Homing_Move_When := Not_Pending_Kind;
                        end case;
                     end;
                  end if;

                  if Homing_Move_When /= Not_Pending_Kind and then Current_Time >= Block.Segment_Time (I) then
                     raise Constraint_Error with "Homing move queued but end of block reached before execution.";
                  end if;

                  if Current_Time /= Block.Segment_Time (I) then
                     if Homing_Move_When = This_Move_Kind then
                        Current_Time := Current_Time + Interpolation_Time;
                     else
                        Current_Time := Current_Time + Pause_Slew_Interpolation_Time (Pause_Slew);
                     end if;
                  end if;

                  if I = Block.N_Corners and then Current_Time > Block.Segment_Time (I) then
                     --  Ensure that the last corner is always enqueued from at least once and we always finish on the
                     --  exact final position. Having the wrong interpolation time here is fine because the final bit
                     --  of an execution block has very low velocity.
                     Current_Time := Block.Segment_Time (I);
                  else
                     exit when Current_Time >= Block.Segment_Time (I);
                  end if;
               end loop;

               Current_Time := Current_Time - Block.Segment_Time (I);
            end loop;

            declare
               Loop_Move_Cycles : Dimensionless := 0.0;
            begin
               if Loop_Move_Command_Index /= 0 then
                  loop
                     select
                        Loop_Cycle_Reporter.Wait (Loop_Move_Command_Index, Loop_Move_Cycles);
                        exit;
                     or
                        delay 3.0;
                     end select;

                     select
                        accept Reset;
                        exit Main;
                     or
                        delay 0.1;
                     end select;
                  end loop;
               end if;

               Finish_Planner_Block
                 (Resetting_Data       => Block.Flush_Resetting_Data,
                  Next_Block_Pos       => To_Motor_Position (Block.Next_Block_Pos, Pos_Map),
                  First_Accel_Distance =>
                    Length'(if Block.N_Corners < 2 then Zero_Length else Segment_Accel_Distance (Block, 2)),
                  Last_Command_Index   => Current_Command_Index,
                  Loop_Move_Offset     => [for A in Axis_Name => Loop_Move_Offset (A) * Loop_Move_Cycles]);
            end;
         end loop Main;
      end loop;
   end Runner;

end Prunt.Step_Generator;
