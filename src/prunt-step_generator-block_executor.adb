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

   type Pause_Slew_Index is new Integer range 0 .. Integer (3.0 * s / Interpolation_Time);
   --  First index is full-speed.

   function Pause_Slew_Interpolation_Time (Value : Pause_Slew_Index) return Time;
   --  Interpolation period to use while slewing into or out of pause.

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
   type Homing_Move_When_Kind is (Not_Pending_Kind, This_Block_Kind, This_Move_Kind);

   Zero_Length : constant Length := 0.0 * mm;

   procedure Dequeue_Block
     (Block : out Active_Planner.Execution_Block; Commands : in out Command_State; Reset_Requested : out Boolean)
   is
      Timed_Out                     : Boolean;
      Waiting_For_Step_Rate_Limiter : Boolean;
   begin
      Reset_Requested := False;

      loop
         Active_Planner.Dequeue (Block, Timed_Out, Waiting_For_Step_Rate_Limiter);

         if Timed_Out and then Waiting_For_Step_Rate_Limiter then
            Step_Rate_Limiter_Stalled;
         end if;

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
     (Block           : Active_Planner.Execution_Block;
      Map             : Motor_Pos_Map;
      Commands        : in out Command_State;
      Reset_Requested : out Boolean)
   is
      Pausing_State           : Pausing_State_Kind := Running_Kind;
      Pause_Slew_Cursor       : Pause_Slew_Index := Pause_Slew_Index'First;
      Current_Time            : Time := 0.0 * s;
      Use_Input_Shapers       : constant Boolean := not Active_Planner.Is_Homing_Move (Block);
      Shapers                 : Input_Shapers.Shapers.Axial_Shapers;
      Homing_Move_When        : Homing_Move_When_Kind := Not_Pending_Kind;
      Loop_Move_Offset        : Position_Offset := [others => Zero_Length];
      Loop_Move_Command_Index : Command_Index := 0;

      procedure Process_Corner_Extra_Data (Data : in out Active_Planner.Corner_Extra_Data_Type);

      procedure Process_Corner_Extra_Data (Data : in out Active_Planner.Corner_Extra_Data_Type) is
      begin
         Start_Corner_Callback (Commands.Current_Command_Index, Data);
      end Process_Corner_Extra_Data;
   begin
      Reset_Requested := False;

      if Active_Planner.Is_Homing_Move (Block) then
         if not Allow_Homing then
            raise Constraint_Error with "Homing moves are not allowed in pause plans.";
         end if;

         if Block.N_Corners /= 2 then
            raise Constraint_Error with "Homing move must have exactly 2 corners.";
         end if;

         Homing_Move_When := This_Block_Kind;
      end if;

      if Use_Input_Shapers then
         Shapers :=
           Input_Shapers.Shapers.Create
             (Active_Planner.Block_Kinematic_Parameters (Block).Axial_Shapers,
              Interpolation_Time,
              Active_Planner.Block_Start_Pos (Block));
      else
         Shapers :=
           Input_Shapers.Shapers.Create
             ([others => (Kind => Input_Shapers.No_Shaper)],
              Interpolation_Time,
              Active_Planner.Block_Start_Pos (Block));
      end if;

      Start_Block_Callback (Active_Planner.Flush_Resetting_Data (Block), Commands.Current_Command_Index);

      Active_Planner.Corner_Extra_Data (Block, Active_Planner.Corners_Index'First, Process_Corner_Extra_Data'Access);

      for I in 2 .. Block.N_Corners loop
         declare
            Finishing_Corner : constant Active_Planner.Finishing_Corners_Index := I;
         begin
            Active_Planner.Corner_Extra_Data (Block, I, Process_Corner_Extra_Data'Access);

            loop
               case Pausing_State is
                  when Running_Kind  =>
                     if Pause_Requested and then Homing_Move_When = Not_Pending_Kind then
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
                        Is_Past_Accel_Part : Boolean;
                        Unshaped_Pos       : constant Position :=
                          Active_Planner.Segment_Pos_At_Time
                            (Block, Finishing_Corner, Current_Time, Is_Past_Accel_Part);
                        Shaped_Pos         : Position := Unshaped_Pos;
                        Vel_Ratio          : constant Dimensionless :=
                          Active_Planner.Segment_Vel_Ratio_At_Time (Block, Finishing_Corner, Current_Time);
                        At_Stop            : constant Boolean :=
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
                                 Queue_Command
                                   (State           => Commands,
                                    Pos             => Shaped_Pos,
                                    Map             => Map,
                                    Loop_Until_Hit  => False,
                                    Safe_Stop_After => J = Extra_Loops_Required,
                                    Vel_Ratio       => Vel_Ratio);

                                 Shaped_Pos := Input_Shapers.Shapers.Do_Step (Shapers, Unshaped_Pos);
                              end loop;
                           end;
                        else
                           if Homing_Move_When = This_Move_Kind then
                              Loop_Move_Offset := Shaped_Pos - Commands.Last_Queued_Position;
                              Loop_Move_Command_Index := Commands.Current_Command_Index + 1;
                           end if;

                           Queue_Command
                             (State           => Commands,
                              Pos             => Shaped_Pos,
                              Map             => Map,
                              Loop_Until_Hit  => Homing_Move_When = This_Move_Kind,
                              Safe_Stop_After => At_Stop,
                              Vel_Ratio       => Vel_Ratio);

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

                  if Homing_Move_When /= Not_Pending_Kind
                    and then Current_Time >= Active_Planner.Segment_Time (Block, Finishing_Corner)
                  then
                     raise Constraint_Error with "Homing move queued but end of block reached before execution.";
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

            Current_Time := Current_Time - Active_Planner.Segment_Time (Block, Finishing_Corner);
         end;
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

               delay 0.1;

               Check_Reset (Reset_Requested);
               if Reset_Requested then
                  return;
               end if;
            end loop;
         end if;

         Finish_Block_Callback
           (Resetting_Data       => Active_Planner.Flush_Resetting_Data (Block),
            Next_Block_Pos       => To_Motor_Position (Active_Planner.Next_Block_Pos (Block), Map),
            First_Accel_Distance =>
              Length'
                (if Block.N_Corners < 2
                 then Zero_Length
                 else Active_Planner.Segment_Accel_Distance (Block, Active_Planner.Finishing_Corners_Index'First)),
            Last_Command_Index   => Commands.Current_Command_Index,
            Loop_Move_Offset     => [for A in Axis_Name => Loop_Move_Offset (A) * Loop_Move_Cycles]);
      end;

   end Execute_Block;

end Prunt.Step_Generator.Block_Executor;
