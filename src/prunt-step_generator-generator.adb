-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_Functions;
with System.Pool_Local;
with Prunt.Input_Shapers.Shapers;
use type Prunt.Input_Shapers.Cycle_Count;

package body Prunt.Step_Generator.Generator is

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

   function To_Stepper_Position (Pos : Position; Map : Stepper_Pos_Map) return Stepper_Position is
      Ret : Stepper_Position := [others => 0.0];
   begin
      for S in Stepper_Name loop
         for A in Axis_Name loop
            --  TODO: Use multiplication for the map instead of division so we don't need this check.
            if Map (A, S) /= Length'Last then
               Ret (S) := Ret (S) + Pos (A) / Map (A, S);
            end if;
         end loop;
      end loop;

      return Ret;
   end To_Stepper_Position;

   task body Runner is
      Current_Command_Index : Command_Index;
      Current_Time          : Time;
      Pos_Map               : Stepper_Pos_Map;

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
   begin
      loop
         Current_Command_Index := 0;
         Current_Time := 0.0 * s;
         Homing_Move_When := Not_Pending_Kind;
         Pausing_State := Running_Kind;
         Pause_Slew := Pause_Slew_Index'First;
         Paused := False;
         Do_Pause := False;
         Loop_Move_Offset := [others => Zero_Length];
         Loop_Move_Command_Index := 0;
         Previous_Position := [others => Zero_Length];

         accept Setup (Map : Stepper_Pos_Map) do
            Pos_Map := Map;
         end Setup;
         Main :
         loop
            declare
               Timed_Out : Boolean;
            begin
               loop
                  Dequeue (Block, Timed_Out);

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

            if Is_Homing_Move (Flush_Resetting_Data (Block)) then
               if Block.N_Corners /= 2 then
                  raise Constraint_Error with "Homing move must have exactly 2 corners.";
               end if;
               Homing_Move_When := This_Block_Kind;

               Current_Shapers :=
                 Input_Shapers.Shapers.Create
                   ((others => (Kind => Input_Shapers.No_Shaper)), Interpolation_Time, Block_Start_Pos (Block));
               --  Shapers are disabled during homing as the interpolation time changes in the middle of the block.

            else
               Current_Shapers :=
                 Input_Shapers.Shapers.Create
                   (Get_Axial_Shaper_Parameters (Block_Persistent_Data (Block)),
                    Interpolation_Time,
                    Block_Start_Pos (Block));
            end if;

            Start_Planner_Block (Flush_Resetting_Data (Block), Block_Persistent_Data (Block), Current_Command_Index);

            for I in 2 .. Block.N_Corners loop
               loop
                  Current_Command_Index := Current_Command_Index + 1;

                  case Pausing_State is
                     when Running_Kind =>
                        if Do_Pause and then Homing_Move_When = Not_Pending_Kind then
                           Pausing_State := Pausing_Kind;
                        end if;

                     when Pausing_Kind =>
                        if Pause_Slew = Pause_Slew_Index'Last then
                           Pausing_State := Paused_Kind;
                        else
                           Pause_Slew := @ + 1;
                        end if;

                     when Paused_Kind =>
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

                  if Current_Time <= Segment_Time (Block, I) then
                     declare
                        Is_Past_Accel_Part : Boolean;
                        Unshaped_Pos       : constant Position :=
                          Segment_Pos_At_Time (Block, I, Current_Time, Is_Past_Accel_Part);
                        Shaped_Pos         : Position := Input_Shapers.Shapers.Do_Step (Current_Shapers, Unshaped_Pos);
                        Vel_Ratio          : constant Dimensionless :=
                          Segment_Vel_Ratio_At_Time (Block, I, Current_Time);
                     begin
                        if Pausing_State = Paused_Kind
                          or else (I = Block.N_Corners and Current_Time >= Segment_Time (Block, I))
                        then
                           declare
                              Extra_Loops_Required : constant Input_Shapers.Cycle_Count :=
                                Input_Shapers.Cycle_Count'Max
                                  (0, Input_Shapers.Shapers.Extra_End_Steps_Required (Current_Shapers));
                           begin
                              for J in 0 .. Extra_Loops_Required loop
                                 Enqueue_Command
                                   (Pos             => Shaped_Pos,
                                    Stepper_Pos     => To_Stepper_Position (Shaped_Pos, Pos_Map),
                                    Data            => Corner_Extra_Data (Block, I),
                                    Index           => Current_Command_Index,
                                    Loop_Until_Hit  => Homing_Move_When = This_Move_Kind and J = 0,
                                    Safe_Stop_After => J = Extra_Loops_Required,
                                    Vel_Ratio       => Vel_Ratio);

                                 if Homing_Move_When = This_Move_Kind and J = 0 then
                                    pragma Assert (Loop_Move_Command_Index = 0);
                                    Loop_Move_Offset := Shaped_Pos - Previous_Position;
                                    Loop_Move_Command_Index := Current_Command_Index;
                                 end if;

                                 Shaped_Pos := Input_Shapers.Shapers.Do_Step (Current_Shapers, Unshaped_Pos);
                              end loop;
                           end;
                        else
                           if Homing_Move_When = This_Move_Kind then
                              pragma Assert (Loop_Move_Command_Index = 0);
                              Loop_Move_Offset := Shaped_Pos - Previous_Position;
                              Loop_Move_Command_Index := Current_Command_Index;
                           end if;

                           Enqueue_Command
                             (Pos             => Shaped_Pos,
                              Stepper_Pos     => To_Stepper_Position (Shaped_Pos, Pos_Map),
                              Data            => Corner_Extra_Data (Block, I),
                              Index           => Current_Command_Index,
                              Loop_Until_Hit  => Homing_Move_When = This_Move_Kind,
                              Safe_Stop_After => False,
                              Vel_Ratio       => Vel_Ratio);
                        end if;

                        Previous_Position := Shaped_Pos;

                        case Homing_Move_When is
                           when This_Block_Kind =>
                              if Is_Past_Accel_Part then
                                 Homing_Move_When := This_Move_Kind; --  Next loop iteration, not this one.

                              end if;

                           when Not_Pending_Kind =>
                              null;

                           when This_Move_Kind =>
                              Homing_Move_When := Not_Pending_Kind;
                        end case;
                     end;
                  end if;

                  if Homing_Move_When /= Not_Pending_Kind and Current_Time >= Segment_Time (Block, I) then
                     raise Constraint_Error with "Homing move queued but end of block reached before execution.";
                  end if;

                  if Current_Time /= Segment_Time (Block, I) then
                     if Homing_Move_When = This_Move_Kind then
                        Current_Time := Current_Time + Loop_Interpolation_Time;
                     else
                        Current_Time := Current_Time + Pause_Slew_Interpolation_Time (Pause_Slew);
                     end if;
                  end if;

                  if I = Block.N_Corners and Current_Time > Segment_Time (Block, I) then
                     Current_Time := Segment_Time (Block, I);
                     --  Ensure that the last corner is always enqueued from at least once and we always finish on
                     --  the exact final position. Having the wrong interpolation time here is fine because the
                     --  final bit of an execution block has very low velocity.

                  else
                     exit when Current_Time >= Segment_Time (Block, I);
                  end if;
               end loop;

               Current_Time := Current_Time - Segment_Time (Block, I);
            end loop;

            declare
               First_Accel_Distance : Length;
            begin
               if Block.N_Corners < 2 then
                  First_Accel_Distance := 0.0 * mm;
               else
                  First_Accel_Distance := Segment_Accel_Distance (Block, 2);
               end if;

               if Is_Homing_Move (Flush_Resetting_Data (Block)) then
                  pragma Assert (Loop_Move_Command_Index /= 0);
                  Report_Loop_Move_Offset (Loop_Move_Command_Index, Loop_Move_Offset);
                  Loop_Move_Command_Index := 0;
               end if;

               Finish_Planner_Block
                 (Flush_Resetting_Data (Block),
                  Block_Persistent_Data (Block),
                  To_Stepper_Position (Next_Block_Pos (Block), Pos_Map),
                  First_Accel_Distance,
                  Current_Command_Index);
            end;
         end loop Main;
      end loop;
   end Runner;

end Prunt.Step_Generator.Generator;
