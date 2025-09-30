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

with Ada.Text_IO;
with Prunt.Input_Shapers.Shapers;

package body Prunt.Motion_Planner.Planner.Step_Rate_Limiter is

   procedure Setup (In_Map : Stepper_Pos_Map) is
   begin
      Runner.Setup (In_Map);
   end Setup;

   procedure Reset is
   begin
      Runner.Reset;
   end Reset;

   procedure Run (Block : in out Execution_Block; Needs_New_Profiles : out Boolean) is
   begin
      Runner.Run (Block, Needs_New_Profiles);
   end Run;

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

   protected body Runner is
      procedure Setup (In_Map : Stepper_Pos_Map) is
      begin
         if Setup_Done then
            raise Constraint_Error with "Setup already done.";
         end if;

         Pos_Map := In_Map;

         Setup_Done := True;
      end Setup;

      procedure Reset is
      begin
         Setup_Done := False;
      end Reset;

      procedure Run (Block : in out Execution_Block; Needs_New_Profiles : out Boolean) is
         Current_Time          : Time := 0.0 * s;
         Current_Shapers       : Input_Shapers.Shapers.Axial_Shapers;
         Last_Stepper_Position : Stepper_Position;
         First_Check           : Boolean := True;

         procedure Check_Step (Stepper_Pos : Stepper_Position; I : Corners_Index) is
         begin
            if First_Check then
               Last_Stepper_Position := Stepper_Pos;
               First_Check := False;
            end if;

            for S in Stepper_Name loop
               declare
                  Change : constant Dimensionless := abs (Last_Stepper_Position (S) - Stepper_Pos (S));
               begin
                  Maximum_Overspeed (I) := Dimensionless'Max (@, Change * 1.01 / Maximum_Stepper_Delta (S));
               end;
            end loop;

            Last_Stepper_Position := Stepper_Pos;
         end Check_Step;
      begin
         In_Step_Rate_Limiter := True;

         Needs_New_Profiles := False;

         if not Setup_Done then
            raise Constraint_Error with "Setup not done.";
         end if;

         if Is_Homing_Move (Flush_Resetting_Data (Block)) or else Block.Disable_Input_Shaping then
            --  Shapers are disabled during homing as the interpolation time changes in the middle of the block.
            Current_Shapers :=
              Input_Shapers.Shapers.Create
                ((others => (Kind => Input_Shapers.No_Shaper)), Interpolation_Time, Block_Start_Pos (Block));
         else
            Current_Shapers :=
              Input_Shapers.Shapers.Create
                (Get_Axial_Shaper_Parameters (Block_Persistent_Data (Block)),
                 Interpolation_Time,
                 Block_Start_Pos (Block));
         end if;

         for I in 2 .. Block.N_Corners loop
            Maximum_Overspeed (I) := 1.0;

            loop
               if Current_Time <= Segment_Time (Block, I) then
                  declare
                     Is_Past_Accel_Part : Boolean;
                     Unshaped_Pos       : constant Position :=
                       Segment_Pos_At_Time (Block, I, Current_Time, Is_Past_Accel_Part);
                     Shaped_Pos         : Position := Input_Shapers.Shapers.Do_Step (Current_Shapers, Unshaped_Pos);
                  begin
                     if I = Block.N_Corners and Current_Time >= Segment_Time (Block, I) then
                        declare
                           Extra_Loops_Required : constant Input_Shapers.Cycle_Count :=
                             Input_Shapers.Cycle_Count'Max
                               (0, Input_Shapers.Shapers.Extra_End_Steps_Required (Current_Shapers));
                        begin
                           for J in 0 .. Extra_Loops_Required loop
                              Check_Step (To_Stepper_Position (Shaped_Pos, Pos_Map), I);

                              Shaped_Pos := Input_Shapers.Shapers.Do_Step (Current_Shapers, Unshaped_Pos);
                           end loop;
                        end;
                     else
                        Check_Step (To_Stepper_Position (Shaped_Pos, Pos_Map), I);
                        --  Short-circuit if we're just going to disable shapers.
                        exit when (not Block.Disable_Input_Shaping) and Maximum_Overspeed (I) > 1.0;
                     end if;
                  end;
               end if;

               if Current_Time /= Segment_Time (Block, I) then
                  Current_Time := Current_Time + Interpolation_Time;
               end if;

               if I = Block.N_Corners and Current_Time > Segment_Time (Block, I) then
                  --  Ensure that the last corner is always enqueued from at least once and we always finish on
                  --  the exact final position. Having the wrong interpolation time here is fine because the
                  --  final bit of an execution block has very low velocity.
                  Current_Time := Segment_Time (Block, I);
               else
                  exit when Current_Time >= Segment_Time (Block, I);
               end if;
            end loop;

            Current_Time := Current_Time - Segment_Time (Block, I);
         end loop;

         for I in 2 .. Block.N_Corners loop
            if Maximum_Overspeed (I) > 1.0 then
               Needs_New_Profiles := True;
               if Block.Disable_Input_Shaping then
                  Block.Limited_Segment_Feedrates (I) :=
                    Block.Limited_Segment_Feedrates (I) / (Maximum_Overspeed (I) * 1.1);
                  Log
                    ("Velocity for upcoming moves reduced due to step rate being too high. This can be caused by a "
                     & "high velocity limit combined with a high microstepping ratio.");
               else
                  Block.Disable_Input_Shaping := True;
                  Log
                    ("All input shaping has been turned off for the next block of moves due to the step rate being "
                     & "too high. This can be caused by a high pressure advance value without smoothing.");
               end if;
            end if;
         end loop;

         In_Step_Rate_Limiter := False;
      end Run;
   end Runner;

end Prunt.Motion_Planner.Planner.Step_Rate_Limiter;
