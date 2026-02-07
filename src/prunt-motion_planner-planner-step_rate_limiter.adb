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

use type Prunt.Input_Shapers.Axial_Shaper_Parameters;

package body Prunt.Motion_Planner.Planner.Step_Rate_Limiter is

   pragma Extensions_Allowed (On);

   procedure Setup (In_Map : Motor_Pos_Map) is
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

   protected body Runner is
      procedure Setup (In_Map : Motor_Pos_Map) is
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
         Last_Motor_Position : Motor_Position;
         First_Check           : Boolean := True;

         procedure Check_Step (Motor_Pos : Motor_Position; I : Corners_Index);

         procedure Check_Step (Motor_Pos : Motor_Position; I : Corners_Index) is
         begin
            if First_Check then
               Last_Motor_Position := Motor_Pos;
               First_Check := False;
            end if;

            for S in Motor_Name loop
               declare
                  Change : constant Dimensionless := abs (Last_Motor_Position (S) - Motor_Pos (S));
               begin
                  Maximum_Overspeed (I) := Dimensionless'Max (@, Change * 1.01 / Maximum_Motor_Delta (S));
               end;
            end loop;

            Last_Motor_Position := Motor_Pos;
         end Check_Step;
      begin
         In_Step_Rate_Limiter := True;

         Needs_New_Profiles := False;

         if not Setup_Done then
            raise Constraint_Error with "Setup not done.";
         end if;

         if Block.Is_Homing_Move then
            --  Shapers are disabled during homing as the interpolation time changes in the middle of the block.
            pragma
              Assert
                (Block.Params.Axial_Shapers
                 = Input_Shapers.Axial_Shaper_Parameters'(others => (Kind => Input_Shapers.No_Shaper)));
         else
            Current_Shapers :=
              Input_Shapers.Shapers.Create (Block.Params.Axial_Shapers, Interpolation_Time, Block_Start_Pos (Block));
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
                     if I = Block.N_Corners and then Current_Time >= Segment_Time (Block, I) then
                        declare
                           Extra_Loops_Required : constant Input_Shapers.Cycle_Count :=
                             Input_Shapers.Cycle_Count'Max
                               (0, Input_Shapers.Shapers.Extra_End_Steps_Required (Current_Shapers));
                        begin
                           for J in 0 .. Extra_Loops_Required loop
                              Check_Step (To_Motor_Position (Shaped_Pos, Pos_Map), I);

                              Shaped_Pos := Input_Shapers.Shapers.Do_Step (Current_Shapers, Unshaped_Pos);
                           end loop;
                        end;
                     else
                        Check_Step (To_Motor_Position (Shaped_Pos, Pos_Map), I);
                        --  Short-circuit if we're just going to disable shapers.
                        exit when
                          Block.Params.Axial_Shapers
                          /= Input_Shapers.Axial_Shaper_Parameters'(others => (Kind => Input_Shapers.No_Shaper))
                          and then Maximum_Overspeed (I) > 1.0;
                     end if;
                  end;
               end if;

               if Current_Time /= Segment_Time (Block, I) then
                  Current_Time := Current_Time + Interpolation_Time;
               end if;

               if I = Block.N_Corners and then Current_Time > Segment_Time (Block, I) then
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
               if Block.Params.Axial_Shapers
                 = Input_Shapers.Axial_Shaper_Parameters'(others => (Kind => Input_Shapers.No_Shaper))
               then
                  Block.Limited_Segment_Feedrates (I) :=
                    Block.Limited_Segment_Feedrates (I) / (Maximum_Overspeed (I) * 1.1);
                  Log
                    ("Velocity for upcoming moves reduced due to step rate being too high. This can be caused by a "
                     & "high velocity limit combined with a high microstepping ratio.");
               else
                  Block.Params.Axial_Shapers := [others => (Kind => Input_Shapers.No_Shaper)];
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
