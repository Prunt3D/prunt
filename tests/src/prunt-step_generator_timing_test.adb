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

package body Prunt.Step_Generator_Timing_Test is

   pragma Extensions_Allowed (On);

   function Block_Allows_Blended_Entry (Data : Boolean) return Boolean is
      pragma Unreferenced (Data);
   begin
      return True;
   end Block_Allows_Blended_Entry;

   procedure Test_Sub_Tick_Segments (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         type Motor_Name is (X_Motor);
         type Motor_Position is array (Motor_Name) of Dimensionless;
         type Motor_Position_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Delta_Limits is array (Motor_Name) of Dimensionless;

         Tick       : constant Time := 0.001 * s;
         Feedrate   : constant Velocity := 10.0 * mm / s;
         Short_Move : constant Length := 0.000_5 * mm;
         --  The planners' minimum segment time is a hundredth of the step generator's tick, so each short move is
         --  crossed in 50 us at full speed and most of them hold no command.

         package Planner is new
           Motion_Planner.Planner
             (Motor_Name                        => Motor_Name,
              Motor_Position_Map                => Motor_Position_Map,
              Motor_Delta_Limits                => Motor_Delta_Limits,
              Maximum_Deltas_Per_Command        => [others => 1.0],
              Flush_Resetting_Data_Type         => Boolean,
              Flush_Resetting_Data_Type_Default => False,
              Corner_Extra_Data_Type            => Boolean,
              Home_Move_Minimum_Coast_Time      => 0.005 * s,
              Home_Move_Maximum_Tail_Time       => 1.0 * s,
              Interpolation_Time                => Tick / 100.0,
              Max_Corners                       => 10,
              Max_Corners_Extra_Data_Count      => 10,
              Max_Corners_Extra_Data_Storage    => 1_024,
              Input_Queue_Length                => 10,
              Allow_Blended_Entry               => Block_Allows_Blended_Entry);

         package Pause_Planner is new
           Motion_Planner.Planner
             (Motor_Name                        => Motor_Name,
              Motor_Position_Map                => Motor_Position_Map,
              Motor_Delta_Limits                => Motor_Delta_Limits,
              Maximum_Deltas_Per_Command        => [others => 1.0],
              Flush_Resetting_Data_Type         => Boolean,
              Flush_Resetting_Data_Type_Default => False,
              Corner_Extra_Data_Type            => Boolean,
              Home_Move_Minimum_Coast_Time      => 0.005 * s,
              Home_Move_Maximum_Tail_Time       => 1.0 * s,
              Interpolation_Time                => Tick / 100.0,
              Max_Corners                       => 10,
              Max_Corners_Extra_Data_Count      => 10,
              Max_Corners_Extra_Data_Storage    => 1_024,
              Input_Queue_Length                => 10,
              Allow_Blended_Entry               => Block_Allows_Blended_Entry);

         Long_Ends     : constant array (1 .. 4) of Length := [10.0 * mm, 20.0 * mm, 30.0 * mm, 40.0 * mm];
         End_Pos       : constant Position := [X_Axis => Long_Ends (Long_Ends'Last), others => 0.0 * mm];
         Commands      : Natural := 0;
         Safe_Stops    : Natural := 0;
         Last_Pos      : Position := [others => 0.0 * mm];
         Last_Safe     : Boolean := False;
         Largest_Step  : Length := 0.0 * mm;
         Smallest_Step : Length := 0.0 * mm;
         Timed_Out     : Boolean := False;

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
            pragma Unreferenced (Transform, Motor);
         begin
            return Axis = X_Axis;
         end Affects_Axis;

         function To_Motor (Pos : Position; Transform : Boolean) return Motor_Position is
            pragma Unreferenced (Transform);
         begin
            return [X_Motor => Pos (X_Axis) / mm];
         end To_Motor;

         procedure Enqueue_Command
           (Pos             : Position;
            Motor_Pos       : Motor_Position;
            Index           : Command_Index;
            Safe_Stop_After : Boolean;
            Vel_Ratio       : Dimensionless)
         is
            pragma Unreferenced (Motor_Pos, Index, Vel_Ratio);
         begin
            if Commands > 0 then
               Largest_Step := Length'Max (Largest_Step, Pos (X_Axis) - Last_Pos (X_Axis));
               Smallest_Step := Length'Min (Smallest_Step, Pos (X_Axis) - Last_Pos (X_Axis));
            end if;
            Commands := @ + 1;
            Last_Pos := Pos;
            Last_Safe := Safe_Stop_After;
            if Safe_Stop_After then
               Safe_Stops := @ + 1;
            end if;
         end Enqueue_Command;

         function Extrusion_Is_Allowed return Boolean
         is (True);

         function Is_Pause_Plan_Done (Data : Boolean) return Boolean
         is (Data);

         function Pin_Motor (Data, Transform : Boolean; Motor : Motor_Name) return Boolean is
            pragma Unreferenced (Data, Transform, Motor);
         begin
            return False;
         end Pin_Motor;

         procedure Finish_Block (Data : Boolean; Pos : Motor_Position; Index : Command_Index) is
            pragma Unreferenced (Data, Pos, Index);
         begin
            Completion.Finish;
         end Finish_Block;

         procedure Finish_Pause_Block (Data : Boolean; Pos : Motor_Position; Index : Command_Index) is null;
         procedure Handle_Pause (Pos : Position; Index : Command_Index) is null;
         procedure Handle_Resume (Pos : Position; Index : Command_Index) is null;
         procedure Setup_Loop_Move (Data : Boolean) is null;
         procedure Start_Block (Data : Boolean; Index : Command_Index) is null;
         procedure Start_Corner (Index : Command_Index; Data : Boolean) is null;
         procedure Wait_Until_Idle (Index : Command_Index) is null;

         package Stepgen is new
           Step_Generator
             (Planner                      => Planner,
              Pause_Planner                => Pause_Planner,
              Motor_Name                   => Motor_Name,
              Motor_Position               => Motor_Position,
              Kinematic_Transform          => Boolean,
              Setup_Loop_Move              => Setup_Loop_Move,
              Pin_Motor_To_Block_Start     => Pin_Motor,
              Transform_To_Motor_Position  => To_Motor,
              Transform_Motor_Affects_Axis => Affects_Axis,
              Motor_Delta_Limits           => Motor_Delta_Limits,
              Maximum_Deltas_Per_Command   => [others => 1.0],
              Start_Planner_Block          => Start_Block,
              Start_Pause_Planner_Block    => Start_Block,
              Enqueue_Command              => Enqueue_Command,
              Extrusion_Is_Allowed         => Extrusion_Is_Allowed,
              Start_Corner                 => Start_Corner,
              Start_Pause_Corner           => Start_Corner,
              Finish_Planner_Block         => Finish_Block,
              Finish_Pause_Planner_Block   => Finish_Pause_Block,
              Is_Pause_Plan_Done           => Is_Pause_Plan_Done,
              Handle_Pause                 => Handle_Pause,
              Handle_Resume                => Handle_Resume,
              Wait_Until_Idle              => Wait_Until_Idle,
              Interpolation_Time           => Tick,
              Runner_CPU                   => System.Multiprocessors.Not_A_Specific_CPU);

         Params    : constant Motion_Planner.Kinematic_Parameters :=
           (Tangential_Velocity_Max  => Feedrate,
            Axial_Velocity_Maxes     => [others => Feedrate],
            Axial_Acceleration_Maxes => [others => 100.0 * mm / s**2],
            Axial_Jerk_Maxes         => [others => 10_000.0 * mm / s**3],
            Axial_Snap_Maxes         => [others => 1.0E7 * mm / s**4],
            Axial_Crackle_Maxes      => [others => 1.0E11 * mm / s**5],
            others                   => <>);
         Motor_Map : constant Motor_Position_Map :=
           [for Axis in Axis_Name => [X_Motor => (if Axis = X_Axis then 1.0 / mm else 0.0 / mm)]];

         Runners_Stopped : Boolean := False;

         procedure Stop_Runners is
         begin
            if Runners_Stopped then
               return;
            end if;
            Runners_Stopped := True;
            Stepgen.Reset;
            Planner.Reset;
            Pause_Planner.Reset;
            abort Stepgen.Runner, Planner.Runner, Pause_Planner.Runner;
         end Stop_Runners;
      begin
         Planner.Runner.Setup (Params, Motor_Map);
         Pause_Planner.Runner.Setup (Params, Motor_Map);
         --  Each short move continues the long move before it in a straight line, so the path never slows for them.
         for Long_End of Long_Ends loop
            Planner.Enqueue_Move ([X_Axis => Long_End, others => 0.0 * mm], Feedrate, Ignore_Bounds => True);
            if Long_End /= Long_Ends (Long_Ends'Last) then
               Planner.Enqueue_Move
                 ([X_Axis => Long_End + Short_Move, others => 0.0 * mm], Feedrate, Ignore_Bounds => True);
            end if;
         end loop;
         Planner.Enqueue_Flush (False);
         Stepgen.Runner.Setup (False, True);
         select
            Completion.Wait;
         or
            delay 30.0;
            Timed_Out := True;
         end select;
         Stop_Runners;

         T.Assert (not Timed_Out, "Timed out executing the block; commands=" & Commands'Image);
         T.Assert (Smallest_Step >= 0.0 * mm, "Commands never move backwards across segment boundaries");
         --  A tick of motion at the feedrate is the largest a command can move. Advancing the clock past a segment
         --  that held no command would move the next command two ticks along the path.
         T.Assert
           (Largest_Step <= 1.000_001 * Feedrate * Tick,
            "No command moves more than one tick of motion; largest step=" & Largest_Step'Image);
         T.Assert
           (Last_Pos = End_Pos and then Last_Safe and then Safe_Stops = 1,
            "The block ends with one safe stop at its exact end; last=" & Last_Pos'Image);
      exception
         when others =>
            Stop_Runners;
            raise;
      end;
   end Test_Sub_Tick_Segments;

   function All_Tests return Trendy_Test.Test_Group
   is ([1 => Test_Sub_Tick_Segments'Access]);

end Prunt.Step_Generator_Timing_Test;
