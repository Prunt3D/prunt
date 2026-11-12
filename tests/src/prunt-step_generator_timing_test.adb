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
with Prunt.Motion_Planner.Planner;
with Prunt.Step_Generator;

package body Prunt.Step_Generator_Timing_Test is

   pragma Extensions_Allowed (On);

   function Block_Allows_Blended_Entry (Data : Boolean) return Boolean is
      pragma Unreferenced (Data);
   begin
      return True;
   end Block_Allows_Blended_Entry;

   procedure Check_Stop_During_Resume_Slew (T : in out Trendy_Test.Operation'Class; Reset_Instead : Boolean) is
   begin
      declare
         type Motor_Name is (X_Motor);
         type Motor_Position is array (Motor_Name) of Dimensionless;
         type Motor_Position_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Delta_Limits is array (Motor_Name) of Dimensionless;

         Tick                 : constant Time := 0.001 * s;
         Feedrate             : constant Velocity := 10.0 * mm / s;
         Full_Speed_Step      : constant Length := Feedrate * Tick;
         Slew_Commands        : constant := 3_000;
         --  The executor's pause and resume slews each last 3 s, which is 3000 ticks here.
         First_Pause_At       : constant := 1_000;
         --  The move is at full speed long before this command.
         Request_After_Resume : constant Natural := (if Reset_Instead then 200 else 1_500);
         --  A cancel 0.2 s into the resume, or a pause halfway up the resume slew, where its speed changes fastest.

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
              Interpolation_Time                => Tick,
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
              Interpolation_Time                => Tick,
              Max_Corners                       => 10,
              Max_Corners_Extra_Data_Count      => 10,
              Max_Corners_Extra_Data_Storage    => 1_024,
              Input_Queue_Length                => 10,
              Allow_Blended_Entry               => Block_Allows_Blended_Entry);

         End_Pos             : constant Position := [X_Axis => 100.0 * mm, others => 0.0 * mm];
         Request_Pause       : access procedure;
         Request_Resume      : access procedure;
         Reset_Gate          : Ada.Synchronous_Task_Control.Suspension_Object;
         Commands            : Natural := 0;
         Pause_Count         : Natural := 0;
         Resume_Start        : Natural := 0;
         --  Commands queued when the first resume plan was requested. The resume slew follows that plan.
         Request_Count       : Natural := 0;
         --  Commands queued when the pause or reset was requested during the resume slew.
         Stop_Count          : Natural := 0;
         --  Commands queued at the first safe stop after that request.
         Last_Pos            : Position := [others => 0.0 * mm];
         Last_Step           : Length := 0.0 * mm;
         Last_Safe           : Boolean := False;
         Step_Grew           : Boolean := False;
         Largest_Step_Change : Length := 0.0 * mm;
         Timed_Out           : Boolean := False;
         Runners_Stopped     : Boolean := False;

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

         procedure Enqueue_Command
           (Pos             : Position;
            Motor_Pos       : Motor_Position;
            Index           : Command_Index;
            Safe_Stop_After : Boolean;
            Vel_Ratio       : Dimensionless)
         is
            pragma Unreferenced (Motor_Pos, Index, Vel_Ratio);
            Step : constant Length := Pos (X_Axis) - Last_Pos (X_Axis);
         begin
            Commands := @ + 1;

            if Request_Count > 0 and then Stop_Count = 0 then
               Largest_Step_Change := Length'Max (Largest_Step_Change, abs (Step - Last_Step));
               --  The command after the request was already a tick further up the resume slew when it was requested.
               if Commands > Request_Count + 1 and then Step > Last_Step + 1.0E-9 * mm then
                  Step_Grew := True;
               end if;

               if Safe_Stop_After then
                  Stop_Count := Commands;
               elsif Reset_Instead then
                  --  Pace commands like hardware, so the resetter's request lands within a few ticks.
                  delay 0.001;
               end if;
            end if;

            Last_Pos := Pos;
            Last_Step := Step;
            Last_Safe := Safe_Stop_After;

            if Resume_Start > 0 and then Request_Count = 0 and then Commands = Resume_Start + Request_After_Resume then
               Request_Count := Commands;
               if Reset_Instead then
                  Ada.Synchronous_Task_Control.Set_True (Reset_Gate);
               else
                  Request_Pause.all;
               end if;
            elsif Pause_Count = 0 and then Commands = First_Pause_At then
               Request_Pause.all;
            end if;
         end Enqueue_Command;

         function Extrusion_Is_Allowed return Boolean
         is (True);

         procedure Finish_Block (Data : Boolean; Pos : Motor_Position; Index : Command_Index) is
            pragma Unreferenced (Data, Pos, Index);
         begin
            Completion.Finish;
         end Finish_Block;

         procedure Finish_Pause_Block (Data : Boolean; Pos : Motor_Position; Index : Command_Index) is null;

         procedure Handle_Pause (Pos : Position; Index : Command_Index) is
            pragma Unreferenced (Index);
         begin
            Pause_Count := @ + 1;
            Pause_Planner.Enqueue_Flush_And_Reset_Position (False, Pos, Ignore_Bounds => True);
            Pause_Planner.Enqueue_Flush (True);
            Request_Resume.all;
         end Handle_Pause;

         procedure Handle_Resume (Pos : Position; Index : Command_Index) is
            pragma Unreferenced (Pos, Index);
         begin
            if Resume_Start = 0 then
               Resume_Start := Commands;
            end if;
            Pause_Planner.Enqueue_Flush (True);
         end Handle_Resume;

         function Is_Pause_Plan_Done (Data : Boolean) return Boolean
         is (Data);

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
            abort Resetter;
            Stepgen.Reset;
            Planner.Reset;
            Pause_Planner.Reset;
            abort Stepgen.Runner, Planner.Runner, Pause_Planner.Runner;
         end Stop_Runners;
      begin
         Request_Pause := Stepgen.Pause'Access;
         Request_Resume := Stepgen.Resume'Access;
         Planner.Runner.Setup (Params, Motor_Map);
         Pause_Planner.Runner.Setup (Params, Motor_Map);
         Planner.Enqueue_Move (End_Pos, Feedrate, Ignore_Bounds => True);
         Planner.Enqueue_Flush (False);
         Stepgen.Runner.Setup (False, True);
         select
            Completion.Wait;
         or
            delay 30.0;
            Timed_Out := True;
         end select;
         Stop_Runners;

         T.Assert
           (not Timed_Out,
            "Timed out; commands="
            & Commands'Image
            & ", pauses="
            & Pause_Count'Image
            & ", resume start="
            & Resume_Start'Image
            & ", request="
            & Request_Count'Image
            & ", stop="
            & Stop_Count'Image);
         T.Assert
           (Request_Count > 0 and then Stop_Count > 0,
            "The request is made during the resume slew and motion then stops; request="
            & Request_Count'Image
            & ", stop="
            & Stop_Count'Image);
         if Reset_Instead then
            --  Finishing the resume slew and then slewing down from full speed takes about 5800 commands.
            T.Assert
              (Stop_Count - Request_Count < Slew_Commands,
               "A reset during the resume slew must stop in less than a full pause slew; took"
               & Integer'Image (Stop_Count - Request_Count)
               & " commands");
            T.Assert (Pause_Count = 1 and then Last_Safe, "The reset ends at a safe stop without a second pause");
         else
            --  Finishing the resume slew and then slewing down from full speed takes about 4500 commands.
            T.Assert
              (Stop_Count - Request_Count <= Request_After_Resume + 5,
               "A pause during the resume slew must slew back down from where the resume had reached; took"
               & Integer'Image (Stop_Count - Request_Count)
               & " commands after"
               & Request_After_Resume'Image
               & " commands of resume slew");
            T.Assert (not Step_Grew, "Motion slows from the first tick after the executor sees the pause");
            T.Assert
              (Pause_Count = 2 and then Last_Pos = End_Pos,
               "The second pause resumes and the move finishes; pauses="
               & Pause_Count'Image
               & ", last="
               & Last_Pos'Image);
         end if;
         T.Assert
           (Largest_Step_Change <= 0.01 * Full_Speed_Step,
            "The speed turns around without a jump; largest step change=" & Largest_Step_Change'Image);
      exception
         when others =>
            Stop_Runners;
            raise;
      end;
   end Check_Stop_During_Resume_Slew;

   procedure Test_Pause_During_Resume_Slew (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Stop_During_Resume_Slew (T, Reset_Instead => False);
   end Test_Pause_During_Resume_Slew;

   procedure Test_Reset_During_Resume_Slew (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Stop_During_Resume_Slew (T, Reset_Instead => True);
   end Test_Reset_During_Resume_Slew;

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
   is ([Test_Sub_Tick_Segments'Access, Test_Pause_During_Resume_Slew'Access, Test_Reset_During_Resume_Slew'Access]);

end Prunt.Step_Generator_Timing_Test;
