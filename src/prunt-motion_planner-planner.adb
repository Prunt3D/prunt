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

with Prunt.Motion_Planner.Planner.Corner_Blender;
with Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter;
with Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;
with Prunt.Motion_Planner.Planner.Kinematic_Limiter;
with Prunt.Motion_Planner.Planner.Preprocessor;
with Prunt.Motion_Planner.Planner.Step_Rate_Limiter;
with System.Pool_Local;

package body Prunt.Motion_Planner.Planner is

   pragma Extensions_Allowed (On);

   package My_Preprocessor is new Preprocessor;
   package My_Corner_Blender is new Corner_Blender;
   package My_Kinematic_Limiter is new Kinematic_Limiter;
   package My_Early_Kinematic_Limiter is new Early_Kinematic_Limiter;
   package My_Feedrate_Profile_Generator is new Feedrate_Profile_Generator;
   package My_Step_Rate_Limiter is new Step_Rate_Limiter;

   procedure Reset is
   begin
      My_Preprocessor.Reset;
      Runner.Reset_Do_Not_Call_From_Other_Packages;
   end Reset;

   procedure Enqueue_Move
     (Pos : Position; Feedrate : Velocity; Dwell_After : Time := 0.0 * s; Ignore_Bounds : Boolean := False) is
   begin
      My_Preprocessor.Enqueue
        ((Kind => Move_Kind, Dwell_After => Dwell_After, Pos => Pos, Feedrate => Feedrate), Ignore_Bounds);
   end Enqueue_Move;

   function Get_Last_Assigned_Corner_ID return Planner_Corner_ID is
   begin
      return My_Preprocessor.Get_Last_Assigned_Corner_ID;
   end Get_Last_Assigned_Corner_ID;

   procedure Enqueue_Corner_Extra_Data (Data : Corner_Extra_Data_Type) is
      Data_Copy : aliased Corner_Extra_Data_Type := Data;
   begin
      My_Preprocessor.Enqueue
        (Comm => (Kind => Corner_Extra_Data_Kind), Ignore_Bounds => False, Extra => Data_Copy'Access);
   end Enqueue_Corner_Extra_Data;

   procedure Enqueue_Flush (Data : Flush_Resetting_Data_Type; Is_Homing_Move : Boolean := False) is
   begin
      My_Preprocessor.Enqueue ((Kind => Flush_Kind, Flush_Resetting_Data => Data, Is_Homing_Move => Is_Homing_Move));
   end Enqueue_Flush;

   procedure Enqueue_Flush_And_Reset_Position
     (Data           : Flush_Resetting_Data_Type;
      Pos            : Position;
      Is_Homing_Move : Boolean := False;
      Ignore_Bounds  : Boolean := False) is
   begin
      My_Preprocessor.Enqueue
        ((Kind                 => Flush_And_Reset_Position_Kind,
          Flush_Resetting_Data => Data,
          Is_Homing_Move       => Is_Homing_Move,
          Reset_Pos            => Pos),
         Ignore_Bounds);
   end Enqueue_Flush_And_Reset_Position;

   procedure Enqueue_Flush_And_Change_Kinematic_Parameters
     (Data : Flush_Resetting_Data_Type; New_Params : Kinematic_Parameters; Is_Homing_Move : Boolean := False) is
   begin
      My_Preprocessor.Enqueue
        ((Kind                 => Flush_And_Change_Parameters_Kind,
          Flush_Resetting_Data => Data,
          Is_Homing_Move       => Is_Homing_Move,
          New_Params           => New_Params));
   end Enqueue_Flush_And_Change_Kinematic_Parameters;

   procedure Dequeue
     (Block : out Execution_Block; Timed_Out : out Boolean; Waiting_For_Step_Rate_Limiter : out Boolean) is
   begin
      select
         Runner.Dequeue_Do_Not_Call_From_Other_Packages (Block);
         Timed_Out := False;
         Waiting_For_Step_Rate_Limiter := False;
         return;
      then abort
         delay 1.0;
      end select;
      Timed_Out := True;
      Waiting_For_Step_Rate_Limiter := In_Step_Rate_Limiter;
   end Dequeue;

   task body Runner is
      type Block_Wrapper is record
         Block : aliased Execution_Block;
      end record;

      Pool : System.Pool_Local.Unbounded_Reclaim_Pool;

      type Block_Wrapper_Access is access Block_Wrapper with Storage_Pool => Pool;

      Working_Block_Wrapper : constant Block_Wrapper_Access := new Block_Wrapper;
      Block                 : Execution_Block renames Working_Block_Wrapper.Block;

      Reset_Called : Boolean := False;
   begin
      loop
         accept Setup (In_Params : Kinematic_Parameters; In_Map : Motor_Pos_Map) do
            My_Preprocessor.Setup (In_Params);
            My_Step_Rate_Limiter.Setup (In_Map);
         end Setup;

         loop
            My_Preprocessor.Run (Block, Reset_Called);

            if Reset_Called then
               accept Reset_Do_Not_Call_From_Other_Packages;
               My_Step_Rate_Limiter.Reset;
               exit;
            end if;

            if Block.Kind /= Extra_Data_Overflow_Block_Kind then
               if Block.Is_Homing_Move and then Block.N_Corners /= 2 then
                  raise Constraint_Error with "Homing move must have exactly 2 corners.";
               end if;

               My_Corner_Blender.Run (Block);
               My_Early_Kinematic_Limiter.Run (Block);

               loop
                  loop
                     My_Kinematic_Limiter.Run (Block);
                     My_Feedrate_Profile_Generator.Run (Block);

                     exit when
                       (not Block.Is_Homing_Move)
                       or else Block.Feedrate_Profiles (2).Coast >= Home_Move_Minimum_Coast_Time;

                     Block.Limited_Segment_Feedrates (2) := Block.Limited_Segment_Feedrates (2) * 0.9;
                  end loop;

                  declare
                     Needs_New_Profiles : Boolean;
                  begin
                     My_Step_Rate_Limiter.Run (Block, Needs_New_Profiles);
                     exit when not Needs_New_Profiles;
                  end;
               end loop;
            end if;

            select
               accept Dequeue_Do_Not_Call_From_Other_Packages (Out_Block : out Execution_Block) do
                  Out_Block := Block;
               end Dequeue_Do_Not_Call_From_Other_Packages;
            or
               accept Reset_Do_Not_Call_From_Other_Packages;
               My_Step_Rate_Limiter.Reset;
               exit;
            end select;
         end loop;
      end loop;
   end Runner;

   function Segment_Time (Block : Execution_Block; Finishing_Corner : Corners_Index) return Time is
   begin
      return Total_Time (Block.Feedrate_Profiles (Finishing_Corner)) + Block.Corner_Dwell_Times (Finishing_Corner);
   end Segment_Time;

   function Segment_Corner_Distance (Block : Execution_Block; Finishing_Corner : Corners_Index) return Length is
   begin
      return
        abs (Block.Corners (Finishing_Corner) * Block.Params.Axial_Scaler
             - Block.Corners (Finishing_Corner - 1) * Block.Params.Axial_Scaler);
   end Segment_Corner_Distance;

   function Segment_Pos_At_Time
     (Block              : Execution_Block;
      Finishing_Corner   : Finishing_Corners_Index;
      Time_Into_Segment  : Time;
      Is_Past_Accel_Part : out Boolean) return Position
   is
      Start_Curve_Half_Distance : constant Length :=
        Distance_At_T (Block.Beziers (Finishing_Corner - 1), 1.0)
        - Distance_At_T (Block.Beziers (Finishing_Corner - 1), 0.5);
      End_Curve_Half_Distance   : constant Length := Distance_At_T (Block.Beziers (Finishing_Corner), 0.5);
      Mid_Distance              : constant Length :=
        abs (Point_At_T (Block.Beziers (Finishing_Corner), 0.0)
             - Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0));

      Distance : constant Length :=
        Distance_At_Time
          (Block.Feedrate_Profiles (Finishing_Corner),
           Time'Min (Time_Into_Segment, Total_Time (Block.Feedrate_Profiles (Finishing_Corner))),
           Block.Params.Crackle_Max,
           Block.Corner_Velocity_Limits (Finishing_Corner - 1),
           Is_Past_Accel_Part);

      Pos : Scaled_Position;
   begin
      if Time_Into_Segment >= Total_Time (Block.Feedrate_Profiles (Finishing_Corner))
        and then (Finishing_Corner = Block.N_Corners or else Block.Corner_Dwell_Times (Finishing_Corner) /= 0.0 * s)
      then
         --  Ensure the return value will be at the exact position.
         Pos := Point_At_Distance (Block.Beziers (Finishing_Corner), 0.0 * mm);
         pragma Assert (Distance_At_T (Block.Beziers (Finishing_Corner), 0.5) = 0.0 * mm);
         pragma
           Assert
             (Velocity_At_Time
                (Block.Feedrate_Profiles (Finishing_Corner),
                 Total_Time (Block.Feedrate_Profiles (Finishing_Corner)),
                 Block.Params.Crackle_Max,
                 Block.Corner_Velocity_Limits (Finishing_Corner - 1))
              < 0.000_1 * mm / s);
         --  In theory the velocity should be zero but in practice there are some floating point errors here. In
         --  testing the error was always within 1E-14 of zero but there is no reason to check for that level of
         --  precision here.

         return Position (Pos * Block.Params.Axial_Scaler);
      else
         pragma Assert (Time_Into_Segment <= Total_Time (Block.Feedrate_Profiles (Finishing_Corner)));

         if Distance < Start_Curve_Half_Distance then
            Pos :=
              Point_At_Distance
                (Block.Beziers (Finishing_Corner - 1),
                 Distance + Distance_At_T (Block.Beziers (Finishing_Corner - 1), 0.5));
         elsif Distance < Start_Curve_Half_Distance + Mid_Distance or else End_Curve_Half_Distance = 0.0 * mm then
            if Mid_Distance = 0.0 * mm then
               Pos := Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0);
            else
               Pos :=
                 Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0)
                 + (Point_At_T (Block.Beziers (Finishing_Corner), 0.0)
                    - Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0))
                   * ((Distance - Start_Curve_Half_Distance) / Mid_Distance);
            end if;
         else
            Pos :=
              Point_At_Distance
                (Block.Beziers (Finishing_Corner), Distance - Start_Curve_Half_Distance - Mid_Distance);
         end if;

         return Position (Pos * Block.Params.Axial_Scaler);
      end if;
   end Segment_Pos_At_Time;

   function Segment_Vel_Ratio_At_Time
     (Block : Execution_Block; Finishing_Corner : Finishing_Corners_Index; Time_Into_Segment : Time)
      return Dimensionless is
   begin
      if Time_Into_Segment > Total_Time (Block.Feedrate_Profiles (Finishing_Corner)) then
         --  Return 1.0 inside dwell parts so the laser can be set to the programmed power level.
         return 1.0;
      else
         return
           Velocity'Max
             (0.0 * mm / s,
              Velocity_At_Time
                (Block.Feedrate_Profiles (Finishing_Corner),
                 Time_Into_Segment,
                 Block.Params.Crackle_Max,
                 Block.Corner_Velocity_Limits (Finishing_Corner - 1)))
           / Block.Original_Segment_Feedrates (Finishing_Corner);
      end if;
   end Segment_Vel_Ratio_At_Time;

   function Next_Block_Pos (Block : Execution_Block) return Position is
   begin
      return Position (Block.Next_Block_Pos * Block.Params.Axial_Scaler);
   end Next_Block_Pos;

   function Block_Start_Pos (Block : Execution_Block) return Position is
   begin
      return Position (Block.Corners (Corners_Index'First) * Block.Params.Axial_Scaler);
   end Block_Start_Pos;

   function Flush_Resetting_Data (Block : Execution_Block) return Flush_Resetting_Data_Type is
   begin
      return Block.Flush_Resetting_Data;
   end Flush_Resetting_Data;

   function Segment_Accel_Distance (Block : Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length
   is
   begin
      return
        Distance_At_Time
          (Profile     => Block.Feedrate_Profiles (Finishing_Corner),
           T           => Total_Time (Block.Feedrate_Profiles (Finishing_Corner).Accel),
           Max_Crackle => Block.Params.Crackle_Max,
           Start_Vel   => Block.Corner_Velocity_Limits (Finishing_Corner - 1));
   end Segment_Accel_Distance;

   function Block_Kind (Block : Execution_Block) return Execution_Block_Kind is
   begin
      return Block.Kind;
   end Block_Kind;

   function Corner_ID (Block : Execution_Block; Corner : Corners_Index) return Planner_Corner_ID is
   begin
      return Block.First_Corner_ID + Planner_Corner_ID (Corner - Corners_Index'First);
   end Corner_ID;

   procedure Corner_Extra_Data
     (Block   : Execution_Block;
      Corner  : Corners_Index;
      Process : access procedure (Data : in out Corner_Extra_Data_Type)) is
   begin
      Block.Corners_Extra_Data.Process_Range
        ((if Corner = Corners_Index'First
          then Corners_Extra_Data_Index'First
          else Corners_Extra_Data_Index (Block.Corners_Extra_Data_End_Indices (Corner - 1) + 1)),
         Block.Corners_Extra_Data_End_Indices (Corner),
         Process);
   end Corner_Extra_Data;

   function Has_Associated_Overflow_Block (Block : Execution_Block) return Boolean is
   begin
      return Block.Associated_Overflow_Block;
   end Has_Associated_Overflow_Block;

   function Block_Kinematic_Parameters (Block : Execution_Block) return Kinematic_Parameters is
   begin
      return Block.Params;
   end Block_Kinematic_Parameters;

   function Is_Homing_Move (Block : Execution_Block) return Boolean is
   begin
      return Block.Is_Homing_Move;
   end Is_Homing_Move;

end Prunt.Motion_Planner.Planner;
