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

with Prunt.Motion_Planner.Planner.Preprocessor;
with Prunt.Motion_Planner.Planner.Corner_Blender;
with Prunt.Motion_Planner.Planner.Kinematic_Limiter;
with Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;

package body Prunt.Motion_Planner.Planner is

   package My_Preprocessor is new Preprocessor;
   package My_Corner_Blender is new Corner_Blender;
   package My_Kinematic_Limiter is new Kinematic_Limiter;
   package My_Feedrate_Profile_Generator is new Feedrate_Profile_Generator;

   procedure Enqueue (Comm : Command; Ignore_Bounds : Boolean := False) is
   begin
      My_Preprocessor.Enqueue (Comm, Ignore_Bounds);
   end Enqueue;

   procedure Dequeue (Block : out Execution_Block) is
   begin
      Runner.Dequeue_Do_Not_Call_From_Other_Packages (Block);
   end Dequeue;

   task body Runner is
      type Block_Wrapper is record
         Block : aliased Execution_Block;
      end record;

      pragma Warnings (Off, "use of an anonymous access type allocator");
      Working_Block_Wrapper : constant access Block_Wrapper := new Block_Wrapper;
      Block renames Working_Block_Wrapper.Block;
      pragma Warnings (On, "use of an anonymous access type allocator");
   begin
      accept Setup (In_Params : Kinematic_Parameters) do
         My_Preprocessor.Setup (In_Params);
      end Setup;

      loop
         My_Preprocessor.Run (Block);

         if Is_Homing_Move (Block.Flush_Extra_Data) and Block.N_Corners /= 2 then
            raise Constraint_Error with "Homing move must have exactly 2 corners.";
         end if;

         My_Corner_Blender.Run (Block);

         loop
            My_Kinematic_Limiter.Run (Block);
            My_Feedrate_Profile_Generator.Run (Block);

            exit when (not Is_Homing_Move (Block.Flush_Extra_Data))
              or else Block.Feedrate_Profiles (2).Coast >= Home_Move_Minimum_Coast_Time;

            Block.Segment_Feedrates (2) := Block.Segment_Feedrates (2) * 0.9;
         end loop;

         accept Dequeue_Do_Not_Call_From_Other_Packages (Out_Block : out Execution_Block) do
            Out_Block := Block;
         end Dequeue_Do_Not_Call_From_Other_Packages;
      end loop;
   end Runner;

   function Segment_Time (Block : Execution_Block; Finishing_Corner : Corners_Index) return Time is
   begin
      return Total_Time (Block.Feedrate_Profiles (Finishing_Corner));
   end Segment_Time;

   function Segment_Corner_Distance (Block : Execution_Block; Finishing_Corner : Corners_Index) return Length is
   begin
      return
        abs
        (Block.Corners (Finishing_Corner) / Block.Params.Axial_Scaler -
         Block.Corners (Finishing_Corner - 1) / Block.Params.Axial_Scaler);
   end Segment_Corner_Distance;

   function Segment_Pos_At_Time
     (Block              :     Execution_Block;
      Finishing_Corner   :     Corners_Index;
      Time_Into_Segment  :     Time;
      Is_Past_Accel_Part : out Boolean)
      return Position
   is
      Start_Curve_Half_Distance : constant Length :=
        Distance_At_T (Block.Beziers (Finishing_Corner - 1), 1.0) -
        Distance_At_T (Block.Beziers (Finishing_Corner - 1), 0.5);
      End_Curve_Half_Distance   : constant Length := Distance_At_T (Block.Beziers (Finishing_Corner), 0.5);
      Mid_Distance              : constant Length :=
        abs
        (Point_At_T (Block.Beziers (Finishing_Corner), 0.0) - Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0));

      Distance : constant Length :=
        Distance_At_Time
          (Block.Feedrate_Profiles (Finishing_Corner),
           Time_Into_Segment,
           Block.Params.Crackle_Max,
           Block.Corner_Velocity_Limits (Finishing_Corner - 1),
           Is_Past_Accel_Part);

      Pos                       : Scaled_Position;
      Tangent                   : Scaled_Position_Offset;
      Unscaled_Velocity_Tangent : Axial_Velocities;
   begin
      if Time_Into_Segment = Total_Time (Block.Feedrate_Profiles (Finishing_Corner)) and
        Finishing_Corner = Block.N_Corners
      then
         --  Ensure the return value here will be equal to start of the next block if the position was not reset.
         Pos := Point_At_Distance (Block.Beziers (Finishing_Corner), 0.0 * mm);
         pragma Assert (Distance_At_T (Block.Beziers (Finishing_Corner), 0.5) = 0.0 * mm);
         pragma Assert
           (Velocity_At_Time
              (Block.Feedrate_Profiles (Finishing_Corner), Time_Into_Segment, Block.Params.Crackle_Max,
               Block.Corner_Velocity_Limits (Finishing_Corner - 1)) <
            0.000_1 * mm / s);
         --  In theory the velocity should be zero but in practice there are some floating point errors here. In
         --  testing the error was always within 1E-14 of zero but there is no reason to check for that level of
         --  precision here.

         return Position (Pos / Block.Params.Axial_Scaler);
      elsif Distance < Start_Curve_Half_Distance then
         Pos     :=
           Point_At_Distance
             (Block.Beziers (Finishing_Corner - 1),
              Distance + Distance_At_T (Block.Beziers (Finishing_Corner - 1), 0.5));
         Tangent :=
           Tangent_At_Distance
             (Block.Beziers (Finishing_Corner - 1),
              Distance + Distance_At_T (Block.Beziers (Finishing_Corner - 1), 0.5));
      elsif Distance < Start_Curve_Half_Distance + Mid_Distance or End_Curve_Half_Distance = 0.0 * mm then
         if Mid_Distance = 0.0 * mm then
            Pos     := Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0);
         Tangent :=
           Tangent_At_Distance
             (Block.Beziers (Finishing_Corner - 1),
              Distance_At_T (Block.Beziers (Finishing_Corner - 1), 1.0));
         else
            Pos     :=
              Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0) +
              (Point_At_T (Block.Beziers (Finishing_Corner), 0.0) -
                 Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0)) *
              ((Distance - Start_Curve_Half_Distance) / Mid_Distance);
            Tangent :=
              Point_At_T
                (Block.Beziers (Finishing_Corner), 0.0) - Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0);
         end if;
      else
         Pos     :=
           Point_At_Distance (Block.Beziers (Finishing_Corner), Distance - Start_Curve_Half_Distance - Mid_Distance);
         Tangent :=
           Tangent_At_Distance (Block.Beziers (Finishing_Corner), Distance - Start_Curve_Half_Distance - Mid_Distance);
      end if;

      Unscaled_Velocity_Tangent :=
        (Tangent / abs Tangent) *
        Velocity_At_Time
          (Block.Feedrate_Profiles (Finishing_Corner), Time_Into_Segment, Block.Params.Crackle_Max,
           Block.Corner_Velocity_Limits (Finishing_Corner - 1)) /
        Block.Params.Axial_Scaler;

      Pos (E_Axis) := Pos (E_Axis) + Block.Params.Pressure_Advance_Time * Unscaled_Velocity_Tangent (E_Axis);

      return Position (Pos / Block.Params.Axial_Scaler);
   end Segment_Pos_At_Time;

   function Next_Block_Pos (Block : Execution_Block) return Position is
   begin
      return Position (Block.Next_Block_Pos / Block.Params.Axial_Scaler);
   end Next_Block_Pos;

   function Flush_Extra_Data (Block : Execution_Block) return Flush_Extra_Data_Type is
   begin
      return Block.Flush_Extra_Data;
   end Flush_Extra_Data;

   function Segment_Accel_Distance (Block : Execution_Block; Finishing_Corner : Corners_Index) return Length is
   begin
      return
        Distance_At_Time
          (Profile     => Block.Feedrate_Profiles (Finishing_Corner),
           T           => Total_Time (Block.Feedrate_Profiles (Finishing_Corner).Accel),
           Max_Crackle => Block.Params.Crackle_Max,
           Start_Vel   => Block.Corner_Velocity_Limits (Finishing_Corner - 1));
   end Segment_Accel_Distance;

   function Corner_Extra_Data (Block : Execution_Block; Corner : Corners_Index) return Corner_Extra_Data_Type is
   begin
      return Block.Corners_Extra_Data (Corner);
   end Corner_Extra_Data;

end Prunt.Motion_Planner.Planner;
