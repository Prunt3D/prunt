--  Deterministic benchmark for stereographic corner duration selection.
--
--  Plans repeated square corners with configurable deviation, leg length and extrusion density. Prints planner
--  wall time and total motion duration, checking that the complete path was planned as one block.

pragma Extensions_Allowed (On);

with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Prunt;                use Prunt;
with Prunt.Motion_Planner; use Prunt.Motion_Planner;
with Prunt.Motion_Planner.Planner;

--  Usage: corner_planner_time deviation_mm [move_count [leg_mm [extrusion_per_mm]]]
--  Zero deviation is the stopped control. Moves follow a square, starting at the origin.

procedure Corner_Planner_Time is
   use type Ada.Real_Time.Time;
   Deviation : constant Length := Dimensionless'Value (Ada.Command_Line.Argument (1)) * mm;
   Moves     : constant Positive :=
     (if Ada.Command_Line.Argument_Count >= 2 then Positive'Value (Ada.Command_Line.Argument (2)) else 2);
   Leg       : constant Length :=
     (if Ada.Command_Line.Argument_Count >= 3 then Dimensionless'Value (Ada.Command_Line.Argument (3)) else 10.0) * mm;
   Density   : constant Dimensionless :=
     (if Ada.Command_Line.Argument_Count >= 4 then Dimensionless'Value (Ada.Command_Line.Argument (4)) else 0.0);
   type Motor_Name is (Motor);
   type Motor_Map_Type is array (Axis_Name, Motor_Name) of Curvature;
   type Motor_Limits is array (Motor_Name) of Dimensionless;

   function Block_Allows_Blended_Entry (Data : Boolean) return Boolean is
      pragma Unreferenced (Data);
   begin
      return True;
   end Block_Allows_Blended_Entry;

   package Planner is new
     Prunt.Motion_Planner.Planner
       (Motor_Name,
        Motor_Map_Type,
        Motor_Limits,
        [others => 1.0],
        Boolean,
        False,
        Boolean,
        0.00025 * s,
        1.0E100 * s,
        0.00005 * s,
        Max_Corners         => 5002,
        Allow_Blended_Entry => Block_Allows_Blended_Entry);
   use type Planner.Corners_Index;
   Params        : Kinematic_Parameters := (others => <>);
   Block         : Planner.Block_Handle;
   Position_Now  : Position := [others => 0.0 * mm];
   Started       : Ada.Real_Time.Time;
   Motion_Time   : Time := 0.0 * s;
   Timed_Out     : Boolean;
   Segment_Count : Natural := 0;
   Block_Count   : Natural := 0;
   Timeouts      : Natural := 0;
begin
   if Moves > 5000 then
      raise Constraint_Error with "at most 5000 moves fit in this benchmark block";
   end if;
   Params.Bounds :=
     (Kind    => Rectangular_Workspace,
      Lower_X => -1000.0 * mm,
      Upper_X => 1000.0 * mm,
      Lower_Y => -1000.0 * mm,
      Upper_Y => 1000.0 * mm,
      Lower_Z => -1000.0 * mm,
      Upper_Z => 1000.0 * mm,
      Lower_E => -1.0E100 * mm,
      Upper_E => 1.0E100 * mm);
   Params.Ignore_E_In_XYZE := True;
   Params.Tangential_Velocity_Max := 250.0 * mm / s;
   Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
   Params.Axial_Acceleration_Maxes := [others => 5000.0 * mm / s**2];
   Params.Axial_Jerk_Maxes := [others => 500000.0 * mm / s**3];
   Params.Axial_Snap_Maxes := [others => 500000000.0 * mm / s**4];
   Params.Axial_Crackle_Maxes := [others => 500000000000.0 * mm / s**5];
   Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => Deviation];
   Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := Deviation;
   Started := Ada.Real_Time.Clock;
   Planner.Runner.Setup (Params, [others => [others => 0.0 / mm]]);
   for I in 1 .. Moves loop
      case I mod 4 is
         when 1      =>
            Position_Now (X_Axis) := Leg;

         when 2      =>
            Position_Now (Y_Axis) := Leg;

         when 3      =>
            Position_Now (X_Axis) := 0.0 * mm;

         when 0      =>
            Position_Now (Y_Axis) := 0.0 * mm;

         when others =>
            null;
      end case;
      Position_Now (E_Axis) := @ + Density * Leg;
      Planner.Enqueue_Move (Position_Now, 250.0 * mm / s);
   end loop;
   Planner.Enqueue_Flush (True);
   Blocks_Loop : loop
      Planner.Dequeue (Block, Timed_Out);
      if Timed_Out then
         --  Dequeue gives up after one second, but a large block can take longer to plan. Only a planner that
         --  makes no progress for an hour is an error; abort it so the program exits instead of waiting for it.
         Timeouts := @ + 1;
         if Timeouts >= 3_600 then
            abort Planner.Runner;
            raise Program_Error with "corner benchmark planner timed out";
         end if;
      else
         loop
            Block_Count := @ + 1;
            for I in Planner.Finishing_Corners_Index'First .. Planner.Corner_Count (Block) loop
               Planner.Prepare_Segment (Block, I);
               Motion_Time := @ + Planner.Segment_Time (Block, I);
               Segment_Count := @ + 1;
            end loop;
            exit Blocks_Loop when Planner.Flush_Resetting_Data (Block);
            exit when not Planner.Has_Next_Block (Block);
            Planner.Advance_Block (Block);
         end loop;
         Planner.Release (Block);
      end if;
   end loop Blocks_Loop;
   if Segment_Count /= Moves or else Block_Count /= 1 or else Planner.Next_Block_Pos (Block) /= Position_Now then
      raise Program_Error with "incomplete corner benchmark plan";
   end if;
   Ada.Text_IO.Put_Line
     ("planning_wall_seconds="
      & Long_Float'Image (Long_Float (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Started))));
   Ada.Text_IO.Put_Line ("total_planned_seconds=" & Dimensionless'Image (Motion_Time / s));
   Ada.Text_IO.Put_Line ("planned_segments=" & Segment_Count'Image);
   Ada.Text_IO.Put_Line ("planned_blocks=" & Block_Count'Image);
   --  All validation and timing are complete. Match the other benchmark drivers: shutdown of the persistent
   --  planner task is outside the measurement and must not introduce a Reset rendezvous race.
   Planner.Release (Block);
   Ada.Text_IO.Flush;
   GNAT.OS_Lib.OS_Exit (0);
end Corner_Planner_Time;
