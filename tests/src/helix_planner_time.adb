--  Deterministic benchmark for helix corner validation and compiler comparisons.
--
--  Plans a deterministic synthetic path dominated by G2/G3-style helices so that the helix branches of the corner
--  blender are exercised: arc-arc (S-bends and same-direction reversals), line-arc, arc-line and Z helices across a
--  range of radii and sweeps. Prints wall time, total planned time and a bit-level checksum of every segment time so
--  that before/after runs can be compared exactly.

pragma Extensions_Allowed (On);

with Ada.Command_Line;
with Ada.Numerics;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;
with Interfaces;
with Prunt;                        use Prunt;
with Prunt.Input_Shapers;
with Prunt.Motion_Planner;         use Prunt.Motion_Planner;
with Prunt.Motion_Planner.Planner;

procedure Helix_Planner_Time is
   use Ada.Numerics.Long_Elementary_Functions;
   use type Ada.Real_Time.Time;
   use type Interfaces.Unsigned_64;

   type Benchmark_Case is (Stereographic_Case, Circular_Case, Biarc_Case);
   subtype Benchmark_Move_Count is Positive range 1 .. 50_000;
   Selected_Case : constant Benchmark_Case :=
     (if Ada.Command_Line.Argument_Count >= 1 then Benchmark_Case'Value (Ada.Command_Line.Argument (1))
      else Stereographic_Case);
   Move_Count    : constant Benchmark_Move_Count :=
     (if Ada.Command_Line.Argument_Count >= 2
      then Benchmark_Move_Count'Value (Ada.Command_Line.Argument (2)) else 20_000);
   --  A third argument of 0 disables extrusion, so E stays fixed at every corner.
   E_Per_mm      : constant Long_Float :=
     (if Ada.Command_Line.Argument_Count >= 3 then Long_Float'Value (Ada.Command_Line.Argument (3)) else 0.04);
   --  A fourth argument of STEEP gives every arc a large Z rise and makes some arcs full circles.
   Steep         : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 4 and then Ada.Command_Line.Argument (4) = "STEEP";
   --  Optional fifth argument: CSV of exact segment-time bits for diagnosing checksum differences.
   Write_Segments : constant Boolean := Ada.Command_Line.Argument_Count >= 5;
   Segment_Output : Ada.Text_IO.File_Type;
   Climbing      : Boolean := True;

   type Motor_Name is (X_Motor, Y_Motor, Z_Motor, E_Motor);
   type Motor_Position_Map is array (Axis_Name, Motor_Name) of Curvature;
   type Motor_Delta_Limits is array (Motor_Name) of Dimensionless;

   package Planner is new
     Prunt.Motion_Planner.Planner
       (Motor_Name                         => Motor_Name,
        Motor_Position_Map                 => Motor_Position_Map,
        Motor_Delta_Limits                 => Motor_Delta_Limits,
        Maximum_Deltas_Per_Command         => [others => 1.0],
        Flush_Resetting_Data_Type          => Boolean,
        Flush_Resetting_Data_Type_Default  => False,
        Corner_Extra_Data_Type             => Boolean,
        Home_Move_Minimum_Coast_Time       => 0.000_25 * s,
        Home_Move_Maximum_Tail_Time        => 1.0E100 * s,
        Interpolation_Time                 => 0.000_05 * s,
        Max_Corners                        => 50_002);

   use type Planner.Corners_Index;

   Deviation : constant Length := 0.02 * mm;

   Params : Kinematic_Parameters :=
     (Bounds                       =>
        (Kind    => Rectangular_Workspace,
         Lower_Z => 0.0 * mm,
         Upper_Z => 300.0 * mm,
         Lower_E => -1.0E100 * mm,
         Upper_E => 1.0E100 * mm,
         Lower_X => 0.0 * mm,
         Upper_X => 300.0 * mm,
         Lower_Y => 0.0 * mm,
         Upper_Y => 300.0 * mm),
      Ignore_E_In_XYZE              => True,
      Tangential_Velocity_Max       => 250.0 * mm / s,
      Axial_Velocity_Maxes          =>
        [X_Axis | Y_Axis => 250.0 * mm / s, Z_Axis => 25.0 * mm / s, E_Axis => 80.0 * mm / s],
      Axial_Acceleration_Maxes      => [others => 5_000.0 * mm / s ** 2],
      Axial_Jerk_Maxes              => [others => 500_000.0 * mm / s ** 3],
      Axial_Snap_Maxes              => [others => 500_000_000.0 * mm / s ** 4],
      Axial_Crackle_Maxes           => [others => 500_000_000_000.0 * mm / s ** 5],
      Cornering                     =>
        (Kind                 => Stereographic,
         Stereographic_Params =>
           (Axial_Deviation_Maxes    => [others => Deviation],
            Corner_Miss_Distance_Max => Deviation,
            Shape_Bias               => 0.0,
            Circularity              => 0.0)),
      Extrusion_Velocity_Change_Max => <>,
      Extrusion_Rounding_Tolerance  => <>,
      Axial_Shapers                 => [others => (Kind => Prunt.Input_Shapers.No_Shaper)]);

   Motor_Map : constant Motor_Position_Map :=
     [X_Axis => [X_Motor => 1.0 / mm, others => 0.0 / mm],
      Y_Axis => [Y_Motor => 1.0 / mm, others => 0.0 / mm],
      Z_Axis => [Z_Motor => 1.0 / mm, others => 0.0 / mm],
      E_Axis => [E_Motor => 1.0 / mm, others => 0.0 / mm]];

   --  Park-Miller minimal standard generator, so every run sees the same path.
   Seed : Interfaces.Unsigned_64 := 20_261_015;
   function Uniform return Long_Float;
   function Uniform return Long_Float is
   begin
      Seed := (Seed * 48_271) mod 2_147_483_647;
      return Long_Float (Seed) / 2_147_483_647.0;
   end Uniform;
   function Uniform (Low, High : Long_Float) return Long_Float
   is (Low + (High - Low) * Uniform);

   function To_Bits is new Ada.Unchecked_Conversion (Long_Float, Interfaces.Unsigned_64);

   Block : Planner.Block_Handle;

   Pi            : constant Long_Float := Ada.Numerics.Pi;
   Center_XY     : constant Long_Float := 150.0;
   --  Heading is the direction of travel in radians; it drifts back toward the bed centre so the path stays in bounds.
   Heading       : Long_Float := 0.0;
   X             : Long_Float := Center_XY;
   Y             : Long_Float := Center_XY;
   Z             : Long_Float := 0.3;
   E             : Long_Float := 0.0;
   Current_Pos   : Position;
   Helix_Count   : Natural := 0;
   Line_Count    : Natural := 0;
   Timed_Out     : Boolean;
   Timeout_Count : Natural := 0;
   Block_Count   : Natural := 0;
   Segment_Count : Natural := 0;
   Total         : Time := 0.0 * s;
   Checksum      : Interfaces.Unsigned_64 := 0;
   Started       : Ada.Real_Time.Time;

   function To_Pos return Position
   is ([X_Axis => Dimensionless (X) * mm,
        Y_Axis => Dimensionless (Y) * mm,
        Z_Axis => Dimensionless (Z) * mm,
        E_Axis => Dimensionless (E) * mm]);
begin
   case Selected_Case is
      when Stereographic_Case =>
         null;
      when Circular_Case =>
         Params.Cornering :=
           (Kind            => Circular,
            Circular_Params =>
              (Axial_Deviation_Maxes => [others => Deviation], Corner_Miss_Distance_Max => Deviation, others => <>));
      when Biarc_Case =>
         Params.Cornering :=
           (Kind         => Biarc,
            Biarc_Params =>
              (Axial_Deviation_Maxes => [others => Deviation], Corner_Miss_Distance_Max => Deviation, others => <>));
   end case;

   Ada.Text_IO.Put_Line ("case=" & Selected_Case'Image & " moves=" & Move_Count'Image);
   if Write_Segments then
      Ada.Text_IO.Create (Segment_Output, Ada.Text_IO.Out_File, Ada.Command_Line.Argument (5));
      Ada.Text_IO.Put_Line (Segment_Output, "segment,time_bits");
   end if;
   Started := Ada.Real_Time.Clock;
   Planner.Runner.Setup (Params, Motor_Map);
   --  The planner starts at the origin. Moving to the bed centre first keeps the whole path in bounds; an initial
   --  reset flush would produce a block that nobody dequeues while this procedure is still enqueueing.
   Current_Pos := To_Pos;
   Planner.Enqueue_Move (Current_Pos, 100.0 * mm / s);

   for Move in 1 .. Move_Count loop
      declare
         --  A random turn at the junction gives a corner that the blender must round. A quarter of the junctions are
         --  left tangent apart from a small kink, which is the common arc-fitted-slicer case.
         Kink       : constant Long_Float :=
           (if Uniform < 0.25 then Uniform (-0.05, 0.05) else Uniform (-0.6 * Pi, 0.6 * Pi));
         Far_Out    : constant Boolean := Sqrt ((X - Center_XY) ** 2 + (Y - Center_XY) ** 2) > 60.0;
         Choice     : constant Long_Float := Uniform;
         Speed      : constant Velocity := Dimensionless (Uniform (40.0, 200.0)) * mm / s;
         Layer_Rise : constant Boolean := Uniform < 0.02;
         Old_X      : constant Long_Float := X;
         Old_Y      : constant Long_Float := Y;
      begin
         Heading := Heading + Kink;
         if Far_Out then
            Heading := Arctan (Center_XY - Y, Center_XY - X);
         end if;

         if Choice < 0.2 and then not Layer_Rise then
            --  Straight line, giving line-arc and arc-line junctions.
            declare
               Length_mm : constant Long_Float := Uniform (0.3, 8.0);
            begin
               X := X + Length_mm * Cos (Heading);
               Y := Y + Length_mm * Sin (Heading);
               E := E + E_Per_mm * Length_mm;
               Current_Pos := To_Pos;
               Planner.Enqueue_Move (Current_Pos, Speed);
               Line_Count := Line_Count + 1;
            end;
         else
            declare
               Clockwise : constant Boolean := Uniform < 0.5;
               Radius    : constant Long_Float :=
                 (if Uniform < 0.3 then Uniform (0.4, 2.0) else Uniform (2.0, 40.0));
               Full      : constant Boolean := Steep and then Uniform < 0.05;
               Sweep     : constant Long_Float :=
                 (if Full then 2.0 * Pi else Uniform (0.1, 1.5)) * (if Clockwise then -1.0 else 1.0);
               --  The centre lies on the left of the heading for counter-clockwise travel and on the right for
               --  clockwise travel, so the arc starts tangent to Heading.
               Normal    : constant Long_Float := Heading + (if Clockwise then -0.5 * Pi else 0.5 * Pi);
               CX        : constant Long_Float := Old_X + Radius * Cos (Normal);
               CY        : constant Long_Float := Old_Y + Radius * Sin (Normal);
               Start_Phi : constant Long_Float := Arctan (Old_Y - CY, Old_X - CX);
               End_Phi   : constant Long_Float := Start_Phi + Sweep;
               Arc_mm    : constant Long_Float := Radius * abs Sweep;
               Rise      : constant Long_Float :=
                 (if Steep then Uniform (0.2, 4.0) * (if Climbing then 1.0 else -1.0)
                  elsif Layer_Rise then 0.3 else 0.0);
            begin
               if Full then
                  X := Old_X;
                  Y := Old_Y;
               else
                  X := CX + Radius * Cos (End_Phi);
                  Y := CY + Radius * Sin (End_Phi);
               end if;
               if Steep and then (Z + Rise > 280.0 or else Z + Rise < 1.0) then
                  Climbing := not Climbing;
               end if;
               Z := Z + (if Steep and then (Z + Rise > 280.0 or else Z + Rise < 1.0) then -Rise else Rise);
               E := E + (if Layer_Rise then 0.0 else E_Per_mm * Arc_mm);
               Heading := Heading + Sweep;
               Current_Pos := To_Pos;
               Planner.Enqueue_Helix
                 (Pos       => Current_Pos,
                  Center    =>
                    [X_Axis => Dimensionless (CX) * mm,
                     Y_Axis => Dimensionless (CY) * mm,
                     Z_Axis => 0.0 * mm,
                     E_Axis => 0.0 * mm],
                  Clockwise => Clockwise,
                  Feedrate  => Speed);
               Helix_Count := Helix_Count + 1;
            end;
         end if;
      end;
   end loop;

   Planner.Enqueue_Flush (True);
   loop
      Planner.Dequeue (Block, Timed_Out);
      if Timed_Out then
         Timeout_Count := Timeout_Count + 1;
         if Timeout_Count >= 3_600 then
            raise Program_Error with "planner did not reach the terminal flush within one hour";
         end if;
      else
         Block_Count := Block_Count + 1;
         if Planner.Corner_Count (Block) >= Planner.Finishing_Corners_Index'First then
            for Corner in Planner.Finishing_Corners_Index'First .. Planner.Corner_Count (Block) loop
               declare
                  T : constant Time := Planner.Segment_Time (Block, Corner);
               begin
                  Total := Total + T;
                  Segment_Count := Segment_Count + 1;
                  Checksum := (Checksum xor To_Bits (Long_Float (T / s))) * 1_099_511_628_211;
                  if Write_Segments then
                     Ada.Text_IO.Put_Line
                       (Segment_Output, Segment_Count'Image & "," & To_Bits (Long_Float (T / s))'Image);
                  end if;
               end;
            end loop;
         end if;
         exit when Planner.Flush_Resetting_Data (Block);
         Planner.Release (Block);
      end if;
   end loop;

   if Planner.Next_Block_Pos (Block) /= Current_Pos then
      raise Program_Error with "terminal planner position differs from the generated position";
   end if;

   Ada.Text_IO.Put_Line
     ("planning_wall_seconds="
      & Long_Float'Image (Long_Float (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Started))));
   Ada.Text_IO.Put_Line ("helix_moves=" & Helix_Count'Image & " line_moves=" & Line_Count'Image);
   Ada.Text_IO.Put_Line ("planned_blocks=" & Block_Count'Image & " planned_segments=" & Segment_Count'Image);
   Ada.Text_IO.Put_Line ("total_planned_seconds=" & Long_Float'Image (Long_Float (Total / s)));
   Ada.Text_IO.Put_Line ("segment_time_checksum=" & Checksum'Image);
   if Write_Segments then
      Ada.Text_IO.Close (Segment_Output);
   end if;
   --  All validation and output are complete. The persistent planner task can defer abort while waiting for input.
   Planner.Release (Block);
   Ada.Text_IO.Flush;
   GNAT.OS_Lib.OS_Exit (0);
end Helix_Planner_Time;
