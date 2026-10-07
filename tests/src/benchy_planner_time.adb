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

pragma Extensions_Allowed (On);

with Ada.Command_Line;
with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Prunt;                        use Prunt;
with Prunt.Input_Shapers;
with Prunt.Motion_Planner;         use Prunt.Motion_Planner;
with Prunt.Motion_Planner.Planner;

procedure Benchy_Planner_Time is
   type Benchmark_Case is
     (Baseline, Tight_E_Deviation, Zero_E_Deviation, E_Acceleration, X_Crackle, Mixed_Limits, Circular_Corners,
      XYZ_Only, Relaxed_E_Limits, Stopped_Path, E_Jump_Zero, E_Jump_One, E_Jump_Five, Rounding_Stress);
   Selected_Case : constant Benchmark_Case :=
     (if Ada.Command_Line.Argument_Count >= 1 then Benchmark_Case'Value (Ada.Command_Line.Argument (1)) else Baseline);
   Gcode_Path : constant String :=
     (if Ada.Command_Line.Argument_Count >= 2 then Ada.Command_Line.Argument (2)
      else "../prunt_simulator/uploads/benchy.gcode");
   use type Ada.Real_Time.Time;

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
        --  Keep one spare corner so the stress fixture's terminal flush joins the motion block instead of
        --  following an automatic capacity flush with an empty block.
        Max_Corners                        => 50_002);

   use type Planner.Corners_Index;

   Params : Kinematic_Parameters :=
     (Bounds                   =>
        (Kind    => Rectangular_Workspace,
         Lower_Z => 0.0 * mm,
         Upper_Z => 300.0 * mm,
         Lower_E => -1.0E100 * mm,
         Upper_E => 1.0E100 * mm,
         Lower_X => 0.0 * mm,
         Upper_X => 300.0 * mm,
         Lower_Y => 0.0 * mm,
         Upper_Y => 300.0 * mm),
      Ignore_E_In_XYZE         => True,
      Tangential_Velocity_Max  => 250.0 * mm / s,
      Axial_Velocity_Maxes     =>
        [X_Axis | Y_Axis => 250.0 * mm / s, Z_Axis => 25.0 * mm / s, E_Axis => 80.0 * mm / s],
      Axial_Acceleration_Maxes => [others => 5_000.0 * mm / s ** 2],
      Axial_Jerk_Maxes         => [others => 500_000.0 * mm / s ** 3],
      Axial_Snap_Maxes         => [others => 500_000_000.0 * mm / s ** 4],
      Axial_Crackle_Maxes      => [others => 500_000_000_000.0 * mm / s ** 5],
      Cornering                =>
        (Kind                 => Stereographic,
         Stereographic_Params =>
           (Axial_Deviation_Maxes    => [others => 0.02 * mm],
            Corner_Miss_Distance_Max => 0.02 * mm,
            Shape_Bias               => 0.0,
            Circularity              => 0.0)),
      Extrusion_Cornering      => <>,
      Extrusion_Rounding_Tolerance => <>,
      Axial_Shapers            => [others => (Kind => Prunt.Input_Shapers.No_Shaper)]);

   Motor_Map : constant Motor_Position_Map :=
     [X_Axis => [X_Motor => 1.0 / mm, others => 0.0 / mm],
      Y_Axis => [Y_Motor => 1.0 / mm, others => 0.0 / mm],
      Z_Axis => [Z_Motor => 1.0 / mm, others => 0.0 / mm],
      E_Axis => [E_Motor => 1.0 / mm, others => 0.0 / mm]];

   function Has_Value (Line : String; Letter : Character) return Boolean;
   function Value_Of (Line : String; Letter : Character) return Long_Float;

   function Has_Value (Line : String; Letter : Character) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Line, String'(1 => Letter)) /= 0;
   end Has_Value;

   function Value_Of (Line : String; Letter : Character) return Long_Float is
      First : constant Natural := Ada.Strings.Fixed.Index (Line, String'(1 => Letter)) + 1;
      Last  : Natural := First;
   begin
      while Last <= Line'Last and then Line (Last) /= ' ' and then Line (Last) /= ';' loop
         Last := Last + 1;
      end loop;
      return Long_Float'Value (Line (First .. Last - 1));
   end Value_Of;

   File          : Ada.Text_IO.File_Type;
   Segment_Output : Ada.Text_IO.File_Type;
   Write_Segments : constant Boolean := Ada.Command_Line.Argument_Count >= 3;
   Line          : String (1 .. 1_024);
   Last          : Natural;
   Current_Pos   : Position := [others => 0.0 * mm];
   Feedrate      : Velocity := 0.1 * mm / s;
   Relative_E    : Boolean := False;
   type Block_Wrapper is record
      Block : aliased Planner.Execution_Block;
   end record;
   type Block_Wrapper_Access is access Block_Wrapper;
   Working_Block : constant Block_Wrapper_Access := new Block_Wrapper;
   Block         : Planner.Execution_Block renames Working_Block.Block;
   Timed_Out     : Boolean;
   Total         : Time := 0.0 * s;
   G1_Count      : Natural := 0;
   Moving_Count  : Natural := 0;
   Block_Count   : Natural := 0;
   Segment_Count : Natural := 0;
   Timeout_Count : Natural := 0;
   Final_Pos     : Position := [others => 0.0 * mm];
   Started       : Ada.Real_Time.Time;
begin
   case Selected_Case is
      when Baseline | XYZ_Only => null;
      when Rounding_Stress =>
         Params.Bounds.Upper_X := 6000.0 * mm;
         Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis) := 0.0 * mm;
      when E_Jump_Zero | E_Jump_One | E_Jump_Five =>
         Params.Extrusion_Cornering :=
           (Kind => Instantaneous_Velocity_Change,
            Velocity_Change_Max =>
              (case Selected_Case is
                 when E_Jump_One => 1.0 * mm / s,
                 when E_Jump_Five => 5.0 * mm / s,
                 when others => 0.0 * mm / s));
      when Relaxed_E_Limits =>
         Params.Axial_Velocity_Maxes (E_Axis) := 1.0E6 * mm / s;
         Params.Axial_Acceleration_Maxes (E_Axis) := 1.0E12 * mm / s ** 2;
         Params.Axial_Jerk_Maxes (E_Axis) := 1.0E18 * mm / s ** 3;
         Params.Axial_Snap_Maxes (E_Axis) := 1.0E24 * mm / s ** 4;
         Params.Axial_Crackle_Maxes (E_Axis) := 1.0E30 * mm / s ** 5;
      when Stopped_Path =>
         --  Match the geometry used when the old planner's zero E allowance prevents corner construction.
         Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => 0.0 * mm];
         Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 0.0 * mm;
      when Tight_E_Deviation =>
         Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis) := 0.0002 * mm;
      when Zero_E_Deviation =>
         Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis) := 0.0 * mm;
      when E_Acceleration =>
         Params.Axial_Acceleration_Maxes (E_Axis) := 50.0 * mm / s ** 2;
      when X_Crackle =>
         Params.Axial_Crackle_Maxes (X_Axis) := 500_000_000.0 * mm / s ** 5;
      when Mixed_Limits =>
         Params.Axial_Acceleration_Maxes (E_Axis) := 50.0 * mm / s ** 2;
         Params.Axial_Crackle_Maxes (X_Axis) := 500_000_000.0 * mm / s ** 5;
      when Circular_Corners =>
         Params.Cornering :=
           (Kind => Circular,
            Circular_Params =>
              (Axial_Deviation_Maxes => [others => 0.02 * mm],
               Corner_Miss_Distance_Max => 0.02 * mm, others => <>));
   end case;
   if Ada.Command_Line.Argument_Count >= 4 then
      Params.Extrusion_Rounding_Tolerance := Dimensionless'Value (Ada.Command_Line.Argument (4)) * mm;
   end if;
   Ada.Text_IO.Put_Line ("case=" & Selected_Case'Image);
   Ada.Text_IO.Put_Line ("rounding_tolerance_mm=" & Dimensionless'Image (Params.Extrusion_Rounding_Tolerance / mm));
   Ada.Text_IO.Flush;
   if Write_Segments then
      Ada.Text_IO.Create (Segment_Output, Ada.Text_IO.Out_File, Ada.Command_Line.Argument (3));
      Ada.Text_IO.Put_Line (Segment_Output, "segment,time_s,commanded_distance_mm,midpoint_velocity_ratio");
   end if;
   Started := Ada.Real_Time.Clock;
   Planner.Runner.Setup (Params, Motor_Map);
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Gcode_Path);

   while not Ada.Text_IO.End_Of_File (File) loop
      Ada.Text_IO.Get_Line (File, Line, Last);
      declare
         Command : constant String := Line (1 .. Last);
      begin
         if Command'Length >= 3 and then Command (1 .. 3) = "M83" then
            Relative_E := True;
         elsif Command'Length >= 2
           and then Command (1 .. 2) = "G1"
           and then (Command'Length = 2 or else Command (3) = ' ')
         then
            declare
               Previous_Pos : constant Position := Current_Pos;
            begin
               G1_Count := G1_Count + 1;
               if Has_Value (Command, 'X') then
                  Current_Pos (X_Axis) := Dimensionless (Value_Of (Command, 'X')) * mm;
               end if;
               if Has_Value (Command, 'Y') then
                  Current_Pos (Y_Axis) := Dimensionless (Value_Of (Command, 'Y')) * mm;
               end if;
               if Has_Value (Command, 'Z') then
                  Current_Pos (Z_Axis) := Dimensionless (Value_Of (Command, 'Z')) * mm;
               end if;
               if Has_Value (Command, 'E') then
                  if Relative_E then
                     Current_Pos (E_Axis) :=
                       Current_Pos (E_Axis) + Dimensionless (Value_Of (Command, 'E')) * mm;
                  else
                     Current_Pos (E_Axis) := Dimensionless (Value_Of (Command, 'E')) * mm;
                  end if;
               end if;
               if Has_Value (Command, 'F') then
                  Feedrate := Dimensionless (Value_Of (Command, 'F') / 60.0) * mm / s;
               end if;
               if Selected_Case = XYZ_Only then
                  Current_Pos (E_Axis) := 0.0 * mm;
               end if;
               if Current_Pos /= Previous_Pos then
                  Moving_Count := Moving_Count + 1;
                  Planner.Enqueue_Move (Current_Pos, Feedrate);
               end if;
            end;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (File);
   if Selected_Case = Rounding_Stress then
      if G1_Count /= 50_000 or else Moving_Count /= 50_000 then
         raise Program_Error with "rounding stress fixture must contain exactly 50000 moving segments";
      end if;
   elsif G1_Count /= 48_649 or else (Selected_Case /= XYZ_Only and then Moving_Count /= 47_924) then
      raise Program_Error with "uploaded Benchy parse count changed";
   end if;

   --  The True marker identifies the block which actually consumes this terminal flush. Overflow blocks retain the
   --  False default, so draining through the marker proves that the complete input—not merely the first available
   --  block—was planned.
   Planner.Enqueue_Flush (True);
   loop
      Planner.Dequeue (Block, Timed_Out);
      if Timed_Out then
         Timeout_Count := Timeout_Count + 1;
         if Timeout_Count >= 3_600 then
            raise Program_Error with "planner did not reach the terminal Benchy flush within one hour";
         end if;
      else
         Block_Count := Block_Count + 1;
         Final_Pos := Planner.Next_Block_Pos (Block'Access);
         if Block.N_Corners >= Planner.Finishing_Corners_Index'First then
            for Corner in Planner.Finishing_Corners_Index'First .. Block.N_Corners loop
               Total := Total + Planner.Segment_Time (Block'Access, Corner);
               Segment_Count := Segment_Count + 1;
               if Selected_Case = Rounding_Stress then
                  declare
                     At_End : constant Time := Planner.Segment_Time (Block'Access, Corner);
                     P : constant Position := Planner.Segment_Pos_At_Time (Block'Access, Corner, At_End);
                  begin
                     if abs (P (E_Axis) - 0.05 * P (X_Axis)) > 1.0E-8 * mm then
                        raise Program_Error with "normalized E execution drifted in the 50000-segment run";
                     end if;
                     if Corner < Block.N_Corners
                       and then Planner.Segment_Vel_Ratio_At_Time (Block'Access, Corner, At_End) <= 0.0
                     then
                        raise Program_Error with "a density mismatch introduced a stop in the normalized run";
                     end if;
                  end;
               end if;
               if Write_Segments then
                  Ada.Text_IO.Put_Line
                    (Segment_Output,
                     Segment_Count'Image & "," & Dimensionless'Image (Planner.Segment_Time (Block'Access, Corner) / s)
                     & "," & Dimensionless'Image (Planner.Segment_Corner_Distance (Block, Corner) / mm)
                     & "," & Dimensionless'Image (Planner.Segment_Vel_Ratio_At_Time
                       (Block'Access, Corner, Planner.Segment_Time (Block'Access, Corner) / 2.0)));
               end if;
            end loop;
         end if;
         exit when Planner.Flush_Resetting_Data (Block'Access);
      end if;
   end loop;

   if Final_Pos /= Current_Pos then
      raise Program_Error with "terminal planner position differs from the parsed Benchy position";
   end if;
   if Selected_Case = Rounding_Stress and then (Block_Count /= 1 or else Segment_Count /= 50_000) then
      raise Program_Error with "rounding stress fixture was not handled as one complete 50000-segment block";
   end if;
   Ada.Text_IO.Put_Line
     ("planning_wall_seconds="
      & Long_Float'Image (Long_Float (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Started))));
   Ada.Text_IO.Put_Line ("g1_commands=" & G1_Count'Image);
   Ada.Text_IO.Put_Line ("position_changing_moves=" & Moving_Count'Image);
   Ada.Text_IO.Put_Line ("planned_blocks=" & Block_Count'Image);
   Ada.Text_IO.Put_Line ("planned_segments=" & Segment_Count'Image);
   Ada.Text_IO.Put_Line ("total_planned_seconds=" & Long_Float'Image (Long_Float (Total / s)));
   if Write_Segments then
      Ada.Text_IO.Close (Segment_Output);
   end if;
   Planner.Reset;
   abort Planner.Runner;
end Benchy_Planner_Time;
