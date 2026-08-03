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

with Ada.Strings.Fixed;
with Ada.Text_IO;
with Prunt;                        use Prunt;
with Prunt.Input_Shapers;
with Prunt.Motion_Planner;         use Prunt.Motion_Planner;
with Prunt.Motion_Planner.Planner;

procedure Benchy_Planner_Time is
   Gcode_Path : constant String := "../prunt_simulator/uploads/benchy.gcode";

   type Motor_Name is (X_Motor, Y_Motor, Z_Motor, E_Motor);
   type Motor_Position_Map is array (Axis_Name, Motor_Name) of Length;
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
        Interpolation_Time                 => 0.000_05 * s,
        Max_Corners                        => 50_000);

   use type Planner.Corners_Index;

   Params : constant Kinematic_Parameters :=
     (Lower_Pos_Limit          => [E_Axis => -1.0E100 * mm, others => 0.0 * mm],
      Upper_Pos_Limit          =>
        [X_Axis | Y_Axis | Z_Axis => 300.0 * mm, E_Axis => 1.0E100 * mm],
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
      Axial_Shapers            => [others => (Kind => Prunt.Input_Shapers.No_Shaper)]);

   Motor_Map : constant Motor_Position_Map :=
     [X_Axis => [X_Motor => 1.0 * mm, others => Length'Last],
      Y_Axis => [Y_Motor => 1.0 * mm, others => Length'Last],
      Z_Axis => [Z_Motor => 1.0 * mm, others => Length'Last],
      E_Axis => [E_Motor => 1.0 * mm, others => Length'Last]];

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
begin
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
               if Current_Pos /= Previous_Pos then
                  Moving_Count := Moving_Count + 1;
                  Planner.Enqueue_Move (Current_Pos, Feedrate);
               end if;
            end;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (File);
   if G1_Count /= 48_649 or else Moving_Count /= 47_924 then
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
            end loop;
         end if;
         exit when Planner.Flush_Resetting_Data (Block'Access);
      end if;
   end loop;

   if Final_Pos /= Current_Pos then
      raise Program_Error with "terminal planner position differs from the parsed Benchy position";
   end if;
   Ada.Text_IO.Put_Line ("g1_commands=" & G1_Count'Image);
   Ada.Text_IO.Put_Line ("position_changing_moves=" & Moving_Count'Image);
   Ada.Text_IO.Put_Line ("planned_blocks=" & Block_Count'Image);
   Ada.Text_IO.Put_Line ("planned_segments=" & Segment_Count'Image);
   Ada.Text_IO.Put_Line ("total_planned_seconds=" & Long_Float'Image (Long_Float (Total / s)));
   Planner.Reset;
   abort Planner.Runner;
end Benchy_Planner_Time;
