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

pragma Check_Float_Overflow;

with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Conversion;
with Prunt.Cleanup_Exceptions;
with Prunt.Motion_Planner.Planner.Corner_Blender;
with Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter;
with Prunt.Motion_Planner.Planner.Extrusion_Density_Normalizer;
with Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;
with Prunt.Motion_Planner.Planner.Kinematic_Limiter;
with Prunt.Motion_Planner.Planner.Preprocessor;

package body Prunt.Motion_Planner.Planner is

   pragma Unsuppress (Overflow_Check);
   pragma Unsuppress (Range_Check);
   pragma Unsuppress (Division_Check);

   pragma Extensions_Allowed (On);

   use type Prunt.Input_Shapers.Shaper_Kind;
   use type Prunt.Input_Shapers.Shaper_Parameters;

   package My_Preprocessor is new Preprocessor;
   package My_Corner_Blender is new Corner_Blender;
   package My_Extrusion_Density_Normalizer is new Extrusion_Density_Normalizer;
   package My_Kinematic_Limiter is new Kinematic_Limiter;
   package My_Early_Kinematic_Limiter is new Early_Kinematic_Limiter;
   package My_Feedrate_Profile_Generator is new Feedrate_Profile_Generator;
   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   protected body Block_Ring is
      procedure Setup is
      begin
         Running := True;
      end Setup;

      procedure Request_Reset is
      begin
         Running := False;
      end Request_Reset;

      procedure Report_Termination is
      begin
         Running := False;
         Runner_Terminated := True;
      end Report_Termination;

      procedure Begin_Discard (Pending : out Block_Set) is
      begin
         for Index in Block_Index loop
            Pending (Index) := States (Index) in Planning | Ready;
            if Pending (Index) then
               States (Index) := Cleaning;
            end if;
         end loop;
         Ready_Count := 0;
         Next_Read := Block_Index'First;
         Next_Write := Block_Index'First;
      end Begin_Discard;

      entry Acquire (Index : out Block_Index; Reset_Called : out Boolean) when not Running or else Free_Count > 0 is
      begin
         Reset_Called := not Running;
         Index := Next_Slot;
         if Reset_Called then
            return;
         end if;
         --  A borrower may retain a slot across reset, or release several borrowed slots out of order.
         while States (Index) /= Free loop
            Index := @ + 1;
         end loop;
         States (Index) := Planning;
         Free_Count := @ - 1;
         Next_Slot := Index + 1;
      end Acquire;

      procedure Publish (Index : Block_Index) is
      begin
         pragma Assert (States (Index) = Planning);
         States (Index) := Ready;
         Ready_Slots (Next_Write) := Index;
         Next_Write := @ + 1;
         Ready_Count := @ + 1;
      end Publish;

      entry Dequeue (Block : in out Block_Handle) when Runner_Terminated or else (Running and then Ready_Count > 0) is
         Index : constant Block_Index := Ready_Slots (Next_Read);
      begin
         if Runner_Terminated then
            raise Tasking_Error with "Motion planner runner has terminated.";
         end if;
         pragma Assert (States (Index) = Ready);
         States (Index) := Borrowed;
         Block.Data := Blocks (Index).Block'Access;
         Block.Index := Index;
         Next_Read := @ + 1;
         Ready_Count := @ - 1;
      end Dequeue;

      procedure Begin_Release (Index : Block_Index) is
      begin
         pragma Assert (States (Index) = Borrowed);
         States (Index) := Cleaning;
      end Begin_Release;

      procedure Finish_Cleanup (Index : Block_Index) is
      begin
         pragma Assert (States (Index) = Cleaning);
         States (Index) := Free;
         Free_Count := @ + 1;
      end Finish_Cleanup;
   end Block_Ring;

   procedure Cleanup_Block (Index : Block_Index; Error : in out Cleanup_Exceptions.Holder);
   --  Finish returning a reserved slot even if a finalizer fails, preserving the first cleanup exception.

   procedure Cleanup_Block (Index : Block_Index; Error : in out Cleanup_Exceptions.Holder) is
   begin
      begin
         Clear_Block_Extra_Data (Index);
      exception
         when E : others =>
            Error.Save (E);
      end;
      Block_Ring.Finish_Cleanup (Index);
   end Cleanup_Block;

   procedure Discard_Blocks;

   procedure Discard_Blocks is
      Pending : Block_Set;
      Error   : Cleanup_Exceptions.Holder;
   begin
      pragma Abort_Defer;
      Block_Ring.Begin_Discard (Pending);
      for Index in Block_Index loop
         if Pending (Index) then
            Cleanup_Block (Index, Error);
         end if;
      end loop;
      Error.Reraise;
   end Discard_Blocks;

   overriding
   procedure Finalize (This : in out Runner_Finalizer) is
      pragma Unreferenced (This);
   begin
      --  Wake pending dequeues on exceptions or abort without replacing the controller's termination handler.
      Block_Ring.Report_Termination;
      Discard_Blocks;
   end Finalize;

   function Scaled_Curvature_Norm (Coefficients : Projection_Coefficients) return Curvature is
      Scale      : Curvature := 0.0 / mm;
      Square_Sum : Dimensionless := 0.0;
   begin
      for Coefficient of Coefficients loop
         Scale := Curvature'Max (Scale, abs Coefficient);
      end loop;

      if Scale = 0.0 / mm then
         return 0.0 / mm;
      end if;

      for Coefficient of Coefficients loop
         Square_Sum := Square_Sum + (Coefficient / Scale) ** 2;
      end loop;

      declare
         Norm_Factor : constant Dimensionless := Dimensionless_Math.Sqrt (Square_Sum);
      begin
         return (if Scale > Curvature'Last / Norm_Factor then Curvature'Last else Scale * Norm_Factor);
      end;
   end Scaled_Curvature_Norm;

   function Scaled_Curvature_Hypot (Left, Right : Curvature) return Curvature is
   begin
      return Scaled_Curvature_Norm ([X_Axis => Left, Y_Axis => Right, others => 0.0 / mm]);
   end Scaled_Curvature_Hypot;

   procedure Clear_Block_Extra_Data (Index : Block_Index) is
      Error : Cleanup_Exceptions.Holder;
   begin
      begin
         Blocks (Index).Extra_Data.Clear;
      exception
         when E : others =>
            Error.Save (E);
      end;
      begin
         Blocks (Index).Block.Flush_Resetting_Data := Flush_Resetting_Data_Type_Default;
      exception
         when E : others =>
            Error.Save (E);
      end;
      Error.Reraise;
   end Clear_Block_Extra_Data;

   procedure Reset is
   begin
      Block_Ring.Request_Reset;
      My_Preprocessor.Reset;
      Runner.Reset_Do_Not_Call_From_Other_Packages;
   end Reset;

   procedure Enqueue_Move
     (Pos : Position; Feedrate : Velocity; Dwell_After : Time := 0.0 * s; Ignore_Bounds : Boolean := False) is
   begin
      My_Preprocessor.Enqueue
        ((Kind => Move_Kind, Dwell_After => Dwell_After, Pos => Pos, Feedrate => Feedrate), Ignore_Bounds);
   end Enqueue_Move;

   procedure Enqueue_Helix
     (Pos           : Position;
      Center        : Position;
      Clockwise     : Boolean;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Ignore_Bounds : Boolean := False) is
   begin
      My_Preprocessor.Enqueue
        ((Kind        => Helix_Move_Kind,
          Dwell_After => Dwell_After,
          Pos         => Pos,
          Center      => Center,
          Clockwise   => Clockwise,
          Feedrate    => Feedrate),
         Ignore_Bounds);
   end Enqueue_Helix;

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

   procedure Enqueue_Flush (Data : Flush_Resetting_Data_Type) is
   begin
      My_Preprocessor.Enqueue ((Kind => Flush_Kind, Flush_Resetting_Data => Data));
   end Enqueue_Flush;

   function Enqueue_Homing_Flush (Data : Flush_Resetting_Data_Type) return Position_Offset is
      Tail_Offset  : Position_Offset;
      Reset_Called : Boolean;
   begin
      My_Preprocessor.Enqueue ((Kind => Homing_Flush_Kind, Flush_Resetting_Data => Data));
      My_Preprocessor.Wait_For_Homing_Tail_Offset (Tail_Offset, Reset_Called);
      if Reset_Called then
         raise Homing_Move_Cancelled_Error with "Planner reset while waiting for a homing move to be planned.";
      end if;
      return Tail_Offset;
   end Enqueue_Homing_Flush;

   procedure Resolve_Homing_Position (Pos : Position) is
   begin
      My_Preprocessor.Resolve_Homing_Position (Pos);
   end Resolve_Homing_Position;

   procedure Enqueue_Flush_And_Reset_Position
     (Data : Flush_Resetting_Data_Type; Pos : Position; Ignore_Bounds : Boolean := False) is
   begin
      My_Preprocessor.Enqueue
        ((Kind => Flush_And_Reset_Position_Kind, Flush_Resetting_Data => Data, Reset_Pos => Pos), Ignore_Bounds);
   end Enqueue_Flush_And_Reset_Position;

   procedure Enqueue_Flush_And_Change_Kinematic_Parameters
     (Data : Flush_Resetting_Data_Type; New_Params : Kinematic_Parameters) is
   begin
      My_Preprocessor.Enqueue
        ((Kind => Flush_And_Change_Parameters_Kind, Flush_Resetting_Data => Data, New_Params => New_Params));
   end Enqueue_Flush_And_Change_Kinematic_Parameters;

   procedure Dequeue (Block : in out Block_Handle; Timed_Out : out Boolean) is
   begin
      if not Is_Empty (Block) then
         --  Keep this check when assertions are disabled: overwriting the handle would leak its borrowed slot.
         raise Program_Error with "Cannot dequeue into a populated block handle.";
      end if;

      select
         Block_Ring.Dequeue (Block);
         Timed_Out := False;
      or
         delay 1.0;
         Timed_Out := True;
      end select;
   end Dequeue;

   procedure Release (Block : in out Block_Handle) is
      Error : Cleanup_Exceptions.Holder;
   begin
      pragma Abort_Defer;
      if Is_Empty (Block) then
         return;
      end if;
      Block_Ring.Begin_Release (Block.Index);
      --  Finalizers may block or call back into the planner. Keep the slot reserved until they finish, but do not hold
      --  the ring lock. Deferring abort prevents a release from abandoning a slot in Cleaning state.
      Cleanup_Block (Block.Index, Error);
      Block.Data := null;
      Error.Reraise;
   end Release;

   overriding
   procedure Finalize (Block : in out Block_Handle) is
   begin
      Release (Block);
   end Finalize;

   function Is_Empty (Block : Block_Handle) return Boolean
   is (Block.Data = null);

   function Corner_Count (Block : Block_Handle) return Corners_Index
   is (Block.Data.N_Corners);

   function Segment_Time (Block : Block_Handle; Finishing_Corner : Corners_Index) return Time
   is (Segment_Time (Block.Data, Finishing_Corner));

   function Segment_Corner_Distance (Block : Block_Handle; Finishing_Corner : Corners_Index) return Length
   is (Segment_Corner_Distance (Block.Data.all, Finishing_Corner));

   function Segment_Pos_At_Time
     (Block : Block_Handle; Finishing_Corner : Finishing_Corners_Index; Time_Into_Segment : Time) return Position
   is (Segment_Pos_At_Time (Block.Data, Finishing_Corner, Time_Into_Segment));

   function Segment_Vel_Ratio_At_Time
     (Block : Block_Handle; Finishing_Corner : Finishing_Corners_Index; Time_Into_Segment : Time) return Dimensionless
   is (Segment_Vel_Ratio_At_Time (Block.Data, Finishing_Corner, Time_Into_Segment));

   function Next_Block_Pos (Block : Block_Handle) return Position
   is (Next_Block_Pos (Block.Data));

   function Block_Start_Pos (Block : Block_Handle) return Position
   is (Block_Start_Pos (Block.Data));

   function Flush_Resetting_Data (Block : Block_Handle) return Flush_Resetting_Data_Type
   is (Flush_Resetting_Data (Block.Data));

   function Is_Homing_Move (Block : Block_Handle) return Boolean
   is (Is_Homing_Move (Block.Data));

   function Loop_Move_Minimum_Time (Block : Block_Handle) return Time
   is (Loop_Move_Minimum_Time (Block.Data));

   function Block_Kind (Block : Block_Handle) return Execution_Block_Kind
   is (Block_Kind (Block.Data.all));

   function Corner_ID (Block : Block_Handle; Corner : Corners_Index) return Planner_Corner_ID
   is (Corner_ID (Block.Data, Corner));

   function Has_Associated_Overflow_Block (Block : Block_Handle) return Boolean
   is (Has_Associated_Overflow_Block (Block.Data));

   function Block_Kinematic_Parameters (Block : Block_Handle) return Kinematic_Parameters
   is (Block_Kinematic_Parameters (Block.Data));

   procedure Corner_Extra_Data
     (Block   : Block_Handle;
      Corner  : Corners_Index;
      Process : not null access procedure (Data : in out Corner_Extra_Data_Type)) is
   begin
      Corner_Extra_Data (Block.Data, Blocks (Block.Index).Extra_Data, Corner, Process);
   end Corner_Extra_Data;

   function Interval_Contains_Phase (Low, High, Base, Period : Dimensionless) return Boolean is
   begin
      return Dimensionless'Ceiling ((Low - Base) / Period) <= Dimensionless'Floor ((High - Base) / Period);
   end Interval_Contains_Phase;

   function Maximum_Absolute_Sine (Start_Phase, End_Phase : Dimensionless) return Dimensionless is
      Phase_Low  : constant Dimensionless := Dimensionless'Min (Start_Phase, End_Phase);
      Phase_High : constant Dimensionless := Dimensionless'Max (Start_Phase, End_Phase);
      Pad        : constant Dimensionless := 1.0E-12 * (1.0 + Dimensionless'Max (abs Phase_Low, abs Phase_High));
      Low        : constant Dimensionless := Phase_Low - Pad;
      High       : constant Dimensionless := Phase_High + Pad;
      Pi         : constant Dimensionless := Ada.Numerics.Pi;
   begin
      if Interval_Contains_Phase (Low, High, 0.5 * Pi, Pi) then
         return 1.0;
      else
         return
           Dimensionless'Min
             (1.0, Dimensionless'Max (abs Dimensionless_Math.Sin (Low), abs Dimensionless_Math.Sin (High)) + Pad);
      end if;
   end Maximum_Absolute_Sine;

   function Maximum_Absolute_Cosine (Start_Phase, End_Phase : Dimensionless) return Dimensionless is
      Phase_Low  : constant Dimensionless := Dimensionless'Min (Start_Phase, End_Phase);
      Phase_High : constant Dimensionless := Dimensionless'Max (Start_Phase, End_Phase);
      Pad        : constant Dimensionless := 1.0E-12 * (1.0 + Dimensionless'Max (abs Phase_Low, abs Phase_High));
      Low        : constant Dimensionless := Phase_Low - Pad;
      High       : constant Dimensionless := Phase_High + Pad;
      Pi         : constant Dimensionless := Ada.Numerics.Pi;
   begin
      if Interval_Contains_Phase (Low, High, 0.0, Pi) then
         return 1.0;
      else
         return
           Dimensionless'Min
             (1.0, Dimensionless'Max (abs Dimensionless_Math.Cos (Low), abs Dimensionless_Math.Cos (High)) + Pad);
      end if;
   end Maximum_Absolute_Cosine;

   function Maximum_Absolute_Offset_Sine
     (Start_Phase, End_Phase : Dimensionless; Amplitude, Offset : Curvature; Phase_Shift : Dimensionless)
      return Curvature
   is
      Shifted_Start : constant Dimensionless := Start_Phase + Phase_Shift;
      Shifted_End   : constant Dimensionless := End_Phase + Phase_Shift;
      Phase_Low     : constant Dimensionless := Dimensionless'Min (Shifted_Start, Shifted_End);
      Phase_High    : constant Dimensionless := Dimensionless'Max (Shifted_Start, Shifted_End);
      Pad           : constant Dimensionless := 1.0E-12 * (1.0 + Dimensionless'Max (abs Phase_Low, abs Phase_High));
      Low           : constant Dimensionless := Phase_Low - Pad;
      High          : constant Dimensionless := Phase_High + Pad;
      Pi            : constant Dimensionless := Ada.Numerics.Pi;
      Two_Pi        : constant Dimensionless := 2.0 * Pi;
      Operand_Scale : constant Curvature := abs Offset + abs Amplitude;
      Result        : Curvature :=
        Curvature'Max
          (abs (Offset + Amplitude * Dimensionless_Math.Sin (Low)),
           abs (Offset + Amplitude * Dimensionless_Math.Sin (High)));
   begin
      if Amplitude = 0.0 / mm then
         return abs Offset;
      end if;

      if Interval_Contains_Phase (Low, High, 0.5 * Pi, Two_Pi) then
         Result := Curvature'Max (Result, abs (Offset + Amplitude));
      end if;

      if Interval_Contains_Phase (Low, High, 1.5 * Pi, Two_Pi) then
         Result := Curvature'Max (Result, abs (Offset - Amplitude));
      end if;

      --  Padding only Result is not sufficient when Offset and the sinusoid nearly cancel: the rounded evaluation can
      --  be zero while the exact residual scales with the much larger operands.
      return Result * (1.0 + 1.0E-12) + 256.0 * Dimensionless'Model_Epsilon * Operand_Scale + 1.0E-15 / mm;
   end Maximum_Absolute_Offset_Sine;

   function Make_Line_Primitive return Path_Primitive is
   begin
      return (Kind => Line_Primitive_Kind);
   end Make_Line_Primitive;

   function Make_Helix_Primitive (Start_Point, End_Point, Center : Position; Clockwise : Boolean) return Path_Primitive
   is
      Result  : constant Path_Primitive := (Kind => Helix_Primitive_Kind, Center => Center, Clockwise => Clockwise);
      Derived : constant Derived_Path_Primitive := Derive_Path_Primitive (Result, Start_Point, End_Point);
   begin
      return (if Derived.Kind = Helix_Primitive_Kind then Result else Make_Line_Primitive);
   end Make_Helix_Primitive;

   function Derive_Path_Primitive
     (Primitive : Path_Primitive; Start_Point, End_Point : Position) return Derived_Path_Primitive
   is
      Offset : constant Position_Offset := End_Point - Start_Point;
   begin
      if Primitive.Kind = Line_Primitive_Kind then
         declare
            Path_Length : constant Length := abs Offset;
         begin
            return
              (Kind      => Line_Primitive_Kind,
               Length    => Path_Length,
               Direction => (if Path_Length > 0.0 * mm then Offset / Path_Length else [others => 0.0]));
         end;
      end if;

      declare
         Start_Offset  : constant Position_Offset := Start_Point - Primitive.Center;
         End_Offset    : constant Position_Offset := End_Point - Primitive.Center;
         Start_Radius  : constant Length := (Start_Offset (X_Axis) ** 2 + Start_Offset (Y_Axis) ** 2) ** (1 / 2);
         End_Radius    : constant Length := (End_Offset (X_Axis) ** 2 + End_Offset (Y_Axis) ** 2) ** (1 / 2);
         Theta_Start   : constant Dimensionless :=
           Dimensionless_Math.Arctan (Start_Offset (Y_Axis) / mm, Start_Offset (X_Axis) / mm);
         Two_Pi        : constant Dimensionless := 2.0 * Ada.Numerics.Pi;
         Offset_Scale  : constant Length :=
           Length'Max
             (abs Start_Offset (X_Axis),
              Length'Max (abs Start_Offset (Y_Axis), Length'Max (abs End_Offset (X_Axis), abs End_Offset (Y_Axis))));
         Coincident_XY : constant Boolean :=
           Start_Offset (X_Axis) = End_Offset (X_Axis) and then Start_Offset (Y_Axis) = End_Offset (Y_Axis);
         Theta_Delta   : Dimensionless := 0.0;
         Axial_Delta   : Position_Offset := Offset;
      begin
         if Start_Radius <= 0.0 * mm or else abs (Start_Radius - End_Radius) > 1.0E-6 * mm then
            return
              (Kind      => Line_Primitive_Kind,
               Length    => abs Offset,
               Direction => (if abs Offset > 0.0 * mm then Offset / abs Offset else [others => 0.0]));
         end if;

         if Coincident_XY then
            Theta_Delta := (if Primitive.Clockwise then -Two_Pi else Two_Pi);
         else
            declare
               Start_X : constant Dimensionless := Start_Offset (X_Axis) / Offset_Scale;
               Start_Y : constant Dimensionless := Start_Offset (Y_Axis) / Offset_Scale;
               End_X   : constant Dimensionless := End_Offset (X_Axis) / Offset_Scale;
               End_Y   : constant Dimensionless := End_Offset (Y_Axis) / Offset_Scale;
               Cross   : constant Dimensionless := Start_X * End_Y - Start_Y * End_X;
               Dot     : constant Dimensionless := Start_X * End_X + Start_Y * End_Y;
            begin
               --  Deriving the relative phase directly avoids subtracting two nearly equal absolute angles.
               Theta_Delta := Dimensionless_Math.Arctan (Cross, Dot);
            end;

            if Theta_Delta = 0.0 then
               --  Distinct radial points do not define a circular sweep. Falling back to their chord is safer than
               --  interpreting an angle that rounded to zero as a complete revolution.
               return
                 (Kind      => Line_Primitive_Kind,
                  Length    => abs Offset,
                  Direction => (if abs Offset > 0.0 * mm then Offset / abs Offset else [others => 0.0]));
            elsif Primitive.Clockwise and then Theta_Delta > 0.0 then
               Theta_Delta := Theta_Delta - Two_Pi;
            elsif not Primitive.Clockwise and then Theta_Delta < 0.0 then
               Theta_Delta := Theta_Delta + Two_Pi;
            end if;
         end if;

         Axial_Delta (X_Axis) := 0.0 * mm;
         Axial_Delta (Y_Axis) := 0.0 * mm;

         declare
            Axial_Per_Phase   : constant Position_Offset := Axial_Delta * (1.0 / Theta_Delta);
            Length_Per_Radian : constant Length := (Start_Radius ** 2 + (abs Axial_Per_Phase) ** 2) ** (1 / 2);
         begin
            return
              (Kind              => Helix_Primitive_Kind,
               Length            => Length_Per_Radian * abs Theta_Delta,
               Radius            => Start_Radius,
               Theta_Start       => Theta_Start,
               Theta_Delta       => Theta_Delta,
               Axial_Per_Phase   => Axial_Per_Phase,
               Length_Per_Radian => Length_Per_Radian);
         end;
      end;
   end Derive_Path_Primitive;

   function Primitive_Phase_At_Distance (Primitive : Derived_Path_Primitive; Distance : Length) return Dimensionless is
      Clamped : constant Length := Length'Max (0.0 * mm, Length'Min (Distance, Primitive.Length));
      Sign    : constant Dimensionless := (if Primitive.Theta_Delta < 0.0 then -1.0 else 1.0);
   begin
      return Primitive.Theta_Start + Sign * Clamped / Primitive.Length_Per_Radian;
   end Primitive_Phase_At_Distance;

   function Spatial_Primitive
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Derived_Path_Primitive
   is
      Start_Point : constant Position := [Block.Corners (Corner - 1) with delta E_Axis => 0.0 * mm];
      End_Point   : constant Position := [Block.Corners (Corner) with delta E_Axis => 0.0 * mm];
      Result      : constant Derived_Path_Primitive :=
        Derive_Path_Primitive (Block.Primitives (Corner), Start_Point, End_Point);
   begin
      --  E-only moves still need a nonzero scalar distance and the usual time-domain limits.
      if Result.Length = 0.0 * mm then
         return Derive_Path_Primitive (Block.Primitives (Corner), Block.Corners (Corner - 1), Block.Corners (Corner));
      end if;
      return Result;
   end Spatial_Primitive;

   function Extrusion_Junction_Ceiling
     (Block : not null access constant Execution_Block; Corner : Corners_Index; Limit : Velocity) return Velocity
   is
      Change  : constant Dimensionless :=
        abs (Block.Extrusion_Densities (Corner + 1) - Block.Extrusion_Densities (Corner));
      Maximum : constant Velocity := Block.Params.Extrusion_Velocity_Change_Max;
   begin
      --  The two density-scaled profiles meet directly, so E's velocity jumps by Change * Limit.
      return (if Change * Limit > Maximum then 0.999 * Maximum / Change else Limit);
   end Extrusion_Junction_Ceiling;

   function Extrusion_Reference_At_Distance
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index; Distance : Length)
      return Length
   is
      Total : constant Length := Segment_Total_Distance (Block, Corner);
      D     : constant Length := Length'Max (0.0 * mm, Length'Min (Distance, Total));
   begin
      --  E follows the shortened path at the commanded density.
      return Block.Extrusion_Reference_Positions (Corner - 1) + Block.Extrusion_Densities (Corner) * D;
   end Extrusion_Reference_At_Distance;

   function Extrusion_Bounds
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index)
      return Unit_Speed_Axial_Derivative_Bounds
   is (Velocity => [E_Axis => abs Block.Extrusion_Densities (Corner), others => 0.0], others => <>);

   function Primitive_Length
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length
   is
      Primitive : constant Derived_Path_Primitive := Spatial_Primitive (Block, Finishing_Corner);
   begin
      return Primitive.Length;
   end Primitive_Length;

   function Primitive_Point_At_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index; Distance : Length)
      return Position
   is (Derived_Primitive_Point_At_Distance
         (Block, Finishing_Corner, Spatial_Primitive (Block, Finishing_Corner), Distance));

   function Derived_Primitive_Point_At_Distance
     (Block            : not null access constant Execution_Block;
      Finishing_Corner : Finishing_Corners_Index;
      Primitive        : Derived_Path_Primitive;
      Distance         : Length) return Position
   is
      Descriptor : constant Path_Primitive := Block.Primitives (Finishing_Corner);
      D          : constant Length := Length'Max (0.0 * mm, Length'Min (Distance, Primitive.Length));
   begin
      case Primitive.Kind is
         when Line_Primitive_Kind  =>
            return Block.Corners (Finishing_Corner - 1) + Primitive.Direction * D;

         when Helix_Primitive_Kind =>
            declare
               Phi      : constant Dimensionless := Primitive_Phase_At_Distance (Primitive, D);
               Fraction : constant Dimensionless := D / Primitive.Length;
               Result   : Position := Block.Corners (Finishing_Corner - 1);
            begin
               --  Subtracting absolute phases loses precision for small sweeps. Z is linear in path distance.
               Result (Z_Axis) :=
                 Block.Corners (Finishing_Corner - 1) (Z_Axis)
                 + (Block.Corners (Finishing_Corner) (Z_Axis) - Block.Corners (Finishing_Corner - 1) (Z_Axis))
                   * Fraction;
               Result (X_Axis) := Descriptor.Center (X_Axis) + Dimensionless_Math.Cos (Phi) * Primitive.Radius;
               Result (Y_Axis) := Descriptor.Center (Y_Axis) + Dimensionless_Math.Sin (Phi) * Primitive.Radius;
               if D = Primitive.Length then
                  Result :=
                    [Block.Corners (Finishing_Corner) with delta
                       E_Axis => Block.Corners (Finishing_Corner - 1) (E_Axis)];
               end if;
               return Result;
            end;
      end case;
   end Derived_Primitive_Point_At_Distance;

   function Primitive_Direction_At_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index; Distance : Length)
      return Position_Scale
   is
      Primitive : constant Derived_Path_Primitive := Spatial_Primitive (Block, Finishing_Corner);
   begin
      case Primitive.Kind is
         when Line_Primitive_Kind  =>
            return Primitive.Direction;

         when Helix_Primitive_Kind =>
            declare
               Phi  : constant Dimensionless := Primitive_Phase_At_Distance (Primitive, Distance);
               Sign : constant Dimensionless := (if Primitive.Theta_Delta < 0.0 then -1.0 else 1.0);
            begin
               return
                 (Position_Scale'
                    (X_Axis => -Dimensionless_Math.Sin (Phi),
                     Y_Axis => Dimensionless_Math.Cos (Phi),
                     Z_Axis => 0.0,
                     E_Axis => 0.0)
                  * (Sign * Primitive.Radius / Primitive.Length_Per_Radian))
                 + (Primitive.Axial_Per_Phase / Primitive.Length_Per_Radian) * Sign;
            end;
      end case;
   end Primitive_Direction_At_Distance;

   function Primitive_Derivative_Jets_At_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index; Distance : Length)
      return Endpoint_Tangent_Jet
   is
      Primitive : constant Derived_Path_Primitive := Spatial_Primitive (Block, Finishing_Corner);
      Result    : Endpoint_Tangent_Jet :=
        (Tangent              => [others => 0.0],
         Tangent_Derivative_1 => [others => 0.0 / mm],
         Tangent_Derivative_2 => [others => 0.0 / mm ** 2],
         Tangent_Derivative_3 => [others => 0.0 / mm ** 3]);
   begin
      case Primitive.Kind is
         when Line_Primitive_Kind  =>
            Result.Tangent := Primitive.Direction;

         when Helix_Primitive_Kind =>
            declare
               Phi       : constant Dimensionless := Primitive_Phase_At_Distance (Primitive, Distance);
               C         : constant Dimensionless := Dimensionless_Math.Cos (Phi);
               S_Phase   : constant Dimensionless := Dimensionless_Math.Sin (Phi);
               Sign      : constant Dimensionless := (if Primitive.Theta_Delta < 0.0 then -1.0 else 1.0);
               R_Over_L  : constant Dimensionless := Primitive.Radius / Primitive.Length_Per_Radian;
               R_Over_L2 : constant Curvature := R_Over_L / Primitive.Length_Per_Radian;
               R_Over_L3 : constant Curvature_To_2 := R_Over_L2 / Primitive.Length_Per_Radian;
               R_Over_L4 : constant Curvature_To_3 := R_Over_L3 / Primitive.Length_Per_Radian;
            begin
               Result.Tangent := Primitive_Direction_At_Distance (Block, Finishing_Corner, Distance);

               Result.Tangent_Derivative_1 (X_Axis) := -C * R_Over_L2;
               Result.Tangent_Derivative_1 (Y_Axis) := -S_Phase * R_Over_L2;

               Result.Tangent_Derivative_2 (X_Axis) := Sign * S_Phase * R_Over_L3;
               Result.Tangent_Derivative_2 (Y_Axis) := -Sign * C * R_Over_L3;

               Result.Tangent_Derivative_3 (X_Axis) := C * R_Over_L4;
               Result.Tangent_Derivative_3 (Y_Axis) := S_Phase * R_Over_L4;
            end;
      end case;

      return Result;
   end Primitive_Derivative_Jets_At_Distance;

   function Primitive_Derivative_Bounds
     (Block            : not null access constant Execution_Block;
      Finishing_Corner : Finishing_Corners_Index;
      Start_Distance   : Length;
      Distance         : Length) return Unit_Speed_Axial_Derivative_Bounds
   is
      Primitive : constant Derived_Path_Primitive := Spatial_Primitive (Block, Finishing_Corner);
      Result    : Unit_Speed_Axial_Derivative_Bounds :=
        (Velocity     => [others => 0.0],
         Acceleration => [others => 0.0 / mm],
         Jerk         => [others => 0.0 / mm ** 2],
         Snap         => [others => 0.0 / mm ** 3],
         Crackle      => [others => 0.0 / mm ** 4]);
   begin
      case Primitive.Kind is
         when Line_Primitive_Kind  =>
            for A in Axis_Name loop
               Result.Velocity (A) := abs Primitive.Direction (A);
            end loop;

         when Helix_Primitive_Kind =>
            declare
               R           : constant Length := Primitive.Radius;
               L           : constant Length := Primitive.Length_Per_Radian;
               XY_V        : constant Dimensionless := R / L;
               XY_A        : constant Curvature := XY_V / L;
               XY_J        : constant Curvature_To_2 := XY_A / L;
               XY_S        : constant Curvature_To_3 := XY_J / L;
               XY_C        : constant Curvature_To_4 := XY_S / L;
               Start_Phase : constant Dimensionless := Primitive_Phase_At_Distance (Primitive, Start_Distance);
               End_Phase   : constant Dimensionless :=
                 Primitive_Phase_At_Distance (Primitive, Start_Distance + Distance);
               Sin_Bound   : constant Dimensionless := Maximum_Absolute_Sine (Start_Phase, End_Phase);
               Cos_Bound   : constant Dimensionless := Maximum_Absolute_Cosine (Start_Phase, End_Phase);
            begin
               Result.Velocity (X_Axis) := XY_V * Sin_Bound;
               Result.Velocity (Y_Axis) := XY_V * Cos_Bound;
               Result.Acceleration (X_Axis) := XY_A * Cos_Bound;
               Result.Acceleration (Y_Axis) := XY_A * Sin_Bound;
               Result.Jerk (X_Axis) := XY_J * Sin_Bound;
               Result.Jerk (Y_Axis) := XY_J * Cos_Bound;
               Result.Snap (X_Axis) := XY_S * Cos_Bound;
               Result.Snap (Y_Axis) := XY_S * Sin_Bound;
               Result.Crackle (X_Axis) := XY_C * Sin_Bound;
               Result.Crackle (Y_Axis) := XY_C * Cos_Bound;

               for A in Axis_Name loop
                  if A /= X_Axis and then A /= Y_Axis then
                     Result.Velocity (A) := abs (Primitive.Axial_Per_Phase (A) / L);
                  end if;
               end loop;
            end;
      end case;

      return Result;
   end Primitive_Derivative_Bounds;

   function Primitive_Motor_Delta_Ceiling
     (Block            : not null access constant Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Finishing_Corner : Finishing_Corners_Index;
      Start_Distance   : Length;
      Distance         : Length;
      Max_Vel          : Velocity) return Velocity
   is
      Primitive : constant Derived_Path_Primitive := Spatial_Primitive (Block, Finishing_Corner);
      Result    : Velocity := Max_Vel;
   begin
      if Primitive.Length <= 0.0 * mm then
         return 0.0 * mm / s;
      end if;

      for M in Motor_Name loop
         declare
            Coefficients : constant Projection_Coefficients := Motor_Projection_Coefficients (Motor_Map, M);
            Projection   : Curvature := 0.0 / mm;
         begin
            case Primitive.Kind is
               when Line_Primitive_Kind  =>
                  for A in Axis_Name loop
                     Projection := Projection + Coefficients (A) * Primitive.Direction (A);
                  end loop;
                  Projection := abs Projection;

               when Helix_Primitive_Kind =>
                  declare
                     Sign            : constant Dimensionless := (if Primitive.Theta_Delta < 0.0 then -1.0 else 1.0);
                     Sin_Coefficient : constant Curvature :=
                       -Sign * Coefficients (X_Axis) * Primitive.Radius / Primitive.Length_Per_Radian;
                     Cos_Coefficient : constant Curvature :=
                       Sign * Coefficients (Y_Axis) * Primitive.Radius / Primitive.Length_Per_Radian;
                     XY_Amp          : constant Curvature := Scaled_Curvature_Hypot (Sin_Coefficient, Cos_Coefficient);
                     Axial_Offset    : Curvature := 0.0 / mm;
                     Phase_Shift     : Dimensionless := 0.0;
                  begin
                     for A in Axis_Name loop
                        if A /= X_Axis and then A /= Y_Axis then
                           Axial_Offset :=
                             Axial_Offset
                             + Coefficients (A) * Primitive.Axial_Per_Phase (A) / Primitive.Length_Per_Radian * Sign;
                        end if;
                     end loop;

                     if XY_Amp > 0.0 / mm then
                        Phase_Shift := Dimensionless_Math.Arctan (Cos_Coefficient * mm, Sin_Coefficient * mm);
                     end if;

                     Projection :=
                       Maximum_Absolute_Offset_Sine
                         (Primitive_Phase_At_Distance (Primitive, Start_Distance),
                          Primitive_Phase_At_Distance (Primitive, Start_Distance + Distance),
                          XY_Amp,
                          Axial_Offset,
                          Phase_Shift);
                  end;
            end case;

            Projection := Shaper_Aware_Projection_Bound (Block.Params, Coefficients, Projection);

            if Projection > 0.0 / mm then
               Result :=
                 Velocity'Min
                   (Result,
                    Motor_Delta_Numerical_Safety_Factor
                    * Maximum_Deltas_Per_Command (M)
                    / (Interpolation_Time * Projection));
            end if;
         end;
      end loop;

      return Velocity'Max (0.0 * mm / s, Result);
   end Primitive_Motor_Delta_Ceiling;

   function Segment_Start_Transition_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length is
   begin
      return
        Arc_Length (Block.Corner_Transitions (Finishing_Corner - 1))
        - Split_Distance (Block.Corner_Transitions (Finishing_Corner - 1));
   end Segment_Start_Transition_Distance;

   function Segment_End_Transition_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length is
   begin
      return Split_Distance (Block.Corner_Transitions (Finishing_Corner));
   end Segment_End_Transition_Distance;

   function Segment_Straight_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length is
   begin
      return Block.Primitive_Distances (Finishing_Corner);
   end Segment_Straight_Distance;

   function Segment_Total_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index) return Length is
   begin
      return
        Segment_Start_Transition_Distance (Block, Finishing_Corner)
        + Block.Primitive_Distances (Finishing_Corner)
        + Segment_End_Transition_Distance (Block, Finishing_Corner);
   end Segment_Total_Distance;

   function Segment_Profile_Window_Candidates
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index)
      return Profile_Window_Candidates
   is
      Start_Transition : constant Length := Segment_Start_Transition_Distance (Block, Finishing_Corner);
      End_Transition   : constant Length := Segment_End_Transition_Distance (Block, Finishing_Corner);
      Total            : constant Length := Segment_Total_Distance (Block, Finishing_Corner);
      Result           : Profile_Window_Candidates;
      Index            : Profile_Window_Candidate_Index := 1;
      procedure Add (First, Tail : Length);

      procedure Add (First, Tail : Length) is
         Last : constant Length := Total - Tail;
         Dist : Length := Last - First;
      begin
         --  Rounding must not make a profile overlap the transition that the selected cut excludes.
         if Dist > 0.0 * mm and then First + Dist > Last then
            Dist := Length'Adjacent (Dist, 0.0 * mm);
         end if;
         Result (Index) := (Start_Distance => First, Distance => Dist);
         if Index < Profile_Window_Candidate_Index'Last then
            Index := Index + 1;
         end if;
      end Add;
   begin
      Add (0.0 * mm, 0.0 * mm);
      Add (Start_Transition, 0.0 * mm);
      Add (0.0 * mm, End_Transition);
      Add (Start_Transition, End_Transition);
      return Result;
   end Segment_Profile_Window_Candidates;

   function Evaluate_Profile_Window
     (Block            : not null access constant Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Finishing_Corner : Finishing_Corners_Index;
      Window           : Profile_Window;
      Max_Vel          : Velocity) return Profile_Window_Evaluation
   is
      Bounds : constant Unit_Speed_Axial_Derivative_Bounds :=
        Window_Axial_Derivative_Bounds (Block, Finishing_Corner, Window);
      Mixed  : constant Mixed_Derivative_Limit_Result :=
        Mixed_Derivative_Limits (Block.Params, (Velocity => Bounds.Velocity, others => <>), Max_Vel);
      Result : Profile_Window_Evaluation;
   begin
      Result :=
        (Valid   => Mixed.Valid,
         Window  => Window,
         Limits  => Mixed.Limits,
         Max_Vel =>
           Motor_Delta_Ceiling_For_Window
             (Block,
              Motor_Map,
              Finishing_Corner,
              Window,
              Constant_Speed_Axial_Ceiling (Block.Params, Bounds, Max_Vel)),
         Bounds  => Bounds,
         Curved  =>
           (for some Axis in Axis_Name =>
              Bounds.Acceleration (Axis) /= 0.0 / mm
              or else Bounds.Jerk (Axis) /= 0.0 / mm ** 2
              or else Bounds.Snap (Axis) /= 0.0 / mm ** 3
              or else Bounds.Crackle (Axis) /= 0.0 / mm ** 4));
      if Window.Start_Distance < 0.0 * mm
        or else Window.Distance < 0.0 * mm
        or else Window.Start_Distance + Window.Distance > Segment_Total_Distance (Block, Finishing_Corner)
      then
         Result.Valid := False;
      end if;
      return Result;
   end Evaluate_Profile_Window;

   function Window_Axial_Derivative_Bounds
     (Block            : not null access constant Execution_Block;
      Finishing_Corner : Finishing_Corners_Index;
      Window           : Profile_Window) return Unit_Speed_Axial_Derivative_Bounds
   is
      Result : Unit_Speed_Axial_Derivative_Bounds :=
        (Velocity     => [others => 0.0],
         Acceleration => [others => 0.0 / mm],
         Jerk         => [others => 0.0 / mm ** 2],
         Snap         => [others => 0.0 / mm ** 3],
         Crackle      => [others => 0.0 / mm ** 4]);

      Start_Transition : constant Length := Segment_Start_Transition_Distance (Block, Finishing_Corner);
      Middle           : constant Length := Block.Primitive_Distances (Finishing_Corner);
      End_Start        : constant Length := Start_Transition + Middle;
      Window_Start     : constant Length := Window.Start_Distance;
      Window_End       : constant Length := Window.Start_Distance + Window.Distance;

      procedure Merge (Bounds : Unit_Speed_Axial_Derivative_Bounds);
      procedure Merge_End_Transition;
      procedure Merge_Primitive;
      procedure Merge_Start_Transition;

      procedure Merge (Bounds : Unit_Speed_Axial_Derivative_Bounds) is
      begin
         for A in Axis_Name loop
            Result.Velocity (A) := Dimensionless'Max (Result.Velocity (A), Bounds.Velocity (A));
            Result.Acceleration (A) := Curvature'Max (Result.Acceleration (A), Bounds.Acceleration (A));
            Result.Jerk (A) := Curvature_To_2'Max (Result.Jerk (A), Bounds.Jerk (A));
            Result.Snap (A) := Curvature_To_3'Max (Result.Snap (A), Bounds.Snap (A));
            Result.Crackle (A) := Curvature_To_4'Max (Result.Crackle (A), Bounds.Crackle (A));
         end loop;
      end Merge;

      procedure Merge_Start_Transition is
         Overlap_Start : constant Length := Length'Max (0.0 * mm, Window_Start);
         Overlap_End   : constant Length := Length'Min (Start_Transition, Window_End);
         Transition    : constant Corner_Transition_Evaluator := Block.Corner_Transitions (Finishing_Corner - 1);
         Split         : constant Length := Split_Distance (Transition);
      begin
         Merge (Derivative_Bounds (Transition, Split + Overlap_Start, Split + Overlap_End));
      end Merge_Start_Transition;

      procedure Merge_Primitive is
         Overlap_Start   : constant Length := Length'Max (Start_Transition, Window_Start);
         Overlap_End     : constant Length := Length'Min (End_Start, Window_End);
         Primitive_Start : constant Length :=
           Block.Primitive_Start_Distances (Finishing_Corner) + Overlap_Start - Start_Transition;
      begin
         Merge (Primitive_Derivative_Bounds (Block, Finishing_Corner, Primitive_Start, Overlap_End - Overlap_Start));
      end Merge_Primitive;

      procedure Merge_End_Transition is
         Overlap_Start : constant Length := Length'Max (End_Start, Window_Start);
         Overlap_End   : constant Length := Length'Min (Segment_Total_Distance (Block, Finishing_Corner), Window_End);
         Transition    : constant Corner_Transition_Evaluator := Block.Corner_Transitions (Finishing_Corner);
      begin
         Merge (Derivative_Bounds (Transition, Overlap_Start - End_Start, Overlap_End - End_Start));
      end Merge_End_Transition;
   begin
      if Window.Distance <= 0.0 * mm then
         return Extrusion_Bounds (Block, Finishing_Corner);
      end if;

      if Window_Start < Start_Transition and then Window_End > 0.0 * mm then
         Merge_Start_Transition;
      end if;

      if Middle > 0.0 * mm and then Window_Start < End_Start and then Window_End > Start_Transition then
         Merge_Primitive;
      end if;

      if Window_Start < Segment_Total_Distance (Block, Finishing_Corner) and then Window_End > End_Start then
         Merge_End_Transition;
      end if;

      declare
         E_Bounds : constant Unit_Speed_Axial_Derivative_Bounds := Extrusion_Bounds (Block, Finishing_Corner);
      begin
         Result.Velocity (E_Axis) := E_Bounds.Velocity (E_Axis);
         Result.Acceleration (E_Axis) := E_Bounds.Acceleration (E_Axis);
         Result.Jerk (E_Axis) := E_Bounds.Jerk (E_Axis);
         Result.Snap (E_Axis) := E_Bounds.Snap (E_Axis);
         Result.Crackle (E_Axis) := E_Bounds.Crackle (E_Axis);
      end;
      return Result;
   end Window_Axial_Derivative_Bounds;

   function Spatial_Motor_Map
     (Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map)
      return Prunt.Motion_Planner.Planner.Motor_Position_Map
   is
      Result : Prunt.Motion_Planner.Planner.Motor_Position_Map := Motor_Map;
   begin
      for M in Motor_Name loop
         Result (E_Axis, M) := 0.0 / mm;
      end loop;
      return Result;
   end Spatial_Motor_Map;

   function Motor_Projection_Coefficients
     (Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map; Motor : Motor_Name) return Projection_Coefficients
   is
      Result : Projection_Coefficients := [others => 0.0 / mm];
   begin
      for A in Axis_Name loop
         Result (A) := Motor_Map (A, Motor);
      end loop;

      return Result;
   end Motor_Projection_Coefficients;

   function Shaper_Aware_Projection_Bound
     (Params : Kinematic_Parameters; Coefficients : Projection_Coefficients; Raw_Bound : Curvature) return Curvature
   is
      type Processed_Axes is array (Axis_Name) of Boolean;
      Processed       : Processed_Axes := [others => False];
      Combined_Gain   : Curvature := 0.0 / mm;
      Response_Groups : Natural := 0;
   begin
      --  Each basic shaper is a normalized nonnegative impulse response. Axes with identical parameters therefore
      --  share a response and can be projected first; their induced gain is the coefficient-vector norm. Responses
      --  from different groups may sample unrelated unit tangents, so their group norms must be added. This is the
      --  analytical worst-case combined-impulse bound and costs only a few axis comparisons per planner query.
      for Representative in Axis_Name loop
         if not Processed (Representative)
           and then Coefficients (Representative) /= 0.0 / mm
           and then Params.Axial_Shapers (Representative).Kind /= Prunt.Input_Shapers.Pressure_Advance
         then
            declare
               Group_Coefficients : Projection_Coefficients := [others => 0.0 / mm];
               Group_Norm         : Curvature;
            begin
               Response_Groups := Response_Groups + 1;
               for Axis in Axis_Name loop
                  if Params.Axial_Shapers (Axis).Kind /= Prunt.Input_Shapers.Pressure_Advance
                    and then Params.Axial_Shapers (Axis) = Params.Axial_Shapers (Representative)
                  then
                     Processed (Axis) := True;
                     Group_Coefficients (Axis) := Coefficients (Axis);
                  end if;
               end loop;

               Group_Norm := Scaled_Curvature_Norm (Group_Coefficients);
               if Combined_Gain > Curvature'Last - Group_Norm then
                  Combined_Gain := Curvature'Last;
               else
                  Combined_Gain := Combined_Gain + Group_Norm;
               end if;
            end;
         end if;
      end loop;

      --  A common response commutes with the motor projection, so the tighter path-specific Raw_Bound remains valid.
      --  Pressure advance is excluded here because only motor-separable axes may use it and its excess is handled by
      --  the step-generator catch-up path.
      return (if Response_Groups > 1 then Curvature'Max (Raw_Bound, Combined_Gain) else Raw_Bound);
   end Shaper_Aware_Projection_Bound;

   function Motor_Delta_Ceiling_For_Projection
     (Params : Kinematic_Parameters; Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map; Max_Vel : Velocity)
      return Velocity
   is
      Result : Velocity := Max_Vel;
   begin
      for M in Motor_Name loop
         declare
            Coefficients : constant Projection_Coefficients := Motor_Projection_Coefficients (Motor_Map, M);
            Projection   : constant Curvature :=
              Shaper_Aware_Projection_Bound (Params, Coefficients, Scaled_Curvature_Norm (Coefficients));
         begin
            if Projection > 0.0 / mm then
               Result :=
                 Velocity'Min
                   (Result,
                    Motor_Delta_Numerical_Safety_Factor
                    * Maximum_Deltas_Per_Command (M)
                    / (Interpolation_Time * Projection));
            end if;
         end;
      end loop;
      return Velocity'Max (0.0 * mm / s, Result);
   end Motor_Delta_Ceiling_For_Projection;

   function Motor_Delta_Ceiling_For_Window
     (Block            : not null access constant Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Finishing_Corner : Finishing_Corners_Index;
      Window           : Profile_Window;
      Max_Vel          : Velocity) return Velocity
   is
      Geometry_Map     : constant Prunt.Motion_Planner.Planner.Motor_Position_Map := Spatial_Motor_Map (Motor_Map);
      Result           : Velocity := Max_Vel;
      Start_Transition : constant Length := Segment_Start_Transition_Distance (Block, Finishing_Corner);
      Middle           : constant Length := Block.Primitive_Distances (Finishing_Corner);
      End_Start        : constant Length := Start_Transition + Middle;
      Window_Start     : constant Length := Window.Start_Distance;
      Window_End       : constant Length := Window.Start_Distance + Window.Distance;
   begin
      declare
         E_Bound : constant Dimensionless := abs Block.Extrusion_Densities (Finishing_Corner);
      begin
         for M in Motor_Name loop
            declare
               Coefficients : constant Projection_Coefficients :=
                 [Motor_Projection_Coefficients (Motor_Map, M) with delta E_Axis => 0.0 / mm];
               Projection   : constant Curvature :=
                 Shaper_Aware_Projection_Bound (Block.Params, Coefficients, Scaled_Curvature_Norm (Coefficients))
                 + abs Motor_Map (E_Axis, M) * E_Bound;
            begin
               if Projection > 0.0 / mm and then E_Bound > 0.0 and then Motor_Map (E_Axis, M) /= 0.0 / mm then
                  Result :=
                    Velocity'Min
                      (Result,
                       Motor_Delta_Numerical_Safety_Factor
                       * Maximum_Deltas_Per_Command (M)
                       / (Interpolation_Time * Projection));
               end if;
            end;
         end loop;
      end;

      if Window.Distance <= 0.0 * mm then
         return Result;
      end if;

      if Window_Start < Start_Transition and then Window_End > 0.0 * mm then
         Result := Motor_Delta_Ceiling_For_Projection (Block.Params, Geometry_Map, Result);
      end if;

      if Middle > 0.0 * mm and then Window_Start < End_Start and then Window_End > Start_Transition then
         declare
            Overlap_Start   : constant Length := Length'Max (Start_Transition, Window_Start);
            Overlap_End     : constant Length := Length'Min (End_Start, Window_End);
            Primitive_Start : constant Length :=
              Block.Primitive_Start_Distances (Finishing_Corner) + Overlap_Start - Start_Transition;
         begin
            Result :=
              Velocity'Min
                (Result,
                 Primitive_Motor_Delta_Ceiling
                   (Block, Geometry_Map, Finishing_Corner, Primitive_Start, Overlap_End - Overlap_Start, Max_Vel));
         end;
      end if;

      if Window_Start < Segment_Total_Distance (Block, Finishing_Corner) and then Window_End > End_Start then
         Result := Motor_Delta_Ceiling_For_Projection (Block.Params, Geometry_Map, Result);
      end if;

      return Result;
   end Motor_Delta_Ceiling_For_Window;

   function Endpoint_Delta_V_Distance
     (Start_Vel : Velocity; End_Vel : Velocity; Limits : Scalar_Derivative_Limits) return Length
   is
      Profile : Feedrate_Profile_Times;
   begin
      Profile :=
        Optimal_Profile_For_Delta_V
          (End_Vel - Start_Vel, Limits.Acceleration_Max, Limits.Jerk_Max, Limits.Snap_Max, Limits.Crackle_Max);
      return
        Fast_Distance_At_Max_Time
          (Profile, (if Start_Vel < End_Vel then Limits.Crackle_Max else -Limits.Crackle_Max), Start_Vel);
   exception
      when Constraint_Error =>
         return Length'Last;
   end Endpoint_Delta_V_Distance;

   function Reachable_Velocity
     (Start_Vel : Velocity; Max_Vel : Velocity; Distance : Length; Limits : Scalar_Derivative_Limits) return Velocity
   is
      Lower : Velocity := Velocity'Min (Start_Vel, Max_Vel);
      Upper : Velocity := Max_Vel;
      Mid   : Velocity;
   begin
      if Distance <= 0.0 * mm or else Max_Vel <= Start_Vel then
         return Lower;
      end if;

      if Endpoint_Delta_V_Distance (Start_Vel, Upper, Limits) <= Distance then
         return Upper;
      end if;

      loop
         --  This form avoids overflowing Lower + Upper. Continue until the midpoint rounds to an endpoint so the
         --  search remains effective even when Max_Vel and the reachable velocity differ by hundreds of decades.
         Mid := Lower + 0.5 * (Upper - Lower);
         exit when Mid = Lower or else Mid = Upper;

         if Endpoint_Delta_V_Distance (Start_Vel, Mid, Limits) <= Distance then
            Lower := Mid;
         else
            Upper := Mid;
         end if;
      end loop;

      return Velocity'Max (Start_Vel, 0.999 * Lower);
   end Reachable_Velocity;

   function Profile_At_Peak
     (Params : Kinematic_Parameters; Eval : Profile_Window_Evaluation; Start_Vel, End_Vel, Peak : Velocity)
      return Window_Profile_Result
   is
      Cache : Mixed_Derivative_Cache;
   begin
      return Profile_At_Peak (Params, Eval, Start_Vel, End_Vel, Peak, Cache);
   end Profile_At_Peak;

   function Profile_At_Peak
     (Params                   : Kinematic_Parameters;
      Eval                     : Profile_Window_Evaluation;
      Start_Vel, End_Vel, Peak : Velocity;
      Cache                    : in out Mixed_Derivative_Cache) return Window_Profile_Result
   is
      Result                         : Window_Profile_Result;
      Peaks                          : Scalar_Derivative_Limits :=
        (0.0 * mm / s ** 2, 0.0 * mm / s ** 3, 0.0 * mm / s ** 4, 0.0 * mm / s ** 5);
      Scale                          : Dimensionless := 1.0;
      Distance                       : Length;
      Accel_Distance, Decel_Distance : Length;
      Actual_Peak                    : Velocity;
      type Ramps is array (1 .. 2) of Feedrate_Profile_Times;
   begin
      if not Eval.Valid or else Peak < Velocity'Max (Start_Vel, End_Vel) or else Peak > Eval.Max_Vel then
         return Result;
      elsif Eval.Window.Distance <= 0.0 * mm then
         Result.Valid := Eval.Window.Distance = 0.0 * mm and then Start_Vel = End_Vel;
         return Result;
      elsif Peak <= 0.0 * mm / s then
         return Result;
      end if;
      Result.Max_Crackle := Eval.Limits.Crackle_Max;
      Result.Profile.Accel :=
        Optimal_Profile_For_Delta_V
          (Peak - Start_Vel,
           Eval.Limits.Acceleration_Max,
           Eval.Limits.Jerk_Max,
           Eval.Limits.Snap_Max,
           Eval.Limits.Crackle_Max);
      --  Equal endpoint velocities request exactly the same ramp, before any curvature time scaling.
      Result.Profile.Decel :=
        (if Start_Vel = End_Vel
         then Result.Profile.Accel
         else
           Optimal_Profile_For_Delta_V
             (Peak - End_Vel,
              Eval.Limits.Acceleration_Max,
              Eval.Limits.Jerk_Max,
              Eval.Limits.Snap_Max,
              Eval.Limits.Crackle_Max));
      if Eval.Curved then
         for Times of Ramps'[Result.Profile.Accel, Result.Profile.Decel] loop
            Peaks.Acceleration_Max :=
              Acceleration'Max
                (Peaks.Acceleration_Max,
                 Result.Max_Crackle * Times (1) * (Times (1) + Times (2)) * (2.0 * Times (1) + Times (2) + Times (3)));
            Peaks.Jerk_Max := Jerk'Max (Peaks.Jerk_Max, Result.Max_Crackle * Times (1) * (Times (1) + Times (2)));
            Peaks.Snap_Max := Snap'Max (Peaks.Snap_Max, Result.Max_Crackle * Times (1));
            if Times (1) > 0.0 * s then
               Peaks.Crackle_Max := Result.Max_Crackle;
            end if;
         end loop;
         Scale := Mixed_Derivative_Time_Scale (Params, Eval.Bounds, Peak, Peaks, Cache);
         if Scale <= 0.0 then
            return Result;
         end if;
         for I in Feedrate_Profile_Times_Index loop
            Result.Profile.Accel (I) := @ / Scale;
            Result.Profile.Decel (I) := @ / Scale;
         end loop;
         --  Multiply successively so Scale**4 cannot underflow before multiplication by a large crackle.
         for I in 1 .. 4 loop
            Result.Max_Crackle := @ * Scale;
         end loop;
      end if;
      if (Peak > Start_Vel or else Peak > End_Vel)
        and then Result.Max_Crackle < Dimensionless'Model_Small * mm / s ** 5
      then
         --  Underflow or subnormal rounding would invalidate both the preserved velocity change and its
         --  derivative certificate. Do not turn an unrepresentable ramp into an apparently feasible coast.
         return Result;
      end if;
      Actual_Peak := Fast_Velocity_At_Max_Time (Result.Profile.Accel, Result.Max_Crackle, Start_Vel);
      Accel_Distance := Fast_Distance_At_Max_Time (Result.Profile.Accel, Result.Max_Crackle, Start_Vel);
      Decel_Distance := Fast_Distance_At_Max_Time (Result.Profile.Decel, -Result.Max_Crackle, Actual_Peak);
      Distance := Accel_Distance + Decel_Distance;
      if Distance <= Eval.Window.Distance and then Actual_Peak > 0.0 * mm / s then
         Result.Profile.Coast := (Eval.Window.Distance - Distance) / Actual_Peak;
         Result.Valid :=
           Profile_Is_Valid
             (Result.Profile,
              Eval.Window.Distance,
              Start_Vel,
              End_Vel,
              Result.Max_Crackle,
              Actual_Peak,
              Accel_Distance,
              Decel_Distance);
      end if;
      return Result;
   exception
      when Constraint_Error =>
         return (others => <>);
   end Profile_At_Peak;

   function Reachable_Window_Velocity
     (Params : Kinematic_Parameters; Eval : Profile_Window_Evaluation; Start_Vel : Velocity) return Velocity
   is
      Lower : Velocity := Velocity'Min (Start_Vel, Eval.Max_Vel);
      Upper : Velocity;
      Cache : Mixed_Derivative_Cache;
   begin
      if not Eval.Curved or else Eval.Window.Distance <= 0.0 * mm or else Eval.Max_Vel <= Start_Vel then
         return Reachable_Velocity (Start_Vel, Eval.Max_Vel, Eval.Window.Distance, Eval.Limits);
      end if;
      --  The tangent-only solve is a cheap upper bound and avoids searching an unnecessarily large speed range.
      Upper := Reachable_Velocity (Start_Vel, Eval.Max_Vel, Eval.Window.Distance, Eval.Limits);
      if Profile_At_Peak (Params, Eval, Start_Vel, Upper, Upper, Cache).Valid then
         return Upper;
      end if;
      loop
         declare
            Mid : constant Velocity := Lower + 0.5 * (Upper - Lower);
         begin
            exit when Mid = Lower or else Mid = Upper or else Upper - Lower <= 1.0E-5 * Lower;
            if Profile_At_Peak (Params, Eval, Start_Vel, Mid, Mid, Cache).Valid then
               Lower := Mid;
            else
               Upper := Mid;
            end if;
         end;
      end loop;
      return Velocity'Max (Start_Vel, 0.999 * Lower);
   end Reachable_Window_Velocity;

   function Identical (Left, Right : Velocity) return Boolean is
      type Velocity_Bits is mod 2 ** Velocity'Size;
      function To_Bits is new Ada.Unchecked_Conversion (Velocity, Velocity_Bits);
   begin
      return To_Bits (Left) = To_Bits (Right);
   end Identical;

   function Generate_Window_Profile
     (Params : Kinematic_Parameters; Eval : Profile_Window_Evaluation; Start_Vel, End_Vel : Velocity)
      return Window_Profile_Result
   is
      Best      : Window_Profile_Result;
      Lower     : Velocity := Velocity'Max (Start_Vel, End_Vel);
      Upper     : Velocity := Eval.Max_Vel;
      Best_Time : Time := 1.0E100 * s;
      Cache     : Mixed_Derivative_Cache;

      Last_Peak       : Velocity := 0.0 * mm / s;
      Last_Duration   : Time := 1.0E100 * s;
      Last_Considered : Boolean := False;
      --  A collapsed feasible interval makes the search repeat one peak. Its certificate is deterministic and
      --  cannot improve Best a second time, so only its duration is needed.

      function Consider (Peak : Velocity) return Time;

      function Consider (Peak : Velocity) return Time is
      begin
         if Last_Considered and then Identical (Peak, Last_Peak) then
            return Last_Duration;
         end if;

         declare
            Candidate : constant Window_Profile_Result :=
              Profile_At_Peak (Params, Eval, Start_Vel, End_Vel, Peak, Cache);
            Duration  : constant Time := (if Candidate.Valid then Total_Time (Candidate.Profile) else 1.0E100 * s);
         begin
            if Candidate.Valid and then (not Best.Valid or else Duration < Best_Time) then
               Best := Candidate;
               Best_Time := Duration;
            end if;
            Last_Peak := Peak;
            Last_Duration := Duration;
            Last_Considered := True;
            return Duration;
         end;
      end Consider;
   begin
      if not Eval.Valid or else Upper < Lower then
         return Best;
      elsif not Eval.Curved then
         Best.Profile :=
           Optimal_Full_Profile
             (Start_Vel,
              Upper,
              End_Vel,
              Eval.Window.Distance,
              Eval.Limits.Acceleration_Max,
              Eval.Limits.Jerk_Max,
              Eval.Limits.Snap_Max,
              Eval.Limits.Crackle_Max);
         Best.Max_Crackle := Eval.Limits.Crackle_Max;
         Best.Valid := True;
         return Best;
      elsif Eval.Window.Distance <= 0.0 * mm then
         return Profile_At_Peak (Params, Eval, Start_Vel, End_Vel, Lower);
      end if;

      --  At the minimum peak there is only one velocity transition, exactly the one certified by lookahead.
      if Lower > 0.0 * mm / s and then Consider (Lower) = 1.0E100 * s then
         return Best;
      end if;
      if Consider (Upper) = 1.0E100 * s then
         --  Locate the feasible peak-speed interval. Every accepted candidate remains available even when
         --  a later trial is infeasible. The relative stop also works for very small representable speeds.
         loop
            declare
               Mid : constant Velocity := Lower + 0.5 * (Upper - Lower);
            begin
               exit when Mid = Lower or else Mid = Upper or else Upper - Lower <= 1.0E-5 * Lower;
               if Consider (Mid) < 1.0E100 * s then
                  Lower := Mid;
               else
                  Upper := Mid;
               end if;
            end;
         end loop;
         Upper := Lower;
      end if;
      Lower := Velocity'Max (Start_Vel, End_Vel);
      --  A feasible maximum peak need not minimize time: a slightly lower peak can free substantial ramp
      --  budget. Search the complete feasible interval; never impose a fixed reduction on coasting speed.
      declare
         Ratio      : constant Dimensionless := 0.618033988749895;
         Left       : Velocity := Upper - Ratio * (Upper - Lower);
         Right      : Velocity := Lower + Ratio * (Upper - Lower);
         Left_Time  : Time := Consider (Left);
         Right_Time : Time := Consider (Right);
      begin
         for Iteration in 1 .. 12 loop
            if Left_Time < Right_Time then
               Upper := Right;
               Right := Left;
               Right_Time := Left_Time;
               Left := Upper - Ratio * (Upper - Lower);
               Left_Time := Consider (Left);
            else
               Lower := Left;
               Left := Right;
               Left_Time := Right_Time;
               Right := Lower + Ratio * (Upper - Lower);
               Right_Time := Consider (Right);
            end if;
         end loop;
      end;
      return Best;
   exception
      when Constraint_Error =>
         return Best;
   end Generate_Window_Profile;

   function Selected_Profile_Window
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index)
      return Profile_Window is
   begin
      return Block.Profile_Windows (Finishing_Corner);
   end Selected_Profile_Window;

   procedure Plan_Fixed_Geometry
     (Block     : aliased in out Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace : not null access Planning_Workspace)
   is
      Blended_Retries : constant Natural := 8;
      Stopped_Retries : constant Natural := 16;
      Homing_Feedrate : constant Velocity :=
        (if Block.Is_Homing_Move then Block.Limited_Segment_Feedrates (2) else 0.0 * mm / s);
      Result          : Profile_Planning_Result;
      Stopped         : Boolean := False;
      Attempts        : Block_Planning_Attempts renames Workspace.Planning_Attempts (Block.Primitives'Range);
   begin
      Attempts := [others => 0];
      loop
         My_Kinematic_Limiter.Run (Block, Motor_Map, Workspace);
         My_Feedrate_Profile_Generator.Run (Block, Motor_Map, Result, Limit_Scale => (if Stopped then 0.5 else 1.0));
         exit when Result.Valid;

         if not Stopped and then Attempts (Result.Failed_Segment) = Blended_Retries then
            My_Corner_Blender.Run (Block, Motor_Map, Workspace, Force_Stops => True);
            My_Early_Kinematic_Limiter.Run (Block, Motor_Map, Normalize_Feedrates => False);
            if Block.Is_Homing_Move then
               --  Geometry recovery must retain the homing loop's requested feedrate reduction.
               Block.Limited_Segment_Feedrates (2) :=
                 Velocity'Min (Block.Limited_Segment_Feedrates (2), Homing_Feedrate);
            end if;
            Stopped := True;
            Attempts := [others => 0];
         elsif Stopped and then Attempts (Result.Failed_Segment) = Stopped_Retries then
            raise Constraint_Error
              with
                "No certifiable stopped profile for segment"
                & Result.Failed_Segment'Image
                & ". Check for zero limits or unrepresentable motion.";
         else
            --  Both adjoining junction ceilings take the minimum of their segments' feedrates, so halving the failed
            --  segment's feedrate also slows its endpoints.
            Block.Limited_Segment_Feedrates (Result.Failed_Segment) :=
              Block.Limited_Segment_Feedrates (Result.Failed_Segment) * 0.5;
            Attempts (Result.Failed_Segment) := Attempts (Result.Failed_Segment) + 1;
         end if;
      end loop;
   end Plan_Fixed_Geometry;

   procedure Plan_Kinematics
     (Block     : aliased in out Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace : not null access Planning_Workspace)
   is
      type Stop_Outcome is (Kept, Rejected_Early, Rejected_Late);
      --  Rejected_Late: the first replanned segment was certified without spending the window's whole old time.

      E_Shift      : Length := 0.0 * mm;
      Materialized : Corners_Index := 2;
      --  References up to Materialized include E_Shift; later ones are still untranslated.

      procedure Materialize (Through : Corners_Index) is
      begin
         while Materialized < Through loop
            Materialized := @ + 1;
            Block.Extrusion_Reference_Positions (Materialized) := @ + E_Shift;
         end loop;
      end Materialize;

      function Straight_Bound
        (Segment : Finishing_Corners_Index; Start_Vel, End_Vel : Velocity; Distance : Length) return Time
      is
         --  The tangent-only optimum over Distance under the limits of the segment's primitive, the least
         --  restrictive of its windows. Every certified profile also respects them, so none can be faster.
         Eval : constant Profile_Window_Evaluation :=
           Evaluate_Profile_Window
             (Block'Access,
              Motor_Map,
              Segment,
              (Start_Distance => Segment_Start_Transition_Distance (Block'Access, Segment),
               Distance       => Block.Primitive_Distances (Segment)),
              Block.Limited_Segment_Feedrates (Segment));
      begin
         if not Eval.Valid
           or else Block.Primitive_Distances (Segment) <= 0.0 * mm
           or else Eval.Max_Vel < Velocity'Max (Start_Vel, End_Vel)
         then
            return 0.0 * s;
         end if;
         begin
            return
              Total_Time
                (Optimal_Full_Profile
                   (Start_Vel,
                    Eval.Max_Vel,
                    End_Vel,
                    Distance,
                    Eval.Limits.Acceleration_Max,
                    Eval.Limits.Jerk_Max,
                    Eval.Limits.Snap_Max,
                    Eval.Limits.Crackle_Max));
         exception
            when Constraint_Error =>
               --  The speed change does not fit even under these limits.
               return 1.0E100 * s;
         end;
      exception
         when Constraint_Error =>
            return 0.0 * s;
      end Straight_Bound;

      procedure Try_Stops
        (First, Last   : Finishing_Corners_Index;
         Keep          : Boolean;
         Required_Gain : Time;
         Outcome       : out Stop_Outcome;
         Gain          : out Time;
         First_Time    : out Time)
      is
         --  Stop at every corner in First .. Last and replan segments First .. Last + 1 with the outer junction
         --  speeds fixed. Gain is the certified saving; the change is kept only if Keep and it exceeds Required_Gain.
         type Saved_Segment is record
            Distance, Start_Distance, E_Reference : Length;
            Feedrate                              : Velocity;
            Profile                               : Feedrate_Profile;
            Max_Crackle                           : Crackle;
            Window                                : Profile_Window;
            Ends                                  : Profile_End_Pair;
         end record;
         type Saved_Corner is record
            Transition : Corner_Transition_Evaluator;
            Bounds     : Unit_Speed_Axial_Derivative_Bounds;
            Speed      : Velocity;
         end record;
         subtype Window_Segments is Finishing_Corners_Index range First .. Last + 1;
         Saved         : constant array (Window_Segments) of Saved_Segment :=
           [for I in Window_Segments =>
              (Block.Primitive_Distances (I),
               Block.Primitive_Start_Distances (I),
               Block.Extrusion_Reference_Positions (I),
               Block.Limited_Segment_Feedrates (I),
               Block.Feedrate_Profiles (I),
               Block.Profile_Crackles (I),
               Block.Profile_Windows (I),
               Block.Profile_Ends (I))];
         Saved_Corners : constant array (First .. Last) of Saved_Corner :=
           [for J in First .. Last =>
              (Block.Corner_Transitions (J),
               Workspace.Corner_Derivative_Bounds (J),
               Block.Corner_Velocity_Limits (J))];
         Old_Times     : constant array (Window_Segments) of Time :=
           [for I in Window_Segments => Segment_Time (Block'Access, I)];
         Old_Distances : constant Block_Segment_Lengths (Window_Segments) :=
           [for I in Window_Segments => Segment_Total_Distance (Block'Access, I)];
         Old_Time      : Time := 0.0 * s;
         Old_Selected  : Time := 0.0 * s;
         New_Time      : Time := 0.0 * s;
         Delta_E       : Length := 0.0 * mm;
         New_Shift     : Length;
         Result        : Profile_Planning_Result;

         procedure Restore is
         begin
            for J in Saved_Corners'Range loop
               Block.Corner_Transitions (J) := Saved_Corners (J).Transition;
               Workspace.Corner_Derivative_Bounds (J) := Saved_Corners (J).Bounds;
               Block.Corner_Velocity_Limits (J) := Saved_Corners (J).Speed;
            end loop;
            for I in Saved'Range loop
               Block.Primitive_Distances (I) := Saved (I).Distance;
               Block.Primitive_Start_Distances (I) := Saved (I).Start_Distance;
               Block.Extrusion_Reference_Positions (I) := Saved (I).E_Reference;
               Block.Limited_Segment_Feedrates (I) := Saved (I).Feedrate;
               Block.Feedrate_Profiles (I) := Saved (I).Profile;
               Block.Profile_Crackles (I) := Saved (I).Max_Crackle;
               Block.Profile_Windows (I) := Saved (I).Window;
               Block.Profile_Ends (I) := Saved (I).Ends;
            end loop;
         end Restore;
      begin
         Outcome := Rejected_Early;
         Gain := 0.0 * s;
         First_Time := 0.0 * s;
         for I in Window_Segments loop
            Old_Time := @ + Old_Times (I);
         end loop;
         Workspace.Local_Comparison_Corners := @ + 1;
         --  Change only these junctions. The retained outer transitions and their velocities remain identical, so
         --  certified replacement profiles splice into the existing trajectory.
         for Corner in First .. Last loop
            Block.Corner_Transitions (Corner) :=
              To_Evaluator (Stop_At ([Block.Corners (Corner) with delta E_Axis => 0.0 * mm]));
            Workspace.Corner_Derivative_Bounds (Corner) := Derivative_Bounds (Block.Corner_Transitions (Corner));
            Block.Corner_Velocity_Limits (Corner) := 0.0 * mm / s;
            Block.Primitive_Distances (Corner) :=
              Primitive_Length (Block'Access, Corner) - Block.Primitive_Start_Distances (Corner);
            Block.Primitive_Distances (Corner + 1) := @ + Block.Primitive_Start_Distances (Corner + 1);
            Block.Primitive_Start_Distances (Corner + 1) := 0.0 * mm;
         end loop;
         for I in Saved'Range loop
            Delta_E :=
              @ + Block.Extrusion_Densities (I) * (Segment_Total_Distance (Block'Access, I) - Old_Distances (I));
            Block.Extrusion_Reference_Positions (I) := Saved (I).E_Reference + Delta_E;
            if Workspace.Check_Extrusion_Position
              and then
                Block.Extrusion_Reference_Positions (I)
                not in Block.Params.Bounds.Lower_E .. Block.Params.Bounds.Upper_E
            then
               Restore;
               return;
            end if;
            --  Retain the already normalized speed cap. The profile generator certifies the newly restored
            --  primitives, including helix, motor and E derivative limits.
            Block.Limited_Segment_Feedrates (I) :=
              Velocity'Min (Saved (I).Feedrate, Segment_Total_Distance (Block'Access, I) / Interpolation_Time);
         end loop;
         New_Shift := E_Shift + Delta_E;
         if Workspace.Check_Extrusion_Position and then Delta_E /= 0.0 * mm and then Last + 1 < Block.N_Corners then
            --  Later profiles change only by an E translation. Suffix bounds avoid scanning or replanning the
            --  rest of the block for each candidate. Conservative bounds may reject a feasible alternative.
            if Length'Adjacent (Workspace.Suffix_E_Min (Last + 2) + New_Shift, -Length'Last)
              < Block.Params.Bounds.Lower_E
              or else
                Length'Adjacent (Workspace.Suffix_E_Max (Last + 2) + New_Shift, Length'Last)
                > Block.Params.Bounds.Upper_E
            then
               Restore;
               return;
            end if;
         end if;
         for I in Window_Segments loop
            My_Feedrate_Profile_Generator.Select_Feedrate_Profile_Window
              (Block'Access, Motor_Map, I, Result, Limit_Scale => 1.0);
            if not Result.Valid then
               Restore;
               return;
            end if;
            New_Time := @ + Segment_Time (Block'Access, I);
            Old_Selected := @ + Old_Times (I);
            if not (New_Time < (if I = Last + 1 then Old_Selected - Required_Gain else Old_Time)) then
               Restore;
               return;
            end if;
            if I = First then
               Outcome := Rejected_Late;
               First_Time := New_Time;
            end if;
         end loop;
         Outcome := Kept;
         Gain := Old_Selected - New_Time;
         if not Keep then
            Restore;
            return;
         end if;
         --  No further solve is needed: these exact profiles were certified at the unchanged outer speeds.
         E_Shift := New_Shift;
         Workspace.Local_Stops_Selected := @ + Natural (Last - First + 1);
         for Corner in First .. Last loop
            Workspace.Static_Corner_Limits (Corner) := 0.0 * mm / s;
         end loop;
         for I in Saved'Range loop
            Workspace.Planning_Attempts (I) := 0;
            for W in Profile_Window_Candidate_Index loop
               Workspace.Window_Evaluations (I) (W).Valid := False;
            end loop;
         end loop;
      exception
         when Constraint_Error =>
            Outcome := Rejected_Early;
            Restore;
      end Try_Stops;
   begin
      Workspace.Local_Comparison_Corners := 0;
      Workspace.Local_Stops_Selected := 0;
      Plan_Fixed_Geometry (Block, Motor_Map, Workspace);
      if Block.Is_Homing_Move
        or else Block.Params.Cornering.Kind /= Stereographic
        or else
          (for all I in Block.Corner_Transitions'Range =>
             Transition_Kind (Block.Corner_Transitions (I)) /= Stereographic_Transition)
      then
         return;
      end if;
      --  Each reference is monotone within its segment, so its endpoints bound it.
      for I in reverse Block.Primitives'Range loop
         declare
            Low  : constant Length :=
              Length'Min (Block.Extrusion_Reference_Positions (I - 1), Block.Extrusion_Reference_Positions (I));
            High : constant Length :=
              Length'Max (Block.Extrusion_Reference_Positions (I - 1), Block.Extrusion_Reference_Positions (I));
         begin
            Workspace.Suffix_E_Min (I) :=
              (if I = Block.N_Corners then Low else Length'Min (Low, Workspace.Suffix_E_Min (I + 1)));
            Workspace.Suffix_E_Max (I) :=
              (if I = Block.N_Corners then High else Length'Max (High, Workspace.Suffix_E_Max (I + 1)));
         end;
      end loop;
      for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
         --  Apply the cumulative translation to each untouched reference just once, keeping the sweep linear in
         --  the number of corners.
         Materialize (I + 1);
         if Transition_Kind (Block.Corner_Transitions (I)) = Stereographic_Transition then
            declare
               Outcome, Next_Outcome               : Stop_Outcome;
               Gain, Next_Gain, First_Time, Unused : Time;
            begin
               Try_Stops (I, I, True, 0.0 * s, Outcome, Gain, First_Time);
               --  A short segment between two corners may be too short to reach the next blend's speed from rest,
               --  or to do so quickly, while stopping at both corners is faster. The bound skips pairs whose
               --  untouched-geometry optimum already loses.
               if Outcome = Rejected_Late
                 and then I + 1 < Block.N_Corners
                 and then Transition_Kind (Block.Corner_Transitions (I + 1)) = Stereographic_Transition
               then
                  Materialize (I + 2);
                  if First_Time
                    + Straight_Bound (I + 1, 0.0 * mm / s, 0.0 * mm / s, Primitive_Length (Block'Access, I + 1))
                    + Straight_Bound
                        (I + 2,
                         0.0 * mm / s,
                         Block.Corner_Velocity_Limits (I + 2),
                         Block.Primitive_Start_Distances (I + 2)
                         + Block.Primitive_Distances (I + 2)
                         + Segment_End_Transition_Distance (Block'Access, I + 2))
                    < Segment_Time (Block'Access, I)
                      + Segment_Time (Block'Access, I + 1)
                      + Segment_Time (Block'Access, I + 2)
                  then
                     Try_Stops (I, I + 1, False, 0.0 * s, Outcome, Gain, Unused);
                     if Outcome = Kept then
                        --  The sweep tries the next corner alone next. Keep that stop now if it gains more, so a
                        --  pair never displaces a better single stop; otherwise replan the pair to keep it.
                        Try_Stops (I + 1, I + 1, True, Gain, Next_Outcome, Next_Gain, Unused);
                        if Next_Outcome /= Kept then
                           Try_Stops (I, I + 1, True, 0.0 * s, Outcome, Gain, Unused);
                        end if;
                     end if;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Plan_Kinematics;

   function Homing_Unavoidable_Tail_Time (Block : not null access constant Execution_Block) return Time is
      Finishing_Corner : constant Finishing_Corners_Index := Finishing_Corners_Index'First;
      Window           : constant Profile_Window := Selected_Profile_Window (Block, Finishing_Corner);
      Suffix_Distance  : constant Length :=
        Segment_Total_Distance (Block, Finishing_Corner) - Window.Start_Distance - Window.Distance;
      Suffix_Time      : constant Time :=
        Constant_Speed_Time (Suffix_Distance, Block.Corner_Velocity_Limits (Finishing_Corner));
   begin
      return
        Home_Move_Minimum_Coast_Time
        + Total_Time (Block.Feedrate_Profiles (Finishing_Corner).Decel)
        + Suffix_Time
        + Block.Corner_Dwell_Times (Finishing_Corner);
   end Homing_Unavoidable_Tail_Time;

   function Homing_Profile_Violation (Block : not null access constant Execution_Block) return Time is
      Finishing_Corner : constant Finishing_Corners_Index := Finishing_Corners_Index'First;
      Coast_Deficit    : constant Time :=
        Time'Max (0.0 * s, Home_Move_Required_Coast_Time - Block.Feedrate_Profiles (Finishing_Corner).Coast);
      Tail_Excess      : constant Time :=
        Time'Max (0.0 * s, Homing_Unavoidable_Tail_Time (Block) - Home_Move_Maximum_Tail_Time);
   begin
      return Coast_Deficit + Tail_Excess;
   end Homing_Profile_Violation;

   procedure Blend_Blocks
     (Left, Right : aliased in out Execution_Block;
      Motor_Map   : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Blended     : out Boolean)
   is
      --  Only the seam corner, where Left ends and Right begins, gets a new geometry search. The velocity solve covers
      --  at most Blend_Radius segments on each side of it, regardless of block size, and geometry and speeds outside
      --  that window stay fixed. Patch corners 1 .. Seam replace Left corners Left_First .. Left_Seam, and Patch
      --  corners Seam .. Patch.N_Corners replace Right corners 1 .. Right_Last.
      Left_Seam      : constant Corners_Index := Left.N_Corners;
      Left_Segments  : constant Corners_Index'Base := Corners_Index'Min (Blend_Radius, Left_Seam - 1);
      Right_Segments : constant Corners_Index'Base := Corners_Index'Min (Blend_Radius, Right.N_Corners - 1);
      Left_First     : constant Corners_Index := Left_Seam - Left_Segments;
      Right_Last     : constant Corners_Index := Right_Segments + 1;
      Seam           : constant Corners_Index := Left_Segments + 1;

      Geometry     : aliased Execution_Block (3);
      --  Scratch block holding the retained parts of the two primitives that meet at the seam.
      Patch        : aliased Execution_Block (Left_Segments + Right_Segments + 1);
      Work         : aliased Planning_Workspace (Corners_Index'Max (3, Patch.N_Corners));
      Result       : Profile_Planning_Result;
      Suffix_Shift : Length;
      --  E translation of Right's untouched references after the window.

      procedure Blend_Seam;
      --  Blend the seam corner on Geometry. Using only the retained parts of the primitives guarantees the new corner
      --  cannot consume a neighbouring transition's allocation. Extrusion densities are authoritative, so they are
      --  copied rather than renormalized.

      procedure Build_Patch;
      --  Copy the window into Patch and install the new seam corner. Copied profiles and speeds are only starting
      --  values; the limiter and profile generator replace every one.

      function Original_Time return Time;
      --  Time the stopped plans spend on the segments Patch replaces.

      function Patch_Time return Time;

      function E_In_Bounds
        (Block : Execution_Block; First, Last : Finishing_Corners_Index; Shift : Length) return Boolean;
      --  Whether segments First .. Last stay within the E bounds with their references translated by Shift.

      procedure Blend_Seam is
         Left_Start : constant Length := Left.Primitive_Start_Distances (Left_Seam);
         Right_End  : constant Length := Right.Primitive_Start_Distances (2) + Right.Primitive_Distances (2);
      begin
         Initialize_Scratch_Block (Geometry);
         Geometry.Params := Left.Params;
         Geometry.Primitives := [Left.Primitives (Left_Seam), Right.Primitives (2)];
         Geometry.Extrusion_Densities := [Left.Extrusion_Densities (Left_Seam), Right.Extrusion_Densities (2)];
         Geometry.Corners :=
           [Primitive_Point_At_Distance (Left'Access, Left_Seam, Left_Start),
            Left.Corners (Left_Seam),
            Primitive_Point_At_Distance (Right'Access, 2, Right_End)];
         --  Primitive points carry E only for E-only moves. Place both retained ends on the commanded density frame so
         --  the blender's E bound fallback checks these points rather than the neighbouring commanded corners.
         Geometry.Corners (1) (E_Axis) :=
           Left.Corners (Left_Seam) (E_Axis)
           - Left.Extrusion_Densities (Left_Seam) * (Primitive_Length (Left'Access, Left_Seam) - Left_Start);
         Geometry.Corners (3) (E_Axis) := Right.Corners (1) (E_Axis) + Right.Extrusion_Densities (2) * Right_End;

         Work.Unblended_Extrusion_Positions (1) := Geometry.Corners (1) (E_Axis);
         for I in Geometry.Primitives'Range loop
            Work.Unblended_Segment_Lengths (I) := Primitive_Length (Geometry'Access, I);
            Work.Unblended_Extrusion_Positions (I) :=
              Work.Unblended_Extrusion_Positions (I - 1)
              + Geometry.Extrusion_Densities (I) * Work.Unblended_Segment_Lengths (I);
         end loop;
         My_Corner_Blender.Run (Geometry, Motor_Map, Work'Access);

         if Geometry.Primitives (2).Kind = Line_Primitive_Kind
           and then Geometry.Primitives (3).Kind = Line_Primitive_Kind
           and then Geometry.Extrusion_Densities (2) /= 0.0
           and then Geometry.Extrusion_Densities (2) = Geometry.Extrusion_Densities (3)
           and then
             (for all Axis in Axis_Name =>
                Axis = E_Axis
                or else
                  (Geometry.Corners (1) (Axis) = Geometry.Corners (2) (Axis)
                   and then Geometry.Corners (2) (Axis) = Geometry.Corners (3) (Axis)))
         then
            --  Same-direction E-only moves have a stationary spatial corner and a continuous scalar path. The spatial
            --  blender deliberately sees zero-length primitives; their E velocity profiles still admit a passthrough.
            Geometry.Corner_Transitions (2) :=
              To_Evaluator (Passthrough_At ([Geometry.Corners (2) with delta E_Axis => 0.0 * mm]));
         end if;
      end Blend_Seam;

      procedure Build_Patch is
         Left_Trim   : constant Length := Primitive_Length (Geometry'Access, 2) - Geometry.Primitive_Distances (2);
         Right_Trim  : constant Length := Geometry.Primitive_Start_Distances (3);
         --  Path the new seam transition takes from each primitive.
         Right_Shift : Length;
         --  E translation of Right's references inside the window.
      begin
         Initialize_Scratch_Block (Patch);
         Patch.Params := Left.Params;
         Copy_Window (Left, Left_First, Left_Seam, Patch, 1);
         Copy_Window (Right, 1, Right_Last, Patch, Seam);
         Patch.Corner_Transitions (Seam) := Geometry.Corner_Transitions (2);

         --  Apply the trims to the original primitive lengths. Measuring the clipped scratch endpoints again can
         --  differ by an ulp; in particular an untrimmed passthrough must not manufacture an extrusion shortening.
         Patch.Primitive_Distances (Seam) := Left.Primitive_Distances (Left_Seam) - Left_Trim;
         Patch.Primitive_Start_Distances (Seam + 1) := Right.Primitive_Start_Distances (2) + Right_Trim;
         Patch.Primitive_Distances (Seam + 1) := Right.Primitive_Distances (2) - Right_Trim;

         --  Translate existing references by the two local shortening deltas. Reintegrating every density would
         --  introduce roundoff drift even at an exactly straight seam and lose exact stopped endpoints.
         Patch.Extrusion_Reference_Positions (Seam) :=
           Left.Extrusion_Reference_Positions (Left_Seam)
           + Left.Extrusion_Densities (Left_Seam)
             * (Segment_Total_Distance (Patch'Access, Seam) - Segment_Total_Distance (Left'Access, Left_Seam));
         Right_Shift :=
           Patch.Extrusion_Reference_Positions (Seam) - Right.Extrusion_Reference_Positions (1)
           + Right.Extrusion_Densities (2)
             * (Segment_Total_Distance (Patch'Access, Seam + 1) - Segment_Total_Distance (Right'Access, 2));
         for I in 2 .. Right_Last loop
            Patch.Extrusion_Reference_Positions (Seam + I - 1) :=
              Right.Extrusion_Reference_Positions (I) + Right_Shift;
         end loop;

         for I in Patch.Corners'Range loop
            Work.Corner_Derivative_Bounds (I) := Derivative_Bounds (Patch.Corner_Transitions (I));
         end loop;
         for I in Patch.Primitives'Range loop
            --  Shortening must still respect the minimum interpolation time.
            Patch.Limited_Segment_Feedrates (I) :=
              Velocity'Min
                (Patch.Limited_Segment_Feedrates (I), Segment_Total_Distance (Patch'Access, I) / Interpolation_Time);
         end loop;
      end Build_Patch;

      function Original_Time return Time is
         Total : Time := 0.0 * s;
      begin
         for I in Left_First + 1 .. Left_Seam loop
            Total := @ + Segment_Time (Left'Access, I);
         end loop;
         for I in 2 .. Right_Last loop
            Total := @ + Segment_Time (Right'Access, I);
         end loop;
         return Total;
      end Original_Time;

      function Patch_Time return Time is
         Total : Time := 0.0 * s;
      begin
         for I in Patch.Primitives'Range loop
            Total := @ + Segment_Time (Patch'Access, I);
         end loop;
         return Total;
      end Patch_Time;

      function E_In_Bounds
        (Block : Execution_Block; First, Last : Finishing_Corners_Index; Shift : Length) return Boolean
      is
         Lower : constant Length := Block.Params.Bounds.Lower_E;
         Upper : constant Length := Block.Params.Bounds.Upper_E;
      begin
         --  Each segment's reference is monotone, so its endpoints bound it.
         return
           (for all I in First .. Last =>
              Block.Extrusion_Reference_Positions (I - 1) + Shift in Lower .. Upper
              and then Block.Extrusion_Reference_Positions (I) + Shift in Lower .. Upper);
      end E_In_Bounds;
   begin
      Blended := False;

      begin
         Blend_Seam;
         if Policy (Geometry.Corner_Transitions (2)) = Hard_Stop then
            return;
         end if;

         Build_Patch;
         My_Kinematic_Limiter.Run (Patch, Motor_Map, Work'Access, Fixed_Ends => True);
         if Patch.Corner_Velocity_Limits (Seam) <= 0.0 * mm / s then
            return;
         end if;
         My_Feedrate_Profile_Generator.Run (Patch, Motor_Map, Result);
         if not Result.Valid or else Patch_Time >= Original_Time then
            return;
         end if;

         --  The seam translates references inside the window as well as after it, and the blender only checked its
         --  scratch frame, so certify every replacement segment. Right's untouched suffix only moves by Suffix_Shift,
         --  which a linear pass certifies without another solve. This also evaluates every sum the commit assigns.
         Suffix_Shift :=
           Patch.Extrusion_Reference_Positions (Patch.N_Corners) - Right.Extrusion_Reference_Positions (Right_Last);
         if not E_In_Bounds (Patch, 2, Patch.N_Corners, 0.0 * mm)
           or else not E_In_Bounds (Right, Right_Last + 1, Right.N_Corners, Suffix_Shift)
         then
            return;
         end if;
      exception
         when others =>
            --  The replacement is optional and may fail in any planning stage. Left and Right are not modified before
            --  the commit below, so both stopped plans remain valid.
            return;
      end;

      --  Patch geometry outside the seam equals the originals, so copying whole windows changes only replanned fields.
      Copy_Window (Patch, 1, Seam, Left, Left_First);
      Copy_Window (Patch, Seam, Patch.N_Corners, Right, 1);
      for I in Right_Last + 1 .. Right.N_Corners loop
         Right.Extrusion_Reference_Positions (I) := @ + Suffix_Shift;
      end loop;
      Blended := True;
   end Blend_Blocks;

   procedure Copy_Window
     (Source : Execution_Block; First, Last : Corners_Index; Target : in out Execution_Block; Into : Corners_Index) is
   begin
      for I in First .. Last loop
         declare
            J : constant Corners_Index := Into + (I - First);
         begin
            Target.Corners (J) := Source.Corners (I);
            Target.Corner_Transitions (J) := Source.Corner_Transitions (I);
            Target.Corner_Velocity_Limits (J) := Source.Corner_Velocity_Limits (I);
            Target.Extrusion_Reference_Positions (J) := Source.Extrusion_Reference_Positions (I);
            if I > First then
               Target.Primitives (J) := Source.Primitives (I);
               Target.Original_Segment_Feedrates (J) := Source.Original_Segment_Feedrates (I);
               Target.Limited_Segment_Feedrates (J) := Source.Limited_Segment_Feedrates (I);
               Target.Corner_Dwell_Times (J) := Source.Corner_Dwell_Times (I);
               Target.Extrusion_Densities (J) := Source.Extrusion_Densities (I);
               Target.Primitive_Start_Distances (J) := Source.Primitive_Start_Distances (I);
               Target.Primitive_Distances (J) := Source.Primitive_Distances (I);
               Target.Feedrate_Profiles (J) := Source.Feedrate_Profiles (I);
               Target.Profile_Crackles (J) := Source.Profile_Crackles (I);
               Target.Profile_Windows (J) := Source.Profile_Windows (I);
               Target.Profile_Ends (J) := Source.Profile_Ends (I);
            end if;
         end;
      end loop;
   end Copy_Window;

   procedure Clear_Planned_Fields (Block : not null access Execution_Block) is
   begin
      Block.Loop_Move_Minimum_Time := 0.0 * s;
      Block.Extrusion_Reference_Positions := [others => 0.0 * mm];
      Block.Extrusion_Densities := [others => 0.0];
      Block.Primitive_Distances := [others => 0.0 * mm];
      Block.Primitive_Start_Distances := [others => 0.0 * mm];
      Block.Corner_Velocity_Limits := [others => 0.0 * mm / s];
      Block.Feedrate_Profiles :=
        [others => (Accel => [others => 0.0 * s], Coast => 0.0 * s, Decel => [others => 0.0 * s])];
      Block.Profile_Crackles := [others => 0.0 * mm / s ** 5];
      Block.Profile_Windows := [others => <>];
      Block.Profile_Ends := [others => <>];
   end Clear_Planned_Fields;

   procedure Initialize_Scratch_Block (Block : aliased out Execution_Block) is
   begin
      Block.Next_Block_Pos := [others => 0.0 * mm];
      Block.Is_Homing_Move := False;
      Block.Corners_Extra_Data_End_Indices := [others => Corner_Extra_Data_Vectors.Extended_Index'First];
      Block.Corners := [others => [others => 0.0 * mm]];
      Block.Primitives := [others => Make_Line_Primitive];
      Block.Original_Segment_Feedrates := [others => 0.0 * mm / s];
      Block.Limited_Segment_Feedrates := [others => 0.0 * mm / s];
      Block.Corner_Dwell_Times := [others => 0.0 * s];
      Clear_Planned_Fields (Block'Access);
   end Initialize_Scratch_Block;

   procedure Copy_Motion_Window
     (Source : Execution_Block; First, Last : Corners_Index; Target : aliased out Execution_Block)
   is
      Start : constant Corners_Index := Target.N_Corners - (Last - First);
   begin
      Initialize_Scratch_Block (Target);
      Target.Blendable_End := Source.Blendable_End;
      Target.Kind := Source.Kind;
      Target.Next_Block_Pos := Source.Next_Block_Pos;
      Target.Params := Source.Params;
      Target.First_Corner_ID := Source.First_Corner_ID;
      Target.Associated_Overflow_Block := Source.Associated_Overflow_Block;
      Target.Is_Homing_Move := Source.Is_Homing_Move;
      Target.Loop_Move_Minimum_Time := Source.Loop_Move_Minimum_Time;
      --  Corners before Start are unused padding. Keep them stationary at the window's first corner.
      Target.Corners := [others => Source.Corners (First)];
      Target.Corner_Transitions := [others => Source.Corner_Transitions (First)];
      Copy_Window (Source, First, Last, Target, Start);
   end Copy_Motion_Window;

   function Blendable_Boundary (Left, Right : Execution_Block) return Boolean
   is (Left.Blendable_End
       and then Left.Kind = Motion_Block_Kind
       and then Right.Kind = Motion_Block_Kind
       and then Left.N_Corners >= 2
       and then Right.N_Corners >= 2
       and then not Left.Associated_Overflow_Block
       and then not Left.Is_Homing_Move
       and then not Right.Is_Homing_Move
       and then Left.Params = Right.Params
       and then Left.Corners (Left.N_Corners) = Right.Corners (1)
       and then Left.Corner_Dwell_Times (Left.N_Corners) = 0.0 * s);

   task body Runner is
      Lifetime  : Runner_Finalizer;
      pragma Unreferenced (Lifetime);
      --  Declare first so allocation failures also notify consumers, and cleanup runs after the workspace is freed.
      Pool      : System.Pool_Local.Unbounded_Reclaim_Pool;
      type Planning_Workspace_Access is access Planning_Workspace with Storage_Pool => Pool;
      Workspace : constant Planning_Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Index     : Block_Index;

      Reset_Called      : Boolean := False;
      Current_Motor_Map : Motor_Position_Map := [others => [others => 0.0 / mm]];
      Next_Block_Start  : Position := Initial_Position;
   begin
      loop
         accept Setup (In_Params : Kinematic_Parameters; In_Motor_Map : Motor_Position_Map) do
            My_Preprocessor.Setup (In_Params);
            Current_Motor_Map := In_Motor_Map;
            Next_Block_Start := Initial_Position;
            Block_Ring.Setup;
         end Setup;

         Planning_Loop : loop
            Block_Ring.Acquire (Index, Reset_Called);
            exit Planning_Loop when Reset_Called;

            declare
               Block : Execution_Block renames Blocks (Index).Block;
            begin
               My_Preprocessor.Run (Block'Access, Blocks (Index).Extra_Data, Next_Block_Start, Reset_Called);

               exit Planning_Loop when Reset_Called;

               if Block.Kind /= Extra_Data_Overflow_Block_Kind then
                  if Block.Is_Homing_Move and then Block.N_Corners /= 2 then
                     raise Constraint_Error with "Homing move must have exactly 2 corners.";
                  end if;

                  My_Extrusion_Density_Normalizer.Run (Block, Workspace);
                  My_Corner_Blender.Run (Block, Current_Motor_Map, Workspace);
                  My_Early_Kinematic_Limiter.Run (Block, Current_Motor_Map);

                  declare
                     Have_Previous_Violation : Boolean := False;
                     Previous_Violation      : Time := 0.0 * s;
                     Adjustment_Count        : Natural := 0;
                  begin
                     loop
                        Plan_Kinematics (Block, Current_Motor_Map, Workspace);

                        exit when
                          (not Block.Is_Homing_Move)
                          or else
                            (Block.Feedrate_Profiles (2).Coast >= Home_Move_Required_Coast_Time
                             and then Homing_Unavoidable_Tail_Time (Block'Access) <= Home_Move_Maximum_Tail_Time);

                        declare
                           Violation         : constant Time := Homing_Profile_Violation (Block'Access);
                           Previous_Feedrate : constant Velocity := Block.Limited_Segment_Feedrates (2);
                           New_Feedrate      : constant Velocity := Previous_Feedrate * 0.9;
                        begin
                           if Have_Previous_Violation and then Violation > Previous_Violation then
                              raise Constraint_Error
                                with "Reducing the homing feedrate made the coast or retained-tail violation worse.";
                           end if;

                           if Adjustment_Count = Maximum_Homing_Feedrate_Adjustments then
                              raise Constraint_Error with "Homing feedrate adjustment exceeded its iteration limit.";
                           end if;

                           if New_Feedrate <= 0.0 * mm / s or else New_Feedrate >= Previous_Feedrate then
                              raise Constraint_Error with "Homing feedrate adjustment made no representable progress.";
                           end if;

                           Have_Previous_Violation := True;
                           Previous_Violation := Violation;
                           Adjustment_Count := @ + 1;
                           Block.Limited_Segment_Feedrates (2) := New_Feedrate;
                        end;
                     end loop;
                  end;

                  if Block.Is_Homing_Move then
                     declare
                        Finishing_Corner  : constant Finishing_Corners_Index := Finishing_Corners_Index'First;
                        Window            : constant Profile_Window :=
                          Selected_Profile_Window (Block'Access, Finishing_Corner);
                        Prefix_Time       : constant Time :=
                          Constant_Speed_Time
                            (Window.Start_Distance, Block.Corner_Velocity_Limits (Finishing_Corner - 1));
                        --  Time spent in the constant-speed prefix before the generated acceleration profile.
                        Coast_Start_Time  : constant Time :=
                          Prefix_Time + Total_Time (Block.Feedrate_Profiles (Finishing_Corner).Accel);
                        --  First time at which the generated profile has reached its constant-velocity coast.
                        End_Time          : constant Time := Segment_Time (Block'Access, Finishing_Corner);
                        --  Time at the end of the complete homing segment, including any suffix and dwell.
                        Minimum_Time      : constant Time :=
                          Time'Max
                            (Coast_Start_Time + Home_Move_Minimum_Coast_Time, End_Time - Home_Move_Maximum_Tail_Time);
                        --  Leave the configured coast margin after acceleration, while placing the loop command late
                        --  enough that every retained command fits within the hardware tail-time limit.
                        Loop_Pos          : constant Position :=
                          Segment_Pos_At_Time (Block'Access, Finishing_Corner, Minimum_Time);
                        --  Planned tool position at the earliest permitted loop-command time. The step generator uses
                        --  the first regular interpolation sample at or after this point, so the published tail is
                        --  conservatively longer than the emitted tail by less than one interpolation period.
                        Tail_Offset       : constant Position_Offset := Block.Corners (Block.N_Corners) - Loop_Pos;
                        --  Conservative planned displacement from a detector hit through the complete retained tail.
                        Resolved_Position : Position;
                     begin
                        Block.Loop_Move_Minimum_Time := Minimum_Time;
                        My_Preprocessor.Publish_Homing_Tail_Offset (Tail_Offset);
                        My_Preprocessor.Wait_For_Resolved_Homing_Position (Resolved_Position, Reset_Called);
                        exit Planning_Loop when Reset_Called;
                        Block.Next_Block_Pos := Resolved_Position;
                     end;
                  end if;
               end if;

               Next_Block_Start := Block.Next_Block_Pos;

               Block_Ring.Publish (Index);
            end;
         end loop Planning_Loop;

         begin
            accept Reset_Do_Not_Call_From_Other_Packages do
               Discard_Blocks;
            end Reset_Do_Not_Call_From_Other_Packages;
         exception
            when others =>
               --  The rendezvous also propagates cleanup errors to the reset caller. All slots have been
               --  returned, so keep Runner alive for the next Setup after any of the reset exit paths.
               null;
         end;
      end loop;
   end Runner;

   function Constant_Speed_Time (Distance : Length; Speed : Velocity) return Time is
   begin
      if Distance <= 0.0 * mm then
         return 0.0 * s;
      elsif Speed > 0.0 * mm / s then
         return Distance / Speed;
      else
         raise Constraint_Error with "A non-zero corner transition cannot be traversed at zero speed.";
      end if;
   end Constant_Speed_Time;

   function Point_At_Segment_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index; Distance : Length)
      return Position
   is
      Start_Transition : constant Length := Segment_Start_Transition_Distance (Block, Finishing_Corner);
      Middle           : constant Length := Block.Primitive_Distances (Finishing_Corner);
      End_Start        : constant Length := Start_Transition + Middle;
      Total            : constant Length := Segment_Total_Distance (Block, Finishing_Corner);
      D                : constant Length := Length'Max (0.0 * mm, Length'Min (Distance, Total));
      function Spatial_Point return Position;

      function Spatial_Point return Position is
      begin
         if D <= Start_Transition then
            return
              Point_At_Distance
                (Block.Corner_Transitions (Finishing_Corner - 1),
                 Split_Distance (Block.Corner_Transitions (Finishing_Corner - 1)) + D);
         elsif D <= End_Start then
            if Middle = 0.0 * mm then
               return Point_At_Parameter (Block.Corner_Transitions (Finishing_Corner - 1), 1.0);
            else
               return
                 Primitive_Point_At_Distance
                   (Block,
                    Finishing_Corner,
                    Block.Primitive_Start_Distances (Finishing_Corner) + D - Start_Transition);
            end if;
         else
            return Point_At_Distance (Block.Corner_Transitions (Finishing_Corner), D - End_Start);
         end if;
      end Spatial_Point;
   begin
      return Result : Position := Spatial_Point do
         Result (E_Axis) := Extrusion_Reference_At_Distance (Block, Finishing_Corner, D);
      end return;
   end Point_At_Segment_Distance;

   function Segment_Prefix_Time
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Time is
   begin
      return
        (if Block.Profile_Ends (Corner).Enabled
         then Total_Time (Block.Profile_Ends (Corner).Prefix.Profile)
         else
           Constant_Speed_Time
             (Block.Profile_Windows (Corner).Start_Distance, Block.Corner_Velocity_Limits (Corner - 1)));
   end Segment_Prefix_Time;

   function Segment_Suffix_Time
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Time
   is
      Window : constant Profile_Window := Block.Profile_Windows (Corner);
   begin
      return
        (if Block.Profile_Ends (Corner).Enabled
         then Total_Time (Block.Profile_Ends (Corner).Suffix.Profile)
         else
           Constant_Speed_Time
             (Segment_Total_Distance (Block, Corner) - Window.Start_Distance - Window.Distance,
              Block.Corner_Velocity_Limits (Corner)));
   end Segment_Suffix_Time;

   function Central_Profile_Start_Velocity
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Velocity is
   begin
      return
        (if Block.Profile_Ends (Corner).Enabled
         then Block.Profile_Ends (Corner).Prefix.Boundary_Velocity
         else Block.Corner_Velocity_Limits (Corner - 1));
   end Central_Profile_Start_Velocity;

   function Segment_Time
     (Block : not null access constant Execution_Block; Finishing_Corner : Corners_Index) return Time is
   begin
      return
        Segment_Prefix_Time (Block, Finishing_Corner)
        + Total_Time (Block.Feedrate_Profiles (Finishing_Corner))
        + Segment_Suffix_Time (Block, Finishing_Corner)
        + Block.Corner_Dwell_Times (Finishing_Corner);
   end Segment_Time;

   function Segment_Corner_Distance (Block : Execution_Block; Finishing_Corner : Corners_Index) return Length is
   begin
      if Finishing_Corner in Block.Primitives'Range then
         return
           Derive_Path_Primitive
             (Block.Primitives (Finishing_Corner),
              Block.Corners (Finishing_Corner - 1),
              Block.Corners (Finishing_Corner))
             .Length;
      else
         return 0.0 * mm;
      end if;
   end Segment_Corner_Distance;

   function Segment_Pos_At_Time
     (Block             : not null access constant Execution_Block;
      Finishing_Corner  : Finishing_Corners_Index;
      Time_Into_Segment : Time) return Position
   is
      Window       : constant Profile_Window := Selected_Profile_Window (Block, Finishing_Corner);
      Ends         : constant Profile_End_Pair := Block.Profile_Ends (Finishing_Corner);
      Prefix_Time  : constant Time := Segment_Prefix_Time (Block, Finishing_Corner);
      Profile_Time : constant Time := Total_Time (Block.Feedrate_Profiles (Finishing_Corner));
      Suffix_Time  : constant Time := Segment_Suffix_Time (Block, Finishing_Corner);
      Motion_Time  : constant Time := Prefix_Time + Profile_Time + Suffix_Time;
      Distance     : Length;
   begin
      if Finishing_Corner = Block.N_Corners
        and then Time_Into_Segment >= Motion_Time
        and then Block.Corner_Velocity_Limits (Finishing_Corner) = 0.0 * mm / s
      then
         --  Reconstructing the endpoint from direction and distance can leave a rounding residual. Shaper draining
         --  and pause/return plans need the exact stopped position, including any path-length extrusion correction.
         return
           [Block.Corners (Finishing_Corner) with delta
              E_Axis => Block.Extrusion_Reference_Positions (Finishing_Corner)];
      elsif Time_Into_Segment >= Motion_Time then
         Distance := Segment_Total_Distance (Block, Finishing_Corner);
      elsif Time_Into_Segment < Prefix_Time then
         Distance :=
           (if Ends.Enabled
            then
              Distance_At_Time
                (Ends.Prefix.Profile,
                 Time_Into_Segment,
                 Ends.Prefix.Max_Crackle,
                 Block.Corner_Velocity_Limits (Finishing_Corner - 1))
            else Block.Corner_Velocity_Limits (Finishing_Corner - 1) * Time_Into_Segment);
      elsif Time_Into_Segment < Prefix_Time + Profile_Time then
         Distance :=
           Window.Start_Distance
           + Distance_At_Time
               (Block.Feedrate_Profiles (Finishing_Corner),
                Time'Min (Time_Into_Segment - Prefix_Time, Profile_Time),
                Block.Profile_Crackles (Finishing_Corner),
                Central_Profile_Start_Velocity (Block, Finishing_Corner));
      else
         Distance :=
           Window.Start_Distance
           + Window.Distance
           + (if Ends.Enabled
              then
                Distance_At_Time
                  (Ends.Suffix.Profile,
                   Time'Min (Time_Into_Segment - (Prefix_Time + Profile_Time), Suffix_Time),
                   Ends.Suffix.Max_Crackle,
                   Ends.Suffix.Boundary_Velocity)
              else
                Block.Corner_Velocity_Limits (Finishing_Corner)
                * Time'Min (Time_Into_Segment - (Prefix_Time + Profile_Time), Suffix_Time));
      end if;
      return Point_At_Segment_Distance (Block, Finishing_Corner, Distance);
   end Segment_Pos_At_Time;

   function Segment_Vel_Ratio_At_Time
     (Block             : not null access constant Execution_Block;
      Finishing_Corner  : Finishing_Corners_Index;
      Time_Into_Segment : Time) return Dimensionless
   is
      Ends         : constant Profile_End_Pair := Block.Profile_Ends (Finishing_Corner);
      Prefix_Time  : constant Time := Segment_Prefix_Time (Block, Finishing_Corner);
      Profile_Time : constant Time := Total_Time (Block.Feedrate_Profiles (Finishing_Corner));
      Suffix_Time  : constant Time := Segment_Suffix_Time (Block, Finishing_Corner);
      Motion_Time  : constant Time := Prefix_Time + Profile_Time + Suffix_Time;
      Speed        : Velocity;
   begin
      if Time_Into_Segment > Motion_Time then
         --  Return 1.0 inside dwell parts so the laser can be set to the programmed power level.
         return 1.0;
      elsif Time_Into_Segment < Prefix_Time then
         Speed :=
           (if Ends.Enabled
            then
              Velocity_At_Time
                (Ends.Prefix.Profile,
                 Time_Into_Segment,
                 Ends.Prefix.Max_Crackle,
                 Block.Corner_Velocity_Limits (Finishing_Corner - 1))
            else Block.Corner_Velocity_Limits (Finishing_Corner - 1));
      elsif Time_Into_Segment < Prefix_Time + Profile_Time then
         Speed :=
           Velocity_At_Time
             (Block.Feedrate_Profiles (Finishing_Corner),
              Time'Min (Time_Into_Segment - Prefix_Time, Profile_Time),
              Block.Profile_Crackles (Finishing_Corner),
              Central_Profile_Start_Velocity (Block, Finishing_Corner));
      else
         Speed :=
           (if Ends.Enabled
            then
              Velocity_At_Time
                (Ends.Suffix.Profile,
                 Time'Min (Time_Into_Segment - (Prefix_Time + Profile_Time), Suffix_Time),
                 Ends.Suffix.Max_Crackle,
                 Ends.Suffix.Boundary_Velocity)
            else Block.Corner_Velocity_Limits (Finishing_Corner));
      end if;
      return Velocity'Max (0.0 * mm / s, Speed) / Block.Original_Segment_Feedrates (Finishing_Corner);
   end Segment_Vel_Ratio_At_Time;

   function Next_Block_Pos (Block : not null access constant Execution_Block) return Position is
   begin
      return Block.Next_Block_Pos;
   end Next_Block_Pos;

   function Block_Start_Pos (Block : not null access constant Execution_Block) return Position is
   begin
      return Block.Corners (Corners_Index'First);
   end Block_Start_Pos;

   function Flush_Resetting_Data (Block : not null access constant Execution_Block) return Flush_Resetting_Data_Type is
   begin
      return Block.Flush_Resetting_Data;
   end Flush_Resetting_Data;

   function Loop_Move_Minimum_Time (Block : not null access constant Execution_Block) return Time is
   begin
      return Block.Loop_Move_Minimum_Time;
   end Loop_Move_Minimum_Time;

   function Block_Kind (Block : Execution_Block) return Execution_Block_Kind is
   begin
      return Block.Kind;
   end Block_Kind;

   function Corner_ID
     (Block : not null access constant Execution_Block; Corner : Corners_Index) return Planner_Corner_ID is
   begin
      return Block.First_Corner_ID + Planner_Corner_ID (Corner - Corners_Index'First);
   end Corner_ID;

   procedure Corner_Extra_Data
     (Block      : not null access constant Execution_Block;
      Extra_Data : Corner_Extra_Data_Vectors.Vector;
      Corner     : Corners_Index;
      Process    : not null access procedure (Data : in out Corner_Extra_Data_Type)) is
   begin
      Extra_Data.Process_Range
        ((if Corner = Corners_Index'First
          then Corners_Extra_Data_Index'First
          else Corners_Extra_Data_Index (Block.Corners_Extra_Data_End_Indices (Corner - 1) + 1)),
         Block.Corners_Extra_Data_End_Indices (Corner),
         Process);
   end Corner_Extra_Data;

   function Has_Associated_Overflow_Block (Block : not null access constant Execution_Block) return Boolean is
   begin
      return Block.Associated_Overflow_Block;
   end Has_Associated_Overflow_Block;

   function Block_Kinematic_Parameters (Block : not null access constant Execution_Block) return Kinematic_Parameters
   is
   begin
      return Block.Params;
   end Block_Kinematic_Parameters;

   function Is_Homing_Move (Block : not null access constant Execution_Block) return Boolean is
   begin
      return Block.Is_Homing_Move;
   end Is_Homing_Move;

end Prunt.Motion_Planner.Planner;
