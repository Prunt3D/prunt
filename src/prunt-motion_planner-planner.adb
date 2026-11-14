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

with Ada.Assertions;
with Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Conversion;
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
         Cursor : Block_Index;
      begin
         Pending := [others => False];
         --  Ready roots have never been borrowed. Their linked descendants are also unreachable by execution.
         --  Preserve every other Linked slot: its predecessor may be borrowed, including across Reset/Setup.
         for Index in Block_Index loop
            if Blocks (Index).State = Ready then
               Cursor := Index;
               loop
                  Pending (Cursor) := True;
                  exit when Blocks (Cursor).Choice /= Offered;
                  Cursor := Blocks (Cursor).Next;
                  pragma Assert (Blocks (Cursor).State = Linked);
               end loop;
            elsif Blocks (Index).State in Planning | Retired then
               Pending (Index) := True;
            end if;
         end loop;
         for Index in Block_Index loop
            Blocks (Index).Planner_Reading := False;
            if Pending (Index) then
               Blocks (Index).State := Cleaning;
            end if;
         end loop;
         Tail_Valid := False;
         Ready_Count := 0;
         Next_Read := Block_Index'First;
         Next_Write := Block_Index'First;
         --  Successors preserved above belong to the old epoch. Releasing them later cannot strand new blocks.
         Epoch := @ + 1;
         Discontinuous := False;
      end Begin_Discard;

      entry Acquire (Index : out Block_Index; Reset_Called : out Boolean) when not Running or else Free_Count > 0 is
      begin
         Reset_Called := not Running;
         Index := Next_Slot;
         if Reset_Called then
            return;
         end if;
         while Blocks (Index).State /= Free loop
            Index := @ + 1;
         end loop;
         Blocks (Index).State := Planning;
         Blocks (Index).Choice := Open;
         Slot_Epochs (Index) := Epoch;
         Free_Count := @ - 1;
         Next_Slot := Index + 1;
      end Acquire;

      procedure Begin_Blend (Previous : out Block_Index; Available : out Boolean) is
         Expected : aliased Tail_State := Open;
      begin
         Previous := Tail_Index;
         Available := False;
         if Running and then Tail_Valid and then Blocks (Previous).State in Ready | Linked | Borrowed then
            --  Pin before the exchange. Even if execution closes and retires this slot, cleanup cannot reuse it
            --  while the producer reads its immutable base trajectory.
            Blocks (Previous).Planner_Reading := True;
            Available := Tail_Atomics.Atomic_Compare_And_Exchange (Blocks (Previous).Choice, Expected, Preparing);
            if not Available then
               Blocks (Previous).Planner_Reading := False;
            end if;
         end if;
      end Begin_Blend;

      procedure Finish_Blend (Previous, Following : Block_Index; Blended : Boolean; Published : out Boolean) is
         Expected : aliased Tail_State := Preparing;
         Unused   : Boolean;
      begin
         Published := False;
         if Blended and then Running then
            Blocks (Following).State := Linked;
            Blocks (Previous).Next := Following;
            --  All replacement motion and the successor's incoming profiles precede this release publication.
            Published := Tail_Atomics.Atomic_Compare_And_Exchange (Blocks (Previous).Choice, Expected, Offered);
            if Published then
               Tail_Index := Following;
               Tail_Valid := True;
            else
               Blocks (Following).State := Planning;
            end if;
         else
            Unused := Tail_Atomics.Atomic_Compare_And_Exchange (Blocks (Previous).Choice, Expected, Closed);
         end if;
         Blocks (Previous).Planner_Reading := False;
      end Finish_Blend;

      procedure Publish (Index : Block_Index) is
      begin
         pragma Assert (Blocks (Index).State = Planning);
         Blocks (Index).State := Ready;
         Ready_Slots (Next_Write) := Index;
         Next_Write := @ + 1;
         Ready_Count := @ + 1;
         Tail_Index := Index;
         Tail_Valid := True;
      end Publish;

      entry Dequeue (Block : in out Block_Handle)
        when Runner_Terminated or else (Running and then Ready_Count > 0 and then not Discontinuous)
      is
         Index : constant Block_Index := Ready_Slots (Next_Read);
      begin
         if Runner_Terminated then
            raise Tasking_Error with "Motion planner runner has terminated.";
         end if;
         pragma Assert (Blocks (Index).State = Ready);
         Block.Data := Blocks (Index).Block'Access;
         Block.Index := Index;
         Block.Use_Tail := False;
         Block.Tail_Decided := False;
         Blocks (Index).State := Borrowed;
         Next_Read := @ + 1;
         Ready_Count := @ - 1;
      end Dequeue;

      entry Begin_Release (for Index in Block_Index) when not Blocks (Index).Planner_Reading is
      begin
         --  Planner_Reading changes only inside this object's procedures, whose completion re-evaluates the barrier.
         pragma Assert (Blocks (Index).State in Borrowed | Linked);
         if Blocks (Index).State = Linked and then Slot_Epochs (Index) = Epoch then
            --  Any block queued after this successor starts at its end, which execution never reached.
            Discontinuous := True;
         end if;
         Blocks (Index).State := Cleaning;
      end Begin_Release;

      procedure Collect_Retired (Pending : out Block_Set) is
      begin
         for Index in Block_Index loop
            Pending (Index) := Blocks (Index).State = Retired and then not Blocks (Index).Planner_Reading;
            if Pending (Index) then
               Blocks (Index).State := Cleaning;
            end if;
         end loop;
      end Collect_Retired;

      procedure Request_Collection is
      begin
         if (for some Slot of Blocks.all => Slot.State = Retired) then
            Collection_Requested := True;
         end if;
      end Request_Collection;

      pragma Warnings (Off, "potentially unsynchronized barrier");
      --  TODO: GNAT bug (Slot incorrectly gets a warning, Blocks incorrectly doesn't).
      entry Wait_For_Retirement (Pending : out Block_Set; Stop : out Boolean)
        when Runner_Terminated
        or else Collection_Requested
        or else (for some Slot of Blocks.all => Slot.State = Linked)
        or else (for some Slot of Blocks.all => Slot.State = Retired)
      is
      begin
         --  The barrier scans for Linked before Retired. A consumer retires its predecessor before claiming the Linked
         --  successor, and only this object moves a slot out of Retired, so a hand-off that hides the successor from
         --  the first scan leaves the retired predecessor for the second.
         Collection_Requested := False;
         Collect_Retired (Pending);
         for Index in Block_Index loop
            if Pending (Index) then
               Deferred_Cleanup (Index) := True;
            end if;
         end loop;
         Stop := Runner_Terminated and then (for all Taken of Pending => not Taken);
      end Wait_For_Retirement;
      pragma Warnings (On, "potentially unsynchronized barrier");

      entry Wait_For_Collection_Request when Runner_Terminated or else Collection_Requested is
      begin
         null;
      end Wait_For_Collection_Request;

      procedure Finish_Cleanup (Index : Block_Index) is
      begin
         pragma Assert (Blocks (Index).State = Cleaning);
         if Tail_Valid and then Tail_Index = Index then
            Tail_Valid := False;
         end if;
         Deferred_Cleanup (Index) := False;
         Blocks (Index).State := Free;
         Free_Count := @ + 1;
      end Finish_Cleanup;

      procedure Defer_Cleanup_Error (Occurrence : Ada.Exceptions.Exception_Occurrence) is
      begin
         Deferred_Error.Save (Occurrence);
      end Defer_Cleanup_Error;

      pragma Warnings (Off, "potentially unsynchronized barrier");
      --  As for Wait_For_Retirement.
      entry Take_Cleanup_Error (Error : in out Cleanup_Exceptions.Holder)
        when (for all Pending of Deferred_Cleanup => not Pending)
      is
      begin
         Cleanup_Exceptions.Transfer (From => Deferred_Error, To => Error);
      end Take_Cleanup_Error;
      pragma Warnings (On, "potentially unsynchronized barrier");
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
   procedure Reclaim_Retired;

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
         Square_Sum := Square_Sum + (Coefficient / Scale)**2;
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

   procedure Wait_For_Input_Space is
   begin
      My_Preprocessor.Wait_For_Input_Space;
   end Wait_For_Input_Space;

   procedure Discard_Input is
   begin
      My_Preprocessor.Discard_Input;
   end Discard_Input;

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
      Index : Block_Index := Block.Index;
   begin
      pragma Abort_Defer;
      if Is_Empty (Block) then
         return;
      end if;
      loop
         declare
            Choice   : constant Tail_State := Tail_Atomics.Atomic_Exchange (Blocks (Index).Choice, Closed);
            Has_Next : constant Boolean := Choice in Offered | Accepted;
            Next     : constant Block_Index := (if Has_Next then Blocks (Index).Next else Index);
         begin
            --  Safe-stop release may wait for a producer which lost the choice to finish reading this slot.
            Block_Ring.Begin_Release (Index);
            Cleanup_Block (Index, Error);
            exit when not Has_Next;
            Index := Next;
         end;
      end loop;
      Block.Data := null;
      begin
         Reclaim_Retired;
      exception
         when E : others =>
            Error.Save (E);
      end;
      --  Waits for Retired_Cleaner's slots in progress so their failures are reported here rather than lost.
      Block_Ring.Take_Cleanup_Error (Error);
      Error.Reraise;
   end Release;

   procedure Prepare_Segment (Block : in out Block_Handle; Finishing_Corner : Finishing_Corners_Index) is
      Choice : Tail_State;
   begin
      pragma Abort_Defer;
      if not Block.Tail_Decided
        and then Finishing_Corner > Block.Data.N_Corners - Corners_Index'Min (Blend_Radius, Block.Data.N_Corners - 1)
      then
         Choice := Tail_Atomics.Atomic_Exchange (Blocks (Block.Index).Choice, Closed);
         Block.Use_Tail := Choice = Offered;
         Block.Tail_Decided := True;
         if Block.Use_Tail then
            Blocks (Block.Index).Choice := Accepted;
         end if;
      end if;
   end Prepare_Segment;

   function Has_Next_Block (Block : Block_Handle) return Boolean
   is (Block.Use_Tail);

   procedure Advance_Block (Block : in out Block_Handle) is
      Previous  : constant Block_Index := Block.Index;
      Following : constant Block_Index := Blocks (Previous).Next;
   begin
      pragma Abort_Defer;
      pragma Assert (Blocks (Following).State = Linked);
      Block.Index := Following;
      Block.Data := Blocks (Following).Block'Access;
      Block.Use_Tail := False;
      Block.Tail_Decided := False;
      Blocks (Previous).Choice := Closed;
      --  The last access to Previous precedes this release store. Cleanup may reclaim the slot only afterwards.
      --  Retire before claiming so Wait_For_Retirement always sees a Linked or Retired slot during the handoff.
      --  Until claimed, Following is a Linked slot with no Ready root, which reset and cleanup both preserve.
      Blocks (Previous).State := Retired;
      Blocks (Following).State := Borrowed;
   end Advance_Block;

   overriding
   procedure Finalize (Block : in out Block_Handle) is
   begin
      Release (Block);
   end Finalize;

   function Is_Empty (Block : Block_Handle) return Boolean
   is (Block.Data = null);

   function Corner_Count (Block : Block_Handle) return Corners_Index
   is (Block.Data.N_Corners);

   function Uses_Tail (Block : Block_Handle; Corner : Corners_Index) return Boolean
   is (Block.Use_Tail
       and then Corner > Block.Data.N_Corners - Corners_Index'Min (Blend_Radius, Block.Data.N_Corners - 1));

   function Tail_Corner (Block : Block_Handle; Corner : Corners_Index) return Corners_Index
   is (Blend_Radius + 1 - (Block.Data.N_Corners - Corner));

   function Segment_Time (Block : Block_Handle; Finishing_Corner : Corners_Index) return Time
   is (if Uses_Tail (Block, Finishing_Corner)
       then Segment_Time (Blocks (Block.Index).Tail'Access, Tail_Corner (Block, Finishing_Corner))
       else Segment_Time (Block.Data, Finishing_Corner));

   function Segment_Corner_Distance (Block : Block_Handle; Finishing_Corner : Corners_Index) return Length
   is (if Uses_Tail (Block, Finishing_Corner)
       then Segment_Corner_Distance (Blocks (Block.Index).Tail, Tail_Corner (Block, Finishing_Corner))
       else Segment_Corner_Distance (Block.Data.all, Finishing_Corner));

   function Segment_Pos_At_Time
     (Block : Block_Handle; Finishing_Corner : Finishing_Corners_Index; Time_Into_Segment : Time) return Position
   is (if Uses_Tail (Block, Finishing_Corner)
       then
         Segment_Pos_At_Time
           (Blocks (Block.Index).Tail'Access, Tail_Corner (Block, Finishing_Corner), Time_Into_Segment)
       else Segment_Pos_At_Time (Block.Data, Finishing_Corner, Time_Into_Segment));

   function Segment_Vel_Ratio_At_Time
     (Block : Block_Handle; Finishing_Corner : Finishing_Corners_Index; Time_Into_Segment : Time) return Dimensionless
   is (if Uses_Tail (Block, Finishing_Corner)
       then
         Segment_Vel_Ratio_At_Time
           (Blocks (Block.Index).Tail'Access, Tail_Corner (Block, Finishing_Corner), Time_Into_Segment)
       else Segment_Vel_Ratio_At_Time (Block.Data, Finishing_Corner, Time_Into_Segment));

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
      Result        : Curvature;
   begin
      if Amplitude = 0.0 / mm then
         return abs Offset;
      end if;

      Result :=
        Curvature'Max
          (abs (Offset + Amplitude * Dimensionless_Math.Sin (Low)),
           abs (Offset + Amplitude * Dimensionless_Math.Sin (High)));

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
         Start_Radius  : constant Length := (Start_Offset (X_Axis)**2 + Start_Offset (Y_Axis)**2)**(1 / 2);
         End_Radius    : constant Length := (End_Offset (X_Axis)**2 + End_Offset (Y_Axis)**2)**(1 / 2);
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
            Length_Per_Radian : constant Length := (Start_Radius**2 + (abs Axial_Per_Phase)**2)**(1 / 2);
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

   function Segment_Run_Direction (Spatial_Length, E_Change : Length) return Run_Direction is
   begin
      if E_Change = 0.0 * mm or else Spatial_Length = 0.0 * mm then
         return No_Run;
      end if;

      return (if E_Change > 0.0 * mm then Extruding else Retracting);
   end Segment_Run_Direction;

   function Primitive_Run_Direction
     (Primitive : Path_Primitive; Start_Point, End_Point : Position) return Run_Direction
   is
      Spatial : constant Derived_Path_Primitive :=
        Derive_Path_Primitive
          (Primitive, [Start_Point with delta E_Axis => 0.0 * mm], [End_Point with delta E_Axis => 0.0 * mm]);
   begin
      return Segment_Run_Direction (Spatial.Length, End_Point (E_Axis) - Start_Point (E_Axis));
   end Primitive_Run_Direction;

   function Continues_Run (Run, Next : Run_Direction; Dwell_Between : Time) return Boolean
   is (Next /= No_Run and then Next = Run and then Dwell_Between = 0.0 * s);

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

   Transition_Deviation_Ratio : constant Dimensionless := 35.0 / 256.0;
   --  A transition's departure from the reference peaks at its corner, at Half_Width * |Density_Change| times this:
   --  twice the integral of h over [0, 1/2].

   Transition_Derivative_Maxima : constant array (1 .. 4) of Dimensionless := [35.0 / 16.0, 7.5132, 52.5, 840.0];
   --  Maxima of |h'|, |h''|, |h'''| and |h''''| over [0, 1]. The second is 84 / (5 * Sqrt (5)), rounded up.

   Narrowest_Extrusion_Transition : constant Length := 0.001 * mm;
   --  A floor on transition half-widths. Nearly unlimited E derivative limits would otherwise give transitions
   --  so narrow that their profile cuts degenerate. Wider never lowers a corner's ceiling, and the extra deviation is
   --  negligible.

   function Transition_Shape (U : Dimensionless) return Dimensionless
   is (U**4 * (35.0 - U * (84.0 - U * (70.0 - 20.0 * U))));
   --  h (U): the fraction of the density change reached at U across the transition.

   function Transition_Offset (Transition : Extrusion_Transition; Distance : Length) return Length;
   --  E's offset from the reference that follows the density on one side of Transition, at Distance from that side's
   --  edge toward the corner: 2 * Half_Width * Density_Change * H (U), where H is the integral of h. By symmetry of h
   --  the same expression holds on both sides, and the offset vanishes with its first four derivatives at the edge.

   function Transition_Offset (Transition : Extrusion_Transition; Distance : Length) return Length is
      Width : constant Length := 2.0 * Transition.Half_Width;
      U     : constant Dimensionless := Distance / Width;
   begin
      return Transition.Density_Change * Width * U**5 * (7.0 - U * (14.0 - U * (10.0 - 2.5 * U)));
   end Transition_Offset;

   function Density_Change_Is_Negligible
     (Block : not null access constant Execution_Block; Corner : Corners_Index) return Boolean
   is (abs (Block.Extrusion_Densities (Corner + 1) - Block.Extrusion_Densities (Corner))
       <= Negligible_Density_Change
          * Dimensionless'Max (abs Block.Extrusion_Densities (Corner), abs Block.Extrusion_Densities (Corner + 1)));

   function Extrusion_Deviation (Params : Kinematic_Parameters) return Length
   is (if Params.Extrusion_Cornering.Kind /= Smooth_Deviation
       then 0.0 * mm
       else
         (case Params.Cornering.Kind is
            when Stereographic => Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis),
            when Circular      => Params.Cornering.Circular_Params.Axial_Deviation_Maxes (E_Axis),
            when Biarc         => Params.Cornering.Biarc_Params.Axial_Deviation_Maxes (E_Axis),
            when Sharp_SCV     => 0.0 * mm));

   function Smooths_Density_Change
     (Block : not null access constant Execution_Block; Corner : Corners_Index) return Boolean
   is
      Incoming : constant Dimensionless := Block.Extrusion_Densities (Corner);
      Outgoing : constant Dimensionless := Block.Extrusion_Densities (Corner + 1);
   begin
      if Block.Params.Extrusion_Cornering.Kind /= Smooth_Deviation
        or else Extrusion_Deviation (Block.Params) = 0.0 * mm
        or else Density_Change_Is_Negligible (Block, Corner)
      then
         return False;
      end if;
      --  A change that is not negligible has a nonzero density on its larger side.
      if abs Outgoing <= abs Incoming then
         return Outgoing / Incoming >= Block.Params.Extrusion_Cornering.Smoothed_Decrease_Ratio_Min;
      else
         return Incoming / Outgoing >= Block.Params.Extrusion_Cornering.Smoothed_Increase_Ratio_Min;
      end if;
   end Smooths_Density_Change;

   function Extrusion_Junction_Ceiling
     (Block : not null access constant Execution_Block; Corner : Corners_Index; Limit : Velocity) return Velocity
   is
      Change  : constant Dimensionless :=
        abs (Block.Extrusion_Densities (Corner + 1) - Block.Extrusion_Densities (Corner));
      Maximum : constant Velocity := Block.Params.Extrusion_Cornering.Velocity_Change_Max;
   begin
      if Block.Extrusion_Transitions (Corner).Half_Width > 0.0 * mm
        or else
          (Block.Params.Extrusion_Cornering.Kind = Smooth_Deviation
           and then Density_Change_Is_Negligible (Block, Corner))
      then
         return Limit;
      end if;
      --  The two density-scaled profiles meet directly, so E's velocity jumps by Change * Limit.
      return (if Change * Limit > Maximum then 0.999 * Maximum / Change else Limit);
   end Extrusion_Junction_Ceiling;

   function Corner_Policy_Ceiling
     (Block     : not null access constant Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Corner    : Corners_Index;
      Bounds    : Unit_Speed_Axial_Derivative_Bounds;
      Limit     : Velocity) return Velocity
   is
      Transition : constant Corner_Transition_Evaluator := Block.Corner_Transitions (Corner);
   begin
      case Policy (Transition) is
         when Hard_Stop              =>
            return 0.0 * mm / s;

         when Passthrough            =>
            return Limit;

         when Square_Corner_Velocity =>
            return Velocity'Min (Limit, Junction_Velocity_Limit (Transition));

         when Derivative_Bounded     =>
            if Arc_Length (Transition) <= 0.0 * mm then
               return 0.0 * mm / s;
            end if;
            return
              Motor_Delta_Ceiling_For_Projection
                (Block.Params,
                 Spatial_Motor_Map (Motor_Map),
                 Constant_Speed_Axial_Ceiling (Block.Params, Bounds, Limit));
      end case;
   end Corner_Policy_Ceiling;

   function Extrusion_Transition_Reach
     (Block     : not null access constant Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Corner    : Corners_Index) return Length
   is
      Change : constant Dimensionless :=
        abs (Block.Extrusion_Densities (Corner + 1) - Block.Extrusion_Densities (Corner));
      Widest : Length := 0.0 * mm;
      Speed  : Dimensionless;
      Result : Length := Narrowest_Extrusion_Transition;

      procedure Fit (Maximum, Limit : Dimensionless; Order : Positive);
      --  Widen Result until the transition's E derivative of this Order at Speed fits Limit, in mm/s**Order. At a
      --  constant speed v it peaks at v**Order * Change * Maximum / (2 * Half_Width)**(Order - 1).

      procedure Fit (Maximum, Limit : Dimensionless; Order : Positive) is
      begin
         Result := Length'Max (Result, 0.5 * mm * Nth_Root_Ratio (Change * Maximum * Speed**Order, Limit, Order - 1));
      end Fit;
   begin
      if not Smooths_Density_Change (Block, Corner) then
         return 0.0 * mm;
      end if;
      Widest := Block.Params.Extrusion_Cornering.Transition_Distance_Max;
      --  Only a quotient beyond every representable cap overflows here, leaving the cap.
      Widest := Length'Min (Widest, Extrusion_Deviation (Block.Params) / (Change * Transition_Deviation_Ratio));
      --  Constant_Speed_Axial_Ceiling keeps 0.999 of each limit. The small extra margin keeps roundoff in the root
      --  from lowering the ceiling the transition should leave alone.
      Speed :=
        Corner_Policy_Ceiling
          (Block,
           Motor_Map,
           Corner,
           Derivative_Bounds (Block.Corner_Transitions (Corner)),
           Velocity'Min (Block.Limited_Segment_Feedrates (Corner), Block.Limited_Segment_Feedrates (Corner + 1)))
        / (0.999 * mm / s)
        * (1.0 + 1.0E-9);
      if Speed <= 0.0 then
         --  A stopped corner needs no transition.
         return 0.0 * mm;
      end if;
      Fit
        (Transition_Derivative_Maxima (1),
         Dimensionless (Block.Params.Axial_Acceleration_Maxes (E_Axis) / (mm / s**2)),
         2);
      Fit (Transition_Derivative_Maxima (2), Dimensionless (Block.Params.Axial_Jerk_Maxes (E_Axis) / (mm / s**3)), 3);
      Fit (Transition_Derivative_Maxima (3), Dimensionless (Block.Params.Axial_Snap_Maxes (E_Axis) / (mm / s**4)), 4);
      Fit
        (Transition_Derivative_Maxima (4), Dimensionless (Block.Params.Axial_Crackle_Maxes (E_Axis) / (mm / s**5)), 5);
      return Length'Min (Result, Widest);
   exception
      when Constraint_Error =>
         --  Only an extreme speed or density overflows, and the widest transition slows the corner least.
         return Widest;
   end Extrusion_Transition_Reach;

   function Plan_Extrusion_Transition
     (Block : not null access constant Execution_Block; Corner : Corners_Index; Reach, Incoming, Outgoing : Length)
      return Extrusion_Transition
   is
      Change : constant Dimensionless := Block.Extrusion_Densities (Corner + 1) - Block.Extrusion_Densities (Corner);
      Half   : constant Length := Length'Min (Reach, Length'Min (Incoming, Outgoing));
   begin
      if Half <= 0.0 * mm then
         return (others => <>);
      end if;
      --  Later stages evaluate the analytical bounds without guards, so reject a transition whose bounds overflow.
      if abs Change * Transition_Derivative_Maxima (4) / (2.0 * Half)**4 >= Curvature_To_4'Last then
         return (others => <>);
      end if;
      return (Half_Width => Half, Density_Change => Change);
   exception
      when Constraint_Error =>
         return (others => <>);
   end Plan_Extrusion_Transition;

   procedure Assign_Extrusion_Transitions
     (Block : aliased in out Execution_Block; Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map)
   is
      function Room (Segment : Finishing_Corners_Index; Corner, Far_Corner : Corners_Index) return Length;
      --  The part of Segment that Corner's transition may take, given what Far_Corner, its other end, keeps.

      function Room (Segment : Finishing_Corners_Index; Corner, Far_Corner : Corners_Index) return Length is
         Total : constant Length := Segment_Total_Distance (Block'Access, Segment);
      begin
         if Far_Corner = Block.Corners'First or else Far_Corner = Block.Corners'Last then
            return 0.5 * Total;
         elsif Far_Corner < Corner then
            --  Already final.
            return Total - Block.Extrusion_Transitions (Far_Corner).Half_Width;
         else
            --  Still holds its reach.
            return Total - Length'Min (Block.Extrusion_Transitions (Far_Corner).Half_Width, 0.5 * Total);
         end if;
      end Room;
   begin
      Block.Extrusion_Transitions := [others => <>];
      if Extrusion_Deviation (Block.Params) = 0.0 * mm then
         return;
      end if;
      for Corner in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
         Block.Extrusion_Transitions (Corner).Half_Width :=
           Extrusion_Transition_Reach (Block'Access, Motor_Map, Corner);
      end loop;
      for Corner in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
         Block.Extrusion_Transitions (Corner) :=
           Plan_Extrusion_Transition
             (Block'Access,
              Corner,
              Block.Extrusion_Transitions (Corner).Half_Width,
              Room (Corner, Corner, Corner - 1),
              Room (Corner + 1, Corner, Corner + 1));
      end loop;
   end Assign_Extrusion_Transitions;

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

   function Extrusion_At_Distance
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index; Distance : Length)
      return Length
   is
      Total  : constant Length := Segment_Total_Distance (Block, Corner);
      D      : constant Length := Length'Max (0.0 * mm, Length'Min (Distance, Total));
      Start  : constant Extrusion_Transition := Block.Extrusion_Transitions (Corner - 1);
      Finish : constant Extrusion_Transition := Block.Extrusion_Transitions (Corner);
      Result : Length := Extrusion_Reference_At_Distance (Block, Corner, D);
   begin
      --  The two transitions never overlap, and a zero Half_Width never applies.
      if D < Start.Half_Width then
         Result := @ + Transition_Offset (Start, Start.Half_Width - D);
      end if;
      if D > Total - Finish.Half_Width then
         Result := @ + Transition_Offset (Finish, D - (Total - Finish.Half_Width));
      end if;
      return Result;
   end Extrusion_At_Distance;

   function Extrusion_Bounds
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index; Window : Profile_Window)
      return Unit_Speed_Axial_Derivative_Bounds
   is
      Total   : constant Length := Segment_Total_Distance (Block, Corner);
      Density : constant Dimensionless := Block.Extrusion_Densities (Corner);
      Low     : constant Length := Length'Max (0.0 * mm, Length'Min (Window.Start_Distance, Total));
      High    : constant Length := Length'Max (Low, Length'Min (Window.Start_Distance + Window.Distance, Total));
      Point   : constant Boolean := Window.Distance <= 0.0 * mm;
      Start   : constant Extrusion_Transition := Block.Extrusion_Transitions (Corner - 1);
      Finish  : constant Extrusion_Transition := Block.Extrusion_Transitions (Corner);
      Result  : Unit_Speed_Axial_Derivative_Bounds :=
        (Velocity => [E_Axis => abs Density, others => 0.0], others => <>);

      procedure Include (Transition : Extrusion_Transition; Near, Far : Length; Side : Dimensionless);
      --  Merge Transition's bounds over the part of Window from Near to Far, measured from the transition's edge on
      --  this segment toward its corner. Side is -1 before the segment's start corner and +1 after its end corner.

      procedure Include (Transition : Extrusion_Transition; Near, Far : Length; Side : Dimensionless) is
         Width  : constant Length := 2.0 * Transition.Half_Width;
         Change : constant Dimensionless := abs Transition.Density_Change;
      begin
         --  The density is monotone across a transition, so the ends of the overlap bound it.
         Result.Velocity (E_Axis) :=
           Dimensionless'Max
             (@,
              Dimensionless'Max
                (abs (Density + Side * Transition.Density_Change * Transition_Shape (Near / Width)),
                 abs (Density + Side * Transition.Density_Change * Transition_Shape (Far / Width))));
         Result.Acceleration (E_Axis) := Curvature'Max (@, Change * Transition_Derivative_Maxima (1) / Width);
         Result.Jerk (E_Axis) := Curvature_To_2'Max (@, Change * Transition_Derivative_Maxima (2) / Width**2);
         Result.Snap (E_Axis) := Curvature_To_3'Max (@, Change * Transition_Derivative_Maxima (3) / Width**3);
         Result.Crackle (E_Axis) := Curvature_To_4'Max (@, Change * Transition_Derivative_Maxima (4) / Width**4);
      end Include;
   begin
      if Start.Half_Width > 0.0 * mm and then (Low < Start.Half_Width or else (Point and then Low = Start.Half_Width))
      then
         Include (Start, Start.Half_Width - Length'Min (High, Start.Half_Width), Start.Half_Width - Low, -1.0);
      end if;
      if Finish.Half_Width > 0.0 * mm
        and then (High > Total - Finish.Half_Width or else (Point and then High = Total - Finish.Half_Width))
      then
         Include
           (Finish,
            Length'Max (Low, Total - Finish.Half_Width) - (Total - Finish.Half_Width),
            High - (Total - Finish.Half_Width),
            1.0);
      end if;
      return Result;
   end Extrusion_Bounds;

   function XYZE_Spatial_Scale (Density : Dimensionless) return Dimensionless is
      Scale : constant Dimensionless := Dimensionless'Max (1.0, abs Density);
   begin
      --  Scale the norm before squaring so E-dominated moves cannot overflow this calculation.
      return (1.0 / Scale) / Dimensionless_Math.Sqrt ((1.0 / Scale)**2 + (Density / Scale)**2);
   end XYZE_Spatial_Scale;

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
      if D = 0.0 * mm then
         --  Reconstructing a helix start from its phase can leave a rounding residual. A primitive rebuilt from its
         --  retained start, as at an inter-block seam, must keep a full turn's endpoints exactly coincident.
         return Block.Corners (Finishing_Corner - 1);
      end if;

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
   is (Derived_Primitive_Direction_At_Distance (Spatial_Primitive (Block, Finishing_Corner), Distance));

   function Derived_Primitive_Direction_At_Distance
     (Primitive : Derived_Path_Primitive; Distance : Length) return Position_Scale is
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
   end Derived_Primitive_Direction_At_Distance;

   function Primitive_Derivative_Jets_At_Distance
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index; Distance : Length)
      return Endpoint_Tangent_Jet
   is (Derived_Primitive_Derivative_Jets_At_Distance (Spatial_Primitive (Block, Finishing_Corner), Distance));

   function Derived_Primitive_Derivative_Jets_At_Distance
     (Primitive : Derived_Path_Primitive; Distance : Length) return Endpoint_Tangent_Jet
   is
      Result : Endpoint_Tangent_Jet :=
        (Tangent              => [others => 0.0],
         Tangent_Derivative_1 => [others => 0.0 / mm],
         Tangent_Derivative_2 => [others => 0.0 / mm**2],
         Tangent_Derivative_3 => [others => 0.0 / mm**3]);
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
               Result.Tangent := Derived_Primitive_Direction_At_Distance (Primitive, Distance);

               Result.Tangent_Derivative_1 (X_Axis) := -C * R_Over_L2;
               Result.Tangent_Derivative_1 (Y_Axis) := -S_Phase * R_Over_L2;

               Result.Tangent_Derivative_2 (X_Axis) := Sign * S_Phase * R_Over_L3;
               Result.Tangent_Derivative_2 (Y_Axis) := -Sign * C * R_Over_L3;

               Result.Tangent_Derivative_3 (X_Axis) := C * R_Over_L4;
               Result.Tangent_Derivative_3 (Y_Axis) := S_Phase * R_Over_L4;
            end;
      end case;

      return Result;
   end Derived_Primitive_Derivative_Jets_At_Distance;

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
         Jerk         => [others => 0.0 / mm**2],
         Snap         => [others => 0.0 / mm**3],
         Crackle      => [others => 0.0 / mm**4]);
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

   function Timing_Run_First
     (Block : not null access constant Execution_Block; Segment : Finishing_Corners_Index)
      return Finishing_Corners_Index
   is (Segment - Block.Timing_Run_Back (Segment));

   function Timing_Run_Last
     (Block : not null access constant Execution_Block; Segment : Finishing_Corners_Index)
      return Finishing_Corners_Index
   is (Segment + Block.Timing_Run_Ahead (Segment));

   function Is_Timing_Junction (Block : Execution_Block; Corner : Corners_Index) return Boolean
   is (Corner = Corners_Index'First or else Corner >= Block.N_Corners or else Block.Timing_Run_Ahead (Corner) = 0);

   function Joins_Timing_Run
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Boolean
   is
      Incoming : constant Velocity := Block.Limited_Segment_Feedrates (Corner);
      Outgoing : constant Velocity := Block.Limited_Segment_Feedrates (Corner + 1);
      Slower   : constant Velocity := Velocity'Min (Incoming, Outgoing);
   begin
      --  Only exact line passthroughs join. A change of ceiling or a binding E velocity jump keeps its junction:
      --  the speed has a plateau there in the continuous design whenever it binds.
      return
        Transition_Kind (Block.Corner_Transitions (Corner)) = Passthrough_Transition
        and then Arc_Length (Block.Corner_Transitions (Corner)) = 0.0 * mm
        and then Block.Primitives (Corner).Kind = Line_Primitive_Kind
        and then Block.Primitives (Corner + 1).Kind = Line_Primitive_Kind
        and then Block.Corner_Dwell_Times (Corner) = 0.0 * s
        and then Block.Primitive_Distances (Corner) > 0.0 * mm
        and then Block.Primitive_Distances (Corner + 1) > 0.0 * mm
        and then Slower > 0.0 * mm / s
        and then abs (Incoming - Outgoing) <= Timing_Run_Feedrate_Tolerance * Velocity'Max (Incoming, Outgoing)
        and then Extrusion_Junction_Ceiling (Block, Corner, Slower) = Slower;
   end Joins_Timing_Run;

   function Can_Merge_Timing_Runs
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Boolean
   is
      Transition : constant Corner_Transition_Evaluator := Block.Corner_Transitions (Corner);
      Incoming   : constant Velocity := Block.Limited_Segment_Feedrates (Corner);
      Outgoing   : constant Velocity := Block.Limited_Segment_Feedrates (Corner + 1);
   begin
      --  Feedrates may differ: each cell keeps its own segment's feedrate as a ceiling, and a merge that would need
      --  a valley at a slower segment is not faster, so the local search rejects it.
      return
        (Policy (Transition) = Passthrough
         or else (Policy (Transition) = Derivative_Bounded and then Arc_Length (Transition) > 0.0 * mm))
        and then Block.Corner_Dwell_Times (Corner) = 0.0 * s
        and then Segment_Total_Distance (Block, Corner) > 0.0 * mm
        and then Segment_Total_Distance (Block, Corner + 1) > 0.0 * mm
        and then Velocity'Min (Incoming, Outgoing) > 0.0 * mm / s;
   end Can_Merge_Timing_Runs;

   procedure Assign_Timing_Runs (Block : aliased in out Execution_Block; Joins : Corner_Flags) is
      Start : Corners_Index'Base := Finishing_Corners_Index'First;
      --  First segment of the run being collected. It passes the last corner once the final run is assigned.
   begin
      Block.Timing_Run_Back := [others => 0];
      Block.Timing_Run_Ahead := [others => 0];
      if not Block.Params.Continuous_Timing or else Block.Is_Homing_Move or else Block.N_Corners < 3 then
         return;
      end if;

      for I in Finishing_Corners_Index'First .. Block.N_Corners loop
         if I = Block.N_Corners or else not Joins (I) then
            for J in Start .. I loop
               Block.Timing_Run_Back (J) := J - Start;
               Block.Timing_Run_Ahead (J) := I - J;
            end loop;
            Start := I + 1;
         end if;
      end loop;
   end Assign_Timing_Runs;

   procedure Assign_Timing_Runs (Block : aliased in out Execution_Block; Forced_Junction : Corners_Index'Base) is
      Joins : Corner_Flags (Block.Corners'Range);
   begin
      if Block.Params.Continuous_Timing then
         for I in Finishing_Corners_Index'First .. Block.N_Corners - 1 loop
            Joins (I) := I /= Forced_Junction and then Joins_Timing_Run (Block'Access, I);
         end loop;
      end if;
      Assign_Timing_Runs (Block, Joins);
   end Assign_Timing_Runs;

   function Is_Cellular_Run
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Boolean
   is (Block.Timing_Run_Ahead (Run_First) /= 0);

   function Timing_Run_Cell_Count
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Natural
   is (Natural (6 * (Block.Timing_Run_Ahead (Run_First) + 1)));

   type Segment_Cut is record
      Local    : Length;
      --  Distance along the segment, where its bounds are evaluated.
      Position : Length;
      --  Run distance of the same point.
   end record;

   type Segment_Cuts is array (1 .. 6) of Segment_Cut;

   function Timing_Segment_Cuts
     (Block     : not null access constant Execution_Block;
      Run_First : Finishing_Corners_Index;
      Segment   : Finishing_Corners_Index;
      Offset    : Length) return Segment_Cuts;
   --  The ends of Segment's geometric pieces, also cut at the edges of its E density transitions, in increasing order,
   --  where Segment starts at run distance Offset in the run starting at Run_First. Without transitions the extra
   --  cuts coincide with the segment's ends. The run's windows start at cuts measured from its start and end at cuts
   --  measured back from its end, so the first segment places every cut from its start and later segments place cuts
   --  near their end back from it: a window ending at the run's end transition then never overlaps it through
   --  roundoff. Cuts at the same point share one position, so the pieces between them leave no gap.

   function Segment_Local_Distance (Cuts : Segment_Cuts; Position : Length) return Length;
   --  The segment distance at run distance Position. A position at a cut gives that cut's own distance, so a window
   --  that ends at a cut never reaches into the piece beyond it.

   function Timing_Segment_Cuts
     (Block     : not null access constant Execution_Block;
      Run_First : Finishing_Corners_Index;
      Segment   : Finishing_Corners_Index;
      Offset    : Length) return Segment_Cuts
   is
      Start_Transition : constant Length := Segment_Start_Transition_Distance (Block, Segment);
      End_Transition   : constant Length := Segment_End_Transition_Distance (Block, Segment);
      Total            : constant Length := Segment_Total_Distance (Block, Segment);
      Segment_End      : constant Length := Offset + Total;
      Start_Extrusion  : constant Length := Length'Min (Total, Block.Extrusion_Transitions (Segment - 1).Half_Width);
      End_Extrusion    : constant Length := Length'Min (Total, Block.Extrusion_Transitions (Segment).Half_Width);

      function Back (Local, Tail : Length) return Segment_Cut
      is (Local, (if Segment = Run_First then Offset + Local else Segment_End - Tail));
      --  The cut at Local, which is Tail before the segment's end.

      Result : Segment_Cuts :=
        [(0.0 * mm, Offset),
         (Start_Transition, Offset + Start_Transition),
         Back (Start_Transition + Block.Primitive_Distances (Segment), End_Transition),
         (Total, Segment_End),
         (Start_Extrusion, Offset + Start_Extrusion),
         Back (Total - End_Extrusion, End_Extrusion)];
   begin
      for J in Result'First + 1 .. Result'Last loop
         declare
            Next : constant Segment_Cut := Result (J);
            K    : Positive := J;
         begin
            while K > Result'First and then Result (K - 1).Local > Next.Local loop
               Result (K) := Result (K - 1);
               K := K - 1;
            end loop;
            Result (K) := Next;
         end;
      end loop;
      for J in Result'First + 1 .. Result'Last loop
         --  Positions measured from opposite ends may cross by roundoff where a piece is negligible.
         Result (J).Position := Length'Max (Result (J).Position, Result (J - 1).Position);
      end loop;
      for J in reverse Result'First .. Result'Last - 1 loop
         --  Cuts at one point take the later of their positions, so a window ending at any of them overlaps none of
         --  the pieces after them.
         if Result (J).Local = Result (J + 1).Local then
            Result (J).Position := Result (J + 1).Position;
         end if;
      end loop;
      return Result;
   end Timing_Segment_Cuts;

   function Segment_Local_Distance (Cuts : Segment_Cuts; Position : Length) return Length is
   begin
      for J in Cuts'Range loop
         if Position <= Cuts (J).Position then
            return
              (if J = Cuts'First or else Position = Cuts (J).Position
               then Cuts (J).Local
               else Length'Min (Cuts (J).Local, Cuts (J - 1).Local + (Position - Cuts (J - 1).Position)));
         end if;
      end loop;
      return Cuts (Cuts'Last).Local;
   end Segment_Local_Distance;

   function Timing_Run_Cells
     (Block     : not null access constant Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Run_First : Finishing_Corners_Index;
      Max_Vel   : Velocity) return Path_Cells
   is
      Result     : Path_Cells (1 .. Timing_Run_Cell_Count (Block, Run_First));
      Count      : Natural := 0;
      Last_Piece : Natural := 0;
      Offset     : Length := 0.0 * mm;

      procedure Add_Piece (Segment : Finishing_Corners_Index; First, Last : Segment_Cut);

      procedure Add_Piece (Segment : Finishing_Corners_Index; First, Last : Segment_Cut) is
         Local   : constant Profile_Window := (Start_Distance => First.Local, Distance => Last.Local - First.Local);
         Bounds  : Unit_Speed_Axial_Derivative_Bounds;
         Ceiling : Velocity;
      begin
         if Last.Local <= First.Local then
            return;
         end if;
         Bounds := Window_Axial_Derivative_Bounds (Block, Segment, Local);
         Ceiling :=
           Constant_Speed_Axial_Ceiling
             (Block.Params, Bounds, Velocity'Min (Max_Vel, Block.Limited_Segment_Feedrates (Segment)));
         Ceiling := Motor_Delta_Ceiling_For_Window (Block, Motor_Map, Segment, Local, Ceiling);
         Count := Count + 1;
         Result (Count) :=
           (Start   => First.Position,
            Stop    => Last.Position,
            Bounds  => Bounds,
            Ceiling => Ceiling,
            Curved  =>
              (for some Axis in Axis_Name =>
                 Bounds.Acceleration (Axis) /= 0.0 / mm
                 or else Bounds.Jerk (Axis) /= 0.0 / mm**2
                 or else Bounds.Snap (Axis) /= 0.0 / mm**3
                 or else Bounds.Crackle (Axis) /= 0.0 / mm**4));
         Last_Piece := Count;
      end Add_Piece;
   begin
      for I in Run_First .. Timing_Run_Last (Block, Run_First) loop
         declare
            Cuts : constant Segment_Cuts := Timing_Segment_Cuts (Block, Run_First, I, Offset);
         begin
            if I > Run_First then
               if Last_Piece > 0 then
                  --  The segment's start may share a cut placed back from its end, past the previous segment's end by
                  --  roundoff. The previous piece reaches it so that the run's cells leave no gap.
                  Result (Last_Piece).Stop := Cuts (Cuts'First).Position;
               end if;
               declare
                  E_Ceiling : constant Velocity := Extrusion_Junction_Ceiling (Block, I - 1, Max_Vel);
               begin
                  --  E's velocity jumps by the density change times the speed at the corner itself.
                  if E_Ceiling < Max_Vel then
                     Count := Count + 1;
                     Result (Count) :=
                       (Start => Offset, Stop => Offset, Bounds => <>, Ceiling => E_Ceiling, Curved => False);
                  end if;
               end;
            end if;
            for J in Cuts'First + 1 .. Cuts'Last loop
               Add_Piece (I, Cuts (J - 1), Cuts (J));
            end loop;
            --  Accumulate in the same order as Timing_Total_Distance so the run's end agrees exactly.
            Offset := Offset + Segment_Total_Distance (Block, I);
         end;
      end loop;
      return Result (1 .. Count);
   end Timing_Run_Cells;

   function Timing_Total_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length
   is
      Total : Length := 0.0 * mm;
   begin
      if Block.Timing_Run_Ahead (Run_First) = 0 then
         return Segment_Total_Distance (Block, Run_First);
      end if;
      --  Accumulate in the same order as Timing_Distance_Offsets so member boundaries agree exactly.
      for I in Run_First .. Timing_Run_Last (Block, Run_First) loop
         Total := Total + Segment_Total_Distance (Block, I);
      end loop;
      return Total;
   end Timing_Total_Distance;

   function Timing_Start_Transition_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length
   is (Segment_Start_Transition_Distance (Block, Run_First));

   function Timing_End_Transition_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length
   is (Segment_End_Transition_Distance (Block, Timing_Run_Last (Block, Run_First)));

   function Timing_Straight_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length
   is (if Block.Timing_Run_Ahead (Run_First) = 0
       then Block.Primitive_Distances (Run_First)
       else
         Timing_Total_Distance (Block, Run_First)
         - Timing_Start_Transition_Distance (Block, Run_First)
         - Timing_End_Transition_Distance (Block, Run_First));

   function Timing_Start_Curve_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length
   is
      Curve     : constant Length := Timing_Start_Transition_Distance (Block, Run_First);
      Extrusion : constant Length := Block.Extrusion_Transitions (Run_First - 1).Half_Width;
   begin
      return (if Extrusion <= Curve then Curve else Length'Min (Extrusion, Timing_Total_Distance (Block, Run_First)));
   end Timing_Start_Curve_Distance;

   function Timing_End_Curve_Distance
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Length
   is
      Curve     : constant Length := Timing_End_Transition_Distance (Block, Run_First);
      Extrusion : constant Length := Block.Extrusion_Transitions (Timing_Run_Last (Block, Run_First)).Half_Width;
   begin
      return
        (if Extrusion <= Curve
         then Curve
         else
           Length'Max
             (Curve,
              Length'Min
                (Extrusion,
                 Timing_Total_Distance (Block, Run_First) - Timing_Start_Curve_Distance (Block, Run_First))));
   end Timing_End_Curve_Distance;

   function Timing_Feedrate
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Velocity
   is
      Result : Velocity := Block.Limited_Segment_Feedrates (Run_First);
   begin
      for I in Run_First + 1 .. Timing_Run_Last (Block, Run_First) loop
         Result := Velocity'Max (Result, Block.Limited_Segment_Feedrates (I));
      end loop;
      return Result;
   end Timing_Feedrate;

   function Evaluate_Cellular_Window
     (Block     : not null access constant Execution_Block;
      Run_First : Finishing_Corners_Index;
      Window    : Profile_Window;
      Max_Vel   : Velocity;
      Cells     : Path_Cells) return Profile_Window_Evaluation;
   --  Evaluate_Timing_Window for a cellular run, given Timing_Run_Cells (Block, Motor_Map, Run_First, Max_Vel). The
   --  cells depend only on the run, so a caller evaluating several of its windows builds them once.

   function Evaluate_Timing_Window
     (Block     : not null access constant Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Run_First : Finishing_Corners_Index;
      Window    : Profile_Window;
      Max_Vel   : Velocity) return Profile_Window_Evaluation is
   begin
      if not Is_Cellular_Run (Block, Run_First) then
         return Evaluate_Profile_Window (Block, Motor_Map, Run_First, Window, Max_Vel);
      end if;
      return
        Evaluate_Cellular_Window
          (Block, Run_First, Window, Max_Vel, Timing_Run_Cells (Block, Motor_Map, Run_First, Max_Vel));
   end Evaluate_Timing_Window;

   function Evaluate_Cellular_Window
     (Block     : not null access constant Execution_Block;
      Run_First : Finishing_Corners_Index;
      Window    : Profile_Window;
      Max_Vel   : Velocity;
      Cells     : Path_Cells) return Profile_Window_Evaluation
   is
      Run_Last      : constant Finishing_Corners_Index := Timing_Run_Last (Block, Run_First);
      Window_End    : constant Length := Window.Start_Distance + Window.Distance;
      Bounds        : Unit_Speed_Axial_Derivative_Bounds :=
        (Velocity     => [others => 0.0],
         Acceleration => [others => 0.0 / mm],
         Jerk         => [others => 0.0 / mm**2],
         Snap         => [others => 0.0 / mm**3],
         Crackle      => [others => 0.0 / mm**4]);
      Mixed         : Mixed_Derivative_Limit_Result;
      Start_Ceiling : Velocity := Max_Vel;
      End_Ceiling   : Velocity := Max_Vel;
      Result        : Profile_Window_Evaluation;

      procedure For_Each_Member
        (Process : not null access procedure (Segment : Finishing_Corners_Index; Local : Profile_Window));
      --  Call Process for every member overlapped by Window, with Window expressed in that member's distance through
      --  the cuts of the run's cells, so a window ending at a cell's edge takes none of the piece beyond it.

      procedure Merge_Member (Segment : Finishing_Corners_Index; Local : Profile_Window);

      procedure For_Each_Member
        (Process : not null access procedure (Segment : Finishing_Corners_Index; Local : Profile_Window))
      is
         Offset : Length := 0.0 * mm;
      begin
         for I in Run_First .. Run_Last loop
            declare
               Member_Distance : constant Length := Segment_Total_Distance (Block, I);
               Cuts            : constant Segment_Cuts := Timing_Segment_Cuts (Block, Run_First, I, Offset);
               Low             : constant Length := Length'Max (Window.Start_Distance, Offset);
               High            : constant Length := Length'Min (Window_End, Offset + Member_Distance);
            begin
               if (if Window.Distance > 0.0 * mm then High > Low else High >= Low) then
                  Process
                    (I, Window_Between (Segment_Local_Distance (Cuts, Low), Segment_Local_Distance (Cuts, High)));
               end if;
               Offset := Offset + Member_Distance;
            end;
         end loop;
      end For_Each_Member;

      procedure Merge_Member (Segment : Finishing_Corners_Index; Local : Profile_Window) is
         Member : constant Unit_Speed_Axial_Derivative_Bounds :=
           Window_Axial_Derivative_Bounds (Block, Segment, Local);
      begin
         for A in Axis_Name loop
            Bounds.Velocity (A) := Dimensionless'Max (Bounds.Velocity (A), Member.Velocity (A));
            Bounds.Acceleration (A) := Curvature'Max (Bounds.Acceleration (A), Member.Acceleration (A));
            Bounds.Jerk (A) := Curvature_To_2'Max (Bounds.Jerk (A), Member.Jerk (A));
            Bounds.Snap (A) := Curvature_To_3'Max (Bounds.Snap (A), Member.Snap (A));
            Bounds.Crackle (A) := Curvature_To_4'Max (Bounds.Crackle (A), Member.Crackle (A));
         end loop;
      end Merge_Member;

   begin
      --  Merging every member's bounds over the part of the window it covers gives the tangent-only envelope, and the
      --  merged certificate that the conservative retry uses. Ordinary profiles are certified against the run's
      --  cells. Each endpoint speed is admitted by the window's cells at that end only, so a low ceiling at one end
      --  does not cap the other; as for one segment, the junction limits admit the coasts outside the window.
      For_Each_Member (Merge_Member'Access);
      Mixed := Mixed_Derivative_Limits (Block.Params, (Velocity => Bounds.Velocity, others => <>), Max_Vel);
      for Cell of Cells loop
         if Cell_Overlaps (Cell, Window.Start_Distance, Window_End) then
            if Cell.Start <= Window.Start_Distance then
               Start_Ceiling := Velocity'Min (Start_Ceiling, Cell.Ceiling);
            end if;
            if Cell.Stop >= Window_End then
               End_Ceiling := Velocity'Min (End_Ceiling, Cell.Ceiling);
            end if;
         end if;
      end loop;
      Result :=
        (Valid         => Mixed.Valid,
         Window        => Window,
         Limits        => Mixed.Limits,
         Max_Vel       => Velocity'Min (Start_Ceiling, End_Ceiling),
         Start_Max_Vel => Start_Ceiling,
         End_Max_Vel   => End_Ceiling,
         Bounds        => Bounds,
         Curved        =>
           (for some Axis in Axis_Name =>
              Bounds.Acceleration (Axis) /= 0.0 / mm
              or else Bounds.Jerk (Axis) /= 0.0 / mm**2
              or else Bounds.Snap (Axis) /= 0.0 / mm**3
              or else Bounds.Crackle (Axis) /= 0.0 / mm**4),
         Cellular      => True);
      if Window.Start_Distance < 0.0 * mm
        or else Window.Distance < 0.0 * mm
        or else Window_End > Timing_Total_Distance (Block, Run_First)
      then
         Result.Valid := False;
      end if;
      return Result;
   end Evaluate_Cellular_Window;

   function Window_Between (First, Last : Length) return Profile_Window is
      Dist : Length := Last - First;
   begin
      if Dist > 0.0 * mm and then First + Dist > Last then
         Dist := Length'Adjacent (Dist, 0.0 * mm);
      end if;
      return (Start_Distance => First, Distance => Dist);
   end Window_Between;

   function Timing_Split_Windows
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index)
      return Split_Profile_Windows
   is
      Total       : constant Length := Timing_Total_Distance (Block, Run_First);
      Start_Curve : constant Length := Timing_Start_Curve_Distance (Block, Run_First);
      End_Curve   : constant Length := Timing_End_Curve_Distance (Block, Run_First);
      Middle      : constant Length :=
        (if Is_Cellular_Run (Block, Run_First)
         then
           --  The run's cells are cut back from its end, so the middle must not reach past that cut by roundoff. An
           --  end curve that reaches back to the start curve may put that cut an ulp before the start curve's end.
           Length'Max (0.0 * mm, Window_Between (Start_Curve, Total - End_Curve).Distance)
         elsif Start_Curve = Timing_Start_Transition_Distance (Block, Run_First)
           and then End_Curve = Timing_End_Transition_Distance (Block, Run_First)
         then Timing_Straight_Distance (Block, Run_First)
         else Total - Start_Curve - End_Curve);
   begin
      return
        [(Start_Distance => 0.0 * mm, Distance => Start_Curve),
         (Start_Distance => Start_Curve, Distance => Middle),
         (Start_Distance => Start_Curve + Middle, Distance => Total - (Start_Curve + Middle))];
   end Timing_Split_Windows;

   function Segment_Profile_Window_Candidates
     (Block : not null access constant Execution_Block; Finishing_Corner : Finishing_Corners_Index)
      return Profile_Window_Candidates
   is
      Start_Transition : constant Length := Timing_Start_Transition_Distance (Block, Finishing_Corner);
      End_Transition   : constant Length := Timing_End_Transition_Distance (Block, Finishing_Corner);
      Total            : constant Length := Timing_Total_Distance (Block, Finishing_Corner);
      Result           : Profile_Window_Candidates;
      Index            : Profile_Window_Candidate_Index := 1;
      procedure Add (First, Tail : Length);

      procedure Add (First, Tail : Length) is
      begin
         --  Rounding must not make a profile overlap the transition that the selected cut excludes.
         Result (Index) := Window_Between (First, Total - Tail);
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

   function Evaluate_Run_Windows
     (Block     : not null access constant Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Run_First : Finishing_Corners_Index) return Profile_Window_Evaluation_Set
   is
      Windows  : constant Profile_Window_Candidates := Segment_Profile_Window_Candidates (Block, Run_First);
      Feedrate : constant Velocity := Timing_Feedrate (Block, Run_First);
   begin
      return Result : Profile_Window_Evaluation_Set do
         declare
            Cells : constant Path_Cells :=
              (if Is_Cellular_Run (Block, Run_First)
               then Timing_Run_Cells (Block, Motor_Map, Run_First, Feedrate)
               else [1 .. 0 => <>]);
         begin
            for W in Profile_Window_Candidate_Index loop
               if (for all Previous in Profile_Window_Candidate_Index'First .. W - 1 =>
                     Windows (Previous) /= Windows (W))
               then
                  Result (W) :=
                    (if Is_Cellular_Run (Block, Run_First)
                     then Evaluate_Cellular_Window (Block, Run_First, Windows (W), Feedrate, Cells)
                     else Evaluate_Profile_Window (Block, Motor_Map, Run_First, Windows (W), Feedrate));
               end if;
            end loop;
         end;
      end return;
   end Evaluate_Run_Windows;

   function Evaluate_Profile_Window
     (Block            : not null access constant Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Finishing_Corner : Finishing_Corners_Index;
      Window           : Profile_Window;
      Max_Vel          : Velocity) return Profile_Window_Evaluation
   is
      Bounds  : constant Unit_Speed_Axial_Derivative_Bounds :=
        Window_Axial_Derivative_Bounds (Block, Finishing_Corner, Window);
      Mixed   : constant Mixed_Derivative_Limit_Result :=
        Mixed_Derivative_Limits (Block.Params, (Velocity => Bounds.Velocity, others => <>), Max_Vel);
      Ceiling : constant Velocity :=
        Motor_Delta_Ceiling_For_Window
          (Block, Motor_Map, Finishing_Corner, Window, Constant_Speed_Axial_Ceiling (Block.Params, Bounds, Max_Vel));
      Result  : Profile_Window_Evaluation;
   begin
      Result :=
        (Valid         => Mixed.Valid,
         Window        => Window,
         Limits        => Mixed.Limits,
         Max_Vel       => Ceiling,
         Start_Max_Vel => Ceiling,
         End_Max_Vel   => Ceiling,
         Bounds        => Bounds,
         Curved        =>
           (for some Axis in Axis_Name =>
              Bounds.Acceleration (Axis) /= 0.0 / mm
              or else Bounds.Jerk (Axis) /= 0.0 / mm**2
              or else Bounds.Snap (Axis) /= 0.0 / mm**3
              or else Bounds.Crackle (Axis) /= 0.0 / mm**4),
         Cellular      => False);
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
         Jerk         => [others => 0.0 / mm**2],
         Snap         => [others => 0.0 / mm**3],
         Crackle      => [others => 0.0 / mm**4]);

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
         return Extrusion_Bounds (Block, Finishing_Corner, Window);
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
         E_Bounds : constant Unit_Speed_Axial_Derivative_Bounds := Extrusion_Bounds (Block, Finishing_Corner, Window);
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
      First_Axis      : Axis_Name := Axis_Name'First;
      Have_First      : Boolean := False;
      Single_Response : Boolean := True;
   begin
      --  The result is Raw_Bound unless the contributing axes fall into more than one response group, which is
      --  rare and cheap to rule out: they all share the response of the first of them.
      for Axis in Axis_Name loop
         if Coefficients (Axis) /= 0.0 / mm
           and then Params.Axial_Shapers (Axis).Kind /= Prunt.Input_Shapers.Pressure_Advance
         then
            if not Have_First then
               Have_First := True;
               First_Axis := Axis;
            elsif Params.Axial_Shapers (Axis) /= Params.Axial_Shapers (First_Axis) then
               Single_Response := False;
            end if;
         end if;
      end loop;
      if Single_Response then
         return Raw_Bound;
      end if;

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

      type Motor_Projections is array (Motor_Name) of Curvature;
      Spatial_Projections : Motor_Projections;
      Projections_Known   : Boolean := False;
      --  Each motor's projection bound for an arbitrary spatial tangent, as Motor_Delta_Ceiling_For_Projection forms
      --  it. It depends only on the parameters and the map, so the E ceiling and both transitions share one copy.

      function Spatial_Projection (M : Motor_Name) return Curvature;
      procedure Apply_Transition_Ceiling;
      --  Result := Motor_Delta_Ceiling_For_Projection (Block.Params, Geometry_Map, Result).

      function Spatial_Projection (M : Motor_Name) return Curvature is
      begin
         if not Projections_Known then
            for Motor in Motor_Name loop
               declare
                  Coefficients : constant Projection_Coefficients :=
                    Motor_Projection_Coefficients (Geometry_Map, Motor);
               begin
                  Spatial_Projections (Motor) :=
                    Shaper_Aware_Projection_Bound (Block.Params, Coefficients, Scaled_Curvature_Norm (Coefficients));
               end;
            end loop;
            Projections_Known := True;
         end if;
         return Spatial_Projections (M);
      end Spatial_Projection;

      procedure Apply_Transition_Ceiling is
      begin
         for M in Motor_Name loop
            if Spatial_Projection (M) > 0.0 / mm then
               Result :=
                 Velocity'Min
                   (Result,
                    Motor_Delta_Numerical_Safety_Factor
                    * Maximum_Deltas_Per_Command (M)
                    / (Interpolation_Time * Spatial_Projection (M)));
            end if;
         end loop;
         Result := Velocity'Max (0.0 * mm / s, Result);
      end Apply_Transition_Ceiling;
   begin
      declare
         E_Bound : constant Dimensionless := Extrusion_Bounds (Block, Finishing_Corner, Window).Velocity (E_Axis);
      begin
         if not Block.Params.Ignore_E_In_XYZE and then E_Bound > abs Block.Extrusion_Densities (Finishing_Corner) then
            --  The segment's feedrate converted the XYZE cap at its own density, which a transition can exceed.
            Result := Velocity'Min (Result, Block.Params.Tangential_Velocity_Max * XYZE_Spatial_Scale (E_Bound));
         end if;
         for M in Motor_Name loop
            if E_Bound > 0.0 and then Motor_Map (E_Axis, M) /= 0.0 / mm then
               declare
                  Projection : constant Curvature := Spatial_Projection (M) + abs Motor_Map (E_Axis, M) * E_Bound;
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
            end if;
         end loop;
      end;

      if Window.Distance <= 0.0 * mm then
         return Result;
      end if;

      if Window_Start < Start_Transition and then Window_End > 0.0 * mm then
         Apply_Transition_Ceiling;
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
         Apply_Transition_Ceiling;
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
     (Start_Vel          : Velocity;
      Max_Vel            : Velocity;
      Distance           : Length;
      Limits             : Scalar_Derivative_Limits;
      Relative_Tolerance : Dimensionless) return Velocity
   is
      Lower  : constant Velocity := Velocity'Min (Start_Vel, Max_Vel);
      Solver : Delta_V_Solver;

      function Ramp_Distance (End_Vel : Velocity) return Length;
      --  Endpoint_Delta_V_Distance from Start_Vel under Limits, with the limit-only work prepared once.

      function Ramp_Distance (End_Vel : Velocity) return Length is
      begin
         return
           Fast_Distance_At_Max_Time
             (Optimal_Profile_For_Delta_V (Solver, End_Vel - Start_Vel),
              (if Start_Vel < End_Vel then Limits.Crackle_Max else -Limits.Crackle_Max),
              Start_Vel);
      exception
         when Constraint_Error =>
            return Length'Last;
      end Ramp_Distance;

      function Excess (End_Vel : Dimensionless) return Dimensionless
      is ((Ramp_Distance (End_Vel * (mm / s)) - Distance) / mm);

      function Highest_End is new Largest_Nonpositive (Excess);

      Upper_Distance : Length;
      Gain           : Velocity;
   begin
      if Distance <= 0.0 * mm or else Max_Vel <= Start_Vel then
         return Lower;
      end if;

      Solver := Prepare_Delta_V (Limits.Acceleration_Max, Limits.Jerk_Max, Limits.Snap_Max, Limits.Crackle_Max);
      Upper_Distance := Ramp_Distance (Max_Vel);
      if Upper_Distance <= Distance then
         return Max_Vel;
      end if;

      --  The ramp's distance grows with the speed it reaches, so the search finds the highest speed whose ramp
      --  fits, to the last machine number unless a tolerance stops it earlier. A ramp to Start_Vel is empty. The
      --  speed gained by the optimal ramp over Distance is that speed up to rounding, so the search normally only
      --  has to settle the last few machine numbers around it.
      Gain := Delta_V_Over_Distance (Solver, Start_Vel, Distance);
      return
        Velocity'Max
          (Start_Vel,
           Reach_Margin
           * (Highest_End
                (Lower              => Start_Vel / (mm / s),
                 Upper              => Max_Vel / (mm / s),
                 Lower_Residual     => -Distance / mm,
                 Upper_Residual     => (Upper_Distance - Distance) / mm,
                 Relative_Tolerance => Relative_Tolerance,
                 Guess              => (if Gain >= 0.0 * mm / s then (Start_Vel + Gain) / (mm / s) else -1.0),
                 Use_Model          => Gain < 0.0 * mm / s)
              * (mm / s)));
   end Reachable_Velocity;

   function Profile_At_Peak
     (Params : Kinematic_Parameters; Eval : Profile_Window_Evaluation; Start_Vel, End_Vel, Peak : Velocity)
      return Window_Profile_Result
   is
      Cache : Window_Search_Cache;
   begin
      return Profile_At_Peak (Params, Eval, Start_Vel, End_Vel, Peak, Cache);
   end Profile_At_Peak;

   procedure Certify_Profile_At_Peak
     (Params                   : Kinematic_Parameters;
      Eval                     : Profile_Window_Evaluation;
      Start_Vel, End_Vel, Peak : Velocity;
      Cache                    : in out Window_Search_Cache;
      Result                   : out Window_Profile_Result;
      Overrun                  : out Length);
   --  Profile_At_Peak, also reporting how far the certified ramps overrun the window. Overrun is nonpositive
   --  exactly when Result is valid, and then the coast covers its magnitude. For a candidate rejected only because
   --  its ramps are too long it is that positive excess, which a search over peak speeds can interpolate; for one
   --  rejected for any other reason it is Length'Last.

   procedure Certify_Profile_At_Peak
     (Params                   : Kinematic_Parameters;
      Eval                     : Profile_Window_Evaluation;
      Start_Vel, End_Vel, Peak : Velocity;
      Cache                    : in out Window_Search_Cache;
      Result                   : out Window_Profile_Result;
      Overrun                  : out Length)
   is
      Peaks                          : Scalar_Derivative_Limits :=
        (0.0 * mm / s**2, 0.0 * mm / s**3, 0.0 * mm / s**4, 0.0 * mm / s**5);
      Scale                          : Dimensionless := 1.0;
      Distance                       : Length;
      Accel_Distance, Decel_Distance : Length;
      Actual_Peak                    : Velocity;
      type Ramps is array (1 .. 2) of Feedrate_Profile_Times;
   begin
      Result := (others => <>);
      Overrun := Length'Last;
      if not Eval.Valid or else Peak < Velocity'Max (Start_Vel, End_Vel) or else Peak > Eval.Max_Vel then
         return;
      elsif Eval.Window.Distance <= 0.0 * mm then
         Result.Valid := Eval.Window.Distance = 0.0 * mm and then Start_Vel = End_Vel;
         if Result.Valid then
            Overrun := 0.0 * mm;
         end if;
         return;
      elsif Peak <= 0.0 * mm / s then
         return;
      end if;
      Result.Max_Crackle := Eval.Limits.Crackle_Max;
      if not Cache.Ramp_Solver_Filled then
         Cache.Ramp_Solver :=
           Prepare_Delta_V
             (Eval.Limits.Acceleration_Max, Eval.Limits.Jerk_Max, Eval.Limits.Snap_Max, Eval.Limits.Crackle_Max);
         Cache.Ramp_Solver_Filled := True;
      end if;
      Result.Profile.Accel := Optimal_Profile_For_Delta_V (Cache.Ramp_Solver, Peak - Start_Vel);
      --  Equal endpoint velocities request exactly the same ramp, before any curvature time scaling.
      Result.Profile.Decel :=
        (if Start_Vel = End_Vel
         then Result.Profile.Accel
         else Optimal_Profile_For_Delta_V (Cache.Ramp_Solver, Peak - End_Vel));
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
         Scale := Mixed_Derivative_Time_Scale (Params, Eval.Bounds, Peak, Peaks, Cache.Mixed);
         if Scale <= 0.0 then
            return;
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
      if (Peak > Start_Vel or else Peak > End_Vel) and then Result.Max_Crackle < Dimensionless'Model_Small * mm / s**5
      then
         --  Underflow or subnormal rounding would invalidate both the preserved velocity change and its
         --  derivative certificate. Do not turn an unrepresentable ramp into an apparently feasible coast.
         return;
      end if;
      Result.Decel_Crackle := Result.Max_Crackle;
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
         if Result.Valid then
            Overrun := Distance - Eval.Window.Distance;
         end if;
      elsif Distance > Eval.Window.Distance then
         Overrun := Distance - Eval.Window.Distance;
      end if;
   exception
      when Constraint_Error =>
         Result := (others => <>);
         Overrun := Length'Last;
   end Certify_Profile_At_Peak;

   function Profile_At_Peak
     (Params                   : Kinematic_Parameters;
      Eval                     : Profile_Window_Evaluation;
      Start_Vel, End_Vel, Peak : Velocity;
      Cache                    : in out Window_Search_Cache) return Window_Profile_Result
   is
      Result  : Window_Profile_Result;
      Overrun : Length;
   begin
      Certify_Profile_At_Peak (Params, Eval, Start_Vel, End_Vel, Peak, Cache, Result, Overrun);
      return Result;
   end Profile_At_Peak;

   function Memo_Answers (Memo : Reachable_Velocity_Memo; Fixed_Vel, Cap : Velocity) return Boolean
   is (Memo.Valid
       and then Identical (Memo.Fixed_Vel, Fixed_Vel)
       and then (Cap <= Memo.Cap or else Memo.Reachable < Memo.Cap));

   function Reachable_Window_Velocity
     (Params    : Kinematic_Parameters;
      Eval      : Profile_Window_Evaluation;
      Start_Vel : Velocity;
      Floor     : Velocity := 0.0 * mm / s;
      Cap       : Velocity := Velocity'Last) return Velocity
   is
      Ceiling : constant Velocity := Velocity'Min (Eval.Max_Vel, Raised_Reach_Cap (Cap));
      --  A reach found at Raised_Reach_Cap (Cap) is one which the unrestricted search would have reported as at
      --  least Cap after its margin, so looking no higher changes nothing that the caller uses.
      Upper   : Velocity;
      Cache   : Window_Search_Cache;

      function Overrun (End_Vel : Velocity) return Length;
      --  How far the certified ramp from Start_Vel to End_Vel overruns the window.

      function Overrun (End_Vel : Velocity) return Length is
         Candidate : Window_Profile_Result;
         Result    : Length;
      begin
         Certify_Profile_At_Peak (Params, Eval, Start_Vel, End_Vel, End_Vel, Cache, Candidate, Result);
         return Result;
      end Overrun;

      function Raw_Overrun (End_Vel : Dimensionless) return Dimensionless
      is (Overrun (End_Vel * (mm / s)) / mm);

      function Highest_End is new Largest_Nonpositive (Raw_Overrun);

      Lower         : Velocity := Velocity'Min (Start_Vel, Ceiling);
      Lower_Overrun : Length := -Eval.Window.Distance;
      Upper_Overrun : Length;
   begin
      if Ceiling <= Floor and then Floor > 0.0 * mm / s then
         --  Nothing in this window is faster than what the caller already has.
         return Ceiling;
      elsif not Eval.Curved or else Eval.Window.Distance <= 0.0 * mm or else Ceiling <= Start_Vel then
         return Reachable_Velocity (Start_Vel, Ceiling, Eval.Window.Distance, Eval.Limits, Reach_Search_Tolerance);
      end if;
      --  The tangent-only solve is a cheap upper bound and avoids searching an unnecessarily large speed range.
      Upper := Reachable_Velocity (Start_Vel, Ceiling, Eval.Window.Distance, Eval.Limits, Reach_Search_Tolerance);
      if Upper <= Floor then
         return Upper;
      end if;
      if Floor > Lower then
         --  Only a reach which still exceeds Floor after its margin is of use. The certified ramp grows with the
         --  speed it reaches, so one evaluation at the lowest such speed decides whether this window has one.
         declare
            Useful : constant Velocity := Velocity'Min (Upper, Raised_Reach_Cap (Floor));
         begin
            if Useful < Upper then
               Lower_Overrun := Overrun (Useful);
               if Lower_Overrun > 0.0 * mm then
                  return Floor;
               end if;
               Lower := Useful;
            end if;
         end;
      end if;
      Upper_Overrun := Overrun (Upper);
      if Upper_Overrun <= 0.0 * mm then
         return Upper;
      elsif Reach_Margin * Upper <= Floor then
         return Floor;
      end if;
      --  The certified ramp grows with the speed it reaches, more steeply than the tangent-only one as curvature
      --  takes more of the derivative budgets. A ramp to Start_Vel itself is empty, leaving the whole window.
      return
        Velocity'Max
          (Velocity'Max (Start_Vel, Floor),
           Reach_Margin
           * (Highest_End
                (Lower              => Lower / (mm / s),
                 Upper              => Upper / (mm / s),
                 Lower_Residual     => Lower_Overrun / mm,
                 Upper_Residual     => Upper_Overrun / mm,
                 Relative_Tolerance => 1.0E-5)
              * (mm / s)));
   end Reachable_Window_Velocity;

   type Ramp_Stage_Index is range 0 .. 15;

   type Ramp_State is record
      X : Length;
      V : Velocity;
      A : Acceleration;
      J : Jerk;
      S : Snap;
   end record;

   type Ramp_State_Array is array (Ramp_Stage_Index) of Ramp_State;
   type Ramp_Stage_Flags is array (Ramp_Stage_Index range 1 .. 15) of Boolean;

   type Ramp_Stages is record
      States   : Ramp_State_Array;
      Crackles : Ramp_Stage_Flags;
   end record;
   --  States (K) is the motion at the end of stage K, with distance from the start of the ramp. Crackles (K) is True
   --  if stage K has nonzero crackle and nonzero duration.

   function Ramp_Boundary_States
     (Times : Feedrate_Profile_Times; Signed_Crackle : Crackle; Start_Vel : Velocity) return Ramp_Stages;
   --  Integrate the 15 stages documented for Feedrate_Profile_Times exactly.

   function Ramp_Speed_Bound (Stages : Ramp_Stages; X : Length; Accelerating : Boolean) return Velocity;
   --  An upper bound on the ramp's speed at distance X from its start. Acceleration is monotone inside each stage,
   --  so v² changes by at most twice the larger endpoint acceleration times the distance travelled in the stage.

   function Ramp_Distance_At_Speed (Stages : Ramp_Stages; Speed : Velocity; Accelerating : Boolean) return Length;
   --  A lower bound on the distance from the speed-ceiling end of the ramp (its start when accelerating, its end
   --  otherwise) at which the ramp reaches Speed. That end must be slower than Speed.

   function Ramp_Stage_Peaks
     (Stages : Ramp_Stages; Low, High : Length; Max_Crackle : Crackle) return Scalar_Derivative_Limits;
   --  The derivative peaks of the stages that overlap the closed distance interval. Acceleration, jerk and snap are
   --  monotone inside every stage, so their peaks are at stage ends.

   procedure Certify_Cellular_Ramp
     (Params      : Kinematic_Parameters;
      Cells       : Path_Cells;
      Caches      : in out Path_Cell_Caches;
      Times       : in out Feedrate_Profile_Times;
      Max_Crackle : in out Crackle;
      Start_Vel   : Velocity;
      Anchor      : Length;
      Forward     : Boolean;
      Valid       : out Boolean);
   --  Stretch one ramp in time until every cell it crosses certifies it. An acceleration ramp starts at run distance
   --  Anchor (Forward); a deceleration ramp, starting at Start_Vel, ends there. Stretching keeps the velocity change
   --  and the anchor, and lowers the speed reached at any distance from the anchor. Each cell is certified at the
   --  highest speed the ramp reaches in it and the derivative peaks of every stage between the anchor and the cell's
   --  far end. A stretch moves each cell onto stages nearer the anchor, so those peaks only fall, and one stretch
   --  normally certifies the ramp.

   function Ramp_Boundary_States
     (Times : Feedrate_Profile_Times; Signed_Crackle : Crackle; Start_Vel : Velocity) return Ramp_Stages
   is
      type Stage_Times is array (Ramp_Stage_Index range 1 .. 15) of Time;
      type Stage_Signs is array (Ramp_Stage_Index range 1 .. 15) of Dimensionless;
      Durations : constant Stage_Times :=
        [Times (1),
         Times (2),
         Times (1),
         Times (3),
         Times (1),
         Times (2),
         Times (1),
         Times (4),
         Times (1),
         Times (2),
         Times (1),
         Times (3),
         Times (1),
         Times (2),
         Times (1)];
      Signs     : constant Stage_Signs :=
        [1.0, 0.0, -1.0, 0.0, -1.0, 0.0, 1.0, 0.0, -1.0, 0.0, 1.0, 0.0, 1.0, 0.0, -1.0];
      Result    : Ramp_Stages;

      pragma Suppress (Overflow_Check);
      --  Each component of a stage's state is the previous stage's plus a polynomial in the stage's duration, so
      --  one that overflows, or becomes a NaN, stays an infinity or a NaN through every later stage. Testing the
      --  final state once therefore rejects exactly the ramps which checking each of these few hundred operations
      --  would, at a fraction of the cost of the most frequently evaluated kernel of cellular planning.
   begin
      Result.States (0) :=
        (X => 0.0 * mm, V => Start_Vel, A => 0.0 * mm / s**2, J => 0.0 * mm / s**3, S => 0.0 * mm / s**4);
      for K in Ramp_Stage_Index range 1 .. 15 loop
         declare
            P : constant Ramp_State := Result.States (K - 1);
            T : constant Time := Durations (K);
            C : constant Crackle := Signs (K) * Signed_Crackle;
         begin
            if T = 0.0 * s then
               --  An empty stage adds exact zeros to every component. Short ramps leave about half of them empty.
               Result.States (K) := P;
               Result.Crackles (K) := False;
            else
               Result.States (K) :=
                 (X => P.X + T * (P.V + T * (P.A / 2.0 + T * (P.J / 6.0 + T * (P.S / 24.0 + T * C / 120.0)))),
                  V => P.V + T * (P.A + T * (P.J / 2.0 + T * (P.S / 6.0 + T * C / 24.0))),
                  A => P.A + T * (P.J + T * (P.S / 2.0 + T * C / 6.0)),
                  J => P.J + T * (P.S + T * C / 2.0),
                  S => P.S + T * C);
               Result.Crackles (K) := Signs (K) /= 0.0 and then T > 0.0 * s;
            end if;
         end;
      end loop;
      declare
         Final : Ramp_State renames Result.States (15);
      begin
         if not (abs Final.X <= Length'Last
                 and then abs Final.V <= Velocity'Last
                 and then abs Final.A <= Acceleration'Last
                 and then abs Final.J <= Jerk'Last
                 and then abs Final.S <= Snap'Last)
         then
            raise Constraint_Error with "Unrepresentable ramp state.";
         end if;
      end;
      return Result;
   end Ramp_Boundary_States;

   function Ramp_Speed_Bound (Stages : Ramp_Stages; X : Length; Accelerating : Boolean) return Velocity is
   begin
      for K in Ramp_Stage_Index range 1 .. 15 loop
         if X <= Stages.States (K).X or else K = 15 then
            declare
               P     : constant Ramp_State := Stages.States (K - 1);
               Q     : constant Ramp_State := Stages.States (K);
               A_Max : constant Acceleration := Acceleration'Max (abs P.A, abs Q.A);
            begin
               if Accelerating then
                  return Velocity'Min (Q.V, (P.V * P.V + 2.0 * A_Max * Length'Max (0.0 * mm, X - P.X))**(1 / 2));
               else
                  return Velocity'Min (P.V, (Q.V * Q.V + 2.0 * A_Max * Length'Max (0.0 * mm, Q.X - X))**(1 / 2));
               end if;
            end;
         end if;
      end loop;
      raise Program_Error;
   end Ramp_Speed_Bound;

   function Ramp_Distance_At_Speed (Stages : Ramp_Stages; Speed : Velocity; Accelerating : Boolean) return Length is
      Total : constant Length := Stages.States (15).X;
   begin
      if Accelerating then
         for K in Ramp_Stage_Index range 1 .. 15 loop
            declare
               P     : constant Ramp_State := Stages.States (K - 1);
               Q     : constant Ramp_State := Stages.States (K);
               A_Max : constant Acceleration := Acceleration'Max (abs P.A, abs Q.A);
            begin
               if Q.V >= Speed then
                  return
                    (if A_Max > 0.0 * mm / s**2
                     then Length'Min (Q.X, P.X + Length'Max (0.0 * mm, (Speed * Speed - P.V * P.V) / (2.0 * A_Max)))
                     else P.X);
               end if;
            end;
         end loop;
         return Total;
      else
         for K in reverse Ramp_Stage_Index range 1 .. 15 loop
            declare
               P     : constant Ramp_State := Stages.States (K - 1);
               Q     : constant Ramp_State := Stages.States (K);
               A_Max : constant Acceleration := Acceleration'Max (abs P.A, abs Q.A);
            begin
               if P.V >= Speed then
                  return
                    Total
                    - (if A_Max > 0.0 * mm / s**2
                       then Length'Max (P.X, Q.X - Length'Max (0.0 * mm, (Speed * Speed - Q.V * Q.V) / (2.0 * A_Max)))
                       else Q.X);
               end if;
            end;
         end loop;
         return Total;
      end if;
   end Ramp_Distance_At_Speed;

   function Ramp_Stage_Peaks
     (Stages : Ramp_Stages; Low, High : Length; Max_Crackle : Crackle) return Scalar_Derivative_Limits
   is
      Result : Scalar_Derivative_Limits := (0.0 * mm / s**2, 0.0 * mm / s**3, 0.0 * mm / s**4, 0.0 * mm / s**5);
   begin
      for K in Ramp_Stage_Index range 1 .. 15 loop
         declare
            P : constant Ramp_State := Stages.States (K - 1);
            Q : constant Ramp_State := Stages.States (K);
         begin
            if P.X <= High and then Q.X >= Low then
               Result.Acceleration_Max :=
                 Acceleration'Max (Result.Acceleration_Max, Acceleration'Max (abs P.A, abs Q.A));
               Result.Jerk_Max := Jerk'Max (Result.Jerk_Max, Jerk'Max (abs P.J, abs Q.J));
               Result.Snap_Max := Snap'Max (Result.Snap_Max, Snap'Max (abs P.S, abs Q.S));
               if Stages.Crackles (K) then
                  Result.Crackle_Max := Max_Crackle;
               end if;
            end if;
         end;
      end loop;
      return Result;
   end Ramp_Stage_Peaks;

   procedure Certify_Cellular_Ramp
     (Params      : Kinematic_Parameters;
      Cells       : Path_Cells;
      Caches      : in out Path_Cell_Caches;
      Times       : in out Feedrate_Profile_Times;
      Max_Crackle : in out Crackle;
      Start_Vel   : Velocity;
      Anchor      : Length;
      Forward     : Boolean;
      Valid       : out Boolean)
   is
      Maximum_Stretches       : constant Positive := 16;
      Mixed_Derivative_Safety : constant Dimensionless := 0.999;
      --  Mixed_Derivative_Time_Scale multiplies every scale it returns by this factor, so a ramp that already fits
      --  returns exactly this value.
   begin
      Valid := False;
      for Attempt in 1 .. Maximum_Stretches loop
         declare
            Stages    : constant Ramp_Stages :=
              Ramp_Boundary_States (Times, (if Forward then Max_Crackle else -Max_Crackle), Start_Vel);
            Ramp      : constant Length := Stages.States (15).X;
            Span_Low  : constant Length := (if Forward then Anchor else Anchor - Ramp);
            Span_High : constant Length := (if Forward then Anchor + Ramp else Anchor);
            Scale     : Dimensionless := 1.0;

            function Speed_Scale (Target : Velocity; Boundary : Length) return Dimensionless;
            --  The stretch that brings the speed at Boundary down to Target. Boundary is a run distance and may lie
            --  beyond the current ramp. Time stretching scales every distance from the ramp's slow end by the same
            --  factor while preserving its speeds.

            function Speed_Scale (Target : Velocity; Boundary : Length) return Dimensionless is
               Slow_End : constant Velocity := (if Forward then Start_Vel else Stages.States (15).V);
               From_End : constant Length := (if Forward then Boundary - Anchor else Anchor - Boundary);
            begin
               if Slow_End >= Target or else From_End <= 0.0 * mm then
                  return 0.0;
               end if;
               return (1.0 - 1.0E-9) * Ramp_Distance_At_Speed (Stages, Target, Forward) / From_End;
            end Speed_Scale;
         begin
            if Ramp <= 0.0 * mm then
               Valid := True;
               return;
            end if;
            for Index in Cells'Range loop
               --  A piece that only touches the ramp at an end holds none of its motion: the ramp's derivatives there
               --  are limits from inside it, and whatever continues the motion (a coast, the other ramp or the
               --  window's end ceiling) admits the speed there.
               if Cell_Overlaps (Cells (Index), Span_Low, Span_High) then
                  declare
                     Cell       : Path_Cell renames Cells (Index);
                     Low        : constant Length := Length'Max (Cell.Start, Span_Low) - Span_Low;
                     High       : constant Length := Length'Min (Cell.Stop, Span_High) - Span_Low;
                     Far        : constant Length := (if Forward then High else Low);
                     Speed      : constant Velocity := Ramp_Speed_Bound (Stages, Far, Forward);
                     Boundary   : constant Length := (if Forward then Cell.Stop else Cell.Start);
                     --  The cell's end away from the anchor, even when only part of the cell overlaps the ramp.
                     --  Clipping the target to the current ramp repeats the same fractional stretch until it spans
                     --  the cell, which can exhaust the attempt budget for an otherwise feasible profile.
                     Cell_Scale : Dimensionless;
                  begin
                     if Speed > Cell.Ceiling then
                        Cell_Scale := Speed_Scale (Cell.Ceiling, Boundary);
                     elsif not Cell.Curved then
                        Cell_Scale := 1.0;
                     else
                        Cell_Scale :=
                          Mixed_Derivative_Time_Scale
                            (Params,
                             Cell.Bounds,
                             Speed,
                             Ramp_Stage_Peaks
                               (Stages,
                                (if Forward then 0.0 * mm else Low),
                                (if Forward then High else Ramp),
                                Max_Crackle),
                             Caches (Index));
                        if Cell_Scale >= Mixed_Derivative_Safety then
                           --  The solver always reserves its safety factor, so this is a pass.
                           Cell_Scale := 1.0;
                        elsif Cell_Scale <= 0.0 then
                           --  Curvature uses the whole budget at this speed. A slightly lower speed frees some.
                           Cell_Scale := Speed_Scale (0.99 * Speed, Boundary);
                        end if;
                     end if;
                     if not (Cell_Scale > 0.0) then
                        return;
                     end if;
                     Scale := Dimensionless'Min (Scale, Cell_Scale);
                  end;
               end if;
            end loop;
            if Scale >= 1.0 then
               Valid := True;
               return;
            end if;
            --  Overshoot slightly so that rounding cannot leave the stretched ramp just short of its certificate.
            Scale := Scale * (1.0 - 1.0E-9);
            for I in Feedrate_Profile_Times_Index loop
               Times (I) := Times (I) / Scale;
            end loop;
            --  Multiply successively so Scale**4 cannot underflow before multiplication by a large crackle.
            for I in 1 .. 4 loop
               Max_Crackle := Max_Crackle * Scale;
            end loop;
            if Max_Crackle < Dimensionless'Model_Small * mm / s**5 then
               return;
            end if;
         end;
      end loop;
   end Certify_Cellular_Ramp;

   function Cellular_Profile_At_Peak
     (Params                   : Kinematic_Parameters;
      Cells                    : Path_Cells;
      Caches                   : in out Path_Cell_Caches;
      Eval                     : Profile_Window_Evaluation;
      Start_Vel, End_Vel, Peak : Velocity) return Window_Profile_Result is
   begin
      return
        Cellular_Profile_At_Peak
          (Params,
           Cells,
           Caches,
           Prepare_Delta_V
             (Eval.Limits.Acceleration_Max, Eval.Limits.Jerk_Max, Eval.Limits.Snap_Max, Eval.Limits.Crackle_Max),
           Eval,
           Start_Vel,
           End_Vel,
           Peak);
   end Cellular_Profile_At_Peak;

   function Cellular_Profile_At_Peak
     (Params                   : Kinematic_Parameters;
      Cells                    : Path_Cells;
      Caches                   : in out Path_Cell_Caches;
      Ramp_Solver              : Delta_V_Solver;
      Eval                     : Profile_Window_Evaluation;
      Start_Vel, End_Vel, Peak : Velocity) return Window_Profile_Result
   is
      Window_Start                   : constant Length := Eval.Window.Start_Distance;
      Window_End                     : constant Length := Eval.Window.Start_Distance + Eval.Window.Distance;
      Result                         : Window_Profile_Result;
      Accel_Valid, Decel_Valid       : Boolean;
      Accel_Distance, Decel_Distance : Length;
      Actual_Peak                    : Velocity;
   begin
      --  Callers admit the endpoint speeds with Eval's endpoint ceilings; the cells cap the peak.
      if not Eval.Valid or else Peak < Velocity'Max (Start_Vel, End_Vel) then
         return Result;
      elsif Eval.Window.Distance <= 0.0 * mm then
         Result.Valid := Eval.Window.Distance = 0.0 * mm and then Start_Vel = End_Vel;
         return Result;
      elsif Peak <= 0.0 * mm / s then
         return Result;
      end if;
      Result.Max_Crackle := Eval.Limits.Crackle_Max;
      Result.Decel_Crackle := Eval.Limits.Crackle_Max;
      Result.Profile.Accel := Optimal_Profile_For_Delta_V (Ramp_Solver, Peak - Start_Vel);
      Result.Profile.Decel := Optimal_Profile_For_Delta_V (Ramp_Solver, Peak - End_Vel);
      Certify_Cellular_Ramp
        (Params, Cells, Caches, Result.Profile.Accel, Result.Max_Crackle, Start_Vel, Window_Start, True, Accel_Valid);
      if not Accel_Valid then
         return (others => <>);
      end if;
      Certify_Cellular_Ramp
        (Params, Cells, Caches, Result.Profile.Decel, Result.Decel_Crackle, Peak, Window_End, False, Decel_Valid);
      if not Decel_Valid then
         return (others => <>);
      end if;
      if (Peak > Start_Vel and then Result.Max_Crackle < Dimensionless'Model_Small * mm / s**5)
        or else (Peak > End_Vel and then Result.Decel_Crackle < Dimensionless'Model_Small * mm / s**5)
      then
         return (others => <>);
      end if;
      Actual_Peak := Fast_Velocity_At_Max_Time (Result.Profile.Accel, Result.Max_Crackle, Start_Vel);
      Accel_Distance := Fast_Distance_At_Max_Time (Result.Profile.Accel, Result.Max_Crackle, Start_Vel);
      Decel_Distance := Fast_Distance_At_Max_Time (Result.Profile.Decel, -Result.Decel_Crackle, Actual_Peak);
      if Accel_Distance + Decel_Distance > Eval.Window.Distance or else Actual_Peak <= 0.0 * mm / s then
         return (others => <>);
      end if;
      declare
         Coast_Start : constant Length := Window_Start + Accel_Distance;
         Coast_End   : constant Length := Window_End - Decel_Distance;
      begin
         for Cell of Cells loop
            if Cell.Start < Coast_End
              and then Cell.Stop > Coast_Start
              and then (Cell.Stop > Cell.Start or else Coast_End > Coast_Start)
              and then Actual_Peak > Cell.Ceiling
            then
               return (others => <>);
            end if;
         end loop;
      end;
      Result.Profile.Coast := (Eval.Window.Distance - Accel_Distance - Decel_Distance) / Actual_Peak;
      Result.Valid :=
        Profile_Is_Valid
          (Result.Profile,
           Eval.Window.Distance,
           Start_Vel,
           End_Vel,
           Result.Decel_Crackle,
           Actual_Peak,
           Accel_Distance,
           Decel_Distance);
      return Result;
   exception
      when Constraint_Error =>
         return (others => <>);
   end Cellular_Profile_At_Peak;

   function Highest_Feasible_Speed (Lower, Upper : Velocity) return Velocity is
      Low  : Velocity := Lower;
      High : Velocity := Upper;
   begin
      for Halving in 1 .. Cellular_Search_Bisections loop
         declare
            Mid : constant Velocity := Low + 0.5 * (High - Low);
         begin
            exit when Mid = Low or else Mid = High or else High - Low <= Cellular_Search_Tolerance * Low;
            if Feasible (Mid) then
               Low := Mid;
            else
               High := Mid;
            end if;
         end;
      end loop;
      return Low;
   end Highest_Feasible_Speed;

   function Generate_Cellular_Profile
     (Params             : Kinematic_Parameters;
      Cells              : Path_Cells;
      Eval               : Profile_Window_Evaluation;
      Start_Vel, End_Vel : Velocity) return Window_Profile_Result
   is
      Window_End      : constant Length := Eval.Window.Start_Distance + Eval.Window.Distance;
      Minimum_Peak    : constant Velocity := Velocity'Max (Start_Vel, End_Vel);
      Best            : Window_Profile_Result;
      Best_Time       : Time := 1.0E100 * s;
      Highest_Ceiling : Velocity := 0.0 * mm / s;
      Lowest_Ceiling  : Velocity := Velocity'Last;
      Straight        : Boolean := True;
      Caches          : Path_Cell_Caches (Cells'Range);
      Solver          : constant Delta_V_Solver :=
        Prepare_Delta_V
          (Eval.Limits.Acceleration_Max, Eval.Limits.Jerk_Max, Eval.Limits.Snap_Max, Eval.Limits.Crackle_Max);

      Last_Peak       : Velocity := 0.0 * mm / s;
      Last_Duration   : Time := 1.0E100 * s;
      Last_Considered : Boolean := False;

      function Consider (Peak : Velocity) return Time;
      --  The duration of the candidate at Peak, or 1.0E100 s if it is infeasible. Every candidate is offered to Best.

      function Consider (Peak : Velocity) return Time is
      begin
         if Last_Considered and then Identical (Peak, Last_Peak) then
            return Last_Duration;
         end if;

         declare
            Candidate      : constant Window_Profile_Result :=
              Cellular_Profile_At_Peak (Params, Cells, Caches, Solver, Eval, Start_Vel, End_Vel, Peak);
            Candidate_Time : constant Time :=
              (if Candidate.Valid then Total_Time (Candidate.Profile) else 1.0E100 * s);
         begin
            if Candidate.Valid and then (not Best.Valid or else Candidate_Time < Best_Time) then
               Best := Candidate;
               Best_Time := Candidate_Time;
            end if;
            Last_Peak := Peak;
            Last_Duration := Candidate_Time;
            Last_Considered := True;
            return Candidate_Time;
         end;
      end Consider;

      function Feasible (Peak : Velocity) return Boolean
      is (Consider (Peak) < 1.0E100 * s);

      function Highest_Feasible_Peak is new Highest_Feasible_Speed (Feasible);

      Top_Peak : Velocity;
      --  The highest peak found feasible.
   begin
      for Cell of Cells loop
         if Cell_Overlaps (Cell, Eval.Window.Start_Distance, Window_End) then
            Straight := Straight and then not Cell.Curved;
            Highest_Ceiling := Velocity'Max (Highest_Ceiling, Cell.Ceiling);
            Lowest_Ceiling := Velocity'Min (Lowest_Ceiling, Cell.Ceiling);
         end if;
      end loop;

      if not Eval.Valid then
         return Best;
      elsif Straight then
         --  Without curvature the tangent-only optimum under the lowest ceiling is certified, as for one segment.
         if Lowest_Ceiling < Minimum_Peak then
            return Best;
         end if;
         declare
            Full : constant Full_Profile_Result :=
              Try_Optimal_Full_Profile
                (Start_Vel,
                 Lowest_Ceiling,
                 End_Vel,
                 Eval.Window.Distance,
                 Eval.Limits.Acceleration_Max,
                 Eval.Limits.Jerk_Max,
                 Eval.Limits.Snap_Max,
                 Eval.Limits.Crackle_Max);
         begin
            --  The handler below would return Best for an unreachable speed change as well, at the cost of an
            --  exception.
            if not Full.Reachable then
               return Best;
            end if;
            Best.Profile := Full.Profile;
         end;
         Best.Max_Crackle := Eval.Limits.Crackle_Max;
         Best.Decel_Crackle := Eval.Limits.Crackle_Max;
         Best.Valid := True;
         return Best;
      end if;

      --  A curved window's peak may exceed the ceilings of cells that its ramps cross at lower speed, but no peak
      --  exceeds the ceiling of every cell.
      if Highest_Ceiling < Minimum_Peak then
         return Best;
      elsif Eval.Window.Distance <= 0.0 * mm then
         return Cellular_Profile_At_Peak (Params, Cells, Caches, Solver, Eval, Start_Vel, End_Vel, Minimum_Peak);
      end if;

      if Minimum_Peak > 0.0 * mm / s and then not Feasible (Minimum_Peak) then
         return Best;
      end if;
      --  TODO: Investigate whether this search is worth its cost on real prints. Compared with the earlier near-top
      --  shortcut and overrun-guided feasibility search, it made Benchy CONTINUOUS planning about 11% slower (4.45 s
      --  to 4.95 s) and its planned motion 45 ms longer, despite the faster profiles in Test_Cellular_Profile_Search.

      --  Keep the intermediate candidates from bisection as well as its feasible endpoint. Cellular stretches change
      --  at cell and ramp-stage boundaries, so fitting a smooth overrun model can skip faster candidates.
      Top_Peak :=
        (if Feasible (Highest_Ceiling)
         then Highest_Ceiling
         else Highest_Feasible_Peak (Minimum_Peak, Highest_Ceiling));
      --  Always search lower peaks too. A cellular profile can have a local time minimum near the highest feasible
      --  peak and a faster one below it, so one nearby comparison cannot justify skipping the interval.
      declare
         Ratio      : constant Dimensionless := 0.618033988749895;
         Lower      : Velocity := Minimum_Peak;
         Upper      : Velocity := Top_Peak;
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
   end Generate_Cellular_Profile;

   function Reachable_Cellular_Velocity
     (Params    : Kinematic_Parameters;
      Cells     : Path_Cells;
      Eval      : Profile_Window_Evaluation;
      Fixed_Vel : Velocity;
      Forward   : Boolean;
      Floor     : Velocity := 0.0 * mm / s;
      Cap       : Velocity := Velocity'Last) return Velocity
   is
      Window_End : constant Length := Eval.Window.Start_Distance + Eval.Window.Distance;
      Useful_Cap : constant Velocity := Raised_Reach_Cap (Cap);
      --  As in Reachable_Window_Velocity, a reach found here is at least Cap after its margin.
      Highest    : Velocity := 0.0 * mm / s;
      Lowest     : Velocity := Velocity'Last;
      Straight   : Boolean := True;
      Lower      : Velocity;
      Upper      : Velocity;
      Ceiling    : Velocity;
      Caches     : Path_Cell_Caches (Cells'Range);
      Solver     : constant Delta_V_Solver :=
        Prepare_Delta_V
          (Eval.Limits.Acceleration_Max, Eval.Limits.Jerk_Max, Eval.Limits.Snap_Max, Eval.Limits.Crackle_Max);

      function Feasible (Free : Velocity) return Boolean
      is (if Forward
          then Cellular_Profile_At_Peak (Params, Cells, Caches, Solver, Eval, Fixed_Vel, Free, Free).Valid
          else Cellular_Profile_At_Peak (Params, Cells, Caches, Solver, Eval, Free, Fixed_Vel, Free).Valid);

      function Highest_Feasible is new Highest_Feasible_Speed (Feasible);
   begin
      for Cell of Cells loop
         if Cell_Overlaps (Cell, Eval.Window.Start_Distance, Window_End) then
            Straight := Straight and then not Cell.Curved;
            Highest := Velocity'Max (Highest, Cell.Ceiling);
            Lowest := Velocity'Min (Lowest, Cell.Ceiling);
         end if;
      end loop;
      if Straight then
         --  The tangent-only problem is symmetric, so both directions reduce to the single-segment solve.
         return
           Reachable_Velocity
             (Fixed_Vel, Velocity'Min (Lowest, Useful_Cap), Eval.Window.Distance, Eval.Limits, Reach_Search_Tolerance);
      end if;

      --  No speed at the reached end exceeds that end's ceiling, even where the fixed end is faster.
      Ceiling :=
        Velocity'Min (Useful_Cap, Velocity'Min (Highest, (if Forward then Eval.End_Max_Vel else Eval.Start_Max_Vel)));
      Lower := Velocity'Min (Fixed_Vel, Ceiling);
      if Eval.Window.Distance <= 0.0 * mm or else Ceiling <= Fixed_Vel then
         return Lower;
      elsif Ceiling <= Floor then
         return Ceiling;
      end if;
      --  The tangent-only solve is a cheap upper bound and avoids searching an unnecessarily large speed range.
      Upper := Reachable_Velocity (Fixed_Vel, Ceiling, Eval.Window.Distance, Eval.Limits, Reach_Search_Tolerance);
      if Upper <= Floor then
         return Upper;
      end if;
      if Floor > Lower then
         --  Only a reach which still exceeds Floor after its margin is of use, so decide first whether there is one.
         declare
            Useful : constant Velocity := Velocity'Min (Upper, Raised_Reach_Cap (Floor));
         begin
            if Useful < Upper then
               if not Feasible (Useful) then
                  return Floor;
               end if;
               Lower := Useful;
            end if;
         end;
      end if;
      if Feasible (Upper) then
         return Upper;
      elsif Reach_Margin * Upper <= Floor then
         return Floor;
      end if;
      return Velocity'Max (Velocity'Max (Fixed_Vel, Floor), Reach_Margin * Highest_Feasible (Lower, Upper));
   end Reachable_Cellular_Velocity;

   function Accel_Ramp_End_Of
     (Profile : Feedrate_Profile; Accel_Crackle : Crackle; Start_Vel : Velocity) return Accel_Ramp_End;

   function Distance_After_Accel_Ramp
     (Profile : Feedrate_Profile; T : Time; Decel_Crackle : Crackle; Ramp_End : Accel_Ramp_End) return Length;
   --  Profile_Distance_At_Time for T beyond the acceleration ramp, which ends at Ramp_End.

   function Velocity_After_Accel_Ramp
     (Profile : Feedrate_Profile; T : Time; Decel_Crackle : Crackle; Accel_Time : Time; Mid_Vel : Velocity)
      return Velocity;
   --  Profile_Velocity_At_Time for T beyond the acceleration ramp, which ends at Accel_Time with Mid_Vel.

   function Accel_Ramp_End_Of
     (Profile : Feedrate_Profile; Accel_Crackle : Crackle; Start_Vel : Velocity) return Accel_Ramp_End
   is
      Accel_Time : constant Time := Total_Time (Profile.Accel);
   begin
      return
        (Accel_Time => Accel_Time,
         Mid_Vel    => Velocity_At_Time (Profile.Accel, Accel_Time, Accel_Crackle, Start_Vel),
         Accel_Dist => Distance_At_Time (Profile.Accel, Accel_Time, Accel_Crackle, Start_Vel));
   end Accel_Ramp_End_Of;

   function Distance_After_Accel_Ramp
     (Profile : Feedrate_Profile; T : Time; Decel_Crackle : Crackle; Ramp_End : Accel_Ramp_End) return Length is
   begin
      if T < Ramp_End.Accel_Time + Profile.Coast then
         return Ramp_End.Accel_Dist + Ramp_End.Mid_Vel * (T - Ramp_End.Accel_Time);
      end if;
      return
        Ramp_End.Accel_Dist
        + Ramp_End.Mid_Vel * Profile.Coast
        + Distance_At_Time
            (Profile.Decel,
             Time'Min (T - (Ramp_End.Accel_Time + Profile.Coast), Total_Time (Profile.Decel)),
             -Decel_Crackle,
             Ramp_End.Mid_Vel);
   end Distance_After_Accel_Ramp;

   function Profile_Distance_At_Time
     (Profile : Feedrate_Profile; T : Time; Accel_Crackle, Decel_Crackle : Crackle; Start_Vel : Velocity) return Length
   is
   begin
      --  Equal crackles need no separate path: this is Distance_At_Time's arithmetic, step for step.
      if T <= Total_Time (Profile.Accel) then
         return Distance_At_Time (Profile.Accel, T, Accel_Crackle, Start_Vel);
      end if;
      return
        Distance_After_Accel_Ramp (Profile, T, Decel_Crackle, Accel_Ramp_End_Of (Profile, Accel_Crackle, Start_Vel));
   end Profile_Distance_At_Time;

   function Velocity_After_Accel_Ramp
     (Profile : Feedrate_Profile; T : Time; Decel_Crackle : Crackle; Accel_Time : Time; Mid_Vel : Velocity)
      return Velocity is
   begin
      if T < Accel_Time + Profile.Coast then
         return Mid_Vel;
      end if;
      return
        Velocity_At_Time
          (Profile.Decel,
           Time'Min (T - (Accel_Time + Profile.Coast), Total_Time (Profile.Decel)),
           -Decel_Crackle,
           Mid_Vel);
   end Velocity_After_Accel_Ramp;

   function Profile_Velocity_At_Time
     (Profile : Feedrate_Profile; T : Time; Accel_Crackle, Decel_Crackle : Crackle; Start_Vel : Velocity)
      return Velocity
   is
      Accel_Time : constant Time := Total_Time (Profile.Accel);
   begin
      --  Equal crackles need no separate path: this is Velocity_At_Time's arithmetic, step for step.
      if T <= Accel_Time then
         return Velocity_At_Time (Profile.Accel, T, Accel_Crackle, Start_Vel);
      end if;
      return
        Velocity_After_Accel_Ramp
          (Profile,
           T,
           Decel_Crackle,
           Accel_Time,
           Velocity_At_Time (Profile.Accel, Accel_Time, Accel_Crackle, Start_Vel));
   end Profile_Velocity_At_Time;

   function Identical (Left, Right : Velocity) return Boolean is
      type Velocity_Bits is mod 2**Velocity'Size;
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
      Cache     : Window_Search_Cache;

      Last_Peak       : Velocity := 0.0 * mm / s;
      Last_Duration   : Time := 1.0E100 * s;
      Last_Overrun    : Length := Length'Last;
      Last_Considered : Boolean := False;
      --  A collapsed feasible interval makes the search repeat one peak. Its certificate is deterministic and
      --  cannot improve Best a second time, so only its duration and ramp overrun are needed.

      Lower_Overrun : Length := -Eval.Window.Distance;
      --  The overrun at the minimum peak. With both ends at rest that peak has no ramps and is never considered.

      Top_Known    : Boolean := False;
      Top_Peak     : Velocity := 0.0 * mm / s;
      Top_Duration : Time := 1.0E100 * s;
      --  The highest valid peak considered so far and its duration. The feasibility search ends on it, usually
      --  after its last evaluation was of a peak just above.

      function Consider (Peak : Velocity) return Time;

      function Consider (Peak : Velocity) return Time is
      begin
         if Last_Considered and then Identical (Peak, Last_Peak) then
            return Last_Duration;
         end if;

         declare
            Candidate          : Window_Profile_Result;
            Candidate_Duration : Time;
         begin
            Certify_Profile_At_Peak (Params, Eval, Start_Vel, End_Vel, Peak, Cache, Candidate, Last_Overrun);
            Candidate_Duration := (if Candidate.Valid then Total_Time (Candidate.Profile) else 1.0E100 * s);
            if Candidate.Valid and then (not Best.Valid or else Candidate_Duration < Best_Time) then
               Best := Candidate;
               Best_Time := Candidate_Duration;
            end if;
            if Candidate.Valid and then (not Top_Known or else Peak >= Top_Peak) then
               Top_Known := True;
               Top_Peak := Peak;
               Top_Duration := Candidate_Duration;
            end if;
            Last_Peak := Peak;
            Last_Duration := Candidate_Duration;
            Last_Considered := True;
            return Candidate_Duration;
         end;
      end Consider;

      function Raw_Overrun (Peak : Dimensionless) return Dimensionless;
      --  The ramp overrun of the candidate at Peak, which Consider also offers to Best.

      function Raw_Overrun (Peak : Dimensionless) return Dimensionless is
         Unused : constant Time := Consider (Peak * (mm / s));
      begin
         return Last_Overrun / mm;
      end Raw_Overrun;

      function Highest_Peak is new Largest_Nonpositive (Raw_Overrun);

      Top_Time : Time;
      --  The duration of the candidate at Upper.
   begin
      if not Eval.Valid or else Upper < Lower then
         return Best;
      elsif not Eval.Curved then
         declare
            Full : constant Full_Profile_Result :=
              Try_Optimal_Full_Profile
                (Start_Vel,
                 Upper,
                 End_Vel,
                 Eval.Window.Distance,
                 Eval.Limits.Acceleration_Max,
                 Eval.Limits.Jerk_Max,
                 Eval.Limits.Snap_Max,
                 Eval.Limits.Crackle_Max);
         begin
            --  A window too short for its speed change is an ordinary outcome for a candidate which lookahead did
            --  not choose, so it must not cost an exception.
            if not Full.Reachable then
               return Best;
            end if;
            Best.Profile := Full.Profile;
         end;
         Best.Max_Crackle := Eval.Limits.Crackle_Max;
         Best.Decel_Crackle := Eval.Limits.Crackle_Max;
         Best.Valid := True;
         return Best;
      elsif Eval.Window.Distance <= 0.0 * mm then
         return Profile_At_Peak (Params, Eval, Start_Vel, End_Vel, Lower);
      end if;

      --  At the minimum peak there is only one velocity transition, exactly the one certified by lookahead.
      if Lower > 0.0 * mm / s then
         if Consider (Lower) = 1.0E100 * s then
            return Best;
         end if;
         Lower_Overrun := Last_Overrun;
      end if;
      Top_Time := Consider (Upper);
      if Top_Time = 1.0E100 * s then
         --  Locate the feasible peak-speed interval: the ramps lengthen with the peak, and its highest feasible
         --  value is where they fill the window. Every accepted candidate remains available even when a later
         --  trial is infeasible. The relative stop also works for very small representable speeds.
         Upper :=
           Highest_Peak
             (Lower              => Lower / (mm / s),
              Upper              => Upper / (mm / s),
              Lower_Residual     => Lower_Overrun / mm,
              Upper_Residual     => Last_Overrun / mm,
              Relative_Tolerance => 1.0E-5)
           * (mm / s);
         Top_Time := (if Top_Known and then Identical (Top_Peak, Upper) then Top_Duration else Consider (Upper));
      end if;
      --  A feasible maximum peak need not minimize time: a slightly lower peak can free substantial ramp
      --  budget. Search the complete feasible interval; never impose a fixed reduction on coasting speed.
      declare
         Ratio    : constant Dimensionless := 0.618033988749895;
         Near_Top : constant Velocity := Upper - Ratio**12 * (Upper - Lower);
      begin
         --  The search below narrows the interval to this fraction of its width around a minimum that it takes
         --  to be the only one. On that same assumption, a candidate that far below the maximum peak which is no
         --  faster than the maximum itself puts the minimum within that distance of the maximum, which has
         --  already been considered. That is the usual case, and it saves the whole search.
         if Top_Time < 1.0E100 * s and then Near_Top > Lower and then Near_Top < Upper then
            if Consider (Near_Top) >= Top_Time then
               return Best;
            end if;
         end if;
      end;
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
      Workspace.Timing_Join_Vetoes (Block.Corners'Range) := [others => False];
      loop
         My_Kinematic_Limiter.Run (Block, Motor_Map, Workspace);
         My_Feedrate_Profile_Generator.Run
           (Block, Motor_Map, Workspace.Window_Evaluations, Result, Limit_Scale => (if Stopped then 0.5 else 1.0));
         exit when Result.Valid;

         if Timing_Run_Last (Block'Access, Result.Failed_Segment) /= Result.Failed_Segment then
            --  A run may need a valley that only a junction provides. Split it before slowing anything down; the
            --  vetoes only grow, so this happens at most once per corner.
            for Corner in Result.Failed_Segment .. Timing_Run_Last (Block'Access, Result.Failed_Segment) - 1 loop
               Workspace.Timing_Join_Vetoes (Corner) := True;
            end loop;
         elsif not Stopped and then Attempts (Result.Failed_Segment) = Blended_Retries then
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
            --  segment's feedrate also slows its endpoints. Failed runs are split above, so only one segment fails.
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

      function Run_Time_Through (Segment : Finishing_Corners_Index) return Time;
      --  Time of Segment and the earlier segments of its timing run, which First_Time replaces as a whole.

      function Run_Time_Through (Segment : Finishing_Corners_Index) return Time is
         Total : Time := 0.0 * s;
      begin
         for J in Timing_Run_First (Block'Access, Segment) .. Segment loop
            Total := Total + Segment_Time (Block'Access, J);
         end loop;
         return Total;
      end Run_Time_Through;

      procedure Materialize (Through : Corners_Index);

      procedure Materialize (Through : Corners_Index) is
      begin
         while Materialized < Through loop
            Materialized := @ + 1;
            Block.Extrusion_Reference_Positions (Materialized) := @ + E_Shift;
         end loop;
      end Materialize;

      function Straight_Bound
        (Segment : Finishing_Corners_Index; Start_Vel, End_Vel : Velocity; Distance : Length) return Time;

      function Straight_Bound
        (Segment : Finishing_Corners_Index; Start_Vel, End_Vel : Velocity; Distance : Length) return Time
      is
         --  The tangent-only optimum over Distance under the limits of the segment's primitive, the least
         --  restrictive of its windows. Every certified profile also respects them, so none can be faster. E
         --  transitions are left out, since a split profile can avoid them.
         Start_Transition : constant Length := Segment_Start_Transition_Distance (Block'Access, Segment);
         Start            : constant Length :=
           Length'Max (Start_Transition, Block.Extrusion_Transitions (Segment - 1).Half_Width);
         Straight         : constant Length :=
           (if Start = Start_Transition
              and then
                Block.Extrusion_Transitions (Segment).Half_Width
                <= Segment_End_Transition_Distance (Block'Access, Segment)
            then Block.Primitive_Distances (Segment)
            else
              Segment_Total_Distance (Block'Access, Segment)
              - Length'Max
                  (Segment_End_Transition_Distance (Block'Access, Segment),
                   Block.Extrusion_Transitions (Segment).Half_Width)
              - Start);
         Eval             : constant Profile_Window_Evaluation :=
           Evaluate_Profile_Window
             (Block'Access,
              Motor_Map,
              Segment,
              (Start_Distance => Start, Distance => Straight),
              Block.Limited_Segment_Feedrates (Segment));
      begin
         if not Eval.Valid or else Straight <= 0.0 * mm or else Eval.Max_Vel < Velocity'Max (Start_Vel, End_Vel) then
            return 0.0 * s;
         end if;
         begin
            --  The handler below must also cover the declaration, so the profile is formed inside this block.
            declare
               Full : constant Full_Profile_Result :=
                 Try_Optimal_Full_Profile
                   (Start_Vel,
                    Eval.Max_Vel,
                    End_Vel,
                    Distance,
                    Eval.Limits.Acceleration_Max,
                    Eval.Limits.Jerk_Max,
                    Eval.Limits.Snap_Max,
                    Eval.Limits.Crackle_Max);
            begin
               --  An unreachable speed change is an ordinary outcome here, so it must not cost an exception.
               return (if Full.Reachable then Total_Time (Full.Profile) else 1.0E100 * s);
            end;
         exception
            when Constraint_Error =>
               --  Any other error of the profile, which no certified profile can avoid either.
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
         First_Time    : out Time);

      procedure Try_Stops
        (First, Last   : Finishing_Corners_Index;
         Keep          : Boolean;
         Required_Gain : Time;
         Outcome       : out Stop_Outcome;
         Gain          : out Time;
         First_Time    : out Time)
      is
         --  Stop at every corner in First .. Last and replan the timing runs containing segments First .. Last + 1
         --  with their outer junction speeds fixed. Gain is the certified saving; the change is kept only if Keep and
         --  it exceeds Required_Gain.
         type Saved_Segment is record
            Distance, Start_Distance, E_Reference : Length;
            Feedrate                              : Velocity;
            Profile                               : Feedrate_Profile;
            Max_Crackle, Decel_Crackle            : Crackle;
            Window                                : Profile_Window;
            Ends                                  : Profile_End_Pair;
            Run_Distance_Offset                   : Length;
            Run_Time_Offset, Run_Segment_Time     : Time;
            End_Speed                             : Velocity;
         end record;
         type Saved_Corner is record
            Transition : Corner_Transition_Evaluator;
            Bounds     : Unit_Speed_Axial_Derivative_Bounds;
            Speed      : Velocity;
            Extrusion  : Extrusion_Transition;
         end record;
         subtype Window_Segments is
           Finishing_Corners_Index
             range Timing_Run_First (Block'Access, First) .. Timing_Run_Last (Block'Access, Last + 1);
         --  The stopped corners are junctions, so whole runs border them. Replanning whole runs keeps each run's one
         --  profile, and its interior corner speeds and member offsets are saved like any other planned field.
         Saved         : constant array (Window_Segments) of Saved_Segment :=
           [for I in Window_Segments =>
              (Block.Primitive_Distances (I),
               Block.Primitive_Start_Distances (I),
               Block.Extrusion_Reference_Positions (I),
               Block.Limited_Segment_Feedrates (I),
               Block.Feedrate_Profiles (I),
               Block.Profile_Crackles (I),
               Block.Profile_Decel_Crackles (I),
               Block.Profile_Windows (I),
               Block.Profile_Ends (I),
               Block.Timing_Distance_Offsets (I),
               Block.Timing_Time_Offsets (I),
               Block.Timing_Segment_Times (I),
               Block.Corner_Velocity_Limits (I))];
         Saved_Corners : constant array (First .. Last) of Saved_Corner :=
           [for J in First .. Last =>
              (Block.Corner_Transitions (J),
               Workspace.Corner_Derivative_Bounds (J),
               Block.Corner_Velocity_Limits (J),
               Block.Extrusion_Transitions (J))];
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

         procedure Restore;

         procedure Restore is
         begin
            for J in Saved_Corners'Range loop
               Block.Corner_Transitions (J) := Saved_Corners (J).Transition;
               Workspace.Corner_Derivative_Bounds (J) := Saved_Corners (J).Bounds;
               Block.Corner_Velocity_Limits (J) := Saved_Corners (J).Speed;
               Block.Extrusion_Transitions (J) := Saved_Corners (J).Extrusion;
            end loop;
            for I in Saved'Range loop
               Block.Primitive_Distances (I) := Saved (I).Distance;
               Block.Primitive_Start_Distances (I) := Saved (I).Start_Distance;
               Block.Extrusion_Reference_Positions (I) := Saved (I).E_Reference;
               Block.Limited_Segment_Feedrates (I) := Saved (I).Feedrate;
               Block.Feedrate_Profiles (I) := Saved (I).Profile;
               Block.Profile_Crackles (I) := Saved (I).Max_Crackle;
               Block.Profile_Decel_Crackles (I) := Saved (I).Decel_Crackle;
               Block.Profile_Windows (I) := Saved (I).Window;
               Block.Profile_Ends (I) := Saved (I).Ends;
               Block.Timing_Distance_Offsets (I) := Saved (I).Run_Distance_Offset;
               Block.Timing_Time_Offsets (I) := Saved (I).Run_Time_Offset;
               Block.Timing_Segment_Times (I) := Saved (I).Run_Segment_Time;
               Block.Corner_Velocity_Limits (I) := Saved (I).End_Speed;
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
         --  Change only these junctions. The retained outer transitions, E transitions and velocities remain
         --  identical, so certified replacement profiles splice into the existing trajectory. Stopping only lengthens
         --  the replanned segments, so their outer E transitions still fit. A stop needs no E transition.
         for Corner in First .. Last loop
            Block.Corner_Transitions (Corner) :=
              To_Evaluator (Stop_At ([Block.Corners (Corner) with delta E_Axis => 0.0 * mm]));
            Workspace.Corner_Derivative_Bounds (Corner) := Derivative_Bounds (Block.Corner_Transitions (Corner));
            Block.Corner_Velocity_Limits (Corner) := 0.0 * mm / s;
            Block.Extrusion_Transitions (Corner) := (others => <>);
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
         if Workspace.Check_Extrusion_Position
           and then Delta_E /= 0.0 * mm
           and then Window_Segments'Last < Block.N_Corners
         then
            --  Later profiles change only by an E translation. Suffix bounds avoid scanning or replanning the
            --  rest of the block for each candidate. Conservative bounds may reject a feasible alternative.
            if Length'Adjacent (Workspace.Suffix_E_Min (Window_Segments'Last + 1) + New_Shift, -Length'Last)
              < Block.Params.Bounds.Lower_E
              or else
                Length'Adjacent (Workspace.Suffix_E_Max (Window_Segments'Last + 1) + New_Shift, Length'Last)
                > Block.Params.Bounds.Upper_E
            then
               Restore;
               return;
            end if;
         end if;
         for Run_First in Window_Segments loop
            if Timing_Run_First (Block'Access, Run_First) = Run_First then
               declare
                  Run_Last : constant Finishing_Corners_Index := Timing_Run_Last (Block'Access, Run_First);
               begin
                  My_Feedrate_Profile_Generator.Select_Feedrate_Profile_Window
                    (Block'Access,
                     Motor_Map,
                     Run_First,
                     Evaluate_Run_Windows (Block'Access, Motor_Map, Run_First),
                     Result,
                     Limit_Scale => 1.0);
                  if not Result.Valid then
                     Restore;
                     return;
                  end if;
                  if Run_Last /= Run_First then
                     Assign_Timing_Members (Block'Access, Run_First);
                  end if;
                  for I in Run_First .. Run_Last loop
                     New_Time := @ + Segment_Time (Block'Access, I);
                     Old_Selected := @ + Old_Times (I);
                  end loop;
                  if not (New_Time
                          < (if Run_Last = Window_Segments'Last then Old_Selected - Required_Gain else Old_Time))
                  then
                     Restore;
                     return;
                  end if;
                  if Run_First = Window_Segments'First then
                     Outcome := Rejected_Late;
                     First_Time := New_Time;
                  end if;
               end;
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
         end loop;
      exception
         when Constraint_Error =>
            Outcome := Rejected_Early;
            Restore;
      end Try_Stops;

      function Stop_Cannot_Pay (Corner : Finishing_Corners_Index) return Boolean;
      --  Whether tangent-only bounds already show that the sweep's comparisons at Corner would change nothing:
      --  stopping at Corner alone would be rejected, and the pair with the next corner would not be tried. A stop
      --  restores both adjoining primitives to their commanded ends, so each replanned segment is bounded below as
      --  in Straight_Bound. This costs two cheap profiles for the common corner whose blend is clearly faster than
      --  a stop, instead of the full replan of both segments.

      function Stop_Cannot_Pay (Corner : Finishing_Corners_Index) return Boolean is
         Old_Time     : Time;
         First_Bound  : Time;
         Second_Bound : Time;
      begin
         if Timing_Run_First (Block'Access, Corner) /= Corner
           or else Timing_Run_Last (Block'Access, Corner + 1) /= Corner + 1
         then
            --  The bounds are for single segments. A run of several is always replanned.
            return False;
         end if;
         --  Try_Stops sums the old and new times in this order and keeps a stop only if the new sum is smaller.
         Old_Time := 0.0 * s + Segment_Time (Block'Access, Corner) + Segment_Time (Block'Access, Corner + 1);
         First_Bound :=
           Straight_Bound
             (Corner,
              Block.Corner_Velocity_Limits (Corner - 1),
              0.0 * mm / s,
              Segment_Start_Transition_Distance (Block'Access, Corner)
              + (Primitive_Length (Block'Access, Corner) - Block.Primitive_Start_Distances (Corner)));
         if First_Bound < Old_Time then
            Second_Bound :=
              Straight_Bound
                (Corner + 1,
                 0.0 * mm / s,
                 Block.Corner_Velocity_Limits (Corner + 1),
                 Block.Primitive_Start_Distances (Corner + 1)
                 + Block.Primitive_Distances (Corner + 1)
                 + Segment_End_Transition_Distance (Block'Access, Corner + 1));
            if First_Bound + Second_Bound < Old_Time then
               return False;
            end if;
            --  The stop alone loses, but its first segment may still be certified in less than the old time of
            --  both. The sweep then tries the pair with the next corner when the pair's own bound allows it. The
            --  first segment's bound is no later than its replanned time, so a pair bound that fails with it
            --  fails with that time too.
            if Corner + 1 < Block.N_Corners
              and then Transition_Kind (Block.Corner_Transitions (Corner + 1)) = Stereographic_Transition
            then
               Materialize (Timing_Run_Last (Block'Access, Corner + 2));
               if First_Bound
                 + Straight_Bound (Corner + 1, 0.0 * mm / s, 0.0 * mm / s, Primitive_Length (Block'Access, Corner + 1))
                 + Straight_Bound
                     (Corner + 2,
                      0.0 * mm / s,
                      Block.Corner_Velocity_Limits (Corner + 2),
                      Block.Primitive_Start_Distances (Corner + 2)
                      + Block.Primitive_Distances (Corner + 2)
                      + Segment_End_Transition_Distance (Block'Access, Corner + 2))
                 < Run_Time_Through (Corner)
                   + Segment_Time (Block'Access, Corner + 1)
                   + Segment_Time (Block'Access, Corner + 2)
               then
                  return False;
               end if;
            end if;
         end if;
         --  Try_Stops counts every corner it compares.
         Workspace.Local_Comparison_Corners := @ + 1;
         return True;
      end Stop_Cannot_Pay;

      procedure Try_Merge (Corner : Finishing_Corners_Index);
      --  Join the timing runs ending and starting at Corner and replan the joined run, with whole-window profiles
      --  only, with its outer junction speeds fixed. Keep it only if it is certified and faster. Geometry and E
      --  references do not change.

      procedure Merge_Timing_Runs;
      --  Try_Merge at every non-binding curved junction, in a forward sweep.

      procedure Try_Merge (Corner : Finishing_Corners_Index) is
         type Saved_Segment is record
            Profile                           : Feedrate_Profile;
            Max_Crackle, Decel_Crackle        : Crackle;
            Window                            : Profile_Window;
            Ends                              : Profile_End_Pair;
            Run_Back, Run_Ahead               : Corners_Index'Base;
            Run_Distance_Offset               : Length;
            Run_Time_Offset, Run_Segment_Time : Time;
            End_Speed                         : Velocity;
         end record;
         subtype Merged_Segments is
           Finishing_Corners_Index
             range Timing_Run_First (Block'Access, Corner) .. Timing_Run_Last (Block'Access, Corner + 1);
         Saved    : constant array (Merged_Segments) of Saved_Segment :=
           [for I in Merged_Segments =>
              (Block.Feedrate_Profiles (I),
               Block.Profile_Crackles (I),
               Block.Profile_Decel_Crackles (I),
               Block.Profile_Windows (I),
               Block.Profile_Ends (I),
               Block.Timing_Run_Back (I),
               Block.Timing_Run_Ahead (I),
               Block.Timing_Distance_Offsets (I),
               Block.Timing_Time_Offsets (I),
               Block.Timing_Segment_Times (I),
               Block.Corner_Velocity_Limits (I))];
         Old_Time : Time := 0.0 * s;
         New_Time : Time := 0.0 * s;
         Result   : Profile_Planning_Result;

         procedure Restore;

         procedure Restore is
         begin
            for I in Saved'Range loop
               Block.Feedrate_Profiles (I) := Saved (I).Profile;
               Block.Profile_Crackles (I) := Saved (I).Max_Crackle;
               Block.Profile_Decel_Crackles (I) := Saved (I).Decel_Crackle;
               Block.Profile_Windows (I) := Saved (I).Window;
               Block.Profile_Ends (I) := Saved (I).Ends;
               Block.Timing_Run_Back (I) := Saved (I).Run_Back;
               Block.Timing_Run_Ahead (I) := Saved (I).Run_Ahead;
               Block.Timing_Distance_Offsets (I) := Saved (I).Run_Distance_Offset;
               Block.Timing_Time_Offsets (I) := Saved (I).Run_Time_Offset;
               Block.Timing_Segment_Times (I) := Saved (I).Run_Segment_Time;
               Block.Corner_Velocity_Limits (I) := Saved (I).End_Speed;
            end loop;
         end Restore;
      begin
         for I in Merged_Segments loop
            Old_Time := @ + Segment_Time (Block'Access, I);
            Block.Timing_Run_Back (I) := I - Merged_Segments'First;
            Block.Timing_Run_Ahead (I) := Merged_Segments'Last - I;
         end loop;
         --  A merged run's middle portion holds every curved interior corner, so a split profile would need cellular
         --  reachability searches there. It rarely decides whether a merge pays, and would dominate the search time.
         My_Feedrate_Profile_Generator.Select_Feedrate_Profile_Window
           (Block'Access,
            Motor_Map,
            Merged_Segments'First,
            Evaluate_Run_Windows (Block'Access, Motor_Map, Merged_Segments'First),
            Result,
            Limit_Scale   => 1.0,
            Split_Profile => False);
         if Result.Valid then
            Assign_Timing_Members (Block'Access, Merged_Segments'First);
            for I in Merged_Segments loop
               New_Time := @ + Segment_Time (Block'Access, I);
            end loop;
            if New_Time < Old_Time then
               return;
            end if;
         end if;
         Restore;
      exception
         when Constraint_Error =>
            Restore;
      end Try_Merge;

      procedure Merge_Timing_Runs is
         Left_Splice  : constant Corners_Index :=
           Block.N_Corners - Corners_Index'Min (Blend_Radius, Block.N_Corners - 1);
         Right_Splice : constant Corners_Index := Corners_Index'Min (Blend_Radius, Block.N_Corners - 1) + 1;
      begin
         if not Block.Params.Continuous_Timing or else Block.Is_Homing_Move then
            return;
         end if;
         for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
            --  Below its static limit, a junction's speed is set by the maximal ramp on one side of it, so it cannot
            --  be a speed minimum. A binding junction is a valley, where a joined run could not dip. Block splice
            --  corners stay junctions so that inter-block blends can still cut there.
            if Is_Timing_Junction (Block, I)
              and then I /= Left_Splice
              and then I /= Right_Splice
              and then Timing_Run_Last (Block'Access, I + 1) - Timing_Run_First (Block'Access, I) < Blend_Radius
              and then
                Block.Corner_Velocity_Limits (I) > Timing_Merge_Minimum_Ratio * Workspace.Static_Corner_Limits (I)
              and then
                Block.Corner_Velocity_Limits (I) < (1.0 - Timing_Merge_Margin) * Workspace.Static_Corner_Limits (I)
              and then Can_Merge_Timing_Runs (Block'Access, I)
            then
               Try_Merge (I);
            end if;
         end loop;
      end Merge_Timing_Runs;
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
         Merge_Timing_Runs;
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
         --  the number of corners. A replanning window ends with the timing run after its last stopped corner.
         Materialize (Timing_Run_Last (Block'Access, I + 1));
         if Transition_Kind (Block.Corner_Transitions (I)) = Stereographic_Transition and then not Stop_Cannot_Pay (I)
         then
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
                  Materialize (Timing_Run_Last (Block'Access, I + 2));
                  if First_Time
                    + Straight_Bound (I + 1, 0.0 * mm / s, 0.0 * mm / s, Primitive_Length (Block'Access, I + 1))
                    + Straight_Bound
                        (I + 2,
                         0.0 * mm / s,
                         Block.Corner_Velocity_Limits (I + 2),
                         Block.Primitive_Start_Distances (I + 2)
                         + Block.Primitive_Distances (I + 2)
                         + Segment_End_Transition_Distance (Block'Access, I + 2))
                    < Run_Time_Through (I) + Segment_Time (Block'Access, I + 1) + Segment_Time (Block'Access, I + 2)
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
      Merge_Timing_Runs;
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

      function Commanded_E_In_Bounds (Block : Execution_Block) return Boolean;
      --  Whether Block's own plan holds its E references to the bounds. As in Corner_Blender and Kinematic_Limiter, a
      --  commanded E outside them (for example after Ignore_Bounds) waives them for the block. Left may be only a
      --  window of its block, which can only hold it to the bounds more often.

      function E_In_Bounds
        (Block : Execution_Block; First, Last : Finishing_Corners_Index; Shift : Length; Checked : Boolean)
         return Boolean;
      --  Whether segments First .. Last stay within the E bounds with their references translated by Shift. Any
      --  finite translation passes unless Checked, but the translated sums are evaluated either way.

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
         Left_Trim   : constant Length :=
           Length'Min
             (Left.Primitive_Distances (Left_Seam),
              Primitive_Length (Geometry'Access, 2) - Geometry.Primitive_Distances (2));
         Right_Trim  : constant Length :=
           Length'Min (Right.Primitive_Distances (2), Geometry.Primitive_Start_Distances (3));
         --  Path the new seam transition takes from each primitive. A transition that takes a whole retained primitive
         --  measures its scratch copy, which can exceed the stored retained length by an ulp.
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

         --  Keep every copied E transition. Only the seam gets a new one, in what the far corners' transitions leave
         --  of its two shortened segments.
         Patch.Extrusion_Transitions (Seam) :=
           Plan_Extrusion_Transition
             (Patch'Access,
              Seam,
              Extrusion_Transition_Reach (Patch'Access, Motor_Map, Seam),
              Segment_Total_Distance (Patch'Access, Seam) - Patch.Extrusion_Transitions (Seam - 1).Half_Width,
              Segment_Total_Distance (Patch'Access, Seam + 1) - Patch.Extrusion_Transitions (Seam + 1).Half_Width);
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

      function Commanded_E_In_Bounds (Block : Execution_Block) return Boolean
      is (for all Corner of Block.Corners =>
            Corner (E_Axis) in Block.Params.Bounds.Lower_E .. Block.Params.Bounds.Upper_E);

      function E_In_Bounds
        (Block : Execution_Block; First, Last : Finishing_Corners_Index; Shift : Length; Checked : Boolean)
         return Boolean
      is
         Lower : constant Length := (if Checked then Block.Params.Bounds.Lower_E else Length'First);
         Upper : constant Length := (if Checked then Block.Params.Bounds.Upper_E else Length'Last);
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
         --  A transition kept beside the seam must still fit in its shortened segment, or E would step at the seam.
         if Patch.Extrusion_Transitions (Seam - 1).Half_Width > Segment_Total_Distance (Patch'Access, Seam)
           or else Patch.Extrusion_Transitions (Seam + 1).Half_Width > Segment_Total_Distance (Patch'Access, Seam + 1)
         then
            return;
         end if;
         loop
            --  The patch is split back into two blocks at the seam, so no timing run may cross it.
            My_Kinematic_Limiter.Run (Patch, Motor_Map, Work'Access, Fixed_Ends => True, Forced_Junction => Seam);
            if Patch.Corner_Velocity_Limits (Seam) <= 0.0 * mm / s then
               return;
            end if;
            My_Feedrate_Profile_Generator.Run (Patch, Motor_Map, Work.Window_Evaluations, Result);
            exit when
              Result.Valid or else Timing_Run_Last (Patch'Access, Result.Failed_Segment) = Result.Failed_Segment;
            --  As in Plan_Fixed_Geometry, split a run that could not be certified.
            for Corner in Result.Failed_Segment .. Timing_Run_Last (Patch'Access, Result.Failed_Segment) - 1 loop
               Work.Timing_Join_Vetoes (Corner) := True;
            end loop;
         end loop;
         if not Result.Valid or else Patch_Time >= Original_Time then
            return;
         end if;

         --  The seam translates references inside the window as well as after it, and the blender only checked its
         --  scratch frame, so certify every replacement segment against the bounds its own block's plan respects.
         --  Right's untouched suffix only moves by Suffix_Shift, which a linear pass certifies without another solve.
         --  This also evaluates every sum the commit assigns.
         Suffix_Shift :=
           Patch.Extrusion_Reference_Positions (Patch.N_Corners) - Right.Extrusion_Reference_Positions (Right_Last);
         declare
            Right_Checked : constant Boolean := Commanded_E_In_Bounds (Right);
         begin
            if not E_In_Bounds (Patch, 2, Seam, 0.0 * mm, Commanded_E_In_Bounds (Left))
              or else not E_In_Bounds (Patch, Seam + 1, Patch.N_Corners, 0.0 * mm, Right_Checked)
              or else not E_In_Bounds (Right, Right_Last + 1, Right.N_Corners, Suffix_Shift, Right_Checked)
            then
               return;
            end if;
         end;
      exception
         when Ada.Assertions.Assertion_Error =>
            --  A failed assertion is a planner defect, not a rejected replacement.
            raise;
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
            Target.Extrusion_Transitions (J) := Source.Extrusion_Transitions (I);
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
               Target.Profile_Decel_Crackles (J) := Source.Profile_Decel_Crackles (I);
               Target.Profile_Windows (J) := Source.Profile_Windows (I);
               Target.Profile_Ends (J) := Source.Profile_Ends (I);
               Target.Timing_Run_Back (J) := Source.Timing_Run_Back (I);
               Target.Timing_Run_Ahead (J) := Source.Timing_Run_Ahead (I);
               Target.Timing_Distance_Offsets (J) := Source.Timing_Distance_Offsets (I);
               Target.Timing_Time_Offsets (J) := Source.Timing_Time_Offsets (I);
               Target.Timing_Segment_Times (J) := Source.Timing_Segment_Times (I);
            end if;
         end;
      end loop;
   end Copy_Window;

   procedure Clear_Planned_Fields (Block : not null access Execution_Block) is
   begin
      Block.Loop_Move_Minimum_Time := 0.0 * s;
      Block.Extrusion_Reference_Positions := [others => 0.0 * mm];
      Block.Extrusion_Densities := [others => 0.0];
      Block.Extrusion_Transitions := [others => <>];
      Block.Primitive_Distances := [others => 0.0 * mm];
      Block.Primitive_Start_Distances := [others => 0.0 * mm];
      Block.Corner_Velocity_Limits := [others => 0.0 * mm / s];
      Block.Feedrate_Profiles :=
        [others => (Accel => [others => 0.0 * s], Coast => 0.0 * s, Decel => [others => 0.0 * s])];
      Block.Profile_Crackles := [others => 0.0 * mm / s**5];
      Block.Profile_Decel_Crackles := [others => 0.0 * mm / s**5];
      Block.Profile_Windows := [others => <>];
      Block.Profile_Ends := [others => <>];
      Block.Timing_Run_Back := [others => 0];
      Block.Timing_Run_Ahead := [others => 0];
      Block.Timing_Distance_Offsets := [others => 0.0 * mm];
      Block.Timing_Time_Offsets := [others => 0.0 * s];
      Block.Timing_Segment_Times := [others => 0.0 * s];
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
      --  Corners before Start are padding, filled only so it holds no unrelated geometry. The segment ending at Start
      --  would also need the part of the transition at First that precedes the window, so no padding segment is a
      --  valid motion and callers evaluate only the segments after Start.
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
       and then Left.Corner_Dwell_Times (Left.N_Corners) = 0.0 * s
       and then Is_Timing_Junction (Left, Left.N_Corners - Corners_Index'Min (Blend_Radius, Left.N_Corners - 1))
       and then Is_Timing_Junction (Right, Corners_Index'Min (Blend_Radius, Right.N_Corners - 1) + 1));

   procedure Reclaim_Retired is
      Pending : Block_Set;
      Error   : Cleanup_Exceptions.Holder;
   begin
      pragma Abort_Defer;
      Block_Ring.Collect_Retired (Pending);
      for Index in Block_Index loop
         if Pending (Index) then
            Cleanup_Block (Index, Error);
         end if;
      end loop;
      Error.Reraise;
   end Reclaim_Retired;

   procedure Finalize_In_Background (Pending : Block_Set);
   --  Finalize slots that Retired_Cleaner took from moving consumers. No caller waits on them, so failures are
   --  deferred to the next Release, which waits for this cleanup before taking them.

   procedure Finalize_In_Background (Pending : Block_Set) is
   begin
      pragma Abort_Defer;
      for Index in Block_Index loop
         if Pending (Index) then
            begin
               Clear_Block_Extra_Data (Index);
            exception
               when E : others =>
                  --  Defer before Finish_Cleanup so a Release waiting on this slot always sees the failure.
                  Block_Ring.Defer_Cleanup_Error (E);
            end;
            Block_Ring.Finish_Cleanup (Index);
         end if;
      end loop;
   end Finalize_In_Background;

   Retirement_Poll_Interval : constant Duration := 0.2;
   --  Moving consumers retire slots with atomic stores, so no protected entry can wake on them. Polling only runs
   --  while a linked or retired slot exists, and Runner's collection requests interrupt it.

   task Retired_Cleaner
     with CPU => Runner_CPU;
   --  Finalizes slots retired while streaming. Keeping these finalizers off Runner means one that waits on the
   --  planner, for example for input queue space, cannot block the only task able to unblock it.

   task body Retired_Cleaner is
      Pending : Block_Set;
      Stop    : Boolean;
   begin
      loop
         Block_Ring.Wait_For_Retirement (Pending, Stop);
         exit when Stop;
         begin
            if (for some Taken of Pending => Taken) then
               Finalize_In_Background (Pending);
            else
               select
                  Block_Ring.Wait_For_Collection_Request;
               or
                  delay Retirement_Poll_Interval;
               end select;
            end if;
         exception
            when E : others =>
               --  Stay alive: later retired slots still need reclaiming for Runner to keep planning.
               Block_Ring.Defer_Cleanup_Error (E);
         end;
      end loop;
   end Retired_Cleaner;

   procedure Prepare_Blend
     (Previous, Following : Block_Index;
      Motor_Map           : Motor_Position_Map;
      Workspace           : not null access Planning_Workspace);
   --  Try to link Following to the published tail block Previous, which Begin_Blend has pinned. Publish Following as
   --  an independently stopped block if the boundary is not blendable or the consumer has already decided.

   procedure Prepare_Blend
     (Previous, Following : Block_Index;
      Motor_Map           : Motor_Position_Map;
      Workspace           : not null access Planning_Workspace)
   is
      Left               : Execution_Block renames Blocks (Previous).Block;
      Right              : Execution_Block renames Blocks (Following).Block;
      Blended, Published : Boolean := False;
   begin
      if Blendable_Boundary (Left, Right) and then Allow_Blended_Entry (Right.Flush_Resetting_Data) then
         declare
            LC         : constant Corners_Index := Corners_Index'Min (Blend_Radius, Left.N_Corners - 1) + 1;
            RC         : constant Corners_Index := Corners_Index'Min (Blend_Radius, Right.N_Corners - 1) + 1;
            Left_Copy  : aliased Execution_Block (LC);
            Right_Head : aliased Execution_Block (RC);
            Suffix     : Block_Segment_Lengths renames
              Workspace.Unblended_Extrusion_Positions (RC + 1 .. Right.N_Corners);
         begin
            Copy_Motion_Window (Left, Left.N_Corners - LC + 1, Left.N_Corners, Left_Copy);
            Copy_Motion_Window (Right, 1, RC, Right_Head);
            Suffix := Right.Extrusion_Reference_Positions (Suffix'Range);
            Blend_Blocks (Left_Copy, Right, Motor_Map, Blended);
            if Blended then
               Copy_Motion_Window (Left_Copy, 1, LC, Blocks (Previous).Tail);
            end if;
            Block_Ring.Finish_Blend (Previous, Following, Blended, Published);
            if Blended and then not Published then
               --  The consumer crossed the decision point during planning. Restore the unpublished successor exactly,
               --  including suffix E references (subtracting their translation would introduce rounding drift).
               Copy_Window (Right_Head, 1, RC, Right, 1);
               Right.Extrusion_Reference_Positions (Suffix'Range) := Suffix;
            end if;
         end;
      else
         Block_Ring.Finish_Blend (Previous, Following, False, Published);
      end if;
      if not Published then
         Block_Ring.Publish (Following);
      end if;
   end Prepare_Blend;

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
         accept Setup
           (In_Params         : Kinematic_Parameters;
            In_Motor_Map      : Motor_Position_Map;
            In_Start_Position : Position := Initial_Position)
         do
            if not Tail_Atomics.Is_Lock_Free (Blocks (Block_Index'First).Choice) then
               raise Program_Error with "Inter-block publication requires lock-free atomic exchange.";
            end if;
            My_Preprocessor.Setup (In_Params, In_Start_Position);
            Current_Motor_Map := In_Motor_Map;
            Next_Block_Start := In_Start_Position;
            Block_Ring.Setup;
         end Setup;

         Planning_Loop : loop
            --  Retired_Cleaner, Release and reset all return slots through Block_Ring, which opens this barrier. Hand
            --  slots retired since the last block to the cleaner now, so a full ring need not wait for its next poll.
            Block_Ring.Request_Collection;
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

               declare
                  Previous  : Block_Index;
                  Available : Boolean;
               begin
                  Block_Ring.Begin_Blend (Previous, Available);
                  if Available then
                     Prepare_Blend (Previous, Index, Current_Motor_Map, Workspace);
                  else
                     Block_Ring.Publish (Index);
                  end if;
               end;
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
         Result (E_Axis) := Extrusion_At_Distance (Block, Finishing_Corner, D);
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
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index; Run_Distance : Length)
      return Time
   is
      Window : constant Profile_Window := Block.Profile_Windows (Corner);
   begin
      return
        (if Block.Profile_Ends (Corner).Enabled
         then Total_Time (Block.Profile_Ends (Corner).Suffix.Profile)
         else
           Constant_Speed_Time
             (Run_Distance - Window.Start_Distance - Window.Distance,
              Block.Corner_Velocity_Limits (Timing_Run_Last (Block, Corner))));
   end Segment_Suffix_Time;

   function Timing_Motion_Time
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Time
   is (Prepare_Timing_Evaluation (Block, Run_First).Motion_Time);

   function Timing_Evaluation_For
     (Block         : not null access constant Execution_Block;
      Run_First     : Finishing_Corners_Index;
      Run_Distance  : Length;
      With_Ramp_End : Boolean) return Timing_Evaluation;
   --  Prepare_Timing_Evaluation for a run whose distance is already known.

   function Timing_Evaluation_For
     (Block         : not null access constant Execution_Block;
      Run_First     : Finishing_Corners_Index;
      Run_Distance  : Length;
      With_Ramp_End : Boolean) return Timing_Evaluation
   is
      Prefix  : constant Time := Segment_Prefix_Time (Block, Run_First);
      Profile : constant Time := Total_Time (Block.Feedrate_Profiles (Run_First));
      Suffix  : constant Time := Segment_Suffix_Time (Block, Run_First, Run_Distance);
      Motion  : constant Time := Prefix + Profile + Suffix;
   begin
      if not With_Ramp_End then
         return
           (Has_Ramp_End   => False,
            Prefix_Time    => Prefix,
            Profile_Time   => Profile,
            Suffix_Time    => Suffix,
            Motion_Time    => Motion,
            Total_Distance => Run_Distance);
      end if;
      return
        (Has_Ramp_End   => True,
         Prefix_Time    => Prefix,
         Profile_Time   => Profile,
         Suffix_Time    => Suffix,
         Motion_Time    => Motion,
         Total_Distance => Run_Distance,
         Ramp_End       =>
           Accel_Ramp_End_Of
             (Block.Feedrate_Profiles (Run_First),
              Block.Profile_Crackles (Run_First),
              Central_Profile_Start_Velocity (Block, Run_First)));
   end Timing_Evaluation_For;

   function Prepare_Timing_Evaluation
     (Block         : not null access constant Execution_Block;
      Run_First     : Finishing_Corners_Index;
      With_Ramp_End : Boolean := False) return Timing_Evaluation
   is (Timing_Evaluation_For (Block, Run_First, Timing_Total_Distance (Block, Run_First), With_Ramp_End));

   function Assigned_Timing_Evaluation
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index) return Timing_Evaluation
   is
      Run_Last : constant Finishing_Corners_Index := Timing_Run_Last (Block, Run_First);
      Distance : constant Length :=
        (if Run_Last = Run_First
         then Segment_Total_Distance (Block, Run_First)
         else Block.Timing_Distance_Offsets (Run_Last) + Segment_Total_Distance (Block, Run_Last));
      --  The member offsets accumulate from zero in Timing_Total_Distance's order, so this is its sum exactly.
   begin
      pragma
        Assert
          (Distance = Timing_Total_Distance (Block, Run_First),
           "Timing run members were not assigned for the current geometry");
      return Timing_Evaluation_For (Block, Run_First, Distance, With_Ramp_End => False);
   end Assigned_Timing_Evaluation;

   function Timing_Distance_At_Time
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index; Run_Time : Time)
      return Length
   is (Timing_Distance_At_Time (Block, Run_First, Run_Time, Prepare_Timing_Evaluation (Block, Run_First)));

   function Timing_Distance_At_Time
     (Block     : not null access constant Execution_Block;
      Run_First : Finishing_Corners_Index;
      Run_Time  : Time;
      Timing    : Timing_Evaluation) return Length
   is
      Window       : constant Profile_Window := Selected_Profile_Window (Block, Run_First);
      Ends         : constant Profile_End_Pair := Block.Profile_Ends (Run_First);
      Prefix_Time  : constant Time := Timing.Prefix_Time;
      Profile_Time : constant Time := Timing.Profile_Time;
      Suffix_Time  : constant Time := Timing.Suffix_Time;
      Motion_Time  : constant Time := Timing.Motion_Time;
   begin
      if Run_Time >= Motion_Time then
         return Timing.Total_Distance;
      elsif Run_Time < Prefix_Time then
         return
           (if Ends.Enabled
            then
              Profile_Distance_At_Time
                (Ends.Prefix.Profile,
                 Run_Time,
                 Ends.Prefix.Max_Crackle,
                 Ends.Prefix.Decel_Crackle,
                 Block.Corner_Velocity_Limits (Run_First - 1))
            else Block.Corner_Velocity_Limits (Run_First - 1) * Run_Time);
      elsif Run_Time < Prefix_Time + Profile_Time then
         declare
            Profile : constant Feedrate_Profile := Block.Feedrate_Profiles (Run_First);
            T       : constant Time := Time'Min (Run_Time - Prefix_Time, Profile_Time);
         begin
            return
              Window.Start_Distance
              + (if Timing.Has_Ramp_End and then T > Timing.Ramp_End.Accel_Time
                 then Distance_After_Accel_Ramp (Profile, T, Block.Profile_Decel_Crackles (Run_First), Timing.Ramp_End)
                 else
                   Profile_Distance_At_Time
                     (Profile,
                      T,
                      Block.Profile_Crackles (Run_First),
                      Block.Profile_Decel_Crackles (Run_First),
                      Central_Profile_Start_Velocity (Block, Run_First)));
         end;
      else
         return
           Window.Start_Distance
           + Window.Distance
           + (if Ends.Enabled
              then
                Profile_Distance_At_Time
                  (Ends.Suffix.Profile,
                   Time'Min (Run_Time - (Prefix_Time + Profile_Time), Suffix_Time),
                   Ends.Suffix.Max_Crackle,
                   Ends.Suffix.Decel_Crackle,
                   Ends.Suffix.Boundary_Velocity)
              else
                Block.Corner_Velocity_Limits (Timing_Run_Last (Block, Run_First))
                * Time'Min (Run_Time - (Prefix_Time + Profile_Time), Suffix_Time));
      end if;
   end Timing_Distance_At_Time;

   function Timing_Velocity_At_Time
     (Block : not null access constant Execution_Block; Run_First : Finishing_Corners_Index; Run_Time : Time)
      return Velocity
   is (Timing_Velocity_At_Time (Block, Run_First, Run_Time, Prepare_Timing_Evaluation (Block, Run_First)));

   function Timing_Velocity_At_Time
     (Block     : not null access constant Execution_Block;
      Run_First : Finishing_Corners_Index;
      Run_Time  : Time;
      Timing    : Timing_Evaluation) return Velocity
   is
      Ends         : constant Profile_End_Pair := Block.Profile_Ends (Run_First);
      Prefix_Time  : constant Time := Timing.Prefix_Time;
      Profile_Time : constant Time := Timing.Profile_Time;
      Suffix_Time  : constant Time := Timing.Suffix_Time;
   begin
      if Run_Time < Prefix_Time then
         return
           (if Ends.Enabled
            then
              Profile_Velocity_At_Time
                (Ends.Prefix.Profile,
                 Run_Time,
                 Ends.Prefix.Max_Crackle,
                 Ends.Prefix.Decel_Crackle,
                 Block.Corner_Velocity_Limits (Run_First - 1))
            else Block.Corner_Velocity_Limits (Run_First - 1));
      elsif Run_Time < Prefix_Time + Profile_Time then
         declare
            Profile : constant Feedrate_Profile := Block.Feedrate_Profiles (Run_First);
            T       : constant Time := Time'Min (Run_Time - Prefix_Time, Profile_Time);
         begin
            return
              (if Timing.Has_Ramp_End and then T > Timing.Ramp_End.Accel_Time
               then
                 Velocity_After_Accel_Ramp
                   (Profile,
                    T,
                    Block.Profile_Decel_Crackles (Run_First),
                    Timing.Ramp_End.Accel_Time,
                    Timing.Ramp_End.Mid_Vel)
               else
                 Profile_Velocity_At_Time
                   (Profile,
                    T,
                    Block.Profile_Crackles (Run_First),
                    Block.Profile_Decel_Crackles (Run_First),
                    Central_Profile_Start_Velocity (Block, Run_First)));
         end;
      else
         return
           (if Ends.Enabled
            then
              Profile_Velocity_At_Time
                (Ends.Suffix.Profile,
                 Time'Min (Run_Time - (Prefix_Time + Profile_Time), Suffix_Time),
                 Ends.Suffix.Max_Crackle,
                 Ends.Suffix.Decel_Crackle,
                 Ends.Suffix.Boundary_Velocity)
            else Block.Corner_Velocity_Limits (Timing_Run_Last (Block, Run_First)));
      end if;
   end Timing_Velocity_At_Time;

   procedure Assign_Timing_Members (Block : not null access Execution_Block; Run_First : Finishing_Corners_Index) is
      Run_Last    : constant Finishing_Corners_Index := Timing_Run_Last (Block, Run_First);
      Timing      : constant Timing_Evaluation := Prepare_Timing_Evaluation (Block, Run_First, With_Ramp_End => True);
      --  Most member boundaries of a long run lie beyond its acceleration ramp, so bisect from its prepared end.
      Motion_Time : constant Time := Timing.Motion_Time;
      Distance    : Length := 0.0 * mm;
      Start_Time  : Time := 0.0 * s;

      function Time_At_Distance (Target : Length) return Time;
      --  Run distance never decreases with time, so bisect until the bracket cannot be split any further.

      function Time_At_Distance (Target : Length) return Time is
         Lower : Time := Start_Time;
         Upper : Time := Motion_Time;
         Mid   : Time;
      begin
         loop
            Mid := Lower + 0.5 * (Upper - Lower);
            exit when Mid = Lower or else Mid = Upper;
            if Timing_Distance_At_Time (Block, Run_First, Mid, Timing) < Target then
               Lower := Mid;
            else
               Upper := Mid;
            end if;
         end loop;
         return Upper;
      end Time_At_Distance;
   begin
      for I in Run_First .. Run_Last loop
         Block.Timing_Distance_Offsets (I) := Distance;
         Block.Timing_Time_Offsets (I) := Start_Time;
         if I = Run_Last then
            Block.Timing_Segment_Times (I) :=
              Time'Max (0.0 * s, Motion_Time - Start_Time) + Block.Corner_Dwell_Times (I);
         else
            Distance := Distance + Segment_Total_Distance (Block, I);
            Block.Timing_Segment_Times (I) := Time_At_Distance (Distance) - Start_Time;
            Start_Time := Start_Time + Block.Timing_Segment_Times (I);
            --  Interior corners are not junctions. Record the speed there for consumers of corner speeds.
            Block.Corner_Velocity_Limits (I) := Timing_Velocity_At_Time (Block, Run_First, Start_Time, Timing);
         end if;
      end loop;
   end Assign_Timing_Members;

   function Segment_Time
     (Block : not null access constant Execution_Block; Finishing_Corner : Corners_Index) return Time is
   begin
      if Block.Timing_Run_Back (Finishing_Corner) /= 0 or else Block.Timing_Run_Ahead (Finishing_Corner) /= 0 then
         return Block.Timing_Segment_Times (Finishing_Corner);
      else
         return Timing_Motion_Time (Block, Finishing_Corner) + Block.Corner_Dwell_Times (Finishing_Corner);
      end if;
   end Segment_Time;

   function Central_Profile_Start_Velocity
     (Block : not null access constant Execution_Block; Corner : Finishing_Corners_Index) return Velocity is
   begin
      return
        (if Block.Profile_Ends (Corner).Enabled
         then Block.Profile_Ends (Corner).Prefix.Boundary_Velocity
         else Block.Corner_Velocity_Limits (Corner - 1));
   end Central_Profile_Start_Velocity;

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
      Run_First  : constant Finishing_Corners_Index := Timing_Run_First (Block, Finishing_Corner);
      Merged     : constant Boolean := Run_First /= Timing_Run_Last (Block, Finishing_Corner);
      Run_Time   : constant Time :=
        (if Merged then Block.Timing_Time_Offsets (Finishing_Corner) + Time_Into_Segment else Time_Into_Segment);
      Run_Offset : constant Length := (if Merged then Block.Timing_Distance_Offsets (Finishing_Corner) else 0.0 * mm);
      Timing     : constant Timing_Evaluation := Assigned_Timing_Evaluation (Block, Run_First);
   begin
      if Finishing_Corner = Block.N_Corners
        and then Run_Time >= Timing.Motion_Time
        and then Block.Corner_Velocity_Limits (Finishing_Corner) = 0.0 * mm / s
      then
         --  Reconstructing the endpoint from direction and distance can leave a rounding residual. Shaper draining
         --  and pause/return plans need the exact stopped position, including any path-length extrusion correction.
         return
           [Block.Corners (Finishing_Corner) with delta
              E_Axis => Block.Extrusion_Reference_Positions (Finishing_Corner)];
      end if;
      return
        Point_At_Segment_Distance
          (Block, Finishing_Corner, Timing_Distance_At_Time (Block, Run_First, Run_Time, Timing) - Run_Offset);
   end Segment_Pos_At_Time;

   function Segment_Vel_Ratio_At_Time
     (Block             : not null access constant Execution_Block;
      Finishing_Corner  : Finishing_Corners_Index;
      Time_Into_Segment : Time) return Dimensionless
   is
      Run_First : constant Finishing_Corners_Index := Timing_Run_First (Block, Finishing_Corner);
      Run_Time  : constant Time :=
        (if Run_First /= Timing_Run_Last (Block, Finishing_Corner)
         then Block.Timing_Time_Offsets (Finishing_Corner) + Time_Into_Segment
         else Time_Into_Segment);
      Timing    : constant Timing_Evaluation := Assigned_Timing_Evaluation (Block, Run_First);
   begin
      if Run_Time > Timing.Motion_Time then
         --  Return 1.0 inside dwell parts so the laser can be set to the programmed power level.
         return 1.0;
      end if;
      return
        Velocity'Max (0.0 * mm / s, Timing_Velocity_At_Time (Block, Run_First, Run_Time, Timing))
        / Block.Original_Segment_Feedrates (Finishing_Corner);
   end Segment_Vel_Ratio_At_Time;

   function Next_Block_Pos (Block : not null access constant Execution_Block) return Position is
   begin
      return Block.Next_Block_Pos;
   end Next_Block_Pos;

   function Block_Start_Pos (Block : not null access constant Execution_Block) return Position is
   begin
      if Block.N_Corners >= 2 and then Block.Corner_Velocity_Limits (1) > 0.0 * mm / s then
         return Segment_Pos_At_Time (Block, 2, 0.0 * s);
      end if;
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
