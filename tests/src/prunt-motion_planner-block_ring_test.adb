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

with Ada.Assertions;
with Ada.Finalization;
with Ada.Task_Identification;
with Prunt.Motion_Planner.Planner;

package body Prunt.Motion_Planner.Block_Ring_Test is

   pragma Extensions_Allowed (On);

   function Block_Allows_Blended_Entry (Data : Integer) return Boolean is
      pragma Unreferenced (Data);
   begin
      return True;
   end Block_Allows_Blended_Entry;

   procedure Test_Block_Ring_Cleanup (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         protected Gate is
            procedure Arm (Value : Positive);
            procedure Enter_Finalizer (Value : Natural; Wait : out Boolean);
            entry Wait_For_Finalizer;
            entry Continue_Finalizer;
            procedure Open;
            procedure Finish;
            function Finished return Boolean;
         private
            Target                            : Natural := 0;
            Started, Can_Finish, Has_Finished : Boolean := False;
         end Gate;

         protected body Gate is
            procedure Arm (Value : Positive) is
            begin
               Target := Value;
               Started := False;
               Can_Finish := False;
               Has_Finished := False;
            end Arm;

            procedure Enter_Finalizer (Value : Natural; Wait : out Boolean) is
            begin
               Wait := Value /= 0 and then Value = Target;
               if Wait then
                  Target := 0;
                  Started := True;
               end if;
            end Enter_Finalizer;

            entry Wait_For_Finalizer when Started is
            begin
               null;
            end Wait_For_Finalizer;

            entry Continue_Finalizer when Can_Finish is
            begin
               null;
            end Continue_Finalizer;

            procedure Finish is
            begin
               Has_Finished := True;
            end Finish;

            function Finished return Boolean
            is (Has_Finished);

            procedure Open is
            begin
               Can_Finish := True;
            end Open;
         end Gate;

         type Data is new Ada.Finalization.Controlled with record
            Value : Natural := 0;
         end record;
         overriding
         procedure Finalize (Object : in out Data);

         overriding
         procedure Finalize (Object : in out Data) is
            Wait : Boolean;
         begin
            Gate.Enter_Finalizer (Object.Value, Wait);
            if Wait then
               --  Bound the wait so a lock regression fails instead of hanging the test's cleanup.
               select
                  Gate.Continue_Finalizer;
               or
                  delay 5.0;
               end select;
               Gate.Finish;
            end if;
         end Finalize;

         type Motor_Name is (Motor);
         type Motor_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Limits is array (Motor_Name) of Dimensionless;

         function Block_Allows_Blended_Entry (Value : Data) return Boolean is
            pragma Unreferenced (Value);
         begin
            return True;
         end Block_Allows_Blended_Entry;

         Empty_Data : constant Data := (Ada.Finalization.Controlled with Value => 0);
         package Planner is new
           Motion_Planner.Planner
             (Motor_Name,
              Motor_Map,
              Motor_Limits,
              [others => 1.0],
              Data,
              Empty_Data,
              Data,
              0.005 * s,
              1.0 * s,
              0.001 * s,
              Max_Corners                    => 4,
              Max_Corners_Extra_Data_Count   => 4,
              Max_Corners_Extra_Data_Storage => 512,
              Input_Queue_Length             => 16,
              Allow_Blended_Entry            => Block_Allows_Blended_Entry);

         Blocks               : array (1 .. Planner.Block_Capacity) of Planner.Block_Handle;
         Spare                : Planner.Block_Handle;
         Params               : Kinematic_Parameters := (others => <>);
         Timed_Out, Succeeded : Boolean;

         task Worker is
            entry Start (Discard : Boolean);
            entry Done (Succeeded : out Boolean);
         end Worker;

         task body Worker is
            Discard   : Boolean;
            Succeeded : Boolean;
         begin
            loop
               select
                  accept Start (Discard : Boolean) do
                     Worker.Discard := Discard;
                  end Start;
               or
                  terminate;
               end select;
               Succeeded := True;
               begin
                  if Discard then
                     Planner.Reset;
                  else
                     Planner.Release (Blocks (1));
                  end if;
               exception
                  when others =>
                     Succeeded := False;
               end;
               accept Done (Succeeded : out Boolean) do
                  Succeeded := Worker.Succeeded;
               end Done;
            end loop;
         end Worker;

         procedure Reset is
         begin
            select
               delay 5.0;
               T.Fail ("Reset must finish while a borrowed slot is being cleaned");
            then abort
               Planner.Reset;
            end select;
         end Reset;

         procedure Take (Block : in out Planner.Block_Handle) is
         begin
            Planner.Dequeue (Block, Timed_Out);
            T.Assert
              (not Timed_Out and then not Planner.Is_Empty (Block), "Planning and dequeue proceed during cleanup");
         end Take;

         procedure Wait_For_Finalizer is
         begin
            select
               Gate.Wait_For_Finalizer;
            or
               delay 5.0;
               T.Fail ("Block cleanup must enter the controlled data finalizer");
            end select;
         end Wait_For_Finalizer;

      begin
         Params.Bounds :=
           (Kind                                  => Rectangular_Workspace,
            Lower_X | Lower_Y | Lower_Z | Lower_E => -100.0 * mm,
            Upper_X | Upper_Y | Upper_Z | Upper_E => 100.0 * mm);
         Params.Tangential_Velocity_Max := 100.0 * mm / s;
         Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
         Params.Axial_Acceleration_Maxes := [others => 1_000.0 * mm / s**2];
         Params.Axial_Jerk_Maxes := [others => 10_000.0 * mm / s**3];
         Params.Axial_Snap_Maxes := [others => 100_000.0 * mm / s**4];
         Params.Axial_Crackle_Maxes := [others => 1_000_000.0 * mm / s**5];
         Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);

         for Value in 1 .. 2 loop
            if Value = 1 then
               Planner.Enqueue_Corner_Extra_Data ((Ada.Finalization.Controlled with Value => Value));
               Planner.Enqueue_Flush (Empty_Data);
            else
               Planner.Enqueue_Flush ((Ada.Finalization.Controlled with Value => Value));
            end if;
            Take (Blocks (1));
            Planner.Enqueue_Flush (Empty_Data);
            Gate.Arm (Value);
            Worker.Start (False);
            Wait_For_Finalizer;
            Take (Blocks (2));
            for I in 3 .. Blocks'Last loop
               Planner.Enqueue_Flush (Empty_Data);
               Take (Blocks (I));
            end loop;
            T.Assert (not Gate.Finished, "Corner and flush finalizers run without holding the ring lock");

            Reset;
            T.Assert (not Gate.Finished, "Reset preserves a slot whose borrower is still finalizing it");
            Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
            Planner.Enqueue_Flush (Empty_Data);
            Planner.Dequeue (Spare, Timed_Out);
            T.Assert (Timed_Out and then Planner.Is_Empty (Spare), "A cleaning slot cannot be reused across reset");
            Gate.Open;
            Worker.Done (Succeeded);
            T.Assert (Succeeded, "Release finishes successfully");
            Take (Blocks (1));
            for Block of Blocks loop
               Planner.Release (Block);
            end loop;
         end loop;

         Planner.Enqueue_Flush (Empty_Data);
         Take (Blocks (1));
         Planner.Enqueue_Move ([X_Axis => 10.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         Planner.Enqueue_Corner_Extra_Data ((Ada.Finalization.Controlled with Value => 3));
         declare
            Tail : constant Position_Offset := Planner.Enqueue_Homing_Flush (Empty_Data);
            pragma Unreferenced (Tail);
         begin
            --  The homing handshake proves the data is in the planning slot, not still in the input queue.
            Gate.Arm (3);
            Worker.Start (True);
            Wait_For_Finalizer;
            Planner.Release (Blocks (1));
            T.Assert (not Gate.Finished, "Discard finalizers do not prevent another borrower from releasing");
            Gate.Open;
            Worker.Done (Succeeded);
            T.Assert (Succeeded, "Discard finishes successfully");
         end;
         abort Planner.Runner;
      exception
         when others =>
            Gate.Open;
            abort Worker;
            begin
               select
                  delay 5.0;
               then abort
                  Planner.Reset;
               end select;
            exception
               when Tasking_Error =>
                  null;
            end;
            abort Planner.Runner;
            for Block of Blocks loop
               Planner.Release (Block);
            end loop;
            Planner.Release (Spare);
            raise;
      end;
   end Test_Block_Ring_Cleanup;

   procedure Test_Block_Ring_Cleanup_Exceptions (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         type Counts is array (Positive range 1 .. 3) of Natural;
         protected Faults is
            procedure Arm (Mode : Positive);
            procedure Finalized (Value : Positive; Fail : out Boolean);
            procedure Snapshot (Result : out Counts);
         private
            Active_Mode   : Natural := 0;
            Finalizations : Counts := [others => 0];
         end Faults;

         protected body Faults is
            procedure Arm (Mode : Positive) is
            begin
               Active_Mode := Mode;
               Finalizations := [others => 0];
            end Arm;

            procedure Finalized (Value : Positive; Fail : out Boolean) is
            begin
               Fail := False;
               if Active_Mode /= 0 then
                  Finalizations (Value) := @ + 1;
                  Fail := Active_Mode = 3 or else (if Active_Mode = 1 then Value < 3 else Value = 3);
               end if;
            end Finalized;

            procedure Snapshot (Result : out Counts) is
            begin
               Result := Finalizations;
               Active_Mode := 0;
            end Snapshot;
         end Faults;

         type Data is new Ada.Finalization.Controlled with record
            Value : Natural := 0;
         end record;
         overriding
         procedure Finalize (Object : in out Data);

         overriding
         procedure Finalize (Object : in out Data) is
            Value : constant Natural := Object.Value;
            Fail  : Boolean;
         begin
            Object.Value := 0;
            if Value /= 0 then
               Faults.Finalized (Value, Fail);
               if Fail then
                  raise Constraint_Error with "Injected block data finalizer failure";
               end if;
            end if;
         end Finalize;

         type Motor_Name is (Motor);
         type Motor_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Limits is array (Motor_Name) of Dimensionless;

         function Block_Allows_Blended_Entry (Value : Data) return Boolean is
            pragma Unreferenced (Value);
         begin
            return True;
         end Block_Allows_Blended_Entry;

         Empty_Data : constant Data := (Ada.Finalization.Controlled with Value => 0);
         package Planner is new
           Motion_Planner.Planner
             (Motor_Name,
              Motor_Map,
              Motor_Limits,
              [others => 1.0],
              Data,
              Empty_Data,
              Data,
              0.005 * s,
              1.0 * s,
              0.001 * s,
              Max_Corners                    => 4,
              Max_Corners_Extra_Data_Count   => 4,
              Max_Corners_Extra_Data_Storage => 512,
              Input_Queue_Length             => 16,
              Allow_Blended_Entry            => Block_Allows_Blended_Entry);

         Block         : Planner.Block_Handle;
         Blocks        : array (1 .. Planner.Block_Capacity) of Planner.Block_Handle;
         Params        : Kinematic_Parameters := (others => <>);
         Finalizations : Counts;
         Timed_Out     : Boolean;

         procedure Take (Block : in out Planner.Block_Handle) is
         begin
            Planner.Dequeue (Block, Timed_Out);
            T.Assert
              (not Timed_Out and then not Planner.Is_Empty (Block),
               "Every ring slot remains available after cleanup errors");
         end Take;
      begin
         Params.Bounds :=
           (Kind                                  => Rectangular_Workspace,
            Lower_X | Lower_Y | Lower_Z | Lower_E => -100.0 * mm,
            Upper_X | Upper_Y | Upper_Z | Upper_E => 100.0 * mm);
         Params.Tangential_Velocity_Max := 100.0 * mm / s;
         Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
         Params.Axial_Acceleration_Maxes := [others => 1_000.0 * mm / s**2];
         Params.Axial_Jerk_Maxes := [others => 10_000.0 * mm / s**3];
         Params.Axial_Snap_Maxes := [others => 100_000.0 * mm / s**4];
         Params.Axial_Crackle_Maxes := [others => 1_000_000.0 * mm / s**5];
         Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);

         for Mode in 1 .. 3 loop
            for Value in 1 .. 2 loop
               Planner.Enqueue_Corner_Extra_Data ((Ada.Finalization.Controlled with Value => Value));
            end loop;
            Planner.Enqueue_Flush ((Ada.Finalization.Controlled with Value => 3));
            Take (Block);
            Faults.Arm (Mode);
            begin
               Planner.Release (Block);
               T.Fail ("Release must propagate the finalizer failure");
            exception
               when Program_Error =>
                  null;
            end;
            Faults.Snapshot (Finalizations);
            T.Assert (Planner.Is_Empty (Block), "Exceptional release must clear the caller's handle");
            T.Assert (Finalizations = Counts'[others => 1], "All corner and flush data is finalized despite errors");
            Planner.Release (Block);

            Planner.Reset;
            Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
            for I in Blocks'Range loop
               Planner.Enqueue_Flush (Empty_Data);
            end loop;
            for B of Blocks loop
               Take (B);
            end loop;
            for B of Blocks loop
               Planner.Release (B);
            end loop;
         end loop;

         --  Automatic cleanup must finalize all handles even if one data finalizer raises.
         begin
            declare
               Owned : array (Counts'Range) of Planner.Block_Handle;
            begin
               for I in Owned'Range loop
                  Planner.Enqueue_Corner_Extra_Data ((Ada.Finalization.Controlled with Value => I));
                  Planner.Enqueue_Flush (Empty_Data);
                  Take (Owned (I));
               end loop;
               Faults.Arm (3);
            end;
            T.Fail ("Handle finalization must propagate the cleanup failure");
         exception
            when Program_Error =>
               null;
         end;
         Faults.Snapshot (Finalizations);
         T.Assert (Finalizations = Counts'[others => 1], "Automatic cleanup visits every owned slot despite errors");
         for B of Blocks loop
            Planner.Enqueue_Flush (Empty_Data);
            Take (B);
         end loop;
         for B of Blocks loop
            Planner.Release (B);
         end loop;

         --  Also fail while discarding two ready slots and one planning slot. The homing handshake establishes
         --  that all three blocks have consumed their corner data before the faults are armed.
         for Value in 1 .. 2 loop
            Planner.Enqueue_Corner_Extra_Data ((Ada.Finalization.Controlled with Value => Value));
            Planner.Enqueue_Flush (Empty_Data);
         end loop;
         Planner.Enqueue_Move ([X_Axis => 10.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         Planner.Enqueue_Corner_Extra_Data ((Ada.Finalization.Controlled with Value => 3));
         declare
            Tail : constant Position_Offset := Planner.Enqueue_Homing_Flush (Empty_Data);
            pragma Unreferenced (Tail);
         begin
            Faults.Arm (3);
            begin
               Planner.Reset;
               T.Fail ("Discard must propagate the finalizer failure");
            exception
               when Program_Error =>
                  null;
            end;
            Faults.Snapshot (Finalizations);
            T.Assert (Finalizations = Counts'[others => 1], "Discard cleans every reserved slot before raising");
         end;
         select
            Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
         or
            delay 5.0;
            T.Fail ("A failed reset must leave the planner accepting Setup");
         end select;
         for I in Blocks'Range loop
            Planner.Enqueue_Flush (Empty_Data);
         end loop;
         for B of Blocks loop
            Take (B);
         end loop;
         for B of Blocks loop
            Planner.Release (B);
         end loop;
         Planner.Reset;
         abort Planner.Runner;
      exception
         when others =>
            Faults.Snapshot (Finalizations);
            --  A regression may leave a handle in Cleaning. Do not let a second release failure prevent the
            --  runner from being stopped while unwinding the failed assertion.
            begin
               Planner.Release (Block);
            exception
               when others =>
                  null;
            end;
            for B of Blocks loop
               begin
                  Planner.Release (B);
               exception
                  when others =>
                     null;
               end;
            end loop;
            begin
               select
                  delay 5.0;
               then abort
                  Planner.Reset;
               end select;
            exception
               when others =>
                  null;
            end;
            abort Planner.Runner;
            raise;
      end;
   end Test_Block_Ring_Cleanup_Exceptions;

   procedure Test_Block_Ring_Lifetime (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         type Motor_Name is (Motor);
         type Motor_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Limits is array (Motor_Name) of Dimensionless;
         package Planner is new
           Motion_Planner.Planner
             (Motor_Name,
              Motor_Map,
              Motor_Limits,
              [others => 1.0],
              Integer,
              0,
              Integer,
              0.005 * s,
              1.0 * s,
              0.001 * s,
              Max_Corners                       => 4,
              Max_Corners_Extra_Data_Count      => 2,
              Max_Corners_Extra_Data_Storage    => 128,
              Max_Corners_Extra_Data_Per_Corner => 2,
              Input_Queue_Length                => 64,
              Allow_Blended_Entry               => Block_Allows_Blended_Entry);

         use type Planner.Execution_Block_Kind;
         use type Planner.Corners_Index;

         Blocks               : array (1 .. Planner.Block_Capacity) of Planner.Block_Handle;
         Spare                : Planner.Block_Handle;
         Params               : Kinematic_Parameters := (others => <>);
         Timed_Out            : Boolean;
         Seen_Count, Seen_Sum : Natural := 0;
         Last_ID              : Planner_Corner_ID := 0;

         procedure Check_Data (Block : Planner.Block_Handle; Count, Sum : Natural);
         procedure Enqueue_Block (Value : Positive);
         procedure Reset;
         procedure Setup;
         procedure Stop_Runner;
         procedure Take (Block : in out Planner.Block_Handle);
         procedure Visit (Data : in out Integer);

         procedure Check_Data (Block : Planner.Block_Handle; Count, Sum : Natural) is
         begin
            Seen_Count := 0;
            Seen_Sum := 0;
            for Corner in 1 .. Planner.Corner_Count (Block) loop
               Planner.Corner_Extra_Data (Block, Corner, Visit'Access);
            end loop;
            T.Assert (Seen_Count = Count and then Seen_Sum = Sum, "Only this block's extra data is retained");
         end Check_Data;

         procedure Enqueue_Block (Value : Positive) is
         begin
            Planner.Enqueue_Move ([X_Axis => Dimensionless (Value) * mm, others => 0.0 * mm], 10.0 * mm / s);
            Planner.Enqueue_Corner_Extra_Data (Value);
            Planner.Enqueue_Flush (Value);
         end Enqueue_Block;

         procedure Reset is
         begin
            select
               delay 5.0;
               T.Fail ("Reset must wake a planner waiting for a free ring slot or input");
            then abort
               Planner.Reset;
            end select;
         end Reset;

         procedure Setup is
         begin
            Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
         end Setup;

         procedure Stop_Runner is
         begin
            --  Reset wakes a task blocked inside protected preprocessing. Its rendezvous may then wait forever
            --  if the task was already reset, so cleanup must also work while it is waiting for Setup or has died.
            begin
               select
                  delay 5.0;
               then abort
                  Planner.Reset;
               end select;
            exception
               when Tasking_Error =>
                  null;
            end;
            abort Planner.Runner;
            for Block of Blocks loop
               Planner.Release (Block);
            end loop;
            Planner.Release (Spare);
         end Stop_Runner;

         procedure Take (Block : in out Planner.Block_Handle) is
         begin
            for Attempt in 1 .. 5 loop
               Planner.Dequeue (Block, Timed_Out);
               exit when not Timed_Out;
            end loop;
            T.Assert (not Timed_Out and then not Planner.Is_Empty (Block), "A completed block must be available");
         end Take;

         procedure Visit (Data : in out Integer) is
         begin
            Seen_Count := @ + 1;
            Seen_Sum := @ + Data;
         end Visit;
      begin
         Params.Bounds :=
           (Kind                                  => Rectangular_Workspace,
            Lower_X | Lower_Y | Lower_Z | Lower_E => -100.0 * mm,
            Upper_X | Upper_Y | Upper_Z | Upper_E => 100.0 * mm);
         Params.Tangential_Velocity_Max := 100.0 * mm / s;
         Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
         Params.Axial_Acceleration_Maxes := [others => 1_000.0 * mm / s**2];
         Params.Axial_Jerk_Maxes := [others => 10_000.0 * mm / s**3];
         Params.Axial_Snap_Maxes := [others => 100_000.0 * mm / s**4];
         Params.Axial_Crackle_Maxes := [others => 1_000_000.0 * mm / s**5];
         Setup;

         for I in Blocks'Range loop
            Enqueue_Block (I);
         end loop;
         for I in Blocks'Range loop
            Take (Blocks (I));
            T.Assert (Planner.Flush_Resetting_Data (Blocks (I)) = I, "Blocks are dequeued in input order");
            T.Assert
              (Planner.Block_Start_Pos (Blocks (I)) (X_Axis) = Dimensionless (I - 1) * mm,
               "Planning the next slot carries forward the previous endpoint");
            Check_Data (Blocks (I), 1, I);
         end loop;

         begin
            Planner.Dequeue (Blocks (1), Timed_Out);
            T.Fail ("Dequeue must reject a populated handle");
         exception
            when Program_Error | Ada.Assertions.Assertion_Error =>
               null;
         end;
         T.Assert (Planner.Flush_Resetting_Data (Blocks (1)) = 1, "A rejected dequeue keeps the handle's slot");

         Enqueue_Block (Planner.Block_Capacity + 1);
         Planner.Dequeue (Spare, Timed_Out);
         T.Assert (Timed_Out and then Planner.Is_Empty (Spare), "All borrowed slots apply backpressure");
         Planner.Release (Blocks (2));
         T.Assert (Planner.Is_Empty (Blocks (2)), "Release clears the caller's reference");
         Planner.Release (Blocks (2));
         Take (Spare);
         T.Assert
           (Planner.Flush_Resetting_Data (Spare) = Planner.Block_Capacity + 1, "The next block survives a full ring");
         Check_Data (Spare, 1, Planner.Block_Capacity + 1);
         Check_Data (Blocks (1), 1, 1);
         Check_Data (Blocks (3), 1, 3);

         Reset;
         T.Assert (Planner.Flush_Resetting_Data (Blocks (1)) = 1, "Reset preserves borrowed metadata");
         Check_Data (Blocks (1), 1, 1);
         Setup;
         Enqueue_Block (Planner.Block_Capacity + 2);
         Planner.Dequeue (Blocks (2), Timed_Out);
         T.Assert
           (Timed_Out and then Planner.Is_Empty (Blocks (2)), "Setup cannot overwrite slots borrowed before reset");
         Planner.Release (Blocks (3));
         Take (Blocks (2));
         T.Assert
           (Planner.Flush_Resetting_Data (Blocks (2)) = Planner.Block_Capacity + 2,
            "Only new input executes after reset");
         T.Assert
           (Planner.Block_Start_Pos (Blocks (2)) = Position'[others => 0.0 * mm],
            "Reset restores the initial position");
         Check_Data (Blocks (1), 1, 1);
         Planner.Release (Spare);
         for Block of Blocks loop
            Planner.Release (Block);
         end loop;

         --  Ownership is released by the handle itself, both on normal exit and during exception unwinding.
         for Raise_Error in Boolean loop
            declare
               Test_Error : exception;
            begin
               declare
                  Owned : array (1 .. Planner.Block_Capacity) of Planner.Block_Handle;
               begin
                  for I in Owned'Range loop
                     T.Assert (Planner.Is_Empty (Owned (I)), "A new handle is empty");
                     Enqueue_Block (I);
                     Take (Owned (I));
                  end loop;
                  if Raise_Error then
                     raise Test_Error;
                  end if;
               end;
            exception
               when Test_Error =>
                  null;
            end;
            for I in Blocks'Range loop
               Enqueue_Block (I);
               Take (Blocks (I));
               T.Assert (Planner.Flush_Resetting_Data (Blocks (I)) = I, "Scope exit returns every slot");
            end loop;
            for Block of Blocks loop
               Planner.Release (Block);
            end loop;
         end loop;

         --  Wrap repeatedly through the same storage, changing the discriminant and populated array ranges.
         for I in 1 .. 180 loop
            for Move in 1 .. I mod 3 loop
               Planner.Enqueue_Move ([X_Axis => Dimensionless (10 + Move) * mm, others => 0.0 * mm], 10.0 * mm / s);
            end loop;
            Planner.Enqueue_Corner_Extra_Data (I);
            Planner.Enqueue_Flush (I);
            Take (Spare);
            T.Assert
              (Planner.Corner_Count (Spare) = Planner.Corners_Index (1 + I mod 3),
               "Reused slots have the new corner count");
            T.Assert (Planner.Flush_Resetting_Data (Spare) = I, "Wraparound preserves FIFO metadata");
            Check_Data (Spare, 1, I);
            T.Assert
              (Planner.Corner_ID (Spare, Planner.Corner_Count (Spare)) > Last_ID,
               "Corner IDs advance across ring reuse");
            Last_ID := Planner.Corner_ID (Spare, Planner.Corner_Count (Spare));
            for Corner in 2 .. Planner.Corner_Count (Spare) loop
               T.Assert
                 (Planner.Segment_Pos_At_Time (Spare, Corner, Planner.Segment_Time (Spare, Corner)) (X_Axis)
                  = Dimensionless (9 + Integer (Corner)) * mm,
                  "Reused geometry has the new segment endpoint");
            end loop;
            Planner.Release (Spare);
         end loop;

         --  Leave the producer idle at both a motion-block limit and an overflow-block limit. Neither block
         --  can be published until the next command establishes whether the corner has more extra data.
         Planner.Enqueue_Move ([X_Axis => 20.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         for I in 1 .. 2 loop
            Planner.Enqueue_Corner_Extra_Data (10 * I);
         end loop;
         Planner.Dequeue (Spare, Timed_Out);
         T.Assert (Timed_Out and then Planner.Is_Empty (Spare), "A full motion block waits for corner continuation");
         for I in 3 .. 4 loop
            Planner.Enqueue_Corner_Extra_Data (10 * I);
         end loop;
         Take (Blocks (1));
         Planner.Dequeue (Spare, Timed_Out);
         T.Assert (Timed_Out and then Planner.Is_Empty (Spare), "A full overflow block waits for corner continuation");
         Planner.Enqueue_Corner_Extra_Data (50);
         Planner.Enqueue_Flush (60);
         for I in 2 .. 3 loop
            Take (Blocks (I));
         end loop;
         T.Assert (Planner.Block_Kind (Blocks (1)) = Planner.Motion_Block_Kind, "Motion precedes overflow");
         T.Assert (Planner.Has_Associated_Overflow_Block (Blocks (1)), "Motion delays publication for overflow");
         T.Assert
           (Planner.Block_Kind (Blocks (2)) = Planner.Extra_Data_Overflow_Block_Kind,
            "Extra data uses its own ring slot");
         T.Assert
           (Planner.Corner_ID (Blocks (1), Planner.Corner_Count (Blocks (1))) = Planner.Corner_ID (Blocks (2), 1),
            "Overflow keeps the motion corner's identifier");
         T.Assert (Planner.Has_Associated_Overflow_Block (Blocks (2)), "Overflow delays publication for more data");
         T.Assert
           (not Planner.Has_Associated_Overflow_Block (Blocks (3))
            and then Planner.Corner_ID (Blocks (2), 1) = Planner.Corner_ID (Blocks (3), 1),
            "Only the last overflow block publishes the shared corner identifier");
         Check_Data (Blocks (1), 2, 30);
         Check_Data (Blocks (2), 2, 70);
         Check_Data (Blocks (3), 1, 50);
         for Block of Blocks loop
            Planner.Release (Block);
         end loop;
         Take (Spare);
         Check_Data (Spare, 0, 0);
         T.Assert (Planner.Flush_Resetting_Data (Spare) = 60, "Flush follows all overflow data");
         Planner.Release (Spare);

         --  The corner-count limit also leaves the final corner open for data arriving later.
         for I in 21 .. 23 loop
            Planner.Enqueue_Move ([X_Axis => Dimensionless (I) * mm, others => 0.0 * mm], 10.0 * mm / s);
         end loop;
         Planner.Dequeue (Spare, Timed_Out);
         T.Assert
           (Timed_Out and then Planner.Is_Empty (Spare), "The corner-count limit waits for corner continuation");
         Planner.Enqueue_Corner_Extra_Data (70);
         Planner.Enqueue_Flush (80);
         for I in 1 .. 3 loop
            Take (Blocks (I));
         end loop;
         T.Assert (Planner.Has_Associated_Overflow_Block (Blocks (1)), "Late data follows a full motion block");
         Check_Data (Blocks (2), 1, 70);
         for Block of Blocks loop
            Planner.Release (Block);
         end loop;

         Planner.Enqueue_Corner_Extra_Data (90);
         Planner.Enqueue_Corner_Extra_Data (91);
         Planner.Dequeue (Spare, Timed_Out);
         T.Assert
           (Timed_Out and then Planner.Is_Empty (Spare), "The planner is waiting for a capacity-boundary lookahead");
         Reset;
         Setup;

         --  Reset also discards work in the ready queue and any partially assembled next block.
         Enqueue_Block (30);
         Enqueue_Block (31);
         Enqueue_Block (32);
         Take (Spare);
         Planner.Release (Spare);
         Planner.Enqueue_Move ([X_Axis => 33.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         Planner.Enqueue_Corner_Extra_Data (33);
         Reset;
         Setup;
         Enqueue_Block (40);
         Take (Spare);
         T.Assert (Planner.Flush_Resetting_Data (Spare) = 40, "Reset discards all pending blocks and input");
         Check_Data (Spare, 1, 40);
         Planner.Release (Spare);
         Reset;
         abort Planner.Runner;
      exception
         when others =>
            Stop_Runner;
            raise;
      end;
   end Test_Block_Ring_Lifetime;

   procedure Test_Block_Ring_Reset_During_Dequeue (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         type Motor_Name is (Motor);
         type Motor_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Limits is array (Motor_Name) of Dimensionless;
         package Planner is new
           Motion_Planner.Planner
             (Motor_Name,
              Motor_Map,
              Motor_Limits,
              [others => 1.0],
              Integer,
              0,
              Integer,
              0.005 * s,
              1.0 * s,
              0.001 * s,
              Max_Corners                    => 4,
              Max_Corners_Extra_Data_Count   => 2,
              Max_Corners_Extra_Data_Storage => 128,
              Input_Queue_Length             => 4,
              Allow_Blended_Entry            => Block_Allows_Blended_Entry);
         Block     : Planner.Block_Handle;
         Params    : Kinematic_Parameters := (others => <>);
         Timed_Out : Boolean;

         procedure Reset;
         procedure Setup;

         procedure Reset is
         begin
            select
               delay 5.0;
               T.Fail ("Reset must wake a planner holding a dequeue transaction");
            then abort
               Planner.Reset;
            end select;
         exception
            when Tasking_Error =>
               T.Fail ("A reset during a dequeue transaction terminated the planner");
         end Reset;

         procedure Setup is
         begin
            Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
         end Setup;
      begin
         Params.Bounds :=
           (Kind                                  => Rectangular_Workspace,
            Lower_X | Lower_Y | Lower_Z | Lower_E => -100.0 * mm,
            Upper_X | Upper_Y | Upper_Z | Upper_E => 100.0 * mm);
         Params.Tangential_Velocity_Max := 100.0 * mm / s;
         Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
         Params.Axial_Acceleration_Maxes := [others => 1_000.0 * mm / s**2];
         Params.Axial_Jerk_Maxes := [others => 10_000.0 * mm / s**3];
         Params.Axial_Snap_Maxes := [others => 100_000.0 * mm / s**4];
         Params.Axial_Crackle_Maxes := [others => 1_000_000.0 * mm / s**5];
         Setup;

         --  Each command wakes the idle planner with its dequeue transaction already open. Resetting immediately
         --  usually clears the queue before the planner commits, so every command kind is exercised many times.
         for I in 1 .. 600 loop
            case I mod 3 is
               when 0      =>
                  Planner.Enqueue_Move ([X_Axis => 1.0 * mm, others => 0.0 * mm], 10.0 * mm / s);

               when 1      =>
                  Planner.Enqueue_Corner_Extra_Data (I);

               when others =>
                  Planner.Enqueue_Flush (I);
            end case;
            Reset;
            Setup;
         end loop;

         Planner.Enqueue_Flush (-1);
         Planner.Dequeue (Block, Timed_Out);
         T.Assert
           (not Timed_Out and then Planner.Flush_Resetting_Data (Block) = -1,
            "The planner keeps working after resets during dequeue transactions");
         Planner.Release (Block);
         Reset;
         abort Planner.Runner;
      exception
         when others =>
            begin
               select
                  delay 5.0;
               then abort
                  Planner.Reset;
               end select;
            exception
               when Tasking_Error =>
                  null;
            end;
            abort Planner.Runner;
            Planner.Release (Block);
            raise;
      end;
   end Test_Block_Ring_Reset_During_Dequeue;

   procedure Test_Block_Ring_Termination (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         type Motor_Name is range 1 .. 1;
         type Motor_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Limits is array (Motor_Name) of Dimensionless;
         package Planner is new
           Motion_Planner.Planner
             (Motor_Name,
              Motor_Map,
              Motor_Limits,
              [others => 1.0],
              Integer,
              0,
              Integer,
              0.005 * s,
              1.0 * s,
              0.001 * s,
              Max_Corners                    => 4,
              Max_Corners_Extra_Data_Count   => 2,
              Max_Corners_Extra_Data_Storage => 128,
              Input_Queue_Length             => 4,
              Allow_Blended_Entry            => Block_Allows_Blended_Entry);
         Block            : Planner.Block_Handle;
         Timed_Out        : Boolean;
         Failure_Reported : Boolean := False;
         task Stopper;
         task body Stopper is
         begin
            --  Allow Dequeue to block before terminating the idle runner. No preprocessing wakeup is needed.
            delay 0.1;
            abort Planner.Runner;
         end Stopper;
      begin
         select
            delay 0.75;
         then abort
            begin
               Planner.Dequeue (Block, Timed_Out);
            exception
               when Tasking_Error =>
                  Failure_Reported := True;
            end;
         end select;
         T.Assert (Failure_Reported, "Termination wakes a pending dequeue before its one-second polling timeout");
      exception
         when others =>
            abort Planner.Runner;
            raise;
      end;
   end Test_Block_Ring_Termination;

   procedure Test_Inter_Block_Chain_Lifetime (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      declare
         type Motor_Name is (Motor);
         type Motor_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Limits is array (Motor_Name) of Dimensionless;
         package Planner is new
           Motion_Planner.Planner
             (Motor_Name,
              Motor_Map,
              Motor_Limits,
              [others => 1.0],
              Integer,
              0,
              Integer,
              0.005 * s,
              1.0 * s,
              0.001 * s,
              Max_Corners                    => 4,
              Max_Corners_Extra_Data_Count   => 2,
              Max_Corners_Extra_Data_Storage => 128,
              Input_Queue_Length             => 16,
              Allow_Blended_Entry            => Block_Allows_Blended_Entry);
         Params       : constant Kinematic_Parameters :=
           (Bounds                   =>
              (Kind                                  => Rectangular_Workspace,
               Lower_X | Lower_Y | Lower_Z | Lower_E => -100.0 * mm,
               Upper_X | Upper_Y | Upper_Z | Upper_E => 100.0 * mm),
            Tangential_Velocity_Max  => 100.0 * mm / s,
            Axial_Velocity_Maxes     => [others => 100.0 * mm / s],
            Axial_Acceleration_Maxes => [others => 1_000.0 * mm / s**2],
            Axial_Jerk_Maxes         => [others => 10_000.0 * mm / s**3],
            Axial_Snap_Maxes         => [others => 100_000.0 * mm / s**4],
            Axial_Crackle_Maxes      => [others => 1_000_000.0 * mm / s**5],
            others                   => <>);
         Block, Spare : Planner.Block_Handle;
         Timed_Out    : Boolean;
         Tail         : Position_Offset;
         Seam         : Position;
      begin
         Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
         for I in 1 .. 2 loop
            Planner.Enqueue_Move ([X_Axis => Dimensionless (I) * 10.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
            Planner.Enqueue_Flush (0);
         end loop;
         Planner.Enqueue_Move ([X_Axis => 30.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         --  This handshake occurs after the preceding two blocks were published. It gives a deterministic
         --  publication barrier without relying on the scheduler or a timed delay.
         Tail := Planner.Enqueue_Homing_Flush (1);
         T.Assert (Tail (X_Axis) > 0.0 * mm, "The following homing block reached its planning handshake");
         Planner.Dequeue (Block, Timed_Out);
         T.Assert (not Timed_Out, "The first prepared block can be borrowed");
         Planner.Prepare_Segment (Block, 2);
         T.Assert (Planner.Has_Next_Block (Block), "The prepared successor is accepted at the tail decision");
         Seam := Planner.Segment_Pos_At_Time (Block, 2, Planner.Segment_Time (Block, 2));
         Planner.Reset;
         Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
         Planner.Enqueue_Flush (1);
         Planner.Dequeue (Spare, Timed_Out);
         T.Assert (not Timed_Out, "Setup can use free slots while the old chain remains borrowed");
         Planner.Advance_Block (Block);
         T.Assert (not Planner.Has_Next_Block (Block), "The chain stops before the homing boundary");
         T.Assert
           (abs (Seam - Planner.Block_Start_Pos (Block)) < 1.0E-8 * mm,
            "The chain remains continuous after reset and another setup");
         Planner.Release (Block);
         Planner.Release (Spare);
         Planner.Reset;
         abort Planner.Runner;
      exception
         when others =>
            select
               delay 2.0;
            then abort
               Planner.Reset;
            end select;
            abort Planner.Runner;
            raise;
      end;
   end Test_Inter_Block_Chain_Lifetime;

   procedure Test_Inter_Block_Streaming (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      declare
         Total_Blocks         : constant Positive := 1_000;
         Policy_Calls         : Natural := 0
         with Atomic;
         First_Started        : Boolean := False
         with Atomic;
         Publication_Complete : Boolean := False
         with Atomic;
         Producer_Failed      : Boolean := False
         with Atomic;
         function Allow_Entry (Data : Integer) return Boolean is
            pragma Unreferenced (Data);
         begin
            Policy_Calls := @ + 1;
            return True;
         end Allow_Entry;
         type Motor_Name is (Motor);
         type Motor_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Limits is array (Motor_Name) of Dimensionless;
         package Planner is new
           Motion_Planner.Planner
             (Motor_Name,
              Motor_Map,
              Motor_Limits,
              [others => 1.0],
              Integer,
              0,
              Integer,
              0.005 * s,
              1.0 * s,
              0.001 * s,
              Max_Corners                    => 80,
              Max_Corners_Extra_Data_Count   => 2,
              Max_Corners_Extra_Data_Storage => 128,
              Input_Queue_Length             => 16,
              Allow_Blended_Entry            => Allow_Entry);
         Params               : Kinematic_Parameters := (others => <>);
         Block                : Planner.Block_Handle;
         Timed_Out            : Boolean;
         Seam                 : Position;
         Completed            : Natural := 0;
         Prefix_Time          : Time;
         task Producer is
            entry Start;
         end Producer;
         task body Producer is
            Tail : Position_Offset;
         begin
            accept Start;
            for I in 1 .. Total_Blocks loop
               if I = 1 then
                  for J in 1 .. 40 loop
                     Planner.Enqueue_Move
                       ([X_Axis => Dimensionless (J) * 0.25 * mm, others => 0.0 * mm], 10.0 * mm / s);
                  end loop;
               else
                  Planner.Enqueue_Move ([X_Axis => Dimensionless (I) * 10.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
               end if;
               Planner.Enqueue_Flush (if I = Total_Blocks then 1 else 0);
               if I = 1 then
                  while not First_Started loop
                     delay 0.001;
                  end loop;
               end if;
            end loop;
            Planner.Enqueue_Move
              ([X_Axis => Dimensionless (Total_Blocks + 1) * 10.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
            Tail := Planner.Enqueue_Homing_Flush (1);
            Publication_Complete := Tail (X_Axis) > 0.0 * mm;
         exception
            when others =>
               Producer_Failed := True;
         end Producer;
      begin
         Params.Bounds :=
           (Kind                                  => Rectangular_Workspace,
            Lower_X | Lower_Y | Lower_Z | Lower_E => -100_000.0 * mm,
            Upper_X | Upper_Y | Upper_Z | Upper_E => 100_000.0 * mm);
         Params.Tangential_Velocity_Max := 100.0 * mm / s;
         Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
         Params.Axial_Acceleration_Maxes := [others => 1_000.0 * mm / s**2];
         Params.Axial_Jerk_Maxes := [others => 10_000.0 * mm / s**3];
         Params.Axial_Snap_Maxes := [others => 100_000.0 * mm / s**4];
         Params.Axial_Crackle_Maxes := [others => 1_000_000.0 * mm / s**5];
         Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
         Producer.Start;
         Planner.Dequeue (Block, Timed_Out);
         T.Assert (not Timed_Out, "The streaming root is available");
         Prefix_Time := Planner.Segment_Time (Block, 2);
         Planner.Prepare_Segment (Block, 2);
         T.Assert (not Planner.Has_Next_Block (Block), "The long first block begins before a successor exists");
         First_Started := True;
         for I in 1 .. Total_Blocks loop
            --  Entering the next boundary's policy proves this boundary was published. This keeps the producer
            --  deliberately ahead without relying on a fixed sleep or requiring the entire stream to fit in memory.
            if I < Total_Blocks then
               select
                  delay 5.0;
                  T.Fail ("Producer failed to prepare streaming boundary" & I'Image);
               then abort
                  loop
                     exit when
                       Producer_Failed
                       or else (if I < Total_Blocks - 1 then Policy_Calls > I else Publication_Complete);
                     delay 0.001;
                  end loop;
               end select;
               T.Assert (not Producer_Failed, "The streaming producer stays alive");
            end if;
            for Corner in Planner.Finishing_Corners_Index'First .. Planner.Corner_Count (Block) loop
               Planner.Prepare_Segment (Block, Corner);
            end loop;
            if I = 1 then
               T.Assert
                 (Planner.Segment_Time (Block, 2) = Prefix_Time,
                  "A blend prepared after execution starts preserves the already consumed prefix");
            end if;
            T.Assert
              (Planner.Has_Next_Block (Block) = (I < Total_Blocks),
               "Only the stream's terminal block has a stopped exit");
            Seam :=
              Planner.Segment_Pos_At_Time
                (Block, Planner.Corner_Count (Block), Planner.Segment_Time (Block, Planner.Corner_Count (Block)));
            if I < Total_Blocks then
               T.Assert
                 (Planner.Segment_Vel_Ratio_At_Time
                    (Block, Planner.Corner_Count (Block), Planner.Segment_Time (Block, Planner.Corner_Count (Block)))
                  > 0.0,
                  "Every moving boundary retains nonzero speed");
               Planner.Advance_Block (Block);
               T.Assert
                 (abs (Seam - Planner.Block_Start_Pos (Block)) < 1.0E-8 * mm,
                  "Position remains continuous through slot reuse");
            end if;
            Completed := @ + 1;
         end loop;
         T.Assert
           (Planner.Block_Capacity = 5 and then Completed = Total_Blocks,
            "A thousand-block stream executes through five reusable slots with one dequeue");
         T.Assert (Seam (X_Axis) = Dimensionless (Total_Blocks) * 10.0 * mm, "The final endpoint is exact");
         Planner.Release (Block);
         Planner.Reset;
         abort Planner.Runner;
      exception
         when others =>
            abort Producer;
            select
               delay 2.0;
            then abort
               Planner.Reset;
            end select;
            abort Planner.Runner;
            raise;
      end;
   end Test_Inter_Block_Streaming;

   procedure Test_Inter_Block_Missed_Deadline (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      declare
         Entered, Continue_Planning : Boolean := False
         with Atomic;
         function Allow_Entry (Data : Integer) return Boolean is
            pragma Unreferenced (Data);
         begin
            Entered := True;
            while not Continue_Planning loop
               delay 0.001;
            end loop;
            return True;
         end Allow_Entry;
         type Motor_Name is (Motor);
         type Motor_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Limits is array (Motor_Name) of Dimensionless;
         package Planner is new
           Motion_Planner.Planner
             (Motor_Name,
              Motor_Map,
              Motor_Limits,
              [others => 1.0],
              Integer,
              0,
              Integer,
              0.005 * s,
              1.0 * s,
              0.001 * s,
              Max_Corners                    => 4,
              Max_Corners_Extra_Data_Count   => 2,
              Max_Corners_Extra_Data_Storage => 128,
              Input_Queue_Length             => 16,
              Allow_Blended_Entry            => Allow_Entry);
         Params                     : Kinematic_Parameters := (others => <>);
         Block                      : Planner.Block_Handle;
         Timed_Out                  : Boolean;
         Original_Time              : Time;
         Tail                       : Position_Offset;
      begin
         Params.Bounds :=
           (Kind                                  => Rectangular_Workspace,
            Lower_X | Lower_Y | Lower_Z | Lower_E => -100.0 * mm,
            Upper_X | Upper_Y | Upper_Z | Upper_E => 100.0 * mm);
         Params.Tangential_Velocity_Max := 100.0 * mm / s;
         Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
         Params.Axial_Acceleration_Maxes := [others => 1_000.0 * mm / s**2];
         Params.Axial_Jerk_Maxes := [others => 10_000.0 * mm / s**3];
         Params.Axial_Snap_Maxes := [others => 100_000.0 * mm / s**4];
         Params.Axial_Crackle_Maxes := [others => 1_000_000.0 * mm / s**5];
         Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
         Planner.Enqueue_Move ([X_Axis => 10.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         Planner.Enqueue_Flush (0);
         Planner.Dequeue (Block, Timed_Out);
         T.Assert (not Timed_Out, "Borrow the first block before its successor is planned");
         Original_Time := Planner.Segment_Time (Block, 2);
         Planner.Enqueue_Move ([X_Axis => 20.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         Planner.Enqueue_Flush (0);
         select
            delay 5.0;
            T.Fail ("Planner did not enter the controlled blend preparation");
         then abort
            while not Entered loop
               delay 0.001;
            end loop;
         end select;
         --  Must return while the planner remains blocked inside the policy, using the immutable stopping plan.
         Planner.Prepare_Segment (Block, 2);
         T.Assert (not Planner.Has_Next_Block (Block), "An unfinished blend cannot be selected");
         T.Assert (Planner.Segment_Time (Block, 2) = Original_Time, "The original stopping profile is untouched");
         Continue_Planning := True;
         Planner.Enqueue_Move ([X_Axis => 30.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         Tail := Planner.Enqueue_Homing_Flush (1);
         T.Assert (Tail (X_Axis) > 0.0 * mm, "The losing producer completes and publishes its stopped successor");
         T.Assert (not Planner.Has_Next_Block (Block), "A late publication cannot change the chosen trajectory");
         Planner.Release (Block);
         Planner.Dequeue (Block, Timed_Out);
         T.Assert (not Timed_Out, "The rejected blend's successor remains queued");
         T.Assert
           (Planner.Segment_Vel_Ratio_At_Time (Block, 2, 0.0 * s) = 0.0,
            "The successor's stopped entry is restored when publication loses the deadline");
         T.Assert
           (Planner.Block_Start_Pos (Block) = Position'[X_Axis => 10.0 * mm, others => 0.0 * mm],
            "The successor starts at the original stopped endpoint");
         Planner.Release (Block);
         Planner.Reset;
         abort Planner.Runner;
      exception
         when others =>
            Continue_Planning := True;
            select
               delay 2.0;
            then abort
               Planner.Reset;
            end select;
            abort Planner.Runner;
            raise;
      end;
   end Test_Inter_Block_Missed_Deadline;

   procedure Test_Inter_Block_Declined_Successor (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      declare
         type Motor_Name is (Motor);
         type Motor_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Limits is array (Motor_Name) of Dimensionless;
         package Planner is new
           Motion_Planner.Planner
             (Motor_Name,
              Motor_Map,
              Motor_Limits,
              [others => 1.0],
              Integer,
              0,
              Integer,
              0.005 * s,
              1.0 * s,
              0.001 * s,
              Max_Corners                    => 4,
              Max_Corners_Extra_Data_Count   => 2,
              Max_Corners_Extra_Data_Storage => 128,
              Input_Queue_Length             => 16,
              Allow_Blended_Entry            => Block_Allows_Blended_Entry);
         Params    : constant Kinematic_Parameters :=
           (Bounds                   =>
              (Kind                                  => Rectangular_Workspace,
               Lower_X | Lower_Y | Lower_Z | Lower_E => -100.0 * mm,
               Upper_X | Upper_Y | Upper_Z | Upper_E => 100.0 * mm),
            Tangential_Velocity_Max  => 100.0 * mm / s,
            Axial_Velocity_Maxes     => [others => 100.0 * mm / s],
            Axial_Acceleration_Maxes => [others => 1_000.0 * mm / s**2],
            Axial_Jerk_Maxes         => [others => 10_000.0 * mm / s**3],
            Axial_Snap_Maxes         => [others => 100_000.0 * mm / s**4],
            Axial_Crackle_Maxes      => [others => 1_000_000.0 * mm / s**5],
            others                   => <>);
         Block     : Planner.Block_Handle;
         Blocks    : array (1 .. Planner.Block_Capacity) of Planner.Block_Handle;
         Timed_Out : Boolean;
         Tail      : Position_Offset;
      begin
         Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
         for I in 1 .. 2 loop
            Planner.Enqueue_Move ([X_Axis => Dimensionless (I) * 10.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
            Planner.Enqueue_Flush (0);
         end loop;
         Planner.Enqueue_Move ([X_Axis => 30.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         --  The homing handshake occurs after the first two blocks were published, so the first offers the second.
         Tail := Planner.Enqueue_Homing_Flush (1);
         T.Assert (Tail (X_Axis) > 0.0 * mm, "The following homing block reached its planning handshake");
         Planner.Dequeue (Block, Timed_Out);
         T.Assert (not Timed_Out, "The first block can be borrowed");
         --  A reset pause can stop before the tail is chosen, releasing the block with its successor still offered.
         Planner.Release (Block);
         Planner.Resolve_Homing_Position ([X_Axis => 30.0 * mm, others => 0.0 * mm]);
         --  The homing block starts where the discarded successor ended, which execution never reached.
         Planner.Dequeue (Block, Timed_Out);
         T.Assert (Timed_Out, "Blocks queued after a discarded successor are withheld until reset");
         Planner.Reset;
         Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
         for I in Blocks'Range loop
            Planner.Enqueue_Flush (1);
         end loop;
         for B of Blocks loop
            Planner.Dequeue (B, Timed_Out);
            T.Assert (not Timed_Out, "Declining a successor does not leak its slot");
         end loop;
         for B of Blocks loop
            Planner.Release (B);
         end loop;
         Planner.Reset;
         abort Planner.Runner;
      exception
         when others =>
            select
               delay 2.0;
            then abort
               Planner.Reset;
            end select;
            abort Planner.Runner;
            raise;
      end;
   end Test_Inter_Block_Declined_Successor;

   procedure Test_Inter_Block_Retired_Cleanup_Error (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      declare
         Armed, Failed  : Boolean := False
         with Atomic;
         Finalizer_Task : Ada.Task_Identification.Task_Id;
         --  Written before Failed is set, so reading it after observing Failed is ordered by that atomic store.

         type Data is new Ada.Finalization.Controlled with record
            Value : Natural := 0;
         end record;
         overriding
         procedure Finalize (Object : in out Data);

         overriding
         procedure Finalize (Object : in out Data) is
            Value : constant Natural := Object.Value;
         begin
            Object.Value := 0;
            if Value /= 0 and then Armed then
               Finalizer_Task := Ada.Task_Identification.Current_Task;
               Failed := True;
               raise Constraint_Error with "Injected retired block data finalizer failure";
            end if;
         end Finalize;

         type Motor_Name is (Motor);
         type Motor_Map is array (Axis_Name, Motor_Name) of Curvature;
         type Motor_Limits is array (Motor_Name) of Dimensionless;
         package Planner is new
           Motion_Planner.Planner
             (Motor_Name,
              Motor_Map,
              Motor_Limits,
              [others => 1.0],
              Integer,
              0,
              Data,
              0.005 * s,
              1.0 * s,
              0.001 * s,
              Max_Corners                    => 4,
              Max_Corners_Extra_Data_Count   => 2,
              Max_Corners_Extra_Data_Storage => 512,
              Input_Queue_Length             => 16,
              Allow_Blended_Entry            => Block_Allows_Blended_Entry);
         Params              : constant Kinematic_Parameters :=
           (Bounds                   =>
              (Kind                                  => Rectangular_Workspace,
               Lower_X | Lower_Y | Lower_Z | Lower_E => -100.0 * mm,
               Upper_X | Upper_Y | Upper_Z | Upper_E => 100.0 * mm),
            Tangential_Velocity_Max  => 100.0 * mm / s,
            Axial_Velocity_Maxes     => [others => 100.0 * mm / s],
            Axial_Acceleration_Maxes => [others => 1_000.0 * mm / s**2],
            Axial_Jerk_Maxes         => [others => 10_000.0 * mm / s**3],
            Axial_Snap_Maxes         => [others => 100_000.0 * mm / s**4],
            Axial_Crackle_Maxes      => [others => 1_000_000.0 * mm / s**5],
            others                   => <>);
         Block, Homing, Last : Planner.Block_Handle;
         Timed_Out           : Boolean;
         Tail                : Position_Offset;

         procedure Release_Quietly (Handle : in out Planner.Block_Handle) is
         begin
            Planner.Release (Handle);
         exception
            when others =>
               null;
         end Release_Quietly;
      begin
         Planner.Runner.Setup (Params, [others => [Motor => 0.0 / mm]]);
         Planner.Enqueue_Corner_Extra_Data ((Ada.Finalization.Controlled with Value => 1));
         for I in 1 .. 2 loop
            Planner.Enqueue_Move ([X_Axis => Dimensionless (I) * 10.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
            Planner.Enqueue_Flush (0);
         end loop;
         Planner.Enqueue_Move ([X_Axis => 30.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         Tail := Planner.Enqueue_Homing_Flush (1);
         T.Assert (Tail (X_Axis) > 0.0 * mm, "The following homing block reached its planning handshake");
         Planner.Dequeue (Block, Timed_Out);
         T.Assert (not Timed_Out, "The first block can be borrowed");
         Planner.Prepare_Segment (Block, 2);
         T.Assert (Planner.Has_Next_Block (Block), "The prepared successor is accepted");
         Armed := True;
         Planner.Advance_Block (Block);
         Planner.Resolve_Homing_Position ([X_Axis => 30.0 * mm, others => 0.0 * mm]);
         Planner.Enqueue_Move ([X_Axis => 40.0 * mm, others => 0.0 * mm], 10.0 * mm / s);
         Planner.Enqueue_Flush (1);
         Planner.Dequeue (Homing, Timed_Out);
         T.Assert (not Timed_Out and then Planner.Is_Homing_Move (Homing), "The homing block is published");
         Planner.Dequeue (Last, Timed_Out);
         T.Assert (not Timed_Out, "Runner keeps planning while the retired slot is cleaned in the background");
         --  The background cleaner polls for retired slots. Keep the failure armed until it has finalized this one.
         select
            delay 5.0;
            T.Fail ("The background cleaner did not finalize the retired slot");
         then abort
            while not Failed loop
               delay 0.001;
            end loop;
         end select;
         declare
            use type Ada.Task_Identification.Task_Id;
         begin
            T.Assert
              (Finalizer_Task /= Planner.Runner'Identity,
               "Retired extra data is not finalized on Runner, which a waiting finalizer could block");
         end;
         Armed := False;
         begin
            Planner.Release (Block);
            T.Fail ("Release must report the deferred retired-slot cleanup failure");
         exception
            when Program_Error =>
               null;
         end;
         T.Assert (Planner.Is_Empty (Block), "Reporting a deferred failure still empties the handle");
         Planner.Release (Homing);
         Planner.Release (Last);
         Planner.Reset;
         abort Planner.Runner;
      exception
         when others =>
            Armed := False;
            Release_Quietly (Block);
            Release_Quietly (Homing);
            Release_Quietly (Last);
            select
               delay 2.0;
            then abort
               Planner.Reset;
            end select;
            abort Planner.Runner;
            raise;
      end;
   end Test_Inter_Block_Retired_Cleanup_Error;

   function All_Tests return Trendy_Test.Test_Group
   is ([Test_Inter_Block_Streaming'Access,
        Test_Inter_Block_Missed_Deadline'Access,
        Test_Inter_Block_Chain_Lifetime'Access,
        Test_Inter_Block_Declined_Successor'Access,
        Test_Inter_Block_Retired_Cleanup_Error'Access,
        Test_Block_Ring_Cleanup'Access,
        Test_Block_Ring_Cleanup_Exceptions'Access,
        Test_Block_Ring_Lifetime'Access,
        Test_Block_Ring_Reset_During_Dequeue'Access,
        Test_Block_Ring_Termination'Access]);

end Prunt.Motion_Planner.Block_Ring_Test;
