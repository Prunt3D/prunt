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

package body Prunt.Motion_Planner.Planner.Preprocessor is

   pragma Extensions_Allowed (On);

   protected body Command_Queue is
      procedure Setup (Initial_Parameters : Kinematic_Parameters) is
      begin
         if Setup_Done then
            raise Constraint_Error with "Setup already done.";
         end if;

         Current_Params := Initial_Parameters;

         Setup_Done := True;
      end Setup;

      procedure Assign_Corner_ID (Kind : Command_Kind) is
      begin
         case Kind is
            when Move_Kind =>
               Last_Assigned_Corner_ID := Last_Assigned_Corner_ID + 1;
               Has_Current_Corner_ID := True;

            when Corner_Extra_Data_Kind =>
               if not Has_Current_Corner_ID then
                  Last_Assigned_Corner_ID := Last_Assigned_Corner_ID + 1;
                  Has_Current_Corner_ID := True;
               end if;

            when Flush_Kind | Flush_And_Reset_Position_Kind | Flush_And_Change_Parameters_Kind =>
               if not Has_Current_Corner_ID then
                  Last_Assigned_Corner_ID := Last_Assigned_Corner_ID + 1;
               end if;
               Has_Current_Corner_ID := False;
         end case;
      end Assign_Corner_ID;

      procedure Append_To_Queue (Comm : Command) is
      begin
         Elements (Next_Write) := Comm;

         if Next_Write = Elements'Last then
            Next_Write := Elements'First;
         else
            Next_Write := @ + 1;
         end if;

         if Next_Write = Next_Read then
            Is_Full := True;
         end if;
      end Append_To_Queue;

      entry Enqueue
        (Comm : Command; Ignore_Bounds : Boolean := False; Extra : access constant Corner_Extra_Data_Type := null)
        when not Is_Full and then High_Priority_Enqueue'Count = 0
      is
      begin
         if High_Priority_Enqueue'Count /= 0 then
            --  This is should never be triggered inside a protected object, but it could catch some errors if this is
            --  converted to a task and High_Priority_Enqueue is called directly for some reason.
            raise Constraint_Error with "High priority queue is not empty.";
         end if;

         if not Setup_Done then
            return;
         end if;

         case Comm.Kind is
            when Flush_Kind                       =>
               Assign_Corner_ID (Comm.Kind);

            when Corner_Extra_Data_Kind           =>
               begin
                  Extra_Data_Storage.Enqueue (Extra.all);
               exception
                  when Corner_Extra_Data_Queues.Out_Of_Space_Error =>
                     requeue High_Priority_Enqueue;
               end;

               Assign_Corner_ID (Comm.Kind);

            when Flush_And_Reset_Position_Kind    =>
               if not Ignore_Bounds then
                  Check_Bounds (Comm.Reset_Pos, Current_Params);
               end if;
               Assign_Corner_ID (Comm.Kind);

            when Flush_And_Change_Parameters_Kind =>
               Current_Params := Comm.New_Params;
               Assign_Corner_ID (Comm.Kind);

            when Move_Kind                        =>
               if not Ignore_Bounds then
                  Check_Bounds (Comm.Pos, Current_Params);
               end if;

               if Comm.Dwell_After < 0.0 * s then
                  raise Constraint_Error with "Negative dwell times are not allowed.";
               end if;

               if Comm.Feedrate <= 0.0 * mm / s then
                  raise Constraint_Error with "Feedrate must be positive.";
               end if;

               Assign_Corner_ID (Comm.Kind);

         end case;
         --  Checking happens here so we can provide instant feedback to the user when g-code is typed in manually.

         Append_To_Queue (Comm);
      end Enqueue;

      entry High_Priority_Enqueue
        (Comm : Command; Ignore_Bounds : Boolean := False; Extra : access constant Corner_Extra_Data_Type := null)
        when not Is_Full and then Retry_High_Priority
      is
      begin
         if not Setup_Done then
            return;
         end if;

         Retry_High_Priority := False;

         case Comm.Kind is
            when Corner_Extra_Data_Kind =>
               begin
                  Extra_Data_Storage.Enqueue (Extra.all);
               exception
                  when Corner_Extra_Data_Queues.Out_Of_Space_Error =>
                     requeue High_Priority_Enqueue;
               end;

               Assign_Corner_ID (Comm.Kind);

            when others                 =>
               raise Program_Error with "Should be unreachable.";

         end case;

         Append_To_Queue (Comm);
      end High_Priority_Enqueue;

      entry Dequeue (Comm : out Command; Reset_Called : out Boolean)
        when Is_Full or else Next_Read /= Next_Write or else not Setup_Done
      is
      begin
         Reset_Called := not Setup_Done;

         if not Setup_Done then
            return;
         end if;

         if In_Dequeue then
            raise Constraint_Error with "Already in dequeue transaction.";
         end if;

         Comm := Elements (Next_Read);
         In_Dequeue := True;
      end Dequeue;

      function Dequeue_Extra_Data return Corner_Extra_Data_Type is
      begin
         return Extra_Data_Storage.Peek;
      end Dequeue_Extra_Data;

      function Get_Last_Assigned_Corner_ID return Planner_Corner_ID is
      begin
         return Last_Assigned_Corner_ID;
      end Get_Last_Assigned_Corner_ID;

      function Next_Is_Corner_Extra_Data return Boolean is
      begin
         return
           Setup_Done
           and then not In_Dequeue
           and then (Is_Full or else Next_Read /= Next_Write)
           and then Elements (Next_Read).Kind = Corner_Extra_Data_Kind;
      end Next_Is_Corner_Extra_Data;

      procedure Finish_Dequeue is
         Current_Comm : constant Command := Elements (Next_Read);
      begin
         if not In_Dequeue then
            raise Constraint_Error with "Not in dequeue transaction.";
         end if;

         if Current_Comm.Kind = Corner_Extra_Data_Kind then
            Extra_Data_Storage.Dequeue;
         end if;

         if Next_Read = Elements'Last then
            Next_Read := Elements'First;
         else
            Next_Read := @ + 1;
         end if;
         Is_Full := False;
         In_Dequeue := False;
         Retry_High_Priority := True;
      end Finish_Dequeue;

      procedure Cancel_Dequeue is
      begin
         if not In_Dequeue then
            raise Constraint_Error with "Not in dequeue transaction.";
         end if;
         In_Dequeue := False;
      end Cancel_Dequeue;

      procedure Reset is
      begin
         Setup_Done := False;
         Is_Full := False;
         In_Dequeue := False;
         Next_Read := Command_Queue_Array_Type'First;
         Next_Write := Command_Queue_Array_Type'First;
         Extra_Data_Storage.Clear;
         Retry_High_Priority := True;
         Has_Current_Corner_ID := False;
      end Reset;
   end Command_Queue;

   procedure Enqueue
     (Comm : Command; Ignore_Bounds : Boolean := False; Extra : access constant Corner_Extra_Data_Type := null) is
   begin
      Command_Queue.Enqueue (Comm, Ignore_Bounds, Extra);
   end Enqueue;

   function Get_Last_Assigned_Corner_ID return Planner_Corner_ID is
   begin
      return Command_Queue.Get_Last_Assigned_Corner_ID;
   end Get_Last_Assigned_Corner_ID;

   procedure Reset is
   begin
      Command_Queue.Reset;
      Runner.Reset (Command_Queue.Get_Last_Assigned_Corner_ID);
   end Reset;

   procedure Run (Block : aliased out Execution_Block; Reset_Called : out Boolean) is
   begin
      Runner.Run (Block, Reset_Called);
   end Run;

   procedure Setup (Initial_Parameters : Kinematic_Parameters) is
   begin
      Command_Queue.Setup (Limit_Higher_Order_Params (Initial_Parameters));
      Runner.Setup (Limit_Higher_Order_Params (Initial_Parameters));
   end Setup;

   protected body Runner is
      procedure Setup (Initial_Parameters : Kinematic_Parameters) is
      begin
         if Setup_Done then
            raise Constraint_Error with "Setup already done.";
         end if;

         Current_Params := Initial_Parameters;

         Setup_Done := True;
      end Setup;

      procedure Run (Block : aliased out Execution_Block; Reset_Called : out Boolean) is
         Flush_Resetting_Data         : Flush_Resetting_Data_Type := Flush_Resetting_Data_Type_Default;
         N_Corners                    : Corners_Index := 1;
         Block_N_Corners              : Corners_Index
         with Address => Block.N_Corners'Address;
         Next_Params                  : Kinematic_Parameters;
         Extra_Data_This_Corner       : Max_Corners_Extra_Data_Type'Base := 0;
         Current_Corner_ID            : Planner_Corner_ID := 0;
         Previous_Output_Corner_ID    : constant Planner_Corner_ID := Last_Assigned_Corner_ID;
         First_Corner_ID              : Planner_Corner_ID := Previous_Output_Corner_ID;
         Is_Homing_Move               : Boolean := False;
         Is_Extra_Data_Overflow_Block : Boolean := False;
         Is_First_Command_In_Block    : Boolean := True;
         Associated_Overflow_Block    : Boolean := False;

         procedure Finish_Dequeue_And_Update_Corner_ID (Kind : Command_Kind; Corner_ID : out Planner_Corner_ID);
         --  Commit the current dequeue transaction and update the runner's mirror of command-queue corner ID state.
         --  Moves always open a new corner, extra data attaches to the current open corner or creates one, and flushes
         --  close the current open corner. Corner_ID is the ID assigned to the consumed command.

         function Try_Append_Extra_Data return Boolean;
         --  Append the dequeued extra data and commit its dequeue transaction. Returns False, with the transaction
         --  cancelled, when the current block has no more extra-data storage and the data should be retried in a later
         --  overflow block.

         procedure Finish_Dequeue_And_Update_Corner_ID (Kind : Command_Kind; Corner_ID : out Planner_Corner_ID) is
         begin
            Command_Queue.Finish_Dequeue;

            --  TODO: We should probably try to combine this with the enqueue logic to lower the risk of a mismatch.
            case Kind is
               when Move_Kind =>
                  Last_Assigned_Corner_ID := Last_Assigned_Corner_ID + 1;
                  Current_Input_Corner_ID := Last_Assigned_Corner_ID;
                  Corner_ID := Current_Input_Corner_ID;

               when Corner_Extra_Data_Kind =>
                  if Current_Input_Corner_ID = 0 then
                     Last_Assigned_Corner_ID := Last_Assigned_Corner_ID + 1;
                     Current_Input_Corner_ID := Last_Assigned_Corner_ID;
                  end if;
                  Corner_ID := Current_Input_Corner_ID;

               when Flush_Kind | Flush_And_Reset_Position_Kind | Flush_And_Change_Parameters_Kind =>
                  if Current_Input_Corner_ID = 0 then
                     Last_Assigned_Corner_ID := Last_Assigned_Corner_ID + 1;
                     Corner_ID := Last_Assigned_Corner_ID;
                  else
                     Corner_ID := Current_Input_Corner_ID;
                  end if;
                  Current_Input_Corner_ID := 0;
            end case;
         end Finish_Dequeue_And_Update_Corner_ID;

         function Try_Append_Extra_Data return Boolean is
            Corner_ID : Planner_Corner_ID;
         begin
            Corners_Extra_Data.Append (Command_Queue.Dequeue_Extra_Data);
            Finish_Dequeue_And_Update_Corner_ID (Corner_Extra_Data_Kind, Corner_ID);
            Current_Corner_ID := Corner_ID;
            return True;
         exception
            when Corner_Extra_Data_Vectors.Out_Of_Space_Error =>
               if Corners_Extra_Data.Is_Empty then
                  raise Constraint_Error with "Extra corner data too big.";
               end if;

               Command_Queue.Cancel_Dequeue;
               return False;
         end Try_Append_Extra_Data;
      begin
         Reset_Called := False;

         if not Setup_Done then
            raise Constraint_Error with "Setup not done.";
         end if;

         Corners_Extra_Data.Clear;

         Next_Params := Current_Params;

         Corners (1) := Last_Pos / Current_Params.Axial_Scaler;
         Corners_Extra_Data_End_Indices (1) := Corners_Extra_Data.Last_Index;
         --  All other arrays start from the second corner.

         Read_In_Commands : loop
            declare
               Next_Command       : Command;
               Accepted_Corner_ID : Planner_Corner_ID := 0;
            begin
               pragma Warnings (Off, "potentially blocking operation in protected operation");
               Command_Queue.Dequeue (Next_Command, Reset_Called);
               pragma Warnings (On, "potentially blocking operation in protected operation");

               if Reset_Called then
                  return;
               end if;

               if Is_First_Command_In_Block then
                  Is_Extra_Data_Overflow_Block :=
                    Next_Command.Kind = Corner_Extra_Data_Kind
                    and then Previous_Output_Corner_ID /= 0
                    and then Current_Input_Corner_ID = Previous_Output_Corner_ID;
                  Is_First_Command_In_Block := False;
               end if;

               if Is_Extra_Data_Overflow_Block then
                  if Next_Command.Kind /= Corner_Extra_Data_Kind
                    or else Current_Input_Corner_ID /= Previous_Output_Corner_ID
                  then
                     Command_Queue.Cancel_Dequeue;
                     exit Read_In_Commands;
                  end if;

                  if not Try_Append_Extra_Data then
                     exit Read_In_Commands;
                  end if;

                  exit Read_In_Commands when Corners_Extra_Data.Last_Index = Corners_Extra_Data_Index'Last;
               else
                  case Next_Command.Kind is
                     when Flush_Kind                       =>
                        Finish_Dequeue_And_Update_Corner_ID (Next_Command.Kind, Accepted_Corner_ID);

                        if Current_Corner_ID = 0 then
                           First_Corner_ID := Accepted_Corner_ID;
                        end if;

                        Flush_Resetting_Data := Next_Command.Flush_Resetting_Data;
                        Is_Homing_Move := Next_Command.Is_Homing_Move;
                        exit Read_In_Commands;

                     when Flush_And_Reset_Position_Kind    =>
                        Finish_Dequeue_And_Update_Corner_ID (Next_Command.Kind, Accepted_Corner_ID);

                        if Current_Corner_ID = 0 then
                           First_Corner_ID := Accepted_Corner_ID;
                        end if;

                        Flush_Resetting_Data := Next_Command.Flush_Resetting_Data;
                        Last_Pos := Next_Command.Reset_Pos;
                        Is_Homing_Move := Next_Command.Is_Homing_Move;
                        exit Read_In_Commands;

                     when Flush_And_Change_Parameters_Kind =>
                        Finish_Dequeue_And_Update_Corner_ID (Next_Command.Kind, Accepted_Corner_ID);

                        if Current_Corner_ID = 0 then
                           First_Corner_ID := Accepted_Corner_ID;
                        end if;

                        Flush_Resetting_Data := Next_Command.Flush_Resetting_Data;
                        Next_Params := Limit_Higher_Order_Params (Next_Command.New_Params);
                        Is_Homing_Move := Next_Command.Is_Homing_Move;
                        exit Read_In_Commands;

                     when Corner_Extra_Data_Kind           =>
                        pragma
                          Assert
                            (Current_Corner_ID = 0 or else Current_Input_Corner_ID = Current_Corner_ID,
                             "Extra data corner ID changed while assembling a block.");

                        if not Try_Append_Extra_Data then
                           exit Read_In_Commands;
                        end if;

                        if N_Corners = Corners_Index'First then
                           First_Corner_ID := Current_Corner_ID;
                        end if;

                        Corners_Extra_Data_End_Indices (N_Corners) := Corners_Extra_Data.Last_Index;
                        Extra_Data_This_Corner := Extra_Data_This_Corner + 1;
                        exit Read_In_Commands when
                          Extra_Data_This_Corner
                          = Max_Corners_Extra_Data_Type'Base (Max_Corners_Extra_Data_Per_Corner);
                        exit Read_In_Commands when Corners_Extra_Data.Last_Index = Corners_Extra_Data_Index'Last;

                     when Move_Kind                        =>
                        Finish_Dequeue_And_Update_Corner_ID (Next_Command.Kind, Accepted_Corner_ID);
                        N_Corners := N_Corners + 1;
                        Extra_Data_This_Corner := 0;
                        Current_Corner_ID := Accepted_Corner_ID;
                        Corners_Extra_Data_End_Indices (N_Corners) := Corners_Extra_Data.Last_Index;
                        Corners (N_Corners) := Next_Command.Pos / Current_Params.Axial_Scaler;
                        Segment_Feedrates (N_Corners) := Next_Command.Feedrate;
                        Corner_Dwell_Times (N_Corners) := Next_Command.Dwell_After;

                        Last_Pos := Next_Command.Pos;

                        exit Read_In_Commands when N_Corners = Corners_Index'Last;
                  end case;
               end if;
            end;
         end loop Read_In_Commands;

         if Is_Extra_Data_Overflow_Block then
            Corners_Extra_Data_End_Indices (Corners_Index'First) := Corners_Extra_Data.Last_Index;
            pragma
              Assert
                (Current_Input_Corner_ID = Previous_Output_Corner_ID,
                 "Extra data overflow block corner ID changed while assembling a block.");
            Associated_Overflow_Block := Command_Queue.Next_Is_Corner_Extra_Data;
         elsif Current_Corner_ID /= 0 then
            Associated_Overflow_Block :=
              Current_Input_Corner_ID = Current_Corner_ID and then Command_Queue.Next_Is_Corner_Extra_Data;
         end if;

         Block_N_Corners := N_Corners;
         --  This is hacky and not portable, but if we try to assign to the entire record as you normally would then
         --  GCC insists on creating a whole Execution_Block on the stack.

         Block.Kind := (if Is_Extra_Data_Overflow_Block then Extra_Data_Overflow_Block_Kind else Motion_Block_Kind);
         Block.Corners_Extra_Data := Corners_Extra_Data.all;
         Block.Corners := Corners (1 .. N_Corners);
         Block.Corners_Extra_Data_End_Indices := Corners_Extra_Data_End_Indices (1 .. N_Corners);
         Block.Original_Segment_Feedrates := Segment_Feedrates (2 .. N_Corners);
         Block.First_Corner_ID :=
           (if Is_Extra_Data_Overflow_Block then Previous_Output_Corner_ID else First_Corner_ID);
         Block.Associated_Overflow_Block := Associated_Overflow_Block;
         Block.Limited_Segment_Feedrates := Segment_Feedrates (2 .. N_Corners);
         Block.Corner_Dwell_Times := Corner_Dwell_Times (2 .. N_Corners);
         Block.Flush_Resetting_Data := Flush_Resetting_Data;
         Block.Params := Current_Params;
         Block.Next_Block_Pos := Last_Pos / Next_Params.Axial_Scaler;
         Block.Is_Homing_Move := Is_Homing_Move;

         if Is_Homing_Move then
            Block.Params.Axial_Shapers := [others => (Kind => Input_Shapers.No_Shaper)];
         end if;

         Reset_Called := False;

         Current_Params := Next_Params;
      end Run;

      procedure Reset (Last_Assigned_ID : Planner_Corner_ID) is
      begin
         Setup_Done := False;
         Last_Pos := Initial_Position;
         Last_Assigned_Corner_ID := Last_Assigned_ID;
         Current_Input_Corner_ID := 0;
      end Reset;

   end Runner;

   procedure Check_Bounds (Pos : Position; Params : Kinematic_Parameters) is
   begin
      for I in Axis_Name loop
         if Pos (I) < Params.Lower_Pos_Limit (I) or else Pos (I) > Params.Upper_Pos_Limit (I) then
            raise Out_Of_Bounds_Error with "Position is out of bounds (" & I'Image & " = " & Pos (I)'Image & ").";
         end if;
      end loop;
   end Check_Bounds;

   function Limit_Higher_Order_Params (Params : Kinematic_Parameters) return Kinematic_Parameters is
      New_Params : Kinematic_Parameters := Params;
   begin
      New_Params.Tangential_Velocity_Max :=
        Velocity'Min (New_Params.Tangential_Velocity_Max, 299_792_458_000.1 * mm / s);

      New_Params.Acceleration_Max :=
        Acceleration'Min (New_Params.Acceleration_Max, New_Params.Tangential_Velocity_Max / Interpolation_Time);
      New_Params.Jerk_Max := Jerk'Min (New_Params.Jerk_Max, New_Params.Acceleration_Max / Interpolation_Time);
      New_Params.Snap_Max := Snap'Min (New_Params.Snap_Max, New_Params.Jerk_Max / Interpolation_Time);
      New_Params.Crackle_Max := Crackle'Min (New_Params.Crackle_Max, New_Params.Snap_Max / Interpolation_Time);

      return New_Params;
   end Limit_Higher_Order_Params;

end Prunt.Motion_Planner.Planner.Preprocessor;
