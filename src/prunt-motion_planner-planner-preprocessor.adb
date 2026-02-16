-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
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
               null;

            when Corner_Extra_Data_Kind           =>
               if not Extra_Data_Storage.Is_Empty then
                  --  TODO: We should implement a ring buffer for this so we can store more than a single element at a
                  --  time.
                  requeue High_Priority_Enqueue;
               end if;

               begin
                  Extra_Data_Storage.Append (Extra.all);
               exception
                  when Corner_Extra_Data_Vectors.Out_Of_Space_Error =>
                     --  TODO: Once we implement a ring buffer for this, we can call requeue here:
                     --  requeue High_Priority_Enqueue;
                     raise Constraint_Error with "Extra corner data is too big.";
               end;

            when Flush_And_Reset_Position_Kind    =>
               if not Ignore_Bounds then
                  Check_Bounds (Comm.Reset_Pos, Current_Params);
               end if;

            when Flush_And_Change_Parameters_Kind =>
               Current_Params := Comm.New_Params;

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

         end case;
         --  Checking happens here so we can provide instant feedback to the user when g-code is typed in manually.

         Append_To_Queue (Comm);
      end Enqueue;

      entry High_Priority_Enqueue
        (Comm : Command; Ignore_Bounds : Boolean := False; Extra : access constant Corner_Extra_Data_Type := null)
        when not Is_Full and then Retry_High_Priority and then Extra_Data_Storage.Is_Empty
        --  TODO: Remove Is_Empty check once we have a ring buffer.
      is
      begin
         if not Setup_Done then
            return;
         end if;

         Retry_High_Priority := False;

         case Comm.Kind is
            when Corner_Extra_Data_Kind =>
               begin
                  Extra_Data_Storage.Append (Extra.all);
               exception
                  when Corner_Extra_Data_Vectors.Out_Of_Space_Error =>
                     requeue High_Priority_Enqueue;
               end;

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
         return Extra_Data_Storage.Element (Corners_Extra_Data_Index'First);
         --  TODO: Will need to be changed for ring buffer.
      end Dequeue_Extra_Data;

      procedure Finish_Dequeue is
         Current_Comm : constant Command := Elements (Next_Read);
      begin
         if not In_Dequeue then
            raise Constraint_Error with "Not in dequeue transaction.";
         end if;

         if Current_Comm.Kind = Corner_Extra_Data_Kind then
            --  TODO: Will need to be changed for ring buffer.
            Extra_Data_Storage.Clear;
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
      end Reset;
   end Command_Queue;

   procedure Enqueue
     (Comm : Command; Ignore_Bounds : Boolean := False; Extra : access constant Corner_Extra_Data_Type := null) is
   begin
      Command_Queue.Enqueue (Comm, Ignore_Bounds, Extra);
   end Enqueue;

   procedure Reset is
   begin
      Command_Queue.Reset;
      Runner.Reset;
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
         Flush_Resetting_Data   : Flush_Resetting_Data_Type := Flush_Resetting_Data_Type_Default;
         N_Corners              : Corners_Index := 1;
         Block_N_Corners        : Corners_Index
         with Address => Block.N_Corners'Address;
         Next_Params            : Kinematic_Parameters;
         Extra_Data_This_Corner : Max_Corners_Extra_Data_Type'Base := 0;
         Next_Extra_Data        : Corners_Extra_Data_Index := Corners_Extra_Data_Index'First;
         Is_Homing_Move         : Boolean := False;
      begin
         Reset_Called := False;

         if not Setup_Done then
            raise Constraint_Error with "Setup not done.";
         end if;

         Corners_Extra_Data.Clear;

         Next_Params := Current_Params;

         Corners (1) := Last_Pos / Current_Params.Axial_Scaler;
         Corners_Extra_Data_End_Indices (1) := Next_Extra_Data;
         --  All other arrays start from the second corner.

         Read_In_Commands : loop
            declare
               Next_Command : Command;
            begin
               pragma Warnings (Off, "potentially blocking operation in protected operation");
               Command_Queue.Dequeue (Next_Command, Reset_Called);
               pragma Warnings (On, "potentially blocking operation in protected operation");

               if Reset_Called then
                  return;
               end if;

               case Next_Command.Kind is
                  when Flush_Kind                       =>
                     Command_Queue.Finish_Dequeue;
                     Flush_Resetting_Data := Next_Command.Flush_Resetting_Data;
                     Is_Homing_Move := Next_Command.Is_Homing_Move;
                     exit Read_In_Commands;

                  when Flush_And_Reset_Position_Kind    =>
                     Command_Queue.Finish_Dequeue;
                     Flush_Resetting_Data := Next_Command.Flush_Resetting_Data;
                     Last_Pos := Next_Command.Reset_Pos;
                     Is_Homing_Move := Next_Command.Is_Homing_Move;
                     exit Read_In_Commands;

                  when Flush_And_Change_Parameters_Kind =>
                     Command_Queue.Finish_Dequeue;
                     Flush_Resetting_Data := Next_Command.Flush_Resetting_Data;
                     Next_Params := Limit_Higher_Order_Params (Next_Command.New_Params);
                     Is_Homing_Move := Next_Command.Is_Homing_Move;
                     exit Read_In_Commands;

                  when Corner_Extra_Data_Kind           =>
                     begin
                        Corners_Extra_Data.Append (Command_Queue.Dequeue_Extra_Data);
                     exception
                        when Corner_Extra_Data_Vectors.Out_Of_Space_Error =>
                           if Corners_Extra_Data.Is_Empty then
                              raise Constraint_Error with "Extra corner data too big.";
                           end if;
                           Command_Queue.Cancel_Dequeue;
                           --  This will be seen as a `Corner_Extra_Data_Kind` command by the next call to `Run` and it
                           --  will be applied to the first corner, which is will be equal to this one.
                           exit Read_In_Commands;
                     end;
                     Command_Queue.Finish_Dequeue;
                     Corners_Extra_Data_End_Indices (N_Corners) := Next_Extra_Data;
                     exit Read_In_Commands when
                       Extra_Data_This_Corner = Max_Corners_Extra_Data_Type'Base (Max_Corners_Extra_Data_Per_Corner);
                     Extra_Data_This_Corner := @ + 1;
                     exit Read_In_Commands when Next_Extra_Data = Corners_Extra_Data_Index'Last;
                     Next_Extra_Data := @ + 1;

                  when Move_Kind                        =>
                     Command_Queue.Finish_Dequeue;
                     N_Corners := N_Corners + 1;
                     Extra_Data_This_Corner := 0;
                     Corners_Extra_Data_End_Indices (N_Corners) := Next_Extra_Data;
                     Corners (N_Corners) := Next_Command.Pos / Current_Params.Axial_Scaler;
                     Segment_Feedrates (N_Corners) := Next_Command.Feedrate;
                     Corner_Dwell_Times (N_Corners) := Next_Command.Dwell_After;

                     Last_Pos := Next_Command.Pos;

                     exit Read_In_Commands when N_Corners = Corners_Index'Last;
               end case;
            end;
         end loop Read_In_Commands;

         Block_N_Corners := N_Corners;
         --  This is hacky and not portable, but if we try to assign to the entire record as you normally would then
         --  GCC insists on creating a whole Execution_Block on the stack.

         Block.Corners_Extra_Data := Corners_Extra_Data.all;
         Block.Corners := Corners (1 .. N_Corners);
         Block.Corners_Extra_Data_End_Indices := Corners_Extra_Data_End_Indices (1 .. N_Corners);
         Block.Original_Segment_Feedrates := Segment_Feedrates (2 .. N_Corners);
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

      procedure Reset is
      begin
         Setup_Done := False;
         Last_Pos := Initial_Position;
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
