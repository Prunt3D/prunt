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

package body Prunt.Motion_Planner.Planner.Preprocessor is

   protected body Command_Queue is
      procedure Setup (Initial_Parameters : Kinematic_Parameters) is
      begin
         if Setup_Done then
            raise Constraint_Error with "Setup already done.";
         end if;

         Current_Params := Initial_Parameters;

         Setup_Done := True;
      end Setup;

      entry Enqueue (Comm : Command; Ignore_Bounds : Boolean := False) when Setup_Done and not Is_Full is
      begin
         case Comm.Kind is
            when Flush_Kind =>
               null;
            when Flush_And_Reset_Position_Kind =>
               if not Ignore_Bounds then
                  Check_Bounds (Comm.Reset_Pos, Current_Params);
               end if;
            when Flush_And_Change_Parameters_Kind =>
               Current_Params := Comm.New_Params;
               --  TODO: Check that scaler will not cause max step rate to be exceeded.
            when Move_Kind =>
               if not Ignore_Bounds then
                  Check_Bounds (Comm.Pos, Current_Params);
               end if;
         end case;
         --  Checking happens here so we can provide instant feedback to the user when g-code is typed in manually.

         Elements (Next_Write) := Comm;

         if Next_Write = Elements'Last then
            Next_Write := Elements'First;
         else
            Next_Write := @ + 1;
         end if;

         if Next_Write = Next_Read then
            Is_Full := True;
         end if;
      end Enqueue;

      entry Dequeue (Comm : out Command) when Is_Full or Next_Read /= Next_Write is
      begin
         Comm := Elements (Next_Read);

         if Next_Read = Elements'Last then
            Next_Read := Elements'First;
         else
            Next_Read := @ + 1;
         end if;
         Is_Full := False;
      end Dequeue;
   end Command_Queue;

   procedure Enqueue (Comm : Command; Ignore_Bounds : Boolean := False) is
   begin
      Command_Queue.Enqueue (Comm, Ignore_Bounds);
   end Enqueue;

   procedure Run (Block : aliased out Execution_Block) is
   begin
      Runner.Run (Block);
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

      procedure Run (Block : aliased out Execution_Block) is
         Flush_Extra_Data : Flush_Extra_Data_Type := Flush_Extra_Data_Default;
         N_Corners        : Corners_Index         := 1;
         Block_N_Corners  : Corners_Index with
           Address => Block.N_Corners'Address;
         Next_Params      : Kinematic_Parameters;
      begin
         if not Setup_Done then
            raise Constraint_Error with "Setup not done.";
         end if;

         Next_Params := Current_Params;

         Corners (1) := Last_Pos / Current_Params.Axial_Scaler;

         loop
            declare
               Next_Command : Command;
            begin
               Command_Queue.Dequeue (Next_Command);

               case Next_Command.Kind is
                  when Flush_Kind =>
                     Flush_Extra_Data := Next_Command.Flush_Extra_Data;
                     exit;
                  when Flush_And_Reset_Position_Kind =>
                     Flush_Extra_Data := Next_Command.Flush_Extra_Data;
                     Last_Pos         := Next_Command.Reset_Pos;
                     exit;
                  when Flush_And_Change_Parameters_Kind =>
                     Flush_Extra_Data := Next_Command.Flush_Extra_Data;
                     Next_Params      := Limit_Higher_Order_Params (Next_Command.New_Params);
                     exit;
                  when Move_Kind =>
                     --  if abs (Last_Pos - Next_Command.Pos) >= Preprocessor_Minimum_Move_Distance then
                     N_Corners                      := N_Corners + 1;
                     Corners (N_Corners)            := Next_Command.Pos / Current_Params.Axial_Scaler;
                     Corners_Extra_Data (N_Corners) := Next_Command.Corner_Extra_Data;
                     Segment_Feedrates (N_Corners)  := Next_Command.Feedrate;

                     if N_Corners > 2 and then abs (Corners (N_Corners) - Corners (N_Corners - 1)) = 0.0 * mm
                       and then abs (Corners (N_Corners - 1) - Corners (N_Corners - 2)) = 0.0 * mm
                     then
                        Corners (N_Corners - 1)            := Corners (N_Corners);
                        Corners_Extra_Data (N_Corners - 1) := Corners_Extra_Data (N_Corners);
                        Segment_Feedrates (N_Corners - 1)  := Segment_Feedrates (N_Corners);
                        N_Corners                          := N_Corners - 1;
                        --  Remove repeated zero-length segments.
                     end if;

                     Last_Pos := Next_Command.Pos;

                     exit when N_Corners = Corners_Index'Last;
                     --  end if;
               end case;
            end;
         end loop;

         Block_N_Corners := N_Corners;
         --  This is hacky and not portable, but if we try to assign to the entire record as you normally would then
         --  GCC insists on creating a whole Execution_Block on the stack.

         Block.Corners            := Corners (1 .. N_Corners);
         Block.Corners_Extra_Data := Corners_Extra_Data (2 .. N_Corners);
         Block.Segment_Feedrates  := Segment_Feedrates (2 .. N_Corners);
         Block.Flush_Extra_Data   := Flush_Extra_Data;
         Block.Params             := Current_Params;
         Block.Next_Block_Pos     := Last_Pos / Next_Params.Axial_Scaler;

         Current_Params := Next_Params;
      end Run;
   end Runner;

   procedure Check_Bounds (Pos : Position; Params : Kinematic_Parameters) is
   begin
      for I in Axis_Name loop
         if Pos (I) < Params.Lower_Pos_Limit (I) or Pos (I) > Params.Upper_Pos_Limit (I) then
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
      New_Params.Jerk_Max         := Jerk'Min (New_Params.Jerk_Max, New_Params.Acceleration_Max / Interpolation_Time);
      New_Params.Snap_Max         := Snap'Min (New_Params.Snap_Max, New_Params.Jerk_Max / Interpolation_Time);
      New_Params.Crackle_Max      := Crackle'Min (New_Params.Crackle_Max, New_Params.Snap_Max / Interpolation_Time);

      return New_Params;
   end Limit_Higher_Order_Params;

end Prunt.Motion_Planner.Planner.Preprocessor;
