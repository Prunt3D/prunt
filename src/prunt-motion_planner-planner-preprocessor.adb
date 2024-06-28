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

   procedure Enqueue (Comm : Command; Ignore_Bounds : Boolean := False) is
   begin
      case Comm.Kind is
         when Flush_Kind =>
            null;
         when Flush_And_Reset_Position_Kind =>
            if not Ignore_Bounds then
               Check_Bounds (Comm.Reset_Pos);
            end if;
         when Flush_And_Change_Parameters_Kind =>
            if Comm.New_Params.Higher_Order_Scaler /= Params.Higher_Order_Scaler then
               raise Constraint_Error with "Changing of the scaler at runtime is not currently supported.";
            end if;
         when Move_Kind =>
            if not Ignore_Bounds then
               Check_Bounds (Comm.Pos);
            end if;
      end case;

      Command_Queue.Enqueue (Comm);
   end Enqueue;

   procedure Setup (Initial_Parameters : Kinematic_Parameters) is
   begin
      if Setup_Done then
         raise Constraint_Error with "Setup already called.";
      end if;

      Params := Initial_Parameters;

      Setup_Done := True;
   end Setup;

   procedure Run (Block : aliased out Execution_Block) is
      Flush_Extra_Data : Flush_Extra_Data_Type := Flush_Extra_Data_Default;
      N_Corners        : Corners_Index := 1;
      Block_N_Corners  : Corners_Index with
        Address => Block.N_Corners'Address;
      Next_Params      : Kinematic_Parameters;
   begin
      if not Setup_Done then
         raise Constraint_Error with "Setup not done.";
      end if;

      Next_Params := Params;

      Corners (1) := Last_Pos * Params.Higher_Order_Scaler;

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
                  Next_Params      := Next_Command.New_Params;
                  exit;
               when Move_Kind =>
                  --  if abs (Last_Pos - Next_Command.Pos) >= Preprocessor_Minimum_Move_Distance then
                     N_Corners                      := N_Corners + 1;
                     Corners (N_Corners)            := Next_Command.Pos * Params.Higher_Order_Scaler;
                     Corners_Extra_Data (N_Corners) := Next_Command.Corner_Extra_Data;

                     --  Adjust tangential velocity limit to account for scale.
                     declare
                        Unscaled_Offset   : constant Position_Offset := Last_Pos - Next_Command.Pos;
                        Unscaled_Distance : constant Length          := abs (Unscaled_Offset);
                        Scaled_Distance   : constant Length          :=
                          abs (Corners (N_Corners - 1) - Corners (N_Corners));
                     begin
                        if Unscaled_Distance = 0.0 * mm then
                           Segment_Feedrates (N_Corners) := 0.0 * mm / s;
                        else
                           Segment_Feedrates (N_Corners) :=
                             Enforce_Feedrate_Limits (Unscaled_Offset, Next_Command.Feedrate) *
                             (Scaled_Distance / Unscaled_Distance);
                        end if;
                     end;

                     Last_Pos := Next_Command.Pos;

                     exit when N_Corners = Corners_Index'Last;
                  --  end if;
            end case;
         end;
      end loop;

      Block_N_Corners := N_Corners;
      --  This is hacky and not portable, but if we try to assign to the entire record as you normally would then GCC
      --  insists on creating a whole Execution_Block on the stack.

      Block.Corners            := Corners (1 .. N_Corners);
      Block.Corners_Extra_Data := Corners_Extra_Data (2 .. N_Corners);
      Block.Segment_Feedrates  := Segment_Feedrates (2 .. N_Corners);
      Block.Flush_Extra_Data   := Flush_Extra_Data;
      Block.Params             := Params;
      Block.Next_Block_Pos     := Last_Pos * Params.Higher_Order_Scaler;

      Params := Next_Params;
   end Run;

   procedure Check_Bounds (Pos : Position) is
   begin
      for I in Axis_Name loop
         if Pos (I) < Params.Lower_Pos_Limit (I) or Pos (I) > Params.Upper_Pos_Limit (I) then
            raise Out_Of_Bounds_Error with "Position is out of bounds (" & I'Image & " = " & Pos (I)'Image & ").";
         end if;
      end loop;
   end Check_Bounds;

   function Enforce_Feedrate_Limits (Offset : Position_Offset; Feedrate : Velocity) return Velocity is
      Has_XYZ : constant Boolean := [Offset with delta E_Axis => 0.0 * mm] /= Position_Offset'[others => Length (0.0)];
      Limited_Feedrate : Velocity := Feedrate;
   begin
      if Params.Ignore_E_In_XYZE and Has_XYZ and Limited_Feedrate /= Velocity'Last then
         Limited_Feedrate := Limited_Feedrate * abs Offset / abs [Offset with delta E_Axis => 0.0 * mm];
      end if;

      if Limited_Feedrate > Params.Tangential_Velocity_Max then
         Limited_Feedrate := Params.Tangential_Velocity_Max;
      end if;

      for I in Axis_Name loop
         if abs Offset (I) > 0.0 * mm then
            Limited_Feedrate :=
              Velocity'Min (Limited_Feedrate, Params.Axial_Velocity_Maxes (I) * abs Offset / abs Offset (I));
         end if;
      end loop;

      return Limited_Feedrate;
   end Enforce_Feedrate_Limits;

end Prunt.Motion_Planner.Planner.Preprocessor;
