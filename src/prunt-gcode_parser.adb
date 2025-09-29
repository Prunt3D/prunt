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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Prunt;
with Prunt.Gcode_Arguments;
with Ada.Exceptions;

package body Prunt.Gcode_Parser is

   use Prunt;
   use Prunt.Gcode_Arguments;

   function Make_Context
     (Initial_Position   : Position;
      Initial_Feedrate   : Velocity;
      Replace_G0_With_G1 : Boolean;
      Default_Fan        : Optional_Fan;
      Default_Laser      : Optional_Laser) return Context is
   begin
      return
        (XYZ_Relative_Mode         => False,
         E_Relative_Mode           => False,
         Pos                       => Initial_Position,
         Feedrate                  => Initial_Feedrate,
         G92_Offset                => (others => Length (0.0)),
         Is_Retracted              => False,
         Current_Retraction_Offset => (others => Length (0.0)),
         M207_Offset               => (others => Length (0.0)),
         M207_Feedrate             => 299_792_458_000.1 * mm / s,
         M208_Offset               => (others => Length (0.0)),
         M208_Feedrate             => 0.0 * mm / s,
         Replace_G0_With_G1        => Replace_G0_With_G1,
         Default_Fan               => Default_Fan,
         Default_Laser             => Default_Laser);
   end Make_Context;

   procedure Parse_Line (Ctx : in out Context; Line : String; Runner : not null access procedure (Comm : Command)) is
      Args : Arguments := Parse_Arguments (Line);

      Ctx_Copy : Context := Ctx;

      Runner_Buffer : Command_Buffers.Vector (20);

      procedure Buffered_Runner (Comm : Command) is
      begin
         Runner_Buffer.Append (Comm);
      end Buffered_Runner;
   begin
      if Kind (Args, 'G') /= Non_Existant_Kind and Kind (Args, 'M') /= Non_Existant_Kind then
         raise Bad_Line with "Only one G or M parameter allowed per line.";
      end if;

      if Kind (Args, 'G') /= Non_Existant_Kind then
         declare
            G_Code : constant Argument_Integer := Consume_Integer (Args, 'G');
         begin
            case G_Code is
               when 0 =>
                  G0_Rapid_Linear_Move (Ctx_Copy, Args, Buffered_Runner'Access);

               when 1 =>
                  G1_Linear_Move (Ctx_Copy, Args, Buffered_Runner'Access);

               when 4 =>
                  G4_Dwell (Ctx_Copy, Args, Buffered_Runner'Access);

               when 10 =>
                  G10_Retract (Ctx_Copy, Args, Buffered_Runner'Access);

               when 11 =>
                  G11_Recover (Ctx_Copy, Args, Buffered_Runner'Access);

               when 21 =>
                  G21_Millimetre_Units (Ctx_Copy, Args, Buffered_Runner'Access);

               when 28 =>
                  G28_Auto_Home (Ctx, Args, Runner);
                  --  TODO: This is a hack until the calls to reset_position are reworked.
                  Ctx_Copy := Ctx;

               when 90 =>
                  G90_Absolute_Positioning (Ctx_Copy, Args, Buffered_Runner'Access);

               when 91 =>
                  G91_Relative_Positioning (Ctx_Copy, Args, Buffered_Runner'Access);

               when 92 =>
                  G92_Set_Virtual_Position (Ctx_Copy, Args, Buffered_Runner'Access);

               when others =>
                  raise Bad_Line with "Unknown G code: " & G_Code'Image;
            end case;
         end;
      elsif Kind (Args, 'M') /= Non_Existant_Kind then
         declare
            M_Code : constant Argument_Integer := Consume_Integer (Args, 'M');
         begin
            case M_Code is
               when 0 | 1 =>
                  M0_M1_Pause (Ctx_Copy, Args, Buffered_Runner'Access);

               when 3 =>
                  M3_Set_Laser_Power (Ctx_Copy, Args, Buffered_Runner'Access);

               when 5 =>
                  M5_Laser_Off (Ctx_Copy, Args, Buffered_Runner'Access);

               when 17 =>
                  M17_Enable_Motors (Ctx_Copy, Args, Buffered_Runner'Access);

               when 18 | 84 =>
                  M18_M84_Disable_Motors (Ctx_Copy, Args, Buffered_Runner'Access);

               when 82 =>
                  M82_E_Axis_Absolute (Ctx_Copy, Args, Buffered_Runner'Access);

               when 83 =>
                  M83_E_Axis_Relative (Ctx_Copy, Args, Buffered_Runner'Access);

               when 104 =>
                  M104_Set_Hotend_Temperature (Ctx_Copy, Args, Buffered_Runner'Access);

               when 106 =>
                  M106_Set_Fan_Speed (Ctx_Copy, Args, Buffered_Runner'Access);

               when 107 =>
                  M107_Fan_Off (Ctx_Copy, Args, Buffered_Runner'Access);

               when 109 =>
                  M109_Wait_For_Hotend_Temperature (Ctx_Copy, Args, Buffered_Runner'Access);

               when 122 =>
                  M122_TMC_Register_Dump (Ctx_Copy, Args, Buffered_Runner'Access);

               when 140 =>
                  M140_Set_Bed_Temperature (Ctx_Copy, Args, Buffered_Runner'Access);

               when 141 =>
                  M141_Set_Chamber_Temperature (Ctx_Copy, Args, Buffered_Runner'Access);

               when 190 =>
                  M190_Wait_For_Bed_Temperature (Ctx_Copy, Args, Buffered_Runner'Access);

               when 191 =>
                  M191_Wait_For_Chamber_Temperature (Ctx_Copy, Args, Buffered_Runner'Access);

               when 205 =>
                  M205_Set_Dynamic_Kinematic_Limits (Ctx_Copy, Args, Buffered_Runner'Access);

               when 207 =>
                  M207_Retraction_Settings (Ctx_Copy, Args, Buffered_Runner'Access);

               when 208 =>
                  M208_Recovery_Settings (Ctx_Copy, Args, Buffered_Runner'Access);

               when 303 =>
                  M303_PID_Autotune (Ctx_Copy, Args, Buffered_Runner'Access);

               when 73 | 204 | 486 =>
                  M73_M204_M486_Ignored (Ctx_Copy, Args, Buffered_Runner'Access);

               when others =>
                  raise Bad_Line with "Unknown M code: " & M_Code'Image;
            end case;
         end;
      end if;

      Validate_All_Consumed (Args);

      for Comm of Runner_Buffer loop
         Runner (Comm);
      end loop;

      Ctx := Ctx_Copy;
   exception
      when E : Parse_Error =>
         raise Bad_Line with Ada.Exceptions.Exception_Message (E);
   end Parse_Line;

   procedure Reset_Position (Ctx : in out Context; Pos : Position) is
   begin
      Ctx.Pos := Pos;
      Ctx.Current_Retraction_Offset := (others => Length (0.0));
      Ctx.Is_Retracted := False;
   end Reset_Position;

   procedure G0_Rapid_Linear_Move
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command))
   is
      Comm : Command := (Kind => Move_Kind, Is_Rapid => True, others => <>);
   begin
      if Ctx.XYZ_Relative_Mode then
         Comm.Pos (X_Axis) := Ctx.Pos (X_Axis) + Consume_Float_Or_Default (Args, 'X', 0.0) * mm;
         Comm.Pos (Y_Axis) := Ctx.Pos (Y_Axis) + Consume_Float_Or_Default (Args, 'Y', 0.0) * mm;
         Comm.Pos (Z_Axis) := Ctx.Pos (Z_Axis) + Consume_Float_Or_Default (Args, 'Z', 0.0) * mm;
      else
         Comm.Pos (X_Axis) :=
           Consume_Float_Or_Default
             (Args, 'X', (Ctx.Pos (X_Axis) - Ctx.Current_Retraction_Offset (X_Axis) - Ctx.G92_Offset (X_Axis)) / mm)
           * mm
           + Ctx.G92_Offset (X_Axis)
           + Ctx.Current_Retraction_Offset (X_Axis);
         Comm.Pos (Y_Axis) :=
           Consume_Float_Or_Default
             (Args, 'Y', (Ctx.Pos (Y_Axis) - Ctx.Current_Retraction_Offset (Y_Axis) - Ctx.G92_Offset (Y_Axis)) / mm)
           * mm
           + Ctx.G92_Offset (Y_Axis)
           + Ctx.Current_Retraction_Offset (Y_Axis);
         Comm.Pos (Z_Axis) :=
           Consume_Float_Or_Default
             (Args, 'Z', (Ctx.Pos (Z_Axis) - Ctx.Current_Retraction_Offset (Z_Axis) - Ctx.G92_Offset (Z_Axis)) / mm)
           * mm
           + Ctx.G92_Offset (Z_Axis)
           + Ctx.Current_Retraction_Offset (Z_Axis);
      end if;

      if Ctx.E_Relative_Mode then
         Comm.Pos (E_Axis) := Ctx.Pos (E_Axis) + Consume_Float_Or_Default (Args, 'E', 0.0) * mm;
      else
         Comm.Pos (E_Axis) :=
           Consume_Float_Or_Default
             (Args, 'E', (Ctx.Pos (E_Axis) - Ctx.Current_Retraction_Offset (E_Axis) - Ctx.G92_Offset (E_Axis)) / mm)
           * mm
           + Ctx.G92_Offset (E_Axis)
           + Ctx.Current_Retraction_Offset (E_Axis);
      end if;

      if Ctx.Replace_G0_With_G1 then
         --  We avoid just calling G1 here as Is_Rapid needs to be set to turn off lasers during G0 moves even when
         --  replacement is enabled.
         Comm.Feedrate := Consume_Float_Or_Default (Args, 'F', Ctx.Feedrate / (mm / min)) * mm / min;
         if Comm.Feedrate <= 0.0 * mm / s then
            raise Bad_Line with "Feedrate must be positive.";
         end if;
         Ctx.Feedrate := Comm.Feedrate;
      else
         Comm.Feedrate := Consume_Float_Or_Default (Args, 'F', 299_792_458_000.1 * 60.0) * mm / min;
         if Comm.Feedrate <= 0.0 * mm / s then
            raise Bad_Line with "Feedrate must be positive.";
         end if;
      end if;

      if Kind (Args, 'X') /= Non_Existant_Kind
        or Kind (Args, 'Y') /= Non_Existant_Kind
        or Kind (Args, 'Z') /= Non_Existant_Kind
        or Kind (Args, 'E') /= Non_Existant_Kind
      then
         if Ctx.Pos /= Comm.Pos then
            Runner (Comm);
            Ctx.Pos := Comm.Pos;
         end if;
      end if;
   end G0_Rapid_Linear_Move;

   procedure G1_Linear_Move
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command))
   is
      Comm : Command := (Kind => Move_Kind, Is_Rapid => False, others => <>);
   begin
      if Ctx.XYZ_Relative_Mode then
         Comm.Pos (X_Axis) := Ctx.Pos (X_Axis) + Consume_Float_Or_Default (Args, 'X', 0.0) * mm;
         Comm.Pos (Y_Axis) := Ctx.Pos (Y_Axis) + Consume_Float_Or_Default (Args, 'Y', 0.0) * mm;
         Comm.Pos (Z_Axis) := Ctx.Pos (Z_Axis) + Consume_Float_Or_Default (Args, 'Z', 0.0) * mm;
      else
         Comm.Pos (X_Axis) :=
           Consume_Float_Or_Default
             (Args, 'X', (Ctx.Pos (X_Axis) - Ctx.Current_Retraction_Offset (X_Axis) - Ctx.G92_Offset (X_Axis)) / mm)
           * mm
           + Ctx.G92_Offset (X_Axis)
           + Ctx.Current_Retraction_Offset (X_Axis);
         Comm.Pos (Y_Axis) :=
           Consume_Float_Or_Default
             (Args, 'Y', (Ctx.Pos (Y_Axis) - Ctx.Current_Retraction_Offset (Y_Axis) - Ctx.G92_Offset (Y_Axis)) / mm)
           * mm
           + Ctx.G92_Offset (Y_Axis)
           + Ctx.Current_Retraction_Offset (Y_Axis);
         Comm.Pos (Z_Axis) :=
           Consume_Float_Or_Default
             (Args, 'Z', (Ctx.Pos (Z_Axis) - Ctx.Current_Retraction_Offset (Z_Axis) - Ctx.G92_Offset (Z_Axis)) / mm)
           * mm
           + Ctx.G92_Offset (Z_Axis)
           + Ctx.Current_Retraction_Offset (Z_Axis);
      end if;

      if Ctx.E_Relative_Mode then
         Comm.Pos (E_Axis) := Ctx.Pos (E_Axis) + Consume_Float_Or_Default (Args, 'E', 0.0) * mm;
      else
         Comm.Pos (E_Axis) :=
           Consume_Float_Or_Default
             (Args, 'E', (Ctx.Pos (E_Axis) - Ctx.Current_Retraction_Offset (E_Axis) - Ctx.G92_Offset (E_Axis)) / mm)
           * mm
           + Ctx.G92_Offset (E_Axis)
           + Ctx.Current_Retraction_Offset (E_Axis);
      end if;

      Comm.Feedrate := Consume_Float_Or_Default (Args, 'F', Ctx.Feedrate / (mm / min)) * mm / min;
      if Comm.Feedrate <= 0.0 * mm / s then
         raise Bad_Line with "Feedrate must be positive.";
      end if;
      Ctx.Feedrate := Comm.Feedrate;

      if Kind (Args, 'X') /= Non_Existant_Kind
        or Kind (Args, 'Y') /= Non_Existant_Kind
        or Kind (Args, 'Z') /= Non_Existant_Kind
        or Kind (Args, 'E') /= Non_Existant_Kind
      then
         if Ctx.Pos /= Comm.Pos then
            Runner (Comm);
            Ctx.Pos := Comm.Pos;
         end if;
      end if;
   end G1_Linear_Move;

   procedure G4_Dwell
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if Kind (Args, 'S') /= Non_Existant_Kind then
         Runner ((Kind => Dwell_Kind, Dwell_Time => Consume_Float (Args, 'S') * s));
      else
         Runner ((Kind => Dwell_Kind, Dwell_Time => Consume_Float (Args, 'P') * ms));
      end if;
   end G4_Dwell;

   procedure G10_Retract
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if not Ctx.Is_Retracted then
         declare
            New_Pos : Position := Ctx.Pos;
         begin
            if Ctx.M207_Offset (E_Axis) /= Length (0.0) then
               New_Pos (E_Axis) := New_Pos (E_Axis) - Ctx.M207_Offset (E_Axis);
               Runner ((Kind => Move_Kind, Pos => New_Pos, Feedrate => Ctx.M207_Feedrate, Is_Rapid => False));
               Ctx.Pos := New_Pos;
               Ctx.Current_Retraction_Offset (E_Axis) :=
                 Ctx.Current_Retraction_Offset (E_Axis) - Ctx.M207_Offset (E_Axis);
            end if;

            Ctx.Is_Retracted := True;

            if Ctx.M207_Offset (Z_Axis) /= Length (0.0) then
               New_Pos (Z_Axis) := New_Pos (Z_Axis) + Ctx.M207_Offset (Z_Axis);
               Runner ((Kind => Move_Kind, Pos => New_Pos, Feedrate => Velocity'Last, Is_Rapid => False));
               Ctx.Pos := New_Pos;
               Ctx.Current_Retraction_Offset (Z_Axis) := Ctx.M207_Offset (Z_Axis);
            end if;
         end;
      end if;
   end G10_Retract;

   procedure G11_Recover
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if Ctx.Is_Retracted then
         declare
            New_Pos : Position := Ctx.Pos;
         begin
            if Ctx.Current_Retraction_Offset (Z_Axis) /= Length (0.0) then
               New_Pos (Z_Axis) := New_Pos (Z_Axis) - Ctx.Current_Retraction_Offset (Z_Axis);
               Runner ((Kind => Move_Kind, Pos => New_Pos, Feedrate => Velocity'Last, Is_Rapid => False));
               Ctx.Pos := New_Pos;
               Ctx.Current_Retraction_Offset (Z_Axis) := Length (0.0);
            end if;

            if Ctx.M207_Offset (E_Axis) /= Length (0.0) then
               New_Pos (E_Axis) := New_Pos (E_Axis) + Ctx.M207_Offset (E_Axis) + Ctx.M208_Offset (E_Axis);
               Runner
                 ((Kind     => Move_Kind,
                   Pos      => New_Pos,
                   Feedrate => Ctx.M207_Feedrate + Ctx.M208_Feedrate,
                   Is_Rapid => False));
               Ctx.Pos := New_Pos;
               Ctx.Current_Retraction_Offset (E_Axis) :=
                 Ctx.Current_Retraction_Offset (E_Axis) + Ctx.M207_Offset (E_Axis) + Ctx.M208_Offset (E_Axis);
            end if;

            Ctx.Is_Retracted := False;
         end;
      end if;
   end G11_Recover;

   procedure G21_Millimetre_Units
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      null;
   end G21_Millimetre_Units;

   procedure G28_Auto_Home
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if Kind (Args, 'E') = Non_Existant_Kind
        and Kind (Args, 'X') = Non_Existant_Kind
        and Kind (Args, 'Y') = Non_Existant_Kind
        and Kind (Args, 'Z') = Non_Existant_Kind
      then
         Runner ((Kind => Home_Kind, Axes => (others => True), Pos_Before_Homing => Ctx.Pos));
      else
         Runner
           ((Kind              => Home_Kind,
             Axes              =>
               (E_Axis => Consume_No_Value_Or_False (Args, 'E'),
                X_Axis => Consume_No_Value_Or_False (Args, 'X'),
                Y_Axis => Consume_No_Value_Or_False (Args, 'Y'),
                Z_Axis => Consume_No_Value_Or_False (Args, 'Z')),
             Pos_Before_Homing => Ctx.Pos));
      end if;
   end G28_Auto_Home;

   procedure G90_Absolute_Positioning
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      Ctx.XYZ_Relative_Mode := False;
      Ctx.E_Relative_Mode := False;
   end G90_Absolute_Positioning;

   procedure G91_Relative_Positioning
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      Ctx.XYZ_Relative_Mode := True;
      Ctx.E_Relative_Mode := True;
   end G91_Relative_Positioning;

   procedure G92_Set_Virtual_Position
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if Kind (Args, 'E') /= Non_Existant_Kind then
         Ctx.G92_Offset (E_Axis) := Ctx.Pos (E_Axis) - Consume_Float (Args, 'E') * mm;
      end if;
      if Kind (Args, 'X') /= Non_Existant_Kind then
         Ctx.G92_Offset (X_Axis) := Ctx.Pos (X_Axis) - Consume_Float (Args, 'X') * mm;
      end if;
      if Kind (Args, 'Y') /= Non_Existant_Kind then
         Ctx.G92_Offset (Y_Axis) := Ctx.Pos (Y_Axis) - Consume_Float (Args, 'Y') * mm;
      end if;
      if Kind (Args, 'Z') /= Non_Existant_Kind then
         Ctx.G92_Offset (Z_Axis) := Ctx.Pos (Z_Axis) - Consume_Float (Args, 'Z') * mm;
      end if;
   end G92_Set_Virtual_Position;

   procedure M0_M1_Pause
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      Runner ((Kind => Pause_Kind));
   end M0_M1_Pause;

   procedure M3_Set_Laser_Power
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command))
   is
      Comm : Command;
   begin
      if not Has_Lasers then
         raise Bad_Line with "This board does not support lasers. Laser commands can not be used.";
      end if;

      Comm :=
        (Kind         => Set_Laser_Power_Kind,
         Laser_Power  => Dimensionless'Min (1.0, Dimensionless'Max (0.0, Consume_Float (Args, 'S') / 255.0)),
         Laser_To_Set => Ctx.Default_Laser.Name);

      if Kind (Args, 'P') /= Non_Existant_Kind then
         if Kind (Args, 'P') = Integer_Kind then
            declare
               Index : constant Argument_Integer := Consume_Integer (Args, 'P');
            begin
               Comm.Laser_To_Set := Laser_Name'Enum_Val (Integer (Index));
            exception
               when Constraint_Error =>
                  raise Bad_Line with "Invalid laser index (" & Index'Image & ").";
            end;
         else
            declare
               Name : constant String := Consume_String (Args, 'P');
            begin
               Comm.Laser_To_Set := Laser_Name'Value (Name);
            exception
               when Constraint_Error =>
                  raise Bad_Line with "Invalid laser name (" & Name & ").";
            end;
         end if;
      end if;

      Runner (Comm);
   end M3_Set_Laser_Power;

   procedure M5_Laser_Off
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command))
   is
      Comm : Command;
   begin
      if not Has_Lasers then
         raise Bad_Line with "This board does not support lasers. Laser commands can not be used.";
      end if;

      Comm := (Kind => Set_Laser_Power_Kind, Laser_Power => 0.0, Laser_To_Set => Ctx.Default_Laser.Name);

      if Kind (Args, 'P') /= Non_Existant_Kind then
         if Kind (Args, 'P') = Integer_Kind then
            declare
               Index : constant Argument_Integer := Consume_Integer (Args, 'P');
            begin
               Comm.Laser_To_Set := Laser_Name'Enum_Val (Integer (Index));
            exception
               when Constraint_Error =>
                  raise Bad_Line with "Invalid laser index (" & Index'Image & ").";
            end;
         else
            declare
               Name : constant String := Consume_String (Args, 'P');
            begin
               Comm.Laser_To_Set := Laser_Name'Value (Name);
            exception
               when Constraint_Error =>
                  raise Bad_Line with "Invalid laser name (" & Name & ").";
            end;
         end if;
      end if;

      Runner (Comm);
   end M5_Laser_Off;

   procedure M17_Enable_Motors
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if Kind (Args, 'E') = Non_Existant_Kind
        and Kind (Args, 'X') = Non_Existant_Kind
        and Kind (Args, 'Y') = Non_Existant_Kind
        and Kind (Args, 'Z') = Non_Existant_Kind
      then
         Runner ((Kind => Enable_Steppers_Kind, Axes => (others => True)));
      else
         Runner
           ((Kind => Enable_Steppers_Kind,
             Axes =>
               (E_Axis => Consume_No_Value_Or_False (Args, 'E'),
                X_Axis => Consume_No_Value_Or_False (Args, 'X'),
                Y_Axis => Consume_No_Value_Or_False (Args, 'Y'),
                Z_Axis => Consume_No_Value_Or_False (Args, 'Z'))));
      end if;
   end M17_Enable_Motors;

   procedure M18_M84_Disable_Motors
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if Kind (Args, 'E') = Non_Existant_Kind
        and Kind (Args, 'X') = Non_Existant_Kind
        and Kind (Args, 'Y') = Non_Existant_Kind
        and Kind (Args, 'Z') = Non_Existant_Kind
      then
         Runner ((Kind => Disable_Steppers_Kind, Axes => (others => True)));
      else
         Runner
           ((Kind => Disable_Steppers_Kind,
             Axes =>
               (E_Axis => Consume_No_Value_Or_False (Args, 'E'),
                X_Axis => Consume_No_Value_Or_False (Args, 'X'),
                Y_Axis => Consume_No_Value_Or_False (Args, 'Y'),
                Z_Axis => Consume_No_Value_Or_False (Args, 'Z'))));
      end if;
   end M18_M84_Disable_Motors;

   procedure M82_E_Axis_Absolute
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      Ctx.E_Relative_Mode := False;
   end M82_E_Axis_Absolute;

   procedure M83_E_Axis_Relative
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      Ctx.E_Relative_Mode := True;
   end M83_E_Axis_Relative;

   procedure M104_Set_Hotend_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if not Has_Heaters then
         raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
      end if;
      declare
         Ignored : Argument_Integer := Consume_Integer_Or_Default (Args, 'T', 0);
      begin
         Runner ((Kind => Set_Hotend_Temperature_Kind, Target_Temperature => Consume_Float (Args, 'S') * celsius));
      end;
   end M104_Set_Hotend_Temperature;

   procedure M106_Set_Fan_Speed
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command))
   is
      Comm : Command;
   begin
      if not Has_Fans then
         raise Bad_Line with "This board does not support fans. Fan commands can not be used.";
      end if;

      Comm :=
        (Kind       => Set_Fan_Speed_Kind,
         Fan_Speed  =>
           Dimensionless'Min (1.0, Dimensionless'Max (0.0, Consume_Float_Or_Default (Args, 'S', 255.0) / 255.0)),
         Fan_To_Set => Ctx.Default_Fan.Name);

      if Kind (Args, 'P') /= Non_Existant_Kind then
         if Kind (Args, 'P') = Integer_Kind then
            declare
               Index : constant Argument_Integer := Consume_Integer (Args, 'P');
            begin
               Comm.Fan_To_Set := Fan_Name'Enum_Val (Integer (Index));
            exception
               when Constraint_Error =>
                  raise Bad_Line with "Invalid fan index (" & Index'Image & ").";
            end;
         else
            declare
               Name : constant String := Consume_String (Args, 'P');
            begin
               Comm.Fan_To_Set := Fan_Name'Value (Name);
            exception
               when Constraint_Error =>
                  raise Bad_Line with "Invalid fan name (" & Name & ").";
            end;
         end if;
      end if;

      Runner (Comm);
   end M106_Set_Fan_Speed;

   procedure M107_Fan_Off
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command))
   is
      Comm : Command;
   begin
      if not Has_Fans then
         raise Bad_Line with "This board does not support fans. Fan commands can not be used.";
      end if;

      Comm := (Kind => Set_Fan_Speed_Kind, Fan_Speed => 0.0, Fan_To_Set => Ctx.Default_Fan.Name);

      if Kind (Args, 'P') /= Non_Existant_Kind then
         if Kind (Args, 'P') = Integer_Kind then
            declare
               Index : constant Argument_Integer := Consume_Integer (Args, 'P');
            begin
               Comm.Fan_To_Set := Fan_Name'Enum_Val (Integer (Index));
            exception
               when Constraint_Error =>
                  raise Bad_Line with "Invalid fan index (" & Index'Image & ").";
            end;
         else
            declare
               Name : constant String := Consume_String (Args, 'P');
            begin
               Comm.Fan_To_Set := Fan_Name'Value (Name);
            exception
               when Constraint_Error =>
                  raise Bad_Line with "Invalid fan name (" & Name & ").";
            end;
         end if;
      end if;

      Runner (Comm);
   end M107_Fan_Off;

   procedure M109_Wait_For_Hotend_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if not Has_Heaters then
         raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
      end if;

      Runner ((Kind => Wait_Hotend_Temperature_Kind, Target_Temperature => Consume_Float (Args, 'S') * celsius));
   end M109_Wait_For_Hotend_Temperature;

   procedure M122_TMC_Register_Dump
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      Runner ((Kind => TMC_Dump_Kind));
   end M122_TMC_Register_Dump;

   procedure M140_Set_Bed_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if not Has_Heaters then
         raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
      end if;

      Runner ((Kind => Set_Bed_Temperature_Kind, Target_Temperature => Consume_Float (Args, 'S') * celsius));
   end M140_Set_Bed_Temperature;

   procedure M141_Set_Chamber_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if not Has_Heaters then
         raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
      end if;

      Runner ((Kind => Set_Chamber_Temperature_Kind, Target_Temperature => Consume_Float (Args, 'S') * celsius));
   end M141_Set_Chamber_Temperature;

   procedure M190_Wait_For_Bed_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if not Has_Heaters then
         raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
      end if;

      Runner ((Kind => Wait_Bed_Temperature_Kind, Target_Temperature => Consume_Float (Args, 'S') * celsius));
   end M190_Wait_For_Bed_Temperature;

   procedure M191_Wait_For_Chamber_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if not Has_Heaters then
         raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
      end if;

      Runner ((Kind => Wait_Chamber_Temperature_Kind, Target_Temperature => Consume_Float (Args, 'S') * celsius));
   end M191_Wait_For_Chamber_Temperature;

   procedure M205_Set_Dynamic_Kinematic_Limits
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      if not Consume_No_Value_Or_False (Args, 'P') then
         raise Bad_Line
           with "M205 requires P parameter with no value on Prunt to prevent conflicts with Marlin g-code.";
      end if;

      if Kind (Args, 'A') /= Non_Existant_Kind then
         Runner ((Kind => Set_Acceleration_Max_Kind, Acceleration_Max => Consume_Float (Args, 'A') * mm / s**2));
      elsif Kind (Args, 'J') /= Non_Existant_Kind then
         Runner ((Kind => Set_Jerk_Max_Kind, Jerk_Max => Consume_Float (Args, 'J') * mm / s**3));
      elsif Kind (Args, 'S') /= Non_Existant_Kind then
         Runner ((Kind => Set_Snap_Max_Kind, Snap_Max => Consume_Float (Args, 'S') * mm / s**4));
      elsif Kind (Args, 'C') /= Non_Existant_Kind then
         Runner ((Kind => Set_Crackle_Max_Kind, Crackle_Max => Consume_Float (Args, 'C') * mm / s**5));
      elsif Kind (Args, 'D') /= Non_Existant_Kind then
         Runner ((Kind => Set_Chord_Error_Max_Kind, Chord_Error_Max => Consume_Float (Args, 'D') * mm));
      elsif Kind (Args, 'L') /= Non_Existant_Kind then
         Runner ((Kind => Set_Pressure_Advance_Time_Kind, Pressure_Advance_Time => Consume_Float (Args, 'L') * s));
      end if;
   end M205_Set_Dynamic_Kinematic_Limits;

   procedure M207_Retraction_Settings
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      Ctx.M207_Feedrate := Consume_Float_Or_Default (Args, 'F', Ctx.M207_Feedrate / (mm / min)) * (mm / min);
      Ctx.M207_Offset (E_Axis) := Consume_Float_Or_Default (Args, 'E', Ctx.M207_Offset (E_Axis) / mm) * mm;
      Ctx.M207_Offset (Z_Axis) := Consume_Float_Or_Default (Args, 'Z', Ctx.M207_Offset (Z_Axis) / mm) * mm;
   end M207_Retraction_Settings;

   procedure M208_Recovery_Settings
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      Ctx.M208_Feedrate := Consume_Float_Or_Default (Args, 'F', Ctx.M208_Feedrate / (mm / min)) * (mm / min);
      Ctx.M208_Offset (E_Axis) := Consume_Float_Or_Default (Args, 'S', Ctx.M208_Offset (E_Axis) / mm) * mm;
   end M208_Recovery_Settings;

   procedure M303_PID_Autotune
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command))
   is
      Comm : Command :=
        (Kind               => Heater_Autotune_Kind,
         Tuning_Temperature => Consume_Float (Args, 'S') * celsius,
         Heater_To_Tune     => <>,
         Max_Cycles         => <>);
   begin
      if not Has_Heaters then
         raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
      end if;

      declare
         Cycles : constant Argument_Integer := Consume_Integer_Or_Default (Args, 'C', 5);
      begin
         if Integer (Cycles) < Integer (PID_Autotune_Cycle_Count'First)
           or Integer (Cycles) > Integer (PID_Autotune_Cycle_Count'Last)
         then
            raise Bad_Line
              with
                "Valid range for parameter 'C' is "
                & PID_Autotune_Cycle_Count'First'Image
                & " .. "
                & PID_Autotune_Cycle_Count'Last'Image;
         else
            Comm.Max_Cycles := PID_Autotune_Cycle_Count (Cycles);
         end if;
      end;

      if Kind (Args, 'T') = Integer_Kind then
         declare
            Index : constant Argument_Integer := Consume_Integer (Args, 'T');
         begin
            Comm.Heater_To_Tune := Heater_Name'Enum_Val (Integer (Index));
         exception
            when Constraint_Error =>
               raise Bad_Line with "Invalid heater index (" & Index'Image & ").";
         end;
      else
         declare
            Name : constant String := Consume_String (Args, 'T');
         begin
            Comm.Heater_To_Tune := Heater_Name'Value (Name);
         exception
            when Constraint_Error =>
               raise Bad_Line with "Invalid heater name (" & Name & ").";
         end;
      end if;
      Runner (Comm);
   end M303_PID_Autotune;

   procedure M73_M204_M486_Ignored
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command)) is
   begin
      null;
   end M73_M204_M486_Ignored;

end Prunt.Gcode_Parser;
