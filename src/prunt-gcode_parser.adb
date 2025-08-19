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

package body Prunt.Gcode_Parser is

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
         M207_Feedrate             => Velocity'Last,
         M208_Offset               => (others => Length (0.0)),
         M208_Feedrate             => 0.0 * mm / s,
         Replace_G0_With_G1        => Replace_G0_With_G1,
         Default_Fan               => Default_Fan,
         Default_Laser             => Default_Laser);
   end Make_Context;

   procedure Parse_Line (Ctx : in out Context; Line : String; Runner : Command_Runner) is
      type Parameter_Kind is (Non_Existant_Kind, No_Value_Kind, Integer_Kind, Float_Kind, String_Kind);

      type Parameter_Integer is range 0 .. 999;

      type Parameter (Kind : Parameter_Kind := Non_Existant_Kind) is record
         Consumed : Boolean;
         case Kind is
            when Non_Existant_Kind =>
               null;

            when No_Value_Kind =>
               null;

            when Integer_Kind =>
               Integer_Value : Parameter_Integer;

            when Float_Kind =>
               Float_Value : Dimensionless;

            when String_Kind =>
               Begin_Quote : Positive;
               End_Quote   : Positive;
         end case;
      end record;

      type Parameters_Index is new Character range 'A' .. 'Z';
      type Parameters_Array is array (Parameters_Index) of Parameter;

      Params : Parameters_Array := [others => (Kind => Non_Existant_Kind, Consumed => False)];
      I      : Positive := Line'First;

      procedure Parse_Number (Param : Parameters_Index) is
         In_Decimal_Part : Boolean := False;
         Is_Negative     : Boolean := False;
         First_Char      : constant Positive := I;
      begin
         loop
            exit when
              I = Line'Last + 1 or else (Line (I) /= '.' and Line (I) /= '-' and not Is_Decimal_Digit (Line (I)));

            if Params (Param).Kind = No_Value_Kind then
               Params (Param) := (Kind => Integer_Kind, Integer_Value => 0, Consumed => False);
            end if;

            if Params (Param).Kind = Integer_Kind and then (Params (Param).Integer_Value >= 100 or Line (I) = '.') then
               Params (Param) := (Kind => Float_Kind, Float_Value => 0.0, Consumed => False);
            end if;

            if Line (I) = '-' then
               if I /= First_Char then
                  raise Bad_Line with "'-' only allowed as first character in number.";
               end if;
               Is_Negative := True;
            elsif Line (I) = '.' then
               if In_Decimal_Part then
                  raise Bad_Line with "Multiple decimal points in number.";
               end if;
               In_Decimal_Part := True;
            else
               if Params (Param).Kind = Integer_Kind then
                  Params (Param).Integer_Value :=
                    @ * 10 + Parameter_Integer (Character'Pos (Line (I)) - Character'Pos ('0'));
               else
                  null;
               end if;
            end if;

            I := I + 1;
         end loop;

         if Params (Param).Kind = Float_Kind then
            Params (Param).Float_Value := Dimensionless'Value (Line (First_Char .. I - 1));
         end if;

         if Is_Negative then
            if Params (Param).Kind = No_Value_Kind then
               raise Bad_Line with "Got '-' with no following number.";
            elsif Params (Param).Kind = Integer_Kind then
               Params (Param) :=
                 (Kind => Float_Kind, Float_Value => -Dimensionless (Params (Param).Integer_Value), Consumed => False);
            end if;
         end if;
      end Parse_Number;

      procedure Parse_Argument (Param : Parameters_Index) is
      begin
         Params (Param) := (Kind => No_Value_Kind, Consumed => False);

         loop
            if I = Line'Last + 1 then
               return;
            end if;
            exit when Line (I) /= ' ';
            I := I + 1;
         end loop;

         if Line (I) = '"' then
            Params (Param) := (Kind => String_Kind, Consumed => False, Begin_Quote => I, End_Quote => <>);
            loop
               I := I + 1;
               if I = Line'Last + 1 then
                  raise Bad_Line with "Unterminated string.";
               end if;
               exit when Line (I) = '"';
            end loop;
            Params (Param).End_Quote := I;
            I := I + 1;
         else
            Parse_Number (Param);
         end if;
      end Parse_Argument;

      function Floatify_Or_Default (Param : Parameters_Index; Default : Dimensionless) return Dimensionless is
      begin
         if Params (Param).Consumed then
            raise Program_Error with "Parameter '" & Character (Param) & "' already consumed.";
         end if;
         Params (Param).Consumed := True;

         case Params (Param).Kind is
            when No_Value_Kind =>
               raise Bad_Line
                 with "Parameter '" & Character (Param) & "' has no value in command requiring number or omission.";

            when Non_Existant_Kind =>
               return Default;

            when Integer_Kind =>
               return Dimensionless (Params (Param).Integer_Value);

            when Float_Kind =>
               return Params (Param).Float_Value;

            when String_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' is string where number was expected.";
         end case;
      end Floatify_Or_Default;

      function Floatify_Or_Error (Param : Parameters_Index) return Dimensionless is
      begin
         if Params (Param).Consumed then
            raise Program_Error with "Parameter '" & Character (Param) & "' already consumed.";
         end if;
         Params (Param).Consumed := True;

         case Params (Param).Kind is
            when No_Value_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' has no value in command requiring number.";

            when Non_Existant_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' missing in command requiring number.";

            when Integer_Kind =>
               return Dimensionless (Params (Param).Integer_Value);

            when Float_Kind =>
               return Params (Param).Float_Value;

            when String_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' is string where number was expected.";
         end case;
      end Floatify_Or_Error;

      function Integer_Or_Default (Param : Parameters_Index; Default : Parameter_Integer) return Parameter_Integer is
      begin
         if Params (Param).Consumed then
            raise Program_Error with "Parameter '" & Character (Param) & "' already consumed.";
         end if;
         Params (Param).Consumed := True;

         case Params (Param).Kind is
            when No_Value_Kind =>
               raise Bad_Line
                 with "Parameter '" & Character (Param) & "' has no value in command requiring integer or omission.";

            when Non_Existant_Kind =>
               return Default;

            when Integer_Kind =>
               return Params (Param).Integer_Value;

            when Float_Kind | String_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' should be non-negative integer < 1000.";
         end case;
      end Integer_Or_Default;

      function Integer_Or_Error (Param : Parameters_Index) return Parameter_Integer is
      begin
         if Params (Param).Consumed then
            raise Program_Error with "Parameter '" & Character (Param) & "' already consumed.";
         end if;
         Params (Param).Consumed := True;

         case Params (Param).Kind is
            when No_Value_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' has no value in command requiring value.";

            when Non_Existant_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' missing in command requiring value.";

            when Integer_Kind =>
               return Params (Param).Integer_Value;

            when Float_Kind | String_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' should be non-negative integer < 1000.";
         end case;
      end Integer_Or_Error;

      function String_Or_Error (Param : Parameters_Index) return String is
      begin
         if Params (Param).Consumed then
            raise Program_Error with "Parameter '" & Character (Param) & "' already consumed.";
         end if;
         Params (Param).Consumed := True;

         case Params (Param).Kind is
            when No_Value_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' has no value in command requiring string.";

            when Non_Existant_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' missing in command requiring string.";

            when Integer_Kind | Float_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' should be a string surrounded by "".";

            when String_Kind =>
               return Line (Params (Param).Begin_Quote + 1 .. Params (Param).End_Quote - 1);
         end case;
      end String_Or_Error;

      function No_Value_Or_False_Or_Error (Param : Parameters_Index) return Boolean is
      begin
         if Params (Param).Consumed then
            raise Program_Error with "Parameter '" & Character (Param) & "' already consumed.";
         end if;
         Params (Param).Consumed := True;

         case Params (Param).Kind is
            when No_Value_Kind =>
               return True;

            when Non_Existant_Kind =>
               return False;

            when Integer_Kind | Float_Kind | String_Kind =>
               raise Bad_Line with "Parameter '" & Character (Param) & "' not allowed to have a value here.";
         end case;
      end No_Value_Or_False_Or_Error;

   begin
      if Line'Length /= 0 then
         loop
            exit when Line (I) = ';';

            if Line (I) = ' ' then
               I := I + 1;
            else
               declare
                  Char : constant Character := To_Upper (Line (I));
               begin
                  if Parameters_Index'Base (Char) not in Parameters_Index then
                     if Is_Control (Char) or Character'Pos (Char) > 127 then
                        raise Bad_Line with "Expected parameter letter, got unprintable character.";
                     else
                        raise Bad_Line with "Expected parameter letter, got '" & Line (I) & "'.";
                     end if;
                  elsif Params (Parameters_Index (Char)).Kind /= Non_Existant_Kind then
                     raise Bad_Line with "Parameter letter '" & Char & "' encountered more than once on line.";
                  else
                     I := I + 1;
                     Parse_Argument (Parameters_Index (Char));
                  end if;
               end;
            end if;

            exit when I = Line'Last + 1;
         end loop;
      end if;

      if Params ('G').Kind /= Non_Existant_Kind and Params ('M').Kind /= Non_Existant_Kind then
         raise Bad_Line with "Only one G or M parameter allowed per line.";
      end if;

      if Params ('G').Kind /= Non_Existant_Kind then
         Params ('G').Consumed := True;

         if Params ('G').Kind /= Integer_Kind then
            raise Bad_Line with "Bad G parameter format: " & Params ('G')'Image;
         end if;

         case Params ('G').Integer_Value is
            when 0 | 1 =>
               declare
                  Comm : Command;
               begin
                  Comm := (Kind => Move_Kind, Is_Rapid => Params ('G').Integer_Value = 0, others => <>);
                  if Ctx.XYZ_Relative_Mode then
                     Comm.Pos (X_Axis) := Ctx.Pos (X_Axis) + Floatify_Or_Default ('X', 0.0) * mm;
                     Comm.Pos (Y_Axis) := Ctx.Pos (Y_Axis) + Floatify_Or_Default ('Y', 0.0) * mm;
                     Comm.Pos (Z_Axis) := Ctx.Pos (Z_Axis) + Floatify_Or_Default ('Z', 0.0) * mm;
                  else
                     Comm.Pos (X_Axis) :=
                       Floatify_Or_Default
                         ('X',
                          (Ctx.Pos (X_Axis) - Ctx.Current_Retraction_Offset (X_Axis) - Ctx.G92_Offset (X_Axis)) / mm)
                       * mm
                       + Ctx.G92_Offset (X_Axis)
                       + Ctx.Current_Retraction_Offset (X_Axis);
                     Comm.Pos (Y_Axis) :=
                       Floatify_Or_Default
                         ('Y',
                          (Ctx.Pos (Y_Axis) - Ctx.Current_Retraction_Offset (Y_Axis) - Ctx.G92_Offset (Y_Axis)) / mm)
                       * mm
                       + Ctx.G92_Offset (Y_Axis)
                       + Ctx.Current_Retraction_Offset (Y_Axis);
                     Comm.Pos (Z_Axis) :=
                       Floatify_Or_Default
                         ('Z',
                          (Ctx.Pos (Z_Axis) - Ctx.Current_Retraction_Offset (Z_Axis) - Ctx.G92_Offset (Z_Axis)) / mm)
                       * mm
                       + Ctx.G92_Offset (Z_Axis)
                       + Ctx.Current_Retraction_Offset (Z_Axis);
                  end if;

                  if Ctx.E_Relative_Mode then
                     Comm.Pos (E_Axis) := Ctx.Pos (E_Axis) + Floatify_Or_Default ('E', 0.0) * mm;
                  else
                     Comm.Pos (E_Axis) :=
                       Floatify_Or_Default
                         ('E',
                          (Ctx.Pos (E_Axis) - Ctx.Current_Retraction_Offset (E_Axis) - Ctx.G92_Offset (E_Axis)) / mm)
                       * mm
                       + Ctx.G92_Offset (E_Axis)
                       + Ctx.Current_Retraction_Offset (E_Axis);
                  end if;

                  if Params ('G').Integer_Value = 0 and not Ctx.Replace_G0_With_G1 then
                     Comm.Feedrate := Floatify_Or_Default ('F', 299_792_458_000.1 * 60.0) * mm / min;
                  else
                     Comm.Feedrate := Floatify_Or_Default ('F', Ctx.Feedrate / (mm / min)) * mm / min;
                     Ctx.Feedrate := Comm.Feedrate;
                  end if;

                  Comm.Old_Pos := Ctx.Pos;

                  --  A move may just contain a new feedrate, in which case we do not want to execute it. If a move is
                  --  zero distance but XYZE parameters were specified then we still run the move as the user may be
                  --  relying on this to prevent blending of a corner.
                  if Params ('X').Kind /= Non_Existant_Kind
                    or Params ('Y').Kind /= Non_Existant_Kind
                    or Params ('Z').Kind /= Non_Existant_Kind
                    or Params ('E').Kind /= Non_Existant_Kind
                  then
                     Runner (Comm);
                     Ctx.Pos := Comm.Pos; --  Must occur here in case Runner raises an exception.

                  end if;
               end;

            when 4 =>
               if Params ('S').Kind /= Non_Existant_Kind then
                  Runner ((Kind => Dwell_Kind, Dwell_Time => Floatify_Or_Error ('S') * s, Pos => Ctx.Pos));
               else
                  Runner ((Kind => Dwell_Kind, Dwell_Time => Floatify_Or_Error ('P') * ms, Pos => Ctx.Pos));
               end if;

            when 10 =>
               if not Ctx.Is_Retracted then
                  declare
                     New_Pos : Position := Ctx.Pos;
                  begin
                     if Ctx.M207_Offset (E_Axis) /= Length (0.0) then
                        New_Pos (E_Axis) := New_Pos (E_Axis) - Ctx.M207_Offset (E_Axis);
                        Runner
                          ((Kind     => Move_Kind,
                            Pos      => New_Pos,
                            Old_Pos  => Ctx.Pos,
                            Feedrate => Ctx.M207_Feedrate,
                            Is_Rapid => False));
                        Ctx.Pos := New_Pos; --  Must occur here in case Runner raises an exception.
                        Ctx.Current_Retraction_Offset (E_Axis) :=
                          Ctx.Current_Retraction_Offset (E_Axis) - Ctx.M207_Offset (E_Axis);
                     end if;

                     Ctx.Is_Retracted := True;

                     if Ctx.M207_Offset (Z_Axis) /= Length (0.0) then
                        New_Pos (Z_Axis) := New_Pos (Z_Axis) + Ctx.M207_Offset (Z_Axis);
                        Runner
                          ((Kind     => Move_Kind,
                            Pos      => New_Pos,
                            Old_Pos  => Ctx.Pos,
                            Feedrate => Velocity'Last,
                            Is_Rapid => False));
                        Ctx.Pos := New_Pos; --  Must occur here in case Runner raises an exception.
                        Ctx.Current_Retraction_Offset (Z_Axis) := Ctx.M207_Offset (Z_Axis);
                     end if;
                  end;
               end if;

            when 11 =>
               if Ctx.Is_Retracted then
                  declare
                     New_Pos : Position := Ctx.Pos;
                  begin
                     if Ctx.Current_Retraction_Offset (Z_Axis) /= Length (0.0) then
                        New_Pos (Z_Axis) := New_Pos (Z_Axis) - Ctx.Current_Retraction_Offset (Z_Axis);
                        Runner
                          ((Kind     => Move_Kind,
                            Pos      => New_Pos,
                            Old_Pos  => Ctx.Pos,
                            Feedrate => Velocity'Last,
                            Is_Rapid => False));
                        Ctx.Pos := New_Pos; --  Must occur here in case Runner raises an exception.
                        Ctx.Current_Retraction_Offset (Z_Axis) := Length (0.0);
                     end if;

                     if Ctx.M207_Offset (E_Axis) /= Length (0.0) then
                        New_Pos (E_Axis) := New_Pos (E_Axis) + Ctx.M207_Offset (E_Axis) + Ctx.M208_Offset (E_Axis);
                        Runner
                          ((Kind     => Move_Kind,
                            Pos      => New_Pos,
                            Old_Pos  => Ctx.Pos,
                            Feedrate => Ctx.M207_Feedrate + Ctx.M208_Feedrate,
                            Is_Rapid => False));
                        Ctx.Pos := New_Pos; --  Must occur here in case Runner raises an exception.
                        Ctx.Current_Retraction_Offset (E_Axis) :=
                          Ctx.Current_Retraction_Offset (E_Axis) + Ctx.M207_Offset (E_Axis) + Ctx.M208_Offset (E_Axis);
                     end if;

                     Ctx.Is_Retracted := False;
                  end;
               end if;

            when 21 =>
               null;

            when 28 =>
               if Params ('E').Kind = Non_Existant_Kind
                 and Params ('X').Kind = Non_Existant_Kind
                 and Params ('Y').Kind = Non_Existant_Kind
                 and Params ('Z').Kind = Non_Existant_Kind
               then
                  Runner ((Kind => Home_Kind, Axes => (others => True), Pos_Before => Ctx.Pos, Pos => Ctx.Pos));
               else
                  Runner
                    ((Kind       => Home_Kind,
                      Axes       =>
                        (E_Axis => No_Value_Or_False_Or_Error ('E'),
                         X_Axis => No_Value_Or_False_Or_Error ('X'),
                         Y_Axis => No_Value_Or_False_Or_Error ('Y'),
                         Z_Axis => No_Value_Or_False_Or_Error ('Z')),
                      Pos_Before => Ctx.Pos,
                      Pos        => Ctx.Pos));
               end if;

            when 90 =>
               Ctx.XYZ_Relative_Mode := False;
               Ctx.E_Relative_Mode := False;

            when 91 =>
               Ctx.XYZ_Relative_Mode := True;
               Ctx.E_Relative_Mode := True;

            when 92 =>
               if Params ('E').Kind /= Non_Existant_Kind then
                  Ctx.G92_Offset (E_Axis) := Ctx.Pos (E_Axis) - Floatify_Or_Error ('E') * mm;
               end if;
               if Params ('X').Kind /= Non_Existant_Kind then
                  Ctx.G92_Offset (X_Axis) := Ctx.Pos (X_Axis) - Floatify_Or_Error ('X') * mm;
               end if;
               if Params ('Y').Kind /= Non_Existant_Kind then
                  Ctx.G92_Offset (Y_Axis) := Ctx.Pos (Y_Axis) - Floatify_Or_Error ('Y') * mm;
               end if;
               if Params ('Z').Kind /= Non_Existant_Kind then
                  Ctx.G92_Offset (Z_Axis) := Ctx.Pos (Z_Axis) - Floatify_Or_Error ('Z') * mm;
               end if;

            when others =>
               raise Bad_Line with "Unknown G code: " & Params ('G').Integer_Value'Image;
         end case;
      elsif Params ('M').Kind /= Non_Existant_Kind then
         Params ('M').Consumed := True;

         if Params ('M').Kind /= Integer_Kind then
            raise Bad_Line with "Bad M parameter format: " & Params ('M')'Image;
         end if;

         case Params ('M').Integer_Value is
            when 0 | 1 =>
               Runner ((Kind => Pause_Kind, Pos => Ctx.Pos));

            when 3 | 5 =>
               if not Has_Lasers then
                  raise Bad_Line with "This board does not support lasers. Laser commands can not be used.";
               end if;

               declare
                  Comm : Command :=
                    (Kind         => Set_Laser_Power_Kind,
                     Laser_Power  => 0.0,
                     Laser_To_Set => Ctx.Default_Laser.Name,
                     Pos          => Ctx.Pos);
               begin
                  if Params ('M').Integer_Value = 3 then
                     Comm.Laser_Power :=
                       Dimensionless'Min (1.0, Dimensionless'Max (0.0, Floatify_Or_Error ('S') / 255.0));
                  end if;

                  case Params ('P').Kind is
                     when No_Value_Kind =>
                        raise Bad_Line with "Parameter 'P' has no value in command requiring value.";

                     when Non_Existant_Kind =>
                        Comm.Laser_To_Set := Ctx.Default_Laser.Name;

                     when Integer_Kind =>
                        begin
                           Comm.Laser_To_Set := Laser_Name'Enum_Val (Integer_Or_Error ('P'));
                           if not Comm.Laser_To_Set'Valid then
                              raise Constraint_Error;
                           end if;
                        exception
                           when Constraint_Error =>
                              raise Bad_Line with "Invalid laser index (" & Params ('P').Integer_Value'Image & ").";
                        end;

                     when Float_Kind =>
                        raise Bad_Line
                          with "Parameter 'P' must be integer between 0 and 999 or string in this command.";

                     when String_Kind =>
                        declare
                           Name : constant String := String_Or_Error ('P');
                        begin
                           Comm.Laser_To_Set := Laser_Name'Value (Name);
                           if not Comm.Laser_To_Set'Valid then
                              raise Constraint_Error;
                           end if;
                        exception
                           when Constraint_Error =>
                              raise Bad_Line with "Invalid laser name (" & Name & ").";
                        end;
                  end case;

                  Runner (Comm);
               end;

            when 17 =>
               Runner
                 ((Kind => Enable_Steppers_Kind,
                   Axes =>
                     (E_Axis => No_Value_Or_False_Or_Error ('E'),
                      X_Axis => No_Value_Or_False_Or_Error ('X'),
                      Y_Axis => No_Value_Or_False_Or_Error ('Y'),
                      Z_Axis => No_Value_Or_False_Or_Error ('Z')),
                   Pos  => Ctx.Pos));

            when 18 | 84 =>
               Runner
                 ((Kind => Disable_Steppers_Kind,
                   Axes =>
                     (E_Axis => No_Value_Or_False_Or_Error ('E'),
                      X_Axis => No_Value_Or_False_Or_Error ('X'),
                      Y_Axis => No_Value_Or_False_Or_Error ('Y'),
                      Z_Axis => No_Value_Or_False_Or_Error ('Z')),
                   Pos  => Ctx.Pos));

            when 82 =>
               Ctx.E_Relative_Mode := False;

            when 83 =>
               Ctx.E_Relative_Mode := True;

            when 104 =>
               if not Has_Heaters then
                  raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
               end if;

               declare
                  Ignored : Parameter_Integer := Integer_Or_Default ('T', 0);
               begin
                  Runner
                    ((Kind               => Set_Hotend_Temperature_Kind,
                      Target_Temperature => Floatify_Or_Error ('S') * celsius,
                      Pos                => Ctx.Pos));
               end;

            when 106 | 107 =>
               if not Has_Fans then
                  raise Bad_Line with "This board does not support fans. Fan commands can not be used.";
               end if;

               declare
                  Comm : Command :=
                    (Kind => Set_Fan_Speed_Kind, Fan_Speed => 0.0, Fan_To_Set => Ctx.Default_Fan.Name, Pos => Ctx.Pos);
               begin
                  if Params ('M').Integer_Value = 106 then
                     Comm.Fan_Speed :=
                       Dimensionless'Min (1.0, Dimensionless'Max (0.0, Floatify_Or_Default ('S', 255.0) / 255.0));
                  end if;

                  case Params ('P').Kind is
                     when No_Value_Kind =>
                        raise Bad_Line with "Parameter 'P' has no value in command requiring value.";

                     when Non_Existant_Kind =>
                        Comm.Fan_To_Set := Ctx.Default_Fan.Name;

                     when Integer_Kind =>
                        begin
                           Comm.Fan_To_Set := Fan_Name'Enum_Val (Integer_Or_Error ('P'));
                           if not Comm.Fan_To_Set'Valid then
                              raise Constraint_Error;
                           end if;
                        exception
                           when Constraint_Error =>
                              raise Bad_Line with "Invalid fan index (" & Params ('P').Integer_Value'Image & ").";
                        end;

                     when Float_Kind =>
                        raise Bad_Line
                          with "Parameter 'P' must be integer between 0 and 999 or string in this command.";

                     when String_Kind =>
                        declare
                           Name : constant String := String_Or_Error ('P');
                        begin
                           Comm.Fan_To_Set := Fan_Name'Value (Name);
                           if not Comm.Fan_To_Set'Valid then
                              raise Constraint_Error;
                           end if;
                        exception
                           when Constraint_Error =>
                              raise Bad_Line with "Invalid fan name (" & Name & ").";
                        end;
                  end case;

                  Runner (Comm);
               end;

            when 109 =>
               if not Has_Heaters then
                  raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
               end if;

               Runner
                 ((Kind               => Wait_Hotend_Temperature_Kind,
                   Target_Temperature => Floatify_Or_Error ('S') * celsius,
                   Pos                => Ctx.Pos));

            when 122 =>
               Runner ((Kind => TMC_Dump_Kind, Pos => Ctx.Pos));

            when 140 =>
               if not Has_Heaters then
                  raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
               end if;

               Runner
                 ((Kind               => Set_Bed_Temperature_Kind,
                   Target_Temperature => Floatify_Or_Error ('S') * celsius,
                   Pos                => Ctx.Pos));

            when 141 =>
               if not Has_Heaters then
                  raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
               end if;

               Runner
                 ((Kind               => Set_Chamber_Temperature_Kind,
                   Target_Temperature => Floatify_Or_Error ('S') * celsius,
                   Pos                => Ctx.Pos));

            when 190 =>
               if not Has_Heaters then
                  raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
               end if;

               Runner
                 ((Kind               => Wait_Bed_Temperature_Kind,
                   Target_Temperature => Floatify_Or_Error ('S') * celsius,
                   Pos                => Ctx.Pos));

            when 191 =>
               if not Has_Heaters then
                  raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
               end if;

               Runner
                 ((Kind               => Wait_Chamber_Temperature_Kind,
                   Target_Temperature => Floatify_Or_Error ('S') * celsius,
                   Pos                => Ctx.Pos));

            when 205 =>
               declare
                  P_Present : constant Boolean := No_Value_Or_False_Or_Error ('P');
               begin
                  if not P_Present then
                     raise Bad_Line;
                  end if;
               exception
                  when Bad_Line =>
                     raise Bad_Line
                       with
                         "M205 requires P parameter with no value on Prunt to prevent conflicts with Marlin g-code.";
               end;

               if Params ('A').Kind /= Non_Existant_Kind then
                  Runner
                    ((Kind             => Set_Acceleration_Max_Kind,
                      Pos              => Ctx.Pos,
                      Acceleration_Max => Floatify_Or_Error ('A') * mm / s**2));
               elsif Params ('J').Kind /= Non_Existant_Kind then
                  Runner
                    ((Kind => Set_Jerk_Max_Kind, Pos => Ctx.Pos, Jerk_Max => Floatify_Or_Error ('J') * mm / s**3));
               elsif Params ('S').Kind /= Non_Existant_Kind then
                  Runner
                    ((Kind => Set_Snap_Max_Kind, Pos => Ctx.Pos, Snap_Max => Floatify_Or_Error ('S') * mm / s**4));
               elsif Params ('C').Kind /= Non_Existant_Kind then
                  Runner
                    ((Kind        => Set_Crackle_Max_Kind,
                      Pos         => Ctx.Pos,
                      Crackle_Max => Floatify_Or_Error ('C') * mm / s**5));
               elsif Params ('D').Kind /= Non_Existant_Kind then
                  Runner
                    ((Kind            => Set_Chord_Error_Max_Kind,
                      Pos             => Ctx.Pos,
                      Chord_Error_Max => Floatify_Or_Error ('D') * mm));
               elsif Params ('L').Kind /= Non_Existant_Kind then
                  Runner
                    ((Kind                  => Set_Pressure_Advance_Time_Kind,
                      Pos                   => Ctx.Pos,
                      Pressure_Advance_Time => Floatify_Or_Error ('L') * s));
               end if;

            when 207 =>
               Ctx.M207_Feedrate := Floatify_Or_Default ('F', Ctx.M207_Feedrate / (mm / min)) * (mm / min);
               Ctx.M207_Offset (E_Axis) := Floatify_Or_Default ('E', Ctx.M207_Offset (E_Axis) / mm) * mm;
               Ctx.M207_Offset (Z_Axis) := Floatify_Or_Default ('Z', Ctx.M207_Offset (Z_Axis) / mm) * mm;

            when 208 =>
               Ctx.M208_Feedrate := Floatify_Or_Default ('F', Ctx.M208_Feedrate / (mm / min)) * (mm / min);
               Ctx.M208_Offset (E_Axis) := Floatify_Or_Default ('S', Ctx.M208_Offset (E_Axis) / mm) * mm;

            when 303 =>
               if not Has_Heaters then
                  raise Bad_Line with "This board does not support heaters. Heater commands can not be used.";
               end if;

               declare
                  Cycles : constant Parameter_Integer := Integer_Or_Default ('C', 5);
                  Comm   : Command :=
                    (Kind               => Heater_Autotune_Kind,
                     Tuning_Temperature => Floatify_Or_Error ('S') * celsius,
                     Heater_To_Tune     => <>,
                     Max_Cycles         => <>,
                     Pos                => Ctx.Pos);
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

                  case Params ('T').Kind is
                     when No_Value_Kind =>
                        raise Bad_Line with "Parameter 'T' has no value in command requiring value.";

                     when Non_Existant_Kind =>
                        raise Bad_Line with "Parameter 'T' is required.";

                     when Integer_Kind =>
                        begin
                           Comm.Heater_To_Tune := Heater_Name'Enum_Val (Integer_Or_Error ('T'));
                           if not Comm.Heater_To_Tune'Valid then
                              raise Constraint_Error;
                           end if;
                        exception
                           when Constraint_Error =>
                              raise Bad_Line with "Invalid heater index (" & Params ('T').Integer_Value'Image & ").";
                        end;

                     when Float_Kind =>
                        raise Bad_Line
                          with "Parameter 'T' must be integer between 0 and 999 or string in this command.";

                     when String_Kind =>
                        declare
                           Name : constant String := String_Or_Error ('T');
                        begin
                           Comm.Heater_To_Tune := Heater_Name'Value (Name);
                           if not Comm.Heater_To_Tune'Valid then
                              raise Constraint_Error;
                           end if;
                        exception
                           when Constraint_Error =>
                              raise Bad_Line with "Invalid heater name (" & Name & ").";
                        end;
                  end case;
                  Runner (Comm);
               end;

            when 73 | 204 | 486 =>
               for I in Params'Range loop
                  Params (I).Consumed := True;
               end loop;

            when others =>
               raise Bad_Line with "Unknown M code: " & Params ('M').Integer_Value'Image;
         end case;
      end if;

      for I in Params'Range loop
         if not Params (I).Consumed and Params (I).Kind /= Non_Existant_Kind then
            raise Bad_Line with "Parameter '" & Character (I) & "' not valid for command on line.";
         end if;
      end loop;
   end Parse_Line;

   procedure Reset_Position (Ctx : in out Context; Pos : Position) is
   begin
      Ctx.Pos := Pos;
      Ctx.Current_Retraction_Offset := (others => Length (0.0));
      Ctx.Is_Retracted := False;
   end Reset_Position;

end Prunt.Gcode_Parser;
