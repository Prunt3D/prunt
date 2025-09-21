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

package body Prunt.Gcode_Arguments is

   function Parse_Arguments (Line : String) return Arguments is
      I : Positive := Line'First;

      Args : Arguments_Array := [others => (Kind => Non_Existant_Kind, Consumed => False)];

      procedure Parse_Number (Index : Arguments_Index) is
         In_Decimal_Part : Boolean := False;
         Is_Negative     : Boolean := False;
         First_Char      : constant Positive := I;
      begin
         loop
            exit when
              I = Line'Last + 1 or else (Line (I) /= '.' and Line (I) /= '-' and not Is_Decimal_Digit (Line (I)));

            if Args (Index).Kind = No_Value_Kind then
               Args (Index) := (Kind => Integer_Kind, Integer_Value => 0, Consumed => False);
            end if;

            if Args (Index).Kind = Integer_Kind and then (Args (Index).Integer_Value >= 100 or Line (I) = '.') then
               Args (Index) := (Kind => Float_Kind, Float_Value => 0.0, Consumed => False);
            end if;

            if Line (I) = '-' then
               if I /= First_Char then
                  raise Parse_Error with "'-' only allowed as first character in number.";
               end if;
               Is_Negative := True;
            elsif Line (I) = '.' then
               if In_Decimal_Part then
                  raise Parse_Error with "Multiple decimal points in number.";
               end if;
               In_Decimal_Part := True;
            else
               if Args (Index).Kind = Integer_Kind then
                  Args (Index).Integer_Value := @ * 10 + Argument_Integer'Value ("" & Line (I));
               else
                  null;
               end if;
            end if;

            I := I + 1;
         end loop;

         if Args (Index).Kind = Float_Kind then
            Args (Index).Float_Value := Dimensionless'Value (Line (First_Char .. I - 1));
         end if;

         if Is_Negative then
            if Args (Index).Kind = No_Value_Kind then
               raise Parse_Error with "Got '-' with no following number.";
            elsif Args (Index).Kind = Integer_Kind then
               Args (Index) :=
                 (Kind => Float_Kind, Float_Value => -Dimensionless (Args (Index).Integer_Value), Consumed => False);
            end if;
         end if;
      end Parse_Number;

      procedure Parse_Argument (Index : Arguments_Index) is
      begin
         Args (Index) := (Kind => No_Value_Kind, Consumed => False);

         loop
            if I = Line'Last + 1 then
               return;
            end if;
            exit when Line (I) /= ' ';
            I := I + 1;
         end loop;

         if Line (I) = '"' then
            Args (Index) :=
              (Kind => String_Kind, Consumed => False, Begin_Quote => I - Line'First + 1, End_Quote => <>);
            loop
               I := I + 1;
               if I = Line'Last + 1 then
                  raise Parse_Error with "Unterminated string.";
               end if;
               exit when Line (I) = '"';
            end loop;
            Args (Index).End_Quote := I - Line'First + 1;
            I := I + 1;
         else
            Parse_Number (Index);
         end if;
      end Parse_Argument;
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
                  if Arguments_Index'Base (Char) not in Arguments_Index then
                     if Is_Control (Char) or Character'Pos (Char) > 127 then
                        raise Parse_Error with "Expected parameter letter, got unprintable character.";
                     else
                        raise Parse_Error with "Expected parameter letter, got '" & Line (I) & "'.";
                     end if;
                  elsif Args (Arguments_Index (Char)).Kind /= Non_Existant_Kind then
                     raise Parse_Error with "Parameter letter '" & Char & "' encountered more than once on line.";
                  else
                     I := I + 1;
                     Parse_Argument (Arguments_Index (Char));
                  end if;
               end;
            end if;

            exit when I = Line'Last + 1;
         end loop;
      end if;

      return (Line_Length => I - Line'First, Line => Line (Line'First .. I - 1), Arguments => Args);
   end Parse_Arguments;

   function Kind (Args : Arguments; Index : Arguments_Index) return Argument_Kind is
   begin
      return Args.Arguments (Index).Kind;
   end Kind;

   function Consume_Float_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : Dimensionless) return Dimensionless is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter '" & Character (Index) & "' already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind =>
            raise Parse_Error
              with "Parameter '" & Character (Index) & "' has no value in command requiring number or omission.";

         when Non_Existant_Kind =>
            return Default;

         when Integer_Kind =>
            return Dimensionless (Args.Arguments (Index).Integer_Value);

         when Float_Kind =>
            return Args.Arguments (Index).Float_Value;

         when String_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' is string where number was expected.";
      end case;
   end Consume_Float_Or_Default;

   function Consume_Float (Args : in out Arguments; Index : Arguments_Index) return Dimensionless is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter '" & Character (Index) & "' already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' has no value in command requiring number.";

         when Non_Existant_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' missing in command requiring number.";

         when Integer_Kind =>
            return Dimensionless (Args.Arguments (Index).Integer_Value);

         when Float_Kind =>
            return Args.Arguments (Index).Float_Value;

         when String_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' is string where number was expected.";
      end case;
   end Consume_Float;

   function Consume_Integer_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : Argument_Integer) return Argument_Integer is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter '" & Character (Index) & "' already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind =>
            raise Parse_Error
              with "Parameter '" & Character (Index) & "' has no value in command requiring integer or omission.";

         when Non_Existant_Kind =>
            return Default;

         when Integer_Kind =>
            return Args.Arguments (Index).Integer_Value;

         when Float_Kind | String_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' should be non-negative integer < 1000.";
      end case;
   end Consume_Integer_Or_Default;

   function Consume_Integer (Args : in out Arguments; Index : Arguments_Index) return Argument_Integer is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter '" & Character (Index) & "' already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' has no value in command requiring value.";

         when Non_Existant_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' missing in command requiring value.";

         when Integer_Kind =>
            return Args.Arguments (Index).Integer_Value;

         when Float_Kind | String_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' should be non-negative integer < 1000.";
      end case;
   end Consume_Integer;

   function Consume_String_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : String) return String is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter '" & Character (Index) & "' already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' has no value in command requiring string.";

         when Non_Existant_Kind =>
            return Default;

         when Integer_Kind | Float_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' should be a string surrounded by "".";

         when String_Kind =>
            return Args.Line (Args.Arguments (Index).Begin_Quote + 1 .. Args.Arguments (Index).End_Quote - 1);
      end case;
   end Consume_String_Or_Default;

   function Consume_String (Args : in out Arguments; Index : Arguments_Index) return String is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter '" & Character (Index) & "' already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' has no value in command requiring string.";

         when Non_Existant_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' missing in command requiring string.";

         when Integer_Kind | Float_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' should be a string surrounded by "".";

         when String_Kind =>
            return Args.Line (Args.Arguments (Index).Begin_Quote + 1 .. Args.Arguments (Index).End_Quote - 1);
      end case;
   end Consume_String;

   function Consume_No_Value_Or_False (Args : in out Arguments; Index : Arguments_Index) return Boolean is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter '" & Character (Index) & "' already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind =>
            return True;

         when Non_Existant_Kind =>
            return False;

         when Integer_Kind | Float_Kind | String_Kind =>
            raise Parse_Error with "Parameter '" & Character (Index) & "' not allowed to have a value here.";
      end case;
   end Consume_No_Value_Or_False;

   procedure Validate_All_Consumed (Args : Arguments) is
   begin
      for I in Args.Arguments'Range loop
         if not Args.Arguments (I).Consumed and Args.Arguments (I).Kind /= Non_Existant_Kind then
            raise Parse_Error with "Parameter '" & Character (I) & "' not valid for command on line.";
         end if;
      end loop;
   end Validate_All_Consumed;

end Prunt.Gcode_Arguments;