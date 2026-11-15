--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  dimension_check implements the dimensionality checking of GNAT and is derived from it, in particular from
--  gcc/ada/sem_dim.adb, Copyright (C) 2011-2025, Free Software Foundation, Inc.
--
--  This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
--  License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any
--  later version.
--
--  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
--  details.
--
--  You should have received a copy of the GNU General Public License along with this program, in the file COPYING. If
--  not, see <https://www.gnu.org/licenses/>.
--------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

package body Dimension_Check.Rationals is

   function GCD (Left, Right : Long_Long_Integer) return Long_Long_Integer is
      A : Long_Long_Integer := abs Left;
      B : Long_Long_Integer := abs Right;
      T : Long_Long_Integer;
   begin
      while B /= 0 loop
         T := A mod B;
         A := B;
         B := T;
      end loop;
      return A;
   end GCD;

   function To_Rational (Numerator : Long_Long_Integer; Denominator : Long_Long_Integer := 1) return Rational is
      Divisor : Long_Long_Integer;
      Num     : Long_Long_Integer := Numerator;
      Den     : Long_Long_Integer := Denominator;
   begin
      if Den = 0 then
         raise Constraint_Error with "rational with zero denominator";
      end if;

      if Den < 0 then
         Num := -Num;
         Den := -Den;
      end if;

      Divisor := GCD (Num, Den);
      if Divisor > 1 then
         Num := Num / Divisor;
         Den := Den / Divisor;
      end if;

      return (Num => Num, Den => Den);
   end To_Rational;

   function Numerator (Value : Rational) return Long_Long_Integer
   is (Value.Num);

   function Denominator (Value : Rational) return Long_Long_Integer
   is (Value.Den);

   function Is_Integer (Value : Rational) return Boolean
   is (Value.Den = 1);

   function "+" (Left, Right : Rational) return Rational is
      Divisor : constant Long_Long_Integer := GCD (Left.Den, Right.Den);
   begin
      return
        To_Rational
          (Left.Num * (Right.Den / Divisor) + Right.Num * (Left.Den / Divisor), (Left.Den / Divisor) * Right.Den);
   end "+";

   function "-" (Right : Rational) return Rational
   is ((Num => -Right.Num, Den => Right.Den));

   function "-" (Left, Right : Rational) return Rational
   is (Left + (-Right));

   function "*" (Left, Right : Rational) return Rational is
      Divisor_1 : constant Long_Long_Integer := Long_Long_Integer'Max (1, GCD (Left.Num, Right.Den));
      Divisor_2 : constant Long_Long_Integer := Long_Long_Integer'Max (1, GCD (Right.Num, Left.Den));
   begin
      return
        To_Rational
          ((Left.Num / Divisor_1) * (Right.Num / Divisor_2), (Left.Den / Divisor_2) * (Right.Den / Divisor_1));
   end "*";

   function "/" (Left, Right : Rational) return Rational is
   begin
      if Right.Num = 0 then
         raise Constraint_Error with "rational division by zero";
      end if;

      return Left * To_Rational (Right.Den, Right.Num);
   end "/";

   function Integer_Divide (Left, Right : Rational) return Rational is
   begin
      if not Is_Integer (Left) or else not Is_Integer (Right) then
         raise Constraint_Error with "integer division of a non-integer";
      end if;

      return To_Rational (Left.Num / Right.Num);
   end Integer_Divide;

   function Image (Value : Rational) return String is
      Num_Image : constant String := Trim (Value.Num'Image, Both);
   begin
      if Value.Den = 1 then
         return Num_Image;
      else
         return Num_Image & "/" & Trim (Value.Den'Image, Both);
      end if;
   end Image;

   function Parse_Numeric_Literal (Text : String) return Rational is
      Clean : String (1 .. Text'Length);
      Last  : Natural := 0;

      Base     : Long_Long_Integer := 10;
      Mantissa : Rational := Zero;
      Scale    : Rational := One;
      Exponent : Long_Long_Integer := 0;
      Negative : Boolean := False;
      Position : Positive;
      Mark     : Character := ' ';
      Seen_Dot : Boolean := False;

      function Digit_Value (C : Character) return Long_Long_Integer is
      begin
         if C in '0' .. '9' then
            return Character'Pos (C) - Character'Pos ('0');
         elsif To_Upper (C) in 'A' .. 'F' then
            return Character'Pos (To_Upper (C)) - Character'Pos ('A') + 10;
         else
            raise Constraint_Error with "invalid digit in numeric literal";
         end if;
      end Digit_Value;
   begin
      for C of Text loop
         if C /= '_' then
            Last := Last + 1;
            Clean (Last) := C;
         end if;
      end loop;

      Position := 1;

      --  A based literal starts with a decimal base followed by '#' or ':'.
      for I in 1 .. Last loop
         if Clean (I) in '#' | ':' then
            Base := Long_Long_Integer'Value (Clean (1 .. I - 1));
            Mark := Clean (I);
            Position := I + 1;
            exit;
         end if;
      end loop;

      if Base not in 2 .. 16 then
         raise Constraint_Error with "invalid base in numeric literal";
      end if;

      while Position <= Last loop
         declare
            C : constant Character := Clean (Position);
         begin
            if C = '.' then
               Seen_Dot := True;
            elsif C = Mark then
               Position := Position + 1;
               exit;
            elsif Mark = ' ' and then C in 'e' | 'E' then
               exit;
            else
               Mantissa := Mantissa * To_Rational (Base) + To_Rational (Digit_Value (C));
               if Seen_Dot then
                  Scale := Scale * To_Rational (Base);
               end if;
            end if;
         end;
         Position := Position + 1;
      end loop;

      if Position <= Last then
         if Clean (Position) not in 'e' | 'E' then
            raise Constraint_Error with "invalid numeric literal";
         end if;
         Position := Position + 1;
         if Position <= Last and then Clean (Position) in '+' | '-' then
            Negative := Clean (Position) = '-';
            Position := Position + 1;
         end if;
         Exponent := Long_Long_Integer'Value (Clean (Position .. Last));
      end if;

      Mantissa := Mantissa / Scale;
      for I in 1 .. Exponent loop
         if Negative then
            Mantissa := Mantissa / To_Rational (Base);
         else
            Mantissa := Mantissa * To_Rational (Base);
         end if;
      end loop;

      return Mantissa;
   end Parse_Numeric_Literal;

end Dimension_Check.Rationals;
