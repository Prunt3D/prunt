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

package Dimension_Check.Rationals is

   type Rational is private;
   --  An exact rational number in lowest terms with a positive denominator. Operations raise Constraint_Error when a
   --  numerator or denominator would not fit in Long_Long_Integer.

   Zero : constant Rational;
   One  : constant Rational;

   function To_Rational (Numerator : Long_Long_Integer; Denominator : Long_Long_Integer := 1) return Rational;
   --  Return Numerator / Denominator in lowest terms. Raises Constraint_Error if Denominator is zero.

   function Numerator (Value : Rational) return Long_Long_Integer;
   --  Return the numerator of Value in lowest terms. Its sign is the sign of Value.

   function Denominator (Value : Rational) return Long_Long_Integer;
   --  Return the positive denominator of Value in lowest terms.

   function Is_Integer (Value : Rational) return Boolean;
   --  Return whether the denominator of Value is one.

   function "+" (Left, Right : Rational) return Rational;
   --  Return the exact sum of Left and Right.

   function "-" (Left, Right : Rational) return Rational;
   --  Return the exact difference of Left and Right.

   function "-" (Right : Rational) return Rational;
   --  Return the negation of Right.

   function "*" (Left, Right : Rational) return Rational;
   --  Return the exact product of Left and Right.

   function "/" (Left, Right : Rational) return Rational;
   --  Return the exact quotient of Left and Right. Raises Constraint_Error if Right is zero.

   function Integer_Divide (Left, Right : Rational) return Rational;
   --  Return Left / Right truncated toward zero as Ada integer division does. Both operands must be integers.

   function Image (Value : Rational) return String;
   --  Return Value as an integer such as "-3" or as a fraction such as "1/2".

   function Parse_Numeric_Literal (Text : String) return Rational;
   --  Return the exact value of the Ada decimal or based numeric literal Text, such as "1_000.5", "2.5E-3" or
   --  "16#F.8#E1". Raises Constraint_Error if Text is not such a literal or its value does not fit.

private

   type Rational is record
      Num : Long_Long_Integer := 0;
      Den : Long_Long_Integer := 1;
   end record;

   Zero : constant Rational := (Num => 0, Den => 1);
   One  : constant Rational := (Num => 1, Den => 1);

end Dimension_Check.Rationals;
