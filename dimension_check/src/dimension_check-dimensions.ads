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

with Dimension_Check.Rationals; use Dimension_Check.Rationals;

package Dimension_Check.Dimensions is

   Max_Dimensions : constant := 16;
   --  The largest number of base dimensions a dimension system may declare.

   subtype Dimension_Index is Positive range 1 .. Max_Dimensions;

   type Vector is array (Dimension_Index) of Rational;
   --  The exponent of each base dimension. Positions past the dimension count of the system are always zero.

   Dimensionless : constant Vector := [others => Zero];

   function "+" (Left, Right : Vector) return Vector;
   --  Return the dimensions of a product of values with dimensions Left and Right.

   function "-" (Left, Right : Vector) return Vector;
   --  Return the dimensions of a quotient of values with dimensions Left and Right.

   function "*" (Left : Vector; Right : Rational) return Vector;
   --  Return the dimensions of a value with dimensions Left raised to the power Right.

   function "/" (Left : Vector; Right : Rational) return Vector;
   --  Return the dimensions of the Right-th root of a value with dimensions Left.

   function Is_Dimensionless (Value : Vector) return Boolean;
   --  Return whether every exponent of Value is zero.

end Dimension_Check.Dimensions;
