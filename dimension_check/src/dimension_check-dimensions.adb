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

package body Dimension_Check.Dimensions is

   function "+" (Left, Right : Vector) return Vector
   is ([for I in Dimension_Index => Left (I) + Right (I)]);

   function "-" (Left, Right : Vector) return Vector
   is ([for I in Dimension_Index => Left (I) - Right (I)]);

   function "*" (Left : Vector; Right : Rational) return Vector
   is ([for I in Dimension_Index => Left (I) * Right]);

   function "/" (Left : Vector; Right : Rational) return Vector
   is ([for I in Dimension_Index => Left (I) / Right]);

   function Is_Dimensionless (Value : Vector) return Boolean
   is (Value = Dimensionless);

end Dimension_Check.Dimensions;
