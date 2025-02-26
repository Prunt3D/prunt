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

package Prunt.TMC_Types is

   type TMC_Boolean is new Boolean with Size => 1;
   for TMC_Boolean use (False => 0, True => 1);

   type Unsigned_1 is range 0 .. 2**1 - 1 with Size => 1;
   type Unsigned_2 is range 0 .. 2**2 - 1 with Size => 2;
   type Unsigned_3 is range 0 .. 2**3 - 1 with Size => 3;
   type Unsigned_4 is range 0 .. 2**4 - 1 with Size => 4;
   type Unsigned_5 is range 0 .. 2**5 - 1 with Size => 5;
   type Unsigned_6 is range 0 .. 2**6 - 1 with Size => 6;
   type Unsigned_7 is range 0 .. 2**7 - 1 with Size => 7;
   type Unsigned_8 is range 0 .. 2**8 - 1 with Size => 8;
   type Unsigned_9 is range 0 .. 2**9 - 1 with Size => 9;
   type Unsigned_10 is range 0 .. 2**10 - 1 with Size => 10;
   type Unsigned_11 is range 0 .. 2**11 - 1 with Size => 11;
   type Unsigned_12 is range 0 .. 2**12 - 1 with Size => 12;
   type Unsigned_13 is range 0 .. 2**13 - 1 with Size => 13;
   type Unsigned_14 is range 0 .. 2**14 - 1 with Size => 14;
   type Unsigned_15 is range 0 .. 2**15 - 1 with Size => 15;
   type Unsigned_16 is range 0 .. 2**16 - 1 with Size => 16;
   type Unsigned_17 is range 0 .. 2**17 - 1 with Size => 17;
   type Unsigned_18 is range 0 .. 2**18 - 1 with Size => 18;
   type Unsigned_19 is range 0 .. 2**19 - 1 with Size => 19;
   type Unsigned_20 is range 0 .. 2**20 - 1 with Size => 20;
   type Unsigned_21 is range 0 .. 2**21 - 1 with Size => 21;
   type Unsigned_22 is range 0 .. 2**22 - 1 with Size => 22;
   type Unsigned_23 is range 0 .. 2**23 - 1 with Size => 23;
   type Unsigned_24 is range 0 .. 2**24 - 1 with Size => 24;
   type Unsigned_25 is range 0 .. 2**25 - 1 with Size => 25;
   type Unsigned_26 is range 0 .. 2**26 - 1 with Size => 26;
   type Unsigned_27 is range 0 .. 2**27 - 1 with Size => 27;
   type Unsigned_28 is range 0 .. 2**28 - 1 with Size => 28;
   type Unsigned_29 is range 0 .. 2**29 - 1 with Size => 29;
   type Unsigned_30 is range 0 .. 2**30 - 1 with Size => 30;
   type Unsigned_31 is range 0 .. 2**31 - 1 with Size => 31;
   type Unsigned_32 is range 0 .. 2**32 - 1 with Size => 32;

end Prunt.TMC_Types;
