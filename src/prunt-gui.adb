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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Prunt.GUI is

   function Float_Image
     (Value       : Number;
      Fore        : Ada.Text_IO.Field := 2;
      Aft         : Ada.Text_IO.Field := Number'Digits - 1;
      Exp         : Ada.Text_IO.Field := 3;
      Zero_Filled : Boolean           := False)
     return String
   is
      --  Based on code from PragmARC.
      --  Copyright (C) 2023 by PragmAda Software Engineering.  All rights reserved.

      package Number_IO is new Float_IO (Number);

      --  Apply Width, Negative, & Zero_Filled to Image
      function Adjust (Image : String; Width : Field; Negative : Boolean; Zero_Filled : Boolean) return String is
         Blank : constant Character := ' ';
         Zero  : constant Character := '0';
         Minus : constant Character := '-';
      begin -- Adjust
         if Zero_Filled then
            if Negative then
               return Minus & [1 .. Width - Image'Length - 1 => Zero] & Image;
            else
               return [1 .. Width - Image'Length => Zero] & Image;
            end if;
         else
            if Negative then
               return [1 .. Width - Image'Length - 1 => Blank] & Minus & Image;
            else
               return [1 .. Width - Image'Length => Blank] & Image;
            end if;
         end if;
      end Adjust;

      Image : String (1 .. 3 * 255 + 3);
      Start : Natural;
      Width : Field := Fore + Aft + 1;
   begin
      Number_IO.Put (To => Image, Item => abs Value, Aft => Aft, Exp => Exp);
      Start := Index_Non_Blank (Image);

      if Exp > 0 then
         Width := Width + Exp + 1;
      end if;

      return Adjust (Image (Start .. Image'Last), Width, Value < 0.0, Zero_Filled);
   end Float_Image;

end Prunt.GUI;
