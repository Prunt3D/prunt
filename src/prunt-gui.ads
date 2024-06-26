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

private with Ada.Text_IO;
private with UXStrings;

package Prunt.GUI is

private

   use UXStrings;

   Nice_Axis_Names : constant array (Axis_Name) of String (1 .. 6) :=
     [X_Axis => "X Axis", Y_Axis => "Y Axis", Z_Axis => "Z Axis", E_Axis => "E Axis"];

   generic
      type Number is digits <>;
   function Float_Image
     (Value       : Number;
      Fore        : Ada.Text_IO.Field := 2;
      Aft         : Ada.Text_IO.Field := Number'Digits - 1;
      Exp         : Ada.Text_IO.Field := 3;
      Zero_Filled : Boolean           := False)
     return String;

   --  The default 'Image outputs scientific notion, which isn't very user friendly, so we use this instead.
   function DF_Image (Number : Dimensioned_Float) return UXString;

end Prunt.GUI;
