-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
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

pragma Extensions_Allowed (On);

with Ada.Text_IO;

package Prunt.Mockable.Text_IO is

   subtype File_Type is Ada.Text_IO.File_Type;
   subtype File_Mode is Ada.Text_IO.File_Mode;

   In_File  : constant File_Mode := Ada.Text_IO.In_File;
   Out_File : constant File_Mode := Ada.Text_IO.Out_File;

   procedure Create (File : in out File_Type; Mode : File_Mode := Out_File; Name : String := ""; Form : String := "")
   renames Ada.Text_IO.Create;

   procedure Open (File : in out File_Type; Mode : File_Mode; Name : String; Form : String := "")
   renames Ada.Text_IO.Open;

   procedure Close (File : in out File_Type) renames Ada.Text_IO.Close;

   procedure Put_Line (File : File_Type; Item : String) renames Ada.Text_IO.Put_Line;

   function End_Of_File (File : File_Type) return Boolean renames Ada.Text_IO.End_Of_File;

   function Is_Open (File : File_Type) return Boolean renames Ada.Text_IO.Is_Open;

end Prunt.Mockable.Text_IO;
