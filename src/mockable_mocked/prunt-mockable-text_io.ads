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

private with Ada.Strings.Unbounded;
private with GNATCOLL.Refcount;

package Prunt.Mockable.Text_IO is

   type File_Type is limited private;
   subtype File_Mode is Ada.Text_IO.File_Mode;

   In_File  : constant File_Mode := Ada.Text_IO.In_File;
   Out_File : constant File_Mode := Ada.Text_IO.Out_File;

   procedure Create (File : in out File_Type; Mode : File_Mode := Out_File; Name : String := ""; Form : String := "");

   procedure Open (File : in out File_Type; Mode : File_Mode; Name : String; Form : String := "");

   procedure Close (File : in out File_Type);

   procedure Put_Line (File : File_Type; Item : String);

   function Get_Line (File : File_Type) return String;

   function End_Of_File (File : File_Type) return Boolean;

   function Is_Open (File : File_Type) return Boolean;

private

   use Ada.Strings.Unbounded;

   package Positive_Shared_Pointers is new GNATCOLL.Refcount.Shared_Pointers (Positive);

   type File_Type is limited record
      Name     : Unbounded_String := Null_Unbounded_String;
      Mode     : File_Mode := In_File;
      Is_Open  : Boolean := False;
      Read_Ptr : Positive_Shared_Pointers.Ref := Positive_Shared_Pointers.Null_Ref;
   end record;

end Prunt.Mockable.Text_IO;
