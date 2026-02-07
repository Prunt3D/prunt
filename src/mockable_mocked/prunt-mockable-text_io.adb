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

package body Prunt.Mockable.Text_IO is

   pragma Extensions_Allowed (On);

   use type Ada.Text_IO.File_Mode;

   procedure Create (File : in out File_Type; Mode : File_Mode := Out_File; Name : String := ""; Form : String := "")
   is
      pragma Unreferenced (Form);
   begin
      if Name = "" then
         raise Constraint_Error with "Temporary files not supported in mock yet.";
      end if;

      Filesystem.Create (Name);

      File.Name := To_Unbounded_String (Name);
      File.Mode := Mode;
      File.Is_Open := True;
      File.Read_Ptr.Set (1);
   end Create;

   procedure Open (File : in out File_Type; Mode : File_Mode; Name : String; Form : String := "") is
      pragma Unreferenced (Form);
   begin
      if not Filesystem.Exists (Name) then
         raise Ada.Text_IO.Name_Error with "File not found: " & Name;
      end if;

      File.Name := To_Unbounded_String (Name);
      File.Mode := Mode;
      File.Is_Open := True;
      File.Read_Ptr.Set (1);
   end Open;

   procedure Close (File : in out File_Type) is
   begin
      if not File.Is_Open then
         raise Constraint_Error with "File not open.";
      end if;

      File.Is_Open := False;
   end Close;

   procedure Put_Line (File : File_Type; Item : String) is
   begin
      if not File.Is_Open then
         raise Constraint_Error with "File not open.";
      elsif File.Mode /= Out_File then
         raise Ada.Text_IO.Mode_Error;
      end if;

      Filesystem.Write_Line (To_String (File.Name), Item);
   end Put_Line;

   function Get_Line (File : File_Type) return String is
   begin
      if not File.Is_Open then
         raise Constraint_Error with "File not open.";
      elsif File.Mode /= In_File then
         raise Ada.Text_IO.Mode_Error;
      end if;

      return Result : constant String := Filesystem.Read_Line (To_String (File.Name), File.Read_Ptr.Get) do
         File.Read_Ptr.Get := File.Read_Ptr.Get + 1;
      end return;
   end Get_Line;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      if not File.Is_Open then
         raise Constraint_Error with "File not open.";
      end if;

      return File.Read_Ptr.Get > Filesystem.Line_Count (To_String (File.Name));
   end End_Of_File;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return File.Is_Open;
   end Is_Open;

end Prunt.Mockable.Text_IO;
