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

with Ada.Directories;
with Ada.Text_IO;

package body Prunt.Mockable is

   pragma Extensions_Allowed (On);

   protected body Filesystem is
      procedure Create (Name : String) is
      begin
         if Store.Contains (Name) then
            Store.Delete (Name);
         end if;
         Store.Insert (Name, File_Content_Vectors.Empty_Vector);
      end Create;

      procedure Write_Line (Name : String; Line : String) is
      begin
         Store (Name).Append (Line);
      end Write_Line;

      function Read_Line (Name : String; Index : Positive) return String is
      begin
         if not Store.Contains (Name) then
            raise Constraint_Error with "File not found: " & Name;
         elsif Index > Store (Name).Last_Index then
            raise Ada.Text_IO.End_Error;
         end if;

         return Store (Name) (Index);
      end Read_Line;

      function Line_Count (Name : String) return Natural is
      begin
         return Natural (Store (Name).Length);
      end Line_Count;

      function Exists (Name : String) return Boolean is
      begin
         return Store.Contains (Name);
      end Exists;

      procedure Delete (Name : String) is
      begin
         if Store.Contains (Name) then
            Store.Delete (Name);
         else
            raise Ada.Directories.Name_Error with "file """ & Name & """ does not exist";
         end if;
      end Delete;

      procedure Rename (Old_Name, New_Name : String) is
         Content : File_Content_Vectors.Vector;
      begin
         if not Store.Contains (Old_Name) then
            raise Ada.Directories.Name_Error with "old file """ & Old_Name & """ does not exist";
         elsif Store.Contains (New_Name) then
            raise Ada.Directories.Use_Error with "new name """ & New_Name & """ designates a file that already exists";
         end if;

         Content := Store (Old_Name);
         Store.Delete (Old_Name);
         Store.Insert (New_Name, Content);
      end Rename;

   end Filesystem;

end Prunt.Mockable;
