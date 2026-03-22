--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
--  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
--  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice (including the next paragraph) shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
--  THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.
--------------------------------------------------

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
