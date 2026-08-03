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
   --  Create and open a mocked text file with the requested mode and name. Form is accepted for compatibility and
   --  ignored.

   procedure Open (File : in out File_Type; Mode : File_Mode; Name : String; Form : String := "");
   --  Open an existing mocked text file with the requested mode. Form is accepted for compatibility and ignored.

   procedure Close (File : in out File_Type);
   --  Close File.

   procedure Put_Line (File : File_Type; Item : String);
   --  Append Item and a line terminator to File.

   function Get_Line (File : File_Type) return String;
   --  Read the next line from File and return it without a line terminator.

   function End_Of_File (File : File_Type) return Boolean;
   --  Return True when File has no unread characters.

   function Is_Open (File : File_Type) return Boolean;
   --  Return True when File is currently open.

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
