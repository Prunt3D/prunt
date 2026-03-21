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
