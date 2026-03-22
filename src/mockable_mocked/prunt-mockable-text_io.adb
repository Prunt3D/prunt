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
