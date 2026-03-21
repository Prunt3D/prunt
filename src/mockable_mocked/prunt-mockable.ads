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

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;

package Prunt.Mockable is
private

   package File_Content_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   use type File_Content_Vectors.Vector;
   package File_Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, File_Content_Vectors.Vector);

   protected Filesystem is
      procedure Create (Name : String);
      procedure Write_Line (Name : String; Line : String);
      function Read_Line (Name : String; Index : Positive) return String;
      function Line_Count (Name : String) return Natural;
      function Exists (Name : String) return Boolean;
      procedure Delete (Name : String);
      procedure Rename (Old_Name, New_Name : String);
   private
      Store : File_Maps.Map;
   end Filesystem;

end Prunt.Mockable;
