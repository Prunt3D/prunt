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

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;
private with Ada.Task_Identification;

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
      procedure Copy (Source, Target : String);
      procedure Replace (Source, Target : String);
      procedure Sync (Name : String);
      procedure Sync_Parent;
      procedure Fail_After (File_Name : String; Steps : Natural; Power_Loss : Boolean);
      procedure Disable_Failure;
      procedure Crash (File_Name : String);
   private
      procedure Checkpoint;
      Store : File_Maps.Map;
      Durable : File_Maps.Map;
      Synced : File_Maps.Map;
      Remaining : Natural := 0;
      Failure_Enabled : Boolean := False;
      Lose_Power : Boolean := False;
      Failure_Name : Ada.Strings.Unbounded.Unbounded_String;
      Failure_Task : Ada.Task_Identification.Task_Id := Ada.Task_Identification.Null_Task_Id;
   end Filesystem;

end Prunt.Mockable;
