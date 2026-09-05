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
with Ada.Strings.Fixed;

package body Prunt.Mockable is

   pragma Extensions_Allowed (On);

   use Ada.Strings.Unbounded;
   use Ada.Task_Identification;

   protected body Filesystem is
      procedure Checkpoint is
      begin
         if Failure_Enabled and then Failure_Task = Current_Task then
            if Remaining = 0 then
               Failure_Enabled := False;
               if Lose_Power then
                  Crash (To_String (Failure_Name));
               end if;
               raise Ada.Text_IO.Use_Error with "Injected configuration IO interruption.";
            end if;
            Remaining := @ - 1;
         end if;
      end Checkpoint;

      procedure Fail_After (File_Name : String; Steps : Natural; Power_Loss : Boolean) is
      begin
         Failure_Name := To_Unbounded_String (File_Name);
         Failure_Task := Current_Task;
         Remaining := Steps;
         Lose_Power := Power_Loss;
         Failure_Enabled := True;
      end Fail_After;

      procedure Disable_Failure is
      begin
         if Failure_Task = Current_Task then
            Failure_Enabled := False;
         end if;
      end Disable_Failure;

      procedure Crash (File_Name : String) is
         function Belongs (Name : String) return Boolean;

         function Belongs (Name : String) return Boolean is
           (Name = File_Name or else Name = File_Name & ".tmp"
            or else Ada.Strings.Fixed.Index (Name, File_Name & "_backup_") = 1);

         Cursor : File_Maps.Cursor := Store.First;
      begin
         while File_Maps.Has_Element (Cursor) loop
            declare
               Name : constant String := File_Maps.Key (Cursor);
               Previous : File_Maps.Cursor := Cursor;
            begin
               File_Maps.Next (Cursor);
               if Belongs (Name) then
                  Store.Delete (Previous);
                  Synced.Exclude (Name);
               end if;
            end;
         end loop;
         for C in Durable.Iterate loop
            if Belongs (File_Maps.Key (C)) then
               Store.Include (File_Maps.Key (C), File_Maps.Element (C));
               Synced.Include (File_Maps.Key (C), File_Maps.Element (C));
            end if;
         end loop;
      end Crash;

      procedure Copy (Source, Target : String) is
      begin
         Checkpoint;
         Store.Include (Target, Store.Element (Source));
         Checkpoint;
      end Copy;

      procedure Replace (Source, Target : String) is
         Content : constant File_Content_Vectors.Vector := Store (Source);
      begin
         Checkpoint;
         Store.Include (Target, Content);
         Synced.Exclude (Target);
         if Synced.Contains (Source) then
            Synced.Include (Target, Synced.Element (Source));
            Synced.Delete (Source);
         end if;
         Store.Delete (Source);
         Checkpoint;
      end Replace;

      procedure Sync (Name : String) is
      begin
         Checkpoint;
         Synced.Include (Name, Store (Name));
         Checkpoint;
      end Sync;

      procedure Sync_Parent is
      begin
         Checkpoint;
         Durable.Clear;
         for C in Store.Iterate loop
            declare
               Name : constant String := File_Maps.Key (C);
            begin
               if Synced.Contains (Name) then
                  Durable.Include (Name, Synced (Name));
               else
                  Durable.Include (Name, File_Content_Vectors.Empty_Vector);
               end if;
            end;
         end loop;
         Checkpoint;
      end Sync_Parent;

      procedure Create (Name : String) is
      begin
         Checkpoint;
         if Store.Contains (Name) then
            Store.Delete (Name);
         end if;
         Store.Insert (Name, File_Content_Vectors.Empty_Vector);
         Synced.Exclude (Name);
         Checkpoint;
      end Create;

      procedure Write_Line (Name : String; Line : String) is
      begin
         Checkpoint;
         Store (Name).Append (Line);
         Checkpoint;
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
         Checkpoint;
         if Store.Contains (Name) then
            Store.Delete (Name);
         else
            raise Ada.Directories.Name_Error with "file """ & Name & """ does not exist";
         end if;
         Checkpoint;
      end Delete;

      procedure Rename (Old_Name, New_Name : String) is
         Content : File_Content_Vectors.Vector;
      begin
         Checkpoint;
         if not Store.Contains (Old_Name) then
            raise Ada.Directories.Name_Error with "old file """ & Old_Name & """ does not exist";
         elsif Store.Contains (New_Name) then
            raise Ada.Directories.Use_Error with "new name """ & New_Name & """ designates a file that already exists";
         end if;

         Content := Store (Old_Name);
         Store.Delete (Old_Name);
         Store.Insert (New_Name, Content);
         Synced.Exclude (New_Name);
         if Synced.Contains (Old_Name) then
            Synced.Include (New_Name, Synced.Element (Old_Name));
            Synced.Delete (Old_Name);
         end if;
         Checkpoint;
      end Rename;

   end Filesystem;

end Prunt.Mockable;
