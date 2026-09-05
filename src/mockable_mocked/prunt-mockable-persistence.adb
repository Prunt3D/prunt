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

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.IO_Exceptions;
with GNAT.OS_Lib;

package body Prunt.Mockable.Persistence is
   use Ada.Strings.Unbounded;
   package Name_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);
   protected Writers is
      procedure Acquire (Name : String);
      procedure Release (Name : String);
   private
      Names : Name_Sets.Set;
   end Writers;

   protected body Writers is
      procedure Acquire (Name : String) is
      begin
         if Names.Contains (Name) then
            raise Ada.IO_Exceptions.Use_Error with "Configuration already has a writer: " & Name;
         end if;
         Names.Insert (Name);
      end Acquire;

      procedure Release (Name : String) is
      begin
         Names.Delete (Name);
      end Release;
   end Writers;

   procedure Acquire (Lease : in out Writer_Lease; File_Name : String) is
   begin
      if Lease.Held then
         raise Program_Error with "Writer lease already held.";
      end if;
      Lease.File_Name := To_Unbounded_String (File_Name);
      Writers.Acquire (GNAT.OS_Lib.Normalize_Pathname (File_Name));
      Lease.Held := True;
   end Acquire;

   function Name (Lease : Writer_Lease) return String is (To_String (Lease.File_Name));

   overriding procedure Finalize (Lease : in out Writer_Lease) is
   begin
      if Lease.Held then
         Writers.Release (GNAT.OS_Lib.Normalize_Pathname (Name (Lease)));
         Lease.Held := False;
      end if;
   end Finalize;

   procedure Copy (Source, Target : String) is
   begin
      Filesystem.Copy (Source, Target);
   end Copy;

   procedure Sync (File_Name : String) is
   begin
      Filesystem.Sync (File_Name);
   end Sync;

   procedure Replace (Source, Target : String) is
   begin
      Filesystem.Replace (Source, Target);
   end Replace;

   procedure Sync_Parent (File_Name : String) is
      pragma Unreferenced (File_Name);
   begin
      Filesystem.Sync_Parent;
   end Sync_Parent;
end Prunt.Mockable.Persistence;
