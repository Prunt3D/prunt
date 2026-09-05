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
with Ada.IO_Exceptions;
with GNAT.OS_Lib;
with Interfaces.C;

package body Prunt.Mockable.Persistence is
   use Ada.Strings.Unbounded;
   use Interfaces.C;
   use type System.Address;
   use type GNAT.OS_Lib.File_Descriptor;

   function Fopen (Path, Mode : char_array) return System.Address
   with Import, Convention => C, External_Name => "fopen";
   function Fileno (Stream : System.Address) return int
   with Import, Convention => C, External_Name => "fileno";
   function Fclose (Stream : System.Address) return int
   with Import, Convention => C, External_Name => "fclose";
   function Flock (Descriptor, Operation : int) return int
   with Import, Convention => C, External_Name => "flock";
   function Fcntl (Descriptor, Command, Value : int) return int
   with Import, Convention => C, External_Name => "fcntl";
   function Fsync (Descriptor : int) return int
   with Import, Convention => C, External_Name => "fsync";
   function Rename (Source, Target : char_array) return int
   with Import, Convention => C, External_Name => "rename";

   function Multiple_Links (Path : char_array) return int
   with Import, Convention => C, External_Name => "prunt_config_multiple_links";

   procedure Acquire (Lease : in out Writer_Lease; File_Name : String) is
   begin
      if Lease.Held then
         raise Program_Error with "Writer lease already held.";
      end if;
      Lease.File_Name := To_Unbounded_String (GNAT.OS_Lib.Normalize_Pathname (File_Name, Resolve_Links => True));
      if Name (Lease) = "" or else Multiple_Links (To_C (Name (Lease))) /= 0 then
         raise Ada.IO_Exceptions.Use_Error with "Configuration must be accessible and must not have hard links.";
      end if;
      Lease.Handle := Fopen (To_C (Name (Lease) & ".lock"), To_C ("a"));
      if Lease.Handle = System.Null_Address then
         raise Ada.IO_Exceptions.Use_Error with "Cannot open configuration writer lock.";
      end if;
      --  F_SETFD / FD_CLOEXEC: unrelated executed programs must not retain this lease.
      if Fcntl (Fileno (Lease.Handle), 2, 1) /= 0 then
         Finalize (Lease);
         raise Ada.IO_Exceptions.Use_Error with "Cannot mark configuration lock close-on-exec.";
      end if;
      --  POSIX LOCK_EX | LOCK_NB. Never unlink the sidecar: waiters must use the same inode.
      if Flock (Fileno (Lease.Handle), 6) /= 0 then
         Finalize (Lease);
         raise Ada.IO_Exceptions.Use_Error with "Configuration already has a writer: " & File_Name;
      end if;
      Lease.Held := True;
   end Acquire;

   function Name (Lease : Writer_Lease) return String
   is (To_String (Lease.File_Name));

   overriding
   procedure Finalize (Lease : in out Writer_Lease) is
      Ignored : int
      with Unreferenced;
   begin
      if Lease.Handle /= System.Null_Address then
         Ignored := Fclose (Lease.Handle);
         Lease.Handle := System.Null_Address;
      end if;
      Lease.Held := False;
   end Finalize;

   procedure Copy (Source, Target : String) is
   begin
      Ada.Directories.Copy_File (Source, Target, "overwrite=true");
   end Copy;

   procedure Sync (File_Name : String) is
      Descriptor : constant GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Open_Read (File_Name, GNAT.OS_Lib.Binary);
      Result     : int;
   begin
      if Descriptor = GNAT.OS_Lib.Invalid_FD then
         raise Ada.IO_Exceptions.Use_Error with "Cannot open for sync: " & File_Name;
      end if;
      Result := Fsync (int (Descriptor));
      GNAT.OS_Lib.Close (Descriptor);
      if Result /= 0 then
         raise Ada.IO_Exceptions.Use_Error with "Cannot sync: " & File_Name;
      end if;
   end Sync;

   procedure Replace (Source, Target : String) is
   begin
      if Multiple_Links (To_C (Target)) /= 0 then
         raise Ada.IO_Exceptions.Use_Error with "Configuration must be accessible and must not have hard links.";
      end if;
      if Rename (To_C (Source), To_C (Target)) /= 0 then
         raise Ada.IO_Exceptions.Use_Error with "Cannot replace configuration: " & Target;
      end if;
   end Replace;

   procedure Sync_Parent (File_Name : String) is
   begin
      Sync (Ada.Directories.Containing_Directory (File_Name));
   end Sync_Parent;
end Prunt.Mockable.Persistence;
