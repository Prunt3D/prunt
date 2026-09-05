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

with Ada.Finalization;
with Ada.Strings.Unbounded;
with System;

package Prunt.Mockable.Persistence is
   type Writer_Lease is new Ada.Finalization.Limited_Controlled with private;
   procedure Acquire (Lease : in out Writer_Lease; File_Name : String);
   function Name (Lease : Writer_Lease) return String;
   overriding procedure Finalize (Lease : in out Writer_Lease);

   procedure Copy (Source, Target : String);
   procedure Sync (File_Name : String);
   procedure Replace (Source, Target : String);
   procedure Sync_Parent (File_Name : String);
   --  Replace is atomic. Sync_Parent makes the directory entry durable.
private
   type Writer_Lease is new Ada.Finalization.Limited_Controlled with record
      File_Name : Ada.Strings.Unbounded.Unbounded_String;
      Handle    : System.Address := System.Null_Address;
      Held      : Boolean := False;
   end record;
end Prunt.Mockable.Persistence;
