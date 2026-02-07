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

package body Prunt.Dummy_Allocator is

   use type System.Address;

   overriding
   procedure Allocate
     (Pool                     : in out Dummy_Pool_Type;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count)
   is
      pragma Unreferenced (Pool, Alignment);
   begin
      Storage_Address := Next_Allocation_Address;

      if Storage_Address = System.Null_Address then
         raise Program_Error with "Next_Allocation_Address must be set before allocation.";
      end if;

      Next_Allocation_Address := System.Null_Address;
   end Allocate;

   overriding
   procedure Deallocate
     (Pool                     : in out Dummy_Pool_Type;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count) is
   begin
      null;
   end Deallocate;

   overriding
   function Storage_Size (Pool : Dummy_Pool_Type) return Storage_Count is
   begin
      return Storage_Count'Last;
   end Storage_Size;

end Prunt.Dummy_Allocator;
