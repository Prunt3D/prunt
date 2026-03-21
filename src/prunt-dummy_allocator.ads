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

with System.Storage_Elements; use System.Storage_Elements;
with System.Storage_Pools;    use System.Storage_Pools;

package Prunt.Dummy_Allocator is

   Next_Allocation_Address : System.Address
   with Thread_Local_Storage;

   type Dummy_Pool_Type is new Root_Storage_Pool with null record;

   overriding
   procedure Allocate
     (Pool                     : in out Dummy_Pool_Type;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count);
   --  Sets `Storage_Address` to `Next_Allocation_Address`. This should not be used unless you fully control the type
   --  and can guarantee that initialization will not also use this procedure. This procedure sets
   --  `Next_Allocation_Address` to null before returning to try and catch such an error.
   --
   --  If this procedure needs to be made to work more generally then every user would need to save and restore
   --  `Next_Allocation_Address`. This could be achieved by making use of a controlled type rather than setting the
   --  allocation address directly.

   overriding
   procedure Deallocate
     (Pool                     : in out Dummy_Pool_Type;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count);

   overriding
   function Storage_Size (Pool : Dummy_Pool_Type) return Storage_Count;

end Prunt.Dummy_Allocator;
