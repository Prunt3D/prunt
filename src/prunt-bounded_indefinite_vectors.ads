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

with System.Storage_Elements;

private with Ada.Finalization;
private with System;
private with System.Storage_Pools.Subpools;
private with Prunt.Dummy_Allocator;

generic
   type Element_Type (<>) is private;
   type Index_Type is range <>;
   Storage_Size : System.Storage_Elements.Storage_Count;
package Prunt.Bounded_Indefinite_Vectors is

   --  pragma Preelaborate (Bounded_Indefinite_Vectors);
   --  TODO: Uncomment above when we switch to GCC 16 where `Ada.Unchecked_Deallocate_Subpool` is preelaborated.

   subtype Extended_Index is Index_Type'Base range Index_Type'First - 1 .. Index_Type'Last;

   type Vector is tagged private;

   Out_Of_Space_Error : exception;

   procedure Append (This : in out Vector; New_Item : Element_Type)
   with Pre => This.Last_Index < Index_Type'Last;
   --  Raises `Out_Of_Space_Error` if there is no space for the element in the backing storage but there might be in
   --  the future. Raises `Program_Error` if there will never be space for the element in the backing storage, this may
   --  occur after a previous call raises `Out_Of_Space_Error`.

   procedure Clear (This : in out Vector);

   procedure Process_Range
     (This   : Vector;
      Start  : Index_Type;
      Finish : Extended_Index;
      Action : not null access procedure (Item : in out Element_Type))
   with Pre => Start > Finish or else Finish <= This.Last_Index;

   function Last_Index (This : Vector) return Extended_Index;

   function Element (This : Vector; Index : Index_Type) return Element_Type
   with Pre => Index <= This.Last_Index;

   function Is_Empty (This : Vector) return Boolean;

private

   use System.Storage_Elements;
   use System.Storage_Pools.Subpools;

   function Round_Up_Size (Size : Storage_Count; Alignment : Storage_Count) return Storage_Count
   is (Size + ((Alignment - (Size mod Alignment)) mod Alignment));

   Rounded_Storage_Size : constant Storage_Count := Round_Up_Size (Storage_Size, Standard'Maximum_Alignment);

   package Subpool_Support is
      --  TODO: Do we really need subpools here of can we just use `Prunt.Dummy_Allocator` for everything?

      function Aligned_Address (Addr : System.Address; Alignment : Storage_Count) return System.Address;
      --  Return `Addr`, rounded up to multiple of `Alignment`.

      Local_Dummy_Pool : Prunt.Dummy_Allocator.Dummy_Pool_Type;

      type Vector_Elements_Root_Pool_Type is limited new Root_Storage_Pool_With_Subpools with null record;

      type Vector_Elements_Subpool is limited new Root_Subpool with record
         --  These subpools hold the actual vector elements.

         Current_Free : System.Address := System.Null_Address;
         --  The first address which has not been returned as part of an allocation.

         End_Address : System.Address := System.Null_Address;
         --  The final address which may be returned as part of an allocation. If an allocation returns this address
         --  then it must only be for a single storage element.
         --
         --  As a special case, when `Storage_Size` is zero, this will not be a valid address to return.
      end record;

      overriding
      function Create_Subpool (Pool : in out Vector_Elements_Root_Pool_Type) return not null Subpool_Handle;
      --  We never use this one. It will raise Program_Error.

      overriding
      procedure Allocate_From_Subpool
        (Pool                     : in out Vector_Elements_Root_Pool_Type;
         Storage_Address          : out System.Address;
         Size_In_Storage_Elements : Storage_Count;
         Alignment                : Storage_Count;
         Subpool                  : not null Subpool_Handle)
      with Pre => Subpool.all in Vector_Elements_Subpool;

      overriding
      procedure Deallocate_Subpool (Pool : in out Vector_Elements_Root_Pool_Type; Subpool : in out Subpool_Handle);

      Vector_Elements_Root_Pool : Vector_Elements_Root_Pool_Type := (Root_Storage_Pool_With_Subpools with null record);
      --  The one and only object of this type ever created.

      Vector_Elements_Subpool_Storage_Size : constant Storage_Count :=
        Round_Up_Size (Vector_Elements_Subpool'Max_Size_In_Storage_Elements, Standard'Maximum_Alignment);
   end Subpool_Support;

   use Subpool_Support;

   type Element_Access is access Element_Type
   with Storage_Pool => Vector_Elements_Root_Pool, Size => Standard'Address_Size;
   --  Size specification needed to ensure contiguous bounds if Element_Type turns out to be an unconstrained array
   --  subtype. We do not want a fat-pointer representation in that case.
   --
   --  TODO: This is copied from GNAT's Bounded_Indefinite_Holder implementation. Why don't we want fat pointers here?

   type Pooled_Subpool_Handle is access Vector_Elements_Subpool with Storage_Pool => Subpool_Support.Local_Dummy_Pool;

   pragma No_Strict_Aliasing (Pooled_Subpool_Handle);
   pragma No_Strict_Aliasing (Element_Access);
   --  Needed because we are unchecked-converting from Address to Element_Access (see package body), which is a
   --  violation of the normal aliasing rules enforced by gcc.
   --
   --  This is copied from GNAT's Bounded_Indefinite_Holder implementation. It does not appear that it is actually
   --  required, but there should be no harm in keeping it.

   type Element_Array is array (Extended_Index) of Element_Access;
   --  We use `Extended_Index` here to help catch any user errors in the `Element` function without requiring an extra
   --  check.

   type Aligned_Storage_Array is array (Storage_Offset range <>) of aliased Storage_Element
   with Component_Size => System.Storage_Unit, Alignment => Standard'Maximum_Alignment;
   --  We use maximum alignment here to simplify copying between vectors by avoiding cases where the alignment of the
   --  new object causes items to not fit when they previously did.

   type Vector is new Ada.Finalization.Controlled with record
      Last_Used_Index : Extended_Index := Extended_Index'First;
      --  The last index within `Elements` which is populated, or `Extended_Index'First` if no elements are used.

      Subpool : Pooled_Subpool_Handle := null;
      --  Pointer to a subpool allocated within `Subpool_Record_Storage`. This allows the subpool to be a limited type.

      Elements : Element_Array := [others => null];
      --  Pointers to elements within `Storage`, allocated when the user calls `Append`.

      Storage : aliased Aligned_Storage_Array (1 .. Rounded_Storage_Size + Vector_Elements_Subpool_Storage_Size);
      --  We place the subpool after the element storage to guarantee that `Subpool.Current_Free` will never overflow.
   end record
   with Preelaborable_Initialization => Element_Type'Preelaborable_Initialization;

   procedure Maybe_Initialize (This : in out Vector);
   --  Populate `This.Subpool` if it is null, otherwise do nothing.

   --  No `Initialize` procedure as `Vector` might have `Preelaborable_Initialization => True`. Instead we just run
   --  initialization when `Subpool = null` inside a procedure which makes use of it.
   overriding
   procedure Adjust (This : in out Vector);
   overriding
   procedure Finalize (This : in out Vector);

end Prunt.Bounded_Indefinite_Vectors;
