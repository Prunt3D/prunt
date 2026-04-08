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
   Storage_Size : System.Storage_Elements.Storage_Count;
package Prunt.Bounded_Indefinite_Queues is

   --  pragma Preelaborate (Bounded_Indefinite_Queues);
   --  TODO: Uncomment above when we switch to GCC 16 where `Ada.Unchecked_Deallocate_Subpool` is preelaborated.

   type Queue is tagged private;

   Out_Of_Space_Error : exception;

   procedure Enqueue (This : in out Queue; New_Item : Element_Type);
   --  Raises `Out_Of_Space_Error` if there is no space for the element in the backing storage but there might be in
   --  the future. Raises `Program_Error` if there will never be space for the element in the backing storage, this may
   --  occur after a previous call raises `Out_Of_Space_Error`.

   procedure Dequeue (This : in out Queue; Item : out Element_Type)
   with Pre => not This.Is_Empty;
   --  Sets `Item` to the head element and removes it.

   procedure Dequeue (This : in out Queue)
   with Pre => not This.Is_Empty;
   --  Removes the head element without returning it.

   function Peek (This : Queue) return Element_Type
   with Pre => not This.Is_Empty;
   --  Returns the head element without removing it.

   procedure Clear (This : in out Queue);

   function Is_Empty (This : Queue) return Boolean;

private

   use System.Storage_Elements;
   use System.Storage_Pools.Subpools;

   function Round_Up_Size (Size : Storage_Count; Alignment : Storage_Count) return Storage_Count
   is (Size + ((Alignment - (Size mod Alignment)) mod Alignment));

   package Subpool_Support is
      --  TODO: Do we really need subpools here of can we just use `Prunt.Dummy_Allocator` for everything?

      function Aligned_Address (Addr : System.Address; Alignment : Storage_Count) return System.Address;
      --  Return `Addr`, rounded up to multiple of `Alignment`.

      Local_Dummy_Pool : Prunt.Dummy_Allocator.Dummy_Pool_Type;

      type Queue_Elements_Root_Pool_Type is limited new Root_Storage_Pool_With_Subpools with null record;

      type Queue_Elements_Subpool is limited new Root_Subpool with record
         Start_Address : System.Address := System.Null_Address;
         --  The first address that may be returned as part of an allocation.
         --
         --  This is the element at the start of the backing storage, not the first address after the allocated region
         --  ends.

         End_Address : System.Address := System.Null_Address;
         --  The final address which may be returned as part of an allocation. If an allocation returns this address
         --  then it must only be for a single storage element.
         --
         --  This is the element at the end of the backing storage, not the last address before the allocated region
         --  starts.

         Current_Free : System.Address := System.Null_Address;
         --  The first address which has not been returned as part of an allocation. Can be past `End_Address` in the
         --  case of an allocation which exactly fills the buffer, in which case wrapping should be attempted.

         Head_Address : System.Address := System.Null_Address;
         --  The address of the head node. We can not allocate at or past this address as it is still in use.

         Last_Allocation_Address : System.Address := System.Null_Address;
         --  The address of the last allocation made from this subpool.
      end record;

      overriding
      function Create_Subpool (Pool : in out Queue_Elements_Root_Pool_Type) return not null Subpool_Handle;
      --  We never use this one. It will raise Program_Error.

      overriding
      procedure Allocate_From_Subpool
        (Pool                     : in out Queue_Elements_Root_Pool_Type;
         Storage_Address          : out System.Address;
         Size_In_Storage_Elements : Storage_Count;
         Alignment                : Storage_Count;
         Subpool                  : not null Subpool_Handle)
      with Pre => Subpool.all in Queue_Elements_Subpool;

      overriding
      procedure Deallocate_Subpool (Pool : in out Queue_Elements_Root_Pool_Type; Subpool : in out Subpool_Handle);

      Queue_Elements_Root_Pool : Queue_Elements_Root_Pool_Type := (Root_Storage_Pool_With_Subpools with null record);
      --  The one and only object of this type ever created.

      Queue_Elements_Subpool_Storage_Size : constant Storage_Count :=
        Round_Up_Size (Queue_Elements_Subpool'Max_Size_In_Storage_Elements, Standard'Maximum_Alignment);
   end Subpool_Support;

   use Subpool_Support;

   type Element_Access is access Element_Type
   with Storage_Pool => Queue_Elements_Root_Pool, Size => Standard'Address_Size;

   type Node;
   type Node_Access is access Node with Storage_Pool => Queue_Elements_Root_Pool, Size => Standard'Address_Size;

   type Node is record
      Next : Node_Access;
      --  Next node in the linked list, null if this is the last node (most recently enqueued).

      Element : Element_Access;
      --  Pointer to copy of data supplied to `Enqueue`.

      Allocation_Address : System.Address;
      --  The address returned by `Allocate_From_Subpool` when allocating this node. This is not necessarily equal to
      --  the address stored in `Next` as the compiler is allowed to add padding or a header.
   end record;

   Rounded_Storage_Size : constant Storage_Count :=
     Round_Up_Size (Storage_Size, Standard'Maximum_Alignment)
     + Round_Up_Size (Node'Max_Size_In_Storage_Elements, Standard'Maximum_Alignment);

   type Pooled_Subpool_Handle is access Queue_Elements_Subpool with Storage_Pool => Subpool_Support.Local_Dummy_Pool;

   pragma No_Strict_Aliasing (Pooled_Subpool_Handle);
   pragma No_Strict_Aliasing (Element_Access);
   pragma No_Strict_Aliasing (Node_Access);
   --  Needed because we are unchecked-converting from Address to Element_Access (see package body), which is a
   --  violation of the normal aliasing rules enforced by gcc.
   --
   --  This is copied from GNAT's Bounded_Indefinite_Holder implementation. It does not appear that it is actually
   --  required, but there should be no harm in keeping it.

   type Aligned_Storage_Array is array (Storage_Offset range <>) of aliased Storage_Element
   with Component_Size => System.Storage_Unit, Alignment => Standard'Maximum_Alignment;

   type Queue is new Ada.Finalization.Controlled with record
      Head : Node_Access := null;
      --  The least recently enqueued node (next to be dequeued).

      Tail : Node_Access := null;
      --  The most recently enqueued node.

      Subpool : Pooled_Subpool_Handle := null;
      Storage : aliased Storage_Array (1 .. Rounded_Storage_Size + Queue_Elements_Subpool_Storage_Size);
      --  We place the subpool after the element storage to guarantee that `Subpool.Current_Free` will never overflow.
   end record
   with Preelaborable_Initialization => Element_Type'Preelaborable_Initialization;

   procedure Maybe_Initialize (This : in out Queue);

   overriding
   procedure Adjust (This : in out Queue);
   overriding
   procedure Finalize (This : in out Queue);

end Prunt.Bounded_Indefinite_Queues;
