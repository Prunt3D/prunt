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

with Ada.Unchecked_Deallocate_Subpool;
with Ada.Unchecked_Deallocation;

package body Prunt.Bounded_Indefinite_Queues is

   pragma Extensions_Allowed (On);

   procedure Free is new Ada.Unchecked_Deallocation (Queue_Elements_Subpool, Pooled_Subpool_Handle);
   procedure Free is new Ada.Unchecked_Deallocation (Element_Type, Element_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   use type System.Address;

   procedure Enqueue (This : in out Queue; New_Item : Element_Type) is
      New_Node         : Node_Access;
      New_Element      : Element_Access;
      Old_Current_Free : System.Address;
   begin
      pragma Abort_Defer;
      --  Exception handlers are an abort completion point.

      This.Maybe_Initialize;

      Old_Current_Free := This.Subpool.Current_Free;

      New_Node := new (This.Subpool.all'Access) Node;
      New_Node.Allocation_Address := This.Subpool.Last_Allocation_Address;

      begin
         New_Element := new (This.Subpool.all'Access) Element_Type'(New_Item);
      exception
         when Out_Of_Space_Error =>
         Free (New_Node);
         This.Subpool.Current_Free := Old_Current_Free;
         --  TODO: Could the initializer for New_Item do something here with the queue which would cause this to
         --  be the wrong value to restore? Do we even care about that possibility?

         if This.Is_Empty then
            --  The item may be smaller than Rounded_Storage_Size but not fit when there's already a node in the
            --  pool.
            raise Program_Error with "Item too large for storage pool";
         else
            raise;
         end if;
      end;

      New_Node.Element := New_Element;
      New_Node.Next := null;
      --  Allocation_Address already set above.

      if This.Tail /= null then
         This.Tail.Next := New_Node;
      else
         This.Head := New_Node;
         This.Subpool.Head_Address := New_Node.Allocation_Address;
      end if;
      This.Tail := New_Node;
   end Enqueue;

   procedure Dequeue (This : in out Queue; Item : out Element_Type) is
      Old_Head : Node_Access := This.Head;
   begin
      Old_Head := This.Head;
      Item := Old_Head.Element.all;

      This.Head := Old_Head.Next;
      if This.Head = null then
         This.Tail := null;
         This.Subpool.Current_Free := This.Subpool.Start_Address;
         This.Subpool.Head_Address := System.Null_Address;
      else
         This.Subpool.Head_Address := This.Head.Allocation_Address;
      end if;

      Free (Old_Head.Element);
      Free (Old_Head);
      pragma Assert (Old_Head = null); --  Silences warning.
   end Dequeue;

   procedure Dequeue (This : in out Queue) is
      Old_Head : Node_Access := This.Head;
   begin
      This.Head := Old_Head.Next;
      if This.Head = null then
         This.Tail := null;
         This.Subpool.Current_Free := This.Subpool.Start_Address;
         This.Subpool.Head_Address := System.Null_Address;
      else
         This.Subpool.Head_Address := This.Head.Allocation_Address;
      end if;

      Free (Old_Head.Element);
      Free (Old_Head);
      pragma Assert (Old_Head = null); --  Silences warning.
   end Dequeue;

   function Peek (This : Queue) return Element_Type is
   begin
      return This.Head.Element.all;
   end Peek;

   procedure Clear (This : in out Queue) is
   begin
      while This.Head /= null loop
         This.Dequeue;
      end loop;
   end Clear;

   function Is_Empty (This : Queue) return Boolean is
   begin
      return This.Head = null;
   end Is_Empty;

   procedure Maybe_Initialize (This : in out Queue) is
   begin
      if This.Subpool = null then
         Prunt.Dummy_Allocator.Next_Allocation_Address := This.Storage (Rounded_Storage_Size + 1)'Address;
         This.Subpool :=
           new Queue_Elements_Subpool'
             (Root_Subpool
              with
                Start_Address           => This.Storage'Address,
                End_Address             => This.Storage (Rounded_Storage_Size)'Address,
                Current_Free            => This.Storage'Address,
                Head_Address            => System.Null_Address,
                Last_Allocation_Address => System.Null_Address);
         Set_Pool_Of_Subpool (This.Subpool.all'Unchecked_Access, Queue_Elements_Root_Pool);
         Prunt.Dummy_Allocator.Next_Allocation_Address := System.Null_Address;
      end if;
   end Maybe_Initialize;

   overriding
   procedure Adjust (This : in out Queue) is
      Old_Head          : Node_Access := This.Head;
      Old_Subpool       : constant Pooled_Subpool_Handle := This.Subpool;
      First_Node_Offset : Storage_Count := 0;
   begin
      if Old_Head /= null then
         First_Node_Offset := Old_Head.Allocation_Address - Old_Subpool.Start_Address;
      end if;

      This.Subpool := null;
      This.Head := null;
      This.Tail := null;

      This.Maybe_Initialize;

      if Old_Head /= null then
         --  We must offset the start of the allocated region to match the old queue as elements may be packed in such
         --  a way that they will only fit with an offset. For example, with storage capacity 40, item A (size 8)
         --  at 32, and item B (size 32, align 16) wrapped at 0. If we copied starting at 0, A goes to 0, padding
         --  pushes B to 16, requiring an end at 46 bytes and therefore wrapping, but there's nowhere to wrap to with A
         --  at 0.
         This.Subpool.Current_Free := This.Subpool.Start_Address + First_Node_Offset;
      end if;

      while Old_Head /= null loop
         This.Enqueue (Old_Head.Element.all);
         Old_Head := Old_Head.Next;
      end loop;
   end Adjust;

   overriding
   procedure Finalize (This : in out Queue) is
   begin
      if This.Subpool /= null then
         declare
            Handle : Subpool_Handle := This.Subpool.all'Unchecked_Access;
         begin
            Clear (This);

            Ada.Unchecked_Deallocate_Subpool (Handle);
            --  Free should be all we need here, but GCC does not currently finalize subpools correctly, leading to
            --  memory corruption as the freed subpool will still belong to the pool. Refer to
            --  https://gcc.gnu.org/bugzilla/show_bug.cgi?id=124107

            Free (This.Subpool);
         end;
      end if;
   end Finalize;

   package body Subpool_Support is

      function Aligned_Address (Addr : System.Address; Alignment : Storage_Count) return System.Address is
         Initial_Align : constant Storage_Count := Addr mod Alignment;
      begin
         if Initial_Align = 0 then
            return Addr;
         else
            return Addr + (Alignment - Initial_Align);
         end if;
      end Aligned_Address;

      overriding
      function Create_Subpool (Pool : in out Queue_Elements_Root_Pool_Type) return not null Subpool_Handle is
      begin
         pragma Annotate (Xcov, Exempt_On, "Should never be called.");
         return raise Program_Error with "Should never be called.";
         pragma Annotate (Xcov, Exempt_Off);
      end Create_Subpool;

      overriding
      procedure Allocate_From_Subpool
        (Pool                     : in out Queue_Elements_Root_Pool_Type;
         Storage_Address          : out System.Address;
         Size_In_Storage_Elements : Storage_Count;
         Alignment                : Storage_Count;
         Subpool                  : not null Subpool_Handle)
      is
         Q_Subpool    : constant access Queue_Elements_Subpool := Queue_Elements_Subpool (Subpool.all)'Access;
         Aligned      : constant System.Address := Aligned_Address (Q_Subpool.Current_Free, Alignment);
         Rounded_Size : constant Storage_Count := Round_Up_Size (Size_In_Storage_Elements, Alignment);

         Start_Aligned : constant System.Address := Aligned_Address (Q_Subpool.Start_Address, Alignment);

         Read_Barrier : constant System.Address :=
           (if Q_Subpool.Head_Address = System.Null_Address
            then Q_Subpool.End_Address + 1
            else Q_Subpool.Head_Address);
         --  End_Address + 1 is safe as we store more data past End_Address.
      begin
         if Rounded_Size > Rounded_Storage_Size then
            raise Program_Error with "Item too large for storage pool";
         end if;

         if Aligned >= Read_Barrier then
            --  We are allocating after a single contiguous used region.
            if Q_Subpool.Current_Free <= Read_Barrier then
               --  We have caught up to Read_Barrier from behind, possibly after skipping over part of an existing
               --  allocation, therefore this address is not safe to use.
               raise Out_Of_Space_Error with "Storage exhausted";
            elsif Rounded_Size <= Q_Subpool.End_Address - Aligned + 1 then
               --  We are allocating in the free area after the used region and the allocation fits there.
               Q_Subpool.Current_Free := Aligned + Rounded_Size;
               Storage_Address := Aligned;
               Q_Subpool.Last_Allocation_Address := Storage_Address;
            elsif Rounded_Size <= Read_Barrier - Start_Aligned then
               --  The allocation does not fit after the used region but does fit before it.
               Q_Subpool.Current_Free := Start_Aligned + Rounded_Size;
               Storage_Address := Start_Aligned;
               Q_Subpool.Last_Allocation_Address := Storage_Address;
            else
               --  The allocation does not fit before or after the used region.
               raise Out_Of_Space_Error with "Storage exhausted";
            end if;
         elsif Rounded_Size <= Read_Barrier - Aligned then
            --  We are allocation before the used region or between two used regions and the allocation fits.
            Q_Subpool.Current_Free := Aligned + Rounded_Size;
            Storage_Address := Aligned;
            Q_Subpool.Last_Allocation_Address := Storage_Address;
            return;
         else
            --  We are allocation before the used region or between two used regions and the allocation does not fit.
            raise Out_Of_Space_Error with "Storage exhausted";
         end if;
      end Allocate_From_Subpool;

      overriding
      procedure Deallocate_Subpool (Pool : in out Queue_Elements_Root_Pool_Type; Subpool : in out Subpool_Handle) is
      begin
         null;
      end Deallocate_Subpool;

   end Subpool_Support;

end Prunt.Bounded_Indefinite_Queues;
