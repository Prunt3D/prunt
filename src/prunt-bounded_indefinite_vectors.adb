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

package body Prunt.Bounded_Indefinite_Vectors is

   pragma Extensions_Allowed (On);

   procedure Append (This : in out Vector; New_Item : Element_Type) is
   begin
      This.Maybe_Initialize;

      pragma Annotate (Xcov, Exempt_On, "Handled by precondition.");
      if This.Last_Used_Index = Index_Type'Last then
         raise Constraint_Error with "Capacity exceeded.";
      end if;
      pragma Annotate (Xcov, Exempt_Off);

      This.Elements (This.Last_Used_Index + 1) := new (This.Subpool.all'Access) Element_Type'(New_Item);
      --  Allocate_From_Subpool may raise an exception, so don't change Last_Used_Index until after this point.

      This.Last_Used_Index := @ + 1;
   end Append;

   procedure Clear (This : in out Vector) is
   begin
      --  No init required. Guarded by Last_Used_Index check.

      for E of This.Elements (Index_Type'First .. This.Last_Used_Index) loop
         Free (E);
         pragma Unreferenced (E);
         E := null;
      end loop;

      This.Last_Used_Index := Extended_Index'First;
   end Clear;

   function Element (This : Vector; Index : Index_Type) return Element_Type is
   begin
      --  No init required. Elements will just be all nulls before init.
      return This.Elements (Index).all;
   end Element;

   procedure Maybe_Initialize (This : in out Vector) is
   begin
      if This.Subpool = null then
         Dummy_Allocator.Next_Allocation_Address := This.Storage (Rounded_Storage_Size + 1)'Address;
         This.Subpool :=
           new Vector_Elements_Subpool'
             (Root_Subpool
              with
                Current_Free => This.Storage'Address,
                End_Address  =>
                  (if Rounded_Storage_Size = 0
                   then This.Storage'Address
                   else This.Storage (Rounded_Storage_Size)'Address));
         Dummy_Allocator.Next_Allocation_Address := System.Null_Address;

         Set_Pool_Of_Subpool (This.Subpool.all'Unchecked_Access, Vector_Elements_Root_Pool);
      end if;
   end Maybe_Initialize;

   overriding
   procedure Adjust (This : in out Vector) is
      Old_Last_Used_Index : constant Extended_Index := This.Last_Used_Index;
   begin
      This.Subpool := null;
      This.Maybe_Initialize;

      for E of This.Elements (Index_Type'First .. Old_Last_Used_Index) loop
         E := new (This.Subpool.all'Access) Element_Type'(E.all);
      end loop;
   end Adjust;

   overriding
   procedure Finalize (This : in out Vector) is
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

   function Is_Empty (This : Vector) return Boolean is
   begin
      return This.Last_Used_Index < Index_Type'First;
   end Is_Empty;

   function Last_Index (This : Vector) return Extended_Index is
   begin
      return This.Last_Used_Index;
   end Last_Index;

   procedure Process_Range
     (This   : Vector;
      Start  : Index_Type;
      Finish : Extended_Index;
      Action : not null access procedure (Item : in out Element_Type)) is
   begin
      --  No init required. Guarded by Last_Used_Index precondition and .all on null.

      for E of This.Elements (Start .. Finish) loop
         Action (E.all);
      end loop;
   end Process_Range;

   package body Subpool_Support is

      use type System.Address;

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
      function Create_Subpool (Pool : in out Vector_Elements_Root_Pool_Type) return not null Subpool_Handle is
      begin
         pragma Annotate (Xcov, Exempt_On, "Should never be called.");
         return raise Program_Error with "Should never be called.";
         pragma Annotate (Xcov, Exempt_Off);
      end Create_Subpool;

      overriding
      procedure Allocate_From_Subpool
        (Pool                     : in out Vector_Elements_Root_Pool_Type;
         Storage_Address          : out System.Address;
         Size_In_Storage_Elements : Storage_Count;
         Alignment                : Storage_Count;
         Subpool                  : not null Subpool_Handle)
      is
         V_Subpool    : constant access Vector_Elements_Subpool := Vector_Elements_Subpool (Subpool.all)'Access;
         Aligned      : constant System.Address := Aligned_Address (V_Subpool.Current_Free, Alignment);
         Rounded_Size : constant Storage_Count := Round_Up_Size (Size_In_Storage_Elements, Alignment);
      begin
         if Rounded_Size > Rounded_Storage_Size then
            raise Program_Error with "Element will never fit in Vector.";
         end if;

         if Aligned > V_Subpool.End_Address or else V_Subpool.End_Address - Aligned < Storage_Offset (Rounded_Size) - 1
         then
            raise Out_Of_Space_Error with "Storage exhausted.";
         end if;

         Storage_Address := Aligned;
         V_Subpool.Current_Free := Aligned + Rounded_Size;
      end Allocate_From_Subpool;

      overriding
      procedure Deallocate_Subpool (Pool : in out Vector_Elements_Root_Pool_Type; Subpool : in out Subpool_Handle) is
      begin
         null;
      end Deallocate_Subpool;

   end Subpool_Support;

end Prunt.Bounded_Indefinite_Vectors;
