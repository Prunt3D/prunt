-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

pragma Extensions_Allowed (On);

with System.Storage_Elements;

private with Ada.Finalization;
private with System;
private with System.Storage_Pools.Subpools;

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

   procedure Append (This : in out Vector; New_Item : Element_Type);
   --  Raises `Out_Of_Space_Error` if there is no space for the element in the backing storage.

   procedure Clear (This : in out Vector);

   procedure Process_Range
     (This   : Vector;
      Start  : Index_Type;
      Finish : Extended_Index;
      Action : not null access procedure (Item : in out Element_Type));

   function Last_Index (This : Vector) return Extended_Index;

   function Element (This : Vector; Index : Index_Type) return Element_Type;

   function Is_Empty (This : Vector) return Boolean;

private

   use System.Storage_Elements;
   use System.Storage_Pools.Subpools;

   Rounded_Storage_Size : constant Storage_Count :=
     Storage_Size
     + (Standard'Maximum_Alignment - (Storage_Size mod Standard'Maximum_Alignment)) mod Standard'Maximum_Alignment;

   package Subpool_Support is
      function Aligned_Address (Addr : System.Address; Alignment : Storage_Count) return System.Address;
      --  Return Addr, rounded up to multiple of Alignment.

      type Vector_Subpool_Root_Pool_Type is limited new Root_Storage_Pool_With_Subpools with null record;

      type Vector_Subpool_Subpool is limited new Root_Subpool with null record;
      --  This is a dummy subpool which lets us create objects of type `Vector_Elements_Subpool` in arrays within
      --  `Vector` without forcing us to make `Vector` a limited type. This works by simply taking addresses of arrays
      --  passed into `Vector_Subpool_Subpool_Address_Passer` and returning them when we allocate from this subpool.

      protected Vector_Subpool_Subpool_Address_Passer is
         --  `pragma Abort_Defer` must be called around a Set/Get pair (i.e. in `Maybe_Initialize`), otherwise an abort
         --  could be raised after a Set which would leave Set blocked forever. We could use a controlled type to
         --  enforce this, but we only use this in one place so there's not much benefit from the extra overhead.
         --
         --  The supplied Address should be maximally aligned and the size of the underlying array should be of size
         --  `Vector_Elements_Subpool_Storage_Size` or larger. This is done to keep everything simple since we expect
         --  uses of this type to have large storage sizes where the small bit of extra overhead will not matter.

         entry Set_Next_Address (Addr : System.Address);
         procedure Get_Next_Address (Addr : out System.Address);
      private
         Next : System.Address := System.Null_Address;
      end Vector_Subpool_Subpool_Address_Passer;

      overriding
      function Create_Subpool (Pool : in out Vector_Subpool_Root_Pool_Type) return not null Subpool_Handle;
      --  We never use this one. It will raise Program_Error.

      Vector_Subpool_Default_Subpool : aliased Vector_Subpool_Subpool := (Root_Subpool with null record);
      --  The one and only object of this type ever created.

      overriding
      procedure Allocate_From_Subpool
        (Pool                     : in out Vector_Subpool_Root_Pool_Type;
         Storage_Address          : out System.Address;
         Size_In_Storage_Elements : Storage_Count;
         Alignment                : Storage_Count;
         Subpool                  : not null Subpool_Handle)
      with Pre => Subpool = Vector_Subpool_Default_Subpool'Unchecked_Access;

      overriding
      procedure Deallocate_Subpool (Pool : in out Vector_Subpool_Root_Pool_Type; Subpool : in out Subpool_Handle);

      Vector_Subpool_Root_Pool : Vector_Subpool_Root_Pool_Type := (Root_Storage_Pool_With_Subpools with null record);
      --  The one and only object of this type ever created.

      type Vector_Elements_Root_Pool_Type is limited new Root_Storage_Pool_With_Subpools with null record;

      type Vector_Elements_Subpool is limited new Root_Subpool with record
         --  These subpools hold the actual vector elements.
         Current_Free : System.Address := System.Null_Address;
         End_Address  : System.Address := System.Null_Address;
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
        Vector_Elements_Subpool'Max_Size_In_Storage_Elements
        + (Standard'Maximum_Alignment
           - (Vector_Elements_Subpool'Max_Size_In_Storage_Elements mod Standard'Maximum_Alignment))
          mod Standard'Maximum_Alignment;
   end Subpool_Support;

   use Subpool_Support;

   type Element_Access is access Element_Type
   with Storage_Pool => Vector_Elements_Root_Pool, Size => Standard'Address_Size;
   --  Size specification needed to ensure contiguous bounds if Element_Type turns out to be an unconstrained array
   --  subtype. We do not want a fat-pointer representation in that case.
   --
   --  TODO: This is copied from GNAT's Bounded_Indefinite_Holder implementation. Why don't we want fat pointers here?

   type Pooled_Subpool_Handle is access Vector_Elements_Subpool with Storage_Pool => Vector_Subpool_Root_Pool;

   pragma No_Strict_Aliasing (Pooled_Subpool_Handle);
   pragma No_Strict_Aliasing (Element_Access);
   --  Needed because we are unchecked-converting from Address to Element_Access (see package body), which is a
   --  violation of the normal aliasing rules enforced by gcc.
   --
   --  This is copied from GNAT's Bounded_Indefinite_Holder implementation. It does not appear that it is actually
   --  required, but there should be no harm in keeping it.

   type Element_Array is array (Index_Type) of Element_Access;

   type Elements_Subpool_Record_Array is array (1 .. Vector_Elements_Subpool_Storage_Size) of aliased Storage_Element
   with Component_Size => System.Storage_Unit, Alignment => Standard'Maximum_Alignment;

   type Aligned_Storage_Array is array (Storage_Offset range <>) of aliased Storage_Element
   with Component_Size => System.Storage_Unit, Alignment => Standard'Maximum_Alignment;
   --  We use maximum alignment here to simplify copying between vectors by avoiding cases where the alignment of the
   --  new object causes items to not fit when they previously did.

   type Vector is new Ada.Finalization.Controlled with record
      Last_Used_Index        : Extended_Index := Extended_Index'First;
      Elements               : Element_Array := [others => null];
      Subpool                : Pooled_Subpool_Handle := null;
      Subpool_Record_Storage : aliased Elements_Subpool_Record_Array;
      Storage                : aliased Storage_Array (1 .. Rounded_Storage_Size);
   end record
   with Preelaborable_Initialization => Element_Type'Preelaborable_Initialization;

   procedure Maybe_Initialize (This : in out Vector);

   --  No initialize procedure as `Vector` might have `Preelaborable_Initialization => True`. Instead we just run
   --  initialization when `Subpool = null` inside a procedure which makes use of it.
   overriding
   procedure Adjust (This : in out Vector);
   overriding
   procedure Finalize (This : in out Vector);

end Prunt.Bounded_Indefinite_Vectors;
