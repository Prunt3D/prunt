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
   type Element_Type (<>) is private with Preelaborable_Initialization => False;
   type Index_Type is range <>;
   Storage_Size : System.Storage_Elements.Storage_Count;
package Prunt.Bounded_Indefinite_Vectors is

   subtype Extended_Index is Index_Type'Base range Index_Type'First - 1 .. Index_Type'Last;

   type Vector is limited private;
   --  This type could be made non-limited but we currently have no use for that and we use `Root_Subpool` internally
   --  which is a limited type and would require special handling.

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
     (Standard'Maximum_Alignment - (Storage_Size mod Standard'Maximum_Alignment)) mod Standard'Maximum_Alignment;

   package Subpool_Support is

      type Vector_Pool_Type is limited new Root_Storage_Pool_With_Subpools with null record;

      type Vector_Subpool is limited new Root_Subpool with record
         Current_Free : System.Address := System.Null_Address;
         End_Address  : System.Address := System.Null_Address;
      end record;

      overriding
      function Create_Subpool (Pool : in out Vector_Pool_Type) return not null Subpool_Handle;
      --  We never use this one. It will raise Program_Error.

      overriding
      procedure Allocate_From_Subpool
        (Pool                     : in out Vector_Pool_Type;
         Storage_Address          : out System.Address;
         Size_In_Storage_Elements : Storage_Count;
         Alignment                : Storage_Count;
         Subpool                  : not null Subpool_Handle);

      overriding
      procedure Deallocate_Subpool (Pool : in out Vector_Pool_Type; Subpool : in out Subpool_Handle);

      The_Storage_Pool : Vector_Pool_Type;
      --  The one and only object of this type ever created.

   end Subpool_Support;

   use Subpool_Support;

   type Element_Access is access Element_Type
   with Storage_Pool => Subpool_Support.The_Storage_Pool, Size => Standard'Address_Size;
   --  Size specification needed to ensure contiguous bounds if Element_Type turns out to be an unconstrained array
   --  subtype. We do not want a fat-pointer representation in that case.
   --
   --  TODO: This is copied from GNAT's Bounded_Indefinite_Holder implementation. Why don't we want fat pointers here?

   pragma No_Strict_Aliasing (Element_Access);
   --  Needed because we are unchecked-converting from Address to Element_Access (see package body), which is a
   --  violation of the normal aliasing rules enforced by gcc.

   type Element_Array is array (Index_Type) of Element_Access;

   type Aligned_Storage_Array is array (Storage_Offset range <>) of aliased Storage_Element
   with Component_Size => System.Storage_Unit, Alignment => Standard'Maximum_Alignment;
   --  We use maximum alignment here to simplify copying between vectors by avoiding cases where the alignment of the
   --  new object causes items to not fit when they previously did.

   type Vector is new Ada.Finalization.Limited_Controlled with record
      Last_Used_Index : Extended_Index := Extended_Index'First;
      Elements        : Element_Array := [others => null];
      Subpool         : aliased Vector_Subpool;
      Storage         : aliased Storage_Array (1 .. Rounded_Storage_Size);
   end record;

   overriding
   procedure Initialize (This : in out Vector);
   overriding
   procedure Finalize (This : in out Vector);

end Prunt.Bounded_Indefinite_Vectors;
