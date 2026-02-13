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

with Ada.Unchecked_Deallocate_Subpool;
with Ada.Unchecked_Deallocation;

package body Prunt.Bounded_Indefinite_Vectors is

   pragma Extensions_Allowed (On);

   procedure Free is new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   procedure Append (This : in out Vector; New_Item : Element_Type) is
      Ptr : Element_Access;
   begin
      if This.Last_Index = Index_Type'Last then
         raise Constraint_Error with "Capacity exceeded";
      end if;

      Ptr := new (This.Subpool'Unchecked_Access) Element_Type'(New_Item);
      --  May raise Out_Of_Space_Error, so don't change `Last_Index` yet.

      This.Last_Used_Index := @ + 1;
      This.Elements (This.Last_Used_Index) := Ptr;
   end Append;

   procedure Clear (This : in out Vector) is
   begin
      for E of This.Elements (Index_Type'First .. This.Last_Index) loop
         Free (E);
         pragma Unreferenced (E);
         E := null;
      end loop;

      This.Last_Used_Index := Extended_Index'First;
   end Clear;

   function Element (This : Vector; Index : Index_Type) return Element_Type is
   begin
      if Index > This.Last_Index or else Index < Index_Type'First then
         raise Constraint_Error with "Index out of bounds";
      end if;
      return This.Elements (Index).all;
   end Element;

   overriding
   procedure Initialize (This : in out Vector) is
   begin
      This.Subpool.Current_Free := This.Storage'Address;
      This.Subpool.End_Address := This.Storage'Address + This.Storage'Length;
      Set_Pool_Of_Subpool (This.Subpool'Unchecked_Access, The_Storage_Pool);
   end Initialize;

   overriding
   procedure Finalize (This : in out Vector) is
      Handle : Subpool_Handle := This.Subpool'Unchecked_Access;
   begin
      Clear (This);

      Ada.Unchecked_Deallocate_Subpool (Handle);
      --  TODO: Do we need this or does it happen automatically?

      pragma Unreferenced (Handle);
   end Finalize;

   function Is_Empty (This : Vector) return Boolean is
   begin
      return This.Last_Index < Index_Type'First;
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
      if Finish > This.Last_Index or else not Start'Valid then
         raise Constraint_Error with "Index out of bounds";
      end if;

      for E of This.Elements (Start .. Finish) loop
         Action (E.all);
      end loop;
   end Process_Range;

   package body Subpool_Support is

      function Aligned_Address (Addr : System.Address; Alignment : Storage_Count) return System.Address;
      --  Return Addr, rounded up to multiple of Alignment.

      overriding
      function Create_Subpool (Pool : in out Vector_Pool_Type) return not null Subpool_Handle is
      begin
         pragma Annotate (Xcov, Exempt_On, "Should never be called.");
         return raise Program_Error;
         pragma Annotate (Xcov, Exempt_Off);
      end Create_Subpool;

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
      procedure Allocate_From_Subpool
        (Pool                     : in out Vector_Pool_Type;
         Storage_Address          : out System.Address;
         Size_In_Storage_Elements : Storage_Count;
         Alignment                : Storage_Count;
         Subpool                  : not null Subpool_Handle)
      is
         use type System.Address;

         V_Subpool : constant access Vector_Subpool := Vector_Subpool (Subpool.all)'Access;

         Aligned : constant System.Address := Aligned_Address (V_Subpool.Current_Free, Alignment);

         Size_Padding : constant Storage_Count := (Alignment - (Size_In_Storage_Elements mod Alignment)) mod Alignment;
         Rounded_Size : constant Storage_Count := Size_In_Storage_Elements + Size_Padding;
      begin
         if Rounded_Size > Bounded_Indefinite_Vectors.Rounded_Storage_Size
           or else V_Subpool.End_Address < Aligned
           or else Aligned <= V_Subpool.Current_Free
           or else V_Subpool.End_Address - Aligned < Rounded_Size
         then
            raise Out_Of_Space_Error with "Storage exhausted";
         end if;

         Storage_Address := Aligned;
         V_Subpool.Current_Free := Aligned + Rounded_Size;
      end Allocate_From_Subpool;

      overriding
      procedure Deallocate_Subpool (Pool : in out Vector_Pool_Type; Subpool : in out Subpool_Handle) is
      begin
         null;
      end Deallocate_Subpool;

   end Subpool_Support;

end Prunt.Bounded_Indefinite_Vectors;
