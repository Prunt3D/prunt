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
