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
