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

with Trendy_Test;             use Trendy_Test;
with Prunt.Dummy_Allocator;
with System;
with System.Storage_Elements; use System.Storage_Elements;

package body Prunt.Dummy_Allocator.Test is

   pragma Extensions_Allowed (On);

   use type System.Address;

   procedure Test_Allocation (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Pool : Prunt.Dummy_Allocator.Dummy_Pool_Type;
      Addr : System.Address;
      Target : aliased Integer;

      Prunt.Dummy_Allocator.Next_Allocation_Address := Target'Address;
      Pool.Allocate (Addr, 10, 1);
      T.Assert (Addr = Target'Address);
   end Test_Allocation;

   procedure Test_Missing_Address (T : in out Trendy_Test.Operation'Class) is
      use System.Storage_Elements;
   begin
      T.Register;

      Pool : Prunt.Dummy_Allocator.Dummy_Pool_Type;
      Addr : System.Address;

      begin
         Pool.Allocate (Addr, 10, 1);
         T.Assert (False, "Should have raised Program_Error");
      exception
         when Program_Error =>
            null;
      end;
   end Test_Missing_Address;

   procedure Test_Storage_Size (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Pool : Prunt.Dummy_Allocator.Dummy_Pool_Type;

      T.Assert (Pool.Storage_Size = Storage_Count'Last);
   end Test_Storage_Size;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return [Test_Allocation'Access, Test_Missing_Address'Access, Test_Storage_Size'Access];
   end All_Tests;

end Prunt.Dummy_Allocator.Test;
