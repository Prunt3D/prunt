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

with Trendy_Test;             use Trendy_Test;

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

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return [Test_Allocation'Access, Test_Missing_Address'Access];
   end All_Tests;

end Prunt.Dummy_Allocator.Test;
