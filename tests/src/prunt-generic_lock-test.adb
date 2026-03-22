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

with Trendy_Test; use Trendy_Test;

package body Prunt.Generic_Lock.Test is

   pragma Extensions_Allowed (On);

   procedure Test_Concurrency (T : in out Trendy_Test.Operation'Class) is
      task type Worker;

      task body Worker is
      begin
         for I in 1 .. 1_000 loop
            declare
               L : Lock_Holder := Lock;
            begin
               null;
            end;
         end loop;
      end Worker;
   begin
      T.Register;

      select
         delay 15.0;
         T.Fail ("Deadlock likely");
      then abort
         declare
            Workers : array (1 .. 100) of Worker;
         begin
            null;
         end;
      end select;
   end Test_Concurrency;

   procedure Test_Double_Finalize (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         L : Lock_Holder := Lock;
      begin
         L.Finalize;
         L.Finalize;
         L.Finalize;
      end;

      declare
         L : Lock_Holder := Lock;
      begin
         null;
      end;
   end Test_Double_Finalize;

   procedure Test_Lock_Unlock (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      for I in 1 .. 100_000 loop
         declare
            L : Lock_Holder := Lock;
         begin
            null;
         end;
      end loop;
   end Test_Lock_Unlock;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Concurrency'Unrestricted_Access,
         Test_Double_Finalize'Unrestricted_Access,
         Test_Lock_Unlock'Unrestricted_Access];
   end All_Tests;

end Prunt.Generic_Lock.Test;
