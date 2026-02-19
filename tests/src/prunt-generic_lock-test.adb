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
