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

with Ada.Containers; use Ada.Containers;
with Ada.Finalization;
with Prunt.Bounded_Indefinite_Queues;
with System.Assertions;
with System.Storage_Elements;
with Trendy_Test;    use Trendy_Test;

package body Prunt.Bounded_Indefinite_Queues_Test is

   pragma Extensions_Allowed (On);

   type Test_Index is range 1 .. 5;

   package Test_Queues is new Prunt.Bounded_Indefinite_Queues (Element_Type => String, Storage_Size => 512);

   procedure Test_Adjust (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q1 : Test_Queues.Queue;
      Q2 : Test_Queues.Queue;
      S : String (1 .. 3);

      Q1.Enqueue ("One");
      Q1.Enqueue ("Two");

      Q2 := Q1;

      Q1.Dequeue (S);
      T.Assert (S = "One");
      Q1.Clear;

      Q2.Dequeue (S);
      T.Assert (S = "One");

      Q2.Dequeue (S);
      T.Assert (S = "Two");

      Q2.Enqueue ("New");
      Q2.Dequeue (S);
      T.Assert (S = "New");
   end Test_Adjust;

   procedure Test_Adjust_Wrap (T : in out Trendy_Test.Operation'Class) is
      package Small_Queues is new Prunt.Bounded_Indefinite_Queues (Element_Type => String, Storage_Size => 40);
   begin
      T.Register;

      Q1 : Small_Queues.Queue;
      Q2 : Small_Queues.Queue;
      S : String (1 .. 1);

      Q1.Enqueue ("A");
      Q1.Enqueue ("B");

      begin
         Q1.Enqueue ("C");
         T.Fail ("We need to trigger Out_Of_Space_Error here so we know we've wrapped.");
      exception
         when Small_Queues.Out_Of_Space_Error =>
            null;
      end;

      Q1.Dequeue (S);
      Q1.Enqueue ("C");

      Q2 := Q1;

      Q2.Dequeue (S);
      T.Assert (S = "B");
      Q2.Dequeue (S);
      T.Assert (S = "C");
   end Test_Adjust_Wrap;

   procedure Test_Alignment (T : in out Trendy_Test.Operation'Class) is
      type Aligned_Item is record
         V : Integer;
      end record
      with Alignment => 64;

      package Aligned_Queues is new
        Prunt.Bounded_Indefinite_Queues (Element_Type => Aligned_Item, Storage_Size => 1024);
   begin
      T.Register;

      --  TODO: Use a controlled type here so we can test the alignment in `Initialize`.

      Q : Aligned_Queues.Queue;
      Item : Aligned_Item := (V => 123);
      Out_Item : Aligned_Item;

      Q.Enqueue (Item);
      Q.Dequeue (Out_Item);
      T.Assert (Out_Item.V = 123);
   end Test_Alignment;

   procedure Test_Alignment_Collision (T : in out Trendy_Test.Operation'Class) is
      --  We need to construct a scenario where Current_Free is unaligned and just before the head address. We're just
      --  relying on coverage testing to tell us we've hit the statement we're trying to test.

      --  TODO: Improve this to check that there's no memory corruption.
      package Address_Queues is new
        Prunt.Bounded_Indefinite_Queues (Element_Type => System.Address, Storage_Size => 256);
   begin
      T.Register;

      Q : Address_Queues.Queue;
      A : System.Address;

      Q.Enqueue (System.Null_Address);

      for I in 1 .. 5 loop
         Q.Enqueue (System.Null_Address);
      end loop;

      Q.Dequeue (A);

      declare
         Full : Boolean := False;
      begin
         for I in 1 .. 100 loop
            begin
               Q.Enqueue (System.Null_Address);
            exception
               when Address_Queues.Out_Of_Space_Error =>
                  Full := True;
                  exit;
            end;
         end loop;
         T.Assert (Full);
      end;
   end Test_Alignment_Collision;

   procedure Test_Assign_Empty (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q1 : Test_Queues.Queue;
      Q2 : Test_Queues.Queue;

      Q2.Enqueue ("Something");
      Q2 := Q1;
      T.Assert (Q2.Is_Empty);

      Q2.Enqueue ("New");

      S : String (1 .. 3);

      Q2.Dequeue (S);
      T.Assert (S = "New");
   end Test_Assign_Empty;

   procedure Test_Clear (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Test_Queues.Queue;

      Q.Enqueue ("One");
      Q.Enqueue ("Two");

      Q.Clear;
      T.Assert (Q.Is_Empty);

      Q.Enqueue ("Three");

      S : String (1 .. 5);

      Q.Dequeue (S);
      T.Assert (S = "Three");
   end Test_Clear;

   procedure Test_Empty_Operations (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Test_Queues.Queue;
      Item : String (1 .. 5);

      Q.Clear;
      T.Assert (Q.Is_Empty);

      begin
         Q.Dequeue (Item);
         T.Fail ("Dequeue empty should raise error");
      exception
         when Constraint_Error | System.Assertions.Assert_Failure =>
            null;
      end;

      begin
         declare
            Val : String := Q.Peek;
         begin
            T.Fail ("Peek empty should raise error");
         end;
      exception
         when Constraint_Error | System.Assertions.Assert_Failure =>
            null;
      end;
   end Test_Empty_Operations;

   procedure Test_Enqueue_And_Dequeue (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         Q : Test_Queues.Queue;

         procedure Deq (Comp : String) is
            Val : String (1 .. Comp'Length);
         begin
            Q.Dequeue (Val);
            T.Assert (Val = Comp, "Expected " & Comp & ", got " & Val);
         end Deq;
      begin
         T.Assert (Q.Is_Empty);

         Q.Enqueue ("One");
         T.Assert (not Q.Is_Empty);

         Q.Enqueue ("Two");

         Deq ("One");

         Q.Enqueue ("Three");
         Q.Enqueue ("Four");
         Q.Enqueue ("Five");
         Q.Enqueue ("Six");

         Deq ("Two");
         Deq ("Three");
         Deq ("Four");
         Deq ("Five");
         Deq ("Six");

         T.Assert (Q.Is_Empty);
      end;
   end Test_Enqueue_And_Dequeue;

   procedure Test_Finalization (T : in out Trendy_Test.Operation'Class) is
      package Finalization_Checks is
         type Counter_Access is access Integer;

         type Test_Item is new Ada.Finalization.Controlled with record
            Finalized_Counter : Counter_Access;
            Value             : Integer;
         end record;

         overriding
         procedure Finalize (Object : in out Test_Item);
      end Finalization_Checks;

      package body Finalization_Checks is
         overriding
         procedure Finalize (Object : in out Test_Item) is
         begin
            if Object.Finalized_Counter /= null then
               Object.Finalized_Counter.all := @ + 1;
            end if;
         end Finalize;
      end Finalization_Checks;

      package Finalization_Queues is new
        Prunt.Bounded_Indefinite_Queues (Element_Type => Finalization_Checks.Test_Item, Storage_Size => 512);

      use Finalization_Checks;
   begin
      T.Register;

      Q : Finalization_Queues.Queue;
      Counter : Counter_Access := new Integer'(0);

      declare
         Item : Test_Item := (Ada.Finalization.Controlled with Finalized_Counter => Counter, Value => 1);
      begin
         Q.Enqueue (Item);
         Q.Enqueue (Item);
         T.Assert (Counter.all = 0);
      end;
      T.Assert (Counter.all = 1, "Temporary item finalized");

      declare
         Item : Test_Item;
      begin
         Q.Dequeue (Item);
         T.Assert (Counter.all = 2);
      end;
      T.Assert (Counter.all = 3);

      Q.Clear;
      T.Assert (Counter.all = 4);
   end Test_Finalization;

   procedure Test_Oversized_Allocation (T : in out Trendy_Test.Operation'Class) is
      package Tiny_Queues is new Prunt.Bounded_Indefinite_Queues (Element_Type => String, Storage_Size => 16);
   begin
      T.Register;

      Q : Tiny_Queues.Queue;

      begin
         Q.Enqueue ((1 .. 20 => 'A'));
         T.Fail ("Should raise Program_Error for oversized component");
      exception
         when Program_Error =>
            null;
         when Tiny_Queues.Out_Of_Space_Error =>
            T.Fail ("Should be Program_Error instead of Out_Of_Space_Error.");
      end;
   end Test_Oversized_Allocation;

   procedure Test_Peek_Normal (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Test_Queues.Queue;

      Q.Enqueue ("PeekMe");
      T.Assert (Q.Peek = "PeekMe");
   end Test_Peek_Normal;

   procedure Test_Really_Oversized_Allocation (T : in out Trendy_Test.Operation'Class) is
      package Tiny_Queues is new Prunt.Bounded_Indefinite_Queues (Element_Type => String, Storage_Size => 16);
      Q : Tiny_Queues.Queue;
   begin
      T.Register;
      begin
         Q.Enqueue ((1 .. 100 => 'A'));
         T.Fail ("Should have raised Program_Error for really oversized component");
      exception
         when Program_Error =>
            null;
         when Tiny_Queues.Out_Of_Space_Error =>
            T.Fail ("Should be Program_Error instead of Out_Of_Space_Error.");
      end;
   end Test_Really_Oversized_Allocation;

   procedure Test_Storage_Exceeded (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Test_Queues.Queue;

      Q.Enqueue ((1 .. 300 => 'A'));

      begin
         Q.Enqueue ((1 .. 300 => 'A'));
         T.Fail ("Should have raised Out_Of_Space_Error.");
      exception
         when Test_Queues.Out_Of_Space_Error =>
            null;
      end;
   end Test_Storage_Exceeded;

   procedure Test_Zero_Size_Storage (T : in out Trendy_Test.Operation'Class) is
      type Null_Record is null record;
      package Zero_Queues is new Prunt.Bounded_Indefinite_Queues (Element_Type => Null_Record, Storage_Size => 0);
   begin
      T.Register;

      Q : Zero_Queues.Queue;
      N : Null_Record;

      Q.Enqueue ((null record));

      begin
         Q.Enqueue ((null record));
         T.Fail ("Should have raised Out_Of_Space_Error.");
      exception
         when Zero_Queues.Out_Of_Space_Error =>
            null;
      end;

      Q.Dequeue (N);
      T.Assert (Q.Is_Empty);

      Q.Enqueue ((null record));
      T.Assert (not Q.Is_Empty);
   end Test_Zero_Size_Storage;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Adjust'Access,
         Test_Adjust_Wrap'Access,
         Test_Alignment'Access,
         Test_Alignment_Collision'Access,
         Test_Assign_Empty'Access,
         Test_Clear'Access,
         Test_Empty_Operations'Access,
         Test_Enqueue_And_Dequeue'Access,
         Test_Finalization'Access,
         Test_Oversized_Allocation'Access,
         Test_Peek_Normal'Access,
         Test_Really_Oversized_Allocation'Access,
         Test_Storage_Exceeded'Access,
         Test_Zero_Size_Storage'Access];
   end All_Tests;

end Prunt.Bounded_Indefinite_Queues_Test;
