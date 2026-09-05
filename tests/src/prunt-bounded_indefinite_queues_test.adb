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
      use System.Storage_Elements;
      package Alignment_Checks is
         type Aligned_Item is new Ada.Finalization.Controlled with record
            V : Integer := 0;
         end record
         with Alignment => Standard'Maximum_Alignment;
         --  GNAT caps controlled types at Maximum_Alignment.

         Adjustments : Natural := 0;
         overriding procedure Adjust (Object : in out Aligned_Item);
      end Alignment_Checks;

      package body Alignment_Checks is
         overriding procedure Adjust (Object : in out Aligned_Item) is
         begin
            --  Adjust runs on the actual pool allocation, unlike a returned copy
            --  from Peek or Dequeue. Enqueue uses a copy, so Initialize is not called.
            T.Assert (Object'Address mod Aligned_Item'Alignment = 0, "Misaligned pool element");
            Adjustments := @ + 1;
         end Adjust;
      end Alignment_Checks;

      use Alignment_Checks;
      package Aligned_Queues is new
        Prunt.Bounded_Indefinite_Queues (Element_Type => Aligned_Item, Storage_Size => 1024);
   begin
      T.Register;
      Q : Aligned_Queues.Queue;
      Item : Aligned_Item;
      Out_Item : Aligned_Item;
      Head : Positive := 1;
      Tail : Natural := 0;
      Failures : Natural := 0;

      for I in 1 .. 200 loop
         Item.V := Tail + 1;
         begin
            Before : constant Natural := Adjustments;
            Q.Enqueue (Item);
            T.Assert (Adjustments = Before + 1, "Must check the allocated element's address");
            Tail := @ + 1;
         exception
            when Aligned_Queues.Out_Of_Space_Error =>
               Failures := @ + 1;
               Q.Dequeue (Out_Item);
               T.Assert (Out_Item.V = Head, "Aligned queue payload corrupted");
               Head := @ + 1;
         end;
      end loop;
      T.Assert (Failures > 1, "Exercise exhaustion and wraparound repeatedly");
      while not Q.Is_Empty loop
         Q.Dequeue (Out_Item);
         T.Assert (Out_Item.V = Head);
         Head := @ + 1;
      end loop;
      T.Assert (Head = Tail + 1);
   end Test_Alignment;

   procedure Test_Alignment_Collision (T : in out Trendy_Test.Operation'Class) is
      --  Distinct variable-sized payloads expose overwrites of still-live data,
      --  including when a node fits but its element allocation must roll back.
      package Small_Queues is new
        Prunt.Bounded_Indefinite_Queues (Element_Type => String, Storage_Size => 256);

      function Payload (Index : Positive) return String is
        (1 .. 1 + (Index * 17 mod 73) => Character'Val (33 + Index mod 90));
   begin
      T.Register;
      Q : Small_Queues.Queue;
      Head : Positive := 1;
      Tail : Natural := 0;
      Failures : Natural := 0;

      for I in 1 .. 1000 loop
         declare
            Value : constant String := Payload (Tail + 1);
            Fits : constant Boolean := Q.Can_Enqueue (Value);
         begin
            begin
               Q.Enqueue (Value);
               T.Assert (Fits, "Capacity probe disagrees with Enqueue");
               Tail := @ + 1;
            exception
               when Small_Queues.Out_Of_Space_Error =>
                  T.Assert (not Fits);
                  Failures := @ + 1;
                  --  Drain only one item to keep live allocations across wraps.
                  Actual : String (Payload (Head)'Range);
                  Q.Dequeue (Actual);
                  T.Assert (Actual = Payload (Head), "Live payload overwritten after exhaustion");
                  Head := @ + 1;
            end;
         end;
         if not Q.Is_Empty then
            T.Assert (Q.Peek = Payload (Head), "Head corrupted during allocation or rollback");
         end if;
      end loop;
      T.Assert (Failures > 10);
      Copy : Small_Queues.Queue := Q;
      while not Q.Is_Empty loop
         Actual : String (Payload (Head)'Range);
         Q.Dequeue (Actual);
         T.Assert (Actual = Payload (Head));
         Copy.Dequeue (Actual);
         T.Assert (Actual = Payload (Head), "Wrapped copy corrupted");
         Head := @ + 1;
      end loop;
      T.Assert (Head = Tail + 1 and then Copy.Is_Empty);
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

   procedure Test_Can_Enqueue (T : in out Trendy_Test.Operation'Class) is
      package Small_Queues is new Prunt.Bounded_Indefinite_Queues (Element_Type => String, Storage_Size => 40);
   begin
      T.Register;

      Q : Test_Queues.Queue;
      S : String (1 .. 300);

      T.Assert (Q.Can_Enqueue ("One"));
      T.Assert (Q.Is_Empty);

      Q.Enqueue ((1 .. 300 => 'A'));
      T.Assert (not Q.Can_Enqueue ((1 .. 300 => 'B')));

      Q.Dequeue (S);
      T.Assert (S = (1 .. 300 => 'A'));
      T.Assert (Q.Can_Enqueue ((1 .. 300 => 'C')));
      T.Assert (Q.Is_Empty);

      Small_Q : Small_Queues.Queue;
      Small_S : String (1 .. 1);

      T.Assert (Small_Q.Can_Enqueue ("A", 2));
      T.Assert (not Small_Q.Can_Enqueue ("A", 3));
      T.Assert (Small_Q.Is_Empty);

      Small_Q.Enqueue ("A");
      T.Assert (Small_Q.Can_Enqueue ("B"));
      T.Assert (not Small_Q.Can_Enqueue ("B", 2));
      Small_Q.Dequeue (Small_S);
      T.Assert (Small_S = "A");
      T.Assert (Small_Q.Is_Empty);

      begin
         if Small_Q.Can_Enqueue ((1 .. 70 => 'A')) then
            null;
         end if;
         T.Fail ("Can_Enqueue should raise Program_Error for items that cannot fit into an empty queue.");
      exception
         when Program_Error =>
            null;
      end;
   end Test_Can_Enqueue;

   procedure Test_Can_Enqueue_Rolls_Back_Unexpected_Exception (T : in out Trendy_Test.Operation'Class) is
      package Raising_Adjust_Checks is
         Adjust_Error : exception;

         type Test_Item is new Ada.Finalization.Controlled with record
            Raise_On_Adjust : Boolean := False;
            Value           : Integer := 0;
         end record;

         overriding
         procedure Adjust (Object : in out Test_Item);
      end Raising_Adjust_Checks;

      package body Raising_Adjust_Checks is
         overriding
         procedure Adjust (Object : in out Test_Item) is
         begin
            if Object.Raise_On_Adjust then
               raise Adjust_Error;
            end if;
         end Adjust;
      end Raising_Adjust_Checks;

      package Raising_Queues is new
        Prunt.Bounded_Indefinite_Queues (Element_Type => Raising_Adjust_Checks.Test_Item, Storage_Size => 512);

      use Raising_Adjust_Checks;
   begin
      T.Register;

      Q : Raising_Queues.Queue;

      declare
         Bad_Item : constant Test_Item := (Ada.Finalization.Controlled with Raise_On_Adjust => True, Value => 1);
      begin
         begin
            if Q.Can_Enqueue (Bad_Item) then
               null;
            end if;
            T.Fail ("Can_Enqueue should propagate exceptions raised while copying the item.");
         exception
            when Adjust_Error =>
               null;
         end;
      exception
         when Adjust_Error =>
            T.Fail ("Test item setup should not raise Adjust_Error.");
      end;

      T.Assert (Q.Is_Empty);

      Good_Item : Test_Item := (Ada.Finalization.Controlled with Raise_On_Adjust => False, Value => 2);
      T.Assert (Q.Can_Enqueue (Good_Item));
      T.Assert (Q.Is_Empty);
   end Test_Can_Enqueue_Rolls_Back_Unexpected_Exception;

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
         Test_Can_Enqueue'Access,
         Test_Can_Enqueue_Rolls_Back_Unexpected_Exception'Access,
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
