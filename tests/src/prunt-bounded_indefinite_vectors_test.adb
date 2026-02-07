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
with Prunt.Bounded_Indefinite_Vectors;
with System.Assertions;
with System.Storage_Elements;
with Trendy_Test;    use Trendy_Test;

package body Prunt.Bounded_Indefinite_Vectors_Test is

   pragma Extensions_Allowed (On);

   type Test_Index is range 1 .. 5;

   package Test_Vectors is new
     Prunt.Bounded_Indefinite_Vectors (Element_Type => String, Index_Type => Test_Index, Storage_Size => 512);

   use Test_Vectors;

   package Finalization_Checks is
      type Counter_Access is access Integer;

      type Test_Item is new Ada.Finalization.Controlled with record
         Finalized_Counter : Counter_Access;
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
            Object.Finalized_Counter := null;
         end if;
      end Finalize;
   end Finalization_Checks;

   package Finalization_Vectors is new
     Prunt.Bounded_Indefinite_Vectors
       (Element_Type => Finalization_Checks.Test_Item,
        Index_Type   => Test_Index,
        Storage_Size => 512);

   procedure Test_Adjust (T : in out Trendy_Test.Operation'Class) is
      V1 : Vector;
      V2 : Vector;
   begin
      T.Register;

      V1.Append ("Hello");
      V1.Append ("World!");

      V2 := V1;

      T.Assert (V2.Last_Index = 2);
      T.Assert (V2.Element (1) = "Hello");
      T.Assert (V2.Element (2) = "World!");

      V1.Clear;
      T.Assert (V2.Last_Index = 2);
      T.Assert (V2.Element (1) = "Hello");
      T.Assert (V2.Element (2) = "World!");

      T.Assert (V1.Last_Index = 0);
   end Test_Adjust;

   procedure Test_Alignment (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         type R is tagged null record;
         type R2 is new R with null record with Alignment => 16;

         package Align_Vectors is new
           Prunt.Bounded_Indefinite_Vectors (Element_Type => R'Class, Index_Type => Test_Index, Storage_Size => 128);

         V : Align_Vectors.Vector;

         procedure Check_Alignment (X : in out R'Class) is
            use System.Storage_Elements;
         begin
            T.Assert (X'Address mod 16 = 0);
         end Check_Alignment;
      begin
         V.Append (R'(null record));
         V.Append (R2'(R with null record));
         V.Append (R'(null record));
         V.Append (R2'(R with null record));

         T.Assert (V.Last_Index = 4);

         T.Assert (V.Element (1) in R);
         T.Assert (V.Element (2) in R2);
         T.Assert (V.Element (3) in R);
         T.Assert (V.Element (4) in R2);

         V.Process_Range (2, 2, Check_Alignment'Access);
         V.Process_Range (4, 4, Check_Alignment'Access);
      end;
   end Test_Alignment;

   procedure Test_Append_And_Read (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      V : Vector;

      T.Assert (V.Is_Empty);
      T.Assert (V.Last_Index = 0);

      V.Append ("Hello");
      T.Assert (not V.Is_Empty);
      T.Assert (V.Last_Index = 1);
      T.Assert (V.Element (1) = "Hello");

      V.Append ("World!");
      T.Assert (V.Last_Index = 2);
      T.Assert (V.Element (2) = "World!");
   end Test_Append_And_Read;

   procedure Test_Capacity_Exceeded (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      V : Vector;

      V.Append ("1");
      V.Append ("2");
      V.Append ("3");
      V.Append ("4");
      V.Append ("5");

      begin
         V.Append ("6");
         T.Fail ("Should be out of space.");
      exception
         when Constraint_Error | System.Assertions.Assert_Failure =>
            null;
      end;
   end Test_Capacity_Exceeded;

   procedure Test_Clear (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      V : Vector;

      V.Append ("One");
      V.Append ("Two");
      T.Assert (V.Last_Index = 2);

      V.Clear;
      T.Assert (V.Is_Empty);
      T.Assert (V.Last_Index = 0);

      V.Append ("Three");
      T.Assert (V.Last_Index = 1);
      T.Assert (V.Element (1) = "Three");
   end Test_Clear;

   procedure Test_Element_Index_Error (T : in out Trendy_Test.Operation'Class) is
      V : Vector;
      S : String (1 .. 5);
   begin
      T.Register;

      V.Append ("Test");

      begin
         S := V.Element (0);
         T.Fail ("Should have raised Constraint_Error (Index < First)");
      exception
         when Constraint_Error | System.Assertions.Assert_Failure =>
            null;
      end;

      begin
         S := V.Element (2);
         T.Fail ("Should have raised Constraint_Error (Index > Last)");
      exception
         when Constraint_Error | System.Assertions.Assert_Failure =>
            null;
      end;
   end Test_Element_Index_Error;

   procedure Test_Element_Too_Large (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      V : Vector;
      Long_String : String (1 .. 1000) := (others => 'A');

      begin
         V.Append (Long_String);
         T.Fail ("Should have raised Program_Error.");
      exception
         when Program_Error =>
            null;
      end;
   end Test_Element_Too_Large;

   procedure Test_Finalization_On_Clear (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         use Finalization_Checks;

         V       : Finalization_Vectors.Vector;
         Counter : Counter_Access := new Integer'(0);
      begin
         declare
            Item : Test_Item := (Ada.Finalization.Controlled with Finalized_Counter => Counter);
         begin
            V.Append (Item);
            V.Append (Item);
         end;
         T.Assert (Counter.all = 1, "Temporary item should be finalized");

         V.Clear;
         T.Assert (Counter.all = 3, "Stored items should be finalized on Clear");
      end;
   end Test_Finalization_On_Clear;

   procedure Test_Finalization_On_Scope_Exit (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         use Finalization_Checks;

         Counter : Counter_Access := new Integer'(0);
      begin
         declare
            V : Finalization_Vectors.Vector;
         begin
            declare
               Item : Test_Item := (Ada.Finalization.Controlled with Finalized_Counter => Counter);
            begin
               V.Append (Item);
               V.Append (Item);
            end;
            T.Assert (Counter.all = 1, "Temporary item should be finalized");
         end;

         T.Assert (Counter.all = 3, "Stored items should be finalized on scope exit");
      end;
   end Test_Finalization_On_Scope_Exit;

   procedure Test_Process_Range (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         V     : Vector;
         Count : Integer := 0;

         procedure Action (Item : in out String) is
         begin
            Count := Count + 1;
            if Count = 1 then
               T.Assert (Item = "B");
            elsif Count = 2 then
               T.Assert (Item = "C");
            end if;
         end Action;
      begin
         V.Append ("A");
         V.Append ("B");
         V.Append ("C");
         V.Append ("D");

         V.Process_Range (2, 1, Action'Access);
         T.Assert (Count = 0);

         V.Process_Range (2, 3, Action'Access);
         T.Assert (Count = 2);
      end;
   end Test_Process_Range;

   procedure Test_Process_Range_Index_Error (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         procedure Action (Item : in out String) is
         begin
            null;
         end Action;

         V : Vector;
      begin
         V.Append ("Test");

         begin
            V.Process_Range (1, 2, Action'Access);
            T.Fail ("Should have raised Constraint_Error (Finish > Last)");
         exception
            when Constraint_Error | System.Assertions.Assert_Failure =>
               null;
         end;

         begin
            V.Process_Range (0, 1, Action'Access);
            T.Fail ("Should have raised Constraint_Error (Start < First)");
         exception
            when Constraint_Error | System.Assertions.Assert_Failure =>
               null;
         end;
      end;
   end Test_Process_Range_Index_Error;

   procedure Test_Storage_Exhausted (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      V : Vector;
      Long_String : String (1 .. 400) := (others => 'A');

      V.Append (Long_String);

      begin
         V.Append (Long_String);
         T.Fail ("Should have raised Out_Of_Space_Error.");
      exception
         when Out_Of_Space_Error =>
            null;
      end;
   end Test_Storage_Exhausted;

   procedure Test_Storage_Exhausted_Bytes (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         type Byte_Boolean is new Boolean with Size => 8, Alignment => 1;
         type Big_Index is range 1 .. 1000;
         package Boolean_Vectors is new
           Prunt.Bounded_Indefinite_Vectors
             (Element_Type => Byte_Boolean,
              Index_Type   => Big_Index,
              Storage_Size => 128);
         V : Boolean_Vectors.Vector;
      begin
         for I in 1 .. 128 loop
            V.Append (True);
         end loop;
         begin
            V.Append (True);
            T.Fail ("Should have raised Out_Of_Space_Error.");
         exception
            when Boolean_Vectors.Out_Of_Space_Error =>
               null;
         end;
      end;
   end Test_Storage_Exhausted_Bytes;

   procedure Test_Zero_Size_Storage (T : in out Trendy_Test.Operation'Class) is
      type Null_Record is null record;
      package Zero_Vectors is new
        Prunt.Bounded_Indefinite_Vectors (Element_Type => Null_Record, Index_Type => Test_Index, Storage_Size => 0);
   begin
      T.Register;

      V : Zero_Vectors.Vector;

      V.Append ((null record));
      V.Append ((null record));

      T.Assert (V.Last_Index = 2);
      T.Assert (V.Element (1) = (null record));

      begin
         N : Null_Record := V.Element (3);
         T.Fail ("Should have raised error.");
      exception
         when Constraint_Error | System.Assertions.Assert_Failure =>
            null;
      end;
   end Test_Zero_Size_Storage;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Adjust'Access,
         Test_Alignment'Access,
         Test_Append_And_Read'Access,
         Test_Capacity_Exceeded'Access,
         Test_Clear'Access,
         Test_Element_Index_Error'Access,
         Test_Element_Too_Large'Access,
         Test_Finalization_On_Clear'Access,
         Test_Finalization_On_Scope_Exit'Access,
         Test_Process_Range'Access,
         Test_Process_Range_Index_Error'Access,
         Test_Storage_Exhausted'Access,
         Test_Storage_Exhausted_Bytes'Access,
         Test_Zero_Size_Storage'Access];
   end All_Tests;

end Prunt.Bounded_Indefinite_Vectors_Test;
