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
with Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;
with Trendy_Test;    use Trendy_Test;

package body Prunt.Indefinite_Ordered_Maps_With_Insertion_Order_Test is

   pragma Extensions_Allowed (On);

   package Test_Maps is new
     Prunt.Indefinite_Ordered_Maps_With_Insertion_Order (Key_Type => Integer, Element_Type => String);

   use Test_Maps;

   procedure Test_Concatenation (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Left : Map;
      Right : Map;

      Left.Insert (1, "A");
      Left.Insert (2, "B");
      Right.Insert (3, "C");
      Right.Insert (4, "D");

      Result : Map := Left & Right;

      T.Assert (Result.Length = 4);
      T.Assert (Result.Element (1) = "A");
      T.Assert (Result.Element (2) = "B");
      T.Assert (Result.Element (3) = "C");
      T.Assert (Result.Element (4) = "D");

      T.Assert (Result.First_Key = 1);
      T.Assert (Result.Last_Key = 4);
   end Test_Concatenation;

   procedure Test_Constant_Reference (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "Hello");

      T.Assert (M (1) = "Hello");
   end Test_Constant_Reference;

   procedure Test_Constant_Reference_Cursor (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "Hello");

      Position : Cursor := M.First;

      T.Assert (Constant_Reference (M, Position) = "Hello");
   end Test_Constant_Reference_Cursor;

   procedure Test_Contains (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      T.Assert (not M.Contains (1));

      M.Insert (1, "A");
      T.Assert (M.Contains (1));
      T.Assert (not M.Contains (2));
   end Test_Contains;

   procedure Test_Delete (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "A");
      M.Insert (2, "B");
      M.Insert (3, "C");
      T.Assert (M.Length = 3);

      M.Delete (2);
      T.Assert (M.Length = 2);
      T.Assert (not M.Contains (2));
      T.Assert (M.Contains (1));
      T.Assert (M.Contains (3));

      T.Assert (M.First_Key = 1);
      T.Assert (M.Last_Key = 3);
   end Test_Delete;

   procedure Test_Empty (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map := Empty;

      T.Assert (M.Length = 0);
      T.Assert (not Has_Element (M.First));
      T.Assert (not Has_Element (M.Last));
   end Test_Empty;

   procedure Test_Exclude_Missing (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "A");
      M.Exclude (2);

      T.Assert (M.Length = 1);
      T.Assert (M.Contains (1));
      T.Assert (not M.Contains (2));
   end Test_Exclude_Missing;

   procedure Test_Find (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      T.Assert (not Has_Element (M.Find (1)));

      M.Insert (1, "A");
      M.Insert (2, "B");

      Cursor_1 : Cursor := M.Find (1);

      T.Assert (Has_Element (Cursor_1));
      T.Assert (Key (Cursor_1) = 1);
      T.Assert (Element (Cursor_1) = "A");

      T.Assert (not Has_Element (M.Find (99)));
   end Test_Find;

   procedure Test_First_Last (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (5, "Five");
      M.Insert (3, "Three");
      M.Insert (7, "Seven");

      T.Assert (M.First_Key = 5);
      T.Assert (M.First_Element = "Five");
      T.Assert (M.Last_Key = 7);
      T.Assert (M.Last_Element = "Seven");
   end Test_First_Last;

   procedure Test_Foreign_Cursors (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      M : aliased Map;
      Other : Map;
      M.Insert (1, "A");
      Other.Insert (1, "B");
      Iter : Map_Iterator_Interfaces.Reversible_Iterator'Class := M.Iterate;
      for Action in 1 .. 5 loop
         begin
            case Action is
               when 1 =>
                  declare
                     R : constant Constant_Reference_Type := M.Constant_Reference (Other.First);
                  begin
                     T.Assert (R.Element.all = "A");
                     T.Fail ("Foreign constant reference accepted");
                  end;
               when 2 =>
                  declare
                     R : constant Reference_Type := M.Reference (Other.First);
                  begin
                     T.Assert (R.Element.all = "A");
                     T.Fail ("Foreign mutable reference accepted");
                  end;
               when 3 =>
                  declare
                     Foreign : Map_Iterator_Interfaces.Reversible_Iterator'Class := M.Iterate (Other.First);
                  begin
                     T.Assert (Foreign.First = M.First);
                     T.Fail ("Foreign iterator start accepted");
                  end;
               when 4 =>
                  T.Assert (Iter.Next (Other.First) = No_Element);
                  T.Fail ("Foreign Next accepted");
               when 5 =>
                  T.Assert (Iter.Previous (Other.First) = No_Element);
                  T.Fail ("Foreign Previous accepted");
            end case;
         exception
            when Program_Error => null;
         end;
      end loop;
      T.Assert (Iter.Next (No_Element) = No_Element);
      T.Assert (Iter.Previous (No_Element) = No_Element);
   end Test_Foreign_Cursors;

   procedure Test_Insert_And_Element (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      T.Assert (M.Length = 0);

      M.Insert (1, "Hello");
      T.Assert (M.Length = 1);
      T.Assert (M.Element (1) = "Hello");

      M.Insert (2, "World");
      T.Assert (M.Length = 2);
      T.Assert (M.Element (2) = "World");
   end Test_Insert_And_Element;

   procedure Test_Insertion_Order (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (3, "C");
      M.Insert (1, "A");
      M.Insert (2, "B");

      T.Assert (M.First_Key = 3, "First should be first inserted, not smallest key");

      Position : Cursor := M.First;

      T.Assert (Key (Position) = 3);
      Next (Position);
      T.Assert (Key (Position) = 1);
      Next (Position);
      T.Assert (Key (Position) = 2);
      Next (Position);
      T.Assert (not Has_Element (Position));
   end Test_Insertion_Order;

   procedure Test_Iterate (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (10, "Ten");
      M.Insert (20, "Twenty");
      M.Insert (30, "Thirty");

      Count : Ada.Containers.Count_Type := 0;

      for Position in M.Iterate loop
         Count := Count + 1;
      end loop;

      T.Assert (Count = 3);
   end Test_Iterate;

   procedure Test_Iterate_With_Start (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "A");
      M.Insert (2, "B");
      M.Insert (3, "C");

      Start : Cursor := Next (M.First);

      Count : Ada.Containers.Count_Type := 0;

      for Position in M.Iterate (Start) loop
         Count := Count + 1;
      end loop;

      T.Assert (Count = 2);
   end Test_Iterate_With_Start;

   procedure Test_Iterator_Tampering (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      M : Map;
      M.Insert (1, "A");
      M.Insert (2, "B");
      for Action in 1 .. 5 loop
         declare
            Iter : Map_Iterator_Interfaces.Reversible_Iterator'Class := M.Iterate;
         begin
            begin
               case Action is
                  when 1 => M.Insert (3, "C");
                  when 2 => M.Delete (1);
                  when 3 => M.Exclude (2);
                  when 4 => M.Include (3, "C");
                  when 5 => M.Reverse_Clear;
               end case;
               T.Fail ("Mutation during iteration must raise Program_Error");
            exception
               when Program_Error => null;
            end;
            T.Assert (M.Length = 2 and then M.Contains (1) and then M.Contains (2));
            T.Assert (not M.Contains (3));
            T.Assert (Element (Iter.First) = "A" and then Element (Iter.Last) = "B");
         end;
      end loop;
      M.Insert (3, "C");
      M.Delete (1);
      T.Assert (M.Length = 2);
      begin
         for Position in M.Iterate (M.First) loop
            raise Constraint_Error;
         end loop;
      exception
         when Constraint_Error => null;
      end;
      M.Reverse_Clear;
      T.Assert (M.Is_Empty);
   end Test_Iterator_Tampering;

   procedure Test_Length (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      T.Assert (M.Length = 0);

      M.Insert (1, "A");
      T.Assert (M.Length = 1);

      M.Insert (2, "B");
      T.Assert (M.Length = 2);

      M.Delete (1);
      T.Assert (M.Length = 1);
   end Test_Length;

   procedure Test_Next_Previous (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "A");
      M.Insert (2, "B");
      M.Insert (3, "C");

      Position : Cursor := M.First;

      T.Assert (Key (Position) = 1);
      Position := Next (Position);
      T.Assert (Key (Position) = 2);
      Position := Next (Position);
      T.Assert (Key (Position) = 3);
      Position := Next (Position);
      T.Assert (not Has_Element (Position));

      Position := M.Last;
      T.Assert (Key (Position) = 3);
      Position := Previous (Position);
      T.Assert (Key (Position) = 2);
      Position := Previous (Position);
      T.Assert (Key (Position) = 1);
      Position := Previous (Position);
      T.Assert (not Has_Element (Position));
   end Test_Next_Previous;

   procedure Test_Previous_Procedure (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "A");
      M.Insert (2, "B");

      Position : Cursor := M.Last;

      T.Assert (Key (Position) = 2);
      Previous (Position);
      T.Assert (Key (Position) = 1);
      Previous (Position);
      T.Assert (not Has_Element (Position));
   end Test_Previous_Procedure;

   procedure Test_Reference (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "Original");
      M (1) := "Modified";
      T.Assert (M.Element (1) = "Modified");
   end Test_Reference;

   procedure Test_Reference_Copy_And_Unwind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      M : aliased Map;
      M.Insert (1, "A");
      begin
         R : constant Constant_Reference_Type := M.Constant_Reference (1);
         Copy : Map := M;
         T.Assert (Copy = M, "Reference bookkeeping must not affect map equality");
         Copy.Include (1, "Independent copy");
         T.Assert (R.Element.all = "A");
         T.Assert (Copy.Element (1) = "Independent copy");
         declare
            Nested : constant Reference_Type := M.Reference (1);
         begin
            Nested.Element.all := "B";
         end;
         begin
            M.Include (1, "Still referenced");
            T.Fail ("The outer reference must still prevent replacement");
         exception
            when Program_Error => null;
         end;
         T.Assert (R.Element.all = "B");
         raise Constraint_Error;
      exception
         when Constraint_Error => null;
      end;
      M.Include (1, "Released after exception");
      T.Assert (M.Element (1) = "Released after exception");
      begin
         R : constant Reference_Type := M.Reference (99);
         T.Fail ("Missing reference should fail: " & R.Element.all);
      exception
         when Constraint_Error => null;
      end;
      M.Include (1, "No leaked guard");
      T.Assert (M.Element (1) = "No leaked guard");
   end Test_Reference_Copy_And_Unwind;

   procedure Test_Reference_Cursor (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "Original");

      Position : Cursor := M.First;

      M (Position) := "Modified";
      T.Assert (M.Element (1) = "Modified");
   end Test_Reference_Cursor;

   procedure Test_Reference_Lifetime (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      M : aliased Map;
      M.Insert (1, "A");
      for Mutable in Boolean loop
         declare
            procedure Try_Delete is
            begin
               begin
                  M.Delete (1);
                  T.Fail ("A live reference must prohibit deletion");
               exception
                  when Program_Error => null;
               end;
               begin
                  M.Include (1, "A replacement with different bounds");
                  T.Fail ("A live reference must prohibit replacement");
               exception
                  when Program_Error => null;
               end;
               T.Assert (M.Length = 1 and then M.Contains (1));
            end Try_Delete;
         begin
            if Mutable then
               declare
                  R : constant Reference_Type := M.Reference (1);
               begin
                  Try_Delete;
                  R.Element.all := "B";
                  T.Assert (M.Element (1) = "B");
               end;
            else
               declare
                  R : constant Constant_Reference_Type := M.Constant_Reference (1);
               begin
                  Try_Delete;
                  T.Assert (R.Element.all = "A");
               end;
            end if;
         end;
      end loop;
      M.Delete (1);
      T.Assert (M.Is_Empty);
   end Test_Reference_Lifetime;

   procedure Test_Reverse_Clear (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "A");
      M.Insert (2, "B");
      M.Insert (3, "C");

      M.Reverse_Clear;

      T.Assert (M.Length = 0);
      T.Assert (not Has_Element (M.First));
   end Test_Reverse_Clear;

   procedure Test_Reverse_Iterate (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (10, "Ten");
      M.Insert (20, "Twenty");
      M.Insert (30, "Thirty");

      Count : Ada.Containers.Count_Type := 0;
      Last_Key : Integer := Integer'Last;

      for Position in reverse M.Iterate loop
         T.Assert (Key (Position) < Last_Key);
         Last_Key := Key (Position);
         Count := Count + 1;
      end loop;

      T.Assert (Count = 3);
   end Test_Reverse_Iterate;

   procedure Test_Reverse_Iterate_Empty (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      for Position in reverse M.Iterate loop
         T.Fail ("Should be empty.");
      end loop;
   end Test_Reverse_Iterate_Empty;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Iterator_Tampering'Access,
         Test_Foreign_Cursors'Access,
         Test_Reference_Lifetime'Access,
         Test_Reference_Copy_And_Unwind'Access,
         Test_Concatenation'Access,
         Test_Constant_Reference'Access,
         Test_Constant_Reference_Cursor'Access,
         Test_Contains'Access,
         Test_Delete'Access,
         Test_Empty'Access,
         Test_Exclude_Missing'Access,
         Test_Find'Access,
         Test_First_Last'Access,
         Test_Insert_And_Element'Access,
         Test_Insertion_Order'Access,
         Test_Iterate'Access,
         Test_Iterate_With_Start'Access,
         Test_Length'Access,
         Test_Next_Previous'Access,
         Test_Previous_Procedure'Access,
         Test_Reference'Access,
         Test_Reference_Cursor'Access,
         Test_Reverse_Clear'Access,
         Test_Reverse_Iterate'Access,
         Test_Reverse_Iterate_Empty'Access];
   end All_Tests;

end Prunt.Indefinite_Ordered_Maps_With_Insertion_Order_Test;
