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

   procedure Test_Reference_Cursor (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      M : Map;

      M.Insert (1, "Original");

      Position : Cursor := M.First;

      M (Position) := "Modified";
      T.Assert (M.Element (1) = "Modified");
   end Test_Reference_Cursor;

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

      Count : Ada.Containers.Count_Type := 0;

      for Position in reverse M.Iterate loop
         Count := Count + 1;
      end loop;

      T.Assert (Count = 0);
   end Test_Reverse_Iterate_Empty;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Concatenation'Access,
         Test_Constant_Reference'Access,
         Test_Constant_Reference_Cursor'Access,
         Test_Contains'Access,
         Test_Delete'Access,
         Test_Empty'Access,
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
