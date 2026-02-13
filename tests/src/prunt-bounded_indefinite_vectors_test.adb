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

with Trendy_Test;    use Trendy_Test;
with System.Storage_Elements;
with Prunt.Bounded_Indefinite_Vectors;
with Ada.Containers; use Ada.Containers;
with Ada.Finalization;

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
      overriding procedure Finalize (Object : in out Test_Item);
   end Finalization_Checks;

   package body Finalization_Checks is
      overriding procedure Finalize (Object : in out Test_Item) is
      begin
         if Object.Finalized_Counter /= null then
            Object.Finalized_Counter.all := @ + 1;
         end if;
      end Finalize;
   end Finalization_Checks;

   package Finalization_Vectors is new Prunt.Bounded_Indefinite_Vectors
     (Element_Type => Finalization_Checks.Test_Item,
      Index_Type   => Test_Index,
      Storage_Size => 512);

   procedure Test_Alignment (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         type High_Align is record
            Val : Integer;
         end record
         with Alignment => 16;

         package Align_Vectors is new
           Prunt.Bounded_Indefinite_Vectors
             (Element_Type => High_Align,
              Index_Type   => Test_Index,
              Storage_Size => 128);

         V    : Align_Vectors.Vector;
         Item : High_Align := (Val => 123);
      begin
         V.Append (Item);
         T.Assert (V.Last_Index = 1);
         T.Assert (V.Element (1).Val = 123);
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

      V.Append ("World");
      T.Assert (V.Last_Index = 2);
      T.Assert (V.Element (2) = "World");
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
         when Constraint_Error =>
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

   procedure Test_Finalization_On_Clear (T : in out Trendy_Test.Operation'Class) is
      use Finalization_Checks;

      V       : Finalization_Vectors.Vector;
      Counter : Counter_Access := new Integer'(0);
   begin
      T.Register;

      declare
         Item : Test_Item := (Ada.Finalization.Controlled with Finalized_Counter => Counter);
      begin
         V.Append (Item);
         V.Append (Item);
      end;
      T.Assert (Counter.all = 1, "Temporary item should be finalized");

      V.Clear;
      T.Assert (Counter.all = 3, "Stored items should be finalized on Clear");
   end Test_Finalization_On_Clear;

   procedure Test_Finalization_On_Scope_Exit (T : in out Trendy_Test.Operation'Class) is
      use Finalization_Checks;

      Counter : Counter_Access := new Integer'(0);
   begin
      T.Register;

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
   end Test_Finalization_On_Scope_Exit;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Alignment'Access,
         Test_Append_And_Read'Access,
         Test_Capacity_Exceeded'Access,
         Test_Clear'Access,
         Test_Process_Range'Access,
         Test_Storage_Exhausted'Access,
         Test_Finalization_On_Clear'Access,
         Test_Finalization_On_Scope_Exit'Access];
   end All_Tests;

end Prunt.Bounded_Indefinite_Vectors_Test;
