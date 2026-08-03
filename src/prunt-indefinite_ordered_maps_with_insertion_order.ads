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

pragma Extensions_Allowed (On);

with Ada.Containers;
with Ada.Iterator_Interfaces;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;

generic
   type Key_Type (<>) is private;
   type Element_Type (<>) is private;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

package Prunt.Indefinite_Ordered_Maps_With_Insertion_Order
is

   --  Not all methods from Indefinite_Ordered_Maps are exposed here, but they can be added in the future. If doing so,
   --  note that the current implementation does not support removal of elements.

   type Map is tagged private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Element_Type,
     Aggregate         => (Empty => Empty, Add_Named => Insert);

   type Cursor is private;

   Empty_Map : constant Map;

   function Empty return Map;
   --  Return a map containing no elements.

   pragma Ada_2022 (Empty);

   No_Element : constant Cursor;
   function Has_Element (Position : Cursor) return Boolean;
   --  Return True when Position designates an element.

   package Map_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Key (Position : Cursor) return Key_Type;
   --  Return the key designated by Position.

   function Element (Position : Cursor) return Element_Type;
   --  Return the element designated by Position.

   type Constant_Reference_Type (Element : not null access constant Element_Type) is limited private
   with Implicit_Dereference => Element;
   --  This is a limited type for compatibility with GCC 16, which will implement AI22-0082.

   type Reference_Type (Element : not null access Element_Type) is limited private
   with Implicit_Dereference => Element;
   --  This is a limited type for compatibility with GCC 16, which will implement AI22-0082.

   function Constant_Reference (Container : aliased Map; Key : Key_Type) return Constant_Reference_Type;
   --  Return a read-only reference to the element associated with Key.

   function Reference (Container : aliased in out Map; Key : Key_Type) return Reference_Type;
   --  Return a mutable reference to the element associated with Key.

   function Constant_Reference (Container : aliased Map; Position : Cursor) return Constant_Reference_Type;
   --  Return a read-only reference to the element designated by Position.

   function Reference (Container : aliased in out Map; Position : Cursor) return Reference_Type;
   --  Return a mutable reference to the element designated by Position.

   procedure Insert (Container : in out Map; Key : Key_Type; New_Item : Element_Type);
   --  Insert New_Item under a previously absent Key and record it as the most recently inserted element.

   function First (Container : Map) return Cursor;
   --  Return the cursor for the earliest inserted element, or No_Element when Container is empty.

   function First_Element (Container : Map) return Element_Type;
   --  Return the earliest inserted element.

   function First_Key (Container : Map) return Key_Type;
   --  Return the key of the earliest inserted element.

   function Last (Container : Map) return Cursor;
   --  Return the cursor for the most recently inserted element, or No_Element when Container is empty.

   function Last_Element (Container : Map) return Element_Type;
   --  Return the most recently inserted element.

   function Last_Key (Container : Map) return Key_Type;
   --  Return the key of the most recently inserted element.

   function Next (Position : Cursor) return Cursor;
   --  Return the next cursor in insertion order, or No_Element after the last element.

   procedure Next (Position : in out Cursor);
   --  Advance Position to the next cursor in insertion order.

   function Previous (Position : Cursor) return Cursor;
   --  Return the previous cursor in insertion order, or No_Element before the first element.

   procedure Previous (Position : in out Cursor);
   --  Move Position to the previous cursor in insertion order.

   function Find (Container : Map; Key : Key_Type) return Cursor;
   --  Return the cursor associated with Key, or No_Element when Key is absent.

   function Element (Container : Map; Key : Key_Type) return Element_Type;
   --  Return the element associated with Key.

   function Contains (Container : Map; Key : Key_Type) return Boolean;
   --  Return True when Container contains Key.

   function Iterate (Container : Map) return Map_Iterator_Interfaces.Reversible_Iterator'Class;
   --  Return a reversible iterator over all elements in insertion order.

   function Iterate (Container : Map; Start : Cursor) return Map_Iterator_Interfaces.Reversible_Iterator'Class;
   --  Return a reversible insertion-order iterator whose forward traversal begins at Start.

   function Length (Container : Map) return Ada.Containers.Count_Type;
   --  Return the number of elements in Container.

   procedure Delete (Container : in out Map; Key : Key_Type);
   --  Delete Key and its position in the insertion-order sequence.

   procedure Include (Container : in out Map; Key : Key_Type; New_Item : Element_Type);
   --  Replace the element at Key, or append a new key and element when Key is absent.

   procedure Exclude (Container : in out Map; Key : Key_Type);
   --  Delete Key when present and otherwise leave Container unchanged.

   function Is_Empty (Container : Map) return Boolean;
   --  Return True when Container contains no elements.

   procedure Reverse_Clear (Container : in out Map);
   --  Delete all elements starting with the most recently inserted.

   function "&" (Left, Right : Map) return Map;
   --  Concatenate two disjoint maps while preserving each operand's insertion order.

private

   package Inner_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Key_Type => Key_Type, Element_Type => Element_Type);
   package Key_Vectors is new Ada.Containers.Indefinite_Vectors (Ada.Containers.Count_Type, Key_Type);

   type Map_Access is access all Map;

   type Map is tagged record
      Map        : Inner_Maps.Map;
      Insertions : Key_Vectors.Vector;
   end record;

   type Cursor is record
      Cursor : Key_Vectors.Cursor;
      Map    : Map_Access;
   end record;

   Empty_Map : constant Map := (Map => Inner_Maps.Empty_Map, Insertions => Key_Vectors.Empty_Vector);

   No_Element : constant Cursor := (Key_Vectors.No_Element, null);

   type Constant_Reference_Type (Element : not null access constant Element_Type) is record
      Inner : Inner_Maps.Constant_Reference_Type (Element);
   end record;

   type Reference_Type (Element : not null access Element_Type) is record
      Inner : Inner_Maps.Reference_Type (Element);
   end record;

   type Key_Vectors_Iterator_Access is access all Key_Vectors.Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Key_Vectors.Vector_Iterator_Interfaces.Reversible_Iterator'Class,
        Key_Vectors_Iterator_Access);

   type Iterator is new Ada.Finalization.Limited_Controlled and Map_Iterator_Interfaces.Reversible_Iterator with record
      Iterator : Key_Vectors_Iterator_Access;
      Map      : Map_Access;
   end record;

   overriding
   procedure Finalize (Object : in out Iterator);
   --  Release the heap-allocated underlying insertion-order iterator.

   overriding
   function First (Object : Iterator) return Cursor;
   --  Return Object's first cursor, or No_Element when its iteration range is empty.

   overriding
   function Last (Object : Iterator) return Cursor;
   --  Return Object's last cursor, or No_Element when its iteration range is empty.

   overriding
   function Next (Object : Iterator; Position : Cursor) return Cursor;
   --  Return the cursor following Position within Object's insertion-order traversal.

   overriding
   function Previous (Object : Iterator; Position : Cursor) return Cursor;
   --  Return the cursor preceding Position within Object's insertion-order traversal.

end Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;
