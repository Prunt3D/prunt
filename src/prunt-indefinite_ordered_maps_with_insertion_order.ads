with Ada.Iterator_Interfaces;
private with Ada.Containers;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Finalization;

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
   pragma Ada_2022 (Empty);

   No_Element : constant Cursor;
   function Has_Element (Position : Cursor) return Boolean;

   package Map_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Key (Position : Cursor) return Key_Type;

   function Element (Position : Cursor) return Element_Type;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is private
   with Implicit_Dereference => Element;

   type Reference_Type (Element : not null access Element_Type) is private with Implicit_Dereference => Element;

   function Constant_Reference (Container : aliased Map; Key : Key_Type) return Constant_Reference_Type;

   function Reference (Container : aliased in out Map; Key : Key_Type) return Reference_Type;

   procedure Insert (Container : in out Map; Key : Key_Type; New_Item : Element_Type);

   function First (Container : Map) return Cursor;

   function First_Element (Container : Map) return Element_Type;

   function First_Key (Container : Map) return Key_Type;

   function Last (Container : Map) return Cursor;

   function Last_Element (Container : Map) return Element_Type;

   function Last_Key (Container : Map) return Key_Type;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;

   procedure Previous (Position : in out Cursor);

   function Find (Container : Map; Key : Key_Type) return Cursor;

   function Element (Container : Map; Key : Key_Type) return Element_Type;

   function Contains (Container : Map; Key : Key_Type) return Boolean;

   function Iterate (Container : Map) return Map_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate (Container : Map; Start : Cursor) return Map_Iterator_Interfaces.Reversible_Iterator'Class;

   function "+" (Left, Right : Map) return Map;

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

   type Iterator is new Ada.Finalization.Limited_Controlled and Map_Iterator_Interfaces.Reversible_Iterator with record
      Iterator : Key_Vectors_Iterator_Access;
      Map      : Map_Access;
   end record;

   overriding
   procedure Finalize (Object : in out Iterator);

   overriding
   function First (Object : Iterator) return Cursor;

   overriding
   function Last (Object : Iterator) return Cursor;

   overriding
   function Next (Object : Iterator; Position : Cursor) return Cursor;

   overriding
   function Previous (Object : Iterator; Position : Cursor) return Cursor;

end Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;
