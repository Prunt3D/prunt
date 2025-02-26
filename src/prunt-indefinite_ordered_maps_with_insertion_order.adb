with Ada.Unchecked_Deallocation;

package body Prunt.Indefinite_Ordered_Maps_With_Insertion_Order is

   use type Key_Vectors.Cursor;

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Key_Vectors.Vector_Iterator_Interfaces.Reversible_Iterator'Class,
        Key_Vectors_Iterator_Access);

   function Empty return Map is
   begin
      return Empty_Map;
   end Empty;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Key_Vectors.Has_Element (Position.Cursor);
   end Has_Element;

   function Key (Position : Cursor) return Key_Type is
   begin
      return Key_Vectors.Element (Position.Cursor);
   end Key;

   function Element (Position : Cursor) return Element_Type is
   begin
      return Inner_Maps.Element (Position.Map.Map, Key (Position));
   end Element;

   function Constant_Reference (Container : aliased Map; Key : Key_Type) return Constant_Reference_Type is
   begin
      --  TODO: Is this correct?
      return
        (Container.Map.Constant_Reference (Key).Element.all'Unchecked_Access, Container.Map.Constant_Reference (Key));
   end Constant_Reference;

   function Reference (Container : aliased in out Map; Key : Key_Type) return Reference_Type is
   begin
      --  TODO: Is this correct?
      return (Container.Map.Reference (Key).Element.all'Unchecked_Access, Container.Map.Reference (Key));
   end Reference;

   procedure Insert (Container : in out Map; Key : Key_Type; New_Item : Element_Type) is
   begin
      Container.Map.Insert (Key, New_Item);
      Container.Insertions.Append (Key);
   end Insert;

   function First (Container : Map) return Cursor is
   begin
      if Container.Insertions.First = Key_Vectors.No_Element then
         return No_Element;
      else
         return (Container.Insertions.First, Container'Unrestricted_Access);
      end if;
   end First;

   function First_Element (Container : Map) return Element_Type is
   begin
      return Container.Element (Container.First_Key);
   end First_Element;

   function First_Key (Container : Map) return Key_Type is
   begin
      return Container.Insertions.First_Element;
   end First_Key;

   function Last (Container : Map) return Cursor is
   begin
      if Container.Insertions.Last = Key_Vectors.No_Element then
         return No_Element;
      else
         return (Container.Insertions.Last, Container'Unrestricted_Access);
      end if;
   end Last;

   function Last_Element (Container : Map) return Element_Type is
   begin
      return Container.Element (Container.Last_Key);
   end Last_Element;

   function Last_Key (Container : Map) return Key_Type is
   begin
      return Container.Insertions.Last_Element;
   end Last_Key;

   function Next (Position : Cursor) return Cursor is
   begin
      if Key_Vectors.Next (Position.Cursor) = Key_Vectors.No_Element then
         return No_Element;
      else
         return (Key_Vectors.Next (Position.Cursor), Position.Map);
      end if;
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   function Previous (Position : Cursor) return Cursor is
   begin
      if Key_Vectors.Previous (Position.Cursor) = Key_Vectors.No_Element then
         return No_Element;
      else
         return (Key_Vectors.Previous (Position.Cursor), Position.Map);
      end if;
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Inner_Cursor : constant Key_Vectors.Cursor := Container.Insertions.Find (Key);
   begin
      if Inner_Cursor = Key_Vectors.No_Element then
         return No_Element;
      else
         return (Inner_Cursor, Container'Unrestricted_Access);
      end if;
   end Find;

   function Element (Container : Map; Key : Key_Type) return Element_Type is
   begin
      return Container.Map.Element (Key);
   end Element;

   function Contains (Container : Map; Key : Key_Type) return Boolean is
   begin
      return Container.Map.Contains (Key);
   end Contains;

   function Iterate (Container : Map) return Map_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return
        Iterator'
          (Ada.Finalization.Limited_Controlled
           with
             Iterator =>
               new Key_Vectors.Vector_Iterator_Interfaces.Reversible_Iterator'Class'(Container.Insertions.Iterate),
             Map      => Container'Unrestricted_Access);
   end Iterate;

   function Iterate (Container : Map; Start : Cursor) return Map_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return
        Iterator'
          (Ada.Finalization.Limited_Controlled
           with
             Iterator =>
               new Key_Vectors.Vector_Iterator_Interfaces.Reversible_Iterator'Class'
                 (Container.Insertions.Iterate (Start.Cursor)),
             Map      => Container'Unrestricted_Access);
   end Iterate;

   overriding
   procedure Finalize (Object : in out Iterator) is
   begin
      Free (Object.Iterator);
   end Finalize;

   overriding
   function First (Object : Iterator) return Cursor is
      Inner_Cursor : constant Key_Vectors.Cursor := Object.Iterator.First;
   begin
      if Inner_Cursor = Key_Vectors.No_Element then
         return No_Element;
      else
         return Object.Map.Find (Key_Vectors.Element (Inner_Cursor));
      end if;
   end First;

   overriding
   function Last (Object : Iterator) return Cursor is
      Inner_Cursor : constant Key_Vectors.Cursor := Object.Iterator.Last;
   begin
      if Inner_Cursor = Key_Vectors.No_Element then
         return No_Element;
      else
         return Object.Map.Find (Key_Vectors.Element (Inner_Cursor));
      end if;
   end Last;

   overriding
   function Next (Object : Iterator; Position : Cursor) return Cursor is
   begin
      --  TODO: We should have tampering checks here.
      return Next (Position);
   end Next;

   overriding
   function Previous (Object : Iterator; Position : Cursor) return Cursor is
   begin
      --  TODO: We should have tampering checks here.
      return Previous (Position);
   end Previous;

end Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;
