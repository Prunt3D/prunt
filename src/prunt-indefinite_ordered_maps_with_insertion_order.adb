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

package body Prunt.Indefinite_Ordered_Maps_With_Insertion_Order is

   pragma Extensions_Allowed (On);

   use type Key_Vectors.Cursor;

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
      return Inner_Maps.Element (Position.Map.Map, Key_Vectors.Element (Position.Cursor));
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

   function Constant_Reference (Container : aliased Map; Position : Cursor) return Constant_Reference_Type is
   begin
      return Constant_Reference (Container, Key (Position));
   end Constant_Reference;

   function Reference (Container : aliased in out Map; Position : Cursor) return Reference_Type is
   begin
      return Reference (Container, Key (Position));
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

   function Length (Container : Map) return Ada.Containers.Count_Type is
   begin
      return Container.Insertions.Length;
   end Length;

   procedure Delete (Container : in out Map; Key : Key_Type) is
   begin
      Container.Map.Delete (Key);
      Container.Insertions.Delete (Container.Insertions.Find_Index (Key));
   end Delete;

   procedure Reverse_Clear (Container : in out Map) is
   begin
      while not Container.Insertions.Is_Empty loop
         Container.Map.Delete (Container.Insertions.Last_Element);
         Container.Insertions.Delete_Last;
      end loop;
   end Reverse_Clear;

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

   procedure Include (Container : in out Map; Key : Key_Type; New_Item : Element_Type) is
   begin
      if Container.Find (Key) = No_Element then
         Container.Insert (Key, New_Item);
      else
         Container.Map.Include (Key, New_Item);
      end if;
   end Include;

   procedure Exclude (Container : in out Map; Key : Key_Type) is
   begin
      if Container.Find (Key) /= No_Element then
         Container.Delete (Key);
      end if;
   end Exclude;

   function Is_Empty (Container : Map) return Boolean is
   begin
      return Container.Map.Is_Empty;
   end Is_Empty;

   function "&" (Left, Right : Map) return Map is
   begin
      return Result : Map do
         for I of Left.Insertions loop
            Result.Map.Insert (I, Left.Map.Element (I));
            Result.Insertions.Append (I);
         end loop;

         for I of Right.Insertions loop
            Result.Map.Insert (I, Right.Map.Element (I));
            Result.Insertions.Append (I);
         end loop;
      end return;
   end "&";

end Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;
