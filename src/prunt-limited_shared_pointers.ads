------------------------------------------------------------------------------
--                                                                          --
--  This file is based on code from GNATCOLL. The below terms below apply   --
--  only to this file. Refer to COPYING in the root directory for the terms --
--  for other parts of Prunt.                                               --
--                                                                          --
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
--                     Copyright (C) 2025, Liam Powell                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a version of `GNATCOLL.Refcount.Shared_Pointers` which works with limited types.

with GNATCOLL.Atomic;
with GNATCOLL.Storage_Pools.Headers; use GNATCOLL.Storage_Pools.Headers;
with GNATCOLL;                       use GNATCOLL;
with System;

private with Ada.Finalization;

generic
   type Element_Type (<>) is limited private;
   --  The element that will be encapsulated within a smart pointer.

   with procedure Release (Self : in out Element_Type) is null;
   --  This procedure should be used if you need to perform actions when the last reference to an element is removed.
   --  Typically, this is used to free element_type and its contents, when it is not a controlled type.

   Atomic_Counters : Boolean := True;
   --  Whether to use atomic (and thus thread-safe) counters. If set to True, the smart pointer is task safe. Of
   --  course, that does not mean that the Element_Type itself is task safe. This has a small impact on performance.

package Prunt.Limited_Shared_Pointers
is

   -------------------
   -- Internal data --
   -------------------
   --  This section provides several types that are used in the implementation of this package. They are not useful for
   --  applications.

   type Weak_Data is record
      Element  : System.Address := System.Null_Address;
      Refcount : aliased Atomic.Atomic_Counter;
      Lock     : aliased Atomic.Atomic_Counter;
      --  To resolve the race condition between the last Ref disappearing and the new Ref creation from Weak_Ref.
   end record;
   type Weak_Data_Access is access all Weak_Data;

   type Counters is record
      Refcount  : aliased Atomic.Atomic_Counter := 1;
      Weak_Data : aliased Weak_Data_Access;
      --  A pointer to the weak pointer's data. This data is created the first time we create a weak pointer. We hold a
      --  reference to that data, so that it can never be freed while at least one reference exists.
   end record;

   type Counters_Access is access all Counters;
   pragma No_Strict_Aliasing (Counters_Access);

   package Headers is new Header_Pools (Counters, Counters_Access);

   pragma Suppress (All_Checks);

   Is_Task_Safe : constant Boolean := Atomic_Counters;
   --  Make the formal parameter visible to users of this package

   type Ref is tagged private;
   Null_Ref : constant Ref;
   --  This type acts like a pointer, but holds a reference to the object, which will thus never be freed while there
   --  exists at least one reference to it.

   type Weak_Ref is tagged private;
   Null_Weak_Ref : constant Weak_Ref;
   --  A weak reference to an object. The value returned by Get will be reset to null when the object is freed (because
   --  its last reference expired). Holding a weak reference does not prevent the deallocation of the object.

   package Pools is new Headers.Typed (Element_Type);
   subtype Element_Access is Pools.Element_Access;

   procedure Set (Self : in out Ref'Class; Get_Data : access function return Element_Type);
   pragma Inline (Set);
   --  The return value of Get_Data will be put under control of Self, and freed when the last reference to it is
   --  removed.

   procedure From_Element (Self : out Ref'Class; Element : Element_Access);
   pragma Inline (From_Element);
   --  Given an element that is already under control of a shared pointer, returns the corresponding shared pointer.
   --  This is especially useful when the element_type is a tagged type. This element might be used for dynamic
   --  dispatching, but it might be necessary to retrieve the smart pointer:
   --
   --      type Object is tagged private;
   --      package Pointers is new Shared_Pointers (Object'Class);
   --      use Pointers;
   --
   --      procedure Method (Self : Object'Class) is
   --         R : Ref;
   --      begin
   --         From_Element (R, Self);
   --      end Method;
   --
   --      R : Ref;
   --      R.Set (Get_Obj);
   --      Method (R.Get);
   --
   --  Warning: this must only be called when Element comes from a
   --  shared pointer, otherwise an invalid memory access will result.

   type Reference_Type (Element : access Element_Type) is limited null record with Implicit_Dereference => Element;
   --  A reference to an element_type.
   --  This type is used as the return value for Get, instead of an Element_Access, because it is safer:
   --     * applications cannot free the returned value (and they should never do it !)
   --     * the Element discriminant cannot be stored in a variable, so that prevents keeping a reference when it could
   --       be freed at any time.
   --     * since the type is limited, it is in general difficult to store it in records. This is intended, since the
   --       shared pointer itself should be stored instead (at the access type might be freed at any time).
   --  This type is often mostly transparent for the application. Assuming the Element_Type is defined as:
   --
   --       type Element_Type is tagged record
   --          Field : Integer;
   --       end record;
   --       procedure Primitive (Self : Element_Type);
   --       procedure Primitive2 (Self : access Element_Type);
   --
   --  then a shared pointer SP can be used as:
   --
   --       SP.Get.Field := 1;
   --       SP.Get.Primitive1;
   --       SP.Get.Element.Primitive2;
   --
   --  WARNING:
   --  The use of a reference_type ensures that Get can return an access to the object (more efficient than a copy when
   --  the objects are large), while preventing users from freeing the returned value. But this does not prevent all
   --  invalid cases. Using 'renames', for instance, can lead to invalid code, as in:
   --
   --     package IP is new Shared_Pointers (Integer);
   --     use IP;
   --     R : Ref;
   --     R.Set (99);
   --     declare
   --        Int : Integer renames R.Get.Element.all;
   --     begin
   --        R := Null_Ref;     --  Frees Int !
   --        Put_Line (I'Img);  --  Invalid memory access
   --     end;
   --
   --  Another dangerous use is to have a procedure that receives the result of Get and modifies the shared pointer, as
   --  in:
   --
   --     package OP is new Shared_Pointers (Object'Class);
   --     use OP;
   --     R : Ref;
   --     procedure Foo (Obj : Object'Class) is
   --     begin
   --        R := Null_Ref;   --  freezes Obj !
   --     end Foo;
   --     Foo (R.Get);
   --
   --  The proper solution here is that Foo should receive the smart pointer itself, not the encapsulated value.

   function Unchecked_Get (Self : Ref'Class) return Element_Access
   with Inline;
   --  A version that returns directly the element access.
   --
   --  This is meant for easy conversion of existing code, but its use is discouraged in new code, where Get should be
   --  used instead.
   --
   --  The resulting access must not be deallocated. Passing it to Set might also be dangerous if the Element_Type
   --  contains data that might be freed when other smart pointers are freed.  It also must not be stored in a record
   --  (store Self instead).

   function Get (Self : Ref'Class) return Reference_Type
   is ((Element => Unchecked_Get (Self)))
   with Inline;
   --  A safer version of Unchecked_Get.
   --
   --  There is no performance penalty, since the compiler knows that a Reference_Type is in fact always of the same
   --  size and can be returned on the stack.
   --
   --  It is safer because the associated access type cannot be converted to a non-local access type, nor freed.

   procedure Process (Self : Ref'Class; Process : not null access procedure (E : Element_Type))
   with Inline;
   --  This procedure is similar to the function Get, but doesn't expose the access type to the user.
   --
   --  This is safer than Get, since it avoids the multiple issues highlighted in the comments for Reference_Type
   --  (namely that Self might become null while the application holds a reference, which then references invalid
   --  memory).
   --
   --  On the other hand, it is more awkward to use, and does not work if you need to pass multiple smart pointers.
   --  There is however nothing tricky in this procedure, since it simply calls
   --      Process (Self.Get)
   --  and the simple fact that Self is a parameter ensures it retains at least one reference during the execution of
   --  Process.
   --
   --  If you want to always be on the safe side and prevent users from using Get, you could add the following
   --  configuration pragma to your compilation:
   --     pragma Restrictions (No_Use_Of_Entity => Prunt.Limited_Shared_Pointers.Get);

   function Is_Null (Self : Ref'Class) return Boolean
   with Inline;
   --  Whether the data is unset. Using this function might avoid the need for a "use type Element_Access" in your
   --  code.

   overriding
   function "=" (P1, P2 : Ref) return Boolean
   with Inline;
   --  This operator checks whether P1 and P2 share the same pointer. When the pointers differ, this operator returns
   --  False even if the two pointed elements are equal.

   function Weak (Self : Ref'Class) return Weak_Ref;
   procedure Set (Self : in out Ref'Class; Weak : Weak_Ref'Class);
   --  Set returns a reference to the object. Otherwise, it would be possible for a procedure to retrieve a pointer
   --  from the weak reference, and then reference it throughout the procedure, even though the pointer might be freed
   --  in between.
   --
   --  If Weak is Null_Weak_Ref, then the element pointed by Self simply loses a reference, and Self points to nothing
   --  on exit.

   function Was_Freed (Self : Weak_Ref'Class) return Boolean;
   --  True if the object referenced by Self was freed.

   function Get_Refcount (Self : Ref'Class) return Natural;
   --  Return the current reference count.
   --  This is mostly intended for debug purposes.

private

   type Ref is new Ada.Finalization.Controlled with record
      Data : Element_Access;
   end record;
   pragma Finalize_Storage_Only (Ref);
   overriding
   procedure Adjust (Self : in out Ref);
   pragma Inline (Adjust);
   overriding
   procedure Finalize (Self : in out Ref);

   type Weak_Ref is new Ada.Finalization.Controlled with record
      Data : Weak_Data_Access;
   end record;
   pragma Finalize_Storage_Only (Weak_Ref);
   overriding
   procedure Adjust (Self : in out Weak_Ref);
   pragma Inline (Adjust);
   overriding
   procedure Finalize (Self : in out Weak_Ref);

   Null_Ref      : constant Ref := (Ada.Finalization.Controlled with Data => null);
   Null_Weak_Ref : constant Weak_Ref := (Ada.Finalization.Controlled with Data => null);

end Prunt.Limited_Shared_Pointers;
