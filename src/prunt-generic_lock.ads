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

--  This package defines a lock holder type that automatically acquires a lock upon creation and releases it upon
--  finalization. This ensures that locks are always released, even in the presence of exceptions. A single lock is
--  available for each instantiation of the generic package.

pragma Extensions_Allowed (On);

private with Ada.Finalization;

generic
package Prunt.Generic_Lock is

   type Lock_Holder (<>) is limited private;
   --  A type representing the ownership of the lock.
   --
   --  The lock is held for as long as an object of this type exists. When the object is finalized (e.g., goes out of
   --  scope), the lock is automatically released.

   function Lock return Lock_Holder;
   --  Acquires the lock and returns a `Lock_Holder` object.
   --
   --  This function blocks until the lock becomes available. The lock will remain held until the returned object is
   --  finalized.

   --  with Nonblocking => False;
   --  TODO: Uncomment above once GNAT adds support for the Nonblocking aspect.

private

   protected Lock_Manager is
      entry Lock;
      procedure Unlock (Holder : in out Lock_Holder);
   private
      Locked : Boolean := False;
   end Lock_Manager;

   type Atomic_Boolean is new Boolean with Atomic, Volatile;

   type Lock_Holder is new Ada.Finalization.Limited_Controlled with record
      Already_Finalized : Atomic_Boolean := False;
   end record;

   overriding
   procedure Finalize (Object : in out Lock_Holder);

end Prunt.Generic_Lock;
