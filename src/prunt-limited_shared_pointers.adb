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

with Ada.Finalization; use Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Atomic;  use GNATCOLL.Atomic;
with System;           use System;

package body Prunt.Limited_Shared_Pointers is

   pragma Extensions_Allowed (On);

   function Inc_Ref (R : access Counters; Atomic : Boolean) return Atomic_Counter
   with Inline;
   --  Increase the refcount and return the new value

   function Inc_Ref (R : access Counters; Atomic : Boolean) return Boolean;
   --  Increase the refcount only if it was non-zero, returns True if the
   --  increment has occurred.

   procedure Inc_Ref (R : access Weak_Data; Atomic : Boolean)
   with Inline;
   --  Increase the refcount

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Weak_Data, Weak_Data_Access);

   procedure Finalize (Data : in out Weak_Data_Access; Atomic : Boolean);
   --  Decrease refcount and free memory if needed

   function Sync_Bool_Compare_And_Swap is new Atomic.Sync_Bool_Compare_And_Swap (Weak_Data, Weak_Data_Access);

   -------------
   -- Inc_Ref --
   -------------

   function Inc_Ref (R : access Counters; Atomic : Boolean) return Boolean is
      Tmp : Atomic_Counter;
      Tm2 : Atomic_Counter;
   begin
      if Atomic then
         Tmp := R.Refcount;
         if Tmp = 0 then
            return False;
         end if;

         loop
            Tm2 := Sync_Val_Compare_And_Swap_Counter (R.Refcount'Access, Tmp, Atomic_Counter'Succ (Tmp));
            if Tm2 = Tmp then
               return True;
            elsif Tm2 = 0 then
               return False;
            else
               Tmp := Tm2;
            end if;
         end loop;

      else
         if R.Refcount = 0 then
            return False;
         end if;

         R.Refcount := Atomic_Counter'Succ (R.Refcount);
         return True;
      end if;
   end Inc_Ref;

   function Inc_Ref (R : access Counters; Atomic : Boolean) return Atomic_Counter is
   begin
      if Atomic then
         return Sync_Add_And_Fetch (R.Refcount'Access, 1);
      else
         Unsafe_Increment (R.Refcount);
         return R.Refcount;
      end if;
   end Inc_Ref;

   procedure Inc_Ref (R : access Weak_Data; Atomic : Boolean) is
   begin
      if Atomic then
         Increment (R.Refcount);
      else
         Unsafe_Increment (R.Refcount);
      end if;
   end Inc_Ref;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Data : in out Weak_Data_Access; Atomic : Boolean) is
   begin
      if Atomic then
         if Decrement (Data.Refcount) then
            Unchecked_Free (Data);
         end if;
      else
         if Unsafe_Decrement (Data.Refcount) then
            Unchecked_Free (Data);
         end if;
      end if;
   end Finalize;

   use type Pools.Element_Access;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation (Element_Type, Pools.Element_Access);

   pragma Warnings (Off, "*possible aliasing problem*");
   function Convert is new Ada.Unchecked_Conversion (Pools.Element_Access, System.Address);
   function Convert is new Ada.Unchecked_Conversion (System.Address, Pools.Element_Access);
   pragma Warnings (On, "*possible aliasing problem*");

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Ref'Class; Get_Data : access function return Element_Type) is
      R : access Counters;
   begin
      Finalize (Self);
      Self.Data := new Element_Type'(Get_Data.all);  --  uses storage pool
      R := Pools.Header_Of (Self.Data);
      R.Refcount := 1;
      R.Weak_Data := null;
   end Set;

   -------------------
   -- Unchecked_Get --
   -------------------

   function Unchecked_Get (Self : Ref'Class) return Element_Access is
   begin
      return Self.Data;
   end Unchecked_Get;

   -------------
   -- Process --
   -------------

   procedure Process (Self : Ref'Class; Process : not null access procedure (E : Element_Type)) is
   begin
      Process (Self.Data.all);
   end Process;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Ref'Class) return Boolean is
   begin
      return Self.Data = null;
   end Is_Null;

   ----------
   -- Weak --
   ----------

   function Weak (Self : Ref'Class) return Weak_Ref is
      R : Counters_Access;
      V : Weak_Data_Access;
   begin
      if Self.Data = null then
         return Null_Weak_Ref;
      end if;

      R := Pools.Header_Of (Self.Data);

      if R.Weak_Data = null then
         V :=
           new Weak_Data'
             (Refcount => 2,
              --  hold by Self and the result
              Lock     => 0,
              Element  => Convert (Self.Data));
         if not Sync_Bool_Compare_And_Swap (R.Weak_Data'Access, Oldval => null, Newval => V) then
            --  Was set by another thread concurrently
            Unchecked_Free (V);

            --  Need to increase refcount for the old weak ref
            Inc_Ref (R.Weak_Data, Atomic_Counters);
         end if;

      else
         Inc_Ref (R.Weak_Data, Atomic_Counters);
      end if;

      return (Controlled with Data => R.Weak_Data);
   end Weak;

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Ref'Class; Weak : Weak_Ref'Class) is
      Data : Pools.Element_Access;
      WD   : Weak_Data_Access := Weak.Data;
      NL   : Atomic_Counter;
   begin
      Finalize (Self);

      if WD = null then
         return;
      end if;

      Data := Convert (WD.Element);

      if Data = null then
         return;
      end if;

      if Integer (Sync_Add_And_Fetch (WD.Lock'Access, 2)) rem 2 /= 0 then
         return;
      end if;

      if Inc_Ref (Pools.Header_Of (Data), Atomic_Counters) then
         Self.Data := Data;
      end if;

      NL := Sync_Sub_And_Fetch (WD.Lock'Access, 2);

      pragma Assert (Integer (NL) rem 2 = 0, "Unexpected Lock value " & NL'Img);
   end Set;

   ---------------
   -- Was_Freed --
   ---------------

   function Was_Freed (Self : Weak_Ref'Class) return Boolean is
   begin
      return Self.Data = null or else Self.Data.Element = System.Null_Address;
   end Was_Freed;

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (P1, P2 : Ref) return Boolean is
   begin
      return P1.Data = P2.Data;
   end "=";

   ------------
   -- Adjust --
   ------------

   overriding
   procedure Adjust (Self : in out Ref) is
      RC : Atomic_Counter;
   begin
      if Self.Data /= null then
         RC := Inc_Ref (Pools.Header_Of (Self.Data), Atomic_Counters);

         pragma Assert (RC > 1, "Unexpected reference counter after adjust" & RC'Img);
      end if;
   end Adjust;

   ------------
   -- Adjust --
   ------------

   overriding
   procedure Adjust (Self : in out Weak_Ref) is
   begin
      if Self.Data /= null then
         Inc_Ref (Self.Data, Atomic_Counters);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Weak_Ref) is
   begin
      if Self.Data /= null then
         Finalize (Self.Data, Atomic_Counters);

         --  Make Finalize idempotent, since it could be called several times for the same instance (RM 7.6.1(24)).

         Self.Data := null;
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Ref) is
      R    : Counters_Access;
      Data : Pools.Element_Access := Self.Data;
   begin
      if Data /= null then
         Self.Data := null;

         R := Pools.Header_Of (Data);

         if (if Atomic_Counters then Decrement (R.Refcount) else Unsafe_Decrement (R.Refcount)) then
            if R.Weak_Data /= null then
               R.Weak_Data.Element := Null_Address;

               --  Spinlock to wait until all Set Ref from Weak_Ref operations completed.

               while R.Weak_Data.Lock /= 0
                 or else not Sync_Bool_Compare_And_Swap_Counter (R.Weak_Data.Lock'Access, 0, 1)
               loop
                  --  Would be better to use GCC _mm_pause instruction instead of zero delay but it is not supported in
                  --  GCC for all platforms.

                  delay 0.0;
               end loop;

               Finalize (R.Weak_Data, Atomic_Counters);
            end if;

            Release (Data.all);
            Unchecked_Free (Data); -- using storage_pool

         end if;
      end if;
   end Finalize;

   ------------------
   -- Get_Refcount --
   ------------------

   function Get_Refcount (Self : Ref'Class) return Natural is
   begin
      if Self.Data = null then
         return 0;
      else
         return Natural (Pools.Header_Of (Self.Data).Refcount);
      end if;
   end Get_Refcount;

   ------------------
   -- From_Element --
   ------------------

   procedure From_Element (Self : out Ref'Class; Element : Element_Access) is
   begin
      if Self.Data /= Element then
         Finalize (Self);
         Self.Data := Element;
         Adjust (Self);
      end if;
   end From_Element;
end Prunt.Limited_Shared_Pointers;
