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

with Ada.Containers.Ordered_Maps;

private with Ada.Finalization;
private with Prunt.Limited_Shared_Pointers;
private with Prunt.JSON;

package Prunt.Status_Manager is

   type Status_Emitter is private;

   type Lock_Free_Dimensionless_Setter is private;

   procedure Set_Value (This : Lock_Free_Dimensionless_Setter; Value : Dimensionless);

   type Lock_Free_Boolean_Setter is private;

   procedure Set_Value (This : Lock_Free_Boolean_Setter; Value : Boolean);

   type Status_Value_Kind is (Real_Kind, Integer_Kind, Boolean_Kind, String_Kind);

   type Status_Value (Kind : Status_Value_Kind := String_Kind) is record
      Unit : Virtual_String;
      --  Unit to display after the value. May be a blank string if the value does not have a unit.

      Description : Virtual_String;
      --  Description of what the value represents.

      Condition : Virtual_String;
      --  What configuration options or other conditions will cause this value to be used. Empty string if always used.
   end record;

   package Status_Value_Maps is new Ada.Containers.Ordered_Maps (Virtual_String, Status_Value);

   function Return_False (Left, Right : Status_Value_Maps.Map with Unreferenced) return Boolean
   is (False);

   package Status_Group_Maps is new
     Ada.Containers.Ordered_Maps (Virtual_String, Status_Value_Maps.Map, "=" => Return_False);

   function Return_False (Left, Right : Status_Group_Maps.Map with Unreferenced) return Boolean
   is (False);

   package Status_Module_Maps is new
     Ada.Containers.Ordered_Maps (Virtual_String, Status_Group_Maps.Map, "=" => Return_False);

   type Status_Data_Collection is limited private;

   function Build_Collection (Modules : Status_Module_Maps.Map) return Status_Data_Collection;

   function Get_Emitter (This : Status_Data_Collection; Module_Name : Virtual_String) return Status_Emitter;

   procedure Set_Value (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String; Value : Dimensionless);
   procedure Set_Value
     (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String; Value : Long_Long_Integer);
   procedure Set_Value (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String; Value : Boolean);
   procedure Set_Value (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String; Value : Virtual_String);

   function Get_Lock_Free_Setter
     (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String) return Lock_Free_Dimensionless_Setter;
   function Get_Lock_Free_Setter
     (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String) return Lock_Free_Boolean_Setter;

   function JSON_Schema (This : Status_Data_Collection) return Virtual_String;
   function JSON_Data (This : Status_Data_Collection) return Virtual_String;

private

   use Prunt.JSON;

   type Atomic_Dimensionless is new Dimensionless with Atomic, Volatile;
   type Atomic_Boolean is new Boolean with Atomic, Volatile;

   package Atomic_Dimensionless_Shared_Pointers is new Limited_Shared_Pointers (Atomic_Dimensionless);
   package Atomic_Boolean_Shared_Pointers is new Limited_Shared_Pointers (Atomic_Boolean);

   type Lock_Free_Dimensionless_Setter is record
      Internal : Atomic_Dimensionless_Shared_Pointers.Ref := Atomic_Dimensionless_Shared_Pointers.Null_Ref;
   end record;

   type Lock_Free_Boolean_Setter is record
      Internal : Atomic_Boolean_Shared_Pointers.Ref := Atomic_Boolean_Shared_Pointers.Null_Ref;
   end record;

   package Atomic_Dimensionless_Ref_Maps is new
     Ada.Containers.Ordered_Maps
       (Virtual_String,
        Atomic_Dimensionless_Shared_Pointers.Ref,
        "=" => Atomic_Dimensionless_Shared_Pointers."=");

   function Return_False (Left, Right : Atomic_Dimensionless_Ref_Maps.Map with Unreferenced) return Boolean
   is (False);

   package Atomic_Dimensionless_Group_Maps is new
     Ada.Containers.Ordered_Maps (Virtual_String, Atomic_Dimensionless_Ref_Maps.Map, "=" => Return_False);

   function Return_False (Left, Right : Atomic_Dimensionless_Group_Maps.Map with Unreferenced) return Boolean
   is (False);

   package Atomic_Dimensionless_Module_Maps is new
     Ada.Containers.Ordered_Maps (Virtual_String, Atomic_Dimensionless_Group_Maps.Map, "=" => Return_False);

   package Atomic_Boolean_Ref_Maps is new
     Ada.Containers.Ordered_Maps
       (Virtual_String,
        Atomic_Boolean_Shared_Pointers.Ref,
        "=" => Atomic_Boolean_Shared_Pointers."=");

   function Return_False (Left, Right : Atomic_Boolean_Ref_Maps.Map with Unreferenced) return Boolean
   is (False);

   package Atomic_Boolean_Group_Maps is new
     Ada.Containers.Ordered_Maps (Virtual_String, Atomic_Boolean_Ref_Maps.Map, "=" => Return_False);

   function Return_False (Left, Right : Atomic_Boolean_Group_Maps.Map with Unreferenced) return Boolean
   is (False);

   package Atomic_Boolean_Module_Maps is new
     Ada.Containers.Ordered_Maps (Virtual_String, Atomic_Boolean_Group_Maps.Map, "=" => Return_False);

   protected type Status_Data_Collection_Internal is
      procedure Initialize (Modules : Status_Module_Maps.Map);

      function Has_Module (Module_Name : Virtual_String) return Boolean;

      procedure Set_Value
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Dimensionless);
      procedure Set_Value
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Long_Long_Integer);
      procedure Set_Value (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Boolean);
      procedure Set_Value
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Virtual_String);

      procedure Get_Lock_Free_Setter
        (Module : Virtual_String;
         Group  : Virtual_String;
         Key    : Virtual_String;
         Value  : out Atomic_Dimensionless_Shared_Pointers.Ref);
      procedure Get_Lock_Free_Setter
        (Module : Virtual_String;
         Group  : Virtual_String;
         Key    : Virtual_String;
         Value  : out Atomic_Boolean_Shared_Pointers.Ref);

      function JSON_Schema return Virtual_String;
      procedure JSON_Data (Value : out Virtual_String);
   private
      procedure Ensure_Module_And_Group_Nodes (Module : Virtual_String; Group : Virtual_String);

      procedure Set_Value_Internal
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : JSON_Value);
      procedure Validate_Key
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Kind : Status_Value_Kind);

      Modules                      : Status_Module_Maps.Map := [];
      Status                       : JSON.JSON_Value := JSON.Create_Object;
      Cached_Schema                : Virtual_String := "{}";
      Lock_Free_Dimensionless_Refs : Atomic_Dimensionless_Module_Maps.Map := [];
      Lock_Free_Boolean_Refs       : Atomic_Boolean_Module_Maps.Map := [];
   end Status_Data_Collection_Internal;

   package Status_Data_Collection_Internal_Shared_Pointers is new
     Limited_Shared_Pointers (Status_Data_Collection_Internal);

   type Status_Data_Collection is new Ada.Finalization.Limited_Controlled with record
      Internal : Status_Data_Collection_Internal_Shared_Pointers.Ref :=
        Status_Data_Collection_Internal_Shared_Pointers.Null_Ref;
   end record;

   overriding
   procedure Initialize (Object : in out Status_Data_Collection);

   overriding
   procedure Finalize (Object : in out Status_Data_Collection);
   --  During finalisation we check that all `Status_Emitter` instances are finalised as the modules that hold them
   --  should be finalised before the relevant `Status_Data_Collection` is.
   --
   --  Excluding this check would not lead to any memory safety issues. This check is just to make sure that modules
   --  are not misbehaving.

   type Status_Emitter is record
      Module   : Virtual_String;
      Internal : Status_Data_Collection_Internal_Shared_Pointers.Ref;
   end record;

end Prunt.Status_Manager;
