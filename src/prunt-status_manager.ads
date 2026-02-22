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

pragma Extensions_Allowed (On);

with Ada.Containers.Ordered_Maps;

private with Ada.Finalization;
private with Prunt.Limited_Shared_Pointers;
private with Prunt.JSON;

package Prunt.Status_Manager is

   --  TODO: Should we allow more complex nested structures here?

   type Status_Emitter (<>) is private;

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

   function JSON_Schema (This : Status_Data_Collection) return Virtual_String;
   function JSON_Data (This : Status_Data_Collection) return Virtual_String;

private

   use Prunt.JSON;

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

      function JSON_Schema return Virtual_String;
      function JSON_Data return Virtual_String;
   private
      procedure Set_Value_Internal
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : JSON_Value);

      Modules       : Status_Module_Maps.Map := [];
      Status        : JSON.JSON_Value := JSON.Create_Object;
      Cached_Schema : Virtual_String := "{}";
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
