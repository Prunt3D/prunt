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

package body Prunt.Status_Manager is

   pragma Extensions_Allowed (On);

   procedure Set_Value (This : Lock_Free_Dimensionless_Setter; Value : Dimensionless) is
   begin
      Atomic_Dimensionless_Shared_Pointers.Unchecked_Get (This.Internal).all := Atomic_Dimensionless (Value);
   end Set_Value;

   procedure Set_Value (This : Lock_Free_Boolean_Setter; Value : Boolean) is
   begin
      Atomic_Boolean_Shared_Pointers.Unchecked_Get (This.Internal).all := Atomic_Boolean (Value);
   end Set_Value;

   procedure Set_Value (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String; Value : Dimensionless) is
   begin
      This.Internal.Get.Set_Value (This.Module, Group, Key, Value);
   end Set_Value;

   procedure Set_Value (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String; Value : Long_Long_Integer)
   is
   begin
      This.Internal.Get.Set_Value (This.Module, Group, Key, Value);
   end Set_Value;

   procedure Set_Value (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String; Value : Boolean) is
   begin
      This.Internal.Get.Set_Value (This.Module, Group, Key, Value);
   end Set_Value;

   procedure Set_Value (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String; Value : Virtual_String) is
   begin
      This.Internal.Get.Set_Value (This.Module, Group, Key, Value);
   end Set_Value;

   function Get_Lock_Free_Setter
     (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String) return Lock_Free_Dimensionless_Setter
   is
      Internal : Atomic_Dimensionless_Shared_Pointers.Ref;
   begin
      This.Internal.Get.Get_Lock_Free_Setter (This.Module, Group, Key, Internal);
      return (Internal => Internal);
   end Get_Lock_Free_Setter;

   function Get_Lock_Free_Setter
     (This : Status_Emitter; Group : Virtual_String; Key : Virtual_String) return Lock_Free_Boolean_Setter
   is
      Internal : Atomic_Boolean_Shared_Pointers.Ref;
   begin
      This.Internal.Get.Get_Lock_Free_Setter (This.Module, Group, Key, Internal);
      return (Internal => Internal);
   end Get_Lock_Free_Setter;

   function Build_Collection (Modules : Status_Module_Maps.Map) return Status_Data_Collection is
   begin
      return Result : Status_Data_Collection do
         Result.Internal.Get.Initialize (Modules);
      end return;
   end Build_Collection;

   function Get_Emitter (This : Status_Data_Collection; Module_Name : Virtual_String) return Status_Emitter is
   begin
      if not This.Internal.Get.Has_Module (Module_Name) then
         raise Constraint_Error with "Status module " & Module_Name'Image & " does not exist.";
      end if;

      return Status_Emitter'(Module_Name, This.Internal);
   end Get_Emitter;

   function JSON_Schema (This : Status_Data_Collection) return Virtual_String is
   begin
      return This.Internal.Get.JSON_Schema;
   end JSON_Schema;

   function JSON_Data (This : Status_Data_Collection) return Virtual_String is
      Result : Virtual_String;
   begin
      This.Internal.Get.JSON_Data (Result);
      return Result;
   end JSON_Data;

   protected body Status_Data_Collection_Internal is

      procedure Initialize (Modules : Status_Module_Maps.Map) is
         Root : constant JSON_Value := Create_Object;
      begin
         Status_Data_Collection_Internal.Modules := Modules;
         for M in Modules.Iterate loop
            declare
               Module_Node   : constant JSON_Value := Create_Object;
               Module_Groups : constant Status_Group_Maps.Map := Status_Module_Maps.Element (M);
            begin
               for G in Module_Groups.Iterate loop
                  declare
                     Group_Node   : constant JSON_Value := Create_Object;
                     Group_Values : constant Status_Value_Maps.Map := Status_Group_Maps.Element (G);
                  begin
                     for V in Group_Values.Iterate loop
                        declare
                           Value_Node : constant JSON_Value := Create_Object;
                           Value_Meta : constant Status_Value := Status_Value_Maps.Element (V);
                        begin
                           if Value_Meta.Kind = Real_Kind then
                              Value_Node.Set_Field ("Kind", "Real");
                           elsif Value_Meta.Kind = Integer_Kind then
                              Value_Node.Set_Field ("Kind", "Integer");
                           elsif Value_Meta.Kind = Boolean_Kind then
                              Value_Node.Set_Field ("Kind", "Boolean");
                           elsif Value_Meta.Kind = String_Kind then
                              Value_Node.Set_Field ("Kind", "String");
                           end if;

                           Value_Node.Set_Field ("Unit", Value_Meta.Unit);
                           Value_Node.Set_Field ("Description", Value_Meta.Description);
                           Value_Node.Set_Field ("Condition", Value_Meta.Condition);

                           Group_Node.Set_Field (Status_Value_Maps.Key (V), Value_Node);
                        end;
                     end loop;
                     Module_Node.Set_Field (Status_Group_Maps.Key (G), Group_Node);
                  end;
               end loop;
               Root.Set_Field (Status_Module_Maps.Key (M), Module_Node);
            end;
         end loop;

         Cached_Schema := Write (Root);
      end Initialize;

      function Has_Module (Module_Name : Virtual_String) return Boolean is
      begin
         return Modules.Contains (Module_Name);
      end Has_Module;

      procedure Validate_Key
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Kind : Status_Value_Kind) is
      begin
         Ensure_Module_And_Group_Nodes (Module, Group);

         if not Modules.Element (Module).Element (Group).Contains (Key) then
            raise Constraint_Error
              with
                "Status key "
                & Key'Image
                & " does not exist in group "
                & Group'Image
                & " in module "
                & Module'Image
                & ".";
         end if;

         if Modules.Element (Module).Element (Group).Element (Key).Kind /= Kind then
            raise Constraint_Error
              with
                "Status key "
                & Key'Image
                & " in group "
                & Group'Image
                & " in module "
                & Module'Image
                & " has the wrong type.";
         end if;
      end Validate_Key;

      procedure Ensure_Module_And_Group_Nodes (Module : Virtual_String; Group : Virtual_String) is
         Module_Node : JSON_Value;
      begin
         if not Modules.Contains (Module) then
            raise Constraint_Error with "Status module " & Module'Image & " does not exist.";
         end if;

         if not Modules.Element (Module).Contains (Group) then
            raise Constraint_Error
              with "Status group " & Group'Image & " does not exist in module " & Module'Image & ".";
         end if;

         if not Status.Has_Field (Module) then
            Status.Set_Field (Module, Create_Object);
         end if;
         Module_Node := Status.Get (Module);

         if not Module_Node.Has_Field (Group) then
            Module_Node.Set_Field (Group, Create_Object);
         end if;
      end Ensure_Module_And_Group_Nodes;

      procedure Set_Value_Internal
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : JSON_Value) is
      begin
         Status.Get (Module).Get (Group).Set_Field (Key, Value);
      end Set_Value_Internal;

      procedure Set_Value
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Dimensionless)
      is
         Module_Refs : Atomic_Dimensionless_Group_Maps.Map;
         Group_Refs  : Atomic_Dimensionless_Ref_Maps.Map;
      begin
         Validate_Key (Module, Group, Key, Real_Kind);

         if Lock_Free_Dimensionless_Refs.Contains (Module) then
            Module_Refs := Lock_Free_Dimensionless_Refs.Element (Module);
            if Module_Refs.Contains (Group) then
               Group_Refs := Module_Refs.Element (Group);
               if Group_Refs.Contains (Key) then
                  Atomic_Dimensionless_Shared_Pointers.Unchecked_Get (Group_Refs.Element (Key)).all :=
                    Atomic_Dimensionless (Value);
                  return;
               end if;
            end if;
         end if;

         Status.Get (Module).Get (Group).Set_Field_Long_Float (Key, Long_Float (Value));
      end Set_Value;

      procedure Set_Value
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Long_Long_Integer) is
      begin
         Validate_Key (Module, Group, Key, Integer_Kind);

         Set_Value_Internal (Module, Group, Key, Create (Value));
      end Set_Value;

      procedure Set_Value (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Boolean) is
         Module_Refs : Atomic_Boolean_Group_Maps.Map;
         Group_Refs  : Atomic_Boolean_Ref_Maps.Map;
      begin
         Validate_Key (Module, Group, Key, Boolean_Kind);

         if Lock_Free_Boolean_Refs.Contains (Module) then
            Module_Refs := Lock_Free_Boolean_Refs.Element (Module);
            if Module_Refs.Contains (Group) then
               Group_Refs := Module_Refs.Element (Group);
               if Group_Refs.Contains (Key) then
                  Atomic_Boolean_Shared_Pointers.Unchecked_Get (Group_Refs.Element (Key)).all :=
                    Atomic_Boolean (Value);
                  return;
               end if;
            end if;
         end if;

         Status.Get (Module).Get (Group).Set_Field (Key, Value);
      end Set_Value;

      procedure Set_Value
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Virtual_String) is
      begin
         Validate_Key (Module, Group, Key, String_Kind);

         Set_Value_Internal (Module, Group, Key, Create (Value));
      end Set_Value;

      procedure Get_Lock_Free_Setter
        (Module : Virtual_String;
         Group  : Virtual_String;
         Key    : Virtual_String;
         Value  : out Atomic_Dimensionless_Shared_Pointers.Ref)
      is
         function Get_Data return Atomic_Dimensionless
         is (if Status.Has_Field (Module)
               and then Status.Get (Module).Has_Field (Group)
               and then Status.Get (Module).Get (Group).Has_Field (Key)
             then Atomic_Dimensionless (Dimensionless'(Status.Get (Module).Get (Group).Get (Key).Get))
             else 0.0);

         Module_Refs : Atomic_Dimensionless_Group_Maps.Map;
         Group_Refs  : Atomic_Dimensionless_Ref_Maps.Map;
      begin
         Validate_Key (Module, Group, Key, Real_Kind);

         if not Lock_Free_Dimensionless_Refs.Contains (Module) then
            Lock_Free_Dimensionless_Refs.Insert (Module, []);
         end if;

         Module_Refs := Lock_Free_Dimensionless_Refs.Element (Module);

         if not Module_Refs.Contains (Group) then
            Module_Refs.Insert (Group, []);
         end if;

         Group_Refs := Module_Refs.Element (Group);

         if Group_Refs.Contains (Key) then
            Value := Group_Refs.Element (Key);
         else
            Value.Set (Get_Data'Access);
            Group_Refs.Insert (Key, Value);
         end if;
      end Get_Lock_Free_Setter;

      procedure Get_Lock_Free_Setter
        (Module : Virtual_String;
         Group  : Virtual_String;
         Key    : Virtual_String;
         Value  : out Atomic_Boolean_Shared_Pointers.Ref)
      is
         function Get_Data return Atomic_Boolean
         is (if Status.Has_Field (Module)
               and then Status.Get (Module).Has_Field (Group)
               and then Status.Get (Module).Get (Group).Has_Field (Key)
             then Atomic_Boolean (Boolean'(Status.Get (Module).Get (Group).Get (Key).Get))
             else Status_Manager.False);

         Module_Refs : Atomic_Boolean_Group_Maps.Map;
         Group_Refs  : Atomic_Boolean_Ref_Maps.Map;
      begin
         Validate_Key (Module, Group, Key, Boolean_Kind);

         if not Lock_Free_Boolean_Refs.Contains (Module) then
            Lock_Free_Boolean_Refs.Insert (Module, []);
         end if;

         Module_Refs := Lock_Free_Boolean_Refs.Element (Module);

         if not Module_Refs.Contains (Group) then
            Module_Refs.Insert (Group, []);
         end if;

         Group_Refs := Module_Refs.Element (Group);

         if Group_Refs.Contains (Key) then
            Value := Group_Refs.Element (Key);
         else
            Value.Set (Get_Data'Access);
            Group_Refs.Insert (Key, Value);
         end if;
      end Get_Lock_Free_Setter;

      function JSON_Schema return Virtual_String is
      begin
         return Cached_Schema;
      end JSON_Schema;

      procedure JSON_Data (Value : out Virtual_String) is
      begin
         for Module in Lock_Free_Dimensionless_Refs.Iterate loop
            for Group in Module.Element.Iterate loop
               for Key in Group.Element.Iterate loop
                  Status.Get (Module.Key).Get (Group.Key).Set_Field_Long_Float
                    (Key.Key, Long_Float (Key.Element.Get.Element.all));
               end loop;
            end loop;
         end loop;

         for Module in Lock_Free_Boolean_Refs.Iterate loop
            for Group in Module.Element.Iterate loop
               for Key in Group.Element.Iterate loop
                  Status.Get (Module.Key).Get (Group.Key).Set_Field (Key.Key, Boolean (Key.Element.Get.Element.all));
               end loop;
            end loop;
         end loop;

         Value := Write (Status);
      end JSON_Data;

   end Status_Data_Collection_Internal;

   overriding
   procedure Initialize (Object : in out Status_Data_Collection) is
      function Get_Data return Status_Data_Collection_Internal is
      begin
         return Result : Status_Data_Collection_Internal;
      end Get_Data;
   begin
      Object.Internal.Set (Get_Data'Access);
   end Initialize;

   overriding
   procedure Finalize (Object : in out Status_Data_Collection) is
      Internal  : constant Status_Data_Collection_Internal_Shared_Pointers.Reference_Type := Object.Internal.Get;
      Ref_Count : constant Natural := Object.Internal.Get_Refcount;
   begin
      if Internal.Element /= null then
         if Ref_Count /= 1 then
            raise Constraint_Error
              with Ref_Count'Image & " references to status data collection still exist during finalisation.";
         end if;

         Object.Internal := Status_Data_Collection_Internal_Shared_Pointers.Null_Ref;
      end if;
   end Finalize;

end Prunt.Status_Manager;
