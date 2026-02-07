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

package body Prunt.Status_Manager is

   pragma Extensions_Allowed (On);

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

   function Add_Module
     (This : Status_Data_Collection; Module_Name : Virtual_String; Groups : Status_Group_Maps.Map)
      return Status_Emitter is
   begin
      This.Internal.Get.Add_Module (Module_Name, Groups);
      return Status_Emitter'(Module_Name, This.Internal);
   end Add_Module;

   function JSON_Schema (This : Status_Data_Collection) return Virtual_String is
   begin
      return This.Internal.Get.JSON_Schema;
   end JSON_Schema;

   function JSON_Data (This : Status_Data_Collection) return Virtual_String is
   begin
      return This.Internal.Get.JSON_Data;
   end JSON_Data;

   protected body Status_Data_Collection_Internal is

      procedure Add_Module (Module_Name : Virtual_String; Groups : Status_Group_Maps.Map) is
         Root : constant JSON_Value := Create_Object;
      begin
         Modules.Insert (Module_Name, Groups);
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
      end Add_Module;

      procedure Ensure_Module_And_Group_Nodes (Module : Virtual_String; Group : Virtual_String) is
         Module_Node : JSON_Value;
      begin
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
         Ensure_Module_And_Group_Nodes (Module, Group);
         Status.Get (Module).Get (Group).Set_Field (Key, Value);
      end Set_Value_Internal;

      procedure Set_Value
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Dimensionless) is
      begin
         Set_Value_Internal (Module, Group, Key, Create (Long_Float (Value)));
      end Set_Value;

      procedure Set_Value
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Long_Long_Integer) is
      begin
         Set_Value_Internal (Module, Group, Key, Create (Value));
      end Set_Value;

      procedure Set_Value (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Boolean) is
      begin
         Set_Value_Internal (Module, Group, Key, Create (Value));
      end Set_Value;

      procedure Set_Value
        (Module : Virtual_String; Group : Virtual_String; Key : Virtual_String; Value : Virtual_String) is
      begin
         Set_Value_Internal (Module, Group, Key, Create (Value));
      end Set_Value;

      function JSON_Schema return Virtual_String is
      begin
         return Cached_Schema;
      end JSON_Schema;

      function JSON_Data return Virtual_String is
      begin
         return Write (Status);
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
