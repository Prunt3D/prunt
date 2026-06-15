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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with VSS.Strings.Conversions;

package body Prunt.Integration_Config_Overlays is

   pragma Style_Checks (Off);

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         raise Program_Error with Message;
      end if;
   end Assert;

   function Config_Patch_For_Overlay (Schema_String : Virtual_String; Overlay : JSON_Value) return JSON_Value is
      Schema : constant JSON_Value := Read (Schema_String);
      Patch  : constant JSON_Value := Create_Object;

      function Default_From_Property (Property : JSON_Value) return JSON_Value;
      procedure Merge_Object (Target : JSON_Value; Source : JSON_Value);

      procedure Add_Module (Name : Virtual_String; Value : JSON_Value);

      function Default_From_Property (Property : JSON_Value) return JSON_Value is
         Kind : constant String := VSS.Strings.Conversions.To_UTF_8_String (Property.Get ("Kind"));
      begin
         if
           Kind = "Boolean"
           or else Kind = "Discrete"
           or else Kind = "String"
           or else Kind = "Integer"
           or else Kind = "Float"
         then
            return Clone (Property.Get ("Default"));
         elsif Kind = "Float_Ratio" then
            return Result : constant JSON_Value := Create_Object do
               Result.Set_Field (+"Numerator", Clone (Property.Get ("Default_Numerator")));
               Result.Set_Field (+"Denominator", Clone (Property.Get ("Default_Denominator")));
            end return;
         elsif Kind = "Sequence" then
            return Result : constant JSON_Value := Create_Object do
               declare
                  procedure Add_Default (Name : Virtual_String; Child : JSON_Value);

                  procedure Add_Default (Name : Virtual_String; Child : JSON_Value) is
                  begin
                     Result.Set_Field (Name, Default_From_Property (Child));
                  end Add_Default;
               begin
                  Property.Get ("Children").Map_JSON_Object (Add_Default'Access);
               end;
            end return;
         elsif Kind = "Variant" then
            return Result : constant JSON_Value := Create_Object do
               Result.Set_Field (+"Selected", Clone (Property.Get ("Default")));
               Result.Set_Field ("Children", Create_Object);
               declare
                  procedure Add_Default (Name : Virtual_String; Child : JSON_Value);

                  procedure Add_Default (Name : Virtual_String; Child : JSON_Value) is
                  begin
                     Result.Get ("Children").Set_Field (Name, Default_From_Property (Child));
                  end Add_Default;
               begin
                  Property.Get ("Children").Map_JSON_Object (Add_Default'Access);
               end;
            end return;
         else
            raise Constraint_Error with "Unhandled config schema property kind: " & Kind;
         end if;
      end Default_From_Property;

      procedure Merge_Object (Target : JSON_Value; Source : JSON_Value) is
         procedure Merge_Field (Name : Virtual_String; Field : JSON_Value);

         procedure Merge_Field (Name : Virtual_String; Field : JSON_Value) is
         begin
            if
              Target.Has_Field (Name)
              and then Target.Get (Name).Kind = JSON_Object_Type
              and then Field.Kind = JSON_Object_Type
            then
               Merge_Object (Target.Get (Name), Field);
            else
               Target.Set_Field (Name, Clone (Field));
            end if;
         end Merge_Field;
      begin
         if Source.Kind /= JSON_Object_Type then
            raise Constraint_Error with "Scenario config overlay values must be JSON objects.";
         end if;

         Source.Map_JSON_Object (Merge_Field'Access);
      end Merge_Object;

      procedure Add_Module (Name : Virtual_String; Value : JSON_Value) is
      begin
         Assert
           (Schema.Get ("Config").Has_Field (Name),
            "Config overlay references unknown module: " & VSS.Strings.Conversions.To_UTF_8_String (Name));
         declare
            Module : constant JSON_Value := Patch.Get ("Config").Get (Name);
         begin
            Merge_Object (Module.Get ("Config"), Value);
         end;
      end Add_Module;

      procedure Add_Default_Module (Name : Virtual_String; Module_Schema : JSON_Value);

      procedure Add_Default_Module (Name : Virtual_String; Module_Schema : JSON_Value) is
         Module : constant JSON_Value := Create_Object;
         Config : constant JSON_Value := Create_Object;

         procedure Add_Default (Property_Name : Virtual_String; Property : JSON_Value);

         procedure Add_Default (Property_Name : Virtual_String; Property : JSON_Value) is
         begin
            Config.Set_Field (Property_Name, Default_From_Property (Property));
         end Add_Default;
      begin
         Module.Set_Field (+"Version", Clone (Module_Schema.Get ("Version")));
         Module.Set_Field (+"Config", Config);
         Module_Schema.Get ("Config").Map_JSON_Object (Add_Default'Access);
         Patch.Get ("Config").Set_Field (Name, Module);
      end Add_Default_Module;
   begin
      Patch.Set_Field ("Prunt config version", Create (Long_Long_Integer'(1)));
      Patch.Set_Field ("Config", Create_Object);
      Schema.Get ("Config").Map_JSON_Object (Add_Default_Module'Access);

      if Overlay.Kind /= JSON_Object_Type then
         raise Constraint_Error with "Scenario config overlay must be a JSON object.";
      end if;

      Overlay.Map_JSON_Object (Add_Module'Access);
      return Patch;
   end Config_Patch_For_Overlay;

   procedure Apply_Config_Overlay
     (Schema_String : Virtual_String;
      Overlay       : JSON_Value;
      Apply         : access procedure
        (Patch : Virtual_String; Result : out Virtual_String; Errors : out Config.Config_Error_Vectors.Vector))
   is
      Patch  : constant JSON_Value := Config_Patch_For_Overlay (Schema_String, Overlay);
      Result : Virtual_String;
      Errors : Config.Config_Error_Vectors.Vector;
   begin
      Apply (Write (Patch), Result, Errors);
      if not Errors.Is_Empty then
         declare
            Message : Unbounded_String := To_Unbounded_String ("Invalid scenario config overlay:");
         begin
            for Error of Errors loop
               Append
                 (Message,
                  " "
                  & Config.Config_Data_Paths.Vector'Image (Error.Path)
                  & ": "
                  & VSS.Strings.Conversions.To_UTF_8_String (Error.Message));
            end loop;
            raise Program_Error with To_String (Message);
         end;
      end if;
   end Apply_Config_Overlay;

end Prunt.Integration_Config_Overlays;
