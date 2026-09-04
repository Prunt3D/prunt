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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Prunt.Mockable.Directories;
with Prunt.Mockable.Text_IO;
with Prunt.Mockable.Text_IO.Unbounded_IO;
with VSS.Strings.Conversions;

package body Prunt.Config is

   pragma Extensions_Allowed (On);

   pragma Unsuppress (All_Checks);

   procedure Validate_Field_Names
     (Val            : JSON_Value;
      Allowed_Fields : VSS.String_Vectors.Virtual_String_Vector;
      Report         : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String);
      Path           : Config_Data_Paths.Vector)
   is
      use Config_Data_Paths;
      procedure Check_Field (Name : Virtual_String; Value : JSON_Value);

      procedure Check_Field (Name : Virtual_String; Value : JSON_Value) is
         pragma Unreferenced (Value);
      begin
         if not Allowed_Fields.Contains (Name) then
            Report (Path & Name, "Field not in schema.");
         end if;
      end Check_Field;
   begin
      Val.Map_JSON_Object (Check_Field'Access);
   end Validate_Field_Names;

   function Get_JSON_Node
     (Root : JSON_Value; Path : Config_Data_Paths.Vector; Module : Virtual_String) return JSON_Value
   is
      Current_Node : JSON_Value := Root;
   begin
      for Key of Path loop
         if not Current_Node.Has_Field (Key) then
            raise Constraint_Error
              with "Invalid path: " & Path'Image & " for module " & Conversions.To_UTF_8_String (Module);
         end if;

         Current_Node := Current_Node.Get (Key);
      end loop;

      return Current_Node;
   end Get_JSON_Node;

   procedure Set_JSON_Node (Root : JSON_Value; Path : Config_Data_Paths.Vector; Value : JSON_Value) is
      use Config_Data_Paths;
      use type Ada.Containers.Count_Type;

      Current_Node : JSON_Value := Root;
   begin
      if Path.Length = 0 then
         raise Constraint_Error with "Empty path.";
      end if;

      if Current_Node.Kind /= JSON_Object_Type then
         raise Constraint_Error with "Invalid path.";
      end if;

      for I in Path.First_Index .. Path.Last_Index - 1 loop
         if not Current_Node.Has_Field (Path.Element (I)) then
            Current_Node.Set_Field (Path.Element (I), Create_Object);
         end if;
         Current_Node := Current_Node.Get (Path.Element (I));
         if Current_Node.Kind /= JSON_Object_Type then
            raise Constraint_Error with "Invalid path.";
         end if;
      end loop;

      Current_Node.Set_Field (Path.Last_Element, Value);
   end Set_JSON_Node;

   procedure Set_Field (Val : JSON_Value; Field : Virtual_String; Value : Dimensionless) is
   begin
      Val.Set_Field_Long_Float (Field, Long_Float (Value));
   end Set_Field;

   procedure Validate_Outer_Config_Structure
     (Config : JSON_Value; Report : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String)) is
   begin
      if Config.Kind /= JSON_Object_Type then
         Report ([], "Top level must be object.");
      else
         Validate_Field_Names (Config, ["Prunt config version", "Config"], Report, []);
         if not Config.Has_Field ("Prunt config version") then
            Report ([], "Prunt config version field is missing.");
         elsif Config.Get ("Prunt config version").Kind /= JSON_Int_Type then
            Report (["Prunt config version"], "Field must be integer.");
         elsif JSON_Value'(Config.Get ("Prunt config version")).Get /= Long_Long_Integer'(1) then
            Report
              (["Prunt config version"], "Must be equal to 1. This config file may be for a newer Prunt version.");
         elsif not Config.Has_Field ("Config") then
            Report ([], "Outer Config field is missing.");
         elsif Config.Get ("Config").Kind /= JSON_Object_Type then
            Report (["Config"], "Config field must be object.");
         end if;
      end if;
   end Validate_Outer_Config_Structure;

   procedure Validate_Module_Config_Structure
     (Config : JSON_Value; Report : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String)) is
   begin
      if Config.Kind /= JSON_Object_Type then
         Report ([], "Top level must be object.");
      else
         Validate_Field_Names (Config, ["Version", "Config"], Report, []);
         if not Config.Has_Field ("Version") then
            Report ([], "Version field is missing.");
         elsif Config.Get ("Version").Kind /= JSON_Int_Type then
            Report (["Version"], "Field must be integer.");
         elsif not Config.Has_Field ("Config") then
            Report ([], "Config field is missing.");
         elsif Config.Get ("Config").Kind /= JSON_Object_Type then
            Report (["Config"], "Config field must be object.");
         end if;
      end if;
   end Validate_Module_Config_Structure;

   procedure Validate_Module_Config_To_Schema
     (Config                   : JSON_Value;
      Schema                   : Config_Property_Maps.Map;
      Report                   : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String);
      Check_For_Missing_Fields : Boolean)
   is
      use Config_Data_Paths;

      procedure Mapper
        (Val      : JSON_Value;
         CB       :
           access procedure
             (Property : Config_Property_Parameters'Class; Path : Config_Data_Paths.Vector; Value : JSON_Value);
         Children : Config_Property_Maps.Map;
         Path     : Config_Data_Paths.Vector);

      procedure Recursive_Validate
        (Property : Config_Property_Parameters'Class; Path : Config_Data_Paths.Vector; Value : JSON_Value);

      procedure Mapper
        (Val      : JSON_Value;
         CB       :
           access procedure
             (Property : Config_Property_Parameters'Class; Path : Config_Data_Paths.Vector; Value : JSON_Value);
         Children : Config_Property_Maps.Map;
         Path     : Config_Data_Paths.Vector)
      is
         procedure Internal (Name : Virtual_String; Value : JSON_Value);

         procedure Internal (Name : Virtual_String; Value : JSON_Value) is
         begin
            if not Children.Contains (Name) then
               Report (Path & Name, "Field not in schema.");
            else
               CB (Children (Name), Path & Name, Value);
            end if;
         end Internal;

      begin
         Val.Map_JSON_Object (Internal'Access);

         if Check_For_Missing_Fields then
            for C in Children.Iterate loop
               if not Val.Has_Field (Config_Property_Maps.Key (C)) then
                  Report (Path & Config_Property_Maps.Key (C), "Field is missing.");
               end if;
            end loop;
         end if;
      end Mapper;

      procedure Recursive_Validate
        (Property : Config_Property_Parameters'Class; Path : Config_Data_Paths.Vector; Value : JSON_Value)
      is
         procedure Validate (Property : Config_Property_Parameters_Boolean);
         procedure Validate (Property : Config_Property_Parameters_Discrete);
         procedure Validate (Property : Config_Property_Parameters_String);
         procedure Validate (Property : Config_Property_Parameters_Integer);
         procedure Validate (Property : Config_Property_Parameters_Float);
         procedure Validate (Property : Config_Property_Parameters_Float_Ratio);
         procedure Validate (Property : Config_Property_Parameters_Sequence);
         procedure Validate (Property : Config_Property_Parameters_Variant);

         procedure Validate (Property : Config_Property_Parameters_Boolean) is
            pragma Unreferenced (Property);
         begin
            if Value.Kind /= JSON_Boolean_Type then
               Report (Path, "Value type should be boolean.");
            end if;
         end Validate;

         procedure Validate (Property : Config_Property_Parameters_Discrete) is
         begin
            if Value.Kind /= JSON_String_Type then
               Report (Path, "Value type should be string.");
            elsif not Property.Options.Contains (Value.Get) then
               Report
                 (Path,
                  Conversions.To_Virtual_String
                    ("Value is not a valid option, options are " & Property.Options'Image & "."));
            end if;
         end Validate;

         procedure Validate (Property : Config_Property_Parameters_String) is
            pragma Unreferenced (Property);
         begin
            if Value.Kind /= JSON_String_Type then
               Report (Path, "Value type should be string.");
            end if;
         end Validate;

         procedure Validate (Property : Config_Property_Parameters_Integer) is
         begin
            if Value.Kind /= JSON_Int_Type then
               Report (Path, "Value type should be integer, floats are not allowed even without a decimal part.");
            elsif Value.Get not in Property.Min .. Property.Max then
               Report
                 (Path,
                  Conversions.To_Virtual_String
                    ("Value out of range, allowed range is "
                     & Property.Min'Image
                     & " .. "
                     & Property.Max'Image
                     & "."));
            end if;
         end Validate;

         procedure Validate (Property : Config_Property_Parameters_Float) is
         begin
            if Value.Kind not in JSON_Int_Type | JSON_Float_Type then
               Report (Path, "Value type should be integer or float.");
            elsif Value.Get not in Property.Min .. Property.Max then
               Report
                 (Path,
                  Conversions.To_Virtual_String
                    ("Value out of range, allowed range is "
                     & Property.Min'Image
                     & " .. "
                     & Property.Max'Image
                     & "."));
            end if;
         end Validate;

         procedure Validate (Property : Config_Property_Parameters_Float_Ratio) is
         begin
            if Value.Kind not in JSON_Object_Type then
               Report (Path, "Value type should be object.");
            else
               Validate_Field_Names (Value, ["Numerator", "Denominator"], Report, Path);

               if not (Value.Has_Field ("Numerator") and then Value.Has_Field ("Denominator")) then
                  if not Value.Has_Field ("Numerator") then
                     Report (Path, "Numerator is missing.");
                  end if;
                  if not Value.Has_Field ("Denominator") then
                     Report (Path, "Denominator is missing.");
                  end if;
               elsif Value.Get ("Numerator").Kind not in JSON_Int_Type | JSON_Float_Type
                 or else Value.Get ("Denominator").Kind not in JSON_Int_Type | JSON_Float_Type
               then
                  if Value.Get ("Numerator").Kind not in JSON_Int_Type | JSON_Float_Type then
                     Report (Path & "Numerator", "Value type should be integer or float.");
                  end if;
                  if Value.Get ("Denominator").Kind not in JSON_Int_Type | JSON_Float_Type then
                     Report (Path & "Denominator", "Value type should be integer or float.");
                  end if;
               elsif Value.Get ("Denominator") = Dimensionless'(0.0) then
                  Report (Path & "Denominator", "Denominator must not be zero.");
               elsif Value.Get ("Numerator") / Value.Get ("Denominator") not in Property.Min .. Property.Max then
                  Report
                    (Path,
                     Conversions.To_Virtual_String
                       ("Value out of range, allowed range is "
                        & Property.Min'Image
                        & " .. "
                        & Property.Max'Image
                        & "."));
               end if;
            end if;
         end Validate;

         procedure Validate (Property : Config_Property_Parameters_Sequence) is
         begin
            if Value.Kind /= JSON_Object_Type then
               Report (Path, "Value type should be object.");
            else
               Mapper (Value, Recursive_Validate'Access, Property.Children, Path);
            end if;
         end Validate;

         procedure Validate (Property : Config_Property_Parameters_Variant) is
         begin
            if Value.Kind /= JSON_Object_Type then
               Report (Path, "Value type should be object.");
            else
               Validate_Field_Names (Value, ["Selected", "Children"], Report, Path);

               if Check_For_Missing_Fields then
                  if not Value.Has_Field ("Selected") then
                     Report (Path, "Selected field is missing.");
                  end if;
                  if not Value.Has_Field ("Children") then
                     Report (Path, "Children field is missing.");
                  end if;
               end if;

               if Value.Has_Field ("Selected") then
                  if Value.Get ("Selected").Kind /= JSON_String_Type then
                     Report (Path & "Selected", "Value type should be string.");
                  elsif not Property.Children.Contains (Value.Get ("Selected").Get) then
                     Report (Path & "Selected", "Value is not a valid option.");
                  end if;
               end if;

               if Value.Has_Field ("Children") then
                  if Value.Get ("Children").Kind /= JSON_Object_Type then
                     Report (Path & "Children", "Value type should be object.");
                  else
                     Mapper (Value.Get ("Children"), Recursive_Validate'Access, Property.Children, Path & "Children");
                  end if;
               end if;
            end if;
         end Validate;
      begin

         if Property in Config_Property_Parameters_Boolean then
            Validate (Config_Property_Parameters_Boolean (Property));
         elsif Property in Config_Property_Parameters_Discrete then
            Validate (Config_Property_Parameters_Discrete (Property));
         elsif Property in Config_Property_Parameters_String then
            Validate (Config_Property_Parameters_String (Property));
         elsif Property in Config_Property_Parameters_Integer then
            Validate (Config_Property_Parameters_Integer (Property));
         elsif Property in Config_Property_Parameters_Float then
            Validate (Config_Property_Parameters_Float (Property));
         elsif Property in Config_Property_Parameters_Float_Ratio then
            Validate (Config_Property_Parameters_Float_Ratio (Property));
         elsif Property in Config_Property_Parameters_Sequence then
            Validate (Config_Property_Parameters_Sequence (Property));
         elsif Property in Config_Property_Parameters_Variant then
            Validate (Config_Property_Parameters_Variant (Property));
         else
            raise Constraint_Error with "Unhandled property type (" & Property'Tag'Image & ").";
         end if;
      end Recursive_Validate;
   begin
      Mapper (Config, Recursive_Validate'Access, Schema, []);
   end Validate_Module_Config_To_Schema;

   function Create_Default_Property_Config (Property : Config_Property_Parameters'Class) return JSON_Value is
      function Generate (Property : Config_Property_Parameters_Boolean) return JSON_Value;
      function Generate (Property : Config_Property_Parameters_Discrete) return JSON_Value;
      function Generate (Property : Config_Property_Parameters_String) return JSON_Value;
      function Generate (Property : Config_Property_Parameters_Integer) return JSON_Value;
      function Generate (Property : Config_Property_Parameters_Float) return JSON_Value;
      function Generate (Property : Config_Property_Parameters_Float_Ratio) return JSON_Value;
      function Generate (Property : Config_Property_Parameters_Sequence) return JSON_Value;
      function Generate (Property : Config_Property_Parameters_Variant) return JSON_Value;

      function Generate (Property : Config_Property_Parameters_Boolean) return JSON_Value is
      begin
         return Create (Property.Default);
      end Generate;

      function Generate (Property : Config_Property_Parameters_Discrete) return JSON_Value is
      begin
         return Create (Property.Default);
      end Generate;

      function Generate (Property : Config_Property_Parameters_String) return JSON_Value is
      begin
         return Create (Property.Default);
      end Generate;

      function Generate (Property : Config_Property_Parameters_Integer) return JSON_Value is
      begin
         return Create (Property.Default);
      end Generate;

      function Generate (Property : Config_Property_Parameters_Float) return JSON_Value is
      begin
         return Create (Long_Float (Property.Default));
      end Generate;

      function Generate (Property : Config_Property_Parameters_Float_Ratio) return JSON_Value is
      begin
         return Result : constant JSON_Value := Create_Object do
            Set_Field (Result, "Numerator", Property.Default.Numerator);
            Set_Field (Result, "Denominator", Property.Default.Denominator);
         end return;
      end Generate;

      function Generate (Property : Config_Property_Parameters_Sequence) return JSON_Value is
      begin
         return Result : constant JSON_Value := Create_Object do
            for C in Property.Children.Iterate loop
               Result.Set_Field
                 (Config_Property_Maps.Key (C), Create_Default_Property_Config (Config_Property_Maps.Element (C)));
            end loop;
         end return;
      end Generate;

      function Generate (Property : Config_Property_Parameters_Variant) return JSON_Value is
      begin
         return Result : constant JSON_Value := Create_Object do
            Result.Set_Field ("Selected", Property.Default);
            Result.Set_Field ("Children", Create_Object);
            for C in Property.Children.Iterate loop
               Result.Get ("Children").Set_Field
                 (Config_Property_Maps.Key (C), Create_Default_Property_Config (Config_Property_Maps.Element (C)));
            end loop;
         end return;
      end Generate;
   begin
      if Property in Config_Property_Parameters_Boolean then
         return Generate (Config_Property_Parameters_Boolean (Property));
      elsif Property in Config_Property_Parameters_Discrete then
         return Generate (Config_Property_Parameters_Discrete (Property));
      elsif Property in Config_Property_Parameters_String then
         return Generate (Config_Property_Parameters_String (Property));
      elsif Property in Config_Property_Parameters_Integer then
         return Generate (Config_Property_Parameters_Integer (Property));
      elsif Property in Config_Property_Parameters_Float then
         return Generate (Config_Property_Parameters_Float (Property));
      elsif Property in Config_Property_Parameters_Float_Ratio then
         return Generate (Config_Property_Parameters_Float_Ratio (Property));
      elsif Property in Config_Property_Parameters_Sequence then
         return Generate (Config_Property_Parameters_Sequence (Property));
      elsif Property in Config_Property_Parameters_Variant then
         return Generate (Config_Property_Parameters_Variant (Property));
      else
         raise Constraint_Error with "Unhandled property type (" & Property'Tag'Image & ").";
      end if;
   end Create_Default_Property_Config;

   function Create_Default_Module_Config (Schema : Config_Property_Maps.Map) return JSON_Value is
   begin
      return Result : constant JSON_Value := Create_Object do
         for C in Schema.Iterate loop
            Result.Set_Field
              (Config_Property_Maps.Key (C), Create_Default_Property_Config (Config_Property_Maps.Element (C)));
         end loop;
      end return;
   end Create_Default_Module_Config;

   function Generate_Schemas_String (Schemas : Config_Schema_Maps.Map) return Virtual_String is
      use type Ada.Tags.Tag;

      function Controller_Is_Variant (Condition : Config_Presentation_Condition) return Boolean;

      function Resolve_Controller_Owner (Condition : Config_Presentation_Condition) return Virtual_String;

      procedure Validate_Presentation_Conditions;

      function Controller_Is_Variant (Condition : Config_Presentation_Condition) return Boolean is
         function Find
           (Property : Config_Property_Parameters'Class; Path : Config_Data_Paths.Vector; Next_Index : Positive)
            return Boolean;

         function Find
           (Property : Config_Property_Parameters'Class; Path : Config_Data_Paths.Vector; Next_Index : Positive)
            return Boolean is
         begin
            if Next_Index > Path.Last_Index then
               return Property in Config_Property_Parameters_Variant;
            elsif Property in Config_Property_Parameters_Sequence then
               return
                 Find
                   (Config_Property_Parameters_Sequence (Property).Children (Path (Next_Index)), Path, Next_Index + 1);
            elsif Property in Config_Property_Parameters_Variant
              and then Path (Next_Index) = "Children"
              and then Next_Index < Path.Last_Index
            then
               return
                 Find
                   (Config_Property_Parameters_Variant (Property).Children (Path (Next_Index + 1)),
                    Path,
                    Next_Index + 2);
            else
               raise Program_Error with "Validated dynamic presentation controller path is malformed.";
            end if;
         end Find;
      begin
         for Module_Cursor in Schemas.Iterate loop
            if Config_Schema_Maps.Element (Module_Cursor).Module_Instance_Tag = Condition.Controller_Tag then
               return
                 Find
                   (Config_Schema_Maps.Element (Module_Cursor).Top_Level_Items
                      (Condition.Controller_Path.Path.First_Element),
                    Condition.Controller_Path.Path,
                    Condition.Controller_Path.Path.First_Index + 1);
            end if;
         end loop;

         raise Program_Error with "Validated dynamic presentation controller module is missing.";
      end Controller_Is_Variant;

      function Resolve_Controller_Owner (Condition : Config_Presentation_Condition) return Virtual_String is
      begin
         for Module_Cursor in Schemas.Iterate loop
            if Config_Schema_Maps.Element (Module_Cursor).Module_Instance_Tag = Condition.Controller_Tag then
               return Config_Schema_Maps.Key (Module_Cursor);
            end if;
         end loop;

         raise Program_Error with "Validated dynamic presentation controller module is missing.";
      end Resolve_Controller_Owner;

      procedure Validate_Presentation_Conditions is
         function Validate_Controller
           (Property : Config_Property_Parameters'Class; Values : Discrete_String_Sets.Set) return Boolean;

         function Match_Controller
           (Items      : Config_Property_Maps.Map;
            Path       : Config_Data_Paths.Vector;
            Path_Index : Positive;
            Values     : Discrete_String_Sets.Set) return Boolean;

         procedure Validate_Condition (Condition : Config_Presentation_Condition);
         procedure Validate_Children (Children : Config_Property_Maps.Map);
         procedure Validate_Property (Property : Config_Property_Parameters'Class);

         function Validate_Controller
           (Property : Config_Property_Parameters'Class; Values : Discrete_String_Sets.Set) return Boolean is
         begin
            if Values.Is_Empty then
               raise Constraint_Error with "Dynamic config presentation values must not be empty.";
            elsif Property in Config_Property_Parameters_Discrete then
               for Value of Values loop
                  if not Config_Property_Parameters_Discrete (Property).Options.Contains (Value) then
                     raise Constraint_Error
                       with
                         "Dynamic config presentation value is not an option of its controller (Value: "
                         & Conversions.To_UTF_8_String (Value)
                         & ").";
                  end if;
               end loop;
            elsif Property in Config_Property_Parameters_Variant then
               for Value of Values loop
                  if not Config_Property_Parameters_Variant (Property).Children.Contains (Value) then
                     raise Constraint_Error
                       with
                         "Dynamic config presentation value is not a variant of its controller (Value: "
                         & Conversions.To_UTF_8_String (Value)
                         & ").";
                  end if;
               end loop;
            else
               raise Constraint_Error with "Dynamic config presentation controller must be discrete or a variant.";
            end if;

            return True;
         end Validate_Controller;

         function Match_Controller
           (Items      : Config_Property_Maps.Map;
            Path       : Config_Data_Paths.Vector;
            Path_Index : Positive;
            Values     : Discrete_String_Sets.Set) return Boolean
         is
            Name : constant Virtual_String := Path (Path_Index);

            function Match_Property
              (Property : Config_Property_Parameters'Class; Next_Index : Positive) return Boolean;

            function Match_Property (Property : Config_Property_Parameters'Class; Next_Index : Positive) return Boolean
            is
            begin
               if Next_Index > Path.Last_Index then
                  return Validate_Controller (Property, Values);
               elsif Property in Config_Property_Parameters_Sequence then
                  declare
                     Children   : Config_Property_Maps.Map renames
                       Config_Property_Parameters_Sequence (Property).Children;
                     Child_Name : constant Virtual_String := Path (Next_Index);
                  begin
                     return
                       Children.Contains (Child_Name) and then Match_Property (Children (Child_Name), Next_Index + 1);
                  end;
               elsif Property in Config_Property_Parameters_Variant
                 and then Path (Next_Index) = "Children"
                 and then Next_Index < Path.Last_Index
               then
                  declare
                     Children   : Config_Property_Maps.Map renames
                       Config_Property_Parameters_Variant (Property).Children;
                     Child_Name : constant Virtual_String := Path (Next_Index + 1);
                  begin
                     return
                       Children.Contains (Child_Name) and then Match_Property (Children (Child_Name), Next_Index + 2);
                  end;
               else
                  return False;
               end if;
            end Match_Property;
         begin
            if not Items.Contains (Name) then
               return False;
            end if;

            return Match_Property (Items (Name), Path_Index + 1);
         end Match_Controller;

         procedure Validate_Condition (Condition : Config_Presentation_Condition) is
            Module_Matches     : Natural := 0;
            Controller_Matched : Boolean := False;
         begin
            if Condition.Controller_Tag = Ada.Tags.No_Tag then
               raise Constraint_Error with "Dynamic config presentation controller tag must not be No_Tag.";
            elsif Condition.Controller_Path.Path.Is_Empty then
               raise Constraint_Error with "Dynamic config presentation controller path must not be empty.";
            end if;

            for Module_Cursor in Schemas.Iterate loop
               if Config_Schema_Maps.Element (Module_Cursor).Module_Instance_Tag = Condition.Controller_Tag then
                  Module_Matches := Module_Matches + 1;
                  Controller_Matched :=
                    Match_Controller
                      (Config_Schema_Maps.Element (Module_Cursor).Top_Level_Items,
                       Condition.Controller_Path.Path,
                       Condition.Controller_Path.Path.First_Index,
                       Condition.Values);
               end if;
            end loop;

            if Module_Matches = 0 then
               raise Constraint_Error
                 with
                   "Dynamic config presentation controller module does not exist (Path: "
                   & Condition.Controller_Path.Path'Image
                   & ").";
            elsif Module_Matches > 1 then
               raise Constraint_Error
                 with
                   "Dynamic config presentation controller module tag is ambiguous (Path: "
                   & Condition.Controller_Path.Path'Image
                   & ").";
            elsif not Controller_Matched then
               raise Constraint_Error
                 with
                   "Dynamic config presentation controller does not exist (Path: "
                   & Condition.Controller_Path.Path'Image
                   & ").";
            end if;
         end Validate_Condition;

         procedure Validate_Children (Children : Config_Property_Maps.Map) is
         begin
            for Child in Children.Iterate loop
               Validate_Property (Config_Property_Maps.Element (Child));
            end loop;
         end Validate_Children;

         procedure Validate_Property (Property : Config_Property_Parameters'Class) is
         begin
            if Property.Present_When.Controller_Tag /= Ada.Tags.No_Tag
              or else not Property.Present_When.Controller_Path.Path.Is_Empty
              or else not Property.Present_When.Values.Is_Empty
            then
               Validate_Condition (Property.Present_When);
            end if;

            if Property in Config_Property_Parameters_Sequence then
               Validate_Children (Config_Property_Parameters_Sequence (Property).Children);
            elsif Property in Config_Property_Parameters_Variant then
               Validate_Children (Config_Property_Parameters_Variant (Property).Children);
            end if;
         end Validate_Property;
      begin
         for Module_Cursor in Schemas.Iterate loop
            Validate_Children (Config_Schema_Maps.Element (Module_Cursor).Top_Level_Items);
         end loop;
      end Validate_Presentation_Conditions;

      function Outer_Generate (Property : Config_Property_Parameters'Class) return JSON_Value;

      function Outer_Generate (Property : Config_Property_Parameters'Class) return JSON_Value is
         Result : constant JSON_Value := Create_Object;
      begin
         Result.Set_Field ("Description", Property.Description);

         if Property.Present_When.Controller_Tag /= Ada.Tags.No_Tag then
            declare
               Parameters      : constant JSON_Value := Create_Object;
               Controller_Path : JSON_Array := Empty_Array;
               Values          : JSON_Array := Empty_Array;
            begin
               for Segment of Property.Present_When.Controller_Path.Path loop
                  Controller_Path.Append (Create (Segment));
               end loop;
               if Controller_Is_Variant (Property.Present_When) then
                  Controller_Path.Append (Create ("Selected"));
               end if;
               for Value of Property.Present_When.Values loop
                  Values.Append (Create (Value));
               end loop;
               Parameters.Set_Field ("Owner", Resolve_Controller_Owner (Property.Present_When));
               Parameters.Set_Field ("Path", Controller_Path);
               Parameters.Set_Field ("Values", Values);
               Result.Set_Field ("Present_When", Parameters);
            end;
         end if;

         if Property in Config_Property_Parameters_Boolean then
            Result.Set_Field ("Kind", "Boolean");
            Result.Set_Field ("Default", Config_Property_Parameters_Boolean (Property).Default);
         elsif Property in Config_Property_Parameters_Discrete then
            Result.Set_Field ("Kind", "Discrete");
            Result.Set_Field ("Default", Config_Property_Parameters_Discrete (Property).Default);
            declare
               Arr : JSON_Array := Empty_Array;
            begin
               for Opt of Config_Property_Parameters_Discrete (Property).Options loop
                  Arr.Append (Create (Opt));
               end loop;
               Result.Set_Field ("Options", Arr);
            end;
         elsif Property in Config_Property_Parameters_String then
            Result.Set_Field ("Kind", "String");
            Result.Set_Field ("Default", Config_Property_Parameters_String (Property).Default);
         elsif Property in Config_Property_Parameters_Integer then
            Result.Set_Field ("Kind", "Integer");
            Result.Set_Field ("Min", Create (Config_Property_Parameters_Integer (Property).Min));
            Result.Set_Field ("Max", Create (Config_Property_Parameters_Integer (Property).Max));
            Result.Set_Field ("Unit", Config_Property_Parameters_Integer (Property).Unit);
            Result.Set_Field ("Default", Create (Config_Property_Parameters_Integer (Property).Default));
         elsif Property in Config_Property_Parameters_Float then
            Result.Set_Field ("Kind", "Float");
            Set_Field (Result, "Min", Config_Property_Parameters_Float (Property).Min);
            Set_Field (Result, "Max", Config_Property_Parameters_Float (Property).Max);
            Set_Field (Result, "Unit", Config_Property_Parameters_Float (Property).Unit);
            Set_Field (Result, "Default", Config_Property_Parameters_Float (Property).Default);
         elsif Property in Config_Property_Parameters_Float_Ratio then
            Result.Set_Field ("Kind", "Float_Ratio");
            Set_Field (Result, "Min", Config_Property_Parameters_Float_Ratio (Property).Min);
            Set_Field (Result, "Max", Config_Property_Parameters_Float_Ratio (Property).Max);
            Set_Field
              (Result, "Default_Numerator", Config_Property_Parameters_Float_Ratio (Property).Default.Numerator);
            Set_Field
              (Result, "Default_Denominator", Config_Property_Parameters_Float_Ratio (Property).Default.Denominator);
         elsif Property in Config_Property_Parameters_Sequence then
            Result.Set_Field ("Kind", "Sequence");
            Result.Set_Field ("Tabbed", Config_Property_Parameters_Sequence (Property).Tabbed);
            Result.Set_Field ("Children", Create_Object);
            for C in Config_Property_Parameters_Sequence (Property).Children.Iterate loop
               Result.Get ("Children").Set_Field
                 (Config_Property_Maps.Key (C), Outer_Generate (Config_Property_Maps.Element (C)));
            end loop;
         elsif Property in Config_Property_Parameters_Variant then
            Result.Set_Field ("Kind", "Variant");
            Result.Set_Field ("Default", Config_Property_Parameters_Variant (Property).Default);
            Result.Set_Field ("Children", Create_Object);
            for C in Config_Property_Parameters_Variant (Property).Children.Iterate loop
               Result.Get ("Children").Set_Field
                 (Config_Property_Maps.Key (C), Outer_Generate (Config_Property_Maps.Element (C)));
            end loop;
         else
            raise Constraint_Error with "Unhandled property type (" & Property'Tag'Image & ").";
         end if;

         return Result;
      end Outer_Generate;

      Result : constant JSON_Value := Create_Object;
   begin
      Validate_Presentation_Conditions;

      Result.Set_Field ("Prunt config version", Create (Long_Long_Integer'(1)));
      Result.Set_Field ("Config", Create_Object);

      for M in Schemas.Iterate loop
         declare
            Module_Result : constant JSON_Value := Create_Object;
            Schema_Ver    : constant Versioned_Config_Schema'Class := Config_Schema_Maps.Element (M);
         begin
            Module_Result.Set_Field ("Version", Integer (Schema_Ver.Version));
            Module_Result.Set_Field ("Config", Create_Object);

            for P in Schema_Ver.Top_Level_Items.Iterate loop
               Module_Result.Get ("Config").Set_Field
                 (Config_Property_Maps.Key (P), Outer_Generate (Config_Property_Maps.Element (P)));
            end loop;

            Result.Get ("Config").Set_Field (Config_Schema_Maps.Key (M), Module_Result);
         end;
      end loop;

      return Write (Result);
   end Generate_Schemas_String;

   function Is_Path_Prefix (Prefix : Config_Data_Paths.Vector; Path : Config_Data_Paths.Vector) return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      if Prefix.Length > Path.Length then
         return False;
      end if;

      if Prefix.Is_Empty then
         return True;
      end if;

      for Offset in 0 .. Natural (Prefix.Length) - 1 loop
         if Prefix.Element (Prefix.First_Index + Offset) /= Path.Element (Path.First_Index + Offset) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Path_Prefix;

   function Paths_Overlap (Left : Config_Data_Paths.Vector; Right : Config_Data_Paths.Vector) return Boolean is
   begin
      return Is_Path_Prefix (Left, Right) or else Is_Path_Prefix (Right, Left);
   end Paths_Overlap;

   function Path_Equals_Override
     (Owner : Virtual_String; Path : Config_Data_Paths.Vector; Overrides : Config_Override_Vectors.Vector)
      return Boolean
   is
      use type Config_Data_Paths.Vector;
   begin
      for Override of Overrides loop
         if Override.Owner = Owner and then Override.Path = Path then
            return True;
         end if;
      end loop;

      return False;
   end Path_Equals_Override;

   function Path_Overlaps_Overrides
     (Owner : Virtual_String; Path : Config_Data_Paths.Vector; Overrides : Config_Override_Vectors.Vector)
      return Boolean is
   begin
      for Override of Overrides loop
         if Override.Owner = Owner and then Paths_Overlap (Path, Override.Path) then
            return True;
         end if;
      end loop;

      return False;
   end Path_Overlaps_Overrides;

   function Unset_JSON_Node (Root : JSON_Value; Path : Config_Data_Paths.Vector) return Boolean is
      use Config_Data_Paths;

      Current_Node : JSON_Value := Root;
   begin
      if Path.Is_Empty or else Root.Kind /= JSON_Object_Type then
         return False;
      end if;

      for I in Path.First_Index .. Path.Last_Index - 1 loop
         if not Current_Node.Has_Field (Path.Element (I)) then
            return False;
         end if;

         Current_Node := Current_Node.Get (Path.Element (I));

         if Current_Node.Kind /= JSON_Object_Type then
            return False;
         end if;
      end loop;

      if Current_Node.Has_Field (Path.Last_Element) then
         Current_Node.Unset_Field (Path.Last_Element);
         return True;
      else
         return False;
      end if;
   end Unset_JSON_Node;

   function Path_Without_Last (Path : Config_Data_Paths.Vector) return Config_Data_Paths.Vector is
      use type Ada.Containers.Count_Type;

      Result : Config_Data_Paths.Vector;
   begin
      if Path.Length < 2 then
         return Result;
      end if;

      for I in Path.First_Index .. Path.Last_Index - 1 loop
         Result.Append (Path.Element (I));
      end loop;

      return Result;
   end Path_Without_Last;

   function Selected_Variant_Default
     (Schema : Config_Property_Maps.Map; Path : Config_Data_Paths.Vector; Default_Value : out JSON_Value)
      return Boolean
   is
      use type Ada.Containers.Count_Type;

      Current_Schema : Config_Property_Maps.Map := Schema;
      I              : Positive;
   begin
      Default_Value := JSON_Null;

      if Path.Length < 2 or else Path.Last_Element /= "Selected" then
         return False;
      end if;

      I := Path.First_Index;
      loop
         if not Current_Schema.Contains (Path.Element (I)) then
            return False;
         end if;

         declare
            Property : constant Config_Property_Parameters'Class := Current_Schema.Element (Path.Element (I));
         begin
            if I = Path.Last_Index - 1 then
               if Property in Config_Property_Parameters_Variant then
                  Default_Value := Create_Default_Property_Config (Property);
                  return True;
               else
                  return False;
               end if;
            elsif Property in Config_Property_Parameters_Sequence then
               Current_Schema := Config_Property_Parameters_Sequence (Property).Children;
               I := I + 1;
            elsif Property in Config_Property_Parameters_Variant then
               declare
                  Variant : constant Config_Property_Parameters_Variant :=
                    Config_Property_Parameters_Variant (Property);
               begin
                  if Path.Element (I + 1) /= "Children"
                    or else I + 2 > Path.Last_Index - 1
                    or else not Variant.Children.Contains (Path.Element (I + 2))
                  then
                     return False;
                  end if;

                  Current_Schema := Variant.Children;
                  I := I + 2;
               end;
            else
               return False;
            end if;
         end;
      end loop;
   end Selected_Variant_Default;

   function Prune_Path_For_Override
     (Schema : Config_Property_Maps.Map; Path : Config_Data_Paths.Vector) return Config_Data_Paths.Vector
   is
      Default_Value : JSON_Value;
   begin
      if Selected_Variant_Default (Schema, Path, Default_Value) then
         return Path_Without_Last (Path);
      else
         return Path;
      end if;
   end Prune_Path_For_Override;

   function Try_Get_JSON_Node
     (Root : JSON_Value; Path : Config_Data_Paths.Vector; Result : out JSON_Value) return Boolean
   is
      Current_Node : JSON_Value := Root;
   begin
      Result := JSON_Null;

      if Path.Is_Empty or else Current_Node.Kind /= JSON_Object_Type then
         return False;
      end if;

      for Key of Path loop
         if not Current_Node.Has_Field (Key) then
            return False;
         end if;

         Current_Node := Current_Node.Get (Key);
      end loop;

      Result := Current_Node;
      return True;
   end Try_Get_JSON_Node;

   procedure Merge_Default_JSON_Node (Root : JSON_Value; Path : Config_Data_Paths.Vector; Default_Value : JSON_Value)
   is
      Existing_Value : JSON_Value;
   begin
      if Try_Get_JSON_Node (Root, Path, Existing_Value) then
         if Default_Value.Kind = JSON_Object_Type and then Existing_Value.Kind = JSON_Object_Type then
            declare
               Merged_Value : constant JSON_Value := Clone (Default_Value);
            begin
               Recursive_Left_Merge (Merged_Value, Existing_Value);
               Set_JSON_Node (Root, Path, Merged_Value);
            end;
         end if;
      else
         Set_JSON_Node (Root, Path, Clone (Default_Value));
      end if;
   end Merge_Default_JSON_Node;

   procedure Apply_Overrides_To_Config
     (Config : JSON_Value; Schemas : Config_Schema_Maps.Map; Overrides : Config_Override_Vectors.Vector) is
   begin
      for Override of Overrides loop
         if Schemas.Contains (Override.Owner) then
            declare
               Default_Value : JSON_Value;
            begin
               if Selected_Variant_Default
                    (Schemas (Override.Owner).Element.Top_Level_Items, Override.Path, Default_Value)
               then
                  Merge_Default_JSON_Node
                    (Config.Get ("Config").Get (Override.Owner).Get ("Config"),
                     Path_Without_Last (Override.Path),
                     Default_Value);
               end if;
            end;
         end if;
      end loop;

      for Override of Overrides loop
         Set_JSON_Node
           (Config.Get ("Config").Get (Override.Owner).Get ("Config"), Override.Path, Clone (Override.Value));
      end loop;
   end Apply_Overrides_To_Config;

   function Prune_Overrides_From_Module_Config
     (Owner         : Virtual_String;
      Module_Config : JSON_Value;
      Module_Schema : Config_Property_Maps.Map;
      Overrides     : Config_Override_Vectors.Vector) return Boolean
   is
      Changed : Boolean := False;
   begin
      for Override of Overrides loop
         if Override.Owner = Owner
           and then Unset_JSON_Node (Module_Config, Prune_Path_For_Override (Module_Schema, Override.Path))
         then
            Changed := True;
         end if;
      end loop;

      return Changed;
   end Prune_Overrides_From_Module_Config;

   function Prune_Overrides_From_Config
     (Config : JSON_Value; Schemas : Config_Schema_Maps.Map; Overrides : Config_Override_Vectors.Vector) return Boolean
   is
      Changed : Boolean := False;
   begin
      for Override of Overrides loop
         if Config.Get ("Config").Has_Field (Override.Owner)
           and then Schemas.Contains (Override.Owner)
           and then
             Prune_Overrides_From_Module_Config
               (Override.Owner,
                Config.Get ("Config").Get (Override.Owner).Get ("Config"),
                Schemas (Override.Owner).Element.Top_Level_Items,
                Overrides)
         then
            Changed := True;
         end if;
      end loop;

      return Changed;
   end Prune_Overrides_From_Config;

   function Prune_Overrides_From_Schemas
     (Schemas : Config_Schema_Maps.Map; Overrides : Config_Override_Vectors.Vector) return Config_Schema_Maps.Map
   is
      Result : Config_Schema_Maps.Map := Schemas;

      procedure Remove_From_Property
        (Property : in out Config_Property_Parameters'Class; Path : Config_Data_Paths.Vector; Index : Positive);
      --  Removes the schema subtree addressed by Path, starting at Index, from a nested property.

      procedure Remove_From_Schema_Map
        (Schema : in out Config_Property_Maps.Map; Path : Config_Data_Paths.Vector; Index : Positive);
      --  Removes the schema entry addressed by Path, starting at Index, from a schema map.

      procedure Remove_From_Property
        (Property : in out Config_Property_Parameters'Class; Path : Config_Data_Paths.Vector; Index : Positive) is
      begin
         if Property in Config_Property_Parameters_Sequence then
            Remove_From_Schema_Map (Config_Property_Parameters_Sequence (Property).Children, Path, Index);
         elsif Property in Config_Property_Parameters_Variant then
            declare
               Variant : Config_Property_Parameters_Variant renames Config_Property_Parameters_Variant (Property);
            begin
               if Path.Element (Index) = "Selected" then
                  Variant.Children := [];
               elsif Path.Element (Index) = "Children" then
                  if Index = Path.Last_Index then
                     Variant.Children := [];
                  elsif Variant.Children.Contains (Path.Element (Index + 1)) then
                     if Index + 1 = Path.Last_Index then
                        Variant.Children.Delete (Path.Element (Index + 1));
                     else
                        declare
                           Child : Config_Property_Parameters'Class renames
                             Variant.Children.Reference (Path.Element (Index + 1));
                        begin
                           Remove_From_Property (Child, Path, Index + 2);
                        end;
                     end if;
                  end if;
               end if;
            end;
         end if;
      end Remove_From_Property;

      procedure Remove_From_Schema_Map
        (Schema : in out Config_Property_Maps.Map; Path : Config_Data_Paths.Vector; Index : Positive)
      is
         Key : constant Virtual_String := Path.Element (Index);
      begin
         if not Schema.Contains (Key) then
            return;
         end if;

         if Index = Path.Last_Index then
            Schema.Delete (Key);
            return;
         end if;

         if Schema.Element (Key) in Config_Property_Parameters_Variant and then Path.Element (Index + 1) = "Selected"
         then
            Schema.Delete (Key);
            return;
         end if;

         declare
            Property : Config_Property_Parameters'Class renames Schema.Reference (Key);
         begin
            Remove_From_Property (Property, Path, Index + 1);
         end;
      end Remove_From_Schema_Map;
   begin
      for Override of Overrides loop
         if Result.Contains (Override.Owner) and then not Override.Path.Is_Empty then
            declare
               Module_Schema : Versioned_Config_Schema'Class := Result.Element (Override.Owner);
            begin
               Remove_From_Schema_Map (Module_Schema.Top_Level_Items, Override.Path, Override.Path.First_Index);
               Result.Replace (Override.Owner, Module_Schema);
            end;
         end if;
      end loop;

      return Result;
   end Prune_Overrides_From_Schemas;

   procedure Validate_Overrides (Schemas : Config_Schema_Maps.Map; Overrides : Config_Override_Vectors.Vector) is
      procedure Raise_Error (Path : Config_Data_Paths.Vector; Message : Virtual_String);
      --  Raises Constraint_Error with a message identifying an invalid override value.

      procedure Raise_Error (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
      begin
         raise Constraint_Error
           with "Invalid config override: " & Conversions.To_UTF_8_String (Message) & " (Path: " & Path'Image & ")";
      end Raise_Error;
   begin
      for Override of Overrides loop
         if Override.Path.Is_Empty then
            raise Constraint_Error with "Invalid config override: path must not be empty.";
         elsif not Schemas.Contains (Override.Owner) then
            raise Constraint_Error
              with "Invalid config override: unknown module " & Conversions.To_UTF_8_String (Override.Owner) & ".";
         end if;
      end loop;

      if not Overrides.Is_Empty then
         for I in Overrides.First_Index .. Overrides.Last_Index loop
            for J in I + 1 .. Overrides.Last_Index loop
               if Overrides.Element (I).Owner = Overrides.Element (J).Owner
                 and then Paths_Overlap (Overrides.Element (I).Path, Overrides.Element (J).Path)
               then
                  raise Constraint_Error
                    with
                      "Invalid config override: overlapping paths for module "
                      & Conversions.To_UTF_8_String (Overrides.Element (I).Owner)
                      & ".";
               end if;
            end loop;
         end loop;
      end if;

      for Override of Overrides loop
         declare
            Module_Config : constant JSON_Value :=
              Create_Default_Module_Config (Schemas (Override.Owner).Element.Top_Level_Items);
         begin
            Set_JSON_Node (Module_Config, Override.Path, Clone (Override.Value));
            Validate_Module_Config_To_Schema
              (Module_Config,
               Schemas (Override.Owner).Element.Top_Level_Items,
               Raise_Error'Access,
               Check_For_Missing_Fields => True);
         end;
      end loop;
   end Validate_Overrides;

   procedure Validate_No_Overrides_In_Patch
     (Owner     : Virtual_String;
      Value     : JSON_Value;
      Overrides : Config_Override_Vectors.Vector;
      Report    : access procedure (Path : Config_Data_Paths.Vector; Message : Virtual_String))
   is
      use Config_Data_Paths;

      procedure Recursive_Check (Path : Config_Data_Paths.Vector; Node : JSON_Value);
      --  Walks a patch subtree and reports any path which overlaps an override.

      procedure Recursive_Check (Path : Config_Data_Paths.Vector; Node : JSON_Value) is
         procedure Check_Child (Name : Virtual_String; Child : JSON_Value);
         --  Continues the recursive patch check for one object member.

         procedure Check_Child (Name : Virtual_String; Child : JSON_Value) is
         begin
            Recursive_Check (Path & Name, Child);
         end Check_Child;
      begin
         if Node.Kind = JSON_Object_Type and then not Path_Equals_Override (Owner, Path, Overrides) then
            Node.Map_JSON_Object (Check_Child'Access);
         elsif Path_Overlaps_Overrides (Owner, Path, Overrides) then
            Report (Path, "Field is overridden and cannot be changed.");
         end if;
      end Recursive_Check;
   begin
      Recursive_Check ([], Value);
   end Validate_No_Overrides_In_Patch;

   protected body Config_File_Internal is
      procedure Initialize
        (File_Name_In : String;
         Schemas_In   : Config_Schema_Maps.Map;
         Overrides_In : Config_Override_Vectors.Vector;
         Migrate      :
           access function
             (Module : Virtual_String; Old_Version : Config_Schema_Version; Old_Config : JSON_Value) return JSON_Value;
         Lock         : File_Access_Lock.Lock_Holder := File_Access_Lock.Lock)
      is
         pragma Unreferenced (Lock);

         procedure Raise_Error (Path : Config_Data_Paths.Vector; Message : Virtual_String);

         procedure Raise_Error (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
         begin
            raise Constraint_Error
              with
                "Config file format error: " & Conversions.To_UTF_8_String (Message) & " (Path: " & Path'Image & ")";
         end Raise_Error;

         Write_Required : Boolean := False;
      begin
         File_Name := Conversions.To_Virtual_String (File_Name_In);
         Schemas := Schemas_In;
         Overrides := Overrides_In;
         Validate_Overrides (Schemas, Overrides);
         Visible_Schemas := Prune_Overrides_From_Schemas (Schemas, Overrides);

         declare
            Global_Schema : Config_Property_Maps.Map;

            procedure Merge_Schemas
              (Result : in out Config_Property_Maps.Map;
               Source : Config_Property_Maps.Map;
               Path   : Config_Data_Paths.Vector);

            procedure Merge_Schemas
              (Result : in out Config_Property_Maps.Map;
               Source : Config_Property_Maps.Map;
               Path   : Config_Data_Paths.Vector)
            is
               use type Config_Data_Paths.Vector;
            begin
               for C in Source.Iterate loop
                  declare
                     Key : constant Virtual_String := Config_Property_Maps.Key (C);
                  begin
                     if Result.Contains (Key) then
                        declare
                           use Config_Property_Maps;
                           use type Ada.Tags.Tag;
                           New_Parameters      : constant Config_Property_Parameters'Class :=
                             Config_Property_Maps.Element (C);
                           Existing_Parameters : Config_Property_Parameters'Class renames Result.Reference (Key);
                        begin
                           if Existing_Parameters'Tag /= New_Parameters'Tag then
                              raise Constraint_Error
                                with "Conflicting config schema keys at " & Path'Image & " (different tags).";
                           elsif Existing_Parameters in Config_Property_Parameters_Sequence then
                              --  TODO: Check that there is only one description or that descriptions are equal.
                              --  TODO: Check that tabbed field matches.
                              Merge_Schemas
                                (Config_Property_Parameters_Sequence (Existing_Parameters).Children,
                                 Config_Property_Parameters_Sequence (New_Parameters).Children,
                                 Path & Key);
                           else
                              raise Constraint_Error
                                with "Conflicting config schema keys at " & Path'Image & " (tags can not be merged).";
                           end if;
                        end;
                     else
                        Result.Insert (Key, Config_Property_Maps.Element (C));
                     end if;
                  end;
               end loop;
            end Merge_Schemas;
         begin
            for M in Schemas.Iterate loop
               Merge_Schemas
                 (Global_Schema,
                  Config_Schema_Maps.Element (M).Top_Level_Items,
                  ["Config", Config_Schema_Maps.Key (M), "Config"]);
            end loop;
         end;

         if Mockable.Directories.Exists (Conversions.To_UTF_8_String (File_Name)) then
            declare
               File          : Mockable.Text_IO.File_Type;
               File_Contents : Virtual_String;
            begin
               Mockable.Text_IO.Open (File, Mockable.Text_IO.In_File, Conversions.To_UTF_8_String (File_Name));
               while not Mockable.Text_IO.End_Of_File (File) loop
                  File_Contents.Append (Conversions.To_Virtual_String (Mockable.Text_IO.Unbounded_IO.Get_Line (File)));
               end loop;
               Mockable.Text_IO.Close (File);

               Live_Config := Read (Strm => File_Contents, Filename => Conversions.To_UTF_8_String (File_Name));
            end;
         else
            Live_Config := Create_Object;
            Set_Field (Live_Config, "Prunt config version", Long_Integer'(1));
            Set_Field (Live_Config, "Config", Create_Object);
            Write_Required := True;
         end if;

         Validate_Outer_Config_Structure (Live_Config, Raise_Error'Access);

         for M in Schemas.Iterate loop
            declare
               Module_Name   : constant Virtual_String := Config_Schema_Maps.Key (M);
               Module_Schema : constant Versioned_Config_Schema'Class := Config_Schema_Maps.Element (M);

               procedure Raise_Error_For_Module (Path : Config_Data_Paths.Vector; Message : Virtual_String);

               procedure Raise_Error_For_Module (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
                  use Config_Data_Paths;
               begin
                  raise Constraint_Error
                    with
                      "Config file format error: "
                      & Message'Image
                      & " (Path: "
                      & Vector'Image (["Config", Module_Name] & Path)
                      & ")";
               end Raise_Error_For_Module;
            begin
               if Live_Config.Get ("Config").Has_Field (Module_Name) then
                  Validate_Module_Config_Structure
                    (Live_Config.Get ("Config").Get (Module_Name), Raise_Error_For_Module'Access);
                  if Live_Config.Get ("Config").Get (Module_Name).Get ("Version") > Integer (Module_Schema.Version)
                  then
                     raise Constraint_Error
                       with
                         Conversions.To_UTF_8_String ("Module config is for a newer version (" & Module_Name & ").");
                  elsif Live_Config.Get ("Config").Get (Module_Name).Get ("Version") < Integer (Module_Schema.Version)
                  then
                     declare
                        Old_Module_Config : constant JSON_Value :=
                          Clone (Live_Config.Get ("Config").Get (Module_Name).Get ("Config"));
                        Mid_Module_Config : constant JSON_Value :=
                          Create_Default_Module_Config (Module_Schema.Top_Level_Items);
                        New_Module_Config : constant JSON_Value :=
                          Create_Default_Module_Config (Module_Schema.Top_Level_Items);
                     begin
                        if Prune_Overrides_From_Module_Config
                             (Module_Name, Old_Module_Config, Module_Schema.Top_Level_Items, Overrides)
                        then
                           Write_Required := True;
                        end if;
                        Recursive_Left_Merge (Mid_Module_Config, Old_Module_Config);
                        --  Adds any new fields in the schema with default values.
                        Live_Config.Get ("Config").Get (Module_Name).Set_Field ("Config", Mid_Module_Config);

                        declare
                           Migrated_Config : constant JSON_Value :=
                             Migrate
                               (Module_Name,
                                Config_Schema_Version
                                  (Integer'(Live_Config.Get ("Config").Get (Module_Name).Get ("Version"))),
                                Mid_Module_Config);
                        begin
                           Recursive_Left_Merge (New_Module_Config, Migrated_Config, Full_Join => False);
                        end;

                        --  Removes any fields not present in the new schema.
                        Live_Config.Get ("Config").Get (Module_Name).Set_Field ("Config", New_Module_Config);
                     end;

                     Live_Config.Get ("Config").Get (Module_Name).Set_Field
                       ("Version", Integer (Module_Schema.Version));
                     Write_Required := True;
                  end if;
               else
                  Live_Config.Get ("Config").Set_Field (Module_Name, Create_Object);
                  Live_Config.Get ("Config").Get (Module_Name).Set_Field ("Version", Integer (Module_Schema.Version));
                  Live_Config.Get ("Config").Get (Module_Name).Set_Field
                    ("Config", Create_Default_Module_Config (Module_Schema.Top_Level_Items));
                  Write_Required := True;
               end if;

            end;
         end loop;

         declare
            procedure Check_For_Extra_Modules (Name : Virtual_String; Value : JSON_Value);

            procedure Check_For_Extra_Modules (Name : Virtual_String; Value : JSON_Value) is
               pragma Unreferenced (Value);
            begin
               if not Schemas.Contains (Name) then
                  raise Constraint_Error with Conversions.To_UTF_8_String ("Unknown config module (" & Name & ").");
               end if;
            end Check_For_Extra_Modules;
         begin
            Live_Config.Get ("Config").Map_JSON_Object (Check_For_Extra_Modules'Access);
         end;

         Stored_Config := Live_Config.Clone;

         if Prune_Overrides_From_Config (Stored_Config, Schemas, Overrides) then
            Write_Required := True;
         end if;

         declare
            procedure Validate_Module (Config_To_Check : JSON_Value; Validation_Schemas : Config_Schema_Maps.Map);
            --  Validates each module in Config_To_Check against the corresponding schema in Validation_Schemas.

            procedure Validate_Module (Config_To_Check : JSON_Value; Validation_Schemas : Config_Schema_Maps.Map) is
            begin
               for M in Validation_Schemas.Iterate loop
                  declare
                     Module_Name   : constant Virtual_String := Config_Schema_Maps.Key (M);
                     Module_Schema : constant Versioned_Config_Schema'Class := Config_Schema_Maps.Element (M);

                     procedure Raise_Error_For_Module (Path : Config_Data_Paths.Vector; Message : Virtual_String);
                     --  Raises a schema validation error with the module name prefixed onto Path.

                     procedure Raise_Error_For_Module (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
                        use Config_Data_Paths;
                     begin
                        raise Constraint_Error
                          with
                            "Config file format error: "
                            & Message'Image
                            & " (Path: "
                            & Vector'Image (["Config", Module_Name, "Config"] & Path)
                            & ")";
                     end Raise_Error_For_Module;
                  begin
                     Validate_Module_Config_To_Schema
                       (Config_To_Check.Get ("Config").Get (Module_Name).Get ("Config"),
                        Module_Schema.Top_Level_Items,
                        Raise_Error_For_Module'Access,
                        Check_For_Missing_Fields => True);
                  end;
               end loop;
            end Validate_Module;
         begin
            Validate_Module (Stored_Config, Visible_Schemas);

            Live_Config := Stored_Config.Clone;
            Apply_Overrides_To_Config (Live_Config, Schemas, Overrides);
            Validate_Module (Live_Config, Schemas);
         end;

         if Write_Required then
            Write_File;
         end if;

         Cached_Schemas := Generate_Schemas_String (Visible_Schemas);
      end Initialize;

      function Get (Owner : Virtual_String; Path : Config_Data_Paths.Vector) return JSON_Value is
      begin
         if not Schemas.Contains (Owner) then
            raise Constraint_Error with Conversions.To_UTF_8_String ("Invalid config module (" & Owner & ").");
         end if;

         if Path.Is_Empty then
            raise Constraint_Error with "Invalid path (error in module).";
         end if;

         return Get_JSON_Node (Live_Config.Get ("Config").Get (Owner).Get ("Config"), Path, Owner);
      end Get;

      procedure Set (Owner : Virtual_String; Path : Config_Data_Paths.Vector; Value : JSON_Value) is
      begin
         if not Schemas.Contains (Owner) then
            raise Constraint_Error with Conversions.To_UTF_8_String ("Invalid config module (" & Owner & ").");
         end if;

         if Path.Is_Empty then
            raise Constraint_Error with "Invalid path (error in module).";
         end if;

         if Path_Overlaps_Overrides (Owner, Path, Overrides) then
            raise Constraint_Error
              with
                "Config field is overridden and cannot be changed (Module: "
                & Conversions.To_UTF_8_String (Owner)
                & ", Path: "
                & Path'Image
                & ").";
         end if;

         if not Update_Deltas.Contains (Owner) then
            Update_Deltas.Insert (Owner, Create_Object);
         end if;

         Set_JSON_Node (Live_Config.Get ("Config").Get (Owner).Get ("Config"), Path, Value);

         Set_JSON_Node (Update_Deltas (Owner), Path, Value);
      end Set;

      procedure Save (Owner : Virtual_String; Lock : File_Access_Lock.Lock_Holder := File_Access_Lock.Lock) is
         pragma Unreferenced (Lock);

         procedure Raise_Error_For_Module (Path : Config_Data_Paths.Vector; Message : Virtual_String);
         --  Raise a schema validation error with Owner prefixed onto Path.

         procedure Raise_Error_For_Module (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
            use Config_Data_Paths;
         begin
            raise Constraint_Error
              with
                "Config file format error: "
                & Message'Image
                & " (Path: "
                & Vector'Image (["Config", Owner] & Path)
                & ")";
         end Raise_Error_For_Module;

         Live_Config_Clone   : constant JSON_Value := Clone (Live_Config.Get ("Config").Get (Owner).Get ("Config"));
         Stored_Config_Clone : constant JSON_Value := Clone (Stored_Config.Get ("Config").Get (Owner).Get ("Config"));
      begin
         if not Update_Deltas.Contains (Owner) then
            return;
         end if;

         Recursive_Left_Merge (Live_Config_Clone, Update_Deltas (Owner));
         Recursive_Left_Merge (Stored_Config_Clone, Update_Deltas (Owner));

         Update_Deltas.Delete (Owner);

         --  Workaround below for https://gcc.gnu.org/bugzilla/show_bug.cgi?id=123185
         Validate_Module_Config_To_Schema
           (Live_Config_Clone,
            Schemas (Owner).Element.Top_Level_Items,
            Raise_Error_For_Module'Access,
            Check_For_Missing_Fields => True);
         Validate_Module_Config_To_Schema
           (Stored_Config_Clone,
            Visible_Schemas (Owner).Element.Top_Level_Items,
            Raise_Error_For_Module'Access,
            Check_For_Missing_Fields => True);

         Live_Config.Get ("Config").Get (Owner).Set_Field ("Config", Live_Config_Clone);
         Stored_Config.Get ("Config").Get (Owner).Set_Field ("Config", Stored_Config_Clone);
         --  We use a clone here to avoid Stored_Config ever being in an invalid state. This is to avoid the case
         --  where a module catches the Constraint_Error raised above then another module could still save after that
         --  point, in that case an invalid Stored_Config would be saved to a file.

         Write_File;
      end Save;

      procedure Apply_Untrusted_Patch
        (Value  : Virtual_String;
         Result : out Virtual_String;
         Errors : out Config_Error_Vectors.Vector;
         Lock   : File_Access_Lock.Lock_Holder := File_Access_Lock.Lock)
      is
         pragma Unreferenced (Lock);

         use Config_Data_Paths;

         Patch : JSON_Value;

         Error_Reported : Boolean := False;

         procedure Report (Path : Config_Data_Paths.Vector; Message : Virtual_String);

         procedure Report (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
         begin
            Errors.Append (Config_Error'(Path, Message));
            Error_Reported := True;
         end Report;
      begin
         Errors := [];

         declare
            JSON_Result : constant Read_Result := Read (Value);
         begin
            case JSON_Result.Success is
               when True  =>
                  Patch := JSON_Result.Value;

               when False =>
                  Report
                    ([],
                     Conversions.To_Virtual_String
                       ("Invalid JSON stream at "
                        & JSON_Result.Error.Line'Image
                        & ":"
                        & JSON_Result.Error.Column'Image
                        & ": ")
                     & JSON_Result.Error.Message);
                  Result := Get_Stored_Config;
                  return;
            end case;
         end;

         if Patch.Is_Empty then
            Result := Get_Stored_Config;
            return;
         end if;

         Validate_Outer_Config_Structure (Patch, Report'Access);
         if Error_Reported then
            Result := Get_Stored_Config;
            return;
         end if;

         declare
            procedure Handle_Module (Name : Virtual_String; Value : JSON_Value);

            procedure Handle_Module (Name : Virtual_String; Value : JSON_Value) is
               Error_Reported_Inner : Boolean := False;

               procedure Report_Inner (Path : Config_Data_Paths.Vector; Message : Virtual_String);
               procedure Report_Inner_Config (Path : Config_Data_Paths.Vector; Message : Virtual_String);

               procedure Report_Inner (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
               begin
                  Report (["Config", Name] & Path, Message);
                  Error_Reported_Inner := True;
               end Report_Inner;

               procedure Report_Inner_Config (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
               begin
                  Report (["Config", Name, "Config"] & Path, Message);
                  Error_Reported_Inner := True;
               end Report_Inner_Config;
            begin
               if not Schemas.Contains (Name) then
                  Report (["Config", Name], "Unknown module.");
                  return;
               end if;

               Validate_Module_Config_Structure (Value, Report_Inner'Access);
               if Error_Reported_Inner then
                  return;
               end if;

               --  Workaround below for https://gcc.gnu.org/bugzilla/show_bug.cgi?id=123185
               if Value.Get ("Version").Get /= Long_Long_Integer (Schemas (Name).Element.Version) then
                  Report (["Config", Name, "Version"], "Wrong version.");
                  return;
               end if;

               Validate_No_Overrides_In_Patch (Name, Value.Get ("Config"), Overrides, Report_Inner_Config'Access);
               if Error_Reported_Inner then
                  return;
               end if;

               --  Workaround below for https://gcc.gnu.org/bugzilla/show_bug.cgi?id=123185
               Validate_Module_Config_To_Schema
                 (Value.Get ("Config"),
                  Visible_Schemas (Name).Element.Top_Level_Items,
                  Report_Inner_Config'Access,
                  Check_For_Missing_Fields => False);
            end Handle_Module;
         begin
            Map_JSON_Object (Patch.Get ("Config"), Handle_Module'Access);
         end;

         if Error_Reported then
            Result := Get_Stored_Config;
            return;
         end if;

         Recursive_Left_Merge (Stored_Config, Patch);
         Write_File;

         Result := Get_Stored_Config;
      end Apply_Untrusted_Patch;

      function Get_Stored_Config return Virtual_String is
      begin
         return Write (Stored_Config);
      end Get_Stored_Config;

      function Get_Schemas return Virtual_String is
      begin
         return Cached_Schemas;
      end Get_Schemas;

      function Last_Save return Save_Counter is
      begin
         return Save_Count;
      end Last_Save;

      procedure Write_File is
         File : Mockable.Text_IO.File_Type;

         function Trim (S : String) return String;

         function Trim (S : String) return String is
         begin
            return Ada.Strings.Fixed.Trim (S, Side => Ada.Strings.Both);
         end Trim;
      begin
         if Mockable.Directories.Exists (Conversions.To_UTF_8_String (File_Name) & "_backup_20") then
            Mockable.Directories.Delete_File (Conversions.To_UTF_8_String (File_Name) & "_backup_20");
         end if;

         for I in reverse 1 .. 19 loop
            if Mockable.Directories.Exists (Conversions.To_UTF_8_String (File_Name) & "_backup_" & Trim (I'Image)) then
               Mockable.Directories.Rename
                 (Old_Name => Conversions.To_UTF_8_String (File_Name) & "_backup_" & Trim (I'Image),
                  New_Name => Conversions.To_UTF_8_String (File_Name) & "_backup_" & Trim (Integer (I + 1)'Image));
            end if;
         end loop;

         if Mockable.Directories.Exists (Conversions.To_UTF_8_String (File_Name)) then
            Mockable.Directories.Rename
              (Old_Name => Conversions.To_UTF_8_String (File_Name),
               New_Name => Conversions.To_UTF_8_String (File_Name) & "_backup_1");
         end if;

         Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Conversions.To_UTF_8_String (File_Name));
         Mockable.Text_IO.Put_Line (File, Conversions.To_UTF_8_String (Write (Stored_Config)));
         Mockable.Text_IO.Close (File);

         pragma Unreferenced (File);

         Save_Count := @ + 1;
      end Write_File;

      procedure Reset_Live_To_Stored (Check_Ref_Count : access procedure) is
      begin
         Check_Ref_Count.all;
         Live_Config := Stored_Config.Clone;
         Apply_Overrides_To_Config (Live_Config, Schemas, Overrides);
      end Reset_Live_To_Stored;
   end Config_File_Internal;

   procedure Reset_Live_To_Stored (This : Config_File) is
      procedure Check_Ref_Count;

      procedure Check_Ref_Count is
         Ref_Count : constant Natural := This.Internal.Get_Refcount;
      begin
         if Ref_Count /= 1 then
            raise Constraint_Error with Ref_Count'Image & " references to config file still exist during reset.";
         end if;
      end Check_Ref_Count;
   begin
      This.Internal.Get.Reset_Live_To_Stored (Check_Ref_Count'Access);
   end Reset_Live_To_Stored;

   overriding
   procedure Finalize (Object : in out Config_File) is
      Internal  : constant Config_File_Internal_Shared_Pointers.Reference_Type := Object.Internal.Get;
      Ref_Count : constant Natural := Object.Internal.Get_Refcount;
   begin
      if Internal.Element /= null then
         --  TODO: Figure out what to do here if there is still any unsaved data left in Update_Deltas.
         if Ref_Count /= 1 then
            raise Constraint_Error
              with Ref_Count'Image & " references to config file still exist during finalisation.";
         end if;

         Object.Internal := Config_File_Internal_Shared_Pointers.Null_Ref;
      end if;
   end Finalize;

   function Create
     (File_Name : String; Schemas : Config_Schema_Maps.Map; Overrides : Config_Override_Vectors.Vector := [])
      return Config_File
   is
      function Make_Config_File_Internal return Config_File_Internal;

      function Make_Config_File_Internal return Config_File_Internal is
      begin
         return Result : Config_File_Internal;
      end Make_Config_File_Internal;
   begin
      return Result : Config_File do
         declare
            function Migrate
              (Module : Virtual_String; Old_Version : Config_Schema_Version; Old_Config : JSON_Value)
               return JSON_Value;

            function Migrate
              (Module : Virtual_String; Old_Version : Config_Schema_Version; Old_Config : JSON_Value) return JSON_Value
            is
               Config : Config_Data :=
                 (For_Migration    => True,
                  Module           => Module,
                  Internal         => Result.Internal,
                  Migration_Config => Clone (Old_Config));
            begin
               Schemas (Module).Migrate (Old_Version, Config);
               return Config.Migration_Config;
            end Migrate;
         begin
            Result.Internal.Set (Make_Config_File_Internal'Access);
            Result.Internal.Get.Initialize (File_Name, Schemas, Overrides, Migrate'Access);
         end;
      end return;
   end Create;

   function Get_Data (This : Config_File; Module_Name : Virtual_String) return Config_Data is
   begin
      return
        Config_Data'
          (For_Migration => False, Module => Module_Name, Internal => This.Internal, Migration_Config => JSON_Null);
   end Get_Data;

   function Get_Schema_String (This : Config_File) return Virtual_String is
   begin
      return This.Internal.Get.Get_Schemas;
   end Get_Schema_String;

   function Get_Data_String (This : Config_File) return Virtual_String is
   begin
      return This.Internal.Get.Get_Stored_Config;
   end Get_Data_String;

   procedure Apply_Untrusted_Patch
     (This   : Config_File;
      Value  : Virtual_String;
      Result : out Virtual_String;
      Errors : out Config_Error_Vectors.Vector) is
   begin
      This.Internal.Get.Apply_Untrusted_Patch (Value, Result, Errors);
   end Apply_Untrusted_Patch;

   function Last_Save (This : Config_File) return Save_Counter is
   begin
      return This.Internal.Get.Last_Save;
   end Last_Save;

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Boolean is
   begin
      if Data.For_Migration then
         return Get (Get_JSON_Node (Data.Migration_Config, Path, Data.Module));
      else
         return Get (Data.Internal.Get.Get (Data.Module, Path));
      end if;
   end Get;

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Long_Float is
   begin
      if Data.For_Migration then
         return Get (Get_JSON_Node (Data.Migration_Config, Path, Data.Module));
      else
         return Get (Data.Internal.Get.Get (Data.Module, Path));
      end if;
   end Get;

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Dimensionless is
   begin
      if Data.For_Migration then
         return Get (Get_JSON_Node (Data.Migration_Config, Path, Data.Module));
      else
         return Get (Data.Internal.Get.Get (Data.Module, Path));
      end if;
   end Get;

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Long_Long_Integer is
   begin
      if Data.For_Migration then
         return Get (Get_JSON_Node (Data.Migration_Config, Path, Data.Module));
      else
         return Get (Data.Internal.Get.Get (Data.Module, Path));
      end if;
   end Get;

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Virtual_String is
   begin
      if Data.For_Migration then
         return Get (Get_JSON_Node (Data.Migration_Config, Path, Data.Module));
      else
         return Get (Data.Internal.Get.Get (Data.Module, Path));
      end if;
   end Get;

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Dimensionless_Ratio is
      use Config_Data_Paths;
   begin
      return (Numerator => Get (Data, Path & "Numerator"), Denominator => Get (Data, Path & "Denominator"));
   end Get;

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Boolean) is
   begin
      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Create (Value));
      else
         Data.Internal.Get.Set (Data.Module, Path, Create (Value));
      end if;
   end Set;

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Long_Float) is
   begin
      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Create (Value));
      else
         Data.Internal.Get.Set (Data.Module, Path, Create (Value));
      end if;
   end Set;

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Dimensionless) is
   begin
      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Create (Long_Float (Value)));
      else
         Data.Internal.Get.Set (Data.Module, Path, Create (Long_Float (Value)));
      end if;
   end Set;

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Long_Long_Integer) is
   begin
      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Create (Value));
      else
         Data.Internal.Get.Set (Data.Module, Path, Create (Value));
      end if;
   end Set;

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Virtual_String) is
   begin
      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Create (Value));
      else
         Data.Internal.Get.Set (Data.Module, Path, Create (Value));
      end if;
   end Set;

   procedure Set (Data : in out Config_Data; Path : Config_Data_Paths.Vector; Value : Dimensionless_Ratio) is
      Val : constant JSON_Value := Create_Object;
   begin
      Set_Field (Val, "Numerator", Value.Numerator);
      Set_Field (Val, "Denominator", Value.Denominator);

      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Val);
      else
         Data.Internal.Get.Set (Data.Module, Path, Val);
      end if;
   end Set;

   procedure Save (Data : in out Config_Data) is
   begin
      if not Data.For_Migration then
         Data.Internal.Get.Save (Data.Module);
      end if;
   end Save;

   function Module_Name (Data : Config_Data) return Virtual_String is
   begin
      return Data.Module;
   end Module_Name;

   function Resolve_Config_Path (Data : Config_Data; Path : Config_Path) return Config_Data_Paths.Vector is
      Ignored : JSON_Value;
   begin
      for Requirement of Path.Required_Selections loop
         declare
            Actual : constant Virtual_String := Data.Get (Requirement.Path);
         begin
            if Actual /= Requirement.Selected then
               raise Constraint_Error
                 with
                   "Configuration error path uses an unselected variant (Path: "
                   & Config_Data_Paths.Vector'Image (Path.Path)
                   & ", Selection path: "
                   & Config_Data_Paths.Vector'Image (Requirement.Path)
                   & ", Expected: "
                   & Requirement.Selected'Image
                   & ", Actual: "
                   & Actual'Image
                   & ").";
            end if;
         end;
      end loop;

      begin
         if Data.For_Migration then
            Ignored := Get_JSON_Node (Data.Migration_Config, Path.Path, Data.Module);
         else
            Ignored := Data.Internal.Get.Get (Data.Module, Path.Path);
         end if;
      exception
         when Occurrence : Constraint_Error =>
            raise Constraint_Error
              with
                "Configuration error path is unavailable (Path: "
                & Config_Data_Paths.Vector'Image (Path.Path)
                & ", Reason: "
                & Ada.Exceptions.Exception_Message (Occurrence)
                & ").";
      end;

      return Path.Path;
   end Resolve_Config_Path;

   procedure Recursive_Left_Merge (Left : JSON_Value; Right : JSON_Value; Full_Join : Boolean := True) is
      procedure Map_Double_JSON_Object is new Gen_Map_JSON_Object (JSON_Value);

      procedure Mapper (User_Object : in out JSON_Value; Name : Virtual_String; Value : JSON_Value);

      procedure Mapper (User_Object : in out JSON_Value; Name : Virtual_String; Value : JSON_Value) is
         Next_Node : JSON_Value;
      begin
         if Full_Join or else User_Object.Has_Field (Name) then
            if User_Object.Kind = JSON_Object_Type
              and then User_Object.Has_Field (Name)
              and then User_Object.Get (Name).Kind = JSON_Object_Type
              and then Value.Kind = JSON_Object_Type
            then
               Next_Node := User_Object.Get (Name);
               Map_Double_JSON_Object (Value, Mapper'Access, Next_Node);
            else
               if User_Object.Kind /= JSON_Object_Type then
                  raise Constraint_Error with "Tried to merge non-object.";
               end if;
               User_Object.Set_Field (Name, Value);
            end if;
         end if;
      end Mapper;

      Start_Object : JSON_Value := Left;
   begin
      Map_Double_JSON_Object (Right, Mapper'Access, Start_Object);

      pragma Unreferenced (Start_Object); --  in out parameter
   end Recursive_Left_Merge;

end Prunt.Config;
