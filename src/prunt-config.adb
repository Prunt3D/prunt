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

with Ada.Strings.Fixed;
with Ada.Tags;
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

   function Create_Default_Module_Config (Schema : Config_Property_Maps.Map) return JSON_Value is
      function Outer_Generate (Property : Config_Property_Parameters'Class) return JSON_Value;

      function Outer_Generate (Property : Config_Property_Parameters'Class) return JSON_Value is
         function Generate (Property : Config_Property_Parameters_Boolean) return JSON_Value;
         function Generate (Property : Config_Property_Parameters_Discrete) return JSON_Value;
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
                  Result.Set_Field (Config_Property_Maps.Key (C), Outer_Generate (Config_Property_Maps.Element (C)));
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
                    (Config_Property_Maps.Key (C), Outer_Generate (Config_Property_Maps.Element (C)));
               end loop;
            end return;
         end Generate;
      begin
         if Property in Config_Property_Parameters_Boolean then
            return Generate (Config_Property_Parameters_Boolean (Property));
         elsif Property in Config_Property_Parameters_Discrete then
            return Generate (Config_Property_Parameters_Discrete (Property));
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
      end Outer_Generate;
   begin
      return Result : constant JSON_Value := Create_Object do
         for C in Schema.Iterate loop
            Result.Set_Field (Config_Property_Maps.Key (C), Outer_Generate (Config_Property_Maps.Element (C)));
         end loop;
      end return;
   end Create_Default_Module_Config;

   function Generate_Schemas_String (Schemas : Config_Schema_Maps.Map) return Virtual_String is
      function Outer_Generate (Property : Config_Property_Parameters'Class) return JSON_Value;

      function Outer_Generate (Property : Config_Property_Parameters'Class) return JSON_Value is
         Result : constant JSON_Value := Create_Object;
      begin
         Result.Set_Field ("Description", Property.Description);

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

   protected body Config_File_Internal is
      procedure Initialize
        (File_Name_In : String;
         Schemas_In   : Config_Schema_Maps.Map;
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
                          Live_Config.Get ("Config").Get (Module_Name).Get ("Config");
                        Mid_Module_Config : constant JSON_Value :=
                          Create_Default_Module_Config (Module_Schema.Top_Level_Items);
                        New_Module_Config : constant JSON_Value :=
                          Create_Default_Module_Config (Module_Schema.Top_Level_Items);
                     begin
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

               Validate_Module_Config_To_Schema
                 (Live_Config.Get ("Config").Get (Module_Name).Get ("Config"),
                  Module_Schema.Top_Level_Items,
                  Raise_Error_For_Module'Access,
                  Check_For_Missing_Fields => True);
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

         if Write_Required then
            Write_File;
         end if;

         Cached_Schemas := Generate_Schemas_String (Schemas);
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

         if not Update_Deltas.Contains (Owner) then
            Update_Deltas.Insert (Owner, Create_Object);
         end if;

         Set_JSON_Node (Live_Config.Get ("Config").Get (Owner).Get ("Config"), Path, Value);

         Set_JSON_Node (Update_Deltas (Owner), Path, Value);
      end Set;

      procedure Save (Owner : Virtual_String; Lock : File_Access_Lock.Lock_Holder := File_Access_Lock.Lock) is
         pragma Unreferenced (Lock);

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
            Schemas (Owner).Element.Top_Level_Items,
            Raise_Error_For_Module'Access,
            Check_For_Missing_Fields => True);

         Live_Config.Get ("Config").Get (Owner).Set_Field ("Config", Live_Config_Clone);
         Stored_Config.Get ("Config").Get (Owner).Set_Field ("Config", Stored_Config_Clone);
         --  We use a clone here to avoid `Stored_Config` ever being in an invalid state. This is to avoid the case
         --  where a module catches the Constraint_Error raised above then another module could still save after that
         --  point, in that case an invalid `Stored_Config` would be saved to a file.

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

               --  Workaround below for https://gcc.gnu.org/bugzilla/show_bug.cgi?id=123185
               Validate_Module_Config_To_Schema
                 (Value.Get ("Config"),
                  Schemas (Name).Element.Top_Level_Items,
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
         Live_Config := Stored_Config;
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
         --  TODO: Figure out what to do here if there is still any unsaved data left in `Update_Deltas`.
         if Ref_Count /= 1 then
            raise Constraint_Error
              with Ref_Count'Image & " references to config file still exist during finalisation.";
         end if;

         Object.Internal := Config_File_Internal_Shared_Pointers.Null_Ref;
      end if;
   end Finalize;

   function Create (File_Name : String; Schemas : Config_Schema_Maps.Map) return Config_File is
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
            Result.Internal.Get.Initialize (File_Name, Schemas, Migrate'Access);
         end;
      end return;
   end Create;

   function Get_Data (This : Config_File; Module_Name : Virtual_String) return Config_Data is
   begin
      return
        Config_Data'(For_Migration => False, Module => Module_Name, Internal => This.Internal, Migration_Config => <>);
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

   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Boolean) is
   begin
      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Create (Value));
      else
         Data.Internal.Get.Set (Data.Module, Path, Create (Value));
      end if;
   end Set;

   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Long_Float) is
   begin
      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Create (Value));
      else
         Data.Internal.Get.Set (Data.Module, Path, Create (Value));
      end if;
   end Set;

   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Dimensionless) is
   begin
      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Create (Long_Float (Value)));
      else
         Data.Internal.Get.Set (Data.Module, Path, Create (Long_Float (Value)));
      end if;
   end Set;

   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Long_Long_Integer) is
   begin
      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Create (Value));
      else
         Data.Internal.Get.Set (Data.Module, Path, Create (Value));
      end if;
   end Set;

   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Virtual_String) is
   begin
      if Data.For_Migration then
         Set_JSON_Node (Data.Migration_Config, Path, Create (Value));
      else
         Data.Internal.Get.Set (Data.Module, Path, Create (Value));
      end if;
   end Set;

   function Get (Data : Config_Data; Path : Config_Data_Paths.Vector) return Dimensionless_Ratio is
      use Config_Data_Paths;
   begin
      return (Numerator => Get (Data, Path & "Numerator"), Denominator => Get (Data, Path & "Denominator"));
   end Get;

   procedure Set (Data : Config_Data; Path : Config_Data_Paths.Vector; Value : Dimensionless_Ratio) is
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

   procedure Save (Data : Config_Data) is
   begin
      if not Data.For_Migration then
         Data.Internal.Get.Save (Data.Module);
      end if;
   end Save;

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
