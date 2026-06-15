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

with Prunt.Config;
with Prunt.JSON; use Prunt.JSON;
with Trendy_Test; use Trendy_Test;

package body Prunt.Integration_Config_Overlays.Test is

   pragma Style_Checks (Off);

   function Boolean_Property (Default : Boolean) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("Kind", +"Boolean");
      Result.Set_Field ("Default", Create (Default));
      return Result;
   end Boolean_Property;

   function Float_Property (Default : Long_Float) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("Kind", +"Float");
      Result.Set_Field ("Default", Create (Default));
      return Result;
   end Float_Property;

   function Float_Ratio_Property (Numerator, Denominator : Long_Float) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("Kind", +"Float_Ratio");
      Result.Set_Field ("Default_Numerator", Create (Numerator));
      Result.Set_Field ("Default_Denominator", Create (Denominator));
      return Result;
   end Float_Ratio_Property;

   function Integer_Property (Default : Long_Long_Integer) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("Kind", +"Integer");
      Result.Set_Field ("Default", Create (Default));
      return Result;
   end Integer_Property;

   function String_Property (Default : String) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("Kind", +"String");
      Result.Set_Field ("Default", +Default);
      return Result;
   end String_Property;

   function Sequence_Property return JSON_Value is
      Result   : constant JSON_Value := Create_Object;
      Children : constant JSON_Value := Create_Object;
   begin
      Children.Set_Field ("Child", String_Property ("default-child"));
      Result.Set_Field ("Kind", +"Sequence");
      Result.Set_Field ("Children", Children);
      return Result;
   end Sequence_Property;

   function Variant_Property return JSON_Value is
      Result   : constant JSON_Value := Create_Object;
      Children : constant JSON_Value := Create_Object;
   begin
      Children.Set_Field ("Enabled", Boolean_Property (True));
      Children.Set_Field ("Disabled", Boolean_Property (False));
      Result.Set_Field ("Kind", +"Variant");
      Result.Set_Field ("Default", +"Disabled");
      Result.Set_Field ("Children", Children);
      return Result;
   end Variant_Property;

   function Module_Schema (Version : Long_Long_Integer; Config : JSON_Value) return JSON_Value is
      Result : constant JSON_Value := Create_Object;
   begin
      Result.Set_Field ("Version", Create (Version));
      Result.Set_Field ("Config", Config);
      return Result;
   end Module_Schema;

   function Test_Schema_String return Virtual_String is
      Root         : constant JSON_Value := Create_Object;
      Modules      : constant JSON_Value := Create_Object;
      Alpha_Config : constant JSON_Value := Create_Object;
      Beta_Config  : constant JSON_Value := Create_Object;
   begin
      Alpha_Config.Set_Field ("Enabled", Boolean_Property (True));
      Alpha_Config.Set_Field ("Count", Integer_Property (11));
      Alpha_Config.Set_Field ("Feed", Float_Property (22.5));
      Alpha_Config.Set_Field ("Ratio", Float_Ratio_Property (1.0, 2.0));
      Alpha_Config.Set_Field ("Nested", Sequence_Property);
      Alpha_Config.Set_Field ("Mode", Variant_Property);

      Beta_Config.Set_Field ("Name", String_Property ("beta-default"));

      Modules.Set_Field ("Alpha", Module_Schema (7, Alpha_Config));
      Modules.Set_Field ("Beta", Module_Schema (3, Beta_Config));
      Root.Set_Field ("Config", Modules);
      return Write (Root);
   end Test_Schema_String;

   procedure Test_Defaults_Omitted_Modules_And_Fields (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Patch : constant JSON_Value :=
        Config_Patch_For_Overlay (Test_Schema_String, Read ("{""Alpha"": {""Count"": 42}}"));
      Config : constant JSON_Value := Patch.Get ("Config");
      Alpha  : constant JSON_Value := Config.Get ("Alpha").Get ("Config");
      Beta   : constant JSON_Value := Config.Get ("Beta").Get ("Config");

      T.Assert (Patch.Get ("Prunt config version").Get = 1, "Patch version was not set");
      T.Assert (Config.Get ("Alpha").Get ("Version").Get = 7, "Alpha version was not preserved");
      T.Assert (Config.Get ("Beta").Get ("Version").Get = 3, "Beta version was not preserved");
      T.Assert (Alpha.Get ("Enabled").Get, "Omitted boolean field was not defaulted");
      T.Assert (Alpha.Get ("Count").Get = 42, "Overlay integer value was not merged");
      T.Assert (Alpha.Get ("Feed").Get = 22.5, "Omitted float field was not defaulted");
      T.Assert (Alpha.Get ("Ratio").Get ("Numerator").Get = 1.0, "Omitted ratio numerator was not defaulted");
      T.Assert (Alpha.Get ("Ratio").Get ("Denominator").Get = 2.0, "Omitted ratio denominator was not defaulted");
      T.Assert (Alpha.Get ("Nested").Get ("Child").Get = "default-child", "Omitted sequence was not defaulted");
      T.Assert (Alpha.Get ("Mode").Get ("Selected").Get = "Disabled", "Omitted variant selection was not defaulted");
      T.Assert
        (Alpha.Get ("Mode").Get ("Children").Get ("Enabled").Get,
         "Omitted variant children were not defaulted");
      T.Assert (Beta.Get ("Name").Get = "beta-default", "Omitted module was not defaulted");
   end Test_Defaults_Omitted_Modules_And_Fields;

   procedure Test_Invalid_Module_Reports_Error (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         Patch : JSON_Value := Create_Object;
         pragma Unreferenced (Patch);
      begin
         Patch := Config_Patch_For_Overlay (Test_Schema_String, Read ("{""Missing"": {}}"));
         T.Fail ("Unknown overlay module should have raised an error");
      exception
         when Program_Error =>
            null;
      end;
   end Test_Invalid_Module_Reports_Error;

   procedure Test_Invalid_Overlay_Type_Reports_Error (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         Patch : JSON_Value := Create_Object;
         pragma Unreferenced (Patch);
      begin
         Patch := Config_Patch_For_Overlay (Test_Schema_String, Read ("{""Alpha"": 1}"));
         T.Fail ("Scalar module overlay should have raised an error");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Invalid_Overlay_Type_Reports_Error;

   procedure Test_Validation_Errors_Are_Propagated (T : in out Trendy_Test.Operation'Class) is
      procedure Reject_String_Count
        (Patch : Virtual_String; Result : out Virtual_String; Errors : out Config.Config_Error_Vectors.Vector);

      procedure Reject_String_Count
        (Patch : Virtual_String; Result : out Virtual_String; Errors : out Config.Config_Error_Vectors.Vector)
      is
         Parsed : constant JSON_Value := Read (Patch);
         Count  : constant JSON_Value := Parsed.Get ("Config").Get ("Alpha").Get ("Config").Get ("Count");
      begin
         Result := +"{}";
         if Count.Kind = JSON_String_Type then
            Errors.Append
              (Config.Config_Error'
                 (Path    => Config.Config_Data_Paths.Vector'(["Alpha", "Count"]),
                  Message => +"wrong type"));
         end if;
      end Reject_String_Count;
   begin
      T.Register;

      declare
         Overlay : constant JSON_Value := Read ("{""Alpha"": {""Count"": ""bad""}}");
      begin
         Apply_Config_Overlay (Test_Schema_String, Overlay, Reject_String_Count'Access);
         T.Fail ("Apply_Config_Overlay should propagate validation errors");
      exception
         when Program_Error =>
            null;
      end;
   end Test_Validation_Errors_Are_Propagated;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Defaults_Omitted_Modules_And_Fields'Unrestricted_Access,
         Test_Invalid_Module_Reports_Error'Unrestricted_Access,
         Test_Invalid_Overlay_Type_Reports_Error'Unrestricted_Access,
         Test_Validation_Errors_Are_Propagated'Unrestricted_Access];
   end All_Tests;

end Prunt.Integration_Config_Overlays.Test;
