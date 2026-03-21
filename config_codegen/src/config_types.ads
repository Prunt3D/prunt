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

pragma Extensions_Allowed (On);

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with VSS.Strings; use VSS.Strings;

package Config_Types is

   type Unit_Data is record
      Conversion : Virtual_String;
      Display    : Virtual_String;
   end record;

   type Component_Data is record
      --  The members of this record represent the following parts of a record component declaration:
      --
      --  ... : `Type_Name` [range `Min` .. `Max`] := `Default` with
      --    [Annotate (Prunt_Config, Min, `Min`),]
      --    [Annotate (Prunt_Config, Max, `Max`),]
      --    [Annotate (Prunt_Config, Fixed_Kind, "`Fixed_Kind`"),]
      --    [Annotate (Prunt_Config, Options_Expr, "`Options_Expr`"),]
      --    [Annotate (Prunt_Config, Present_When, "`Present_When`"),]
      --    [Annotate (Prunt_Config, Schema_Default_Expr, "`Schema_Default_Expr`"),]
      --    [Annotate (Prunt_Config, Unit, "`Conversion_Unit`" [, "`Display_Unit`"])];
      --  -- `Description`
      --
      --  Min and Max annotations are used for ratios where a range can not be used.
      --
      --  The Fixed_Kind annotation is used when Type_Name refers to a record with discriminant which should have a
      --  value set by the expression contained within the string. The string may contain a `?` character, which will
      --  be replaced with the current index of last array in the config tree.
      --
      --  Options_Expr overrides the generated options expression for enum-backed discrete values.
      --
      --  Present_When conditionally includes the field in the generated schema and skips corresponding reader/setter
      --  code when false.
      --
      --  Schema_Default_Expr overrides the generated schema default expression.

      Type_Name           : Virtual_String;
      --  Type name is fully qualified.
      Default             : Virtual_String;
      Description         : Virtual_String;
      Min                 : Virtual_String;
      Max                 : Virtual_String;
      Fixed_Kind          : Virtual_String;
      Options_Expr        : Virtual_String;
      Present_When        : Virtual_String;
      Schema_Default_Expr : Virtual_String;
      Unit                : Unit_Data;
   end record;

   package Component_Data_Maps is new Ada.Containers.Ordered_Maps (Virtual_String, Component_Data);

   type Variant_Case_Data is record
      --  The members of this record represent the following parts of a record variant case:
      --
      --  type ... is record (...) is
      --     ...
      --     case ... is
      --        when ... =>
      --           --  `Description`
      --           `Components` --  Keys are component names.
      --     end case;
      --  end record with ...;

      Components  : Component_Data_Maps.Map;
      Description : Virtual_String;
   end record;

   package Variant_Case_Maps is new Ada.Containers.Ordered_Maps (Virtual_String, Variant_Case_Data);

   package Virtual_String_Maps is new Ada.Containers.Ordered_Maps (Virtual_String, Virtual_String);

   type Record_Data (Has_Variant : Boolean := False) is record
      --  The members of this record represent the following parts of a record declaration:
      --
      --  type ... is record [(`Discriminant` : `Discriminant_Type` := `Discriminant_Default`)] is
      --     --  Description
      --     `Components` --  Keys are component names.
      --     [case `Discriminant` is
      --        `Variants` --  Keys are choice lists.
      --     end case;]
      --  end record with
      --    [Annotate (Prunt_Config, Tabbed),] --  `Tabbed` is True if this annotation is present.
      --    Annotate (Prunt_Config, User_Config | Root_User_Config);

      Components  : Component_Data_Maps.Map;
      --  Does not include variant components.
      Description : Virtual_String;
      Tabbed      : Boolean;
      case Has_Variant is
         when False =>
            null;

         when True =>
            Discriminant_Type    : Virtual_String;
            --  Type name is fully qualified.
            Discriminant         : Virtual_String;
            Discriminant_Default : Virtual_String;
            Variants             : Variant_Case_Maps.Map;
      end case;
   end record;

   type Enum_Data is record
      --  Keys are enum literal names. Values are expressions controlling whether the literal is present in the
      --  generated schema.
      Present_When : Virtual_String_Maps.Map;
   end record;

   type Array_Data is record
      --  The members of this record represent the following parts of an array declaration:
      --
      --  type ... is array (`Index_Type`) of `Element_Type` with
      --    [Annotate (Prunt_Config, Tabbed),] --  `Tabbed` is True if this annotation is present.
      --    Annotate (Prunt_Config, User_Config);
      --
      --  TODO: Add support for ranges on Index_Type.
      --
      --  TODO: Add support for ranges on Element_Type.

      Index_Type   : Virtual_String;
      --  Type name is fully qualified.
      Element_Type : Virtual_String;
      --  Type name is fully qualified.
      Tabbed       : Boolean;
      Min          : Virtual_String;
      Max          : Virtual_String;
   end record;

   type Primitive_Type_Kind is (Boolean_Kind, Integer_Kind, Float_Kind, Ratio_Kind);

   type Boolean_Data is record
      --  Boolean types are currently hardcoded rather than being parsed.
      null;
   end record;

   type Ratio_Data is record
      --  Ratio types are currently hardcoded rather than being parsed.

      Unit : Unit_Data;
   end record;

   type Float_Data is record
      --  The members of this record represent the following parts of a float declaration:
      --
      --  [sub]type ... is [digits ... | type_name] [range `Min` .. `Max`] with
      --    [Annotate (Prunt_Config, Unit, "`Conversion_Unit`" [, "`Display_Unit`"]),]
      --    [Annotate (Prunt_Config, User_Config)]; --  Not required if units are specified.
      --
      --  Ratio types are currently hardcoded rather than being parsed.

      Unit : Unit_Data;
   end record;

   type Integer_Data is record
      --  The members of this record represent the following parts of an integer declaration:
      --
      --  [sub]type ... is [type_name | range `Min` .. `Max`] with
      --    [Annotate (Prunt_Config, Unit, "`Conversion_Unit`" [, "`Display_Unit`"]),]
      --    [Annotate (Prunt_Config, User_Config)]; --  Not required if units are specified.

      Unit : Unit_Data;
   end record;

   type Config_Kind is (Record_Kind, Array_Kind, Enum_Kind, Boolean_Kind, Integer_Kind, Float_Kind, Ratio_Kind);

   type Config_Type (Kind : Config_Kind := Record_Kind) is record
      case Kind is
         when Record_Kind =>
            Record_Value : Record_Data;

         when Array_Kind =>
            Array_Value : Array_Data;

         when Enum_Kind =>
            Enum_Value : Enum_Data;

         when Boolean_Kind =>
            Boolean_Value : Boolean_Data;

         when Float_Kind =>
            Float_Value : Float_Data;

         when Integer_Kind =>
            Integer_Value : Integer_Data;

         when Ratio_Kind =>
            Ratio_Value : Ratio_Data;
      end case;
   end record;

   package Config_Maps is new Ada.Containers.Ordered_Maps (Virtual_String, Config_Type);

   type Gcode_Argument_Kind is (Integer_Kind, Float_Kind, String_Kind, No_Value_Kind, Not_Present_Kind);

   type Gcode_Argument_Kind_Set is array (Gcode_Argument_Kind) of Boolean;

   type Gcode_Argument_Data is record
      Type_Name   : Virtual_String;
      Default     : Virtual_String;
      Description : Virtual_String;
      Arg_Kinds   : Gcode_Argument_Kind_Set;
   end record;

   package Gcode_Argument_Maps is new Ada.Containers.Ordered_Maps (Virtual_String, Gcode_Argument_Data);

   type Gcode_Command_Data is record
      Name        : Virtual_String;
      Arguments   : Gcode_Argument_Maps.Map;
      Description : Virtual_String;
   end record;

   type Gcode_Key is record
      Letter : Virtual_String;
      Number : Integer;
   end record;

   function "<" (Left, Right : Gcode_Key) return Boolean
   is (Left.Letter < Right.Letter or else (Left.Letter = Right.Letter and then Left.Number < Right.Number));

   package Gcode_Command_Vectors is new Ada.Containers.Vectors (Positive, Gcode_Command_Data);

   package Gcode_Command_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Gcode_Key,
        Element_Type => Gcode_Command_Vectors.Vector,
        "="          => Gcode_Command_Vectors."=");

   type Module_Data is record
      Name           : Virtual_String;
      Filename       : Virtual_String;
      Root_Type      : Virtual_String;
      Config         : Config_Maps.Map;
      Gcode_Commands : Gcode_Command_Maps.Map;
   end record;

   package Module_Data_Vectors is new Ada.Containers.Vectors (Positive, Module_Data);

end Config_Types;
