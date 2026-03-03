------------------------------------------------------------------------------
--                                                                          --
--  This file is based on code from GNATCOLL. The below terms below apply   --
--  only to this file. Refer to COPYING in the root directory for the terms --
--  for other parts of Prunt.                                               --
--                                                                          --
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2011-2024, AdaCore                     --
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

--  This is a quick and dirty edit of GNATCOLL.JSON to use Virtual_String for its public API. Changes to the logic are
--  minimal to avoid introducing any bugs, but that means there's still plenty of room for improvement to fully take
--  advantage of Virtual_String.
--
--  This also changed integers which are too large for an integer to be parsed as floats and makes object elements
--  ordered based on their insertion order.

--  GNATCOLL.JSON exposes an API to parse and serialize data using the JSON
--  (JavaScript Object Notation) format.
--
--  Parsing JSON is as easy as calling the Read function::
--
--     Data : JSON_Value := Read ("[1, ""foo"", {""foo"": null}]");
--
--  Encoding to JSON is not any more complex::
--
--     JSON_String : String := Write (Data);
--
--  JSON trees (JSON_Value) are available for both inspection and
--  modification::
--
--     Float_Number : JSON_Value := Create (Float'(1.0));
--     --  Mere float number
--
--     Object : JSON_Value := Get (Get (Data), 3);
--     --  JSON object from Data: {"foo": null}
--
--     Some_Array : JSON_Value :=
--        Create (Float_Number & Object & Create (False));
--     --  Synthetic JSON array: [1.0, {"foo": null}, False]
--
--     --  Modify Data in place
--     Data.Append (Some_Array);

with Ada.Finalization;
with GNATCOLL.Buffer;
with VSS.Characters;

private with Ada.Containers.Vectors;
private with GNATCOLL;
private with GNATCOLL.Atomic;
private with Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;

package Prunt.JSON is

   -----------------
   -- JSON Parser --
   -----------------

   type JSON_Parser is tagged limited private;
   --  A JSON Parser object used mainly with SAX like API.

   type JSON_Parser_Event_Kind is
     (STRING_VALUE,
      INTEGER_VALUE,
      NUMBER_VALUE,
      TRUE_VALUE,
      FALSE_VALUE,
      NULL_VALUE,
      ARRAY_START,
      OBJECT_START,
      ARRAY_END,
      OBJECT_END,
      VALUE_SEP,
      NAME_SEP,
      DOC_END);

   type JSON_Parser_Event is record
      Kind  : JSON_Parser_Event_Kind;
      First : Long_Long_Integer;
      Last  : Long_Long_Integer;
   end record;
   --  A JSON Event is emitted by Parse_Next for each value and some
   --  structural elements. Parse_Next never emits VALUE_SEP and NAME_SEP
   --  events (used only internally by the parser).
   --
   --  An event consists a Kind that identify the nature of the element and
   --  a range that allows to retrieve the content of the associated token
   --  by calling the Token function of the associated Reader object.
   --
   --  The end user is in charge of calling the right conversion function to
   --  convert a Token into the final value.
   --
   --  STRING_VALUE: a string value. Call
   --      Prunt.JSON.Un_Escape_String to decode the string.
   --  INTEGER_VALUE: an integer value. 'Value on an integer like type can be
   --      called. Notice that JSON format does not impose any limit on the
   --      integer size.
   --  NUMBER_VALUE: a number value that may not be an integer. Float'Value
   --      and Long_Float'Value can be used to convert the value. Some
   --      rounding might occurs during conversion depending on the target
   --      float format used.
   --  TRUE_VALUE, FALSE_VALUE, NULL_VALUE: No conversion is needed to infer
   --      the final value
   --  ARRAY_START, ARRAY_END: Events emitted at the start and end of an array
   --  OBJECT_START, OBJECT_END: Events emitted at the start and end of an
   --    object. Once inside an object the parser will return sequentially the
   --    keys and associated value.
   --  DOC_END: emitted when the end of a stream is reached

   function Parse_Next (Self : in out JSON_Parser; Data : in out GNATCOLL.Buffer.Reader) return JSON_Parser_Event;
   --  Fetch the next JSON parser event.

   type JSON_Value_Type is
     (JSON_Null_Type,
      --  Null value: all such JSON values are equivalent

      JSON_Boolean_Type,
      --  Boolean value: either true or false

      JSON_Int_Type,
      --  Integer value, encoded as an Ada Long_Long_Integer

      JSON_Float_Type,
      --  Float value, encoded as an Ada Long_Float

      JSON_String_Type,
      --  UTF-8 encoded string

      JSON_Array_Type,
      --  Array of JSON values

      JSON_Object_Type
      --  Sequence of fields. Each field has a unique name and maps to a
      --  JSON value. Depending on the context, this sequence can be processed
      --  as a mapping, because each field name is unique, but iterating on
      --  fields is deterministic because it is a sequence underneath.
     );
   --  Each JSON value (JSON_Value below) has a specific kind...

   subtype JSON_Elementary_Value_Type is JSON_Value_Type range JSON_Null_Type .. JSON_String_Type;
   --  Some are atoms...

   subtype JSON_Container_Value_Type is JSON_Value_Type range JSON_Array_Type .. JSON_Object_Type;
   --  While others are containers for other values

   Invalid_JSON_Stream : exception;

   type JSON_Value is tagged private;
   --  Store a JSON value, which can be either a simple type (integer, string,
   --  ...) or an object with multiple fields, or an array (see JSON_Value_Type
   --  above).
   --
   --  This type has by-reference semantics, so using the standard assignment
   --  operator as in::
   --
   --      A := B;
   --
   --  just creates an alias. This means that modifying B will also modify A
   --  (and modifying A will of course modify B).
   --
   --  If you want to create a separate copy, you must use the Clone function.

   type JSON_Array is private
   with
     Iterable =>
       (First => Array_First, Next => Array_Next, Has_Element => Array_Has_Element, Element => Array_Element);
   --  JSON array type. If an object of type JSON_Array is not otherwise
   --  initialized, it is initialized to Empty_Array.
   --
   --  Note that we use the Iterable aspect instead of the standard Ada 2012
   --  iterator aspects because the latter brings impossible constraints: Ada
   --  2012 iterators require JSON_Array to be tagged, which would break the
   --  existing API: the Get function would be dispatching over more than one
   --  type (JSON_Array because of the Arr argument, and JSON_Value because of
   --  the return type).

   JSON_Null   : constant JSON_Value;
   Empty_Array : constant JSON_Array;

   --------------------
   -- Array handling --
   --------------------

   function Is_Empty (Arr : JSON_Array) return Boolean;
   --  Return whether Arr is an empty array

   function Length (Arr : JSON_Array) return Natural;
   --  Return the number of elements in Arr

   function Get (Arr : JSON_Array; Index : Positive) return JSON_Value;
   --  If Arr has at least Index elements, return the element at that index.
   --  Raise a Constraint_Error otherwise.

   procedure Append (Arr : in out JSON_Array; Val : JSON_Value);
   --  Append Val as a new element at the end of the Arr array

   procedure Prepend (Arr : in out JSON_Array; Val : JSON_Value);
   --  Insert Val as the first element of the Arr array

   procedure Clear (Arr : in out JSON_Array);
   --  Remove all elements in Arr

   procedure Sort (Arr : in out JSON_Array; Less : access function (Left, Right : JSON_Value) return Boolean);
   --  Reorder the elements in Arr such that they are sorted smallest first
   --  according to the strict comparison that Less implements.

   procedure Set_Element (Arr : in out JSON_Array; Index : Positive; Item : JSON_Value);
   --  If Arr has at least Index elements, replace the element at that index
   --  with Item. Raise a Constraint_Error otherwise.

   --  Both functions below are less efficient than Append because they
   --  result in an extra copy of the array, but they are easier to use when
   --  manipulating small arrays.

   function "&" (Arr : JSON_Array; Value : JSON_Value) return JSON_Array;
   --  Return a new array that appends Value to Arr

   function "&" (Value1, Value2 : JSON_Value) return JSON_Array;
   --  Return a new array that contains Value1 and Value2

   function Is_Empty (Val : JSON_Value) return Boolean;
   --  Return True if Val is empty array, empty object or null value. Return
   --  False in all other cases.

   ---------------------
   -- Array iteration --
   ---------------------

   function Array_First (Arr : JSON_Array) return Positive;
   function Array_Next (Arr : JSON_Array; Index : Positive) return Positive;
   function Array_Has_Element (Arr : JSON_Array; Index : Positive) return Boolean;
   function Array_Element (Arr : JSON_Array; Index : Positive) return JSON_Value;

   ----------------------------------------------
   -- Serialization/deserialization primitives --
   ----------------------------------------------

   type Parsing_Error is record
      Line, Column : Natural;
      --  Line and column numbers at which a parsing error is detected

      Message : Virtual_String;
      --  Short description of the parsing error
   end record;

   function Format_Parsing_Error (Error : Parsing_Error) return Virtual_String;
   --  Return a human-readable string to describe Error

   type Read_Result (Success : Boolean := True) is record
      case Success is
         when True =>
            Value : JSON_Value;

         when False =>
            Error : Parsing_Error;
      end case;
   end record;

   function Read (Strm : Virtual_String; Filename : String := "<data>") return JSON_Value;
   --  Parse the JSON document in Strm and return it. On parsing error, print
   --  an error message referencing Filename on the standard output and raise
   --  an Invalid_JSON_Stream exception.

   function Read (Strm : Virtual_String) return Read_Result;
   --  Parse the JSON document in Strm and return it. If there is a parsing
   --  error, return the corresponding error information.

   function Read (Data : in out GNATCOLL.Buffer.Reader) return Read_Result;
   function Read_File (Path : String) return Read_Result;
   --  Likewise for streams and files

   function Write (Item : JSON_Value; Compact : Boolean := True) return Virtual_String;
   --  Return a string that encodes Item in JSON. Unless Compact is True, this
   --  creates an indented multi-line representation.

   -----------------------------
   -- Creation of JSON values --
   -----------------------------

   function Create return JSON_Value
   with Post => Create'Result.Kind = JSON_Null_Type;
   --  Create a 'null' JSON value

   function Create (Val : Boolean) return JSON_Value
   with Post => Create'Result.Kind = JSON_Boolean_Type;
   --  Create a boolean-typed JSON value

   function Create (Val : Integer) return JSON_Value
   with Post => Create'Result.Kind = JSON_Int_Type;
   function Create (Val : Long_Integer) return JSON_Value
   with Post => Create'Result.Kind = JSON_Int_Type;
   function Create (Val : Long_Long_Integer) return JSON_Value
   with Post => Create'Result.Kind = JSON_Int_Type;
   --  Create an integer-typed JSON value

   function Create (Val : Float) return JSON_Value
   with Post => Create'Result.Kind = JSON_Float_Type;
   --  Create a float-typed JSON value

   function Create (Val : Long_Float) return JSON_Value
   with Post => Create'Result.Kind = JSON_Float_Type;

   function Create (Val : Virtual_String) return JSON_Value
   with Post => Create'Result.Kind = JSON_String_Type;
   --  Create a string-typed JSON value

   function Create (Val : JSON_Array) return JSON_Value
   with Post => Create'Result.Kind = JSON_Array_Type;
   --  Create a JSON value from the JSON array

   function Create_Object return JSON_Value
   with Post => Create_Object'Result.Kind = JSON_Object_Type;
   --  Create an empty object. Values need to be added using the below
   --  Set_Field methods.

   procedure Sort (Val : in out JSON_Value; Less : access function (Left, Right : JSON_Value) return Boolean);
   --  If Val is a JSON array, reorder its elements/fields
   --  such that they are sorted smallest first according to the strict
   --  comparison that Less implements.

   procedure Append (Arr : JSON_Value; Item : JSON_Value)
   with Pre => Arr.Kind = JSON_Array_Type;
   --  Assuming Arr is a JSON array, append Item to it

   function Clone (Val : JSON_Value) return JSON_Value;
   --  Return a deep clone of Val.  Any later change in Val or its fields
   --  (recursively) will have no impact on the resulting value.

   overriding
   function "=" (Left, Right : JSON_Value) return Boolean;
   --  Return whether Left and Right are structurally identical.
   --
   --  The actual contents is compared, not the pointers. So two objects
   --  constructed independently, with the same contents, will match. For JSON
   --  objects, the order for fields is irrelevant, for objects. It is relevant
   --  for arrays however.

   procedure Set_Field (Val : JSON_Value; Field_Name : Virtual_String; Field : JSON_Value)
   with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, add a new field or modify the existing
   --  one for the given Field_Name. The field value is Field afterwards.

   --  All the Set_Field overloads below are convenience shortcut that first
   --  create a JSON value from their Field argument and then call the above
   --  Set_Field procedures with the result.

   procedure Set_Field (Val : JSON_Value; Field_Name : Virtual_String; Field : Boolean)
   with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field (Val : JSON_Value; Field_Name : Virtual_String; Field : Integer)
   with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field (Val : JSON_Value; Field_Name : Virtual_String; Field : Long_Integer)
   with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field (Val : JSON_Value; Field_Name : Virtual_String; Field : Float)
   with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field_Long_Float (Val : JSON_Value; Field_Name : Virtual_String; Field : Long_Float)
   with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field (Val : JSON_Value; Field_Name : Virtual_String; Field : Virtual_String)
   with Pre => Val.Kind = JSON_Object_Type;

   procedure Set_Field (Val : JSON_Value; Field_Name : Virtual_String; Field : JSON_Array)
   with Pre => Val.Kind = JSON_Object_Type;
   --  This performs a a shallow copy of Field, so any change you do to the
   --  passed array for Field afterwards will not impact Val.

   procedure Set_Field_If_Not_Empty (Val : JSON_Value; Field_Name : Virtual_String; Field : Virtual_String)
   with Pre => Val.Kind = JSON_Object_Type;
   --  Set Field only if it is not empty string

   procedure Set_Field_If_Not_Empty (Val : JSON_Value; Field_Name : Virtual_String; Field : JSON_Array)
   with Pre => Val.Kind = JSON_Object_Type;
   --  Set Field only if it is not empty array.
   --  This performs a a shallow copy of Field, so any change you do to the
   --  passed array for Field afterwards will not impact Val.

   procedure Unset_Field (Val : JSON_Value; Field_Name : Virtual_String)
   with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, remove its field whose name matches
   --  Field_Name. Do nothing if there is no such a field.

   ------------------------------------------------------
   -- Conversions from JSON values to native Ada types --
   ------------------------------------------------------

   function Kind (Val : JSON_Value) return JSON_Value_Type;
   --  Return the kind corresponding to the Val JSON value

   function Get (Val : JSON_Value) return Boolean
   with Pre => Val.Kind = JSON_Boolean_Type;

   function Get (Val : JSON_Value) return Integer
   with Pre => Val.Kind = JSON_Int_Type;

   function Get (Val : JSON_Value) return Long_Integer
   with Pre => Val.Kind = JSON_Int_Type;

   function Get (Val : JSON_Value) return Long_Long_Integer
   with Pre => Val.Kind = JSON_Int_Type;

   function Get (Val : JSON_Value) return Float
   with Pre => Val.Kind in JSON_Int_Type | JSON_Float_Type;

   function Get (Val : JSON_Value) return Long_Float
   with Pre => Val.Kind in JSON_Int_Type | JSON_Float_Type;

   function Get (Val : JSON_Value) return Dimensionless
   with Pre => Val.Kind in JSON_Int_Type | JSON_Float_Type;

   function Get (Val : JSON_Value) return Virtual_String
   with Pre => Val.Kind = JSON_String_Type;

   function Get_Array (Val : JSON_Value) return JSON_Array
   with Pre => Val.Kind = JSON_Array_Type;

   function Has_Field (Val : JSON_Value; Field : Virtual_String) return Boolean
   with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, return whether it contains a field whose
   --  name is Field.

   function Get (Val : JSON_Value; Field : Virtual_String) return JSON_Value
   with Pre => Val.Kind = JSON_Object_Type;

   function Get (Val : JSON_Value; Field : Virtual_String) return Boolean
   with Pre => Val.Kind = JSON_Object_Type and then Get (Val, Field).Kind = JSON_Boolean_Type;

   function Get (Val : JSON_Value; Field : Virtual_String) return Integer
   with Pre => Val.Kind = JSON_Object_Type and then Get (Val, Field).Kind = JSON_Int_Type;

   function Get (Val : JSON_Value; Field : Virtual_String) return Long_Integer
   with Pre => Val.Kind = JSON_Object_Type and then Get (Val, Field).Kind = JSON_Int_Type;

   function Get (Val : JSON_Value; Field : Virtual_String) return Float
   with Pre => Val.Kind = JSON_Object_Type and then Get (Val, Field).Kind in JSON_Int_Type | JSON_Float_Type;

   function Get (Val : JSON_Value; Field : Virtual_String) return Long_Float
   with Pre => Val.Kind = JSON_Object_Type and then Get (Val, Field).Kind in JSON_Int_Type | JSON_Float_Type;

   function Get (Val : JSON_Value; Field : Virtual_String) return Dimensionless
   with Pre => Val.Kind = JSON_Object_Type and then Get (Val, Field).Kind in JSON_Int_Type | JSON_Float_Type;

   function Get (Val : JSON_Value; Field : Virtual_String) return Virtual_String
   with Pre => Val.Kind = JSON_Object_Type and then Get (Val, Field).Kind = JSON_String_Type;

   function Get_Array (Val : JSON_Value; Field : Virtual_String) return JSON_Array
   with Pre => Val.Kind = JSON_Object_Type and then Get (Val, Field).Kind = JSON_Array_Type;

   ---------------
   -- Iteration --
   ---------------

   procedure Map_JSON_Object (Val : JSON_Value; CB : access procedure (Name : Virtual_String; Value : JSON_Value))
   with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, call CB on all its fields

   generic
      type Mapped (<>) is private;
   procedure Gen_Map_JSON_Object
     (Val         : JSON_Value;
      CB          : access procedure (User_Object : in out Mapped; Name : Virtual_String; Value : JSON_Value);
      User_Object : in out Mapped)
   with Pre => Val.Kind = JSON_Object_Type;
   --  Assuming Val is a JSON object, call CB on all its field, passing the
   --  given User_Object from call to call.

   --------------------------------
   -- From GNATCOLL.JSON.Utility --
   --------------------------------

   function Escape_Non_Print_Character (C : VSS.Characters.Virtual_Character) return Virtual_String;

   function Escape_String (Text : Virtual_String) return Virtual_String;
   --  Translates a Virtual_String into a JSON-escaped Virtual_String

   function Un_Escape_String (Text : Virtual_String) return Virtual_String;
   --  Translates a JSON-escaped Virtual_String into an unescaped Virtual_String

private

   --  JSON Parser structure
   type JSON_Parser_State is
     (EXPECT_VALUE,

      EXPECT_OBJECT_KEY,
      EXPECT_OBJECT_FIRST_KEY,
      EXPECT_OBJECT_NAME_SEP,
      EXPECT_OBJECT_VALUE,
      EXPECT_OBJECT_SEP,

      EXPECT_ARRAY_FIRST_VALUE,
      EXPECT_ARRAY_VALUE,
      EXPECT_ARRAY_SEP,

      EXPECT_DOC_END);

   type JSON_Parser_States is array (Integer range <>) of JSON_Parser_State;
   type JSON_Parser_States_Access is access all JSON_Parser_States;

   type JSON_Parser is new Ada.Finalization.Limited_Controlled with record
      --  Implementation note: Ada.Containers.Vector could be used here
      --  instead of a manually managed access to an array. Nevertheless
      --  this impacts significatively the performance of the parser.
      State         : JSON_Parser_States_Access := null;
      State_Current : Integer := 0;
   end record;
   pragma Finalize_Storage_Only (JSON_Parser);
   overriding
   procedure Finalize (Self : in out JSON_Parser);

   --  JSON_Value
   type JSON_Array_Internal;
   type JSON_Array_Access is access all JSON_Array_Internal;

   type JSON_Object_Internal;
   type JSON_Object_Access is access all JSON_Object_Internal;

   type JSON_String_Internal;
   type JSON_String_Access is access all JSON_String_Internal;

   --  Implementation note: it's important to keep Data_Type size as low as
   --  possible in oder to reduce memory footprint of a JSON_Value and also
   --  provide performance increase. That's the reason why for example for
   --  JSON_String_Type an access to a XString is used rather than the
   --  XString itself. It increase code complexity but reduce by 2 the size
   --  of the structure.
   type Data_Type (Kind : JSON_Value_Type := JSON_Null_Type) is record
      case Kind is
         when JSON_Null_Type =>
            null;

         when JSON_Boolean_Type =>
            Bool_Value : Boolean;

         when JSON_Int_Type =>
            Int_Value : Long_Long_Integer;

         when JSON_Float_Type =>
            Flt_Value : Long_Float;

         when JSON_String_Type =>
            Str_Value : JSON_String_Access;

         when JSON_Array_Type =>
            Arr_Value : JSON_Array_Access;

         when JSON_Object_Type =>
            Obj_Value : JSON_Object_Access;
      end case;
   end record;

   type JSON_Value is new Ada.Finalization.Controlled with record
      Data : Data_Type;
   end record;
   --  We cannot merge Data_Type and JSON_Value, because JSON_Value cannot
   --  have a discriminant with a default value.

   overriding
   procedure Adjust (Obj : in out JSON_Value);
   overriding
   procedure Finalize (Obj : in out JSON_Value);

   --  JSON Array definition
   package Vect_Pkg is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => JSON_Value);

   type JSON_Array is record
      Vals : Vect_Pkg.Vector;
   end record;

   type JSON_Array_Internal is record
      Cnt : aliased GNATCOLL.Atomic.Atomic_Counter := 1;
      Arr : JSON_Array;
   end record;

   --  JSON String definition
   type JSON_String_Internal is record
      Cnt : aliased GNATCOLL.Atomic.Atomic_Counter := 1;
      Str : Virtual_String;
   end record;

   Empty_Array : constant JSON_Array := (Vals => Vect_Pkg.Empty_Vector);

   --  JSON Object definition
   package Object_Items_Pkg is new
     Indefinite_Ordered_Maps_With_Insertion_Order (Key_Type => Virtual_String, Element_Type => JSON_Value, "<" => "<");

   type JSON_Object_Internal is record
      Cnt  : aliased GNATCOLL.Atomic.Atomic_Counter := 1;
      Vals : Object_Items_Pkg.Map;
   end record;

   --  JSON Null constant definition
   JSON_Null : constant JSON_Value := (Ada.Finalization.Controlled with others => <>);
   --  Can't call Create, because we would need to see the body of
   --  Initialize and Adjust.

end Prunt.JSON;
