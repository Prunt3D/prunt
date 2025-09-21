-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
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

package Prunt.Gcode_Arguments is

   Parse_Error : exception;

   type Arguments_Index is new Character range 'A' .. 'Z';

   type Argument_Kind is (Non_Existant_Kind, No_Value_Kind, Integer_Kind, Float_Kind, String_Kind);

   type Argument_Integer is range 0 .. 999;
   --  Changing the range of this type will not change the range of values reported as integers.

   type Arguments (Line_Length : Natural) is private;

   function Parse_Arguments (Line : String) return Arguments;
   --  Parses a line of G-code made up of made up of single letter parameters from A to Z, and optional arguments
   --  following each, which may be any of the following:
   --
   --  - An integer from 0 to 999.
   --  - A real number with an optional decimal part.
   --  - A string surrounded by `"` characters.
   --
   --  Duplicate parameters are not allowed, parameters may be in any order, and spaces are ignored. Anything after a
   --  `;` character is also ignored.
   --
   --  Raises `Parse_Error` if the line is malformed with a descriptive message suitable to be shown to the user.

   function Kind (Args : Arguments; Index : Arguments_Index) return Argument_Kind;
   --  Returns the kind of the argument for a given `Index`. The returned kind is as described in the documentation for
   --  `Parse_Arguments`.

   function Consume_Float_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : Dimensionless) return Dimensionless;
   --  Retrieve the value of an argument.
   --
   --  If the argument associated with `Index` exists and has an integer or float value then that value is returned. If
   --  the argument does not exist then `Default` is returned. Otherwise, `Parse_Error` will be raised with a
   --  descriptive message suitable to be shown to the user.
   --
   --  This procedure marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_Float (Args : in out Arguments; Index : Arguments_Index) return Dimensionless;
   --  Retrieve the value of an argument.
   --
   --  If the argument associated with `Index` exists and has an integer or float value then that value is returned.
   --  Otherwise, `Parse_Error` will be raised with a descriptive message suitable to be shown to the user.
   --
   --  This procedure marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_Integer_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : Argument_Integer) return Argument_Integer;
   --  Retrieve the value of an argument.
   --
   --  If the argument associated with `Index` exists and has an integer value then that value is returned. If the
   --  argument does not exist then `Default` is returned. Otherwise, `Parse_Error` will be raised with a descriptive
   --  message suitable to be shown to the user.
   --
   --  This procedure marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_Integer (Args : in out Arguments; Index : Arguments_Index) return Argument_Integer;
   --  Retrieve the value of an argument.
   --
   --  If the argument associated with `Index` exists and has an integer value then that value is returned. Otherwise,
   --  `Parse_Error` will be raised with a descriptive message suitable to be shown to the user.
   --
   --  This procedure marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_String_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : String) return String;
   --  Retrieve the value of an argument.
   --
   --  If the argument associated with `Index` exists and has a string value then that value is returned.
   --  If the argument does not exist then `Default` is returned. Otherwise, `Parse_Error` will be raised with a
   --  descriptive message suitable to be shown to the user.
   --
   --  This procedure marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_String (Args : in out Arguments; Index : Arguments_Index) return String;
   --  Retrieve the value of an argument.
   --
   --  If the argument associated with `Index` exists and has a string value then that value is returned. Otherwise,
   --  `Parse_Error` will be raised with a descriptive message suitable to be shown to the user.
   --
   --  This procedure marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_No_Value_Or_False (Args : in out Arguments; Index : Arguments_Index) return Boolean;
   --  Consumes a argument that should not have a value.
   --
   --  Returns `True` if the argument associated with `Index` is present without a value, and `False` if the argument
   --  is not present at all. If the argument has a value `Parse_Error` will be raised with a descriptive message
   --  suitable to be shown to the user.
   --
   --  This procedure marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   procedure Validate_All_Consumed (Args : Arguments);
   --  Checks that all arguments in `Args` have been consumed.
   --
   --  This is used to enforce the rule that unused arguments are not allowed. If any argument that was present on the
   --  line has not been consumed by one of the `Consume_*` functions, this procedure will raise `Parse_Error` with a
   --  descriptive message suitable to be shown to the user.

private

   type Argument (Kind : Argument_Kind := Non_Existant_Kind) is record
      Consumed : Boolean;
      case Kind is
         when Non_Existant_Kind =>
            null;

         when No_Value_Kind =>
            null;

         when Integer_Kind =>
            Integer_Value : Argument_Integer;

         when Float_Kind =>
            Float_Value : Dimensionless;

         when String_Kind =>
            Begin_Quote : Positive;
            End_Quote   : Positive;
      end case;
   end record;

   type Arguments_Array is array (Arguments_Index) of Argument;

   type Arguments (Line_Length : Natural) is record
      Line      : String (1 .. Line_Length);
      Arguments : Arguments_Array;
   end record;

end Prunt.Gcode_Arguments;
