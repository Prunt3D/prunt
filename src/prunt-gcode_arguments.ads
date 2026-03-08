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

package Prunt.Gcode_Arguments is

   Parse_Error : exception;
   --  Raised when a line is not valid with a descriptive message suitable to be shown to the user.

   subtype Arguments_Index is Wide_Wide_Character range 'A' .. 'Z';
   --  Used to index G-code arguments. G-code parameters are single letters from 'A' to 'Z'.

   type Argument_Kind is (Non_Existent_Kind, No_Value_Kind, Integer_Kind, Float_Kind, String_Kind);

   type Argument_Integer is range 0 .. 999;
   --  The parser will only report a number as an `Integer_Kind` rather than a `Float_Kind` if it fits in this
   --  type. Changing the range of this type will not change the range of values reported as integers as it is
   --  hardcoded to 0-999 elsewhere.

   type Arguments (<>) is private;
   --  Holds the parsed arguments from a single G-code line.

   function Parse_Arguments (Line : Virtual_String) return Arguments;
   --  Parses a line of G-code. The line is made up of single letter parameters from `A` to `Z`, and optional arguments
   --  following each.
   --
   --  The arguments may be any of the following:
   --  - An integer from 0 to 999.
   --  - A real number with an optional decimal part.
   --  - A string surrounded by `"` characters.
   --
   --  Duplicate parameters are not allowed. Parameters may be in any order. Spaces are ignored. Anything after a `;`
   --  character is also ignored as a comment.
   --
   --  Raises `Parse_Error` if the line is malformed.

   function Kind (Args : Arguments; Index : Arguments_Index) return Argument_Kind;
   --  Returns the kind of the argument for a given `Index`. The kind is as described in the documentation for the
   --  `Parse_Arguments` function.

   function Consume_Float_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : Dimensionless) return Dimensionless;
   --  Retrieves the value of an argument.
   --
   --  Returns the value of the argument if it exists and is an integer or float. Returns `Default` if the argument
   --  does not exist. Raises `Parse_Error` if the argument has the wrong type.
   --
   --  This function marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_Float (Args : in out Arguments; Index : Arguments_Index) return Dimensionless;
   --  Retrieves the value of an argument.
   --
   --  Returns the value of the argument if it exists and is an integer or float. Raises `Parse_Error` if the argument
   --  does not exist or has the wrong type.
   --
   --  This function marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_Integer_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : Argument_Integer) return Argument_Integer;
   --  Retrieves the value of an argument.
   --
   --  Returns the value of the argument if it exists and is an integer. Returns `Default` if the argument does not
   --  exist. Raises `Parse_Error` if the argument has the wrong type.
   --
   --  This function marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_Integer (Args : in out Arguments; Index : Arguments_Index) return Argument_Integer;
   --  Retrieves the value of an argument.
   --
   --  Returns the value of the argument if it exists and is an integer. Raises `Parse_Error` if the argument does not
   --  exist or has the wrong type.
   --
   --  This function marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_String_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : Virtual_String) return Virtual_String;
   --  Retrieves the value of an argument.
   --
   --  Returns the value of the argument if it exists and is a string. Returns `Default` if the argument does not
   --  exist. Raises `Parse_Error` if the argument has the wrong type.
   --
   --  This function marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_String (Args : in out Arguments; Index : Arguments_Index) return Virtual_String;
   --  Retrieves the value of an argument.
   --
   --  Returns the value of the argument if it exists and is a string. Raises `Parse_Error` if the argument does not
   --  exist or has the wrong type.
   --
   --  This function marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_No_Value_Or_False (Args : in out Arguments; Index : Arguments_Index) return Boolean;
   --  Consumes an argument that should not have a value.
   --
   --  Returns `True` if the argument is present without a value, and `False` if the argument is not present at all.
   --  If the argument has a value, `Parse_Error` will be raised.
   --
   --  This function marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   function Consume_No_Value (Args : in out Arguments; Index : Arguments_Index) return Boolean;
   --  Consumes an argument that should not have a value.
   --
   --  Returns `True` if the argument is present without a value.  If the argument has a value or is not present,
   --  `Parse_Error` will be raised.
   --
   --  This function marks the argument as consumed. If the argument has already been consumed, it will raise
   --  `Constraint_Error`.

   procedure Validate_All_Consumed (Args : Arguments);
   --  Checks that all arguments in `Args` have been consumed.
   --
   --  This is used to enforce the rule that unused arguments are not allowed. If any argument that was present on the
   --  line has not been consumed, this procedure will raise `Parse_Error`.

private

   type Argument (Kind : Argument_Kind := Non_Existent_Kind) is record
      Consumed : Boolean;
      case Kind is
         when Non_Existent_Kind =>
            null;

         when No_Value_Kind =>
            null;

         when Integer_Kind =>
            Integer_Value : Argument_Integer;

         when Float_Kind =>
            Float_Value : Dimensionless;

         when String_Kind =>
            String_Value : Virtual_String;
      end case;
   end record;

   type Arguments_Array is array (Arguments_Index) of Argument;

   type Arguments is record
      Arguments : Arguments_Array;
      Line      : Virtual_String;
   end record;

end Prunt.Gcode_Arguments;
