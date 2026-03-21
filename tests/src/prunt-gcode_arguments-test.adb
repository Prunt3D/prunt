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

with Trendy_Test; use Trendy_Test;

package body Prunt.Gcode_Arguments.Test is

   pragma Extensions_Allowed (On);

   procedure Test_Consume_Arguments (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("G1 X10 Y20.5 Z""hello"" A");

      T.Assert (Consume_Integer (Args, 'G') = 1);
      T.Assert (Consume_Integer (Args, 'X') = 10);
      T.Assert (Consume_Float (Args, 'Y') = 20.5);
      T.Assert (Consume_String (Args, 'Z') = "hello");
      T.Assert (Consume_No_Value_Or_False (Args, 'A'));
   end Test_Consume_Arguments;

   procedure Test_Consume_Error_Already_Consumed_Float (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X10");
      Val : Dimensionless := Consume_Float (Args, 'X')
      with Unreferenced;
      Val2 : Dimensionless := Consume_Float (Args, 'X')
      with Unreferenced;

      T.Fail ("Consuming an argument twice should raise Constraint_Error");
   exception
      when Constraint_Error =>
         null;
   end Test_Consume_Error_Already_Consumed_Float;

   procedure Test_Consume_Error_Already_Consumed_Float_Or_Default (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X10");
      Val : Dimensionless := Consume_Float_Or_Default (Args, 'X', 0.0)
      with Unreferenced;
      Val2 : Dimensionless := Consume_Float_Or_Default (Args, 'X', 0.0)
      with Unreferenced;

      T.Fail ("Consuming an argument twice should raise Constraint_Error");
   exception
      when Constraint_Error =>
         null;
   end Test_Consume_Error_Already_Consumed_Float_Or_Default;

   procedure Test_Consume_Error_Already_Consumed_Integer (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X10");
      Val : Argument_Integer := Consume_Integer (Args, 'X')
      with Unreferenced;
      Val2 : Argument_Integer := Consume_Integer (Args, 'X')
      with Unreferenced;

      T.Fail ("Consuming an argument twice should raise Constraint_Error");
   exception
      when Constraint_Error =>
         null;
   end Test_Consume_Error_Already_Consumed_Integer;

   procedure Test_Consume_Error_Already_Consumed_Integer_Or_Default (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X10");
      Val : Argument_Integer := Consume_Integer_Or_Default (Args, 'X', 0)
      with Unreferenced;
      Val2 : Argument_Integer := Consume_Integer_Or_Default (Args, 'X', 0)
      with Unreferenced;

      T.Fail ("Consuming an argument twice should raise Constraint_Error");
   exception
      when Constraint_Error =>
         null;
   end Test_Consume_Error_Already_Consumed_Integer_Or_Default;

   procedure Test_Consume_Error_Already_Consumed_No_Value (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("A");
      Val : Boolean := Consume_No_Value_Or_False (Args, 'A')
      with Unreferenced;
      Val2 : Boolean := Consume_No_Value_Or_False (Args, 'A')
      with Unreferenced;

      T.Fail ("Consuming an argument twice should raise Constraint_Error");
   exception
      when Constraint_Error =>
         null;
   end Test_Consume_Error_Already_Consumed_No_Value;

   procedure Test_Consume_Error_Already_Consumed_String (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("T""a""");
      Val : Virtual_String := Consume_String (Args, 'T')
      with Unreferenced;
      Val2 : Virtual_String := Consume_String (Args, 'T')
      with Unreferenced;

      T.Fail ("Consuming an argument twice should raise Constraint_Error");
   exception
      when Constraint_Error =>
         null;
   end Test_Consume_Error_Already_Consumed_String;

   procedure Test_Consume_Error_Already_Consumed_String_Or_Default (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("T""a""");
      Val : Virtual_String := Consume_String_Or_Default (Args, 'T', "")
      with Unreferenced;
      Val2 : Virtual_String := Consume_String_Or_Default (Args, 'T', "")
      with Unreferenced;

      T.Fail ("Consuming an argument twice should raise Constraint_Error");
   exception
      when Constraint_Error =>
         null;
   end Test_Consume_Error_Already_Consumed_String_Or_Default;

   procedure Test_Consume_Float_Error_No_Value (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X");
      Val : Dimensionless := Consume_Float (Args, 'X')
      with Unreferenced;

      T.Fail ("Consume_Float No_Value_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Float_Error_No_Value;

   procedure Test_Consume_Float_Error_Non_Existent (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("Y1");
      Val : Dimensionless := Consume_Float (Args, 'X')
      with Unreferenced;

      T.Fail ("Consume_Float Non_Existent_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Float_Error_Non_Existent;

   procedure Test_Consume_Float_Error_String_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X""abc""");
      Val : Dimensionless := Consume_Float (Args, 'X')
      with Unreferenced;

      T.Fail ("Consume_Float String_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Float_Error_String_Kind;

   procedure Test_Consume_Float_Or_Default_Error_No_Value (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X");
      Val : Dimensionless := Consume_Float_Or_Default (Args, 'X', 0.0)
      with Unreferenced;

      T.Fail ("Consume_Float_Or_Default No_Value_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Float_Or_Default_Error_No_Value;

   procedure Test_Consume_Float_Or_Default_Error_String_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X""abc""");
      Val : Dimensionless := Consume_Float_Or_Default (Args, 'X', 0.0)
      with Unreferenced;

      T.Fail ("Consume_Float_Or_Default String_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Float_Or_Default_Error_String_Kind;

   procedure Test_Consume_Float_Or_Default_Float_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X10.5");
      Val : constant Dimensionless := Consume_Float_Or_Default (Args, 'X', 0.0);

      T.Assert (Val = 10.5);
   end Test_Consume_Float_Or_Default_Float_Kind;

   procedure Test_Consume_Float_Or_Default_Integer_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X10");
      Val : constant Dimensionless := Consume_Float_Or_Default (Args, 'X', 0.0);

      T.Assert (Val = 10.0);
   end Test_Consume_Float_Or_Default_Integer_Kind;

   procedure Test_Consume_Float_Or_Default_Non_Existent (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("Y1");
      Val : constant Dimensionless := Consume_Float_Or_Default (Args, 'X', 5.0);

      T.Assert (Val = 5.0);
   end Test_Consume_Float_Or_Default_Non_Existent;

   procedure Test_Consume_Integer_Error_Float_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X1.5");
      Val : Argument_Integer := Consume_Integer (Args, 'X')
      with Unreferenced;

      T.Fail ("Consume_Integer Float_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Integer_Error_Float_Kind;

   procedure Test_Consume_Integer_Error_No_Value (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X");
      Val : Argument_Integer := Consume_Integer (Args, 'X')
      with Unreferenced;

      T.Fail ("Consume_Integer No_Value_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Integer_Error_No_Value;

   procedure Test_Consume_Integer_Error_Non_Existent (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("Y1");
      Val : Argument_Integer := Consume_Integer (Args, 'X')
      with Unreferenced;

      T.Fail ("Consume_Integer Non_Existent_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Integer_Error_Non_Existent;

   procedure Test_Consume_Integer_Error_String_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X""abc""");
      Val : Argument_Integer := Consume_Integer (Args, 'X')
      with Unreferenced;

      T.Fail ("Consume_Integer String_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Integer_Error_String_Kind;

   procedure Test_Consume_Integer_Or_Default_Error_Float_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X1.5");
      Val : Argument_Integer := Consume_Integer_Or_Default (Args, 'X', 0)
      with Unreferenced;

      T.Fail ("Consume_Integer_Or_Default Float_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Integer_Or_Default_Error_Float_Kind;

   procedure Test_Consume_Integer_Or_Default_Error_No_Value (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X");
      Val : Argument_Integer := Consume_Integer_Or_Default (Args, 'X', 0)
      with Unreferenced;

      T.Fail ("Consume_Integer_Or_Default No_Value_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Integer_Or_Default_Error_No_Value;

   procedure Test_Consume_Integer_Or_Default_Error_String_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X""abc""");
      Val : Argument_Integer := Consume_Integer_Or_Default (Args, 'X', 0)
      with Unreferenced;

      T.Fail ("Consume_Integer_Or_Default String_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_Integer_Or_Default_Error_String_Kind;

   procedure Test_Consume_Integer_Or_Default_Integer_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X10");
      Val : constant Argument_Integer := Consume_Integer_Or_Default (Args, 'X', 0);

      T.Assert (Val = 10);
   end Test_Consume_Integer_Or_Default_Integer_Kind;

   procedure Test_Consume_Integer_Or_Default_Non_Existent (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("Y1");
      Val : constant Argument_Integer := Consume_Integer_Or_Default (Args, 'X', 5);

      T.Assert (Val = 5);
   end Test_Consume_Integer_Or_Default_Non_Existent;

   procedure Test_Consume_No_Value_Or_False_Error_Float_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("H1.0");
      Val : Boolean := Consume_No_Value_Or_False (Args, 'H')
      with Unreferenced;

      T.Fail ("Consume_No_Value_Or_False Float_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_No_Value_Or_False_Error_Float_Kind;

   procedure Test_Consume_No_Value_Or_False_Error_Integer_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("H1");
      Val : Boolean := Consume_No_Value_Or_False (Args, 'H')
      with Unreferenced;

      T.Fail ("Consume_No_Value_Or_False Integer_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_No_Value_Or_False_Error_Integer_Kind;

   procedure Test_Consume_No_Value_Or_False_Error_String_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("H""a""");
      Val : Boolean := Consume_No_Value_Or_False (Args, 'H')
      with Unreferenced;

      T.Fail ("Consume_No_Value_Or_False String_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_No_Value_Or_False_Error_String_Kind;

   procedure Test_Consume_No_Value_Or_False_Non_Existent (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("Y1");
      Val : constant Boolean := Consume_No_Value_Or_False (Args, 'X');

      T.Assert (not Val);
   end Test_Consume_No_Value_Or_False_Non_Existent;

   procedure Test_Consume_String_Error_Float_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("T1.0");
      Val : Virtual_String := Consume_String (Args, 'T')
      with Unreferenced;

      T.Fail ("Consume_String Float_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_String_Error_Float_Kind;

   procedure Test_Consume_String_Error_Integer_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("T1");
      Val : Virtual_String := Consume_String (Args, 'T')
      with Unreferenced;

      T.Fail ("Consume_String Integer_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_String_Error_Integer_Kind;

   procedure Test_Consume_String_Error_No_Value (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("T");
      Val : Virtual_String := Consume_String (Args, 'T')
      with Unreferenced;

      T.Fail ("Consume_String No_Value_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_String_Error_No_Value;

   procedure Test_Consume_String_Error_Non_Existent (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X1");
      Val : Virtual_String := Consume_String (Args, 'T')
      with Unreferenced;

      T.Fail ("Consume_String Non_Existent_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_String_Error_Non_Existent;

   procedure Test_Consume_String_Or_Default_Error_Float_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("T1.0");
      Val : Virtual_String := Consume_String_Or_Default (Args, 'T', "")
      with Unreferenced;

      T.Fail ("Consume_String_Or_Default Float_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_String_Or_Default_Error_Float_Kind;

   procedure Test_Consume_String_Or_Default_Error_Integer_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("T1");
      Val : Virtual_String := Consume_String_Or_Default (Args, 'T', "")
      with Unreferenced;

      T.Fail ("Consume_String_Or_Default Integer_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_String_Or_Default_Error_Integer_Kind;

   procedure Test_Consume_String_Or_Default_Error_No_Value (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("T");
      Val : Virtual_String := Consume_String_Or_Default (Args, 'T', "")
      with Unreferenced;

      T.Fail ("Consume_String_Or_Default No_Value_Kind should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Consume_String_Or_Default_Error_No_Value;

   procedure Test_Consume_String_Or_Default_Non_Existent (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X1");
      Val : constant Virtual_String := Consume_String_Or_Default (Args, 'T', "default");

      T.Assert (Val = "default");
   end Test_Consume_String_Or_Default_Non_Existent;

   procedure Test_Consume_String_Or_Default_String_Kind (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("A""foo""");
      Val : constant Virtual_String := Consume_String_Or_Default (Args, 'A', "bar");

      T.Assert (Val = "foo");
      T.Assert (Args.Arguments ('A').Consumed);
   end Test_Consume_String_Or_Default_String_Kind;

   procedure Test_Parse_Argument_No_Value_Followed_By_Param (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : constant Arguments := Parse_Arguments ("A B");

      T.Assert (Kind (Args, 'A') = No_Value_Kind);
      T.Assert (Kind (Args, 'B') = No_Value_Kind);
   end Test_Parse_Argument_No_Value_Followed_By_Param;

   procedure Test_Parse_Arguments_Comment (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : constant Arguments := Parse_Arguments ("; this is a comment");

      for C in Arguments_Index loop
         T.Assert (Kind (Args, C) = Non_Existent_Kind);
      end loop;
   end Test_Parse_Arguments_Comment;

   procedure Test_Parse_Arguments_Empty (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : constant Arguments := Parse_Arguments ("");

      for C in Arguments_Index loop
         T.Assert (Kind (Args, C) = Non_Existent_Kind);
      end loop;
   end Test_Parse_Arguments_Empty;

   procedure Test_Parse_Arguments_Error_Bad_Chars (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("!")
      with Unreferenced;

      T.Fail ("Bad characters should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Arguments_Error_Bad_Chars;

   procedure Test_Parse_Arguments_Error_Duplicate (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("G1 G2")
      with Unreferenced;

      T.Fail ("Duplicate parameter should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Arguments_Error_Duplicate;

   procedure Test_Parse_Arguments_Error_Invalid_Chars (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("1G")
      with Unreferenced;

      T.Fail ("Invalid character should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Arguments_Error_Invalid_Chars;

   procedure Test_Parse_Arguments_Error_Unterminated_String (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("G1 Z""hello")
      with Unreferenced;

      T.Fail ("Unterminated string should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Arguments_Error_Unterminated_String;

   procedure Test_Parse_Arguments_Integer_Boundary (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args_Min : Arguments := Parse_Arguments ("G0");
      Args_Max : Arguments := Parse_Arguments ("G999");

      T.Assert (Kind (Args_Min, 'G') = Integer_Kind);
      T.Assert (Consume_Integer (Args_Min, 'G') = 0);
      T.Assert (Kind (Args_Max, 'G') = Integer_Kind);
      T.Assert (Consume_Integer (Args_Max, 'G') = 999);
   end Test_Parse_Arguments_Integer_Boundary;

   procedure Test_Parse_Arguments_Mixed (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("G1 X10 Y20.5 Z""hello"" A");

      T.Assert (Kind (Args, 'G') = Integer_Kind);
      T.Assert (Consume_Integer (Args, 'G') = 1);
      T.Assert (Kind (Args, 'X') = Integer_Kind);
      T.Assert (Consume_Integer (Args, 'X') = 10);
      T.Assert (Kind (Args, 'Y') = Float_Kind);
      T.Assert (Consume_Float (Args, 'Y') = 20.5);
      T.Assert (Kind (Args, 'Z') = String_Kind);
      T.Assert (Consume_String (Args, 'Z') = "hello");
      T.Assert (Kind (Args, 'A') = No_Value_Kind);
      T.Assert (Consume_No_Value_Or_False (Args, 'A'));
   end Test_Parse_Arguments_Mixed;

   procedure Test_Parse_Arguments_Space_Between (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X 123");

      T.Assert (Kind (Args, 'X') = Integer_Kind);
      T.Assert (Consume_Integer (Args, 'X') = 123);
   end Test_Parse_Arguments_Space_Between;

   procedure Test_Parse_Arguments_Whitespace (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : constant Arguments := Parse_Arguments ("   ");

      for C in Arguments_Index loop
         T.Assert (Kind (Args, C) = Non_Existent_Kind);
      end loop;
   end Test_Parse_Arguments_Whitespace;

   procedure Test_Parse_Arguments_Whitespace_Around (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments (" G1  X10 ");

      T.Assert (Kind (Args, 'G') = Integer_Kind);
      T.Assert (Consume_Integer (Args, 'G') = 1);
      T.Assert (Kind (Args, 'X') = Integer_Kind);
      T.Assert (Consume_Integer (Args, 'X') = 10);
   end Test_Parse_Arguments_Whitespace_Around;

   procedure Test_Parse_Number_Error_Bad_Minus_Decimal (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("A.-1");

      T.Assert (False);
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Number_Error_Bad_Minus_Decimal;

   procedure Test_Parse_Number_Error_Bad_Minus_Double (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("A--1");

      T.Assert (False);
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Number_Error_Bad_Minus_Double;

   procedure Test_Parse_Number_Error_Bad_Minus_Middle (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("A1-1");

      T.Assert (False);
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Number_Error_Bad_Minus_Middle;

   procedure Test_Parse_Number_Error_Decimal_No_Number (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X.")
      with Unreferenced;

      T.Fail ("Decimal point with no number should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Number_Error_Decimal_No_Number;

   procedure Test_Parse_Number_Error_Minus_Decimal_No_Number (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X-.")
      with Unreferenced;

      T.Fail ("Minus sign with decimal point but no number should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Number_Error_Minus_Decimal_No_Number;

   procedure Test_Parse_Number_Error_Minus_No_Number (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X-")
      with Unreferenced;

      T.Fail ("Minus sign with no number should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Number_Error_Minus_No_Number;

   procedure Test_Parse_Number_Error_Minus_Not_At_Beginning (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X1-2")
      with Unreferenced;

      T.Fail ("Minus sign not at beginning should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Number_Error_Minus_Not_At_Beginning;

   procedure Test_Parse_Number_Error_Multiple_Decimal_Points (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X1.2.3")
      with Unreferenced;

      T.Fail ("Multiple decimal points should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Number_Error_Multiple_Decimal_Points;

   procedure Test_Parse_Number_Error_No_Preceding_Letter (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("-10")
      with Unreferenced;

      T.Fail ("Number without preceding letter should raise Parse_Error");
   exception
      when Parse_Error =>
         null;
   end Test_Parse_Number_Error_No_Preceding_Letter;

   procedure Test_Parse_Number_Float_Ends_With_Decimal (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X5.");

      T.Assert (Kind (Args, 'X') = Float_Kind);
      T.Assert (Consume_Float (Args, 'X') = 5.0);
   end Test_Parse_Number_Float_Ends_With_Decimal;

   procedure Test_Parse_Number_Float_Starts_With_Decimal (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X.5");

      T.Assert (Kind (Args, 'X') = Float_Kind);
      T.Assert (Consume_Float (Args, 'X') = 0.5);
   end Test_Parse_Number_Float_Starts_With_Decimal;

   procedure Test_Parse_Number_Huge_Integer_Overflow (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("A9999999999999999999999999999");

      T.Assert (Kind (Args, 'A') = Float_Kind);
      T.Assert (Consume_Float (Args, 'A') = 9999999999999999999999999999.0);
   end Test_Parse_Number_Huge_Integer_Overflow;

   procedure Test_Parse_Number_Large_Integer (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("A1000");

      T.Assert (Kind (Args, 'A') = Float_Kind);
      T.Assert (Consume_Float (Args, 'A') = 1000.0);
   end Test_Parse_Number_Large_Integer;

   procedure Test_Parse_Number_Negative_Float (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X-10.5");

      T.Assert (Kind (Args, 'X') = Float_Kind);
      T.Assert (Consume_Float (Args, 'X') = -10.5);
   end Test_Parse_Number_Negative_Float;

   procedure Test_Parse_Number_Negative_Integer (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("X-10");

      T.Assert (Kind (Args, 'X') = Float_Kind);
      T.Assert (Consume_Float (Args, 'X') = -10.0);
   end Test_Parse_Number_Negative_Integer;

   procedure Test_Parse_String_Empty (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("A""""");

      T.Assert (Kind (Args, 'A') = String_Kind);
      T.Assert (Consume_String (Args, 'A') = "");
   end Test_Parse_String_Empty;

   procedure Test_Parse_String_Immediate_Unterminated (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Args : Arguments := Parse_Arguments ("A""");

      T.Assert (False);
   exception
      when Parse_Error =>
         null;
   end Test_Parse_String_Immediate_Unterminated;

   procedure Test_Validate_All_Consumed (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      begin
         declare
            Args : Arguments := Parse_Arguments ("G1 X10");
         begin
            T.Assert (Consume_Integer (Args, 'G') = 1);
            Validate_All_Consumed (Args);
            T.Fail ("Not all consumed should raise Parse_Error");
         end;
      exception
         when Parse_Error =>
            null;
      end;

      declare
         Args : Arguments := Parse_Arguments ("G1 X10");
      begin
         T.Assert (Consume_Integer (Args, 'G') = 1);
         T.Assert (Consume_Integer (Args, 'X') = 10);
         Validate_All_Consumed (Args);
      end;
   end Test_Validate_All_Consumed;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Consume_Arguments'Access,
         Test_Consume_Error_Already_Consumed_Float'Access,
         Test_Consume_Error_Already_Consumed_Float_Or_Default'Access,
         Test_Consume_Error_Already_Consumed_Integer'Access,
         Test_Consume_Error_Already_Consumed_Integer_Or_Default'Access,
         Test_Consume_Error_Already_Consumed_No_Value'Access,
         Test_Consume_Error_Already_Consumed_String'Access,
         Test_Consume_Error_Already_Consumed_String_Or_Default'Access,
         Test_Consume_Float_Error_No_Value'Access,
         Test_Consume_Float_Error_Non_Existent'Access,
         Test_Consume_Float_Error_String_Kind'Access,
         Test_Consume_Float_Or_Default_Error_No_Value'Access,
         Test_Consume_Float_Or_Default_Error_String_Kind'Access,
         Test_Consume_Float_Or_Default_Float_Kind'Access,
         Test_Consume_Float_Or_Default_Integer_Kind'Access,
         Test_Consume_Float_Or_Default_Non_Existent'Access,
         Test_Consume_Integer_Error_Float_Kind'Access,
         Test_Consume_Integer_Error_No_Value'Access,
         Test_Consume_Integer_Error_Non_Existent'Access,
         Test_Consume_Integer_Error_String_Kind'Access,
         Test_Consume_Integer_Or_Default_Error_Float_Kind'Access,
         Test_Consume_Integer_Or_Default_Error_No_Value'Access,
         Test_Consume_Integer_Or_Default_Error_String_Kind'Access,
         Test_Consume_Integer_Or_Default_Integer_Kind'Access,
         Test_Consume_Integer_Or_Default_Non_Existent'Access,
         Test_Consume_No_Value_Or_False_Error_Float_Kind'Access,
         Test_Consume_No_Value_Or_False_Error_Integer_Kind'Access,
         Test_Consume_No_Value_Or_False_Error_String_Kind'Access,
         Test_Consume_No_Value_Or_False_Non_Existent'Access,
         Test_Consume_String_Error_Float_Kind'Access,
         Test_Consume_String_Error_Integer_Kind'Access,
         Test_Consume_String_Error_No_Value'Access,
         Test_Consume_String_Error_Non_Existent'Access,
         Test_Consume_String_Or_Default_Error_Float_Kind'Access,
         Test_Consume_String_Or_Default_Error_Integer_Kind'Access,
         Test_Consume_String_Or_Default_Error_No_Value'Access,
         Test_Consume_String_Or_Default_Non_Existent'Access,
         Test_Consume_String_Or_Default_String_Kind'Access,
         Test_Parse_Argument_No_Value_Followed_By_Param'Access,
         Test_Parse_Arguments_Comment'Access,
         Test_Parse_Arguments_Empty'Access,
         Test_Parse_Arguments_Error_Bad_Chars'Access,
         Test_Parse_Arguments_Error_Duplicate'Access,
         Test_Parse_Arguments_Error_Invalid_Chars'Access,
         Test_Parse_Arguments_Error_Unterminated_String'Access,
         Test_Parse_Arguments_Integer_Boundary'Access,
         Test_Parse_Arguments_Mixed'Access,
         Test_Parse_Arguments_Space_Between'Access,
         Test_Parse_Arguments_Whitespace'Access,
         Test_Parse_Arguments_Whitespace_Around'Access,
         Test_Parse_Number_Error_Bad_Minus_Decimal'Access,
         Test_Parse_Number_Error_Bad_Minus_Double'Access,
         Test_Parse_Number_Error_Bad_Minus_Middle'Access,
         Test_Parse_Number_Error_Decimal_No_Number'Access,
         Test_Parse_Number_Error_Minus_Decimal_No_Number'Access,
         Test_Parse_Number_Error_Minus_No_Number'Access,
         Test_Parse_Number_Error_Minus_Not_At_Beginning'Access,
         Test_Parse_Number_Error_Multiple_Decimal_Points'Access,
         Test_Parse_Number_Error_No_Preceding_Letter'Access,
         Test_Parse_Number_Float_Ends_With_Decimal'Access,
         Test_Parse_Number_Float_Starts_With_Decimal'Access,
         Test_Parse_Number_Huge_Integer_Overflow'Access,
         Test_Parse_Number_Large_Integer'Access,
         Test_Parse_Number_Negative_Float'Access,
         Test_Parse_Number_Negative_Integer'Access,
         Test_Parse_String_Empty'Access,
         Test_Parse_String_Immediate_Unterminated'Access,
         Test_Validate_All_Consumed'Access];
   end All_Tests;

end Prunt.Gcode_Arguments.Test;
