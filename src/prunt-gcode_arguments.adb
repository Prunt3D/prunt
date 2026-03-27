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

with Ada.Assertions;                  use Ada.Assertions;
with VSS.Characters;                  use VSS.Characters;
with VSS.Strings.Character_Iterators; use VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;         use VSS.Strings.Conversions;
with VSS.Strings.Markers;             use VSS.Strings.Markers;

package body Prunt.Gcode_Arguments is

   pragma Extensions_Allowed (On);

   function Parse_Arguments (Line : Virtual_String) return Arguments is
      Args : Arguments_Array := [others => (Kind => Non_Existent_Kind, Consumed => False)];
      Iter : Character_Iterator := Line.Before_First_Character;

      use type VSS.Characters.Virtual_Character;

      procedure Parse_Number (Index : Arguments_Index) is
         In_Decimal_Part : Boolean := False;
         Is_Negative     : Boolean := False;
         Has_Digit       : Boolean := False;
         Start_Marker    : Character_Marker;
      begin
         Start_Marker := Iter.Marker;

         loop
            if Iter.Element not in '.' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' then
               --  0 to 9 might not be a contiguous range for a given VSS encoding.
               Assert (Iter.Backward);
               exit;
            end if;

            if Iter.Element = '-' then
               if Is_Negative or else In_Decimal_Part or else Has_Digit then
                  raise Parse_Error with "'-' only allowed as first character in number.";
               end if;
               Is_Negative := True;
            elsif Iter.Element = '.' then
               if In_Decimal_Part then
                  raise Parse_Error with "Multiple decimal points in number.";
               end if;
               In_Decimal_Part := True;
            else
               Has_Digit := True;
            end if;

            if not Iter.Forward then
               exit;
            end if;
         end loop;

         if not Has_Digit then
            if In_Decimal_Part then
               raise Parse_Error with "Got '.' with no preceding or following number.";
            else
               Assert (Is_Negative);
               raise Parse_Error with "Got '-' with no following number.";
            end if;
         end if;

         declare
            Num_String : constant String := To_UTF_8_String (Line.Slice (Start_Marker, Iter.Marker));
         begin
            if In_Decimal_Part or else Is_Negative then
               Args (Index) :=
                 (Kind => Float_Kind, Float_Value => Dimensionless'Value (Num_String), Consumed => False);
            else
               begin
                  declare
                     Val : constant Integer := Integer'Value (Num_String);
                  begin
                     if Val in 0 .. 999 then
                        Args (Index) :=
                          (Kind => Integer_Kind, Integer_Value => Argument_Integer (Val), Consumed => False);
                     else
                        Args (Index) := (Kind => Float_Kind, Float_Value => Dimensionless (Val), Consumed => False);
                     end if;
                  end;
               exception
                  when Constraint_Error =>
                     Args (Index) :=
                       (Kind => Float_Kind, Float_Value => Dimensionless'Value (Num_String), Consumed => False);
               end;
            end if;
         end;
      end Parse_Number;

      procedure Parse_String (Index : Arguments_Index; Delimiter : Virtual_Character) is
         Start_String : constant Character_Marker := Iter.Marker;
      begin
         while Iter.Element /= Delimiter loop
            if not Iter.Forward then
               raise Parse_Error with "Unterminated string.";
            end if;
         end loop;

         Assert (Iter.Backward);
         Args (Index) :=
           (Kind => String_Kind, String_Value => Line.Slice (Start_String, Iter.Marker), Consumed => False);
         Assert (Iter.Forward);
      end Parse_String;

      procedure Parse_Argument (Index : Arguments_Index) is
      begin
         Args (Index) := (Kind => No_Value_Kind, Consumed => False);

         loop
            if not Iter.Forward then
               return;
            end if;
            if Iter.Element.Get_General_Category /= Space_Separator then
               exit;
            end if;
         end loop;

         if Iter.Element in '"' | ''' then
            declare
               Delimiter : constant Virtual_Character := Iter.Element;
            begin
               if not Iter.Forward then
                  raise Parse_Error with "Unterminated string.";
               end if;
               Parse_String (Index, Delimiter);
            end;
         elsif Iter.Element in '.' | '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' then
            --  0 to 9 might not be a contiguous range for a given VSS encoding.
            Parse_Number (Index);
         else
            Assert (Iter.Backward);
         end if;
      end Parse_Argument;
   begin
      while Iter.Forward and then Iter.Element /= ';' loop
         if Iter.Element.Get_General_Category = Space_Separator then
            null;
         elsif Wide_Wide_Character (Iter.Element.Get_Simple_Uppercase_Mapping) in Arguments_Index'Range then
            declare
               Index : constant Arguments_Index := Arguments_Index (Iter.Element.Get_Simple_Uppercase_Mapping);
            begin
               if Args (Index).Kind /= Non_Existent_Kind then
                  raise Parse_Error with "Parameter letter " & Index'Image & " encountered more than once on line.";
               end if;

               Parse_Argument (Index);
            end;
         else
            raise Parse_Error with "Expected parameter letter, got invalid " & Iter.Element'Image & ".";
         end if;
      end loop;

      return (Line => Line, Arguments => Args);
   end Parse_Arguments;

   function Kind (Args : Arguments; Index : Arguments_Index) return Argument_Kind is
   begin
      return Args.Arguments (Index).Kind;
   end Kind;

   function Consume_Float_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : Dimensionless) return Dimensionless is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter " & Index'Image & " already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind     =>
            raise Parse_Error
              with "Parameter " & Index'Image & " has no value in command requiring number or omission.";

         when Non_Existent_Kind =>
            return Default;

         when Integer_Kind      =>
            return Dimensionless (Args.Arguments (Index).Integer_Value);

         when Float_Kind        =>
            return Args.Arguments (Index).Float_Value;

         when String_Kind       =>
            raise Parse_Error with "Parameter " & Index'Image & " is string where number was expected.";
      end case;
   end Consume_Float_Or_Default;

   function Consume_Float (Args : in out Arguments; Index : Arguments_Index) return Dimensionless is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter " & Index'Image & " already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind     =>
            raise Parse_Error with "Parameter " & Index'Image & " has no value in command requiring number.";

         when Non_Existent_Kind =>
            raise Parse_Error with "Parameter " & Index'Image & " missing in command requiring number.";

         when Integer_Kind      =>
            return Dimensionless (Args.Arguments (Index).Integer_Value);

         when Float_Kind        =>
            return Args.Arguments (Index).Float_Value;

         when String_Kind       =>
            raise Parse_Error with "Parameter " & Index'Image & " is string where number was expected.";
      end case;
   end Consume_Float;

   function Consume_Integer_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : Argument_Integer) return Argument_Integer is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter " & Index'Image & " already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind            =>
            raise Parse_Error
              with "Parameter " & Index'Image & " has no value in command requiring integer or omission.";

         when Non_Existent_Kind        =>
            return Default;

         when Integer_Kind             =>
            return Args.Arguments (Index).Integer_Value;

         when Float_Kind | String_Kind =>
            raise Parse_Error with "Parameter " & Index'Image & " should be non-negative integer < 1000.";
      end case;
   end Consume_Integer_Or_Default;

   function Consume_Integer (Args : in out Arguments; Index : Arguments_Index) return Argument_Integer is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter " & Index'Image & " already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind            =>
            raise Parse_Error with "Parameter " & Index'Image & " has no value in command requiring value.";

         when Non_Existent_Kind        =>
            raise Parse_Error with "Parameter " & Index'Image & " missing in command requiring value.";

         when Integer_Kind             =>
            return Args.Arguments (Index).Integer_Value;

         when Float_Kind | String_Kind =>
            raise Parse_Error with "Parameter " & Index'Image & " should be non-negative integer < 1000.";
      end case;
   end Consume_Integer;

   function Consume_String_Or_Default
     (Args : in out Arguments; Index : Arguments_Index; Default : Virtual_String) return Virtual_String is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter " & Index'Image & " already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind             =>
            raise Parse_Error with "Parameter " & Index'Image & " has no value in command requiring string.";

         when Non_Existent_Kind         =>
            return Default;

         when Integer_Kind | Float_Kind =>
            raise Parse_Error with "Parameter " & Index'Image & " should be a string surrounded by "" or '.";

         when String_Kind               =>
            return Args.Arguments (Index).String_Value;
      end case;
   end Consume_String_Or_Default;

   function Consume_String (Args : in out Arguments; Index : Arguments_Index) return Virtual_String is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter " & Index'Image & " already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind             =>
            raise Parse_Error with "Parameter " & Index'Image & " has no value in command requiring string.";

         when Non_Existent_Kind         =>
            raise Parse_Error with "Parameter " & Index'Image & " missing in command requiring string.";

         when Integer_Kind | Float_Kind =>
            raise Parse_Error with "Parameter " & Index'Image & " should be a string surrounded by "" or '.";

         when String_Kind               =>
            return Args.Arguments (Index).String_Value;
      end case;
   end Consume_String;

   function Consume_No_Value_Or_False (Args : in out Arguments; Index : Arguments_Index) return Boolean is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter " & Index'Image & " already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind                           =>
            return True;

         when Non_Existent_Kind                       =>
            return False;

         when Integer_Kind | Float_Kind | String_Kind =>
            raise Parse_Error with "Parameter " & Index'Image & " not allowed to have a value here.";
      end case;
   end Consume_No_Value_Or_False;

   function Consume_No_Value (Args : in out Arguments; Index : Arguments_Index) return Boolean is
   begin
      if Args.Arguments (Index).Consumed then
         raise Constraint_Error with "Parameter " & Index'Image & " already consumed.";
      end if;
      Args.Arguments (Index).Consumed := True;

      case Args.Arguments (Index).Kind is
         when No_Value_Kind                           =>
            return True;

         when Non_Existent_Kind                       =>
            raise Parse_Error with "Parameter " & Index'Image & " missing.";

         when Integer_Kind | Float_Kind | String_Kind =>
            raise Parse_Error with "Parameter " & Index'Image & " not allowed to have a value here.";
      end case;
   end Consume_No_Value;

   procedure Validate_All_Consumed (Args : Arguments) is
   begin
      for I in Args.Arguments'Range loop
         if not Args.Arguments (I).Consumed and then Args.Arguments (I).Kind /= Non_Existent_Kind then
            raise Parse_Error with "Parameter " & I'Image & " not valid for command on line.";
         end if;
      end loop;
   end Validate_All_Consumed;

end Prunt.Gcode_Arguments;
