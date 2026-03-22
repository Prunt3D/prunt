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

with Ada.Strings;
with Ada.Strings.Fixed;
with VSS.Strings.Conversions;

package body Prunt.Controller_Helpers is

   pragma Extensions_Allowed (On);

   function Identifier_Image (Identifier : Module_Types.Gcode_Command_Identifier) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      --  Using a case here is a bit cleaner than doing 'Pos and 'Val shenanigans to convert the Wide_Wide_Character to
      --  a Character. GNAT will warn us if we add new argument letters to the static predicate without updating this.
      case Identifier.Argument is
         when 'G' =>
            return "G" & Trim (Integer (Identifier.Number)'Image, Both);

         when 'M' =>
            return "M" & Trim (Integer (Identifier.Number)'Image, Both);
      end case;
   end Identifier_Image;

   function Are_Kinds_Disjoint (Left, Right : Module_Types.Gcode_Argument_Allowed_Kinds) return Boolean is
      use Gcode_Arguments;
   begin
      for Kind in Argument_Kind loop
         if Left (Kind) and then Right (Kind) then
            return False;
         end if;
      end loop;

      return True;
   end Are_Kinds_Disjoint;

   function Overlaps (Left, Right : Gcode_Dispatch_Entry) return Boolean is
      use Module_Types;
   begin
      for Index in Gcode_User_Argument_Index loop
         if Are_Kinds_Disjoint (Left.Argument_Kinds (Index), Right.Argument_Kinds (Index)) then
            return False;
         end if;
      end loop;

      return True;
   end Overlaps;

   function Build_Dispatch_Entry
     (Module_Name : Virtual_String; Command : Module_Types.Gcode_Command) return Gcode_Dispatch_Entry
   is
      use Gcode_Arguments;

      Result : Gcode_Dispatch_Argument_Kinds := [others => [Non_Existent_Kind => True, others => False]];
   begin
      for C in Command.Arguments.Iterate loop
         Result (C.Key) := C.Element.Allowed_Kinds;
      end loop;

      return (Module_Name => Module_Name, Argument_Kinds => Result);
   end Build_Dispatch_Entry;

   function Build_Gcode_Dispatch_Map (Active_Modules : Module_Maps.Map) return Gcode_Dispatch_Maps.Map is
      use Gcode_Dispatch_Entry_Vectors;
      use Gcode_Dispatch_Maps;

      Active_Module_Gcode_Dispatch_Map : Gcode_Dispatch_Maps.Map := [];
   begin
      for C in Active_Modules.Iterate loop
         for G of Module_Maps.Element (C).Gcode_Commands loop
            declare
               New_Entry : constant Gcode_Dispatch_Entry := Build_Dispatch_Entry (Module_Maps.Key (C), G);
            begin
               if Active_Module_Gcode_Dispatch_Map.Contains (G.Identifier) then
                  declare
                     Entries : Vector := Active_Module_Gcode_Dispatch_Map.Element (G.Identifier);
                  begin
                     for Existing_Entry of Entries loop
                        if Overlaps (Existing_Entry, New_Entry) then
                           raise Program_Error
                             with
                               "G-code command overlap detected for "
                               & Identifier_Image (G.Identifier)
                               & " between modules """
                               & Conversions.To_UTF_8_String (Existing_Entry.Module_Name)
                               & """ and """
                               & Conversions.To_UTF_8_String (New_Entry.Module_Name)
                               & """.";
                        end if;
                     end loop;

                     Active_Module_Gcode_Dispatch_Map.Replace (G.Identifier, Entries & New_Entry);
                  end;
               else
                  Active_Module_Gcode_Dispatch_Map.Insert (G.Identifier, [New_Entry]);
               end if;
            end;
         end loop;
      end loop;

      return Active_Module_Gcode_Dispatch_Map;
   end Build_Gcode_Dispatch_Map;

   function Matches (Dispatch_Entry : Gcode_Dispatch_Entry; Args : Gcode_Arguments.Arguments) return Boolean is
      use Gcode_Arguments;
      use Module_Types;
   begin
      for Index in Gcode_User_Argument_Index loop
         if not Dispatch_Entry.Argument_Kinds (Index) (Kind (Args, Index)) then
            return False;
         end if;
      end loop;

      return True;
   end Matches;

   function Find_Module_Name
     (Dispatch_Map : Gcode_Dispatch_Maps.Map;
      Identifier   : Module_Types.Gcode_Command_Identifier;
      Args         : Gcode_Arguments.Arguments) return Virtual_String
   is
      Result : Virtual_String;
   begin
      if not Dispatch_Map.Contains (Identifier) then
         return "";
      end if;

      for Dispatch_Entry of Dispatch_Map.Element (Identifier) loop
         if Matches (Dispatch_Entry, Args) then
            if Result.Is_Empty then
               Result := Dispatch_Entry.Module_Name;
            else
               raise Program_Error with "Ambiguous G-code dispatch for " & Identifier_Image (Identifier) & ".";
            end if;
         end if;
      end loop;

      return Result;
   end Find_Module_Name;

   function Build_Gcode_JSON (Active_Modules : Module_Maps.Map) return JSON.JSON_Value is
      use Prunt.JSON;
      Root_Object : constant JSON_Value := Create_Object;
   begin
      for C in Active_Modules.Iterate loop
         declare
            Module_Commands : JSON_Array := Empty_Array;
         begin
            for G of Module_Maps.Element (C).Gcode_Commands loop
               declare
                  use Prunt.Gcode_Arguments;
                  use Prunt.Module_Types;

                  Command_Object    : constant JSON_Value := Create_Object;
                  Identifier_Object : constant JSON_Value := Create_Object;
                  Args_Object       : constant JSON_Value := Create_Object;
               begin
                  Identifier_Object.Set_Field ("Argument", To_Virtual_String ("" & G.Identifier.Argument));
                  Identifier_Object.Set_Field ("Number", Integer (G.Identifier.Number));

                  Command_Object.Set_Field ("Identifier", Identifier_Object);
                  Command_Object.Set_Field ("Name", G.Name);
                  Command_Object.Set_Field ("Description", G.Description);

                  for Arg in G.Arguments.Iterate loop
                     declare
                        Arg_Object        : constant JSON_Value := Create_Object;
                        Allowed_Kinds_Arr : JSON_Array := Empty_Array;
                     begin
                        Arg_Object.Set_Field ("Description", Arg.Element.Description);

                        for Kind in Argument_Kind when Arg.Element.Allowed_Kinds (Kind) loop
                           case Kind is
                              when Non_Existent_Kind =>
                                 Allowed_Kinds_Arr.Append (Create ("Non_Existent"));

                              when No_Value_Kind     =>
                                 Allowed_Kinds_Arr.Append (Create ("No_Value"));

                              when Integer_Kind      =>
                                 Allowed_Kinds_Arr.Append (Create ("Integer"));

                              when Float_Kind        =>
                                 Allowed_Kinds_Arr.Append (Create ("Real"));

                              when String_Kind       =>
                                 Allowed_Kinds_Arr.Append (Create ("String"));
                           end case;
                        end loop;
                        Arg_Object.Set_Field ("Allowed_Kinds", Allowed_Kinds_Arr);
                        Args_Object.Set_Field (To_Virtual_String ("" & Arg.Key), Arg_Object);
                     end;
                  end loop;

                  Command_Object.Set_Field ("Arguments", Args_Object);
                  Module_Commands.Append (Command_Object);
               end;
            end loop;

            Root_Object.Set_Field (Module_Maps.Key (C), Module_Commands);
         end;
      end loop;

      return Root_Object;
   end Build_Gcode_JSON;

end Prunt.Controller_Helpers;
