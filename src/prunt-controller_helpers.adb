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

package body Prunt.Controller_Helpers is

   pragma Extensions_Allowed (On);

   function "<" (Left, Right : Gcode_Dispatch_Key) return Boolean is
      use Gcode_Arguments;
   begin
      pragma Warnings (Off, "comparison on unordered enumeration type ""Gcode_Identifier_Argument_Index""");
      --  We do not care what the order is here, we just need something to sort with.
      if Left.Identifier.Argument /= Right.Identifier.Argument then
         return Left.Identifier.Argument < Right.Identifier.Argument;
      elsif Left.Identifier.Number /= Right.Identifier.Number then
         return Left.Identifier.Number < Right.Identifier.Number;
      else
         return Left.Argument_Kinds < Right.Argument_Kinds;
      end if;
      pragma Warnings (On, "comparison on unordered enumeration type ""Gcode_Identifier_Argument_Index""");
   end "<";

   function Build_Gcode_Dispatch_Map (Active_Modules : Module_Maps.Map) return Gcode_Dispatch_Maps.Map is
      Active_Module_Gcode_Dispatch_Map : Gcode_Dispatch_Maps.Map := [];
   begin
      for C in Active_Modules.Iterate loop
         for G of Module_Maps.Element (C).Gcode_Commands loop
            declare
               use Prunt.Gcode_Arguments;
               use Prunt.Module_Types;

               procedure Recursive_Insert
                 (Current_Index : Arguments_Index; Current_Kinds : Gcode_Dispatch_Argument_Kinds)
               is
                  Allowed_Kinds : Gcode_Argument_Allowed_Kinds := [others => False];
               begin
                  if Current_Index = G.Identifier.Argument then
                     Allowed_Kinds (Integer_Kind) := True;
                  elsif Current_Index in Gcode_Identifier_Argument_Index then
                     Allowed_Kinds (Non_Existent_Kind) := True;
                  elsif G.Arguments.Contains (Current_Index) then
                     Allowed_Kinds := G.Arguments (Current_Index).Allowed_Kinds;
                  else
                     Allowed_Kinds (Non_Existent_Kind) := True;
                  end if;

                  for Kind in Argument_Kind loop
                     if Allowed_Kinds (Kind) then
                        if Current_Index = Arguments_Index'Last then
                           Active_Module_Gcode_Dispatch_Map.Insert
                             ((Identifier     => G.Identifier,
                               Argument_Kinds => (Current_Kinds with delta Current_Index => Kind)),
                              Module_Maps.Key (C));
                        else
                           Recursive_Insert
                             (Arguments_Index'Succ (Current_Index), (Current_Kinds with delta Current_Index => Kind));
                        end if;
                     end if;
                  end loop;
               end Recursive_Insert;
            begin
               Recursive_Insert (Arguments_Index'First, [others => Non_Existent_Kind]);
            end;
         end loop;
      end loop;

      return Active_Module_Gcode_Dispatch_Map;
   end Build_Gcode_Dispatch_Map;

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
