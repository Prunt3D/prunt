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

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Exceptions.Is_Null_Occurrence;
with Ada.Text_IO;
with Prunt.Logger;
with Ada.Characters;
with Ada.Characters.Latin_1;

package body Prunt is

   protected body Fatal_Exception_Occurrence_Holder_Type is
      procedure Set
        (Cause      : Ada.Task_Termination.Cause_Of_Termination;
         ID         : Ada.Task_Identification.Task_Id;
         Occurrence : Ada.Exceptions.Exception_Occurrence) is
      begin
         if Ada.Exceptions.Is_Null_Occurrence (Data) then
            Ada.Exceptions.Save_Occurrence (Data, Occurrence);
         end if;

         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Occurrence));
      end Set;

      entry Get (Occurrence : out Ada.Exceptions.Exception_Occurrence)
        when not Ada.Exceptions.Is_Null_Occurrence (Data)
      is
      begin
         Ada.Exceptions.Save_Occurrence (Occurrence, Data);
      end Get;

      function Is_Set return Boolean
      is (not Ada.Exceptions.Is_Null_Occurrence (Data));

      function Null_Occurrence return Ada.Exceptions.Exception_Occurrence is
      begin
         return X : Ada.Exceptions.Exception_Occurrence do
            Ada.Exceptions.Save_Occurrence (X, Ada.Exceptions.Null_Occurrence);
         end return;
      end Null_Occurrence;
   end Fatal_Exception_Occurrence_Holder_Type;

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensioned_Float);
   use Math;

   function "*" (Left : Position; Right : Position_Scale) return Scaled_Position is
   begin
      return [for I in Axis_Name => Left (I) * Right (I)];
   end "*";

   function "*" (Left : Position_Offset; Right : Position_Scale) return Position_Offset is
   begin
      return [for I in Axis_Name => Left (I) * Right (I)];
   end "*";

   function "*" (Left : Position_Scale; Right : Dimensionless) return Position_Scale is
   begin
      return [for I in Axis_Name => Left (I) * Right];
   end "*";

   function "*" (Left : Position_Scale; Right : Length) return Scaled_Position_Offset is
   begin
      return [for I in Axis_Name => Left (I) * Right];
   end "*";

   function "*" (Left : Position_Scale; Right : Velocity) return Axial_Velocities is
   begin
      return [for I in Axis_Name => Left (I) * Right];
   end "*";

   function "*" (Left : Scaled_Position; Right : Position_Scale) return Scaled_Position is
   begin
      return [for I in Axis_Name => Left (I) * Right (I)];
   end "*";

   function "*" (Left : Scaled_Position; Right : Dimensionless) return Scaled_Position is
   begin
      return [for I in Axis_Name => Left (I) * Right];
   end "*";

   function "*" (Left : Scaled_Position_Offset; Right : Position_Scale) return Scaled_Position_Offset is
   begin
      return [for I in Axis_Name => Left (I) * Right (I)];
   end "*";

   function "*" (Left : Scaled_Position_Offset; Right : Dimensionless) return Scaled_Position_Offset is
   begin
      return [for I in Axis_Name => Left (I) * Right];
   end "*";

   function "+" (Left : Scaled_Position; Right : Scaled_Position_Offset) return Scaled_Position is
   begin
      return [for I in Axis_Name => Left (I) + Right (I)];
   end "+";

   function "+" (Left, Right : Position_Scale) return Position_Scale is
   begin
      return [for I in Axis_Name => Left (I) + Right (I)];
   end "+";

   function "+" (Left : Position; Right : Position_Offset) return Position is
   begin
      return [for I in Axis_Name => Left (I) + Right (I)];
   end "+";

   function "-" (Left, Right : Position) return Position_Offset is
   begin
      return [for I in Axis_Name => Left (I) - Right (I)];
   end "-";

   function "-" (Left, Right : Position_Scale) return Position_Scale is
   begin
      return [for I in Axis_Name => Left (I) - Right (I)];
   end "-";

   function "-" (Left, Right : Scaled_Position) return Scaled_Position_Offset is
   begin
      return [for I in Axis_Name => Left (I) - Right (I)];
   end "-";

   function "-" (Left, Right : Scaled_Position_Offset) return Scaled_Position_Offset is
   begin
      return [for I in Axis_Name => Left (I) - Right (I)];
   end "-";

   function "-" (Left : Scaled_Position; Right : Scaled_Position_Offset) return Scaled_Position is
   begin
      return [for I in Axis_Name => Left (I) - Right (I)];
   end "-";

   function "-" (Left : Position; Right : Position_Offset) return Position is
   begin
      return [for I in Axis_Name => Left (I) - Right (I)];
   end "-";

   function "/" (Left : Axial_Velocities; Right : Position_Scale) return Axial_Velocities is
   begin
      return [for I in Axis_Name => Left (I) / Right (I)];
   end "/";

   function "/" (Left : Position_Offset; Right : Length) return Position_Scale is
   begin
      return [for I in Axis_Name => Left (I) / Right];
   end "/";

   function "/" (Left : Position_Scale; Right : Dimensionless) return Position_Scale is
   begin
      return [for I in Axis_Name => Left (I) / Right];
   end "/";

   function "/" (Left : Scaled_Position_Offset; Right : Length) return Position_Scale is
   begin
      return [for I in Axis_Name => Left (I) / Right];
   end "/";

   function "/" (Left : Scaled_Position; Right : Dimensionless) return Scaled_Position is
   begin
      return [for I in Axis_Name => Left (I) / Right];
   end "/";

   function "/" (Left : Scaled_Position; Right : Position_Scale) return Scaled_Position is
   begin
      return [for I in Axis_Name => Left (I) / Right (I)];
   end "/";

   function "/" (Left : Position; Right : Position_Scale) return Scaled_Position is
   begin
      return [for I in Axis_Name => Left (I) / Right (I)];
   end "/";

   function "/" (Left : Scaled_Position_Offset; Right : Position_Scale) return Scaled_Position_Offset is
   begin
      return [for I in Axis_Name => Left (I) / Right (I)];
   end "/";

   function "abs" (Left : Position_Offset) return Length is
      Square_Sum : Area := 0.0 * mm**2;
   begin
      for X of Left loop
         Square_Sum := Square_Sum + X * X;
      end loop;

      return Sqrt (Square_Sum);
   end "abs";

   function "abs" (Left : Position_Scale) return Dimensionless is
      Square_Sum : Dimensionless := 0.0;
   begin
      for X of Left loop
         Square_Sum := Square_Sum + X * X;
      end loop;

      return Sqrt (Square_Sum);
   end "abs";

   function "abs" (Left : Scaled_Position_Offset) return Length is
      Square_Sum : Area := 0.0 * mm**2;
   begin
      for X of Left loop
         Square_Sum := Square_Sum + X * X;
      end loop;

      return Sqrt (Square_Sum);
   end "abs";

   function Dot (Left, Right : Position_Scale) return Dimensionless is
      Sum : Dimensionless := 0.0;
   begin
      for I in Axis_Name loop
         Sum := Sum + Left (I) * Right (I);
      end loop;

      return Sum;
   end Dot;

   function Dot (Left : Scaled_Position_Offset; Right : Position_Scale) return Length is
      Sum : Length := 0.0 * mm;
   begin
      for I in Axis_Name loop
         Sum := Sum + Left (I) * Right (I);
      end loop;

      return Sum;
   end Dot;

   function Dot (Left, Right : Scaled_Position_Offset) return Area is
      Sum : Area := 0.0 * mm**2;
   begin
      for I in Axis_Name loop
         Sum := Sum + Left (I) * Right (I);
      end loop;

      return Sum;
   end Dot;

   function JSON_Escape_Character (C : Character) return String is
   begin
      case C is
         when Character'Val (16#00#) =>
            return "\u0000";

         when Character'Val (16#01#) =>
            return "\u0001";

         when Character'Val (16#02#) =>
            return "\u0002";

         when Character'Val (16#03#) =>
            return "\u0003";

         when Character'Val (16#04#) =>
            return "\u0004";

         when Character'Val (16#05#) =>
            return "\u0005";

         when Character'Val (16#06#) =>
            return "\u0006";

         when Character'Val (16#07#) =>
            return "\u0007";

         when Ada.Characters.Latin_1.BS =>
            return "\b";

         when Ada.Characters.Latin_1.HT =>
            return "\t";

         when Ada.Characters.Latin_1.LF =>
            return "\n";

         when Character'Val (16#0B#) =>
            return "\u000B";

         when Ada.Characters.Latin_1.FF =>
            return "\f";

         when Ada.Characters.Latin_1.CR =>
            return "\r";

         when Character'Val (16#0E#) =>
            return "\u000E";

         when Character'Val (16#0F#) =>
            return "\u000F";

         when Character'Val (16#10#) =>
            return "\u0010";

         when Character'Val (16#11#) =>
            return "\u0011";

         when Character'Val (16#12#) =>
            return "\u0012";

         when Character'Val (16#13#) =>
            return "\u0013";

         when Character'Val (16#14#) =>
            return "\u0014";

         when Character'Val (16#15#) =>
            return "\u0015";

         when Character'Val (16#16#) =>
            return "\u0016";

         when Character'Val (16#17#) =>
            return "\u0017";

         when Character'Val (16#18#) =>
            return "\u0018";

         when Character'Val (16#19#) =>
            return "\u0019";

         when Character'Val (16#1A#) =>
            return "\u001A";

         when Character'Val (16#1B#) =>
            return "\u001B";

         when Character'Val (16#1C#) =>
            return "\u001C";

         when Character'Val (16#1D#) =>
            return "\u001D";

         when Character'Val (16#1E#) =>
            return "\u001E";

         when Character'Val (16#1F#) =>
            return "\u001F";

         when '\' =>
            return "\\";

         when '"' =>
            return "\""";

         when others =>
            return (1 => C);
      end case;
   end JSON_Escape_Character;

   function JSON_Escape (S : String) return String is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for C of S loop
         Ada.Strings.Unbounded.Append (Result, JSON_Escape_Character (C));
      end loop;

      return Ada.Strings.Unbounded.To_String (Result);
   end JSON_Escape;

   function JSON_Escape (S : Ada.Strings.Unbounded.Unbounded_String) return Ada.Strings.Unbounded.Unbounded_String is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for C of Ada.Strings.Unbounded.To_String (S) loop
         Ada.Strings.Unbounded.Append (Result, JSON_Escape_Character (C));
      end loop;

      return Result;
   end JSON_Escape;

end Prunt;
