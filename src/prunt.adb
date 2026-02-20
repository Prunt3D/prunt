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

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed;
with VSS.Strings.Conversions;

package body Prunt is

   pragma Extensions_Allowed (On);

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
      Square_Sum : Area := 0.0 * mm ** 2;
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
      Square_Sum : Area := 0.0 * mm ** 2;
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
      Sum : Area := 0.0 * mm ** 2;
   begin
      for I in Axis_Name loop
         Sum := Sum + Left (I) * Right (I);
      end loop;

      return Sum;
   end Dot;

   protected body Test_File_Name_Generator is
      procedure Get_Next (Name : out Virtual_String) is
      begin
         Counter := @ + 1;
         Name := +("/tmp/prunt_tests/test_file_" & Ada.Strings.Fixed.Trim (Counter'Image, Ada.Strings.Both));
      end Get_Next;
   end Test_File_Name_Generator;

   function Next_Test_Filename return String is
      Name : Virtual_String;
   begin
      Test_File_Name_Generator.Get_Next (Name);
      return Conversions.To_UTF_8_String (Name);
   end Next_Test_Filename;

   function Next_Test_Filename return Virtual_String is
      Name : Virtual_String;
   begin
      Test_File_Name_Generator.Get_Next (Name);
      return Name;
   end Next_Test_Filename;

end Prunt;
