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

generic
   type T is digits <>;
package Prunt.Moving_Averages is

   type Cascading_Moving_Average
     (N_Levels        : Positive;
      Width_Per_Level : Natural)
   is
     private;

   function Create (N_Levels : Positive; Max_Total_Width : Natural; Initial_Value : T) return Cascading_Moving_Average;
   --  Creates a new filter with the requested parameters. `Max_Total_Width` is a maximum rather than an exact value as
   --  it must be split evenly between `N_Levels` levels.

   function Total_Delay (CMA : Cascading_Moving_Average) return Natural
   is ((CMA.N_Levels * (CMA.Width_Per_Level - 1)) / 2);
   --  Returns the number of cycles from when a signal enters the filter to when the effect of the signal on the output
   --  is maximised. This may not be equal to `Max_Total_Width` / 2 as provided to `Create` as that value may be
   --  reduced to suit the number of layers specified.

   function Do_Step (CMA : in out Cascading_Moving_Average; Input : T) return T;

private

   subtype N_Levels_Type is Positive;
   subtype Width_Per_Level_Type is Natural;

   type Single_Moving_Average_Sums is array (N_Levels_Type range <>) of T;
   type Single_Moving_Average_Buffers is array (N_Levels_Type range <>, Width_Per_Level_Type range <>) of T;

   type Cascading_Moving_Average
     (N_Levels        : Positive;
      Width_Per_Level : Natural)
   is record
      Current_Index : Width_Per_Level_Type;
      Sums          : Single_Moving_Average_Sums (1 .. N_Levels);
      Buffers       : Single_Moving_Average_Buffers (1 .. Width_Per_Level, 1 .. N_Levels);
      --  RM 3.6.2(11/3) advices that row-major order should be used by default. The order of the indices here improves
      --  memory locality when there are many levels if that advice is followed.
   end record;

end Prunt.Moving_Averages;
