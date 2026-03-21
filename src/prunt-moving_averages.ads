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

pragma Extensions_Allowed (On);

generic
   type Number is digits <>;
package Prunt.Moving_Averages is

   type Cascading_Moving_Average
     (N_Levels        : Positive;
      Width_Per_Level : Natural)
   is
     private;

   function Create
     (N_Levels : Positive; Max_Total_Width : Natural; Initial_Value : Number) return Cascading_Moving_Average;
   --  Creates a new filter with the requested parameters. `Max_Total_Width` is a maximum rather than an exact value as
   --  it must be split evenly between `N_Levels` levels.

   function Total_Delay (CMA : Cascading_Moving_Average) return Natural
   is (if CMA.Width_Per_Level > 1 then (CMA.N_Levels * (CMA.Width_Per_Level - 1)) / 2 else 0);
   --  Returns the number of cycles from when a signal enters the filter to when the effect of the signal on the output
   --  is maximised. This may not be equal to `Max_Total_Width` / 2 as provided to `Create` as that value may be
   --  reduced to suit the number of layers specified.

   function Do_Step (CMA : in out Cascading_Moving_Average; Input : Number) return Number;

private

   subtype N_Levels_Type is Positive;
   subtype Width_Per_Level_Type is Natural;

   type Single_Moving_Average_Sums is array (N_Levels_Type range <>) of Number;
   type Single_Moving_Average_Buffers is array (N_Levels_Type range <>, Width_Per_Level_Type range <>) of Number;

   type Cascading_Moving_Average
     (N_Levels        : Positive;
      Width_Per_Level : Natural)
   is record
      Current_Index : Width_Per_Level_Type;
      Sums          : Single_Moving_Average_Sums (1 .. N_Levels);
      Buffers       : Single_Moving_Average_Buffers (1 .. Width_Per_Level, 1 .. N_Levels);
      --  RM 3.6.2(11/3) advises that row-major order should be used by default. The order of the indices here improves
      --  memory locality when there are many levels if that advice is followed.
   end record;

end Prunt.Moving_Averages;
