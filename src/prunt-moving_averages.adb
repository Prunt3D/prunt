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

package body Prunt.Moving_Averages is

   pragma Extensions_Allowed (On);

   function Create
     (N_Levels : Positive; Max_Total_Width : Natural; Initial_Value : Number) return Cascading_Moving_Average is
   begin
      return
        (N_Levels        => N_Levels,
         Width_Per_Level => Max_Total_Width / N_Levels,
         Current_Index   => 1,
         Sums            => [others => Initial_Value * Number'Base (Max_Total_Width / N_Levels)],
         Buffers         => [others => [others => Initial_Value]]);
   end Create;

   function Do_Step (CMA : in out Cascading_Moving_Average; Input : Number) return Number is
      Result : Number := Input;
   begin
      if CMA.Width_Per_Level > 1 then
         for Level in CMA.Buffers'Range (2) loop
            CMA.Sums (Level) := @ - CMA.Buffers (CMA.Current_Index, Level) + Result;
            CMA.Buffers (CMA.Current_Index, Level) := Result;
            Result := Number (CMA.Sums (Level) / Number'Base (CMA.Width_Per_Level));
         end loop;

         CMA.Current_Index := @ mod CMA.Width_Per_Level + 1;
      end if;

      return Result;
   end Do_Step;

end Prunt.Moving_Averages;
