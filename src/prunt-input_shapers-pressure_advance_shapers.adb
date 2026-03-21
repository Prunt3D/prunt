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

package body Prunt.Input_Shapers.Pressure_Advance_Shapers is

   pragma Extensions_Allowed (On);

   function Create
     (Parameters : Shaper_Parameters; Interpolation_Time : Time; Start_Position : Length)
      return Pressure_Advance_Shaper
   is
      CMA         : constant Length_Moving_Averages.Cascading_Moving_Average :=
        Length_Moving_Averages.Create
          (N_Levels        => Parameters.Pressure_Advance_Smooth_Levels,
           Max_Total_Width => Natural (Parameters.Pressure_Advance_Smooth_Time / Interpolation_Time),
           Initial_Value   =>
             (if Parameters.Pressure_Advance_Smooth_Added_Part_Only then 0.0 * mm else Start_Position));
      Total_Delay : constant Cycle_Count := Cycle_Count (Length_Moving_Averages.Total_Delay (CMA));
   begin
      return
        (Input_Offset           => -Total_Delay,
         Extra_End_Time         => Cycle_Count (Parameters.Pressure_Advance_Smooth_Time / Interpolation_Time),
         Filter_N_Levels        => CMA.N_Levels,
         Filter_Width_Per_Level => CMA.Width_Per_Level,
         Buffer_Size            => (if Parameters.Pressure_Advance_Smooth_Added_Part_Only then Total_Delay else 0),
         Pressure_Advance_Time  => Parameters.Pressure_Advance_Time,
         Interpolation_Time     => Interpolation_Time,
         Smooth_Added_Part_Only => Parameters.Pressure_Advance_Smooth_Added_Part_Only,
         Previous_Input         => Start_Position,
         Current_Buffer_Index   => 1,
         Buffer                 => [others => Start_Position],
         Filter                 => CMA);
   end Create;

   overriding
   function Do_Step (This : in out Pressure_Advance_Shaper; Step : Length) return Length is
      Vel : constant Velocity := abs ((Step - This.Previous_Input) / This.Interpolation_Time);
   begin
      This.Previous_Input := Step;
      if This.Smooth_Added_Part_Only then
         return
            Result : constant Length :=
              (if This.Buffer_Size > 0 then This.Buffer (This.Current_Buffer_Index) else Step)
              + Length_Moving_Averages.Do_Step (This.Filter, Vel * This.Pressure_Advance_Time)
         do
            if This.Buffer_Size > 0 then
               This.Current_Buffer_Index := @ mod This.Buffer_Size + 1;
               This.Buffer (This.Current_Buffer_Index) := Step;
            end if;
         end return;
      else
         return Length_Moving_Averages.Do_Step (This.Filter, Step + Vel * This.Pressure_Advance_Time);
      end if;
   end Do_Step;

end Prunt.Input_Shapers.Pressure_Advance_Shapers;
