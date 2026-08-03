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

--  This package implements a pressure advance shaper.
--
--  Pressure advance advances the axis during acceleration by a multiple of the acceleration. This is meant to
--  compensate for elasticity in the system. A CMA filter is applied to either the output or just the added part,
--  depending on the provided parameters, to reduce the velocity of the output.
--
--  An alternative approach to smoothing may be found in commit 9b197ac428bb1eb52f9a0c6163cb2cabf1cf3e2a. The approach
--  used here appears to produce results with less error.

pragma Extensions_Allowed (On);

with Prunt.Input_Shapers.Shapers;
with Prunt.Moving_Averages;

package Prunt.Input_Shapers.Pressure_Advance_Shapers is

   type Pressure_Advance_Shaper
     (Input_Offset           : Cycle_Count;
      Extra_End_Time         : Cycle_Count;
      Filter_N_Levels        : Positive;
      Filter_Width_Per_Level : Natural;
      Buffer_Size            : Cycle_Count)
   is new Shapers.Shaper with private;

   function Create
     (Parameters : Shaper_Parameters; Interpolation_Time : Time; Start_Position : Length)
      return Pressure_Advance_Shaper
   with Pre => Parameters.Kind in Pressure_Advance;
   --  Construct a pressure-advance shaper and smoothing filter initialized to Start_Position.

   overriding
   function Do_Step (This : in out Pressure_Advance_Shaper; Step : Length) return Length;
   --  Apply velocity-based pressure advance and smoothing to the next commanded position Step.

private

   package Length_Moving_Averages is new Moving_Averages (Length);

   type Buffer_Array is array (Cycle_Count range <>) of Length;

   type Pressure_Advance_Shaper
     (Input_Offset           : Cycle_Count;
      Extra_End_Time         : Cycle_Count;
      Filter_N_Levels        : Positive;
      Filter_Width_Per_Level : Natural;
      Buffer_Size            : Cycle_Count)
   is new Shapers.Shaper (Input_Offset => Input_Offset, Extra_End_Time => Extra_End_Time) with record
      Pressure_Advance_Time  : Time;
      Interpolation_Time     : Time;
      Smooth_Added_Part_Only : Boolean;
      Previous_Input         : Length;
      Current_Buffer_Index   : Cycle_Count; --  Unused when Smooth_Added_Part_Only is False.
      Buffer                 : Buffer_Array (1 .. Buffer_Size); --  Unused when Smooth_Added_Part_Only is False.
      Filter                 :
        Length_Moving_Averages.Cascading_Moving_Average
          (N_Levels => Filter_N_Levels, Width_Per_Level => Filter_Width_Per_Level);
   end record;

end Prunt.Input_Shapers.Pressure_Advance_Shapers;
