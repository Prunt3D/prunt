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

--  An alternative approach to smoothing may be found in commit 9b197ac428bb1eb52f9a0c6163cb2cabf1cf3e2a. The approach
--  used here appears to produce results with less error.

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

   overriding
   function Do_Step (This : in out Pressure_Advance_Shaper; Step : Length) return Length;

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
      Current_Buffer_Index   : Cycle_Count;
      --  Unused when `Smooth_Added_Part_Only` is False.
      Buffer                 : Buffer_Array (1 .. Buffer_Size);
      --  Unused when `Smooth_Added_Part_Only` is False.
      Filter                 :
        Length_Moving_Averages.Cascading_Moving_Average
          (N_Levels => Filter_N_Levels, Width_Per_Level => Filter_Width_Per_Level);
   end record;

end Prunt.Input_Shapers.Pressure_Advance_Shapers;
