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

package body Prunt.Input_Shapers.Pressure_Advance_Shapers is

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
         Buffer                 => (others => Start_Position),
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
              This.Buffer (This.Current_Buffer_Index)
              + Length_Moving_Averages.Do_Step (This.Filter, Vel * This.Pressure_Advance_Time)
         do
            This.Current_Buffer_Index := @ mod This.Buffer_Size + 1;
            This.Buffer (This.Current_Buffer_Index) := Step;
         end return;
      else
         return Length_Moving_Averages.Do_Step (This.Filter, Step + Vel * This.Pressure_Advance_Time);
      end if;
   end Do_Step;

end Prunt.Input_Shapers.Pressure_Advance_Shapers;
