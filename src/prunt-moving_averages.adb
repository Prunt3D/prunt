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

package body Prunt.Moving_Averages is

   function Create (N_Levels : Positive; Max_Total_Width : Natural; Initial_Value : T) return Cascading_Moving_Average
   is
   begin
      return
        (N_Levels        => N_Levels,
         Width_Per_Level => Max_Total_Width / N_Levels,
         Current_Index   => 1,
         Sums            => (others => Initial_Value * T'Base (Max_Total_Width / N_Levels)),
         Buffers         => (others => (others => Initial_Value)));
   end Create;

   function Do_Step (CMA : in out Cascading_Moving_Average; Input : T) return T is
      Result : T := Input;
   begin
      if CMA.Width_Per_Level > 1 then
         for Level in CMA.Buffers'Range (2) loop
            CMA.Sums (Level) := @ - CMA.Buffers (CMA.Current_Index, Level) + Result;
            CMA.Buffers (CMA.Current_Index, Level) := Result;
            Result := T (CMA.Sums (Level) / T'Base (CMA.Width_Per_Level));
         end loop;

         CMA.Current_Index := @ mod CMA.Width_Per_Level + 1;
      end if;

      return Result;
   end Do_Step;

end Prunt.Moving_Averages;
