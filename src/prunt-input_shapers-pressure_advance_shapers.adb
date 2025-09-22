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
      Half_Smooth_Time : constant Cycle_Count :=
        Cycle_Count (Dimensionless'Floor (0.5 * Parameters.Pressure_Advance_Smooth_Time / Interpolation_Time));
   begin
      return
        (Input_Offset           => -Half_Smooth_Time - (if Half_Smooth_Time = 0 then 0 else 1),
         Extra_End_Time         => Half_Smooth_Time * 2 + (if Half_Smooth_Time = 0 then 0 else 1),
         Buffer_Size            => Half_Smooth_Time * 2 + 1,
         Buffer                 => (others => (Pos => Start_Position, PA_Part => 0.0 * mm)),
         Current_Buffer_Index   => 0,
         Previous_Outputs       =>
           (others =>
              (if Parameters.Pressure_Advance_Smooth_Added_Part_Only
               then 0.0 * mm
               else Start_Position * Dimensionless (Half_Smooth_Time)**2)),
         Previous_Input         => Start_Position,
         Pressure_Advance_Time  => Parameters.Pressure_Advance_Time,
         Interpolation_Time     => Interpolation_Time,
         Smooth_Added_Part_Only => Parameters.Pressure_Advance_Smooth_Added_Part_Only);
   end Create;

   overriding
   function Do_Step (This : in out Pressure_Advance_Shaper; Step : Length) return Length is
      Half_Smooth_Time : constant Cycle_Count := This.Buffer_Size / 2;
      Vel              : constant Velocity := abs ((Step - This.Previous_Input) / This.Interpolation_Time);
      Left             : constant Cycle_Count := (This.Current_Buffer_Index - Half_Smooth_Time) mod This.Buffer_Size;
      Mid              : constant Cycle_Count := This.Current_Buffer_Index;
      Right            : constant Cycle_Count := (This.Current_Buffer_Index + Half_Smooth_Time) mod This.Buffer_Size;
   begin
      This.Previous_Input := Step;

      if Half_Smooth_Time = 0 then
         return Step + This.Pressure_Advance_Time * Vel;
      else
         This.Buffer (Right) := (Pos => Step, PA_Part => This.Pressure_Advance_Time * Vel);
         This.Current_Buffer_Index := (@ + 1) mod This.Buffer_Size;
         declare
            Result : constant Length :=
              (if This.Smooth_Added_Part_Only
               then 0.0 * mm
               else This.Buffer (Left).Pos + This.Buffer (Right).Pos - 2.0 * This.Buffer (Mid).Pos)
              + (This.Buffer (Left).PA_Part
                 + This.Buffer (Right).PA_Part
                 - 2.0 * This.Buffer (Mid).PA_Part
                 + 2.0 * This.Previous_Outputs (-1)
                 - This.Previous_Outputs (-2));
         begin
            This.Previous_Outputs (-2) := This.Previous_Outputs (-1);
            This.Previous_Outputs (-1) := Result;
            return
              (if This.Smooth_Added_Part_Only then This.Buffer (Mid).Pos else 0.0 * mm)
              + (Result / Dimensionless (Half_Smooth_Time)**2);
         end;
      end if;
   end Do_Step;

end Prunt.Input_Shapers.Pressure_Advance_Shapers;
