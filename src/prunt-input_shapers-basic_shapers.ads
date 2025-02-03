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

with Prunt.Input_Shapers.Shapers;

package Prunt.Input_Shapers.Basic_Shapers is

   type Impulse_Index is range 1 .. 5;

   type Basic_Shaper
     (Input_Offset   : Cycle_Count;
      Extra_End_Time : Cycle_Count;
      Impulse_Count  : Impulse_Index;
      Buffer_Size    : Cycle_Count)
   is
     new Shapers.Shaper with private;

   function Create
     (Parameters         : Shaper_Parameters;
      Interpolation_Time : Time;
      Start_Position     : Length)
      return Basic_Shaper with
     Pre => Parameters.Kind in No_Shaper | Zero_Vibration | Extra_Insensitive;

   overriding function Do_Step (This : in out Basic_Shaper; Step : Length) return Length;

private

   subtype Impulse_Ratio is Dimensionless range 0.0 .. 1.0;

   type Impulse is record
      Output_Delay : Cycle_Count;
      Output_Ratio : Impulse_Ratio;
   end record;

   type Impulses_Array is array (Impulse_Index range <>) of Impulse;

   type Buffer_Array is array (Cycle_Count range <>) of Length;

   type Basic_Shaper
     (Input_Offset   : Cycle_Count;
      Extra_End_Time : Cycle_Count;
      Impulse_Count  : Impulse_Index;
      Buffer_Size    : Cycle_Count)
   is
   new Shapers.Shaper (Input_Offset => Input_Offset, Extra_End_Time => Extra_End_Time) with record
      Impulses             : Impulses_Array (1 .. Impulse_Count);
      Buffer               : Buffer_Array (0 .. Buffer_Size);
      --  These buffers are technically 1 larger than Buffer_Size, but that does not matter. Starting at 0 makes the
      --  implementation simpler. We can not subtract from a value that comes from a discriminant to get the correct
      --  size while starting at 0.
      Current_Buffer_Index : Cycle_Count;
   end record with
     Dynamic_Predicate =>
      (for all I in 1 .. Impulse_Count - 1 =>
         Basic_Shaper.Impulses (I).Output_Delay <= Basic_Shaper.Impulses (I + 1).Output_Delay) and
      Basic_Shaper.Buffer_Size = Basic_Shaper.Impulses (Basic_Shaper.Impulses'Last).Output_Delay + 1 and
      (for all I of Basic_Shaper.Impulses => I.Output_Delay >= 0) and
      abs ([for I of Basic_Shaper.Impulses => I.Output_Ratio]'Reduce ("+", Dimensionless (1.0)) - 1.0) < 0.000_000_1;

   function Compute_Impulses (Parameters : Shaper_Parameters; Interpolation_Time : Time) return Impulses_Array;
   function Compute_Input_Offset (Impulses : Impulses_Array) return Cycle_Count;

end Prunt.Input_Shapers.Basic_Shapers;
