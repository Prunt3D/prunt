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

--  This package provides implementations for basic input shapers such as Zero Vibration (ZV) and Extra Insensitive
--  (EI) shapers. These shapers work by convolving the input signal with a sequence of impulses to cancel out system
--  vibrations.

pragma Extensions_Allowed (On);

with Prunt.Input_Shapers.Shapers;

package Prunt.Input_Shapers.Basic_Shapers is

   type Impulse_Index is range 1 .. 5;

   type Basic_Shaper
     (Input_Offset   : Cycle_Count;
      Extra_End_Time : Cycle_Count;
      Impulse_Count  : Impulse_Index;
      Buffer_Size    : Cycle_Count)
   is new Shapers.Shaper with private;

   function Create
     (Parameters : Shaper_Parameters; Interpolation_Time : Time; Start_Position : Length) return Basic_Shaper
   with Pre => Parameters.Kind in No_Shaper | Zero_Vibration | Extra_Insensitive;

   overriding
   function Do_Step (This : in out Basic_Shaper; Step : Length) return Length;

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
   is new Shapers.Shaper (Input_Offset => Input_Offset, Extra_End_Time => Extra_End_Time) with record
      Current_Buffer_Index : Cycle_Count;
      Impulses             : Impulses_Array (1 .. Impulse_Count);
      Buffer               : Buffer_Array (0 .. Buffer_Size);
      --  These buffers are technically 1 larger than Buffer_Size, but that does not matter. Starting at 0 makes the
      --  implementation simpler. We can not subtract from a value that comes from a discriminant to get the correct
      --  size while starting at 0.
   end record
   with
     Dynamic_Predicate =>
       (for all I in 1 .. Impulse_Count - 1 =>
          Basic_Shaper.Impulses (I).Output_Delay <= Basic_Shaper.Impulses (I + 1).Output_Delay)
       and then Basic_Shaper.Buffer_Size = Basic_Shaper.Impulses (Basic_Shaper.Impulses'Last).Output_Delay + 1
       and then (for all I of Basic_Shaper.Impulses => I.Output_Delay >= 0);
   --  TODO: Gnatcov can not parse 'Reduce, rewrite this without it:
   --  and abs ([for I of Basic_Shaper.Impulses => I.Output_Ratio]'Reduce ("+", Dimensionless (1.0)) - 1.0)
   --      < 0.000_000_1;

   function Compute_Impulses (Parameters : Shaper_Parameters; Interpolation_Time : Time) return Impulses_Array;
   function Compute_Input_Offset (Impulses : Impulses_Array) return Cycle_Count;

end Prunt.Input_Shapers.Basic_Shapers;
