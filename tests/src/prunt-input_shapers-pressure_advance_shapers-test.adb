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

package body Prunt.Input_Shapers.Pressure_Advance_Shapers.Test is

   pragma Extensions_Allowed (On);

   --  A 2 mm E move that accelerates to 40 mm/s at 1000 mm/s^2, cruises for 10 ms, then decelerates, sampled every
   --  millisecond. With K = 0.04 s the unsmoothed advance during the cruise is 1.6 mm, which is larger than the
   --  0.8 mm covered while accelerating, so an advance that ignores the direction of motion moves the wrong way.

   Interpolation_Time : constant Time := 0.001 * s;
   Advance_Time       : constant Time := 0.04 * s;
   Move_Acceleration  : constant Acceleration := 1_000.0 * mm / s**2;
   Move_Velocity      : constant Velocity := 40.0 * mm / s;
   Ramp_Time          : constant Time := Move_Velocity / Move_Acceleration;
   Cruise_Time        : constant Time := 0.01 * s;
   Ramp_Distance      : constant Length := 0.5 * Move_Acceleration * Ramp_Time**2;
   Move_Distance      : constant Length := 2.0 * Ramp_Distance + Move_Velocity * Cruise_Time;
   Move_Time          : constant Time := 2.0 * Ramp_Time + Cruise_Time;
   Move_Cycles        : constant Cycle_Count := Cycle_Count (Move_Time / Interpolation_Time);
   Tolerance          : constant Length := 1.0E-9 * mm;

   type Length_Array is array (Cycle_Count range <>) of Length;

   function Move_Offset (Cycle : Cycle_Count) return Length is
      T : constant Time := Dimensionless (Cycle) * Interpolation_Time;
   begin
      if T <= 0.0 * s then
         return 0.0 * mm;
      elsif T < Ramp_Time then
         return 0.5 * Move_Acceleration * T**2;
      elsif T < Ramp_Time + Cruise_Time then
         return Ramp_Distance + Move_Velocity * (T - Ramp_Time);
      elsif T < Move_Time then
         return Move_Distance - 0.5 * Move_Acceleration * (Move_Time - T)**2;
      else
         return Move_Distance;
      end if;
   end Move_Offset;

   function Parameters (Smooth_Time : Time; Smooth_Added_Part_Only : Boolean) return Shaper_Parameters
   is ((Kind                                    => Pressure_Advance,
        Pressure_Advance_Time                   => Advance_Time,
        Pressure_Advance_Smooth_Time            => Smooth_Time,
        Pressure_Advance_Smooth_Added_Part_Only => Smooth_Added_Part_Only,
        Pressure_Advance_Smooth_Levels          => 2));

   function Shape_Move
     (Params : Shaper_Parameters; Start_Position : Length; Direction : Dimensionless) return Length_Array
   is
      Shaper : Pressure_Advance_Shaper := Create (Params, Interpolation_Time, Start_Position);
      Result : Length_Array (0 .. Move_Cycles + Shaper.Extra_End_Time + 2);
   begin
      for I in Result'Range loop
         Result (I) := Shaper.Do_Step (Start_Position + Direction * Move_Offset (I));
      end loop;
      return Result;
   end Shape_Move;

   procedure Test_Retraction_Mirrors_Extrusion (T : in out Trendy_Test.Operation'Class) is
      Start : constant Length := 10.0 * mm;

      procedure Check (Smooth_Time : Time; Smooth_Added_Part_Only : Boolean; Label : String) is
         Params   : constant Shaper_Parameters := Parameters (Smooth_Time, Smooth_Added_Part_Only);
         Forward  : constant Length_Array := Shape_Move (Params, Start, 1.0);
         Backward : constant Length_Array := Shape_Move (Params, Start, -1.0);
      begin
         for I in Forward'Range loop
            T.Assert
              (abs ((Forward (I) - Start) + (Backward (I) - Start)) <= Tolerance,
               Label
               & ": retraction does not mirror extrusion at cycle"
               & I'Image
               & ":"
               & Forward (I)'Image
               & Backward (I)'Image);
         end loop;
         T.Assert
           (abs (Backward (Backward'Last) - (Start - Move_Distance)) <= Tolerance,
            Label & ": retraction should finish at the commanded position:" & Backward (Backward'Last)'Image);
      end Check;
   begin
      T.Register;

      Check (0.0 * s, Smooth_Added_Part_Only => False, Label => "Unsmoothed");
      Check (0.02 * s, Smooth_Added_Part_Only => False, Label => "Smoothed output");
      Check (0.02 * s, Smooth_Added_Part_Only => True, Label => "Smoothed added part");
   end Test_Retraction_Mirrors_Extrusion;

   procedure Test_Unsmoothed_Retraction_Never_Moves_Forward (T : in out Trendy_Test.Operation'Class) is
      Start         : constant Length := 10.0 * mm;
      Outputs       : constant Length_Array :=
        Shape_Move (Parameters (0.0 * s, Smooth_Added_Part_Only => False), Start, -1.0);
      Mid_Cruise    : constant Cycle_Count := Cycle_Count ((Ramp_Time + 0.5 * Cruise_Time) / Interpolation_Time);
      Cruise_Output : constant Length := Start - Move_Offset (Mid_Cruise) - Advance_Time * Move_Velocity;
   begin
      T.Register;

      for I in Outputs'Range loop
         T.Assert (Outputs (I) <= Start, "Retraction moved forward at cycle" & I'Image & ":" & Outputs (I)'Image);
      end loop;
      T.Assert
        (abs (Outputs (Mid_Cruise) - Cruise_Output) <= Tolerance,
         "Cruise output should lead the input backwards by K * v:" & Outputs (Mid_Cruise)'Image);
      T.Assert
        (abs (Outputs (Outputs'Last) - (Start - Move_Distance)) <= Tolerance,
         "Retraction should finish at the commanded position:" & Outputs (Outputs'Last)'Image);
   end Test_Unsmoothed_Retraction_Never_Moves_Forward;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Retraction_Mirrors_Extrusion'Unrestricted_Access,
         Test_Unsmoothed_Retraction_Never_Moves_Forward'Unrestricted_Access];
   end All_Tests;

end Prunt.Input_Shapers.Pressure_Advance_Shapers.Test;
