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

pragma Extensions_Allowed (On);

package Prunt.Input_Shapers is

   type Cycle_Count is range -2 ** 63 + 1 .. 2 ** 63 - 1;

   subtype Shaper_Damping_Ratio is Dimensionless range 0.0 .. 1.0 with Annotate => (Prunt_Config, Unit, "", "×");
   subtype Residual_Vibration_Level is Dimensionless range 0.0 .. 1.0 with Annotate => (Prunt_Config, Unit, "", "×");

   type Zero_Vibration_Deriviatives_Count is range 0 .. 3 with Annotate => (Prunt_Config, User_Config);
   type Extra_Insensitive_Humps_Count is range 1 .. 3 with Annotate => (Prunt_Config, User_Config);
   type Modified_Zero_Vibration_Impulses_Count is range 2 .. 3 with Annotate => (Prunt_Config, User_Config);

   type Shaper_Kind is (No_Shaper, Zero_Vibration, Extra_Insensitive, Pressure_Advance);

   type Shaper_Parameters (Kind : Shaper_Kind := No_Shaper) is record
      case Kind is
         when No_Shaper =>
            null;

         when Zero_Vibration =>
            Zero_Vibration_Frequency     : Frequency;
            Zero_Vibration_Damping_Ratio : Shaper_Damping_Ratio;
            Zero_Vibration_Deriviatives  : Zero_Vibration_Deriviatives_Count;

         when Extra_Insensitive =>
            Extra_Insensitive_Frequency          : Frequency;
            Extra_Insensitive_Damping_Ratio      : Shaper_Damping_Ratio;
            Extra_Insensitive_Humps              : Extra_Insensitive_Humps_Count;
            Extra_Insensitive_Residual_Vibration : Residual_Vibration_Level;

         when Pressure_Advance =>
            Pressure_Advance_Time                   : Time;
            Pressure_Advance_Smooth_Time            : Time;
            Pressure_Advance_Smooth_Added_Part_Only : Boolean;
            Pressure_Advance_Smooth_Levels          : Positive;
      end case;
   end record;

   type Axial_Shaper_Parameters is array (Axis_Name) of Shaper_Parameters;

end Prunt.Input_Shapers;
