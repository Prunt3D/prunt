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

package Prunt.Thermistors is

   type Thermistor_Kind is (Disabled_Kind, Steinhart_Hart_Kind, Callendar_Van_Dusen_Kind);

   --  TODO: Should a polynomial mode be added? It would allow for higher accuracy, but what we already have is more
   --  than good enough for 3d printers.
   type Thermistor_Parameters (Kind : Thermistor_Kind := Disabled_Kind) is record
      Minimum_Temperature : Temperature := 0.0 * celsius;
      Maximum_Temperature : Temperature := 0.0 * celsius;
      case Kind is
         when Disabled_Kind =>
            null;

         when Steinhart_Hart_Kind =>
            SH_A, SH_B, SH_C : Dimensionless;

         when Callendar_Van_Dusen_Kind =>
            CVD_R0       : Resistance;
            CVD_A, CVD_B : Dimensionless;
      end case;
   end record;

   function Temperature_To_Resistance (Params : Thermistor_Parameters; Temp : Temperature) return Resistance
   with
     Pre =>
       Temp >= Params.Minimum_Temperature
       and then Temp <= Params.Maximum_Temperature
       and then Params.Kind /= Disabled_Kind;

private

   function Safe_Cbrt (Val : Dimensionless) return Dimensionless;

end Prunt.Thermistors;
