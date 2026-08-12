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

pragma Extensions_Allowed (On);

private with Ada.Numerics.Generic_Elementary_Functions;

package Prunt.Thermistors is

   type Thermistor_Kind is (Disabled_Kind, Steinhart_Hart_Kind, Callendar_Van_Dusen_Kind);

   Absolute_Zero : constant Temperature := -273.15 * celsius;

   Minimum_Supported_Resistance : constant Resistance := 1.0E-100 * ohm;
   Maximum_Supported_Resistance : constant Resistance := 1.0E100 * ohm;

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

   function Steinhart_Hart_Model_Is_Valid (Params : Thermistor_Parameters) return Boolean
   with Pre => Params.Kind = Steinhart_Hart_Kind;
   --  Return whether the model is single-valued and produces a supported resistance throughout its configured
   --  temperature interval.

   function Callendar_Van_Dusen_Model_Is_Valid (Params : Thermistor_Parameters) return Boolean
   with Pre => Params.Kind = Callendar_Van_Dusen_Kind;
   --  Return whether the model is single-valued and produces a supported resistance throughout its configured
   --  temperature interval.

   function Temperature_To_Resistance (Params : Thermistor_Parameters; Temp : Temperature) return Resistance
   with
     Pre =>
       Temp >= Params.Minimum_Temperature
       and then Temp <= Params.Maximum_Temperature
       and then Params.Kind /= Disabled_Kind
       and then (Params.Kind /= Steinhart_Hart_Kind or else Steinhart_Hart_Model_Is_Valid (Params))
       and then (Params.Kind /= Callendar_Van_Dusen_Kind or else Callendar_Van_Dusen_Model_Is_Valid (Params));
   --  Convert Temp to the resistance predicted by the configured thermistor model.

private

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   Coefficient_Limit : constant Dimensionless := 1.0E100;

   Minimum_Ln_R     : constant Dimensionless := Math.Log (Minimum_Supported_Resistance / ohm);
   Maximum_Ln_R     : constant Dimensionless := Math.Log (Maximum_Supported_Resistance / ohm);
   Maximum_Abs_Ln_R : constant Dimensionless := Dimensionless'Max (abs Minimum_Ln_R, abs Maximum_Ln_R);

   function Steinhart_Hart_Value (Params : Thermistor_Parameters; Ln_R : Dimensionless) return Dimensionless
   with Pre => Params.Kind = Steinhart_Hart_Kind;

   function Steinhart_Hart_Is_Increasing (Params : Thermistor_Parameters) return Boolean
   with Pre => Params.Kind = Steinhart_Hart_Kind;

   function Steinhart_Hart_Is_Decreasing (Params : Thermistor_Parameters) return Boolean
   with Pre => Params.Kind = Steinhart_Hart_Kind;

   function Temperature_Is_In_Steinhart_Hart_Range (Params : Thermistor_Parameters; Temp : Temperature) return Boolean
   with
     Pre =>
       Params.Kind = Steinhart_Hart_Kind
       and then Temp > Absolute_Zero
       and then (Steinhart_Hart_Is_Increasing (Params) or else Steinhart_Hart_Is_Decreasing (Params));

   function Callendar_Van_Dusen_Value (Params : Thermistor_Parameters; Temp : Temperature) return Resistance
   with Pre => Params.Kind = Callendar_Van_Dusen_Kind;

end Prunt.Thermistors;
