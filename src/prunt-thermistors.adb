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

package body Prunt.Thermistors is

   pragma Extensions_Allowed (On);

   function Steinhart_Hart_Value (Params : Thermistor_Parameters; Ln_R : Dimensionless) return Dimensionless is
   begin
      return Params.SH_A + Ln_R * (Params.SH_B + Params.SH_C * Ln_R ** 2);
   end Steinhart_Hart_Value;

   function Steinhart_Hart_Is_Increasing (Params : Thermistor_Parameters) return Boolean is
      Derivative_At_Zero : constant Dimensionless := Params.SH_B;
      Derivative_At_Edge : constant Dimensionless := Params.SH_B + 3.0 * Params.SH_C * Maximum_Abs_Ln_R ** 2;
   begin
      return
        Derivative_At_Zero >= 0.0
        and then Derivative_At_Edge >= 0.0
        and then (Derivative_At_Zero > 0.0 or else Derivative_At_Edge > 0.0);
   end Steinhart_Hart_Is_Increasing;

   function Steinhart_Hart_Is_Decreasing (Params : Thermistor_Parameters) return Boolean is
      Derivative_At_Zero : constant Dimensionless := Params.SH_B;
      Derivative_At_Edge : constant Dimensionless := Params.SH_B + 3.0 * Params.SH_C * Maximum_Abs_Ln_R ** 2;
   begin
      return
        Derivative_At_Zero <= 0.0
        and then Derivative_At_Edge <= 0.0
        and then (Derivative_At_Zero < 0.0 or else Derivative_At_Edge < 0.0);
   end Steinhart_Hart_Is_Decreasing;

   function Temperature_Is_In_Steinhart_Hart_Range (Params : Thermistor_Parameters; Temp : Temperature) return Boolean
   is
      Inv_T       : constant Dimensionless := 1.0 / ((Temp - Absolute_Zero) / celsius);
      Lower_Value : constant Dimensionless := Steinhart_Hart_Value (Params, Minimum_Ln_R);
      Upper_Value : constant Dimensionless := Steinhart_Hart_Value (Params, Maximum_Ln_R);
   begin
      if Steinhart_Hart_Is_Increasing (Params) then
         return Inv_T >= Lower_Value and then Inv_T <= Upper_Value;
      else
         return Inv_T <= Lower_Value and then Inv_T >= Upper_Value;
      end if;
   end Temperature_Is_In_Steinhart_Hart_Range;

   function Steinhart_Hart_Model_Is_Valid (Params : Thermistor_Parameters) return Boolean is
   begin
      return
        Params.Minimum_Temperature > Absolute_Zero
        and then Params.Maximum_Temperature > Params.Minimum_Temperature
        and then Params.Maximum_Temperature <= 1.0E100 * celsius
        and then abs Params.SH_A <= Coefficient_Limit
        and then abs Params.SH_B <= Coefficient_Limit
        and then abs Params.SH_C <= Coefficient_Limit
        and then (Steinhart_Hart_Is_Increasing (Params) or else Steinhart_Hart_Is_Decreasing (Params))
        and then Temperature_Is_In_Steinhart_Hart_Range (Params, Params.Minimum_Temperature)
        and then Temperature_Is_In_Steinhart_Hart_Range (Params, Params.Maximum_Temperature);
   end Steinhart_Hart_Model_Is_Valid;

   function Callendar_Van_Dusen_Value (Params : Thermistor_Parameters; Temp : Temperature) return Resistance is
      Temp_C : constant Dimensionless := Temp / celsius;
   begin
      return Params.CVD_R0 * (1.0 + Temp_C * (Params.CVD_A + Params.CVD_B * Temp_C));
   end Callendar_Van_Dusen_Value;

   function Callendar_Van_Dusen_Model_Is_Valid (Params : Thermistor_Parameters) return Boolean is
   begin
      if Params.Minimum_Temperature <= Absolute_Zero
        or else Params.Maximum_Temperature <= Params.Minimum_Temperature
        or else Params.Maximum_Temperature > 1.0E100 * celsius
        or else Params.CVD_R0 < Minimum_Supported_Resistance
        or else Params.CVD_R0 > Maximum_Supported_Resistance
        or else abs Params.CVD_A > Coefficient_Limit
        or else abs Params.CVD_B > Coefficient_Limit
      then
         return False;
      end if;

      declare
         Minimum_Temp_C        : constant Dimensionless := Params.Minimum_Temperature / celsius;
         Maximum_Temp_C        : constant Dimensionless := Params.Maximum_Temperature / celsius;
         Minimum_Derivative    : constant Dimensionless := Params.CVD_A + 2.0 * Params.CVD_B * Minimum_Temp_C;
         Maximum_Derivative    : constant Dimensionless := Params.CVD_A + 2.0 * Params.CVD_B * Maximum_Temp_C;
         Resistance_At_Minimum : constant Resistance := Callendar_Van_Dusen_Value (Params, Params.Minimum_Temperature);
         Resistance_At_Maximum : constant Resistance := Callendar_Van_Dusen_Value (Params, Params.Maximum_Temperature);
         Increasing            : constant Boolean :=
           Minimum_Derivative >= 0.0
           and then Maximum_Derivative >= 0.0
           and then (Minimum_Derivative > 0.0 or else Maximum_Derivative > 0.0);
         Decreasing            : constant Boolean :=
           Minimum_Derivative <= 0.0
           and then Maximum_Derivative <= 0.0
           and then (Minimum_Derivative < 0.0 or else Maximum_Derivative < 0.0);
      begin
         return
           (Increasing or else Decreasing)
           and then Resistance_At_Minimum >= Minimum_Supported_Resistance
           and then Resistance_At_Minimum <= Maximum_Supported_Resistance
           and then Resistance_At_Maximum >= Minimum_Supported_Resistance
           and then Resistance_At_Maximum <= Maximum_Supported_Resistance;
      end;
   end Callendar_Van_Dusen_Model_Is_Valid;

   function Temperature_To_Resistance (Params : Thermistor_Parameters; Temp : Temperature) return Resistance is
   begin
      case Params.Kind is
         when Disabled_Kind            =>
            pragma Annotate (Xcov, Exempt_On, "Handled by precondition.");
            raise Constraint_Error with "Thermistor is disabled.";
            pragma Annotate (Xcov, Exempt_Off);

         when Steinhart_Hart_Kind      =>
            --  The standard Steinhart-Hart equation is:
            --
            --  1/T = A + B*ln(R) + C*(ln(R))^3
            --  C*ln(R)^3 + B*ln(R) + (A - 1/T) = 0
            --
            declare
               Inv_T      : constant Dimensionless := 1.0 / ((Temp - Absolute_Zero) / celsius);
               Increasing : constant Boolean := Steinhart_Hart_Is_Increasing (Params);
               Low        : Dimensionless := Minimum_Ln_R;
               High       : Dimensionless := Maximum_Ln_R;
            begin
               --  Validation guarantees one root in this interval. Bisection avoids dividing by either coefficient
               --  and remains stable when B or C is zero or very small.
               for I in 1 .. Dimensioned_Float'Machine_Mantissa + 8 loop
                  declare
                     Mid       : constant Dimensionless := Low + (High - Low) / 2.0;
                     Mid_Value : constant Dimensionless := Steinhart_Hart_Value (Params, Mid);
                  begin
                     if (Increasing and then Mid_Value < Inv_T) or else (not Increasing and then Mid_Value > Inv_T)
                     then
                        Low := Mid;
                     else
                        High := Mid;
                     end if;
                  end;
               end loop;

               return Math.Exp (Low + (High - Low) / 2.0) * ohm;
            end;

         when Callendar_Van_Dusen_Kind =>
            return Callendar_Van_Dusen_Value (Params, Temp);
      end case;
   end Temperature_To_Resistance;

end Prunt.Thermistors;
