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

with Ada.Numerics.Generic_Elementary_Functions;

package body Prunt.Thermistors is

   pragma Extensions_Allowed (On);

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensioned_Float);

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
            --  When C = 0:
            --
            --  B*ln(R) + (A - 1/T) = 0
            --  B*ln(R) = 1/T - A
            --  ln(R)   = (1/T - A)/B
            --
            --  When C != 0:
            --
            --  C*(ln(R))^3 + B*ln(R)     + A - 1/T     = 0
            --  ln(R)^3     + (B/C)*ln(R) + (A - 1/T)/C = 0
            --
            --  Apply Cardano's formula:
            --
            --  u^3 + p*u + q = 0
            --  w = (q^2/4 + p^3/27)^(1/2)
            --  u = (-q/2 + w)^(1/3) + (-q/2 - w)^(1/3)
            --
            --  u = ln(R)
            --  p = (B/C)
            --  q = (A - 1/T)/C

            declare
               Inv_T : constant Dimensionless := 1.0 / (Temp / celsius + 273.15);
               Ln_R  : Dimensionless;
            begin
               if abs Params.SH_C < 1.0e-12 then
                  --  TODO: What if SH_A or SH_B are also very small? Should we define some limits in the config?
                  Ln_R := (Inv_T - Params.SH_A) / Params.SH_B;
               else
                  declare
                     Q : constant Dimensionless := (Params.SH_A - Inv_T) / Params.SH_C;
                     P : constant Dimensionless := Params.SH_B / Params.SH_C;
                     W : constant Dimensionless := (Q ** 2 / 4.0 + P ** 3 / 27.0) ** (1 / 2);
                  begin
                     Ln_R := Safe_Cbrt (-Q / 2.0 + W) + Safe_Cbrt (-Q / 2.0 - W);
                  end;
               end if;

               return Math.Exp (Ln_R) * ohm;
            end;

         when Callendar_Van_Dusen_Kind =>
            return Params.CVD_R0 * (1.0 + Params.CVD_A * Temp / celsius + Params.CVD_B * (Temp / celsius) ** 2);
      end case;
   end Temperature_To_Resistance;

   function Safe_Cbrt (Val : Dimensionless) return Dimensionless is
   begin
      if Val < 0.0 then
         return -((-Val) ** (1 / 3));
      else
         return Val ** (1 / 3);
      end if;
   end Safe_Cbrt;

end Prunt.Thermistors;
