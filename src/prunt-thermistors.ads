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
     Pre => Temp >= Params.Minimum_Temperature and Temp <= Params.Maximum_Temperature and Params.Kind /= Disabled_Kind;

end Prunt.Thermistors;
