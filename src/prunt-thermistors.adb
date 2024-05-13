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

with Ada.Numerics.Generic_Elementary_Functions;

package body Prunt.Thermistors is

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensioned_Float);

   function Temperature_To_Resistance (Params : Thermistor_Parameters; Temp : Temperature) return Resistance is
   begin
      case Params.Kind is
         when Disabled_Kind =>
            raise Constraint_Error with "Thermistor is disabled.";
         when Steinhart_Hart_Kind =>
            declare
               X : constant Dimensionless := 1.0 / Params.SH_C * (Params.SH_A - 1.0 / (Temp / celcius - 273.15));
               Y : constant Dimensionless := ((Params.SH_B / (3.0 * Params.SH_C))**3 + X**2 / 4.0)**(1 / 2);
            begin
               return Math.Exp ((Y - X)**(1 / 3) + X**2 / 4.0) * ohm;
            end;
         when Callendar_Van_Dusen_Kind =>
            return Params.CVD_R0 * (1.0 + Params.CVD_A * Temp / celcius + Params.CVD_B * (Temp / celcius)**2);
      end case;
   end Temperature_To_Resistance;

end Prunt.Thermistors;
