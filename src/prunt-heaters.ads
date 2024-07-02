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

package Prunt.Heaters is

   type Heater_Kind is (Disabled_Kind, PID_Kind, Bang_Bang_Kind);

   type Heater_Parameters (Kind : Heater_Kind := Disabled_Kind) is record
      Max_Cumulative_Error : Temperature     := 120.0 * celcius;
      Check_Gain_Time      : Time            := 20.0 * s;
      Check_Minimum_Gain   : Temperature     := 2.0 * celcius;
      Hysteresis           : Temperature     := 3.0 * celcius;
      case Kind is
         when Disabled_Kind =>
            null;
         when PID_Kind =>
            Proportional_Scale : Dimensionless := 0.0;
            Integral_Scale     : Dimensionless := 0.0;
            Derivative_Scale   : Dimensionless := 0.0;
         when Bang_Bang_Kind =>
            null;
      end case;
   end record;

end Prunt.Heaters;
