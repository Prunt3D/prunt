-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
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

pragma Extensions_Allowed (On);

with Trendy_Test;

package Prunt.Thermistors.Test is

   function All_Tests return Trendy_Test.Test_Group;

private

   generic
      Params : Thermistor_Parameters;
   procedure Test_Thermistor (T : in out Trendy_Test.Operation'Class);

   function Newton_Inverse_Solve_Steinhart_Hart (Params : Thermistor_Parameters; Temp : Temperature) return Resistance
   with Pre => Params.Kind = Steinhart_Hart_Kind;

   function Solve_Callendar_Van_Dusen (Params : Thermistor_Parameters; Temp : Temperature) return Resistance
   with Pre => Params.Kind = Callendar_Van_Dusen_Kind;

end Prunt.Thermistors.Test;
