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

generic
package Prunt.Moving_Averages.Test is

   function All_Tests return Trendy_Test.Test_Group;

private

   type Number_Array is array (Positive range <>) of Number;

   procedure Run_Test
     (CMA              : in out Cascading_Moving_Average;
      Inputs           : Number_Array;
      Expected_Outputs : Number_Array;
      T                : in out Trendy_Test.Operation'Class)
   with Pre => Inputs'First = Expected_Outputs'First and then Inputs'Last = Expected_Outputs'Last;

end Prunt.Moving_Averages.Test;
