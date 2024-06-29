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

with Gnoga.Gui.Element.Form;
with UXStrings; use UXStrings;

generic
   type T is digits <>;
package Prunt.GUI.Numeric_Floating_Inputs is

   pragma Unsuppress (All_Checks);

   type Numeric_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;

   overriding procedure Create
     (Element : in out Numeric_Input;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Value   :        UXString := "";
      Name    :        UXString := "";
      ID      :        UXString := "");

   procedure Create_For_Parameter_Row
     (Element : in out Numeric_Input;
      Parent  : in out Gnoga.Gui.Element.Element_Type'Class;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class);

   function Get (Input : Numeric_Input) return T;

   procedure Set (Input : in out Numeric_Input; Value : T);

end Prunt.GUI.Numeric_Floating_Inputs;
