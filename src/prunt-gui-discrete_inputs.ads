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
   type T is (<>);
package Prunt.GUI.Discrete_Inputs is

   type Discrete_Input is new Gnoga.Gui.Element.Form.Selection_Type with null record;

   overriding procedure Create
     (Element         : in out Discrete_Input;
      Form            : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Multiple_Select :        Boolean  := False;
      Visible_Lines   :        Positive := 1;
      Name            :        UXString := "";
      ID              :        UXString := "");

   function Get (Input : Discrete_Input) return T;

   procedure Set (Input : in out Discrete_Input; Value : T);

end Prunt.GUI.Discrete_Inputs;
