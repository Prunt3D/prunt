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

with Gnoga.Gui.View;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.Element.Table;
with Gnoga.Gui.Base;
with UXStrings; use UXStrings;

package Prunt.GUI.Cards_Table is

   type Cards_Table_Type is new Gnoga.Gui.Element.Table.Table_Type with record
      Tabs      : aliased Gnoga.Gui.View.Card.Tab_Type;
      Cards     : aliased Gnoga.Gui.View.Card.Card_View_Type;
      Tabs_Row  : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Cards_Row : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
   end record;

   overriding procedure Create
     (Table : in out Cards_Table_Type; Parent : in out Gnoga.Gui.Base.Base_Type'Class; ID : UXString := "");

   procedure Add_Tab
     (Table : in out Cards_Table_Type; Label : UXString; Card : access Gnoga.Gui.View.View_Base_Type'Class);

end Prunt.GUI.Cards_Table;
