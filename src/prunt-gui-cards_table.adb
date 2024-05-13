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

package body Prunt.GUI.Cards_Table is

   overriding procedure Create
     (Table : in out Cards_Table_Type; Parent : in out Gnoga.Gui.Base.Base_Type'Class; ID : UXString := "")
   is
   begin
      Gnoga.Gui.Element.Table.Table_Type (Table).Create (Parent, ID);
      Table.Tabs_Row.Create (Table);
      Table.Cards_Row.Create (Table);
      Table.Cards.Create (Table.Cards_Row);
      Table.Tabs.Create (Table.Tabs_Row, Table.Cards);
      Table.Cards_Row.Style ("border", "medium solid black");
      Table.Style ("border-collapse", "collapse");
      Table.Tabs.Style ("margin-block-end", "9px");
   end Create;

   procedure Add_Tab
     (Table : in out Cards_Table_Type; Label : UXString; Card : access Gnoga.Gui.View.View_Base_Type'Class)
   is
   begin
      Table.Cards.Add_Card (Label, Card);
      Table.Tabs.Add_Tab (Label, Label);
   end Add_Tab;

end Prunt.GUI.Cards_Table;
