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

with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element;
with UXStrings; use UXStrings;
with Gnoga.Types;

generic
   type T is private;
   Unit_Name : UXString;
   type Element_Type is new Gnoga.Gui.Element.Element_Type with private;
   with procedure Create_For_Parameter_Row
     (Element : in out Element_Type;
      Parent  : in out Gnoga.Gui.Element.Element_Type'Class;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class) is <>;
   with function Get (Input : Element_Type) return T is <>;
   with procedure Set (Input : in out Element_Type; Value : T) is <>;
package Prunt.GUI.Parameter_Rows is

   subtype Parent_Type is Gnoga.Gui.Element.Table.Table_Row_Type;

   type Parameter_Row is new Parent_Type with private;

   procedure Create
     (Row         : in out Parameter_Row;
      Parent      : in out Gnoga.Gui.Element.Element_Type'Class;
      Form        : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Name        :        UXString;
      Description :        UXString;
      ID          :        UXString := "");

   function Get_Data (Input : Parameter_Row) return T;

   procedure Set_Data (Input : in out Parameter_Row; Value : T);

private

   overriding procedure Create
     (Row : in out Parameter_Row; Parent : in out Gnoga.Gui.Element.Element_Type'Class; ID : UXString := "");

   type Parameter_Row is new Parent_Type with record
      Name        : Gnoga.Gui.Element.Common.DIV_Type;
      Description : Gnoga.Gui.Element.Common.DIV_Type;
      Data        : Element_Type;

      Name_Col        : Gnoga.Gui.Element.Table.Table_Column_Type;
      Description_Col : Gnoga.Gui.Element.Table.Table_Column_Type;
      Data_Col        : Gnoga.Gui.Element.Table.Table_Column_Type;
   end record;

end Prunt.GUI.Parameter_Rows;
