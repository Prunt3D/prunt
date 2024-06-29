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

package body Prunt.GUI.Parameter_Rows is

   overriding procedure Create
     (Row : in out Parameter_Row; Parent : in out Gnoga.Gui.Element.Element_Type'Class; ID : UXString := "")
   is
   begin
      Parent_Type (Row).Create (Parent, ID);
      Row.Name_Col.Create (Row);
      Row.Name.Create (Row.Name_Col);
      Row.Data_Col.Create (Row);
      Row.Description_Col.Create (Row);
      Row.Description.Create (Row.Description_Col);
      Row.Style ("border", "1px solid black");
   end Create;

   procedure Create
     (Row         : in out Parameter_Row;
      Parent      : in out Gnoga.Gui.Element.Element_Type'Class;
      Form        : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Name        :        UXString;
      Description :        UXString;
      ID          :        UXString := "")
   is
   begin
      Create_For_Parameter_Row (Row.Data, Parent, Form);
      Row.Create (Parent, ID);
      if Unit_Name = "" then
         Row.Name.Text (Name & ":");
      else
         Row.Name.Text (Name & " (" & Unit_Name & "):");
      end if;
      Row.Description.Text (Description);
      Row.Data.Place_Inside_Top_Of (Row.Data_Col);
   end Create;

   function Get_Data (Input : Parameter_Row) return T is
   begin
      return Get (Input.Data);
   end Get_Data;

   procedure Set_Data (Input : in out Parameter_Row; Value : T) is
   begin
      Set (Input.Data, Value);
   end Set_Data;

end Prunt.GUI.Parameter_Rows;
