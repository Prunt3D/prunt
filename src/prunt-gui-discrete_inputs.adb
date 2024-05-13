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

package body Prunt.GUI.Discrete_Inputs is

   overriding procedure Create
     (Element         : in out Discrete_Input;
      Form            : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Multiple_Select :        Boolean  := False;
      Visible_Lines   :        Positive := 1;
      Name            :        UXString := "";
      ID              :        UXString := "")
   is
   begin
      Gnoga.Gui.Element.Form.Selection_Type (Element).Create
        (Form => Form, Multiple_Select => Multiple_Select, Visible_Lines => Visible_Lines, Name => Name, ID => ID);

      for I in T loop
         Element.Add_Option (Value => UXStrings.From_UTF_8 (I'Image), Text => UXStrings.From_UTF_8 (I'Image));
      end loop;
   end Create;

   function Get (Input : Discrete_Input) return T is
   begin
      return T'Value (Input.Value.To_UTF_8);
   end Get;

   procedure Set (Input : in out Discrete_Input; Value : T) is
   begin
      for I in 1 .. Input.Length loop
         Input.Selected (I, Input.Value (I) = UXStrings.From_UTF_8 (Value'Image));
      end loop;
   end Set;

end Prunt.GUI.Discrete_Inputs;
