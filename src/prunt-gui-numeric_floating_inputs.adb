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

with Ada.Strings;

package body Prunt.GUI.Numeric_Floating_Inputs is

   pragma Warnings (Off, "dimensions mismatch");

   overriding procedure Create
     (Element : in out Numeric_Input;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class;
      Value   :        UXString := "";
      Name    :        UXString := "";
      ID      :        UXString := "")
   is
   begin
      Gnoga.Gui.Element.Form.Number_Type (Element).Create (Form => Form, Value => Value, Name => Name, ID => ID);
      Gnoga.Gui.Element.Form.Number_Type (Element).Minimum
        (UXStrings.From_UTF_8 (T'First'Image).Trim (Ada.Strings.Both));
      Gnoga.Gui.Element.Form.Number_Type (Element).Maximum
        (UXStrings.From_UTF_8 (T'Last'Image).Trim (Ada.Strings.Both));
      Gnoga.Gui.Element.Form.Number_Type (Element).Step ("any");
   end Create;

   procedure Create_For_Parameter_Row
     (Element : in out Numeric_Input;
      Parent  : in out Gnoga.Gui.Element.Element_Type'Class;
      Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class)
   is
      pragma Unreferenced (Parent);
   begin
      Create (Element, Form);
   end Create_For_Parameter_Row;

   function Get (Input : Numeric_Input) return T is
   begin
      return T'Value (Input.Value.To_UTF_8);
   end Get;

   function Nice_Image (Number : T) return UXString is
      function T_Image is new Float_Image (T);
      Raw : constant String := T_Image (Number, Fore => 1, Aft => T'Digits - 1, Exp => 0);
   begin
      for I in reverse Raw'Range loop
         if Raw (I) /= '0' then
            if Raw (I) = '.' then
               if I - Raw'First > T'Digits + 1 then
                  return UXStrings.From_UTF_8 (Number'Image).Trim (Ada.Strings.Both);
               else
                  return UXStrings.From_UTF_8 (Raw (Raw'First .. I + 1)).Trim (Ada.Strings.Both);
               end if;
            else
               return UXStrings.From_UTF_8 (Raw (Raw'First .. I)).Trim (Ada.Strings.Both);
            end if;
         end if;
      end loop;

      raise Program_Error;
   end Nice_Image;

   procedure Set (Input : in out Numeric_Input; Value : T) is
   begin
      Input.Value (Nice_Image (Value));
   end Set;

end Prunt.GUI.Numeric_Floating_Inputs;
