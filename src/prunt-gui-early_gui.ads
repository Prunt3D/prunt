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

with Prunt.Config;
private with Prunt.GUI.Config_Editor;
with Gnoga.Types;
with Gnoga.Gui.Window;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.Element.Table;
with Prunt.Logger;
with Prunt.GUI.Cards_Table; use Prunt.GUI.Cards_Table;
with UXStrings;             use UXStrings;

generic
   with package My_Logger is new Prunt.Logger (<>);
   Fatal_Exception_Occurrence_Holder : in out Fatal_Exception_Occurrence_Holder_Type;
package Prunt.GUI.Early_GUI is

   procedure Run;
   procedure Stop;
   procedure Show_Update_Button;
   procedure Block_Until_Update_Allowed;

private

   protected Update_Check is
      procedure Set_Check_For_Update;
      function Get_Check_For_Update return Boolean;
      procedure Set_Update_Allowed;
      entry Block_Until_Check_For_Update;
      entry Block_Until_Update_Allowed;
   private
      Check_For_Update : Boolean := False;
      Update_Allowed   : Boolean := False;
   end Update_Check;

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      Main_Window : aliased Gnoga.Gui.Window.Pointer_To_Window_Class;

      Loading_Div : aliased Gnoga.Gui.Element.Common.DIV_Type;

      Fatal_Error_Div : aliased Gnoga.Gui.Element.Common.DIV_Type;

      Main_Table : aliased Cards_Table_Type;

      Log_Widget : aliased Gnoga.Gui.View.Console.Console_View_Type;

      Status_Table         : aliased Gnoga.Gui.Element.Table.Table_Type;
      Status_Message_Row   : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Status_Message_Text  : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Status_Update_Row    : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Status_Update_Div    : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Status_Update_Form   : aliased Gnoga.Gui.Element.Form.Form_Type;
      Status_Update_Button : aliased Gnoga.Gui.Element.Form.Submit_Button_Type;
   end record;

   type App_Access is access all App_Data;
end Prunt.GUI.Early_GUI;
