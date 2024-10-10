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

with Ada.Unchecked_Deallocation;
with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.View.Card;
with Gnoga.Gui.Base;
with Gnoga.Server;
with Ada.Directories;
with Ada.Characters.Latin_1;
with Gnoga.Server.Connection;
with Ada.Real_Time;
with Prunt.Thermistors;
with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Prunt.GUI.Early_GUI is

   function To_HTML (S : UXString) return UXString is
      function Translate_Character (C : Unicode_Character) return UXString is
      begin
         if C = Unicode_Character'Val (10) then
            return "<br>";
         elsif C = Unicode_Character'Val (13) then
            return "<br>";
         elsif C = '&' then
            return "&amp;";
         elsif C = '<' then
            return "&lt;";
         elsif C = '>' then
            return "&gt;";
         elsif C = '"' then
            return "&quot;";
         elsif C = ''' then
            return "&#39;";
         else
            return From_Unicode (C);
         end if;
      end Translate_Character;

      R : UXString;
   begin
      for C in S loop
         Append (R, Translate_Character (S (C)));
      end loop;

      return R;
   end To_HTML;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      App : App_Access := new App_Data;

      procedure Free_Data is new Ada.Unchecked_Deallocation (App_Data, App_Access);

      procedure On_Update_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
      begin
         if Update_Check.Get_Check_For_Update then
            Update_Check.Set_Update_Allowed;
         else
            raise Constraint_Error with "Update button should not be visible.";
         end if;
      end On_Update_Submit;
   begin
      Main_Window.Connection_Data (Data => App, Dynamic => False);

      App.Main_Window := Main_Window'Unchecked_Access;

      App.Main_Window.Disable_Auto_Set_View;

      App.Loading_Div.Create
        (App.Main_Window.all,
         "<h1>Loading, please wait.</h1>" &
         "<p>If this takes more than a few seconds then it is likely that something has gone wrong, " &
         "check the console output.</p>");
      App.Loading_Div.Place_Inside_Top_Of (App.Main_Window.Document.Body_Element.all);

      Main_Window.Buffer_Connection (True);

      App.Main_Table.Create (App.Main_Window.all);
      App.Main_Table.Hidden (True);

      App.Main_Table.Place_Inside_Top_Of (App.Main_Window.Document.Body_Element.all);
      --  This avoids the window resizing our tables.

      begin
         if not Fatal_Exception_Occurrence_Holder.Is_Set then
            --  Status
            begin
               App.Status_Table.Create (App.Main_Table.Cards);
               App.Main_Table.Add_Tab ("Status", App.Status_Table'Access);

               App.Status_Message_Row.Create (App.Status_Table);
               App.Status_Message_Text.Create (App.Status_Message_Row);
               App.Status_Message_Text.Inner_HTML ("Waiting for board setup.");

               App.Status_Update_Row.Create (App.Status_Table);
               App.Status_Update_Div.Create (App.Status_Update_Row);
               App.Status_Update_Div.Hidden (True);
               App.Status_Update_Form.Create (App.Status_Update_Div);
               App.Status_Update_Button.Create (App.Status_Update_Form, Value => "Update");
               App.Status_Update_Form.On_Submit_Handler (On_Update_Submit'Unrestricted_Access);
            end;

            --  Log
            begin
               App.Log_Widget.Create (App.Main_Table.Cards);
               App.Log_Widget.Style ("height", "500px");
               App.Main_Table.Add_Tab ("Log", App.Log_Widget'Access);
            end;

            App.Main_Table.Cards.Show_Card ("Status");
            App.Loading_Div.Hidden (True);
            App.Main_Table.Hidden (False);

            Main_Window.Buffer_Connection (False);
         end if;
      exception
         when Gnoga.Server.Connection.Connection_Error =>
            null; --  We ignore this error because it can be caused by a connection being closed during loading.
         when E : others                               =>
            Fatal_Exception_Occurrence_Holder.Set
              (Ada.Task_Termination.Abnormal, Ada.Task_Identification.Current_Task, E);
      end;

      declare
         Log_Handle : My_Logger.Handle;

         procedure Log_To_Tab (Message : String) is
         begin
            App.Log_Widget.Put_Line (To_HTML (UXStrings.From_UTF_8 (Message)));
            App.Log_Widget.New_Line;
         end Log_To_Tab;
      begin
         My_Logger.Set_Receiver (Log_Handle, Log_To_Tab'Unrestricted_Access);

         select
            Connection.Hold;
         then abort
            declare
               Occurrence : Ada.Exceptions.Exception_Occurrence;
            begin
               select
                  Fatal_Exception_Occurrence_Holder.Get (Occurrence);
               then abort
                  Update_Check.Block_Until_Check_For_Update;
                  App.Status_Message_Text.Inner_HTML ("Firmware update required, click below button to allow.");
                  App.Status_Update_Div.Hidden (False);
                  Update_Check.Block_Until_Update_Allowed;
                  App.Status_Message_Text.Inner_HTML ("Waiting for update and board setup.");
                  App.Status_Update_Div.Hidden (True);
                  Fatal_Exception_Occurrence_Holder.Get (Occurrence);
               end select;
               Fatal_Exception_Occurrence_Holder.Get (Occurrence);
               --  Above line to compensate for GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=116832
               App.Fatal_Error_Div.Create
                 (App.Main_Window.all,
                  UXStrings.From_UTF_8 ("<h1>FATAL ERROR</h1><p>") &
                  To_HTML (UXStrings.From_UTF_8 (Ada.Exceptions.Exception_Information (Occurrence))) &
                  UXStrings.From_UTF_8 ("</p>"));
               App.Fatal_Error_Div.Place_Inside_Top_Of (App.Main_Window.Document.Body_Element.all);
               App.Main_Table.Hidden (True);
               App.Loading_Div.Hidden (True);
               Main_Window.Buffer_Connection (False);
            end;
            Connection.Hold;
         end select;

         My_Logger.Set_Receiver (Log_Handle, null);
      end;

      --  Gnoga leaks memory if we allow it to handle freeing the data.
      Free_Data (App);
   end On_Connect;

   procedure Run is
   begin
      Gnoga.Application.Title ("Prunt");
      Gnoga.Application.HTML_On_Close
        ("Early setup done. Page will reload. <meta http-equiv=""refresh"" content=""0""/>");
      Gnoga.Application.Multi_Connect.Initialize (Verbose => False, Host => From_UTF_8 (Host), Port => Integer (Port));
      Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");
      Gnoga.Application.Multi_Connect.Message_Loop;
   end Run;

   procedure Stop is
   begin
      Gnoga.Application.Multi_Connect.End_Application;
   end Stop;

   procedure Show_Update_Button is
   begin
      Update_Check.Set_Check_For_Update;
   end Show_Update_Button;

   procedure Block_Until_Update_Allowed is
   begin
      Update_Check.Block_Until_Update_Allowed;
   end Block_Until_Update_Allowed;

   protected body Update_Check is
      procedure Set_Check_For_Update is
      begin
         Check_For_Update := True;
      end Set_Check_For_Update;

      function Get_Check_For_Update return Boolean is
      begin
         return Check_For_Update;
      end Get_Check_For_Update;

      procedure Set_Update_Allowed is
      begin
         Update_Allowed := True;
      end Set_Update_Allowed;

      entry Block_Until_Check_For_Update when Check_For_Update is
      begin
         null;
      end Block_Until_Check_For_Update;

      entry Block_Until_Update_Allowed when Update_Allowed is
      begin
         null;
      end Block_Until_Update_Allowed;
   end Update_Check;

end Prunt.GUI.Early_GUI;
