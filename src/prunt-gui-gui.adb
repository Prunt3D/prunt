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

package body Prunt.GUI.GUI is

   function To_HTML (S : UXString) return UXString is
      function Translate_Character (C : Unicode_Character) return UXString is
      begin
         if C = Unicode_Character'Val (10) then
            return "</br>";
         elsif C = Unicode_Character'Val (13) then
            return "</br>";
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

   task body Status_Updater is
      App : App_Access;
   begin
      accept Start (In_App : App_Access) do
         App := In_App;
      end Start;

      loop
         select
            accept Stop;
            exit;
         or
            delay 0.5;
            declare
               Pos  : Position := Get_Position;
               Text : UXString := From_UTF_8 ("");
               CR   : Character renames Ada.Characters.Latin_1.CR;
            begin
               Append (Text, From_UTF_8 ("Status:" & CR & "    " & Get_Status_Message & CR & CR));

               Append (Text, From_UTF_8 ("Position:" & CR));
               for A in Axis_Name loop
                  Append (Text, From_UTF_8 (A'Image & ": ") & DF_Image (Pos (A) / mm) & From_UTF_8 (" mm" & CR));
               end loop;

               Append (Text, From_UTF_8 (CR & "Temperatures:" & CR));
               for T in My_Config.Thermistor_Name loop
                  Append
                    (Text,
                     From_UTF_8 (T'Image & ": ") & DF_Image (Get_Temperature (T) / celcius) &
                     From_UTF_8 (" C" & CR));
               end loop;

               App.Status_Message_Text.Inner_HTML (To_HTML (Text));
            end;
         end select;
      end loop;
   end Status_Updater;

   procedure On_Connect
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      Status_Updater_Task : Status_Updater;

      App : App_Access := new App_Data;

      procedure Free_Data is new Ada.Unchecked_Deallocation (App_Data, App_Access);

      procedure On_Pause_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Pause_Stepgen;
      end On_Pause_Submit;

      procedure On_Resume_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Resume_Stepgen;
      end On_Resume_Submit;

      procedure On_Manual_Gcode_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         Command   : constant UXString := App.Manual_Gcode_Form_Entry.Value;
         Succeeded : Boolean;
      begin
         Submit_Gcode_Command (To_UTF_8 (Command), Succeeded);
         if Succeeded then
            App.Manual_Gcode_Log.Put_Line (To_HTML (Command));
            App.Manual_Gcode_Form_Entry.Value ("");
         else
            App.Manual_Gcode_Log.Put_Line
              (From_UTF_8 ("Commands can not run while other commands or files are running."));
         end if;
      end On_Manual_Gcode_Submit;

      procedure On_Auto_Gcode_File_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         Path      : constant UXString := App.Auto_Gcode_File_Form_Entry.Value;
         Succeeded : Boolean;
      begin
         if Path /= "" then
            Submit_Gcode_File
              (To_UTF_8
                 (Gnoga.Server.Application_Directory & Gnoga.Server.Directory_Separator & "upload" &
                  Gnoga.Server.Directory_Separator & Path),
               Succeeded);
            if Succeeded then
               App.Auto_Gcode_Log.Put_Line (To_HTML (Path));
               App.Auto_Gcode_File_Form_Entry.Value ("");
            else
               App.Auto_Gcode_Log.Put_Line (From_UTF_8 ("Files can not run while other files are running."));
            end if;
         end if;
      end On_Auto_Gcode_File_Submit;

      procedure On_Auto_Gcode_Refresh_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         use Ada.Directories;

         procedure Fill_List (Directory_Entry : Directory_Entry_Type) is
         begin
            if Kind (Directory_Entry) = Ordinary_File then
               App.Auto_Gcode_File_Form_Entry.Add_Option
                 (From_UTF_8 (Simple_Name (Directory_Entry)), From_UTF_8 (Simple_Name (Directory_Entry)));
            end if;
         end Fill_List;
      begin
         App.Auto_Gcode_File_Form_Entry.Empty_Options;

         Ada.Directories.Search
           (Directory => To_UTF_8 (Gnoga.Server.Application_Directory & Gnoga.Server.Directory_Separator & "upload"),
            Pattern   => "*.gcode",
            Process   => Fill_List'Access);

         App.Auto_Gcode_File_Form_Entry.Value ("");
      end On_Auto_Gcode_Refresh_Submit;

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

               Status_Updater_Task.Start (App);

               App.Status_Pause_Resume_Row.Create (App.Status_Table);
               App.Status_Pause_Resume_Div.Create (App.Status_Pause_Resume_Row);
               App.Status_Pause_Form.Create (App.Status_Pause_Resume_Div);
               App.Status_Pause_Button.Create (App.Status_Pause_Form, Value => "Pause");
               App.Status_Pause_Form.On_Submit_Handler (On_Pause_Submit'Unrestricted_Access);
               App.Status_Resume_Form.Create (App.Status_Pause_Resume_Div);
               App.Status_Resume_Button.Create (App.Status_Resume_Form, Value => "Resume");
               App.Status_Resume_Form.On_Submit_Handler (On_Resume_Submit'Unrestricted_Access);
            end;

            --  Config Editor
            begin
               App.Config_Editor_Table.Create (App.Main_Table.Cards);
               App.Main_Table.Add_Tab ("Config Editor", App.Config_Editor_Table'Access);

               --  Config Editor > Prunt
               App.Config_Editor_Prunt_Widget.Create_Widget (App.Config_Editor_Table.Cards);
               App.Config_Editor_Table.Add_Tab ("Prunt", App.Config_Editor_Prunt_Widget'Access);

               --  Config Editor > Steppers
               begin
                  App.Config_Editor_Steppers_Table.Create (App.Config_Editor_Table.Cards);
                  App.Config_Editor_Table.Add_Tab ("Steppers", App.Config_Editor_Steppers_Table'Access);

                  --  Config Editor > Steppers > X
                  for I in App.Config_Editor_Stepper_Widgets'Range loop
                     App.Config_Editor_Stepper_Widgets (I).Create_Widget (App.Config_Editor_Steppers_Table.Cards, I);
                     App.Config_Editor_Steppers_Table.Add_Tab
                       (UXStrings.From_UTF_8 (I'Image), App.Config_Editor_Stepper_Widgets (I)'Access);
                  end loop;
               end;

               --  Config Editor > Kinematics
               App.Config_Editor_Kinematics_Widget.Create_Widget (App.Config_Editor_Table.Cards);
               App.Config_Editor_Table.Add_Tab ("Kinematics", App.Config_Editor_Kinematics_Widget'Access);

               --  Config Editor > Input Switches
               begin
                  App.Config_Editor_Input_Switches_Table.Create (App.Config_Editor_Table.Cards);
                  App.Config_Editor_Table.Add_Tab ("Input Switches", App.Config_Editor_Input_Switches_Table'Access);

                  --  Config Editor > Input Switches > X
                  for I in App.Config_Editor_Input_Switch_Widgets'Range loop
                     App.Config_Editor_Input_Switch_Widgets (I).Create_Widget
                       (App.Config_Editor_Input_Switches_Table.Cards, I);
                     App.Config_Editor_Input_Switches_Table.Add_Tab
                       (UXStrings.From_UTF_8 (I'Image), App.Config_Editor_Input_Switch_Widgets (I)'Access);
                  end loop;
               end;

               --  Config Editor > Homing
               begin
                  App.Config_Editor_Homing_Table.Create (App.Config_Editor_Table.Cards);
                  App.Config_Editor_Table.Add_Tab ("Homing", App.Config_Editor_Homing_Table'Access);

                  --  Config Editor > Homing > X
                  for I in App.Config_Editor_Homing_Widgets'Range loop
                     App.Config_Editor_Homing_Widgets (I).Create_Widget (App.Config_Editor_Homing_Table.Cards, I);
                     App.Config_Editor_Homing_Table.Add_Tab
                       (UXStrings.From_UTF_8 (I'Image), App.Config_Editor_Homing_Widgets (I)'Access);
                  end loop;
               end;

               --  Config Editor > Extruder
               App.Config_Editor_Extruder_Widget.Create_Widget (App.Config_Editor_Table.Cards);
               App.Config_Editor_Table.Add_Tab ("Extruder", App.Config_Editor_Extruder_Widget'Access);

               --  Config Editor > Thermistors
               begin
                  App.Config_Editor_Thermistors_Table.Create (App.Config_Editor_Table.Cards);
                  App.Config_Editor_Table.Add_Tab ("Thermistors", App.Config_Editor_Thermistors_Table'Access);

                  --  Config Editor > Thermistors > X
                  for I in App.Config_Editor_Thermistor_Widgets'Range loop
                     App.Config_Editor_Thermistor_Widgets (I).Create_Widget
                       (App.Config_Editor_Thermistors_Table.Cards, I);
                     App.Config_Editor_Thermistors_Table.Add_Tab
                       (UXStrings.From_UTF_8 (I'Image), App.Config_Editor_Thermistor_Widgets (I)'Access);
                  end loop;
               end;

               --  Config Editor > Heaters
               begin
                  App.Config_Editor_Heaters_Table.Create (App.Config_Editor_Table.Cards);
                  App.Config_Editor_Table.Add_Tab ("Heaters", App.Config_Editor_Heaters_Table'Access);

                  --  Config Editor > Heaters > X
                  for I in App.Config_Editor_Heater_Widgets'Range loop
                     App.Config_Editor_Heater_Widgets (I).Create_Widget (App.Config_Editor_Heaters_Table.Cards, I);
                     App.Config_Editor_Heaters_Table.Add_Tab
                       (UXStrings.From_UTF_8 (I'Image), App.Config_Editor_Heater_Widgets (I)'Access);
                  end loop;
               end;

               --  Config Editor > Bed_Mesh
               App.Config_Editor_Bed_Mesh_Widget.Create_Widget (App.Config_Editor_Table.Cards);
               App.Config_Editor_Table.Add_Tab ("Bed Mesh", App.Config_Editor_Bed_Mesh_Widget'Access);

               --  Config Editor > Fans
               begin
                  App.Config_Editor_Fans_Table.Create (App.Config_Editor_Table.Cards);
                  App.Config_Editor_Table.Add_Tab ("Fans", App.Config_Editor_Fans_Table'Access);

                  --  Config Editor > Fans > X
                  for I in App.Config_Editor_Fan_Widgets'Range loop
                     App.Config_Editor_Fan_Widgets (I).Create_Widget (App.Config_Editor_Fans_Table.Cards, I);
                     App.Config_Editor_Fans_Table.Add_Tab
                       (UXStrings.From_UTF_8 (I'Image), App.Config_Editor_Fan_Widgets (I)'Access);
                  end loop;
               end;

               --  Config Editor > G-Code Assignment
               App.Config_Editor_G_Code_Assignment_Widget.Create_Widget (App.Config_Editor_Table.Cards);
               App.Config_Editor_Table.Add_Tab
                 ("G-Code Assignment", App.Config_Editor_G_Code_Assignment_Widget'Access);

            end;

            --  G-Code Console
            begin
               App.Manual_Gcode_Table.Create (App.Main_Table.Cards);

               App.Manual_Gcode_Log_Row.Create (App.Manual_Gcode_Table);
               App.Manual_Gcode_Log.Create (App.Manual_Gcode_Log_Row);
               App.Manual_Gcode_Log.Style ("height", "500px");

               App.Manual_Gcode_Form_Row.Create (App.Manual_Gcode_Table);
               App.Manual_Gcode_Form_Div.Create (App.Manual_Gcode_Form_Row);
               App.Manual_Gcode_Form.Create (App.Manual_Gcode_Form_Div);
               App.Manual_Gcode_Form_Entry.Create (App.Manual_Gcode_Form, Size => 40);
               App.Manual_Gcode_Form_Submit_Button.Create (App.Manual_Gcode_Form, Value => "Submit");
               App.Manual_Gcode_Form.On_Submit_Handler (On_Manual_Gcode_Submit'Unrestricted_Access);

               App.Main_Table.Add_Tab ("G-Code Console", App.Manual_Gcode_Table'Access);
            end;

            --  Run File
            begin
               App.Auto_Gcode_Table.Create (App.Main_Table.Cards);

               App.Auto_Gcode_Log_Row.Create (App.Auto_Gcode_Table);
               App.Auto_Gcode_Log.Create (App.Auto_Gcode_Log_Row);
               App.Auto_Gcode_Log.Style ("height", "500px");

               App.Auto_Gcode_Form_Row.Create (App.Auto_Gcode_Table);
               App.Auto_Gcode_Form_Div.Create (App.Auto_Gcode_Form_Row);
               App.Auto_Gcode_File_Form.Create (App.Auto_Gcode_Form_Div);
               App.Auto_Gcode_File_Form_Entry.Create (App.Auto_Gcode_File_Form);
               App.Auto_Gcode_File_Form_Submit_Button.Create (App.Auto_Gcode_File_Form, Value => "Run File");
               App.Auto_Gcode_File_Form.On_Submit_Handler (On_Auto_Gcode_File_Submit'Unrestricted_Access);

               App.Auto_Gcode_Refresh_Form.Create (App.Auto_Gcode_Form_Div);
               App.Auto_Gcode_Refresh_Form_Submit_Button.Create (App.Auto_Gcode_Refresh_Form, Value => "Refresh");
               App.Auto_Gcode_Refresh_Form.On_Submit_Handler (On_Auto_Gcode_Refresh_Submit'Unrestricted_Access);

               On_Auto_Gcode_Refresh_Submit (App.Auto_Gcode_Refresh_Form);

               App.Main_Table.Add_Tab ("Run File", App.Auto_Gcode_Table'Access);
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
         when E : others =>
            Fatal_Exception_Occurrence_Holder.Set
              (Ada.Task_Termination.Abnormal, Ada.Task_Identification.Current_Task, E);
      end;

      select
         Connection.Hold;
      then abort
         declare
            Occurrence : Ada.Exceptions.Exception_Occurrence;
         begin
            Fatal_Exception_Occurrence_Holder.Get (Occurrence);
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

      Status_Updater_Task.Stop;

      --  Gnoga leaks memory if we allow it to handle freeing the data.
      Free_Data (App);
   end On_Connect;

   procedure Log_And_Switch_Tab (Object : Gnoga.Types.Pointer_to_Connection_Data_Class; Message : UXString) is
      procedure Inner (App : App_Access) is
      begin
         App.Log_Widget.Put_Line (To_HTML (Message));
         App.Log_Widget.New_Line;
         App.Main_Table.Tabs.Select_Tab ("Log");
      end Inner;
   begin
      Inner (App_Access (Object));
   end Log_And_Switch_Tab;

   procedure Run is
   begin
      Gnoga.Application.Title ("Prunt");
      Gnoga.Application.HTML_On_Close ("Prunt terminated. Reload this page to reconnect.");
      Gnoga.Application.Multi_Connect.Initialize;
      Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");
      Gnoga.Application.Multi_Connect.Message_Loop;
   end Run;

end Prunt.GUI.GUI;
