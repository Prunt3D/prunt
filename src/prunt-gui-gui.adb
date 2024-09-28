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
               Pos  : constant Position := Get_Position;
               Text : UXString          := From_UTF_8 ("");
               LF   : Character renames Ada.Characters.Latin_1.LF;
            begin
               Append (Text, From_UTF_8 ("Machine position (not accounting for G92 or retraction):" & LF));
               for A in Axis_Name loop
                  Append (Text, From_UTF_8 (A'Image & ": ") & DF_Image (Pos (A) / mm) & From_UTF_8 (" mm" & LF));
               end loop;

               Append (Text, From_UTF_8 (LF & "Temperatures:" & LF));
               for T in My_Config.Thermistor_Name loop
                  Append
                    (Text,
                     From_UTF_8 (T'Image & ": ") & DF_Image (Get_Temperature (T) / celcius) & From_UTF_8 (" C" & LF));
               end loop;

               Append (Text, From_UTF_8 (LF & "Heater powers:" & LF));
               for H in My_Config.Heater_Name loop
                  Append (Text, From_UTF_8 (H'Image & ": ") & DF_Image (Get_Heater_Power (H)) & From_UTF_8 ("" & LF));
               end loop;

               Append (Text, From_UTF_8 (LF & "Input switch states:" & LF));
               for S in My_Config.Input_Switch_Name loop
                  Append (Text, From_UTF_8 (S'Image & ": " & Get_Input_Switch_State (S)'Image & LF));
               end loop;

               App.Status_Message_Text.Inner_HTML (To_HTML (Text));

               declare
                  JS_Text : UXString := From_UTF_8 ("");
               begin
                  for T in My_Config.Thermistor_Name loop
                     Append (JS_Text, DF_Image (Get_Temperature (T) / celcius) & ",");
                  end loop;

                  Gnoga.Server.Connection.Execute_Script
                    (App.Status_Thermal_Chart_Div.Connection_ID,
                     "window.status_thermal_chart_data" &
                     ".forEach(function (a) { if (a.length >= 1200) {a.splice(0, 1);} });" &
                     "window.status_thermal_chart_data.forEach(function (a, i) { a.push({x: " &
                     From_UTF_8 (Ada.Real_Time.Clock'Image) & " * 1000, y: [" & JS_Text & "][i]}); });" &
                     "window.status_thermal_chart.update();");
               end;

               declare
                  JS_Text : UXString := From_UTF_8 ("");
               begin
                  for H in My_Config.Heater_Name loop
                     Append (JS_Text, DF_Image (Get_Heater_Power (H)) & ",");
                  end loop;

                  Gnoga.Server.Connection.Execute_Script
                    (App.Status_Heater_Power_Chart_Div.Connection_ID,
                     "window.status_heater_power_chart_data" &
                     ".forEach(function (a) { if (a.length >= 1200) {a.splice(0, 1);} });" &
                     "window.status_heater_power_chart_data.forEach(function (a, i) { a.push({x: " &
                     From_UTF_8 (Ada.Real_Time.Clock'Image) & " * 1000, y: [" & JS_Text & "][i]}); });" &
                     "window.status_heater_power_chart.update();");
               end;
            exception
               when Gnoga.Server.Connection.Connection_Error =>
                  null; --  We ignore this error because it can be caused by a connection being closed during loading.
               when E : others                               =>
                  Fatal_Exception_Occurrence_Holder.Set
                    (Ada.Task_Termination.Abnormal, Ada.Task_Identification.Current_Task, E);
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
         pragma Unreferenced (Object);
      begin
         Pause_Stepgen;
      end On_Pause_Submit;

      procedure On_Resume_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
      begin
         Resume_Stepgen;
      end On_Resume_Submit;

      procedure On_Manual_Gcode_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         pragma Unreferenced (Object);
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
         pragma Unreferenced (Object);
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
         pragma Unreferenced (Object);
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

               declare
                  JS_Names   : UXString          := "";
                  JS_Visible : UXString          := "";
                  JS_Colours : constant UXString :=
                    "d3.color('#e6194b'), d3.color('#3cb44b'), d3.color('#ffe119'), d3.color('#4363d8'), " &
                    "d3.color('#f58231'), d3.color('#911eb4'), d3.color('#46f0f0'), d3.color('#f032e6'), " &
                    "d3.color('#bcf60c'), d3.color('#fabebe'), d3.color('#008080'), d3.color('#e6beff'), " &
                    "d3.color('#9a6324'), d3.color('#fffac8'), d3.color('#800000'), d3.color('#aaffc3'), " &
                    "d3.color('#808000'), d3.color('#ffd8b1'), d3.color('#000075'), d3.color('#808080'), ";
               begin
                  for T in My_Config.Thermistor_Name loop
                     JS_Names.Append (From_UTF_8 ("'" & T'Image & "', "));
                     declare
                        use type Thermistors.Thermistor_Kind;
                        Params : Thermistors.Thermistor_Parameters;
                     begin
                        My_Config.Config_File.Read (Params, T);
                        JS_Visible.Append
                          (From_UTF_8 ("" & Boolean'(Params.Kind /= Thermistors.Disabled_Kind)'Image & ", "));
                     end;
                  end loop;

                  App.Status_Thermal_Chart_Row.Create (App.Status_Table);
                  App.Status_Thermal_Chart_Div.Create (App.Status_Thermal_Chart_Row);
                  App.Status_Thermal_Chart_Div.Style ("width", "500px");
                  App.Status_Thermal_Chart_Div.Style ("height", "400px");
                  --!pp off
                  pragma Warnings (Off, "this line is too long");
                  Gnoga.Server.Connection.Execute_Script
                    (App.Status_Thermal_Chart_Div.Connection_ID,
                       "window.status_thermal_chart_data = Array.from(Array(" & From_UTF_8 (My_Config.Thermistor_Name'Pos (My_Config.Thermistor_Name'Last)'Image) & " + 1), () => new Array(0));"
                       & "window.status_thermal_chart_base_time = Date.now() - 1000 * " & From_UTF_8 (Ada.Real_Time.Clock'Image) & ";"
                       & "window.status_thermal_chart = new TimeChart(document.getElementById('" & App.Status_Thermal_Chart_Div.ID & "'), {"
                       & "    series: window.status_thermal_chart_data.map(function(a, i) {"
                       & "        return {"
                       & "            name: [" & JS_Names & "][i],"
                       & "            data: a,"
                       & "            visible: true,"
                       & "            color: [" & JS_Colours & "][i],"
                       & "            lineWidth: 1"
                       & "        }"
                       & "    }).filter((a, i) => [" & JS_Visible & "][i]),"
                       & "    realTime: true,"
                       & "    xRange: {"
                       & "        min: 0,"
                       & "        max: 600000"
                       & "    },"
                       & "    yRange: 'auto',"
                       & "    baseTime: window.status_thermal_chart_base_time,"
                       & "    legend: false,"
                       & "    tooltip: {"
                       & "        enabled: true,"
                       & "        xFormatter: (x) => new Date(x + window.status_thermal_chart_base_time).toLocaleString([], {"
                       & "            hour: '2-digit',"
                       & "            minute: '2-digit',"
                       & "            second: '2-digit'"
                       & "        }),"
                       & "    }"
                       & "});"
                       & "window.status_thermal_chart.update();"
                       & "window.status_thermal_chart.onResize();");
                  pragma Warnings (On, "this line is too long");
                 --!pp on
               end;

               declare
                  JS_Names              : UXString          := "";
                  JS_Colours            : constant UXString :=
                    "d3.color('#e6194b'), d3.color('#3cb44b'), d3.color('#ffe119'), d3.color('#4363d8'), " &
                    "d3.color('#f58231'), d3.color('#911eb4'), d3.color('#46f0f0'), d3.color('#f032e6'), " &
                    "d3.color('#bcf60c'), d3.color('#fabebe'), d3.color('#008080'), d3.color('#e6beff'), " &
                    "d3.color('#9a6324'), d3.color('#fffac8'), d3.color('#800000'), d3.color('#aaffc3'), " &
                    "d3.color('#808000'), d3.color('#ffd8b1'), d3.color('#000075'), d3.color('#808080'), ";
                  JS_Colour_Assignments : UXString          := "";
               begin
                  for H in My_Config.Heater_Name loop
                     JS_Names.Append (From_UTF_8 ("'" & H'Image & "', "));

                     declare
                        Params : My_Config.Heater_Full_Parameters;
                     begin
                        My_Config.Config_File.Read (Params, H);
                        JS_Colour_Assignments.Append
                          (From_UTF_8 (My_Config.Thermistor_Name'Pos (Params.Thermistor)'Image & ", "));
                     end;
                  end loop;

                  App.Status_Heater_Power_Chart_Row.Create (App.Status_Table);
                  App.Status_Heater_Power_Chart_Div.Create (App.Status_Heater_Power_Chart_Row);
                  App.Status_Heater_Power_Chart_Div.Style ("width", "500px");
                  App.Status_Heater_Power_Chart_Div.Style ("height", "400px");
                  --!pp off
                  pragma Warnings (Off, "this line is too long");
                  Gnoga.Server.Connection.Execute_Script
                    (App.Status_Heater_Power_Chart_Div.Connection_ID,
                     "window.status_heater_power_chart_data = Array.from(Array(" & From_UTF_8 (My_Config.Heater_Name'Pos (My_Config.Heater_Name'Last)'Image) & " + 1), () => new Array(0));"
                       & "window.status_heater_power_chart_base_time = Date.now() - 1000 * " & From_UTF_8 (Ada.Real_Time.Clock'Image) & ";"
                       & "window.status_heater_power_chart = new TimeChart(document.getElementById('" & App.Status_Heater_Power_Chart_Div.ID & "'), {"
                       & "    series: window.status_heater_power_chart_data.map(function(a, i) {"
                       & "        return {"
                       & "            name: [" & JS_Names & "][i],"
                       & "            data: a,"
                       & "            visible: true,"
                       & "            color: [" & JS_Colours & "][[" & JS_Colour_Assignments & "][i]],"
                       & "            lineWidth: 1"
                       & "        }"
                       & "    }),"
                       & "    realTime: true,"
                       & "    xRange: {"
                       & "        min: 0,"
                       & "        max: 600000"
                       & "    },"
                       & "    yRange: 'auto',"
                       & "    baseTime: window.status_heater_power_chart_base_time,"
                       & "    legend: false,"
                       & "    tooltip: {"
                       & "        enabled: true,"
                       & "        xFormatter: (x) => new Date(x + window.status_heater_power_chart_base_time).toLocaleString([], {"
                       & "            hour: '2-digit',"
                       & "            minute: '2-digit',"
                       & "            second: '2-digit'"
                       & "        }),"
                       & "    }"
                       & "});"
                       & "window.status_heater_power_chart.update();"
                       & "window.status_heater_power_chart.onResize();");
                  pragma Warnings (On, "this line is too long");
                  --!pp on
               end;

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
                       (UXStrings.From_UTF_8
                          (I'Image & " (" & Trim (My_Config.Stepper_Name'Pos (I)'Image, Left) & ")"),
                        App.Config_Editor_Stepper_Widgets (I)'Access);
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
                       (UXStrings.From_UTF_8
                          (I'Image & " (" & Trim (My_Config.Input_Switch_Name'Pos (I)'Image, Left) & ")"),
                        App.Config_Editor_Input_Switch_Widgets (I)'Access);
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
                       (UXStrings.From_UTF_8
                          (I'Image & " (" & Trim (My_Config.Thermistor_Name'Pos (I)'Image, Left) & ")"),
                        App.Config_Editor_Thermistor_Widgets (I)'Access);
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
                       (UXStrings.From_UTF_8 (I'Image & " (" & Trim (My_Config.Heater_Name'Pos (I)'Image, Left) & ")"),
                        App.Config_Editor_Heater_Widgets (I)'Access);
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
                       (UXStrings.From_UTF_8 (I'Image & " (" & Trim (My_Config.Fan_Name'Pos (I)'Image, Left) & ")"),
                        App.Config_Editor_Fan_Widgets (I)'Access);
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

            Status_Updater_Task.Start (App);

            Gnoga.Server.Connection.Execute_Script
              (App.Status_Thermal_Chart_Div.Connection_ID, "window.status_thermal_chart.onResize();");
            Gnoga.Server.Connection.Execute_Script
              (App.Status_Heater_Power_Chart_Div.Connection_ID, "window.status_heater_power_chart.onResize();");
         end if;
      exception
         when Gnoga.Server.Connection.Connection_Error =>
            null; --  We ignore this error because it can be caused by a connection being closed during loading.
         when E : others                                   =>
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

         My_Logger.Set_Receiver (Log_Handle, null);
      end;

      Status_Updater_Task.Stop;

      --  Gnoga leaks memory if we allow it to handle freeing the data.
      Free_Data (App);
   end On_Connect;

   procedure Log_And_Switch_Tab (Object : Gnoga.Types.Pointer_to_Connection_Data_Class; Message : String) is
      procedure Inner (App : App_Access) is
      begin
         My_Logger.Log (Message);
         App.Main_Table.Tabs.Select_Tab ("Log");
      end Inner;
   begin
      Inner (App_Access (Object));
   end Log_And_Switch_Tab;

   procedure Run is
   begin
      Gnoga.Application.Title ("Prunt");
      Gnoga.Application.HTML_On_Close ("Prunt terminated. Reload this page to reconnect.");
      Gnoga.Application.Multi_Connect.Initialize (Verbose => False);
      Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");
      Gnoga.Application.Multi_Connect.Message_Loop;
   end Run;

end Prunt.GUI.GUI;
