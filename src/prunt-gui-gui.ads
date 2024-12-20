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
with GNAT.Sockets;
with Prunt.GUI.Cards_Table; use Prunt.GUI.Cards_Table;
with UXStrings;             use UXStrings;

generic
   with package My_Logger is new Prunt.Logger (<>);
   with package My_Config is new Prunt.Config (<>);
   with function Get_Position return Prunt.Position;
   with function Get_Thermistor_Temperature (Thermistor : My_Config.Thermistor_Name) return Prunt.Temperature;
   with function Get_Stepper_Temperature (Thermistor : My_Config.Stepper_Name) return Prunt.Temperature;
   type Board_Temperature_Probe_Name is (<>);
   with function Get_Board_Temperature (Thermistor : Board_Temperature_Probe_Name) return Prunt.Temperature;
   with function Get_Heater_Power (Heater : My_Config.Heater_Name) return Prunt.PWM_Scale;
   with function Get_Input_Switch_State (Switch : My_Config.Input_Switch_Name) return Prunt.Pin_State;
   with function Get_Tachometer_Frequency (Fan : My_Config.Fan_Name) return Frequency;
   with procedure Submit_Gcode_Command (Command : String; Succeeded : out Boolean);
   with procedure Submit_Gcode_File (Path : String; Succeeded : out Boolean);
   with function Is_Stepgen_Paused return Boolean;
   with procedure Pause_Stepgen;
   with procedure Resume_Stepgen;
   Fatal_Exception_Occurrence_Holder : in out Fatal_Exception_Occurrence_Holder_Type;
   Host : String;
   Port : GNAT.Sockets.Port_Type;
package Prunt.GUI.GUI is

   procedure Run;
   procedure Show_Update_Button;
   procedure Block_Until_Update_Allowed;
   procedure Notify_Startup_Complete (Enabled : Boolean);

private

   procedure Log_And_Switch_Tab (Object : Gnoga.Types.Pointer_to_Connection_Data_Class; Message : String);

   package My_Config_Editor is new Config_Editor (My_Config => My_Config, Log_And_Switch_Tab => Log_And_Switch_Tab);

   type Stepper_Widgets is array (My_Config.Stepper_Name) of aliased My_Config_Editor.Section_Widgets.Stepper_Widget;
   type Input_Switch_Widgets is
     array (My_Config.Input_Switch_Name) of aliased My_Config_Editor.Section_Widgets.Input_Switch_Widget;
   type Homing_Widgets is array (Axis_Name) of aliased My_Config_Editor.Section_Widgets.Homing_Widget;
   type Thermistor_Widgets is
     array (My_Config.Thermistor_Name) of aliased My_Config_Editor.Section_Widgets.Thermistor_Widget;
   type Heater_Widgets is array (My_Config.Heater_Name) of aliased My_Config_Editor.Section_Widgets.Heater_Widget;
   type Fan_Widgets is array (My_Config.Fan_Name) of aliased My_Config_Editor.Section_Widgets.Fan_Widget;

   protected Startup_State is
      procedure Set_Check_For_Update;
      function Get_Check_For_Update return Boolean;
      procedure Set_Update_Allowed;
      entry Block_Until_Check_For_Update;
      entry Block_Until_Update_Allowed;
      procedure Set_Startup_Complete (Enabled : Boolean);
      entry Block_Until_Startup_Complete (Enabled : out Boolean);
   private
      Check_For_Update : Boolean := False;
      Update_Allowed   : Boolean := False;
      Startup_Complete : Boolean := False;
      Prunt_Enabled    : Boolean := False;
   end Startup_State;

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      Main_Window : aliased Gnoga.Gui.Window.Pointer_To_Window_Class;

      Loading_Div : aliased Gnoga.Gui.Element.Common.DIV_Type;

      Fatal_Error_Div : aliased Gnoga.Gui.Element.Common.DIV_Type;

      Main_Table : aliased Cards_Table_Type;

      Config_Editor_Table                    : aliased Cards_Table_Type;
      Config_Editor_Prunt_Widget             : aliased My_Config_Editor.Section_Widgets.Prunt_Widget;
      Config_Editor_Steppers_Table           : aliased Cards_Table_Type;
      Config_Editor_Stepper_Widgets          : aliased Stepper_Widgets;
      Config_Editor_Kinematics_Widget        : aliased My_Config_Editor.Section_Widgets.Kinematics_Widget;
      Config_Editor_Input_Switches_Table     : aliased Cards_Table_Type;
      Config_Editor_Input_Switch_Widgets     : aliased Input_Switch_Widgets;
      Config_Editor_Homing_Table             : aliased Cards_Table_Type;
      Config_Editor_Homing_Widgets           : aliased Homing_Widgets;
      Config_Editor_Extruder_Widget          : aliased My_Config_Editor.Section_Widgets.Extruder_Widget;
      Config_Editor_Thermistors_Table        : aliased Cards_Table_Type;
      Config_Editor_Thermistor_Widgets       : aliased Thermistor_Widgets;
      Config_Editor_Heaters_Table            : aliased Cards_Table_Type;
      Config_Editor_Heater_Widgets           : aliased Heater_Widgets;
      Config_Editor_Bed_Mesh_Widget          : aliased My_Config_Editor.Section_Widgets.Bed_Mesh_Widget;
      Config_Editor_Fans_Table               : aliased Cards_Table_Type;
      Config_Editor_Fan_Widgets              : aliased Fan_Widgets;
      Config_Editor_G_Code_Assignment_Widget : aliased My_Config_Editor.Section_Widgets.G_Code_Assignment_Widget;

      Log_Widget : aliased Gnoga.Gui.View.Console.Console_View_Type;

      Status_Table                  : aliased Gnoga.Gui.Element.Table.Table_Type;
      Status_Message_Row            : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Status_Message_Text           : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Status_Update_Row             : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Status_Update_Div             : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Status_Update_Form            : aliased Gnoga.Gui.Element.Form.Form_Type;
      Status_Update_Button          : aliased Gnoga.Gui.Element.Form.Submit_Button_Type;
      Status_Thermal_Chart_Row      : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Status_Thermal_Chart_Div      : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Status_Heater_Power_Chart_Row : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Status_Heater_Power_Chart_Div : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Status_Pause_Resume_Row       : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Status_Pause_Resume_Div       : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Status_Pause_Form             : aliased Gnoga.Gui.Element.Form.Form_Type;
      Status_Pause_Button           : aliased Gnoga.Gui.Element.Form.Submit_Button_Type;
      Status_Resume_Form            : aliased Gnoga.Gui.Element.Form.Form_Type;
      Status_Resume_Button          : aliased Gnoga.Gui.Element.Form.Submit_Button_Type;

      Manual_Gcode_Table              : aliased Gnoga.Gui.Element.Table.Table_Type;
      Manual_Gcode_Log_Row            : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Manual_Gcode_Log                : aliased Gnoga.Gui.View.Console.Console_View_Type;
      Manual_Gcode_Form_Row           : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Manual_Gcode_Form_Div           : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Manual_Gcode_Form               : aliased Gnoga.Gui.Element.Form.Form_Type;
      Manual_Gcode_Form_Entry         : aliased Gnoga.Gui.Element.Form.Text_Type;
      Manual_Gcode_Form_Submit_Button : aliased Gnoga.Gui.Element.Form.Submit_Button_Type;

      Auto_Gcode_Table                      : aliased Gnoga.Gui.Element.Table.Table_Type;
      Auto_Gcode_Log_Row                    : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Auto_Gcode_Log                        : aliased Gnoga.Gui.View.Console.Console_View_Type;
      Auto_Gcode_Form_Row                   : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Auto_Gcode_Form_Div                   : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Auto_Gcode_File_Form                  : aliased Gnoga.Gui.Element.Form.Form_Type;
      Auto_Gcode_File_Form_Entry            : aliased Gnoga.Gui.Element.Form.Selection_Type;
      Auto_Gcode_File_Form_Submit_Button    : aliased Gnoga.Gui.Element.Form.Submit_Button_Type;
      Auto_Gcode_Refresh_Form               : aliased Gnoga.Gui.Element.Form.Form_Type;
      Auto_Gcode_Refresh_Form_Entry         : aliased Gnoga.Gui.Element.Form.Selection_Type;
      Auto_Gcode_Refresh_Form_Submit_Button : aliased Gnoga.Gui.Element.Form.Submit_Button_Type;

      Shutdown_Table  : aliased Gnoga.Gui.Element.Table.Table_Type;
      Shutdown_Row    : aliased Gnoga.Gui.Element.Table.Table_Row_Type;
      Shutdown_Div    : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Shutdown_Form   : aliased Gnoga.Gui.Element.Form.Form_Type;
      Shutdown_Button : aliased Gnoga.Gui.Element.Form.Submit_Button_Type;
      Restart_Button  : aliased Gnoga.Gui.Element.Form.Submit_Button_Type;
   end record;

   type App_Access is access all App_Data;

   task type Status_Updater is
      entry Start (In_App : App_Access);
      entry Stop;
   end Status_Updater;

end Prunt.GUI.GUI;
