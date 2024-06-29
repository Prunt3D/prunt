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
with Prunt.Config;
with UXStrings;             use UXStrings;
with Gnoga.Types;
with Prunt.GUI.Cards_Table; use Prunt.GUI.Cards_Table;
with Prunt.GUI.Discrete_Inputs;

private generic
   with package My_Config is new Prunt.Config (<>);
   with procedure Log_And_Switch_Tab (Object : Gnoga.Types.Pointer_to_Connection_Data_Class; Message : UXString);
package Prunt.GUI.Config_Editor is

   package Basic_Inputs is

      type Length_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Length_Input) return Length;
      procedure Set (Input : in out Length_Input; Value : Length);

      type Time_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Time_Input) return Time;
      procedure Set (Input : in out Time_Input; Value : Time);

      type Temperature_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Temperature_Input) return Temperature;
      procedure Set (Input : in out Temperature_Input; Value : Temperature);

      type Dimensionless_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Dimensionless_Input) return Dimensionless;
      procedure Set (Input : in out Dimensionless_Input; Value : Dimensionless);

      type PWM_Scale_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : PWM_Scale_Input) return PWM_Scale;
      procedure Set (Input : in out PWM_Scale_Input; Value : PWM_Scale);

      type Voltage_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Voltage_Input) return Voltage;
      procedure Set (Input : in out Voltage_Input; Value : Voltage);

      type Velocity_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Velocity_Input) return Velocity;
      procedure Set (Input : in out Velocity_Input; Value : Velocity);

      type Acceleration_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Acceleration_Input) return Acceleration;
      procedure Set (Input : in out Acceleration_Input; Value : Acceleration);

      type Jerk_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Jerk_Input) return Jerk;
      procedure Set (Input : in out Jerk_Input; Value : Jerk);

      type Snap_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Snap_Input) return Snap;
      procedure Set (Input : in out Snap_Input; Value : Snap);

      type Crackle_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Crackle_Input) return Crackle;
      procedure Set (Input : in out Crackle_Input; Value : Crackle);

      type Resistance_Input is new Gnoga.Gui.Element.Form.Number_Type with null record;
      function Get (Input : Resistance_Input) return Resistance;
      procedure Set (Input : in out Resistance_Input; Value : Resistance);

      type Path_String_Input is new Gnoga.Gui.Element.Form.Text_Type with null record;
      function Get (Input : Path_String_Input) return My_Config.Path_Strings.Bounded_String;
      procedure Set (Input : in out Path_String_Input; Value : My_Config.Path_Strings.Bounded_String);

      type Boolean_Input is new Gnoga.Gui.Element.Form.Check_Box_Type with null record;
      function Get (Input : Boolean_Input) return Boolean;
      procedure Set (Input : in out Boolean_Input; Value : Boolean);

      package Stepper_Name_Input is new Discrete_Inputs (My_Config.Stepper_Name);
      package Heater_Name_Input is new Discrete_Inputs (My_Config.Heater_Name);
      package Thermistor_Name_Input is new Discrete_Inputs (My_Config.Thermistor_Name);
      package Fan_Name_Input is new Discrete_Inputs (My_Config.Fan_Name);
      package Input_Switch_Name_Input is new Discrete_Inputs (My_Config.Input_Switch_Name);

   end Basic_Inputs;

   package Parameter_Rows is

      subtype Parent_Type is Gnoga.Gui.Element.Table.Table_Row_Type;

      type Parameter_Row is new Parent_Type with private;

      procedure Create
        (Row         : in out Parameter_Row;
         Parent      : in out Gnoga.Gui.Element.Element_Type'Class;
         Name        :        UXString;
         Description :        UXString;
         Data        : in out Gnoga.Gui.Element.Element_Type'Class;
         ID          :        UXString := "");

   private

      overriding procedure Create
        (Row : in out Parameter_Row; Parent : in out Gnoga.Gui.Element.Element_Type'Class; ID : UXString := "");

      type Parameter_Row is new Parent_Type with record
         Name        : Gnoga.Gui.Element.Common.DIV_Type;
         Description : Gnoga.Gui.Element.Common.DIV_Type;

         Name_Col        : Gnoga.Gui.Element.Table.Table_Column_Type;
         Description_Col : Gnoga.Gui.Element.Table.Table_Column_Type;
         Data_Col        : Gnoga.Gui.Element.Table.Table_Column_Type;
      end record;

   end Parameter_Rows;

   package Grouped_Element_Widgets is

      type Position_Widget is new Gnoga.Gui.Element.Table.Table_Type with private;

      procedure Create
        (Widget : in out Position_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "");

      function Get (Widget : Position_Widget) return Prunt.Position;
      procedure Set (Widget : in out Position_Widget; Pos : Prunt.Position);

      type Position_Scale_Widget is new Gnoga.Gui.Element.Table.Table_Type with private;

      procedure Create
        (Widget : in out Position_Scale_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "");

      function Get (Widget : Position_Scale_Widget) return Position_Scale;
      procedure Set (Widget : in out Position_Scale_Widget; Scale : Position_Scale);

      type Axial_Velocities_Widget is new Gnoga.Gui.Element.Table.Table_Type with private;

      procedure Create
        (Widget : in out Axial_Velocities_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "");

      function Get (Widget : Axial_Velocities_Widget) return Axial_Velocities;
      procedure Set (Widget : in out Axial_Velocities_Widget; Vels : Axial_Velocities);

      type Attached_Steppers_Widget is new Gnoga.Gui.Element.Table.Table_Type with private;

      procedure Create
        (Widget : in out Attached_Steppers_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "");

      function Get (Widget : Attached_Steppers_Widget) return My_Config.Attached_Steppers;
      procedure Set (Widget : in out Attached_Steppers_Widget; Steppers : My_Config.Attached_Steppers);

   private

      type Numeric_Row is new Gnoga.Gui.Element.Table.Table_Row_Type with record
         Name      : Gnoga.Gui.Element.Common.DIV_Type;
         Name_Col  : Gnoga.Gui.Element.Table.Table_Column_Type;
         Input_Col : Gnoga.Gui.Element.Table.Table_Column_Type;
         Input     : Gnoga.Gui.Element.Form.Number_Type;
      end record;

      procedure Create
        (Row    : in out Numeric_Row;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         Name   :        UXString;
         ID     :        UXString := "");

      type Check_Box_Row is new Gnoga.Gui.Element.Table.Table_Row_Type with record
         Name      : Gnoga.Gui.Element.Common.DIV_Type;
         Name_Col  : Gnoga.Gui.Element.Table.Table_Column_Type;
         Input_Col : Gnoga.Gui.Element.Table.Table_Column_Type;
         Input     : Gnoga.Gui.Element.Form.Check_Box_Type;
      end record;

      procedure Create
        (Row    : in out Check_Box_Row;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         Name   :        UXString;
         ID     :        UXString := "");

      type Position_Rows is array (Axis_Name) of Numeric_Row;
      type Position_Widget is new Gnoga.Gui.Element.Table.Table_Type with record
         Rows : Position_Rows;
      end record;

      type Position_Scale_Rows is array (Axis_Name) of Numeric_Row;
      type Position_Scale_Widget is new Gnoga.Gui.Element.Table.Table_Type with record
         Rows : Position_Scale_Rows;
      end record;

      type Axial_Velocities_Rows is array (Axis_Name) of Numeric_Row;
      type Axial_Velocities_Widget is new Gnoga.Gui.Element.Table.Table_Type with record
         Rows : Axial_Velocities_Rows;
      end record;

      type Stepper_Rows is array (My_Config.Stepper_Name) of Check_Box_Row;
      type Attached_Steppers_Widget is new Gnoga.Gui.Element.Table.Table_Type with record
         Rows : Stepper_Rows;
      end record;

   end Grouped_Element_Widgets;

   package Outer_Section_Widgets is

      subtype Parent_Type is Gnoga.Gui.Element.Form.Form_Type;

      type Outer_Section_Widget is abstract new Parent_Type with null record;

      procedure Read_Data (View : in out Outer_Section_Widget) is abstract;

      procedure Save_Data (View : in out Outer_Section_Widget; Image : out UXString) is abstract;

      procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   end Outer_Section_Widgets;

   package Section_Widgets is

      subtype Parent_Type is Outer_Section_Widgets.Outer_Section_Widget;

      type Prunt_Widget is new Parent_Type with private;

      procedure Create_Widget (View : in out Prunt_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class);

      type Stepper_Widget is new Parent_Type with private;

      procedure Create_Widget
        (View    : in out Stepper_Widget;
         Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
         Stepper :        My_Config.Stepper_Name);

      type Kinematics_Widget is new Parent_Type with private;

      procedure Create_Widget (View : in out Kinematics_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class);

      type Input_Switch_Widget is new Parent_Type with private;

      procedure Create_Widget
        (View         : in out Input_Switch_Widget;
         Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
         Input_Switch :        My_Config.Input_Switch_Name);

      type Homing_Widget is new Parent_Type with private;

      procedure Create_Widget
        (View : in out Homing_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Axis : Axis_Name);

      type Extruder_Widget is new Parent_Type with private;

      procedure Create_Widget (View : in out Extruder_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class);

      type Thermistor_Widget is new Parent_Type with private;

      procedure Create_Widget
        (View       : in out Thermistor_Widget;
         Parent     : in out Gnoga.Gui.Base.Base_Type'Class;
         Thermistor :        My_Config.Thermistor_Name);

      type Heater_Widget is new Parent_Type with private;

      procedure Create_Widget
        (View : in out Heater_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Heater : My_Config.Heater_Name);

      type Bed_Mesh_Widget is new Parent_Type with private;

      procedure Create_Widget (View : in out Bed_Mesh_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class);

      type Fan_Widget is new Parent_Type with private;

      procedure Create_Widget
        (View : in out Fan_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Fan : My_Config.Fan_Name);

      type G_Code_Assignment_Widget is new Parent_Type with private;

      procedure Create_Widget (View : in out G_Code_Assignment_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class);

   private

      type Prunt_Widget is new Parent_Type with record
         Widget_Table  : Gnoga.Gui.Element.Table.Table_Type;
         Enabled_Row   : Parameter_Rows.Parameter_Row;
         Enabled_Input : Basic_Inputs.Boolean_Input;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Stepper_Widget is new Parent_Type with record
         Stepper : My_Config.Stepper_Name;

         Widget_Table      : Gnoga.Gui.Element.Table.Table_Type;
         Enabled_Row       : Parameter_Rows.Parameter_Row;
         Enabled_Input     : Basic_Inputs.Boolean_Input;
         Mm_Per_Step_Row   : Parameter_Rows.Parameter_Row;
         Mm_Per_Step_Input : Basic_Inputs.Length_Input;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Kinematics_Widget is new Parent_Type with record
         Widget_Table                  : Gnoga.Gui.Element.Table.Table_Type;
         Lower_Pos_Limit_Row           : Parameter_Rows.Parameter_Row;
         Lower_Pos_Limit_Input         : Grouped_Element_Widgets.Position_Widget;
         Upper_Pos_Limit_Row           : Parameter_Rows.Parameter_Row;
         Upper_Pos_Limit_Input         : Grouped_Element_Widgets.Position_Widget;
         Tangential_Velocity_Max_Row   : Parameter_Rows.Parameter_Row;
         Tangential_Velocity_Max_Input : Basic_Inputs.Velocity_Input;
         Acceleration_Max_Row          : Parameter_Rows.Parameter_Row;
         Acceleration_Max_Input        : Basic_Inputs.Acceleration_Input;
         Jerk_Max_Row                  : Parameter_Rows.Parameter_Row;
         Jerk_Max_Input                : Basic_Inputs.Jerk_Input;
         Snap_Max_Row                  : Parameter_Rows.Parameter_Row;
         Snap_Max_Input                : Basic_Inputs.Snap_Input;
         Crackle_Max_Row               : Parameter_Rows.Parameter_Row;
         Crackle_Max_Input             : Basic_Inputs.Crackle_Input;
         Pop_Max_Row                   : Parameter_Rows.Parameter_Row;
         Pop_Max_Input                 : Basic_Inputs.Crackle_Input;
         Axial_Velocity_Maxes_Row      : Parameter_Rows.Parameter_Row;
         Axial_Velocity_Maxes_Input    : Grouped_Element_Widgets.Axial_Velocities_Widget;
         Ignore_E_In_XYZE_Row          : Parameter_Rows.Parameter_Row;
         Ignore_E_In_XYZE_Input        : Basic_Inputs.Boolean_Input;
         Shift_Blended_Corners_Row     : Parameter_Rows.Parameter_Row;
         Shift_Blended_Corners_Input   : Basic_Inputs.Boolean_Input;
         Pressure_Advance_Time_Row     : Parameter_Rows.Parameter_Row;
         Pressure_Advance_Time_Input   : Basic_Inputs.Time_Input;
         Chord_Error_Max_Row           : Parameter_Rows.Parameter_Row;
         Chord_Error_Max_Input         : Basic_Inputs.Length_Input;
         Higher_Order_Scaler_Row       : Parameter_Rows.Parameter_Row;
         Higher_Order_Scaler_Input     : Grouped_Element_Widgets.Position_Scale_Widget;
         Z_Steppers_Row                : Parameter_Rows.Parameter_Row;
         Z_Steppers_Input              : Grouped_Element_Widgets.Attached_Steppers_Widget;
         E_Steppers_Row                : Parameter_Rows.Parameter_Row;
         E_Steppers_Input              : Grouped_Element_Widgets.Attached_Steppers_Widget;

         Kind_Table : Cards_Table_Type;

         Cartesian_Table  : aliased Gnoga.Gui.Element.Table.Table_Type;
         X_Steppers_Row   : Parameter_Rows.Parameter_Row;
         X_Steppers_Input : Grouped_Element_Widgets.Attached_Steppers_Widget;
         Y_Steppers_Row   : Parameter_Rows.Parameter_Row;
         Y_Steppers_Input : Grouped_Element_Widgets.Attached_Steppers_Widget;

         Core_XY_Table    : aliased Gnoga.Gui.Element.Table.Table_Type;
         A_Steppers_Row   : Parameter_Rows.Parameter_Row;
         A_Steppers_Input : Grouped_Element_Widgets.Attached_Steppers_Widget;
         B_Steppers_Row   : Parameter_Rows.Parameter_Row;
         B_Steppers_Input : Grouped_Element_Widgets.Attached_Steppers_Widget;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Input_Switch_Widget is new Parent_Type with record
         Input_Switch : My_Config.Input_Switch_Name;

         Widget_Table      : Gnoga.Gui.Element.Table.Table_Type;
         Enabled_Row       : Parameter_Rows.Parameter_Row;
         Enabled_Input     : Basic_Inputs.Boolean_Input;
         Hit_On_High_Row   : Parameter_Rows.Parameter_Row;
         Hit_On_High_Input : Basic_Inputs.Boolean_Input;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Homing_Widget is new Parent_Type with record
         Axis : Axis_Name;

         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;

         Kind_Table : Cards_Table_Type;

         Double_Tap_Table             : aliased Gnoga.Gui.Element.Table.Table_Type;
         Switch_Row                   : Parameter_Rows.Parameter_Row;
         Switch_Input                 : Basic_Inputs.Input_Switch_Name_Input.Discrete_Input;
         First_Move_Distance_Row      : Parameter_Rows.Parameter_Row;
         First_Move_Distance_Input    : Basic_Inputs.Length_Input;
         Back_Off_Move_Distance_Row   : Parameter_Rows.Parameter_Row;
         Back_Off_Move_Distance_Input : Basic_Inputs.Length_Input;
         Second_Move_Distance_Row     : Parameter_Rows.Parameter_Row;
         Second_Move_Distance_Input   : Basic_Inputs.Length_Input;
         Switch_Position_Row          : Parameter_Rows.Parameter_Row;
         Switch_Position_Input        : Basic_Inputs.Length_Input;

         Set_To_Value_Table : aliased Gnoga.Gui.Element.Table.Table_Type;
         Value_Row          : Parameter_Rows.Parameter_Row;
         Value_Input        : Basic_Inputs.Length_Input;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Extruder_Widget is new Parent_Type with record
         Widget_Table                         : Gnoga.Gui.Element.Table.Table_Type;
         Nozzle_Diameter_Row                  : Parameter_Rows.Parameter_Row;
         Nozzle_Diameter_Input                : Basic_Inputs.Length_Input;
         Filament_Diameter_Row                : Parameter_Rows.Parameter_Row;
         Filament_Diameter_Input              : Basic_Inputs.Length_Input;
         Starting_Pressure_Advance_Time_Row   : Parameter_Rows.Parameter_Row;
         Starting_Pressure_Advance_Time_Input : Basic_Inputs.Time_Input;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Thermistor_Widget is new Parent_Type with record
         Thermistor : My_Config.Thermistor_Name;

         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;

         Minimum_Temperature_Row   : Parameter_Rows.Parameter_Row;
         Minimum_Temperature_Input : Basic_Inputs.Temperature_Input;
         Maximum_Temperature_Row   : Parameter_Rows.Parameter_Row;
         Maximum_Temperature_Input : Basic_Inputs.Temperature_Input;

         Kind_Table : Cards_Table_Type;

         Disabled_Table : aliased Gnoga.Gui.Element.Table.Table_Type;

         Steinhart_Hart_Table : aliased Gnoga.Gui.Element.Table.Table_Type;
         SH_A_Row             : Parameter_Rows.Parameter_Row;
         SH_A_Input           : Basic_Inputs.Dimensionless_Input;
         SH_B_Row             : Parameter_Rows.Parameter_Row;
         SH_B_Input           : Basic_Inputs.Dimensionless_Input;
         SH_C_Row             : Parameter_Rows.Parameter_Row;
         SH_C_Input           : Basic_Inputs.Dimensionless_Input;

         Callendar_Van_Dusen_Table : aliased Gnoga.Gui.Element.Table.Table_Type;
         CVD_R0_Row                : Parameter_Rows.Parameter_Row;
         CVD_R0_Input              : Basic_Inputs.Resistance_Input;
         CVD_A_Row                 : Parameter_Rows.Parameter_Row;
         CVD_A_Input               : Basic_Inputs.Dimensionless_Input;
         CVD_B_Row                 : Parameter_Rows.Parameter_Row;
         CVD_B_Input               : Basic_Inputs.Dimensionless_Input;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Heater_Widget is new Parent_Type with record
         Heater : My_Config.Heater_Name;

         Widget_Table               : Gnoga.Gui.Element.Table.Table_Type;
         Thermistor_Row             : Parameter_Rows.Parameter_Row;
         Thermistor_Input           : Basic_Inputs.Thermistor_Name_Input.Discrete_Input;
         Max_Cumulative_Error_Row   : Parameter_Rows.Parameter_Row;
         Max_Cumulative_Error_Input : Basic_Inputs.Temperature_Input;
         Check_Gain_Time_Row        : Parameter_Rows.Parameter_Row;
         Check_Gain_Time_Input      : Basic_Inputs.Time_Input;
         Check_Minimum_Gain_Row     : Parameter_Rows.Parameter_Row;
         Check_Minimum_Gain_Input   : Basic_Inputs.Temperature_Input;
         Hysteresis_Row             : Parameter_Rows.Parameter_Row;
         Hysteresis_Input           : Basic_Inputs.Temperature_Input;

         Kind_Table : Cards_Table_Type;

         Disabled_Table : aliased Gnoga.Gui.Element.Table.Table_Type;

         PID_Table                : aliased Gnoga.Gui.Element.Table.Table_Type;
         Proportional_Scale_Row   : Parameter_Rows.Parameter_Row;
         Proportional_Scale_Input : Basic_Inputs.Dimensionless_Input;
         Integral_Scale_Row       : Parameter_Rows.Parameter_Row;
         Integral_Scale_Input     : Basic_Inputs.Dimensionless_Input;
         Derivative_Scale_Row     : Parameter_Rows.Parameter_Row;
         Derivative_Scale_Input   : Basic_Inputs.Dimensionless_Input;

         Bang_Bang_Table : aliased Gnoga.Gui.Element.Table.Table_Type;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Bed_Mesh_Widget is new Parent_Type with record
         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;

         Kind_Table : Cards_Table_Type;

         No_Mesh_Table : aliased Gnoga.Gui.Element.Table.Table_Type;

         Beacon_Table               : aliased Gnoga.Gui.Element.Table.Table_Type;
         Serial_Port_Path_Row       : Parameter_Rows.Parameter_Row;
         Serial_Port_Path_Input     : Basic_Inputs.Path_String_Input;
         X_Offset_Row               : Parameter_Rows.Parameter_Row;
         X_Offset_Input             : Basic_Inputs.Length_Input;
         Y_Offset_Row               : Parameter_Rows.Parameter_Row;
         Y_Offset_Input             : Basic_Inputs.Length_Input;
         Calibration_Floor_Row      : Parameter_Rows.Parameter_Row;
         Calibration_Floor_Input    : Basic_Inputs.Length_Input;
         Calibration_Ceiling_Row    : Parameter_Rows.Parameter_Row;
         Calibration_Ceiling_Input  : Basic_Inputs.Length_Input;
         Calibration_Feedrate_Row   : Parameter_Rows.Parameter_Row;
         Calibration_Feedrate_Input : Basic_Inputs.Velocity_Input;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Fan_Widget is new Parent_Type with record
         Fan : My_Config.Fan_Name;

         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;

         Kind_Table : Cards_Table_Type;

         Disabled_Table : aliased Gnoga.Gui.Element.Table.Table_Type;

         Dynamic_PWM_Table       : aliased Gnoga.Gui.Element.Table.Table_Type;
         Disable_Below_PWM_Row   : Parameter_Rows.Parameter_Row;
         Disable_Below_PWM_Input : Basic_Inputs.PWM_Scale_Input;
         Max_PWM_Row             : Parameter_Rows.Parameter_Row;
         Max_PWM_Input           : Basic_Inputs.PWM_Scale_Input;

         Always_On_Table     : aliased Gnoga.Gui.Element.Table.Table_Type;
         Always_On_PWM_Row   : Parameter_Rows.Parameter_Row;
         Always_On_PWM_Input : Basic_Inputs.PWM_Scale_Input;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type G_Code_Assignment_Widget is new Parent_Type with record
         Widget_Table        : Gnoga.Gui.Element.Table.Table_Type;
         Bed_Heater_Row      : Parameter_Rows.Parameter_Row;
         Bed_Heater_Input    : Basic_Inputs.Heater_Name_Input.Discrete_Input;
         --  Chamber_Heater_Row   : Parameter_Rows.Parameter_Row;
         --  Chamber_Heater_Input : Basic_Inputs.Heater_Name_Input.Discrete_Input;
         Hotend_Heater_Row   : Parameter_Rows.Parameter_Row;
         Hotend_Heater_Input : Basic_Inputs.Heater_Name_Input.Discrete_Input;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      overriding procedure Read_Data (View : in out Prunt_Widget);
      overriding procedure Save_Data (View : in out Prunt_Widget; Image : out UXString);

      overriding procedure Read_Data (View : in out Stepper_Widget);
      overriding procedure Save_Data (View : in out Stepper_Widget; Image : out UXString);

      overriding procedure Read_Data (View : in out Kinematics_Widget);
      overriding procedure Save_Data (View : in out Kinematics_Widget; Image : out UXString);

      overriding procedure Read_Data (View : in out Input_Switch_Widget);
      overriding procedure Save_Data (View : in out Input_Switch_Widget; Image : out UXString);

      overriding procedure Read_Data (View : in out Homing_Widget);
      overriding procedure Save_Data (View : in out Homing_Widget; Image : out UXString);

      overriding procedure Read_Data (View : in out Extruder_Widget);
      overriding procedure Save_Data (View : in out Extruder_Widget; Image : out UXString);

      overriding procedure Read_Data (View : in out Thermistor_Widget);
      overriding procedure Save_Data (View : in out Thermistor_Widget; Image : out UXString);

      overriding procedure Read_Data (View : in out Heater_Widget);
      overriding procedure Save_Data (View : in out Heater_Widget; Image : out UXString);

      overriding procedure Read_Data (View : in out Bed_Mesh_Widget);
      overriding procedure Save_Data (View : in out Bed_Mesh_Widget; Image : out UXString);

      overriding procedure Read_Data (View : in out Fan_Widget);
      overriding procedure Save_Data (View : in out Fan_Widget; Image : out UXString);

      overriding procedure Read_Data (View : in out G_Code_Assignment_Widget);
      overriding procedure Save_Data (View : in out G_Code_Assignment_Widget; Image : out UXString);

   end Section_Widgets;

end Prunt.GUI.Config_Editor;
