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
with Gnoga.Gui.Element;
with Prunt.Config;
with UXStrings;             use UXStrings;
with Gnoga.Types;
with Prunt.GUI.Cards_Table; use Prunt.GUI.Cards_Table;
with Prunt.GUI.Discrete_Inputs;
with Prunt.GUI.Numeric_Discrete_Inputs;
with Prunt.GUI.Numeric_Floating_Inputs;
with Prunt.GUI.Parameter_Rows;
with Prunt.TMC_Types;
with Prunt.TMC_Types.TMC2240;

private generic
   with package My_Config is new Prunt.Config (<>);
   with procedure Log_And_Switch_Tab (Object : Gnoga.Types.Pointer_to_Connection_Data_Class; Message : String);
package Prunt.GUI.Config_Editor is

   package Basic_Inputs is

      package Length_Inputs is new Numeric_Floating_Inputs (Length);
      use Length_Inputs;
      package Length_Rows is new Parameter_Rows (Length, "mm", Length_Inputs.Numeric_Input);

      package Time_Inputs is new Numeric_Floating_Inputs (Time);
      use Time_Inputs;
      package Time_Rows is new Parameter_Rows (Time, "s", Time_Inputs.Numeric_Input);

      package Temperature_Inputs is new Numeric_Floating_Inputs (Temperature);
      use Temperature_Inputs;
      package Temperature_Rows is new Parameter_Rows (Temperature, "C", Temperature_Inputs.Numeric_Input);

      package Dimensionless_Inputs is new Numeric_Floating_Inputs (Dimensionless);
      use Dimensionless_Inputs;
      package Dimensionless_Rows is new Parameter_Rows (Dimensionless, "", Dimensionless_Inputs.Numeric_Input);

      package PWM_Scale_Inputs is new Numeric_Floating_Inputs (PWM_Scale);
      use PWM_Scale_Inputs;
      package PWM_Scale_Rows is new Parameter_Rows (PWM_Scale, "", PWM_Scale_Inputs.Numeric_Input);

      package Voltage_Inputs is new Numeric_Floating_Inputs (Voltage);
      use Voltage_Inputs;
      package Voltage_Rows is new Parameter_Rows (Voltage, "V", Voltage_Inputs.Numeric_Input);

      package Current_Inputs is new Numeric_Floating_Inputs (Current);
      use Current_Inputs;
      package Current_Rows is new Parameter_Rows (Current, "A", Current_Inputs.Numeric_Input);

      package Velocity_Inputs is new Numeric_Floating_Inputs (Velocity);
      use Velocity_Inputs;
      package Velocity_Rows is new Parameter_Rows (Velocity, "mm/s", Velocity_Inputs.Numeric_Input);

      package Acceleration_Inputs is new Numeric_Floating_Inputs (Acceleration);
      use Acceleration_Inputs;
      package Acceleration_Rows is new Parameter_Rows (Acceleration, "mm/s^2", Acceleration_Inputs.Numeric_Input);

      package Jerk_Inputs is new Numeric_Floating_Inputs (Jerk);
      use Jerk_Inputs;
      package Jerk_Rows is new Parameter_Rows (Jerk, "mm/s^3", Jerk_Inputs.Numeric_Input);

      package Snap_Inputs is new Numeric_Floating_Inputs (Snap);
      use Snap_Inputs;
      package Snap_Rows is new Parameter_Rows (Snap, "mm/s^4", Snap_Inputs.Numeric_Input);

      package Crackle_Inputs is new Numeric_Floating_Inputs (Crackle);
      use Crackle_Inputs;
      package Crackle_Rows is new Parameter_Rows (Crackle, "mm/s^5", Crackle_Inputs.Numeric_Input);

      package Resistance_Inputs is new Numeric_Floating_Inputs (Resistance);
      use Resistance_Inputs;
      package Resistance_Rows is new Parameter_Rows (Resistance, "ohm", Resistance_Inputs.Numeric_Input);

      type Path_String_Input is new Gnoga.Gui.Element.Form.Text_Type with null record;
      function Get (Input : Path_String_Input) return My_Config.Path_Strings.Bounded_String;
      procedure Set (Input : in out Path_String_Input; Value : My_Config.Path_Strings.Bounded_String);
      procedure Create_For_Parameter_Row
        (Element : in out Path_String_Input;
         Parent  : in out Gnoga.Gui.Element.Element_Type'Class;
         Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class);
      package Path_String_Rows is new Parameter_Rows (My_Config.Path_Strings.Bounded_String, "", Path_String_Input);

      type Boolean_Input is new Gnoga.Gui.Element.Form.Check_Box_Type with null record;
      function Get (Input : Boolean_Input) return Boolean;
      procedure Set (Input : in out Boolean_Input; Value : Boolean);
      procedure Create_For_Parameter_Row
        (Element : in out Boolean_Input;
         Parent  : in out Gnoga.Gui.Element.Element_Type'Class;
         Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class);
      package Boolean_Rows is new Parameter_Rows (Boolean, "", Boolean_Input);

      package Stepper_Name_Inputs is new Discrete_Inputs (My_Config.Stepper_Name);
      use Stepper_Name_Inputs;
      package Stepper_Name_Rows is new Parameter_Rows (My_Config.Stepper_Name, "", Stepper_Name_Inputs.Discrete_Input);

      package Heater_Name_Inputs is new Discrete_Inputs (My_Config.Heater_Name);
      use Heater_Name_Inputs;
      package Heater_Name_Rows is new Parameter_Rows (My_Config.Heater_Name, "", Heater_Name_Inputs.Discrete_Input);

      package Thermistor_Name_Inputs is new Discrete_Inputs (My_Config.Thermistor_Name);
      use Thermistor_Name_Inputs;
      package Thermistor_Name_Rows is new Parameter_Rows
        (My_Config.Thermistor_Name, "", Thermistor_Name_Inputs.Discrete_Input);

      package Fan_Name_Inputs is new Discrete_Inputs (My_Config.Fan_Name);
      use Fan_Name_Inputs;
      package Fan_Name_Rows is new Parameter_Rows (My_Config.Fan_Name, "", Fan_Name_Inputs.Discrete_Input);

      package Input_Switch_Name_Inputs is new Discrete_Inputs (My_Config.Input_Switch_Name);
      use Input_Switch_Name_Inputs;
      package Input_Switch_Name_Rows is new Parameter_Rows
        (My_Config.Input_Switch_Name, "", Input_Switch_Name_Inputs.Discrete_Input);

      package TMC_Unsigned_1_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_1);
      use TMC_Unsigned_1_Inputs;
      package TMC_Unsigned_1_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_1, "", TMC_Unsigned_1_Inputs.Numeric_Input);

      package TMC_Unsigned_2_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_2);
      use TMC_Unsigned_2_Inputs;
      package TMC_Unsigned_2_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_2, "", TMC_Unsigned_2_Inputs.Numeric_Input);

      package TMC_Unsigned_3_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_3);
      use TMC_Unsigned_3_Inputs;
      package TMC_Unsigned_3_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_3, "", TMC_Unsigned_3_Inputs.Numeric_Input);

      package TMC_Unsigned_4_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_4);
      use TMC_Unsigned_4_Inputs;
      package TMC_Unsigned_4_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_4, "", TMC_Unsigned_4_Inputs.Numeric_Input);

      package TMC_Unsigned_5_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_5);
      use TMC_Unsigned_5_Inputs;
      package TMC_Unsigned_5_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_5, "", TMC_Unsigned_5_Inputs.Numeric_Input);

      package TMC_Unsigned_6_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_6);
      use TMC_Unsigned_6_Inputs;
      package TMC_Unsigned_6_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_6, "", TMC_Unsigned_6_Inputs.Numeric_Input);

      package TMC_Unsigned_7_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_7);
      use TMC_Unsigned_7_Inputs;
      package TMC_Unsigned_7_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_7, "", TMC_Unsigned_7_Inputs.Numeric_Input);

      package TMC_Unsigned_8_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_8);
      use TMC_Unsigned_8_Inputs;
      package TMC_Unsigned_8_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_8, "", TMC_Unsigned_8_Inputs.Numeric_Input);

      package TMC_Unsigned_9_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_9);
      use TMC_Unsigned_9_Inputs;
      package TMC_Unsigned_9_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_9, "", TMC_Unsigned_9_Inputs.Numeric_Input);

      package TMC_Unsigned_10_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_10);
      use TMC_Unsigned_10_Inputs;
      package TMC_Unsigned_10_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_10, "", TMC_Unsigned_10_Inputs.Numeric_Input);

      package TMC_Unsigned_11_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_11);
      use TMC_Unsigned_11_Inputs;
      package TMC_Unsigned_11_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_11, "", TMC_Unsigned_11_Inputs.Numeric_Input);

      package TMC_Unsigned_12_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_12);
      use TMC_Unsigned_12_Inputs;
      package TMC_Unsigned_12_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_12, "", TMC_Unsigned_12_Inputs.Numeric_Input);

      package TMC_Unsigned_13_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_13);
      use TMC_Unsigned_13_Inputs;
      package TMC_Unsigned_13_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_13, "", TMC_Unsigned_13_Inputs.Numeric_Input);

      package TMC_Unsigned_14_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_14);
      use TMC_Unsigned_14_Inputs;
      package TMC_Unsigned_14_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_14, "", TMC_Unsigned_14_Inputs.Numeric_Input);

      package TMC_Unsigned_15_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_15);
      use TMC_Unsigned_15_Inputs;
      package TMC_Unsigned_15_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_15, "", TMC_Unsigned_15_Inputs.Numeric_Input);

      package TMC_Unsigned_16_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_16);
      use TMC_Unsigned_16_Inputs;
      package TMC_Unsigned_16_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_16, "", TMC_Unsigned_16_Inputs.Numeric_Input);

      package TMC_Unsigned_17_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_17);
      use TMC_Unsigned_17_Inputs;
      package TMC_Unsigned_17_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_17, "", TMC_Unsigned_17_Inputs.Numeric_Input);

      package TMC_Unsigned_18_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_18);
      use TMC_Unsigned_18_Inputs;
      package TMC_Unsigned_18_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_18, "", TMC_Unsigned_18_Inputs.Numeric_Input);

      package TMC_Unsigned_19_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_19);
      use TMC_Unsigned_19_Inputs;
      package TMC_Unsigned_19_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_19, "", TMC_Unsigned_19_Inputs.Numeric_Input);

      package TMC_Unsigned_20_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_20);
      use TMC_Unsigned_20_Inputs;
      package TMC_Unsigned_20_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_20, "", TMC_Unsigned_20_Inputs.Numeric_Input);

      package TMC_Unsigned_21_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_21);
      use TMC_Unsigned_21_Inputs;
      package TMC_Unsigned_21_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_21, "", TMC_Unsigned_21_Inputs.Numeric_Input);

      package TMC_Unsigned_22_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_22);
      use TMC_Unsigned_22_Inputs;
      package TMC_Unsigned_22_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_22, "", TMC_Unsigned_22_Inputs.Numeric_Input);

      package TMC_Unsigned_23_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_23);
      use TMC_Unsigned_23_Inputs;
      package TMC_Unsigned_23_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_23, "", TMC_Unsigned_23_Inputs.Numeric_Input);

      package TMC_Unsigned_24_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_24);
      use TMC_Unsigned_24_Inputs;
      package TMC_Unsigned_24_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_24, "", TMC_Unsigned_24_Inputs.Numeric_Input);

      package TMC_Unsigned_25_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_25);
      use TMC_Unsigned_25_Inputs;
      package TMC_Unsigned_25_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_25, "", TMC_Unsigned_25_Inputs.Numeric_Input);

      package TMC_Unsigned_26_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_26);
      use TMC_Unsigned_26_Inputs;
      package TMC_Unsigned_26_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_26, "", TMC_Unsigned_26_Inputs.Numeric_Input);

      package TMC_Unsigned_27_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_27);
      use TMC_Unsigned_27_Inputs;
      package TMC_Unsigned_27_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_27, "", TMC_Unsigned_27_Inputs.Numeric_Input);

      package TMC_Unsigned_28_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_28);
      use TMC_Unsigned_28_Inputs;
      package TMC_Unsigned_28_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_28, "", TMC_Unsigned_28_Inputs.Numeric_Input);

      package TMC_Unsigned_29_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_29);
      use TMC_Unsigned_29_Inputs;
      package TMC_Unsigned_29_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_29, "", TMC_Unsigned_29_Inputs.Numeric_Input);

      package TMC_Unsigned_30_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_30);
      use TMC_Unsigned_30_Inputs;
      package TMC_Unsigned_30_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_30, "", TMC_Unsigned_30_Inputs.Numeric_Input);

      package TMC_Unsigned_31_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_31);
      use TMC_Unsigned_31_Inputs;
      package TMC_Unsigned_31_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_31, "", TMC_Unsigned_31_Inputs.Numeric_Input);

      package TMC_Unsigned_32_Inputs is new Numeric_Discrete_Inputs (TMC_Types.Unsigned_32);
      use TMC_Unsigned_32_Inputs;
      package TMC_Unsigned_32_Rows is new Parameter_Rows
        (TMC_Types.Unsigned_32, "", TMC_Unsigned_32_Inputs.Numeric_Input);

      package TMC2240_Slope_Control_Inputs is new Discrete_Inputs (TMC_Types.TMC2240.Slope_Control_Type);
      use TMC2240_Slope_Control_Inputs;
      package TMC2240_Slope_Control_Rows is new Parameter_Rows
        (TMC_Types.TMC2240.Slope_Control_Type, "", TMC2240_Slope_Control_Inputs.Discrete_Input);

      package TMC2240_Microstep_Resolution_Inputs is new Discrete_Inputs (TMC_Types.TMC2240.Microstep_Resolution_Type);
      use TMC2240_Microstep_Resolution_Inputs;
      package TMC2240_Microstep_Resolution_Rows is new Parameter_Rows
        (TMC_Types.TMC2240.Microstep_Resolution_Type, "", TMC2240_Microstep_Resolution_Inputs.Discrete_Input);

      package TMC2240_Freewheel_Inputs is new Discrete_Inputs (TMC_Types.TMC2240.Freewheel_Type);
      use TMC2240_Freewheel_Inputs;
      package TMC2240_Freewheel_Rows is new Parameter_Rows
        (TMC_Types.TMC2240.Freewheel_Type, "", TMC2240_Freewheel_Inputs.Discrete_Input);

   end Basic_Inputs;

   package Grouped_Element_Widgets is

      package Plain_Widgets is

         type Position_Widget is new Gnoga.Gui.Element.Table.Table_Type with private;
         procedure Create_For_Parameter_Row
           (Widget : in out Position_Widget;
            Parent : in out Gnoga.Gui.Element.Element_Type'Class;
            Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class);
         function Get (Widget : Position_Widget) return Prunt.Position;
         procedure Set (Widget : in out Position_Widget; Pos : Prunt.Position);

         type Position_Scale_Widget is new Gnoga.Gui.Element.Table.Table_Type with private;
         procedure Create_For_Parameter_Row
           (Widget : in out Position_Scale_Widget;
            Parent : in out Gnoga.Gui.Element.Element_Type'Class;
            Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class);
         function Get (Widget : Position_Scale_Widget) return Position_Scale;
         procedure Set (Widget : in out Position_Scale_Widget; Scale : Position_Scale);

         type Axial_Velocities_Widget is new Gnoga.Gui.Element.Table.Table_Type with private;
         procedure Create_For_Parameter_Row
           (Widget : in out Axial_Velocities_Widget;
            Parent : in out Gnoga.Gui.Element.Element_Type'Class;
            Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class);
         function Get (Widget : Axial_Velocities_Widget) return Axial_Velocities;
         procedure Set (Widget : in out Axial_Velocities_Widget; Vels : Axial_Velocities);

         type Attached_Steppers_Widget is new Gnoga.Gui.Element.Table.Table_Type with private;
         procedure Create_For_Parameter_Row
           (Widget : in out Attached_Steppers_Widget;
            Parent : in out Gnoga.Gui.Element.Element_Type'Class;
            Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class);
         function Get (Widget : Attached_Steppers_Widget) return My_Config.Attached_Steppers;
         procedure Set (Widget : in out Attached_Steppers_Widget; Steppers : My_Config.Attached_Steppers);

      private

         type Position_Rows is array (Axis_Name) of Basic_Inputs.Length_Rows.Parameter_Row;
         type Position_Widget is new Gnoga.Gui.Element.Table.Table_Type with record
            Rows : Position_Rows;
         end record;

         type Position_Scale_Rows is array (Axis_Name) of Basic_Inputs.Dimensionless_Rows.Parameter_Row;
         type Position_Scale_Widget is new Gnoga.Gui.Element.Table.Table_Type with record
            Rows : Position_Scale_Rows;
         end record;

         type Axial_Velocities_Rows is array (Axis_Name) of Basic_Inputs.Velocity_Rows.Parameter_Row;
         type Axial_Velocities_Widget is new Gnoga.Gui.Element.Table.Table_Type with record
            Rows : Axial_Velocities_Rows;
         end record;

         type Stepper_Rows is array (My_Config.Stepper_Name) of Basic_Inputs.Boolean_Rows.Parameter_Row;
         type Attached_Steppers_Widget is new Gnoga.Gui.Element.Table.Table_Type with record
            Rows : Stepper_Rows;
         end record;

      end Plain_Widgets;

      use Plain_Widgets;

      package Position_Widget_Rows is new Parameter_Rows (Prunt.Position, "", Position_Widget);
      package Position_Scale_Widget_Rows is new Parameter_Rows (Position_Scale, "", Position_Scale_Widget);
      package Axial_Velocities_Widget_Rows is new Parameter_Rows (Axial_Velocities, "", Axial_Velocities_Widget);
      package Attached_Steppers_Widget_Rows is new Parameter_Rows
        (My_Config.Attached_Steppers, "", Attached_Steppers_Widget);

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
         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;
         Enabled      : Basic_Inputs.Boolean_Rows.Parameter_Row;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Stepper_Widget is new Parent_Type with record
         Stepper : My_Config.Stepper_Name;

         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;
         Enabled      : Basic_Inputs.Boolean_Rows.Parameter_Row;
         Mm_Per_Step  : Basic_Inputs.Length_Rows.Parameter_Row;

         --  TMC2240
         Output_Current       : Basic_Inputs.Current_Rows.Parameter_Row;
         Slope_Control        : Basic_Inputs.TMC2240_Slope_Control_Rows.Parameter_Row;
         I_Hold               : Basic_Inputs.TMC_Unsigned_5_Rows.Parameter_Row;
         I_Run                : Basic_Inputs.TMC_Unsigned_5_Rows.Parameter_Row;
         I_Hold_Delay         : Basic_Inputs.TMC_Unsigned_4_Rows.Parameter_Row;
         I_Run_Delay          : Basic_Inputs.TMC_Unsigned_4_Rows.Parameter_Row;
         T_Power_Down         : Basic_Inputs.TMC_Unsigned_8_Rows.Parameter_Row;
         T_PWM_Thrs           : Basic_Inputs.TMC_Unsigned_20_Rows.Parameter_Row;
         T_Cool_Thrs          : Basic_Inputs.TMC_Unsigned_20_Rows.Parameter_Row;
         T_High               : Basic_Inputs.TMC_Unsigned_20_Rows.Parameter_Row;
         TOFF                 : Basic_Inputs.TMC_Unsigned_4_Rows.Parameter_Row;
         HSTRT_TFD210         : Basic_Inputs.TMC_Unsigned_3_Rows.Parameter_Row;
         HEND_OFFSET          : Basic_Inputs.TMC_Unsigned_4_Rows.Parameter_Row;
         FD3                  : Basic_Inputs.TMC_Unsigned_1_Rows.Parameter_Row;
         DISFDCC              : Basic_Inputs.Boolean_Rows.Parameter_Row;
         CHM                  : Basic_Inputs.Boolean_Rows.Parameter_Row;
         VHIGHFS              : Basic_Inputs.Boolean_Rows.Parameter_Row;
         VHIGHCHM             : Basic_Inputs.Boolean_Rows.Parameter_Row;
         TPFD                 : Basic_Inputs.TMC_Unsigned_4_Rows.Parameter_Row;
         Microstep_Resolution : Basic_Inputs.TMC2240_Microstep_Resolution_Rows.Parameter_Row;
         PWM_OFS              : Basic_Inputs.TMC_Unsigned_8_Rows.Parameter_Row;
         PWM_Grad             : Basic_Inputs.TMC_Unsigned_8_Rows.Parameter_Row;
         PWM_Freq             : Basic_Inputs.TMC_Unsigned_2_Rows.Parameter_Row;
         PWM_Auto_Scale       : Basic_Inputs.Boolean_Rows.Parameter_Row;
         PWM_Auto_Grad        : Basic_Inputs.Boolean_Rows.Parameter_Row;
         Freewheel            : Basic_Inputs.TMC2240_Freewheel_Rows.Parameter_Row;
         PWM_Meas_SD_Enable   : Basic_Inputs.Boolean_Rows.Parameter_Row;
         PWM_Dis_Reg_Stst     : Basic_Inputs.Boolean_Rows.Parameter_Row;
         PWM_Reg              : Basic_Inputs.TMC_Unsigned_4_Rows.Parameter_Row;
         PWM_Lim              : Basic_Inputs.TMC_Unsigned_4_Rows.Parameter_Row;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Kinematics_Widget is new Parent_Type with record
         Widget_Table            : Gnoga.Gui.Element.Table.Table_Type;
         Lower_Pos_Limit         : Grouped_Element_Widgets.Position_Widget_Rows.Parameter_Row;
         Upper_Pos_Limit         : Grouped_Element_Widgets.Position_Widget_Rows.Parameter_Row;
         Tangential_Velocity_Max : Basic_Inputs.Velocity_Rows.Parameter_Row;
         Acceleration_Max        : Basic_Inputs.Acceleration_Rows.Parameter_Row;
         Jerk_Max                : Basic_Inputs.Jerk_Rows.Parameter_Row;
         Snap_Max                : Basic_Inputs.Snap_Rows.Parameter_Row;
         Crackle_Max             : Basic_Inputs.Crackle_Rows.Parameter_Row;
         Axial_Velocity_Maxes    : Grouped_Element_Widgets.Axial_Velocities_Widget_Rows.Parameter_Row;
         Ignore_E_In_XYZE        : Basic_Inputs.Boolean_Rows.Parameter_Row;
         Shift_Blended_Corners   : Basic_Inputs.Boolean_Rows.Parameter_Row;
         Pressure_Advance_Time   : Basic_Inputs.Time_Rows.Parameter_Row;
         Chord_Error_Max         : Basic_Inputs.Length_Rows.Parameter_Row;
         Axial_Scaler            : Grouped_Element_Widgets.Position_Scale_Widget_Rows.Parameter_Row;
         Z_Steppers              : Grouped_Element_Widgets.Attached_Steppers_Widget_Rows.Parameter_Row;
         E_Steppers              : Grouped_Element_Widgets.Attached_Steppers_Widget_Rows.Parameter_Row;

         Kind_Table : Cards_Table_Type;

         Cartesian_Table : aliased Gnoga.Gui.Element.Table.Table_Type;
         X_Steppers      : Grouped_Element_Widgets.Attached_Steppers_Widget_Rows.Parameter_Row;
         Y_Steppers      : Grouped_Element_Widgets.Attached_Steppers_Widget_Rows.Parameter_Row;

         Core_XY_Table : aliased Gnoga.Gui.Element.Table.Table_Type;
         A_Steppers    : Grouped_Element_Widgets.Attached_Steppers_Widget_Rows.Parameter_Row;
         B_Steppers    : Grouped_Element_Widgets.Attached_Steppers_Widget_Rows.Parameter_Row;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Input_Switch_Widget is new Parent_Type with record
         Input_Switch : My_Config.Input_Switch_Name;

         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;
         Enabled      : Basic_Inputs.Boolean_Rows.Parameter_Row;
         Hit_On_High  : Basic_Inputs.Boolean_Rows.Parameter_Row;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Homing_Widget is new Parent_Type with record
         Axis : Axis_Name;

         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;

         Kind_Table : Cards_Table_Type;

         Double_Tap_Table       : aliased Gnoga.Gui.Element.Table.Table_Type;
         Switch                 : Basic_Inputs.Input_Switch_Name_Rows.Parameter_Row;
         First_Move_Distance    : Basic_Inputs.Length_Rows.Parameter_Row;
         Back_Off_Move_Distance : Basic_Inputs.Length_Rows.Parameter_Row;
         Second_Move_Distance   : Basic_Inputs.Length_Rows.Parameter_Row;
         Switch_Position        : Basic_Inputs.Length_Rows.Parameter_Row;

         Set_To_Value_Table : aliased Gnoga.Gui.Element.Table.Table_Type;
         Value              : Basic_Inputs.Length_Rows.Parameter_Row;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Extruder_Widget is new Parent_Type with record
         Widget_Table                   : Gnoga.Gui.Element.Table.Table_Type;
         Nozzle_Diameter                : Basic_Inputs.Length_Rows.Parameter_Row;
         Filament_Diameter              : Basic_Inputs.Length_Rows.Parameter_Row;
         Starting_Pressure_Advance_Time : Basic_Inputs.Time_Rows.Parameter_Row;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Thermistor_Widget is new Parent_Type with record
         Thermistor : My_Config.Thermistor_Name;

         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;

         Minimum_Temperature : Basic_Inputs.Temperature_Rows.Parameter_Row;
         Maximum_Temperature : Basic_Inputs.Temperature_Rows.Parameter_Row;

         Kind_Table : Cards_Table_Type;

         Disabled_Table : aliased Gnoga.Gui.Element.Table.Table_Type;

         Steinhart_Hart_Table : aliased Gnoga.Gui.Element.Table.Table_Type;
         SH_A                 : Basic_Inputs.Dimensionless_Rows.Parameter_Row;
         SH_B                 : Basic_Inputs.Dimensionless_Rows.Parameter_Row;
         SH_C                 : Basic_Inputs.Dimensionless_Rows.Parameter_Row;

         Callendar_Van_Dusen_Table : aliased Gnoga.Gui.Element.Table.Table_Type;
         CVD_R0                    : Basic_Inputs.Resistance_Rows.Parameter_Row;
         CVD_A                     : Basic_Inputs.Dimensionless_Rows.Parameter_Row;
         CVD_B                     : Basic_Inputs.Dimensionless_Rows.Parameter_Row;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Heater_Widget is new Parent_Type with record
         Heater : My_Config.Heater_Name;

         Widget_Table               : Gnoga.Gui.Element.Table.Table_Type;
         Thermistor                 : Basic_Inputs.Thermistor_Name_Rows.Parameter_Row;
         Check_Max_Cumulative_Error : Basic_Inputs.Temperature_Rows.Parameter_Row;
         Check_Gain_Time            : Basic_Inputs.Time_Rows.Parameter_Row;
         Check_Minimum_Gain         : Basic_Inputs.Temperature_Rows.Parameter_Row;
         Check_Hysteresis           : Basic_Inputs.Temperature_Rows.Parameter_Row;

         Kind_Table : Cards_Table_Type;

         Disabled_Table : aliased Gnoga.Gui.Element.Table.Table_Type;

         PID_Table          : aliased Gnoga.Gui.Element.Table.Table_Type;
         Proportional_Scale : Basic_Inputs.Dimensionless_Rows.Parameter_Row;
         Integral_Scale     : Basic_Inputs.Dimensionless_Rows.Parameter_Row;
         Derivative_Scale   : Basic_Inputs.Dimensionless_Rows.Parameter_Row;

         Bang_Bang_Table      : aliased Gnoga.Gui.Element.Table.Table_Type;
         Bang_Bang_Hysteresis : Basic_Inputs.Temperature_Rows.Parameter_Row;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Bed_Mesh_Widget is new Parent_Type with record
         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;

         Kind_Table : Cards_Table_Type;

         No_Mesh_Table : aliased Gnoga.Gui.Element.Table.Table_Type;

         Beacon_Table         : aliased Gnoga.Gui.Element.Table.Table_Type;
         Serial_Port_Path     : Basic_Inputs.Path_String_Rows.Parameter_Row;
         X_Offset             : Basic_Inputs.Length_Rows.Parameter_Row;
         Y_Offset             : Basic_Inputs.Length_Rows.Parameter_Row;
         Calibration_Floor    : Basic_Inputs.Length_Rows.Parameter_Row;
         Calibration_Ceiling  : Basic_Inputs.Length_Rows.Parameter_Row;
         Calibration_Feedrate : Basic_Inputs.Velocity_Rows.Parameter_Row;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type Fan_Widget is new Parent_Type with record
         Fan : My_Config.Fan_Name;

         Widget_Table : Gnoga.Gui.Element.Table.Table_Type;

         Invert_Output : Basic_Inputs.Boolean_Rows.Parameter_Row;

         Kind_Table : Cards_Table_Type;

         Dynamic_PWM_Table : aliased Gnoga.Gui.Element.Table.Table_Type;
         Disable_Below_PWM : Basic_Inputs.PWM_Scale_Rows.Parameter_Row;
         Max_PWM           : Basic_Inputs.PWM_Scale_Rows.Parameter_Row;

         Always_On_Table : aliased Gnoga.Gui.Element.Table.Table_Type;
         Always_On_PWM   : Basic_Inputs.PWM_Scale_Rows.Parameter_Row;

         Submit_Button : Gnoga.Gui.Element.Form.Submit_Button_Type;
      end record;

      type G_Code_Assignment_Widget is new Parent_Type with record
         Widget_Table  : Gnoga.Gui.Element.Table.Table_Type;
         Bed_Heater    : Basic_Inputs.Heater_Name_Rows.Parameter_Row;
         --  Chamber_Heater : Basic_Inputs.Heater_Name_Rows.Parameter_Row;
         Hotend_Heater : Basic_Inputs.Heater_Name_Rows.Parameter_Row;

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
