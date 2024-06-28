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

with Ada.Exceptions;
with Ada.Strings;
with Gnoga.Gui.View;
use type Gnoga.Gui.View.Pointer_To_View_Base_Class;
with Prunt.Thermistors; use Prunt.Thermistors;

package body Prunt.GUI.Config_Editor is

   pragma Unsuppress (All_Checks);

   package body Basic_Inputs is

      function Get (Input : Length_Input) return Length is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8) * mm;
      end Get;

      procedure Set (Input : in out Length_Input; Value : Length) is
      begin
         Input.Value (DF_Image (Value / mm));
      end Set;

      function Get (Input : Time_Input) return Time is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8) * s;
      end Get;

      procedure Set (Input : in out Time_Input; Value : Time) is
      begin
         Input.Value (DF_Image (Value / s));
      end Set;

      function Get (Input : Temperature_Input) return Temperature is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8) * celcius;
      end Get;

      procedure Set (Input : in out Temperature_Input; Value : Temperature) is
      begin
         Input.Value (DF_Image (Value / celcius));
      end Set;

      function Get (Input : Dimensionless_Input) return Dimensionless is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8);
      end Get;

      procedure Set (Input : in out Dimensionless_Input; Value : Dimensionless) is
      begin
         Input.Value (DF_Image (Value));
      end Set;

      function Get (Input : PWM_Scale_Input) return PWM_Scale is
      begin
         return PWM_Scale'Value (Input.Value.To_UTF_8);
      end Get;

      procedure Set (Input : in out PWM_Scale_Input; Value : PWM_Scale) is
      begin
         Input.Value (DF_Image (Value));
      end Set;

      function Get (Input : Voltage_Input) return Voltage is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8) * volt;
      end Get;

      procedure Set (Input : in out Voltage_Input; Value : Voltage) is
      begin
         Input.Value (DF_Image (Value / volt));
      end Set;

      function Get (Input : Velocity_Input) return Velocity is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8) * mm / s;
      end Get;

      procedure Set (Input : in out Velocity_Input; Value : Velocity) is
      begin
         Input.Value (DF_Image (Value / (mm / s)));
      end Set;

      function Get (Input : Acceleration_Input) return Acceleration is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8) * mm / s**2;
      end Get;

      procedure Set (Input : in out Acceleration_Input; Value : Acceleration) is
      begin
         Input.Value (DF_Image (Value / (mm / s**2)));
      end Set;

      function Get (Input : Jerk_Input) return Jerk is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8) * mm / s**3;
      end Get;

      procedure Set (Input : in out Jerk_Input; Value : Jerk) is
      begin
         Input.Value (DF_Image (Value / (mm / s**3)));
      end Set;

      function Get (Input : Snap_Input) return Snap is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8) * mm / s**4;
      end Get;

      procedure Set (Input : in out Snap_Input; Value : Snap) is
      begin
         Input.Value (DF_Image (Value / (mm / s**4)));
      end Set;

      function Get (Input : Crackle_Input) return Crackle is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8) * mm / s**5;
      end Get;

      procedure Set (Input : in out Crackle_Input; Value : Crackle) is
      begin
         Input.Value (DF_Image (Value / (mm / s**5)));
      end Set;

      function Get (Input : Resistance_Input) return Resistance is
      begin
         return Dimensionless'Value (Input.Value.To_UTF_8) * ohm;
      end Get;

      procedure Set (Input : in out Resistance_Input; Value : Resistance) is
      begin
         Input.Value (DF_Image (Value / ohm));
      end Set;

      function Get (Input : Path_String_Input) return My_Config.Path_Strings.Bounded_String is
      begin
         return My_Config.Path_Strings.To_Bounded_String (Input.Value.To_UTF_8);
      end Get;

      procedure Set (Input : in out Path_String_Input; Value : My_Config.Path_Strings.Bounded_String) is
      begin
         Input.Value (Value => UXStrings.From_UTF_8 (My_Config.Path_Strings.To_String (Value)));
      end Set;

      function Get (Input : Boolean_Input) return Boolean is
      begin
         return Input.Checked;
      end Get;

      procedure Set (Input : in out Boolean_Input; Value : Boolean) is
      begin
         Input.Checked (Value);
      end Set;

   end Basic_Inputs;

   package body Parameter_Rows is

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
         Name        :        UXString;
         Description :        UXString;
         Data        : in out Gnoga.Gui.Element.Element_Type'Class;
         ID          :        UXString := "")
      is
      begin
         Row.Create (Parent, ID);
         Row.Name.Text (Name);
         Row.Description.Text (Description);
         Data.Place_Inside_Top_Of (Row.Data_Col);
      end Create;

   end Parameter_Rows;

   package body Grouped_Element_Widgets is

      procedure Create
        (Row    : in out Numeric_Row;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         Name   :        UXString;
         ID     :        UXString := "")
      is
      begin
         Row.Create (Parent, ID);
         Row.Name_Col.Create (Row);
         Row.Name.Create (Row.Name_Col);
         Row.Name.Text (Name);
         Row.Input_Col.Create (Row);
         Row.Input.Create (Form);
         Row.Input.Place_Inside_Top_Of (Row.Input_Col);
         Row.Style ("border", "1px solid black");
      end Create;

      procedure Create
        (Row    : in out Check_Box_Row;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         Name   :        UXString;
         ID     :        UXString := "")
      is
      begin
         Row.Create (Parent, ID);
         Row.Input_Col.Create (Row);
         Row.Input.Create (Form);
         Row.Input.Place_Inside_Top_Of (Row.Input_Col);
         Row.Name_Col.Create (Row);
         Row.Name.Create (Row.Name_Col);
         Row.Name.Text (Name);
         Row.Style ("border", "1px solid black");
      end Create;

      procedure Create
        (Widget : in out Position_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "")
      is
      begin
         Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent, ID);
         for I in Widget.Rows'Range loop
            Widget.Rows (I).Create (Widget, Form, UXStrings.From_UTF_8 (Nice_Axis_Names (I) & " (mm):"));
         end loop;
      end Create;

      function Get (Widget : Position_Widget) return Prunt.Position is
         Pos : Prunt.Position;
      begin
         for I in Pos'Range loop
            Pos (I) := Dimensionless'Value (Widget.Rows (I).Input.Value.To_UTF_8) * mm;
         end loop;

         return Pos;
      end Get;

      procedure Set (Widget : in out Position_Widget; Pos : Prunt.Position) is
      begin
         for I in Pos'Range loop
            Widget.Rows (I).Input.Value (DF_Image (Pos (I) / mm));
         end loop;
      end Set;

      procedure Create
        (Widget : in out Position_Scale_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "")
      is
      begin
         Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent, ID);
         for I in Widget.Rows'Range loop
            Widget.Rows (I).Create (Widget, Form, UXStrings.From_UTF_8 (Nice_Axis_Names (I) & ":"));
         end loop;
      end Create;

      function Get (Widget : Position_Scale_Widget) return Position_Scale is
         Scale : Position_Scale;
      begin
         for I in Scale'Range loop
            Scale (I) := Dimensionless'Value (Widget.Rows (I).Input.Value.To_UTF_8);
         end loop;

         return Scale;
      end Get;

      procedure Set (Widget : in out Position_Scale_Widget; Scale : Position_Scale) is
      begin
         for I in Scale'Range loop
            Widget.Rows (I).Input.Value (DF_Image (Scale (I)));
         end loop;
      end Set;

      procedure Create
        (Widget : in out Axial_Velocities_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "")
      is
      begin
         Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent, ID);
         for I in Widget.Rows'Range loop
            Widget.Rows (I).Create (Widget, Form, UXStrings.From_UTF_8 (Nice_Axis_Names (I) & " (mm / s):"));
         end loop;
      end Create;

      function Get (Widget : Axial_Velocities_Widget) return Axial_Velocities is
         Vels : Axial_Velocities;
      begin
         for I in Vels'Range loop
            Vels (I) := Dimensionless'Value (Widget.Rows (I).Input.Value.To_UTF_8) * mm / s;
         end loop;

         return Vels;
      end Get;

      procedure Set (Widget : in out Axial_Velocities_Widget; Vels : Axial_Velocities) is
      begin
         for I in Vels'Range loop
            Widget.Rows (I).Input.Value (DF_Image (Vels (I) / (mm / s)));
         end loop;
      end Set;

      procedure Create
        (Widget : in out Attached_Steppers_Widget;
         Parent : in out Gnoga.Gui.Element.Element_Type'Class;
         Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class;
         ID     :        UXString := "")
      is
      begin
         Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent, ID);
         for I in Widget.Rows'Range loop
            Widget.Rows (I).Create (Widget, Form, UXStrings.From_UTF_8 (I'Image));
         end loop;
      end Create;

      function Get (Widget : Attached_Steppers_Widget) return My_Config.Attached_Steppers is
         Steppers : My_Config.Attached_Steppers;
      begin
         for I in Widget.Rows'Range loop
            Steppers (I) := Widget.Rows (I).Input.Checked;
         end loop;

         return Steppers;
      end Get;

      procedure Set (Widget : in out Attached_Steppers_Widget; Steppers : My_Config.Attached_Steppers) is
      begin
         for I in Widget.Rows'Range loop
            Widget.Rows (I).Input.Checked (Steppers (I));
         end loop;
      end Set;

   end Grouped_Element_Widgets;

   package body Outer_Section_Widgets is

      procedure On_Submit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
         App   : constant Gnoga.Types.Pointer_to_Connection_Data_Class := Object.Connection_Data;
         Image : UXString;
         procedure Inner (Section : in out Outer_Section_Widget'Class) is
         begin
            Log_And_Switch_Tab (App, "Saving. Please wait.");
            Save_Data (Section, Image);
            Log_And_Switch_Tab (App, "Save done.");
            Log_And_Switch_Tab (App, Image);
            begin
               Log_And_Switch_Tab (App, "Please wait for read-back.");
               Read_Data (Section);
               Log_And_Switch_Tab (App, "Read-back done.");
               Log_And_Switch_Tab
                 (App,
                  "A restart is required to apply the new configuration. " &
                  "You may keep editing the configuration and further changes will also be saved.");
            exception
               when E : others =>
                  Log_And_Switch_Tab (App, "Read-back failed.");
                  Log_And_Switch_Tab (App, UXStrings.From_UTF_8 (Ada.Exceptions.Exception_Information (E)));
            end;
         exception
            when E : others =>
               Log_And_Switch_Tab (App, "Save failed.");
               Log_And_Switch_Tab (App, UXStrings.From_UTF_8 (Ada.Exceptions.Exception_Information (E)));
         end Inner;
      begin
         Inner (Outer_Section_Widget'Class (Object));
      end On_Submit;

   end Outer_Section_Widgets;

   package body Section_Widgets is

      procedure Create_Widget (View : in out Prunt_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Enabled_Input.Create (Form => View);
         View.Enabled_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Enabled:",
            Description => "If set, allow Prunt to start.",
            Data        => View.Enabled_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Prunt_Widget) is
         Params : My_Config.Prunt_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         View.Enabled_Input.Set (Params.Enabled);
      end Read_Data;

      overriding procedure Save_Data (View : in out Prunt_Widget; Image : out UXString) is
         Params : My_Config.Prunt_Parameters;
      begin
         Params.Enabled := View.Enabled_Input.Get;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View    : in out Stepper_Widget;
         Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
         Stepper :        My_Config.Stepper_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Stepper := Stepper;

         View.Enabled_Input.Create (Form => View);
         View.Enabled_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Enabled:",
            Description =>
              "If set, enable outputs to the stepper and allows it to be used for motion. " &
              "A stepper that is not enabled will never output any signals. " &
              "On some boards the outputs may be high impedance in this state.",
            Data        => View.Enabled_Input);

         View.Mm_Per_Step_Input.Create (Form => View);
         View.Mm_Per_Step_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Mm Per Step (mm):",
            Description => "The distance moved by the stepper for each step signal.",
            Data        => View.Mm_Per_Step_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Stepper_Widget) is
         Params : My_Config.Stepper_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Stepper);

         View.Enabled_Input.Set (Params.Enabled);
         View.Mm_Per_Step_Input.Set (Params.Mm_Per_Step);
      end Read_Data;

      overriding procedure Save_Data (View : in out Stepper_Widget; Image : out UXString) is
         Params : My_Config.Stepper_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Stepper);
         --  TODO: Remove this after we add parameters per stepper kind.

         Params.Enabled     := View.Enabled_Input.Get;
         Params.Mm_Per_Step := View.Mm_Per_Step_Input.Get;

         My_Config.Config_File.Write (Params, View.Stepper);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget (View : in out Kinematics_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Lower_Pos_Limit_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Lower_Pos_Limit_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Lower Position Limit:",
            Description =>
              "Minimum position that the printer may move to. The E axis may be set to " &
              DF_Image (Length'First / 2.0) & " for effectively infinite range.",
            Data        => View.Lower_Pos_Limit_Input);

         View.Upper_Pos_Limit_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Upper_Pos_Limit_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Upper Position Limit:",
            Description =>
              "Maximum position that the printer may move to. The E axis may be set to " &
              DF_Image (Length'Last / 2.0) & " for effectively infinite range.",
            Data        => View.Upper_Pos_Limit_Input);

         View.Tangential_Velocity_Max_Input.Create (Form => View);
         View.Tangential_Velocity_Max_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Max Feedrate (mm / s):",
            Description => "Maximum tangential feedrate. Feedrates higher than this value will be clipped.",
            Data        => View.Tangential_Velocity_Max_Input);

         View.Acceleration_Max_Input.Create (Form => View);
         View.Acceleration_Max_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Max Acceleration (mm / s**2):",
            Description => "",
            Data        => View.Acceleration_Max_Input);

         View.Jerk_Max_Input.Create (Form => View);
         View.Jerk_Max_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Max Jerk (mm / s**3):",
            Description => "",
            Data        => View.Jerk_Max_Input);

         View.Snap_Max_Input.Create (Form => View);
         View.Snap_Max_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Max Snap (mm / s**4):",
            Description => "",
            Data        => View.Snap_Max_Input);

         View.Crackle_Max_Input.Create (Form => View);
         View.Crackle_Max_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Max Crackle (mm / s**5):",
            Description => "",
            Data        => View.Crackle_Max_Input);

         View.Pop_Max_Input.Create (Form => View);
         View.Pop_Max_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Max Pop:",
            Description => "Does nothing aside from completing the set.",
            Data        => View.Pop_Max_Input);

         View.Axial_Velocity_Maxes_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Axial_Velocity_Maxes_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Axial Velocity Maxes:",
            Description =>
              "Maximum axial velocities. " &
              "Feedrates that result in axial velocities higher than these values will be clipped.",
            Data        => View.Axial_Velocity_Maxes_Input);

         View.Ignore_E_In_XYZE_Input.Create (Form => View);
         View.Ignore_E_In_XYZE_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Ignore E In XYZE:",
            Description =>
              "Ignore the E axis unless it is the only axis involved in a move. " &
              "This behaviour is the default in some other 3D printer motion controllers.",
            Data        => View.Ignore_E_In_XYZE_Input);

         View.Shift_Blended_Corners_Input.Create (Form => View);
         View.Shift_Blended_Corners_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Shift Blended Corners:",
            Description =>
              "When blending corners, shift the virtual corners so that the curve intersects the original corner. " &
              "Corners that are too close to the edges of the work area will never be shifted.",
            Data        => View.Shift_Blended_Corners_Input);

         View.Pressure_Advance_Time_Input.Create (Form => View);
         View.Pressure_Advance_Time_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Pressure Advance Time (s):",
            Description => "The E axis velocity is multiplied by this value added to the E axis position.",
            Data        => View.Pressure_Advance_Time_Input);

         View.Chord_Error_Max_Input.Create (Form => View);
         View.Chord_Error_Max_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Max Chord Error (mm):",
            Description => "Maximum distance that the path may deviate from the commanded path.",
            Data        => View.Chord_Error_Max_Input);

         View.Higher_Order_Scaler_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Higher_Order_Scaler_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Higher Order Scaler:",
            Description =>
              "Inside the motion planner, " &
              "all positions are multiples by this value before applying motion profile limits, " &
              "allowing for different limits on different axes. " &
              "You do not need to take this value in to account when setting position limits or mm per step values. " &
              "Feedrates are limited based on the real positions, not the scaled positions." &
              "Corner deviation is based on scaled positions." &
              "Acceleration and above are limited based on scaled values.",
            Data        => View.Higher_Order_Scaler_Input);

         View.Z_Steppers_Input.Create (Parent => View.Widget_Table, Form => View);
         View.Z_Steppers_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Z Steppers:",
            Description => "Steppers attached to the Z axis.",
            Data        => View.Z_Steppers_Input);

         View.E_Steppers_Input.Create (Parent => View.Widget_Table, Form => View);
         View.E_Steppers_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "E Steppers:",
            Description => "Steppers attached to the E axis.",
            Data        => View.E_Steppers_Input);

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Cartesian_Table.Create (View.Kind_Table);
         View.Cartesian_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Cartesian", View.Cartesian_Table'Access);

         View.X_Steppers_Input.Create (Parent => View.Cartesian_Table, Form => View);
         View.X_Steppers_Row.Create
           (Parent      => View.Cartesian_Table,
            Name        => "X Steppers:",
            Description => "Steppers attached to the X axis.",
            Data        => View.X_Steppers_Input);

         View.Y_Steppers_Input.Create (Parent => View.Cartesian_Table, Form => View);
         View.Y_Steppers_Row.Create
           (Parent      => View.Cartesian_Table,
            Name        => "Y Steppers:",
            Description => "Steppers attached to the Y axis.",
            Data        => View.Y_Steppers_Input);

         View.Core_XY_Table.Create (View.Kind_Table);
         View.Core_XY_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Core XY", View.Core_XY_Table'Access);

         View.A_Steppers_Input.Create (Parent => View.Core_XY_Table, Form => View);
         View.A_Steppers_Row.Create
           (Parent      => View.Core_XY_Table,
            Name        => "A Steppers:",
            Description => "Steppers attached to the A axis. X axis = 0.5 * (A + B), Y axis = 0.5 * (A – B).",
            Data        => View.A_Steppers_Input);

         View.B_Steppers_Input.Create (Parent => View.Core_XY_Table, Form => View);
         View.B_Steppers_Row.Create
           (Parent      => View.Core_XY_Table,
            Name        => "B Steppers:",
            Description => "Steppers attached to the B axis. X axis = 0.5 * (A + B), Y axis = 0.5 * (A – B).",
            Data        => View.B_Steppers_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Kinematics_Widget) is
         Params : My_Config.Kinematics_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         case Params.Kind is
            when My_Config.Cartesian_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Cartesian");
               View.X_Steppers_Input.Set (Params.X_Steppers);
               View.Y_Steppers_Input.Set (Params.Y_Steppers);
            when My_Config.Core_XY_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Core XY");
               View.A_Steppers_Input.Set (Params.A_Steppers);
               View.B_Steppers_Input.Set (Params.B_Steppers);
         end case;
         View.Lower_Pos_Limit_Input.Set (Params.Planner_Parameters.Lower_Pos_Limit);
         View.Upper_Pos_Limit_Input.Set (Params.Planner_Parameters.Upper_Pos_Limit);
         View.Tangential_Velocity_Max_Input.Set (Params.Planner_Parameters.Tangential_Velocity_Max);
         View.Acceleration_Max_Input.Set (Params.Planner_Parameters.Acceleration_Max);
         View.Jerk_Max_Input.Set (Params.Planner_Parameters.Jerk_Max);
         View.Snap_Max_Input.Set (Params.Planner_Parameters.Snap_Max);
         View.Crackle_Max_Input.Set (Params.Planner_Parameters.Crackle_Max);
         View.Axial_Velocity_Maxes_Input.Set (Params.Planner_Parameters.Axial_Velocity_Maxes);
         View.Ignore_E_In_XYZE_Input.Set (Params.Planner_Parameters.Ignore_E_In_XYZE);
         View.Shift_Blended_Corners_Input.Set (Params.Planner_Parameters.Shift_Blended_Corners);
         View.Pressure_Advance_Time_Input.Set (Params.Planner_Parameters.Pressure_Advance_Time);
         View.Chord_Error_Max_Input.Set (Params.Planner_Parameters.Chord_Error_Max);
         View.Higher_Order_Scaler_Input.Set (Params.Planner_Parameters.Higher_Order_Scaler);
         View.Z_Steppers_Input.Set (Params.Z_Steppers);
         View.E_Steppers_Input.Set (Params.E_Steppers);
      end Read_Data;

      overriding procedure Save_Data (View : in out Kinematics_Widget; Image : out UXString) is
         Params : My_Config.Kinematics_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Cartesian_Table'Unrestricted_Access then
            Params            := (Kind => My_Config.Cartesian_Kind, others => <>);
            Params.X_Steppers := View.X_Steppers_Input.Get;
            Params.Y_Steppers := View.Y_Steppers_Input.Get;
         elsif View.Kind_Table.Cards.Current_Card = View.Core_XY_Table'Unrestricted_Access then
            Params            := (Kind => My_Config.Core_XY_Kind, others => <>);
            Params.A_Steppers := View.A_Steppers_Input.Get;
            Params.B_Steppers := View.B_Steppers_Input.Get;
         else
            raise Constraint_Error with "Kinematics type must be selected.";
         end if;

         Params.Planner_Parameters.Lower_Pos_Limit         := View.Lower_Pos_Limit_Input.Get;
         Params.Planner_Parameters.Upper_Pos_Limit         := View.Upper_Pos_Limit_Input.Get;
         Params.Planner_Parameters.Tangential_Velocity_Max := View.Tangential_Velocity_Max_Input.Get;
         Params.Planner_Parameters.Acceleration_Max        := View.Acceleration_Max_Input.Get;
         Params.Planner_Parameters.Jerk_Max                := View.Jerk_Max_Input.Get;
         Params.Planner_Parameters.Snap_Max                := View.Snap_Max_Input.Get;
         Params.Planner_Parameters.Crackle_Max             := View.Crackle_Max_Input.Get;
         Params.Planner_Parameters.Axial_Velocity_Maxes    := View.Axial_Velocity_Maxes_Input.Get;
         Params.Planner_Parameters.Ignore_E_In_XYZE        := View.Ignore_E_In_XYZE_Input.Get;
         Params.Planner_Parameters.Shift_Blended_Corners   := View.Shift_Blended_Corners_Input.Get;
         Params.Planner_Parameters.Pressure_Advance_Time   := View.Pressure_Advance_Time_Input.Get;
         Params.Planner_Parameters.Chord_Error_Max         := View.Chord_Error_Max_Input.Get;
         Params.Planner_Parameters.Higher_Order_Scaler     := View.Higher_Order_Scaler_Input.Get;
         Params.Z_Steppers                                 := View.Z_Steppers_Input.Get;
         Params.E_Steppers                                 := View.E_Steppers_Input.Get;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View         : in out Input_Switch_Widget;
         Parent       : in out Gnoga.Gui.Base.Base_Type'Class;
         Input_Switch :        My_Config.Input_Switch_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Input_Switch := Input_Switch;

         View.Enabled_Input.Create (Form => View);
         View.Enabled_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Enabled:",
            Description => "If set, marks the switch as enabled so it may be used for homing.",
            Data        => View.Enabled_Input);

         View.Hit_On_High_Input.Create (Form => View);
         View.Hit_On_High_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Hit On High:",
            Description => "If set, the switch is considered to be hit when the related signal is high.",
            Data        => View.Hit_On_High_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Input_Switch_Widget) is
         Params : My_Config.Input_Switch_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Input_Switch);

         View.Enabled_Input.Set (Params.Enabled);
         View.Hit_On_High_Input.Set (Params.Hit_On_High);
      end Read_Data;

      overriding procedure Save_Data (View : in out Input_Switch_Widget; Image : out UXString) is
         Params : My_Config.Input_Switch_Parameters;
      begin
         Params.Enabled     := View.Enabled_Input.Get;
         Params.Hit_On_High := View.Hit_On_High_Input.Get;

         My_Config.Config_File.Write (Params, View.Input_Switch);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View : in out Homing_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Axis : Axis_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Axis := Axis;

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Double_Tap_Table.Create (View.Kind_Table);
         View.Double_Tap_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Double Tap", View.Double_Tap_Table'Access);

         View.Switch_Input.Create (Form => View);
         View.Switch_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "Switch:",
            Description => "The switch used for homing this axis.",
            Data        => View.Switch_Input);

         View.First_Move_Distance_Input.Create (Form => View);
         View.First_Move_Distance_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "First Move Distance (mm):",
            Description =>
              "The minimum length of the first move, may be negative to home towards negative infinity. " &
              "The axis will move at least this far in total during the first homing move, " &
              "and no further than this far after the switch is hit.",
            Data        => View.First_Move_Distance_Input);

         View.Back_Off_Move_Distance_Input.Create (Form => View);
         View.Back_Off_Move_Distance_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "Back-Off Move Distance (mm):",
            Description =>
              "The distance to back off after the first move, after the second move, " &
              "and before the first move iff the switch is hit before the first move." &
              "If the move after the second move would place the axis outside of the work area then the axis will " &
              "instead move to the closest face of the work area to the desired position.",
            Data        => View.Back_Off_Move_Distance_Input);

         View.Second_Move_Distance_Input.Create (Form => View);
         View.Second_Move_Distance_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "Second Move Distance (mm):",
            Description =>
              "The minimum length of the second move, may be negative to home towards negative infinity, " &
              "must be the same sign as the first distance. " & "The axis will move at least this far in total, " &
              "and no further than this far after the switch is hit.",
            Data        => View.Second_Move_Distance_Input);

         View.Switch_Position_Input.Create (Form => View);
         View.Switch_Position_Row.Create
           (Parent      => View.Double_Tap_Table,
            Name        => "Switch Position (mm):",
            Description =>
              "The position that the axis is considered to be at when the switch is hit during the second move." &
              "This position does not need to be inside the working area. " &
              "If it is outside then the axis will move back in to the working area after homing.",
            Data        => View.Switch_Position_Input);

         View.Set_To_Value_Table.Create (View.Kind_Table);
         View.Set_To_Value_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Set To Value", View.Set_To_Value_Table'Access);

         View.Value_Input.Create (Form => View);
         View.Value_Row.Create
           (Parent      => View.Set_To_Value_Table,
            Name        => "Value (mm):",
            Description =>
              "The value the axis is assumed to be at when the axis is homed. No homing move will occur in this mode.",
            Data        => View.Value_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Homing_Widget) is
         Params : My_Config.Homing_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Axis);

         case Params.Kind is
            when My_Config.Double_Tap_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Double Tap");
               View.Switch_Input.Set (Params.Switch);
               View.First_Move_Distance_Input.Set (Params.First_Move_Distance);
               View.Back_Off_Move_Distance_Input.Set (Params.Back_Off_Move_Distance);
               View.Second_Move_Distance_Input.Set (Params.Second_Move_Distance);
               View.Switch_Position_Input.Set (Params.Switch_Position);
            when My_Config.Set_To_Value_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Set To Value");
               View.Value_Input.Set (Params.Value);
         end case;
      end Read_Data;

      overriding procedure Save_Data (View : in out Homing_Widget; Image : out UXString) is
         Params : My_Config.Homing_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Double_Tap_Table'Unrestricted_Access then
            Params                        := (Kind => My_Config.Double_Tap_Kind, others => <>);
            Params.Switch                 := View.Switch_Input.Get;
            Params.First_Move_Distance    := View.First_Move_Distance_Input.Get;
            Params.Back_Off_Move_Distance := View.Back_Off_Move_Distance_Input.Get;
            Params.Second_Move_Distance   := View.Second_Move_Distance_Input.Get;
            Params.Switch_Position        := View.Switch_Position_Input.Get;
         elsif View.Kind_Table.Cards.Current_Card = View.Set_To_Value_Table'Unrestricted_Access then
            Params       := (Kind => My_Config.Set_To_Value_Kind, others => <>);
            Params.Value := View.Value_Input.Get;
         else
            raise Constraint_Error with "Homing type must be selected.";
         end if;

         My_Config.Config_File.Write (Params, View.Axis);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget (View : in out Extruder_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Nozzle_Diameter_Input.Create (Form => View);
         View.Nozzle_Diameter_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Nozzle Diameter (mm):",
            Description => "The diameter of the nozzle.",
            Data        => View.Nozzle_Diameter_Input);

         View.Filament_Diameter_Input.Create (Form => View);
         View.Filament_Diameter_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Filament Diameter (mm):",
            Description => "The diameter of the filament.",
            Data        => View.Filament_Diameter_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Extruder_Widget) is
         Params : My_Config.Extruder_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         View.Nozzle_Diameter_Input.Set (Params.Nozzle_Diameter);
         View.Filament_Diameter_Input.Set (Params.Filament_Diameter);
      end Read_Data;

      overriding procedure Save_Data (View : in out Extruder_Widget; Image : out UXString) is
         Params : My_Config.Extruder_Parameters;
      begin
         Params.Nozzle_Diameter   := View.Nozzle_Diameter_Input.Get;
         Params.Filament_Diameter := View.Filament_Diameter_Input.Get;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View       : in out Thermistor_Widget;
         Parent     : in out Gnoga.Gui.Base.Base_Type'Class;
         Thermistor :        My_Config.Thermistor_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Thermistor := Thermistor;

         View.Minimum_Temperature_Input.Create (Form => View);
         View.Minimum_Temperature_Row.Create
           (Parent      => View.Widget_Table,
            Name        => UXStrings.From_UTF_8 ("Minimum Temperature (C):"),
            Description => "Any temperature below this value indicates a failure.",
            Data        => View.Minimum_Temperature_Input);

         View.Maximum_Temperature_Input.Create (Form => View);
         View.Maximum_Temperature_Row.Create
           (Parent      => View.Widget_Table,
            Name        => UXStrings.From_UTF_8 ("Maximum Temperature (C):"),
            Description => "Any temperature above this value indicates a failure.",
            Data        => View.Maximum_Temperature_Input);

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Disabled_Table.Create (View.Kind_Table);
         View.Disabled_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Disabled", View.Disabled_Table'Access);

         View.Steinhart_Hart_Table.Create (View.Kind_Table);
         View.Steinhart_Hart_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Steinhart-Hart", View.Steinhart_Hart_Table'Access);

         View.SH_A_Input.Create (Form => View);
         View.SH_A_Row.Create
           (Parent      => View.Steinhart_Hart_Table,
            Name        => UXStrings.From_UTF_8 ("Coefficient A:"),
            Description => "In 1 / T = A + B * ln (R) + C * (ln (R))**3 where T is in kelvins and R is in ohms.",
            Data        => View.SH_A_Input);

         View.SH_B_Input.Create (Form => View);
         View.SH_B_Row.Create
           (Parent      => View.Steinhart_Hart_Table,
            Name        => UXStrings.From_UTF_8 ("Coefficient B:"),
            Description => "In 1 / T = A + B * ln (R) + C * (ln (R))**3 where T is in kelvins and R is in ohms.",
            Data        => View.SH_B_Input);

         View.SH_C_Input.Create (Form => View);
         View.SH_C_Row.Create
           (Parent      => View.Steinhart_Hart_Table,
            Name        => UXStrings.From_UTF_8 ("Coefficient C:"),
            Description => "In 1 / T = A + B * ln (R) + C * (ln (R))**3 where T is in kelvins and R is in ohms.",
            Data        => View.SH_C_Input);

         View.Callendar_Van_Dusen_Table.Create (View.Kind_Table);
         View.Callendar_Van_Dusen_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Callendar-Van Dusen", View.Callendar_Van_Dusen_Table'Access);

         View.CVD_R0_Input.Create (Form => View);
         View.CVD_R0_Row.Create
           (Parent      => View.Callendar_Van_Dusen_Table,
            Name        => UXStrings.From_UTF_8 ("Resistance at 0C (ohm):"),
            Description => "",
            Data        => View.CVD_R0_Input);

         View.CVD_A_Input.Create (Form => View);
         View.CVD_A_Row.Create
           (Parent      => View.Callendar_Van_Dusen_Table,
            Name        => UXStrings.From_UTF_8 ("Coefficient A:"),
            Description => "In R (0) * (1 + A * T + B * T**2) where T is in celcius.",
            Data        => View.CVD_A_Input);

         View.CVD_B_Input.Create (Form => View);
         View.CVD_B_Row.Create
           (Parent      => View.Callendar_Van_Dusen_Table,
            Name        => UXStrings.From_UTF_8 ("Coefficient B:"),
            Description => "In R (0) * (1 + A * T + B * T**2) where T is in celcius.",
            Data        => View.CVD_B_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Thermistor_Widget) is
         Params : Thermistor_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Thermistor);

         case Params.Kind is
            when Disabled_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Disabled");
            when Steinhart_Hart_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Steinhart-Hart");
               View.SH_A_Input.Set (Params.SH_A);
               View.SH_B_Input.Set (Params.SH_B);
               View.SH_C_Input.Set (Params.SH_C);
            when Callendar_Van_Dusen_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Callendar-Van Dusen");
               View.CVD_R0_Input.Set (Params.CVD_R0);
               View.CVD_A_Input.Set (Params.CVD_A);
               View.CVD_B_Input.Set (Params.CVD_B);
         end case;
         View.Minimum_Temperature_Input.Set (Params.Minimum_Temperature);
         View.Maximum_Temperature_Input.Set (Params.Maximum_Temperature);
      end Read_Data;

      overriding procedure Save_Data (View : in out Thermistor_Widget; Image : out UXString) is
         Params : Thermistor_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Disabled_Table'Unrestricted_Access then
            Params := (Kind => Disabled_Kind, others => <>);
         elsif View.Kind_Table.Cards.Current_Card = View.Steinhart_Hart_Table'Unrestricted_Access then
            Params      := (Kind => Steinhart_Hart_Kind, others => <>);
            Params.SH_A := View.SH_A_Input.Get;
            Params.SH_B := View.SH_B_Input.Get;
            Params.SH_C := View.SH_C_Input.Get;
         elsif View.Kind_Table.Cards.Current_Card = View.Callendar_Van_Dusen_Table'Unrestricted_Access then
            Params        := (Kind => Callendar_Van_Dusen_Kind, others => <>);
            Params.CVD_R0 := View.CVD_R0_Input.Get;
            Params.CVD_A  := View.CVD_A_Input.Get;
            Params.CVD_B  := View.CVD_B_Input.Get;
         else
            raise Constraint_Error with "Thermistor type must be selected";
         end if;
         Params.Minimum_Temperature := View.Minimum_Temperature_Input.Get;
         Params.Maximum_Temperature := View.Maximum_Temperature_Input.Get;

         My_Config.Config_File.Write (Params, View.Thermistor);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View : in out Heater_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Heater : My_Config.Heater_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Heater := Heater;

         View.Thermistor_Input.Create (Form => View);
         View.Thermistor_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Thermistor:",
            Description => "Thermistor associated with this heater.",
            Data        => View.Thermistor_Input);

         View.Max_Cumulative_Error_Input.Create (Form => View);
         View.Max_Cumulative_Error_Row.Create
           (Parent      => View.Widget_Table,
            Name        => UXStrings.From_UTF_8 ("Max Cumulative Error (C):"),
            Description => "Maximum cumulative error before a failure is detected.",
            Data        => View.Max_Cumulative_Error_Input);

         View.Check_Gain_Time_Input.Create (Form => View);
         View.Check_Gain_Time_Row.Create
           (Parent      => View.Widget_Table,
            Name        => UXStrings.From_UTF_8 ("Check Gain Time (s):"),
            Description => "Period to check for temperature rise over during heating to detect failures.",
            Data        => View.Check_Gain_Time_Input);

         View.Check_Minimum_Gain_Input.Create (Form => View);
         View.Check_Minimum_Gain_Row.Create
           (Parent      => View.Widget_Table,
            Name        => UXStrings.From_UTF_8 ("Check Minimum Gain (C):"),
            Description => "Minium temperature rise required in gain period to reset cumulative error.",
            Data        => View.Check_Minimum_Gain_Input);

         View.Hysteresis_Input.Create (Form => View);
         View.Hysteresis_Row.Create
           (Parent      => View.Widget_Table,
            Name        => UXStrings.From_UTF_8 ("Hysteresis (C):"),
            Description =>
              "Maximum temperature below or above the setpoint where " &
              "the heater is considered to be at temperature. " &
              "Also used bang-bang mode to determine when to switch on and off.",
            Data        => View.Hysteresis_Input);

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Disabled_Table.Create (View.Kind_Table);
         View.Disabled_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Disabled", View.Disabled_Table'Access);

         View.PID_Table.Create (View.Kind_Table);
         View.PID_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("PID", View.PID_Table'Access);

         View.Proportional_Scale_Input.Create (Form => View);
         View.Proportional_Scale_Row.Create
           (Parent      => View.PID_Table,
            Name        => "Proportional Scale:",
            Description => "Coefficient for the proportional term.",
            Data        => View.Proportional_Scale_Input);

         View.Integral_Scale_Input.Create (Form => View);
         View.Integral_Scale_Row.Create
           (Parent      => View.PID_Table,
            Name        => "Integral Scale:",
            Description => "Coefficient for the integral term.",
            Data        => View.Integral_Scale_Input);

         View.Derivative_Scale_Input.Create (Form => View);
         View.Derivative_Scale_Row.Create
           (Parent      => View.PID_Table,
            Name        => "Derivative Scale:",
            Description => "Coefficient for the derivative term.",
            Data        => View.Derivative_Scale_Input);

         View.Proportional_On_Measurement_Input.Create (Form => View);
         View.Proportional_On_Measurement_Row.Create
           (Parent      => View.PID_Table,
            Name        => "Proportional On Measurement:",
            Description =>
              "Use the incoming measurement rather than the error for the proportional term. " &
              "This drastically changes how the PID algorithm works and as such most common PID " &
              "tuning methods will not work. When tuning these values note that a higher " &
              "proportional scale will make the algorithm more conservative.",
            Data        => View.Proportional_On_Measurement_Input);

         View.Bang_Bang_Table.Create (View.Kind_Table);
         View.Bang_Bang_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Bang Bang", View.Bang_Bang_Table'Access);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Heater_Widget) is
         Params : My_Config.Heater_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Heater);

         case Params.Kind is
            when My_Config.Disabled_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Disabled");
            when My_Config.PID_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("PID");
               View.Proportional_Scale_Input.Set (Params.Proportional_Scale);
               View.Integral_Scale_Input.Set (Params.Integral_Scale);
               View.Derivative_Scale_Input.Set (Params.Derivative_Scale);
               View.Proportional_On_Measurement_Input.Set (Params.Proportional_On_Measurement);
            when My_Config.Bang_Bang_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Bang Bang");
         end case;
         View.Thermistor_Input.Set (Params.Thermistor);
         View.Max_Cumulative_Error_Input.Set (Params.Max_Cumulative_Error);
         View.Check_Gain_Time_Input.Set (Params.Check_Gain_Time);
         View.Check_Minimum_Gain_Input.Set (Params.Check_Minimum_Gain);
         View.Hysteresis_Input.Set (Params.Hysteresis);
      end Read_Data;

      overriding procedure Save_Data (View : in out Heater_Widget; Image : out UXString) is
         Params : My_Config.Heater_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Disabled_Table'Unrestricted_Access then
            Params := (Kind => My_Config.Disabled_Kind, others => <>);
         elsif View.Kind_Table.Cards.Current_Card = View.PID_Table'Unrestricted_Access then
            Params                             := (Kind => My_Config.PID_Kind, others => <>);
            Params.Proportional_Scale          := View.Proportional_Scale_Input.Get;
            Params.Integral_Scale              := View.Integral_Scale_Input.Get;
            Params.Derivative_Scale            := View.Derivative_Scale_Input.Get;
            Params.Proportional_On_Measurement := View.Proportional_On_Measurement_Input.Get;
         elsif View.Kind_Table.Cards.Current_Card = View.Bang_Bang_Table'Unrestricted_Access then
            Params := (Kind => My_Config.Bang_Bang_Kind, others => <>);
         else
            raise Constraint_Error with "Heater type must be selected.";
         end if;

         Params.Thermistor           := View.Thermistor_Input.Get;
         Params.Max_Cumulative_Error := View.Max_Cumulative_Error_Input.Get;
         Params.Check_Gain_Time      := View.Check_Gain_Time_Input.Get;
         Params.Check_Minimum_Gain   := View.Check_Minimum_Gain_Input.Get;
         Params.Hysteresis           := View.Hysteresis_Input.Get;

         My_Config.Config_File.Write (Params, View.Heater);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget (View : in out Bed_Mesh_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.No_Mesh_Table.Create (View.Kind_Table);
         View.No_Mesh_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("No Mesh", View.No_Mesh_Table'Access);

         View.Beacon_Table.Create (View.Kind_Table);
         View.Beacon_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Beacon", View.Beacon_Table'Access);

         View.Serial_Port_Path_Input.Create (Form => View);
         View.Serial_Port_Path_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "Serial Port Path:",
            Description => "Path to the Beacon serial port.",
            Data        => View.Serial_Port_Path_Input);

         View.X_Offset_Input.Create (Form => View);
         View.X_Offset_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "X Offset (mm):",
            Description => "Offset along the X axis of the probe to the nozzle.",
            Data        => View.X_Offset_Input);

         View.Y_Offset_Input.Create (Form => View);
         View.Y_Offset_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "Y Offset (mm):",
            Description => "Offset along the Y axis of the probe to the nozzle.",
            Data        => View.Y_Offset_Input);

         View.Calibration_Floor_Input.Create (Form => View);
         View.Calibration_Floor_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "Calibration Floor (mm):",
            Description => "Z axis value to use for lowest calibration point.",
            Data        => View.Calibration_Floor_Input);

         View.Calibration_Ceiling_Input.Create (Form => View);
         View.Calibration_Ceiling_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "Calibration Ceiling (mm):",
            Description => "Z axis value to use for highest calibration point.",
            Data        => View.Calibration_Ceiling_Input);

         View.Calibration_Feedrate_Input.Create (Form => View);
         View.Calibration_Feedrate_Row.Create
           (Parent      => View.Beacon_Table,
            Name        => "Calibration Feedrate (mm / s):",
            Description => "Feedrate used for calibration sequence.",
            Data        => View.Calibration_Feedrate_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Bed_Mesh_Widget) is
         Params : My_Config.Bed_Mesh_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         case Params.Kind is
            when My_Config.No_Mesh_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("No Mesh");
            when My_Config.Beacon_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Beacon");
               View.Serial_Port_Path_Input.Set (Params.Serial_Port_Path);
               View.X_Offset_Input.Set (Params.X_Offset);
               View.Y_Offset_Input.Set (Params.Y_Offset);
               View.Calibration_Floor_Input.Set (Params.Calibration_Floor);
               View.Calibration_Ceiling_Input.Set (Params.Calibration_Ceiling);
               View.Calibration_Feedrate_Input.Set (Params.Calibration_Feedrate);
         end case;
      end Read_Data;

      overriding procedure Save_Data (View : in out Bed_Mesh_Widget; Image : out UXString) is
         Params : My_Config.Bed_Mesh_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.No_Mesh_Table'Unrestricted_Access then
            Params := (Kind => My_Config.No_Mesh_Kind);
         elsif View.Kind_Table.Cards.Current_Card = View.Beacon_Table'Unrestricted_Access then
            Params                      := (Kind => My_Config.Beacon_Kind, others => <>);
            Params.Serial_Port_Path     := View.Serial_Port_Path_Input.Get;
            Params.X_Offset             := View.X_Offset_Input.Get;
            Params.Y_Offset             := View.Y_Offset_Input.Get;
            Params.Calibration_Floor    := View.Calibration_Floor_Input.Get;
            Params.Calibration_Ceiling  := View.Calibration_Ceiling_Input.Get;
            Params.Calibration_Feedrate := View.Calibration_Feedrate_Input.Get;
         else
            raise Constraint_Error with "Bed Mesh type must be selected.";
         end if;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget
        (View : in out Fan_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class; Fan : My_Config.Fan_Name)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Fan := Fan;

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Disabled_Table.Create (View.Kind_Table);
         View.Disabled_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Disabled", View.Disabled_Table'Access);

         View.Dynamic_PWM_Table.Create (View.Kind_Table);
         View.Dynamic_PWM_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Dynamic PWM", View.Dynamic_PWM_Table'Access);

         View.Disable_Below_PWM_Input.Create (Form => View);
         View.Disable_Below_PWM_Row.Create
           (Parent      => View.Dynamic_PWM_Table,
            Name        => "Disable Below PWM:",
            Description => "Any set PWM ratio below this value will shut off power to the fan.",
            Data        => View.Disable_Below_PWM_Input);

         View.Max_PWM_Input.Create (Form => View);
         View.Max_PWM_Row.Create
           (Parent      => View.Dynamic_PWM_Table,
            Name        => "Max PWM:",
            Description =>
              "The maximum PWM ratio of the fan, corresponding to 100% power setting in g-code or the UI.",
            Data        => View.Max_PWM_Input);

         View.Always_On_Table.Create (View.Kind_Table);
         View.Always_On_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Always On", View.Always_On_Table'Access);

         View.Always_On_PWM_Input.Create (Form => View);
         View.Always_On_PWM_Row.Create
           (Parent      => View.Always_On_Table,
            Name        => "Always On PWM:",
            Description => "The constant PWM ratio to output.",
            Data        => View.Always_On_PWM_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Fan_Widget) is
         Params : My_Config.Fan_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Fan);

         case Params.Kind is
            when My_Config.Disabled_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Disabled");
            when My_Config.Dynamic_PWM_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Dynamic PWM");
               View.Disable_Below_PWM_Input.Set (Params.Disable_Below_PWM);
               View.Max_PWM_Input.Set (Params.Max_PWM);
            when My_Config.Always_On_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Always On");
               View.Always_On_PWM_Input.Set (Params.Always_On_PWM);
         end case;
      end Read_Data;

      overriding procedure Save_Data (View : in out Fan_Widget; Image : out UXString) is
         Params : My_Config.Fan_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Disabled_Table'Unrestricted_Access then
            Params := (Kind => My_Config.Disabled_Kind);
         elsif View.Kind_Table.Cards.Current_Card = View.Dynamic_PWM_Table'Unrestricted_Access then
            Params                   := (Kind => My_Config.Dynamic_PWM_Kind, others => <>);
            Params.Disable_Below_PWM := View.Disable_Below_PWM_Input.Get;
            Params.Max_PWM           := View.Max_PWM_Input.Get;
         elsif View.Kind_Table.Cards.Current_Card = View.Always_On_Table'Unrestricted_Access then
            Params               := (Kind => My_Config.Always_On_Kind, others => <>);
            Params.Always_On_PWM := View.Always_On_PWM_Input.Get;
         else
            raise Constraint_Error with "Fan type must be selected.";
         end if;

         My_Config.Config_File.Write (Params, View.Fan);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget (View : in out G_Code_Assignment_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class)
      is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Bed_Heater_Input.Create (Form => View);
         View.Bed_Heater_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Bed Heater:",
            Description => "The heater assigned to the bed in g-code.",
            Data        => View.Bed_Heater_Input);

         --  View.Chamber_Heater_Input.Create (Form => View);
         --  View.Chamber_Heater_Row.Create
         --    (Parent      => View.Widget_Table,
         --     Name        => "Chamber_Heater:",
         --     Description => "The heater assigned to the chamber in g-code.",
         --     Data        => View.Chamber_Heater_Input);

         View.Hotend_Heater_Input.Create (Form => View);
         View.Hotend_Heater_Row.Create
           (Parent      => View.Widget_Table,
            Name        => "Hotend Heater:",
            Description => "The heater assigned to the hotend in g-code.",
            Data        => View.Hotend_Heater_Input);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out G_Code_Assignment_Widget) is
         Params : My_Config.G_Code_Assignment_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         View.Bed_Heater_Input.Set (Params.Bed_Heater);
         --  View.Chamber_Heater_Input.Set (Params.Chamber_Heater);
         View.Hotend_Heater_Input.Set (Params.Hotend_Heater);
      end Read_Data;

      overriding procedure Save_Data (View : in out G_Code_Assignment_Widget; Image : out UXString) is
         Params : My_Config.G_Code_Assignment_Parameters;
      begin
         Params.Bed_Heater    := View.Bed_Heater_Input.Get;
         --  Params.Chamber_Heater := View.Chamber_Heater_Input.Get;
         Params.Hotend_Heater := View.Hotend_Heater_Input.Get;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

   end Section_Widgets;

end Prunt.GUI.Config_Editor;
