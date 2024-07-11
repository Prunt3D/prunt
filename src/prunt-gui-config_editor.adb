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
with Prunt.Heaters;     use Prunt.Heaters;

package body Prunt.GUI.Config_Editor is

   pragma Unsuppress (All_Checks);

   package body Basic_Inputs is

      function Get (Input : Path_String_Input) return My_Config.Path_Strings.Bounded_String is
      begin
         return My_Config.Path_Strings.To_Bounded_String (Input.Value.To_UTF_8);
      end Get;

      procedure Set (Input : in out Path_String_Input; Value : My_Config.Path_Strings.Bounded_String) is
      begin
         Input.Value (Value => UXStrings.From_UTF_8 (My_Config.Path_Strings.To_String (Value)));
      end Set;

      procedure Create_For_Parameter_Row
        (Element : in out Path_String_Input;
         Parent  : in out Gnoga.Gui.Element.Element_Type'Class;
         Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class)
      is
         pragma Unreferenced (Parent);
      begin
         Element.Create (Form);
      end Create_For_Parameter_Row;

      function Get (Input : Boolean_Input) return Boolean is
      begin
         return Input.Checked;
      end Get;

      procedure Set (Input : in out Boolean_Input; Value : Boolean) is
      begin
         Input.Checked (Value);
      end Set;

      procedure Create_For_Parameter_Row
        (Element : in out Boolean_Input;
         Parent  : in out Gnoga.Gui.Element.Element_Type'Class;
         Form    : in out Gnoga.Gui.Element.Form.Form_Type'Class)
      is
         pragma Unreferenced (Parent);
      begin
         Element.Create (Form);
      end Create_For_Parameter_Row;

   end Basic_Inputs;

   package body Grouped_Element_Widgets is

      package body Plain_Widgets is

         procedure Create_For_Parameter_Row
           (Widget : in out Position_Widget;
            Parent : in out Gnoga.Gui.Element.Element_Type'Class;
            Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class)
         is
         begin
            Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent);
            for I in Widget.Rows'Range loop
               Widget.Rows (I).Create (Widget, Form, UXStrings.From_UTF_8 (I'Image), "");
            end loop;
         end Create_For_Parameter_Row;

         function Get (Widget : Position_Widget) return Prunt.Position is
            Pos : Prunt.Position;
         begin
            for I in Pos'Range loop
               Pos (I) := Widget.Rows (I).Get_Data;
            end loop;

            return Pos;
         end Get;

         procedure Set (Widget : in out Position_Widget; Pos : Prunt.Position) is
         begin
            for I in Pos'Range loop
               Widget.Rows (I).Set_Data (Pos (I));
            end loop;
         end Set;

         procedure Create_For_Parameter_Row
           (Widget : in out Position_Scale_Widget;
            Parent : in out Gnoga.Gui.Element.Element_Type'Class;
            Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class)
         is
         begin
            Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent);
            for I in Widget.Rows'Range loop
               Widget.Rows (I).Create (Widget, Form, UXStrings.From_UTF_8 (I'Image), "");
            end loop;
         end Create_For_Parameter_Row;

         function Get (Widget : Position_Scale_Widget) return Position_Scale is
            Scale : Position_Scale;
         begin
            for I in Scale'Range loop
               Scale (I) := Widget.Rows (I).Get_Data;
            end loop;

            return Scale;
         end Get;

         procedure Set (Widget : in out Position_Scale_Widget; Scale : Position_Scale) is
         begin
            for I in Scale'Range loop
               Widget.Rows (I).Set_Data (Scale (I));
            end loop;
         end Set;

         procedure Create_For_Parameter_Row
           (Widget : in out Axial_Velocities_Widget;
            Parent : in out Gnoga.Gui.Element.Element_Type'Class;
            Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class)
         is
         begin
            Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent);
            for I in Widget.Rows'Range loop
               Widget.Rows (I).Create (Widget, Form, UXStrings.From_UTF_8 (I'Image), "");
            end loop;
         end Create_For_Parameter_Row;

         function Get (Widget : Axial_Velocities_Widget) return Axial_Velocities is
            Vels : Axial_Velocities;
         begin
            for I in Vels'Range loop
               Vels (I) := Widget.Rows (I).Get_Data;
            end loop;

            return Vels;
         end Get;

         procedure Set (Widget : in out Axial_Velocities_Widget; Vels : Axial_Velocities) is
         begin
            for I in Vels'Range loop
               Widget.Rows (I).Set_Data (Vels (I));
            end loop;
         end Set;

         procedure Create_For_Parameter_Row
           (Widget : in out Attached_Steppers_Widget;
            Parent : in out Gnoga.Gui.Element.Element_Type'Class;
            Form   : in out Gnoga.Gui.Element.Form.Form_Type'Class)
         is
         begin
            Gnoga.Gui.Element.Table.Table_Type (Widget).Create (Parent);
            for I in Widget.Rows'Range loop
               Widget.Rows (I).Create (Widget, Form, UXStrings.From_UTF_8 (I'Image), "");
            end loop;
         end Create_For_Parameter_Row;

         function Get (Widget : Attached_Steppers_Widget) return My_Config.Attached_Steppers is
            Steppers : My_Config.Attached_Steppers;
         begin
            for I in Widget.Rows'Range loop
               Steppers (I) := Widget.Rows (I).Get_Data;
            end loop;

            return Steppers;
         end Get;

         procedure Set (Widget : in out Attached_Steppers_Widget; Steppers : My_Config.Attached_Steppers) is
         begin
            for I in Widget.Rows'Range loop
               Widget.Rows (I).Set_Data (Steppers (I));
            end loop;
         end Set;

      end Plain_Widgets;

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

         View.Enabled.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Enabled",
            Description => "If set, allow Prunt to start.");

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Prunt_Widget) is
         Params : My_Config.Prunt_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         View.Enabled.Set_Data (Params.Enabled);
      end Read_Data;

      overriding procedure Save_Data (View : in out Prunt_Widget; Image : out UXString) is
         Params : My_Config.Prunt_Parameters;
      begin
         Params.Enabled := View.Enabled.Get_Data;

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

         View.Enabled.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Enabled",
            Description =>
              "If set, enable outputs to the stepper and allows it to be used for motion. " &
              "A stepper that is not enabled will never output any signals. " &
              "On some boards the outputs may be high impedance in this state.");

         View.Mm_Per_Step.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Mm Per Step",
            Description => "The distance moved by the stepper for each step signal.");

         case My_Config.Stepper_Kinds (Stepper) is
            when Basic_Kind =>
               null;
            when TMC2240_UART_Kind =>
               View.Output_Current.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "Output_Current",
                  Description =>
                    "Target output current for the motor. The lowest suitable Current_Range will be chosen " &
                    "and the Global_Scaler will be adjusted to a suitable value.");

               View.Slope_Control.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "Slope_Control",
                  Description => "As described in TMC2240 datasheet.");

               View.I_Hold.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "I_Hold",
                  Description => "Standstill current (0 = 1/32 ... 31 = 32/32).");

               View.I_Run.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "I_Run",
                  Description => "Run current (0 = 1/32 ... 31 = 32/32).");

               View.I_Hold_Delay.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "I_Hold_Delay",
                  Description => "As described in TMC2240 datasheet.");

               View.I_Run_Delay.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "I_Run_Delay",
                  Description => "As described in TMC2240 datasheet.");

               View.T_Power_Down.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "T_Power_Down",
                  Description => "As described in TMC2240 datasheet.");

               View.T_PWM_Thrs.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "T_PWM_Thrs",
                  Description => "As described in TMC2240 datasheet.");

               View.T_Cool_Thrs.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "T_Cool_Thrs",
                  Description => "As described in TMC2240 datasheet.");

               View.T_High.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "T_High",
                  Description => "As described in TMC2240 datasheet.");

               View.TOFF.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "TOFF",
                  Description => "As described in TMC2240 datasheet.");

               View.HSTRT_TFD210.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "HSTRT_TFD210",
                  Description => "As described in TMC2240 datasheet.");

               View.HEND_OFFSET.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "HEND_OFFSET",
                  Description => "As described in TMC2240 datasheet.");

               View.FD3.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "FD3",
                  Description => "As described in TMC2240 datasheet.");

               View.DISFDCC.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "DISFDCC",
                  Description => "As described in TMC2240 datasheet.");

               View.CHM.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "CHM",
                  Description => "As described in TMC2240 datasheet.");

               View.VHIGHFS.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "VHIGHFS",
                  Description => "As described in TMC2240 datasheet.");

               View.VHIGHCHM.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "VHIGHCHM",
                  Description => "As described in TMC2240 datasheet.");

               View.TPFD.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "TPFD",
                  Description => "As described in TMC2240 datasheet.");

               View.Microstep_Resolution.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "Microstep_Resolution",
                  Description => "As described in TMC2240 datasheet.");

               View.PWM_OFS.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "PWM_OFS",
                  Description => "As described in TMC2240 datasheet.");

               View.PWM_Grad.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "PWM_Grad",
                  Description => "As described in TMC2240 datasheet.");

               View.PWM_Freq.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "PWM_Freq",
                  Description => "As described in TMC2240 datasheet.");

               View.PWM_Auto_Scale.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "PWM_Auto_Scale",
                  Description => "As described in TMC2240 datasheet.");

               View.PWM_Auto_Grad.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "PWM_Auto_Grad",
                  Description => "As described in TMC2240 datasheet.");

               View.Freewheel.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "Freewheel",
                  Description => "As described in TMC2240 datasheet.");

               View.PWM_Meas_SD_Enable.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "PWM_Meas_SD_Enable",
                  Description => "As described in TMC2240 datasheet.");

               View.PWM_Dis_Reg_Stst.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "PWM_Dis_Reg_Stst",
                  Description => "As described in TMC2240 datasheet.");

               View.PWM_Reg.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "PWM_Reg",
                  Description => "As described in TMC2240 datasheet.");

               View.PWM_Lim.Create
                 (Parent      => View.Widget_Table,
                  Form        => View,
                  Name        => "PWM_Lim",
                  Description => "As described in TMC2240 datasheet.");
         end case;

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Stepper_Widget) is
         Params : My_Config.Stepper_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Stepper);

         case My_Config.Stepper_Kinds (View.Stepper) is
            when Basic_Kind =>
               null;
            when TMC2240_UART_Kind =>
               View.Output_Current.Set_Data (Params.Output_Current);
               View.Slope_Control.Set_Data (Params.Slope_Control);
               View.I_Hold.Set_Data (Params.I_Hold);
               View.I_Run.Set_Data (Params.I_Run);
               View.I_Hold_Delay.Set_Data (Params.I_Hold_Delay);
               View.I_Run_Delay.Set_Data (Params.I_Run_Delay);
               View.T_Power_Down.Set_Data (Params.T_Power_Down);
               View.T_PWM_Thrs.Set_Data (Params.T_PWM_Thrs);
               View.T_Cool_Thrs.Set_Data (Params.T_Cool_Thrs);
               View.T_High.Set_Data (Params.T_High);
               View.TOFF.Set_Data (Params.TOFF);
               View.HSTRT_TFD210.Set_Data (Params.HSTRT_TFD210);
               View.HEND_OFFSET.Set_Data (Params.HEND_OFFSET);
               View.FD3.Set_Data (Params.FD3);
               View.DISFDCC.Set_Data (Params.DISFDCC);
               View.CHM.Set_Data (Params.CHM);
               View.VHIGHFS.Set_Data (Params.VHIGHFS);
               View.VHIGHCHM.Set_Data (Params.VHIGHCHM);
               View.TPFD.Set_Data (Params.TPFD);
               View.Microstep_Resolution.Set_Data (Params.Microstep_Resolution);
               View.PWM_OFS.Set_Data (Params.PWM_OFS);
               View.PWM_Grad.Set_Data (Params.PWM_Grad);
               View.PWM_Freq.Set_Data (Params.PWM_Freq);
               View.PWM_Auto_Scale.Set_Data (Params.PWM_Auto_Scale);
               View.PWM_Auto_Grad.Set_Data (Params.PWM_Auto_Grad);
               View.Freewheel.Set_Data (Params.Freewheel);
               View.PWM_Meas_SD_Enable.Set_Data (Params.PWM_Meas_SD_Enable);
               View.PWM_Dis_Reg_Stst.Set_Data (Params.PWM_Dis_Reg_Stst);
               View.PWM_Reg.Set_Data (Params.PWM_Reg);
               View.PWM_Lim.Set_Data (Params.PWM_Lim);
         end case;
         View.Enabled.Set_Data (Params.Enabled);
         View.Mm_Per_Step.Set_Data (Params.Mm_Per_Step);
      end Read_Data;

      overriding procedure Save_Data (View : in out Stepper_Widget; Image : out UXString) is
         Params : My_Config.Stepper_Parameters;
      begin
         case My_Config.Stepper_Kinds (View.Stepper) is
            when Basic_Kind =>
               Params := (Kind => Basic_Kind, others => <>);
            when TMC2240_UART_Kind =>
               Params                      := (Kind => TMC2240_UART_Kind, others => <>);
               Params.Output_Current       := View.Output_Current.Get_Data;
               Params.Slope_Control        := View.Slope_Control.Get_Data;
               Params.I_Hold               := View.I_Hold.Get_Data;
               Params.I_Run                := View.I_Run.Get_Data;
               Params.I_Hold_Delay         := View.I_Hold_Delay.Get_Data;
               Params.I_Run_Delay          := View.I_Run_Delay.Get_Data;
               Params.T_Power_Down         := View.T_Power_Down.Get_Data;
               Params.T_PWM_Thrs           := View.T_PWM_Thrs.Get_Data;
               Params.T_Cool_Thrs          := View.T_Cool_Thrs.Get_Data;
               Params.T_High               := View.T_High.Get_Data;
               Params.TOFF                 := View.TOFF.Get_Data;
               Params.HSTRT_TFD210         := View.HSTRT_TFD210.Get_Data;
               Params.HEND_OFFSET          := View.HEND_OFFSET.Get_Data;
               Params.FD3                  := View.FD3.Get_Data;
               Params.DISFDCC              := View.DISFDCC.Get_Data;
               Params.CHM                  := View.CHM.Get_Data;
               Params.VHIGHFS              := View.VHIGHFS.Get_Data;
               Params.VHIGHCHM             := View.VHIGHCHM.Get_Data;
               Params.TPFD                 := View.TPFD.Get_Data;
               Params.Microstep_Resolution := View.Microstep_Resolution.Get_Data;
               Params.PWM_OFS              := View.PWM_OFS.Get_Data;
               Params.PWM_Grad             := View.PWM_Grad.Get_Data;
               Params.PWM_Freq             := View.PWM_Freq.Get_Data;
               Params.PWM_Auto_Scale       := View.PWM_Auto_Scale.Get_Data;
               Params.PWM_Auto_Grad        := View.PWM_Auto_Grad.Get_Data;
               Params.Freewheel            := View.Freewheel.Get_Data;
               Params.PWM_Meas_SD_Enable   := View.PWM_Meas_SD_Enable.Get_Data;
               Params.PWM_Dis_Reg_Stst     := View.PWM_Dis_Reg_Stst.Get_Data;
               Params.PWM_Reg              := View.PWM_Reg.Get_Data;
               Params.PWM_Lim              := View.PWM_Lim.Get_Data;
         end case;
         Params.Enabled     := View.Enabled.Get_Data;
         Params.Mm_Per_Step := View.Mm_Per_Step.Get_Data;

         My_Config.Config_File.Write (Params, View.Stepper);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

      procedure Create_Widget (View : in out Kinematics_Widget; Parent : in out Gnoga.Gui.Base.Base_Type'Class) is
      begin
         Parent_Type (View).Create (Parent => Parent);
         View.Widget_Table.Create (View);
         View.Widget_Table.Style ("border-collapse", "collapse");

         View.Lower_Pos_Limit.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Lower Position Limit",
            Description =>
              "Minimum position that the printer may move to. The E axis may be set to " &
              DF_Image (Length'First / 2.0) & " for effectively infinite range.");

         View.Upper_Pos_Limit.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Upper Position Limit",
            Description =>
              "Maximum position that the printer may move to. The E axis may be set to " &
              DF_Image (Length'Last / 2.0) & " for effectively infinite range.");

         View.Tangential_Velocity_Max.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Max Feedrate",
            Description => "Maximum tangential feedrate. Feedrates higher than this value will be clipped.");

         View.Acceleration_Max.Create
           (Parent => View.Widget_Table, Form => View, Name => "Max Acceleration", Description => "");

         View.Jerk_Max.Create (Parent => View.Widget_Table, Form => View, Name => "Max Jerk", Description => "");

         View.Snap_Max.Create (Parent => View.Widget_Table, Form => View, Name => "Max Snap", Description => "");

         View.Crackle_Max.Create (Parent => View.Widget_Table, Form => View, Name => "Max Crackle", Description => "");

         View.Ignore_E_In_XYZE.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Ignore E In XYZE",
            Description =>
              "Ignore the E axis unless it is the only axis involved in a move. " &
              "This behaviour is the default in some other 3D printer motion controllers.");

         View.Shift_Blended_Corners.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Shift Blended Corners",
            Description =>
              "When blending corners, shift the virtual corners so that the curve intersects the original corner. " &
              "Corners that are too close to the edges of the work area will never be shifted.");

         View.Pressure_Advance_Time.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Pressure Advance Time",
            Description => "The E axis velocity is multiplied by this value and then added to the E axis position.");

         View.Chord_Error_Max.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Max Chord Error",
            Description => "Maximum distance that the path may deviate from the commanded path.");

         View.Axial_Scaler.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Axial Scaler",
            Description =>
              "Inside the motion planner, " &
              "all positions are multiples by this value before applying motion profile limits, " &
              "allowing for different limits on different axes. " &
              "You do not need to take this value in to account when setting position limits or mm per step values. " &
              "Corner deviation and feedrate, acceleration, etc. is based on scaled positions, " &
              "so a tangential feedrate of 10mm/s and a scaler of 0.5 will set the axial limit to 5mm/s.");

         View.Z_Steppers.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Z Steppers",
            Description => "Steppers attached to the Z axis.");

         View.E_Steppers.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "E Steppers",
            Description => "Steppers attached to the E axis.");

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Cartesian_Table.Create (View.Kind_Table);
         View.Cartesian_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Cartesian", View.Cartesian_Table'Access);

         View.X_Steppers.Create
           (Parent      => View.Cartesian_Table,
            Form        => View,
            Name        => "X Steppers",
            Description => "Steppers attached to the X axis.");

         View.Y_Steppers.Create
           (Parent      => View.Cartesian_Table,
            Form        => View,
            Name        => "Y Steppers",
            Description => "Steppers attached to the Y axis.");

         View.Core_XY_Table.Create (View.Kind_Table);
         View.Core_XY_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Core XY", View.Core_XY_Table'Access);

         View.A_Steppers.Create
           (Parent      => View.Core_XY_Table,
            Form        => View,
            Name        => "A Steppers",
            Description => "Steppers attached to the A axis. X axis = 0.5 * (A + B), Y axis = 0.5 * (A – B).");

         View.B_Steppers.Create
           (Parent      => View.Core_XY_Table,
            Form        => View,
            Name        => "B Steppers",
            Description => "Steppers attached to the B axis. X axis = 0.5 * (A + B), Y axis = 0.5 * (A – B).");

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
               View.X_Steppers.Set_Data (Params.X_Steppers);
               View.Y_Steppers.Set_Data (Params.Y_Steppers);
            when My_Config.Core_XY_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Core XY");
               View.A_Steppers.Set_Data (Params.A_Steppers);
               View.B_Steppers.Set_Data (Params.B_Steppers);
         end case;
         View.Lower_Pos_Limit.Set_Data (Params.Planner_Parameters.Lower_Pos_Limit);
         View.Upper_Pos_Limit.Set_Data (Params.Planner_Parameters.Upper_Pos_Limit);
         View.Tangential_Velocity_Max.Set_Data (Params.Planner_Parameters.Tangential_Velocity_Max);
         View.Acceleration_Max.Set_Data (Params.Planner_Parameters.Acceleration_Max);
         View.Jerk_Max.Set_Data (Params.Planner_Parameters.Jerk_Max);
         View.Snap_Max.Set_Data (Params.Planner_Parameters.Snap_Max);
         View.Crackle_Max.Set_Data (Params.Planner_Parameters.Crackle_Max);
         View.Ignore_E_In_XYZE.Set_Data (Params.Planner_Parameters.Ignore_E_In_XYZE);
         View.Shift_Blended_Corners.Set_Data (Params.Planner_Parameters.Shift_Blended_Corners);
         View.Pressure_Advance_Time.Set_Data (Params.Planner_Parameters.Pressure_Advance_Time);
         View.Chord_Error_Max.Set_Data (Params.Planner_Parameters.Chord_Error_Max);
         View.Axial_Scaler.Set_Data (Params.Planner_Parameters.Axial_Scaler);
         View.Z_Steppers.Set_Data (Params.Z_Steppers);
         View.E_Steppers.Set_Data (Params.E_Steppers);
      end Read_Data;

      overriding procedure Save_Data (View : in out Kinematics_Widget; Image : out UXString) is
         Params : My_Config.Kinematics_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Cartesian_Table'Unrestricted_Access then
            Params            := (Kind => My_Config.Cartesian_Kind, others => <>);
            Params.X_Steppers := View.X_Steppers.Get_Data;
            Params.Y_Steppers := View.Y_Steppers.Get_Data;
         elsif View.Kind_Table.Cards.Current_Card = View.Core_XY_Table'Unrestricted_Access then
            Params            := (Kind => My_Config.Core_XY_Kind, others => <>);
            Params.A_Steppers := View.A_Steppers.Get_Data;
            Params.B_Steppers := View.B_Steppers.Get_Data;
         else
            raise Constraint_Error with "Kinematics type must be selected.";
         end if;

         Params.Planner_Parameters.Lower_Pos_Limit         := View.Lower_Pos_Limit.Get_Data;
         Params.Planner_Parameters.Upper_Pos_Limit         := View.Upper_Pos_Limit.Get_Data;
         Params.Planner_Parameters.Tangential_Velocity_Max := View.Tangential_Velocity_Max.Get_Data;
         Params.Planner_Parameters.Acceleration_Max        := View.Acceleration_Max.Get_Data;
         Params.Planner_Parameters.Jerk_Max                := View.Jerk_Max.Get_Data;
         Params.Planner_Parameters.Snap_Max                := View.Snap_Max.Get_Data;
         Params.Planner_Parameters.Crackle_Max             := View.Crackle_Max.Get_Data;
         Params.Planner_Parameters.Ignore_E_In_XYZE        := View.Ignore_E_In_XYZE.Get_Data;
         Params.Planner_Parameters.Shift_Blended_Corners   := View.Shift_Blended_Corners.Get_Data;
         Params.Planner_Parameters.Pressure_Advance_Time   := View.Pressure_Advance_Time.Get_Data;
         Params.Planner_Parameters.Chord_Error_Max         := View.Chord_Error_Max.Get_Data;
         Params.Planner_Parameters.Axial_Scaler            := View.Axial_Scaler.Get_Data;
         Params.Z_Steppers                                 := View.Z_Steppers.Get_Data;
         Params.E_Steppers                                 := View.E_Steppers.Get_Data;

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

         View.Enabled.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Enabled",
            Description => "If set, marks the switch as enabled so it may be used for homing.");

         View.Hit_On_High.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Hit On High",
            Description => "If set, the switch is considered to be hit when the related signal is high.");

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Input_Switch_Widget) is
         Params : My_Config.Input_Switch_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Input_Switch);

         View.Enabled.Set_Data (Params.Enabled);
         View.Hit_On_High.Set_Data (Params.Hit_On_High);
      end Read_Data;

      overriding procedure Save_Data (View : in out Input_Switch_Widget; Image : out UXString) is
         Params : My_Config.Input_Switch_Parameters;
      begin
         Params.Enabled     := View.Enabled.Get_Data;
         Params.Hit_On_High := View.Hit_On_High.Get_Data;

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

         View.Switch.Create
           (Parent      => View.Double_Tap_Table,
            Form        => View,
            Name        => "Switch",
            Description => "The switch used for homing this axis.");

         View.First_Move_Distance.Create
           (Parent      => View.Double_Tap_Table,
            Form        => View,
            Name        => "First Move Distance",
            Description =>
              "The minimum length of the first move, may be negative to home towards negative infinity. " &
              "The axis will move at least this far in total during the first homing move, " &
              "and no further than this far after the switch is hit.");

         View.Back_Off_Move_Distance.Create
           (Parent      => View.Double_Tap_Table,
            Form        => View,
            Name        => "Back-Off Move Distance",
            Description =>
              "The distance to back off after the first move, after the second move, " &
              "and before the first move iff the switch is hit before the first move." &
              "If the move after the second move would place the axis outside of the work area then the axis will " &
              "instead move to the closest face of the work area to the desired position.");

         View.Second_Move_Distance.Create
           (Parent      => View.Double_Tap_Table,
            Form        => View,
            Name        => "Second Move Distance",
            Description =>
              "The minimum length of the second move, may be negative to home towards negative infinity, " &
              "must be the same sign as the first distance. " & "The axis will move at least this far in total, " &
              "and no further than this far after the switch is hit.");

         View.Switch_Position.Create
           (Parent      => View.Double_Tap_Table,
            Form        => View,
            Name        => "Switch Position",
            Description =>
              "The position that the axis is considered to be at when the switch is hit during the second move." &
              "This position does not need to be inside the working area. " &
              "If it is outside then the axis will move back in to the working area after homing.");

         View.Set_To_Value_Table.Create (View.Kind_Table);
         View.Set_To_Value_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Set To Value", View.Set_To_Value_Table'Access);

         View.Value.Create
           (Parent      => View.Set_To_Value_Table,
            Form        => View,
            Name        => "Value",
            Description =>
              "The value the axis is assumed to be at when the axis is homed. " &
              "No homing move will occur in this mode.");

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
               View.Switch.Set_Data (Params.Switch);
               View.First_Move_Distance.Set_Data (Params.First_Move_Distance);
               View.Back_Off_Move_Distance.Set_Data (Params.Back_Off_Move_Distance);
               View.Second_Move_Distance.Set_Data (Params.Second_Move_Distance);
               View.Switch_Position.Set_Data (Params.Switch_Position);
            when My_Config.Set_To_Value_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Set To Value");
               View.Value.Set_Data (Params.Value);
         end case;
      end Read_Data;

      overriding procedure Save_Data (View : in out Homing_Widget; Image : out UXString) is
         Params : My_Config.Homing_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Double_Tap_Table'Unrestricted_Access then
            Params                        := (Kind => My_Config.Double_Tap_Kind, others => <>);
            Params.Switch                 := View.Switch.Get_Data;
            Params.First_Move_Distance    := View.First_Move_Distance.Get_Data;
            Params.Back_Off_Move_Distance := View.Back_Off_Move_Distance.Get_Data;
            Params.Second_Move_Distance   := View.Second_Move_Distance.Get_Data;
            Params.Switch_Position        := View.Switch_Position.Get_Data;
         elsif View.Kind_Table.Cards.Current_Card = View.Set_To_Value_Table'Unrestricted_Access then
            Params       := (Kind => My_Config.Set_To_Value_Kind, others => <>);
            Params.Value := View.Value.Get_Data;
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

         View.Nozzle_Diameter.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Nozzle Diameter",
            Description => "The diameter of the nozzle.");

         View.Filament_Diameter.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Filament Diameter",
            Description => "The diameter of the filament.");

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Extruder_Widget) is
         Params : My_Config.Extruder_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         View.Nozzle_Diameter.Set_Data (Params.Nozzle_Diameter);
         View.Filament_Diameter.Set_Data (Params.Filament_Diameter);
      end Read_Data;

      overriding procedure Save_Data (View : in out Extruder_Widget; Image : out UXString) is
         Params : My_Config.Extruder_Parameters;
      begin
         Params.Nozzle_Diameter   := View.Nozzle_Diameter.Get_Data;
         Params.Filament_Diameter := View.Filament_Diameter.Get_Data;

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

         View.Minimum_Temperature.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => UXStrings.From_UTF_8 ("Minimum Temperature"),
            Description => "Any temperature below this value indicates a failure.");

         View.Maximum_Temperature.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => UXStrings.From_UTF_8 ("Maximum Temperature"),
            Description => "Any temperature above this value indicates a failure.");

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Disabled_Table.Create (View.Kind_Table);
         View.Disabled_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Disabled", View.Disabled_Table'Access);

         View.Steinhart_Hart_Table.Create (View.Kind_Table);
         View.Steinhart_Hart_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Steinhart-Hart", View.Steinhart_Hart_Table'Access);

         View.SH_A.Create
           (Parent      => View.Steinhart_Hart_Table,
            Form        => View,
            Name        => UXStrings.From_UTF_8 ("Coefficient A"),
            Description => "In 1 / T = A + B * ln (R) + C * (ln (R))**3 where T is in kelvins and R is in ohms.");

         View.SH_B.Create
           (Parent      => View.Steinhart_Hart_Table,
            Form        => View,
            Name        => UXStrings.From_UTF_8 ("Coefficient B"),
            Description => "In 1 / T = A + B * ln (R) + C * (ln (R))**3 where T is in kelvins and R is in ohms.");

         View.SH_C.Create
           (Parent      => View.Steinhart_Hart_Table,
            Form        => View,
            Name        => UXStrings.From_UTF_8 ("Coefficient C"),
            Description => "In 1 / T = A + B * ln (R) + C * (ln (R))**3 where T is in kelvins and R is in ohms.");

         View.Callendar_Van_Dusen_Table.Create (View.Kind_Table);
         View.Callendar_Van_Dusen_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Callendar-Van Dusen", View.Callendar_Van_Dusen_Table'Access);

         View.CVD_R0.Create
           (Parent      => View.Callendar_Van_Dusen_Table,
            Form        => View,
            Name        => UXStrings.From_UTF_8 ("Resistance at 0C"),
            Description => "");

         View.CVD_A.Create
           (Parent      => View.Callendar_Van_Dusen_Table,
            Form        => View,
            Name        => UXStrings.From_UTF_8 ("Coefficient A"),
            Description => "In R (0) * (1 + A * T + B * T**2) where T is in celcius.");

         View.CVD_B.Create
           (Parent      => View.Callendar_Van_Dusen_Table,
            Form        => View,
            Name        => UXStrings.From_UTF_8 ("Coefficient B"),
            Description => "In R (0) * (1 + A * T + B * T**2) where T is in celcius.");

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
               View.SH_A.Set_Data (Params.SH_A);
               View.SH_B.Set_Data (Params.SH_B);
               View.SH_C.Set_Data (Params.SH_C);
            when Callendar_Van_Dusen_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Callendar-Van Dusen");
               View.CVD_R0.Set_Data (Params.CVD_R0);
               View.CVD_A.Set_Data (Params.CVD_A);
               View.CVD_B.Set_Data (Params.CVD_B);
         end case;
         View.Minimum_Temperature.Set_Data (Params.Minimum_Temperature);
         View.Maximum_Temperature.Set_Data (Params.Maximum_Temperature);
      end Read_Data;

      overriding procedure Save_Data (View : in out Thermistor_Widget; Image : out UXString) is
         Params : Thermistor_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Disabled_Table'Unrestricted_Access then
            Params := (Kind => Disabled_Kind, others => <>);
         elsif View.Kind_Table.Cards.Current_Card = View.Steinhart_Hart_Table'Unrestricted_Access then
            Params      := (Kind => Steinhart_Hart_Kind, others => <>);
            Params.SH_A := View.SH_A.Get_Data;
            Params.SH_B := View.SH_B.Get_Data;
            Params.SH_C := View.SH_C.Get_Data;
         elsif View.Kind_Table.Cards.Current_Card = View.Callendar_Van_Dusen_Table'Unrestricted_Access then
            Params        := (Kind => Callendar_Van_Dusen_Kind, others => <>);
            Params.CVD_R0 := View.CVD_R0.Get_Data;
            Params.CVD_A  := View.CVD_A.Get_Data;
            Params.CVD_B  := View.CVD_B.Get_Data;
         else
            raise Constraint_Error with "Thermistor type must be selected";
         end if;
         Params.Minimum_Temperature := View.Minimum_Temperature.Get_Data;
         Params.Maximum_Temperature := View.Maximum_Temperature.Get_Data;

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

         View.Thermistor.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Thermistor",
            Description => "Thermistor associated with this heater.");

         View.Max_Cumulative_Error.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Max Cumulative Error",
            Description => "Maximum cumulative error before a failure is detected.");

         View.Check_Gain_Time.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Check Gain Time",
            Description => "Period to check for temperature rise over during heating to detect failures.");

         View.Check_Minimum_Gain.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Check Minimum Gain",
            Description => "Minium temperature rise required in gain period to reset cumulative error.");

         View.Hysteresis.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Hysteresis",
            Description =>
              "Maximum temperature below or above the setpoint where " &
              "the heater is considered to be at temperature. " &
              "Also used bang-bang mode to determine when to switch on and off.");

         View.Kind_Table.Create (View);
         View.Kind_Table.Style ("width", "100%");
         View.Kind_Table.Place_After (View.Widget_Table);

         View.Disabled_Table.Create (View.Kind_Table);
         View.Disabled_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Disabled", View.Disabled_Table'Access);

         View.PID_Table.Create (View.Kind_Table);
         View.PID_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("PID", View.PID_Table'Access);

         View.Proportional_Scale.Create
           (Parent      => View.PID_Table,
            Form        => View,
            Name        => "Proportional Scale",
            Description => "Coefficient for the proportional term.");

         View.Integral_Scale.Create
           (Parent      => View.PID_Table,
            Form        => View,
            Name        => "Integral Scale",
            Description => "Coefficient for the integral term.");

         View.Derivative_Scale.Create
           (Parent      => View.PID_Table,
            Form        => View,
            Name        => "Derivative Scale",
            Description => "Coefficient for the derivative term.");

         View.Bang_Bang_Table.Create (View.Kind_Table);
         View.Bang_Bang_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Bang Bang", View.Bang_Bang_Table'Access);

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Kind_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out Heater_Widget) is
         Params : My_Config.Heater_Full_Parameters;
      begin
         My_Config.Config_File.Read (Params, View.Heater);

         case Params.Params.Kind is
            when Heaters.Disabled_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Disabled");
            when Heaters.PID_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("PID");
               View.Proportional_Scale.Set_Data (Params.Params.Proportional_Scale);
               View.Integral_Scale.Set_Data (Params.Params.Integral_Scale);
               View.Derivative_Scale.Set_Data (Params.Params.Derivative_Scale);
            when Heaters.Bang_Bang_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Bang Bang");
         end case;
         View.Thermistor.Set_Data (Params.Thermistor);
         View.Max_Cumulative_Error.Set_Data (Params.Params.Max_Cumulative_Error);
         View.Check_Gain_Time.Set_Data (Params.Params.Check_Gain_Time);
         View.Check_Minimum_Gain.Set_Data (Params.Params.Check_Minimum_Gain);
         View.Hysteresis.Set_Data (Params.Params.Hysteresis);
      end Read_Data;

      overriding procedure Save_Data (View : in out Heater_Widget; Image : out UXString) is
         Params : My_Config.Heater_Full_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Disabled_Table'Unrestricted_Access then
            Params := (Params => (Kind => Heaters.Disabled_Kind, others => <>), others => <>);
         elsif View.Kind_Table.Cards.Current_Card = View.PID_Table'Unrestricted_Access then
            Params                           := (Params => (Kind => Heaters.PID_Kind, others => <>), others => <>);
            Params.Params.Proportional_Scale := View.Proportional_Scale.Get_Data;
            Params.Params.Integral_Scale     := View.Integral_Scale.Get_Data;
            Params.Params.Derivative_Scale   := View.Derivative_Scale.Get_Data;
         elsif View.Kind_Table.Cards.Current_Card = View.Bang_Bang_Table'Unrestricted_Access then
            Params := (Params => (Kind => Heaters.Bang_Bang_Kind, others => <>), others => <>);
         else
            raise Constraint_Error with "Heater type must be selected.";
         end if;

         Params.Thermistor                  := View.Thermistor.Get_Data;
         Params.Params.Max_Cumulative_Error := View.Max_Cumulative_Error.Get_Data;
         Params.Params.Check_Gain_Time      := View.Check_Gain_Time.Get_Data;
         Params.Params.Check_Minimum_Gain   := View.Check_Minimum_Gain.Get_Data;
         Params.Params.Hysteresis           := View.Hysteresis.Get_Data;

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

         View.Serial_Port_Path.Create
           (Parent      => View.Beacon_Table,
            Form        => View,
            Name        => "Serial Port Path",
            Description => "Path to the Beacon serial port.");

         View.X_Offset.Create
           (Parent      => View.Beacon_Table,
            Form        => View,
            Name        => "X Offset",
            Description => "Offset along the X axis of the probe to the nozzle.");

         View.Y_Offset.Create
           (Parent      => View.Beacon_Table,
            Form        => View,
            Name        => "Y Offset",
            Description => "Offset along the Y axis of the probe to the nozzle.");

         View.Calibration_Floor.Create
           (Parent      => View.Beacon_Table,
            Form        => View,
            Name        => "Calibration Floor",
            Description => "Z axis value to use for lowest calibration point.");

         View.Calibration_Ceiling.Create
           (Parent      => View.Beacon_Table,
            Form        => View,
            Name        => "Calibration Ceiling",
            Description => "Z axis value to use for highest calibration point.");

         View.Calibration_Feedrate.Create
           (Parent      => View.Beacon_Table,
            Form        => View,
            Name        => "Calibration Feedrate",
            Description => "Feedrate used for calibration sequence.");

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
               View.Serial_Port_Path.Set_Data (Params.Serial_Port_Path);
               View.X_Offset.Set_Data (Params.X_Offset);
               View.Y_Offset.Set_Data (Params.Y_Offset);
               View.Calibration_Floor.Set_Data (Params.Calibration_Floor);
               View.Calibration_Ceiling.Set_Data (Params.Calibration_Ceiling);
               View.Calibration_Feedrate.Set_Data (Params.Calibration_Feedrate);
         end case;
      end Read_Data;

      overriding procedure Save_Data (View : in out Bed_Mesh_Widget; Image : out UXString) is
         Params : My_Config.Bed_Mesh_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.No_Mesh_Table'Unrestricted_Access then
            Params := (Kind => My_Config.No_Mesh_Kind);
         elsif View.Kind_Table.Cards.Current_Card = View.Beacon_Table'Unrestricted_Access then
            Params                      := (Kind => My_Config.Beacon_Kind, others => <>);
            Params.Serial_Port_Path     := View.Serial_Port_Path.Get_Data;
            Params.X_Offset             := View.X_Offset.Get_Data;
            Params.Y_Offset             := View.Y_Offset.Get_Data;
            Params.Calibration_Floor    := View.Calibration_Floor.Get_Data;
            Params.Calibration_Ceiling  := View.Calibration_Ceiling.Get_Data;
            Params.Calibration_Feedrate := View.Calibration_Feedrate.Get_Data;
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

         View.Disable_Below_PWM.Create
           (Parent      => View.Dynamic_PWM_Table,
            Form        => View,
            Name        => "Disable Below PWM",
            Description => "Any set PWM ratio below this value will shut off power to the fan.");

         View.Max_PWM.Create
           (Parent      => View.Dynamic_PWM_Table,
            Form        => View,
            Name        => "Max PWM",
            Description =>
              "The maximum PWM ratio of the fan, corresponding to 100% power setting in g-code or the UI.");

         View.Always_On_Table.Create (View.Kind_Table);
         View.Always_On_Table.Style ("border-collapse", "collapse");
         View.Kind_Table.Add_Tab ("Always On", View.Always_On_Table'Access);

         View.Always_On_PWM.Create
           (Parent      => View.Always_On_Table,
            Form        => View,
            Name        => "Always On PWM",
            Description => "The constant PWM ratio to output.");

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
               View.Disable_Below_PWM.Set_Data (Params.Disable_Below_PWM);
               View.Max_PWM.Set_Data (Params.Max_PWM);
            when My_Config.Always_On_Kind =>
               View.Kind_Table.Tabs.Select_Tab ("Always On");
               View.Always_On_PWM.Set_Data (Params.Always_On_PWM);
         end case;
      end Read_Data;

      overriding procedure Save_Data (View : in out Fan_Widget; Image : out UXString) is
         Params : My_Config.Fan_Parameters;
      begin
         if View.Kind_Table.Cards.Current_Card = View.Disabled_Table'Unrestricted_Access then
            Params := (Kind => My_Config.Disabled_Kind);
         elsif View.Kind_Table.Cards.Current_Card = View.Dynamic_PWM_Table'Unrestricted_Access then
            Params                   := (Kind => My_Config.Dynamic_PWM_Kind, others => <>);
            Params.Disable_Below_PWM := View.Disable_Below_PWM.Get_Data;
            Params.Max_PWM           := View.Max_PWM.Get_Data;
         elsif View.Kind_Table.Cards.Current_Card = View.Always_On_Table'Unrestricted_Access then
            Params               := (Kind => My_Config.Always_On_Kind, others => <>);
            Params.Always_On_PWM := View.Always_On_PWM.Get_Data;
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

         View.Bed_Heater.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Bed Heater",
            Description => "The heater assigned to the bed in g-code.");

         --  View.Chamber_Heater.Create
         --    (Parent      => View.Widget_Table,
         --     Form        => View,
         --     Name        => "Chamber_Heater",
         --     Description => "The heater assigned to the chamber in g-code.");

         View.Hotend_Heater.Create
           (Parent      => View.Widget_Table,
            Form        => View,
            Name        => "Hotend Heater",
            Description => "The heater assigned to the hotend in g-code.");

         View.Read_Data;

         View.On_Submit_Handler (Outer_Section_Widgets.On_Submit'Unrestricted_Access);
         View.Submit_Button.Create (Form => View, Value => "Save");
         View.Submit_Button.Place_After (View.Widget_Table);
      end Create_Widget;

      overriding procedure Read_Data (View : in out G_Code_Assignment_Widget) is
         Params : My_Config.G_Code_Assignment_Parameters;
      begin
         My_Config.Config_File.Read (Params);

         View.Bed_Heater.Set_Data (Params.Bed_Heater);
         --  View.Chamber_Heater.Set_Data (Params.Chamber_Heater);
         View.Hotend_Heater.Set_Data (Params.Hotend_Heater);
      end Read_Data;

      overriding procedure Save_Data (View : in out G_Code_Assignment_Widget; Image : out UXString) is
         Params : My_Config.G_Code_Assignment_Parameters;
      begin
         Params.Bed_Heater    := View.Bed_Heater.Get_Data;
         --  Params.Chamber_Heater := View.Chamber_Heater.Get_Data;
         Params.Hotend_Heater := View.Hotend_Heater.Get_Data;

         My_Config.Config_File.Write (Params);

         Image := UXStrings.From_UTF_8 (Params'Image);
      end Save_Data;

   end Section_Widgets;

end Prunt.GUI.Config_Editor;
