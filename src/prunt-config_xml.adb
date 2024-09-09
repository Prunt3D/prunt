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

with Ada.Directories;
with Ada.Text_IO;
with GNAT.String_Split;
with DOM.Core.Nodes;
with Ada.Strings;
with Ada.Strings.Fixed;
with Input_Sources.File;
with Input_Sources.Strings;
with Schema.Schema_Readers;
with Unicode.CES.Utf8;
with Sax.Readers;
with Sax.Utils;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;

package body Prunt.Config_XML is

   pragma Unsuppress (All_Checks);

   protected body Config_File is

      function Get_Schema return Unbounded_String is
      begin
         return Config_Schema_String;
      end Get_Schema;

      function Read_File return Unbounded_String is
      begin
         return To_Unbounded_String ("");
      end Read_File;

      procedure Replace_File (Data : Unbounded_String) is
      begin
         null;
      end Replace_File;

      procedure Read (Data : out Prunt_Parameters) is
      begin
         Maybe_Read_Tree;

         Data := (Enabled => Boolean'Value (Get_Node ("Prunt/Enabled")));
      end Read;

      procedure Read (Data : out Stepper_Parameters; Stepper : Stepper_Name) is
         Path_Start : constant String := "Steppers/" & Ada.Strings.Fixed.Trim (Stepper'Image, Ada.Strings.Both) & "/";
      begin
         Maybe_Read_Tree;

         case Stepper_Kinds (Stepper) is
            when Basic_Kind =>
               Data :=
                 (Kind => Basic_Kind,
                  Enabled => Boolean'Value (Get_Node (Path_Start & "Enabled")),
                  Mm_Per_Step => Length'Value (Get_Node (Path_Start & "Mm_Per_Step")));
            when TMC2240_UART_Kind =>
               Data :=
                 (Kind => TMC2240_UART_Kind,
                  Enabled => Boolean'Value (Get_Node (Path_Start & "Enabled")),
                  Mm_Per_Step => Length'Value (Get_Node (Path_Start & "Mm_Per_Step")),
                  Output_Current => Current'Value (Get_Node (Path_Start & "Output_Current")),
                  Slope_Control =>
                    TMC_Types.TMC2240.Slope_Control_Type'Value (Get_Node (Path_Start & "Slope_Control")),
                  I_Hold => TMC_Types.Unsigned_5'Value (Get_Node (Path_Start & "I_Hold")),
                  I_Run => TMC_Types.Unsigned_5'Value (Get_Node (Path_Start & "I_Run")),
                  I_Hold_Delay => TMC_Types.Unsigned_4'Value (Get_Node (Path_Start & "I_Hold_Delay")),
                  I_Run_Delay => TMC_Types.Unsigned_4'Value (Get_Node (Path_Start & "I_Run_Delay")),
                  T_Power_Down => TMC_Types.Unsigned_8'Value (Get_Node (Path_Start & "T_Power_Down")),
                  T_PWM_Thrs => TMC_Types.Unsigned_20'Value (Get_Node (Path_Start & "T_PWM_Thrs")),
                  T_Cool_Thrs => TMC_Types.Unsigned_20'Value (Get_Node (Path_Start & "T_Cool_Thrs")),
                  T_High => TMC_Types.Unsigned_20'Value (Get_Node (Path_Start & "T_High")),
                  TOFF => TMC_Types.Unsigned_4'Value (Get_Node (Path_Start & "TOFF")),
                  HSTRT_TFD210 => TMC_Types.Unsigned_3'Value (Get_Node (Path_Start & "HSTRT_TFD210")),
                  HEND_OFFSET => TMC_Types.Unsigned_4'Value (Get_Node (Path_Start & "HEND_OFFSET")),
                  FD3 => TMC_Types.Unsigned_1'Value (Get_Node (Path_Start & "FD3")),
                  DISFDCC => Boolean'Value (Get_Node (Path_Start & "DISFDCC")),
                  CHM => Boolean'Value (Get_Node (Path_Start & "CHM")),
                  VHIGHFS => Boolean'Value (Get_Node (Path_Start & "VHIGHFS")),
                  VHIGHCHM => Boolean'Value (Get_Node (Path_Start & "VHIGHCHM")),
                  TPFD => TMC_Types.Unsigned_4'Value (Get_Node (Path_Start & "TPFD")),
                  Microstep_Resolution =>
                    TMC_Types.TMC2240.Microstep_Resolution_Type'Value (Get_Node (Path_Start & "Microstep_Resolution")),
                  PWM_OFS => TMC_Types.Unsigned_8'Value (Get_Node (Path_Start & "PWM_OFS")),
                  PWM_Grad => TMC_Types.Unsigned_8'Value (Get_Node (Path_Start & "PWM_Grad")),
                  PWM_Freq => TMC_Types.Unsigned_2'Value (Get_Node (Path_Start & "PWM_Freq")),
                  PWM_Auto_Scale => Boolean'Value (Get_Node (Path_Start & "PWM_Auto_Scale")),
                  PWM_Auto_Grad => Boolean'Value (Get_Node (Path_Start & "PWM_Auto_Grad")),
                  Freewheel => TMC_Types.TMC2240.Freewheel_Type'Value (Get_Node (Path_Start & "Freewheel")),
                  PWM_Meas_SD_Enable => Boolean'Value (Get_Node (Path_Start & "PWM_Meas_SD_Enable")),
                  PWM_Dis_Reg_Stst => Boolean'Value (Get_Node (Path_Start & "PWM_Dis_Reg_Stst")),
                  PWM_Reg => TMC_Types.Unsigned_4'Value (Get_Node (Path_Start & "PWM_Reg")),
                  PWM_Lim => TMC_Types.Unsigned_4'Value (Get_Node (Path_Start & "PWM_Lim")));
         end case;
      end Read;

      procedure Read (Data : out Kinematics_Parameters) is
      begin
         Maybe_Read_Tree;

         if Node_Exists ("Kinematics/Cartesian") then
            Data :=
              (Kind => Cartesian_Kind,
               Planner_Parameters =>
                 (Lower_Pos_Limit =>
                    (X_Axis => Length'Value (Get_Node ("Kinematics/Lower_Pos_Limit/X_Axis")),
                     Y_Axis => Length'Value (Get_Node ("Kinematics/Lower_Pos_Limit/Y_Axis")),
                     Z_Axis => Length'Value (Get_Node ("Kinematics/Lower_Pos_Limit/Z_Axis")),
                     E_Axis => Length'Value (Get_Node ("Kinematics/Lower_Pos_Limit/E_Axis"))),
                  Upper_Pos_Limit =>
                    (X_Axis => Length'Value (Get_Node ("Kinematics/Upper_Pos_Limit/X_Axis")),
                     Y_Axis => Length'Value (Get_Node ("Kinematics/Upper_Pos_Limit/Y_Axis")),
                     Z_Axis => Length'Value (Get_Node ("Kinematics/Upper_Pos_Limit/Z_Axis")),
                     E_Axis => Length'Value (Get_Node ("Kinematics/Upper_Pos_Limit/E_Axis"))),
                  Ignore_E_In_XYZE => Boolean'Value (Get_Node ("Kinematics/Ignore_E_In_XYZE")),
                  Shift_Blended_Corners => Boolean'Value (Get_Node ("Kinematics/Shift_Blended_Corners")),
                  Tangential_Velocity_Max => Velocity'Value (Get_Node ("Kinematics/Tangential_Velocity_Max")),
                  Axial_Velocity_Maxes =>
                    (X_Axis => Velocity'Value (Get_Node ("Kinematics/Axial_Velocity_Maxes/X_Axis")),
                     Y_Axis => Velocity'Value (Get_Node ("Kinematics/Axial_Velocity_Maxes/Y_Axis")),
                     Z_Axis => Velocity'Value (Get_Node ("Kinematics/Axial_Velocity_Maxes/Z_Axis")),
                     E_Axis => Velocity'Value (Get_Node ("Kinematics/Axial_Velocity_Maxes/E_Axis"))),
                  Pressure_Advance_Time => Time'Value (Get_Node ("Kinematics/Pressure_Advance_Time")),
                  Acceleration_Max => Acceleration'Value (Get_Node ("Kinematics/Acceleration_Max")),
                  Jerk_Max => Jerk'Value (Get_Node ("Kinematics/Jerk_Max")),
                  Snap_Max => Snap'Value (Get_Node ("Kinematics/Snap_Max")),
                  Crackle_Max => Crackle'Value (Get_Node ("Kinematics/Crackle")),
                  Chord_Error_Max => Length'Value (Get_Node ("Kinematics/Chord_Error_Max")),
                  Axial_Scaler =>
                    (X_Axis => Dimensionless'Value (Get_Node ("Kinematics/Axial_Scaler/X_Axis")),
                     Y_Axis => Dimensionless'Value (Get_Node ("Kinematics/Axial_Scaler/Y_Axis")),
                     Z_Axis => Dimensionless'Value (Get_Node ("Kinematics/Axial_Scaler/Z_Axis")),
                     E_Axis => Dimensionless'Value (Get_Node ("Kinematics/Axial_Scaler/E_Axis")))),
               Z_Steppers => (for S in Stepper_Name =>
                                Get_Node ("Kinematics/Cartesian/" &
                                            Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both)) = "Z_Axis"),
               E_Steppers => (for S in Stepper_Name =>
                                Get_Node ("Kinematics/Cartesian/" &
                                            Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both)) = "E_Axis"),
               X_Steppers => (for S in Stepper_Name =>
                                Get_Node ("Kinematics/Cartesian/" &
                                            Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both)) = "X_Axis"),
               Y_Steppers => (for S in Stepper_Name =>
                                Get_Node ("Kinematics/Cartesian/" &
                                            Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both)) = "Y_Axis"));
         elsif Node_Exists ("Kinematics/Core_XY") then
            Data :=
              (Kind => Core_XY_Kind,
               Planner_Parameters =>
                 (Lower_Pos_Limit =>
                    (X_Axis => Length'Value (Get_Node ("Kinematics/Lower_Pos_Limit/X_Axis")),
                     Y_Axis => Length'Value (Get_Node ("Kinematics/Lower_Pos_Limit/Y_Axis")),
                     Z_Axis => Length'Value (Get_Node ("Kinematics/Lower_Pos_Limit/Z_Axis")),
                     E_Axis => Length'Value (Get_Node ("Kinematics/Lower_Pos_Limit/E_Axis"))),
                  Upper_Pos_Limit =>
                    (X_Axis => Length'Value (Get_Node ("Kinematics/Upper_Pos_Limit/X_Axis")),
                     Y_Axis => Length'Value (Get_Node ("Kinematics/Upper_Pos_Limit/Y_Axis")),
                     Z_Axis => Length'Value (Get_Node ("Kinematics/Upper_Pos_Limit/Z_Axis")),
                     E_Axis => Length'Value (Get_Node ("Kinematics/Upper_Pos_Limit/E_Axis"))),
                  Ignore_E_In_XYZE => Boolean'Value (Get_Node ("Kinematics/Ignore_E_In_XYZE")),
                  Shift_Blended_Corners => Boolean'Value (Get_Node ("Kinematics/Shift_Blended_Corners")),
                  Tangential_Velocity_Max => Velocity'Value (Get_Node ("Kinematics/Tangential_Velocity_Max")),
                  Axial_Velocity_Maxes =>
                    (X_Axis => Velocity'Value (Get_Node ("Kinematics/Axial_Velocity_Maxes/X_Axis")),
                     Y_Axis => Velocity'Value (Get_Node ("Kinematics/Axial_Velocity_Maxes/Y_Axis")),
                     Z_Axis => Velocity'Value (Get_Node ("Kinematics/Axial_Velocity_Maxes/Z_Axis")),
                     E_Axis => Velocity'Value (Get_Node ("Kinematics/Axial_Velocity_Maxes/E_Axis"))),
                  Pressure_Advance_Time => Time'Value (Get_Node ("Kinematics/Pressure_Advance_Time")),
                  Acceleration_Max => Acceleration'Value (Get_Node ("Kinematics/Acceleration_Max")),
                  Jerk_Max => Jerk'Value (Get_Node ("Kinematics/Jerk_Max")),
                  Snap_Max => Snap'Value (Get_Node ("Kinematics/Snap_Max")),
                  Crackle_Max => Crackle'Value (Get_Node ("Kinematics/Crackle")),
                  Chord_Error_Max => Length'Value (Get_Node ("Kinematics/Chord_Error_Max")),
                  Axial_Scaler =>
                    (X_Axis => Dimensionless'Value (Get_Node ("Kinematics/Axial_Scaler/X_Axis")),
                     Y_Axis => Dimensionless'Value (Get_Node ("Kinematics/Axial_Scaler/Y_Axis")),
                     Z_Axis => Dimensionless'Value (Get_Node ("Kinematics/Axial_Scaler/Z_Axis")),
                     E_Axis => Dimensionless'Value (Get_Node ("Kinematics/Axial_Scaler/E_Axis")))),
               Z_Steppers => (for S in Stepper_Name =>
                                Get_Node ("Kinematics/Core_XY/" &
                                            Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both)) = "Z_Axis"),
               E_Steppers => (for S in Stepper_Name =>
                                Get_Node ("Kinematics/Core_XY/" &
                                            Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both)) = "E_Axis"),
               A_Steppers => (for S in Stepper_Name =>
                                Get_Node ("Kinematics/Core_XY/" &
                                            Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both)) = "A_Axis"),
               B_Steppers => (for S in Stepper_Name =>
                                Get_Node ("Kinematics/Core_XY/" &
                                            Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both)) = "B_Axis"));
         else
            raise Constraint_Error with "Not implemented.";
         end if;
      end Read;

      procedure Read (Data : out Input_Switch_Parameters; Input_Switch : Input_Switch_Name) is
         Path_Start : constant String :=
           "Input_Switches/" & Ada.Strings.Fixed.Trim (Input_Switch'Image, Ada.Strings.Both) & "/";
      begin
         Maybe_Read_Tree;

         Data :=
           (Enabled     => Boolean'Value (Get_Node (Path_Start & "Enabled")),
            Hit_On_High => Boolean'Value (Get_Node (Path_Start & "/Hit_On_High")));
      end Read;

      procedure Read (Data : out Homing_Parameters; Axis : Axis_Name) is
         Path_Start : constant String := "Homing/" & Ada.Strings.Fixed.Trim (Axis'Image, Ada.Strings.Both) & "/";
      begin
         Maybe_Read_Tree;

         if Node_Exists (Path_Start & "Double_Tap") then
            Data :=
              (Kind                 => Double_Tap_Kind,
               Switch               => Input_Switch_Name'Value (Get_Node (Path_Start & "Double_Tap/Switch")),
               First_Move_Distance => Length'Value (Get_Node (Path_Start & "Double_Tap/First_Move_Distance")),
               Second_Move_Distance =>
                 Length'Value (Get_Node (Path_Start & "Double_Tap/Second_Move_Distance")),
               Switch_Position      => Length'Value (Get_Node (Path_Start & "Double_Tap/Switch_Position")));
         elsif Node_Exists (Path_Start & "Set_To_Value") then
            Data :=
              (Kind  => Set_To_Value_Kind,
               Value => Length'Value (Get_Node (Path_Start & "Set_To_Value/Value")));
         else
            raise Constraint_Error with "Not implemented.";
         end if;
      end Read;

      procedure Read (Data : out Thermistor_Parameters; Thermistor : Thermistor_Name) is
         Path_Start : constant String := "Thermistors/" & Ada.Strings.Fixed.Trim (Thermistor'Image, Ada.Strings.Both) & "/";
      begin
         Maybe_Read_Tree;

      end Read;

      procedure Read (Data : out Heater_Full_Parameters; Heater : Heater_Name) is
         Path_Start : constant String := "Heaters/" & Ada.Strings.Fixed.Trim (Heater'Image, Ada.Strings.Both) & "/";
      begin
         Maybe_Read_Tree;

      end Read;

      procedure Read (Data : out Fan_Parameters; Fan : Fan_Name) is
      begin
         Maybe_Read_Tree;

         null;
      end Read;

      procedure Read (Data : out G_Code_Assignment_Parameters) is
      begin
         Maybe_Read_Tree;

         null;
      end Read;

      procedure Maybe_Read_Tree is
         use type DOM.Core.Document;
      begin
         if Schema.Dom_Readers.Get_Tree (XML_Reader) /= null then
            return;
         end if;

         if not Ada.Directories.Exists (Config_Path) then
            Generate_Initial_File;
         end if;

         --  TODO: Migrations go here once we have multiple schema versions as the following validation will fail if
         --  we try to do it with the wrong version.

         declare
            Input : Input_Sources.File.File_Input;
         begin
            Input_Sources.File.Open (Config_Path, Input);
            Schema.Dom_Readers.Set_Warnings_As_Errors (XML_Reader, True);
            Schema.Dom_Readers.Set_Symbol_Table
              (XML_Reader, Schema.Validators.Get_Symbol_Table (Config_Schema_Grammar));
            Schema.Dom_Readers.Set_Grammar (XML_Reader, Config_Schema_Grammar);
            Schema.Dom_Readers.Set_Feature (XML_Reader, Sax.Readers.Schema_Validation_Feature, True);
            Schema.Dom_Readers.Set_Feature (XML_Reader, Sax.Readers.Validation_Feature, False);
            Schema.Dom_Readers.Parse (XML_Reader, Input);
            Input_Sources.File.Close (Input);
         end;

      end Maybe_Read_Tree;

      function Get_Node (Path : DOM.Core.DOM_String) return DOM.Core.DOM_String is
         use type DOM.Core.Node;
         Current_Node : DOM.Core.Node := XML_Reader.Get_Tree;
      begin
         for Next_Node_Name of GNAT.String_Split.Create ("Config/" & Path, "/") loop
            declare
               Current_Node_List : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Current_Node);
            begin
               Current_Node := null;
               for N in 0 .. DOM.Core.Nodes.Length (Current_Node_List) - 1 loop
                  if Ada.Characters.Handling.To_Lower
                      (DOM.Core.Nodes.Local_Name (DOM.Core.Nodes.Item (Current_Node_List, N))) =
                    Ada.Characters.Handling.To_Lower (Next_Node_Name)
                  then
                     if Current_Node /= null then
                        raise Constraint_Error
                          with "Duplicate node name: " &
                          DOM.Core.Nodes.Local_Name (DOM.Core.Nodes.Item (Current_Node_List, N));
                     else
                        Current_Node := DOM.Core.Nodes.Item (Current_Node_List, N);
                     end if;
                  end if;
               end loop;
               if Current_Node = null then
                  raise Constraint_Error with "Node not found: " & Next_Node_Name;
               end if;
            end;
         end loop;

         if DOM.Core.Nodes.Length (DOM.Core.Nodes.Child_Nodes (Current_Node)) /= 1 then
            raise Constraint_Error with "Expected single child node.";
         end if;

         return DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.Item (DOM.Core.Nodes.Child_Nodes (Current_Node), 0));
      end Get_Node;

      function Node_Exists (Path : DOM.Core.DOM_String) return Boolean is
         use type DOM.Core.Node;
         Current_Node : DOM.Core.Node := XML_Reader.Get_Tree;
      begin
         for Next_Node_Name of GNAT.String_Split.Create ("Config/" & Path, "/") loop
            declare
               Current_Node_List : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Current_Node);
            begin
               Current_Node := null;
               for N in 0 .. DOM.Core.Nodes.Length (Current_Node_List) - 1 loop
                  if Ada.Characters.Handling.To_Lower
                      (DOM.Core.Nodes.Local_Name (DOM.Core.Nodes.Item (Current_Node_List, N))) =
                    Ada.Characters.Handling.To_Lower (Next_Node_Name)
                  then
                     if Current_Node /= null then
                        raise Constraint_Error
                          with "Duplicate node name: " &
                          DOM.Core.Nodes.Local_Name (DOM.Core.Nodes.Item (Current_Node_List, N));
                     else
                        Current_Node := DOM.Core.Nodes.Item (Current_Node_List, N);
                     end if;
                  end if;
               end loop;
               if Current_Node = null then
                  return False;
               end if;
            end;
         end loop;

         return True;
      end Node_Exists;

      procedure Generate_Initial_File is
         F : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Config_Path);

         --!pp off
         Ada.Text_IO.Put_Line (F, "<?xml version=""1.0"" encoding=""utf-8""?>");
         Ada.Text_IO.Put_Line (F, "<Config xmlns=""http://xml.prunt3d.com/Prunt_Settings_Schema_Is_Unique_To_Each_Board_And_May_Change_Between_Versions"" xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"">");
         Ada.Text_IO.Put_Line (F, "   <Schema_Version>1</Schema_Version>");
         Ada.Text_IO.Put_Line (F, "   <Prunt>");
         Ada.Text_IO.Put_Line (F, "      <Enabled>false</Enabled>");
         Ada.Text_IO.Put_Line (F, "   </Prunt>");
         Ada.Text_IO.Put_Line (F, "   <Steppers>");
         for S in Stepper_Name loop
            Ada.Text_IO.Put_Line (F, "      <" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & ">");
            case Stepper_Kinds (S) is
               when Basic_Kind =>
                  Ada.Text_IO.Put_Line (F, "         <Enabled>false</Enabled>");
                  Ada.Text_IO.Put_Line (F, "         <Mm_Per_Step>1000000000000000000000000000.0</Mm_Per_Step>");
               when TMC2240_UART_Kind =>
                  Ada.Text_IO.Put_Line (F, "         <Enabled>false</Enabled>");
                  Ada.Text_IO.Put_Line (F, "         <Mm_Per_Step>1000000000000000000000000000.0</Mm_Per_Step>");
                  Ada.Text_IO.Put_Line (F, "         <Output_Current>0.15</Output_Current>");
                  Ada.Text_IO.Put_Line (F, "         <Slope_Control>Slope_100V_Per_us</Slope_Control>");
                  Ada.Text_IO.Put_Line (F, "         <I_Hold>16</I_Hold>");
                  Ada.Text_IO.Put_Line (F, "         <I_Run>31</I_Run>");
                  Ada.Text_IO.Put_Line (F, "         <I_Hold_Delay>1</I_Hold_Delay>");
                  Ada.Text_IO.Put_Line (F, "         <I_Run_Delay>4</I_Run_Delay>");
                  Ada.Text_IO.Put_Line (F, "         <T_Power_Down>10</T_Power_Down>");
                  Ada.Text_IO.Put_Line (F, "         <T_PWM_Thrs>0</T_PWM_Thrs>");
                  Ada.Text_IO.Put_Line (F, "         <T_Cool_Thrs>0</T_Cool_Thrs>");
                  Ada.Text_IO.Put_Line (F, "         <T_High>0</T_High>");
                  Ada.Text_IO.Put_Line (F, "         <TOFF>3</TOFF>");
                  Ada.Text_IO.Put_Line (F, "         <HSTRT_TFD210>5</HSTRT_TFD210>");
                  Ada.Text_IO.Put_Line (F, "         <HEND_OFFSET>2</HEND_OFFSET>");
                  Ada.Text_IO.Put_Line (F, "         <FD3>0</FD3>");
                  Ada.Text_IO.Put_Line (F, "         <DISFDCC>false</DISFDCC>");
                  Ada.Text_IO.Put_Line (F, "         <CHM>false</CHM>");
                  Ada.Text_IO.Put_Line (F, "         <VHIGHFS>false</VHIGHFS>");
                  Ada.Text_IO.Put_Line (F, "         <VHIGHCHM>false</VHIGHCHM>");
                  Ada.Text_IO.Put_Line (F, "         <TPFD>4</TPFD>");
                  Ada.Text_IO.Put_Line (F, "         <Microstep_Resolution>MS_256</Microstep_Resolution>");
                  Ada.Text_IO.Put_Line (F, "         <PWM_OFS>29</PWM_OFS>");
                  Ada.Text_IO.Put_Line (F, "         <PWM_Grad>0</PWM_Grad>");
                  Ada.Text_IO.Put_Line (F, "         <PWM_Freq>0</PWM_Freq>");
                  Ada.Text_IO.Put_Line (F, "         <PWM_Auto_Scale>true</PWM_Auto_Scale>");
                  Ada.Text_IO.Put_Line (F, "         <PWM_Auto_Grad>true</PWM_Auto_Grad>");
                  Ada.Text_IO.Put_Line (F, "         <Freewheel>Normal</Freewheel>");
                  Ada.Text_IO.Put_Line (F, "         <PWM_Meas_SD_Enable>false</PWM_Meas_SD_Enable>");
                  Ada.Text_IO.Put_Line (F, "         <PWM_Dis_Reg_Stst>false</PWM_Dis_Reg_Stst>");
                  Ada.Text_IO.Put_Line (F, "         <PWM_Reg>4</PWM_Reg>");
                  Ada.Text_IO.Put_Line (F, "         <PWM_Lim>12</PWM_Lim>");
            end case;
            Ada.Text_IO.Put_Line (F, "      </" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & ">");
         end loop;
         Ada.Text_IO.Put_Line (F, "   </Steppers>");
         Ada.Text_IO.Put_Line (F, "   <Kinematics>");
         Ada.Text_IO.Put_Line (F, "      <Lower_Pos_Limit>");
         Ada.Text_IO.Put_Line (F, "         <X_Axis>0.0</X_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Y_Axis>0.0</Y_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Z_Axis>0.0</Z_Axis>");
         Ada.Text_IO.Put_Line (F, "         <E_Axis>0.0</E_Axis>");
         Ada.Text_IO.Put_Line (F, "      </Lower_Pos_Limit>");
         Ada.Text_IO.Put_Line (F, "      <Upper_Pos_Limit>");
         Ada.Text_IO.Put_Line (F, "         <X_Axis>0.0</X_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Y_Axis>0.0</Y_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Z_Axis>0.0</Z_Axis>");
         Ada.Text_IO.Put_Line (F, "         <E_Axis>0.0</E_Axis>");
         Ada.Text_IO.Put_Line (F, "      </Upper_Pos_Limit>");
         Ada.Text_IO.Put_Line (F, "      <Ignore_E_In_XYZE>true</Ignore_E_In_XYZE>");
         Ada.Text_IO.Put_Line (F, "      <Shift_Blended_Corners>false</Shift_Blended_Corners>");
         Ada.Text_IO.Put_Line (F, "      <Tangential_Velocity_Max>0.0</Tangential_Velocity_Max>");
         Ada.Text_IO.Put_Line (F, "      <Axial_Velocity_Maxes>");
         Ada.Text_IO.Put_Line (F, "         <X_Axis>0.0</X_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Y_Axis>0.0</Y_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Z_Axis>0.0</Z_Axis>");
         Ada.Text_IO.Put_Line (F, "         <E_Axis>0.0</E_Axis>");
         Ada.Text_IO.Put_Line (F, "      </Axial_Velocity_Maxes>");
         Ada.Text_IO.Put_Line (F, "      <Pressure_Advance_Time>0.0</Pressure_Advance_Time>");
         Ada.Text_IO.Put_Line (F, "      <Acceleration_Max>0.0</Acceleration_Max>");
         Ada.Text_IO.Put_Line (F, "      <Jerk_Max>0.0</Jerk_Max>");
         Ada.Text_IO.Put_Line (F, "      <Snap_Max>0.0</Snap_Max>");
         Ada.Text_IO.Put_Line (F, "      <Crackle_Max>0.0</Crackle_Max>");
         Ada.Text_IO.Put_Line (F, "      <Chord_Error_Max>0.0</Chord_Error_Max>");
         Ada.Text_IO.Put_Line (F, "      <Axial_Scaler>");
         Ada.Text_IO.Put_Line (F, "         <X_Axis>1.0</X_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Y_Axis>1.0</Y_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Z_Axis>1.0</Z_Axis>");
         Ada.Text_IO.Put_Line (F, "         <E_Axis>1.0</E_Axis>");
         Ada.Text_IO.Put_Line (F, "      </Axial_Scaler>");
         Ada.Text_IO.Put_Line (F, "      <Cartesian>");
         for S in Stepper_Name loop
            Ada.Text_IO.Put_Line (F, "         <" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & ">Unused</" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & ">");
         end loop;
         Ada.Text_IO.Put_Line (F, "      </Cartesian>");
         Ada.Text_IO.Put_Line (F, "   </Kinematics>");
         Ada.Text_IO.Put_Line (F, "   <Input_Switches>");
         Ada.Text_IO.Put_Line (F, "      <Enabled>false</Enabled>");
         Ada.Text_IO.Put_Line (F, "      <Hit_On_High>true</Hit_On_High>");
         Ada.Text_IO.Put_Line (F, "   </Input_Switches>");
         Ada.Text_IO.Put_Line (F, "   <Homing>");
         Ada.Text_IO.Put_Line (F, "      <X_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Set_To_Value>");
         Ada.Text_IO.Put_Line (F, "            <Value>0.0</Value>");
         Ada.Text_IO.Put_Line (F, "         </Set_To_Value>");
         Ada.Text_IO.Put_Line (F, "      </X_Axis>");
         Ada.Text_IO.Put_Line (F, "      <Y_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Set_To_Value>");
         Ada.Text_IO.Put_Line (F, "            <Value>0.0</Value>");
         Ada.Text_IO.Put_Line (F, "         </Set_To_Value>");
         Ada.Text_IO.Put_Line (F, "      </Y_Axis>");
         Ada.Text_IO.Put_Line (F, "      <Z_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Set_To_Value>");
         Ada.Text_IO.Put_Line (F, "            <Value>0.0</Value>");
         Ada.Text_IO.Put_Line (F, "         </Set_To_Value>");
         Ada.Text_IO.Put_Line (F, "      </Z_Axis>");
         Ada.Text_IO.Put_Line (F, "      <E_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Set_To_Value>");
         Ada.Text_IO.Put_Line (F, "            <Value>0.0</Value>");
         Ada.Text_IO.Put_Line (F, "         </Set_To_Value>");
         Ada.Text_IO.Put_Line (F, "      </E_Axis>");
         Ada.Text_IO.Put_Line (F, "   </Homing>");
         Ada.Text_IO.Put_Line (F, "   <Heaters>");
         for H in Heater_Name loop
            Ada.Text_IO.Put_Line (F, "      <" & Ada.Strings.Fixed.Trim (H'Image, Ada.Strings.Both) & ">");
            Ada.Text_IO.Put_Line (F, "         <Thermistor>" & Ada.Strings.Fixed.Trim (Thermistor_Name'First'Image, Ada.Strings.Both) & "</Thermistor>");
            Ada.Text_IO.Put_Line (F, "         <Check_Max_Cumulative_Error>120.0</Check_Max_Cumulative_Error>");
            Ada.Text_IO.Put_Line (F, "         <Check_Gain_Time>20.0</Check_Gain_Time>");
            Ada.Text_IO.Put_Line (F, "         <Check_Minimum_Gain>2.0</Check_Minimum_Gain>");
            Ada.Text_IO.Put_Line (F, "         <Check_Hysteresis>3.0</Check_Hysteresis>");
            Ada.Text_IO.Put_Line (F, "         <Disabled />");
            Ada.Text_IO.Put_Line (F, "      </" & Ada.Strings.Fixed.Trim (H'Image, Ada.Strings.Both) & ">");
         end loop;
         Ada.Text_IO.Put_Line (F, "   </Heaters>");
         Ada.Text_IO.Put_Line (F, "   <Fans>");
         for Fan in Fan_Name loop
            Ada.Text_IO.Put_Line (F, "      <" & Ada.Strings.Fixed.Trim (Fan'Image, Ada.Strings.Both) & ">");
            Ada.Text_IO.Put_Line (F, "         <Disabled />");
            Ada.Text_IO.Put_Line (F, "      </" & Ada.Strings.Fixed.Trim (Fan'Image, Ada.Strings.Both) & ">");
         end loop;
         Ada.Text_IO.Put_Line (F, "   </Fans>");
         Ada.Text_IO.Put_Line (F, "   <G_Code_Assignments>");
         Ada.Text_IO.Put_Line (F, "      <Bed_Heater>");
         Ada.Text_IO.Put_Line (F, "         <Disabled />");
         Ada.Text_IO.Put_Line (F, "      </Bed_Heater>");
         Ada.Text_IO.Put_Line (F, "      <Hotend_Heater>");
         Ada.Text_IO.Put_Line (F, "         <Disabled />");
         Ada.Text_IO.Put_Line (F, "      </Hotend_Heater>");
         Ada.Text_IO.Put_Line (F, "      <Chamber_Heater>");
         Ada.Text_IO.Put_Line (F, "         <Disabled />");
         Ada.Text_IO.Put_Line (F, "      </Chamber_Heater>");
         Ada.Text_IO.Put_Line (F, "   </G_Code_Assignments>");
         Ada.Text_IO.Put_Line (F, "</Config>");
         --!pp on

         Ada.Text_IO.Close (F);
      end Generate_Initial_File;

   end Config_File;

   function Build_Schema return Unbounded_String is
      Schema : Unbounded_String;
      LF : constant Character := Ada.Characters.Latin_1.LF;
   begin
      --!pp off
      Append (Schema, "<?xml version=""1.0"" encoding=""UTF-8""?>" & LF);
      Append (Schema, "<schema" & LF);
      Append (Schema, "   targetNamespace=""http://xml.prunt3d.com/Prunt_Settings_Schema_Is_Unique_To_Each_Board_And_May_Change_Between_Versions""" & LF);
      Append (Schema, "   elementFormDefault=""qualified"" xmlns=""http://www.w3.org/2001/XMLSchema""" & LF);
      Append (Schema, "   xmlns:tns=""http://xml.prunt3d.com/Prunt_Settings_Schema_Is_Unique_To_Each_Board_And_May_Change_Between_Versions"">" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Input_Switch_Name"">" & LF);
      Append (Schema, "      <restriction base=""string"">" & LF);
      for I in Input_Switch_Name loop
      Append (Schema, "         <enumeration value=""" & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Both) & """ tns:localise=""" & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Input_Switch_Name'Pos (I)'Image, Ada.Strings.Both) & ")"" />" & LF);
      end loop;
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Heater_Name"">" & LF);
      Append (Schema, "      <restriction base=""string"">" & LF);
      for H in Heater_Name loop
      Append (Schema, "         <enumeration value=""" & Ada.Strings.Fixed.Trim (H'Image, Ada.Strings.Both) & """ tns:localise=""" & Ada.Strings.Fixed.Trim (H'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Heater_Name'Pos (H)'Image, Ada.Strings.Both) & ")"" />" & LF);
      end loop;
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Velocity"">" & LF);
      Append (Schema, "      <restriction base=""double"" />" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Acceleration"">" & LF);
      Append (Schema, "      <restriction base=""double"" />" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Jerk"">" & LF);
      Append (Schema, "      <restriction base=""double"" />" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Snap"">" & LF);
      Append (Schema, "      <restriction base=""double"" />" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Crackle"">" & LF);
      Append (Schema, "      <restriction base=""double"" />" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Time"">" & LF);
      Append (Schema, "      <restriction base=""double"" />" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Temperature"">" & LF);
      Append (Schema, "      <restriction base=""double"" />" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Dimensionless"">" & LF);
      Append (Schema, "      <restriction base=""double"" />" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Length"">" & LF);
      Append (Schema, "      <restriction base=""double"" />" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC_Unsigned_1"">" & LF);
      Append (Schema, "      <restriction base=""int"">" & LF);
      Append (Schema, "         <minInclusive value=""0"" />" & LF);
      Append (Schema, "         <maxExclusive value=""2"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC_Unsigned_2"">" & LF);
      Append (Schema, "      <restriction base=""int"">" & LF);
      Append (Schema, "         <minInclusive value=""0"" />" & LF);
      Append (Schema, "         <maxExclusive value=""4"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC_Unsigned_3"">" & LF);
      Append (Schema, "      <restriction base=""int"">" & LF);
      Append (Schema, "         <minInclusive value=""0"" />" & LF);
      Append (Schema, "         <maxExclusive value=""8"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC_Unsigned_4"">" & LF);
      Append (Schema, "      <restriction base=""int"">" & LF);
      Append (Schema, "         <minInclusive value=""0"" />" & LF);
      Append (Schema, "         <maxExclusive value=""16"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC_Unsigned_5"">" & LF);
      Append (Schema, "      <restriction base=""int"">" & LF);
      Append (Schema, "         <minInclusive value=""0"" />" & LF);
      Append (Schema, "         <maxExclusive value=""32"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC_Unsigned_8"">" & LF);
      Append (Schema, "      <restriction base=""int"">" & LF);
      Append (Schema, "         <minInclusive value=""0"" />" & LF);
      Append (Schema, "         <maxExclusive value=""256"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC_Unsigned_20"">" & LF);
      Append (Schema, "      <restriction base=""int"">" & LF);
      Append (Schema, "         <minInclusive value=""0"" />" & LF);
      Append (Schema, "         <maxExclusive value=""1048576"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC2240_Slope_Control"">" & LF);
      Append (Schema, "      <restriction base=""string"">" & LF);
      Append (Schema, "         <enumeration value=""Slope_100V_Per_us"" />" & LF);
      Append (Schema, "         <enumeration value=""Slope_200V_Per_us"" />" & LF);
      Append (Schema, "         <enumeration value=""Slope_400V_Per_us"" />" & LF);
      Append (Schema, "         <enumeration value=""Slope_800V_Per_us"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC2240_Microstep_Resolution"">" & LF);
      Append (Schema, "      <restriction base=""string"">" & LF);
      Append (Schema, "         <enumeration value=""MS_256"" />" & LF);
      Append (Schema, "         <enumeration value=""MS_128"" />" & LF);
      Append (Schema, "         <enumeration value=""MS_64"" />" & LF);
      Append (Schema, "         <enumeration value=""MS_32"" />" & LF);
      Append (Schema, "         <enumeration value=""MS_16"" />" & LF);
      Append (Schema, "         <enumeration value=""MS_2"" />" & LF);
      Append (Schema, "         <enumeration value=""MS_Full_Steps"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC2240_Freewheel"">" & LF);
      Append (Schema, "      <restriction base=""string"">" & LF);
      Append (Schema, "         <enumeration value=""Normal"" />" & LF);
      Append (Schema, "         <enumeration value=""Freewheel"" />" & LF);
      Append (Schema, "         <enumeration value=""Short_Via_LS"" />" & LF);
      Append (Schema, "         <enumeration value=""Short_Via_HS"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""TMC2240_Current"">" & LF);
      Append (Schema, "      <restriction base=""double"">" & LF);
      Append (Schema, "         <minInclusive value=""0.125"" />" & LF);
      Append (Schema, "         <maxInclusive value=""3"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Individual_Stepper_Parameters_TMC2240_UART"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Enabled"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""Mm_Per_Step"" type=""tns:Length"" />" & LF);
      Append (Schema, "         <element name=""Output_Current"" type=""tns:TMC2240_Current"" />" & LF);
      Append (Schema, "         <element name=""Slope_Control"" type=""tns:TMC2240_Slope_Control"" />" & LF);
      Append (Schema, "         <element name=""I_Hold"" type=""tns:TMC_Unsigned_5"" />" & LF);
      Append (Schema, "         <element name=""I_Run"" type=""tns:TMC_Unsigned_5"" />" & LF);
      Append (Schema, "         <element name=""I_Hold_Delay"" type=""tns:TMC_Unsigned_4"" />" & LF);
      Append (Schema, "         <element name=""I_Run_Delay"" type=""tns:TMC_Unsigned_4"" />" & LF);
      Append (Schema, "         <element name=""T_Power_Down"" type=""tns:TMC_Unsigned_8"" />" & LF);
      Append (Schema, "         <element name=""T_PWM_Thrs"" type=""tns:TMC_Unsigned_20"" />" & LF);
      Append (Schema, "         <element name=""T_Cool_Thrs"" type=""tns:TMC_Unsigned_20"" />" & LF);
      Append (Schema, "         <element name=""T_High"" type=""tns:TMC_Unsigned_20"" />" & LF);
      Append (Schema, "         <element name=""TOFF"" type=""tns:TMC_Unsigned_4"" />" & LF);
      Append (Schema, "         <element name=""HSTRT_TFD210"" type=""tns:TMC_Unsigned_3"" />" & LF);
      Append (Schema, "         <element name=""HEND_OFFSET"" type=""tns:TMC_Unsigned_4"" />" & LF);
      Append (Schema, "         <element name=""FD3"" type=""tns:TMC_Unsigned_1"" />" & LF);
      Append (Schema, "         <element name=""DISFDCC"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""CHM"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""VHIGHFS"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""VHIGHCHM"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""TPFD"" type=""tns:TMC_Unsigned_4"" />" & LF);
      Append
        (Schema, "         <element name=""Microstep_Resolution"" type=""tns:TMC2240_Microstep_Resolution"" />" & LF);
      Append (Schema, "         <element name=""PWM_OFS"" type=""tns:TMC_Unsigned_8"" />" & LF);
      Append (Schema, "         <element name=""PWM_Grad"" type=""tns:TMC_Unsigned_8"" />" & LF);
      Append (Schema, "         <element name=""PWM_Freq"" type=""tns:TMC_Unsigned_2"" />" & LF);
      Append (Schema, "         <element name=""PWM_Auto_Scale"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""PWM_Auto_Grad"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""Freewheel"" type=""tns:TMC2240_Freewheel"" />" & LF);
      Append (Schema, "         <element name=""PWM_Meas_SD_Enable"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""PWM_Dis_Reg_Stst"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""PWM_Reg"" type=""tns:TMC_Unsigned_4"" />" & LF);
      Append (Schema, "         <element name=""PWM_Lim"" type=""tns:TMC_Unsigned_4"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Individual_Stepper_Parameters_Basic"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Enabled"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""Mm_Per_Step"" type=""tns:Length"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Stepper_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      for S in Stepper_Name loop
         case Stepper_Kinds (S) is
            when Basic_Kind =>
               Append (Schema, "         <element name=""" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & """ type=""tns:Individual_Stepper_Parameters_Basic"" tns:localise=""" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Stepper_Name'Pos (S)'Image, Ada.Strings.Both) & ")"" />" & LF);
            when TMC2240_UART_Kind =>
               Append (Schema, "         <element name=""" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & """ type=""tns:Individual_Stepper_Parameters_TMC2240_UART"" tns:localise=""" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Stepper_Name'Pos (S)'Image, Ada.Strings.Both) & ")"" />" & LF);
         end case;
      end loop;
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Position"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""X_Axis"" type=""tns:Length"" />" & LF);
      Append (Schema, "         <element name=""Y_Axis"" type=""tns:Length"" />" & LF);
      Append (Schema, "         <element name=""Z_Axis"" type=""tns:Length"" />" & LF);
      Append (Schema, "         <element name=""E_Axis"" type=""tns:Length"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Axial_Velocities"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""X_Axis"" type=""tns:Velocity"" />" & LF);
      Append (Schema, "         <element name=""Y_Axis"" type=""tns:Velocity"" />" & LF);
      Append (Schema, "         <element name=""Z_Axis"" type=""tns:Velocity"" />" & LF);
      Append (Schema, "         <element name=""E_Axis"" type=""tns:Velocity"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Position_Scale"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""X_Axis"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "         <element name=""Y_Axis"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "         <element name=""Z_Axis"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "         <element name=""E_Axis"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Cartesian_Motor_Axis"">" & LF);
      Append (Schema, "      <restriction base=""string"">" & LF);
      Append (Schema, "         <enumeration value=""X_Axis"" />" & LF);
      Append (Schema, "         <enumeration value=""Y_Axis"" />" & LF);
      Append (Schema, "         <enumeration value=""Z_Axis"" />" & LF);
      Append (Schema, "         <enumeration value=""E_Axis"" />" & LF);
      Append (Schema, "         <enumeration value=""Unused"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Core_XY_Motor_Axis"">" & LF);
      Append (Schema, "      <restriction base=""string"">" & LF);
      Append (Schema, "         <enumeration value=""X_Axis"" />" & LF);
      Append (Schema, "         <enumeration value=""Y_Axis"" />" & LF);
      Append (Schema, "         <enumeration value=""Z_Axis"" />" & LF);
      Append (Schema, "         <enumeration value=""E_Axis"" />" & LF);
      Append (Schema, "         <enumeration value=""Unused"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Cartesian_Motor_Axis_Assignments"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      for S in Stepper_Name loop
      Append (Schema, "         <element name=""" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & """ type=""tns:Cartesian_Motor_Axis"" tns:localise=""" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Stepper_Name'Pos (S)'Image, Ada.Strings.Both) & ")"" />" & LF);
      end loop;
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Core_XY_Motor_Axis_Assignments"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      for S in Stepper_Name loop
      Append (Schema, "         <element name=""" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & """ type=""tns:Core_XY_Motor_Axis"" tns:localise=""" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Stepper_Name'Pos (S)'Image, Ada.Strings.Both) & ")"" />" & LF);
      end loop;
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Kinematic_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Lower_Pos_Limit"" type=""tns:Position"" />" & LF);
      Append (Schema, "         <element name=""Upper_Pos_Limit"" type=""tns:Position"" />" & LF);
      Append (Schema, "         <element name=""Ignore_E_In_XYZE"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""Shift_Blended_Corners"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""Tangential_Velocity_Max"" type=""tns:Velocity"" />" & LF);
      Append (Schema, "         <element name=""Axial_Velocity_Maxes"" type=""tns:Axial_Velocities"" />" & LF);
      Append (Schema, "         <element name=""Pressure_Advance_Time"" type=""tns:Time"" />" & LF);
      Append (Schema, "         <element name=""Acceleration_Max"" type=""tns:Acceleration"" />" & LF);
      Append (Schema, "         <element name=""Jerk_Max"" type=""tns:Jerk"" />" & LF);
      Append (Schema, "         <element name=""Snap_Max"" type=""tns:Snap"" />" & LF);
      Append (Schema, "         <element name=""Crackle_Max"" type=""tns:Crackle"" />" & LF);
      Append (Schema, "         <element name=""Chord_Error_Max"" type=""tns:Length"" />" & LF);
      Append (Schema, "         <element name=""Axial_Scaler"" type=""tns:Position_Scale"" />" & LF);
      Append (Schema, "         <choice>" & LF);
      Append (Schema, "            <element name=""Core_XY"" type=""tns:Core_XY_Motor_Axis_Assignments"" />" & LF);
      Append (Schema, "            <element name=""Cartesian"" type=""tns:Cartesian_Motor_Axis_Assignments"" />" & LF);
      Append (Schema, "         </choice>" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Input_Switch_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Enabled"" type=""boolean"" />" & LF);
      Append (Schema, "         <element name=""Hit_On_High"" type=""boolean"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Input_Switches"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      for I in Input_Switch_Name loop
      Append (Schema, "         <element name=""" & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Both) & """ type=""tns:Input_Switch_Parameters"" tns:localise=""" & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Input_Switch_Name'Pos (I)'Image, Ada.Strings.Both) & ")"" />" & LF);
      end loop;
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Double_Tap_Homing_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Switch"" type=""tns:Input_Switch_Name"" />" & LF);
      Append (Schema, "         <element name=""First_Move_Distance"" type=""tns:Length"" />" & LF);
      Append (Schema, "         <element name=""Back_Off_Move_Distance"" type=""tns:Length"" />" & LF);
      Append (Schema, "         <element name=""Second_Move_Distance"" type=""tns:Length"" />" & LF);
      Append (Schema, "         <element name=""Switch_Position"" type=""tns:Length"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Set_To_Value_Homing_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Value"" type=""tns:Length"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Axis_Homing_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <choice>" & LF);
      Append (Schema, "            <element name=""Double_Tap"" type=""tns:Double_Tap_Homing_Parameters"" />" & LF);
      Append (Schema, "            <element name=""Set_To_Value"" type=""tns:Set_To_Value_Homing_Parameters"" />" & LF);
      Append (Schema, "         </choice>" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Homing_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""X_Axis"" type=""tns:Axis_Homing_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Y_Axis"" type=""tns:Axis_Homing_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Z_Axis"" type=""tns:Axis_Homing_Parameters"" />" & LF);
      Append (Schema, "         <element name=""E_Axis"" type=""tns:Axis_Homing_Parameters"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Steinhart_Hart_Thermistor_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""A"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "         <element name=""B"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "         <element name=""C"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Callendar_Van_Dusen_Thermistor_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""R0"" type=""tns:Resistance"" />" & LF);
      Append (Schema, "         <element name=""A"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "         <element name=""B"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Individual_Thermistor_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Minimum_Temperature"" type=""tns:Temperature"" />" & LF);
      Append (Schema, "         <element name=""Maximum_Temperature"" type=""tns:Temperature"" />" & LF);
      Append (Schema, "         <choice>" & LF);
      Append (Schema, "            <element name=""Disabled"" type=""tns:Disabled_Thermistor_Parameters"" />" & LF);
      Append (Schema, "            <element name=""Steinhart_Hart"" type=""tns:Steinhart_Hart_Thermistor_Parameters"" />" & LF);
      Append (Schema, "            <element name=""Callendar_Van_Dusen"" type=""tns:Callendar_Van_Dusen_Thermistor_Parameters"" />" & LF);
      Append (Schema, "         </choice>" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Thermistor_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      for T in Thermistor_Name loop
      Append (Schema, "         <element name=""" & Ada.Strings.Fixed.Trim (T'Image, Ada.Strings.Both) & """ tns:localise=""" & Ada.Strings.Fixed.Trim (T'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Thermistor_Name'Pos (T)'Image, Ada.Strings.Both) & ")"" type=""tns:Individual_Thermistor_Parameters"" />" & LF);
      end loop;
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Thermistor_Name"">" & LF);
      Append (Schema, "      <restriction base=""string"">" & LF);
      for T in Thermistor_Name loop
      Append (Schema, "         <enumeration value=""" & Ada.Strings.Fixed.Trim (T'Image, Ada.Strings.Both) & """ tns:localise=""" & Ada.Strings.Fixed.Trim (T'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Thermistor_Name'Pos (T)'Image, Ada.Strings.Both) & ")"" />" & LF);
      end loop;
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Disabled_Heater_Parameters"">" & LF);
      Append (Schema, "      <sequence />" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""PID_Heater_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Proportional_Scale"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "         <element name=""Integral_Scale"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "         <element name=""Derivative_Scale"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Bang_Bang_Heater_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Bang_Bang_Hysteresis"" type=""tns:Temperature"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Individual_Heater_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Thermistor"" type=""tns:Thermistor_Name"" />" & LF);
      Append (Schema, "         <element name=""Check_Max_Cumulative_Error"" type=""tns:Temperature"" />" & LF);
      Append (Schema, "         <element name=""Check_Gain_Time"" type=""tns:Time"" />" & LF);
      Append (Schema, "         <element name=""Check_Minimum_Gain"" type=""tns:Temperature"" />" & LF);
      Append (Schema, "         <element name=""Check_Hysteresis"" type=""tns:Temperature"" />" & LF);
      Append (Schema, "         <choice>" & LF);
      Append (Schema, "            <element name=""Disabled"" type=""tns:Disabled_Heater_Parameters"" />" & LF);
      Append (Schema, "            <element name=""PID"" type=""tns:PID_Heater_Parameters"" />" & LF);
      Append (Schema, "            <element name=""Bang_Bang"" type=""tns:Bang_Bang_Heater_Parameters"" />" & LF);
      Append (Schema, "         </choice>" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Heater_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      for H in Heater_Name loop
      Append (Schema, "         <element name=""" & Ada.Strings.Fixed.Trim (H'Image, Ada.Strings.Both) & """ tns:localise=""" & Ada.Strings.Fixed.Trim (H'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Heater_Name'Pos (H)'Image, Ada.Strings.Both) & ")"" type=""tns:Individual_Heater_Parameters"" />" & LF);
      end loop;
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Disabled_Fan_Parameters"">" & LF);
      Append (Schema, "      <sequence />" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Dynamic_PWM_Fan_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Proportional_Scale"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "         <element name=""Integral_Scale"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "         <element name=""Derivative_Scale"" type=""tns:Dimensionless"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Always_On_Fan_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Bang_Bang_Hysteresis"" type=""tns:Temperature"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Individual_Fan_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <choice>" & LF);
      Append (Schema, "            <element name=""Disabled"" type=""tns:Disabled_Fan_Parameters"" />" & LF);
      Append (Schema, "            <element name=""Dynamic_PWM"" type=""tns:Dynamic_PWM_Fan_Parameters"" />" & LF);
      Append (Schema, "            <element name=""Always_On"" type=""tns:Always_On_Fan_Parameters"" />" & LF);
      Append (Schema, "         </choice>" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Fan_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      for F in Fan_Name loop
      Append (Schema, "         <element name=""" & Ada.Strings.Fixed.Trim (F'Image, Ada.Strings.Both) & """ tns:localise=""" & Ada.Strings.Fixed.Trim (F'Image, Ada.Strings.Both) & " (" & Ada.Strings.Fixed.Trim (Fan_Name'Pos (F)'Image, Ada.Strings.Both) & ")"" type=""tns:Individual_Fan_Parameters"" />" & LF);
      end loop;
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Disabled_G_Code_Heater_Parameters"">" & LF);
      Append (Schema, "      <sequence />" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Enabled_G_Code_Heater_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Assigned_Heater"" type=""tns:Heater_Name"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Individual_G_Code_Heater_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <choice>" & LF);
      Append (Schema, "            <element name=""Disabled"" type=""tns:Disabled_G_Code_Heater_Parameters"" />" & LF);
      Append (Schema, "            <element name=""Enabled"" type=""tns:Enabled_G_Code_Heater_Parameters"" />" & LF);
      Append (Schema, "         </choice>" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""G_Code_Assignment_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Bed_Heater"" type=""tns:Individual_G_Code_Heater_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Hotend_Heater"" type=""tns:Individual_G_Code_Heater_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Chamber_Heater"" type=""tns:Individual_G_Code_Heater_Parameters"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Prunt_Parameters"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Enabled"" type=""boolean"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <simpleType name=""Schema_Version"">" & LF);
      Append (Schema, "      <restriction base=""int"">" & LF);
      Append (Schema, "         <enumeration value=""1"" />" & LF);
      Append (Schema, "      </restriction>" & LF);
      Append (Schema, "   </simpleType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <complexType name=""Config"">" & LF);
      Append (Schema, "      <sequence>" & LF);
      Append (Schema, "         <element name=""Schema_Version"" type=""tns:Schema_Version"" />" & LF);
      Append (Schema, "         <element name=""Prunt"" type=""tns:Prunt_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Steppers"" type=""tns:Stepper_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Kinematics"" type=""tns:Kinematic_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Input_Switches"" type=""tns:Input_Switch_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Homing"" type=""tns:Homing_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Thermistors"" type=""tns:Thermistor_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Heaters"" type=""tns:Heater_Parameters"" />" & LF);
      Append (Schema, "         <element name=""Fans"" type=""tns:Fan_Parameters"" />" & LF);
      Append (Schema, "         <element name=""G_Code_Assignments"" type=""tns:G_Code_Assignment_Parameters"" />" & LF);
      Append (Schema, "      </sequence>" & LF);
      Append (Schema, "   </complexType>" & LF);
      Append (Schema, "" & LF);
      Append (Schema, "   <element name=""Config"" type=""tns:Config"" />" & LF);
      Append (Schema, "</schema>" & LF);
      --!pp on

      return Schema;
   end Build_Schema;

   function Build_Schema return Schema.Validators.XML_Grammar is
      Input : Input_Sources.Strings.String_Input;
      Reader : Schema.Schema_Readers.Schema_Reader;
   begin
      Input_Sources.Strings.Open
        (Ada.Strings.Unbounded.To_String (Build_Schema), Unicode.CES.Utf8.Utf8_Encoding, Input);
      Schema.Schema_Readers.Set_Symbol_Table (Reader, Sax.Utils.Allocate);
      Schema.Schema_Readers.Parse (Reader, Input);
      Input_Sources.Strings.Close (Input);
      return Schema.Schema_Readers.Get_Grammar (Reader);
   end Build_Schema;

end Prunt.Config_XML;
