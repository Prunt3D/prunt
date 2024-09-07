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

package body Prunt.Config_XML is

   pragma Unsuppress (All_Checks);

   protected body Config_File is

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
         null;
      end Read;

      procedure Read (Data : out Stepper_Parameters; Stepper : Stepper_Name) is
      begin
         null;
      end Read;

      procedure Read (Data : out Kinematics_Parameters) is
      begin
         null;
      end Read;

      procedure Read (Data : out Input_Switch_Parameters; Input_Switch : Input_Switch_Name) is
      begin
         null;
      end Read;

      procedure Read (Data : out Homing_Parameters; Axis : Axis_Name) is
      begin
         null;
      end Read;

      procedure Read (Data : out Extruder_Parameters) is
      begin
         null;
      end Read;

      procedure Read (Data : out Thermistor_Parameters; Thermistor : Thermistor_Name) is
      begin
         null;
      end Read;

      procedure Read (Data : out Heater_Full_Parameters; Heater : Heater_Name) is
      begin
         null;
      end Read;

      procedure Read (Data : out Fan_Parameters; Fan : Fan_Name) is
      begin
         null;
      end Read;

      procedure Read (Data : out G_Code_Assignment_Parameters) is
      begin
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
            Schema.Dom_Readers.Set_Feature (XML_Reader, Sax.Readers.Validation_Feature, True);
            Schema.Dom_Readers.Parse (XML_Reader, Input);
            Input_Sources.File.Close (Input);
         end;

      end Maybe_Read_Tree;

      function Get_Node (Path : DOM.Core.DOM_String) return DOM.Core.DOM_String is
         use type DOM.Core.Node;
         Current_Node : DOM.Core.Node := XML_Reader.Get_Tree;
      begin
         for Next_Node_Name of GNAT.String_Split.Create (Path (Path'First + 1 .. Path'Last), "/") loop
            declare
               Current_Node_List : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Current_Node);
            begin
               Current_Node := null;
               for N in 0 .. DOM.Core.Nodes.Length (Current_Node_List) loop
                  if DOM.Core.Nodes.Local_Name (DOM.Core.Nodes.Item (Current_Node_List, N)) = Next_Node_Name then
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

         return DOM.Core.Nodes.Node_Value (Current_Node);
      end Get_Node;

      function Node_Exists (Path : DOM.Core.DOM_String) return Boolean is
         use type DOM.Core.Node;
         Current_Node : DOM.Core.Node := XML_Reader.Get_Tree;
      begin
         for Next_Node_Name of GNAT.String_Split.Create (Path (Path'First + 1 .. Path'Last), "/") loop
            declare
               Current_Node_List : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (Current_Node);
            begin
               Current_Node := null;
               for N in 0 .. DOM.Core.Nodes.Length (Current_Node_List) loop
                  if DOM.Core.Nodes.Local_Name (DOM.Core.Nodes.Item (Current_Node_List, N)) = Next_Node_Name then
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

         Ada.Text_IO.Put_Line (F, "<?xml version=""1.0"" encoding=""utf-8""?>");
         Ada.Text_IO.Put_Line
           (F,
            "<Config xmlns=""http://xml.prunt3d.com/Prunt_Settings_Schema_Is_Unique_To_Each_Board_And_May_Change_Between_Versions"" xsi:schemaLocation=""http://xml.prunt3d.com/Prunt_Settings_Schema_Is_Unique_To_Each_Board_And_May_Change_Between_Versions schema.xsd"" xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"">");
         Ada.Text_IO.Put_Line (F, "   <Schema_Version>1</Schema_Version>");
         Ada.Text_IO.Put_Line (F, "   <Prunt_Parameters>");
         Ada.Text_IO.Put_Line (F, "      <Enabled>false</Enabled>");
         Ada.Text_IO.Put_Line (F, "   </Prunt_Parameters>");
         Ada.Text_IO.Put_Line (F, "   <Stepper_Parameters>");
         for S in Stepper_Name loop
            Ada.Text_IO.Put_Line (F, "      <" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & ">");
            case Stepper_Kinds (S) is
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
               when others =>
                  raise Constraint_Error with "Not implemented.";
            end case;
            Ada.Text_IO.Put_Line (F, "      </" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & ">");
         end loop;
         Ada.Text_IO.Put_Line (F, "   </Stepper_Parameters>");
         Ada.Text_IO.Put_Line (F, "   <Kinematic_Parameters>");
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
            Ada.Text_IO.Put_Line
              (F,
               "         <" & Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & ">Unused</" &
               Ada.Strings.Fixed.Trim (S'Image, Ada.Strings.Both) & ">");
         end loop;
         Ada.Text_IO.Put_Line (F, "      </Cartesian>");
         Ada.Text_IO.Put_Line (F, "   </Kinematic_Parameters>");
         Ada.Text_IO.Put_Line (F, "   <Input_Switch_Parameters>");
         Ada.Text_IO.Put_Line (F, "      <Enabled>false</Enabled>");
         Ada.Text_IO.Put_Line (F, "      <Hit_On_High>true</Hit_On_High>");
         Ada.Text_IO.Put_Line (F, "   </Input_Switch_Parameters>");
         Ada.Text_IO.Put_Line (F, "   <Homing_Parameters>");
         Ada.Text_IO.Put_Line (F, "      <X_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Set_To_Value_Kind>");
         Ada.Text_IO.Put_Line (F, "            <Value>0.0</Value>");
         Ada.Text_IO.Put_Line (F, "         </Set_To_Value_Kind>");
         Ada.Text_IO.Put_Line (F, "      </X_Axis>");
         Ada.Text_IO.Put_Line (F, "      <Y_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Set_To_Value_Kind>");
         Ada.Text_IO.Put_Line (F, "            <Value>0.0</Value>");
         Ada.Text_IO.Put_Line (F, "         </Set_To_Value_Kind>");
         Ada.Text_IO.Put_Line (F, "      </Y_Axis>");
         Ada.Text_IO.Put_Line (F, "      <Z_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Set_To_Value_Kind>");
         Ada.Text_IO.Put_Line (F, "            <Value>0.0</Value>");
         Ada.Text_IO.Put_Line (F, "         </Set_To_Value_Kind>");
         Ada.Text_IO.Put_Line (F, "      </Z_Axis>");
         Ada.Text_IO.Put_Line (F, "      <E_Axis>");
         Ada.Text_IO.Put_Line (F, "         <Set_To_Value_Kind>");
         Ada.Text_IO.Put_Line (F, "            <Value>0.0</Value>");
         Ada.Text_IO.Put_Line (F, "         </Set_To_Value_Kind>");
         Ada.Text_IO.Put_Line (F, "      </E_Axis>");
         Ada.Text_IO.Put_Line (F, "   </Homing_Parameters>");
         Ada.Text_IO.Put_Line (F, "   <Heater_Parameters>");
         for H in Heater_Name loop
            Ada.Text_IO.Put_Line (F, "      <" & Ada.Strings.Fixed.Trim (H'Image, Ada.Strings.Both) & ">");
            Ada.Text_IO.Put_Line
              (F,
               "         <Thermistor>" & Ada.Strings.Fixed.Trim (Thermistor_Name'First'Image, Ada.Strings.Both) &
               "</Thermistor>");
            Ada.Text_IO.Put_Line (F, "         <Check_Max_Cumulative_Error>120.0</Check_Max_Cumulative_Error>");
            Ada.Text_IO.Put_Line (F, "         <Check_Gain_Time>20.0</Check_Gain_Time>");
            Ada.Text_IO.Put_Line (F, "         <Check_Minimum_Gain>2.0</Check_Minimum_Gain>");
            Ada.Text_IO.Put_Line (F, "         <Check_Hysteresis>3.0</Check_Hysteresis>");
            Ada.Text_IO.Put_Line (F, "         <Disabled_Kind />");
            Ada.Text_IO.Put_Line (F, "      </" & Ada.Strings.Fixed.Trim (H'Image, Ada.Strings.Both) & ">");
         end loop;
         Ada.Text_IO.Put_Line (F, "   </Heater_Parameters>");
         Ada.Text_IO.Put_Line (F, "   <Fan_Parameters>");
         for Fan in Fan_Name loop
            Ada.Text_IO.Put_Line (F, "      <" & Ada.Strings.Fixed.Trim (Fan'Image, Ada.Strings.Both) & ">");
            Ada.Text_IO.Put_Line (F, "         <Disabled_Kind />");
            Ada.Text_IO.Put_Line (F, "      </" & Ada.Strings.Fixed.Trim (Fan'Image, Ada.Strings.Both) & ">");
         end loop;
         Ada.Text_IO.Put_Line (F, "   </Fan_Parameters>");
         Ada.Text_IO.Put_Line (F, "   <G_Code_Assignment_Parameters>");
         Ada.Text_IO.Put_Line (F, "      <Bed_Heater>");
         Ada.Text_IO.Put_Line (F, "         <Disabled_Kind />");
         Ada.Text_IO.Put_Line (F, "      </Bed_Heater>");
         Ada.Text_IO.Put_Line (F, "      <Hotend_Heater>");
         Ada.Text_IO.Put_Line (F, "         <Disabled_Kind />");
         Ada.Text_IO.Put_Line (F, "      </Hotend_Heater>");
         Ada.Text_IO.Put_Line (F, "      <Chamber_Heater>");
         Ada.Text_IO.Put_Line (F, "         <Disabled_Kind />");
         Ada.Text_IO.Put_Line (F, "      </Chamber_Heater>");
         Ada.Text_IO.Put_Line (F, "   </G_Code_Assignment_Parameters>");
         Ada.Text_IO.Put_Line (F, "</Config>");

         Ada.Text_IO.Close (F);
      end Generate_Initial_File;

   end Config_File;

   function Build_Schema return Unbounded_String is
      Schema : Unbounded_String;
   begin
      Append (Schema, "<?xml version=""1.0"" encoding=""UTF-8""?>");
      Append (Schema, "<schema");
      Append
        (Schema,
         "   targetNamespace=""http://xml.prunt3d.com/Prunt_Settings_Schema_Is_Unique_To_Each_Board_And_May_Change_Between_Versions""");
      Append (Schema, "   elementFormDefault=""qualified"" xmlns=""http://www.w3.org/2001/XMLSchema""");
      Append
        (Schema,
         "   xmlns:tns=""http://xml.prunt3d.com/Prunt_Settings_Schema_Is_Unique_To_Each_Board_And_May_Change_Between_Versions"">");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Input_Switch_Name"">");
      Append (Schema, "      <restriction base=""string"">");
      Append (Schema, "         <enumeration value=""Input_Switch_1"" tns:localise=""Input_Switch_1 (1)"" />");
      Append (Schema, "         <enumeration value=""Input_Switch_2"" tns:localise=""Input_Switch_2 (2)"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Heater_Name"">");
      Append (Schema, "      <restriction base=""string"">");
      Append (Schema, "         <enumeration value=""Heater_1"" tns:localise=""Heater_1 (1)"" />");
      Append (Schema, "         <enumeration value=""Heater_2"" tns:localise=""Heater_2 (2)"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Velocity"">");
      Append (Schema, "      <restriction base=""double"" />");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Acceleration"">");
      Append (Schema, "      <restriction base=""double"" />");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Jerk"">");
      Append (Schema, "      <restriction base=""double"" />");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Snap"">");
      Append (Schema, "      <restriction base=""double"" />");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Crackle"">");
      Append (Schema, "      <restriction base=""double"" />");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Time"">");
      Append (Schema, "      <restriction base=""double"" />");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Temperature"">");
      Append (Schema, "      <restriction base=""double"" />");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Dimensionless"">");
      Append (Schema, "      <restriction base=""double"" />");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Length"">");
      Append (Schema, "      <restriction base=""double"" />");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC_Unsigned_1"">");
      Append (Schema, "      <restriction base=""int"">");
      Append (Schema, "         <minInclusive value=""0"" />");
      Append (Schema, "         <maxExclusive value=""2"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC_Unsigned_2"">");
      Append (Schema, "      <restriction base=""int"">");
      Append (Schema, "         <minInclusive value=""0"" />");
      Append (Schema, "         <maxExclusive value=""4"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC_Unsigned_3"">");
      Append (Schema, "      <restriction base=""int"">");
      Append (Schema, "         <minInclusive value=""0"" />");
      Append (Schema, "         <maxExclusive value=""8"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC_Unsigned_4"">");
      Append (Schema, "      <restriction base=""int"">");
      Append (Schema, "         <minInclusive value=""0"" />");
      Append (Schema, "         <maxExclusive value=""16"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC_Unsigned_5"">");
      Append (Schema, "      <restriction base=""int"">");
      Append (Schema, "         <minInclusive value=""0"" />");
      Append (Schema, "         <maxExclusive value=""32"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC_Unsigned_8"">");
      Append (Schema, "      <restriction base=""int"">");
      Append (Schema, "         <minInclusive value=""0"" />");
      Append (Schema, "         <maxExclusive value=""256"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC_Unsigned_20"">");
      Append (Schema, "      <restriction base=""int"">");
      Append (Schema, "         <minInclusive value=""0"" />");
      Append (Schema, "         <maxExclusive value=""1048576"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC2240_Slope_Control"">");
      Append (Schema, "      <restriction base=""string"">");
      Append (Schema, "         <enumeration value=""Slope_100V_Per_us"" />");
      Append (Schema, "         <enumeration value=""Slope_200V_Per_us"" />");
      Append (Schema, "         <enumeration value=""Slope_400V_Per_us"" />");
      Append (Schema, "         <enumeration value=""Slope_800V_Per_us"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC2240_Microstep_Resolution"">");
      Append (Schema, "      <restriction base=""string"">");
      Append (Schema, "         <enumeration value=""MS_256"" />");
      Append (Schema, "         <enumeration value=""MS_128"" />");
      Append (Schema, "         <enumeration value=""MS_64"" />");
      Append (Schema, "         <enumeration value=""MS_32"" />");
      Append (Schema, "         <enumeration value=""MS_16"" />");
      Append (Schema, "         <enumeration value=""MS_2"" />");
      Append (Schema, "         <enumeration value=""MS_Full_Steps"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC2240_Freewheel"">");
      Append (Schema, "      <restriction base=""string"">");
      Append (Schema, "         <enumeration value=""Normal"" />");
      Append (Schema, "         <enumeration value=""Freewheel"" />");
      Append (Schema, "         <enumeration value=""Short_Via_LS"" />");
      Append (Schema, "         <enumeration value=""Short_Via_HS"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""TMC2240_Current"">");
      Append (Schema, "      <restriction base=""double"">");
      Append (Schema, "         <minInclusive value=""0.125"" />");
      Append (Schema, "         <maxInclusive value=""3"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Individual_Stepper_Parameters_TMC2240_UART"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Enabled"" type=""boolean"" />");
      Append (Schema, "         <element name=""Mm_Per_Step"" type=""tns:Length"" />");
      Append (Schema, "         <element name=""Output_Current"" type=""tns:TMC2240_Current"" />");
      Append (Schema, "         <element name=""Slope_Control"" type=""tns:TMC2240_Slope_Control"" />");
      Append (Schema, "         <element name=""I_Hold"" type=""tns:TMC_Unsigned_5"" />");
      Append (Schema, "         <element name=""I_Run"" type=""tns:TMC_Unsigned_5"" />");
      Append (Schema, "         <element name=""I_Hold_Delay"" type=""tns:TMC_Unsigned_4"" />");
      Append (Schema, "         <element name=""I_Run_Delay"" type=""tns:TMC_Unsigned_4"" />");
      Append (Schema, "         <element name=""T_Power_Down"" type=""tns:TMC_Unsigned_8"" />");
      Append (Schema, "         <element name=""T_PWM_Thrs"" type=""tns:TMC_Unsigned_20"" />");
      Append (Schema, "         <element name=""T_Cool_Thrs"" type=""tns:TMC_Unsigned_20"" />");
      Append (Schema, "         <element name=""T_High"" type=""tns:TMC_Unsigned_20"" />");
      Append (Schema, "         <element name=""TOFF"" type=""tns:TMC_Unsigned_4"" />");
      Append (Schema, "         <element name=""HSTRT_TFD210"" type=""tns:TMC_Unsigned_3"" />");
      Append (Schema, "         <element name=""HEND_OFFSET"" type=""tns:TMC_Unsigned_4"" />");
      Append (Schema, "         <element name=""FD3"" type=""tns:TMC_Unsigned_1"" />");
      Append (Schema, "         <element name=""DISFDCC"" type=""boolean"" />");
      Append (Schema, "         <element name=""CHM"" type=""boolean"" />");
      Append (Schema, "         <element name=""VHIGHFS"" type=""boolean"" />");
      Append (Schema, "         <element name=""VHIGHCHM"" type=""boolean"" />");
      Append (Schema, "         <element name=""TPFD"" type=""tns:TMC_Unsigned_4"" />");
      Append (Schema, "         <element name=""Microstep_Resolution"" type=""tns:TMC2240_Microstep_Resolution"" />");
      Append (Schema, "         <element name=""PWM_OFS"" type=""tns:TMC_Unsigned_8"" />");
      Append (Schema, "         <element name=""PWM_Grad"" type=""tns:TMC_Unsigned_8"" />");
      Append (Schema, "         <element name=""PWM_Freq"" type=""tns:TMC_Unsigned_2"" />");
      Append (Schema, "         <element name=""PWM_Auto_Scale"" type=""boolean"" />");
      Append (Schema, "         <element name=""PWM_Auto_Grad"" type=""boolean"" />");
      Append (Schema, "         <element name=""Freewheel"" type=""tns:TMC2240_Freewheel"" />");
      Append (Schema, "         <element name=""PWM_Meas_SD_Enable"" type=""boolean"" />");
      Append (Schema, "         <element name=""PWM_Dis_Reg_Stst"" type=""boolean"" />");
      Append (Schema, "         <element name=""PWM_Reg"" type=""tns:TMC_Unsigned_4"" />");
      Append (Schema, "         <element name=""PWM_Lim"" type=""tns:TMC_Unsigned_4"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Stepper_Parameters"">");
      Append (Schema, "      <sequence>");
      Append
        (Schema,
         "         <element name=""Stepper_1"" type=""tns:Individual_Stepper_Parameters_TMC2240_UART"" tns:localise=""Stepper_1 (1)"" />");
      Append
        (Schema,
         "         <element name=""Stepper_2"" type=""tns:Individual_Stepper_Parameters_TMC2240_UART"" tns:localise=""Stepper_2 (2)"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Position"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""X_Axis"" type=""tns:Length"" />");
      Append (Schema, "         <element name=""Y_Axis"" type=""tns:Length"" />");
      Append (Schema, "         <element name=""Z_Axis"" type=""tns:Length"" />");
      Append (Schema, "         <element name=""E_Axis"" type=""tns:Length"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Axial_Velocities"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""X_Axis"" type=""tns:Velocity"" />");
      Append (Schema, "         <element name=""Y_Axis"" type=""tns:Velocity"" />");
      Append (Schema, "         <element name=""Z_Axis"" type=""tns:Velocity"" />");
      Append (Schema, "         <element name=""E_Axis"" type=""tns:Velocity"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Position_Scale"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""X_Axis"" type=""tns:Dimensionless"" />");
      Append (Schema, "         <element name=""Y_Axis"" type=""tns:Dimensionless"" />");
      Append (Schema, "         <element name=""Z_Axis"" type=""tns:Dimensionless"" />");
      Append (Schema, "         <element name=""E_Axis"" type=""tns:Dimensionless"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Cartesian_Motor_Axis"">");
      Append (Schema, "      <restriction base=""string"">");
      Append (Schema, "         <enumeration value=""X_Axis"" />");
      Append (Schema, "         <enumeration value=""Y_Axis"" />");
      Append (Schema, "         <enumeration value=""Z_Axis"" />");
      Append (Schema, "         <enumeration value=""E_Axis"" />");
      Append (Schema, "         <enumeration value=""Unused"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Core_XY_Motor_Axis"">");
      Append (Schema, "      <restriction base=""string"">");
      Append (Schema, "         <enumeration value=""X_Axis"" />");
      Append (Schema, "         <enumeration value=""Y_Axis"" />");
      Append (Schema, "         <enumeration value=""Z_Axis"" />");
      Append (Schema, "         <enumeration value=""E_Axis"" />");
      Append (Schema, "         <enumeration value=""Unused"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Cartesian_Motor_Axis_Assignments"">");
      Append (Schema, "      <sequence>");
      Append
        (Schema,
         "         <element name=""Stepper_1"" type=""tns:Cartesian_Motor_Axis"" tns:localise=""Stepper_1 (1)"" />");
      Append
        (Schema,
         "         <element name=""Stepper_2"" type=""tns:Cartesian_Motor_Axis"" tns:localise=""Stepper_2 (2)"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Core_XY_Motor_Axis_Assignments"">");
      Append (Schema, "      <sequence>");
      Append
        (Schema,
         "         <element name=""Stepper_1"" type=""tns:Core_XY_Motor_Axis"" tns:localise=""Stepper_1 (1)"" />");
      Append
        (Schema,
         "         <element name=""Stepper_2"" type=""tns:Core_XY_Motor_Axis"" tns:localise=""Stepper_2 (2)"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Kinematic_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Lower_Pos_Limit"" type=""tns:Position"" />");
      Append (Schema, "         <element name=""Upper_Pos_Limit"" type=""tns:Position"" />");
      Append (Schema, "         <element name=""Ignore_E_In_XYZE"" type=""boolean"" />");
      Append (Schema, "         <element name=""Shift_Blended_Corners"" type=""boolean"" />");
      Append (Schema, "         <element name=""Tangential_Velocity_Max"" type=""tns:Velocity"" />");
      Append (Schema, "         <element name=""Axial_Velocity_Maxes"" type=""tns:Axial_Velocities"" />");
      Append (Schema, "         <element name=""Pressure_Advance_Time"" type=""tns:Time"" />");
      Append (Schema, "         <element name=""Acceleration_Max"" type=""tns:Acceleration"" />");
      Append (Schema, "         <element name=""Jerk_Max"" type=""tns:Jerk"" />");
      Append (Schema, "         <element name=""Snap_Max"" type=""tns:Snap"" />");
      Append (Schema, "         <element name=""Crackle_Max"" type=""tns:Crackle"" />");
      Append (Schema, "         <element name=""Chord_Error_Max"" type=""tns:Length"" />");
      Append (Schema, "         <element name=""Axial_Scaler"" type=""tns:Position_Scale"" />");
      Append (Schema, "         <choice>");
      Append (Schema, "            <element name=""Core_XY"" type=""tns:Core_XY_Motor_Axis_Assignments"" />");
      Append (Schema, "            <element name=""Cartesian"" type=""tns:Cartesian_Motor_Axis_Assignments"" />");
      Append (Schema, "         </choice>");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Input_Switch_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Enabled"" type=""boolean"" />");
      Append (Schema, "         <element name=""Hit_On_High"" type=""boolean"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Input_Switches"">");
      Append (Schema, "      <sequence>");
      Append
        (Schema,
         "         <element name=""Input_Switch_1"" type=""tns:Input_Switch_Parameters"" tns:localise=""Input_Switch_1 (1)"" />");
      Append
        (Schema,
         "         <element name=""Input_Switch_2"" type=""tns:Input_Switch_Parameters"" tns:localise=""Input_Switch_2 (2)"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Double_Tap_Homing_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Switch"" type=""tns:Input_Switch_Name"" />");
      Append (Schema, "         <element name=""First_Move_Distance"" type=""tns:Length"" />");
      Append (Schema, "         <element name=""Back_Off_Move_Distance"" type=""tns:Length"" />");
      Append (Schema, "         <element name=""Second_Move_Distance"" type=""tns:Length"" />");
      Append (Schema, "         <element name=""Switch_Position"" type=""tns:Length"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Set_To_Value_Homing_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Value"" type=""tns:Length"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Axis_Homing_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <choice>");
      Append (Schema, "            <element name=""Double_Tap_Kind"" type=""tns:Double_Tap_Homing_Parameters"" />");
      Append
        (Schema, "            <element name=""Set_To_Value_Kind"" type=""tns:Set_To_Value_Homing_Parameters"" />");
      Append (Schema, "         </choice>");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Homing_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""X_Axis"" type=""tns:Axis_Homing_Parameters"" />");
      Append (Schema, "         <element name=""Y_Axis"" type=""tns:Axis_Homing_Parameters"" />");
      Append (Schema, "         <element name=""Z_Axis"" type=""tns:Axis_Homing_Parameters"" />");
      Append (Schema, "         <element name=""E_Axis"" type=""tns:Axis_Homing_Parameters"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Thermistor_Name"">");
      Append (Schema, "      <restriction base=""string"">");
      Append (Schema, "         <enumeration value=""Thermistor_1"" tns:localise=""Thermistor_1 (1)"" />");
      Append (Schema, "         <enumeration value=""Thermistor_2"" tns:localise=""Thermistor_2 (2)"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Disabled_Heater_Parameters"">");
      Append (Schema, "      <sequence />");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""PID_Heater_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Proportional_Scale"" type=""tns:Dimensionless"" />");
      Append (Schema, "         <element name=""Integral_Scale"" type=""tns:Dimensionless"" />");
      Append (Schema, "         <element name=""Derivative_Scale"" type=""tns:Dimensionless"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Bang_Bang_Heater_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Bang_Bang_Hysteresis"" type=""tns:Temperature"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Individual_Heater_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Thermistor"" type=""tns:Thermistor_Name"" />");
      Append (Schema, "         <element name=""Check_Max_Cumulative_Error"" type=""tns:Temperature"" />");
      Append (Schema, "         <element name=""Check_Gain_Time"" type=""tns:Time"" />");
      Append (Schema, "         <element name=""Check_Minimum_Gain"" type=""tns:Temperature"" />");
      Append (Schema, "         <element name=""Check_Hysteresis"" type=""tns:Temperature"" />");
      Append (Schema, "         <choice>");
      Append (Schema, "            <element name=""Disabled_Kind"" type=""tns:Disabled_Heater_Parameters"" />");
      Append (Schema, "            <element name=""PID_Kind"" type=""tns:PID_Heater_Parameters"" />");
      Append (Schema, "            <element name=""Bang_Bang_Kind"" type=""tns:Bang_Bang_Heater_Parameters"" />");
      Append (Schema, "         </choice>");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Heater_Parameters"">");
      Append (Schema, "      <sequence>");
      Append
        (Schema,
         "         <element name=""Heater_1"" tns:localise=""Heater_1 (1)"" type=""tns:Individual_Heater_Parameters"" />");
      Append
        (Schema,
         "         <element name=""Heater_2"" tns:localise=""Heater_2 (2)"" type=""tns:Individual_Heater_Parameters"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Disabled_Fan_Parameters"">");
      Append (Schema, "      <sequence />");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Dynamic_PWM_Fan_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Proportional_Scale"" type=""tns:Dimensionless"" />");
      Append (Schema, "         <element name=""Integral_Scale"" type=""tns:Dimensionless"" />");
      Append (Schema, "         <element name=""Derivative_Scale"" type=""tns:Dimensionless"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Always_On_Fan_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Bang_Bang_Hysteresis"" type=""tns:Temperature"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Individual_Fan_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <choice>");
      Append (Schema, "            <element name=""Disabled_Kind"" type=""tns:Disabled_Fan_Parameters"" />");
      Append (Schema, "            <element name=""Dynamic_PWM_Kind"" type=""tns:Dynamic_PWM_Fan_Parameters"" />");
      Append (Schema, "            <element name=""Always_On_Kind"" type=""tns:Always_On_Fan_Parameters"" />");
      Append (Schema, "         </choice>");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Fan_Parameters"">");
      Append (Schema, "      <sequence>");
      Append
        (Schema,
         "         <element name=""Fan_1"" tns:localise=""Fan_1 (1)"" type=""tns:Individual_Fan_Parameters"" />");
      Append
        (Schema,
         "         <element name=""Fan_2"" tns:localise=""Fan_2 (2)"" type=""tns:Individual_Fan_Parameters"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Disabled_G_Code_Heater_Parameters"">");
      Append (Schema, "      <sequence />");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Enabled_G_Code_Heater_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Assigned_Heater"" type=""tns:Heater_Name"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Individual_G_Code_Heater_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <choice>");
      Append (Schema, "            <element name=""Disabled_Kind"" type=""tns:Disabled_G_Code_Heater_Parameters"" />");
      Append (Schema, "            <element name=""Enabled_Kind"" type=""tns:Enabled_G_Code_Heater_Parameters"" />");
      Append (Schema, "         </choice>");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""G_Code_Assignment_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Bed_Heater"" type=""tns:Individual_G_Code_Heater_Parameters"" />");
      Append (Schema, "         <element name=""Hotend_Heater"" type=""tns:Individual_G_Code_Heater_Parameters"" />");
      Append (Schema, "         <element name=""Chamber_Heater"" type=""tns:Individual_G_Code_Heater_Parameters"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Prunt_Parameters"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Enabled"" type=""boolean"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <simpleType name=""Schema_Version"">");
      Append (Schema, "      <restriction base=""int"">");
      Append (Schema, "         <enumeration value=""1"" />");
      Append (Schema, "      </restriction>");
      Append (Schema, "   </simpleType>");
      Append (Schema, "");
      Append (Schema, "   <complexType name=""Config"">");
      Append (Schema, "      <sequence>");
      Append (Schema, "         <element name=""Schema_Version"" type=""tns:Schema_Version"" />");
      Append (Schema, "         <element name=""Prunt"" type=""tns:Prunt_Parameters"" />");
      Append (Schema, "         <element name=""Steppers"" type=""tns:Stepper_Parameters"" />");
      Append (Schema, "         <element name=""Kinematics"" type=""tns:Kinematic_Parameters"" />");
      Append (Schema, "         <element name=""Input_Switches"" type=""tns:Input_Switch_Parameters"" />");
      Append (Schema, "         <element name=""Homings"" type=""tns:Homing_Parameters"" />");
      Append (Schema, "         <element name=""Heaters"" type=""tns:Heater_Parameters"" />");
      Append (Schema, "         <element name=""Fans"" type=""tns:Fan_Parameters"" />");
      Append (Schema, "         <element name=""G_Code_Assignments"" type=""tns:G_Code_Assignment_Parameters"" />");
      Append (Schema, "      </sequence>");
      Append (Schema, "   </complexType>");
      Append (Schema, "");
      Append (Schema, "   <element name=""Config"" type=""tns:Config"" />");
      Append (Schema, "</schema>");

      return Schema;
   end Build_Schema;

   function Build_Schema return Schema.Validators.XML_Grammar is
      Input  : Input_Sources.Strings.String_Input;
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
