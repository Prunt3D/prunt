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
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Text_IO.Unbounded_IO;
with GNATCOLL;
with GNATCOLL.JSON;
with Prunt.TMC_Types;
with Prunt.TMC_Types.TMC2240;

package body Prunt.Config is

   pragma Unsuppress (All_Checks);

   Has_Heaters        : constant Boolean := Heater_Name'Pos (Heater_Name'Last) >= Heater_Name'Pos (Heater_Name'First);
   Has_Thermistors    : constant Boolean :=
     Thermistor_Name'Pos (Thermistor_Name'Last) >= Thermistor_Name'Pos (Thermistor_Name'First);
   Has_Fans           : constant Boolean := Fan_Name'Pos (Fan_Name'Last) >= Fan_Name'Pos (Fan_Name'First);
   Has_Input_Switches : constant Boolean :=
     Input_Switch_Name'Pos (Input_Switch_Name'Last) >= Input_Switch_Name'Pos (Input_Switch_Name'First);
   Has_Lasers         : constant Boolean := Laser_Name'Pos (Laser_Name'Last) >= Laser_Name'Pos (Laser_Name'First);
   Has_StallGuard2    : constant Boolean :=
     not (for all S in Stepper_Name => Stepper_Hardware (S).Kind not in TMC2240_UART_Kind);
   Has_StallGuard4    : constant Boolean :=
     not (for all S in Stepper_Name => Stepper_Hardware (S).Kind not in TMC2240_UART_Kind);

   function Build_Schema return Property_Maps.Map is separate;

   function JSON_To_User_Config (Data : JSON_Value) return User_Config is separate;

   use type TMC_Types.TMC2240.CHM_Type;

   pragma Warnings (Off, "cannot call * before body seen");
   --  TODO: Find a way to fix this.

   function My_Get_Long_Float (Val : JSON_Value) return Long_Float is
   begin
      if Kind (Val) = JSON_Float_Type then
         return Get_Long_Float (Val);
      elsif Kind (Val) = JSON_Int_Type then
         return Long_Float (Long_Long_Integer'(Get (Val)));
      else
         raise Constraint_Error with "Not a number.";
      end if;
   end My_Get_Long_Float;

   function My_Get_Long_Float (Val : JSON_Value; Field : UTF8_String) return Long_Float is
   begin
      return My_Get_Long_Float (Get (Val, Field));
   end My_Get_Long_Float;

   function Get_JSON_Integer (Val : JSON_Value; Field : UTF8_String) return T is
   begin
      return T (Long_Long_Integer'(Get (Get (Val, Field))));
   end Get_JSON_Integer;

   pragma Warnings (Off, "function ""Get"" is not referenced");
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_1);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_2);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_3);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_4);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_5);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_6);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_7);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_8);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_9);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_10);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_11);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_12);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_13);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_14);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_15);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_16);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_17);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_18);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_19);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_20);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_21);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_22);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_23);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_24);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_25);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_26);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_27);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_28);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_29);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_30);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_31);
   function Get is new Get_JSON_Integer (TMC_Types.Unsigned_32);
   pragma Warnings (On, "function ""Get"" is not referenced");

   function Get (Val : JSON_Value; Field : UTF8_String) return Dimensionless is
   begin
      return Dimensionless (My_Get_Long_Float (Val, Field));
   end Get;

   function Trim (S : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Side => Ada.Strings.Both);
   end Trim;

   function Schema_To_JSON (Schema : Property_Maps.Map) return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;

      procedure DFS (Node : Property_Maps.Map; Path : String) is
         use Property_Maps;
         use Discrete_String_Sets;
      begin
         Append (Result, "{");
         for I in Node.Iterate loop
            Append
              (Result,
               """" & Key (I) & """:{""Description"":""" & "<p>" & JSON_Escape (Element (I).Description) & "</p>");

            if Get_Board_Specific_Documentation (Path & (if Path = "" then "" else "$") & Key (I)) /= "" then
               Append
                 (Result,
                  "<p><br>Board-specific documentation:</p>"
                  & JSON_Escape (Get_Board_Specific_Documentation (Path & (if Path = "" then "" else "$") & Key (I))));
            end if;

            if Enable_Documentation_Dev_Mode then
               Append (Result, "<p>Schema key: " & Path & (if Path = "" then "" else "$") & Key (I) & "</p>");
            end if;

            Append (Result, """,");

            case Element (I).Kind is
               when Boolean_Kind =>
                  Append (Result, """Kind"":""Boolean""}");

               when Discrete_Kind =>
                  Append (Result, """Kind"":""Discrete"",""Options"":[");
                  declare
                     Options : constant Discrete_String_Sets.Set := Element (I).Discrete_Options;
                  begin
                     for J in Options.Iterate loop
                        Append (Result, """" & Element (J) & """");
                        if Element (J) /= Last_Element (Options) then
                           Append (Result, ",");
                        end if;
                     end loop;
                  end;
                  Append (Result, "]}");

               when Integer_Kind =>
                  Append
                    (Result,
                     """Kind"":""Integer"""
                     & ",""Min"":"
                     & Trim (Element (I).Integer_Min'Image)
                     & ",""Max"":"
                     & Trim (Element (I).Integer_Max'Image)
                     & ",""Unit"":"
                     & Element (I).Integer_Unit'Image
                     & "}");

               when Float_Kind =>
                  Append
                    (Result,
                     """Kind"":""Float"""
                     & ",""Min"":"
                     & Trim (Element (I).Float_Min'Image)
                     & ",""Max"":"
                     & Trim (Element (I).Float_Max'Image)
                     & ",""Unit"":"
                     & Element (I).Float_Unit'Image
                     & "}");

               when Float_Ratio_Kind =>
                  Append
                    (Result,
                     """Kind"":""Float_Ratio"""
                     & ",""Min"":"
                     & Trim (Element (I).Float_Ratio_Min'Image)
                     & ",""Max"":"
                     & Trim (Element (I).Float_Ratio_Max'Image)
                     & "}");

               when Sequence_Kind =>
                  if Element (I).Sequence_Tabbed then
                     Append (Result, """Kind"":""Tabbed_Sequence"",""Children"":");
                  else
                     Append (Result, """Kind"":""Sequence"",""Children"":");
                  end if;
                  DFS (Element (I).Sequence_Children, Path & (if Path = "" then "" else "$") & Key (I));
                  Append (Result, "}");

               when Variant_Kind =>
                  Append (Result, """Kind"":""Variant"",""Children"":");
                  DFS (Element (I).Variant_Children, Path & (if Path = "" then "" else "$") & Key (I));
                  Append (Result, "}");
            end case;
            if Key (I) /= Last_Key (Node) then
               Append (Result, ",");
            end if;
         end loop;
         Append (Result, "}");
      end DFS;
   begin
      DFS (Schema, "");
      return Result;
   end Schema_To_JSON;

   function Build_Flat_Schema (Schema : Property_Maps.Map) return Flat_Schemas.Map is
      Result : Flat_Schemas.Map;

      procedure DFS (Node : Property_Maps.Map; Path : String) is
         use Property_Maps;
         use Flat_Schemas;
         use Discrete_String_Sets;
      begin
         for I in Node.Iterate loop
            declare
               New_Path : constant String := Path & (if Path = "" then "" else "$") & Key (I);
            begin
               case Element (I).Kind is
                  when Boolean_Kind | Discrete_Kind | Integer_Kind | Float_Kind =>
                     Insert (Result, New_Path, Element (I));

                  when Float_Ratio_Kind =>
                     Insert (Result, New_Path & "$Numerator", Element (I));
                     Insert (Result, New_Path & "$Denominator", Element (I));

                  when Sequence_Kind =>
                     DFS (Element (I).Sequence_Children, New_Path);

                  when Variant_Kind =>
                     declare
                        Children : constant Property_Maps.Map := Element (I).Variant_Children;
                        Options  : Discrete_String_Sets.Set;
                     begin
                        for J in Children.Iterate loop
                           Insert (Options, Key (J));
                           --  TODO: Obviously an aggregate expression would be cleaner here, but GCC 14.2 appears
                           --  to have a bug that prevents this. This bug is fixed in 15 at the time of writing. A
                           --  standalone test program is available here:
                           --  https://gist.github.com/liampwll/87a498ebf6aa335dfa3cda0f9ef2dcc5
                        end loop;
                        Insert
                          (Result,
                           New_Path,
                           new Property_Parameters'
                             (Kind             => Discrete_Kind,
                              Description      => To_Unbounded_String (""),
                              Discrete_Default => Element (I).Variant_Default,
                              Discrete_Options => Options));
                        DFS (Children, New_Path);
                     end;
               end case;
            end;
         end loop;
      end DFS;
   begin
      DFS (Schema, "");
      return Result;
   end Build_Flat_Schema;

   protected body Config_File is

      procedure Patch
        (Data : in out Ada.Strings.Unbounded.Unbounded_String; Report : access procedure (Key, Message : String))
      is
         New_Data : constant JSON_Value := Read (Data);

         procedure Merge_Into_Config (Name : UTF8_String; Value : JSON_Value) is
         begin
            Set_Field (Get (Current_Properties, "Properties"), Name, Value);
         end Merge_Into_Config;

         Had_Schema_Error : Boolean := False;

         procedure Report_Schema_Error (Key, Message : String) is
         begin
            Had_Schema_Error := True;
            Report (Key, "Data does not match schema, no values will be saved: " & Message);
         end Report_Schema_Error;
      begin
         Validate_Config_To_Schema (New_Data, Report_Schema_Error'Access);

         if not Had_Schema_Error then
            Map_JSON_Object (New_Data, Merge_Into_Config'Access);
            Validate_Current_Config (Report);
            Write_File;
         end if;

         Get_Values (Data);
      end Patch;

      procedure Validate_Initial_Config (Report : access procedure (Key, Message : String)) is
      begin
         Maybe_Do_Init;
         Validate_Config (JSON_To_User_Config (Get (Initial_Properties, "Properties")), Report);
      end Validate_Initial_Config;

      procedure Validate_Current_Config (Report : access procedure (Key, Message : String)) is
      begin
         Maybe_Do_Init;
         Validate_Config (JSON_To_User_Config (Get (Current_Properties, "Properties")), Report);
      end Validate_Current_Config;

      procedure Write_File is
         File : Ada.Text_IO.File_Type;
      begin
         if Ada.Directories.Exists (Config_Path & "_backup_18") then
            --  Always keep the very oldest file just in case the user put an important comment in the original file.
            --  We can not easily detect if the user modified the file when a backup already existed, but this is
            --  better than nothing.
            if Ada.Directories.Exists (Config_Path & "_backup_19") then
               Ada.Directories.Delete_File (Config_Path & "_backup_18");
            else
               Ada.Directories.Rename (Old_Name => Config_Path & "_backup_18", New_Name => Config_Path & "_backup_19");
            end if;
         end if;

         for I in reverse 1 .. 17 loop
            if Ada.Directories.Exists (Config_Path & "_backup_" & Trim (I'Image)) then
               Ada.Directories.Rename
                 (Old_Name => Config_Path & "_backup_" & Trim (I'Image),
                  New_Name => Config_Path & "_backup_" & Trim (Integer (I + 1)'Image));
            end if;
         end loop;

         if Ada.Directories.Exists (Config_Path) then
            Ada.Directories.Rename (Old_Name => Config_Path, New_Name => Config_Path & "_backup_1");
         end if;

         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Config_Path);
         Ada.Text_IO.Put_Line (File, Write (Current_Properties));
         Ada.Text_IO.Close (File);
      exception
         when others =>
            if Ada.Text_IO.Is_Open (File) then
               Ada.Text_IO.Close (File);
            end if;
            raise;
      end Write_File;

      procedure Maybe_Do_Init is
         use Flat_Schemas;
      begin
         if Init_Failed then
            raise Constraint_Error with "Init failed, no further operations may be performed.";
         end if;

         if Init_Done then
            return;
         end if;

         if Is_Empty (Flat_Schema) then
            Schema := Build_Schema;
            Schema_JSON := Schema_To_JSON (Schema);
            Flat_Schema := Build_Flat_Schema (Schema);
         end if;

         if Ada.Directories.Exists (Config_Path) then
            declare
               File          : Ada.Text_IO.File_Type;
               File_Contents : Ada.Strings.Unbounded.Unbounded_String;
            begin
               Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Config_Path);
               while not Ada.Text_IO.End_Of_File (File) loop
                  Ada.Strings.Unbounded.Append (File_Contents, Ada.Text_IO.Unbounded_IO.Get_Line (File));
               end loop;
               Ada.Text_IO.Close (File);

               Current_Properties := Read (File_Contents);
            end;
         else
            Current_Properties := Create_Object;
            Set_Field (Current_Properties, "Schema version", Long_Integer'(11));
            Set_Field (Current_Properties, "Properties", Create_Object);
         end if;

         if Kind (Current_Properties) /= JSON_Object_Type then
            raise Config_File_Format_Error with "Config file should contain a JSON object.";
         end if;

         if not Has_Field (Current_Properties, "Schema version")
           or else Kind (Get (Current_Properties, "Schema version")) /= JSON_Int_Type
           or else not Has_Field (Current_Properties, "Properties")
           or else Kind (Get (Current_Properties, "Properties")) /= JSON_Object_Type
         then
            raise Config_File_Format_Error
              with "Config file format should be {""Schema version"": Integer, ""Properties"": {...}}.";
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(1) then
            --  Version 2 adds input shaper parameters.
            Set_Field (Current_Properties, "Schema version", Long_Integer'(2));
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(2) then
            --  Version 3 adds g-code default fan parameter.
            Set_Field (Current_Properties, "Schema version", Long_Integer'(3));
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(3) then
            --  Version 4 changes the range of HSTRT from 0..7 to 1..8.
            for S in Stepper_Name loop
               if Stepper_Hardware (S).Kind = TMC2240_UART_Kind then
                  Set_Field
                    (Get (Current_Properties, "Properties"),
                     "Steppers$" & S'Image & "$CHM$SpreadCycle$Manual$HSTRT",
                     Integer'
                       (Get
                          (Get (Current_Properties, "Properties"),
                           "Steppers$" & S'Image & "$CHM$SpreadCycle$Manual$HSTRT"))
                     + 1);
               end if;
            end loop;

            Set_Field (Current_Properties, "Schema version", Long_Integer'(4));
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(4) then
            --  Version 5 adds homing prerequisites.
            declare
               procedure Rename_Field_For_Axes (Field_Name : String) is
               begin
                  for A in Axis_Name loop
                     Set_Field
                       (Get (Current_Properties, "Properties"),
                        "Homing$" & A'Image & "$Homing method" & Field_Name,
                        JSON_Value'(Get (Get (Current_Properties, "Properties"), "Homing$" & A'Image & Field_Name)));
                     Unset_Field (Get (Current_Properties, "Properties"), "Homing$" & A'Image & Field_Name);
                  end loop;
               end Rename_Field_For_Axes;
            begin
               Rename_Field_For_Axes ("");
               Rename_Field_For_Axes ("$Set to value$Position");
               Rename_Field_For_Axes ("$Use input switch$Back off move distance");
               Rename_Field_For_Axes ("$Use input switch$First move distance");
               Rename_Field_For_Axes ("$Use input switch$Move to after");
               Rename_Field_For_Axes ("$Use input switch$Move towards negative infinity");
               Rename_Field_For_Axes ("$Use input switch$Second move distance");
               Rename_Field_For_Axes ("$Use input switch$Switch");
               Rename_Field_For_Axes ("$Use input switch$Switch position");
            end;

            Set_Field (Current_Properties, "Schema version", Long_Integer'(5));
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(5) then
            --  Version 6 changes the Core XY mapping function to match other motion controllers.
            if Get (Get (Current_Properties, "Properties"), "Kinematics$Kinematics kind") = "Core XY" then
               for S in Stepper_Name loop
                  if Get (Get (Current_Properties, "Properties"), "Kinematics$Kinematics kind$Core XY$" & S'Image)
                     in "A_AXIS" | "B_AXIS"
                  then
                     Set_Field_Long_Float
                       (Get (Current_Properties, "Properties"),
                        "Steppers$" & S'Image & "$Distance per step",
                        My_Get_Long_Float
                          (Get (Current_Properties, "Properties"), "Steppers$" & S'Image & "$Distance per step")
                        * 0.5);
                  end if;
               end loop;
            end if;

            Set_Field (Current_Properties, "Schema version", Long_Integer'(6));
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(6) then
            --  Version 7 adds more options for defining distance per step.
            for S in Stepper_Name loop
               declare
                  use TMC_Types.TMC2240;

                  MS_Lookup : constant array (Microstep_Resolution_Type) of Long_Float :=
                    (MS_256        => 256.0,
                     MS_128        => 128.0,
                     MS_64         => 64.0,
                     MS_32         => 32.0,
                     MS_16         => 16.0,
                     MS_8          => 8.0,
                     MS_4          => 4.0,
                     MS_2          => 2.0,
                     MS_Full_Steps => 1.0);

                  Microsteps : constant Long_Float :=
                    (if Stepper_Hardware (S).Kind = TMC2240_UART_Kind
                     then
                       MS_Lookup
                         (Microstep_Resolution_Type'Value
                            (Get (Get (Current_Properties, "Properties"), "Steppers$" & S'Image & "$MRES")))
                     else 1.0);

                  Distance : constant Long_Float :=
                    My_Get_Long_Float
                      (Get (Current_Properties, "Properties"), "Steppers$" & S'Image & "$Distance per step");
               begin
                  Set_Field_Long_Float
                    (Get (Current_Properties, "Properties"),
                     "Steppers$" & S'Image & "$Distance per step$Direct entry$Distance per step",
                     abs Distance * Microsteps);
                  Set_Field
                    (Get (Current_Properties, "Properties"),
                     "Steppers$" & S'Image & "$Distance per step$Direct entry$Reverse direction",
                     Distance < 0.0);
                  Set_Field
                    (Get (Current_Properties, "Properties"),
                     "Steppers$" & S'Image & "$Distance per step",
                     "Direct entry");
               end;
            end loop;

            Set_Field (Current_Properties, "Schema version", Long_Integer'(7));
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(7) then
            --  Version 8 adds IRUN during homing for TMC2240 drivers.
            for S in Stepper_Name loop
               if Stepper_Hardware (S).Kind = TMC2240_UART_Kind then
                  Set_Field_Long_Float
                    (Get (Current_Properties, "Properties"),
                     "Steppers$" & S'Image & "$IRUN during homing",
                     My_Get_Long_Float (Get (Current_Properties, "Properties"), "Steppers$" & S'Image & "$IRUN"));
               end if;
            end loop;

            Set_Field (Current_Properties, "Schema version", Long_Integer'(8));
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(8) then
            --  Version 9 adds velocity limits for homing.
            Set_Field (Current_Properties, "Schema version", Long_Integer'(9));
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(9) then
            --  Version 10 adds the ability to hide input switches from the user.
            for I in Input_Switch_Name loop
               if not Input_Switch_Visible_To_User (I) then
                  Unset_Field (Get (Current_Properties, "Properties"), "Input switches$" & I'Image & "$Enabled");
                  Unset_Field (Get (Current_Properties, "Properties"), "Input switches$" & I'Image & "$Hit on high");
               end if;
            end loop;

            for A in Axis_Name loop
               if not Input_Switch_Visible_To_User
                        (Input_Switch_Name'Value
                           (Get
                              (Get (Current_Properties, "Properties"),
                               "Homing$" & A'Image & "$Homing method$Use input switch$Switch")))
               then
                  Unset_Field
                    (Get (Current_Properties, "Properties"),
                     "Homing$" & A'Image & "$Homing method$Use input switch$Switch");
                  Set_Field
                    (Get (Current_Properties, "Properties"), "Homing$" & A'Image & "$Homing method", "Disabled");
               end if;
            end loop;

            Set_Field (Current_Properties, "Schema version", Long_Integer'(10));
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(10) then
            --  Version 11 adds StallGuard homing.
            Set_Field (Current_Properties, "Schema version", Long_Integer'(11));
         end if;

         if Get (Current_Properties, "Schema version") = Long_Integer'(11) then
            --  Version 12 moves pressure advance to a shaper.
            if My_Get_Long_Float (Get (Current_Properties, "Properties"), "Kinematics$Pressure advance time") /= 0.0
            then
               Set_Field
                 (Get (Current_Properties, "Properties"),
                  "Input shaping$" & Axis_Name'Image (E_Axis),
                  "Pressure advance");
               Set_Field_Long_Float
                 (Get (Current_Properties, "Properties"),
                  "Input shaping$" & Axis_Name'Image (E_Axis) & "$Pressure advance$Pressure advance time",
                  My_Get_Long_Float (Get (Current_Properties, "Properties"), "Kinematics$Pressure advance time"));
            end if;
            Unset_Field (Get (Current_Properties, "Properties"), "Kinematics$Pressure advance time");

            Set_Field (Current_Properties, "Schema version", Long_Integer'(12));
         end if;

         if Get (Current_Properties, "Schema version") /= Long_Integer'(12) then
            raise Config_File_Format_Error with "This config file is for a newer Prunt version.";
         end if;

         for X in Flat_Schema.Iterate loop
            if not Has_Field (Get (Current_Properties, "Properties"), Key (X)) then
               case Element (X).Kind is
                  when Boolean_Kind =>
                     Set_Field (Get (Current_Properties, "Properties"), Key (X), Element (X).Boolean_Default);

                  when Discrete_Kind =>
                     Set_Field (Get (Current_Properties, "Properties"), Key (X), Element (X).Discrete_Default);

                  when Integer_Kind =>
                     Set_Field (Get (Current_Properties, "Properties"), Key (X), Create (Element (X).Integer_Default));

                  when Float_Kind =>
                     Set_Field_Long_Float (Get (Current_Properties, "Properties"), Key (X), Element (X).Float_Default);

                  when Float_Ratio_Kind =>
                     if Key (X) (Key (X)'Last - String'("$Numerator")'Length + 1 .. Key (X)'Last) = "$Numerator" then
                        Set_Field_Long_Float
                          (Get (Current_Properties, "Properties"), Key (X), Element (X).Float_Ratio_Default_Numerator);
                     elsif Key (X) (Key (X)'Last - String'("$Denominator")'Length + 1 .. Key (X)'Last) = "$Denominator"
                     then
                        Set_Field_Long_Float
                          (Get (Current_Properties, "Properties"),
                           Key (X),
                           Element (X).Float_Ratio_Default_Denominator);
                     else
                        raise Constraint_Error;
                     end if;

                  when Sequence_Kind | Variant_Kind =>
                     raise Constraint_Error with "Field type should not exist here: " & Element (X).Kind'Image;
               end case;
            end if;
         end loop;

         declare
            Has_Errors : Boolean := False;

            procedure Report (Key, Message : String) is
            begin
               Ada.Text_IO.Put_Line ("Schema mismatch: " & Key & ": " & Message);
               Has_Errors := True;
            end Report;
         begin
            Validate_Config_To_Schema (Get (Current_Properties, "Properties"), Report'Access);

            if Has_Errors then
               Init_Failed := True;
               raise Config_File_Format_Error
                 with
                   "Config file errors logged to console. If you did not edit the config file by hand then this "
                   & "is a bug in Prunt.";
            end if;
         end;

         Write_File;

         declare
            procedure Handle_Invalid_Config (Key, Message : String) is
               pragma Unreferenced (Key);
               pragma Unreferenced (Message);
            begin
               Initial_Config_Valid := False;
               Get (Current_Properties, "Properties").Set_Field ("Prunt$Enabled", False);
               Write_File;
            end Handle_Invalid_Config;
         begin
            Initial_Config_Valid := True;
            Validate_Config
              (JSON_To_User_Config (Get (Current_Properties, "Properties")), Handle_Invalid_Config'Access);

            if Initial_Config_Valid then
               Initial_Config := User_Config_To_Config (JSON_To_User_Config (Get (Current_Properties, "Properties")));
            end if;
         end;

         Initial_Properties := Clone (Current_Properties);

         Init_Done := True;
      end Maybe_Do_Init;

      procedure Reset is
      begin
         Init_Done := False;
         Init_Failed := False;
         Initial_Config_Valid := False;
      end Reset;

      procedure Disable_Prunt is
      begin
         Maybe_Do_Init;
         Get (Current_Properties, "Properties").Set_Field ("Prunt$Enabled", False);
         Write_File;
      end Disable_Prunt;

      procedure Read (Data : out Prunt_Parameters) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.Prunt;
      end Read;

      procedure Read (Data : out Stepper_Parameters; Stepper : Stepper_Name) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.Steppers (Stepper);
      end Read;

      procedure Read (Data : out Kinematics_Parameters) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.Kinematics;
      end Read;

      procedure Read (Data : out Input_Switch_Parameters; Input_Switch : Input_Switch_Name) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.Switches (Input_Switch);
      end Read;

      procedure Read (Data : out Homing_Parameters; Axis : Axis_Name) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.Homing (Axis);
      end Read;

      procedure Read (Data : out Thermistor_Parameters; Thermistor : Thermistor_Name) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.Thermistors (Thermistor);
      end Read;

      procedure Read (Data : out Heater_Full_Parameters; Heater : Heater_Name) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.Heaters (Heater);
      end Read;

      procedure Read (Data : out Fan_Parameters; Fan : Fan_Name) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.Fans (Fan);
      end Read;

      procedure Read (Data : out G_Code_Assignment_Parameters) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.G_Code_Assignments;
      end Read;

      procedure Read (Data : out Shaper_Parameters; Axis : Axis_Name) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.Shapers (Axis);
      end Read;

      pragma Warnings (Off, "value not in range of type ""Laser_Name"" *");
      --  TODO: This is obviously a GCC bug as the parameter is of the type in question, but it is difficult to isolate
      --  this bug.
      procedure Read (Data : out Laser_Parameters; Laser : Laser_Name) is
      begin
         Maybe_Do_Init;
         Error_If_Initial_Config_Invalid;
         Data := Initial_Config.Lasers (Laser);
      end Read;
      pragma Warnings (On, "value not in range of type ""Laser_Name"" *");

      procedure Validate_Config_To_Schema (Config : JSON_Value; Report : access procedure (Key, Message : String)) is
         procedure Check_Field (Name : UTF8_String; Value : JSON_Value) is
            use Flat_Schemas;
         begin
            if not Flat_Schema.Contains (Name) then
               Report (Name, "Key is not a recognised parameter name.");
               return;
            end if;

            case Element (Flat_Schema, Name).Kind is
               when Boolean_Kind =>
                  if Kind (Value) /= JSON_Boolean_Type then
                     Report (Name, "Element must be boolean.");
                  end if;

               when Discrete_Kind =>
                  if Kind (Value) /= JSON_String_Type then
                     Report (Name, "Element must be string.");
                  elsif not Discrete_String_Sets.Contains (Element (Flat_Schema, Name).Discrete_Options, Get (Value))
                  then
                     Report (Name, "Element must be one of: " & Element (Flat_Schema, Name).Discrete_Options'Image);
                  end if;

               when Integer_Kind =>
                  if Kind (Value) = JSON_Float_Type then
                     Report (Name, "Element must be integer, floats are not allowed even if they are whole numbers.");
                  elsif Kind (Value) /= JSON_Int_Type then
                     Report (Name, "Element must be integer.");
                  elsif Get (Value) < Element (Flat_Schema, Name).Integer_Min then
                     Report
                       (Name, "Element must not be less than " & Trim (Element (Flat_Schema, Name).Integer_Min'Image));
                  elsif Get (Value) > Element (Flat_Schema, Name).Integer_Max then
                     Report
                       (Name,
                        "Element must not be greater than " & Trim (Element (Flat_Schema, Name).Integer_Max'Image));
                  end if;

               when Float_Kind =>
                  if Kind (Value) not in JSON_Float_Type | JSON_Int_Type then
                     Report (Name, "Element must be float or integer (automatically upcast).");
                  elsif My_Get_Long_Float (Value) < Element (Flat_Schema, Name).Float_Min then
                     Report
                       (Name, "Element must not be less than " & Trim (Element (Flat_Schema, Name).Float_Min'Image));
                  elsif My_Get_Long_Float (Value) > Element (Flat_Schema, Name).Float_Max then
                     Report
                       (Name,
                        "Element must not be greater than " & Trim (Element (Flat_Schema, Name).Float_Max'Image));
                  end if;

               when Float_Ratio_Kind =>
                  declare
                     function Get_Split_Point return Positive is
                     begin
                        for I in reverse Name'Range loop
                           if Name (I) = '$' then
                              return I - 1;
                           end if;
                        end loop;

                        raise Constraint_Error;
                     end Get_Split_Point;

                     Prefix : constant String := Name (Name'First .. Get_Split_Point);
                  begin

                     if (not Has_Field (Config, Prefix & "$Numerator"))
                       or else (not Has_Field (Config, Prefix & "$Denominator"))
                     then
                        --  TODO: Document that numerator and denominator must both be present.
                        Report
                          (Name,
                           "Both numerator and denominator must be present. "
                           & "This is an error in the config editor, not in the entered values.");
                     elsif Kind (Get (Config, Prefix & "$Numerator")) not in JSON_Float_Type | JSON_Int_Type then
                        if Name = Prefix & "$Numerator" then
                           --  Only report errors once per ratio.
                           Report (Prefix & "$Numerator", "Element must be float or integer (automatically upcast).");
                        end if;
                     elsif Kind (Get (Config, Prefix & "$Denominator")) not in JSON_Float_Type | JSON_Int_Type then
                        if Name = Prefix & "$Denominator" then
                           --  Only report errors once per ratio.
                           Report
                             (Prefix & "$Denominator", "Element must be float or integer (automatically upcast).");
                        end if;
                     elsif My_Get_Long_Float (Config, Prefix & "$Denominator") = 0.0 then
                        if Name = Prefix & "$Denominator" then
                           --  Only report errors once per ratio.
                           Report (Name, "Denominator must not be zero.");
                        end if;
                     elsif My_Get_Long_Float (Config, Prefix & "$Numerator")
                       / My_Get_Long_Float (Config, Prefix & "$Denominator")
                       > Element (Flat_Schema, Name).Float_Ratio_Max
                     then
                        if Name = Prefix & "$Numerator" then
                           --  Only report errors once per ratio.
                           Report
                             (Name,
                              "A/B for A:B must not be greater than "
                              & Element (Flat_Schema, Name).Float_Ratio_Max'Image);
                        end if;
                     elsif My_Get_Long_Float (Config, Prefix & "$Numerator")
                       / My_Get_Long_Float (Config, Prefix & "$Denominator")
                       < Element (Flat_Schema, Name).Float_Ratio_Min
                     then
                        if Name = Prefix & "$Numerator" then
                           Report
                             (Name,
                              "A/B for A:B must not be less than "
                              & Element (Flat_Schema, Name).Float_Ratio_Min'Image);
                        end if;
                     end if;
                  end;

               when Sequence_Kind | Variant_Kind =>
                  raise Constraint_Error with "Field type should not exist here: " & Element (Flat_Schema, Name)'Image;
            end case;
         end Check_Field;
      begin
         if Kind (Config) /= JSON_Object_Type then
            Report ("Something is very broken if you're seeing this.", "Config file should contain a JSON object.");
            return;
         end if;

         Map_JSON_Object (Config, Check_Field'Access);
      end Validate_Config_To_Schema;

      procedure Get_Schema (Schema : out Ada.Strings.Unbounded.Unbounded_String) is
      begin
         Maybe_Do_Init;
         Schema := Schema_JSON;
      end Get_Schema;

      procedure Get_Values (Values : out Ada.Strings.Unbounded.Unbounded_String) is
      begin
         Maybe_Do_Init;
         Values := Write (Get (Current_Properties, "Properties"));
      end Get_Values;

      procedure Error_If_Initial_Config_Invalid is
      begin
         if not Init_Done then
            raise Constraint_Error with "Init not done.";
         end if;

         if not Initial_Config_Valid then
            raise Constraint_Error with "Config is invalid, this procedure should not have been called.";
         end if;
      end Error_If_Initial_Config_Invalid;

      procedure Get_Values_And_Validate
        (Report : access procedure (Key, Message : String); Values : out Ada.Strings.Unbounded.Unbounded_String) is
      begin
         Validate_Current_Config (Report);
         Get_Values (Values);
      end Get_Values_And_Validate;

      procedure Prunt_Is_Enabled (Result : out Boolean) is
      begin
         Maybe_Do_Init;
         Result := Get (Get (Initial_Properties, "Properties"), "Prunt$Enabled");
      end Prunt_Is_Enabled;

   end Config_File;

   procedure Disable_Prunt is
   begin
      Config_File.Disable_Prunt;
   end Disable_Prunt;

   function Read return Prunt_Parameters is
      Data : Prunt_Parameters;
   begin
      Config_File.Read (Data);
      return Data;
   end Read;

   function Read (Stepper : Stepper_Name) return Stepper_Parameters is
      Data : Stepper_Parameters;
   begin
      Config_File.Read (Data, Stepper);
      return Data;
   end Read;

   function Read return Kinematics_Parameters is
      Data : Kinematics_Parameters;
   begin
      Config_File.Read (Data);
      return Data;
   end Read;

   function Read (Input_Switch : Input_Switch_Name) return Input_Switch_Parameters is
      Data : Input_Switch_Parameters;
   begin
      Config_File.Read (Data, Input_Switch);
      return Data;
   end Read;

   function Read (Axis : Axis_Name) return Homing_Parameters is
      Data : Homing_Parameters;
   begin
      Config_File.Read (Data, Axis);
      return Data;
   end Read;

   function Read (Thermistor : Thermistor_Name) return Thermistor_Parameters is
      Data : Thermistor_Parameters;
   begin
      Config_File.Read (Data, Thermistor);
      return Data;
   end Read;

   function Read (Heater : Heater_Name) return Heater_Full_Parameters is
      Data : Heater_Full_Parameters;
   begin
      Config_File.Read (Data, Heater);
      return Data;
   end Read;

   function Read (Fan : Fan_Name) return Fan_Parameters is
      Data : Fan_Parameters;
   begin
      Config_File.Read (Data, Fan);
      return Data;
   end Read;

   function Read return G_Code_Assignment_Parameters is
      Data : G_Code_Assignment_Parameters;
   begin
      Config_File.Read (Data);
      return Data;
   end Read;

   function Read (Axis : Axis_Name) return Shaper_Parameters is
      Data : Shaper_Parameters;
   begin
      Config_File.Read (Data, Axis);
      return Data;
   end Read;

   pragma Warnings (Off, "value not in range of type ""Laser_Name"" *");
   --  TODO: This is obviously a GCC bug as the parameter is of the type in question, but it is difficult to isolate
   --  this bug.
   function Read (Laser : Laser_Name) return Laser_Parameters is
      Data : Laser_Parameters;
   begin
      Config_File.Read (Data, Laser);
      return Data;
   end Read;
   pragma Warnings (On, "value not in range of type ""Laser_Name"" *");

   procedure Patch
     (Data : in out Ada.Strings.Unbounded.Unbounded_String; Report : access procedure (Key, Message : String)) is
   begin
      Config_File.Patch (Data, Report);
   end Patch;

   procedure Validate_Initial_Config (Report : access procedure (Key, Message : String)) is
   begin
      Config_File.Validate_Initial_Config (Report);
   end Validate_Initial_Config;

   procedure Validate_Current_Config (Report : access procedure (Key, Message : String)) is
   begin
      Config_File.Validate_Current_Config (Report);
   end Validate_Current_Config;

   function Get_Schema return Ada.Strings.Unbounded.Unbounded_String is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Config_File.Get_Schema (Result);
      return Result;
   end Get_Schema;

   function Get_Values return Ada.Strings.Unbounded.Unbounded_String is
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Config_File.Get_Values (Result);
      return Result;
   end Get_Values;

   function Get_Values_And_Validate
     (Report : access procedure (Key, Message : String)) return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Values : Ada.Strings.Unbounded.Unbounded_String do
         Config_File.Get_Values_And_Validate (Report, Values);
      end return;
   end Get_Values_And_Validate;

   function Prunt_Is_Enabled return Boolean is
   begin
      return Result : Boolean do
         Config_File.Prunt_Is_Enabled (Result);
      end return;
   end Prunt_Is_Enabled;

   procedure Reset is
   begin
      Config_File.Reset;
   end Reset;

   procedure Validate_Config (Config : User_Config; Report : access procedure (Key, Message : String)) is
   begin
      --  TODO: Get rid of the reference strings below and replace them with something generated by
      --  config_codegen.py.

      if not Config.Prunt.Enabled then
         Report ("Prunt$Enabled", "Prunt is disabled. Enable after setting other options.");
      end if;

      for S in Stepper_Name loop
         declare
            Stepper_Enabled : Boolean;
         begin
            case Stepper_Hardware (S).Kind is
               when Basic_Kind =>
                  Stepper_Enabled := Config.Steppers (S).Basic_Kind.Enabled;

               when TMC2240_UART_Kind =>
                  Stepper_Enabled := Config.Steppers (S).TMC_2240_Kind.Enabled;
            end case;

            if not Stepper_Enabled then
               case Config.Kinematics.Kinematics_Kind.Kind is
                  when Cartesian =>
                     if Config.Kinematics.Kinematics_Kind.Cartesian (S) /= None then
                        Report
                          ("Steppers$" & S'Image & "$Enabled",
                           "Stepper is attached to "
                           & Config.Kinematics.Kinematics_Kind.Cartesian (S)'Image
                           & " but stepper is not enabled.");
                     end if;

                  when Core_XY =>
                     if Config.Kinematics.Kinematics_Kind.Core_XY (S) /= None then
                        Report
                          ("Steppers$" & S'Image & "$Enabled",
                           "Stepper is attached to "
                           & Config.Kinematics.Kinematics_Kind.Core_XY (S)'Image
                           & " but stepper is not enabled.");
                     end if;
               end case;
            end if;
         end;
      end loop;

      for S in Stepper_Name loop
         case Stepper_Hardware (S).Kind is
            when Basic_Kind =>
               null;

            when TMC2240_UART_Kind =>
               declare
                  Sum_Too_High                : Boolean;
                  Sum_Too_High_For_Full_Scale : Boolean;
                  Excessive_Heating           : Boolean;
                  Driver_Voltage_Too_Low      : Boolean;
                  HSTRT                       : TMC_Types.Unsigned_3;
                  HEND                        : TMC_Types.Unsigned_4;
                  IRUN                        : constant TMC_Types.Unsigned_5 :=
                    TMC_Types.Unsigned_5
                      (Dimensionless'Max
                         (0.0, Dimensionless'Floor (Config.Steppers (S).TMC_2240_Kind.IRUN * 32.0 - 1.0)));
                  IRUN_During_Homing          : constant TMC_Types.Unsigned_5 :=
                    TMC_Types.Unsigned_5
                      (Dimensionless'Max
                         (0.0,
                          Dimensionless'Floor (Config.Steppers (S).TMC_2240_Kind.IRUN_During_Homing * 32.0 - 1.0)));
               begin
                  if Config.Steppers (S).TMC_2240_Kind.Enabled then
                     if IRUN_During_Homing > IRUN then
                        Report
                          ("Steppers$" & S'Image & "$IRUN during homing",
                           "IRUN during homing must be less than or equal to IRUN.");
                     end if;
                  end if;

                  if not Config.Steppers (S).TMC_2240_Kind.Enabled then
                     null;
                  elsif Config.Steppers (S).TMC_2240_Kind.CHM.Kind = SpreadCycle
                    and then Config.Steppers (S).TMC_2240_Kind.CHM.SpreadCycle.Kind = Derived
                  then
                     TMC_Types.TMC2240.Optimize_Spreadcycle
                       (Driver_Voltage              =>
                          Config.Steppers (S).TMC_2240_Kind.CHM.SpreadCycle.Derived.Input_Voltage,
                        TBL                         => Config.Steppers (S).TMC_2240_Kind.TBL,
                        Motor_Inductance            =>
                          Config.Steppers (S).TMC_2240_Kind.CHM.SpreadCycle.Derived.Phase_Inductance,
                        Motor_Resistance            =>
                          Config.Steppers (S).TMC_2240_Kind.CHM.SpreadCycle.Derived.Phase_Resistance,
                        Motor_Peak_Current          => Config.Steppers (S).TMC_2240_Kind.Run_Current,
                        TOFF                        => Config.Steppers (S).TMC_2240_Kind.TOFF,
                        IRUN                        => IRUN,
                        HSTRT                       => HSTRT,
                        HEND                        => HEND,
                        Sum_Too_High                => Sum_Too_High,
                        Sum_Too_High_For_Full_Scale => Sum_Too_High_For_Full_Scale,
                        Excessive_Heating           => Excessive_Heating,
                        Driver_Voltage_Too_Low      => Driver_Voltage_Too_Low);
                     if Sum_Too_High then
                        Report
                          ("Steppers$" & S'Image & "$CHM$SpreadCycle$Derived",
                           "Automatically computed hysteresis sum is too high. Check that motor parameters are "
                           & "correct. If parameters are correct then decrease TBL, decrease IRUN, or use manual "
                           & "tuning.");
                     elsif Sum_Too_High_For_Full_Scale and IRUN = 31 then
                        Report
                          ("Steppers$" & S'Image & "$CHM$SpreadCycle$Derived",
                           "Automatically computed hysteresis sum is too high. Check that motor parameters are "
                           & "correct. If parameters are correct then decrease TBL, decrease IRUN, or use manual "
                           & "tuning. A very small reduction of IRUN to 0.97 will allow the computed parameters "
                           & "to be used.");
                     end if;

                     if Excessive_Heating then
                        Report
                          ("Steppers$" & S'Image & "$CHM$SpreadCycle$Derived",
                           "The stepper motor is likely to heat up excessively at the given driver voltage. "
                           & "Check that parameters are correct. If parameters are correct and you still want to "
                           & "use this motor then use manual tuning.");
                     end if;

                     if Driver_Voltage_Too_Low then
                        Report
                          ("Steppers$" & S'Image & "$CHM$SpreadCycle$Derived",
                           "The stepper motor requires a higher driver voltage to reach full current. Check that "
                           & "parameters are correct. If parameters are correct and you still want to use this "
                           & "motor then use manual tuning.");
                     end if;
                  elsif Config.Steppers (S).TMC_2240_Kind.CHM.Kind = SpreadCycle
                    and then Config.Steppers (S).TMC_2240_Kind.CHM.SpreadCycle.Kind = Manual
                  then
                     if IRUN = 31
                       and then Config.Steppers (S).TMC_2240_Kind.CHM.SpreadCycle.Manual.HEND
                                + Config.Steppers (S).TMC_2240_Kind.CHM.SpreadCycle.Manual.HSTRT
                                > 14
                     then
                        --  The TMC2240 datasheet says that the maximum here is 15 rather than 14, but that looks
                        --  to be an off-by-one error as the default sine wave peak is 248. 248 + 16/2 = 256 but
                        --  the maximum is probably actually 255.
                        Report
                          ("Steppers$" & S'Image & "$CHM$SpreadCycle$Manual",
                           "HSTRT + HEND must be less than 15 unless IRUN is reduced to 0.97 or below as "
                           & "otherwise the hysteresis start setting will be greater than the full scale "
                           & "current, leading to incorrect operation.");
                     end if;
                  end if;
               end;
         end case;
      end loop;

      for F in Fan_Name loop
         case Fan_Hardware (F).Kind is
            when Fixed_Switching_Kind =>
               if Config.Fans (F).Fixed_Switching.PWM_Frequency > Fan_Hardware (F).Maximum_PWM_Frequency then
                  Report
                    ("Fans$" & F'Image & "$PWM frequency",
                     "Fan frequency must be less than or equal to "
                     & Trim (Fan_Hardware (F).Maximum_PWM_Frequency'Image)
                     & " Hz.");
               end if;

            when Low_Or_High_Side_Switching_Kind =>
               if Config.Fans (F).Low_Or_High_Side_Switching.Use_High_Side_Switching then
                  if Config.Fans (F).Low_Or_High_Side_Switching.PWM_Frequency
                    > Fan_Hardware (F).Maximum_High_Side_PWM_Frequency
                  then
                     Report
                       ("Fans$" & F'Image & "$PWM frequency",
                        "Fan frequency must be less than or equal to "
                        & Trim (Fan_Hardware (F).Maximum_High_Side_PWM_Frequency'Image)
                        & " Hz when in high-side switching mode, which is currently selected (the limit is "
                        & Trim (Fan_Hardware (F).Maximum_Low_Side_PWM_Frequency'Image)
                        & " in low-side mode).");
                  end if;
               else
                  if Config.Fans (F).Low_Or_High_Side_Switching.PWM_Frequency
                    > Fan_Hardware (F).Maximum_Low_Side_PWM_Frequency
                  then
                     Report
                       ("Fans$" & F'Image & "$PWM frequency",
                        "Fan frequency must be less than or equal to "
                        & Trim (Fan_Hardware (F).Maximum_Low_Side_PWM_Frequency'Image)
                        & " Hz when in low-side switching mode, which is currently selected (the limit is "
                        & Trim (Fan_Hardware (F).Maximum_High_Side_PWM_Frequency'Image)
                        & " in high-side mode).");
                  end if;
               end if;
         end case;
      end loop;

      for Start_Axis in Axis_Name loop
         declare
            Visited_Axes : array (Axis_Name) of Boolean := (for A in Axis_Name => A = Start_Axis);

            procedure Report_Loops (Current_Axis : Axis_Name; Report_Location : Axis_Name; Path : String) is
               --  Report_Location is set in second iteration, set to Start_Axis in first.
            begin
               for Next_Axis in Axis_Name loop
                  if Next_Axis /= Current_Axis then
                     --  Self-references are detected elsewhere and a better error message is emitted, so we ignore
                     --  them here.
                     if Config.Homing (Current_Axis).Prerequisites (Next_Axis).Kind /= No_Requirement then
                        if Visited_Axes (Next_Axis) then
                           if Next_Axis = Start_Axis then
                              --  Only report within the loop, not in paths that lead in to it.
                              Report
                                ("Homing$" & Start_Axis'Image & "$Prerequisites$" & Report_Location'Image,
                                 "Loop detected in homing prerequisites: " & Path & " -> " & Next_Axis'Image & ".");
                           end if;
                        else
                           Visited_Axes (Next_Axis) := True;
                           Report_Loops
                             (Next_Axis,
                              (if Report_Location = Start_Axis then Next_Axis else Report_Location),
                              Path & " -> " & Next_Axis'Image);
                        end if;
                     end if;
                  end if;
               end loop;
            end Report_Loops;
         begin
            if Config.Homing (Start_Axis).Prerequisites (Start_Axis).Kind /= No_Requirement then
               Report
                 ("Homing$" & Start_Axis'Image & "$Prerequisites$" & Start_Axis'Image,
                  "The homing procedure for an axis can not have itself as a prerequisite.");
            end if;
            Report_Loops (Start_Axis, Start_Axis, Start_Axis'Image);
         end;
      end loop;

      for A in Axis_Name loop
         for B in Axis_Name loop
            if Config.Homing (A).Prerequisites (B).Kind = Must_Be_At_Position then
               declare
                  Position    : constant Length := Config.Homing (A).Prerequisites (B).Must_Be_At_Position.Position;
                  Lower_Limit : constant Length := Config.Kinematics.Lower_Position_Limit (B);
                  Upper_Limit : constant Length := Config.Kinematics.Upper_Position_Limit (B);
               begin
                  if Position < Lower_Limit or Position > Upper_Limit then
                     Report
                       ("Homing$" & A'Image & "$Prerequisites$" & B'Image & "$Must be at position$Position",
                        "Position is outside of axis limits ("
                        & Lower_Limit'Image
                        & " mm to "
                        & Upper_Limit'Image
                        & " mm).");
                  end if;
               end;
            end if;
         end loop;
      end loop;

      for A in Axis_Name loop
         declare
            Position    : Length;
            Lower_Limit : constant Length := Config.Kinematics.Lower_Position_Limit (A);
            Upper_Limit : constant Length := Config.Kinematics.Upper_Position_Limit (A);
            Do_Check    : Boolean := False;
         begin
            case Config.Homing (A).Homing_Method.Kind is
               when Use_Input_Switch =>
                  Position := Config.Homing (A).Homing_Method.Use_Input_Switch.Move_To_After;
                  if Position < Lower_Limit or Position > Upper_Limit then
                     Report
                       ("Homing$" & A'Image & "$Homing method$Use input switch$Move to after",
                        "Position is outside of axis limits ("
                        & Lower_Limit'Image
                        & " mm to "
                        & Upper_Limit'Image
                        & " mm).");
                  end if;

               when Use_StallGuard2 =>
                  Position := Config.Homing (A).Homing_Method.Use_StallGuard2.Move_To_After;
                  if Position < Lower_Limit or Position > Upper_Limit then
                     Report
                       ("Homing$" & A'Image & "$Homing method$Use StallGuard2$Move to after",
                        "Position is outside of axis limits ("
                        & Lower_Limit'Image
                        & " mm to "
                        & Upper_Limit'Image
                        & " mm).");
                  end if;

               when Use_StallGuard4 =>
                  Position := Config.Homing (A).Homing_Method.Use_StallGuard4.Move_To_After;
                  if Position < Lower_Limit or Position > Upper_Limit then
                     Report
                       ("Homing$" & A'Image & "$Homing method$Use StallGuard4$Move to after",
                        "Position is outside of axis limits ("
                        & Lower_Limit'Image
                        & " mm to "
                        & Upper_Limit'Image
                        & " mm).");
                  end if;

               when others =>
                  null;
            end case;
         end;
      end loop;

      for A in Axis_Name loop
         declare
            Motor              : Stepper_Name;
            Homing_Method_Name : constant String :=
              (if Config.Homing (A).Homing_Method.Kind = Use_StallGuard2
               then "Use StallGuard2"
               else "Use StallGuard4");
            Do_Check           : Boolean := False;
         begin
            case Config.Homing (A).Homing_Method.Kind is
               when Use_StallGuard2 =>
                  Motor := Config.Homing (A).Homing_Method.Use_StallGuard2.Motor;
                  Do_Check := True;

               when Use_StallGuard4 =>
                  Motor := Config.Homing (A).Homing_Method.Use_StallGuard4.Motor;
                  Do_Check := True;

               when others =>
                  null;
            end case;

            if Do_Check then
               case Config.Kinematics.Kinematics_Kind.Kind is
                  when Cartesian =>
                     declare
                        Motor_Axis : constant User_Config_Cartesian_Axis_Name :=
                          Config.Kinematics.Kinematics_Kind.Cartesian (Motor);
                     begin
                        if not (Motor_Axis'Image = A'Image) then
                           Report
                             ("Homing$" & A'Image & "$Homing method$" & Homing_Method_Name & "$Motor",
                              "Motor is not assigned to this axis, motor is assigned to " & Motor_Axis'Image & ".");
                        end if;
                     end;

                  when Core_XY =>
                     declare
                        Motor_Axis : constant User_Config_Core_XY_Axis_Name :=
                          Config.Kinematics.Kinematics_Kind.Core_XY (Motor);
                     begin
                        if not ((A = X_Axis and then Motor_Axis in A_Axis | B_Axis)
                                or else (A = Y_Axis and then Motor_Axis in A_Axis | B_Axis)
                                or else Motor_Axis'Image = A'Image)
                        then
                           Report
                             ("Homing$" & A'Image & "$Homing method$" & Homing_Method_Name & "$Motor",
                              "Motor is not assigned to this axis, motor is assigned to " & Motor_Axis'Image & ".");
                        end if;
                     end;
               end case;
            end if;
         end;
      end loop;
   end Validate_Config;

   function User_Config_To_Config (Data : User_Config) return Full_Config is
      Config : Full_Config;

      function Get_Distance_Per_Step (S : Stepper_Name) return Length is
         use TMC_Types.TMC2240;

         MS_Lookup : constant array (Microstep_Resolution_Type) of Dimensionless :=
           (MS_256        => 256.0,
            MS_128        => 128.0,
            MS_64         => 64.0,
            MS_32         => 32.0,
            MS_16         => 16.0,
            MS_8          => 8.0,
            MS_4          => 4.0,
            MS_2          => 2.0,
            MS_Full_Steps => 1.0);

         Microsteps      : constant Dimensionless :=
           (if Stepper_Hardware (S).Kind = Basic_Kind
            then Data.Steppers (S).Basic_Kind.Microsteps
            elsif Stepper_Hardware (S).Kind = TMC2240_UART_Kind
            then MS_Lookup (Data.Steppers (S).TMC_2240_Kind.MRES)
            else raise Constraint_Error);
         Distance_Config : constant User_Config_Steppers_Distance_Per_Step :=
           (if Stepper_Hardware (S).Kind = Basic_Kind
            then Data.Steppers (S).Basic_Kind.Distance_Per_Step
            elsif Stepper_Hardware (S).Kind = TMC2240_UART_Kind
            then Data.Steppers (S).TMC_2240_Kind.Distance_Per_Step
            else raise Constraint_Error);
      begin
         case Distance_Config.Kind is
            when Direct_Entry =>
               declare
                  Direction_Multiplier : constant Dimensionless :=
                    (if Distance_Config.Direct_Entry.Reverse_Direction then -1.0 else 1.0);
                  Distance             : constant Length := Distance_Config.Direct_Entry.Distance_Per_Step;
               begin
                  return Direction_Multiplier * Distance / Microsteps;
               end;

            when Lead_Screw =>
               declare
                  Direction_Multiplier : constant Dimensionless :=
                    (if Distance_Config.Lead_Screw.Reverse_Direction then -1.0 else 1.0);
                  Lead                 : constant Length := Distance_Config.Lead_Screw.Lead;
                  Numerator            : constant Dimensionless := Distance_Config.Lead_Screw.Gear_Ratio.Numerator;
                  Denominator          : constant Dimensionless := Distance_Config.Lead_Screw.Gear_Ratio.Denominator;
                  Full_Steps           : constant Dimensionless := Distance_Config.Lead_Screw.Full_Steps_Per_Rotation;
               begin
                  return Direction_Multiplier * Lead / (Full_Steps * Microsteps * Numerator / Denominator);
               end;

            when Gear_With_Circumference =>
               declare
                  Direction_Multiplier : constant Dimensionless :=
                    (if Distance_Config.Gear_With_Circumference.Reverse_Direction then -1.0 else 1.0);
                  Circumference        : constant Length := Distance_Config.Gear_With_Circumference.Circumference;
                  Numerator            : constant Dimensionless :=
                    Distance_Config.Gear_With_Circumference.Gear_Ratio.Numerator;
                  Denominator          : constant Dimensionless :=
                    Distance_Config.Gear_With_Circumference.Gear_Ratio.Denominator;
                  Full_Steps           : constant Dimensionless :=
                    Distance_Config.Gear_With_Circumference.Full_Steps_Per_Rotation;
               begin
                  return Direction_Multiplier * Circumference / (Full_Steps * Microsteps * Numerator / Denominator);
               end;

            when Gear_With_Tooth_Count_And_Pitch =>
               declare
                  Direction_Multiplier : constant Dimensionless :=
                    (if Distance_Config.Gear_With_Tooth_Count_And_Pitch.Reverse_Direction then -1.0 else 1.0);
                  Tooth_Count          : constant Dimensionless :=
                    Distance_Config.Gear_With_Tooth_Count_And_Pitch.Tooth_Count;
                  Tooth_Pitch          : constant Length :=
                    Distance_Config.Gear_With_Tooth_Count_And_Pitch.Tooth_Pitch;
                  Numerator            : constant Dimensionless :=
                    Distance_Config.Gear_With_Tooth_Count_And_Pitch.Gear_Ratio.Numerator;
                  Denominator          : constant Dimensionless :=
                    Distance_Config.Gear_With_Tooth_Count_And_Pitch.Gear_Ratio.Denominator;
                  Full_Steps           : constant Dimensionless :=
                    Distance_Config.Gear_With_Tooth_Count_And_Pitch.Full_Steps_Per_Rotation;
               begin
                  return
                    Direction_Multiplier
                    * Tooth_Count
                    * Tooth_Pitch
                    / (Full_Steps * Microsteps * Numerator / Denominator);
               end;
         end case;
      end Get_Distance_Per_Step;
   begin
      Config.Prunt := (Enabled => Data.Prunt.Enabled, Replace_G0_With_G1 => Data.Prunt.Replace_G0_With_G1);

      case Data.Kinematics.Kinematics_Kind.Kind is
         when Cartesian =>
            Config.Kinematics :=
              (Kind               => Cartesian_Kind,
               Planner_Parameters => <>,
               Z_Steppers         => (others => False),
               E_Steppers         => (others => False),
               X_Steppers         => (others => False),
               Y_Steppers         => (others => False));
            for S in Stepper_Name loop
               case Data.Kinematics.Kinematics_Kind.Cartesian (S) is
                  when Z_Axis =>
                     Config.Kinematics.Z_Steppers (S) := True;

                  when E_Axis =>
                     Config.Kinematics.E_Steppers (S) := True;

                  when X_Axis =>
                     Config.Kinematics.X_Steppers (S) := True;

                  when Y_Axis =>
                     Config.Kinematics.Y_Steppers (S) := True;

                  when None =>
                     null;
               end case;
            end loop;

         when Core_XY =>
            Config.Kinematics :=
              (Kind               => Core_XY_Kind,
               Planner_Parameters => <>,
               Z_Steppers         => (others => False),
               E_Steppers         => (others => False),
               A_Steppers         => (others => False),
               B_Steppers         => (others => False));
            for S in Stepper_Name loop
               case Data.Kinematics.Kinematics_Kind.Core_XY (S) is
                  when Z_Axis =>
                     Config.Kinematics.Z_Steppers (S) := True;

                  when E_Axis =>
                     Config.Kinematics.E_Steppers (S) := True;

                  when A_Axis =>
                     Config.Kinematics.A_Steppers (S) := True;

                  when B_Axis =>
                     Config.Kinematics.B_Steppers (S) := True;

                  when None =>
                     null;
               end case;
            end loop;
      end case;

      Config.Kinematics.Planner_Parameters :=
        (Lower_Pos_Limit         => Data.Kinematics.Lower_Position_Limit,
         Upper_Pos_Limit         => Data.Kinematics.Upper_Position_Limit,
         Ignore_E_In_XYZE        => Data.Kinematics.Ignore_E_In_XYZE,
         Shift_Blended_Corners   => Data.Kinematics.Shift_Blended_Corners,
         Tangential_Velocity_Max => Data.Kinematics.Maximum_Tangential_Velocity,
         Axial_Velocity_Maxes    => (for A in Axis_Name => Data.Kinematics.Axial_Velocity_Limits (A)),
         Acceleration_Max        => Data.Kinematics.Maximum_Acceleration,
         Jerk_Max                => Data.Kinematics.Maximum_Jerk,
         Snap_Max                => Data.Kinematics.Maximum_Snap,
         Crackle_Max             => Data.Kinematics.Maximum_Crackle,
         Chord_Error_Max         => Data.Kinematics.Maximum_Chord_Error,
         Axial_Scaler            => Data.Kinematics.Axial_Scaler);

      Config.G_Code_Assignments :=
        (Has_Heaters => Has_Heaters, Has_Fans => Has_Fans, Has_Lasers => Has_Lasers, others => <>);

      if Has_Heaters then
         Config.G_Code_Assignments.Bed_Heater := Data.G_Code_Assignments.Bed_Heater;
         Config.G_Code_Assignments.Hotend_Heater := Data.G_Code_Assignments.Hotend_Heater;
      end if;

      if Has_Fans then
         Config.G_Code_Assignments.Default_Fan := Data.G_Code_Assignments.Default_Fan;
      end if;

      if Has_Lasers then
         Config.G_Code_Assignments.Default_Laser := Data.G_Code_Assignments.Default_Laser;
      end if;

      for S in Stepper_Name loop
         case Stepper_Hardware (S).Kind is
            when Basic_Kind =>
               Config.Steppers (S) :=
                 (Kind        => Basic_Kind,
                  Enabled     => Data.Steppers (S).Basic_Kind.Enabled,
                  Mm_Per_Step => Get_Distance_Per_Step (S));

            when TMC2240_UART_Kind =>
               declare
                  TMC_Data : constant User_Config_Stepper_TMC2240 := Data.Steppers (S).TMC_2240_Kind;
               begin
                  Config.Steppers (S) :=
                    (Kind               => TMC2240_UART_Kind,
                     Enabled            => TMC_Data.Enabled,
                     Mm_Per_Step        => Get_Distance_Per_Step (S),
                     GCONF              =>
                       (Reserved_1       => 0,
                        Fast_Standstill  => TMC_Boolean (TMC_Data.FAST_STANDSTILL),
                        En_PWM_Mode      => TMC_Boolean (TMC_Data.StealthChop2.Kind = Enabled),
                        Multistep_Filt   =>
                          (if TMC_Data.StealthChop2.Kind = Enabled
                           then TMC_Boolean (TMC_Data.StealthChop2.Enabled.MULTISTEP_FILT)
                           else False),
                        Invert_Direction => False,
                        Diag0_Error      => False,
                        Diag0_OTPW       => False,
                        Diag0_Stall      => False,
                        Diag1_Stall      => False,
                        Diag1_Index      => False,
                        Diag1_On_State   => False,
                        Reserved_2       => 0,
                        Diag_0_Push_Pull => False,
                        Diag_1_Push_Pull => False,
                        Small_Hysteresis => True,
                        Stop_Enable      => False,
                        Direct_Mode      => False,
                        Reserved_3       => 0),
                     DRV_CONF           =>
                       (Current_Range =>
                          (if TMC_Data.Run_Current > 2.0 * amp
                           then TMC_Types.TMC2240.Max_3A
                           elsif TMC_Data.Run_Current > 1.0 * amp
                           then TMC_Types.TMC2240.Max_2A
                           else TMC_Types.TMC2240.Max_1A),
                        Reserved_1    => 0,
                        Slope_Control => TMC_Data.SLOPE_CONTROL,
                        Reserved_2    => 0),
                     GLOBAL_SCALER      =>
                       (Global_Scaler =>
                          (if TMC_Data.Run_Current = 3.0 * amp
                           then 0
                           elsif TMC_Data.Run_Current > 2.0 * amp
                           then
                             TMC_Types.TMC2240.Global_Scaler_Type
                               (Dimensionless'Floor (Dimensionless (TMC_Data.Run_Current / (3.0 * amp)) * 256.0))
                           elsif TMC_Data.Run_Current = 2.0 * amp
                           then 0
                           elsif TMC_Data.Run_Current > 1.0 * amp
                           then
                             TMC_Types.TMC2240.Global_Scaler_Type
                               (Dimensionless'Floor (Dimensionless (TMC_Data.Run_Current / (2.0 * amp)) * 256.0))
                           elsif TMC_Data.Run_Current = 1.0 * amp
                           then 0
                           else
                             TMC_Types.TMC2240.Global_Scaler_Type
                               (Dimensionless'Max
                                  (32.0,
                                   Dimensionless'Floor (Dimensionless (TMC_Data.Run_Current / (1.0 * amp)) * 256.0)))),
                        Reserved      => 0),
                     IHOLD_IRUN         =>
                       (I_Hold       =>
                          (if TMC_Data.Enabled
                           then
                             TMC_Types.Unsigned_5
                               (Dimensionless'Max (0.0, Dimensionless'Floor (TMC_Data.IHOLD * 32.0 - 1.0)))
                           else 0),
                        Reserved_1   => 0,
                        I_Run        =>
                          (if TMC_Data.Enabled
                           then
                             TMC_Types.Unsigned_5
                               (Dimensionless'Max (0.0, Dimensionless'Floor (TMC_Data.IRUN * 32.0 - 1.0)))
                           else 0),
                        Reserved_2   => 0,
                        I_Hold_Delay =>
                          TMC_Types.Unsigned_4
                            (Dimensionless'Floor (Dimensionless (TMC_Data.IHOLDDELAY / (21.0 * ms)))),
                        Reserved_3   => 0,
                        I_Run_Delay  =>
                          TMC_Types.Unsigned_4
                            (Dimensionless'Floor (Dimensionless (TMC_Data.IRUNDELAY / (0.041 * ms)))),
                        Reserved_4   => 0),
                     IRUN_During_Homing =>
                       (if TMC_Data.Enabled
                        then
                          TMC_Types.Unsigned_5
                            (Dimensionless'Max (0.0, Dimensionless'Floor (TMC_Data.IRUN_During_Homing * 32.0 - 1.0)))
                        else 0),
                     TPOWERDOWN         =>
                       (T_Power_Down =>
                          TMC_Types.Unsigned_8
                            (Dimensionless'Floor (Dimensionless (TMC_Data.TPOWERDOWN / (21.0 * ms)))),
                        Reserved     => 0),
                     TPWMTHRS           =>
                       (T_PWM_Thrs =>
                          (if TMC_Data.StealthChop2.Kind = Enabled
                           then
                             TMC_Types.Unsigned_20
                               (Dimensionless'Floor
                                  (Dimensionless'Min
                                     (12_500_000.0
                                      * hertz
                                      * abs (Get_Distance_Per_Step (S))
                                      / (TMC_Data.StealthChop2.Enabled.TPWMTHRS + 1.0E-100 * mm / Prunt.s),
                                      2.0**20 - 1.0)))
                           else TMC_Types.Unsigned_20'Last),
                        Reserved   => 0),
                     TCOOLTHRS          => (T_Cool_Thrs => TMC_Types.Unsigned_20'Last, Reserved => 0),
                     THIGH              =>
                       (T_High   =>
                          TMC_Types.Unsigned_20
                            (Dimensionless'Floor
                               (Dimensionless'Min
                                  (12_500_000.0
                                   * hertz
                                   * abs (Get_Distance_Per_Step (S))
                                   / (TMC_Data.THIGH + 1.0E-100 * mm / Prunt.s),
                                   2.0**20 - 1.0))),
                        Reserved => 0),
                     CHOPCONF           =>
                       (TOFF                 => (if TMC_Data.Enabled then TMC_Data.TOFF else Disable_Driver),
                        HSTRT_TFD210         => 5,
                        --  Set later if required.
                        HEND_OFFSET          => 3,
                        --  Set later if required.
                        FD3                  => 0,
                        --  Set later if required.
                        DISFDCC              =>
                          (if TMC_Data.CHM.Kind = Constant_Off_Time
                           then TMC_Boolean (TMC_Data.CHM.Constant_Off_Time.DISFDCC)
                           else False),
                        Reserved_1           => 0,
                        CHM                  =>
                          (if TMC_Data.Enabled and then TMC_Data.CHM.Kind = SpreadCycle
                           then SpreadCycle_Mode
                           elsif TMC_Data.CHM.Kind = Constant_Off_Time
                           then Constant_Off_Time_Mode
                           else SpreadCycle_Mode),
                        TBL                  => TMC_Data.TBL,
                        Reserved_2           => 0,
                        VHIGHFS              => TMC_Boolean (TMC_Data.VHIGHFS),
                        VHIGHCHM             => TMC_Boolean (TMC_Data.VHIGHCHM),
                        TPFD                 => TMC_Data.TPFD,
                        Microstep_Resolution => TMC_Data.MRES,
                        Interpolate          => False,
                        Double_Edge          => TMC_Boolean (Stepper_Hardware (S).Double_Edge_Stepping),
                        --  Set correctly in Prunt.Controller.
                        Disable_S2G          => False,
                        Disable_S2Vs         => False),
                     PWMCONF            =>
                       (PWM_OFS            =>
                          (if TMC_Data.StealthChop2.Kind = Enabled then TMC_Data.StealthChop2.Enabled.PWM_OFS else 29),
                        PWM_Grad           =>
                          (if TMC_Data.StealthChop2.Kind = Enabled then TMC_Data.StealthChop2.Enabled.PWM_GRAD else 0),
                        PWM_Freq           =>
                          (if TMC_Data.StealthChop2.Kind = Enabled
                           then TMC_Data.StealthChop2.Enabled.PWM_FREQ
                           else Freq_1024),
                        PWM_Auto_Scale     =>
                          (if TMC_Data.StealthChop2.Kind = Enabled
                           then TMC_Boolean (TMC_Data.StealthChop2.Enabled.PWM_AUTOSCALE)
                           else True),
                        PWM_Auto_Grad      =>
                          (if TMC_Data.StealthChop2.Kind = Enabled
                           then TMC_Boolean (TMC_Data.StealthChop2.Enabled.PWM_AUTOGRAD)
                           else True),
                        Freewheel          =>
                          (if TMC_Data.StealthChop2.Kind = Enabled
                           then TMC_Data.StealthChop2.Enabled.FREEWHEEL
                           else Normal),
                        PWM_Meas_SD_Enable =>
                          (if TMC_Data.StealthChop2.Kind = Enabled
                           then TMC_Boolean (TMC_Data.StealthChop2.Enabled.PWM_MEAS_SD_ENABLE)
                           else False),
                        PWM_Dis_Reg_Stst   =>
                          (if TMC_Data.StealthChop2.Kind = Enabled
                           then TMC_Boolean (TMC_Data.StealthChop2.Enabled.PWM_DIS_REG_STST)
                           else False),
                        PWM_Reg            =>
                          (if TMC_Data.StealthChop2.Kind = Enabled then TMC_Data.StealthChop2.Enabled.PWM_REG else 4),
                        PWM_Lim            =>
                          (if TMC_Data.StealthChop2.Kind = Enabled
                           then TMC_Data.StealthChop2.Enabled.PWM_LIM
                           else 12)));

                  if not TMC_Data.Enabled then
                     --  Use default parameters.
                     null;
                  elsif TMC_Data.CHM.Kind = Constant_Off_Time then
                     Config.Steppers (S).CHOPCONF.DISFDCC := TMC_Boolean (TMC_Data.CHM.Constant_Off_Time.DISFDCC);
                     Config.Steppers (S).CHOPCONF.HEND_OFFSET :=
                       TMC_Types.Unsigned_4 (TMC_Data.CHM.Constant_Off_Time.OFFSET + 3);
                     Config.Steppers (S).CHOPCONF.HSTRT_TFD210 :=
                       TMC_Types.Unsigned_3 (TMC_Data.CHM.Constant_Off_Time.TFD rem 8);
                     Config.Steppers (S).CHOPCONF.FD3 := TMC_Types.Unsigned_1 (TMC_Data.CHM.Constant_Off_Time.TFD / 8);
                  elsif TMC_Data.CHM.Kind = SpreadCycle and then TMC_Data.CHM.SpreadCycle.Kind = Manual then
                     Config.Steppers (S).CHOPCONF.HEND_OFFSET :=
                       TMC_Types.Unsigned_4 (TMC_Data.CHM.SpreadCycle.Manual.HEND + 3);
                     Config.Steppers (S).CHOPCONF.HSTRT_TFD210 :=
                       TMC_Types.Unsigned_3 (TMC_Data.CHM.SpreadCycle.Manual.HSTRT - 1);
                  elsif TMC_Data.CHM.Kind = SpreadCycle and then TMC_Data.CHM.SpreadCycle.Kind = Derived then
                     declare
                        Sum_Too_High                : Boolean;
                        Sum_Too_High_For_Full_Scale : Boolean;
                        Excessive_Heating           : Boolean;
                        Driver_Voltage_Too_Low      : Boolean;
                     begin
                        TMC_Types.TMC2240.Optimize_Spreadcycle
                          (Driver_Voltage              => TMC_Data.CHM.SpreadCycle.Derived.Input_Voltage,
                           TBL                         => Config.Steppers (S).CHOPCONF.TBL,
                           Motor_Inductance            => TMC_Data.CHM.SpreadCycle.Derived.Phase_Inductance,
                           Motor_Resistance            => TMC_Data.CHM.SpreadCycle.Derived.Phase_Resistance,
                           Motor_Peak_Current          => TMC_Data.Run_Current,
                           TOFF                        => Config.Steppers (S).CHOPCONF.TOFF,
                           IRUN                        => Config.Steppers (S).IHOLD_IRUN.I_Run,
                           HSTRT                       => Config.Steppers (S).CHOPCONF.HSTRT_TFD210,
                           HEND                        => Config.Steppers (S).CHOPCONF.HEND_OFFSET,
                           Sum_Too_High                => Sum_Too_High,
                           Sum_Too_High_For_Full_Scale => Sum_Too_High_For_Full_Scale,
                           Excessive_Heating           => Excessive_Heating,
                           Driver_Voltage_Too_Low      => Driver_Voltage_Too_Low);
                        if Sum_Too_High
                          or (Sum_Too_High_For_Full_Scale and Config.Steppers (S).IHOLD_IRUN.I_Run = 31)
                          or Excessive_Heating
                          or Driver_Voltage_Too_Low
                        then
                           raise Constraint_Error with "Invalid config should have been caught earlier.";
                        end if;
                     end;
                  end if;

                  --  The TMC2240 datasheet says that the maximum here is 15 rather than 14, but that looks to be an
                  --  off-by-one error as the default sine wave peak is 248. 248 + 16/2 = 256 but the maximum is
                  --  probably actually 255.
                  if Config.Steppers (S).CHOPCONF.CHM = TMC_Types.TMC2240.SpreadCycle_Mode
                    and (Dimensionless (Config.Steppers (S).CHOPCONF.HEND_OFFSET)
                         - 3.0
                         + Dimensionless (Config.Steppers (S).CHOPCONF.HSTRT_TFD210)
                         + 1.0
                         > 14.0)
                    and (Config.Steppers (S).IHOLD_IRUN.I_Run = 31)
                  then
                     raise Constraint_Error with "Invalid config should have been caught earlier.";
                  end if;
               end;
         end case;
      end loop;

      for I in Input_Switch_Name loop
         --  Do not use a iterator filter here due to GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=121316
         if Input_Switch_Visible_To_User (I) then
            Config.Switches (I) :=
              (Enabled     => Data.Input_Switches (I).Visible.Enabled,
               Hit_On_High => Data.Input_Switches (I).Visible.Hit_On_High);
         else
            Config.Switches (I) := (Enabled => False, Hit_On_High => False);
         end if;
      end loop;

      for A in Axis_Name loop
         case Data.Homing (A).Homing_Method.Kind is
            when Disabled =>
               Config.Homing (A) := (Kind => Disabled_Kind, Prerequisites => (others => <>));

            when Use_Input_Switch =>
               declare
                  Homing_Data : constant User_Config_Homing_Use_Input_Switch :=
                    Data.Homing (A).Homing_Method.Use_Input_Switch;
               begin
                  Config.Homing (A) :=
                    (Kind                   => Double_Tap_Kind,
                     Switch                 => Homing_Data.Switch,
                     First_Move_Distance    => Homing_Data.First_Move_Distance,
                     Back_Off_Move_Distance => -Homing_Data.Back_Off_Move_Distance,
                     Second_Move_Distance   => Homing_Data.Second_Move_Distance,
                     Switch_Position        => Homing_Data.Switch_Position,
                     Move_To_After          => Homing_Data.Move_To_After,
                     Velocity_Limit         => Homing_Data.Velocity_Limit,
                     Prerequisites          => (others => <>));

                  if Homing_Data.Move_Towards_Negative_Infinity then
                     Config.Homing (A).First_Move_Distance := -Config.Homing (A).First_Move_Distance;
                     Config.Homing (A).Back_Off_Move_Distance := -Config.Homing (A).Back_Off_Move_Distance;
                     Config.Homing (A).Second_Move_Distance := -Config.Homing (A).Second_Move_Distance;
                  end if;
               end;

            when Set_To_Value =>
               Config.Homing (A) :=
                 (Kind          => Set_To_Value_Kind,
                  Value         => Data.Homing (A).Homing_Method.Set_To_Value.Position,
                  Prerequisites => (others => <>));

            when Use_StallGuard2 =>
               declare
                  Homing_Data : constant User_Config_Homing_Use_StallGuard2 :=
                    Data.Homing (A).Homing_Method.Use_StallGuard2;
               begin
                  Config.Homing (A) :=
                    (Kind               => StallGuard2_Kind,
                     Move_To_Negative   => Homing_Data.Move_Towards_Negative_Infinity,
                     Enable_Filter      => Homing_Data.Enable_Filter,
                     Motor              => Homing_Data.Motor,
                     SG2_Threshold      => TMC_Types.Unsigned_7 (Homing_Data.Threshold mod 2**7),
                     Switch_Position    => Homing_Data.Stop_Position,
                     Move_To_After      => Homing_Data.Move_To_After,
                     Velocity_Limit     => Homing_Data.Velocity_Limit,
                     Acceleration_Limit => Homing_Data.Acceleration_Limit,
                     Prerequisites      => (others => <>));
               end;

            when Use_StallGuard4 =>
               declare
                  Homing_Data : constant User_Config_Homing_Use_StallGuard4 :=
                    Data.Homing (A).Homing_Method.Use_StallGuard4;
               begin
                  Config.Homing (A) :=
                    (Kind               => StallGuard4_Kind,
                     Move_To_Negative   => Homing_Data.Move_Towards_Negative_Infinity,
                     Enable_Filter      => Homing_Data.Enable_Filter,
                     Motor              => Homing_Data.Motor,
                     SG4_Threshold      => TMC_Types.Unsigned_8 (Homing_Data.Threshold),
                     Switch_Position    => Homing_Data.Stop_Position,
                     Move_To_After      => Homing_Data.Move_To_After,
                     Velocity_Limit     => Homing_Data.Velocity_Limit,
                     Acceleration_Limit => Homing_Data.Acceleration_Limit,
                     Prerequisites      => (others => <>));
               end;
         end case;

         for B in Axis_Name loop
            case Data.Homing (A).Prerequisites (B).Kind is
               when No_Requirement =>
                  Config.Homing (A).Prerequisites (B) := (Kind => No_Requirement_Kind);

               when Must_Be_Homed =>
                  Config.Homing (A).Prerequisites (B) := (Kind => Must_Be_Homed_Kind);

               when Must_Be_At_Position =>
                  Config.Homing (A).Prerequisites (B) :=
                    (Kind     => Must_Be_At_Position_Kind,
                     Position => Data.Homing (A).Prerequisites (B).Must_Be_At_Position.Position);
            end case;
         end loop;
      end loop;

      for T in Thermistor_Name loop
         case Data.Thermistors (T).Thermistor_Kind.Kind is
            when Disabled =>
               Config.Thermistors (T) := (Kind => Disabled_Kind, Minimum_Temperature => <>, Maximum_Temperature => <>);

            when Custom_Steinhart_Hart_Model =>
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  SH_A                => Data.Thermistors (T).Thermistor_Kind.Custom_Steinhart_Hart_Model.A,
                  SH_B                => Data.Thermistors (T).Thermistor_Kind.Custom_Steinhart_Hart_Model.B,
                  SH_C                => Data.Thermistors (T).Thermistor_Kind.Custom_Steinhart_Hart_Model.C,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when Custom_Callendar_Van_Dusen_Model =>
               Config.Thermistors (T) :=
                 (Kind                => Callendar_Van_Dusen_Kind,
                  CVD_R0              => Data.Thermistors (T).Thermistor_Kind.Custom_Callendar_Van_Dusen_Model.R0,
                  CVD_A               => Data.Thermistors (T).Thermistor_Kind.Custom_Callendar_Van_Dusen_Model.A,
                  CVD_B               => Data.Thermistors (T).Thermistor_Kind.Custom_Callendar_Van_Dusen_Model.B,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when ATC_Semitec_104GT_2 =>
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  SH_A                => 8.0965E-4,
                  SH_B                => 2.1163E-4,
                  SH_C                => 7.0742E-8,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when ATC_Semitec_104NT_4_R025H42G =>
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  SH_A                => 7.9582E-4,
                  SH_B                => 2.1360E-4,
                  SH_C                => 6.4830E-8,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when EPCOS_100K_B57560G104F =>
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  SH_A                => 7.2213E-4,
                  SH_B                => 2.1676E-4,
                  SH_C                => 8.9293E-8,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when Generic_3950 =>
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  SH_A                => 7.9347E-4,
                  SH_B                => 2.0076E-4,
                  SH_C                => 1.6328E-7,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when SliceEngineering_450 =>
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  SH_A                => 3.0553E-4,
                  SH_B                => 2.1171E-4,
                  SH_C                => 1.1962E-7,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when TDK_NTCG104LH104JT1 =>
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  SH_A                => 9.7639E-4,
                  SH_B                => 1.9688E-4,
                  SH_C                => 7.2671E-8,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when Honeywell_100K_135_104LAG_J01 =>
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  SH_A                => 4.5695E-4,
                  SH_B                => 2.5163E-4,
                  SH_C                => 0.0,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when NTC_100K_MGB18_104F39050L32 =>
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  SH_A                => 5.4598E-4,
                  SH_B                => 2.4390E-4,
                  SH_C                => 0.0,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when PT_1000_PT_385 =>
               Config.Thermistors (T) :=
                 (Kind                => Callendar_Van_Dusen_Kind,
                  CVD_R0              => 1_000.0 * ohm,
                  CVD_A               => 3.9083E-3,
                  CVD_B               => -5.775E-7,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);

            when PT_1000_PT_392 =>
               Config.Thermistors (T) :=
                 (Kind                => Callendar_Van_Dusen_Kind,
                  CVD_R0              => 1_000.0 * ohm,
                  CVD_A               => 3.9827E-3,
                  CVD_B               => -5.875E-7,
                  Minimum_Temperature => <>,
                  Maximum_Temperature => <>);
         end case;

         Config.Thermistors (T).Minimum_Temperature := Data.Thermistors (T).Minimum_Temperature;
         Config.Thermistors (T).Maximum_Temperature := Data.Thermistors (T).Maximum_Temperature;
      end loop;

      for H in Heater_Name loop
         case Data.Heaters (H).Control_Method.Kind is
            when Disabled =>
               Config.Heaters (H).Params :=
                 (Kind                       => Disabled_Kind,
                  Check_Max_Cumulative_Error => <>,
                  Check_Gain_Time            => <>,
                  Check_Minimum_Gain         => <>,
                  Check_Hysteresis           => <>);

            when PID =>
               Config.Heaters (H).Params :=
                 (Kind                       => PID_Kind,
                  Proportional_Scale         => Data.Heaters (H).Control_Method.PID.Proportional_Scale,
                  Integral_Scale             => Data.Heaters (H).Control_Method.PID.Integral_Scale,
                  Derivative_Scale           => Data.Heaters (H).Control_Method.PID.Derivative_Scale,
                  Check_Max_Cumulative_Error => <>,
                  Check_Gain_Time            => <>,
                  Check_Minimum_Gain         => <>,
                  Check_Hysteresis           => <>);

            when Bang_Bang =>
               Config.Heaters (H).Params :=
                 (Kind                       => Bang_Bang_Kind,
                  Bang_Bang_Hysteresis       => Data.Heaters (H).Control_Method.Bang_Bang.Hysteresis,
                  Check_Max_Cumulative_Error => <>,
                  Check_Gain_Time            => <>,
                  Check_Minimum_Gain         => <>,
                  Check_Hysteresis           => <>);
         end case;

         Config.Heaters (H).Thermistor := Data.Heaters (H).Thermistor;
         Config.Heaters (H).Params.Check_Max_Cumulative_Error := Data.Heaters (H).Check_Maximum_Cumulative_Error;
         Config.Heaters (H).Params.Check_Gain_Time := Data.Heaters (H).Check_Gain_Time;
         Config.Heaters (H).Params.Check_Minimum_Gain := Data.Heaters (H).Check_Minimum_Gain;
         Config.Heaters (H).Params.Check_Hysteresis := Data.Heaters (H).Check_Hysteresis;
      end loop;

      for F in Fan_Name loop
         case Fan_Hardware (F).Kind is
            when Fixed_Switching_Kind =>
               case Data.Fans (F).Fixed_Switching.Control_Method.Kind is
                  when Always_On =>
                     Config.Fans (F) :=
                       (Kind                    => Always_On_Kind,
                        Invert_Output           => Data.Fans (F).Fixed_Switching.Invert_PWM_Output,
                        PWM_Frequency           => Data.Fans (F).Fixed_Switching.PWM_Frequency,
                        Always_On_PWM           => Data.Fans (F).Fixed_Switching.Control_Method.Always_On.Duty_Cycle,
                        Use_High_Side_Switching => False);

                  when Dynamic_Duty_Cycle =>
                     Config.Fans (F) :=
                       (Kind                    => Dynamic_PWM_Kind,
                        Invert_Output           => Data.Fans (F).Fixed_Switching.Invert_PWM_Output,
                        PWM_Frequency           => Data.Fans (F).Fixed_Switching.PWM_Frequency,
                        Disable_Below_PWM       =>
                          Data.Fans (F).Fixed_Switching.Control_Method.Dynamic_Duty_Cycle.Disable_Below,
                        Max_PWM                 =>
                          Data.Fans (F).Fixed_Switching.Control_Method.Dynamic_Duty_Cycle.Maximum_Duty_Cycle,
                        Use_High_Side_Switching => False);
               end case;

            when Low_Or_High_Side_Switching_Kind =>
               case Data.Fans (F).Low_Or_High_Side_Switching.Control_Method.Kind is
                  when Always_On =>
                     Config.Fans (F) :=
                       (Kind                    => Always_On_Kind,
                        Invert_Output           => Data.Fans (F).Low_Or_High_Side_Switching.Invert_PWM_Output,
                        PWM_Frequency           => Data.Fans (F).Low_Or_High_Side_Switching.PWM_Frequency,
                        Use_High_Side_Switching => Data.Fans (F).Low_Or_High_Side_Switching.Use_High_Side_Switching,
                        Always_On_PWM           =>
                          Data.Fans (F).Low_Or_High_Side_Switching.Control_Method.Always_On.Duty_Cycle);

                  when Dynamic_Duty_Cycle =>
                     Config.Fans (F) :=
                       (Kind                    => Dynamic_PWM_Kind,
                        Invert_Output           => Data.Fans (F).Low_Or_High_Side_Switching.Invert_PWM_Output,
                        PWM_Frequency           => Data.Fans (F).Low_Or_High_Side_Switching.PWM_Frequency,
                        Use_High_Side_Switching => Data.Fans (F).Low_Or_High_Side_Switching.Use_High_Side_Switching,
                        Disable_Below_PWM       =>
                          Data.Fans (F).Low_Or_High_Side_Switching.Control_Method.Dynamic_Duty_Cycle.Disable_Below,
                        Max_PWM                 =>
                          Data.Fans (F)
                            .Low_Or_High_Side_Switching
                            .Control_Method
                            .Dynamic_Duty_Cycle
                            .Maximum_Duty_Cycle);
               end case;
         end case;
      end loop;

      for A in Axis_Name loop
         case Data.Input_Shaping (A).Kind is
            when No_Shaper =>
               Config.Shapers (A) := (Kind => No_Shaper);

            when ZV =>
               Config.Shapers (A) :=
                 (Kind                         => Zero_Vibration,
                  Zero_Vibration_Frequency     => Data.Input_Shaping (A).ZV.Freq,
                  Zero_Vibration_Damping_Ratio => Shaper_Damping_Ratio (Data.Input_Shaping (A).ZV.Damping_Ratio),
                  Zero_Vibration_Deriviatives  =>
                    Zero_Vibration_Deriviatives_Count (Data.Input_Shaping (A).ZV.Number_Of_Derivatives));

            when EI =>
               Config.Shapers (A) :=
                 (Kind                                 => Extra_Insensitive,
                  Extra_Insensitive_Frequency          => Data.Input_Shaping (A).EI.Freq,
                  Extra_Insensitive_Damping_Ratio      =>
                    Shaper_Damping_Ratio (Data.Input_Shaping (A).EI.Damping_Ratio),
                  Extra_Insensitive_Residual_Vibration =>
                    Residual_Vibration_Level (Data.Input_Shaping (A).EI.Residual_Vibration_Level),
                  Extra_Insensitive_Humps              =>
                    Extra_Insensitive_Humps_Count (Data.Input_Shaping (A).EI.Number_Of_Humps));

            when Pressure_Advance =>
               Config.Shapers (A) :=
                 (Kind                                    => Pressure_Advance,
                  Pressure_Advance_Time                   =>
                    Data.Input_Shaping (A).Pressure_Advance.Pressure_Advance_Time,
                  Pressure_Advance_Smooth_Time            =>
                    Data.Input_Shaping (A).Pressure_Advance.Pressure_Advance_Smooth_Time,
                  Pressure_Advance_Smooth_Added_Part_Only =>
                    Data.Input_Shaping (A).Pressure_Advance.Smooth_Added_Part_Only);
         end case;
      end loop;

      for L in Laser_Name loop
         Config.Lasers (L) := (Modulate_With_Velocity => Data.Lasers (L).Modulate_With_Velocity);
      end loop;

      return Config;
   end User_Config_To_Config;

end Prunt.Config;
