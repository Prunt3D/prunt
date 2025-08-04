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

with Prunt.Thermistors;   use Prunt.Thermistors;
with GNATCOLL.JSON;       use GNATCOLL.JSON;
with Prunt.TMC_Types;     use Prunt.TMC_Types;
with Prunt.Input_Shapers; use Prunt.Input_Shapers;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;
with Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;
with Ada.Containers.Indefinite_Ordered_Sets;
with Prunt.Motion_Planner;
with Prunt.TMC_Types.TMC2240;
with Prunt.Controller_Generic_Types;

generic
   with package Generic_Types is new Controller_Generic_Types (<>);
   use Generic_Types;

   Stepper_Hardware : Generic_Types.Stepper_Hardware_Parameters_Array_Type;
   Fan_Hardware : Generic_Types.Fan_Hardware_Parameters_Array_Type;

   with function Get_Board_Specific_Documentation (Key : String) return String;

   Config_Path : String;

   Enable_Documentation_Dev_Mode : Boolean;

   Input_Switch_Visible_To_User : Generic_Types.Input_Switch_Visible_To_User_Type;
package Prunt.Config is

   type Attached_Steppers is array (Stepper_Name) of Boolean;

   IO_Error                 : exception;
   Config_File_Format_Error : exception;

   type Prunt_Parameters is record
      Enabled            : Boolean := False;
      Replace_G0_With_G1 : Boolean := False;
   end record;

   type Stepper_Parameters (Kind : Stepper_Hardware_Kind := Basic_Kind) is record
      Enabled     : Boolean := False;
      Mm_Per_Step : Length := Length'Last / 2.0;
      case Kind is
         when Basic_Kind =>
            null;

         when TMC2240_UART_Kind =>
            GCONF              : TMC_Types.TMC2240.GCONF;
            DRV_CONF           : TMC_Types.TMC2240.DRV_CONF;
            GLOBAL_SCALER      : TMC_Types.TMC2240.GLOBAL_SCALER;
            IHOLD_IRUN         : TMC_Types.TMC2240.IHOLD_IRUN;
            IRUN_During_Homing : TMC_Types.Unsigned_5;
            TPOWERDOWN         : TMC_Types.TMC2240.TPOWERDOWN;
            TPWMTHRS           : TMC_Types.TMC2240.TPWMTHRS;
            TCOOLTHRS          : TMC_Types.TMC2240.TCOOLTHRS;
            THIGH              : TMC_Types.TMC2240.THIGH;
            PWMCONF            : TMC_Types.TMC2240.PWMCONF;
            CHOPCONF           : TMC_Types.TMC2240.CHOPCONF;
      end case;
   end record;

   type Kinematics_Kind is (Cartesian_Kind, Core_XY_Kind);

   type Kinematics_Parameters (Kind : Kinematics_Kind := Cartesian_Kind) is record
      Planner_Parameters : Motion_Planner.Kinematic_Parameters := (others => <>);
      Z_Steppers         : Attached_Steppers := [others => False];
      E_Steppers         : Attached_Steppers := [others => False];
      case Kind is
         when Cartesian_Kind =>
            X_Steppers : Attached_Steppers := [others => False];
            Y_Steppers : Attached_Steppers := [others => False];

         when Core_XY_Kind =>
            A_Steppers : Attached_Steppers := [others => False];
            B_Steppers : Attached_Steppers := [others => False];
      end case;
   end record;

   type Input_Switch_Parameters is record
      Enabled     : Boolean := False;
      Hit_On_High : Boolean := False;
   end record;

   type Homing_Prerequisite_Kind is (No_Requirement_Kind, Must_Be_Homed_Kind, Must_Be_At_Position_Kind);

   type Homing_Prerequisite_Parameters (Kind : Homing_Prerequisite_Kind := No_Requirement_Kind) is record
      case Kind is
         when No_Requirement_Kind =>
            null;

         when Must_Be_Homed_Kind =>
            null;

         when Must_Be_At_Position_Kind =>
            Position : Length;
      end case;
   end record;

   type Axial_Homing_Prerequisites is array (Axis_Name) of Homing_Prerequisite_Parameters;

   type Homing_Kind is (Disabled_Kind, Double_Tap_Kind, Set_To_Value_Kind, StallGuard2_Kind, StallGuard4_Kind);

   type Homing_Parameters (Kind : Homing_Kind := Disabled_Kind) is record
      Prerequisites : Axial_Homing_Prerequisites := (others => (Kind => No_Requirement_Kind));

      case Kind is
         when Disabled_Kind =>
            null;

         when Set_To_Value_Kind =>
            Value : Length := 0.0 * mm;

         when Double_Tap_Kind | StallGuard2_Kind | StallGuard4_Kind =>
            Switch_Position : Length := 0.0 * mm;
            Move_To_After   : Length := 5.0 * mm;
            Velocity_Limit  : Velocity := 1.0E100 * mm / s;

            case Kind is
               when Disabled_Kind | Set_To_Value_Kind =>
                  null;

               when Double_Tap_Kind =>
                  Switch                 : Input_Switch_Name := Input_Switch_Name'First;
                  First_Move_Distance    : Length := 0.0 * mm;
                  Back_Off_Move_Distance : Length := 0.0 * mm;
                  Second_Move_Distance   : Length := 0.0 * mm;

               when StallGuard2_Kind | StallGuard4_Kind =>
                  Move_To_Negative : Boolean;
                  Enable_Filter    : Boolean;
                  Motor            : Stepper_Name;

                  case Kind is
                     when Disabled_Kind | Set_To_Value_Kind | Double_Tap_Kind =>
                        null;

                     when StallGuard2_Kind =>
                        SG2_Threshold : TMC_Types.Unsigned_7;

                     when StallGuard4_Kind =>
                        SG4_Threshold : TMC_Types.Unsigned_8;
                  end case;
            end case;
      end case;
   end record;

   --  Thermistor_Parameters in Prunt.Thermistors.

   type Heater_Full_Parameters is record
      Thermistor : Thermistor_Name := Thermistor_Name'First;
      Params     : Heater_Parameters;
   end record;

   type G_Code_Assignment_Parameters is record
      Bed_Heater    : Heater_Name := Heater_Name'First;
      Hotend_Heater : Heater_Name := Heater_Name'First;
      Default_Fan   : Fan_Name := Fan_Name'First;
   end record;

   type Fan_Kind is (Dynamic_PWM_Kind, Always_On_Kind);

   type Fan_Parameters (Kind : Fan_Kind := Always_On_Kind) is record
      Invert_Output : Boolean := False;
      PWM_Frequency : Fan_PWM_Frequency := 30.0 * hertz;

      Use_High_Side_Switching : Boolean := True;
      --  Ignore if not using a Low_Or_High_Side_Switching_Kind.

      case Kind is
         when Dynamic_PWM_Kind =>
            Disable_Below_PWM : PWM_Scale := 0.5;
            Max_PWM           : PWM_Scale := 1.0;

         when Always_On_Kind =>
            Always_On_PWM : PWM_Scale := 1.0;
      end case;
   end record;

   procedure Disable_Prunt;
   --  Modifies the configuration file to cause Prunt_Parameters.Enabled to be set to False. This does not take effect
   --  until the next startup.

   procedure Read (Data : out Prunt_Parameters);
   procedure Read (Data : out Stepper_Parameters; Stepper : Stepper_Name)
   with Post => Data.Kind = Stepper_Hardware (Stepper).Kind;
   procedure Read (Data : out Kinematics_Parameters);
   procedure Read (Data : out Input_Switch_Parameters; Input_Switch : Input_Switch_Name);
   procedure Read (Data : out Homing_Parameters; Axis : Axis_Name);
   procedure Read (Data : out Thermistor_Parameters; Thermistor : Thermistor_Name);
   procedure Read (Data : out Heater_Full_Parameters; Heater : Heater_Name)
   with Post => Data.Params.Kind not in PID_Autotune_Kind;
   procedure Read (Data : out Fan_Parameters; Fan : Fan_Name);
   procedure Read (Data : out G_Code_Assignment_Parameters);
   procedure Read (Data : out Shaper_Parameters; Axis : Axis_Name);
   --  The above procedures read the initial configuration values, not configurations values that have been changed
   --  after the first read.

   procedure Patch
     (Data : in out Ada.Strings.Unbounded.Unbounded_String; Report : access procedure (Key, Message : String));
   --  Sets Data and the configuration file to the union of Data and Get_Values. Also reports any errors in the union
   --  in the same way as Validate_Current_Config but atomically. The format of Data should match the format described
   --  in the Get_Values documentation comment.

   procedure Validate_Initial_Config (Report : access procedure (Key, Message : String));
   --  Calls Report for each error in the initial configuration, i.e. the one used by Read.

   procedure Validate_Current_Config (Report : access procedure (Key, Message : String));
   --  Calls Report for each error in the current configuration, i.e. not the one used by Read.

   function Get_Schema return Ada.Strings.Unbounded.Unbounded_String;
   --  Returns the schema to be used to build a GUI as a JSON value.
   --
   --  The schema consists of:
   --  - Integer and float fields, which contain a unit and an inclusive range.
   --  - Float ratio fields, which should be presented at A:B and contain an inclusive range which is based on A/B.
   --  - Boolean fields.
   --  - Discrete fields, which may be set to one of a list of options and should be presented as a drop-down.
   --  - Sequence fields, which contain a list of other fields that should all be shown at the same time and in order.
   --  - Variant fields, which contain a set of fields where only one should be shown, controlled by the user.
   --  - Tabbed sequence fields, which should be presented as tabs.
   --
   --  The returned schema matches SettingsSchemaEntry in the below TypeScript interface:
   --
   --  interface SettingsSchemaBase {
   --      Description: string;
   --  }
   --
   --  interface IntegerSettingsSchema extends SettingsSchemaBase {
   --      Kind: "Integer";
   --      Min: number;
   --      Max: number;
   --      Unit: string;
   --  }
   --
   --  interface FloatSettingsSchema extends SettingsSchemaBase {
   --      Kind: "Float";
   --      Min: number;
   --      Max: number;
   --      Unit: string;
   --  }
   --
   --
   --  interface FloatRatioSettingsSchema extends SettingsSchemaBase {
   --      Kind: "Float_Ratio";
   --      Min: number;
   --      Max: number;
   --  }

   --  interface TabbedSequenceSettingsSchema extends SettingsSchemaBase {
   --      Kind: "Tabbed_Sequence";
   --      Children: Record<string, SettingsSchemaEntry>;
   --  }
   --
   --  interface SequenceSettingsSchema extends SettingsSchemaBase {
   --      Kind: "Sequence";
   --      Children: Record<string, SettingsSchemaEntry>;
   --  }
   --
   --  interface VariantSettingsSchema extends SettingsSchemaBase {
   --      Kind: "Variant";
   --      Children: Record<string, SettingsSchemaEntry>;
   --  }
   --
   --  interface DiscreteSettingsSchema extends SettingsSchemaBase {
   --      Kind: "Discrete";
   --      Options: string[];
   --  }
   --
   --  interface BooleanSettingsSchema extends SettingsSchemaBase {
   --      Kind: "Boolean";
   --  }
   --
   --  type SettingsSchemaEntry =
   --      | IntegerSettingsSchema
   --      | FloatSettingsSchema
   --      | TabbedSequenceSettingsSchema
   --      | SequenceSettingsSchema
   --      | VariantSettingsSchema
   --      | DiscreteSettingsSchema
   --      | BooleanSettingsSchema;

   function Get_Values return Ada.Strings.Unbounded.Unbounded_String;
   --  Get the values matching the schema returned by Get_Schema as a flat JSON map. The keys of the map are the paths
   --  within the schema (i.e. the keys of the maps in sequences and variants) joined by the $ character. There may be
   --  extra values if the user has manually edited the configuration file, these values should be ignored.
   --
   --  For an integer the value is a number with no decimal part.
   --
   --  For a float the value is a number.
   --
   --  For a float ratio the value is two numbers under Numerator and Denominator subkeys.
   --
   --  For a boolean the value is a boolean.
   --
   --  For a discrete the value is a string matching the user-selected value.
   --
   --  For a sequence or tabbed sequence there is no key in the map.
   --
   --  For a variant the value is the key of the user-selected child. All the child keys are always present,
   --  regardless of the selection.

   function Prunt_Is_Enabled return Boolean;

   function Get_Values_And_Validate
     (Report : access procedure (Key, Message : String)) return Ada.Strings.Unbounded.Unbounded_String;
   --  Identical to Validate_Current_Config followed by Get_Values, but guarantees that the values will not change
   --  between the two operations.

   procedure Reset;

private

   package Discrete_String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   type Property_Kind is
     (Boolean_Kind, Discrete_Kind, Integer_Kind, Float_Kind, Float_Ratio_Kind, Sequence_Kind, Variant_Kind);

   type Property_Parameters (Kind : Property_Kind);
   type Property_Parameters_Access is not null access constant Property_Parameters;

   package Property_Maps is new Indefinite_Ordered_Maps_With_Insertion_Order (String, Property_Parameters_Access);

   type Property_Parameters (Kind : Property_Kind) is record
      Description : Ada.Strings.Unbounded.Unbounded_String;
      case Kind is
         when Boolean_Kind =>
            Boolean_Default : Boolean;

         when Discrete_Kind =>
            Discrete_Options : Discrete_String_Sets.Set;
            Discrete_Default : Ada.Strings.Unbounded.Unbounded_String;

         when Integer_Kind =>
            Integer_Min     : Long_Long_Integer;
            Integer_Max     : Long_Long_Integer;
            Integer_Unit    : Ada.Strings.Unbounded.Unbounded_String;
            Integer_Default : Long_Long_Integer;

         when Float_Kind =>
            Float_Min     : Long_Float;
            Float_Max     : Long_Float;
            Float_Unit    : Ada.Strings.Unbounded.Unbounded_String;
            Float_Default : Long_Float;

         when Float_Ratio_Kind =>
            Float_Ratio_Min                 : Long_Float;
            Float_Ratio_Max                 : Long_Float;
            Float_Ratio_Default_Numerator   : Long_Float;
            Float_Ratio_Default_Denominator : Long_Float;

         when Sequence_Kind =>
            Sequence_Children : Property_Maps.Map;
            Sequence_Tabbed   : Boolean;

         when Variant_Kind =>
            Variant_Children : Property_Maps.Map;
            Variant_Default  : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   package Flat_Schemas is new Ada.Containers.Indefinite_Ordered_Maps (String, Property_Parameters_Access);

   --  This function leaks memory as it is only meant to be called once over the lifetime of the program.
   function Build_Schema return Property_Maps.Map;

   function Schema_To_JSON (Schema : Property_Maps.Map) return Ada.Strings.Unbounded.Unbounded_String;

   --  This function leaks memory as it is only meant to be called once over the lifetime of the program.
   function Build_Flat_Schema (Schema : Property_Maps.Map) return Flat_Schemas.Map
   with
     Post =>
       (for all P of Build_Flat_Schema'Result =>
          P.Kind in Boolean_Kind | Discrete_Kind | Integer_Kind | Float_Kind | Float_Ratio_Kind);

   type Stepper_Parameters_Array is array (Stepper_Name) of Stepper_Parameters;

   type Input_Switch_Parameters_Array is array (Input_Switch_Name) of Input_Switch_Parameters;

   type Homing_Parameters_Array is array (Axis_Name) of Homing_Parameters;

   type Thermistor_Parameters_Array is array (Thermistor_Name) of Thermistor_Parameters;

   type Heater_Full_Parameters_Array is array (Heater_Name) of Heater_Full_Parameters;

   type Fan_Parameters_Array is array (Fan_Name) of Fan_Parameters;

   type Shaper_Parameters_Array is array (Axis_Name) of Shaper_Parameters;

   type Full_Config is record
      Prunt              : Prunt_Parameters;
      Kinematics         : Kinematics_Parameters;
      G_Code_Assignments : G_Code_Assignment_Parameters;
      Steppers           : Stepper_Parameters_Array;
      Switches           : Input_Switch_Parameters_Array;
      Homing             : Homing_Parameters_Array;
      Thermistors        : Thermistor_Parameters_Array;
      Heaters            : Heater_Full_Parameters_Array;
      Fans               : Fan_Parameters_Array;
      Shapers            : Shaper_Parameters_Array;
   end record;

   protected Config_File is
      procedure Disable_Prunt;
      procedure Read (Data : out Prunt_Parameters);
      procedure Read (Data : out Stepper_Parameters; Stepper : Stepper_Name)
      with Post => Data.Kind = Stepper_Hardware (Stepper).Kind;
      procedure Read (Data : out Kinematics_Parameters);
      procedure Read (Data : out Input_Switch_Parameters; Input_Switch : Input_Switch_Name);
      procedure Read (Data : out Homing_Parameters; Axis : Axis_Name);
      procedure Read (Data : out Thermistor_Parameters; Thermistor : Thermistor_Name);
      procedure Read (Data : out Heater_Full_Parameters; Heater : Heater_Name)
      with Post => Data.Params.Kind not in PID_Autotune_Kind;
      procedure Read (Data : out Fan_Parameters; Fan : Fan_Name);
      procedure Read (Data : out G_Code_Assignment_Parameters);
      procedure Read (Data : out Shaper_Parameters; Axis : Axis_Name);
      procedure Patch
        (Data : in out Ada.Strings.Unbounded.Unbounded_String; Report : access procedure (Key, Message : String));
      procedure Validate_Initial_Config (Report : access procedure (Key, Message : String));
      procedure Validate_Current_Config (Report : access procedure (Key, Message : String));
      procedure Get_Schema (Schema : out Ada.Strings.Unbounded.Unbounded_String);
      procedure Get_Values (Values : out Ada.Strings.Unbounded.Unbounded_String);
      procedure Get_Values_And_Validate
        (Report : access procedure (Key, Message : String); Values : out Ada.Strings.Unbounded.Unbounded_String);
      procedure Prunt_Is_Enabled (Result : out Boolean);
      procedure Reset;
   private
      procedure Error_If_Initial_Config_Invalid;
      procedure Validate_Config (Config : JSON_Value; Report : access procedure (Key, Message : String));
      function JSON_To_Config (Data : JSON_Value) return Full_Config;
      procedure Validate_Config_To_Schema (Config : JSON_Value; Report : access procedure (Key, Message : String));
      procedure Maybe_Do_Init;
      procedure Write_File;
      Init_Done            : Boolean := False;
      Init_Failed          : Boolean := False;
      Initial_Config_Valid : Boolean := False;
      Initial_Properties   : JSON_Value;
      Current_Properties   : JSON_Value;
      Schema               : Property_Maps.Map := Property_Maps.Empty;
      Schema_JSON          : Ada.Strings.Unbounded.Unbounded_String;
      Flat_Schema          : Flat_Schemas.Map := Flat_Schemas.Empty;
      Initial_Config       : Full_Config;
   end Config_File;

   function Get (Val : JSON_Value; Field : UTF8_String) return Dimensionless
   with Pre => Val.Kind = JSON_Object_Type and then Get (Val, Field).Kind in JSON_Float_Type | JSON_Int_Type;

   function My_Get_Long_Float (Val : JSON_Value; Field : UTF8_String) return Long_Float
   with Pre => Val.Kind = JSON_Object_Type and then Get (Val, Field).Kind in JSON_Float_Type | JSON_Int_Type;

   generic
      type T is range <>;
   function Get_JSON_Integer (Val : JSON_Value; Field : UTF8_String) return T;

   function To_Unbounded_String (Source : String) return Ada.Strings.Unbounded.Unbounded_String
   renames Ada.Strings.Unbounded.To_Unbounded_String;

   function Get (Val : JSON_Value; Field : UTF8_String) return TMC_Boolean;

end Prunt.Config;
