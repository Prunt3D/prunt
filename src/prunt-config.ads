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

with Prunt.Thermistors;       use Prunt.Thermistors;
with GNATCOLL.JSON;           use GNATCOLL.JSON;
with Prunt.TMC_Types;         use Prunt.TMC_Types;
with Prunt.Input_Shapers;     use Prunt.Input_Shapers;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;
with Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;
with Ada.Containers.Indefinite_Ordered_Sets;
with Prunt.Motion_Planner;
with Prunt.TMC_Types.TMC2240; use Prunt.TMC_Types.TMC2240;
with Prunt.Controller_Generic_Types;
with System.Pool_Local;

generic
   with package Generic_Types is new Controller_Generic_Types (<>);
   use Generic_Types;

   Stepper_Hardware : Generic_Types.Stepper_Hardware_Parameters_Array_Type;
   Fan_Hardware : Generic_Types.Fan_Hardware_Parameters_Array_Type;

   with function Get_Board_Specific_Documentation (Key : String) return String;
   --  For each Description field returned by Get_Schema, the return value of this function will be appended after a
   --  string denoting it as board specific documentation. If there is no relevant board specific documentation them
   --  this function should return an empty string. JSON escaping is applied to the returned string.

   Config_Path : String;
   --  The path to store the configuration file. This is passed to Ada.Text_IO.Create as-is. Multiple backup files
   --  named `Config_Path & "_backup_NN"` will also be created.

   Enable_Documentation_Dev_Mode : Boolean;
   --  If this value is True then each Description field returned by Get_Schema will have its schema key appended to
   --  it. This is the same key used in the return value of Get_Values and passed to Get_Board_Specific_Documentation.

   Input_Switch_Visible_To_User : Generic_Types.Input_Switch_Visible_To_User_Type;
   --  Input switches set to false in this array will have no configuration options presented to the user. This is
   --  intended to be used for StallGuard inputs.
package Prunt.Config is

   type Attached_Steppers is array (Stepper_Name) of Boolean;

   Config_File_Format_Error : exception;
   --  Raised by the first function called which needs access to the configuration file values if the configuration
   --  file is for a newer Prunt version or if the configuration file does not match the schema.

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
                  Move_To_Negative   : Boolean;
                  Enable_Filter      : Boolean;
                  Motor              : Stepper_Name;
                  Acceleration_Limit : Acceleration;

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

   type G_Code_Assignment_Parameters
     (Has_Heaters : Boolean := False;
      Has_Fans    : Boolean := False;
      Has_Lasers  : Boolean := False)
   is record
      Bed_Heater    : Heater_Name;
      Hotend_Heater : Heater_Name;
      Default_Fan   : Fan_Name;
      Default_Laser : Laser_Name;
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

   type Laser_Parameters is record
      Modulate_With_Velocity : Boolean;
   end record;

   procedure Disable_Prunt;
   --  Modifies the configuration file to cause Prunt_Parameters.Enabled to be set to False. This does not take effect
   --  until the next startup.

   function Read return Prunt_Parameters;
   function Read (Stepper : Stepper_Name) return Stepper_Parameters
   with Post => Read'Result.Kind = Stepper_Hardware (Stepper).Kind;
   function Read return Kinematics_Parameters
   with
     Post =>
       Read'Result.Planner_Parameters.Tangential_Velocity_Max > 0.0 * mm / s
       and Read'Result.Planner_Parameters.Acceleration_Max > 0.0 * mm / s**2
       and Read'Result.Planner_Parameters.Jerk_Max > 0.0 * mm / s**3
       and Read'Result.Planner_Parameters.Snap_Max > 0.0 * mm / s**4
       and Read'Result.Planner_Parameters.Crackle_Max > 0.0 * mm / s**5
       and Read'Result.Planner_Parameters.Chord_Error_Max >= 0.0 * mm
       and (for all X of Read'Result.Planner_Parameters.Axial_Velocity_Maxes => X > 0.0 * mm / s)
       and (for all X of Read'Result.Planner_Parameters.Axial_Scaler => X > 0.0);
   function Read (Input_Switch : Input_Switch_Name) return Input_Switch_Parameters;
   function Read (Axis : Axis_Name) return Homing_Parameters
   with
     Post =>
       (for all A in Axis_Name =>
          (case Read'Result.Prerequisites (A).Kind is
             when No_Requirement_Kind | Must_Be_Homed_Kind => True,
             when Must_Be_At_Position_Kind =>
               Read'Result.Prerequisites (A).Position
               <= Kinematics_Parameters'(Read).Planner_Parameters.Upper_Pos_Limit (A)
               and Read'Result.Prerequisites (A).Position
                   >= Kinematics_Parameters'(Read).Planner_Parameters.Lower_Pos_Limit (A)))
       and (case Read'Result.Kind is
              when Disabled_Kind | Set_To_Value_Kind => True,
              when Double_Tap_Kind | StallGuard2_Kind | StallGuard4_Kind =>
                Read'Result.Velocity_Limit > 0.0 * mm / s
                and Read'Result.Move_To_After <= Kinematics_Parameters'(Read).Planner_Parameters.Upper_Pos_Limit (Axis)
                and Read'Result.Move_To_After
                    >= Kinematics_Parameters'(Read).Planner_Parameters.Lower_Pos_Limit (Axis));
   function Read (Thermistor : Thermistor_Name) return Thermistor_Parameters;
   function Read (Heater : Heater_Name) return Heater_Full_Parameters
   with Post => Read'Result.Params.Kind not in PID_Autotune_Kind;
   function Read (Fan : Fan_Name) return Fan_Parameters;
   function Read return G_Code_Assignment_Parameters
   with
     Post =>
       Read'Result.Has_Heaters = (Heater_Name'Pos (Heater_Name'Last) >= Heater_Name'Pos (Heater_Name'First))
       and Read'Result.Has_Fans = (Fan_Name'Pos (Fan_Name'Last) >= Fan_Name'Pos (Fan_Name'First))
       and Read'Result.Has_Lasers = (Laser_Name'Pos (Laser_Name'Last) >= Laser_Name'Pos (Laser_Name'First));
   function Read (Axis : Axis_Name) return Shaper_Parameters;
   function Read (Laser : Laser_Name) return Laser_Parameters;
   --  The above functions read the initial configuration values, not configurations values that have been changed
   --  after the first read.

   procedure Patch
     (Data : in out Ada.Strings.Unbounded.Unbounded_String; Report : access procedure (Key, Message : String));
   --  Sets Data and the configuration file to the union of Data and Get_Values. Also reports any errors in the union
   --  in the same way as Validate_Current_Config but atomically. The format of Data should match the format described
   --  in the Get_Values documentation comment. If the values do not match the schema then no values will be saved.

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

   Pool : System.Pool_Local.Unbounded_Reclaim_Pool;

   package Discrete_String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   type Property_Kind is
     (Boolean_Kind, Discrete_Kind, Integer_Kind, Float_Kind, Float_Ratio_Kind, Sequence_Kind, Variant_Kind);

   type Property_Parameters (Kind : Property_Kind);
   --  TODO: It's not ideal for the below access type to not be constant.
   type Property_Parameters_Access is not null access Property_Parameters with Storage_Pool => Pool;

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

   type Laser_Parameters_Array is array (Laser_Name) of Laser_Parameters;

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
      Lasers             : Laser_Parameters_Array;
   end record;

   type User_Config;

   function JSON_To_User_Config (Data : JSON_Value) return User_Config;
   procedure Validate_Config (Config : User_Config; Report : access procedure (Key, Message : String));
   function User_Config_To_Config (Data : User_Config) return Full_Config;

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
      procedure Read (Data : out Laser_Parameters; Laser : Laser_Name);
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

   type Dimensionless_Ratio is record
      Numerator   : Dimensionless;
      Denominator : Dimensionless;
   end record;

   --!PRUNT BEGIN USER CONFIG DECLARATIONS

   type User_Config_Prunt is record
      --  Description: This section contains general settings for Prunt.

      Enabled : Boolean := False;
      --  Key: Enabled
      --  Description: This is the main switch to enable or disable all functionality of your machine. For safety,
      --               Prunt will automatically disable itself if it detects a critical error, such as a heater failure
      --               or an invalid configuration. This prevents further operation until the issue is resolved. You
      --               will need to re-enable it manually after fixing the problem.

      Replace_G0_With_G1 : Boolean := False;
      --  Key: Replace G0 with G1
      --  Description: In G-code, G0 and G1 are both commands for linear movement. Technically, G0 is for rapid,
      --               non-printing moves, while G1 is for controlled, printing moves. Some 3D printer firmwares treat
      --               G0 and G1 identically, using the same feedrate for both. Other firmwares, including Prunt by
      --               default, use the maximum possible speed for G0 moves. If your slicer generates G-code that
      --               assumes G0 moves will be performed at the same speed as G1 moves, you should enable this
      --               setting. It will make Prunt treat all G0 commands as G1 commands, ensuring that the specified
      --               feedrate is used. Note that this setting only affects the movement speed; laser-based tools will
      --               always be disabled during G0 moves regardless of this setting.
   end record;

   type User_Config_Steppers_Direct_Entry is record
      --  Description: Use this option if you already know the exact distance the printer's axis moves for each step of
      --               the stepper motor. This is the most straightforward way to configure your steppers, but it
      --               requires you to have already calculated this value.

      Distance_Per_Step : Length := 1.0E100 * mm;
      --  Key: Distance per step
      --  Description: This is the linear distance that the axis moves for a single step of the motor. It's important
      --               to note that this value is for a full step, not a microstep. For example, if your stepper driver
      --               is set to 16 microsteps and your motor moves 0.01mm per microstep, you would enter 0.16mm here
      --               (0.01mm * 16).
      --  Min: 1.0E-100
      --  Max: 1.0E100

      Reverse_Direction : Boolean := False;
      --  Key: Reverse direction
      --  Description: If an axis moves in the opposite direction to what you expect (e.g., moving to the right when it
      --               should move to the left), you can enable this setting to reverse the motor's direction.
   end record;

   type User_Config_Steppers_Lead_Screw is record
      --  Description: Use this option to calculate the distance per step for an axis that is driven by a lead screw.

      Lead : Length := 1.0E100 * mm;
      --  Key: Lead
      --  Description: The lead of a screw is the linear distance the nut travels for one complete revolution of the
      --               screw. This is often confused with pitch, which is the distance between adjacent threads. For a
      --               single-start screw, the lead and pitch are the same. However, for multi-start screws, the lead
      --               is the pitch multiplied by the number of starts. For example, a lead screw with a 2mm pitch and
      --               4 starts has a lead of 8mm.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Reverse_Direction : Boolean := False;
      --  Key: Reverse direction
      --  Description: If an axis moves in the opposite direction to what you expect (e.g., moving to the right when it
      --               should move to the left), you can enable this setting to reverse the motor's direction.

      Gear_Ratio : Dimensionless_Ratio := (1.0, 1.0);
      --  Key: Gear ratio
      --  Description: If there is a gear system between the stepper motor and the lead screw, you need to specify the
      --               gear ratio here. The format is A:B, where A is the number of teeth on the gear attached to the
      --               lead screw, and B is the number of teeth on the gear attached to the motor. For a direct-drive
      --               system, where the motor is coupled directly to the lead screw, the gear ratio is 1:1.
      --  Min: 1.0E-100
      --  Max: 1.0E100

      Full_Steps_Per_Rotation : Dimensionless := 200.0;
      --  Key: Full steps per rotation
      --  Description: This is the number of full steps the stepper motor needs to complete one full 360-degree
      --               rotation. For most common stepper motors, this value is 200, which corresponds to a 1.8-degree
      --               step angle. Some motors have a 0.9-degree step angle, which means they have 400 steps per
      --               rotation.
      --  Min: 1.0E-100
      --  Max: 1.0E100
   end record;

   type User_Config_Steppers_Gear_With_Circumference is record
      --  Description: Use this option to calculate the distance per step for an axis driven by a belt and pulley
      --               system, where the circumference of the pulley is known.

      Circumference : Length := 1.0E100 * mm;
      --  Key: Circumference
      --  Description: This is the circumference of the pulley that drives the belt attached to the linearly moving
      --               part.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Reverse_Direction : Boolean := False;
      --  Key: Reverse direction
      --  Description: If an axis moves in the opposite direction to what you expect (e.g., moving to the right when it
      --               should move to the left), you can enable this setting to reverse the motor's direction.

      Gear_Ratio : Dimensionless_Ratio := (1.0, 1.0);
      --  Key: Gear ratio
      --  Description: If there is a gear system between the stepper motor and the pulley, specify the gear ratio here.
      --               The format is A:B, where A is the number of teeth on the gear attached to the pulley, and B is
      --               the number of teeth on the gear attached to the motor. For a direct-drive system, the gear ratio
      --               is 1:1.
      --  Min: 1.0E-100
      --  Max: 1.0E100

      Full_Steps_Per_Rotation : Dimensionless := 200.0;
      --  Key: Full steps per rotation
      --  Description: This is the number of full steps the stepper motor needs to complete one full 360-degree
      --               rotation. For most common stepper motors, this value is 200, which corresponds to a 1.8-degree
      --               step angle. Some motors have a 0.9-degree step angle, which means they have 400 steps per
      --               rotation.
      --  Min: 1.0E-100
      --  Max: 1.0E100
   end record;

   type User_Config_Steppers_Gear_With_Tooth_Count_And_Pitch is record
      --  Description: Use this option to calculate the distance per step for an axis driven by a belt and pulley
      --               system, using the pulley's tooth count and the belt's pitch.

      Tooth_Count : Dimensionless := 1.0E100;
      --  Key: Tooth count
      --  Description: This is the number of teeth on the pulley that drives the belt attached to the linearly moving
      --               part.
      --  Min: 1.0E-100
      --  Max: 1.0E100

      Tooth_Pitch : Length := 1.0E100 * mm;
      --  Key: Tooth pitch
      --  Description: This is the distance between the centres of two adjacent teeth on the belt. Common belt pitches
      --               in 3D printers are 2mm (for GT2 belts) and 3mm (for GT3 belts).
      --  Min: -1.0E100
      --  Max: 1.0E100

      Reverse_Direction : Boolean := False;
      --  Key: Reverse direction
      --  Description: If an axis moves in the opposite direction to what you expect (e.g., moving to the right when it
      --               should move to the left), you can enable this setting to reverse the motor's direction.

      Gear_Ratio : Dimensionless_Ratio := (1.0, 1.0);
      --  Key: Gear ratio
      --  Description: If there is a gear system between the stepper motor and the pulley, specify the gear ratio here.
      --               The format is A:B, where A is the number of teeth on the gear attached to the pulley, and B is
      --               the number of teeth on the gear attached to the motor. For a direct-drive system, the gear ratio
      --               is 1:1.
      --  Min: 1.0E-100
      --  Max: 1.0E100

      Full_Steps_Per_Rotation : Dimensionless := 200.0;
      --  Key: Full steps per rotation
      --  Description: This is the number of full steps the stepper motor needs to complete one full 360-degree
      --               rotation. For most common stepper motors, this value is 200, which corresponds to a 1.8-degree
      --               step angle. Some motors have a 0.9-degree step angle, which means they have 400 steps per
      --               rotation.
      --  Min: 1.0E-100
      --  Max: 1.0E100
   end record;

   type User_Config_Steppers_TMC2240_CHM_SpreadCycle_Derived is record
      --  Description: This option automatically calculates the optimal settings for the SpreadCycle chopper, which is
      --               a feature of Trinamic drivers that provides smooth and quiet motor operation. This is the
      --               recommended mode for most users when the phase inductance and resistance are known, these
      --               parameters may be found in your stepper motor's datasheet. If these parameters can not be found
      --               then the default parameters in the 'Manual' mode may be used.

      Input_Voltage : Voltage := 24.0 * volt;
      --  Key: Input voltage
      --  Description: This is the voltage supplied to the stepper motor driver, not the voltage rating of the stepper
      --               motor itself. It's crucial that this value is accurate. At startup, Prunt will measure the
      --               actual driver voltage and if it differs from this setting by more than 10%, it will generate an
      --               error.
      --  Min: 6.5
      --  Max: 40.0

      Phase_Inductance : Inductance := 0.000_000_1 * millihenry;
      --  Key: Phase inductance
      --  Description: This is the inductance of each of the motor's coils. You can find this value in your stepper
      --               motor's datasheet. Make sure to check that the units in the datasheet are the same as the units
      --               here.
      --  Min: 0.0000001
      --  Max: 10000000.0

      Phase_Resistance : Resistance := 0.000_000_1 * ohm;
      --  Key: Phase resistance
      --  Description: This is the resistance of each of the motor's coils. You can find this value in your stepper
      --               motor's datasheet.
      --  Min: 0.0000001
      --  Max: 10000000.0
   end record;

   type User_Config_Steppers_TMC2240_CHM_SpreadCycle_Manual is record
      --  Description: This section allows for manual configuration of the SpreadCycle chopper. These settings should
      --               only be adjusted by advanced users who are familiar with Trinamic driver tuning and have access
      --               to an oscilloscope to monitor the motor's current waveform. Incorrectly configured settings can
      --               lead to poor motor performance, excessive noise, and overheating. For most users, the 'Derived'
      --               mode is strongly recommended. If the parameters for the 'Derived' mode can not be found then
      --               the default values specified below may be used.

      HSTRT : Integer := 6;
      --  Key: HSTRT
      --  Description: This setting determines the start of the hysteresis for the SpreadCycle chopper. Refer to the
      --               TMC2240 datasheet for a detailed explanation. The value here is the resultant value (1 to 8),
      --               not the raw register value. The default is 6.
      --  Min: 1
      --  Max: 8

      HEND : Integer := -1;
      --  Key: HEND
      --  Description: This setting determines the end of the hysteresis for the SpreadCycle chopper. Refer to the
      --               TMC2240 datasheet for a detailed explanation. The value here is the resultant value (-3 to 12),
      --               not the raw register value. The default is -1.
      --  Min: -3
      --  Max: 12
   end record;

   type User_Config_TMC2240_CHM_Kind is
     (Derived,
      --  Key: Derived
      Manual
      --  Key: Manual
     );

   type User_Config_Steppers_TMC2240_CHM_SpreadCycle (Kind : User_Config_TMC2240_CHM_Kind := Derived) is record
      --  Description: This section allows you to choose between automatically calculated ('Derived') or manually
      --               configured SpreadCycle settings. For the vast majority of users, 'Derived' is the best choice.
      --               Only select 'Manual' if you are familiar with the tuning process for TMC2240 stepper drivers or
      --               if the parameters required for the 'Derived' mode can not be found.

      case Kind is
         when Derived =>
            Derived : User_Config_Steppers_TMC2240_CHM_SpreadCycle_Derived;
            --  Key: Derived

         when Manual =>
            Manual : User_Config_Steppers_TMC2240_CHM_SpreadCycle_Manual;
            --  Key: Manual
      end case;
   end record;

   type User_Config_Steppers_TMC2240_CHM_Constant_Off_Time is record
      --  Description: This option allows for configuration of the Constant Off-Time chopper mode. There is no
      --               automatic tuning for these parameters. It is strongly recommended to use the SpreadCycle chopper
      --               unless you have a specific reason to use this mode.

      DISFDCC : Boolean := False;
      --  Key: DISFDCC
      --  Description: This setting disables the use of the current comparator to terminate the fast decay cycle. When
      --               enabled, the fast decay cycle will end early if the negative current exceeds the previous
      --               positive current value. Refer to the TMC2240 datasheet for more details.

      OFFSET : Integer := -1;
      --  Key: OFFSET
      --  Description: This setting adjusts the sine wave offset. Refer to the TMC2240 datasheet for a detailed
      --               explanation. The value here is the resultant value (-3 to 12), not the raw register value.
      --  Min: -3
      --  Max: 12

      TFD : Integer := 5;
      --  Key: TFD
      --  Description: This setting controls the fast decay time. It sets both the FD3 and HSTRT_TFD210 bits in the
      --               CHOPCONF register. Refer to the TMC2240 datasheet for more details.
      --  Min: 0
      --  Max: 15
   end record;

   type User_Config_Distance_Per_Step_Kind is
     (Direct_Entry,
      --  Key: Direct entry
      Lead_Screw,
      --  Key: Lead screw
      Gear_With_Circumference,
      --  Key: Gear with circumference
      Gear_With_Tooth_Count_And_Pitch
      --  Key: Gear with tooth count and pitch
     );

   type User_Config_Steppers_Distance_Per_Step (Kind : User_Config_Distance_Per_Step_Kind := Direct_Entry) is record
      --  Description: This section determines how Prunt calculates the distance an axis moves for each step of the
      --               motor. You can either enter the value directly if you know it, or use one of the provided
      --               calculators for common mechanisms like lead screws and belt-driven systems.

      case Kind is
         when Direct_Entry =>
            Direct_Entry : User_Config_Steppers_Direct_Entry;
            --  Key: Direct_Entry

         when Lead_Screw =>
            Lead_Screw : User_Config_Steppers_Lead_Screw;
            --  Key: Lead_Screw

         when Gear_With_Circumference =>
            Gear_With_Circumference : User_Config_Steppers_Gear_With_Circumference;
            --  Key: Gear_With_Circumference

         when Gear_With_Tooth_Count_And_Pitch =>
            Gear_With_Tooth_Count_And_Pitch : User_Config_Steppers_Gear_With_Tooth_Count_And_Pitch;
            --  Key: Gear_With_Tooth_Count_And_Pitch
      end case;
   end record;

   type User_Config_Steppers_TMC2240_CHM_Kind is
     (SpreadCycle,
      --  Key: SpreadCycle
      Constant_Off_Time
      --  Key: Constant off time
     );

   type User_Config_Steppers_TMC2240_CHM (Kind : User_Config_Steppers_TMC2240_CHM_Kind := SpreadCycle) is record
      --  Description: This setting selects the chopper mode for the stepper motor driver. The chopper is responsible
      --               for controlling the current to the motor coils, which in turn affects the motor's performance,
      --               noise level, and heat generation.
      --
      --               SpreadCycle is the recommended mode for most applications. Constant Off-Time is a more
      --               traditional chopper mode that may be useful in some specific situations, but it generally worse
      --               than SpreadCycle.
      --
      --               This setting is used when StealthChop2 is disabled, or when the motor's velocity exceeds the
      --               TPWMTHRS limit for StealthChop2. The VHIGHCHM setting can also override this when the velocity
      --               exceeds the VHIGH limit.

      case Kind is
         when SpreadCycle =>
            SpreadCycle : User_Config_Steppers_TMC2240_CHM_SpreadCycle;
            --  Key: SpreadCycle

         when Constant_Off_Time =>
            Constant_Off_Time : User_Config_Steppers_TMC2240_CHM_Constant_Off_Time;
            --  Key: Constant_Off_Time
      end case;
   end record;

   type User_Config_Steppers_TMC2240_StealthChop2_Enabled is record
      --  Description: This section contains the settings for StealthChop2. For most users, the settings, aside from
      --               TPWMTHRS, should always be set to the default values with auto-tuning enabled.

      TPWMTHRS : Velocity := 1.0E100 * mm / s;
      --  Key: TPWMTHRS
      --  Description: This is the velocity threshold at which the driver will switch from StealthChop2 to the chopper
      --               mode selected in the CHM section (usually SpreadCycle). This allows for quiet operation at low
      --               speeds and high-torque operation at high speeds. The default value of 1E100 will cause
      --               StealthChop2 to always be active.
      --  Min: 0.0
      --  Max: 1.0E100

      PWM_OFS : TMC_Types.Unsigned_8 := 29;
      --  Key: PWM_OFS
      --  Description: This setting defines the fixed part of the maximum PWM amplitude in StealthChop2. It's generally
      --               recommended to leave this at the default value of 29 and enable PWM_AUTOSCALE to allow the
      --               driver to automatically tune this value.
      --  Min: 0
      --  Max: 255

      PWM_GRAD : TMC_Types.Unsigned_8 := 0;
      --  Key: PWM_GRAD
      --  Description: This setting defines the velocity-dependent part of the maximum PWM amplitude in StealthChop2.
      --               It's recommended to leave this at the default value of 0 and enable PWM_AUTOGRAD to allow the
      --               driver to automatically tune this value.
      --  Min: 0
      --  Max: 255

      PWM_FREQ : TMC_Types.TMC2240.PWM_Freq_Type := Freq_683;
      --  Key: PWM_FREQ
      --  Description: This setting determines the PWM frequency for StealthChop2. The duration is measured in half TMC
      --               clock cycles (a half cycle is typically 40ns). A value of 683, resulting in a frequency of
      --               approximately 36kHz, is a good starting point for most motors.
      --  Map: "1024" => Freq_1024
      --       "683"  => Freq_683
      --       "512"  => Freq_512
      --       "410"  => Freq_410

      PWM_AUTOSCALE : Boolean := True;
      --  Key: PWM_AUTOSCALE
      --  Description: This enables automatic tuning of the PWM_OFS value. It is highly recommended to keep this
      --               enabled, as disabling it cause current limits to not be correctly applied, potentially resulting
      --               in destruction of the motor.

      PWM_AUTOGRAD : Boolean := True;
      --  Key: PWM_AUTOGRAD
      --  Description: This enables automatic tuning of the PWM_GRAD value. It is highly recommended to keep this
      --               enabled, as disabling it cause current limits to not be correctly applied, potentially resulting
      --               in destruction of the motor.

      FREEWHEEL : TMC_Types.TMC2240.Freewheel_Type := Normal;
      --  Key: FREEWHEEL
      --  Description: This setting determines the behavior of the motor coils when the motor is at a standstill and
      --               the hold current (IHOLD) is set to 0. Refer to the TMC2240 datasheet for a detailed explanation
      --               of the different freewheeling options. This setting is not relevant for most users.
      --  Map: "NORMAL"       => Normal
      --       "FREEWHEEL"    => Freewheel
      --       "SHORT_VIA_LS" => Short_Via_LS
      --       "SHORT_VIA_HS" => Short_Via_HS

      PWM_MEAS_SD_ENABLE : Boolean := False;
      --  Key: PWM_MEAS_SD_ENABLE
      --  Description: This setting enables the use of the slow decay phase on the low side of the H-bridge to measure
      --               the motor current while in StealthChop2 mode.

      PWM_DIS_REG_STST : Boolean := False;
      --  Key: PWM_DIS_REG_STST
      --  Description: This setting disables StealthChop2 current regulation when the motor is at a standstill, instead
      --               the duty cycle is reduced to a very low value.

      PWM_REG : TMC_Types.Unsigned_4 := 4;
      --  Key: PWM_REG
      --  Description: This setting controls the rate of change for the automatic PWM amplitude scaling in
      --               StealthChop2. The value is measured in half increments, so a value of 4 corresponds to 2
      --               increments per half wave.
      --  Min: 0
      --  Max: 15

      PWM_LIM : TMC_Types.Unsigned_4 := 12;
      --  Key: PWM_LIM
      --  Description: This setting limits the amplitude of the automatic PWM scaling when the driver switches from
      --               SpreadCycle to StealthChop2. It limits the upper 4 bits of the PWM value.
      --  Min: 0
      --  Max: 15

      MULTISTEP_FILT : Boolean := False;
      --  Key: MULTISTEP_FILT
      --  Description: This is a filtering feature for StealthChop2 with an undocumented implementation. On official
      --               Prunt hardware, the generated step signals have extremely low jitter, so this filtering is not
      --               necessary and should be left disabled.
   end record;

   type User_Config_Steppers_TMC2240_StealthChop2_Disabled is record
      --  Description: When this option is selected, StealthChop2 is always disabled. The driver will use the chopper
      --               mode selected in the CHM section (usually SpreadCycle) for all movements.
      null;
   end record;

   type User_Config_TMC2240_StealthChop2_Kind is
     (Disabled,
      --  Key: Disabled
      Enabled
      --  Key: Enabled
     );

   type User_Config_Steppers_TMC2240_StealthChop2 (Kind : User_Config_TMC2240_StealthChop2_Kind := Disabled) is record
      --  Description: This section allows you to enable or disable StealthChop2 for this driver. StealthChop2 is a
      --               feature of TMC2240 stepper drivers that allows for extremely quiet stepper motor operation,
      --               especially at low speeds. While it can make your printer nearly silent, it may also have less
      --               torque than SpreadCycle, especially at higher speeds.

      case Kind is
         when Disabled =>
            Disabled : User_Config_Steppers_TMC2240_StealthChop2_Disabled := (others => <>);
            --  Key: Disabled

         when Enabled =>
            Enabled : User_Config_Steppers_TMC2240_StealthChop2_Enabled := (others => <>);
            --  Key: Enabled
      end case;
   end record;

   type User_Config_Stepper_Basic is record
      --  Description: This section contains the basic configuration for a stepper motor driver. These settings are
      --               applicable to any stepper driver that is controlled by step and direction signals.

      Enabled : Boolean := False;
      --  Key: Enabled
      --  Description: Enable this stepper driver. If a driver is not enabled, it cannot be assigned to an axis and
      --               will not be used.

      Microsteps : Dimensionless := 1.0;
      --  Key: Microsteps
      --  Description: This is the number of microsteps configured on the external stepper driver. This will not change
      --               the configuration of the stepper driver itself, it is simply used to make configuration of the
      --               distance per step easier.
      --  Min: 1.0
      --  Max: 1.0E100

      Distance_Per_Step : User_Config_Steppers_Distance_Per_Step := (others => <>);
      --  Key: Distance per step
   end record;

   type User_Config_Stepper_TMC2240 is record
      --  Description: This section contains the configuration settings for a TMC2240 stepper motor driver. These
      --               drivers offer many advanced features for quiet and smooth operation.

      Enabled : Boolean := False;
      --  Key: Enabled
      --  Description: Enable this stepper driver. If a driver is not enabled, it cannot be assigned to an axis and
      --               will not be used.

      Distance_Per_Step : User_Config_Steppers_Distance_Per_Step := (others => <>);
      --  Key: Distance per step

      Run_Current : Current := 0.125 * amp;
      --  Key: Run current
      --  Description: This is the peak current that will be supplied to each coil of the stepper motor. Consult your
      --               stepper motor's datasheet for the recommended current rating. Prunt will automatically select
      --               the smallest suitable current range on the driver and use the GLOBALSCALER register to fine-tune
      --               the current.
      --  Min: 0.125
      --  Max: 3.0
      IHOLD       : Dimensionless := 1.0;
      --  Key: IHOLD
      --  Description: This setting determines the percentage of the run current that is used when the motor is not
      --               moving. A value of 1.0 corresponds to 100% of the run current, while a value of 0.0 corresponds
      --               to 0%. The value has a resolution of 1/32, but any value may be enetered here as it is
      --               automatically rounded.
      --  Min: 0.03125
      --  Max: 1.0

      IRUN : Dimensionless := 1.0;
      --  Key: IRUN
      --  Description: This setting determines the percentage of the run current that is used when the motor is moving.
      --               A value of 1.0 corresponds to 100% of the run current, while a value of 0.0 corresponds to 0%.
      --               The value has a resolution of 1/32, but any value may be enetered here as it is automatically
      --               rounded.
      --  Min: 0.03125
      --  Max: 1.0

      IRUN_During_Homing : Dimensionless := 1.0;
      --  Key: IRUN during homing
      --  Description: This setting allows you to specify a different IRUN value to be used during homing moves. This
      --               can be useful for sensorless homing, where a lower current may be needed to reliably detect a
      --               stall. The value has a resolution of 1/32, but any value may be enetered here as it is
      --               automatically rounded.
      --  Min: 0.03125
      --  Max: 1.0

      IHOLDDELAY : Time := 315.0 * ms;
      --  Key: IHOLDDELAY
      --  Description: This is the time it takes for the driver to reduce the current from the run level to the hold
      --               level after the stepper driver detects the motor has stopped (see FAST_STANDSTILL below) and
      --               after TPOWERDOWN (specified below). The resolution of this setting is 21ms, but any value may be
      --               enetered here as it is automatically rounded.
      --  Min: 0.0
      --  Max: 315.0

      IRUNDELAY : Time := 0.0 * ms;
      --  Key: IRUNDELAY
      --  Description: This is the time it takes for the driver to increase the current from the hold level to the run
      --               level when the motor starts moving again. The resolution of this setting is 0.041ms, but any
      --               value may be enetered here as it is automatically rounded.
      --  Min: 0.0
      --  Max: 0.615

      TPOWERDOWN : Time := 5_355.0 * ms;
      --  Key: TPOWERDOWN
      --  Description: This is the delay after the motor stops before the driver begins to reduce the current to the
      --               hold level (as defined by IHOLD). A minimum value of 42ms is required for the automatic tuning
      --               of StealthChop2 to work correctly. The resolution of this setting is 21ms, but any value may be
      --               enetered here as it is automatically rounded.
      --  Min: 0.0
      --  Max: 5355.0

      THIGH : Velocity := 1.0E100 * mm / s;
      --  Key: THIGH
      --  Description: This velocity threshold is used to switch the driver into a high-velocity mode controlled by
      --               VHIGHFS and VHIGHCHM. It also serves as an upper velocity limit for features CoolStep and
      --               StealthChop2, if they are enabled.
      --  Min: 0.0
      --  Max: 1.0E100

      SLOPE_CONTROL : TMC_Types.TMC2240.Slope_Control_Type := Slope_400V_Per_us;
      --  Key: SLOPE_CONTROL
      --  Description: This setting controls the slew rate of the driver's output. A higher slew rate can reduce power
      --               dissipation, but may also increase electromagnetic interference (EMI). 400V/us is usually a good
      --               setting.
      --  Map: "SLOPE_100V_PER_US" => Slope_100V_Per_us
      --       "SLOPE_200V_PER_US" => Slope_200V_Per_us
      --       "SLOPE_400V_PER_US" => Slope_400V_Per_us
      --       "SLOPE_800V_PER_US" => Slope_800V_Per_us

      TOFF : TMC_Types.TMC2240.TOFF_Type := Off_120;
      --  Key: TOFF
      --  Description: This setting determines the duration of the slow decay (off-time) phase of the chopper cycle,
      --               measured in TMC clock cycles (typically 80ns). A value of 120, combined with a TBL setting of
      --               36, is a good starting point that results in a theoretical maximum chopper frequency of around
      --               40kHz.
      --  Map: "56"  => Off_56
      --       "88"  => Off_88
      --       "120" => Off_120
      --       "152" => Off_152
      --       "184" => Off_184
      --       "216" => Off_216
      --       "248" => Off_248
      --       "280" => Off_280
      --       "312" => Off_312
      --       "344" => Off_344
      --       "376" => Off_376
      --       "408" => Off_408
      --       "440" => Off_440
      --       "472" => Off_472
      --       "504" => Off_504

      TBL : TMC_Types.TMC2240.TBL_Type := Blank_36;
      --  Key: TBL
      --  Description: This setting determines the blanking time for the current sense comparators, measured in TMC
      --               clock cycles (typically 80ns).
      --  Map: "24" => Blank_24
      --       "36" => Blank_36
      --       "54" => Blank_54

      VHIGHFS : Boolean := False;
      --  Key: VHIGHFS
      --  Description: When this setting is enabled, the driver will switch to full-step mode (disabling microstepping)
      --               when the velocity exceeds the THIGH threshold.

      VHIGHCHM : Boolean := False;
      --  Key: VHIGHCHM
      --  Description: When this setting is enabled, the driver will switch to the constant off-time chopper mode, set
      --               TFD to 0, and approximately double the TOFF time when the velocity exceeds the THIGH threshold.

      TPFD : TMC_Types.Unsigned_4 := 4;
      --  Key: TPFD
      --  Description: This setting determines the duration of the passive fast decay phase after a change in the
      --               bridge polarity. The duration, in TMC clock cycles (typically 80ns), is 128 multiplied by this
      --               value.
      --  Min: 0
      --  Max: 15

      MRES : TMC_Types.TMC2240.Microstep_Resolution_Type := MS_256;
      --  Key: MRES
      --  Description: This setting determines the microstep resolution for the driver. Higher microstep resolutions
      --               result in smoother and quieter motor operation, but can limit the maximum achievable speed. 256
      --               microsteps is the highest resolution available and is generally recommended as Prunt will slow
      --               down and emit a warning when the maximum step rate would be exceeded by a G-code command rather
      --               than halting.
      --  Map: "MS_256"        => MS_256
      --       "MS_128"        => MS_128
      --       "MS_64"         => MS_64
      --       "MS_32"         => MS_32
      --       "MS_16"         => MS_16
      --       "MS_8"          => MS_8
      --       "MS_4"          => MS_4
      --       "MS_2"          => MS_2
      --       "MS_FULL_STEPS" => MS_Full_Steps

      FAST_STANDSTILL : Boolean := False;
      --  Key: FAST_STANDSTILL
      --  Description: When enabled, the driver will wait for a shorter period of time (2^18 TMC clock cycles,
      --               typically 21ms) after the last step signal before it begins to detect a standstill. When
      --               disabled, the wait time is 2^20 cycles (typically 84ms).

      CHM : User_Config_Steppers_TMC2240_CHM := (others => <>);
      --  Key: CHM

      StealthChop2 : User_Config_Steppers_TMC2240_StealthChop2 := (others => <>);
      --  Key: StealthChop2 (EN_PWM_MODE)
   end record;

   type User_Config_Stepper (Fixed_Kind : Stepper_Hardware_Kind := Basic_Kind) is record
      --  Description: This section contains the configuration for a single stepper motor driver.

      case Fixed_Kind is
         when Basic_Kind =>
            Basic_Kind : User_Config_Stepper_Basic := (others => <>);
            --  Key: Basic_Kind

         when TMC2240_UART_Kind =>
            TMC_2240_Kind : User_Config_Stepper_TMC2240 := (others => <>);
            --  Key: TMC2240_UART_Kind
      end case;
   end record;

   type User_Config_Cartesian_Axis_Name is
     (None,
      --  Key: NONE
      X_Axis,
      --  Key: X_AXIS
      Y_Axis,
      --  Key: Y_AXIS
      Z_Axis,
      --  Key: Z_AXIS
      E_Axis
      --  Key: E_AXIS
     );

   type User_Config_Kinematics_Cartesian is array (Stepper_Name) of User_Config_Cartesian_Axis_Name;

   type User_Config_Core_XY_Axis_Name is
     (None,
      --  Key: NONE
      A_Axis,
      --  Key: A_AXIS
      B_Axis,
      --  Key: B_AXIS
      Z_Axis,
      --  Key: Z_AXIS
      E_Axis
      --  Key: E_AXIS
     );

   type User_Config_Kinematics_Core_XY is array (Stepper_Name) of User_Config_Core_XY_Axis_Name;

   type User_Config_Kinematics_Kind is
     (Cartesian,
      --  Key: Cartesian
      Core_XY
      --  Key: Core XY
     );

   type User_Config_Kinematics_Variant (Kind : User_Config_Kinematics_Kind := Cartesian) is record
      --  Description: This setting defines the kinematic system of your machine. The kinematics determine how the
      --               movement of the individual stepper motors is translated into the movement of the toolhead in the
      --               X, Y, and Z dimensions.

      case Kind is
         when Cartesian =>
            Cartesian : User_Config_Kinematics_Cartesian := (others => None);
            --  Key: Cartesian
            --  Description: For a Cartesian machine, where a given motor only moves one axis, assign each stepper
            --               motor to the axis it controls (X, Y, Z, or E for extruder). If a stepper is not used,
            --               assign it to 'None'.

         when Core_XY =>
            Core_XY : User_Config_Kinematics_Core_XY := (others => None);
            --  Key: Core_XY
            --  Description: For a Core XY machine, where some motors move both the X and Y axis, assign the two motors
            --               that control the X and Y movement to the A and B axes. Assign the remaining motors to the
            --               axes they directly control (E for extruder and Z). If a stepper is not used, assign it to
            --               'None'.
      end case;
   end record;

   type User_Config_Axial_Velocity_Limits_Array is array (Axis_Name) of Velocity;

   type User_Config_Kinematics is record
      --  Description: This section contains settings related to the machine's movement, geometry, and motion planning.

      Lower_Position_Limit : Position := (others => 0.0 * mm);
      --  Key: Lower position limit
      --  Description: This defines the minimum position that each axis can travel to. To effectively disable the lower
      --               limit for an axis, you can set it to -1E100. The E axis should almost always be set to -1E100.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Upper_Position_Limit : Position := (others => 0.0 * mm);
      --  Key: Upper position limit
      --  Description: This defines the maximum position that each axis can travel to. To effectively disable the upper
      --               limit for an axis, you can set it to 1E100. The E axis should almost always be set to 1E100.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Ignore_E_In_XYZE : Boolean := True;
      --  Key: Ignore E in XYZE
      --  Description: This setting changes how the feedrate is applied when both the extruder (E) and other axes (X,
      --               Y, Z) are moving simultaneously. When enabled (which is the default and mimics the behavior of
      --               most other 3D printer firmwares), the feedrate specified in the G-code command will only apply
      --               to the X, Y, and Z axes. The extruder will move as fast as necessary to keep up, within its
      --               velocity limits. This is generally the desired behavior. If you disable this setting, the
      --               feedrate will be distributed among all moving axes, including the extruder.
      --
      --               For example, with the command 'G1 X1 E100 F100', if this setting is enabled, the X-axis will
      --               move at 100 mm/min, and the E-axis will move proportionally. If disabled, the combined speed of
      --               the X and E axes will be 100 mm/min, meaning the X axis will only move at approximately 1
      --               mm/min.
      --
      --               Regardless of this setting, the individual feedrate limits for each axis will always be
      --               respected.

      Shift_Blended_Corners : Boolean := False;
      --  Key: Shift blended corners
      --  Description: When the motion planner blends corners to maintain a higher speed, it does so by creating a
      --               curved path that cuts inside the original corner. When this setting is enabled, Prunt will
      --               attempt to shift the blended corner path so that it intersects the original corner point. This
      --               can result in a path that is slightly more faithful to curved sections in the original CAD
      --               models before export and slicing, but it also means that the straight line segments leading into
      --               and out of the corner will be shifted outwards slightly.

      Maximum_Tangential_Velocity : Velocity := 10.0 * mm / s;
      --  Key: Maximum tangential velocity
      --  Description: This is the maximum combined speed of all axes, including the extruder. It's a global limit on
      --               the toolhead's speed. In most cases, it's better to set this to a very high value (e.g., 1E100)
      --               and use the per-axis velocity limits below to control the speed of your machine.
      --  Min: 0.000001
      --  Max: 1.0E100

      Axial_Velocity_Limits : User_Config_Axial_Velocity_Limits_Array := (others => 10.0 * mm / s);
      --  Key: Axial velocity limits
      --  Description: This sets the maximum speed for each individual axis.
      --  Min: 0.000001
      --  Max: 1.0E100

      Maximum_Chord_Error : Length := 0.1 * mm;
      --  Key: Maximum chord error
      --  Description: This setting controls how far a path is allowed to deviate from the path specified in G-code.
      --               Instead of coming to a complete stop at every corner, the motion planner can create a smooth,
      --               curved path that 'cuts' the corner. This allows the machine to maintain a higher average speed.
      --               This setting defines the maximum allowed distance between the curved path and the original,
      --               sharp corner. A value of 0 will disable corner blending, causing the machine to come to a full
      --               stop at every corner.
      --  Min: 0.0
      --  Max: 1.0E100

      Maximum_Acceleration : Acceleration := 100.0 * mm / s**2;
      --  Key: Maximum acceleration
      --  Description: This is the maximum rate at which the printer can change its velocity. A higher acceleration
      --               will result in faster prints, but may also cause vibrations and ringing artifacts. This can be
      --               set to a very high value 1E100 to effectively disable the limit and rely on the higher order
      --               constraints below.
      --  Min: 0.000001
      --  Max: 1.0E100

      Maximum_Jerk : Jerk := 100.0E2 * mm / s**3;
      --  Key: Maximum jerk
      --  Description: Jerk is the rate of change of acceleration. It determines how abruptly the printer can change
      --               its acceleration. A higher jerk value allows for faster changes in direction, but can also
      --               introduce vibrations. A good starting point for tuning is to set the jerk to 100 times the
      --               maximum acceleration (e.g., if acceleration is 1000, set jerk to 100,000). You can do this by
      --               appending 'E2' to your acceleration value in this field. This can be set to a very high value
      --               1E100 to effectively disable the limit.
      --  Min: 0.000001
      --  Max: 1.0E100

      Maximum_Snap : Snap := 100.0E5 * mm / s**4;
      --  Key: Maximum snap
      --  Description: Snap is the rate of change of jerk. It's a higher-order derivative of motion that can help to
      --               smooth out movements even further. A good starting point for tuning is to set the snap to
      --               100,000 times the maximum acceleration (append 'E5' to your acceleration value in this field).
      --               This can be set to a very high value 1E100 to effectively disable the limit.
      --  Min: 0.000001
      --  Max: 1.0E100

      Maximum_Crackle : Crackle := 100.0E9 * mm / s**5;
      --  Key: Maximum crackle
      --  Description: Crackle is the rate of change of snap. It's an even higher-order derivative of motion. A good
      --               starting point for tuning is to set the crackle to 1,000,000,000 times the maximum acceleration
      --               (append 'E9' to your acceleration value in this field). This can be set to a very high value
      --               1E100 to effectively disable the limit.
      --  Min: 0.000001
      --  Max: 1.0E100

      Axial_Scaler : Position_Scale := (others => 1.0);
      --  Key: Axial scaler
      --  Description: Inside the motion planner, all positions are divided by this value before applying motion
      --               profile limits, allowing for different limits on different axes. You do not need to take this
      --               value in to account when setting position limits, mm per step values, axial velocity limits, or
      --               when setting the feedrate in g-code. Corner deviation and tangential feedrate, acceleration,
      --               etc. is based on scaled positions, so a tangential acceleration of 10mm/s^2 and a scaler of 0.5
      --               will set the axial limit to 5mm/s^2.
      --  Min: 1.0E-100
      --  Max: 1.0E100

      Kinematics_Kind : User_Config_Kinematics_Variant := (others => <>);
      --  Key: Kinematics kind
   end record;

   type User_Config_Input_Switch_Visible is record
      --  Description: This section contains the configuration for a single input switch.

      Enabled : Boolean := False;
      --  Key: Enabled
      --  Description: Enable this switch. If a switch is not enabled, it cannot be used for homing or other functions.

      Hit_On_High : Boolean := False;
      --  Key: Hit on high
      --  Description: This setting determines the logic level that triggers the switch. If your switch is normally
      --               open (NO) you likely want this setting disabled. If your switch is normally closed (NC) then you
      --               likely want to enable this setting.
   end record;

   type User_Config_Input_Switch_Not_Visible is record
      --  This record will never be visible to the user as we filter out non-visible input switches from
      --  Input_Switch_Name_Strings.
      null;
   end record;

   type User_Config_Input_Switch_Kind is (Visible, Not_Visible);

   type User_Config_Input_Switch (Fixed_Kind : User_Config_Input_Switch_Kind := Visible) is record
      --  Description: This section contains the configuration for a single input switch.

      case Fixed_Kind is
         when Visible =>
            Visible : User_Config_Input_Switch_Visible;
            --  Key: Visible

         when Not_Visible =>
            Not_Visible : User_Config_Input_Switch_Not_Visible;
            --  Key: Not_Visible
      end case;
   end record;

   type User_Config_Thermistors_Disabled is record
      --  Description: The thermistor is disabled and cannot be used.
      null;
   end record;

   type User_Config_Thermistors_ATC_Semitec_104GT_2 is record
      --  Description: This option sets values recommended for the ATC Semitec 104GT-2 thermistor. The temperature is
      --               calculated using the Steinhart-Hart equation with the coefficients A=8.0965E-4, B=2.1163E-4, and
      --               C=7.0742E-8. Always verify that the temperature readings are accurate before using the heater.
      null;
   end record;

   type User_Config_Thermistors_ATC_Semitec_104NT_4_R025H42G is record
      --  Description: This option sets values recommended for the ATC Semitec 104NT-4-R025H42G thermistor. The
      --               temperature is calculated using the Steinhart-Hart equation with the coefficients A=7.9582E-4,
      --               B=2.1360E-4, and C=6.4830E-8. Always verify that the temperature readings are accurate before
      --               using the heater.
      null;
   end record;

   type User_Config_Thermistors_EPCOS_100K_B57560G104F is record
      --  Description: This option sets values recommended for the EPCOS 100K B57560G104F thermistor. The temperature
      --               is calculated using the Steinhart-Hart equation with the coefficients A=7.2213E-4, B=2.1676E-4,
      --               and C=8.9293E-8. Always verify that the temperature readings are accurate before using the
      --               heater.
      null;
   end record;

   type User_Config_Thermistors_Generic_3950 is record
      --  Description: This option sets values recommended for a generic 100k thermistor with a B-value of 3950. The
      --               temperature is calculated using the Steinhart-Hart equation with the coefficients A=7.9347E-4,
      --               B=2.0076E-4, and C=1.6328E-7. Always verify that the temperature readings are accurate before
      --               using the heater.
      null;
   end record;

   type User_Config_Thermistors_SliceEngineering_450 is record
      --  Description: This option sets values recommended for the Slice Engineering 450 thermistor. The temperature is
      --               calculated using the Steinhart-Hart equation with the coefficients A=3.0553E-4, B=2.1171E-4, and
      --               C=1.1962E-7. Always verify that the temperature readings are accurate before using the heater.
      null;
   end record;

   type User_Config_Thermistors_TDK_NTCG104LH104JT1 is record
      --  Description: This option sets values recommended for the TDK NTCG104LH104JT1 thermistor. The temperature is
      --               calculated using the Steinhart-Hart equation with the coefficients A=9.7639E-4, B=1.9688E-4, and
      --               C=7.2671E-8. Always verify that the temperature readings are accurate before using the heater.
      null;
   end record;

   type User_Config_Thermistors_Honeywell_100K_135_104LAG_J01 is record
      --  Description: This option sets values recommended for the Honeywell 100K 135-104LAG-J01 thermistor. The
      --               temperature is calculated using the Steinhart-Hart equation with the coefficients A=4.5695E-4,
      --               B=2.5163E-4, and C=0. Always verify that the temperature readings are accurate before using the
      --               heater.
      null;
   end record;

   type User_Config_Thermistors_NTC_100K_MGB18_104F39050L32 is record
      --  Description: This option sets values recommended for the NTC 100K MGB18-104F39050L32 thermistor. The
      --               temperature is calculated using the Steinhart-Hart equation with the coefficients A=5.4598E-4,
      --               B=2.4390E-4, and C=0. Always verify that the temperature readings are accurate before using the
      --               heater.
      null;
   end record;

   type User_Config_Thermistors_PT_1000_PT_385 is record
      --  Description: This option sets values recommended for a PT-385 class PT1000 RTD sensor above 0 Celsius. The
      --               temperature is calculated using the Callendar-Van Dusen equation with R(0)=1000, A=3.9083E-3,
      --               and B=-5.775E-7. Always verify that the temperature readings are accurate before use.
      null;
   end record;

   type User_Config_Thermistors_PT_1000_PT_392 is record
      --  Description: This option sets values recommended for a PT-392 class PT1000 RTD sensor above 0 Celsius. The
      --               temperature is calculated using the Callendar-Van Dusen equation with R(0)=1000, A=3.9827E-3,
      --               and B=-5.875E-7. Always verify that the temperature readings are accurate before use.
      null;
   end record;

   type User_Config_Thermistors_Custom_Steinhart_Hart is record
      --  Description: If your thermistor is not in the list of predefined thermistors, you can use this section to
      --               manually enter its Steinhart-Hart coefficients. The equation is '1/T = A + B*ln(R) +
      --               C*(ln(R))^3' where T is the temperature in Kelvin and R is the resistance of the thermistor in
      --               Ohms.

      A : Dimensionless := 0.0;
      --  Key: A
      --  Min: -1.0E100
      --  Max: 1.0E100

      B : Dimensionless := 0.0;
      --  Key: B
      --  Min: -1.0E100
      --  Max: 1.0E100

      C : Dimensionless := 0.0;
      --  Key: C
      --  Min: -1.0E100
      --  Max: 1.0E100
   end record;

   type User_Config_Thermistors_Custom_Callendar_Van_Dusen is record
      --  Description: If you are using an RTD sensor that is not in the list, you can use this section to manually
      --               enter its Callendar-Van Dusen coefficients. The equation is 'R(T) = R(0) * (1 + A*T + B*T^2)'
      --               where T is the temperature in Celsius, R(T) is the resistance at that temperature, and R(0) is
      --               the resistance at 0 Celsius.

      R0 : Resistance := 0.0 * ohm;
      --  Key: R(0)
      --  Min: -1.0E100
      --  Max: 1.0E100

      A : Dimensionless := 0.0;
      --  Key: A
      --  Min: -1.0E100
      --  Max: 1.0E100

      B : Dimensionless := 0.0;
      --  Key: B
      --  Min: -1.0E100
      --  Max: 1.0E100
   end record;

   type User_Config_Thermistor_Kind is
     (Disabled,
      --  Key: Disabled

      ATC_Semitec_104GT_2,
      --  Key: ATC Semitec 104GT-2

      ATC_Semitec_104NT_4_R025H42G,
      --  Key: ATC Semitec 104NT-4-R025H42G

      EPCOS_100K_B57560G104F,
      --  Key: EPCOS 100K B57560G104F

      Generic_3950,
      --  Key: Generic 3950

      SliceEngineering_450,
      --  Key: SliceEngineering 450

      TDK_NTCG104LH104JT1,
      --  Key: TDK NTCG104LH104JT1

      Honeywell_100K_135_104LAG_J01,
      --  Key: Honeywell 100K 135-104LAG-J01

      NTC_100K_MGB18_104F39050L32,
      --  Key: NTC 100K MGB18-104F39050L32

      PT_1000_PT_385,
      --  Key: PT-1000 (PT-385 class above 0C)

      PT_1000_PT_392,
      --  Key: PT-1000 (PT-392 class above 0C)

      Custom_Steinhart_Hart_Model,
      --  Key: Custom Steinhart-Hart model

      Custom_Callendar_Van_Dusen_Model
      --  Key: Custom Callendar-Van Dusen model
     );

   type User_Config_Thermistors_Variant (Kind : User_Config_Thermistor_Kind := Disabled) is record
      --  Description: This section allows you to select the type of thermistor or RTD sensor that is connected to this
      --               input. If your sensor is not in the list, you can select one of the custom models and enter the
      --               coefficients manually.

      case Kind is
         when Disabled =>
            Disabled : User_Config_Thermistors_Disabled;
            --  Key: Disabled

         when ATC_Semitec_104GT_2 =>
            ATC_Semitec_104GT_2 : User_Config_Thermistors_ATC_Semitec_104GT_2;
            --  Key: ATC_Semitec_104GT_2

         when ATC_Semitec_104NT_4_R025H42G =>
            ATC_Semitec_104NT_4_R025H42G : User_Config_Thermistors_ATC_Semitec_104NT_4_R025H42G;
            --  Key: ATC_Semitec_104NT_4_R025H42G

         when EPCOS_100K_B57560G104F =>
            EPCOS_100K_B57560G104F : User_Config_Thermistors_EPCOS_100K_B57560G104F;
            --  Key: EPCOS_100K_B57560G104F

         when Generic_3950 =>
            Generic_3950 : User_Config_Thermistors_Generic_3950;
            --  Key: Generic_3950

         when SliceEngineering_450 =>
            SliceEngineering_450 : User_Config_Thermistors_SliceEngineering_450;
            --  Key: SliceEngineering_450

         when TDK_NTCG104LH104JT1 =>
            TDK_NTCG104LH104JT1 : User_Config_Thermistors_TDK_NTCG104LH104JT1;
            --  Key: TDK_NTCG104LH104JT1

         when Honeywell_100K_135_104LAG_J01 =>
            Honeywell_100K_135_104LAG_J01 : User_Config_Thermistors_Honeywell_100K_135_104LAG_J01;
            --  Key: Honeywell_100K_135_104LAG_J01

         when NTC_100K_MGB18_104F39050L32 =>
            NTC_100K_MGB18_104F39050L32 : User_Config_Thermistors_NTC_100K_MGB18_104F39050L32;
            --  Key: NTC_100K_MGB18_104F39050L32

         when PT_1000_PT_385 =>
            PT_1000_PT_385 : User_Config_Thermistors_PT_1000_PT_385;
            --  Key: PT_1000_PT_385

         when PT_1000_PT_392 =>
            PT_1000_PT_392 : User_Config_Thermistors_PT_1000_PT_392;
            --  Key: PT_1000_PT_392

         when Custom_Steinhart_Hart_Model =>
            Custom_Steinhart_Hart_Model : User_Config_Thermistors_Custom_Steinhart_Hart;
            --  Key: Custom_Steinhart_Hart_Model

         when Custom_Callendar_Van_Dusen_Model =>
            Custom_Callendar_Van_Dusen_Model : User_Config_Thermistors_Custom_Callendar_Van_Dusen;
            --  Key: Custom_Callendar_Van_Dusen_Model
      end case;
   end record;

   type User_Config_Thermistor is record
      --  Description: This section contains the configuration for a single thermistor or RTD temperature sensor.

      Minimum_Temperature : Temperature := 0.0 * celsius;
      --  Key: Minimum temperature
      --  Description: This is a safety feature. If the thermistor reports a temperature below this value while it is
      --               being used by a heater, Prunt will assume there is a problem (e.g., the thermistor has become
      --               disconnected) and will perform an emergency stop. The temperature values shown in the user
      --               interface will also be capped at this minimum value.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Maximum_Temperature : Temperature := 0.0 * celsius;
      --  Key: Maximum temperature
      --  Description: This is a safety feature. If the thermistor reports a temperature above this value while it is
      --               being used by a heater, Prunt will assume there is a critical problem (e.g., a thermal runaway)
      --               and will perform an emergency stop. The temperature values shown in the user interface will also
      --               be capped at this maximum value.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Thermistor_Kind : User_Config_Thermistors_Variant;
      --  Key: Thermistor kind
   end record;

   type User_Config_Heaters_Disabled is record
      --  Description: The heater is disabled and cannot be used.
      null;
   end record;

   type User_Config_Heaters_PID is record
      --  Description: This option enables basic PID (Proportional-Integral-Derivative) control for the heater. The PID
      --               values can be automatically tuned using the M303 G-code command.

      Proportional_Scale : Dimensionless := 0.0;
      --  Key: Proportional scale
      --  Description: Scale for proportional part.
      --  Min: 0.0
      --  Max: 1.0E100

      Integral_Scale : Dimensionless := 0.0;
      --  Key: Integral scale
      --  Description: Scale for integral part.
      --  Min: 0.0
      --  Max: 1.0E100

      Derivative_Scale : Dimensionless := 0.0;
      --  Key: Derivative scale
      --  Description: Scale for derivative part.
      --  Min: 0.0
      --  Max: 1.0E100
   end record;

   type User_Config_Heaters_Bang_Bang is record
      --  Description: This option enables bang-bang control for the heater. Bang-bang control turns on when the
      --               temperature drops below the setpoint minus half the hysteresis value, and turns off when the
      --               temperature rises above the setpoint plus half the hysteresis value.

      Hysteresis : Temperature := 0.0 * celsius;
      --  Key: Hysteresis
      --  Description: This is the temperature range around the setpoint within which the heater will not change its
      --               state. A larger hysteresis will result in less frequent switching, but larger temperature
      --               swings.
      --  Min: 0.0
      --  Max: 1.0E100
   end record;

   type User_Config_Heaters_Control_Method_Kind is
     (Disabled,
      --  Key: Disabled
      PID,
      --  Key: PID
      Bang_Bang
      --  Key: Bang bang
     );

   type User_Config_Heaters_Control_Method (Kind : User_Config_Heaters_Control_Method_Kind := Disabled) is record
      --  Description: This section allows you to select the control method for this heater.

      case Kind is
         when Disabled =>
            Disabled : User_Config_Heaters_Disabled;
            --  Key: Disabled

         when PID =>
            PID : User_Config_Heaters_PID;
            --  Key: PID

         when Bang_Bang =>
            Bang_Bang : User_Config_Heaters_Bang_Bang;
            --  Key: Bang_Bang
      end case;
   end record;

   type User_Config_Heater is record
      --  Description: This section contains the configuration for a single heater.

      Thermistor : Thermistor_Name'Base := Thermistor_Name'Base'First;
      --  Key: Thermistor
      --  Description: Select the thermistor that is used to measure the temperature of this heater.

      Check_Maximum_Cumulative_Error : Temperature := 120.0 * celsius;
      --  Key: Check maximum cumulative error
      --  Description: This is a safety feature to detect a heater that is not heating up as expected. It's the maximum
      --               cumulative temperature error that is allowed before a failure is triggered.
      --  Min: 0.0
      --  Max: 1.0E100

      Check_Gain_Time : Time := 20.0 * s;
      --  Key: Check gain time
      --  Description: This is the time period over which the firmware will check for a temperature rise to ensure the
      --               heater is working correctly.
      --  Min: 0.0
      --  Max: 1.0E100

      Check_Minimum_Gain : Temperature := 2.0 * celsius;
      --  Key: Check minimum gain
      --  Description: This is the minimum temperature rise that must occur within the 'Check gain time' period to
      --               reset the cumulative error counter during heating.
      --  Min: 0.0
      --  Max: 1.0E100

      Check_Hysteresis : Temperature := 3.0 * celsius;
      --  Key: Check hysteresis
      --  Description: This is the temperature range above and below the setpoint where the heater is considered to be
      --               at the target temperature.
      --  Min: 0.0
      --  Max: 1.0E100

      Control_Method : User_Config_Heaters_Control_Method;
      --  Key: Control method
   end record;

   type User_Config_Laser is record
      --  Description: This section contains the configuration for a single laser.

      Modulate_With_Velocity : Boolean := True;
      --  Key: Modulate with velocity
      --  Description: When enabled, the laser's duty cycle will be linearly adjusted based on the toolhead's velocity
      --               compared to the target velocity. This ensures that the amount of energy applied to the material
      --               per unit of distance remains constant, even during acceleration and deceleration. When the
      --               toolhead is stationary during a dwell command, the laser power will be set to the programmed
      --               power level even though the velocity is zero.
   end record;

   type User_Config_Homing_Disabled is record
      --  Description: Homing is not yet configured for this axis. Movement on this axis will be disabled until a
      --               homing method is selected and configured.
      null;
   end record;

   type User_Config_Homing_Set_To_Value is record
      --  Description: This homing method doesn't involve any physical movement. When the homing procedure is
      --               initiated, it simply sets the current position of the axis to the specified value. This should
      --               generally only used for the E axis.

      Position : Length := 0.0 * mm;
      --  Key: Position
      --  Description: The position to which the axis will be set when homed.
      --  Min: -1.0E100
      --  Max: 1.0E100
   end record;

   type User_Config_Homing_Use_Input_Switch is record
      --  Description: This is the traditional homing method that uses a physical switch (endstop) to determine the end
      --               of the axis. The homing procedure consists of a fast move towards the switch, a back-off move,
      --               and then a slower move to accurately record the switch position.

      Switch : Input_Switch_Name'Base := Input_Switch_Name'Base'First;
      --  Key: Switch
      --  Description: Select the input switch to be used for homing this axis.

      Move_Towards_Negative_Infinity : Boolean := True;
      --  Key: Move towards negative infinity
      --  Description: This setting determines the direction of the homing move. If enabled, the axis will move towards
      --               the minimum position. If disabled, it will move towards the maximum position.

      First_Move_Distance : Length := 0.1 * mm;
      --  Key: First move distance
      --  Description: This is the distance the axis is allowed to travel past the switch trigger point during the
      --               initial fast homing move.
      --  Min: 0.000001
      --  Max: 1.0E100

      Back_Off_Move_Distance : Length := 2.0 * mm;
      --  Key: Back off move distance
      --  Description: After the switch is triggered for the first time, the axis will back off by this distance before
      --               starting the second, slower homing move.
      --  Min: 0.0
      --  Max: 1.0E100

      Second_Move_Distance : Length := 0.1 * mm;
      --  Key: Second move distance
      --  Description: This is the distance the axis is allowed to travel past the switch trigger point during the
      --               second, slower homing move.
      --  Min: 0.000001
      --  Max: 1.0E100

      Switch_Position : Length := 0.0 * mm;
      --  Key: Switch position
      --  Description: This is the known position of the switch on the axis. This value is allowed to be outside of the
      --               machine's travel limits.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Move_To_After : Length := 0.0 * mm;
      --  Key: Move to after
      --  Description: Immediately after the homing procedure for this axis (not all axes) is complete, the axis will
      --               move to this position. This position must be within the machine's travel limits.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Velocity_Limit : Velocity := 1.0E100 * mm / s;
      --  Key: Velocity limit
      --  Description: This sets a velocity limit specifically for the homing moves on this axis. This does not allow
      --               the movement to exceed any regular velocity limits.
      --  Min: 0.000001
      --  Max: 1.0E100
   end record;

   type User_Config_Homing_Use_StallGuard2 is record
      --  Description: This homing method uses Trinamic's StallGuard2 feature for sensorless homing.

      Motor : Stepper_Name'Base := Stepper_Name'Base'First;
      --  Key: Motor
      --  Description: Select the motor that will be used for stall detection. Must be attached to the given axis, or
      --               the A/B axis for X/Y in Core XY.
      --  Override_String_Set: StallGuard2_Stepper_Name_Strings

      Move_Towards_Negative_Infinity : Boolean := True;
      --  Key: Move towards negative infinity
      --  Description: This setting determines the direction of the homing move. If enabled, the axis will move towards
      --               the minimum position. If disabled, it will move towards the maximum position.

      Threshold : Integer := -64;
      --  Key: Threshold
      --  Description: Threshold for StallGuard2. Higher values are less sensitive.
      --  Min: -64
      --  Max: 63

      Enable_Filter : Boolean := False;
      --  Key: Enable filter
      --  Description: Enabling this filter can provide more precise measurements, but it reduces the measurement
      --               frequency to once every four full steps.

      Stop_Position : Length := 0.0 * mm;
      --  Key: Stop position
      --  Description: This is the position where the axis will hit the hard endstop. This value is allowed to be
      --               outside of the machine's travel limits.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Move_To_After : Length := 0.0 * mm;
      --  Key: Move to after
      --  Description: Immediately after the homing procedure for this axis (not all axes) is complete, the axis will
      --               move to this position. This position must be within the machine's travel limits.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Velocity_Limit : Velocity := 50.0 * mm / s;
      --  Key: Velocity limit
      --  Description: This sets a velocity limit specifically for the homing moves on this axis. This does not allow
      --               the movement to exceed any regular velocity limits.
      --  Min: 0.000001
      --  Max: 50.0

      Acceleration_Limit : Acceleration := 1_000.0 * mm / s**2;
      --  Key: Acceleration limit
      --  Description: This sets an acceleration limit specifically for the homing moves on this axis.
      --  Min: 0.000001
      --  Max: 1.0E100
   end record;

   type User_Config_Homing_Use_StallGuard4 is record
      --  Description: This homing method uses Trinamic's StallGuard4 feature for sensorless homing.

      Motor : Stepper_Name'Base := Stepper_Name'Base'First;
      --  Key: Motor
      --  Description: Select the motor that will be used for stall detection.
      --  Override_String_Set: StallGuard4_Stepper_Name_Strings

      Move_Towards_Negative_Infinity : Boolean := True;
      --  Key: Move towards negative infinity
      --  Description: This setting determines the direction of the homing move.

      Threshold : Integer := 255;
      --  Key: Threshold
      --  Description: Threshold for StallGuard4. Higher values are more sensitive.
      --  Min: 0
      --  Max: 255

      Enable_Filter : Boolean := False;
      --  Key: Enable filter
      --  Description: Enabling this filter can provide more precise measurements, but it reduces the measurement
      --               frequency to once every four full steps.

      Stop_Position : Length := 0.0 * mm;
      --  Key: Stop position
      --  Description: This is the position where the axis will hit the hard endstop. This value is allowed to be
      --               outside of the machine's travel limits.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Move_To_After : Length := 0.0 * mm;
      --  Key: Move to after
      --  Description: Immediately after the homing procedure for this axis (not all axes) is complete, the axis will
      --               move to this position. This position must be within the machine's travel limits.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Velocity_Limit : Velocity := 50.0 * mm / s;
      --  Key: Velocity limit
      --  Description: This sets a velocity limit specifically for the homing moves on this axis. This does not allow
      --               the movement to exceed any regular velocity limits.
      --  Min: 0.000001
      --  Max: 1.0E100

      Acceleration_Limit : Acceleration := 1_000.0 * mm / s**2;
      --  Key: Acceleration limit
      --  Description: This sets an acceleration limit specifically for the homing moves on this axis.
      --  Min: 0.000001
      --  Max: 1.0E100
   end record;

   type User_Config_Homing_Method_Kind is
     (Disabled,
      --  Key: Disabled
      Set_To_Value,
      --  Key: Set to value
      Use_Input_Switch,
      --  Key: Use input switch
      Use_StallGuard2,
      --  Key: Use StallGuard2
      Use_StallGuard4
      --  Key: Use StallGuard4
     );

   type User_Config_Homing_Method (Kind : User_Config_Homing_Method_Kind := Disabled) is record
      --  Description: This section allows you to select the homing method for this axis. Homing is the process of
      --               moving the axes to a known, fixed position so that the machine knows the location of the
      --               toolhead.

      case Kind is
         when Disabled =>
            Disabled : User_Config_Homing_Disabled;
            --  Key: Disabled

         when Set_To_Value =>
            Set_To_Value : User_Config_Homing_Set_To_Value;
            --  Key: Set_To_Value

         when Use_Input_Switch =>
            Use_Input_Switch : User_Config_Homing_Use_Input_Switch;
            --  Key: Use_Input_Switch
            --  Include_If: Has_Input_Switches

         when Use_StallGuard2 =>
            Use_StallGuard2 : User_Config_Homing_Use_StallGuard2;
            --  Key: Use_StallGuard2
            --  Include_If: Has_StallGuard2

         when Use_StallGuard4 =>
            Use_StallGuard4 : User_Config_Homing_Use_StallGuard4;
            --  Key: Use_StallGuard4
            --  Include_If: Has_StallGuard4
      end case;
   end record;

   type User_Config_Homing_Prereq_No_Requirement is record
      --  Description: There are no requirements for this axis during homing.
      null;
   end record;

   type User_Config_Homing_Prereq_Must_Be_Homed is record
      --  Description: This axis must be homed prior to the parent axis, but the position does not matter.
      null;
   end record;

   type User_Config_Homing_Prereq_Must_Be_At_Position is record
      --  Description: This axis must be homed prior to the parent axis and it must be at a specified position.

      Position : Length := 0.0 * mm;
      --  Key: Position
      --  Description: The position to move this axis to before homing the parent axis.
      --  Min: -1.0E100
      --  Max: 1.0E100
   end record;

   type User_Config_Homing_Prereq_Kind is
     (No_Requirement,
      --  Key: No requirement
      Must_Be_Homed,
      --  Key: Must be homed
      Must_Be_At_Position
      --  Key: Must be at position
     );

   type User_Config_Homing_Prereq (Kind : User_Config_Homing_Prereq_Kind := No_Requirement) is record
      --  Description: This setting defines the required state of the selected axis before the parent axis can be
      --               homed.

      case Kind is
         when No_Requirement =>
            No_Requirement : User_Config_Homing_Prereq_No_Requirement;
            --  Key: No_Requirement

         when Must_Be_Homed =>
            Must_Be_Homed : User_Config_Homing_Prereq_Must_Be_Homed;
            --  Key: Must_Be_Homed

         when Must_Be_At_Position =>
            Must_Be_At_Position : User_Config_Homing_Prereq_Must_Be_At_Position;
            --  Key: Must_Be_At_Position
      end case;
   end record;

   type User_Config_Homing_Prereq_Array is array (Axis_Name) of User_Config_Homing_Prereq;
   --  Description: This section allows you to define the prerequisites for homing this axis. For example, you might
   --               require the Z-axis to be at a certain height before homing the X and Y axes to prevent the nozzle
   --               from crashing into the bed.

   type User_Config_Homing is record
      --  Description: This section contains the homing procedure configuration for a single axis.

      Homing_Method : User_Config_Homing_Method;
      --  Key: Homing method

      Prerequisites : User_Config_Homing_Prereq_Array := (others => <>);
      --  Key: Prerequisites
   end record;

   type User_Config_Fans_Dynamic is record
      --  Description: This mode allows the fan's PWM signal duty cycle to be controlled dynamically while the printer
      --               is running using the M106 and M107 G-code commands.

      Disable_Below : PWM_Scale := 0.0;
      --  Key: Disable below
      --  Description: If the requested duty cycle is below this value, the fan will be turned off completely. This can
      --               be useful for fans that don't spin reliably at very low duty cycles.
      --  Min: 0.0
      --  Max: 1.0

      Maximum_Duty_Cycle : PWM_Scale := 1.0;
      --  Key: Maximum duty cycle
      --  Description: This sets the PWM duty cycle that corresponds to 100% fan power in G-code. For example, if you
      --               set this to 0.8, a request for 100% fan power will result in an 80% duty cycle and 50% power
      --               will result in 40% duty cycle.
      --  Min: 0.0
      --  Max: 1.0
   end record;

   type User_Config_Fans_Always_On is record
      --  Description: In this mode, the fan will always be on at a fixed duty cycle whenever Prunt is connected to the
      --               machine. This is useful for hotend fans or electronics cooling fans that need to run
      --               continuously.

      Duty_Cycle : PWM_Scale := 1.0;
      --  Key: Duty cycle
      --  Description: The fixed PWM duty cycle for the fan.
      --  Min: 0.0
      --  Max: 1.0
   end record;

   type User_Config_Fans_Control_Method_Kind is
     (Dynamic_Duty_Cycle,
      --  Key: Dynamic duty cycle
      Always_On
      --  Key: Always on
     );

   type User_Config_Fans_Control_Method (Kind : User_Config_Fans_Control_Method_Kind := Always_On) is record
      --  Description: This section allows you to select the control method for this fan.

      case Kind is
         when Dynamic_Duty_Cycle =>
            Dynamic_Duty_Cycle : User_Config_Fans_Dynamic;
            --  Key: Dynamic_Duty_Cycle

         when Always_On =>
            Always_On : User_Config_Fans_Always_On;
            --  Key: Always_On
      end case;
   end record;

   type User_Config_Fan_Fixed_Switching is record
      --  Description: This section contains the configuration for a single fan.

      Invert_PWM_Output : Boolean := False;
      --  Key: Invert PWM output
      --  Description: Invert the PWM signal. This may be necessary depending on how your fan is wired.

      PWM_Frequency : Frequency := 30.0 * hertz;
      --  Key: PWM frequency
      --  Description: This sets the frequency of the PWM signal used to control the fan's speed. For standard 2-wire
      --               fans, a low frequency like 30Hz is usually best. For 4-wire (PWM-controlled) fans, a much higher
      --               frequency, such as 25000Hz, is typically required.
      --  Min: 1.0
      --  Max: 50000.0

      Control_Method : User_Config_Fans_Control_Method;
      --  Key: Control method
   end record;

   type User_Config_Fan_Low_High_Side_Switching is record
      --  Description: This section contains the configuration for a single fan.

      Invert_PWM_Output : Boolean := False;
      --  Key: Invert PWM output
      --  Description: Invert the PWM signal.

      PWM_Frequency : Frequency := 30.0 * hertz;
      --  Key: PWM frequency
      --  Description: The frequency of the PWM signal. 30Hz is usually best for 2-wire fans, while 25000Hz is
      --               typically required for 4-wire fans.
      --  Min: 1.0
      --  Max: 50000.0

      Control_Method : User_Config_Fans_Control_Method;
      --  Key: Control method

      Use_High_Side_Switching : Boolean := False;
      --  Key: Use high-side switching
      --  Description: When enabled, the fan's power pin will be toggled to control its speed, instead of the PWM pin.
      --               This can be useful for 3-wire fans that have a tachometer signal, as it allows the tachometer to
      --               function correctly as long as the fan's ground pin to be connected to the connector's ground
      --               pin, rather than the PWM pin.
   end record;

   type User_Config_Fan (Fixed_Kind : Fan_Hardware_Kind := Fixed_Switching_Kind) is record
      --  Description: This section contains the configuration for a single fan.

      case Fixed_Kind is
         when Fixed_Switching_Kind =>
            Fixed_Switching : User_Config_Fan_Fixed_Switching := (others => <>);
            --  Key: Fixed_Switching_Kind

         when Low_Or_High_Side_Switching_Kind =>
            Low_Or_High_Side_Switching : User_Config_Fan_Low_High_Side_Switching := (others => <>);
            --  Key: Low_Or_High_Side_Switching_Kind
      end case;
   end record;

   type User_Config_Input_Shaping_No_Shaper is record
      --  Description: No input shaping will be applied to this axis.
      null;
   end record;

   type User_Config_Input_Shaping_ZV is record
      --  Description: Zero vibration shaper.

      Freq : Frequency := 1.0 * hertz;
      --  Key: Frequency
      --  Description: This parameter is not copied between different shapers.
      --  Min: 1.0E-10
      --  Max: 1.0E100

      Damping_Ratio : Dimensionless := 0.1;
      --  Key: Damping ratio
      --  Description: This parameter is not copied between different shapers.
      --  Min: 0.001
      --  Max: 0.999

      Number_Of_Derivatives : Integer := 0;
      --  Key: Number of derivatives
      --  Description: 0 = ZV, 1 = ZVD, 2 = ZVDD, 3 = ZVDDD.
      --  Min: 0
      --  Max: 3
   end record;

   type User_Config_Input_Shaping_EI is record
      --  Description: Extra insensitive shaper.

      Freq : Frequency := 1.0 * hertz;
      --  Key: Frequency
      --  Description: This parameter is not copied between different shapers.
      --  Min: 1.0E-10
      --  Max: 1.0E100

      Damping_Ratio : Dimensionless := 0.1;
      --  Key: Damping ratio
      --  Description: This parameter is not copied between different shapers.
      --  Min: 0.001
      --  Max: 0.999

      Residual_Vibration_Level : Dimensionless := 0.05;
      --  Key: Residual vibration level
      --  Description: This is hard-coded to 0.05 in other 3D printer motion controllers. Usually it does not need to
      --               be changed.
      --  Min: 0.001
      --  Max: 0.999

      Number_Of_Humps : Integer := 1;
      --  Key: Number of humps
      --  Min: 1
      --  Max: 3
   end record;

   type User_Config_Input_Shaping_Pressure_Advance is record
      --  Description: Pressure advance shaper with optional smoothing.

      Pressure_Advance_Time : Time := 0.0 * s;
      --  Key: Pressure advance time
      --  Description: The E axis velocity is multiplied by this value and then added to the axis position.
      --  Min: -1.0E100
      --  Max: 1.0E100

      Pressure_Advance_Smooth_Time : Time := 0.0 * s;
      --  Key: Pressure advance smooth time
      --  Description: This applies a triangular smoothing window of the specified length, either to the added part or
      --               to the entire output, as set by 'Apply smoothing to added part only'.
      --  Min: 0.0
      --  Max: 0.2

      Smooth_Added_Part_Only : Boolean := False;
      --  Key: Apply smoothing to added part only
      --  Description: If set, apply smoothing to only the part added by pressure advance, otherwise smoothing the
      --               entire output.

      Smoothing_Levels : Integer := 2;
      --  Key: Smoothing levels
      --  Description: Number of cascaded moving average levels to apply. 2 is equivalent to Klipper's smoothing.
      --  Min: 1
      --  Max: 10
   end record;

   type User_Config_Input_Shaping_Method_Kind is
     (No_Shaper,
      --  Key: No shaper
      ZV,
      --  Key: Zero vibration (ZV/ZVD/ZVDD/etc.)
      EI,
      --  Key: Extra insensitive (EI/2HEI/3HEI)
      Pressure_Advance
      --  Key: Pressure advance
     );

   type User_Config_Input_Shaping (Kind : User_Config_Input_Shaping_Method_Kind := No_Shaper) is record
      --  Description: Input shaping method used for this axis.

      case Kind is
         when No_Shaper =>
            No_Shaper : User_Config_Input_Shaping_No_Shaper;
            --  Key: No_Shaper

         when ZV =>
            ZV : User_Config_Input_Shaping_ZV;
            --  Key: ZV

         when EI =>
            EI : User_Config_Input_Shaping_EI;
            --  Key: EI

         when Pressure_Advance =>
            Pressure_Advance : User_Config_Input_Shaping_Pressure_Advance;
            --  Key: Pressure_Advance
      end case;
   end record;

   type User_Config_G_Code_Assignments is record
      --  Description: Assign heaters, fans, and lasers to their corresponding G-code commands.

      Hotend_Heater : Heater_Name'Base := Heater_Name'Base'First;
      --  Key: Hotend heater
      --  Description: Heater to use for the hotend.
      --  Include_If: Has_Heaters

      Bed_Heater : Heater_Name'Base := Heater_Name'Base'First;
      --  Key: Bed heater
      --  Description: Heater to use for the bed.
      --  Include_If: Has_Heaters

      Default_Fan : Fan_Name'Base := Fan_Name'Base'First;
      --  Key: Default fan
      --  Description: Fan to control when no P parameter is used for M106/M107.
      --  Include_If: Has_Fans

      Default_Laser : Laser_Name'Base := Laser_Name'Base'First;
      --  Key: Default laser
      --  Description: Laser to control when no P parameter is used for M3/M5.
      --  Include_If: Has_Lasers
   end record;

   type User_Config_Thermistor_Array is array (Thermistor_Name) of User_Config_Thermistor;
   type User_Config_Stepper_Array is array (Stepper_Name) of User_Config_Stepper;
   type User_Config_Input_Switch_Array is array (Input_Switch_Name) of User_Config_Input_Switch;
   type User_Config_Heater_Array is array (Heater_Name) of User_Config_Heater;
   type User_Config_Laser_Array is array (Laser_Name) of User_Config_Laser;
   type User_Config_Homing_Array is array (Axis_Name) of User_Config_Homing;
   type User_Config_Fan_Array is array (Fan_Name) of User_Config_Fan;
   type User_Config_Input_Shaping_Array is array (Axis_Name) of User_Config_Input_Shaping;

   type User_Config is record
      Prunt : User_Config_Prunt := (others => <>);
      --  Key: Prunt

      Steppers : User_Config_Stepper_Array := (others => <>);
      --  Key: Steppers
      --  Fixed_Kind: Stepper_Hardware (Stepper_Name'Value (Index_???)).Kind
      --  Description: This section contains the configuration for all stepper motor drivers.
      --  Tabbed: True

      Kinematics : User_Config_Kinematics := (others => <>);
      --  Key: Kinematics

      Input_Switches : User_Config_Input_Switch_Array := (others => <>);
      --  Key: Input switches
      --  Include_If: Has_Input_Switches
      --  Fixed_Kind: (if Input_Switch_Visible_To_User (Input_Switch_Name'Value (Index_???)) then Visible else Not_Visible)
      --  Description: This section contains the configuration for all input switches.
      --  Tabbed: True

      Thermistors : User_Config_Thermistor_Array := (others => <>);
      --  Key: Thermistors
      --  Include_If: Has_Thermistors
      --  Description: This section contains the configuration for all thermistors and RTD temperature sensors.
      --  Tabbed: True

      Heaters : User_Config_Heater_Array := (others => <>);
      --  Key: Heaters
      --  Include_If: (Has_Heaters and Has_Thermistors)
      --  Description: This section contains the configuration for all heaters.
      --  Tabbed: True

      Lasers : User_Config_Laser_Array := (others => <>);
      --  Key: Lasers
      --  Include_If: Has_Heaters
      --  Description: This section contains the configuration for all lasers.
      --  Tabbed: True

      Homing : User_Config_Homing_Array := (others => <>);
      --  Key: Homing
      --  Description: This section contains the homing procedure configuration for each axis.
      --  Tabbed: True

      Fans : User_Config_Fan_Array := (others => <>);
      --  Key: Fans
      --  Include_If: Has_Fans
      --  Fixed_Kind: Fan_Hardware (Fan_Name'Value (Index_???)).Kind
      --  Description: This section contains the configuration for all fans.
      --  Tabbed: True

      Input_Shaping : User_Config_Input_Shaping_Array := (others => <>);
      --  Key: Input shaping
      --  Description: This section contains the input shaping configuration for each axis.
      --  Tabbed: True

      G_Code_Assignments : User_Config_G_Code_Assignments := (others => <>);
      --  Key: G-code assignments
      --  Description: This section assigns heaters, fans, and lasers to their corresponding G-code commands.
   end record;

   --!PRUNT END USER CONFIG DECLARATIONS

end Prunt.Config;
