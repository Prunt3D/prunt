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

package body Prunt.Config is

   pragma Unsuppress (All_Checks);

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

   function Get (Val : JSON_Value; Field : UTF8_String) return TMC_Boolean is
   begin
      return TMC_Types.TMC_Boolean (Boolean'(Get (Val, Field)));
   end Get;

   function Trim (S : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Side => Ada.Strings.Both);
   end Trim;

   function Build_Schema return Property_Maps.Map is
      function Boolean (Description : String; Default : Boolean) return Property_Parameters_Access is
      begin
         return
           new Property_Parameters'
             (Kind => Boolean_Kind, Description => To_Unbounded_String (Description), Boolean_Default => Default);
      end Boolean;

      function Sequence (Description : String; Children : Property_Maps.Map) return Property_Parameters_Access is
      begin
         return
           new Property_Parameters'
             (Kind              => Sequence_Kind,
              Description       => To_Unbounded_String (Description),
              Sequence_Children => Children,
              Sequence_Tabbed   => False);
      end Sequence;

      function Tabbed_Sequence (Description : String; Children : Property_Maps.Map) return Property_Parameters_Access
      is
      begin
         return
           new Property_Parameters'
             (Kind              => Sequence_Kind,
              Description       => To_Unbounded_String (Description),
              Sequence_Children => Children,
              Sequence_Tabbed   => True);
      end Tabbed_Sequence;

      function Variant
        (Description : String; Default : String; Children : Property_Maps.Map) return Property_Parameters_Access is
      begin
         return
           new Property_Parameters'
             (Kind             => Variant_Kind,
              Description      => To_Unbounded_String (Description),
              Variant_Children => Children,
              Variant_Default  => To_Unbounded_String (Default));
      end Variant;

      function Integer
        (Description : String; Default, Min, Max : Long_Long_Integer; Unit : String) return Property_Parameters_Access
      is
      begin
         return
           new Property_Parameters'
             (Kind            => Integer_Kind,
              Description     => To_Unbounded_String (Description),
              Integer_Min     => Min,
              Integer_Max     => Max,
              Integer_Unit    => To_Unbounded_String (Unit),
              Integer_Default => Default);
      end Integer;

      function Float
        (Description : String; Default, Min, Max : Long_Float; Unit : String) return Property_Parameters_Access is
      begin
         return
           new Property_Parameters'
             (Kind          => Float_Kind,
              Description   => To_Unbounded_String (Description),
              Float_Min     => Min,
              Float_Max     => Max,
              Float_Unit    => To_Unbounded_String (Unit),
              Float_Default => Default);
      end Float;

      function Float_Ratio
        (Description : String; Default_Numerator, Default_Denominator, Min, Max : Long_Float)
         return Property_Parameters_Access is
      begin
         return
           new Property_Parameters'
             (Kind                            => Float_Ratio_Kind,
              Description                     => To_Unbounded_String (Description),
              Float_Ratio_Min                 => Min,
              Float_Ratio_Max                 => Max,
              Float_Ratio_Default_Numerator   => Default_Numerator,
              Float_Ratio_Default_Denominator => Default_Denominator);
      end Float_Ratio;

      function Discrete
        (Description : String; Default : String; Options : Discrete_String_Sets.Set) return Property_Parameters_Access
      is
      begin
         return
           new Property_Parameters'
             (Kind             => Discrete_Kind,
              Description      => To_Unbounded_String (Description),
              Discrete_Options => Options,
              Discrete_Default => To_Unbounded_String (Default));
      end Discrete;

      function Sequence_Over_Axes
        (Description : String; Property : Property_Parameters_Access) return Property_Parameters_Access
      is
         Children : Property_Maps.Map := [];
      begin
         for A in Axis_Name loop
            Property_Maps.Insert (Children, A'Image, Property);
         end loop;

         return
           new Property_Parameters'
             (Kind              => Sequence_Kind,
              Description       => To_Unbounded_String (Description),
              Sequence_Children => Children,
              Sequence_Tabbed   => False);
      end Sequence_Over_Axes;

      function Sequence_Over_Steppers
        (Description : String; Property : Property_Parameters_Access) return Property_Parameters_Access
      is
         Children : Property_Maps.Map := [];
      begin
         for S in Stepper_Name loop
            Property_Maps.Insert (Children, S'Image, Property);
         end loop;

         return
           new Property_Parameters'
             (Kind              => Sequence_Kind,
              Description       => To_Unbounded_String (Description),
              Sequence_Children => Children,
              Sequence_Tabbed   => False);
      end Sequence_Over_Steppers;

      Input_Switch_Name_Strings        : constant Discrete_String_Sets.Set :=
        Discrete_String_Sets.Difference
          ([for I in Input_Switch_Name => (if Input_Switch_Visible_To_User (I) then I'Image else "")], [""]);
      StallGuard2_Stepper_Name_Strings : constant Discrete_String_Sets.Set :=
        Discrete_String_Sets.Difference
          ([for S in Stepper_Name => (if Stepper_Hardware (S).Kind in TMC2240_UART_Kind then S'Image else "")], [""]);
      StallGuard4_Stepper_Name_Strings : constant Discrete_String_Sets.Set :=
        Discrete_String_Sets.Difference
          ([for S in Stepper_Name => (if Stepper_Hardware (S).Kind in TMC2240_UART_Kind then S'Image else "")], [""]);
      Thermistor_Name_Strings          : constant Discrete_String_Sets.Set := [for T in Thermistor_Name => T'Image];
      Heater_Name_Strings              : constant Discrete_String_Sets.Set := [for H in Heater_Name => H'Image];
      Fan_Name_Strings                 : constant Discrete_String_Sets.Set := [for F in Fan_Name => F'Image];

      --!pp off
      Stepper_Distance_Variant_Children : constant Property_Maps.Map :=
        ["Direct entry" =>
          Sequence
            ("Directly enter the resulting movement from a step.",
             ["Distance per step" =>
               Float
                 ("Distance moved by the attached motor for each step.",
                  Default => 1.0E100,
                  Min     => 1.0E-100,
                  Max     => 1.0E100,
                  Unit    => "mm"),
               "Reverse direction" =>
                 Boolean
                   ("Reverse the rotation direction for this motor.",
                    Default => False)]),
         "Lead screw" =>
           Sequence
             ("Calculate the distance per step based on a lead screw.",
              ["Lead" =>
                Float
                  ("Distance moved by the nut after one full rotation. This is different from the " &
                     "pitch if your screw has multiple starts.",
                   Default => 1.0E100,
                   Min     => -1.0E100,
                   Max     => 1.0E100,
                   Unit    => "mm"),
               "Reverse direction" =>
                 Boolean
                   ("Reverse the rotation direction for this motor.",
                    Default => False),
               "Gear ratio" =>
                 Float_Ratio
                   ("Ratio of gears where the A is on the lead screw and B is on the motor in A:B. " &
                      "Set to 1:1 for direct drive.",
                    Default_Numerator   => 1.0,
                    Default_Denominator => 1.0,
                    Min                 => 1.0E-100,
                    Max                 => 1.0E100),
               "Full steps per rotation" =>
                 Float
                   ("Number of steps for the motor shaft to make one full rotation before any gearing. " &
                      "This is 200 for most stepper motors.",
                    Default => 200.0,
                    Min     => 1.0E-100,
                    Max     => 1.0E100,
                    Unit    => "")]),
         "Gear with circumference" =>
           Sequence
             ("Calculate the distance per step based on a gear where the circumference is known.",
              ["Circumference" =>
                Float
                  ("Circumference of the main gear driving the main belts.",
                   Default => 1.0E100,
                   Min     => -1.0E100,
                   Max     => 1.0E100,
                   Unit    => "mm"),
               "Reverse direction" =>
                 Boolean
                   ("Reverse the rotation direction for this motor.",
                    Default => False),
               "Gear ratio" =>
                 Float_Ratio
                   ("Ratio of gears where the A is on the main gear and B is on the motor in A:B. " &
                      "Set to 1:1 for direct drive.",
                    Default_Numerator   => 1.0,
                    Default_Denominator => 1.0,
                    Min                 => 1.0E-100,
                    Max                 => 1.0E100),
               "Full steps per rotation" =>
                 Float
                   ("Number of steps for the motor shaft to make one full rotation before any gearing. " &
                      "This is 200 for most stepper motors.",
                    Default => 200.0,
                    Min     => 1.0E-100,
                    Max     => 1.0E100,
                    Unit    => "")]),
         "Gear with tooth count and pitch" =>
           Sequence
             ("Calculate the distance per step based on a gear where the tooth count and pitch is known.",
              ["Tooth count" =>
                Float
                  ("Tooth count of the main gear driving the main belts.",
                   Default => 1.0E100,
                   Min     => 1.0E-100,
                   Max     => 1.0E100,
                   Unit    => ""),
               "Tooth pitch" =>
                 Float
                  ("Tooth pitch of the main gear driving the main belts.",
                   Default => 1.0E100,
                   Min     => -1.0E100,
                   Max     => 1.0E100,
                   Unit    => "mm"),
               "Reverse direction" =>
                 Boolean
                   ("Reverse the rotation direction for this motor.",
                    Default => False),
               "Gear ratio" =>
                 Float_Ratio
                   ("Ratio of gears where the A is on the main gear and B is on the motor in A:B. " &
                      "Set to 1:1 for direct drive.",
                    Default_Numerator   => 1.0,
                    Default_Denominator => 1.0,
                    Min                 => 1.0E-100,
                    Max                 => 1.0E100),
               "Full steps per rotation" =>
                 Float
                   ("Number of steps for the motor shaft to make one full rotation before any gearing. " &
                      "This is 200 for most stepper motors.",
                    Default => 200.0,
                    Min     => 1.0E-100,
                    Max     => 1.0E100,
                    Unit    => "")])];

      Basic_Stepper_Sequence : constant Property_Parameters_Access :=
        Sequence
          ("Basic stepper driver settings.",
           ["Enabled" =>
              Boolean
                ("Enable this stepper driver, allowing it to be attached to an axis.",
                 Default => False),
            "Distance per step" =>
              Variant
                ("Distance moved by the attached motor for each step signal. " &
                   "This does not take in to account any microstepping settings in the driver.",
                 "Direct entry",
                 Stepper_Distance_Variant_Children)]);

      --  TODO: Add StallGuard and CoolStep.
      TMC2240_Stepper_Sequence : constant Property_Parameters_Access :=
        Sequence
          ("TMC2240 stepper driver settings.",
           ["Enabled" =>
              Boolean
                ("Enable this stepper driver, allowing it to be attached to an axis.",
                 Default => False),
            "Distance per step" =>
              Variant
                ("Distance moved by the attached motor for each step. Microstepping is automatically accounted for.",
                 "Direct entry",
                 Stepper_Distance_Variant_Children),
            "Run current" =>
              Float
                ("Peak current limit for each motor coil. This parameter will be used to set CURRENT_RANGE to the " &
                   "smallest suitable range before setting GLOBALSCALER.",
                 Default => 0.125,
                 Min     => 0.125,
                 Max     => 3.0,
                 Unit    => "A"),
            "SLOPE_CONTROL" =>
              Discrete
                ("Output slew rate. 400V/uS is usually a good setting. 800V/uS provides a minimal decrease in power " &
                   "dissipation.",
                 Default => "SLOPE_400V_PER_US",
                 --  [for X in TMC_Types.TMC2240.Slope_Control_Type => X'Image]),
                 --  TODO: GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=118082
                 Options => ["SLOPE_100V_PER_US", "SLOPE_200V_PER_US", "SLOPE_400V_PER_US", "SLOPE_800V_PER_US"]),
            "IHOLD" =>
              Float
                ("Standstill current scale. 0 = 0% of set current, 1 = 100% of set current. Resolution of 1/32.",
                 Default => 1.0,
                 Min     => 0.031_25,
                 Max     => 1.0,
                 Unit    => ""),
            "IRUN" =>
              Float
                ("Run current scale. 0 = 0% of set current, 1 = 100% of set current. Resolution of 1/32.",
                 Default => 1.0,
                 Min     => 0.031_25,
                 Max     => 1.0,
                 Unit    => ""),
            "IRUN during homing" =>
              Float
                ("Run current scale during homing moves. This will be applied while any axis is homing. "
                   & "0 = 0% of set current, 1 = 100% of set current. Resolution of 1/32.",
                 Default => 1.0,
                 Min     => 0.031_25,
                 Max     => 1.0,
                 Unit    => ""),
            "IHOLDDELAY" =>
              Float
                ("Slew time for motor power down after standstill detected. Resolution of 21ms.",
                 --  Actually 80ns*2^18 = 20.97152ms, but 21ms is close enough given the internal oscillator is +/- 5%.
                 Default => 315.0,
                 Min     => 0.0,
                 Max     => 315.0,
                 Unit    => "ms"),
            "IRUNDELAY" =>
              --  TODO: This description and allowed values assumes the internal oscillator is used.
              Float
                ("Slew time for motor power up after end of standstill. Resolution is 0.041ms",
                 --  Actually 40.96us, but 21us is close enough given the internal oscillator is +/- 5%.
                 Default => 0.0,
                 Min     => 0.0,
                 Max     => 0.615,
                 Unit    => "ms"),
            "TPOWERDOWN" =>
              --  TODO: This description and allowed values assumes the internal oscillator is used.
              Float
                ("Delay before motor power down after standstill detected. A minimum value of 42ms is required for " &
                   "automatic StealthChop2 tuning. Resolution of 21ms.",
                 --  Actually 80ns*2^18 = 20.97152ms, but 21ms is close enough given the internal oscillator is +/- 5%.
                 Default => 5_355.0,
                 Min     => 0.0,
                 Max     => 5_355.0,
                 Unit    => "ms"),
            "THIGH" =>
              --  TODO: This description and allowed values assumes the internal oscillator is used.
              Float
                ("Lower velocity limit for high velocity mode and upper velocity limit for CoolStep/StealthChop2 " &
                   "mode (if enabled).",
                 Default => 1.0E100,
                 Min     => 0.0,
                 Max     => 1.0E100,
                 Unit    => "mm/s"),
            "TOFF" =>
              --  TODO: This description assumes the internal oscillator is used.
              Discrete
                ("Slow decay (i.e. off time) duration in TMC clock cycles (typically 80ns). 120 is usually a good " &
                   "setting when combined with TBL = 36 for a theoretical maximum chopper frequency of 40kHz.",
                 --  [for X in TMC_Types.TMC2240.TOFF_Type
                 --     range TMC_Types.TMC2240.Off_56 .. TMC_Types.TMC2240.Off_504 => X'Image]),
                 --  TODO: GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=118082
                 Default => "120",
                 Options => ["56", "88", "120", "152", "184", "216", "248", "280", "312", "344", "376", "408", "440",
                  "472", "504"]),
            "TBL" =>
              --  TODO: This description and allowed values assumes the internal oscillator is used.
              Discrete
                ("Comparator blank time measured in TMC clock cycles (typically 80ns). 36 is usually a good " &
                   "setting when combined with TOFF = 120 for a theoretical maximum chopper frequency of 40kHz.",
                 --  [for X in TMC_Types.TMC2240.TBL_Type
                 --     range TMC_Types.TMC2240.Blank_24 .. TMC_Types.TMC2240.Blank_54 =>
                 --     X'Image]),
                 --  TODO: GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=118082
                 Default => "36",
                 Options => ["24", "36", "54"]),
            "VHIGHFS" =>
              Boolean
                ("Switch to full stepping (no microstep outputs) when in high velocity mode.", Default => False),
            "VHIGHCHM" =>
              Boolean
                ("Set CHM to constant off time, TFD to 0, and approximately double TOFF when in high velocity mode.",
                 Default => False),
            "TPFD" =>
              Integer
                ("Passive fast decay duration after bridge polarity change. Duration in TMC clock cycles (typically " &
                   "80ns) = 128 * TPFD.",
                 Default => 4,
                 Min     => 0,
                 Max     => 15,
                 Unit    => ""),
            "MRES" =>
              Discrete
                --  TODO: Emit error when microstep resolution is too high and document that feature here.
                ("Microstep resolution.",
                 --  [for X in TMC_Types.TMC2240.Microstep_Resolution_Type => X'Image]),
                 --  TODO: GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=118082
                 Default => "MS_256",
                 Options => ["MS_256", "MS_128", "MS_64", "MS_32", "MS_16", "MS_8", "MS_4", "MS_2", "MS_FULL_STEPS"]),
            "FAST_STANDSTILL" =>
              --  TODO: This description assumes the internal oscillator is used.
              Boolean
                ("If enabled, wait 2^18 TMC clock cycles (typically 21ms) instead of 2^20 cycles (84ms) after a " &
                   "step signal before beginning standstill detection.",
                 Default => False),
            "CHM" =>
              Variant
                ("Select the chopper mode to be used when StealthChop2 is disabled or the upper velocity limit for " &
                   "StealthChop2 is exceeded. VHIGHCHM may override this when the velocity set by VHIGH is " &
                   "exceeded. SpreadCycle usually produces better results.",
                 "SpreadCycle",
                 ["SpreadCycle" =>
                    Variant
                      ("Select manual SpreadCycle settings or settings derived from motor parameters and other " &
                         "driver parameters. Derived mode should be used unless manual tuning with an oscilloscope " &
                         "is being performed.",
                       "Derived",
                       ["Derived" =>
                         Sequence
                           ("Automatic calculation of optimal SpreadCycle parameters from motor parameters.",
                            ["Input voltage" =>
                              Float
                                ("Driver input voltage, not the voltage listed on the stepper motor specifications. " &
                                   "An error will be emitted if the measured driver voltage does not match this " &
                                   "parameter to within 10% at startup. Changing of the driver voltage after " &
                                   "startup will cause the motor to perform poorly.",
                                 Default => 24.0,
                                 Min     => 6.5,
                                 Max     => 40.0,
                                 Unit    => "V"),
                             "Phase inductance" =>
                               --  The very low values below will cause errors so there's no risk of the user
                               --  forgetting to set these values.
                               Float
                                 ("Inductance of each motor coil as listed on the motor specifications.",
                                  Default => 0.000_000_1,
                                  Min     => 0.000_000_1,
                                  Max     => 10_000_000.0,
                                  Unit    => "mH"),
                             "Phase resistance" =>
                               Float
                                 ("Resistance of each motor coil as listed on the motor specifications.",
                                  Default => 0.000_000_1,
                                  Min     => 0.000_000_1,
                                  Max     => 10_000_000.0,
                                  Unit    => "Ohm")]),
                       "Manual" =>
                         Sequence
                           ("SpreadCycle chopper settings. Refer to the TMC2240 datasheet for details on tuning " &
                              "these values. Derived parameters mode should be used unless manual tuning with " &
                              "an oscilloscope is being performed.",
                            ["HSTRT" =>
                              Integer
                                ("Hysteresis start setting as described in the TMC2240 datasheet. This is the " &
                                    "resultant value from 1 to 8, not the raw register value.",
                                 Default => 6,
                                 Min     => 1,
                                 Max     => 8,
                                 Unit    => ""),
                             "HEND" =>
                               Integer
                                 ("Hysteresis end setting as described in the TMC2240 datasheet. This is the " &
                                    "resultant value from -3 to 12, not the raw register value.",
                                  Default => -1,
                                  Min     => -3,
                                  Max     => 12,
                                  Unit    => "")])]),
                  "Constant off time" =>
                    Sequence
                      ("Constant off time chopper settings. No automatic tuning of these parameters is available. " &
                         "Refer to the TMC2240 datasheet for details on tuning these values. SpreadCycle in derived " &
                         "parameters mode should be used unless manual tuning with an oscilloscope is being " &
                         "performed.",
                       ["DISFDCC" =>
                          Boolean
                            ("If set, disable the usage of the current comparator for termination of the fast decay " &
                               "cycle. If not set then the fast decay cycle will be terminated early if the " &
                               "negative current value exceeds the previous positive value.",
                             Default => False),
                        "OFFSET" =>
                          Integer
                            ("Sine wave offset as described in the TMC2240 datasheet. This is the resultant value " &
                               "from -3 to 12, not the raw register value.",
                             Default => -1,
                             Min     => -3,
                             Max     => 12,
                             Unit    => ""),
                        "TFD" =>
                          Integer
                            ("Fast decay time setting as described in the TMC2240 datasheet. This parameter sets " &
                               "both FD3 and HSTRT_TFD210.",
                             Default => 5,
                             Min     => 0,
                             Max     => 15,
                             Unit    => "")])]),
            "StealthChop2 (EN_PWM_MODE)" =>
              Variant
                ("Enable or disable StealthChop2 for this driver. StealthChop2 reduces audible noise at low " &
                   "velocities. In most cases no tuning of parameters is required for good results.",
                 "Disabled",
                 ["Disabled" => Sequence ("StealthChop2 is disabled.", []),
                  "Enabled" =>
                    Sequence
                      ("StealthChop2 settings.",
                       ["TPWMTHRS" =>
                          Float
                            ("Upper velocity limit for StealthChop2 mode.",
                             Default => 1.0E100,
                             Min     => 0.0,
                             Max     => 1.0E100,
                             Unit    => "mm/s"),
                        "PWM_OFS" =>
                          Integer
                            ("Fixed part of StealthChop2 maximum PWM amplitude as described in TMC2240 datasheet. " &
                               "Usually this should be left at the default value of 29 and PWM_AUTOSCALE should be " &
                               "enabled.",
                             Default => 29,
                             Min     => 0,
                             Max     => 255,
                             Unit    => ""),
                        "PWM_GRAD" =>
                          Integer
                            ("Velocity dependent part of StealthChop2 maximum PWM described in TMC2240 datasheet. " &
                               "Usually this should be left at the default value of 0 and PWM_AUTOGRAD should be " &
                               "enabled.",
                             Default => 0,
                             Min     => 0,
                             Max     => 255,
                             Unit    => ""),
                        "PWM_FREQ" =>
                          --  TODO: This description assumes the internal oscillator is used.
                          Discrete
                            ("StealthChop2 PWM cycle duration measured in half TMC clock cycles (typically a half " &
                               "cycle is 40ns). Usually a value of 683 is a good default for a resultant frequency " &
                               "of 36kHz.",
                             Default => "683",
                             --  [for X in TMC_Types.TMC2240.PWM_Freq_Type => X'Image]),
                             --  TODO: GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=118082
                             Options => ["1024", "683", "512", "410"]),
                        "PWM_AUTOSCALE" =>
                          Boolean
                            ("Enable automatic tuning of PWM_OFS. Set current limits may not be effective if this " &
                               "is disabled!",
                             Default => True),
                        "PWM_AUTOGRAD" =>
                          Boolean
                            ("Enable automatic tuning of PWM_GRAD. Set current limits may not be effective if this " &
                               "is disabled!",
                             Default => True),
                        "FREEWHEEL" =>
                          Discrete
                            ("StealthChop2 standstill freewheeling mode when IHOLD = 0.",
                             Default => "NORMAL",
                             --  [for X in TMC_Types.TMC2240.Freewheel_Type => X'Image]),
                             --  TODO: GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=118082
                             Options => ["NORMAL", "FREEWHEEL", "SHORT_VIA_LS", "SHORT_VIA_HS"]),
                        "PWM_MEAS_SD_ENABLE" =>
                          Boolean
                            ("Use slow decay phase on low side to measure motor current when in StealthChop2 mode.",
                             Default => False),
                        "PWM_DIS_REG_STST" =>
                          Boolean
                            ("Disable StealthChop2 current regulation when in standstill and reduce the duty cycle " &
                               "to a very low value.",
                             Default => False),
                        "PWM_REG" =>
                          Integer
                            ("StealthChop2 maximum PWM auto-scaling change per half wave measured in half " &
                               "increments, with 1 being 0.5 increments.",
                             Default => 4,
                             Min     => 0,
                             Max     => 15,
                             Unit    => ""),
                        "PWM_LIM" =>
                          Integer
                            ("StealthChop2 PWM auto-scaling amplitude limit when switching from SpreadCycle to " &
                               "StealthChop2. Limits the upper 4 bits.",
                             Default => 12,
                             Min     => 0,
                             Max     => 15,
                             Unit    => ""),
                        "MULTISTEP_FILT" =>
                          Boolean
                            ("Some sort of undocumented filtering for StealthChop2. This should be left off on " &
                               "official Prunt hardware as the generated step signals have extremely low jitter.",
                             Default => False)])])]);

      Thermistor_Sequence : constant Property_Parameters_Access :=
        Sequence
          ("Thermistor settings.",
           ["Minimum temperature" =>
              Float
                ("Any temperature below this temperature will cause an emergency stop if the heater is enabled.",
                 Default => 0.0,
                 Min     => -1.0E100,
                 Max     => 1.0E100,
                 Unit    => "C"),
           "Maximum temperature" =>
             Float
               ("Any temperature above this temperature will cause an emergency stop if the heater is enabled.",
                Default => 0.0,
                Min     => -1.0E100,
                Max     => 1.0E100,
                Unit    => "C"),
            "Thermistor kind" =>
              Variant
                ("The kind of thermistor connected to this input.",
                 "Disabled",
                 ["Disabled" =>
                    Sequence
                      ("Thermistor is disabled.",
                       []),
                  "ATC Semitec 104GT-2" =>
                    Sequence
                      ("Using Steinhart-Hart model with A=8.0965E-4 B=2.1163E-4 C=7.0742E-8. " &
                         "You should ensure the thermistor output is correct before use.",
                       []),
                  "ATC Semitec 104NT-4-R025H42G" =>
                    Sequence
                      ("Using Steinhart-Hart model with A=7.9582E-4 B=2.1360E-4 C=6.4830E-8. " &
                         "You should ensure the thermistor output is correct before use.",
                       []),
                  "EPCOS 100K B57560G104F" =>
                    Sequence
                      ("Using Steinhart-Hart model with A=7.2213E-4 B=2.1676E-4 C=8.9293E-8. " &
                         "You should ensure the thermistor output is correct before use.",
                       []),
                  "Generic 3950" =>
                    Sequence
                      ("Using Steinhart-Hart model with A=7.9347E-4 B=2.0076E-4 C=1.6328E-7. " &
                         "You should ensure the thermistor output is correct before use.",
                       []),
                  "SliceEngineering 450" =>
                    Sequence
                      ("Using Steinhart-Hart model with A=3.0553E-4 B=2.1171E-4 C=1.1962E-7. " &
                         "You should ensure the thermistor output is correct before use.",
                       []),
                  "TDK NTCG104LH104JT1" =>
                    Sequence
                      ("Using Steinhart-Hart model with A=9.7639E-4 B=1.9688E-4 C=7.2671E-8. " &
                         "You should ensure the thermistor output is correct before use.",
                       []),
                  "Honeywell 100K 135-104LAG-J01" =>
                    Sequence
                      ("Using Steinhart-Hart model with A=4.5695E-4 B=2.5163E-4 C=0. " &
                         "You should ensure the thermistor output is correct before use.",
                       []),
                  "NTC 100K MGB18-104F39050L32" =>
                    Sequence
                      ("Using Steinhart-Hart model with A=5.4598E-4 B=2.4390E-4 C=0. " &
                         "You should ensure the thermistor output is correct before use.",
                       []),
                  "PT-1000 (PT-385 class above 0C)" =>
                    Sequence
                      ("Using Callendar-Van Dusen model with R(0)=1000 A=3.9083E-3 B=-5.775E-7. " &
                         "You should ensure the thermistor output is correct before use.",
                       []),
                  "PT-1000 (PT-392 class above 0C)" =>
                    Sequence
                      ("Using Callendar-Van Dusen model with R(0)=1000 A=3.9827E-3 B=-5.875E-7. " &
                         "You should ensure the thermistor output is correct before use.",
                       []),
                  "Custom Steinhart-Hart model" =>
                    Sequence
                      ("1 / T = A + B * ln(R) + C * (ln(R))**3 where T is in kelvins.",
                       ["A" =>
                          Float
                            ("",
                             Default => 0.0,
                             Min     => -1.0E100,
                             Max     => 1.0E100,
                             Unit    => ""),
                        "B" =>
                          Float
                            ("",
                             Default => 0.0,
                             Min     => -1.0E100,
                             Max     => 1.0E100,
                             Unit    => ""),
                        "C" =>
                          Float
                            ("",
                             Default => 0.0,
                             Min     => -1.0E100,
                             Max     => 1.0E100,
                             Unit    => "")]),
                 "Custom Callendar-Van Dusen model" =>
                    Sequence
                      ("R = R(0) * (1 + A * T + B * T**2) where T is in celsius.",
                       ["R(0)" =>
                          Float
                            ("",
                             Default => 0.0,
                             Min     => -1.0E100,
                             Max     => 1.0E100,
                             Unit    => "ohm"),
                        "A" =>
                          Float
                            ("",
                             Default => 0.0,
                             Min     => -1.0E100,
                             Max     => 1.0E100,
                             Unit    => ""),
                        "B" =>
                          Float
                            ("",
                             Default => 0.0,
                             Min     => -1.0E100,
                             Max     => 1.0E100,
                             Unit    => "")])])]);

      Input_Switch_Sequence : constant Property_Parameters_Access :=
        Sequence
          ("Input switch settings.",
           ["Enabled" =>
              Boolean
                ("Enable the switch, allowing it to be used for homing.",
                 Default => False),
            "Hit on high" =>
              Boolean
                ("Consider the switch to be hit when the input pin is in the high state.",
                 Default => False)]);

      TMC_Homing_Sequence : constant Property_Parameters_Access :=
        Sequence
          ("Homing parameters for this axis.",
           ["Homing method" =>
              Variant
                ("Homing method for this axis. Usually ""Set to value"" should be used for E axis and "
                 & """Use input switch"" should be used for XYZ.",
                 "Disabled",
                 ["Disabled" =>
                    Sequence
                      ("Homing not yet configured. Axis will not allow movement until homing is configured.", []),
                  "Set to value" =>
                    Sequence
                      ("Set this axis to a given position when homing is performed without actually moving.",
                       ["Position" =>
                          Float
                            ("Position to set the axis to.",
                             Default => 0.0,
                             Min => -1.0E100,
                             Max => 1.0E100,
                             Unit => "mm")]),
                  "Use input switch" =>
                    Sequence
                      ("Move towards a switch until it is hit, then back off, then move towards it slower and record "
                       & "the position.",
                       ["Switch" =>
                          Discrete
                            ("Input switch to use.",
                             Default => Input_Switch_Name_Strings.First_Element,
                             Options => Input_Switch_Name_Strings),
                        "Move towards negative infinity" =>
                          Boolean
                            ("If set then homing will move towards lower positions, otherwise homing will move "
                             & "towards higher positions.",
                             Default => True),
                        "First move distance" =>
                          Float
                            ("Distance past switch hit point allowed in first seeking move.",
                             Default => 0.1,
                             Min => 0.000_001,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Back off move distance" =>
                          Float
                            ("Distance that is moved back after the switch is first hit.",
                             Default => 2.0,
                             Min => 0.0,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Second move distance" =>
                          Float
                            ("Distance past switch hit point allowed in second seeking move.",
                             Default => 0.1,
                             Min => 0.000_001,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Switch position" =>
                          Float
                            ("Position of the switch on the axis. This value is allowed to be outside of the machine "
                             & "limits.",
                             Default => 0.0,
                             Min => -1.0E100,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Move to after" =>
                          Float
                            ("Position to move to after homing. This position must be inside the machine limits.",
                             Default => 0.0,
                             Min => -1.0E100,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Velocity limit" =>
                          Float
                           ("Velocity limit for this axis. May be safely set to 1E100 mm/s to solely use distance "
                            & "limits. Does not override regular velocity limit.",
                             Default => 1.0E100,
                             Min => 0.000_001,
                             Max => 1.0E100,
                            Unit => "mm/s")]),
                  "Use StallGuard2" =>
                    Sequence
                      ("Move until StallGuard2 is triggered on the selected motor. Configured SpreadCycle parameters "
                       & "will be used if SpreadCycle is enabled, otherwise the default SpreadCycle parameters "
                       & "will be used. The motor is always switched to SpreadCycle mode when homing this axis, "
                       & "regardless of configured velocity limits.",
                       ["Motor" =>
                          Discrete
                            ("Motor to use. Must be attached to the given axis, or the A/B axis for X/Y in Core XY.",
                             Default => StallGuard2_Stepper_Name_Strings.First_Element,
                             Options => StallGuard2_Stepper_Name_Strings),
                        "Move towards negative infinity" =>
                          Boolean
                            ("If set then homing will move towards lower positions, otherwise homing will move "
                             & "towards higher positions.",
                             Default => True),
                        "Threshold" =>
                          Integer
                            ("Threshold for StallGuard2. Higher values are less sensitive.",
                             Default => -64,
                             Min => -64,
                             Max => 63,
                             Unit => ""),
                        "Enable filter" =>
                          Boolean
                            ("Enable filter for more precise measurements. Causes measurement frequency to reduce "
                             & "to every 4 full steps.",
                             Default => False),
                        "Stop position" =>
                          Float
                            ("Position where the axis will hit a hard limit. This value is allowed to be outside of "
                             & "the machine limits.",
                             Default => 0.0,
                             Min => -1.0E100,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Move to after" =>
                          Float
                            ("Position to move to after homing. This position must be inside the machine limits.",
                             Default => 0.0,
                             Min => -1.0E100,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Velocity limit" =>
                          Float
                           ("Velocity limit for this axis. Does not override regular velocity limit.",
                             Default => 50.0,
                             Min => 0.000_001,
                             Max => 50.0,
                            Unit => "mm/s"),
                        "Acceleration limit" =>
                          Float
                           ("Acceleration limit for this axis. Does not override regular acceleration limit.",
                             Default => 1_000.0,
                             Min => 0.000_001,
                             Max => 1_000.0,
                            Unit => "mm/s^2")]),
                  "Use StallGuard4" =>
                    --  TODO: StallGuard 4 settings should not be available on machines with only SG2 capable drivers.
                    Sequence
                      ("Move until StallGuard4 is triggered on the selected motor. Configured StealthChop parameters "
                       & "will be used if StealthChop is enabled, otherwise the default StealthChop parameters "
                       & "will be used. The motor is always switched to StealthChop mode when homing this axis, "
                       & "regardless of configured velocity limits.",
                       ["Motor" =>
                          Discrete
                            ("Motor to use. Must be attached to the given axis, or the A/B axis for X/Y in Core XY.",
                             Default => StallGuard4_Stepper_Name_Strings.First_Element,
                             Options => StallGuard4_Stepper_Name_Strings),
                        "Move towards negative infinity" =>
                          Boolean
                            ("If set then homing will move towards lower positions, otherwise homing will move "
                             & "towards higher positions.",
                             Default => True),
                        "Threshold" =>
                          Integer
                            ("Threshold for StallGuard4. Higher values are more sensitive.",
                             Default => 255,
                             Min => 0,
                             Max => 255,
                             Unit => ""),
                        "Enable filter" =>
                          Boolean
                            ("Enable filter for more precise measurements. Causes measurements to become average of "
                             & "last 4 full steps.",
                             Default => False),
                        "Stop position" =>
                          Float
                            ("Position where the axis will hit a hard limit. This value is allowed to be outside of "
                             & "the machine limits.",
                             Default => 0.0,
                             Min => -1.0E100,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Move to after" =>
                          Float
                            ("Position to move to after homing. This position must be inside the machine limits.",
                             Default => 0.0,
                             Min => -1.0E100,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Velocity limit" =>
                          Float
                           ("Velocity limit for this axis. Does not override regular velocity limit.",
                             Default => 1.0E100,
                             Min => 0.000_001,
                             Max => 1.0E100,
                            Unit => "mm/s"),
                        "Acceleration limit" =>
                          Float
                           ("Acceleration limit for this axis. Does not override regular acceleration limit.",
                             Default => 1_000.0,
                             Min => 0.000_001,
                             Max => 1_000.0,
                            Unit => "mm/s^2")])]),
            "Prerequisites" =>
              Sequence_Over_Axes
                ("Required states of other axes.",
                 Variant
                   ("Required state of selected axis.",
                    "No requirement",
                    ["No requirement" =>
                      Sequence ("There are no requirements for this axis during homing.", []),
                     "Must be homed" =>
                       Sequence
                         ("This axis must be homed prior to the parent axis, but the position does not matter. " &
                            "Homing of the parent axis will also trigger homing of this axis if it is not already " &
                            "homed.",
                          []),
                     "Must be at position" =>
                       Sequence
                         ("This axis must be homed prior to the parent axis and it must be at a specified position. " &
                            "Homing of the parent axis will also trigger homing of this axis if it is not already " &
                            "homed. After this axis is homed it will move to the specified position.",
                          ["Position" =>
                            Float
                              ("Position to move to before homing the parent axis. This position must be inside the " &
                                 "machine limits.",
                               Default => 0.0,
                               Min => -1.0E100,
                               Max => 1.0E100,
                               Unit => "mm")])]))]);

      Regular_Homing_Sequence : constant Property_Parameters_Access :=
        Sequence
          ("Homing parameters for this axis.",
           ["Homing method" =>
              Variant
                ("Homing method for this axis. Usually ""Set to value"" should be used for E axis and "
                 & """Use input switch"" should be used for XYZ.",
                 "Disabled",
                 ["Disabled" =>
                    Sequence
                      ("Homing not yet configured. Axis will not allow movement until homing is configured.", []),
                  "Set to value" =>
                    Sequence
                      ("Set this axis to a given position when homing is performed without actually moving.",
                       ["Position" =>
                          Float
                            ("Position to set the axis to.",
                             Default => 0.0,
                             Min => -1.0E100,
                             Max => 1.0E100,
                             Unit => "mm")]),
                  "Use input switch" =>
                    Sequence
                      ("Move towards a switch until it is hit, then back off, then move towards it slower and record "
                       & "the position.",
                       ["Switch" =>
                          Discrete
                            ("Input switch to use.",
                             Default => Input_Switch_Name_Strings.First_Element,
                             Options => Input_Switch_Name_Strings),
                        "Move towards negative infinity" =>
                          Boolean
                            ("If set then homing will move towards lower positions, otherwise homing will move "
                             & "towards higher positions.",
                             Default => True),
                        "First move distance" =>
                          Float
                            ("Distance past switch hit point allowed in first seeking move.",
                             Default => 0.1,
                             Min => 0.000_001,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Back off move distance" =>
                          Float
                            ("Distance that is moved back after the switch is first hit.",
                             Default => 2.0,
                             Min => 0.0,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Second move distance" =>
                          Float
                            ("Distance past switch hit point allowed in second seeking move.",
                             Default => 0.1,
                             Min => 0.000_001,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Switch position" =>
                          Float
                            ("Position of the switch on the axis. This value is allowed to be outside of the machine "
                             & "limits.",
                             Default => 0.0,
                             Min => -1.0E100,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Move to after" =>
                          Float
                            ("Position to move to after homing. This position must be inside the machine limits.",
                             Default => 0.0,
                             Min => -1.0E100,
                             Max => 1.0E100,
                             Unit => "mm"),
                        "Velocity limit" =>
                          Float
                           ("Velocity limit for this axis. May be safely set to 1E100 mm/s to solely use distance "
                            & "limits. Does not override regular velocity limit.",
                             Default => 1.0E100,
                             Min => 0.000_001,
                             Max => 1.0E100,
                            Unit => "mm/s")])]),
            "Prerequisites" =>
              Sequence_Over_Axes
                ("Required states of other axes.",
                 Variant
                   ("Required state of selected axis.",
                    "No requirement",
                    ["No requirement" =>
                      Sequence ("There are no requirements for this axis during homing.", []),
                     "Must be homed" =>
                       Sequence
                         ("This axis must be homed prior to the parent axis, but the position does not matter. " &
                            "Homing of the parent axis will also trigger homing of this axis if it is not already " &
                            "homed.",
                          []),
                     "Must be at position" =>
                       Sequence
                         ("This axis must be homed prior to the parent axis and it must be at a specified position. " &
                            "Homing of the parent axis will also trigger homing of this axis if it is not already " &
                            "homed. After this axis is homed it will move to the specified position.",
                          ["Position" =>
                            Float
                              ("Position to move to before homing the parent axis. This position must be inside the " &
                                 "machine limits.",
                               Default => 0.0,
                               Min => -1.0E100,
                               Max => 1.0E100,
                               Unit => "mm")])]))]);

      Heater_Sequence : constant Property_Parameters_Access :=
        Sequence
          ("Parameters for this heater.",
           ["Thermistor" =>
              Discrete
                ("Thermistor connected to this heater.",
                 Default => Thermistor_Name'First'Image,
                 Options => Thermistor_Name_Strings),
            "Check maximum cumulative error" =>
              Float
                ("Maximum cumulative error before a failure is detected.",
                 Default => 120.0,
                 Min     => 0.0,
                 Max     => 1.0E100,
                 Unit    => "C"),
            "Check gain time" =>
              Float
                ("Period to check for temperature rise over during heating to detect failures.",
                 Default => 20.0,
                 Min     => 0.0,
                 Max     => 1.0E100,
                 Unit    => "s"),
            "Check minimum gain" =>
              Float
                ("Minium temperature rise required in gain period to reset cumulative error.",
                 Default => 2.0,
                 Min     => 0.0,
                 Max     => 1.0E100,
                 Unit    => "C"),
            "Check hysteresis" =>
              Float
                ("Maximum temperature below or above the setpoint where the heater is considered to be at " &
                   "temperature.",
                 Default => 3.0,
                 Min     => 0.0,
                 Max     => 1.0E100,
                 Unit    => "C"),
            "Control method" =>
              Variant
                ("Type of control used for this heater.",
                 "Disabled",
                 ["Disabled" =>
                    Sequence
                      ("This heater is disabled.",
                       []),
                  "PID" =>
                    Sequence
                      ("Basic PID control. Can be automatically tuned with M303 g-code command.",
                      ["Proportional scale" =>
                         Float
                           ("Scale for proportional part.",
                            Default => 0.0,
                            Min     => 0.0,
                            Max     => 1.0E100,
                            Unit    => ""),
                       "Integral scale" =>
                         Float
                           ("Scale for integral part.",
                            Default => 0.0,
                            Min     => 0.0,
                            Max     => 1.0E100,
                             Unit    => ""),
                       "Derivative scale" =>
                         Float
                           ("Scale for derivative part.",
                            Default => 0.0,
                            Min     => 0.0,
                            Max     => 1.0E100,
                            Unit    => "")]),
                  "Bang bang" =>
                    Sequence
                      ("Turn on the heater when the temperature falls below setpoint - hysteresis/2 and turn " &
                         "off the heater when the temperature rises above setpoint + hysteresis/2.",
                      ["Hysteresis" =>
                         Float
                           ("Hysteresis for switching the heater on or off.",
                            Default => 0.0,
                            Min     => 0.0,
                            Max     => 1.0E100,
                            Unit    => "C")])])]);

      Fixed_Switching_Fan_Sequence : constant Property_Parameters_Access :=
        Sequence
          ("Settings for this fan.",
           ["Invert PWM output" =>
              Boolean
                ("Invert the PWM output.",
                 Default => False),
           "PWM frequency" =>
              Float
                ("Frequency of the PWM output. 30Hz is usually the best value for 2-wire fans and 25000Hz is " &
                   "usually required for 4-wire fans.",
                 Default => 30.0,
                 Min     => 1.0,
                 Max     => 50_000.0,
                 --  TODO: The above range should be set based on Fan_Hardware, currently the range is only checked on
                 --  the server side.
                 Unit    => "Hz"),
            "Control method" =>
              Variant
                ("Type of control used for this fan.",
                 "Always on",
                 ["Dynamic duty cycle" =>
                    Sequence
                      ("Allow for setting of the duty cycle while the printer is running (e.g. via M106/M107).",
                      ["Disable below" =>
                         Float
                           ("Set the duty cycle to zero if it falls below this value.",
                            Default => 0.0,
                            Min     => 0.0,
                            Max     => 1.0,
                            Unit    => ""),
                      "Maximum duty cycle" =>
                         Float
                           ("Duty cycle corresponding to 100%.",
                            Default => 1.0,
                            Min     => 0.0,
                            Max     => 1.0,
                            Unit    => "")]),
                  "Always on" =>
                    Sequence
                      ("The fan always stays with a fixed PWM duty cycle.",
                      ["Duty cycle" =>
                         Float
                           ("Duty cycle to send to the fan.",
                            Default => 1.0,
                            Min     => 0.0,
                            Max     => 1.0,
                            Unit    => "")])])]);

      Low_Or_High_Side_Switching_Fan_Sequence : constant Property_Parameters_Access :=
        Sequence
          ("Settings for this fan.",
           ["Invert PWM output" =>
              Boolean
                ("Invert the PWM output.",
                 Default => False),
           "PWM frequency" =>
              Float
                ("Frequency of the PWM output. 30Hz is usually the best value for 2-wire fans and 25000Hz is " &
                   "usually required for 4-wire fans.",
                 Default => 30.0,
                 Min     => 1.0,
                 Max     => 50_000.0,
                 --  TODO: The above range should be set based on Fan_Hardware, currently the range is only checked on
                 --  the server side.
                 Unit    => "Hz"),
            "Control method" =>
              Variant
                ("Type of control used for this fan.",
                 "Always on",
                 ["Dynamic duty cycle" =>
                    Sequence
                      ("Allow for setting of the duty cycle while the printer is running (e.g. via M106/M107).",
                      ["Disable below" =>
                         Float
                           ("Set the duty cycle to zero if it falls below this value.",
                            Default => 0.0,
                            Min     => 0.0,
                            Max     => 1.0,
                            Unit    => ""),
                      "Maximum duty cycle" =>
                         Float
                           ("Duty cycle corresponding to 100%.",
                            Default => 1.0,
                            Min     => 0.0,
                            Max     => 1.0,
                            Unit    => "")]),
                  "Always on" =>
                    Sequence
                      ("The fan always stays with a fixed PWM duty cycle.",
                      ["Duty cycle" =>
                         Float
                           ("Duty cycle to send to the fan.",
                            Default => 1.0,
                            Min     => 0.0,
                            Max     => 1.0,
                            Unit    => "")])]),
            "Use high-side switching" =>
              Boolean
                ("If set then the power pin will be toggled to control the fan instead of the PWM pin. This allows " &
                   "for the tachometer to be used on 3-pin fans with a tachometer if the ground pin of the fan is" &
                   "connected to the ground pin of the connector rather than the PWM pin.", False)]);

      Shaper_Sequence : constant Property_Parameters_Access :=
        Variant
          ("Input shaping method used for this axis.",
           "No shaper",
           ["No shaper" =>
              Sequence
                ("No input shaping will be applied to this axis.",
                 []),
            "Zero vibration (ZV/ZVD/ZVDD/etc.)" =>
              Sequence
                ("Zero vibration shaper defined in N. C. Singer and W. P. Seering, ""Preshaping Command Inputs to " &
                   "Reduce System Vibration,"" Journal of Dynamic Systems, Measurement, and Control, vol. 112, no. " &
                   "1. ASME International, pp. 76-82, Mar. 01, 1990. doi: 10.1115/1.2894142.",
                 ["Frequency" =>
                    Float
                      ("This parameter is not copied between different shapers.",
                       Default => 1.0,
                       Min     => 1.0E-10,
                       Max     => 1.0E100,
                       Unit    => "Hz"),
                  "Damping ratio" =>
                    Float
                      ("This parameter is not copied between different shapers.",
                       Default => 0.1,
                       Min     => 0.001,
                       Max     => 0.999,
                       Unit    => "Hz"),
                  "Number of derivatives" =>
                    Integer
                      ("0 = ZV, 1 = ZVD, 2 = ZVDD, 3 = ZVDDD.",
                       Default => 0,
                       Min     => 0,
                       Max     => 3,
                       Unit    => "")]),
            "Extra insensitive (EI/2HEI/3HEI)" =>
              Sequence
                ("Extra insensitive shaper defined in W. E. Singhose, L. J. Porter, T. D. Tuttle, and N. C. Singer, " &
                   """Vibration Reduction Using Multi-Hump Input Shapers,"" Journal of Dynamic Systems, " &
                   "Measurement, and Control, vol. 119, no. 2. ASME International, pp. 320-326, Jun. 01, 1997. doi: " &
                   "10.1115/1.2801257.",
                 ["Frequency" =>
                    Float
                      ("This parameter is not copied between different shapers.",
                       Default => 1.0,
                       Min     => 1.0E-10,
                       Max     => 1.0E100,
                       Unit    => "Hz"),
                  "Damping ratio" =>
                    Float
                      ("This parameter is not copied between different shapers.",
                       Default => 0.1,
                       Min     => 0.001,
                       Max     => 0.999,
                       Unit    => "Hz"),
                  "Residual vibration level" =>
                    Float
                      ("This is hard-coded to 0.05 in other 3D printer motion controllers. Usually it does not need " &
                         "to be changed.",
                       Default => 0.05,
                       Min     => 0.001,
                       Max     => 0.999,
                       Unit    => ""),
                  "Number of humps" =>
                    Integer
                      ("",
                       Default => 1,
                       Min     => 1,
                       Max     => 3,
                       Unit    => "")])]);
      --!pp on

      Result : Property_Maps.Map;
   begin
      --!pp off
      Result :=
        ["Prunt" =>
          Sequence
            ("Prunt settings.",
             ["Enabled" =>
                Boolean
                  ("Enable the printer. The printer will automatically by disabled if an error occurs or if the " &
                     "configuration is invalid.",
                   Default => False),
              "Replace G0 with G1" =>
                Boolean
                  ("Replace all G0 g-code commands with G1 commands to mimic the behaviour of some other 3D " &
                     "printer motion controllers.",
                   Default => False)]),
         "Steppers" =>
           Tabbed_Sequence
             ("Stepper driver settings.",
              []),
         "Kinematics" =>
           Sequence
             ("Kinematic settings.",
              ["Lower position limit" =>
                 Sequence_Over_Axes
                   ("Minimum position that the printer may move to. Any axis may be set to -1E100 for effectively " &
                      "infinite range.",
                   Float
                     ("",
                      Default => 0.0,
                      Min     => -1.0E100,
                      Max     => 1.0E100,
                      Unit    => "mm")),
               "Upper position limit" =>
                 Sequence_Over_Axes
                   ("Maximum position that the printer may move to. Any axis may be set to 1E100 for effectively " &
                      "infinite range.",
                    Float
                      ("",
                       Default => 0.0,
                       Min     => -1.0E100,
                       Max     => 1.0E100,
                       Unit    => "mm")),
               "Ignore E in XYZE" =>
                 Boolean
                   ("Ignore the E axis component of the feedrate unless E is the only axis in a command (e.g. " &
                      "'G1 X1 E100 F1' will cause the X axis to move at 1 mm/min and the E axis will move as fast " &
                      "as required). This is the behaviour that most other 3D printer motion controllers use. The " &
                      "E axis feedrate limit and global feedrate limit will always be respected regardless of " &
                      "this setting.",
                    Default => True),
               "Shift blended corners" =>
                 Boolean
                   ("Attempt to shift blended corners such that the blended path intersects the original corner " &
                      "rather than cutting off the corner. Enabling this will also cause line segments to move " &
                      "outwards slightly to match the corners.",
                    Default => False),
               "Maximum tangential velocity" =>
                 Float
                   ("The maximum combined feedrate of all axes, including the E axis. Usually this should be set " &
                      "to a high value (e.g. 1E100) and the per-axis limits should be used instead.",
                    Default => 10.0,
                    Min     => 0.000_001,
                    Max     => 1.0E100,
                    Unit    => "mm/s"),
               "Axial velocity limits" =>
                 Sequence_Over_Axes
                   ("Maximum feedrate for each individual axis.",
                    Float
                      ("",
                       Default => 10.0,
                       Min     => 0.000_001,
                       Max     => 1.0E100,
                       Unit    => "mm/s")),
               "Pressure advance time" =>
                 Float
                   ("The E axis velocity is multiplied by this value and then added to the E axis position. " &
                      "This means that the maximum E axis velocity is the set maximum plus the pressure advance " &
                      "time multiplied by the set maximum acceleration. The same applies to jerk etc.. There is " &
                      "currently no option for smoothing of this value, so anything beyond a very small value may " &
                      "cause the velocity to be lowered significantly to avoid exceeding the maximum step rate of " &
                      "the stepper drivers.",
                    Default => 0.0,
                    Min     => -1.0E100,
                    Max     => 1.0E100,
                    Unit    => "s"),
               "Maximum chord error" =>
                 Float
                   ("The maximum distance that the planned path may deviate from the commanded path. Setting this " &
                      "parameter to 0 will cause the printer to come to a complete stop at every corner.",
                    Default => 0.1,
                    Min     => 0.0,
                    Max     => 1.0E100,
                    Unit    => "mm"),
               "Maximum acceleration" =>
                 Float
                   ("May safely be set to 1E100 for effectively infinite acceleration (to the extent allowed by " &
                      "other constraints). Axial values will go above this value when corner blending is enabled.",
                    Default => 100.0,
                    Min     => 0.000_001,
                    Max     => 1.0E100,
                    Unit    => "mm/s^2"),
               "Maximum jerk" =>
                 Float
                   ("May safely be set to 1E100 for effectively infinite jerk (to the extent allowed by " &
                      "other constraints). Axial values will go above this value when corner blending is enabled. " &
                      "A good starting point for tuning is 100 times the set maximum acceleration. " &
                      "This can be achieved by appending E2 to you acceleration value in this field.",
                    Default => 100.0E2,
                    Min     => 0.000_001,
                    Max     => 1.0E100,
                    Unit    => "mm/s^3"),
               "Maximum snap" =>
                 Float
                   ("May safely be set to 1E100 for effectively infinite snap (to the extent allowed by " &
                      "other constraints). Axial values will go above this value when corner blending is enabled. " &
                      "A good starting point for tuning is 100,000 times the set maximum acceleration. " &
                      "This can be achieved by appending E5 to you acceleration value in this field.",
                    Default => 100.0E5,
                    Min     => 0.000_001,
                    Max     => 1.0E100,
                    Unit    => "mm/s^4"),
               "Maximum crackle" =>
                 Float
                   ("May safely be set to 1E100 for effectively infinite crackle (to the extent allowed by " &
                      "other constraints). Axial values will go above this value when corner blending is enabled. " &
                      "A good starting point for tuning is 1,000,000,000 times the set maximum acceleration. " &
                      "This can be achieved by appending E9 to you acceleration value in this field.",
                    Default => 100.0E9,
                    Min     => 0.000_001,
                    Max     => 1.0E100,
                    Unit    => "mm/s^5"),
               "Axial scaler" =>
                 Sequence_Over_Axes
                   ("Inside the motion planner, all positions are divided by this value before applying motion " &
                      "profile limits, allowing for different limits on different axes. You do not need to take " &
                      "this value in to account when setting position limits, mm per step values, axial velocity " &
                      "limits, or when setting the feedrate in g-code. Corner deviation and tangential feedrate, " &
                      "acceleration, etc. is based on scaled positions, so a tangential acceleration of 10mm/s^2 " &
                      "and a scaler of 0.5 will set the axial limit to 5mm/s^2.",
                    Float
                      ("",
                       Default => 1.0,
                       Min     => 1.0E-100,
                       Max     => 1.0E100,
                       Unit    => "")),
               "Kinematics kind" =>
                 Variant
                   ("The type of kinematics used by the printer.",
                    "Cartesian",
                    ["Cartesian" =>
                       Sequence_Over_Steppers
                         ("Axis each stepper is attached to.",
                          Discrete
                            ("",
                             Default => "NONE",
                             Options => ["NONE", "X_AXIS", "Y_AXIS", "Z_AXIS", "E_AXIS"])),
                     "Core XY" =>
                       Sequence_Over_Steppers
                         ("Axis each stepper is attached to.",
                          Discrete
                            ("",
                             Default => "NONE",
                             Options => ["NONE", "A_AXIS", "B_AXIS", "Z_AXIS", "E_AXIS"]))])]),
         "Input switches" =>
           Tabbed_Sequence
             ("Input switch settings.",
              []),
         "Thermistors" =>
           Tabbed_Sequence
             ("Thermistor settings.",
              []),
         "Heaters" =>
           Tabbed_Sequence
             ("Heater settings.",
              []),
         "Homing" =>
           Tabbed_Sequence
             ("Homing settings.",
              []),
         "Fans" =>
           Tabbed_Sequence
             ("Fan settings.",
              []),
         "Input shaping" =>
           Tabbed_Sequence
             ("Input shaping settings.",
              []),
         "G-code assignments" =>
           Sequence
             ("Assign heaters to g-code commands.",
              ["Hotend heater" =>
                 Discrete
                   ("Heater to use for the hotend.",
                    Default => Heater_Name'First'Image,
                    Options => Heater_Name_Strings),
              "Bed heater" =>
                 Discrete
                   ("Heater to use for the bed.",
                    Default => Heater_Name'First'Image,
                    Options => Heater_Name_Strings),
              "Default fan" =>
                 Discrete
                   ("Fan to control when no P parameter is used for M106/M107.",
                    Default => Fan_Name'First'Image,
                    Options => Fan_Name_Strings)])];
      --!pp on

      for S in Stepper_Name loop
         Property_Maps.Insert
           (Property_Maps.Reference (Result, "Steppers").Element.all.Sequence_Children,
            S'Image,
            (if Stepper_Hardware (S).Kind = Basic_Kind
             then Basic_Stepper_Sequence
             elsif Stepper_Hardware (S).Kind = TMC2240_UART_Kind
             then TMC2240_Stepper_Sequence
             else raise Constraint_Error with "Config not implemented for stepper kind " & S'Image));
      end loop;

      for I in Input_Switch_Name loop
         --  Do not use a iterator filter here due to GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=121316
         if Input_Switch_Visible_To_User (I) then
            Property_Maps.Insert
              (Property_Maps.Reference (Result, "Input switches").Element.all.Sequence_Children,
               I'Image,
               Input_Switch_Sequence);
         end if;
      end loop;

      for T in Thermistor_Name loop
         Property_Maps.Insert
           (Property_Maps.Reference (Result, "Thermistors").Element.all.Sequence_Children,
            T'Image,
            Thermistor_Sequence);
      end loop;

      for H in Heater_Name loop
         Property_Maps.Insert
           (Property_Maps.Reference (Result, "Heaters").Element.all.Sequence_Children, H'Image, Heater_Sequence);
      end loop;

      for F in Fan_Name loop
         Property_Maps.Insert
           (Property_Maps.Reference (Result, "Fans").Element.all.Sequence_Children,
            F'Image,
            (if Fan_Hardware (F).Kind = Fixed_Switching_Kind
             then Fixed_Switching_Fan_Sequence
             elsif Fan_Hardware (F).Kind = Low_Or_High_Side_Switching_Kind
             then Low_Or_High_Side_Switching_Fan_Sequence
             else raise Constraint_Error with "Config not implemented for fan kind " & F'Image));
      end loop;

      for A in Axis_Name loop
         if [for S in Stepper_Name => Stepper_Hardware (S).Kind in TMC2240_UART_Kind]'Reduce ("or", False) then
            Property_Maps.Insert
              (Property_Maps.Reference (Result, "Homing").Element.all.Sequence_Children, A'Image, TMC_Homing_Sequence);
         else
            Property_Maps.Insert
              (Property_Maps.Reference (Result, "Homing").Element.all.Sequence_Children,
               A'Image,
               Regular_Homing_Sequence);
         end if;
      end loop;

      for A in Axis_Name loop
         Property_Maps.Insert
           (Property_Maps.Reference (Result, "Input shaping").Element.all.Sequence_Children, A'Image, Shaper_Sequence);
      end loop;

      return Result;
   end Build_Schema;

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
         Validate_Config (Get (Initial_Properties, "Properties"), Report);
      end Validate_Initial_Config;

      procedure Validate_Current_Config (Report : access procedure (Key, Message : String)) is
      begin
         Maybe_Do_Init;
         Validate_Config (Get (Current_Properties, "Properties"), Report);
      end Validate_Current_Config;

      procedure Validate_Config (Config : JSON_Value; Report : access procedure (Key, Message : String)) is
      begin
         if not Boolean'(Get (Config, "Prunt$Enabled")) then
            Report ("Prunt$Enabled", "Prunt is disabled. Enable after setting other options.");
         end if;

         for S in Stepper_Name loop
            if Get (Config, "Kinematics$Kinematics kind$" & Get (Config, "Kinematics$Kinematics kind") & "$" & S'Image)
              /= "NONE"
            then
               if not Boolean'(Get (Config, "Steppers$" & S'Image & "$Enabled")) then
                  Report
                    ("Steppers$" & S'Image & "$Enabled",
                     "Stepper is attached to "
                     & Get
                         (Config,
                          "Kinematics$Kinematics kind$" & Get (Config, "Kinematics$Kinematics kind") & "$" & S'Image)
                     & " but stepper is not enabled.");
               end if;
            end if;
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
                         (Long_Float'Max
                            (0.0,
                             Long_Float'Floor
                               (My_Get_Long_Float (Config, "Steppers$" & S'Image & "$IRUN") * 32.0 - 1.0)));
                     IRUN_During_Homing          : constant TMC_Types.Unsigned_5 :=
                       TMC_Types.Unsigned_5
                         (Long_Float'Max
                            (0.0,
                             Long_Float'Floor
                               (My_Get_Long_Float (Config, "Steppers$" & S'Image & "$IRUN during homing")
                                * 32.0
                                - 1.0)));
                  begin
                     if not Boolean'(Get (Config, "Steppers$" & S'Image & "$Enabled")) then
                        null;
                     elsif Get (Config, "Steppers$" & S'Image & "$CHM") = "SpreadCycle"
                       and then Get (Config, "Steppers$" & S'Image & "$CHM$SpreadCycle") = "Derived"
                     then
                        if IRUN_During_Homing > IRUN then
                           Report
                             ("Steppers$" & S'Image & "$IRUN during homing",
                              "IRUN during homing must be less than or equal to IRUN.");
                        end if;

                        TMC_Types.TMC2240.Optimize_Spreadcycle
                          (Driver_Voltage              =>
                             Get (Config, "Steppers$" & S'Image & "$CHM$SpreadCycle$Derived$Input voltage") * volt,
                           TBL                         =>
                             TMC_Types.TMC2240.TBL_Type'Value
                               ("BLANK_" & Get (Config, "Steppers$" & S'Image & "$TBL")),
                           Motor_Inductance            =>
                             Get (Config, "Steppers$" & S'Image & "$CHM$SpreadCycle$Derived$Phase inductance")
                             / 1_000.0
                             * henry,
                           Motor_Resistance            =>
                             Get (Config, "Steppers$" & S'Image & "$CHM$SpreadCycle$Derived$Phase resistance") * ohm,
                           Motor_Peak_Current          => Get (Config, "Steppers$" & S'Image & "$Run current") * amp,
                           TOFF                        =>
                             TMC_Types.TMC2240.TOFF_Type'Value
                               ("OFF_" & Get (Config, "Steppers$" & S'Image & "$TOFF")),
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
                     elsif Get (Config, "Steppers$" & S'Image & "$CHM") = "SpreadCycle"
                       and then Get (Config, "Steppers$" & S'Image & "$CHM$SpreadCycle") = "Manual"
                     then
                        if IRUN_During_Homing > IRUN then
                           Report
                             ("Steppers$" & S'Image & "$IRUN during homing",
                              "IRUN during homing must be less than or equal to IRUN.");
                        end if;

                        if IRUN = 31
                          and then Integer'
                                     (Get (Config, "Steppers$" & S'Image & "$CHM$SpreadCycle$Manual$HEND")
                                      + Get (Config, "Steppers$" & S'Image & "$CHM$SpreadCycle$Manual$HSTRT"))
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
            declare
               Freq : constant Frequency := Get (Config, "Fans$" & F'Image & "$PWM frequency") * hertz;
            begin
               case Fan_Hardware (F).Kind is
                  when Fixed_Switching_Kind =>
                     if Freq > Fan_Hardware (F).Maximum_PWM_Frequency then
                        Report
                          ("Fans$" & F'Image & "$PWM frequency",
                           "Fan frequency must be less than or equal to "
                           & Trim (Fan_Hardware (F).Maximum_PWM_Frequency'Image)
                           & " Hz.");
                     end if;

                  when Low_Or_High_Side_Switching_Kind =>
                     if Boolean'(Get (Config, "Fans$" & F'Image & "$Use high-side switching")) then
                        if Freq > Fan_Hardware (F).Maximum_High_Side_PWM_Frequency then
                           Report
                             ("Fans$" & F'Image & "$PWM frequency",
                              "Fan frequency must be less than or equal to "
                              & Trim (Fan_Hardware (F).Maximum_High_Side_PWM_Frequency'Image)
                              & " Hz when in high-side switching mode, which is currently selected (the limit is "
                              & Trim (Fan_Hardware (F).Maximum_Low_Side_PWM_Frequency'Image)
                              & " in low-side mode).");
                        end if;
                     else
                        if Freq > Fan_Hardware (F).Maximum_Low_Side_PWM_Frequency then
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
            end;
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
                        if Get (Config, "Homing$" & Current_Axis'Image & "$Prerequisites$" & Next_Axis'Image)
                          /= "No requirement"
                        then
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
               if Get (Config, "Homing$" & Start_Axis'Image & "$Prerequisites$" & Start_Axis'Image) /= "No requirement"
               then
                  Report
                    ("Homing$" & Start_Axis'Image & "$Prerequisites$" & Start_Axis'Image,
                     "The homing procedure for an axis can not have itself as a prerequisite.");
               end if;
               Report_Loops (Start_Axis, Start_Axis, Start_Axis'Image);
            end;
         end loop;

         for A in Axis_Name loop
            for B in Axis_Name loop
               if Get (Config, "Homing$" & A'Image & "$Prerequisites$" & B'Image) = "Must be at position" then
                  declare
                     Position    : constant Length :=
                       Get
                         (Config, "Homing$" & A'Image & "$Prerequisites$" & B'Image & "$Must be at position$Position")
                       * mm;
                     Lower_Limit : constant Length := Get (Config, "Kinematics$Lower position limit$" & B'Image) * mm;
                     Upper_Limit : constant Length := Get (Config, "Kinematics$Upper position limit$" & B'Image) * mm;
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
            for X of Discrete_String_Sets.Set'["input switch", "StallGuard2", "StallGuard4"] loop
               if Get (Config, "Homing$" & A'Image & "$Homing method") = "Use " & X then
                  declare
                     Position    : constant Length :=
                       Get (Config, "Homing$" & A'Image & "$Homing method$Use " & X & "$Move to after") * mm;
                     Lower_Limit : constant Length := Get (Config, "Kinematics$Lower position limit$" & A'Image) * mm;
                     Upper_Limit : constant Length := Get (Config, "Kinematics$Upper position limit$" & A'Image) * mm;
                  begin
                     if Position < Lower_Limit or Position > Upper_Limit then
                        Report
                          ("Homing$" & A'Image & "$Homing method$Use " & X & "$Move to after",
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
            for X of Discrete_String_Sets.Set'["StallGuard2", "StallGuard4"] loop
               if Get (Config, "Homing$" & A'Image & "$Homing method") = "Use " & X then
                  declare
                     Motor           : constant Stepper_Name :=
                       Stepper_Name'Value (Get (Config, "Homing$" & A'Image & "$Homing method$Use " & X & "$Motor"));
                     Kinematics_Kind : constant String := Get (Config, "Kinematics$Kinematics kind");
                     Motor_Axis      : constant String :=
                       Get (Config, "Kinematics$Kinematics kind$" & Kinematics_Kind & "$" & Motor'Image);
                  begin
                     if Motor_Axis /= Trim (A'Image)
                       and not (A in X_Axis | Y_Axis and Motor_Axis in "A_AXIS" | "B_AXIS")
                     then
                        Report
                          ("Homing$" & A'Image & "$Homing method$Use " & X & "$Motor",
                           "Motor is not assigned to this axis, motor is assigned to " & Motor_Axis & ".");
                     end if;
                  end;
               end if;
            end loop;
         end loop;

      --  TODO: Check that scaler will not cause max step rate to be exceeded.
      end Validate_Config;

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

         if Get (Current_Properties, "Schema version") /= Long_Integer'(11) then
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
            Validate_Config (Get (Current_Properties, "Properties"), Handle_Invalid_Config'Access);

            if Initial_Config_Valid then
               Initial_Config := JSON_To_Config (Get (Current_Properties, "Properties"));
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

      function JSON_To_Config (Data : JSON_Value) return Full_Config is
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

            Microsteps : constant Dimensionless :=
              (if Stepper_Hardware (S).Kind = TMC2240_UART_Kind
               then MS_Lookup (Microstep_Resolution_Type'Value (Get (Data, "Steppers$" & S'Image & "$MRES")))
               else 1.0);
         begin
            if Get (Data, "Steppers$" & S'Image & "$Distance per step") = "Direct entry" then
               declare
                  Prefix               : constant String := "Steppers$" & S'Image & "$Distance per step$Direct entry$";
                  Direction_Multiplier : constant Dimensionless :=
                    (if Boolean'(Get (Data, Prefix & "Reverse direction")) then -1.0 else 1.0);
                  Distance             : constant Length := Get (Data, Prefix & "Distance per step") * mm;
               begin
                  return Direction_Multiplier * Distance / Microsteps;
               end;
            elsif Get (Data, "Steppers$" & S'Image & "$Distance per step") = "Lead screw" then
               declare
                  Prefix               : constant String := "Steppers$" & S'Image & "$Distance per step$Lead screw$";
                  Direction_Multiplier : constant Dimensionless :=
                    (if Boolean'(Get (Data, Prefix & "Reverse direction")) then -1.0 else 1.0);
                  Lead                 : constant Length := Get (Data, Prefix & "Lead") * mm;
                  Numerator            : constant Dimensionless := Get (Data, Prefix & "Gear ratio$Numerator");
                  Denominator          : constant Dimensionless := Get (Data, Prefix & "Gear ratio$Denominator");
                  Full_Steps           : constant Dimensionless := Get (Data, Prefix & "Full steps per rotation");
               begin
                  return Direction_Multiplier * Lead / (Full_Steps * Microsteps * Numerator / Denominator);
               end;
            elsif Get (Data, "Steppers$" & S'Image & "$Distance per step") = "Gear with circumference" then
               declare
                  Prefix               : constant String :=
                    "Steppers$" & S'Image & "$Distance per step$Gear with circumference$";
                  Direction_Multiplier : constant Dimensionless :=
                    (if Boolean'(Get (Data, Prefix & "Reverse direction")) then -1.0 else 1.0);
                  Circumference        : constant Length := Get (Data, Prefix & "Circumference") * mm;
                  Numerator            : constant Dimensionless := Get (Data, Prefix & "Gear ratio$Numerator");
                  Denominator          : constant Dimensionless := Get (Data, Prefix & "Gear ratio$Denominator");
                  Full_Steps           : constant Dimensionless := Get (Data, Prefix & "Full steps per rotation");
               begin
                  return Direction_Multiplier * Circumference / (Full_Steps * Microsteps * Numerator / Denominator);
               end;
            elsif Get (Data, "Steppers$" & S'Image & "$Distance per step") = "Gear with tooth count and pitch" then
               declare
                  Prefix               : constant String :=
                    "Steppers$" & S'Image & "$Distance per step$Gear with tooth count and pitch$";
                  Direction_Multiplier : constant Dimensionless :=
                    (if Boolean'(Get (Data, Prefix & "Reverse direction")) then -1.0 else 1.0);
                  Tooth_Count          : constant Dimensionless := Get (Data, Prefix & "Tooth count");
                  Tooth_Pitch          : constant Length := Get (Data, Prefix & "Tooth pitch") * mm;
                  Numerator            : constant Dimensionless := Get (Data, Prefix & "Gear ratio$Numerator");
                  Denominator          : constant Dimensionless := Get (Data, Prefix & "Gear ratio$Denominator");
                  Full_Steps           : constant Dimensionless := Get (Data, Prefix & "Full steps per rotation");
               begin
                  return
                    Direction_Multiplier
                    * Tooth_Count
                    * Tooth_Pitch
                    / (Full_Steps * Microsteps * Numerator / Denominator);
               end;
            else
               raise Constraint_Error;
            end if;
         end Get_Distance_Per_Step;
      begin
         Config.Prunt :=
           (Enabled => Get (Data, "Prunt$Enabled"), Replace_G0_With_G1 => Get (Data, "Prunt$Replace G0 with G1"));

         if Get (Data, "Kinematics$Kinematics kind") = "Cartesian" then
            Config.Kinematics :=
              (Kind               => Cartesian_Kind,
               Planner_Parameters => <>,
               Z_Steppers         => (others => False),
               E_Steppers         => (others => False),
               X_Steppers         => (others => False),
               Y_Steppers         => (others => False));
            for S in Stepper_Name loop
               if Get (Data, "Kinematics$Kinematics kind$Cartesian$" & S'Image) = "Z_AXIS" then
                  Config.Kinematics.Z_Steppers (S) := True;
               elsif Get (Data, "Kinematics$Kinematics kind$Cartesian$" & S'Image) = "E_AXIS" then
                  Config.Kinematics.E_Steppers (S) := True;
               elsif Get (Data, "Kinematics$Kinematics kind$Cartesian$" & S'Image) = "X_AXIS" then
                  Config.Kinematics.X_Steppers (S) := True;
               elsif Get (Data, "Kinematics$Kinematics kind$Cartesian$" & S'Image) = "Y_AXIS" then
                  Config.Kinematics.Y_Steppers (S) := True;
               elsif Get (Data, "Kinematics$Kinematics kind$Cartesian$" & S'Image) = "NONE" then
                  null;
               else
                  raise Constraint_Error;
               end if;
            end loop;
         elsif Get (Data, "Kinematics$Kinematics kind") = "Core XY" then
            Config.Kinematics :=
              (Kind               => Core_XY_Kind,
               Planner_Parameters => <>,
               Z_Steppers         => (others => False),
               E_Steppers         => (others => False),
               A_Steppers         => (others => False),
               B_Steppers         => (others => False));
            for S in Stepper_Name loop
               if Get (Data, "Kinematics$Kinematics kind$Core XY$" & S'Image) = "Z_AXIS" then
                  Config.Kinematics.Z_Steppers (S) := True;
               elsif Get (Data, "Kinematics$Kinematics kind$Core XY$" & S'Image) = "E_AXIS" then
                  Config.Kinematics.E_Steppers (S) := True;
               elsif Get (Data, "Kinematics$Kinematics kind$Core XY$" & S'Image) = "A_AXIS" then
                  Config.Kinematics.A_Steppers (S) := True;
               elsif Get (Data, "Kinematics$Kinematics kind$Core XY$" & S'Image) = "B_AXIS" then
                  Config.Kinematics.B_Steppers (S) := True;
               elsif Get (Data, "Kinematics$Kinematics kind$Core XY$" & S'Image) = "NONE" then
                  null;
               else
                  raise Constraint_Error;
               end if;
            end loop;
         else
            raise Constraint_Error;
         end if;

         Config.Kinematics.Planner_Parameters :=
           (Lower_Pos_Limit         =>
              (X_Axis => Get (Data, "Kinematics$Lower position limit$X_AXIS") * mm,
               Y_Axis => Get (Data, "Kinematics$Lower position limit$Y_AXIS") * mm,
               Z_Axis => Get (Data, "Kinematics$Lower position limit$Z_AXIS") * mm,
               E_Axis => Get (Data, "Kinematics$Lower position limit$E_AXIS") * mm),
            Upper_Pos_Limit         =>
              (X_Axis => Get (Data, "Kinematics$Upper position limit$X_AXIS") * mm,
               Y_Axis => Get (Data, "Kinematics$Upper position limit$Y_AXIS") * mm,
               Z_Axis => Get (Data, "Kinematics$Upper position limit$Z_AXIS") * mm,
               E_Axis => Get (Data, "Kinematics$Upper position limit$E_AXIS") * mm),
            Ignore_E_In_XYZE        => Get (Data, "Kinematics$Ignore E in XYZE"),
            Shift_Blended_Corners   => Get (Data, "Kinematics$Shift blended corners"),
            Tangential_Velocity_Max => Get (Data, "Kinematics$Maximum tangential velocity") * mm / s,
            Axial_Velocity_Maxes    =>
              (X_Axis => Get (Data, "Kinematics$Axial velocity limits$X_AXIS") * mm / s,
               Y_Axis => Get (Data, "Kinematics$Axial velocity limits$Y_AXIS") * mm / s,
               Z_Axis => Get (Data, "Kinematics$Axial velocity limits$Z_AXIS") * mm / s,
               E_Axis => Get (Data, "Kinematics$Axial velocity limits$E_AXIS") * mm / s),
            Pressure_Advance_Time   => Get (Data, "Kinematics$Pressure advance time") * s,
            Acceleration_Max        => Get (Data, "Kinematics$Maximum acceleration") * mm / s**2,
            Jerk_Max                => Get (Data, "Kinematics$Maximum jerk") * mm / s**3,
            Snap_Max                => Get (Data, "Kinematics$Maximum snap") * mm / s**4,
            Crackle_Max             => Get (Data, "Kinematics$Maximum crackle") * mm / s**5,
            Chord_Error_Max         => Get (Data, "Kinematics$Maximum chord error") * mm,
            Axial_Scaler            =>
              (X_Axis => Get (Data, "Kinematics$Axial scaler$X_AXIS"),
               Y_Axis => Get (Data, "Kinematics$Axial scaler$Y_AXIS"),
               Z_Axis => Get (Data, "Kinematics$Axial scaler$Z_AXIS"),
               E_Axis => Get (Data, "Kinematics$Axial scaler$E_AXIS")));

         Config.G_Code_Assignments :=
           (Bed_Heater    => Heater_Name'Value (Get (Data, "G-code assignments$Bed heater")),
            Hotend_Heater => Heater_Name'Value (Get (Data, "G-code assignments$Hotend heater")),
            Default_Fan   => Fan_Name'Value (Get (Data, "G-code assignments$Default fan")));

         for S in Stepper_Name loop
            case Stepper_Hardware (S).Kind is
               when Basic_Kind =>
                  Config.Steppers (S) :=
                    (Kind        => Basic_Kind,
                     Enabled     => Get (Data, "Steppers$" & S'Image & "$Enabled"),
                     Mm_Per_Step => Get_Distance_Per_Step (S));

               when TMC2240_UART_Kind =>
                  Config.Steppers (S) :=
                    (Kind               => TMC2240_UART_Kind,
                     Enabled            => Get (Data, "Steppers$" & S'Image & "$Enabled"),
                     Mm_Per_Step        => Get_Distance_Per_Step (S),
                     GCONF              =>
                       (Reserved_1       => 0,
                        Fast_Standstill  => Get (Data, "Steppers$" & S'Image & "$FAST_STANDSTILL"),
                        En_PWM_Mode      =>
                          TMC_Boolean (Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)") = "Enabled"),
                        Multistep_Filt   =>
                          Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$MULTISTEP_FILT"),
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
                          (if My_Get_Long_Float (Data, "Steppers$" & S'Image & "$Run current") > 2.0
                           then TMC_Types.TMC2240.Max_3A
                           elsif My_Get_Long_Float (Data, "Steppers$" & S'Image & "$Run current") > 1.0
                           then TMC_Types.TMC2240.Max_2A
                           else TMC_Types.TMC2240.Max_1A),
                        Reserved_1    => 0,
                        Slope_Control =>
                          TMC_Types.TMC2240.Slope_Control_Type'Value
                            (Get (Data, "Steppers$" & S'Image & "$SLOPE_CONTROL")),
                        Reserved_2    => 0),
                     GLOBAL_SCALER      =>
                       (Global_Scaler =>
                          (if My_Get_Long_Float (Data, "Steppers$" & S'Image & "$Run current") = 3.0
                           then 0
                           elsif My_Get_Long_Float (Data, "Steppers$" & S'Image & "$Run current") > 2.0
                           then
                             TMC_Types.TMC2240.Global_Scaler_Type
                               (Dimensionless'Floor (Get (Data, "Steppers$" & S'Image & "$Run current") / 3.0 * 256.0))
                           elsif My_Get_Long_Float (Data, "Steppers$" & S'Image & "$Run current") = 2.0
                           then 0
                           elsif My_Get_Long_Float (Data, "Steppers$" & S'Image & "$Run current") > 1.0
                           then
                             TMC_Types.TMC2240.Global_Scaler_Type
                               (Dimensionless'Floor (Get (Data, "Steppers$" & S'Image & "$Run current") / 2.0 * 256.0))
                           elsif My_Get_Long_Float (Data, "Steppers$" & S'Image & "$Run current") = 1.0
                           then 0
                           else
                             TMC_Types.TMC2240.Global_Scaler_Type
                               (Dimensionless'Max
                                  (32.0,
                                   Dimensionless'Floor
                                     (Get (Data, "Steppers$" & S'Image & "$Run current") / 1.0 * 256.0)))),
                        Reserved      => 0),
                     IHOLD_IRUN         =>
                       (I_Hold       =>
                          (if Boolean'(Get (Data, "Steppers$" & S'Image & "$Enabled"))
                           then
                             TMC_Types.Unsigned_5
                               (Long_Float'Max
                                  (0.0,
                                   Long_Float'Floor
                                     (My_Get_Long_Float (Data, "Steppers$" & S'Image & "$IHOLD") * 32.0 - 1.0)))
                           else 0),
                        Reserved_1   => 0,
                        I_Run        =>
                          (if Boolean'(Get (Data, "Steppers$" & S'Image & "$Enabled"))
                           then
                             TMC_Types.Unsigned_5
                               (Long_Float'Max
                                  (0.0,
                                   Long_Float'Floor
                                     (My_Get_Long_Float (Data, "Steppers$" & S'Image & "$IRUN") * 32.0 - 1.0)))
                           else 0),
                        Reserved_2   => 0,
                        I_Hold_Delay =>
                          TMC_Types.Unsigned_4
                            (Long_Float'Floor
                               (My_Get_Long_Float (Data, "Steppers$" & S'Image & "$IHOLDDELAY") / 21.0)),
                        Reserved_3   => 0,
                        I_Run_Delay  =>
                          TMC_Types.Unsigned_4
                            (Long_Float'Floor
                               (My_Get_Long_Float (Data, "Steppers$" & S'Image & "$IRUNDELAY") / 0.041)),
                        Reserved_4   => 0),
                     IRUN_During_Homing =>
                       (if Boolean'(Get (Data, "Steppers$" & S'Image & "$Enabled"))
                        then
                          TMC_Types.Unsigned_5
                            (Long_Float'Max
                               (0.0,
                                Long_Float'Floor
                                  (My_Get_Long_Float (Data, "Steppers$" & S'Image & "$IRUN during homing")
                                   * 32.0
                                   - 1.0)))
                        else 0),
                     TPOWERDOWN         =>
                       (T_Power_Down =>
                          TMC_Types.Unsigned_8
                            (Long_Float'Floor
                               (My_Get_Long_Float (Data, "Steppers$" & S'Image & "$TPOWERDOWN") / 21.0)),
                        Reserved     => 0),
                     TPWMTHRS           =>
                       (T_PWM_Thrs =>
                          TMC_Types.Unsigned_20
                            (Dimensionless'Floor
                               (Dimensionless'Min
                                  (12_500_000.0
                                   * hertz
                                   * abs (Get_Distance_Per_Step (S))
                                   / ((Get
                                         (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$TPWMTHRS")
                                       + 1.0E-100)
                                      * (mm / Prunt.s)),
                                   2.0**20 - 1.0))),
                        Reserved   => 0),
                     TCOOLTHRS          => (T_Cool_Thrs => 0, Reserved => 0),
                     THIGH              =>
                       (T_High   =>
                          TMC_Types.Unsigned_20
                            (Dimensionless'Floor
                               (Dimensionless'Min
                                  (12_500_000.0
                                   * hertz
                                   * abs (Get_Distance_Per_Step (S))
                                   / ((Get (Data, "Steppers$" & S'Image & "$THIGH") + 1.0E-100) * mm / Prunt.s),
                                   2.0**20 - 1.0))),
                        Reserved => 0),
                     CHOPCONF           =>
                       (TOFF                 =>
                          (if Boolean'(Get (Data, "Steppers$" & S'Image & "$Enabled"))
                           then
                             TMC_Types.TMC2240.TOFF_Type'Value ("OFF_" & Get (Data, "Steppers$" & S'Image & "$TOFF"))
                           else Prunt.TMC_Types.TMC2240.Disable_Driver),
                        HSTRT_TFD210         => 5,
                        --  Set later if required.
                        HEND_OFFSET          => 3,
                        --  Set later if required.
                        FD3                  => 0,
                        --  Set later if required.
                        DISFDCC              => Get (Data, "Steppers$" & S'Image & "$CHM$Constant off time$DISFDCC"),
                        Reserved_1           => 0,
                        CHM                  =>
                          (if Get (Data, "Steppers$" & S'Image & "$Enabled")
                             or else Get (Data, "Steppers$" & S'Image & "$CHM") = "SpreadCycle"
                           then TMC_Types.TMC2240.SpreadCycle_Mode
                           elsif Get (Data, "Steppers$" & S'Image & "$CHM") = "Constant off time"
                           then TMC_Types.TMC2240.Constant_Off_Time_Mode
                           else raise Constraint_Error with "Not implemented."),
                        TBL                  =>
                          TMC_Types.TMC2240.TBL_Type'Value ("BLANK_" & Get (Data, "Steppers$" & S'Image & "$TBL")),
                        Reserved_2           => 0,
                        VHIGHFS              => Get (Data, "Steppers$" & S'Image & "$VHIGHFS"),
                        VHIGHCHM             => Get (Data, "Steppers$" & S'Image & "$VHIGHCHM"),
                        TPFD                 => Get (Data, "Steppers$" & S'Image & "$TPFD"),
                        Microstep_Resolution =>
                          TMC_Types.TMC2240.Microstep_Resolution_Type'Value
                            (Get (Data, "Steppers$" & S'Image & "$MRES")),
                        Interpolate          => False,
                        Double_Edge          => TMC_Types.TMC_Boolean (Stepper_Hardware (S).Double_Edge_Stepping),
                        --  Set correctly in Prunt.Controller.
                        Disable_S2G          => False,
                        Disable_S2Vs         => False),
                     PWMCONF            =>
                       (PWM_OFS            =>
                          Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$PWM_OFS"),
                        PWM_Grad           =>
                          Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$PWM_GRAD"),
                        PWM_Freq           =>
                          TMC_Types.TMC2240.PWM_Freq_Type'Value
                            ("FREQ_"
                             & Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$PWM_FREQ")),
                        PWM_Auto_Scale     =>
                          Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$PWM_AUTOSCALE"),
                        PWM_Auto_Grad      =>
                          Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$PWM_AUTOGRAD"),
                        Freewheel          =>
                          TMC_Types.TMC2240.Freewheel_Type'Value
                            (Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$FREEWHEEL")),
                        PWM_Meas_SD_Enable =>
                          Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$PWM_MEAS_SD_ENABLE"),
                        PWM_Dis_Reg_Stst   =>
                          Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$PWM_DIS_REG_STST"),
                        PWM_Reg            =>
                          Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$PWM_REG"),
                        PWM_Lim            =>
                          Get (Data, "Steppers$" & S'Image & "$StealthChop2 (EN_PWM_MODE)$Enabled$PWM_LIM")));

                  if not Boolean'(Get (Data, "Steppers$" & S'Image & "$Enabled")) then
                     --  Use default parameters.
                     null;
                  elsif Get (Data, "Steppers$" & S'Image & "$CHM") = "Constant off time" then
                     Config.Steppers (S).CHOPCONF.DISFDCC :=
                       Get (Data, "Steppers$" & S'Image & "$CHM$Constant off time$DISFDCC");
                     Config.Steppers (S).CHOPCONF.HEND_OFFSET :=
                       TMC_Types.Unsigned_4
                         (Long_Integer'(Get (Data, "Steppers$" & S'Image & "$CHM$Constant off time$OFFSET") + 3));
                     Config.Steppers (S).CHOPCONF.HSTRT_TFD210 :=
                       TMC_Types.Unsigned_3
                         (Long_Integer'(Get (Data, "Steppers$" & S'Image & "$CHM$Constant off time$TFD") rem 8));
                     Config.Steppers (S).CHOPCONF.FD3 :=
                       TMC_Types.Unsigned_1
                         (Long_Integer'(Get (Data, "Steppers$" & S'Image & "$CHM$Constant off time$TFD") / 8));
                  elsif Get (Data, "Steppers$" & S'Image & "$CHM") = "SpreadCycle"
                    and then Get (Data, "Steppers$" & S'Image & "$CHM$SpreadCycle") = "Manual"
                  then
                     Config.Steppers (S).CHOPCONF.HEND_OFFSET :=
                       TMC_Types.Unsigned_4
                         (Long_Integer'(Get (Data, "Steppers$" & S'Image & "$CHM$SpreadCycle$Manual$HEND") + 3));
                     Config.Steppers (S).CHOPCONF.HSTRT_TFD210 :=
                       TMC_Types.Unsigned_3
                         (Long_Integer'(Get (Data, "Steppers$" & S'Image & "$CHM$SpreadCycle$Manual$HSTRT") - 1));
                  elsif Get (Data, "Steppers$" & S'Image & "$CHM") = "SpreadCycle"
                    and then Get (Data, "Steppers$" & S'Image & "$CHM$SpreadCycle") = "Derived"
                  then
                     declare
                        Sum_Too_High                : Boolean;
                        Sum_Too_High_For_Full_Scale : Boolean;
                        Excessive_Heating           : Boolean;
                        Driver_Voltage_Too_Low      : Boolean;
                     begin
                        TMC_Types.TMC2240.Optimize_Spreadcycle
                          (Driver_Voltage              =>
                             Get (Data, "Steppers$" & S'Image & "$CHM$SpreadCycle$Derived$Input voltage") * volt,
                           TBL                         => Config.Steppers (S).CHOPCONF.TBL,
                           Motor_Inductance            =>
                             Get (Data, "Steppers$" & S'Image & "$CHM$SpreadCycle$Derived$Phase inductance")
                             / 1_000.0
                             * henry,
                           Motor_Resistance            =>
                             Get (Data, "Steppers$" & S'Image & "$CHM$SpreadCycle$Derived$Phase resistance") * ohm,
                           Motor_Peak_Current          => Get (Data, "Steppers$" & S'Image & "$Run current") * amp,
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
                  else
                     raise Constraint_Error with "Not implemented.";
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
            end case;
         end loop;

         for I in Input_Switch_Name loop
            --  Do not use a iterator filter here due to GCC bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=121316
            if Input_Switch_Visible_To_User (I) then
               Config.Switches (I) :=
                 (Enabled     => Get (Data, "Input switches$" & I'Image & "$Enabled"),
                  Hit_On_High => Get (Data, "Input switches$" & I'Image & "$Hit on high"));
            else
               Config.Switches (I) := (Enabled => False, Hit_On_High => False);
            end if;
         end loop;

         for A in Axis_Name loop
            if Get (Data, "Homing$" & A'Image & "$Homing method") = "Disabled" then
               Config.Homing (A) := (Kind => Disabled_Kind, Prerequisites => (others => <>));
            elsif Get (Data, "Homing$" & A'Image & "$Homing method") = "Use input switch" then
               Config.Homing (A) :=
                 (Kind                   => Double_Tap_Kind,
                  Switch                 =>
                    Input_Switch_Name'Value
                      (Get (Data, "Homing$" & A'Image & "$Homing method$Use input switch$Switch")),
                  First_Move_Distance    =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use input switch$First move distance") * mm,
                  Back_Off_Move_Distance =>
                    -Get (Data, "Homing$" & A'Image & "$Homing method$Use input switch$Back off move distance") * mm,
                  Second_Move_Distance   =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use input switch$Second move distance") * mm,
                  Switch_Position        =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use input switch$Switch position") * mm,
                  Move_To_After          =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use input switch$Move to after") * mm,
                  Velocity_Limit         =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use input switch$Velocity limit") * mm / s,
                  Prerequisites          => (others => <>));

               if Boolean'
                   (Get (Data, "Homing$" & A'Image & "$Homing method$Use input switch$Move towards negative infinity"))
               then
                  Config.Homing (A).First_Move_Distance := -Config.Homing (A).First_Move_Distance;
                  Config.Homing (A).Back_Off_Move_Distance := -Config.Homing (A).Back_Off_Move_Distance;
                  Config.Homing (A).Second_Move_Distance := -Config.Homing (A).Second_Move_Distance;
               end if;
            elsif Get (Data, "Homing$" & A'Image & "$Homing method") = "Set to value" then
               Config.Homing (A) :=
                 (Kind          => Set_To_Value_Kind,
                  Value         => Get (Data, "Homing$" & A'Image & "$Homing method$Set to value$Position") * mm,
                  Prerequisites => (others => <>));
            elsif Get (Data, "Homing$" & A'Image & "$Homing method") = "Use StallGuard2" then
               Config.Homing (A) :=
                 (Kind               => StallGuard2_Kind,
                  Move_To_Negative   =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard2$Move towards negative infinity"),
                  Enable_Filter      =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard2$Enable filter"),
                  Motor              =>
                    Stepper_Name'Value (Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard2$Motor")),
                  SG2_Threshold      =>
                    TMC_Types.Unsigned_7
                      (Integer'
                         (Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard2$Threshold") mod 2**7)),
                  Switch_Position    =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard2$Stop position") * mm,
                  Move_To_After      =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard2$Move to after") * mm,
                  Velocity_Limit     =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard2$Velocity limit") * mm / s,
                  Acceleration_Limit =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard2$Acceleration limit") * mm / s**2,
                  Prerequisites      => (others => <>));
            elsif Get (Data, "Homing$" & A'Image & "$Homing method") = "Use StallGuard4" then
               Config.Homing (A) :=
                 (Kind               => StallGuard4_Kind,
                  Move_To_Negative   =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard4$Move towards negative infinity"),
                  Enable_Filter      =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard4$Enable filter"),
                  Motor              =>
                    Stepper_Name'Value (Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard4$Motor")),
                  SG4_Threshold      => Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard4$Threshold"),
                  Switch_Position    =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard4$Stop position") * mm,
                  Move_To_After      =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard4$Move to after") * mm,
                  Velocity_Limit     =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard4$Velocity limit") * mm / s,
                  Acceleration_Limit =>
                    Get (Data, "Homing$" & A'Image & "$Homing method$Use StallGuard4$Acceleration limit") * mm / s**2,
                  Prerequisites      => (others => <>));
            else
               raise Constraint_Error;
            end if;

            for B in Axis_Name loop
               if Get (Data, "Homing$" & A'Image & "$Prerequisites$" & B'Image) = "No requirement" then
                  Config.Homing (A).Prerequisites (B) := (Kind => No_Requirement_Kind);
               elsif Get (Data, "Homing$" & A'Image & "$Prerequisites$" & B'Image) = "Must be homed" then
                  Config.Homing (A).Prerequisites (B) := (Kind => Must_Be_Homed_Kind);
               elsif Get (Data, "Homing$" & A'Image & "$Prerequisites$" & B'Image) = "Must be at position" then
                  Config.Homing (A).Prerequisites (B) :=
                    (Kind     => Must_Be_At_Position_Kind,
                     Position =>
                       Get (Data, "Homing$" & A'Image & "$Prerequisites$" & B'Image & "$Must be at position$Position")
                       * mm);
               end if;
            end loop;
         end loop;

         for T in Thermistor_Name loop
            if Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "Disabled" then
               Config.Thermistors (T) :=
                 (Kind                => Disabled_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius);
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "Custom Steinhart-Hart model" then
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  SH_A                =>
                    Get (Data, "Thermistors$" & T'Image & "$Thermistor kind$Custom Steinhart-Hart model$A"),
                  SH_B                =>
                    Get (Data, "Thermistors$" & T'Image & "$Thermistor kind$Custom Steinhart-Hart model$B"),
                  SH_C                =>
                    Get (Data, "Thermistors$" & T'Image & "$Thermistor kind$Custom Steinhart-Hart model$C"));
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "Custom Callendar-Van Dusen model" then
               Config.Thermistors (T) :=
                 (Kind                => Callendar_Van_Dusen_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  CVD_R0              =>
                    Get (Data, "Thermistors$" & T'Image & "$Thermistor kind$Custom Callendar-Van Dusen model$R(0)")
                    * ohm,
                  CVD_A               =>
                    Get (Data, "Thermistors$" & T'Image & "$Thermistor kind$Custom Callendar-Van Dusen model$A"),
                  CVD_B               =>
                    Get (Data, "Thermistors$" & T'Image & "$Thermistor kind$Custom Callendar-Van Dusen model$B"));
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "ATC Semitec 104GT-2" then
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  SH_A                => 8.0965E-4,
                  SH_B                => 2.1163E-4,
                  SH_C                => 7.0742E-8);
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "ATC Semitec 104NT-4-R025H42G" then
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  SH_A                => 7.9582E-4,
                  SH_B                => 2.1360E-4,
                  SH_C                => 6.4830E-8);
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "EPCOS 100K B57560G104F" then
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  SH_A                => 7.2213E-4,
                  SH_B                => 2.1676E-4,
                  SH_C                => 8.9293E-8);
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "Generic 3950" then
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  SH_A                => 7.9347E-4,
                  SH_B                => 2.0076E-4,
                  SH_C                => 1.6328E-7);
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "SliceEngineering 450" then
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  SH_A                => 3.0553E-4,
                  SH_B                => 2.1171E-4,
                  SH_C                => 1.1962E-7);
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "TDK NTCG104LH104JT1" then
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  SH_A                => 9.7639E-4,
                  SH_B                => 1.9688E-4,
                  SH_C                => 7.2671E-8);
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "Honeywell 100K 135-104LAG-J01" then
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  SH_A                => 4.5695E-4,
                  SH_B                => 2.5163E-4,
                  SH_C                => 0.0);
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "NTC 100K MGB18-104F39050L32" then
               Config.Thermistors (T) :=
                 (Kind                => Steinhart_Hart_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  SH_A                => 5.4598E-4,
                  SH_B                => 2.4390E-4,
                  SH_C                => 0.0);
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "PT-1000 (PT-385 class above 0C)" then
               Config.Thermistors (T) :=
                 (Kind                => Callendar_Van_Dusen_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  CVD_R0              => 1_000.0 * ohm,
                  CVD_A               => 3.9083E-3,
                  CVD_B               => -5.775E-7);
            elsif Get (Data, "Thermistors$" & T'Image & "$Thermistor kind") = "PT-1000 (PT-392 class above 0C)" then
               Config.Thermistors (T) :=
                 (Kind                => Callendar_Van_Dusen_Kind,
                  Minimum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Minimum temperature") * celsius,
                  Maximum_Temperature => Get (Data, "Thermistors$" & T'Image & "$Maximum temperature") * celsius,
                  CVD_R0              => 1_000.0 * ohm,
                  CVD_A               => 3.9827E-3,
                  CVD_B               => -5.875E-7);
            else
               raise Constraint_Error;
            end if;
         end loop;

         for H in Heater_Name loop
            if Get (Data, "Heaters$" & H'Image & "$Control method") = "Disabled" then
               Config.Heaters (H) :=
                 (Thermistor => Thermistor_Name'Value (Get (Data, "Heaters$" & H'Image & "$Thermistor")),
                  Params     =>
                    (Kind                       => Disabled_Kind,
                     Check_Max_Cumulative_Error =>
                       Get (Data, "Heaters$" & H'Image & "$Check maximum cumulative error") * celsius,
                     Check_Gain_Time            => Get (Data, "Heaters$" & H'Image & "$Check gain time") * s,
                     Check_Minimum_Gain         => Get (Data, "Heaters$" & H'Image & "$Check minimum gain") * celsius,
                     Check_Hysteresis           => Get (Data, "Heaters$" & H'Image & "$Check hysteresis") * celsius));
            elsif Get (Data, "Heaters$" & H'Image & "$Control method") = "PID" then
               Config.Heaters (H) :=
                 (Thermistor => Thermistor_Name'Value (Get (Data, "Heaters$" & H'Image & "$Thermistor")),
                  Params     =>
                    (Kind                       => PID_Kind,
                     Check_Max_Cumulative_Error =>
                       Get (Data, "Heaters$" & H'Image & "$Check maximum cumulative error") * celsius,
                     Check_Gain_Time            => Get (Data, "Heaters$" & H'Image & "$Check gain time") * s,
                     Check_Minimum_Gain         => Get (Data, "Heaters$" & H'Image & "$Check minimum gain") * celsius,
                     Check_Hysteresis           => Get (Data, "Heaters$" & H'Image & "$Check hysteresis") * celsius,
                     Proportional_Scale         =>
                       Get (Data, "Heaters$" & H'Image & "$Control method$PID$Proportional scale"),
                     Integral_Scale             =>
                       Get (Data, "Heaters$" & H'Image & "$Control method$PID$Integral scale"),
                     Derivative_Scale           =>
                       Get (Data, "Heaters$" & H'Image & "$Control method$PID$Derivative scale")));
            elsif Get (Data, "Heaters$" & H'Image & "$Control method") = "Bang bang" then
               Config.Heaters (H) :=
                 (Thermistor => Thermistor_Name'Value (Get (Data, "Heaters$" & H'Image & "$Thermistor")),
                  Params     =>
                    (Kind                       => Bang_Bang_Kind,
                     Check_Max_Cumulative_Error =>
                       Get (Data, "Heaters$" & H'Image & "$Check maximum cumulative error") * celsius,
                     Check_Gain_Time            => Get (Data, "Heaters$" & H'Image & "$Check gain time") * s,
                     Check_Minimum_Gain         => Get (Data, "Heaters$" & H'Image & "$Check minimum gain") * celsius,
                     Check_Hysteresis           => Get (Data, "Heaters$" & H'Image & "$Check hysteresis") * celsius,
                     Bang_Bang_Hysteresis       =>
                       Get (Data, "Heaters$" & H'Image & "$Control method$Bang bang$Hysteresis") * celsius));
            else
               raise Constraint_Error;
            end if;
         end loop;

         for F in Fan_Name loop
            if Get (Data, "Fans$" & F'Image & "$Control method") = "Always on" then
               Config.Fans (F) :=
                 (Kind                    => Always_On_Kind,
                  Invert_Output           => Get (Data, "Fans$" & F'Image & "$Invert PWM output"),
                  PWM_Frequency           => Get (Data, "Fans$" & F'Image & "$PWM frequency") * hertz,
                  Use_High_Side_Switching =>
                    (if Fan_Hardware (F).Kind = Low_Or_High_Side_Switching_Kind
                     then Get (Data, "Fans$" & F'Image & "$Use high-side switching")
                     else False),
                  Always_On_PWM           => Get (Data, "Fans$" & F'Image & "$Control method$Always on$Duty cycle"));
            elsif Get (Data, "Fans$" & F'Image & "$Control method") = "Dynamic duty cycle" then
               Config.Fans (F) :=
                 (Kind                    => Dynamic_PWM_Kind,
                  Invert_Output           => Get (Data, "Fans$" & F'Image & "$Invert PWM output"),
                  PWM_Frequency           => Get (Data, "Fans$" & F'Image & "$PWM frequency") * hertz,
                  Use_High_Side_Switching =>
                    (if Fan_Hardware (F).Kind = Low_Or_High_Side_Switching_Kind
                     then Get (Data, "Fans$" & F'Image & "$Use high-side switching")
                     else False),
                  Disable_Below_PWM       =>
                    Get (Data, "Fans$" & F'Image & "$Control method$Dynamic duty cycle$Disable below"),
                  Max_PWM                 =>
                    Get (Data, "Fans$" & F'Image & "$Control method$Dynamic duty cycle$Maximum duty cycle"));
            else
               raise Constraint_Error;
            end if;
         end loop;

         for A in Axis_Name loop
            if Get (Data, "Input shaping$" & A'Image) = "No shaper" then
               Config.Shapers (A) := (Kind => No_Shaper);
            elsif Get (Data, "Input shaping$" & A'Image) = "Zero vibration (ZV/ZVD/ZVDD/etc.)" then
               Config.Shapers (A) :=
                 (Kind                         => Zero_Vibration,
                  Zero_Vibration_Frequency     =>
                    Get (Data, "Input shaping$" & A'Image & "$Zero vibration (ZV/ZVD/ZVDD/etc.)$Frequency") * hertz,
                  Zero_Vibration_Damping_Ratio =>
                    Get (Data, "Input shaping$" & A'Image & "$Zero vibration (ZV/ZVD/ZVDD/etc.)$Damping ratio"),
                  Zero_Vibration_Deriviatives  =>
                    Zero_Vibration_Deriviatives_Count
                      (Integer'
                         (Get
                            (Data,
                             "Input shaping$"
                             & A'Image
                             & "$Zero vibration (ZV/ZVD/ZVDD/etc.)$Number of derivatives"))));
            elsif Get (Data, "Input shaping$" & A'Image) = "Extra insensitive (EI/2HEI/3HEI)" then
               Config.Shapers (A) :=
                 (Kind                                 => Extra_Insensitive,
                  Extra_Insensitive_Frequency          =>
                    Get (Data, "Input shaping$" & A'Image & "$Extra insensitive (EI/2HEI/3HEI)$Frequency") * hertz,
                  Extra_Insensitive_Damping_Ratio      =>
                    Get (Data, "Input shaping$" & A'Image & "$Extra insensitive (EI/2HEI/3HEI)$Damping ratio"),
                  Extra_Insensitive_Residual_Vibration =>
                    Get
                      (Data,
                       "Input shaping$" & A'Image & "$Extra insensitive (EI/2HEI/3HEI)$Residual vibration level"),
                  Extra_Insensitive_Humps              =>
                    Extra_Insensitive_Humps_Count
                      (Integer'
                         (Get
                            (Data,
                             "Input shaping$"
                             & A'Image
                             & "$Extra insensitive (EI/2HEI/3HEI)$Number of derivatives"))));
            else
               raise Constraint_Error;
            end if;
         end loop;

         return Config;
      end JSON_To_Config;

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

   procedure Read (Data : out Prunt_Parameters) is
   begin
      Config_File.Read (Data);
   end Read;

   procedure Read (Data : out Stepper_Parameters; Stepper : Stepper_Name) is
   begin
      Config_File.Read (Data, Stepper);
   end Read;

   procedure Read (Data : out Kinematics_Parameters) is
   begin
      Config_File.Read (Data);
   end Read;

   procedure Read (Data : out Input_Switch_Parameters; Input_Switch : Input_Switch_Name) is
   begin
      Config_File.Read (Data, Input_Switch);
   end Read;

   procedure Read (Data : out Homing_Parameters; Axis : Axis_Name) is
   begin
      Config_File.Read (Data, Axis);
   end Read;

   procedure Read (Data : out Thermistor_Parameters; Thermistor : Thermistor_Name) is
   begin
      Config_File.Read (Data, Thermistor);
   end Read;

   procedure Read (Data : out Heater_Full_Parameters; Heater : Heater_Name) is
   begin
      Config_File.Read (Data, Heater);
   end Read;

   procedure Read (Data : out Fan_Parameters; Fan : Fan_Name) is
   begin
      Config_File.Read (Data, Fan);
   end Read;

   procedure Read (Data : out G_Code_Assignment_Parameters) is
   begin
      Config_File.Read (Data);
   end Read;

   procedure Read (Data : out Shaper_Parameters; Axis : Axis_Name) is
   begin
      Config_File.Read (Data, Axis);
   end Read;

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

end Prunt.Config;
