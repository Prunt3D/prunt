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

package body Prunt.Config_JSON is

   pragma Unsuppress (All_Checks);

   function Trim (S : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Side => Ada.Strings.Both);
   end Trim;

   function Build_Schema return Property_Maps.Map is
      function Boolean (Description : String) return Property_Parameters_Access is
      begin
         return new Property_Parameters'(Kind => Boolean_Kind, Description => To_Unbounded_String (Description));
      end Boolean;

      function Sequence (Description : String; Children : Property_Maps.Map) return Property_Parameters_Access
      is
      begin
         return
           new Property_Parameters'
             (Kind        => Sequence_Kind,
              Description => To_Unbounded_String (Description),
              Children    => Children);
      end Sequence;

      function Variant (Description : String; Children : Property_Maps.Map) return Property_Parameters_Access
      is
      begin
         return
           new Property_Parameters'
             (Kind        => Variant_Kind,
              Description => To_Unbounded_String (Description),
              Children    => Children);
      end Variant;

      function Integer
        (Description : String; Min, Max : Long_Long_Integer; Unit : String) return Property_Parameters_Access
      is
      begin
         return
           new Property_Parameters'
             (Kind         => Integer_Kind,
              Description  => To_Unbounded_String (Description),
              Integer_Min  => Min,
              Integer_Max  => Max,
              Integer_Unit => To_Unbounded_String (Unit));
      end Integer;

      function Float (Description : String; Min, Max : Long_Float; Unit : String) return Property_Parameters_Access
      is
      begin
         return
           new Property_Parameters'
             (Kind        => Float_Kind,
              Description => To_Unbounded_String (Description),
              Float_Min   => Min,
              Float_Max   => Max,
              Float_Unit  => To_Unbounded_String (Unit));
      end Float;

      function Discrete (Description : String; Options : Discrete_String_Sets.Set) return Property_Parameters_Access
      is
      begin
         return
           new Property_Parameters'
             (Kind             => Discrete_Kind,
              Description      => To_Unbounded_String (Description),
              Discrete_Options => Options);
      end Discrete;

      Stepper_Name_Strings : constant Discrete_String_Sets.Set := [for S in Stepper_Name => S'Image];
   begin
      --!pp off
      return
        ["Prunt" =>
          Sequence
            ("Prunt Settings.",
             ["Enabled" =>
               Boolean
                 ("Enable the printer."),
              "Replace G0 with G1" =>
                Boolean
                  ("Replace all G0 g-code commands with G1 commands to mimic the behaviour of some other 3D " &
                     "printer motion controllers.")]),
         "Steppers" =>
           Sequence
             ("Stepper driver settings.",
              [for S of Stepper_Name_Strings =>
                (if Stepper_Kinds (Stepper_Name'Value (S)) = Basic_Kind then
                    Sequence
                      ("Basic stepper driver settings.",
                       ["Enabled" =>
                         Boolean ("Enable this stepper driver, allowing it to be attached to an axis."),
                        "Distance per step" =>
                          Float ("Distance moved by the attached motor for each step signal.",
                                 -8.0E307, 8.0E307, "mm")])
                 elsif Stepper_Kinds (Stepper_Name'Value (S)) = TMC2240_UART_Kind then
                    Sequence
                      ("TMC2240 stepper driver settings.",
                       ["Enabled" =>
                         Boolean
                           ("Enable this stepper driver, allowing it to be attached to an axis."),
                        "Distance per step" =>
                          Float
                            ("Distance moved by the attached motor for each step signal.",
                             Min => -8.0E307, Max => 8.0E307, Unit => "mm"),
                        "Run current" =>
                          Float
                            ("Peak current limit for each motor coil. This parameter will be used to set " &
                               "CURRENT_RANGE to the smallest suitable range before setting GLOBALSCALER.",
                             Min => 0.125, Max => 3.0, Unit => "A"),
                        "SLOPE_CONTROL" =>
                          Discrete
                            ("Output slew rate. 400V/uS is usually a good setting. 800V/uS provides a minimal " &
                               "decrease in power dissipation.",
                             [for X in TMC_Types.TMC2240.Slope_Control_Type => X'Image]),
                        "IHOLD" =>
                          Integer
                            ("Standstill current setting. 0 = 1/32, ..., 31 = 32/32.",
                             Min => 0, Max => 31, Unit => ""),
                        "IRUN" =>
                          Integer
                            ("Run current setting. 0 = 1/32, ..., 31 = 32/32.",
                             Min => 0, Max => 31, Unit => ""),
                        "IHOLDDELAY" =>
                          Integer
                            ("Slew time for motor power down in multiples of 2^18 TMC clock cycles (2^18 cycles is " &
                               "typically approximately 20ms).",
                             Min => 0, Max => 15, Unit => ""),
                        "IRUNDELAY" =>
                          Integer
                            ("Slew time for motor power up after exiting standstill in multiples of 512 TMC clock " &
                               "cycles (512 cycles is typically approximately 40us).",
                             Min => 0, Max => 15, Unit => ""),
                        "TPOWERDOWN" =>
                          Integer
                            ("Delay before motor power down in multiples of 2^18 TMC clock cycles (2^18 cycles is " &
                               "typically approximately 20ms). A minimum value of 2 is required for automatic " &
                               "StealthChop2 tuning.",
                             Min => 0, Max => 255, Unit => ""),
                        "TCOOLTHRS" =>
                          Integer
                            ("Lower velocity limit for CoolStep mode measured in the number of TMC clock cycles " &
                               "between step signals (1 cycle is typically approximately 80ns).",
                             Min => 0, Max => 16#FFFFF#, Unit => ""),
                        "THIGH" =>
                          Integer
                            ("Lower velocity limit for high velocity mode and upper velocity limit for " &
                               "CoolStep/StealthChop2 mode measured in the number of TMC clock cycles between step " &
                               "signals (1 cycle is typically approximately 80ns).",
                             Min => 0, Max => 16#FFFFF#, Unit => ""),
                        "TOFF" =>
                          --  TODO: This description assumes the internal oscillator is used.
                          Discrete
                            ("Slow decay (i.e. off time) duration in TMC clock cycles (typically approximately " &
                               "80ns). 120 is usually a good setting when combined with TBL = 36 for a " &
                               "theoretical maximum chopper frequency of 40kHz.",
                             Min => 0, Max => 15, Unit => ""),
                        "TBL" =>
                          --  TODO: This description and allowed values assumes the internal oscillator is used.
                          Discrete
                            ("Comparator blank time measured in TMC clock cycles (typically approximately 80ns). " &
                               "36 is usually a good setting when combined with TOFF = 120 for a theoretical " &
                               "maximum chopper frequency of 40kHz.",
                             [for X in TMC_Types.TMC2240.TBL_Type range Blank_24 .. Blank_54 => X'Image]),
                        "VHIGHFS" =>
                          Boolean
                            ("Switch to full stepping (no microstep outputs) when in high velocity mode."),
                        "VHIGHCHM" =>
                          Boolean
                            ("Set CHM to constant off time, TFD to 0, and approximately double TOFF when in high " &
                               "velocity mode."),
                        "TPFD" =>
                          Integer
                            ("Passive fast decay duration after bridge polarity change. Duration in TMC clock " &
                               "cycles (typically approximately 80ns) = 128 * TPFD.",
                             Min => 0, Max => 15, Unit => ""),
                        "MRES" =>
                          Discrete
                            --  TODO: Emit error when microstep resolution is too high and document that feature here.
                            ("Microstep resolution.",
                             [for X in TMC_Types.TMC2240.Microstep_Resolution_Type => X'Image]),
                        "FAST_STANDSTILL" =>
                          Boolean
                            ("If enabled, wait 2^18 TMC clock cycles (typically approximately 20ms) instead of 2^20 " &
                               "cycles (80ms) after a step signal be before beginning standstill detection."),
                        "CHM" => Variant
                          ("Select the chopper mode to be used when StealthChop2 is disabled or the upper " &
                             "velocity limit for StealthChop2 is exceeded. VHIGHCHM may override this when the " &
                             "velocity set by VHIGH is exceeded. SpreadCycle usually produces better results.",
                           ["SpreadCycle" => Variant
                             ("Select manual SpreadCycle settings or settings derived from motor parameters and " &
                                "other driver parameters. Derived mode should be used unless manual tuning with an " &
                                "oscilloscope is being performed.",
                              ["Derived" =>
                                Sequence
                                  ("Automatic calculation of optimal SpreadCycle parameters from motor parameters.",
                                   ["Input voltage" =>
                                     Float
                                       ("Driver input voltage, not the voltage listed on the stepper motor " &
                                          "specifications. An error will be emitted if the measured driver " &
                                          "voltage does not match this parameter to within 10% at startup. " &
                                          "Changing of the driver voltage after startup will cause the motor to " &
                                          "perform poorly.",
                                        Min => 6.5, Max => 40.0, Unit => "V"),
                                    "Phase inductance" =>
                                      Float
                                        ("Inductance of each motor coil as listed on the motor specifications.",
                                         Min => 0.000_000_1, Max => 10_000_000.0, Unit => "mH"),
                                    "Phase resistance" =>
                                      Float
                                        ("Resistance of each motor coil as listed on the motor specifications.",
                                         Min => 0.000_000_1, Max => 10_000_000.0, Unit => "Ohm"),
                                   ]),
                               "Manual" =>
                                 Sequence
                                   ("SpreadCycle chopper settings. Refer to the TMC2240 datasheet for details on " &
                                      "tuning these values. Derived parameters mode should be used unless manual " &
                                      "tuning with an oscilloscope is being performed.",
                                    ["HSTRT" =>
                                      Integer
                                        ("Hysteresis start setting as described in the TMC2240 datasheet.",
                                         Min => 0, Max => 7, Unit => ""),
                                     "HEND" =>
                                       Integer
                                         ("Hysteresis end setting as described in the TMC2240 datasheet. This is " &
                                            "the resultant value from -3 to 12, not the raw register value.",
                                          Min => -3, Max => 12, Unit => "")])]),
                            "Constant off time" => Sequence
                              ("Constant off time chopper settings. No automatic tuning of these parameters is " &
                                 "available. Refer to the TMC2240 datasheet for details on tuning these values. " &
                                 "SpreadCycle in derived parameters mode should be used unless manual tuning with " &
                                 "an oscilloscope is being performed.",
                               ["DISFDCC" =>
                                 Boolean
                                   ("If set, disable the usage of the current comparator for termination of the " &
                                      "fast decay cycle. If not set then the fast decay cycle will be terminated " &
                                      "early if the negative current value exceeds the previous positive value."),
                                "OFFSET" =>
                                  Integer
                                    ("Sine wave offset as described in the TMC2240 datasheet. This is the " &
                                       "resultant value from -3 to 12, not the raw register value.",
                                     Min => -3, Max => 12, Unit => ""),
                                "TFD" =>
                                  Integer
                                    ("Fast decay time setting as described in the TMC2240 datasheet. This " &
                                       "parameter sets both FD3 and HSTRT_TFD210.",
                                     Min => 0, Max => 15, Unit => "")])]),
                        "StealthChop2 (EN_PWM_MODE)" => Variant
                          ("Enable or disable StealthChop2 for this driver.",
                           ["Disabled" =>
                             Sequence
                               ("StealthChop2 is disabled.",
                                []),
                            "Enabled" =>
                              Sequence
                                ("StealthChop2 settings.",
                                 ["TPWMTHRS" =>
                                   Integer
                                     ("Upper velocity limit for StealthChop2 mode measured in the number of TMC " &
                                        "clock cycles between step signals (1 cycle is typically approximately 80ns).",
                                      MIN => 0, MAX => 16#FFFFF#, UNIT => ""),
                                  "PWM_OFS" =>
                                    Integer
                                      ("Fixed part of StealthChop2 maximum PWM amplitude as described in TMC2240 " &
                                         "datasheet. Usually this should be left at the default value of 29 and " &
                                         "PWM_AUTOSCALE should be enabled.",
                                       Min => 0, Max => 255, Unit => ""),
                                  "PWM_GRAD" =>
                                    Integer
                                      ("Velocity dependent part of StealthChop2 maximum PWM described in TMC2240 " &
                                         "datasheet. Usually this should be left at the default value of 0 and " &
                                         "PWM_AUTOGRAD should be enabled.",
                                       Min => 0, Max => 255, Unit => ""),
                                  "PWM_FREQ" =>
                                    Discrete
                                      ("StealthChop2 PWM cycle duration. PWM cycle duration in TMC clock cycles " &
                                         "(typically approximately 80ns) = PWM_FREQ / 2. Usually a value of 683 " &
                                         "is a good default for a resultant frequency of 36kHz.",
                                       [for X in TMC_Types.TMC2240.PWM_Freq_Type => X'Image]),
                                  "PWM_AUTOSCALE" =>
                                    Boolean
                                      ("Enable automatic tuning of PWM_OFS. Set current limits may not be effective " &
                                         "if this is disabled!"),
                                  "PWM_AUTOGRAD" =>
                                    Boolean
                                      ("Enable automatic tuning of PWM_GRAD. Set current limits may not be " &
                                         "effective if this is disabled!"),
                                  "FREEWHEEL" =>
                                    Discrete
                                      ("StealthChop2 standstill freewheeling mode when IHOLD = 0.",
                                       [for X in TMC_Types.TMC2240.Freewheel_Type => X'Image]),
                                  "PWM_MEAS_SD_ENABLE" =>
                                    Boolean
                                      ("Use slow decay phase on low side to measure motor current when in " &
                                         "StealthChop2 mode."),
                                  "PWM_DIS_REG_STST" =>
                                    Boolean
                                      ("Disable StealthChop2 current regulation when in standstill and reduce the " &
                                         "duty cycle to a very low value."),
                                  "PWM_REG" =>
                                    Integer
                                      ("StealthChop2 maximum PWM auto-scaling change per half wave measured in half " &
                                         "increments, with 1 being 0.5 increments.",
                                       Min => 0, Max => 15, Unit => ""),
                                  "PWM_LIM" =>
                                    Integer
                                      ("StealthChop2 PWM auto-scaling amplitude limit when switching from " &
                                         "SpreadCycle to StealthChop2. Limits the upper 4 bits.",
                                       Min => 0, Max => 15, Unit => ""),
                                  "MULTISTEP_FILT" =>
                                    Boolean
                                      ("Some sort of undocumented filtering for StealthChop2. This should be left " &
                                         "off on official Prunt hardware as the generated step signals have " &
                                         "extremely low jitter.")])])])
                 else
                    raise Constraint_Error with "Config not implemented for stepper kind " & S)])];
      --!pp on
   end Build_Schema;

   function Schema_To_JSON (Schema : Property_Maps.Map) return String is
      --  String escape functionality can be added here if required.

      Result : Unbounded_String;

      procedure DFS (Node : Property_Maps.Map) is
         use Property_Maps;
         use Discrete_String_Sets;
      begin
         Append (Result, "{");
         for I in Node.Iterate loop
            Append (Result, """" & Key (I) & """:{""Description"":""" & Element (I).all.Description & ",");
            case Element (I).all.Kind is
               when Boolean_Kind =>
                  Append (Result, """Kind"":""Boolean""}");
               when Discrete_Kind =>
                  Append (Result, """Kind"":""Discrete"",""Options"":[");
                  declare
                     Options : Discrete_String_Sets.Set := Element (I).all.Discrete_Options;
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
                     """Kind"":""Integer""" &
                       ",""Min"":" & Trim (Element (I).all.Integer_Min'Image) &
                       ",""Max"":" & Trim (Element (I).all.Integer_Max'Image) &
                       ",""Unit"":" & Element (I).all.Integer_Unit'Image & "}");
               when Float_Kind =>
                  Append
                    (Result,
                     """Kind"":""Float""" &
                       ",""Min"":" & Trim (Element (I).all.Float_Min'Image) &
                       ",""Max"":" & Trim (Element (I).all.Float_Max'Image) &
                       ",""Unit"":" & Element (I).all.Float_Unit'Image & "}");
               when Sequence_Kind =>
                  Append (Result, """Kind"":""Sequence"",""Children"":");
                  DFS (Element (I).all.Children);
                  Append (Result, "}");
               when Variant_Kind =>
                  Append (Result, """Kind"":""Variant"",""Children"":");
                  DFS (Element (I).all.Children);
                  Append (Result, "}");
            end case;
            if Key (I) /= Last_Key (Schema) then
               Append (Result, ",");
            end if;
         end loop;
         Append (Result, "}");
      end DFS;
   begin
      DFS (Schema);
      return To_String (Result);
   end Schema_To_JSON;

   function Build_Flat_Schema return Flat_Schemas.Map is
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
               case Element (I).all.Kind is
                  when Boolean_Kind | Discrete_Kind | Integer_Kind | Float_Kind =>
                     Insert (Result, New_Path, Element (I).all);
                  when Sequence_Kind =>
                     DFS (Element (I).all.Children, New_Path);
                  when Variant_Kind =>
                     declare
                        Children : Property_Maps.Map := Element (I).all.Children;
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
                           (Kind             => Discrete_Kind,
                            Description      => To_Unbounded_String (""),
                            Discrete_Options => Options));
                        DFS (Children, New_Path);
                     end;
               end case;
            end;
         end loop;
      end DFS;
   begin
      DFS (Internal_Schema, "");
      return Result;
   end Build_Flat_Schema;

end Prunt.Config_JSON;
