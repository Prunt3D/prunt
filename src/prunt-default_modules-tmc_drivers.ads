-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
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

pragma Extensions_Allowed (On);

with Ada.Tags;
with Prunt.Config;
with Prunt.Controller_Generic_Types;
with Prunt.Default_Modules.Motor_Drivers;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types;      use Prunt.Module_Types;
with Prunt.Status_Manager;
with Prunt.TMC_Types;         use Prunt.TMC_Types;
with Prunt.TMC_Types.TMC2240; use Prunt.TMC_Types.TMC2240;

generic
   with package My_Controller_Generic_Types is new Controller_Generic_Types (<>);
   Motor_Hardware : My_Controller_Generic_Types.Motor_Hardware_Parameters_Array_Type;
   with package Motor_Drivers_Module is new
     Default_Modules.Motor_Drivers (Motor_Name => My_Controller_Generic_Types.Motor_Name);
package Prunt.Default_Modules.TMC_Drivers is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   type Module_Instance (<>) is new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   procedure Start (This : in out Module_Instance);

private

   type User_Config_TMC2240_SpreadCycle_Derived is record
      --  This option automatically calculates the optimal settings for the SpreadCycle chopper, which is a feature of
      --  Trinamic drivers that provides smooth and quiet motor operation. This is the recommended mode for most users
      --  when the phase inductance and resistance are known, these parameters may be found in your stepper motor's
      --  datasheet. If these parameters can not be found then the default parameters in the 'Manual' mode may be used.

      Input_Voltage : Voltage range 6.5 * volt .. 40.0 * volt := 24.0 * volt;
      --  This is the voltage supplied to the stepper motor driver, not the voltage rating of the stepper motor itself.
      --  It's crucial that this value is accurate. At startup, Prunt will measure the actual driver voltage and if it
      --  differs from this setting by more than 10%, it will generate an error.

      Phase_Inductance : Inductance range 0.000_000_1 * millihenry .. 1_000_000.0 * millihenry :=
        0.000_000_1 * millihenry;
      --  This is the inductance of each of the motor's coils. You can find this value in your stepper motor's
      --  datasheet. Make sure to check that the units in the datasheet are the same as the units here.

      Phase_Resistance : Resistance range 0.000_000_1 * ohm .. 1_000_000.0 * ohm := 0.000_000_1 * ohm;
      --  This is the resistance of each of the motor's coils. You can find this value in your stepper motor's
      --  datasheet.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_TMC2240_SpreadCycle_Manual is record
      --  This section allows for manual configuration of the SpreadCycle chopper. These settings should only be
      --  adjusted by advanced users who are familiar with Trinamic driver tuning and have access to an oscilloscope to
      --  monitor the motor's current waveform. Incorrectly configured settings can lead to poor motor performance,
      --  excessive noise, and overheating. For most users, the 'Derived' mode is strongly recommended. If the
      --  parameters for the 'Derived' mode can not be found then the default values specified below may be used.

      HSTRT : User_Config_Integer range 1 .. 8 := 6;
      --  This setting determines the start of the hysteresis for the SpreadCycle chopper. Refer to the
      --  TMC2240 datasheet for a detailed explanation. The value here is the resultant value (1 to 8),
      --  not the raw register value. The default is 6.

      HEND : User_Config_Integer range -3 .. 12 := -1;
      --  This setting determines the end of the hysteresis for the SpreadCycle chopper. Refer to the TMC2240 datasheet
      --  for a detailed explanation. The value here is the resultant value (-3 to 12), not the raw register value. The
      --  default is -1.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_TMC2240_SpreadCycle_Kind is (Derived, Manual);

   type User_Config_TMC2240_SpreadCycle (Kind : User_Config_TMC2240_SpreadCycle_Kind := Derived) is record
      --  This section allows you to choose between automatically calculated ('Derived') or manually configured
      --  SpreadCycle settings. For the vast majority of users, 'Derived' is the best choice. Only select 'Manual' if
      --  you are familiar with the tuning process for TMC2240 stepper drivers or if the parameters required for the
      --  'Derived' mode can not be found.

      case Kind is
         when Derived =>
            Derived : User_Config_TMC2240_SpreadCycle_Derived;

         when Manual =>
            Manual : User_Config_TMC2240_SpreadCycle_Manual;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_TMC2240_Constant_Off_Time is record
      --  This option allows for configuration of the Constant Off-Time chopper mode. There is no automatic tuning for
      --  these parameters. It is strongly recommended to use the SpreadCycle chopper unless you have a specific reason
      --  to use this mode.

      DISFDCC : Boolean := False;
      --  This setting disables the use of the current comparator to terminate the fast decay cycle. When enabled, the
      --  fast decay cycle will end early if the negative current exceeds the previous positive current value. Refer to
      --  the TMC2240 datasheet for more details.

      OFFSET : User_Config_Integer range -3 .. 10 := -1;
      --  This setting adjusts the sine wave offset. Refer to the TMC2240 datasheet for a detailed explanation. The
      --  value here is the resultant value (-3 to 12), not the raw register value.

      TFD : User_Config_Integer range 0 .. 15 := 5;
      --  This setting controls the fast decay time. It sets both the FD3 and HSTRT_TFD210 bits in the CHOPCONF
      --  register. Refer to the TMC2240 datasheet for more details.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_TMC2240_CHM_Kind is (SpreadCycle, Constant_Off_Time);

   type User_Config_TMC2240_CHM (Kind : User_Config_TMC2240_CHM_Kind := SpreadCycle) is record
      --  This setting selects the chopper mode for the stepper motor driver. The chopper is responsible for
      --  controlling the current to the motor coils, which in turn affects the motor's performance, noise level, and
      --  heat generation.
      --
      --  SpreadCycle is the recommended mode for most applications. Constant Off-Time is a more traditional chopper
      --  mode that may be useful in some specific situations, but it generally worse than SpreadCycle.
      --
      --  This setting is used when StealthChop2 is disabled, or when the motor's velocity exceeds the TPWMTHRS limit
      --  for StealthChop2. The VHIGHCHM setting can also override this when the velocity exceeds the VHIGH limit.

      case Kind is
         when SpreadCycle =>
            SpreadCycle : User_Config_TMC2240_SpreadCycle;

         when Constant_Off_Time =>
            Constant_Off_Time : User_Config_TMC2240_Constant_Off_Time;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_TMC2240_StealthChop2_Enabled is record
      --  This section contains the settings for StealthChop2. For most users, the settings, aside from TPWMTHRS,
      --  should always be set to the default values with auto-tuning enabled.

      TPWMTHRS : Velocity range 0.0 * mm / s .. 1.0E100 * mm / s := 1.0E100 * mm / s;
      --  This is the velocity threshold at which the driver will switch from StealthChop2 to the chopper mode selected
      --  in the CHM section (usually SpreadCycle). This allows for quiet operation at low speeds and high-torque
      --  operation at high speeds. The default value of 1E100 will cause StealthChop2 to always be active.

      PWM_OFS : TMC_Types.Unsigned_8 := 29;
      --  This setting defines the fixed part of the maximum PWM amplitude in StealthChop2. It's generally recommended
      --  to leave this at the default value of 29 and enable PWM_AUTOSCALE to allow the driver to automatically tune
      --  this value.

      PWM_GRAD : TMC_Types.Unsigned_8 := 0;
      --  This setting defines the velocity-dependent part of the maximum PWM amplitude in StealthChop2. It's
      --  recommended to leave this at the default value of 0 and enable PWM_AUTOGRAD to allow the driver to
      --  automatically tune this value.

      PWM_FREQ : TMC_Types.TMC2240.PWM_Freq_Type := Freq_683;
      --  This setting determines the PWM frequency for StealthChop2. The duration is measured in half TMC clock cycles
      --  (a half cycle is typically 40ns). A value of 683, resulting in a frequency of approximately 36kHz, is a good
      --  starting point for most motors.

      PWM_AUTOSCALE : Boolean := True;
      --  This enables automatic tuning of the PWM_OFS value. It is highly recommended to keep this enabled, as
      --  disabling it cause current limits to not be correctly applied, potentially resulting in destruction of the
      --  motor.

      PWM_AUTOGRAD : Boolean := True;
      --  This enables automatic tuning of the PWM_GRAD value. It is highly recommended to keep this enabled, as
      --  disabling it cause current limits to not be correctly applied, potentially resulting in destruction of the
      --  motor.

      FREEWHEEL : TMC_Types.TMC2240.Freewheel_Type := Normal;
      --  This setting determines the behavior of the motor coils when the motor is at a standstill and the hold
      --  current (IHOLD) is set to 0. Refer to the TMC2240 datasheet for a detailed explanation of the different
      --  freewheeling options. This setting is not relevant for most users.

      PWM_MEAS_SD_ENABLE : Boolean := False;
      --  This setting enables the use of the slow decay phase on the low side of the H-bridge to measure the motor
      --  current while in StealthChop2 mode.

      PWM_DIS_REG_STST : Boolean := False;
      --  This setting disables StealthChop2 current regulation when the motor is at a standstill, instead the duty
      --  cycle is reduced to a very low value.

      PWM_REG : TMC_Types.Unsigned_4 := 4;
      --  This setting controls the rate of change for the automatic PWM amplitude scaling in StealthChop2. The value
      --  is measured in half increments, so a value of 4 corresponds to 2 increments per half wave.

      PWM_LIM : TMC_Types.Unsigned_4 := 12;
      --  This setting limits the amplitude of the automatic PWM scaling when the driver switches from SpreadCycle to
      --  StealthChop2. It limits the upper 4 bits of the PWM value.

      MULTISTEP_FILT : Boolean := False;
      --  This is a filtering feature for StealthChop2 with an undocumented implementation. On official Prunt hardware,
      --  the generated step signals have extremely low jitter, so this filtering is not necessary and should be left
      --  disabled.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_TMC2240_StealthChop2_Kind is (Disabled, Enabled);

   type User_Config_TMC2240_StealthChop2 (Kind : User_Config_TMC2240_StealthChop2_Kind := Disabled) is record
      --  This section allows you to enable or disable StealthChop2 for this driver. StealthChop2 is a feature of
      --  TMC2240 stepper drivers that allows for extremely quiet stepper motor operation, especially at low speeds.
      --  While it can make your printer nearly silent, it may also have less torque than SpreadCycle, especially at
      --  higher speeds.

      case Kind is
         when Disabled =>
            Empty : User_Config_Empty;

         when Enabled =>
            Parameters : User_Config_TMC2240_StealthChop2_Enabled;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_TMC2240 is record
      Run_Current : Current range 0.125 * amp .. 3.0 * amp := 0.125 * amp;
      --  This is the peak current that will be supplied to each coil of the stepper motor. Consult your stepper
      --  motor's datasheet for the recommended current rating. Prunt will automatically select the smallest suitable
      --  current range on the driver and use the GLOBALSCALER register to fine-tune the current.

      IHOLD : Dimensionless range 0.03125 .. 1.0 := 1.0;
      --  This setting determines the percentage of the run current that is used when the motor is not moving. A value
      --  of 1.0 corresponds to 100% of the run current, while a value of 0.0 corresponds to 0%. The value has a
      --  resolution of 1/32, but any value may be enetered here as it is automatically rounded.

      IRUN : Dimensionless range 0.03125 .. 1.0 := 1.0;
      --  This setting determines the percentage of the run current that is used when the motor is moving.  A value of
      --  1.0 corresponds to 100% of the run current, while a value of 0.0 corresponds to 0%.  The value has a
      --  resolution of 1/32, but any value may be enetered here as it is automatically rounded.

      IRUN_During_Homing : Dimensionless range 0.03125 .. 1.0 := 1.0;
      --  This setting allows you to specify a different IRUN value to be used during homing moves. This can be useful
      --  for sensorless homing, where a lower current may be needed to reliably detect a stall. The value has a
      --  resolution of 1/32, but any value may be enetered here as it is automatically rounded.

      IHOLDDELAY : Time range 0.0 * ms .. 315.0 * ms := 315.0 * ms;
      --  This is the time it takes for the driver to reduce the current from the run level to the hold level after the
      --  stepper driver detects the motor has stopped (see FAST_STANDSTILL below) and after TPOWERDOWN (specified
      --  below). The resolution of this setting is 21ms, but any value may be enetered here as it is automatically
      --  rounded.

      IRUNDELAY : Time range 0.0 * ms .. 0.615 * ms := 0.0 * ms;
      --  This is the time it takes for the driver to increase the current from the hold level to the run level when
      --  the motor starts moving again. The resolution of this setting is 0.041ms, but any value may be enetered here
      --  as it is automatically rounded.

      TPOWERDOWN : Time range 0.0 * ms .. 5355.0 * ms := 5_355.0 * ms;
      --  This is the delay after the motor stops before the driver begins to reduce the current to the hold level (as
      --  defined by IHOLD). A minimum value of 42ms is required for the automatic tuning of StealthChop2 to work
      --  correctly. The resolution of this setting is 21ms, but any value may be enetered here as it is automatically
      --  rounded.

      THIGH : Velocity range 0.0 * mm / s .. 1.0E100 * mm / s := 1.0E100 * mm / s;
      --  This velocity threshold is used to switch the driver into a high-velocity mode controlled by VHIGHFS and
      --  VHIGHCHM. It also serves as an upper velocity limit for features CoolStep and StealthChop2, if they are
      --  enabled.

      SLOPE_CONTROL : TMC_Types.TMC2240.Slope_Control_Type := Slope_400V_Per_us;
      --  This setting controls the slew rate of the driver's output. A higher slew rate can reduce power dissipation,
      --  but may also increase electromagnetic interference (EMI). 400V/us is usually a good setting.

      TOFF : TMC_Types.TMC2240.TOFF_Type := Off_120;
      --  This setting determines the duration of the slow decay (off-time) phase of the chopper cycle, measured in TMC
      --  clock cycles (typically 80ns). A value of 120, combined with a TBL setting of 36, is a good starting point
      --  that results in a theoretical maximum chopper frequency of around 40kHz.

      TBL : TMC_Types.TMC2240.TBL_Type := Blank_36;
      --  This setting determines the blanking time for the current sense comparators, measured in TMC clock cycles
      --  (typically 80ns).

      VHIGHFS : Boolean := False;
      --  When this setting is enabled, the driver will switch to full-step mode (disabling microstepping) when the
      --  velocity exceeds the THIGH threshold.

      VHIGHCHM : Boolean := False;
      --  When this setting is enabled, the driver will switch to the constant off-time chopper mode, set TFD to 0, and
      --  approximately double the TOFF time when the velocity exceeds the THIGH threshold.

      TPFD : TMC_Types.Unsigned_4 := 4;
      --  This setting determines the duration of the passive fast decay phase after a change in the bridge polarity.
      --  The duration, in TMC clock cycles (typically 80ns), is 128 multiplied by this value.

      MRES : TMC_Types.TMC2240.Microstep_Resolution_Type := MS_256;
      --  This setting determines the microstep resolution for the driver. Higher microstep resolutions result in
      --  smoother and quieter motor operation, but can limit the maximum achievable speed. 256 microsteps is the
      --  highest resolution available and is generally recommended as Prunt will slow down and emit a warning when the
      --  maximum step rate would be exceeded by a G-code command rather than halting.

      FAST_STANDSTILL : Boolean := False;
      --  When enabled, the driver will wait for a shorter period of time (2^18 TMC clock cycles, typically 21ms) after
      --  the last step signal before it begins to detect a standstill. When disabled, the wait time is 2^20 cycles
      --  (typically 84ms).

      CHM : User_Config_TMC2240_CHM := (others => <>);

      StealthChop2 : User_Config_TMC2240_StealthChop2 := (others => <>);
   end record
   with Annotate => (Prunt_Config, User_Config);

   --  TODO: We need to add support for `when others =>` in the configuration code generator so we don't need to edit
   --  the record below this every time we add a new motor type.

   type User_Config_Motor (Fixed_Kind : Motor_Hardware_Kind := Basic_Motor_Kind) is record
      case Fixed_Kind is
         when TMC2240_UART_Kind =>
            TMC2240_Parameters : User_Config_TMC2240;

         when Basic_Motor_Kind =>
            Empty : User_Config_Empty;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motor_Array is array (My_Controller_Generic_Types.Motor_Name) of User_Config_Motor
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Motors : User_Config_Motor_Array := [others => <>]with
        Annotate => (Prunt_Config, Fixed_Kind, "Motor_Hardware (Index_?).Kind");
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config);

   type Module_Instance is new My_Modules.Module_Instance with record
      Config : User_Config;
   end record;

end Prunt.Default_Modules.TMC_Drivers;
