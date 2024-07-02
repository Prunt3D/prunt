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

with Prunt.Motion_Planner;
with TOML;
with Ada.Strings.Bounded;
with Prunt.Thermistors; use Prunt.Thermistors;
with Prunt.Heaters;     use Prunt.Heaters;
with Prunt.TMC_Types;
with Prunt.TMC_Types.TMC2240;

--  Serialises and deserialises records to and from a TOML file.

generic
   type Stepper_Name is (<>);
   type Stepper_Kinds_Type is array (Stepper_Name) of Stepper_Kind;
   Stepper_Kinds : Stepper_Kinds_Type;
   type Heater_Name is (<>);
   type Thermistor_Name is (<>);
   type Fan_Name is (<>);
   type Input_Switch_Name is (<>);
   Config_Path : String;
package Prunt.Config is

   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 200);

   type Attached_Steppers is array (Stepper_Name) of Boolean;

   IO_Error                 : exception;
   Config_File_Format_Error : exception;

   type Prunt_Parameters is record
      Enabled : Boolean := False;
   end record;

   type Stepper_Parameters (Kind : Stepper_Kind := Basic_Kind) is record
      Enabled     : Boolean := False;
      Mm_Per_Step : Length  := Length'Last / 2.0;
      case Kind is
         when Basic_Kind =>
            null;
         when TMC2240_UART_Kind =>
            --  GLOBALSCALER and DRVCONF:
            Output_Current       : Current                                     := 0.15 * amp;
            Slope_Control        : TMC_Types.TMC2240.Slope_Control_Type        := TMC_Types.TMC2240.Slope_100V_Per_us;
            --  IHOLDIRUN:
            I_Hold               : TMC_Types.Unsigned_5                        := 16;
            I_Run                : TMC_Types.Unsigned_5                        := 31;
            I_Hold_Delay         : TMC_Types.Unsigned_4                        := 1;
            I_Run_Delay          : TMC_Types.Unsigned_4                        := 4;
            --  TPOWERDOWN:
            T_Power_Down         : TMC_Types.Unsigned_8                        := 10;
            --  TPWMTHRS:
            T_PWM_Thrs           : TMC_Types.Unsigned_20                       := 0;
            --  TCOOLTHRS:
            T_Cool_Thrs          : TMC_Types.Unsigned_20                       := 0;
            --  THIGH:
            T_High               : TMC_Types.Unsigned_20                       := 0;
            --  CHOPCONF:
            TOFF                 : TMC_Types.Unsigned_4                        := 3;
            HSTRT_TFD210         : TMC_Types.Unsigned_3                        := 5;
            HEND_OFFSET          : TMC_Types.Unsigned_4                        := 2;
            FD3                  : TMC_Types.Unsigned_1                        := 0;
            DISFDCC              : Boolean                                     := False;
            CHM                  : Boolean                                     := False;
            VHIGHFS              : Boolean                                     := False;
            VHIGHCHM             : Boolean                                     := False;
            TPFD                 : TMC_Types.Unsigned_4                        := 4;
            Microstep_Resolution : TMC_Types.TMC2240.Microstep_Resolution_Type := TMC_Types.TMC2240.MS_256;
            --  PWMCONF:
            PWM_OFS              : TMC_Types.Unsigned_8                        := 29;
            PWM_Grad             : TMC_Types.Unsigned_8                        := 0;
            PWM_Freq             : TMC_Types.Unsigned_2                        := 0;
            PWM_Auto_Scale       : Boolean                                     := True;
            PWM_Auto_Grad        : Boolean                                     := True;
            Freewheel            : TMC_Types.TMC2240.Freewheel_Type            := TMC_Types.TMC2240.Normal;
            PWM_Meas_SD_Enable   : Boolean                                     := False;
            PWM_Dis_Reg_Stst     : Boolean                                     := False;
            PWM_Reg              : TMC_Types.Unsigned_4                        := 4;
            PWM_Lim              : TMC_Types.Unsigned_4                        := 12;
      end case;
   end record;

   type Kinematics_Kind is (Cartesian_Kind, Core_XY_Kind);

   type Kinematics_Parameters (Kind : Kinematics_Kind := Cartesian_Kind) is record
      Planner_Parameters : Motion_Planner.Kinematic_Parameters := (others => <>);
      Z_Steppers         : Attached_Steppers                   := [others => False];
      E_Steppers         : Attached_Steppers                   := [others => False];
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

   type Homing_Kind is (Double_Tap_Kind, Set_To_Value_Kind);

   type Homing_Parameters (Kind : Homing_Kind := Double_Tap_Kind) is record
      case Kind is
         when Double_Tap_Kind =>
            Switch                 : Input_Switch_Name := Input_Switch_Name'First;
            First_Move_Distance    : Length            := 0.0 * mm;
            Back_Off_Move_Distance : Length            := 0.0 * mm;
            Second_Move_Distance   : Length            := 0.0 * mm;
            Switch_Position        : Length            := 0.0 * mm;
         when Set_To_Value_Kind =>
            Value : Length := 0.0 * mm;
      end case;
   end record;

   type Extruder_Parameters is record
      Nozzle_Diameter   : Length := 0.4 * mm;
      Filament_Diameter : Length := 1.75 * mm;
   end record;

   --  Thermistor_Parameters in Prunt.Thermistors.

   type Heater_Full_Parameters is record
      Thermistor : Thermistor_Name := Thermistor_Name'First;
      Params     : Heaters.Heater_Parameters;
   end record;

   type Bed_Mesh_Kind is (No_Mesh_Kind, Beacon_Kind);

   type Bed_Mesh_Parameters (Kind : Bed_Mesh_Kind := No_Mesh_Kind) is record
      case Kind is
         when No_Mesh_Kind =>
            null;
         when Beacon_Kind =>
            Serial_Port_Path     : Path_Strings.Bounded_String := Path_Strings.To_Bounded_String ("");
            X_Offset             : Length                      := 0.0 * mm;
            Y_Offset             : Length                      := 0.0 * mm;
            Calibration_Floor    : Length                      := 0.2 * mm;
            Calibration_Ceiling  : Length                      := 5.0 * mm;
            Calibration_Feedrate : Velocity                    := 1.0 * mm / s;
      end case;
   end record;

   type Fan_Kind is (Disabled_Kind, Dynamic_PWM_Kind, Always_On_Kind);

   type Fan_Parameters (Kind : Fan_Kind := Disabled_Kind) is record
      case Kind is
         when Disabled_Kind =>
            null;
         when Dynamic_PWM_Kind =>
            Disable_Below_PWM : PWM_Scale := 0.5;
            Max_PWM           : PWM_Scale := 1.0;
         when Always_On_Kind =>
            Always_On_PWM : PWM_Scale := 1.0;
      end case;
   end record;

   type G_Code_Assignment_Parameters is record
      Bed_Heater    : Heater_Name := Heater_Name'First;
      --  Chamber_Heater : Heater_Name := Heater_Name'First;
      Hotend_Heater : Heater_Name := Heater_Name'First;
   end record;

   protected Config_File is
      procedure Read (Data : out Prunt_Parameters);
      procedure Write (Data : Prunt_Parameters; Append_Only : Boolean := False);
      procedure Read (Data : out Stepper_Parameters; Stepper : Stepper_Name) with
        Post => Data.Kind = Stepper_Kinds (Stepper);
      procedure Write (Data : Stepper_Parameters; Stepper : Stepper_Name; Append_Only : Boolean := False) with
        Pre => Data.Kind = Stepper_Kinds (Stepper);
      procedure Read (Data : out Kinematics_Parameters);
      procedure Write (Data : Kinematics_Parameters; Append_Only : Boolean := False);
      procedure Read (Data : out Input_Switch_Parameters; Input_Switch : Input_Switch_Name);
      procedure Write
        (Data : Input_Switch_Parameters; Input_Switch : Input_Switch_Name; Append_Only : Boolean := False);
      procedure Read (Data : out Homing_Parameters; Axis : Axis_Name);
      procedure Write (Data : Homing_Parameters; Axis : Axis_Name; Append_Only : Boolean := False);
      procedure Read (Data : out Extruder_Parameters);
      procedure Write (Data : Extruder_Parameters; Append_Only : Boolean := False);
      procedure Read (Data : out Thermistor_Parameters; Thermistor : Thermistor_Name);
      procedure Write (Data : Thermistor_Parameters; Thermistor : Thermistor_Name; Append_Only : Boolean := False);
      procedure Read (Data : out Heater_Full_Parameters; Heater : Heater_Name);
      procedure Write (Data : Heater_Full_Parameters; Heater : Heater_Name; Append_Only : Boolean := False);
      procedure Read (Data : out Bed_Mesh_Parameters);
      procedure Write (Data : Bed_Mesh_Parameters; Append_Only : Boolean := False);
      procedure Read (Data : out Fan_Parameters; Fan : Fan_Name);
      procedure Write (Data : Fan_Parameters; Fan : Fan_Name; Append_Only : Boolean := False);
      procedure Read (Data : out G_Code_Assignment_Parameters);
      procedure Write (Data : G_Code_Assignment_Parameters; Append_Only : Boolean := False);
   private
      procedure Maybe_Read_File;
      procedure Write_File;
      File_Read : Boolean         := False;
      TOML_Data : TOML.TOML_Value := TOML.No_TOML_Value;
   end Config_File;

end Prunt.Config;
