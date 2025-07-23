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

with System; use System;

package Prunt.TMC_Types.TMC2240 is

   type UART_Node_Address is range 0 .. 2**8 - 1 with Size => 8;
   type UART_Sync_Nibble is range 0 .. 2**4 - 1 with Size => 4;
   type UART_CRC is mod 2**8 with Size => 8;

   type GCONF is record
      Reserved_1       : Unsigned_1;
      Fast_Standstill  : TMC_Boolean;
      En_PWM_Mode      : TMC_Boolean;
      Multistep_Filt   : TMC_Boolean;
      Invert_Direction : TMC_Boolean;
      Diag0_Error      : TMC_Boolean;
      Diag0_OTPW       : TMC_Boolean;
      Diag0_Stall      : TMC_Boolean;
      Diag1_Stall      : TMC_Boolean;
      Diag1_Index      : TMC_Boolean;
      Diag1_On_State   : TMC_Boolean;
      Reserved_2       : Unsigned_1;
      Diag_0_Push_Pull : TMC_Boolean;
      Diag_1_Push_Pull : TMC_Boolean;
      Small_Hysteresis : TMC_Boolean;
      Stop_Enable      : TMC_Boolean;
      Direct_Mode      : TMC_Boolean;
      Reserved_3       : Unsigned_15;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type GSTAT is record
      Reset          : TMC_Boolean;
      Drv_Err        : TMC_Boolean;
      UV_CP          : TMC_Boolean;
      Register_Reset : TMC_Boolean;
      VM_UVLO        : TMC_Boolean;
      Reserved       : Unsigned_27;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type IFCNT is record
      If_Cnt   : Unsigned_8;
      Reserved : Unsigned_24;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type Send_Delay_Type is (Delay_1x8, Delay_3x8, Delay_5x8, Delay_7x8, Delay_9x8, Delay_11x8, Delay_13x8, Delay_15x8)
   with Size => 4;
   for Send_Delay_Type use
     (Delay_1x8  => 16#0#,
      Delay_3x8  => 16#2#,
      Delay_5x8  => 16#4#,
      Delay_7x8  => 16#6#,
      Delay_9x8  => 16#8#,
      Delay_11x8 => 16#A#,
      Delay_13x8 => 16#C#,
      Delay_15x8 => 16#E#);

   type NODECONF is record
      Node_Addr  : UART_Node_Address;
      Send_Delay : Send_Delay_Type;
      Reserved   : Unsigned_20;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type IOIN is record
      Step        : TMC_Boolean;
      Dir         : TMC_Boolean;
      Enc_B       : TMC_Boolean;
      Enc_A       : TMC_Boolean;
      Drv_Enn     : TMC_Boolean;
      Enc_N       : TMC_Boolean;
      UART_En     : TMC_Boolean;
      Reserved_1  : Unsigned_1;
      Comp_A      : TMC_Boolean;
      Comp_B      : TMC_Boolean;
      Comp_B1_B2  : TMC_Boolean;
      Comp_A1_A2  : TMC_Boolean;
      Output      : TMC_Boolean;
      Ext_Res_Det : TMC_Boolean;
      Ext_Clk     : TMC_Boolean;
      ADC_Err     : TMC_Boolean;
      Silicon_RV  : Unsigned_3;
      Reserved_2  : Unsigned_5;
      Version     : Unsigned_8;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type Slope_Control_Type is (Slope_100V_Per_us, Slope_200V_Per_us, Slope_400V_Per_us, Slope_800V_Per_us)
   with Size => 2;
   for Slope_Control_Type use
     (Slope_100V_Per_us => 0, Slope_200V_Per_us => 1, Slope_400V_Per_us => 2, Slope_800V_Per_us => 3);

   type Current_Range_Type is (Max_1A, Max_2A, Max_3A) with Size => 2;
   for Current_Range_Type use (Max_1A => 0, Max_2A => 1, Max_3A => 2);

   type DRV_CONF is record
      Current_Range : Current_Range_Type;
      Reserved_1    : Unsigned_2;
      Slope_Control : Slope_Control_Type;
      Reserved_2    : Unsigned_26;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type Global_Scaler_Type is range 0 .. 255
   with Size => 8, Static_Predicate => Global_Scaler_Type = 0 or Global_Scaler_Type > 31;

   type GLOBAL_SCALER is record
      Global_Scaler : Global_Scaler_Type;
      Reserved      : Unsigned_24;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type IHOLD_IRUN is record
      I_Hold       : Unsigned_5;
      Reserved_1   : Unsigned_3;
      I_Run        : Unsigned_5;
      Reserved_2   : Unsigned_3;
      I_Hold_Delay : Unsigned_4;
      Reserved_3   : Unsigned_4;
      I_Run_Delay  : Unsigned_4;
      Reserved_4   : Unsigned_4;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type TPOWERDOWN is record
      T_Power_Down : Unsigned_8;
      Reserved     : Unsigned_24;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type TSTEP is record
      T_Step   : Unsigned_20;
      Reserved : Unsigned_12;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type TPWMTHRS is record
      T_PWM_Thrs : Unsigned_20;
      Reserved   : Unsigned_12;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type TCOOLTHRS is record
      T_Cool_Thrs : Unsigned_20;
      Reserved    : Unsigned_12;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type THIGH is record
      T_High   : Unsigned_20;
      Reserved : Unsigned_12;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type DIRECT_MODE is record
      Direct_Coil_A : Unsigned_9;
      Reserved_1    : Unsigned_7;
      Direct_Coil_B : Unsigned_9;
      Reserved_2    : Unsigned_7;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type ENCMODE is record
      Pol_A           : TMC_Boolean;
      Pol_B           : TMC_Boolean;
      Pol_N           : TMC_Boolean;
      Ignore_AB       : TMC_Boolean;
      Clr_Cont        : TMC_Boolean;
      Clr_Once        : TMC_Boolean;
      Pos_Neg_Edge    : Unsigned_2;
      Clr_Enc_X       : TMC_Boolean;
      Reserved_1      : Unsigned_1;
      Enc_Sel_Decimal : TMC_Boolean;
      Reserved_2      : Unsigned_21;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type X_ENC is record
      X_Enc : Unsigned_32;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type ENC_CONST is record
      Enc_Const : Unsigned_32;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type ENC_STATUS is record
      N_Event  : TMC_Boolean;
      Reserved : Unsigned_31;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type ENC_LATCH is record
      Enc_Latch : Unsigned_32;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type ADC_V_Supply_Volts_Type is delta 0.009_732 range 0.0 .. 0.009_732 * (2**13 - 1)
   with Size => 13, Small => 0.009_732;
   type ADC_AIN_Volts_Type is delta 0.000_305_2 range 0.0 .. 0.000_305_2 * (2**13 - 1)
   with Size => 13, Small => 0.000_305_2;

   type ADC_VSUPPLY_AIN is record
      ADC_V_Supply : ADC_V_Supply_Volts_Type;
      Reserved_1   : Unsigned_3;
      ADC_AIN      : ADC_AIN_Volts_Type;
      Reserved_2   : Unsigned_3;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type ADC_Temp_Almost_Kelvin_Type is delta 10.0 / 77.0 range 0.0 .. 10.0 / 77.0 * (2.0**13 - 1.0)
   with Size => 13, Small => 10.0 / 77.0;
   --  This range starts at −2038.0/(77.0/10.0) celsius (approx -264), but GNAT will not generate a biased
   --  representation for delta types, so we can not represent this in kelvin.

   type ADC_TEMP is record
      ADC_Temp : ADC_Temp_Almost_Kelvin_Type;
      Reserved : Unsigned_19;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type OTW_OV_VTH is record
      OverVoltage_VTH        : ADC_V_Supply_Volts_Type;
      Reserved_1             : Unsigned_3;
      OverTempPreWarning_VTH : ADC_Temp_Almost_Kelvin_Type;
      Reserved_2             : Unsigned_3;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type MSLUT_Values_Type is array (1 .. 32) of Unsigned_1 with Pack, Size => 32;
   type MSLUT is record
      Values : MSLUT_Values_Type;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type MSLUTSEL is record
      W0 : Unsigned_2;
      W1 : Unsigned_2;
      W2 : Unsigned_2;
      W3 : Unsigned_2;
      X1 : Unsigned_8;
      X2 : Unsigned_8;
      X3 : Unsigned_8;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type MSLUTSTART is record
      --  The below values are all two's complement, but there's no way to guarantee that we are on a two's complement
      --  system.
      Start_Sin    : Unsigned_8;
      Reserved_1   : Unsigned_8;
      Start_Sin90  : Unsigned_8;
      Offset_Sin90 : Unsigned_8;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type MSCNT is record
      Count    : Unsigned_10;
      Reserved : Unsigned_22;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type MSCURACT is record
      Cur_B      : Unsigned_9;
      Reserved_1 : Unsigned_7;
      Cur_A      : Unsigned_9;
      Reserved_2 : Unsigned_7;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type Microstep_Resolution_Type is (MS_256, MS_128, MS_64, MS_32, MS_16, MS_8, MS_4, MS_2, MS_Full_Steps)
   with Size => 4;
   for Microstep_Resolution_Type use
     (MS_256        => 0,
      MS_128        => 1,
      MS_64         => 2,
      MS_32         => 3,
      MS_16         => 4,
      MS_8          => 5,
      MS_4          => 6,
      MS_2          => 7,
      MS_Full_Steps => 8);

   type CHM_Type is (SpreadCycle_Mode, Constant_Off_Time_Mode) with Size => 1;
   for CHM_Type use (SpreadCycle_Mode => 0, Constant_Off_Time_Mode => 1);

   type TOFF_Type is
     (Disable_Driver,
      Off_56,
      Off_88,
      Off_120,
      Off_152,
      Off_184,
      Off_216,
      Off_248,
      Off_280,
      Off_312,
      Off_344,
      Off_376,
      Off_408,
      Off_440,
      Off_472,
      Off_504)
   with Size => 4;
   for TOFF_Type use
     (Disable_Driver => 0,
      Off_56         => 1,
      Off_88         => 2,
      Off_120        => 3,
      Off_152        => 4,
      Off_184        => 5,
      Off_216        => 6,
      Off_248        => 7,
      Off_280        => 8,
      Off_312        => 9,
      Off_344        => 10,
      Off_376        => 11,
      Off_408        => 12,
      Off_440        => 13,
      Off_472        => 14,
      Off_504        => 15);

   type TBL_Type is (Blank_16, Blank_24, Blank_36, Blank_54) with Size => 2;
   for TBL_Type use (Blank_16 => 0, Blank_24 => 1, Blank_36 => 2, Blank_54 => 3);

   type CHOPCONF is record
      TOFF                 : TOFF_Type;
      HSTRT_TFD210         : Unsigned_3;
      HEND_OFFSET          : Unsigned_4;
      FD3                  : Unsigned_1;
      DISFDCC              : TMC_Boolean;
      Reserved_1           : Unsigned_1;
      CHM                  : CHM_Type;
      TBL                  : TBL_Type;
      Reserved_2           : Unsigned_1;
      VHIGHFS              : TMC_Boolean;
      VHIGHCHM             : TMC_Boolean;
      TPFD                 : Unsigned_4;
      Microstep_Resolution : Microstep_Resolution_Type;
      Interpolate          : TMC_Boolean;
      Double_Edge          : TMC_Boolean;
      Disable_S2G          : TMC_Boolean;
      Disable_S2Vs         : TMC_Boolean;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type COOLCONF is record
      SEMIN      : Unsigned_4;
      Reserved_1 : Unsigned_1;
      SEUP       : Unsigned_2;
      Reserved_2 : Unsigned_1;
      SEMAX      : Unsigned_4;
      Reserved_3 : Unsigned_1;
      SEDN       : Unsigned_2;
      SEIMIN     : Unsigned_1;
      SGT        : Unsigned_7;
      SFILT      : TMC_Boolean;
      Reserved_4 : Unsigned_7;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type DRV_STATUS is record
      SG_Result  : Unsigned_10;
      Reserved_1 : Unsigned_2;
      S2VSA      : TMC_Boolean;
      S2VSB      : TMC_Boolean;
      Stealth    : TMC_Boolean;
      FSActive   : TMC_Boolean;
      CS_Actual  : Unsigned_5;
      Reserved_2 : Unsigned_3;
      StallGuard : TMC_Boolean;
      OT         : TMC_Boolean;
      OTPW       : TMC_Boolean;
      S2GA       : TMC_Boolean;
      S2GB       : TMC_Boolean;
      OLA        : TMC_Boolean;
      OLB        : TMC_Boolean;
      STST       : TMC_Boolean;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type Freewheel_Type is (Normal, Freewheel, Short_Via_LS, Short_Via_HS) with Size => 2;
   for Freewheel_Type use (Normal => 0, Freewheel => 1, Short_Via_LS => 2, Short_Via_HS => 3);

   type PWM_Freq_Type is (Freq_1024, Freq_683, Freq_512, Freq_410) with Size => 2;
   for PWM_Freq_Type use (Freq_1024 => 0, Freq_683 => 1, Freq_512 => 2, Freq_410 => 3);

   type PWMCONF is record
      PWM_OFS            : Unsigned_8;
      PWM_Grad           : Unsigned_8;
      PWM_Freq           : PWM_Freq_Type;
      PWM_Auto_Scale     : TMC_Boolean;
      PWM_Auto_Grad      : TMC_Boolean;
      Freewheel          : Freewheel_Type;
      PWM_Meas_SD_Enable : TMC_Boolean;
      PWM_Dis_Reg_Stst   : TMC_Boolean;
      PWM_Reg            : Unsigned_4;
      PWM_Lim            : Unsigned_4;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type PWM_SCALE is record
      PWM_Scale_Sum  : Unsigned_10;
      Reserved_1     : Unsigned_6;
      PWM_Scale_Auto : Unsigned_9;
      Reserved_2     : Unsigned_7;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type PWM_AUTO is record
      PWM_OFS_Auto  : Unsigned_8;
      Reserved_1    : Unsigned_8;
      PWM_Grad_Auto : Unsigned_8;
      Reserved_2    : Unsigned_8;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type SG4_THRS is record
      SG4_Thrs        : Unsigned_8;
      SG4_Filt_En     : TMC_Boolean;
      SG_Angle_Offset : TMC_Boolean;
      Reserved_2      : Unsigned_22;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type SG4_RESULT is record
      SG4_Result : Unsigned_10;
      Reserved   : Unsigned_22;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type SG4_IND is record
      SG4_Ind_0 : Unsigned_8;
      SG4_Ind_1 : Unsigned_8;
      SG4_Ind_2 : Unsigned_8;
      SG4_Ind_3 : Unsigned_8;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 32, Pack;

   type UART_Byte is mod 2**8 with Size => 8;
   type UART_Data_Byte_Array is array (1 .. 8) of UART_Byte with Pack;
   type UART_Query_Byte_Array is array (1 .. 4) of UART_Byte with Pack;

   type UART_Register_Address is
     (GCONF_Address,
      GSTAT_Address,
      IFCNT_Address,
      NODECONF_Address,
      IOIN_Address,
      DRV_CONF_Address,
      GLOBAL_SCALER_Address,
      IHOLD_IRUN_Address,
      TPOWERDOWN_Address,
      TSTEP_Address,
      TPWMTHRS_Address,
      TCOOLTHRS_Address,
      THIGH_Address,
      DIRECT_MODE_Address,
      ENCMODE_Address,
      X_ENC_Address,
      ENC_CONST_Address,
      ENC_STATUS_Address,
      ENC_LATCH_Address,
      ADC_VSUPPLY_AIN_Address,
      ADC_TEMP_Address,
      OTW_OV_VTH_Address,
      MSLUT0_Address,
      MSLUT1_Address,
      MSLUT2_Address,
      MSLUT3_Address,
      MSLUT4_Address,
      MSLUT5_Address,
      MSLUT6_Address,
      MSLUT7_Address,
      MSLUTSEL_Address,
      MSLUTSTART_Address,
      MSCNT_Address,
      MSCURACT_Address,
      CHOPCONF_Address,
      COOLCONF_Address,
      DRV_STATUS_Address,
      PWMCONF_Address,
      PWM_SCALE_Address,
      PWM_AUTO_Address,
      SG4_THRS_Address,
      SG4_RESULT_Address,
      SG4_IND_Address)
   with Size => 7;

   for UART_Register_Address use
     (GCONF_Address           => 16#0#,
      GSTAT_Address           => 16#1#,
      IFCNT_Address           => 16#2#,
      NODECONF_Address        => 16#3#,
      IOIN_Address            => 16#4#,
      DRV_CONF_Address        => 16#A#,
      GLOBAL_SCALER_Address   => 16#B#,
      IHOLD_IRUN_Address      => 16#10#,
      TPOWERDOWN_Address      => 16#11#,
      TSTEP_Address           => 16#12#,
      TPWMTHRS_Address        => 16#13#,
      TCOOLTHRS_Address       => 16#14#,
      THIGH_Address           => 16#15#,
      DIRECT_MODE_Address     => 16#2D#,
      ENCMODE_Address         => 16#38#,
      X_ENC_Address           => 16#39#,
      ENC_CONST_Address       => 16#3A#,
      ENC_STATUS_Address      => 16#3B#,
      ENC_LATCH_Address       => 16#3C#,
      ADC_VSUPPLY_AIN_Address => 16#50#,
      ADC_TEMP_Address        => 16#51#,
      OTW_OV_VTH_Address      => 16#52#,
      MSLUT0_Address          => 16#60#,
      MSLUT1_Address          => 16#61#,
      MSLUT2_Address          => 16#62#,
      MSLUT3_Address          => 16#63#,
      MSLUT4_Address          => 16#64#,
      MSLUT5_Address          => 16#65#,
      MSLUT6_Address          => 16#66#,
      MSLUT7_Address          => 16#67#,
      MSLUTSEL_Address        => 16#68#,
      MSLUTSTART_Address      => 16#69#,
      MSCNT_Address           => 16#6A#,
      MSCURACT_Address        => 16#6B#,
      CHOPCONF_Address        => 16#6C#,
      COOLCONF_Address        => 16#6D#,
      DRV_STATUS_Address      => 16#6F#,
      PWMCONF_Address         => 16#70#,
      PWM_SCALE_Address       => 16#71#,
      PWM_AUTO_Address        => 16#72#,
      SG4_THRS_Address        => 16#74#,
      SG4_RESULT_Address      => 16#75#,
      SG4_IND_Address         => 16#76#);

   type UART_Data_Message_Inner (Register : UART_Register_Address := GCONF_Address) is record
      Sync     : UART_Sync_Nibble := 2#0101#;
      Reserved : Unsigned_4 := 0;
      Node     : UART_Node_Address;
      Is_Write : TMC_Boolean := True;
      CRC      : UART_CRC;
      case Register is
         when GCONF_Address =>
            GCONF_Data : GCONF;

         when GSTAT_Address =>
            GSTAT_Data : GSTAT;

         when IOIN_Address =>
            IOIN_Data : IOIN;

         when GLOBAL_SCALER_Address =>
            GLOBAL_SCALER_Data : GLOBAL_SCALER;

         when IHOLD_IRUN_Address =>
            IHOLD_IRUN_Data : IHOLD_IRUN;

         when IFCNT_Address =>
            IFCNT_Data : IFCNT;

         when NODECONF_Address =>
            NODECONF_Data : NODECONF;

         when DRV_CONF_Address =>
            DRV_CONF_Data : DRV_CONF;

         when TPOWERDOWN_Address =>
            TPOWERDOWN_Data : TPOWERDOWN;

         when TSTEP_Address =>
            TSTEP_Data : TSTEP;

         when TPWMTHRS_Address =>
            TPWMTHRS_Data : TPWMTHRS;

         when TCOOLTHRS_Address =>
            TCOOLTHRS_Data : TCOOLTHRS;

         when THIGH_Address =>
            THIGH_Data : THIGH;

         when DIRECT_MODE_Address =>
            DIRECT_MODE_Data : DIRECT_MODE;

         when ENCMODE_Address =>
            ENCMODE_Data : ENCMODE;

         when X_ENC_Address =>
            X_ENC_Data : X_ENC;

         when ENC_CONST_Address =>
            ENC_CONST_Data : ENC_CONST;

         when ENC_STATUS_Address =>
            ENC_STATUS_Data : ENC_STATUS;

         when ENC_LATCH_Address =>
            ENC_LATCH_Data : ENC_LATCH;

         when ADC_VSUPPLY_AIN_Address =>
            ADC_VSUPPLY_AIN_Data : ADC_VSUPPLY_AIN;

         when ADC_TEMP_Address =>
            ADC_TEMP_Data : ADC_TEMP;

         when OTW_OV_VTH_Address =>
            OTW_OV_VTH_Data : OTW_OV_VTH;

         when MSLUT0_Address =>
            MSLUT0_Data : MSLUT;

         when MSLUT1_Address =>
            MSLUT1_Data : MSLUT;

         when MSLUT2_Address =>
            MSLUT2_Data : MSLUT;

         when MSLUT3_Address =>
            MSLUT3_Data : MSLUT;

         when MSLUT4_Address =>
            MSLUT4_Data : MSLUT;

         when MSLUT5_Address =>
            MSLUT5_Data : MSLUT;

         when MSLUT6_Address =>
            MSLUT6_Data : MSLUT;

         when MSLUT7_Address =>
            MSLUT7_Data : MSLUT;

         when MSLUTSEL_Address =>
            MSLUTSEL_Data : MSLUTSEL;

         when MSLUTSTART_Address =>
            MSLUTSTART_Data : MSLUTSTART;

         when MSCNT_Address =>
            MSCNT_Data : MSCNT;

         when MSCURACT_Address =>
            MSCURACT_Data : MSCURACT;

         when CHOPCONF_Address =>
            CHOPCONF_Data : CHOPCONF;

         when COOLCONF_Address =>
            COOLCONF_Data : COOLCONF;

         when DRV_STATUS_Address =>
            DRV_STATUS_Data : DRV_STATUS;

         when PWMCONF_Address =>
            PWMCONF_Data : PWMCONF;

         when PWM_SCALE_Address =>
            PWM_SCALE_Data : PWM_SCALE;

         when PWM_AUTO_Address =>
            PWM_AUTO_Data : PWM_AUTO;

         when SG4_THRS_Address =>
            SG4_THRS_Data : SG4_THRS;

         when SG4_RESULT_Address =>
            SG4_RESULT_Data : SG4_RESULT;

         when SG4_IND_Address =>
            SG4_IND_Data : SG4_IND;
      end case;
   end record
   with Bit_Order => Low_Order_First, Scalar_Storage_Order => Low_Order_First, Size => 64;

   for UART_Data_Message_Inner use
     record
       Sync at 7 range 0 .. 3;
       Reserved at 7 range 4 .. 7;
       Node at 6 range 0 .. 7;
       Register at 5 range 0 .. 6;
       Is_Write at 5 range 7 .. 7;
       CRC at 0 range 0 .. 7;
       GCONF_Data at 1 range 0 .. 31;
       GSTAT_Data at 1 range 0 .. 31;
       IOIN_Data at 1 range 0 .. 31;
       GLOBAL_SCALER_Data at 1 range 0 .. 31;
       IHOLD_IRUN_Data at 1 range 0 .. 31;
       IFCNT_Data at 1 range 0 .. 31;
       NODECONF_Data at 1 range 0 .. 31;
       DRV_CONF_Data at 1 range 0 .. 31;
       TPOWERDOWN_Data at 1 range 0 .. 31;
       TSTEP_Data at 1 range 0 .. 31;
       TPWMTHRS_Data at 1 range 0 .. 31;
       TCOOLTHRS_Data at 1 range 0 .. 31;
       THIGH_Data at 1 range 0 .. 31;
       DIRECT_MODE_Data at 1 range 0 .. 31;
       ENCMODE_Data at 1 range 0 .. 31;
       X_ENC_Data at 1 range 0 .. 31;
       ENC_CONST_Data at 1 range 0 .. 31;
       ENC_STATUS_Data at 1 range 0 .. 31;
       ENC_LATCH_Data at 1 range 0 .. 31;
       ADC_VSUPPLY_AIN_Data at 1 range 0 .. 31;
       ADC_TEMP_Data at 1 range 0 .. 31;
       OTW_OV_VTH_Data at 1 range 0 .. 31;
       MSLUT0_Data at 1 range 0 .. 31;
       MSLUT1_Data at 1 range 0 .. 31;
       MSLUT2_Data at 1 range 0 .. 31;
       MSLUT3_Data at 1 range 0 .. 31;
       MSLUT4_Data at 1 range 0 .. 31;
       MSLUT5_Data at 1 range 0 .. 31;
       MSLUT6_Data at 1 range 0 .. 31;
       MSLUT7_Data at 1 range 0 .. 31;
       MSLUTSEL_Data at 1 range 0 .. 31;
       MSLUTSTART_Data at 1 range 0 .. 31;
       MSCNT_Data at 1 range 0 .. 31;
       MSCURACT_Data at 1 range 0 .. 31;
       CHOPCONF_Data at 1 range 0 .. 31;
       COOLCONF_Data at 1 range 0 .. 31;
       DRV_STATUS_Data at 1 range 0 .. 31;
       PWMCONF_Data at 1 range 0 .. 31;
       PWM_SCALE_Data at 1 range 0 .. 31;
       PWM_AUTO_Data at 1 range 0 .. 31;
       SG4_THRS_Data at 1 range 0 .. 31;
       SG4_RESULT_Data at 1 range 0 .. 31;
       SG4_IND_Data at 1 range 0 .. 31;
     end record;

   type UART_Data_Message (Bytes_Mode : Boolean := False) is record
      case Bytes_Mode is
         when True =>
            Bytes : UART_Data_Byte_Array;

         when False =>
            Content : UART_Data_Message_Inner;
      end case;
   end record
   with Unchecked_Union, Size => 64;

   for UART_Data_Message use
     record
       Bytes at 0 range 0 .. 63;
       Content at 0 range 0 .. 63;
     end record;

   type UART_Query_Message_Inner is record
      Sync     : UART_Sync_Nibble := 2#0101#;
      Reserved : Unsigned_4 := 0;
      Node     : UART_Node_Address;
      Is_Write : TMC_Boolean := False;
      CRC      : UART_CRC;
      Register : UART_Register_Address;
   end record
   with Bit_Order => Low_Order_First, Size => 32;

   for UART_Query_Message_Inner use
     record
       Sync at 3 range 0 .. 3;
       Reserved at 3 range 4 .. 7;
       Node at 2 range 0 .. 7;
       Register at 1 range 0 .. 6;
       Is_Write at 1 range 7 .. 7;
       CRC at 0 range 0 .. 7;
     end record;

   type UART_Query_Message (Bytes_Mode : Boolean := False) is record
      case Bytes_Mode is
         when True =>
            Bytes : UART_Query_Byte_Array;

         when False =>
            Content : UART_Query_Message_Inner;
      end case;
   end record
   with Unchecked_Union, Size => 32;

   for UART_Query_Message use
     record
       Bytes at 0 range 0 .. 31;
       Content at 0 range 0 .. 31;
     end record;

   function Compute_CRC (Message : UART_Data_Message) return UART_CRC;
   function Compute_CRC (Message : UART_Query_Message) return UART_CRC;

   procedure Optimize_Spreadcycle
     (Driver_Voltage              : Voltage;
      TBL                         : TBL_Type;
      Motor_Inductance            : Inductance;
      Motor_Resistance            : Resistance;
      Motor_Peak_Current          : Current;
      TOFF                        : TOFF_Type;
      IRUN                        : Unsigned_5;
      HSTRT                       : out Unsigned_3;
      HEND                        : out Unsigned_4;
      Sum_Too_High                : out Boolean;
      Sum_Too_High_For_Full_Scale : out Boolean;
      Excessive_Heating           : out Boolean;
      Driver_Voltage_Too_Low      : out Boolean);

private

   type UART_Bytes_For_CRC is array (Integer range <>) of UART_Byte;
   function Compute_CRC (Bytes : UART_Bytes_For_CRC) return UART_CRC;

end Prunt.TMC_Types.TMC2240;
