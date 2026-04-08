# Prunt User Manual

## 1. Introduction
Welcome to the Prunt Motion Controller User Manual.

## 2. Configuration Reference
The following sections describe the available configuration options.

### `CHM_Type`
| Field | Type | Description |
|---|---|---|
| `TOFF` | `TOFF_Type` |  |
| `HSTRT_TFD210` | `Unsigned_3` |  |
| `HEND_OFFSET` | `Unsigned_4` |  |
| `FD3` | `Unsigned_1` |  |
| `DISFDCC` | `TMC_Boolean` |  |
| `Reserved_1` | `Unsigned_1` |  |
| `CHM` | `CHM_Type` |  |
| `TBL` | `TBL_Type` |  |
| `Reserved_2` | `Unsigned_1` |  |
| `VHIGHFS` | `TMC_Boolean` |  |
| `VHIGHCHM` | `TMC_Boolean` |  |
| `TPFD` | `Unsigned_4` |  |
| `Microstep_Resolution` | `Microstep_Resolution_Type` |  |
| `Interpolate` | `TMC_Boolean` |  |
| `Double_Edge` | `TMC_Boolean` |  |
| `Disable_S2G` | `TMC_Boolean` |  |
| `Disable_S2Vs` | `TMC_Boolean` |  |

### `Current_Range_Type`
| Field | Type | Description |
|---|---|---|
| `Current_Range` | `Current_Range_Type` |  |
| `Reserved_1` | `Unsigned_2` |  |
| `Slope_Control` | `Slope_Control_Type` |  |
| `Reserved_2` | `Unsigned_26` |  |

### `Homing_Event_Subscriber`
| Field | Type | Description |
|---|---|---|
| `Motor` | `Motor_Name` |  |
| `Threshold` | `User_Config_Integer range -64 .. 63` |  |
| `Enable_Filter` | `Boolean` |  |
| `Motor` | `Motor_Name` |  |
| `Threshold` | `User_Config_Integer range 0 .. 255` |  |
| `Enable_Filter` | `Boolean` |  |
| `Switch` | `Input_Switch_Name` |  |
| `Use_StallGuard2` | `Homing_StallGuard2_Parameters` |  |
| `Use_StallGuard4` | `Homing_StallGuard4_Parameters` |  |
| `Subscriber` | `not null access function return Homing_Event_Subscriber'Class)` |  |
| `Config_Data` | `Config.Config_Data` |  |
| `Report_Config_Error` | `access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String)` |  |
| `Status_Emitter` | `Status_Manager.Status_Emitter` |  |
| `Get_Other_Instance` | `access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)` |  |
| `Self_Ref` | `My_Modules.Module_Instance_Shared_Pointers.Ref` |  |
| `Args` | `in out Gcode_Arguments.Arguments` |  |
| `Planner` | `Planner_Interface'Class` |  |
| `Command_Identifier` | `Gcode_Command_Identifier)` |  |
| `Position` | `Length range -1.0E100 * mm .. 1.0E100 * mm` |  |

### `UART_Node_Address`
| Field | Type | Description |
|---|---|---|
| `Reserved_1` | `Unsigned_1` |  |
| `Fast_Standstill` | `TMC_Boolean` |  |
| `En_PWM_Mode` | `TMC_Boolean` |  |
| `Multistep_Filt` | `TMC_Boolean` |  |
| `Invert_Direction` | `TMC_Boolean` |  |
| `Diag0_Error` | `TMC_Boolean` |  |
| `Diag0_OTPW` | `TMC_Boolean` |  |
| `Diag0_Stall` | `TMC_Boolean` |  |
| `Diag1_Stall` | `TMC_Boolean` |  |
| `Diag1_Index` | `TMC_Boolean` |  |
| `Diag1_On_State` | `TMC_Boolean` |  |
| `Reserved_2` | `Unsigned_1` |  |
| `Diag_0_Push_Pull` | `TMC_Boolean` |  |
| `Diag_1_Push_Pull` | `TMC_Boolean` |  |
| `Small_Hysteresis` | `TMC_Boolean` |  |
| `Stop_Enable` | `TMC_Boolean` |  |
| `Direct_Mode` | `TMC_Boolean` |  |
| `Reserved_3` | `Unsigned_15` |  |

### `User_Config`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Axial_Scaler_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Axis_Name) of Dimensionless range 1.0E-100 .. 1.0E100` | Array type |

### `User_Config_Axial_Velocity_Limits_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Axis_Name) of Velocity range 1.0E-6 * mm / s .. 1.0E100 * mm / s` | Array type |

### `User_Config_Axis_Homing`
| Field | Type | Description |
|---|---|---|
| `Velocity_Limit` | `Velocity range 0.000001 * mm / s .. 50.0 * mm / s` |  |
| `Move_To_After` | `Length range -1.0E100 * mm .. 1.0E100 * mm` |  |

### `User_Config_Axis_Homing_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Axis_Name) of User_Config_Axis_Homing` | Array type |

### `User_Config_CHM_Kind`
| Field | Type | Description |
|---|---|---|
| `SpreadCycle` | `User_Config_SpreadCycle` |  |
| `Constant_Off_Time` | `User_Config_Constant_Off_Time` |  |

### `User_Config_Constant_Off_Time`
| Field | Type | Description |
|---|---|---|
| `DISFDCC` | `Boolean` |  |
| `OFFSET` | `User_Config_Integer range -3 .. 10` |  |
| `TFD` | `User_Config_Integer range 0 .. 15` |  |

### `User_Config_Core_XY_Axis_Name`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Motor_Name) of User_Config_Core_XY_Axis_Name` | Array type |

### `User_Config_Default_Heater_Kind`
| Field | Type | Description |
|---|---|---|
| `Heater` | `Heater_Name` |  |

### `User_Config_Fan`
| Field | Type | Description |
|---|---|---|
| `Invert_PWM_Output` | `Boolean` |  |
| `Fixed_Switching` | `User_Config_Fan_Fixed_Switching` |  |

### `User_Config_Fan_Always_On`
| Field | Type | Description |
|---|---|---|
| `Duty_Cycle` | `PWM_Scale` |  |

### `User_Config_Fan_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Fan_Name) of User_Config_Fan` | Array type |

### `User_Config_Fan_Control_Method_Kind`
| Field | Type | Description |
|---|---|---|
| `Dynamic_Duty_Cycle` | `User_Config_Fan_Dynamic_Duty_Cycle` |  |
| `Always_On` | `User_Config_Fan_Always_On` |  |

### `User_Config_Fan_Fixed_Switching`
| Field | Type | Description |
|---|---|---|
| `PWM_Frequency` | `Frequency range 0.0 * hertz .. 1.0E100 * hertz` |  |

### `User_Config_Fan_High_Side_Switching`
| Field | Type | Description |
|---|---|---|
| `PWM_Frequency` | `Frequency range 0.0 * hertz .. 1.0E100 * hertz` |  |

### `User_Config_Fan_Low_Or_High_Side_Switching_Kind`
| Field | Type | Description |
|---|---|---|
| `Low_Side_Switching` | `User_Config_Fan_Low_Side_Switching` |  |
| `High_Side_Switching` | `User_Config_Fan_High_Side_Switching` |  |

### `User_Config_Fan_Low_Side_Switching`
| Field | Type | Description |
|---|---|---|
| `PWM_Frequency` | `Frequency range 0.0 * hertz .. 1.0E100 * hertz` |  |

### `User_Config_Gcode_Defaults`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Heater`
| Field | Type | Description |
|---|---|---|
| `Thermistor` | `Thermistor_Name` |  |
| `Check_Gain_Time` | `Time range 0.0 * s .. 1.0E100 * s` |  |
| `Check_Minimum_Gain` | `Temperature range 0.0 * celsius .. 1.0E100 * celsius` |  |
| `Check_Maximum_Cumulative_Error` | `Temperature range 0.0 * celsius .. 1.0E100 * celsius` |  |
| `Check_Hysteresis` | `Temperature range 0.0 * celsius .. 1.0E100 * celsius` |  |

### `User_Config_Heater_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Heater_Name) of User_Config_Heater` | Array type |

### `User_Config_Heater_Bang_Bang`
| Field | Type | Description |
|---|---|---|
| `Hysteresis` | `Temperature range 0.0 * celsius .. 1.0E100 * celsius` |  |

### `User_Config_Heater_Kind`
| Field | Type | Description |
|---|---|---|
| `PID` | `User_Config_Heater_PID` |  |
| `Bang_Bang` | `User_Config_Heater_Bang_Bang` |  |

### `User_Config_Homing_Method`
| Field | Type | Description |
|---|---|---|
| `Set_To_Value` | `User_Config_Homing_Set_To_Value` |  |
| `Use_Input_Switch` | `User_Config_Homing_Use_Input_Switch` |  |
| `Use_StallGuard2` | `User_Config_Homing_Use_StallGuard2` |  |
| `Use_StallGuard4` | `User_Config_Homing_Use_StallGuard4` |  |

### `User_Config_Homing_Method_Kind`
| Field | Type | Description |
|---|---|---|
| `Motor` | `Motor_Name` |  |
| `Move_Towards_Negative_Infinity` | `Boolean` |  |
| `Threshold` | `User_Config_Integer range 0 .. 255` |  |
| `Enable_Filter` | `Boolean` |  |
| `Stop_Position` | `Length range -1.0E100 * mm .. 1.0E100 * mm` |  |

### `User_Config_Homing_Prereq_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Axis_Name) of User_Config_Homing_Prereq` | Array type |

### `User_Config_Homing_Prereq_Kind`
| Field | Type | Description |
|---|---|---|
| `Must_Be_At_Position` | `User_Config_Homing_Prereq_Must_Be_At_Position` |  |

### `User_Config_Homing_Prereq_Must_Be_At_Position`
| Field | Type | Description |
|---|---|---|
| `Position` | `Length range -1.0E100 * mm .. 1.0E100 * mm` |  |

### `User_Config_Homing_Use_Input_Switch`
| Field | Type | Description |
|---|---|---|
| `Switch` | `Input_Switch_Name` |  |
| `Move_Towards_Negative_Infinity` | `Boolean` |  |
| `First_Move_Distance` | `Length range 0.000001 * mm .. 1.0E100 * mm` |  |
| `Back_Off_Move_Distance` | `Length range 0.0 * mm .. 1.0E100 * mm` |  |
| `Second_Move_Distance` | `Length range 0.000001 * mm .. 1.0E100 * mm` |  |
| `Switch_Position` | `Length range -1.0E100 * mm .. 1.0E100 * mm` |  |

### `User_Config_Homing_Use_StallGuard2`
| Field | Type | Description |
|---|---|---|
| `Motor` | `Motor_Name` |  |
| `Move_Towards_Negative_Infinity` | `Boolean` |  |
| `Threshold` | `User_Config_Integer range -64 .. 63` |  |
| `Enable_Filter` | `Boolean` |  |
| `Stop_Position` | `Length range -1.0E100 * mm .. 1.0E100 * mm` |  |

### `User_Config_Input_Shaping_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Axis_Name) of User_Config_Input_Shaping_Method` | Array type |

### `User_Config_Input_Shaping_EI`
| Field | Type | Description |
|---|---|---|
| `Shaper_Frequency` | `Frequency range 1.0E-10 * hertz .. 1.0E100 * hertz` |  |
| `Damping_Ratio` | `Prunt.Input_Shapers.Shaper_Damping_Ratio range 0.001 .. 0.999` |  |
| `Residual_Vibration_Level` | `Prunt.Input_Shapers.Residual_Vibration_Level range 0.001 .. 0.999` |  |
| `Number_Of_Humps` | `Prunt.Input_Shapers.Extra_Insensitive_Humps_Count` |  |

### `User_Config_Input_Shaping_Method_Kind`
| Field | Type | Description |
|---|---|---|
| `No_Shaper` | `User_Config_Input_Shaping_No_Shaper` |  |
| `ZV` | `User_Config_Input_Shaping_ZV` |  |
| `EI` | `User_Config_Input_Shaping_EI` |  |
| `Pressure_Advance` | `User_Config_Input_Shaping_Pressure_Advance` |  |

### `User_Config_Input_Shaping_Pressure_Advance`
| Field | Type | Description |
|---|---|---|
| `Pressure_Advance_Time` | `Time range -1.0E100 * s .. 1.0E100 * s` |  |
| `Pressure_Advance_Smooth_Time` | `Time range 0.0 * s .. 0.2 * s` |  |
| `Smooth_Added_Part_Only` | `Boolean` |  |
| `Smoothing_Levels` | `User_Config_Integer range 1 .. 10` |  |

### `User_Config_Input_Shaping_ZV`
| Field | Type | Description |
|---|---|---|
| `Shaper_Frequency` | `Frequency range 1.0E-10 * hertz .. 1.0E100 * hertz` |  |
| `Damping_Ratio` | `Prunt.Input_Shapers.Shaper_Damping_Ratio range 0.001 .. 0.999` |  |
| `Number_Of_Derivatives` | `Prunt.Input_Shapers.Zero_Vibration_Deriviatives_Count` |  |

### `User_Config_Input_Switch_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Input_Switch_Name) of User_Config_Input_Switch` | Array type |

### `User_Config_Kinematics`
| Field | Type | Description |
|---|---|---|
| `Ignore_E_In_XYZE` | `Boolean` |  |
| `Shift_Blended_Corners` | `Boolean` |  |
| `Maximum_Tangential_Velocity` | `Velocity range 0.000_001 * mm / s .. 1.0E100 * mm / s` |  |
| `Maximum_Chord_Error` | `Length range 0.0 * mm .. 1.0E100 * mm` |  |
| `Maximum_Acceleration` | `Acceleration range 0.000_001 * mm / s ** 2 .. 1.0E100 * mm / s ** 2` |  |
| `Maximum_Jerk` | `Jerk range 0.000_001 * mm / s ** 3 .. 1.0E100 * mm / s ** 3` |  |
| `Maximum_Snap` | `Snap range 0.000_001 * mm / s ** 4 .. 1.0E100 * mm / s ** 4` |  |
| `Maximum_Crackle` | `Crackle range 0.000_001 * mm / s ** 5 .. 1.0E100 * mm / s ** 5` |  |

### `User_Config_Kinematics_Cartesian`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Motor_Name) of User_Config_Cartesian_Axis_Name` | Array type |

### `User_Config_Kinematics_Kind`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Motion_Units_Gear_With_Circumference`
| Field | Type | Description |
|---|---|---|
| `Circumference` | `Length range 1.0E-100 * mm .. 1.0E100 * mm` |  |
| `Gear_Ratio` | `Dimensionless_Ratio` |  |

### `User_Config_Motion_Units_Gear_With_Tooth_Count_And_Pitch`
| Field | Type | Description |
|---|---|---|
| `Tooth_Count` | `Dimensionless range 1.0E-100 .. 1.0E100` |  |
| `Tooth_Pitch` | `Length range 1.0E-100 * mm .. 1.0E100 * mm` |  |
| `Gear_Ratio` | `Dimensionless_Ratio` |  |

### `User_Config_Motion_Units_Kind`
| Field | Type | Description |
|---|---|---|
| `Units_Per_Rotation` | `Dimensionless range 1.0E-100 .. 1.0E100` |  |
| `Reverse_Direction` | `Boolean` |  |
| `Direct_Entry` | `User_Config_Motion_Units_Direct_Entry` |  |
| `Lead_Screw` | `User_Config_Motion_Units_Lead_Screw` |  |
| `Gear_With_Circumference` | `User_Config_Motion_Units_Gear_With_Circumference` |  |
| `Gear_With_Tooth_Count_And_Pitch` | `User_Config_Motion_Units_Gear_With_Tooth_Count_And_Pitch` |  |

### `User_Config_Motion_Units_Lead_Screw`
| Field | Type | Description |
|---|---|---|
| `Lead` | `Length range 1.0E-100 * mm .. 1.0E100 * mm` |  |
| `Gear_Ratio` | `Dimensionless_Ratio` |  |

### `User_Config_Motor`
| Field | Type | Description |
|---|---|---|
| `Enabled` | `Boolean` |  |

### `User_Config_Motor_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Motor_Name) of User_Config_Motor` | Array type |

### `User_Config_Position_Limits_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Axis_Name) of Length range -1.0E100 * mm .. 1.0E100 * mm` | Array type |

### `User_Config_SpreadCycle_Kind`
| Field | Type | Description |
|---|---|---|
| `Derived` | `User_Config_SpreadCycle_Derived` |  |
| `Manual` | `User_Config_SpreadCycle_Manual` |  |

### `User_Config_SpreadCycle_Manual`
| Field | Type | Description |
|---|---|---|
| `HSTRT` | `User_Config_Integer range 1 .. 8` |  |
| `HEND` | `User_Config_Integer range -3 .. 12` |  |

### `User_Config_StealthChop2_Enabled`
| Field | Type | Description |
|---|---|---|
| `TPWMTHRS` | `Velocity range 0.0 * mm / s .. 1.0E100 * mm / s` |  |
| `PWM_OFS` | `TMC_Types.Unsigned_8` |  |
| `PWM_GRAD` | `TMC_Types.Unsigned_8` |  |
| `PWM_FREQ` | `TMC_Types.TMC2240.PWM_Freq_Type` |  |
| `PWM_AUTOSCALE` | `Boolean` |  |
| `PWM_AUTOGRAD` | `Boolean` |  |
| `FREEWHEEL` | `TMC_Types.TMC2240.Freewheel_Type` |  |
| `PWM_MEAS_SD_ENABLE` | `Boolean` |  |
| `PWM_DIS_REG_STST` | `Boolean` |  |
| `PWM_REG` | `TMC_Types.Unsigned_4` |  |
| `PWM_LIM` | `TMC_Types.Unsigned_4` |  |
| `MULTISTEP_FILT` | `Boolean` |  |

### `User_Config_StealthChop2_Kind`
| Field | Type | Description |
|---|---|---|
| `Parameters` | `User_Config_StealthChop2_Enabled` |  |

### `User_Config_TMC2240`
| Field | Type | Description |
|---|---|---|
| `Run_Current` | `Current range 0.125 * amp .. 3.0 * amp` |  |
| `IHOLD` | `Dimensionless range 0.03125 .. 1.0` |  |
| `IRUN` | `Dimensionless range 0.03125 .. 1.0` |  |
| `IRUN_During_Homing` | `Dimensionless range 0.03125 .. 1.0` |  |
| `IHOLDDELAY` | `Time range 0.0 * ms .. 315.0 * ms` |  |
| `IRUNDELAY` | `Time range 0.0 * ms .. 0.615 * ms` |  |
| `TPOWERDOWN` | `Time range 0.0 * ms .. 5355.0 * ms` |  |
| `THIGH` | `Velocity range 0.0 * mm / s .. 1.0E100 * mm / s` |  |
| `SLOPE_CONTROL` | `TMC_Types.TMC2240.Slope_Control_Type` |  |
| `TOFF` | `TMC_Types.TMC2240.TOFF_Type` |  |
| `TBL` | `TMC_Types.TMC2240.TBL_Type` |  |
| `VHIGHFS` | `Boolean` |  |
| `VHIGHCHM` | `Boolean` |  |
| `TPFD` | `TMC_Types.Unsigned_4` |  |
| `MRES` | `TMC_Types.TMC2240.Microstep_Resolution_Type` |  |
| `FAST_STANDSTILL` | `Boolean` |  |

### `User_Config_Tachometer_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Tachometer_Name) of User_Config_Tachometer` | Array type |

### `User_Config_Thermistor`
| Field | Type | Description |
|---|---|---|
| `Minimum_Temperature` | `Temperature range -1.0E100 * celsius .. 1.0E100 * celsius` |  |
| `Maximum_Temperature` | `Temperature range -1.0E100 * celsius .. 1.0E100 * celsius` |  |

### `User_Config_Thermistor_ATC_Semitec_104GT_2`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Thermistor_ATC_Semitec_104NT_4_R025H42G`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Thermistor_Array`
| Field | Type | Description |
|---|---|---|
| `Array` | `array(Thermistor_Name) of User_Config_Thermistor` | Array type |

### `User_Config_Thermistor_Custom_Callendar_Van_Dusen`
| Field | Type | Description |
|---|---|---|
| `R0` | `Resistance range 1.0E-100 * ohm .. 1.0E100 * ohm` |  |
| `A` | `Dimensionless range -1.0E100 .. 1.0E100` |  |
| `B` | `Dimensionless range -1.0E100 .. 1.0E100` |  |

### `User_Config_Thermistor_Custom_Steinhart_Hart`
| Field | Type | Description |
|---|---|---|
| `A` | `Dimensionless range -1.0E100 .. 1.0E100` |  |
| `B` | `Dimensionless range -1.0E100 .. 1.0E100` |  |
| `C` | `Dimensionless range -1.0E100 .. 1.0E100` |  |

### `User_Config_Thermistor_EPCOS_100K_B57560G104F`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Thermistor_Generic_3950`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Thermistor_Honeywell_100K_135_104LAG_J01`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Thermistor_Model_Kind`
| Field | Type | Description |
|---|---|---|
| `Disabled` | `User_Config_Thermistor_Disabled` |  |
| `ATC_Semitec_104GT_2` | `User_Config_Thermistor_ATC_Semitec_104GT_2` |  |
| `ATC_Semitec_104NT_4_R025H42G` | `User_Config_Thermistor_ATC_Semitec_104NT_4_R025H42G` |  |
| `EPCOS_100K_B57560G104F` | `User_Config_Thermistor_EPCOS_100K_B57560G104F` |  |
| `Generic_3950` | `User_Config_Thermistor_Generic_3950` |  |
| `Slice_Engineering_450` | `User_Config_Thermistor_Slice_Engineering_450` |  |
| `TDK_NTCG104LH104JT1` | `User_Config_Thermistor_TDK_NTCG104LH104JT1` |  |
| `Honeywell_100K_135_104LAG_J01` | `User_Config_Thermistor_Honeywell_100K_135_104LAG_J01` |  |
| `NTC_100K_MGB18_104F39050L32` | `User_Config_Thermistor_NTC_100K_MGB18_104F39050L32` |  |
| `PT_1000_PT_385` | `User_Config_Thermistor_PT_1000_PT_385` |  |
| `PT_1000_PT_392` | `User_Config_Thermistor_PT_1000_PT_392` |  |
| `Custom_Steinhart_Hart` | `User_Config_Thermistor_Custom_Steinhart_Hart` |  |
| `Custom_Callendar_Van_Dusen` | `User_Config_Thermistor_Custom_Callendar_Van_Dusen` |  |

### `User_Config_Thermistor_NTC_100K_MGB18_104F39050L32`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Thermistor_PT_1000_PT_385`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Thermistor_PT_1000_PT_392`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Thermistor_Slice_Engineering_450`
*(Type definition not explicitly parsed or no fields)*

### `User_Config_Thermistor_TDK_NTCG104LH104JT1`
*(Type definition not explicitly parsed or no fields)*

## 3. G-code Command Reference
The following G-code commands are supported by Prunt:

### `G0`
**Parameters:**
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`
- `F: Gcode_Optional_Float`

### `G1`
**Parameters:**
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`
- `F: Gcode_Optional_Float`

### `G2`
**Parameters:**
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`
- `F: Gcode_Optional_Float`
- `I: Dimensionless`
- `J: Dimensionless`
**Parameters:**
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`
- `F: Gcode_Optional_Float`
- `R: Dimensionless`

### `G3`
**Parameters:**
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`
- `F: Gcode_Optional_Float`
- `I: Dimensionless`
- `J: Dimensionless`
**Parameters:**
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`
- `F: Gcode_Optional_Float`
- `R: Dimensionless`

### `G4`
Does nothing. This command is provided for compatibility with Marlin g-code.

Dwell for a specified time in seconds.  Negative times will cause an error to be emitted.  This command differs from Marlin in that `S` and `P` can not be specified at the same time. In Marlin if both `S` and `P` are specified then `P` will be silently ignored, however in Prunt it is an error to specify both.

**Parameters:**
- `S: Dimensionless`
Dwell for a specified time in milliseconds.  Negative times will cause an error to be emitted.  This command differs from Marlin in that `S` and `P` can not be specified at the same time. In Marlin if both `S` and `P` are specified then `P` will be silently ignored, however in Prunt it is an error to specify both.

**Parameters:**
- `P: Dimensionless`

### `G10`

### `G11`

### `G21`

### `G28`
Home the specified axes using the method and parameters specified in the configuration. If no axes are specified then all axes are homed, including the E axis.  The `ABCUVW` parameters from Marlin are not present as Prunt does not support these axes. The `LOR` parameters are not present but are planned for a future version. These parameters are present in Marlin.  The `E` parameter is not present in Marlin as Marlin does not homing of the E axis.

**Parameters:**
- `X: Gcode_Optional_No_Value`
- `Y: Gcode_Optional_No_Value`
- `Z: Gcode_Optional_No_Value`
- `E: Gcode_Optional_No_Value`

### `G60`
**Parameters:**
- `S: Gcode_Arguments.Argument_Integer`
**Parameters:**
- `D: Gcode_Arguments.Argument_Integer`
**Parameters:**
- `D: Gcode_No_Value`
**Parameters:**
- `Q: Gcode_Arguments.Argument_Integer`
- `F: Gcode_Optional_Float`
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`

### `G61`
**Parameters:**
- `F: Gcode_Optional_Float`
- `S: Gcode_Arguments.Argument_Integer`
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`

### `G90`

### `G91`

### `G92`
**Parameters:**
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`

### `M16`
Halt if the machine name does not match the provided string. The machine name can be set in the configuration page or via M550.  This command has the same function as M16 in Marlin but the format is slightly different. Specifically, the string to match against must be wrapped in quotation marks and must come after the `P` parameter letter.

**Parameters:**
- `P: Virtual_String`

### `M17`
Enable one or more steppers immediately.

**Parameters:**
- `X: Gcode_Optional_No_Value`
- `Y: Gcode_Optional_No_Value`
- `Z: Gcode_Optional_No_Value`
- `E: Gcode_Optional_No_Value`
- `A: Gcode_Optional_No_Value`
- `B: Gcode_Optional_No_Value`
- `C: Gcode_Optional_No_Value`
- `U: Gcode_Optional_No_Value`
- `V: Gcode_Optional_No_Value`
- `W: Gcode_Optional_No_Value`

### `M18`
Disable one or more steppers immediately or update the inactivity timeout.

**Parameters:**
- `S: Gcode_Optional_Integer`
- `X: Gcode_Optional_No_Value`
- `Y: Gcode_Optional_No_Value`
- `Z: Gcode_Optional_No_Value`
- `E: Gcode_Optional_No_Value`
- `A: Gcode_Optional_No_Value`
- `B: Gcode_Optional_No_Value`
- `C: Gcode_Optional_No_Value`
- `U: Gcode_Optional_No_Value`
- `V: Gcode_Optional_No_Value`
- `W: Gcode_Optional_No_Value`

### `M80`
Turn on or report the power supply state.

**Parameters:**
- `S: Gcode_Optional_No_Value`

### `M81`
Turn off the power supply.


### `M82`

### `M83`

### `M84`
Alias of M18.

**Parameters:**
- `S: Gcode_Optional_Integer`
- `X: Gcode_Optional_No_Value`
- `Y: Gcode_Optional_No_Value`
- `Z: Gcode_Optional_No_Value`
- `E: Gcode_Optional_No_Value`
- `A: Gcode_Optional_No_Value`
- `B: Gcode_Optional_No_Value`
- `C: Gcode_Optional_No_Value`
- `U: Gcode_Optional_No_Value`
- `V: Gcode_Optional_No_Value`
- `W: Gcode_Optional_No_Value`

### `M85`
Configure inactivity shutdown.

**Parameters:**
- `S: Gcode_Arguments.Argument_Integer`

### `M105`
Report temperatures to the logger.

**Parameters:**
- `R: Gcode_Optional_No_Value`
- `T: Gcode_Optional_Integer`

### `M106`
Set the speed of the default fan. The speed is scaled according to the maximum speed configured for the selected fan. It is an error to attempt to set the speed of a fan that is configured to be always on.  This command differs from Marlin in that the `I` and `T` parameters are not available. Additionally, the `S` parameter allows for a real number instead of just an integer.

**Parameters:**
- `S: Dimensionless`
Set the speed of a fan by index number. The speed is scaled according to the maximum speed configured for the selected fan. It is an error to attempt to set the speed of a fan that is configured to be always on.  This command differs from Marlin in that the `I` and `T` parameters are not available. Additionally, the `S` parameter allows for a real number instead of just an integer.

**Parameters:**
- `P: Gcode_Arguments.Argument_Integer`
- `S: Dimensionless`
Set the speed of a fan by name. The speed is scaled according to the maximum speed configured for the selected fan. It is an error to attempt to set the speed of a fan that is configured to be always on.  This command variant is not present in Marlin.

**Parameters:**
- `P: Virtual_String`
- `S: Dimensionless`

### `M107`
Turn the default fan off. It is an error to attempt to turn off a fan that is configured to be always on.

Turn a fan off by index number. It is an error to attempt to turn off a fan that is configured to be always on.

**Parameters:**
- `P: Gcode_Arguments.Argument_Integer`
Turn a fan off by name. It is an error to attempt to turn off a fan that is configured to be always on.

**Parameters:**
- `P: Virtual_String`

### `M109`
Set the hotend target temperature and wait for the hotend to go over the given temperature. This only waits for the hotend to heat up, it does not wait for the hotend to cool down.  This command differs from Marlin in that the B, F, I, and T parameters are not available.

**Parameters:**
- `S: Dimensionless`
Set the hotend target temperature and wait for the hotend to reach the given temperature. This applies to heating or cooling.  This command differs from Marlin in that the B, F, I, and T parameters are not available.

**Parameters:**
- `R: Dimensionless`

### `M114`
Report the current position to the logger.  THE `DER` parameters from Marlin are not present.


### `M119`
Report configured input switch states to the logger.


### `M122`
Report TMC diagnostics to the logger.

**Parameters:**
- `I: Gcode_Optional_No_Value`
- `X: Gcode_Optional_No_Value`
- `Y: Gcode_Optional_No_Value`
- `Z: Gcode_Optional_No_Value`
- `E: Gcode_Optional_No_Value`
- `V: Gcode_Optional_No_Value`
- `S: Gcode_Optional_Integer`
- `P: Gcode_Optional_Integer`
Report TMC diagnostics for a selected motor index.

**Parameters:**
- `I: Gcode_Optional_No_Value`
- `N: Gcode_Arguments.Argument_Integer`
- `V: Gcode_Optional_No_Value`
- `S: Gcode_Optional_Integer`
- `P: Gcode_Optional_Integer`
Report TMC diagnostics for a selected motor name.

**Parameters:**
- `I: Gcode_Optional_No_Value`
- `N: Virtual_String`
- `V: Gcode_Optional_No_Value`
- `S: Gcode_Optional_Integer`
- `P: Gcode_Optional_Integer`

### `M123`
Report tachometer readings to the log immediately. This will not interrupt readings that are being reported on an interval.

Report tachometer readings to the log repeatedly with a given interval. If this command has been called previously then this will override the previous interval rather than using both.  This command differs from Marlin in that the `S` parameter may be a real number instead of just an integer.

**Parameters:**
- `S: Dimensionless`

### `M140`
Set the bed target temperature and continue without waiting for the bed to reach the given temperature.  This command differs from Marlin in that the I parameter is not available.

**Parameters:**
- `S: Dimensionless`

### `M141`
Set the chamber target temperature and continue without waiting for the chamber to reach the given temperature.

**Parameters:**
- `S: Dimensionless`

### `M154`
Configure automatic position reporting to the logger.  This command differs from Marlin in that the `S` parameter is not optional.

**Parameters:**
- `S: Dimensionless`

### `M155`
Configure automatic temperature reporting to the logger.

**Parameters:**
- `S: Gcode_Optional_Integer`

### `M190`
Set the bed target temperature and wait for the bed to go over the given temperature. This only waits for the bed to heat up, it does not wait for the bed to cool down.  If the T parameter is present then heating will be performed as a linear interpolation over the given time starting from the current temperature. If the temperature is already over the target temperature then no interpolation will be performed.  This command differs from Marlin in that the I parameter is not available and the T parameter is available for heating as well as cooling.

**Parameters:**
- `S: Dimensionless`
- `T: Dimensionless`
Set the bed target temperature and wait for the bed to reach the given temperature. This applies to heating or cooling.  If the T parameter is present then heating or cooling will be performed as a linear interpolation over the given time starting from the current temperature.  This command differs from Marlin in that the I parameter is not available and the T parameter is available for heating as well as cooling.

**Parameters:**
- `R: Dimensionless`
- `T: Dimensionless`

### `M191`
Set the chamber target temperature and wait for the chamber to go over the given temperature. This only waits for the chamber to heat up, it does not wait for the chamber to cool down.

**Parameters:**
- `S: Dimensionless`
Set the chamber target temperature and wait for the chamber to reach the given temperature. This applies to heating or cooling.

**Parameters:**
- `R: Dimensionless`

### `M203`
Set maximum axial feedrates. May be saved using `M500`.  The `T` parameter from Marlin is not present.

**Parameters:**
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`

### `M205`
Set dynamic kinematic limits. May be saved using `M500`.  This command differs significantly from `M205` in Marlin, so `P"Prunt"` must always be present to prevent conflicts.

**Parameters:**
- `P: Virtual_String`
- `A: Gcode_Optional_Float`
- `J: Gcode_Optional_Float`
- `S: Gcode_Optional_Float`
- `C: Gcode_Optional_Float`
- `D: Gcode_Optional_Float`

### `M207`
**Parameters:**
- `F: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`

### `M208`
**Parameters:**
- `F: Gcode_Optional_Float`
- `S: Gcode_Optional_Float`

### `M209`
**Parameters:**
- `S: Gcode_Optional_Float`

### `M220`
**Parameters:**
- `S: Gcode_Optional_Float`

### `M221`
**Parameters:**
- `S: Gcode_Optional_Float`

### `M350`
Set stepper microstepping values.

**Parameters:**
- `B: Gcode_Optional_Integer`
- `S: Gcode_Optional_Integer`
- `X: Gcode_Optional_Integer`
- `Y: Gcode_Optional_Integer`
- `Z: Gcode_Optional_Integer`
- `A: Gcode_Optional_Integer`
- `C: Gcode_Optional_Integer`
- `U: Gcode_Optional_Integer`
- `V: Gcode_Optional_Integer`
- `W: Gcode_Optional_Integer`
- `E: Gcode_Optional_Integer`

### `M351`
Set raw microstep pin states.

**Parameters:**
- `S: Gcode_Arguments.Argument_Integer`
- `B: Gcode_Optional_Integer`
- `X: Gcode_Optional_Integer`
- `Y: Gcode_Optional_Integer`
- `Z: Gcode_Optional_Integer`
- `E: Gcode_Optional_Integer`

### `M493`
Configure input shaping for one or more axes.  Each provided axis parameter must be a JSON object inside a G-code string. Use single quotes around the whole JSON payload so normal JSON double quotes can be used inside it. Below are the various options:  `{"Kind" : "No_Shaper"}`  `{"Kind" : "Zero_Vibration", "Shaper_Frequency" : 40.0, "Damping_Ratio" : 0.1, "Number_Of_Derivatives" : 1}`  `{"Kind" : "Extra_Insensitive", "Shaper_Frequency" : 40.0, "Damping_Ratio":0.1, "Residual_Vibration_Level" : 0.05, "Number_Of_Humps" : 1}`  `{"Kind" : "Pressure_Advance", "Pressure_Advance_Time" : 0.02, "Pressure_Advance_Smooth_Time" : 0.01, "Smooth_Added_Part_Only" : false, "Smoothing_Levels" : 2}`.  `Shaper_Frequency` is in hertz and all time values are in seconds. Changes can be saved with `M500`.

**Parameters:**
- `P: Virtual_String`
- `X: Gcode_Optional_String`
- `Y: Gcode_Optional_String`
- `Z: Gcode_Optional_String`
- `E: Gcode_Optional_String`

### `M500`
Save all configurable settings for all modules that have been temporarily set as a result of g-code commands. Settings and g-code commands which use this functionality make a note of this in their own descriptions.  This command differs from Marlin in that the exact settings that are available to be saved may not be the same.

Save all configurable settings for a specific module that have been temporarily set as a result of g-code commands. Settings and g-code commands which use this functionality make a note of this in their own descriptions.  This command is not present in Marlin.

**Parameters:**
- `I: Virtual_String`
List modules with savable settings.  This command is not present in Marlin.

**Parameters:**
- `I: Gcode_No_Value`

### `M550`
Set the machine name. Saved by M500. This can also be set in the configuration page.  This command has the same function as M550 in Marlin but the format is slightly different. Specifically, there is no loose string form.

**Parameters:**
- `P: Virtual_String`
Report the current machine name to the log.


### `M569`
Toggle TMC stepping mode.

**Parameters:**
- `S: Gcode_Optional_Integer`
- `X: Gcode_Optional_No_Value`
- `Y: Gcode_Optional_No_Value`
- `Z: Gcode_Optional_No_Value`
- `E: Gcode_Optional_No_Value`
- `I: Gcode_Optional_Integer`
- `T: Gcode_Optional_Integer`
Toggle TMC stepping mode for a selected motor index.

**Parameters:**
- `S: Gcode_Optional_Integer`
- `N: Gcode_Arguments.Argument_Integer`
Toggle TMC stepping mode for a selected motor name.

**Parameters:**
- `S: Gcode_Optional_Integer`
- `N: Virtual_String`

### `M906`
Set TMC current values.

**Parameters:**
- `E: Gcode_Optional_Integer`
- `I: Gcode_Optional_Integer`
- `T: Gcode_Optional_Integer`
- `X: Gcode_Optional_Integer`
- `Y: Gcode_Optional_Integer`
- `Z: Gcode_Optional_Integer`

### `M907`
Set stepper current through a digital trimpot interface.

**Parameters:**
- `B: Gcode_Optional_Float`
- `C: Gcode_Optional_Float`
- `D: Gcode_Optional_Float`
- `E: Gcode_Optional_Float`
- `S: Gcode_Optional_Float`
- `X: Gcode_Optional_Float`
- `Y: Gcode_Optional_Float`
- `Z: Gcode_Optional_Float`
- `I: Gcode_Optional_Float`
- `J: Gcode_Optional_Float`
- `K: Gcode_Optional_Float`
- `U: Gcode_Optional_Float`
- `V: Gcode_Optional_Float`
- `W: Gcode_Optional_Float`

### `M908`
Set a trimpot value by raw pin/address.

**Parameters:**
- `P: Gcode_Arguments.Argument_Integer`
- `S: Gcode_Arguments.Argument_Integer`

### `M909`
Report DAC current values to the logger.


### `M910`
Commit DAC values to external EEPROM.


### `M911`
Report TMC overtemperature prewarn state to the logger.


### `M912`
Clear TMC overtemperature prewarn state.

**Parameters:**
- `I: Gcode_Optional_Integer`
- `X: Gcode_Optional_No_Value`
- `Y: Gcode_Optional_No_Value`
- `Z: Gcode_Optional_No_Value`
- `E: Gcode_Optional_No_Value`
Clear TMC overtemperature prewarn state for a selected motor index.

**Parameters:**
- `I: Gcode_Optional_Integer`
- `N: Gcode_Arguments.Argument_Integer`
Clear TMC overtemperature prewarn state for a selected motor name.

**Parameters:**
- `I: Gcode_Optional_Integer`
- `N: Virtual_String`

### `M913`
Set TMC hybrid threshold speeds.

**Parameters:**
- `I: Gcode_Optional_Integer`
- `T: Gcode_Optional_Integer`
- `X: Gcode_Optional_Integer`
- `Y: Gcode_Optional_Integer`
- `Z: Gcode_Optional_Integer`
- `A: Gcode_Optional_Integer`
- `B: Gcode_Optional_Integer`
- `C: Gcode_Optional_Integer`
- `U: Gcode_Optional_Integer`
- `V: Gcode_Optional_Integer`
- `W: Gcode_Optional_Integer`
- `E: Gcode_Optional_Integer`

### `M919`
Set TMC chopper timing values.

**Parameters:**
- `O: Gcode_Optional_Integer`
- `P: Gcode_Optional_Float`
- `S: Gcode_Optional_Integer`
- `I: Gcode_Optional_Integer`
- `T: Gcode_Optional_Integer`
- `X: Gcode_Optional_No_Value`
- `Y: Gcode_Optional_No_Value`
- `Z: Gcode_Optional_No_Value`
- `A: Gcode_Optional_No_Value`
- `B: Gcode_Optional_No_Value`
- `C: Gcode_Optional_No_Value`
- `U: Gcode_Optional_No_Value`
- `V: Gcode_Optional_No_Value`
- `W: Gcode_Optional_No_Value`
Set TMC chopper timing for a selected motor index.

**Parameters:**
- `O: Gcode_Optional_Integer`
- `P: Gcode_Optional_Float`
- `S: Gcode_Optional_Integer`
- `N: Gcode_Arguments.Argument_Integer`
Set TMC chopper timing for a selected motor name.

**Parameters:**
- `O: Gcode_Optional_Integer`
- `P: Gcode_Optional_Float`
- `S: Gcode_Optional_Integer`
- `N: Virtual_String`

### `M920`
Set TMC homing current values.

**Parameters:**
- `I: Gcode_Optional_Integer`
- `X: Gcode_Optional_Integer`
- `Y: Gcode_Optional_Integer`
- `Z: Gcode_Optional_Integer`
- `A: Gcode_Optional_Integer`
- `B: Gcode_Optional_Integer`
- `C: Gcode_Optional_Integer`
- `U: Gcode_Optional_Integer`
- `V: Gcode_Optional_Integer`
- `W: Gcode_Optional_Integer`
