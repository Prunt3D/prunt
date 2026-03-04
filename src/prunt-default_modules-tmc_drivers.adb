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

package body Prunt.Default_Modules.TMC_Drivers is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema is
   begin
      return (Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      My_Config                         : constant User_Config := Config_Data_To_User_Config (Config_Data.Get);
      Motor_Drivers_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Motor_Drivers_Module.Module_Instance'Tag);
      Motor_Drivers_Module_Instance     : Motor_Drivers_Module.Module_Instance renames
        Motor_Drivers_Module.Module_Instance (Motor_Drivers_Module_Instance_Ref.Get.Element.all);
      Registers                         : Motor_Registers_Map;
   begin
      for M in My_Controller_Generic_Types.Motor_Name loop
         case My_Config.Motors (M).Fixed_Kind is
            when TMC2240_UART_Kind =>
               --  TODO: We have to provide the motor config before creating the default registers as we need to get
               --  back distance per unit.

               Registers (M) :=
                 (Kind         => TMC2240_UART_Kind,
                  TMC2240_Regs =>
                    Generate_Default_Registers
                      (My_Config.Motors (M).TMC2240_Parameters,
                       Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M),
                       Report_Config_Error,
                       M,
                       Motor_Drivers_Module_Instance.Distance_Per_Unit (M)));

            when others            =>
               null;
         end case;
      end loop;

      return Module_Instance'(My_Modules.Module_Instance with Config => My_Config, Registers => Registers);
   end Initialize;

   overriding
   procedure Start (This : in out Module_Instance) is
   begin
      --  TODO
      null;
   end Start;

   function Generate_Default_Registers
     (Config              : User_Config_TMC2240;
      Motor_Enabled       : Boolean;
      Report_Config_Error : access procedure (Path : Prunt.Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Motor               : My_Controller_Generic_Types.Motor_Name;
      Distance_Per_Step   : Length) return TMC2240_Registers
   is

      procedure Report (Path : Prunt.Config.Config_Data_Paths.Vector; Message : Virtual_String) is
         use type Prunt.Config.Config_Data_Paths.Vector;
      begin
         Report_Config_Error (["Motors", +Motor'Image, "TMC2240_Parameters"] & Path, Message);
      end Report;
   begin
      return
         Result : TMC2240_Registers :=
           (GCONF         =>
              (Reserved_1       => 0,
               Fast_Standstill  => TMC_Boolean (Config.FAST_STANDSTILL),
               En_PWM_Mode      => TMC_Boolean (Config.StealthChop2.Kind = Enabled),
               Multistep_Filt   =>
                 TMC_Boolean
                   (Config.StealthChop2.Kind = Enabled and then Config.StealthChop2.Parameters.MULTISTEP_FILT),
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
            DRV_CONF      =>
              (Current_Range =>
                 (if Config.Run_Current > 2.0 * amp
                  then Max_3A
                  elsif Config.Run_Current > 1.0 * amp
                  then Max_2A
                  else Max_1A),
               Reserved_1    => 0,
               Slope_Control => Config.SLOPE_CONTROL,
               Reserved_2    => 0),
            GLOBAL_SCALER =>
              (Global_Scaler =>
                 (if Config.Run_Current = 3.0 * amp
                  then 0
                  elsif Config.Run_Current > 2.0 * amp
                  then
                    TMC_Types.TMC2240.Global_Scaler_Type
                      (Dimensionless'Floor (Dimensionless (Config.Run_Current / (3.0 * amp)) * 256.0))
                  elsif Config.Run_Current = 2.0 * amp
                  then 0
                  elsif Config.Run_Current > 1.0 * amp
                  then
                    TMC_Types.TMC2240.Global_Scaler_Type
                      (Dimensionless'Floor (Dimensionless (Config.Run_Current / (2.0 * amp)) * 256.0))
                  elsif Config.Run_Current = 1.0 * amp
                  then 0
                  else
                    TMC_Types.TMC2240.Global_Scaler_Type
                      (Dimensionless'Max
                         (32.0, Dimensionless'Floor (Dimensionless (Config.Run_Current / (1.0 * amp)) * 256.0)))),
               Reserved      => 0),
            IHOLD_IRUN    =>
              (I_Hold       =>
                 (if Motor_Enabled
                  then TMC_Types.Unsigned_5 (Dimensionless'Max (0.0, Dimensionless'Floor (Config.IHOLD * 32.0 - 1.0)))
                  else 0),
               Reserved_1   => 0,
               I_Run        =>
                 (if Motor_Enabled
                  then TMC_Types.Unsigned_5 (Dimensionless'Max (0.0, Dimensionless'Floor (Config.IRUN * 32.0 - 1.0)))
                  else 0),
               Reserved_2   => 0,
               I_Hold_Delay =>
                 TMC_Types.Unsigned_4 (Dimensionless'Floor (Dimensionless (Config.IHOLDDELAY / (21.0 * ms)))),
               Reserved_3   => 0,
               I_Run_Delay  =>
                 TMC_Types.Unsigned_4 (Dimensionless'Floor (Dimensionless (Config.IRUNDELAY / (0.041 * ms)))),
               Reserved_4   => 0),
            TPOWERDOWN    =>
              (T_Power_Down =>
                 TMC_Types.Unsigned_8 (Dimensionless'Floor (Dimensionless (Config.TPOWERDOWN / (21.0 * ms)))),
               Reserved     => 0),
            TPWMTHRS      =>
              (T_PWM_Thrs =>
                 (if Config.StealthChop2.Kind = Enabled
                  then
                    TMC_Types.Unsigned_20
                      (Dimensionless'Floor
                         (Dimensionless'Min
                            (12_500_000.0
                             * hertz
                             * abs (Distance_Per_Step)
                             / (Config.StealthChop2.Parameters.TPWMTHRS + 1.0E-100 * mm / s),
                             2.0 ** 20 - 1.0)))
                  else TMC_Types.Unsigned_20'Last),
               Reserved   => 0),
            TCOOLTHRS     => (T_Cool_Thrs => TMC_Types.Unsigned_20'Last, Reserved => 0),
            THIGH         =>
              (T_High   =>
                 TMC_Types.Unsigned_20
                   (Dimensionless'Floor
                      (Dimensionless'Min
                         (12_500_000.0 * hertz * abs (Distance_Per_Step) / (Config.THIGH + 1.0E-100 * mm / s),
                          2.0 ** 20 - 1.0))),
               Reserved => 0),
            PWMCONF       =>
              (PWM_OFS            =>
                 (if Config.StealthChop2.Kind = Enabled then Config.StealthChop2.Parameters.PWM_OFS else 29),
               PWM_Grad           =>
                 (if Config.StealthChop2.Kind = Enabled then Config.StealthChop2.Parameters.PWM_GRAD else 0),
               PWM_Freq           =>
                 (if Config.StealthChop2.Kind = Enabled then Config.StealthChop2.Parameters.PWM_FREQ else Freq_1024),
               PWM_Auto_Scale     =>
                 (if Config.StealthChop2.Kind = Enabled
                  then TMC_Boolean (Config.StealthChop2.Parameters.PWM_AUTOSCALE)
                  else True),
               PWM_Auto_Grad      =>
                 (if Config.StealthChop2.Kind = Enabled
                  then TMC_Boolean (Config.StealthChop2.Parameters.PWM_AUTOGRAD)
                  else True),
               Freewheel          =>
                 (if Config.StealthChop2.Kind = Enabled then Config.StealthChop2.Parameters.FREEWHEEL else Normal),
               PWM_Meas_SD_Enable =>
                 (if Config.StealthChop2.Kind = Enabled
                  then TMC_Boolean (Config.StealthChop2.Parameters.PWM_MEAS_SD_ENABLE)
                  else False),
               PWM_Dis_Reg_Stst   =>
                 (if Config.StealthChop2.Kind = Enabled
                  then TMC_Boolean (Config.StealthChop2.Parameters.PWM_DIS_REG_STST)
                  else False),
               PWM_Reg            =>
                 (if Config.StealthChop2.Kind = Enabled then Config.StealthChop2.Parameters.PWM_REG else 4),
               PWM_Lim            =>
                 (if Config.StealthChop2.Kind = Enabled then Config.StealthChop2.Parameters.PWM_LIM else 12)),
            CHOPCONF      =>
              (TOFF                 => (if Motor_Enabled then Config.TOFF else Disable_Driver),
               HSTRT_TFD210         => 5,
               --  Set later if required.
               HEND_OFFSET          => 3,
               --  Set later if required.
               FD3                  => 0,
               --  Set later if required.
               DISFDCC              => False,
               --  Set later if required.
               Reserved_1           => 0,
               CHM                  => SpreadCycle_Mode,
               --  Set later if required.
               TBL                  => Config.TBL,
               Reserved_2           => 0,
               VHIGHFS              => TMC_Boolean (Config.VHIGHFS),
               VHIGHCHM             => TMC_Boolean (Config.VHIGHCHM),
               TPFD                 => Config.TPFD,
               Microstep_Resolution => Config.MRES,
               Interpolate          => False,
               Double_Edge          => False,
               --  TODO: Extract from stepper hardware data.
               Disable_S2G          => False,
               Disable_S2Vs         => False))
      do
         if Motor_Enabled then
            if Config.IRUN_During_Homing > Config.IRUN then
               Report (["IRUN_During_Homing"], "IRUN during homing must be less than or equal to IRUN.");
            end if;
         end if;

         if not Motor_Enabled then
            --  Use default parameters.
            null;
         elsif Config.CHM.Kind = Constant_Off_Time then
            Result.CHOPCONF.CHM := Constant_Off_Time_Mode;
            Result.CHOPCONF.DISFDCC := TMC_Boolean (Config.CHM.Constant_Off_Time.DISFDCC);
            Result.CHOPCONF.HEND_OFFSET := TMC_Types.Unsigned_4 (Config.CHM.Constant_Off_Time.OFFSET + 3);
            Result.CHOPCONF.HSTRT_TFD210 := TMC_Types.Unsigned_3 (Config.CHM.Constant_Off_Time.TFD rem 8);
            Result.CHOPCONF.FD3 := TMC_Types.Unsigned_1 (Config.CHM.Constant_Off_Time.TFD / 8);
         elsif Config.CHM.Kind = SpreadCycle and then Config.CHM.SpreadCycle.Kind = Manual then
            Result.CHOPCONF.CHM := SpreadCycle_Mode;
            Result.CHOPCONF.HEND_OFFSET := TMC_Types.Unsigned_4 (Config.CHM.SpreadCycle.Manual.HEND + 3);
            Result.CHOPCONF.HSTRT_TFD210 := TMC_Types.Unsigned_3 (Config.CHM.SpreadCycle.Manual.HSTRT - 1);

            if Result.IHOLD_IRUN.I_Run = 31
              and then Config.CHM.SpreadCycle.Manual.HEND + Config.CHM.SpreadCycle.Manual.HSTRT > 14
            then
               --  The TMC2240 datasheet says that the maximum here is 15 rather than 14, but that looks
               --  to be an off-by-one error as the default sine wave peak is 248. 248 + 16/2 = 256 but
               --  the maximum is probably actually 255.
               Report
                 (["CHM", "SpreadCycle", "Manual"],
                  "HSTRT + HEND must be less than 15 unless IRUN is reduced to 0.97 or below as otherwise the "
                  & "hysteresis start setting will be greater than the full scale current, leading to incorrect "
                  & "operation.");
            end if;
         elsif Config.CHM.Kind = SpreadCycle and then Config.CHM.SpreadCycle.Kind = Derived then
            Result.CHOPCONF.CHM := SpreadCycle_Mode;
            declare
               Sum_Too_High                : Boolean;
               Sum_Too_High_For_Full_Scale : Boolean;
               Excessive_Heating           : Boolean;
               Driver_Voltage_Too_Low      : Boolean;
            begin
               TMC_Types.TMC2240.Optimize_Spreadcycle
                 (Driver_Voltage              => Config.CHM.SpreadCycle.Derived.Input_Voltage,
                  TBL                         => Result.CHOPCONF.TBL,
                  Motor_Inductance            => Config.CHM.SpreadCycle.Derived.Phase_Inductance,
                  Motor_Resistance            => Config.CHM.SpreadCycle.Derived.Phase_Resistance,
                  Motor_Peak_Current          => Config.Run_Current,
                  TOFF                        => Result.CHOPCONF.TOFF,
                  IRUN                        => Result.IHOLD_IRUN.I_Run,
                  HSTRT                       => Result.CHOPCONF.HSTRT_TFD210,
                  HEND                        => Result.CHOPCONF.HEND_OFFSET,
                  Sum_Too_High                => Sum_Too_High,
                  Sum_Too_High_For_Full_Scale => Sum_Too_High_For_Full_Scale,
                  Excessive_Heating           => Excessive_Heating,
                  Driver_Voltage_Too_Low      => Driver_Voltage_Too_Low);

               if Sum_Too_High then
                  Report
                    (["CHM", "SpreadCycle", "Derived"],
                     "Automatically computed hysteresis sum is too high. Check that motor parameters are "
                     & "correct. If parameters are correct then decrease TBL, decrease IRUN, or use manual "
                     & "tuning.");
               elsif Sum_Too_High_For_Full_Scale and Result.IHOLD_IRUN.I_Run = 31 then
                  Report
                    (["CHM", "SpreadCycle", "Derived"],
                     "Automatically computed hysteresis sum is too high. Check that motor parameters are "
                     & "correct. If parameters are correct then decrease TBL, decrease IRUN, or use manual "
                     & "tuning. A very small reduction of IRUN to 0.97 will allow the computed parameters "
                     & "to be used.");
               end if;

               if Excessive_Heating then
                  Report
                    (["CHM", "SpreadCycle", "Derived"],
                     "The stepper motor is likely to heat up excessively at the given driver voltage. "
                     & "Check that parameters are correct. If parameters are correct and you still want to "
                     & "use this motor then use manual tuning.");
               end if;

               if Driver_Voltage_Too_Low then
                  Report
                    (["CHM", "SpreadCycle", "Derived"],
                     "The stepper motor requires a higher driver voltage to reach full current. Check that "
                     & "parameters are correct. If parameters are correct and you still want to use this "
                     & "motor then use manual tuning.");
               end if;
            end;
         end if;

         --  The TMC2240 datasheet says that the maximum here is 15 rather than 14, but that looks to be an
         --  off-by-one error as the default sine wave peak is 248. 248 + 16/2 = 256 but the maximum is
         --  probably actually 255.
         if Result.CHOPCONF.CHM = TMC_Types.TMC2240.SpreadCycle_Mode
           and
             (Dimensionless (Result.CHOPCONF.HEND_OFFSET) - 3.0 + Dimensionless (Result.CHOPCONF.HSTRT_TFD210) + 1.0
              > 14.0)
           and (Result.IHOLD_IRUN.I_Run = 31)
         then
            raise Constraint_Error with "Invalid config should have been caught earlier.";
         end if;
      end return;
   end Generate_Default_Registers;

end Prunt.Default_Modules.TMC_Drivers;
