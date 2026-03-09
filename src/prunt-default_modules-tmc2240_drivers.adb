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

with Ada.Real_Time;

package body Prunt.Default_Modules.TMC2240_Drivers is

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
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map is
      function TMC2240_Fields
        (Kind : Status_Manager.Status_Value_Kind; Unit : Virtual_String; Description : Virtual_String)
         return Status_Manager.Status_Value_Maps.Map
      is
         Result : Status_Manager.Status_Value_Maps.Map;
      begin
         for M in My_Controller_Generic_Types.Motor_Name loop
            if Motor_Hardware (M).Kind in TMC2240_UART_Kind then
               Result.Insert
                 (+M'Image,
                  (Kind        => Kind,
                   Unit        => Unit,
                   Description => Description & Conversions.To_Virtual_String (M'Image),
                   Condition   => ""));
            end if;
         end loop;
         return Result;
      end TMC2240_Fields;
   begin
      return
        ["Temperature"                 =>
           TMC2240_Fields (Status_Manager.Real_Kind, "°C", "Driver temperature of motor "),
         "StallGuard value"            =>
           TMC2240_Fields (Status_Manager.Integer_Kind, "", "StallGuard result of motor "),
         "Supply voltage"              => TMC2240_Fields (Status_Manager.Real_Kind, "V", "Supply voltage of motor "),
         "Overtemperature"             =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Overtemperature flag of motor "),
         "Overtemperature pre-warning" =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Overtemperature pre-warning of motor "),
         "Stall detected"              =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Stall detected for motor "),
         "Driver error"                =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Driver error flag of motor "),
         "Undervoltage charge pump"    =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Undervoltage charge pump flag of motor "),
         "VM undervoltage"             =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "VM undervoltage flag of motor "),
         "Short to VS phase A"         =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Short to VS (phase A) flag of motor "),
         "Short to VS phase B"         =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Short to VS (phase B) flag of motor "),
         "StealthChop active"          =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "StealthChop active flag of motor "),
         "Full step active"            =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Full step active flag of motor "),
         "Short to GND phase A"        =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Short to GND (phase A) flag of motor "),
         "Short to GND phase B"        =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Short to GND (phase B) flag of motor "),
         "Open load phase A"           =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Open load (phase A) flag of motor "),
         "Open load phase B"           =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Open load (phase B) flag of motor "),
         "Motor standstill"            =>
           TMC2240_Fields (Status_Manager.Boolean_Kind, "", "Motor standstill flag of motor ")];
   end Status_Schema;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
   begin
      return Result : Module_Instance do
         Result.Initialize
           (Config_In                         => Config_Data_To_User_Config (Config_Data.Get),
            Motor_Drivers_Module_Instance_Ref => Get_Other_Instance (Motor_Drivers_Module.Module_Instance'Tag),
            Report_Config_Error               => Report_Config_Error,
            Status_Emitter_In                 => Status_Emitter);
      end return;
   end Initialize;

   overriding
   procedure Finalize (Object : in out TMC_Motor_Manager) is
   begin
      case Object.Kind is
         when TMC2240_UART_Kind =>
            Object.UART.Get.Stop;

         when others            =>
            null;
      end case;
   end Finalize;

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
               Double_Edge          => TMC_Boolean (Motor_Hardware (Motor).Double_Edge_Stepping),
               Disable_S2G          => False,
               Disable_S2Vs         => False))
      do
         if Motor_Enabled then
            if Config.IRUN_During_Homing > Config.IRUN then
               Report (["IRUN_During_Homing"], "IRUN during homing must be less than or equal to IRUN.");
            end if;

            if Config.TOFF = Disable_Driver then
               Report
                 (["TOFF"],
                  "Setting TOFF to Disable_Driver will cause the motor to never be powered. If you do not want this "
                  & "motor to be used then use the motor disable toggle.");
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

   function MRES_To_Dimensionless (MRES : TMC_Types.TMC2240.Microstep_Resolution_Type) return Dimensionless is
   begin
      case MRES is
         when MS_256        =>
            return 256.0;

         when MS_128        =>
            return 128.0;

         when MS_64         =>
            return 64.0;

         when MS_32         =>
            return 32.0;

         when MS_16         =>
            return 16.0;

         when MS_8          =>
            return 8.0;

         when MS_4          =>
            return 4.0;

         when MS_2          =>
            return 2.0;

         when MS_Full_Steps =>
            return 1.0;
      end case;
   end MRES_To_Dimensionless;

   procedure Write_And_Validate
     (Message : TMC_Types.TMC2240.UART_Data_Message; Motor : My_Controller_Generic_Types.Motor_Name) is
   begin
      if Motor_Hardware (Motor).Kind /= TMC2240_UART_Kind then
         --  This is always going to be a slow procedure so it is fine to have a check here in release builds.
         raise Constraint_Error;
      end if;

      declare
         Message_With_Address : constant TMC_Types.TMC2240.UART_Data_Message :=
           (Message
            with delta Content => (Message.Content with delta Node => Motor_Hardware (Motor).TMC2240_UART_Address));
         Message_With_CRC     : constant TMC_Types.TMC2240.UART_Data_Message :=
           (Message_With_Address
            with delta Content => (Message_With_Address.Content with delta CRC => Message_With_Address.Compute_CRC));
         Query                : TMC_Types.TMC2240.UART_Query_Message :=
           (Bytes_Mode => False,
            Content    =>
              (Node => Message_With_CRC.Content.Node, Register => Message_With_CRC.Content.Register, others => <>));
         Reply                : TMC_Types.TMC2240.UART_Data_Message;
         Receive_Failed       : Boolean;
      begin
         Motor_Hardware (Motor).TMC2240_UART_Write (Message_With_CRC.Bytes);

         Query.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Query);
         Motor_Hardware (Motor).TMC2240_UART_Read (Query.Bytes, Receive_Failed, Reply.Bytes);
         if Receive_Failed then
            raise TMC_UART_Error with "No response from motor driver " & Motor'Image;
         elsif Reply.Content.CRC /= TMC_Types.TMC2240.Compute_CRC (Reply) then
            raise TMC_UART_Error with "Bad CRC from motor driver " & Motor'Image;
         elsif Reply.Content.Node /= 255 then
            raise TMC_UART_Error with "Bad node address from motor driver " & Motor'Image;
         elsif Query.Content.Register /= Reply.Content.Register then
            raise TMC_UART_Error
              with "Register address read from TMC stepper does not match sent data for stepper " & Motor'Image;
         elsif Query.Content.Register /= TMC_Types.TMC2240.GSTAT_Address
           and then
             (Reply.Content with delta CRC => 0, Node => 0)
             /= (Message.Content with delta CRC => 0, Node => 0, Is_Write => TMC_Types.False)
         then
            raise TMC_UART_Error with "Data read from TMC stepper does not match sent data for stepper " & Motor'Image;
         end if;
      exception
         when TMC_UART_Error =>
            My_Logger.Log ("Data from TMC2240 Write_And_Validate after error:");
            My_Logger.Log (+("Sent: " & Message_With_CRC.Content'Image));
            My_Logger.Log (+("Received: " & Reply.Content'Image));
            raise;
      end;
   end Write_And_Validate;

   function Read
     (Address : TMC_Types.TMC2240.UART_Register_Address; Motor : My_Controller_Generic_Types.Motor_Name)
      return TMC_Types.TMC2240.UART_Data_Message is
   begin
      if Motor_Hardware (Motor).Kind /= TMC2240_UART_Kind then
         --  This is always going to be a slow procedure so it is fine to have a check here in release builds.
         raise Constraint_Error;
      end if;

      declare
         Query          : TMC_Types.TMC2240.UART_Query_Message :=
           (Bytes_Mode => False,
            Content    => (Node => Motor_Hardware (Motor).TMC2240_UART_Address, Register => Address, others => <>));
         Reply          : TMC_Types.TMC2240.UART_Data_Message;
         Receive_Failed : Boolean;
      begin
         Query.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Query);

         Motor_Hardware (Motor).TMC2240_UART_Read (Query.Bytes, Receive_Failed, Reply.Bytes);

         if Receive_Failed then
            raise TMC_UART_Error with "No response from motor driver " & Motor'Image;
         elsif Reply.Content.CRC /= TMC_Types.TMC2240.Compute_CRC (Reply) then
            raise TMC_UART_Error with "Bad CRC from motor driver " & Motor'Image;
         elsif Reply.Content.Node /= 255 then
            raise TMC_UART_Error with "Bad node address from motor driver " & Motor'Image;
         elsif Query.Content.Register /= Reply.Content.Register then
            raise TMC_UART_Error
              with "Register address read from TMC stepper does not match sent data for stepper " & Motor'Image;
         end if;

         return Reply;
      exception
         when TMC_UART_Error =>
            My_Logger.Log ("Data from TMC2240 Read after error:");
            My_Logger.Log (+("Sent: " & Query.Content'Image));
            My_Logger.Log (+("Received: " & Reply.Content'Image));
            raise;
      end;
   end Read;

   overriding
   procedure Enable_Motor (This : in out UART_Motor_Handler) is
   begin
      This.Manager.Get.Enable;
   end Enable_Motor;

   overriding
   procedure Disable_Motor (This : in out UART_Motor_Handler) is
   begin
      This.Manager.Get.Disable;
   end Disable_Motor;

   task body UART_Motor_Manager is
      use type Ada.Real_TIme.Time;

      My_Regs        : TMC2240_Registers;
      My_Motor       : My_Controller_Generic_Types.Motor_Name;
      Status_Ref     : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Stop_Requested : Boolean := False;
      Next_Poll_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      select
         accept Setup
           (Regs           : TMC2240_Registers;
            Motor          : My_Controller_Generic_Types.Motor_Name;
            Status_Emitter : My_Modules.Status_Emitter_Shared_Pointers.Ref)
         do
            My_Regs := Regs;
            My_Motor := Motor;
            Status_Ref := Status_Emitter;

            if Motor_Hardware (Motor).Kind /= TMC2240_UART_Kind then
               --  This is always going to be a slow procedure so it is fine to have a check here in release builds.
               raise Constraint_Error;
            end if;

            Write_And_Validate
              ((Bytes_Mode => False, Content => (Register => GCONF_Address, GCONF_Data => Regs.GCONF, others => <>)),
               Motor);
            Write_And_Validate
              ((Bytes_Mode => False,
                Content    => (Register => DRV_CONF_Address, DRV_CONF_Data => Regs.DRV_CONF, others => <>)),
               Motor);
            Write_And_Validate
              ((Bytes_Mode => False,
                Content    =>
                  (Register => GLOBAL_SCALER_Address, GLOBAL_SCALER_Data => Regs.GLOBAL_SCALER, others => <>)),
               Motor);
            Write_And_Validate
              ((Bytes_Mode => False,
                Content    => (Register => IHOLD_IRUN_Address, IHOLD_IRUN_Data => Regs.IHOLD_IRUN, others => <>)),
               Motor);
            Write_And_Validate
              ((Bytes_Mode => False,
                Content    => (Register => TPOWERDOWN_Address, TPOWERDOWN_Data => Regs.TPOWERDOWN, others => <>)),
               Motor);
            Write_And_Validate
              ((Bytes_Mode => False,
                Content    => (Register => TPWMTHRS_Address, TPWMTHRS_Data => Regs.TPWMTHRS, others => <>)),
               Motor);
            Write_And_Validate
              ((Bytes_Mode => False,
                Content    => (Register => TCOOLTHRS_Address, TCOOLTHRS_Data => Regs.TCOOLTHRS, others => <>)),
               Motor);
            Write_And_Validate
              ((Bytes_Mode => False, Content => (Register => THIGH_Address, THIGH_Data => Regs.THIGH, others => <>)),
               Motor);
            Write_And_Validate
              ((Bytes_Mode => False,
                Content    => (Register => PWMCONF_Address, PWMCONF_Data => Regs.PWMCONF, others => <>)),
               Motor);
            Write_And_Validate
              ((Bytes_Mode => False,
                Content    => (Register => CHOPCONF_Address, CHOPCONF_Data => Regs.CHOPCONF, others => <>)),
               Motor);
         end Setup;
      or
         accept Stop;
         Stop_Requested := True;
      end select;

      while not Stop_Requested loop
         select
            accept Enable do
               if My_Regs.CHOPCONF.TOFF = Disable_Driver then
                  raise Constraint_Error with "Tried to enable motor which is disabled in config.";
               end if;

               Write_And_Validate
                 ((Bytes_Mode => False,
                   Content    => (Register => CHOPCONF_Address, CHOPCONF_Data => My_Regs.CHOPCONF, others => <>)),
                  My_Motor);
            end Enable;
         or
            accept Disable do
               Write_And_Validate
                 ((Bytes_Mode => False,
                   Content    =>
                     (Register      => CHOPCONF_Address,
                      CHOPCONF_Data => (My_Regs.CHOPCONF with delta TOFF => Disable_Driver),
                      others        => <>)),
                  My_Motor);
            end Disable;
         or
            accept Stop;
            Stop_Requested := True;
         or
            delay until Next_Poll_Time;
            Next_Poll_Time := Next_Poll_Time + Ada.Real_Time.Milliseconds (500);

            declare
               GSTAT_Reply : TMC_Types.TMC2240.UART_Data_Message;
            begin
               GSTAT_Reply := Read (TMC_Types.TMC2240.GSTAT_Address, My_Motor);
               Status_Ref.Get.Set_Value
                 ("Driver error", +My_Motor'Image, Boolean (GSTAT_Reply.Content.GSTAT_Data.Drv_Err));
               Status_Ref.Get.Set_Value
                 ("Undervoltage charge pump", +My_Motor'Image, Boolean (GSTAT_Reply.Content.GSTAT_Data.UV_CP));
               Status_Ref.Get.Set_Value
                 ("VM undervoltage", +My_Motor'Image, Boolean (GSTAT_Reply.Content.GSTAT_Data.VM_UVLO));
            exception
               when TMC_UART_Error =>
                  null;
            end;

            declare
               ADC_VSUPPLY_AIN_Reply : TMC_Types.TMC2240.UART_Data_Message;
            begin
               ADC_VSUPPLY_AIN_Reply := Read (TMC_Types.TMC2240.ADC_VSUPPLY_AIN_Address, My_Motor);
               Status_Ref.Get.Set_Value
                 ("Supply voltage",
                  +My_Motor'Image,
                  Dimensionless (ADC_VSUPPLY_AIN_Reply.Content.ADC_VSUPPLY_AIN_Data.ADC_V_Supply));
            exception
               when TMC_UART_Error =>
                  null;
            end;

            declare
               ADC_TEMP_Reply : TMC_Types.TMC2240.UART_Data_Message;
            begin
               ADC_TEMP_Reply := Read (TMC_Types.TMC2240.ADC_TEMP_Address, My_Motor);
               Status_Ref.Get.Set_Value
                 ("Temperature",
                  +My_Motor'Image,
                  Dimensionless (ADC_TEMP_Reply.Content.ADC_TEMP_Data.ADC_Temp) - 2038.0 * (10.0 / 77.0));
            exception
               when TMC_UART_Error =>
                  null;
            end;

            declare
               DRV_STATUS_Reply : TMC_Types.TMC2240.UART_Data_Message;
            begin
               DRV_STATUS_Reply := Read (TMC_Types.TMC2240.DRV_STATUS_Address, My_Motor);
               Status_Ref.Get.Set_Value
                 ("StallGuard value",
                  +My_Motor'Image,
                  Long_Long_Integer (DRV_STATUS_Reply.Content.DRV_STATUS_Data.SG_Result));
               Status_Ref.Get.Set_Value
                 ("Short to VS phase A", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.S2VSA));
               Status_Ref.Get.Set_Value
                 ("Short to VS phase B", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.S2VSB));
               Status_Ref.Get.Set_Value
                 ("StealthChop active", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.Stealth));
               Status_Ref.Get.Set_Value
                 ("Full step active", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.FSActive));
               Status_Ref.Get.Set_Value
                 ("Stall detected", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.StallGuard));
               Status_Ref.Get.Set_Value
                 ("Overtemperature", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.OT));
               Status_Ref.Get.Set_Value
                 ("Overtemperature pre-warning",
                  +My_Motor'Image,
                  Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.OTPW));
               Status_Ref.Get.Set_Value
                 ("Short to GND phase A", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.S2GA));
               Status_Ref.Get.Set_Value
                 ("Short to GND phase B", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.S2GB));
               Status_Ref.Get.Set_Value
                 ("Open load phase A", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.OLA));
               Status_Ref.Get.Set_Value
                 ("Open load phase B", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.OLB));
               Status_Ref.Get.Set_Value
                 ("Motor standstill", +My_Motor'Image, Boolean (DRV_STATUS_Reply.Content.DRV_STATUS_Data.STST));
            exception
               when TMC_UART_Error =>
                  null;
            end;
         end select;
      end loop;
   end UART_Motor_Manager;

   protected body Module_Instance is
      procedure Initialize
        (Config_In                         : User_Config;
         Motor_Drivers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Report_Config_Error               :
           access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
         Status_Emitter_In                 : My_Modules.Status_Emitter_Shared_Pointers.Ref)
      is
         Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance renames
           Motor_Drivers_Module.Module_Instance (Motor_Drivers_Module_Instance_Ref.Get.Element.all);

         function Create_UART_Manager return UART_Motor_Manager is
         begin
            return Result : UART_Motor_Manager;
         end Create_UART_Manager;
      begin
         Config := Config_In;
         Status_Emitter := Status_Emitter_In;

         for M in My_Controller_Generic_Types.Motor_Name loop
            case Config.Motors (M).Fixed_Kind is
               when TMC2240_UART_Kind =>
                  declare
                     Manager_Ref : UART_Motor_Manager_Pointers.Ref;
                  begin
                     Manager_Ref.Set (Create_UART_Manager'Access);
                     Managers (M) := (Kind => TMC2240_UART_Kind, UART => Manager_Ref);

                     Motor_Drivers_Module_Instance.Provide_Motor_Configuration
                       (M,
                        (Microsteps => MRES_To_Dimensionless (Config.Motors (M).TMC2240_Parameters.MRES)),
                        UART_Motor_Handler'(Motor_Drivers_Module.Motor_Handler with Manager => Manager_Ref));
                     --  We have to provide the motor config before creating the default registers as we need to get
                     --  back distance per unit.
                  end;

                  Registers (M) :=
                    Generate_Default_Registers
                      (Config.Motors (M).TMC2240_Parameters,
                       Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M),
                       Report_Config_Error,
                       M,
                       Motor_Drivers_Module_Instance.Distance_Per_Unit (M));

               when others            =>
                  null;
            end case;
         end loop;
      end Initialize;

      procedure Start is
      begin
         for M in My_Controller_Generic_Types.Motor_Name loop
            case Managers (M).Kind is
               when TMC2240_UART_Kind =>
                  Managers (M).UART.Get.Setup (Registers (M), M, Status_Emitter);

               when others            =>
                  null;
            end case;
         end loop;
      end Start;

      procedure Gcode_Dispatch
        (Args               : in out Gcode_Arguments.Arguments;
         Planner            : Planner_Interface'Class;
         Command_Identifier : Gcode_Command_Identifier) is
      begin
         raise Constraint_Error with "Not implemented.";
      end Gcode_Dispatch;
   end Module_Instance;


end Prunt.Default_Modules.TMC2240_Drivers;
