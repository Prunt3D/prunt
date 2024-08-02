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

with Prunt.Controller.Gcode_Handler;
with Ada.Task_Termination;
with Ada.Task_Identification;
with Prunt.Thermistors;
with Prunt.TMC_Types.TMC2240;
with Prunt.TMC_Types;
with Ada.Text_IO;
with Prunt.Logger;

use type Prunt.TMC_Types.TMC2240.UART_CRC;
use type Prunt.TMC_Types.TMC2240.UART_Node_Address;
use type Prunt.TMC_Types.TMC2240.UART_Register_Address;
use type Prunt.TMC_Types.Unsigned_1;
use type Prunt.TMC_Types.Unsigned_2;
use type Prunt.TMC_Types.Unsigned_3;
use type Prunt.TMC_Types.Unsigned_4;
use type Prunt.TMC_Types.Unsigned_5;
use type Prunt.TMC_Types.Unsigned_6;
use type Prunt.TMC_Types.Unsigned_7;
use type Prunt.TMC_Types.Unsigned_8;
use type Prunt.TMC_Types.Unsigned_9;
use type Prunt.TMC_Types.Unsigned_10;
use type Prunt.TMC_Types.Unsigned_11;
use type Prunt.TMC_Types.Unsigned_12;
use type Prunt.TMC_Types.Unsigned_13;
use type Prunt.TMC_Types.Unsigned_14;
use type Prunt.TMC_Types.Unsigned_15;
use type Prunt.TMC_Types.Unsigned_16;
use type Prunt.TMC_Types.Unsigned_17;
use type Prunt.TMC_Types.Unsigned_18;
use type Prunt.TMC_Types.Unsigned_19;
use type Prunt.TMC_Types.Unsigned_20;
use type Prunt.TMC_Types.Unsigned_21;
use type Prunt.TMC_Types.Unsigned_22;
use type Prunt.TMC_Types.Unsigned_23;
use type Prunt.TMC_Types.Unsigned_24;
use type Prunt.TMC_Types.Unsigned_25;
use type Prunt.TMC_Types.Unsigned_26;
use type Prunt.TMC_Types.Unsigned_27;
use type Prunt.TMC_Types.Unsigned_28;
use type Prunt.TMC_Types.Unsigned_29;
use type Prunt.TMC_Types.Unsigned_30;
use type Prunt.TMC_Types.Unsigned_31;
use type Prunt.TMC_Types.Unsigned_32;

package body Prunt.Controller is

   --  Atomics are used rather than protected objects here to avoid any potential blocking in the stepgen task.
   type Atomic_Volatile_Position is new Position with
     Atomic_Components, Volatile_Components;
   Last_Position : Atomic_Volatile_Position := (others => Length (0.0));
   --  Note that each element here is updated atomically, not the entire position. This value is only meant to be used
   --  for displaying a value in the GUI, so it is not an issue if elements are out of sync.

   type Atomic_Volatile_Heater_Targets is new Heater_Targets with
     Atomic_Components, Volatile_Components;
   Last_Heater_Targets : Atomic_Volatile_Heater_Targets := (others => Temperature (0.0));

   Last_Temperatures : array (Thermistor_Name) of Temperature := (others => Temperature (0.0)) with
       Atomic_Components, Volatile_Components;

   Last_Heater_Powers : array (Heater_Name) of PWM_Scale := (others => PWM_Scale (0.0)) with
       Atomic_Components, Volatile_Components;

   type Atomic_Volatile_Heater_Thermistor_Map is new Heater_Thermistor_Map with
     Atomic_Components, Volatile_Components;
   Stored_Heater_Thermistors : Atomic_Volatile_Heater_Thermistor_Map;

   type Temperature_Report_Counter is mod 2**32;

   Last_Temperatures_Counters : array (Thermistor_Name) of Temperature_Report_Counter := (others => 0) with
     Atomic_Components, Volatile_Components;

   package My_Gcode_Handler is new Gcode_Handler;

   function Is_Homing_Move (Data : Flush_Extra_Data) return Boolean is
   begin
      return Data.Is_Homing_Move;
   end Is_Homing_Move;

   procedure Finished_Block (Data : Flush_Extra_Data; First_Segment_Accel_Distance : Length) is
   begin
      My_Gcode_Handler.Finished_Block (Data, First_Segment_Accel_Distance);
   end Finished_Block;

   function Get_Position return Position is
   begin
      return (for A in Axis_Name => Last_Position (A));
   end Get_Position;

   function Get_Temperature (Thermistor : Thermistor_Name) return Temperature is
   begin
      return Last_Temperatures (Thermistor);
   end Get_Temperature;

   function Get_Heater_Power (Heater : Heater_Name) return PWM_Scale is
   begin
      return Last_Heater_Powers (Heater);
   end Get_Heater_Power;

   procedure Submit_Gcode_Command (Command : String; Succeeded : out Boolean) is
   begin
      My_Gcode_Handler.Try_Queue_Command (Command, Succeeded);
   end Submit_Gcode_Command;

   procedure Submit_Gcode_File (Path : String; Succeeded : out Boolean) is
   begin
      My_Gcode_Handler.Try_Set_File (Path, Succeeded);
   end Submit_Gcode_File;

   task body GUI_Runner is
   begin
      accept Start;
      My_GUI.Run;
      accept Finish;
   end;

   procedure Run is
      Prunt_Params : My_Config.Prunt_Parameters;

   begin
      begin
         Ada.Task_Termination.Set_Specific_Handler
           (My_Planner.Runner'Identity, Fatal_Exception_Occurrence_Holder.all.Set'Access);
         Ada.Task_Termination.Set_Specific_Handler
           (My_Gcode_Handler.Runner'Identity, Fatal_Exception_Occurrence_Holder.all.Set'Access);
         Ada.Task_Termination.Set_Specific_Handler
           (My_Step_Generator.Runner'Identity, Fatal_Exception_Occurrence_Holder.all.Set'Access);

         GUI_Runner.Start;

         My_Config.Config_File.Read (Prunt_Params);

         if not Prunt_Params.Enabled then
            Logger.Log ("Prunt is disabled. Enable in config editor after setting other settings.");
         else
            begin
               Logger.Log ("Running setup.");

               Setup_Thermistors_And_Heater_Assignments;
               Setup_Planner;
               Setup_Step_Generator;
               Setup_Gcode_Handler;

               for S in Stepper_Name loop
                  Setup_Stepper (S);
               end loop;

               for H in Heater_Name loop
                  declare
                     Heater_Params : My_Config.Heater_Full_Parameters;
                  begin
                     My_Config.Config_File.Read (Heater_Params, H);
                     Reconfigure_Heater (H, Heater_Params.Params);
                  end;
               end loop;
            exception
               when E : Config_Constraint_Error =>
                  declare
                     Prunt_Params : My_Config.Prunt_Parameters;
                  begin
                     My_Config.Config_File.Read (Prunt_Params);
                     Prunt_Params.Enabled := False;
                     My_Config.Config_File.Write (Prunt_Params);
                  end;
                  raise;
            end;
         end if;

         Logger.Log ("Setup done.");
      exception
         when E : others =>
            Fatal_Exception_Occurrence_Holder.all.Set
              (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, E);
      end;

      GUI_Runner.Finish;
   end Run;

   procedure Report_Temperature (Thermistor : Thermistor_Name; Temp : Temperature) is
   begin
      Last_Temperatures (Thermistor)          := Temp;
      Last_Temperatures_Counters (Thermistor) := @ + 1;
   end Report_Temperature;

   procedure Report_Heater_Power (Heater : Heater_Name; Power : PWM_Scale) is
   begin
      Last_Heater_Powers (Heater) := Power;
   end Report_Heater_Power;

   --  TODO
   procedure Report_Last_Command_Executed (Index : Command_Index) is null;

   procedure Report_External_Error (Message : String) is
      External_Error : exception;
   begin
      raise External_Error with Message;
   exception
      when E : External_Error =>
         Fatal_Exception_Occurrence_Holder.Set
           (Ada.Task_Termination.Abnormal, Ada.Task_Identification.Current_Task, E);
   end Report_External_Error;

   procedure Report_External_Error (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      Fatal_Exception_Occurrence_Holder.Set
        (Ada.Task_Termination.Abnormal, Ada.Task_Identification.Current_Task, Occurrence);
   end Report_External_Error;

   First_Block : Boolean := True;

   procedure Start_Planner_Block (Data : Flush_Extra_Data; Last_Command_Index : Command_Index) is
   begin
      if First_Block then
         Reset_Position ([others => 0.0]);
         First_Block := False;
      end if;

      if Data.Is_Homing_Move then
         Wait_Until_Idle (Last_Command_Index);
         Setup_For_Loop_Move (Data.Home_Switch, Data.Home_Hit_On_State);
      end if;

      if Data.Is_Conditional_Move then
         Wait_Until_Idle (Last_Command_Index);
         Setup_For_Conditional_Move (Data.Conditional_Switch, Data.Conditional_Hit_On_State);
      end if;
   end Start_Planner_Block;

   procedure Enqueue_Command_Internal
     (Pos             : Position;
      Stepper_Pos     : Stepper_Position;
      Data            : Corner_Extra_Data;
      Index           : Command_Index;
      Loop_Until_Hit  : Boolean;
      Safe_Stop_After : Boolean)
   is
   begin
      Enqueue_Command
        ((Index           => Index,
          Pos             => Stepper_Pos,
          Fans            => Data.Fans,
          Heaters         => Data.Heaters,
          Safe_Stop_After => Safe_Stop_After,
          Loop_Until_Hit  => Loop_Until_Hit));
      Last_Position       := (for A in Axis_Name => Pos (A));
      Last_Heater_Targets := (for H in Heater_Name => Data.Heaters (H));
   end Enqueue_Command_Internal;

   procedure Finish_Planner_Block
     (Data                 : Flush_Extra_Data;
      Next_Block_Pos       : Stepper_Position;
      First_Accel_Distance : Length;
      Next_Command_Index   : Command_Index)
   is
   begin
      if Data.Is_Conditional_Move or Data.Is_Homing_Move then
         Wait_Until_Idle (Next_Command_Index - 1);
      end if;

      --  TODO: Should we require the user to implement this instead?
      if Data.Dwell_Time /= Time (0.0) then
         Wait_Until_Idle (Next_Command_Index - 1);
         delay Duration (Data.Dwell_Time / s);
      end if;

      if Data.Wait_For_Heater then
         declare
            Start_Counter : constant Temperature_Report_Counter :=
              Last_Temperatures_Counters (Stored_Heater_Thermistors (Data.Wait_For_Heater_Name));
         begin
            loop
               exit when Last_Temperatures_Counters (Stored_Heater_Thermistors (Data.Wait_For_Heater_Name)) /=
                 Start_Counter;
            end loop;
         end;
         loop
            exit when Last_Temperatures (Stored_Heater_Thermistors (Data.Wait_For_Heater_Name)) >=
              Last_Heater_Targets (Data.Wait_For_Heater_Name);
         end loop;
      end if;

      My_Gcode_Handler.Finished_Block (Data, First_Accel_Distance);
      Reset_Position (Next_Block_Pos);
   end Finish_Planner_Block;

   procedure Setup_Thermistors_And_Heater_Assignments is
      Thermistor_Params_Array : Thermistor_Parameters_Array_Type;
      Heater_Thermistors      : Heater_Thermistor_Map;
   begin
      for H in Heater_Name loop
         declare
            Heater_Params : My_Config.Heater_Full_Parameters;
         begin
            My_Config.Config_File.Read (Heater_Params, H);
            Heater_Thermistors (H)        := Heater_Params.Thermistor;
            Stored_Heater_Thermistors (H) := Heater_Params.Thermistor;
         end;
      end loop;

      for T in Thermistor_Name loop
         My_Config.Config_File.Read (Thermistor_Params_Array (T), T);
      end loop;


      Setup (Heater_Thermistors, Thermistor_Params_Array);
   end Setup_Thermistors_And_Heater_Assignments;

   procedure TMC2240_UART_Write_And_Validate (Message : TMC_Types.TMC2240.UART_Data_Message; Stepper : Stepper_Name)
   is
      Query          : TMC_Types.TMC2240.UART_Query_Message :=
        (Bytes_Mode => False,
         Content    =>
           (Node     => Message.Content.Node,
            Register => Message.Content.Register,
            others   => <>));
      Reply          : TMC_Types.TMC2240.UART_Data_Message;
      Receive_Failed : Boolean;

      use type TMC_Types.TMC2240.UART_Data_Message_Inner;
   begin
      Stepper_Hardware (Stepper).TMC2240_UART_Write (Message.Bytes);

      Query.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Query);
      Stepper_Hardware (Stepper).TMC2240_UART_Read (Query.Bytes, Receive_Failed, Reply.Bytes);
      if Receive_Failed then
         raise TMC_UART_Error with "No response from stepper " & Stepper'Image;
      elsif Reply.Content.CRC /= TMC_Types.TMC2240.Compute_CRC (Reply) then
         raise TMC_UART_Error with "Bad CRC from stepper " & Stepper'Image;
      elsif Reply.Content.Node /= 255 then
         raise TMC_UART_Error with "Bad node address from stepper " & Stepper'Image;
      elsif (Reply.Content with delta CRC => 0, Node => 0)
        /= (Message.Content with delta CRC => 0, Node => 0, Is_Write => TMC_Types.False)
      then
         raise TMC_UART_Error with "Data read from TMC stepper does not match sent data for stepper " & Stepper'Image;
      end if;
   exception
      when E : TMC_UART_Error =>
         Logger.Log ("Data from TMC2240_UART_Write_And_Validate after error:");
         Logger.Log ("Sent: " & Message.Content'Image);
         Logger.Log ("Received: " & Reply.Content'Image);
         raise;
   end TMC2240_UART_Write_And_Validate;

   procedure Setup_Stepper (Stepper : Stepper_Name) is
      Stepper_Params : My_Config.Stepper_Parameters;

   begin
      My_Config.Config_File.Read (Stepper_Params, Stepper);

      case Stepper_Hardware (Stepper).Kind is
         when Basic_Kind =>
            null;
         when TMC2240_UART_Kind =>
            if Stepper_Params.TOFF = 0 and Stepper_Params.Enabled then
               raise Config_Constraint_Error
                 with "TOFF of 0 disables output for stepper " & Stepper'Image &
                 ". This is not allowed for enabled steppers.";
            end if;

            declare
               Query          : TMC_Types.TMC2240.UART_Query_Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node     => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register => TMC_Types.TMC2240.IOIN_Address,
                     others    => <>));
               Reply          : TMC_Types.TMC2240.UART_Data_Message;
               Receive_Failed : Boolean;
            begin
               Query.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Query);
               Stepper_Hardware (Stepper).TMC2240_UART_Read (Query.Bytes, Receive_Failed, Reply.Bytes);
               if Receive_Failed then
                  raise TMC_UART_Error with "No response from stepper " & Stepper'Image;
               elsif Reply.Content.CRC /= TMC_Types.TMC2240.Compute_CRC (Reply) then
                  raise TMC_UART_Error with "Bad CRC from stepper " & Stepper'Image;
               elsif Reply.Content.Register /= TMC_Types.TMC2240.IOIN_Address then
                  raise TMC_UART_Error with "Wrong register from stepper " & Stepper'Image;
               elsif Reply.Content.IOIN_Data.Version /= 16#40# then
                  raise TMC_UART_Error
                    with "Unexpected version from " & Stepper'Image & " (" &
                    Reply.Content.IOIN_Data.Version'Image & ")";
               end if;
            end;

            declare
               Global_Scaler : TMC_Types.TMC2240.Global_Scaler_Type;
               Current_Range : TMC_Types.TMC2240.Current_Range_Type;
               Message       : TMC_Types.TMC2240.UART_Data_Message;
            begin
               if Stepper_Params.Output_Current > 3.0 * amp then
                  raise Config_Constraint_Error
                    with "Current must not be greater than 3A for stepper " & Stepper'Image;
               elsif Stepper_Params.Output_Current = 3.0 * amp then
                  Current_Range := TMC_Types.TMC2240.Max_3A;
                  Global_Scaler := 0;
               elsif Stepper_Params.Output_Current > 2.0 * amp then
                  Current_Range := TMC_Types.TMC2240.Max_3A;
                  Global_Scaler :=
                    TMC_Types.TMC2240.Global_Scaler_Type
                      (Dimensionless'Floor (Stepper_Params.Output_Current / (3.0 * amp) * 256.0));
               elsif Stepper_Params.Output_Current = 2.0 * amp then
                  Current_Range := TMC_Types.TMC2240.Max_2A;
                  Global_Scaler := 0;
               elsif Stepper_Params.Output_Current > 1.0 * amp then
                  Current_Range := TMC_Types.TMC2240.Max_2A;
                  Global_Scaler :=
                    TMC_Types.TMC2240.Global_Scaler_Type
                      (Dimensionless'Floor (Stepper_Params.Output_Current / (2.0 * amp) * 256.0));
               elsif Stepper_Params.Output_Current = 1.0 * amp then
                  Current_Range := TMC_Types.TMC2240.Max_1A;
                  Global_Scaler := 0;
               elsif Stepper_Params.Output_Current >= 0.125 * amp then
                  Current_Range := TMC_Types.TMC2240.Max_1A;
                  Global_Scaler :=
                    TMC_Types.TMC2240.Global_Scaler_Type
                      (Dimensionless'Max
                         (32.0,
                            Dimensionless'Floor (Stepper_Params.Output_Current / (1.0 * amp) * 256.0)));
               else
                  raise Config_Constraint_Error
                    with "Current must not be less than 0.125A for stepper " & Stepper'Image;
               end if;

               Message             :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node         => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register     => TMC_Types.TMC2240.DRV_CONF_Address,
                     DRV_CONF_Data =>
                       (Current_Range => Current_Range,
                        Reserved_1    => 0,
                        Slope_Control => Stepper_Params.Slope_Control,
                        Reserved_2    => 0),
                     others       => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message             :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node               => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register           => TMC_Types.TMC2240.GLOBAL_SCALER_Address,
                     GLOBAL_SCALER_Data => (Global_Scaler => Global_Scaler, Reserved => 0),
                     others             => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message             :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node            => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register        => TMC_Types.TMC2240.IHOLD_IRUN_Address,
                     IHOLD_IRUN_Data =>
                       (I_Hold       => Stepper_Params.I_Hold,
                        Reserved_1   => 0,
                        I_Run        => Stepper_Params.I_Run,
                        Reserved_2   => 0,
                        I_Hold_Delay => Stepper_Params.I_Hold_Delay,
                        Reserved_3   => 0,
                        I_Run_Delay  => Stepper_Params.I_Run_Delay,
                        Reserved_4   => 0),
                     others          => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message             :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node            => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register        => TMC_Types.TMC2240.TPOWERDOWN_Address,
                     TPOWERDOWN_Data => (T_Power_Down => Stepper_Params.T_Power_Down, Reserved => 0),
                     others          => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message             :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node          => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register      => TMC_Types.TMC2240.TPWMTHRS_Address,
                     TPWMTHRS_Data => (T_PWM_Thrs => Stepper_Params.T_PWM_Thrs, Reserved => 0),
                     others        => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message             :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node           => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register       => TMC_Types.TMC2240.TCOOLTHRS_Address,
                     TCOOLTHRS_Data => (T_Cool_Thrs => Stepper_Params.T_Cool_Thrs, Reserved => 0),
                     others         => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message             :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node       => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register   => TMC_Types.TMC2240.THIGH_Address,
                     THIGH_Data => (T_High => Stepper_Params.T_High, Reserved => 0),
                     others     => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message             :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node          => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register      => TMC_Types.TMC2240.CHOPCONF_Address,
                     CHOPCONF_Data =>
                       (TOFF                 => Stepper_Params.TOFF,
                        HSTRT_TFD210         => Stepper_Params.HSTRT_TFD210,
                        HEND_OFFSET          => Stepper_Params.HEND_OFFSET,
                        FD3                  => Stepper_Params.FD3,
                        DISFDCC              => TMC_Types.TMC_Boolean (Stepper_Params.DISFDCC),
                        Reserved_1           => 0,
                        CHM                  => TMC_Types.TMC_Boolean (Stepper_Params.CHM),
                        TBL                  => 2,
                        Reserved_2           => 0,
                        VHIGHFS              => TMC_Types.TMC_Boolean (Stepper_Params.VHIGHFS),
                        VHIGHCHM             => TMC_Types.TMC_Boolean (Stepper_Params.VHIGHCHM),
                        TPFD                 => Stepper_Params.TPFD,
                        Microstep_Resolution => Stepper_Params.Microstep_Resolution,
                        Interpolate          => TMC_Types.TMC_Boolean (False),
                        Double_Edge          => TMC_Types.TMC_Boolean (False),
                        Disable_S2G          => TMC_Types.TMC_Boolean (False),
                        Disable_S2Vs         => TMC_Types.TMC_Boolean (False)),
                     others        => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message             :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node         => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register     => TMC_Types.TMC2240.PWMCONF_Address,
                     PWMCONF_Data =>
                       (PWM_OFS            => Stepper_Params.PWM_OFS,
                        PWM_Grad           => Stepper_Params.PWM_Grad,
                        PWM_Freq           => Stepper_Params.PWM_Freq,
                        PWM_Auto_Scale     => TMC_Types.TMC_Boolean (Stepper_Params.PWM_Auto_Scale),
                        PWM_Auto_Grad      => TMC_Types.TMC_Boolean (Stepper_Params.PWM_Auto_Grad),
                        Freewheel          => Stepper_Params.Freewheel,
                        PWM_Meas_SD_Enable => TMC_Types.TMC_Boolean (Stepper_Params.PWM_Meas_SD_Enable),
                        PWM_Dis_Reg_Stst   => TMC_Types.TMC_Boolean (Stepper_Params.PWM_Dis_Reg_Stst),
                        PWM_Reg            => Stepper_Params.PWM_Reg,
                        PWM_Lim            => Stepper_Params.PWM_Lim),
                     others       => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);
            end;
      end case;

      if Stepper_Params.Enabled then
         Stepper_Hardware (Stepper).Enable_Stepper (Stepper);
      else
         Stepper_Hardware (Stepper).Disable_Stepper (Stepper);
      end if;
   end Setup_Stepper;

   procedure Setup_Planner is
      Kinematics_Params : My_Config.Kinematics_Parameters;
   begin
      My_Config.Config_File.Read (Kinematics_Params);

      if Kinematics_Params.Planner_Parameters.Tangential_Velocity_Max <= 0.0 * mm / s then
         raise Config_Constraint_Error with "Max velocity must be greater than 0.";
      end if;

      if Kinematics_Params.Planner_Parameters.Acceleration_Max <= 0.0 * mm / s**2 then
         raise Config_Constraint_Error with "Max acceleration must be greater than 0.";
      end if;

      if Kinematics_Params.Planner_Parameters.Jerk_Max <= 0.0 * mm / s**3 then
         raise Config_Constraint_Error with "Max jerk must be greater than 0.";
      end if;

      if Kinematics_Params.Planner_Parameters.Snap_Max <= 0.0 * mm / s**4 then
         raise Config_Constraint_Error with "Max snap must be greater than 0.";
      end if;

      if Kinematics_Params.Planner_Parameters.Crackle_Max <= 0.0 * mm / s**5 then
         raise Config_Constraint_Error with "Max crackle must be greater than 0.";
      end if;

      for A in Axis_Name loop
         if Kinematics_Params.Planner_Parameters.Axial_Velocity_Maxes (A) <= 0.0 * mm / s then
            raise Config_Constraint_Error with "Max " & A'Image & " velocity must be greater than 0.";
         end if;
      end loop;

      --  TODO: Check that scaler will not cause max step rate to be exceeded.

      My_Planner.Runner.Setup (Kinematics_Params.Planner_Parameters);
   end Setup_Planner;

   procedure Setup_Step_Generator is
      Map               : My_Step_Generator.Stepper_Pos_Map := [others => [others => Length'Last]];
      Kinematics_Params : My_Config.Kinematics_Parameters;

      Used_Steppers : array (Stepper_Name) of Boolean := [others => False];

      procedure Check_Stepper (S : Stepper_Name) is
         Stepper_Params : My_Config.Stepper_Parameters;
      begin
         My_Config.Config_File.Read (Stepper_Params, S);

         if not Stepper_Params.Enabled then
            raise Config_Constraint_Error with "Stepper " & S'Image & " attached to an axis but not enabled.";
         end if;

         if Used_Steppers (S) then
            raise Config_Constraint_Error with "Stepper " & S'Image & " attached to multiples axes.";
         end if;

         Used_Steppers (S) := True;
      end Check_Stepper;
   begin
      My_Config.Config_File.Read (Kinematics_Params);

      for S in Stepper_Name loop
         declare
            Stepper_Params : My_Config.Stepper_Parameters;
         begin
            My_Config.Config_File.Read (Stepper_Params, S);

            case Kinematics_Params.Kind is
               when My_Config.Cartesian_Kind =>
                  if Kinematics_Params.X_Steppers (S) then
                     Check_Stepper (S);
                     Map (X_Axis, S) := Stepper_Params.Mm_Per_Step;
                  end if;

                  if Kinematics_Params.Y_Steppers (S) then
                     Check_Stepper (S);
                     Map (Y_Axis, S) := Stepper_Params.Mm_Per_Step;
                  end if;
               when My_Config.Core_XY_Kind =>
                  if Kinematics_Params.A_Steppers (S) then
                     Check_Stepper (S);
                     Map (X_Axis, S) := 0.5 * Stepper_Params.Mm_Per_Step;
                     Map (Y_Axis, S) := 0.5 * Stepper_Params.Mm_Per_Step;
                  end if;

                  if Kinematics_Params.B_Steppers (S) then
                     Check_Stepper (S);
                     Map (X_Axis, S) := 0.5 * Stepper_Params.Mm_Per_Step;
                     Map (Y_Axis, S) := -0.5 * Stepper_Params.Mm_Per_Step;
                  end if;
            end case;

            if Kinematics_Params.Z_Steppers (S) then
               Check_Stepper (S);
               Map (Z_Axis, S) := Stepper_Params.Mm_Per_Step;
            end if;

            if Kinematics_Params.E_Steppers (S) then
               Check_Stepper (S);
               Map (E_Axis, S) := Stepper_Params.Mm_Per_Step;
            end if;
         end;
      end loop;

      My_Step_Generator.Runner.Setup (Map);
   end Setup_Step_Generator;

   procedure Setup_Gcode_Handler is
      Corner_Data : Corner_Extra_Data := (Fans => (others => 0.0), Heaters => (others => Temperature (0.0)));
      Fan_Params  : My_Config.Fan_Parameters;
   begin
      for F in Fan_Name loop
         My_Config.Config_File.Read (Fan_Params, F);
         case Fan_Params.Kind is
            when My_Config.Disabled_Kind =>
               Corner_Data.Fans (F) := 0.0;
            when My_Config.Dynamic_PWM_Kind =>
               Corner_Data.Fans (F) := 0.0;
            when My_Config.Always_On_Kind =>
               Corner_Data.Fans (F) := Fan_Params.Always_On_PWM;
         end case;
      end loop;
      My_Gcode_Handler.Runner.Start (Corner_Data);
   end Setup_Gcode_Handler;

end Prunt.Controller;
