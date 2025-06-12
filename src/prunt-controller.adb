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
with Prunt.Controller.Gcode_Handler;
with Ada.Task_Termination;
with Ada.Task_Identification;
with Prunt.TMC_Types.TMC2240;
with Prunt.TMC_Types;

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
   type Atomic_Volatile_Position is new Position with Atomic_Components, Volatile_Components;
   Last_Position : Atomic_Volatile_Position := (others => Length (0.0));
   --  Note that each element here is updated atomically, not the entire position. This value is only meant to be used
   --  for displaying a value in the GUI, so it is not an issue if elements are out of sync.

   type Atomic_Volatile_Heater_Targets is new Heater_Targets with Atomic_Components, Volatile_Components;
   Last_Heater_Targets : Atomic_Volatile_Heater_Targets := (others => Temperature (0.0));

   Last_Thermistor_Temperatures : array (Thermistor_Name) of Temperature := (others => Temperature (0.0))
   with Atomic_Components, Volatile_Components;

   Last_Stepper_Temperatures : array (Stepper_Name) of Temperature := (others => Temperature (0.0))
   with Atomic_Components, Volatile_Components;

   Last_Board_Temperatures : array (Board_Temperature_Probe_Name) of Temperature := (others => Temperature (0.0))
   with Atomic_Components, Volatile_Components;

   Last_Input_Switch_States : array (Input_Switch_Name) of Pin_State := (others => Low_State)
   with Atomic_Components, Volatile_Components;

   Last_Heater_Powers : array (Heater_Name) of PWM_Scale := (others => PWM_Scale (0.0))
   with Atomic_Components, Volatile_Components;

   Last_Tachometer_Frequencies : array (Fan_Name) of Frequency := (others => Frequency (0.0))
   with Atomic_Components, Volatile_Components;

   type Atomic_Volatile_Heater_Thermistor_Map is new Heater_Thermistor_Map with Atomic_Components, Volatile_Components;
   Stored_Heater_Thermistors : Atomic_Volatile_Heater_Thermistor_Map;

   type Temperature_Report_Counter is mod 2**32;

   Last_Thermistor_Temperatures_Counters : array (Thermistor_Name) of Temperature_Report_Counter := (others => 0)
   with Atomic_Components, Volatile_Components;

   Last_Line : File_Line_Count := 0
   with Atomic, Volatile;

   package My_Gcode_Handler is new Gcode_Handler;

   function Is_Homing_Move (Data : Flush_Resetting_Data) return Boolean is
   begin
      return Data.Is_Homing_Move;
   end Is_Homing_Move;

   procedure Finished_Block (Data : Flush_Resetting_Data; First_Segment_Accel_Distance : Length) is
   begin
      My_Gcode_Handler.Finished_Block (Data, First_Segment_Accel_Distance);
   end Finished_Block;

   function Get_Position return Position is
   begin
      return (for A in Axis_Name => Last_Position (A));
   end Get_Position;

   function Get_Temperature (Thermistor : Thermistor_Name) return Temperature is
   begin
      return Last_Thermistor_Temperatures (Thermistor);
   end Get_Temperature;

   function Get_Temperature (Stepper : Stepper_Name) return Temperature is
   begin
      return Last_Stepper_Temperatures (Stepper);
   end Get_Temperature;

   function Get_Temperature (Temperature_Probe : Board_Temperature_Probe_Name) return Temperature is
   begin
      if Board_Temperature_Probe_Name'First > Board_Temperature_Probe_Name'Last then
         --  This is here to keep GCC happy.
         return 0.0 * celsius;
      else
         return Last_Board_Temperatures (Temperature_Probe);
      end if;
   end Get_Temperature;

   function Get_Heater_Power (Heater : Heater_Name) return PWM_Scale is
   begin
      return Last_Heater_Powers (Heater);
   end Get_Heater_Power;

   function Get_Input_Switch_State (Switch : Input_Switch_Name) return Pin_State is
   begin
      return Last_Input_Switch_States (Switch);
   end Get_Input_Switch_State;

   function Get_Tachometer_Frequency (Fan : Fan_Name) return Frequency is
   begin
      return Last_Tachometer_Frequencies (Fan);
   end Get_Tachometer_Frequency;

   protected body Current_File_Name is
      function Get_File_Name return String is
      begin
         return Ada.Strings.Unbounded.To_String (File);
      end Get_File_Name;

      procedure Set_File_Name (Name : String) is
      begin
         File := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      end Set_File_Name;
   end Current_File_Name;

   function Get_Line return File_Line_Count is
   begin
      return Last_Line;
   end Get_Line;

   procedure Submit_Gcode_Command (Command : String; Succeeded : out Boolean) is
   begin
      My_Gcode_Handler.Try_Queue_Command (Command, Succeeded);
   end Submit_Gcode_Command;

   procedure Submit_Gcode_File (Path : String; Succeeded : out Boolean) is
   begin
      My_Gcode_Handler.Try_Set_File (Path, Succeeded);
   end Submit_Gcode_File;

   task body TMC_Temperature_Updater is
   begin
      loop
         accept Start;

         Inner :
         loop
            for S in Stepper_Name loop
               case Stepper_Hardware (S).Kind is
                  when Basic_Kind =>
                     null;

                  when TMC2240_UART_Kind =>
                     declare
                        Query          : TMC_Types.TMC2240.UART_Query_Message :=
                          (Bytes_Mode => False,
                           Content    =>
                             (Node     => Stepper_Hardware (S).TMC2240_UART_Address,
                              Register => TMC_Types.TMC2240.ADC_TEMP_Address,
                              others   => <>));
                        Receive_Failed : Boolean;
                        Reply          : TMC_Types.TMC2240.UART_Data_Message;
                     begin
                        Query.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Query);
                        Stepper_Hardware (S).TMC2240_UART_Read (Query.Bytes, Receive_Failed, Reply.Bytes);

                        if Receive_Failed then
                           null;
                        elsif Reply.Content.CRC /= TMC_Types.TMC2240.Compute_CRC (Reply) then
                           null;
                        elsif Reply.Content.Node /= 255 then
                           null;
                        elsif Reply.Content.Register /= Query.Content.Register then
                           null;
                        else
                           Last_Stepper_Temperatures (S) :=
                             Temperature (Reply.Content.ADC_TEMP_Data.ADC_Temp) - 264.675 * celsius;
                        end if;
                     end;
               end case;
            end loop;

            select
               accept Reset;
               exit Inner;
            or
               delay 1.0;
            end select;
         end loop Inner;
      end loop;
   end TMC_Temperature_Updater;

   procedure Prompt_For_Update is
   begin
      My_Web_Server.Wait_For_User_To_Allow_Update;
   end Prompt_For_Update;

   procedure Run is
   begin
      if not Ada.Directories.Exists ("uploads") then
         Ada.Directories.Create_Directory ("uploads");
      end if;

      Ada.Task_Termination.Set_Specific_Handler
        (My_Planner.Runner'Identity, Fatal_Exception_Occurrence_Holder.all.Set'Access);
      Ada.Task_Termination.Set_Specific_Handler
        (My_Gcode_Handler.Runner'Identity, Fatal_Exception_Occurrence_Holder.all.Set'Access);
      Ada.Task_Termination.Set_Specific_Handler
        (My_Step_Generator.Runner'Identity, Fatal_Exception_Occurrence_Holder.all.Set'Access);
      Ada.Task_Termination.Set_Specific_Handler
        (TMC_Temperature_Updater'Identity, Fatal_Exception_Occurrence_Holder.all.Set'Access);
      My_Web_Server.Task_Termination_Set_Specific_Handler (Fatal_Exception_Occurrence_Holder.all.Set'Access);

      <<Restart_Main>>
      Main :
      loop
         declare
            Is_Config_Valid : Boolean := True;

            procedure Log_Config_Error (Key, Message : String) is
            begin
               Is_Config_Valid := False;
               My_Logger.Log ("Config error: " & Key & ": " & Message);
            end Log_Config_Error;
         begin
            --  Configuration validation and initial setup
            My_Config.Validate_Initial_Config (Log_Config_Error'Access);

            if My_Config.Prunt_Is_Enabled and not Is_Config_Valid then
               My_Logger.Log ("Prunt disabled due to invalid config.");
               My_Config.Disable_Prunt;
               My_Config.Reset;
               My_Web_Server.Reset;
               goto Restart_Main;
            end if;
         end;

         if not My_Config.Prunt_Is_Enabled then
            My_Logger.Log ("Prunt is disabled. Enable in config editor after setting other settings.");

            declare
               Fatal_Exception : Ada.Exceptions.Exception_Occurrence;
            begin
               select
                  Reload_Signal.Wait;
                  goto Restart_Main;
               then abort
                  Fatal_Exception_Occurrence_Holder.Get (Fatal_Exception);
                  exit Main;
               end select;
            end;
         end if;

         declare
            Prunt_Params : My_Config.Prunt_Parameters;
         begin
            My_Config.Read (Prunt_Params);

            My_Logger.Log ("Running setup.");

            Setup_Thermistors_And_Heater_Assignments;

            for F in Fan_Name loop
               declare
                  Fan_Params : My_Config.Fan_Parameters;
               begin
                  My_Config.Read (Fan_Params, F);

                  case Fan_Hardware (F).Kind is
                     when Fixed_Switching_Kind =>
                        Fan_Hardware (F).Reconfigure_Fixed_Switching_Fan (F, Fan_Params.PWM_Frequency);

                     when Low_Or_High_Side_Switching_Kind =>
                        Fan_Hardware (F).Reconfigure_Low_Or_High_Side_Switching_Fan
                          (F, Fan_Params.PWM_Frequency, Fan_Params.Use_High_Side_Switching);
                  end case;
               end;
            end loop;

            for S in Stepper_Name loop
               Setup_Stepper (S);
            end loop;

            for H in Heater_Name loop
               declare
                  Heater_Params : My_Config.Heater_Full_Parameters;
               begin
                  My_Config.Read (Heater_Params, H);
                  Reconfigure_Heater (H, Heater_Params.Params);
               end;
            end loop;
         end;

         TMC_Temperature_Updater.Start;

         My_Gcode_Handler.Runner.Start;
         Setup_Step_Generator;

         My_Logger.Log ("Setup done.");

         My_Web_Server.Notify_Startup_Done;

         declare
            Fatal_Exception : Ada.Exceptions.Exception_Occurrence;
         begin
            select
               Reload_Signal.Wait;
            then abort
               Fatal_Exception_Occurrence_Holder.Get (Fatal_Exception);
               exit Main;
            end select;
         end;

         My_Logger.Log ("Reload requested. Resetting...");

         loop
            My_Step_Generator.Pause;
            select
               My_Step_Generator.Runner.Reset;
               exit;
            or
               delay 1.0;
               --  Rather than making sure the user can't click resume we instead just call Pause repeatedly until the
               --  Reset entry call is accepted.
            end select;
         end loop;
         My_Planner.Reset;
         My_Gcode_Handler.Runner.Reset;
         TMC_Temperature_Updater.Reset;
         My_Config.Reset;
         Reset;
         My_Web_Server.Reset;
      end loop Main;

      --  Currently we only exit the above loop if there is an exception in a different task.

      raise Constraint_Error with "Error in other task.";
   exception
      when E : others =>
         Fatal_Exception_Occurrence_Holder.all.Set
           (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, E);

         for I in 1 .. 100 loop
            My_Step_Generator.Pause;
            delay 0.1;
            --  Make sure the step generator stays paused even if the user is sending resume requests.
         end loop;

         Reset;

         raise;
   end Run;

   procedure Report_Input_Switch_State (Switch : Input_Switch_Name; State : Pin_State) is
   begin
      Last_Input_Switch_States (Switch) := State;
   end Report_Input_Switch_State;

   procedure Report_Temperature (Thermistor : Thermistor_Name; Temp : Temperature) is
   begin
      Last_Thermistor_Temperatures (Thermistor) := Temp;
      Last_Thermistor_Temperatures_Counters (Thermistor) := @ + 1;
   end Report_Temperature;

   procedure Report_Temperature (Temperature_Probe : Board_Temperature_Probe_Name; Temp : Temperature) is
   begin
      Last_Board_Temperatures (Temperature_Probe) := Temp;
   end Report_Temperature;

   procedure Report_Heater_Power (Heater : Heater_Name; Power : PWM_Scale) is
   begin
      Last_Heater_Powers (Heater) := Power;
   end Report_Heater_Power;

   procedure Report_Tachometer_Frequency (Fan : Fan_Name; Freq : Frequency) is
   begin
      Last_Tachometer_Frequencies (Fan) := Freq;
   end Report_Tachometer_Frequency;

   --  TODO
   procedure Report_Last_Command_Executed (Index : Command_Index) is null;

   procedure Report_External_Error (Message : String) is
      External_Error : exception;
   begin
      My_Config.Disable_Prunt;
      raise External_Error with Message;
   exception
      when E : External_Error =>
         Fatal_Exception_Occurrence_Holder.Set
           (Ada.Task_Termination.Abnormal, Ada.Task_Identification.Current_Task, E);
   end Report_External_Error;

   procedure Report_External_Error (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      My_Config.Disable_Prunt;
      Fatal_Exception_Occurrence_Holder.Set
        (Ada.Task_Termination.Abnormal, Ada.Task_Identification.Current_Task, Occurrence);
   end Report_External_Error;

   First_Block : Boolean := True;

   procedure Start_Planner_Block
     (Resetting_Data     : Flush_Resetting_Data;
      Persistent_Data    : Block_Persistent_Data;
      Last_Command_Index : Command_Index) is
   begin
      if First_Block then
         Reset_Position ([others => 0.0]);
         First_Block := False;
      end if;

      if Resetting_Data.Is_Homing_Move then
         Wait_Until_Idle (Last_Command_Index);
         Setup_For_Loop_Move (Resetting_Data.Home_Switch, Resetting_Data.Home_Hit_On_State);
      end if;

      if Resetting_Data.Is_Conditional_Move then
         Wait_Until_Idle (Last_Command_Index);
         Setup_For_Conditional_Move (Resetting_Data.Conditional_Switch, Resetting_Data.Conditional_Hit_On_State);
      end if;

      Current_File_Name.Set_File_Name (Ada.Strings.Unbounded.To_String (Persistent_Data.Current_File));
   end Start_Planner_Block;

   procedure Enqueue_Command_Internal
     (Pos             : Position;
      Stepper_Pos     : Stepper_Position;
      Data            : Corner_Extra_Data;
      Index           : Command_Index;
      Loop_Until_Hit  : Boolean;
      Safe_Stop_After : Boolean) is
   begin
      Enqueue_Command
        ((Index           => Index,
          Pos             => Stepper_Pos,
          Fans            => Data.Fans,
          Heaters         => Data.Heaters,
          Safe_Stop_After => Safe_Stop_After,
          Loop_Until_Hit  => Loop_Until_Hit));
      Last_Position := (for A in Axis_Name => Pos (A));
      Last_Heater_Targets := (for H in Heater_Name => Data.Heaters (H));
      Last_Line := Data.Current_Line;
   end Enqueue_Command_Internal;

   procedure Finish_Planner_Block
     (Resetting_Data       : Flush_Resetting_Data;
      Persistent_Data      : Block_Persistent_Data;
      Next_Block_Pos       : Stepper_Position;
      First_Accel_Distance : Length;
      Next_Command_Index   : Command_Index) is
   begin
      if Resetting_Data.Is_Conditional_Move or Resetting_Data.Is_Homing_Move then
         Wait_Until_Idle (Next_Command_Index - 1);
      end if;

      --  TODO: Should we require the user to implement this instead for greater precision?
      if Resetting_Data.Dwell_Time /= Time (0.0) then
         Wait_Until_Idle (Next_Command_Index - 1);
         delay Duration (Resetting_Data.Dwell_Time / s);
      end if;

      if Resetting_Data.Pause_After then
         My_Step_Generator.Pause;
      end if;

      if Resetting_Data.Wait_For_Heater then
         declare
            Start_Counter : constant Temperature_Report_Counter :=
              Last_Thermistor_Temperatures_Counters (Stored_Heater_Thermistors (Resetting_Data.Wait_For_Heater_Name));
         begin
            loop
               exit when
                 Last_Thermistor_Temperatures_Counters
                   (Stored_Heater_Thermistors (Resetting_Data.Wait_For_Heater_Name))
                 /= Start_Counter;
            end loop;
         end;
         loop
            exit when
              Last_Thermistor_Temperatures (Stored_Heater_Thermistors (Resetting_Data.Wait_For_Heater_Name))
              >= Last_Heater_Targets (Resetting_Data.Wait_For_Heater_Name);
         end loop;
      end if;

      My_Gcode_Handler.Finished_Block (Resetting_Data, First_Accel_Distance);
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
            My_Config.Read (Heater_Params, H);
            Heater_Thermistors (H) := Heater_Params.Thermistor;
            Stored_Heater_Thermistors (H) := Heater_Params.Thermistor;
         end;
      end loop;

      for T in Thermistor_Name loop
         My_Config.Read (Thermistor_Params_Array (T), T);
      end loop;

      Setup (Heater_Thermistors, Thermistor_Params_Array);
   end Setup_Thermistors_And_Heater_Assignments;

   procedure TMC2240_UART_Write_And_Validate (Message : TMC_Types.TMC2240.UART_Data_Message; Stepper : Stepper_Name) is
      Query          : TMC_Types.TMC2240.UART_Query_Message :=
        (Bytes_Mode => False,
         Content    => (Node => Message.Content.Node, Register => Message.Content.Register, others => <>));
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
      when TMC_UART_Error =>
         My_Logger.Log ("Data from TMC2240_UART_Write_And_Validate after error:");
         My_Logger.Log ("Sent: " & Message.Content'Image);
         My_Logger.Log ("Received: " & Reply.Content'Image);
         raise;
   end TMC2240_UART_Write_And_Validate;

   procedure Setup_Stepper (Stepper : Stepper_Name) is
      Stepper_Params : My_Config.Stepper_Parameters;

   begin
      My_Config.Read (Stepper_Params, Stepper);

      case Stepper_Hardware (Stepper).Kind is
         when Basic_Kind =>
            null;

         when TMC2240_UART_Kind =>
            null;
            declare
               Query          : TMC_Types.TMC2240.UART_Query_Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node     => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register => TMC_Types.TMC2240.IOIN_Address,
                     others   => <>));
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
                    with
                      "Unexpected version from " & Stepper'Image & " (" & Reply.Content.IOIN_Data.Version'Image & ")";
               end if;
            end;

            declare
               Message : TMC_Types.TMC2240.UART_Data_Message;
            begin
               --  A delay greater than 8 bit times is required with multiple nodes or else nodes other than the
               --  addressed node may detect transmission errors during reads. Technically we should have a delay
               --  after reads until this is set for all nodes, but it's not currently an issue in any firmware
               --  implementations.
               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node          => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register      => TMC_Types.TMC2240.CHOPCONF_Address,
                     CHOPCONF_Data => (Stepper_Params.CHOPCONF with delta TOFF => TMC_Types.TMC2240.Disable_Driver),
                     others        => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);
               --  We set TOFF correctly later. Using it to disable the driver before setting other registers ensures
               --  that a half-configured driver will be in a safe state.

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node          => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register      => TMC_Types.TMC2240.NODECONF_Address,
                     NODECONF_Data => (Node_Addr => 0, Send_Delay => TMC_Types.TMC2240.Delay_3x8, Reserved => 0),
                     others        => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node       => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register   => TMC_Types.TMC2240.GCONF_Address,
                     GCONF_Data => Stepper_Params.GCONF,
                     others     => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node          => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register      => TMC_Types.TMC2240.DRV_CONF_Address,
                     DRV_CONF_Data => Stepper_Params.DRV_CONF,
                     others        => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node               => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register           => TMC_Types.TMC2240.GLOBAL_SCALER_Address,
                     GLOBAL_SCALER_Data => Stepper_Params.GLOBAL_SCALER,
                     others             => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node            => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register        => TMC_Types.TMC2240.IHOLD_IRUN_Address,
                     IHOLD_IRUN_Data => Stepper_Params.IHOLD_IRUN,
                     others          => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node            => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register        => TMC_Types.TMC2240.TPOWERDOWN_Address,
                     TPOWERDOWN_Data => Stepper_Params.TPOWERDOWN,
                     others          => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node          => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register      => TMC_Types.TMC2240.TPWMTHRS_Address,
                     TPWMTHRS_Data => Stepper_Params.TPWMTHRS,
                     others        => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node           => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register       => TMC_Types.TMC2240.TCOOLTHRS_Address,
                     TCOOLTHRS_Data => Stepper_Params.TCOOLTHRS,
                     others         => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node       => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register   => TMC_Types.TMC2240.THIGH_Address,
                     THIGH_Data => Stepper_Params.THIGH,
                     others     => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node         => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register     => TMC_Types.TMC2240.PWMCONF_Address,
                     PWMCONF_Data => Stepper_Params.PWMCONF,
                     others       => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node          => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register      => TMC_Types.TMC2240.CHOPCONF_Address,
                     CHOPCONF_Data => Stepper_Params.CHOPCONF,
                     others        => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);
            end;
      end case;

      if Stepper_Params.Enabled then
         Enable_Stepper (Stepper);
      else
         Disable_Stepper (Stepper);
      end if;
   end Setup_Stepper;

   procedure Setup_Step_Generator is
      Map               : My_Step_Generator.Stepper_Pos_Map := [others => [others => Length'Last]];
      Kinematics_Params : My_Config.Kinematics_Parameters;
   begin
      My_Config.Read (Kinematics_Params);

      for S in Stepper_Name loop
         declare
            Stepper_Params : My_Config.Stepper_Parameters;
         begin
            My_Config.Read (Stepper_Params, S);

            case Kinematics_Params.Kind is
               when My_Config.Cartesian_Kind =>
                  if Kinematics_Params.X_Steppers (S) then
                     Map (X_Axis, S) := Stepper_Params.Mm_Per_Step;
                  end if;

                  if Kinematics_Params.Y_Steppers (S) then
                     Map (Y_Axis, S) := Stepper_Params.Mm_Per_Step;
                  end if;

               when My_Config.Core_XY_Kind =>
                  if Kinematics_Params.A_Steppers (S) then
                     Map (X_Axis, S) := 0.5 * Stepper_Params.Mm_Per_Step;
                     Map (Y_Axis, S) := 0.5 * Stepper_Params.Mm_Per_Step;
                  end if;

                  if Kinematics_Params.B_Steppers (S) then
                     Map (X_Axis, S) := 0.5 * Stepper_Params.Mm_Per_Step;
                     Map (Y_Axis, S) := -0.5 * Stepper_Params.Mm_Per_Step;
                  end if;
            end case;

            if Kinematics_Params.Z_Steppers (S) then
               Map (Z_Axis, S) := Stepper_Params.Mm_Per_Step;
            end if;

            if Kinematics_Params.E_Steppers (S) then
               Map (E_Axis, S) := Stepper_Params.Mm_Per_Step;
            end if;
         end;
      end loop;

      My_Step_Generator.Runner.Setup (Map);
   end Setup_Step_Generator;

   procedure Log (Message : String) is
   begin
      My_Logger.Log (Message);
   end Log;

   function Get_Axial_Shaper_Parameters (Data : Block_Persistent_Data) return Input_Shapers.Axial_Shaper_Parameters is
   begin
      return Data.Shaper_Parameters;
   end Get_Axial_Shaper_Parameters;

   protected body Reload_Signal is
      entry Wait when Reload_Requested is
      begin
         Reload_Requested := False;
      end Wait;

      procedure Signal is
      begin
         Reload_Requested := True;
      end Signal;
   end Reload_Signal;

   procedure Signal_Reload is
   begin
      Reload_Signal.Signal;
   end Signal_Reload;

   procedure Enable_Stepper (Stepper : Stepper_Name) is
   begin
      case Stepper_Hardware (Stepper).Kind is
         when Basic_Kind =>
            Stepper_Hardware (Stepper).Enable_Stepper (Stepper);

         when TMC2240_UART_Kind =>
            declare
               Stepper_Params : My_Config.Stepper_Parameters;
               Message        : TMC_Types.TMC2240.UART_Data_Message;
            begin
               My_Config.Read (Stepper_Params, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node          => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register      => TMC_Types.TMC2240.CHOPCONF_Address,
                     CHOPCONF_Data => Stepper_Params.CHOPCONF,
                     others        => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);
            end;
      end case;
   end Enable_Stepper;

   procedure Disable_Stepper (Stepper : Stepper_Name) is
   begin
      case Stepper_Hardware (Stepper).Kind is
         when Basic_Kind =>
            Stepper_Hardware (Stepper).Disable_Stepper (Stepper);

         when TMC2240_UART_Kind =>
            declare
               Stepper_Params : My_Config.Stepper_Parameters;
               Message        : TMC_Types.TMC2240.UART_Data_Message;
            begin
               My_Config.Read (Stepper_Params, Stepper);

               Message :=
                 (Bytes_Mode => False,
                  Content    =>
                    (Node          => Stepper_Hardware (Stepper).TMC2240_UART_Address,
                     Register      => TMC_Types.TMC2240.CHOPCONF_Address,
                     CHOPCONF_Data => (Stepper_Params.CHOPCONF with delta TOFF => TMC_Types.TMC2240.Disable_Driver),
                     others        => <>));
               Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
               TMC2240_UART_Write_And_Validate (Message, Stepper);
            end;
      end case;
   end Disable_Stepper;

end Prunt.Controller;
