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

use type Prunt.TMC_Types.TMC2240.UART_CRC;
use type Prunt.TMC_Types.TMC2240.UART_Node_Address;
use type Prunt.TMC_Types.TMC2240.UART_Register_Address;
use type Prunt.TMC_Types.Unsigned_8;

package body Prunt.Controller is

   type Atomic_Volatile_Position is new Position with
     Atomic_Components, Volatile_Components;
   Last_Position : Atomic_Volatile_Position := (others => Length (0.0));

   Last_Temperatures : array (Thermistor_Name) of Temperature := (others => Temperature (0.0)) with
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

   function Get_Status_Message return String is
   begin
      return Status_Message.Get;
   end Get_Status_Message;

   function Get_Position return Position is
   begin
      return (for A in Axis_Name => Last_Position (A));
   end Get_Position;

   function Get_Temperature (Thermistor : Thermistor_Name) return Temperature is
   begin
      return Last_Temperatures (Thermistor);
   end Get_Temperature;

   procedure Submit_Gcode_Command (Command : String; Succeeded : out Boolean) is
   begin
      My_Gcode_Handler.Try_Queue_Command (Command, Succeeded);
   end Submit_Gcode_Command;

   procedure Submit_Gcode_File (Path : String; Succeeded : out Boolean) is
   begin
      My_Gcode_Handler.Try_Set_File (Path, Succeeded);
   end Submit_Gcode_File;

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

         My_Config.Config_File.Read (Prunt_Params);

         if not Prunt_Params.Enabled then
            Status_Message.Set ("Prunt is disabled. Enable in config editor after setting other settings.");
         else
            begin
               declare
                  Thermistor_Params_Array : Thermistor_Parameters_Array_Type;
                  Heater_Params_Array     : Heater_Parameters_Array_Type;
               begin
                  for H in Heater_Name loop
                     declare
                        Heater_Params : My_Config.Heater_Parameters;
                     begin
                        My_Config.Config_File.Read (Heater_Params, H);
                        case Heater_Params.Kind is
                           when My_Config.Disabled_Kind =>
                              Heater_Params_Array (H) :=
                                (Kind                 => Disabled_Kind,
                                 Thermistor           => Heater_Params.Thermistor,
                                 Max_Cumulative_Error => Heater_Params.Max_Cumulative_Error,
                                 Check_Gain_Time      => Heater_Params.Check_Gain_Time,
                                 Check_Minimum_Gain   => Heater_Params.Check_Minimum_Gain,
                                 Hysteresis           => Heater_Params.Hysteresis);
                           when My_Config.PID_Kind =>
                              Heater_Params_Array (H) :=
                                (Kind                        => PID_Kind,
                                 Thermistor                  => Heater_Params.Thermistor,
                                 Max_Cumulative_Error => Heater_Params.Max_Cumulative_Error,
                                 Check_Gain_Time             => Heater_Params.Check_Gain_Time,
                                 Check_Minimum_Gain          => Heater_Params.Check_Minimum_Gain,
                                 Hysteresis                  => Heater_Params.Hysteresis,
                                 Proportional_Scale          => Heater_Params.Proportional_Scale,
                                 Integral_Scale              => Heater_Params.Integral_Scale,
                                 Derivative_Scale            => Heater_Params.Derivative_Scale);
                           when My_Config.Bang_Bang_Kind =>
                              Heater_Params_Array (H) :=
                                (Kind                 => Bang_Bang_Kind,
                                 Thermistor           => Heater_Params.Thermistor,
                                 Max_Cumulative_Error => Heater_Params.Max_Cumulative_Error,
                                 Check_Gain_Time      => Heater_Params.Check_Gain_Time,
                                 Check_Minimum_Gain   => Heater_Params.Check_Minimum_Gain,
                                 Hysteresis           => Heater_Params.Hysteresis);
                        end case;
                     end;
                  end loop;

                  for T in Thermistor_Name loop
                     My_Config.Config_File.Read (Thermistor_Params_Array (T), T);
                  end loop;

                  Setup (Heater_Params_Array, Thermistor_Params_Array);
               end;

               for S in Stepper_Name loop
                  declare
                     Stepper_Params : My_Config.Stepper_Parameters;
                  begin
                     My_Config.Config_File.Read (Stepper_Params, S);

                     if Stepper_Params.Enabled then
                        Stepper_Hardware (S).Enable_Stepper (S);
                     else
                        Stepper_Hardware (S).Disable_Stepper (S);
                     end if;
                  end;
               end loop;

               for S in Stepper_Name loop
                  declare
                     Stepper_Params : My_Config.Stepper_Parameters;
                  begin
                     My_Config.Config_File.Read (Stepper_Params, S);

                     case Stepper_Hardware (S).Kind is
                        when Basic_Kind =>
                           null;
                        when TMC2240_UART_Kind =>
                           declare
                              Query          : TMC_Types.TMC2240.UART_Query_Message :=
                                (Bytes_Mode => False,
                                 Content    =>
                                   (Node     => Stepper_Hardware (S).TMC2240_UART_Address,
                                    Register => TMC_Types.TMC2240.IOIN_Address,
                                    others   => <>));
                              Reply          : TMC_Types.TMC2240.UART_Data_Message;
                              Receive_Failed : Boolean;
                           begin
                              Query.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Query);
                              Stepper_Hardware (S).TMC2240_UART_Read (Query.Bytes, Receive_Failed, Reply.Bytes);
                              if Receive_Failed then
                                 raise TMC_UART_Error with "No response from stepper " & S'Image;
                              elsif Reply.Content.CRC /= TMC_Types.TMC2240.Compute_CRC (Reply) then
                                 raise TMC_UART_Error with "Bad CRC from stepper " & S'Image;
                              elsif Reply.Content.Register /= TMC_Types.TMC2240.IOIN_Address then
                                 raise TMC_UART_Error with "Wrong register from stepper " & S'Image;
                              elsif Reply.Content.IOIN_Data.Version /= 16#40# then
                                 raise TMC_UART_Error
                                   with "Unexpected version from " & S'Image & " (" &
                                   Reply.Content.IOIN_Data.Version'Image & ")";
                              end if;
                           end;

                           --  TODO: Temporary.
                           declare
                              Message : TMC_Types.TMC2240.UART_Data_Message :=
                                (Bytes_Mode => False,
                                 Content    =>
                                   (Node          => Stepper_Hardware (S).TMC2240_UART_Address,
                                    Register      => TMC_Types.TMC2240.CHOPCONF_Address,
                                    CHOPCONF_Data =>
                                      (TOFF         => 3,
                                       HSTRT_TFD210 => 5,
                                       HEND_OFFSET  => 2,
                                       TBL          => 2#10#,
                                       TPFD         => 4,
                                       others       => <>),
                                    others        => <>));
                           begin
                              Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
                              Stepper_Hardware (S).TMC2240_UART_Write (Message.Bytes);
                           end;

                           --  TODO: Temporary.
                           declare
                              Message : TMC_Types.TMC2240.UART_Data_Message :=
                                (Bytes_Mode => False,
                                 Content    =>
                                   (Node          => Stepper_Hardware (S).TMC2240_UART_Address,
                                    Register      => TMC_Types.TMC2240.DRV_CONF_Address,
                                    DRV_CONF_Data => (Current_Range => TMC_Types.TMC2240.Max_1A, others => <>),
                                    others        => <>));
                           begin
                              Message.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Message);
                              Stepper_Hardware (S).TMC2240_UART_Write (Message.Bytes);
                           end;
                     end case;
                  end;
               end loop;

               declare
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

                  My_Planner.Runner.Setup (Kinematics_Params.Planner_Parameters);
               end;

               declare
                  Map               : My_Step_Generator.Stepper_Pos_Map := [others => [others => Length'Last]];
                  Kinematics_Params : My_Config.Kinematics_Parameters;

                  Used_Steppers : array (Stepper_Name) of Boolean := [others => False];

                  procedure Check_Stepper (S : Stepper_Name) is
                     Stepper_Params : My_Config.Stepper_Parameters;
                  begin
                     My_Config.Config_File.Read (Stepper_Params, S);

                     if not Stepper_Params.Enabled then
                        raise Config_Constraint_Error
                          with "Stepper " & S'Image & " attached to an axis but not enabled.";
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
               end;

               declare
                  Corner_Data : Corner_Extra_Data :=
                    (Fans => (others => 0.0), Heaters => (others => Temperature (0.0)));
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
               end;

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
      exception
         when E : others =>
            Fatal_Exception_Occurrence_Holder.all.Set
              (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, E);
      end;

      My_GUI.Run;
   end Run;

   protected body Status_Message is
      procedure Set (S : String) is
      begin
         Set_Unbounded_String (Local, S);
      end Set;

      function Get return String is
      begin
         return To_String (Local);
      end Get;
   end Status_Message;

   procedure Report_Temperature (Thermistor : Thermistor_Name; Temp : Temperature) is
   begin
      Last_Temperatures (Thermistor) := Temp;
   end Report_Temperature;

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

   procedure Start_Planner_Block (Data : Flush_Extra_Data) is
   begin
      if First_Block then
         Reset_Position ([others => 0.0]);
         First_Block := False;
      end if;

      if Data.Is_Homing_Move then
         Setup_For_Loop_Move (Data.Home_Switch, Data.Home_Hit_On_State);
      end if;

      if Data.Is_Conditional_Move then
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
      Last_Position := (for A in Axis_Name => Pos (A));
   end Enqueue_Command_Internal;

   procedure Finish_Planner_Block
     (Data                 : Flush_Extra_Data;
      Next_Block_Pos       : Stepper_Position;
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index)
   is
   begin
      if Data.Is_Conditional_Move or Data.Is_Homing_Move then
         Wait_Until_Idle (Last_Command_Index);
      end if;

      --  TODO: Should we require the user to implement this instead?
      if Data.Dwell_Time /= Time (0.0) then
         Wait_Until_Idle (Last_Command_Index);
         delay Duration (Data.Dwell_Time / s);
      end if;

      if Data.Wait_For_Heater then
         Wait_Until_Heater_Stable (Last_Command_Index, Data.Wait_For_Heater_Name);
      end if;

      My_Gcode_Handler.Finished_Block (Data, First_Accel_Distance);
      Reset_Position (Next_Block_Pos);
   end Finish_Planner_Block;

end Prunt.Controller;
