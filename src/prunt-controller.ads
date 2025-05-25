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
with Prunt.Config;
with Prunt.Motion_Planner.Planner;
with Prunt.Web_Server;
with Prunt.Controller_Generic_Types;
with Ada.Exceptions;
with System.Multiprocessors;
with Prunt.Step_Generator.Generator;
with Prunt.Input_Shapers;
with Prunt.Heaters;
with Prunt.TMC_Types;
with Prunt.TMC_Types.TMC2240;
with Prunt.Logger;
with Prunt.Command_Line_Arguments;
with Prunt.Update_Checker;

generic
   with package Generic_Types is new Controller_Generic_Types (<>);
   use Generic_Types;

   Stepper_Hardware : Generic_Types.Stepper_Hardware_Parameters_Array_Type;
   --  The parameters of the stepper drivers installed on the hardware that the implementation is designed for.

   Interpolation_Time : Time;
   --  The time delta for all moves except loop moves.

   Loop_Interpolation_Time : Time;
   --  The time delta for loop moves.

   with procedure Setup (Heater_Thermistors : Heater_Thermistor_Map; Thermistors : Thermistor_Parameters_Array_Type);
   --  Run any required setup and store parameters for later use. This procedure will only be called once and will be
   --  called before any other procedures. Should configure all heaters as disabled until Reconfigure_Heater is called.

   with
     procedure Reconfigure_Heater (Heater : Heater_Name; Params : Prunt.Heaters.Heater_Parameters)
     with Pre => Params.Kind not in (Prunt.Heaters.PID_Autotune_Kind);
   --  Reconfigure a heater. May be called multiple times per heater with different parameters. May be called from any
   --  task.

   with
     procedure Autotune_Heater (Heater : Heater_Name; Params : Prunt.Heaters.Heater_Parameters)
     with Pre => Params.Kind in (Prunt.Heaters.PID_Autotune_Kind);
   --  Run autotuning for the given heater and setpoint. Should not return until the autotune is complete.
   --
   --  TODO: Save the results to the config file.

   with procedure Reconfigure_Fan (Fan : Fan_Name; PWM_Freq : Fan_PWM_Frequency);
   --  Set the frequency of a fan PWM output. May be called multiple times per heater with different parameters. May be
   --  called from any task. A fan should keep the same duty cycle after changing frequency.

   with procedure Setup_For_Loop_Move (Switch : Input_Switch_Name; Hit_State : Pin_State);
   --  Setup the step generator for an upcoming loop move. A loop move should stop looping when the state of Switch =
   --  Hit_State. This procedure will always be called before any loop moves are called, but may be called zero, one,
   --  or many times for each loop move. The last parameters of this call before an Enqueue_Move call containing a loop
   --  move should always be the values used for the given loop move. This procedure will not be called if the last
   --  call to Enqueue_Move had the Safe_Stop_After parameter set to False. This procedure may be called before any
   --  moves are queued. Wait_Until_Idle will always be called before this procedure.

   with procedure Setup_For_Conditional_Move (Switch : Input_Switch_Name; Hit_State : Pin_State);
   --  Ignore all commands until after the next command where Safe_Stop_After = True if the Switch is currently in
   --  Hit_State. This procedure will not be called if the last call to Enqueue_Command had the Safe_Stop_After
   --  parameter set to False. Wait_Until_Idle will always be called before this procedure.

   with procedure Enqueue_Command (Command : Queued_Command);
   --  Enqueue a command to be executed.
   --
   --  If Loop_Until_Hit = False then the time delta of the move is Interpolation_Time, otherwise the time delta is
   --  Loop_Interpolation_Time.
   --
   --  If Loop_Until_Hit = True then the move should be repeated indefinitely until the condition set by
   --  Setup_For_Loop_Move is met. If the condition is met before the loop move is reached then Report_External_Error
   --  should be called.
   --
   --  If the queue runs dry on a move where Safe_Stop_After = False then Report_External_Error should be called and
   --  all heaters and motors should be disabled. Keep in mind that the motors may still be moving when the queue runs
   --  dry, so a delay may be required before disabling the motors.
   --
   --  Index starts at 1 and will increase by 1 for each call.
   --
   --  The implementation is allowed to buffer fan and heater targets and only send the latest value every n commands.

   with procedure Reset_Position (Pos : Stepper_Position);
   --  Reset the position of all steppers to the given position. This procedure should not cause the steppers to move,
   --  it just informs the steppers of their position. This procedure will always be called before Enqueue_Command is
   --  first called. This procedure will not be called if the last call to Enqueue_Command had the Safe_Stop_After
   --  parameter set to False.

   with procedure Wait_Until_Idle (Last_Command : Command_Index);
   --  Block until all queued commands are completed. This procedure will not be called if the last call to
   --  Enqueue_Command had the Safe_Stop_After parameter set to False. This procedure should not wait for heaters to
   --  reach targets. Last_Command indicates the last command index that was enqueued. May be called from any task.

   with procedure Reset;
   --  Reset the device to power-on state.

   Config_Path : String;
   --  Path of the printer configuration file.

   Update_Check : Update_Check_Details := (Method => None);
package Prunt.Controller is

   procedure Prompt_For_Update;
   --  Prompts the user to click a button to allow a firmware update in the GUI and returns when the user clicks the
   --  button. This is used to prevent a broken firmware updater from getting stuck in a loop and wearing out the flash
   --  of the board being updated.
   --
   --  Should only be called before Run as it does not make sense to update the firmware after Prunt has started to
   --  initialise the board.

   procedure Run;
   --  Start the controller. Does not return while the controller is running.

   procedure Report_Temperature (Thermistor : Thermistor_Name; Temp : Temperature);
   --  Report the current thermistor output. There are no restrictions on how often this procedure needs to be called
   --  but some g-code commands will block until it is called after the start of command execution.

   procedure Report_Temperature (Temperature_Probe : Board_Temperature_Probe_Name; Temp : Temperature);
   --  Report the current board temperature probe output. There are no restrictions on how often this procedure needs
   --  to be called.

   procedure Report_Heater_Power (Heater : Heater_Name; Power : PWM_Scale);
   --  Report the current power setting of a heater. There are no restrictions on how often this procedure needs to be
   --  called.

   procedure Report_Input_Switch_State (Switch : Input_Switch_Name; State : Pin_State);
   --  Report the current state of an input switch. There are no restrictions on how often this procedure needs to be
   --  called.

   procedure Report_Tachometer_Frequency (Fan : Fan_Name; Freq : Frequency);
   --  Report the current frequency of a tachometer input.

   procedure Report_Last_Command_Executed (Index : Command_Index);
   --  Report the last command that has been fully executed. There are no restrictions on how often this procedure
   --  needs to be called.

   procedure Report_External_Error (Message : String);
   procedure Report_External_Error (Occurrence : Ada.Exceptions.Exception_Occurrence);
   --  Report an error to Prunt and cause the printer to halt.

   procedure Log (Message : String);
   --  Log a message for the user.

private

   pragma Warnings (Off, "use of an anonymous access type allocator");
   Fatal_Exception_Occurrence_Holder : constant access Fatal_Exception_Occurrence_Holder_Type :=
     new Fatal_Exception_Occurrence_Holder_Type;
   --  The only reason that this is an allocation is so that we can safely call 'Access on the Set procedure to be
   --  passed to Ada.Task_Termination.Set_Specific_Handler. 'Unrestricted_Access works if we replace this with just a
   --  plain variable but that is a GNAT extension and of course introduces the risk of dangling pointers.
   pragma Warnings (On, "use of an anonymous access type allocator");

   procedure Helper_Lock_Memory
   with Import => True, Convention => C, External_Name => "prunt_controller_helper_lock_memory";

   type Flush_Resetting_Data is record
      Is_Homing_Move           : Boolean := False;
      Home_Switch              : Input_Switch_Name := Input_Switch_Name'First;
      Home_Hit_On_State        : Pin_State := High_State;
      Is_Conditional_Move      : Boolean := False;
      Conditional_Switch       : Input_Switch_Name := Input_Switch_Name'First;
      Conditional_Hit_On_State : Pin_State := High_State;
      Wait_For_Heater          : Boolean := False;
      Wait_For_Heater_Name     : Heater_Name := Heater_Name'First;
      Dwell_Time               : Time := Time (0.0);
      Pause_After              : Boolean := False;
   end record;

   type Block_Persistent_Data is record
      Shaper_Parameters : Input_Shapers.Axial_Shaper_Parameters := (others => (Kind => Input_Shapers.No_Shaper));
      Current_File      : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Corner_Extra_Data is record
      Fans         : Fan_PWMs;
      Heaters      : Heater_Targets;
      Current_Line : File_Line_Count;
   end record;

   function Is_Homing_Move (Data : Flush_Resetting_Data) return Boolean;

   pragma Warnings (Off, "cannot call * before body seen");
   package My_Logger is new Logger;
   pragma Warnings (On, "cannot call * before body seen");

   package My_Planner is new
     Motion_Planner.Planner
       (Flush_Resetting_Data_Type     => Flush_Resetting_Data,
        Flush_Resetting_Data_Default  => (others => <>),
        Block_Persistent_Data_Type    => Block_Persistent_Data,
        Block_Persistent_Data_Default => (others => <>),
        Corner_Extra_Data_Type        => Corner_Extra_Data,
        Initial_Position              => [others => 0.0 * mm],
        Max_Corners                   => Command_Line_Arguments.Max_Planner_Block_Corners,
        Is_Homing_Move                => Is_Homing_Move,
        Interpolation_Time            => Interpolation_Time,
        Home_Move_Minimum_Coast_Time  => 4.0 * Interpolation_Time + Loop_Interpolation_Time,
        Runner_CPU                    => Command_Line_Arguments.Motion_Planner_CPU);

   procedure Start_Planner_Block
     (Resetting_Data     : Flush_Resetting_Data;
      Persistent_Data    : Block_Persistent_Data;
      Last_Command_Index : Command_Index);
   procedure Enqueue_Command_Internal
     (Pos             : Position;
      Stepper_Pos     : Stepper_Position;
      Data            : Corner_Extra_Data;
      Index           : Command_Index;
      Loop_Until_Hit  : Boolean;
      Safe_Stop_After : Boolean);
   procedure Finish_Planner_Block
     (Resetting_Data       : Flush_Resetting_Data;
      Persistent_Data      : Block_Persistent_Data;
      Next_Block_Pos       : Stepper_Position;
      First_Accel_Distance : Length;
      Next_Command_Index   : Command_Index);

   function Get_Axial_Shaper_Parameters (Data : Block_Persistent_Data) return Input_Shapers.Axial_Shaper_Parameters;

   package My_Step_Generator is new
     Step_Generator.Generator
       (Planner                     => My_Planner,
        Stepper_Name                => Stepper_Name,
        Stepper_Position            => Stepper_Position,
        Start_Planner_Block         => Start_Planner_Block,
        Enqueue_Command             => Enqueue_Command_Internal,
        Finish_Planner_Block        => Finish_Planner_Block,
        Get_Axial_Shaper_Parameters => Get_Axial_Shaper_Parameters,
        Interpolation_Time          => Interpolation_Time,
        Loop_Interpolation_Time     => Loop_Interpolation_Time,
        Runner_CPU                  => Command_Line_Arguments.Step_Generator_CPU);

   type Stepper_Kinds_Type is array (Stepper_Name) of Stepper_Kind;

   package My_Config is new
     Config
       (Stepper_Name       => Stepper_Name,
        Stepper_Kinds_Type => Stepper_Kinds_Type,
        Stepper_Kinds      => [for I in Stepper_Name => Stepper_Hardware (I).Kind],
        Heater_Name        => Heater_Name,
        Thermistor_Name    => Thermistor_Name,
        Fan_Name           => Fan_Name,
        Input_Switch_Name  => Input_Switch_Name,
        Config_Path        => Config_Path);

   procedure Finished_Block (Data : Flush_Resetting_Data; First_Segment_Accel_Distance : Length);

   function Get_Position return Position;

   function Get_Temperature (Thermistor : Thermistor_Name) return Temperature;

   function Get_Temperature (Stepper : Stepper_Name) return Temperature;

   function Get_Temperature (Temperature_Probe : Board_Temperature_Probe_Name) return Temperature;

   function Get_Heater_Power (Heater : Heater_Name) return PWM_Scale;

   function Get_Input_Switch_State (Switch : Input_Switch_Name) return Pin_State;

   function Get_Tachometer_Frequency (Fan : Fan_Name) return Frequency;

   function Get_Line return File_Line_Count;

   procedure Submit_Gcode_Command (Command : String; Succeeded : out Boolean);
   procedure Submit_Gcode_File (Path : String; Succeeded : out Boolean);

   package My_Update_Checker is new Update_Checker (My_Logger => My_Logger, Details => Update_Check);

   protected Current_File_Name is
      function Get_File_Name return String;
      procedure Set_File_Name (Name : String);
   private
      File : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("<NO FILE>");
   end Current_File_Name;

   protected Reload_Signal is
      entry Wait;
      procedure Signal;
   private
      Reload_Requested : Boolean := False;
   end Reload_Signal;

   procedure Signal_Reload;

   pragma Warnings (Off, "cannot call * before body seen");
   package My_Web_Server is new
     Web_Server
       (My_Logger                         => My_Logger,
        My_Config                         => My_Config,
        My_Update_Checker                 => My_Update_Checker,
        Get_Position                      => Get_Position,
        Get_Thermistor_Temperature        => Get_Temperature,
        Get_Stepper_Temperature           => Get_Temperature,
        Board_Temperature_Probe_Name      => Board_Temperature_Probe_Name,
        Get_Board_Temperature             => Get_Temperature,
        Get_Heater_Power                  => Get_Heater_Power,
        Get_Input_Switch_State            => Get_Input_Switch_State,
        Get_Tachometer_Frequency          => Get_Tachometer_Frequency,
        Get_File_Name                     => Current_File_Name.Get_File_Name,
        Get_Line                          => Get_Line,
        Submit_Gcode_Command              => Submit_Gcode_Command,
        Submit_Gcode_File                 => Submit_Gcode_File,
        Is_Stepgen_Paused                 => My_Step_Generator.Is_Paused,
        Pause_Stepgen                     => My_Step_Generator.Pause,
        Resume_Stepgen                    => My_Step_Generator.Resume,
        Reload_Server                     => Signal_Reload,
        Fatal_Exception_Occurrence_Holder => Fatal_Exception_Occurrence_Holder.all,
        Port                              => Command_Line_Arguments.Web_Server_Port);
   pragma Warnings (On, "cannot call * before body seen");

   procedure TMC2240_UART_Write_And_Validate (Message : TMC_Types.TMC2240.UART_Data_Message; Stepper : Stepper_Name);
   procedure Setup_Thermistors_And_Heater_Assignments;
   procedure Setup_Stepper (Stepper : Stepper_Name);
   procedure Setup_Step_Generator;

   task TMC_Temperature_Updater is
      entry Start;
      entry Reset;
   end TMC_Temperature_Updater;

   procedure Enable_Stepper (Stepper : Stepper_Name);
   procedure Disable_Stepper (Stepper : Stepper_Name);

end Prunt.Controller;
