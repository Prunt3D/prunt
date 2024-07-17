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
with Prunt.GUI.GUI;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Prunt.Controller_Generic_Types;
with Ada.Exceptions;
with System.Multiprocessors;
with Prunt.Step_Generator;
with Prunt.Heaters;
with Prunt.TMC_Types;
with Prunt.TMC_Types.TMC2240;

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

   with procedure Reconfigure_Heater (Heater : Heater_Name; Params : Prunt.Heaters.Heater_Parameters) with
     Pre => Params.Kind not in PID_Autotune_Kind;
   --  Reconfigure a heater. May be called multiple times per heater with different parameters. May be called from any
   --  task.

   with procedure Autotune_Heater (Heater : Heater_Name; Params : Prunt.Heaters.Heater_Parameters) with
     Pre => Params.Kind in PID_Autotune_Kind;
   --  Run autotuning for the given heater and setpoint. Should not return until the autotune is complete.
   --
   --  TODO: Save the results to the config file.

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

   Config_Path : String;
   --  Path of the printer configuration file.

   Command_Generator_CPU : System.Multiprocessors.CPU_Range;
   --  The CPU to run the command generator task on. This is the task that calls Setup_For_Loop_Move,
   --  Setup_For_Conditional_Move, Enqueue_Command, and Reset_Position. No other tasks will call these procedures.

   Max_Planner_Block_Corners : Motion_Planner.Max_Corners_Type := 3_000;
   --  Number of corners to be planned in a single block. Increasing this value will minimise the number of complete
   --  stops required at the cost of using more memory. Increasing this value too far may cause the printer to pause
   --  after each block while the next is loaded.
package Prunt.Controller is

   procedure Run;
   --  Start the controller. Does not return while the controller is running.

   procedure Report_Temperature (Thermistor : Thermistor_Name; Temp : Temperature);
   --  Report the current thermistor output. There are no restrictions on how often this procedure needs to be called
   --  but some g-code commands will block until it is called after the start of command execution.

   procedure Report_Last_Command_Executed (Index : Command_Index);
   --  Report the last command that has been fully executed. There are no restrictions on how often this procedure
   --  needs to be called.

   procedure Report_External_Error (Message : String);
   procedure Report_External_Error (Occurrence : Ada.Exceptions.Exception_Occurrence);
   --  Report an error to Prunt and cause the printer to halt.

private

   pragma Warnings (Off, "use of an anonymous access type allocator");
   Fatal_Exception_Occurrence_Holder : constant access Fatal_Exception_Occurrence_Holder_Type :=
     new Fatal_Exception_Occurrence_Holder_Type;
   --  The only reason that this is an allocation is so that we can safely call 'Access on the Set procedure to be
   --  passed to Ada.Task_Termination.Set_Specific_Handler. 'Unrestricted_Access works if we replace this with just a
   --  plain variable but that is a GNAT extension and of course introduces the risk of dangling pointers.
   pragma Warnings (On, "use of an anonymous access type allocator");

   Config_Constraint_Error : exception;
   --  Raised when the configuration file is found to be invalid during setup.

   procedure Helper_Lock_Memory with
     Import => True, Convention => C, External_Name => "prunt_controller_helper_lock_memory";

   type Flush_Extra_Data is record
      Is_Homing_Move           : Boolean           := False;
      Home_Switch              : Input_Switch_Name := Input_Switch_Name'First;
      Home_Hit_On_State        : Pin_State         := High_State;
      Is_Conditional_Move      : Boolean           := False;
      Conditional_Switch       : Input_Switch_Name := Input_Switch_Name'First;
      Conditional_Hit_On_State : Pin_State         := High_State;
      Wait_For_Heater          : Boolean           := False;
      Wait_For_Heater_Name     : Heater_Name       := Heater_Name'First;
      Dwell_Time               : Time              := Time (0.0);
   end record;

   type Corner_Extra_Data is record
      Fans    : Fan_PWMs;
      Heaters : Heater_Targets;
   end record;

   function Is_Homing_Move (Data : Flush_Extra_Data) return Boolean;

   package My_Planner is new Motion_Planner.Planner
     (Flush_Extra_Data_Type        => Flush_Extra_Data,
      Flush_Extra_Data_Default     => (others => <>),
      Corner_Extra_Data_Type       => Corner_Extra_Data,
      Initial_Position             => [others => 0.0 * mm],
      Max_Corners                  => Max_Planner_Block_Corners,
      Is_Homing_Move               => Is_Homing_Move,
      Home_Move_Minimum_Coast_Time => 4.0 * Interpolation_Time + Loop_Interpolation_Time);

   procedure Start_Planner_Block (Data : Flush_Extra_Data; Last_Command_Index : Command_Index);
   procedure Enqueue_Command_Internal
     (Pos             : Position;
      Stepper_Pos     : Stepper_Position;
      Data            : Corner_Extra_Data;
      Index           : Command_Index;
      Loop_Until_Hit  : Boolean;
      Safe_Stop_After : Boolean);
   procedure Finish_Planner_Block
     (Data                 : Flush_Extra_Data;
      Next_Block_Pos       : Stepper_Position;
      First_Accel_Distance : Length;
      Next_Command_Index   : Command_Index);

   package My_Step_Generator is new Step_Generator
     (Planner                 => My_Planner,
      Stepper_Name            => Stepper_Name,
      Stepper_Position        => Stepper_Position,
      Start_Planner_Block     => Start_Planner_Block,
      Enqueue_Command         => Enqueue_Command_Internal,
      Finish_Planner_Block    => Finish_Planner_Block,
      Interpolation_Time      => Interpolation_Time,
      Loop_Interpolation_Time => Loop_Interpolation_Time,
      Runner_CPU              => Command_Generator_CPU,
      Initial_Position        => [others => 0.0 * mm]);

   type Stepper_Kinds_Type is array (Stepper_Name) of Stepper_Kind;

   package My_Config is new Config
     (Stepper_Name       => Stepper_Name,
      Stepper_Kinds_Type => Stepper_Kinds_Type,
      Stepper_Kinds      => [for I in Stepper_Name => Stepper_Hardware (I).Kind],
      Heater_Name        => Heater_Name,
      Thermistor_Name    => Thermistor_Name,
      Fan_Name           => Fan_Name,
      Input_Switch_Name  => Input_Switch_Name,
      Config_Path        => Config_Path);

   procedure Finished_Block (Data : Flush_Extra_Data; First_Segment_Accel_Distance : Length);

   function Get_Status_Message return String;

   function Get_Position return Position;

   function Get_Temperature (Thermistor : Thermistor_Name) return Temperature;

   procedure Submit_Gcode_Command (Command : String; Succeeded : out Boolean);
   procedure Submit_Gcode_File (Path : String; Succeeded : out Boolean);

   package My_GUI is new GUI.GUI
     (My_Config                         => My_Config,
      Get_Status_Message                => Get_Status_Message,
      Get_Position                      => Get_Position,
      Get_Temperature                   => Get_Temperature,
      Submit_Gcode_Command              => Submit_Gcode_Command,
      Submit_Gcode_File                 => Submit_Gcode_File,
      Pause_Stepgen                     => My_Step_Generator.Pause,
      Resume_Stepgen                    => My_Step_Generator.Resume,
      Fatal_Exception_Occurrence_Holder => Fatal_Exception_Occurrence_Holder.all);

   protected Status_Message is
      procedure Set (S : String);
      function Get return String;
   private
      Local : Unbounded_String := To_Unbounded_String ("");
   end Status_Message;

   procedure TMC2240_UART_Write_And_Validate (Message : TMC_Types.TMC2240.UART_Data_Message; Stepper : Stepper_Name);
   procedure Setup_Thermistors_And_Heater_Assignments;
   procedure Setup_Stepper (Stepper : Stepper_Name);
   procedure Setup_Planner;
   procedure Setup_Step_Generator;
   procedure Setup_Gcode_Handler;

   task GUI_Runner is
      entry Start;
      entry Finish;
   end GUI_Runner;

end Prunt.Controller;
