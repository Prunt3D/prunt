--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
--  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
--  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice (including the next paragraph) shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
--  THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.
--------------------------------------------------

pragma Extensions_Allowed (On);

with Ada.Exceptions;
with Ada.Streams;
with GNAT.Sockets;
with Prunt.Command_Line_Arguments;
with Prunt.Config;
with Prunt.Controller_Generic_Types;
with Prunt.Exception_Occurrence_Holders;
with Prunt.Motion_Planner;
with System.Multiprocessors;

private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Vectors;
private with Prunt.Controller_Helpers;
private with Prunt.Default_Modules;
private with Prunt.Default_Modules.Basic_Config;
private with Prunt.Default_Modules.Basic_Motor_Drivers;
private with Prunt.Default_Modules.Blocking_Tracker;
private with Prunt.Default_Modules.Config_Saving;
private with Prunt.Default_Modules.Dwell;
private with Prunt.Default_Modules.Fans;
private with Prunt.Default_Modules.Heaters;
private with Prunt.Default_Modules.Homing;
private with Prunt.Default_Modules.Idle_Emitter;
private with Prunt.Default_Modules.Input_Shapers;
private with Prunt.Default_Modules.Input_Switches;
private with Prunt.Default_Modules.Internal_Status_Reporter;
private with Prunt.Default_Modules.Kinematics;
private with Prunt.Default_Modules.Machine_Idle_Timeout;
private with Prunt.Default_Modules.Machine_Name;
private with Prunt.Default_Modules.Motion;
private with Prunt.Default_Modules.Motor_Drivers;
private with Prunt.Default_Modules.Power_Control;
private with Prunt.Default_Modules.TMC2240_Drivers;
private with Prunt.Default_Modules.Tachometers;
private with Prunt.Default_Modules.Thermistors;
private with Prunt.Gcode_Queues;
private with Prunt.Indefinite_Ordered_Maps_With_Insertion_Order;
private with Prunt.Logger;
private with Prunt.Module_Types;
private with Prunt.Motion_Planner.Planner;
private with Prunt.Status_Manager;
private with Prunt.Step_Generator;
private with Prunt.Update_Checker;
private with Prunt.Web_Server;

generic
   with package Generic_Types is new Controller_Generic_Types (<>);
   use Generic_Types;

   Hardware : Generic_Types.Hardware_Parameters;

   Interpolation_Time : Time;
   --  The time delta for all moves.

   with procedure Enqueue_Command (Command : Queued_Command);
   --  Enqueue a command to be executed.
   --
   --  If Loop_Until_Hit = False then the time delta of the move is Interpolation_Time, otherwise the time delta is
   --  Loop_Interpolation_Time.
   --
   --  If Loop_Until_Hit = True then the move should be repeated indefinitely until the condition set by
   --  Setup_For_Loop_Move is met. If the condition is met before the loop move is reached then
   --  Report_External_Error should be called. After the loop move is completed, Report_Loop_Cycles must be called.
   --
   --  If the queue runs dry on a move where Safe_Stop_After = False then Report_External_Error should be called
   --  and all heaters and motors should be disabled. Keep in mind that the motors may still be moving when the queue
   --  runs dry, so a delay may be required before disabling the motors.

   with procedure Reset_Position (Pos : Motor_Position);
   --  Reset the position of all motors to the given position. This procedure should not cause the motors to move, it
   --  just informs the motors of their position. This procedure will always be called before Enqueue_Command is
   --  first called. This procedure will not be called if the last call to Enqueue_Command had the Safe_Stop_After
   --  parameter set to False.

   with procedure Wait_Until_Idle (Last_Command_Index : Command_Index);
   --  Block until all queued commands are completed. This procedure will not be called if the last call to
   --  Enqueue_Command had the Safe_Stop_After parameter set to False. Will only be called from the same task as
   --  Enqueue_Command.

   with procedure Reset_Hardware;
   --  Reset the device to power-on state.

   Config_Path : String;
   --  Path of the printer configuration file.

   Config_Overrides : Config.Config_Override_Vectors.Vector := [];
   --  Immutable configuration values used by modules but hidden from the web interface and omitted from the
   --  configuration file.

   with
     function Get_Extra_HTTP_Content (Name : Virtual_String) return access constant Ada.Streams.Stream_Element_Array;
   --  Get a file to be exposed via the built in web server under the extras/ path. This is intended to be used for
   --  images for board-specific documentation but can also be used for other files such as firmware binaries.

   with function Get_Board_Specific_Documentation (Key : Virtual_String) return Virtual_String;
   --  Get the board specific documentation HTML to be appended to the documentation for a given configuration option.
   --  Keys can be displayed in the web interface by running the server with --enable-documentation-dev-mode=true.

   Update_Check : Update_Check_Details := (Method => None);
   --  Method to be used for update checking. When an update is available it will be displayed via the web interface.

   Executed_Command_Position_Ring_Capacity : Positive;
   --  Capacity of the ring buffer used to transfer queued execution positions from the step generator path to
   --  execution reporting. This sets an upper bound on how many commands may be queued before new commands stop being
   --  emitted.

   Web_Server_Port : GNAT.Sockets.Port_Type := Command_Line_Arguments.Web_Server_Port;
   --  Port used by this controller's web server. Defaults to the value from Prunt.Command_Line_Arguments.

   Motion_Planner_CPU : System.Multiprocessors.CPU_Range := Command_Line_Arguments.Motion_Planner_CPU;
   --  CPU used by this controller's motion planner, or 0 to allow the runtime to select one. Defaults to the value
   --  from Prunt.Command_Line_Arguments.

   Step_Generator_CPU : System.Multiprocessors.CPU_Range := Command_Line_Arguments.Step_Generator_CPU;
   --  CPU used by this controller's step generator, or 0 to allow the runtime to select one. Defaults to the value
   --  from Prunt.Command_Line_Arguments.

   Max_Planner_Block_Corners : Motion_Planner.Max_Corners_Type := Command_Line_Arguments.Max_Planner_Block_Corners;
   --  Maximum number of corners in a planner block for this controller. Defaults to the value from
   --  Prunt.Command_Line_Arguments.

   Disable_Default_Modules : Boolean := False;

   Extra_Modules : Module_Maps.Map := [];
package Prunt.Controller is

   pragma Extensions_Allowed (On);
   --  TODO: We need to repeat this here or we get errors, but it's unclear why. Does the pragma above the generic not
   --  carry through to the instantiation?

   pragma Unreferenced (Get_Board_Specific_Documentation);

   pragma Assert (Executed_Command_Position_Ring_Capacity >= 2);

   procedure Prompt_For_Update;
   --  Prompts the user to click a button to allow a firmware update in the GUI and returns when the user clicks the
   --  button. This is used to prevent a broken firmware updater from getting stuck in a loop and wearing out the flash
   --  of the board being updated.
   --
   --  Should only be called before Run as it does not make sense to update the firmware after Prunt has started to
   --  initialise the board.

   procedure Run;
   --  Start the controller. Does not return while the controller is running.

   procedure Report_Last_Command_Executed (Index : Command_Index);
   --  Report the last command that has been fully executed. There are no restrictions on how often this procedure
   --  needs to be called.

   procedure Report_Loop_Move_Cycles (Index : Command_Index; Cycles : Dimensionless);
   --  Report the number of iterations completed by the loop move identified by Index.

   procedure Report_External_Error (Message : String; Is_Fatal : Boolean := True);
   --  Report an error to Prunt and cause the printer to halt.

   procedure Report_External_Error (Occurrence : Ada.Exceptions.Exception_Occurrence; Is_Fatal : Boolean := True);
   --  Report an error to Prunt and cause the printer to halt.

   function Last_Error_Message return String;
   --  Return the stored controller error message, or an empty string if no error has been stored.

   procedure Log (Message : String);
   --  Log a message for the user.

   procedure Submit_Gcode_Command (Command : Virtual_String; Succeeded : out Boolean);
   --  Queue a single G-code command for execution.

   procedure Submit_Gcode_File (Path : Virtual_String; Succeeded : out Boolean);
   --  Queue a G-code file for execution.

   procedure Cancel_Gcode (Succeeded : out Boolean);
   --  Cancel all pending G-code and reset the runtime planners around the last executed physical position.

   procedure Pause_Stepgen;
   --  Request runtime pause handling.

   procedure Resume_Stepgen;
   --  Request runtime resume handling.

   function Stepgen_Paused return Boolean;
   --  Return True while the step generator is paused.

   function Ready_For_Gcode return Boolean;
   --  Return True once the G-code processor task is running and ready to consume queued commands/files.

   procedure Apply_Untrusted_Config_Patch
     (Patch : Virtual_String; Result : out Virtual_String; Errors : out Config.Config_Error_Vectors.Vector);
   --  Apply the same validated configuration patch format used by the web UI.

   function Get_Config_Schema_String return Virtual_String;
   --  Return the JSON configuration schema currently used by this controller instance.

   procedure Reset_Live_Config_To_Stored;
   --  Replace the live configuration with the stored configuration values.

private

   use Prunt.Module_Types;

   package My_Logger is new Prunt.Logger;

   package My_Controller_Helpers is new Prunt.Controller_Helpers (Generic_Types);
   use My_Controller_Helpers;

   package My_Default_Modules is new Default_Modules (My_Modules, My_Logger);

   function Get_Current_Position return Position;
   --  Return the most recently reported executed cartesian position for status reporting.

   function Get_Current_File_Name return Virtual_String;
   --  Return the path of the G-code file currently being consumed, or an empty string when none is active.

   function Get_Current_File_Line return File_Line_Count;
   --  Return the current one-based line number within the active G-code file.

   Machine_Idle_Timeout_Error : exception;

   procedure Request_Machine_Idle_Timeout_Shutdown (Message : String);
   --  Shut the controller down after an M85 timeout.

   package My_Default_Modules_Children is
      package Idle_Emitter is new My_Default_Modules.Idle_Emitter;
      --  TODO: Idle emitter needs to be connected to controller idle interface.
      package Basic_Config is new My_Default_Modules.Basic_Config;
      package Blocking_Tracker is new My_Default_Modules.Blocking_Tracker;
      package Config_Saving is new My_Default_Modules.Config_Saving;
      package Machine_Name is new My_Default_Modules.Machine_Name (Config_Saving_Module => Config_Saving);
      package Machine_Idle_Timeout is new
        My_Default_Modules.Machine_Idle_Timeout
          (Config_Saving_Module => Config_Saving,
           Idle_Emitter_Module  => Idle_Emitter,
           Request_Shutdown     => Request_Machine_Idle_Timeout_Shutdown);
      package Power_Control is new My_Default_Modules.Power_Control;
      package Dwell is new My_Default_Modules.Dwell;
      package Input_Switches is new
        My_Default_Modules.Input_Switches
          (My_Controller_Generic_Types => Generic_Types,
           Input_Switch_Hardware       => Hardware.Input_Switch_Hardware);
      package Homing is new
        My_Default_Modules.Homing
          (My_Controller_Generic_Types => Generic_Types,
           Motor_Hardware              => Hardware.Motor_Hardware,
           Input_Switch_Hardware       => Hardware.Input_Switch_Hardware,
           Input_Switches_Module       => Input_Switches);
      package Internal_Status_Reporter is new
        My_Default_Modules.Internal_Status_Reporter
          (Get_Position   => Get_Current_Position,
           Get_File_Name  => Get_Current_File_Name,
           Get_Line       => Get_Current_File_Line,
           Stepgen_Paused => Stepgen_Paused);
      package Fans is new
        My_Default_Modules.Fans (My_Controller_Generic_Types => Generic_Types, Fan_Hardware => Hardware.Fan_Hardware);
      package Tachometers is new
        My_Default_Modules.Tachometers
          (My_Controller_Generic_Types => Generic_Types,
           Tachometer_Hardware         => Hardware.Tachometer_Hardware);
      package Motor_Drivers is new My_Default_Modules.Motor_Drivers (My_Controller_Generic_Types => Generic_Types);
      package Basic_Motor_Drivers is new
        My_Default_Modules.Basic_Motor_Drivers
          (My_Controller_Generic_Types => Generic_Types,
           Motor_Hardware              => Hardware.Motor_Hardware,
           Motor_Drivers_Module        => Motor_Drivers);
      package TMC2240_Drivers is new
        My_Default_Modules.TMC2240_Drivers
          (My_Controller_Generic_Types => Generic_Types,
           Motor_Hardware              => Hardware.Motor_Hardware,
           Motor_Drivers_Module        => Motor_Drivers);
      package Kinematics is new
        My_Default_Modules.Kinematics
          (My_Controller_Generic_Types => Generic_Types,
           Config_Saving_Module        => Config_Saving,
           Motor_Drivers_Module        => Motor_Drivers);
      package Input_Shapers is new
        My_Default_Modules.Input_Shapers (Config_Saving_Module => Config_Saving, Kinematics_Module => Kinematics);
      package Motion is new
        My_Default_Modules.Motion
          (Config_Saving_Module       => Config_Saving,
           Kinematics_Module          => Kinematics,
           Pending_State_Queue_Length => Max_Planner_Block_Corners);
      package Thermistors is new
        My_Default_Modules.Thermistors
          (My_Controller_Generic_Types => Generic_Types,
           Thermistor_Hardware         => Hardware.Thermistor_Hardware);
      package Heaters is new
        My_Default_Modules.Heaters
          (My_Controller_Generic_Types => Generic_Types,
           Heater_Hardware             => Hardware.Heater_Hardware,
           Thermistors_Module          => Thermistors,
           Blocking_Tracker_Module     => Blocking_Tracker);
   end My_Default_Modules_Children;

   procedure Setup_Planner_Runners
     (Params : Motion_Planner.Kinematic_Parameters; Map : My_Default_Modules_Children.Kinematics.Motor_Position_Map);
   --  Configure both motion planners and the step generator with the active kinematics and motor-position map.

   type Executed_Command_Position is record
      Index : Command_Index;
      Pos   : Position;
   end record;

   package Last_Command_Executed is
      --  We use a package here rather than a protected object as this needs to be lock free to be used in the realtime
      --  path. In practice, a lock here would probably be fine, but it is still better to avoid it if we can.

      procedure Reset (Pos : Position);

      procedure Record_Queued_Position (Index : Command_Index; Pos : Position);

      procedure Report (Index : Command_Index);

      function Get return Command_Index;

      function Is_Idle return Boolean;
      --  Return True when execution has caught up to every queued command.

      function Get_Current_Position return Position;
      --  Note that this function may return parts from different position reports. This is really only intended for
      --  retrieval of a value for status reporting purposes.

   private
      type Atomic_Command_Index is new Command_Index with Atomic, Volatile;
      type Atomic_Length is new Length with Atomic, Volatile;

      type Pending_Position_Ring_Index is new Positive range 1 .. Executed_Command_Position_Ring_Capacity
      with Atomic, Volatile;

      type Atomic_Position_Array is array (Axis_Name) of Atomic_Length;

      type Pending_Position_Slot is record
         Index : Atomic_Command_Index := 0;
         --  Command index associated with this queued position.

         Pos : Atomic_Position_Array;
         --  Queued cartesian position for this command.
      end record;

      type Pending_Position_Slots is array (Pending_Position_Ring_Index) of Pending_Position_Slot;

      function Next_Slot (Slot : Pending_Position_Ring_Index) return Pending_Position_Ring_Index;

      function Previous_Slot (Slot : Pending_Position_Ring_Index) return Pending_Position_Ring_Index;

      function Advance_Slot (Slot : Pending_Position_Ring_Index; Count : Natural) return Pending_Position_Ring_Index;

      function Slot_Distance (From_Slot, To_Slot : Pending_Position_Ring_Index) return Natural;

      procedure Write_Position (Target : out Atomic_Position_Array; Pos : Position);

      function Read_Position (Source : Atomic_Position_Array) return Position;

      Pending_Position_Write_Slot : Pending_Position_Ring_Index := Pending_Position_Ring_Index'First;
      --  Next ring slot the producer will try to publish into. One slot is always left empty, so the ring is full when
      --  advancing this slot would collide with Pending_Position_Read_Slot.

      Pending_Position_Read_Slot : Pending_Position_Ring_Index := Pending_Position_Ring_Index'First;
      --  Next ring slot the consumer will examine when advancing executed position state. The ring is empty when this
      --  matches Pending_Position_Write_Slot.

      Pending_Positions : Pending_Position_Slots := [others => <>];
      --  Fixed-capacity SPSC ring carrying queued positions from step generation to execution reporting.

      Last_Command_Executed_Index : Atomic_Command_Index := 0;
      --  Highest command index reported as fully executed.

      Current_Position_Data : Atomic_Position_Array := [others => Atomic_Length (0.0 * mm)];
      --  Last committed executed cartesian position.
   end Last_Command_Executed;

   use type Module_Maps.Map;

   pragma Warnings (Off, "use of an anonymous access type allocator");
   Exception_Occurrence_Holder : constant access Exception_Occurrence_Holders.Exception_Occurrence_Holder_Type :=
     new Exception_Occurrence_Holders.Exception_Occurrence_Holder_Type;
   --  This needs to be an allocation is so that we can safely call 'Access on the Set procedure to be passed to
   --  Ada.Task_Termination.Set_Specific_Handler. This of course leaks memory, but no one should be instantiating this
   --  package thousands of times, which is the only time a leak will matter.
   pragma Warnings (On, "use of an anonymous access type allocator");

   function Get_Modules_For_Hardware return Module_Maps.Map
   is ["Basic Config"             =>
         My_Default_Modules_Children.Basic_Config.Module'(My_Modules.Module with null record),
       "Config Saving"            =>
         My_Default_Modules_Children.Config_Saving.Module'(My_Modules.Module with null record),
       "Blocking Tracker"         =>
         My_Default_Modules_Children.Blocking_Tracker.Module'(My_Modules.Module with null record),
       "Dwell"                    => My_Default_Modules_Children.Dwell.Module'(My_Modules.Module with null record),
       "Fans"                     => My_Default_Modules_Children.Fans.Module'(My_Modules.Module with null record),
       "Heaters"                  => My_Default_Modules_Children.Heaters.Module'(My_Modules.Module with null record),
       "Homing"                   => My_Default_Modules_Children.Homing.Module'(My_Modules.Module with null record),
       "Idle Emitter"             =>
         My_Default_Modules_Children.Idle_Emitter.Module'(My_Modules.Module with null record),
       "Input Shapers"            =>
         My_Default_Modules_Children.Input_Shapers.Module'(My_Modules.Module with null record),
       "Input Switches"           =>
         My_Default_Modules_Children.Input_Switches.Module'(My_Modules.Module with null record),
       "Internal Status Reporter" =>
         My_Default_Modules_Children.Internal_Status_Reporter.Module'(My_Modules.Module with null record),
       "Machine Idle Timeout"     =>
         My_Default_Modules_Children.Machine_Idle_Timeout.Module'(My_Modules.Module with null record),
       "Machine Name"             =>
         My_Default_Modules_Children.Machine_Name.Module'(My_Modules.Module with null record),
       "Motion"                   => My_Default_Modules_Children.Motion.Module'(My_Modules.Module with null record),
       "Motor Drivers"            =>
         My_Default_Modules_Children.Motor_Drivers.Module'(My_Modules.Module with null record),
       "Basic Motor Drivers"      =>
         My_Default_Modules_Children.Basic_Motor_Drivers.Module'(My_Modules.Module with null record),
       "TMC2240 Drivers"          =>
         My_Default_Modules_Children.TMC2240_Drivers.Module'(My_Modules.Module with null record),
       "Kinematics"               =>
         My_Default_Modules_Children.Kinematics.Module'(My_Modules.Module with null record),
       "Power Control"            =>
         My_Default_Modules_Children.Power_Control.Module'(My_Modules.Module with null record),
       "Tachometers"              =>
         My_Default_Modules_Children.Tachometers.Module'(My_Modules.Module with null record),
       "Thermistors"              =>
         My_Default_Modules_Children.Thermistors.Module'(My_Modules.Module with null record)];
   --  Return one instance of every default module supported by the configured hardware generic parameters.
   --
   --  TODO: We should avoid returning modules that are not used at all by the specified hardware. It doesn't hurt to
   --  have modules that do nothing, however it would be cleaner to not have them at all.

   Active_Modules : constant Module_Maps.Map :=
     ((if Disable_Default_Modules then Module_Maps.Map'[] else Get_Modules_For_Hardware) & Extra_Modules);

   Active_Module_Config_Schemas : constant Config.Config_Schema_Maps.Map :=
     [for C in Active_Modules.Iterate use Module_Maps.Key (C) => Module_Maps.Element (C).Config_Schema];

   Active_Module_Status_Schemas : constant Status_Manager.Status_Module_Maps.Map :=
     [for C in Active_Modules.Iterate use Module_Maps.Key (C) => Module_Maps.Element (C).Status_Schema];

   My_Status_Data : constant Status_Manager.Status_Data_Collection :=
     Status_Manager.Build_Collection (Active_Module_Status_Schemas);

   package Module_Instance_Maps is new
     Prunt.Indefinite_Ordered_Maps_With_Insertion_Order
       (Virtual_String,
        My_Modules.Module_Instance_Shared_Pointers.Ref,
        "=" => My_Modules.Module_Instance_Shared_Pointers."=");
   --  We need the insertion order here so we an start instances in the same order that they are initialised. This
   --  allows for all of an instances dependencies to start before it.

   package Module_Instance_Vectors is new
     Ada.Containers.Vectors
       (Positive,
        My_Modules.Module_Instance_Shared_Pointers.Ref,
        "=" => My_Modules.Module_Instance_Shared_Pointers."=");

   Active_Config_File : constant Config.Config_File :=
     Config.Create (Config_Path, Active_Module_Config_Schemas, Config_Overrides);

   package My_Update_Checker is new Update_Checker (My_Logger, Update_Check);

   package Extra_Block_Resetting_Data_Holders is new
     Ada.Containers.Indefinite_Holders (Module_Types.Extra_Block_Resetting_Data'Class, Module_Types."=");

   Hardware_Maximum_Deltas_Per_Command : constant Motor_Position :=
     [for M in Motor_Name => Hardware.Motor_Hardware (M).Maximum_Delta_Per_Command];

   package My_Motion_Planner is new
     Motion_Planner.Planner
       (Motor_Name                        => Motor_Name,
        Motor_Position_Map                => My_Default_Modules_Children.Kinematics.Motor_Position_Map,
        Motor_Delta_Limits                => Motor_Position,
        Maximum_Deltas_Per_Command        => Hardware_Maximum_Deltas_Per_Command,
        Flush_Resetting_Data_Type         => Extra_Block_Resetting_Data_Holders.Holder,
        Flush_Resetting_Data_Type_Default =>
          Extra_Block_Resetting_Data_Holders.To_Holder (Module_Types.Extra_Block_Resetting_Data'(null record)),
        Corner_Extra_Data_Type            => Module_Types.Extra_Corner_Data'Class,
        Home_Move_Minimum_Coast_Time      => 5.0 * Interpolation_Time,
        Interpolation_Time                => Interpolation_Time,
        Runner_CPU                        => Motion_Planner_CPU,
        Max_Corners                       => Max_Planner_Block_Corners);

   package My_Pause_Motion_Planner is new
     Motion_Planner.Planner
       (Motor_Name                        => Motor_Name,
        Motor_Position_Map                => My_Default_Modules_Children.Kinematics.Motor_Position_Map,
        Motor_Delta_Limits                => Motor_Position,
        Maximum_Deltas_Per_Command        => Hardware_Maximum_Deltas_Per_Command,
        Flush_Resetting_Data_Type         => Extra_Block_Resetting_Data_Holders.Holder,
        Flush_Resetting_Data_Type_Default =>
          Extra_Block_Resetting_Data_Holders.To_Holder (Module_Types.Extra_Block_Resetting_Data'(null record)),
        Corner_Extra_Data_Type            => Module_Types.Extra_Corner_Data'Class,
        Home_Move_Minimum_Coast_Time      => 5.0 * Interpolation_Time,
        Interpolation_Time                => Interpolation_Time,
        Max_Corners                       => 100);

   procedure Start_Planner_Block
     (Resetting_Data : Extra_Block_Resetting_Data_Holders.Holder; Last_Command_Index : Command_Index);
   --  Receive notification that step generation has started a planner block.

   procedure Enqueue_Command_Internal
     (Pos             : Position;
      Motor_Pos       : Motor_Position;
      Index           : Command_Index;
      Loop_Until_Hit  : Boolean;
      Safe_Stop_After : Boolean;
      Vel_Ratio       : Dimensionless);
   --  Record the cartesian position for Index and forward the corresponding motor command to the hardware queue.

   procedure Start_Corner (Last_Command_Index : Command_Index; Data : Module_Types.Extra_Corner_Data'Class);
   --  Process a corner's extra data after the final command reaching that corner has been queued.

   procedure Finish_Planner_Block
     (Resetting_Data       : Extra_Block_Resetting_Data_Holders.Holder;
      Next_Block_Pos       : Motor_Position;
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index;
      Loop_Move_Offset     : Position_Offset);
   --  Process block-reset data and reset the hardware position when step generation finishes a planner block.

   procedure Handle_Pause (Pause_Position : Position; Last_Command_Index : Command_Index);
   --  Ask registered pause handlers, in initialization order, to build a pause plan from Pause_Position.

   procedure Handle_Resume (Pause_Position : Position; Last_Command_Index : Command_Index);
   --  Ask registered pause handlers, in reverse order, to build the resume plan from Pause_Position.

   function Is_Pause_Plan_Done (Resetting_Data : Extra_Block_Resetting_Data_Holders.Holder) return Boolean;
   --  Return True when Resetting_Data marks the terminal block of a generated pause or resume plan.

   protected Loop_Move_Cycles is new Loop_Cycle_Reporter_Interface with
      procedure Report (Index : Command_Index; Cycles : Dimensionless);
      --  Report the number of loops executed for a given loop move.

      overriding
      entry Wait (Index : Command_Index; Cycles : out Dimensionless);
      --  Wait for loop move cycles to be reported.

      --  TODO: Reset procedure for when controller is reset.
   private
      Current_Index  : Command_Index := 0;
      Current_Cycles : Dimensionless := 0.0;
      Has_Data       : Boolean := False;
   end Loop_Move_Cycles;

   package My_Step_Generator is new
     Step_Generator
       (Planner                    => My_Motion_Planner,
        Pause_Planner              => My_Pause_Motion_Planner,
        Motor_Name                 => Motor_Name,
        Motor_Position             => Motor_Position,
        Motor_Delta_Limits         => Motor_Position,
        Maximum_Deltas_Per_Command => Hardware_Maximum_Deltas_Per_Command,
        Start_Planner_Block        => Start_Planner_Block,
        Start_Pause_Planner_Block  => Start_Planner_Block,
        Enqueue_Command            => Enqueue_Command_Internal,
        Start_Corner               => Start_Corner,
        Start_Pause_Corner         => Start_Corner,
        Finish_Planner_Block       => Finish_Planner_Block,
        Finish_Pause_Planner_Block => Finish_Planner_Block,
        Is_Pause_Plan_Done         => Is_Pause_Plan_Done,
        Handle_Pause               => Handle_Pause,
        Handle_Resume              => Handle_Resume,
        Loop_Cycle_Reporter        => Loop_Move_Cycles'Access,
        Interpolation_Time         => Interpolation_Time,
        Runner_CPU                 => Step_Generator_CPU);

   My_Gcode_Queue : Gcode_Queues.Queue;

   procedure Reload_Server;
   --  Handle a web-server reload request.

   function Get_Status_Values_String return Virtual_String
   is (My_Status_Data.JSON_Data);
   --  Return the current status values encoded as JSON for the web server.

   Active_Module_Gcode_Dispatch_Map : constant Gcode_Dispatch_Maps.Map := Build_Gcode_Dispatch_Map (Active_Modules);

   Active_Module_Gcode_JSON_String : constant Virtual_String := Build_Gcode_JSON (Active_Modules).Write;

   package My_Web_Server is new
     Web_Server
       (Apply_Config_Patch          => Apply_Untrusted_Config_Patch,
        My_Logger                   => My_Logger,
        My_Update_Checker           => My_Update_Checker,
        Submit_Gcode_Command        => Submit_Gcode_Command,
        Submit_Gcode_File           => Submit_Gcode_File,
        Pause_Stepgen               => My_Step_Generator.Pause,
        Resume_Stepgen              => My_Step_Generator.Resume,
        Cancel_Gcode                => Cancel_Gcode,
        Reload_Server               => Reload_Server,
        Get_Extra_HTTP_Content      => Get_Extra_HTTP_Content,
        Exception_Occurrence_Holder => Exception_Occurrence_Holder.all,
        Config_Schema_String        => Active_Config_File.Get_Schema_String,
        Status_Schema_String        => My_Status_Data.JSON_Schema,
        Gcode_JSON_String           => Active_Module_Gcode_JSON_String,
        Get_Status_Values_String    => Get_Status_Values_String,
        Port                        => Web_Server_Port);

   function Recursive_Module_Initialization
     (Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      My_Config_File      : Config.Config_File;
      Log_Dependency_Tree : Boolean := False) return Module_Instance_Maps.Map;
   --  Initialize every active module after recursively satisfying its dependencies.

   protected Patch_Processor is
      procedure Apply
        (Patch : Virtual_String; Result : out Virtual_String; Errors : out Config.Config_Error_Vectors.Vector);
   private
      Cached_Result       : Virtual_String;
      Cached_Errors       : Config.Config_Error_Vectors.Vector;
      Cached_Save_Counter : Config.Save_Counter;
      Has_Cache           : Boolean := False;
   end Patch_Processor;

   protected Reload_Signal is
      entry Wait;
      procedure Signal;
      procedure Mark_Startup_Done;
   private
      Reload_Requested : Boolean := False;
      Startup_Done     : Boolean := False;
   end Reload_Signal;

   procedure Signal_Reload;
   --  Forward a reload request to Reload_Signal so the controller task can process it safely.

   procedure Catch_Up_Planner_State_Handlers (Executed_Corner_ID : Planner_Corner_ID);
   --  Notify loaded planner-state handlers that all primary planner corners up to Executed_Corner_ID have executed.
   --  Handlers should commit any speculative state anchored at or before this ID and leave later state pending.

   procedure Prepare_Config_For_Save_Handlers;
   --  Notify loaded config-save preparers to copy their current runtime state into their registered Config_Data
   --  handles before config saving persists those handles.

   procedure Handle_Cancellation_Handlers
     (Executed_Corner_ID      : Planner_Corner_ID;
      Cancellation_Barrier_ID : Planner_Corner_ID;
      Current_Position        : Position);
   --  Notify loaded cancellation handlers after G-code intake has been stopped and in-flight G-code processing has
   --  drained. Executed_Corner_ID is the last primary planner corner executed before cancellation.
   --  Cancellation_Barrier_ID is the last primary planner corner assigned before the pending planner work is flushed.
   --  Current_Position is the physical position that planning will restart from.

   Startup_Position : constant Position := [others => 0.0 * mm];

   type Cancellation_Generation_Type is mod 2 ** 64;

   type Idle_Notification_Phase is (Active, Starting_Idle, Idle, Ending_Idle);

   type Idle_Activity_Generation is mod 2 ** 64;

   type Idle_Completion_Serial is mod 2 ** 64;

   type Idle_Activity_Completion is record
      Generation         : Idle_Activity_Generation;
      Completion_Serial  : Idle_Completion_Serial;
      Last_Command_Index : Command_Index;
   end record;

   protected Idle_Notification_State is
      procedure Reset;
      --  Discard all tracked activity and pending completions for a fresh controller run. Any completion already held
      --  by the notification worker is invalidated by advancing the generation.

      entry Begin_Activity (Notify : out Boolean);
      --  Record the start of a planner block before it can emit commands. Notify is True only when this is the first
      --  activity after an idle interval, in which case the caller must emit Idle_End and then call Finish_Idle_End.

      procedure Finish_Idle_End;
      --  Complete an Idle_End notification and mark the controller active.

      procedure Complete_Activity (Last_Command_Index : Command_Index);
      --  Record that a planner block and its block-end handler have finished. When this completes the outermost active
      --  block, publish Last_Command_Index for the notification worker to wait on.

      entry Abandon_Activities (Last_Command_Index : Command_Index);
      --  Discard activities interrupted by cancellation. If the controller was active, publish Last_Command_Index as
      --  the new completion boundary; if it was already idle, preserve the idle state without another notification.

      procedure Publish_Completion_When_Inactive (Last_Command_Index : Command_Index);
      --  Publish a completion boundary only when no planner block is active. Used to establish the initial idle state
      --  after module startup.

      entry Wait_For_Completion (Completion : out Idle_Activity_Completion);
      --  Wait for and consume the newest completion boundary that the notification worker must observe.

      procedure Begin_Idle (Completion : Idle_Activity_Completion; Notify : out Boolean);
      --  Begin an Idle_Start transition if Completion is still the newest boundary and no activity has since started.
      --  Notify is True only when the caller must emit Idle_Start and then call Finish_Idle.

      procedure Finish_Idle;
      --  Complete an Idle_Start notification and allow a waiting activity start to emit Idle_End.
   private
      Phase                    : Idle_Notification_Phase := Active;
      Generation               : Idle_Activity_Generation := 0;
      Active_Activity_Count    : Natural := 0;
      Latest_Completion_Serial : Idle_Completion_Serial := 0;
      Completion_Pending       : Boolean := False;
      Pending_Completion       : Idle_Activity_Completion :=
        (Generation => 0, Completion_Serial => 0, Last_Command_Index => 0);
   end Idle_Notification_State;

   protected type Handler_Instances is
      procedure Load (New_Handlers : Module_Instance_Vectors.Vector);
      procedure Clear;
      procedure Snapshot (Result : out Module_Instance_Vectors.Vector);
   private
      Handlers : Module_Instance_Vectors.Vector;
   end Handler_Instances;

   Pause_Handler_Instances         : Handler_Instances;
   Planner_State_Handler_Instances : Handler_Instances;
   Config_Save_Preparer_Instances  : Handler_Instances;
   Cancellation_Handler_Instances  : Handler_Instances;
   Idle_Notification_Instances     : Handler_Instances;
   --  We use these wrappers because the instances need to be accessed from multiple threads.

   protected Gcode_Cancellation_Barrier is
      entry Start_Cancellation;
      --  Begin a cancellation barrier. This entry is only open when no cancellation is active. Callers that should
      --  reject duplicate cancellation requests must use a conditional entry call. This rejects new work but does
      --  not wait for already-started submissions or line processing to finish.

      procedure Finish_Cancellation;
      --  End the active cancellation barrier and allow queued submissions and line processing to start again.

      entry Start_Submission;
      --  Register a G-code command or file submission. This entry is only open when no cancellation is active.
      --  Successful calls must be paired with Finish_Submission.

      procedure Finish_Submission;
      --  Mark one active G-code submission as finished.

      entry Start_Line;
      --  Register processing of one dequeued G-code line. This entry is only open when no cancellation is active.
      --  Successful calls must be paired with Finish_Line.

      procedure Finish_Line;
      --  Mark the current G-code line as finished.

      entry Wait_Until_Not_Processing;
      --  Wait until no G-code line is being processed and no command or file submission is active.

      entry Wait_Until_Not_Submitting;
      --  Wait until no command or file submission is active.

      entry Wait_Until_Not_Cancelling;
      --  Wait until the active cancellation barrier has finished.

      function Cancellation_Generation return Cancellation_Generation_Type;
      --  Return a monotonic generation value which changes when cancellation starts.

   private
      Processing_Line     : Boolean := False;
      Active_Submissions  : Natural := 0;
      Cancellation_Active : Boolean := False;
      Cancellation_Count  : Cancellation_Generation_Type := 0;
   end Gcode_Cancellation_Barrier;

   protected type Planner_State_Type is
      procedure Reset;
      function Get_Last_Position return Position;
      function Get_Last_Kinematic_Parameters return Motion_Planner.Kinematic_Parameters;
      procedure Set_Last_Position (Pos : Position);
      procedure Set_Last_Kinematic_Parameters (Params : Motion_Planner.Kinematic_Parameters);
   private
      Last_Position             : Position := [others => 0.0 * mm];
      Last_Kinematic_Parameters : Motion_Planner.Kinematic_Parameters := (others => <>);
   end Planner_State_Type;

   Primary_Planner_State : Planner_State_Type;
   Pause_Planner_State   : Planner_State_Type;
   Pause_Default_State   : Planner_State_Type;

   type Planner_Target_Kind is (Primary_Planner_Target, Pause_Planner_Target);

   type Planner_Wrapper is new Planner_Interface with record
      Startup_Mode : Boolean := False;
      Target       : Planner_Target_Kind := Primary_Planner_Target;
   end record;

   type Pause_Plan_End_Event is new Module_Types.Extra_Block_Resetting_Data with null record;

   overriding
   procedure Process_After_Block (This : Pause_Plan_End_Event; Context : Block_End_Context'Class);
   --  Process the marker at the terminal block of a pause plan.

   type Pause_Context_Data is new Pause_Context with record
      Pause_Position     : Position;
      Last_Command_Index : Command_Index;
   end record;

   overriding
   function Get_Pause_Position (This : Pause_Context_Data) return Position;
   --  Return the physical position at which pause handling began.

   overriding
   function Get_Last_Command_Index (This : Pause_Context_Data) return Command_Index;
   --  Return the last command index assigned before pause handling began.

   type Planner_Block_End_Context is limited new Module_Types.Block_End_Context with record
      First_Accel_Distance     : Length;
      Last_Command_Index       : Command_Index;
      Loop_Move_Offset         : Position_Offset;
      State_Catch_Up_Corner_ID : Planner_Corner_ID;
   end record;

   overriding
   function Get_Last_Position (This : Planner_Wrapper) return Position;
   --  Return the tracked endpoint of the planner selected by This.

   overriding
   function Get_Last_Kinematic_Parameters (This : Planner_Wrapper) return Motion_Planner.Kinematic_Parameters;
   --  Return the tracked kinematic parameters of the planner selected by This.

   overriding
   function Get_State_Anchor_Corner_ID (This : Planner_Wrapper) return Planner_Corner_ID;
   --  Return the primary-planner corner that anchors speculative module state for This's target planner.

   overriding
   function Get_Last_Executed_Corner_ID (This : Planner_Wrapper) return Planner_Corner_ID;
   --  Return the last primary-planner corner whose final step-generator command has been queued.

   overriding
   procedure Mark_Axis_Homed (This : Planner_Wrapper; Axis : Axis_Name);
   --  Mark Axis homed.

   overriding
   procedure Mark_Axis_Unhomed (This : Planner_Wrapper; Axis : Axis_Name);
   --  Mark Axis unhomed.

   overriding
   function Axis_Is_Homed (This : Planner_Wrapper; Axis : Axis_Name) return Boolean;
   --  Report whether Axis is marked as homed.

   overriding
   function Get_First_Accel_Distance (This : Planner_Block_End_Context) return Length;
   --  Return the length of the first acceleration portion in the completed planner block.

   overriding
   function Get_Last_Command_Index (This : Planner_Block_End_Context) return Command_Index;
   --  Return the final command index assigned to the completed planner block.

   overriding
   function Get_Loop_Move_Offset (This : Planner_Block_End_Context) return Position_Offset;
   --  Return the position offset accumulated by repeated loop-move cycles in the completed block.

   overriding
   procedure Wait_For_Idle (This : Planner_Block_End_Context);
   --  Wait until hardware has completed every command through This's final command index.

   overriding
   procedure Catch_Up_Planner_State (This : Planner_Block_End_Context);
   --  Commit speculative module state through the corner recorded for this completed block.

   overriding
   procedure Prepare_Config_For_Save (This : Planner_Block_End_Context);
   --  Ask registered modules to copy their current runtime state into their savable configuration handles.

   overriding
   procedure Add_Corner
     (This          : Planner_Wrapper;
      Pos           : Position;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Require_Homed : Boolean := True);
   --  Queue a linear move on This's selected planner and update its tracked endpoint.

   overriding
   procedure Add_Helix
     (This          : Planner_Wrapper;
      Center        : Position;
      Pos           : Position;
      Clockwise     : Boolean;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Require_Homed : Boolean := True);
   --  Queue a helical move on This's selected planner and update its tracked endpoint.

   overriding
   procedure Add_Corner_Data (This : Planner_Wrapper; Corner_Data : Extra_Corner_Data'Class);
   --  Attach extra data to the latest corner of This's selected planner.

   overriding
   procedure Flush
     (This           : Planner_Wrapper;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False);
   --  Queue a flush on This's selected planner, carrying Extra_Data to block completion.

   overriding
   procedure Flush_And_Change_Kinematic_Parameters
     (This           : Planner_Wrapper;
      Params         : Motion_Planner.Kinematic_Parameters;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False);
   --  Queue a flush that changes the selected planner's kinematic parameters for subsequent motion.

   overriding
   procedure Flush_And_Reset_Position
     (This           : Planner_Wrapper;
      New_Position   : Position;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False);
   --  Queue a flush that resets the selected planner's tracked and planned position to New_Position.

   Pipeline_Is_Set_Up         : Boolean := False;
   Gcode_Processor_Is_Running : Boolean := False
   with Atomic, Volatile;
   Current_Motor_Position_Map : My_Default_Modules_Children.Kinematics.Motor_Position_Map :=
     [others => [others => Length'Last]];
   --  Runtime setup state shared between Run and package-level callbacks such as G-code cancellation.
   --  Pipeline_Is_Set_Up tracks whether the planners and step generator have accepted Setup and can therefore be reset
   --  or reused safely. Current_Motor_Position_Map caches the active kinematics module's Motors_To_Position table
   --  after startup; Cancel_Gcode resets the planners and step generator without rebuilding the module tree, so it
   --  reuses this map to restart the same mechanical mapping at the current executed position.
   --
   --  Length'Last is the unused axis/motor sentinel consumed by To_Motor_Position. These invalid initial maps are
   --  overwritten before Pipeline_Is_Set_Up is set True.

   procedure Notify_Activity_Start;

   procedure Notify_Idle_Start (Completion : Idle_Activity_Completion);

end Prunt.Controller;
