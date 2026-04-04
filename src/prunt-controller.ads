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
with Prunt.Config;
with Prunt.Controller_Generic_Types;
with Prunt.Exception_Occurrence_Holders;

private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Ordered_Maps;
private with Prunt.Command_Line_Arguments;
private with Prunt.Controller_Helpers;
private with Prunt.Default_Modules;
private with Prunt.Default_Modules.Basic_Config;
private with Prunt.Default_Modules.Config_Saving;
private with Prunt.Default_Modules.Blocking_Tracker;
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
private with Prunt.Default_Modules.Print_Job;
private with Prunt.Default_Modules.Shutdown;
private with Prunt.Default_Modules.TMC2240_Drivers;
private with Prunt.Default_Modules.Thermistors;
private with Prunt.Gcode_Arguments;
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
   --  If `Loop_Until_Hit` = False then the time delta of the move is `Interpolation_Time`, otherwise the time delta is
   --  `Loop_Interpolation_Time`.
   --
   --  If `Loop_Until_Hit` = True then the move should be repeated indefinitely until the condition set by
   --  `Setup_For_Loop_Move` is met. If the condition is met before the loop move is reached then
   --  `Report_External_Error` should be called. After the loop move is completed, `Report_Loop_Cycles` must be called.
   --
   --  If the queue runs dry on a move where `Safe_Stop_After` = False then `Report_External_Error` should be called
   --  and all heaters and motors should be disabled. Keep in mind that the motors may still be moving when the queue
   --  runs dry, so a delay may be required before disabling the motors.

   with procedure Reset_Position (Pos : Motor_Position);
   --  Reset the position of all motors to the given position. This procedure should not cause the motors to move, it
   --  just informs the motors of their position. This procedure will always be called before `Enqueue_Command` is
   --  first called. This procedure will not be called if the last call to `Enqueue_Command` had the `Safe_Stop_After`
   --  parameter set to False.

   with procedure Wait_Until_Idle (Last_Command_Index : Command_Index);
   --  Block until all queued commands are completed. This procedure will not be called if the last call to
   --  `Enqueue_Command` had the `Safe_Stop_After` parameter set to False. Will only be called from the same task as
   --  `Enqueue_Command`.

   with procedure Reset_Hardware;
   --  Reset the device to power-on state.

   Config_Path : String;
   --  Path of the printer configuration file.

   with
     function Get_Extra_HTTP_Content (Name : Virtual_String) return access constant Ada.Streams.Stream_Element_Array;
   --  Get a file to be exposed via the built in web server under the extras/ path. This is intended to be used for
   --  images for board-specific documentation but can also be used for other files such as firmware binaries.

   with function Get_Board_Specific_Documentation (Key : Virtual_String) return Virtual_String;
   --  Get the board specific documentation HTML to be appended to the documentation for a given configuration option.
   --  Keys can be displayed in the web interface by running the server with `--enable-documentation-dev-mode=true`.

   Update_Check : Update_Check_Details := (Method => None);
   --  Method to be used for update checking. When an update is available it will be displayed via the web interface.

   Disable_Default_Modules : Boolean := False;

   Extra_Modules : Module_Maps.Map := [];
package Prunt.Controller is

   pragma Extensions_Allowed (On);
   --  TODO: We need to repeat this here or we get errors, but it's unclear why. Does the pragma above the generic not
   --  carry through to the instantiation?

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

   procedure Report_External_Error (Message : String; Is_Fatal : Boolean := True);
   --  Report an error to Prunt and cause the printer to halt.

   procedure Report_External_Error (Occurrence : Ada.Exceptions.Exception_Occurrence; Is_Fatal : Boolean := True);
   --  Report an error to Prunt and cause the printer to halt.

   procedure Log (Message : String);
   --  Log a message for the user.

private

   use Prunt.Module_Types;

   package My_Logger is new Prunt.Logger;

   package My_Controller_Helpers is new Prunt.Controller_Helpers (Generic_Types);
   use My_Controller_Helpers;

   package My_Default_Modules is new Default_Modules (My_Modules, My_Logger);

   function Get_Current_Position return Position;

   function Get_Current_File_Name return Virtual_String;

   function Get_Current_File_Line return File_Line_Count;

   function Stepgen_Paused return Boolean;

   package My_Default_Modules_Children is
      package Idle_Emitter is new My_Default_Modules.Idle_Emitter;
      --  TODO: Idle emitter needs to be connected to controller idle interface.
      package Basic_Config is new My_Default_Modules.Basic_Config;
      package Blocking_Tracker is new My_Default_Modules.Blocking_Tracker;
      package Config_Saving is new My_Default_Modules.Config_Saving;
      package Machine_Name is new My_Default_Modules.Machine_Name (Config_Saving_Module => Config_Saving);
      package Machine_Idle_Timeout is new My_Default_Modules.Machine_Idle_Timeout;
      package Print_Job is new My_Default_Modules.Print_Job;
      package Power_Control is new My_Default_Modules.Power_Control;
      package Shutdown is new My_Default_Modules.Shutdown;
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
      package Motion is new My_Default_Modules.Motion;
      package Input_Shapers is new My_Default_Modules.Input_Shapers (Config_Saving_Module => Config_Saving);
      package Fans is new
        My_Default_Modules.Fans (My_Controller_Generic_Types => Generic_Types, Fan_Hardware => Hardware.Fan_Hardware);
      package Motor_Drivers is new My_Default_Modules.Motor_Drivers (My_Controller_Generic_Types => Generic_Types);
      package Kinematics is new
        My_Default_Modules.Kinematics
          (My_Controller_Generic_Types => Generic_Types,
           Config_Saving_Module        => Config_Saving,
           Motor_Drivers_Module        => Motor_Drivers,
           Input_Shapers_Module        => Input_Shapers);
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
      package TMC2240_Drivers is new
        My_Default_Modules.TMC2240_Drivers
          (My_Controller_Generic_Types => Generic_Types,
           Motor_Hardware              => Hardware.Motor_Hardware,
           Motor_Drivers_Module        => Motor_Drivers);
   end My_Default_Modules_Children;

   protected Last_Command_Executed
     with Lock_Free
   is
      procedure Report (Index : Command_Index);
      function Get return Command_Index;
   private
      Current_Index : Command_Index := 0;
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
       "Kinematics"               =>
         My_Default_Modules_Children.Kinematics.Module'(My_Modules.Module with null record),
       "Machine Idle Timeout"     =>
         My_Default_Modules_Children.Machine_Idle_Timeout.Module'(My_Modules.Module with null record),
       "Machine Name"             =>
         My_Default_Modules_Children.Machine_Name.Module'(My_Modules.Module with null record),
       "Motion"                   => My_Default_Modules_Children.Motion.Module'(My_Modules.Module with null record),
       "Motor Drivers"            =>
         My_Default_Modules_Children.Motor_Drivers.Module'(My_Modules.Module with null record),
       "Power Control"            =>
         My_Default_Modules_Children.Power_Control.Module'(My_Modules.Module with null record),
       "Print Job"                => My_Default_Modules_Children.Print_Job.Module'(My_Modules.Module with null record),
       "Shutdown"                 => My_Default_Modules_Children.Shutdown.Module'(My_Modules.Module with null record),
       "TMC2240 Drivers"          =>
         My_Default_Modules_Children.TMC2240_Drivers.Module'(My_Modules.Module with null record),
       "Thermistors"              =>
         My_Default_Modules_Children.Thermistors.Module'(My_Modules.Module with null record)];

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

   Maximum_Motor_Delta : constant Motor_Position :=
     [for S in Motor_Name => Hardware.Motor_Hardware (S).Maximum_Delta_Per_Command];

   Active_Config_File : constant Config.Config_File := Config.Create (Config_Path, Active_Module_Config_Schemas);

   package My_Update_Checker is new Update_Checker (My_Logger, Update_Check);

   package Extra_Block_Resetting_Data_Holders is new
     Ada.Containers.Indefinite_Holders (Module_Types.Extra_Block_Resetting_Data'Class, Module_Types."=");

   package My_Motion_Planner is new
     Motion_Planner.Planner
       (Flush_Resetting_Data_Type         => Extra_Block_Resetting_Data_Holders.Holder,
        Flush_Resetting_Data_Type_Default =>
          Extra_Block_Resetting_Data_Holders.To_Holder (Module_Types.Extra_Block_Resetting_Data'(null record)),
        Corner_Extra_Data_Type            => Module_Types.Extra_Corner_Data'Class,
        Home_Move_Minimum_Coast_Time      => 5.0 * Interpolation_Time,
        Interpolation_Time                => Interpolation_Time,
        Motor_Name                        => Motor_Name,
        Motor_Position                    => Motor_Position,
        Maximum_Motor_Delta               => Maximum_Motor_Delta,
        Log                               => My_Logger.Log,
        Runner_CPU                        => Command_Line_Arguments.Motion_Planner_CPU);

   procedure Start_Planner_Block
     (Resetting_Data : Extra_Block_Resetting_Data_Holders.Holder; Last_Command_Index : Command_Index);

   procedure Enqueue_Command_Internal
     (Pos             : Position;
      Motor_Pos       : Motor_Position;
      Index           : Command_Index;
      Loop_Until_Hit  : Boolean;
      Safe_Stop_After : Boolean;
      Vel_Ratio       : Dimensionless);

   procedure Start_Corner (Last_Command_Index : Command_Index; Data : Module_Types.Extra_Corner_Data'Class);

   procedure Finish_Planner_Block
     (Resetting_Data       : Extra_Block_Resetting_Data_Holders.Holder;
      Next_Block_Pos       : Motor_Position;
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index;
      Loop_Move_Offset     : Position_Offset);

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
       (Planner              => My_Motion_Planner,
        Motor_Name           => Motor_Name,
        Motor_Position       => Motor_Position,
        Start_Planner_Block  => Start_Planner_Block,
        Enqueue_Command      => Enqueue_Command_Internal,
        Start_Corner         => Start_Corner,
        Finish_Planner_Block => Finish_Planner_Block,
        Loop_Cycle_Reporter  => Loop_Move_Cycles'Access,
        Interpolation_Time   => Interpolation_Time,
        Runner_CPU           => Command_Line_Arguments.Step_Generator_CPU);

   My_Gcode_Queue : Gcode_Queues.Queue;

   procedure Apply_Untrusted_Config_Patch
     (Patch : Virtual_String; Result : out Virtual_String; Errors : out Config.Config_Error_Vectors.Vector);

   procedure Submit_Gcode_Command (Command : Virtual_String; Succeeded : out Boolean);

   procedure Submit_Gcode_File (Path : Virtual_String; Succeeded : out Boolean);

   procedure Reload_Server;

   function Get_Status_Values_String return Virtual_String
   is (My_Status_Data.JSON_Data);

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
        Reload_Server               => Reload_Server,
        Get_Extra_HTTP_Content      => Get_Extra_HTTP_Content,
        Exception_Occurrence_Holder => Exception_Occurrence_Holder.all,
        Config_Schema_String        => Active_Config_File.Get_Schema_String,
        Status_Schema_String        => My_Status_Data.JSON_Schema,
        Gcode_JSON_String           => Active_Module_Gcode_JSON_String,
        Get_Status_Values_String    => Get_Status_Values_String,
        Port                        => Command_Line_Arguments.Web_Server_Port);

   function Recursive_Module_Initialization
     (Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      My_Config_File      : Config.Config_File;
      Log_Dependency_Tree : Boolean := False) return Module_Instance_Maps.Map;

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

   protected Planner_State is
      procedure Reset;
      function Get_Last_Position return Position;
      function Get_Last_Kinematic_Parameters return Motion_Planner.Kinematic_Parameters;
      procedure Set_Last_Position (Pos : Position);
      procedure Set_Last_Kinematic_Parameters (Params : Motion_Planner.Kinematic_Parameters);
   private
      Last_Position             : Position := [others => 0.0 * mm];
      Last_Kinematic_Parameters : Motion_Planner.Kinematic_Parameters := (others => <>);
   end Planner_State;

   type Planner_Wrapper is new Planner_Interface with record
      Startup_Mode : Boolean := False;
   end record;

   overriding
   function Get_Last_Position (This : Planner_Wrapper) return Position;

   overriding
   function Get_Last_Kinematic_Parameters (This : Planner_Wrapper) return Motion_Planner.Kinematic_Parameters;

   overriding
   procedure Mark_Axis_Homed (This : Planner_Wrapper; Axis : Axis_Name);

   overriding
   procedure Mark_Axis_Unhomed (This : Planner_Wrapper; Axis : Axis_Name);

   overriding
   function Axis_Is_Homed (This : Planner_Wrapper; Axis : Axis_Name) return Boolean;

   overriding
   procedure Add_Corner
     (This          : Planner_Wrapper;
      Pos           : Position;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Require_Homed : Boolean := True;
      Corner_Data   : Extra_Corner_Data'Class := Extra_Corner_Data'(null record));

   overriding
   procedure Add_Corner_Data (This : Planner_Wrapper; Corner_Data : Extra_Corner_Data'Class);

   overriding
   procedure Flush
     (This           : Planner_Wrapper;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False);

   overriding
   procedure Flush_And_Change_Kinematic_Parameters
     (This           : Planner_Wrapper;
      Params         : Motion_Planner.Kinematic_Parameters;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False);

   overriding
   procedure Flush_And_Reset_Position
     (This           : Planner_Wrapper;
      New_Position   : Position;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False);

end Prunt.Controller;
