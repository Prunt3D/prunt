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

private with Ada.Containers.Vectors;
with Ada.Tags;
with Prunt.Config;
with Prunt.Default_Modules.Input_Switches;
with Prunt.Default_Modules.Kinematics;
with Prunt.Default_Modules.TMC2240_Drivers;
with Prunt.Gcode_Arguments;
with Prunt.Limited_Shared_Pointers;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
   with package Kinematics_Module is new Default_Modules.Kinematics (<>);
   with package Input_Switches_Module is new Default_Modules.Input_Switches (<>);
   with package TMC2240_Drivers_Module is new Default_Modules.TMC2240_Drivers (<>);
   Interpolation_Time : Time;
package Prunt.Default_Modules.Homing is

   use My_Controller_Generic_Types;

   type Homing_Event_Subscriber is limited interface;
   --  This is for modules that need to perform actions around axis homing. Examples include reducing motor current for
   --  sensorless homing, enabling/disabling motors, or raising an exception if homing cannot be performed in the
   --  current state.

   procedure On_Homing_Axis_Start (This : in out Homing_Event_Subscriber; Axis : Axis_Name) is abstract;
   --  Called just before the homing moves for the given axis begin. May raise an exception to abort homing. No
   --  guarantees are provided as to which task this will be called from.

   procedure On_Homing_Axis_Finish (This : in out Homing_Event_Subscriber; Axis : Axis_Name) is abstract;
   --  Called just after homing of the given axis is complete and the axis position has been set. Called from the task
   --  executing the homing command. No guarantees are provided as to which task this will be called from.

   type Module is new My_Modules.Module with null record;

   type Axis_Homing_Parameters_Kind is
     (No_Axis_Homing_Parameters_Kind, Use_Input_Switch_Kind, Use_StallGuard2_Kind, Use_StallGuard4_Kind);

   type Homing_StallGuard2_Parameters is record
      Motor         : Motor_Name;
      Threshold     : User_Config_Integer range -64 .. 63;
      Enable_Filter : Boolean;
   end record;

   type Homing_StallGuard4_Parameters is record
      Motor         : Motor_Name;
      Threshold     : User_Config_Integer range 0 .. 255;
      Enable_Filter : Boolean;
   end record;

   type Axis_Homing_Parameters (Kind : Axis_Homing_Parameters_Kind := No_Axis_Homing_Parameters_Kind) is record
      case Kind is
         when No_Axis_Homing_Parameters_Kind =>
            null;

         when Use_Input_Switch_Kind =>
            Switch : Input_Switch_Name;

         when Use_StallGuard2_Kind =>
            Use_StallGuard2 : Homing_StallGuard2_Parameters;

         when Use_StallGuard4_Kind =>
            Use_StallGuard4 : Homing_StallGuard4_Parameters;
      end case;
   end record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class;
   --  Return the configuration schema.

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;
   --  Return the supported G-code commands.

   type Module_Instance_Interface is synchronized interface;

   procedure Subscribe_To_Homing
     (This       : in out Module_Instance_Interface;
      Subscriber : not null access function return Homing_Event_Subscriber'Class)
   is abstract;
   --  Register a homing subscriber.

   function Get_Homing_Parameters
     (This : Module_Instance_Interface; Axis : Axis_Name; Motor : Motor_Name) return Axis_Homing_Parameters
   is abstract;
   --  Return homing-driver parameters for Motor while homing Axis.

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Module_Instance_Interface with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;
   --  Create a module instance.

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);
   --  Dispatch a G-code command.

private

   function First_StallGuard_Motor return Motor_Name;

   function First_User_Visible_Input_Switch return Input_Switch_Name;

   type User_Config_Homing_Set_To_Value is record
      --  This homing method doesn't involve any physical movement. When the homing procedure is initiated, it simply
      --  sets the current position of the axis to the specified value.

      Position : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  The position to which the axis will be set when homed.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Detector_Kind is
     (Disabled, Input_Switch, StallGuard2, StallGuard4)
     --  Select the detector used to stop the homing move.
   with
     Annotate =>
       (Prunt_Config,
        Present_When,
        "Input_Switch",
        "(for some S in Input_Switch_Name => Input_Switches_Module.Input_Switch_Hardware (S).Visible_To_User)"),
     Annotate =>
       (Prunt_Config,
        Present_When,
        "StallGuard2",
        "(for some M in Motor_Name => TMC2240_Drivers_Module.Motor_Hardware (M).Kind = TMC2240_UART_Kind)"),
     Annotate =>
       (Prunt_Config,
        Present_When,
        "StallGuard4",
        "(for some M in Motor_Name => TMC2240_Drivers_Module.Motor_Hardware (M).Kind = TMC2240_UART_Kind)"),
     Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_StallGuard2 is record
      --  StallGuard2 is used with SpreadCycle. The homing module temporarily installs these settings before motion
      --  and restores the driver's normal settings after homing succeeds, fails, or is cancelled.

      Motor : Motor_Name := First_StallGuard_Motor with
        Annotate =>
          (Prunt_Config,
           Options_Expr,
           "[for M in Motor_Name when TMC2240_Drivers_Module.Motor_Hardware (M).Kind = TMC2240_UART_Kind => +(M'Image)]");
      --  Motor whose StallGuard2 output stops the homing move. The motor must drive the axis or delta tower being
      --  homed.

      Threshold : User_Config_Integer range -64 .. 63 := -64;
      --  StallGuard2 trigger threshold. Higher values make stall detection less sensitive. Start conservatively and
      --  tune this value with the same current and velocity that will be used for homing.

      Enable_Filter : Boolean := False;
      --  Enable filtering. This can improve precision, but reduces the measurement frequency to once every four full
      --  steps.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_StallGuard4 is record
      --  StallGuard4 is used with StealthChop. The homing module temporarily installs these settings before motion
      --  and restores the driver's normal settings after homing succeeds, fails, or is cancelled.

      Motor : Motor_Name := First_StallGuard_Motor with
        Annotate =>
          (Prunt_Config,
           Options_Expr,
           "[for M in Motor_Name when TMC2240_Drivers_Module.Motor_Hardware (M).Kind = TMC2240_UART_Kind => +(M'Image)]");
      --  Motor whose StallGuard4 output stops the homing move. The motor must drive the axis or delta tower being
      --  homed.

      Threshold : User_Config_Integer range 0 .. 255 := 255;
      --  StallGuard4 trigger threshold. Higher values make stall detection more sensitive. Start conservatively and
      --  tune this value with the same current and velocity that will be used for homing.

      Enable_Filter : Boolean := False;
      --  Enable filtering. This can improve precision, but reduces the measurement frequency to once every four full
      --  steps.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Detector (Kind : User_Config_Homing_Detector_Kind := Disabled) is record
      --  Select the detector used to terminate a homing approach.

      case Kind is
         when Disabled =>
            --  No detector has been configured. Detector-based homing cannot run until another option is selected.
            null;

         when Input_Switch =>
            --  Stop on a physical input configured by the Input Switches module.
            Switch : Input_Switch_Name := First_User_Visible_Input_Switch with
              Annotate =>
                (Prunt_Config,
                 Options_Expr,
                 "[for S in Input_Switch_Name when Input_Switches_Module.Input_Switch_Hardware (S).Visible_To_User => +(S'Image)]");
            --  Physical endstop input. Its normally-closed setting determines the triggered state.

         when StallGuard2 =>
            --  Stop when the selected motor's StallGuard2 output reports a stall.
            StallGuard2_Parameters : User_Config_Homing_StallGuard2 := (others => <>);

         when StallGuard4 =>
            --  Stop when the selected motor's StallGuard4 output reports a stall.
            StallGuard4_Parameters : User_Config_Homing_StallGuard4 := (others => <>);
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Motion is record
      --  Settings for homing sequences. The machine approaches quickly, executes a bounded tail when a detector stops
      --  the move, backs off far enough to release every detector, and approaches again at the slower velocity. During
      --  delta homing all three towers approach simultaneously and each motor monitors its configured detector.

      Fast_Approach_Velocity : Velocity range 0.000_001 * mm / s .. 1.0E100 * mm / s := 50.0 * mm / s;
      --  Requested velocity for the first detector approach. Normal kinematic limits still apply, so the actual
      --  velocity may be lower. For delta homing this is the upward Cartesian-Z velocity shared by all three towers.

      Slow_Approach_Velocity : Velocity range 0.000_001 * mm / s .. 1.0E100 * mm / s := 5.0 * mm / s;
      --  Requested velocity for the second, more precise detector approach and for backoff and post-home moves.
      --  Normal kinematic limits still apply.

      Maximum_Overtravel : Length range 0.000_001 * mm .. 1.0E100 * mm := 1.0 * mm;
      --  Maximum distance the machine may continue towards the endstop after it triggers. Set this no greater than the
      --  distance the endstop and machine can safely travel beyond the trigger point. The same limit is used for both
      --  approaches.

      Backoff_Distance : Length range 0.000_001 * mm .. 1.0E100 * mm := 2.0 * mm;
      --  Distance moved away from the detector after the fast pass and, when necessary, before the fast pass. Every
      --  detector must be inactive at the end of this move or homing fails.

      Fast_Maximum_Travel : Length range 0.000_001 * mm .. 1.0E100 * mm := 400.0 * mm;
      --  Maximum physical travel allowed while searching for a detector during the fast pass. Homing fails if any
      --  participating detector has not triggered within this distance.

   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_To_Detector is record
      --  Home a non-delta axis with the shared fast-approach, backoff, and slow-approach sequence.

      Detector : User_Config_Homing_Detector := (others => <>);
      --  Detector used by the motors participating in this axis. A physical input is shared by those motors, while
      --  sensorless homing uses each participating motor's own DIAG0 input.

      Move_Towards_Negative_Infinity : Boolean := True;
      --  Select the approach direction.

      Home_Position : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Known axis position at the detector trigger point. It may lie outside the normal travel limits.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Method_Kind is (Disabled, Set_To_Value, Home_To_Detector)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Method (Kind : User_Config_Homing_Method_Kind := Disabled) is record
      --  This section allows you to select the homing method for this axis. Homing is the process of moving the axes
      --  to a known, fixed position so that the machine knows the location of the toolhead.

      case Kind is
         when Disabled =>
            --  Homing is not yet configured for this axis. Movement on this axis will be disabled until a homing
            --  method is selected and configured.
            null;

         when Set_To_Value =>
            --  Mark the current axis position as a configured value without moving toward a detector.
            Set_To_Value : User_Config_Homing_Set_To_Value;

         when Home_To_Detector =>
            --  Perform a standard homing sequence.
            Home_To_Detector : User_Config_Homing_To_Detector;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Prereq_Must_Be_At_Position is record
      --  This axis must be homed prior to the parent axis and it must be at a specified position.

      Position : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  The position to move this axis to before homing the parent axis.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Prereq_Kind is (No_Requirement, Must_Be_Homed, Must_Be_At_Position)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Prereq (Kind : User_Config_Homing_Prereq_Kind := No_Requirement) is record
      --  This setting defines the required state of the selected axis before the parent axis can be homed. For
      --  example, you might require the Z-axis to be at a certain height before homing the X and Y axes to prevent
      --  the nozzle from crashing into the bed.

      case Kind is
         when No_Requirement =>
            --  There are no requirements for this axis during homing.
            null;

         when Must_Be_Homed =>
            --  This axis must be homed prior to the parent axis, but the position does not matter.
            null;

         when Must_Be_At_Position =>
            --  Home this prerequisite axis and move it to the configured position before homing the parent axis.
            Must_Be_At_Position : User_Config_Homing_Prereq_Must_Be_At_Position;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Prereq_Array is array (Axis_Name) of User_Config_Homing_Prereq
   with
     Annotate => (Prunt_Config, Present_When, "Index_? /= Index_?? and then Index_? /= E_Axis"),
     Annotate => (Prunt_Config, User_Config);

   type User_Config_Axis_Homing is record
      --  This section contains the homing procedure configuration for a single axis.

      Homing_Method : User_Config_Homing_Method := (others => <>);
      --  Select the homing method and configure its parameters.

      Motion : User_Config_Homing_Motion := (others => <>);
      --  Motion profile used by Home_To_Detector.

      Move_To_After : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Immediately after the homing procedure for this axis is complete, the axis will move to this position. This
      --  position must be within the machine's travel limits.

      Prerequisites : User_Config_Homing_Prereq_Array := [others => <>];
      --  Define the required state of each other axis before this axis can be homed.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axis_Homing_Array is array (Axis_Name) of User_Config_Axis_Homing
   with
     Annotate => (Prunt_Config, Tabbed),
     Annotate => (Prunt_Config, Present_When, "Index_? /= E_Axis"),
     Annotate => (Prunt_Config, User_Config);

   type User_Config_Linear_Delta_Homing is record
      --  Homing settings used only with linear-delta kinematics. A G28 request for any of X, Y, or Z homes all three
      --  towers together: fast approach, backoff, slow approach, coordinate reset, and an optional final Z move.
      --
      --  Warning: while one tower is stopping, the other towers continue towards their detectors. Large endstop-hit
      --  offsets therefore increase the temporary XY excursion during homing and can move the toolhead outside the
      --  configured workspace. Only configure offsets that are mechanically safe for the complete homing movement.
      --  Towers sharing an input also move individually during their final detector passes, which introduces an
      --  additional lateral excursion.

      Endstop_Hit_Offset_X : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Nozzle X offset from the workspace centre when all three tower detectors are at their trigger points. A
      --  circular workspace is always centred on X=0, Y=0.

      Endstop_Hit_Offset_Y : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Nozzle Y offset from the workspace centre when all three tower detectors are at their trigger points. A
      --  circular workspace is always centred on X=0, Y=0.

      Home_Z : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Nozzle Z coordinate when all three tower detectors are at their trigger points. The retained deceleration
      --  tail moves beyond this point; its endpoint is calculated from the planned profile and is not assigned Home_Z.

      Tower_A : User_Config_Homing_Detector := (others => <>);
      --  Select the homing detector for tower A. Towers may share an input; motors using that input stop together.

      Tower_B : User_Config_Homing_Detector := (others => <>);
      --  Select the homing detector for tower B. Towers may share an input; motors using that input stop together.

      Tower_C : User_Config_Homing_Detector := (others => <>);
      --  Select the homing detector for tower C. Towers may share an input; motors using that input stop together.

      Motion : User_Config_Homing_Motion := (others => <>);
      --  Motion profile shared by all three towers. The motors approach together and start the same retained
      --  deceleration tail when their configured detectors trigger. Motors sharing an input start their tails
      --  together, then receive individual slow passes after the shared signal has been released. A failure on any
      --  tower leaves X, Y, and Z unhomed.

      Move_To_After : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Cartesian Z position to move to after successful homing. It must lie within the configured Z limits.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Configuration is record
      --  The available options will change based on the selected kinematics mode. Set the kinematics mode for your
      --  printer before configuring these settings.

      Axes : User_Config_Axis_Homing_Array := [others => <>] with
        Annotate =>
          (Prunt_Config,
           Dynamic_Present_When,
           Kinematics_Module.Module_Instance'Tag,
           Kinematics_Module.Kinematics_Kind_Config_Path,
           "Cartesian",
           "Core_XY");
      --  Configure X, Y, and Z separately. This is displayed for Cartesian and CoreXY kinematics.

      Linear_Delta_Homing : User_Config_Linear_Delta_Homing := (others => <>) with
        Annotate =>
          (Prunt_Config,
           Dynamic_Present_When,
           Kinematics_Module.Module_Instance'Tag,
           Kinematics_Module.Kinematics_Kind_Config_Path,
           "Linear_Delta");
      --  Configure grouped three-tower homing. This is displayed for linear-delta kinematics. Bare G28, G28 X,
      --  G28 Y, and G28 Z all run the complete grouped procedure; individual Cartesian axes cannot be homed
      --  separately.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Homing : User_Config_Homing_Configuration := (others => <>);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;
   --  Build the configuration schema.

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;
   --  Convert validated configuration data.

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);
   --  Store the configuration in Data.

   function Required_Loop_Count (Maximum_Travel : Length; Approach_Velocity : Velocity) return Dimensionless;
   --  Return the ceiling of Maximum_Travel divided by the distance covered per interpolation period, plus two guard
   --  periods, with a minimum result of one.

   function Loop_Count_For (Maximum_Travel : Length; Approach_Velocity : Velocity) return Loop_Move_Count;
   --  Convert Required_Loop_Count to the hardware loop-count type, raising Constraint_Error when it is not
   --  representable.

   function Homing_Driver_Parameters_For
     (This : Module_Instance; Axis : Axis_Name; Motor : Motor_Name)
      return TMC2240_Drivers_Module.Homing_Driver_Parameters;
   --  Return the temporary TMC2240 homing configuration for Motor while Axis is being homed.

   procedure Configure_Homing_Driver (This : Module_Instance; Axis : Axis_Name);
   --  Apply the temporary TMC2240 settings required by every motor participating in homing Axis.

   procedure Restore_Homing_Driver (This : Module_Instance; Axis : Axis_Name);
   --  Restore every temporary TMC2240 setting applied while homing Axis.

   procedure Restore_All_Homing_Drivers (This : Module_Instance);
   --  Restore temporary TMC2240 homing settings for every homable axis.

   package Homing_Event_Subscriber_Shared_Pointers is new
     Prunt.Limited_Shared_Pointers (Homing_Event_Subscriber'Class);

   function Return_False (Left, Right : Homing_Event_Subscriber_Shared_Pointers.Ref with Unreferenced) return Boolean
   is (False);
   --  Return False.

   package Homing_Subscriber_Vectors is new
     Ada.Containers.Vectors (Positive, Homing_Event_Subscriber_Shared_Pointers.Ref, "=" => Return_False);

   type Axis_Start_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Axis                : Axis_Name;
   end record;

   overriding
   procedure Process_After_Block (This : Axis_Start_Event; Context : Block_End_Context'Class);
   --  Configure the axis homing drivers and notify subscribers that axis homing has started.

   type Axis_Loop_Move_Event is new Extra_Block_Resetting_Data with null record;

   overriding
   procedure Process_After_Block (This : Axis_Loop_Move_Event; Context : Block_End_Context'Class);
   --  Wait for an axis loop move to finish before processing the following block.

   type Axis_Finish_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Axis                : Axis_Name;
   end record;

   overriding
   procedure Process_After_Block (This : Axis_Finish_Event; Context : Block_End_Context'Class);
   --  Notify subscribers that axis homing has finished and restore the axis homing drivers.

   type Delta_Setup_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
   end record;

   overriding
   procedure Process_After_Block (This : Delta_Setup_Event; Context : Block_End_Context'Class);
   --  Configure all delta homing drivers and notify subscribers that grouped homing has started.

   type Delta_Loop_Move_Event is new Extra_Block_Resetting_Data with null record;

   overriding
   procedure Process_After_Block (This : Delta_Loop_Move_Event; Context : Block_End_Context'Class);
   --  Wait for a delta loop move to finish before processing the following block.

   type Check_Released_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Axis                : Axis_Name;
      Loop_Setup          : Loop_Move_Setup;
   end record;

   overriding
   procedure Process_After_Block (This : Check_Released_Event; Context : Block_End_Context'Class);
   --  Fail homing if any detector associated with the axis remains active after backoff.

   type Check_Motor_Released_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Loop_Setup          : Loop_Move_Setup;
      Motor               : Motor_Name;
   end record;

   overriding
   procedure Process_After_Block (This : Check_Motor_Released_Event; Context : Block_End_Context'Class);
   --  Fail homing if the selected motor group's detector remains active after backoff.

   type Delta_Clear_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
   end record;

   overriding
   procedure Process_After_Block (This : Delta_Clear_Event; Context : Block_End_Context'Class);
   --  Notify subscribers that grouped delta homing has finished and restore all homing drivers.

   procedure Auto_Home
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_No_Value;
      --  If included then the X axis will be homed.
      Y        : Gcode_Optional_No_Value;
      --  If included then the Y axis will be homed.
      Z        : Gcode_Optional_No_Value
      --  If included then the Z axis will be homed.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G28");
   --  Home the specified axes using the method and parameters specified in the configuration. If no axes are specified
   --  then all homing axes are homed.
   --
   --  The `ABCUVW` parameters from Marlin are not present as Prunt does not support these axes. The `LOR` parameters
   --  are not present but are planned for a future version. These parameters are present in Marlin.

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize
        (Config_In                              : User_Config;
         Input_Switches_Module_Instance_Ref_In  : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Kinematics_Module_Instance_Ref_In      : My_Modules.Module_Instance_Shared_Pointers.Ref;
         TMC2240_Drivers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      overriding
      procedure Subscribe_To_Homing (Subscriber : not null access function return Homing_Event_Subscriber'Class);

      overriding
      function Get_Homing_Parameters (Axis : Axis_Name; Motor : Motor_Name) return Axis_Homing_Parameters;

      procedure Notify_Homing_Axis_Start (Axis : Axis_Name);

      procedure Notify_Homing_Axis_Finish (Axis : Axis_Name);

      function Get_Config return User_Config;
      --  This is fine to use directly in g-code processors as we do not provide any way to change it post-startup in
      --  this module.

      function Switch_Is_Normally_Closed (Switch : Input_Switch_Name) return Boolean;
      function Switch_Is_Enabled (Switch : Input_Switch_Name) return Boolean;
      function Motor_Affects_Axis (Motor : Motor_Name; Axis : Axis_Name) return Boolean;
      function Get_Kinematics_Homing_Configuration return Kinematics_Module.Kinematics_Homing_Configuration;
      function Get_TMC2240_Drivers_Module_Instance_Ref return My_Modules.Module_Instance_Shared_Pointers.Ref;

   private

      Config                              : User_Config;
      Subscribers                         : Homing_Subscriber_Vectors.Vector;
      Input_Switches_Module_Instance_Ref  : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Kinematics_Module_Instance_Ref      : My_Modules.Module_Instance_Shared_Pointers.Ref;
      TMC2240_Drivers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
   end Module_Instance;

end Prunt.Default_Modules.Homing;
