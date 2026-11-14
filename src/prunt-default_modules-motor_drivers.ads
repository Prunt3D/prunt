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

with Ada.Tags;
with Prunt.Config;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;
with Prunt.Status_Manager;

private with Ada.Containers.Indefinite_Ordered_Maps;

generic
   use My_Controller_Generic_Types;
package Prunt.Default_Modules.Motor_Drivers is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class;
   --  Return the configuration schema.

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;
   --  Return the supported G-code commands.

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;
   --  Create a module instance.

   type Motor_Configuration is record
      Microsteps : Dimensionless range 1.0 .. 1.0E100 := 1.0;
      --  Simply acts as a divisor for the mm per unit value provided by the user. This can either be exposed to the
      --  user or set to a default value for a motor where it does not make sense for the user to set a microsteps
      --  value.
      --
      --  This is set in the module for the specific motor type as some motor types require microsteps to be selected
      --  from a list of possible options.
   end record;

   type Motor_Handler is interface;

   procedure Enable_Motor (This : in out Motor_Handler) is abstract;
   --  Power up the motor if possible. Will only be called when the motor is enabled in the user configuration.

   procedure Disable_Motor (This : in out Motor_Handler) is abstract;
   --  Power down the motor if possible. May be called regardless of whether or not the motor is enabled in the user
   --  configuration.

   type Module_Instance_Interface is synchronized interface;

   procedure Provide_Motor_Configuration
     (This          : in out Module_Instance_Interface;
      Motor         : Motor_Name;
      Configuration : Motor_Configuration;
      Handler       : Motor_Handler'Class)
   is abstract;
   --  Register a motor handler.

   function Motor_Is_Enabled_In_Config (This : Module_Instance_Interface; Motor : Motor_Name) return Boolean
   is abstract;
   --  Returns `True` is the user has enabled the motor in the current configuration. This can be used to avoid
   --  checking the validity of constraints for motors which will never be used. This can also be used to check if it
   --  makes sense to allow the user to specify the motor for another parameter.

   function Distance_Per_Rotation (This : Module_Instance_Interface; Motor : Motor_Name) return Length is abstract;
   --  Result will be negative if an increase in motor units results in a decrease in position as defined by the user.

   function Distance_Per_Unit (This : Module_Instance_Interface; Motor : Motor_Name) return Length is abstract;
   --  Result will be negative if an increase in motor units results in a decrease in position as defined by the user.

   function Distance_Per_Unit
     (This : Module_Instance_Interface; Motor : Motor_Name; Microsteps : Dimensionless) return Length
   is abstract;
   --  Like the above version, but does not rely on the motor configuration having been provided by the handler.

   type Motor_Axis_Map is array (Axis_Name, Motor_Name) of Boolean;

   procedure Set_Motor_Axis_Map (This : in out Module_Instance_Interface; Map : Motor_Axis_Map) is abstract;
   --  Provide the axes affected by each motor. Kinematics provides this after it resolves the configured motor map.

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Module_Instance_Interface
     and My_Modules.Motion_Preparer
     and Cancellation_Handler with private;

   overriding
   procedure Finalize (This : in out Module_Instance) is null;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);
   --  Dispatch a G-code command.

   overriding
   procedure Prepare_For_Motion
     (This          : Module_Instance;
      Self_Ref      : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner       : Planner_Interface'Class;
      Moving_Motors : My_Modules.Motor_Selection;
      Pause_Motion  : Boolean);
   --  Turn on any configured motor in Moving_Motors that is not already on or queued to be turned on, at the block
   --  boundary in front of the move. Motors are treated as off until Prunt has turned them on, as their state at
   --  startup is not known.

private

   type User_Config_Motion_Units_Direct_Entry is record
      --  Use this option if you already know the exact distance the printer's axis moves for each rotation of the
      --  motor. This is the most straightforward way to configure your steppers, but it requires you to have already
      --  calculated this value.

      Distance_Per_Rotation : Length range 1.0E-100 * mm .. 1.0E100 * mm := 1.0E100 * mm;
      --  This is the linear distance that the axis moves for a single rotation of the motor.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motion_Units_Lead_Screw is record
      --  Use this option to calculate the distance per rotation for an axis that is driven by a lead screw.

      Lead : Length range 1.0E-100 * mm .. 1.0E100 * mm := 1.0E100 * mm;
      --  The lead of a screw is the linear distance the nut travels for one complete revolution of the screw. This is
      --  often confused with pitch, which is the distance between adjacent threads. For a single-start screw, the lead
      --  and pitch are the same. However, for multi-start screws, the lead is the pitch multiplied by the number of
      --  starts. For example, a lead screw with a 2 mm pitch and 4 starts has a lead of 8 mm.

      Gear_Ratio : Dimensionless_Ratio := (1.0, 1.0) with
        Annotate => (Prunt_Config, Min, 1.0E-100),
        Annotate => (Prunt_Config, Max, 1.0E100);
      --  If there is a gear system between the motor and the lead screw, you need to specify the gear ratio here. The
      --  format is A:B, where A is the number of teeth on the gear attached to the lead screw, and B is the number of
      --  teeth on the gear attached to the motor. For a direct-drive system, where the motor is coupled directly to
      --  the lead screw, the gear ratio is 1:1.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motion_Units_Gear_With_Circumference is record
      --  Use this option to calculate the distance per step for an axis driven by a belt and pulley system, where the
      --  circumference of the pulley is known.

      Circumference : Length range 1.0E-100 * mm .. 1.0E100 * mm := 1.0E100 * mm;
      --  This is the circumference of the pulley that drives the belt attached to the linearly moving part.

      Gear_Ratio : Dimensionless_Ratio := (1.0, 1.0) with
        Annotate => (Prunt_Config, Min, 1.0E-100),
        Annotate => (Prunt_Config, Max, 1.0E100);
      --  If there is a gear system between the motor and the pulley, specify the gear ratio here. The format is A:B,
      --  where A is the number of teeth on the gear attached to the pulley, and B is the number of teeth on the gear
      --  attached to the motor. For a direct-drive system, the gear ratio is 1:1.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motion_Units_Gear_With_Tooth_Count_And_Pitch is record
      --  Use this option to calculate the distance per rotation for an axis driven by a belt and pulley system, using
      --  the pulley's tooth count and the belt's pitch.

      Tooth_Count : Dimensionless range 1.0E-100 .. 1.0E100 := 1.0E100;
      --  This is the number of teeth on the pulley that drives the belt attached to the linearly moving part.

      Tooth_Pitch : Length range 1.0E-100 * mm .. 1.0E100 * mm := 1.0E100 * mm;
      --  This is the distance between the centres of two adjacent teeth on the belt. Common belt pitches in 3D
      --  printers are 2mm (for GT2 belts) and 3mm (for GT3 belts).

      Gear_Ratio : Dimensionless_Ratio := (1.0, 1.0) with
        Annotate => (Prunt_Config, Min, 1.0E-100),
        Annotate => (Prunt_Config, Max, 1.0E100);
      --  If there is a gear system between the motor and the pulley, specify the gear ratio here. The format is A:B,
      --  where A is the number of teeth on the gear attached to the pulley, and B is the number of teeth on the gear
      --  attached to the motor. For a direct-drive system, the gear ratio is 1:1.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motion_Units_Kind is
     (Direct_Entry, Lead_Screw, Gear_With_Circumference, Gear_With_Tooth_Count_And_Pitch)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motion_Units (Kind : User_Config_Motion_Units_Kind := Direct_Entry) is record
      Units_Per_Rotation : Dimensionless range 1.0E-100 .. 1.0E100 := 1.0;
      --  This is the number of motor driver units that the motor needs to complete one full 360-degree rotation. For
      --  most common stepper motors, this value is 200, which corresponds to a 1.8-degree step angle. Some other
      --  stepper motors have a 0.9-degree step angle, which means they have 400 steps per rotation.
      --
      --  It is usually safer to start with a lower value here if you are unsure of what your motor and driver requires
      --  as a lower value will result in less rotation.

      Reverse_Direction : Boolean := False;
      --  If an axis moves in the opposite direction to what you expect (e.g., moving to the right when it should move
      --  to the left), you can enable this setting to reverse the motor's direction.

      case Kind is
         when Direct_Entry =>
            Direct_Entry : User_Config_Motion_Units_Direct_Entry;

         when Lead_Screw =>
            Lead_Screw : User_Config_Motion_Units_Lead_Screw;

         when Gear_With_Circumference =>
            Gear_With_Circumference : User_Config_Motion_Units_Gear_With_Circumference;

         when Gear_With_Tooth_Count_And_Pitch =>
            Gear_With_Tooth_Count_And_Pitch : User_Config_Motion_Units_Gear_With_Tooth_Count_And_Pitch;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motor is record
      Enabled : Boolean := False;
      --  Enable this motor. If a motor is not enabled, it cannot be assigned to an axis and will remain powered down
      --  if the motor supports a powered-down state.

      Motion_Units : User_Config_Motion_Units := (others => <>);
      --  This section determines how Prunt calculates the distance an axis moves for each unit of motor driver
      --  movement. You can either enter the value directly if you know it, or use one of the provided calculators for
      --  common mechanisms like lead screws and belt-driven systems.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motor_Array is array (Motor_Name) of User_Config_Motor
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Motors : User_Config_Motor_Array := [others => <>] with
        Annotate => (Prunt_Config, Category, "motors", "Motors & drivers", 40);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;
   --  Build the configuration schema.

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;
   --  Convert validated configuration data.

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);
   --  Store the configuration in Data.

   type Motor_Configuration_Array is array (Motor_Name) of Motor_Configuration;
   type Motor_Configuration_Provided_Array is array (Motor_Name) of Boolean;

   procedure Enable_Steppers
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_No_Value;
      --  Enable every configured motor affecting the X axis.
      Y        : Gcode_Optional_No_Value;
      --  Enable every configured motor affecting the Y axis.
      Z        : Gcode_Optional_No_Value;
      --  Enable every configured motor affecting the Z axis.
      E        : Gcode_Optional_No_Value
      --  Enable every configured motor affecting the E axis.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M17");
   --  Enable the configured motors affecting the selected axes after queued motion stops. With no axis arguments,
   --  enable every configured motor.

   procedure Disable_Steppers
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_No_Value;
      --  Disable every configured motor affecting the X axis.
      Y        : Gcode_Optional_No_Value;
      --  Disable every configured motor affecting the Y axis.
      Z        : Gcode_Optional_No_Value;
      --  Disable every configured motor affecting the Z axis.
      E        : Gcode_Optional_No_Value
      --  Disable every configured motor affecting the E axis.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M18");
   --  Disable the configured motors affecting the selected axes after queued motion stops. With no arguments, disable
   --  every configured motor. Every X, Y or Z axis affected by a disabled motor is marked unhomed. E stays homed
   --  because its origin is arbitrary and it has no homing procedure. A later move or homing move that needs a
   --  disabled motor turns it back on first.

   procedure Disable_Steppers_M84
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_No_Value;
      --  Disable every configured motor affecting the X axis.
      Y        : Gcode_Optional_No_Value;
      --  Disable every configured motor affecting the Y axis.
      Z        : Gcode_Optional_No_Value;
      --  Disable every configured motor affecting the Z axis.
      E        : Gcode_Optional_No_Value
      --  Disable every configured motor affecting the E axis.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M84");
   --  Alias of `M18`.

   type Axis_Selection is array (Axis_Name) of Boolean;

   package Motor_Handler_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (Key_Type => Motor_Name, Element_Type => Motor_Handler'Class);

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface
     and My_Modules.Motion_Preparer
     and Cancellation_Handler with
      procedure Initialize (Config_In : User_Config);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      overriding
      procedure Provide_Motor_Configuration
        (Motor : Motor_Name; Configuration : Motor_Configuration; Handler : Motor_Handler'Class);

      overriding
      procedure Set_Motor_Axis_Map (Map : Motor_Axis_Map);

      overriding
      function Motor_Is_Enabled_In_Config (Motor : Motor_Name) return Boolean;

      overriding
      function Distance_Per_Rotation (Motor : Motor_Name) return Length;

      overriding
      function Distance_Per_Unit (Motor : Motor_Name) return Length;

      overriding
      function Distance_Per_Unit (Motor : Motor_Name; Microsteps : Dimensionless) return Length;

      function Selected_Motors (Requested_Axes : Axis_Selection) return My_Modules.Motor_Selection;
      --  Return the configured motors affecting Requested_Axes, or every configured motor when none is requested.

      function Affected_Axes (Requested_Axes : Axis_Selection) return Axis_Selection;

      procedure Plan_Power (Motors : My_Modules.Motor_Selection; Enable : Boolean);
      --  Record that M17, M18 or M84 has queued Motors to be turned on or off.

      procedure Claim_Motors_To_Enable
        (Moving_Motors : My_Modules.Motor_Selection;
         Pause_Motion  : Boolean;
         To_Enable     : out My_Modules.Motor_Selection);
      --  Return the configured motors in Moving_Motors that still need a queued enable before moving, and record that
      --  one is queued for them. Primary motion compares against the state queued so far. Pause motion runs before
      --  queued primary work, so it compares against the current state and enables already queued by pause motion.

      procedure Release_Claim (Motors : My_Modules.Motor_Selection; Pause_Motion : Boolean);
      --  Undo Claim_Motors_To_Enable when its enable could not be queued.

      procedure Apply_Power (Motors : My_Modules.Motor_Selection; Enable : Boolean);
      --  Turn Motors on or off and record their current state.

      overriding
      procedure Handle_Cancel
        (Executed_Corner_ID      : Planner_Corner_ID;
         Cancellation_Barrier_ID : Planner_Corner_ID;
         Current_Position        : Position);
      --  Queued power changes are discarded at a cancellation barrier, so the current state becomes the planned one.

   private
      Config                 : User_Config;
      Self_Ref               : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
      Motor_Configs          : Motor_Configuration_Array;
      Motor_Configs_Provided : Motor_Configuration_Provided_Array := [others => False];
      Motor_Axes             : Motor_Axis_Map := [others => [others => False]];
      Motor_Handlers         : Motor_Handler_Maps.Map;
      Current_On             : My_Modules.Motor_Selection := [others => False];
      --  Motors Prunt has turned on and not turned off since. The state at startup is not known, so it starts off.
      Planned_On             : My_Modules.Motor_Selection := [others => False];
      --  Current_On after every power change queued on the primary planner has been applied.
      Pause_Enable_Queued    : My_Modules.Motor_Selection := [others => False];
      --  Motors with an enable queued by pause motion that has not been applied yet.
   end Module_Instance;

   type Motor_Power_Event is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Motors              : My_Modules.Motor_Selection;
      Enable              : Boolean;
   end record;

   overriding
   procedure Process_After_Block (This : Motor_Power_Event; Context : Block_End_Context'Class);
   --  Turn Motors on or off at the end of the preceding motion. M17, M18, M84 and the enables queued by
   --  Prepare_For_Motion all go through this event so that they take effect in G-code order. Disabling waits for the
   --  preceding motion to finish executing first. Enabling does not need to, because no queued step can be for a motor
   --  that is off, and the next block's commands are only queued after this returns.

end Prunt.Default_Modules.Motor_Drivers;
