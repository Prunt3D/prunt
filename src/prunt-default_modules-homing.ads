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

pragma Extensions_Allowed (On);

private with Ada.Containers.Vectors;
with Ada.Tags;
with Prunt.Config;
with Prunt.Controller_Generic_Types;
with Prunt.Gcode_Arguments;
with Prunt.Limited_Shared_Pointers;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
   with package My_Controller_Generic_Types is new Controller_Generic_Types (<>);
   Motor_Hardware : My_Controller_Generic_Types.Motor_Hardware_Parameters_Array_Type;
   Input_Switch_Hardware : My_Controller_Generic_Types.Input_Switch_Hardware_Parameters_Array_Type;
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
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   --  overriding
   --  function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Module_Instance_Interface is synchronized interface;

   procedure Subscribe_To_Homing
     (This       : in out Module_Instance_Interface;
      Subscriber : not null access function return Homing_Event_Subscriber'Class)
   is abstract;

   function Get_Homing_Parameters (This : Module_Instance_Interface; Axis : Axis_Name) return Axis_Homing_Parameters
   is abstract;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Module_Instance_Interface with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

private

   function First_StallGuard2_Motor return Motor_Name;

   function First_StallGuard4_Motor return Motor_Name;

   function First_User_Visible_Input_Switch return Input_Switch_Name;

   type User_Config_Homing_Set_To_Value is record
      --  This homing method doesn't involve any physical movement. When the homing procedure is initiated, it simply
      --  sets the current position of the axis to the specified value. This should generally only be used for the E
      --  axis.

      Position : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  The position to which the axis will be set when homed.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Use_Input_Switch is record
      --  This is the traditional homing method that uses a physical switch (endstop) to determine the end of the
      --  axis. The homing procedure consists of a fast move towards the switch, a back-off move, and then a slower
      --  move to accurately record the switch position.

      Switch : Input_Switch_Name := First_User_Visible_Input_Switch with
        Annotate =>
          (Prunt_Config,
           Options_Expr,
           "[for S in Input_Switch_Name when Input_Switch_Hardware (S).Visible_To_User => +(S'Image)]");
      --  Select the input switch to be used for homing this axis.

      Move_Towards_Negative_Infinity : Boolean := True;
      --  This setting determines the direction of the homing move. If enabled, the axis will move towards the minimum
      --  position. If disabled, it will move towards the maximum position.

      First_Move_Distance : Length range 0.000001 * mm .. 1.0E100 * mm := 0.1 * mm;
      --  This is the distance the axis is allowed to travel past the switch trigger point during the initial fast
      --  homing move.

      Back_Off_Move_Distance : Length range 0.0 * mm .. 1.0E100 * mm := 2.0 * mm;
      --  After the switch is triggered for the first time, the axis will back off by this distance before starting
      --  the second, slower homing move.

      Second_Move_Distance : Length range 0.000001 * mm .. 1.0E100 * mm := 0.1 * mm;
      --  This is the distance the axis is allowed to travel past the switch trigger point during the second, slower
      --  homing move.

      Switch_Position : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  This is the known position of the switch on the axis. This value is allowed to be outside of the machine's
      --  travel limits.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Use_StallGuard2 is record
      --  This homing method uses Trinamic's StallGuard2 feature for sensorless homing.

      Motor : Motor_Name := First_StallGuard2_Motor with
        Annotate =>
          (Prunt_Config,
           Options_Expr,
           "[for M in Motor_Name when Motor_Hardware (M).Kind = TMC2240_UART_Kind => +(M'Image)]");
      --  Select the motor that will be used for stall detection.

      Move_Towards_Negative_Infinity : Boolean := True;
      --  This setting determines the direction of the homing move. If enabled, the axis will move towards the minimum
      --  position. If disabled, it will move towards the maximum position.

      Threshold : User_Config_Integer range -64 .. 63 := -64;
      --  Threshold for StallGuard2. Higher values are less sensitive.

      Enable_Filter : Boolean := False;
      --  Enabling this filter can provide more precise measurements, but it reduces the measurement frequency to once
      --  every four full steps.

      Stop_Position : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  This is the position where the axis will hit the hard endstop. This value is allowed to be outside of the
      --  machine's travel limits.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Method_Kind is (Disabled, Set_To_Value, Use_Input_Switch, Use_StallGuard2, Use_StallGuard4)
   with
     Annotate =>
       (Prunt_Config,
        Present_When,
        "Use_Input_Switch",
        "(for some S in Input_Switch_Name => Input_Switch_Hardware (S).Visible_To_User)"),
     Annotate =>
       (Prunt_Config,
        Present_When,
        "Use_StallGuard2",
        "(for some M in Motor_Name => Motor_Hardware (M).Kind = TMC2240_UART_Kind)"),
     Annotate =>
       (Prunt_Config,
        Present_When,
        "Use_StallGuard4",
        "(for some M in Motor_Name => Motor_Hardware (M).Kind = TMC2240_UART_Kind)"),
     Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Use_StallGuard4 is record
      --  This homing method uses Trinamic's StallGuard4 feature for sensorless homing.

      Motor : Motor_Name := First_StallGuard4_Motor with
        Annotate =>
          (Prunt_Config,
           Options_Expr,
           "[for M in Motor_Name when Motor_Hardware (M).Kind = TMC2240_UART_Kind => +(M'Image)]");
      --  Select the motor that will be used for stall detection.

      Move_Towards_Negative_Infinity : Boolean := True;
      --  This setting determines the direction of the homing move. If enabled, the axis will move towards the minimum
      --  position. If disabled, it will move towards the maximum position.

      Threshold : User_Config_Integer range 0 .. 255 := 255;
      --  Threshold for StallGuard4. Higher values are more sensitive.

      Enable_Filter : Boolean := False;
      --  Enabling this filter can provide more precise measurements, but it reduces the measurement frequency to once
      --  every four full steps.

      Stop_Position : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  This is the position where the axis will hit the hard endstop. This value is allowed to be outside of the
      --  machine's travel limits.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Method (Kind : User_Config_Homing_Method_Kind := Disabled) is record
      --  This section allows you to select the homing method for this axis. Homing is the process of moving the axes
      --  to a known, fixed position so that the machine knows the location of the toolhead.

      case Kind is
         when Disabled =>
            Disabled : User_Config_Empty;
            --  Homing is not yet configured for this axis. Movement on this axis will be disabled until a homing
            --  method is selected and configured.

         when Set_To_Value =>
            Set_To_Value : User_Config_Homing_Set_To_Value;

         when Use_Input_Switch =>
            Use_Input_Switch : User_Config_Homing_Use_Input_Switch;

         when Use_StallGuard2 =>
            Use_StallGuard2 : User_Config_Homing_Use_StallGuard2;

         when Use_StallGuard4 =>
            Use_StallGuard4 : User_Config_Homing_Use_StallGuard4;
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
            Empty_1 : User_Config_Empty;
            --  There are no requirements for this axis during homing.

         when Must_Be_Homed =>
            Empty_2 : User_Config_Empty;
            --  This axis must be homed prior to the parent axis, but the position does not matter.

         when Must_Be_At_Position =>
            Must_Be_At_Position : User_Config_Homing_Prereq_Must_Be_At_Position;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Homing_Prereq_Array is array (Axis_Name) of User_Config_Homing_Prereq
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axis_Homing is record
      --  This section contains the homing procedure configuration for a single axis.

      Homing_Method : User_Config_Homing_Method := (others => <>);
      --  Select the homing method and configure its parameters.

      Velocity_Limit : Velocity range 0.000001 * mm / s .. 50.0 * mm / s := 50.0 * mm / s;
      --  This sets a velocity limit specifically for the homing moves on this axis. This does not allow the movement
      --  to exceed any regular velocity limits.

      Move_To_After : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Immediately after the homing procedure for this axis is complete, the axis will move to this position. This
      --  position must be within the machine's travel limits.

      Prerequisites : User_Config_Homing_Prereq_Array := (others => <>);
      --  Define the required state of each other axis before this axis can be homed.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axis_Homing_Array is array (Axis_Name) of User_Config_Axis_Homing
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Homing : User_Config_Axis_Homing_Array := [others => <>];
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config);

   package Homing_Event_Subscriber_Shared_Pointers is new
     Prunt.Limited_Shared_Pointers (Homing_Event_Subscriber'Class);

   function Return_False (Left, Right : Homing_Event_Subscriber_Shared_Pointers.Ref with Unreferenced) return Boolean
   is (False);

   package Homing_Subscriber_Vectors is new
     Ada.Containers.Vectors (Positive, Homing_Event_Subscriber_Shared_Pointers.Ref, "=" => Return_False);

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize (Config_In : User_Config);

      overriding
      procedure Start;

      overriding
      procedure Gcode_Dispatch
        (Args               : in out Gcode_Arguments.Arguments;
         Planner            : Planner_Interface'Class;
         Command_Identifier : Gcode_Command_Identifier);

      overriding
      procedure Subscribe_To_Homing (Subscriber : not null access function return Homing_Event_Subscriber'Class);

      overriding
      function Get_Homing_Parameters (Axis : Axis_Name) return Axis_Homing_Parameters;
   private
      Config      : User_Config;
      Subscribers : Homing_Subscriber_Vectors.Vector;
   end Module_Instance;

end Prunt.Default_Modules.Homing;
