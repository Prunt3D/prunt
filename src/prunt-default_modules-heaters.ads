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

--  TODO: Add a `Safe_Below` component to each heater to determine when we should raise an error if the heater is not
--  cooling down when off.

pragma Extensions_Allowed (On);

with Ada.Tags;
with Prunt.Config;
with Prunt.Controller_Generic_Types;
with Prunt.Default_Modules.Blocking_Tracker;
with Prunt.Default_Modules.Thermistors;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
   with package My_Controller_Generic_Types is new Controller_Generic_Types (<>);
   Heater_Hardware : My_Controller_Generic_Types.Heater_Hardware_Parameters_Array_Type;
   with package Thermistors_Module is new
     Default_Modules.Thermistors
       (My_Controller_Generic_Types => My_Controller_Generic_Types,
        Thermistor_Hardware         => <>);
   with package Blocking_Tracker_Module is new Default_Modules.Blocking_Tracker;
package Prunt.Default_Modules.Heaters is

   use My_Controller_Generic_Types;

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map;

   type Module_Instance_Interface is synchronized interface;

   function Heater_Is_Enabled_In_Config (This : Module_Instance_Interface; Heater : Heater_Name) return Boolean
   is abstract;

   function Assigned_Thermistor (This : Module_Instance_Interface; Heater : Heater_Name) return Thermistor_Name
   is abstract;
   --  Raises `Constraint_Error` if the heater is not enabled.

   function Get_Heater_Parameters (This : Module_Instance_Interface; Heater : Heater_Name) return Heater_Parameters
   is abstract;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Module_Instance_Interface with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

private

   Wait_Period : constant Duration := 0.1;

   type User_Config_Heater_PID is record
      --  Use PID control for this heater.

      Proportional_Scale : Dimensionless range 0.0 .. 1.0E100 := 0.0;
      --  Scale for the proportional term.

      Integral_Scale : Dimensionless range 0.0 .. 1.0E100 := 0.0;
      --  Scale for the integral term.

      Derivative_Scale : Dimensionless range 0.0 .. 1.0E100 := 0.0;
      --  Scale for the derivative term.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Heater_Bang_Bang is record
      --  Use bang-bang control for this heater. Bang-bang control turns on when the temperature drops below the
      --  setpoint minus half the hysteresis value, and turns off when the temperature rises above the setpoint plus
      --  half the hysteresis value.

      Hysteresis : Temperature range 0.0 * celsius .. 1.0E100 * celsius := 0.0 * celsius;
      --  This is the temperature range around the setpoint within which the heater will not change its state. A larger
      --  hysteresis will result in less frequent switching, but larger temperature swings.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Heater_Kind is (Disabled, Enabled) with Annotate => (Prunt_Config, User_Config);

   type User_Config_Heater_Control_Method_Kind is (PID, Bang_Bang) with Annotate => (Prunt_Config, User_Config);

   type User_Config_Heater_Control_Method (Kind : User_Config_Heater_Control_Method_Kind := PID) is record
      --  Select how this heater is controlled.

      case Kind is
         when PID =>
            PID : User_Config_Heater_PID;

         when Bang_Bang =>
            Bang_Bang : User_Config_Heater_Bang_Bang;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Heater (Kind : User_Config_Heater_Kind := Disabled) is record
      --  This section contains the configuration for a single heater.

      case Kind is
         when Disabled =>
            --  Disable this heater. The output remains off and the heater cannot be used.

            Disabled : User_Config_Empty;

         when Enabled =>
            Thermistor : Thermistor_Name := Thermistor_Name'First;
            --  Select the thermistor used to measure this heater's temperature.

            Check_Gain_Time : Time range 0.0 * s .. 1.0E100 * s := 20.0 * s;
            --  Time window used when checking that the heater is gaining temperature.

            Check_Minimum_Gain : Temperature range 0.0 * celsius .. 1.0E100 * celsius := 2.0 * celsius;
            --  Minimum temperature rise required within the gain time to reset the cumulative error counter.

            Check_Maximum_Cumulative_Error : Temperature range 0.0 * celsius .. 1.0E100 * celsius := 120.0 * celsius;
            --  Maximum accumulated temperature error allowed before the heater is treated as failed.

            --  TODO: Above needs a better description.

            Check_Hysteresis : Temperature range 0.0 * celsius .. 1.0E100 * celsius := 3.0 * celsius;
            --  Temperature range around the target where the heater is considered on target for fault checking.

            Control_Method : User_Config_Heater_Control_Method := (others => <>);
            --  Select the control method for this heater.
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Heater_Array is array (Heater_Name) of User_Config_Heater
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config_Default_Heater_Kind is (Disabled, Enabled) with Annotate => (Prunt_Config, User_Config);

   type User_Config_Default_Heater (Kind : User_Config_Default_Heater_Kind := Disabled) is record
      --  Select the heater used for the related g-code commands.

      case Kind is
         when Disabled =>
            Disabled : User_Config_Empty;

         when Enabled =>
            Heater : Heater_Name := Heater_Name'First;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Gcode_Defaults is record
      Hotend : User_Config_Default_Heater := (others => <>);
      --  Heater used for hotend temperature g-code commands.

      Bed : User_Config_Default_Heater := (others => <>);
      --  Heater used for bed temperature g-code commands.

      Chamber : User_Config_Default_Heater := (others => <>);
      --  Heater used for chamber temperature g-code commands.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Heaters        : User_Config_Heater_Array := [others => <>];
      Gcode_Defaults : User_Config_Gcode_Defaults := (others => <>);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   type Heater_Target_Status_Setters is array (Heater_Name) of Status_Manager.Lock_Free_Dimensionless_Setter;

   type Heater_Target_Command is new Extra_Corner_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Heater              : Heater_Name;
      Target_Status       : Status_Manager.Lock_Free_Dimensionless_Setter;
      Target              : Temperature;
   end record;

   overriding
   procedure Process (This : Heater_Target_Command; Last_Command_Index : Command_Index);
   --  Set the given heater to the given temperature and update the status value. This does not wait for the
   --  temperature to be reached.

   type Heater_Temperature_Wait is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref             : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Heater                          : Heater_Name;
      Target_Status                   : Status_Manager.Lock_Free_Dimensionless_Setter;
      Thermistors_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Assigned_Thermistor             : Thermistor_Name;
      Target                          : Temperature;
      Check_Hysteresis                : Temperature;
      Wait_Only_If_Heating            : Boolean;
      Ramp_Duration                   : Time;
      Ramp_Only_If_Heating            : Boolean;
   end record;

   overriding
   procedure Process_After_Block
     (This                 : Heater_Temperature_Wait;
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index;
      Loop_Move_Offset     : Position_Offset);

   type Heater_Target_Array is array (Heater_Name) of Temperature;

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize
        (Config_In                           : User_Config;
         Status_Emitter_In                   : Status_Manager.Status_Emitter;
         Thermistors_Module_Instance_In      : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Blocking_Tracker_Module_Instance_In : My_Modules.Module_Instance_Shared_Pointers.Ref);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Queue_Target_Command (Planner : Planner_Interface'Class; Heater : Heater_Name; Target : Temperature);

      procedure Wait_For_Hotend_Temperature_Heat
        (Planner : Planner_Interface'Class;
         S       : Dimensionless
         --  Hotend target temperature in Celsius.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M109");
      --  Set the hotend target temperature and wait for the hotend to go over the given temperature. This only waits
      --  for the hotend to heat up, it does not wait for the hotend to cool down.
      --
      --  This command differs from Marlin in that the B, F, I, and T parameters are not available.

      procedure Wait_For_Hotend_Temperature_Heat_Or_Cool
        (Planner : Planner_Interface'Class;
         R       : Dimensionless
         --  Hotend target temperature in Celsius.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M109");
      --  Set the hotend target temperature and wait for the hotend to reach the given temperature. This applies to
      --  heating or cooling.
      --
      --  This command differs from Marlin in that the B, F, I, and T parameters are not available.

      procedure Set_Bed_Temperature
        (Planner : Planner_Interface'Class;
         S       : Dimensionless
         --  Bed target temperature in Celsius.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M140");
      --  Set the bed target temperature and continue without waiting for the bed to reach the given temperature.
      --
      --  This command differs from Marlin in that the I parameter is not available.

      procedure Set_Chamber_Temperature
        (Planner : Planner_Interface'Class;
         S       : Dimensionless
         --  Chamber target temperature in Celsius.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M141");
      --  Set the chamber target temperature and continue without waiting for the chamber to reach the given
      --  temperature.

      procedure Wait_For_Bed_Temperature_Heat
        (Planner : Planner_Interface'Class;
         S       : Dimensionless;
         --  Bed target temperature in Celsius.
         T       : Dimensionless := 0.0
         --  If present then spread out heating over this many seconds.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M190");
      --  Set the bed target temperature and wait for the bed to go over the given temperature. This only waits for the
      --  bed to heat up, it does not wait for the bed to cool down.
      --
      --  If the T parameter is present then heating will be performed as a linear interpolation over the given time
      --  starting from the current temperature. If the temperature is already over the target temperature then no
      --  interpolation will be performed.
      --
      --  This command differs from Marlin in that the I parameter is not available and the T parameter is available
      --  for heating as well as cooling.

      procedure Wait_For_Bed_Temperature_Heat_Or_Cool
        (Planner : Planner_Interface'Class;
         R       : Dimensionless;
         --  Bed target temperature in Celsius.
         T       : Dimensionless := 0.0
         --  If present then spread out heating over this many seconds.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M190");
      --  Set the bed target temperature and wait for the bed to reach the given temperature. This applies to
      --  heating or cooling.
      --
      --  If the T parameter is present then heating or cooling will be performed as a linear interpolation over the
      --  given time starting from the current temperature.
      --
      --  This command differs from Marlin in that the I parameter is not available and the T parameter is available
      --  for heating as well as cooling.

      procedure Wait_For_Chamber_Temperature_Heat
        (Planner : Planner_Interface'Class;
         S       : Dimensionless
         --  Chamber target temperature in Celsius.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M191");
      --  Set the chamber target temperature and wait for the chamber to go over the given temperature. This only waits
      --  for the chamber to heat up, it does not wait for the chamber to cool down.

      procedure Wait_For_Chamber_Temperature_Heat_Or_Cool
        (Planner : Planner_Interface'Class;
         R       : Dimensionless
         --  Chamber target temperature in Celsius.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M191");
      --  Set the chamber target temperature and wait for the chamber to reach the given temperature. This applies to
      --  heating or cooling.

      --  TODO: PID and autotune.

      overriding
      function Heater_Is_Enabled_In_Config (Heater : Heater_Name) return Boolean;

      overriding
      function Assigned_Thermistor (Heater : Heater_Name) return Thermistor_Name;

      overriding
      function Get_Heater_Parameters (Heater : Heater_Name) return Heater_Parameters;

      procedure Set_Blocking_Tracker (Value : Virtual_String);

      procedure Clear_Blocking_Tracker;
   private
      procedure Queue_Temperature_Wait
        (Planner              : Planner_Interface'Class;
         Heater               : Heater_Name;
         Target               : Temperature;
         Wait_Only_If_Heating : Boolean;
         Ramp_Duration        : Time;
         Ramp_Only_If_Heating : Boolean);

      procedure Validate_Target (Heater : Heater_Name; Target : Temperature);

      function Get_Default_Heater (Selection : User_Config_Default_Heater; Display_Name : String) return Heater_Name;

      Config                               : User_Config;
      Self_Ref                             : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
      Thermistors_Module_Instance_Ref      : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Blocking_Tracker_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Target_Status_Setters                : Heater_Target_Status_Setters;
   end Module_Instance;

end Prunt.Default_Modules.Heaters;
