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
with Prunt.Controller_Generic_Types;
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
package Prunt.Default_Modules.Heaters is

   use My_Controller_Generic_Types;

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Module_Instance_Interface is synchronized interface;

   function Heater_Is_Enabled_In_Config (This : Module_Instance_Interface; Heater : Heater_Name) return Boolean
   is abstract;

   function Assigned_Thermistor (This : Module_Instance_Interface; Heater : Heater_Name) return Thermistor_Name
   is abstract;

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
      --  Use bang-bang control for this heater.

      Hysteresis : Temperature range 0.0 * celsius .. 1.0E100 * celsius := 0.0 * celsius;
      --  Temperature range around the target where the heater output does not switch.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Heater_Control_Method_Kind is (Disabled, PID, Bang_Bang)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Heater_Control_Method (Kind : User_Config_Heater_Control_Method_Kind := Disabled) is record
      --  Select how this heater is controlled.

      case Kind is
         when Disabled =>
            Disabled : User_Config_Empty;
            --  Disable this heater. The output remains off and the heater cannot be used.

         when PID =>
            PID : User_Config_Heater_PID;

         when Bang_Bang =>
            Bang_Bang : User_Config_Heater_Bang_Bang;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Heater is record
      --  This section contains the configuration for a single heater.

      Thermistor : Thermistor_Name := Thermistor_Name'First;
      --  Select the thermistor used to measure this heater's temperature.

      Check_Maximum_Cumulative_Error : Temperature range 0.0 * celsius .. 1.0E100 * celsius := 120.0 * celsius;
      --  Maximum accumulated temperature error allowed before the heater is treated as failed.

      Check_Gain_Time : Time range 0.0 * s .. 1.0E100 * s := 20.0 * s;
      --  Time window used when checking that the heater is gaining temperature.

      Check_Minimum_Gain : Temperature range 0.0 * celsius .. 1.0E100 * celsius := 2.0 * celsius;
      --  Minimum temperature rise required within the gain time to reset the cumulative error counter.

      Check_Hysteresis : Temperature range 0.0 * celsius .. 1.0E100 * celsius := 3.0 * celsius;
      --  Temperature range around the target where the heater is considered on target for fault checking.

      Control_Method : User_Config_Heater_Control_Method := (others => <>);
      --  Select the control method for this heater.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Heater_Array is array (Heater_Name) of User_Config_Heater
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Heaters : User_Config_Heater_Array := [others => <>];
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   --  TODO: Expose settings for default hotend/bed/chamber heater under User_Config.Gcode_Defaults.

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize (Config_In : User_Config);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Set_Idle_Timeout
        (Planner : Planner_Interface'Class;
         S       : Gcode_Optional_Integer;
         --  Timeout period in seconds, after which the temperature will be reduced. Setting this to zero will disable
         --  the idle timeout functionality.
         T       : Gcode_Optional_Float;
         --  Hotend trigger temperature in Celsius, below which timeouts will not trigger. This refers to the set
         --  temperature, not the real temperature.
         E       : Gcode_Optional_Float;
         --  Hotend idle temperature. Must not be greater than the trigger temperature.
         B       : Gcode_Optional_Float
         --  Bed idle temperature.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M86");
      --  Configure the idle timeout temperatures. These can be saved using M500. These can also be configured on the
      --  configuration page.
      --
      --  When the machine is idle for the given time with the hotend set to above the given temperature, the
      --  temperatures will be reduced to the given values. This will only ever decrease the bed temperature, it will
      --  never increase it.
      --
      --  Before the machine resumes, temperatures will be increased back to the previous values.

      --  TODO: Expose above in config records.

      procedure Disable_Idle_Timeout (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M87");
      --  Disable heater idle timeout functionality.

      --  TODO: Expose above in config records.

      procedure Set_Hotend_Temperature
        (Planner : Planner_Interface'Class;
         S       : Dimensionless
         --  Hotend target temperature in Celsius.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M104");
      --  Set the hotend target temperature and continue without waiting for the hotend to reach the given temperature.
      --
      --  This command differs from Marlin in that the B, F, I, and T parameters are not available.

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
         I       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float
         --  Bed target temperature in Celsius.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M140");
      --  Set the bed target temperature and continue without waiting for the bed to reach the given temperature.
      --
      --  This command differs from Marlin in that the I parameter is not available.

      procedure Set_Chamber_Temperature
        (Planner : Planner_Interface'Class;
         S       : Gcode_Optional_Float
         --  Chamber target temperature in Celsius.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M141");
      --  Set the chamber target temperature and continue without waiting for the chamber to reach the given temperature.

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
   private
      Config   : User_Config;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
   end Module_Instance;

end Prunt.Default_Modules.Heaters;
