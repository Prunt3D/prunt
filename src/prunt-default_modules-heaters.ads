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
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
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

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config);

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize (Config_In : User_Config);

      overriding
      procedure Start;

      procedure Set_Idle_Timeout
        (Planner : Planner_Interface'Class;
         S       : Gcode_Optional_Integer;
         T       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         B       : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M86");
      --  Configure the idle timeout temperatures.

      procedure Disable_Idle_Timeout (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M87");
      --  Disable the heater idle timeout.

      procedure Set_Hotend_Temperature
        (Planner : Planner_Interface'Class;
         I       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float;
         F       : Gcode_Optional_Float;
         B       : Gcode_Optional_Float;
         T       : Gcode_Optional_Integer)
      with Annotate => (Prunt_Config, Gcode_Command, "M104");
      --  Set a hotend target temperature without waiting.

      procedure Wait_For_Hotend_Temperature
        (Planner : Planner_Interface'Class;
         I       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float;
         R       : Gcode_Optional_Float;
         F       : Gcode_Optional_Float;
         B       : Gcode_Optional_Float;
         T       : Gcode_Optional_Integer)
      with Annotate => (Prunt_Config, Gcode_Command, "M109");
      --  Set and wait for a hotend target temperature.

      procedure Set_Bed_Temperature
        (Planner : Planner_Interface'Class; I : Gcode_Optional_Integer; S : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M140");
      --  Set the bed target temperature.

      procedure Set_Chamber_Temperature (Planner : Planner_Interface'Class; S : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M141");
      --  Set the chamber target temperature.

      procedure Set_Laser_Cooler_Temperature (Planner : Planner_Interface'Class; S : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M143");
      --  Set the laser cooler target temperature.

      procedure Wait_For_Bed_Temperature
        (Planner : Planner_Interface'Class;
         I       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float;
         R       : Gcode_Optional_Float;
         T       : Gcode_Optional_Integer)
      with Annotate => (Prunt_Config, Gcode_Command, "M190");
      --  Set and wait for a bed target temperature.

      procedure Wait_For_Chamber_Temperature
        (Planner : Planner_Interface'Class; S : Gcode_Optional_Float; R : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M191");
      --  Set and wait for a chamber target temperature.

      procedure Wait_For_Laser_Cooler_Temperature (Planner : Planner_Interface'Class; S : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M193");
      --  Set and wait for a laser cooler target temperature.

      procedure Set_Hotend_PID
        (Planner : Planner_Interface'Class;
         E       : Gcode_Optional_Integer;
         P       : Gcode_Optional_Float;
         I       : Gcode_Optional_Float;
         D       : Gcode_Optional_Float;
         C       : Gcode_Optional_Float;
         L       : Gcode_Optional_Float;
         F       : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M301");
      --  Set hotend PID values.

      procedure Cold_Extrude_Settings
        (Planner : Planner_Interface'Class; S : Gcode_Optional_Float; P : Gcode_Optional_Integer)
      with Annotate => (Prunt_Config, Gcode_Command, "M302");
      --  Configure cold extrusion settings.

      procedure PID_Autotune
        (Planner : Planner_Interface'Class;
         E       : Gcode_Optional_Float;
         C       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float;
         U       : Gcode_Optional_Integer;
         D       : Gcode_Optional_No_Value)
      with Annotate => (Prunt_Config, Gcode_Command, "M303");
      --  Run PID autotune.

      procedure Set_Bed_PID
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_Float;
         I       : Gcode_Optional_Float;
         D       : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M304");
      --  Set bed PID values.

      procedure Set_MPC_Values
        (Planner : Planner_Interface'Class;
         A       : Gcode_Optional_Float;
         C       : Gcode_Optional_Float;
         E       : Gcode_Optional_Integer;
         F       : Gcode_Optional_Float;
         H       : Gcode_Optional_Float;
         P       : Gcode_Optional_Float;
         R       : Gcode_Optional_Float;
         S       : Gcode_Optional_Integer;
         T       : Gcode_Optional_No_Value)
      with Annotate => (Prunt_Config, Gcode_Command, "M306");
      --  Set model-predictive temperature-control values.

      procedure Set_Chamber_PID
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_Float;
         I       : Gcode_Optional_Float;
         D       : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M309");
      --  Set chamber PID values.

      overriding
      function Heater_Is_Enabled_In_Config (Heater : Heater_Name) return Boolean;

      overriding
      function Assigned_Thermistor (Heater : Heater_Name) return Thermistor_Name;

      overriding
      function Get_Heater_Parameters (Heater : Heater_Name) return Heater_Parameters;
   private
      Config : User_Config;
   end Module_Instance;

end Prunt.Default_Modules.Heaters;
