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

   type Module_Instance_Interface is synchronized interface;

   function Heater_Is_Enabled_In_Config
     (This : Module_Instance_Interface; Heater : Heater_Name) return Boolean
   is abstract;

   function Assigned_Thermistor
     (This : Module_Instance_Interface; Heater : Heater_Name) return Thermistor_Name
   is abstract;

   function Get_Heater_Parameters
     (This : Module_Instance_Interface; Heater : Heater_Name) return Heater_Parameters
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

      overriding
      procedure Gcode_Dispatch
        (Args               : in out Gcode_Arguments.Arguments;
         Planner            : Planner_Interface'Class;
         Command_Identifier : Gcode_Command_Identifier);

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
