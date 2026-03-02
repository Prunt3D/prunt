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
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;
with Prunt.Status_Manager;

generic
   type Motor_Name is (<>);
package Prunt.Default_Modules.Motor_Drivers is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   type Module_Instance (<>) is new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   procedure Start (This : in out Module_Instance);

   type Motor_Configuration is record
      Microsteps : Dimensionless range 1.0 .. 1.0E100 := 1.0;
      --  Simply acts as a divisor for the mm per unit value provided by the user. This can either be exposed to the
      --  user or set to a default value for a motor where it does not make sense for the user to set a microsteps
      --  value.
      --
      --  This is set in the module for the specific motor type as some motor types require microsteps to be selected
      --  from a list of possible options.
   end record;

   type Motor_Handler is abstract tagged limited private;

   procedure Enable_Motor (This : in out Motor_Handler) is abstract;
   --  Power up the motor if possible. Will only be called when the motor is enabled in the user configuration.

   procedure Disable_Motor (This : in out Motor_Handler) is abstract;
   --  Power down the motor if possible. May be called regardless of whether or not the motor is enabled in the user
   --  configuration.

   procedure Provide_Motor_Configuration
     (This          : in out Module_Instance;
      Motor         : Motor_Name;
      Configuration : Motor_Configuration;
      Handler       : Motor_Handler'Class);

   function Motor_Is_Enabled_In_Config (This : Module_Instance; Motor : Motor_Name) return Boolean;
   --  Returns `True` is the user has enabled the motor in the current configuration. This can be used to avoid
   --  checking the validity of constraints for motors which will never be used. This can also be used to check if it
   --  makes sense to allow the user to specify the motor for another parameter.

private

   type Motor_Handler is abstract tagged limited null record;

   type User_Config_Motion_Units_Direct_Entry is record
      --  Use this option if you already know the exact distance the printer's axis moves for each rotation of the
      --  motor. This is the most straightforward way to configure your steppers, but it requires you to have already
      --  calculated this value.

      Distance_Per_Rotation : Length range 1.0E-100 * mm .. 1.0E100 * mm := 1.0E100 * mm;
      --  This is the linear distance that the axis moves for a single rotation of the motor.

      Reverse_Direction : Boolean := False;
      --  If an axis moves in the opposite direction to what you expect (e.g., moving to the right when it should move
      --  to the left), you can enable this setting to reverse the motor's direction.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motion_Units_Lead_Screw is record
      --  Use this option to calculate the distance per rotation for an axis that is driven by a lead screw.

      Lead : Length range 1.0E-100 * mm .. 1.0E100 * mm := 1.0E100 * mm;
      --  The lead of a screw is the linear distance the nut travels for one complete revolution of the screw. This is
      --  often confused with pitch, which is the distance between adjacent threads. For a single-start screw, the lead
      --  and pitch are the same. However, for multi-start screws, the lead is the pitch multiplied by the number of
      --  starts. For example, a lead screw with a 2 mm pitch and 4 starts has a lead of 8 mm.

      Reverse_Direction : Boolean := False;
      --  If an axis moves in the opposite direction to what you expect (e.g., moving to the right when it should move
      --  to the left), you can enable this setting to reverse the motor's direction.

      Gear_Ratio : Dimensionless_Ratio := (1.0, 1.0)with
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

      Reverse_Direction : Boolean := False;
      --  If an axis moves in the opposite direction to what you expect (e.g., moving to the right when it should move
      --  to the left), you can enable this setting to reverse the motor's direction.

      Gear_Ratio : Dimensionless_Ratio := (1.0, 1.0)with
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

      Reverse_Direction : Boolean := False;
      --  If an axis moves in the opposite direction to what you expect (e.g., moving to the right when it should move
      --  to the left), you can enable this setting to reverse the motor's direction.

      Gear_Ratio : Dimensionless_Ratio := (1.0, 1.0)with
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
      Motors : User_Config_Motor_Array := [others => <>];
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config);

   type Module_Instance is new My_Modules.Module_Instance with record
      Config : User_Config;
   end record;

end Prunt.Default_Modules.Motor_Drivers;
