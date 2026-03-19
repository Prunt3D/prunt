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
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
   with package My_Controller_Generic_Types is new Controller_Generic_Types (<>);
   Fan_Hardware : My_Controller_Generic_Types.Fan_Hardware_Parameters_Array_Type;
package Prunt.Default_Modules.Fans is

   use My_Controller_Generic_Types;

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
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

   type User_Config_Fan_Dynamic_Duty_Cycle is record
      --  Allow this fan to be controlled while the printer is running with M106 and M107.

      Disable_Below : PWM_Scale := 0.0;
      --  Turn the fan fully off when the requested duty cycle is below this threshold.

      Maximum_Duty_Cycle : PWM_Scale := 1.0;
      --  Duty cycle used when g-code requests 100% fan power.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Fan_Always_On is record
      --  Keep this fan running whenever Prunt is connected to the machine.

      Duty_Cycle : PWM_Scale := 1.0;
      --  Fixed duty cycle for the fan.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Fan_Control_Method_Kind is (Dynamic_Duty_Cycle, Always_On)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Fan_Control_Method (Kind : User_Config_Fan_Control_Method_Kind := Always_On) is record
      --  Select how this fan is controlled.

      case Kind is
         when Dynamic_Duty_Cycle =>
            Dynamic_Duty_Cycle : User_Config_Fan_Dynamic_Duty_Cycle;

         when Always_On =>
            Always_On : User_Config_Fan_Always_On;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Fan is record
      --  This section contains the configuration for a single fan.

      Invert_PWM_Output : Boolean := False;
      --  Invert the PWM signal. This may be required depending on how the fan is wired.

      PWM_Frequency : Fan_PWM_Frequency := 30.0 * hertz;
      --  Set the PWM frequency. Low frequencies such as 30 Hz are often best for 2-wire fans, while 4-wire PWM fans
      --  usually require much higher values such as 25000 Hz.

      Control_Method : User_Config_Fan_Control_Method := (others => <>);
      --  Select how this fan is controlled.

      Use_High_Side_Switching : Boolean := False with
        Annotate =>
          (Prunt_Config, Present_When, "Fan_Hardware (Index_?).Kind = Low_Or_High_Side_Switching_Kind");
      --  Toggle the fan's power pin instead of its PWM pin. This is primarily useful for 3-wire fans where the
      --  tachometer must keep a fixed ground reference.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Fan_Array is array (Fan_Name) of User_Config_Fan
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Fans : User_Config_Fan_Array := [others => <>];
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   protected type Module_Instance is new My_Modules.Module_Instance with
      procedure Initialize (Config_In : User_Config);

      overriding
      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref);

      procedure Set_Fan_Speed
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_Integer;
         --  Fan index. Uses the first fan if not specified.
         S       : Gcode_Optional_Integer
         --  Fan speed from 0 to 255. Uses full speed if not specified.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M106");
      --  Set the speed of a fan.

      procedure Set_Fan_Speed
        (Planner : Planner_Interface'Class;
         P       : Virtual_String;
         --  Fan name.
         S       : Gcode_Optional_Integer
         --  Fan speed from 0 to 255. Uses full speed if not specified.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M106");
      --  Set the speed of a fan by name.

      procedure Turn_Off_Fan
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_Integer
         --  Fan index. Uses the first fan if not specified.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M107");
      --  Turn a fan off.

      procedure Turn_Off_Fan
        (Planner : Planner_Interface'Class;
         P       : Virtual_String
         --  Fan name.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M107");
      --  Turn a fan off by name.

      procedure Report_Fan_Tachometers
        (Planner : Planner_Interface'Class;
         S       : Gcode_Optional_Integer
         --  Interval in seconds between reports. Omit to report immediately.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M123");
      --  Report fan tachometer readings to the logger.

   private
      Config   : User_Config;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
   end Module_Instance;

end Prunt.Default_Modules.Fans;
