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
with Prunt.Default_Modules.Motor_Drivers;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;
with Prunt.Status_Manager;

generic
   with package My_Controller_Generic_Types is new Controller_Generic_Types (<>);
   Motor_Hardware : My_Controller_Generic_Types.Motor_Hardware_Parameters_Array_Type;
   with package Motor_Drivers_Module is new
     Default_Modules.Motor_Drivers (My_Controller_Generic_Types => My_Controller_Generic_Types);
package Prunt.Default_Modules.Basic_Motor_Drivers is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class;
   --  Return the configuration schema.

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;
   --  Return the supported G-code commands.

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
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

   type User_Config_Basic_Motor is record
      Microsteps : Dimensionless range 1.0 .. 1.0E100 := 1.0;
      --  This setting does not change the settings if the motor driver. It should be set to match the external
      --  settings of the driver.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motor (Fixed_Kind : Motor_Hardware_Kind := Basic_Motor_Kind) is record
      case Fixed_Kind is
         when Basic_Motor_Kind =>
            Basic_Parameters : User_Config_Basic_Motor;

         when TMC2240_UART_Kind =>
            null;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motor_Array is array (My_Controller_Generic_Types.Motor_Name) of User_Config_Motor
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Motors : User_Config_Motor_Array := [others => <>]with
        Annotate => (Prunt_Config, Fixed_Kind, "Motor_Hardware (Index_?).Kind");
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;
   --  Build the configuration schema.

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;
   --  Convert validated configuration data.

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);
   --  Store the configuration in Data.

   type Basic_Motor_Handler is new Motor_Drivers_Module.Motor_Handler with record
      Motor : My_Controller_Generic_Types.Motor_Name;
   end record;

   overriding
   procedure Enable_Motor (This : in out Basic_Motor_Handler);
   --  Enable the driver.

   overriding
   procedure Disable_Motor (This : in out Basic_Motor_Handler);
   --  Disable the driver.

   protected type Module_Instance is new My_Modules.Module_Instance with
      procedure Initialize
        (Config_In : User_Config; Motor_Drivers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref)
      with
        Pre =>
          Motor_Drivers_Module_Instance_Ref.Get.Element.all in Motor_Drivers_Module.Module_Instance_Interface'Class;

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

   end Module_Instance;

end Prunt.Default_Modules.Basic_Motor_Drivers;
