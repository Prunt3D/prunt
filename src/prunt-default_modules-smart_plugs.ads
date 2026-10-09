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

with Prunt.Smart_Plugs;

generic
package Prunt.Default_Modules.Smart_Plugs is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class;
   --  Return the smart plug configuration schema.

   function Read_Configuration (Data : Config.Config_Data) return Prunt.Smart_Plugs.Plug_Config;
   --  Read server-level configuration without initializing board modules.

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance with private;

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

   type Watchdog_Seconds_Type is range 30 .. 3600 with Annotate => (Prunt_Config, User_Config);

   type User_Config_Plug_Kind is (Disabled, Enabled) with Annotate => (Prunt_Config, User_Config);

   type User_Config_Plug (Kind : User_Config_Plug_Kind := Disabled) is record
      --  Smart plugs are controlled from the web UI, even while the board is disconnected.
      --  Changes apply on server restart. Applying changed settings releases the old watchdog and switches it off.

      case Kind is
         when Disabled =>
            null;

         when Enabled =>
            Host : Virtual_String := "";
            --  IPv4 address or DNS hostname, optionally with a port. For example, 10.1.1.29.

            Provider : Prunt.Smart_Plugs.Provider_Kind := Prunt.Smart_Plugs.Tasmota;
            --  Plug firmware. Tasmota uses its local HTTP command API and relay 1.

            Watchdog_Seconds : Watchdog_Seconds_Type := 60;
            --  Auto-off timeout on the plug. Prunt refreshes it every five seconds after a manual power-on.
            --  A server stop, lost connection, or late refresh requires another manual power-on.
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Smart_Plug : User_Config_Plug := (others => <>)with
        Annotate => (Prunt_Config, Category, "machine", "Machine", 10);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;
   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;
   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   protected type Module_Instance is new My_Modules.Module_Instance with
      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);
   end Module_Instance;

end Prunt.Default_Modules.Smart_Plugs;
