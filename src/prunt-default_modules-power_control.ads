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
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
   with package My_Controller_Generic_Types is new Controller_Generic_Types (<>);
   Power_Control_Hardware : My_Controller_Generic_Types.Power_Control_Hardware_Parameters;
package Prunt.Default_Modules.Power_Control is

   type Module is new My_Modules.Module with null record;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;
   --  Return the supported G-code commands.

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path'Class; Message : Virtual_String);
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

   type Power_State_Change_Event is new Extra_Block_Resetting_Data with record
      Turn_On : Boolean;
   end record;

   overriding
   procedure Process_After_Block (This : Power_State_Change_Event; Context : Block_End_Context'Class);
   --  Forward a power-state change to the board after queued work has stopped.

   type Power_State_Report_Event is new Extra_Block_Resetting_Data with null record;

   overriding
   procedure Process_After_Block (This : Power_State_Report_Event; Context : Block_End_Context'Class);
   --  Read the board power state and log it.

   procedure Power_On (Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M80");
   --  Turn on the power supply.

   procedure Report_Power_State
     (Planner : Planner_Interface'Class;
      S       : Gcode_No_Value
      --  Select the power-state reporting variant.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M80");
   --  Report the power supply state.

   procedure Power_Off (Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M81");
   --  Turn off the power supply.

   protected type Module_Instance is new My_Modules.Module_Instance with
      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);
   private
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
   end Module_Instance;

end Prunt.Default_Modules.Power_Control;
