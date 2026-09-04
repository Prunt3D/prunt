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

package body Prunt.Default_Modules.Power_Control is

   pragma Extensions_Allowed (On);

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   overriding
   procedure Process_After_Block (This : Power_State_Change_Event; Context : Block_End_Context'Class) is
   begin
      Context.Wait_For_Idle;

      if This.Turn_On then
         Power_Control_Hardware.Turn_On.all;
      else
         Power_Control_Hardware.Turn_Off.all;
      end if;
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Power_State_Report_Event; Context : Block_End_Context'Class) is
      pragma Unreferenced (This);
   begin
      Context.Wait_For_Idle;
      Context.Log (+(if Power_Control_Hardware.Is_On.all then "Power supply is on" else "Power supply is off"));
   end Process_After_Block;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
   begin
      return Result : Module_Instance;
   end Initialize;

   protected body Module_Instance is
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Planner);
      begin
         Self_Ref := Self_Ref_In;
      end Start;
   end Module_Instance;

   procedure Power_On (Planner : Planner_Interface'Class) is
   begin
      Planner.Flush (Power_State_Change_Event'(Turn_On => True));
   end Power_On;

   procedure Report_Power_State (Planner : Planner_Interface'Class; S : Gcode_No_Value) is
   begin
      pragma Unreferenced (S);
      Planner.Flush (Power_State_Report_Event'(null record));
   end Report_Power_State;

   procedure Power_Off (Planner : Planner_Interface'Class) is
   begin
      Planner.Flush (Power_State_Change_Event'(Turn_On => False));
   end Power_Off;

end Prunt.Default_Modules.Power_Control;
