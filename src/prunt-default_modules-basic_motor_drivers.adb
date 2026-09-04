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

package body Prunt.Default_Modules.Basic_Motor_Drivers is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class is
      pragma Unreferenced (This);
   begin
      return
        Config.Versioned_Config_Schema'
          (Version => 1, Module_Instance_Tag => Module_Instance'Tag, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is
      pragma Unreferenced (This);
   begin
      return [];
   end Gcode_Commands;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Report_Config_Error, Status_Emitter);
   begin
      return Result : Module_Instance do
         Result.Initialize
           (Config_In                         => Config_Data_To_User_Config (Config_Data),
            Motor_Drivers_Module_Instance_Ref => Get_Other_Instance (Motor_Drivers_Module.Module_Instance'Tag));
      end return;
   end Initialize;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is
   begin
      pragma Unreferenced (This, Self_Ref, Args, Planner, Command_Identifier);
      raise Constraint_Error with "Basic Motor Drivers does not define any G-code commands.";
   end Gcode_Dispatch;

   overriding
   procedure Enable_Motor (This : in out Basic_Motor_Handler) is
   begin
      Motor_Hardware (This.Motor).Enable (This.Motor);
   end Enable_Motor;

   overriding
   procedure Disable_Motor (This : in out Basic_Motor_Handler) is
   begin
      Motor_Hardware (This.Motor).Disable (This.Motor);
   end Disable_Motor;

   protected body Module_Instance is
      procedure Initialize
        (Config_In : User_Config; Motor_Drivers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref)
      is
         Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance_Interface'Class renames
           Motor_Drivers_Module.Module_Instance_Interface'Class (Motor_Drivers_Module_Instance_Ref.Get.Element.all);
      begin
         for M in Motor_Name loop
            case Config_In.Motors (M).Fixed_Kind is
               when Basic_Motor_Kind  =>
                  Motor_Drivers_Module_Instance.Provide_Motor_Configuration
                    (M,
                     (Microsteps => Config_In.Motors (M).Basic_Parameters.Microsteps),
                     Basic_Motor_Handler'(Motor_Drivers_Module.Motor_Handler with Motor => M));

               when TMC2240_UART_Kind =>
                  null;
            end case;
         end loop;
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Self_Ref_In, Planner);
      begin
         null;
      end Start;
   end Module_Instance;

end Prunt.Default_Modules.Basic_Motor_Drivers;
