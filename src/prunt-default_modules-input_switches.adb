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

package body Prunt.Default_Modules.Input_Switches is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

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
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class is
   begin
      return Config.Versioned_Config_Schema'(Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map is
      pragma Unreferenced (This);
   begin
      return
        ["Triggered" =>
           [for S in Input_Switch_Name use+S'Image =>
              (Kind        => Status_Manager.Boolean_Kind,
               Unit        => "",
               Description => "True if input switch " & (+S'Image) & " is currently triggered.",
               Condition   => "")]];
   end Status_Schema;

   function Switch_Is_Triggered (Config : User_Config; Switch : Input_Switch_Name) return Boolean is
   begin
      return
        (if Config.Switches (Switch).Kind = Normally_Closed
         then not Input_Switch_Hardware (Switch).Get_State (Switch)
         else Input_Switch_Hardware (Switch).Get_State (Switch));
   end Switch_Is_Triggered;

   overriding
   procedure Process_After_Block (This : Endstop_Report_Event; Context : Block_End_Context'Class) is
      Found_Enabled_Switch : Boolean := False;
   begin
      Context.Wait_For_Idle;

      Context.Log ("Input switch status:");

      for Switch in Input_Switch_Name loop
         if This.Config.Switches (Switch).Enabled then
            Found_Enabled_Switch := True;
            Context.Log
              (+(Switch'Image
                 & ": "
                 & (if Switch_Is_Triggered (This.Config, Switch) then "Triggered" else "Not triggered")));
         end if;
      end loop;

      if not Found_Enabled_Switch then
         Context.Log ("No input switches are enabled.");
      end if;
   end Process_After_Block;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Report_Config_Error, Get_Other_Instance);
   begin
      return Result : Module_Instance do
         Result.Initialize (Config_Data_To_User_Config (Config_Data), Status_Emitter);
      end return;
   end Initialize;

   task body Status_Updater is
      Config_Ref    : User_Config;
      Status_Ref    : Status_Manager.Status_Emitter;
      Stop_Received : Boolean := False;
   begin
      select
         accept Stop;
         Stop_Received := True;
      or
         accept Start (Config : User_Config; Status_Emitter : Status_Manager.Status_Emitter) do
            Config_Ref := Config;
            Status_Ref := Status_Emitter;
         end Start;
      end select;

      while not Stop_Received loop
         select
            accept Stop;
            Stop_Received := True;
         or
            delay Status_Report_Period;

            for S in Input_Switch_Name loop
               Status_Ref.Set_Value ("Triggered", +S'Image, Switch_Is_Triggered (Config_Ref, S));
            end loop;
         end select;
      end loop;
   end Status_Updater;

   overriding
   procedure Finalize (Object : in out Status_Updater_Wrapper) is
   begin
      Object.Updater.Stop;
   end Finalize;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config; Status_Emitter_In : Status_Manager.Status_Emitter) is
         function Make_Updater_Task return Status_Updater_Wrapper;

         function Make_Updater_Task return Status_Updater_Wrapper is
         begin
            return Result : Status_Updater_Wrapper;
         end Make_Updater_Task;
      begin
         Config := Config_In;
         Status_Emitter := Status_Emitter_In;
         Updater.Set (Make_Updater_Task'Access);
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Self_Ref_In, Planner);
      begin
         Updater.Get.Updater.Start (Config, Status_Emitter);
      end Start;

      function Switch_Is_Enabled_In_Config (Switch : Input_Switch_Name) return Boolean is
      begin
         return Config.Switches (Switch).Enabled;
      end Switch_Is_Enabled_In_Config;

      function Switch_Is_Normally_Closed (Switch : Input_Switch_Name) return Boolean is
      begin
         return Config.Switches (Switch).Kind = Normally_Closed;
      end Switch_Is_Normally_Closed;

      function Get_Config return User_Config is
      begin
         return Config;
      end Get_Config;
   end Module_Instance;

   procedure Report_Endstop_States (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      Planner.Flush (Endstop_Report_Event'(Config => This.Get_Config));
   end Report_Endstop_States;

end Prunt.Default_Modules.Input_Switches;
