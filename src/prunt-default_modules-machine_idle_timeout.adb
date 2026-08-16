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

with Ada.Real_Time;
with Ada.Strings;
with Ada.Strings.Fixed;

package body Prunt.Default_Modules.Machine_Idle_Timeout is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class is
      pragma Unreferenced (This);
   begin
      return Config.Versioned_Config_Schema'(Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

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
   procedure Process_After_Block (This : Inactivity_Shutdown_Update; Context : Block_End_Context'Class) is
      pragma Unreferenced (Context);

      Instance : Module_Instance renames Module_Instance (This.Module_Instance_Ref.Get.Element.all);
      Watchdog : Inactivity_Watchdog_Wrapper_Pointers.Ref;
   begin
      Instance.Apply_Runtime_Timeout (This.Timeout);
      Watchdog := Instance.Get_Watchdog;
      Watchdog.Get.Watchdog.Set_Timeout (This.Timeout);
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Inactivity_Shutdown_Report_Event; Context : Block_End_Context'Class) is
   begin
      Context.Wait_For_Idle;
      Context.Log
        (+("Inactivity timeout "
           & Ada.Strings.Fixed.Trim
               (Module_Instance (This.Module_Instance_Ref.Get.Element.all).Get_Timeout'Image, Ada.Strings.Both)
           & " s."));
   end Process_After_Block;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path'Class; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Report_Config_Error, Status_Emitter);

      Parsed_Config                     : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Config_Saving_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Config_Saving_Module.Module_Instance'Tag);
      Config_Saver                      : Config_Saving_Module.Config_Saver'Class renames
        Config_Saving_Module.Config_Saver'Class (Config_Saving_Module_Instance_Ref.Get.Element.all);
      Idle_Emitter_Instance             : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Idle_Emitter_Module.Module_Instance'Tag);
   begin
      return Result : Module_Instance do
         Config_Saver.Register_For_Saving (Config_Data);
         Result.Initialize (Parsed_Config, Config_Data, Idle_Emitter_Instance);
      end return;
   end Initialize;

   task body Inactivity_Watchdog is
      use type Ada.Real_Time.Time;

      Current_Timeout  : Duration := 0.0;
      Deadline         : Ada.Real_Time.Time := Ada.Real_Time.Time_Last;
      Machine_Was_Idle : Boolean := False;
      Stop_Received    : Boolean := False;

      procedure Reset_Deadline;

      procedure Reset_Deadline is
      begin
         if Current_Timeout > 0.0 and then Machine_Was_Idle then
            Deadline := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Current_Timeout);
         else
            Deadline := Ada.Real_Time.Time_Last;
         end if;
      end Reset_Deadline;
   begin
      select
         accept Stop;
         Stop_Received := True;
      or
         accept Start;
         Reset_Deadline;
      end select;

      while not Stop_Received loop
         select
            accept Stop;
            Stop_Received := True;
         or
            accept Set_Timeout (Value : Duration) do
               Current_Timeout := Value;
               Reset_Deadline;
            end Set_Timeout;
         or
            accept Idle_Start;
            Machine_Was_Idle := True;
            Reset_Deadline;
         or
            accept Idle_End;
            Machine_Was_Idle := False;
            Reset_Deadline;
         or
            when Current_Timeout > 0.0 and then Machine_Was_Idle =>
            delay until Deadline;

            --  Disarm before reporting so a delayed controller shutdown cannot report the same timeout twice.
            Current_Timeout := 0.0;
            Deadline := Ada.Real_Time.Time_Last;
            Request_Shutdown ("M85 inactivity timeout expired.");
         end select;
      end loop;
   end Inactivity_Watchdog;

   overriding
   procedure Finalize (Object : in out Inactivity_Watchdog_Wrapper) is
   begin
      Object.Watchdog.Stop;
   end Finalize;

   protected body Module_Instance is
      procedure Initialize
        (Config_In       : User_Config;
         Config_Data_In  : Prunt.Config.Config_Data;
         Idle_Emitter_In : My_Modules.Module_Instance_Shared_Pointers.Ref)
      is
         function Make_Watchdog return Inactivity_Watchdog_Wrapper;

         function Make_Watchdog return Inactivity_Watchdog_Wrapper is
         begin
            return Result : Inactivity_Watchdog_Wrapper;
         end Make_Watchdog;
      begin
         Config := Config_In;
         Config_Data := Config_Data_In;
         Timeout := Duration (Config.Machine_Idle_Timeout.Timeout / s);
         Watchdog.Set (Make_Watchdog'Access);
         Idle_Emitter_Instance := Idle_Emitter_In.Weak;
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Planner);

         Idle_Emitter_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Self_Ref         : My_Modules.Module_Instance_Shared_Pointers.Ref;
      begin
         Watchdog.Get.Watchdog.Start;
         Watchdog.Get.Watchdog.Set_Timeout (Timeout);
         Idle_Emitter_Ref.Set (Idle_Emitter_Instance);
         Self_Ref.Set (Self_Ref_In);
         Idle_Emitter_Module.Idle_Notification_Emitter'Class (Idle_Emitter_Ref.Get.Element.all)
           .Request_Idle_Notifications (Self_Ref);
      end Start;

      procedure Apply_Runtime_Timeout (Value : Duration) is
      begin
         Timeout := Value;
         Config.Machine_Idle_Timeout.Timeout := Dimensionless (Value) * s;
         User_Config_To_Config_Data (Config_Data, Config);
      end Apply_Runtime_Timeout;

      function Get_Timeout return Duration is
      begin
         return Timeout;
      end Get_Timeout;

      function Get_Watchdog return Inactivity_Watchdog_Wrapper_Pointers.Ref is
      begin
         return Watchdog;
      end Get_Watchdog;

      procedure Idle_Start is
      begin
         Watchdog.Get.Watchdog.Idle_Start;
      end Idle_Start;

      procedure Idle_End is
      begin
         Watchdog.Get.Watchdog.Idle_End;
      end Idle_End;
   end Module_Instance;

   procedure Set_Inactivity_Shutdown
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Gcode_Arguments.Argument_Integer)
   is
      pragma Unreferenced (This);
   begin
      Planner.Flush (Inactivity_Shutdown_Update'(Module_Instance_Ref => Self_Ref, Timeout => Duration (S)));
   end Set_Inactivity_Shutdown;

   procedure Report_Inactivity_Shutdown
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class) is
   begin
      pragma Unreferenced (This);
      Planner.Flush (Inactivity_Shutdown_Report_Event'(Module_Instance_Ref => Self_Ref));
   end Report_Inactivity_Shutdown;

end Prunt.Default_Modules.Machine_Idle_Timeout;
