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

package body Prunt.Default_Modules.Tachometers is

   pragma Extensions_Allowed (On);

   use type Ada.Real_Time.Time;

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   function Current_Speed
     (Config : User_Config; Tachometer : Tachometer_Name; Requires_Fresh : Boolean) return Dimensionless is
   begin
      return
        Dimensionless (Tachometer_Hardware (Tachometer).Get_Pulse_Frequency (Tachometer, Requires_Fresh) / hertz)
        * 60.0
        / Config.Tachometers (Tachometer).Pulses_Per_Revolution;
   end Current_Speed;

   procedure Log_Tachometers (Config : User_Config; Requires_Fresh : Boolean) is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Found_Enabled_Tachometer : Boolean := False;
   begin
      My_Logger.Log ("Tachometer speeds:");

      for T in Tachometer_Name loop
         if Config.Tachometers (T).Enabled then
            Found_Enabled_Tachometer := True;
            My_Logger.Log
              (+(T'Image
                 & ": "
                 & Trim (Dimensionless'Image (Current_Speed (Config, T, Requires_Fresh)), Both)
                 & " RPM"));
         end if;
      end loop;

      if not Found_Enabled_Tachometer then
         My_Logger.Log ("No tachometers are enabled.");
      end if;
   end Log_Tachometers;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class is
   begin
      return
        Config.Versioned_Config_Schema'
          (Version => 1, Module_Instance_Tag => Module_Instance'Tag, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map is
      pragma Unreferenced (This);
   begin
      return
        ["Speed" =>
           [for T in Tachometer_Name use+T'Image =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "RPM",
               Description => "Measured speed of tachometer " & (+T'Image),
               Condition   => "")]];
   end Status_Schema;

   overriding
   procedure Process_After_Block (This : Tachometer_Report_Event; Context : Block_End_Context'Class) is
      use Ada.Strings;
      use Ada.Strings.Fixed;

      Found_Enabled_Tachometer : Boolean := False;
   begin
      Context.Wait_For_Idle;
      Context.Log ("Tachometer speeds:");

      for T in Tachometer_Name loop
         if This.Config.Tachometers (T).Enabled then
            Found_Enabled_Tachometer := True;
            Context.Log
              (+(T'Image
                 & ": "
                 & Trim (Dimensionless'Image (Current_Speed (This.Config, T, Requires_Fresh => True)), Both)
                 & " RPM"));
         end if;
      end loop;

      if not Found_Enabled_Tachometer then
         Context.Log ("No tachometers are enabled.");
      end if;
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Tachometer_Auto_Report_Event; Context : Block_End_Context'Class) is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      Context.Wait_For_Idle;

      Module_Instance (This.Module_Instance_Ref.Get.Element.all).Set_Auto_Report_Interval (This.Interval);

      if This.Interval = 0.0 then
         Context.Log_If_Interactive ("Tachometer logging stopped.");
      else
         Context.Log_If_Interactive
           (+("Tachometer auto-reporting every "
              & Trim (This.Interval'Image, Both)
              & " seconds started; reports will appear in the log."));
      end if;
   end Process_After_Block;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Report_Config_Error, Get_Other_Instance);

      Parsed_Config : constant User_Config := Config_Data_To_User_Config (Config_Data);
   begin
      return Result : Module_Instance do
         Result.Initialize (Parsed_Config, Status_Emitter);
      end return;
   end Initialize;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   task body Status_Updater is
      Config_Ref                   : User_Config;
      Speed_Status_Setters_Ref     : Tachometer_Speed_Status_Setters;
      Stop_Received                : Boolean := False;
      Current_Auto_Report_Interval : Duration := 0.0;
      Next_Status_Report           : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
      Next_Auto_Report             : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
   begin
      select
         accept Stop;
         Stop_Received := True;
      or
         accept Start (Config : User_Config; Speed_Status_Setters_In : Tachometer_Speed_Status_Setters) do
            Config_Ref := Config;
            Speed_Status_Setters_Ref := Speed_Status_Setters_In;
            Next_Status_Report := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Status_Report_Period);
         end Start;
      end select;

      while not Stop_Received loop
         select
            accept Stop;
            Stop_Received := True;
         or
            accept Set_Auto_Report_Interval (Value : Duration) do
               Current_Auto_Report_Interval := Value;

               if Current_Auto_Report_Interval > 0.0 then
                  Next_Auto_Report := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Current_Auto_Report_Interval);
               end if;
            end Set_Auto_Report_Interval;
         or
            delay until Next_Status_Report;

            for T in Tachometer_Name loop
               if Config_Ref.Tachometers (T).Enabled then
                  Speed_Status_Setters_Ref (T).Set_Value (Current_Speed (Config_Ref, T, Requires_Fresh => False));
               else
                  Speed_Status_Setters_Ref (T).Set_Value (0.0);
               end if;
            end loop;

            Next_Status_Report := Next_Status_Report + Ada.Real_Time.To_Time_Span (Status_Report_Period);
            if Ada.Real_Time.Clock > Next_Status_Report then
               Next_Status_Report := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Status_Report_Period);
            end if;
         or
            when Current_Auto_Report_Interval > 0.0 =>
            delay until Next_Auto_Report;

            Log_Tachometers (Config_Ref, Requires_Fresh => False);

            Next_Auto_Report := Next_Auto_Report + Ada.Real_Time.To_Time_Span (Current_Auto_Report_Interval);
            if Ada.Real_Time.Clock > Next_Auto_Report then
               Next_Auto_Report := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (Current_Auto_Report_Interval);
            end if;
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

         for T in Tachometer_Name loop
            Speed_Status_Setters (T) := Status_Emitter_In.Get_Lock_Free_Setter ("Speed", +T'Image);
         end loop;

         Updater.Set (Make_Updater_Task'Access);
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Planner);
      begin
         Self_Ref := Self_Ref_In;
         Updater.Get.Updater.Start (Config, Speed_Status_Setters);
      end Start;

      procedure Set_Auto_Report_Interval (Value : Duration) is
      begin
         Updater.Get.Updater.Set_Auto_Report_Interval (Value);
      end Set_Auto_Report_Interval;

      function Get_Config return User_Config is
      begin
         return Config;
      end Get_Config;
   end Module_Instance;

   procedure Report_Tachometers (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      Planner.Flush (Tachometer_Report_Event'(Config => This.Get_Config));
   end Report_Tachometers;

   procedure Set_Tachometer_Auto_Report
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Dimensionless)
   is
      Interval : Duration;
   begin
      pragma Unreferenced (This);

      if S = 0.0 then
         Planner.Flush (Tachometer_Auto_Report_Event'(Module_Instance_Ref => Self_Ref, Interval => 0.0));
         return;
      end if;

      if S < 0.1 then
         raise Gcode_Bad_Inputs_Error with "Tachometer auto-report interval must be 0.1 or higher.";
      end if;

      if S > Dimensionless (Duration'Last) then
         raise Gcode_Bad_Inputs_Error
           with "Tachometer auto-report interval must not be greater than " & Duration'Last'Image;
      end if;

      begin
         Interval := Duration (S);
      exception
         when Constraint_Error =>
            raise Gcode_Bad_Inputs_Error with "Tachometer auto-report interval is out of range.";
      end;

      Planner.Flush (Tachometer_Auto_Report_Event'(Module_Instance_Ref => Self_Ref, Interval => Interval));
   end Set_Tachometer_Auto_Report;

end Prunt.Default_Modules.Tachometers;
