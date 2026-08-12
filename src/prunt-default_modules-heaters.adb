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
with Prunt.Thermistors;

package body Prunt.Default_Modules.Heaters is

   pragma Extensions_Allowed (On);

   use type Ada.Real_Time.Time;
   use type Ada.Real_Time.Time_Span;

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

   function Pause_Action_Changes_Target (Action : User_Config_Heater_Pause_Action) return Boolean
   is (Action.Kind in Set_Pause_Target);

   function Pause_Target_For_Heater (Action : User_Config_Heater_Pause_Action) return Temperature is
   begin
      case Action.Kind is
         when Keep_Target      =>
            raise Program_Error with "Pause action does not change target.";

         when Set_Pause_Target =>
            return Action.Target;
      end case;
   end Pause_Target_For_Heater;

   function To_Heater_Parameters (Config : User_Config_Heater) return Heater_Parameters is
   begin
      if Config.Kind = Disabled then
         return (Kind => Disabled_Kind);
      end if;

      case Config.Control_Method.Kind is
         when PID       =>
            return
              (Kind                       => PID_Kind,
               Safe_Below                 => Config.Safe_Below,
               Check_Max_Cumulative_Error => Config.Check_Maximum_Cumulative_Error,
               Check_Gain_Time            => Config.Check_Gain_Time,
               Check_Minimum_Gain         => Config.Check_Minimum_Gain,
               Check_Hysteresis           => Config.Check_Hysteresis,
               Proportional_Scale         => Config.Control_Method.PID.Proportional_Scale,
               Integral_Scale             => Config.Control_Method.PID.Integral_Scale,
               Derivative_Scale           => Config.Control_Method.PID.Derivative_Scale);

         when Bang_Bang =>
            return
              (Kind                       => Bang_Bang_Kind,
               Safe_Below                 => Config.Safe_Below,
               Check_Max_Cumulative_Error => Config.Check_Maximum_Cumulative_Error,
               Check_Gain_Time            => Config.Check_Gain_Time,
               Check_Minimum_Gain         => Config.Check_Minimum_Gain,
               Check_Hysteresis           => Config.Check_Hysteresis,
               Bang_Bang_Hysteresis       => Config.Control_Method.Bang_Bang.Hysteresis);
      end case;
   end To_Heater_Parameters;

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
        ["Target" =>
           [for H in Heater_Name use+H'Image =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "°C",
               Description => "Requested target temperature of heater " & (+H'Image),
               Condition   => "")]];
   end Status_Schema;

   overriding
   procedure Process (This : Heater_Target_Command; Last_Command_Index : Command_Index) is
      pragma Unreferenced (Last_Command_Index);
   begin
      Heater_Hardware (This.Heater).Set_Temperature (This.Heater, This.Target);
      This.Target_Status.Set_Value (This.Target / celsius);
      Module_Instance (This.Module_Instance_Ref.Get.Element.all).Record_Heater_Target (This.Heater, This.Target);
   end Process;

   overriding
   procedure Process_After_Block (This : Heater_Temperature_Wait; Context : Block_End_Context'Class) is
      pragma Unreferenced (Context);

      Thermistors_Module_Instance : Thermistors_Module.Module_Instance_Interface'Class renames
        Thermistors_Module.Module_Instance_Interface'Class (This.Thermistors_Module_Instance_Ref.Get.Element.all);
      Heaters_Module_Instance     : Module_Instance renames Module_Instance (This.Module_Instance_Ref.Get.Element.all);
      Ramp_Start_Temperature      : constant Temperature :=
        Thermistors_Module_Instance.Get_Temperature (This.Assigned_Thermistor, Requires_Fresh => True);

      procedure Set_Target (Value : Temperature);

      procedure Set_Target (Value : Temperature) is
      begin
         Heater_Hardware (This.Heater).Set_Temperature (This.Heater, Value);
         This.Target_Status.Set_Value (Value / celsius);
         Heaters_Module_Instance.Record_Heater_Target (This.Heater, Value);
      end Set_Target;
   begin

      if This.Ramp_Duration > 0.0 * s
        and then (not This.Ramp_Only_If_Heating or else Ramp_Start_Temperature < This.Target)
      then
         Heaters_Module_Instance.Set_Blocking_Tracker ("Waiting for heater temperature.");

         declare
            Ramp_Start  : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
            Ramp_Second : constant Duration := Duration (This.Ramp_Duration / s);
         begin
            loop
               declare
                  Ratio : constant Dimensionless :=
                    Dimensionless'Min
                      (1.0,
                       Dimensionless (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Ramp_Start) / Ramp_Second));
               begin
                  Set_Target (Ramp_Start_Temperature + (This.Target - Ramp_Start_Temperature) * Ratio);
                  exit when Ratio >= 1.0;
               end;

               delay Wait_Period;
            end loop;
         end;
      else
         Set_Target (This.Target);
      end if;

      Heaters_Module_Instance.Set_Blocking_Tracker ("Waiting for heater temperature.");

      loop
         if This.Wait_Only_If_Heating then
            exit when
              Thermistors_Module_Instance.Get_Temperature (This.Assigned_Thermistor, Requires_Fresh => True)
              >= This.Target;
         else
            exit when
              abs (Thermistors_Module_Instance.Get_Temperature (This.Assigned_Thermistor, Requires_Fresh => True)
                   - This.Target)
              <= This.Check_Hysteresis;
         end if;

         delay Wait_Period;
      end loop;

      Heaters_Module_Instance.Clear_Blocking_Tracker;
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
      pragma Unreferenced (This);

      Parsed_Config                        : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Thermistors_Module_Instance_Ref      : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Thermistors_Module.Module_Instance'Tag);
      Blocking_Tracker_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Blocking_Tracker_Module.Module_Instance'Tag);
   begin
      return Result : Module_Instance do
         declare
            Thermistors_Module_Instance : Thermistors_Module.Module_Instance_Interface'Class renames
              Thermistors_Module.Module_Instance_Interface'Class (Thermistors_Module_Instance_Ref.Get.Element.all);
         begin
            Result.Initialize
              (Parsed_Config, Status_Emitter, Thermistors_Module_Instance_Ref, Blocking_Tracker_Module_Instance_Ref);

            for H in Heater_Name loop
               if Parsed_Config.Heaters (H).Kind /= Disabled then
                  if not Thermistors_Module_Instance.Thermistor_Is_Enabled_In_Config
                           (Parsed_Config.Heaters (H).Thermistor)
                  then
                     Report_Config_Error (["Heaters", +H'Image, "Thermistor"], "This thermistor is disabled.");
                  elsif Parsed_Config.Heaters (H).Pause_Action.Kind = Set_Pause_Target then
                     declare
                        Thermistor_Params : constant Prunt.Thermistors.Thermistor_Parameters :=
                          Thermistors_Module_Instance.Get_Thermistor_Parameters (Parsed_Config.Heaters (H).Thermistor);
                        Target_Path       : constant Config.Config_Data_Paths.Vector :=
                          ["Heaters",
                           +H'Image,
                           "Kind",
                           "Children",
                           "Enabled",
                           "Pause_Action",
                           "Kind",
                           "Children",
                           "Set_Pause_Target",
                           "Target"];
                     begin
                        if Parsed_Config.Heaters (H).Pause_Action.Target < Thermistor_Params.Minimum_Temperature then
                           Report_Config_Error (Target_Path, "This target is below the thermistor minimum.");
                        end if;

                        if Parsed_Config.Heaters (H).Pause_Action.Target > Thermistor_Params.Maximum_Temperature then
                           Report_Config_Error (Target_Path, "This target is above the thermistor maximum.");
                        end if;
                     end;
                  end if;
               end if;
            end loop;

            declare
               procedure Validate_Default (Name : Virtual_String; Selection : User_Config_Default_Heater);

               procedure Check_Overlap
                 (Left_Name       : Virtual_String;
                  Left_Selection  : User_Config_Default_Heater;
                  Right_Name      : Virtual_String;
                  Right_Selection : User_Config_Default_Heater);

               procedure Validate_Default (Name : Virtual_String; Selection : User_Config_Default_Heater) is
               begin
                  if Selection.Kind = Enabled and then Parsed_Config.Heaters (Selection.Heater).Kind = Disabled then
                     Report_Config_Error (["Gcode_Defaults", Name, "Heater"], "This heater is disabled.");
                  end if;
               end Validate_Default;

               procedure Check_Overlap
                 (Left_Name       : Virtual_String;
                  Left_Selection  : User_Config_Default_Heater;
                  Right_Name      : Virtual_String;
                  Right_Selection : User_Config_Default_Heater) is
               begin
                  if Left_Selection.Kind = Enabled
                    and then Right_Selection.Kind = Enabled
                    and then Left_Selection.Heater = Right_Selection.Heater
                  then
                     Report_Config_Error
                       (["Gcode_Defaults", Right_Name, "Heater"],
                        "This heater is already selected for " & Left_Name & ".");
                  end if;
               end Check_Overlap;
            begin
               Validate_Default ("Hotend", Parsed_Config.Gcode_Defaults.Hotend);
               Validate_Default ("Bed", Parsed_Config.Gcode_Defaults.Bed);
               Validate_Default ("Chamber", Parsed_Config.Gcode_Defaults.Chamber);

               Check_Overlap ("Hotend", Parsed_Config.Gcode_Defaults.Hotend, "Bed", Parsed_Config.Gcode_Defaults.Bed);
               Check_Overlap
                 ("Hotend", Parsed_Config.Gcode_Defaults.Hotend, "Chamber", Parsed_Config.Gcode_Defaults.Chamber);
               Check_Overlap
                 ("Bed", Parsed_Config.Gcode_Defaults.Bed, "Chamber", Parsed_Config.Gcode_Defaults.Chamber);
            end;
         end;
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize
        (Config_In                           : User_Config;
         Status_Emitter_In                   : Status_Manager.Status_Emitter;
         Thermistors_Module_Instance_In      : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Blocking_Tracker_Module_Instance_In : My_Modules.Module_Instance_Shared_Pointers.Ref) is
      begin
         Config := Config_In;

         Thermistors_Module_Instance_Ref := Thermistors_Module_Instance_In;
         Blocking_Tracker_Module_Instance_Ref := Blocking_Tracker_Module_Instance_In;

         for H in Heater_Name loop
            Target_Status_Setters (H) := Status_Emitter_In.Get_Lock_Free_Setter ("Target", +H'Image);
         end loop;
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Planner);
      begin
         Self_Ref := Self_Ref_In;

         for H in Heater_Name loop
            declare
               Min_Temp : constant Temperature :=
                 (if Config.Heaters (H).Kind = Disabled
                  then 0.0 * celsius
                  else
                    Thermistors_Module.Module_Instance_Interface'Class
                      (Thermistors_Module_Instance_Ref.Get.Element.all)
                      .Get_Thermistor_Parameters (Config.Heaters (H).Thermistor)
                      .Minimum_Temperature);
            begin
               Heater_Hardware (H).Reconfigure
                 (H,
                  To_Heater_Parameters (Config.Heaters (H)),
                  (if Config.Heaters (H).Kind = Disabled
                   then Thermistor_Name'First
                   else Config.Heaters (H).Thermistor));
               Heater_Hardware (H).Set_Temperature (H, Min_Temp);
               Target_Status_Setters (H).Set_Value (Min_Temp / celsius);
               Current_Targets (H) := Min_Temp;
            end;
         end loop;
      end Start;

      procedure Handle_Pause (Planner : Planner_Interface'Class; Context : Pause_Context'Class) is
         pragma Unreferenced (Context);
      begin
         Save_Pause_Targets;

         for H in Heater_Name loop
            if Config.Heaters (H).Kind = Enabled and then Pause_Action_Changes_Target (Config.Heaters (H).Pause_Action)
            then
               Planner.Add_Corner_Data
                 (Build_Target_Command (H, Pause_Target_For_Heater (Config.Heaters (H).Pause_Action)));
            end if;
         end loop;
      end Handle_Pause;

      procedure Handle_Resume (Planner : Planner_Interface'Class; Context : Pause_Context'Class) is
         pragma Unreferenced (Context);

         Saved_Pause_Targets : constant Heater_Target_Array := Get_Pause_Targets;
      begin
         for H in Heater_Name loop
            if Config.Heaters (H).Kind = Enabled and then Pause_Action_Changes_Target (Config.Heaters (H).Pause_Action)
            then
               Planner.Add_Corner_Data (Build_Target_Command (H, Saved_Pause_Targets (H)));
            end if;
         end loop;

         for H in Heater_Name loop
            if Config.Heaters (H).Kind = Enabled and then Pause_Action_Changes_Target (Config.Heaters (H).Pause_Action)
            then
               Planner.Flush
                 (Build_Temperature_Wait
                    (Heater               => H,
                     Target               => Saved_Pause_Targets (H),
                     Wait_Only_If_Heating => True,
                     Ramp_Duration        => 0.0 * Prunt.s,
                     Ramp_Only_If_Heating => True));
            end if;
         end loop;

         Clear_Pause_Targets;
      end Handle_Resume;

      function Build_Target_Command (Heater : Heater_Name; Target : Temperature) return Heater_Target_Command is
         Self_Ref_Strong : My_Modules.Module_Instance_Shared_Pointers.Ref;

         Junk : Dummy_Type := Validate_Target (Heater, Target);
      begin
         Self_Ref_Strong.Set (Self_Ref);
         return
           Heater_Target_Command'
             (Module_Instance_Ref => Self_Ref_Strong,
              Heater              => Heater,
              Target_Status       => Target_Status_Setters (Heater),
              Target              => Target);
      end Build_Target_Command;

      procedure Record_Heater_Target (Heater : Heater_Name; Target : Temperature) is
      begin
         Current_Targets (Heater) := Target;
      end Record_Heater_Target;

      procedure Save_Pause_Targets is
      begin
         Pause_Targets := Current_Targets;
         Pause_Targets_Valid := True;
      end Save_Pause_Targets;

      function Get_Pause_Targets return Heater_Target_Array is
      begin
         if not Pause_Targets_Valid then
            raise Constraint_Error with "Heater pause targets were not saved before resume.";
         end if;

         return Pause_Targets;
      end Get_Pause_Targets;

      procedure Clear_Pause_Targets is
      begin
         Pause_Targets_Valid := False;
      end Clear_Pause_Targets;

      function Build_Temperature_Wait
        (Heater               : Heater_Name;
         Target               : Temperature;
         Wait_Only_If_Heating : Boolean;
         Ramp_Duration        : Time;
         Ramp_Only_If_Heating : Boolean) return Heater_Temperature_Wait
      is
         Self_Ref_Strong : My_Modules.Module_Instance_Shared_Pointers.Ref;

         Junk : Dummy_Type := Validate_Target (Heater, Target);
      begin
         Self_Ref_Strong.Set (Self_Ref);
         return
           Heater_Temperature_Wait'
             (Module_Instance_Ref             => Self_Ref_Strong,
              Heater                          => Heater,
              Target_Status                   => Target_Status_Setters (Heater),
              Thermistors_Module_Instance_Ref => Thermistors_Module_Instance_Ref,
              Assigned_Thermistor             => Config.Heaters (Heater).Thermistor,
              Target                          => Target,
              Check_Hysteresis                => Config.Heaters (Heater).Check_Hysteresis,
              Wait_Only_If_Heating            => Wait_Only_If_Heating,
              Ramp_Duration                   => Ramp_Duration,
              Ramp_Only_If_Heating            => Ramp_Only_If_Heating);
      end Build_Temperature_Wait;

      function Validate_Target (Heater : Heater_Name; Target : Temperature) return Dummy_Type is
         Thermistors_Module_Instance : Thermistors_Module.Module_Instance_Interface'Class renames
           Thermistors_Module.Module_Instance_Interface'Class (Thermistors_Module_Instance_Ref.Get.Element.all);
      begin
         if Config.Heaters (Heater).Kind = Disabled then
            raise Gcode_Bad_Inputs_Error with "This heater is disabled in config.";
         end if;

         declare
            Thermistor_Params : constant Prunt.Thermistors.Thermistor_Parameters :=
              Thermistors_Module_Instance.Get_Thermistor_Parameters (Config.Heaters (Heater).Thermistor);
         begin
            if Target < Thermistor_Params.Minimum_Temperature then
               raise Gcode_Bad_Inputs_Error
                 with
                   "Target temperature must not be less than "
                   & Dimensionless'Image (Thermistor_Params.Minimum_Temperature / celsius)
                   & " °C.";
            end if;

            if Target > Thermistor_Params.Maximum_Temperature then
               raise Gcode_Bad_Inputs_Error
                 with
                   "Target temperature must not be greater than "
                   & Dimensionless'Image (Thermistor_Params.Maximum_Temperature / celsius)
                   & " °C.";
            end if;
         end;

         return (null record);
      end Validate_Target;

      function Get_Default_Heater (Selection : User_Config_Default_Heater; Display_Name : String) return Heater_Name is
      begin
         if Selection.Kind = Disabled then
            raise Gcode_Bad_Inputs_Error with "The default " & Display_Name & " heater is disabled in config.";
         end if;

         return Selection.Heater;
      end Get_Default_Heater;

      procedure Set_Blocking_Tracker (Value : Virtual_String) is
         Blocking_Tracker_Instance : Blocking_Tracker_Module.Module_Instance_Interface'Class renames
           Blocking_Tracker_Module.Module_Instance_Interface'Class
             (Blocking_Tracker_Module_Instance_Ref.Get.Element.all);
      begin
         Blocking_Tracker_Instance.Set_Blocker (Value);
      end Set_Blocking_Tracker;

      procedure Clear_Blocking_Tracker is
         Blocking_Tracker_Instance : Blocking_Tracker_Module.Module_Instance_Interface'Class renames
           Blocking_Tracker_Module.Module_Instance_Interface'Class
             (Blocking_Tracker_Module_Instance_Ref.Get.Element.all);
      begin
         Blocking_Tracker_Instance.Clear_Blocker;
      end Clear_Blocking_Tracker;

      function Heater_Is_Enabled_In_Config (Heater : Heater_Name) return Boolean is
      begin
         return Config.Heaters (Heater).Kind /= Disabled;
      end Heater_Is_Enabled_In_Config;

      function Assigned_Thermistor (Heater : Heater_Name) return Thermistor_Name is
      begin
         if Config.Heaters (Heater).Kind = Disabled then
            raise Constraint_Error with "Disabled heaters do not have assigned thermistors.";
         end if;

         return Config.Heaters (Heater).Thermistor;
      end Assigned_Thermistor;

      function Get_Heater_Parameters (Heater : Heater_Name) return Heater_Parameters is
      begin
         return To_Heater_Parameters (Config.Heaters (Heater));
      end Get_Heater_Parameters;

      function Get_Config return User_Config is
      begin
         return Config;
      end Get_Config;
   end Module_Instance;

   procedure Wait_For_Hotend_Temperature_Heat
     (This : Module_Instance; Planner : Planner_Interface'Class; S : Dimensionless)
   is
      Config : constant User_Config := This.Get_Config;
   begin
      Planner.Flush
        (This.Build_Temperature_Wait
           (Heater               => This.Get_Default_Heater (Config.Gcode_Defaults.Hotend, "hotend"),
            Target               => S * celsius,
            Wait_Only_If_Heating => True,
            Ramp_Duration        => 0.0 * Prunt.s,
            Ramp_Only_If_Heating => True));
   end Wait_For_Hotend_Temperature_Heat;

   procedure Wait_For_Hotend_Temperature_Heat_Or_Cool
     (This : Module_Instance; Planner : Planner_Interface'Class; R : Dimensionless)
   is
      Config : constant User_Config := This.Get_Config;
   begin
      Planner.Flush
        (This.Build_Temperature_Wait
           (Heater               => This.Get_Default_Heater (Config.Gcode_Defaults.Hotend, "hotend"),
            Target               => R * celsius,
            Wait_Only_If_Heating => False,
            Ramp_Duration        => 0.0 * Prunt.s,
            Ramp_Only_If_Heating => True));
   end Wait_For_Hotend_Temperature_Heat_Or_Cool;

   procedure Set_Bed_Temperature (This : Module_Instance; Planner : Planner_Interface'Class; S : Dimensionless) is
      Config : constant User_Config := This.Get_Config;
   begin
      Planner.Add_Corner_Data
        (This.Build_Target_Command (This.Get_Default_Heater (Config.Gcode_Defaults.Bed, "bed"), S * celsius));
   end Set_Bed_Temperature;

   procedure Set_Chamber_Temperature (This : Module_Instance; Planner : Planner_Interface'Class; S : Dimensionless) is
      Config : constant User_Config := This.Get_Config;
   begin
      Planner.Add_Corner_Data
        (This.Build_Target_Command (This.Get_Default_Heater (Config.Gcode_Defaults.Chamber, "chamber"), S * celsius));
   end Set_Chamber_Temperature;

   procedure Wait_For_Bed_Temperature_Heat
     (This : Module_Instance; Planner : Planner_Interface'Class; S : Dimensionless; T : Dimensionless := 0.0)
   is
      Config : constant User_Config := This.Get_Config;
   begin
      if T < 0.0 then
         raise Gcode_Bad_Inputs_Error with "The T parameter must not be less than 0.";
      end if;

      Planner.Flush
        (This.Build_Temperature_Wait
           (Heater               => This.Get_Default_Heater (Config.Gcode_Defaults.Bed, "bed"),
            Target               => S * celsius,
            Wait_Only_If_Heating => True,
            Ramp_Duration        => T * Prunt.s,
            Ramp_Only_If_Heating => True));
   end Wait_For_Bed_Temperature_Heat;

   procedure Wait_For_Bed_Temperature_Heat_Or_Cool
     (This : Module_Instance; Planner : Planner_Interface'Class; R : Dimensionless; T : Dimensionless := 0.0)
   is
      Config : constant User_Config := This.Get_Config;
   begin
      if T < 0.0 then
         raise Gcode_Bad_Inputs_Error with "The T parameter must not be less than 0.";
      end if;

      Planner.Flush
        (This.Build_Temperature_Wait
           (Heater               => This.Get_Default_Heater (Config.Gcode_Defaults.Bed, "bed"),
            Target               => R * celsius,
            Wait_Only_If_Heating => False,
            Ramp_Duration        => T * Prunt.s,
            Ramp_Only_If_Heating => False));
   end Wait_For_Bed_Temperature_Heat_Or_Cool;

   procedure Wait_For_Chamber_Temperature_Heat
     (This : Module_Instance; Planner : Planner_Interface'Class; S : Dimensionless)
   is
      Config : constant User_Config := This.Get_Config;
   begin
      Planner.Flush
        (This.Build_Temperature_Wait
           (Heater               => This.Get_Default_Heater (Config.Gcode_Defaults.Chamber, "chamber"),
            Target               => S * celsius,
            Wait_Only_If_Heating => True,
            Ramp_Duration        => 0.0 * Prunt.s,
            Ramp_Only_If_Heating => False));
   end Wait_For_Chamber_Temperature_Heat;

   procedure Wait_For_Chamber_Temperature_Heat_Or_Cool
     (This : Module_Instance; Planner : Planner_Interface'Class; R : Dimensionless)
   is
      Config : constant User_Config := This.Get_Config;
   begin
      Planner.Flush
        (This.Build_Temperature_Wait
           (Heater               => This.Get_Default_Heater (Config.Gcode_Defaults.Chamber, "chamber"),
            Target               => R * celsius,
            Wait_Only_If_Heating => False,
            Ramp_Duration        => 0.0 * Prunt.s,
            Ramp_Only_If_Heating => False));
   end Wait_For_Chamber_Temperature_Heat_Or_Cool;

end Prunt.Default_Modules.Heaters;
