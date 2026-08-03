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

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Prunt.Default_Modules.Fans is

   pragma Extensions_Allowed (On);

   use type Gcode_Arguments.Argument_Integer;

   function Valid_Fan_Indices return Virtual_String is
   begin
      return Result : Virtual_String := "" do
         for F in Fan_Name loop
            if Result /= "" then
               Result.Append (", ");
            end if;

            Result.Append (+Trim (Fan_Hardware (F).Gcode_Index'Image, Both));
            Result.Append (" (");
            Result.Append (+F'Image);
            Result.Append (")");
         end loop;
      end return;
   end Valid_Fan_Indices;

   function Valid_Fan_Names return Virtual_String is
   begin
      return Result : Virtual_String := "" do
         for F in Fan_Name loop
            if Result /= "" then
               Result.Append (", ");
            end if;

            Result.Append (+F'Image);
         end loop;
      end return;
   end Valid_Fan_Names;

   function PWM_Frequency (Config : User_Config_Fan) return Frequency is
   begin
      case Config.Fixed_Kind is
         when Fixed_Switching_Kind            =>
            return Config.Fixed_Switching.PWM_Frequency;

         when Low_Or_High_Side_Switching_Kind =>
            case Config.Low_Or_High_Side_Switching.Kind is
               when Low_Side_Switching  =>
                  return Config.Low_Or_High_Side_Switching.Low_Side_Switching.PWM_Frequency;

               when High_Side_Switching =>
                  return Config.Low_Or_High_Side_Switching.High_Side_Switching.PWM_Frequency;
            end case;
      end case;
   end PWM_Frequency;

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema is
   begin
      return (Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

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

      Parsed_Config : constant User_Config := Config_Data_To_User_Config (Config_Data);
   begin
      return Result : Module_Instance do
         Result.Initialize (Parsed_Config, Status_Emitter);
      end return;
   end Initialize;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map is
      pragma Unreferenced (This);
   begin
      return
        ["Speed" =>
           [for F in Fan_Name use+F'Image =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "×",
               Description => "Requested speed of fan " & (+F'Image),
               Condition   => "")]];
   end Status_Schema;

   overriding
   procedure Process (This : Fan_Speed_Change; Last_Command_Index : Command_Index) is
      pragma Unreferenced (Last_Command_Index);
   begin
      Fan_Hardware (This.Fan).Set_Duty_Cycle
        (This.Fan, (if This.Invert then 1.0 - This.Duty_Cycle else This.Duty_Cycle));
      This.Speed_Status.Set_Value (This.Duty_Cycle);
   end Process;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config; Status_Emitter_In : Status_Manager.Status_Emitter) is
      begin
         Config := Config_In;

         for F in Fan_Name loop
            Speed_Status_Setters (F) := Status_Emitter_In.Get_Lock_Free_Setter ("Speed", +F'Image);
         end loop;
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Planner);
      begin
         Self_Ref := Self_Ref_In;

         for F in Fan_Name loop
            case Fan_Hardware (F).Kind is
               when Fixed_Switching_Kind            =>
                  Fan_Hardware (F).Reconfigure_Fixed_Switching_Fan (F, PWM_Frequency (Config.Fans (F)));

               when Low_Or_High_Side_Switching_Kind =>
                  Fan_Hardware (F).Reconfigure_Low_Or_High_Side_Switching_Fan
                    (F,
                     PWM_Frequency (Config.Fans (F)),
                     Config.Fans (F).Low_Or_High_Side_Switching.Kind = High_Side_Switching);
            end case;

            if Config.Fans (F).Control_Method.Kind = Always_On then
               declare
                  Duty_Cycle : constant PWM_Scale := Config.Fans (F).Control_Method.Always_On.Duty_Cycle;
               begin
                  Fan_Hardware (F).Set_Duty_Cycle
                    (F, (if Config.Fans (F).Invert_PWM_Output then 1.0 - Duty_Cycle else Duty_Cycle));
                  Speed_Status_Setters (F).Set_Value (Duty_Cycle);
               end;
            else
               Fan_Hardware (F).Set_Duty_Cycle (F, (if Config.Fans (F).Invert_PWM_Output then 1.0 else 0.0));
               Speed_Status_Setters (F).Set_Value (0.0);
            end if;
         end loop;
      end Start;

      function Prepare_Fan_Speed_Change (Fan : Fan_Name; Speed : Dimensionless) return Fan_Speed_Change is
      begin
         if Config.Fans (Fan).Control_Method.Kind /= Dynamic_Duty_Cycle then
            raise Gcode_Bad_Inputs_Error
              with "Fan " & Fan'Image & " is not configured for dynamic duty cycle operation.";
         end if;

         if Speed < 0.0 then
            raise Gcode_Bad_Inputs_Error with "Speed must not be less than 0.";
         end if;

         if Speed > 255.0 then
            raise Gcode_Bad_Inputs_Error with "Speed must not be greater than 255.";
         end if;

         declare
            Duty_Cycle : PWM_Scale :=
              Speed / 255.0 * Config.Fans (Fan).Control_Method.Dynamic_Duty_Cycle.Maximum_Duty_Cycle;
         begin
            if Duty_Cycle < Config.Fans (Fan).Control_Method.Dynamic_Duty_Cycle.Disable_Below then
               Duty_Cycle := 0.0;
            end if;

            return
              Fan_Speed_Change'
                (Fan          => Fan,
                 Invert       => Config.Fans (Fan).Invert_PWM_Output,
                 Duty_Cycle   => Duty_Cycle,
                 Speed_Status => Speed_Status_Setters (Fan));
         end;
      end Prepare_Fan_Speed_Change;

      function Default_Fan return Fan_Name is
      begin
         return Config.Gcode_Defaults.Default_Fan;
      end Default_Fan;
   end Module_Instance;

   procedure Set_Fan_Speed_For_Default_Fan
     (This : Module_Instance; Planner : Planner_Interface'Class; S : Dimensionless := 255.0) is
   begin
      Planner.Add_Corner_Data (This.Prepare_Fan_Speed_Change (This.Default_Fan, S));
   end Set_Fan_Speed_For_Default_Fan;

   procedure Set_Fan_Speed
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      P       : Gcode_Arguments.Argument_Integer;
      S       : Dimensionless := 255.0) is
   begin
      for F in Fan_Name when Fan_Hardware (F).Gcode_Index = P loop
         --  There is a predicate on the Fan_Hardware type to avoid duplicate indices. A vendor could bypass this but
         --  it's not our problem at that point.
         Planner.Add_Corner_Data (This.Prepare_Fan_Speed_Change (F, S));
         return;
      end loop;

      My_Logger.Log ("Valid fan indices: " & Valid_Fan_Indices);
      raise Gcode_Bad_Inputs_Error with "Fan index not known. Refer to valid fan indices in log.";
   end Set_Fan_Speed;

   procedure Set_Fan_Speed
     (This : Module_Instance; Planner : Planner_Interface'Class; P : Virtual_String; S : Dimensionless := 255.0)
   is
      Fan : Fan_Name;
   begin
      begin
         Fan := Fan_Name'Value (Conversions.To_UTF_8_String (P));
      exception
         when Constraint_Error =>
            My_Logger.Log ("Valid fan names: " & Valid_Fan_Names);
            raise Gcode_Bad_Inputs_Error with "Fan name not known. Refer to valid fan names in log.";
      end;

      Planner.Add_Corner_Data (This.Prepare_Fan_Speed_Change (Fan, S));
   end Set_Fan_Speed;

   procedure Turn_Off_Default_Fan (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      Planner.Add_Corner_Data (This.Prepare_Fan_Speed_Change (This.Default_Fan, 0.0));
   end Turn_Off_Default_Fan;

   procedure Turn_Off_Fan
     (This : Module_Instance; Planner : Planner_Interface'Class; P : Gcode_Arguments.Argument_Integer) is
   begin
      for F in Fan_Name when Fan_Hardware (F).Gcode_Index = P loop
         --  There is a predicate on the Fan_Hardware type to avoid duplicate indices. A vendor could bypass this but
         --  it's not our problem at that point.
         Planner.Add_Corner_Data (This.Prepare_Fan_Speed_Change (F, 0.0));
         return;
      end loop;

      My_Logger.Log ("Valid fan indices: " & Valid_Fan_Indices);
      raise Gcode_Bad_Inputs_Error with "Fan index not known. Refer to valid fan indices in log.";
   end Turn_Off_Fan;

   procedure Turn_Off_Fan (This : Module_Instance; Planner : Planner_Interface'Class; P : Virtual_String) is
      Fan : Fan_Name;
   begin
      begin
         Fan := Fan_Name'Value (Conversions.To_UTF_8_String (P));
      exception
         when Constraint_Error =>
            My_Logger.Log ("Valid fan names: " & Valid_Fan_Names);
            raise Gcode_Bad_Inputs_Error with "Fan name not known. Refer to valid fan names in log.";
      end;

      Planner.Add_Corner_Data (This.Prepare_Fan_Speed_Change (Fan, 0.0));
   end Turn_Off_Fan;

end Prunt.Default_Modules.Fans;
