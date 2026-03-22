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

package body Prunt.Default_Modules.Heaters is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   function Thermistor_Path (Heater : Heater_Name) return Config.Config_Data_Paths.Vector
   is (["Heaters", +Heater'Image, "Thermistor"]);

   function To_Heater_Parameters (Config : User_Config_Heater) return Heater_Parameters is
   begin
      case Config.Control_Method.Kind is
         when Disabled  =>
            return
              (Kind                       => Disabled_Kind,
               Check_Max_Cumulative_Error => Config.Check_Maximum_Cumulative_Error,
               Check_Gain_Time            => Config.Check_Gain_Time,
               Check_Minimum_Gain         => Config.Check_Minimum_Gain,
               Check_Hysteresis           => Config.Check_Hysteresis);

         when PID       =>
            return
              (Kind                       => PID_Kind,
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
               Check_Max_Cumulative_Error => Config.Check_Maximum_Cumulative_Error,
               Check_Gain_Time            => Config.Check_Gain_Time,
               Check_Minimum_Gain         => Config.Check_Minimum_Gain,
               Check_Hysteresis           => Config.Check_Hysteresis,
               Bang_Bang_Hysteresis       => Config.Control_Method.Bang_Bang.Hysteresis);
      end case;
   end To_Heater_Parameters;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema is
   begin
      return (Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Status_Emitter);

      Parsed_Config                   : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Thermistors_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Thermistors_Module.Module_Instance'Tag);
   begin
      return Result : Module_Instance do
         declare
            Thermistors_Module_Instance : Thermistors_Module.Module_Instance_Interface'Class renames
              Thermistors_Module.Module_Instance_Interface'Class (Thermistors_Module_Instance_Ref.Get.Element.all);
         begin
            Result.Initialize (Parsed_Config);

            for H in Heater_Name loop
               if Parsed_Config.Heaters (H).Control_Method.Kind /= Disabled
                 and then
                   not Thermistors_Module_Instance.Thermistor_Is_Enabled_In_Config
                         (Parsed_Config.Heaters (H).Thermistor)
               then
                  Report_Config_Error (Thermistor_Path (H), "This thermistor is disabled in Thermistors.");
               end if;
            end loop;
         end;
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config) is
      begin
         Config := Config_In;
      end Initialize;

      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         Self_Ref := Self_Ref_In;

         for H in Heater_Name loop
            Heater_Hardware (H).Reconfigure
              (H, To_Heater_Parameters (Config.Heaters (H)), Config.Heaters (H).Thermistor);
         end loop;
      end Start;

      procedure Set_Idle_Timeout
        (Planner : Planner_Interface'Class;
         S       : Gcode_Optional_Integer;
         T       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         B       : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, S, T, E, B);
         raise Constraint_Error with "M86 is not implemented yet.";
      end Set_Idle_Timeout;

      procedure Disable_Idle_Timeout (Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Planner);
         raise Constraint_Error with "M87 is not implemented yet.";
      end Disable_Idle_Timeout;

      procedure Set_Hotend_Temperature
        (Planner : Planner_Interface'Class;
         I       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float;
         F       : Gcode_Optional_Float;
         B       : Gcode_Optional_Float;
         T       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, I, S, F, B, T);
         raise Constraint_Error with "M104 is not implemented yet.";
      end Set_Hotend_Temperature;

      procedure Wait_For_Hotend_Temperature
        (Planner : Planner_Interface'Class;
         I       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float;
         R       : Gcode_Optional_Float;
         F       : Gcode_Optional_Float;
         B       : Gcode_Optional_Float;
         T       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, I, S, R, F, B, T);
         raise Constraint_Error with "M109 is not implemented yet.";
      end Wait_For_Hotend_Temperature;

      procedure Set_Bed_Temperature
        (Planner : Planner_Interface'Class; I : Gcode_Optional_Integer; S : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, I, S);
         raise Constraint_Error with "M140 is not implemented yet.";
      end Set_Bed_Temperature;

      procedure Set_Chamber_Temperature (Planner : Planner_Interface'Class; S : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, S);
         raise Constraint_Error with "M141 is not implemented yet.";
      end Set_Chamber_Temperature;

      procedure Set_Laser_Cooler_Temperature (Planner : Planner_Interface'Class; S : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, S);
         raise Constraint_Error with "M143 is not implemented yet.";
      end Set_Laser_Cooler_Temperature;

      procedure Wait_For_Bed_Temperature
        (Planner : Planner_Interface'Class;
         I       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float;
         R       : Gcode_Optional_Float;
         T       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, I, S, R, T);
         raise Constraint_Error with "M190 is not implemented yet.";
      end Wait_For_Bed_Temperature;

      procedure Wait_For_Chamber_Temperature
        (Planner : Planner_Interface'Class; S : Gcode_Optional_Float; R : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, S, R);
         raise Constraint_Error with "M191 is not implemented yet.";
      end Wait_For_Chamber_Temperature;

      procedure Wait_For_Laser_Cooler_Temperature (Planner : Planner_Interface'Class; S : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, S);
         raise Constraint_Error with "M193 is not implemented yet.";
      end Wait_For_Laser_Cooler_Temperature;

      procedure Set_Hotend_PID
        (Planner : Planner_Interface'Class;
         E       : Gcode_Optional_Integer;
         P       : Gcode_Optional_Float;
         I       : Gcode_Optional_Float;
         D       : Gcode_Optional_Float;
         C       : Gcode_Optional_Float;
         L       : Gcode_Optional_Float;
         F       : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, E, P, I, D, C, L, F);
         raise Constraint_Error with "M301 is not implemented yet.";
      end Set_Hotend_PID;

      procedure Cold_Extrude_Settings
        (Planner : Planner_Interface'Class; S : Gcode_Optional_Float; P : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, S, P);
         raise Constraint_Error with "M302 is not implemented yet.";
      end Cold_Extrude_Settings;

      procedure PID_Autotune
        (Planner : Planner_Interface'Class;
         E       : Gcode_Optional_Float;
         C       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float;
         U       : Gcode_Optional_Integer;
         D       : Gcode_Optional_No_Value) is
      begin
         pragma Unreferenced (Planner, E, C, S, U, D);
         raise Constraint_Error with "M303 is not implemented yet.";
      end PID_Autotune;

      procedure Set_Bed_PID
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_Float;
         I       : Gcode_Optional_Float;
         D       : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, P, I, D);
         raise Constraint_Error with "M304 is not implemented yet.";
      end Set_Bed_PID;

      procedure Set_MPC_Values
        (Planner : Planner_Interface'Class;
         A       : Gcode_Optional_Float;
         C       : Gcode_Optional_Float;
         E       : Gcode_Optional_Integer;
         F       : Gcode_Optional_Float;
         H       : Gcode_Optional_Float;
         P       : Gcode_Optional_Float;
         R       : Gcode_Optional_Float;
         S       : Gcode_Optional_Integer;
         T       : Gcode_Optional_No_Value) is
      begin
         pragma Unreferenced (Planner, A, C, E, F, H, P, R, S, T);
         raise Constraint_Error with "M306 is not implemented yet.";
      end Set_MPC_Values;

      procedure Set_Chamber_PID
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_Float;
         I       : Gcode_Optional_Float;
         D       : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, P, I, D);
         raise Constraint_Error with "M309 is not implemented yet.";
      end Set_Chamber_PID;

      function Heater_Is_Enabled_In_Config (Heater : Heater_Name) return Boolean is
      begin
         return Config.Heaters (Heater).Control_Method.Kind /= Disabled;
      end Heater_Is_Enabled_In_Config;

      function Assigned_Thermistor (Heater : Heater_Name) return Thermistor_Name is
      begin
         return Config.Heaters (Heater).Thermistor;
      end Assigned_Thermistor;

      function Get_Heater_Parameters (Heater : Heater_Name) return Heater_Parameters is
      begin
         return To_Heater_Parameters (Config.Heaters (Heater));
      end Get_Heater_Parameters;
   end Module_Instance;

end Prunt.Default_Modules.Heaters;
