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

package body Prunt.Default_Modules.Thermistors is

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

   function To_Thermistor_Parameters
     (Config : User_Config_Thermistor) return Prunt.Thermistors.Thermistor_Parameters
   is
   begin
      case Config.Sensor_Model.Kind is
         when Disabled =>
            return
              (Kind                => Prunt.Thermistors.Disabled_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature);

         when ATC_Semitec_104GT_2 =>
            return
              (Kind                => Prunt.Thermistors.Steinhart_Hart_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               SH_A                => 8.0965E-4,
               SH_B                => 2.1163E-4,
               SH_C                => 7.0742E-8);

         when ATC_Semitec_104NT_4_R025H42G =>
            return
              (Kind                => Prunt.Thermistors.Steinhart_Hart_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               SH_A                => 7.9582E-4,
               SH_B                => 2.1360E-4,
               SH_C                => 6.4830E-8);

         when EPCOS_100K_B57560G104F =>
            return
              (Kind                => Prunt.Thermistors.Steinhart_Hart_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               SH_A                => 7.2213E-4,
               SH_B                => 2.1676E-4,
               SH_C                => 8.9293E-8);

         when Generic_3950 =>
            return
              (Kind                => Prunt.Thermistors.Steinhart_Hart_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               SH_A                => 7.9347E-4,
               SH_B                => 2.0076E-4,
               SH_C                => 1.6328E-7);

         when Slice_Engineering_450 =>
            return
              (Kind                => Prunt.Thermistors.Steinhart_Hart_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               SH_A                => 3.0553E-4,
               SH_B                => 2.1171E-4,
               SH_C                => 1.1962E-7);

         when TDK_NTCG104LH104JT1 =>
            return
              (Kind                => Prunt.Thermistors.Steinhart_Hart_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               SH_A                => 9.7639E-4,
               SH_B                => 1.9688E-4,
               SH_C                => 7.2671E-8);

         when Honeywell_100K_135_104LAG_J01 =>
            return
              (Kind                => Prunt.Thermistors.Steinhart_Hart_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               SH_A                => 4.5695E-4,
               SH_B                => 2.5163E-4,
               SH_C                => 0.0);

         when NTC_100K_MGB18_104F39050L32 =>
            return
              (Kind                => Prunt.Thermistors.Steinhart_Hart_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               SH_A                => 5.4598E-4,
               SH_B                => 2.4390E-4,
               SH_C                => 0.0);

         when PT_1000_PT_385 =>
            return
              (Kind                => Prunt.Thermistors.Callendar_Van_Dusen_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               CVD_R0              => 1000.0 * ohm,
               CVD_A               => 3.9083E-3,
               CVD_B               => -5.775E-7);

         when PT_1000_PT_392 =>
            return
              (Kind                => Prunt.Thermistors.Callendar_Van_Dusen_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               CVD_R0              => 1000.0 * ohm,
               CVD_A               => 3.9827E-3,
               CVD_B               => -5.875E-7);

         when Custom_Steinhart_Hart =>
            return
              (Kind                => Prunt.Thermistors.Steinhart_Hart_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               SH_A                => Config.Sensor_Model.Custom_Steinhart_Hart.A,
               SH_B                => Config.Sensor_Model.Custom_Steinhart_Hart.B,
               SH_C                => Config.Sensor_Model.Custom_Steinhart_Hart.C);

         when Custom_Callendar_Van_Dusen =>
            return
              (Kind                => Prunt.Thermistors.Callendar_Van_Dusen_Kind,
               Minimum_Temperature => Config.Minimum_Temperature,
               Maximum_Temperature => Config.Maximum_Temperature,
               CVD_R0              => Config.Sensor_Model.Custom_Callendar_Van_Dusen.R0,
               CVD_A               => Config.Sensor_Model.Custom_Callendar_Van_Dusen.A,
               CVD_B               => Config.Sensor_Model.Custom_Callendar_Van_Dusen.B);
      end case;
   end To_Thermistor_Parameters;

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
      pragma Unreferenced (This, Status_Emitter, Get_Other_Instance);

      Parsed_Config : constant User_Config := Config_Data_To_User_Config (Config_Data);
   begin
      return Result : Module_Instance do
         Result.Initialize (Parsed_Config);

         for T in Thermistor_Name loop
            if Parsed_Config.Thermistors (T).Sensor_Model.Kind /= Disabled
              and then Parsed_Config.Thermistors (T).Minimum_Temperature >= Parsed_Config.Thermistors (T).Maximum_Temperature
            then
               Report_Config_Error
                 (["Thermistors", +T'Image, "Maximum_Temperature"],
                  "Maximum temperature must be greater than minimum temperature.");
            end if;
         end loop;
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

         for T in Thermistor_Name loop
            Thermistor_Hardware (T).Reconfigure (T, To_Thermistor_Parameters (Config.Thermistors (T)));
         end loop;
      end Start;

      procedure Report_Temperatures
        (Planner : Planner_Interface'Class;
         R       : Gcode_Optional_No_Value;
         T       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, R, T);
         My_Logger.Log ("M105 reporting is not implemented yet.");
      end Report_Temperatures;

      procedure Set_Temperature_Auto_Report
        (Planner : Planner_Interface'Class;
         S       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, S);
         My_Logger.Log ("M155 auto-reporting is not implemented yet.");
      end Set_Temperature_Auto_Report;

      function Thermistor_Is_Enabled_In_Config (Thermistor : Thermistor_Name) return Boolean is
      begin
         return Config.Thermistors (Thermistor).Sensor_Model.Kind /= Disabled;
      end Thermistor_Is_Enabled_In_Config;

      function Get_Thermistor_Parameters
        (Thermistor : Thermistor_Name) return Prunt.Thermistors.Thermistor_Parameters
      is
      begin
         return To_Thermistor_Parameters (Config.Thermistors (Thermistor));
      end Get_Thermistor_Parameters;

      function Get_Temperature (Thermistor : Thermistor_Name; Requires_Fresh : Boolean) return Temperature is
      begin
         return Thermistor_Hardware (Thermistor).Get_Temperature (Thermistor, Requires_Fresh);
      end Get_Temperature;
   end Module_Instance;

end Prunt.Default_Modules.Thermistors;
