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
with Prunt.Thermistors;

generic
   with package My_Controller_Generic_Types is new Controller_Generic_Types (<>);
   Thermistor_Hardware : My_Controller_Generic_Types.Thermistor_Hardware_Parameters_Array_Type;
package Prunt.Default_Modules.Thermistors is

   use My_Controller_Generic_Types;

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;
   --  Return the configuration schema.

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;
   --  Return the supported G-code commands.

   type Module_Instance_Interface is synchronized interface;

   function Thermistor_Is_Enabled_In_Config
     (This : Module_Instance_Interface; Thermistor : Thermistor_Name) return Boolean
   is abstract;
   --  Return whether Thermistor is enabled.

   function Get_Thermistor_Parameters
     (This : Module_Instance_Interface; Thermistor : Thermistor_Name) return Prunt.Thermistors.Thermistor_Parameters
   is abstract;
   --  Return Thermistor's conversion parameters.

   function Get_Temperature
     (This : Module_Instance_Interface; Thermistor : Thermistor_Name; Requires_Fresh : Boolean) return Temperature
   is abstract;
   --  Return Thermistor's temperature.

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Module_Instance_Interface with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
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

   type User_Config_Thermistor_Disabled is record
      --  Disable this thermistor input.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_ATC_Semitec_104GT_2 is record
      --  Use the recommended Steinhart-Hart coefficients for the ATC Semitec 104GT-2 thermistor:
      --  A=8.0965E-4, B=2.1163E-4, C=7.0742E-8. Always verify temperature readings before using a heater.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_ATC_Semitec_104NT_4_R025H42G is record
      --  Use the recommended Steinhart-Hart coefficients for the ATC Semitec 104NT-4-R025H42G thermistor:
      --  A=7.9582E-4, B=2.1360E-4, C=6.4830E-8. Always verify temperature readings before using a heater.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_EPCOS_100K_B57560G104F is record
      --  Use the recommended Steinhart-Hart coefficients for the EPCOS 100K B57560G104F thermistor:
      --  A=7.2213E-4, B=2.1676E-4, C=8.9293E-8. Always verify temperature readings before using a heater.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_Generic_3950 is record
      --  Use the recommended Steinhart-Hart coefficients for a generic 100k thermistor with a B-value of 3950:
      --  A=7.9347E-4, B=2.0076E-4, C=1.6328E-7. Always verify temperature readings before using a heater.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_Slice_Engineering_450 is record
      --  Use the recommended Steinhart-Hart coefficients for the Slice Engineering 450 thermistor:
      --  A=3.0553E-4, B=2.1171E-4, C=1.1962E-7. Always verify temperature readings before using a heater.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_TDK_NTCG104LH104JT1 is record
      --  Use the recommended Steinhart-Hart coefficients for the TDK NTCG104LH104JT1 thermistor:
      --  A=9.7639E-4, B=1.9688E-4, C=7.2671E-8. Always verify temperature readings before using a heater.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_Honeywell_100K_135_104LAG_J01 is record
      --  Use the recommended Steinhart-Hart coefficients for the Honeywell 100K 135-104LAG-J01 thermistor:
      --  A=4.5695E-4, B=2.5163E-4, C=0.0. Always verify temperature readings before using a heater.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_NTC_100K_MGB18_104F39050L32 is record
      --  Use the recommended Steinhart-Hart coefficients for the NTC 100K MGB18-104F39050L32 thermistor:
      --  A=5.4598E-4, B=2.4390E-4, C=0.0. Always verify temperature readings before using a heater.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_PT_1000_PT_385 is record
      --  Use the recommended Callendar-Van Dusen coefficients for a PT-385 class PT1000 RTD above 0 C:
      --  R0=1000, A=3.9083E-3, B=-5.775E-7. Always verify temperature readings before use.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_PT_1000_PT_392 is record
      --  Use the recommended Callendar-Van Dusen coefficients for a PT-392 class PT1000 RTD above 0 C:
      --  R0=1000, A=3.9827E-3, B=-5.875E-7. Always verify temperature readings before use.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_Custom_Steinhart_Hart is record
      --  Enter custom Steinhart-Hart coefficients using the equation
      --  `1/T = A + B*ln(R) + C*(ln(R))^3`, where `T` is in Kelvin and `R` is in Ohms.

      A : Dimensionless range -1.0E100 .. 1.0E100 := 0.0;
      --  Steinhart-Hart A coefficient.

      B : Dimensionless range -1.0E100 .. 1.0E100 := 0.0;
      --  Steinhart-Hart B coefficient.

      C : Dimensionless range -1.0E100 .. 1.0E100 := 0.0;
      --  Steinhart-Hart C coefficient.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_Custom_Callendar_Van_Dusen is record
      --  Enter custom Callendar-Van Dusen coefficients using the equation
      --  `R(T) = R(0) * (1 + A*T + B*T^2)`, where `T` is in Celsius and `R(0)` is in Ohms.

      R0 : Resistance range 1.0E-100 * ohm .. 1.0E100 * ohm := 1000.0 * ohm;
      --  Resistance at 0 C.

      A : Dimensionless range -1.0E100 .. 1.0E100 := 0.0;
      --  Callendar-Van Dusen A coefficient.

      B : Dimensionless range -1.0E100 .. 1.0E100 := 0.0;
      --  Callendar-Van Dusen B coefficient.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_Model_Kind is
     (Disabled,
      ATC_Semitec_104GT_2,
      ATC_Semitec_104NT_4_R025H42G,
      EPCOS_100K_B57560G104F,
      Generic_3950,
      Slice_Engineering_450,
      TDK_NTCG104LH104JT1,
      Honeywell_100K_135_104LAG_J01,
      NTC_100K_MGB18_104F39050L32,
      PT_1000_PT_385,
      PT_1000_PT_392,
      Custom_Steinhart_Hart,
      Custom_Callendar_Van_Dusen)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_Model (Kind : User_Config_Thermistor_Model_Kind := Disabled) is record
      --  Select the thermistor or RTD model connected to this input. Choose one of the custom models if your sensor
      --  is not listed.

      case Kind is
         when Disabled =>
            Disabled : User_Config_Thermistor_Disabled;

         when ATC_Semitec_104GT_2 =>
            ATC_Semitec_104GT_2 : User_Config_Thermistor_ATC_Semitec_104GT_2;

         when ATC_Semitec_104NT_4_R025H42G =>
            ATC_Semitec_104NT_4_R025H42G : User_Config_Thermistor_ATC_Semitec_104NT_4_R025H42G;

         when EPCOS_100K_B57560G104F =>
            EPCOS_100K_B57560G104F : User_Config_Thermistor_EPCOS_100K_B57560G104F;

         when Generic_3950 =>
            Generic_3950 : User_Config_Thermistor_Generic_3950;

         when Slice_Engineering_450 =>
            Slice_Engineering_450 : User_Config_Thermistor_Slice_Engineering_450;

         when TDK_NTCG104LH104JT1 =>
            TDK_NTCG104LH104JT1 : User_Config_Thermistor_TDK_NTCG104LH104JT1;

         when Honeywell_100K_135_104LAG_J01 =>
            Honeywell_100K_135_104LAG_J01 : User_Config_Thermistor_Honeywell_100K_135_104LAG_J01;

         when NTC_100K_MGB18_104F39050L32 =>
            NTC_100K_MGB18_104F39050L32 : User_Config_Thermistor_NTC_100K_MGB18_104F39050L32;

         when PT_1000_PT_385 =>
            PT_1000_PT_385 : User_Config_Thermistor_PT_1000_PT_385;

         when PT_1000_PT_392 =>
            PT_1000_PT_392 : User_Config_Thermistor_PT_1000_PT_392;

         when Custom_Steinhart_Hart =>
            Custom_Steinhart_Hart : User_Config_Thermistor_Custom_Steinhart_Hart;

         when Custom_Callendar_Van_Dusen =>
            Custom_Callendar_Van_Dusen : User_Config_Thermistor_Custom_Callendar_Van_Dusen;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor is record
      --  This section contains the configuration for a single thermistor or RTD temperature sensor.

      Minimum_Temperature : Temperature range -1.0E100 * celsius .. 1.0E100 * celsius := 0.0 * celsius;
      --  If the measured temperature drops below this value while the sensor is used by a heater, Prunt treats it as
      --  a fault and performs an emergency stop.

      Maximum_Temperature : Temperature range -1.0E100 * celsius .. 1.0E100 * celsius := 0.0 * celsius;
      --  If the measured temperature rises above this value while the sensor is used by a heater, Prunt treats it as
      --  a fault and performs an emergency stop.

      Sensor_Model : User_Config_Thermistor_Model := (others => <>);
      --  This selects the sensor model and any custom coefficients needed to interpret its resistance.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Thermistor_Array is array (Thermistor_Name) of User_Config_Thermistor
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Thermistors : User_Config_Thermistor_Array := [others => <>];
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;
   --  Build the configuration schema.

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;
   --  Convert validated configuration data.

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);
   --  Store the configuration in Data.

   function To_Thermistor_Parameters (Config : User_Config_Thermistor) return Prunt.Thermistors.Thermistor_Parameters;
   --  Convert a thermistor configuration.

   procedure Report_Temperatures
     (Planner : Planner_Interface'Class;
      R       : Gcode_Optional_No_Value;
      --  Include redundant temperature information if present.
      T       : Gcode_Optional_Integer
      --  Optional hotend index.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M105");
   --  Report temperatures to the logger.

   procedure Set_Temperature_Auto_Report
     (Planner : Planner_Interface'Class;
      S       : Gcode_Optional_Integer
      --  Interval in seconds between reports. `S0` disables auto-reporting.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M155");
   --  Configure automatic temperature reporting to the logger.

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize (Config_In : User_Config);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      overriding
      function Thermistor_Is_Enabled_In_Config (Thermistor : Thermistor_Name) return Boolean;

      overriding
      function Get_Thermistor_Parameters (Thermistor : Thermistor_Name) return Prunt.Thermistors.Thermistor_Parameters;

      overriding
      function Get_Temperature (Thermistor : Thermistor_Name; Requires_Fresh : Boolean) return Temperature;
   private
      Config   : User_Config;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
   end Module_Instance;

end Prunt.Default_Modules.Thermistors;
