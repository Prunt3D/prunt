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
with Prunt.Default_Modules.Config_Saving;
with Prunt.Default_Modules.Kinematics;
with Prunt.Gcode_Arguments;
with Prunt.Input_Shapers;
with Prunt.Module_Types; use Prunt.Module_Types;

private with Ada.Containers.Ordered_Maps;

generic
   with package Config_Saving_Module is new Default_Modules.Config_Saving;
   with package Kinematics_Module is new Default_Modules.Kinematics (others => <>);
package Prunt.Default_Modules.Input_Shapers is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class;
   --  Return the configuration schema.

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;
   --  Return the supported G-code commands.

   type Module_Instance_Interface is synchronized interface;

   function Get_Current_Axial_Shapers
     (This : Module_Instance_Interface) return Prunt.Input_Shapers.Axial_Shaper_Parameters
   is abstract;
   --  Return the active shaper parameters.

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

   type User_Config_Input_Shaping_No_Shaper is record
      --  Leave this axis unshaped.
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Input_Shaping_ZV is record
      --  Apply a zero-vibration shaper to this axis. Increase the derivative count for ZVD, ZVDD, or ZVDDD.

      Shaper_Frequency : Frequency range 1.0E-10 * hertz .. 1.0E100 * hertz := 1.0 * hertz;
      --  Resonant frequency to target. This value is specific to the selected shaper kind and is not copied when
      --  switching to another kind.

      Damping_Ratio : Prunt.Input_Shapers.Shaper_Damping_Ratio range 0.001 .. 0.999 := 0.1;
      --  Estimated damping ratio of the axis. This value is specific to the selected shaper kind and is not copied
      --  when switching to another kind.

      Number_Of_Derivatives : Prunt.Input_Shapers.Zero_Vibration_Deriviatives_Count := 0;
      --  Number of derivatives to include: 0 = ZV, 1 = ZVD, 2 = ZVDD, 3 = ZVDDD.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Input_Shaping_EI is record
      --  Apply an extra-insensitive shaper to this axis.

      Shaper_Frequency : Frequency range 1.0E-10 * hertz .. 1.0E100 * hertz := 1.0 * hertz;
      --  Resonant frequency to target. This value is specific to the selected shaper kind and is not copied when
      --  switching to another kind.

      Damping_Ratio : Prunt.Input_Shapers.Shaper_Damping_Ratio range 0.001 .. 0.999 := 0.1;
      --  Estimated damping ratio of the axis. This value is specific to the selected shaper kind and is not copied
      --  when switching to another kind.

      Residual_Vibration_Level : Prunt.Input_Shapers.Residual_Vibration_Level range 0.001 .. 0.999 := 0.05;
      --  Residual vibration target for the EI family. Other motion controllers usually hard-code this to 0.05.

      Number_Of_Humps : Prunt.Input_Shapers.Extra_Insensitive_Humps_Count := 1;
      --  Number of humps to use: 1 = EI, 2 = 2HEI, 3 = 3HEI.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Input_Shaping_Pressure_Advance is record
      --  Apply pressure advance with optional smoothing.
      --
      --  Pressure advance requires an independently mapped axis and cannot be used on CoreXY X or Y. This is due to
      --  the fact that pressure advance uses a catch-up mechanism when a motor speed limit would be exceeded rather
      --  than slowing down other axes, which would require extra code to handle. If you really need pressure advance
      --  on the X or Y axis on CoreXY then contact us and we can add support for it.

      Pressure_Advance_Time : Time range -1.0E100 * s .. 1.0E100 * s := 0.0 * s;
      --  Advance time added based on this axis velocity.

      Pressure_Advance_Smooth_Time : Time range 0.0 * s .. 0.2 * s := 0.0 * s;
      --  Length of the triangular smoothing window applied either to the added component or to the full output.

      Smooth_Added_Part_Only : Boolean := False;
      --  If enabled, smooth only the pressure-advance contribution. Otherwise smooth the full output position.

      Smoothing_Levels : User_Config_Integer range 1 .. 10 := 2;
      --  Number of cascaded moving-average stages to apply. A value of 2 matches Klipper's smoothing behaviour.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Input_Shaping_Method_Kind is (No_Shaper, ZV, EI, Pressure_Advance)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Input_Shaping_Method (Kind : User_Config_Input_Shaping_Method_Kind := No_Shaper) is record
      --  Select the shaping method to apply to this axis.

      case Kind is
         when No_Shaper =>
            No_Shaper : User_Config_Input_Shaping_No_Shaper;

         when ZV =>
            ZV : User_Config_Input_Shaping_ZV;

         when EI =>
            EI : User_Config_Input_Shaping_EI;

         when Pressure_Advance =>
            Pressure_Advance : User_Config_Input_Shaping_Pressure_Advance;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Input_Shaping_Array is array (Axis_Name) of User_Config_Input_Shaping_Method
   with Annotate => (Prunt_Config, Tabbed), Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Input_Shaping : User_Config_Input_Shaping_Array := [others => <>];
      --  Configure the default input shaper used for each axis.
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;
   --  Build the configuration schema.

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;
   --  Convert validated configuration data.

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);
   --  Store the configuration in Data.

   type Pressure_Advance_Axis_Set is array (Axis_Name) of Boolean;

   package Input_Shaping_Update_Maps is new Ada.Containers.Ordered_Maps (Axis_Name, User_Config_Input_Shaping_Method);

   type Input_Shaping_Config_Update is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Updated_Configs     : Input_Shaping_Update_Maps.Map;
   end record;

   overriding
   procedure Process_After_Block (This : Input_Shaping_Config_Update; Context : Block_End_Context'Class);
   --  Apply shaper changes.

   function Build_Shaper_Parameters
     (Method : User_Config_Input_Shaping_Method) return Prunt.Input_Shapers.Shaper_Parameters;
   --  Convert a shaping configuration.

   function Parse_Axial_Shaper_Config (Value : Virtual_String) return User_Config_Input_Shaping_Method;
   --  Parse an axis configuration.

   procedure Configure_Input_Shaping
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      P        : Virtual_String;
      --  Must be set to `"Prunt"` to avoid conflicts with Marlin g-code.
      X        : Gcode_Optional_String;
      --  JSON object describing the X-axis shaper.
      Y        : Gcode_Optional_String;
      --  JSON object describing the Y-axis shaper.
      Z        : Gcode_Optional_String;
      --  JSON object describing the Z-axis shaper.
      E        : Gcode_Optional_String
      --  JSON object describing the E-axis shaper.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M493");
   --  Configure input shaping for one or more axes.
   --
   --  Each provided axis parameter must be a JSON object inside a G-code string. Use single quotes around the whole
   --  JSON payload so normal JSON double quotes can be used inside it. Below are the various options:
   --
   --  `{"Kind" : "No_Shaper"}`
   --
   --  `{"Kind" : "Zero_Vibration", "Shaper_Frequency" : 40.0, "Damping_Ratio" : 0.1, "Number_Of_Derivatives" : 1}`
   --
   --  `{"Kind" : "Extra_Insensitive", "Shaper_Frequency" : 40.0, "Damping_Ratio":0.1, "Residual_Vibration_Level" :
   --  0.05, "Number_Of_Humps" : 1}`
   --
   --  `{"Kind" : "Pressure_Advance", "Pressure_Advance_Time" : 0.02, "Pressure_Advance_Smooth_Time" : 0.01,
   --  "Smooth_Added_Part_Only" : false, "Smoothing_Levels" : 2}`.
   --
   --  `Shaper_Frequency` is in hertz and all time values are in seconds. Changes can be saved with `M500`.

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize
        (Config_In                        : User_Config;
         Config_Data_In                   : Prunt.Config.Config_Data;
         Pressure_Advance_Allowed_Axes_In : Pressure_Advance_Axis_Set);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Apply_Runtime_Config (Updates : Input_Shaping_Update_Maps.Map);

      overriding
      function Get_Current_Axial_Shapers return Prunt.Input_Shapers.Axial_Shaper_Parameters;

      function Pressure_Advance_Is_Allowed (Axis : Axis_Name) return Boolean;
   private
      Config                        : User_Config;
      Config_Data                   : Prunt.Config.Config_Data;
      Pressure_Advance_Allowed_Axes : Pressure_Advance_Axis_Set := [others => False];
      Self_Ref                      : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
   end Module_Instance;

end Prunt.Default_Modules.Input_Shapers;
