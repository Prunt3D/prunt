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

pragma Extensions_Allowed (On);

with Ada.Tags;
with Prunt.Config;
with Prunt.Gcode_Arguments;
with Prunt.Input_Shapers;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
package Prunt.Default_Modules.Input_Shapers is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Module_Instance_Interface is synchronized interface;

   function Get_Default_Axial_Shapers
     (This : Module_Instance_Interface) return Prunt.Input_Shapers.Axial_Shaper_Parameters
   is abstract;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Module_Instance_Interface with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

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

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize (Config_In : User_Config);

      overriding
      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref);

      procedure Configure_Input_Shaping
        (Planner : Planner_Interface'Class;
         P       : Virtual_String;
         --  Must be set to `"Prunt"`.
         X       : Gcode_Optional_String;
         --  Optional X-axis shaping mode or payload.
         Y       : Gcode_Optional_String;
         --  Optional Y-axis shaping mode or payload.
         Z       : Gcode_Optional_String;
         --  Optional Z-axis shaping mode or payload.
         E       : Gcode_Optional_String
         --  Optional E-axis shaping mode or payload.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M493");
      --  Configure input shaping for one or more axes.

      overriding
      function Get_Default_Axial_Shapers return Prunt.Input_Shapers.Axial_Shaper_Parameters;
   private
      Config   : User_Config;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
   end Module_Instance;

end Prunt.Default_Modules.Input_Shapers;
