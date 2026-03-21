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
with Prunt.Default_Modules.Input_Shapers;
with Prunt.Default_Modules.Motor_Drivers;
with Prunt.Gcode_Arguments;
with Prunt.Motion_Planner;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
   type Motor_Name is (<>);
   with package Motor_Drivers_Module is new Default_Modules.Motor_Drivers (Motor_Name => Motor_Name);
   with package Input_Shapers_Module is new Default_Modules.Input_Shapers;
package Prunt.Default_Modules.Kinematics is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Motor_Position_Map is array (Axis_Name, Motor_Name) of Length;

   type Motion_Planner_Configuration is record
      Parameters         : Motion_Planner.Kinematic_Parameters;
      Motors_To_Position : Motor_Position_Map := [others => [others => Length'Last]];
   end record;

   type Module_Instance_Interface is synchronized interface;

   function Get_Default_Motion_Planner_Configuration
     (This : Module_Instance_Interface) return Motion_Planner_Configuration
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

   type User_Config_Cartesian_Axis_Name is (None, X_Axis, Y_Axis, Z_Axis, E_Axis)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics_Cartesian is array (Motor_Name) of User_Config_Cartesian_Axis_Name
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Core_XY_Axis_Name is (None, A_Axis, B_Axis, Z_Axis, E_Axis)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics_Core_XY is array (Motor_Name) of User_Config_Core_XY_Axis_Name
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics_Kind is (Cartesian, Core_XY) with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics_Variant (Kind : User_Config_Kinematics_Kind := Cartesian) is record
      --  This setting defines the kinematic system of your machine. The kinematics determine how the movement of the
      --  individual stepper motors is translated into the movement of the toolhead in the X, Y, and Z dimensions.

      case Kind is
         when Cartesian =>
            Cartesian : User_Config_Kinematics_Cartesian := [others => None];
            --  For a Cartesian machine, where a given motor only moves one axis, assign each motor to the axis it
            --  controls (X, Y, Z, or E for extruder). If a motor is not used, assign it to 'None'.

         when Core_XY =>
            Core_XY : User_Config_Kinematics_Core_XY := [others => None];
            --  For a Core XY machine, where some motors move both the X and Y axis, assign the two motors that
            --  control the X and Y movement to the A and B axes. Assign the remaining motors to the axes they
            --  directly control (E for extruder and Z). If a motor is not used, assign it to 'None'.
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Position_Limits_Array is array (Axis_Name) of Length range -1.0E100 * mm .. 1.0E100 * mm
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axial_Velocity_Limits_Array is
     array (Axis_Name) of Velocity range 0.000_001 * mm / s .. 1.0E100 * mm / s
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axial_Scaler_Array is array (Axis_Name) of Dimensionless range 1.0E-100 .. 1.0E100
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics is record
      --  This section contains settings related to the machine's movement, geometry, and motion planning.

      Lower_Position_Limit : User_Config_Position_Limits_Array := [others => 0.0 * mm];
      --  This defines the minimum position that each axis can travel to. To effectively disable the lower limit for
      --  an axis, you can set it to -1E100. The E axis should almost always be set to -1E100.

      Upper_Position_Limit : User_Config_Position_Limits_Array := [others => 0.0 * mm];
      --  This defines the maximum position that each axis can travel to. To effectively disable the upper limit for
      --  an axis, you can set it to 1E100. The E axis should almost always be set to 1E100.

      Ignore_E_In_XYZE : Boolean := True;
      --  This setting changes how the feedrate is applied when both the extruder (E) and other axes (X, Y, Z) are
      --  moving simultaneously. When enabled (which is the default and mimics the behavior of most other 3D printer
      --  firmwares), the feedrate specified in the G-code command will only apply to the X, Y, and Z axes. The
      --  extruder will move as fast as necessary to keep up, within its velocity limits. This is generally the
      --  desired behavior. If you disable this setting, the feedrate will be distributed among all moving axes,
      --  including the extruder.
      --
      --  For example, with the command 'G1 X1 E100 F100', if this setting is enabled, the X-axis will move at
      --  100 mm/min, and the E-axis will move proportionally. If disabled, the combined speed of the X and E axes
      --  will be 100 mm/min, meaning the X axis will only move at approximately 1 mm/min.
      --
      --  Regardless of this setting, the individual feedrate limits for each axis will always be respected.

      Shift_Blended_Corners : Boolean := False;
      --  When the motion planner blends corners to maintain a higher speed, it does so by creating a curved path that
      --  cuts inside the original corner. When this setting is enabled, Prunt will attempt to shift the blended
      --  corner path so that it intersects the original corner point. This can result in a path that is slightly more
      --  faithful to curved sections in the original CAD models before export and slicing, but it also means that the
      --  straight line segments leading into and out of the corner will be shifted outwards slightly.

      Maximum_Tangential_Velocity : Velocity range 0.000_001 * mm / s .. 1.0E100 * mm / s := 10.0 * mm / s;
      --  This is the maximum combined speed of all axes, including the extruder. It's a global limit on the
      --  toolhead's speed. In most cases, it's better to set this to a very high value (e.g., 1E100) and use the
      --  per-axis velocity limits below to control the speed of your machine.

      Axial_Velocity_Limits : User_Config_Axial_Velocity_Limits_Array := [others => 10.0 * mm / s];
      --  This sets the maximum speed for each individual axis.

      Maximum_Chord_Error : Length range 0.0 * mm .. 1.0E100 * mm := 0.1 * mm;
      --  This setting controls how far a path is allowed to deviate from the path specified in G-code. Instead of
      --  coming to a complete stop at every corner, the motion planner can create a smooth, curved path that 'cuts'
      --  the corner. This allows the machine to maintain a higher average speed. This setting defines the maximum
      --  allowed distance between the curved path and the original, sharp corner. A value of 0 will disable corner
      --  blending, causing the machine to come to a full stop at every corner.

      Maximum_Acceleration : Acceleration range 0.000_001 * mm / s ** 2 .. 1.0E100 * mm / s ** 2 :=
        100.0 * mm / s ** 2;
      --  This is the maximum rate at which the printer can change its velocity. A higher acceleration will result in
      --  faster prints, but may also cause vibrations and ringing artifacts. This can be set to a very high value
      --  1E100 to effectively disable the limit and rely on the higher order constraints below.

      Maximum_Jerk : Jerk range 0.000_001 * mm / s ** 3 .. 1.0E100 * mm / s ** 3 := 100.0E2 * mm / s ** 3;
      --  Jerk is the rate of change of acceleration. It determines how abruptly the printer can change its
      --  acceleration. A higher jerk value allows for faster changes in direction, but can also introduce vibrations.
      --  A good starting point for tuning is to set the jerk to 100 times the maximum acceleration (e.g., if
      --  acceleration is 1000, set jerk to 100,000). You can do this by appending 'E2' to your acceleration value in
      --  this field. This can be set to a very high value 1E100 to effectively disable the limit.

      Maximum_Snap : Snap range 0.000_001 * mm / s ** 4 .. 1.0E100 * mm / s ** 4 := 100.0E5 * mm / s ** 4;
      --  Snap is the rate of change of jerk. It's a higher-order derivative of motion that can help to smooth out
      --  movements even further. A good starting point for tuning is to set the snap to 100,000 times the maximum
      --  acceleration (append 'E5' to your acceleration value in this field). This can be set to a very high value
      --  1E100 to effectively disable the limit.

      Maximum_Crackle : Crackle range 0.000_001 * mm / s ** 5 .. 1.0E100 * mm / s ** 5 := 100.0E9 * mm / s ** 5;
      --  Crackle is the rate of change of snap. It's an even higher-order derivative of motion. A good starting point
      --  for tuning is to set the crackle to 1,000,000,000 times the maximum acceleration (append 'E9' to your
      --  acceleration value in this field). This can be set to a very high value 1E100 to effectively disable the
      --  limit.

      Axial_Scaler : User_Config_Axial_Scaler_Array := [others => 1.0];
      --  Inside the motion planner, all positions are divided by this value before applying motion profile limits,
      --  allowing for different limits on different axes. You do not need to take this value into account when
      --  setting position limits, mm per step values, axial velocity limits, or when setting the feedrate in g-code.
      --  Corner deviation and tangential feedrate, acceleration, etc. is based on scaled positions, so a tangential
      --  acceleration of 10mm/s^2 and a scaler of 0.5 will set the axial limit to 5mm/s^2.

      Kinematics_Kind : User_Config_Kinematics_Variant := (others => <>);
      --  This selects the kinematic layout and assigns motors to the axes they control.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Kinematics : User_Config_Kinematics := (others => <>);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize
        (Config_In                            : User_Config;
         Motor_Drivers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Input_Shapers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref);

      overriding
      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref);

      procedure Set_Print_And_Travel_Move_Limits
        (Planner : Planner_Interface'Class;
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         T       : Gcode_Optional_Integer;
         F       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M201");
      --  Set maximum acceleration and XY frequency-limit values.

      procedure Set_Max_Feedrate
        (Planner : Planner_Interface'Class;
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         T       : Gcode_Optional_Integer)
      with Annotate => (Prunt_Config, Gcode_Command, "M203");
      --  Set maximum feedrates.

      procedure Set_Starting_Acceleration
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_Float;
         R       : Gcode_Optional_Float;
         T       : Gcode_Optional_Float;
         S       : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M204");
      --  Set preferred starting acceleration values.

      procedure Set_Advanced_Motion_Settings
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_String;
         --  Must be set to `"Prunt"` for Prunt-specific settings.
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         B       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float;
         T       : Gcode_Optional_Float;
         J       : Gcode_Optional_Float)
      with Annotate => (Prunt_Config, Gcode_Command, "M205");
      --  Set advanced motion settings.

      overriding
      function Get_Default_Motion_Planner_Configuration return Motion_Planner_Configuration;
   private
      Config                            : User_Config;
      Self_Ref                          : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
      Motor_Drivers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Input_Shapers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
   end Module_Instance;

end Prunt.Default_Modules.Kinematics;
