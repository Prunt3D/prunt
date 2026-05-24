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
with Prunt.Default_Modules.Config_Saving;
with Prunt.Default_Modules.Input_Shapers;
with Prunt.Default_Modules.Motor_Drivers;
with Prunt.Gcode_Arguments;
with Prunt.Motion_Planner;
with Prunt.Module_Types; use Prunt.Module_Types;

private with Ada.Containers.Ordered_Maps;

generic
   with package My_Controller_Generic_Types is new Controller_Generic_Types (<>);
   --  We need to pass in the whole package rather than just `Motor_Name` so codegen can properly resolve the types.
   use My_Controller_Generic_Types;
   with package Config_Saving_Module is new Default_Modules.Config_Saving;
   with package Motor_Drivers_Module is new
     Default_Modules.Motor_Drivers (My_Controller_Generic_Types => My_Controller_Generic_Types);
   with package Input_Shapers_Module is new Default_Modules.Input_Shapers (others => <>);
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
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
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
      --  This setting defines the kinematic system of your machine. The kinematics determine how the movement of
      --  individual motors is translated into the movement of the toolhead in the X, Y, and Z axes.

      case Kind is
         when Cartesian =>
            Cartesian : User_Config_Kinematics_Cartesian := [others => None];
            --  For a Cartesian machine, where a given motor only moves one axis, assign each motor to the axis it
            --  controls (X, Y, Z, or E for extruder). If a motor is not used, assign it to `None`.

         when Core_XY =>
            Core_XY : User_Config_Kinematics_Core_XY := [others => None];
            --  For a Core XY machine, where some motors move both the X and Y axis, assign the two motors that control
            --  the X and Y movement to the A and B axes. Assign the remaining motors to the axes they directly control
            --  (E for extruder and Z). If a motor is not used, assign it to `None`.
            --
            --  TODO: Put motion equations here and details on fixing direction.
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Position_Limits_Array is array (Axis_Name) of Length range -1.0E100 * mm .. 1.0E100 * mm
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axial_Velocity_Limits_Array is
     array (Axis_Name) of Velocity range 1.0E-6 * mm / s .. 1.0E100 * mm / s
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axial_Scaler_Array is array (Axis_Name) of Dimensionless range 1.0E-100 .. 1.0E100
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics is record
      --  This section contains settings related to the machine's movement, geometry, and motion planning.

      Lower_Position_Limit : User_Config_Position_Limits_Array := [others => 0.0 * mm];
      --  This defines the minimum position that each axis can travel to. To effectively disable the lower limit for an
      --  axis, you can set it to `-1E100`. The E axis should almost always be set to `-1E100`.

      Upper_Position_Limit : User_Config_Position_Limits_Array := [others => 0.0 * mm];
      --  This defines the maximum position that each axis can travel to. To effectively disable the upper limit for
      --  an axis, you can set it to `1E100`. The E axis should almost always be set to `1E100`.

      Ignore_E_In_XYZE : Boolean := True;
      --  This setting changes how the feedrate is applied when both the extruder (E) and other axes (X, Y, Z) are
      --  moving simultaneously. When enabled (which is the default and mimics the behaviour of most other 3D printer
      --  firmwares), the feedrate specified in the G-code command will only apply to the X, Y, and Z axes. The
      --  extruder will move as fast as necessary to keep up, within its velocity limits. This is generally the desired
      --  behaviour. If you disable this setting, the feedrate will be distributed among all moving axes, including the
      --  extruder.
      --
      --  For example, with the command 'G1 X1 E100 F100', if this setting is enabled, the X-axis will move at 100
      --  mm/min, and the E-axis will move proportionally. If disabled, the combined speed of the X and E axes will be
      --  100 mm/min, meaning the X axis will only move at approximately 1 mm/min.
      --
      --  Regardless of this setting, the individual feedrate limits for each axis will always be respected.

      Shift_Blended_Corners : Boolean := False;
      --  When the motion planner blends corners to maintain a higher speed, it does so by creating a curved path that
      --  cuts inside the original corner. When this setting is enabled, Prunt will attempt to shift the blended corner
      --  path so that it intersects the original corner point. This can result in a path that is slightly more
      --  faithful to curved sections in the original CAD models before export and slicing, but it also means that the
      --  straight line segments leading into and out of the corner will be shifted outwards slightly.
      --
      --  TODO: Image here.

      Maximum_Tangential_Velocity : Velocity range 0.000_001 * mm / s .. 1.0E100 * mm / s := 10.0 * mm / s;
      --  This is the maximum combined speed of all axes. It's a global limit on the toolhead's speed. In most cases,
      --  it's better to set this to a very high value (e.g., `1E100`) and use the per-axis velocity limits below to
      --  control the speed of your machine.

      Axial_Velocity_Limits : User_Config_Axial_Velocity_Limits_Array := [others => 10.0 * mm / s];
      --  This sets the maximum speed for each individual axis.

      Maximum_Chord_Error : Length range 0.0 * mm .. 1.0E100 * mm := 0.1 * mm;
      --  This setting controls how far a path is allowed to deviate from the path specified in G-code. Instead of
      --  coming to a complete stop at every corner, the motion planner can create a smooth, curved path that 'cuts'
      --  the corner. This allows the machine to maintain a higher average speed. This setting defines the maximum
      --  allowed distance between the curved path and the original, sharp corner. A value of 0 will disable corner
      --  blending, causing the machine to come to a full stop at every corner.
      --
      --  TODO: Image here.

      Maximum_Acceleration : Acceleration range 0.000_001 * mm / s ** 2 .. 1.0E100 * mm / s ** 2 :=
        100.0 * mm / s ** 2;
      --  This is the maximum rate at which the printer can change its velocity. A higher acceleration will result in
      --  faster prints, but may also cause vibrations and ringing artifacts. This can be set to a very high value
      --  to effectively disable the limit and rely on the higher order constraints below.

      Maximum_Jerk : Jerk range 0.000_001 * mm / s ** 3 .. 1.0E100 * mm / s ** 3 := 100.0E2 * mm / s ** 3;
      --  Jerk is the rate of change of acceleration. It determines how abruptly the printer can change its
      --  acceleration. A higher jerk value allows for faster changes in direction, but can also introduce vibrations.
      --  A good starting point for tuning is to set the jerk to 100 times the maximum acceleration (e.g., if
      --  acceleration is 1000, set jerk to 100,000). You can do this by appending `E2` to your acceleration value in
      --  this field. This can be set to a very high value to effectively disable the limit.

      Maximum_Snap : Snap range 0.000_001 * mm / s ** 4 .. 1.0E100 * mm / s ** 4 := 100.0E5 * mm / s ** 4;
      --  Snap is the rate of change of jerk. It's a higher-order derivative of motion that can help to smooth out
      --  movements even further. A good starting point for tuning is to set the snap to 100,000 times the maximum
      --  acceleration (append `E5` to your acceleration value in this field). This can be set to a very high value to
      --  effectively disable the limit.

      Maximum_Crackle : Crackle range 0.000_001 * mm / s ** 5 .. 1.0E100 * mm / s ** 5 := 100.0E9 * mm / s ** 5;
      --  Crackle is the rate of change of snap. It's an even higher-order derivative of motion. A good starting point
      --  for tuning is to set the crackle to 1,000,000,000 times the maximum acceleration (append `E9` to your
      --  acceleration value in this field). This can be set to a very high value to effectively disable the limit.

      Axial_Scaler : User_Config_Axial_Scaler_Array := [others => 1.0];
      --  Inside the motion planner, all positions are divided by this value before applying motion profile limits,
      --  allowing for different limits on different axes. You do not need to take this value into account when setting
      --  position limits, mm per step values, axial velocity limits, or when setting the feedrate in g-code. Corner
      --  deviation and tangential feedrate, acceleration, etc. is based on scaled positions, so a tangential
      --  acceleration of 10 mm/s² and a scaler of 0.5 will set the axial limit to 5mm/s².

      Kinematics_Kind : User_Config_Kinematics_Variant := (others => <>);
      --  This selects the kinematic layout and allows motors to be assigned to the axes they control.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Kinematics : User_Config_Kinematics := (others => <>);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   function Velocity_Values_Equal (Left, Right : Velocity) return Boolean
   is (Left = Right);

   package Axial_Velocity_Update_Maps is new
     Ada.Containers.Ordered_Maps (Axis_Name, Velocity, "=" => Velocity_Values_Equal);

   type Runtime_Kinematics_Updates is record
      Axial_Velocity_Limits    : Axial_Velocity_Update_Maps.Map := [];
      Has_Maximum_Acceleration : Boolean := False;
      Maximum_Acceleration     : Acceleration := 1.0E-6 * mm / s ** 2;
      Has_Maximum_Jerk         : Boolean := False;
      Maximum_Jerk             : Jerk := 1.0E-6 * mm / s ** 3;
      Has_Maximum_Snap         : Boolean := False;
      Maximum_Snap             : Snap := 1.0E-6 * mm / s ** 4;
      Has_Maximum_Crackle      : Boolean := False;
      Maximum_Crackle          : Crackle := 1.0E-6 * mm / s ** 5;
      Has_Maximum_Chord_Error  : Boolean := False;
      Maximum_Chord_Error      : Length := 0.0 * mm;
   end record;

   type Kinematics_Config_Update is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Updates             : Runtime_Kinematics_Updates;
   end record;

   overriding
   procedure Process_After_Block (This : Kinematics_Config_Update; Context : Block_End_Context'Class);

   procedure Set_Max_Feedrate
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_Float;
      --  If present, set the X axis maximum feedrate in mm/s.
      Y        : Gcode_Optional_Float;
      --  If present, set the Y axis maximum feedrate in mm/s.
      Z        : Gcode_Optional_Float;
      --  If present, set the Z axis maximum feedrate in mm/s.
      E        : Gcode_Optional_Float
      --  If present, set the E axis maximum feedrate in mm/s.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M203");
   --  Set maximum axial feedrates. May be saved using `M500`.
   --
   --  The `T` parameter from Marlin is not present.

   procedure Set_Dynamic_Kinematic_Limits
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      P        : Virtual_String;
      --  Must be set to `"Prunt"` to prevent conflicts with Marlin g-code.
      A        : Gcode_Optional_Float;
      --  Acceleration limit in mm/s². Not modified if not specified.
      J        : Gcode_Optional_Float;
      --  Jerk limit in mm/s³. Not modified if not specified.
      S        : Gcode_Optional_Float;
      --  Snap limit in mm/s⁴. Not modified if not specified.
      C        : Gcode_Optional_Float;
      --  Crackle limit in mm/s⁵. Not modified if not specified.
      D        : Gcode_Optional_Float
      --  Path deviation limit in mm. Not modified if not specified.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M205");
   --  Set dynamic kinematic limits. May be saved using `M500`.
   --
   --  This command differs significantly from `M205` in Marlin, so `P"Prunt"` must always be present to prevent
   --  conflicts.

   protected type Module_Instance is new My_Modules.Module_Instance and Module_Instance_Interface with
      procedure Initialize
        (Config_In                            : User_Config;
         Config_Data_In                       : Config.Config_Data;
         Motor_Drivers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Input_Shapers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Apply_Runtime_Config (Updates : Runtime_Kinematics_Updates);

      overriding
      function Get_Default_Motion_Planner_Configuration return Motion_Planner_Configuration;

      function Get_Config return User_Config;
   private
      Config                            : User_Config;
      Config_Data                       : Prunt.Config.Config_Data;
      Motor_Drivers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Input_Shapers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
   end Module_Instance;

end Prunt.Default_Modules.Kinematics;
