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
with Prunt.Default_Modules.Motor_Drivers;
with Prunt.Gcode_Arguments;
with Prunt.Kinematic_Transforms;
with Prunt.Motion_Planner;
with Prunt.Module_Types; use Prunt.Module_Types;

generic
   use My_Controller_Generic_Types;
   with package Transforms is new Prunt.Kinematic_Transforms (Motor_Name, Motor_Position);
   with package Config_Saving_Module is new Default_Modules.Config_Saving;
   with package Motor_Drivers_Module is new Default_Modules.Motor_Drivers;
package Prunt.Default_Modules.Kinematics is

   type Delta_Tower_Motor_Array is array (Transforms.Delta_Tower_Name, Motor_Name) of Boolean;
   --  Membership map from each logical tower to all of its physical drive motors.

   type Motion_Planner_Configuration is record
      Parameters : Motion_Planner.Kinematic_Parameters;
      Transform  : Transforms.Kinematic_Transform := Transforms.Default_Transform;
   end record;

   type Kinematics_Homing_Kind is (Non_Delta_Kinematics, Linear_Delta_Kinematics);

   type Kinematics_Homing_Configuration (Kind : Kinematics_Homing_Kind := Non_Delta_Kinematics) is record
      case Kind is
         when Non_Delta_Kinematics =>
            null;

         when Linear_Delta_Kinematics =>
            Tower_Motors   : Delta_Tower_Motor_Array := [others => [others => False]];
            Planner_Config : Motion_Planner_Configuration := (others => <>);
      end case;
   end record;

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class;
   --  Return the configuration schema.

   function Kinematics_Kind_Config_Path return Config.Config_Path;
   --  Return the typed path of the configured kinematics-kind selector for presentation dependencies.

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;
   --  Return the supported G-code commands.

   type Module_Instance_Interface is synchronized interface;

   function Get_Default_Motion_Planner_Configuration
     (This : Module_Instance_Interface) return Motion_Planner_Configuration
   is abstract;
   --  Return the current planner parameters and motor map.

   function Axis_Is_Motor_Separable (This : Module_Instance_Interface; Axis : Axis_Name) return Boolean is abstract;
   --  Return whether Axis maps independently.

   function Motor_Affects_Axis (This : Module_Instance_Interface; Motor : Motor_Name; Axis : Axis_Name) return Boolean
   is abstract;
   --  Return whether movement of Axis changes Motor's position.

   function Get_Homing_Configuration (This : Module_Instance_Interface) return Kinematics_Homing_Configuration
   is abstract;
   --  Return the kinematics information required to configure and execute homing.

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Module_Instance_Interface with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
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

   use Transforms;

   type User_Config_Cartesian_Axis_Name is (None, X_Axis, Y_Axis, Z_Axis, E_Axis)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics_Cartesian is array (Motor_Name) of User_Config_Cartesian_Axis_Name
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Core_XY_Axis_Name is (None, A_Axis, B_Axis, Z_Axis, E_Axis)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics_Core_XY is array (Motor_Name) of User_Config_Core_XY_Axis_Name
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Delta_Axis_Name is (None, Tower_A, Tower_B, Tower_C, E_Axis)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics_Delta_Motors is array (Motor_Name) of User_Config_Delta_Axis_Name
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics_Delta_Tower is record
      Arm_Length : Length range 0.000_001 * mm .. 1.0E100 * mm := 250.0 * mm;
      --  Length of the diagonal rod connecting this tower's carriage to the effector.

      Angle : Standard.Prunt.Angle range -360.0 * deg .. 360.0 * deg := 0.0 * deg;
      --  Counter-clockwise angle of this tower around the build-area centre. Zero degrees points along positive X.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics_Delta is record
      Motors : User_Config_Kinematics_Delta_Motors := [others => None];
      --  Assign each enabled motor to a tower or to the extruder axis. Multiple motors may drive the same tower or
      --  extruder axis.

      Delta_Radius : Length range 0.0 * mm .. 1.0E100 * mm := 100.0 * mm;
      --  Horizontal distance from the centre of the machine to each tower carriage axis.

      Tower_A : User_Config_Kinematics_Delta_Tower := (Arm_Length => 1.0 * mm, Angle => 210.0 * deg);
      --  Geometry for the front-left tower.

      Tower_B : User_Config_Kinematics_Delta_Tower := (Arm_Length => 1.0 * mm, Angle => 330.0 * deg);
      --  Geometry for the front-right tower.

      Tower_C : User_Config_Kinematics_Delta_Tower := (Arm_Length => 1.0 * mm, Angle => 90.0 * deg);
      --  Geometry for the rear tower.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics_Kind is (Cartesian, Core_XY, Linear_Delta)
   with Annotate => (Prunt_Config, User_Config), Annotate => (Prunt_Config, Experimental, "Linear_Delta");

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

         when Linear_Delta =>
            Linear_Delta : User_Config_Kinematics_Delta := (others => <>);
            --  A classic three-tower linear delta.
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Workspace_Kind is (Rectangular, Circular) with Annotate => (Prunt_Config, User_Config);

   type User_Config_Workspace_Bounds (Kind : User_Config_Workspace_Kind := Rectangular) is record
      --  Bounds of the commanded X/Y/Z/E coordinate space. The selected shape applies to X/Y only.

      Lower_Z : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Minimum allowed Z coordinate.

      Upper_Z : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Maximum allowed Z coordinate.

      Lower_E : Length range -1.0E100 * mm .. 1.0E100 * mm := -1.0E100 * mm;
      --  Minimum allowed E coordinate. The default of -1.0E100 effectively disables the lower extruder position limit.

      Upper_E : Length range -1.0E100 * mm .. 1.0E100 * mm := 1.0E100 * mm;
      --  Maximum allowed E coordinate. The default of 1.0E100 effectively disables the upper extruder position limit.

      case Kind is
         when Rectangular =>
            Lower_X : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
            --  Minimum allowed X coordinate.

            Upper_X : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
            --  Maximum allowed X coordinate.

            Lower_Y : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
            --  Minimum allowed Y coordinate.

            Upper_Y : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
            --  Maximum allowed Y coordinate.

         when Circular =>
            Radius : Length range 0.0 * mm .. 1.0E100 * mm := 0.0 * mm;
            --  Maximum radial distance from X=0, Y=0.
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axial_Deviation_Limits_Array is array (Axis_Name) of Length range 0.0 * mm .. 1.0E100 * mm
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axial_Velocity_Limits_Array is
     array (Axis_Name) of Velocity range 1.0E-6 * mm / s .. 1.0E100 * mm / s
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axial_Acceleration_Limits_Array is
     array (Axis_Name) of Acceleration range 1.0E-6 * mm / s ** 2 .. 1.0E100 * mm / s ** 2
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axial_Jerk_Limits_Array is
     array (Axis_Name) of Jerk range 1.0E-6 * mm / s ** 3 .. 1.0E100 * mm / s ** 3
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axial_Snap_Limits_Array is
     array (Axis_Name) of Snap range 1.0E-6 * mm / s ** 4 .. 1.0E100 * mm / s ** 4
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Axial_Crackle_Limits_Array is
     array (Axis_Name) of Crackle range 1.0E-6 * mm / s ** 5 .. 1.0E100 * mm / s ** 5
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Cornering_Stereographic is record
      --  Use an inverse-stereographic unit-tangent curve for supported line/helix combinations. This is the default
      --  and the only family that preserves the planner's higher-order endpoint continuity: position and its first
      --  four distance derivatives match the adjoining path at each endpoint.

      Axial_Deviation_Limits : User_Config_Axial_Deviation_Limits_Array := [others => 0.1 * mm];
      --  Maximum deviation from the commanded path along each scaled axis. The limits form an axis-aligned corridor
      --  around the requested path. Setting every component to zero disables corner curves. Setting one component to
      --  zero requires that coordinate to be preserved exactly.

      Maximum_Corner_Miss_Distance : Length range 0.0 * mm .. 1.0E100 * mm := 0.1 * mm;
      --  Maximum distance by which the curve may miss the commanded corner point.

      Shape_Bias : Dimensionless range -1.0 .. 1.0 := 0.0;
      --  Bias the curve toward the incoming side when negative and toward the outgoing side when positive.

      Circularity : Dimensionless range 0.0 .. 1.0 := 0.0;
      --  Prefer a more circular-looking curve without increasing the allowed deviation.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Cornering_Circular is record
      --  Use an exact circular fillet for supported line-to-line corners. Position and tangent are continuous and
      --  acceleration remains bounded, but acceleration may jump at the endpoints. Jerk, snap, and crackle limits do
      --  not apply to those endpoint jumps.

      Axial_Deviation_Limits : User_Config_Axial_Deviation_Limits_Array := [others => 0.1 * mm];
      --  Maximum deviation from the commanded path along each scaled axis.

      Maximum_Corner_Miss_Distance : Length range 0.0 * mm .. 1.0E100 * mm := 0.1 * mm;
      --  Maximum distance by which the arc may miss the commanded corner point.

      Maximum_Radius : Length range 0.0 * mm .. 1.0E100 * mm := 1.0E100 * mm;
      --  Maximum circular-fillet radius. The default of 1.0E100 leaves the radius effectively uncapped so the
      --  deviation limits determine its size.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Cornering_Parabolic is record
      --  Use a quadratic parabolic curve for supported line-to-line corners. Position and tangent are continuous and
      --  acceleration remains bounded, but acceleration may jump at the endpoints. Jerk, snap, and crackle limits do
      --  not apply to those endpoint jumps.

      Axial_Deviation_Limits : User_Config_Axial_Deviation_Limits_Array := [others => 0.1 * mm];
      --  Maximum deviation from the commanded path along each scaled axis.

      Maximum_Corner_Miss_Distance : Length range 0.0 * mm .. 1.0E100 * mm := 0.1 * mm;
      --  Maximum distance by which the curve may miss the commanded corner point.

      Shape_Bias : Dimensionless range -1.0 .. 1.0 := 0.0;
      --  Bias the curve toward the incoming side when negative and toward the outgoing side when positive.

      Maximum_Trim : Length range 0.0 * mm .. 1.0E100 * mm := 1.0E100 * mm;
      --  Maximum distance that the curve may trim from either adjoining path. The default of 1.0E100 leaves trimming
      --  effectively uncapped so the deviation limits determine the curve size.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Cornering_Biarc is record
      --  Use two tangent circular arcs for line/helix corners where a certified biarc exists. Position and tangent are
      --  continuous. Acceleration may jump at the endpoints and at the internal arc splice, so jerk, snap, and crackle
      --  limits do not apply at those locations.

      Axial_Deviation_Limits : User_Config_Axial_Deviation_Limits_Array := [others => 0.1 * mm];
      --  Maximum deviation from the commanded path along each scaled axis.

      Maximum_Corner_Miss_Distance : Length range 0.0 * mm .. 1.0E100 * mm := 0.1 * mm;
      --  Maximum distance by which the biarc may miss the commanded corner point.

      Shape_Bias : Dimensionless range -1.0 .. 1.0 := 0.0;
      --  Bias the biarc toward the incoming side when negative and toward the outgoing side when positive.

      Maximum_Trim : Length range 0.0 * mm .. 1.0E100 * mm := 1.0E100 * mm;
      --  Maximum distance that the biarc may trim from either adjoining path. The default of 1.0E100 leaves trimming
      --  effectively uncapped so the deviation limits determine the biarc size.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Cornering_Sharp_SCV is record
      --  Keep corners between primitives with usable tangents geometrically sharp and use Klipper-style square-corner
      --  velocity to limit traversal speed according to the change in direction. Velocity direction is discontinuous;
      --  acceleration, jerk, snap, and crackle limits intentionally do not apply at that junction. Spatial tangents
      --  are compared in XYZ when Ignore_E_In_XYZE is enabled; mixed pure-E/spatial junctions become hard stops.

      Square_Corner_Velocity : Velocity range 0.0 * mm / s .. 1.0E100 * mm / s := 5.0 * mm / s;
      --  Junction speed at a 90-degree corner. Shallower corners may be traversed faster and reversals still stop.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Cornering_Kind is (Stereographic, Circular, Parabolic, Biarc, Sharp_SCV)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Cornering (Kind : User_Config_Cornering_Kind := Stereographic) is record
      --  Select the geometric and junction-limit model used at path corners. Unsupported or uncertifiable corners
      --  always become exact stops rather than silently changing to another cornering method.

      case Kind is
         when Stereographic =>
            Stereographic_Params : User_Config_Cornering_Stereographic;

         when Circular =>
            Circular_Params : User_Config_Cornering_Circular;

         when Parabolic =>
            Parabolic_Params : User_Config_Cornering_Parabolic;

         when Biarc =>
            Biarc_Params : User_Config_Cornering_Biarc;

         when Sharp_SCV =>
            Sharp_SCV_Params : User_Config_Cornering_Sharp_SCV;
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Kinematics is record
      --  This section contains settings related to the machine's movement, geometry, and motion planning.

      Workspace_Bounds : User_Config_Workspace_Bounds := (others => <>);
      --  Allowed commanded coordinate space.

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

      Maximum_Tangential_Velocity : Velocity range 0.000_001 * mm / s .. 1.0E100 * mm / s := 10.0 * mm / s;
      --  This is the maximum combined speed of all axes. It's a global limit on the toolhead's speed. In most cases,
      --  it's better to set this to a very high value (e.g., `1E100`) and use the per-axis velocity limits below to
      --  control the speed of your machine.

      Axial_Velocity_Limits : User_Config_Axial_Velocity_Limits_Array := [others => 10.0 * mm / s];
      --  This sets the maximum speed for each individual axis.

      Axial_Acceleration_Limits : User_Config_Axial_Acceleration_Limits_Array := [others => 100.0 * mm / s ** 2];
      --  This sets the maximum acceleration for each individual axis. These values can be set to a very high value to
      --  effectively disable this limit and rely solely on the higher order constraints below.

      Axial_Jerk_Limits : User_Config_Axial_Jerk_Limits_Array := [others => 100.0E2 * mm / s ** 3];
      --  This sets the maximum jerk for each individual axis. Jerk is the rate of change of acceleration. A good
      --  starting point for tuning is to set the jerk to 100 times the maximum acceleration. This can be achieved by
      --  placing the max acceleration values in these fields and appending E2 to them.
      --
      --  This can be set to a very high value to effectively disable the limit.

      Axial_Snap_Limits : User_Config_Axial_Snap_Limits_Array := [others => 100.0E5 * mm / s ** 4];
      --  This sets the maximum snap for each individual axis. Snap is the rate of change of jerk. A good starting
      --  point for tuning is to set the snap to 100,000 times the maximum acceleration. This can be achieved by
      --  placing the max acceleration values in these fields and appending E5 to them.
      --
      --  This can be set to a very high value to effectively disable the limit.

      Axial_Crackle_Limits : User_Config_Axial_Crackle_Limits_Array := [others => 100.0E9 * mm / s ** 5];
      --  This sets the maximum crackle for each individual axis. Crackle is the rate of change of snap. A good
      --  starting point for tuning is to set the crackle to 1,000,000,000 times the maximum acceleration. This can be
      --  achieved by placing the max acceleration values in these fields and appending E9 to them.
      --
      --  This can be set to a very high value to effectively disable the limit.

      Cornering : User_Config_Cornering := (others => <>);
      --  Select the corner-transition model and configure the parameters relevant to that model.

      Kinematics_Kind : User_Config_Kinematics_Variant := (others => <>);
      --  This selects the kinematic layout and allows motors to be assigned to the axes they control.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Kinematics : User_Config_Kinematics := (others => <>);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;
   --  Build the configuration schema.

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;
   --  Convert validated configuration data.

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);
   --  Store the configuration in Data.

   function Planner_Workspace_Bounds (Config : User_Config_Workspace_Bounds) return Motion_Planner.Workspace_Bounds;
   --  Convert the configured workspace bounds to the representation used by the motion planner.

   function Hypot (X, Y : Length) return Length;

   procedure Workspace_Tower_Extents
     (Workspace      : User_Config_Workspace_Bounds;
      Tower_X        : Length;
      Tower_Y        : Length;
      Maximum_DX     : out Length;
      Maximum_DY     : out Length;
      Maximum_Radius : out Length);
   --  Return the maximum horizontal component distances and radial distance from a delta tower to any point in the
   --  configured workspace.

   function Build_Cornering_Parameters (Cornering : User_Config_Cornering) return Motion_Planner.Cornering_Parameters;
   --  Convert the selected user-config cornering branch to the corresponding planner parameters.

   type Axial_Update_Set is array (Axis_Name) of Boolean;

   type Runtime_Kinematics_Updates is record
      Has_Axial_Velocity_Limit     : Axial_Update_Set := [others => False];
      Axial_Velocity_Limits        : Axial_Velocities := [others => 1.0E-6 * mm / s];
      Has_Axial_Acceleration_Limit : Axial_Update_Set := [others => False];
      Axial_Acceleration_Limits    : Axial_Accelerations := [others => 1.0E-6 * mm / s ** 2];
      Has_Axial_Jerk_Limit         : Axial_Update_Set := [others => False];
      Axial_Jerk_Limits            : Axial_Jerks := [others => 1.0E-6 * mm / s ** 3];
      Has_Axial_Snap_Limit         : Axial_Update_Set := [others => False];
      Axial_Snap_Limits            : Axial_Snaps := [others => 1.0E-6 * mm / s ** 4];
      Has_Axial_Crackle_Limit      : Axial_Update_Set := [others => False];
      Axial_Crackle_Limits         : Axial_Crackles := [others => 1.0E-6 * mm / s ** 5];
   end record;

   type Kinematics_Config_Update is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Updates             : Runtime_Kinematics_Updates;
   end record;

   overriding
   procedure Process_After_Block (This : Kinematics_Config_Update; Context : Block_End_Context'Class);
   --  Apply kinematic-limit changes.

   function Build_Motion_Planner_Configuration
     (Config : User_Config; Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance_Interface'Class)
      return Motion_Planner_Configuration;
   --  Build the motion-planner configuration.

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
      C        : Gcode_Optional_Float
      --  Crackle limit in mm/s⁵. Not modified if not specified.
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
         Motor_Drivers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      procedure Apply_Runtime_Config (Updates : Runtime_Kinematics_Updates);

      overriding
      function Get_Default_Motion_Planner_Configuration return Motion_Planner_Configuration;

      overriding
      function Axis_Is_Motor_Separable (Axis : Axis_Name) return Boolean;
      overriding
      function Motor_Affects_Axis (Motor : Motor_Name; Axis : Axis_Name) return Boolean;
      overriding
      function Get_Homing_Configuration return Kinematics_Homing_Configuration;

      function Get_Config return User_Config;
   private
      Config                            : User_Config;
      Config_Data                       : Prunt.Config.Config_Data;
      Motor_Drivers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
   end Module_Instance;

end Prunt.Default_Modules.Kinematics;
