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
with Prunt.Default_Modules.Kinematics;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;
with Prunt.Motion_Planner;
with Prunt.Status_Manager;

generic
   with package Kinematics_Module is new Default_Modules.Kinematics (<>);
package Prunt.Default_Modules.Motion is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Pause_Handler with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

private

   type User_Config_Pause_Park_Kind is (No_Park_Move, Relative_Park_Move, Absolute_Park_Move)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Pause_Park_Out_Of_Bounds_Behavior is (Error_If_Out_Of_Bounds, Clip_To_Bounds)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Pause_Park_Absolute_Z_Kind is (Absolute_Z_Position, Relative_Z_Offset)
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Pause_Park_Absolute_Z (Kind : User_Config_Pause_Park_Absolute_Z_Kind := Absolute_Z_Position) is
   record
      --  Select whether absolute park mode uses an absolute Z position or a relative Z movement from the pause
      --  position.

      case Kind is
         when Absolute_Z_Position =>
            Z_Position : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
            --  Absolute Z position to move to while paused.

            Avoid_Lowering_Z : Boolean := True;
            --  If the absolute Z target is below the pause Z position, leave Z at the pause position instead of
            --  lowering it.

         when Relative_Z_Offset =>
            Z_Offset : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
            --  Relative Z movement from the pause position.
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Pause_Park_Relative_Park_Move is record
      Out_Of_Bounds_Behavior : User_Config_Pause_Park_Out_Of_Bounds_Behavior := Clip_To_Bounds;
      --  Select what happens if applying the relative pause offsets would place any axis outside the configured
      --  position limits.

      X_Offset : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Relative X movement from the pause position.

      Y_Offset : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Relative Y movement from the pause position.

      Z_Offset : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Relative Z movement from the pause position.

      E_Offset : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Relative E movement from the pause position.

      Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 50.0 * mm / s;
      --  Feedrate used for the pause park move.

      Return_Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 50.0 * mm / s;
      --  Feedrate used to return to the pause position before resuming.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Pause_Park_Absolute_Park_Move is record
      Out_Of_Bounds_Behavior : User_Config_Pause_Park_Out_Of_Bounds_Behavior := Clip_To_Bounds;
      --  Select what happens if the runtime-dependent parts of the pause position would place any axis outside the
      --  configured position limits.

      X_Position : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Absolute X position to move to while paused.

      Y_Position : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Absolute Y position to move to while paused.

      Z_Target : User_Config_Pause_Park_Absolute_Z := (others => <>);
      --  Z movement to perform while paused.

      E_Offset : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Relative E movement from the pause position.

      Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 50.0 * mm / s;
      --  Feedrate used for the pause park move.

      Return_Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 50.0 * mm / s;
      --  Feedrate used to return to the pause position before resuming.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Pause_Park (Kind : User_Config_Pause_Park_Kind := No_Park_Move) is record
      --  Configure the optional movement performed while the printer is paused.

      case Kind is
         when No_Park_Move =>
            --  Do not move the toolhead while paused.
            null;

         when Relative_Park_Move =>
            Relative_Park_Move : User_Config_Pause_Park_Relative_Park_Move := (others => <>);
            --  Relative movement to perform while paused.

         when Absolute_Park_Move =>
            Absolute_Park_Move : User_Config_Pause_Park_Absolute_Park_Move := (others => <>);
            --  Absolute movement to perform while paused.
      end case;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Motion_Gcode is record
      --  This section contains settings which impact G-code commands contained within the motion module.

      Replace_G0_With_G1 : Boolean := False;
      --  If set, replace all `G0` commands with `G1` during execution.
      --
      --  `G0` and `G1` are both commands for linear movement. Technically, `G0` is for rapid, non-printing moves,
      --  while `G1` is for controlled, printing moves. Some 3D printer firmwares treat `G0` and `G1` identically,
      --  using the same feedrate for both. Other firmwares, including Prunt by default, use the maximum possible speed
      --  for `G0` moves.
      --
      --  If your slicer generates G-code that assumes G0 moves will be performed at the same speed as `G1` moves, you
      --  should enable this setting.
      --
      --  Note that this setting only affects the movement speed. Laser-based tools will always be disabled during G0
      --  moves regardless of this setting.

      Default_G1_Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 0.1 * mm / s;
      --  Sets the default feedrate to use for G1 before a G1 command is executed with an F parameter.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Motion_Gcode : User_Config_Motion_Gcode := (others => <>);

      Pause_Park : User_Config_Pause_Park := (others => <>);
      --  Movement to perform while paused before waiting for resume.
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   procedure Add_Corner_If_Moved
     (Planner : Planner_Interface'Class; Current : in out Position; Target : Position; Feedrate : Velocity);
   --  Add Target to Planner and update Current, but only if Target differs from Current.

   function Bounds_Checked_Position
     (Target             : Position;
      Behavior           : User_Config_Pause_Park_Out_Of_Bounds_Behavior;
      Target_Description : String;
      Params             : Motion_Planner.Kinematic_Parameters) return Position;
   --  Return Target clipped to Params bounds, or raise Constraint_Error if Behavior requires an error.

   function Park_Position
     (Config : User_Config_Pause_Park; Pause_Position : Position; Params : Motion_Planner.Kinematic_Parameters)
      return Position
   with Pre => Config.Kind in Relative_Park_Move | Absolute_Park_Move;
   --  Resolve Config into the actual pause park target, including bounds handling and the Z lowering guard.

   function Park_Feedrate (Config : User_Config_Pause_Park) return Velocity
   with Pre => Config.Kind in Relative_Park_Move | Absolute_Park_Move;
   --  Return the feedrate to use while moving from the pause position to the park position.

   function Park_Return_Feedrate (Config : User_Config_Pause_Park) return Velocity
   with Pre => Config.Kind in Relative_Park_Move | Absolute_Park_Move;
   --  Return the feedrate to use while moving back from the park position to the pause position.

   procedure Rapid_Linear_Move
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float)
   with Annotate => (Prunt_Config, Gcode_Command, "G0");

   procedure Linear_Move
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float)
   with Annotate => (Prunt_Config, Gcode_Command, "G1");

   procedure Clockwise_Arc_Move_Offset_Form
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float;
      I       : Dimensionless;
      J       : Dimensionless)
   with Annotate => (Prunt_Config, Gcode_Command, "G2");

   procedure Clockwise_Arc_Move_Radius_Form
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float;
      R       : Dimensionless)
   with Annotate => (Prunt_Config, Gcode_Command, "G2");

   procedure Counter_Clockwise_Arc_Move_Offset_Form
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float;
      I       : Dimensionless;
      J       : Dimensionless)
   with Annotate => (Prunt_Config, Gcode_Command, "G3");

   procedure Counter_Clockwise_Arc_Move_Radius_Form
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float;
      R       : Dimensionless)
   with Annotate => (Prunt_Config, Gcode_Command, "G3");

   procedure Retract (This : Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G10");

   procedure Recover (This : Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G11");

   procedure Millimeter_Units (This : Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G21");

   procedure Report_Stored_Positions (This : Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G60");

   procedure Save_Current_Position
     (This : Module_Instance; Planner : Planner_Interface'Class; S : Gcode_Arguments.Argument_Integer)
   with Annotate => (Prunt_Config, Gcode_Command, "G60");

   procedure Delete_Stored_Position
     (This : Module_Instance; Planner : Planner_Interface'Class; D : Gcode_Arguments.Argument_Integer)
   with Annotate => (Prunt_Config, Gcode_Command, "G60");

   procedure Delete_All_Stored_Positions
     (This : Module_Instance; Planner : Planner_Interface'Class; D : Gcode_No_Value)
   with Annotate => (Prunt_Config, Gcode_Command, "G60");

   procedure Restore_Saved_Position_G60
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      Q       : Gcode_Arguments.Argument_Integer;
      F       : Gcode_Optional_Float;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float)
   with Annotate => (Prunt_Config, Gcode_Command, "G60");

   procedure Return_To_Saved_Position
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      F       : Gcode_Optional_Float;
      S       : Gcode_Arguments.Argument_Integer;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float)
   with Annotate => (Prunt_Config, Gcode_Command, "G61");

   procedure Absolute_Positioning (This : Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G90");

   procedure Relative_Positioning (This : Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G91");

   procedure Set_Virtual_Position
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float)
   with Annotate => (Prunt_Config, Gcode_Command, "G92");

   procedure E_Axis_Absolute (This : Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M82");

   procedure E_Axis_Relative (This : Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M83");

   procedure Retraction_Settings
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      F       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float)
   with Annotate => (Prunt_Config, Gcode_Command, "M207");

   procedure Recover_Settings
     (This : Module_Instance; Planner : Planner_Interface'Class; F : Gcode_Optional_Float; S : Gcode_Optional_Float)
   with Annotate => (Prunt_Config, Gcode_Command, "M208");

   procedure Set_Auto_Retract (This : Module_Instance; Planner : Planner_Interface'Class; S : Gcode_Optional_Float)
   with Annotate => (Prunt_Config, Gcode_Command, "M209");

   procedure Set_Feedrate_Percentage
     (This : Module_Instance; Planner : Planner_Interface'Class; S : Gcode_Optional_Float)
   with Annotate => (Prunt_Config, Gcode_Command, "M220");

   procedure Set_Flow_Percentage (This : Module_Instance; Planner : Planner_Interface'Class; S : Gcode_Optional_Float)
   with Annotate => (Prunt_Config, Gcode_Command, "M221");

   protected type Module_Instance is new My_Modules.Module_Instance and Pause_Handler with
      procedure Initialize (Config_In : User_Config; Status_Emitter_In : Status_Manager.Status_Emitter);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      overriding
      procedure Handle_Pause (Planner : Planner_Interface'Class; Context : Pause_Context'Class);

      overriding
      procedure Handle_Resume (Planner : Planner_Interface'Class; Context : Pause_Context'Class);

      function Get_Config return User_Config;

      function Get_Feedrate return Velocity;

      procedure Set_Feedrate (Value : Velocity);
   private
      Config         : User_Config;
      Self_Ref       : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
      Status_Emitter : Status_Manager.Status_Emitter;
      Feedrate       : Velocity;
   end Module_Instance;

end Prunt.Default_Modules.Motion;
