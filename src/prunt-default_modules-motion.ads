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
with Prunt.Module_Types; use Prunt.Module_Types;
with Prunt.Motion_Planner;
with Prunt.Status_Manager;
private with Prunt.Bounded_Indefinite_Queues;
private with System.Storage_Elements;

generic
   with package Config_Saving_Module is new Default_Modules.Config_Saving;
   with package Kinematics_Module is new Default_Modules.Kinematics (<>);
   Pending_State_Queue_Length : Motion_Planner.Max_Corners_Type;
package Prunt.Default_Modules.Motion is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Module_Instance (<>) is synchronized
     new My_Modules.Module_Instance
     and Pause_Handler
     and Planner_State_Handler
     and Config_Save_Preparer
     and Cancellation_Handler with private;

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

   use type System.Storage_Elements.Storage_Count;

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

      Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 10.0 * mm / s;
      --  Feedrate used for the pause park move.

      Return_Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 10.0 * mm / s;
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

      Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 10.0 * mm / s;
      --  Feedrate used for the pause park move.

      Return_Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 10.0 * mm / s;
      --  Feedrate used to return to the pause position before resuming.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config_Pause_Park (Kind : User_Config_Pause_Park_Kind := No_Park_Move) is record
      --  Configure the optional movement performed when the printer is paused.

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

   type Linear_Units_Mode is (Millimeter_Units_Mode, Inch_Units_Mode) with Annotate => (Prunt_Config, User_Config);

   type Positioning_Mode is (Absolute_Positioning_Mode, Relative_Positioning_Mode)
   with Annotate => (Prunt_Config, User_Config);

   type E_Positioning_Mode is (Follow_XYZ_Positioning_Mode, Absolute_E_Positioning_Mode, Relative_E_Positioning_Mode)
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
      --  If your slicer generates G-code that assumes `G0` moves will be performed at the same speed as `G1` moves,
      --  you should enable this setting.
      --
      --  Note that this setting only affects the movement speed. Laser-based tools will always be disabled during `G0`
      --  moves regardless of this setting.

      Default_G1_Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 0.1 * mm / s;
      --  Sets the default feedrate to use for `G1` before a `G1` command is executed with an `F` parameter.

      Default_Units : Linear_Units_Mode := Millimeter_Units_Mode;
      --  Sets the input unit mode used before a `G20` or `G21` command is executed.

      Default_Positioning : Positioning_Mode := Absolute_Positioning_Mode;
      --  Sets the XYZ positioning mode used before a `G90` or `G91` command is executed.

      Default_E_Positioning : E_Positioning_Mode := Follow_XYZ_Positioning_Mode;
      --  Sets the E-axis positioning mode used before a `G90`, `G91`, `M82`, or `M83` command is executed.

      Default_G92_X_Offset : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Initial logical X offset applied before a `G92` command is executed.

      Default_G92_Y_Offset : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Initial logical Y offset applied before a `G92` command is executed.

      Default_G92_Z_Offset : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Initial logical Z offset applied before a `G92` command is executed.

      Default_G92_E_Offset : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Initial logical E offset applied before a `G92` command is executed.

      Default_Auto_Retract_Enabled : Boolean := False;
      --  Sets whether automatic retract detection is enabled before an `M209` command is executed.

      Default_Feedrate_Scale : Dimensionless range 1.0E-100 .. 1.0E100 := 1.0;
      --  Sets the initial feedrate scale used before an `M220` command is executed. A value of 1.0 means 100%.

      Default_Flow_Scale : Dimensionless range 1.0E-100 .. 1.0E100 := 1.0;
      --  Sets the initial E-axis flow scale used before an `M221` command is executed. A value of 1.0 means 100%.

      Firmware_Retract_Length : Length range 0.0 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Default length used by `G10` firmware retract moves.

      Firmware_Retract_Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 45.0 * mm / s;
      --  Default feedrate used by `G10` firmware retract moves.

      Firmware_Retract_Z_Lift : Length range 0.0 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Default Z lift used by `G10`.

      Firmware_Recover_Extra_Length : Length range -1.0E100 * mm .. 1.0E100 * mm := 0.0 * mm;
      --  Default additional length recovered by `G11` after restoring the firmware retract length. This is added to
      --  the retract distance. A negative value will cause the recovery distance to be shorter than the retraction
      --  distance.

      Firmware_Recover_Feedrate : Velocity range 1.0E-100 * mm / s .. 1.0E100 * mm / s := 8.0 * mm / s;
      --  Default feedrate used by `G11` firmware recover moves.

      Auto_Retract_Min_Length : Length range 0.0 * mm .. 1.0E100 * mm := 0.1 * mm;
      --  Minimum E-only move length converted to firmware retract/recover when `M209` is enabled.

      Auto_Retract_Max_Length : Length range 0.0 * mm .. 1.0E100 * mm := 10.0 * mm;
      --  Maximum E-only move length converted to firmware retract/recover when `M209` is enabled.
   end record
   with Annotate => (Prunt_Config, User_Config);

   type User_Config is record
      Motion_Gcode : User_Config_Motion_Gcode := (others => <>);
      Pause_Park   : User_Config_Pause_Park := (others => <>);
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config);

   type Saved_Position is record
      Present : Boolean := False;
      Pos     : Position := [others => 0.0 * mm];
   end record;

   type Saved_Position_Array is array (Gcode_Arguments.Argument_Integer) of Saved_Position;

   type Motion_State is record
      Feedrate              : Velocity := 0.1 * mm / s;
      Units                 : Linear_Units_Mode := Millimeter_Units_Mode;
      Positioning           : Positioning_Mode := Absolute_Positioning_Mode;
      E_Positioning         : E_Positioning_Mode := Follow_XYZ_Positioning_Mode;
      G92_Offset            : Position_Offset := [others => 0.0 * mm];
      Feedrate_Scale        : Dimensionless := 1.0;
      Backup_Feedrate_Scale : Dimensionless := 1.0;
      Flow_Scale            : Dimensionless := 1.0;
      Retract_Length        : Length := 0.0 * mm;
      Retract_Feedrate      : Velocity := 1.0 * mm / s;
      Retract_Z_Lift        : Length := 0.0 * mm;
      Recover_Extra_Length  : Length := 0.0 * mm;
      Recover_Feedrate      : Velocity := 1.0 * mm / s;
      Auto_Retract_Enabled  : Boolean := False;
      Is_Retracted          : Boolean := False;
      Current_Z_Hop         : Length := 0.0 * mm;
   end record;

   type Pending_State_Snapshot is record
      Anchor_ID : Planner_Corner_ID := 0;
      State     : Motion_State;
   end record;

   Pending_State_Queue_Storage_Size : constant System.Storage_Elements.Storage_Count :=
     System.Storage_Elements.Storage_Count (Pending_State_Queue_Length)
     * System.Storage_Elements.Storage_Count (Pending_State_Snapshot'Max_Size_In_Storage_Elements);

   package Pending_State_Queues is new
     Prunt.Bounded_Indefinite_Queues
       (Element_Type => Pending_State_Snapshot,
        Storage_Size => Pending_State_Queue_Storage_Size);

   type Stored_Position_Update_Event_Kind is
     (Save_Stored_Position, Delete_Stored_Position, Delete_All_Stored_Positions);

   type Stored_Position_Update_Event (Kind : Stored_Position_Update_Event_Kind := Save_Stored_Position) is record
      Anchor_ID : Planner_Corner_ID := 0;

      case Kind is
         when Save_Stored_Position =>
            Saved_Slot     : Gcode_Arguments.Argument_Integer := 0;
            Saved_Position : Position := [others => 0.0 * mm];

         when Delete_Stored_Position =>
            Deleted_Slot : Gcode_Arguments.Argument_Integer := 0;

         when Delete_All_Stored_Positions =>
            null;
      end case;
   end record;

   Pending_Stored_Position_Update_Queue_Storage_Size : constant System.Storage_Elements.Storage_Count :=
     100 * System.Storage_Elements.Storage_Count (Stored_Position_Update_Event'Max_Size_In_Storage_Elements);

   package Pending_Stored_Position_Update_Queues is new
     Prunt.Bounded_Indefinite_Queues
       (Element_Type => Stored_Position_Update_Event,
        Storage_Size => Pending_Stored_Position_Update_Queue_Storage_Size);

   procedure Apply_Stored_Position_Update
     (Stored_Positions : in out Saved_Position_Array; Update : Stored_Position_Update_Event);
   --  Apply one queued saved-position action to the supplied planned or committed saved-position array.

   function Stored_Position_Update_Changes
     (Stored_Positions : Saved_Position_Array; Update : Stored_Position_Update_Event) return Boolean;
   --  Return True if applying Update would change Stored_Positions, so no-op actions are not queued.

   procedure Add_Corner_If_Moved
     (Planner : Planner_Interface'Class; Current : in out Position; Target : Position; Feedrate : Velocity);
   --  Add Target to Planner and update Current, but only if Target differs from Current.

   function Unit_Scale (Units : Linear_Units_Mode) return Length;
   --  Return the length represented by one g-code unit in Units.

   function To_Current_Units_Length (Value : Dimensionless; Units : Linear_Units_Mode) return Length;
   --  Convert a g-code length argument in Units to an internal length.

   function To_Current_Units_Feedrate (Value : Dimensionless; Units : Linear_Units_Mode) return Velocity;
   --  Convert a g-code feedrate argument in Units per minute to an internal velocity.

   function E_Is_Relative (Positioning : Positioning_Mode; E_Positioning : E_Positioning_Mode) return Boolean;
   --  Return True if the current E-axis modal state makes E movement relative.

   function Optional_Float_Length (Value : Gcode_Optional_Float_Or_No_Value; Units : Linear_Units_Mode) return Length;
   --  Convert a present optional-or-bare g-code float to a length, treating omitted and bare values as zero.

   procedure Update_Status (Status_Emitter : Status_Manager.Status_Emitter; State : Motion_State);
   --  Publish all committed motion state values through Status_Emitter.

   function Logical_Position_From_Physical
     (Physical_Position : Position; G92_Offset : Position_Offset; Current_Z_Hop : Length) return Position;
   --  Convert a planner-space physical position to logical g-code space.

   function Bounds_Checked_Position
     (Target             : Position;
      Behavior           : User_Config_Pause_Park_Out_Of_Bounds_Behavior;
      Target_Description : String;
      Params             : Motion_Planner.Kinematic_Parameters) return Position;
   --  Return Target clipped to Params bounds, or raise if Behavior requires an error.

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

   function Position_Report (Prefix : String; Pos : Position; Units : Linear_Units_Mode) return Virtual_String;

   type Motion_Report_Event is new Extra_Block_Resetting_Data with record
      Message : Virtual_String;
   end record;

   overriding
   procedure Process_After_Block (This : Motion_Report_Event; Context : Block_End_Context'Class);

   procedure Rapid_Linear_Move
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_Float;
      --  Absolute or relative X coordinate in the current units.
      Y        : Gcode_Optional_Float;
      --  Absolute or relative Y coordinate in the current units.
      Z        : Gcode_Optional_Float;
      --  Absolute or relative Z coordinate in the current units.
      E        : Gcode_Optional_Float;
      --  Absolute or relative E coordinate in the current units.
      F        : Gcode_Optional_Float
      --  Feedrate in current units per minute for this rapid move only.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G0");
   --  Perform a non-print linear move. Axes which are not specified will not move. Moves at the maximum feedrate if
   --  feedrate is not specified.
   --
   --  Prunt contains a setting `Replace G0 with G1` which will replace all `G0` commands with `G1` commands if set.
   --  This emulates the behaviour seen in some other motion controllers, including Marlin. This setting is off by
   --  default.
   --
   --  All specified axes must be homed and the target position must be within the limits defined in the printer
   --  configuration or else an error will be raised.
   --
   --  The `G90`, `G91`, `M82`, and `M83` are used to switch between relative and absolute mode.
   --
   --  The `ABCUVW` parameters are not present as Prunt does not support these axes. The `S` parameter is not present
   --  as laser tools are not yet supported. These parameters are present in Marlin.

   procedure Linear_Move
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_Float;
      --  Absolute or relative X coordinate in the current units.
      Y        : Gcode_Optional_Float;
      --  Absolute or relative Y coordinate in the current units.
      Z        : Gcode_Optional_Float;
      --  Absolute or relative Z coordinate in the current units.
      E        : Gcode_Optional_Float;
      --  Absolute or relative E coordinate in the current units.
      F        : Gcode_Optional_Float
      --  Feedrate in current units per minute. Positive values update the stored feedrate immediately.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G1");
   --  Perform a linear move. Axes which are not specified will not move. Moves at the same feedrate as the last `G1`
   --  command if feedrate is not specified.
   --
   --  All specified axes must be homed and the target position must be within the limits defined in the printer
   --  configuration or else an error will be raised.
   --
   --  The `G90`, `G91`, `M82`, and `M83` are used to switch between relative and absolute mode.
   --
   --  The `ABCUVW` parameters are not present as Prunt does not support these axes. The `S` parameter is not present
   --  as laser tools are not yet supported. These parameters are present in Marlin.

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

   procedure Retract
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Gcode_Optional_Integer)
   with Annotate => (Prunt_Config, Gcode_Command, "G10");
   --  Perform a retraction move with the values specified by the last `M207` command. Multiple `G10` commands without
   --  a `G11` command between them are ignored.
   --
   --  `S` is accepted for Marlin compatibility, but only omitted or `S0` is supported because Prunt currently has one
   --  extruder and no swap retract.

   procedure Recover
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G11");
   --  Perform a recovery move with the values specified by the last `M207` and `M208` commands. Multiple `G11`
   --  commands without a `G10` command between them are ignored.

   procedure Inch_Units
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G20");
   --  Set motion input units to inches. Only motion-module coordinates and feedrates are affected.

   procedure Millimeter_Units
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G21");
   --  Set motion input units to millimeters. Only motion-module coordinates and feedrates are affected.

   procedure Save_Current_Position
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Gcode_Integer_Or_No_Value
      --  Slot to save the current planner-space position to. Bare `S` uses slot zero.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G60");
   --  Save the current physical position to a stored-position slot.

   procedure Delete_Stored_Position
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      D        : Gcode_Integer_Or_No_Value
      --  Slot to delete, or bare `D` to delete all slots.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G60");
   --  Delete one or all stored-position slots.

   procedure Move_To_Stored_Position
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      Q        : Gcode_Integer_Or_No_Value;
      --  Slot to restore using `G61` behavior. Bare `Q` uses slot zero.
      F        : Gcode_Optional_Float;
      --  Optional restore feedrate in current units per minute.
      X        : Gcode_Optional_Float_Or_No_Value;
      --  Optional X restore offset in current units. Bare `X` means zero offset.
      Y        : Gcode_Optional_Float_Or_No_Value;
      --  Optional Y restore offset in current units. Bare `Y` means zero offset.
      Z        : Gcode_Optional_Float_Or_No_Value;
      --  Optional Z restore offset in current units. Bare `Z` means zero offset.
      E        : Gcode_Optional_Float_Or_No_Value
      --  Optional E restore offset in current units. Bare `E` means zero offset.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G60");
   --  Move to a stored position using `G61` behavior.

   procedure Report_Stored_Positions
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G60");
   --  Report all stored positions.

   procedure Return_To_Saved_Position
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      F        : Gcode_Optional_Float;
      --  Optional restore feedrate in current units per minute.
      S        : Gcode_Optional_Integer_Or_No_Value;
      --  Slot to restore. Omitted or bare `S` uses slot zero.
      X        : Gcode_Optional_Float_Or_No_Value;
      --  Optional X restore offset in current units. Bare `X` means zero offset.
      Y        : Gcode_Optional_Float_Or_No_Value;
      --  Optional Y restore offset in current units. Bare `Y` means zero offset.
      Z        : Gcode_Optional_Float_Or_No_Value;
      --  Optional Z restore offset in current units. Bare `Z` means zero offset.
      E        : Gcode_Optional_Float_Or_No_Value
      --  Optional E restore offset in current units. Bare `E` means zero offset.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G61");
   --  Restore a saved physical position. If no axes are specified, `XYZ` are moved to the stored position and `E` is
   --  set logically without physical extrusion. If axes are specified, only those axes are restored.

   procedure Absolute_Positioning
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G90");
   --  Use absolute positioning for `XYZ` and `E`, clearing `M82`/`M83` E-axis overrides.

   procedure Relative_Positioning
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G91");
   --  Use relative positioning for `XYZ` and `E`, clearing `M82`/`M83` E-axis overrides.

   procedure Set_Virtual_Position
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_Float;
      --  New logical X coordinate in current units.
      Y        : Gcode_Optional_Float;
      --  New logical Y coordinate in current units.
      Z        : Gcode_Optional_Float;
      --  New logical Z coordinate in current units.
      E        : Gcode_Optional_Float
      --  New logical E coordinate in current units.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G92");
   --  Set logical g-code position without physical motion.

   procedure E_Axis_Absolute
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M82");
   --  Override E axis to absolute positioning until `G90` or `G91`.

   procedure E_Axis_Relative
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M83");
   --  Override E axis to relative positioning until `G90` or `G91`.

   procedure Retraction_Settings
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      F        : Gcode_Optional_Float;
      --  Retract feedrate in current units per minute.
      S        : Gcode_Optional_Float;
      --  Firmware retract length in current units.
      Z        : Gcode_Optional_Float
      --  Z lift in current units.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M207");
   --  Set or report firmware retract settings. No arguments reports the current settings.
   --
   --  Saved by `M500`.

   procedure Recover_Settings
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      F        : Gcode_Optional_Float;
      --  Recover feedrate in current units per minute.
      S        : Gcode_Optional_Float
      --  Additional recover length in current units.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M208");
   --  Set or report firmware recover settings. No arguments reports the current settings.
   --
   --  Saved by `M500`.

   procedure Set_Auto_Retract
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Gcode_Optional_Integer)
   with Annotate => (Prunt_Config, Gcode_Command, "M209");
   --  Set or report automatic retract detection. With `S1` to `S999`, qualifying E-only moves are converted to
   --  firmware retractions. Use `S0` to disable.

   procedure Set_Feedrate_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Dimensionless
      --  New feedrate percentage.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M220");
   --  Set feedrate percentage.

   procedure Backup_Feedrate_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      B        : Gcode_No_Value)
   with Annotate => (Prunt_Config, Gcode_Command, "M220");
   --  Backup the current feedrate percentage.

   procedure Restore_Feedrate_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      R        : Gcode_No_Value)
   with Annotate => (Prunt_Config, Gcode_Command, "M220");
   --  Restore the backed-up feedrate percentage.

   procedure Report_Feedrate_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M220");
   --  Report feedrate percentage.

   procedure Set_Flow_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Dimensionless
      --  New flow percentage for future E deltas.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M221");
   --  Set flow percentage.

   procedure Report_Flow_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M221");
   --  Report flow percentage.

   protected type Module_Instance is new My_Modules.Module_Instance
   and Pause_Handler
   and Planner_State_Handler
   and Config_Save_Preparer
   and Cancellation_Handler with
      procedure Initialize
        (Config_In         : User_Config;
         Config_Data_In    : Prunt.Config.Config_Data;
         Status_Emitter_In : Status_Manager.Status_Emitter);

      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      overriding
      procedure Handle_Pause (Planner : Planner_Interface'Class; Context : Pause_Context'Class);

      overriding
      procedure Handle_Resume (Planner : Planner_Interface'Class; Context : Pause_Context'Class);

      overriding
      procedure Catch_Up_Planner_State (Executed_Corner_ID : Planner_Corner_ID);

      overriding
      procedure Prepare_Config_For_Save;

      overriding
      procedure Handle_Cancel
        (Executed_Corner_ID      : Planner_Corner_ID;
         Cancellation_Barrier_ID : Planner_Corner_ID;
         Current_Position        : Position);

      procedure Execute_Linear_Move
        (Planner : Planner_Interface'Class;
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         F       : Gcode_Optional_Float;
         Rapid   : Boolean);

      procedure Execute_Retract (Planner : Planner_Interface'Class; S : Gcode_Optional_Integer);

      procedure Execute_Recover (Planner : Planner_Interface'Class);

      procedure Set_Inch_Units (Planner : Planner_Interface'Class);

      procedure Set_Millimeter_Units (Planner : Planner_Interface'Class);

      procedure Execute_Save_Current_Position (Planner : Planner_Interface'Class; S : Gcode_Integer_Or_No_Value);

      procedure Execute_Delete_Stored_Position (Planner : Planner_Interface'Class; D : Gcode_Integer_Or_No_Value);

      procedure Execute_Return_To_Saved_Position
        (Planner : Planner_Interface'Class;
         F       : Gcode_Optional_Float;
         S       : Gcode_Optional_Integer_Or_No_Value;
         X       : Gcode_Optional_Float_Or_No_Value;
         Y       : Gcode_Optional_Float_Or_No_Value;
         Z       : Gcode_Optional_Float_Or_No_Value;
         E       : Gcode_Optional_Float_Or_No_Value);

      procedure Set_Absolute_Positioning (Planner : Planner_Interface'Class);

      procedure Set_Relative_Positioning (Planner : Planner_Interface'Class);

      procedure Set_Virtual_Position_State
        (Planner : Planner_Interface'Class;
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float);

      procedure Set_E_Axis_Absolute (Planner : Planner_Interface'Class);

      procedure Set_E_Axis_Relative (Planner : Planner_Interface'Class);

      procedure Apply_Retraction_Settings
        (Planner : Planner_Interface'Class;
         F       : Gcode_Optional_Float;
         S       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float);

      procedure Apply_Recover_Settings
        (Planner : Planner_Interface'Class; F : Gcode_Optional_Float; S : Gcode_Optional_Float);

      procedure Set_Auto_Retract_State (Planner : Planner_Interface'Class; S : Gcode_Arguments.Argument_Integer);

      procedure Apply_Set_Feedrate_Percentage (Planner : Planner_Interface'Class; S : Dimensionless);

      procedure Apply_Backup_Feedrate_Percentage (Planner : Planner_Interface'Class);

      procedure Apply_Restore_Feedrate_Percentage (Planner : Planner_Interface'Class);

      procedure Apply_Set_Flow_Percentage (Planner : Planner_Interface'Class; S : Dimensionless);

      function Stored_Positions_Report return Virtual_String;

      function Retraction_Settings_Report return Virtual_String;

      function Recover_Settings_Report return Virtual_String;

      function Auto_Retract_Report return Virtual_String;

      function Feedrate_Scale_Report return Virtual_String;

      function Flow_Scale_Report return Virtual_String;
   private
      procedure Ensure_Can_Queue_Planned_State (Planner : Planner_Interface'Class; Pending_Snapshots : Positive := 1);
      --  Use before touching planner state or module state other than Planned_State when the command may later queue
      --  Planned_State. If the pending state queue is full, this restores Planned_State to Last_Queued_State before
      --  raising Gcode_Temporarily_Rejected_Error.

      procedure Maybe_Queue_Planned_State (Planner : Planner_Interface'Class);
      --  Queue Planned_State if it differs from Last_Queued_State. If the pending state queue is full, this restores
      --  Planned_State to Last_Queued_State before raising Gcode_Temporarily_Rejected_Error. Callers relying on this
      --  rollback must only have changed Planned_State since the last successfully queued state.

      procedure Queue_Stored_Position_Update
        (Planner : Planner_Interface'Class; Update : Stored_Position_Update_Event);
      --  Apply Update to Planned_Stored_Positions and queue it for the committed array. If the pending update queue is
      --  full, Planned_Stored_Positions is left unchanged and Gcode_Temporarily_Rejected_Error is raised.

      Planned_State                   : Motion_State;
      Last_Queued_State               : Motion_State;
      Committed_State                 : Motion_State;
      Planned_Stored_Positions        : Saved_Position_Array := [others => <>];
      Committed_Stored_Positions      : Saved_Position_Array := [others => <>];
      Committed_Corner_ID             : Planner_Corner_ID := 0;
      Pending_States                  : Pending_State_Queues.Queue;
      Pending_Stored_Position_Updates : Pending_Stored_Position_Update_Queues.Queue;
      Config                          : User_Config;
      Config_Data                     : Prunt.Config.Config_Data;
      Status_Emitter                  : Status_Manager.Status_Emitter;
   end Module_Instance;

end Prunt.Default_Modules.Motion;
