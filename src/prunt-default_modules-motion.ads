-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

pragma Extensions_Allowed (On);

with Ada.Tags;
with Prunt.Config;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;
with Prunt.Status_Manager;

generic
package Prunt.Default_Modules.Motion is

   type Module is new My_Modules.Module with null record;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Module_Instance (<>) is new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

private

   type User_Config_Motion_Gcode is record
      --  This section contains settings which impact G-code commands contained within the motion module.

      Replace_G0_With_G1 : Boolean := False;
      --  If set, replace all G0 commands with G1 during execution.
      --
      --  G0 and G1 are both commands for linear movement. Technically, G0 is for rapid, non-printing moves, while G1
      --  is for controlled, printing moves. Some 3D printer firmwares treat G0 and G1 identically, using the same
      --  feedrate for both. Other firmwares, including Prunt by default, use the maximum possible speed for G0 moves.
      --
      --  If your slicer generates G-code that assumes G0 moves will be performed at the same speed as G1 moves, you
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
   end record
   with Annotate => (Prunt_Config, Root_User_Config);

   procedure Rapid_Linear_Move
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      --  X axis target position in mm, or offset in relative move mode.
      Y       : Gcode_Optional_Float;
      --  Y axis target position in mm, or offset in relative move mode.
      Z       : Gcode_Optional_Float;
      --  Z axis target position in mm, or offset in relative move mode.
      E       : Gcode_Optional_Float;
      --  E axis target position in mm, or offset in relative move mode.
      F       : Gcode_Optional_Float
      --  Feedrate in mm/min, maximum if not specified.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G0");
   --  Perform a non-print linear move. Axes which are not specified will not move. Moves at the maximum feedrate if
   --  feedrate is not specified.
   --
   --  Prunt contains a setting under Prunt > Replace G0 with G1 which will replace all G0 commands with G1 commands if
   --  set. This emulates the behaviour seen in some other motion controllers, including Marlin. This setting is off by
   --  default.
   --
   --  All specified axes must be homed and the complete move must be within the limits defined in the printer
   --  configuration or else an error will be raised.
   --
   --  The G90, G91, M82, and M83 commands are used to switch between relative and absolute mode.

   procedure Linear_Move
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      --  X axis target position in mm, or offset in relative move mode.
      Y       : Gcode_Optional_Float;
      --  Y axis target position in mm, or offset in relative move mode.
      Z       : Gcode_Optional_Float;
      --  Z axis target position in mm, or offset in relative move mode.
      E       : Gcode_Optional_Float;
      --  E axis target position in mm, or offset in relative move mode.
      F       : Gcode_Optional_Float
      --  Feedrate in mm/min, from last G1, G2, or G3 if not specified.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G1");
   --  Perform a linear move. Axes which are not specified will not move. Moves at the same feedrate as the last G1
   --  command if feedrate is not specified.
   --
   --  All specified axes must be homed and the complete move must be within the limits defined in the printer
   --  configuration or else an error will be raised.
   --
   --  The G90, G91, M82, and M83 commands are used to switch between relative and absolute mode.

   procedure Clockwise_Arc_Move_Offset_Form
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      --  X axis target position in mm, or offset in relative move mode.
      Y       : Gcode_Optional_Float;
      --  Y axis target position in mm, or offset in relative move mode.
      Z       : Gcode_Optional_Float;
      --  Z axis target position in mm, or offset in relative move mode. If different from the current Z, the arc
      --  becomes a helix with Z interpolating linearly over the arc.
      E       : Gcode_Optional_Float;
      --  E axis target position in mm, or offset in relative move mode.
      F       : Gcode_Optional_Float;
      --  Feedrate in mm/min, from last G1, G2, or G3 if not specified.
      I       : Dimensionless;
      --  X offset from the current position to the arc centre in mm.
      J       : Dimensionless
      --  Y offset from the current position to the arc centre in mm.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G2");
   --  Perform a clockwise arc move in the XY plane. The endpoint is specified by X and Y, and the arc centre is
   --  specified by I and J offsets from the current position.
   --
   --  If just one of X or Y is specified then the command will act as if the value of that parameter were zero in
   --  relative movement mode, as with G0 and G1. If neither X nor Y are specified, a complete circle is performed.
   --
   --  All specified axes must be homed and the arc must be fully within the limits defined in the printer
   --  configuration or else an error will be raised.
   --
   --  The G90, G91, M82, and M83 commands are used to switch between relative and absolute mode.

   procedure Clockwise_Arc_Move_Radius_Form
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      --  X axis target position in mm, or offset in relative move mode.
      Y       : Gcode_Optional_Float;
      --  Y axis target position in mm, or offset in relative move mode.
      Z       : Gcode_Optional_Float;
      --  Z axis target position in mm, or offset in relative move mode. If different from the current Z, the arc
      --  becomes a helix with Z interpolating linearly over the arc.
      E       : Gcode_Optional_Float;
      --  E axis target position in mm, or offset in relative move mode.
      F       : Gcode_Optional_Float;
      --  Feedrate in mm/min, from last G1, G2, or G3 if not specified.
      R       : Dimensionless
      --  Arc radius in mm.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G2");
   --  Perform a clockwise arc move in the XY plane. The endpoint is specified by X and Y, and the arc centre is
   --  specified by R as the arc radius.
   --
   --  If just one of X or Y is specified then the command will act as if the value of that parameter were zero in
   --  relative movement mode, as with G0 and G1. If neither X nor Y are specified, a complete circle is performed.
   --
   --  All specified axes must be homed and the arc must be fully within the limits defined in the printer
   --  configuration or else an error will be raised.
   --
   --  The G90, G91, M82, and M83 commands are used to switch between relative and absolute mode.

   procedure Counter_Clockwise_Arc_Move_Offset_Form
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      --  X axis target position in mm, or offset in relative move mode.
      Y       : Gcode_Optional_Float;
      --  Y axis target position in mm, or offset in relative move mode.
      Z       : Gcode_Optional_Float;
      --  Z axis target position in mm, or offset in relative move mode. If different from the current Z, the arc
      --  becomes a helix with Z interpolating linearly over the arc.
      E       : Gcode_Optional_Float;
      --  E axis target position in mm, or offset in relative move mode.
      F       : Gcode_Optional_Float;
      --  Feedrate in mm/min, from last G1, G2, or G3 if not specified.
      I       : Dimensionless;
      --  X offset from the current position to the arc centre in mm.
      J       : Dimensionless
      --  Y offset from the current position to the arc centre in mm.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G3");
   --  Perform a counter-clockwise arc move in the XY plane. The endpoint is specified by X and Y, and the arc centre
   --  is specified by I and J offsets from the current position.
   --
   --  If just one of X or Y is specified then the command will act as if the value of that parameter were zero in
   --  relative movement mode, as with G0 and G1. If neither X nor Y are specified, a complete circle is performed.
   --
   --  All specified axes must be homed and the arc must be fully within the limits defined in the printer
   --  configuration or else an error will be raised.
   --
   --  The G90, G91, M82, and M83 commands are used to switch between relative and absolute mode.

   procedure Counter_Clockwise_Arc_Move_Radius_Form
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      --  X axis target position in mm, or offset in relative move mode.
      Y       : Gcode_Optional_Float;
      --  Y axis target position in mm, or offset in relative move mode.
      Z       : Gcode_Optional_Float;
      --  Z axis target position in mm, or offset in relative move mode. If different from the current Z, the arc
      --  becomes a helix with Z interpolating linearly over the arc.
      E       : Gcode_Optional_Float;
      --  E axis target position in mm, or offset in relative move mode.
      F       : Gcode_Optional_Float;
      --  Feedrate in mm/min, from last G1, G2, or G3 if not specified.
      R       : Dimensionless
      --  Arc radius in mm.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G3");
   --  Perform a counter-clockwise arc move in the XY plane. The endpoint is specified by X and Y, and the arc centre
   --  is specified by R as the arc radius.
   --
   --  If just one of X or Y is specified then the command will act as if the value of that parameter were zero in
   --  relative movement mode, as with G0 and G1. If neither X nor Y are specified, a complete circle is performed.
   --
   --  All specified axes must be homed and the arc must be fully within the limits defined in the printer
   --  configuration or else an error will be raised.
   --
   --  The G90, G91, M82, and M83 commands are used to switch between relative and absolute mode.

   procedure Retract (This : in out Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G10");
   --  Perform a retraction move with the values specified by the last M207 command. Multiple G10 commands without a
   --  G11 command between them are ignored.

   procedure Recover (This : in out Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G11");
   --  Perform a recovery move with the values specified by the last M207 and M208 commands. Multiple G11 commands
   --  without a G10 command between them are ignored.

   procedure Millimeter_Units (This : in out Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G21");
   --  Does nothing. Provided for compatibility with other motion controllers where G21 sets the units to millimetres.
   --  Prunt always uses millimetres as the unit for g-code.

   procedure Report_Stored_Positions (This : in out Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G60");
   --  Report all stored positions.

   procedure Save_Current_Position
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      S       : Gcode_Arguments.Argument_Integer
      --  Memory slot to restore from.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G60");
   --  Save the current position to the specified memory slot. The saved position can later be restored with G60 or
   --  G61.

   procedure Delete_Stored_Position
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      D       : Gcode_Arguments.Argument_Integer
      --  Memory slot to delete.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G60");
   --  Delete the stored position in the specified slot.

   procedure Delete_All_Stored_Positions
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      D       : Gcode_No_Value
      --  Must be present without a value.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G60");
   --  Delete all stored positions.

   procedure Restore_Saved_Position_G60
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      Q       : Gcode_Arguments.Argument_Integer;
      --  Memory slot to restore from.
      F       : Gcode_Optional_Float;
      --  Feedrate in mm/min. From last move command if not specified.
      X       : Gcode_Optional_Float;
      --  Restore the X axis. If a value is given it is applied as an offset to the saved position.
      Y       : Gcode_Optional_Float;
      --  Restore the Y axis. If a value is given it is applied as an offset to the saved position.
      Z       : Gcode_Optional_Float;
      --  Restore the Z axis. If a value is given it is applied as an offset to the saved position.
      E       : Gcode_Optional_Float
      --  Restore the virtual E axis position without moving the extruder. If a value is given it is applied as an
      --  offset to the saved position.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G60");
   --  Move to a previously saved position from a G60 command.
   --
   --  Without axis parameters, all axes are restored. If axis parameters are included, only those axes are restored.
   --
   --  A numeric value after an axis letter applies an offset to the saved position.
   --
   --  Equivalent to G61.

   procedure Return_To_Saved_Position
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      F       : Gcode_Optional_Float;
      --  Feedrate in mm/min. From last move command if not specified.
      S       : Gcode_Arguments.Argument_Integer;
      --  Memory slot to restore from.
      X       : Gcode_Optional_Float;
      --  Restore the X axis. If a value is given it is applied as an offset to the saved position.
      Y       : Gcode_Optional_Float;
      --  Restore the Y axis. If a value is given it is applied as an offset to the saved position.
      Z       : Gcode_Optional_Float;
      --  Restore the Z axis. If a value is given it is applied as an offset to the saved position.
      E       : Gcode_Optional_Float
      --  Restore the virtual E axis position without moving the extruder. If a value is given it is applied as an
      --  offset to the saved position.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G61");
   --  Move to a previously saved position from a G60 command.
   --
   --  Without axis parameters, all axes are restored. If axis parameters are included, only those axes are restored.
   --
   --  A numeric value after an axis letter applies an offset to the saved position.

   procedure Absolute_Positioning (This : in out Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G90");
   --  Set the printer to absolute positioning mode. In this mode G0 and G1 specify absolute coordinates. M83 overrides
   --  this behaviour for the E axis. This command acts as-if a M82 command is run at the same time, meaning that M83
   --  must be called again to set the extruder to relative mode if it was called previously.

   procedure Relative_Positioning (This : in out Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "G91");
   --  Set the printer to relative positioning mode. In this mode G0 and G1 specify relative coordinates. M82 overrides
   --  this behaviour for the E axis. This command acts as-if a M83 command is run at the same time, meaning that M82
   --  must be called again to set the extruder to absolute mode if it was called previously.

   procedure Set_Virtual_Position
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      --  The X position to set. If not specified then the X axis position will not be adjusted.
      Y       : Gcode_Optional_Float;
      --  The Y position to set. If not specified then the Y axis position will not be adjusted.
      Z       : Gcode_Optional_Float;
      --  The Z position to set. If not specified then the Z axis position will not be adjusted.
      E       : Gcode_Optional_Float
      --  The E position to set. If not specified then the E axis position will not be adjusted.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "G92");
   --  Set the current position to be used by other g-code commands. This does not change how other parts of Prunt see
   --  the position, so features such as bounds still function as expected.

   procedure E_Axis_Absolute (This : in out Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M82");
   --  Set the E axis to absolute positioning mode, overrides G90. Cleared by G90.

   procedure E_Axis_Relative (This : in out Module_Instance; Planner : Planner_Interface'Class)
   with Annotate => (Prunt_Config, Gcode_Command, "M83");
   --  Set the E axis to relative positioning mode, overrides G91. Cleared by G91.

   procedure Retraction_Settings
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      F       : Gcode_Optional_Float;
      --  Feedrate in mm/min. Not modified if not specified.
      E       : Gcode_Optional_Float;
      --  E axis retraction distance. Not modified if not specified.
      Z       : Gcode_Optional_Float
      --  Z axis retraction distance. Not modified if not specified.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M207");
   --  Adjust the retraction settings used by G10 and G11.

   procedure Recover_Settings
     (This    : in out Module_Instance;
      Planner : Planner_Interface'Class;
      F       : Gcode_Optional_Float;
      --  Additional feedrate in mm/min. Not modified if not specified.
      S       : Gcode_Optional_Float
      --  Additional E axis recovery distance. Not modified if not specified.
      )
   with Annotate => (Prunt_Config, Gcode_Command, "M208");
   --  Adjust the recovery settings used by G11.

   function Build_Schema return Config.Config_Property_Maps.Map;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config);

   type Module_Instance is new My_Modules.Module_Instance with record
      Config         : User_Config;
      Status_Emitter : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Feedrate       : Velocity;
   end record;

end Prunt.Default_Modules.Motion;
