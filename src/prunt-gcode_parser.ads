-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
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

private with Prunt.Gcode_Arguments;
private with Ada.Containers.Bounded_Vectors;

generic
   type Heater_Name is (<>);
   type Fan_Name is (<>);
   type Laser_Name is (<>);
package Prunt.Gcode_Parser is

   Has_Heaters : constant Boolean := Heater_Name'Pos (Heater_Name'Last) >= Heater_Name'Pos (Heater_Name'First);
   Has_Fans    : constant Boolean := Fan_Name'Pos (Fan_Name'Last) >= Fan_Name'Pos (Fan_Name'First);
   Has_Lasers  : constant Boolean := Laser_Name'Pos (Laser_Name'Last) >= Laser_Name'Pos (Laser_Name'First);

   type Optional_Fan (Has_Fans : Boolean := Gcode_Parser.Has_Fans) is record
      case Has_Fans is
         when True =>
            Name : Fan_Name;

         when False =>
            null;
      end case;
   end record
   with Dynamic_Predicate => Optional_Fan.Has_Fans = Has_Fans;

   type Optional_Laser (Has_Lasers : Boolean := Gcode_Parser.Has_Lasers) is record
      case Has_Lasers is
         when True =>
            Name : Laser_Name;

         when False =>
            null;
      end case;
   end record
   with Dynamic_Predicate => Optional_Laser.Has_Lasers = Has_Lasers;

   type Command_Kind is
     (None_Kind,
      Pause_Kind,
      Move_Kind,
      Dwell_Kind,
      Home_Kind,
      Enable_Steppers_Kind,
      Disable_Steppers_Kind,
      Set_Hotend_Temperature_Kind,
      Wait_Hotend_Temperature_Kind,
      Set_Bed_Temperature_Kind,
      Wait_Bed_Temperature_Kind,
      Set_Chamber_Temperature_Kind,
      Wait_Chamber_Temperature_Kind,
      Set_Fan_Speed_Kind,
      TMC_Dump_Kind,
      Heater_Autotune_Kind,
      Set_Acceleration_Max_Kind,
      Set_Jerk_Max_Kind,
      Set_Snap_Max_Kind,
      Set_Crackle_Max_Kind,
      Set_Chord_Error_Max_Kind,
      Set_Pressure_Advance_Time_Kind,
      Set_Laser_Power_Kind);

   type Axes_Set is array (Axis_Name) of Boolean;

   type Command (Kind : Command_Kind := None_Kind) is record
      Pos : Position;
      case Kind is
         when None_Kind | Pause_Kind | TMC_Dump_Kind =>
            null;

         when Move_Kind =>
            Old_Pos  : Position;
            Feedrate : Velocity;
            Is_Rapid : Boolean;

         when Dwell_Kind =>
            Dwell_Time : Time;

         when Home_Kind | Enable_Steppers_Kind | Disable_Steppers_Kind =>
            Axes : Axes_Set;
            case Kind is
               when Home_Kind =>
                  Pos_Before : Position;

               when others =>
                  null;
            end case;

         when Set_Hotend_Temperature_Kind
            | Wait_Hotend_Temperature_Kind
            | Set_Bed_Temperature_Kind
            | Wait_Bed_Temperature_Kind
            | Set_Chamber_Temperature_Kind
            | Wait_Chamber_Temperature_Kind
         =>
            Target_Temperature : Temperature;

         when Set_Fan_Speed_Kind =>
            Fan_To_Set : Fan_Name;
            Fan_Speed  : PWM_Scale;

         when Heater_Autotune_Kind =>
            Tuning_Temperature : Temperature;
            Heater_To_Tune     : Heater_Name;
            Max_Cycles         : PID_Autotune_Cycle_Count;

         when Set_Acceleration_Max_Kind =>
            Acceleration_Max : Acceleration;

         when Set_Jerk_Max_Kind =>
            Jerk_Max : Jerk;

         when Set_Snap_Max_Kind =>
            Snap_Max : Snap;

         when Set_Crackle_Max_Kind =>
            Crackle_Max : Crackle;

         when Set_Chord_Error_Max_Kind =>
            Chord_Error_Max : Length;

         when Set_Pressure_Advance_Time_Kind =>
            Pressure_Advance_Time : Time;

         when Set_Laser_Power_Kind =>
            Laser_To_Set : Laser_Name;
            Laser_Power  : PWM_Scale;
      end case;
   end record;

   type Context is private;

   function Make_Context
     (Initial_Position   : Position;
      Initial_Feedrate   : Velocity;
      Replace_G0_With_G1 : Boolean;
      Default_Fan        : Optional_Fan;
      Default_Laser      : Optional_Laser) return Context;

   procedure Parse_Line (Ctx : in out Context; Line : String; Runner : not null access procedure (Comm : Command));
   --  `Runner` must not call `Parse_Line` with the same context as the context is not updated until after `Runner`
   --  returns.

   procedure Reset_Position (Ctx : in out Context; Pos : Position);

   Bad_Line : exception;

private

   use Gcode_Arguments;

   type Context is record
      XYZ_Relative_Mode         : Boolean;
      E_Relative_Mode           : Boolean;
      Pos                       : Position;
      Feedrate                  : Velocity;
      G92_Offset                : Position_Offset;
      Is_Retracted              : Boolean;
      Current_Retraction_Offset : Position_Offset;
      M207_Offset               : Position_Offset;
      M207_Feedrate             : Velocity;
      M208_Offset               : Position_Offset;
      M208_Feedrate             : Velocity;
      Replace_G0_With_G1        : Boolean;
      Default_Fan               : Optional_Fan;
      Default_Laser             : Optional_Laser;
   end record;

   procedure G0_Rapid_Linear_Move
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Perform a non-print linear move. Axes which are not specified will not move. Moves at the maximum feedrate if
   --  feedrate is not specified.
   --
   --  All specified axes must be homed and the target position must be within the limits defined in the printer
   --  configuration or else an error will be raised.
   --
   --  The `G90`, `G91`, `M82`, and `M83` are used to switch between relative and absolute mode.
   --
   --  The `ABCUVW` parameters are not present as Prunt does not support these axes. The `S` parameter is not present
   --  as Prunt uses M3/M5 instead. These parameters are present in Marlin.
   --
   --  Parameters:
   --  - [X] (Real): X axis target position in mm, or offset in relative move mode.
   --  - [Y] (Real): Y axis target position in mm, or offset in relative move mode.
   --  - [Z] (Real): Z axis target position in mm, or offset in relative move mode.
   --  - [E] (Real): E axis target position in mm, or offset in relative move mode.
   --  - [F] (Real): Feedrate in mm/min, maximum if not specified.

   procedure G1_Linear_Move
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Perform a linear move. Axes which are not specified will not move. Moves at the same feedrate as the last G1
   --  command if feedrate is not specified.
   --
   --  All specified axes must be homed and the target position must be within the limits defined in the printer
   --  configuration or else an error will be raised.
   --
   --  The `G90`, `G91`, `M82`, and `M83` are used to switch between relative and absolute mode.
   --
   --  The `ABCUVW` parameters are not present as Prunt does not support these axes. The `S` parameter is not present
   --  as Prunt uses M3/M5 instead. These parameters are present in Marlin.
   --
   --  Parameters:
   --  - [X] (Real): X axis target position in mm, or offset in relative move mode.
   --  - [Y] (Real): Y axis target position in mm, or offset in relative move mode.
   --  - [Z] (Real): Z axis target position in mm, or offset in relative move mode.
   --  - [E] (Real): E axis target position in mm, or offset in relative move mode.
   --  - [F] (Real): Feedrate in mm/min, from last G1 if not specified.

   procedure G4_Dwell
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Pause the printer for a given number of seconds after the last move is completed.
   --
   --  Exactly one of the below parameters must be present or else an error will be raised. This differs from Marlin
   --  where `S` silently takes precedence over `P`.
   --
   --  The `S` and `P` parameters are allowed to have a decimal part. This differs from Marlin where only an integer is
   --  allowed.
   --
   --  Parameters:
   --  - [S] (Real): Time to pause in seconds.
   --  - [P] (Real): Time to pause in milliseconds.

   procedure G10_Retract
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Perform a retraction move with the values specified by the last M207 command. Multiple G10 commands without a
   --  G11 command between them are ignored.
   --
   --  No parameters. This differs from Marlin where an optional `S` parameter may be used to specify a different
   --  retraction length for tool swaps.

   procedure G11_Recover
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Perform a recovery move with the values specified by the last M207 and M208 commands. Multiple G11 commands
   --  without a G10 command between them are ignored.
   --
   --  No parameters.

   procedure G21_Millimetre_Units
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Does nothing. Provided for compatibility with other motion controllers where G21 sets the units to millimetres.
   --  Prunt always uses millimetres as the unit for g-code.
   --
   --  No parameters.

   procedure G28_Auto_Home
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Home the specified axes using the method and parameters specified in the configuration. If no axes are specified
   --  then all axes are homed, including the E axis.
   --
   --  The `ABCUVW` parameters are not present as Prunt does not support these axes. The `LOR` parameters are not
   --  present but are planned for a future version. These parameters are present in Marlin.
   --
   --  The `E` parameter is not present in Marlin as Marlin does not homing of the E axis.
   --
   --  Parameters:
   --  - [X] (None): If included then the X axis will be homed.
   --  - [Y] (None): If included then the Y axis will be homed.
   --  - [Z] (None): If included then the Z axis will be homed.
   --  - [E] (None): If included then the E axis will be homed.

   procedure G90_Absolute_Positioning
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the printer to absolute positioning mode. In this mode G0 and G1 specify absolute coordinates. M83 overrides
   --  this behaviour for the E axis. This command acts as-if a M82 command is run at the same time, meaning that M83
   --  must be called again to set the extruder to relative mode if it was called previously.
   --
   --  No parameters.

   procedure G91_Relative_Positioning
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the printer to relative positioning mode. In this mode G0 and G1 specify relative coordinates. M82 overrides
   --  this behaviour for the E axis. This command acts as-if a M83 command is run at the same time, meaning that M82
   --  must be called again to set the extruder to absolute mode if it was called previously.
   --
   --  No parameters.

   procedure G92_Set_Virtual_Position
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the current position to be used by other g-code commands. This does not change how other parts of Prunt see
   --  the position, so features such as bounds still function as expected.
   --
   --  The `ABCUVW` parameters are not present as Prunt does not support these axes. These parameters are present in
   --  Marlin.
   --
   --  Parameters:
   --  - [X] (Real): The X position to set. If not specified then the X axis position will not be adjusted.
   --  - [Y] (Real): The X position to set. If not specified then the Y axis position will not be adjusted.
   --  - [Z] (Real): The X position to set. If not specified then the Z axis position will not be adjusted.
   --  - [E] (Real): The X position to set. If not specified then the E axis position will not be adjusted.

   procedure M0_M1_Pause
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Pause the printer and wait for the user to command the printer to continue.
   --
   --  The `SP` parameters are not present as Prunt does not support pause expiry. These parameters are present in
   --  Marlin.
   --
   --  No parameters.

   procedure M3_Set_Laser_Power
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the power for a given laser. If no laser is specified then the default laser from the configuration is used.
   --
   --  Parameters:
   --  - [P] (Integer/String): Laser name or index. Default laser if not specified.
   --  - S (Real): Power from 0 to 255 with 255 being maximum power.

   procedure M5_Laser_Off
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Turn off the specified laser.
   --
   --  If no laser is specified then the default laser from the configuration is used.
   --
   --  Parameters:
   --  - [P] (Integer/String): Laser name or index. Default laser if not specified.

   procedure M17_Enable_Motors
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Enable the motors assigned to the specified axes. If no axes are specified then all axes are enabled. On a
   --  CoreXY machine specifying X or Y will enable all XY motors.
   --
   --  The `ABCUVW` parameters are not present as Prunt does not support these axes. These parameters are present in
   --  Marlin.
   --
   --  Parameters:
   --  - [X] (None): If included, enable the X axis steppers.
   --  - [Y] (None): If included, enable the Y axis steppers.
   --  - [Z] (None): If included, enable the Z axis steppers.
   --  - [E] (None): If included, enable the E axis steppers.

   procedure M18_M84_Disable_Motors
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Enable the motors assigned to the specified axes. If no axes are specified then all axes are disabled. On a
   --  CoreXY machine specifying X or Y will disable all XY motors. Disabled axes are marked as unhomed.
   --
   --  The `ABCUVW` parameters are not present as Prunt does not support these axes. These parameters are present in
   --  Marlin.
   --
   --  Parameters:
   --  - [X] (None): If included, disable the X axis steppers.
   --  - [Y] (None): If included, disable the Y axis steppers.
   --  - [Z] (None): If included, disable the Z axis steppers.
   --  - [E] (None): If included, disable the E axis steppers.

   procedure M82_E_Axis_Absolute
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the E axis to absolute positioning mode, overrides G90. Cleared by G90.
   --
   --  No parameters.

   procedure M83_E_Axis_Relative
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the E axis to relative positioning mode, overrides G91. Cleared by G91.
   --
   --  No parameters.

   procedure M104_Set_Hotend_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the target temperature for the hotend.
   --
   --  The `BFI` parameters are not present. These parameters are present in Marlin.
   --
   --  Parameters:
   --  - S (Real): Target temperature in Celsius.
   --  - [T] (Integer): Ignored. Provided for Marlin slicer compatibility. This parameter will be used to support
   --                   multiple tools in a future version.

   procedure M106_Set_Fan_Speed
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the speed for a given fan. If no fan is specified then the default fan from the configuration is used.
   --
   --  This command differs from Marlin in that the `P` and `S` parameters support types other than integers.
   --
   --  The `IT` parameters are not present. These parameters are present in Marlin.
   --
   --  Parameters:
   --  - [P] (Integer/String): Fan name or index. First fan if not specified.
   --  - [S] (Real): Speed from 0 to 255 with 255 being maximum speed. 255 if not specified.

   procedure M107_Fan_Off
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Turn off the specified fan.
   --
   --  If no fan is specified then the default fan from the configuration is used.
   --
   --  Parameters:
   --  - [P] (Integer/String): Fan name or index. Default fan if not specified.

   procedure M109_Wait_For_Hotend_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the hotend target temperature and then wait until the hotend reaches or exceeds the given temperature.
   --
   --  The `BFIR` parameters are not present. These parameters are present in Marlin.
   --
   --  Parameters:
   --  - S (Real): Temperature to wait for in Celsius.
   --  - [T] (Integer): Ignored. Provided for Marlin slicer compatibility. This parameter will be used to support
   --                   multiple tools in a future version.

   procedure M122_TMC_Register_Dump
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Log all register values for all connected Trinamic stepper drivers.
   --
   --  No Parameters. This differs from Marlin.

   procedure M140_Set_Bed_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the target temperature for the bed.
   --
   --  The `I` parameter is not present. This parameter is present in Marlin.
   --
   --  Parameters:
   --  - S (Real): Target temperature in Celsius.

   procedure M141_Set_Chamber_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the target temperature for the chamber.
   --
   --  Parameters:
   --  - S (Real): Target temperature in Celsius.

   procedure M190_Wait_For_Bed_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the target temperature for the bed and then wait until the bed reaches or exceeds the given temperature.
   --
   --  The `IRT` parameters are not present. These parameters are present in Marlin.
   --
   --  Parameters:
   --  - S (Real): Temperature to wait for in Celsius.

   procedure M191_Wait_For_Chamber_Temperature
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Set the target temperature for the chamber and then wait until the chamber reaches or exceeds the given
   --  temperature.
   --
   --  The `R` parameter is not present. This parameter is present in Marlin.
   --
   --  Parameters:
   --  - S (Real): Temperature to wait for in Celsius.

   procedure M205_Set_Dynamic_Kinematic_Limits
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  This command differs significantly from Marlin due to the different parameters in Prunt.
   --
   --  Parameters:
   --  - P (None): Required to prevent accidental usage of commands meant for other motion controllers.
   --  - [A] (Real): Acceleration in mm/s². Not modified if not specified.
   --  - [J] (Real): Jerk in mm/s³. Not modified if not specified.
   --  - [S] (Real): Snap in mm/s⁴. Not modified if not specified.
   --  - [C] (Real): Crackle in mm/s⁵. Not modified if not specified.
   --  - [D] (Real): Path deviation in mm. Not modified if not specified.
   --  - [L] (Real): Pressure advance time in s. Not modified if not specified.

   procedure M207_Retraction_Settings
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Adjust the retraction settings used by G10 and G11.
   --
   --  The `W` parameter is not present. This parameter is present in Marlin.
   --
   --  Parameters:
   --  - [F] (Real): Feedrate in mm/min. Not modified if not specified.
   --  - [E] (Real): E axis retraction distance. Not modified if not specified.
   --  - [Z] (Real): Z axis retraction distance. Not modified if not specified.

   procedure M208_Recovery_Settings
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Adjust the recovery settings used by G11.
   --
   --  The `RW` parameters are not present. These parameters are present in Marlin.
   --
   --  Parameters:
   --  - [F] (Real): Additional feedrate in mm/min. Not modified if not specified.
   --  - [S] (Real): Additional E axis recovery distance. Not modified if not specified.

   procedure M303_PID_Autotune
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Perform PID autotuning and output the result to the log.
   --
   --  The `DU` parameters are not present. These parameters are present in Marlin.
   --
   --  The `E` parameter from Marlin has been replaced with the `T` parameter with different semantics.
   --
   --  Parameters:
   --  - T (Integer/String): Heater index or name.
   --  - S (Real): Target temperature in Celsius.
   --  - [C] (Integer): Maximum number of cycles. Defaults to 5.

   procedure M73_M204_M486_Ignored
     (Ctx : in out Context; Args : in out Arguments; Runner : not null access procedure (Comm : Command));
   --  Ignored command provided for slicer compatibility when outputting Marlin G-code.

   package Command_Buffers is new Ada.Containers.Bounded_Vectors (Positive, Command);

end Prunt.Gcode_Parser;
