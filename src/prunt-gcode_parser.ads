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

   type Command_Runner is access procedure (Comm : Command);

   procedure Parse_Line (Ctx : in out Context; Line : String; Runner : Command_Runner);
   procedure Reset_Position (Ctx : in out Context; Pos : Position);

   Bad_Line : exception;

private

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

end Prunt.Gcode_Parser;
