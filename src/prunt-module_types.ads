------------------------------------------------------
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

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with Prunt.Gcode_Arguments;
with Prunt.Motion_Planner;
with System.Storage_Elements;

package Prunt.Module_Types is

   use type Gcode_Arguments.Arguments_Index;
   use type Gcode_Arguments.Argument_Integer;

   type User_Config_Integer is range -1_000_000 .. 1_000_000 with Annotate => (Prunt_Config, User_Config);

   type User_Config_Empty is record
      null;
   end record
   with Annotate => (Prunt_Config, User_Config);

   type Planner_Interface is limited interface;
   --  Subprograms towards end of file.

   type Gcode_Argument_Allowed_Kinds is array (Gcode_Arguments.Argument_Kind) of Boolean;

   subtype Gcode_Identifier_Argument_Index is Gcode_Arguments.Arguments_Index
   with Static_Predicate => Gcode_Identifier_Argument_Index in 'G' | 'M';

   subtype Gcode_User_Argument_Index is Gcode_Arguments.Arguments_Index
   with Static_Predicate => Gcode_User_Argument_Index not in Gcode_Identifier_Argument_Index;

   type Gcode_Argument_Definition is record
      Description   : Virtual_String;
      Allowed_Kinds : Gcode_Argument_Allowed_Kinds;
   end record
   with
     Dynamic_Predicate =>
       (Allowed_Kinds with delta Gcode_Arguments.Non_Existent_Kind => False)
       /= Gcode_Argument_Allowed_Kinds'(others => False);

   package Gcode_Argument_Maps is new
     Ada.Containers.Ordered_Maps (Gcode_User_Argument_Index, Gcode_Argument_Definition);

   Gcode_Bad_Inputs_Error : exception;

   type Gcode_Command_Identifier is record
      Argument : Gcode_Identifier_Argument_Index;
      Number   : Gcode_Arguments.Argument_Integer;
   end record;

   type Gcode_Command is record
      Identifier  : Gcode_Command_Identifier;
      Name        : Virtual_String;
      Description : Virtual_String;
      Arguments   : Gcode_Argument_Maps.Map;
   end record;

   type Gcode_Optional_Integer (Present : Boolean := True) is record
      case Present is
         when False =>
            null;

         when True =>
            Value : Gcode_Arguments.Argument_Integer;
      end case;
   end record;

   type Gcode_Optional_Float (Present : Boolean := True) is record
      case Present is
         when False =>
            null;

         when True =>
            Value : Dimensionless;
      end case;
   end record;

   type Gcode_Optional_String (Present : Boolean := True) is record
      case Present is
         when False =>
            null;

         when True =>
            Value : Virtual_String;
      end case;
   end record;

   type Gcode_Optional_No_Value (Present : Boolean := True) is record
      case Present is
         when False | True =>
            null;
      end case;
   end record;

   type Gcode_No_Value is null record;

   function "<" (Left, Right : Gcode_Command_Identifier) return Boolean
   is (if Left.Argument /= Right.Argument then Left.Argument < Right.Argument else Left.Number < Right.Number);

   package Gcode_Command_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Gcode_Command);

   function Get_Last_Position (This : Planner_Interface) return Position is abstract;

   function Get_Last_Kinematic_Parameters (This : Planner_Interface) return Motion_Planner.Kinematic_Parameters
   is abstract;

   type Extra_Corner_Data is tagged null record;

   procedure Process (This : Extra_Corner_Data; Last_Command_Index : Command_Index) is null;

   type Extra_Block_Resetting_Data is tagged null record;

   procedure Process_Before_Block (This : Extra_Block_Resetting_Data; Last_Command_Index : Command_Index) is null;

   procedure Process_After_Block
     (This                 : Extra_Block_Resetting_Data;
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index;
      Loop_Move_Offset     : Position_Offset)
   is null;

   procedure Mark_Axis_Homed (This : Planner_Interface; Axis : Axis_Name) is abstract;

   procedure Mark_Axis_Unhomed (This : Planner_Interface; Axis : Axis_Name) is abstract;

   procedure Add_Corner
     (This          : Planner_Interface;
      Pos           : Position;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Require_Homed : Boolean := True;
      Corner_Data   : Extra_Corner_Data'Class := Extra_Corner_Data'(null record))
   is abstract;

   procedure Add_Corner_Data (This : Planner_Interface; Corner_Data : Extra_Corner_Data'Class) is abstract;
   --  May be attached to a dummy corner in the next block.

   procedure Flush
     (This       : Planner_Interface;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record))
   is abstract;

   procedure Flush_And_Change_Kinematic_Parameters
     (This       : Planner_Interface;
      Params     : Motion_Planner.Kinematic_Parameters;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record))
   is abstract;

   procedure Flush_And_Reset_Position
     (This         : Planner_Interface;
      New_Position : Position;
      Extra_Data   : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record))
   is abstract;

end Prunt.Module_Types;
