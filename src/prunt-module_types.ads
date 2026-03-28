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

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with Prunt.Gcode_Arguments;
with Prunt.Motion_Planner;
with System.Storage_Elements;

package Prunt.Module_Types is

   use type Gcode_Arguments.Arguments_Index;
   use type Gcode_Arguments.Argument_Integer;

   type User_Config_Integer is range -1_000_000 .. 1_000_000 with Annotate => (Prunt_Config, User_Config);

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
   --  TODO: This should probably be changed to a set of procedures to update a particular field without reading all
   --  the kinematic limits so that there are less restrictions on the planner.

   type Extra_Corner_Data is tagged null record;
   --  Use this for non-blocking handlers with a low cost that should not interrupt motion. If this procedure takes too
   --  long then the motion queue will run dry in an unsafe state.

   procedure Process (This : Extra_Corner_Data; Last_Command_Index : Command_Index) is null;

   type Extra_Block_Resetting_Data is tagged null record;
   -- Use this for handlers which need motion to stop before processing.

   --  procedure Process_Before_Block (This : Extra_Block_Resetting_Data; Last_Command_Index : Command_Index) is null;
   --  TODO: Do we need the above? It's probably a bad idea to have this with the module system. Modules should instead
   --  just use `Process_After_Block` for everything.

   procedure Process_After_Block
     (This                 : Extra_Block_Resetting_Data;
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index;
      Loop_Move_Offset     : Position_Offset)
   is null;
   --  TODO: We should pass in an anonymous-access-to-interface here that exposes everything instead of direct
   --  parameters since we might want to add or remove things later. The interface can also expose `Wait_For_Idle` as a
   --  procedure instead of having to work with the command index directly.

   procedure Mark_Axis_Homed (This : Planner_Interface; Axis : Axis_Name) is abstract;

   procedure Mark_Axis_Unhomed (This : Planner_Interface; Axis : Axis_Name) is abstract;

   function Axis_Is_Homed (This : Planner_Interface; Axis : Axis_Name) return Boolean is abstract;

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
     (This           : Planner_Interface;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False)
   is abstract;

   procedure Flush_And_Change_Kinematic_Parameters
     (This           : Planner_Interface;
      Params         : Motion_Planner.Kinematic_Parameters;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False)
   is abstract;

   procedure Flush_And_Reset_Position
     (This           : Planner_Interface;
      New_Position   : Position;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False)
   is abstract;

end Prunt.Module_Types;
