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

package Prunt.Module_Types is

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

   Gcode_Temporarily_Rejected_Error : exception;
   --  Raised by a module when it cannot accept more G-code right now without exceeding a bounded internal queue. The
   --  controller will flush already-accepted planner work and retry the same line later unless cancellation interrupts
   --  the retry.

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

   type Gcode_Optional_Or_No_Value_Kind is (Gcode_Value_Not_Present, Gcode_No_Value_Present, Gcode_Value_Present);

   type Gcode_Optional_Float_Or_No_Value (Kind : Gcode_Optional_Or_No_Value_Kind := Gcode_Value_Present) is record
      case Kind is
         when Gcode_Value_Not_Present | Gcode_No_Value_Present =>
            null;

         when Gcode_Value_Present =>
            Value : Dimensionless;
      end case;
   end record;

   type Gcode_Optional_Integer_Or_No_Value (Kind : Gcode_Optional_Or_No_Value_Kind := Gcode_Value_Present) is record
      case Kind is
         when Gcode_Value_Not_Present | Gcode_No_Value_Present =>
            null;

         when Gcode_Value_Present =>
            Value : Gcode_Arguments.Argument_Integer;
      end case;
   end record;

   type Gcode_Value_Or_No_Value_Kind is (Gcode_No_Value_Present, Gcode_Value_Present);

   type Gcode_Float_Or_No_Value (Kind : Gcode_Value_Or_No_Value_Kind := Gcode_Value_Present) is record
      case Kind is
         when Gcode_No_Value_Present =>
            null;

         when Gcode_Value_Present =>
            Value : Dimensionless;
      end case;
   end record;

   type Gcode_Integer_Or_No_Value (Kind : Gcode_Value_Or_No_Value_Kind := Gcode_Value_Present) is record
      case Kind is
         when Gcode_No_Value_Present =>
            null;

         when Gcode_Value_Present =>
            Value : Gcode_Arguments.Argument_Integer;
      end case;
   end record;

   type Gcode_No_Value is null record;

   function "<" (Left, Right : Gcode_Command_Identifier) return Boolean
   is (if Left.Argument /= Right.Argument then Left.Argument < Right.Argument else Left.Number < Right.Number);
   --  Order command identifiers first by their G or M argument and then by command number.

   package Gcode_Command_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Gcode_Command);

   function Get_Last_Position (This : Planner_Interface) return Position is abstract;
   --  Return the current queued/planner endpoint used as the start of subsequently queued motion.

   function Get_Last_Kinematic_Parameters (This : Planner_Interface) return Motion_Planner.Kinematic_Parameters
   is abstract;
   --  Return the current planner parameters that apply to subsequently queued motion.
   --
   --  TODO: This should probably be changed to a set of procedures to update a particular field without reading all
   --  the kinematic limits so that there are less restrictions on the planner.

   function Get_State_Anchor_Corner_ID (This : Planner_Interface) return Planner_Corner_ID is abstract;
   --  Returns the primary corner ID that newly planned module state should be anchored to. If primary planner work is
   --  already queued, this is the latest primary corner that must execute before the newly planned state becomes
   --  committed. If no primary corner is pending, this is the already-executed/cancelled state anchor and the state
   --  may be committed immediately. Pause planner motion does not create primary corner anchors, but may still observe
   --  the current primary anchor for cancellation consistency.

   function Get_Last_Executed_Corner_ID (This : Planner_Interface) return Planner_Corner_ID is abstract;
   --  Returns the last primary planner corner that step generation has actually reached, including dummy corners used
   --  to process primary extra corner data. Pause/park/resume motion does not advance this value. Modules use this to
   --  catch committed state up to physical motion progress before reporting status or handling cancellation; it may
   --  lag Get_State_Anchor_Corner_ID while primary motion is queued or after a cancellation barrier has been
   --  established.

   type Extra_Corner_Data is tagged null record;
   --  Use this for non-blocking handlers with a low cost that should not interrupt motion. If this procedure takes too
   --  long then the motion queue will run dry in an unsafe state.

   procedure Process (This : Extra_Corner_Data; Last_Command_Index : Command_Index) is null;
   --  Process non-blocking data attached to a planner corner after its motion commands have been queued.

   type Extra_Block_Resetting_Data is tagged null record;
   --  Use this for handlers which need motion to stop before processing.

   --  procedure Process_Before_Block (This : Extra_Block_Resetting_Data; Last_Command_Index : Command_Index) is null;
   --  TODO: Do we need the above? It's probably a bad idea to have this with the module system. Modules should instead
   --  just use Process_After_Block for everything.

   type Block_End_Context is limited interface;

   function Get_First_Accel_Distance (This : Block_End_Context) return Length is abstract;
   --  Return the acceleration distance of the first segment in the completed block.

   function Get_Last_Command_Index (This : Block_End_Context) return Command_Index is abstract;
   --  Return the last interpolation or step-generator command index queued for the completed block.

   function Get_Loop_Move_Offset (This : Block_End_Context) return Position_Offset is abstract;
   --  Return the accumulated position offset produced by any repeated loop move in the completed block.

   procedure Wait_For_Idle (This : Block_End_Context) is abstract;
   --  Wait until hardware execution has reached the end of the completed block.

   procedure Catch_Up_Planner_State (This : Block_End_Context) is abstract;
   --  Commit speculative module state through the last physically executed primary-planner corner.

   procedure Prepare_Config_For_Save (This : Block_End_Context) is abstract;
   --  Ask registered modules to copy their current runtime settings into their persistent configuration handles.

   procedure Log (This : Block_End_Context; Message : Virtual_String) is abstract;
   --  Emit command output using the source associated with this block. Interactive commands send output to their web
   --  card, file commands include their source location in the global log, and internal blocks use the global log.

   procedure Log_If_Interactive (This : Block_End_Context; Message : Virtual_String) is abstract;
   --  Emit Message only when this block belongs to an interactively submitted command.

   procedure Process_After_Block (This : Extra_Block_Resetting_Data; Context : Block_End_Context'Class) is null;
   --  Process resetting data after motion has stopped at the end of a planner block.

   type Gcode_Message_Event is new Extra_Block_Resetting_Data with record
      Message : Virtual_String;
   end record;

   overriding
   procedure Process_After_Block (This : Gcode_Message_Event; Context : Block_End_Context'Class);
   --  Emit a preformatted G-code reply at its block boundary.

   type Pause_Context is abstract tagged null record;

   function Get_Pause_Position (This : Pause_Context) return Position is abstract;
   --  Return the physical position at which pause handling begins or resume handling ends.

   function Get_Last_Command_Index (This : Pause_Context) return Command_Index is abstract;
   --  Return the last command index queued before the pause transition.

   type Pause_Handler is synchronized interface;

   procedure Handle_Pause
     (This : in out Pause_Handler; Planner : Planner_Interface'Class; Context : Pause_Context'Class)
   is null;
   --  Add any module-specific parking or shutdown work to Planner when a pause begins.

   procedure Handle_Resume
     (This : in out Pause_Handler; Planner : Planner_Interface'Class; Context : Pause_Context'Class)
   is null;
   --  Add any module-specific restoration work to Planner before normal motion resumes.

   type Planner_State_Handler is synchronized interface;

   procedure Catch_Up_Planner_State (This : in out Planner_State_Handler; Executed_Corner_ID : Planner_Corner_ID)
   is null;
   --  Commit pending module state anchored at or before Executed_Corner_ID.

   type Config_Save_Preparer is synchronized interface;

   procedure Prepare_Config_For_Save (This : in out Config_Save_Preparer) is null;
   --  Copy the module's current runtime settings into the configuration data used for saving.

   type Cancellation_Handler is synchronized interface;

   procedure Handle_Cancel
     (This                    : in out Cancellation_Handler;
      Executed_Corner_ID      : Planner_Corner_ID;
      Cancellation_Barrier_ID : Planner_Corner_ID;
      Current_Position        : Position)
   is null;
   --  Discard or commit pending module state at a cancellation barrier and synchronize it with Current_Position.

   procedure Mark_Axis_Homed (This : Planner_Interface; Axis : Axis_Name) is abstract;
   --  Mark Axis as homed. Primary planners block on an execution marker so cancellation leaves Axis unhomed; isolated
   --  pause planners update only their speculative pause state.

   procedure Mark_Axis_Unhomed (This : Planner_Interface; Axis : Axis_Name) is abstract;
   --  Unconditionally mark Axis as unhomed. Primary planners then block on an execution marker; isolated pause
   --  planners update only their speculative pause state.

   function Axis_Is_Homed (This : Planner_Interface; Axis : Axis_Name) return Boolean is abstract;
   --  Return whether Axis is currently marked as homed.

   procedure Add_Corner
     (This          : Planner_Interface;
      Pos           : Position;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Require_Homed : Boolean := True)
   is abstract;
   --  Append a linear move to Pos, optionally requiring every moving axis to have been homed.

   procedure Add_Helix
     (This          : Planner_Interface;
      Center        : Position;
      Pos           : Position;
      Clockwise     : Boolean;
      Feedrate      : Velocity;
      Dwell_After   : Time := 0.0 * s;
      Require_Homed : Boolean := True)
   is abstract;
   --  Append a clockwise or counter-clockwise XY helix ending at Pos around Center.

   procedure Add_Corner_Data (This : Planner_Interface; Corner_Data : Extra_Corner_Data'Class) is abstract;
   --  May be attached to a dummy corner in the next block.

   procedure Flush
     (This           : Planner_Interface;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False)
   is abstract;
   --  Finish the current planner block and attach Extra_Data for processing once its motion stops.

   procedure Flush_And_Change_Kinematic_Parameters
     (This           : Planner_Interface;
      Params         : Motion_Planner.Kinematic_Parameters;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False)
   is abstract;
   --  Flush pending motion, process Extra_Data, and make Params apply to subsequently planned motion.

   procedure Flush_And_Reset_Position
     (This           : Planner_Interface;
      New_Position   : Position;
      Extra_Data     : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record);
      Is_Homing_Move : Boolean := False)
   is abstract;
   --  Flush pending motion, process Extra_Data, and reset the subsequent planner position to New_Position.

end Prunt.Module_Types;
