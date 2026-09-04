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
with Prunt.Gcode_Arguments;
with Prunt.Limited_Shared_Pointers;
with Prunt.Module_Types; use Prunt.Module_Types;
with Prunt.Status_Manager;

generic
   type Motor_Name is (<>);
   type Input_Switch_Name is (<>);
package Prunt.Modules is

   type Loop_Move_Stop_Condition is record
      Input_Switch : Input_Switch_Name := Input_Switch_Name'First;
      --  Input switch monitored for the corresponding motor when its loop-command offset is nonzero. Motors may share
      --  an input.
      Stop_State   : Boolean := True;
      --  Input-switch state that stops the corresponding motor.
   end record;

   type Loop_Move_Stop_Condition_Array is array (Motor_Name) of Loop_Move_Stop_Condition;

   type Loop_Move_Planner_Interface is limited interface and Module_Types.Planner_Interface;
   --  Optional planner capability for modules which generate hardware loop moves.

   function Flush_Loop_Move
     (This               : Loop_Move_Planner_Interface;
      Stop_Conditions    : Loop_Move_Stop_Condition_Array;
      Maximum_Loop_Count : Loop_Move_Count;
      Extra_Data         : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record))
      return Position_Offset
   is abstract;
   --  Finish the current block as a loop move and return its retained-tail Cartesian offset. A condition is used only
   --  when the corresponding motor has a nonzero loop-command offset.

   function Flush_Motor_Loop_Move
     (This               : Loop_Move_Planner_Interface;
      Motor              : Motor_Name;
      Stop_Condition     : Loop_Move_Stop_Condition;
      Maximum_Loop_Count : Loop_Move_Count;
      Extra_Data         : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record))
      return Position_Offset
   is abstract;
   --  Finish the current block as a loop move in which only the selected motor group executes the planned transformed
   --  motion. For Cartesian kinematics the group contains every motor assigned to Motor's axis, including duplicated
   --  axis motors. Motor-selective moves are rejected for CoreXY and other linear kinematics in which a motor affects
   --  multiple Cartesian axes. For delta kinematics the group contains every motor assigned to Motor's tower; the
   --  independent extruder forms a group by itself. An unused Motor is rejected. Stop_Condition applies to every motor
   --  in the group. All motors outside the group are held at their block-start positions throughout the move and its
   --  retained tail.

   procedure Flush_Motor_Move
     (This       : Loop_Move_Planner_Interface;
      Motor      : Motor_Name;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record))
   is abstract;
   --  Finish the current block with only the selected motor group executing the planned transformed motion. For
   --  Cartesian kinematics the group contains every motor assigned to Motor's axis, including duplicated axis motors.
   --  The move is rejected for CoreXY and other linear kinematics in which a motor affects multiple Cartesian axes.
   --  For delta kinematics the group contains every motor assigned to Motor's tower; the independent extruder forms a
   --  group by itself. An unused Motor is rejected. All motors outside the group are held at their block-start
   --  positions. This operation does not configure a repeated command or a stop condition.

   type Module is abstract tagged private;

   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class
   is (Config.Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => Ada.Tags.No_Tag, Top_Level_Items => []));
   --  Return this module's versioned configuration schema, or an empty version-one schema by default.

   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector
   is ([]);
   --  Return the G-code commands implemented by this module, or an empty vector by default.

   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map
   is ([]);
   --  Return the status groups emitted by this module, or an empty map by default.

   type Module_Instance_Parent is synchronized interface;

   package Module_Instance_Shared_Pointers is new Limited_Shared_Pointers (Module_Instance_Parent'Class);
   --  We need this outer type so that we can use it in a primitive on Module_Instance. This is a bit messy, but
   --  users of Module_Instance_Shared_Pointers are always going to cast the result to a more specific type anyway.

   type Module_Instance is synchronized interface and Module_Instance_Parent;
   --  Children of Module_Instance should be declared with an unknown discriminant part to prevent accidental
   --  instantiation without a constructor.
   --
   --  Finalization should reset motors/heaters/etc. to their initial power-on state iff the module instance has been
   --  started.

   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier)
   is abstract;
   --  This is intentionally not an in out parameter to discourage making changes to the module instance at this
   --  point. It is possible to use Self_Ref to bypass this, however it generally does not make sense to do so as
   --  g-code is only meant to execute after it makes its way through the planner.
   --
   --  TODO: Default to null if this GCC bug is ever fixed: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=124418

   procedure Start
     (This     : in out Module_Instance;
      Self_Ref : Module_Instance_Shared_Pointers.Weak_Ref;
      Planner  : Planner_Interface'Class)
   is abstract;
   --  Modules should not start in the initialize procedure as the initialize procedure can be used to check for config
   --  errors without actually starting.
   --
   --  Self_Ref is a reference to the instance that this is being called on and is mainly useful for g-code commands
   --  which need to make use of the instance.
   --
   --  Access to a planner is provided here mainly for setting kinematic limits, motion should not be generated during
   --  startup.
   --
   --  TODO: Default to null if this GCC bug is ever fixed: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=124418

   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return Module_Instance_Shared_Pointers.Ref)
      return Module_Instance'Class
   is abstract;
   --  Get_Other_Instance attempts to initialize all other modules and then returns the instance with a tag equal to
   --  Tag. If the requested tag cannot be resolved, the controller raises an exception. The active initialization
   --  chain is also logged so that dependency loops can be diagnosed.
   --
   --  All instances returned from Get_Other_Instance will not be started at this point.
   --
   --  Config_Data values share the underlying module configuration state, so copies may be kept after this function
   --  returns to be used in g-code commands.

private

   type Module is abstract tagged null record;

end Prunt.Modules;
