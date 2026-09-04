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

--  This package receives idle notifications from the controller and then distributes them to everything else.

pragma Extensions_Allowed (On);

with Ada.Containers.Vectors;
with Ada.Tags;
with Prunt.Config;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;
with Prunt.Controller_Interfaces;

generic
package Prunt.Default_Modules.Idle_Emitter is

   type Module is new My_Modules.Module with null record;

   type Idle_Notification_Receiver is synchronized interface;

   procedure Idle_Start (This : in out Idle_Notification_Receiver) is abstract;
   --  Called when command execution catches up to the last emitted command and there is no end-of-block handler
   --  running.

   procedure Idle_End (This : in out Idle_Notification_Receiver) is abstract;
   --  Called before a new command is emitted after command execution catches up to the last emitted command. This
   --  procedure is allowed to block to stop the given command from being enqueued until the machine is in a state
   --  where it is ready to do so. For example, a module might need to wait for heaters to heat back up.

   type Idle_Notification_Emitter is limited interface;

   procedure Request_Idle_Notifications
     (This : in out Idle_Notification_Emitter; Receiver : My_Modules.Module_Instance_Shared_Pointers.Ref)
   is abstract;
   --  Register an idle-state receiver.

   type Module_Instance (<>) is synchronized
     new My_Modules.Module_Instance
     and Idle_Notification_Emitter
     and Controller_Interfaces.Idle_Notification_Receiver with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;
   --  Create a module instance.

private

   package Idle_Notification_Receiver_Vectors is new
     Ada.Containers.Vectors
       (Positive,
        My_Modules.Module_Instance_Shared_Pointers.Ref,
        "=" => My_Modules.Module_Instance_Shared_Pointers."=");

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);
   --  Reject G-code dispatch.

   protected type Module_Instance is new My_Modules.Module_Instance
   and Idle_Notification_Emitter
   and Controller_Interfaces.Idle_Notification_Receiver with
      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      overriding
      procedure Request_Idle_Notifications (Receiver : My_Modules.Module_Instance_Shared_Pointers.Ref);

      overriding
      procedure Idle_Start;

      overriding
      procedure Idle_End;
   private
      Receivers : Idle_Notification_Receiver_Vectors.Vector;
   end Module_Instance;

end Prunt.Default_Modules.Idle_Emitter;
