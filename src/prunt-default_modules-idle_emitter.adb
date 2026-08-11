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

package body Prunt.Default_Modules.Idle_Emitter is

   pragma Extensions_Allowed (On);

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
   begin
      return Result : Module_Instance;
   end Initialize;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is
   begin
      raise Constraint_Error with "Not implemented.";
   end Gcode_Dispatch;

   protected body Module_Instance is
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         null;
      end Start;

      procedure Request_Idle_Notifications (Receiver : My_Modules.Module_Instance_Shared_Pointers.Ref) is
      begin
         if Receiver.Get.Element.all not in Idle_Notification_Receiver'Class then
            raise Constraint_Error with "Idle notification receiver does not implement the receiver interface.";
         end if;
         Receivers.Append (Receiver);
      end Request_Idle_Notifications;

      procedure Idle_Start is
      begin
         for Receiver of Receivers loop
            Idle_Notification_Receiver'Class (Receiver.Get.Element.all).Idle_Start;
         end loop;
      end Idle_Start;

      procedure Idle_End is
      begin
         for Receiver of Receivers loop
            Idle_Notification_Receiver'Class (Receiver.Get.Element.all).Idle_End;
         end loop;
      end Idle_End;
   end Module_Instance;

end Prunt.Default_Modules.Idle_Emitter;
