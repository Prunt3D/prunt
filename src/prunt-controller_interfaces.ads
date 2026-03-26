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

--  This package contains various interfaces for modules which require special handling by the controller.

pragma Extensions_Allowed (On);

package Prunt.Controller_Interfaces is

   type Idle_Notification_Receiver is synchronized interface;

   procedure Idle_Start (This : in out Idle_Notification_Receiver) is abstract;
   --  Called when command execution catches up to the last emitted command and there is no end--of-block handler
   --  running.

   procedure Idle_End (This : in out Idle_Notification_Receiver) is abstract;
   --  Called before a new command is emitted after command execution catches up to the last emitted command. This
   --  procedure is allowed to block to stop the given command from being enqueued until the machine is in a state
   --  where it is ready to do so. For example, a module might need to wait for heaters to heat back up.

end Prunt.Controller_Interfaces;
