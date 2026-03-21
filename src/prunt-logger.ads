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

pragma Extensions_Allowed (On);

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Synchronized_Queue_Interfaces;
private with Ada.Containers.Unbounded_Synchronized_Queues;
private with Ada.Finalization;

--  TODO: This doesn't really need to be a generic. It would probably be better to pass around some kind of handle type
--  to log message emitters instead of the whole package.

generic
package Prunt.Logger is

   type Receiver is access procedure (Message : Virtual_String);

   type Handle is tagged limited private;

   procedure Set_Receiver (Log_Handle : in out Handle; Log_Receiver : Receiver);
   --  Set a receiver for log messages. Log_Receiver may be null. Messages will stop being sent after Log_Handle is
   --  finalized. Updates and finalization may not apply instantly.

   procedure Log (Message : Virtual_String);
   --  Call all log receivers with the given message as the parameter. Message is placed in a queue so this procedure
   --  is unlikely to block for a long period of time, but logging may not occur instantly.

private

   package Receiver_Lists is new Ada.Containers.Doubly_Linked_Lists (Receiver);

   type Handle is new Ada.Finalization.Limited_Controlled with record
      Cursor : Receiver_Lists.Cursor := Receiver_Lists.No_Element;
   end record;

   overriding
   procedure Initialize (Object : in out Handle);
   overriding
   procedure Finalize (Object : in out Handle);

   protected List_Handler is
      procedure Initialize (Log_Handle : in out Handle);
      procedure Finalize (Log_Handle : in out Handle);
      procedure Set_Receiver (Log_Handle : in out Handle; Log_Receiver : Receiver);
      procedure Update_If_Required (Receivers_Copy : in out Receiver_Lists.List);
   private
      Receivers            : Receiver_Lists.List;
      Receivers_Has_Update : Boolean := True;
   end List_Handler;

   package Virtual_String_Queue_Interfaces is new
     Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Virtual_String);

   package Virtual_String_Queues is new
     Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Virtual_String_Queue_Interfaces);

   Message_Queue : Virtual_String_Queues.Queue;

   task Log_Pusher;

end Prunt.Logger;
