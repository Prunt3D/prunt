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

with Ada.Exceptions;
with Ada.Text_IO;
with VSS.Strings.Conversions;

package body Prunt.Logger is

   pragma Extensions_Allowed (On);

   procedure Set_Receiver (Log_Handle : in out Handle; Log_Receiver : Receiver) is
   begin
      List_Handler.Set_Receiver (Log_Handle, Log_Receiver);
   end Set_Receiver;

   procedure Log (Message : Virtual_String) is
   begin
      Message_Queue.Enqueue (Message);
   end Log;

   overriding
   procedure Initialize (Object : in out Handle) is
   begin
      List_Handler.Initialize (Object);
   end Initialize;

   overriding
   procedure Finalize (Object : in out Handle) is
   begin
      List_Handler.Finalize (Object);
   end Finalize;

   protected body List_Handler is
      procedure Initialize (Log_Handle : in out Handle) is
         use type Receiver_Lists.Cursor;
      begin
         if Log_Handle.Cursor /= Receiver_Lists.No_Element then
            raise Constraint_Error with "Initialize called multiple times, this should not be possible.";
         end if;

         Receivers.Insert (Receiver_Lists.No_Element, null, Log_Handle.Cursor);
         Receivers_Has_Update := True;
      end Initialize;

      procedure Finalize (Log_Handle : in out Handle) is
      begin
         Receivers.Delete (Log_Handle.Cursor);
         Receivers_Has_Update := True;
      end Finalize;

      procedure Set_Receiver (Log_Handle : in out Handle; Log_Receiver : Receiver) is
      begin
         Receivers.Replace_Element (Log_Handle.Cursor, Log_Receiver);
         Receivers_Has_Update := True;
      end Set_Receiver;

      procedure Update_If_Required (Receivers_Copy : in out Receiver_Lists.List) is
      begin
         if Receivers_Has_Update then
            Receiver_Lists.Assign (Target => Receivers_Copy, Source => Receivers);
            Receivers_Has_Update := False;
         end if;
      end Update_If_Required;
   end List_Handler;

   task body Log_Pusher is
      Message   : Virtual_String;
      Receivers : Receiver_Lists.List;
   begin
      loop
         Message_Queue.Dequeue (Message);
         List_Handler.Update_If_Required (Receivers);
         Ada.Text_IO.Put_Line (Conversions.To_UTF_8_String (Message));
         for R of Receivers loop
            if R /= null then
               begin
                  R (Message);
               exception
                  when E : others =>
                     Ada.Text_IO.Put_Line ("Exception in log pusher:");
                     Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
               end;
            end if;
         end loop;
      end loop;
   end Log_Pusher;

end Prunt.Logger;
