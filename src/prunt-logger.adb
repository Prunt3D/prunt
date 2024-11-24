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

with Ada.Text_IO;
with Ada.Exceptions;

package body Prunt.Logger is

   procedure Set_Receiver (Log_Handle : in out Handle; Log_Receiver : Receiver) is
   begin
      List_Handler.Set_Receiver (Log_Handle, Log_Receiver);
   end Set_Receiver;

   procedure Log (Message : String) is
   begin
      Message_Queue.Enqueue (To_Unbounded_String (Message));
   end Log;

   overriding procedure Initialize (Object : in out Handle) is
   begin
      List_Handler.Initialize (Object);
   end Initialize;

   overriding procedure Finalize (Object : in out Handle) is
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
      Message   : Unbounded_String;
      Receivers : Receiver_Lists.List;
   begin
      loop
         Message_Queue.Dequeue (Message);
         List_Handler.Update_If_Required (Receivers);
         declare
            Message_String : constant String := To_String (Message);
         begin
            Ada.Text_IO.Put_Line (Message_String);
            for R of Receivers loop
               if R /= null then
                  begin
                     R (Message_String);
                  exception
                     when E : others =>
                        Ada.Text_IO.Put_Line ("Exception in log pusher:");
                        Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
                  end;
               end if;
            end loop;
         end;
      end loop;
   end Log_Pusher;

end Prunt.Logger;
