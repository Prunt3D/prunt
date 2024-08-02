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

package body Prunt.Logger is

   procedure Set_Receiver (Log_Handle : in out Handle; Log_Receiver : Receiver) is
   begin
      Handler.Set_Receiver (Log_Handle, Log_Receiver);
   end Set_Receiver;

   procedure Log (Message : String) is
   begin
      Handler.Log (Message);
   end Log;

   overriding procedure Initialize (Object : in out Handle) is
   begin
      Handler.Initialize (Object);
   end Initialize;

   overriding procedure Finalize (Object : in out Handle) is
   begin
      Handler.Finalize (Object);
   end Finalize;

   protected body Handler is
      procedure Initialize (Log_Handle : in out Handle) is
         use type Receiver_Lists.Cursor;
      begin
         if Log_Handle.Cursor /= Receiver_Lists.No_Element then
            raise Constraint_Error with "Initialize called multiple times, this should not be possible.";
         end if;

         Receivers.Insert (Receiver_Lists.No_Element, null, Log_Handle.Cursor);
      end Initialize;

      procedure Finalize (Log_Handle : in out Handle) is
      begin
         Receivers.Delete (Log_Handle.Cursor);
      end Finalize;

      procedure Set_Receiver (Log_Handle : in out Handle; Log_Receiver : Receiver) is
      begin
         Receivers.Replace_Element (Log_Handle.Cursor, Log_Receiver);
      end Set_Receiver;

      procedure Log (Message : String) is
      begin
         Ada.Text_IO.Put_Line (Message);

         for R of Receivers loop
            R (Message);
         end loop;
      end Log;
   end Handler;

end Prunt.Logger;
