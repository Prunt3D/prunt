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

private with Ada.Finalization;
private with Ada.Containers.Doubly_Linked_Lists;

package Prunt.Logger is

   type Receiver is access procedure (Message : String);

   type Handle is tagged limited private;

   procedure Set_Receiver (Log_Handle : in out Handle; Log_Receiver : Receiver);
   --  Set a receiver for log messages. Log_Receiver may be null. Messages will stop being sent after Log_Handle is
   --  finalized.

   procedure Log (Message : String);
   --  Call all log receivers with the given message as the parameter.

private

   package Receiver_Lists is new Ada.Containers.Doubly_Linked_Lists (Receiver);

   type Handle is new Ada.Finalization.Limited_Controlled with record
      Cursor : Receiver_Lists.Cursor := Receiver_Lists.No_Element;
   end record;

   overriding procedure Initialize (Object : in out Handle);
   overriding procedure Finalize (Object : in out Handle);

   protected Handler is
      procedure Initialize (Log_Handle : in out Handle);
      procedure Finalize (Log_Handle : in out Handle);
      procedure Set_Receiver (Log_Handle : in out Handle; Log_Receiver : Receiver);
      procedure Log (Message : String);
   private
      Receivers : Receiver_Lists.List;
   end Handler;

end Prunt.Logger;
