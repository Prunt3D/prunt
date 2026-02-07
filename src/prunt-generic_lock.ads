-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
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

pragma Extensions_Allowed (On);

private with Ada.Finalization;

generic
package Prunt.Generic_Lock is

   type Lock_Holder (<>) is limited private;

   function Lock return Lock_Holder;
   --  with Nonblocking => False;
   --  TODO: uncomment above once GNAT adds support for the Nonblocking aspect.

private

   protected Lock_Manager is
      entry Lock;
      procedure Unlock (Holder : in out Lock_Holder);
   private
      Locked : Boolean := False;
   end Lock_Manager;

   type Atomic_Boolean is new Boolean with Atomic, Volatile;

   type Lock_Holder is new Ada.Finalization.Limited_Controlled with record
      Already_Finalized : Atomic_Boolean := False;
   end record;

   overriding
   procedure Finalize (Object : in out Lock_Holder);

end Prunt.Generic_Lock;
