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

package body Prunt.Generic_Lock is

   pragma Extensions_Allowed (On);

   function Lock return Lock_Holder is
   begin
      Lock_Manager.Lock;
      return (Ada.Finalization.Limited_Controlled with Already_Finalized => False);
   end Lock;

   protected body Lock_Manager is
      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      procedure Unlock (Holder : in out Lock_Holder) is
      begin
         if Holder.Already_Finalized then
            return;
         end if;

         if not Locked then
            raise Constraint_Error with "Attempted unlock when not locked.";
         end if;

         Holder.Already_Finalized := True;
         Locked := False;
      end Unlock;
   end Lock_Manager;

   overriding
   procedure Finalize (Object : in out Lock_Holder) is
   begin
      Lock_Manager.Unlock (Object);
   end Finalize;

end Prunt.Generic_Lock;
