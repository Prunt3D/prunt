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

with Ada.Finalization;

generic
package Prunt.TMC_Readings_Updater_Blocker is

   type Blocker is new Ada.Finalization.Controlled with null record;
   overriding
   procedure Initialize (My_Blocker : in out Blocker);
   overriding
   procedure Finalize (My_Blocker : in out Blocker);

   procedure Wait_Until_Unpaused;

private

   protected Pause_Counter is
      procedure Pause_Readings_Updater;
      procedure Resume_Readings_Updater;
      entry Wait_Until_Unpaused;
   private
      Pause_Count : Natural := 0;
   end Pause_Counter;

end Prunt.TMC_Readings_Updater_Blocker;
