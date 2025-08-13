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

package body Prunt.TMC_Readings_Updater_Blocker is

   protected body Pause_Counter is
      procedure Pause_Readings_Updater is
      begin
         Pause_Count := Pause_Count + 1;
      end Pause_Readings_Updater;

      procedure Resume_Readings_Updater is
      begin
         if Pause_Count = 0 then
            raise Constraint_Error with "Too many Resume_Readings_Updater calls.";
         end if;
         Pause_Count := Pause_Count - 1;
      end Resume_Readings_Updater;

      entry Wait_Until_Unpaused when Pause_Count = 0 is
      begin
         null;
      end Wait_Until_Unpaused;
   end Pause_Counter;

   overriding
   procedure Initialize (My_Blocker : in out Blocker) is
      pragma Unreferenced (My_Blocker);
   begin
      Pause_Counter.Pause_Readings_Updater;
   end Initialize;

   overriding
   procedure Finalize (My_Blocker : in out Blocker) is
      pragma Unreferenced (My_Blocker);
   begin
      Pause_Counter.Resume_Readings_Updater;
   end Finalize;

   procedure Wait_Until_Unpaused is
   begin
      Pause_Counter.Wait_Until_Unpaused;
   end Wait_Until_Unpaused;

end Prunt.TMC_Readings_Updater_Blocker;
