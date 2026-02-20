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

with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Task_Termination;

package Prunt.Exception_Occurrence_Holders is

   protected type Exception_Occurrence_Holder_Type is
      function Is_Set return Boolean;
      --  Check if any exceptions have been stored.

      procedure Set_Fatal
        (Cause      : Ada.Task_Termination.Cause_Of_Termination;
         ID         : Ada.Task_Identification.Task_Id;
         Occurrence : Ada.Exceptions.Exception_Occurrence)
      with Post => Is_Set;
      --  Store an exception if no fatal exception has been stored previously. Also prints all exceptions.

      procedure Set_Recoverable
        (Cause      : Ada.Task_Termination.Cause_Of_Termination;
         ID         : Ada.Task_Identification.Task_Id;
         Occurrence : Ada.Exceptions.Exception_Occurrence)
      with Post => Is_Set;
      --  Store an exception if no exception has been stored previously. Also prints all exceptions.

      entry Get (Occurrence : out Ada.Exceptions.Exception_Occurrence; Is_Fatal : out Boolean);
      --  Get the stored exception. Blocks until an exception is available.

      entry Enter_When_Fatal_Set;

      procedure Reset;
   private
      function Null_Occurrence return Ada.Exceptions.Exception_Occurrence;
      Data                    : aliased Ada.Exceptions.Exception_Occurrence := Null_Occurrence;
      Fatal_Occurrence_Stored : Boolean := False;
   end Exception_Occurrence_Holder_Type;

end Prunt.Exception_Occurrence_Holders;
