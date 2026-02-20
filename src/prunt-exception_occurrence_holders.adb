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

with Ada.Exceptions.Is_Null_Occurrence;
with Ada.Text_IO;

package body Prunt.Exception_Occurrence_Holders is

   protected body Exception_Occurrence_Holder_Type is
      procedure Set_Fatal
        (Cause      : Ada.Task_Termination.Cause_Of_Termination;
         ID         : Ada.Task_Identification.Task_Id;
         Occurrence : Ada.Exceptions.Exception_Occurrence)
      is
         use type Ada.Task_Termination.Cause_Of_Termination;

         pragma Unreferenced (ID);
      begin
         if Cause = Ada.Task_Termination.Normal then
            return;
         end if;

         if Cause = Ada.Task_Termination.Abnormal then
            --  TODO: This indicates that a task was aborted. What is the correct action here?
            return;
         end if;

         if Ada.Exceptions.Is_Null_Occurrence (Data) or else not Fatal_Occurrence_Stored then
            Ada.Exceptions.Save_Occurrence (Data, Occurrence);
            Fatal_Occurrence_Stored := True;
         end if;

         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Occurrence));
      end Set_Fatal;

      procedure Set_Recoverable
        (Cause      : Ada.Task_Termination.Cause_Of_Termination;
         ID         : Ada.Task_Identification.Task_Id;
         Occurrence : Ada.Exceptions.Exception_Occurrence)
      is
         use type Ada.Task_Termination.Cause_Of_Termination;

         pragma Unreferenced (ID);
      begin
         if Cause = Ada.Task_Termination.Normal then
            return;
         end if;

         if Cause = Ada.Task_Termination.Abnormal then
            --  TODO: This indicates that a task was aborted. What is the correct action here?
            return;
         end if;

         if Ada.Exceptions.Is_Null_Occurrence (Data) then
            Ada.Exceptions.Save_Occurrence (Data, Occurrence);
         end if;

         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Occurrence));
      end Set_Recoverable;

      entry Get (Occurrence : out Ada.Exceptions.Exception_Occurrence; Is_Fatal : out Boolean)
        when not Ada.Exceptions.Is_Null_Occurrence (Data)
      is
      begin
         Is_Fatal := Fatal_Occurrence_Stored;
         Ada.Exceptions.Save_Occurrence (Occurrence, Data);
      end Get;

      entry Enter_When_Fatal_Set when Fatal_Occurrence_Stored is
      begin
         null;
      end Enter_When_Fatal_Set;

      function Is_Set return Boolean
      is (not Ada.Exceptions.Is_Null_Occurrence (Data));

      procedure Reset is
      begin
         if not Fatal_Occurrence_Stored then
            Ada.Exceptions.Save_Occurrence (Data, Ada.Exceptions.Null_Occurrence);
         end if;
      end Reset;

      function Null_Occurrence return Ada.Exceptions.Exception_Occurrence is
      begin
         return X : Ada.Exceptions.Exception_Occurrence do
            Ada.Exceptions.Save_Occurrence (X, Ada.Exceptions.Null_Occurrence);
         end return;
      end Null_Occurrence;
   end Exception_Occurrence_Holder_Type;

end Prunt.Exception_Occurrence_Holders;
