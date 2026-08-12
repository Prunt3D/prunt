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
         else
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Occurrence));
         end if;
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
         else
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Occurrence));
         end if;
      end Set_Recoverable;

      entry Get (Occurrence : out Ada.Exceptions.Exception_Occurrence; Is_Fatal : out Boolean)
        when not Ada.Exceptions.Is_Null_Occurrence (Data)
      is
      begin
         Is_Fatal := Fatal_Occurrence_Stored;
         Ada.Exceptions.Save_Occurrence (Occurrence, Data);
      end Get;

      procedure Get_Snapshot (Occurrence : out Ada.Exceptions.Exception_Occurrence; Is_Fatal : out Boolean) is
      begin
         Is_Fatal := Fatal_Occurrence_Stored;
         Ada.Exceptions.Save_Occurrence (Occurrence, Data);
      end Get_Snapshot;

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
