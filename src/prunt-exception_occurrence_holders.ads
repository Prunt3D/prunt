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

      procedure Get_Snapshot (Occurrence : out Ada.Exceptions.Exception_Occurrence; Is_Fatal : out Boolean);
      --  Copy the current exception state without blocking. Occurrence is null when no exception has been set.

      entry Enter_When_Fatal_Set;

      procedure Reset;
   private
      function Null_Occurrence return Ada.Exceptions.Exception_Occurrence;
      Data                    : aliased Ada.Exceptions.Exception_Occurrence := Null_Occurrence;
      Fatal_Occurrence_Stored : Boolean := False;
   end Exception_Occurrence_Holder_Type;

end Prunt.Exception_Occurrence_Holders;
