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

with Ada.Exceptions;
with Ada.Task_Identification;
with Ada.Task_Termination;
with System.Assertions;
with Trendy_Test; use Trendy_Test;

package body Prunt.Exception_Occurrence_Holders.Test is

   pragma Extensions_Allowed (On);

   Test_Exception  : exception;
   Other_Exception : exception;

   procedure Raise_And_Save (Occurrence : out Ada.Exceptions.Exception_Occurrence; Ex : Ada.Exceptions.Exception_Id) is
   begin
      Ada.Exceptions.Raise_Exception (Ex);
   exception
      when E : others =>
         Ada.Exceptions.Save_Occurrence (Occurrence, E);
   end Raise_And_Save;

   procedure Test_Enter_When_Fatal_Set (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      Occurrence : Ada.Exceptions.Exception_Occurrence;

      Raise_And_Save (Occurrence, Test_Exception'Identity);

      declare
         task Waiter is
            entry Done;
         end Waiter;

         task body Waiter is
         begin
            Holder.Enter_When_Fatal_Set;
            select
               accept Done;
            or
               terminate;
            end select;
         end Waiter;
      begin
         delay 0.1;
         Holder.Set_Fatal (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Occurrence);
         select
            Waiter.Done;
         or
            delay 5.0;
            T.Fail ("Timed out waiting for Enter_When_Fatal_Set");
         end select;
      end;
   end Test_Enter_When_Fatal_Set;

   procedure Test_Get_Blocks_Until_Set (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      Occurrence : Ada.Exceptions.Exception_Occurrence;
      Result : Ada.Exceptions.Exception_Occurrence;
      Got_Fatal : Boolean;

      Raise_And_Save (Occurrence, Test_Exception'Identity);

      declare
         task Getter is
            entry Done (Is_Fatal : out Boolean);
         end Getter;

         task body Getter is
            Local_Result : Ada.Exceptions.Exception_Occurrence;
            Local_Fatal  : Boolean;
         begin
            Holder.Get (Local_Result, Local_Fatal);
            Ada.Exceptions.Save_Occurrence (Result, Local_Result);
            select
               accept Done (Is_Fatal : out Boolean) do
                  Is_Fatal := Local_Fatal;
               end Done;
            or
               terminate;
            end select;
         end Getter;
      begin
         delay 0.1;
         Holder.Set_Recoverable
           (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Occurrence);
         select
            Getter.Done (Got_Fatal);
         or
            delay 5.0;
            T.Fail ("Timed out waiting for Get");
         end select;
      end;

      T.Assert (not Got_Fatal);
      T.Assert (Ada.Exceptions.Exception_Name (Result) = Ada.Exceptions.Exception_Name (Occurrence));
   end Test_Get_Blocks_Until_Set;

   procedure Test_Initial_State (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;

      T.Assert (not Holder.Is_Set);
   end Test_Initial_State;

   procedure Test_Reset_Clears_Non_Fatal (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      Occurrence : Ada.Exceptions.Exception_Occurrence;

      Raise_And_Save (Occurrence, Test_Exception'Identity);
      Holder.Set_Recoverable
        (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Occurrence);
      T.Assert (Holder.Is_Set);

      Holder.Reset;
      T.Assert (not Holder.Is_Set);
   end Test_Reset_Clears_Non_Fatal;

   procedure Test_Reset_Does_Not_Clear_Fatal (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      Occurrence : Ada.Exceptions.Exception_Occurrence;

      Raise_And_Save (Occurrence, Test_Exception'Identity);
      Holder.Set_Fatal (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Occurrence);
      T.Assert (Holder.Is_Set);

      Holder.Reset;
      T.Assert (Holder.Is_Set);
   end Test_Reset_Does_Not_Clear_Fatal;

   procedure Test_Set_Fatal_Abnormal_Cause (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      Occurrence : Ada.Exceptions.Exception_Occurrence;

      Raise_And_Save (Occurrence, Test_Exception'Identity);

      begin
         Holder.Set_Fatal (Ada.Task_Termination.Abnormal, Ada.Task_Identification.Current_Task, Occurrence);
         T.Fail ("Should have raised Assert_Failure from postcondition");
      exception
         when System.Assertions.Assert_Failure =>
            null;
      end;
   end Test_Set_Fatal_Abnormal_Cause;

   procedure Test_Set_Fatal_Does_Not_Overwrite_Fatal (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      First_Occurrence : Ada.Exceptions.Exception_Occurrence;
      Second_Occurrence : Ada.Exceptions.Exception_Occurrence;
      Result : Ada.Exceptions.Exception_Occurrence;
      Is_Fatal : Boolean;

      Raise_And_Save (First_Occurrence, Test_Exception'Identity);
      Raise_And_Save (Second_Occurrence, Other_Exception'Identity);

      Holder.Set_Fatal
        (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, First_Occurrence);
      Holder.Set_Fatal
        (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Second_Occurrence);
      Holder.Get (Result, Is_Fatal);

      T.Assert (Is_Fatal);
      T.Assert (Ada.Exceptions.Exception_Name (Result) = Ada.Exceptions.Exception_Name (First_Occurrence));
   end Test_Set_Fatal_Does_Not_Overwrite_Fatal;

   procedure Test_Set_Fatal_Normal_Cause (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      Occurrence : Ada.Exceptions.Exception_Occurrence;

      Raise_And_Save (Occurrence, Test_Exception'Identity);

      begin
         Holder.Set_Fatal (Ada.Task_Termination.Normal, Ada.Task_Identification.Current_Task, Occurrence);
         T.Fail ("Should have raised Assert_Failure from postcondition");
      exception
         when System.Assertions.Assert_Failure =>
            null;
      end;
   end Test_Set_Fatal_Normal_Cause;

   procedure Test_Set_Fatal_Overwrites_Recoverable (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      Recoverable_Occurrence : Ada.Exceptions.Exception_Occurrence;
      Fatal_Occurrence : Ada.Exceptions.Exception_Occurrence;
      Result : Ada.Exceptions.Exception_Occurrence;
      Is_Fatal : Boolean;

      Raise_And_Save (Recoverable_Occurrence, Test_Exception'Identity);
      Raise_And_Save (Fatal_Occurrence, Other_Exception'Identity);

      Holder.Set_Recoverable
        (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Recoverable_Occurrence);
      Holder.Set_Fatal
        (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Fatal_Occurrence);
      Holder.Get (Result, Is_Fatal);

      T.Assert (Is_Fatal);
      T.Assert (Ada.Exceptions.Exception_Name (Result) = Ada.Exceptions.Exception_Name (Fatal_Occurrence));
   end Test_Set_Fatal_Overwrites_Recoverable;

   procedure Test_Set_Recoverable_Abnormal_Cause (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      Occurrence : Ada.Exceptions.Exception_Occurrence;

      Raise_And_Save (Occurrence, Test_Exception'Identity);

      begin
         Holder.Set_Recoverable (Ada.Task_Termination.Abnormal, Ada.Task_Identification.Current_Task, Occurrence);
         T.Fail ("Should have raised Assert_Failure from postcondition");
      exception
         when System.Assertions.Assert_Failure =>
            null;
      end;
   end Test_Set_Recoverable_Abnormal_Cause;

   procedure Test_Set_Recoverable_Does_Not_Overwrite (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      First_Occurrence : Ada.Exceptions.Exception_Occurrence;
      Second_Occurrence : Ada.Exceptions.Exception_Occurrence;
      Result : Ada.Exceptions.Exception_Occurrence;
      Is_Fatal : Boolean;

      Raise_And_Save (First_Occurrence, Test_Exception'Identity);
      Raise_And_Save (Second_Occurrence, Other_Exception'Identity);

      Holder.Set_Recoverable
        (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, First_Occurrence);
      Holder.Set_Recoverable
        (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Second_Occurrence);
      Holder.Get (Result, Is_Fatal);

      T.Assert (not Is_Fatal);
      T.Assert (Ada.Exceptions.Exception_Name (Result) = Ada.Exceptions.Exception_Name (First_Occurrence));
   end Test_Set_Recoverable_Does_Not_Overwrite;

   procedure Test_Set_Recoverable_Normal_Cause (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      Occurrence : Ada.Exceptions.Exception_Occurrence;

      Raise_And_Save (Occurrence, Test_Exception'Identity);

      begin
         Holder.Set_Recoverable (Ada.Task_Termination.Normal, Ada.Task_Identification.Current_Task, Occurrence);
         T.Fail ("Should have raised Assert_Failure from postcondition");
      exception
         when System.Assertions.Assert_Failure =>
            null;
      end;
   end Test_Set_Recoverable_Normal_Cause;

   procedure Test_Set_Recoverable_Sets (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Holder : Exception_Occurrence_Holder_Type;
      Occurrence : Ada.Exceptions.Exception_Occurrence;
      Result : Ada.Exceptions.Exception_Occurrence;
      Is_Fatal : Boolean;

      Raise_And_Save (Occurrence, Test_Exception'Identity);
      Holder.Set_Recoverable
        (Ada.Task_Termination.Unhandled_Exception, Ada.Task_Identification.Current_Task, Occurrence);

      T.Assert (Holder.Is_Set);
      Holder.Get (Result, Is_Fatal);
      T.Assert (not Is_Fatal);
      T.Assert (Ada.Exceptions.Exception_Name (Result) = Ada.Exceptions.Exception_Name (Occurrence));
   end Test_Set_Recoverable_Sets;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Enter_When_Fatal_Set'Access,
         Test_Get_Blocks_Until_Set'Access,
         Test_Initial_State'Access,
         Test_Reset_Clears_Non_Fatal'Access,
         Test_Reset_Does_Not_Clear_Fatal'Access,
         Test_Set_Fatal_Abnormal_Cause'Access,
         Test_Set_Fatal_Does_Not_Overwrite_Fatal'Access,
         Test_Set_Fatal_Normal_Cause'Access,
         Test_Set_Fatal_Overwrites_Recoverable'Access,
         Test_Set_Recoverable_Abnormal_Cause'Access,
         Test_Set_Recoverable_Does_Not_Overwrite'Access,
         Test_Set_Recoverable_Normal_Cause'Access,
         Test_Set_Recoverable_Sets'Access];
   end All_Tests;

end Prunt.Exception_Occurrence_Holders.Test;
