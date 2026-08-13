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

with Prunt.Mockable.Text_IO;

package body Prunt.Gcode_Queues.Test is

   pragma Extensions_Allowed (On);

   procedure Test_Barrier_False (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Queue;
      Line : Virtual_String;
      Source : Queue_Item_Source;
      End_Of_Item : Boolean;
      Stopped : Boolean;

      select
         Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
         T.Fail ("Barrier should be closed");
      else
         null;
      end select;
   end Test_Barrier_False;

   procedure Test_Cancel_All (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Succeeded : Boolean;
      Filename : constant String := Prunt.Next_Test_Filename;
      File : Mockable.Text_IO.File_Type;
      Q : Queue;

      Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
      File.Put_Line ("G1 X1");
      File.Close;

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue file");

      Q.Cancel_All;
      T.Assert (Q.Get_Current_File = "", "File not cancelled");
      T.Assert (Q.Get_Current_Line_Number = 0, "Line number not reset");

      Q.Try_Set_Command ("M112", 1, Succeeded);
      T.Assert (Succeeded, "Failed to queue command after cancelling file");
      Q.Cancel_All;
      T.Assert (Q.Get_Current_Command = "", "Command not cancelled");
   end Test_Cancel_All;

   procedure Test_Cancel_Command (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Succeeded : Boolean;
      Q : Queue;

      Q.Try_Set_Command ("M112", 1, Succeeded);
      T.Assert (Succeeded, "Failed to queue command");

      Q.Cancel_Command;
      T.Assert (Q.Get_Current_Command = "", "Command not cancelled");
   end Test_Cancel_Command;

   procedure Test_Cancel_File (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Succeeded : Boolean;
      Filename : constant String := Prunt.Next_Test_Filename;
      File : Mockable.Text_IO.File_Type;
      Q : Queue;

      Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
      File.Put_Line ("M112");
      File.Close;

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue file");

      Q.Cancel_File;
      T.Assert (Q.Get_Current_File = "", "File not cancelled");
   end Test_Cancel_File;

   procedure Test_Cancel_File_No_File (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Queue;

      Q.Cancel_File;
   end Test_Cancel_File_No_File;

   procedure Test_Command_Queue_Success (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Succeeded : Boolean;
      Line : Virtual_String;
      Source : Queue_Item_Source;
      End_Of_Item : Boolean;
      Stopped : Boolean;
      Cmd : constant Virtual_String := "G28";
      Q : Queue;

      Q.Try_Set_Command (Cmd, 42, Succeeded);
      T.Assert (Succeeded, "Failed to queue command");
      T.Assert (Q.Get_Current_Command = Cmd, "Current command not set");

      Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
      T.Assert (Line = Cmd, "Wrong command line");
      T.Assert (Source.Kind = Command_Item, "Wrong item kind");
      T.Assert (Source.Command_ID = 42, "Wrong command ID");
      T.Assert (End_Of_Item, "Command should end the item");
      T.Assert (not Stopped, "Command should not stop the queue");
      T.Assert (Q.Get_Current_Command = "", "Current command should be cleared");
   end Test_Command_Queue_Success;

   procedure Test_Conflicts (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Succeeded : Boolean;
      Filename : constant String := Prunt.Next_Test_Filename;
      File : Mockable.Text_IO.File_Type;
      Q : Queue;

      Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
      File.Put_Line ("G1");
      File.Close;

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Setup failed");

      Q.Try_Set_File ("other.gcode", Succeeded);
      T.Assert (not Succeeded, "Should fail to set file when file active");

      Q.Try_Set_Command ("G28", 1, Succeeded);
      T.Assert (not Succeeded, "Should fail to set command when file active");

      Q.Cancel_File;
      Q.Try_Set_Command ("G28", 2, Succeeded);
      T.Assert (Succeeded, "Try_Set_Command failed");

      Q.Try_Set_Command ("G29", 3, Succeeded);
      T.Assert (not Succeeded, "Should fail to set command when command active");

      Q.Cancel_Command;
   end Test_Conflicts;

   procedure Test_Empty_File_Requeue (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Succeeded : Boolean;
      Filename : constant String := Prunt.Next_Test_Filename;
      File : Mockable.Text_IO.File_Type;
      Line : Virtual_String;
      Source : Queue_Item_Source;
      End_Of_Item : Boolean;
      Stopped : Boolean;
      Q : Queue;

      Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
      File.Close;

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue empty file");

      select
         Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
         T.Fail ("Should have requeued/blocked on empty file");
      else
         T.Assert (Q.Get_Current_File = "", "File should be closed/cleared");
      end select;
   end Test_Empty_File_Requeue;

   procedure Test_Exception_In_Try_Set_File (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Succeeded : Boolean;
      Q : Queue;

      Q.Try_Set_File ("/tmp/prunt_tests/nonexistent_file_e2wrqkjlofdsa8u9erqfwij", Succeeded);
      T.Assert (not Succeeded, "Should return False when file open fails");
   end Test_Exception_In_Try_Set_File;

   procedure Test_Execution_Order (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Queue;
      Succeeded : Boolean;
      Filename : constant String := Prunt.Next_Test_Filename;
      File : Mockable.Text_IO.File_Type;
      Line : Virtual_String;
      Source : Queue_Item_Source;
      End_Of_Item : Boolean;
      Stopped : Boolean;

      Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
      File.Put_Line ("FILE_LINE_1");
      File.Put_Line ("FILE_LINE_2");
      File.Close;

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue file");

      Q.Try_Set_Command ("CMD_FAIL", 1, Succeeded);
      T.Assert (not Succeeded, "Should not be able to queue command while file is active");

      Q.Cancel_File;

      Q.Try_Set_Command ("CMD_1", 2, Succeeded);
      T.Assert (Succeeded, "Failed to queue command");

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue file over command");

      Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
      T.Assert (Line = "FILE_LINE_1");
      T.Assert (Source.Kind = File_Item, "First item should be a file");
      T.Assert (Source.File_Name = +Filename, "First line should retain its filename");
      T.Assert (Source.Line_Number = 1, "First line should have line number 1");
      T.Assert (not End_Of_Item, "First file line should not end the item");
      T.Assert (not Stopped, "File line should not stop the queue");
      Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
      T.Assert (Line = "FILE_LINE_2");
      T.Assert (Source.Kind = File_Item, "Second item should still be a file");
      T.Assert (Source.File_Name = +Filename, "Final line should retain its filename");
      T.Assert (Source.Line_Number = 2, "Final line should have line number 2");
      T.Assert (End_Of_Item, "Last file line should end the item");
      T.Assert (not Stopped, "File line should not stop the queue");
      Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
      T.Assert (Line = "CMD_1");
      T.Assert (Source.Kind = Command_Item, "Last item should be a direct command");
      T.Assert (Source.Command_ID = 2, "Last item should retain its command ID");
      T.Assert (End_Of_Item, "Direct command should end the item");
      T.Assert (not Stopped, "Command should not stop the queue");

      T.Assert (Q.Get_Current_Command = "", "Command should be cleared");
      T.Assert (Q.Get_Current_File = "", "File should be cleared");
   end Test_Execution_Order;

   procedure Test_File_Queue_Success (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Queue;
      Filename : constant String := Prunt.Next_Test_Filename;
      File : Mockable.Text_IO.File_Type;

      Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
      File.Put_Line ("G1 X10");
      File.Put_Line ("G1 Y10");
      File.Close;

      Succeeded : Boolean;
      Line : Virtual_String;
      Source : Queue_Item_Source;
      End_Of_Item : Boolean;
      Stopped : Boolean;

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue file");
      T.Assert (Q.Get_Current_File = +Filename, "Current file not set");

      Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
      T.Assert (Line = "G1 X10", "Wrong first line");
      T.Assert (Source.Kind = File_Item, "Wrong item kind for first line");
      T.Assert (not End_Of_Item, "First file line should not end the item");
      T.Assert (not Stopped, "File line should not stop the queue");
      T.Assert (Q.Get_Current_File = +Filename, "Current file cleared too early");

      Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
      T.Assert (Line = "G1 Y10", "Wrong second line");
      T.Assert (Source.Kind = File_Item, "Wrong item kind for second line");
      T.Assert (Source.File_Name = +Filename, "Final line should retain its filename");
      T.Assert (Source.Line_Number = 2, "Final line should retain its line number");
      T.Assert (End_Of_Item, "Second file line should end the item");
      T.Assert (not Stopped, "File line should not stop the queue");

      T.Assert (Q.Get_Current_File = "", "Current file should be cleared after last line");
   end Test_File_Queue_Success;

   procedure Test_File_Item_Boundary_With_Trailing_Comments (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Queue;
      Filename : constant String := Prunt.Next_Test_Filename;
      File : Mockable.Text_IO.File_Type;
      Succeeded : Boolean;
      Line : Virtual_String;
      Source : Queue_Item_Source;
      End_Of_Item : Boolean;
      Stopped : Boolean;

      Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
      File.Put_Line ("G1 X10");
      File.Put_Line ("; trailing comment");
      File.Put_Line ("");
      File.Close;

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue file");

      Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
      T.Assert (Line = "G1 X10", "Wrong motion line");
      T.Assert (Source.Kind = File_Item, "Wrong item kind for first line");
      T.Assert (not End_Of_Item, "First line should not end the item");
      T.Assert (not Stopped, "Motion line should not stop the queue");

      Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
      T.Assert (Line = "; trailing comment", "Wrong comment line");
      T.Assert (Source.Kind = File_Item, "Wrong item kind for comment line");
      T.Assert (not End_Of_Item, "Comment line should not end the item when a blank line follows");
      T.Assert (not Stopped, "Comment line should not stop the queue");

      Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
      T.Assert (Line.Is_Empty, "Wrong blank line");
      T.Assert (Source.Kind = File_Item, "Wrong item kind for blank line");
      T.Assert (End_Of_Item, "Blank line should still carry the file boundary");
      T.Assert (not Stopped, "Blank line should not stop the queue");
   end Test_File_Item_Boundary_With_Trailing_Comments;

   procedure Test_Stop_Waiting (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Queue;
      Line : Virtual_String;
      Source : Queue_Item_Source;
      End_Of_Item : Boolean;
      Stopped : Boolean;

      Q.Stop_Waiting;
      Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);

      T.Assert (Stopped, "Stop_Waiting should wake Get_Next_Line");
      T.Assert (Line.Is_Empty, "Stopped wakeup should not return a line");
      T.Assert (not End_Of_Item, "Stopped wakeup should not report an item boundary");

      select
         Q.Get_Next_Line (Line, Source, End_Of_Item, Stopped);
         T.Fail ("Barrier should be closed after the stop request is consumed");
      else
         null;
      end select;
   end Test_Stop_Waiting;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Barrier_False'Unrestricted_Access,
         Test_Cancel_All'Unrestricted_Access,
         Test_Cancel_Command'Unrestricted_Access,
         Test_Cancel_File'Unrestricted_Access,
         Test_Cancel_File_No_File'Unrestricted_Access,
         Test_Command_Queue_Success'Unrestricted_Access,
         Test_Conflicts'Unrestricted_Access,
         Test_Empty_File_Requeue'Unrestricted_Access,
         Test_Exception_In_Try_Set_File'Unrestricted_Access,
         Test_Execution_Order'Unrestricted_Access,
         Test_File_Queue_Success'Unrestricted_Access,
         Test_File_Item_Boundary_With_Trailing_Comments'Unrestricted_Access,
         Test_Stop_Waiting'Unrestricted_Access];
   end All_Tests;

end Prunt.Gcode_Queues.Test;
