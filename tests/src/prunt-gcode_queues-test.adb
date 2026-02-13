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

with Prunt.Mockable.Text_IO;
with VSS.Strings.Conversions;

package body Prunt.Gcode_Queues.Test is

   pragma Extensions_Allowed (On);

   procedure Test_Barrier_False (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Q : Queue;
      Line : Virtual_String;

      select
         Q.Get_Next_Line (Line);
         T.Fail ("Barrier should be closed");
      else
         null;
      end select;
   end Test_Barrier_False;

   procedure Test_Cancel_Command (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Succeeded : Boolean;
      Q : Queue;

      Q.Try_Set_Command ("M112", Succeeded);
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
      Cmd : constant Virtual_String := "G28";
      Q : Queue;

      Q.Try_Set_Command (Cmd, Succeeded);
      T.Assert (Succeeded, "Failed to queue command");
      T.Assert (Q.Get_Current_Command = Cmd, "Current command not set");

      Q.Get_Next_Line (Line);
      T.Assert (Line = Cmd, "Wrong command line");
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

      Q.Try_Set_Command ("G28", Succeeded);
      T.Assert (not Succeeded, "Should fail to set command when file active");

      Q.Cancel_File;
      Q.Try_Set_Command ("G28", Succeeded);
      T.Assert (Succeeded, "Try_Set_Command failed");

      Q.Try_Set_Command ("G29", Succeeded);
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
      Q : Queue;

      Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
      File.Close;

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue empty file");

      select
         Q.Get_Next_Line (Line);
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

      Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
      File.Put_Line ("FILE_LINE_1");
      File.Put_Line ("FILE_LINE_2");
      File.Close;

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue file");

      Q.Try_Set_Command ("CMD_FAIL", Succeeded);
      T.Assert (not Succeeded, "Should not be able to queue command while file is active");

      Q.Cancel_File;

      Q.Try_Set_Command ("CMD_1", Succeeded);
      T.Assert (Succeeded, "Failed to queue command");

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue file over command");

      Q.Get_Next_Line (Line);
      T.Assert (Line = "FILE_LINE_1");
      Q.Get_Next_Line (Line);
      T.Assert (Line = "FILE_LINE_2");
      Q.Get_Next_Line (Line);
      T.Assert (Line = "CMD_1");

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

      Q.Try_Set_File (+Filename, Succeeded);
      T.Assert (Succeeded, "Failed to queue file");
      T.Assert (Q.Get_Current_File = +Filename, "Current file not set");

      Q.Get_Next_Line (Line);
      T.Assert (Line = "G1 X10", "Wrong first line");
      T.Assert (Q.Get_Current_File = +Filename, "Current file cleared too early");

      Q.Get_Next_Line (Line);
      T.Assert (Line = "G1 Y10", "Wrong second line");

      T.Assert (Q.Get_Current_File = "", "Current file should be cleared after last line");
   end Test_File_Queue_Success;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Barrier_False'Unrestricted_Access,
         Test_Cancel_Command'Unrestricted_Access,
         Test_Cancel_File'Unrestricted_Access,
         Test_Cancel_File_No_File'Unrestricted_Access,
         Test_Command_Queue_Success'Unrestricted_Access,
         Test_Conflicts'Unrestricted_Access,
         Test_Empty_File_Requeue'Unrestricted_Access,
         Test_Exception_In_Try_Set_File'Unrestricted_Access,
         Test_Execution_Order'Unrestricted_Access,
         Test_File_Queue_Success'Unrestricted_Access];
   end All_Tests;

end Prunt.Gcode_Queues.Test;
