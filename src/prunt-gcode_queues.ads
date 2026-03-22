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

package Prunt.Gcode_Queues is

   protected type Queue is
      procedure Try_Set_File (File_Name : Virtual_String; Succeeded : out Boolean);
      --  Set the next file to run as soon as possible. Succeeded is set to False if a file is already enqueued or
      --  running.

      procedure Cancel_File;
      --  Cancel the currently enqueued or running file.

      procedure Try_Set_Command (Command : Virtual_String; Succeeded : out Boolean);
      --  Set a command to run as soon as possible. Will only succeed if no file is enqueued or running and no command
      --  is enqueued.

      procedure Cancel_Command;
      --  Cancel the currently enqueued command.

      function Get_Current_File return Virtual_String;
      --  Get the name of the currently running file. Returns empty string if no file is running.

      function Get_Current_Line_Number return File_Line_Count;

      function Get_Current_Command return Virtual_String;
      --  Get the currently running command. Returns empty string if no command is running.

      entry Get_Next_Line (Line : out Virtual_String);
      --  Get the next gcode line to process. Once the last line in a file is read the file will be cleared and
      --  Try_Set_File will succeed. Once a direct command is read then Try_Set_Command will succeed as long as there
      --  is not also a file queued.
   private
      Current_File        : Mockable.Text_IO.File_Type;
      Current_File_Name   : Virtual_String := "";
      Current_Command     : Virtual_String := "";
      Current_Line_Number : File_Line_Count := 0;
   end Queue;

end Prunt.Gcode_Queues;
