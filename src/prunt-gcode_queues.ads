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

   type Queue_Item_Kind is (Command_Item, File_Item);

   type Queue_Item_Source (Kind : Queue_Item_Kind := Command_Item) is record
      case Kind is
         when Command_Item =>
            Command_ID : Gcode_Command_ID := 0;

         when File_Item =>
            File_Name   : Virtual_String;
            Line_Number : File_Line_Count := 0;
      end case;
   end record;
   --  Immutable origin information captured when a line is removed from the queue.

   protected type Queue is
      procedure Try_Set_File (File_Name : Virtual_String; Succeeded : out Boolean);
      --  Set the next file to run as soon as possible. Succeeded is set to False if a file is already enqueued or
      --  running.

      procedure Stop_Waiting;
      --  Wake any task blocked in Get_Next_Line and have its next call return with Stopped = True.

      procedure Cancel_File;
      --  Cancel the currently enqueued or running file.

      procedure Try_Set_Command (Command : Virtual_String; Command_ID : Gcode_Command_ID; Succeeded : out Boolean);
      --  Set a command to run as soon as possible. Will only succeed if no file is enqueued or running and no command
      --  is enqueued. Command_ID is returned in the immutable source snapshot when the command is dequeued.

      procedure Cancel_Command;
      --  Cancel the currently enqueued command.

      procedure Cancel_All;
      --  Cancel all queued or running G-code and forget the current file.

      function Get_Current_File return Virtual_String;
      --  Get the name of the currently running file. Returns empty string if no file is running.

      function Get_Current_Line_Number return File_Line_Count;

      function Get_Current_Command return Virtual_String;
      --  Get the currently running command. Returns empty string if no command is running.

      entry Get_Next_Line
        (Line : out Virtual_String; Source : out Queue_Item_Source; End_Of_Item : out Boolean; Stopped : out Boolean);
      --  Get the next gcode line to process. Once the last line in a file is read the file will be cleared and
      --  Try_Set_File will succeed. Once a direct command is read then Try_Set_Command will succeed as long as there
      --  is not also a file queued.
      --
      --  End_Of_Item is set to True when Line is the final line for the submitted command or file item.
      --
      --  Stopped is set to True when Stop_Waiting has been requested and no line should be processed.
   private
      Current_File        : Mockable.Text_IO.File_Type;
      Current_File_Name   : Virtual_String := "";
      Current_Command     : Virtual_String := "";
      Current_Command_ID  : Gcode_Command_ID := 0;
      Current_Line_Number : File_Line_Count := 0;
      Stop_Requested      : Boolean := False;
   end Queue;

end Prunt.Gcode_Queues;
