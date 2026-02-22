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
