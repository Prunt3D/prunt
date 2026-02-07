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

with Prunt.Mockable.Text_IO.Unbounded_IO;
with VSS.Strings.Conversions;

package body Prunt.Gcode_Queues is

   protected body Queue is
      procedure Try_Set_File (File_Name : Virtual_String; Succeeded : out Boolean) is
      begin
         if Current_File.Is_Open then
            Succeeded := False;
         else
            begin
               Current_File_Name := File_Name;
               Current_File.Open (Mode => Mockable.Text_IO.In_File, Name => Conversions.To_UTF_8_String (File_Name));
               Succeeded := True;
            exception
               --  TODO: Handle specific exceptions.
               when others =>
                  Succeeded := False;
            end;
         end if;
      end Try_Set_File;

      procedure Stop_Waiting is
      begin
         Stop_Requested := True;
      end Stop_Waiting;

      procedure Cancel_File is
      begin
         if Current_File.Is_Open then
            Current_File.Close;
            Current_File_Name := "";
         end if;
      end Cancel_File;

      procedure Try_Set_Command (Command : Virtual_String; Succeeded : out Boolean) is
      begin
         if Current_File.Is_Open or else Current_Command /= "" then
            Succeeded := False;
         else
            Current_Command := Command;
            Succeeded := True;
         end if;
      end Try_Set_Command;

      procedure Cancel_Command is
      begin
         Current_Command := "";
      end Cancel_Command;

      function Get_Current_File return Virtual_String is
      begin
         return Current_File_Name;
      end Get_Current_File;

      function Get_Current_Line_Number return File_Line_Count is
      begin
         return Current_Line_Number;
      end Get_Current_Line_Number;

      function Get_Current_Command return Virtual_String is
      begin
         return Current_Command;
      end Get_Current_Command;

      entry Get_Next_Line
        (Line : out Virtual_String; Item_Kind : out Queue_Item_Kind; End_Of_Item : out Boolean; Stopped : out Boolean)
        when Stop_Requested or else Current_File.Is_Open or else Current_Command /= ""
      is
      begin
         if Stop_Requested then
            Line := "";
            Item_Kind := Command_Item;
            End_Of_Item := False;
            Stopped := True;
            Stop_Requested := False;
         elsif Current_File.Is_Open then
            if Current_File.End_Of_File then
               --  The file could be empty so we need to check this before returning a line.
               Current_File.Close;
               Current_File_Name := "";
               Current_Line_Number := 0;
               requeue Get_Next_Line with abort;
            end if;

            Line := Conversions.To_Virtual_String (Mockable.Text_IO.Unbounded_IO.Get_Line (Current_File));
            Current_Line_Number := @ + 1;
            Item_Kind := File_Item;
            End_Of_Item := Current_File.End_Of_File;
            Stopped := False;

            if End_Of_Item then
               Current_File.Close;
               Current_File_Name := "";
               Current_Line_Number := 0;
            end if;
         else
            Line := Current_Command;
            Item_Kind := Command_Item;
            End_Of_Item := True;
            Stopped := False;
            Current_Command := "";
         end if;
      end Get_Next_Line;
   end Queue;

end Prunt.Gcode_Queues;
