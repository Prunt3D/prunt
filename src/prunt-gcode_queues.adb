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

      function Get_Current_Command return Virtual_String is
      begin
         return Current_Command;
      end Get_Current_Command;

      entry Get_Next_Line (Line : out Virtual_String) when Current_File.Is_Open or else Current_Command /= "" is
      begin
         if Current_File.Is_Open then
            if Current_File.End_Of_File then
               --  The file could be empty so we need to check this before returning a line.
               Current_File.Close;
               Current_File_Name := "";
               requeue Get_Next_Line with abort;
            end if;

            Line := Conversions.To_Virtual_String (Mockable.Text_IO.Unbounded_IO.Get_Line (Current_File));

            if Current_File.End_Of_File then
               Current_File.Close;
               Current_File_Name := "";
            end if;
         else
            Line := Current_Command;
            Current_Command := "";
         end if;
      end Get_Next_Line;
   end Queue;

end Prunt.Gcode_Queues;
