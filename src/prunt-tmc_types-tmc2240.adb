-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
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

package body Prunt.TMC_Types.TMC2240 is

   function Compute_CRC (Bytes : UART_Bytes_For_CRC) return UART_CRC is
      CRC : UART_CRC := 0;
   begin
      for I in reverse Bytes'Range loop
         declare
            Current_Byte : UART_Byte := Bytes (I);
         begin
            for J in 0 .. 7 loop
               if UART_Byte (CRC / 2**7) = (Current_Byte and 1) then
                  CRC := CRC * 2;
               else
                  CRC := (CRC * 2) xor 7;
               end if;
               Current_Byte := Current_Byte / 2;
            end loop;
         end;
      end loop;

      return CRC;
   end Compute_CRC;

   function Compute_CRC (Message : UART_Data_Message) return UART_CRC is
   begin
      return Compute_CRC (UART_Bytes_For_CRC (Message.Bytes (2 .. 8)));
   end Compute_CRC;

   function Compute_CRC (Message : UART_Query_Message) return UART_CRC is
   begin
      return Compute_CRC (UART_Bytes_For_CRC (Message.Bytes (2 .. 4)));
   end Compute_CRC;

end Prunt.TMC_Types.TMC2240;
