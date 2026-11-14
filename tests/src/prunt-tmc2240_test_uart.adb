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

package body Prunt.TMC2240_Test_UART is

   use TMC_Types.TMC2240;

   type Register_Flags is array (UART_Register_Address) of Boolean;

   Stored  : array (UART_Register_Address) of UART_Data_Byte_Array := [others => [others => 0]];
   Written : Register_Flags := [others => False];
   Dropped : Register_Flags := [others => False];

   procedure Reset is
   begin
      Written := [others => False];
      Dropped := [others => False];
   end Reset;

   procedure Drop_Next_Readback (Register : UART_Register_Address) is
   begin
      Dropped (Register) := True;
   end Drop_Next_Readback;

   function Was_Written (Register : UART_Register_Address) return Boolean
   is (Written (Register));

   function CHOPCONF return TMC_Types.TMC2240.CHOPCONF is
      Frame : UART_Data_Message;
   begin
      Frame.Bytes := Stored (CHOPCONF_Address);
      return Frame.Content.CHOPCONF_Data;
   end CHOPCONF;

   procedure Write (Message : UART_Data_Byte_Array) is
      Frame : UART_Data_Message;
   begin
      Frame.Bytes := Message;
      Stored (Frame.Content.Register) := Message;
      Written (Frame.Content.Register) := True;
   end Write;

   procedure Read (Message : UART_Query_Byte_Array; Receive_Failed : out Boolean; Reply : out UART_Data_Byte_Array) is
      Query : UART_Query_Message;
      Frame : UART_Data_Message;
   begin
      Query.Bytes := Message;
      declare
         Register : constant UART_Register_Address := Query.Content.Register;
      begin
         if not Written (Register) then
            Receive_Failed := True;
            Reply := [others => 0];
            return;
         end if;

         Frame.Bytes := Stored (Register);
         Frame.Content.Node := 255;
         Frame.Content.Is_Write := TMC_Types.False;
         Frame.Content.CRC := Compute_CRC (Frame);
         Reply := Frame.Bytes;
         Receive_Failed := Dropped (Register);
         Dropped (Register) := False;
      end;
   end Read;

end Prunt.TMC2240_Test_UART;
