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

with Prunt.TMC_Types.TMC2240;

package Prunt.TMC2240_Test_UART is
   --  A simulated TMC2240 register file shared by every motor that uses these callbacks. Calls must not overlap.

   procedure Reset;
   --  Forget all register contents and pending readback faults.

   procedure Drop_Next_Readback (Register : TMC_Types.TMC2240.UART_Register_Address);
   --  Report the next read of Register as missing. Writes still update the register.

   function Was_Written (Register : TMC_Types.TMC2240.UART_Register_Address) return Boolean;
   --  Return whether Register has been written since the last Reset.

   function CHOPCONF return TMC_Types.TMC2240.CHOPCONF
   with Pre => Was_Written (TMC_Types.TMC2240.CHOPCONF_Address);
   --  Return the last value written to CHOPCONF.

   procedure Write (Message : TMC_Types.TMC2240.UART_Data_Byte_Array);
   --  Store a register write frame.

   procedure Read
     (Message        : TMC_Types.TMC2240.UART_Query_Byte_Array;
      Receive_Failed : out Boolean;
      Reply          : out TMC_Types.TMC2240.UART_Data_Byte_Array);
   --  Reply with the stored register, or report a failure for an unwritten register or a dropped readback.

end Prunt.TMC2240_Test_UART;
