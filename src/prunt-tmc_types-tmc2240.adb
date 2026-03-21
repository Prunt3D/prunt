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

with Ada.Numerics.Generic_Elementary_Functions;

package body Prunt.TMC_Types.TMC2240 is

   pragma Extensions_Allowed (On);

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

   procedure Optimize_Spreadcycle
     (Driver_Voltage              : Voltage;
      TBL                         : TBL_Type;
      Motor_Inductance            : Inductance;
      Motor_Resistance            : Resistance;
      Motor_Peak_Current          : Current;
      TOFF                        : TOFF_Type;
      IRUN                        : Unsigned_5;
      HSTRT                       : out Unsigned_3;
      HEND                        : out Unsigned_4;
      Sum_Too_High                : out Boolean;
      Sum_Too_High_For_Full_Scale : out Boolean;
      Excessive_Heating           : out Boolean;
      Driver_Voltage_Too_Low      : out Boolean)
   is
      package Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensioned_Float);
      use Math;

      System_Cycle_Time : constant Time := 1.0 / (12_500_000.0 * hertz);
      Blank_Time        : constant Time := 1.5**Dimensionless (TBL_Type'Enum_Rep (TBL)) * 16.0 * System_Cycle_Time;
      Slow_Decay_Time   : constant Time := Dimensionless (24 + 32 * TOFF_Type'Enum_Rep (TOFF)) * System_Cycle_Time;
      Blank_Drop        : constant Current := Driver_Voltage * Blank_Time / Motor_Inductance;
      Slow_Decay_Drop   : constant Current :=
        Motor_Resistance * Motor_Peak_Current * 2.0 * Slow_Decay_Time / Motor_Inductance;
      Total_Drop        : constant Current := (Blank_Drop + Slow_Decay_Drop) * 2.0;
      Current_Scaler    : constant Dimensionless := Dimensionless (IRUN) / 32.0;
      Hysteresis_Sum    : constant Dimensionless :=
        Dimensionless'Max
          (Dimensionless'Ceiling (Total_Drop * 248.0 * Current_Scaler / Motor_Peak_Current - 8.0), -2.0);
      Hysteresis_Start  : constant Dimensionless := Dimensionless'Max (Dimensionless'Min (Hysteresis_Sum, 8.0), 1.0);
      Hysteresis_End    : constant Dimensionless := Dimensionless'Min (Hysteresis_Sum - Hysteresis_Start, 12.0);
   begin
      HSTRT := Unsigned_3 (Hysteresis_Start - 1.0);
      HEND := Unsigned_4 (Hysteresis_End + 3.0);

      Sum_Too_High := Hysteresis_Sum > 20.0;
      Sum_Too_High_For_Full_Scale := Hysteresis_Sum > 14.0;
      --  The TMC2240 datasheet says that the maximum here is 15 rather than 14, but that looks to be an off-by-one
      --  error as the default sine wave peak is 248. 248 + 16/2 = 256 but the maximum is probably actually 255.
      Excessive_Heating := 20.0 * Motor_Resistance * Motor_Peak_Current / Sqrt (2.0) < Driver_Voltage;
      Driver_Voltage_Too_Low := 2.0 * Motor_Resistance * Motor_Peak_Current / Sqrt (2.0) > Driver_Voltage;
   end Optimize_Spreadcycle;

end Prunt.TMC_Types.TMC2240;
