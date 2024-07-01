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

with Prunt.Thermistors;
with Prunt.TMC_Types.TMC2240;
with Prunt.Heaters;

generic
   --  'Image of each value of these types will be shown in the GUI. The names should correspond to names on the board.
   type Stepper_Name is (<>);
   type Heater_Name is (<>);
   type Thermistor_Name is (<>);
   type Fan_Name is (<>);
   type Input_Switch_Name is (<>);
package Prunt.Controller_Generic_Types is

   type Stepper_Position is array (Stepper_Name) of Dimensionless;
   --  Position multiplied by mm/step values provided by the user. This array is using floating point types and the
   --  numbers are not rounded. An implementation is allowed to round these values if the decimal part is not useful.

   type Fan_PWMs is array (Fan_Name) of PWM_Scale;
   type Heater_Targets is array (Heater_Name) of Temperature;

   type Queued_Command is record
      Index           : Command_Index;
      --  Monotonically increasing identifier.
      Pos             : Stepper_Position;
      --  Position to move to.
      Fans            : Fan_PWMs;
      --  Fan PWMs to set.
      Heaters         : Heater_Targets;
      --  Temperatures for heaters to target. In the case that a value is too low, it should be clipped. In the case
      --  that a value is too high, an exception should be raised.
      --
      --  TODO: We should accept a maximum value here and raise the exception within Prunt.
      Safe_Stop_After : Boolean;
      --  If True then the machine can stop after executing this move without violating kinematic constraints. If the
      --  implementation runs out of moves to execute before receiving a Safe_Stop_After move then an exception should
      --  be raised. It is recommended that an implementation buffers moves until a Safe_Stop_After move is received,
      --  at which point it should begin executing the buffer, if a buffer becomes full then execution should of course
      --  be started at that point instead.
      Loop_Until_Hit  : Boolean;
      --  If True then this move should be looped until the condition set in Setup_For_Loop_Move is met.
   end record;

   --  Vendor defined parameters:

   type Stepper_Hardware_Parameters (Kind : Stepper_Kind := Basic_Kind) is record
      Enable_Stepper  : access procedure (Stepper : Stepper_Name);
      Disable_Stepper : access procedure (Stepper : Stepper_Name);
      case Kind is
         when Basic_Kind =>
            null;
         when TMC2240_UART_Kind =>
            TMC2240_UART_Address : TMC_Types.TMC2240.UART_Node_Address;
            TMC2240_UART_Write   : access procedure (Message : TMC_Types.TMC2240.UART_Data_Byte_Array);
            --  Bytes sent in reverse order. Least significant bit sent first.
            TMC2240_UART_Read    : access procedure
              (Message        :     TMC_Types.TMC2240.UART_Query_Byte_Array;
               Receive_Failed : out Boolean;
               Reply          : out TMC_Types.TMC2240.UART_Data_Byte_Array);
            --  Bytes sent in reverse order. Least significant bit sent first. Reply received in same way.
      end case;
   end record;

   type Stepper_Hardware_Parameters_Array_Type is array (Stepper_Name) of Stepper_Hardware_Parameters;

   --  User defined parameters:

   type Heater_Thermistor_Map is array (Heater_Name) of Thermistor_Name;
   type Thermistor_Parameters_Array_Type is array (Thermistor_Name) of Thermistors.Thermistor_Parameters;

end Prunt.Controller_Generic_Types;
