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

generic
   --  'Image of each value of these types will be shown in the GUI. The names should correspond to names on the board.
   type Stepper_Name is (<>);
   type Heater_Name is (<>);
   type Thermistor_Name is (<>);
   type Fan_Name is (<>);
   type Input_Switch_Name is (<>);
package Prunt.Controller_Generic_Types is

   type Stepper_Position is array (Stepper_Name) of Dimensionless;
   type Fan_PWMs is array (Fan_Name) of PWM_Scale;
   type Heater_Targets is array (Heater_Name) of Temperature;

   type Queued_Command is record
      Index           : Command_Index;
      Pos             : Stepper_Position;
      Fans            : Fan_PWMs;
      Heaters         : Heater_Targets;
      Safe_Stop_After : Boolean;
      Loop_Until_Hit  : Boolean;
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

   type Heater_Kind is (Disabled_Kind, PID_Kind, Bang_Bang_Kind);

   type Heater_Parameters (Kind : Heater_Kind := Disabled_Kind) is record
      Thermistor : Thermistor_Name := Thermistor_Name'First;
      case Kind is
         when Disabled_Kind =>
            null;
         when PID_Kind =>
            Proportional_Scale          : Dimensionless := 0.0;
            Integral_Scale              : Dimensionless := 0.0;
            Derivative_Scale            : Dimensionless := 0.0;
            Proportional_On_Measurement : Boolean       := True;
         when Bang_Bang_Kind =>
            Max_Delta : Temperature := 2.0 * celcius;
      end case;
   end record;

   type Heater_Parameters_Array_Type is array (Heater_Name) of Heater_Parameters;

   type Thermistor_Parameters_Array_Type is array (Thermistor_Name) of Thermistors.Thermistor_Parameters;

end Prunt.Controller_Generic_Types;
