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

with Prunt.Controller_Generic_Types;

package Prunt_Simulator_Types is

   Maximum_Loop_Move_Tail_Length : constant Positive := 65_535;

   type Motor_Name is (X_Motor, Y_Motor, Z_Motor, E_Motor);
   type Heater_Name is (Dummy_Heater);
   type Thermistor_Name is (Dummy_Thermistor);
   type Board_Temperature_Probe_Name is (Dummy_Board_Temperature_Probe);
   type Fan_Name is (Dummy_Fan);
   type Tachometer_Name is (Dummy_Tachometer);
   type Input_Switch_Name is (X_Endstop, Y_Endstop, Z_Endstop);

   package Generic_Types is new
     Prunt.Controller_Generic_Types
       (Motor_Name                   => Motor_Name,
        Heater_Name                  => Heater_Name,
        Thermistor_Name              => Thermistor_Name,
        Board_Temperature_Probe_Name => Board_Temperature_Probe_Name,
        Fan_Name                     => Fan_Name,
        Tachometer_Name              => Tachometer_Name,
        Input_Switch_Name            => Input_Switch_Name);

end Prunt_Simulator_Types;
