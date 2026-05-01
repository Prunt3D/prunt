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

with Prunt;
with Prunt.Thermistors;
with Prunt_Simulator_Types;

package Prunt_Simulator_Hardware is

   use Prunt;
   use Prunt_Simulator_Types;

   procedure Enable_Motor (Motor : Motor_Name);
   procedure Disable_Motor (Motor : Motor_Name);
   procedure Set_Fan_Duty_Cycle (Fan : Fan_Name; Duty_Cycle : PWM_Scale);
   procedure Reconfigure_Fan (Fan : Fan_Name; PWM_Frequency : Frequency);
   function Get_Tachometer_Frequency (Tachometer : Tachometer_Name; Requires_Fresh : Boolean) return Frequency;
   function Get_Input_Switch_State (Switch : Input_Switch_Name) return Boolean;

   procedure Reconfigure_Heater
     (Heater : Heater_Name; Params : Heater_Parameters; Assigned_Thermistor : Thermistor_Name);
   procedure Set_Heater_Temperature (Heater : Heater_Name; Target : Temperature);
   procedure Autotune_Heater
     (Heater : Heater_Name; Params : Heater_Parameters; Assigned_Thermistor : Thermistor_Name);

   procedure Reconfigure_Thermistor
     (Thermistor : Thermistor_Name; Params : Thermistors.Thermistor_Parameters);
   function Get_Thermistor_Temperature (Thermistor : Thermistor_Name; Requires_Fresh : Boolean) return Temperature;
   function Get_Board_Temperature
     (Probe : Board_Temperature_Probe_Name; Requires_Fresh : Boolean) return Temperature;

   Hardware : constant Generic_Types.Hardware_Parameters :=
     (Motor_Hardware                   =>
        [others =>
           (Kind                      => Basic_Motor_Kind,
            Maximum_Delta_Per_Command => 1.0,
            Enable                    => Enable_Motor'Access,
            Disable                   => Disable_Motor'Access)],
      Fan_Hardware                     =>
        [others =>
           (Kind                            => Fixed_Switching_Kind,
            Set_Duty_Cycle                  => Set_Fan_Duty_Cycle'Access,
            Gcode_Index                     => 0,
            Reconfigure_Fixed_Switching_Fan => Reconfigure_Fan'Access,
            Maximum_PWM_Frequency           => 25_000.0 * hertz)],
      Tachometer_Hardware              =>
        [others => (Get_Pulse_Frequency => Get_Tachometer_Frequency'Access)],
      Input_Switch_Hardware            =>
        [others => (Visible_To_User => False, Get_State => Get_Input_Switch_State'Access)],
      Heater_Hardware                  =>
        [others =>
           (Reconfigure     => Reconfigure_Heater'Access,
            Set_Temperature => Set_Heater_Temperature'Access,
            Autotune        => Autotune_Heater'Access)],
      Thermistor_Hardware              =>
        [others =>
           (Reconfigure     => Reconfigure_Thermistor'Access,
            Get_Temperature => Get_Thermistor_Temperature'Access)],
      Board_Temperature_Probe_Hardware =>
        [others => (Get_Temperature => Get_Board_Temperature'Access)]);

end Prunt_Simulator_Hardware;
