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

with Prunt.Logger;
with Prunt.Config;

generic
   with package My_Logger is new Prunt.Logger (<>);
   with package My_Config is new Prunt.Config (<>);
   with function Get_Position return Prunt.Position;
   with function Get_Thermistor_Temperature (Thermistor : My_Config.Thermistor_Name) return Prunt.Temperature;
   with function Get_Stepper_Temperature (Thermistor : My_Config.Stepper_Name) return Prunt.Temperature;
   type Board_Temperature_Probe_Name is (<>);
   with function Get_Board_Temperature (Thermistor : Board_Temperature_Probe_Name) return Prunt.Temperature;
   with function Get_Heater_Power (Heater : My_Config.Heater_Name) return Prunt.PWM_Scale;
   with function Get_Input_Switch_State (Switch : My_Config.Input_Switch_Name) return Prunt.Pin_State;
   with function Get_Tachometer_Frequency (Fan : My_Config.Fan_Name) return Frequency;
   with procedure Submit_Gcode_Command (Command : String; Succeeded : out Boolean);
   with procedure Submit_Gcode_File (Path : String; Succeeded : out Boolean);
   with function Is_Stepgen_Paused return Boolean;
   with procedure Pause_Stepgen;
   with procedure Resume_Stepgen;
   Fatal_Exception_Occurrence_Holder : in out Fatal_Exception_Occurrence_Holder_Type;
package Prunt.Web_Server is

   task Server;

end Prunt.Web_Server;
