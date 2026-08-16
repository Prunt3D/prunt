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

package body Prunt_Simulator_Hardware is

   protected Power_Supply_State is
      procedure Set (New_Value : Boolean);
      function Get return Boolean;
   private
      Value : Boolean := True;
   end Power_Supply_State;

   protected body Power_Supply_State is
      procedure Set (New_Value : Boolean) is
      begin
         Value := New_Value;
      end Set;

      function Get return Boolean is
      begin
         return Value;
      end Get;
   end Power_Supply_State;

   procedure Enable_Motor (Motor : Motor_Name) is
   begin
      pragma Unreferenced (Motor);
      null;
   end Enable_Motor;

   procedure Disable_Motor (Motor : Motor_Name) is
   begin
      pragma Unreferenced (Motor);
      null;
   end Disable_Motor;

   procedure Set_Fan_Duty_Cycle (Fan : Fan_Name; Duty_Cycle : PWM_Scale) is
   begin
      pragma Unreferenced (Fan, Duty_Cycle);
      null;
   end Set_Fan_Duty_Cycle;

   procedure Reconfigure_Fan (Fan : Fan_Name; PWM_Frequency : Frequency) is
   begin
      pragma Unreferenced (Fan, PWM_Frequency);
      null;
   end Reconfigure_Fan;

   function Get_Tachometer_Frequency (Tachometer : Tachometer_Name; Requires_Fresh : Boolean) return Frequency is
   begin
      pragma Unreferenced (Tachometer, Requires_Fresh);
      return 0.0 * hertz;
   end Get_Tachometer_Frequency;

   function Get_Input_Switch_State (Switch : Input_Switch_Name) return Boolean is
   begin
      pragma Unreferenced (Switch);
      return False;
   end Get_Input_Switch_State;

   procedure Reconfigure_Heater
     (Heater : Heater_Name; Params : Heater_Parameters; Assigned_Thermistor : Thermistor_Name) is
   begin
      pragma Unreferenced (Heater, Params, Assigned_Thermistor);
      null;
   end Reconfigure_Heater;

   procedure Set_Heater_Temperature (Heater : Heater_Name; Target : Temperature) is
   begin
      pragma Unreferenced (Heater, Target);
      null;
   end Set_Heater_Temperature;

   procedure Autotune_Heater
     (Heater : Heater_Name; Params : Heater_Parameters; Assigned_Thermistor : Thermistor_Name) is
   begin
      pragma Unreferenced (Heater, Params, Assigned_Thermistor);
      null;
   end Autotune_Heater;

   procedure Reconfigure_Thermistor
     (Thermistor : Thermistor_Name; Params : Thermistors.Thermistor_Parameters) is
   begin
      pragma Unreferenced (Thermistor, Params);
      null;
   end Reconfigure_Thermistor;

   function Get_Thermistor_Temperature (Thermistor : Thermistor_Name; Requires_Fresh : Boolean) return Temperature is
   begin
      pragma Unreferenced (Thermistor, Requires_Fresh);
      return 25.0 * celsius;
   end Get_Thermistor_Temperature;

   function Get_Board_Temperature
     (Probe : Board_Temperature_Probe_Name; Requires_Fresh : Boolean) return Temperature is
   begin
      pragma Unreferenced (Probe, Requires_Fresh);
      return 25.0 * celsius;
   end Get_Board_Temperature;

   procedure Turn_Power_Supply_On is
   begin
      Power_Supply_State.Set (True);
   end Turn_Power_Supply_On;

   procedure Turn_Power_Supply_Off is
   begin
      Power_Supply_State.Set (False);
   end Turn_Power_Supply_Off;

   function Power_Supply_Is_On return Boolean is
   begin
      return Power_Supply_State.Get;
   end Power_Supply_Is_On;

end Prunt_Simulator_Hardware;
