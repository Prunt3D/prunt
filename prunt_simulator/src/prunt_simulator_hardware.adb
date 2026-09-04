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

with Ada.Characters.Handling;
with Ada.Environment_Variables;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Prunt_Simulator_Hardware is

   type Simulator_Kinematics_Kind is (Cartesian, Core_XY, Linear_Delta);
   type Endstop_Direction_Kind is (Minimum, Maximum);

   type Motor_Float_Array is array (Motor_Name) of Long_Float;
   type Motor_Boolean_Array is array (Motor_Name) of Boolean;
   type Endstop_Float_Array is array (Input_Switch_Name) of Long_Float;
   type Endstop_Direction_Array is array (Input_Switch_Name) of Endstop_Direction_Kind;
   type Endstop_Boolean_Array is array (Input_Switch_Name) of Boolean;

   function Motor_Label (Motor : Motor_Name) return String;
   function Endstop_Label (Switch : Input_Switch_Name) return String;
   function Environment_String (Name : String; Default : String) return String;
   function Environment_Float (Name : String; Default : Long_Float; Must_Be_Positive : Boolean) return Long_Float;
   function Environment_Boolean (Name : String; Default : Boolean) return Boolean;
   function Environment_Kinematics return Simulator_Kinematics_Kind;
   function Environment_Endstop_Direction (Switch : Input_Switch_Name) return Endstop_Direction_Kind;

   function Motor_Label (Motor : Motor_Name) return String is
     (case Motor is
         when X_Motor => "X",
         when Y_Motor => "Y",
         when Z_Motor => "Z",
         when E_Motor => "E");

   function Endstop_Label (Switch : Input_Switch_Name) return String is
     (case Switch is
         when X_Endstop => "X",
         when Y_Endstop => "Y",
         when Z_Endstop => "Z");

   function Environment_String (Name : String; Default : String) return String is
   begin
      if Ada.Environment_Variables.Exists (Name) then
         return Trim (Ada.Environment_Variables.Value (Name), Both);
      else
         return Default;
      end if;
   end Environment_String;

   function Environment_Float (Name : String; Default : Long_Float; Must_Be_Positive : Boolean) return Long_Float is
      Image : constant String := Environment_String (Name, Long_Float'Image (Default));
      Value : Long_Float;
   begin
      begin
         Value := Long_Float'Value (Image);
      exception
         when Constraint_Error =>
            raise Constraint_Error with "Invalid floating-point value for " & Name & ": " & Image;
      end;

      if not (Value >= Long_Float'First and then Value <= Long_Float'Last) then
         raise Constraint_Error with Name & " must be a finite floating-point value.";
      end if;

      if Must_Be_Positive and then not (Value > 0.0) then
         raise Constraint_Error with Name & " must be a positive floating-point value.";
      end if;

      return Value;
   end Environment_Float;

   function Environment_Boolean (Name : String; Default : Boolean) return Boolean is
      Image : constant String :=
        Ada.Characters.Handling.To_Upper (Environment_String (Name, (if Default then "TRUE" else "FALSE")));
   begin
      if Image = "TRUE" then
         return True;
      elsif Image = "FALSE" then
         return False;
      else
         raise Constraint_Error with Name & " must be either true or false.";
      end if;
   end Environment_Boolean;

   function Environment_Kinematics return Simulator_Kinematics_Kind is
      Name  : constant String := "PRUNT_SIM_KINEMATICS";
      Image : constant String := Ada.Characters.Handling.To_Upper (Environment_String (Name, "CARTESIAN"));
   begin
      if Image = "CARTESIAN" then
         return Cartesian;
      elsif Image = "CORE_XY" then
         return Core_XY;
      elsif Image = "LINEAR_DELTA" then
         return Linear_Delta;
      else
         raise Constraint_Error with Name & " must be CARTESIAN, CORE_XY, or LINEAR_DELTA.";
      end if;
   end Environment_Kinematics;

   function Environment_Endstop_Direction (Switch : Input_Switch_Name) return Endstop_Direction_Kind is
      Name  : constant String := "PRUNT_SIM_ENDSTOP_" & Endstop_Label (Switch) & "_DIRECTION";
      Image : constant String := Ada.Characters.Handling.To_Upper (Environment_String (Name, "MINIMUM"));
   begin
      if Image = "MINIMUM" then
         return Minimum;
      elsif Image = "MAXIMUM" then
         return Maximum;
      else
         raise Constraint_Error with Name & " must be MINIMUM or MAXIMUM.";
      end if;
   end Environment_Endstop_Direction;

   Kinematics : constant Simulator_Kinematics_Kind := Environment_Kinematics;

   Motor_Steps_Per_MM : constant Motor_Float_Array :=
     [for Motor in Motor_Name =>
        Environment_Float ("PRUNT_SIM_MOTOR_" & Motor_Label (Motor) & "_STEPS_PER_MM", 1.0, True)];

   Motor_Reversed : constant Motor_Boolean_Array :=
     [for Motor in Motor_Name =>
        Environment_Boolean ("PRUNT_SIM_MOTOR_" & Motor_Label (Motor) & "_REVERSED", False)];

   Initial_Motor_Position : constant Motor_Position :=
     [for Motor in Motor_Name =>
        Dimensionless
          (Environment_Float
             ("PRUNT_SIM_INITIAL_MOTOR_" & Motor_Label (Motor) & "_POSITION", 0.0, False))];

   Endstop_Trigger_MM : constant Endstop_Float_Array :=
     [for Switch in Input_Switch_Name =>
        Environment_Float ("PRUNT_SIM_ENDSTOP_" & Endstop_Label (Switch) & "_TRIGGER_MM", 0.0, False)];

   Endstop_Direction : constant Endstop_Direction_Array :=
     [for Switch in Input_Switch_Name => Environment_Endstop_Direction (Switch)];

   Endstop_Normally_Closed : constant Endstop_Boolean_Array :=
     [for Switch in Input_Switch_Name =>
        Environment_Boolean ("PRUNT_SIM_ENDSTOP_" & Endstop_Label (Switch) & "_NORMALLY_CLOSED", False)];

   protected Motor_Position_State is
      procedure Set (Position : Motor_Position);
      function Get return Motor_Position;
   private
      Position : Motor_Position := Initial_Motor_Position;
   end Motor_Position_State;

   protected body Motor_Position_State is
      procedure Set (Position : Motor_Position) is
      begin
         Motor_Position_State.Position := Position;
      end Set;

      function Get return Motor_Position is
      begin
         return Position;
      end Get;
   end Motor_Position_State;

   function Motor_Distance_MM (Position : Motor_Position; Motor : Motor_Name) return Long_Float;
   function Endstop_Coordinate_MM (Position : Motor_Position; Switch : Input_Switch_Name) return Long_Float;

   function Motor_Distance_MM (Position : Motor_Position; Motor : Motor_Name) return Long_Float is
      Direction : constant Long_Float := (if Motor_Reversed (Motor) then -1.0 else 1.0);
   begin
      return Direction * Long_Float (Position (Motor)) / Motor_Steps_Per_MM (Motor);
   end Motor_Distance_MM;

   function Endstop_Coordinate_MM (Position : Motor_Position; Switch : Input_Switch_Name) return Long_Float is
      Motor_X : constant Long_Float := Motor_Distance_MM (Position, X_Motor);
      Motor_Y : constant Long_Float := Motor_Distance_MM (Position, Y_Motor);
   begin
      case Kinematics is
         when Cartesian    =>
            return
              (case Switch is
                  when X_Endstop => Motor_X,
                  when Y_Endstop => Motor_Y,
                  when Z_Endstop => Motor_Distance_MM (Position, Z_Motor));

         when Core_XY      =>
            return
              (case Switch is
                  when X_Endstop => (Motor_X + Motor_Y) / 2.0,
                  when Y_Endstop => (Motor_X - Motor_Y) / 2.0,
                  when Z_Endstop => Motor_Distance_MM (Position, Z_Motor));

         when Linear_Delta =>
            --  Delta endstops are attached to the three independently moving tower carriages. X/Y/Z name tower A/B/C.
            return
              (case Switch is
                  when X_Endstop => Motor_X,
                  when Y_Endstop => Motor_Y,
                  when Z_Endstop => Motor_Distance_MM (Position, Z_Motor));
      end case;
   end Endstop_Coordinate_MM;

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
      return Get_Input_Switch_State_At_Position (Switch, Motor_Position_State.Get);
   end Get_Input_Switch_State;

   function Get_Initial_Motor_Position return Motor_Position is
     (Initial_Motor_Position);

   function Get_Input_Switch_State_At_Position
     (Switch : Input_Switch_Name; Position : Motor_Position) return Boolean
   is
      Coordinate : constant Long_Float := Endstop_Coordinate_MM (Position, Switch);
      Triggered  : constant Boolean :=
        (case Endstop_Direction (Switch) is
            when Minimum => Coordinate <= Endstop_Trigger_MM (Switch),
            when Maximum => Coordinate >= Endstop_Trigger_MM (Switch));
   begin
      --  A normally-open switch drives the raw input high when triggered; a normally-closed switch drives it low.
      return Triggered /= Endstop_Normally_Closed (Switch);
   end Get_Input_Switch_State_At_Position;

   procedure Set_Current_Motor_Position (Position : Motor_Position) is
   begin
      Motor_Position_State.Set (Position);
   end Set_Current_Motor_Position;

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
