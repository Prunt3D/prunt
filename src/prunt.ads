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

pragma Extensions_Allowed (On);

with Ada.Numerics;
with VSS.Strings; use VSS.Strings;
with VSS.Strings.Conversions;

package Prunt is

   pragma Preelaborate (Prunt);

   type Motor_Hardware_Kind is (Basic_Motor_Kind, TMC2240_UART_Kind);

   type Command_Index is range 0 .. 2 ** 63 - 2;

   type Loop_Move_Count is range 0 .. 2 ** 31 - 1;

   type Planner_Corner_ID is range 0 .. 2 ** 63 - 2;

   type Gcode_Command_ID is range 0 .. 2 ** 63 - 2;
   --  Identifies an interactively submitted G-code command. Zero is reserved for the absence of an interactive
   --  command.

   type Gcode_Command_Update_Kind is (Running, Output, Completed, Cancelled, Failed);
   --  Updates published to web clients for interactively submitted G-code commands.

   type Pin_State is (High_State, Low_State);

   --  You may notice a lot of math similar to 5.0**(1/2) here when using the below types. This may seem like it should
   --  be evaluated as 5.0**(1/2) = 5.0**0 = 1.0, which it would be under normal circumstances, but GNAT does some
   --  magic to evaluate it as 5.0**0.5 and keeps the dimensions intact. If porting this to a different Ada compiler
   --  then you will have to use ** from Ada.Numerics.Generic_Elementary_Functions and replace all the rationals with
   --  floating point literals.
   type Dimensioned_Float is new Long_Float
   with
     Dimension_System =>
       ((Unit_Name => Millimeter, Unit_Symbol => "mm", Dim_Symbol => "Length"),
        (Unit_Name => Second, Unit_Symbol => "s", Dim_Symbol => "Time"),
        (Unit_Name => Celsius, Unit_Symbol => "°C", Dim_Symbol => "Temperature"),
        (Unit_Name => Amp, Unit_Symbol => "A", Dim_Symbol => "Current"),
        (Unit_Name => Gram, Unit_Symbol => "g", Dim_Symbol => "Mass")),
     Annotate         => (Prunt_Config, User_Config);

   subtype Length is Dimensioned_Float
   with Dimension => (Symbol => "mm", Millimeter => 1, others => 0), Annotate => (Prunt_Config, Unit, "mm");

   subtype Time is Dimensioned_Float
   with Dimension => (Symbol => "s", Second => 1, others => 0), Annotate => (Prunt_Config, Unit, "s");

   subtype Temperature is Dimensioned_Float
   with Dimension => (Symbol => "°C", Celsius => 1, others => 0), Annotate => (Prunt_Config, Unit, "celsius", "°C");

   subtype Angle is Dimensioned_Float
   with Dimension => (Symbol => "rad", others => 0), Annotate => (Prunt_Config, Unit, "deg", "°");

   subtype Dimensionless is Dimensioned_Float
   with Dimension => (Symbol => "×", others => 0), Annotate => (Prunt_Config, Unit, "", "×");

   subtype Voltage is Dimensioned_Float
   with
     Dimension => (Symbol => "nV", Gram => 1, Millimeter => 2, Second => -3, Amp => -1, others => 0),
     Annotate  => (Prunt_Config, Unit, "volt", "V");

   subtype Current is Dimensioned_Float
   with Dimension => (Symbol => "A", Amp => 1, others => 0), Annotate => (Prunt_Config, Unit, "amp", "A");

   subtype Mass is Dimensioned_Float
   with Dimension => (Symbol => "g", Gram => 1, others => 0), Annotate => (Prunt_Config, Unit, "gram", "g");

   subtype Resistance is Dimensioned_Float
   with
     Dimension => (Symbol => "nohm", Gram => 1, Millimeter => 2, Second => -3, Amp => -2, others => 0),
     Annotate  => (Prunt_Config, Unit, "ohm", "Ω");

   subtype Power is Dimensioned_Float
   with
     Dimension => (Symbol => "nW", Gram => 1, Millimeter => 2, Second => -3, others => 0),
     Annotate  => (Prunt_Config, Unit, "watt", "W");

   subtype Frequency is Dimensioned_Float
   with Dimension => (Symbol => "Hz", Second => -1, others => 0), Annotate => (Prunt_Config, Unit, "hertz", "Hz");

   subtype Energy is Dimensioned_Float
   with
     Dimension => (Symbol => "nJ", Gram => 1, Millimeter => 2, Second => -2, others => 0),
     Annotate  => (Prunt_Config, Unit, "joule", "J");

   subtype Inductance is Dimensioned_Float
   with
     Dimension => (Symbol => "nH", Gram => 1, Millimeter => 2, Second => -2, Amp => -2, others => 0),
     Annotate  => (Prunt_Config, Unit, "millihenry", "mH");

   subtype PWM_Scale is Dimensionless range 0.0 .. 1.0 with Annotate => (Prunt_Config, Unit, "", "×");

   subtype Cruise_Ratio is Dimensionless range 0.03 .. 0.97;

   type Dimensionless_Ratio is record
      Numerator, Denominator : Dimensionless;
   end record;

   pragma Warnings (Off, "assumed to be");
   mm        : constant Length := 1.0;
   s         : constant Time := 1.0;
   celsius   : constant Temperature := 1.0;
   radian    : constant Angle := 1.0;
   nanovolt  : constant Voltage := 1.0;
   amp       : constant Current := 1.0;
   nanoohm   : constant Resistance := 1.0;
   hertz     : constant Frequency := 1.0;
   nanowatt  : constant Power := 1.0;
   nanojoule : constant Energy := 1.0;
   gram      : constant Mass := 1.0;
   nanohenry : constant Inductance := 1.0;
   pragma Warnings (On, "assumed to be");

   volt       : constant Voltage := 1_000_000_000.0 * nanovolt;
   ohm        : constant Resistance := 1_000_000_000.0 * nanoohm;
   watt       : constant Power := 1_000_000_000.0 * nanowatt;
   joule      : constant Energy := 1_000_000_000.0 * nanojoule;
   henry      : constant Inductance := 1_000_000_000.0 * nanohenry;
   millihenry : constant Inductance := 1_000_000.0 * nanohenry;
   ms         : constant Time := s / 1_000.0;
   min        : constant Time := s * 60.0;
   deg        : constant Angle := (Ada.Numerics.Pi / 180.0) * radian;

   subtype Velocity is Dimensioned_Float
   with
     Dimension => (Symbol => "mm/s", Millimeter => 1, Second => -1, others => 0),
     Annotate  => (Prunt_Config, Unit, "mm/s");
   subtype Acceleration is Dimensioned_Float
   with
     Dimension => (Symbol => "mm/s²", Millimeter => 1, Second => -2, others => 0),
     Annotate  => (Prunt_Config, Unit, "mm/s**2", "mm/s²");
   subtype Jerk is Dimensioned_Float
   with
     Dimension => (Symbol => "mm/s³", Millimeter => 1, Second => -3, others => 0),
     Annotate  => (Prunt_Config, Unit, "mm/s**3", "mm/s³");
   subtype Snap is Dimensioned_Float
   with
     Dimension => (Symbol => "mm/s⁴", Millimeter => 1, Second => -4, others => 0),
     Annotate  => (Prunt_Config, Unit, "mm/s**4", "mm/s⁴");
   subtype Crackle is Dimensioned_Float
   with
     Dimension => (Symbol => "mm/s⁵", Millimeter => 1, Second => -5, others => 0),
     Annotate  => (Prunt_Config, Unit, "mm/s**5", "mm/s⁵");

   subtype Area is Dimensioned_Float with Dimension => (Symbol => "mm²", Millimeter => 2, others => 0);
   subtype Volume is Dimensioned_Float with Dimension => (Symbol => "mm³", Millimeter => 3, others => 0);
   subtype Hypervolume is Dimensioned_Float with Dimension => (Symbol => "mm⁴", Millimeter => 4, others => 0);

   subtype Curvature is Dimensioned_Float with Dimension => (Symbol => "mm**(-1)", Millimeter => -1, others => 0);
   subtype Curvature_To_2 is Dimensioned_Float with Dimension => (Symbol => "mm**(-2)", Millimeter => -2, others => 0);
   subtype Curvature_To_3 is Dimensioned_Float with Dimension => (Symbol => "mm**(-3)", Millimeter => -3, others => 0);
   subtype Curvature_To_4 is Dimensioned_Float with Dimension => (Symbol => "mm**(-4)", Millimeter => -4, others => 0);
   subtype Curvature_To_5 is Dimensioned_Float with Dimension => (Symbol => "mm**(-5)", Millimeter => -5, others => 0);

   subtype Heat_Flux is Dimensioned_Float
   with Dimension => (Symbol => "mW/mm**2", Gram => 1, Second => -3, others => 0);

   subtype Heat_Transfer_Coefficient is Dimensioned_Float
   with Dimension => (Symbol => "mW/(m**2 degC)", Gram => 1, Second => -3, Celsius => -1, others => 0);

   subtype Thermal_Conductance is Dimensioned_Float
   with Dimension => (Symbol => "nW/degC", Gram => 1, Millimeter => 2, Second => -3, Celsius => -1, others => 0);

   subtype Heat_Capacity is Dimensioned_Float
   with Dimension => (Symbol => "nJ/degC", Gram => 1, Millimeter => 2, Second => -2, Celsius => -1, others => 0);

   subtype Specific_Heat_Capacity is Dimensioned_Float
   with Dimension => (Symbol => "nJ/(g*degC)", Millimeter => 2, Second => -2, Celsius => -1, others => 0);

   subtype Inverse_Temperature is Dimensioned_Float
   with Dimension => (Symbol => "degC**(-1)", Celsius => -1, others => 0);
   subtype Time_Over_Temperature is Dimensioned_Float
   with Dimension => (Symbol => "s/degC", Second => 1, Celsius => -1, others => 0);
   subtype Frequency_Over_Temperature is Dimensioned_Float
   with Dimension => (Symbol => "Hz/degC", Second => -1, Celsius => -1, others => 0);

   type Axis_Name is (X_Axis, Y_Axis, Z_Axis, E_Axis) with Annotate => (Prunt_Config, User_Config);

   type Position is array (Axis_Name) of Length;
   type Position_Offset is new Position;
   type Position_Scale is array (Axis_Name) of Dimensionless;
   type Axial_Velocities is array (Axis_Name) of Velocity;
   type Axial_Accelerations is array (Axis_Name) of Acceleration;
   type Axial_Jerks is array (Axis_Name) of Jerk;
   type Axial_Snaps is array (Axis_Name) of Snap;
   type Axial_Crackles is array (Axis_Name) of Crackle;

   function "*" (Left : Position; Right : Position_Scale) return Position;
   --  Multiply each axis of Left by the corresponding scale in Right.

   function "*" (Left : Position_Offset; Right : Position_Scale) return Position_Offset;
   --  Multiply each axis offset in Left by the corresponding scale in Right.

   function "*" (Left : Position_Scale; Right : Dimensionless) return Position_Scale;
   --  Multiply every axis scale in Left by the scalar Right.

   function "*" (Left : Position_Scale; Right : Length) return Position_Offset;
   --  Scale each axis proportion in Left by the physical length Right.

   function "*" (Left : Position_Scale; Right : Velocity) return Axial_Velocities;
   --  Resolve scalar velocity Right into per-axis velocities using Left.

   function "*" (Left : Position; Right : Dimensionless) return Position;
   --  Multiply every coordinate in Left by the scalar Right.

   function "*" (Left : Position_Offset; Right : Dimensionless) return Position_Offset;
   --  Multiply every axis offset in Left by the scalar Right.

   function "+" (Left, Right : Position_Scale) return Position_Scale;
   --  Add corresponding axis scales from Left and Right.

   function "+" (Left : Position; Right : Position_Offset) return Position;
   --  Translate Left by the per-axis offset Right.

   function "-" (Left, Right : Position) return Position_Offset;
   --  Return the per-axis offset from Right to Left.

   function "-" (Left, Right : Position_Scale) return Position_Scale;
   --  Subtract corresponding axis scales in Right from Left.

   function "-" (Left, Right : Position_Offset) return Position_Offset;
   --  Subtract corresponding axis offsets in Right from Left.

   function "-" (Left : Position; Right : Position_Offset) return Position;
   --  Translate Left by the negative of the per-axis offset Right.

   function "/" (Left : Axial_Velocities; Right : Position_Scale) return Axial_Velocities;
   --  Divide each axial velocity in Left by the corresponding scale in Right.

   function "/" (Left : Position_Offset; Right : Length) return Position_Scale;
   --  Normalize each axis offset in Left by the physical length Right.

   function "/" (Left : Position_Scale; Right : Dimensionless) return Position_Scale;
   --  Divide every axis scale in Left by the scalar Right.

   function "/" (Left : Position; Right : Dimensionless) return Position;
   --  Divide every coordinate in Left by the scalar Right.

   function "/" (Left : Position; Right : Position_Scale) return Position;
   --  Divide each coordinate in Left by the corresponding scale in Right.

   function "/" (Left : Position_Offset; Right : Position_Scale) return Position_Offset;
   --  Divide each axis offset in Left by the corresponding scale in Right.

   function "abs" (Left : Position_Offset) return Length;
   --  Return the Euclidean magnitude of the four-axis physical offset Left.

   function "abs" (Left : Position_Scale) return Dimensionless;
   --  Return the Euclidean magnitude of the four-axis dimensionless vector Left.

   function Dot (Left, Right : Position_Scale) return Dimensionless;
   --  Return the Euclidean dot product of two dimensionless axis vectors.

   function Dot (Left : Position_Offset; Right : Position_Scale) return Length;
   --  Return the Euclidean dot product of a physical offset and a dimensionless axis vector.

   function Dot (Left, Right : Position_Offset) return Area;
   --  Return the Euclidean dot product of two physical axis offsets.

   TMC_UART_Error : exception;

   type Update_Check_Method is (None, Github);

   type Update_Check_Details (Method : Update_Check_Method := None) is record
      case Method is
         when None =>
            null;

         when Github =>
            Repository   : Virtual_String;
            Expected_Tag : Virtual_String;
      end case;
   end record;

   type File_Line_Count is range 0 .. 2 ** 63 - 1;

   type PID_Autotune_Cycle_Count is range 2 .. 1_000;

   type Heater_Kind is (Disabled_Kind, PID_Kind, Bang_Bang_Kind, PID_Autotune_Kind);

   type Heater_Parameters (Kind : Heater_Kind := Disabled_Kind) is record
      case Kind is
         when Disabled_Kind =>
            null;

         when others =>
            Safe_Below                 : Temperature := 70.0 * celsius;
            Check_Max_Cumulative_Error : Temperature := 120.0 * celsius;
            Check_Gain_Time            : Time := 20.0 * s;
            Check_Minimum_Gain         : Temperature := 2.0 * celsius;
            Check_Hysteresis           : Temperature := 3.0 * celsius;

            case Kind is
               when Disabled_Kind =>
                  null;

               when PID_Kind =>
                  Proportional_Scale : Dimensionless := 0.0;
                  Integral_Scale     : Dimensionless := 0.0;
                  Derivative_Scale   : Dimensionless := 0.0;

               when Bang_Bang_Kind =>
                  Bang_Bang_Hysteresis : Temperature := 0.0 * celsius;

               when PID_Autotune_Kind =>
                  Max_Cycles                 : PID_Autotune_Cycle_Count := 5;
                  Proportional_Tuning_Factor : Dimensionless := 0.6;
                  Derivative_Tuning_Factor   : Frequency := 0.125 * hertz;
                  PID_Tuning_Temperature     : Temperature := 0.0 * celsius;
            end case;
      end case;
   end record;

   type Fan_Hardware_Kind is (Fixed_Switching_Kind, Low_Or_High_Side_Switching_Kind);

   function "+" (Left : String) return Virtual_String renames Conversions.To_Virtual_String;
   --  Convert a UTF-8 String to Virtual_String.

   function Next_Test_Filename return String;
   --  Return a process-unique temporary test-file path encoded as a UTF-8 String.

   function Next_Test_Filename return Virtual_String;
   --  Return a process-unique temporary test-file path as a Virtual_String.

   protected Test_File_Name_Generator is
      procedure Get_Next (Name : out Virtual_String);
   private
      Counter : Natural := 0;
   end Test_File_Name_Generator;

end Prunt;
