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

with Ada.Numerics;
with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

package Prunt is

   type Stepper_Kind is (Basic_Kind, TMC2240_UART_Kind);

   type Command_Index is range 0 .. 2**63 - 2;

   type Pin_State is (High_State, Low_State);

   protected type Fatal_Exception_Occurrence_Holder_Type is
      function Is_Set return Boolean;
      --  Check if any exceptions have been stored.

      procedure Set
        (Cause      : Ada.Task_Termination.Cause_Of_Termination;
         ID         : Ada.Task_Identification.Task_Id;
         Occurrence : Ada.Exceptions.Exception_Occurrence) with
        Post => Is_Set;
        --  Store an exception if no exception has been stored previously. Also prints all exceptions.

      entry Get (Occurrence : out Ada.Exceptions.Exception_Occurrence);
      --  Get the stored exception. Blocks until an exception is available.
   private
      function Null_Occurrence return Ada.Exceptions.Exception_Occurrence;
      Data : aliased Ada.Exceptions.Exception_Occurrence := Null_Occurrence;
   end Fatal_Exception_Occurrence_Holder_Type;

   --  You may notice a lot of math similar to 5.0**(1/2) here when using the below types. This may seem like it should
   --  be evaluated as 5.0**(1/2) = 5.0**0 = 1.0, which it would be under normal circumstances, but GNAT does some
   --  magic to evaluate it as 5.0**0.5 and keeps the dimensions intact. If porting this to a different Ada compiler
   --  then you will have to use ** from Ada.Numerics.Generic_Elementary_Functions and replace all the rationals with
   --  floating point literals.
   type Dimensioned_Float is new Long_Float with
     Dimension_System =>
      ((Unit_Name => Millimeter, Unit_Symbol => "mm", Dim_Symbol => "Length"),
       (Unit_Name => Second, Unit_Symbol => "s", Dim_Symbol => "Time"),
       (Unit_Name => Celsius, Unit_Symbol => "°C", Dim_Symbol => "Temperature"),
       (Unit_Name => Amp, Unit_Symbol => "A", Dim_Symbol => "Current"),
       (Unit_Name => Gram, Unit_Symbol => "g", Dim_Symbol => "Mass"));

   subtype Length is Dimensioned_Float with
       Dimension => (Symbol => "mm", Millimeter => 1, others => 0);

   subtype Time is Dimensioned_Float with
       Dimension => (Symbol => "s", Second => 1, others => 0);

   subtype Temperature is Dimensioned_Float with
       Dimension => (Symbol => "°C", Celsius => 1, others => 0);

   subtype Angle is Dimensioned_Float with
       Dimension => (Symbol => "rad", others => 0);

   subtype Dimensionless is Dimensioned_Float with
       Dimension => (Symbol => "×", others => 0);

   subtype Voltage is Dimensioned_Float with
       Dimension => (Symbol => "nV", Gram => 1, Millimeter => 2, Second => -3, Amp => -1, others => 0);

   subtype Current is Dimensioned_Float with
       Dimension => (Symbol => "A", Amp => 1, others => 0);

   subtype Mass is Dimensioned_Float with
       Dimension => (Symbol => "g", Gram => 1, others => 0);

   subtype Resistance is Dimensioned_Float with
       Dimension => (Symbol => "nΩ", Gram => 1, Millimeter => 2, Second => -3, Amp => -2, others => 0);

   subtype Power is Dimensioned_Float with
       Dimension => (Symbol => "nW", Gram => 1, Millimeter => 2, Second => -3, others => 0);

   subtype Frequency is Dimensioned_Float with
       Dimension => (Symbol => "Hz", Second => -1, others => 0);

   subtype Energy is Dimensioned_Float with
       Dimension => (Symbol => "nJ", Gram => 1, Millimeter => 2, Second => -2, others => 0);

   subtype Inductance is Dimensioned_Float with
       Dimension => (Symbol => "nH", Gram => 1, Millimeter => 2, Second => -2, Amp => -2, others => 0);

   subtype PWM_Scale is Dimensionless range 0.0 .. 1.0;

   subtype Cruise_Ratio is Dimensionless range 0.03 .. 0.97;

   pragma Warnings (Off, "assumed to be");
   mm        : constant Length      := 1.0;
   s         : constant Time        := 1.0;
   celsius   : constant Temperature := 1.0;
   radian    : constant Angle       := 1.0;
   nanovolt  : constant Voltage     := 1.0;
   amp       : constant Current     := 1.0;
   nanoohm   : constant Resistance  := 1.0;
   hertz     : constant Frequency   := 1.0;
   nanowatt  : constant Power       := 1.0;
   nanojoule : constant Energy      := 1.0;
   gram      : constant Mass        := 1.0;
   nanohenry : constant Inductance  := 1.0;
   pragma Warnings (On, "assumed to be");

   volt  : constant Voltage    := 1_000_000_000.0 * nanovolt;
   ohm   : constant Resistance := 1_000_000_000.0 * nanoohm;
   watt  : constant Power      := 1_000_000_000.0 * nanowatt;
   joule : constant Energy     := 1_000_000_000.0 * nanojoule;
   henry : constant Inductance := 1_000_000_000.0 * nanohenry;
   ms    : constant Time       := s / 1_000.0;
   min   : constant Time       := s * 60.0;
   deg   : constant Angle      := (Ada.Numerics.Pi / 180.0) * radian;

   subtype Fan_PWM_Frequency is Frequency range 1.0 * hertz .. 50_000.0 * hertz;

   subtype Velocity is Dimensioned_Float with
       Dimension => (Symbol => "mm/s", Millimeter => 1, Second => -1, others => 0);
   subtype Acceleration is Dimensioned_Float with
       Dimension => (Symbol => "mm/s²", Millimeter => 1, Second => -2, others => 0);
   subtype Jerk is Dimensioned_Float with
       Dimension => (Symbol => "mm/s³", Millimeter => 1, Second => -3, others => 0);
   subtype Snap is Dimensioned_Float with
       Dimension => (Symbol => "mm/s⁴", Millimeter => 1, Second => -4, others => 0);
   subtype Crackle is Dimensioned_Float with
       Dimension => (Symbol => "mm/s⁵", Millimeter => 1, Second => -5, others => 0);

   subtype Area is Dimensioned_Float with
       Dimension => (Symbol => "mm²", Millimeter => 2, others => 0);
   subtype Volume is Dimensioned_Float with
       Dimension => (Symbol => "mm³", Millimeter => 3, others => 0);
   subtype Hypervolume is Dimensioned_Float with
       Dimension => (Symbol => "mm⁴", Millimeter => 4, others => 0);

   subtype Curvature is Dimensioned_Float with
       Dimension => (Symbol => "mm⁻¹", Millimeter => -1, others => 0);
   subtype Curvature_To_2 is Dimensioned_Float with
       Dimension => (Symbol => "mm⁻²", Millimeter => -2, others => 0);
   subtype Curvature_To_3 is Dimensioned_Float with
       Dimension => (Symbol => "mm⁻³", Millimeter => -3, others => 0);
   subtype Curvature_To_4 is Dimensioned_Float with
       Dimension => (Symbol => "mm⁻⁴", Millimeter => -4, others => 0);

   subtype Heat_Flux is Dimensioned_Float with
       Dimension => (Symbol => "mW/mm²", Gram => 1, Second => -3, others => 0);

   subtype Heat_Transfer_Coefficient is Dimensioned_Float with
       Dimension => (Symbol => "mW/(m²°C)", Gram => 1, Second => -3, Celsius => -1, others => 0);

   subtype Thermal_Conductance is Dimensioned_Float with
       Dimension => (Symbol => "nW/°C", Gram => 1, Millimeter => 2, Second => -3, Celsius => -1, others => 0);

   subtype Heat_Capacity is Dimensioned_Float with
       Dimension => (Symbol => "nJ/°C", Gram => 1, Millimeter => 2, Second => -2, Celsius => -1, others => 0);

   subtype Specific_Heat_Capacity is Dimensioned_Float with
       Dimension =>
        (Symbol => "nJ/(g°C)",  Millimeter => 2, Second => -2, Celsius => -1, others => 0);

   subtype Inverse_Temperature is Dimensioned_Float with
       Dimension => (Symbol => "°C⁻¹", Celsius => -1, others => 0);
   subtype Time_Over_Temperature is Dimensioned_Float with
       Dimension => (Symbol => "s/°C", Second => 1, Celsius => -1, others => 0);
   subtype Frequency_Over_Temperature is Dimensioned_Float with
       Dimension => (Symbol => "Hz/°C", Second => -1, Celsius => -1, others => 0);

   type Axis_Name is (X_Axis, Y_Axis, Z_Axis, E_Axis);

   type Position is array (Axis_Name) of Length;
   type Scaled_Position is array (Axis_Name) of Length;
   --  A Scaled_Position is any absolute position that does not represent real machine coordinates.
   type Position_Offset is array (Axis_Name) of Length;
   type Scaled_Position_Offset is array (Axis_Name) of Length;
   type Position_Scale is array (Axis_Name) of Dimensionless;
   type Axial_Velocities is array (Axis_Name) of Velocity;

   function "*" (Left : Position; Right : Position_Scale) return Scaled_Position;
   function "*" (Left : Position_Offset; Right : Position_Scale) return Position_Offset;
   function "*" (Left : Position_Scale; Right : Dimensionless) return Position_Scale;
   function "*" (Left : Position_Scale; Right : Length) return Scaled_Position_Offset;
   function "*" (Left : Position_Scale; Right : Velocity) return Axial_Velocities;
   function "*" (Left : Scaled_Position; Right : Position_Scale) return Scaled_Position;
   function "*" (Left : Scaled_Position; Right : Dimensionless) return Scaled_Position;
   function "*" (Left : Scaled_Position_Offset; Right : Position_Scale) return Scaled_Position_Offset;
   function "*" (Left : Scaled_Position_Offset; Right : Dimensionless) return Scaled_Position_Offset;
   function "+" (Left : Scaled_Position; Right : Scaled_Position_Offset) return Scaled_Position;
   function "+" (Left, Right : Position_Scale) return Position_Scale;
   function "+" (Left : Position; Right : Position_Offset) return Position;
   function "-" (Left, Right : Position) return Position_Offset;
   function "-" (Left, Right : Position_Scale) return Position_Scale;
   function "-" (Left, Right : Scaled_Position) return Scaled_Position_Offset;
   function "-" (Left, Right : Scaled_Position_Offset) return Scaled_Position_Offset;
   function "-" (Left : Scaled_Position; Right : Scaled_Position_Offset) return Scaled_Position;
   function "-" (Left : Position; Right : Position_Offset) return Position;
   function "/" (Left : Axial_Velocities; Right : Position_Scale) return Axial_Velocities;
   function "/" (Left : Position_Offset; Right : Length) return Position_Scale;
   function "/" (Left : Position_Scale; Right : Dimensionless) return Position_Scale;
   function "/" (Left : Scaled_Position_Offset; Right : Length) return Position_Scale;
   function "/" (Left : Scaled_Position; Right : Dimensionless) return Scaled_Position;
   function "/" (Left : Scaled_Position; Right : Position_Scale) return Scaled_Position;
   function "/" (Left : Position; Right : Position_Scale) return Scaled_Position;
   function "/" (Left : Scaled_Position_Offset; Right : Position_Scale) return Scaled_Position_Offset;
   function "abs" (Left : Position_Offset) return Length;
   function "abs" (Left : Position_Scale) return Dimensionless;
   function "abs" (Left : Scaled_Position_Offset) return Length;
   function Dot (Left, Right : Position_Scale) return Dimensionless;
   function Dot (Left : Scaled_Position_Offset; Right : Position_Scale) return Length;
   function Dot (Left, Right : Scaled_Position_Offset) return Area;

   TMC_UART_Error : exception;

   function JSON_Escape (S : String) return String;
   function JSON_Escape (S : Ada.Strings.Unbounded.Unbounded_String) return Ada.Strings.Unbounded.Unbounded_String;

end Prunt;
