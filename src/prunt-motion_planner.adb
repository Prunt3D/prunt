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

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Conversion;
with Prunt.Motion_Planner.Trajectory.Searches;

package body Prunt.Motion_Planner is

   pragma Extensions_Allowed (On);

   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   function Motion_Limits_Are_Valid (Limits : Motion_Limits) return Boolean is
   begin
      --  Written as negated comparisons so NaN fails as well.
      if not (Limits.Tangential_Velocity_Max > 0.0 * mm / s
              and then Limits.Tangential_Acceleration_Max > 0.0 * mm / s**2
              and then Limits.Tangential_Jerk_Max > 0.0 * mm / s**3
              and then Limits.Tangential_Snap_Max > 0.0 * mm / s**4
              and then Limits.Tangential_Crackle_Max > 0.0 * mm / s**5)
      then
         return False;
      end if;
      for Axis in Axis_Name loop
         if not (Limits.Axial_Velocity_Maxes (Axis) > 0.0 * mm / s
                 and then Limits.Axial_Acceleration_Maxes (Axis) > 0.0 * mm / s**2
                 and then Limits.Axial_Jerk_Maxes (Axis) > 0.0 * mm / s**3
                 and then Limits.Axial_Snap_Maxes (Axis) > 0.0 * mm / s**4
                 and then Limits.Axial_Crackle_Maxes (Axis) > 0.0 * mm / s**5)
         then
            return False;
         end if;
      end loop;
      return True;
   end Motion_Limits_Are_Valid;

   function Capped_Parameters (Params : Kinematic_Parameters; Limits : Motion_Limits) return Kinematic_Parameters is
   begin
      --  The planner calls this for every profile window it evaluates, so cap the copy in place rather than building
      --  intermediate limit records.
      return Result : Kinematic_Parameters := Params do
         Result.Tangential_Velocity_Max :=
           Velocity'Min (Params.Tangential_Velocity_Max, Limits.Tangential_Velocity_Max);
         Result.Tangential_Acceleration_Max :=
           Acceleration'Min (Params.Tangential_Acceleration_Max, Limits.Tangential_Acceleration_Max);
         Result.Tangential_Jerk_Max := Jerk'Min (Params.Tangential_Jerk_Max, Limits.Tangential_Jerk_Max);
         Result.Tangential_Snap_Max := Snap'Min (Params.Tangential_Snap_Max, Limits.Tangential_Snap_Max);
         Result.Tangential_Crackle_Max := Crackle'Min (Params.Tangential_Crackle_Max, Limits.Tangential_Crackle_Max);
         for Axis in Axis_Name loop
            Result.Axial_Velocity_Maxes (Axis) :=
              Velocity'Min (Params.Axial_Velocity_Maxes (Axis), Limits.Axial_Velocity_Maxes (Axis));
            Result.Axial_Acceleration_Maxes (Axis) :=
              Acceleration'Min (Params.Axial_Acceleration_Maxes (Axis), Limits.Axial_Acceleration_Maxes (Axis));
            Result.Axial_Jerk_Maxes (Axis) :=
              Jerk'Min (Params.Axial_Jerk_Maxes (Axis), Limits.Axial_Jerk_Maxes (Axis));
            Result.Axial_Snap_Maxes (Axis) :=
              Snap'Min (Params.Axial_Snap_Maxes (Axis), Limits.Axial_Snap_Maxes (Axis));
            Result.Axial_Crackle_Maxes (Axis) :=
              Crackle'Min (Params.Axial_Crackle_Maxes (Axis), Limits.Axial_Crackle_Maxes (Axis));
         end loop;
      end return;
   end Capped_Parameters;

   function Parameter_Limits (Params : Kinematic_Parameters) return Motion_Limits
   is (Tangential_Velocity_Max     => Params.Tangential_Velocity_Max,
       Tangential_Acceleration_Max => Params.Tangential_Acceleration_Max,
       Tangential_Jerk_Max         => Params.Tangential_Jerk_Max,
       Tangential_Snap_Max         => Params.Tangential_Snap_Max,
       Tangential_Crackle_Max      => Params.Tangential_Crackle_Max,
       Axial_Velocity_Maxes        => Params.Axial_Velocity_Maxes,
       Axial_Acceleration_Maxes    => Params.Axial_Acceleration_Maxes,
       Axial_Jerk_Maxes            => Params.Axial_Jerk_Maxes,
       Axial_Snap_Maxes            => Params.Axial_Snap_Maxes,
       Axial_Crackle_Maxes         => Params.Axial_Crackle_Maxes);

   function Lower_Limits (Left, Right : Motion_Limits) return Motion_Limits is
   begin
      return Result : Motion_Limits := Left do
         Result.Tangential_Velocity_Max := Velocity'Min (Left.Tangential_Velocity_Max, Right.Tangential_Velocity_Max);
         Result.Tangential_Acceleration_Max :=
           Acceleration'Min (Left.Tangential_Acceleration_Max, Right.Tangential_Acceleration_Max);
         Result.Tangential_Jerk_Max := Jerk'Min (Left.Tangential_Jerk_Max, Right.Tangential_Jerk_Max);
         Result.Tangential_Snap_Max := Snap'Min (Left.Tangential_Snap_Max, Right.Tangential_Snap_Max);
         Result.Tangential_Crackle_Max := Crackle'Min (Left.Tangential_Crackle_Max, Right.Tangential_Crackle_Max);
         for Axis in Axis_Name loop
            Result.Axial_Velocity_Maxes (Axis) :=
              Velocity'Min (Left.Axial_Velocity_Maxes (Axis), Right.Axial_Velocity_Maxes (Axis));
            Result.Axial_Acceleration_Maxes (Axis) :=
              Acceleration'Min (Left.Axial_Acceleration_Maxes (Axis), Right.Axial_Acceleration_Maxes (Axis));
            Result.Axial_Jerk_Maxes (Axis) := Jerk'Min (Left.Axial_Jerk_Maxes (Axis), Right.Axial_Jerk_Maxes (Axis));
            Result.Axial_Snap_Maxes (Axis) := Snap'Min (Left.Axial_Snap_Maxes (Axis), Right.Axial_Snap_Maxes (Axis));
            Result.Axial_Crackle_Maxes (Axis) :=
              Crackle'Min (Left.Axial_Crackle_Maxes (Axis), Right.Axial_Crackle_Maxes (Axis));
         end loop;
      end return;
   end Lower_Limits;

   function XY_Position_Is_In_Bounds (Pos : Position; Params : Kinematic_Parameters) return Boolean is
   begin
      case Params.Bounds.Kind is
         when Rectangular_Workspace =>
            return
              Pos (X_Axis) >= Params.Bounds.Lower_X
              and then Pos (X_Axis) <= Params.Bounds.Upper_X
              and then Pos (Y_Axis) >= Params.Bounds.Lower_Y
              and then Pos (Y_Axis) <= Params.Bounds.Upper_Y;

         when Circular_Workspace    =>
            if Params.Bounds.Radius < 0.0 * mm then
               return False;
            end if;

            return
              Dimensionless_Math.Sqrt ((Pos (X_Axis) / mm)**2 + (Pos (Y_Axis) / mm)**2) <= Params.Bounds.Radius / mm;
      end case;
   end XY_Position_Is_In_Bounds;

   function Position_Is_In_Bounds (Pos : Position; Params : Kinematic_Parameters) return Boolean is
   begin
      return
        Pos (Z_Axis) >= Params.Bounds.Lower_Z
        and then Pos (Z_Axis) <= Params.Bounds.Upper_Z
        and then Pos (E_Axis) >= Params.Bounds.Lower_E
        and then Pos (E_Axis) <= Params.Bounds.Upper_E
        and then XY_Position_Is_In_Bounds (Pos, Params);
   end Position_Is_In_Bounds;

   function Helix_Is_In_Bounds
     (Start_Pos, Finish_Pos, Center : Position; Clockwise : Boolean; Params : Kinematic_Parameters) return Boolean
   is
      Two_Pi           : constant Dimensionless := 2.0 * Ada.Numerics.Pi;
      Radius_Tolerance : constant Length := 1.0E-6 * mm;

      function Hypot (X, Y : Length) return Length;

      function Phase_Is_On_Arc (Phase, Theta_Start, Theta_Delta : Dimensionless) return Boolean;

      function Hypot (X, Y : Length) return Length is
         DX    : constant Dimensionless := X / mm;
         DY    : constant Dimensionless := Y / mm;
         Scale : constant Dimensionless := Dimensionless'Max (abs DX, abs DY);
      begin
         if Scale = 0.0 then
            return 0.0 * mm;
         end if;
         return Scale * Dimensionless_Math.Sqrt ((DX / Scale)**2 + (DY / Scale)**2) * mm;
      end Hypot;

      function Phase_Is_On_Arc (Phase, Theta_Start, Theta_Delta : Dimensionless) return Boolean is
         Progress  : Dimensionless := (if Theta_Delta > 0.0 then Phase - Theta_Start else Theta_Start - Phase);
         Magnitude : constant Dimensionless := abs Theta_Delta;
         Tolerance : constant Dimensionless :=
           64.0
           * Dimensionless'Model_Epsilon
           * (1.0 + Dimensionless'Max (abs Phase, Dimensionless'Max (abs Theta_Start, Magnitude)));
      begin
         if Progress < 0.0 then
            Progress := Progress + Two_Pi;
         end if;
         return Progress <= Magnitude + Tolerance;
      end Phase_Is_On_Arc;

      Start_DX      : constant Length := Start_Pos (X_Axis) - Center (X_Axis);
      Start_DY      : constant Length := Start_Pos (Y_Axis) - Center (Y_Axis);
      Finish_DX     : constant Length := Finish_Pos (X_Axis) - Center (X_Axis);
      Finish_DY     : constant Length := Finish_Pos (Y_Axis) - Center (Y_Axis);
      Start_Radius  : constant Length := Hypot (Start_DX, Start_DY);
      Finish_Radius : constant Length := Hypot (Finish_DX, Finish_DY);
   begin
      if not Position_Is_In_Bounds (Start_Pos, Params) or else not Position_Is_In_Bounds (Finish_Pos, Params) then
         return False;
      end if;

      --  Radius-zero and materially mismatched arcs are executed as lines. Both supported XY workspaces are convex,
      --  and Z/E are affine, so the endpoint checks enclose the complete fallback path.
      if Start_Radius <= 0.0 * mm or else abs (Start_Radius - Finish_Radius) > Radius_Tolerance then
         return True;
      end if;

      declare
         Theta_Start   : constant Dimensionless := Dimensionless_Math.Arctan (Start_DY / mm, Start_DX / mm);
         Offset_Scale  : constant Length :=
           Length'Max (abs Start_DX, Length'Max (abs Start_DY, Length'Max (abs Finish_DX, abs Finish_DY)));
         Coincident_XY : constant Boolean := Start_DX = Finish_DX and then Start_DY = Finish_DY;
         Theta_Delta   : Dimensionless := 0.0;

         function Point_Is_In_Bounds (Phase : Dimensionless) return Boolean;

         function Candidate_Passes (Phase : Dimensionless) return Boolean;

         function Point_Is_In_Bounds (Phase : Dimensionless) return Boolean is
            Pos : Position := Start_Pos;
         begin
            Pos (X_Axis) := Center (X_Axis) + Start_Radius * Dimensionless_Math.Cos (Phase);
            Pos (Y_Axis) := Center (Y_Axis) + Start_Radius * Dimensionless_Math.Sin (Phase);
            return Position_Is_In_Bounds (Pos, Params);
         end Point_Is_In_Bounds;

         function Candidate_Passes (Phase : Dimensionless) return Boolean
         is (not Phase_Is_On_Arc (Phase, Theta_Start, Theta_Delta) or else Point_Is_In_Bounds (Phase));
      begin
         if Coincident_XY then
            Theta_Delta := (if Clockwise then -Two_Pi else Two_Pi);
         else
            declare
               Start_X  : constant Dimensionless := Start_DX / Offset_Scale;
               Start_Y  : constant Dimensionless := Start_DY / Offset_Scale;
               Finish_X : constant Dimensionless := Finish_DX / Offset_Scale;
               Finish_Y : constant Dimensionless := Finish_DY / Offset_Scale;
               Cross    : constant Dimensionless := Start_X * Finish_Y - Start_Y * Finish_X;
               Dot      : constant Dimensionless := Start_X * Finish_X + Start_Y * Finish_Y;
            begin
               Theta_Delta := Dimensionless_Math.Arctan (Cross, Dot);
            end;

            if Theta_Delta = 0.0 then
               return True;
            elsif Clockwise and then Theta_Delta > 0.0 then
               Theta_Delta := Theta_Delta - Two_Pi;
            elsif not Clockwise and then Theta_Delta < 0.0 then
               Theta_Delta := Theta_Delta + Two_Pi;
            end if;
         end if;

         case Params.Bounds.Kind is
            when Rectangular_Workspace =>
               if not Candidate_Passes (0.0)
                 or else not Candidate_Passes (0.5 * Ada.Numerics.Pi)
                 or else not Candidate_Passes (Ada.Numerics.Pi)
                 or else not Candidate_Passes (-0.5 * Ada.Numerics.Pi)
               then
                  return False;
               end if;

            when Circular_Workspace    =>
               --  Distance from the workspace centre is greatest where the arc's radial vector points in the same
               --  direction as the vector from the workspace centre to the arc centre.
               if (Center (X_Axis) /= 0.0 * mm or else Center (Y_Axis) /= 0.0 * mm)
                 and then not Candidate_Passes (Dimensionless_Math.Arctan (Center (Y_Axis) / mm, Center (X_Axis) / mm))
               then
                  return False;
               end if;
         end case;

         --  Within the accepted radius tolerance, the executed primitive uses Start_Radius rather than the requested
         --  finish radius. Check that projected endpoint explicitly.
         if Finish_Radius <= 0.0 * mm then
            return False;
         end if;

         declare
            Pos   : Position := Finish_Pos;
            Scale : constant Dimensionless := Start_Radius / Finish_Radius;
         begin
            Pos (X_Axis) := Center (X_Axis) + Scale * Finish_DX;
            Pos (Y_Axis) := Center (Y_Axis) + Scale * Finish_DY;
            return Position_Is_In_Bounds (Pos, Params);
         end;
      end;
   exception
      when Constraint_Error =>
         return False;
   end Helix_Is_In_Bounds;

   function Nth_Root_Ratio (Numerator, Denominator : Dimensionless; Degree : Positive) return Dimensionless is
      Exponent_Difference : Integer;
      Root_Exponent       : Integer;
      Exponent_Remainder  : Integer;
   begin
      if Numerator <= 0.0 then
         return 0.0;
      elsif Denominator <= 0.0 then
         return Dimensionless'Last;
      elsif Degree = 1
        and then Dimensionless'Machine_Radix = 2
        and then Numerator in 2.0**(-128) .. 2.0**128
        and then Denominator in 2.0**(-128) .. 2.0**128
      then
         --  Both the quotient and its operands are normal. Power-of-two normalization cannot change the
         --  rounded quotient here, so avoid decomposing and reconstructing the common tangent-only ratio.
         return Numerator / Denominator;
      end if;

      Exponent_Difference := Dimensionless'Exponent (Numerator) - Dimensionless'Exponent (Denominator);
      Root_Exponent := Exponent_Difference / Degree;
      Exponent_Remainder := Exponent_Difference rem Degree;
      if Exponent_Remainder < 0 then
         Exponent_Remainder := @ + Degree;
         Root_Exponent := @ - 1;
      end if;

      declare
         Radicand : constant Dimensionless :=
           Dimensionless'Fraction (Numerator)
           / Dimensionless'Fraction (Denominator)
           * Dimensionless (Dimensionless'Machine_Radix**Exponent_Remainder);
         Mantissa : constant Dimensionless :=
           (if Degree = 1 then Radicand else Dimensionless_Math."**" (Radicand, 1.0 / Dimensionless (Degree)));
      begin
         --  Check the normalized exponent first to avoid infinity, including subnormal denominators produced by
         --  outward-rounded geometric bounds.
         if Root_Exponent + Dimensionless'Exponent (Mantissa) > Dimensionless'Exponent (Dimensionless'Last) then
            return Dimensionless'Last;
         end if;
         return Dimensionless'Scaling (Mantissa, Root_Exponent);
      end;
   end Nth_Root_Ratio;

   function Constant_Speed_Axial_Ceiling
     (Params  : Kinematic_Parameters;
      Bounds  : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel : Velocity;
      Safety  : Dimensionless := 0.999) return Velocity
   is
      Result : Velocity := Max_Vel;

      procedure Apply_Power_Ceiling (Numerator, Denominator : Dimensionless; Degree : Positive);

      procedure Apply_Power_Ceiling (Numerator, Denominator : Dimensionless; Degree : Positive) is
         Root      : Dimensionless;
         Candidate : Dimensionless;
      begin
         if Denominator <= 0.0 or else Result <= 0.0 * mm / s then
            return;
         elsif Numerator <= 0.0 or else Safety <= 0.0 then
            Result := 0.0 * mm / s;
            return;
         end if;

         if Degree > 1
           and then Dimensionless'Machine_Radix = 2
           and then Dimensionless'Machine_Mantissa <= 53
           and then Dimensionless'Machine_Emax >= 1024
           and then Safety in 2.0**(-8) .. 2.0**8
           and then Numerator in 2.0**(-300) .. 2.0**300
           and then Denominator in 2.0**(-300) .. 2.0**300
           and then Result in 2.0**(-100) * mm / s .. 2.0**100 * mm / s
         then
            --  The ceiling Safety * (Numerator / Denominator)**(1 / Degree) leaves Result alone exactly when
            --  Numerator >= Denominator * (Result / Safety)**Degree. The ranges above keep that power and product
            --  normal, so they are correct to a dozen units in the last place, and the root below to a few. A
            --  numerator beyond the product by 2**-40 therefore has a computed ceiling above Result, which taking
            --  the minimum would discard. Most ceilings are that far from binding and need no root at all.
            declare
               pragma Suppress (Overflow_Check);
               Speed : constant Dimensionless := (Result / (mm / s)) / Safety;
               Power : Dimensionless := Speed;
            begin
               for Factor in 2 .. Degree loop
                  Power := Power * Speed;
               end loop;
               if Numerator > Denominator * Power * (1.0 + 2.0**(-40)) then
                  return;
               end if;
            end;
         end if;

         Root := Nth_Root_Ratio (Numerator, Denominator, Degree);
         if Safety <= 1.0 or else Root <= Dimensionless'Last / Safety then
            Candidate := Safety * Root;
         else
            Candidate := Dimensionless'Last;
         end if;
         Result := Velocity'Min (Result, Candidate * mm / s);
      end Apply_Power_Ceiling;
   begin
      for A in Axis_Name loop
         if Bounds.Velocity (A) > 0.0 then
            Apply_Power_Ceiling (Dimensionless (Params.Axial_Velocity_Maxes (A) / (mm / s)), Bounds.Velocity (A), 1);
         end if;

         if Bounds.Acceleration (A) > 0.0 / mm then
            Apply_Power_Ceiling
              (Dimensionless (Params.Axial_Acceleration_Maxes (A) / (mm / s**2)),
               Dimensionless (Bounds.Acceleration (A) / (1.0 / mm)),
               2);
         end if;

         if Bounds.Jerk (A) > 0.0 / mm**2 then
            Apply_Power_Ceiling
              (Dimensionless (Params.Axial_Jerk_Maxes (A) / (mm / s**3)),
               Dimensionless (Bounds.Jerk (A) / (1.0 / mm**2)),
               3);
         end if;

         if Bounds.Snap (A) > 0.0 / mm**3 then
            Apply_Power_Ceiling
              (Dimensionless (Params.Axial_Snap_Maxes (A) / (mm / s**4)),
               Dimensionless (Bounds.Snap (A) / (1.0 / mm**3)),
               4);
         end if;

         if Bounds.Crackle (A) > 0.0 / mm**4 then
            Apply_Power_Ceiling
              (Dimensionless (Params.Axial_Crackle_Maxes (A) / (mm / s**5)),
               Dimensionless (Bounds.Crackle (A) / (1.0 / mm**4)),
               5);
         end if;
      end loop;

      return Velocity'Max (0.0 * mm / s, Result);
   end Constant_Speed_Axial_Ceiling;

   procedure Solve_Mixed_Derivative_Limits
     (Params       : Kinematic_Parameters;
      Bounds       : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel      : Velocity;
      Safety       : Dimensionless;
      Scalar_Maxes : Scalar_Derivative_Limits;
      Time_Scaling : Boolean;
      Cache        : in out Mixed_Derivative_Cache;
      Result       : out Mixed_Derivative_Limit_Result;
      Limit_Scale  : out Dimensionless);

   procedure Solve_Mixed_Derivative_Limits
     (Params       : Kinematic_Parameters;
      Bounds       : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel      : Velocity;
      Safety       : Dimensionless;
      Scalar_Maxes : Scalar_Derivative_Limits;
      Time_Scaling : Boolean;
      Cache        : in out Mixed_Derivative_Cache;
      Result       : out Mixed_Derivative_Limit_Result;
      Limit_Scale  : out Dimensionless)
   is
      Base : Scalar_Derivative_Limits :=
        (Acceleration_Max => 1.0E100 * mm / s**2,
         Jerk_Max         => 1.0E100 * mm / s**3,
         Snap_Max         => 1.0E100 * mm / s**4,
         Crackle_Max      => 1.0E100 * mm / s**5);

      type Scaled_Nonnegative_Array is array (Positive range <>) of Scaled_Nonnegative;
      type Dimensionless_Array is array (Positive range <>) of Dimensionless;

      Root_Settling_Attempts : constant Positive := 12;
      --  How many steps below a computed root may be tried before bisecting instead. The steps quadruple from
      --  one part in 2**52, so together they reach far beyond any rounding of the root.

      Maximum_Newton_Steps : constant Positive := 64;
      --  Each step from far above the root of a quartic still removes a quarter of the distance to zero, and the
      --  starting bounds below leave little of that, so this is only a guard.

      Zero : constant Scaled_Nonnegative := (Fraction => 0.0, Exponent => 0);
      One  : constant Scaled_Nonnegative :=
        (Fraction => Dimensionless'Fraction (1.0), Exponent => Dimensionless'Exponent (1.0));

      --  Chain-rule coefficients, decomposed as by To_Scaled without repeating it for every axis and call.
      Two     : constant Scaled_Nonnegative :=
        (Fraction => Dimensionless'Fraction (2.0), Exponent => Dimensionless'Exponent (2.0));
      Three   : constant Scaled_Nonnegative :=
        (Fraction => Dimensionless'Fraction (3.0), Exponent => Dimensionless'Exponent (3.0));
      Four    : constant Scaled_Nonnegative :=
        (Fraction => Dimensionless'Fraction (4.0), Exponent => Dimensionless'Exponent (4.0));
      Five    : constant Scaled_Nonnegative :=
        (Fraction => Dimensionless'Fraction (5.0), Exponent => Dimensionless'Exponent (5.0));
      Six     : constant Scaled_Nonnegative :=
        (Fraction => Dimensionless'Fraction (6.0), Exponent => Dimensionless'Exponent (6.0));
      Ten     : constant Scaled_Nonnegative :=
        (Fraction => Dimensionless'Fraction (10.0), Exponent => Dimensionless'Exponent (10.0));
      Fifteen : constant Scaled_Nonnegative :=
        (Fraction => Dimensionless'Fraction (15.0), Exponent => Dimensionless'Exponent (15.0));

      function To_Scaled (Value : Dimensionless) return Scaled_Nonnegative;
      function Scaled_Product (Left, Right : Scaled_Nonnegative) return Scaled_Nonnegative;
      function Scaled_Product (Factors : Scaled_Nonnegative_Array) return Scaled_Nonnegative;
      function Aligned_Fraction (Value : Scaled_Nonnegative; Exponent : Integer) return Dimensionless;
      function Scaled_Add (Left, Right : Scaled_Nonnegative) return Scaled_Nonnegative;
      function Scaled_Sum (Values : Scaled_Nonnegative_Array) return Scaled_Nonnegative;
      function Scaled_Less_Than (Left, Right : Scaled_Nonnegative) return Boolean;
      function Scaled_Less_Or_Equal (Left, Right : Scaled_Nonnegative) return Boolean;
      function Scaled_Subtract (Left, Right : Scaled_Nonnegative) return Scaled_Nonnegative;
      function Scaled_Square_Root (Value : Scaled_Nonnegative) return Scaled_Nonnegative;
      function Scaled_Ratio (Numerator, Denominator : Scaled_Nonnegative) return Dimensionless;
      function Positive_Quadratic_Scale (Remainder, Linear, Quadratic : Scaled_Nonnegative) return Dimensionless;

      procedure Newton_Root (C1, C2, C3, C4, Remainder, Start, Floor : Dimensionless; Estimate : out Dimensionless);
      --  Estimate the largest scale at which the budget polynomial C1 * s + C2 * s**2 + C3 * s**3 + C4 * s**4 still
      --  fits a positive Remainder, given that it does not fit at Start. The coefficients are nonnegative, so the
      --  polynomial is increasing and convex, and Newton's method from above its root decreases onto the root.
      --  Each positive monomial alone also bounds the root from above, which gives a start near it.
      --
      --  The estimate carries no certificate: the caller accepts only a scale at which it evaluates the budget to
      --  fit. Iteration stops at the first scale which fits by the same Horner evaluation as the callers use, at a
      --  step which no longer decreases, or below Floor, where the caller's plain evaluation is not valid.
      --
      --  Only sums, products, quotients, comparisons and square roots of quotients of the inputs are formed.
      --  While every one of them stays normal, multiplying all coefficients and Remainder by one power of two
      --  therefore changes nothing: the two callers, one with plain coefficients and one with the same
      --  coefficients normalized to a common exponent, obtain the same estimate bit for bit.

      procedure Apply_Linear_Constraint (Maximum, Fixed, Linear : Scaled_Nonnegative);
      procedure Apply_Quadratic_Constraint (Maximum, Fixed, Linear, Quadratic : Scaled_Nonnegative);
      procedure Apply_Time_Constraint (Maximum, Fixed : Scaled_Nonnegative; Terms : Scaled_Nonnegative_Array);
      procedure Apply_Bounded_Time_Constraint (Maximum, Fixed : Dimensionless; Terms : Dimensionless_Array)
      with Inline_Always;
      procedure Tighten_Bounded_Time_Constraint (Maximum, Fixed : Dimensionless; Terms : Dimensionless_Array)
      with No_Inline;

      function To_Scaled (Value : Dimensionless) return Scaled_Nonnegative is
      begin
         if Value <= 0.0 then
            return Zero;
         elsif not (Value <= Dimensionless'Last) then
            raise Constraint_Error with "Nonfinite derivative-limit coefficient.";
         else
            return (Fraction => Dimensionless'Fraction (Value), Exponent => Dimensionless'Exponent (Value));
         end if;
      end To_Scaled;

      function Scaled_Product (Left, Right : Scaled_Nonnegative) return Scaled_Nonnegative is
         Product     : Dimensionless;
         Renormalize : Boolean;
      begin
         if Left.Fraction = 0.0 or else Right.Fraction = 0.0 then
            return Zero;
         end if;

         declare
            pragma Suppress (Overflow_Check);
            --  Nonzero fractions are normalized into [1/radix, 1). Their product lies in [1/radix**2, 1),
            --  so a single exact radix multiplication normalizes it, without decomposing the float twice.
         begin
            Product := Left.Fraction * Right.Fraction;
            Renormalize := Product < 1.0 / Dimensionless (Dimensionless'Machine_Radix);
            if Renormalize then
               Product := Product * Dimensionless (Dimensionless'Machine_Radix);
            end if;
         end;
         --  Keep integer exponent arithmetic checked independently of the bounded mantissa arithmetic.
         return (Fraction => Product, Exponent => Left.Exponent + Right.Exponent - Boolean'Pos (Renormalize));
      end Scaled_Product;

      function Scaled_Product (Factors : Scaled_Nonnegative_Array) return Scaled_Nonnegative is
         Product : Scaled_Nonnegative := One;
      begin
         for Factor of Factors loop
            if Factor.Fraction = 0.0 then
               return Zero;
            end if;
            Product := Scaled_Product (Product, Factor);
         end loop;
         return Product;
      end Scaled_Product;

      function Aligned_Fraction (Value : Scaled_Nonnegative; Exponent : Integer) return Dimensionless is
      begin
         if Value.Fraction = 0.0 then
            return 0.0;
         elsif Value.Exponent = Exponent then
            return Value.Fraction;
         else
            return Dimensionless'Scaling (Value.Fraction, Value.Exponent - Exponent);
         end if;
      exception
         when Constraint_Error =>
            return 0.0;
      end Aligned_Fraction;

      function Scaled_Add (Left, Right : Scaled_Nonnegative) return Scaled_Nonnegative is
         Common_Exponent : constant Integer := Integer'Max (Left.Exponent, Right.Exponent);
         Sum             : Dimensionless;
         Renormalize     : Boolean;
      begin
         if Left.Fraction = 0.0 then
            return Right;
         elsif Right.Fraction = 0.0 then
            return Left;
         end if;

         declare
            pragma Suppress (Overflow_Check);
            --  Alignment only scales down. At least one operand retains its normalized fraction, so the sum
            --  lies in [1/radix, 2); dividing by radix once is sufficient when the sum reaches one.
         begin
            Sum := Aligned_Fraction (Left, Common_Exponent) + Aligned_Fraction (Right, Common_Exponent);
            Renormalize := Sum >= 1.0;
            if Renormalize then
               Sum := Sum / Dimensionless (Dimensionless'Machine_Radix);
            end if;
         end;
         return (Fraction => Sum, Exponent => Common_Exponent + Boolean'Pos (Renormalize));
      end Scaled_Add;

      function Scaled_Sum (Values : Scaled_Nonnegative_Array) return Scaled_Nonnegative is
         Sum : Scaled_Nonnegative := Zero;
      begin
         for Value of Values loop
            Sum := Scaled_Add (Sum, Value);
         end loop;
         return Sum;
      end Scaled_Sum;

      function Scaled_Less_Than (Left, Right : Scaled_Nonnegative) return Boolean is
      begin
         if Left.Fraction = 0.0 then
            return Right.Fraction /= 0.0;
         elsif Right.Fraction = 0.0 then
            return False;
         elsif Left.Exponent /= Right.Exponent then
            return Left.Exponent < Right.Exponent;
         else
            return Left.Fraction < Right.Fraction;
         end if;
      end Scaled_Less_Than;

      function Scaled_Less_Or_Equal (Left, Right : Scaled_Nonnegative) return Boolean is
      begin
         return not Scaled_Less_Than (Left => Right, Right => Left);
      end Scaled_Less_Or_Equal;

      function Scaled_Subtract (Left, Right : Scaled_Nonnegative) return Scaled_Nonnegative is
      begin
         if Right.Fraction = 0.0 then
            return Left;
         elsif not Scaled_Less_Or_Equal (Left => Right, Right => Left) then
            return Zero;
         end if;

         declare
            Difference      : constant Dimensionless := Left.Fraction - Aligned_Fraction (Right, Left.Exponent);
            Exponent_Change : Integer;
         begin
            if Difference <= 0.0 then
               return Zero;
            end if;
            Exponent_Change := Dimensionless'Exponent (Difference);
            return (Fraction => Dimensionless'Fraction (Difference), Exponent => Left.Exponent + Exponent_Change);
         end;
      end Scaled_Subtract;

      function Scaled_Square_Root (Value : Scaled_Nonnegative) return Scaled_Nonnegative is
      begin
         if Value.Fraction = 0.0 then
            return Zero;
         end if;

         declare
            Exponent_Remainder : constant Integer := Value.Exponent mod 2;
            Half_Exponent      : constant Integer := (Value.Exponent - Exponent_Remainder) / 2;
            Root               : constant Scaled_Nonnegative :=
              To_Scaled (Dimensionless_Math.Sqrt (Dimensionless'Scaling (Value.Fraction, Exponent_Remainder)));
         begin
            return (Fraction => Root.Fraction, Exponent => Root.Exponent + Half_Exponent);
         end;
      end Scaled_Square_Root;

      function Scaled_Ratio (Numerator, Denominator : Scaled_Nonnegative) return Dimensionless is
      begin
         if Numerator.Fraction = 0.0 then
            return 0.0;
         elsif Denominator.Fraction = 0.0 or else not Scaled_Less_Or_Equal (Numerator, Denominator) then
            return 1.0;
         end if;

         declare
            Fraction_Ratio  : constant Dimensionless := Numerator.Fraction / Denominator.Fraction;
            Exponent_Change : constant Integer := Dimensionless'Exponent (Fraction_Ratio);
            Exponent        : constant Integer := Numerator.Exponent - Denominator.Exponent + Exponent_Change;
         begin
            return Dimensionless'Scaling (Dimensionless'Fraction (Fraction_Ratio), Exponent);
         exception
            when Constraint_Error =>
               return 0.0;
         end;
      end Scaled_Ratio;

      function Positive_Quadratic_Scale (Remainder, Linear, Quadratic : Scaled_Nonnegative) return Dimensionless is
      begin
         if Remainder.Fraction = 0.0 then
            return (if Linear.Fraction = 0.0 and then Quadratic.Fraction = 0.0 then 1.0 else 0.0);
         elsif Scaled_Less_Or_Equal (Scaled_Sum ([Linear, Quadratic]), Remainder) then
            return 1.0;
         elsif Quadratic.Fraction = 0.0 then
            return Scaled_Ratio (Remainder, Linear);
         end if;

         declare
            --  This is the cancellation-free conjugate root
            --  2R / (L + sqrt (L**2 + 4QR)), evaluated without forming overflowing products or ratios.
            Discriminant : constant Scaled_Nonnegative :=
              Scaled_Sum
                ([Scaled_Product (Linear, Linear), Scaled_Product (Four, Scaled_Product (Quadratic, Remainder))]);
            Denominator  : constant Scaled_Nonnegative := Scaled_Sum ([Linear, Scaled_Square_Root (Discriminant)]);
         begin
            return Scaled_Ratio (Scaled_Product (Two, Remainder), Denominator);
         end;
      end Positive_Quadratic_Scale;

      procedure Newton_Root (C1, C2, C3, C4, Remainder, Start, Floor : Dimensionless; Estimate : out Dimensionless) is
         pragma Suppress (Overflow_Check);
         --  Both callers bound every operand so that no finite intermediate can overflow; see them for the ranges.
         S                    : Dimensionless := Start;
         Value, Slope, Next_S : Dimensionless;
      begin
         if C1 > 0.0 and then C1 * S > Remainder then
            S := Remainder / C1;
         end if;
         if C2 > 0.0 and then C2 * S * S > Remainder then
            S := Dimensionless_Math.Sqrt (Remainder / C2);
         end if;
         if C4 > 0.0 and then C4 * S * S * S * S > Remainder then
            S := Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (Remainder / C4));
         end if;
         for Step in 1 .. Maximum_Newton_Steps loop
            exit when not (S >= Floor);
            --  The same operations as the callers' evaluation, whose first step adds C4 to an exact zero.
            Value := (((C4 * S + C3) * S + C2) * S + C1) * S;
            exit when Value <= Remainder;
            Slope := ((4.0 * C4 * S + 3.0 * C3) * S + 2.0 * C2) * S + C1;
            --  Every term of S * Slope is at least the matching term of Value, so this step is at most S.
            Next_S := S - (Value - Remainder) / Slope;
            exit when not (Next_S < S);
            S := Next_S;
         end loop;
         Estimate := S;
      end Newton_Root;

      procedure Apply_Linear_Constraint (Maximum, Fixed, Linear : Scaled_Nonnegative) is
      begin
         if Scaled_Less_Than (Maximum, Fixed) then
            Result.Valid := False;
         elsif Linear.Fraction /= 0.0 then
            Limit_Scale := Dimensionless'Min (Limit_Scale, Scaled_Ratio (Scaled_Subtract (Maximum, Fixed), Linear));
         end if;
      end Apply_Linear_Constraint;

      procedure Apply_Quadratic_Constraint (Maximum, Fixed, Linear, Quadratic : Scaled_Nonnegative) is
      begin
         if Scaled_Less_Than (Maximum, Fixed) then
            Result.Valid := False;
         else
            Limit_Scale :=
              Dimensionless'Min
                (Limit_Scale, Positive_Quadratic_Scale (Scaled_Subtract (Maximum, Fixed), Linear, Quadratic));
         end if;
      end Apply_Quadratic_Constraint;

      procedure Apply_Time_Constraint (Maximum, Fixed : Scaled_Nonnegative; Terms : Scaled_Nonnegative_Array) is
         Remainder            : constant Scaled_Nonnegative := Scaled_Subtract (Maximum, Fixed);
         Lower                : Dimensionless := 0.0;
         Upper                : Dimensionless := Limit_Scale;
         Normalized_Terms     : array (Terms'Range) of Dimensionless;
         Normalized_Remainder : Dimensionless;
         Use_Normalized       : Boolean := False;

         function Fits (Scale : Dimensionless) return Boolean;
         procedure Normalize_Coefficients;

         procedure Normalize_Coefficients is
            Common_Exponent : Integer := Remainder.Exponent;
         begin
            if Terms'First /= 1
              or else Terms'Length > 4
              or else Dimensionless'Machine_Radix /= 2
              or else Dimensionless'Model_Emin > -704
            then
               return;
            end if;
            for Term of Terms loop
               if Term.Fraction /= 0.0 then
                  Common_Exponent := Integer'Max (Common_Exponent, Term.Exponent);
               end if;
            end loop;
            if Common_Exponent - Remainder.Exponent > 128
              or else (for some Term of Terms => Term.Fraction /= 0.0 and then Common_Exponent - Term.Exponent > 128)
            then
               return;
            end if;
            Normalized_Remainder := Aligned_Fraction (Remainder, Common_Exponent);
            for I in Terms'Range loop
               Normalized_Terms (I) := Aligned_Fraction (Terms (I), Common_Exponent);
            end loop;
            Use_Normalized := True;
         end Normalize_Coefficients;

         function Fits (Scale : Dimensionless) return Boolean is
            Value : Scaled_Nonnegative := Zero;
         begin
            if Use_Normalized and then Scale >= 2.0**(-144) and then Scale <= 1.0 then
               declare
                  pragma Suppress (Overflow_Check);
                  Sum : Dimensionless := 0.0;
                  --  There are at most four nonnegative coefficients, each below one and each nonzero one
                  --  at least 2**-129. With Scale in [2**-144, 1], every nonzero Horner intermediate is normal
                  --  (at least 2**-705) and at most four. A shared power-of-two normalization is therefore
                  --  exact and commutes with each rounded operation; it preserves the scaled path's decision.
               begin
                  for I in reverse Terms'Range loop
                     Sum := (Sum + Normalized_Terms (I)) * Scale;
                  end loop;
                  return Sum <= Normalized_Remainder;
               end;
            end if;
            declare
               Factor : constant Scaled_Nonnegative := To_Scaled (Scale);
            begin
               for I in reverse Terms'Range loop
                  Value := Scaled_Product (Scaled_Add (Value, Terms (I)), Factor);
               end loop;
            end;
            return Scaled_Less_Or_Equal (Value, Remainder);
         end Fits;
      begin
         if Scaled_Less_Than (Maximum, Fixed) then
            Result.Valid := False;
            return;
         elsif Fits (Upper) then
            return;
         elsif Remainder.Fraction = 0.0 then
            Limit_Scale := 0.0;
            return;
         end if;

         --  The budget is a polynomial in the scale with nonnegative coefficients, so it is increasing and its
         --  root below Upper is the largest scale that fits. Most binding budgets are the linear acceleration
         --  budget or the quadratic jerk budget, whose roots have closed forms. Such a root is the limit to
         --  within rounding, on either side of it, and only a scale at which the budget is evaluated to fit is
         --  accepted, so step below the root until one does. That evaluation is the certificate, exactly as for
         --  a bisected scale; every later budget only lowers the scale, and the evaluation is monotone in it.
         declare
            Degree   : Natural := 0;
            Estimate : Dimensionless;
            Step     : Dimensionless := Dimensionless'Model_Epsilon;
         begin
            for I in Terms'Range loop
               if Terms (I).Fraction > 0.0 then
                  Degree := I;
               end if;
            end loop;
            if Degree in 1 .. 2 then
               Estimate :=
                 Dimensionless'Min
                   (Upper,
                    (if Degree = 1
                     then Scaled_Ratio (Remainder, Terms (1))
                     else Positive_Quadratic_Scale (Remainder, Terms (1), Terms (2))));
               for Attempt in 1 .. Root_Settling_Attempts loop
                  if Fits (Estimate) then
                     Limit_Scale := Estimate;
                     return;
                  end if;
                  Estimate := Estimate * (1.0 - Step);
                  Step := 4.0 * Step;
               end loop;
            end if;
         end;

         --  The cubic snap and quartic crackle budgets, and any root that would not settle, are found by Newton's
         --  method where the coefficients can share one exponent. Normalized, they and every intermediate of
         --  Newton_Root are normal for scales from 2**-144, as for the evaluation in Fits. The estimate is then
         --  settled like a closed-form root: the first evaluated fit at or just below it is the certificate.
         Normalize_Coefficients;
         if Use_Normalized then
            declare
               Padded   : array (1 .. 4) of Dimensionless := [others => 0.0];
               Estimate : Dimensionless;
               Step     : Dimensionless := Dimensionless'Model_Epsilon;
            begin
               for I in Terms'Range loop
                  Padded (I) := Normalized_Terms (I);
               end loop;
               Newton_Root
                 (Padded (1), Padded (2), Padded (3), Padded (4), Normalized_Remainder, Upper, 2.0**(-144), Estimate);
               for Attempt in 1 .. Root_Settling_Attempts loop
                  exit when not (Estimate >= 2.0**(-144));
                  if Fits (Estimate) then
                     Limit_Scale := Estimate;
                     return;
                  end if;
                  Estimate := Estimate * (1.0 - Step);
                  Step := 4.0 * Step;
               end loop;
            end;
         end if;

         --  Otherwise bisect. Each positive monomial bounds the root from above. Establish its magnitude before
         --  bisection, including when the feasible scale is hundreds of decades below one.
         for I in Terms'Range loop
            if Terms (I).Fraction > 0.0 then
               declare
                  Exponent : constant Integer := Remainder.Exponent - Terms (I).Exponent;
                  Root     : constant Dimensionless :=
                    Nth_Root_Ratio (Dimensionless'Scaling (Remainder.Fraction, Exponent mod I), Terms (I).Fraction, I);
               begin
                  if Exponent <= 0 then
                     Upper := Dimensionless'Min (Upper, Dimensionless'Scaling (Root, (Exponent - Exponent mod I) / I));
                  end if;
               exception
                  when Constraint_Error =>
                     --  Underflow of a positive root leaves no representable certified scale.
                     Upper := 0.0;
               end;
            end if;
         end loop;
         if Fits (Upper) then
            Limit_Scale := Upper;
            return;
         end if;
         --  Extreme exponent spreads retain scaled arithmetic here.
         for Iteration in 1 .. 16 loop
            declare
               Mid : constant Dimensionless := Lower + 0.5 * (Upper - Lower);
            begin
               if Fits (Mid) then
                  Lower := Mid;
               else
                  Upper := Mid;
               end if;
            end;
         end loop;
         Limit_Scale := Lower;
      end Apply_Time_Constraint;

      procedure Apply_Bounded_Time_Constraint (Maximum, Fixed : Dimensionless; Terms : Dimensionless_Array) is
         pragma Suppress (Overflow_Check);
         Sum : Dimensionless := 0.0;
      begin
         --  The caller bounds every nonzero input factor to [2**-64, 2**64]. Each coefficient contains
         --  at most five such factors and a constant <= 15, with at most two monomials per coefficient.
         --  Nonzero coefficients are therefore >= 2**-320 and <= 2**325. The fixed term has at most six
         --  factors and is <= 2**384. All products and sums are normal and preserve the scaled evaluation
         --  order. A positive Maximum - Fixed is >= 2**-117 with at most 53 mantissa bits.
         --
         --  Only search with Upper >= 2**-128: sixteen bisections then keep every positive trial >= 2**-144.
         --  Four Horner steps stay in [2**-896, 2**327] when nonzero. Thus no overflow or underflow can alter
         --  a budget comparison. Smaller scales use Apply_Time_Constraint, retaining the exact coefficients.
         --
         --  Nearly every budget already fits at the scale that the earlier ones left, so that one evaluation is
         --  inlined at each of the sixteen calls, where the number of terms is known, and only a budget that
         --  binds calls the search.
         if Fixed > Maximum then
            Result.Valid := False;
            return;
         elsif Limit_Scale = 0.0 then
            return;
         elsif Limit_Scale >= 2.0**(-128) then
            for I in reverse Terms'Range loop
               Sum := (Sum + Terms (I)) * Limit_Scale;
            end loop;
            if Sum <= Maximum - Fixed then
               return;
            end if;
         end if;
         Tighten_Bounded_Time_Constraint (Maximum, Fixed, Terms);
      end Apply_Bounded_Time_Constraint;

      procedure Tighten_Bounded_Time_Constraint (Maximum, Fixed : Dimensionless; Terms : Dimensionless_Array) is
         pragma Suppress (Overflow_Check);
         Remainder : constant Dimensionless := Maximum - Fixed;
         Degree    : Natural := 0;
         Estimate  : Dimensionless;
         Step      : Dimensionless := Dimensionless'Model_Epsilon;

         function Fits (Scale : Dimensionless) return Boolean;
         function Shares_Exponent return Boolean;
         --  Whether Apply_Time_Constraint would normalize these coefficients: the binary exponents of the remainder
         --  and of the positive terms span at most 128.
         procedure Fallback;
         procedure Search;
         --  Find the root by Newton's method, or else bound the scale by each monomial and bisect below that,
         --  each exactly as Apply_Time_Constraint does.

         function Fits (Scale : Dimensionless) return Boolean is
            Sum : Dimensionless := 0.0;
         begin
            for I in reverse Terms'Range loop
               Sum := (Sum + Terms (I)) * Scale;
            end loop;
            return Sum <= Remainder;
         end Fits;

         procedure Fallback is
            Scaled_Terms : Scaled_Nonnegative_Array (Terms'Range);
         begin
            for I in Terms'Range loop
               Scaled_Terms (I) := To_Scaled (Terms (I));
            end loop;
            Apply_Time_Constraint (To_Scaled (Maximum), To_Scaled (Fixed), Scaled_Terms);
         end Fallback;

         function Shares_Exponent return Boolean is
            type Bits is mod 2**64;
            function To_Bits is new Ada.Unchecked_Conversion (Dimensionless, Bits);
            --  Every value here is positive and normal, so the exponent field orders like 'Exponent and differs
            --  from it by a constant.
            Use_Fields : constant Boolean := Dimensionless'Size = 64 and then Dimensionless'Machine_Mantissa = 53;

            function Binary_Exponent (Value : Dimensionless) return Integer
            is (if Use_Fields then Integer (To_Bits (Value) / 2**52) else Dimensionless'Exponent (Value));

            Low  : Integer := Binary_Exponent (Remainder);
            High : Integer := Low;
         begin
            if Terms'Length > 4 or else Dimensionless'Model_Emin > -704 then
               return False;
            end if;
            for Term of Terms loop
               if Term > 0.0 then
                  Low := Integer'Min (Low, Binary_Exponent (Term));
                  High := Integer'Max (High, Binary_Exponent (Term));
               end if;
            end loop;
            return High - Low <= 128;
         end Shares_Exponent;

         procedure Search is
            Lower : Dimensionless := 0.0;
            Upper : Dimensionless := Limit_Scale;
         begin
            if Shares_Exponent then
               --  Positive terms are in [2**-320, 2**325] and a positive remainder in [2**-117, 2**64], and the
               --  search stays at scales from 2**-128. Every Horner step of the value and of its slope is then
               --  normal, the quotients of the remainder by a term are within [2**-442, 2**384], and the Newton
               --  step is a normal number no greater than the scale or an exact zero.
               declare
                  Padded        : array (1 .. 4) of Dimensionless := [others => 0.0];
                  Root_Estimate : Dimensionless;
                  Settling_Step : Dimensionless := Dimensionless'Model_Epsilon;
               begin
                  for I in Terms'Range loop
                     Padded (I) := Terms (I);
                  end loop;
                  Newton_Root
                    (Padded (1),
                     Padded (2),
                     Padded (3),
                     Padded (4),
                     Remainder,
                     Limit_Scale,
                     2.0**(-128),
                     Root_Estimate);
                  for Attempt in 1 .. Root_Settling_Attempts loop
                     if not (Root_Estimate >= 2.0**(-128)) then
                        Fallback;
                        return;
                     elsif Fits (Root_Estimate) then
                        Limit_Scale := Root_Estimate;
                        return;
                     end if;
                     Root_Estimate := Root_Estimate * (1.0 - Settling_Step);
                     Settling_Step := 4.0 * Settling_Step;
                  end loop;
               end;
            end if;
            for I in Terms'Range loop
               if Terms (I) > 0.0 then
                  Upper := Dimensionless'Min (Upper, Nth_Root_Ratio (Remainder, Terms (I), I));
               end if;
            end loop;
            if Upper < 2.0**(-128) then
               Fallback;
               return;
            elsif Fits (Upper) then
               Limit_Scale := Upper;
               return;
            end if;
            for Iteration in 1 .. 16 loop
               declare
                  Mid : constant Dimensionless := Lower + 0.5 * (Upper - Lower);
               begin
                  if Fits (Mid) then
                     Lower := Mid;
                  else
                     Upper := Mid;
                  end if;
               end;
            end loop;
            Limit_Scale := Lower;
         end Search;
      begin
         --  Apply_Bounded_Time_Constraint has found that the budget holds a positive Limit_Scale and does not fit
         --  at it, or that the scale is too small to evaluate in plain arithmetic.
         --
         --  This follows Apply_Time_Constraint step for step, in plain arithmetic: the ranges that the caller
         --  guarantees keep every sum, product, quotient and root here normal, so each rounds as its scaled
         --  counterpart does and both procedures reach the same scale.
         if Limit_Scale < 2.0**(-128) then
            Fallback;
            return;
         elsif Remainder = 0.0 then
            Limit_Scale := 0.0;
            return;
         end if;

         for I in Terms'Range loop
            if Terms (I) > 0.0 then
               Degree := I;
            end if;
         end loop;
         case Degree is
            when 1      =>
               Estimate := (if Remainder > Terms (1) then 1.0 else Remainder / Terms (1));

            when 2      =>
               if Terms (1) + Terms (2) <= Remainder then
                  Estimate := 1.0;
               else
                  declare
                     Denominator : constant Dimensionless :=
                       Terms (1) + Dimensionless_Math.Sqrt (Terms (1) * Terms (1) + 4.0 * (Terms (2) * Remainder));
                  begin
                     Estimate := (if 2.0 * Remainder > Denominator then 1.0 else 2.0 * Remainder / Denominator);
                  end;
               end if;

            when others =>
               Search;
               return;
         end case;

         Estimate := Dimensionless'Min (Estimate, Limit_Scale);
         for Attempt in 1 .. Root_Settling_Attempts loop
            if not (Estimate >= 2.0**(-128)) then
               Fallback;
               return;
            elsif Fits (Estimate) then
               Limit_Scale := Estimate;
               return;
            end if;
            Estimate := Estimate * (1.0 - Step);
            Step := 4.0 * Step;
         end loop;
         Search;
      end Tighten_Bounded_Time_Constraint;

      Base_Acceleration_Raw                                         : Dimensionless := 1.0E100;
      Base_Jerk_Raw                                                 : Dimensionless := 1.0E100;
      Base_Snap_Raw                                                 : Dimensionless := 1.0E100;
      Base_Crackle_Raw                                              : Dimensionless := 1.0E100;
      Scaled_Acceleration, Scaled_Jerk, Scaled_Snap, Scaled_Crackle : Scaled_Nonnegative;
      V                                                             : Scaled_Nonnegative;
      Dimensionless_Inputs                                          : Boolean;
      Plain_Limit_Inputs                                            : Boolean;
      Scaled_Inputs_Filled                                          : Boolean := False;
      Positive_Finite_Max_Vel                                       : constant Boolean :=
        Max_Vel > 0.0 * mm / s and then Max_Vel <= Velocity'Last;
   begin
      if Time_Scaling and then Positive_Finite_Max_Vel and then not Cache.Ceiling_Filled then
         --  With finite limits and bounds, the velocity-only ceiling applies the same sequence of exact minima
         --  to any positive finite Max_Vel. Starting from the largest speed lets each later solve apply it once.
         Cache.Ceiling_Usable :=
           (for all A in Axis_Name =>
              Params.Axial_Velocity_Maxes (A) <= Velocity'Last and then Bounds.Velocity (A) <= Dimensionless'Last);
         if Cache.Ceiling_Usable then
            Cache.Velocity_Ceiling :=
              Constant_Speed_Axial_Ceiling
                (Params, (Velocity => Bounds.Velocity, others => <>), Velocity'Last, Safety => 1.0);
         end if;
         Cache.Ceiling_Filled := True;
      end if;
      Result :=
        (Valid   => True,
         Limits  => Base,
         Max_Vel =>
           (if not Time_Scaling
            then Constant_Speed_Axial_Ceiling (Params, Bounds, Max_Vel, Safety)
            elsif Positive_Finite_Max_Vel and then Cache.Ceiling_Usable
            then Velocity'Max (0.0 * mm / s, Velocity'Min (Max_Vel, Cache.Velocity_Ceiling))
            else
              Constant_Speed_Axial_Ceiling
                (Params, (Velocity => Bounds.Velocity, others => <>), Max_Vel, Safety => 1.0)));
      Limit_Scale := 1.0;
      if Time_Scaling and then Result.Max_Vel < Max_Vel then
         Result.Valid := False;
         Limit_Scale := 0.0;
         return;
      end if;
      if not Time_Scaling then
         for A in Axis_Name loop
            declare
               X1 : constant Dimensionless := Bounds.Velocity (A);
            begin
               if X1 > 0.0 then
                  Base_Acceleration_Raw :=
                    Dimensionless'Min
                      (Base_Acceleration_Raw,
                       Nth_Root_Ratio (Dimensionless (Params.Axial_Acceleration_Maxes (A) / (mm / s**2)), X1, 1));
                  Base_Jerk_Raw :=
                    Dimensionless'Min
                      (Base_Jerk_Raw,
                       Nth_Root_Ratio (Dimensionless (Params.Axial_Jerk_Maxes (A) / (mm / s**3)), X1, 1));
                  Base_Snap_Raw :=
                    Dimensionless'Min
                      (Base_Snap_Raw,
                       Nth_Root_Ratio (Dimensionless (Params.Axial_Snap_Maxes (A) / (mm / s**4)), X1, 1));
                  Base_Crackle_Raw :=
                    Dimensionless'Min
                      (Base_Crackle_Raw,
                       Nth_Root_Ratio (Dimensionless (Params.Axial_Crackle_Maxes (A) / (mm / s**5)), X1, 1));
               end if;
            end;
         end loop;

         Base_Acceleration_Raw :=
           Dimensionless'Min (Base_Acceleration_Raw, Scalar_Maxes.Acceleration_Max / (mm / s**2));
         Base_Jerk_Raw := Dimensionless'Min (Base_Jerk_Raw, Scalar_Maxes.Jerk_Max / (mm / s**3));
         Base_Snap_Raw := Dimensionless'Min (Base_Snap_Raw, Scalar_Maxes.Snap_Max / (mm / s**4));
         Base_Crackle_Raw := Dimensionless'Min (Base_Crackle_Raw, Scalar_Maxes.Crackle_Max / (mm / s**5));

      end if;

      if Time_Scaling then
         --  Scale the supplied trajectory itself, without clipping its peaks independently.
         Base_Acceleration_Raw := Scalar_Maxes.Acceleration_Max / (mm / s**2);
         Base_Jerk_Raw := Scalar_Maxes.Jerk_Max / (mm / s**3);
         Base_Snap_Raw := Scalar_Maxes.Snap_Max / (mm / s**4);
         Base_Crackle_Raw := Scalar_Maxes.Crackle_Max / (mm / s**5);
      end if;

      Base :=
        (Acceleration_Max => Base_Acceleration_Raw * mm / s**2,
         Jerk_Max         => Base_Jerk_Raw * mm / s**3,
         Snap_Max         => Base_Snap_Raw * mm / s**4,
         Crackle_Max      => Base_Crackle_Raw * mm / s**5);
      Result.Limits := Base;
      Dimensionless_Inputs :=
        Time_Scaling
        and then Dimensionless'Machine_Radix = 2
        and then Dimensionless'Machine_Mantissa <= 53
        and then Dimensionless'Model_Emin <= -1021
        and then Dimensionless'Machine_Emax >= 1024
        and then
          (for all Value of
             Dimensionless_Array'
               [Base_Acceleration_Raw, Base_Jerk_Raw, Base_Snap_Raw, Base_Crackle_Raw, Result.Max_Vel / (mm / s)] =>
             Value = 0.0 or else Value in 2.0**(-64) .. 2.0**64);
      Plain_Limit_Inputs :=
        not Time_Scaling
        and then Dimensionless'Machine_Radix = 2
        and then Dimensionless'Machine_Mantissa <= 53
        and then Dimensionless'Model_Emin <= -1021
        and then Dimensionless'Machine_Emax >= 1024
        and then
          (for all Value of
             Dimensionless_Array'
               [Base_Acceleration_Raw, Base_Jerk_Raw, Base_Snap_Raw, Base_Crackle_Raw, Result.Max_Vel / (mm / s)] =>
             Value = 0.0 or else Value in 2.0**(-64) .. 2.0**64);

      if not Time_Scaling
        and then Dimensionless'Machine_Radix = 2
        and then Dimensionless'Machine_Mantissa <= 53
        and then Dimensionless'Model_Emin <= -1021
        and then Dimensionless'Machine_Emax >= 1024
        and then Result.Max_Vel <= Velocity'Last
        and then
          (for all Value of
             Dimensionless_Array'[Base_Acceleration_Raw, Base_Jerk_Raw, Base_Snap_Raw, Base_Crackle_Raw] =>
             Value = 0.0 or else Value in 2.0**(-300) .. 2.0**350)
        and then
          (for all A in Axis_Name =>
             (Bounds.Velocity (A) <= 0.0 or else Bounds.Velocity (A) in 2.0**(-64) .. 2.0**64)
             and then Bounds.Acceleration (A) <= 0.0 / mm
             and then Bounds.Jerk (A) <= 0.0 / mm**2
             and then Bounds.Snap (A) <= 0.0 / mm**3
             and then Bounds.Crackle (A) <= 0.0 / mm**4
             and then
               (for all Value of
                  Dimensionless_Array'
                    [Params.Axial_Acceleration_Maxes (A) / (mm / s**2),
                     Params.Axial_Jerk_Maxes (A) / (mm / s**3),
                     Params.Axial_Snap_Maxes (A) / (mm / s**4),
                     Params.Axial_Crackle_Maxes (A) / (mm / s**5)] =>
                  Value = 0.0 or else Value in 2.0**(-300) .. 2.0**300))
      then
         --  Tangent-only bounds, as for every window's limits, leave each chain-rule budget with the single term
         --  X1 * base. The scaled solve below then reduces to capping the scale at maximum / (X1 * base) for each
         --  axis and derivative. With every nonzero operand within the ranges checked above, those products are
         --  normal and at most 2**414, and each quotient below one is normal and at least 2**-714. Plain arithmetic
         --  therefore rounds exactly as the scaled operations do and returns the same limits, without decomposing
         --  thirty-six coefficients per window. Anything outside these ranges, including a nonfinite value that
         --  the scaled solve rejects, takes that solve.
         declare
            pragma Suppress (Overflow_Check);

            procedure Apply_Plain (Maximum, Linear : Dimensionless);

            procedure Apply_Plain (Maximum, Linear : Dimensionless) is
            begin
               if Linear = 0.0 then
                  null;
               elsif Maximum = 0.0 then
                  Limit_Scale := 0.0;
               elsif Maximum < Linear then
                  Limit_Scale := Dimensionless'Min (Limit_Scale, Maximum / Linear);
               end if;
            end Apply_Plain;
         begin
            for A in Axis_Name loop
               declare
                  X1 : constant Dimensionless := Dimensionless'Max (0.0, Bounds.Velocity (A));
               begin
                  if X1 > 0.0 then
                     Apply_Plain (Params.Axial_Acceleration_Maxes (A) / (mm / s**2), X1 * Base_Acceleration_Raw);
                     Apply_Plain (Params.Axial_Jerk_Maxes (A) / (mm / s**3), X1 * Base_Jerk_Raw);
                     Apply_Plain (Params.Axial_Snap_Maxes (A) / (mm / s**4), X1 * Base_Snap_Raw);
                     Apply_Plain (Params.Axial_Crackle_Maxes (A) / (mm / s**5), X1 * Base_Crackle_Raw);
                  end if;
               end;
            end loop;
         end;
         Limit_Scale := Safety * Dimensionless'Min (1.0, Limit_Scale);
         Result.Limits :=
           (Acceleration_Max => Limit_Scale * Base.Acceleration_Max,
            Jerk_Max         => Limit_Scale * Base.Jerk_Max,
            Snap_Max         => Limit_Scale * Base.Snap_Max,
            Crackle_Max      => Limit_Scale * Base.Crackle_Max);
         return;
      end if;

      for A in Axis_Name loop
         if not Cache.Axes_Filled (A) then
            --  Check on first use, so a nonfinite bound or limit is still only rejected once a solve reaches its
            --  axis.
            declare
               Plain  : constant Mixed_Derivative_Plain_Coefficients :=
                 (X1                   => Dimensionless'Max (0.0, Bounds.Velocity (A)),
                  X2                   => Dimensionless'Max (0.0, Bounds.Acceleration (A) / (1.0 / mm)),
                  X3                   => Dimensionless'Max (0.0, Bounds.Jerk (A) / (1.0 / mm**2)),
                  X4                   => Dimensionless'Max (0.0, Bounds.Snap (A) / (1.0 / mm**3)),
                  X5                   => Dimensionless'Max (0.0, Bounds.Crackle (A) / (1.0 / mm**4)),
                  Acceleration_Maximum =>
                    Dimensionless'Max (0.0, Dimensionless (Params.Axial_Acceleration_Maxes (A) / (mm / s**2))),
                  Jerk_Maximum         =>
                    Dimensionless'Max (0.0, Dimensionless (Params.Axial_Jerk_Maxes (A) / (mm / s**3))),
                  Snap_Maximum         =>
                    Dimensionless'Max (0.0, Dimensionless (Params.Axial_Snap_Maxes (A) / (mm / s**4))),
                  Crackle_Maximum      =>
                    Dimensionless'Max (0.0, Dimensionless (Params.Axial_Crackle_Maxes (A) / (mm / s**5))));
               Values : constant Dimensionless_Array :=
                 [Plain.X1,
                  Plain.X2,
                  Plain.X3,
                  Plain.X4,
                  Plain.X5,
                  Plain.Acceleration_Maximum,
                  Plain.Jerk_Maximum,
                  Plain.Snap_Maximum,
                  Plain.Crackle_Maximum];
            begin
               --  Exactly the values which To_Scaled rejects, without decomposing any of them. The limits are
               --  tested as given, since a maximum need not propagate a NaN.
               if (for some Value of
                     Dimensionless_Array'
                       [Plain.X1,
                        Plain.X2,
                        Plain.X3,
                        Plain.X4,
                        Plain.X5,
                        Dimensionless (Params.Axial_Acceleration_Maxes (A) / (mm / s**2)),
                        Dimensionless (Params.Axial_Jerk_Maxes (A) / (mm / s**3)),
                        Dimensionless (Params.Axial_Snap_Maxes (A) / (mm / s**4)),
                        Dimensionless (Params.Axial_Crackle_Maxes (A) / (mm / s**5))] =>
                     not (Value <= 0.0) and then not (Value <= Dimensionless'Last))
               then
                  raise Constraint_Error with "Nonfinite derivative-limit coefficient.";
               end if;
               Cache.Plain_Axes (A) := Plain;
               Cache.Dimensionless_Axes (A) :=
                 Time_Scaling
                 and then Dimensionless'Machine_Radix = 2
                 and then
                   (for all Value of Values => Value = 0.0 or else (Value >= 2.0**(-64) and then Value < 2.0**64));
               Cache.Plain_Limit_Axes (A) :=
                 not Time_Scaling
                 and then Dimensionless'Machine_Radix = 2
                 and then
                   (for all Value of Values => Value = 0.0 or else (Value >= 2.0**(-64) and then Value < 2.0**64));
               Cache.Still_Axes (A) :=
                 Plain.X1 = 0.0
                 and then Plain.X2 = 0.0
                 and then Plain.X3 = 0.0
                 and then Plain.X4 = 0.0
                 and then Plain.X5 = 0.0;
               Cache.Tangent_Only_Axes (A) :=
                 Plain.X2 = 0.0 and then Plain.X3 = 0.0 and then Plain.X4 = 0.0 and then Plain.X5 = 0.0;
               Cache.Axes_Filled (A) := True;
            end;
         end if;

         if Cache.Still_Axes (A) then
            --  Every budget of this axis has a zero fixed term and zero coefficients against a nonnegative limit,
            --  so each fits at any scale and neither solve below would change anything.
            null;
         elsif Dimensionless_Inputs and then Cache.Dimensionless_Axes (A) and then Cache.Tangent_Only_Axes (A) then
            declare
               pragma Suppress (Overflow_Check);
               Plain : Mixed_Derivative_Plain_Coefficients renames Cache.Plain_Axes (A);
            begin
               --  The general budgets below with their zero products written as the exact zeros they are.
               Apply_Bounded_Time_Constraint (Plain.Acceleration_Maximum, 0.0, [Plain.X1 * Base_Acceleration_Raw]);
               Apply_Bounded_Time_Constraint (Plain.Jerk_Maximum, 0.0, [0.0, Plain.X1 * Base_Jerk_Raw]);
               Apply_Bounded_Time_Constraint (Plain.Snap_Maximum, 0.0, [0.0, 0.0, Plain.X1 * Base_Snap_Raw]);
               Apply_Bounded_Time_Constraint
                 (Plain.Crackle_Maximum, 0.0, [0.0, 0.0, 0.0, Plain.X1 * Base_Crackle_Raw]);
            end;
         elsif Dimensionless_Inputs and then Cache.Dimensionless_Axes (A) then
            declare
               pragma Suppress (Overflow_Check);
               Plain        : Mixed_Derivative_Plain_Coefficients renames Cache.Plain_Axes (A);
               X1           : constant Dimensionless := Plain.X1;
               X2           : constant Dimensionless := Plain.X2;
               X3           : constant Dimensionless := Plain.X3;
               X4           : constant Dimensionless := Plain.X4;
               X5           : constant Dimensionless := Plain.X5;
               Speed        : constant Dimensionless := Result.Max_Vel / (mm / s);
               Accel        : constant Dimensionless := Base_Acceleration_Raw;
               Jerk_Peak    : constant Dimensionless := Base_Jerk_Raw;
               Snap_Peak    : constant Dimensionless := Base_Snap_Raw;
               Crackle_Peak : constant Dimensionless := Base_Crackle_Raw;
            begin
               --  Keep the same left-to-right multiplication and summation order as the scaled monomials.
               Apply_Bounded_Time_Constraint (Plain.Acceleration_Maximum, X2 * Speed * Speed, [X1 * Accel]);
               Apply_Bounded_Time_Constraint
                 (Plain.Jerk_Maximum, X3 * Speed * Speed * Speed, [3.0 * X2 * Speed * Accel, X1 * Jerk_Peak]);
               Apply_Bounded_Time_Constraint
                 (Plain.Snap_Maximum,
                  X4 * Speed * Speed * Speed * Speed,
                  [6.0 * X3 * Speed * Speed * Accel,
                   4.0 * X2 * Speed * Jerk_Peak + 3.0 * X2 * Accel * Accel,
                   X1 * Snap_Peak]);
               Apply_Bounded_Time_Constraint
                 (Plain.Crackle_Maximum,
                  X5 * Speed * Speed * Speed * Speed * Speed,
                  [10.0 * X4 * Speed * Speed * Speed * Accel,
                   10.0 * X3 * Speed * Speed * Jerk_Peak + 15.0 * X3 * Speed * Accel * Accel,
                   5.0 * X2 * Speed * Snap_Peak + 10.0 * X2 * Accel * Jerk_Peak,
                   X1 * Crackle_Peak]);
            end;
         elsif Plain_Limit_Inputs and then Cache.Plain_Limit_Axes (A) then
            --  Every nonzero factor is in [2**-64, 2**64]. A budget term is a product of at most six of them and a
            --  constant no greater than fifteen, and a budget sums at most four terms, so each nonzero fixed,
            --  linear or quadratic coefficient is a normal number within [2**-384, 2**391]. A positive remainder of
            --  a limit is at least 2**-116. The square of the linear coefficient and four times the product of the
            --  quadratic one with the remainder are then normal and below 2**783, and every quotient taken is of
            --  a smaller normal number by a larger one. Nothing can overflow or underflow, so the budgets are
            --  evaluated in plain arithmetic, by the same operations in the same order as the scaled solve below.
            declare
               pragma Suppress (Overflow_Check);
               Plain        : Mixed_Derivative_Plain_Coefficients renames Cache.Plain_Axes (A);
               X1           : constant Dimensionless := Plain.X1;
               X2           : constant Dimensionless := Plain.X2;
               X3           : constant Dimensionless := Plain.X3;
               X4           : constant Dimensionless := Plain.X4;
               X5           : constant Dimensionless := Plain.X5;
               Speed        : constant Dimensionless := Result.Max_Vel / (mm / s);
               Accel        : constant Dimensionless := Base_Acceleration_Raw;
               Jerk_Peak    : constant Dimensionless := Base_Jerk_Raw;
               Snap_Peak    : constant Dimensionless := Base_Snap_Raw;
               Crackle_Peak : constant Dimensionless := Base_Crackle_Raw;

               function Ratio (Numerator, Denominator : Dimensionless) return Dimensionless
               is (if Numerator = 0.0
                   then 0.0
                   elsif Denominator = 0.0 or else Numerator > Denominator
                   then 1.0
                   else Numerator / Denominator);
               --  As Scaled_Ratio.

               procedure Apply_Linear (Maximum, Fixed, Linear : Dimensionless);
               procedure Apply_Quadratic (Maximum, Fixed, Linear, Quadratic : Dimensionless);
               --  As Apply_Linear_Constraint and Apply_Quadratic_Constraint.

               procedure Apply_Linear (Maximum, Fixed, Linear : Dimensionless) is
               begin
                  if Maximum < Fixed then
                     Result.Valid := False;
                  elsif Linear /= 0.0 then
                     Limit_Scale := Dimensionless'Min (Limit_Scale, Ratio (Maximum - Fixed, Linear));
                  end if;
               end Apply_Linear;

               procedure Apply_Quadratic (Maximum, Fixed, Linear, Quadratic : Dimensionless) is
                  Remainder : constant Dimensionless := Maximum - Fixed;
               begin
                  if Maximum < Fixed then
                     Result.Valid := False;
                  elsif Remainder = 0.0 then
                     if Linear /= 0.0 or else Quadratic /= 0.0 then
                        Limit_Scale := Dimensionless'Min (Limit_Scale, 0.0);
                     end if;
                  elsif Linear + Quadratic <= Remainder then
                     null;
                  elsif Quadratic = 0.0 then
                     Limit_Scale := Dimensionless'Min (Limit_Scale, Ratio (Remainder, Linear));
                  else
                     --  The cancellation-free conjugate root, as in Positive_Quadratic_Scale.
                     Limit_Scale :=
                       Dimensionless'Min
                         (Limit_Scale,
                          Ratio
                            (2.0 * Remainder,
                             Linear + Dimensionless_Math.Sqrt (Linear * Linear + 4.0 * (Quadratic * Remainder))));
                  end if;
               end Apply_Quadratic;
            begin
               Apply_Linear (Plain.Acceleration_Maximum, X2 * Speed * Speed, X1 * Accel);
               Apply_Linear
                 (Plain.Jerk_Maximum, X3 * Speed * Speed * Speed, 3.0 * X2 * Speed * Accel + X1 * Jerk_Peak);
               Apply_Quadratic
                 (Plain.Snap_Maximum,
                  X4 * Speed * Speed * Speed * Speed,
                  6.0 * X3 * Speed * Speed * Accel + 4.0 * X2 * Speed * Jerk_Peak + X1 * Snap_Peak,
                  3.0 * X2 * Accel * Accel);
               Apply_Quadratic
                 (Plain.Crackle_Maximum,
                  X5 * Speed * Speed * Speed * Speed * Speed,
                  10.0 * X4 * Speed * Speed * Speed * Accel + 10.0 * X3 * Speed * Speed * Jerk_Peak
                  + 5.0 * X2 * Speed * Snap_Peak
                  + X1 * Crackle_Peak,
                  15.0 * X3 * Speed * Accel * Accel + 10.0 * X2 * Accel * Jerk_Peak);
            end;
         else
            if not Cache.Axes_Decomposed (A) then
               declare
                  Plain : Mixed_Derivative_Plain_Coefficients renames Cache.Plain_Axes (A);
               begin
                  Cache.Axes (A) :=
                    (X1                   => To_Scaled (Plain.X1),
                     X2                   => To_Scaled (Plain.X2),
                     X3                   => To_Scaled (Plain.X3),
                     X4                   => To_Scaled (Plain.X4),
                     X5                   => To_Scaled (Plain.X5),
                     Acceleration_Maximum => To_Scaled (Plain.Acceleration_Maximum),
                     Jerk_Maximum         => To_Scaled (Plain.Jerk_Maximum),
                     Snap_Maximum         => To_Scaled (Plain.Snap_Maximum),
                     Crackle_Maximum      => To_Scaled (Plain.Crackle_Maximum));
                  Cache.Axes_Decomposed (A) := True;
               end;
            end if;
            if not Scaled_Inputs_Filled then
               --  Decompose the changing inputs only if an axis needs the full exponent range.
               Scaled_Acceleration := To_Scaled (Base_Acceleration_Raw);
               Scaled_Jerk := To_Scaled (Base_Jerk_Raw);
               Scaled_Snap := To_Scaled (Base_Snap_Raw);
               Scaled_Crackle := To_Scaled (Base_Crackle_Raw);
               V := To_Scaled (Dimensionless (Result.Max_Vel / (mm / s)));
               Scaled_Inputs_Filled := True;
            end if;
            declare
               X1                   : Scaled_Nonnegative renames Cache.Axes (A).X1;
               X2                   : Scaled_Nonnegative renames Cache.Axes (A).X2;
               X3                   : Scaled_Nonnegative renames Cache.Axes (A).X3;
               X4                   : Scaled_Nonnegative renames Cache.Axes (A).X4;
               X5                   : Scaled_Nonnegative renames Cache.Axes (A).X5;
               Acceleration_Maximum : Scaled_Nonnegative renames Cache.Axes (A).Acceleration_Maximum;
               Jerk_Maximum         : Scaled_Nonnegative renames Cache.Axes (A).Jerk_Maximum;
               Snap_Maximum         : Scaled_Nonnegative renames Cache.Axes (A).Snap_Maximum;
               Crackle_Maximum      : Scaled_Nonnegative renames Cache.Axes (A).Crackle_Maximum;
            begin
               if Time_Scaling then
                  --  For a fixed velocity change the chain-rule budgets are monotone polynomials in inverse
                  --  time stretch. Unlike uniform scaling, unused configured jerk/snap limits consume no budget.
                  Apply_Time_Constraint
                    (Acceleration_Maximum, Scaled_Product ([X2, V, V]), [Scaled_Product ([X1, Scaled_Acceleration])]);
                  Apply_Time_Constraint
                    (Jerk_Maximum,
                     Scaled_Product ([X3, V, V, V]),
                     [Scaled_Product ([Three, X2, V, Scaled_Acceleration]), Scaled_Product ([X1, Scaled_Jerk])]);
                  Apply_Time_Constraint
                    (Snap_Maximum,
                     Scaled_Product ([X4, V, V, V, V]),
                     [Scaled_Product ([Six, X3, V, V, Scaled_Acceleration]),
                      Scaled_Sum
                        ([Scaled_Product ([Four, X2, V, Scaled_Jerk]),
                          Scaled_Product ([Three, X2, Scaled_Acceleration, Scaled_Acceleration])]),
                      Scaled_Product ([X1, Scaled_Snap])]);
                  Apply_Time_Constraint
                    (Crackle_Maximum,
                     Scaled_Product ([X5, V, V, V, V, V]),
                     [Scaled_Product ([Ten, X4, V, V, V, Scaled_Acceleration]),
                      Scaled_Sum
                        ([Scaled_Product ([Ten, X3, V, V, Scaled_Jerk]),
                          Scaled_Product ([Fifteen, X3, V, Scaled_Acceleration, Scaled_Acceleration])]),
                      Scaled_Sum
                        ([Scaled_Product ([Five, X2, V, Scaled_Snap]),
                          Scaled_Product ([Ten, X2, Scaled_Acceleration, Scaled_Jerk])]),
                      Scaled_Product ([X1, Scaled_Crackle])]);
               else
                  Apply_Linear_Constraint
                    (Acceleration_Maximum, Scaled_Product ([X2, V, V]), Scaled_Product ([X1, Scaled_Acceleration]));
                  Apply_Linear_Constraint
                    (Jerk_Maximum,
                     Scaled_Product ([X3, V, V, V]),
                     Scaled_Sum
                       ([Scaled_Product ([Three, X2, V, Scaled_Acceleration]), Scaled_Product ([X1, Scaled_Jerk])]));
                  Apply_Quadratic_Constraint
                    (Snap_Maximum,
                     Scaled_Product ([X4, V, V, V, V]),
                     Scaled_Sum
                       ([Scaled_Product ([Six, X3, V, V, Scaled_Acceleration]),
                         Scaled_Product ([Four, X2, V, Scaled_Jerk]),
                         Scaled_Product ([X1, Scaled_Snap])]),
                     Scaled_Product ([Three, X2, Scaled_Acceleration, Scaled_Acceleration]));
                  Apply_Quadratic_Constraint
                    (Crackle_Maximum,
                     Scaled_Product ([X5, V, V, V, V, V]),
                     Scaled_Sum
                       ([Scaled_Product ([Ten, X4, V, V, V, Scaled_Acceleration]),
                         Scaled_Product ([Ten, X3, V, V, Scaled_Jerk]),
                         Scaled_Product ([Five, X2, V, Scaled_Snap]),
                         Scaled_Product ([X1, Scaled_Crackle])]),
                     Scaled_Sum
                       ([Scaled_Product ([Fifteen, X3, V, Scaled_Acceleration, Scaled_Acceleration]),
                         Scaled_Product ([Ten, X2, Scaled_Acceleration, Scaled_Jerk])]));
               end if;
            end;
         end if;
         exit when not Result.Valid;
      end loop;
      if not Result.Valid or else Limit_Scale < 0.0 then
         Result.Valid := False;
         return;
      end if;

      Limit_Scale := Safety * Dimensionless'Min (1.0, Limit_Scale);
      Result.Limits :=
        (Acceleration_Max => Limit_Scale * Base.Acceleration_Max,
         Jerk_Max         => Limit_Scale * Base.Jerk_Max,
         Snap_Max         => Limit_Scale * Base.Snap_Max,
         Crackle_Max      => Limit_Scale * Base.Crackle_Max);
   end Solve_Mixed_Derivative_Limits;

   function Mixed_Derivative_Limits
     (Params       : Kinematic_Parameters;
      Bounds       : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel      : Velocity;
      Safety       : Dimensionless := 0.999;
      Scalar_Maxes : Scalar_Derivative_Limits := (others => <>)) return Mixed_Derivative_Limit_Result
   is
      Cache : Mixed_Derivative_Cache;
   begin
      return Mixed_Derivative_Limits (Params, Bounds, Max_Vel, Safety, Scalar_Maxes, Cache);
   end Mixed_Derivative_Limits;

   function Mixed_Derivative_Limits
     (Params       : Kinematic_Parameters;
      Bounds       : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel      : Velocity;
      Safety       : Dimensionless;
      Scalar_Maxes : Scalar_Derivative_Limits;
      Cache        : in out Mixed_Derivative_Cache) return Mixed_Derivative_Limit_Result
   is
      Result : Mixed_Derivative_Limit_Result;
      Scale  : Dimensionless;
   begin
      Solve_Mixed_Derivative_Limits (Params, Bounds, Max_Vel, Safety, Scalar_Maxes, False, Cache, Result, Scale);
      return Result;
   end Mixed_Derivative_Limits;

   function Mixed_Derivative_Time_Scale
     (Params  : Kinematic_Parameters;
      Bounds  : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel : Velocity;
      Peaks   : Scalar_Derivative_Limits) return Dimensionless
   is
      Cache : Mixed_Derivative_Cache;
   begin
      return Mixed_Derivative_Time_Scale (Params, Bounds, Max_Vel, Peaks, Cache);
   end Mixed_Derivative_Time_Scale;

   function Mixed_Derivative_Time_Scale
     (Params  : Kinematic_Parameters;
      Bounds  : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel : Velocity;
      Peaks   : Scalar_Derivative_Limits;
      Cache   : in out Mixed_Derivative_Cache) return Dimensionless
   is
      Result : Mixed_Derivative_Limit_Result;
      Scale  : Dimensionless;
   begin
      Solve_Mixed_Derivative_Limits (Params, Bounds, Max_Vel, 0.999, Peaks, True, Cache, Result, Scale);
      return (if Result.Valid then Scale else 0.0);
   end Mixed_Derivative_Time_Scale;

   function Largest_Nonpositive
     (Lower, Upper                   : Dimensionless;
      Lower_Residual, Upper_Residual : Dimensionless;
      Relative_Tolerance             : Dimensionless := 0.0;
      Guess                          : Dimensionless := -1.0;
      Use_Model                      : Boolean := True) return Dimensionless
   is
      pragma Assertion_Policy (Ignore);
      --  The search's contract, assertions and ghost code are only for its proof. Its postcondition and loop
      --  invariants speak of ghost functions, which are never compiled, and its precondition bounds residuals more
      --  tightly than the planner's are.

      function Search is new Trajectory.Searches.Largest_Nonpositive (Residual);
   begin
      return Search (Lower, Upper, Lower_Residual, Upper_Residual, Relative_Tolerance, Guess, Use_Model);
   end Largest_Nonpositive;

end Prunt.Motion_Planner;
