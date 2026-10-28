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

with Ada.Unchecked_Conversion;

package body Prunt.Motion_Planner is

   pragma Extensions_Allowed (On);

   type Feedrate_Profile_Stage_Index is range 1 .. 15;

   function Derivative_Limits_Are_Valid (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean
   is (Am >= 0.0 * mm / s**2
       and then Am <= Acceleration'Last
       and then Jm >= 0.0 * mm / s**3
       and then Jm <= Jerk'Last
       and then Sm >= 0.0 * mm / s**4
       and then Sm <= Snap'Last
       and then Cm >= 0.0 * mm / s**5
       and then Cm <= Crackle'Last);

   function Profile_Is_Valid
     (Profile : Feedrate_Profile; Distance : Length; Start_Vel, End_Vel : Velocity; Max_Crackle : Crackle)
      return Boolean
   is
      Peak : Velocity;
   begin
      Peak := Fast_Velocity_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel);
      return
        Profile_Is_Valid
          (Profile,
           Distance,
           Start_Vel,
           End_Vel,
           Max_Crackle,
           Peak,
           Fast_Distance_At_Max_Time (Profile.Accel, Max_Crackle, Start_Vel),
           Fast_Distance_At_Max_Time (Profile.Decel, -Max_Crackle, Peak));
   exception
      when Constraint_Error =>
         return False;
   end Profile_Is_Valid;

   function Profile_Is_Valid
     (Profile                        : Feedrate_Profile;
      Distance                       : Length;
      Start_Vel, End_Vel             : Velocity;
      Max_Crackle                    : Crackle;
      Peak                           : Velocity;
      Accel_Distance, Decel_Distance : Length) return Boolean
   is
      Motion_Time : Time;
      Finish      : Velocity;
      Travel      : Length;
   begin
      if not (Distance >= 0.0 * mm
              and then Distance <= Length'Last
              and then Start_Vel >= 0.0 * mm / s
              and then Start_Vel <= Velocity'Last
              and then End_Vel >= 0.0 * mm / s
              and then End_Vel <= Velocity'Last
              and then Max_Crackle >= 0.0 * mm / s**5
              and then Max_Crackle <= Crackle'Last
              and then Profile.Coast >= 0.0 * s
              and then Profile.Coast <= Time'Last)
        or else (for some T of Profile.Accel => not (T >= 0.0 * s and then T <= Time'Last))
        or else (for some T of Profile.Decel => not (T >= 0.0 * s and then T <= Time'Last))
      then
         return False;
      end if;
      Motion_Time := Total_Time (Profile);
      if not (Motion_Time >= 0.0 * s and then Motion_Time <= Time'Last)
        or else (Distance > 0.0 * mm and then Motion_Time = 0.0 * s)
      then
         return False;
      end if;
      Finish := Fast_Velocity_At_Max_Time (Profile.Decel, -Max_Crackle, Peak);
      Travel := Accel_Distance + Peak * Profile.Coast + Decel_Distance;
      --  The analytic ramp solutions and endpoint evaluation round independently. Use relative tolerances so tiny
      --  motion cannot disappear beneath an absolute tolerance. A positive distance must actually be traversed.
      return
        Peak >= 0.0 * mm / s
        and then Peak <= Velocity'Last
        and then Finish >= -Velocity'Last
        and then Finish <= Velocity'Last
        and then Travel >= 0.0 * mm
        and then Travel <= Length'Last
        and then (Distance = 0.0 * mm or else Travel > 0.0 * mm)
        and then abs (Travel - Distance) <= 1.0E-9 * Distance
        and then abs (Finish - End_Vel) <= 1.0E-9 * Velocity'Max (Start_Vel, Velocity'Max (Peak, End_Vel));
   exception
      when Constraint_Error =>
         return False;
   end Profile_Is_Valid;

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

      procedure Apply_Linear_Constraint (Maximum, Fixed, Linear : Scaled_Nonnegative);
      procedure Apply_Quadratic_Constraint (Maximum, Fixed, Linear, Quadratic : Scaled_Nonnegative);
      procedure Apply_Time_Constraint (Maximum, Fixed : Scaled_Nonnegative; Terms : Scaled_Nonnegative_Array);
      procedure Apply_Bounded_Time_Constraint (Maximum, Fixed : Dimensionless; Terms : Dimensionless_Array);

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
            if Terms'Length > 4 or else Dimensionless'Machine_Radix /= 2 or else Dimensionless'Model_Emin > -704 then
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

         --  Each positive monomial bounds the root from above. Establish its magnitude before bisection,
         --  including when the feasible scale is hundreds of decades below one.
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
         --  Only repeated bisection needs this cache. Extreme exponent spreads retain scaled arithmetic.
         Normalize_Coefficients;
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
         Remainder : Dimensionless;
         Lower     : Dimensionless := 0.0;
         Upper     : Dimensionless := Limit_Scale;

         function Fits (Scale : Dimensionless) return Boolean;
         procedure Fallback;

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
      begin
         --  The caller bounds every nonzero input factor to [2**-64, 2**64]. Each coefficient contains
         --  at most five such factors and a constant <= 15, with at most two monomials per coefficient.
         --  Nonzero coefficients are therefore >= 2**-320 and <= 2**325. The fixed term has at most six
         --  factors and is <= 2**384. All products and sums are normal and preserve the scaled evaluation
         --  order. A positive Maximum - Fixed is >= 2**-117 with at most 53 mantissa bits.
         --
         --  Only search with Upper >= 2**-128: sixteen bisections then keep every positive trial >= 2**-144.
         --  Four Horner steps stay in [2**-896, 2**327] when nonzero. Thus no overflow or underflow can alter
         --  a budget comparison. Smaller scales use the original solver, retaining the exact coefficients.
         if Fixed > Maximum then
            Result.Valid := False;
            return;
         elsif Upper = 0.0 then
            return;
         elsif Upper < 2.0**(-128) then
            Fallback;
            return;
         end if;
         Remainder := Maximum - Fixed;
         if Fits (Upper) then
            return;
         elsif Remainder = 0.0 then
            Limit_Scale := 0.0;
            return;
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
      end Apply_Bounded_Time_Constraint;

      Base_Acceleration_Raw                                         : Dimensionless := 1.0E100;
      Base_Jerk_Raw                                                 : Dimensionless := 1.0E100;
      Base_Snap_Raw                                                 : Dimensionless := 1.0E100;
      Base_Crackle_Raw                                              : Dimensionless := 1.0E100;
      Scaled_Acceleration, Scaled_Jerk, Scaled_Snap, Scaled_Crackle : Scaled_Nonnegative;
      V                                                             : Scaled_Nonnegative;
      Dimensionless_Inputs                                          : Boolean;
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

      for A in Axis_Name loop
         if not Cache.Axes_Filled (A) then
            --  Decompose on first use, so a nonfinite bound or limit is still only rejected once a solve reaches
            --  its axis.
            Cache.Axes (A) :=
              (X1                   => To_Scaled (Dimensionless'Max (0.0, Bounds.Velocity (A))),
               X2                   => To_Scaled (Dimensionless'Max (0.0, Bounds.Acceleration (A) / (1.0 / mm))),
               X3                   => To_Scaled (Dimensionless'Max (0.0, Bounds.Jerk (A) / (1.0 / mm**2))),
               X4                   => To_Scaled (Dimensionless'Max (0.0, Bounds.Snap (A) / (1.0 / mm**3))),
               X5                   => To_Scaled (Dimensionless'Max (0.0, Bounds.Crackle (A) / (1.0 / mm**4))),
               Acceleration_Maximum => To_Scaled (Dimensionless (Params.Axial_Acceleration_Maxes (A) / (mm / s**2))),
               Jerk_Maximum         => To_Scaled (Dimensionless (Params.Axial_Jerk_Maxes (A) / (mm / s**3))),
               Snap_Maximum         => To_Scaled (Dimensionless (Params.Axial_Snap_Maxes (A) / (mm / s**4))),
               Crackle_Maximum      => To_Scaled (Dimensionless (Params.Axial_Crackle_Maxes (A) / (mm / s**5))));
            Cache.Axes_Filled (A) := True;
            declare
               Coefficients : Mixed_Derivative_Axis_Coefficients renames Cache.Axes (A);
            begin
               Cache.Dimensionless_Axes (A) :=
                 Time_Scaling
                 and then Dimensionless'Machine_Radix = 2
                 and then
                   (for all Value of
                      Scaled_Nonnegative_Array'
                        [Coefficients.X1,
                         Coefficients.X2,
                         Coefficients.X3,
                         Coefficients.X4,
                         Coefficients.X5,
                         Coefficients.Acceleration_Maximum,
                         Coefficients.Jerk_Maximum,
                         Coefficients.Snap_Maximum,
                         Coefficients.Crackle_Maximum] =>
                      Value.Fraction = 0.0 or else Value.Exponent in -63 .. 64);
            end;
         end if;

         if Dimensionless_Inputs and then Cache.Dimensionless_Axes (A) then
            declare
               pragma Suppress (Overflow_Check);
               X1           : constant Dimensionless := Dimensionless'Max (0.0, Bounds.Velocity (A));
               X2           : constant Dimensionless := Dimensionless'Max (0.0, Bounds.Acceleration (A) / (1.0 / mm));
               X3           : constant Dimensionless := Dimensionless'Max (0.0, Bounds.Jerk (A) / (1.0 / mm**2));
               X4           : constant Dimensionless := Dimensionless'Max (0.0, Bounds.Snap (A) / (1.0 / mm**3));
               X5           : constant Dimensionless := Dimensionless'Max (0.0, Bounds.Crackle (A) / (1.0 / mm**4));
               Speed        : constant Dimensionless := Result.Max_Vel / (mm / s);
               Accel        : constant Dimensionless := Base_Acceleration_Raw;
               Jerk_Peak    : constant Dimensionless := Base_Jerk_Raw;
               Snap_Peak    : constant Dimensionless := Base_Snap_Raw;
               Crackle_Peak : constant Dimensionless := Base_Crackle_Raw;
            begin
               --  Keep the same left-to-right multiplication and summation order as the scaled monomials.
               Apply_Bounded_Time_Constraint
                 (Dimensionless'Max (0.0, Params.Axial_Acceleration_Maxes (A) / (mm / s**2)),
                  X2 * Speed * Speed,
                  [X1 * Accel]);
               Apply_Bounded_Time_Constraint
                 (Dimensionless'Max (0.0, Params.Axial_Jerk_Maxes (A) / (mm / s**3)),
                  X3 * Speed * Speed * Speed,
                  [3.0 * X2 * Speed * Accel, X1 * Jerk_Peak]);
               Apply_Bounded_Time_Constraint
                 (Dimensionless'Max (0.0, Params.Axial_Snap_Maxes (A) / (mm / s**4)),
                  X4 * Speed * Speed * Speed * Speed,
                  [6.0 * X3 * Speed * Speed * Accel,
                   4.0 * X2 * Speed * Jerk_Peak + 3.0 * X2 * Accel * Accel,
                   X1 * Snap_Peak]);
               Apply_Bounded_Time_Constraint
                 (Dimensionless'Max (0.0, Params.Axial_Crackle_Maxes (A) / (mm / s**5)),
                  X5 * Speed * Speed * Speed * Speed * Speed,
                  [10.0 * X4 * Speed * Speed * Speed * Accel,
                   10.0 * X3 * Speed * Speed * Jerk_Peak + 15.0 * X3 * Speed * Accel * Accel,
                   5.0 * X2 * Speed * Snap_Peak + 10.0 * X2 * Accel * Jerk_Peak,
                   X1 * Crackle_Peak]);
            end;
         else
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
      Cache  : Mixed_Derivative_Cache;
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

   function Fast_Distance_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;
      Vs : constant Velocity := Start_Vel;
   begin
      return
        (Vs + Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4) / 2.0)
        * (8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4);
      --  Symbolically equivalent to: return Distance_At_Time (Profile, Total_Time(Profile), Max_Crackle, Start_Vel);
   end Fast_Distance_At_Max_Time;

   function Fast_Velocity_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;
      Vs : constant Velocity := Start_Vel;
   begin
      return Vs + Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4);
      --  Symbolically equivalent to: return Velocity_At_Time (Profile, Total_Time(Profile), Max_Crackle, Start_Vel);
   end Fast_Velocity_At_Max_Time;

   function Total_Time (Times : Feedrate_Profile_Times) return Time is
   begin
      return 8.0 * Times (1) + 4.0 * Times (2) + 2.0 * Times (3) + Times (4);
   end Total_Time;

   function Total_Time (Profile : Feedrate_Profile) return Time is
   begin
      return Total_Time (Profile.Accel) + Profile.Coast + Total_Time (Profile.Decel);
   end Total_Time;

   function Crackle_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Crackle is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;
   begin
      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);

      if T < T1 then
         return Cm;
      elsif T < T1 + T2 then
         return 0.0 * mm / s**5;
      elsif T < 2.0 * T1 + T2 then
         return -Cm;
      elsif T < 2.0 * T1 + T2 + T3 then
         return 0.0 * mm / s**5;
      elsif T < 3.0 * T1 + T2 + T3 then
         return -Cm;
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return 0.0 * mm / s**5;
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Cm;
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return 0.0 * mm / s**5;
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return -Cm;
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return 0.0 * mm / s**5;
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Cm;
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return 0.0 * mm / s**5;
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Cm;
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return 0.0 * mm / s**5;
      else
         return -Cm;
      end if;
   end Crackle_At_Time;

   function Snap_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Snap is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Snap_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Snap;
      --  Return snap at offset DT within a feedrate profile stage.

      function Snap_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Snap is
      begin
         case Stage is
            when 1  =>
               return Cm * DT;

            when 2  =>
               return Snap_At_Stage (T1, 1);

            when 3  =>
               return Snap_At_Stage (T2, 2) - Cm * DT;

            when 4  =>
               return Snap_At_Stage (T1, 3);

            when 5  =>
               return Snap_At_Stage (T3, 4) - Cm * DT;

            when 6  =>
               return Snap_At_Stage (T1, 5);

            when 7  =>
               return Snap_At_Stage (T2, 6) + Cm * DT;

            when 8  =>
               return Snap_At_Stage (T1, 7);

            when 9  =>
               return Snap_At_Stage (T4, 8) - Cm * DT;

            when 10 =>
               return Snap_At_Stage (T1, 9);

            when 11 =>
               return Snap_At_Stage (T2, 10) + Cm * DT;

            when 12 =>
               return Snap_At_Stage (T1, 11);

            when 13 =>
               return Snap_At_Stage (T3, 12) + Cm * DT;

            when 14 =>
               return Snap_At_Stage (T1, 13);

            when 15 =>
               return Snap_At_Stage (T2, 14) - Cm * DT;
         end case;
      end Snap_At_Stage;

   begin
      pragma Assert (T <= Total_Time (Profile));

      if T < T1 then
         return Snap_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Snap_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Snap_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Snap_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Snap_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Snap_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Snap_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Snap_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Snap_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Snap_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Snap_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Snap_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Snap_At_Time;

   function Jerk_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Jerk is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Jerk_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Jerk;
      --  Return jerk at offset DT within a feedrate profile stage.

      function Jerk_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Jerk is
      begin
         case Stage is
            when 1  =>
               return Cm * DT**2 / 2.0;

            when 2  =>
               return Jerk_At_Stage (T1, 1) + Cm * DT * T1;

            when 3  =>
               return Jerk_At_Stage (T2, 2) + Cm * DT * (-DT + 2.0 * T1) / 2.0;

            when 4  =>
               return Jerk_At_Stage (T1, 3);

            when 5  =>
               return Jerk_At_Stage (T3, 4) - Cm * DT**2 / 2.0;

            when 6  =>
               return Jerk_At_Stage (T1, 5) - Cm * DT * T1;

            when 7  =>
               return Jerk_At_Stage (T2, 6) + Cm * DT * (DT - 2.0 * T1) / 2.0;

            when 8  =>
               return Jerk_At_Stage (T1, 7);

            when 9  =>
               return Jerk_At_Stage (T4, 8) - Cm * DT**2 / 2.0;

            when 10 =>
               return Jerk_At_Stage (T1, 9) - Cm * DT * T1;

            when 11 =>
               return Jerk_At_Stage (T2, 10) + Cm * DT * (DT - 2.0 * T1) / 2.0;

            when 12 =>
               return Jerk_At_Stage (T1, 11);

            when 13 =>
               return Jerk_At_Stage (T3, 12) + Cm * DT**2 / 2.0;

            when 14 =>
               return Jerk_At_Stage (T1, 13) + Cm * DT * T1;

            when 15 =>
               return Jerk_At_Stage (T2, 14) + Cm * DT * (-DT + 2.0 * T1) / 2.0;
         end case;
      end Jerk_At_Stage;

   begin
      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);

      if T < T1 then
         return Jerk_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Jerk_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Jerk_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Jerk_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Jerk_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Jerk_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Jerk_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Jerk_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Jerk_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Jerk_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Jerk_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Jerk_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Jerk_At_Time;

   function Acceleration_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Acceleration
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Acceleration_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Acceleration;
      --  Return acceleration at offset DT within a feedrate profile stage.

      function Acceleration_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Acceleration is
      begin
         case Stage is
            when 1  =>
               return Cm * DT**3 / 6.0;

            when 2  =>
               return Acceleration_At_Stage (T1, 1) + Cm * DT * T1 * (DT + T1) / 2.0;

            when 3  =>
               return
                 Acceleration_At_Stage (T2, 2) + Cm * DT * (-DT**2 + 3.0 * DT * T1 + 3.0 * T1 * (T1 + 2.0 * T2)) / 6.0;

            when 4  =>
               return Acceleration_At_Stage (T1, 3) + Cm * DT * T1 * (T1 + T2);

            when 5  =>
               return Acceleration_At_Stage (T3, 4) + Cm * DT * (-DT**2 + 6.0 * T1 * (T1 + T2)) / 6.0;

            when 6  =>
               return Acceleration_At_Stage (T1, 5) + Cm * DT * T1 * (-DT + T1 + 2.0 * T2) / 2.0;

            when 7  =>
               return Acceleration_At_Stage (T2, 6) + Cm * DT * (DT**2 - 3.0 * DT * T1 + 3.0 * T1**2) / 6.0;

            when 8  =>
               return Acceleration_At_Stage (T1, 7);

            when 9  =>
               return Acceleration_At_Stage (T4, 8) - Cm * DT**3 / 6.0;

            when 10 =>
               return Acceleration_At_Stage (T1, 9) + Cm * DT * T1 * (-DT - T1) / 2.0;

            when 11 =>
               return
                 Acceleration_At_Stage (T2, 10) + Cm * DT * (DT**2 - 3.0 * DT * T1 - 3.0 * T1 * (T1 + 2.0 * T2)) / 6.0;

            when 12 =>
               return Acceleration_At_Stage (T1, 11) - Cm * DT * T1 * (T1 + T2);

            when 13 =>
               return Acceleration_At_Stage (T3, 12) + Cm * DT * (DT**2 - 6.0 * T1 * (T1 + T2)) / 6.0;

            when 14 =>
               return Acceleration_At_Stage (T1, 13) + Cm * DT * T1 * (DT - T1 - 2.0 * T2) / 2.0;

            when 15 =>
               return Acceleration_At_Stage (T2, 14) + Cm * DT * (-DT**2 + 3.0 * DT * T1 - 3.0 * T1**2) / 6.0;
         end case;
      end Acceleration_At_Stage;

   begin
      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);

      if T < T1 then
         return Acceleration_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Acceleration_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Acceleration_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Acceleration_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Acceleration_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Acceleration_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Acceleration_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Acceleration_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Acceleration_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Acceleration_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Acceleration_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Acceleration_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Acceleration_At_Time;

   function Velocity_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      function Velocity_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Velocity;
      --  Return velocity at offset DT within a feedrate profile stage.

      function Velocity_At_Stage (DT : Time; Stage : Feedrate_Profile_Stage_Index) return Velocity is
      begin
         case Stage is
            when 1  =>
               return Start_Vel + Cm * DT**4 / 24.0;

            when 2  =>
               return Velocity_At_Stage (T1, 1) + Cm * DT * T1 * (2.0 * DT**2 + 3.0 * DT * T1 + 2.0 * T1**2) / 12.0;

            when 3  =>
               return
                 Velocity_At_Stage (T2, 2)
                 + Cm
                   * DT
                   * (-DT**3 + 4.0 * DT**2 * T1 + 6.0 * DT * T1 * (T1 + 2.0 * T2)
                      + 4.0 * T1 * (T1**2 + 3.0 * T1 * T2 + 3.0 * T2**2))
                   / 24.0;

            when 4  =>
               return
                 Velocity_At_Stage (T1, 3)
                 + Cm * DT * T1 * (DT * (T1 + T2) + 2.0 * T1**2 + 3.0 * T1 * T2 + T2**2) / 2.0;

            when 5  =>
               return
                 Velocity_At_Stage (T3, 4)
                 + Cm
                   * DT
                   * (-DT**3 + 12.0 * DT * T1 * (T1 + T2)
                      + 12.0 * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + 2.0 * T1 * T3 + T2**2 + 2.0 * T2 * T3))
                   / 24.0;

            when 6  =>
               return
                 Velocity_At_Stage (T1, 5)
                 + Cm
                   * DT
                   * T1
                   * (-2.0 * DT**2 + 3.0 * DT * (T1 + 2.0 * T2) + 22.0 * T1**2 + 30.0 * T1 * T2 + 12.0 * T1 * T3
                      + 6.0 * T2**2
                      + 12.0 * T2 * T3)
                   / 12.0;

            when 7  =>
               return
                 Velocity_At_Stage (T2, 6)
                 + Cm
                   * DT
                   * (DT**3 - 4.0 * DT**2 * T1 + 6.0 * DT * T1**2
                      + 4.0 * T1 * (11.0 * T1**2 + 18.0 * T1 * T2 + 6.0 * T1 * T3 + 6.0 * T2**2 + 6.0 * T2 * T3))
                   / 24.0;

            when 8  =>
               return
                 Velocity_At_Stage (T1, 7) + Cm * DT * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T1 * T3 + T2**2 + T2 * T3);

            when 9  =>
               return
                 Velocity_At_Stage (T4, 8)
                 + Cm * DT * (-DT**3 + 24.0 * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T1 * T3 + T2**2 + T2 * T3)) / 24.0;

            when 10 =>
               return
                 Velocity_At_Stage (T1, 9)
                 + Cm
                   * DT
                   * T1
                   * (-2.0 * DT**2 - 3.0 * DT * T1 + 22.0 * T1**2 + 36.0 * T1 * T2 + 12.0 * T1 * T3 + 12.0 * T2**2
                      + 12.0 * T2 * T3)
                   / 12.0;

            when 11 =>
               return
                 Velocity_At_Stage (T2, 10)
                 + Cm
                   * DT
                   * (DT**3 - 4.0 * DT**2 * T1 - 6.0 * DT * T1 * (T1 + 2.0 * T2)
                      + 4.0 * T1 * (11.0 * T1**2 + 15.0 * T1 * T2 + 6.0 * T1 * T3 + 3.0 * T2**2 + 6.0 * T2 * T3))
                   / 24.0;

            when 12 =>
               return
                 Velocity_At_Stage (T1, 11)
                 + Cm
                   * DT
                   * T1
                   * (-DT * (T1 + T2) + 2.0 * T1**2 + 3.0 * T1 * T2 + 2.0 * T1 * T3 + T2**2 + 2.0 * T2 * T3)
                   / 2.0;

            when 13 =>
               return
                 Velocity_At_Stage (T3, 12)
                 + Cm
                   * DT
                   * (DT**3 - 12.0 * DT * T1 * (T1 + T2) + 12.0 * T1 * (2.0 * T1**2 + 3.0 * T1 * T2 + T2**2))
                   / 24.0;

            when 14 =>
               return
                 Velocity_At_Stage (T1, 13)
                 + Cm
                   * DT
                   * T1
                   * (2.0 * DT**2 - 3.0 * DT * (T1 + 2.0 * T2) + 2.0 * T1**2 + 6.0 * T1 * T2 + 6.0 * T2**2)
                   / 12.0;

            when 15 =>
               return
                 Velocity_At_Stage (T2, 14)
                 + Cm * DT * (-DT**3 + 4.0 * DT**2 * T1 - 6.0 * DT * T1**2 + 4.0 * T1**3) / 24.0;
         end case;
      end Velocity_At_Stage;

   begin
      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);

      if T < T1 then
         return Velocity_At_Stage (T, 1);
      elsif T < T1 + T2 then
         return Velocity_At_Stage (T - (T1), 2);
      elsif T < 2.0 * T1 + T2 then
         return Velocity_At_Stage (T - (T1 + T2), 3);
      elsif T < 2.0 * T1 + T2 + T3 then
         return Velocity_At_Stage (T - (2.0 * T1 + T2), 4);
      elsif T < 3.0 * T1 + T2 + T3 then
         return Velocity_At_Stage (T - (2.0 * T1 + T2 + T3), 5);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         return Velocity_At_Stage (T - (3.0 * T1 + T2 + T3), 6);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         return Velocity_At_Stage (T - (3.0 * T1 + 2.0 * T2 + T3), 7);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3), 8);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (4.0 * T1 + 2.0 * T2 + T3 + T4), 9);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (5.0 * T1 + 2.0 * T2 + T3 + T4), 10);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         return Velocity_At_Stage (T - (5.0 * T1 + 3.0 * T2 + T3 + T4), 11);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Velocity_At_Stage (T - (6.0 * T1 + 3.0 * T2 + T3 + T4), 12);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         return Velocity_At_Stage (T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 13);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         return Velocity_At_Stage (T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4), 14);
      else
         return Velocity_At_Stage (T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4), 15);
      end if;
   end Velocity_At_Time;

   function Distance_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;

      type Profile_State is record
         Sn   : Snap;
         Jr   : Jerk;
         Acc  : Acceleration;
         Vel  : Velocity;
         Dist : Length;
      end record;

      Zero_State : constant Profile_State :=
        (Sn => 0.0 * mm / s**4, Jr => 0.0 * mm / s**3, Acc => 0.0 * mm / s**2, Vel => 0.0 * mm / s, Dist => 0.0 * mm);

      function Local_Distance (State : Profile_State; DT : Time; Stage_Crackle : Crackle) return Length;
      --  Return the distance contribution beyond Start_Vel * T from State after DT.

      function Local_Velocity (State : Profile_State; DT : Time; Stage_Crackle : Crackle) return Velocity;
      --  Return the velocity contribution from State after DT.

      procedure Advance (State : in out Profile_State; DT : Time; Stage_Crackle : Crackle);
      --  Advance State through a complete stage of duration DT and constant crackle Stage_Crackle.

      function Stage_Crackle_For (Stage : Feedrate_Profile_Stage_Index) return Crackle;
      --  Return the constant crackle for Stage.

      function Checkpoint_4 return Profile_State;
      --  Return the state at the start of stage 4.  This avoids cancellation from stages 1..3.

      function Checkpoint_8 return Profile_State;
      --  Return the state at the start of stage 8.  This avoids cancellation from stages 1..7.

      function Checkpoint_12 return Profile_State;
      --  Return the state at the start of stage 12.  This avoids cancellation from stages 8..11.

      function Local_Distance (State : Profile_State; DT : Time; Stage_Crackle : Crackle) return Length is
      begin
         return
           DT
           * (State.Vel
              + DT * (State.Acc / 2.0 + DT * (State.Jr / 6.0 + DT * (State.Sn / 24.0 + DT * Stage_Crackle / 120.0))));
      end Local_Distance;

      function Local_Velocity (State : Profile_State; DT : Time; Stage_Crackle : Crackle) return Velocity is
      begin
         return DT * (State.Acc + DT * (State.Jr / 2.0 + DT * (State.Sn / 6.0 + DT * Stage_Crackle / 24.0)));
      end Local_Velocity;

      procedure Advance (State : in out Profile_State; DT : Time; Stage_Crackle : Crackle) is
         Old : constant Profile_State := State;
      begin
         State.Dist := Old.Dist + Local_Distance (Old, DT, Stage_Crackle);
         State.Vel := Old.Vel + Local_Velocity (Old, DT, Stage_Crackle);
         State.Acc := Old.Acc + DT * (Old.Jr + DT * (Old.Sn / 2.0 + DT * Stage_Crackle / 6.0));
         State.Jr := Old.Jr + DT * (Old.Sn + DT * Stage_Crackle / 2.0);
         State.Sn := Old.Sn + DT * Stage_Crackle;
      end Advance;

      function Stage_Crackle_For (Stage : Feedrate_Profile_Stage_Index) return Crackle is
      begin
         case Stage is
            when 1 | 7 | 11 | 13 =>
               return Cm;

            when 3 | 5 | 9 | 15  =>
               return -Cm;

            when others          =>
               return 0.0 * mm / s**5;
         end case;
      end Stage_Crackle_For;

      function Checkpoint_4 return Profile_State is
      begin
         return
           (Sn   => 0.0 * mm / s**4,
            Jr   => Cm * T1 * (T1 + T2),
            Acc  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2) / 2.0,
            Vel  => Cm * T1 * (T1 + T2) * (7.0 * T1**2 + 7.0 * T1 * T2 + 2.0 * T2**2) / 12.0,
            Dist => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2) * (3.0 * T1**2 + 3.0 * T1 * T2 + T2**2) / 24.0);
      end Checkpoint_4;

      function Checkpoint_8 return Profile_State is
      begin
         return
           (Sn   => 0.0 * mm / s**4,
            Jr   => 0.0 * mm / s**3,
            Acc  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3),
            Vel  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3) / 2.0,
            Dist =>
              Cm
              * T1
              * (T1 + T2)
              * (2.0 * T1 + T2 + T3)
              * (27.0 * T1**2 + 27.0 * T1 * T2 + 14.0 * T1 * T3 + 7.0 * T2**2 + 7.0 * T2 * T3 + 2.0 * T3**2)
              / 12.0);
      end Checkpoint_8;

      function Checkpoint_12 return Profile_State is
      begin
         return
           (Sn   => 0.0 * mm / s**4,
            Jr   => -Cm * T1 * (T1 + T2),
            Acc  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + 2.0 * T3) / 2.0,
            Vel  =>
              Cm
              * T1
              * (T1 + T2)
              * (89.0 * T1**2 + 89.0 * T1 * T2 + 60.0 * T1 * T3 + 24.0 * T1 * T4 + 22.0 * T2**2 + 30.0 * T2 * T3
                 + 12.0 * T2 * T4
                 + 6.0 * T3**2
                 + 12.0 * T3 * T4)
              / 12.0,
            Dist =>
              Cm
              * T1
              * (T1 + T2)
              * (390.0 * T1**3 + 585.0 * T1**2 * T2 + 302.0 * T1**2 * T3 + 192.0 * T1**2 * T4 + 293.0 * T1 * T2**2
                 + 302.0 * T1 * T2 * T3
                 + 192.0 * T1 * T2 * T4
                 + 60.0 * T1 * T3**2
                 + 120.0 * T1 * T3 * T4
                 + 24.0 * T1 * T4**2
                 + 49.0 * T2**3
                 + 76.0 * T2**2 * T3
                 + 48.0 * T2**2 * T4
                 + 30.0 * T2 * T3**2
                 + 60.0 * T2 * T3 * T4
                 + 12.0 * T2 * T4**2
                 + 4.0 * T3**3
                 + 12.0 * T3**2 * T4
                 + 12.0 * T3 * T4**2)
              / 24.0);
      end Checkpoint_12;

      Stage : Feedrate_Profile_Stage_Index := 1;
      DT    : Time := 0.0 * s;
      State : Profile_State := Zero_State;

   begin
      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);

      if T < T1 then
         Stage := 1;
         DT := T;
      elsif T < T1 + T2 then
         Stage := 2;
         DT := T - T1;
      elsif T < 2.0 * T1 + T2 then
         Stage := 3;
         DT := T - (T1 + T2);
      elsif T < 2.0 * T1 + T2 + T3 then
         Stage := 4;
         DT := T - (2.0 * T1 + T2);
      elsif T < 3.0 * T1 + T2 + T3 then
         Stage := 5;
         DT := T - (2.0 * T1 + T2 + T3);
      elsif T < 3.0 * T1 + 2.0 * T2 + T3 then
         Stage := 6;
         DT := T - (3.0 * T1 + T2 + T3);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 then
         Stage := 7;
         DT := T - (3.0 * T1 + 2.0 * T2 + T3);
      elsif T < 4.0 * T1 + 2.0 * T2 + T3 + T4 then
         Stage := 8;
         DT := T - (4.0 * T1 + 2.0 * T2 + T3);
      elsif T < 5.0 * T1 + 2.0 * T2 + T3 + T4 then
         Stage := 9;
         DT := T - (4.0 * T1 + 2.0 * T2 + T3 + T4);
      elsif T < 5.0 * T1 + 3.0 * T2 + T3 + T4 then
         Stage := 10;
         DT := T - (5.0 * T1 + 2.0 * T2 + T3 + T4);
      elsif T < 6.0 * T1 + 3.0 * T2 + T3 + T4 then
         Stage := 11;
         DT := T - (5.0 * T1 + 3.0 * T2 + T3 + T4);
      elsif T < 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         Stage := 12;
         DT := T - (6.0 * T1 + 3.0 * T2 + T3 + T4);
      elsif T < 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4 then
         Stage := 13;
         DT := T - (6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4);
      elsif T < 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4 then
         Stage := 14;
         DT := T - (7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4);
      else
         Stage := 15;
         DT := T - (7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4);
      end if;

      case Stage is
         when 1 .. 3   =>
            State := Zero_State;
            if Stage >= 2 then
               Advance (State, T1, Cm);
            end if;
            if Stage >= 3 then
               Advance (State, T2, 0.0 * mm / s**5);
            end if;

         when 4 .. 7   =>
            State := Checkpoint_4;
            if Stage >= 5 then
               Advance (State, T3, 0.0 * mm / s**5);
            end if;
            if Stage >= 6 then
               Advance (State, T1, -Cm);
            end if;
            if Stage >= 7 then
               Advance (State, T2, 0.0 * mm / s**5);
            end if;

         when 8 .. 11  =>
            State := Checkpoint_8;
            if Stage >= 9 then
               Advance (State, T4, 0.0 * mm / s**5);
            end if;
            if Stage >= 10 then
               Advance (State, T1, -Cm);
            end if;
            if Stage >= 11 then
               Advance (State, T2, 0.0 * mm / s**5);
            end if;

         when 12 .. 15 =>
            State := Checkpoint_12;
            if Stage >= 13 then
               Advance (State, T3, 0.0 * mm / s**5);
            end if;
            if Stage >= 14 then
               Advance (State, T1, Cm);
            end if;
            if Stage = 15 then
               Advance (State, T2, 0.0 * mm / s**5);
            end if;
      end case;

      return Start_Vel * T + State.Dist + Local_Distance (State, DT, Stage_Crackle_For (Stage));
   end Distance_At_Time;

   function Crackle_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Crackle is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         return Crackle_At_Time (Profile.Accel, T, Max_Crackle);
      elsif T < Total_Time (Profile.Accel) + Profile.Coast then
         return 0.0 * mm / s**5;
      else
         declare
            Decel_T : constant Time :=
              Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
         begin
            return Crackle_At_Time (Profile.Decel, Decel_T, -Max_Crackle);
         end;
      end if;
   end Crackle_At_Time;

   function Snap_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Snap is
   begin
      pragma Assert (T <= Total_Time (Profile.Accel) + Profile.Coast + Total_Time (Profile.Decel));
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         return Snap_At_Time (Profile.Accel, T, Max_Crackle);
      elsif T < Total_Time (Profile.Accel) + Profile.Coast then
         return 0.0 * mm / s**4;
      else
         declare
            Decel_T : constant Time :=
              Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
         begin
            return Snap_At_Time (Profile.Decel, Decel_T, -Max_Crackle);
         end;
      end if;
   end Snap_At_Time;

   function Jerk_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Jerk is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         return Jerk_At_Time (Profile.Accel, T, Max_Crackle);
      elsif T < Total_Time (Profile.Accel) + Profile.Coast then
         return 0.0 * mm / s**3;
      else
         declare
            Decel_T : constant Time :=
              Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
         begin
            return Jerk_At_Time (Profile.Decel, Decel_T, -Max_Crackle);
         end;
      end if;
   end Jerk_At_Time;

   function Acceleration_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Acceleration is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         return Acceleration_At_Time (Profile.Accel, T, Max_Crackle);
      elsif T < Total_Time (Profile.Accel) + Profile.Coast then
         return 0.0 * mm / s**2;
      else
         declare
            Decel_T : constant Time :=
              Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
         begin
            return Acceleration_At_Time (Profile.Decel, Decel_T, -Max_Crackle);
         end;
      end if;
   end Acceleration_At_Time;

   function Velocity_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         return Velocity_At_Time (Profile.Accel, T, Max_Crackle, Start_Vel);
      end if;

      declare
         --  Only the later phases start from the end of acceleration.
         Mid_Vel : constant Velocity :=
           Velocity_At_Time (Profile.Accel, Total_Time (Profile.Accel), Max_Crackle, Start_Vel);
      begin
         if T < Total_Time (Profile.Accel) + Profile.Coast then
            return Mid_Vel;
         else
            declare
               Decel_T : constant Time :=
                 Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
            begin
               return Velocity_At_Time (Profile.Decel, Decel_T, -Max_Crackle, Mid_Vel);
            end;
         end if;
      end;
   end Velocity_At_Time;

   function Distance_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length is
   begin
      pragma Assert (T <= Total_Time (Profile));

      if T <= Total_Time (Profile.Accel) then
         return Distance_At_Time (Profile.Accel, T, Max_Crackle, Start_Vel);
      end if;

      declare
         --  Only the later phases start from the end of acceleration.
         Mid_Vel    : constant Velocity :=
           Velocity_At_Time (Profile.Accel, Total_Time (Profile.Accel), Max_Crackle, Start_Vel);
         Accel_Dist : constant Length :=
           Distance_At_Time (Profile.Accel, Total_Time (Profile.Accel), Max_Crackle, Start_Vel);
      begin
         if T < Total_Time (Profile.Accel) + Profile.Coast then
            return Accel_Dist + Mid_Vel * (T - Total_Time (Profile.Accel));
         else
            declare
               Mid_Dist : constant Length := Mid_Vel * Profile.Coast;
               Decel_T  : constant Time :=
                 Time'Min (T - (Total_Time (Profile.Accel) + Profile.Coast), Total_Time (Profile.Decel));
            begin
               return Accel_Dist + Mid_Dist + Distance_At_Time (Profile.Decel, Decel_T, -Max_Crackle, Mid_Vel);
            end;
         end if;
      end;
   end Distance_At_Time;

   function Optimal_Profile_For_Distance_Internal
     (Start_Vel        : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Internal_Profile_Result
   is
      D      : constant Length := Distance;
      Vs     : constant Velocity := Start_Vel;
      Am     : constant Acceleration := Acceleration_Max;
      Jm     : constant Jerk := Jerk_Max;
      Sm     : constant Snap := Snap_Max;
      Cm     : constant Crackle := Crackle_Max;
      Cases  : array (Feedrate_Profile_Times_Index) of Feedrate_Profile_Times;
      Region : Constraint_Region;
      T2, T3 : Time;

      function Solve_Distance_At_Time
        (Profile : Feedrate_Profile_Times; Variable : Feedrate_Profile_Times_Index) return Internal_Profile_Result;
      --  Solve one profile time variable so the profile covers the requested distance.

      function Solve_Distance_At_Time
        (Profile : Feedrate_Profile_Times; Variable : Feedrate_Profile_Times_Index) return Internal_Profile_Result
      is
         Result : Feedrate_Profile_Times := Profile;

         Lower : Time := 0.0 * s;
         Upper : Time := 86_400.0 * s;
         --  A maximum of 24 hours should be more than enough.

         type Casted_Time is mod 2**64;
         function Cast_Time is new Ada.Unchecked_Conversion (Time, Casted_Time);
         function Cast_Time is new Ada.Unchecked_Conversion (Casted_Time, Time);
      begin
         --  This probably breaks when not using IEEE 754 floats or on other weird systems, so try to check for
         --  that.
         pragma Assert (Time'Size = 64);
         pragma Assert (Casted_Time'Size = 64);
         pragma Assert (Cast_Time (86_400.0 * s) = 4_680_673_776_000_565_248);
         pragma Assert (Cast_Time (0.123_45 * s) = 4_593_559_930_647_147_132);

         loop
            Result (Variable) := Cast_Time (Cast_Time (Lower) + (Cast_Time (Upper) - Cast_Time (Lower)) / 2);
            pragma
              Annotate (Xcov, Exempt_On, "Binary search convergence depends on floating-point rounding direction.");
            exit when Lower = Result (Variable) or else Upper = Result (Variable);
            pragma Annotate (Xcov, Exempt_Off);
            if Fast_Distance_At_Max_Time (Result, Cm, Vs) <= D then
               Lower := Result (Variable);
            else
               Upper := Result (Variable);
            end if;
         end loop;

         return (Result, Region, Integer (Variable));
      end Solve_Distance_At_Time;

   begin
      if not (D >= 0.0 * mm and then D <= Length'Last and then Vs >= 0.0 * mm / s and then Vs <= Velocity'Last)
        or else not Derivative_Limits_Are_Valid (Am, Jm, Sm, Cm)
      then
         raise Constraint_Error with "Invalid distance-profile inputs.";
      elsif Am = 0.0 * mm / s**2 or else Jm = 0.0 * mm / s**3 or else Sm = 0.0 * mm / s**4 or else Cm = 0.0 * mm / s**5
      then
         if D = 0.0 * mm then
            return ([others => 0.0 * s], Region_5, 1);
         elsif Vs > 0.0 * mm / s then
            declare
               Coast_Time : constant Time := D / Vs;
            begin
               if not (Coast_Time > 0.0 * s and then Coast_Time <= Time'Last) then
                  raise Constraint_Error with "Unrepresentable constant-speed duration.";
               end if;
               return ([0.0 * s, 0.0 * s, 0.0 * s, Coast_Time], Region_5, 4);
            end;
         else
            raise Constraint_Error with "Positive distance from rest requires positive derivative limits.";
         end if;
      end if;
      --  Each region test is algebraically equivalent to a phase time being nonnegative. Test the computed phase time
      --  itself, since a rearranged test rounds differently and can select a region whose phase time is negative.
      T2 := Jm / Sm - Sm / Cm;
      if T2 > 0.0 * s then
         --  Sm ** 2 < Jm * Cm
         T3 := Am / Jm - Jm / Sm - Sm / Cm;
         if T3 >= 0.0 * s then
            --  Am >= Jm * (Jm / Sm + Sm / Cm)
            Region := Region_1;
            Cases :=
              [
               --  Reachable: Sm, Jm, Am (Test Case D1.4)
               4 => [Sm / Cm, T2, T3, 0.0 * s],
               --  Reachable: Sm, Jm (Test Case D1.3)
               3 => [Sm / Cm, T2, 0.0 * s, 0.0 * s],
               --  Reachable: Sm (Test Case D1.2)
               2 => [Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None (Test Case D1.1)
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]];
         else
            T2 := Dimensionless_Math.Sqrt (0.25 * Sm**2 / Cm**2 + Am / Sm) - 1.5 * Sm / Cm;
            if T2 >= 0.0 * s then
               --  Am >= 2.0 * Sm ** 3 / Cm ** 2
               Region := Region_2;
               Cases :=
                 [
                  --  Reachable: Sm, Am (Test Case D2.4)
                  4 => [Sm / Cm, T2, 0.0 * s, 0.0 * s],
                  --  Impossible case.
                  3 => [Sm / Cm, T2, 0.0 * s, 0.0 * s],
                  --  Reachable: Sm (Test Case D2.2)
                  2 => [Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s],
                  --  Reachable: None (Test Case D2.1)
                  1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]];
            else
               Region := Region_3;
               Cases :=
                 [
                  --  Reachable: Am (Test Case D3.4)
                  4 => [(0.5 * Am / Cm)**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
                  --  Impossible case.
                  3 => [(0.5 * Am / Cm)**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
                  --  Impossible case.
                  2 => [(0.5 * Am / Cm)**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
                  --  Reachable: None (Test Case D3.1)
                  1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]];
            end if;
         end if;
      else
         T3 := Am / Jm - 2.0 * Dimensionless_Math.Sqrt (Jm / Cm);
         if T3 > 0.0 * s then
            --  Am > 2.0 * Jm * Dimensionless_Math.Sqrt (Jm / Cm)
            Region := Region_4;
            Cases :=
              [
               --  Reachable: Jm, Am (Test Case D4.4)
               4 => [Dimensionless_Math.Sqrt (Jm / Cm), 0.0 * s, T3, 0.0 * s],
               --  Reachable: Jm (Test Case D4.3)
               3 => [Dimensionless_Math.Sqrt (Jm / Cm), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               2 => [Dimensionless_Math.Sqrt (Jm / Cm), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None (Test Case D4.1)
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]];
         else
            Region := Region_5;
            Cases :=
              [
               --  Reachable: Am (Test Case D5.4)
               4 => [(Am / (2.0 * Cm))**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               3 => [(Am / (2.0 * Cm))**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Impossible case.
               2 => [(Am / (2.0 * Cm))**(1 / 3), 0.0 * s, 0.0 * s, 0.0 * s],
               --  Reachable: None (Test Case D5.1)
               1 => [0.0 * s, 0.0 * s, 0.0 * s, 0.0 * s]];
         end if;
      end if;

      for I in reverse Cases'Range loop
         if I = Cases'First or else D > Fast_Distance_At_Max_Time (Cases (I), Cm, Vs) then
            return Solve_Distance_At_Time (Cases (I), I);
            --  There are simple analytical solutions for a lot of these, but this is already fast so there is no
            --  reason to optimise it.

         end if;
      end loop;

      pragma Annotate (Xcov, Exempt_On, "Unreachable.");
      raise Program_Error;
      pragma Annotate (Xcov, Exempt_Off);
   end Optimal_Profile_For_Distance_Internal;

   function Optimal_Profile_For_Distance
     (Start_Vel        : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Feedrate_Profile_Times is
   begin
      return
        Optimal_Profile_For_Distance_Internal (Start_Vel, Distance, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
          .Profile;
   end Optimal_Profile_For_Distance;

   function Optimal_Profile_For_Delta_V_Internal
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
      return Internal_Profile_Result
   is
      Vd     : constant Velocity := abs Delta_V;
      Am     : constant Acceleration := Acceleration_Max;
      Jm     : constant Jerk := Jerk_Max;
      Sm     : constant Snap := Snap_Max;
      Cm     : constant Crackle := Crackle_Max;
      Region : Constraint_Region;
      T2, T3 : Time;
      T4     : Time;

      function Solve_Velocity_At_Time
        (Profile : Feedrate_Profile_Times; Variable : Feedrate_Profile_Times_Index; Target : Velocity)
         return Internal_Profile_Result;
      --  Solve one profile time variable so the profile reaches Target velocity.

      function Solve_Velocity_At_Time
        (Profile : Feedrate_Profile_Times; Variable : Feedrate_Profile_Times_Index; Target : Velocity)
         return Internal_Profile_Result
      is
         Result : Feedrate_Profile_Times := Profile;

         Lower : Time := 0.0 * s;
         Upper : Time := 86_400.0 * s;
         --  A maximum of 24 hours should be more than enough.

         type Casted_Time is mod 2**64;
         function Cast_Time is new Ada.Unchecked_Conversion (Time, Casted_Time);
         function Cast_Time is new Ada.Unchecked_Conversion (Casted_Time, Time);
      begin
         --  This probably breaks when not using IEEE 754 floats or on other weird systems, so try to check for
         --  that.
         pragma Assert (Time'Size = 64);
         pragma Assert (Casted_Time'Size = 64);
         pragma Assert (Cast_Time (86_400.0 * s) = 4_680_673_776_000_565_248);
         pragma Assert (Cast_Time (0.123_45 * s) = 4_593_559_930_647_147_132);

         loop
            Result (Variable) := Cast_Time (Cast_Time (Lower) + (Cast_Time (Upper) - Cast_Time (Lower)) / 2);
            pragma
              Annotate (Xcov, Exempt_On, "Binary search convergence depends on floating-point rounding direction.");
            exit when Lower = Result (Variable) or else Upper = Result (Variable);
            pragma Annotate (Xcov, Exempt_Off);
            if Fast_Velocity_At_Max_Time (Result, Cm, 0.0 * mm / s) <= Target then
               Lower := Result (Variable);
            else
               Upper := Result (Variable);
            end if;
         end loop;

         return (Result, Region, Integer (Variable));
      end Solve_Velocity_At_Time;
   begin
      if not (Vd <= Velocity'Last) or else not Derivative_Limits_Are_Valid (Am, Jm, Sm, Cm) then
         raise Constraint_Error with "Invalid velocity-profile inputs.";
      elsif Am = 0.0 * mm / s**2 or else Jm = 0.0 * mm / s**3 or else Sm = 0.0 * mm / s**4 or else Cm = 0.0 * mm / s**5
      then
         if Vd = 0.0 * mm / s then
            return ([others => 0.0 * s], Region_5, 1);
         end if;
         raise Constraint_Error with "A velocity change requires positive derivative limits.";
      end if;
      --  This function is called a lot more than Optimal_Profile_For_Distance, so we use simple analytical solutions
      --  where they exist. In the one case where we resort to Solve_Velocity_At_Time, the analytical solution that
      --  Mathematica outputs involves a Cm**18, which is far outside the range of Dimensioned_Float for reasonable
      --  values of Cm.
      --
      --  For reference:
      --  ToRadicals[
      --    Solve[
      --      With[
      --        {T1 = Sm/Cm, T3 = 0, T4 = 0},
      --        v == Cm*T1*(T1 + T2)*(2*T1 + T2 + T3)*(4*T1 + 2*T2 + T3 + T4)
      --      ],
      --      T2,
      --      NonNegativeReals
      --    ]
      --  ]
      --
      --  As in Optimal_Profile_For_Distance_Internal, each case is selected by testing the sign of the computed phase
      --  time it would use, with the equivalent algebraic condition noted beside each test.
      T2 := Jm / Sm - Sm / Cm;
      if T2 > 0.0 * s then
         --  Sm ** 2 < Jm * Cm
         T3 := Am / Jm - Jm / Sm - Sm / Cm;
         if T3 >= 0.0 * s then
            --  Am >= Jm * (Jm / Sm + Sm / Cm)
            Region := Region_1;
            T4 := Vd / Am - Am / Jm - Jm / Sm - Sm / Cm;
            if T4 > 0.0 * s then
               --  Vd > Am * (Am / Jm + Jm / Sm + Sm / Cm)
               --  Reachable: Sm, Jm, Am (Test Case V1.4)
               return ([Sm / Cm, T2, T3, T4], Region, 4);
            end if;
            T3 := 0.5 * Dimensionless_Math.Sqrt ((Jm / Sm + Sm / Cm)**2 + 4.0 * Vd / Jm) - 1.5 * (Jm / Sm + Sm / Cm);
            if T3 > 0.0 * s then
               --  Vd > 2.0 * Jm * (Jm / Sm + Sm / Cm) ** 2
               --  Reachable: Sm, Jm (Test Case V1.3)
               return ([Sm / Cm, T2, T3, 0.0 * s], Region, 3);
            elsif Vd > 8.0 * Sm**4 / Cm**3 then
               --  Reachable: Sm (Test Case V1.2)
               return Solve_Velocity_At_Time ([Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s], 2, Vd);
            else
               --  Reachable: None (Test Case V1.1)
               return
                 ([Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)), 0.0 * s, 0.0 * s, 0.0 * s],
                  Region,
                  1);
            end if;
         end if;
         T2 := Dimensionless_Math.Sqrt (0.25 * Sm**2 / Cm**2 + Am / Sm) - 1.5 * Sm / Cm;
         if T2 >= 0.0 * s then
            --  Am >= 2.0 * Sm ** 3 / Cm ** 2
            Region := Region_2;
            T4 := Vd / Am - Sm / Cm - 2.0 * Dimensionless_Math.Sqrt (0.25 * Sm**2 / Cm**2 + Am / Sm);
            if T4 > 0.0 * s then
               --  Vd > Am * (2.0 * Dimensionless_Math.Sqrt (0.25 * Sm ** 2 / Cm ** 2 + Am / Sm) + Sm / Cm)
               --  Reachable: Sm, Am (Test Case V2.4)
               return ([Sm / Cm, T2, 0.0 * s, T4], Region, 4);
            elsif Vd > 8.0 * Sm**4 / Cm**3 then
               --  Reachable: Sm (Test Case V2.2)
               return Solve_Velocity_At_Time ([Sm / Cm, 0.0 * s, 0.0 * s, 0.0 * s], 2, Vd);
            else
               --  Reachable: None (Test Case V2.1)
               return
                 ([Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)), 0.0 * s, 0.0 * s, 0.0 * s],
                  Region,
                  1);
            end if;
         else
            Region := Region_3;
            T4 := Vd / Am - 4.0 * (0.5 * Am / Cm)**(1 / 3);
            if T4 > 0.0 * s then
               --  Vd > 8.0 * Cm * (0.5 * Am / Cm) ** (4 / 3)
               --  Reachable: Am (Test Case V3.4)
               return ([(0.5 * Am / Cm)**(1 / 3), 0.0 * s, 0.0 * s, T4], Region, 4);
            else
               --  Reachable: None (Test Case V3.1)
               return
                 ([Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)), 0.0 * s, 0.0 * s, 0.0 * s],
                  Region,
                  1);
            end if;
         end if;
      else
         T3 := Am / Jm - 2.0 * Dimensionless_Math.Sqrt (Jm / Cm);
         if T3 > 0.0 * s then
            --  Am > 2.0 * Jm * Dimensionless_Math.Sqrt (Jm / Cm)
            Region := Region_4;
            T4 := Vd / Am - Am / Jm - 2.0 * Dimensionless_Math.Sqrt (Jm / Cm);
            if T4 > 0.0 * s then
               --  Vd > Am * (Am / Jm + 2.0 * Dimensionless_Math.Sqrt (Jm / Cm))
               --  Reachable: Jm, Am (Test Case V4.4)
               return ([Dimensionless_Math.Sqrt (Jm / Cm), 0.0 * s, T3, T4], Region, 4);
            end if;
            T3 := Dimensionless_Math.Sqrt (Jm / Cm + Vd / Jm) - 3.0 * Dimensionless_Math.Sqrt (Jm / Cm);
            if T3 > 0.0 * s then
               --  Vd > 8.0 * Jm ** 2 / Cm
               --  Reachable: Jm (Test Case V4.3)
               return ([Dimensionless_Math.Sqrt (Jm / Cm), 0.0 * s, T3, 0.0 * s], Region, 3);
            else
               --  Reachable: None (Test Case V4.1)
               return
                 ([Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)), 0.0 * s, 0.0 * s, 0.0 * s],
                  Region,
                  1);
            end if;
         else
            Region := Region_5;
            T4 := Vd / Am - 4.0 * (0.5 * Am / Cm)**(1 / 3);
            if T4 > 0.0 * s then
               --  Vd > 8.0 * Cm * (0.5 * Am / Cm) ** (4 / 3)
               --  Reachable: Am (Test Case V5.4)
               return ([(0.5 * Am / Cm)**(1 / 3), 0.0 * s, 0.0 * s, T4], Region, 4);
            else
               --  Reachable: None (Test Case V5.1)
               return
                 ([Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)), 0.0 * s, 0.0 * s, 0.0 * s],
                  Region,
                  1);
            end if;
         end if;
      end if;
   end Optimal_Profile_For_Delta_V_Internal;

   function Optimal_Profile_For_Delta_V
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
      return Feedrate_Profile_Times is
   begin
      return Optimal_Profile_For_Delta_V_Internal (Delta_V, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max).Profile;
   end Optimal_Profile_For_Delta_V;

   function Optimal_Full_Profile
     (Start_Vel        : Velocity;
      Max_Vel          : Velocity;
      End_Vel          : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Feedrate_Profile
   is
      Profile : Feedrate_Profile;
   begin
      if not (Distance >= 0.0 * mm
              and then Distance <= Length'Last
              and then Start_Vel >= 0.0 * mm / s
              and then Start_Vel <= Velocity'Last
              and then End_Vel >= 0.0 * mm / s
              and then End_Vel <= Velocity'Last
              and then Max_Vel >= 0.0 * mm / s
              and then Max_Vel <= Velocity'Last)
        or else not Derivative_Limits_Are_Valid (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
      then
         raise Constraint_Error with "Invalid full-profile inputs.";
      end if;
      if Max_Vel < Start_Vel then
         raise Constraint_Error with "Max_Vel can not be smaller than Start_Vel.";
      end if;

      if Max_Vel < End_Vel then
         raise Constraint_Error with "Max_Vel can not be smaller than End_Vel.";
      end if;

      if Distance = 0.0 * mm then
         if Start_Vel /= End_Vel then
            raise Constraint_Error with "A zero-distance profile cannot change velocity.";
         end if;
         return (Accel => [others => 0.0 * s], Coast => 0.0 * s, Decel => [others => 0.0 * s]);
      end if;

      if Acceleration_Max = 0.0 * mm / s**2
        or else Jerk_Max = 0.0 * mm / s**3
        or else Snap_Max = 0.0 * mm / s**4
        or else Crackle_Max = 0.0 * mm / s**5
      then
         if Start_Vel /= End_Vel or else Start_Vel = 0.0 * mm / s then
            raise Constraint_Error with "This motion requires positive derivative limits.";
         end if;
         Profile := (Accel => [others => 0.0 * s], Coast => Distance / Start_Vel, Decel => [others => 0.0 * s]);
         if not Profile_Is_Valid (Profile, Distance, Start_Vel, End_Vel, Crackle_Max) then
            raise Constraint_Error with "Unrepresentable constant-speed profile.";
         end if;
         return Profile;
      end if;

      if Max_Vel = 0.0 * mm / s then
         raise Constraint_Error with "Positive distance requires positive velocity.";
      end if;

      declare
         Profile : constant Feedrate_Profile_Times :=
           Optimal_Profile_For_Delta_V (Start_Vel - End_Vel, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);

         Profile_Distance : constant Length :=
           Fast_Distance_At_Max_Time
             (Profile,
              (if Start_Vel < End_Vel then Crackle_Max / (mm / s**5) else -Crackle_Max / (mm / s**5))
              * (mm / s**5), --  Temporarily drop the dimension otherwise gnatcov breaks it.
              Start_Vel);
      begin
         if Distance < Profile_Distance then
            raise Constraint_Error with "End_Vel is not reachable under given constraints.";
         end if;
      end;

      Profile.Accel :=
        Optimal_Profile_For_Delta_V (Start_Vel - Max_Vel, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
      Profile.Decel :=
        (if Start_Vel = End_Vel
         then Profile.Accel
         else Optimal_Profile_For_Delta_V (End_Vel - Max_Vel, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max));

      declare
         Accel_Distance : Length := Fast_Distance_At_Max_Time (Profile.Accel, Crackle_Max, Start_Vel);
         Decel_Distance : Length := Fast_Distance_At_Max_Time (Profile.Decel, -Crackle_Max, Max_Vel);
      begin
         if Accel_Distance + Decel_Distance <= Distance then
            Profile.Coast := (Distance - Accel_Distance - Decel_Distance) / Max_Vel;
         else
            Profile.Coast := 0.0 * s;
            declare
               type Casted_Vel is mod 2**64;
               function Cast_Vel is new Ada.Unchecked_Conversion (Velocity, Casted_Vel);
               function Cast_Vel is new Ada.Unchecked_Conversion (Casted_Vel, Velocity);
               Upper : Velocity := Max_Vel;
               Lower : Velocity := Velocity'Max (Start_Vel, End_Vel);
               Mid   : Velocity;
            begin
               --  This probably breaks when not using IEEE 754 floats or on other weird systems, so try to check
               --  for that.
               pragma Assert (Velocity'Size = 64);
               pragma Assert (Casted_Vel'Size = 64);
               pragma Assert (Cast_Vel (86_400.0 * mm / s) = 4_680_673_776_000_565_248);
               pragma Assert (Cast_Vel (0.123_45 * mm / s) = 4_593_559_930_647_147_132);

               loop
                  Mid := Cast_Vel (Cast_Vel (Lower) + (Cast_Vel (Upper) - Cast_Vel (Lower)) / 2);
                  pragma
                    Annotate
                      (Xcov, Exempt_On, "Binary search convergence depends on floating-point rounding direction.");
                  exit when Lower = Mid or else Upper = Mid;
                  pragma Annotate (Xcov, Exempt_Off);

                  Profile.Accel :=
                    Optimal_Profile_For_Delta_V (Start_Vel - Mid, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
                  Profile.Decel :=
                    (if Start_Vel = End_Vel
                     then Profile.Accel
                     else
                       Optimal_Profile_For_Delta_V (End_Vel - Mid, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max));

                  Accel_Distance := Fast_Distance_At_Max_Time (Profile.Accel, Crackle_Max, Start_Vel);
                  Decel_Distance := Fast_Distance_At_Max_Time (Profile.Decel, -Crackle_Max, Mid);

                  if Accel_Distance + Decel_Distance <= Distance then
                     Lower := Mid;
                  else
                     Upper := Mid;
                  end if;
               end loop;

               Profile.Accel :=
                 Optimal_Profile_For_Delta_V (Start_Vel - Lower, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
               Profile.Decel :=
                 (if Start_Vel = End_Vel
                  then Profile.Accel
                  else
                    Optimal_Profile_For_Delta_V (End_Vel - Lower, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max));

               Accel_Distance := Fast_Distance_At_Max_Time (Profile.Accel, Crackle_Max, Start_Vel);
               Decel_Distance := Fast_Distance_At_Max_Time (Profile.Decel, -Crackle_Max, Lower);

               if Lower > 0.0 * mm / s and then Accel_Distance + Decel_Distance < Distance then
                  Profile.Coast := (Distance - Accel_Distance - Decel_Distance) / Lower;
               end if;
            end;
         end if;
      end;

      if not Profile_Is_Valid (Profile, Distance, Start_Vel, End_Vel, Crackle_Max) then
         raise Constraint_Error with "The generated profile cannot represent the requested motion.";
      end if;
      return Profile;
   end Optimal_Full_Profile;

end Prunt.Motion_Planner;
