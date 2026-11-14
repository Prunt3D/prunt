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

   function Fast_Distance_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := Max_Crackle;
      Vs : constant Velocity := Start_Vel;

      pragma Suppress (Overflow_Check);
      --  Every operation below reaches the result through sums and products only, so one which overflows, or meets
      --  a NaN, leaves the result an infinity or a NaN. Testing the result once therefore raises for exactly the
      --  inputs for which a check on each operation would. Every ramp certificate evaluates this.
      Result : constant Length :=
        (Vs + Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4) / 2.0)
        * (8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4);
      --  Symbolically equivalent to: Distance_At_Time (Profile, Total_Time(Profile), Max_Crackle, Start_Vel);
   begin
      if not (abs Result <= Length'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result;
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

      pragma Suppress (Overflow_Check);
      --  As in Fast_Distance_At_Max_Time.
      Result : constant Velocity := Vs + Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4);
      --  Symbolically equivalent to: Velocity_At_Time (Profile, Total_Time(Profile), Max_Crackle, Start_Vel);
   begin
      if not (abs Result <= Velocity'Last) then
         raise Constraint_Error with "Unrepresentable ramp velocity.";
      end if;
      return Result;
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

   ------------------------------------------------------------------------------------------------------------------
   --  Distance along a ramp
   --
   --  The distance of a ramp whose speeds are not negative never decreases as the time increases, to the last bit.
   --  Rounding alone does not give that: adjacent times round differently wherever two expressions meet or large
   --  terms cancel. So the evaluation is arranged as follows.
   --
   --  A ramp which slows down to a speed which is not negative is evaluated backwards: as the distance of the whole
   --  ramp less the distance, in the time which is left, of the ramp which speeds up from the final speed. Nothing
   --  large is then subtracted from Start_Vel * T. The price is that the error of such a distance is a few units in
   --  the last place of the whole ramp's distance at every time, the start of the ramp included, where the forward
   --  evaluation had an error in proportion to the distance.
   --
   --  The distance of a ramp which speeds up is Start_Vel * T plus a gain, and the gain is that of the same ramp
   --  from rest. Within a stage the gain is the gain at the start of the stage plus a polynomial in the time into
   --  the stage. The polynomial is evaluated to twice the working precision and rounded once: its exact value grows
   --  by at least half a unit in its last place between adjacent times, so the rounded value does not decrease. The
   --  time into the stage is at most the stage's length, and the gain is kept between the gain at the start of the
   --  stage and the gain at the start of the next, which is what makes the stages join. The first, fourth, eighth
   --  and twelfth stages start from closed forms, which avoid the rounding of the stages before them, and the
   --  stages between start from the end of the stage before. Each such group of stages is kept between its own
   --  closed form and that of the next group, which can otherwise lie below it when a stage is shorter than the
   --  spacing of the times around it.
   --
   --  Distance_At_Time derives all of that from the ramp for every time. A Prepared_Ramp holds it for all fifteen
   --  stages. The two must agree to the last bit, because the planner places the members of a run with one and
   --  execution evaluates the run with the other. So every operation which rounds is in a subprogram both of them
   --  call, and those are not inlined.
   --
   --  All of this requires every product and every sum to be rounded on its own. A compiler which fuses a product
   --  and a sum into one operation makes the error terms of the polynomial wrong, and the distance then decreases
   --  between some adjacent times. The project is therefore compiled with -ffp-contract=off, which matters on every
   --  target with a fused multiply-add.
   --
   --  Those subprograms reach their results through sums, products and quotients by constants only, so an operation
   --  which overflows, or meets a NaN, leaves a result which is not finite. They test their results once in place of
   --  a check on each operation, as Fast_Distance_At_Max_Time does, and raise exactly when one of those checks
   --  would. The operations include those which split a number into halves, which overflow for magnitudes above
   --  about 1.0E300 where the polynomial alone would not.
   ------------------------------------------------------------------------------------------------------------------

   type Ramp_State is record
      Sn   : Snap := 0.0 * mm / s**4;
      Jr   : Jerk := 0.0 * mm / s**3;
      Acc  : Acceleration := 0.0 * mm / s**2;
      Vel  : Velocity := 0.0 * mm / s;
      Dist : Length := 0.0 * mm;
   end record;
   --  A ramp which speeds up from rest, at the start of one of its stages.

   function Stage_Crackle (Stage : Feedrate_Profile_Stage_Index; Cm : Crackle) return Crackle
   is (case Stage is
         when 1 | 7 | 11 | 13 => Cm,
         when 3 | 5 | 9 | 15  => -Cm,
         when others          => 0.0 * mm / s**5);

   function Stage_Length (Stage : Feedrate_Profile_Stage_Index; Profile : Feedrate_Profile_Times) return Time
   is (case Stage is
         when 2 | 6 | 10 | 14 => Profile (2),
         when 4 | 12          => Profile (3),
         when 8               => Profile (4),
         when others          => Profile (1));

   function Ramp_Bounds (Profile : Feedrate_Profile_Times) return Ramp_Stage_Bounds
   with No_Inline;
   --  When every stage but the last ends: the sums with which the other evaluators compare the time, as they write
   --  them. No bound is below the one before it, because each differs from it by a larger multiple of a phase time
   --  or by one more term.

   function Ramp_Bounds (Profile : Feedrate_Profile_Times) return Ramp_Stage_Bounds is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);

      pragma Suppress (Overflow_Check);
      Result : constant Ramp_Stage_Bounds :=
        [1  => T1,
         2  => T1 + T2,
         3  => 2.0 * T1 + T2,
         4  => 2.0 * T1 + T2 + T3,
         5  => 3.0 * T1 + T2 + T3,
         6  => 3.0 * T1 + 2.0 * T2 + T3,
         7  => 4.0 * T1 + 2.0 * T2 + T3,
         8  => 4.0 * T1 + 2.0 * T2 + T3 + T4,
         9  => 5.0 * T1 + 2.0 * T2 + T3 + T4,
         10 => 5.0 * T1 + 3.0 * T2 + T3 + T4,
         11 => 6.0 * T1 + 3.0 * T2 + T3 + T4,
         12 => 6.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4,
         13 => 7.0 * T1 + 3.0 * T2 + 2.0 * T3 + T4,
         14 => 7.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4];
   begin
      if not (for all Bound of Result => abs Bound <= Time'Last) then
         raise Constraint_Error with "Unrepresentable ramp time.";
      end if;
      return Result;
   end Ramp_Bounds;

   function Stage_At (Bounds : Ramp_Stage_Bounds; T : Time) return Feedrate_Profile_Stage_Index;
   --  The stage which contains T: the first which T is before the end of, as the other evaluators select it.

   function Stage_At (Bounds : Ramp_Stage_Bounds; T : Time) return Feedrate_Profile_Stage_Index is
      --  One more than how many bounds T has reached, by bisection.
      Reached : Feedrate_Profile_Stage_Index'Base := (if T >= Bounds (8) then 8 else 0);
   begin
      if T >= Bounds (Reached + 4) then
         Reached := Reached + 4;
      end if;
      if T >= Bounds (Reached + 2) then
         Reached := Reached + 2;
      end if;
      if Reached < 14 and then T >= Bounds (Reached + 1) then
         Reached := Reached + 1;
      end if;
      return Reached + 1;
   end Stage_At;

   function Stage_End_Distance (State : Ramp_State; DT : Time; Stage_Crackle : Crackle) return Length
   with No_Inline;
   --  The gain at the end of a stage of duration DT and constant crackle Stage_Crackle which starts at State. It is
   --  never below the gain at the start.

   function Stage_End_Distance (State : Ramp_State; DT : Time; Stage_Crackle : Crackle) return Length is
      pragma Suppress (Overflow_Check);
      Local  : constant Length :=
        DT
        * (State.Vel
           + DT * (State.Acc / 2.0 + DT * (State.Jr / 6.0 + DT * (State.Sn / 24.0 + DT * Stage_Crackle / 120.0))));
      --  How far the stage gets in DT.
      Result : constant Length := State.Dist + Length'Max (0.0 * mm, Local);
   begin
      if not (abs Local <= Length'Last and then abs Result <= Length'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result;
   end Stage_End_Distance;

   function Advance (State : Ramp_State; DT : Time; Stage_Crackle : Crackle) return Ramp_State
   with No_Inline;
   --  State after a complete stage of duration DT and constant crackle Stage_Crackle.

   function Advance (State : Ramp_State; DT : Time; Stage_Crackle : Crackle) return Ramp_State is
      pragma Suppress (Overflow_Check);
      Result : constant Ramp_State :=
        (Dist => Stage_End_Distance (State, DT, Stage_Crackle),
         Vel  =>
           State.Vel + DT * (State.Acc + DT * (State.Jr / 2.0 + DT * (State.Sn / 6.0 + DT * Stage_Crackle / 24.0))),
         Acc  => State.Acc + DT * (State.Jr + DT * (State.Sn / 2.0 + DT * Stage_Crackle / 6.0)),
         Jr   => State.Jr + DT * (State.Sn + DT * Stage_Crackle / 2.0),
         Sn   => State.Sn + DT * Stage_Crackle);
   begin
      if not (abs Result.Vel <= Velocity'Last
              and then abs Result.Acc <= Acceleration'Last
              and then abs Result.Jr <= Jerk'Last
              and then abs Result.Sn <= Snap'Last)
      then
         raise Constraint_Error with "Unrepresentable ramp state.";
      end if;
      return Result;
   end Advance;

   function Checkpoint_4_Distance (T1, T2 : Time; Cm : Crackle) return Length
   with No_Inline;

   function Checkpoint_8_Distance (T1, T2, T3 : Time; Cm : Crackle) return Length
   with No_Inline;

   function Checkpoint_12_Distance (T1, T2, T3, T4 : Time; Cm : Crackle) return Length
   with No_Inline;
   --  The gain at the start of stage 4, 8 or 12, in closed form.

   function Checkpoint_4 (T1, T2 : Time; Cm : Crackle; Dist : Length) return Ramp_State
   with No_Inline;
   --  The state at the start of stage 4, with Dist for Checkpoint_4_Distance. This avoids cancellation from stages
   --  1 .. 3.

   function Checkpoint_8 (T1, T2, T3 : Time; Cm : Crackle; Dist : Length) return Ramp_State
   with No_Inline;
   --  The state at the start of stage 8. This avoids cancellation from stages 1 .. 7.

   function Checkpoint_12 (T1, T2, T3, T4 : Time; Cm : Crackle; Dist : Length) return Ramp_State
   with No_Inline;
   --  The state at the start of stage 12. This avoids cancellation from stages 8 .. 11.

   function Checkpoint_4_Distance (T1, T2 : Time; Cm : Crackle) return Length is
      pragma Suppress (Overflow_Check);
      Result : constant Length := Cm * T1 * (T1 + T2) * (2.0 * T1 + T2) * (3.0 * T1**2 + 3.0 * T1 * T2 + T2**2) / 24.0;
   begin
      if not (abs Result <= Length'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result;
   end Checkpoint_4_Distance;

   function Checkpoint_8_Distance (T1, T2, T3 : Time; Cm : Crackle) return Length is
      pragma Suppress (Overflow_Check);
      Result : constant Length :=
        Cm
        * T1
        * (T1 + T2)
        * (2.0 * T1 + T2 + T3)
        * (27.0 * T1**2 + 27.0 * T1 * T2 + 14.0 * T1 * T3 + 7.0 * T2**2 + 7.0 * T2 * T3 + 2.0 * T3**2)
        / 12.0;
   begin
      if not (abs Result <= Length'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result;
   end Checkpoint_8_Distance;

   function Checkpoint_12_Distance (T1, T2, T3, T4 : Time; Cm : Crackle) return Length is
      pragma Suppress (Overflow_Check);
      Result : constant Length :=
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
        / 24.0;
   begin
      if not (abs Result <= Length'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result;
   end Checkpoint_12_Distance;

   function Checked (State : Ramp_State) return Ramp_State;
   --  State, which was computed without a check on each operation.

   function Checked (State : Ramp_State) return Ramp_State is
   begin
      if not (abs State.Vel <= Velocity'Last
              and then abs State.Acc <= Acceleration'Last
              and then abs State.Jr <= Jerk'Last
              and then abs State.Sn <= Snap'Last)
      then
         raise Constraint_Error with "Unrepresentable ramp state.";
      end if;
      return State;
   end Checked;

   function Checkpoint_4 (T1, T2 : Time; Cm : Crackle; Dist : Length) return Ramp_State is
      pragma Suppress (Overflow_Check);
   begin
      return
        Checked
          ((Sn   => 0.0 * mm / s**4,
            Jr   => Cm * T1 * (T1 + T2),
            Acc  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2) / 2.0,
            Vel  => Cm * T1 * (T1 + T2) * (7.0 * T1**2 + 7.0 * T1 * T2 + 2.0 * T2**2) / 12.0,
            Dist => Dist));
   end Checkpoint_4;

   function Checkpoint_8 (T1, T2, T3 : Time; Cm : Crackle; Dist : Length) return Ramp_State is
      pragma Suppress (Overflow_Check);
   begin
      return
        Checked
          ((Sn   => 0.0 * mm / s**4,
            Jr   => 0.0 * mm / s**3,
            Acc  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3),
            Vel  => Cm * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3) / 2.0,
            Dist => Dist));
   end Checkpoint_8;

   function Checkpoint_12 (T1, T2, T3, T4 : Time; Cm : Crackle; Dist : Length) return Ramp_State is
      pragma Suppress (Overflow_Check);
   begin
      return
        Checked
          ((Sn   => 0.0 * mm / s**4,
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
            Dist => Dist));
   end Checkpoint_12;

   function Make_Stage
     (State : Ramp_State; Start, Len : Time; Stage_Crackle : Crackle; Lo, Hi : Length) return Ramp_Stage
   with No_Inline;
   --  The stage which starts at State at time Start and nominally lasts Len, in a group of stages whose gain is to
   --  stay between Lo and Hi.

   function Make_Stage
     (State : Ramp_State; Start, Len : Time; Stage_Crackle : Crackle; Lo, Hi : Length) return Ramp_Stage
   is
      pragma Suppress (Overflow_Check);
      --  A finite value divided by a constant above one is finite.
   begin
      return
        (Start => Start,
         Len   => Len,
         Vel   => State.Vel,
         Acc_2 => State.Acc / 2.0,
         Jr_6  => State.Jr / 6.0,
         Sn_24 => State.Sn / 24.0,
         C_120 => Stage_Crackle / 120.0,
         Dist  => State.Dist,
         Lo    => Length'Min (Length'Max (State.Dist, Lo), Hi),
         Hi    => Length'Min (Length'Max (Stage_End_Distance (State, Len, Stage_Crackle), Lo), Hi));
   end Make_Stage;

   function Rising_Stage
     (Profile : Feedrate_Profile_Times; Cm : Crackle; Bounds : Ramp_Stage_Bounds; Stage : Feedrate_Profile_Stage_Index)
      return Ramp_Stage;
   --  The stage Stage of the ramp which speeds up from rest with crackle Cm, started as described above.

   function Rising_Stage
     (Profile : Feedrate_Profile_Times; Cm : Crackle; Bounds : Ramp_Stage_Bounds; Stage : Feedrate_Profile_Stage_Index)
      return Ramp_Stage
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);

      Zero : constant Crackle := 0.0 * mm / s**5;

      State  : Ramp_State;
      Lo, Hi : Length;
   begin
      case Stage is
         when 1 .. 3   =>
            Lo := 0.0 * mm;
            Hi := Checkpoint_4_Distance (T1, T2, Cm);
            if Stage >= 2 then
               State := Advance (State, T1, Cm);
            end if;
            if Stage >= 3 then
               State := Advance (State, T2, Zero);
            end if;

         when 4 .. 7   =>
            Lo := Checkpoint_4_Distance (T1, T2, Cm);
            Hi := Checkpoint_8_Distance (T1, T2, T3, Cm);
            State := Checkpoint_4 (T1, T2, Cm, Lo);
            if Stage >= 5 then
               State := Advance (State, T3, Zero);
            end if;
            if Stage >= 6 then
               State := Advance (State, T1, -Cm);
            end if;
            if Stage >= 7 then
               State := Advance (State, T2, Zero);
            end if;

         when 8 .. 11  =>
            Lo := Checkpoint_8_Distance (T1, T2, T3, Cm);
            Hi := Length'Max (Checkpoint_12_Distance (T1, T2, T3, T4, Cm), Lo);
            State := Checkpoint_8 (T1, T2, T3, Cm, Lo);
            if Stage >= 9 then
               State := Advance (State, T4, Zero);
            end if;
            if Stage >= 10 then
               State := Advance (State, T1, -Cm);
            end if;
            if Stage >= 11 then
               State := Advance (State, T2, Zero);
            end if;

         when 12 .. 15 =>
            Hi := Length'Last;
            State := Checkpoint_12 (T1, T2, T3, T4, Cm, Checkpoint_12_Distance (T1, T2, T3, T4, Cm));
            Lo := Length'Max (State.Dist, Checkpoint_8_Distance (T1, T2, T3, Cm));
            if Stage >= 13 then
               State := Advance (State, T3, Zero);
            end if;
            if Stage >= 14 then
               State := Advance (State, T1, Cm);
            end if;
            if Stage = 15 then
               State := Advance (State, T2, Zero);
            end if;
      end case;

      return
        Make_Stage
          (State,
           (if Stage = 1 then 0.0 * s else Bounds (Stage - 1)),
           Stage_Length (Stage, Profile),
           Stage_Crackle (Stage, Cm),
           Lo,
           Hi);
   end Rising_Stage;

   Splitter : constant Dimensionless := 134_217_729.0;
   --  2**27 + 1. A number times this, less what that product exceeds the number by, is the number rounded to its
   --  upper 26 bits, and the rest of the number then also has at most 26 bits. Products of such halves are exact.

   function High_Half (A : Dimensionless) return Dimensionless
   with Inline_Always;
   --  A rounded to its upper 26 bits.

   function Product_Error (A, A_High, X, X_High, Product : Dimensionless) return Dimensionless
   with Inline_Always;
   --  What the rounding of A * X to Product lost, exactly, from the halves of A and X.

   function Sum_Error (A, B, Sum : Dimensionless) return Dimensionless
   with Inline_Always;
   --  What the rounding of A + B to Sum lost, exactly.

   function Compensated_Local (Stage : Ramp_Stage; X : Time) return Length;
   --  X * (Vel + X * (Acc_2 + X * (Jr_6 + X * (Sn_24 + X * C_120)))) by the Horner scheme, with the rounding error
   --  of every product and sum carried along in a second Horner scheme and added at the end. The error of a product
   --  is exact from the halves of its factors, so no target needs a fused multiply-add, and every target computes
   --  the same value provided that it fuses nothing here itself (see -ffp-contract=off above).

   function High_Half (A : Dimensionless) return Dimensionless is
      pragma Suppress (Overflow_Check);
      Wide : constant Dimensionless := Splitter * A;
   begin
      return Wide - (Wide - A);
   end High_Half;

   function Product_Error (A, A_High, X, X_High, Product : Dimensionless) return Dimensionless is
      pragma Suppress (Overflow_Check);
      A_Low : constant Dimensionless := A - A_High;
      X_Low : constant Dimensionless := X - X_High;
   begin
      return A_Low * X_Low - (((Product - A_High * X_High) - A_Low * X_High) - A_High * X_Low);
   end Product_Error;

   function Sum_Error (A, B, Sum : Dimensionless) return Dimensionless is
      pragma Suppress (Overflow_Check);
      Other : constant Dimensionless := Sum - A;
   begin
      return (A - (Sum - Other)) + (B - Other);
   end Sum_Error;

   function Compensated_Local (Stage : Ramp_Stage; X : Time) return Length is
      pragma Suppress (Overflow_Check);
      --  An operation below which overflows, or meets a NaN, leaves a result which is not finite: every later
      --  operation is a sum or a product. Testing the result once therefore raises wherever a check on each
      --  operation would, those of the plain scheme among them.

      Raw_X  : constant Dimensionless := X / s;
      X_High : constant Dimensionless := High_Half (Raw_X);

      C_5 : constant Dimensionless := Stage.C_120 / (mm / s**5);
      C_4 : constant Dimensionless := Stage.Sn_24 / (mm / s**4);
      C_3 : constant Dimensionless := Stage.Jr_6 / (mm / s**3);
      C_2 : constant Dimensionless := Stage.Acc_2 / (mm / s**2);
      C_1 : constant Dimensionless := Stage.Vel / (mm / s);

      --  P is a product by X and S the sum of it and the next coefficient, both rounded. R is what the scheme has
      --  lost so far, itself carried by the Horner scheme.
      P_4 : constant Dimensionless := C_5 * Raw_X;
      S_4 : constant Dimensionless := P_4 + C_4;
      R_4 : constant Dimensionless :=
        Product_Error (C_5, High_Half (C_5), Raw_X, X_High, P_4) + Sum_Error (P_4, C_4, S_4);

      P_3 : constant Dimensionless := S_4 * Raw_X;
      S_3 : constant Dimensionless := P_3 + C_3;
      R_3 : constant Dimensionless :=
        R_4 * Raw_X + (Product_Error (S_4, High_Half (S_4), Raw_X, X_High, P_3) + Sum_Error (P_3, C_3, S_3));

      P_2 : constant Dimensionless := S_3 * Raw_X;
      S_2 : constant Dimensionless := P_2 + C_2;
      R_2 : constant Dimensionless :=
        R_3 * Raw_X + (Product_Error (S_3, High_Half (S_3), Raw_X, X_High, P_2) + Sum_Error (P_2, C_2, S_2));

      P_1 : constant Dimensionless := S_2 * Raw_X;
      S_1 : constant Dimensionless := P_1 + C_1;
      R_1 : constant Dimensionless :=
        R_2 * Raw_X + (Product_Error (S_2, High_Half (S_2), Raw_X, X_High, P_1) + Sum_Error (P_1, C_1, S_1));

      P_0    : constant Dimensionless := S_1 * Raw_X;
      Result : constant Dimensionless :=
        P_0 + (R_1 * Raw_X + Product_Error (S_1, High_Half (S_1), Raw_X, X_High, P_0));
   begin
      if not (abs Result <= Dimensionless'Last) then
         raise Constraint_Error with "Unrepresentable ramp distance.";
      end if;
      return Result * mm;
   end Compensated_Local;

   function Stage_Gain (Stage : Ramp_Stage; T : Time) return Length
   with No_Inline;
   --  The gain at T, which Stage contains.

   function Stage_Gain (Stage : Ramp_Stage; T : Time) return Length is
   begin
      return
        Length'Min
          (Length'Max (Stage.Dist + Compensated_Local (Stage, Time'Min (T - Stage.Start, Stage.Len)), Stage.Lo),
           Stage.Hi);
   end Stage_Gain;

   Rest_Tolerance : constant Dimensionless := 2.0**(-20);
   --  A ramp which was solved to end at rest has a final speed within a far smaller fraction of its starting speed
   --  than this of zero, on either side. A final speed this little below zero is taken for rest.

   function Shape_Of (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Ramp_Shape
   with No_Inline;

   function Shape_Of (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Ramp_Shape
   is
   begin
      if Max_Crackle >= 0.0 * mm / s**5 then
         return (Kind => Speeds_Up, Vel => Start_Vel, Total => Total_Time (Profile), Whole => 0.0 * mm);
      end if;
      declare
         End_Vel : constant Velocity := Fast_Velocity_At_Max_Time (Profile, Max_Crackle, Start_Vel);
      begin
         if End_Vel < -Rest_Tolerance * abs Start_Vel then
            --  The ramp turns back, so its distance is not monotone and nothing is gained by evaluating it
            --  backwards.
            return (Kind => Turns_Back, Vel => Start_Vel, Total => Total_Time (Profile), Whole => 0.0 * mm);
         end if;
         declare
            Final : constant Velocity := Velocity'Max (0.0 * mm / s, End_Vel);
         begin
            return
              (Kind  => Slows_Down,
               Vel   => Final,
               Total => Total_Time (Profile),
               Whole => Fast_Distance_At_Max_Time (Profile, -Max_Crackle, Final));
         end;
      end;
   end Shape_Of;

   function Rising_Time (Shape : Ramp_Shape; T : Time) return Time
   is (if Shape.Kind = Slows_Down then Time'Max (0.0 * s, Shape.Total - T) else T);
   --  The time at which the ramp which speeds up from rest is evaluated for the distance at T.

   function Ramp_Distance (Shape : Ramp_Shape; T, Rising_T : Time; Gain : Length) return Length
   with No_Inline;
   --  The distance at T, from the gain at Rising_T of the ramp which speeds up from rest.

   function Ramp_Distance (Shape : Ramp_Shape; T, Rising_T : Time; Gain : Length) return Length is
   begin
      case Shape.Kind is
         when Speeds_Up  =>
            return Shape.Vel * T + Gain;

         when Turns_Back =>
            return Shape.Vel * T - Gain;

         when Slows_Down =>
            if T <= 0.0 * s then
               return 0.0 * mm;
            end if;
            --  Whole is not the backwards ramp's own distance at its end, so the difference can be just below zero.
            return Length'Max (0.0 * mm, Shape.Whole - (Shape.Vel * Rising_T + Gain));
      end case;
   end Ramp_Distance;

   function Distance_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length
   is
      Shape    : constant Ramp_Shape := Shape_Of (Profile, Max_Crackle, Start_Vel);
      Rising_T : constant Time := Rising_Time (Shape, T);
      Bounds   : constant Ramp_Stage_Bounds := Ramp_Bounds (Profile);
   begin
      pragma Assert (T <= Total_Time (Profile));
      pragma Assert (T >= 0.0 * s);

      return
        Ramp_Distance
          (Shape,
           T,
           Rising_T,
           Stage_Gain (Rising_Stage (Profile, abs Max_Crackle, Bounds, Stage_At (Bounds, Rising_T)), Rising_T));
   end Distance_At_Time;

   function Prepare
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Prepared_Ramp
   is
      T1 : constant Time := Profile (1);
      T2 : constant Time := Profile (2);
      T3 : constant Time := Profile (3);
      T4 : constant Time := Profile (4);
      Cm : constant Crackle := abs Max_Crackle;

      Bounds : constant Ramp_Stage_Bounds := Ramp_Bounds (Profile);
      D4     : constant Length := Checkpoint_4_Distance (T1, T2, Cm);
      D8     : constant Length := Checkpoint_8_Distance (T1, T2, T3, Cm);
      D12    : constant Length := Checkpoint_12_Distance (T1, T2, T3, T4, Cm);
      Late   : constant Length := Length'Max (D12, D8);

      State  : Ramp_State;
      Stages : Ramp_Stage_Array;
   begin
      --  Every stage as Rising_Stage makes it, with each state taken from the stage before in place of a new start
      --  from the closed form of its group.
      for Stage in Feedrate_Profile_Stage_Index loop
         case Stage is
            when 1      =>
               null;

            when 4      =>
               State := Checkpoint_4 (T1, T2, Cm, D4);

            when 8      =>
               State := Checkpoint_8 (T1, T2, T3, Cm, D8);

            when 12     =>
               State := Checkpoint_12 (T1, T2, T3, T4, Cm, D12);

            when others =>
               State := Advance (State, Stage_Length (Stage - 1, Profile), Stage_Crackle (Stage - 1, Cm));
         end case;
         Stages (Stage) :=
           Make_Stage
             (State,
              (if Stage = 1 then 0.0 * s else Bounds (Stage - 1)),
              Stage_Length (Stage, Profile),
              Stage_Crackle (Stage, Cm),
              (case Stage is
                 when 1 .. 3   => 0.0 * mm,
                 when 4 .. 7   => D4,
                 when 8 .. 11  => D8,
                 when 12 .. 15 => Late),
              (case Stage is
                 when 1 .. 3   => D4,
                 when 4 .. 7   => D8,
                 when 8 .. 11  => Late,
                 when 12 .. 15 => Length'Last));
      end loop;
      return (Shape => Shape_Of (Profile, Max_Crackle, Start_Vel), Bounds => Bounds, Stages => Stages);
   end Prepare;

   function Distance_At_Time (Ramp : Prepared_Ramp; T : Time) return Length is
      Rising_T : constant Time := Rising_Time (Ramp.Shape, T);
   begin
      pragma Assert (T <= Ramp.Shape.Total);
      pragma Assert (T >= 0.0 * s);

      return
        Ramp_Distance (Ramp.Shape, T, Rising_T, Stage_Gain (Ramp.Stages (Stage_At (Ramp.Bounds, Rising_T)), Rising_T));
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

   function Largest_Nonpositive
     (Lower, Upper                   : Dimensionless;
      Lower_Residual, Upper_Residual : Dimensionless;
      Relative_Tolerance             : Dimensionless := 0.0;
      Guess                          : Dimensionless := -1.0;
      Use_Model                      : Boolean := True) return Dimensionless
   is
      type Bits is mod 2**64;
      function To_Bits is new Ada.Unchecked_Conversion (Dimensionless, Bits);
      function To_Real is new Ada.Unchecked_Conversion (Bits, Dimensionless);
      --  Nonnegative machine numbers are ordered like their bit patterns, so pattern differences count the machine
      --  numbers between two bracket ends. This relies on IEEE 754 doubles, as the checks below confirm.

      Settled_Gap         : constant Bits := 4;
      --  A model estimate within this many machine numbers of a bracket end has converged onto that end.
      Maximum_Model_Steps : constant Positive := 48;
      First_Closing_Step  : constant Bits := 2;
      Closing_Growth      : constant Bits := 8;

      Lo      : Dimensionless := (if Lower = 0.0 then 0.0 else Lower);
      Hi      : Dimensionless := Upper;
      Near_Lo : Boolean := True;
      --  Whether the latest estimate of the result is the lower end of the bracket rather than the upper one.

      function Closed return Boolean
      is (To_Bits (Hi) - To_Bits (Lo) <= 1
          or else (Relative_Tolerance > 0.0 and then Hi - Lo <= Relative_Tolerance * Lo));

      procedure Evaluate (X : Dimensionless; Value : out Dimensionless);
      --  Evaluate Residual at X, strictly inside the bracket, and move the matching end there.

      procedure Evaluate (X : Dimensionless; Value : out Dimensionless) is
      begin
         Value := Residual (X);
         if Value <= 0.0 then
            Lo := X;
            Near_Lo := True;
         else
            Hi := X;
            Near_Lo := False;
         end if;
      end Evaluate;

      Unused : Dimensionless;
   begin
      pragma Assert (Dimensionless'Size = 64);
      pragma Assert (To_Bits (86_400.0) = 4_680_673_776_000_565_248);
      pragma Assert (To_Bits (0.123_45) = 4_593_559_930_647_147_132);

      if not (Lo >= 0.0 and then Lo < Hi and then Hi <= Dimensionless'Last) then
         return Lower;
      end if;

      if Guess > Lo and then Guess < Hi then
         Evaluate (Guess, Unused);
      elsif Guess >= Hi then
         --  The caller's estimate is at or beyond the upper end, so the result is just below that end.
         Near_Lo := False;
      elsif Use_Model and then Lower_Residual < 0.0 then
         declare
            Target           : constant Dimensionless := -Lower_Residual;
            --  The model is anchored at the original lower end: the excess over Lower_Residual is zero there and
            --  must reach Target at the result.
            Anchor           : constant Dimensionless := Lo;
            Previous_Offset  : Dimensionless := 0.0;
            Previous_Excess  : Dimensionless := 0.0;
            Latest_Offset    : Dimensionless := Hi - Lo;
            Latest_Excess    : Dimensionless :=
              (if Upper_Residual > 0.0 and then Upper_Residual <= Dimensionless'Last - Target
               then Upper_Residual + Target
               else 0.0);
            --  Zero leaves the model without a point to fit, as for an upper residual of unknown size.
            Last_Magnitude   : Dimensionless := Dimensionless'Last;
            Exponent         : Dimensionless := 1.0;
            Poor_Steps       : Natural := 0;
            --  Consecutive model steps that failed to halve the residual; two of them force a bisection step.
            Estimate, Value  : Dimensionless;
            Have_Estimate    : Boolean;
            Within_Tolerance : Boolean;
         begin
            Near_Lo := False;
            for Step in 1 .. Maximum_Model_Steps loop
               exit when Closed or else To_Bits (Hi) - To_Bits (Lo) <= 2 * Settled_Gap;
               Have_Estimate := False;
               if Poor_Steps < 2 and then Latest_Excess > 0.0 and then Latest_Offset > 0.0 then
                  begin
                     if Previous_Excess > 0.0
                       and then Previous_Offset > 0.0
                       and then Previous_Excess /= Latest_Excess
                       and then Previous_Offset /= Latest_Offset
                     then
                        declare
                           Fitted : constant Dimensionless :=
                             Dimensionless_Math.Log (Latest_Excess / Previous_Excess)
                             / Dimensionless_Math.Log (Latest_Offset / Previous_Offset);
                        begin
                           if Fitted >= 0.05 and then Fitted <= 8.0 then
                              Exponent := Fitted;
                           end if;
                        end;
                     end if;
                     declare
                        Log_Step : constant Dimensionless :=
                          Dimensionless_Math.Log (Target / Latest_Excess) / Exponent;
                     begin
                        if abs Log_Step <= 80.0 then
                           Estimate := Anchor + Latest_Offset * Dimensionless_Math.Exp (Log_Step);
                           Have_Estimate := True;
                        end if;
                     end;
                  exception
                     when Constraint_Error | Ada.Numerics.Argument_Error =>
                        Have_Estimate := False;
                  end;
               end if;
               Within_Tolerance := False;
               if Have_Estimate and then Estimate > Lo and then Estimate < Hi then
                  if To_Bits (Estimate) - To_Bits (Lo) <= Settled_Gap then
                     Near_Lo := True;
                     exit;
                  elsif To_Bits (Hi) - To_Bits (Estimate) <= Settled_Gap then
                     Near_Lo := False;
                     exit;
                  end if;
                  --  An estimate that has stopped moving by more than a fraction of the tolerance only needs the
                  --  bracket closed around it.
                  Within_Tolerance :=
                    Relative_Tolerance > 0.0
                    and then abs (Estimate - (Anchor + Latest_Offset)) <= 0.25 * Relative_Tolerance * Estimate;
                  Evaluate (Estimate, Value);
                  Poor_Steps := (if abs Value > 0.5 * Last_Magnitude then Poor_Steps + 1 else 0);
               elsif Have_Estimate and then Estimate <= Lo then
                  Near_Lo := True;
                  exit;
               elsif Have_Estimate and then Estimate >= Hi then
                  Near_Lo := False;
                  exit;
               else
                  --  Without a usable model, halve the bracket. Its ends may be many orders of magnitude apart
                  --  only when one is zero, and the closing search below handles that by machine numbers.
                  Estimate := Lo + 0.5 * (Hi - Lo);
                  if not (Estimate > Lo and then Estimate < Hi) then
                     Estimate := To_Real (To_Bits (Lo) + (To_Bits (Hi) - To_Bits (Lo)) / 2);
                  end if;
                  Evaluate (Estimate, Value);
                  Poor_Steps := 0;
               end if;
               Last_Magnitude := abs Value;
               Previous_Offset := Latest_Offset;
               Previous_Excess := Latest_Excess;
               Latest_Offset := Estimate - Anchor;
               --  Target is finite, so only a residual already at the top of the range could overflow here, and the
               --  model then simply stops proposing estimates.
               Latest_Excess := (if Value <= Dimensionless'Last - Target then Value + Target else 0.0);
               exit when Within_Tolerance;
            end loop;
         end;
      end if;

      --  Close the bracket around the estimate: step away from it by a growing count of machine numbers until the
      --  residual changes sign, then bisect the machine numbers in between.
      declare
         Step : Bits := First_Closing_Step;
         Gap  : Bits;
      begin
         if Relative_Tolerance > 0.0 and then not Closed then
            --  A first step of half the tolerance closes the bracket at once when the estimate is that good.
            declare
               Probe : constant Dimensionless :=
                 (if Near_Lo then Lo + 0.5 * Relative_Tolerance * Lo else Hi - 0.5 * Relative_Tolerance * Hi);
            begin
               if Probe > Lo and then Probe < Hi then
                  Step :=
                    Bits'Max
                      (First_Closing_Step,
                       (if Near_Lo then To_Bits (Probe) - To_Bits (Lo) else To_Bits (Hi) - To_Bits (Probe)));
               end if;
            end;
         end if;
         loop
            exit when Closed;
            Gap := To_Bits (Hi) - To_Bits (Lo);
            exit when Step >= Gap;
            if Near_Lo then
               Evaluate (To_Real (To_Bits (Lo) + Step), Unused);
               exit when not Near_Lo;
            else
               Evaluate (To_Real (To_Bits (Hi) - Step), Unused);
               exit when Near_Lo;
            end if;
            Step := (if Step > Bits'Last / Closing_Growth then Bits'Last else Step * Closing_Growth);
         end loop;
         loop
            exit when Closed;
            Evaluate (To_Real (To_Bits (Lo) + (To_Bits (Hi) - To_Bits (Lo)) / 2), Unused);
         end loop;
      end;
      return Lo;
   end Largest_Nonpositive;

   function Prepare_Delta_V
     (Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle) return Delta_V_Solver
   is
      Am     : constant Acceleration := Acceleration_Max;
      Jm     : constant Jerk := Jerk_Max;
      Sm     : constant Snap := Snap_Max;
      Cm     : constant Crackle := Crackle_Max;
      Solver : Delta_V_Solver;
      T2, T3 : Time;
   begin
      Solver.Limits_Valid := Derivative_Limits_Are_Valid (Am, Jm, Sm, Cm);
      if not Solver.Limits_Valid then
         return Solver;
      end if;
      Solver.Am := Am;
      Solver.Jm := Jm;
      Solver.Cm := Cm;
      Solver.Zero_Limit :=
        Am = 0.0 * mm / s**2 or else Jm = 0.0 * mm / s**3 or else Sm = 0.0 * mm / s**4 or else Cm = 0.0 * mm / s**5;
      if Solver.Zero_Limit then
         return Solver;
      end if;

      --  Each case is selected by testing the sign of the computed phase time it would use, with the equivalent
      --  algebraic condition noted beside each test. A rearranged test rounds differently and can select a region
      --  whose phase time is negative.
      begin
         Solver.Jerk_Over_Snap := Jm / Sm;
         Solver.Snap_Over_Crackle := Sm / Cm;
         T2 := Solver.Jerk_Over_Snap - Solver.Snap_Over_Crackle;
         if T2 > 0.0 * s then
            --  Sm ** 2 < Jm * Cm
            Solver.Accel_Over_Jerk := Am / Jm;
            T3 := Solver.Accel_Over_Jerk - Solver.Jerk_Over_Snap - Solver.Snap_Over_Crackle;
            if T3 >= 0.0 * s then
               --  Am >= Jm * (Jm / Sm + Sm / Cm)
               Solver.Region := Region_1;
               Solver.T1 := Solver.Snap_Over_Crackle;
               Solver.T2 := T2;
               Solver.T3 := T3;
            else
               Solver.Snap_Root := Dimensionless_Math.Sqrt (0.25 * Sm**2 / Cm**2 + Am / Sm);
               T2 := Solver.Snap_Root - 1.5 * Sm / Cm;
               if T2 >= 0.0 * s then
                  --  Am >= 2.0 * Sm ** 3 / Cm ** 2
                  Solver.Region := Region_2;
                  Solver.T1 := Solver.Snap_Over_Crackle;
                  Solver.T2 := T2;
               else
                  Solver.Region := Region_3;
                  Solver.T1 := (0.5 * Am / Cm)**(1 / 3);
               end if;
            end if;
         else
            Solver.Accel_Over_Jerk := Am / Jm;
            Solver.Jerk_Over_Crackle := Jm / Cm;
            Solver.T1 := Dimensionless_Math.Sqrt (Solver.Jerk_Over_Crackle);
            T3 := Solver.Accel_Over_Jerk - 2.0 * Solver.T1;
            if T3 > 0.0 * s then
               --  Am > 2.0 * Jm * Dimensionless_Math.Sqrt (Jm / Cm)
               Solver.Region := Region_4;
               Solver.T3 := T3;
            else
               Solver.Region := Region_5;
               Solver.T1 := (0.5 * Am / Cm)**(1 / 3);
            end if;
         end if;
      exception
         when Constraint_Error =>
            Solver.Unclassifiable := True;
            return Solver;
      end;

      if Solver.Region in Region_1 | Region_2 then
         begin
            Solver.Snap_Only_Above := 8.0 * Sm**4 / Cm**3;
         exception
            when Constraint_Error =>
               Solver.Threshold_Raises := True;
         end;
      end if;
      return Solver;
   end Prepare_Delta_V;

   function Optimal_Profile_For_Delta_V_Internal
     (Solver : Delta_V_Solver; Delta_V : Velocity) return Internal_Profile_Result
   is
      Vd     : constant Velocity := abs Delta_V;
      Am     : Acceleration renames Solver.Am;
      Jm     : Jerk renames Solver.Jm;
      Cm     : Crackle renames Solver.Cm;
      T1     : Time renames Solver.T1;
      Region : Constraint_Region renames Solver.Region;
      T3, T4 : Time;

      function Crackle_Limited return Internal_Profile_Result
      is ([Dimensionless_Math.Sqrt (Dimensionless_Math.Sqrt (0.125 * Vd / Cm)), 0.0 * s, 0.0 * s, 0.0 * s], Region, 1);
      --  No limit other than crackle is reached.

      function Snap_Limited return Internal_Profile_Result;
      --  Snap is the only derivative limit reached, so the second phase time solves
      --
      --     Vd = Cm*T1*(T1 + T2)*(2*T1 + T2)*(4*T1 + 2*T2)
      --
      --  with T1 = Sm/Cm. The analytical solution that Mathematica outputs involves a Cm**18, which is far outside
      --  the range of Dimensioned_Float for reasonable values of Cm. For reference:
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
      --  The result is the largest time below 24 hours whose profile, as Fast_Velocity_At_Max_Time evaluates it,
      --  does not exceed Vd. That evaluation is a product of sums of nonnegative terms, so it is monotone in machine
      --  arithmetic and the search returns what bisecting every machine number would.

      function Snap_Limited return Internal_Profile_Result is
         function Excess (T2_Seconds : Dimensionless) return Dimensionless
         is ((Fast_Velocity_At_Max_Time ([T1, T2_Seconds * s, 0.0 * s, 0.0 * s], Cm, 0.0 * mm / s) - Vd) / (mm / s));

         function Solve is new Largest_Nonpositive (Excess);

         Guess : Dimensionless := -1.0;
      begin
         --  With U = T2/T1 the equation is 2*(1 + U)*(2 + U)**2 = Vd/(Cm*T1**4), whose left side is convex and
         --  increasing, so Newton's method from above the root decreases onto it.
         declare
            Scaled      : constant Dimensionless := Vd / (Cm * T1**4);
            U           : Dimensionless := (Scaled - 8.0) / 16.0;
            --  The tangent at zero lies below the curve, so this is at or above the root.
            Next, Value : Dimensionless;
         begin
            if U > 4.0 then
               --  The cubic term alone bounds the root more tightly once it dominates.
               U := Dimensionless'Min (U, Dimensionless_Math."**" (0.5 * Scaled, 1.0 / 3.0));
            end if;
            if U > 0.0 then
               for Iteration in 1 .. 64 loop
                  Value := 2.0 * (1.0 + U) * (2.0 + U)**2 - Scaled;
                  Next := U - Value / ((2.0 + U) * (8.0 + 6.0 * U));
                  exit when not (Next < U) or else Next <= 0.0;
                  U := Next;
               end loop;
               Guess := U * T1 / s;
            end if;
         exception
            when Constraint_Error | Ada.Numerics.Argument_Error =>
               Guess := -1.0;
         end;
         return
           ([T1,
             Solve
               (Lower          => 0.0,
                Upper          => 86_400.0,
                --  A maximum of 24 hours should be more than enough.
                Lower_Residual => 0.0,
                Upper_Residual => 0.0,
                Guess          => Guess,
                Use_Model      => False)
             * s,
             0.0 * s,
             0.0 * s],
            Region,
            2);
      end Snap_Limited;
   begin
      if not (Vd <= Velocity'Last) or else not Solver.Limits_Valid then
         raise Constraint_Error with "Invalid velocity-profile inputs.";
      elsif Solver.Zero_Limit then
         if Vd = 0.0 * mm / s then
            return ([others => 0.0 * s], Region_5, 1);
         end if;
         raise Constraint_Error with "A velocity change requires positive derivative limits.";
      elsif Solver.Unclassifiable then
         raise Constraint_Error with "Unrepresentable phase times for these derivative limits.";
      end if;
      --  This function is called a lot more than Optimal_Profile_For_Distance, so we use simple analytical solutions
      --  where they exist, and the limit-only parts of those come prepared in Solver.
      --
      --  As in Optimal_Profile_For_Distance_Internal, each case is selected by testing the sign of the computed phase
      --  time it would use, with the equivalent algebraic condition noted beside each test.
      case Region is
         when Region_1            =>
            T4 := Vd / Am - Solver.Accel_Over_Jerk - Solver.Jerk_Over_Snap - Solver.Snap_Over_Crackle;
            if T4 > 0.0 * s then
               --  Vd > Am * (Am / Jm + Jm / Sm + Sm / Cm)
               --  Reachable: Sm, Jm, Am (Test Case V1.4)
               return ([T1, Solver.T2, Solver.T3, T4], Region, 4);
            end if;
            T3 :=
              0.5 * Dimensionless_Math.Sqrt ((Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle)**2 + 4.0 * Vd / Jm)
              - 1.5 * (Solver.Jerk_Over_Snap + Solver.Snap_Over_Crackle);
            if T3 > 0.0 * s then
               --  Vd > 2.0 * Jm * (Jm / Sm + Sm / Cm) ** 2
               --  Reachable: Sm, Jm (Test Case V1.3)
               return ([T1, Solver.T2, T3, 0.0 * s], Region, 3);
            elsif Solver.Threshold_Raises then
               raise Constraint_Error with "Unrepresentable snap-limited velocity threshold.";
            elsif Vd > Solver.Snap_Only_Above then
               --  Vd > 8.0 * Sm ** 4 / Cm ** 3
               --  Reachable: Sm (Test Case V1.2)
               return Snap_Limited;
            else
               --  Reachable: None (Test Case V1.1)
               return Crackle_Limited;
            end if;

         when Region_2            =>
            T4 := Vd / Am - Solver.Snap_Over_Crackle - 2.0 * Solver.Snap_Root;
            if T4 > 0.0 * s then
               --  Vd > Am * (2.0 * Dimensionless_Math.Sqrt (0.25 * Sm ** 2 / Cm ** 2 + Am / Sm) + Sm / Cm)
               --  Reachable: Sm, Am (Test Case V2.4)
               return ([T1, Solver.T2, 0.0 * s, T4], Region, 4);
            elsif Solver.Threshold_Raises then
               raise Constraint_Error with "Unrepresentable snap-limited velocity threshold.";
            elsif Vd > Solver.Snap_Only_Above then
               --  Vd > 8.0 * Sm ** 4 / Cm ** 3
               --  Reachable: Sm (Test Case V2.2)
               return Snap_Limited;
            else
               --  Reachable: None (Test Case V2.1)
               return Crackle_Limited;
            end if;

         when Region_3 | Region_5 =>
            T4 := Vd / Am - 4.0 * T1;
            if T4 > 0.0 * s then
               --  Vd > 8.0 * Cm * (0.5 * Am / Cm) ** (4 / 3)
               --  Reachable: Am (Test Cases V3.4 and V5.4)
               return ([T1, 0.0 * s, 0.0 * s, T4], Region, 4);
            else
               --  Reachable: None (Test Cases V3.1 and V5.1)
               return Crackle_Limited;
            end if;

         when Region_4            =>
            T4 := Vd / Am - Solver.Accel_Over_Jerk - 2.0 * T1;
            if T4 > 0.0 * s then
               --  Vd > Am * (Am / Jm + 2.0 * Dimensionless_Math.Sqrt (Jm / Cm))
               --  Reachable: Jm, Am (Test Case V4.4)
               return ([T1, 0.0 * s, Solver.T3, T4], Region, 4);
            end if;
            T3 := Dimensionless_Math.Sqrt (Solver.Jerk_Over_Crackle + Vd / Jm) - 3.0 * T1;
            if T3 > 0.0 * s then
               --  Vd > 8.0 * Jm ** 2 / Cm
               --  Reachable: Jm (Test Case V4.3)
               return ([T1, 0.0 * s, T3, 0.0 * s], Region, 3);
            else
               --  Reachable: None (Test Case V4.1)
               return Crackle_Limited;
            end if;
      end case;
   end Optimal_Profile_For_Delta_V_Internal;

   function Optimal_Profile_For_Delta_V_Internal
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
      return Internal_Profile_Result is
   begin
      return
        Optimal_Profile_For_Delta_V_Internal
          (Prepare_Delta_V (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max), Delta_V);
   end Optimal_Profile_For_Delta_V_Internal;

   function Delta_V_Over_Distance (Solver : Delta_V_Solver; Start_Vel : Velocity; Distance : Length) return Velocity is
      pragma Suppress (Overflow_Check);
      --  This is plain arithmetic on raw values: a nonfinite or implausible outcome is rejected at the end.

      type Phase_Times is array (1 .. 4) of Dimensionless;

      Vs : constant Dimensionless := Start_Vel / (mm / s);
      D  : constant Dimensionless := Distance / mm;
      Cm : constant Dimensionless := Solver.Cm / (mm / s**5);

      function Gain (T : Phase_Times) return Dimensionless
      is (Cm * T (1) * (T (1) + T (2)) * (2.0 * T (1) + T (2) + T (3)) * (4.0 * T (1) + 2.0 * T (2) + T (3) + T (4)));
      --  The velocity change, as in Fast_Velocity_At_Max_Time.

      function Travel (T : Phase_Times) return Dimensionless
      is ((Vs + Gain (T) / 2.0) * (8.0 * T (1) + 4.0 * T (2) + 2.0 * T (3) + T (4)));
      --  The distance, as in Fast_Distance_At_Max_Time.

      function Travel_Slope (T : Phase_Times; Variable : Positive) return Dimensionless;
      --  The derivative of Travel with respect to one phase time.

      function Travel_Slope (T : Phase_Times; Variable : Positive) return Dimensionless is
         A          : constant Dimensionless := T (1);
         B          : constant Dimensionless := T (1) + T (2);
         C          : constant Dimensionless := 2.0 * T (1) + T (2) + T (3);
         E          : constant Dimensionless := 4.0 * T (1) + 2.0 * T (2) + T (3) + T (4);
         Total      : constant Dimensionless := 8.0 * T (1) + 4.0 * T (2) + 2.0 * T (3) + T (4);
         Gain_Slope : constant Dimensionless :=
           (case Variable is
              when 1      => Cm * (B * C * E + A * C * E + 2.0 * A * B * E + 4.0 * A * B * C),
              when 2      => Cm * A * (C * E + B * E + 2.0 * B * C),
              when 3      => Cm * A * B * (E + C),
              when others => Cm * A * B * C);
         Time_Slope : constant Dimensionless :=
           (case Variable is
              when 1      => 8.0,
              when 2      => 4.0,
              when 3      => 2.0,
              when others => 1.0);
      begin
         return Gain_Slope / 2.0 * Total + (Vs + Cm * A * B * C * E / 2.0) * Time_Slope;
      end Travel_Slope;

      Full     : constant Phase_Times := [Solver.T1 / s, Solver.T2 / s, Solver.T3 / s, 0.0];
      --  Every limit below velocity saturated, as in the fastest case of each constraint region.
      T        : Phase_Times := Full;
      Variable : Positive;
      Result   : Dimensionless;
   begin
      if not Solver.Limits_Valid
        or else Solver.Zero_Limit
        or else Solver.Unclassifiable
        or else not (Vs >= 0.0 and then D > 0.0)
      then
         return -1.0 * mm / s;
      end if;

      --  As in Optimal_Profile_For_Distance, saturate phases in order until the distance is exceeded, then solve
      --  the one phase time that is left free. Distance is convex and increasing in each phase time.
      if D > Travel (Full) then
         --  The constant-acceleration phase is free, and the distance is a quadratic in it, solved in the
         --  cancellation-free form.
         declare
            Peak_Accel : constant Dimensionless := Cm * T (1) * (T (1) + T (2)) * (2.0 * T (1) + T (2) + T (3));
            Ramp_Gain  : constant Dimensionless := 4.0 * T (1) + 2.0 * T (2) + T (3);
            Ramp_Time  : constant Dimensionless := 8.0 * T (1) + 4.0 * T (2) + 2.0 * T (3);
            Linear     : constant Dimensionless := Vs + Peak_Accel * (Ramp_Gain + Ramp_Time) / 2.0;
            Short      : constant Dimensionless := D - (Vs + Peak_Accel * Ramp_Gain / 2.0) * Ramp_Time;
         begin
            T (4) := 2.0 * Short / (Linear + Dimensionless_Math.Sqrt (Linear**2 + 2.0 * Peak_Accel * Short));
         end;
      else
         if Full (3) > 0.0 and then D > Travel ([Full (1), Full (2), 0.0, 0.0]) then
            Variable := 3;
         elsif Full (2) > 0.0 and then D > Travel ([Full (1), 0.0, 0.0, 0.0]) then
            Variable := 2;
         else
            Variable := 1;
         end if;
         for I in Variable .. 3 loop
            T (I) := 0.0;
         end loop;
         --  Newton's method from above the root decreases onto it. The saturated time is above it by the case
         --  selection, and so is the tangent at zero, which lies below the curve.
         declare
            Base  : constant Dimensionless := Travel (T);
            Slope : constant Dimensionless := Travel_Slope (T, Variable);
            X     : Dimensionless := Full (Variable);
            Next  : Dimensionless;
         begin
            if Slope > 0.0 then
               X := Dimensionless'Min (X, (D - Base) / Slope);
            end if;
            if Variable = 1 and then 32.0 * Cm * X**5 > D then
               --  From rest, or nearly, the fifth-power term decides the scale.
               X := Dimensionless'Min (X, Dimensionless_Math."**" (D / (32.0 * Cm), 0.2));
            end if;
            for Iteration in 1 .. 48 loop
               exit when not (X > 0.0);
               T (Variable) := X;
               Next := X - (Travel (T) - D) / Travel_Slope (T, Variable);
               exit when not (Next < X);
               X := Next;
            end loop;
            T (Variable) := Dimensionless'Max (0.0, X);
         end;
      end if;

      Result := Gain (T);
      if Result >= 0.0 and then Result <= Dimensionless'Last then
         return Result * (mm / s);
      else
         return -1.0 * mm / s;
      end if;
   exception
      when Constraint_Error | Ada.Numerics.Argument_Error =>
         return -1.0 * mm / s;
   end Delta_V_Over_Distance;

   function Optimal_Profile_For_Delta_V (Solver : Delta_V_Solver; Delta_V : Velocity) return Feedrate_Profile_Times is
   begin
      if Delta_V = 0.0 * mm / s and then Solver.Limits_Valid then
         --  Reachability certificates put the peak at one endpoint, so one ramp is usually empty.
         return [others => 0.0 * s];
      end if;
      return Optimal_Profile_For_Delta_V_Internal (Solver, Delta_V).Profile;
   end Optimal_Profile_For_Delta_V;

   function Optimal_Profile_For_Delta_V
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
      return Feedrate_Profile_Times is
   begin
      if Delta_V = 0.0 * mm / s
        and then Derivative_Limits_Are_Valid (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max)
      then
         --  Reachability certificates put the peak at one endpoint, so one ramp is usually empty.
         return [others => 0.0 * s];
      end if;
      return Optimal_Profile_For_Delta_V_Internal (Delta_V, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max).Profile;
   end Optimal_Profile_For_Delta_V;

   function Try_Optimal_Full_Profile
     (Start_Vel        : Velocity;
      Max_Vel          : Velocity;
      End_Vel          : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Full_Profile_Result
   is
      Profile                        : Feedrate_Profile;
      Solver                         : Delta_V_Solver;
      Direct_Distance                : Length;
      --  The distance of the ramp straight from Start_Vel to End_Vel.
      Accel_Distance, Decel_Distance : Length;

      procedure Set_Peak (Peak : Velocity);
      --  Form both ramps for a profile that peaks at Peak, and their distances.

      procedure Set_Peak (Peak : Velocity) is
      begin
         Profile.Accel := Optimal_Profile_For_Delta_V (Solver, Start_Vel - Peak);
         Profile.Decel :=
           (if Start_Vel = End_Vel then Profile.Accel else Optimal_Profile_For_Delta_V (Solver, End_Vel - Peak));
         Accel_Distance := Fast_Distance_At_Max_Time (Profile.Accel, Crackle_Max, Start_Vel);
         Decel_Distance := Fast_Distance_At_Max_Time (Profile.Decel, -Crackle_Max, Peak);
      end Set_Peak;
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
         return (Reachable => True, Profile => <>);
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
         return (Reachable => True, Profile => Profile);
      end if;

      if Max_Vel = 0.0 * mm / s then
         raise Constraint_Error with "Positive distance requires positive velocity.";
      end if;

      Solver := Prepare_Delta_V (Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);

      declare
         Profile : constant Feedrate_Profile_Times := Optimal_Profile_For_Delta_V (Solver, Start_Vel - End_Vel);
      begin
         Direct_Distance :=
           Fast_Distance_At_Max_Time
             (Profile,
              (if Start_Vel < End_Vel then Crackle_Max / (mm / s**5) else -Crackle_Max / (mm / s**5))
              * (mm / s**5), --  Temporarily drop the dimension otherwise gnatcov breaks it.
              Start_Vel);
         if Distance < Direct_Distance then
            return (Reachable => False, Profile => <>);
         end if;
      end;

      Set_Peak (Max_Vel);

      if Accel_Distance + Decel_Distance <= Distance then
         Profile.Coast := (Distance - Accel_Distance - Decel_Distance) / Max_Vel;
      else
         Profile.Coast := 0.0 * s;
         declare
            function Excess (Peak : Dimensionless) return Dimensionless;
            --  How far the ramps to and from Peak overrun Distance.

            function Excess (Peak : Dimensionless) return Dimensionless is
            begin
               Set_Peak (Peak * (mm / s));
               return (Accel_Distance + Decel_Distance - Distance) / mm;
            end Excess;

            function Highest_Peak is new Largest_Nonpositive (Excess);

            Peak_Tolerance : constant Dimensionless := 2.0**(-40);
            --  How closely the search brackets the highest peak, as a fraction of it. The duration is not stationary
            --  at that peak: a peak this fraction below it lengthens the profile by at most the same fraction, and
            --  for a shortfall this small by at most about 3/8 of it, so about 3.4E-13 of the duration. Settling
            --  the last machine number as well would take most of the evaluations of the search.

            --  The ramp distances grow with the peak, so the fastest profile has the highest peak whose ramps fit
            --  in Distance. Whatever distance the peak found leaves over is coasted. The ramps for the lowest peak
            --  are the direct ramp checked above, which fits.
            --
            --  Between equal speeds the two ramps mirror each other, so each covers half of Distance and the
            --  peak is the speed that one ramp gains over that half. That estimate only steers the search.
            Symmetric_Gain : constant Velocity :=
              (if Start_Vel = End_Vel
               then Delta_V_Over_Distance (Solver, Start_Vel, Distance / 2.0)
               else -1.0 * mm / s);
            Lower          : constant Velocity :=
              Highest_Peak
                (Lower              => Velocity'Max (Start_Vel, End_Vel) / (mm / s),
                 Upper              => Max_Vel / (mm / s),
                 Lower_Residual     => (Direct_Distance - Distance) / mm,
                 Upper_Residual     => (Accel_Distance + Decel_Distance - Distance) / mm,
                 Relative_Tolerance => Peak_Tolerance,
                 Guess              =>
                   (if Symmetric_Gain >= 0.0 * mm / s then (Start_Vel + Symmetric_Gain) / (mm / s) else -1.0),
                 Use_Model          => Symmetric_Gain < 0.0 * mm / s)
              * (mm / s);
         begin
            Set_Peak (Lower);

            if Lower > 0.0 * mm / s and then Accel_Distance + Decel_Distance < Distance then
               Profile.Coast := (Distance - Accel_Distance - Decel_Distance) / Lower;
            end if;
         end;
      end if;

      if not Profile_Is_Valid (Profile, Distance, Start_Vel, End_Vel, Crackle_Max) then
         raise Constraint_Error with "The generated profile cannot represent the requested motion.";
      end if;
      return (Reachable => True, Profile => Profile);
   end Try_Optimal_Full_Profile;

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
      Result : constant Full_Profile_Result :=
        Try_Optimal_Full_Profile
          (Start_Vel, Max_Vel, End_Vel, Distance, Acceleration_Max, Jerk_Max, Snap_Max, Crackle_Max);
   begin
      if not Result.Reachable then
         raise Constraint_Error with "End_Vel is not reachable under given constraints.";
      end if;
      return Result.Profile;
   end Optimal_Full_Profile;

end Prunt.Motion_Planner;
