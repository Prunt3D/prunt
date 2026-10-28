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

with Ada.Numerics;

package body Prunt.Motion_Planner.Corner_Transitions is

   use type Stereographic_Curves.Blend_Result_Kind;

   Epsilon : constant Dimensionless := 256.0 * Dimensionless'Model_Epsilon;

   function Finite (X : Dimensionless) return Boolean
   is (X >= -Dimensionless'Last and then X <= Dimensionless'Last);

   function Finite_Length (X : Length) return Boolean
   is (X >= Length'First and then X <= Length'Last);

   function Dot (A, B : Position_Scale) return Dimensionless is
      R : Dimensionless := 0.0;
   begin
      for Axis in Axis_Name loop
         R := R + A (Axis) * B (Axis);
      end loop;
      return R;
   end Dot;

   function Norm (A : Position_Scale) return Dimensionless is
   begin
      return abs A;
   exception
      when Constraint_Error =>
         return Dimensionless'Last;
   end Norm;

   function Unit (A : Position_Scale; Good : out Boolean) return Position_Scale is
      N : constant Dimensionless := Norm (A);
   begin
      Good := N > 1.0E-14 and then Finite (N);
      return (if Good then A / N else [others => 0.0]);
   end Unit;

   function Unit_Offset (A : Position_Offset; Good : out Boolean) return Position_Scale is
      N : constant Length := abs A;
   begin
      Good := N > 1.0E-12 * mm and then Finite (Dimensionless (N / mm));
      return (if Good then A / N else [others => 0.0]);
   end Unit_Offset;

   function Arc_Constant_Axes (Arc : Arc_Data) return Structural_Axes is
      R : Structural_Axes;
   begin
      for Axis in Axis_Name loop
         R (Axis) := Arc.Radial_Start (Axis) = 0.0 and then Arc.Tangent_Start (Axis) = 0.0;
      end loop;
      return R;
   end Arc_Constant_Axes;

   function Biarc_Constant_Axes (Data : Biarc_Data) return Structural_Axes is
      First  : constant Structural_Axes := Arc_Constant_Axes (Data.First);
      Second : constant Structural_Axes := Arc_Constant_Axes (Data.Second);
      R      : Structural_Axes;
   begin
      for Axis in Axis_Name loop
         R (Axis) := First (Axis) and then Second (Axis) and then Data.First.Centre (Axis) = Data.Second.Centre (Axis);
      end loop;
      return R;
   end Biarc_Constant_Axes;

   function Point_Envelope (P : Position) return Position_Envelope
   is ([for Axis in Axis_Name => (Lower => P (Axis), Upper => P (Axis))]);

   function Union (A, B : Position_Envelope) return Position_Envelope is
      R : Position_Envelope;
   begin
      for Axis in Axis_Name loop
         R (Axis) :=
           (Lower => Length'Min (A (Axis).Lower, B (Axis).Lower),
            Upper => Length'Max (A (Axis).Upper, B (Axis).Upper));
      end loop;
      return R;
   end Union;

   function Phase_In_Range (Phase, Low, High : Dimensionless) return Boolean is
   begin
      for Turn in -2 .. 2 loop
         declare
            Candidate : constant Dimensionless := Phase + Dimensionless (Turn) * 2.0 * Ada.Numerics.Pi;
         begin
            if Candidate >= Low and then Candidate <= High then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Phase_In_Range;

   function Trig_Absolute_Maximum (Cos_Coefficient, Sin_Coefficient, Low, High : Dimensionless) return Dimensionless is
      At_Low    : constant Dimensionless := abs (Cos_Coefficient * Math.Cos (Low) + Sin_Coefficient * Math.Sin (Low));
      At_High   : constant Dimensionless :=
        abs (Cos_Coefficient * Math.Cos (High) + Sin_Coefficient * Math.Sin (High));
      Amplitude : constant Dimensionless :=
        Math.Sqrt (Dimensionless'Max (0.0, Cos_Coefficient**2 + Sin_Coefficient**2));
      Phase     : Dimensionless := 0.0;
   begin
      if Amplitude > 0.0 then
         Phase := Math.Arctan (Sin_Coefficient, Cos_Coefficient);
      end if;
      return
        (if Phase_In_Range (Phase, Low, High) or else Phase_In_Range (Phase + Ada.Numerics.Pi, Low, High)
         then Amplitude
         else Dimensionless'Max (At_Low, At_High));
   end Trig_Absolute_Maximum;

   function Arc_Envelope (Arc : Arc_Data) return Position_Envelope is
   begin
      return Arc_Envelope (Arc, 0.0 * mm, Arc.Length_Value);
   end Arc_Envelope;

   function Arc_Envelope (Arc : Arc_Data; Start_Distance, End_Distance : Length) return Position_Envelope is
      R          : Position_Envelope;
      Theta_Low  : constant Dimensionless :=
        (if Arc.Length_Value > 0.0 * mm then Arc.Sweep * Start_Distance / Arc.Length_Value else 0.0);
      Theta_High : constant Dimensionless :=
        (if Arc.Length_Value > 0.0 * mm then Arc.Sweep * End_Distance / Arc.Length_Value else 0.0);
   begin
      for Axis in Axis_Name loop
         declare
            A          : constant Dimensionless := Arc.Radial_Start (Axis);
            B          : constant Dimensionless := Arc.Tangent_Start (Axis);
            Low_Value  : constant Dimensionless := A * Math.Cos (Theta_Low) + B * Math.Sin (Theta_Low);
            High_Value : constant Dimensionless := A * Math.Cos (Theta_High) + B * Math.Sin (Theta_High);
            Minimum    : Dimensionless := Dimensionless'Min (Low_Value, High_Value);
            Maximum    : Dimensionless := Dimensionless'Max (Low_Value, High_Value);
            Amplitude  : constant Dimensionless := Math.Sqrt (Dimensionless'Max (0.0, A**2 + B**2));
            Phase      : Dimensionless := 0.0;
         begin
            if Amplitude > 0.0 then
               Phase := Math.Arctan (B, A);
               if Phase_In_Range (Phase, Theta_Low, Theta_High) then
                  Maximum := Amplitude;
               end if;
               if Phase_In_Range (Phase + Ada.Numerics.Pi, Theta_Low, Theta_High) then
                  Minimum := -Amplitude;
               end if;
            end if;
            R (Axis) :=
              (Lower => Arc.Centre (Axis) + Arc.Radius * Minimum, Upper => Arc.Centre (Axis) + Arc.Radius * Maximum);
         end;
      end loop;
      return R;
   end Arc_Envelope;

   function Arc_Bounds (Arc : Arc_Data) return Unit_Speed_Axial_Derivative_Bounds is
   begin
      return Arc_Bounds (Arc, 0.0 * mm, Arc.Length_Value);
   end Arc_Bounds;

   function Arc_Bounds
     (Arc : Arc_Data; Start_Distance, End_Distance : Length) return Unit_Speed_Axial_Derivative_Bounds
   is
      R          : Unit_Speed_Axial_Derivative_Bounds := (others => <>);
      K          : Curvature;
      Theta_Low  : constant Dimensionless :=
        (if Arc.Length_Value > 0.0 * mm then Arc.Sweep * Start_Distance / Arc.Length_Value else 0.0);
      Theta_High : constant Dimensionless :=
        (if Arc.Length_Value > 0.0 * mm then Arc.Sweep * End_Distance / Arc.Length_Value else 0.0);
   begin
      if Arc.Radius <= 0.0 * mm then
         return R;
      end if;
      K := Curvature'Adjacent (1.0 / Arc.Radius, Curvature'Last);
      for Axis in Axis_Name loop
         declare
            Tangent_Max : constant Dimensionless :=
              Dimensionless'Min
                (1.0,
                 Dimensionless'Adjacent
                   (Trig_Absolute_Maximum (Arc.Tangent_Start (Axis), -Arc.Radial_Start (Axis), Theta_Low, Theta_High),
                    Dimensionless'Last));
            Radial_Max  : constant Dimensionless :=
              Dimensionless'Min
                (1.0,
                 Dimensionless'Adjacent
                   (Trig_Absolute_Maximum (Arc.Radial_Start (Axis), Arc.Tangent_Start (Axis), Theta_Low, Theta_High),
                    Dimensionless'Last));
         begin
            R.Velocity (Axis) := Tangent_Max;
            R.Acceleration (Axis) := Curvature'Adjacent (Radial_Max * K, Curvature'Last);
            R.Jerk (Axis) := Curvature_To_2'Adjacent (Tangent_Max * K**2, Curvature_To_2'Last);
            R.Snap (Axis) := Curvature_To_3'Adjacent (Radial_Max * K**3, Curvature_To_3'Last);
            R.Crackle (Axis) := Curvature_To_4'Adjacent (Tangent_Max * K**4, Curvature_To_4'Last);
         end;
      end loop;
      return R;
   end Arc_Bounds;

   function Merge_Bounds (A, B : Unit_Speed_Axial_Derivative_Bounds) return Unit_Speed_Axial_Derivative_Bounds is
      R : Unit_Speed_Axial_Derivative_Bounds;
   begin
      for Axis in Axis_Name loop
         R.Velocity (Axis) := Dimensionless'Max (A.Velocity (Axis), B.Velocity (Axis));
         R.Acceleration (Axis) := Curvature'Max (A.Acceleration (Axis), B.Acceleration (Axis));
         R.Jerk (Axis) := Curvature_To_2'Max (A.Jerk (Axis), B.Jerk (Axis));
         R.Snap (Axis) := Curvature_To_3'Max (A.Snap (Axis), B.Snap (Axis));
         R.Crackle (Axis) := Curvature_To_4'Max (A.Crackle (Axis), B.Crackle (Axis));
      end loop;
      return R;
   end Merge_Bounds;

   function Arc_Point (Arc : Arc_Data; Distance : Length) return Position is
      Theta : constant Dimensionless :=
        (if Arc.Length_Value = 0.0 * mm then 0.0 else Arc.Sweep * Distance / Arc.Length_Value);
      C     : constant Dimensionless := Math.Cos (Theta);
      Sine  : constant Dimensionless := Math.Sin (Theta);
      R     : Position;
   begin
      for Axis in Axis_Name loop
         R (Axis) :=
           (if Arc.Radial_Start (Axis) = 0.0 and then Arc.Tangent_Start (Axis) = 0.0
            then Arc.Centre (Axis)
            else Arc.Centre (Axis) + Arc.Radius * (Arc.Radial_Start (Axis) * C + Arc.Tangent_Start (Axis) * Sine));
      end loop;
      return R;
   end Arc_Point;

   procedure Arc_From_Start
     (Start_Point, Finish_Point : Position; Start_Tangent : Position_Scale; Arc : out Arc_Data; Good : out Boolean)
   is
      T_Good                      : Boolean;
      T                           : constant Position_Scale := Unit (Start_Tangent, T_Good);
      D                           : constant Position_Offset := Finish_Point - Start_Point;
      Chord                       : constant Length := abs D;
      Along                       : Length := 0.0 * mm;
      Perp                        : Position_Offset;
      Perp_Length                 : Length;
      N                           : Position_Scale;
      N_Good                      : Boolean;
      Radius                      : Length;
      End_Radial                  : Position_Scale;
      Cos_Sweep, Sin_Sweep, Sweep : Dimensionless;
   begin
      Arc := (others => <>);
      Good := False;
      if not T_Good or else Chord <= 1.0E-12 * mm then
         return;
      end if;
      for Axis in Axis_Name loop
         Along := Along + D (Axis) * T (Axis);
      end loop;
      Perp := D - T * Along;
      Perp_Length := abs Perp;
      if Perp_Length <= Chord * 1.0E-10 then
         return;
      end if;
      N := Unit_Offset (Perp, N_Good);
      if not N_Good then
         return;
      end if;
      Radius := Chord**2 / (2.0 * Perp_Length);
      if Radius <= 1.0E-12 * mm or else not Finite (Dimensionless (Radius / mm)) then
         return;
      end if;
      Arc.Centre := Start_Point + N * Radius;
      Arc.Radial_Start := N * (-1.0);
      Arc.Tangent_Start := T;
      Arc.Radius := Radius;
      End_Radial := (Finish_Point - Arc.Centre) / Radius;
      Cos_Sweep := Dot (Arc.Radial_Start, End_Radial);
      Sin_Sweep := Dot (Arc.Tangent_Start, End_Radial);
      Sweep := Math.Arctan (Sin_Sweep, Cos_Sweep);
      if Sweep <= 1.0E-10 then
         Sweep := Sweep + 2.0 * Ada.Numerics.Pi;
      end if;
      if Sweep <= 1.0E-10 or else Sweep > Ada.Numerics.Pi + 1.0E-10 then
         return;
      end if;
      Arc.Sweep := Sweep;
      Arc.Length_Value := Radius * Sweep;
      Good := Finite (Dimensionless (Arc.Length_Value / mm));
   exception
      when Constraint_Error =>
         Arc := (others => <>);
         Good := False;
   end Arc_From_Start;

   function Stop_At (Point : Position) return Corner_Transition is
   begin
      return
        (Kind_Value    => Hard_Stop_Transition,
         Point         => Point,
         Bounds        => (others => <>),
         Envelope      => Point_Envelope (Point),
         Error         => 0.0 * mm,
         Constant_Axes => [others => True],
         SCV_Limit     => 0.0 * mm / s);
   end Stop_At;

   function Passthrough_At (Point : Position) return Corner_Transition is
   begin
      return
        (Kind_Value    => Passthrough_Transition,
         Point         => Point,
         Bounds        => (others => <>),
         Envelope      => Point_Envelope (Point),
         Error         => 0.0 * mm,
         Constant_Axes => [others => True],
         SCV_Limit     => Velocity'Last);
   end Passthrough_At;

   function Sharp_At (Point : Position; Velocity_Limit : Velocity) return Corner_Transition is
   begin
      if Velocity_Limit < 0.0 * mm / s or else not Finite (Dimensionless (Velocity_Limit / (mm / s))) then
         return Stop_At (Point);
      end if;
      return
        (Kind_Value    => Sharp_SCV_Transition,
         Point         => Point,
         Bounds        => (others => <>),
         Envelope      => Point_Envelope (Point),
         Error         => 0.0 * mm,
         Constant_Axes => [others => True],
         SCV_Limit     => Velocity_Limit);
   end Sharp_At;

   function From_Stereographic (Curve : Stereographic_Curves.Stereographic_Curve) return Corner_Transition is
      L     : constant Length := Stereographic_Curves.Arc_Length (Curve);
      E     : constant Length := Stereographic_Curves.Position_Error_Bound (Curve);
      Start : constant Position := Stereographic_Curves.Point_At_Distance (Curve, 0.0 * mm);
      Env   : Position_Envelope;
      C     : Structural_Axes;
   begin
      for Axis in Axis_Name loop
         Env (Axis) := (Lower => Start (Axis) - L, Upper => Start (Axis) + L);
         C (Axis) := Stereographic_Curves.Axis_Is_Structurally_Constant (Curve, Axis);
         if C (Axis) then
            Env (Axis) := (Lower => Start (Axis), Upper => Start (Axis));
         end if;
      end loop;
      return
        (Kind_Value    => Stereographic_Transition,
         Stereo        => Curve,
         Bounds        => Stereographic_Curves.Derivative_Bounds (Curve),
         Envelope      => Env,
         Error         => E,
         Constant_Axes => C,
         SCV_Limit     => Velocity'Last);
   end From_Stereographic;

   function Create_Stereographic (Request : Stereographic_Curves.Blend_Request) return Construction_Result is
      R : constant Stereographic_Curves.Blend_Result := Stereographic_Curves.Create_Blend (Request);
   begin
      if R.Kind = Stereographic_Curves.Blend_Success then
         return (Status => Construction_Success, Transition => From_Stereographic (R.Curve));
      else
         return (Status => Stereographic_Construction_Failed, Transition => <>);
      end if;
   end Create_Stereographic;

   function Create_Circular
     (Start_Point, Commanded_Corner, Finish_Point : Position; Maximum_Radius : Length := 1.0E100 * mm)
      return Construction_Result
   is
      In_Offset           : constant Position_Offset := Commanded_Corner - Start_Point;
      Out_Offset          : constant Position_Offset := Finish_Point - Commanded_Corner;
      In_Length           : constant Length := abs In_Offset;
      Out_Length          : constant Length := abs Out_Offset;
      Good_In, Good_Out   : Boolean;
      T0                  : constant Position_Scale := Unit_Offset (In_Offset, Good_In);
      T1                  : constant Position_Scale := Unit_Offset (Out_Offset, Good_Out);
      D, Theta, Radius    : Length;
      Dot_Value, Sin_Half : Dimensionless;
      N, U                : Position_Scale;
      Arc                 : Arc_Data;
      Env                 : Position_Envelope;
   begin
      if not Good_In or else not Good_Out or else Maximum_Radius <= 0.0 * mm then
         return (Status => Invalid_Input, Transition => <>);
      end if;
      if abs (In_Length - Out_Length) > Length'Max (1.0E-9 * mm, 1.0E-9 * Length'Max (In_Length, Out_Length)) then
         return (Status => Unsupported_Geometry, Transition => <>);
      end if;
      D := 0.5 * (In_Length + Out_Length);
      Dot_Value := Dimensionless'Max (-1.0, Dimensionless'Min (1.0, Dot (T0, T1)));
      if Dot_Value >= 1.0 - 1.0E-12 or else Dot_Value <= -1.0 + 1.0E-12 then
         return (Status => Unsupported_Geometry, Transition => <>);
      end if;
      Sin_Half := Math.Sqrt (Dimensionless'Max (0.0, 0.5 * (1.0 - Dot_Value)));
      Radius := D * Math.Sqrt (0.5 * (1.0 + Dot_Value)) / Sin_Half;
      if Radius > Maximum_Radius then
         return (Status => Radius_Limit_Exceeded, Transition => <>);
      end if;
      Theta := Math.Arctan (Sin_Half, Math.Sqrt (0.5 * (1.0 + Dot_Value))) * 2.0 * mm;
      N := (T1 - T0 * Dot_Value) / Math.Sqrt (Dimensionless'Max (Epsilon, 1.0 - Dot_Value**2));
      U := N * (-1.0);
      Arc :=
        (Centre        => Start_Point + N * Radius,
         Radial_Start  => U,
         Tangent_Start => T0,
         Radius        => Radius,
         Sweep         => Theta / mm,
         Length_Value  => Radius * (Theta / mm));
      if abs (Arc_Point (Arc, Arc.Length_Value) - Finish_Point) > 1.0E-7 * mm then
         return (Status => Numerically_Unsafe, Transition => <>);
      end if;
      Env := Arc_Envelope (Arc);
      return
        (Status     => Construction_Success,
         Transition =>
           (Kind_Value    => Circular_Transition,
            Circle        => Arc,
            Bounds        => Arc_Bounds (Arc),
            Envelope      => Env,
            Error         => Epsilon * (Radius + D),
            Constant_Axes => Arc_Constant_Axes (Arc),
            SCV_Limit     => Velocity'Last));
   exception
      when Constraint_Error =>
         return (Status => Numerically_Unsafe, Transition => <>);
   end Create_Circular;

   function Reverse_Arc (Arc : Arc_Data) return Arc_Data is
      C            : constant Dimensionless := Math.Cos (Arc.Sweep);
      Sine         : constant Dimensionless := Math.Sin (Arc.Sweep);
      U_End, T_End : Position_Scale;
   begin
      for Axis in Axis_Name loop
         U_End (Axis) := Arc.Radial_Start (Axis) * C + Arc.Tangent_Start (Axis) * Sine;
         T_End (Axis) := -Arc.Radial_Start (Axis) * Sine + Arc.Tangent_Start (Axis) * C;
      end loop;
      return
        (Centre        => Arc.Centre,
         Radial_Start  => U_End,
         Tangent_Start => T_End * (-1.0),
         Radius        => Arc.Radius,
         Sweep         => Arc.Sweep,
         Length_Value  => Arc.Length_Value);
   end Reverse_Arc;

   function Arc_Has_Nonnegative_Progress (Arc : Arc_Data; Direction : Position_Scale) return Boolean is
      A             : constant Dimensionless := Dot (Arc.Tangent_Start, Direction);
      B             : constant Dimensionless := -Dot (Arc.Radial_Start, Direction);
      Finish_Value  : constant Dimensionless := A * Math.Cos (Arc.Sweep) + B * Math.Sin (Arc.Sweep);
      Minimum_Phase : Dimensionless := Math.Arctan (-B, -A);
      Tolerance     : constant Dimensionless := 1.0E-12;
   begin
      if Minimum_Phase < 0.0 then
         Minimum_Phase := Minimum_Phase + 2.0 * Ada.Numerics.Pi;
      end if;
      --  Tangential progress is A*cos(theta)+B*sin(theta).  Endpoints alone are insufficient for a semicircle, so
      --  explicitly reject an interior phase attaining the negative amplitude.  Arc_From_Start already limits the
      --  sweep to pi; together these checks prove the subarc never runs backward along the endpoint chord.
      return
        A >= -Tolerance
        and then Finish_Value >= -Tolerance
        and then not (Minimum_Phase > Tolerance and then Minimum_Phase < Arc.Sweep - Tolerance);
   end Arc_Has_Nonnegative_Progress;

   function Biarc_Envelope (Data : Biarc_Data; Start_Distance, End_Distance : Length) return Position_Envelope is
      Split : constant Length := Data.First.Length_Value;
   begin
      if End_Distance <= Split then
         return Arc_Envelope (Data.First, Start_Distance, End_Distance);
      elsif Start_Distance >= Split then
         return Arc_Envelope (Data.Second, Start_Distance - Split, End_Distance - Split);
      else
         return
           Union
             (Arc_Envelope (Data.First, Start_Distance, Split),
              Arc_Envelope (Data.Second, 0.0 * mm, End_Distance - Split));
      end if;
   end Biarc_Envelope;

   function Biarc_Bounds
     (Data : Biarc_Data; Start_Distance, End_Distance : Length) return Unit_Speed_Axial_Derivative_Bounds
   is
      Split : constant Length := Data.First.Length_Value;
   begin
      if End_Distance <= Split then
         return Arc_Bounds (Data.First, Start_Distance, End_Distance);
      elsif Start_Distance >= Split then
         return Arc_Bounds (Data.Second, Start_Distance - Split, End_Distance - Split);
      else
         return
           Merge_Bounds
             (Arc_Bounds (Data.First, Start_Distance, Split),
              Arc_Bounds (Data.Second, 0.0 * mm, End_Distance - Split));
      end if;
   end Biarc_Bounds;

   function Create_Biarc
     (Start_Point, Finish_Point     : Position;
      Start_Tangent, Finish_Tangent : Position_Scale;
      Maximum_Length                : Length := 1.0E100 * mm;
      Preferred_Trim_Ratio          : Dimensionless := 1.0) return Construction_Result
   is
      G0, G1          : Boolean;
      T0              : constant Position_Scale := Unit (Start_Tangent, G0);
      T1              : constant Position_Scale := Unit (Finish_Tangent, G1);
      V               : constant Position_Offset := Finish_Point - Start_Point;
      VV              : Dimensionless := 0.0;
      TT              : Dimensionless;
      Chord_Length    : Length;
      Chord_Direction : Position_Scale;
      type Candidate is record
         Valid                                          : Boolean := False;
         Data                                           : Biarc_Data;
         Peak_Curvature, Curvature_Jump, Ratio_Distance : Dimensionless := Dimensionless'Last;
      end record;
      Best            : Candidate;
      Env             : Position_Envelope;
      Bounds          : Unit_Speed_Axial_Derivative_Bounds;

      function Better (Left, Right : Candidate) return Boolean;
      function Evaluate (Ratio : Dimensionless) return Candidate;

      function Better (Left, Right : Candidate) return Boolean is
         --  Radii are formed from cancellation-prone chord projections, so their rounding noise far exceeds a few
         --  ulps. When the endpoints are cocircular every member lies on the same circle and only that noise would
         --  separate them; treat smaller differences as ties so the preferred trim ratio decides.
         Slack    : constant Dimensionless := 1.0E-9;
         Jump_Tie : Dimensionless;
      begin
         if not Left.Valid then
            return False;
         elsif not Right.Valid then
            return True;
         elsif Left.Peak_Curvature < Right.Peak_Curvature * (1.0 - Slack) then
            return True;
         elsif Right.Peak_Curvature < Left.Peak_Curvature * (1.0 - Slack) then
            return False;
         end if;
         --  A curvature jump of zero has no relative scale, so compare jumps against the shared peak curvature.
         Jump_Tie := Slack * Dimensionless'Max (Left.Peak_Curvature, Right.Peak_Curvature);
         if Left.Curvature_Jump < Right.Curvature_Jump - Jump_Tie then
            return True;
         elsif Right.Curvature_Jump < Left.Curvature_Jump - Jump_Tie then
            return False;
         else
            return Left.Ratio_Distance < Right.Ratio_Distance;
         end if;
      end Better;

      function Evaluate (Ratio : Dimensionless) return Candidate is
         A, B, Discriminant, D0_Raw    : Dimensionless;
         D0, D1                        : Length;
         Q0, Q1, Join                  : Position;
         First, Reverse_Second, Second : Arc_Data;
         Good_First, Good_Second       : Boolean;
         Result                        : Candidate;
         First_End_Tangent             : Position_Scale;
         C                             : Dimensionless;
         Sine                          : Dimensionless;
      begin
         if Ratio < 1.0 / 20.0 or else Ratio > 20.0 then
            return Result;
         end if;
         --  With D1 = Ratio * D0, the arcs share a tangent at the join exactly when |Q1 - Q0| = D0 + D1, i.e.
         --
         --     2·Ratio·(1 - T0·T1)·D0² + 2·V·(T0 + Ratio·T1)·D0 - V·V = 0.
         A := 2.0 * Ratio * TT;
         B := 0.0;
         for Axis in Axis_Name loop
            B := B + 2.0 * Dimensionless (V (Axis) / mm) * (T0 (Axis) + Ratio * T1 (Axis));
         end loop;
         Discriminant := B**2 + 4.0 * A * VV;
         if A <= 1.0E-14 or else Discriminant <= 0.0 then
            return Result;
         end if;
         D0_Raw := (-B + Math.Sqrt (Discriminant)) / (2.0 * A);
         if D0_Raw <= 1.0E-12 then
            return Result;
         end if;
         D0 := D0_Raw * mm;
         D1 := Ratio * D0;
         Q0 := Start_Point + T0 * D0;
         Q1 := Finish_Point - T1 * D1;
         for Axis in Axis_Name loop
            --  A rounded weighted average of identical coordinates can leave their plane. That would give both
            --  arcs a spurious displacement and prevent structural constant-axis detection.
            Join (Axis) :=
              (if Q0 (Axis) = Q1 (Axis) then Q0 (Axis) else (Ratio * Q0 (Axis) + Q1 (Axis)) / (Ratio + 1.0));
         end loop;
         Arc_From_Start (Start_Point, Join, T0, First, Good_First);
         Arc_From_Start (Finish_Point, Join, T1 * (-1.0), Reverse_Second, Good_Second);
         if not Good_First or else not Good_Second then
            return Result;
         end if;
         Second := Reverse_Arc (Reverse_Second);
         C := Math.Cos (First.Sweep);
         Sine := Math.Sin (First.Sweep);
         for Axis in Axis_Name loop
            First_End_Tangent (Axis) := -First.Radial_Start (Axis) * Sine + First.Tangent_Start (Axis) * C;
         end loop;
         if Dot (First_End_Tangent, Second.Tangent_Start) < 1.0 - 1.0E-7 then
            return Result;
         end if;
         if not Arc_Has_Nonnegative_Progress (First, Chord_Direction)
           or else not Arc_Has_Nonnegative_Progress (Second, Chord_Direction)
         then
            --  Reject backward or externally bulging members of the standard family.  The original corner path is
            --  retained by the caller when no searched ratio passes this certificate.
            return Result;
         end if;
         Result.Data := (First => First, Second => Second, Length_Value => First.Length_Value + Second.Length_Value);
         if Result.Data.Length_Value > Maximum_Length or else Result.Data.Length_Value > 1_000.0 * Chord_Length then
            return (others => <>);
         end if;
         Result.Peak_Curvature :=
           Dimensionless'Max (Dimensionless (mm / First.Radius), Dimensionless (mm / Second.Radius));
         Result.Curvature_Jump := abs (Dimensionless (mm / First.Radius) - Dimensionless (mm / Second.Radius));
         Result.Ratio_Distance := abs Math.Log (Ratio / Preferred_Trim_Ratio);
         Result.Valid := True;
         return Result;
      exception
         when Constraint_Error =>
            return (others => <>);
      end Evaluate;
   begin
      if not G0
        or else not G1
        or else Maximum_Length <= 0.0 * mm
        or else not Finite (Preferred_Trim_Ratio)
        or else Preferred_Trim_Ratio < 1.0 / 20.0
        or else Preferred_Trim_Ratio > 20.0
      then
         return (Status => Invalid_Input, Transition => <>);
      end if;
      for Axis in Axis_Name loop
         VV := VV + Dimensionless ((V (Axis) / mm)**2);
      end loop;
      TT := 1.0 - Dot (T0, T1);
      if VV <= 1.0E-20 or else TT <= 1.0E-12 then
         return (Status => Unsupported_Geometry, Transition => <>);
      end if;
      Chord_Length := abs V;
      Chord_Direction := V / Chord_Length;
      --  Search every one of 32 log-space intervals.  Each interval receives a fixed 32-step golden refinement;
      --  infeasible samples simply compare worse than feasible ones.  This bounds construction time independently of
      --  geometry and catches every feasible sampled local-minimum bracket.
      for Interval_Index in 0 .. 31 loop
         declare
            Log_Lower : constant Dimensionless :=
              Math.Log (1.0 / 20.0) + Dimensionless (Interval_Index) * Math.Log (400.0) / 32.0;
            Log_Upper : constant Dimensionless :=
              Math.Log (1.0 / 20.0) + Dimensionless (Interval_Index + 1) * Math.Log (400.0) / 32.0;
            Left      : Dimensionless := Log_Lower;
            Right     : Dimensionless := Log_Upper;
            Phi       : constant Dimensionless := 0.5 * (Math.Sqrt (5.0) - 1.0);
            X1, X2    : Dimensionless;
            C1, C2    : Candidate;
         begin
            X1 := Right - Phi * (Right - Left);
            X2 := Left + Phi * (Right - Left);
            C1 := Evaluate (Math.Exp (X1));
            C2 := Evaluate (Math.Exp (X2));
            for Iteration in 1 .. 32 loop
               if Better (C1, Best) then
                  Best := C1;
               end if;
               if Better (C2, Best) then
                  Best := C2;
               end if;
               exit when Iteration = 32;
               --  Golden-section refinement retains one interior point. Reuse its fully checked candidate;
               --  reconstructing both arcs at both points doubles the dominant construction work.
               if Better (C1, C2) then
                  Right := X2;
                  X2 := X1;
                  C2 := C1;
                  X1 := Right - Phi * (Right - Left);
                  C1 := Evaluate (Math.Exp (X1));
               else
                  Left := X1;
                  X1 := X2;
                  C1 := C2;
                  X2 := Left + Phi * (Right - Left);
                  C2 := Evaluate (Math.Exp (X2));
               end if;
            end loop;
         end;
      end loop;
      if not Best.Valid then
         return (Status => Unsupported_Geometry, Transition => <>);
      end if;
      Env := Union (Arc_Envelope (Best.Data.First), Arc_Envelope (Best.Data.Second));
      Bounds := Merge_Bounds (Arc_Bounds (Best.Data.First), Arc_Bounds (Best.Data.Second));
      return
        (Status     => Construction_Success,
         Transition =>
           (Kind_Value    => Biarc_Transition,
            Two_Arcs      => Best.Data,
            Bounds        => Bounds,
            Envelope      => Env,
            Error         => Epsilon * (Best.Data.Length_Value + Best.Data.First.Radius + Best.Data.Second.Radius),
            Constant_Axes => Biarc_Constant_Axes (Best.Data),
            SCV_Limit     => Velocity'Last));
   exception
      when Constraint_Error =>
         return (Status => Numerically_Unsafe, Transition => <>);
   end Create_Biarc;

   function Compute_Sharp_SCV_Limit
     (Incoming_Tangent, Outgoing_Tangent : Position_Scale; Configured_SCV : Velocity; Ignore_E_In_XYZE : Boolean)
      return SCV_Result
   is
      A, B                 : Position_Scale := [others => 0.0];
      NA, NB               : Dimensionless := 0.0;
      Selected_Speed_Scale : Dimensionless := 1.0;
      Good_A, Good_B       : Boolean;
      D, C, Ratio          : Dimensionless;
   begin
      if Configured_SCV < 0.0 * mm / s or else not Finite (Dimensionless (Configured_SCV / (mm / s))) then
         return (Status => SCV_Invalid_Input, Velocity_Limit => 0.0 * mm / s);
      end if;
      if Ignore_E_In_XYZE then
         for Axis in Axis_Name loop
            if Axis /= E_Axis then
               A (Axis) := Incoming_Tangent (Axis);
               B (Axis) := Outgoing_Tangent (Axis);
            end if;
         end loop;
         NA := Norm (A);
         NB := Norm (B);
         if NA <= 1.0E-14 and then NB <= 1.0E-14 then
            A := Incoming_Tangent;
            B := Outgoing_Tangent;
         elsif (NA <= 1.0E-14) /= (NB <= 1.0E-14) then
            return (Status => SCV_Mixed_Pure_E, Velocity_Limit => 0.0 * mm / s);
         else
            --  The planner's scalar distance includes E, while SCV is expressed in the selected XYZ space.  Convert
            --  the selected-space cap back to full-path speed so a simultaneous extrusion component does not make a
            --  nominal 90-degree corner unnecessarily slower than the configured SCV.
            Selected_Speed_Scale := Dimensionless'Max (NA, NB);
         end if;
      else
         A := Incoming_Tangent;
         B := Outgoing_Tangent;
      end if;
      A := Unit (A, Good_A);
      B := Unit (B, Good_B);
      if not Good_A or else not Good_B then
         return (Status => SCV_Invalid_Input, Velocity_Limit => 0.0 * mm / s);
      end if;
      D := Dimensionless'Max (-1.0, Dimensionless'Min (1.0, Dot (A, B)));
      if D >= 1.0 - 1.0E-12 then
         return (Status => SCV_Passthrough, Velocity_Limit => Velocity'Last);
      elsif D <= -1.0 + 1.0E-12 then
         return (Status => SCV_Reversal_Stop, Velocity_Limit => 0.0 * mm / s);
      end if;
      C := Math.Sqrt (0.5 * (1.0 + D));
      Ratio := (Math.Sqrt (2.0) - 1.0) * C / (1.0 - C);
      return (Status => SCV_Success, Velocity_Limit => Configured_SCV * Math.Sqrt (Ratio) / Selected_Speed_Scale);
   exception
      when Constraint_Error =>
         return (Status => SCV_Invalid_Input, Velocity_Limit => 0.0 * mm / s);
   end Compute_Sharp_SCV_Limit;

   function Transition_Kind (Transition : Corner_Transition) return Corner_Transition_Kind
   is (Transition.Kind_Value);
   function Transition_Kind (Evaluator : Corner_Transition_Evaluator) return Corner_Transition_Kind
   is (Evaluator.Kind_Value);

   function Policy_For (Kind : Corner_Transition_Kind) return Junction_Policy_Kind is
   begin
      case Kind is
         when Hard_Stop_Transition   =>
            return Hard_Stop;

         when Passthrough_Transition =>
            return Passthrough;

         when Sharp_SCV_Transition   =>
            return Square_Corner_Velocity;

         when others                 =>
            return Derivative_Bounded;
      end case;
   end Policy_For;

   function Policy (Transition : Corner_Transition) return Junction_Policy_Kind is
   begin
      return Policy_For (Transition.Kind_Value);
   end Policy;

   function Policy (Evaluator : Corner_Transition_Evaluator) return Junction_Policy_Kind is
   begin
      return Policy_For (Evaluator.Kind_Value);
   end Policy;

   function Continuity_For (Kind : Corner_Transition_Kind) return Continuity_Metadata is
   begin
      case Kind is
         when Hard_Stop_Transition | Passthrough_Transition | Sharp_SCV_Transition =>
            return (0, False, 0);

         when Stereographic_Transition                                             =>
            return (4, False, 0);

         when Circular_Transition                                                  =>
            return (1, False, 0);

         when Biarc_Transition                                                     =>
            return (1, True, 1);
      end case;
   end Continuity_For;

   function Continuity (Transition : Corner_Transition) return Continuity_Metadata is
   begin
      return Continuity_For (Transition.Kind_Value);
   end Continuity;

   function Continuity (Evaluator : Corner_Transition_Evaluator) return Continuity_Metadata is
   begin
      return Continuity_For (Evaluator.Kind_Value);
   end Continuity;

   function Arc_Length (Transition : Corner_Transition) return Length is
   begin
      case Transition.Kind_Value is
         when Stereographic_Transition =>
            return Stereographic_Curves.Arc_Length (Transition.Stereo);

         when Circular_Transition      =>
            return Transition.Circle.Length_Value;

         when Biarc_Transition         =>
            return Transition.Two_Arcs.Length_Value;

         when others                   =>
            return 0.0 * mm;
      end case;
   end Arc_Length;

   function Arc_Length (Evaluator : Corner_Transition_Evaluator) return Length is
   begin
      case Evaluator.Kind_Value is
         when Stereographic_Transition =>
            return Stereographic_Curves.Arc_Length (Evaluator.Stereo);

         when Circular_Transition      =>
            return Evaluator.Circle.Length_Value;

         when Biarc_Transition         =>
            return Evaluator.Two_Arcs.Length_Value;

         when others                   =>
            return 0.0 * mm;
      end case;
   end Arc_Length;

   function Split_Distance (Transition : Corner_Transition) return Length is
   begin
      case Transition.Kind_Value is
         when Stereographic_Transition | Circular_Transition =>
            return Arc_Length (Transition) / 2.0;

         when Biarc_Transition                               =>
            return Transition.Two_Arcs.First.Length_Value;

         when others                                         =>
            return 0.0 * mm;
      end case;
   end Split_Distance;

   function Split_Distance (Evaluator : Corner_Transition_Evaluator) return Length is
   begin
      case Evaluator.Kind_Value is
         when Stereographic_Transition | Circular_Transition =>
            return Arc_Length (Evaluator) / 2.0;

         when Biarc_Transition                               =>
            return Evaluator.Two_Arcs.First.Length_Value;

         when others                                         =>
            return 0.0 * mm;
      end case;
   end Split_Distance;

   function Junction_Velocity_Limit (Transition : Corner_Transition) return Velocity
   is (Transition.SCV_Limit);
   function Junction_Velocity_Limit (Evaluator : Corner_Transition_Evaluator) return Velocity
   is (Evaluator.SCV_Limit);

   function Point_At_Distance (Transition : Corner_Transition; Distance : Length) return Position is
   begin
      case Transition.Kind_Value is
         when Stereographic_Transition =>
            return Stereographic_Curves.Point_At_Distance (Transition.Stereo, Distance);

         when Circular_Transition      =>
            return Arc_Point (Transition.Circle, Distance);

         when Biarc_Transition         =>
            if Distance <= Transition.Two_Arcs.First.Length_Value then
               return Arc_Point (Transition.Two_Arcs.First, Distance);
            else
               return Arc_Point (Transition.Two_Arcs.Second, Distance - Transition.Two_Arcs.First.Length_Value);
            end if;

         when others                   =>
            return Transition.Point;
      end case;
   end Point_At_Distance;

   function Point_At_Parameter (Transition : Corner_Transition; Parameter : Transition_Parameter) return Position is
   begin
      return Point_At_Distance (Transition, Arc_Length (Transition) * Parameter);
   end Point_At_Parameter;

   function Point_At_Parameter
     (Evaluator : Corner_Transition_Evaluator; Parameter : Transition_Parameter) return Position is
   begin
      return Point_At_Distance (Evaluator, Arc_Length (Evaluator) * Parameter);
   end Point_At_Parameter;

   function Point_At_Distance (Evaluator : Corner_Transition_Evaluator; Distance : Length) return Position is
   begin
      case Evaluator.Kind_Value is
         when Stereographic_Transition =>
            return Stereographic_Curves.Point_At_Distance (Evaluator.Stereo, Distance);

         when Circular_Transition      =>
            return Arc_Point (Evaluator.Circle, Distance);

         when Biarc_Transition         =>
            if Distance <= Evaluator.Two_Arcs.First.Length_Value then
               return Arc_Point (Evaluator.Two_Arcs.First, Distance);
            else
               return Arc_Point (Evaluator.Two_Arcs.Second, Distance - Evaluator.Two_Arcs.First.Length_Value);
            end if;

         when others                   =>
            return Evaluator.Point;
      end case;
   end Point_At_Distance;

   function Derivative_Bounds (Transition : Corner_Transition) return Unit_Speed_Axial_Derivative_Bounds
   is (Transition.Bounds);

   function Derivative_Bounds
     (Transition : Corner_Transition; Start_Distance, End_Distance : Length) return Unit_Speed_Axial_Derivative_Bounds
   is
   begin
      case Transition.Kind_Value is
         when Stereographic_Transition =>
            return Stereographic_Curves.Derivative_Bounds (Transition.Stereo, Start_Distance, End_Distance);

         when Circular_Transition      =>
            return Arc_Bounds (Transition.Circle, Start_Distance, End_Distance);

         when Biarc_Transition         =>
            return Biarc_Bounds (Transition.Two_Arcs, Start_Distance, End_Distance);

         when others                   =>
            return Transition.Bounds;
      end case;
   end Derivative_Bounds;

   function Derivative_Bounds (Evaluator : Corner_Transition_Evaluator) return Unit_Speed_Axial_Derivative_Bounds
   is (Evaluator.Bounds);

   function Derivative_Bounds
     (Evaluator : Corner_Transition_Evaluator; Start_Distance, End_Distance : Length)
      return Unit_Speed_Axial_Derivative_Bounds is
   begin
      case Evaluator.Kind_Value is
         when Circular_Transition =>
            return Arc_Bounds (Evaluator.Circle, Start_Distance, End_Distance);

         when Biarc_Transition    =>
            return Biarc_Bounds (Evaluator.Two_Arcs, Start_Distance, End_Distance);

         when others              =>
            return Evaluator.Bounds;
      end case;
   end Derivative_Bounds;

   function Position_Error_Bound (Transition : Corner_Transition) return Length
   is (Transition.Error);
   function Position_Error_Bound (Evaluator : Corner_Transition_Evaluator) return Length
   is (Evaluator.Error);
   function Certified_Position_Envelope (Transition : Corner_Transition) return Position_Envelope
   is (Pad_Envelope (Transition.Envelope, Transition.Error, Transition.Constant_Axes));
   function Certified_Position_Envelope (Evaluator : Corner_Transition_Evaluator) return Position_Envelope
   is (Pad_Envelope (Evaluator.Envelope, Evaluator.Error, Evaluator.Constant_Axes));

   function Pad_Envelope
     (Envelope : Position_Envelope; Padding : Length; Constant_Axes : Structural_Axes) return Position_Envelope
   is
      R : Position_Envelope;
   begin
      for Axis in Axis_Name loop
         if Constant_Axes (Axis) then
            R (Axis) := Envelope (Axis);
         elsif not Finite_Length (Padding)
           or else Padding < 0.0 * mm
           or else not Finite_Length (Envelope (Axis).Lower)
           or else not Finite_Length (Envelope (Axis).Upper)
         then
            R (Axis) := (Lower => Length'First, Upper => Length'Last);
         else
            R (Axis).Lower :=
              (if Envelope (Axis).Lower < Length'First + Padding
               then Length'First
               else Length'Adjacent (Envelope (Axis).Lower - Padding, Length'First));
            R (Axis).Upper :=
              (if Envelope (Axis).Upper > Length'Last - Padding
               then Length'Last
               else Length'Adjacent (Envelope (Axis).Upper + Padding, Length'Last));
         end if;
      end loop;
      return R;
   exception
      when Constraint_Error =>
         return [others => (Lower => Length'First, Upper => Length'Last)];
   end Pad_Envelope;

   function Range_Envelope
     (Start_Point, End_Point : Position; Span, Error : Length; Constant_Axes : Structural_Axes)
      return Position_Envelope
   is
      R : Position_Envelope;
   begin
      for Axis in Axis_Name loop
         if Constant_Axes (Axis) then
            R (Axis) := (Lower => Start_Point (Axis), Upper => Start_Point (Axis));
         else
            R (Axis) :=
              (Lower => Length'Min (Start_Point (Axis), End_Point (Axis)),
               Upper => Length'Max (Start_Point (Axis), End_Point (Axis)));
         end if;
      end loop;
      if not Finite_Length (Span) or else not Finite_Length (Error) or else Span < 0.0 * mm or else Error < 0.0 * mm
      then
         return [others => (Lower => Length'First, Upper => Length'Last)];
      elsif Span > Length'Last - Error then
         return [others => (Lower => Length'First, Upper => Length'Last)];
      else
         return Pad_Envelope (R, Span + Error, Constant_Axes);
      end if;
   end Range_Envelope;

   function Stereographic_Envelope
     (Curve : Stereographic_Curves.Stereographic_Curve; Start_Distance, End_Distance : Length) return Position_Envelope
   is
      Cells          : constant Positive := 32;
      Whole_Bounds   : constant Unit_Speed_Axial_Derivative_Bounds := Stereographic_Curves.Derivative_Bounds (Curve);
      Previous_Point : Position := Stereographic_Curves.Point_At_Distance (Curve, Start_Distance);
      Result         : Position_Envelope := Point_Envelope (Previous_Point);
   begin
      for Cell in 0 .. Cells - 1 loop
         declare
            Cell_Start    : constant Length :=
              Start_Distance + (End_Distance - Start_Distance) * Dimensionless (Cell) / Dimensionless (Cells);
            Cell_End      : constant Length :=
              (if Cell = Cells - 1
               then End_Distance
               else
                 Start_Distance + (End_Distance - Start_Distance) * Dimensionless (Cell + 1) / Dimensionless (Cells));
            End_Point     : constant Position := Stereographic_Curves.Point_At_Distance (Curve, Cell_End);
            Cell_Envelope : Position_Envelope;
         begin
            for Axis in Axis_Name loop
               if Stereographic_Curves.Axis_Is_Structurally_Constant (Curve, Axis) then
                  Cell_Envelope (Axis) := (Lower => Previous_Point (Axis), Upper => Previous_Point (Axis));
               elsif Whole_Bounds.Velocity (Axis) < 0.0 or else not Finite (Whole_Bounds.Velocity (Axis)) then
                  return [others => (Lower => Length'First, Upper => Length'Last)];
               else
                  declare
                     Reach : constant Length :=
                       Length'Adjacent (0.5 * (Cell_End - Cell_Start) * Whole_Bounds.Velocity (Axis), Length'Last);
                  begin
                     if not Finite_Length (Reach) or else Reach < 0.0 * mm then
                        return [others => (Lower => Length'First, Upper => Length'Last)];
                     end if;
                     Cell_Envelope (Axis) :=
                       (Lower =>
                          Length'Adjacent (Length'Min (Previous_Point (Axis), End_Point (Axis)) - Reach, Length'First),
                        Upper =>
                          Length'Adjacent (Length'Max (Previous_Point (Axis), End_Point (Axis)) + Reach, Length'Last));
                  end;
               end if;
            end loop;
            Result := Union (Result, Cell_Envelope);
            Previous_Point := End_Point;
         end;
      end loop;
      return Result;
   exception
      when Constraint_Error =>
         return [others => (Lower => Length'First, Upper => Length'Last)];
   end Stereographic_Envelope;

   function Certified_Position_Envelope
     (Transition : Corner_Transition; Start_Distance, End_Distance : Length) return Position_Envelope is
   begin
      case Transition.Kind_Value is
         when Stereographic_Transition =>
            return
              Pad_Envelope
                (Stereographic_Envelope (Transition.Stereo, Start_Distance, End_Distance),
                 Transition.Error,
                 Transition.Constant_Axes);

         when Circular_Transition      =>
            return
              Pad_Envelope
                (Arc_Envelope (Transition.Circle, Start_Distance, End_Distance),
                 Transition.Error,
                 Transition.Constant_Axes);

         when Biarc_Transition         =>
            return
              Pad_Envelope
                (Biarc_Envelope (Transition.Two_Arcs, Start_Distance, End_Distance),
                 Transition.Error,
                 Transition.Constant_Axes);

         when others                   =>
            return
              Range_Envelope
                (Point_At_Distance (Transition, Start_Distance),
                 Point_At_Distance (Transition, End_Distance),
                 End_Distance - Start_Distance,
                 Transition.Error,
                 Transition.Constant_Axes);
      end case;
   end Certified_Position_Envelope;

   function Certified_Position_Envelope
     (Evaluator : Corner_Transition_Evaluator; Start_Distance, End_Distance : Length) return Position_Envelope is
   begin
      case Evaluator.Kind_Value is
         when Circular_Transition =>
            return
              Pad_Envelope
                (Arc_Envelope (Evaluator.Circle, Start_Distance, End_Distance),
                 Evaluator.Error,
                 Evaluator.Constant_Axes);

         when Biarc_Transition    =>
            return
              Pad_Envelope
                (Biarc_Envelope (Evaluator.Two_Arcs, Start_Distance, End_Distance),
                 Evaluator.Error,
                 Evaluator.Constant_Axes);

         when others              =>
            return
              Range_Envelope
                (Point_At_Distance (Evaluator, Start_Distance),
                 Point_At_Distance (Evaluator, End_Distance),
                 End_Distance - Start_Distance,
                 Evaluator.Error,
                 Evaluator.Constant_Axes);
      end case;
   end Certified_Position_Envelope;
   function Axis_Is_Structurally_Constant (Transition : Corner_Transition; Axis : Axis_Name) return Boolean
   is (Transition.Constant_Axes (Axis));
   function Axis_Is_Structurally_Constant (Evaluator : Corner_Transition_Evaluator; Axis : Axis_Name) return Boolean
   is (Evaluator.Constant_Axes (Axis));

   function To_Evaluator (Transition : Corner_Transition) return Corner_Transition_Evaluator is
   begin
      case Transition.Kind_Value is
         when Stereographic_Transition =>
            return
              (Kind_Value    => Stereographic_Transition,
               Stereo        => Stereographic_Curves.To_Evaluator (Transition.Stereo),
               Bounds        => Transition.Bounds,
               Envelope      => Transition.Envelope,
               Error         => Transition.Error,
               Constant_Axes => Transition.Constant_Axes,
               SCV_Limit     => Transition.SCV_Limit);

         when Circular_Transition      =>
            return
              (Kind_Value    => Circular_Transition,
               Circle        => Transition.Circle,
               Bounds        => Transition.Bounds,
               Envelope      => Transition.Envelope,
               Error         => Transition.Error,
               Constant_Axes => Transition.Constant_Axes,
               SCV_Limit     => Transition.SCV_Limit);

         when Biarc_Transition         =>
            return
              (Kind_Value    => Biarc_Transition,
               Two_Arcs      => Transition.Two_Arcs,
               Bounds        => Transition.Bounds,
               Envelope      => Transition.Envelope,
               Error         => Transition.Error,
               Constant_Axes => Transition.Constant_Axes,
               SCV_Limit     => Transition.SCV_Limit);

         when Hard_Stop_Transition     =>
            return
              (Kind_Value    => Hard_Stop_Transition,
               Point         => Transition.Point,
               Bounds        => Transition.Bounds,
               Envelope      => Transition.Envelope,
               Error         => Transition.Error,
               Constant_Axes => Transition.Constant_Axes,
               SCV_Limit     => Transition.SCV_Limit);

         when Passthrough_Transition   =>
            return
              (Kind_Value    => Passthrough_Transition,
               Point         => Transition.Point,
               Bounds        => Transition.Bounds,
               Envelope      => Transition.Envelope,
               Error         => Transition.Error,
               Constant_Axes => Transition.Constant_Axes,
               SCV_Limit     => Transition.SCV_Limit);

         when Sharp_SCV_Transition     =>
            return
              (Kind_Value    => Sharp_SCV_Transition,
               Point         => Transition.Point,
               Bounds        => Transition.Bounds,
               Envelope      => Transition.Envelope,
               Error         => Transition.Error,
               Constant_Axes => Transition.Constant_Axes,
               SCV_Limit     => Transition.SCV_Limit);
      end case;
   end To_Evaluator;

end Prunt.Motion_Planner.Corner_Transitions;
