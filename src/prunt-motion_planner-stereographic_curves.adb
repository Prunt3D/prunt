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

with Ada.Numerics;
with Ada.Numerics.Generic_Complex_Types;
with Prunt.LAPACK;

pragma Extensions_Allowed (On);

package body Prunt.Motion_Planner.Stereographic_Curves is

   function Build_Binomial_Table return Binomial_Table_Type is
      Result : Binomial_Table_Type := [others => [others => 0.0]];
   begin
      --  Fill Pascal's triangle once so later Bernstein conversions use exact table lookups:
      --
      --     C(n, 0) = C(n, n) = 1,   C(n, k) = C(n - 1, k - 1) + C(n - 1, k).
      for N in 0 .. Maximum_Binomial_Degree loop
         Result (N, 0) := 1.0;
         Result (N, N) := 1.0;
         if N > 1 then
            for K in 1 .. N - 1 loop
               Result (N, K) := Result (N - 1, K - 1) + Result (N - 1, K);
            end loop;
         end if;
      end loop;
      return Result;
   end Build_Binomial_Table;

   Binomial_Table : constant Binomial_Table_Type := Build_Binomial_Table;

   function Build_Exact_Binomial_Table return Exact_Binomial_Table_Type is
      use type Interfaces.Unsigned_128;
      Result : Exact_Binomial_Table_Type := [others => [others => 0]];
   begin
      --  Use the same Pascal recurrence as the floating-point table, but keep every entry as an integer. The V7
      --  derivative certificate later combines several binomial coefficients before converting them to intervals;
      --  delaying that conversion prevents rounded table values from invalidating the enclosure.
      for N in 0 .. Maximum_Derivative_Bernstein_Degree loop
         Result (N, 0) := 1;
         Result (N, N) := 1;
         if N > 1 then
            for K in 1 .. N - 1 loop
               Result (N, K) := Result (N - 1, K - 1) + Result (N - 1, K);
            end loop;
         end if;
      end loop;
      return Result;
   end Build_Exact_Binomial_Table;

   Exact_Binomial_Table : constant Exact_Binomial_Table_Type := Build_Exact_Binomial_Table;
   --  Elaboration-time cache used by Bernstein multiplication and degree elevation without rounded table entries.

   function Exact_Binomial (N, K : Natural) return Exact_Binomial_Value
   is (if K > N or else N > Maximum_Derivative_Bernstein_Degree then 0 else Exact_Binomial_Table (N, K));

   function Binomial (N, K : Natural) return Dimensionless is
   begin
      return (if K > N or else N > Maximum_Binomial_Degree then 0.0 else Binomial_Table (N, K));
   end Binomial;

   function Closure_Envelope_Bernstein return Raw_Bernstein is
   begin
      --  The closure envelope is one degree-eight Bernstein basis function:
      --
      --     256·U⁴·(1 - U)⁴ = (128/35)·B₄,₈(U).
      --
      --  Store that coefficient directly, avoiding cancellation in a power conversion.
      return [4 => 128.0 / 35.0, others => 0.0];
   end Closure_Envelope_Bernstein;

   function Closure_Envelope_Value (U : Dimensionless) return Dimensionless is
   begin
      return 256.0 * U ** 4 * (1.0 - U) ** 4;
   end Closure_Envelope_Value;

   function Factorial (N : Natural) return Dimensionless is
      Result : Dimensionless := 1.0;
   begin
      --  The largest requested factorial is small enough to form directly without overflow or a lookup table.
      for I in 2 .. N loop
         Result := Result * Dimensionless (I);
      end loop;
      return Result;
   end Factorial;

   function Power_Basis (Coefficients : Bernstein_Chart) return Power_Chart is
      Result      : Power_Chart := [others => [others => 0.0]];
      Differences : array (Chart_Coefficient_Index) of Dimensionless;
   begin
      --  Power coefficient k is binomial(n, k) times the kth forward difference of the Bernstein controls.
      for Component in Chart_Component_Index loop
         for Index in Chart_Coefficient_Index loop
            Differences (Index) := Coefficients (Index, Component);
         end loop;
         for Order in Chart_Coefficient_Index loop
            Result (Order, Component) := Binomial (Fixed_Chart_Degree, Order) * Differences (0);
            if Order < Fixed_Chart_Degree then
               for Index in 0 .. Fixed_Chart_Degree - Order - 1 loop
                  Differences (Index) := Differences (Index + 1) - Differences (Index);
               end loop;
            end if;
         end loop;
      end loop;
      return Result;
   end Power_Basis;

   function Base_Tangent_Range_Majorants
     (Curve         : Stereographic_Curve;
      Start_V       : Dimensionless;
      End_V         : Dimensionless;
      Highest_Order : Majorant_Order := Maximum_Majorant_Order)
      return Axis_Majorants
   is
      Y                   : Chart_Majorants := [others => [others => 0.0]];
      R2                  : Scalar_Majorants := [others => 0.0];
      Inverse_Denominator : Scalar_Majorants := [others => 0.0];
      Local               : Frame_Majorants := [others => [others => 0.0]];
      Result              : Axis_Majorants := [others => [others => 0.0]];
      Constant_Chart      : Boolean := True;

      function Add_Product (Sum, Left, Right : Dimensionless) return Dimensionless;
      --  Return Sum + Left * Right with upward rounding and saturation on overflow.

      function Add_Product (Sum, Left, Right : Dimensionless) return Dimensionless is
      begin
         if Sum >= Dimensionless'Last or else Left >= Dimensionless'Last or else Right >= Dimensionless'Last then
            return Dimensionless'Last;
         elsif Left = 0.0 or else Right = 0.0 then
            return Sum;
         else
            declare
               --  Every quantity entering the majorant recurrences is nonnegative. Round each primitive upward: a
               --  final blanket inflation cannot repair an underestimate that has already propagated through a
               --  reciprocal or composition recurrence.
               Product : constant Dimensionless := Up (Left * Right);
               Value   : constant Dimensionless := Up (Sum + Product);
            begin
               if not Is_Finite (Product) or else not Is_Finite (Value) then
                  return Dimensionless'Last;
               else
                  return Value;
               end if;
            end;
         end if;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Add_Product;

      function Divide_By_Positive (Numerator, Denominator : Interval) return Interval;
      --  Enclose division by an interval that is known to be strictly positive.

      function Divide_By_Positive (Numerator, Denominator : Interval) return Interval is
      begin
         if not Numerator.Valid or else not Denominator.Valid or else Denominator.Lower <= 0.0 then
            return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
         end if;
         declare
            --  The numerator may straddle zero, so evaluate all four endpoint quotients before taking the hull.
            Q1 : constant Dimensionless := Numerator.Lower / Denominator.Lower;
            Q2 : constant Dimensionless := Numerator.Lower / Denominator.Upper;
            Q3 : constant Dimensionless := Numerator.Upper / Denominator.Lower;
            Q4 : constant Dimensionless := Numerator.Upper / Denominator.Upper;
         begin
            return
              Checked_Interval
                (Down (Dimensionless'Min (Dimensionless'Min (Q1, Q2), Dimensionless'Min (Q3, Q4))),
                 Up (Dimensionless'Max (Dimensionless'Max (Q1, Q2), Dimensionless'Max (Q3, Q4))));
         end;
      exception
         when Constraint_Error =>
            return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      end Divide_By_Positive;

      function Constant_Tangent_Component_Bound (Axis : Axis_Name) return Dimensionless;
      --  Bound one physical tangent component when the stereographic chart is constant.

      function Constant_Tangent_Component_Bound (Axis : Axis_Name) return Dimensionless is
         Y     : constant array (Chart_Component_Index) of Interval :=
           [for C in Chart_Component_Index => Interval_Exact (Curve.Coefficients (0, C))];
         R2    : Interval := Interval_Exact (0.0);
         Local : array (Frame_Component_Index) of Interval := [others => Interval_Exact (0.0)];
         Value : Interval := Interval_Exact (0.0);
      begin
         for C in Chart_Component_Index loop
            R2 := Interval_Add (R2, Interval_Multiply (Y (C), Y (C)));
         end loop;
         --  Preserve the analytic nonnegative lower bound when squaring values very close to zero.
         R2.Lower := Dimensionless'Max (0.0, R2.Lower);
         declare
            Denominator : constant Interval := Interval_Add (Interval_Exact (1.0), R2);
         begin
            Local (0) := Divide_By_Positive (Interval_Subtract (Interval_Exact (1.0), R2), Denominator);
            for C in Chart_Component_Index loop
               Local (C) := Divide_By_Positive (Interval_Multiply (Interval_Exact (2.0), Y (C)), Denominator);
            end loop;
         end;
         for C in Frame_Component_Index loop
            Value := Interval_Add (Value, Interval_Multiply (Interval_Exact (Curve.Frame (C) (Axis)), Local (C)));
         end loop;
         return Interval_Abs_Max (Value);
      end Constant_Tangent_Component_Bound;

      function Frame_Axis_Speed_Bound (Axis : Axis_Name) return Dimensionless;
      --  Bound a physical-axis component by the outward-rounded norm of its frame row.

      function Frame_Axis_Speed_Bound (Axis : Axis_Name) return Dimensionless is
         Square_Sum : Interval := Interval_Exact (0.0);
      begin
         --  For a unit local tangent, Cauchy--Schwarz bounds one physical component by the norm of the corresponding
         --  stored frame row.
         for C in Frame_Component_Index loop
            declare
               Value : constant Interval := Interval_Exact (Curve.Frame (C) (Axis));
            begin
               Square_Sum := Interval_Add (Square_Sum, Interval_Multiply (Value, Value));
            end;
         end loop;
         if not Square_Sum.Valid or else Square_Sum.Upper < 0.0 then
            return Dimensionless'Last;
         else
            return Certified_Upper_Square_Root (Dimensionless'Max (0.0, Square_Sum.Upper));
         end if;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Frame_Axis_Speed_Bound;

      function Chart_Derivative_Bound (Component : Chart_Component_Index; Order : Natural) return Dimensionless;
      --  Bound a chart component derivative over the requested parameter interval.

      function Chart_Derivative_Bound (Component : Chart_Component_Index; Order : Natural) return Dimensionless is
         type Interval_Bernstein is array (Chart_Coefficient_Index) of Interval;

         Work   : Interval_Bernstein :=
           [for I in Chart_Coefficient_Index => Interval_Exact (Curve.Coefficients (I, Component))];
         Degree : Natural := Fixed_Chart_Degree;

         function Unit_Interval (Value : Interval) return Interval;
         --  Intersect a valid interval with the analytically known unit interval.

         function Unit_Interval (Value : Interval) return Interval is
         begin
            if not Value.Valid or else Value.Upper < 0.0 or else Value.Lower > 1.0 then
               return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
            else
               --  The subdivision parameters are analytically in [0, 1]. Intersecting with that known domain avoids
               --  retaining the harmless one-ulp spill introduced by outward subtraction.
               return Checked_Interval (Dimensionless'Max (0.0, Value.Lower), Dimensionless'Min (1.0, Value.Upper));
            end if;
         end Unit_Interval;

         function Divide_Positive (Numerator, Denominator : Interval) return Interval;
         --  Divide nonnegative intervals while requiring a strictly positive denominator.

         function Divide_Positive (Numerator, Denominator : Interval) return Interval is
         begin
            if not Numerator.Valid
              or else not Denominator.Valid
              or else Numerator.Lower < 0.0
              or else Denominator.Lower <= 0.0
            then
               return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
            else
               return
                 Checked_Interval
                   (Down (Numerator.Lower / Denominator.Upper), Up (Numerator.Upper / Denominator.Lower));
            end if;
         exception
            when Constraint_Error =>
               return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
         end Divide_Positive;

         function Scale_Nonnegative (Value, Scale : Interval) return Interval;
         --  Multiply an interval by a scale interval whose lower bound is nonnegative.

         function Scale_Nonnegative (Value, Scale : Interval) return Interval is
         begin
            if not Value.Valid or else not Scale.Valid or else Scale.Lower < 0.0 then
               return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
            elsif Scale.Upper = 0.0 or else (Value.Lower = 0.0 and then Value.Upper = 0.0) then
               return Interval_Exact (0.0);
            elsif Value.Lower >= 0.0 then
               return Checked_Interval (Down (Value.Lower * Scale.Lower), Up (Value.Upper * Scale.Upper));
            elsif Value.Upper <= 0.0 then
               return Checked_Interval (Down (Value.Lower * Scale.Upper), Up (Value.Upper * Scale.Lower));
            else
               return Checked_Interval (Down (Value.Lower * Scale.Upper), Up (Value.Upper * Scale.Upper));
            end if;
         exception
            when Constraint_Error =>
               return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
         end Scale_Nonnegative;

         function Interpolate (Left, Right, T : Interval) return Interval;
         --  Interpolate two intervals at an interval parameter constrained to [0, 1].

         function Interpolate (Left, Right, T : Interval) return Interval is
            Clamped_T   : constant Interval := Unit_Interval (T);
            One_Minus_T : constant Interval := Unit_Interval (Interval_Subtract (Interval_Exact (1.0), Clamped_T));
         begin
            return Interval_Add (Scale_Nonnegative (Left, One_Minus_T), Scale_Nonnegative (Right, Clamped_T));
         end Interpolate;

         procedure Keep_Left (T : Interval);
         --  Replace Work with the left part of its de Casteljau subdivision at T.

         procedure Keep_Left (T : Interval) is
            Triangle : array (Chart_Coefficient_Index, Chart_Coefficient_Index) of Interval :=
              [others => [others => Interval_Exact (0.0)]];
         begin
            for I in 0 .. Degree loop
               Triangle (0, I) := Work (I);
            end loop;
            for Level in 1 .. Degree loop
               for I in 0 .. Degree - Level loop
                  Triangle (Level, I) := Interpolate (Triangle (Level - 1, I), Triangle (Level - 1, I + 1), T);
               end loop;
            end loop;
            for I in 0 .. Degree loop
               Work (I) := Triangle (I, 0);
            end loop;
         end Keep_Left;

         procedure Keep_Right (T : Interval);
         --  Replace Work with the right part of its de Casteljau subdivision at T.

         procedure Keep_Right (T : Interval) is
            Triangle : array (Chart_Coefficient_Index, Chart_Coefficient_Index) of Interval :=
              [others => [others => Interval_Exact (0.0)]];
         begin
            for I in 0 .. Degree loop
               Triangle (0, I) := Work (I);
            end loop;
            for Level in 1 .. Degree loop
               for I in 0 .. Degree - Level loop
                  Triangle (Level, I) := Interpolate (Triangle (Level - 1, I), Triangle (Level - 1, I + 1), T);
               end loop;
            end loop;
            for I in 0 .. Degree loop
               Work (Degree - I) := Triangle (I, Degree - I);
            end loop;
         end Keep_Right;
      begin
         if Order > Fixed_Chart_Degree then
            return 0.0;
         end if;
         --  Repeated Bernstein differences form the requested derivative control polygon:
         --
         --     bᵏᵢ = n!/(n-k)! · Δᵏbᵢ.
         --
         --  The stored majorants use Taylor coefficients, so divide the final hull by k!.
         for K in 1 .. Order loop
            for I in 0 .. Degree - 1 loop
               Work (I) :=
                 Scale_Nonnegative
                   (Interval_Subtract (Work (I + 1), Work (I)), Interval_Exact (Dimensionless (Degree)));
            end loop;
            Degree := Degree - 1;
         end loop;
         if Start_V > 0.0 then
            --  First retain [Start_V, 1], then map End_V into that restricted polynomial and retain its left part.
            Keep_Right (Interval_Exact (Start_V));
         end if;
         if End_V < 1.0 then
            Keep_Left
              (Unit_Interval
                 (Divide_Positive
                    (Unit_Interval (Interval_Subtract (Interval_Exact (End_V), Interval_Exact (Start_V))),
                     Interval_Subtract (Interval_Exact (1.0), Interval_Exact (Start_V)))));
         end if;
         declare
            Bound : Dimensionless := 0.0;
         begin
            for I in 0 .. Degree loop
               if not Work (I).Valid then
                  return Dimensionless'Last;
               end if;
               Bound := Dimensionless'Max (Bound, Up (Interval_Abs_Max (Work (I)) / Factorial (Order)));
            end loop;
            return Bound;
         end;
      end Chart_Derivative_Bound;
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return Result;
      end if;

      for C in Chart_Component_Index loop
         for I in 1 .. Fixed_Chart_Degree loop
            Constant_Chart := Constant_Chart and then Curve.Coefficients (I, C) = Curve.Coefficients (0, C);
         end loop;
      end loop;

      --  Restrict each derivative control polygon directly to the requested range. This is fixed work (three de
      --  Casteljau triangles), unlike the former 128-way interval-Taylor subdivision.
      for C in Chart_Component_Index loop
         for K in 0 .. Natural'Min (Fixed_Chart_Degree, Highest_Order) loop
            Y (C) (K) := Chart_Derivative_Bound (C, K);
         end loop;
      end loop;

      --  For r²(V) = Y(V)·Y(V), the Taylor-majorant product recurrence is
      --
      --     [r²]ₖ ≤ Σ꜀ Σⱼ₌₀ᵏ [Y꜀]ⱼ·[Y꜀]ₖ₋ⱼ.
      for K in 0 .. Highest_Order loop
         for C in Chart_Component_Index loop
            for J in 0 .. K loop
               R2 (K) := Add_Product (R2 (K), Y (C) (J), Y (C) (K - J));
            end loop;
         end loop;
      end loop;

      --  D = 1 + dot(Y,Y) is at least one everywhere. The reciprocal recurrence can therefore omit division by D(0)
      --  when forming an upper majorant.
      Inverse_Denominator (0) := 1.0;
      for K in 1 .. Highest_Order loop
         for J in 1 .. K loop
            Inverse_Denominator (K) := Add_Product (Inverse_Denominator (K), R2 (J), Inverse_Denominator (K - J));
         end loop;
      end loop;

      --  Apply the inverse-stereographic formula
      --
      --     S(Y) = ((1 - Y·Y), 2Y) / (1 + Y·Y).
      --
      --  For derivative orders k > 0, (1 - r²)/(1 + r²) = 2/(1 + r²) - 1, so the constant -1 disappears.
      Local (0) (0) := 1.0;
      for K in 1 .. Highest_Order loop
         Local (0) (K) := Up (2.0 * Inverse_Denominator (K));
      end loop;
      for C in Chart_Component_Index loop
         for K in 0 .. Highest_Order loop
            for J in 0 .. K loop
               Local (C) (K) := Add_Product (Local (C) (K), 2.0 * Y (C) (J), Inverse_Denominator (K - J));
            end loop;
         end loop;
      end loop;

      for Axis in Axis_Name loop
         if Constant_Chart then
            Result (Axis) (0) := Constant_Tangent_Component_Bound (Axis);
         else
            --  The local inverse-stereographic vector is exactly unit length, but the stored floating-point frame is
            --  only certified to be approximately orthonormal. Its outward row norm is the tight physical-axis bound;
            --  an exact 1.0 shortcut can otherwise miss by a few ulps.
            Result (Axis) (0) := Frame_Axis_Speed_Bound (Axis);
         end if;
         for K in 1 .. Highest_Order loop
            for C in Frame_Component_Index loop
               Result (Axis) (K) := Add_Product (Result (Axis) (K), abs Curve.Frame (C) (Axis), Local (C) (K));
            end loop;
         end loop;
      end loop;
      return Result;
   exception
      when Constraint_Error =>
         return [others => [others => Dimensionless'Last]];
   end Base_Tangent_Range_Majorants;

   function Tangent_Range_Majorants
     (Curve          : Stereographic_Curve;
      Start_U, End_U : Dimensionless;
      Highest_Order  : Majorant_Order := Maximum_Majorant_Order;
      Base_Majorants : access constant Axis_Majorants := null) return Axis_Majorants
   is
      Warp        : constant Dimensionless := Curve.Warp_Factor;
      Result      : Axis_Majorants := [others => [others => 0.0]];
      Warp_Series : Scalar_Majorants := [others => 0.0];

      function Add_Product (Sum, Left, Right : Dimensionless) return Dimensionless;
      --  Return Sum + Left * Right with upward rounding and saturation on overflow.

      function Add_Product (Sum, Left, Right : Dimensionless) return Dimensionless is
      begin
         if Sum >= Dimensionless'Last or else Left >= Dimensionless'Last or else Right >= Dimensionless'Last then
            return Dimensionless'Last;
         elsif Left = 0.0 or else Right = 0.0 then
            return Sum;
         else
            declare
               Product : constant Dimensionless := Up (Left * Right);
               Value   : constant Dimensionless := Up (Sum + Product);
            begin
               return (if Is_Finite (Product) and then Is_Finite (Value) then Value else Dimensionless'Last);
            end;
         end if;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Add_Product;

      function Divide_Positive (Numerator, Denominator : Interval) return Interval;
      --  Divide nonnegative intervals while requiring a strictly positive denominator.

      function Divide_Positive (Numerator, Denominator : Interval) return Interval is
      begin
         if not Numerator.Valid
           or else not Denominator.Valid
           or else Numerator.Lower < 0.0
           or else Denominator.Lower <= 0.0
         then
            return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
         else
            return
              Checked_Interval (Down (Numerator.Lower / Denominator.Upper), Up (Numerator.Upper / Denominator.Lower));
         end if;
      exception
         when Constraint_Error =>
            return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      end Divide_Positive;

      function Warp_Value_Interval (U : Dimensionless) return Interval;
      --  Enclose the warped chart parameter corresponding to U.

      function Warp_Value_Interval (U : Dimensionless) return Interval is
         U_Value     : constant Interval := Interval_Exact (U);
         Warp_Value  : constant Interval := Interval_Exact (Warp);
         Denominator : Interval;
      begin
         if U <= 0.0 then
            return Interval_Exact (0.0);
         elsif U >= 1.0 then
            return Interval_Exact (1.0);
         end if;
         --  Evaluate V(U) = W·U/(1 + (W - 1)·U) with outward-rounded interval primitives.
         Denominator :=
           Interval_Add
             (Interval_Exact (1.0), Interval_Multiply (Interval_Subtract (Warp_Value, Interval_Exact (1.0)), U_Value));
         return Divide_Positive (Interval_Multiply (Warp_Value, U_Value), Denominator);
      end Warp_Value_Interval;

      function Base_For_Range return Axis_Majorants;
      --  Return supplied base majorants or construct them for the warped range.

      function Base_For_Range return Axis_Majorants is
      begin
         if Base_Majorants /= null then
            return Base_Majorants.all;
         end if;

         declare
            Start_V_Interval : constant Interval := Warp_Value_Interval (Start_U);
            End_V_Interval   : constant Interval := Warp_Value_Interval (End_U);
            Start_V          : constant Dimensionless := Dimensionless'Max (0.0, Start_V_Interval.Lower);
            End_V            : constant Dimensionless := Dimensionless'Min (1.0, End_V_Interval.Upper);
         begin
            if Start_V_Interval.Valid and then End_V_Interval.Valid and then Start_V <= End_V then
               return Base_Tangent_Range_Majorants (Curve, Start_V, End_V, Highest_Order);
            else
               return [others => [others => Dimensionless'Last]];
            end if;
         end;
      end Base_For_Range;

      Base : constant Axis_Majorants := Base_For_Range;
   begin
      if not Is_Finite (Warp) or else Warp <= 0.0 then
         return [others => [others => Dimensionless'Last]];
      end if;
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind or else Warp = 1.0 then
         return Base;
      end if;

      declare
         Warp_Offset     : constant Interval := Interval_Subtract (Interval_Exact (Warp), Interval_Exact (1.0));
         Q_Left          : constant Interval :=
           Interval_Add (Interval_Exact (1.0), Interval_Multiply (Warp_Offset, Interval_Exact (Start_U)));
         Q_Right         : constant Interval :=
           Interval_Add (Interval_Exact (1.0), Interval_Multiply (Warp_Offset, Interval_Exact (End_U)));
         --  Q(U) = 1 + (W - 1)·U is affine, so the lesser outward-rounded endpoint lower bound applies over the
         --  complete requested range.
         Q_Min           : constant Dimensionless := Dimensionless'Min (Q_Left.Lower, Q_Right.Lower);
         Warp_Difference : constant Dimensionless :=
           Interval_Abs_Max (Interval_Subtract (Interval_Exact (1.0), Interval_Exact (Warp)));
         Delta_Power     : Dimensionless := 1.0;
         Q_Power         : Dimensionless := Down (Q_Min * Q_Min);
      begin
         if not Q_Left.Valid
           or else not Q_Right.Valid
           or else Warp <= 0.0
           or else Q_Min <= 0.0
           or else not Is_Finite (Warp_Difference)
         then
            return [others => [others => Dimensionless'Last]];
         end if;
         --  For k ≥ 1, the Taylor coefficient of V(U) = W·U/Q(U) has magnitude
         --
         --     [V]ₖ ≤ W·|1 - W|ᵏ⁻¹ / Q_Minᵏ⁺¹.
         for K in 1 .. Highest_Order loop
            if Q_Power <= 0.0 then
               return [others => [others => Dimensionless'Last]];
            end if;
            Warp_Series (K) := Up (Up (Warp * Delta_Power) / Q_Power);
            Delta_Power := Up (Delta_Power * Warp_Difference);
            Q_Power := Down (Q_Power * Q_Min);
         end loop;
      end;

      declare
         type Composition_Power_Array is array (Majorant_Order) of Scalar_Majorants;
         Powers : Composition_Power_Array := [others => [others => 0.0]];
      begin
         --  Powers(j, n) bounds the nth Taylor coefficient of (V(U) - V₀)ʲ. Composing the base tangent series uses
         --
         --     [T∘V]ₙ ≤ Σⱼ₌₀ⁿ [T]ⱼ·[ΔVʲ]ₙ.
         Powers (0) (0) := 1.0;
         for J in 1 .. Highest_Order loop
            for N in J .. Highest_Order loop
               for K in 1 .. N loop
                  Powers (J) (N) := Add_Product (Powers (J) (N), Powers (J - 1) (N - K), Warp_Series (K));
               end loop;
            end loop;
         end loop;

         for Axis in Axis_Name loop
            Result (Axis) (0) := Base (Axis) (0);
            for J in 1 .. Highest_Order loop
               for N in J .. Highest_Order loop
                  Result (Axis) (N) := Add_Product (Result (Axis) (N), Base (Axis) (J), Powers (J) (N));
               end loop;
            end loop;
         end loop;
      end;
      return Result;
   exception
      when Constraint_Error =>
         return [others => [others => Dimensionless'Last]];
   end Tangent_Range_Majorants;

   function Is_Finite (Value : Dimensionless) return Boolean is
   begin
      --  The ordered comparisons reject both infinities and NaNs. Avoid Dimensionless'Valid here: GNAT implements it
      --  with a comparatively heavy runtime classification helper, and this predicate is used by every primitive
      --  operation in the construction certificates.
      return Value >= -Dimensionless'Last and then Value <= Dimensionless'Last;
   end Is_Finite;

   function Satisfies_Unit_Tangent_Identities
     (Tangent              : Position_Scale;
      Tangent_Derivative_1 : Tangent_Derivative_1_Vector;
      Tangent_Derivative_2 : Tangent_Derivative_2_Vector;
      Tangent_Derivative_3 : Tangent_Derivative_3_Vector) return Boolean
   is
      Norm_Squared, First_Residual, Second_Residual, Third_Residual : Dimensionless := 0.0;
      Norm_Work, First_Work, Second_Work, Third_Work                : Dimensionless := 0.0;

      function Is_Roundoff (Residual, Work : Dimensionless) return Boolean;
      --  Test whether a residual is consistent with accumulated floating-point work.

      function Is_Roundoff (Residual, Work : Dimensionless) return Boolean is
         Tolerance : constant Dimensionless :=
           Jet_Tolerance_Factor * Dimensionless'Model_Epsilon * Dimensionless'Max (1.0, Work);
      begin
         return Is_Finite (Residual) and then abs Residual <= Tolerance;
      end Is_Roundoff;
   begin
      for Axis in Axis_Name loop
         declare
            T     : constant Dimensionless := Tangent (Axis);
            D1    : constant Dimensionless := Tangent_Derivative_1 (Axis) * mm;
            D2    : constant Dimensionless := Tangent_Derivative_2 (Axis) * mm ** 2;
            D3    : constant Dimensionless := Tangent_Derivative_3 (Axis) * mm ** 3;
            T_T   : constant Dimensionless := T * T;
            T_D1  : constant Dimensionless := T * D1;
            T_D2  : constant Dimensionless := T * D2;
            D1_D1 : constant Dimensionless := D1 * D1;
            T_D3  : constant Dimensionless := T * D3;
            D1_D2 : constant Dimensionless := D1 * D2;
         begin
            if not Is_Finite (T) or else not Is_Finite (D1) or else not Is_Finite (D2) or else not Is_Finite (D3) then
               return False;
            end if;

            Norm_Squared := Norm_Squared + T_T;
            First_Residual := First_Residual + T_D1;
            Second_Residual := Second_Residual + T_D2 + D1_D1;
            Third_Residual := Third_Residual + T_D3 + 3.0 * D1_D2;

            Norm_Work := Norm_Work + abs T_T;
            First_Work := First_Work + abs T_D1;
            Second_Work := Second_Work + abs T_D2 + abs D1_D1;
            Third_Work := Third_Work + abs T_D3 + 3.0 * abs D1_D2;
         end;
      end loop;

      return
        Is_Roundoff (Norm_Squared - 1.0, Norm_Work)
        and then Is_Roundoff (First_Residual, First_Work)
        and then Is_Roundoff (Second_Residual, Second_Work)
        and then Is_Roundoff (Third_Residual, Third_Work);
   exception
      when Constraint_Error =>
         return False;
   end Satisfies_Unit_Tangent_Identities;

   function Safe_Norm (Value : Position_Scale; Success : out Boolean) return Dimensionless is
      Scale : Dimensionless := 0.0;
      Sum   : Dimensionless := 0.0;
   begin
      for A in Axis_Name loop
         if not Is_Finite (Value (A)) then
            Success := False;
            return 0.0;
         end if;
         Scale := Dimensionless'Max (Scale, abs Value (A));
      end loop;

      if Scale = 0.0 then
         Success := True;
         return 0.0;
      end if;

      --  Scaling by the largest component keeps Σ(Value/Scale)² away from both overflow and underflow.
      for A in Axis_Name loop
         Sum := Sum + (Value (A) / Scale) ** 2;
      end loop;
      Success := True;
      return Scale * Dimensionless_Math.Sqrt (Sum);
   exception
      when Constraint_Error =>
         Success := False;
         return 0.0;
   end Safe_Norm;

   function Safe_Norm (Value : Position_Offset; Success : out Boolean) return Length is
      Scale : Dimensionless := 0.0;
      Sum   : Dimensionless := 0.0;
   begin
      for A in Axis_Name loop
         if not Is_Finite (Dimensionless (Value (A) / mm)) then
            Success := False;
            return 0.0 * mm;
         end if;
         Scale := Dimensionless'Max (Scale, abs Dimensionless (Value (A) / mm));
      end loop;

      if Scale = 0.0 then
         Success := True;
         return 0.0 * mm;
      end if;

      --  Compute ‖Value‖ = Scale·√Σ(Value/Scale)² after removing the physical unit.
      for A in Axis_Name loop
         Sum := Sum + (Dimensionless (Value (A) / mm) / Scale) ** 2;
      end loop;
      Success := True;
      return Dimensionless (Scale * Dimensionless_Math.Sqrt (Sum)) * mm;
   exception
      when Constraint_Error =>
         Success := False;
         return 0.0 * mm;
   end Safe_Norm;

   function Raw_Taylor_Multiply (Left, Right : Raw_Taylor) return Raw_Taylor is
      Result : Raw_Taylor := [others => 0.0];
   begin
      --  Truncated Cauchy product: (Left·Right)ₖ = Σⱼ₌₀ᵏ Leftⱼ·Rightₖ₋ⱼ.
      for I in Result'Range loop
         for J in 0 .. I loop
            Result (I) := Result (I) + Left (J) * Right (I - J);
         end loop;
      end loop;
      return Result;
   end Raw_Taylor_Multiply;

   function Raw_Taylor_Reciprocal (Value : Raw_Taylor) return Raw_Taylor is
      Result : Raw_Taylor := [others => 0.0];
   begin
      if abs Value (0) <= Minimum_Safe_Reciprocal_Denominator then
         raise Constraint_Error with "unsafe Taylor reciprocal";
      end if;

      --  Solve Value·Result = 1 coefficient by coefficient:
      --
      --     Result₀ = 1/Value₀,
      --     Resultₖ = -Σⱼ₌₁ᵏ Valueⱼ·Resultₖ₋ⱼ / Value₀.
      Result (0) := 1.0 / Value (0);
      for I in 1 .. Result'Last loop
         for J in 1 .. I loop
            Result (I) := Result (I) - Value (J) * Result (I - J);
         end loop;
         Result (I) := Result (I) / Value (0);
      end loop;
      return Result;
   end Raw_Taylor_Reciprocal;

   function Canonicalize_And_Validate_Jet
     (Jet : Endpoint_Tangent_Jet; Chord_Length : Length; Canonical : out Scaled_Tangent_Jet) return Boolean
   is
      Raw_Jet      : Scaled_Tangent_Jet := [others => [others => 0.0]];
      Norm_Series  : Raw_Taylor := [others => 0.0];
      Root_Series  : Raw_Taylor := [others => 0.0];
      Inverse_Root : Raw_Taylor;
   begin
      Canonical := [others => [others => 0.0]];

      --  Validate before changing coordinates. Multiplying the kth identity residual by Chord_Length**k and then
      --  retaining an unscaled absolute tolerance floor made validity depend on chord length; in particular, a short
      --  chord could hide a material longitudinal derivative. The physical check has one fixed unit convention and is
      --  therefore independent of the later construction coordinate.
      if Chord_Length <= 0.0 * mm
        or else not Is_Finite (Dimensionless (Chord_Length / mm))
        or else
          not Satisfies_Unit_Tangent_Identities
                (Jet.Tangent, Jet.Tangent_Derivative_1, Jet.Tangent_Derivative_2, Jet.Tangent_Derivative_3)
      then
         return False;
      end if;

      --  Change from physical arc distance S to the dimensionless coordinate U = S/L, where L is the chord length.
      --  Raw_Jet stores Taylor coefficients rather than derivatives:
      --
      --     R(U) = T + L·T′·U + (L²·T″/2!)·U² + (L³·T‴/3!)·U³.
      --
      --  This removes the physical units and puts every endpoint jet in the coordinate system used by construction.
      for A in Axis_Name loop
         if not Is_Finite (Jet.Tangent (A))
           or else not Is_Finite (Dimensionless (Jet.Tangent_Derivative_1 (A) / (1.0 / mm)))
           or else not Is_Finite (Dimensionless (Jet.Tangent_Derivative_2 (A) / (1.0 / mm ** 2)))
           or else not Is_Finite (Dimensionless (Jet.Tangent_Derivative_3 (A) / (1.0 / mm ** 3)))
         then
            return False;
         end if;

         Raw_Jet (0) (A) := Jet.Tangent (A);
         Raw_Jet (1) (A) := Jet.Tangent_Derivative_1 (A) * Chord_Length;
         Raw_Jet (2) (A) := (Jet.Tangent_Derivative_2 (A) * Chord_Length / 2.0) * Chord_Length;
         Raw_Jet (3) (A) := ((Jet.Tangent_Derivative_3 (A) * Chord_Length / 6.0) * Chord_Length) * Chord_Length;
         if (for some Order in Raw_Jet'Range => not Is_Finite (Raw_Jet (Order) (A))) then
            return False;
         end if;
      end loop;

      --  Form the Taylor coefficients of the squared norm N(U) = R(U)·R(U):
      --
      --     Nₖ = Σⱼ₌₀ᵏ Rⱼ·Rₖ₋ⱼ.
      for K in Scaled_Tangent_Jet'Range loop
         for J in 0 .. K loop
            for A in Axis_Name loop
               Norm_Series (K) := Norm_Series (K) + Raw_Jet (J) (A) * Raw_Jet (K - J) (A);
            end loop;
         end loop;
      end loop;

      --  Recover Q(U) = √N(U) from Q(U)² = N(U):
      --
      --     Q₀ = √N₀
      --     Qₖ = (Nₖ − Σⱼ₌₁ᵏ⁻¹ Qⱼ·Qₖ₋ⱼ) / (2·Q₀).
      Root_Series (0) := Dimensionless_Math.Sqrt (Norm_Series (0));
      for K in 1 .. Scaled_Tangent_Jet'Last loop
         Root_Series (K) := Norm_Series (K);
         for J in 1 .. K - 1 loop
            Root_Series (K) := Root_Series (K) - Root_Series (J) * Root_Series (K - J);
         end loop;
         Root_Series (K) := Root_Series (K) / (2.0 * Root_Series (0));
      end loop;

      --  Inverse_Root is the truncated Taylor series 1/Q(U).
      Inverse_Root := Raw_Taylor_Reciprocal (Root_Series);

      --  Canonicalize the complete jet, not just its zeroth-order tangent:
      --
      --     C(U) = R(U)/||R(U)|| = R(U)·Inverse_Root(U).
      --
      --  Thus Canonical(K) = (1/k!)·dᵏC/dUᵏ at U = 0. Exact valid input is unchanged mathematically; accepted
      --  roundoff-sized discrepancies are projected back onto the unit tangent sphere through order three.
      for K in Scaled_Tangent_Jet'Range loop
         for J in 0 .. K loop
            for A in Axis_Name loop
               Canonical (K) (A) := Canonical (K) (A) + Raw_Jet (J) (A) * Inverse_Root (K - J);
            end loop;
         end loop;
      end loop;
      return True;
   exception
      when Constraint_Error =>
         Canonical := [others => [others => 0.0]];
         return False;
   end Canonicalize_And_Validate_Jet;

   function Frame_Coordinate
     (Frame : Frame_Vector_Array; Component : Frame_Component_Index; Value : Position_Scale) return Dimensionless
   is
      Result : Dimensionless := 0.0;
   begin
      for A in Axis_Name loop
         Result := Result + Frame (Component) (A) * Value (A);
      end loop;
      return Result;
   end Frame_Coordinate;

   function Frame_Vector (Frame : Frame_Vector_Array; Components : Raw_Vector_4) return Position_Scale is
      Result : Position_Scale := [others => 0.0];
   begin
      for C in Frame_Component_Index loop
         for A in Axis_Name loop
            Result (A) := Result (A) + Components (C) * Frame (C) (A);
         end loop;
      end loop;
      return Result;
   end Frame_Vector;

   function Complete_Frame
     (First                                          : Position_Scale;
      Start_Tangent, Finish_Tangent, Chord_Direction : Position_Scale;
      Frame                                          : out Frame_Vector_Array) return Boolean
   is
      Candidates : constant Frame_Candidate_Array :=
        [First,
         Start_Tangent,
         Finish_Tangent,
         Chord_Direction,
         [X_Axis => 1.0, others => 0.0],
         [Y_Axis => 1.0, others => 0.0],
         [Z_Axis => 1.0, others => 0.0],
         [E_Axis => 1.0, others => 0.0]];
      Count      : Natural := 0;

      procedure Try_Add (Value : Position_Scale);

      procedure Try_Add (Value : Position_Scale) is
         V        : Position_Scale := Value;
         Raw_Norm : Dimensionless;
         Success  : Boolean;
      begin
         if Count = Frame_Vector_Array'Length then
            return;
         end if;

         Raw_Norm := Safe_Norm (V, Success);
         if not Success or else Raw_Norm <= 128.0 * Dimensionless'Model_Epsilon then
            return;
         end if;

         --  Two modified Gram-Schmidt passes suppress the loss of orthogonality caused by nearly dependent
         --  geometric candidates.
         for Pass in 1 .. 2 loop
            for I in 0 .. Count - 1 loop
               declare
                  Projection : constant Dimensionless := Frame_Coordinate (Frame, I, V);
               begin
                  V := V - Frame (I) * Projection;
               end;
            end loop;
         end loop;

         Raw_Norm := Safe_Norm (V, Success);
         if Success and then Raw_Norm > 256.0 * Dimensionless'Model_Epsilon then
            Frame (Count) := V / Raw_Norm;
            Count := Count + 1;
         end if;
      end Try_Add;
   begin
      Frame := [others => [others => 0.0]];
      for Candidate of Candidates loop
         Try_Add (Candidate);
      end loop;

      if Count /= Frame_Vector_Array'Length then
         return False;
      end if;

      --  Recheck Fᵢ·Fⱼ ≈ δᵢⱼ after construction instead of assuming normalization made the frame trustworthy.
      for I in Frame_Component_Index loop
         for J in Frame_Component_Index loop
            declare
               Expected : constant Dimensionless := (if I = J then 1.0 else 0.0);
               Actual   : constant Dimensionless := Frame_Coordinate (Frame, I, Frame (J));
            begin
               if abs (Actual - Expected) > Frame_Residual_Tolerance then
                  return False;
               end if;
            end;
         end loop;
      end loop;
      return True;
   end Complete_Frame;

   function Chart_From_Canonical_Jet
     (Frame : Frame_Vector_Array; Jet : Scaled_Tangent_Jet; Result : out Chart_Jet_Array) return Boolean
   is
      Frame_Series : array (Frame_Component_Index) of Raw_Taylor := [others => [others => 0.0]];
      Denominator  : Raw_Taylor := [others => 0.0];
      Inverse_Den  : Raw_Taylor;
   begin
      Result := [others => [others => 0.0]];
      for C in Frame_Component_Index loop
         for K in Scaled_Tangent_Jet'Range loop
            Frame_Series (C) (K) := Frame_Coordinate (Frame, C, Jet (K));
         end loop;
      end loop;

      --  Inverse stereographic projection from a unit tangent T in this frame is
      --
      --     Y꜀ = T꜀/(1 + T₀).
      --
      --  Divide the complete Taylor series so endpoint derivatives through order three are preserved.
      Denominator := Frame_Series (0);
      Denominator (0) := Denominator (0) + 1.0;
      if Denominator (0) <= Minimum_Safe_Reciprocal_Denominator then
         return False;
      end if;
      Inverse_Den := Raw_Taylor_Reciprocal (Denominator);

      for C in Chart_Component_Index loop
         declare
            Product : constant Raw_Taylor := Raw_Taylor_Multiply (Frame_Series (C), Inverse_Den);
         begin
            for K in Scaled_Tangent_Jet'Range loop
               Result (K) (C) := Product (K);
            end loop;
         end;
      end loop;
      return True;
   exception
      when Constraint_Error =>
         Result := [others => [others => 0.0]];
         return False;
   end Chart_From_Canonical_Jet;

   procedure Build_Chart
     (Start_Jets, Finish_Jets             : Chart_Jet_Array;
      Lambda                              : Dimensionless;
      C0                                  : Raw_Vector_3;
      Warp_Factor                         : Dimensionless;
      Coefficients, D_Lambda_Coefficients : out Bernstein_Chart)
   is
      Envelope        : constant Raw_Bernstein := Closure_Envelope_Bernstein;
      Warp_Difference : constant Dimensionless := Warp_Factor - 1.0;
   begin
      Coefficients := [others => [others => 0.0]];
      D_Lambda_Coefficients := [others => [others => 0.0]];

      for C in Chart_Component_Index loop
         declare
            Degree_7                        : Raw_Bernstein_7 := [others => 0.0];
            D_Degree_7                      : Raw_Bernstein_7 := [others => 0.0];
            Elevated                        : Raw_Bernstein := [others => 0.0];
            D_Elevated                      : Raw_Bernstein := [others => 0.0];
            Start_Base, Finish_Base         : Raw_Taylor := [others => 0.0];
            D_Start_Base, D_Finish_Base     : Raw_Taylor := [others => 0.0];
            Start_Scaled, Finish_Scaled     : Raw_Taylor := [others => 0.0];
            D_Start_Scaled, D_Finish_Scaled : Raw_Taylor := [others => 0.0];
         begin
            --  Scaling the normalized distance by λ scales the kth endpoint derivative by λᵏ. Form the analytic
            --  λ-derivative beside it for the closure Jacobian.
            for K in Raw_Taylor'Range loop
               Start_Scaled (K) := Lambda ** K * Start_Jets (K) (C);
               Finish_Scaled (K) := Lambda ** K * Finish_Jets (K) (C);
               if K > 0 then
                  D_Start_Scaled (K) := Dimensionless (K) * Lambda ** (K - 1) * Start_Jets (K) (C);
                  D_Finish_Scaled (K) := Dimensionless (K) * Lambda ** (K - 1) * Finish_Jets (K) (C);
               end if;
            end loop;

            --  Build the polynomial in V, where
            --
            --     V = W·U/(1 + (W - 1)·U).
            --
            --  Inverse-composing the requested U jets makes the composed tangent match every endpoint derivative
            --  through order three.
            Start_Base (0) := Start_Scaled (0);
            Start_Base (1) := Start_Scaled (1) / Warp_Factor;
            Start_Base (2) := (Start_Scaled (2) + Warp_Difference * Start_Scaled (1)) / Warp_Factor ** 2;
            Start_Base (3) :=
              (Start_Scaled (3) + 2.0 * Warp_Difference * Start_Scaled (2) + Warp_Difference ** 2 * Start_Scaled (1))
              / Warp_Factor ** 3;
            D_Start_Base (0) := D_Start_Scaled (0);
            D_Start_Base (1) := D_Start_Scaled (1) / Warp_Factor;
            D_Start_Base (2) := (D_Start_Scaled (2) + Warp_Difference * D_Start_Scaled (1)) / Warp_Factor ** 2;
            D_Start_Base (3) :=
              (D_Start_Scaled (3) + 2.0 * Warp_Difference * D_Start_Scaled (2)
               + Warp_Difference ** 2 * D_Start_Scaled (1))
              / Warp_Factor ** 3;

            Finish_Base (0) := Finish_Scaled (0);
            Finish_Base (1) := Warp_Factor * Finish_Scaled (1);
            Finish_Base (2) :=
              Warp_Factor ** 2 * Finish_Scaled (2) + Warp_Factor * Warp_Difference * Finish_Scaled (1);
            Finish_Base (3) :=
              Warp_Factor ** 3 * Finish_Scaled (3) + 2.0 * Warp_Factor ** 2 * Warp_Difference * Finish_Scaled (2)
              + Warp_Factor * Warp_Difference ** 2 * Finish_Scaled (1);
            D_Finish_Base (0) := D_Finish_Scaled (0);
            D_Finish_Base (1) := Warp_Factor * D_Finish_Scaled (1);
            D_Finish_Base (2) :=
              Warp_Factor ** 2 * D_Finish_Scaled (2) + Warp_Factor * Warp_Difference * D_Finish_Scaled (1);
            D_Finish_Base (3) :=
              Warp_Factor ** 3 * D_Finish_Scaled (3) + 2.0 * Warp_Factor ** 2 * Warp_Difference * D_Finish_Scaled (2)
              + Warp_Factor * Warp_Difference ** 2 * D_Finish_Scaled (1);

            for I in 0 .. 3 loop
               --  A degree-7 Hermite interpolant has exactly eight endpoint conditions: value plus three derivatives
               --  at each end. For factorial-scaled Taylor data tₖ, the endpoint Bernstein controls satisfy
               --
               --     bᵢ = Σₖ₌₀ⁱ C(i, k)·tₖ/C(7, k).
               for K in 0 .. I loop
                  declare
                     Start_Term    : constant Dimensionless := Binomial (I, K) * Start_Base (K) / Binomial (7, K);
                     Finish_Term   : constant Dimensionless :=
                       (if K mod 2 = 0 then 1.0 else -1.0) * Binomial (I, K) * Finish_Base (K) / Binomial (7, K);
                     D_Start_Term  : constant Dimensionless := Binomial (I, K) * D_Start_Base (K) / Binomial (7, K);
                     D_Finish_Term : constant Dimensionless :=
                       (if K mod 2 = 0 then 1.0 else -1.0) * Binomial (I, K) * D_Finish_Base (K) / Binomial (7, K);
                  begin
                     Degree_7 (I) := Degree_7 (I) + Start_Term;
                     Degree_7 (7 - I) := Degree_7 (7 - I) + Finish_Term;
                     D_Degree_7 (I) := D_Degree_7 (I) + D_Start_Term;
                     D_Degree_7 (7 - I) := D_Degree_7 (7 - I) + D_Finish_Term;
                  end;
               end loop;
            end loop;

            for J in Chart_Coefficient_Index loop
               --  Elevate degree 7 to the fixed degree 8 without changing the represented polynomial:
               --
               --     b′ⱼ = Σᵢ C(7, i)·C(1, j-i)/C(8, j)·bᵢ.
               for I in 0 .. 7 loop
                  if J >= I and then J - I <= Fixed_Chart_Degree - 7 then
                     declare
                        Weight : constant Dimensionless :=
                          Binomial (7, I)
                          * Binomial (Fixed_Chart_Degree - 7, J - I)
                          / Binomial (Fixed_Chart_Degree, J);
                     begin
                        Elevated (J) := Elevated (J) + Weight * Degree_7 (I);
                        D_Elevated (J) := D_Elevated (J) + Weight * D_Degree_7 (I);
                     end;
                  end if;
               end loop;
            end loop;

            --  Generic degree elevation is mathematically endpoint-jet preserving, but its rounded multiply/add
            --  sequence need not leave controls which should be identical bit-for-bit. Preserve structural zero
            --  endpoint jets in the production chart itself. Besides making the requested derivatives exact for this
            --  common line-line case, this exposes the V⁴ and (1 - V)⁴ factors used by the strict no-bulge certificate
            --  below. Apply the same canonicalization to the analytic lambda derivative so the closure Jacobian
            --  describes exactly the chart being solved.
            if (for all K in 1 .. 3 => Start_Base (K) = 0.0) then
               for J in 0 .. 3 loop
                  Elevated (J) := Start_Base (0);
                  D_Elevated (J) := D_Start_Base (0);
               end loop;
            end if;
            if (for all K in 1 .. 3 => Finish_Base (K) = 0.0) then
               for J in 5 .. Fixed_Chart_Degree loop
                  Elevated (J) := Finish_Base (0);
                  D_Elevated (J) := D_Finish_Base (0);
               end loop;
            end if;

            for J in Chart_Coefficient_Index loop
               Coefficients (J, C) := Elevated (J);
               D_Lambda_Coefficients (J, C) := D_Elevated (J);
            end loop;
            --  Add C₀·256·V⁴·(1 - V)⁴. Its fourfold zeros leave every endpoint jet through order three unchanged.
            for J in Chart_Coefficient_Index loop
               Coefficients (J, C) := Coefficients (J, C) + Dimensionless (Envelope (J) * C0 (C));
            end loop;
         end;
      end loop;
   end Build_Chart;

   procedure Build_Chart
     (Start_Jets, Finish_Jets : Chart_Jet_Array;
      Lambda                  : Dimensionless;
      C0                      : Raw_Vector_3;
      Warp_Factor             : Dimensionless;
      Coefficients            : out Bernstein_Chart)
   is
      Ignored : Bernstein_Chart;
   begin
      Build_Chart (Start_Jets, Finish_Jets, Lambda, C0, Warp_Factor, Coefficients, Ignored);
   end Build_Chart;

   function Chart_Value (Coefficients : Bernstein_Chart; U : Dimensionless) return Chart_Vector is
      Result : Raw_Vector_3 := [others => 0.0];
      Basis  : Dimensionless := 1.0;
   begin
      if U <= 0.0 then
         return [for C in Chart_Component_Index => Coefficients (0, C)];
      elsif U >= 1.0 then
         return [for C in Chart_Component_Index => Coefficients (Fixed_Chart_Degree, C)];
      elsif U <= 0.5 then
         declare
            One_Minus_U : constant Dimensionless := 1.0 - U;
            Ratio       : constant Dimensionless := U / One_Minus_U;
         begin
            --  Generate Bᵢ,ₙ(U) forward from B₀,ₙ = (1 - U)ⁿ:
            --
            --     Bᵢ₊₁,ₙ/Bᵢ,ₙ = (n - i)/(i + 1) · U/(1 - U).
            for I in 1 .. Fixed_Chart_Degree loop
               Basis := Basis * One_Minus_U;
            end loop;
            for I in Chart_Coefficient_Index loop
               for C in Chart_Component_Index loop
                  Result (C) := Result (C) + Basis * Coefficients (I, C);
               end loop;
               if I < Fixed_Chart_Degree then
                  Basis := Basis * Dimensionless (Fixed_Chart_Degree - I) / Dimensionless (I + 1) * Ratio;
               end if;
            end loop;
         end;
      else
         declare
            Ratio : constant Dimensionless := (1.0 - U) / U;
         begin
            --  Near U = 1, run the reciprocal recurrence backward from Bₙ,ₙ = Uⁿ to avoid a large U/(1 - U).
            for I in 1 .. Fixed_Chart_Degree loop
               Basis := Basis * U;
            end loop;
            for I in reverse Chart_Coefficient_Index loop
               for C in Chart_Component_Index loop
                  Result (C) := Result (C) + Basis * Coefficients (I, C);
               end loop;
               if I > 0 then
                  Basis := Basis * Dimensionless (I) / Dimensionless (Fixed_Chart_Degree - I + 1) * Ratio;
               end if;
            end loop;
         end;
      end if;
      return [for C in Chart_Component_Index => Result (C)];
   end Chart_Value;

   function Stereographic (Y : Chart_Vector) return Raw_Vector_4 is
      R2 : Dimensionless := 0.0;
   begin
      for C in Chart_Component_Index loop
         R2 := R2 + Y (C) ** 2;
      end loop;
      declare
         Denominator : constant Dimensionless := 1.0 + R2;
      begin
         --  Map ℝ³ to the unit 3-sphere:
         --
         --     S(Y) = ((1 - ‖Y‖²), 2Y)/(1 + ‖Y‖²).
         return
           [0 => (1.0 - R2) / Denominator,
            1 => 2.0 * Y (1) / Denominator,
            2 => 2.0 * Y (2) / Denominator,
            3 => 2.0 * Y (3) / Denominator];
      end;
   end Stereographic;

   function Stereographic_Jacobian (Y : Chart_Vector) return Raw_Stereo_Jacobian is
      Result : Raw_Stereo_Jacobian := [others => [others => 0.0]];
      R2     : Dimensionless := 0.0;
   begin
      for C in Chart_Component_Index loop
         R2 := R2 + Y (C) ** 2;
      end loop;
      declare
         Denominator : constant Dimensionless := 1.0 + R2;
      begin
         --  Differentiate S₀ = (1 - r²)/(1 + r²) and Sᵢ = 2Yᵢ/(1 + r²) with respect to each Yⱼ.
         for J in Chart_Component_Index loop
            Result (0, J) := -4.0 * Y (J) / Denominator ** 2;
         end loop;
         for I in Chart_Component_Index loop
            for J in Chart_Component_Index loop
               Result (I, J) := (if I = J then 2.0 / Denominator else 0.0) - 4.0 * Y (I) * Y (J) / Denominator ** 2;
            end loop;
         end loop;
      end;
      return Result;
   end Stereographic_Jacobian;

   function Warp_Parameter (U, Warp_Factor : Dimensionless) return Dimensionless is
   begin
      if U <= 0.0 then
         return 0.0;
      elsif U >= 1.0 then
         return 1.0;
      else
         declare
            Result : constant Dimensionless := Warp_Factor * U / ((1.0 - U) + Warp_Factor * U);
         begin
            return Dimensionless'Max (0.0, Dimensionless'Min (1.0, Result));
         end;
      end if;
   end Warp_Parameter;

   function Unwarp_Parameter (V, Warp_Factor : Dimensionless) return Dimensionless is
   begin
      if V <= 0.0 then
         return 0.0;
      elsif V >= 1.0 then
         return 1.0;
      else
         declare
            Result : constant Dimensionless := V / (Warp_Factor * (1.0 - V) + V);
         begin
            return Dimensionless'Max (0.0, Dimensionless'Min (1.0, Result));
         end;
      end if;
   end Unwarp_Parameter;

   function Tangent_At
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Warp_Factor, U : Dimensionless) return Position_Scale
   is
      V : constant Dimensionless := Warp_Parameter (U, Warp_Factor);
   begin
      return Frame_Vector (Frame, Stereographic (Chart_Value (Coefficients, V)));
   end Tangent_At;

   function Integrate_GL16
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Warp_Factor, A, B : Dimensionless)
      return Position_Scale
   is
      Result : Position_Scale := [others => 0.0];
      Middle : constant Dimensionless := 0.5 * (A + B);
      Half   : constant Dimensionless := 0.5 * (B - A);
   begin
      if A = B then
         return Result;
      end if;

      --  Map the positive and negative GL16 nodes from [-1, 1] onto [A, B]. Pairing them preserves symmetry.
      for I in GL16_Positive_Index loop
         declare
            Left  : constant Position_Scale :=
              Tangent_At (Frame, Coefficients, Warp_Factor, Middle - Half * GL16_Nodes (I));
            Right : constant Position_Scale :=
              Tangent_At (Frame, Coefficients, Warp_Factor, Middle + Half * GL16_Nodes (I));
         begin
            Result := Result + (Left + Right) * Dimensionless (Half * GL16_Weights (I));
         end;
      end loop;
      return Result;
   end Integrate_GL16;

   function Down (Value : Dimensionless) return Dimensionless is
      Inflation : Dimensionless;
      Candidate : Dimensionless;
   begin
      if Value <= -Dimensionless'Last then
         return -Dimensionless'Last;
      end if;
      --  Move by at least two model epsilons, falling back to Adjacent when subtraction rounds back to Value.
      Inflation := Dimensionless'Max (Dimensionless'Model_Small, 2.0 * Dimensionless'Model_Epsilon * abs Value);
      Candidate := Value - Inflation;
      return (if Candidate < Value then Candidate else Dimensionless'Adjacent (Value, -Dimensionless'Last));
   end Down;

   function Up (Value : Dimensionless) return Dimensionless is
      Inflation : Dimensionless;
      Candidate : Dimensionless;
   begin
      if Value >= Dimensionless'Last then
         return Dimensionless'Last;
      end if;
      --  Move by at least two model epsilons, falling back to Adjacent when addition rounds back to Value.
      Inflation := Dimensionless'Max (Dimensionless'Model_Small, 2.0 * Dimensionless'Model_Epsilon * abs Value);
      Candidate := Value + Inflation;
      return (if Candidate > Value then Candidate else Dimensionless'Adjacent (Value, Dimensionless'Last));
   end Up;

   function Checked_Interval (Lower, Upper : Dimensionless; Valid : Boolean := True) return Interval is
   begin
      if Valid and then Is_Finite (Lower) and then Is_Finite (Upper) and then Lower <= Upper then
         return (Lower => Lower, Upper => Upper, Valid => True);
      else
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      end if;
   end Checked_Interval;

   function Interval_Exact (Value : Dimensionless) return Interval is
   begin
      --  Values already stored in the curve, exact integers, and dyadic cell boundaries are the authoritative inputs
      --  to the implemented formula. They do not acquire uncertainty merely by being loaded. Arithmetic involving
      --  them is still expanded after every primitive operation below.
      return Checked_Interval (Value, Value, Is_Finite (Value));
   end Interval_Exact;

   function Interval_Add (Left, Right : Interval) return Interval is
   begin
      if not Left.Valid or else not Right.Valid then
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      elsif Left.Lower = 0.0 and then Left.Upper = 0.0 then
         return Right;
      elsif Right.Lower = 0.0 and then Right.Upper = 0.0 then
         return Left;
      end if;
      return Checked_Interval (Down (Left.Lower + Right.Lower), Up (Left.Upper + Right.Upper));
   exception
      when Constraint_Error =>
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
   end Interval_Add;

   function Interval_Negate (Value : Interval) return Interval is
   begin
      if not Value.Valid then
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      elsif Value.Lower = 0.0 and then Value.Upper = 0.0 then
         return Value;
      end if;
      --  Unary negation is exact for every finite model number; only reverse the interval endpoints. Inflating here
      --  makes the later subtraction of nearby stored coordinates scale with their absolute coordinates instead of
      --  with the small correction being enclosed.
      return Checked_Interval (-Value.Upper, -Value.Lower, Value.Valid);
   exception
      when Constraint_Error =>
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
   end Interval_Negate;

   function Interval_Subtract (Left, Right : Interval) return Interval is
   begin
      return Interval_Add (Left, Interval_Negate (Right));
   end Interval_Subtract;

   function Interval_Multiply (Left, Right : Interval) return Interval is
   begin
      if not Left.Valid or else not Right.Valid then
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      elsif (Left.Lower = 0.0 and then Left.Upper = 0.0) or else (Right.Lower = 0.0 and then Right.Upper = 0.0) then
         return Interval_Exact (0.0);
      elsif Left.Lower = 1.0 and then Left.Upper = 1.0 then
         return Right;
      elsif Right.Lower = 1.0 and then Right.Upper = 1.0 then
         return Left;
      elsif Left.Lower = -1.0 and then Left.Upper = -1.0 then
         return Interval_Negate (Right);
      elsif Right.Lower = -1.0 and then Right.Upper = -1.0 then
         return Interval_Negate (Left);
      end if;
      declare
         Lower_Product : Dimensionless;
         Upper_Product : Dimensionless;
      begin
         --  Except when both intervals span zero, their signs identify the two corner products which attain the
         --  product interval's extrema. Avoid forming the other two products and the four-way min/max in the common
         --  cases; all binomial weights and denominator reciprocals take one of these two-product paths.
         if Left.Lower >= 0.0 then
            if Right.Lower >= 0.0 then
               Lower_Product := Left.Lower * Right.Lower;
               Upper_Product := Left.Upper * Right.Upper;
            elsif Right.Upper <= 0.0 then
               Lower_Product := Left.Upper * Right.Lower;
               Upper_Product := Left.Lower * Right.Upper;
            else
               Lower_Product := Left.Upper * Right.Lower;
               Upper_Product := Left.Upper * Right.Upper;
            end if;
         elsif Left.Upper <= 0.0 then
            if Right.Lower >= 0.0 then
               Lower_Product := Left.Lower * Right.Upper;
               Upper_Product := Left.Upper * Right.Lower;
            elsif Right.Upper <= 0.0 then
               Lower_Product := Left.Upper * Right.Upper;
               Upper_Product := Left.Lower * Right.Lower;
            else
               Lower_Product := Left.Lower * Right.Upper;
               Upper_Product := Left.Lower * Right.Lower;
            end if;
         elsif Right.Lower >= 0.0 then
            Lower_Product := Left.Lower * Right.Upper;
            Upper_Product := Left.Upper * Right.Upper;
         elsif Right.Upper <= 0.0 then
            Lower_Product := Left.Upper * Right.Lower;
            Upper_Product := Left.Lower * Right.Lower;
         else
            Lower_Product := Dimensionless'Min (Left.Lower * Right.Upper, Left.Upper * Right.Lower);
            Upper_Product := Dimensionless'Max (Left.Lower * Right.Lower, Left.Upper * Right.Upper);
         end if;
         return Checked_Interval (Down (Lower_Product), Up (Upper_Product));
      end;
   exception
      when Constraint_Error =>
         return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
   end Interval_Multiply;

   function Interval_Abs_Max (Value : Interval) return Dimensionless is
   begin
      if not Value.Valid then
         return Dimensionless'Last;
      else
         return Dimensionless'Max (abs Value.Lower, abs Value.Upper);
      end if;
   end Interval_Abs_Max;

   function Certified_Upper_Square_Root (Value : Dimensionless) return Dimensionless is
      Result : Dimensionless;
   begin
      if Value < 0.0 or else not Is_Finite (Value) then
         return Dimensionless'Last;
      elsif Value = 0.0 then
         return 0.0;
      end if;

      Result := Up (Dimensionless_Math.Sqrt (Value));
      for Attempt in 1 .. 8 loop
         if Result <= 0.0 or else not Is_Finite (Result) or else Result >= Dimensionless'Last then
            return Dimensionless'Last;
         end if;
         declare
            Quotient_Upper : constant Dimensionless := Up (Value / Result);
         begin
            --  Result >= Value / Result is equivalent to Result**2 >= Value for positive Result. Certifying with an
            --  outward-rounded quotient avoids squaring near Model_Small: the generic interval multiplier must use
            --  an absolute Model_Small inflation there, which is intentionally conservative but too wide to prove
            --  this particular inequality.
            if Is_Finite (Quotient_Upper) and then Result >= Quotient_Upper then
               return Result;
            end if;
         end;
         Result := Up (Result);
      end loop;
      return Dimensionless'Last;
   exception
      when Constraint_Error =>
         return Dimensionless'Last;
   end Certified_Upper_Square_Root;

   function Frame_Speed_Upper_Bound (Frame : Frame_Vector_Array) return Dimensionless;
   --  Bound the operator norm of a floating-point tangent frame from an outward-rounded Gram matrix.

   function Frame_Speed_Upper_Bound (Frame : Frame_Vector_Array) return Dimensionless is
      Maximum_Row_Sum : Dimensionless := 0.0;
   begin
      for I in Frame_Component_Index loop
         declare
            Row_Sum : Interval := Interval_Exact (0.0);
         begin
            for J in Frame_Component_Index loop
               declare
                  Dot : Interval := Interval_Exact (0.0);
               begin
                  for Axis in Axis_Name loop
                     Dot :=
                       Interval_Add
                         (Dot,
                          Interval_Multiply (Interval_Exact (Frame (I) (Axis)), Interval_Exact (Frame (J) (Axis))));
                  end loop;
                  if not Dot.Valid then
                     return Dimensionless'Last;
                  end if;
                  Row_Sum := Interval_Add (Row_Sum, Interval_Exact (Interval_Abs_Max (Dot)));
               end;
            end loop;
            if not Row_Sum.Valid then
               return Dimensionless'Last;
            end if;
            Maximum_Row_Sum := Dimensionless'Max (Maximum_Row_Sum, Row_Sum.Upper);
         end;
      end loop;
      return Certified_Upper_Square_Root (Dimensionless'Max (0.0, Maximum_Row_Sum));
   exception
      when Constraint_Error =>
         return Dimensionless'Last;
   end Frame_Speed_Upper_Bound;

   function Finish_Correction_Interval (Evaluator : Stereographic_Curve_Evaluator; Axis : Axis_Name) return Interval is
      Finish_Value      : constant Dimensionless := Dimensionless (Evaluator.Finish_Point (Axis) / mm);
      Uncorrected_Value : constant Dimensionless := Dimensionless (Evaluator.Uncorrected_Finish_Point (Axis) / mm);
   begin
      if Finish_Value = Uncorrected_Value then
         return Interval_Exact (0.0);
      else
         return Interval_Subtract (Interval_Exact (Finish_Value), Interval_Exact (Uncorrected_Value));
      end if;
   end Finish_Correction_Interval;

   function Finish_Correction_Position_Error_Bound (Evaluator : Stereographic_Curve_Evaluator) return Dimensionless;
   --  Bound the Euclidean endpoint-correction displacement in millimetres without a rounded vector subtraction.

   function Finish_Correction_Position_Error_Bound (Evaluator : Stereographic_Curve_Evaluator) return Dimensionless is
      Axis_Bounds : Dimensionless_Axis_Vector := [others => 0.0];
      Scale       : Dimensionless := 0.0;
      Square_Sum  : Dimensionless := 0.0;
   begin
      --  The smootherstep lies in [0, 1], so the Euclidean norm of Finish_Point - Uncorrected_Finish_Point bounds
      --  its uniform position contribution. Form every subtraction outward and scale the norm to avoid overflow.
      for Axis in Axis_Name loop
         declare
            Correction : constant Interval := Finish_Correction_Interval (Evaluator, Axis);
         begin
            if not Correction.Valid then
               return Dimensionless'Last;
            end if;
            Axis_Bounds (Axis) := Interval_Abs_Max (Correction);
            Scale := Dimensionless'Max (Scale, Axis_Bounds (Axis));
         end;
      end loop;
      if Scale = 0.0 then
         return 0.0;
      end if;
      for Axis in Axis_Name loop
         declare
            Ratio : constant Dimensionless := Up (Axis_Bounds (Axis) / Scale);
         begin
            Square_Sum := Up (Square_Sum + Up (Ratio * Ratio));
         end;
      end loop;
      declare
         Unit_Norm_Bound : constant Dimensionless := Certified_Upper_Square_Root (Square_Sum);
      begin
         if Unit_Norm_Bound >= Dimensionless'Last
           or else not Is_Finite (Unit_Norm_Bound)
           or else Scale > Dimensionless'Last / Unit_Norm_Bound
         then
            return Dimensionless'Last;
         else
            return Up (Scale * Unit_Norm_Bound);
         end if;
      end;
   exception
      when Constraint_Error =>
         return Dimensionless'Last;
   end Finish_Correction_Position_Error_Bound;

   function Is_Zero_Projection (Coefficients : Projection_Coefficients) return Boolean is
   begin
      for A in Axis_Name loop
         if Coefficients (A) /= 0.0 / mm then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero_Projection;

   ----------------------------------------------------------------------------------------------------------------
   --  Closure construction. A bounded direct four-variable solve determines
   --  the three closure-mode amplitudes and the curve length together.
   ----------------------------------------------------------------------------------------------------------------

   procedure Closure_Residual
     (Frame, Closure_Frame    : Frame_Vector_Array;
      Start_Jets, Finish_Jets : Chart_Jet_Array;
      Chord_Direction         : Position_Scale;
      Lambda                  : Dimensionless;
      C0                      : Raw_Vector_3;
      Warp_Factor             : Dimensionless;
      Residual                : out Raw_Vector_4;
      Jacobian_C              : out Raw_Matrix_4_3;
      D_Residual_D_Lambda     : out Raw_Vector_4;
      Panel_Count             : Positive := 8)
   is
      Power_Coefficients, D_Power_Coefficients : Power_Chart;
      Integral                                 : Position_Scale := [others => 0.0];
      D_Integral_D_Lambda                      : Position_Scale := [others => 0.0];
      D_Integral_D_C                           : array (Chart_Component_Index) of Position_Scale :=
        [others => [others => 0.0]];

      function Power_Chart_Value (Value : Power_Chart; U : Dimensionless) return Chart_Vector;
      --  Evaluate a power-basis chart at U using Horner's method.

      function Power_Chart_Value (Value : Power_Chart; U : Dimensionless) return Chart_Vector is
         Result : Raw_Vector_3 := [others => 0.0];
      begin
         --  Horner evaluation uses one multiply and one add per power coefficient:
         --
         --     Y꜀(U) = (…((a₈·U + a₇)·U + a₆)…)·U + a₀.
         for C in Chart_Component_Index loop
            for Degree in reverse 0 .. Fixed_Chart_Degree loop
               Result (C) := Result (C) * U + Value (Degree, C);
            end loop;
         end loop;
         return [for C in Chart_Component_Index => Result (C)];
      end Power_Chart_Value;

      procedure Build_Power_Charts;

      procedure Build_Power_Charts is
         Chart, D_Chart : Bernstein_Chart;
      begin
         Build_Chart (Start_Jets, Finish_Jets, Lambda, C0, Warp_Factor, Chart, D_Chart);
         Power_Coefficients := Power_Basis (Chart);
         D_Power_Coefficients := Power_Basis (D_Chart);
         for C in Chart_Component_Index loop
            declare
               Correction     : constant Dimensionless := Dimensionless (256.0 * C0 (C)) - Power_Coefficients (8, C);
               D_Correction   : constant Dimensionless := -D_Power_Coefficients (8, C);
               Envelope_Power : constant array (Natural range 4 .. 8) of Dimensionless := [1.0, -4.0, 6.0, -4.0, 1.0];
            begin
               --  Correct with V⁴·(1 - V)⁴ as a whole. Changing only the V⁸ coefficient, as V3 did, perturbs the
               --  finish jet.
               for Degree in Envelope_Power'Range loop
                  Power_Coefficients (Degree, C) :=
                    Power_Coefficients (Degree, C) + Correction * Envelope_Power (Degree);
                  D_Power_Coefficients (Degree, C) :=
                    D_Power_Coefficients (Degree, C) + D_Correction * Envelope_Power (Degree);
               end loop;
            end;
         end loop;
      end Build_Power_Charts;

      procedure Accumulate (U, Weight : Dimensionless);

      procedure Accumulate (U, Weight : Dimensionless) is
         V                : constant Dimensionless := Warp_Parameter (U, Warp_Factor);
         Y                : constant Chart_Vector := Power_Chart_Value (Power_Coefficients, V);
         T_Local          : constant Raw_Vector_4 := Stereographic (Y);
         T_Global         : constant Position_Scale := Frame_Vector (Frame, T_Local);
         Stereo_Jac       : constant Raw_Stereo_Jacobian := Stereographic_Jacobian (Y);
         D_Y_Lambda       : constant Chart_Vector := Power_Chart_Value (D_Power_Coefficients, V);
         Envelope         : constant Dimensionless := Closure_Envelope_Value (V);
         D_T_Lambda_Local : Raw_Vector_4 := [others => 0.0];
      begin
         --  Accumulate T(U), ∂T/∂λ, and the three ∂T/∂C₀ components. The chain rule is
         --
         --     ∂T/∂x = Jₛ(Y)·∂Y/∂x,
         --
         --  where Jₛ is the inverse-stereographic Jacobian.
         Integral := Integral + T_Global * Weight;
         for I in Frame_Component_Index loop
            for J in Chart_Component_Index loop
               D_T_Lambda_Local (I) := D_T_Lambda_Local (I) + Stereo_Jac (I, J) * D_Y_Lambda (J);
            end loop;
         end loop;
         D_Integral_D_Lambda := D_Integral_D_Lambda + Frame_Vector (Frame, D_T_Lambda_Local) * Weight;

         for J in Chart_Component_Index loop
            declare
               Local : Raw_Vector_4 := [others => 0.0];
            begin
               for I in Frame_Component_Index loop
                  Local (I) := Stereo_Jac (I, J) * Envelope;
               end loop;
               D_Integral_D_C (J) := D_Integral_D_C (J) + Frame_Vector (Frame, Local) * Weight;
            end;
         end loop;
      end Accumulate;
   begin
      Build_Power_Charts;
      --  The direct Newton proposal uses a fixed composite rule. A separately certified integral gates every
      --  accepted candidate.
      for Segment in 0 .. Panel_Count - 1 loop
         declare
            Middle : constant Dimensionless := (Dimensionless (Segment) + 0.5) / Dimensionless (Panel_Count);
            Half   : constant Dimensionless := 0.5 / Dimensionless (Panel_Count);
         begin
            for I in GL16_Positive_Index loop
               Accumulate (Middle - Half * GL16_Nodes (I), Half * GL16_Weights (I));
               Accumulate (Middle + Half * GL16_Nodes (I), Half * GL16_Weights (I));
            end loop;
         end;
      end loop;

      declare
         Global_Residual : constant Position_Scale := Integral * Lambda - Chord_Direction;
         Global_D_Lambda : constant Position_Scale := Integral + D_Integral_D_Lambda * Lambda;
      begin
         --  Closure in chord-normalized coordinates is
         --
         --     R(λ, C₀) = λ·∫₀¹T(U; λ, C₀)dU - c = 0.
         --
         --  Resolve the four physical residual components into Closure_Frame so the Newton system is well scaled.
         for I in Frame_Component_Index loop
            Residual (I) := Frame_Coordinate (Closure_Frame, I, Global_Residual);
            D_Residual_D_Lambda (I) := Frame_Coordinate (Closure_Frame, I, Global_D_Lambda);
            for J in Chart_Component_Index loop
               Jacobian_C (I, J) := Lambda * Frame_Coordinate (Closure_Frame, I, D_Integral_D_C (J));
            end loop;
         end loop;
      end;
   end Closure_Residual;

   function Solve_Closure_Fixed
     (Frame                     : Frame_Vector_Array;
      Start_Jets, Finish_Jets   : Chart_Jet_Array;
      Chord_Direction           : Position_Scale;
      Maximum_Lambda, Tolerance : Dimensionless;
      Warp_Factor               : Dimensionless;
      Initial                   : Closure_Solution;
      Panel_Count               : Positive := 1) return Closure_Solution
   is
      Closure_Frame  : Frame_Vector_Array;
      Frame_OK       : constant Boolean :=
        Complete_Frame (Chord_Direction, Frame (1), Frame (2), Frame (3), Closure_Frame);
      Seed_Fractions : constant array (Positive range 1 .. 1) of Dimensionless := [0.1];
      Best           : Closure_Solution := (Success => False, others => <>);
      Best_C_Norm    : Dimensionless := Dimensionless'Last;

      function Norm (Value : Raw_Vector_4) return Dimensionless
      is (Dimensionless_Math.Sqrt (Value (0) ** 2 + Value (1) ** 2 + Value (2) ** 2 + Value (3) ** 2));

      function Solve_Linear (Matrix : Raw_Matrix_4; Right : Raw_Vector_4; Solution : out Raw_Vector_4) return Boolean;
      --  Solve a four-by-four linear system and report whether a stable solution was found.

      function Solve_Linear (Matrix : Raw_Matrix_4; Right : Raw_Vector_4; Solution : out Raw_Vector_4) return Boolean
      is
         Work : array (Frame_Component_Index, Natural range 0 .. 4) of Dimensionless := [others => [others => 0.0]];
      begin
         Solution := [others => 0.0];
         for Row in Frame_Component_Index loop
            for Column in Frame_Component_Index loop
               Work (Row, Column) := Matrix (Row, Column);
            end loop;
            Work (Row, 4) := Right (Row);
         end loop;
         --  Gaussian elimination with partial pivoting solves J·Δ = -R. Reject a pivot that is too small to support
         --  a trustworthy Newton direction.
         for Column in Frame_Component_Index loop
            declare
               Pivot : Frame_Component_Index := Column;
            begin
               for Row in Column .. Frame_Component_Index'Last loop
                  if abs Work (Row, Column) > abs Work (Pivot, Column) then
                     Pivot := Row;
                  end if;
               end loop;
               if abs Work (Pivot, Column) <= 1.0E-20 then
                  return False;
               end if;
               if Pivot /= Column then
                  for J in Column .. 4 loop
                     declare
                        Swap : constant Dimensionless := Work (Column, J);
                     begin
                        Work (Column, J) := Work (Pivot, J);
                        Work (Pivot, J) := Swap;
                     end;
                  end loop;
               end if;
               for Row in Column + 1 .. Frame_Component_Index'Last loop
                  declare
                     Factor : constant Dimensionless := Work (Row, Column) / Work (Column, Column);
                  begin
                     for J in Column .. 4 loop
                        Work (Row, J) := Work (Row, J) - Factor * Work (Column, J);
                     end loop;
                  end;
               end loop;
            end;
         end loop;
         for Row in reverse Frame_Component_Index loop
            declare
               Value : Dimensionless := Work (Row, 4);
            begin
               for Column in Row + 1 .. Frame_Component_Index'Last loop
                  Value := Value - Work (Row, Column) * Solution (Column);
               end loop;
               Solution (Row) := Value / Work (Row, Row);
            end;
         end loop;
         return True;
      exception
         when Constraint_Error =>
            Solution := [others => 0.0];
            return False;
      end Solve_Linear;
   begin
      if not Frame_OK or else Maximum_Lambda < 1.0 then
         return (Success => False, others => <>);
      end if;
      for Seed of Seed_Fractions loop
         declare
            --  λ is constrained to [1, Maximum_Lambda] because a unit-speed curve cannot be shorter than its chord.
            --  Start just inside that interval unless the caller supplied a previous solution.
            Lambda : Dimensionless :=
              (if Initial.Success then Initial.Lambda else 1.0 + Seed * Dimensionless'Min (Maximum_Lambda - 1.0, 1.0));
            C0     : Raw_Vector_3 := (if Initial.Success then Initial.C0 else [others => 0.0]);
         begin
            Iteration_Loop : for Iteration in 1 .. 20 loop
               declare
                  Residual, D_Lambda : Raw_Vector_4;
                  Jacobian_C         : Raw_Matrix_4_3;
               begin
                  Closure_Residual
                    (Frame,
                     Closure_Frame,
                     Start_Jets,
                     Finish_Jets,
                     Chord_Direction,
                     Lambda,
                     C0,
                     Warp_Factor,
                     Residual,
                     Jacobian_C,
                     D_Lambda,
                     Panel_Count => Panel_Count);
                  if Norm (Residual) <= Tolerance then
                     declare
                        C_Norm : constant Dimensionless :=
                          Dimensionless_Math.Sqrt (C0 (1) ** 2 + C0 (2) ** 2 + C0 (3) ** 2);
                     begin
                        --  If multiple seeds converge, retain the smallest closure bubble ‖C₀‖.
                        if C_Norm < Best_C_Norm then
                           Best := (Success => True, Lambda => Lambda, C0 => C0, others => <>);
                           Best_C_Norm := C_Norm;
                        end if;
                     end;
                     exit Iteration_Loop;
                  end if;
                  declare
                     Jacobian    : Raw_Matrix_4 := [others => [others => 0.0]];
                     Right       : Raw_Vector_4;
                     Newton_Step : Raw_Vector_4;
                     Solved      : Boolean;
                  begin
                     for Row in Frame_Component_Index loop
                        Jacobian (Row, 0) := D_Lambda (Row);
                        Right (Row) := -Residual (Row);
                        for Column in Chart_Component_Index loop
                           Jacobian (Row, Column) := Jacobian_C (Row, Column);
                        end loop;
                     end loop;
                     Solved := Solve_Linear (Jacobian, Right, Newton_Step);
                     exit Iteration_Loop when not Solved;
                     declare
                        Scale        : Dimensionless := 1.0;
                        Accepted     : Boolean := False;
                        Current_Norm : constant Dimensionless := Norm (Residual);
                     begin
                        --  Backtrack Δ by powers of two until ‖R(x + α·Δ)‖ < ‖R(x)‖. Clamp λ but leave C₀
                        --  unconstrained so all three closure modes remain available.
                        for Trial in 1 .. 8 loop
                           declare
                              Trial_Lambda                   : constant Dimensionless :=
                                Dimensionless'Max
                                  (1.0, Dimensionless'Min (Maximum_Lambda, Lambda + Scale * Newton_Step (0)));
                              Trial_C0                       : Raw_Vector_3 := C0;
                              Trial_Residual, Trial_D_Lambda : Raw_Vector_4;
                              Trial_Jacobian                 : Raw_Matrix_4_3;
                           begin
                              for Column in Chart_Component_Index loop
                                 Trial_C0 (Column) := Trial_C0 (Column) + Scale * Newton_Step (Column);
                              end loop;
                              Closure_Residual
                                (Frame,
                                 Closure_Frame,
                                 Start_Jets,
                                 Finish_Jets,
                                 Chord_Direction,
                                 Trial_Lambda,
                                 Trial_C0,
                                 Warp_Factor,
                                 Trial_Residual,
                                 Trial_Jacobian,
                                 Trial_D_Lambda,
                                 Panel_Count => Panel_Count);
                              if Norm (Trial_Residual) < Current_Norm then
                                 Lambda := Trial_Lambda;
                                 C0 := Trial_C0;
                                 Accepted := True;
                                 exit;
                              end if;
                           end;
                           Scale := 0.5 * Scale;
                        end loop;
                        exit Iteration_Loop when not Accepted;
                     end;
                  end;
               end;
            end loop Iteration_Loop;
         end;
      end loop;
      return Best;
   exception
      when Constraint_Error =>
         return (Success => False, Numerically_Unsafe => True, others => <>);
   end Solve_Closure_Fixed;

   ----------------------------------------------------------------------------------------------------------------
   --  Outward-rounded bounds for the retained realtime tangent
   --
   --  V7 prefers a direct rational Bernstein certificate for nontrivial ranges. The earlier pole-based Taylor method
   --  remains as a fail-closed fallback and is still sharper for a point-sized query, where partial-fraction terms can
   --  cancel before their absolute values are bounded.
   ----------------------------------------------------------------------------------------------------------------

   function Pole_Taylor_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless) return Axis_Majorants
   is
      --  The public bounds require A_0 through A_4, where
      --
      --     A_n (U) = T^(n) (U) / n!.
      --
      --  Two additional coefficients support a second-order Taylor enclosure on each cell.
      Maximum_Internal_Order : constant := Maximum_Majorant_Order + 2;
      subtype Internal_Order is Natural range 0 .. Maximum_Internal_Order;
      type Internal_Scalar_Intervals is array (Internal_Order) of Interval;
      type Internal_Axis_Intervals is array (Axis_Name) of Internal_Scalar_Intervals;

      Cell_Count : constant Positive := 64;
      Result     : Axis_Majorants := [others => [others => 0.0]];
      Cache      : Rational_Antiderivative renames Curve.Evaluator_Data.Antiderivative_Cache;
      Length_Raw : constant Dimensionless := Dimensionless (Curve.Evaluator_Data.Length_Value / mm);

      type Interval_Complex is record
         Real_Part      : Interval;
         Imaginary_Part : Interval;
      end record;

      function Unbounded_Interval return Interval
      is ((Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False));

      function Divide_By_Positive (Numerator, Denominator : Interval) return Interval;
      --  Enclose division by an interval that is known to be strictly positive.

      function Divide_By_Positive (Numerator, Denominator : Interval) return Interval is
      begin
         if not Numerator.Valid or else not Denominator.Valid or else Denominator.Lower <= 0.0 then
            return Unbounded_Interval;
         elsif Numerator.Lower = 0.0 and then Numerator.Upper = 0.0 then
            return Interval_Exact (0.0);
         end if;
         declare
            Q1 : constant Dimensionless := Numerator.Lower / Denominator.Lower;
            Q2 : constant Dimensionless := Numerator.Lower / Denominator.Upper;
            Q3 : constant Dimensionless := Numerator.Upper / Denominator.Lower;
            Q4 : constant Dimensionless := Numerator.Upper / Denominator.Upper;
         begin
            return
              Checked_Interval
                (Down (Dimensionless'Min (Dimensionless'Min (Q1, Q2), Dimensionless'Min (Q3, Q4))),
                 Up (Dimensionless'Max (Dimensionless'Max (Q1, Q2), Dimensionless'Max (Q3, Q4))));
         end;
      exception
         when Constraint_Error =>
            return Unbounded_Interval;
      end Divide_By_Positive;

      function Complex_Multiply (Left, Right : Interval_Complex) return Interval_Complex;
      --  Return an interval enclosure of the product of two complex intervals.

      function Complex_Multiply (Left, Right : Interval_Complex) return Interval_Complex is
      begin
         return
           (Real_Part      =>
              Interval_Subtract
                (Interval_Multiply (Left.Real_Part, Right.Real_Part),
                 Interval_Multiply (Left.Imaginary_Part, Right.Imaginary_Part)),
            Imaginary_Part =>
              Interval_Add
                (Interval_Multiply (Left.Real_Part, Right.Imaginary_Part),
                 Interval_Multiply (Left.Imaginary_Part, Right.Real_Part)));
      end Complex_Multiply;

      function Reciprocal_At (U, Pole_Real, Pole_Imaginary : Dimensionless) return Interval_Complex;
      --  Enclose the reciprocal of U minus the supplied complex pole.

      function Reciprocal_At (U, Pole_Real, Pole_Imaginary : Dimensionless) return Interval_Complex is
         Real_Difference  : constant Interval := Interval_Subtract (Interval_Exact (U), Interval_Exact (Pole_Real));
         Imaginary        : constant Interval := Interval_Exact (Pole_Imaginary);
         Real_Square      : Interval := Interval_Multiply (Real_Difference, Real_Difference);
         Imaginary_Square : Interval := Interval_Multiply (Imaginary, Imaginary);
         Denominator      : Interval;
      begin
         --  Dependency can give a slightly negative lower endpoint for X*X. The analytic square is nonnegative.
         Real_Square.Lower := Dimensionless'Max (0.0, Real_Square.Lower);
         Imaginary_Square.Lower := Dimensionless'Max (0.0, Imaginary_Square.Lower);
         Denominator := Interval_Add (Real_Square, Imaginary_Square);
         Denominator.Lower := Dimensionless'Max (0.0, Denominator.Lower);
         return
           (Real_Part      => Divide_By_Positive (Real_Difference, Denominator),
            Imaginary_Part => Divide_By_Positive (Imaginary, Denominator));
      exception
         when Constraint_Error =>
            return (Real_Part => Unbounded_Interval, Imaginary_Part => Unbounded_Interval);
      end Reciprocal_At;

      function Safe_Add (Left, Right : Dimensionless) return Dimensionless;
      --  Add nonnegative bounds with upward rounding and saturation.

      function Safe_Add (Left, Right : Dimensionless) return Dimensionless is
      begin
         if Left >= Dimensionless'Last or else Right >= Dimensionless'Last or else Left < 0.0 or else Right < 0.0 then
            return Dimensionless'Last;
         elsif Left = 0.0 then
            return Right;
         elsif Right = 0.0 then
            return Left;
         end if;
         declare
            Value : constant Dimensionless := Up (Left + Right);
         begin
            return (if Is_Finite (Value) then Value else Dimensionless'Last);
         end;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Safe_Add;

      function Safe_Multiply (Left, Right : Dimensionless) return Dimensionless;
      --  Multiply nonnegative bounds with upward rounding and saturation.

      function Safe_Multiply (Left, Right : Dimensionless) return Dimensionless is
      begin
         if Left >= Dimensionless'Last or else Right >= Dimensionless'Last or else Left < 0.0 or else Right < 0.0 then
            return Dimensionless'Last;
         elsif Left = 0.0 or else Right = 0.0 then
            return 0.0;
         elsif Left = 1.0 then
            return Right;
         elsif Right = 1.0 then
            return Left;
         end if;
         declare
            Value : constant Dimensionless := Up (Left * Right);
         begin
            return (if Is_Finite (Value) then Value else Dimensionless'Last);
         end;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Safe_Multiply;

      function Lower_Power (Base : Dimensionless; Exponent : Positive) return Dimensionless;
      --  Compute a downward-rounded positive integer power.

      function Lower_Power (Base : Dimensionless; Exponent : Positive) return Dimensionless is
         Value : Dimensionless := 1.0;
      begin
         if Base <= 0.0 or else not Is_Finite (Base) then
            return 0.0;
         end if;
         for I in 1 .. Exponent loop
            Value := Down (Value * Base);
            if Value <= 0.0 or else not Is_Finite (Value) then
               return 0.0;
            end if;
         end loop;
         return Value;
      exception
         when Constraint_Error =>
            return 0.0;
      end Lower_Power;

      function Safe_Quotient (Numerator, Denominator : Dimensionless) return Dimensionless;
      --  Divide nonnegative bounds with upward rounding and saturation.

      function Safe_Quotient (Numerator, Denominator : Dimensionless) return Dimensionless is
      begin
         if Numerator >= Dimensionless'Last or else Numerator < 0.0 or else Denominator <= 0.0 then
            return Dimensionless'Last;
         elsif Numerator = 0.0 then
            return 0.0;
         end if;
         declare
            Value : constant Dimensionless := Up (Numerator / Denominator);
         begin
            return (if Is_Finite (Value) then Value else Dimensionless'Last);
         end;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Safe_Quotient;

      function Upper_Hypot (X, Y : Dimensionless) return Dimensionless;
      --  Return an upward-rounded bound for the Euclidean norm of (X, Y).

      function Upper_Hypot (X, Y : Dimensionless) return Dimensionless is
         AX    : constant Dimensionless := abs X;
         AY    : constant Dimensionless := abs Y;
         Scale : constant Dimensionless := Dimensionless'Max (AX, AY);
      begin
         if not Is_Finite (X) or else not Is_Finite (Y) or else Scale >= Dimensionless'Last then
            return Dimensionless'Last;
         elsif Scale = 0.0 then
            return 0.0;
         end if;
         declare
            RX  : constant Dimensionless := Up (AX / Scale);
            RY  : constant Dimensionless := Up (AY / Scale);
            Sum : constant Dimensionless := Safe_Add (Safe_Multiply (RX, RX), Safe_Multiply (RY, RY));
         begin
            if Sum >= Dimensionless'Last then
               return Dimensionless'Last;
            else
               return Safe_Multiply (Scale, Up (Dimensionless_Math.Sqrt (Sum)));
            end if;
         end;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Upper_Hypot;

      function Correction_Coefficient (Order : Internal_Order; U : Dimensionless) return Interval;
      --  Enclose one correction-series coefficient at U.

      function Correction_Coefficient (Order : Internal_Order; U : Dimensionless) return Interval is
         type Controls is array (Natural range 0 .. 11) of Interval;
         Work        : Controls := [for Index in 0 .. 11 => Interval_Exact (if Index <= 5 then 0.0 else 1.0)];
         Degree      : Natural := 11;
         T           : constant Interval := Interval_Exact (U);
         One_Minus_T : constant Interval := Interval_Subtract (Interval_Exact (1.0), T);
      begin
         --  H has degree-eleven Bernstein controls [0,0,0,0,0,0,1,1,1,1,1,1]. Repeated differences form
         --  H^(Order + 1), and division by Order! converts it to the coefficient contributed to A_Order.
         for Derivative in 1 .. Order + 1 loop
            for Index in 0 .. Degree - 1 loop
               Work (Index) :=
                 Interval_Multiply
                   (Interval_Exact (Dimensionless (Degree)), Interval_Subtract (Work (Index + 1), Work (Index)));
            end loop;
            Degree := Degree - 1;
         end loop;
         if Order > 1 then
            for Index in 0 .. Degree loop
               Work (Index) := Divide_By_Positive (Work (Index), Interval_Exact (Factorial (Order)));
            end loop;
         end if;
         for Level in 1 .. Degree loop
            for Index in 0 .. Degree - Level loop
               Work (Index) :=
                 Interval_Add (Interval_Multiply (One_Minus_T, Work (Index)), Interval_Multiply (T, Work (Index + 1)));
            end loop;
         end loop;
         return Work (0);
      exception
         when Constraint_Error =>
            return Unbounded_Interval;
      end Correction_Coefficient;

      function Correction_Coefficient_Bound (Order : Internal_Order) return Dimensionless;
      --  Bound one correction coefficient over the full parameter interval.

      function Correction_Coefficient_Bound (Order : Internal_Order) return Dimensionless is
         type Controls is array (Natural range 0 .. 11) of Interval;
         Work   : Controls := [for Index in 0 .. 11 => Interval_Exact (if Index <= 5 then 0.0 else 1.0)];
         Degree : Natural := 11;
         Bound  : Dimensionless := 0.0;
      begin
         for Derivative in 1 .. Order + 1 loop
            for Index in 0 .. Degree - 1 loop
               Work (Index) :=
                 Interval_Multiply
                   (Interval_Exact (Dimensionless (Degree)), Interval_Subtract (Work (Index + 1), Work (Index)));
            end loop;
            Degree := Degree - 1;
         end loop;
         if Order > 1 then
            for Index in 0 .. Degree loop
               Work (Index) := Divide_By_Positive (Work (Index), Interval_Exact (Factorial (Order)));
            end loop;
         end if;
         for Index in 0 .. Degree loop
            Bound := Dimensionless'Max (Bound, Interval_Abs_Max (Work (Index)));
         end loop;
         return Bound;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Correction_Coefficient_Bound;

      function Midpoint_Coefficients (U : Dimensionless) return Internal_Axis_Intervals;
      --  Evaluate interval correction coefficients at the midpoint parameter U.

      function Midpoint_Coefficients (U : Dimensionless) return Internal_Axis_Intervals is
         Values : Internal_Axis_Intervals := [others => [others => Interval_Exact (0.0)]];

         procedure Accumulate_Pole
           (Pole_Real, Pole_Imaginary       : Dimensionless;
            Real_Residue, Imaginary_Residue : Dimensionless_Axis_Vector;
            Pair                            : Boolean);
         --  Accumulate one real pole or conjugate-pole pair into the midpoint coefficients.

         procedure Accumulate_Pole
           (Pole_Real, Pole_Imaginary       : Dimensionless;
            Real_Residue, Imaginary_Residue : Dimensionless_Axis_Vector;
            Pair                            : Boolean)
         is
            Reciprocal : constant Interval_Complex := Reciprocal_At (U, Pole_Real, Pole_Imaginary);
            Power      : Interval_Complex := Reciprocal;
         begin
            for Order in Internal_Order loop
               for Axis in Axis_Name loop
                  declare
                     Term : Interval :=
                       Interval_Subtract
                         (Interval_Multiply (Interval_Exact (Real_Residue (Axis)), Power.Real_Part),
                          Interval_Multiply (Interval_Exact (Imaginary_Residue (Axis)), Power.Imaginary_Part));
                  begin
                     if Pair then
                        Term := Interval_Multiply (Interval_Exact (2.0), Term);
                     end if;
                     if Order mod 2 = 1 then
                        Term := Interval_Negate (Term);
                     end if;
                     Values (Axis) (Order) := Interval_Add (Values (Axis) (Order), Term);
                  end;
               end loop;
               if Order < Internal_Order'Last then
                  Power := Complex_Multiply (Power, Reciprocal);
               end if;
            end loop;
         end Accumulate_Pole;
      begin
         for Axis in Axis_Name loop
            Values (Axis) (0) := Interval_Exact (Cache.Constant_Tangent (Axis));
         end loop;
         for Index in 1 .. Cache.Real_Pole_Count loop
            Accumulate_Pole
              (Cache.Pole_Slots (Index).Pole_Component,
               0.0,
               Cache.Pole_Slots (Index).Residue_Component,
               [others => 0.0],
               False);
         end loop;
         for Pair in 1 .. Cache.Pair_Count loop
            declare
               Real_Slot      : Rational_Pole_Slot renames
                 Cache.Pole_Slots (Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)));
               Imaginary_Slot : Rational_Pole_Slot renames
                 Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)));
            begin
               Accumulate_Pole
                 (Real_Slot.Pole_Component,
                  Imaginary_Slot.Pole_Component,
                  Real_Slot.Residue_Component,
                  Imaginary_Slot.Residue_Component,
                  True);
            end;
         end loop;
         for Axis in Axis_Name loop
            declare
               Correction : constant Interval := Finish_Correction_Interval (Curve.Evaluator_Data, Axis);
            begin
               if Correction.Lower /= 0.0 or else Correction.Upper /= 0.0 then
                  declare
                     Ratio : constant Interval := Divide_By_Positive (Correction, Interval_Exact (Length_Raw));
                  begin
                     for Order in Internal_Order loop
                        Values (Axis) (Order) :=
                          Interval_Add
                            (Values (Axis) (Order), Interval_Multiply (Ratio, Correction_Coefficient (Order, U)));
                     end loop;
                  end;
               end if;
            end;
         end loop;
         return Values;
      exception
         when Constraint_Error =>
            return [others => [others => Unbounded_Interval]];
      end Midpoint_Coefficients;

      function Real_Distance_Lower (Pole, Cell_Left, Cell_Right : Dimensionless) return Dimensionless;
      --  Return a lower bound on the distance from a real pole to a parameter cell.

      function Real_Distance_Lower (Pole, Cell_Left, Cell_Right : Dimensionless) return Dimensionless is
         Distance : Dimensionless;
      begin
         if Pole < Cell_Left then
            Distance := Cell_Left - Pole;
         elsif Pole > Cell_Right then
            Distance := Pole - Cell_Right;
         else
            return 0.0;
         end if;
         return Dimensionless'Max (0.0, Down (Distance));
      exception
         when Constraint_Error =>
            return 0.0;
      end Real_Distance_Lower;

      function Complex_Distance_Lower
        (Pole_Real, Pole_Imaginary, Cell_Left, Cell_Right : Dimensionless) return Dimensionless;
      --  Return a lower bound on the distance from a complex pole to a parameter cell.

      function Complex_Distance_Lower
        (Pole_Real, Pole_Imaginary, Cell_Left, Cell_Right : Dimensionless) return Dimensionless
      is
         Horizontal : Dimensionless := 0.0;
      begin
         if Pole_Real < Cell_Left then
            Horizontal := Dimensionless'Max (0.0, Down (Cell_Left - Pole_Real));
         elsif Pole_Real > Cell_Right then
            Horizontal := Dimensionless'Max (0.0, Down (Pole_Real - Cell_Right));
         end if;
         declare
            Horizontal_Square : constant Dimensionless := Down (Horizontal * Horizontal);
            Imaginary_Square  : constant Dimensionless := Down (abs Pole_Imaginary * abs Pole_Imaginary);
            Square            : constant Dimensionless := Down (Horizontal_Square + Imaginary_Square);
         begin
            if Square <= 0.0 or else not Is_Finite (Square) then
               return 0.0;
            else
               return Dimensionless'Max (0.0, Down (Dimensionless_Math.Sqrt (Square)));
            end if;
         end;
      exception
         when Constraint_Error =>
            return 0.0;
      end Complex_Distance_Lower;

      function Remainder_Bound
        (Axis : Axis_Name; Order : Internal_Order; Cell_Left, Cell_Right : Dimensionless) return Dimensionless;
      --  Bound the truncated correction-series remainder for one axis and cell.

      function Remainder_Bound
        (Axis : Axis_Name; Order : Internal_Order; Cell_Left, Cell_Right : Dimensionless) return Dimensionless
      is
         Bound : Dimensionless := 0.0;
      begin
         for Index in 1 .. Cache.Real_Pole_Count loop
            declare
               Distance : constant Dimensionless :=
                 Real_Distance_Lower (Cache.Pole_Slots (Index).Pole_Component, Cell_Left, Cell_Right);
               Power    : constant Dimensionless := Lower_Power (Distance, Order + 1);
               Term     : constant Dimensionless :=
                 Safe_Quotient (abs Cache.Pole_Slots (Index).Residue_Component (Axis), Power);
            begin
               Bound := Safe_Add (Bound, Term);
            end;
         end loop;
         for Pair in 1 .. Cache.Pair_Count loop
            declare
               Real_Slot      : Rational_Pole_Slot renames
                 Cache.Pole_Slots (Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)));
               Imaginary_Slot : Rational_Pole_Slot renames
                 Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)));
               Distance       : constant Dimensionless :=
                 Complex_Distance_Lower
                   (Real_Slot.Pole_Component, Imaginary_Slot.Pole_Component, Cell_Left, Cell_Right);
               Power          : constant Dimensionless := Lower_Power (Distance, Order + 1);
               Residue_Norm   : constant Dimensionless :=
                 Upper_Hypot (Real_Slot.Residue_Component (Axis), Imaginary_Slot.Residue_Component (Axis));
               Term           : constant Dimensionless := Safe_Quotient (Safe_Multiply (2.0, Residue_Norm), Power);
            begin
               Bound := Safe_Add (Bound, Term);
            end;
         end loop;
         declare
            Correction : constant Interval := Finish_Correction_Interval (Curve.Evaluator_Data, Axis);
         begin
            if Correction.Lower /= 0.0 or else Correction.Upper /= 0.0 then
               declare
                  Ratio : constant Dimensionless := Safe_Quotient (Interval_Abs_Max (Correction), Length_Raw);
               begin
                  Bound := Safe_Add (Bound, Safe_Multiply (Ratio, Correction_Coefficient_Bound (Order)));
               end;
            end if;
         end;
         return Bound;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Remainder_Bound;
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return Result;
      elsif Length_Raw <= 0.0 or else not Is_Finite (Length_Raw) then
         return [others => [others => Dimensionless'Last]];
      end if;

      declare
         Point_Sized_Range : constant Boolean :=
           End_U - Start_U <= 8.0 * Dimensionless'Model_Epsilon * (1.0 + Dimensionless'Max (abs Start_U, abs End_U));
         Cells             : constant Positive := (if Point_Sized_Range then 1 else Cell_Count);
         Previous_Right    : Dimensionless := Start_U;
      begin
         for Cell in 0 .. Cells - 1 loop
            declare
               Cell_Left  : constant Dimensionless := (if Cell = 0 then Start_U else Previous_Right);
               Cell_Right : constant Dimensionless :=
                 (if Cell = Cells - 1
                  then End_U
                  else Start_U + (End_U - Start_U) * Dimensionless (Cell + 1) / Dimensionless (Cells));
               Midpoint   : constant Dimensionless :=
                 (if Cell_Left = Cell_Right then Cell_Left else 0.5 * Cell_Left + 0.5 * Cell_Right);
               Radius     : constant Dimensionless :=
                 (if Cell_Left = Cell_Right
                  then 0.0
                  else Up (Dimensionless'Max (abs (Midpoint - Cell_Left), abs (Cell_Right - Midpoint))));
               Middle     : constant Internal_Axis_Intervals := Midpoint_Coefficients (Midpoint);
            begin
               Previous_Right := Cell_Right;
               for Axis in Axis_Name loop
                  if not (Curve.Structurally_Constant_Axes (Axis)
                          and then
                            Curve.Evaluator_Data.Finish_Point (Axis)
                            = Curve.Evaluator_Data.Uncorrected_Finish_Point (Axis))
                  then
                     for Order in Majorant_Order loop
                        declare
                           First_Term            : constant Dimensionless := Interval_Abs_Max (Middle (Axis) (Order));
                           Linear_Term           : constant Dimensionless :=
                             Safe_Multiply
                               (Dimensionless (Order + 1),
                                Safe_Multiply (Radius, Interval_Abs_Max (Middle (Axis) (Order + 1))));
                           Quadratic_Coefficient : constant Dimensionless :=
                             Dimensionless ((Order + 1) * (Order + 2)) / 2.0;
                           Quadratic_Term        : constant Dimensionless :=
                             Safe_Multiply
                               (Quadratic_Coefficient,
                                Safe_Multiply
                                  (Safe_Multiply (Radius, Radius),
                                   Remainder_Bound (Axis, Order + 2, Cell_Left, Cell_Right)));
                           Cell_Bound            : constant Dimensionless :=
                             Safe_Add (First_Term, Safe_Add (Linear_Term, Quadratic_Term));
                        begin
                           Result (Axis) (Order) := Dimensionless'Max (Result (Axis) (Order), Cell_Bound);
                        end;
                     end loop;
                  end if;
               end loop;
            end;
         end loop;
      end;
      return Result;
   exception
      when Constraint_Error =>
         return [others => [others => Dimensionless'Last]];
   end Pole_Taylor_Tangent_Range_Majorants;

   function Bernstein_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless; Success : out Boolean) return Axis_Majorants
   is
      --  This path operates on the common-denominator Bernstein certificate retained while the realtime evaluator is
      --  compiled. It reconstructs each factorial-scaled derivative as a rational Bernstein polynomial, restricts it
      --  to the requested range, and bounds it by outward-rounded De Casteljau subdivision. The smooth endpoint
      --  correction is bounded independently and added only after the rational part has been certified.
      subtype Polynomial_Index is Natural range 0 .. Maximum_Derivative_Bernstein_Degree;
      type Polynomial is array (Polynomial_Index) of Interval;
      type Axis_Polynomials is array (Axis_Name) of Polynomial;
      type Axis_Bounds is array (Axis_Name) of Dimensionless;
      type Axis_Flags is array (Axis_Name) of Boolean;
      type Correction_Bound_Array is array (Majorant_Order) of Dimensionless;

      Maximum_Subdivision_Depth : constant := 2;
      --  Four leaf hulls retain margin below the five-percent dense-oracle tolerance, including the supported 20:1
      --  warps, while avoiding the old fixed sixty-four-cell cost.

      Tightness_Factor : constant Dimensionless := 1.02;
      --  Accept a valid rational hull once it is within two percent of the magnitude already forced by a segment
      --  endpoint. Axes which remain looser are subdivided independently.

      Tightness_Floor : constant Dimensionless := 1.0E-12;
      --  Absolute allowance in the tightness comparison. Without this floor, an axis whose true value is near zero
      --  would always consume the full subdivision depth despite already having a negligible bound.

      Certificate              : Retained_Tangent_Bernstein_Certificate renames Curve.Retained_Tangent_Certificate;
      Q_Degree                 : constant Natural := Certificate.Degree;
      Q                        : Polynomial := [others => Interval_Exact (0.0)];
      Q_Derivative             : Polynomial := [others => Interval_Exact (0.0)];
      Q_Derivative_Degree      : Natural := 0;
      Denominator_Power        : Polynomial := [others => Interval_Exact (0.0)];
      Denominator_Power_Degree : Natural := Q_Degree;
      Numerators               : Axis_Polynomials := [others => [others => Interval_Exact (0.0)]];
      Numerator_Degree         : Natural range 0 .. Maximum_Derivative_Bernstein_Degree := Q_Degree;
      Zero_Numerator_Axes      : Axis_Flags := [others => False];
      Result                   : Axis_Majorants := [others => [others => 0.0]];
      Length_Raw               : constant Dimensionless := Dimensionless (Curve.Evaluator_Data.Length_Value / mm);

      --  Return the package's fail-closed interval sentinel. Propagating this value makes any unsafe intermediate
      --  calculation turn into Success = False instead of an apparently finite certificate.
      function Invalid_Interval return Interval
      is (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);

      --  Divide two intervals with outward rounding. A denominator which is invalid or may contain zero cannot prove
      --  a finite quotient and therefore produces Invalid_Interval.
      function Divide_Intervals (Numerator, Denominator : Interval) return Interval;
      --  Enclose general interval division when the denominator excludes zero.

      function Divide_Intervals (Numerator, Denominator : Interval) return Interval is
      begin
         if not Numerator.Valid
           or else not Denominator.Valid
           or else (Denominator.Lower <= 0.0 and then Denominator.Upper >= 0.0)
         then
            return Invalid_Interval;
         end if;
         declare
            Reciprocal : constant Interval :=
              Checked_Interval (Down (1.0 / Denominator.Upper), Up (1.0 / Denominator.Lower));
         begin
            return Interval_Multiply (Numerator, Reciprocal);
         end;
      exception
         when Constraint_Error =>
            return Invalid_Interval;
      end Divide_Intervals;

      --  Enclose the Bernstein product or degree-elevation weight
      --
      --     C(Left_N, Left_K) * C(Right_N, Right_K) / C(Denominator_N, Denominator_K).
      --
      --  The integer products are exact; only their final conversion and division need outward rounding.
      function Exact_Binomial_Product_Ratio
        (Left_N, Left_K, Right_N, Right_K, Denominator_N, Denominator_K : Natural) return Interval;
      --  Enclose a ratio of exact binomial-coefficient products.

      function Exact_Binomial_Product_Ratio
        (Left_N, Left_K, Right_N, Right_K, Denominator_N, Denominator_K : Natural) return Interval
      is
         use type Interfaces.Unsigned_128;
         Numerator   : constant Exact_Binomial_Value :=
           Exact_Binomial (Left_N, Left_K) * Exact_Binomial (Right_N, Right_K);
         Denominator : constant Exact_Binomial_Value := Exact_Binomial (Denominator_N, Denominator_K);
      begin
         if Denominator = 0 then
            return Invalid_Interval;
         elsif Numerator = 0 then
            return Interval_Exact (0.0);
         elsif Numerator = Denominator then
            return Interval_Exact (1.0);
         end if;
         declare
            Numerator_Centre   : constant Dimensionless := Dimensionless (Numerator);
            Denominator_Centre : constant Dimensionless := Dimensionless (Denominator);
            Numerator_Bound    : constant Interval :=
              Checked_Interval (Down (Numerator_Centre), Up (Numerator_Centre));
            Denominator_Bound  : constant Interval :=
              Checked_Interval (Down (Denominator_Centre), Up (Denominator_Centre));
         begin
            --  Both bounds are strictly positive. Specializing their quotient avoids constructing a reciprocal and
            --  feeding it through the generic four-product interval multiplier for every Bernstein product weight.
            return
              Checked_Interval
                (Down (Numerator_Bound.Lower / Denominator_Bound.Upper),
                 Up (Numerator_Bound.Upper / Denominator_Bound.Lower));
         end;
      exception
         when Constraint_Error =>
            return Invalid_Interval;
      end Exact_Binomial_Product_Ratio;

      --  Multiply two Bernstein polynomials without converting through the cancellation-prone power basis. The
      --  weighted convolution uses Exact_Binomial_Product_Ratio so every resulting control remains an enclosure.
      function Multiply_Bernstein
        (Left : Polynomial; Left_Degree : Natural; Right : Polynomial; Right_Degree : Natural) return Polynomial;
      --  Multiply two Bernstein polynomials using exact combinatorial weights.

      function Multiply_Bernstein
        (Left : Polynomial; Left_Degree : Natural; Right : Polynomial; Right_Degree : Natural) return Polynomial
      is
         Result         : Polynomial := [others => Interval_Exact (0.0)];
         Product_Degree : constant Natural := Left_Degree + Right_Degree;
      begin
         if Product_Degree > Maximum_Derivative_Bernstein_Degree then
            return [others => Invalid_Interval];
         end if;
         for I in 0 .. Left_Degree loop
            for J in 0 .. Right_Degree loop
               declare
                  Product_Index : constant Natural := I + J;
                  Weight        : constant Interval :=
                    Exact_Binomial_Product_Ratio (Left_Degree, I, Right_Degree, J, Product_Degree, Product_Index);
               begin
                  Result (Product_Index) :=
                    Interval_Add
                      (Result (Product_Index), Interval_Multiply (Weight, Interval_Multiply (Left (I), Right (J))));
               end;
            end loop;
         end loop;
         return Result;
      exception
         when Constraint_Error =>
            return [others => Invalid_Interval];
      end Multiply_Bernstein;

      --  Re-express Source at Target_Degree while preserving the represented polynomial. Degree elevation lets
      --  independently derived numerators and denominators share an index before interval hull operations.
      function Elevate_Bernstein (Source : Polynomial; Source_Degree, Target_Degree : Natural) return Polynomial;
      --  Elevate a Bernstein polynomial to Target_Degree without changing its value.

      function Elevate_Bernstein (Source : Polynomial; Source_Degree, Target_Degree : Natural) return Polynomial is
         Result : Polynomial := [others => Interval_Exact (0.0)];
      begin
         if Source_Degree > Target_Degree or else Target_Degree > Maximum_Derivative_Bernstein_Degree then
            return [others => Invalid_Interval];
         elsif Source_Degree = Target_Degree then
            return Source;
         end if;
         for Target_Index in 0 .. Target_Degree loop
            declare
               Degree_Increase : constant Natural := Target_Degree - Source_Degree;
               First_Source    : constant Natural :=
                 (if Target_Index > Degree_Increase then Target_Index - Degree_Increase else 0);
               Last_Source     : constant Natural := Natural'Min (Source_Degree, Target_Index);
            begin
               for Source_Index in First_Source .. Last_Source loop
                  declare
                     Weight : constant Interval :=
                       Exact_Binomial_Product_Ratio
                         (Source_Degree,
                          Source_Index,
                          Degree_Increase,
                          Target_Index - Source_Index,
                          Target_Degree,
                          Target_Index);
                  begin
                     Result (Target_Index) :=
                       Interval_Add (Result (Target_Index), Interval_Multiply (Weight, Source (Source_Index)));
                  end;
               end loop;
            end;
         end loop;
         return Result;
      exception
         when Constraint_Error =>
            return [others => Invalid_Interval];
      end Elevate_Bernstein;

      --  Differentiate a Bernstein polynomial by taking scaled forward differences of adjacent controls.
      function Differentiate_Bernstein
        (Source : Polynomial; Source_Degree : Natural; Result_Degree : out Natural) return Polynomial;
      --  Differentiate a Bernstein polynomial and return its resulting degree.

      function Differentiate_Bernstein
        (Source : Polynomial; Source_Degree : Natural; Result_Degree : out Natural) return Polynomial
      is
         Result : Polynomial := [others => Interval_Exact (0.0)];
      begin
         if Source_Degree = 0 then
            Result_Degree := 0;
            return Result;
         end if;
         Result_Degree := Source_Degree - 1;
         for Index in 0 .. Result_Degree loop
            Result (Index) :=
              Interval_Multiply
                (Interval_Exact (Dimensionless (Source_Degree)),
                 Interval_Subtract (Source (Index + 1), Source (Index)));
         end loop;
         return Result;
      exception
         when Constraint_Error =>
            Result_Degree := 0;
            return [others => Invalid_Interval];
      end Differentiate_Bernstein;

      --  Multiply every active control by the same enclosing scale.
      function Scale_Bernstein (Source : Polynomial; Degree : Natural; Scale : Interval) return Polynomial;
      --  Multiply the active Bernstein coefficients by an interval scale.

      function Scale_Bernstein (Source : Polynomial; Degree : Natural; Scale : Interval) return Polynomial is
         Result : Polynomial := [others => Interval_Exact (0.0)];
      begin
         for Index in 0 .. Degree loop
            Result (Index) := Interval_Multiply (Scale, Source (Index));
         end loop;
         return Result;
      end Scale_Bernstein;

      --  Recognize an algebraically zero interval polynomial. This is deliberately exact: a merely small numerator
      --  still needs normal certification and must not be discarded as numerical noise.
      function Is_Exact_Zero (Value : Polynomial; Degree : Natural) return Boolean;
      --  Test whether every active coefficient is the exact zero interval.

      function Is_Exact_Zero (Value : Polynomial; Degree : Natural) return Boolean is
      begin
         return
           (for all Index in 0 .. Degree =>
              Value (Index).Valid and then Value (Index).Lower = 0.0 and then Value (Index).Upper = 0.0);
      end Is_Exact_Zero;

      --  Split Source at an interval-enclosed parameter using the De Casteljau triangle. Left and Right represent the
      --  two restricted polynomial pieces, including uncertainty in the split parameter.
      procedure Split_Bernstein (Source : Polynomial; Degree : Natural; T : Interval; Left, Right : out Polynomial);
      --  Split a Bernstein polynomial at the interval parameter T.

      procedure Split_Bernstein (Source : Polynomial; Degree : Natural; T : Interval; Left, Right : out Polynomial) is
         Work        : Polynomial := Source;
         One_Minus_T : constant Interval := Interval_Subtract (Interval_Exact (1.0), T);

         function Half (Value : Interval) return Interval;
         --  Scale an interval by one half with outward rounding.

         function Half (Value : Interval) return Interval is
         begin
            if not Value.Valid then
               return Invalid_Interval;
            end if;
            return Checked_Interval (Down (0.5 * Value.Lower), Up (0.5 * Value.Upper));
         end Half;

         function Interpolate (First, Last : Interval) return Interval;
         --  Interpolate two coefficients using the enclosing split parameter.

         function Interpolate (First, Last : Interval) return Interval is
         begin
            if T.Valid and then T.Lower = 0.5 and then T.Upper = 0.5 then
               --  Keep invalid-control midpoint propagation cheap on the generic path without using the four-product
               --  interval multiplier. Certified adaptive refinements use the dedicated splitter below.
               return Interval_Add (Half (First), Half (Last));
            else
               return Interval_Add (Interval_Multiply (One_Minus_T, First), Interval_Multiply (T, Last));
            end if;
         end Interpolate;
      begin
         Left := [others => Interval_Exact (0.0)];
         Right := [others => Interval_Exact (0.0)];
         Left (0) := Work (0);
         Right (Degree) := Work (Degree);
         if Degree > 0 then
            for Level in 1 .. Degree loop
               for Index in 0 .. Degree - Level loop
                  Work (Index) := Interpolate (Work (Index), Work (Index + 1));
               end loop;
               Left (Level) := Work (0);
               Right (Degree - Level) := Work (Degree - Level);
            end loop;
         end if;
      exception
         when Constraint_Error =>
            Left := [others => Invalid_Interval];
            Right := [others => Invalid_Interval];
      end Split_Bernstein;

      --  Form the outward-rounded midpoint used by dyadic subdivision. Halving before addition avoids overflow and
      --  equal inputs are preserved exactly so subdivision does not widen constant controls.
      function Valid_Interval_Midpoint (First, Last : Interval) return Interval;
      --  Construct a valid outward-rounded midpoint interval between two coefficients.

      function Valid_Interval_Midpoint (First, Last : Interval) return Interval is
      begin
         if First.Lower = Last.Lower and then First.Upper = Last.Upper then
            return First;
         end if;
         --  Halving first prevents overflow. The dyadic products are exact for normal model numbers, and the
         --  Model_Small component of Down/Up covers possible subnormal loss, so only one outward expansion is needed.
         return
           Checked_Interval (Down (0.5 * First.Lower + 0.5 * Last.Lower), Up (0.5 * First.Upper + 0.5 * Last.Upper));
      end Valid_Interval_Midpoint;

      --  Specialized De Casteljau split at one half for a single polynomial. It avoids repeated generic interval
      --  multiplication on the adaptive-certification path while retaining the generic fallback for invalid inputs.
      procedure Split_Bernstein_Midpoint (Source : Polynomial; Degree : Natural; Left, Right : out Polynomial);
      --  Split a Bernstein polynomial at the exact midpoint.

      procedure Split_Bernstein_Midpoint (Source : Polynomial; Degree : Natural; Left, Right : out Polynomial) is
      begin
         --  Invalid controls are not expected on the certified path. Retain the generic split's exact propagation
         --  semantics for them rather than making the optimized loop pay a validity branch at every triangle node.
         for Index in 0 .. Degree loop
            if not Source (Index).Valid then
               Split_Bernstein (Source, Degree, Interval_Exact (0.5), Left, Right);
               return;
            end if;
         end loop;

         Left := [others => Interval_Exact (0.0)];
         Right := [others => Interval_Exact (0.0)];
         for Index in 0 .. Degree loop
            Right (Index) := Source (Index);
         end loop;
         Left (0) := Right (0);
         if Degree > 0 then
            for Level in 1 .. Degree loop
               --  Right doubles as the de Casteljau workspace. Updating in ascending order leaves Index + 1 intact
               --  until it has contributed and naturally leaves every right-child boundary in its final slot.
               for Index in 0 .. Degree - Level loop
                  Right (Index) := Valid_Interval_Midpoint (Right (Index), Right (Index + 1));
               end loop;
               Left (Level) := Right (0);
            end loop;
         end if;
      exception
         when Constraint_Error =>
            Left := [others => Invalid_Interval];
            Right := [others => Invalid_Interval];
      end Split_Bernstein_Midpoint;

      --  Split a common denominator and all active axis numerators in one De Casteljau traversal. Sharing this work
      --  preserves corresponding subranges and avoids repeating the denominator calculation for every machine axis.
      procedure Split_Rational_Bernstein_Midpoint
        (Source_Numerators                   : Axis_Polynomials;
         Source_Denominator                  : Polynomial;
         Active_Axes                         : Axis_Flags;
         Degree                              : Natural;
         Left_Numerators, Right_Numerators   : out Axis_Polynomials;
         Left_Denominator, Right_Denominator : out Polynomial);
      --  Split the active numerator and denominator polynomials at the midpoint.

      procedure Split_Rational_Bernstein_Midpoint
        (Source_Numerators                   : Axis_Polynomials;
         Source_Denominator                  : Polynomial;
         Active_Axes                         : Axis_Flags;
         Degree                              : Natural;
         Left_Numerators, Right_Numerators   : out Axis_Polynomials;
         Left_Denominator, Right_Denominator : out Polynomial)
      is
         All_Valid : Boolean := True;
      begin
         for Index in 0 .. Degree loop
            if not Source_Denominator (Index).Valid then
               All_Valid := False;
               exit;
            end if;
         end loop;
         if All_Valid then
            for Axis in Axis_Name loop
               if Active_Axes (Axis) then
                  for Index in 0 .. Degree loop
                     if not Source_Numerators (Axis) (Index).Valid then
                        All_Valid := False;
                        exit;
                     end if;
                  end loop;
               end if;
               exit when not All_Valid;
            end loop;
         end if;

         Left_Numerators := [others => [others => Interval_Exact (0.0)]];
         Right_Numerators := [others => [others => Interval_Exact (0.0)]];
         if not All_Valid then
            Split_Bernstein_Midpoint (Source_Denominator, Degree, Left_Denominator, Right_Denominator);
            for Axis in Axis_Name loop
               if Active_Axes (Axis) then
                  Split_Bernstein_Midpoint
                    (Source_Numerators (Axis), Degree, Left_Numerators (Axis), Right_Numerators (Axis));
               end if;
            end loop;
            return;
         end if;

         Left_Denominator := [others => Interval_Exact (0.0)];
         Right_Denominator := [others => Interval_Exact (0.0)];
         for Index in 0 .. Degree loop
            Right_Denominator (Index) := Source_Denominator (Index);
         end loop;
         for Axis in Axis_Name loop
            if Active_Axes (Axis) then
               for Index in 0 .. Degree loop
                  Right_Numerators (Axis) (Index) := Source_Numerators (Axis) (Index);
               end loop;
               Left_Numerators (Axis) (0) := Right_Numerators (Axis) (0);
            end if;
         end loop;
         Left_Denominator (0) := Right_Denominator (0);

         if Degree > 0 then
            for Level in 1 .. Degree loop
               for Index in 0 .. Degree - Level loop
                  Right_Denominator (Index) :=
                    Valid_Interval_Midpoint (Right_Denominator (Index), Right_Denominator (Index + 1));
               end loop;
               Left_Denominator (Level) := Right_Denominator (0);
               for Axis in Axis_Name loop
                  if Active_Axes (Axis) then
                     for Index in 0 .. Degree - Level loop
                        Right_Numerators (Axis) (Index) :=
                          Valid_Interval_Midpoint
                            (Right_Numerators (Axis) (Index), Right_Numerators (Axis) (Index + 1));
                     end loop;
                     Left_Numerators (Axis) (Level) := Right_Numerators (Axis) (0);
                  end if;
               end loop;
            end loop;
         end if;
      exception
         when Constraint_Error =>
            Left_Denominator := [others => Invalid_Interval];
            Right_Denominator := [others => Invalid_Interval];
            Left_Numerators := [others => [others => Interval_Exact (0.0)]];
            Right_Numerators := [others => [others => Interval_Exact (0.0)]];
            for Axis in Axis_Name loop
               if Active_Axes (Axis) then
                  Left_Numerators (Axis) := [others => Invalid_Interval];
                  Right_Numerators (Axis) := [others => Invalid_Interval];
               end if;
            end loop;
      end Split_Rational_Bernstein_Midpoint;

      --  Evaluate Source at T by splitting it and taking the shared boundary control.
      function Evaluate_Bernstein (Source : Polynomial; Degree : Natural; T : Interval) return Interval;
      --  Evaluate a Bernstein polynomial over an interval parameter using de Casteljau subdivision.

      function Evaluate_Bernstein (Source : Polynomial; Degree : Natural; T : Interval) return Interval is
         Left, Right : Polynomial;
      begin
         Split_Bernstein (Source, Degree, T, Left, Right);
         return Left (Degree);
      end Evaluate_Bernstein;

      --  Reparameterize Source onto the requested closed subrange. A point-sized range becomes a degree-zero value;
      --  otherwise successive left and right splits return controls whose domain is again zero through one.
      function Restrict_Bernstein
        (Source : Polynomial; Degree : Natural; Range_Start, Range_End : Dimensionless) return Polynomial;
      --  Restrict a Bernstein polynomial to the requested parameter subrange.

      function Restrict_Bernstein
        (Source : Polynomial; Degree : Natural; Range_Start, Range_End : Dimensionless) return Polynomial
      is
         Work        : Polynomial := Source;
         Left, Right : Polynomial;
      begin
         if Range_Start = Range_End then
            Work := [others => Interval_Exact (0.0)];
            Work (0) := Evaluate_Bernstein (Source, Degree, Interval_Exact (Range_Start));
            return Work;
         end if;
         if Range_Start > 0.0 then
            Split_Bernstein (Work, Degree, Interval_Exact (Range_Start), Left, Right);
            Work := Right;
         end if;
         if Range_End < 1.0 then
            declare
               Local_End : constant Interval :=
                 Divide_Intervals
                   (Interval_Subtract (Interval_Exact (Range_End), Interval_Exact (Range_Start)),
                    Interval_Subtract (Interval_Exact (1.0), Interval_Exact (Range_Start)));
            begin
               Split_Bernstein (Work, Degree, Local_End, Left, Right);
               Work := Left;
            end;
         end if;
         return Work;
      exception
         when Constraint_Error =>
            return [others => Invalid_Interval];
      end Restrict_Bernstein;

      --  Return a lower bound on absolute value. This is used only to decide whether subdivision could materially
      --  tighten a valid upper bound; an interval spanning zero provides no positive evidence.
      function Interval_Abs_Min (Value : Interval) return Dimensionless;
      --  Return the minimum absolute magnitude represented by an interval.

      function Interval_Abs_Min (Value : Interval) return Dimensionless is
      begin
         if not Value.Valid or else (Value.Lower <= 0.0 and then Value.Upper >= 0.0) then
            return 0.0;
         elsif Value.Lower > 0.0 then
            return Value.Lower;
         else
            return -Value.Upper;
         end if;
      end Interval_Abs_Min;

      --  Bound all active rational functions N_axis / Q on the requested chart-coordinate range. Numerators and the
      --  positive denominator are elevated to a common degree, restricted together, and adaptively subdivided until
      --  the rational Bernstein hull is acceptably close to values already forced at the subrange endpoints.
      function Rational_Range_Bounds
        (Axis_Numerators        : Axis_Polynomials;
         Numerator_Degree       : Natural;
         Denominator            : Polynomial;
         Denominator_Degree     : Natural;
         Active_Axes            : Axis_Flags;
         Range_Start, Range_End : Dimensionless;
         Fallback_Denominator   : Dimensionless) return Axis_Bounds;
      --  Bound active rational Bernstein functions over a parameter subrange.

      function Rational_Range_Bounds
        (Axis_Numerators        : Axis_Polynomials;
         Numerator_Degree       : Natural;
         Denominator            : Polynomial;
         Denominator_Degree     : Natural;
         Active_Axes            : Axis_Flags;
         Range_Start, Range_End : Dimensionless;
         Fallback_Denominator   : Dimensionless) return Axis_Bounds
      is
         Degree                 : constant Natural := Natural'Max (Numerator_Degree, Denominator_Degree);
         Restricted_Numerators  : Axis_Polynomials := [others => [others => Interval_Exact (0.0)]];
         Restricted_Denominator : constant Polynomial :=
           Restrict_Bernstein
             (Elevate_Bernstein (Denominator, Denominator_Degree, Degree), Degree, Range_Start, Range_End);

         --  Compute the convex-hull bound for one rational Bernstein segment. When every denominator control is
         --  positive, N_i / Q_i are controls of the same rational function under positive normalized weights.
         procedure Hull_Bounds
           (Local_Numerators  : Axis_Polynomials;
            Local_Denominator : Polynomial;
            Local_Active      : Axis_Flags;
            Bounds            : out Axis_Bounds;
            Valid             : out Axis_Flags);

         --  Obtain conservative lower bounds on the magnitudes at the two segment endpoints. These are not returned as
         --  certificates; they only provide a scale for deciding whether the current upper hull deserves refinement.
         function Point_Lower_Bounds
           (Local_Numerators : Axis_Polynomials; Local_Denominator : Polynomial; Local_Active : Axis_Flags)
            return Axis_Bounds;

         --  Recursively split loose axes while accepting already tight axes at the current node.
         function Refined_Bounds
           (Local_Numerators  : Axis_Polynomials;
            Local_Denominator : Polynomial;
            Local_Active      : Axis_Flags;
            Depth             : Natural) return Axis_Bounds;

         procedure Hull_Bounds
           (Local_Numerators  : Axis_Polynomials;
            Local_Denominator : Polynomial;
            Local_Active      : Axis_Flags;
            Bounds            : out Axis_Bounds;
            Valid             : out Axis_Flags)
         is
            Denominator_Is_Valid : Boolean := True;
            Reciprocals          : Polynomial := [others => Interval_Exact (0.0)];
         begin
            Bounds := [others => 0.0];
            Valid := [others => False];
            for Index in 0 .. Degree loop
               if not Local_Denominator (Index).Valid or else Local_Denominator (Index).Lower <= 0.0 then
                  Denominator_Is_Valid := False;
                  exit;
               end if;
               Reciprocals (Index) := Divide_Intervals (Interval_Exact (1.0), Local_Denominator (Index));
               if not Reciprocals (Index).Valid then
                  Denominator_Is_Valid := False;
                  exit;
               end if;
            end loop;
            if not Denominator_Is_Valid then
               for Axis in Axis_Name loop
                  if Local_Active (Axis) then
                     Bounds (Axis) := Dimensionless'Last;
                  end if;
               end loop;
               return;
            end if;
            for Axis in Axis_Name loop
               if Local_Active (Axis) then
                  Valid (Axis) := True;
                  for Index in 0 .. Degree loop
                     declare
                        Quotient : constant Interval :=
                          Interval_Multiply (Local_Numerators (Axis) (Index), Reciprocals (Index));
                     begin
                        if not Quotient.Valid then
                           Bounds (Axis) := Dimensionless'Last;
                           Valid (Axis) := False;
                           exit;
                        end if;
                        Bounds (Axis) := Dimensionless'Max (Bounds (Axis), Interval_Abs_Max (Quotient));
                     end;
                  end loop;
                  if Valid (Axis) then
                     Bounds (Axis) := Up (Bounds (Axis));
                  end if;
               end if;
            end loop;
         end Hull_Bounds;

         function Point_Lower_Bounds
           (Local_Numerators : Axis_Polynomials; Local_Denominator : Polynomial; Local_Active : Axis_Flags)
            return Axis_Bounds
         is
            Result           : Axis_Bounds := [others => 0.0];
            First_Reciprocal : constant Interval := Divide_Intervals (Interval_Exact (1.0), Local_Denominator (0));
            Last_Reciprocal  : constant Interval :=
              Divide_Intervals (Interval_Exact (1.0), Local_Denominator (Degree));
         begin
            for Axis in Axis_Name loop
               if Local_Active (Axis) then
                  declare
                     First : constant Interval := Interval_Multiply (Local_Numerators (Axis) (0), First_Reciprocal);
                     Last  : constant Interval :=
                       Interval_Multiply (Local_Numerators (Axis) (Degree), Last_Reciprocal);
                  begin
                     Result (Axis) := Dimensionless'Max (Interval_Abs_Min (First), Interval_Abs_Min (Last));
                  end;
               end if;
            end loop;
            return Result;
         end Point_Lower_Bounds;

         --  Fall back to the numerator control hull divided by the independently certified global denominator lower
         --  bound. This is less tight than paired rational controls but remains valid if local denominator controls do
         --  not individually prove positivity.
         function Fallback_Bound (Local_Numerator : Polynomial) return Dimensionless;
         --  Produce a conservative numerator bound when subdivision cannot certify the ratio.

         function Fallback_Bound (Local_Numerator : Polynomial) return Dimensionless is
            Numerator_Bound : Dimensionless := 0.0;
         begin
            if Fallback_Denominator <= 0.0 then
               return Dimensionless'Last;
            end if;
            for Index in 0 .. Degree loop
               if not Local_Numerator (Index).Valid then
                  return Dimensionless'Last;
               end if;
               Numerator_Bound := Dimensionless'Max (Numerator_Bound, Interval_Abs_Max (Local_Numerator (Index)));
            end loop;
            declare
               Quotient : constant Dimensionless := Up (Numerator_Bound / Fallback_Denominator);
            begin
               return
                 (if Is_Finite (Quotient) and then Quotient < Dimensionless'Last
                  then Quotient
                  else Dimensionless'Last);
            end;
         end Fallback_Bound;

         function Refined_Bounds
           (Local_Numerators  : Axis_Polynomials;
            Local_Denominator : Polynomial;
            Local_Active      : Axis_Flags;
            Depth             : Natural) return Axis_Bounds
         is
            Current    : Axis_Bounds;
            Hull_Valid : Axis_Flags;
            Lower      : Axis_Bounds := [others => 0.0];
            Need_Split : Axis_Flags := [others => False];
            Result     : Axis_Bounds := [others => 0.0];
            Any_Split  : Boolean := False;
         begin
            Hull_Bounds (Local_Numerators, Local_Denominator, Local_Active, Current, Hull_Valid);
            if Depth < Maximum_Subdivision_Depth then
               Lower := Point_Lower_Bounds (Local_Numerators, Local_Denominator, Local_Active);
            end if;
            for Axis in Axis_Name loop
               if Local_Active (Axis) then
                  if Hull_Valid (Axis)
                    and then
                      (Depth = Maximum_Subdivision_Depth
                       or else Current (Axis) <= Tightness_Factor * Lower (Axis) + Tightness_Floor)
                  then
                     Result (Axis) := Current (Axis);
                  elsif Depth = Maximum_Subdivision_Depth then
                     Result (Axis) := Fallback_Bound (Local_Numerators (Axis));
                  else
                     Need_Split (Axis) := True;
                     Any_Split := True;
                  end if;
               end if;
            end loop;
            if Any_Split then
               declare
                  Left_Numerators, Right_Numerators   : Axis_Polynomials;
                  Left_Denominator, Right_Denominator : Polynomial;
                  Left_Bounds, Right_Bounds           : Axis_Bounds;
               begin
                  Split_Rational_Bernstein_Midpoint
                    (Local_Numerators,
                     Local_Denominator,
                     Need_Split,
                     Degree,
                     Left_Numerators,
                     Right_Numerators,
                     Left_Denominator,
                     Right_Denominator);
                  Left_Bounds := Refined_Bounds (Left_Numerators, Left_Denominator, Need_Split, Depth + 1);
                  Right_Bounds := Refined_Bounds (Right_Numerators, Right_Denominator, Need_Split, Depth + 1);
                  for Axis in Axis_Name loop
                     if Need_Split (Axis) then
                        Result (Axis) := Dimensionless'Max (Left_Bounds (Axis), Right_Bounds (Axis));
                     end if;
                  end loop;
               end;
            end if;
            return Result;
         end Refined_Bounds;
      begin
         for Axis in Axis_Name loop
            if Active_Axes (Axis) then
               Restricted_Numerators (Axis) :=
                 Restrict_Bernstein
                   (Elevate_Bernstein (Axis_Numerators (Axis), Numerator_Degree, Degree),
                    Degree,
                    Range_Start,
                    Range_End);
            end if;
         end loop;
         if Range_Start = Range_End then
            declare
               Result     : Axis_Bounds := [others => 0.0];
               Reciprocal : constant Interval := Divide_Intervals (Interval_Exact (1.0), Restricted_Denominator (0));
            begin
               for Axis in Axis_Name loop
                  if Active_Axes (Axis) then
                     Result (Axis) :=
                       Interval_Abs_Max (Interval_Multiply (Restricted_Numerators (Axis) (0), Reciprocal));
                  end if;
               end loop;
               return Result;
            end;
         else
            return Refined_Bounds (Restricted_Numerators, Restricted_Denominator, Active_Axes, 0);
         end if;
      exception
         when Constraint_Error =>
            return [others => Dimensionless'Last];
      end Rational_Range_Bounds;

      --  Bound an ordinary Bernstein polynomial by recursively taking absolute control hulls. The endpoint-correction
      --  polynomial is small, so a fixed depth gives a tight inexpensive bound without rational machinery.
      function Polynomial_Subdivision_Bound (Value : Polynomial; Degree, Depth : Natural) return Dimensionless;
      --  Bound a polynomial by recursively subdividing its Bernstein control polygon.

      function Polynomial_Subdivision_Bound (Value : Polynomial; Degree, Depth : Natural) return Dimensionless is
      begin
         if Depth = 0 or else Degree = 0 then
            declare
               Bound : Dimensionless := 0.0;
            begin
               for Index in 0 .. Degree loop
                  Bound := Dimensionless'Max (Bound, Interval_Abs_Max (Value (Index)));
               end loop;
               return Up (Bound);
            end;
         end if;
         declare
            Left, Right : Polynomial;
         begin
            Split_Bernstein_Midpoint (Value, Degree, Left, Right);
            return
              Dimensionless'Max
                (Polynomial_Subdivision_Bound (Left, Degree, Depth - 1),
                 Polynomial_Subdivision_Bound (Right, Degree, Depth - 1));
         end;
      end Polynomial_Subdivision_Bound;

      --  Bound the selected factorial-scaled derivative of the degree-eleven endpoint smootherstep on the requested
      --  physical-parameter range. The correction displacement divided by curve length is applied later per axis.
      function Correction_Coefficient_Bound (Order : Majorant_Order) return Dimensionless;
      --  Return the cached or computed bound for one correction order.

      function Correction_Coefficient_Bound (Order : Majorant_Order) return Dimensionless is
         Work   : Polynomial := [others => Interval_Exact (0.0)];
         Degree : Natural := 11;
      begin
         for Index in 6 .. 11 loop
            Work (Index) := Interval_Exact (1.0);
         end loop;
         for Derivative in 1 .. Natural (Order) + 1 loop
            declare
               Next_Degree : Natural;
               Next        : constant Polynomial := Differentiate_Bernstein (Work, Degree, Next_Degree);
            begin
               Work := Next;
               Degree := Next_Degree;
            end;
         end loop;
         if Order > 1 then
            Work :=
              Scale_Bernstein
                (Work, Degree, Checked_Interval (Down (1.0 / Factorial (Order)), Up (1.0 / Factorial (Order))));
         end if;
         Work := Restrict_Bernstein (Work, Degree, Start_U, End_U);
         if Start_U = End_U then
            return Interval_Abs_Max (Work (0));
         else
            return Polynomial_Subdivision_Bound (Work, Degree, 4);
         end if;
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Correction_Coefficient_Bound;

      --  Compute a nonnegative downward-rounded lower bound for Base**Exponent. Returning zero on underflow or unsafe
      --  arithmetic forces the caller to reject a denominator-based fallback instead of overstating its safety.
      function Lower_Power (Base : Dimensionless; Exponent : Positive) return Dimensionless;
      --  Compute a downward-rounded positive integer power.

      function Lower_Power (Base : Dimensionless; Exponent : Positive) return Dimensionless is
         Value : Dimensionless := 1.0;
      begin
         if Base <= 0.0 or else not Is_Finite (Base) then
            return 0.0;
         end if;
         for Count in 1 .. Exponent loop
            Value := Down (Value * Base);
            if Value <= 0.0 or else not Is_Finite (Value) then
               return 0.0;
            end if;
         end loop;
         return Value;
      exception
         when Constraint_Error =>
            return 0.0;
      end Lower_Power;

      --  Map normalized physical distance U to polynomial chart coordinate V with an outward-rounded enclosure of the
      --  endpoint-preserving Möbius warp. Exact endpoints remain exact to avoid widening full-range queries.
      function Warp_Interval (U : Dimensionless) return Interval;
      --  Enclose the distance-warp mapping at U.

      function Warp_Interval (U : Dimensionless) return Interval is
         U_Interval  : constant Interval := Interval_Exact (U);
         W_Interval  : constant Interval := Interval_Exact (Curve.Warp_Factor);
         Numerator   : Interval;
         Denominator : Interval;
      begin
         if U = 0.0 then
            return Interval_Exact (0.0);
         elsif U = 1.0 then
            return Interval_Exact (1.0);
         end if;
         Numerator := Interval_Multiply (W_Interval, U_Interval);
         Denominator := Interval_Add (Interval_Subtract (Interval_Exact (1.0), U_Interval), Numerator);
         return Divide_Intervals (Numerator, Denominator);
      exception
         when Constraint_Error =>
            return Invalid_Interval;
      end Warp_Interval;

      Start_V           : Dimensionless;
      End_V             : Dimensionless;
      G                 : Polynomial := [others => Interval_Exact (0.0)];
      G_Degree          : Natural range 0 .. 2;
      Correction_Bounds : Correction_Bound_Array := [others => 0.0];

      --  Advance from A_Order = N / Q**(Order + 1) to the next factorial-scaled physical-distance derivative. In chart
      --  coordinate V the recurrence is
      --
      --     N_next = (dV/dU) * (N' * Q / (Order + 1) - N * Q'),
      --
      --  while the denominator gains one additional factor of Q. G stores dV/dU as a degree-two polynomial in V.
      procedure Advance_Numerators (Order : Majorant_Order);
      --  Advance the rational derivative numerator recurrence to Order.

      procedure Advance_Numerators (Order : Majorant_Order) is
         Derivatives        : Axis_Polynomials := [others => [others => Interval_Exact (0.0)]];
         First_Products     : Axis_Polynomials := [others => [others => Interval_Exact (0.0)]];
         Second_Products    : Axis_Polynomials := [others => [others => Interval_Exact (0.0)]];
         Differences        : Axis_Polynomials := [others => [others => Interval_Exact (0.0)]];
         Next_Numerators    : Axis_Polynomials := [others => [others => Interval_Exact (0.0)]];
         Derivative_Degree  : Natural;
         Difference_Degree  : Natural;
         Next_Degree        : Natural;
         Divisor            : constant Dimensionless := Dimensionless (Natural (Order) + 1);
         Reciprocal_Divisor : constant Interval := Checked_Interval (Down (1.0 / Divisor), Up (1.0 / Divisor));
      begin
         if Q_Degree = 0 then
            --  A degree-zero rational tangent is constant in V. Every higher derivative of its rational part is
            --  therefore exactly zero; endpoint correction is still added independently below.
            Numerators := [others => [others => Interval_Exact (0.0)]];
            Zero_Numerator_Axes := [others => True];
            Numerator_Degree := 0;
            return;
         end if;

         Derivative_Degree := Numerator_Degree - 1;
         for Axis in Axis_Name loop
            if not Zero_Numerator_Axes (Axis) then
               declare
                  Computed_Degree : Natural;
                  Derivative      : constant Polynomial :=
                    Differentiate_Bernstein (Numerators (Axis), Numerator_Degree, Computed_Degree);
               begin
                  if Computed_Degree /= Derivative_Degree then
                     raise Constraint_Error;
                  end if;
                  Derivatives (Axis) := Derivative;
               end;
            end if;
         end loop;

         Difference_Degree := Derivative_Degree + Q_Degree;
         --  Accumulate N' * Q once in the same I/J order as Multiply_Bernstein, but reuse every binomial weight for
         --  all axes. Scaling remains after the complete product so interval operation ordering is unchanged.
         for I in 0 .. Derivative_Degree loop
            for J in 0 .. Q_Degree loop
               declare
                  Product_Index : constant Natural := I + J;
                  Weight        : constant Interval :=
                    Exact_Binomial_Product_Ratio (Derivative_Degree, I, Q_Degree, J, Difference_Degree, Product_Index);
               begin
                  for Axis in Axis_Name loop
                     if not Zero_Numerator_Axes (Axis) then
                        First_Products (Axis) (Product_Index) :=
                          Interval_Add
                            (First_Products (Axis) (Product_Index),
                             Interval_Multiply (Weight, Interval_Multiply (Derivatives (Axis) (I), Q (J))));
                     end if;
                  end loop;
               end;
            end loop;
         end loop;

         --  N * Q' has the same degree. Keep its independent accumulator so subtraction still happens only after
         --  both complete Bernstein products have been outward-rounded.
         for I in 0 .. Numerator_Degree loop
            for J in 0 .. Q_Derivative_Degree loop
               declare
                  Product_Index : constant Natural := I + J;
                  Weight        : constant Interval :=
                    Exact_Binomial_Product_Ratio
                      (Numerator_Degree, I, Q_Derivative_Degree, J, Difference_Degree, Product_Index);
               begin
                  for Axis in Axis_Name loop
                     if not Zero_Numerator_Axes (Axis) then
                        Second_Products (Axis) (Product_Index) :=
                          Interval_Add
                            (Second_Products (Axis) (Product_Index),
                             Interval_Multiply (Weight, Interval_Multiply (Numerators (Axis) (I), Q_Derivative (J))));
                     end if;
                  end loop;
               end;
            end loop;
         end loop;

         for Axis in Axis_Name loop
            if not Zero_Numerator_Axes (Axis) then
               for Index in 0 .. Difference_Degree loop
                  Differences (Axis) (Index) :=
                    Interval_Subtract
                      (Interval_Multiply (Reciprocal_Divisor, First_Products (Axis) (Index)),
                       Second_Products (Axis) (Index));
               end loop;
            end if;
         end loop;

         Next_Degree := Difference_Degree + G_Degree;
         if G_Degree = 0 then
            Next_Numerators := Differences;
         else
            --  G is common to every axis. Preserve Multiply_Bernstein's common-polynomial-first accumulation order
            --  while generating each product weight only once.
            for I in 0 .. G_Degree loop
               for J in 0 .. Difference_Degree loop
                  declare
                     Product_Index : constant Natural := I + J;
                     Weight        : constant Interval :=
                       Exact_Binomial_Product_Ratio (G_Degree, I, Difference_Degree, J, Next_Degree, Product_Index);
                  begin
                     for Axis in Axis_Name loop
                        if not Zero_Numerator_Axes (Axis) then
                           Next_Numerators (Axis) (Product_Index) :=
                             Interval_Add
                               (Next_Numerators (Axis) (Product_Index),
                                Interval_Multiply (Weight, Interval_Multiply (G (I), Differences (Axis) (J))));
                        end if;
                     end loop;
                  end;
               end loop;
            end loop;
         end if;

         for Axis in Axis_Name loop
            if Zero_Numerator_Axes (Axis) or else Is_Exact_Zero (Next_Numerators (Axis), Next_Degree) then
               Numerators (Axis) := [others => Interval_Exact (0.0)];
               Zero_Numerator_Axes (Axis) := True;
            else
               Numerators (Axis) := Next_Numerators (Axis);
            end if;
         end loop;
         Numerator_Degree := Next_Degree;
      end Advance_Numerators;
   begin
      Success := False;
      --  Handle representations whose exact structure already supplies the answer before consulting the retained
      --  certificate. A zero curve has zero majorants, while a pole-free evaluator without endpoint correction has a
      --  constant tangent and therefore only order-zero values.
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         Success := True;
         return Result;
      elsif Curve.Evaluator_Data.Antiderivative_Cache.Real_Pole_Count = 0
        and then Curve.Evaluator_Data.Antiderivative_Cache.Pair_Count = 0
        and then
          (for all Axis in Axis_Name =>
             Curve.Evaluator_Data.Finish_Point (Axis) = Curve.Evaluator_Data.Uncorrected_Finish_Point (Axis))
      then
         --  With no poles and no endpoint correction, the installed evaluator itself has a constant tangent. Besides
         --  making the exact straight-line result explicit, this avoids manufacturing roundoff-sized higher
         --  derivatives by differentiating interval controls whose numerator and denominator are proportional.
         for Axis in Axis_Name loop
            Result (Axis) (0) := abs Curve.Evaluator_Data.Antiderivative_Cache.Constant_Tangent (Axis);
         end loop;
         Success := True;
         return Result;
      elsif not Certificate.Valid
        or else Certificate.Minimum_Denominator <= 0.0
        or else not Is_Finite (Certificate.Minimum_Denominator)
        or else Length_Raw <= 0.0
        or else not Is_Finite (Length_Raw)
      then
         return Result;
      end if;

      declare
         Start_Image : constant Interval := Warp_Interval (Start_U);
         End_Image   : constant Interval := Warp_Interval (End_U);
      begin
         if not Start_Image.Valid or else not End_Image.Valid then
            return Result;
         end if;
         --  The positive-W Möbius warp is monotone. Using the lower enclosure of its start image and the upper
         --  enclosure of its end image retains every point in the requested U range despite endpoint roundoff.
         Start_V := Dimensionless'Max (0.0, Start_Image.Lower);
         End_V := Dimensionless'Min (1.0, End_Image.Upper);
         if Start_V > End_V then
            return Result;
         end if;
      end;

      --  Copy the active retained controls into degree-eighty-four workspaces. Numerators initially represent the
      --  tangent itself, and Denominator_Power initially contains Q; each derivative step updates both together.
      for Index in 0 .. Q_Degree loop
         Q (Index) := Certificate.Denominator (Index);
         Denominator_Power (Index) := Q (Index);
         for Axis in Axis_Name loop
            Numerators (Axis) (Index) := Certificate.Axis_Numerators (Axis) (Index);
         end loop;
      end loop;
      for Axis in Axis_Name loop
         Zero_Numerator_Axes (Axis) := Is_Exact_Zero (Numerators (Axis), Numerator_Degree);
      end loop;
      Q_Derivative := Differentiate_Bernstein (Q, Q_Degree, Q_Derivative_Degree);

      --  Express dV/dU in Bernstein form as a polynomial in V. The identity warp has the exact constant value one;
      --  otherwise the Möbius derivative has quadratic controls W, 1, and 1/W.
      if Curve.Warp_Factor = 1.0 then
         G_Degree := 0;
         G (0) := Interval_Exact (1.0);
      else
         G_Degree := 2;
         G (0) := Interval_Exact (Curve.Warp_Factor);
         G (1) := Interval_Exact (1.0);
         G (2) := Divide_Intervals (Interval_Exact (1.0), Interval_Exact (Curve.Warp_Factor));
      end if;

      for Order in Majorant_Order loop
         Correction_Bounds (Order) := Correction_Coefficient_Bound (Order);
         if Correction_Bounds (Order) >= Dimensionless'Last or else not Is_Finite (Correction_Bounds (Order)) then
            return Result;
         end if;
      end loop;

      --  Certify each derivative order before advancing the rational recurrence. Structural zero axes bypass interval
      --  work, and the endpoint smootherstep contribution is combined only after the rational part has a finite bound.
      for Order in Majorant_Order loop
         declare
            Fallback_Denominator : constant Dimensionless := Lower_Power (Certificate.Minimum_Denominator, Order + 1);
            Active_Rational_Axes : constant Axis_Flags :=
              [for Axis in Axis_Name =>
                 not Zero_Numerator_Axes (Axis)
                 and then
                   not (Curve.Structurally_Constant_Axes (Axis)
                        and then
                          Curve.Evaluator_Data.Finish_Point (Axis)
                          = Curve.Evaluator_Data.Uncorrected_Finish_Point (Axis))];
            Any_Active           : constant Boolean := (for some Axis in Axis_Name => Active_Rational_Axes (Axis));
            Rational_Bounds      : constant Axis_Bounds :=
              (if Any_Active
               then
                 Rational_Range_Bounds
                   (Numerators,
                    Numerator_Degree,
                    Denominator_Power,
                    Denominator_Power_Degree,
                    Active_Rational_Axes,
                    Start_V,
                    End_V,
                    Fallback_Denominator)
               else [others => 0.0]);
         begin
            for Axis in Axis_Name loop
               if Curve.Structurally_Constant_Axes (Axis)
                 and then
                   Curve.Evaluator_Data.Finish_Point (Axis) = Curve.Evaluator_Data.Uncorrected_Finish_Point (Axis)
               then
                  Result (Axis) (Order) := 0.0;
               else
                  declare
                     Rational_Bound   : constant Dimensionless := Rational_Bounds (Axis);
                     Correction       : constant Interval := Finish_Correction_Interval (Curve.Evaluator_Data, Axis);
                     Correction_Ratio : constant Interval :=
                       Divide_Intervals (Correction, Interval_Exact (Length_Raw));
                     Correction_Bound : constant Dimensionless :=
                       (if Correction_Ratio.Valid
                        then Up (Interval_Abs_Max (Correction_Ratio) * Correction_Bounds (Order))
                        else Dimensionless'Last);
                  begin
                     if Rational_Bound >= Dimensionless'Last
                       or else Correction_Bound >= Dimensionless'Last
                       or else not Is_Finite (Rational_Bound)
                       or else not Is_Finite (Correction_Bound)
                     then
                        return Result;
                     end if;
                     declare
                        Combined_Bound : constant Dimensionless := Up (Rational_Bound + Correction_Bound);
                     begin
                        if Combined_Bound >= Dimensionless'Last or else not Is_Finite (Combined_Bound) then
                           return Result;
                        end if;
                        Result (Axis) (Order) := Combined_Bound;
                     end;
                  end;
               end if;
            end loop;
         end;

         if Order < Majorant_Order'Last then
            Advance_Numerators (Order);
            Denominator_Power := Multiply_Bernstein (Denominator_Power, Denominator_Power_Degree, Q, Q_Degree);
            Denominator_Power_Degree := Denominator_Power_Degree + Q_Degree;
         end if;
      end loop;

      Success := True;
      return Result;
   exception
      when Constraint_Error =>
         Success := False;
         return [others => [others => 0.0]];
   end Bernstein_Tangent_Range_Majorants;

   function Realtime_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless) return Axis_Majorants;
   --  Compute the majorants used by the real-time tangent evaluator over a range.

   function Realtime_Tangent_Range_Majorants
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless) return Axis_Majorants
   is
      Success : Boolean;
   begin
      --  At a point, the partial-fraction evaluator gives substantially sharper cancellation than repeatedly
      --  differentiating interval Bernstein controls. Its point path uses one cell, not the old fixed 64-cell pass.
      if End_U - Start_U <= 8.0 * Dimensionless'Model_Epsilon * (1.0 + Dimensionless'Max (abs Start_U, abs End_U)) then
         return Pole_Taylor_Tangent_Range_Majorants (Curve, Start_U, End_U);
      end if;
      declare
         Result : constant Axis_Majorants := Bernstein_Tangent_Range_Majorants (Curve, Start_U, End_U, Success);
      begin
         return (if Success then Result else Pole_Taylor_Tangent_Range_Majorants (Curve, Start_U, End_U));
      end;
   end Realtime_Tangent_Range_Majorants;

   function Bounds_On_Parameter_Range
     (Curve : Stereographic_Curve; Start_U, End_U : Dimensionless) return Unit_Speed_Axial_Derivative_Bounds
   is
      Result : Unit_Speed_Axial_Derivative_Bounds := (others => <>);
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return Result;
      end if;

      declare
         --  Majorants store factorial-scaled parameter derivatives:
         --
         --     Mₖ ≥ |(1/k!)·dᵏT/dUᵏ|.
         --
         --  Physical arc distance is S = L·U, so |dᵏT/dSᵏ| ≤ k!·Mₖ/Lᵏ.
         Majorants  : constant Axis_Majorants :=
           (if Start_U = 0.0 and then End_U = 1.0 and then Curve.Has_Whole_Curve_Majorants
            then Curve.Whole_Curve_Majorants
            else Realtime_Tangent_Range_Majorants (Curve, Start_U, End_U));
         Length_Raw : constant Dimensionless := Dimensionless (Curve.Evaluator_Data.Length_Value / mm);

         function Physical_Bound (Majorant : Dimensionless; Order : Positive) return Dimensionless;
         --  Convert a normalized derivative majorant to physical-distance units.

         function Physical_Bound (Majorant : Dimensionless; Order : Positive) return Dimensionless is
            Value : Dimensionless := Majorant;
         begin
            if Length_Raw <= 0.0
              or else not Is_Finite (Length_Raw)
              or else Majorant >= Dimensionless'Last
              or else not Is_Finite (Majorant)
            then
               return Dimensionless'Last;
            elsif Majorant = 0.0 then
               return 0.0;
            end if;
            for Factor in 1 .. Order loop
               --  Form k!·M_k/L^k one factor at a time. Dividing first for L >= 1 avoids a needless intermediate
               --  overflow; multiplying first for L < 1 preserves the upward direction. In either case a saturated
               --  sentinel is returned immediately and can never be divided back into a finite underbound.
               if Length_Raw >= 1.0 then
                  Value := Up (Value / Length_Raw);
                  if Value >= Dimensionless'Last or else not Is_Finite (Value) then
                     return Dimensionless'Last;
                  end if;
                  Value := Up (Value * Dimensionless (Factor));
               else
                  Value := Up (Value * Dimensionless (Factor));
                  if Value >= Dimensionless'Last or else not Is_Finite (Value) then
                     return Dimensionless'Last;
                  end if;
                  Value := Up (Value / Length_Raw);
               end if;
               if Value >= Dimensionless'Last or else not Is_Finite (Value) then
                  return Dimensionless'Last;
               end if;
            end loop;
            return Value;
         exception
            when Constraint_Error =>
               return Dimensionless'Last;
         end Physical_Bound;
      begin
         for A in Axis_Name loop
            if Curve.Structurally_Constant_Axes (A) then
               --  Interval padding must not turn an algebraic zero into a visible planner bound.
               Result.Velocity (A) := 0.0;
               Result.Acceleration (A) := 0.0 / mm;
               Result.Jerk (A) := 0.0 / mm ** 2;
               Result.Snap (A) := 0.0 / mm ** 3;
               Result.Crackle (A) := 0.0 / mm ** 4;
            else
               Result.Velocity (A) := Majorants (A) (0);
               Result.Acceleration (A) := Physical_Bound (Majorants (A) (1), 1) / mm;
               Result.Jerk (A) := Physical_Bound (Majorants (A) (2), 2) / mm ** 2;
               Result.Snap (A) := Physical_Bound (Majorants (A) (3), 3) / mm ** 3;
               Result.Crackle (A) := Physical_Bound (Majorants (A) (4), 4) / mm ** 4;
            end if;
         end loop;
         return Result;
      end;
   exception
      when Constraint_Error =>
         return
           (Velocity     => [others => Dimensionless'Last],
            Acceleration => [others => Curvature'Last],
            Jerk         => [others => Curvature_To_2'Last],
            Snap         => [others => Curvature_To_3'Last],
            Crackle      => [others => Curvature_To_4'Last]);
   end Bounds_On_Parameter_Range;

   function Projected_Bound_On_Parameter_Range
     (Curve        : Stereographic_Curve;
      Start_U      : Dimensionless;
      End_U        : Dimensionless;
      Coefficients : Projection_Coefficients) return Curvature
   is
      Frame_Square_Sum      : Dimensionless := 0.0;
      Projection_Square_Sum : Dimensionless := 0.0;
      Correction_Numerator  : Interval := Interval_Exact (0.0);
      Component_Bound       : Dimensionless := 0.0;

      function Divide_Interval_By_Positive (Numerator, Denominator : Interval) return Interval;
      --  Enclose interval division while requiring a strictly positive denominator.

      function Divide_Interval_By_Positive (Numerator, Denominator : Interval) return Interval is
      begin
         if not Numerator.Valid or else not Denominator.Valid or else Denominator.Lower <= 0.0 then
            return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
         elsif Numerator.Lower = 0.0 and then Numerator.Upper = 0.0 then
            return Interval_Exact (0.0);
         end if;
         declare
            Q1 : constant Dimensionless := Numerator.Lower / Denominator.Lower;
            Q2 : constant Dimensionless := Numerator.Lower / Denominator.Upper;
            Q3 : constant Dimensionless := Numerator.Upper / Denominator.Lower;
            Q4 : constant Dimensionless := Numerator.Upper / Denominator.Upper;
         begin
            return
              Checked_Interval
                (Down (Dimensionless'Min (Dimensionless'Min (Q1, Q2), Dimensionless'Min (Q3, Q4))),
                 Up (Dimensionless'Max (Dimensionless'Max (Q1, Q2), Dimensionless'Max (Q3, Q4))));
         end;
      exception
         when Constraint_Error =>
            return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      end Divide_Interval_By_Positive;
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind or else Is_Zero_Projection (Coefficients) then
         return 0.0 / mm;
      end if;
      if (for all Axis in Axis_Name => Curve.Structurally_Constant_Axes (Axis) or else Coefficients (Axis) = 0.0 / mm)
      then
         return 0.0 / mm;
      end if;

      --  For the authoritative ideal unit tangent, |p·F·T| <= ||F'·p||. The retained cache differs from that tangent
      --  by at most Certified_Tangent_Error in Euclidean norm, and the endpoint correction adds
      --  (R/L)·H'(U). Bound all three terms independently while preserving cancellation in both dot products.
      for Component in Frame_Component_Index loop
         declare
            Local_Projection : Interval := Interval_Exact (0.0);
         begin
            for Axis in Axis_Name loop
               if not Curve.Structurally_Constant_Axes (Axis) then
                  Local_Projection :=
                    Interval_Add
                      (Local_Projection,
                       Interval_Multiply
                         (Interval_Exact (Dimensionless (Coefficients (Axis) / (1.0 / mm))),
                          Interval_Exact (Curve.Frame (Component) (Axis))));
               end if;
            end loop;
            declare
               Bound : constant Dimensionless := Interval_Abs_Max (Local_Projection);
            begin
               Frame_Square_Sum := Up (Frame_Square_Sum + Up (Bound * Bound));
            end;
         end;
      end loop;

      declare
         Range_Majorants : constant Axis_Majorants :=
           (if Start_U = 0.0 and then End_U = 1.0 and then Curve.Has_Whole_Curve_Majorants
            then Curve.Whole_Curve_Majorants
            else Realtime_Tangent_Range_Majorants (Curve, Start_U, End_U));
      begin
         for Axis in Axis_Name loop
            if not Curve.Structurally_Constant_Axes (Axis) then
               declare
                  Signed_Coefficient : constant Dimensionless := Dimensionless (Coefficients (Axis) / (1.0 / mm));
                  Coefficient        : constant Dimensionless := abs Signed_Coefficient;
               begin
                  if Coefficient /= 0.0 then
                     Projection_Square_Sum := Up (Projection_Square_Sum + Up (Coefficient * Coefficient));
                     Correction_Numerator :=
                       Interval_Add
                         (Correction_Numerator,
                          Interval_Multiply
                            (Interval_Exact (Signed_Coefficient),
                             Finish_Correction_Interval (Curve.Evaluator_Data, Axis)));
                     if Range_Majorants (Axis) (0) >= Dimensionless'Last then
                        Component_Bound := Dimensionless'Last;
                     elsif Component_Bound < Dimensionless'Last and then Range_Majorants (Axis) (0) /= 0.0 then
                        Component_Bound := Up (Component_Bound + Up (Coefficient * Range_Majorants (Axis) (0)));
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end;

      declare
         Correction_Projection : constant Interval :=
           Divide_Interval_By_Positive
             (Correction_Numerator, Interval_Exact (Dimensionless (Curve.Evaluator_Data.Length_Value / mm)));
         Ideal_Bound           : constant Dimensionless := Certified_Upper_Square_Root (Frame_Square_Sum);
         Projection_Norm       : constant Dimensionless := Certified_Upper_Square_Root (Projection_Square_Sum);
         Cache_Error           : constant Dimensionless := Up (Projection_Norm * Curve.Certified_Tangent_Error);
         Correction_Bound      : constant Dimensionless :=
           Up ((693.0 / 256.0) * Interval_Abs_Max (Correction_Projection));
         Global_Result         : constant Dimensionless := Up (Ideal_Bound + Up (Cache_Error + Correction_Bound));
         Result                : constant Dimensionless := Dimensionless'Min (Global_Result, Component_Bound);
      begin
         return (if Result >= Dimensionless'Last or else not Is_Finite (Result) then Curvature'Last else Result / mm);
      end;
   exception
      when Constraint_Error =>
         return Curvature'Last;
   end Projected_Bound_On_Parameter_Range;

   ---------------------------------------------------------------------------
   --  Rational realtime evaluator
   ---------------------------------------------------------------------------
   --
   --  In the polynomial chart coordinate the inverse-stereographic tangent is a degree-sixteen rational function.
   --  Construction factors its positive denominator and stores a real partial-fraction expansion. The poles and
   --  residues are then transformed through the same Möbius map as the ideal chart, so realtime evaluation works
   --  directly in normalized physical distance.

   function Complex_Pair_Real_Slot
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Rational_Degree_Slot is
   begin
      return Rational_Degree_Slot (Cache.Real_Pole_Count + 2 * Natural (Pair) - 1);
   end Complex_Pair_Real_Slot;

   function Complex_Pair_Imaginary_Slot
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Rational_Degree_Slot is
   begin
      return Complex_Pair_Real_Slot (Cache, Pair) + 1;
   end Complex_Pair_Imaginary_Slot;

   function Rational_Antiderivative_Is_Well_Formed (Cache : Rational_Antiderivative) return Boolean is
      Active_Slot_Count : constant Natural := Cache.Real_Pole_Count + 2 * Cache.Pair_Count;

      function Finite (Value : Dimensionless_Axis_Vector) return Boolean
      is (for all Axis in Axis_Name => Is_Finite (Value (Axis)));

      function Is_Zero (Slot : Rational_Pole_Slot) return Boolean
      is (Slot.Pole_Component = 0.0 and then (for all Axis in Axis_Name => Slot.Residue_Component (Axis) = 0.0));
   begin
      if Active_Slot_Count > Maximum_Rational_Degree or else not Finite (Cache.Constant_Tangent) then
         return False;
      end if;

      --  Slots first contain real poles, then adjacent real/imaginary records for each conjugate pair. Real poles
      --  must lie outside [0, 1], pair imaginary components must be positive, and all inactive slots must be zero.
      for Slot in Rational_Degree_Slot loop
         if Slot <= Cache.Real_Pole_Count then
            if not Is_Finite (Cache.Pole_Slots (Slot).Pole_Component)
              or else Cache.Pole_Slots (Slot).Pole_Component in 0.0 .. 1.0
              or else not Finite (Cache.Pole_Slots (Slot).Residue_Component)
            then
               return False;
            end if;
         elsif Slot <= Active_Slot_Count then
            if not Is_Finite (Cache.Pole_Slots (Slot).Pole_Component)
              or else not Finite (Cache.Pole_Slots (Slot).Residue_Component)
              or else ((Slot - Cache.Real_Pole_Count) mod 2 = 0 and then Cache.Pole_Slots (Slot).Pole_Component <= 0.0)
            then
               return False;
            end if;
         elsif not Is_Zero (Cache.Pole_Slots (Slot)) then
            return False;
         end if;
      end loop;

      return True;
   exception
      when Constraint_Error =>
         return False;
   end Rational_Antiderivative_Is_Well_Formed;

   function Rational_Antiderivative_Primitives_Are_Safe (Cache : Rational_Antiderivative) return Boolean is
      Work : Dimensionless_Axis_Vector := [for Axis in Axis_Name => abs Cache.Constant_Tangent (Axis)];

      function Accumulate_Work
        (Total : in out Dimensionless; Magnitude, Primitive_Magnitude : Dimensionless) return Boolean;
      --  Accumulate a rounding-work estimate and report whether it remains finite.

      function Accumulate_Work
        (Total : in out Dimensionless; Magnitude, Primitive_Magnitude : Dimensionless) return Boolean
      is
         Scaled_Primitive : Dimensionless;
         Product          : Dimensionless;
      begin
         if not Is_Finite (Total)
           or else not Is_Finite (Magnitude)
           or else not Is_Finite (Primitive_Magnitude)
           or else Magnitude < 0.0
           or else Primitive_Magnitude < 0.0
         then
            return False;
         elsif Magnitude = 0.0 or else Primitive_Magnitude = 0.0 then
            return True;
         end if;

         --  Reserve a factor of 64 before every product. There are at most seventeen accumulated terms per axis
         --  (the constant and sixteen pole slots), so this fixed headroom proves that neither an individual product
         --  nor any evaluation prefix can approach overflow even if ordinary round-to-nearest arithmetic moves the
         --  estimated primitive work downward by a few ulps.
         Scaled_Primitive := Dimensionless'Max (1.0, Primitive_Magnitude);
         if Magnitude > (Dimensionless'Last / 64.0) / Scaled_Primitive then
            return False;
         end if;
         Product := Up (Magnitude * Primitive_Magnitude);
         Total := Up (Total + Product);
         return Is_Finite (Total) and then Total <= Dimensionless'Last / 2.0;
      exception
         when Constraint_Error =>
            return False;
      end Accumulate_Work;
   begin
      if not Rational_Antiderivative_Is_Well_Formed (Cache)
        or else (for some Axis in Axis_Name => Work (Axis) > Dimensionless'Last / 64.0)
      then
         return False;
      end if;

      --  Check primitive and accumulation requirements once during construction. A real-pole logarithm and
      --  conjugate-pair argument change are monotone from zero; the closest point and finish are the extrema of a
      --  pair's log-distance ratio. The fixed headroom in Accumulate_Work keeps every evaluation prefix finite despite
      --  arbitrary residue signs.
      for Index in 1 .. Cache.Real_Pole_Count loop
         declare
            Slot      : Rational_Pole_Slot renames Cache.Pole_Slots (Index);
            Primitive : constant Dimensionless := abs Stable_Real_Log_Ratio (Slot.Pole_Component, 1.0);
         begin
            if not Is_Finite (Primitive) then
               return False;
            end if;
            for Axis in Axis_Name loop
               if not Accumulate_Work (Work (Axis), abs Slot.Residue_Component (Axis), Primitive) then
                  return False;
               end if;
            end loop;
         end;
      end loop;

      for Pair in 1 .. Cache.Pair_Count loop
         declare
            Real_Slot      : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)));
            Imaginary_Slot : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)));
            Closest        : constant Dimensionless :=
              Dimensionless'Max (0.0, Dimensionless'Min (1.0, Real_Slot.Pole_Component));
            Closest_Log    : constant Dimensionless :=
              Stable_Complex_Log_Ratio (Real_Slot.Pole_Component, Imaginary_Slot.Pole_Component, Closest);
            Finish_Log     : constant Dimensionless :=
              Stable_Complex_Log_Ratio (Real_Slot.Pole_Component, Imaginary_Slot.Pole_Component, 1.0);
            Finish_Angle   : constant Dimensionless :=
              Stable_Complex_Angle_Delta (Real_Slot.Pole_Component, Imaginary_Slot.Pole_Component, 1.0);
            Log_Work       : constant Dimensionless := Dimensionless'Max (abs Closest_Log, abs Finish_Log);
            Angle_Work     : constant Dimensionless := 2.0 * abs Finish_Angle;
         begin
            if not Is_Finite (Closest_Log)
              or else not Is_Finite (Finish_Log)
              or else not Is_Finite (Finish_Angle)
              or else not Is_Finite (Angle_Work)
            then
               return False;
            end if;
            for Axis in Axis_Name loop
               if not Accumulate_Work (Work (Axis), abs Real_Slot.Residue_Component (Axis), Log_Work)
                 or else not Accumulate_Work (Work (Axis), abs Imaginary_Slot.Residue_Component (Axis), Angle_Work)
               then
                  return False;
               end if;
            end loop;
         end;
      end loop;
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Rational_Antiderivative_Primitives_Are_Safe;

   function Rational_Antiderivative_Is_Canonical_Zero (Cache : Rational_Antiderivative) return Boolean is
   begin
      return
        Cache.Real_Pole_Count = 0
        and then Cache.Pair_Count = 0
        and then (for all Axis in Axis_Name => Cache.Constant_Tangent (Axis) = 0.0)
        and then
          (for all Slot in Rational_Degree_Slot =>
             Cache.Pole_Slots (Slot).Pole_Component = 0.0
             and then (for all Axis in Axis_Name => Cache.Pole_Slots (Slot).Residue_Component (Axis) = 0.0));
   exception
      when Constraint_Error =>
         return False;
   end Rational_Antiderivative_Is_Canonical_Zero;

   function Stable_Log_One_Plus (Value : Dimensionless) return Dimensionless is
      Reduced, Reduced_Square, Term, Sum : Dimensionless;
   begin
      if abs Value > 0.125 then
         return Dimensionless_Math.Log (1.0 + Value);
      end if;

      --  log(1 + x) = 2·atanh(x/(2 + x)). The short odd series
      --
      --     log(1 + x) = 2·Σₖ₌₀⁹ z²ᵏ⁺¹/(2k + 1),   z = x/(2 + x),
      --
      --  avoids losing the small displacement when a pole is far from the curve.
      Reduced := Value / (2.0 + Value);
      Reduced_Square := Reduced * Reduced;
      Term := Reduced;
      Sum := Reduced;
      for Order in 1 .. 9 loop
         Term := Term * Reduced_Square;
         Sum := Sum + Term / Dimensionless (2 * Order + 1);
      end loop;
      return 2.0 * Sum;
   end Stable_Log_One_Plus;

   function Stable_Real_Log_Ratio (Pole, Normalized_Distance : Dimensionless) return Dimensionless is
   begin
      if Normalized_Distance = 0.0 then
         return 0.0;
      elsif Normalized_Distance <= 0.125 * abs Pole then
         return Stable_Log_One_Plus (-Normalized_Distance / Pole);
      end if;

      --  Near a pole just outside an endpoint, -U/Pole can round to -1 even though the exact log argument is
      --  positive. The difference-of-logs form preserves that positive distance. The small-relative-change branch
      --  above avoids cancellation between these logarithms for distant poles.
      return Dimensionless_Math.Log (abs (Pole - Normalized_Distance)) - Dimensionless_Math.Log (abs Pole);
   end Stable_Real_Log_Ratio;

   function Stable_Complex_Log_Ratio
     (Pole_Real, Pole_Imaginary, Normalized_Distance : Dimensionless) return Dimensionless
   is
      Base_Absolute_Real  : constant Dimensionless := abs Pole_Real;
      Point_Absolute_Real : constant Dimensionless := abs (Normalized_Distance - Pole_Real);
      Base_Scale          : constant Dimensionless := Dimensionless'Max (Base_Absolute_Real, Pole_Imaginary);
      Point_Scale         : constant Dimensionless := Dimensionless'Max (Point_Absolute_Real, Pole_Imaginary);
      Base_Minor_Ratio    : constant Dimensionless :=
        Dimensionless'Min (Base_Absolute_Real, Pole_Imaginary) / Base_Scale;
      Point_Minor_Ratio   : constant Dimensionless :=
        Dimensionless'Min (Point_Absolute_Real, Pole_Imaginary) / Point_Scale;
      Base_Minor_Square   : constant Dimensionless := Base_Minor_Ratio * Base_Minor_Ratio;
      Point_Minor_Square  : constant Dimensionless := Point_Minor_Ratio * Point_Minor_Ratio;
      Base_Shape          : constant Dimensionless := 1.0 + Base_Minor_Square;
      Point_Shape         : constant Dimensionless := 1.0 + Point_Minor_Square;
   begin
      --  Preserve small changes for distant poles. Scaling by max(|A|, B) avoids overflow in both square sums.
      if Normalized_Distance <= Base_Scale then
         declare
            Scaled_U        : constant Dimensionless := Normalized_Distance / Base_Scale;
            Scaled_A        : constant Dimensionless := Pole_Real / Base_Scale;
            Relative_Change : constant Dimensionless := Scaled_U * (Scaled_U - 2.0 * Scaled_A) / Base_Shape;
         begin
            if abs Relative_Change <= 0.125 then
               return Stable_Log_One_Plus (Relative_Change);
            end if;
         end;
      end if;

      --  Away from one, form the strictly positive ratio from independently scaled distances. In particular, at
      --  U = A this retains B^2 instead of subtracting A^2 from A^2 + B^2. Use the reciprocal branch to prevent
      --  overflow; if the squared scale ratio underflows, logarithm differences remain well defined.
      if Point_Scale <= Base_Scale then
         declare
            Scale_Ratio : constant Dimensionless := Point_Scale / Base_Scale;
            Ratio       : constant Dimensionless := (Scale_Ratio * Scale_Ratio) * (Point_Shape / Base_Shape);
         begin
            if Ratio > 0.0 then
               return Dimensionless_Math.Log (Ratio);
            end if;
         end;
      else
         declare
            Scale_Ratio   : constant Dimensionless := Base_Scale / Point_Scale;
            Inverse_Ratio : constant Dimensionless := (Scale_Ratio * Scale_Ratio) * (Base_Shape / Point_Shape);
         begin
            if Inverse_Ratio > 0.0 then
               return -Dimensionless_Math.Log (Inverse_Ratio);
            end if;
         end;
      end if;

      return
        2.0
        * (Dimensionless_Math.Log (Point_Scale) - Dimensionless_Math.Log (Base_Scale))
        + Stable_Log_One_Plus (Point_Minor_Square)
        - Stable_Log_One_Plus (Base_Minor_Square);
   end Stable_Complex_Log_Ratio;

   function Stable_Complex_Angle_Delta
     (Pole_Real, Pole_Imaginary, Normalized_Distance : Dimensionless) return Dimensionless
   is
      Scale    : constant Dimensionless :=
        Dimensionless'Max (abs Normalized_Distance, Dimensionless'Max (abs Pole_Real, Pole_Imaginary));
      Scaled_U : constant Dimensionless := Normalized_Distance / Scale;
      Scaled_A : constant Dimensionless := Pole_Real / Scale;
      Scaled_B : constant Dimensionless := Pole_Imaginary / Scale;
   begin
      --  These are the cross and dot products between (-A,-B) and (U-A,-B), both divided by Scale**2.
      --  Scaling before multiplication avoids overflow for large finite poles without changing the returned angle.
      return Dimensionless_Math.Arctan (Scaled_B * Scaled_U, Scaled_B * Scaled_B + Scaled_A * (Scaled_A - Scaled_U));
   end Stable_Complex_Angle_Delta;

   function Evaluate_Rational_Displacement
     (Cache : Rational_Antiderivative; Normalized_Distance : Dimensionless) return Dimensionless_Axis_Vector
   is
      Result : Dimensionless_Axis_Vector :=
        [for Axis in Axis_Name => Normalized_Distance * Cache.Constant_Tangent (Axis)];
   begin
      --  Integrating c + Σr/(U - p) from 0 to U gives
      --
      --     c·U + Σr·log(1 - U/p).
      for Index in 1 .. Cache.Real_Pole_Count loop
         declare
            Slot      : Rational_Pole_Slot renames Cache.Pole_Slots (Index);
            Primitive : constant Dimensionless := Stable_Real_Log_Ratio (Slot.Pole_Component, Normalized_Distance);
         begin
            for Axis in Axis_Name loop
               Result (Axis) := Result (Axis) + Primitive * Slot.Residue_Component (Axis);
            end loop;
         end;
      end loop;

      for Pair in 1 .. Cache.Pair_Count loop
         declare
            Real_Slot      : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)));
            Imaginary_Slot : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)));
            A              : constant Dimensionless := Real_Slot.Pole_Component;
            B              : constant Dimensionless := Imaginary_Slot.Pole_Component;
            Log_Ratio      : constant Dimensionless := Stable_Complex_Log_Ratio (A, B, Normalized_Distance);
            Angle_Delta    : constant Dimensionless := Stable_Complex_Angle_Delta (A, B, Normalized_Distance);
         begin
            --  Combine conjugate residues and poles into real arithmetic. For p = A + iB and r = R + iI, the pair
            --  primitive is
            --
            --     R·log(((U - A)² + B²)/(A² + B²)) - 2·I·Δarg(U - p).
            for Axis in Axis_Name loop
               Result (Axis) :=
                 Result (Axis) + Real_Slot.Residue_Component (Axis) * Log_Ratio
                 - Imaginary_Slot.Residue_Component (Axis) * (2.0 * Angle_Delta);
            end loop;
         end;
      end loop;
      return Result;
   end Evaluate_Rational_Displacement;

   function Endpoint_Smootherstep (Normalized_Distance : Dimensionless) return Dimensionless;
   --  Evaluate the endpoint-flattening smootherstep correction.

   function Endpoint_Smootherstep (Normalized_Distance : Dimensionless) return Dimensionless is
      U          : constant Dimensionless :=
        (if Normalized_Distance <= 0.5 then Normalized_Distance else 1.0 - Normalized_Distance);
      Lower_Half : Dimensionless;
   begin
      --  This is the degree-eleven generalized smoothstep
      --
      --     462 U^6 - 1980 U^7 + 3465 U^8 - 3080 U^9 + 1386 U^10 - 252 U^11.
      --
      --  Its first five derivatives vanish at both endpoints. Evaluate only on [0, 1/2] and use symmetry for the
      --  upper half, avoiding the severe cancellation of the power form near one.
      if Normalized_Distance <= 0.0 then
         return 0.0;
      elsif Normalized_Distance >= 1.0 then
         return 1.0;
      end if;
      Lower_Half := U ** 6 * (462.0 + U * (-1_980.0 + U * (3_465.0 + U * (-3_080.0 + U * (1_386.0 - 252.0 * U)))));
      return (if Normalized_Distance <= 0.5 then Lower_Half else 1.0 - Lower_Half);
   end Endpoint_Smootherstep;

   function Evaluate_Uncorrected_Rational_Point
     (Evaluator : Stereographic_Curve_Evaluator; Normalized_Distance : Dimensionless) return Position;
   --  Evaluate the rational antiderivative before applying endpoint correction.

   function Evaluate_Uncorrected_Rational_Point
     (Evaluator : Stereographic_Curve_Evaluator; Normalized_Distance : Dimensionless) return Position
   is
      Displacement : constant Dimensionless_Axis_Vector :=
        Evaluate_Rational_Displacement (Evaluator.Antiderivative_Cache, Normalized_Distance);
   begin
      return [for Axis in Axis_Name => Evaluator.Start_Point (Axis) + Evaluator.Length_Value * Displacement (Axis)];
   end Evaluate_Uncorrected_Rational_Point;

   function Evaluate_Rational_Point
     (Evaluator : Stereographic_Curve_Evaluator; Normalized_Distance : Dimensionless) return Position
   is
      Uncorrected : constant Position := Evaluate_Uncorrected_Rational_Point (Evaluator, Normalized_Distance);
      Blend       : constant Dimensionless := Endpoint_Smootherstep (Normalized_Distance);
      Complement  : constant Dimensionless := 1.0 - Blend;
   begin
      --  In exact arithmetic this is
      --
      --     Uncorrected(U) + H(U)·(Finish_Point - Uncorrected(1)).
      --
      --  The barycentric form below retains the same continuous endpoint-flat correction while anchoring both ends
      --  in floating-point arithmetic. At U = 0 only Uncorrected(0) remains; at U = 1 both differences from the
      --  stored bit-exact Uncorrected(1) vanish and only Finish_Point remains. This avoids asking one correction
      --  value to bridge a cancellation gap which may contain no representable floating-point number.
      return
        [for Axis in Axis_Name =>
           (if Evaluator.Finish_Point (Axis) = Evaluator.Uncorrected_Finish_Point (Axis)
            then Uncorrected (Axis)
            else
              Complement * Uncorrected (Axis) + Blend * Evaluator.Finish_Point (Axis)
              + Blend * (Uncorrected (Axis) - Evaluator.Uncorrected_Finish_Point (Axis)))];
   end Evaluate_Rational_Point;

   function Cached_Evaluator_Tangent_Derivative_At
     (Cache : Rational_Antiderivative;
      U     : Dimensionless;
      Order : Endpoint_Tangent_Derivative_Order;
      Value : out Interval_Position_Scale) return Boolean;
   --  Outward-enclose a cached tangent derivative at one normalized distance.

   function Cached_Evaluator_Tangent_Derivative_At
     (Cache : Rational_Antiderivative;
      U     : Dimensionless;
      Order : Endpoint_Tangent_Derivative_Order;
      Value : out Interval_Position_Scale) return Boolean
   is
      type Interval_Complex is record
         Real_Part      : Interval;
         Imaginary_Part : Interval;
      end record;

      function Unbounded_Interval return Interval
      is ((Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False));

      function Divide_By_Nonzero (Numerator, Denominator : Interval) return Interval;
      --  Enclose division by an interval that excludes zero.

      function Divide_By_Nonzero (Numerator, Denominator : Interval) return Interval is
      begin
         if not Numerator.Valid
           or else not Denominator.Valid
           or else (Denominator.Lower <= 0.0 and then Denominator.Upper >= 0.0)
         then
            return Unbounded_Interval;
         end if;
         declare
            Q1 : constant Dimensionless := Numerator.Lower / Denominator.Lower;
            Q2 : constant Dimensionless := Numerator.Lower / Denominator.Upper;
            Q3 : constant Dimensionless := Numerator.Upper / Denominator.Lower;
            Q4 : constant Dimensionless := Numerator.Upper / Denominator.Upper;
         begin
            return
              Checked_Interval
                (Down (Dimensionless'Min (Dimensionless'Min (Q1, Q2), Dimensionless'Min (Q3, Q4))),
                 Up (Dimensionless'Max (Dimensionless'Max (Q1, Q2), Dimensionless'Max (Q3, Q4))));
         end;
      exception
         when Constraint_Error =>
            return Unbounded_Interval;
      end Divide_By_Nonzero;

      function Divide_By_Positive (Numerator, Denominator : Interval) return Interval;
      --  Enclose division by an interval that is strictly positive.

      function Divide_By_Positive (Numerator, Denominator : Interval) return Interval is
      begin
         if not Denominator.Valid or else Denominator.Lower <= 0.0 then
            return Unbounded_Interval;
         else
            return Divide_By_Nonzero (Numerator, Denominator);
         end if;
      end Divide_By_Positive;

      function Multiply (Left, Right : Interval_Complex) return Interval_Complex
      is (Real_Part      =>
            Interval_Subtract
              (Interval_Multiply (Left.Real_Part, Right.Real_Part),
               Interval_Multiply (Left.Imaginary_Part, Right.Imaginary_Part)),
          Imaginary_Part =>
            Interval_Add
              (Interval_Multiply (Left.Real_Part, Right.Imaginary_Part),
               Interval_Multiply (Left.Imaginary_Part, Right.Real_Part)));

      function Complex_Reciprocal (Point, Pole_Real, Pole_Imaginary : Dimensionless) return Interval_Complex;
      --  Enclose the reciprocal distance from Point to a complex pole.

      function Complex_Reciprocal (Point, Pole_Real, Pole_Imaginary : Dimensionless) return Interval_Complex is
         Offset           : constant Interval :=
           Interval_Subtract (Interval_Exact (Point), Interval_Exact (Pole_Real));
         Imaginary        : constant Interval := Interval_Exact (Pole_Imaginary);
         Offset_Square    : Interval := Interval_Multiply (Offset, Offset);
         Imaginary_Square : Interval := Interval_Multiply (Imaginary, Imaginary);
         Denominator      : Interval;
      begin
         Offset_Square.Lower := Dimensionless'Max (0.0, Offset_Square.Lower);
         Imaginary_Square.Lower := Dimensionless'Max (0.0, Imaginary_Square.Lower);
         Denominator := Interval_Add (Offset_Square, Imaginary_Square);
         Denominator.Lower := Dimensionless'Max (0.0, Denominator.Lower);
         return
           (Real_Part      => Divide_By_Positive (Offset, Denominator),
            Imaginary_Part => Divide_By_Positive (Imaginary, Denominator));
      exception
         when Constraint_Error =>
            return (Real_Part => Unbounded_Interval, Imaginary_Part => Unbounded_Interval);
      end Complex_Reciprocal;

      Signed_Factor : constant Dimensionless := (if Order mod 2 = 0 then Factorial (Order) else -Factorial (Order));
   begin
      Value :=
        (if Order = 0
         then [for Axis in Axis_Name => Interval_Exact (Cache.Constant_Tangent (Axis))]
         else [others => Interval_Exact (0.0)]);
      for Index in 1 .. Cache.Real_Pole_Count loop
         declare
            Slot       : Rational_Pole_Slot renames Cache.Pole_Slots (Index);
            Reciprocal : constant Interval :=
              Divide_By_Nonzero
                (Interval_Exact (1.0), Interval_Subtract (Interval_Exact (U), Interval_Exact (Slot.Pole_Component)));
            Power      : Interval := Interval_Exact (1.0);
         begin
            for Exponent in 1 .. Order + 1 loop
               Power := Interval_Multiply (Power, Reciprocal);
            end loop;
            for Axis in Axis_Name loop
               Value (Axis) :=
                 Interval_Add
                   (Value (Axis),
                    Interval_Multiply
                      (Interval_Exact (Signed_Factor),
                       Interval_Multiply (Interval_Exact (Slot.Residue_Component (Axis)), Power)));
            end loop;
         end;
      end loop;

      for Pair in 1 .. Cache.Pair_Count loop
         declare
            Real_Slot        : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair)));
            Imaginary_Slot   : Rational_Pole_Slot renames
              Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair)));
            Reciprocal       : constant Interval_Complex :=
              Complex_Reciprocal (U, Real_Slot.Pole_Component, Imaginary_Slot.Pole_Component);
            Reciprocal_Power : Interval_Complex :=
              (Real_Part => Interval_Exact (1.0), Imaginary_Part => Interval_Exact (0.0));
         begin
            for Exponent in 1 .. Order + 1 loop
               Reciprocal_Power := Multiply (Reciprocal_Power, Reciprocal);
            end loop;
            for Axis in Axis_Name loop
               Value (Axis) :=
                 Interval_Add
                   (Value (Axis),
                    Interval_Multiply
                      (Interval_Exact (2.0 * Signed_Factor),
                       Interval_Subtract
                         (Interval_Multiply
                            (Interval_Exact (Real_Slot.Residue_Component (Axis)), Reciprocal_Power.Real_Part),
                          Interval_Multiply
                            (Interval_Exact (Imaginary_Slot.Residue_Component (Axis)),
                             Reciprocal_Power.Imaginary_Part))));
            end loop;
         end;
      end loop;
      return (for all Axis in Axis_Name => Value (Axis).Valid);
   exception
      when Constraint_Error =>
         Value := [others => Unbounded_Interval];
         return False;
   end Cached_Evaluator_Tangent_Derivative_At;

   function Measure_Evaluator_Endpoint_Jet_Error
     (Evaluator            : Stereographic_Curve_Evaluator;
      Requested_Start_Jet  : Endpoint_Tangent_Jet;
      Requested_Finish_Jet : Endpoint_Tangent_Jet;
      Error_Bounds         : out Endpoint_Jet_Error_Bounds) return Boolean
   is
      function Expected_Derivative
        (Jet : Endpoint_Tangent_Jet; Axis : Axis_Name; Order : Endpoint_Tangent_Derivative_Order) return Interval;

      function Measure_Endpoint
        (Jet : Endpoint_Tangent_Jet; U : Dimensionless; Bounds : out Endpoint_Jet_Error_Bounds) return Boolean;

      function Expected_Derivative
        (Jet : Endpoint_Tangent_Jet; Axis : Axis_Name; Order : Endpoint_Tangent_Derivative_Order) return Interval
      is
         Length_Raw : constant Interval := Interval_Exact (Dimensionless (Evaluator.Length_Value / mm));
         Result     : Interval;
      begin
         case Order is
            when 0 =>
               Result := Interval_Exact (Jet.Tangent (Axis));

            when 1 =>
               Result := Interval_Exact (Dimensionless (Jet.Tangent_Derivative_1 (Axis) * mm));

            when 2 =>
               Result := Interval_Exact (Dimensionless (Jet.Tangent_Derivative_2 (Axis) * mm ** 2));

            when 3 =>
               Result := Interval_Exact (Dimensionless (Jet.Tangent_Derivative_3 (Axis) * mm ** 3));
         end case;
         for Exponent in 1 .. Order loop
            Result := Interval_Multiply (Result, Length_Raw);
         end loop;
         return Result;
      end Expected_Derivative;

      function Measure_Endpoint
        (Jet : Endpoint_Tangent_Jet; U : Dimensionless; Bounds : out Endpoint_Jet_Error_Bounds) return Boolean
      is
         Actual : Interval_Position_Scale;
      begin
         Bounds := [others => 0.0];
         for Order in Endpoint_Tangent_Derivative_Order loop
            if not Cached_Evaluator_Tangent_Derivative_At (Evaluator.Antiderivative_Cache, U, Order, Actual) then
               return False;
            end if;
            declare
               Square_Sum : Interval := Interval_Exact (0.0);
            begin
               for Axis in Axis_Name loop
                  declare
                     Difference : constant Interval :=
                       Interval_Subtract (Actual (Axis), Expected_Derivative (Jet, Axis, Order));
                     Axis_Error : constant Dimensionless := Interval_Abs_Max (Difference);
                  begin
                     if not Difference.Valid or else not Is_Finite (Axis_Error) then
                        return False;
                     end if;
                     Square_Sum :=
                       Interval_Add
                         (Square_Sum, Interval_Multiply (Interval_Exact (Axis_Error), Interval_Exact (Axis_Error)));
                  end;
               end loop;
               if not Square_Sum.Valid or else Square_Sum.Upper < 0.0 then
                  return False;
               end if;
               Bounds (Order) := Certified_Upper_Square_Root (Dimensionless'Max (0.0, Square_Sum.Upper));
               if not Is_Finite (Bounds (Order)) or else Bounds (Order) >= Dimensionless'Last then
                  return False;
               end if;
            end;
         end loop;
         return True;
      end Measure_Endpoint;

      Start_Bounds, Finish_Bounds : Endpoint_Jet_Error_Bounds;
   begin
      Error_Bounds := [others => 0.0];
      if Evaluator.Kind /= Positive_Curve_Kind
        or else not Measure_Endpoint (Requested_Start_Jet, 0.0, Start_Bounds)
        or else not Measure_Endpoint (Requested_Finish_Jet, 1.0, Finish_Bounds)
      then
         Error_Bounds := [others => Dimensionless'Last];
         return False;
      end if;
      for Order in Endpoint_Tangent_Derivative_Order loop
         Error_Bounds (Order) := Dimensionless'Max (Start_Bounds (Order), Finish_Bounds (Order));
      end loop;
      return True;
   exception
      when Constraint_Error =>
         Error_Bounds := [others => Dimensionless'Last];
         return False;
   end Measure_Evaluator_Endpoint_Jet_Error;

   --  The final representation certificate compares two rational tangent fields in Bernstein form. Degree sixteen
   --  is enough for either numerator or denominator; cross multiplication therefore needs degree thirty-two.
   subtype Certificate_Polynomial_Index is Natural range 0 .. 2 * Maximum_Rational_Degree;
   type Certificate_Polynomial is array (Certificate_Polynomial_Index) of Interval;
   type Certificate_Centres is array (Certificate_Polynomial_Index) of Dimensionless;
   type Quadratic_Factor_Coefficients is array (Natural range 0 .. 2) of Interval;

   type Rational_Factor is record
      Degree        : Positive range 1 .. 2 := 1;
      Is_Real       : Boolean := True;
      Cache_Index   : Positive range 1 .. Maximum_Rational_Degree := 1;
      Coefficients  : Quadratic_Factor_Coefficients := [others => (Lower => 0.0, Upper => 0.0, Valid => True)];
      Normalization : Interval := (Lower => 1.0, Upper => 1.0, Valid => True);
   end record;

   type Rational_Factor_Array is array (Rational_Degree_Slot) of Rational_Factor;
   function Multiply_Bernstein
     (Left : Certificate_Polynomial; Left_Degree : Natural; Right : Certificate_Polynomial; Right_Degree : Natural)
      return Certificate_Polynomial;
   --  Multiply certificate polynomials in Bernstein form with interval coefficients.

   function Multiply_Bernstein
     (Left : Certificate_Polynomial; Left_Degree : Natural; Right : Certificate_Polynomial; Right_Degree : Natural)
      return Certificate_Polynomial
   is
      Result : Certificate_Polynomial := [others => Interval_Exact (0.0)];
   begin
      for I in 0 .. Left_Degree loop
         for J in 0 .. Right_Degree loop
            declare
               Product_Degree : constant Natural := Left_Degree + Right_Degree;
               Product_Index  : constant Natural := I + J;
               Raw_Weight     : constant Dimensionless :=
                 Binomial (Left_Degree, I) * Binomial (Right_Degree, J) / Binomial (Product_Degree, Product_Index);
               Weight         : constant Interval := Checked_Interval (Down (Raw_Weight), Up (Raw_Weight));
            begin
               Result (Product_Index) :=
                 Interval_Add
                   (Result (Product_Index), Interval_Multiply (Weight, Interval_Multiply (Left (I), Right (J))));
            end;
         end loop;
      end loop;
      return Result;
   end Multiply_Bernstein;

   function Elevate_Bernstein
     (Source : Certificate_Polynomial; Source_Degree : Natural; Target_Degree : Natural) return Certificate_Polynomial;
   --  Elevate a certificate polynomial to Target_Degree without changing its value.

   function Elevate_Bernstein
     (Source : Certificate_Polynomial; Source_Degree : Natural; Target_Degree : Natural) return Certificate_Polynomial
   is
      Result : Certificate_Polynomial := [others => Interval_Exact (0.0)];
   begin
      if Source_Degree > Target_Degree then
         raise Constraint_Error;
      end if;

      for Target_Index in 0 .. Target_Degree loop
         for Source_Index in 0 .. Source_Degree loop
            if Target_Index >= Source_Index and then Target_Index - Source_Index <= Target_Degree - Source_Degree then
               declare
                  Raw_Weight : constant Dimensionless :=
                    Binomial (Source_Degree, Source_Index)
                    * Binomial (Target_Degree - Source_Degree, Target_Index - Source_Index)
                    / Binomial (Target_Degree, Target_Index);
                  Weight     : constant Interval := Checked_Interval (Down (Raw_Weight), Up (Raw_Weight));
               begin
                  Result (Target_Index) :=
                    Interval_Add (Result (Target_Index), Interval_Multiply (Weight, Source (Source_Index)));
               end;
            end if;
         end loop;
      end loop;
      return Result;
   end Elevate_Bernstein;

   function Power_To_Bernstein
     (Power : Quadratic_Factor_Coefficients; Degree : Positive) return Quadratic_Factor_Coefficients;
   --  Convert a quadratic-factor polynomial from power to Bernstein coefficients.

   function Power_To_Bernstein
     (Power : Quadratic_Factor_Coefficients; Degree : Positive) return Quadratic_Factor_Coefficients
   is
      Result : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
   begin
      for Bernstein_Index in 0 .. Degree loop
         for Power_Index in 0 .. Bernstein_Index loop
            declare
               Raw_Weight : constant Dimensionless :=
                 Binomial (Bernstein_Index, Power_Index) / Binomial (Degree, Power_Index);
               Weight     : constant Interval := Checked_Interval (Down (Raw_Weight), Up (Raw_Weight));
            begin
               Result (Bernstein_Index) :=
                 Interval_Add (Result (Bernstein_Index), Interval_Multiply (Weight, Power (Power_Index)));
            end;
         end loop;
      end loop;
      return Result;
   end Power_To_Bernstein;

   procedure Add_Scaled_Polynomial
     (Target : in out Certificate_Polynomial; Source : Certificate_Polynomial; Degree : Natural; Scale : Interval);
   --  Add a scaled certificate polynomial to Target over its active degree.

   procedure Add_Scaled_Polynomial
     (Target : in out Certificate_Polynomial; Source : Certificate_Polynomial; Degree : Natural; Scale : Interval) is
   begin
      for Index in 0 .. Degree loop
         Target (Index) := Interval_Add (Target (Index), Interval_Multiply (Scale, Source (Index)));
      end loop;
   end Add_Scaled_Polynomial;

   function Certified_Realtime_Tangent_Error
     (Curve                 : Stereographic_Curve;
      Final_Cache           : Rational_Antiderivative;
      Denominator_Scale     : Dimensionless;
      Requested_Point_Error : Length;
      Retained_Certificate  : out Retained_Tangent_Bernstein_Certificate) return Tangent_Certificate_Result;
   --  Certify the real-time tangent approximation and retain its Bernstein certificate.

   function Certified_Realtime_Tangent_Error
     (Curve                 : Stereographic_Curve;
      Final_Cache           : Rational_Antiderivative;
      Denominator_Scale     : Dimensionless;
      Requested_Point_Error : Length;
      Retained_Certificate  : out Retained_Tangent_Bernstein_Certificate) return Tangent_Certificate_Result
   is
      Factors      : Rational_Factor_Array;
      Factor_Count : Natural range 0 .. Maximum_Rational_Degree := 0;

      Cached_Denominator          : Certificate_Polynomial := [others => Interval_Exact (0.0)];
      Cached_Denominator_Degree   : Natural range 0 .. Maximum_Rational_Degree := 0;
      Comparison_Degree           : constant Natural := Maximum_Tangent_Numerator_Degree;
      Compared_Cached_Denominator : Certificate_Polynomial := [others => Interval_Exact (0.0)];
      Cached_Denominator_Scale    : constant Interval := Interval_Exact (Denominator_Scale);
      Minimum_Cached_Denominator  : Dimensionless := 0.0;

      Warp            : constant Interval := Interval_Exact (Curve.Warp_Factor);
      Warp_Difference : constant Interval := Interval_Subtract (Warp, Interval_Exact (1.0));

      --  U = V / H(V), where V is the polynomial chart coordinate and U
      --  is normalized physical distance.
      H : constant array (Natural range 0 .. 1) of Interval := [0 => Warp, 1 => Interval_Negate (Warp_Difference)];

      Ideal_Chart_Squared_Norm       : Certificate_Polynomial := [others => Interval_Exact (0.0)];
      Ideal_Denominator              : Certificate_Polynomial := [others => Interval_Exact (0.0)];
      Ideal_Local_Numerator          : array (Frame_Component_Index) of Certificate_Polynomial :=
        [others => [others => Interval_Exact (0.0)]];
      Ideal_Axis_Numerator           : array (Axis_Name) of Certificate_Polynomial :=
        [others => [others => Interval_Exact (0.0)]];
      Cached_Axis_Numerator          : array (Axis_Name) of Certificate_Polynomial :=
        [others => [others => Interval_Exact (0.0)]];
      Compared_Cached_Axis_Numerator : array (Axis_Name) of Certificate_Polynomial :=
        [others => [others => Interval_Exact (0.0)]];

      procedure Append_Factor
        (Degree        : Positive;
         Is_Real       : Boolean;
         Cache_Index   : Positive;
         Coefficients  : Quadratic_Factor_Coefficients;
         Normalization : Interval);

      function Divide_Intervals (Numerator, Denominator : Interval) return Interval;

      procedure Product_Of_Other_Factors
        (Excluded : Positive; Product : out Certificate_Polynomial; Degree : out Natural);

      function Polynomial_Range_Bound
        (Polynomial : Certificate_Polynomial; Degree : Natural; Target : Dimensionless) return Dimensionless;

      procedure Append_Factor
        (Degree        : Positive;
         Is_Real       : Boolean;
         Cache_Index   : Positive;
         Coefficients  : Quadratic_Factor_Coefficients;
         Normalization : Interval) is
      begin
         --  Store each denominator factor normalized to 1 at V = 0. The removed scale is tracked separately so
         --  products retain the authoritative denominator normalization.
         if Degree > 2
           or else Factor_Count = Maximum_Rational_Degree
           or else not Normalization.Valid
           or else (Normalization.Lower <= 0.0 and then Normalization.Upper >= 0.0)
         then
            raise Constraint_Error;
         end if;

         Factor_Count := Factor_Count + 1;
         Factors (Factor_Count) :=
           (Degree        => Degree,
            Is_Real       => Is_Real,
            Cache_Index   => Cache_Index,
            Coefficients  => Coefficients,
            Normalization => Normalization);
      end Append_Factor;

      function Divide_Intervals (Numerator, Denominator : Interval) return Interval is
      begin
         if not Numerator.Valid
           or else not Denominator.Valid
           or else (Denominator.Lower <= 0.0 and then Denominator.Upper >= 0.0)
         then
            return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
         end if;

         return
           Interval_Multiply
             (Numerator, Checked_Interval (Down (1.0 / Denominator.Upper), Up (1.0 / Denominator.Lower)));
      exception
         when Constraint_Error =>
            return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      end Divide_Intervals;

      procedure Product_Of_Other_Factors
        (Excluded : Positive; Product : out Certificate_Polynomial; Degree : out Natural) is
      begin
         --  A partial-fraction numerator for factor qᵢ is multiplied by ∏ⱼ≠ᵢqⱼ before all terms are summed over
         --  the common denominator.
         Product := [others => Interval_Exact (0.0)];
         Product (0) := Cached_Denominator_Scale;
         Degree := 0;

         for Factor_Index in 1 .. Factor_Count loop
            if Factor_Index /= Excluded then
               declare
                  Factor_Polynomial : Certificate_Polynomial := [others => Interval_Exact (0.0)];
               begin
                  for Coefficient_Index in 0 .. Factors (Factor_Index).Degree loop
                     Factor_Polynomial (Coefficient_Index) := Factors (Factor_Index).Coefficients (Coefficient_Index);
                  end loop;
                  Product := Multiply_Bernstein (Product, Degree, Factor_Polynomial, Factors (Factor_Index).Degree);
                  Degree := Degree + Factors (Factor_Index).Degree;
               end;
            end if;
         end loop;
      end Product_Of_Other_Factors;

      function Polynomial_Range_Bound
        (Polynomial : Certificate_Polynomial; Degree : Natural; Target : Dimensionless) return Dimensionless
      is
         Maximum_Subdivision_Depth : constant := 6;
         Centres                   : Certificate_Centres := [others => 0.0];
         Radii                     : Certificate_Centres := [others => 0.0];

         function Coefficient_Bound (Local_Centres, Local_Radii : Certificate_Centres) return Dimensionless;

         function Refined_Bound
           (Local_Centres, Local_Radii : Certificate_Centres; Depth : Natural) return Dimensionless;

         function Coefficient_Bound (Local_Centres, Local_Radii : Certificate_Centres) return Dimensionless is
            Bound : Dimensionless := 0.0;
         begin
            --  The value of a Bernstein polynomial lies in the convex hull of its controls.
            for Index in 0 .. Degree loop
               Bound := Dimensionless'Max (Bound, abs Local_Centres (Index) + Local_Radii (Index));
            end loop;
            return Up (Bound);
         end Coefficient_Bound;

         function Refined_Bound
           (Local_Centres, Local_Radii : Certificate_Centres; Depth : Natural) return Dimensionless
         is
            Current                    : constant Dimensionless := Coefficient_Bound (Local_Centres, Local_Radii);
            Work_Centres               : Certificate_Centres := Local_Centres;
            Work_Radii                 : Certificate_Centres := Local_Radii;
            Left_Centres, Left_Radii   : Certificate_Centres := [others => 0.0];
            Right_Centres, Right_Radii : Certificate_Centres := [others => 0.0];
         begin
            if Current <= Target or else Depth = Maximum_Subdivision_Depth or else Degree = 0 then
               return Current;
            end if;

            Left_Centres (0) := Work_Centres (0);
            Left_Radii (0) := Work_Radii (0);
            Right_Centres (Degree) := Work_Centres (Degree);
            Right_Radii (Degree) := Work_Radii (Degree);

            --  De Casteljau subdivision at one half preserves Bernstein
            --  form. The radius tracks roundoff introduced while forming
            --  every midpoint.
            for Level in 1 .. Degree loop
               for Index in 0 .. Degree - Level loop
                  declare
                     New_Centre : constant Dimensionless := 0.5 * (Work_Centres (Index) + Work_Centres (Index + 1));
                     Magnitude  : constant Dimensionless :=
                       abs Work_Centres (Index)
                       + abs Work_Centres (Index + 1)
                       + Work_Radii (Index)
                       + Work_Radii (Index + 1)
                       + Dimensionless'Model_Small;
                  begin
                     Work_Centres (Index) := New_Centre;
                     Work_Radii (Index) :=
                       (0.5 * (Work_Radii (Index) + Work_Radii (Index + 1))
                        + 8.0 * Dimensionless'Model_Epsilon * Magnitude)
                       * (1.0 + 16.0 * Dimensionless'Model_Epsilon);
                  end;
               end loop;
               Left_Centres (Level) := Work_Centres (0);
               Left_Radii (Level) := Work_Radii (0);
               Right_Centres (Degree - Level) := Work_Centres (Degree - Level);
               Right_Radii (Degree - Level) := Work_Radii (Degree - Level);
            end loop;

            return
              Dimensionless'Max
                (Refined_Bound (Left_Centres, Left_Radii, Depth + 1),
                 Refined_Bound (Right_Centres, Right_Radii, Depth + 1));
         end Refined_Bound;
      begin
         for Index in 0 .. Degree loop
            if not Polynomial (Index).Valid then
               return Dimensionless'Last;
            end if;

            Centres (Index) := Polynomial (Index).Lower + 0.5 * (Polynomial (Index).Upper - Polynomial (Index).Lower);
            Radii (Index) :=
              Dimensionless'Max
                (abs (Centres (Index) - Polynomial (Index).Lower), abs (Polynomial (Index).Upper - Centres (Index)))
              * (1.0 + 8.0 * Dimensionless'Model_Epsilon);
         end loop;
         return Refined_Bound (Centres, Radii, 0);
      exception
         when Constraint_Error =>
            return Dimensionless'Last;
      end Polynomial_Range_Bound;

   begin
      Retained_Certificate := (others => <>);
      if not Rational_Antiderivative_Primitives_Are_Safe (Final_Cache)
        or else Denominator_Scale <= 0.0
        or else not Is_Finite (Denominator_Scale)
        or else Requested_Point_Error <= 0.0 * mm
        or else Curve.Evaluator_Data.Length_Value <= 0.0 * mm
      then
         return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
      end if;

      --  Reconstruct every factor of the final U-space cache after pulling it back through U = V/H(V).
      --  Normalizing each factor at V = 0 avoids large and cancellation-prone power coefficients.
      for Index in 1 .. Final_Cache.Real_Pole_Count loop
         declare
            Slot             : Rational_Pole_Slot renames Final_Cache.Pole_Slots (Index);
            Pole             : constant Interval := Interval_Exact (Slot.Pole_Component);
            Factor_Power     : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
            Normalized_Power : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
            Normalization    : Interval;
         begin
            Factor_Power (0) := Interval_Negate (Interval_Multiply (Pole, H (0)));
            Factor_Power (1) := Interval_Add (Interval_Exact (1.0), Interval_Multiply (Pole, Warp_Difference));
            Normalization := Factor_Power (0);
            Normalized_Power (0) := Interval_Exact (1.0);
            Normalized_Power (1) := Divide_Intervals (Factor_Power (1), Normalization);
            Append_Factor
              (Degree        => 1,
               Is_Real       => True,
               Cache_Index   => Index,
               Coefficients  => Power_To_Bernstein (Normalized_Power, 1),
               Normalization => Normalization);
         end;
      end loop;

      for Pair in 1 .. Final_Cache.Pair_Count loop
         declare
            Pair_Index       : constant Rational_Pair_Index := Rational_Pair_Index (Pair);
            Real_Slot        : Rational_Pole_Slot renames
              Final_Cache.Pole_Slots (Complex_Pair_Real_Slot (Final_Cache, Pair_Index));
            Imaginary_Slot   : Rational_Pole_Slot renames
              Final_Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Final_Cache, Pair_Index));
            Pole_Real        : constant Interval := Interval_Exact (Real_Slot.Pole_Component);
            Pole_Imaginary   : constant Interval := Interval_Exact (Imaginary_Slot.Pole_Component);
            Real_Linear      : constant array (Natural range 0 .. 1) of Interval :=
              [0 => Interval_Negate (Interval_Multiply (Pole_Real, H (0))),
               1 => Interval_Subtract (Interval_Exact (1.0), Interval_Multiply (Pole_Real, H (1)))];
            Imaginary_Linear : constant array (Natural range 0 .. 1) of Interval :=
              [for Index in 0 .. 1 => Interval_Multiply (Pole_Imaginary, H (Index))];
            Factor_Power     : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
            Normalized_Power : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
            Normalization    : Interval;
         begin
            for Left_Index in 0 .. 1 loop
               for Right_Index in 0 .. 1 loop
                  Factor_Power (Left_Index + Right_Index) :=
                    Interval_Add
                      (Factor_Power (Left_Index + Right_Index),
                       Interval_Add
                         (Interval_Multiply (Real_Linear (Left_Index), Real_Linear (Right_Index)),
                          Interval_Multiply (Imaginary_Linear (Left_Index), Imaginary_Linear (Right_Index))));
               end loop;
            end loop;
            Normalization := Factor_Power (0);
            Normalized_Power (0) := Interval_Exact (1.0);
            for Index in 1 .. 2 loop
               Normalized_Power (Index) := Divide_Intervals (Factor_Power (Index), Normalization);
            end loop;
            Append_Factor
              (Degree        => 2,
               Is_Real       => False,
               Cache_Index   => Pair,
               Coefficients  => Power_To_Bernstein (Normalized_Power, 2),
               Normalization => Normalization);
         end;
      end loop;

      Cached_Denominator (0) := Cached_Denominator_Scale;
      --  Multiply the normalized linear and quadratic factors to reconstruct the cached denominator Q(V).
      for Factor_Index in 1 .. Factor_Count loop
         declare
            Factor_Polynomial : Certificate_Polynomial := [others => Interval_Exact (0.0)];
         begin
            for Coefficient_Index in 0 .. Factors (Factor_Index).Degree loop
               Factor_Polynomial (Coefficient_Index) := Factors (Factor_Index).Coefficients (Coefficient_Index);
            end loop;
            Cached_Denominator :=
              Multiply_Bernstein
                (Cached_Denominator, Cached_Denominator_Degree, Factor_Polynomial, Factors (Factor_Index).Degree);
            Cached_Denominator_Degree := Cached_Denominator_Degree + Factors (Factor_Index).Degree;
         end;
      end loop;

      for Axis in Axis_Name loop
         Add_Scaled_Polynomial
           (Cached_Axis_Numerator (Axis),
            Cached_Denominator,
            Cached_Denominator_Degree,
            Interval_Exact (Final_Cache.Constant_Tangent (Axis)));
      end loop;

      --  Add each partial-fraction numerator after multiplying by every denominator factor except its own.
      for Factor_Index in 1 .. Factor_Count loop
         declare
            Other_Factors : Certificate_Polynomial;
            Other_Degree  : Natural;
         begin
            Product_Of_Other_Factors (Factor_Index, Other_Factors, Other_Degree);

            if Factors (Factor_Index).Is_Real then
               declare
                  Slot : Rational_Pole_Slot renames Final_Cache.Pole_Slots (Factors (Factor_Index).Cache_Index);
               begin
                  for Axis in Axis_Name loop
                     declare
                        Residue             : constant Interval := Interval_Exact (Slot.Residue_Component (Axis));
                        Numerator_Power     : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
                        Numerator_Bernstein : Quadratic_Factor_Coefficients;
                        Numerator           : Certificate_Polynomial := [others => Interval_Exact (0.0)];
                        Term                : Certificate_Polynomial;
                     begin
                        for Index in 0 .. 1 loop
                           Numerator_Power (Index) :=
                             Divide_Intervals
                               (Interval_Multiply (Residue, H (Index)), Factors (Factor_Index).Normalization);
                        end loop;
                        Numerator_Bernstein := Power_To_Bernstein (Numerator_Power, 1);
                        for Index in 0 .. 1 loop
                           Numerator (Index) := Numerator_Bernstein (Index);
                        end loop;
                        Term := Multiply_Bernstein (Other_Factors, Other_Degree, Numerator, 1);
                        Add_Scaled_Polynomial
                          (Cached_Axis_Numerator (Axis), Term, Other_Degree + 1, Interval_Exact (1.0));
                     end;
                  end loop;
               end;
            else
               declare
                  Pair           : constant Rational_Pair_Index :=
                    Rational_Pair_Index (Factors (Factor_Index).Cache_Index);
                  Real_Slot      : Rational_Pole_Slot renames
                    Final_Cache.Pole_Slots (Complex_Pair_Real_Slot (Final_Cache, Pair));
                  Imaginary_Slot : Rational_Pole_Slot renames
                    Final_Cache.Pole_Slots (Complex_Pair_Imaginary_Slot (Final_Cache, Pair));
                  Pole_Real      : constant Interval := Interval_Exact (Real_Slot.Pole_Component);
                  Pole_Imaginary : constant Interval := Interval_Exact (Imaginary_Slot.Pole_Component);
               begin
                  for Axis in Axis_Name loop
                     declare
                        Residue_Real        : constant Interval := Interval_Exact (Real_Slot.Residue_Component (Axis));
                        Residue_Imaginary   : constant Interval :=
                          Interval_Exact (Imaginary_Slot.Residue_Component (Axis));
                        Real_Linear         : constant array (Natural range 0 .. 1) of Interval :=
                          [0 => Interval_Negate (Interval_Multiply (Pole_Real, H (0))),
                           1 => Interval_Subtract (Interval_Exact (1.0), Interval_Multiply (Pole_Real, H (1)))];
                        Inner               : array (Natural range 0 .. 1) of Interval;
                        Numerator_Power     : Quadratic_Factor_Coefficients := [others => Interval_Exact (0.0)];
                        Numerator_Bernstein : Quadratic_Factor_Coefficients;
                        Pair_Numerator      : Certificate_Polynomial := [others => Interval_Exact (0.0)];
                        Term                : Certificate_Polynomial;
                     begin
                        for Index in 0 .. 1 loop
                           Inner (Index) :=
                             Interval_Subtract
                               (Interval_Multiply (Residue_Real, Real_Linear (Index)),
                                Interval_Multiply (Residue_Imaginary, Interval_Multiply (Pole_Imaginary, H (Index))));
                        end loop;

                        for H_Index in 0 .. 1 loop
                           for Inner_Index in 0 .. 1 loop
                              Numerator_Power (H_Index + Inner_Index) :=
                                Interval_Add
                                  (Numerator_Power (H_Index + Inner_Index),
                                   Interval_Multiply
                                     (Interval_Exact (2.0), Interval_Multiply (H (H_Index), Inner (Inner_Index))));
                           end loop;
                        end loop;

                        for Index in 0 .. 2 loop
                           Numerator_Power (Index) :=
                             Divide_Intervals (Numerator_Power (Index), Factors (Factor_Index).Normalization);
                        end loop;
                        Numerator_Bernstein := Power_To_Bernstein (Numerator_Power, 2);
                        for Index in 0 .. 2 loop
                           Pair_Numerator (Index) := Numerator_Bernstein (Index);
                        end loop;
                        Term := Multiply_Bernstein (Other_Factors, Other_Degree, Pair_Numerator, 2);
                        Add_Scaled_Polynomial
                          (Cached_Axis_Numerator (Axis), Term, Other_Degree + 2, Interval_Exact (1.0));
                     end;
                  end loop;
               end;
            end if;
         end;
      end loop;

      --  Build the authoritative ideal tangent directly from the stored degree-eight Bernstein chart:
      --
      --     local numerator = (1 - Y·Y, 2Y)
      --     denominator      = 1 + Y·Y.
      --
      --  Avoiding a power-basis round trip is important: sharp curves have large intermediate power coefficients
      --  whose cancellation would make the interval certificate needlessly wide.
      for Component in Chart_Component_Index loop
         declare
            Chart  : Certificate_Polynomial := [others => Interval_Exact (0.0)];
            Square : Certificate_Polynomial;
         begin
            for Index in Chart_Coefficient_Index loop
               Chart (Index) := Interval_Exact (Curve.Coefficients (Index, Component));
            end loop;
            Square := Multiply_Bernstein (Chart, Fixed_Chart_Degree, Chart, Fixed_Chart_Degree);
            for Index in 0 .. Comparison_Degree loop
               Ideal_Chart_Squared_Norm (Index) := Interval_Add (Ideal_Chart_Squared_Norm (Index), Square (Index));
            end loop;
         end;
      end loop;

      Ideal_Denominator := Ideal_Chart_Squared_Norm;
      for Index in 0 .. Comparison_Degree loop
         Ideal_Denominator (Index) := Interval_Add (Ideal_Denominator (Index), Interval_Exact (1.0));
         Ideal_Local_Numerator (0) (Index) :=
           Interval_Subtract (Interval_Exact (1.0), Ideal_Chart_Squared_Norm (Index));
      end loop;

      for Component in Chart_Component_Index loop
         declare
            Chart    : Certificate_Polynomial := [others => Interval_Exact (0.0)];
            Elevated : Certificate_Polynomial;
         begin
            for Index in Chart_Coefficient_Index loop
               Chart (Index) := Interval_Exact (Curve.Coefficients (Index, Component));
            end loop;
            Elevated := Elevate_Bernstein (Chart, Fixed_Chart_Degree, Comparison_Degree);
            for Index in 0 .. Comparison_Degree loop
               Ideal_Local_Numerator (Component) (Index) := Interval_Multiply (Interval_Exact (2.0), Elevated (Index));
            end loop;
         end;
      end loop;

      for Axis in Axis_Name loop
         for Component in Frame_Component_Index loop
            Add_Scaled_Polynomial
              (Ideal_Axis_Numerator (Axis),
               Ideal_Local_Numerator (Component),
               Comparison_Degree,
               Interval_Exact (Curve.Frame (Component) (Axis)));
         end loop;
      end loop;

      Compared_Cached_Denominator :=
        Elevate_Bernstein (Cached_Denominator, Cached_Denominator_Degree, Comparison_Degree);
      for Axis in Axis_Name loop
         Compared_Cached_Axis_Numerator (Axis) :=
           Elevate_Bernstein (Cached_Axis_Numerator (Axis), Cached_Denominator_Degree, Comparison_Degree);
      end loop;

      --  D is at least one because D = 1 + Y·Y. If the reconstructed cached Q differs from D by less than one,
      --  then
      --
      --     Q ≥ D - |Q - D| ≥ 1 - |Q - D| > 0,
      --
      --  so division by Q is safe over the complete curve.
      declare
         Denominator_Difference : Certificate_Polynomial := [others => Interval_Exact (0.0)];
         Difference_Bound       : Dimensionless;
      begin
         for Index in 0 .. Comparison_Degree loop
            Denominator_Difference (Index) :=
              Interval_Subtract (Compared_Cached_Denominator (Index), Ideal_Denominator (Index));
         end loop;
         Difference_Bound := Polynomial_Range_Bound (Denominator_Difference, Comparison_Degree, 0.25);
         if Difference_Bound = Dimensionless'Last or else not Is_Finite (Difference_Bound) then
            return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
         elsif Difference_Bound >= 1.0 then
            return (Status => Tangent_Could_Not_Be_Certified);
         end if;

         Minimum_Cached_Denominator := Down (1.0 - Difference_Bound);
         if Minimum_Cached_Denominator <= 0.0 then
            return (Status => Tangent_Could_Not_Be_Certified);
         end if;
      end;

      --  Publish the same outward-rounded common-denominator form used by the ideal-to-realtime tangent proof.
      --  Reusing these controls for later derivative bounds avoids reconstructing the rational function from poles
      --  a second time, and setting Valid only after Q has a positive lower bound prevents a partial certificate
      --  from escaping on an earlier failure path.
      Retained_Certificate.Valid := True;
      Retained_Certificate.Degree := Cached_Denominator_Degree;
      Retained_Certificate.Minimum_Denominator := Minimum_Cached_Denominator;
      for Index in 0 .. Cached_Denominator_Degree loop
         Retained_Certificate.Denominator (Index) := Cached_Denominator (Index);
         for Axis in Axis_Name loop
            Retained_Certificate.Axis_Numerators (Axis) (Index) := Cached_Axis_Numerator (Axis) (Index);
         end loop;
      end loop;

      declare
         Squared_Error_Sum          : Dimensionless := 0.0;
         Axis_Count                 : constant Dimensionless :=
           Dimensionless (Axis_Name'Pos (Axis_Name'Last) - Axis_Name'Pos (Axis_Name'First) + 1);
         Requested_Normalized_Error : constant Dimensionless :=
           Requested_Point_Error / Curve.Evaluator_Data.Length_Value;
         Subdivision_Target         : constant Dimensionless :=
           Minimum_Cached_Denominator * Requested_Normalized_Error / (16.0 * Dimensionless_Math.Sqrt (Axis_Count));
      begin
         for Axis in Axis_Name loop
            declare
               --  Compare cached N/Q with ideal P/D without interval division:
               --
               --     N/Q - P/D = (N·D - P·Q)/(Q·D).
               --
               --  Since D ≥ 1 and Q ≥ Minimum_Cached_Denominator, the axis error is bounded by the numerator
               --  residual divided by Minimum_Cached_Denominator.
               Cached_Times_Ideal_Denominator : constant Certificate_Polynomial :=
                 Multiply_Bernstein
                   (Compared_Cached_Axis_Numerator (Axis), Comparison_Degree, Ideal_Denominator, Comparison_Degree);
               Ideal_Times_Cached_Denominator : constant Certificate_Polynomial :=
                 Multiply_Bernstein
                   (Ideal_Axis_Numerator (Axis), Comparison_Degree, Compared_Cached_Denominator, Comparison_Degree);
               Cross_Residual                 : Certificate_Polynomial := [others => Interval_Exact (0.0)];
               Residual_Bound                 : Dimensionless;
               Axis_Error                     : Dimensionless;
            begin
               for Index in 0 .. 2 * Comparison_Degree loop
                  Cross_Residual (Index) :=
                    Interval_Subtract (Cached_Times_Ideal_Denominator (Index), Ideal_Times_Cached_Denominator (Index));
               end loop;
               Residual_Bound := Polynomial_Range_Bound (Cross_Residual, 2 * Comparison_Degree, Subdivision_Target);
               if Residual_Bound = Dimensionless'Last or else not Is_Finite (Residual_Bound) then
                  return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
               end if;
               Axis_Error := Up (Residual_Bound / Minimum_Cached_Denominator);
               Squared_Error_Sum := Up (Squared_Error_Sum + Up (Axis_Error * Axis_Error));
            end;
         end loop;
         declare
            Error_Bound : constant Dimensionless := Certified_Upper_Square_Root (Squared_Error_Sum);
         begin
            --  Combine independent axis bounds in Euclidean norm. Build_Rational_Representation later multiplies
            --  this unit-tangent error by curve length to bound integrated position error.
            if Error_Bound = Dimensionless'Last or else not Is_Finite (Error_Bound) then
               return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
            else
               return (Status => Tangent_Was_Certified, Error_Bound => Error_Bound);
            end if;
         end;
      end;
   exception
      when Constraint_Error =>
         return (Status => Tangent_Certificate_Is_Numerically_Unsafe);
   end Certified_Realtime_Tangent_Error;

   function Build_Rational_Representation
     (Curve                  : in out Stereographic_Curve;
      Power_Coefficients     : Power_Chart;
      Maximum_Position_Error : Length;
      Start_Jet, Finish_Jet  : Endpoint_Tangent_Jet) return Realtime_Compilation_Status
   is
      subtype Polynomial_Index is Natural range 0 .. Maximum_Rational_Degree;
      type Real_Polynomial is array (Polynomial_Index) of Dimensionless;

      package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Dimensionless);
      use Complex_Types;

      type Complex_Root_Array is array (Rational_Degree_Slot) of Complex;
      type Temporary_Real_Poles is array (Rational_Degree_Slot) of Dimensionless;
      type Temporary_Complex_Poles is array (Rational_Pair_Index) of Complex;

      R2, Denominator        : Real_Polynomial := [others => 0.0];
      Local_Numerators       : array (Frame_Component_Index) of Real_Polynomial := [others => [others => 0.0]];
      Numerators             : array (Axis_Name) of Real_Polynomial := [others => [others => 0.0]];
      Roots                  : Complex_Root_Array := [others => Compose_From_Cartesian (0.0, 0.0)];
      Denominator_Derivative : Real_Polynomial := [others => 0.0];
      Real_Poles             : Temporary_Real_Poles := [others => 0.0];
      Pair_Poles             : Temporary_Complex_Poles := [others => Compose_From_Cartesian (0.0, 0.0)];
      Real_Pole_Count        : Natural range 0 .. Maximum_Rational_Degree := 0;
      Pair_Count             : Natural range 0 .. Maximum_Rational_Degree / 2 := 0;
      Chart_Degree           : Natural range 0 .. Fixed_Chart_Degree := 0;
      Denominator_Degree     : Natural range 0 .. Maximum_Rational_Degree := 0;
      Cache                  : Rational_Antiderivative := (others => <>);

      function Evaluate_Polynomial (Coefficients : Real_Polynomial; Degree : Natural; Value : Complex) return Complex;
      --  Evaluate a real-coefficient polynomial at a complex value using Horner's method.

      function Evaluate_Polynomial (Coefficients : Real_Polynomial; Degree : Natural; Value : Complex) return Complex
      is
         Result : Complex := Compose_From_Cartesian (Coefficients (Degree), 0.0);
      begin
         if Degree > 0 then
            for Index in reverse 0 .. Degree - 1 loop
               Result := Result * Value + Compose_From_Cartesian (Coefficients (Index), 0.0);
            end loop;
         end if;
         return Result;
      end Evaluate_Polynomial;

      function Complex_Is_Finite (Value : Complex) return Boolean
      is (Value.Re >= -Dimensionless'Last
          and then Value.Re <= Dimensionless'Last
          and then Value.Im >= -Dimensionless'Last
          and then Value.Im <= Dimensionless'Last);

      function Root_Scale (Root : Complex) return Dimensionless
      is (Dimensionless'Max (1.0, Dimensionless (abs Root)));

      function Polynomial_Scale_At
        (Coefficients : Real_Polynomial; Degree : Natural; Root : Complex) return Dimensionless;
      --  Estimate the polynomial evaluation scale at a candidate root.

      function Polynomial_Scale_At
        (Coefficients : Real_Polynomial; Degree : Natural; Root : Complex) return Dimensionless
      is
         Scale  : Dimensionless := 0.0;
         Power  : Dimensionless := 1.0;
         Radius : constant Dimensionless := Dimensionless (abs Root);
      begin
         for Index in 0 .. Degree loop
            Scale := Scale + abs Coefficients (Index) * Power;
            Power := Power * Radius;
         end loop;
         return Dimensionless'Max (1.0, Scale);
      end Polynomial_Scale_At;

      function Distance_From_Unit_Interval (Root : Complex) return Dimensionless;
      --  Return the distance from a complex root to the real unit interval.

      function Distance_From_Unit_Interval (Root : Complex) return Dimensionless is
         Root_Real     : constant Dimensionless := Dimensionless (Re (Root));
         Real_Distance : constant Dimensionless :=
           (if Root_Real < 0.0 then -Root_Real elsif Root_Real > 1.0 then Root_Real - 1.0 else 0.0);
      begin
         return
           Dimensionless_Math.Sqrt
             (Real_Distance * Real_Distance + Dimensionless (Im (Root)) * Dimensionless (Im (Root)));
      end Distance_From_Unit_Interval;

      function Find_Roots
        (Coefficients : Real_Polynomial; Degree : Natural; Result : out Complex_Root_Array) return Boolean;
      --  Find and validate all complex roots of the supplied real polynomial.

      function Find_Roots
        (Coefficients : Real_Polynomial; Degree : Natural; Result : out Complex_Root_Array) return Boolean
      is
         Order                       : constant Prunt.LAPACK.Fortran_Integer := Prunt.LAPACK.Fortran_Integer (Degree);
         Matrix                      :
           Prunt.LAPACK.Double_Precision_Matrix
             (1 .. Prunt.LAPACK.Fortran_Integer (Maximum_Rational_Degree),
              1 .. Prunt.LAPACK.Fortran_Integer (Maximum_Rational_Degree)) := [others => [others => 0.0]];
         Real_Roots, Imaginary_Roots :
           Prunt.LAPACK.Double_Precision_Vector (1 .. Prunt.LAPACK.Fortran_Integer (Maximum_Rational_Degree)) :=
             [others => 0.0];
         Dummy_Left, Dummy_Right     : Prunt.LAPACK.Double_Precision_Matrix (1 .. 1, 1 .. 1) :=
           [others => [others => 0.0]];
         Work_Length                 : constant Prunt.LAPACK.Fortran_Integer :=
           Prunt.LAPACK.Fortran_Integer'Max (1, Prunt.LAPACK.Fortran_Integer (4 * Degree));
         Work                        : Prunt.LAPACK.Double_Precision_Vector (1 .. Work_Length) := [others => 0.0];
         Info                        : Prunt.LAPACK.Fortran_Integer;
         Leading                     : constant Dimensionless := Coefficients (Degree);
      begin
         Result := [others => Compose_From_Cartesian (0.0, 0.0)];
         if Degree = 0 then
            return True;
         elsif Leading = 0.0 or else not Is_Finite (Leading) then
            return False;
         end if;

         --  The eigenvalues of the monic companion matrix are the roots of
         --
         --     aₙxⁿ + aₙ₋₁xⁿ⁻¹ + … + a₀.
         for Row in 2 .. Degree loop
            Matrix (Prunt.LAPACK.Fortran_Integer (Row), Prunt.LAPACK.Fortran_Integer (Row - 1)) := 1.0;
         end loop;
         for Row in 1 .. Degree loop
            declare
               Coefficient : constant Dimensionless := Coefficients (Row - 1) / Leading;
            begin
               if not Is_Finite (Coefficient) then
                  return False;
               end if;
               Matrix (Prunt.LAPACK.Fortran_Integer (Row), Order) := Prunt.LAPACK.Double_Precision (-Coefficient);
            end;
         end loop;

         Prunt.LAPACK.DGEEV
           (Jobvl => 'N',
            Jobvr => 'N',
            N     => Order,
            A     => Matrix,
            Lda   => Prunt.LAPACK.Fortran_Integer (Maximum_Rational_Degree),
            Wr    => Real_Roots,
            Wi    => Imaginary_Roots,
            Vl    => Dummy_Left,
            Ldvl  => 1,
            Vr    => Dummy_Right,
            Ldvr  => 1,
            Work  => Work,
            Lwork => Work_Length,
            Info  => Info);
         if Integer (Info) /= 0 then
            return False;
         end if;

         for Index in 1 .. Degree loop
            Result (Index) :=
              Compose_From_Cartesian
                (Dimensionless (Real_Roots (Prunt.LAPACK.Fortran_Integer (Index))),
                 Dimensionless (Imaginary_Roots (Prunt.LAPACK.Fortran_Integer (Index))));
         end loop;
         return True;
      exception
         when Constraint_Error =>
            Result := [others => Compose_From_Cartesian (0.0, 0.0)];
            return False;
      end Find_Roots;

      function Ideal_Axis_Is_Structurally_Zero (Axis : Axis_Name) return Boolean;
      --  Test whether the ideal tangent numerator is identically zero for an axis.

      function Ideal_Axis_Is_Structurally_Zero (Axis : Axis_Name) return Boolean is
      begin
         if Curve.Frame (0) (Axis) /= 0.0 then
            return False;
         end if;
         for Component in Chart_Component_Index loop
            if Curve.Frame (Component) (Axis) /= 0.0
              and then
                (for some Coefficient in Chart_Coefficient_Index => Curve.Coefficients (Coefficient, Component) /= 0.0)
            then
               return False;
            end if;
         end loop;
         return True;
      exception
         when Constraint_Error =>
            return False;
      end Ideal_Axis_Is_Structurally_Zero;

   begin
      --  Reject malformed inputs before any polynomial or root arithmetic can contaminate the output cache.
      if Curve.Evaluator_Data.Kind /= Positive_Curve_Kind
        or else Curve.Evaluator_Data.Length_Value <= 0.0 * mm
        or else Maximum_Position_Error <= 0.0 * mm
        or else Maximum_Position_Error > Length'Last
      then
         return Realtime_Compilation_Numerically_Unsafe;
      end if;

      for Axis in Axis_Name loop
         Curve.Structurally_Constant_Axes (Axis) :=
           Curve.Evaluator_Data.Start_Point (Axis) = Curve.Evaluator_Data.Finish_Point (Axis)
           and then Ideal_Axis_Is_Structurally_Zero (Axis);
      end loop;

      for Degree in reverse 0 .. Fixed_Chart_Degree loop
         declare
            Row_Norm : Dimensionless := 0.0;
         begin
            for Component in Chart_Component_Index loop
               Row_Norm := Row_Norm + Power_Coefficients (Degree, Component) ** 2;
            end loop;
            if Row_Norm > 0.0 then
               Chart_Degree := Degree;
               exit;
            end if;
         end;
      end loop;
      Denominator_Degree := 2 * Chart_Degree;

      --  The inverse-stereographic tangent in chart coordinate V is
      --
      --     T(V) = F·((1 - Y·Y), 2Y)/(1 + Y·Y).
      --
      --  Build its common denominator D = 1 + Y·Y and one numerator Nₐ for each physical axis.
      for Component in Chart_Component_Index loop
         for Left in 0 .. Chart_Degree loop
            for Right in 0 .. Chart_Degree loop
               R2 (Left + Right) :=
                 R2 (Left + Right) + Power_Coefficients (Left, Component) * Power_Coefficients (Right, Component);
            end loop;
         end loop;
      end loop;
      Denominator := R2;
      Denominator (0) := Denominator (0) + 1.0;
      for Degree in Polynomial_Index loop
         Local_Numerators (0) (Degree) := -R2 (Degree);
      end loop;
      Local_Numerators (0) (0) := Local_Numerators (0) (0) + 1.0;
      for Component in Chart_Component_Index loop
         for Degree in 0 .. Chart_Degree loop
            Local_Numerators (Component) (Degree) := 2.0 * Power_Coefficients (Degree, Component);
         end loop;
      end loop;
      for Axis in Axis_Name loop
         for Component in Frame_Component_Index loop
            for Degree in 0 .. Denominator_Degree loop
               Numerators (Axis) (Degree) :=
                 Numerators (Axis) (Degree) + Curve.Frame (Component) (Axis) * Local_Numerators (Component) (Degree);
            end loop;
         end loop;
      end loop;

      if not Find_Roots (Denominator, Denominator_Degree, Roots) then
         return Realtime_Representation_Insufficient;
      end if;
      if Denominator_Degree > 0 then
         for Degree in 1 .. Denominator_Degree loop
            Denominator_Derivative (Degree - 1) := Dimensionless (Degree) * Denominator (Degree);
         end loop;
      end if;

      for Index in 1 .. Denominator_Degree loop
         --  DGEEV supplies approximate companion-matrix eigenvalues. Up to three Newton steps reduce the polynomial
         --  residual, but a step is accepted only when it improves |D(p)|.
         for Pass in 1 .. 3 loop
            exit when not Complex_Is_Finite (Roots (Index)) or else abs Roots (Index) > 1.0E6;
            declare
               Value      : constant Complex := Evaluate_Polynomial (Denominator, Denominator_Degree, Roots (Index));
               Derivative : constant Complex :=
                 Evaluate_Polynomial (Denominator_Derivative, Denominator_Degree - 1, Roots (Index));
            begin
               exit when
                 not Complex_Is_Finite (Value)
                 or else not Complex_Is_Finite (Derivative)
                 or else abs Derivative <= 64.0 * Dimensionless'Model_Epsilon;
               declare
                  Trial : constant Complex := Roots (Index) - Value / Derivative;
               begin
                  if Complex_Is_Finite (Trial)
                    and then abs Evaluate_Polynomial (Denominator, Denominator_Degree, Trial) < abs Value
                  then
                     Roots (Index) := Trial;
                  end if;
               end;
            end;
         end loop;
      end loop;

      for Index in 1 .. Denominator_Degree loop
         declare
            Root_Is_Finite    : constant Boolean := Complex_Is_Finite (Roots (Index));
            Interval_Distance : constant Dimensionless := Distance_From_Unit_Interval (Roots (Index));
            Residual          : constant Dimensionless :=
              Dimensionless (abs Evaluate_Polynomial (Denominator, Denominator_Degree, Roots (Index)));
            Residual_Scale    : constant Dimensionless :=
              Polynomial_Scale_At (Denominator, Denominator_Degree, Roots (Index));
         begin
            --  Every pole must be finite, separated from the evaluation interval, and an accurate root of D.
            if not Root_Is_Finite or else Interval_Distance < 2.0E-9 or else Residual > 2.0E-8 * Residual_Scale then
               return Realtime_Representation_Insufficient;
            end if;
         end;
         for Other in Index + 1 .. Denominator_Degree loop
            --  The residue formula divides by D′(p), so repeated or numerically merged roots are unsupported.
            if Dimensionless (abs (Roots (Index) - Roots (Other)))
              < 2.0E-13 * Dimensionless'Max (Root_Scale (Roots (Index)), Root_Scale (Roots (Other)))
            then
               return Realtime_Representation_Insufficient;
            end if;
         end loop;
      end loop;

      declare
         Used : array (Rational_Degree_Slot) of Boolean := [others => False];
      begin
         --  Classify nearly real roots directly and pair every remaining root with its numerical conjugate. Average
         --  each pair to restore an exactly real quadratic representation.
         for Index in 1 .. Denominator_Degree loop
            if not Used (Index) then
               declare
                  Scale : constant Dimensionless := Root_Scale (Roots (Index));
               begin
                  if Dimensionless (abs Im (Roots (Index))) <= 1.0E-11 * Scale then
                     Real_Pole_Count := Real_Pole_Count + 1;
                     Real_Poles (Real_Pole_Count) := Dimensionless (Re (Roots (Index)));
                     Used (Index) := True;
                  else
                     declare
                        Target        : constant Complex :=
                          Compose_From_Cartesian (Re (Roots (Index)), -Im (Roots (Index)));
                        Best          : Natural := 0;
                        Best_Distance : Dimensionless := Dimensionless'Last;
                     begin
                        for Other in 1 .. Denominator_Degree loop
                           if Other /= Index
                             and then not Used (Other)
                             and then Dimensionless (abs (Roots (Other) - Target)) < Best_Distance
                           then
                              Best := Other;
                              Best_Distance := Dimensionless (abs (Roots (Other) - Target));
                           end if;
                        end loop;
                        if Best = 0
                          or else Best_Distance > 1.0E-7 * Scale
                          or else Pair_Count = Maximum_Rational_Degree / 2
                        then
                           return Realtime_Representation_Insufficient;
                        end if;
                        declare
                           Positive_Root  : constant Complex :=
                             (if Im (Roots (Index)) > 0.0 then Roots (Index) else Roots (Best));
                           Negative_Root  : constant Complex :=
                             (if Im (Roots (Index)) > 0.0 then Roots (Best) else Roots (Index));
                           Real_Part      : constant Dimensionless :=
                             Dimensionless (0.5 * (Re (Positive_Root) + Re (Negative_Root)));
                           Imaginary_Part : constant Dimensionless :=
                             Dimensionless (0.5 * (Im (Positive_Root) - Im (Negative_Root)));
                        begin
                           if Imaginary_Part <= 1.0E-14 * Scale then
                              return Realtime_Representation_Insufficient;
                           end if;
                           Pair_Count := Pair_Count + 1;
                           Pair_Poles (Pair_Count) := Compose_From_Cartesian (Real_Part, Imaginary_Part);
                           Used (Index) := True;
                           Used (Best) := True;
                        end;
                     end;
                  end if;
               end;
            end if;
         end loop;
      end;

      if Real_Pole_Count + 2 * Pair_Count /= Denominator_Degree then
         return Realtime_Representation_Insufficient;
      end if;
      Cache.Real_Pole_Count := Real_Pole_Count;
      Cache.Pair_Count := Pair_Count;
      for Index in 1 .. Real_Pole_Count loop
         Cache.Pole_Slots (Index).Pole_Component := Real_Poles (Index);
      end loop;
      for Pair in 1 .. Pair_Count loop
         declare
            Real_Slot      : constant Rational_Degree_Slot :=
              Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair));
            Imaginary_Slot : constant Rational_Degree_Slot :=
              Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair));
         begin
            Cache.Pole_Slots (Real_Slot).Pole_Component := Dimensionless (Re (Pair_Poles (Pair)));
            Cache.Pole_Slots (Imaginary_Slot).Pole_Component := Dimensionless (Im (Pair_Poles (Pair)));
         end;
      end loop;

      for Axis in Axis_Name loop
         declare
            Remainder : Real_Polynomial := Numerators (Axis);
            Quotient  : Dimensionless;
         begin
            --  Numerator and denominator have equal degree, so polynomial division leaves one constant quotient.
            --  For each simple pole p, the partial-fraction residue is
            --
            --     r = Remainder(p)/D′(p).
            if Denominator_Degree = 0 then
               Quotient := Remainder (0) / Denominator (0);
               Remainder (0) := 0.0;
            else
               Quotient := Remainder (Denominator_Degree) / Denominator (Denominator_Degree);
               for Degree in 0 .. Denominator_Degree loop
                  Remainder (Degree) := Remainder (Degree) - Quotient * Denominator (Degree);
               end loop;
            end if;
            Cache.Constant_Tangent (Axis) := Quotient;

            for Index in 1 .. Real_Pole_Count loop
               declare
                  Root       : constant Complex := Compose_From_Cartesian (Real_Poles (Index), 0.0);
                  Derivative : constant Complex :=
                    Evaluate_Polynomial (Denominator_Derivative, Denominator_Degree - 1, Root);
                  Residue    : Complex;
               begin
                  if Dimensionless (abs Derivative) <= 64.0 * Dimensionless'Model_Epsilon then
                     return Realtime_Representation_Insufficient;
                  end if;
                  Residue := Evaluate_Polynomial (Remainder, Denominator_Degree - 1, Root) / Derivative;
                  if not Complex_Is_Finite (Residue) then
                     return Realtime_Representation_Insufficient;
                  end if;
                  Cache.Pole_Slots (Index).Residue_Component (Axis) := Dimensionless (Re (Residue));
               end;
            end loop;
            for Pair in 1 .. Pair_Count loop
               declare
                  Real_Slot      : constant Rational_Degree_Slot :=
                    Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair));
                  Imaginary_Slot : constant Rational_Degree_Slot :=
                    Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair));
                  Root           : constant Complex := Pair_Poles (Pair);
                  Derivative     : constant Complex :=
                    Evaluate_Polynomial (Denominator_Derivative, Denominator_Degree - 1, Root);
                  Residue        : Complex;
               begin
                  if Dimensionless (abs Derivative) <= 64.0 * Dimensionless'Model_Epsilon then
                     return Realtime_Representation_Insufficient;
                  end if;
                  Residue := Evaluate_Polynomial (Remainder, Denominator_Degree - 1, Root) / Derivative;
                  if not Complex_Is_Finite (Residue) then
                     return Realtime_Representation_Insufficient;
                  end if;
                  Cache.Pole_Slots (Real_Slot).Residue_Component (Axis) := Dimensionless (Re (Residue));
                  Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Axis) := Dimensionless (Im (Residue));
               end;
            end loop;
         end;
      end loop;

      for Axis in Axis_Name loop
         if Curve.Structurally_Constant_Axes (Axis) then
            Cache.Constant_Tangent (Axis) := 0.0;
            for Slot in Rational_Degree_Slot loop
               Cache.Pole_Slots (Slot).Residue_Component (Axis) := 0.0;
            end loop;
         end if;
      end loop;
      if not Rational_Antiderivative_Is_Well_Formed (Cache) then
         return Realtime_Representation_Insufficient;
      end if;

      declare
         Warp            : constant Dimensionless := Curve.Warp_Factor;
         Warp_Difference : constant Dimensionless := Warp - 1.0;
      begin
         --  Convert the V-space partial fractions to physical U using V = W·U/(1 + (W - 1)·U). For a pole p and
         --  residue r,
         --
         --     pᵤ = p/(W - (W - 1)·p),
         --     rᵤ = W·r/(W - (W - 1)·p)²,
         --
         --  plus a constant tangent contribution (W - 1)·r/(W - (W - 1)·p).
         for Index in 1 .. Cache.Real_Pole_Count loop
            declare
               Pole     : constant Dimensionless := Cache.Pole_Slots (Index).Pole_Component;
               Scale    : constant Dimensionless := Warp - Warp_Difference * Pole;
               New_Pole : constant Dimensionless := Pole / Scale;
            begin
               if Scale = 0.0 or else not Is_Finite (Scale) or else not Is_Finite (New_Pole) then
                  return Realtime_Compilation_Numerically_Unsafe;
               end if;
               for Axis in Axis_Name loop
                  declare
                     Residue           : constant Dimensionless := Cache.Pole_Slots (Index).Residue_Component (Axis);
                     Constant_Addition : constant Dimensionless := Warp_Difference * Residue / Scale;
                     New_Residue       : constant Dimensionless := Warp * Residue / (Scale * Scale);
                  begin
                     Cache.Constant_Tangent (Axis) := Cache.Constant_Tangent (Axis) + Constant_Addition;
                     Cache.Pole_Slots (Index).Residue_Component (Axis) := New_Residue;
                  end;
               end loop;
               Cache.Pole_Slots (Index).Pole_Component := New_Pole;
            end;
         end loop;

         for Pair in 1 .. Cache.Pair_Count loop
            declare
               Real_Slot      : constant Rational_Degree_Slot :=
                 Complex_Pair_Real_Slot (Cache, Rational_Pair_Index (Pair));
               Imaginary_Slot : constant Rational_Degree_Slot :=
                 Complex_Pair_Imaginary_Slot (Cache, Rational_Pair_Index (Pair));
               Pole           : constant Complex :=
                 Compose_From_Cartesian
                   (Cache.Pole_Slots (Real_Slot).Pole_Component, Cache.Pole_Slots (Imaginary_Slot).Pole_Component);
               Scale          : constant Complex :=
                 Compose_From_Cartesian (Warp, 0.0) - Compose_From_Cartesian (Warp_Difference, 0.0) * Pole;
               New_Pole       : constant Complex := Pole / Scale;
            begin
               if not Complex_Is_Finite (Scale)
                 or else abs Scale = 0.0
                 or else not Complex_Is_Finite (New_Pole)
                 or else Im (New_Pole) <= 0.0
               then
                  return Realtime_Compilation_Numerically_Unsafe;
               end if;
               for Axis in Axis_Name loop
                  declare
                     Residue           : constant Complex :=
                       Compose_From_Cartesian
                         (Cache.Pole_Slots (Real_Slot).Residue_Component (Axis),
                          Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Axis));
                     Constant_Addition : constant Complex :=
                       Compose_From_Cartesian (Warp_Difference, 0.0) * Residue / Scale;
                     New_Residue       : constant Complex :=
                       Compose_From_Cartesian (Warp, 0.0) * Residue / (Scale * Scale);
                  begin
                     if not Complex_Is_Finite (New_Residue) or else not Complex_Is_Finite (Constant_Addition) then
                        return Realtime_Compilation_Numerically_Unsafe;
                     end if;
                     Cache.Constant_Tangent (Axis) :=
                       Cache.Constant_Tangent (Axis) + 2.0 * Dimensionless (Re (Constant_Addition));
                     Cache.Pole_Slots (Real_Slot).Residue_Component (Axis) := Dimensionless (Re (New_Residue));
                     Cache.Pole_Slots (Imaginary_Slot).Residue_Component (Axis) := Dimensionless (Im (New_Residue));
                  end;
               end loop;
               Cache.Pole_Slots (Real_Slot).Pole_Component := Dimensionless (Re (New_Pole));
               Cache.Pole_Slots (Imaginary_Slot).Pole_Component := Dimensionless (Im (New_Pole));
            end;
         end loop;

         if not Rational_Antiderivative_Is_Well_Formed (Cache) then
            return Realtime_Representation_Insufficient;
         end if;
         declare
            Retained_Certificate : Retained_Tangent_Bernstein_Certificate;
            Certificate          : constant Tangent_Certificate_Result :=
              Certified_Realtime_Tangent_Error
                (Curve, Cache, Denominator (0), Maximum_Position_Error, Retained_Certificate);
         begin
            --  Do not install the realtime cache until interval arithmetic proves its tangent field close enough to
            --  the authoritative Bernstein chart over the complete curve.
            case Certificate.Status is
               when Tangent_Could_Not_Be_Certified            =>
                  return Realtime_Representation_Insufficient;

               when Tangent_Certificate_Is_Numerically_Unsafe =>
                  return Realtime_Compilation_Numerically_Unsafe;

               when Tangent_Was_Certified                     =>
                  declare
                     Physical_Error : constant Length :=
                       Up (Dimensionless (Curve.Evaluator_Data.Length_Value / mm) * Certificate.Error_Bound) * mm;
                  begin
                     if not Is_Finite (Dimensionless (Physical_Error / mm)) then
                        return Realtime_Compilation_Numerically_Unsafe;
                     elsif Physical_Error > Maximum_Position_Error then
                        return Realtime_Representation_Insufficient;
                     end if;
                     Curve.Evaluator_Data :=
                       (Kind                     => Positive_Curve_Kind,
                        Start_Point              => Curve.Evaluator_Data.Start_Point,
                        Finish_Point             => Curve.Evaluator_Data.Finish_Point,
                        Length_Value             => Curve.Evaluator_Data.Length_Value,
                        Antiderivative_Cache     => Cache,
                        Uncorrected_Finish_Point => [others => 0.0 * mm]);
                     Curve.Evaluator_Data.Uncorrected_Finish_Point :=
                       Evaluate_Uncorrected_Rational_Point (Curve.Evaluator_Data, 1.0);
                     --  The retained point formula is continuously anchored at both endpoints. Keep the exact check
                     --  as a construction invariant, but do not require a rounded scalar correction to close a
                     --  sub-ULP cancellation gap.
                     if Evaluate_Uncorrected_Rational_Point (Curve.Evaluator_Data, 1.0)
                       /= Curve.Evaluator_Data.Uncorrected_Finish_Point
                       or else Evaluate_Rational_Point (Curve.Evaluator_Data, 0.0) /= Curve.Evaluator_Data.Start_Point
                       or else Evaluate_Rational_Point (Curve.Evaluator_Data, 1.0) /= Curve.Evaluator_Data.Finish_Point
                     then
                        return Realtime_Representation_Insufficient;
                     end if;
                     declare
                        Endpoint_Jet_Error : Endpoint_Jet_Error_Bounds;
                     begin
                        if Measure_Evaluator_Endpoint_Jet_Error
                             (Curve.Evaluator_Data, Start_Jet, Finish_Jet, Endpoint_Jet_Error)
                        then
                           Curve.Certified_Endpoint_Jet_Error := Endpoint_Jet_Error;
                        else
                           --  Endpoint diagnostics must not veto a cache already accepted by the uniform retained
                           --  tangent and position certificates. Saturate the optional per-order report instead;
                           --  callers can distinguish this explicit unavailable marker from every finite enclosure.
                           Curve.Certified_Endpoint_Jet_Error := [others => Dimensionless'Last];
                        end if;
                     end;
                     Curve.Certified_Position_Error := Physical_Error;
                     Curve.Certified_Tangent_Error := Certificate.Error_Bound;
                     --  Keep the construction-only common-denominator proof beside the installed cache. Later range
                     --  queries use it for direct derivative bounds; To_Evaluator intentionally discards it because
                     --  realtime point evaluation needs only the compact pole/residue representation.
                     Curve.Retained_Tangent_Certificate := Retained_Certificate;
                     return Realtime_Compilation_Succeeded;
                  end;
            end case;
         end;
      end;
   exception
      when Constraint_Error =>
         Curve.Evaluator_Data.Antiderivative_Cache := (others => <>);
         return Realtime_Compilation_Numerically_Unsafe;
   end Build_Rational_Representation;
   ----------------------------------------------------------------------------------------------------------------
   --  Candidate construction and public API
   ----------------------------------------------------------------------------------------------------------------

   function Chord_Is_Numerically_In_Endpoint_Plane
     (Request : Blend_Request; Chord_Direction, Start_Tangent, Finish_Tangent : Position_Scale) return Boolean
   is
      --  The Gram determinant is the conditioning measure for resolving the chord in the two tangent directions.
      --  Nearly parallel tangents do not define a stable plane basis and must use the general closure path.
      G           : constant Dimensionless := Dot (Start_Tangent, Finish_Tangent);
      Denominator : constant Dimensionless := 1.0 - G * G;

      --  Return an outward-rounded half-width of the rounding cell represented by a stored coordinate. Both adjacent
      --  spacings are considered because floating-point bins can have different widths on either side of a value.
      function Half_ULP (Value : Length) return Dimensionless;
      --  Return half the spacing of adjacent representable values at Value, expressed in millimetres.

      function Half_ULP (Value : Length) return Dimensionless is
         Previous  : constant Length := Length'Adjacent (Value, Length'First);
         Following : constant Length := Length'Adjacent (Value, Length'Last);
         Spacing   : constant Dimensionless :=
           Dimensionless'Max
             (abs Dimensionless ((Value - Previous) / mm), abs Dimensionless ((Following - Value) / mm));
         Half      : constant Dimensionless := 0.5 * Spacing;
      begin
         --  Preserve a nonzero outward allowance at the subnormal boundary.
         return (if Half = 0.0 and then Spacing > 0.0 then Spacing else Up (Half));
      end Half_ULP;
   begin
      if Denominator <= 1.0E-10 then
         return False;
      end if;

      declare
         Start_Dual           : constant Position_Scale := (Start_Tangent - Finish_Tangent * G) / Denominator;
         Finish_Dual          : constant Position_Scale := (Finish_Tangent - Start_Tangent * G) / Denominator;
         Incoming             : constant Dimensionless := Dot (Chord_Direction, Start_Dual);
         Outgoing             : constant Dimensionless := Dot (Chord_Direction, Finish_Dual);
         Scale                : constant Dimensionless :=
           Dimensionless'Max (1.0, Dimensionless'Max (abs Incoming, abs Outgoing));
         Residual             : constant Position_Scale :=
           Chord_Direction - Start_Tangent * Incoming - Finish_Tangent * Outgoing;
         Residual_OK          : Boolean;
         Residual_Norm        : constant Dimensionless := Safe_Norm (Residual, Residual_OK);
         Arithmetic_Tolerance : constant Dimensionless := 4_096.0 * Dimensionless'Model_Epsilon * Scale;
      begin
         if not Residual_OK then
            return False;
         elsif Residual_Norm <= Arithmetic_Tolerance then
            --  Preserve every seed admitted by the original normalized-arithmetic allowance.
            return True;
         end if;

         declare
            Chord             : constant Position_Offset := Request.Finish.Point - Request.Start.Point;
            Chord_Raw         : constant Position_Scale :=
              [for Axis in Axis_Name => Dimensionless (Chord (Axis) / mm)];
            Raw_Incoming      : constant Dimensionless := Dot (Chord_Raw, Start_Dual);
            Raw_Outgoing      : constant Dimensionless := Dot (Chord_Raw, Finish_Dual);
            Raw_Residual      : constant Position_Scale :=
              Chord_Raw - Start_Tangent * Raw_Incoming - Finish_Tangent * Raw_Outgoing;
            Chord_Uncertainty : Position_Scale;
            Arithmetic_Radius : constant Dimensionless :=
              Up
                (Arithmetic_Tolerance
                 * Dimensionless'Max
                     (abs Dimensionless (Chord (X_Axis) / mm),
                      Dimensionless'Max
                        (abs Dimensionless (Chord (Y_Axis) / mm),
                         Dimensionless'Max
                           (abs Dimensionless (Chord (Z_Axis) / mm), abs Dimensionless (Chord (E_Axis) / mm)))));
         begin
            --  Treat each stored endpoint as the centre of its floating-point rounding cell. Project that physical
            --  per-axis uncertainty through the same orthogonal complement used to measure the endpoint-plane
            --  residual. This admits accumulated-coordinate cancellation independently of chord length without
            --  allowing the ULP of an unrelated axis to excuse a real normal displacement.
            for Axis in Axis_Name loop
               Chord_Uncertainty (Axis) :=
                 Up
                   (Up (Half_ULP (Request.Start.Point (Axis)) + Half_ULP (Request.Finish.Point (Axis)))
                    + Half_ULP (Chord (Axis)));
            end loop;

            for Output_Axis in Axis_Name loop
               declare
                  Projected_Uncertainty : Dimensionless := 0.0;
               begin
                  for Input_Axis in Axis_Name loop
                     declare
                        Identity  : constant Dimensionless := (if Output_Axis = Input_Axis then 1.0 else 0.0);
                        Projector : constant Dimensionless :=
                          Identity - Start_Tangent (Output_Axis) * Start_Dual (Input_Axis)
                          - Finish_Tangent (Output_Axis) * Finish_Dual (Input_Axis);
                     begin
                        Projected_Uncertainty :=
                          Up (Projected_Uncertainty + Up (abs Projector * Chord_Uncertainty (Input_Axis)));
                     end;
                  end loop;
                  if abs Raw_Residual (Output_Axis) > Up (Projected_Uncertainty + Arithmetic_Radius) then
                     return False;
                  end if;
               end;
            end loop;
            return True;
         end;
      end;
   exception
      when Constraint_Error =>
         return False;
   end Chord_Is_Numerically_In_Endpoint_Plane;

   function Realtime_Point_Roundoff_Bound (Evaluator : Stereographic_Curve_Evaluator) return Length;
   --  Bound floating-point roundoff in public point evaluation directly from the executable evaluator.

   function Realtime_Point_Roundoff_Bound (Evaluator : Stereographic_Curve_Evaluator) return Length is
      Axis_Errors : Dimensionless_Axis_Vector := [others => 0.0];
      Scale       : Dimensionless := 0.0;
      Sum         : Dimensionless := 0.0;
   begin
      --  The rational certificate is expressed as normalized displacement from Start_Point. Include the
      --  transcendental primitive work, dimensional multiplication, and translated coordinate addition which public
      --  evaluation performs afterward.
      for Axis in Axis_Name loop
         declare
            Normalized_Work : Dimensionless :=
              Dimensionless'Max (1.0, abs Evaluator.Antiderivative_Cache.Constant_Tangent (Axis));
            Coordinate_Work : Dimensionless;
         begin
            for Index in 1 .. Evaluator.Antiderivative_Cache.Real_Pole_Count loop
               declare
                  Slot      : Rational_Pole_Slot renames Evaluator.Antiderivative_Cache.Pole_Slots (Index);
                  Primitive : constant Dimensionless := abs Stable_Real_Log_Ratio (Slot.Pole_Component, 1.0);
               begin
                  Normalized_Work := Normalized_Work + Primitive * abs Slot.Residue_Component (Axis);
               end;
            end loop;
            for Pair in 1 .. Evaluator.Antiderivative_Cache.Pair_Count loop
               declare
                  Real_Slot      : Rational_Pole_Slot renames
                    Evaluator.Antiderivative_Cache.Pole_Slots
                      (Complex_Pair_Real_Slot (Evaluator.Antiderivative_Cache, Rational_Pair_Index (Pair)));
                  Imaginary_Slot : Rational_Pole_Slot renames
                    Evaluator.Antiderivative_Cache.Pole_Slots
                      (Complex_Pair_Imaginary_Slot (Evaluator.Antiderivative_Cache, Rational_Pair_Index (Pair)));
                  A              : constant Dimensionless := Real_Slot.Pole_Component;
                  B              : constant Dimensionless := Imaginary_Slot.Pole_Component;
                  Closest        : constant Dimensionless := Dimensionless'Max (0.0, Dimensionless'Min (1.0, A));
                  Closest_Log    : constant Dimensionless := Stable_Complex_Log_Ratio (A, B, Closest);
                  Finish_Log     : constant Dimensionless := Stable_Complex_Log_Ratio (A, B, 1.0);
                  Log_Work       : constant Dimensionless := Dimensionless'Max (abs Closest_Log, abs Finish_Log);
               begin
                  if not Is_Finite (Closest_Log) or else not Is_Finite (Finish_Log) then
                     return Length'Last;
                  end if;
                  Normalized_Work :=
                    Normalized_Work + Log_Work * abs Real_Slot.Residue_Component (Axis)
                    + 2.0 * Dimensionless (Ada.Numerics.Pi) * abs Imaginary_Slot.Residue_Component (Axis);
               end;
            end loop;
            Coordinate_Work :=
              Dimensionless'Max
                (1.0,
                 Dimensionless'Max
                   (abs Dimensionless (Evaluator.Start_Point (Axis) / mm),
                    abs Dimensionless (Evaluator.Finish_Point (Axis) / mm)));
            Coordinate_Work :=
              Dimensionless'Max
                (Coordinate_Work, abs Dimensionless (Evaluator.Length_Value / mm) * (1.0 + Normalized_Work));
            Coordinate_Work :=
              Dimensionless'Max (Coordinate_Work, abs Dimensionless (Evaluator.Uncorrected_Finish_Point (Axis) / mm));
            Axis_Errors (Axis) := Up (16_384.0 * Dimensionless'Model_Epsilon * Coordinate_Work);
            if not Is_Finite (Axis_Errors (Axis)) then
               return Length'Last;
            end if;
            Scale := Dimensionless'Max (Scale, Axis_Errors (Axis));
         end;
      end loop;
      if Scale = 0.0 then
         return 0.0 * mm;
      end if;
      --  Combine per-axis error bounds with a scaled Euclidean norm to avoid squaring large absolute coordinates.
      for Axis in Axis_Name loop
         Sum := Up (Sum + (Axis_Errors (Axis) / Scale) ** 2);
      end loop;
      if not Is_Finite (Sum) then
         return Length'Last;
      end if;
      declare
         Unit_Norm_Bound : constant Dimensionless := Certified_Upper_Square_Root (Sum);
      begin
         if Unit_Norm_Bound >= Dimensionless'Last or else Scale > Dimensionless'Last / Unit_Norm_Bound then
            return Length'Last;
         end if;
         return Up (Scale * Unit_Norm_Bound) * mm;
      end;
   exception
      when Constraint_Error =>
         return Length'Last;
   end Realtime_Point_Roundoff_Bound;

   function Build_Candidate
     (Request                             : Blend_Request;
      Chord_Length                        : Length;
      Chord_Direction                     : Position_Scale;
      Frame                               : Frame_Vector_Array;
      Start_Chart_Jets, Finish_Chart_Jets : Chart_Jet_Array;
      Warp_Factor                         : Dimensionless;
      Initial_Closure                     : Closure_Seed) return Candidate_Result
   is
      Maximum_Lambda   : constant Dimensionless := Dimensionless (Request.Maximum_Arc_Length / Chord_Length);
      Epsilon          : constant Dimensionless := Dimensionless (Request.Maximum_Position_Error / Chord_Length);
      Coarse_Tolerance : constant Dimensionless :=
        Dimensionless'Max (Closure_Absolute_Floor, Dimensionless'Min (1.0E-8, Epsilon / 2_048.0));
      Fine_Tolerance   : constant Dimensionless :=
        Dimensionless'Max (Closure_Absolute_Floor, Dimensionless'Min (1.0E-11, Epsilon / 4_096.0));

      --  Closure is solved in chord-normalized coordinates. Tie both Newton tolerances to the available normalized
      --  position budget ε = Maximum_Position_Error/Chord_Length, while retaining an arithmetic noise floor.
      First_Coarse_Solution : constant Closure_Solution :=
        (if Initial_Closure.Valid
         then
           --  Select_Distance_Warp integrates the endpoint-flat chart in its natural coordinate, where even a strong
           --  Möbius map remains well resolved. Re-solving that proposal with a coarse grid in U can move a good
           --  asymmetric solution away from closure.
           (Success => True, Numerically_Unsafe => False, Lambda => Initial_Closure.Lambda, C0 => Initial_Closure.C0)
         else
           Solve_Closure_Fixed
             (Frame,
              Start_Chart_Jets,
              Finish_Chart_Jets,
              Chord_Direction,
              Maximum_Lambda,
              Coarse_Tolerance,
              Warp_Factor,
              Initial     => (others => <>),
              Panel_Count => 1));

      Coarse_Solution : constant Closure_Solution := First_Coarse_Solution;
      Solution        : constant Closure_Solution :=
        (if Initial_Closure.Valid
         then
           --  The final global tangent certificate and continuous endpoint correction still verify the proposal. If
           --  either fails, the candidate fails; there is no unwarped or unseeded fallback.
           Coarse_Solution
         elsif Coarse_Solution.Success
         then
           Solve_Closure_Fixed
             (Frame,
              Start_Chart_Jets,
              Finish_Chart_Jets,
              Chord_Direction,
              Maximum_Lambda,
              Fine_Tolerance,
              Warp_Factor,
              Initial     => Coarse_Solution,
              Panel_Count => 2)
         else Coarse_Solution);

   begin
      if not Solution.Success then
         if Solution.Numerically_Unsafe then
            return (Status => Candidate_Numerically_Unsafe);
         else
            return (Status => Candidate_Closure_Failed);
         end if;
      end if;

      declare
         Coefficients     : Bernstein_Chart;
         Effective_C0     : Raw_Vector_3 := Solution.C0;
         --  λ = Arc_Length/Chord_Length. Clamp the solved value back into its analytic and requested bounds before
         --  rebuilding the authoritative chart.
         Length_Value     : constant Length :=
           Length'Max (Chord_Length, Length'Min (Request.Maximum_Arc_Length, Solution.Lambda * Chord_Length));
         Effective_Lambda : constant Dimensionless := Dimensionless (Length_Value / Chord_Length);
      begin
         Build_Chart (Start_Chart_Jets, Finish_Chart_Jets, Effective_Lambda, Effective_C0, Warp_Factor, Coefficients);

         --  The no-bulge certificates treat the last two chart coordinates as normal components. For flat endpoint
         --  jets and a chord in the endpoint-tangent plane up to stored-coordinate resolution, cancellation can leave
         --  roundoff-sized residues there, and the closure solve can amplify them into a spurious normal mode.
         --  Canonicalize the structurally planar result before deriving any runtime representation. A no-bulge
         --  request may remove a normal closure mode which is forbidden by the request; an allow-bulge request removes
         --  it only when every coefficient proves that it is numerical noise. The representation and continuous
         --  endpoint-correction certificates below still gate the resulting production curve.
         declare
            Endpoint_Derivatives_Are_Flat : constant Boolean :=
              (for all K in 1 .. Start_Chart_Jets'Last =>
                 (for all C in Chart_Component_Index =>
                    Start_Chart_Jets (K) (C) = 0.0 and then Finish_Chart_Jets (K) (C) = 0.0));
            Start_Tangent                 : constant Position_Scale :=
              Frame_Vector (Frame, Stereographic (Start_Chart_Jets (0)));
            Finish_Tangent                : constant Position_Scale :=
              Frame_Vector (Frame, Stereographic (Finish_Chart_Jets (0)));
            Chord_Is_Structurally_Planar  : constant Boolean :=
              Chord_Is_Numerically_In_Endpoint_Plane (Request, Chord_Direction, Start_Tangent, Finish_Tangent);
         begin
            if Endpoint_Derivatives_Are_Flat then
               for C in 2 .. 3 loop
                  if abs Start_Chart_Jets (0) (C) <= Frame_Residual_Tolerance
                    and then abs Finish_Chart_Jets (0) (C) <= Frame_Residual_Tolerance
                    and then
                      (abs Frame_Coordinate (Frame, C, Chord_Direction) <= Frame_Residual_Tolerance
                       or else Chord_Is_Structurally_Planar)
                    and then
                      (not Request.Allow_Bulge
                       or else
                         (for all K in Chart_Coefficient_Index => abs Coefficients (K, C) <= Frame_Residual_Tolerance))
                  then
                     for K in Chart_Coefficient_Index loop
                        Coefficients (K, C) := 0.0;
                     end loop;
                     Effective_C0 (C) := 0.0;
                  end if;
               end loop;
            end if;
         end;

         declare
            Rational_Power_Coefficients : Power_Chart := Power_Basis (Coefficients);
            Frame_Speed_Upper           : constant Dimensionless := Frame_Speed_Upper_Bound (Frame);
            Curve                       : Stereographic_Curve :=
              (Evaluator_Data               =>
                 (Kind                     => Positive_Curve_Kind,
                  Start_Point              => Request.Start.Point,
                  Finish_Point             => Request.Finish.Point,
                  Length_Value             => Length_Value,
                  Antiderivative_Cache     => (others => <>),
                  Uncorrected_Finish_Point => [others => 0.0 * mm]),
               Frame                        => Frame,
               Coefficients                 => Coefficients,
               Warp_Factor                  => Distance_Warp_Factor (Warp_Factor),
               Certified_Frame_Speed_Upper  => Frame_Speed_Upper,
               Certified_Position_Error     => 0.0 * mm,
               Certified_Tangent_Error      => 0.0,
               Certified_Endpoint_Jet_Error => [others => 0.0],
               Retained_Tangent_Certificate => (others => <>),
               Whole_Curve_Majorants        => [others => [others => 0.0]],
               Has_Whole_Curve_Majorants    => False,
               Structurally_Constant_Axes   => [others => False],
               Bounds                       => (others => <>));
            Correction_Error_Raw        : Dimensionless := Dimensionless'Last;
         begin
            if Frame_Speed_Upper <= 0.0
              or else Frame_Speed_Upper >= Dimensionless'Last
              or else not Is_Finite (Frame_Speed_Upper)
            then
               return (Status => Candidate_Numerically_Unsafe);
            end if;
            for Component in Chart_Component_Index loop
               declare
                  Correction     : constant Dimensionless :=
                    256.0 * Effective_C0 (Component) - Rational_Power_Coefficients (Fixed_Chart_Degree, Component);
                  Envelope_Power : constant array (Natural range 4 .. 8) of Dimensionless :=
                    [1.0, -4.0, 6.0, -4.0, 1.0];
               begin
                  --  Repair the endpoint-invisible closure mode as the complete polynomial V⁴·(1 - V)⁴. Altering
                  --  only the V⁸ coefficient would change the finish tangent jet seen by the rational compiler.
                  for Degree in Envelope_Power'Range loop
                     Rational_Power_Coefficients (Degree, Component) :=
                       Rational_Power_Coefficients (Degree, Component) + Correction * Envelope_Power (Degree);
                  end loop;
               end;
            end loop;

            declare
               type Interval_Bernstein is array (Natural range <>) of Interval;

               function Strict_Bernstein_Nonnegative
                 (Coefficients : Interval_Bernstein; Depth : Natural) return Boolean;
               function Certify_Planar_Control_Hull (Applicable : out Boolean) return Boolean;
               function Certify_Planar_No_Bulge (Applicable : out Boolean) return Boolean;
               function Exact_Dot_Interval (Left, Right : Position_Scale) return Interval;
               function Certify_Nonnegative_Projection (Direction : Interval_Position_Scale) return Boolean;
               function Certify_Nonnegative_Projection (Direction : Position_Scale) return Boolean;

               function Exact_Dot_Interval (Left, Right : Position_Scale) return Interval is
                  Result : Interval := Interval_Exact (0.0);
               begin
                  for Axis in Axis_Name loop
                     Result :=
                       Interval_Add
                         (Result, Interval_Multiply (Interval_Exact (Left (Axis)), Interval_Exact (Right (Axis))));
                  end loop;
                  return Result;
               end Exact_Dot_Interval;

               function Strict_Bernstein_Nonnegative
                 (Coefficients : Interval_Bernstein; Depth : Natural) return Boolean
               is
                  All_Nonnegative : Boolean := True;
                  All_Negative    : Boolean := True;
               begin
                  --  A nonnegative lower bound on every control certifies p(U) ≥ 0 by the Bernstein convex-hull
                  --  property. If the full hull straddles zero, bisect with de Casteljau and retry each half.
                  for Coefficient of Coefficients loop
                     if not Coefficient.Valid then
                        return False;
                     end if;
                     All_Nonnegative := All_Nonnegative and then Coefficient.Lower >= 0.0;
                     All_Negative := All_Negative and then Coefficient.Upper < 0.0;
                  end loop;
                  if All_Nonnegative then
                     return True;
                  elsif All_Negative or else Depth = 0 or else Coefficients'Length = 1 then
                     return False;
                  end if;

                  declare
                     Degree : constant Natural := Coefficients'Length - 1;
                     Work   : Interval_Bernstein (0 .. Degree) := Coefficients;
                     Left   : Interval_Bernstein (0 .. Degree) := [others => Interval_Exact (0.0)];
                     Right  : Interval_Bernstein (0 .. Degree) := [others => Interval_Exact (0.0)];
                  begin
                     Left (0) := Work (0);
                     Right (Degree) := Work (Degree);
                     for Level in 1 .. Degree loop
                        for I in 0 .. Degree - Level loop
                           Work (I) := Interval_Multiply (Interval_Exact (0.5), Interval_Add (Work (I), Work (I + 1)));
                        end loop;
                        Left (Level) := Work (0);
                        Right (Degree - Level) := Work (Degree - Level);
                     end loop;
                     return
                       Strict_Bernstein_Nonnegative (Left, Depth - 1)
                       and then Strict_Bernstein_Nonnegative (Right, Depth - 1);
                  end;
               end Strict_Bernstein_Nonnegative;

               function Certify_Planar_Control_Hull (Applicable : out Boolean) return Boolean is
                  Start_Value  : constant Dimensionless := Curve.Coefficients (0, 1);
                  Finish_Value : constant Dimensionless := Curve.Coefficients (Fixed_Chart_Degree, 1);
                  Lower_Value  : constant Dimensionless := Dimensionless'Min (Start_Value, Finish_Value);
                  Upper_Value  : constant Dimensionless := Dimensionless'Max (Start_Value, Finish_Value);
                  Cross_Factor : constant Interval :=
                    Interval_Add
                      (Interval_Exact (1.0),
                       Interval_Multiply (Interval_Exact (Start_Value), Interval_Exact (Finish_Value)));
               begin
                  Applicable := False;
                  for C in 2 .. 3 loop
                     for I in Chart_Coefficient_Index loop
                        if Curve.Coefficients (I, C) /= 0.0 then
                           return False;
                        end if;
                     end loop;
                  end loop;
                  Applicable := True;

                  --  A scalar Bernstein polynomial lies in the convex hull of its controls. Keeping every control
                  --  between the two endpoint chart values therefore keeps the entire ideal tangent in their cone;
                  --  monotonicity in the parameter is unnecessary. The positive cross factor selects the minor
                  --  inverse-stereographic arc in one open hemisphere.
                  if Cross_Factor.Lower <= 0.0 then
                     return False;
                  end if;
                  for I in Chart_Coefficient_Index loop
                     if Curve.Coefficients (I, 1) < Lower_Value or else Curve.Coefficients (I, 1) > Upper_Value then
                        return False;
                     end if;
                  end loop;
                  return True;
               end Certify_Planar_Control_Hull;

               function Certify_Planar_No_Bulge (Applicable : out Boolean) return Boolean is
                  Start_Factor_Order          : constant Positive :=
                    (if (for all I in 1 .. 3 => Curve.Coefficients (I, 1) = Curve.Coefficients (0, 1)) then 4 else 1);
                  Finish_Factor_Order         : constant Positive :=
                    (if (for all I in 5 .. Fixed_Chart_Degree - 1 =>
                           Curve.Coefficients (I, 1) = Curve.Coefficients (Fixed_Chart_Degree, 1))
                     then 4
                     else 1);
                  Start_Quotient              : Interval_Bernstein (0 .. Fixed_Chart_Degree - Start_Factor_Order) :=
                    [others => Interval_Exact (0.0)];
                  Finish_Quotient             : Interval_Bernstein (0 .. Fixed_Chart_Degree - Finish_Factor_Order) :=
                    [others => Interval_Exact (0.0)];
                  Start_Factor, Finish_Factor : Interval_Bernstein (Chart_Coefficient_Index) :=
                    [others => Interval_Exact (0.0)];
                  Start_Product               :
                    Interval_Bernstein (0 .. 2 * Fixed_Chart_Degree - Start_Factor_Order) :=
                      [others => Interval_Exact (0.0)];
                  Finish_Product              :
                    Interval_Bernstein (0 .. 2 * Fixed_Chart_Degree - Finish_Factor_Order) :=
                      [others => Interval_Exact (0.0)];
                  Start_Value                 : constant Dimensionless := Curve.Coefficients (0, 1);
                  Finish_Value                : constant Dimensionless := Curve.Coefficients (Fixed_Chart_Degree, 1);

                  function Positive_Ratio (Numerator, Denominator : Dimensionless) return Interval;
                  --  Enclose a ratio whose numerator is nonnegative and denominator is positive.

                  function Positive_Ratio (Numerator, Denominator : Dimensionless) return Interval is
                     Value : Dimensionless;
                  begin
                     if Numerator < 0.0 or else Denominator <= 0.0 then
                        return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
                     end if;
                     Value := Numerator / Denominator;
                     return Checked_Interval (Down (Value), Up (Value));
                  exception
                     when Constraint_Error =>
                        return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
                  end Positive_Ratio;

                  procedure Build_Product
                    (Left : Interval_Bernstein; Right : Interval_Bernstein; Result : out Interval_Bernstein);

                  procedure Build_Product
                    (Left : Interval_Bernstein; Right : Interval_Bernstein; Result : out Interval_Bernstein)
                  is
                     Left_Degree    : constant Natural := Left'Length - 1;
                     Right_Degree   : constant Natural := Right'Length - 1;
                     Product_Degree : constant Natural := Left_Degree + Right_Degree;
                  begin
                     Result := [others => Interval_Exact (0.0)];
                     for I in Left'Range loop
                        for J in Right'Range loop
                           declare
                              K      : constant Natural := I + J;
                              Weight : constant Interval :=
                                Positive_Ratio
                                  (Binomial (Left_Degree, I) * Binomial (Right_Degree, J),
                                   Binomial (Product_Degree, K));
                           begin
                              Result (K) :=
                                Interval_Add
                                  (Result (K), Interval_Multiply (Weight, Interval_Multiply (Left (I), Right (J))));
                           end;
                        end loop;
                     end loop;
                  end Build_Product;

                  Orientation      : Interval;
                  Orientation_Sign : Dimensionless;
               begin
                  Applicable := False;
                  for C in 2 .. 3 loop
                     for I in Chart_Coefficient_Index loop
                        if Curve.Coefficients (I, C) /= 0.0 then
                           return False;
                        end if;
                     end loop;
                  end loop;
                  Applicable := True;

                  --  For the planar inverse-stereographic tangent
                  --
                  --     S(Y) = ((1 - Y²), 2Y)/(1 + Y²),
                  --
                  --  the two endpoint-cone determinants factor as
                  --
                  --     (Y - Yₛ)·(1 + Yₛ·Y),   (Yꜰ - Y)·(1 + Yꜰ·Y).
                  --
                  --  Every Bernstein chart has the exact endpoint factors Y - Yₛ = V·Qₛ and
                  --  Yꜰ - Y = (1 - V)·Qꜰ. Structurally flat endpoint controls expose fourth-order factors instead.
                  --  Form the strongest available quotient directly in Bernstein form, then certify the remaining
                  --  products with outward-rounded intervals.
                  Orientation :=
                    Interval_Multiply
                      (Interval_Subtract (Interval_Exact (Finish_Value), Interval_Exact (Start_Value)),
                       Interval_Add
                         (Interval_Exact (1.0),
                          Interval_Multiply (Interval_Exact (Start_Value), Interval_Exact (Finish_Value))));
                  if Orientation.Lower > 0.0 then
                     Orientation_Sign := 1.0;
                  elsif Orientation.Upper < 0.0 then
                     Orientation_Sign := -1.0;
                  else
                     return False;
                  end if;

                  for J in Start_Quotient'Range loop
                     Start_Quotient (J) :=
                       Interval_Multiply
                         (Interval_Subtract
                            (Interval_Exact (Curve.Coefficients (J + Start_Factor_Order, 1)),
                             Interval_Exact (Start_Value)),
                          Positive_Ratio
                            (Binomial (Fixed_Chart_Degree, J + Start_Factor_Order),
                             Binomial (Fixed_Chart_Degree - Start_Factor_Order, J)));
                  end loop;
                  for J in Finish_Quotient'Range loop
                     Finish_Quotient (J) :=
                       Interval_Multiply
                         (Interval_Subtract
                            (Interval_Exact (Finish_Value), Interval_Exact (Curve.Coefficients (J, 1))),
                          Positive_Ratio
                            (Binomial (Fixed_Chart_Degree, J),
                             Binomial (Fixed_Chart_Degree - Finish_Factor_Order, J)));
                  end loop;
                  for I in Chart_Coefficient_Index loop
                     Start_Factor (I) :=
                       Interval_Add
                         (Interval_Exact (1.0),
                          Interval_Multiply
                            (Interval_Exact (Start_Value), Interval_Exact (Curve.Coefficients (I, 1))));
                     Finish_Factor (I) :=
                       Interval_Add
                         (Interval_Exact (1.0),
                          Interval_Multiply
                            (Interval_Exact (Finish_Value), Interval_Exact (Curve.Coefficients (I, 1))));
                  end loop;
                  Build_Product (Start_Quotient, Start_Factor, Start_Product);
                  Build_Product (Finish_Quotient, Finish_Factor, Finish_Product);
                  for I in Start_Product'Range loop
                     Start_Product (I) := Interval_Multiply (Interval_Exact (Orientation_Sign), Start_Product (I));
                  end loop;
                  for I in Finish_Product'Range loop
                     Finish_Product (I) := Interval_Multiply (Interval_Exact (Orientation_Sign), Finish_Product (I));
                  end loop;
                  return
                    Strict_Bernstein_Nonnegative (Start_Product, 8)
                    and then Strict_Bernstein_Nonnegative (Finish_Product, 8);
               exception
                  when Constraint_Error =>
                     Applicable := True;
                     return False;
               end Certify_Planar_No_Bulge;

               function Certify_Nonnegative_Projection (Direction : Interval_Position_Scale) return Boolean is
                  Numerator : Interval_Bernstein (Tangent_Numerator_Index) := [others => Interval_Exact (0.0)];
                  R2        : Interval_Bernstein (Tangent_Numerator_Index) := [others => Interval_Exact (0.0)];
                  Ones      : constant Interval_Bernstein (Chart_Coefficient_Index) :=
                    [others => Interval_Exact (1.0)];

                  function Dot_Interval (Left : Position_Scale; Right : Interval_Position_Scale) return Interval;
                  --  Enclose the dot product of an exact vector and an interval vector.

                  function Dot_Interval (Left : Position_Scale; Right : Interval_Position_Scale) return Interval is
                     Result : Interval := Interval_Exact (0.0);
                  begin
                     for Axis in Axis_Name loop
                        Result :=
                          Interval_Add (Result, Interval_Multiply (Interval_Exact (Left (Axis)), Right (Axis)));
                     end loop;
                     return Result;
                  end Dot_Interval;

                  function Positive_Ratio (Numerator, Denominator : Dimensionless) return Interval;
                  --  Enclose a ratio whose numerator is nonnegative and denominator is positive.

                  function Positive_Ratio (Numerator, Denominator : Dimensionless) return Interval is
                     Value : Dimensionless;
                  begin
                     if Numerator < 0.0 or else Denominator <= 0.0 then
                        return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
                     end if;
                     Value := Numerator / Denominator;
                     return Checked_Interval (Down (Value), Up (Value));
                  exception
                     when Constraint_Error =>
                        return (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
                  end Positive_Ratio;

                  procedure Build_Product
                    (Left : Interval_Bernstein; Right : Interval_Bernstein; Result : out Interval_Bernstein);
                  --  Build the interval Bernstein product of Left and Right.

                  procedure Build_Product
                    (Left : Interval_Bernstein; Right : Interval_Bernstein; Result : out Interval_Bernstein)
                  is
                     Left_Degree    : constant Natural := Left'Length - 1;
                     Right_Degree   : constant Natural := Right'Length - 1;
                     Product_Degree : constant Natural := Left_Degree + Right_Degree;
                  begin
                     Result := [others => Interval_Exact (0.0)];
                     for I in Left'Range loop
                        for J in Right'Range loop
                           declare
                              K      : constant Natural := I + J;
                              Weight : constant Interval :=
                                Positive_Ratio
                                  (Binomial (Left_Degree, I) * Binomial (Right_Degree, J),
                                   Binomial (Product_Degree, K));
                           begin
                              Result (K) :=
                                Interval_Add
                                  (Result (K), Interval_Multiply (Weight, Interval_Multiply (Left (I), Right (J))));
                           end;
                        end loop;
                     end loop;
                  end Build_Product;
               begin
                  --  Form the inverse-stereographic tangent numerator
                  --
                  --     Frame₀·(1 - Y·Y) + 2·Σ꜀Frame꜀·Y꜀
                  --
                  --  directly from the authoritative degree-eight Bernstein chart. In particular, do not prove this
                  --  property using the ordinary-rounded power-basis evaluator copy: that would certify a nearby
                  --  polynomial rather than the ideal curve. Multiplication by the degree-eight constant-one
                  --  polynomial both elevates Y to degree sixteen and keeps every conversion outward-rounded.
                  for C in Chart_Component_Index loop
                     declare
                        Y       : constant Interval_Bernstein (Chart_Coefficient_Index) :=
                          [for I in Chart_Coefficient_Index => Interval_Exact (Curve.Coefficients (I, C))];
                        Squared : Interval_Bernstein (Tangent_Numerator_Index);
                     begin
                        Build_Product (Y, Y, Squared);
                        for K in Tangent_Numerator_Index loop
                           R2 (K) := Interval_Add (R2 (K), Squared (K));
                        end loop;
                     end;
                  end loop;
                  declare
                     Frame_0_Projection : constant Interval := Dot_Interval (Curve.Frame (0), Direction);
                  begin
                     for K in Tangent_Numerator_Index loop
                        Numerator (K) :=
                          Interval_Subtract (Frame_0_Projection, Interval_Multiply (Frame_0_Projection, R2 (K)));
                     end loop;
                  end;
                  for C in Chart_Component_Index loop
                     declare
                        Frame_Projection : constant Interval := Dot_Interval (Curve.Frame (C), Direction);
                        Y                : constant Interval_Bernstein (Chart_Coefficient_Index) :=
                          [for I in Chart_Coefficient_Index => Interval_Exact (Curve.Coefficients (I, C))];
                        Elevated         : Interval_Bernstein (Tangent_Numerator_Index);
                     begin
                        Build_Product (Y, Ones, Elevated);
                        for K in Tangent_Numerator_Index loop
                           Numerator (K) :=
                             Interval_Add
                               (Numerator (K),
                                Interval_Multiply
                                  (Interval_Exact (2.0), Interval_Multiply (Frame_Projection, Elevated (K))));
                        end loop;
                     end;
                  end loop;
                  return Strict_Bernstein_Nonnegative (Numerator, 8);
               end Certify_Nonnegative_Projection;

               function Certify_Nonnegative_Projection (Direction : Position_Scale) return Boolean is
               begin
                  return
                    Certify_Nonnegative_Projection
                      (Interval_Position_Scale'([for Axis in Axis_Name => Interval_Exact (Direction (Axis))]));
               end Certify_Nonnegative_Projection;

               function Certify_No_Bulge return Boolean;
               --  Certify that the curve remains inside the endpoint-tangent cone.

               function Certify_No_Bulge return Boolean is
                  Planar_Applicable : Boolean;
                  Planar_Certified  : Boolean := Certify_Planar_Control_Hull (Planar_Applicable);
               begin
                  if Planar_Applicable and then not Planar_Certified then
                     declare
                        Slow_Applicable : Boolean;
                     begin
                        Planar_Certified := Certify_Planar_No_Bulge (Slow_Applicable);
                        pragma Assert (Slow_Applicable);
                     end;
                  end if;
                  if Planar_Applicable then
                     return Planar_Certified;
                  end if;

                  declare
                     Start_Tangent  : constant Position_Scale :=
                       Frame_Vector (Frame, Stereographic (Start_Chart_Jets (0)));
                     Finish_Tangent : constant Position_Scale :=
                       Frame_Vector (Frame, Stereographic (Finish_Chart_Jets (0)));
                     Start_Square   : constant Interval := Exact_Dot_Interval (Start_Tangent, Start_Tangent);
                     Finish_Square  : constant Interval := Exact_Dot_Interval (Finish_Tangent, Finish_Tangent);
                     Cross          : constant Interval := Exact_Dot_Interval (Start_Tangent, Finish_Tangent);
                     Determinant    : constant Interval :=
                       Interval_Subtract
                         (Interval_Multiply (Start_Square, Finish_Square), Interval_Multiply (Cross, Cross));
                  begin
                     --  The Gram determinant
                     --
                     --     Δ = (s·s)·(f·f) - (s·f)²
                     --
                     --  must stay positive before the endpoint-tangent dual directions can be certified.
                     if not Determinant.Valid or else Determinant.Lower <= 1.0E-10 then
                        --  A single chord projection cannot certify a nearly collapsed endpoint cone. The scalar
                        --  planar proof above handles the normal line-line path; reject a genuinely non-planar
                        --  degenerate case rather than weaken no-bulge.
                        return False;
                     end if;
                     declare
                        Start_Dual_Numerator   : Interval_Position_Scale;
                        Finish_Dual_Numerator  : Interval_Position_Scale;
                        Out_Of_Plane_Tolerance : constant Dimensionless := 1.0E-10;
                     begin
                        for Axis in Axis_Name loop
                           Start_Dual_Numerator (Axis) :=
                             Interval_Subtract
                               (Interval_Multiply (Finish_Square, Interval_Exact (Start_Tangent (Axis))),
                                Interval_Multiply (Cross, Interval_Exact (Finish_Tangent (Axis))));
                           Finish_Dual_Numerator (Axis) :=
                             Interval_Subtract
                               (Interval_Multiply (Start_Square, Interval_Exact (Finish_Tangent (Axis))),
                                Interval_Multiply (Cross, Interval_Exact (Start_Tangent (Axis))));
                        end loop;
                        for C in 2 .. 3 loop
                           for K in Chart_Coefficient_Index loop
                              if abs Curve.Coefficients (K, C) > Out_Of_Plane_Tolerance then
                                 return False;
                              end if;
                           end loop;
                        end loop;
                        return
                          Certify_Nonnegative_Projection (Start_Dual_Numerator)
                          and then Certify_Nonnegative_Projection (Finish_Dual_Numerator);
                     end;
                  end;
               end Certify_No_Bulge;
            begin
               --  Allow_Bulge is a relaxation of the default geometry, not a switch to a disjoint acceptance rule.
               --  Retain a certified no-bulge candidate first; only a candidate which leaves the endpoint tangent
               --  cone needs the chord-monotonicity proof. This matters for obtuse asymmetric line corners, whose
               --  fixed endpoint tangent can point backwards along the chord even though the complete curve stays
               --  inside its tangent cone.
               if not Certify_No_Bulge
                 and then (not Request.Allow_Bulge or else not Certify_Nonnegative_Projection (Chord_Direction))
               then
                  return (Status => Candidate_Closure_Failed);
               end if;
            end;
            --  Only after ideal closure and geometry are certified is the compact rational evaluator compiled and
            --  compared against the authoritative chart.
            case Build_Rational_Representation
                   (Curve,
                    Rational_Power_Coefficients,
                    Request.Maximum_Position_Error,
                    Request.Start.Jet,
                    Request.Finish.Jet)
            is
               when Realtime_Compilation_Succeeded          =>
                  null;

               when Realtime_Representation_Insufficient    =>
                  return (Status => Candidate_Representation_Failed);

               when Realtime_Compilation_Numerically_Unsafe =>
                  return (Status => Candidate_Numerically_Unsafe);
            end case;

            declare
               Public_Roundoff            : constant Length := Realtime_Point_Roundoff_Bound (Curve.Evaluator_Data);
               Error_With_Public_Roundoff : constant Length :=
                 (if Public_Roundoff = Length'Last or else Curve.Certified_Position_Error = Length'Last
                  then Length'Last
                  else
                    Up (Dimensionless (Curve.Certified_Position_Error / mm) + Dimensionless (Public_Roundoff / mm))
                    * mm);
            begin
               --  The stored error budget is cumulative:
               --
               --     representation error + public evaluation roundoff + continuous endpoint correction.
               --
               --  Charge each term before exposing the curve.
               if Public_Roundoff = Length'Last
                 or else not Is_Finite (Dimensionless (Public_Roundoff / mm))
                 or else not Is_Finite (Dimensionless (Error_With_Public_Roundoff / mm))
               then
                  return (Status => Candidate_Numerically_Unsafe);
               elsif Error_With_Public_Roundoff > Request.Maximum_Position_Error then
                  return (Status => Candidate_Representation_Failed);
               end if;
               Curve.Certified_Position_Error := Error_With_Public_Roundoff;
            end;

            Correction_Error_Raw := Finish_Correction_Position_Error_Bound (Curve.Evaluator_Data);

            declare
               Total_Error : constant Length :=
                 (if Correction_Error_Raw = Dimensionless'Last or else Curve.Certified_Position_Error = Length'Last
                  then Length'Last
                  else Up (Dimensionless (Curve.Certified_Position_Error / mm) + Correction_Error_Raw) * mm);
            begin
               if Correction_Error_Raw = Dimensionless'Last
                 or else not Is_Finite (Correction_Error_Raw)
                 or else not Is_Finite (Dimensionless (Total_Error / mm))
               then
                  return (Status => Candidate_Numerically_Unsafe);
               elsif Total_Error > Request.Maximum_Position_Error then
                  --  Representation and public evaluator roundoff already passed their independent budget checks
                  --  above. Any remaining excess is the continuous endpoint correction, so this is a closure failure
                  --  rather than a representation failure.
                  return (Status => Candidate_Closure_Failed);
               end if;
               Curve.Certified_Position_Error := Total_Error;
            end;
            Curve.Whole_Curve_Majorants := Realtime_Tangent_Range_Majorants (Curve, 0.0, 1.0);
            for Axis in Axis_Name loop
               for Order in Majorant_Order loop
                  if Curve.Whole_Curve_Majorants (Axis) (Order) >= Dimensionless'Last
                    or else not Is_Finite (Curve.Whole_Curve_Majorants (Axis) (Order))
                  then
                     return (Status => Candidate_Numerically_Unsafe);
                  end if;
               end loop;
            end loop;
            Curve.Has_Whole_Curve_Majorants := True;
            Curve.Bounds := Bounds_On_Parameter_Range (Curve, 0.0, 1.0);
            for Axis in Axis_Name loop
               if Curve.Bounds.Velocity (Axis) >= Dimensionless'Last
                 or else Curve.Bounds.Acceleration (Axis) >= Curvature'Last
                 or else Curve.Bounds.Jerk (Axis) >= Curvature_To_2'Last
                 or else Curve.Bounds.Snap (Axis) >= Curvature_To_3'Last
                 or else Curve.Bounds.Crackle (Axis) >= Curvature_To_4'Last
               then
                  return (Status => Candidate_Numerically_Unsafe);
               end if;
            end loop;
            return (Status => Candidate_Success, Curve => Curve);
         end;
      end;
   end Build_Candidate;

   function Arc_Length (Curve : Stereographic_Curve) return Length is
   begin
      return Arc_Length (Curve.Evaluator_Data);
   end Arc_Length;

   function Arc_Length (Evaluator : Stereographic_Curve_Evaluator) return Length is
   begin
      return (if Evaluator.Kind = Zero_Curve_Kind then 0.0 * mm else Evaluator.Length_Value);
   end Arc_Length;

   function Axis_Is_Structurally_Constant (Curve : Stereographic_Curve; Axis : Axis_Name) return Boolean is
   begin
      return Curve.Evaluator_Data.Kind = Zero_Curve_Kind or else Curve.Structurally_Constant_Axes (Axis);
   end Axis_Is_Structurally_Constant;

   function Select_Distance_Warp
     (Request                                        : Blend_Request;
      Chord_Direction, Start_Tangent, Finish_Tangent : Position_Scale;
      Maximum_Lambda                                 : Dimensionless) return Warp_Selection
   is
      --  U is normalized physical distance and V is the coordinate of the polynomial tangent chart. Select W in the
      --  endpoint-preserving map
      --
      --     V(U) = W·U / (1 - U + W·U).
      --
      --  W = 1 is the identity, W < 1 spends more distance near the start tangent, and W > 1 spends more distance
      --  near the finish tangent.

      --  For unit endpoint tangents s and f, G = s·f = cos(θ) and 1 - G² is the determinant of their Gram matrix.
      G           : constant Dimensionless := Dot (Start_Tangent, Finish_Tangent);
      Denominator : constant Dimensionless := 1.0 - G * G;

      --  A zero-bubble chart is a useful closure proposal only when the requested endpoint tangent jets are flat
      --  through order 3 and the physical chord is in their plane up to floating-point backward error.
      function Endpoint_Jets_Are_Flat return Boolean;
      --  Test whether all requested endpoint tangent derivatives through order three vanish.

      function Endpoint_Jets_Are_Flat return Boolean is
      begin
         for Axis in Axis_Name loop
            if Request.Start.Jet.Tangent_Derivative_1 (Axis) /= 0.0 / mm
              or else Request.Start.Jet.Tangent_Derivative_2 (Axis) /= 0.0 / mm ** 2
              or else Request.Start.Jet.Tangent_Derivative_3 (Axis) /= 0.0 / mm ** 3
              or else Request.Finish.Jet.Tangent_Derivative_1 (Axis) /= 0.0 / mm
              or else Request.Finish.Jet.Tangent_Derivative_2 (Axis) /= 0.0 / mm ** 2
              or else Request.Finish.Jet.Tangent_Derivative_3 (Axis) /= 0.0 / mm ** 3
            then
               return False;
            end if;
         end loop;
         return True;
      end Endpoint_Jets_Are_Flat;
   begin
      --  Nearly parallel or antiparallel endpoint tangents make the dual basis below ill-conditioned. In that case
      --  there is no trustworthy scalar trim decomposition, so retain the identity map.
      if Denominator <= 1.0E-10 then
         return (Status => Warp_Was_Selected, Factor => 1.0, Seed => <>);
      end if;
      declare
         --  Construct the dual vectors
         --
         --     s* = (s - G·f) / (1 - G²),
         --     f* = (f - G·s) / (1 - G²).
         --
         --  They obey s*·s = f*·f = 1 and s*·f = f*·s = 0. Consequently Incoming = c·s* and Outgoing = c·f* are the
         --  chord coordinates in the oblique endpoint-tangent basis:
         --
         --     c ≈ Incoming·s + Outgoing·f.
         Start_Dual  : constant Position_Scale := (Start_Tangent - Finish_Tangent * G) / Denominator;
         Finish_Dual : constant Position_Scale := (Finish_Tangent - Start_Tangent * G) / Denominator;
         Incoming    : constant Dimensionless := Dot (Chord_Direction, Start_Dual);
         Outgoing    : constant Dimensionless := Dot (Chord_Direction, Finish_Dual);
         Scale       : constant Dimensionless :=
           Dimensionless'Max (1.0, Dimensionless'Max (abs Incoming, abs Outgoing));

         Chord_In_Endpoint_Plane : constant Boolean :=
           Chord_Is_Numerically_In_Endpoint_Plane (Request, Chord_Direction, Start_Tangent, Finish_Tangent);
      begin
         if Incoming > 0.0 and then Outgoing > 0.0 then
            --  The scalar warp describes an ordinary corner only when both tangent-basis coordinates are positive.
            --  Enforce
            --
            --     1 / Maximum_Trim_Asymmetry
            --        ≤ Outgoing / Incoming
            --        ≤ Maximum_Trim_Asymmetry.
            --
            --  Check the requested cap before selecting the identity map for numerically negligible asymmetry.
            --  Otherwise a positive but very short leg could bypass the advertised 20:1 limit.
            if Incoming > Maximum_Trim_Asymmetry_Check * Outgoing
              or else Outgoing > Maximum_Trim_Asymmetry_Check * Incoming
            then
               return (Status => Warp_Trim_Asymmetry_Is_Unsupported, others => <>);
            elsif Incoming <= 1.0E-12 * Scale or else Outgoing <= 1.0E-12 * Scale then
               return (Status => Warp_Was_Selected, Factor => 1.0, Seed => <>);
            end if;
         else
            return (Status => Warp_Was_Selected, Factor => 1.0, Seed => <>);
         end if;

         declare
            --  Let R be the required outgoing-to-incoming trim ratio. For endpoint angle θ, Cos_Half = cos(θ/2),
            --  Sin_Half = sin(θ/2), and Q = tan(θ/4). Q is the stereographic coordinate of either endpoint relative
            --  to the angle bisector.
            Ratio      : constant Dimensionless := Outgoing / Incoming;
            Cos_Half   : constant Dimensionless := Dimensionless_Math.Sqrt (Dimensionless'Max (0.0, 0.5 * (1.0 + G)));
            Sin_Half   : constant Dimensionless := Dimensionless_Math.Sqrt (Dimensionless'Max (0.0, 0.5 * (1.0 - G)));
            Q          : constant Dimensionless := Sin_Half / (1.0 + Cos_Half);
            Log_Low    : Dimensionless := Distance_Warp_Log_Lower_Bound;
            Log_High   : Dimensionless := Distance_Warp_Log_Upper_Bound;
            Log_Warp   : Dimensionless :=
              Dimensionless'Max (Log_Low, Dimensionless'Min (Log_High, 1.2 * Dimensionless_Math.Log (Ratio)));
            Integral_A : Dimensionless := 0.0;
            Integral_B : Dimensionless := 0.0;

            procedure Integrate (Candidate_Log_Warp : Dimensionless; A, B, D_A, D_B : out Dimensionless);

            --  For W = exp(Candidate_Log_Warp), integrate the ideal tangent transition as
            --
            --     ∫₀¹ T(U) dU = A(W)·s + B(W)·f,
            --
            --  and also return dA/dlog(W) and dB/dlog(W). Working in log(W) guarantees W > 0 and treats reciprocal
            --  warps symmetrically.
            procedure Integrate (Candidate_Log_Warp : Dimensionless; A, B, D_A, D_B : out Dimensionless) is
               Candidate_Warp   : constant Dimensionless := Dimensionless_Math.Exp (Candidate_Log_Warp);
               Panel_Count      : constant Positive := 4;
               --  Integrate in the unwarped chart coordinate V, using
               --
               --     U(V)   = V / (W·(1 - V) + V),
               --     dU/dV  = W / (W - (W - 1)·V)².
               --
               --  For a strong warp, dU/dV is concentrated at one endpoint. Subtract that endpoint tangent before
               --  applying the Jacobian and add its exactly known integral back. The degree-seven flat chart makes
               --  the remaining integrand vanish to fourth order in the concentrated boundary layer, so fixed GL16
               --  work remains accurate even at the 20:1 trim limit.
               Reference_Start  : constant Dimensionless := (if Candidate_Warp < 1.0 then 1.0 else 0.0);
               Reference_Finish : constant Dimensionless := (if Candidate_Warp < 1.0 then 0.0 else 1.0);

               procedure Accumulate (V, Weight : Dimensionless);

               procedure Accumulate (V, Weight : Dimensionless) is
                  --  D_Warp_Jacobian is ∂(dU/dV)/∂log(W), which supplies the derivative needed by the safeguarded
                  --  Newton solve.
                  Warp_Denominator   : constant Dimensionless := Candidate_Warp - (Candidate_Warp - 1.0) * V;
                  Warp_Jacobian      : constant Dimensionless := Candidate_Warp / Warp_Denominator ** 2;
                  D_Warp_Jacobian    : constant Dimensionless :=
                    Warp_Jacobian * (1.0 - 2.0 * Candidate_Warp * (1.0 - V) / Warp_Denominator);
                  --  H is the degree-7 smoothstep
                  --
                  --     H(V) = V⁴·(35 - 84·V + 70·V² - 20·V³).
                  --
                  --  Its first three derivatives vanish at V = 0 and V = 1. Z moves from -Q to +Q; inverse
                  --  stereographic projection then traces the unit tangent from s to f.
                  H                  : constant Dimensionless := V ** 4 * (35.0 + V * (-84.0 + V * (70.0 - 20.0 * V)));
                  Z                  : constant Dimensionless := Q * (2.0 * H - 1.0);
                  Z2                 : constant Dimensionless := Z * Z;
                  Stereo_Denominator : constant Dimensionless := 1.0 + Z2;
                  X                  : constant Dimensionless := (1.0 - Z2) / Stereo_Denominator;
                  Y                  : constant Dimensionless := 2.0 * Z / Stereo_Denominator;
                  --  Resolve the planar unit tangent into the endpoint basis:
                  --
                  --     T(V) = Start_Coefficient·s + Finish_Coefficient·f.
                  Start_Coefficient  : constant Dimensionless := 0.5 * (X / Cos_Half - Y / Sin_Half);
                  Finish_Coefficient : constant Dimensionless := 0.5 * (X / Cos_Half + Y / Sin_Half);
                  Start_Delta        : constant Dimensionless := Start_Coefficient - Reference_Start;
                  Finish_Delta       : constant Dimensionless := Finish_Coefficient - Reference_Finish;
               begin
                  A := A + Weight * Warp_Jacobian * Start_Delta;
                  B := B + Weight * Warp_Jacobian * Finish_Delta;
                  D_A := D_A + Weight * D_Warp_Jacobian * Start_Delta;
                  D_B := D_B + Weight * D_Warp_Jacobian * Finish_Delta;
               end Accumulate;
            begin
               A := Reference_Start;
               B := Reference_Finish;
               D_A := 0.0;
               D_B := 0.0;
               for Panel in 0 .. Panel_Count - 1 loop
                  declare
                     Middle : constant Dimensionless := (Dimensionless (Panel) + 0.5) / Dimensionless (Panel_Count);
                     Half   : constant Dimensionless := 0.5 / Dimensionless (Panel_Count);
                  begin
                     for I in GL16_Positive_Index loop
                        Accumulate (Middle - Half * GL16_Nodes (I), Half * GL16_Weights (I));
                        Accumulate (Middle + Half * GL16_Nodes (I), Half * GL16_Weights (I));
                     end loop;
                  end;
               end loop;
            end Integrate;
         begin
            --  Equal trim coordinates give R = 1 and, by symmetry, W = 1. Avoid all scalar-warp work on this
            --  overwhelmingly common planner path. For the endpoint-flat zero-bubble proposal, symmetry gives
            --
            --     λ = Incoming / A(1) = Outgoing / B(1).
            if abs (Outgoing - Incoming)
              <= 128.0 * Dimensionless'Model_Epsilon * Dimensionless'Max (Incoming, Outgoing)
            then
               if Endpoint_Jets_Are_Flat and then Chord_In_Endpoint_Plane then
                  declare
                     D_A, D_B : Dimensionless;
                     Seed     : Closure_Seed;
                  begin
                     Integrate (0.0, Integral_A, Integral_B, D_A, D_B);
                     if Integral_A > 0.0 then
                        declare
                           Lambda : constant Dimensionless := Incoming / Integral_A;
                        begin
                           if Is_Finite (Lambda) and then Lambda >= 1.0 and then Lambda <= Maximum_Lambda then
                              Seed := (Valid => True, Lambda => Lambda, C0 => [others => 0.0]);
                           end if;
                        end;
                     end if;
                     return (Status => Warp_Was_Selected, Factor => 1.0, Seed => Seed);
                  end;
               else
                  return (Status => Warp_Was_Selected, Factor => 1.0, Seed => <>);
               end if;
            end if;

            --  With zero closure bubble, the degree-seven endpoint-flat chart is monotone in the angle-bisector
            --  frame. Closure for some length ratio λ requires
            --
            --     λ·A(W) = Incoming,     λ·B(W) = Outgoing.
            --
            --  Eliminating λ leaves the scalar equation
            --
            --     F(log(W)) = B(W) - R·A(W) = 0.
            --
            --  A and B vary monotonically with log(W), so the root is unique. Safeguarded Newton retains fixed
            --  bounded construction cost, and the wide log bracket covers the complete advertised 20:1 trim domain
            --  near reversals.
            declare
               Low_A, Low_B, Low_D_A, Low_D_B     : Dimensionless;
               High_A, High_B, High_D_A, High_D_B : Dimensionless;
               Low_Residual, High_Residual        : Dimensionless;
               Residual_Increases                 : Boolean := False;
            begin
               Integrate (Log_Low, Low_A, Low_B, Low_D_A, Low_D_B);
               Integrate (Log_High, High_A, High_B, High_D_A, High_D_B);
               Low_Residual := Low_B - Ratio * Low_A;
               High_Residual := High_B - Ratio * High_A;
               --  The endpoint residuals must be finite and must bracket zero. Equal signs mean no supported W
               --  reproduces R.
               if not Is_Finite (Low_Residual) or else not Is_Finite (High_Residual) then
                  return (Status => Warp_Selection_Is_Numerically_Unsafe, others => <>);
               elsif Low_Residual = 0.0 then
                  Log_Warp := Log_Low;
               elsif High_Residual = 0.0 then
                  Log_Warp := Log_High;
               elsif (Low_Residual < 0.0) = (High_Residual < 0.0) then
                  return (Status => Warp_Closure_Failed, others => <>);
               else
                  Residual_Increases := Low_Residual < High_Residual;
               end if;

               if Low_Residual /= 0.0 and then High_Residual /= 0.0 then
                  --  Each iteration first contracts the proven bracket, then takes
                  --
                  --     log(W) ← log(W) - F/F′.
                  --
                  --  A nonfinite or out-of-bracket Newton proposal is replaced by the bisection midpoint.
                  for Iteration in 1 .. 56 loop
                     declare
                        D_A, D_B             : Dimensionless;
                        Residual, D_Residual : Dimensionless;
                        Proposed             : Dimensionless;
                        Residual_Scale       : Dimensionless;
                     begin
                        Integrate (Log_Warp, Integral_A, Integral_B, D_A, D_B);
                        Residual := Integral_B - Ratio * Integral_A;
                        D_Residual := D_B - Ratio * D_A;
                        Residual_Scale := Dimensionless'Max (1.0, abs Integral_B + abs (Ratio * Integral_A));
                        if abs Residual <= 2_048.0 * Dimensionless'Model_Epsilon * Residual_Scale
                          or else Log_High - Log_Low <= 2.0E-13
                        then
                           exit;
                        elsif (Residual < 0.0) = Residual_Increases then
                           Log_Low := Log_Warp;
                        else
                           Log_High := Log_Warp;
                        end if;

                        Proposed :=
                          (if D_Residual /= 0.0 and then Is_Finite (D_Residual)
                           then Log_Warp - Residual / D_Residual
                           else 0.5 * (Log_Low + Log_High));
                        if not Is_Finite (Proposed) or else Proposed <= Log_Low or else Proposed >= Log_High then
                           Proposed := 0.5 * (Log_Low + Log_High);
                        end if;
                        Log_Warp := Proposed;
                     end;
                  end loop;
               end if;
            end;

            declare
               D_A, D_B       : Dimensionless;
               Factor         : constant Dimensionless := Dimensionless_Math.Exp (Log_Warp);
               Seed           : Closure_Seed;
               Final_Residual : Dimensionless;
               Final_Scale    : Dimensionless;
            begin
               --  Re-evaluate the selected W independently. A valid result needs positive finite A and B and must
               --  reproduce B/A = R within the final scaled roundoff allowance.
               Integrate (Log_Warp, Integral_A, Integral_B, D_A, D_B);
               Final_Residual := Integral_B - Ratio * Integral_A;
               Final_Scale := Dimensionless'Max (1.0, abs Integral_B + abs (Ratio * Integral_A));
               if not Is_Finite (Factor)
                 or else not Is_Finite (Integral_A)
                 or else not Is_Finite (Integral_B)
                 or else not Is_Finite (Final_Residual)
                 or else not Is_Finite (Final_Scale)
                 or else Integral_A <= 0.0
                 or else Integral_B <= 0.0
               then
                  return (Status => Warp_Selection_Is_Numerically_Unsafe, others => <>);
               elsif abs Final_Residual > 8_192.0 * Dimensionless'Model_Epsilon * Final_Scale then
                  return (Status => Warp_Closure_Failed, others => <>);
               end if;
               if Endpoint_Jets_Are_Flat and then Chord_In_Endpoint_Plane then
                  --  For structurally planar flat endpoint jets, offer the zero-bubble construction as a proposal
                  --  when it respects the arc-length cap. Build_Candidate canonicalizes the admitted normal noise and
                  --  includes its continuous endpoint correction in the executed derivative bounds.
                  declare
                     Lambda : constant Dimensionless := Incoming / Integral_A;
                  begin
                     if Is_Finite (Lambda) and then Lambda >= 1.0 and then Lambda <= Maximum_Lambda then
                        Seed := (Valid => True, Lambda => Lambda, C0 => [others => 0.0]);
                     end if;
                  end;
               end if;
               return (Status => Warp_Was_Selected, Factor => Factor, Seed => Seed);
            end;
         end;
      end;
   end Select_Distance_Warp;

   function Create_Blend (Request : Blend_Request) return Blend_Result is
      Chord                             : Position_Offset;
      Chord_Length                      : Length;
      Norm_OK                           : Boolean;
      Coordinate_Scale                  : Dimensionless := 1.0;
      Start_Canonical, Finish_Canonical : Scaled_Tangent_Jet;

      function Safe_Point_Component (Value : Length) return Boolean;
      function Safe_Length_Magnitude (Value : Length) return Boolean;

      function Safe_Point_Component (Value : Length) return Boolean is
         Raw : constant Dimensionless := Dimensionless (Value / mm);
      begin
         --  Later squared-distance calculations need headroom for four axes, interval inflation, and intermediate
         --  additions, hence the conservative √Last/16 component limit.
         return Is_Finite (Raw) and then abs Raw <= Dimensionless_Math.Sqrt (Dimensionless'Last) / 16.0;
      end Safe_Point_Component;

      function Safe_Length_Magnitude (Value : Length) return Boolean is
         Raw : constant Dimensionless := Dimensionless (Value / mm);
      begin
         return Is_Finite (Raw) and then abs Raw <= Dimensionless_Math.Sqrt (Dimensionless'Last) / 16.0;
      end Safe_Length_Magnitude;

   begin
      for A in Axis_Name loop
         if not Safe_Point_Component (Request.Start.Point (A)) then
            return (Kind => Blend_Invalid_Start_Point);
         end if;
      end loop;

      for A in Axis_Name loop
         if not Safe_Point_Component (Request.Finish.Point (A)) then
            return (Kind => Blend_Invalid_Finish_Point);
         end if;
      end loop;

      Chord := Request.Finish.Point - Request.Start.Point;
      Chord_Length := Safe_Norm (Chord, Norm_OK);

      --  Judge endpoint separation relative to the absolute coordinate scale. This rejects a chord that disappeared in
      --  subtraction even when its computed norm is nonzero.
      for A in Axis_Name loop
         Coordinate_Scale :=
           Dimensionless'Max
             (Coordinate_Scale,
              Dimensionless'Max
                (abs Dimensionless (Request.Start.Point (A) / mm), abs Dimensionless (Request.Finish.Point (A) / mm)));
      end loop;

      if not Norm_OK
        or else Dimensionless (Chord_Length / mm) <= 256.0 * Dimensionless'Model_Epsilon * Coordinate_Scale
      then
         return (Kind => Blend_Endpoints_Too_Close);
      end if;

      if not Canonicalize_And_Validate_Jet (Request.Start.Jet, Chord_Length, Start_Canonical) then
         return (Kind => Blend_Invalid_Start_Jets);
      elsif not Canonicalize_And_Validate_Jet (Request.Finish.Jet, Chord_Length, Finish_Canonical) then
         return (Kind => Blend_Invalid_Finish_Jets);
      elsif not Safe_Length_Magnitude (Request.Maximum_Position_Error)
        or else Request.Maximum_Position_Error <= 0.0 * mm
      then
         return (Kind => Blend_Invalid_Position_Error);
      elsif not Safe_Length_Magnitude (Request.Maximum_Arc_Length) or else Request.Maximum_Arc_Length < Chord_Length
      then
         return (Kind => Blend_Invalid_Arc_Length_Limit);
      end if;

      declare
         Chord_Direction            : constant Position_Scale := Chord / Chord_Length;
         Selection                  : constant Warp_Selection :=
           Select_Distance_Warp
             (Request,
              Chord_Direction,
              Start_Canonical (0),
              Finish_Canonical (0),
              Dimensionless (Request.Maximum_Arc_Length / Chord_Length));
         Warp_Factor                : constant Dimensionless := Selection.Factor;
         --  Try the angle bisector first, then chord-biased and endpoint-biased frame directions. The distance warp is
         --  geometry-wide and remains fixed while only the stereographic projection pole changes.
         Candidates                 : constant Frame_Candidate_Array :=
           [Start_Canonical (0) + Finish_Canonical (0),
            Chord_Direction + Start_Canonical (0) + Finish_Canonical (0),
            Chord_Direction,
            Start_Canonical (0) + Chord_Direction,
            Finish_Canonical (0) + Chord_Direction,
            Start_Canonical (0),
            Finish_Canonical (0)];
         Tried_Directions           : Frame_Candidate_Array (Candidates'Range) := [others => [others => 0.0]];
         Tried_Count                : Natural := 0;
         Saw_Representation_Failure : Boolean := False;
         Saw_Numerical_Failure      : Boolean := False;
      begin
         case Selection.Status is
            when Warp_Trim_Asymmetry_Is_Unsupported | Warp_Closure_Failed =>
               return (Kind => Blend_Closure_Failed);

            when Warp_Selection_Is_Numerically_Unsafe                     =>
               return (Kind => Blend_Numerically_Unsafe);

            when Warp_Was_Selected                                        =>
               null;
         end case;
         if Warp_Factor <= 0.0 or else not Is_Finite (Warp_Factor) then
            return (Kind => Blend_Numerically_Unsafe);
         end if;
         for Candidate of Candidates loop
            declare
               Candidate_Norm : constant Dimensionless := Safe_Norm (Candidate, Norm_OK);
               Direction      : Position_Scale := [others => 0.0];
               Duplicate      : Boolean := False;
            begin
               if Norm_OK and then Candidate_Norm > 256.0 * Dimensionless'Model_Epsilon then
                  Direction := Candidate / Candidate_Norm;
                  --  Nearly identical first frame vectors produce the same basis after Gram-Schmidt, so avoid
                  --  repeating an equivalent closure and compilation attempt.
                  for I in 1 .. Tried_Count loop
                     if 1.0 - Dot (Direction, Tried_Directions (I)) <= 512.0 * Dimensionless'Model_Epsilon then
                        Duplicate := True;
                     end if;
                  end loop;

                  if not Duplicate then
                     Tried_Count := Tried_Count + 1;
                     Tried_Directions (Tried_Count) := Direction;
                     declare
                        Frame : Frame_Vector_Array;
                     begin
                        --  Inverse stereographic projection divides by 1 + T₀. Reject frames which place either
                        --  endpoint tangent too close to the projection pole T₀ = -1.
                        if Complete_Frame
                             (Direction, Start_Canonical (0), Finish_Canonical (0), Chord_Direction, Frame)
                          and then
                            1.0 + Frame_Coordinate (Frame, 0, Start_Canonical (0))
                            > Minimum_Safe_Reciprocal_Denominator
                          and then
                            1.0 + Frame_Coordinate (Frame, 0, Finish_Canonical (0))
                            > Minimum_Safe_Reciprocal_Denominator
                        then
                           declare
                              Start_Chart, Finish_Chart : Chart_Jet_Array;
                           begin
                              if Chart_From_Canonical_Jet (Frame, Start_Canonical, Start_Chart)
                                and then Chart_From_Canonical_Jet (Frame, Finish_Canonical, Finish_Chart)
                              then
                                 declare
                                    Built : constant Candidate_Result :=
                                      Build_Candidate
                                        (Request,
                                         Chord_Length,
                                         Chord_Direction,
                                         Frame,
                                         Start_Chart,
                                         Finish_Chart,
                                         Warp_Factor,
                                         (if Candidate = Candidates (Candidates'First)
                                          then Selection.Seed
                                          else (others => <>)));
                                 begin
                                    case Built.Status is
                                       when Candidate_Success               =>
                                          return (Kind => Blend_Success, Curve => Built.Curve);

                                       when Candidate_Closure_Failed        =>
                                          null;

                                       when Candidate_Representation_Failed =>
                                          Saw_Representation_Failure := True;

                                       when Candidate_Numerically_Unsafe    =>
                                          Saw_Numerical_Failure := True;
                                    end case;
                                 end;
                              end if;
                           end;
                        end if;
                     end;
                  end if;
               end if;
            end;
         end loop;

         --  Report the most informative failure class seen across all supported frames.
         if Saw_Representation_Failure then
            return (Kind => Blend_Representation_Failed);
         elsif Saw_Numerical_Failure then
            return (Kind => Blend_Numerically_Unsafe);
         else
            return (Kind => Blend_Closure_Failed);
         end if;
      end;
   end Create_Blend;

   function Derivative_Bounds (Curve : Stereographic_Curve) return Unit_Speed_Axial_Derivative_Bounds is
   begin
      return (if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then (others => <>) else Curve.Bounds);
   end Derivative_Bounds;

   function Derivative_Bounds
     (Curve : Stereographic_Curve; Start_Distance, End_Distance : Length) return Unit_Speed_Axial_Derivative_Bounds is
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return (others => <>);
      else
         declare
            Start_Raw : constant Dimensionless := Dimensionless (Start_Distance / Curve.Evaluator_Data.Length_Value);
            End_Raw   : constant Dimensionless := Dimensionless (End_Distance / Curve.Evaluator_Data.Length_Value);
            Start_U   : constant Dimensionless :=
              (if Start_Distance = 0.0 * mm
               then 0.0
               elsif Start_Distance = Curve.Evaluator_Data.Length_Value
               then 1.0
               else Dimensionless'Max (0.0, Down (Start_Raw)));
            End_U     : constant Dimensionless :=
              (if End_Distance = 0.0 * mm
               then 0.0
               elsif End_Distance = Curve.Evaluator_Data.Length_Value
               then 1.0
               else Dimensionless'Min (1.0, Up (End_Raw)));
         begin
            --  The public range is expressed in physical distance. Preserve exact curve endpoints so full-range and
            --  point-at-end queries take their intended certificate paths; widen every interior division before
            --  asking the parameter-space majorant for a proof.
            return Bounds_On_Parameter_Range (Curve, Start_U, End_U);
         end;
      end if;
   end Derivative_Bounds;

   procedure Certified_Ideal_Point_At_Parameter
     (Curve     : Stereographic_Curve;
      Parameter : Curve_Parameter;
      Point     : out Position;
      Error     : out Length;
      Success   : out Boolean) is
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         Point := Curve.Evaluator_Data.Start_Point;
         Error := 0.0 * mm;
      else
         --  Ideal_Point_At_Parameter is a diagnostic quadrature of the authoritative chart. Its result is covered by
         --  the same cumulative construction certificate exposed by the realtime evaluator.
         Point := Point_At_Parameter (Curve, Parameter);
         Error := Curve.Certified_Position_Error;
      end if;
      Success := Error >= 0.0 * mm and then Error < Length'Last;
   exception
      when Constraint_Error =>
         Point := Curve.Evaluator_Data.Start_Point;
         Error := Length'Last;
         Success := False;
   end Certified_Ideal_Point_At_Parameter;

   function Ideal_Point_At_Parameter (Curve : Stereographic_Curve; Parameter : Curve_Parameter) return Position is
      Panel_Count       : constant Positive := 16;
      Integral          : Position_Scale := [others => 0.0];
      Target_U          : constant Dimensionless := Dimensionless (Parameter);
      Target_V          : constant Dimensionless := Warp_Parameter (Target_U, Curve.Warp_Factor);
      Reference_Tangent : Position_Scale;
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind or else Parameter = 0.0 then
         return Curve.Evaluator_Data.Start_Point;
      end if;

      --  Integrate in chart coordinate V rather than merely choosing panel boundaries there and then integrating
      --  each panel in U. A strong Möbius warp can map one such panel onto most of the distance range, leaving its
      --  U-space integrand sharply concentrated at one end.
      --
      --  Since
      --
      --     U = V/(W·(1 - V) + V),
      --
      --  the chart-space integrand is Tangent (V) * dU/dV, where
      --
      --     dU/dV = W/(W - (W - 1)·V)².
      --
      --  Subtract the tangent at the end where that Jacobian is largest. The difference tends to zero there,
      --  avoiding needless loss of precision. Its exactly integrable constant part is restored after quadrature
      --  because ∫₀ᵀᵃʳᵍᵉᵗⱽ(dU/dV)dV = Target_U.
      Reference_Tangent :=
        (if Curve.Warp_Factor <= 1.0
         then Frame_Vector (Curve.Frame, Stereographic (Chart_Value (Curve.Coefficients, 0.0)))
         else Frame_Vector (Curve.Frame, Stereographic (Chart_Value (Curve.Coefficients, 1.0))));

      for Panel in 1 .. Panel_Count loop
         declare
            Left_V   : constant Dimensionless := Target_V * Dimensionless (Panel - 1) / Dimensionless (Panel_Count);
            Right_V  : constant Dimensionless := Target_V * Dimensionless (Panel) / Dimensionless (Panel_Count);
            Middle_V : constant Dimensionless := 0.5 * (Left_V + Right_V);
            Half_V   : constant Dimensionless := 0.5 * (Right_V - Left_V);
         begin
            for I in GL16_Positive_Index loop
               declare
                  Left_Node  : constant Dimensionless := Middle_V - Half_V * GL16_Nodes (I);
                  Right_Node : constant Dimensionless := Middle_V + Half_V * GL16_Nodes (I);

                  Left_Denominator  : constant Dimensionless :=
                    Curve.Warp_Factor - (Curve.Warp_Factor - 1.0) * Left_Node;
                  Right_Denominator : constant Dimensionless :=
                    Curve.Warp_Factor - (Curve.Warp_Factor - 1.0) * Right_Node;

                  Left_Jacobian  : constant Dimensionless := Curve.Warp_Factor / Left_Denominator ** 2;
                  Right_Jacobian : constant Dimensionless := Curve.Warp_Factor / Right_Denominator ** 2;

                  Left_Tangent  : constant Position_Scale :=
                    Frame_Vector (Curve.Frame, Stereographic (Chart_Value (Curve.Coefficients, Left_Node)));
                  Right_Tangent : constant Position_Scale :=
                    Frame_Vector (Curve.Frame, Stereographic (Chart_Value (Curve.Coefficients, Right_Node)));
               begin
                  Integral :=
                    Integral
                    + ((Left_Tangent - Reference_Tangent) * Left_Jacobian
                       + (Right_Tangent - Reference_Tangent) * Right_Jacobian)
                      * Dimensionless (Half_V * GL16_Weights (I));
               end;
            end loop;
         end;
      end loop;

      Integral := Integral + Reference_Tangent * Target_U;

      --  Unit-speed displacement is Length·∫₀ᵁT(u)du.
      return
        [for Axis in Axis_Name =>
           Curve.Evaluator_Data.Start_Point (Axis) + Curve.Evaluator_Data.Length_Value * Integral (Axis)];
   end Ideal_Point_At_Parameter;

   function Point_At_Parameter (Curve : Stereographic_Curve; Parameter : Curve_Parameter) return Position is
   begin
      return Point_At_Parameter (Curve.Evaluator_Data, Parameter);
   end Point_At_Parameter;

   function Point_At_Parameter
     (Evaluator : Stereographic_Curve_Evaluator; Parameter : Curve_Parameter) return Position is
   begin
      if Evaluator.Kind = Zero_Curve_Kind then
         return Evaluator.Start_Point;
      end if;
      return Point_At_Distance (Evaluator, Evaluator.Length_Value * Parameter);
   end Point_At_Parameter;

   function Point_At_Distance (Curve : Stereographic_Curve; Distance : Length) return Position is
   begin
      return Point_At_Distance (Curve.Evaluator_Data, Distance);
   end Point_At_Distance;

   function Point_At_Distance (Evaluator : Stereographic_Curve_Evaluator; Distance : Length) return Position is
   begin
      if Evaluator.Kind = Zero_Curve_Kind then
         return Evaluator.Start_Point;
      end if;
      --  Construction makes this one continuous formula bit-exact at both endpoints. Clamp only the normalized input
      --  domain; do not substitute a separately snapped point.
      return
        Evaluate_Rational_Point
          (Evaluator,
           (if Distance <= 0.0 * mm
            then 0.0
            elsif Distance >= Evaluator.Length_Value
            then 1.0
            else Dimensionless (Distance / Evaluator.Length_Value)));
   end Point_At_Distance;

   function To_Evaluator (Curve : Stereographic_Curve) return Stereographic_Curve_Evaluator
   is (Curve.Evaluator_Data);

   function Retained_Tangent_Error_Bound (Curve : Stereographic_Curve) return Dimensionless is
      Axis_Correction_Bounds : Dimensionless_Axis_Vector := [others => 0.0];
      Length_Raw             : Dimensionless;
      Scale                  : Dimensionless := 0.0;
      Sum                    : Dimensionless := 0.0;
      Correction_Bound       : Dimensionless := 0.0;
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return 0.0;
      end if;

      Length_Raw := Dimensionless (Curve.Evaluator_Data.Length_Value / mm);
      if Length_Raw <= 0.0 or else not Is_Finite (Length_Raw) then
         return Dimensionless'Last;
      end if;

      for Axis in Axis_Name loop
         declare
            Correction : constant Interval := Finish_Correction_Interval (Curve.Evaluator_Data, Axis);
         begin
            if not Correction.Valid then
               return Dimensionless'Last;
            end if;
            Axis_Correction_Bounds (Axis) := Up (Interval_Abs_Max (Correction) / Length_Raw);
            if not Is_Finite (Axis_Correction_Bounds (Axis)) then
               return Dimensionless'Last;
            end if;
            Scale := Dimensionless'Max (Scale, Axis_Correction_Bounds (Axis));
         end;
      end loop;

      if Scale > 0.0 then
         for Axis in Axis_Name loop
            Sum := Up (Sum + Up ((Axis_Correction_Bounds (Axis) / Scale) ** 2));
         end loop;
         if not Is_Finite (Sum) then
            return Dimensionless'Last;
         end if;
         declare
            Unit_Norm_Bound : constant Dimensionless := Certified_Upper_Square_Root (Sum);
         begin
            if Unit_Norm_Bound >= Dimensionless'Last or else Scale > Dimensionless'Last / Unit_Norm_Bound then
               return Dimensionless'Last;
            end if;
            Correction_Bound := Up ((693.0 / 256.0) * Up (Scale * Unit_Norm_Bound));
         end;
      end if;

      if not Is_Finite (Correction_Bound) or else Curve.Certified_Tangent_Error > Dimensionless'Last - Correction_Bound
      then
         return Dimensionless'Last;
      else
         return Up (Curve.Certified_Tangent_Error + Correction_Bound);
      end if;
   exception
      when Constraint_Error =>
         return Dimensionless'Last;
   end Retained_Tangent_Error_Bound;

   function Retained_Endpoint_Jet_Error_Bound
     (Curve : Stereographic_Curve; Order : Endpoint_Tangent_Derivative_Order) return Dimensionless is
   begin
      return (if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then 0.0 else Curve.Certified_Endpoint_Jet_Error (Order));
   end Retained_Endpoint_Jet_Error_Bound;

   function Position_Error_Bound (Curve : Stereographic_Curve) return Length is
   begin
      return (if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then 0.0 * mm else Curve.Certified_Position_Error);
   end Position_Error_Bound;

   function Projected_Tangent_Bound
     (Curve : Stereographic_Curve; Coefficients : Projection_Coefficients) return Curvature is
   begin
      return Projected_Bound_On_Parameter_Range (Curve, 0.0, 1.0, Coefficients);
   end Projected_Tangent_Bound;

   function Projected_Tangent_Bound
     (Curve : Stereographic_Curve; Start_Distance, End_Distance : Length; Coefficients : Projection_Coefficients)
      return Curvature is
   begin
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind then
         return 0.0 / mm;
      else
         declare
            Start_Raw : constant Dimensionless := Dimensionless (Start_Distance / Curve.Evaluator_Data.Length_Value);
            End_Raw   : constant Dimensionless := Dimensionless (End_Distance / Curve.Evaluator_Data.Length_Value);
            Start_U   : constant Dimensionless := Dimensionless'Max (0.0, Down (Start_Raw));
            End_U     : constant Dimensionless := Dimensionless'Min (1.0, Up (End_Raw));
         begin
            --  Widen both floating divisions before applying the requested-range projection certificate.
            return Projected_Bound_On_Parameter_Range (Curve, Start_U, End_U, Coefficients);
         end;
      end if;
   end Projected_Tangent_Bound;

   function Zero_Blend (Point : Position) return Stereographic_Curve is
   begin
      --  The canonical zero curve stores one point, zero length, and an empty antiderivative cache. Defaulted
      --  construction-only fields remain algebraic zeros.
      return
        (Evaluator_Data =>
           (Kind                     => Zero_Curve_Kind,
            Start_Point              => Point,
            Finish_Point             => Point,
            Length_Value             => 0.0 * mm,
            Antiderivative_Cache     => (others => <>),
            Uncorrected_Finish_Point => Point),
         others         => <>);
   end Zero_Blend;

end Prunt.Motion_Planner.Stereographic_Curves;
