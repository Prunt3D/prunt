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

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Prunt.Motion_Planner.Trajectory.Elementary; use Prunt.Motion_Planner.Trajectory.Elementary;

package body Prunt.Motion_Planner.Trajectory.Solvers.Estimates
  with SPARK_Mode => On
is

   pragma Extensions_Allowed (On);

   function Snap_Limited_Guess (Vd : Velocity; Cm : Crackle; T1 : Time) return Dimensionless is
      Speed_Limit   : constant Velocity := Velocity (2#1.0#E100);
      Crackle_Floor : constant Crackle := Crackle (2#1.0#E-300);
      Crackle_Limit : constant Crackle := Crackle (2#1.0#E300);
      Time_Floor    : constant Time := Time (2#1.0#E-150);
      Time_Limit    : constant Time := Time (2#1.0#E150);
      --  Arguments outside these get no estimate. They are far beyond what any machine has, and within them no
      --  operation below overflows.

      Iterate_Limit : constant Dimensionless := 2.0**300;
      --  The largest starting point from which the iteration is run.

      Guess : Dimensionless := -1.0;
   begin
      if not (Vd >= 0.0 * mm / s
              and then Vd <= Speed_Limit
              and then Cm >= Crackle_Floor
              and then Cm <= Crackle_Limit
              and then T1 >= Time_Floor
              and then T1 <= Time_Limit)
      then
         return Guess;
      end if;

      --  With U = T2/T1 the equation is 2*(1 + U)*(2 + U)**2 = Vd/(Cm*T1**4), whose left side is convex and
      --  increasing, so Newton's method from above the root decreases onto it.
      declare
         Scaled      : constant Dimensionless := Vd / (Cm * (T1**2)**2);
         U           : Dimensionless := (Scaled - 8.0) / 16.0;
         --  The tangent at zero lies below the curve, so this is at or above the root.
         Next, Value : Dimensionless;
      begin
         if U > 4.0 then
            --  The cubic term alone bounds the root more tightly once it dominates.
            U := Dimensionless'Min (U, Cube_Root (0.5 * Scaled));
         end if;
         if U > 0.0 and then U <= Iterate_Limit then
            for Iteration in 1 .. 64 loop
               pragma Loop_Invariant (U > 0.0 and then U <= Iterate_Limit);
               Value := 2.0 * (1.0 + U) * (2.0 + U)**2 - Scaled;
               Next := U - Value / ((2.0 + U) * (8.0 + 6.0 * U));
               exit when not (Next < U) or else Next <= 0.0;
               U := Next;
            end loop;
            Guess := U * T1 / s;
         end if;
      end;
      return Guess;
   end Snap_Limited_Guess;

   function Delta_V_Over_Distance (Solver : Delta_V_Solver; Start_Vel : Velocity; Distance : Length) return Velocity is
      pragma Suppress (Overflow_Check);
      --  No operation below overflows, which is proved, so the checks would only cost time.

      type Phase_Times is array (1 .. 4) of Dimensionless;

      Value_Limit   : constant Dimensionless := 2.0**200;
      Time_Limit    : constant Dimensionless := 2.0**100;
      Divisor_Floor : constant Dimensionless := 2.0**(-300);
      Root_Limit    : constant Dimensionless := 2.0**400;
      --  Speeds, distances and the crackle limit above Value_Limit in mm and s, phase times above Time_Limit, a
      --  crackle limit or a divisor below Divisor_Floor and terms under the square root above Root_Limit get no
      --  estimate, or end the iteration where it stands. They are far beyond what any machine has, and within
      --  them no operation below overflows.

      Gain_Limit   : constant Dimensionless := 2.0**606;
      Travel_Limit : constant Dimensionless := 2.0**710;
      Slope_Limit  : constant Dimensionless := 2.0**612;
      --  What the three functions below can return for arguments within the limits above.

      Vs : constant Dimensionless := Start_Vel / (mm / s);
      D  : constant Dimensionless := Distance / mm;
      Cm : constant Dimensionless := Solver.Cm / (mm / s**5);

      function Inputs_In_Range return Boolean
      is (Vs >= 0.0 and then Vs <= Value_Limit and then Cm >= Divisor_Floor and then Cm <= Value_Limit);

      function Times_In_Range (T : Phase_Times) return Boolean
      is (T (1) >= 0.0
          and then T (1) <= Time_Limit
          and then T (2) >= 0.0
          and then T (2) <= Time_Limit
          and then T (3) >= 0.0
          and then T (3) <= Time_Limit
          and then T (4) >= 0.0
          and then T (4) <= Time_Limit);

      procedure Lemma_Store (Before, After : Phase_Times; Index : Positive)
      with
        Ghost,
        Pre  =>
          Times_In_Range (Before)
          and then Index in 1 .. 4
          and then After (Index) >= 0.0
          and then After (Index) <= Time_Limit
          and then (if Index /= 1 then After (1) = Before (1))
          and then (if Index /= 2 then After (2) = Before (2))
          and then (if Index /= 3 then After (3) = Before (3))
          and then (if Index /= 4 then After (4) = Before (4)),
        Post => Times_In_Range (After);
      --  Times within range stay so when one of them, chosen at run time, is set to a value within range. Stated
      --  on its own, away from the arithmetic, the provers settle it at once.

      procedure Lemma_Store (Before, After : Phase_Times; Index : Positive) is null;

      function Gain (T : Phase_Times) return Dimensionless
      is (Cm * T (1) * (T (1) + T (2)) * (2.0 * T (1) + T (2) + T (3)) * (4.0 * T (1) + 2.0 * T (2) + T (3) + T (4)))
      with
        Pre  => Inputs_In_Range and then Times_In_Range (T),
        Post => Gain'Result >= 0.0 and then Gain'Result <= Gain_Limit;
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Gain);
      --  The velocity change, as in Fast_Velocity_At_Max_Time. What follows is proved from its bounds alone: its
      --  expression, repeated wherever it is called, only burdens the provers. The same holds for Travel.

      function Travel (T : Phase_Times) return Dimensionless
      is ((Vs + Gain (T) / 2.0) * (8.0 * T (1) + 4.0 * T (2) + 2.0 * T (3) + T (4)))
      with
        Pre  => Inputs_In_Range and then Times_In_Range (T),
        Post => Travel'Result >= 0.0 and then Travel'Result <= Travel_Limit;
      pragma Annotate (GNATprove, Hide_Info, "Expression_Function_Body", Travel);
      --  The distance, as in Fast_Distance_At_Max_Time.

      function Travel_Slope (T : Phase_Times; Variable : Positive) return Dimensionless
      with
        Pre  => Inputs_In_Range and then Times_In_Range (T),
        Post => Travel_Slope'Result >= 0.0 and then Travel_Slope'Result <= Slope_Limit;
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
      T_Before : Phase_Times
      with Ghost;
      --  T before the latest assignment to one of its components.
      Variable : Positive range 1 .. 3;
      Result   : Dimensionless;
   begin
      if not Solver.Limits_Valid
        or else Solver.Zero_Limit
        or else Solver.Unclassifiable
        or else not (Vs >= 0.0 and then D > 0.0)
        or else not (Inputs_In_Range and then D <= Value_Limit and then Times_In_Range (Full))
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
            if not (Short > 0.0
                    and then Short <= Root_Limit
                    and then Linear <= Root_Limit
                    and then Peak_Accel <= Root_Limit)
            then
               return -1.0 * mm / s;
            end if;
            declare
               Root : constant Dimensionless := Dimensionless_Math.Sqrt (Linear**2 + 2.0 * Peak_Accel * Short);
            begin
               if not (Root <= 2.0 * Root_Limit and then Linear + Root >= Divisor_Floor) then
                  return -1.0 * mm / s;
               end if;
               declare
                  Free_Time : constant Dimensionless := 2.0 * Short / (Linear + Root);
               begin
                  pragma Assert (Free_Time >= 0.0);
                  if not (Free_Time <= Time_Limit) then
                     return -1.0 * mm / s;
                  end if;
                  T_Before := T;
                  T (4) := Free_Time;
                  Lemma_Store (T_Before, T, 4);
               end;
            end;
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
            T_Before := T;
            T (I) := 0.0;
            Lemma_Store (T_Before, T, I);
            pragma Loop_Invariant (Times_In_Range (T));
         end loop;
         --  Newton's method from above the root decreases onto it. The saturated time is above it by the case
         --  selection, and so is the tangent at zero, which lies below the curve.
         declare
            Base  : constant Dimensionless := Travel (T);
            Slope : constant Dimensionless := Travel_Slope (T, Variable);
            X     : Dimensionless := Full (Variable);
            Next  : Dimensionless;
         begin
            pragma Assert (X <= Time_Limit);
            if Slope >= Divisor_Floor then
               X := Dimensionless'Min (X, (D - Base) / Slope);
            end if;
            if Variable = 1 and then X > 0.0 and then 32.0 * Cm * Fifth_Power (X) > D then
               --  From rest, or nearly, the fifth-power term decides the scale.
               X := Dimensionless'Min (X, Fifth_Root (D / (32.0 * Cm)));
            end if;
            pragma Assert (X <= Time_Limit);
            for Iteration in 1 .. 48 loop
               pragma Loop_Invariant (X <= Time_Limit and then Times_In_Range (T));
               exit when not (X > 0.0);
               T_Before := T;
               T (Variable) := X;
               Lemma_Store (T_Before, T, Variable);
               declare
                  Step_Slope : constant Dimensionless := Travel_Slope (T, Variable);
               begin
                  exit when not (Step_Slope >= Divisor_Floor);
                  Next := X - (Travel (T) - D) / Step_Slope;
               end;
               exit when not (Next < X);
               X := Next;
            end loop;
            pragma Assert (X <= Time_Limit);
            T_Before := T;
            T (Variable) := Dimensionless'Max (0.0, X);
            Lemma_Store (T_Before, T, Variable);
         end;
      end if;

      Result := Gain (T);
      return Result * (mm / s);
   end Delta_V_Over_Distance;

end Prunt.Motion_Planner.Trajectory.Solvers.Estimates;
