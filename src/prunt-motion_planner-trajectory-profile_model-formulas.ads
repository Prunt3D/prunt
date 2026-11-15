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

--  The stage polynomials of Prunt.Motion_Planner.Trajectory.Profiles are the integrals of the exact ramp.
--
--  Snap_At_Time, Jerk_At_Time, Acceleration_At_Time and Velocity_At_Time in Trajectory.Profiles each evaluate
--  a stage with a nested function of one shape: the value at the end of the previous stage, which is a recursive call
--  with that stage's full duration, plus a polynomial in the time into the stage, the phase times and the crackle.
--  Stage 1 starts from zero, or from the starting velocity. Those polynomials were derived by hand. The functions
--  below are the same polynomials over the reals, written as the code writes them, and the lemmas prove that each is
--  exactly what integrating the stage gives. With Lemma_Stage_Chain, which is what the recursive call relies on, that
--  is every step of an induction over the stages by which the code, evaluated in exact arithmetic, computes the
--  model. The induction itself, and the rounding of the floating-point evaluation, are not part of this unit.

package Prunt.Motion_Planner.Trajectory.Profile_Model.Formulas
  with SPARK_Mode => On, Ghost, Always_Terminates
is

   ------------------------------------------------------------------------------------------------------------------
   --  The polynomials of the code
   --
   --  Each expression has the grouping and the coefficients of the code. T1 to T4 and Cm there are R.T1 to R.T4 and
   --  R.C here, a power is a repeated product, in parentheses where the code multiplies by it, and a negation is a
   --  subtraction from zero. A stage for which the code is only the recursive call adds nothing.
   ------------------------------------------------------------------------------------------------------------------

   function Snap_Increment (R : Ramp; Stage : Stage_Index; DT : Real) return Real
   is (case Stage is
         when 1  => R.C * DT,
         when 2  => 0.0,
         when 3  => 0.0 - R.C * DT,
         when 4  => 0.0,
         when 5  => 0.0 - R.C * DT,
         when 6  => 0.0,
         when 7  => R.C * DT,
         when 8  => 0.0,
         when 9  => 0.0 - R.C * DT,
         when 10 => 0.0,
         when 11 => R.C * DT,
         when 12 => 0.0,
         when 13 => R.C * DT,
         when 14 => 0.0,
         when 15 => 0.0 - R.C * DT);
   --  Mirrors Snap_At_Stage of Prunt.Motion_Planner.Trajectory.Profiles: what the stage adds to snap in time DT.

   function Jerk_Increment (R : Ramp; Stage : Stage_Index; DT : Real) return Real
   is (case Stage is
         when 1  => R.C * (DT * DT) / 2.0,
         when 2  => R.C * DT * R.T1,
         when 3  => R.C * DT * (0.0 - DT + 2.0 * R.T1) / 2.0,
         when 4  => 0.0,
         when 5  => 0.0 - R.C * (DT * DT) / 2.0,
         when 6  => 0.0 - R.C * DT * R.T1,
         when 7  => R.C * DT * (DT - 2.0 * R.T1) / 2.0,
         when 8  => 0.0,
         when 9  => 0.0 - R.C * (DT * DT) / 2.0,
         when 10 => 0.0 - R.C * DT * R.T1,
         when 11 => R.C * DT * (DT - 2.0 * R.T1) / 2.0,
         when 12 => 0.0,
         when 13 => R.C * (DT * DT) / 2.0,
         when 14 => R.C * DT * R.T1,
         when 15 => R.C * DT * (0.0 - DT + 2.0 * R.T1) / 2.0);
   --  Mirrors Jerk_At_Stage of Prunt.Motion_Planner.Trajectory.Profiles: what the stage adds to jerk in time DT.

   function Accel_Increment (R : Ramp; Stage : Stage_Index; DT : Real) return Real
   is (case Stage is
         when 1  => R.C * (DT * DT * DT) / 6.0,
         when 2  => R.C * DT * R.T1 * (DT + R.T1) / 2.0,
         when 3  => R.C * DT * (0.0 - DT * DT + 3.0 * DT * R.T1 + 3.0 * R.T1 * (R.T1 + 2.0 * R.T2)) / 6.0,
         when 4  => R.C * DT * R.T1 * (R.T1 + R.T2),
         when 5  => R.C * DT * (0.0 - DT * DT + 6.0 * R.T1 * (R.T1 + R.T2)) / 6.0,
         when 6  => R.C * DT * R.T1 * (0.0 - DT + R.T1 + 2.0 * R.T2) / 2.0,
         when 7  => R.C * DT * (DT * DT - 3.0 * DT * R.T1 + 3.0 * (R.T1 * R.T1)) / 6.0,
         when 8  => 0.0,
         when 9  => 0.0 - R.C * (DT * DT * DT) / 6.0,
         when 10 => R.C * DT * R.T1 * (0.0 - DT - R.T1) / 2.0,
         when 11 => R.C * DT * (DT * DT - 3.0 * DT * R.T1 - 3.0 * R.T1 * (R.T1 + 2.0 * R.T2)) / 6.0,
         when 12 => 0.0 - R.C * DT * R.T1 * (R.T1 + R.T2),
         when 13 => R.C * DT * (DT * DT - 6.0 * R.T1 * (R.T1 + R.T2)) / 6.0,
         when 14 => R.C * DT * R.T1 * (DT - R.T1 - 2.0 * R.T2) / 2.0,
         when 15 => R.C * DT * (0.0 - DT * DT + 3.0 * DT * R.T1 - 3.0 * (R.T1 * R.T1)) / 6.0);
   --  Mirrors Acceleration_At_Stage of Trajectory.Profiles: what the stage adds to acceleration in time DT.

   function Velocity_Increment (R : Ramp; Stage : Stage_Index; DT : Real) return Real
   is (case Stage is
         when 1  => R.C * (DT * DT * DT * DT) / 24.0,
         when 2  => R.C * DT * R.T1 * (2.0 * (DT * DT) + 3.0 * DT * R.T1 + 2.0 * (R.T1 * R.T1)) / 12.0,
         when 3  =>
           R.C
           * DT
           * (0.0 - DT * DT * DT + 4.0 * (DT * DT) * R.T1 + 6.0 * DT * R.T1 * (R.T1 + 2.0 * R.T2)
              + 4.0 * R.T1 * (R.T1 * R.T1 + 3.0 * R.T1 * R.T2 + 3.0 * (R.T2 * R.T2)))
           / 24.0,
         when 4  =>
           R.C * DT * R.T1 * (DT * (R.T1 + R.T2) + 2.0 * (R.T1 * R.T1) + 3.0 * R.T1 * R.T2 + R.T2 * R.T2) / 2.0,
         when 5  =>
           R.C
           * DT
           * (0.0 - DT * DT * DT + 12.0 * DT * R.T1 * (R.T1 + R.T2)
              + 12.0
                * R.T1
                * (2.0 * (R.T1 * R.T1) + 3.0 * R.T1 * R.T2 + 2.0 * R.T1 * R.T3 + R.T2 * R.T2 + 2.0 * R.T2 * R.T3))
           / 24.0,
         when 6  =>
           R.C
           * DT
           * R.T1
           * (0.0 - 2.0 * (DT * DT) + 3.0 * DT * (R.T1 + 2.0 * R.T2) + 22.0 * (R.T1 * R.T1) + 30.0 * R.T1 * R.T2
              + 12.0 * R.T1 * R.T3
              + 6.0 * (R.T2 * R.T2)
              + 12.0 * R.T2 * R.T3)
           / 12.0,
         when 7  =>
           R.C
           * DT
           * (DT * DT * DT - 4.0 * (DT * DT) * R.T1 + 6.0 * DT * (R.T1 * R.T1)
              + 4.0
                * R.T1
                * (11.0 * (R.T1 * R.T1) + 18.0 * R.T1 * R.T2 + 6.0 * R.T1 * R.T3 + 6.0 * (R.T2 * R.T2)
                   + 6.0 * R.T2 * R.T3))
           / 24.0,
         when 8  =>
           R.C * DT * R.T1 * (2.0 * (R.T1 * R.T1) + 3.0 * R.T1 * R.T2 + R.T1 * R.T3 + R.T2 * R.T2 + R.T2 * R.T3),
         when 9  =>
           R.C
           * DT
           * (0.0 - DT * DT * DT
              + 24.0 * R.T1 * (2.0 * (R.T1 * R.T1) + 3.0 * R.T1 * R.T2 + R.T1 * R.T3 + R.T2 * R.T2 + R.T2 * R.T3))
           / 24.0,
         when 10 =>
           R.C
           * DT
           * R.T1
           * (0.0 - 2.0 * (DT * DT) - 3.0 * DT * R.T1 + 22.0 * (R.T1 * R.T1) + 36.0 * R.T1 * R.T2 + 12.0 * R.T1 * R.T3
              + 12.0 * (R.T2 * R.T2)
              + 12.0 * R.T2 * R.T3)
           / 12.0,
         when 11 =>
           R.C
           * DT
           * (DT * DT * DT - 4.0 * (DT * DT) * R.T1 - 6.0 * DT * R.T1 * (R.T1 + 2.0 * R.T2)
              + 4.0
                * R.T1
                * (11.0 * (R.T1 * R.T1) + 15.0 * R.T1 * R.T2 + 6.0 * R.T1 * R.T3 + 3.0 * (R.T2 * R.T2)
                   + 6.0 * R.T2 * R.T3))
           / 24.0,
         when 12 =>
           R.C
           * DT
           * R.T1
           * (0.0 - DT * (R.T1 + R.T2) + 2.0 * (R.T1 * R.T1) + 3.0 * R.T1 * R.T2 + 2.0 * R.T1 * R.T3 + R.T2 * R.T2
              + 2.0 * R.T2 * R.T3)
           / 2.0,
         when 13 =>
           R.C
           * DT
           * (DT * DT * DT - 12.0 * DT * R.T1 * (R.T1 + R.T2)
              + 12.0 * R.T1 * (2.0 * (R.T1 * R.T1) + 3.0 * R.T1 * R.T2 + R.T2 * R.T2))
           / 24.0,
         when 14 =>
           R.C
           * DT
           * R.T1
           * (2.0 * (DT * DT) - 3.0 * DT * (R.T1 + 2.0 * R.T2) + 2.0 * (R.T1 * R.T1) + 6.0 * R.T1 * R.T2
              + 6.0 * (R.T2 * R.T2))
           / 12.0,
         when 15 =>
           R.C
           * DT
           * (0.0 - DT * DT * DT + 4.0 * (DT * DT) * R.T1 - 6.0 * DT * (R.T1 * R.T1) + 4.0 * (R.T1 * R.T1 * R.T1))
           / 24.0);
   --  Mirrors Velocity_At_Stage of Trajectory.Profiles: what the stage adds to velocity in time DT.

   ------------------------------------------------------------------------------------------------------------------
   --  The polynomials are the integrals
   --
   --  These are identities: they hold for every ramp, every stage and every time, inside the stage or not.
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Snap_Formula (R : Ramp; Stage : Stage_Index; DT : Real)
   with Post => State_In (R, Stage, DT).Snap = Start_State (R, Stage).Snap + Snap_Increment (R, Stage, DT);

   procedure Lemma_Jerk_Formula (R : Ramp; Stage : Stage_Index; DT : Real)
   with Post => State_In (R, Stage, DT).Jerk = Start_State (R, Stage).Jerk + Jerk_Increment (R, Stage, DT);

   procedure Lemma_Accel_Formula (R : Ramp; Stage : Stage_Index; DT : Real)
   with Post => State_In (R, Stage, DT).Accel = Start_State (R, Stage).Accel + Accel_Increment (R, Stage, DT);

   procedure Lemma_Velocity_Formula (R : Ramp; Stage : Stage_Index; DT : Real)
   with Post => State_In (R, Stage, DT).Vel = Start_State (R, Stage).Vel + Velocity_Increment (R, Stage, DT);
   --  At time DT into a stage, each quantity of the model is its value at the start of the stage plus the
   --  polynomial of the code.

   procedure Lemma_Stage_Chain (R : Ramp; Stage : Stage_Index)
   with
     Pre  => Stage >= 2,
     Post =>
       Start_State (R, Stage).Snap = State_In (R, Stage - 1, Stage_Time (R, Stage - 1)).Snap
       and then Start_State (R, Stage).Jerk = State_In (R, Stage - 1, Stage_Time (R, Stage - 1)).Jerk
       and then Start_State (R, Stage).Accel = State_In (R, Stage - 1, Stage_Time (R, Stage - 1)).Accel
       and then Start_State (R, Stage).Vel = State_In (R, Stage - 1, Stage_Time (R, Stage - 1)).Vel;
   --  A stage starts where the previous stage is after its full duration. This is what the code's recursive call
   --  with T1, T2, T3 or T4 for DT computes.

end Prunt.Motion_Planner.Trajectory.Profile_Model.Formulas;
