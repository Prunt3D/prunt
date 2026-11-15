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
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile. The
--  ghost code among them computes with exact rationals and would be far too slow to run.

--  The 15-phase bounded-crackle ramp as exact mathematics, for specification and proof only.
--
--  A ramp is fifteen stages of constant crackle. Its phase times T1 to T4 give the stage durations and its crackle
--  C the stage crackles, in the pattern which the comment on Feedrate_Profile_Times in Trajectory.Profiles
--  shows. Snap, jerk and acceleration start at zero, velocity at Vs and distance at zero, and each is the integral
--  of the one before: Advance is that integration over a time of constant crackle, exactly, and Start_State applies
--  it stage after stage. Nothing here is floating point. The child packages prove what the comment in Profiles
--  only observes: the closed forms, the limits which the phase times imply, and that the ramp is monotone.

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Trajectory.Profile_Model
  with SPARK_Mode => On, Ghost, Always_Terminates
is
   use type Prunt.Motion_Planner.Reals.Real;

   type Ramp is record
      T1, T2, T3, T4 : Real;
      --  The phase times.
      C              : Real;
      --  The crackle of the first stage: positive for an acceleration ramp and negative for a deceleration ramp.
      Vs             : Real;
      --  The velocity at the start.
   end record;

   function Is_Ramp (R : Ramp) return Boolean
   is (R.T1 >= 0.0 and then R.T2 >= 0.0 and then R.T3 >= 0.0 and then R.T4 >= 0.0);

   subtype Stage_Index is Integer range 1 .. 15;
   subtype Boundary_Index is Integer range 1 .. 16;
   --  Boundary K is the start of stage K, and boundary 16 the end of the ramp.

   function Stage_Time (R : Ramp; Stage : Stage_Index) return Real
   is (case Stage is
         when 1 | 3 | 5 | 7 | 9 | 11 | 13 | 15 => R.T1,
         when 2 | 6 | 10 | 14                  => R.T2,
         when 4 | 12                           => R.T3,
         when 8                                => R.T4);

   function Stage_Crackle (R : Ramp; Stage : Stage_Index) return Real
   is (case Stage is
         when 1 | 7 | 11 | 13 => R.C,
         when 3 | 5 | 9 | 15  => 0.0 - R.C,
         when others          => 0.0);

   type State is record
      Snap, Jerk, Accel, Vel, Dist : Real;
   end record;

   function Advance (From : State; Crackle, DT : Real) return State
   is ((Snap  => From.Snap + Crackle * DT,
        Jerk  => From.Jerk + From.Snap * DT + Crackle * DT * DT / 2.0,
        Accel => From.Accel + From.Jerk * DT + From.Snap * DT * DT / 2.0 + Crackle * DT * DT * DT / 6.0,
        Vel   =>
          From.Vel + From.Accel * DT + From.Jerk * DT * DT / 2.0 + From.Snap * DT * DT * DT / 6.0
          + Crackle * DT * DT * DT * DT / 24.0,
        Dist  =>
          From.Dist + From.Vel * DT + From.Accel * DT * DT / 2.0 + From.Jerk * DT * DT * DT / 6.0
          + From.Snap * DT * DT * DT * DT / 24.0
          + Crackle * DT * DT * DT * DT * DT / 120.0));
   --  The state after time DT of constant crackle: each quantity is the exact integral of the one before.

   function Start_State (R : Ramp; Boundary : Boundary_Index) return State
   is (if Boundary = 1
       then (Snap => 0.0, Jerk => 0.0, Accel => 0.0, Vel => R.Vs, Dist => 0.0)
       else Advance (Start_State (R, Boundary - 1), Stage_Crackle (R, Boundary - 1), Stage_Time (R, Boundary - 1)))
   with Subprogram_Variant => (Decreases => Boundary);
   --  The state at the start of a stage, or at the end of the ramp.

   function State_In (R : Ramp; Stage : Stage_Index; DT : Real) return State
   is (Advance (Start_State (R, Stage), Stage_Crackle (R, Stage), DT));
   --  The state at time DT into a stage.

   function Boundary_Time (R : Ramp; Boundary : Boundary_Index) return Real
   is (case Boundary is
         when 1  => 0.0,
         when 2  => R.T1,
         when 3  => R.T1 + R.T2,
         when 4  => 2.0 * R.T1 + R.T2,
         when 5  => 2.0 * R.T1 + R.T2 + R.T3,
         when 6  => 3.0 * R.T1 + R.T2 + R.T3,
         when 7  => 3.0 * R.T1 + 2.0 * R.T2 + R.T3,
         when 8  => 4.0 * R.T1 + 2.0 * R.T2 + R.T3,
         when 9  => 4.0 * R.T1 + 2.0 * R.T2 + R.T3 + R.T4,
         when 10 => 5.0 * R.T1 + 2.0 * R.T2 + R.T3 + R.T4,
         when 11 => 5.0 * R.T1 + 3.0 * R.T2 + R.T3 + R.T4,
         when 12 => 6.0 * R.T1 + 3.0 * R.T2 + R.T3 + R.T4,
         when 13 => 6.0 * R.T1 + 3.0 * R.T2 + 2.0 * R.T3 + R.T4,
         when 14 => 7.0 * R.T1 + 3.0 * R.T2 + 2.0 * R.T3 + R.T4,
         when 15 => 7.0 * R.T1 + 4.0 * R.T2 + 2.0 * R.T3 + R.T4,
         when 16 => 8.0 * R.T1 + 4.0 * R.T2 + 2.0 * R.T3 + R.T4);
   --  The time from the start of the ramp to a boundary. These are the stage boundaries which the evaluation
   --  functions of Prunt.Motion_Planner.Trajectory.Profiles compare a time against.

   function In_Stage (R : Ramp; Stage : Stage_Index; DT : Real) return Boolean
   is (DT >= 0.0 and then DT <= Stage_Time (R, Stage));

   ------------------------------------------------------------------------------------------------------------------
   --  What the phase times imply
   ------------------------------------------------------------------------------------------------------------------

   function Peak_Snap (R : Ramp) return Real
   is (R.C * R.T1);

   function Peak_Jerk (R : Ramp) return Real
   is (R.C * R.T1 * (R.T1 + R.T2));

   function Peak_Accel (R : Ramp) return Real
   is (R.C * R.T1 * (R.T1 + R.T2) * (2.0 * R.T1 + R.T2 + R.T3));

   function Velocity_Gain (R : Ramp) return Real
   is (R.C * R.T1 * (R.T1 + R.T2) * (2.0 * R.T1 + R.T2 + R.T3) * (4.0 * R.T1 + 2.0 * R.T2 + R.T3 + R.T4));

   function Total_Time (R : Ramp) return Real
   is (8.0 * R.T1 + 4.0 * R.T2 + 2.0 * R.T3 + R.T4);

   function Travel (R : Ramp) return Real
   is ((R.Vs + Velocity_Gain (R) / 2.0) * Total_Time (R));

end Prunt.Motion_Planner.Trajectory.Profile_Model;
