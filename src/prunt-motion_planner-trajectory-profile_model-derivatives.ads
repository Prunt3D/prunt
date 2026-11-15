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

--  Snap, jerk and acceleration of the exact ramp: their values at every stage boundary, and the limits which hold
--  at every instant.

package Prunt.Motion_Planner.Trajectory.Profile_Model.Derivatives
  with SPARK_Mode => On, Ghost, Always_Terminates
is

   function Start_Snap (R : Ramp; Boundary : Boundary_Index) return Real
   is (case Boundary is
         when 2 | 3 | 14 | 15 => R.C * R.T1,
         when 6 | 7 | 10 | 11 => 0.0 - R.C * R.T1,
         when others          => 0.0);

   function First_Half_Jerk (R : Ramp; Boundary : Boundary_Index) return Real
   is (case Boundary is
         when 2 | 7  => R.C * R.T1 * R.T1 / 2.0,
         when 3 | 6  => R.C * R.T1 * (R.T1 / 2.0 + R.T2),
         when 4 | 5  => R.C * R.T1 * (R.T1 + R.T2),
         when others => 0.0)
   with Pre => Boundary <= 8;

   function Start_Jerk (R : Ramp; Boundary : Boundary_Index) return Real
   is (if Boundary <= 8 then First_Half_Jerk (R, Boundary) else 0.0 - First_Half_Jerk (R, Boundary - 8));

   function First_Half_Accel (R : Ramp; Boundary : Boundary_Index) return Real
   is (case Boundary is
         when 1      => 0.0,
         when 2      => R.C * R.T1 * R.T1 * R.T1 / 6.0,
         when 3      => R.C * R.T1 * (R.T1 * R.T1 / 6.0 + R.T1 * R.T2 / 2.0 + R.T2 * R.T2 / 2.0),
         when 4      => R.C * R.T1 * (R.T1 + R.T2) * (2.0 * R.T1 + R.T2) / 2.0,
         when 5      => R.C * R.T1 * (R.T1 + R.T2) * (2.0 * R.T1 + R.T2 + 2.0 * R.T3) / 2.0,
         when 6      =>
           R.C * R.T1 * (R.T1 + R.T2) * (2.0 * R.T1 + R.T2 + R.T3)
           - R.C * R.T1 * (R.T1 * R.T1 / 6.0 + R.T1 * R.T2 / 2.0 + R.T2 * R.T2 / 2.0),
         when 7      => R.C * R.T1 * (R.T1 + R.T2) * (2.0 * R.T1 + R.T2 + R.T3) - R.C * R.T1 * R.T1 * R.T1 / 6.0,
         when others => R.C * R.T1 * (R.T1 + R.T2) * (2.0 * R.T1 + R.T2 + R.T3))
   with Pre => Boundary <= 8;

   function Start_Accel (R : Ramp; Boundary : Boundary_Index) return Real
   is (if Boundary <= 8 then First_Half_Accel (R, Boundary) else Peak_Accel (R) - First_Half_Accel (R, Boundary - 8));
   --  The second half of a ramp mirrors the first: snap and jerk change sign and acceleration is taken from its
   --  peak.

   procedure Lemma_Start_Derivatives (R : Ramp; Boundary : Boundary_Index)
   with
     Post =>
       Start_State (R, Boundary).Snap = Start_Snap (R, Boundary)
       and then Start_State (R, Boundary).Jerk = Start_Jerk (R, Boundary)
       and then Start_State (R, Boundary).Accel = Start_Accel (R, Boundary);
   --  The closed forms above are what integrating the stages gives, for any ramp at all.

   procedure Lemma_Ends_At_Rest (R : Ramp)
   with
     Post =>
       Start_State (R, 16).Snap = 0.0 and then Start_State (R, 16).Jerk = 0.0 and then Start_State (R, 16).Accel = 0.0;
   --  A ramp ends with no snap, jerk or acceleration, so ramps and coasting join smoothly.

   procedure Lemma_Mirror (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  => Stage <= 7,
     Post =>
       State_In (R, Stage + 8, DT).Snap = 0.0 - State_In (R, Stage, DT).Snap
       and then State_In (R, Stage + 8, DT).Jerk = 0.0 - State_In (R, Stage, DT).Jerk
       and then State_In (R, Stage + 8, DT).Accel = Peak_Accel (R) - State_In (R, Stage, DT).Accel;
   --  Stages 9 to 15 repeat stages 1 to 7 with snap and jerk negated and acceleration taken from its peak.

   procedure Lemma_Accelerating_Limits (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  => Is_Ramp (R) and then R.C >= 0.0 and then In_Stage (R, Stage, DT),
     Post =>
       State_In (R, Stage, DT).Snap >= 0.0 - Peak_Snap (R)
       and then State_In (R, Stage, DT).Snap <= Peak_Snap (R)
       and then State_In (R, Stage, DT).Jerk >= 0.0 - Peak_Jerk (R)
       and then State_In (R, Stage, DT).Jerk <= Peak_Jerk (R)
       and then (if Stage <= 8 then State_In (R, Stage, DT).Jerk >= 0.0 else State_In (R, Stage, DT).Jerk <= 0.0)
       and then State_In (R, Stage, DT).Accel >= 0.0
       and then State_In (R, Stage, DT).Accel <= Peak_Accel (R);
   --  At every instant of a ramp with nonnegative crackle, snap and jerk are within their peaks, and
   --  acceleration is between zero and its peak: it rises through the first half and falls through the second.

   function Negated (R : Ramp) return Ramp
   is ((T1 => R.T1, T2 => R.T2, T3 => R.T3, T4 => R.T4, C => 0.0 - R.C, Vs => R.Vs));

   procedure Lemma_Negated (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Post =>
       State_In (Negated (R), Stage, DT).Snap = 0.0 - State_In (R, Stage, DT).Snap
       and then State_In (Negated (R), Stage, DT).Jerk = 0.0 - State_In (R, Stage, DT).Jerk
       and then State_In (Negated (R), Stage, DT).Accel = 0.0 - State_In (R, Stage, DT).Accel
       and then State_In (Negated (R), Stage, DT).Vel + State_In (R, Stage, DT).Vel = 2.0 * R.Vs
       and then
         State_In (Negated (R), Stage, DT).Dist + State_In (R, Stage, DT).Dist
         = 2.0 * R.Vs * (Boundary_Time (R, Stage) + DT);
   --  Negating the crackle of a ramp negates its snap, jerk and acceleration, and reflects its velocity about the
   --  starting velocity and its distance about travel at the starting velocity. A deceleration ramp is the
   --  negation of an acceleration ramp.

   procedure Lemma_Limits (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  => Is_Ramp (R) and then In_Stage (R, Stage, DT),
     Post =>
       abs State_In (R, Stage, DT).Snap <= abs Peak_Snap (R)
       and then abs State_In (R, Stage, DT).Jerk <= abs Peak_Jerk (R)
       and then abs State_In (R, Stage, DT).Accel <= abs Peak_Accel (R)
       and then (if R.C >= 0.0 then State_In (R, Stage, DT).Accel >= 0.0 else State_In (R, Stage, DT).Accel <= 0.0);
   --  The limits which the phase times imply, at every instant of any ramp: snap never exceeds C * T1 in
   --  magnitude, jerk never C * T1 * (T1 + T2), and acceleration never C * T1 * (T1 + T2) * (2 * T1 + T2 + T3),
   --  and acceleration never opposes the crackle of the first stage.

   ------------------------------------------------------------------------------------------------------------------
   --  One direction within a stage
   --
   --  Within a stage crackle is constant, snap keeps one sign and jerk keeps one sign. So snap, jerk and
   --  acceleration each move in one direction only from the start of a stage to its end, and each is, at every
   --  instant of the stage, between its values at the two ends: the largest magnitude which any of them has over a
   --  stage is at one of its ends. Prunt's planner takes the peaks of a ramp from the states at its stage
   --  boundaries on that ground.
   ------------------------------------------------------------------------------------------------------------------

   function Snap_Rises_In (R : Ramp; Stage : Stage_Index) return Boolean
   is (Stage_Crackle (R, Stage) >= 0.0);
   --  The direction in which snap moves through a stage is the sign of the crackle of the stage. With no crackle
   --  snap is constant.

   function Snap_Is_Nonnegative_In (R : Ramp; Stage : Stage_Index) return Boolean
   is ((R.C >= 0.0) = (Stage in 1 .. 4 | 12 .. 15));
   --  The sign which snap keeps through a stage, which is the direction in which jerk moves through it: that of
   --  the crackle of the first stage in stages 1 to 4 and 12 to 15, and the opposite in stages 5 to 11. Snap is
   --  zero throughout stages 4, 8 and 12.

   function Jerk_Is_Nonnegative_In (R : Ramp; Stage : Stage_Index) return Boolean
   is ((R.C >= 0.0) = (Stage <= 8));
   --  The sign which jerk keeps through a stage, which is the direction in which acceleration moves through it:
   --  that of the crackle of the first stage in the first half of the ramp, and the opposite in the second. Jerk
   --  is zero throughout stage 8.

   procedure Lemma_Stage_Signs (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  => Is_Ramp (R) and then In_Stage (R, Stage, DT),
     Post =>
       (if Snap_Is_Nonnegative_In (R, Stage)
        then State_In (R, Stage, DT).Snap >= 0.0
        else State_In (R, Stage, DT).Snap <= 0.0)
       and then
         (if Jerk_Is_Nonnegative_In (R, Stage)
          then State_In (R, Stage, DT).Jerk >= 0.0
          else State_In (R, Stage, DT).Jerk <= 0.0);
   --  Snap and jerk keep one sign through each stage of any ramp.

   procedure Lemma_Stage_Monotone (R : Ramp; Stage : Stage_Index; DT_1, DT_2 : Real)
   with
     Pre  => Is_Ramp (R) and then In_Stage (R, Stage, DT_1) and then In_Stage (R, Stage, DT_2) and then DT_1 <= DT_2,
     Post =>
       (if Snap_Rises_In (R, Stage)
        then State_In (R, Stage, DT_1).Snap <= State_In (R, Stage, DT_2).Snap
        else State_In (R, Stage, DT_1).Snap >= State_In (R, Stage, DT_2).Snap)
       and then
         (if Snap_Is_Nonnegative_In (R, Stage)
          then State_In (R, Stage, DT_1).Jerk <= State_In (R, Stage, DT_2).Jerk
          else State_In (R, Stage, DT_1).Jerk >= State_In (R, Stage, DT_2).Jerk)
       and then
         (if Jerk_Is_Nonnegative_In (R, Stage)
          then State_In (R, Stage, DT_1).Accel <= State_In (R, Stage, DT_2).Accel
          else State_In (R, Stage, DT_1).Accel >= State_In (R, Stage, DT_2).Accel);
   --  Between two instants of one stage of any ramp, snap moves in the direction of the crackle of the stage,
   --  jerk in the direction of the sign which snap keeps there, and acceleration in the direction of the sign
   --  which jerk keeps there. None of them turns round within a stage.

   function Is_Between (X, End_1, End_2 : Real) return Boolean
   is ((X >= End_1 and then X <= End_2) or else (X <= End_1 and then X >= End_2));

   function Larger (X, Y : Real) return Real
   is (if X >= Y then X else Y);

   procedure Lemma_Stage_Range (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  => Is_Ramp (R) and then In_Stage (R, Stage, DT),
     Post =>
       Is_Between (State_In (R, Stage, DT).Snap, Start_State (R, Stage).Snap, Start_State (R, Stage + 1).Snap)
       and then Is_Between (State_In (R, Stage, DT).Jerk, Start_State (R, Stage).Jerk, Start_State (R, Stage + 1).Jerk)
       and then
         Is_Between (State_In (R, Stage, DT).Accel, Start_State (R, Stage).Accel, Start_State (R, Stage + 1).Accel)
       and then
         abs State_In (R, Stage, DT).Snap
         <= Larger (abs Start_State (R, Stage).Snap, abs Start_State (R, Stage + 1).Snap)
       and then
         abs State_In (R, Stage, DT).Jerk
         <= Larger (abs Start_State (R, Stage).Jerk, abs Start_State (R, Stage + 1).Jerk)
       and then
         abs State_In (R, Stage, DT).Accel
         <= Larger (abs Start_State (R, Stage).Accel, abs Start_State (R, Stage + 1).Accel);
   --  At every instant of a stage of any ramp, snap, jerk and acceleration are each between their values at the
   --  start and at the end of the stage, so none exceeds in magnitude the larger of its magnitudes at the two
   --  ends.

end Prunt.Motion_Planner.Trajectory.Profile_Model.Derivatives;
