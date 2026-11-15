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

--  Velocity and distance of the exact ramp: their values where Trajectory.Profiles relies on closed forms,
--  and that a ramp is monotone.

package Prunt.Motion_Planner.Trajectory.Profile_Model.Motion
  with SPARK_Mode => On, Ghost, Always_Terminates
is

   ------------------------------------------------------------------------------------------------------------------
   --  Closed forms
   --
   --  Distance_At_Time in Trajectory.Profiles starts from the state at the start of stage 4, 8 or 12 instead
   --  of integrating from the start of the ramp. These are the expressions it uses, which leave out the starting
   --  velocity and the distance travelled at it.
   ------------------------------------------------------------------------------------------------------------------

   function Checkpoint_4_Vel (R : Ramp) return Real
   is (R.C * R.T1 * (R.T1 + R.T2) * (7.0 * R.T1 * R.T1 + 7.0 * R.T1 * R.T2 + 2.0 * R.T2 * R.T2) / 12.0);

   function Checkpoint_4_Dist (R : Ramp) return Real
   is (R.C
       * R.T1
       * (R.T1 + R.T2)
       * (2.0 * R.T1 + R.T2)
       * (3.0 * R.T1 * R.T1 + 3.0 * R.T1 * R.T2 + R.T2 * R.T2)
       / 24.0);

   function Checkpoint_8_Vel (R : Ramp) return Real
   is (R.C * R.T1 * (R.T1 + R.T2) * (2.0 * R.T1 + R.T2 + R.T3) * (4.0 * R.T1 + 2.0 * R.T2 + R.T3) / 2.0);

   function Checkpoint_8_Dist (R : Ramp) return Real
   is (R.C
       * R.T1
       * (R.T1 + R.T2)
       * (2.0 * R.T1 + R.T2 + R.T3)
       * (27.0 * R.T1 * R.T1 + 27.0 * R.T1 * R.T2 + 14.0 * R.T1 * R.T3 + 7.0 * R.T2 * R.T2 + 7.0 * R.T2 * R.T3
          + 2.0 * R.T3 * R.T3)
       / 12.0);

   function Checkpoint_12_Vel (R : Ramp) return Real
   is (R.C
       * R.T1
       * (R.T1 + R.T2)
       * (89.0 * R.T1 * R.T1 + 89.0 * R.T1 * R.T2 + 60.0 * R.T1 * R.T3 + 24.0 * R.T1 * R.T4 + 22.0 * R.T2 * R.T2
          + 30.0 * R.T2 * R.T3
          + 12.0 * R.T2 * R.T4
          + 6.0 * R.T3 * R.T3
          + 12.0 * R.T3 * R.T4)
       / 12.0);

   function Checkpoint_12_Dist (R : Ramp) return Real
   is (R.C
       * R.T1
       * (R.T1 + R.T2)
       * (390.0 * R.T1 * R.T1 * R.T1 + 585.0 * R.T1 * R.T1 * R.T2 + 302.0 * R.T1 * R.T1 * R.T3
          + 192.0 * R.T1 * R.T1 * R.T4
          + 293.0 * R.T1 * R.T2 * R.T2
          + 302.0 * R.T1 * R.T2 * R.T3
          + 192.0 * R.T1 * R.T2 * R.T4
          + 60.0 * R.T1 * R.T3 * R.T3
          + 120.0 * R.T1 * R.T3 * R.T4
          + 24.0 * R.T1 * R.T4 * R.T4
          + 49.0 * R.T2 * R.T2 * R.T2
          + 76.0 * R.T2 * R.T2 * R.T3
          + 48.0 * R.T2 * R.T2 * R.T4
          + 30.0 * R.T2 * R.T3 * R.T3
          + 60.0 * R.T2 * R.T3 * R.T4
          + 12.0 * R.T2 * R.T4 * R.T4
          + 4.0 * R.T3 * R.T3 * R.T3
          + 12.0 * R.T3 * R.T3 * R.T4
          + 12.0 * R.T3 * R.T4 * R.T4)
       / 24.0);

   procedure Lemma_Checkpoint_4 (R : Ramp)
   with
     Post =>
       Start_State (R, 4).Vel = R.Vs + Checkpoint_4_Vel (R)
       and then Start_State (R, 4).Dist = R.Vs * Boundary_Time (R, 4) + Checkpoint_4_Dist (R);

   procedure Lemma_Checkpoint_8 (R : Ramp)
   with
     Post =>
       Start_State (R, 8).Vel = R.Vs + Checkpoint_8_Vel (R)
       and then Start_State (R, 8).Dist = R.Vs * Boundary_Time (R, 8) + Checkpoint_8_Dist (R);

   procedure Lemma_Checkpoint_12 (R : Ramp)
   with
     Post =>
       Start_State (R, 12).Vel = R.Vs + Checkpoint_12_Vel (R)
       and then Start_State (R, 12).Dist = R.Vs * Boundary_Time (R, 12) + Checkpoint_12_Dist (R);

   procedure Lemma_End (R : Ramp)
   with
     Post =>
       Start_State (R, 16).Vel = R.Vs + Velocity_Gain (R)
       and then Start_State (R, 16).Dist = Travel (R)
       and then Boundary_Time (R, 16) = Total_Time (R);
   --  What Fast_Velocity_At_Max_Time and Fast_Distance_At_Max_Time compute is the end of the ramp: its speed has
   --  changed by C * T1 * (T1 + T2) * (2 * T1 + T2 + T3) * (4 * T1 + 2 * T2 + T3 + T4), and it has travelled its
   --  duration at the mean of its starting and final speeds.

   ------------------------------------------------------------------------------------------------------------------
   --  Monotone motion
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Velocity_Range (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  => Is_Ramp (R) and then In_Stage (R, Stage, DT),
     Post =>
       (if R.C >= 0.0
        then State_In (R, Stage, DT).Vel >= R.Vs and then State_In (R, Stage, DT).Vel <= R.Vs + Velocity_Gain (R)
        else State_In (R, Stage, DT).Vel <= R.Vs and then State_In (R, Stage, DT).Vel >= R.Vs + Velocity_Gain (R));
   --  At every instant of a ramp the velocity is between its starting and final values: a ramp never overshoots
   --  and never moves against its own direction of change.

   procedure Lemma_Distance_Range (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  =>
       Is_Ramp (R) and then In_Stage (R, Stage, DT) and then R.Vs >= 0.0 and then R.Vs + Velocity_Gain (R) >= 0.0,
     Post => State_In (R, Stage, DT).Dist >= 0.0 and then State_In (R, Stage, DT).Dist <= Travel (R);
   --  A ramp between two nonnegative speeds never moves backwards, so at every instant its distance is between
   --  zero and its travel.

   procedure Lemma_Velocity_Monotone (R : Ramp; Stage : Stage_Index; DT_1, DT_2 : Real)
   with
     Pre  => Is_Ramp (R) and then In_Stage (R, Stage, DT_1) and then In_Stage (R, Stage, DT_2) and then DT_1 <= DT_2,
     Post =>
       (if R.C >= 0.0
        then State_In (R, Stage, DT_1).Vel <= State_In (R, Stage, DT_2).Vel
        else State_In (R, Stage, DT_1).Vel >= State_In (R, Stage, DT_2).Vel);
   --  Between two instants of one stage the velocity never moves against the ramp's direction of change.

   procedure Lemma_Start_Velocity_Monotone (R : Ramp; From, To : Boundary_Index)
   with
     Pre                => Is_Ramp (R) and then From <= To,
     Post               =>
       (if R.C >= 0.0
        then Start_State (R, From).Vel <= Start_State (R, To).Vel
        else Start_State (R, From).Vel >= Start_State (R, To).Vel),
     Subprogram_Variant => (Decreases => To);
   --  Nor does it from one boundary to a later one.

   procedure Lemma_Distance_Monotone (R : Ramp; Stage : Stage_Index; DT_1, DT_2 : Real)
   with
     Pre  =>
       Is_Ramp (R)
       and then In_Stage (R, Stage, DT_1)
       and then In_Stage (R, Stage, DT_2)
       and then DT_1 <= DT_2
       and then R.Vs >= 0.0
       and then R.Vs + Velocity_Gain (R) >= 0.0,
     Post => State_In (R, Stage, DT_1).Dist <= State_In (R, Stage, DT_2).Dist;
   --  Between two instants of one stage a ramp between two nonnegative speeds never moves backwards.

   procedure Lemma_Start_Distance_Monotone (R : Ramp; From, To : Boundary_Index)
   with
     Pre                =>
       Is_Ramp (R) and then From <= To and then R.Vs >= 0.0 and then R.Vs + Velocity_Gain (R) >= 0.0,
     Post               => Start_State (R, From).Dist <= Start_State (R, To).Dist,
     Subprogram_Variant => (Decreases => To);
   --  Nor does it from one boundary to a later one.

end Prunt.Motion_Planner.Trajectory.Profile_Model.Motion;
