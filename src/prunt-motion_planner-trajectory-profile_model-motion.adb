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

pragma Style_Checks ("-s");
--  Lemmas are declared by their bodies, where they are proved: a separate declaration would only repeat them.

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

with Prunt.Motion_Planner.Trajectory.Profile_Model.Derivatives;
use Prunt.Motion_Planner.Trajectory.Profile_Model.Derivatives;

package body Prunt.Motion_Planner.Trajectory.Profile_Model.Motion
  with SPARK_Mode => On
is

   ------------------------------------------------------------------------------------------------------------------
   --  Closed forms
   --
   --  Each closed form which the spec names is proved from the one before it, through the stages between them.
   --
   --  Each Lemma_Stages is the polynomial identity of those stages. It takes the state at the start of every one of
   --  them as plain numbers which its precondition defines one after another: snap, jerk and acceleration by the
   --  closed forms of Derivatives, velocity and distance as Advance gives them from the stage before. A prover
   --  substitutes the definitions and is left with a polynomial to normalise. The lemma of the spec then only has to
   --  match the definitions against Start_State.
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Next_Start (R : Ramp; Stage : Stage_Index)
   with
     Post =>
       Start_State (R, Stage + 1).Vel
       = Start_State (R, Stage).Vel + Start_State (R, Stage).Accel * Stage_Time (R, Stage)
         + Start_State (R, Stage).Jerk * Stage_Time (R, Stage) * Stage_Time (R, Stage) / 2.0
         + Start_State (R, Stage).Snap * Stage_Time (R, Stage) * Stage_Time (R, Stage) * Stage_Time (R, Stage) / 6.0
         + Stage_Crackle (R, Stage)
           * Stage_Time (R, Stage)
           * Stage_Time (R, Stage)
           * Stage_Time (R, Stage)
           * Stage_Time (R, Stage)
           / 24.0
       and then
         Start_State (R, Stage + 1).Dist
         = Start_State (R, Stage).Dist + Start_State (R, Stage).Vel * Stage_Time (R, Stage)
           + Start_State (R, Stage).Accel * Stage_Time (R, Stage) * Stage_Time (R, Stage) / 2.0
           + Start_State (R, Stage).Jerk * Stage_Time (R, Stage) * Stage_Time (R, Stage) * Stage_Time (R, Stage) / 6.0
           + Start_State (R, Stage).Snap
             * Stage_Time (R, Stage)
             * Stage_Time (R, Stage)
             * Stage_Time (R, Stage)
             * Stage_Time (R, Stage)
             / 24.0
           + Stage_Crackle (R, Stage)
             * Stage_Time (R, Stage)
             * Stage_Time (R, Stage)
             * Stage_Time (R, Stage)
             * Stage_Time (R, Stage)
             * Stage_Time (R, Stage)
             / 120.0
   is
   begin
      null;
   end Lemma_Next_Start;
   --  Velocity and distance at the next boundary, as Advance gives them from the state at this one.

   procedure Lemma_Stages_1_To_3
     (T1, T2, T3, T4, C, Vs                                     : Real;
      Time_1, Crackle_1, Snap_1, Jerk_1, Accel_1, Vel_1, Dist_1 : Real;
      Time_2, Crackle_2, Snap_2, Jerk_2, Accel_2, Vel_2, Dist_2 : Real;
      Time_3, Crackle_3, Snap_3, Jerk_3, Accel_3, Vel_3, Dist_3 : Real;
      Vel_4, Dist_4                                             : Real)
   with
     Pre  =>
       Vel_1 = Vs
       and then Dist_1 = 0.0
       and then Time_1 = T1
       and then Crackle_1 = C
       and then Snap_1 = 0.0
       and then Jerk_1 = 0.0
       and then Accel_1 = 0.0
       and then
         Vel_2
         = Vel_1 + Accel_1 * Time_1 + Jerk_1 * Time_1 * Time_1 / 2.0 + Snap_1 * Time_1 * Time_1 * Time_1 / 6.0
           + Crackle_1 * Time_1 * Time_1 * Time_1 * Time_1 / 24.0
       and then
         Dist_2
         = Dist_1 + Vel_1 * Time_1 + Accel_1 * Time_1 * Time_1 / 2.0 + Jerk_1 * Time_1 * Time_1 * Time_1 / 6.0
           + Snap_1 * Time_1 * Time_1 * Time_1 * Time_1 / 24.0
           + Crackle_1 * Time_1 * Time_1 * Time_1 * Time_1 * Time_1 / 120.0
       and then Time_2 = T2
       and then Crackle_2 = 0.0
       and then Snap_2 = C * T1
       and then Jerk_2 = C * T1 * T1 / 2.0
       and then Accel_2 = C * T1 * T1 * T1 / 6.0
       and then
         Vel_3
         = Vel_2 + Accel_2 * Time_2 + Jerk_2 * Time_2 * Time_2 / 2.0 + Snap_2 * Time_2 * Time_2 * Time_2 / 6.0
           + Crackle_2 * Time_2 * Time_2 * Time_2 * Time_2 / 24.0
       and then
         Dist_3
         = Dist_2 + Vel_2 * Time_2 + Accel_2 * Time_2 * Time_2 / 2.0 + Jerk_2 * Time_2 * Time_2 * Time_2 / 6.0
           + Snap_2 * Time_2 * Time_2 * Time_2 * Time_2 / 24.0
           + Crackle_2 * Time_2 * Time_2 * Time_2 * Time_2 * Time_2 / 120.0
       and then Time_3 = T1
       and then Crackle_3 = 0.0 - C
       and then Snap_3 = C * T1
       and then Jerk_3 = C * T1 * (T1 / 2.0 + T2)
       and then Accel_3 = C * T1 * (T1 * T1 / 6.0 + T1 * T2 / 2.0 + T2 * T2 / 2.0)
       and then
         Vel_4
         = Vel_3 + Accel_3 * Time_3 + Jerk_3 * Time_3 * Time_3 / 2.0 + Snap_3 * Time_3 * Time_3 * Time_3 / 6.0
           + Crackle_3 * Time_3 * Time_3 * Time_3 * Time_3 / 24.0
       and then
         Dist_4
         = Dist_3 + Vel_3 * Time_3 + Accel_3 * Time_3 * Time_3 / 2.0 + Jerk_3 * Time_3 * Time_3 * Time_3 / 6.0
           + Snap_3 * Time_3 * Time_3 * Time_3 * Time_3 / 24.0
           + Crackle_3 * Time_3 * Time_3 * Time_3 * Time_3 * Time_3 / 120.0,
     Post =>
       Vel_4 = Vs + C * T1 * (T1 + T2) * (7.0 * T1 * T1 + 7.0 * T1 * T2 + 2.0 * T2 * T2) / 12.0
       and then
         Dist_4
         = Vs * (2.0 * T1 + T2)
           + C * T1 * (T1 + T2) * (2.0 * T1 + T2) * (3.0 * T1 * T1 + 3.0 * T1 * T2 + T2 * T2) / 24.0
   is
   begin
      null;
   end Lemma_Stages_1_To_3;

   procedure Lemma_Checkpoint_4 (R : Ramp) is
   begin
      Lemma_Start_Derivatives (R, 1);
      Lemma_Next_Start (R, 1);
      Lemma_Start_Derivatives (R, 2);
      Lemma_Next_Start (R, 2);
      Lemma_Start_Derivatives (R, 3);
      Lemma_Next_Start (R, 3);
      Lemma_Stages_1_To_3
        (R.T1,
         R.T2,
         R.T3,
         R.T4,
         R.C,
         R.Vs,
         Stage_Time (R, 1),
         Stage_Crackle (R, 1),
         Start_State (R, 1).Snap,
         Start_State (R, 1).Jerk,
         Start_State (R, 1).Accel,
         Start_State (R, 1).Vel,
         Start_State (R, 1).Dist,
         Stage_Time (R, 2),
         Stage_Crackle (R, 2),
         Start_State (R, 2).Snap,
         Start_State (R, 2).Jerk,
         Start_State (R, 2).Accel,
         Start_State (R, 2).Vel,
         Start_State (R, 2).Dist,
         Stage_Time (R, 3),
         Stage_Crackle (R, 3),
         Start_State (R, 3).Snap,
         Start_State (R, 3).Jerk,
         Start_State (R, 3).Accel,
         Start_State (R, 3).Vel,
         Start_State (R, 3).Dist,
         Start_State (R, 4).Vel,
         Start_State (R, 4).Dist);
   end Lemma_Checkpoint_4;

   procedure Lemma_Stages_4_To_7
     (T1, T2, T3, T4, C, Vs                                     : Real;
      Time_4, Crackle_4, Snap_4, Jerk_4, Accel_4, Vel_4, Dist_4 : Real;
      Time_5, Crackle_5, Snap_5, Jerk_5, Accel_5, Vel_5, Dist_5 : Real;
      Time_6, Crackle_6, Snap_6, Jerk_6, Accel_6, Vel_6, Dist_6 : Real;
      Time_7, Crackle_7, Snap_7, Jerk_7, Accel_7, Vel_7, Dist_7 : Real;
      Vel_8, Dist_8                                             : Real)
   with
     Pre  =>
       Vel_4 = Vs + C * T1 * (T1 + T2) * (7.0 * T1 * T1 + 7.0 * T1 * T2 + 2.0 * T2 * T2) / 12.0
       and then
         Dist_4
         = Vs * (2.0 * T1 + T2)
           + C * T1 * (T1 + T2) * (2.0 * T1 + T2) * (3.0 * T1 * T1 + 3.0 * T1 * T2 + T2 * T2) / 24.0
       and then Time_4 = T3
       and then Crackle_4 = 0.0
       and then Snap_4 = 0.0
       and then Jerk_4 = C * T1 * (T1 + T2)
       and then Accel_4 = C * T1 * (T1 + T2) * (2.0 * T1 + T2) / 2.0
       and then
         Vel_5
         = Vel_4 + Accel_4 * Time_4 + Jerk_4 * Time_4 * Time_4 / 2.0 + Snap_4 * Time_4 * Time_4 * Time_4 / 6.0
           + Crackle_4 * Time_4 * Time_4 * Time_4 * Time_4 / 24.0
       and then
         Dist_5
         = Dist_4 + Vel_4 * Time_4 + Accel_4 * Time_4 * Time_4 / 2.0 + Jerk_4 * Time_4 * Time_4 * Time_4 / 6.0
           + Snap_4 * Time_4 * Time_4 * Time_4 * Time_4 / 24.0
           + Crackle_4 * Time_4 * Time_4 * Time_4 * Time_4 * Time_4 / 120.0
       and then Time_5 = T1
       and then Crackle_5 = 0.0 - C
       and then Snap_5 = 0.0
       and then Jerk_5 = C * T1 * (T1 + T2)
       and then Accel_5 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + 2.0 * T3) / 2.0
       and then
         Vel_6
         = Vel_5 + Accel_5 * Time_5 + Jerk_5 * Time_5 * Time_5 / 2.0 + Snap_5 * Time_5 * Time_5 * Time_5 / 6.0
           + Crackle_5 * Time_5 * Time_5 * Time_5 * Time_5 / 24.0
       and then
         Dist_6
         = Dist_5 + Vel_5 * Time_5 + Accel_5 * Time_5 * Time_5 / 2.0 + Jerk_5 * Time_5 * Time_5 * Time_5 / 6.0
           + Snap_5 * Time_5 * Time_5 * Time_5 * Time_5 / 24.0
           + Crackle_5 * Time_5 * Time_5 * Time_5 * Time_5 * Time_5 / 120.0
       and then Time_6 = T2
       and then Crackle_6 = 0.0
       and then Snap_6 = 0.0 - C * T1
       and then Jerk_6 = C * T1 * (T1 / 2.0 + T2)
       and then
         Accel_6 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - C * T1 * (T1 * T1 / 6.0 + T1 * T2 / 2.0 + T2 * T2 / 2.0)
       and then
         Vel_7
         = Vel_6 + Accel_6 * Time_6 + Jerk_6 * Time_6 * Time_6 / 2.0 + Snap_6 * Time_6 * Time_6 * Time_6 / 6.0
           + Crackle_6 * Time_6 * Time_6 * Time_6 * Time_6 / 24.0
       and then
         Dist_7
         = Dist_6 + Vel_6 * Time_6 + Accel_6 * Time_6 * Time_6 / 2.0 + Jerk_6 * Time_6 * Time_6 * Time_6 / 6.0
           + Snap_6 * Time_6 * Time_6 * Time_6 * Time_6 / 24.0
           + Crackle_6 * Time_6 * Time_6 * Time_6 * Time_6 * Time_6 / 120.0
       and then Time_7 = T1
       and then Crackle_7 = C
       and then Snap_7 = 0.0 - C * T1
       and then Jerk_7 = C * T1 * T1 / 2.0
       and then Accel_7 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - C * T1 * T1 * T1 / 6.0
       and then
         Vel_8
         = Vel_7 + Accel_7 * Time_7 + Jerk_7 * Time_7 * Time_7 / 2.0 + Snap_7 * Time_7 * Time_7 * Time_7 / 6.0
           + Crackle_7 * Time_7 * Time_7 * Time_7 * Time_7 / 24.0
       and then
         Dist_8
         = Dist_7 + Vel_7 * Time_7 + Accel_7 * Time_7 * Time_7 / 2.0 + Jerk_7 * Time_7 * Time_7 * Time_7 / 6.0
           + Snap_7 * Time_7 * Time_7 * Time_7 * Time_7 / 24.0
           + Crackle_7 * Time_7 * Time_7 * Time_7 * Time_7 * Time_7 / 120.0,
     Post =>
       Vel_8 = Vs + C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3) / 2.0
       and then
         Dist_8
         = Vs * (4.0 * T1 + 2.0 * T2 + T3)
           + C
             * T1
             * (T1 + T2)
             * (2.0 * T1 + T2 + T3)
             * (27.0 * T1 * T1 + 27.0 * T1 * T2 + 14.0 * T1 * T3 + 7.0 * T2 * T2 + 7.0 * T2 * T3 + 2.0 * T3 * T3)
             / 12.0
   is
   begin
      null;
   end Lemma_Stages_4_To_7;

   procedure Lemma_Checkpoint_8 (R : Ramp) is
   begin
      Lemma_Checkpoint_4 (R);
      Lemma_Start_Derivatives (R, 4);
      Lemma_Next_Start (R, 4);
      Lemma_Start_Derivatives (R, 5);
      Lemma_Next_Start (R, 5);
      Lemma_Start_Derivatives (R, 6);
      Lemma_Next_Start (R, 6);
      Lemma_Start_Derivatives (R, 7);
      Lemma_Next_Start (R, 7);
      Lemma_Stages_4_To_7
        (R.T1,
         R.T2,
         R.T3,
         R.T4,
         R.C,
         R.Vs,
         Stage_Time (R, 4),
         Stage_Crackle (R, 4),
         Start_State (R, 4).Snap,
         Start_State (R, 4).Jerk,
         Start_State (R, 4).Accel,
         Start_State (R, 4).Vel,
         Start_State (R, 4).Dist,
         Stage_Time (R, 5),
         Stage_Crackle (R, 5),
         Start_State (R, 5).Snap,
         Start_State (R, 5).Jerk,
         Start_State (R, 5).Accel,
         Start_State (R, 5).Vel,
         Start_State (R, 5).Dist,
         Stage_Time (R, 6),
         Stage_Crackle (R, 6),
         Start_State (R, 6).Snap,
         Start_State (R, 6).Jerk,
         Start_State (R, 6).Accel,
         Start_State (R, 6).Vel,
         Start_State (R, 6).Dist,
         Stage_Time (R, 7),
         Stage_Crackle (R, 7),
         Start_State (R, 7).Snap,
         Start_State (R, 7).Jerk,
         Start_State (R, 7).Accel,
         Start_State (R, 7).Vel,
         Start_State (R, 7).Dist,
         Start_State (R, 8).Vel,
         Start_State (R, 8).Dist);
   end Lemma_Checkpoint_8;

   procedure Lemma_Stages_8_To_11
     (T1, T2, T3, T4, C, Vs                                            : Real;
      Time_8, Crackle_8, Snap_8, Jerk_8, Accel_8, Vel_8, Dist_8        : Real;
      Time_9, Crackle_9, Snap_9, Jerk_9, Accel_9, Vel_9, Dist_9        : Real;
      Time_10, Crackle_10, Snap_10, Jerk_10, Accel_10, Vel_10, Dist_10 : Real;
      Time_11, Crackle_11, Snap_11, Jerk_11, Accel_11, Vel_11, Dist_11 : Real;
      Vel_12, Dist_12                                                  : Real)
   with
     Pre  =>
       Vel_8 = Vs + C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3) / 2.0
       and then
         Dist_8
         = Vs * (4.0 * T1 + 2.0 * T2 + T3)
           + C
             * T1
             * (T1 + T2)
             * (2.0 * T1 + T2 + T3)
             * (27.0 * T1 * T1 + 27.0 * T1 * T2 + 14.0 * T1 * T3 + 7.0 * T2 * T2 + 7.0 * T2 * T3 + 2.0 * T3 * T3)
             / 12.0
       and then Time_8 = T4
       and then Crackle_8 = 0.0
       and then Snap_8 = 0.0
       and then Jerk_8 = 0.0
       and then Accel_8 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3)
       and then
         Vel_9
         = Vel_8 + Accel_8 * Time_8 + Jerk_8 * Time_8 * Time_8 / 2.0 + Snap_8 * Time_8 * Time_8 * Time_8 / 6.0
           + Crackle_8 * Time_8 * Time_8 * Time_8 * Time_8 / 24.0
       and then
         Dist_9
         = Dist_8 + Vel_8 * Time_8 + Accel_8 * Time_8 * Time_8 / 2.0 + Jerk_8 * Time_8 * Time_8 * Time_8 / 6.0
           + Snap_8 * Time_8 * Time_8 * Time_8 * Time_8 / 24.0
           + Crackle_8 * Time_8 * Time_8 * Time_8 * Time_8 * Time_8 / 120.0
       and then Time_9 = T1
       and then Crackle_9 = 0.0 - C
       and then Snap_9 = 0.0
       and then Jerk_9 = 0.0 - 0.0
       and then Accel_9 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - (0.0)
       and then
         Vel_10
         = Vel_9 + Accel_9 * Time_9 + Jerk_9 * Time_9 * Time_9 / 2.0 + Snap_9 * Time_9 * Time_9 * Time_9 / 6.0
           + Crackle_9 * Time_9 * Time_9 * Time_9 * Time_9 / 24.0
       and then
         Dist_10
         = Dist_9 + Vel_9 * Time_9 + Accel_9 * Time_9 * Time_9 / 2.0 + Jerk_9 * Time_9 * Time_9 * Time_9 / 6.0
           + Snap_9 * Time_9 * Time_9 * Time_9 * Time_9 / 24.0
           + Crackle_9 * Time_9 * Time_9 * Time_9 * Time_9 * Time_9 / 120.0
       and then Time_10 = T2
       and then Crackle_10 = 0.0
       and then Snap_10 = 0.0 - C * T1
       and then Jerk_10 = 0.0 - C * T1 * T1 / 2.0
       and then Accel_10 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - (C * T1 * T1 * T1 / 6.0)
       and then
         Vel_11
         = Vel_10 + Accel_10 * Time_10 + Jerk_10 * Time_10 * Time_10 / 2.0
           + Snap_10 * Time_10 * Time_10 * Time_10 / 6.0
           + Crackle_10 * Time_10 * Time_10 * Time_10 * Time_10 / 24.0
       and then
         Dist_11
         = Dist_10 + Vel_10 * Time_10 + Accel_10 * Time_10 * Time_10 / 2.0
           + Jerk_10 * Time_10 * Time_10 * Time_10 / 6.0
           + Snap_10 * Time_10 * Time_10 * Time_10 * Time_10 / 24.0
           + Crackle_10 * Time_10 * Time_10 * Time_10 * Time_10 * Time_10 / 120.0
       and then Time_11 = T1
       and then Crackle_11 = C
       and then Snap_11 = 0.0 - C * T1
       and then Jerk_11 = 0.0 - C * T1 * (T1 / 2.0 + T2)
       and then
         Accel_11
         = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - (C * T1 * (T1 * T1 / 6.0 + T1 * T2 / 2.0 + T2 * T2 / 2.0))
       and then
         Vel_12
         = Vel_11 + Accel_11 * Time_11 + Jerk_11 * Time_11 * Time_11 / 2.0
           + Snap_11 * Time_11 * Time_11 * Time_11 / 6.0
           + Crackle_11 * Time_11 * Time_11 * Time_11 * Time_11 / 24.0
       and then
         Dist_12
         = Dist_11 + Vel_11 * Time_11 + Accel_11 * Time_11 * Time_11 / 2.0
           + Jerk_11 * Time_11 * Time_11 * Time_11 / 6.0
           + Snap_11 * Time_11 * Time_11 * Time_11 * Time_11 / 24.0
           + Crackle_11 * Time_11 * Time_11 * Time_11 * Time_11 * Time_11 / 120.0,
     Post =>
       Vel_12
       = Vs
         + C
           * T1
           * (T1 + T2)
           * (89.0 * T1 * T1 + 89.0 * T1 * T2 + 60.0 * T1 * T3 + 24.0 * T1 * T4 + 22.0 * T2 * T2 + 30.0 * T2 * T3
              + 12.0 * T2 * T4
              + 6.0 * T3 * T3
              + 12.0 * T3 * T4)
           / 12.0
       and then
         Dist_12
         = Vs * (6.0 * T1 + 3.0 * T2 + T3 + T4)
           + C
             * T1
             * (T1 + T2)
             * (390.0 * T1 * T1 * T1 + 585.0 * T1 * T1 * T2 + 302.0 * T1 * T1 * T3 + 192.0 * T1 * T1 * T4
                + 293.0 * T1 * T2 * T2
                + 302.0 * T1 * T2 * T3
                + 192.0 * T1 * T2 * T4
                + 60.0 * T1 * T3 * T3
                + 120.0 * T1 * T3 * T4
                + 24.0 * T1 * T4 * T4
                + 49.0 * T2 * T2 * T2
                + 76.0 * T2 * T2 * T3
                + 48.0 * T2 * T2 * T4
                + 30.0 * T2 * T3 * T3
                + 60.0 * T2 * T3 * T4
                + 12.0 * T2 * T4 * T4
                + 4.0 * T3 * T3 * T3
                + 12.0 * T3 * T3 * T4
                + 12.0 * T3 * T4 * T4)
             / 24.0
   is
   begin
      null;
   end Lemma_Stages_8_To_11;

   procedure Lemma_Checkpoint_12 (R : Ramp) is
   begin
      Lemma_Checkpoint_8 (R);
      Lemma_Start_Derivatives (R, 8);
      Lemma_Next_Start (R, 8);
      Lemma_Start_Derivatives (R, 9);
      Lemma_Next_Start (R, 9);
      Lemma_Start_Derivatives (R, 10);
      Lemma_Next_Start (R, 10);
      Lemma_Start_Derivatives (R, 11);
      Lemma_Next_Start (R, 11);
      Lemma_Stages_8_To_11
        (R.T1,
         R.T2,
         R.T3,
         R.T4,
         R.C,
         R.Vs,
         Stage_Time (R, 8),
         Stage_Crackle (R, 8),
         Start_State (R, 8).Snap,
         Start_State (R, 8).Jerk,
         Start_State (R, 8).Accel,
         Start_State (R, 8).Vel,
         Start_State (R, 8).Dist,
         Stage_Time (R, 9),
         Stage_Crackle (R, 9),
         Start_State (R, 9).Snap,
         Start_State (R, 9).Jerk,
         Start_State (R, 9).Accel,
         Start_State (R, 9).Vel,
         Start_State (R, 9).Dist,
         Stage_Time (R, 10),
         Stage_Crackle (R, 10),
         Start_State (R, 10).Snap,
         Start_State (R, 10).Jerk,
         Start_State (R, 10).Accel,
         Start_State (R, 10).Vel,
         Start_State (R, 10).Dist,
         Stage_Time (R, 11),
         Stage_Crackle (R, 11),
         Start_State (R, 11).Snap,
         Start_State (R, 11).Jerk,
         Start_State (R, 11).Accel,
         Start_State (R, 11).Vel,
         Start_State (R, 11).Dist,
         Start_State (R, 12).Vel,
         Start_State (R, 12).Dist);
   end Lemma_Checkpoint_12;

   procedure Lemma_Stages_12_To_15
     (T1, T2, T3, T4, C, Vs                                            : Real;
      Time_12, Crackle_12, Snap_12, Jerk_12, Accel_12, Vel_12, Dist_12 : Real;
      Time_13, Crackle_13, Snap_13, Jerk_13, Accel_13, Vel_13, Dist_13 : Real;
      Time_14, Crackle_14, Snap_14, Jerk_14, Accel_14, Vel_14, Dist_14 : Real;
      Time_15, Crackle_15, Snap_15, Jerk_15, Accel_15, Vel_15, Dist_15 : Real;
      Vel_16, Dist_16, Gain, Total                                     : Real)
   with
     Pre  =>
       Vel_12
       = Vs
         + C
           * T1
           * (T1 + T2)
           * (89.0 * T1 * T1 + 89.0 * T1 * T2 + 60.0 * T1 * T3 + 24.0 * T1 * T4 + 22.0 * T2 * T2 + 30.0 * T2 * T3
              + 12.0 * T2 * T4
              + 6.0 * T3 * T3
              + 12.0 * T3 * T4)
           / 12.0
       and then
         Dist_12
         = Vs * (6.0 * T1 + 3.0 * T2 + T3 + T4)
           + C
             * T1
             * (T1 + T2)
             * (390.0 * T1 * T1 * T1 + 585.0 * T1 * T1 * T2 + 302.0 * T1 * T1 * T3 + 192.0 * T1 * T1 * T4
                + 293.0 * T1 * T2 * T2
                + 302.0 * T1 * T2 * T3
                + 192.0 * T1 * T2 * T4
                + 60.0 * T1 * T3 * T3
                + 120.0 * T1 * T3 * T4
                + 24.0 * T1 * T4 * T4
                + 49.0 * T2 * T2 * T2
                + 76.0 * T2 * T2 * T3
                + 48.0 * T2 * T2 * T4
                + 30.0 * T2 * T3 * T3
                + 60.0 * T2 * T3 * T4
                + 12.0 * T2 * T4 * T4
                + 4.0 * T3 * T3 * T3
                + 12.0 * T3 * T3 * T4
                + 12.0 * T3 * T4 * T4)
             / 24.0
       and then Time_12 = T3
       and then Crackle_12 = 0.0
       and then Snap_12 = 0.0
       and then Jerk_12 = 0.0 - C * T1 * (T1 + T2)
       and then Accel_12 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - (C * T1 * (T1 + T2) * (2.0 * T1 + T2) / 2.0)
       and then
         Vel_13
         = Vel_12 + Accel_12 * Time_12 + Jerk_12 * Time_12 * Time_12 / 2.0
           + Snap_12 * Time_12 * Time_12 * Time_12 / 6.0
           + Crackle_12 * Time_12 * Time_12 * Time_12 * Time_12 / 24.0
       and then
         Dist_13
         = Dist_12 + Vel_12 * Time_12 + Accel_12 * Time_12 * Time_12 / 2.0
           + Jerk_12 * Time_12 * Time_12 * Time_12 / 6.0
           + Snap_12 * Time_12 * Time_12 * Time_12 * Time_12 / 24.0
           + Crackle_12 * Time_12 * Time_12 * Time_12 * Time_12 * Time_12 / 120.0
       and then Time_13 = T1
       and then Crackle_13 = C
       and then Snap_13 = 0.0
       and then Jerk_13 = 0.0 - C * T1 * (T1 + T2)
       and then
         Accel_13 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - (C * T1 * (T1 + T2) * (2.0 * T1 + T2 + 2.0 * T3) / 2.0)
       and then
         Vel_14
         = Vel_13 + Accel_13 * Time_13 + Jerk_13 * Time_13 * Time_13 / 2.0
           + Snap_13 * Time_13 * Time_13 * Time_13 / 6.0
           + Crackle_13 * Time_13 * Time_13 * Time_13 * Time_13 / 24.0
       and then
         Dist_14
         = Dist_13 + Vel_13 * Time_13 + Accel_13 * Time_13 * Time_13 / 2.0
           + Jerk_13 * Time_13 * Time_13 * Time_13 / 6.0
           + Snap_13 * Time_13 * Time_13 * Time_13 * Time_13 / 24.0
           + Crackle_13 * Time_13 * Time_13 * Time_13 * Time_13 * Time_13 / 120.0
       and then Time_14 = T2
       and then Crackle_14 = 0.0
       and then Snap_14 = C * T1
       and then Jerk_14 = 0.0 - C * T1 * (T1 / 2.0 + T2)
       and then
         Accel_14
         = C
           * T1
           * (T1 + T2)
           * (2.0 * T1 + T2 + T3)
           - (C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - C * T1 * (T1 * T1 / 6.0 + T1 * T2 / 2.0 + T2 * T2 / 2.0))
       and then
         Vel_15
         = Vel_14 + Accel_14 * Time_14 + Jerk_14 * Time_14 * Time_14 / 2.0
           + Snap_14 * Time_14 * Time_14 * Time_14 / 6.0
           + Crackle_14 * Time_14 * Time_14 * Time_14 * Time_14 / 24.0
       and then
         Dist_15
         = Dist_14 + Vel_14 * Time_14 + Accel_14 * Time_14 * Time_14 / 2.0
           + Jerk_14 * Time_14 * Time_14 * Time_14 / 6.0
           + Snap_14 * Time_14 * Time_14 * Time_14 * Time_14 / 24.0
           + Crackle_14 * Time_14 * Time_14 * Time_14 * Time_14 * Time_14 / 120.0
       and then Time_15 = T1
       and then Crackle_15 = 0.0 - C
       and then Snap_15 = C * T1
       and then Jerk_15 = 0.0 - C * T1 * T1 / 2.0
       and then
         Accel_15
         = C
           * T1
           * (T1 + T2)
           * (2.0 * T1 + T2 + T3)
           - (C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - C * T1 * T1 * T1 / 6.0)
       and then
         Vel_16
         = Vel_15 + Accel_15 * Time_15 + Jerk_15 * Time_15 * Time_15 / 2.0
           + Snap_15 * Time_15 * Time_15 * Time_15 / 6.0
           + Crackle_15 * Time_15 * Time_15 * Time_15 * Time_15 / 24.0
       and then
         Dist_16
         = Dist_15 + Vel_15 * Time_15 + Accel_15 * Time_15 * Time_15 / 2.0
           + Jerk_15 * Time_15 * Time_15 * Time_15 / 6.0
           + Snap_15 * Time_15 * Time_15 * Time_15 * Time_15 / 24.0
           + Crackle_15 * Time_15 * Time_15 * Time_15 * Time_15 * Time_15 / 120.0
       and then Gain = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) * (4.0 * T1 + 2.0 * T2 + T3 + T4)
       and then Total = 8.0 * T1 + 4.0 * T2 + 2.0 * T3 + T4,
     Post => Vel_16 = Vs + Gain and then Dist_16 = (Vs + Gain / 2.0) * Total
   is
   begin
      null;
   end Lemma_Stages_12_To_15;

   procedure Lemma_End (R : Ramp) is
   begin
      Lemma_Checkpoint_12 (R);
      Lemma_Start_Derivatives (R, 12);
      Lemma_Next_Start (R, 12);
      Lemma_Start_Derivatives (R, 13);
      Lemma_Next_Start (R, 13);
      Lemma_Start_Derivatives (R, 14);
      Lemma_Next_Start (R, 14);
      Lemma_Start_Derivatives (R, 15);
      Lemma_Next_Start (R, 15);
      Lemma_Stages_12_To_15
        (R.T1,
         R.T2,
         R.T3,
         R.T4,
         R.C,
         R.Vs,
         Stage_Time (R, 12),
         Stage_Crackle (R, 12),
         Start_State (R, 12).Snap,
         Start_State (R, 12).Jerk,
         Start_State (R, 12).Accel,
         Start_State (R, 12).Vel,
         Start_State (R, 12).Dist,
         Stage_Time (R, 13),
         Stage_Crackle (R, 13),
         Start_State (R, 13).Snap,
         Start_State (R, 13).Jerk,
         Start_State (R, 13).Accel,
         Start_State (R, 13).Vel,
         Start_State (R, 13).Dist,
         Stage_Time (R, 14),
         Stage_Crackle (R, 14),
         Start_State (R, 14).Snap,
         Start_State (R, 14).Jerk,
         Start_State (R, 14).Accel,
         Start_State (R, 14).Vel,
         Start_State (R, 14).Dist,
         Stage_Time (R, 15),
         Stage_Crackle (R, 15),
         Start_State (R, 15).Snap,
         Start_State (R, 15).Jerk,
         Start_State (R, 15).Accel,
         Start_State (R, 15).Vel,
         Start_State (R, 15).Dist,
         Start_State (R, 16).Vel,
         Start_State (R, 16).Dist,
         Velocity_Gain (R),
         Total_Time (R));
   end Lemma_End;
   ------------------------------------------------------------------------------------------------------------------
   --  Monotone motion
   --
   --  Within a stage the change of velocity between two instants is the integral of a cubic and the change of
   --  distance the integral of a quartic, and there are quadrature rules which give those integrals exactly: each
   --  change is the time between the instants multiplied by a positive combination of accelerations, or velocities,
   --  at instants between them. So neither changes against the sign which the quantity before it keeps.
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Product_Nonnegative (X, Y : Real) with Pre => X >= 0.0 and then Y >= 0.0, Post => X * Y >= 0.0 is
   begin
      null;
   end Lemma_Product_Nonnegative;

   procedure Lemma_Velocity_Change (From : State; Crackle, X, Y : Real)
   with
     Post =>
       Advance (From, Crackle, Y).Vel - Advance (From, Crackle, X).Vel
       = (Y - X)
         * (Advance (From, Crackle, X).Accel
            + 4.0 * Advance (From, Crackle, (X + Y) / 2.0).Accel
            + Advance (From, Crackle, Y).Accel)
         / 6.0
   is
   begin
      null;
   end Lemma_Velocity_Change;
   --  Acceleration is cubic in time within a stage, so Simpson's rule gives the change of velocity exactly.

   procedure Lemma_Distance_Change (From : State; Crackle, X, Y : Real)
   with
     Post =>
       Advance (From, Crackle, Y).Dist - Advance (From, Crackle, X).Dist
       = (Y - X)
         * (7.0 * Advance (From, Crackle, X).Vel + 32.0 * Advance (From, Crackle, (3.0 * X + Y) / 4.0).Vel
            + 12.0 * Advance (From, Crackle, (X + Y) / 2.0).Vel
            + 32.0 * Advance (From, Crackle, (X + 3.0 * Y) / 4.0).Vel
            + 7.0 * Advance (From, Crackle, Y).Vel)
         / 90.0
   is
   begin
      null;
   end Lemma_Distance_Change;
   --  Velocity is quartic in time within a stage, so Boole's rule gives the change of distance exactly.

   procedure Lemma_Stage_Ends (R : Ramp; Stage : Stage_Index)
   with
     Post =>
       State_In (R, Stage, 0.0).Vel = Start_State (R, Stage).Vel
       and then State_In (R, Stage, 0.0).Dist = Start_State (R, Stage).Dist
       and then State_In (R, Stage, Stage_Time (R, Stage)).Vel = Start_State (R, Stage + 1).Vel
       and then State_In (R, Stage, Stage_Time (R, Stage)).Dist = Start_State (R, Stage + 1).Dist
   is
   begin
      null;
   end Lemma_Stage_Ends;
   --  A stage starts in the state at its boundary and ends in the state at the next.

   procedure Lemma_Velocity_Monotone (R : Ramp; Stage : Stage_Index; DT_1, DT_2 : Real) is
      Middle : constant Real := (DT_1 + DT_2) / 2.0;
      Sum    : constant Real :=
        State_In (R, Stage, DT_1).Accel + 4.0 * State_In (R, Stage, Middle).Accel + State_In (R, Stage, DT_2).Accel;
   begin
      Lemma_Limits (R, Stage, DT_1);
      Lemma_Limits (R, Stage, Middle);
      Lemma_Limits (R, Stage, DT_2);
      Lemma_Velocity_Change (Start_State (R, Stage), Stage_Crackle (R, Stage), DT_1, DT_2);
      pragma Assert (State_In (R, Stage, DT_2).Vel - State_In (R, Stage, DT_1).Vel = (DT_2 - DT_1) * Sum / 6.0);
      if R.C >= 0.0 then
         Lemma_Product_Nonnegative (DT_2 - DT_1, Sum);
      else
         Lemma_Product_Nonnegative (DT_2 - DT_1, 0.0 - Sum);
         pragma Assert ((DT_2 - DT_1) * (0.0 - Sum) = 0.0 - (DT_2 - DT_1) * Sum);
      end if;
   end Lemma_Velocity_Monotone;

   procedure Lemma_Start_Velocity_Monotone (R : Ramp; From, To : Boundary_Index) is
   begin
      if From < To then
         Lemma_Start_Velocity_Monotone (R, From, To - 1);
         Lemma_Velocity_Monotone (R, To - 1, 0.0, Stage_Time (R, To - 1));
         Lemma_Stage_Ends (R, To - 1);
      end if;
   end Lemma_Start_Velocity_Monotone;

   procedure Lemma_Velocity_Range (R : Ramp; Stage : Stage_Index; DT : Real) is
   begin
      Lemma_Start_Velocity_Monotone (R, 1, Stage);
      Lemma_Stage_Ends (R, Stage);
      Lemma_Velocity_Monotone (R, Stage, 0.0, DT);
      Lemma_Velocity_Monotone (R, Stage, DT, Stage_Time (R, Stage));
      Lemma_Start_Velocity_Monotone (R, Stage + 1, 16);
      Lemma_End (R);
   end Lemma_Velocity_Range;

   procedure Lemma_Distance_Monotone (R : Ramp; Stage : Stage_Index; DT_1, DT_2 : Real) is
      Quarter        : constant Real := (3.0 * DT_1 + DT_2) / 4.0;
      Middle         : constant Real := (DT_1 + DT_2) / 2.0;
      Three_Quarters : constant Real := (DT_1 + 3.0 * DT_2) / 4.0;
      Sum            : constant Real :=
        7.0 * State_In (R, Stage, DT_1).Vel + 32.0 * State_In (R, Stage, Quarter).Vel
        + 12.0 * State_In (R, Stage, Middle).Vel
        + 32.0 * State_In (R, Stage, Three_Quarters).Vel
        + 7.0 * State_In (R, Stage, DT_2).Vel;
   begin
      Lemma_Velocity_Range (R, Stage, DT_1);
      Lemma_Velocity_Range (R, Stage, Quarter);
      Lemma_Velocity_Range (R, Stage, Middle);
      Lemma_Velocity_Range (R, Stage, Three_Quarters);
      Lemma_Velocity_Range (R, Stage, DT_2);
      Lemma_Distance_Change (Start_State (R, Stage), Stage_Crackle (R, Stage), DT_1, DT_2);
      pragma Assert (State_In (R, Stage, DT_2).Dist - State_In (R, Stage, DT_1).Dist = (DT_2 - DT_1) * Sum / 90.0);
      Lemma_Product_Nonnegative (DT_2 - DT_1, Sum);
   end Lemma_Distance_Monotone;

   procedure Lemma_Start_Distance_Monotone (R : Ramp; From, To : Boundary_Index) is
   begin
      if From < To then
         Lemma_Start_Distance_Monotone (R, From, To - 1);
         Lemma_Distance_Monotone (R, To - 1, 0.0, Stage_Time (R, To - 1));
         Lemma_Stage_Ends (R, To - 1);
      end if;
   end Lemma_Start_Distance_Monotone;

   procedure Lemma_Distance_Range (R : Ramp; Stage : Stage_Index; DT : Real) is
   begin
      Lemma_Start_Distance_Monotone (R, 1, Stage);
      Lemma_Stage_Ends (R, Stage);
      Lemma_Distance_Monotone (R, Stage, 0.0, DT);
      Lemma_Distance_Monotone (R, Stage, DT, Stage_Time (R, Stage));
      Lemma_Start_Distance_Monotone (R, Stage + 1, 16);
      Lemma_End (R);
   end Lemma_Distance_Range;

end Prunt.Motion_Planner.Trajectory.Profile_Model.Motion;
