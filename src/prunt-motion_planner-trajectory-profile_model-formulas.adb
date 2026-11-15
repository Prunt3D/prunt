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

package body Prunt.Motion_Planner.Trajectory.Profile_Model.Formulas
  with SPARK_Mode => On
is

   ------------------------------------------------------------------------------------------------------------------
   --  One identity for each polynomial of the code
   --
   --  A0, J0 and S0 stand for the acceleration, jerk and snap at the start of a stage, and Cr for the crackle of the
   --  stage. The precondition gives them the values which Derivatives proves they have. The left side of the
   --  postcondition is the polynomial as the code writes it, and the right side is what Advance adds over time DT.
   --  Stages with the same polynomial and the same start share a lemma, which is named for the first of them.
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Jerk_Stage_1 (C, DT, S0, Cr : Real)
   with Pre => S0 = 0.0 and then Cr = C, Post => C * (DT * DT) / 2.0 = S0 * DT + Cr * DT * DT / 2.0
   is
   begin
      null;
   end Lemma_Jerk_Stage_1;

   procedure Lemma_Jerk_Stage_2 (T1, C, DT, S0, Cr : Real)
   with Pre => S0 = C * T1 and then Cr = 0.0, Post => C * DT * T1 = S0 * DT + Cr * DT * DT / 2.0
   is
   begin
      null;
   end Lemma_Jerk_Stage_2;

   procedure Lemma_Jerk_Stage_3 (T1, C, DT, S0, Cr : Real)
   with
     Pre  => S0 = C * T1 and then Cr = 0.0 - C,
     Post => C * DT * (0.0 - DT + 2.0 * T1) / 2.0 = S0 * DT + Cr * DT * DT / 2.0
   is
   begin
      null;
   end Lemma_Jerk_Stage_3;

   procedure Lemma_Jerk_Stage_4 (DT, S0, Cr : Real)
   with Pre => S0 = 0.0 and then Cr = 0.0, Post => 0.0 = S0 * DT + Cr * DT * DT / 2.0
   is
   begin
      null;
   end Lemma_Jerk_Stage_4;

   procedure Lemma_Jerk_Stage_5 (C, DT, S0, Cr : Real)
   with Pre => S0 = 0.0 and then Cr = 0.0 - C, Post => 0.0 - C * (DT * DT) / 2.0 = S0 * DT + Cr * DT * DT / 2.0
   is
   begin
      null;
   end Lemma_Jerk_Stage_5;

   procedure Lemma_Jerk_Stage_6 (T1, C, DT, S0, Cr : Real)
   with Pre => S0 = 0.0 - C * T1 and then Cr = 0.0, Post => 0.0 - C * DT * T1 = S0 * DT + Cr * DT * DT / 2.0
   is
   begin
      null;
   end Lemma_Jerk_Stage_6;

   procedure Lemma_Jerk_Stage_7 (T1, C, DT, S0, Cr : Real)
   with Pre => S0 = 0.0 - C * T1 and then Cr = C, Post => C * DT * (DT - 2.0 * T1) / 2.0 = S0 * DT + Cr * DT * DT / 2.0
   is
   begin
      null;
   end Lemma_Jerk_Stage_7;

   procedure Lemma_Accel_Stage_1 (C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = 0.0 and then S0 = 0.0 and then Cr = C,
     Post => C * (DT * DT * DT) / 6.0 = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_1;

   procedure Lemma_Accel_Stage_2 (T1, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = C * T1 * T1 / 2.0 and then S0 = C * T1 and then Cr = 0.0,
     Post => C * DT * T1 * (DT + T1) / 2.0 = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_2;

   procedure Lemma_Accel_Stage_3 (T1, T2, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = C * T1 * (T1 / 2.0 + T2) and then S0 = C * T1 and then Cr = 0.0 - C,
     Post =>
       C * DT * (0.0 - DT * DT + 3.0 * DT * T1 + 3.0 * T1 * (T1 + 2.0 * T2)) / 6.0
       = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_3;

   procedure Lemma_Accel_Stage_4 (T1, T2, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = C * T1 * (T1 + T2) and then S0 = 0.0 and then Cr = 0.0,
     Post => C * DT * T1 * (T1 + T2) = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_4;

   procedure Lemma_Accel_Stage_5 (T1, T2, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = C * T1 * (T1 + T2) and then S0 = 0.0 and then Cr = 0.0 - C,
     Post =>
       C * DT * (0.0 - DT * DT + 6.0 * T1 * (T1 + T2)) / 6.0 = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_5;

   procedure Lemma_Accel_Stage_6 (T1, T2, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = C * T1 * (T1 / 2.0 + T2) and then S0 = 0.0 - C * T1 and then Cr = 0.0,
     Post => C * DT * T1 * (0.0 - DT + T1 + 2.0 * T2) / 2.0 = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_6;

   procedure Lemma_Accel_Stage_7 (T1, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = C * T1 * T1 / 2.0 and then S0 = 0.0 - C * T1 and then Cr = C,
     Post =>
       C * DT * (DT * DT - 3.0 * DT * T1 + 3.0 * (T1 * T1)) / 6.0
       = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_7;

   procedure Lemma_Accel_Stage_8 (DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = 0.0 and then S0 = 0.0 and then Cr = 0.0,
     Post => 0.0 = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_8;

   procedure Lemma_Accel_Stage_9 (C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = 0.0 and then S0 = 0.0 and then Cr = 0.0 - C,
     Post => 0.0 - C * (DT * DT * DT) / 6.0 = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_9;

   procedure Lemma_Accel_Stage_10 (T1, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = 0.0 - C * T1 * T1 / 2.0 and then S0 = 0.0 - C * T1 and then Cr = 0.0,
     Post => C * DT * T1 * (0.0 - DT - T1) / 2.0 = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_10;

   procedure Lemma_Accel_Stage_11 (T1, T2, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = 0.0 - C * T1 * (T1 / 2.0 + T2) and then S0 = 0.0 - C * T1 and then Cr = C,
     Post =>
       C * DT * (DT * DT - 3.0 * DT * T1 - 3.0 * T1 * (T1 + 2.0 * T2)) / 6.0
       = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_11;

   procedure Lemma_Accel_Stage_12 (T1, T2, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = 0.0 - C * T1 * (T1 + T2) and then S0 = 0.0 and then Cr = 0.0,
     Post => 0.0 - C * DT * T1 * (T1 + T2) = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_12;

   procedure Lemma_Accel_Stage_13 (T1, T2, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = 0.0 - C * T1 * (T1 + T2) and then S0 = 0.0 and then Cr = C,
     Post => C * DT * (DT * DT - 6.0 * T1 * (T1 + T2)) / 6.0 = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_13;

   procedure Lemma_Accel_Stage_14 (T1, T2, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = 0.0 - C * T1 * (T1 / 2.0 + T2) and then S0 = C * T1 and then Cr = 0.0,
     Post => C * DT * T1 * (DT - T1 - 2.0 * T2) / 2.0 = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_14;

   procedure Lemma_Accel_Stage_15 (T1, C, DT, J0, S0, Cr : Real)
   with
     Pre  => J0 = 0.0 - C * T1 * T1 / 2.0 and then S0 = C * T1 and then Cr = 0.0 - C,
     Post =>
       C * DT * (0.0 - DT * DT + 3.0 * DT * T1 - 3.0 * (T1 * T1)) / 6.0
       = J0 * DT + S0 * DT * DT / 2.0 + Cr * DT * DT * DT / 6.0
   is
   begin
      null;
   end Lemma_Accel_Stage_15;

   procedure Lemma_Velocity_Stage_1 (C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  => A0 = 0.0 and then J0 = 0.0 and then S0 = 0.0 and then Cr = C,
     Post =>
       C * (DT * DT * DT * DT) / 24.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_1;

   procedure Lemma_Velocity_Stage_2 (T1, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  => A0 = C * T1 * T1 * T1 / 6.0 and then J0 = C * T1 * T1 / 2.0 and then S0 = C * T1 and then Cr = 0.0,
     Post =>
       C * DT * T1 * (2.0 * (DT * DT) + 3.0 * DT * T1 + 2.0 * (T1 * T1)) / 12.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_2;

   procedure Lemma_Velocity_Stage_3 (T1, T2, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * (T1 * T1 / 6.0 + T1 * T2 / 2.0 + T2 * T2 / 2.0)
       and then J0 = C * T1 * (T1 / 2.0 + T2)
       and then S0 = C * T1
       and then Cr = 0.0 - C,
     Post =>
       C
       * DT
       * (0.0 - DT * DT * DT + 4.0 * (DT * DT) * T1 + 6.0 * DT * T1 * (T1 + 2.0 * T2)
          + 4.0 * T1 * (T1 * T1 + 3.0 * T1 * T2 + 3.0 * (T2 * T2)))
       / 24.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_3;

   procedure Lemma_Velocity_Stage_4 (T1, T2, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * (T1 + T2) * (2.0 * T1 + T2) / 2.0
       and then J0 = C * T1 * (T1 + T2)
       and then S0 = 0.0
       and then Cr = 0.0,
     Post =>
       C * DT * T1 * (DT * (T1 + T2) + 2.0 * (T1 * T1) + 3.0 * T1 * T2 + T2 * T2) / 2.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_4;

   procedure Lemma_Velocity_Stage_5 (T1, T2, T3, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + 2.0 * T3) / 2.0
       and then J0 = C * T1 * (T1 + T2)
       and then S0 = 0.0
       and then Cr = 0.0 - C,
     Post =>
       C
       * DT
       * (0.0 - DT * DT * DT + 12.0 * DT * T1 * (T1 + T2)
          + 12.0 * T1 * (2.0 * (T1 * T1) + 3.0 * T1 * T2 + 2.0 * T1 * T3 + T2 * T2 + 2.0 * T2 * T3))
       / 24.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_5;

   procedure Lemma_Velocity_Stage_6 (T1, T2, T3, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - C * T1 * (T1 * T1 / 6.0 + T1 * T2 / 2.0 + T2 * T2 / 2.0)
       and then J0 = C * T1 * (T1 / 2.0 + T2)
       and then S0 = 0.0 - C * T1
       and then Cr = 0.0,
     Post =>
       C
       * DT
       * T1
       * (0.0 - 2.0 * (DT * DT) + 3.0 * DT * (T1 + 2.0 * T2) + 22.0 * (T1 * T1) + 30.0 * T1 * T2 + 12.0 * T1 * T3
          + 6.0 * (T2 * T2)
          + 12.0 * T2 * T3)
       / 12.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_6;

   procedure Lemma_Velocity_Stage_7 (T1, T2, T3, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - C * T1 * T1 * T1 / 6.0
       and then J0 = C * T1 * T1 / 2.0
       and then S0 = 0.0 - C * T1
       and then Cr = C,
     Post =>
       C
       * DT
       * (DT * DT * DT - 4.0 * (DT * DT) * T1 + 6.0 * DT * (T1 * T1)
          + 4.0 * T1 * (11.0 * (T1 * T1) + 18.0 * T1 * T2 + 6.0 * T1 * T3 + 6.0 * (T2 * T2) + 6.0 * T2 * T3))
       / 24.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_7;

   procedure Lemma_Velocity_Stage_8 (T1, T2, T3, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  => A0 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) and then J0 = 0.0 and then S0 = 0.0 and then Cr = 0.0,
     Post =>
       C * DT * T1 * (2.0 * (T1 * T1) + 3.0 * T1 * T2 + T1 * T3 + T2 * T2 + T2 * T3)
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_8;

   procedure Lemma_Velocity_Stage_9 (T1, T2, T3, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  => A0 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) and then J0 = 0.0 and then S0 = 0.0 and then Cr = 0.0 - C,
     Post =>
       C
       * DT
       * (0.0 - DT * DT * DT + 24.0 * T1 * (2.0 * (T1 * T1) + 3.0 * T1 * T2 + T1 * T3 + T2 * T2 + T2 * T3))
       / 24.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_9;

   procedure Lemma_Velocity_Stage_10 (T1, T2, T3, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - C * T1 * T1 * T1 / 6.0
       and then J0 = 0.0 - C * T1 * T1 / 2.0
       and then S0 = 0.0 - C * T1
       and then Cr = 0.0,
     Post =>
       C
       * DT
       * T1
       * (0.0 - 2.0 * (DT * DT) - 3.0 * DT * T1 + 22.0 * (T1 * T1) + 36.0 * T1 * T2 + 12.0 * T1 * T3 + 12.0 * (T2 * T2)
          + 12.0 * T2 * T3)
       / 12.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_10;

   procedure Lemma_Velocity_Stage_11 (T1, T2, T3, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - C * T1 * (T1 * T1 / 6.0 + T1 * T2 / 2.0 + T2 * T2 / 2.0)
       and then J0 = 0.0 - C * T1 * (T1 / 2.0 + T2)
       and then S0 = 0.0 - C * T1
       and then Cr = C,
     Post =>
       C
       * DT
       * (DT * DT * DT - 4.0 * (DT * DT) * T1 - 6.0 * DT * T1 * (T1 + 2.0 * T2)
          + 4.0 * T1 * (11.0 * (T1 * T1) + 15.0 * T1 * T2 + 6.0 * T1 * T3 + 3.0 * (T2 * T2) + 6.0 * T2 * T3))
       / 24.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_11;

   procedure Lemma_Velocity_Stage_12 (T1, T2, T3, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - C * T1 * (T1 + T2) * (2.0 * T1 + T2) / 2.0
       and then J0 = 0.0 - C * T1 * (T1 + T2)
       and then S0 = 0.0
       and then Cr = 0.0,
     Post =>
       C
       * DT
       * T1
       * (0.0 - DT * (T1 + T2) + 2.0 * (T1 * T1) + 3.0 * T1 * T2 + 2.0 * T1 * T3 + T2 * T2 + 2.0 * T2 * T3)
       / 2.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_12;

   procedure Lemma_Velocity_Stage_13 (T1, T2, T3, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * (T1 + T2) * (2.0 * T1 + T2 + T3) - C * T1 * (T1 + T2) * (2.0 * T1 + T2 + 2.0 * T3) / 2.0
       and then J0 = 0.0 - C * T1 * (T1 + T2)
       and then S0 = 0.0
       and then Cr = C,
     Post =>
       C
       * DT
       * (DT * DT * DT - 12.0 * DT * T1 * (T1 + T2) + 12.0 * T1 * (2.0 * (T1 * T1) + 3.0 * T1 * T2 + T2 * T2))
       / 24.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_13;

   procedure Lemma_Velocity_Stage_14 (T1, T2, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * (T1 * T1 / 6.0 + T1 * T2 / 2.0 + T2 * T2 / 2.0)
       and then J0 = 0.0 - C * T1 * (T1 / 2.0 + T2)
       and then S0 = C * T1
       and then Cr = 0.0,
     Post =>
       C
       * DT
       * T1
       * (2.0 * (DT * DT) - 3.0 * DT * (T1 + 2.0 * T2) + 2.0 * (T1 * T1) + 6.0 * T1 * T2 + 6.0 * (T2 * T2))
       / 12.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_14;

   procedure Lemma_Velocity_Stage_15 (T1, C, DT, A0, J0, S0, Cr : Real)
   with
     Pre  =>
       A0 = C * T1 * T1 * T1 / 6.0 and then J0 = 0.0 - C * T1 * T1 / 2.0 and then S0 = C * T1 and then Cr = 0.0 - C,
     Post =>
       C * DT * (0.0 - DT * DT * DT + 4.0 * (DT * DT) * T1 - 6.0 * DT * (T1 * T1) + 4.0 * (T1 * T1 * T1)) / 24.0
       = A0 * DT + J0 * DT * DT / 2.0 + S0 * DT * DT * DT / 6.0 + Cr * DT * DT * DT * DT / 24.0
   is
   begin
      null;
   end Lemma_Velocity_Stage_15;

   ------------------------------------------------------------------------------------------------------------------
   --  The lemmas of the spec
   --
   --  State_In is Advance from the start of the stage, so each quantity is its value there plus the right side of
   --  the identity of its stage, and the increment function is the left side.
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Snap_Formula (R : Ramp; Stage : Stage_Index; DT : Real) is
   begin
      null;
   end Lemma_Snap_Formula;

   procedure Lemma_Jerk_Formula (R : Ramp; Stage : Stage_Index; DT : Real) is
      From : constant State := Start_State (R, Stage);
      Cr   : constant Real := Stage_Crackle (R, Stage);
   begin
      Derivatives.Lemma_Start_Derivatives (R, Stage);
      case Stage is
         when 1 | 13     =>
            Lemma_Jerk_Stage_1 (R.C, DT, From.Snap, Cr);

         when 2 | 14     =>
            Lemma_Jerk_Stage_2 (R.T1, R.C, DT, From.Snap, Cr);

         when 3 | 15     =>
            Lemma_Jerk_Stage_3 (R.T1, R.C, DT, From.Snap, Cr);

         when 4 | 8 | 12 =>
            Lemma_Jerk_Stage_4 (DT, From.Snap, Cr);

         when 5 | 9      =>
            Lemma_Jerk_Stage_5 (R.C, DT, From.Snap, Cr);

         when 6 | 10     =>
            Lemma_Jerk_Stage_6 (R.T1, R.C, DT, From.Snap, Cr);

         when 7 | 11     =>
            Lemma_Jerk_Stage_7 (R.T1, R.C, DT, From.Snap, Cr);
      end case;
   end Lemma_Jerk_Formula;

   procedure Lemma_Accel_Formula (R : Ramp; Stage : Stage_Index; DT : Real) is
      From : constant State := Start_State (R, Stage);
      Cr   : constant Real := Stage_Crackle (R, Stage);
   begin
      Derivatives.Lemma_Start_Derivatives (R, Stage);
      case Stage is
         when 1  =>
            Lemma_Accel_Stage_1 (R.C, DT, From.Jerk, From.Snap, Cr);

         when 2  =>
            Lemma_Accel_Stage_2 (R.T1, R.C, DT, From.Jerk, From.Snap, Cr);

         when 3  =>
            Lemma_Accel_Stage_3 (R.T1, R.T2, R.C, DT, From.Jerk, From.Snap, Cr);

         when 4  =>
            Lemma_Accel_Stage_4 (R.T1, R.T2, R.C, DT, From.Jerk, From.Snap, Cr);

         when 5  =>
            Lemma_Accel_Stage_5 (R.T1, R.T2, R.C, DT, From.Jerk, From.Snap, Cr);

         when 6  =>
            Lemma_Accel_Stage_6 (R.T1, R.T2, R.C, DT, From.Jerk, From.Snap, Cr);

         when 7  =>
            Lemma_Accel_Stage_7 (R.T1, R.C, DT, From.Jerk, From.Snap, Cr);

         when 8  =>
            Lemma_Accel_Stage_8 (DT, From.Jerk, From.Snap, Cr);

         when 9  =>
            Lemma_Accel_Stage_9 (R.C, DT, From.Jerk, From.Snap, Cr);

         when 10 =>
            Lemma_Accel_Stage_10 (R.T1, R.C, DT, From.Jerk, From.Snap, Cr);

         when 11 =>
            Lemma_Accel_Stage_11 (R.T1, R.T2, R.C, DT, From.Jerk, From.Snap, Cr);

         when 12 =>
            Lemma_Accel_Stage_12 (R.T1, R.T2, R.C, DT, From.Jerk, From.Snap, Cr);

         when 13 =>
            Lemma_Accel_Stage_13 (R.T1, R.T2, R.C, DT, From.Jerk, From.Snap, Cr);

         when 14 =>
            Lemma_Accel_Stage_14 (R.T1, R.T2, R.C, DT, From.Jerk, From.Snap, Cr);

         when 15 =>
            Lemma_Accel_Stage_15 (R.T1, R.C, DT, From.Jerk, From.Snap, Cr);
      end case;
   end Lemma_Accel_Formula;

   procedure Lemma_Velocity_Formula (R : Ramp; Stage : Stage_Index; DT : Real) is
      From : constant State := Start_State (R, Stage);
      Cr   : constant Real := Stage_Crackle (R, Stage);
   begin
      Derivatives.Lemma_Start_Derivatives (R, Stage);
      case Stage is
         when 1  =>
            Lemma_Velocity_Stage_1 (R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 2  =>
            Lemma_Velocity_Stage_2 (R.T1, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 3  =>
            Lemma_Velocity_Stage_3 (R.T1, R.T2, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 4  =>
            Lemma_Velocity_Stage_4 (R.T1, R.T2, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 5  =>
            Lemma_Velocity_Stage_5 (R.T1, R.T2, R.T3, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 6  =>
            Lemma_Velocity_Stage_6 (R.T1, R.T2, R.T3, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 7  =>
            Lemma_Velocity_Stage_7 (R.T1, R.T2, R.T3, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 8  =>
            Lemma_Velocity_Stage_8 (R.T1, R.T2, R.T3, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 9  =>
            Lemma_Velocity_Stage_9 (R.T1, R.T2, R.T3, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 10 =>
            Lemma_Velocity_Stage_10 (R.T1, R.T2, R.T3, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 11 =>
            Lemma_Velocity_Stage_11 (R.T1, R.T2, R.T3, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 12 =>
            Lemma_Velocity_Stage_12 (R.T1, R.T2, R.T3, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 13 =>
            Lemma_Velocity_Stage_13 (R.T1, R.T2, R.T3, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 14 =>
            Lemma_Velocity_Stage_14 (R.T1, R.T2, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);

         when 15 =>
            Lemma_Velocity_Stage_15 (R.T1, R.C, DT, From.Accel, From.Jerk, From.Snap, Cr);
      end case;
   end Lemma_Velocity_Formula;

   procedure Lemma_Stage_Chain (R : Ramp; Stage : Stage_Index) is
   begin
      null;
   end Lemma_Stage_Chain;

end Prunt.Motion_Planner.Trajectory.Profile_Model.Formulas;
