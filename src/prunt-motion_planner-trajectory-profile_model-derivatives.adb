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

package body Prunt.Motion_Planner.Trajectory.Profile_Model.Derivatives
  with SPARK_Mode => On
is

   ------------------------------------------------------------------------------------------------------------------
   --  Arithmetic
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Product_Nonnegative (X, Y : Real) with Pre => X >= 0.0 and then Y >= 0.0, Post => X * Y >= 0.0 is
   begin
      null;
   end Lemma_Product_Nonnegative;

   procedure Lemma_Product_Monotone (P, X, Y : Real) with Pre => P >= 0.0 and then X <= Y, Post => P * X <= P * Y is
   begin
      null;
   end Lemma_Product_Monotone;

   ------------------------------------------------------------------------------------------------------------------
   --  One stage of constant crackle, from any state
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Advance_Zero (From : State; Crackle : Real)
   with
     Post =>
       Advance (From, Crackle, 0.0).Snap = From.Snap
       and then Advance (From, Crackle, 0.0).Jerk = From.Jerk
       and then Advance (From, Crackle, 0.0).Accel = From.Accel
   is
   begin
      null;
   end Lemma_Advance_Zero;

   procedure Lemma_Advance_Mirrored (From, Mirrored : State; Crackle, Mirrored_Crackle, DT, Peak : Real)
   with
     Pre  =>
       Mirrored.Snap = 0.0 - From.Snap
       and then Mirrored.Jerk = 0.0 - From.Jerk
       and then Mirrored.Accel = Peak - From.Accel
       and then Mirrored_Crackle = 0.0 - Crackle,
     Post =>
       Advance (Mirrored, Mirrored_Crackle, DT).Snap = 0.0 - Advance (From, Crackle, DT).Snap
       and then Advance (Mirrored, Mirrored_Crackle, DT).Jerk = 0.0 - Advance (From, Crackle, DT).Jerk
       and then Advance (Mirrored, Mirrored_Crackle, DT).Accel = Peak - Advance (From, Crackle, DT).Accel
   is
   begin
      null;
   end Lemma_Advance_Mirrored;
   --  Integration is linear: negating snap, jerk and crackle negates what a stage adds to each.

   procedure Lemma_Jerk_Rises (From : State; Crackle, X, Y : Real)
   with
     Pre  => X <= Y and then Advance (From, Crackle, X).Snap >= 0.0 and then Advance (From, Crackle, Y).Snap >= 0.0,
     Post => Advance (From, Crackle, X).Jerk <= Advance (From, Crackle, Y).Jerk
   is
      Snap_X : constant Real := From.Snap + Crackle * X;
      Snap_Y : constant Real := From.Snap + Crackle * Y;
   begin
      pragma
        Assert (Advance (From, Crackle, Y).Jerk - Advance (From, Crackle, X).Jerk = (Y - X) * (Snap_X + Snap_Y) / 2.0);
      Lemma_Product_Nonnegative (Y - X, Snap_X + Snap_Y);
   end Lemma_Jerk_Rises;
   --  Snap is linear in time within a stage, so the trapezium rule gives the change of jerk exactly.

   procedure Lemma_Jerk_Falls (From : State; Crackle, X, Y : Real)
   with
     Pre  => X <= Y and then Advance (From, Crackle, X).Snap <= 0.0 and then Advance (From, Crackle, Y).Snap <= 0.0,
     Post => Advance (From, Crackle, X).Jerk >= Advance (From, Crackle, Y).Jerk
   is
      Snap_X : constant Real := From.Snap + Crackle * X;
      Snap_Y : constant Real := From.Snap + Crackle * Y;
   begin
      pragma
        Assert
          (Advance (From, Crackle, X).Jerk - Advance (From, Crackle, Y).Jerk
           = (Y - X) * ((0.0 - Snap_X) + (0.0 - Snap_Y)) / 2.0);
      Lemma_Product_Nonnegative (Y - X, (0.0 - Snap_X) + (0.0 - Snap_Y));
   end Lemma_Jerk_Falls;

   procedure Lemma_Accel_Rises (From : State; Crackle, X, Y : Real)
   with
     Pre  =>
       X <= Y
       and then Advance (From, Crackle, X).Jerk >= 0.0
       and then Advance (From, Crackle, (X + Y) / 2.0).Jerk >= 0.0
       and then Advance (From, Crackle, Y).Jerk >= 0.0,
     Post => Advance (From, Crackle, X).Accel <= Advance (From, Crackle, Y).Accel
   is
      M      : constant Real := (X + Y) / 2.0;
      Jerk_X : constant Real := From.Jerk + From.Snap * X + Crackle * X * X / 2.0;
      Jerk_M : constant Real := From.Jerk + From.Snap * M + Crackle * M * M / 2.0;
      Jerk_Y : constant Real := From.Jerk + From.Snap * Y + Crackle * Y * Y / 2.0;
   begin
      pragma
        Assert
          (Advance (From, Crackle, Y).Accel - Advance (From, Crackle, X).Accel
           = (Y - X) * (Jerk_X + 4.0 * Jerk_M + Jerk_Y) / 6.0);
      Lemma_Product_Nonnegative (Y - X, Jerk_X + 4.0 * Jerk_M + Jerk_Y);
   end Lemma_Accel_Rises;
   --  Jerk is quadratic in time within a stage, so Simpson's rule gives the change of acceleration exactly.

   procedure Lemma_Accel_Falls (From : State; Crackle, X, Y : Real)
   with
     Pre  =>
       X <= Y
       and then Advance (From, Crackle, X).Jerk <= 0.0
       and then Advance (From, Crackle, (X + Y) / 2.0).Jerk <= 0.0
       and then Advance (From, Crackle, Y).Jerk <= 0.0,
     Post => Advance (From, Crackle, X).Accel >= Advance (From, Crackle, Y).Accel
   is
      M      : constant Real := (X + Y) / 2.0;
      Jerk_X : constant Real := From.Jerk + From.Snap * X + Crackle * X * X / 2.0;
      Jerk_M : constant Real := From.Jerk + From.Snap * M + Crackle * M * M / 2.0;
      Jerk_Y : constant Real := From.Jerk + From.Snap * Y + Crackle * Y * Y / 2.0;
   begin
      pragma
        Assert
          (Advance (From, Crackle, X).Accel - Advance (From, Crackle, Y).Accel
           = (Y - X) * ((0.0 - Jerk_X) + 4.0 * (0.0 - Jerk_M) + (0.0 - Jerk_Y)) / 6.0);
      Lemma_Product_Nonnegative (Y - X, (0.0 - Jerk_X) + 4.0 * (0.0 - Jerk_M) + (0.0 - Jerk_Y));
   end Lemma_Accel_Falls;

   procedure Lemma_Snap_Moves (From : State; Crackle, X, Y : Real)
   with
     Pre  => X <= Y,
     Post =>
       (if Crackle >= 0.0
        then Advance (From, Crackle, X).Snap <= Advance (From, Crackle, Y).Snap
        else Advance (From, Crackle, X).Snap >= Advance (From, Crackle, Y).Snap)
   is
   begin
      if Crackle >= 0.0 then
         Lemma_Product_Monotone (Crackle, X, Y);
      else
         Lemma_Product_Monotone (0.0 - Crackle, X, Y);
         pragma Assert ((0.0 - Crackle) * X = 0.0 - Crackle * X);
         pragma Assert ((0.0 - Crackle) * Y = 0.0 - Crackle * Y);
      end if;
   end Lemma_Snap_Moves;
   --  Snap is linear in time within a stage, with the crackle of the stage as its slope.

   ------------------------------------------------------------------------------------------------------------------
   --  The closed forms at the stage boundaries
   ------------------------------------------------------------------------------------------------------------------

   function Step_Holds (R : Ramp; Boundary : Boundary_Index) return Boolean
   is (Start_Snap (R, Boundary)
       = Start_Snap (R, Boundary - 1) + Stage_Crackle (R, Boundary - 1) * Stage_Time (R, Boundary - 1)
       and then
         First_Half_Jerk (R, Boundary)
         = First_Half_Jerk (R, Boundary - 1) + Start_Snap (R, Boundary - 1) * Stage_Time (R, Boundary - 1)
           + Stage_Crackle (R, Boundary - 1) * Stage_Time (R, Boundary - 1) * Stage_Time (R, Boundary - 1) / 2.0
       and then
         First_Half_Accel (R, Boundary)
         = First_Half_Accel (R, Boundary - 1) + First_Half_Jerk (R, Boundary - 1) * Stage_Time (R, Boundary - 1)
           + Start_Snap (R, Boundary - 1) * Stage_Time (R, Boundary - 1) * Stage_Time (R, Boundary - 1) / 2.0
           + Stage_Crackle (R, Boundary - 1)
             * Stage_Time (R, Boundary - 1)
             * Stage_Time (R, Boundary - 1)
             * Stage_Time (R, Boundary - 1)
             / 6.0)
   with Pre => Boundary in 2 .. 8;
   --  Integrating the stage before a boundary of the first half takes the closed forms at the boundary before to
   --  those at this one.

   procedure Lemma_Step_2 (R : Ramp) with Post => Step_Holds (R, 2) is
   begin
      null;
   end Lemma_Step_2;

   procedure Lemma_Step_3 (R : Ramp) with Post => Step_Holds (R, 3) is
   begin
      null;
   end Lemma_Step_3;

   procedure Lemma_Step_4 (R : Ramp) with Post => Step_Holds (R, 4) is
   begin
      null;
   end Lemma_Step_4;

   procedure Lemma_Step_5 (R : Ramp) with Post => Step_Holds (R, 5) is
   begin
      null;
   end Lemma_Step_5;

   procedure Lemma_Step_6 (R : Ramp) with Post => Step_Holds (R, 6) is
   begin
      null;
   end Lemma_Step_6;

   procedure Lemma_Step_7 (R : Ramp) with Post => Step_Holds (R, 7) is
   begin
      null;
   end Lemma_Step_7;

   procedure Lemma_Step_8 (R : Ramp) with Post => Step_Holds (R, 8) is
   begin
      null;
   end Lemma_Step_8;

   procedure Lemma_Step (R : Ramp; Boundary : Boundary_Index)
   with Pre => Boundary in 2 .. 8, Post => Step_Holds (R, Boundary)
   is
   begin
      case Boundary is
         when 2      =>
            Lemma_Step_2 (R);
            pragma Assert (Step_Holds (R, Boundary));

         when 3      =>
            Lemma_Step_3 (R);
            pragma Assert (Step_Holds (R, Boundary));

         when 4      =>
            Lemma_Step_4 (R);
            pragma Assert (Step_Holds (R, Boundary));

         when 5      =>
            Lemma_Step_5 (R);
            pragma Assert (Step_Holds (R, Boundary));

         when 6      =>
            Lemma_Step_6 (R);
            pragma Assert (Step_Holds (R, Boundary));

         when 7      =>
            Lemma_Step_7 (R);
            pragma Assert (Step_Holds (R, Boundary));

         when others =>
            Lemma_Step_8 (R);
            pragma Assert (Step_Holds (R, Boundary));
      end case;
   end Lemma_Step;

   function Closed_Forms_Hold (R : Ramp; Boundary : Boundary_Index) return Boolean
   is (Start_State (R, Boundary).Snap = Start_Snap (R, Boundary)
       and then Start_State (R, Boundary).Jerk = Start_Jerk (R, Boundary)
       and then Start_State (R, Boundary).Accel = Start_Accel (R, Boundary));

   procedure Lemma_First_Half_Boundary (R : Ramp; Boundary : Boundary_Index)
   with Pre => Boundary in 2 .. 8 and then Closed_Forms_Hold (R, Boundary - 1), Post => Closed_Forms_Hold (R, Boundary)
   is
   begin
      Lemma_Step (R, Boundary);
   end Lemma_First_Half_Boundary;

   procedure Lemma_Middle_Boundary (R : Ramp) with Pre => Closed_Forms_Hold (R, 8), Post => Closed_Forms_Hold (R, 9) is
   begin
      null;
   end Lemma_Middle_Boundary;

   procedure Lemma_Stage_Mirrored (R : Ramp; Stage : Stage_Index)
   with
     Pre  => Stage <= 7,
     Post =>
       Stage_Time (R, Stage + 8) = Stage_Time (R, Stage)
       and then Stage_Crackle (R, Stage + 8) = 0.0 - Stage_Crackle (R, Stage)
       and then Start_Snap (R, Stage + 8) = 0.0 - Start_Snap (R, Stage)
       and then Start_Snap (R, Stage + 9) = 0.0 - Start_Snap (R, Stage + 1)
   is
   begin
      null;
   end Lemma_Stage_Mirrored;

   procedure Lemma_Second_Half_Boundary (R : Ramp; Boundary : Boundary_Index)
   with
     Pre  => Boundary in 10 .. 16 and then Closed_Forms_Hold (R, Boundary - 1),
     Post => Closed_Forms_Hold (R, Boundary)
   is
      First   : constant State :=
        (Snap  => Start_Snap (R, Boundary - 9),
         Jerk  => First_Half_Jerk (R, Boundary - 9),
         Accel => First_Half_Accel (R, Boundary - 9),
         Vel   => 0.0,
         Dist  => 0.0);
      --  The closed forms eight boundaries before the last one.
      Crackle : constant Real := Stage_Crackle (R, Boundary - 9);
      Time    : constant Real := Stage_Time (R, Boundary - 9);
   begin
      Lemma_Stage_Mirrored (R, Boundary - 9);
      Lemma_Step (R, Boundary - 8);
      pragma Assert (Advance (First, Crackle, Time).Snap = Start_Snap (R, Boundary - 8));
      pragma Assert (Advance (First, Crackle, Time).Jerk = First_Half_Jerk (R, Boundary - 8));
      pragma Assert (Advance (First, Crackle, Time).Accel = First_Half_Accel (R, Boundary - 8));
      Lemma_Advance_Mirrored
        (From             => First,
         Mirrored         => Start_State (R, Boundary - 1),
         Crackle          => Crackle,
         Mirrored_Crackle => Stage_Crackle (R, Boundary - 1),
         DT               => Time,
         Peak             => Peak_Accel (R));
      pragma
        Assert
          (Start_State (R, Boundary).Jerk
           = Advance (Start_State (R, Boundary - 1), Stage_Crackle (R, Boundary - 1), Time).Jerk);
      pragma Assert (Start_State (R, Boundary).Jerk = 0.0 - First_Half_Jerk (R, Boundary - 8));
   end Lemma_Second_Half_Boundary;

   procedure Lemma_Boundary (R : Ramp; Boundary : Boundary_Index)
   with Subprogram_Variant => (Decreases => Boundary), Post => Closed_Forms_Hold (R, Boundary)
   is
   begin
      if Boundary = 1 then
         null;
      elsif Boundary <= 8 then
         Lemma_Boundary (R, Boundary - 1);
         Lemma_First_Half_Boundary (R, Boundary);
      elsif Boundary = 9 then
         Lemma_Boundary (R, 8);
         Lemma_Middle_Boundary (R);
      else
         Lemma_Boundary (R, Boundary - 1);
         Lemma_Second_Half_Boundary (R, Boundary);
      end if;
   end Lemma_Boundary;

   procedure Lemma_Start_Derivatives (R : Ramp; Boundary : Boundary_Index) is
   begin
      Lemma_Boundary (R, Boundary);
   end Lemma_Start_Derivatives;

   procedure Lemma_Ends_At_Rest (R : Ramp) is
   begin
      Lemma_Start_Derivatives (R, 16);
   end Lemma_Ends_At_Rest;

   procedure Lemma_Mirror (R : Ramp; Stage : Stage_Index; DT : Real) is
   begin
      Lemma_Start_Derivatives (R, Stage);
      Lemma_Start_Derivatives (R, Stage + 8);
      Lemma_Stage_Mirrored (R, Stage);
      Lemma_Advance_Mirrored
        (From             => Start_State (R, Stage),
         Mirrored         => Start_State (R, Stage + 8),
         Crackle          => Stage_Crackle (R, Stage),
         Mirrored_Crackle => Stage_Crackle (R, Stage + 8),
         DT               => DT,
         Peak             => Peak_Accel (R));
   end Lemma_Mirror;

   ------------------------------------------------------------------------------------------------------------------
   --  Limits of the first half of a ramp with nonnegative crackle
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_First_Half_Snap (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  => Is_Ramp (R) and then R.C >= 0.0 and then Stage <= 8 and then In_Stage (R, Stage, DT),
     Post =>
       State_In (R, Stage, DT).Snap >= 0.0 - Peak_Snap (R)
       and then State_In (R, Stage, DT).Snap <= Peak_Snap (R)
       and then (if Stage <= 4 then State_In (R, Stage, DT).Snap >= 0.0)
       and then (if Stage >= 4 then State_In (R, Stage, DT).Snap <= 0.0)
   is
   begin
      Lemma_Start_Derivatives (R, Stage);
      Lemma_Product_Nonnegative (R.C, R.T1);
      Lemma_Product_Nonnegative (R.C, DT);
      if Stage in 1 | 3 | 5 | 7 then
         Lemma_Product_Monotone (R.C, DT, R.T1);
      end if;
      pragma Assert (State_In (R, Stage, DT).Snap = Start_Snap (R, Stage) + Stage_Crackle (R, Stage) * DT);
   end Lemma_First_Half_Snap;
   --  Snap rises to its peak, holds it and falls to zero over stages 1 to 3, and does the opposite over stages 5
   --  to 7.

   procedure Lemma_Start_Jerk_Range (R : Ramp; Boundary : Boundary_Index)
   with
     Pre  => Is_Ramp (R) and then R.C >= 0.0 and then Boundary <= 9,
     Post => Start_Jerk (R, Boundary) >= 0.0 and then Start_Jerk (R, Boundary) <= Peak_Jerk (R)
   is
      P : constant Real := R.C * R.T1;
   begin
      Lemma_Product_Nonnegative (R.C, R.T1);
      Lemma_Product_Monotone (P, 0.0, R.T1 / 2.0);
      Lemma_Product_Monotone (P, R.T1 / 2.0, R.T1 / 2.0 + R.T2);
      Lemma_Product_Monotone (P, R.T1 / 2.0 + R.T2, R.T1 + R.T2);
      pragma Assert (P * R.T1 / 2.0 = P * (R.T1 / 2.0));
   end Lemma_Start_Jerk_Range;

   procedure Lemma_First_Half_Jerk (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  => Is_Ramp (R) and then R.C >= 0.0 and then Stage <= 8 and then In_Stage (R, Stage, DT),
     Post => State_In (R, Stage, DT).Jerk >= 0.0 and then State_In (R, Stage, DT).Jerk <= Peak_Jerk (R)
   is
      From    : constant State := Start_State (R, Stage);
      Crackle : constant Real := Stage_Crackle (R, Stage);
      Time    : constant Real := Stage_Time (R, Stage);
   begin
      Lemma_Start_Derivatives (R, Stage);
      Lemma_Start_Derivatives (R, Stage + 1);
      Lemma_Start_Jerk_Range (R, Stage);
      Lemma_Start_Jerk_Range (R, Stage + 1);
      Lemma_Advance_Zero (From, Crackle);
      Lemma_First_Half_Snap (R, Stage, 0.0);
      Lemma_First_Half_Snap (R, Stage, DT);
      Lemma_First_Half_Snap (R, Stage, Time);
      pragma Assert (Advance (From, Crackle, Time).Jerk = Start_State (R, Stage + 1).Jerk);
      if Stage <= 4 then
         Lemma_Jerk_Rises (From, Crackle, 0.0, DT);
         Lemma_Jerk_Rises (From, Crackle, DT, Time);
      else
         Lemma_Jerk_Falls (From, Crackle, 0.0, DT);
         Lemma_Jerk_Falls (From, Crackle, DT, Time);
      end if;
   end Lemma_First_Half_Jerk;
   --  Jerk rises from zero to its peak over stages 1 to 3, holds it over stage 4 and falls to zero over stages
   --  5 to 7.

   procedure Lemma_First_Half_Accel_Rises (R : Ramp; Stage : Stage_Index; X, Y : Real)
   with
     Pre  =>
       Is_Ramp (R)
       and then R.C >= 0.0
       and then Stage <= 8
       and then X >= 0.0
       and then X <= Y
       and then Y <= Stage_Time (R, Stage),
     Post => State_In (R, Stage, X).Accel <= State_In (R, Stage, Y).Accel
   is
   begin
      Lemma_First_Half_Jerk (R, Stage, X);
      Lemma_First_Half_Jerk (R, Stage, (X + Y) / 2.0);
      Lemma_First_Half_Jerk (R, Stage, Y);
      Lemma_Accel_Rises (Start_State (R, Stage), Stage_Crackle (R, Stage), X, Y);
   end Lemma_First_Half_Accel_Rises;

   procedure Lemma_Start_Accel_Rises (R : Ramp; From, To : Boundary_Index)
   with
     Subprogram_Variant => (Decreases => To),
     Pre                => Is_Ramp (R) and then R.C >= 0.0 and then From <= To and then To <= 9,
     Post               => Start_State (R, From).Accel <= Start_State (R, To).Accel
   is
   begin
      if From < To then
         Lemma_Start_Accel_Rises (R, From, To - 1);
         Lemma_First_Half_Accel_Rises (R, To - 1, 0.0, Stage_Time (R, To - 1));
         Lemma_Advance_Zero (Start_State (R, To - 1), Stage_Crackle (R, To - 1));
         pragma Assert (State_In (R, To - 1, 0.0).Accel = Start_State (R, To - 1).Accel);
         pragma Assert (State_In (R, To - 1, Stage_Time (R, To - 1)).Accel = Start_State (R, To).Accel);
      end if;
   end Lemma_Start_Accel_Rises;

   procedure Lemma_First_Half_Accel (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  => Is_Ramp (R) and then R.C >= 0.0 and then Stage <= 8 and then In_Stage (R, Stage, DT),
     Post => State_In (R, Stage, DT).Accel >= 0.0 and then State_In (R, Stage, DT).Accel <= Peak_Accel (R)
   is
   begin
      Lemma_Start_Derivatives (R, 1);
      Lemma_Start_Derivatives (R, 9);
      Lemma_Start_Accel_Rises (R, 1, Stage);
      Lemma_Start_Accel_Rises (R, Stage + 1, 9);
      Lemma_First_Half_Accel_Rises (R, Stage, 0.0, DT);
      Lemma_First_Half_Accel_Rises (R, Stage, DT, Stage_Time (R, Stage));
      Lemma_Advance_Zero (Start_State (R, Stage), Stage_Crackle (R, Stage));
      pragma Assert (State_In (R, Stage, 0.0).Accel = Start_State (R, Stage).Accel);
      pragma Assert (State_In (R, Stage, Stage_Time (R, Stage)).Accel = Start_State (R, Stage + 1).Accel);
   end Lemma_First_Half_Accel;
   --  Jerk is never negative in the first half, so acceleration only rises, from zero to its peak.

   procedure Lemma_Accelerating_Limits (R : Ramp; Stage : Stage_Index; DT : Real) is
   begin
      if Stage <= 8 then
         Lemma_First_Half_Snap (R, Stage, DT);
         Lemma_First_Half_Jerk (R, Stage, DT);
         Lemma_First_Half_Accel (R, Stage, DT);
      else
         Lemma_Stage_Mirrored (R, Stage - 8);
         Lemma_Mirror (R, Stage - 8, DT);
         Lemma_First_Half_Snap (R, Stage - 8, DT);
         Lemma_First_Half_Jerk (R, Stage - 8, DT);
         Lemma_First_Half_Accel (R, Stage - 8, DT);
      end if;
   end Lemma_Accelerating_Limits;

   ------------------------------------------------------------------------------------------------------------------
   --  Negated crackle
   ------------------------------------------------------------------------------------------------------------------

   function Reflects (Of_Ramp, Of_Negated : State; Vs, Time : Real) return Boolean
   is (Of_Negated.Snap = 0.0 - Of_Ramp.Snap
       and then Of_Negated.Jerk = 0.0 - Of_Ramp.Jerk
       and then Of_Negated.Accel = 0.0 - Of_Ramp.Accel
       and then Of_Negated.Vel + Of_Ramp.Vel = 2.0 * Vs
       and then Of_Negated.Dist + Of_Ramp.Dist = 2.0 * Vs * Time);
   --  How the state of a ramp with negated crackle relates to the state of the ramp at the same time.

   procedure Lemma_Reflected_Velocity (V1, A1, J1, S1, C1, V2, A2, J2, S2, C2, DT : Real)
   with
     Pre  => A2 = 0.0 - A1 and then J2 = 0.0 - J1 and then S2 = 0.0 - S1 and then C2 = 0.0 - C1,
     Post =>
       (V2 + A2 * DT + J2 * DT * DT / 2.0 + S2 * DT * DT * DT / 6.0 + C2 * DT * DT * DT * DT / 24.0)
       + (V1 + A1 * DT + J1 * DT * DT / 2.0 + S1 * DT * DT * DT / 6.0 + C1 * DT * DT * DT * DT / 24.0)
       = V2 + V1
   is
   begin
      null;
   end Lemma_Reflected_Velocity;

   procedure Lemma_Reflected_Distance (D1, V1, A1, J1, S1, C1, D2, V2, A2, J2, S2, C2, DT : Real)
   with
     Pre  => A2 = 0.0 - A1 and then J2 = 0.0 - J1 and then S2 = 0.0 - S1 and then C2 = 0.0 - C1,
     Post =>
       (D2 + V2 * DT + A2 * DT * DT / 2.0 + J2 * DT * DT * DT / 6.0 + S2 * DT * DT * DT * DT / 24.0
        + C2 * DT * DT * DT * DT * DT / 120.0)
       + (D1 + V1 * DT + A1 * DT * DT / 2.0 + J1 * DT * DT * DT / 6.0 + S1 * DT * DT * DT * DT / 24.0
          + C1 * DT * DT * DT * DT * DT / 120.0)
       = (D2 + D1) + (V2 + V1) * DT
   is
   begin
      null;
   end Lemma_Reflected_Distance;

   procedure Lemma_Advance_Reflected (Of_Ramp, Of_Negated : State; Crackle, Negated_Crackle, DT, Vs, Time : Real)
   with
     Pre  => Reflects (Of_Ramp, Of_Negated, Vs, Time) and then Negated_Crackle = 0.0 - Crackle,
     Post => Reflects (Advance (Of_Ramp, Crackle, DT), Advance (Of_Negated, Negated_Crackle, DT), Vs, Time + DT)
   is
   begin
      Lemma_Advance_Mirrored (Of_Ramp, Of_Negated, Crackle, Negated_Crackle, DT, 0.0);
      Lemma_Reflected_Velocity
        (Of_Ramp.Vel,
         Of_Ramp.Accel,
         Of_Ramp.Jerk,
         Of_Ramp.Snap,
         Crackle,
         Of_Negated.Vel,
         Of_Negated.Accel,
         Of_Negated.Jerk,
         Of_Negated.Snap,
         Negated_Crackle,
         DT);
      Lemma_Reflected_Distance
        (Of_Ramp.Dist,
         Of_Ramp.Vel,
         Of_Ramp.Accel,
         Of_Ramp.Jerk,
         Of_Ramp.Snap,
         Crackle,
         Of_Negated.Dist,
         Of_Negated.Vel,
         Of_Negated.Accel,
         Of_Negated.Jerk,
         Of_Negated.Snap,
         Negated_Crackle,
         DT);
      pragma Assert (2.0 * Vs * Time + 2.0 * Vs * DT = 2.0 * Vs * (Time + DT));
   end Lemma_Advance_Reflected;

   procedure Lemma_Negated_Stage (R : Ramp; Stage : Stage_Index)
   with
     Post =>
       Stage_Time (Negated (R), Stage) = Stage_Time (R, Stage)
       and then Stage_Crackle (Negated (R), Stage) = 0.0 - Stage_Crackle (R, Stage)
       and then Boundary_Time (R, Stage + 1) = Boundary_Time (R, Stage) + Stage_Time (R, Stage)
   is
   begin
      null;
   end Lemma_Negated_Stage;

   procedure Lemma_Negated_Start (R : Ramp; Boundary : Boundary_Index)
   with
     Subprogram_Variant => (Decreases => Boundary),
     Post               =>
       Reflects (Start_State (R, Boundary), Start_State (Negated (R), Boundary), R.Vs, Boundary_Time (R, Boundary))
   is
   begin
      if Boundary > 1 then
         Lemma_Negated_Start (R, Boundary - 1);
         Lemma_Negated_Stage (R, Boundary - 1);
         Lemma_Advance_Reflected
           (Of_Ramp         => Start_State (R, Boundary - 1),
            Of_Negated      => Start_State (Negated (R), Boundary - 1),
            Crackle         => Stage_Crackle (R, Boundary - 1),
            Negated_Crackle => Stage_Crackle (Negated (R), Boundary - 1),
            DT              => Stage_Time (R, Boundary - 1),
            Vs              => R.Vs,
            Time            => Boundary_Time (R, Boundary - 1));
      end if;
   end Lemma_Negated_Start;

   procedure Lemma_Negated (R : Ramp; Stage : Stage_Index; DT : Real) is
   begin
      Lemma_Negated_Start (R, Stage);
      Lemma_Negated_Stage (R, Stage);
      Lemma_Advance_Reflected
        (Of_Ramp         => Start_State (R, Stage),
         Of_Negated      => Start_State (Negated (R), Stage),
         Crackle         => Stage_Crackle (R, Stage),
         Negated_Crackle => Stage_Crackle (Negated (R), Stage),
         DT              => DT,
         Vs              => R.Vs,
         Time            => Boundary_Time (R, Stage));
   end Lemma_Negated;

   ------------------------------------------------------------------------------------------------------------------
   --  Any ramp
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Negated_Peaks (R : Ramp)
   with
     Post =>
       Peak_Snap (Negated (R)) = 0.0 - Peak_Snap (R)
       and then Peak_Jerk (Negated (R)) = 0.0 - Peak_Jerk (R)
       and then Peak_Accel (Negated (R)) = 0.0 - Peak_Accel (R)
   is
   begin
      null;
   end Lemma_Negated_Peaks;

   procedure Lemma_Limits (R : Ramp; Stage : Stage_Index; DT : Real) is
   begin
      if R.C >= 0.0 then
         Lemma_Accelerating_Limits (R, Stage, DT);
      else
         Lemma_Negated_Peaks (R);
         Lemma_Negated_Stage (R, Stage);
         Lemma_Accelerating_Limits (Negated (R), Stage, DT);
         Lemma_Negated (R, Stage, DT);
      end if;
   end Lemma_Limits;

   ------------------------------------------------------------------------------------------------------------------
   --  One direction within a stage
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Accelerating_Signs (R : Ramp; Stage : Stage_Index; DT : Real)
   with
     Pre  => Is_Ramp (R) and then R.C >= 0.0 and then In_Stage (R, Stage, DT),
     Post =>
       (if Stage in 1 .. 4 | 12 .. 15
        then State_In (R, Stage, DT).Snap >= 0.0
        else State_In (R, Stage, DT).Snap <= 0.0)
       and then (if Stage <= 8 then State_In (R, Stage, DT).Jerk >= 0.0 else State_In (R, Stage, DT).Jerk <= 0.0)
   is
   begin
      if Stage <= 8 then
         Lemma_First_Half_Snap (R, Stage, DT);
         Lemma_First_Half_Jerk (R, Stage, DT);
      else
         Lemma_Stage_Mirrored (R, Stage - 8);
         Lemma_Mirror (R, Stage - 8, DT);
         Lemma_First_Half_Snap (R, Stage - 8, DT);
         Lemma_First_Half_Jerk (R, Stage - 8, DT);
      end if;
   end Lemma_Accelerating_Signs;
   --  The signs of snap and jerk in a ramp with nonnegative crackle: the second half mirrors the first.

   procedure Lemma_Stage_Signs (R : Ramp; Stage : Stage_Index; DT : Real) is
   begin
      if R.C >= 0.0 then
         Lemma_Accelerating_Signs (R, Stage, DT);
      else
         Lemma_Negated_Stage (R, Stage);
         Lemma_Accelerating_Signs (Negated (R), Stage, DT);
         Lemma_Negated (R, Stage, DT);
      end if;
   end Lemma_Stage_Signs;

   procedure Lemma_Stage_Monotone (R : Ramp; Stage : Stage_Index; DT_1, DT_2 : Real) is
      From    : constant State := Start_State (R, Stage);
      Crackle : constant Real := Stage_Crackle (R, Stage);
      Middle  : constant Real := (DT_1 + DT_2) / 2.0;
   begin
      pragma Assert (In_Stage (R, Stage, Middle));
      Lemma_Stage_Signs (R, Stage, DT_1);
      Lemma_Stage_Signs (R, Stage, Middle);
      Lemma_Stage_Signs (R, Stage, DT_2);
      Lemma_Snap_Moves (From, Crackle, DT_1, DT_2);
      if Snap_Is_Nonnegative_In (R, Stage) then
         Lemma_Jerk_Rises (From, Crackle, DT_1, DT_2);
      else
         Lemma_Jerk_Falls (From, Crackle, DT_1, DT_2);
      end if;
      if Jerk_Is_Nonnegative_In (R, Stage) then
         Lemma_Accel_Rises (From, Crackle, DT_1, DT_2);
      else
         Lemma_Accel_Falls (From, Crackle, DT_1, DT_2);
      end if;
   end Lemma_Stage_Monotone;

   procedure Lemma_Stage_Range (R : Ramp; Stage : Stage_Index; DT : Real) is
      From    : constant State := Start_State (R, Stage);
      Crackle : constant Real := Stage_Crackle (R, Stage);
      Time    : constant Real := Stage_Time (R, Stage);
   begin
      Lemma_Advance_Zero (From, Crackle);
      pragma Assert (State_In (R, Stage, 0.0).Snap = Start_State (R, Stage).Snap);
      pragma Assert (State_In (R, Stage, 0.0).Jerk = Start_State (R, Stage).Jerk);
      pragma Assert (State_In (R, Stage, 0.0).Accel = Start_State (R, Stage).Accel);
      pragma Assert (State_In (R, Stage, Time).Snap = Start_State (R, Stage + 1).Snap);
      pragma Assert (State_In (R, Stage, Time).Jerk = Start_State (R, Stage + 1).Jerk);
      pragma Assert (State_In (R, Stage, Time).Accel = Start_State (R, Stage + 1).Accel);
      Lemma_Stage_Monotone (R, Stage, 0.0, DT);
      Lemma_Stage_Monotone (R, Stage, DT, Time);
   end Lemma_Stage_Range;

end Prunt.Motion_Planner.Trajectory.Profile_Model.Derivatives;
