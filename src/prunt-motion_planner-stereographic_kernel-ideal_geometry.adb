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

pragma Warnings ("F");
--  A lemma can take a parameter only for its contract, which is not compiled.

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile. The
--  ghost code among them computes with exact rationals and would be far too slow to run.
pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Planar;
with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Projection;
with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Spatial;
with Prunt.Motion_Planner.Stereographic_Kernel.Intervals; use Prunt.Motion_Planner.Stereographic_Kernel.Intervals;

package body Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry
  with SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  The ideal tangent over the real numbers
   ---------------------------------------------------------------------------

   function Chart_Controls
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index) return Bernstein.Controls
   is
      Result : Bernstein.Controls (0 .. Fixed_Chart_Degree) := [others => 0.0];
   begin
      for I in Chart_Coefficient_Index loop
         Result (I) := R (Coefficients (I, Component));
         pragma Loop_Invariant (for all J in 0 .. I => Result (J) = R (Coefficients (J, Component)));
      end loop;
      return Result;
   end Chart_Controls;

   procedure Lemma_Chart_At_Ends (Coefficients : Bernstein_Chart; Component : Chart_Component_Index)
   with
     Ghost,
     Post =>
       Chart_Value (Coefficients, Component, 0.0) = R (Coefficients (0, Component))
       and then Chart_Value (Coefficients, Component, 1.0) = R (Coefficients (Fixed_Chart_Degree, Component));
   --  A chart coordinate takes its first control at the start and its last at the finish.

   procedure Lemma_Chart_At_Ends (Coefficients : Bernstein_Chart; Component : Chart_Component_Index) is
   begin
      Bernstein.Lemma_Value_At_Ends (Chart_Controls (Coefficients, Component));
   end Lemma_Chart_At_Ends;

   procedure Lemma_Cramer (Ys, Yf, Y, W, T, Fd, Sd : Real)
   with
     Ghost,
     Pre  =>
       T = (Yf - Ys) * (1.0 + Ys * Yf)
       and then Fd = (Yf - Y) * (1.0 + Yf * Y) - Yf * W
       and then Sd = (Y - Ys) * (1.0 + Ys * Y) + Ys * W,
     Post =>
       T * (1.0 - Y * Y - W) = Fd * (1.0 - Ys * Ys) + Sd * (1.0 - Yf * Yf)
       and then T * (2.0 * Y) = Fd * (2.0 * Ys) + Sd * (2.0 * Yf);
   --  Cramer's rule for the tangent numerator (1 - Y² - W, 2·Y) in terms of those of the two endpoint tangents,
   --  with T half the determinant of the endpoint numerators and Fd, Sd half the determinants of the numerator
   --  with each of them.

   procedure Lemma_Cramer (Ys, Yf, Y, W, T, Fd, Sd : Real) is null;

   procedure Lemma_Cancel (Factor, Left, Right : Real)
   with Ghost, Pre => Factor /= 0.0 and then Factor * Left = Factor * Right, Post => Left = Right;

   procedure Lemma_Cancel (Factor, Left, Right : Real) is null;

   procedure Lemma_Combination (Scale, X, A, B, Da, Db : Real)
   with
     Ghost,
     Pre  => Scale /= 0.0 and then Scale * X = Da * A + Db * B,
     Post => X = (Da / Scale) * A + (Db / Scale) * B;
   --  Dividing Cramer's rule through by the determinant.

   procedure Lemma_Combination (Scale, X, A, B, Da, Db : Real) is
      Wa : constant Real := Da / Scale;
      Wb : constant Real := Db / Scale;
   begin
      pragma Assert (Wa * Scale = Da);
      pragma Assert (Wb * Scale = Db);
      pragma Assert (Scale * (Wa * A + Wb * B) = (Wa * Scale) * A + (Wb * Scale) * B);
      Lemma_Cancel (Scale, X, Wa * A + Wb * B);
   end Lemma_Combination;

   procedure Lemma_Quotient_Sign (Numerator_Value, Divisor : Real)
   with
     Ghost,
     Pre  => (Divisor > 0.0 and then Numerator_Value >= 0.0) or else (Divisor < 0.0 and then Numerator_Value <= 0.0),
     Post => Numerator_Value / Divisor >= 0.0;

   procedure Lemma_Quotient_Sign (Numerator_Value, Divisor : Real) is null;

   procedure Lemma_Cone_Algebra (Ys, Yf, Y, W, T, Fd, Sd, N0, N1, A0, A1, B0, B1, Sw, Fw : Real)
   with
     Ghost,
     Pre  =>
       T = (Yf - Ys) * (1.0 + Ys * Yf)
       and then Fd = (Yf - Y) * (1.0 + Yf * Y) - Yf * W
       and then Sd = (Y - Ys) * (1.0 + Ys * Y) + Ys * W
       and then N0 = 1.0 - Y * Y - W
       and then N1 = 2.0 * Y
       and then A0 = 1.0 - Ys * Ys
       and then A1 = 2.0 * Ys
       and then B0 = 1.0 - Yf * Yf
       and then B1 = 2.0 * Yf
       and then T /= 0.0
       and then Sw = Fd / T
       and then Fw = Sd / T
       and then
         ((T > 0.0 and then Fd >= 0.0 and then Sd >= 0.0) or else (T < 0.0 and then Fd <= 0.0 and then Sd <= 0.0)),
     Post => Sw >= 0.0 and then Fw >= 0.0 and then N0 = Sw * A0 + Fw * B0 and then N1 = Sw * A1 + Fw * B1;
   --  With a nonzero turn, the tangent numerator (N0, N1) is the combination of the endpoint numerators (A0, A1)
   --  and (B0, B1) whose weights are the two determinants over the turn, and those weights are nonnegative when
   --  the determinants have the sign of the turn.

   procedure Lemma_Cone_Algebra (Ys, Yf, Y, W, T, Fd, Sd, N0, N1, A0, A1, B0, B1, Sw, Fw : Real) is
   begin
      Lemma_Cramer (Ys, Yf, Y, W, T, Fd, Sd);
      Lemma_Combination (T, N0, A0, B0, Fd, Sd);
      Lemma_Combination (T, N1, A1, B1, Fd, Sd);
      Lemma_Quotient_Sign (Fd, T);
      Lemma_Quotient_Sign (Sd, T);
   end Lemma_Cone_Algebra;

   procedure Lemma_Trivial_Combination (N, A, B, Sw, Fw : Real)
   with Ghost, Pre => Sw = 1.0 and then Fw = 0.0 and then N = A, Post => N = Sw * A + Fw * B;

   procedure Lemma_Trivial_Combination (N, A, B, Sw, Fw : Real) is null;

   procedure Lemma_Numerators_At_Ends (Coefficients : Bernstein_Chart)
   with
     Ghost,
     Pre  => Endpoints_Are_In_Frame_Plane (Coefficients),
     Post =>
       Numerator (Coefficients, 0, 0.0) = 1.0 - Start_Value (Coefficients) * Start_Value (Coefficients)
       and then Numerator (Coefficients, 1, 0.0) = 2.0 * Start_Value (Coefficients)
       and then Numerator (Coefficients, 2, 0.0) = 0.0
       and then Numerator (Coefficients, 3, 0.0) = 0.0
       and then Numerator (Coefficients, 0, 1.0) = 1.0 - Finish_Value (Coefficients) * Finish_Value (Coefficients)
       and then Numerator (Coefficients, 1, 1.0) = 2.0 * Finish_Value (Coefficients)
       and then Numerator (Coefficients, 2, 1.0) = 0.0
       and then Numerator (Coefficients, 3, 1.0) = 0.0;
   --  The endpoint tangents are those of the endpoint chart values, in the frame plane.

   procedure Lemma_Numerators_At_Ends (Coefficients : Bernstein_Chart) is
   begin
      Lemma_Chart_At_Ends (Coefficients, 1);
      Lemma_Chart_At_Ends (Coefficients, 2);
      Lemma_Chart_At_Ends (Coefficients, 3);
      pragma Assert (Chart_Value (Coefficients, 2, 0.0) = 0.0 and then Chart_Value (Coefficients, 3, 0.0) = 0.0);
      pragma Assert (Chart_Value (Coefficients, 2, 1.0) = 0.0 and then Chart_Value (Coefficients, 3, 1.0) = 0.0);
      pragma Assert (Normal_Square (Coefficients, 0.0) = 0.0);
      pragma Assert (Normal_Square (Coefficients, 1.0) = 0.0);
   end Lemma_Numerators_At_Ends;

   procedure Lemma_Cone_Weights (Coefficients : Bernstein_Chart; V : Real) is
      Ys : constant Real := Start_Value (Coefficients);
      Yf : constant Real := Finish_Value (Coefficients);
      Y  : constant Real := Chart_Value (Coefficients, 1, V);
      W  : constant Real := Normal_Square (Coefficients, V);
   begin
      Lemma_Numerators_At_Ends (Coefficients);
      if Turn (Coefficients) /= 0.0 then
         Lemma_Cone_Algebra
           (Ys => Ys,
            Yf => Yf,
            Y  => Y,
            W  => W,
            T  => Turn (Coefficients),
            Fd => Finish_Determinant (Coefficients, V),
            Sd => Start_Determinant (Coefficients, V),
            N0 => Numerator (Coefficients, 0, V),
            N1 => Numerator (Coefficients, 1, V),
            A0 => Numerator (Coefficients, 0, 0.0),
            A1 => Numerator (Coefficients, 1, 0.0),
            B0 => Numerator (Coefficients, 0, 1.0),
            B1 => Numerator (Coefficients, 1, 1.0),
            Sw => Start_Weight (Coefficients, V),
            Fw => Finish_Weight (Coefficients, V));
      else
         --  The cone is a single ray and the tangent is the start tangent.
         pragma Assert (Y = Ys and then W = 0.0);
         pragma Assert (Start_Weight (Coefficients, V) = 1.0 and then Finish_Weight (Coefficients, V) = 0.0);
         pragma Assert (Numerator (Coefficients, 0, V) = 1.0 - Ys * Ys);
         pragma Assert (Numerator (Coefficients, 1, V) = 2.0 * Ys);
         Lemma_Trivial_Combination
           (Numerator (Coefficients, 0, V),
            Numerator (Coefficients, 0, 0.0),
            Numerator (Coefficients, 0, 1.0),
            Start_Weight (Coefficients, V),
            Finish_Weight (Coefficients, V));
         Lemma_Trivial_Combination
           (Numerator (Coefficients, 1, V),
            Numerator (Coefficients, 1, 0.0),
            Numerator (Coefficients, 1, 1.0),
            Start_Weight (Coefficients, V),
            Finish_Weight (Coefficients, V));
      end if;
   end Lemma_Cone_Weights;

   ---------------------------------------------------------------------------
   --  Planar certificates
   ---------------------------------------------------------------------------

   function Planar_Control_Hull_Certified (Coefficients : Bernstein_Chart) return Boolean is
      Start_Value  : constant Dimensionless := Coefficients (0, 1);
      Finish_Value : constant Dimensionless := Coefficients (Fixed_Chart_Degree, 1);
      Lower_Value  : constant Dimensionless := Dimensionless'Min (Start_Value, Finish_Value);
      Upper_Value  : constant Dimensionless := Dimensionless'Max (Start_Value, Finish_Value);
      Cross_Factor : constant Interval :=
        Interval_Add
          (Interval_Exact (1.0), Interval_Multiply (Interval_Exact (Start_Value), Interval_Exact (Finish_Value)));

      Product : constant Interval := Interval_Multiply (Interval_Exact (Start_Value), Interval_Exact (Finish_Value))
      with Ghost;
   begin
      if Cross_Factor.Lower <= 0.0 then
         return False;
      end if;
      --  An invalid interval has a negative lower endpoint, so the cross factor and the product in it are valid,
      --  and they enclose 1 + Yₛ·Yꜰ from below.
      pragma Assert (Cross_Factor.Valid and then Product.Valid);
      pragma Assert (R (Product.Lower) <= R (Start_Value) * R (Finish_Value));
      pragma Assert (R (Cross_Factor.Lower) <= 1.0 + R (Product.Lower));
      pragma Assert (1.0 + R (Start_Value) * R (Finish_Value) > 0.0);
      for I in Chart_Coefficient_Index loop
         if Coefficients (I, 1) < Lower_Value or else Coefficients (I, 1) > Upper_Value then
            return False;
         end if;
         pragma
           Loop_Invariant
             (for all J in 0 .. I => Coefficients (J, 1) >= Lower_Value and then Coefficients (J, 1) <= Upper_Value);
      end loop;
      return True;
   end Planar_Control_Hull_Certified;

   function Planar_No_Bulge_Certified (Coefficients : Bernstein_Chart) return Boolean
   with Refined_Post => Planar_No_Bulge_Certified'Result = Planar.Is_Certified (Coefficients)
   is
   begin
      return Planar.Is_Certified (Coefficients);
   end Planar_No_Bulge_Certified;

   function Spatial_No_Bulge_Certified (Coefficients : Bernstein_Chart) return Boolean
   with Refined_Post => Spatial_No_Bulge_Certified'Result = Spatial.Is_Certified (Coefficients)
   is
   begin
      return Spatial.Is_Certified (Coefficients);
   end Spatial_No_Bulge_Certified;

   function Nonnegative_Projection_Certified
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale) return Boolean
   with
     Refined_Post => Nonnegative_Projection_Certified'Result = Projection.Is_Certified (Frame, Coefficients, Direction)
   is
   begin
      return Projection.Is_Certified (Frame, Coefficients, Direction);
   end Nonnegative_Projection_Certified;

   ---------------------------------------------------------------------------
   --  What the certificates establish
   ---------------------------------------------------------------------------

   procedure Lemma_Planar_Chart_Is_Flat (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Pre  => Is_Structurally_Planar (Coefficients) and then 0.0 <= V and then V <= 1.0,
     Post => Normal_Square (Coefficients, V) = 0.0;
   --  Chart coordinates whose controls all vanish are zero.

   procedure Lemma_Planar_Chart_Is_Flat (Coefficients : Bernstein_Chart; V : Real) is
   begin
      for C in 2 .. 3 loop
         pragma Assert (Bernstein.All_Within (Chart_Controls (Coefficients, C), 0.0, 0.0));
         Bernstein.Lemma_Convex_Hull (Chart_Controls (Coefficients, C), V, 0.0, 0.0);
         pragma Loop_Invariant (for all D in 2 .. C => Chart_Value (Coefficients, D, V) = 0.0);
      end loop;
   end Lemma_Planar_Chart_Is_Flat;

   procedure Lemma_Between_Keeps_Factor_Positive (Ys, Yf, Y : Real)
   with
     Ghost,
     Pre  => Ys < Yf and then Ys <= Y and then Y <= Yf and then 1.0 + Ys * Yf > 0.0,
     Post => 1.0 + Ys * Y > 0.0 and then 1.0 + Yf * Y > 0.0;
   --  1 + Yₛ·Y is linear in Y and positive at both Y = Yₛ and Y = Yꜰ, so it is positive between them; likewise
   --  1 + Yꜰ·Y.

   procedure Lemma_Between_Keeps_Factor_Positive (Ys, Yf, Y : Real) is
   begin
      pragma Assert ((1.0 + Ys * Y) * (Yf - Ys) = (1.0 + Ys * Ys) * (Yf - Y) + (1.0 + Ys * Yf) * (Y - Ys));
      pragma Assert ((1.0 + Yf * Y) * (Yf - Ys) = (1.0 + Ys * Yf) * (Yf - Y) + (1.0 + Yf * Yf) * (Y - Ys));
      pragma Assert ((1.0 + Ys * Ys) * (Yf - Y) >= 0.0 and then (1.0 + Ys * Yf) * (Y - Ys) >= 0.0);
      pragma Assert ((1.0 + Ys * Yf) * (Yf - Y) >= 0.0 and then (1.0 + Yf * Yf) * (Y - Ys) >= 0.0);
      if Y = Ys then
         pragma Assert ((1.0 + Ys * Ys) * (Yf - Y) > 0.0);
         pragma Assert ((1.0 + Ys * Yf) * (Yf - Y) > 0.0);
      else
         pragma Assert ((1.0 + Ys * Yf) * (Y - Ys) > 0.0);
         pragma Assert ((1.0 + Yf * Yf) * (Y - Ys) > 0.0);
      end if;
      pragma Assert ((1.0 + Ys * Y) * (Yf - Ys) > 0.0);
      pragma Assert ((1.0 + Yf * Y) * (Yf - Ys) > 0.0);
   end Lemma_Between_Keeps_Factor_Positive;

   procedure Lemma_Planar_Hull_In_Cone (Ys, Yf, Y : Real)
   with
     Ghost,
     Pre  => 1.0 + Ys * Yf > 0.0 and then ((Ys <= Y and then Y <= Yf) or else (Yf <= Y and then Y <= Ys)),
     Post =>
       ((Yf - Ys) * (1.0 + Ys * Yf) > 0.0
        and then (Y - Ys) * (1.0 + Ys * Y) >= 0.0
        and then (Yf - Y) * (1.0 + Yf * Y) >= 0.0)
       or else
         ((Yf - Ys) * (1.0 + Ys * Yf) < 0.0
          and then (Y - Ys) * (1.0 + Ys * Y) <= 0.0
          and then (Yf - Y) * (1.0 + Yf * Y) <= 0.0)
       or else ((Yf - Ys) * (1.0 + Ys * Yf) = 0.0 and then Y = Ys and then Ys = Yf);
   --  A chart value between the endpoint values gives a tangent in the cone.

   procedure Lemma_Planar_Hull_In_Cone (Ys, Yf, Y : Real) is
   begin
      if Ys < Yf then
         Lemma_Between_Keeps_Factor_Positive (Ys, Yf, Y);
         pragma Assert ((Yf - Ys) * (1.0 + Ys * Yf) > 0.0);
         pragma Assert ((Y - Ys) * (1.0 + Ys * Y) >= 0.0);
         pragma Assert ((Yf - Y) * (1.0 + Yf * Y) >= 0.0);
      elsif Yf < Ys then
         Lemma_Between_Keeps_Factor_Positive (Ys => Yf, Yf => Ys, Y => Y);
         pragma Assert ((Yf - Ys) * (1.0 + Ys * Yf) < 0.0);
         pragma Assert ((Y - Ys) * (1.0 + Ys * Y) <= 0.0);
         pragma Assert ((Yf - Y) * (1.0 + Yf * Y) <= 0.0);
      end if;
   end Lemma_Planar_Hull_In_Cone;

   procedure Lemma_Planar_Control_Hull (Coefficients : Bernstein_Chart; V : Real) is
      Lower_Value : constant Dimensionless :=
        Dimensionless'Min (Coefficients (0, 1), Coefficients (Fixed_Chart_Degree, 1));
      Upper_Value : constant Dimensionless :=
        Dimensionless'Max (Coefficients (0, 1), Coefficients (Fixed_Chart_Degree, 1));
      Y           : constant Real := Chart_Value (Coefficients, 1, V);
   begin
      Lemma_Planar_Chart_Is_Flat (Coefficients, V);
      pragma Assert (Bernstein.All_Within (Chart_Controls (Coefficients, 1), R (Lower_Value), R (Upper_Value)));
      Bernstein.Lemma_Convex_Hull (Chart_Controls (Coefficients, 1), V, R (Lower_Value), R (Upper_Value));
      Lemma_Planar_Hull_In_Cone (Start_Value (Coefficients), Finish_Value (Coefficients), Y);
   end Lemma_Planar_Control_Hull;

   procedure Lemma_Warped_Is_In_Unit_Interval (U, Warp_Factor : Real) is
      Divisor : constant Real := 1.0 - U + Warp_Factor * U;
   begin
      --  The divisor is a convex combination of one and the warp factor, so it is positive and no smaller than
      --  the numerator.
      pragma Assert (Warp_Factor * U >= 0.0);
      pragma Assert (if U < 1.0 then Divisor > 0.0);
      pragma Assert (if U = 1.0 then Divisor = Warp_Factor);
      pragma Assert (Divisor > 0.0);
      pragma Assert (Warp_Factor * U <= Divisor);
      pragma Assert (Warp_Factor * U / Divisor >= 0.0);
      pragma Assert (Warp_Factor * U / Divisor <= 1.0);
   end Lemma_Warped_Is_In_Unit_Interval;

   procedure Lemma_No_Bulge (Coefficients : Bernstein_Chart; V : Real) is
   begin
      if Is_Structurally_Planar (Coefficients) then
         if Planar_Control_Hull_Certified (Coefficients) then
            Lemma_Planar_Control_Hull (Coefficients, V);
         else
            Planar.Lemma_Certified (Coefficients, V);
         end if;
      else
         Spatial.Lemma_Certified (Coefficients, V);
      end if;
   end Lemma_No_Bulge;

   procedure Lemma_Nonnegative_Projection
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale; V : Real) is
   begin
      Projection.Lemma_Certified (Frame, Coefficients, Direction, V);
   end Lemma_Nonnegative_Projection;

   procedure Lemma_Ideal_Geometry
     (Frame           : Frame_Vector_Array;
      Coefficients    : Bernstein_Chart;
      Bulge_Allowed   : Boolean;
      Chord_Direction : Position_Scale;
      V               : Real) is
   begin
      if No_Bulge_Certified (Coefficients) then
         Lemma_No_Bulge (Coefficients, V);
      else
         Lemma_Nonnegative_Projection (Frame, Coefficients, Chord_Direction, V);
      end if;
   end Lemma_Ideal_Geometry;

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
