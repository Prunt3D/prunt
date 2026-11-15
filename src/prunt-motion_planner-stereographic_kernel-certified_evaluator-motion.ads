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

--  What the certificates guarantee about the motion which is executed.
--
--  The realtime evaluator returns, at normalized distance U along a curve of length L,
--
--     Start + L·∫₀ᵁ R + H(U)·(Finish - Uncorrected),
--
--  with R the retained tangent and the second term the endpoint correction (see Retained_Tangent and
--  Finish_Correction). Its derivative with respect to physical distance, the tangent of the executed motion, is
--
--     R(U) + H′(U)·(Finish - Uncorrected)/L,
--
--  which is Executed_Tangent below, for the real numbers which the stored floating-point data denote. The
--  theorems here are about that function: it stays within a certified distance of the ideal tangent, hence in
--  the cone of the endpoint tangents up to that distance (or, where bulging was allowed, never travels backwards
--  along the chord by more), and its length is within a certified distance of one, so that the executed path has
--  the length the planner assumes to within the same fraction.
--
--  That the closed form which the evaluator computes is the integral above is calculus, and how closely its
--  floating-point evaluation, with the library's logarithm and arctangent, follows that closed form is not
--  covered here. Everything here is ghost code.

with Prunt.Motion_Planner.Stereographic_Kernel.Finish_Correction;
with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed;

package Prunt.Motion_Planner.Stereographic_Kernel.Certified_Evaluator.Motion
  with SPARK_Mode => On, Ghost
is

   Defect_Allowance : constant := Certified_Geometry.Defect_Allowance;
   --  Added to a frame's measured defect to cover the rounding of the measurement.

   type Real_Vector is array (Axis_Name) of Real;
   --  A vector on the machine axes.

   function Is_Evaluator
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Total        : Dimensionless) return Boolean
   is (Kind (Value) = Sampled
       and then Total < Dimensionless'Last
       and then Finish_Correction.Is_Total_Bound (Tangent_Error (Value), Finish, Uncorrected, Length_Value, Total));
   --  Whether Value certifies the cache of an evaluator with these finish points and this length, all in
   --  millimetres, and Total is a bound on its tangent error which includes the endpoint correction: what
   --  Finish_Correction.Total_Tangent_Error_Bound returns for Tangent_Error (Value).

   function Executed_Tangent
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Axis         : Axis_Name;
      U            : Real) return Real
   is (Retained_Value (Value, Axis, U)
       + Finish_Correction.Rate (U) * (R (Finish (Axis)) - R (Uncorrected (Axis))) / R (Length_Value))
   with Pre => Has_Layout (Cache (Value)) and then Length_Value > 0.0;
   --  One machine-axis component of the tangent of the executed motion at normalized distance U.

   function Dot_Executed
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Direction    : Real_Vector;
      U            : Real) return Real
   is (Direction (X_Axis) * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, X_Axis, U)
       + Direction (Y_Axis) * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Y_Axis, U)
       + Direction (Z_Axis) * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Z_Axis, U)
       + Direction (E_Axis) * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, E_Axis, U))
   with Pre => Has_Layout (Cache (Value)) and then Length_Value > 0.0;
   --  The component of the executed tangent along Direction.

   function Dot_Ideal (Value : Certificate; Direction : Real_Vector; U : Real) return Real
   is (Direction (X_Axis) * Ideal_Tangent (Value, X_Axis, U) + Direction (Y_Axis) * Ideal_Tangent (Value, Y_Axis, U)
       + Direction (Z_Axis) * Ideal_Tangent (Value, Z_Axis, U)
       + Direction (E_Axis) * Ideal_Tangent (Value, E_Axis, U))
   with Pre => 0.0 <= U and then U <= 1.0 and then Warp (Value) > 0.0;
   --  The component of the ideal tangent along Direction.

   function Dot (Left, Right : Real_Vector) return Real
   is (Left (X_Axis) * Right (X_Axis) + Left (Y_Axis) * Right (Y_Axis) + Left (Z_Axis) * Right (Z_Axis)
       + Left (E_Axis) * Right (E_Axis));

   function Dot_Frame (Value : Certificate; Component : Frame_Component_Index; Direction : Real_Vector) return Real
   is (Direction (X_Axis) * R (Certified_Geometry.Frame (Geometry (Value)) (Component) (X_Axis))
       + Direction (Y_Axis) * R (Certified_Geometry.Frame (Geometry (Value)) (Component) (Y_Axis))
       + Direction (Z_Axis) * R (Certified_Geometry.Frame (Geometry (Value)) (Component) (Z_Axis))
       + Direction (E_Axis) * R (Certified_Geometry.Frame (Geometry (Value)) (Component) (E_Axis)));
   --  The component of a frame vector along Direction.

   function Endpoint_Numerator (Value : Certificate; At_Finish : Boolean) return Real_Vector
   with
     Post =>
       (for all Axis in Axis_Name =>
          Endpoint_Numerator'Result (Axis)
          = Ideal_Geometry.Unit_Speed.Tangent_Numerator
              (Certified_Geometry.Frame (Geometry (Value)),
               Certified_Geometry.Coefficients (Geometry (Value)),
               Axis,
               (if At_Finish then Real'(1.0) else Real'(0.0))));
   --  The ideal tangent at the start or at the finish of the curve, times its positive denominator there: a
   --  positive multiple of the endpoint tangent.

   ---------------------------------------------------------------------------
   --  The theorems
   ---------------------------------------------------------------------------

   procedure Lemma_Executed_Tangent
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Total        : Dimensionless;
      Direction    : Real_Vector;
      U            : Real)
   with
     Pre  => Is_Evaluator (Value, Finish, Uncorrected, Length_Value, Total) and then 0.0 <= U and then U <= 1.0,
     Post =>
       Warp (Value) > 0.0
       and then Has_Layout (Cache (Value))
       and then Poles_Are_Off_The_Curve (Cache (Value))
       and then Length_Value > 0.0
       and then
         (Dot_Executed (Value, Finish, Uncorrected, Length_Value, Direction, U) - Dot_Ideal (Value, Direction, U))
         * (Dot_Executed (Value, Finish, Uncorrected, Length_Value, Direction, U) - Dot_Ideal (Value, Direction, U))
         <= Dot (Direction, Direction) * (R (Total) * R (Total));
   --  Everywhere on the curve, and along every direction, the executed tangent differs from the ideal tangent by
   --  at most Total times the length of the direction: the two tangents are within Total of each other.

   procedure Lemma_Executed_In_Cone
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Total        : Dimensionless;
      Normal       : Real_Vector;
      U            : Real)
   with
     Pre  =>
       Is_Evaluator (Value, Finish, Uncorrected, Length_Value, Total)
       and then Certified_Geometry.Kind (Geometry (Value)) = Certified_Geometry.In_Cone
       and then 0.0 <= U
       and then U <= 1.0
       and then Dot (Normal, Normal) <= 1.0
       and then Dot_Frame (Value, 2, Normal) = 0.0
       and then Dot_Frame (Value, 3, Normal) = 0.0
       and then Dot (Normal, Endpoint_Numerator (Value, At_Finish => False)) >= 0.0
       and then Dot (Normal, Endpoint_Numerator (Value, At_Finish => True)) >= 0.0,
     Post =>
       Has_Layout (Cache (Value))
       and then Length_Value > 0.0
       and then Dot_Executed (Value, Finish, Uncorrected, Length_Value, Normal, U) >= 0.0 - R (Total);
   --  The executed motion stays in the cone of the endpoint tangents, up to Total. A cone in the plane of the
   --  first two frame vectors is the intersection of the half-planes n·T ≥ 0 over its inward normals n: the
   --  directions in that plane, which is to say orthogonal to the other two frame vectors, with a nonnegative
   --  component along both endpoint tangents. For every such normal of length at most one, the executed tangent's
   --  component along it is at least -Total, everywhere on the curve. Travel along the other two frame vectors is
   --  unrestricted, as for the ideal tangent.

   procedure Lemma_Executed_Chord_Progress
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Total        : Dimensionless;
      Chord_Length : Real;
      U            : Real)
   with
     Pre  =>
       Is_Evaluator (Value, Finish, Uncorrected, Length_Value, Total)
       and then Certified_Geometry.Kind (Geometry (Value)) = Certified_Geometry.Chord_Progress
       and then 0.0 <= U
       and then U <= 1.0
       and then Chord_Length >= 0.0
       and then
         Dot_Real
           (Certified_Geometry.Chord_Direction (Geometry (Value)),
            Certified_Geometry.Chord_Direction (Geometry (Value)))
         <= Chord_Length * Chord_Length,
     Post =>
       Has_Layout (Cache (Value))
       and then Length_Value > 0.0
       and then
         R (Certified_Geometry.Chord_Direction (Geometry (Value)) (X_Axis))
         * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, X_Axis, U)
         + R (Certified_Geometry.Chord_Direction (Geometry (Value)) (Y_Axis))
           * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Y_Axis, U)
         + R (Certified_Geometry.Chord_Direction (Geometry (Value)) (Z_Axis))
           * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Z_Axis, U)
         + R (Certified_Geometry.Chord_Direction (Geometry (Value)) (E_Axis))
           * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, E_Axis, U)
         >= 0.0 - Chord_Length * R (Total);
   --  Where bulging was allowed and the cone was not certified: the executed motion never travels backwards
   --  along the chord direction by more than Total times the length of that direction, which is one to within
   --  rounding.

   procedure Lemma_Executed_Speed
     (Value        : Certificate;
      Finish       : Dimensionless_Axis_Vector;
      Uncorrected  : Dimensionless_Axis_Vector;
      Length_Value : Dimensionless;
      Total        : Dimensionless;
      U            : Real)
   with
     Pre  =>
       Is_Evaluator (Value, Finish, Uncorrected, Length_Value, Total)
       and then Certified_Geometry.Frame_Defect (Geometry (Value)) < Dimensionless'Last
       and then 0.0 <= U
       and then U <= 1.0,
     Post =>
       Has_Layout (Cache (Value))
       and then Length_Value > 0.0
       and then
         abs (Executed_Tangent (Value, Finish, Uncorrected, Length_Value, X_Axis, U)
              * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, X_Axis, U)
              + Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Y_Axis, U)
                * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Y_Axis, U)
              + Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Z_Axis, U)
                * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, Z_Axis, U)
              + Executed_Tangent (Value, Finish, Uncorrected, Length_Value, E_Axis, U)
                * Executed_Tangent (Value, Finish, Uncorrected, Length_Value, E_Axis, U)
              - 1.0)
         <= 4.0 * (R (Certified_Geometry.Frame_Defect (Geometry (Value))) + Defect_Allowance)
            + 2.0
              * (1.0 + 2.0 * (R (Certified_Geometry.Frame_Defect (Geometry (Value))) + Defect_Allowance))
              * R (Total)
            + R (Total) * R (Total);
   --  The executed motion has unit speed up to the frame's defect and the certified tangent error: with δ the
   --  defect and its allowance and ε = Total, the squared speed is within 4·δ + 2·(1 + 2·δ)·ε + ε² of one,
   --  everywhere on the curve. The length of the executed path is the integral of the speed over the planner's
   --  distance, so it is the planner's length to within the same fraction.

end Prunt.Motion_Planner.Stereographic_Kernel.Certified_Evaluator.Motion;
