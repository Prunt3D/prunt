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

--  That the two functions which the sampled tangent certificate samples are polynomials of the chart coordinate
--  with at most thirty-three coefficients, which is what lets their values at thirty-three nodes bound them
--  everywhere. Everything here is ghost code.

with Prunt.Motion_Planner.Stereographic_Kernel.Polynomials;
with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Model;

package Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Structure
  with SPARK_Mode => On, Ghost, Always_Terminates
is
   use type Prunt.Motion_Planner.Reals.Real;

   Largest_Degree : constant := 2 * Maximum_Rational_Degree;

   function Is_Regular (Warp : Dimensionless; Cache : Rational_Antiderivative) return Boolean
   is (Warp > 0.0 and then Has_Layout (Cache) and then Poles_Are_Off_The_Curve (Cache));
   --  What the algebra needs: a positive warp, and no pole on the curve.

   procedure Lemma_Distance (Warp : Dimensionless; V : Real)
   with
     Pre  => Warp > 0.0 and then 0.0 <= V and then V <= 1.0,
     Post =>
       Model.Warp_Scale (Warp, V) > 0.0
       and then 0.0 <= Model.Distance (Warp, V)
       and then Model.Distance (Warp, V) <= 1.0;
   --  Every chart coordinate of the curve is at a normalized distance in [0, 1].

   procedure Lemma_Distance_Of_Warped (Warp : Dimensionless; U : Real)
   with Pre => Warp > 0.0 and then 0.0 <= U and then U <= 1.0, Post => Model.Distance (Warp, Warped (U, R (Warp))) = U;
   --  Distance undoes the warp: the point at normalized distance U has chart coordinate Warped (U, W).

   function Denominator_Error_Polynomial
     (Coefficients      : Bernstein_Chart;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Warp              : Dimensionless) return Polynomials.Polynomial
   with
     Pre  => Is_Regular (Warp, Cache),
     Post =>
       Polynomials.Is_Polynomial (Denominator_Error_Polynomial'Result)
       and then Denominator_Error_Polynomial'Result'Last <= Largest_Degree;
   --  The coefficients of e.

   procedure Lemma_Denominator_Error
     (Coefficients      : Bernstein_Chart;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Warp              : Dimensionless;
      V                 : Real)
   with
     Pre  => Is_Regular (Warp, Cache) and then 0.0 <= V and then V <= 1.0,
     Post =>
       Polynomials.Eval (Denominator_Error_Polynomial (Coefficients, Cache, Denominator_Scale, Warp), V)
       = Model.Denominator_Error (Coefficients, Cache, Denominator_Scale, Warp, V);

   function Numerator_Error_Polynomial
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Axis              : Axis_Name) return Polynomials.Polynomial
   with
     Pre  => Is_Regular (Warp, Cache),
     Post =>
       Polynomials.Is_Polynomial (Numerator_Error_Polynomial'Result)
       and then Numerator_Error_Polynomial'Result'Last <= Largest_Degree;
   --  The coefficients of g on one axis.

   procedure Lemma_Numerator_Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Axis              : Axis_Name;
      V                 : Real)
   with
     Pre  => Is_Regular (Warp, Cache) and then 0.0 <= V and then V <= 1.0,
     Post =>
       Polynomials.Eval (Numerator_Error_Polynomial (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis), V)
       = Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, V);
   --  On the curve, where no retained term has a zero denominator, g is that polynomial.

   procedure Lemma_Tangent_Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Axis              : Axis_Name;
      V                 : Real)
   with
     Pre  => Has_Layout (Cache),
     Post =>
       Model.Numerator_Error (Frame, Coefficients, Warp, Cache, Denominator_Scale, Axis, V)
       = Model.Cached_Denominator (Cache, Denominator_Scale, Warp, V)
         * Denominator (Coefficients, V)
         * Model.Tangent_Error (Frame, Coefficients, Warp, Cache, Axis, V);
   --  g = Q·D·(P/D - R).

   function Vanishes_On_Axis
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Cache : Rational_Antiderivative; Axis : Axis_Name)
      return Boolean
   is (Cache.Constant_Tangent (Axis) = 0.0
       and then Frame (0) (Axis) = 0.0
       and then
         (for all Component in Chart_Component_Index =>
            Frame (Component) (Axis) = 0.0
            or else (for all K in Chart_Coefficient_Index => Coefficients (K, Component) = 0.0))
       and then
         (for all Slot in Rational_Degree_Slot =>
            (if Slot <= Cache.Real_Pole_Count + 2 * Cache.Pair_Count
             then Cache.Pole_Slots (Slot).Residue_Component (Axis) = 0.0)))
   with Pre => Has_Layout (Cache);
   --  Whether both tangents vanish identically on an axis: the conditions under which the certificate leaves
   --  the axis out.

   procedure Lemma_Inactive_Axis
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Warp         : Dimensionless;
      Cache        : Rational_Antiderivative;
      Axis         : Axis_Name;
      V            : Real)
   with
     Pre  => Has_Layout (Cache) and then Vanishes_On_Axis (Frame, Coefficients, Cache, Axis),
     Post => Model.Tangent_Error (Frame, Coefficients, Warp, Cache, Axis, V) = 0.0;
   --  Such an axis contributes exactly zero error.

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Structure;
