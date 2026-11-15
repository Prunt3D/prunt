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

--  The functions of the chart coordinate V which the sampled tangent certificate compares, over the real numbers
--  which the stored floating-point data denote. Sampled_Tangent explains them. Everything here is ghost code.

with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed;
with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Model
  with SPARK_Mode => On, Ghost
is
   use type Prunt.Motion_Planner.Reals.Real;

   function Warp_Scale (Warp : Dimensionless; V : Real) return Real
   is (R (Warp) * (1.0 - V) + V);
   --  h(V) = W·(1 - V) + V, which is positive on [0, 1] for a positive warp.

   function Distance (Warp : Dimensionless; V : Real) return Real
   is (if Warp_Scale (Warp, V) = 0.0 then 0.0 else V / Warp_Scale (Warp, V));
   --  U(V) = V/h(V): the normalized physical distance at chart coordinate V. It is the inverse of
   --  Ideal_Geometry.Warped.

   ---------------------------------------------------------------------------
   --  The cached denominator Q
   ---------------------------------------------------------------------------

   function Real_Factor
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Warp : Dimensionless; V : Real) return Real
   is (if R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp) = 0.0
       then 1.0
       else
         (R (Cache.Pole_Slots (Slot).Pole_Component) * Warp_Scale (Warp, V) - V)
         / (R (Cache.Pole_Slots (Slot).Pole_Component) * R (Warp)));
   --  (V - p·h(V))/(-p·W) = (h(V)/W)·(p - U)/p: linear in V, and one at V = 0.

   function Pair_Norm (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Real
   is (R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component)
       * R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component)
       + R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component)
         * R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component))
   with Pre => Has_Layout (Cache) and then Pair <= Cache.Pair_Count;
   --  |π|² = A² + B².

   function Pair_Factor
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Warp : Dimensionless; V : Real) return Real
   is (if Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp)) = 0.0
       then 1.0
       else
         ((V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Warp_Scale (Warp, V))
          * (V - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component) * Warp_Scale (Warp, V))
          + (R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * Warp_Scale (Warp, V))
            * (R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component) * Warp_Scale (Warp, V)))
         / (Pair_Norm (Cache, Pair) * (R (Warp) * R (Warp))))
   with Pre => Has_Layout (Cache) and then Pair <= Cache.Pair_Count;
   --  |V - π·h(V)|²/(|π|²·W²) = (h(V)/W)²·|U - π|²/|π|²: quadratic in V, and one at V = 0.

   function Real_Product (Cache : Rational_Antiderivative; Count : Natural; Warp : Dimensionless; V : Real) return Real
   is (if Count = 0 then 1.0 else Real_Product (Cache, Count - 1, Warp, V) * Real_Factor (Cache, Count, Warp, V))
   with Pre => Count <= Cache.Real_Pole_Count, Subprogram_Variant => (Decreases => Count);
   --  The factors of the first Count real poles.

   function Pair_Product (Cache : Rational_Antiderivative; Count : Natural; Warp : Dimensionless; V : Real) return Real
   is (if Count = 0 then 1.0 else Pair_Product (Cache, Count - 1, Warp, V) * Pair_Factor (Cache, Count, Warp, V))
   with Pre => Has_Layout (Cache) and then Count <= Cache.Pair_Count, Subprogram_Variant => (Decreases => Count);
   --  The factors of the first Count pairs.

   function Cached_Denominator
     (Cache : Rational_Antiderivative; Denominator_Scale, Warp : Dimensionless; V : Real) return Real
   is (R (Denominator_Scale)
       * Real_Product (Cache, Cache.Real_Pole_Count, Warp, V)
       * Pair_Product (Cache, Cache.Pair_Count, Warp, V))
   with Pre => Has_Layout (Cache);
   --  Q(V).

   ---------------------------------------------------------------------------
   --  What is sampled, and what is bounded
   ---------------------------------------------------------------------------

   function Denominator_Error
     (Coefficients      : Bernstein_Chart;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Warp              : Dimensionless;
      V                 : Real) return Real
   is (Cached_Denominator (Cache, Denominator_Scale, Warp, V) - Denominator (Coefficients, V))
   with Pre => Has_Layout (Cache);
   --  e(V) = Q(V) - D(V).

   function Ideal_Tangent
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real) return Real
   is (Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, V) / Denominator (Coefficients, V));
   --  One machine-axis component of the ideal tangent, P(V)/D(V).

   function Tangent_Error
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Warp         : Dimensionless;
      Cache        : Rational_Antiderivative;
      Axis         : Axis_Name;
      V            : Real) return Real
   is (Ideal_Tangent (Frame, Coefficients, Axis, V) - Retained_Tangent.Value (Cache, Axis, Distance (Warp, V)))
   with Pre => Has_Layout (Cache);
   --  The difference between the ideal tangent at V and the retained tangent at the same point of the curve.

   function Numerator_Error
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Axis              : Axis_Name;
      V                 : Real) return Real
   is (Cached_Denominator (Cache, Denominator_Scale, Warp, V)
       * (Ideal_Geometry.Unit_Speed.Tangent_Numerator (Frame, Coefficients, Axis, V)
          - Denominator (Coefficients, V) * Retained_Tangent.Value (Cache, Axis, Distance (Warp, V))))
   with Pre => Has_Layout (Cache);
   --  g(V) = Q(V)·D(V)·(P(V)/D(V) - R(U(V))).

   function Squared_Tangent_Error
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Warp         : Dimensionless;
      Cache        : Rational_Antiderivative;
      V            : Real) return Real
   is (Tangent_Error (Frame, Coefficients, Warp, Cache, X_Axis, V)
       * Tangent_Error (Frame, Coefficients, Warp, Cache, X_Axis, V)
       + Tangent_Error (Frame, Coefficients, Warp, Cache, Y_Axis, V)
         * Tangent_Error (Frame, Coefficients, Warp, Cache, Y_Axis, V)
       + Tangent_Error (Frame, Coefficients, Warp, Cache, Z_Axis, V)
         * Tangent_Error (Frame, Coefficients, Warp, Cache, Z_Axis, V)
       + Tangent_Error (Frame, Coefficients, Warp, Cache, E_Axis, V)
         * Tangent_Error (Frame, Coefficients, Warp, Cache, E_Axis, V))
   with Pre => Has_Layout (Cache);
   --  The squared Euclidean distance between the two tangents.

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Model;
