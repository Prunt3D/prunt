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

--  The certificates that the ideal tangent of a blend stays inside the cone of its endpoint tangents or, where
--  bulging is allowed, never travels backwards along the chord.
--
--  The ideal tangent is the inverse stereographic projection of a degree-eight polynomial chart Y(V), resolved on a
--  frame of four vectors F₀ .. F₃:
--
--     T(V) = (F₀·(1 - Y·Y) + 2·(F₁·Y₁ + F₂·Y₂ + F₃·Y₃)) / (1 + Y·Y),   0 ≤ V ≤ 1.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Prunt.Motion_Planner.Stereographic_Kernel.Bernstein;
with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   Fixed_Chart_Degree : constant := 8;
   --  Degree of the polynomial chart. Degree eight provides nine coefficients: eight satisfy the tangent value and
   --  first three derivatives at both endpoints, while the remaining mode is available to close the curve position.

   subtype Chart_Coefficient_Index is Natural range 0 .. Fixed_Chart_Degree;
   --  Index of a coefficient in the fixed degree-eight chart.

   subtype Chart_Component_Index is Positive range 1 .. 3;
   --  Index of one coordinate in the three-dimensional stereographic chart used to represent a unit tangent.

   subtype Frame_Component_Index is Natural range 0 .. 3;
   --  Index of a basis vector in the four-dimensional motion space. Component zero defines the axis opposite the
   --  stereographic projection pole; components one through three correspond to Chart_Component_Index.

   type Bernstein_Chart is array (Chart_Coefficient_Index, Chart_Component_Index) of Dimensionless;
   --  The three chart polynomials stored as Bernstein control coefficients.

   type Frame_Vector_Array is array (Frame_Component_Index) of Position_Scale;
   --  A complete basis for the four machine axes. Each element is one basis vector expressed in machine-axis
   --  coordinates.

   ---------------------------------------------------------------------------
   --  The ideal tangent over the real numbers
   ---------------------------------------------------------------------------

   function Chart_Controls
     (Coefficients : Bernstein_Chart; Component : Chart_Component_Index) return Bernstein.Controls
   with
     Ghost,
     Post =>
       Chart_Controls'Result'First = 0
       and then Chart_Controls'Result'Last = Fixed_Chart_Degree
       and then (for all I in Chart_Coefficient_Index => Chart_Controls'Result (I) = R (Coefficients (I, Component)));
   --  The controls of one chart coordinate, as the real numbers which the stored coefficients denote.

   function Chart_Value (Coefficients : Bernstein_Chart; Component : Chart_Component_Index; V : Real) return Real
   is (Bernstein.Value (Chart_Controls (Coefficients, Component), V))
   with Ghost;
   --  Y꜀(V).

   function Normal_Square (Coefficients : Bernstein_Chart; V : Real) return Real
   is (Chart_Value (Coefficients, 2, V) * Chart_Value (Coefficients, 2, V)
       + Chart_Value (Coefficients, 3, V) * Chart_Value (Coefficients, 3, V))
   with Ghost;
   --  W(V) = Y₂² + Y₃², which measures how far the tangent has left the plane of the first two frame vectors.

   function Denominator (Coefficients : Bernstein_Chart; V : Real) return Real
   is (1.0 + Chart_Value (Coefficients, 1, V) * Chart_Value (Coefficients, 1, V) + Normal_Square (Coefficients, V))
   with Ghost, Post => Denominator'Result >= 1.0;
   --  1 + Y·Y, which divides every component of the tangent.

   function Numerator (Coefficients : Bernstein_Chart; Component : Frame_Component_Index; V : Real) return Real
   is (if Component = 0
       then 1.0 - Chart_Value (Coefficients, 1, V) * Chart_Value (Coefficients, 1, V) - Normal_Square (Coefficients, V)
       else 2.0 * Chart_Value (Coefficients, Component, V))
   with Ghost;
   --  The component of the tangent along one frame vector, times Denominator: 1 - Y·Y, then 2·Y꜀.

   function Start_Value (Coefficients : Bernstein_Chart) return Real
   is (R (Coefficients (0, 1)))
   with Ghost;
   --  Yₛ, the first chart coordinate of the start tangent.

   function Finish_Value (Coefficients : Bernstein_Chart) return Real
   is (R (Coefficients (Fixed_Chart_Degree, 1)))
   with Ghost;
   --  Yꜰ, the first chart coordinate of the finish tangent.

   function Turn (Coefficients : Bernstein_Chart) return Real
   is ((Finish_Value (Coefficients) - Start_Value (Coefficients))
       * (1.0 + Start_Value (Coefficients) * Finish_Value (Coefficients)))
   with Ghost;
   --  Half the determinant of the start and finish tangent numerators in the frame plane. Its sign is that of the
   --  turn from the start tangent to the finish tangent.

   function Start_Determinant (Coefficients : Bernstein_Chart; V : Real) return Real
   is ((Chart_Value (Coefficients, 1, V) - Start_Value (Coefficients))
       * (1.0 + Start_Value (Coefficients) * Chart_Value (Coefficients, 1, V))
       + Start_Value (Coefficients) * Normal_Square (Coefficients, V))
   with Ghost;
   --  Half the determinant of the start tangent numerator and the frame-plane part of the tangent numerator at V.

   function Finish_Determinant (Coefficients : Bernstein_Chart; V : Real) return Real
   is ((Finish_Value (Coefficients) - Chart_Value (Coefficients, 1, V))
       * (1.0 + Finish_Value (Coefficients) * Chart_Value (Coefficients, 1, V))
       - Finish_Value (Coefficients) * Normal_Square (Coefficients, V))
   with Ghost;
   --  Half the determinant of the frame-plane part of the tangent numerator at V and the finish tangent numerator.

   function Is_In_Cone (Coefficients : Bernstein_Chart; V : Real) return Boolean
   is ((Turn (Coefficients) > 0.0
        and then Start_Determinant (Coefficients, V) >= 0.0
        and then Finish_Determinant (Coefficients, V) >= 0.0)
       or else
         (Turn (Coefficients) < 0.0
          and then Start_Determinant (Coefficients, V) <= 0.0
          and then Finish_Determinant (Coefficients, V) <= 0.0)
       or else
         (Turn (Coefficients) = 0.0
          and then Chart_Value (Coefficients, 1, V) = Start_Value (Coefficients)
          and then Start_Value (Coefficients) = Finish_Value (Coefficients)
          and then Normal_Square (Coefficients, V) = 0.0))
   with Ghost;
   --  Whether the frame-plane part of the tangent at V lies in the cone of the endpoint tangents: on the finish
   --  side of the start tangent and on the start side of the finish tangent, for whichever way the cone turns. In
   --  the last case the cone has collapsed to one ray, and the tangent is that ray.

   function Start_Weight (Coefficients : Bernstein_Chart; V : Real) return Real
   is (if Turn (Coefficients) = 0.0 then 1.0 else Finish_Determinant (Coefficients, V) / Turn (Coefficients))
   with Ghost;

   function Finish_Weight (Coefficients : Bernstein_Chart; V : Real) return Real
   is (if Turn (Coefficients) = 0.0 then 0.0 else Start_Determinant (Coefficients, V) / Turn (Coefficients))
   with Ghost;
   --  The weights of the start and finish tangent numerators which combine to the frame-plane part of the tangent
   --  numerator at V.

   function Endpoints_Are_In_Frame_Plane (Coefficients : Bernstein_Chart) return Boolean
   is (for all C in 2 .. 3 => Coefficients (0, C) = 0.0 and then Coefficients (Fixed_Chart_Degree, C) = 0.0);
   --  Whether both endpoint tangents lie in the plane of the first two frame vectors.

   procedure Lemma_Cone_Weights (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Pre  => Endpoints_Are_In_Frame_Plane (Coefficients) and then Is_In_Cone (Coefficients, V),
     Post =>
       Start_Weight (Coefficients, V) >= 0.0
       and then Finish_Weight (Coefficients, V) >= 0.0
       and then
         Numerator (Coefficients, 0, V)
         = Start_Weight (Coefficients, V) * Numerator (Coefficients, 0, 0.0)
           + Finish_Weight (Coefficients, V) * Numerator (Coefficients, 0, 1.0)
       and then
         Numerator (Coefficients, 1, V)
         = Start_Weight (Coefficients, V) * Numerator (Coefficients, 1, 0.0)
           + Finish_Weight (Coefficients, V) * Numerator (Coefficients, 1, 1.0)
       and then Numerator (Coefficients, 2, 0.0) = 0.0
       and then Numerator (Coefficients, 3, 0.0) = 0.0
       and then Numerator (Coefficients, 2, 1.0) = 0.0
       and then Numerator (Coefficients, 3, 1.0) = 0.0;
   --  A tangent in the cone is a nonnegative combination of the two endpoint tangents, up to its part outside the
   --  frame plane: its numerator along the first two frame vectors is Start_Weight times that of the start
   --  tangent plus Finish_Weight times that of the finish tangent, and the endpoint tangents have no other part.

   ---------------------------------------------------------------------------
   --  Certificates
   ---------------------------------------------------------------------------

   function Is_Structurally_Planar (Coefficients : Bernstein_Chart) return Boolean
   is (for all C in 2 .. 3 => (for all I in Chart_Coefficient_Index => Coefficients (I, C) = 0.0));
   --  Whether the last two chart coordinates vanish identically, so that the tangent never leaves the plane of the
   --  first two frame vectors.

   function Planar_Control_Hull_Certified (Coefficients : Bernstein_Chart) return Boolean
   with
     Pre  => Is_Structurally_Planar (Coefficients),
     Post =>
       (if Planar_Control_Hull_Certified'Result
        then
          1.0 + Start_Value (Coefficients) * Finish_Value (Coefficients) > 0.0
          and then
            (for all I in Chart_Coefficient_Index =>
               Coefficients (I, 1) >= Dimensionless'Min (Coefficients (0, 1), Coefficients (Fixed_Chart_Degree, 1))
               and then
                 Coefficients (I, 1)
                 <= Dimensionless'Max (Coefficients (0, 1), Coefficients (Fixed_Chart_Degree, 1))));
   --  A scalar Bernstein polynomial lies in the convex hull of its controls. Keeping every control between the two
   --  endpoint chart values therefore keeps the entire ideal tangent in their cone; monotonicity in the parameter is
   --  unnecessary. A positive cross factor selects the minor inverse-stereographic arc in one open hemisphere.

   function Planar_No_Bulge_Certified (Coefficients : Bernstein_Chart) return Boolean
   with Pre => Is_Structurally_Planar (Coefficients);
   --  For the planar inverse-stereographic tangent
   --
   --     S(Y) = ((1 - Y²), 2Y)/(1 + Y²),
   --
   --  the two endpoint-cone determinants factor as
   --
   --     (Y - Yₛ)·(1 + Yₛ·Y),   (Yꜰ - Y)·(1 + Yꜰ·Y).
   --
   --  Every Bernstein chart has the exact endpoint factors Y - Yₛ = V·Qₛ and Yꜰ - Y = (1 - V)·Qꜰ. Structurally flat
   --  endpoint controls expose fourth-order factors instead. The strongest available quotient is formed directly in
   --  Bernstein form, and the remaining products are certified with outward-rounded intervals.

   function Spatial_No_Bulge_Certified (Coefficients : Bernstein_Chart) return Boolean;
   --  The frame-plane projection of the tangent numerator is (1 - |Y|², 2·Y₁). With both endpoint tangents in that
   --  plane (Y₂ = Y₃ = 0 at both ends) and W = Y₂² + Y₃², the cone determinants are
   --
   --     (Y₁ - Yₛ)·(1 + Yₛ·Y₁) + Yₛ·W,   (Yꜰ - Y₁)·(1 + Yꜰ·Y₁) - Yꜰ·W.
   --
   --  They reduce to the planar factors when W = 0. Every term has an exact endpoint root: Y₁ - Yₛ = Vᵏ·Q and
   --  Y₂, Y₃ = Vᵐ·P at the start, and likewise in 1 - V at the finish. The common power is divided out so that a
   --  curve which leaves an endpoint along the cone boundary still certifies.

   function Nonnegative_Projection_Certified
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale) return Boolean;
   --  Certify that the inverse-stereographic tangent numerator
   --
   --     Frame₀·(1 - Y·Y) + 2·Σ꜀Frame꜀·Y꜀,
   --
   --  projected onto Direction, is nonnegative. It is formed directly from the authoritative degree-eight Bernstein
   --  chart. Multiplication by the degree-eight constant-one polynomial both elevates Y to degree sixteen and keeps
   --  every conversion outward-rounded.

   function No_Bulge_Certified (Coefficients : Bernstein_Chart) return Boolean
   is (if Is_Structurally_Planar (Coefficients)
       then Planar_Control_Hull_Certified (Coefficients) or else Planar_No_Bulge_Certified (Coefficients)
       else Spatial_No_Bulge_Certified (Coefficients));
   --  Certify that the tangent's projection onto the endpoint-tangent plane stays inside the endpoint cone.

   function Ideal_Geometry_Is_Certified
     (Frame           : Frame_Vector_Array;
      Coefficients    : Bernstein_Chart;
      Bulge_Allowed   : Boolean;
      Chord_Direction : Position_Scale) return Boolean
   is (No_Bulge_Certified (Coefficients)
       or else (Bulge_Allowed and then Nonnegative_Projection_Certified (Frame, Coefficients, Chord_Direction)));
   --  Certify that the ideal tangent stays inside the endpoint tangent cone or, where Bulge_Allowed, that its
   --  projection onto the chord is nonnegative.
   --
   --  Allowing bulge is a relaxation of the default geometry, not a switch to a disjoint acceptance rule. A
   --  certified no-bulge candidate is retained first; only a candidate which leaves the endpoint tangent cone needs
   --  the chord-monotonicity proof. This matters for obtuse asymmetric line corners, whose fixed endpoint tangent
   --  can point backwards along the chord even though the complete curve stays inside its tangent cone.

   ---------------------------------------------------------------------------
   --  What the certificates establish
   ---------------------------------------------------------------------------

   function Dot_Real (Left, Right : Position_Scale) return Real
   is (R (Left (X_Axis)) * R (Right (X_Axis)) + R (Left (Y_Axis)) * R (Right (Y_Axis))
       + R (Left (Z_Axis)) * R (Right (Z_Axis))
       + R (Left (E_Axis)) * R (Right (E_Axis)))
   with Ghost;
   --  The exact dot product of two stored vectors.

   function Projected_Numerator
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale; V : Real) return Real
   is (Numerator (Coefficients, 0, V) * Dot_Real (Frame (0), Direction)
       + Numerator (Coefficients, 1, V) * Dot_Real (Frame (1), Direction)
       + Numerator (Coefficients, 2, V) * Dot_Real (Frame (2), Direction)
       + Numerator (Coefficients, 3, V) * Dot_Real (Frame (3), Direction))
   with Ghost;
   --  The tangent at V, times Denominator, projected onto Direction: the tangent's frame components resolved on
   --  the machine axes by the frame vectors.

   procedure Lemma_No_Bulge (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Pre  => No_Bulge_Certified (Coefficients) and then 0.0 <= V and then V <= 1.0,
     Post => Endpoints_Are_In_Frame_Plane (Coefficients) and then Is_In_Cone (Coefficients, V);
   --  A certified chart keeps the frame-plane part of its tangent in the cone of its endpoint tangents, everywhere
   --  on the curve. Lemma_Cone_Weights then gives the nonnegative weights.

   procedure Lemma_Nonnegative_Projection
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Direction : Position_Scale; V : Real)
   with
     Ghost,
     Pre  => Nonnegative_Projection_Certified (Frame, Coefficients, Direction) and then 0.0 <= V and then V <= 1.0,
     Post => Projected_Numerator (Frame, Coefficients, Direction, V) >= 0.0;
   --  A certified projection never goes negative: the tangent makes no backward progress along Direction.

   procedure Lemma_Ideal_Geometry
     (Frame           : Frame_Vector_Array;
      Coefficients    : Bernstein_Chart;
      Bulge_Allowed   : Boolean;
      Chord_Direction : Position_Scale;
      V               : Real)
   with
     Ghost,
     Pre  =>
       Ideal_Geometry_Is_Certified (Frame, Coefficients, Bulge_Allowed, Chord_Direction)
       and then 0.0 <= V
       and then V <= 1.0,
     Post =>
       (if No_Bulge_Certified (Coefficients)
        then Endpoints_Are_In_Frame_Plane (Coefficients) and then Is_In_Cone (Coefficients, V)
        else Bulge_Allowed and then Projected_Numerator (Frame, Coefficients, Chord_Direction, V) >= 0.0);
   --  What a certified ideal geometry guarantees at every point of the curve: the tangent is in the endpoint
   --  cone or, only where bulging is allowed, it at least never travels backwards along the chord.

   function Warped (U, Warp_Factor : Real) return Real
   is (Warp_Factor * U / (1.0 - U + Warp_Factor * U))
   with Ghost, Pre => 0.0 <= U and then U <= 1.0 and then Warp_Factor > 0.0;
   --  The chart parameter V at normalized physical distance U along the curve, for the curve's Möbius distance
   --  warp. The tangent at distance U is the ideal tangent at V = Warped (U, Warp_Factor).

   procedure Lemma_Warped_Is_In_Unit_Interval (U, Warp_Factor : Real)
   with
     Ghost,
     Pre  => 0.0 <= U and then U <= 1.0 and then Warp_Factor > 0.0,
     Post => 0.0 <= Warped (U, Warp_Factor) and then Warped (U, Warp_Factor) <= 1.0;
   --  Every point of the curve has its chart parameter in [0, 1], so what is proved for all V there holds along
   --  the whole curve whatever the warp.

   procedure Lemma_Planar_Control_Hull (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Pre  =>
       Is_Structurally_Planar (Coefficients)
       and then Planar_Control_Hull_Certified (Coefficients)
       and then 0.0 <= V
       and then V <= 1.0,
     Post => Is_In_Cone (Coefficients, V);

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
