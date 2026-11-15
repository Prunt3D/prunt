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

--  The first stage of the sampled tangent certificate: check magnitudes and gather the poles.

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Poles
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   Largest_Slope : constant Dimensionless := 1.0E125;
   --  Sixteen poles, each with a residue within twice Magnitude_Limit and a squared distance of at least
   --  Smallest_Distance_Square, cannot give a larger Slope.

   function Is_Chart_Prepared (Coefficients : Bernstein_Chart; Prepared : Preparation) return Boolean
   is (for all Component in Chart_Component_Index =>
         Prepared.Delta_Y (Component) >= 0.0
         and then Prepared.Delta_Y (Component) <= Chart_Allowance * Magnitude_Limit
         and then
           Prepared.Vanishes (Component) = (for all K in Chart_Coefficient_Index => Coefficients (K, Component) = 0.0)
         and then
           (for all K in Chart_Coefficient_Index =>
              abs Coefficients (K, Component) <= Magnitude_Limit
              and then
                R (Prepared.Delta_Y (Component))
                >= Chart_Allowance_Real * abs R (Coefficients (K, Component)) * (1.0 - Epsilon) - Eta))
   with Ghost;
   --  Every chart coefficient is within Magnitude_Limit, and Delta_Y is Chart_Allowance times the largest
   --  magnitude among those of its component, as that product rounds.

   function Is_Frame_Prepared
     (Frame        : Frame_Vector_Array;
      Coefficients : Bernstein_Chart;
      Cache        : Rational_Antiderivative;
      Prepared     : Preparation) return Boolean
   is ((for all Component in Frame_Component_Index =>
          (for all Axis in Axis_Name => abs Frame (Component) (Axis) <= Magnitude_Limit))
       and then (for all Axis in Axis_Name => abs Cache.Constant_Tangent (Axis) <= Magnitude_Limit)
       and then
         (for all Axis in Axis_Name =>
            (if not Prepared.Active (Axis)
             then
               Cache.Constant_Tangent (Axis) = 0.0
               and then Frame (0) (Axis) = 0.0
               and then
                 (for all Component in Chart_Component_Index =>
                    Frame (Component) (Axis) = 0.0
                    or else (for all K in Chart_Coefficient_Index => Coefficients (K, Component) = 0.0))
               and then
                 (for all Slot in Rational_Degree_Slot =>
                    (if Slot <= Cache.Real_Pole_Count + 2 * Cache.Pair_Count
                     then Cache.Pole_Slots (Slot).Residue_Component (Axis) = 0.0)))))
   with Ghost, Pre => Has_Layout (Cache);
   --  Every frame component and cached constant is within Magnitude_Limit, and on an axis which is not active
   --  both tangents vanish identically: the cache has no constant and no residue there, and every frame vector
   --  either has no component there or multiplies a chart coordinate which is identically zero.

   Smallest_Distance_Square : constant := 2.0**(-200);
   --  No pole of a prepared cache is closer to the curve than the square root of this.

   function Horizontal_Distance (Pole_Real : Real) return Real
   is (if Pole_Real < 0.0 then 0.0 - Pole_Real elsif Pole_Real > 1.0 then Pole_Real - 1.0 else 0.0)
   with Ghost;
   --  The distance of a real part from [0, 1].

   function Distance_Square (Term : Pole_Term) return Real
   is (Horizontal_Distance (R (Term.Real_Part)) * Horizontal_Distance (R (Term.Real_Part))
       + R (Term.Imaginary_Part) * R (Term.Imaginary_Part))
   with Ghost;
   --  d², the squared distance of a pole from the curve, which is the interval [0, 1] of the real axis.

   function Slope_Term (Term : Pole_Term; Axis : Axis_Name) return Real
   is (if Distance_Square (Term) = 0.0
       then 0.0
       else (abs R (Term.Residue_Real (Axis)) + abs R (Term.Residue_Imaginary (Axis))) / Distance_Square (Term))
   with Ghost;
   --  |r|/d², which bounds the derivative of the pole's term on the curve.

   function Slope_Sum (Prepared : Preparation; Count : Natural; Axis : Axis_Name) return Real
   is (if Count = 0 then 0.0 else Slope_Sum (Prepared, Count - 1, Axis) + Slope_Term (Prepared.Terms (Count), Axis))
   with Ghost, Pre => Count <= Maximum_Rational_Degree, Subprogram_Variant => (Decreases => Count);

   function Is_Term_Bounded (Term : Pole_Term) return Boolean
   is (abs Term.Real_Part <= Magnitude_Limit
       and then Term.Imaginary_Part >= 0.0
       and then Term.Imaginary_Part <= Magnitude_Limit
       and then abs Term.Reciprocal_Norm <= Magnitude_Limit
       and then
         (for all Axis in Axis_Name =>
            abs Term.Residue_Real (Axis) <= Magnitude_Limit
            and then abs Term.Residue_Imaginary (Axis) <= Magnitude_Limit)
       and then Distance_Square (Term) >= Smallest_Distance_Square)
   with Ghost;
   --  What keeps the node loop finite: every magnitude is within Magnitude_Limit and the pole keeps its distance.

   function Is_Real_Term
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Term : Pole_Term) return Boolean
   is (R (Term.Real_Part) = R (Cache.Pole_Slots (Slot).Pole_Component)
       and then Term.Imaginary_Part = 0.0
       and then (Term.Real_Part < 0.0 or else Term.Real_Part > 1.0)
       and then
         abs (R (Term.Reciprocal_Norm) - 1.0 / R (Term.Real_Part)) <= 2.0 * Epsilon * abs (1.0 / R (Term.Real_Part))
       and then
         (for all Axis in Axis_Name =>
            R (Term.Residue_Real (Axis)) = R (Cache.Pole_Slots (Slot).Residue_Component (Axis))
            and then Term.Residue_Imaginary (Axis) = 0.0))
   with Ghost;
   --  Term is the real pole of Slot, with the reciprocal of the pole as one division rounds it.

   function Is_Pair_Term (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Term : Pole_Term) return Boolean
   is (R (Term.Real_Part) = R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component)
       and then R (Term.Imaginary_Part) = R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component)
       and then Term.Imaginary_Part > 0.0
       and then
         abs (R (Term.Reciprocal_Norm)
              - 1.0 / (R (Term.Real_Part) * R (Term.Real_Part) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
         <= 8.0
            * Epsilon
            * (1.0 / (R (Term.Real_Part) * R (Term.Real_Part) + R (Term.Imaginary_Part) * R (Term.Imaginary_Part)))
       and then
         (for all Axis in Axis_Name =>
            abs (R (Term.Residue_Real (Axis))
                 - 2.0 * R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis)))
            <= 2.0 * Epsilon * abs R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis)) + Eta
            and then
              abs (R (Term.Residue_Imaginary (Axis))
                   - 2.0 * R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis)))
              <= 2.0
                 * Epsilon
                 * abs R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis))
                 + Eta))
   with Ghost, Pre => Has_Layout (Cache) and then Pair <= Cache.Pair_Count;
   --  Term is the pair Pair with its residue doubled, and the reciprocal of |π|² as its few operations round it.

   function Is_Cache_Prepared (Cache : Rational_Antiderivative; Prepared : Preparation) return Boolean
   is (Prepared.Count = Cache.Real_Pole_Count + Cache.Pair_Count
       and then (for all Index in 1 .. Prepared.Count => Is_Term_Bounded (Prepared.Terms (Index)))
       and then (for all Slot in 1 .. Cache.Real_Pole_Count => Is_Real_Term (Cache, Slot, Prepared.Terms (Slot)))
       and then
         (for all Pair in 1 .. Cache.Pair_Count =>
            Is_Pair_Term (Cache, Pair, Prepared.Terms (Cache.Real_Pole_Count + Pair)))
       and then
         (for all Axis in Axis_Name =>
            Prepared.Slope (Axis) >= 0.0
            and then Prepared.Slope (Axis) <= Largest_Slope
            and then R (Prepared.Slope (Axis)) >= Slope_Sum (Prepared, Prepared.Count, Axis)))
   with Ghost, Pre => Has_Layout (Cache);
   --  Terms holds the poles and residues of Cache: the real poles in order, then the pairs, with what the node
   --  loop relies on about their magnitudes and their distances from the curve, and Slope is at least Σ |r|/d².

   function Is_Prepared
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless;
      Prepared          : Preparation) return Boolean
   is (Prepared.Outcome = Certified
       and then Denominator_Scale > 0.0
       and then Denominator_Scale <= Magnitude_Limit
       and then Warp >= Smallest_Warp
       and then Warp <= Largest_Warp
       and then Has_Layout (Cache)
       and then Poles_Are_Off_The_Curve (Cache)
       and then Is_Chart_Prepared (Coefficients, Prepared)
       and then Is_Frame_Prepared (Frame, Coefficients, Cache, Prepared)
       and then Is_Cache_Prepared (Cache, Prepared))
   with Ghost;
   --  What the node loop relies on.

   function Prepare
     (Frame             : Frame_Vector_Array;
      Coefficients      : Bernstein_Chart;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless) return Preparation
   with
     Post =>
       (if Prepare'Result.Outcome = Certified
        then Is_Prepared (Frame, Coefficients, Warp, Cache, Denominator_Scale, Prepare'Result));
   --  Check magnitudes and gather the poles. Unless the outcome is Certified it is the outcome of the
   --  certificate.

end Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Poles;
