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

--  The tangent which the realtime evaluator integrates, as a function over the real numbers.
--
--  The evaluator stores a partial-fraction expansion in normalized physical distance U,
--
--     R(U) = c + Σ r/(U - p) + Σ 2·Re(ρ/(U - π)),
--
--  with one term per real pole p and one per conjugate pair of poles π = A ± i·B with residues ρ = R ± i·I, and
--  evaluates its antiderivative in closed form:
--
--     c·U + Σ r·log(1 - U/p) + Σ (R·log(((U - A)² + B²)/(A² + B²)) - 2·I·Δarg(U - π)).
--
--  Value below is R itself, for the real numbers which the stored floating-point poles and residues denote. That
--  the closed form is the antiderivative of R is calculus, which is outside what is stated here.

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Retained_Tangent
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   type Dimensionless_Axis_Vector is array (Axis_Name) of Dimensionless;
   --  A unit-free vector indexed by machine axes, used for rational residues and displacements normalized by length.

   Maximum_Rational_Degree : constant := 16;
   --  Maximum denominator degree of the rational tangent representation.

   subtype Rational_Degree_Slot is Positive range 1 .. Maximum_Rational_Degree;
   --  Index of storage associated with one real pole or one component of a complex-conjugate pair.

   subtype Rational_Pair_Index is Positive range 1 .. Maximum_Rational_Degree / 2;
   --  Index of a complex-conjugate pole pair in the compact antiderivative cache.

   type Rational_Pole_Slot is record
      Pole_Component : Dimensionless := 0.0;
      --  A real pole, or one Cartesian component of a complex pole.

      Residue_Component : Dimensionless_Axis_Vector := [others => 0.0];
      --  Matching real residue, or one Cartesian component of a complex residue.
   end record;
   --  One real-valued storage slot. Complex poles and residues use two consecutive slots so runtime evaluation never
   --  requires a complex numeric type.

   type Rational_Pole_Slots is array (Rational_Degree_Slot) of Rational_Pole_Slot;
   --  Fixed-capacity pole and residue storage, avoiding allocation in planner and realtime data.

   type Rational_Antiderivative is record
      Real_Pole_Count : Natural range 0 .. Maximum_Rational_Degree := 0;
      --  Real poles occupy the leading slots, one slot each.

      Pair_Count : Natural range 0 .. Maximum_Rational_Degree / 2 := 0;
      --  Each conjugate pair occupies two slots after the real-pole prefix.

      Constant_Tangent : Dimensionless_Axis_Vector := [others => 0.0];
      --  Constant polynomial quotient. Its primitive is this value times normalised distance.

      Pole_Slots : Rational_Pole_Slots := [others => <>];
      --  A pair stores its real pole/residue components first and its positive-imaginary components in the following
      --  slot.
   end record;
   --  An analytically integrable partial-fraction representation of the compiled tangent. Real poles contribute
   --  logarithms; conjugate pairs contribute real logarithm and arctangent terms.

   function Has_Layout (Cache : Rational_Antiderivative) return Boolean
   is (Cache.Real_Pole_Count + 2 * Cache.Pair_Count <= Maximum_Rational_Degree);
   --  Whether the poles fit the slots.

   function Pair_Real_Slot (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Rational_Degree_Slot
   is (Cache.Real_Pole_Count + 2 * Pair - 1)
   with Pre => Has_Layout (Cache) and then Pair <= Cache.Pair_Count;
   --  The slot which holds the real parts of a pair's pole and residue.

   function Pair_Imaginary_Slot
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index) return Rational_Degree_Slot
   is (Cache.Real_Pole_Count + 2 * Pair)
   with Pre => Has_Layout (Cache) and then Pair <= Cache.Pair_Count;
   --  The slot which holds their imaginary parts.

   function Poles_Are_Off_The_Curve (Cache : Rational_Antiderivative) return Boolean
   is (Has_Layout (Cache)
       and then
         (for all Slot in 1 .. Cache.Real_Pole_Count =>
            Cache.Pole_Slots (Slot).Pole_Component < 0.0 or else Cache.Pole_Slots (Slot).Pole_Component > 1.0)
       and then
         (for all Pair in 1 .. Cache.Pair_Count =>
            Cache.Pole_Slots (Cache.Real_Pole_Count + 2 * Pair).Pole_Component > 0.0));
   --  Whether no pole lies on the curve, which is the interval [0, 1] of the real axis: every real pole is
   --  outside it, and every pair is off the real axis.

   ---------------------------------------------------------------------------
   --  The retained tangent over the real numbers
   ---------------------------------------------------------------------------

   function Real_Term
     (Cache : Rational_Antiderivative; Slot : Rational_Degree_Slot; Axis : Axis_Name; U : Real) return Real
   is (if U = R (Cache.Pole_Slots (Slot).Pole_Component)
       then 0.0
       else R (Cache.Pole_Slots (Slot).Residue_Component (Axis)) / (U - R (Cache.Pole_Slots (Slot).Pole_Component)))
   with Ghost;
   --  r/(U - p). The value at the pole itself is immaterial: no pole is on a certified curve.

   function Pair_Distance_Square (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; U : Real) return Real
   is ((U - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component))
       * (U - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component))
       + R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component)
         * R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component))
   with Ghost, Pre => Has_Layout (Cache) and then Pair <= Cache.Pair_Count;
   --  |U - π|² = (U - A)² + B².

   function Pair_Term
     (Cache : Rational_Antiderivative; Pair : Rational_Pair_Index; Axis : Axis_Name; U : Real) return Real
   is (if Pair_Distance_Square (Cache, Pair, U) = 0.0
       then 0.0
       else
         2.0
         * (R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Residue_Component (Axis))
            * (U - R (Cache.Pole_Slots (Pair_Real_Slot (Cache, Pair)).Pole_Component))
            - R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Residue_Component (Axis))
              * R (Cache.Pole_Slots (Pair_Imaginary_Slot (Cache, Pair)).Pole_Component))
         / Pair_Distance_Square (Cache, Pair, U))
   with Ghost, Pre => Has_Layout (Cache) and then Pair <= Cache.Pair_Count;
   --  ρ/(U - π) plus its conjugate: 2·(R·(U - A) - I·B)/((U - A)² + B²).

   function Real_Sum (Cache : Rational_Antiderivative; Count : Natural; Axis : Axis_Name; U : Real) return Real
   is (if Count = 0 then 0.0 else Real_Sum (Cache, Count - 1, Axis, U) + Real_Term (Cache, Count, Axis, U))
   with Ghost, Pre => Count <= Cache.Real_Pole_Count, Subprogram_Variant => (Decreases => Count);
   --  The terms of the first Count real poles.

   function Pair_Sum (Cache : Rational_Antiderivative; Count : Natural; Axis : Axis_Name; U : Real) return Real
   is (if Count = 0 then 0.0 else Pair_Sum (Cache, Count - 1, Axis, U) + Pair_Term (Cache, Count, Axis, U))
   with
     Ghost,
     Pre                => Has_Layout (Cache) and then Count <= Cache.Pair_Count,
     Subprogram_Variant => (Decreases => Count);
   --  The terms of the first Count pairs.

   function Value (Cache : Rational_Antiderivative; Axis : Axis_Name; U : Real) return Real
   is (R (Cache.Constant_Tangent (Axis))
       + Real_Sum (Cache, Cache.Real_Pole_Count, Axis, U)
       + Pair_Sum (Cache, Cache.Pair_Count, Axis, U))
   with Ghost, Pre => Has_Layout (Cache);
   --  One machine-axis component of the retained tangent at normalized distance U.

end Prunt.Motion_Planner.Stereographic_Kernel.Retained_Tangent;
