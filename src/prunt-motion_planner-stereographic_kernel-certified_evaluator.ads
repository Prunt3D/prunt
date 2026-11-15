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

--  Proof-carrying evidence about the tangent which the realtime evaluator integrates.
--
--  A Certificate is a value which can only be made by Certify, and only if the sampled tangent certificate
--  accepted a geometry certificate's frame and chart together with a warp and an evaluator cache. Whoever holds
--  one whose kind is Sampled therefore holds the conclusion of Lemma_Tangent_Error: everywhere on the curve,
--  the retained tangent is within Tangent_Error of the ideal tangent which the geometry certificate is about.
--  Motion, a child of this package, carries that over to the corrected evaluator and combines it with what the
--  geometry certificate guarantees.

with Prunt.Motion_Planner.Stereographic_Kernel.Certified_Geometry;
with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
use Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
with Prunt.Motion_Planner.Reals;
use Prunt.Motion_Planner.Reals;
with Prunt.Motion_Planner.Stereographic_Kernel.Retained_Tangent;
use Prunt.Motion_Planner.Stereographic_Kernel.Retained_Tangent;
with Prunt.Motion_Planner.Stereographic_Kernel.Sample_Nodes.Table;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Bound;
with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Model;

package Prunt.Motion_Planner.Stereographic_Kernel.Certified_Evaluator
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;
   use type Prunt.Motion_Planner.Stereographic_Kernel.Certified_Geometry.Certificate_Kind;
   use type Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Status;

   type Certificate_Kind is
     (Not_Certified,
      --  Nothing is known about the evaluator.

      Sampled
      --  The sampled tangent certificate bounds the distance between the retained and the ideal tangent.
     );

   type Certificate is private with Default_Initial_Condition => Kind (Certificate) = Not_Certified;

   function Kind (Value : Certificate) return Certificate_Kind;

   function Geometry (Value : Certificate) return Certified_Geometry.Certificate
   with
     Post =>
       (if Kind (Value) = Sampled then Certified_Geometry.Kind (Geometry'Result) /= Certified_Geometry.Not_Certified);
   --  The geometry certificate whose frame and chart define the ideal tangent.

   function Warp (Value : Certificate) return Dimensionless;
   --  The distance warp factor which was certified.

   function Cache (Value : Certificate) return Rational_Antiderivative;
   --  The evaluator cache which was certified.

   function Tangent_Error (Value : Certificate) return Dimensionless
   with
     Post =>
       Tangent_Error'Result >= 0.0 and then (if Kind (Value) = Sampled then Tangent_Error'Result < Dimensionless'Last);
   --  The certified bound on the Euclidean distance between the two tangents.

   type Certification is record
      Outcome : Sampled_Tangent.Status := Sampled_Tangent.Not_Certified;
      --  The outcome of the sampled tangent certificate.

      Error_Bound : Dimensionless := 0.0;
      --  Its bound, when the outcome is Certified.

      Evidence : Certificate;
      --  The certificate, of kind Sampled exactly when the outcome is Certified and the sample nodes passed
      --  their own check when the program started.
   end record;

   function Certify
     (Geometry          : Certified_Geometry.Certificate;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless) return Certification
   with Post => (if Kind (Certify'Result.Evidence) = Sampled then Certify'Result.Outcome = Sampled_Tangent.Certified);
   --  Run the sampled tangent certificate on the frame and chart of Geometry. Denominator_Scale is the ideal
   --  denominator at the start of the chart. The evidence holds copies of what it was given.

   ---------------------------------------------------------------------------
   --  What a certificate guarantees
   ---------------------------------------------------------------------------

   function Ideal_Tangent (Value : Certificate; Axis : Axis_Name; U : Real) return Real
   is (Sampled_Tangent.Model.Ideal_Tangent
         (Certified_Geometry.Frame (Geometry (Value)),
          Certified_Geometry.Coefficients (Geometry (Value)),
          Axis,
          Warped (U, R (Warp (Value)))))
   with Ghost, Pre => 0.0 <= U and then U <= 1.0 and then Warp (Value) > 0.0;
   --  One machine-axis component of the ideal tangent at normalized distance U along the curve.

   function Retained_Value (Value : Certificate; Axis : Axis_Name; U : Real) return Real
   is (Retained_Tangent.Value (Cache (Value), Axis, U))
   with Ghost, Pre => Has_Layout (Cache (Value));
   --  The same component of the retained tangent there.

   function Squared_Distance (Value : Certificate; U : Real) return Real
   is ((Ideal_Tangent (Value, X_Axis, U) - Retained_Value (Value, X_Axis, U))
       * (Ideal_Tangent (Value, X_Axis, U) - Retained_Value (Value, X_Axis, U))
       + (Ideal_Tangent (Value, Y_Axis, U) - Retained_Value (Value, Y_Axis, U))
         * (Ideal_Tangent (Value, Y_Axis, U) - Retained_Value (Value, Y_Axis, U))
       + (Ideal_Tangent (Value, Z_Axis, U) - Retained_Value (Value, Z_Axis, U))
         * (Ideal_Tangent (Value, Z_Axis, U) - Retained_Value (Value, Z_Axis, U))
       + (Ideal_Tangent (Value, E_Axis, U) - Retained_Value (Value, E_Axis, U))
         * (Ideal_Tangent (Value, E_Axis, U) - Retained_Value (Value, E_Axis, U)))
   with Ghost, Pre => 0.0 <= U and then U <= 1.0 and then Warp (Value) > 0.0 and then Has_Layout (Cache (Value));
   --  The squared Euclidean distance between the two tangents at normalized distance U.

   procedure Lemma_Tangent_Error (Value : Certificate; U : Real)
   with
     Ghost,
     Pre  => Kind (Value) = Sampled and then 0.0 <= U and then U <= 1.0,
     Post =>
       Warp (Value) > 0.0
       and then Has_Layout (Cache (Value))
       and then Poles_Are_Off_The_Curve (Cache (Value))
       and then Squared_Distance (Value, U) <= R (Tangent_Error (Value)) * R (Tangent_Error (Value));
   --  Everywhere on the curve the retained tangent is within Tangent_Error of the ideal tangent, and no pole of
   --  the retained tangent lies on the curve.

private

   type Certificate is record
      Kind              : Certificate_Kind := Not_Certified;
      Geometry          : Certified_Geometry.Certificate;
      Warp              : Dimensionless := 1.0;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless := 1.0;
      Value             : Sampled_Tangent.Result;
   end record
   with
     Type_Invariant =>
       Certificate.Value.Error_Bound >= 0.0
       and then
         (if Certificate.Kind = Sampled
          then
            Sample_Nodes.Table.Is_Certified
            and then Certified_Geometry.Kind (Certificate.Geometry) /= Certified_Geometry.Not_Certified
            and then
              Sampled_Tangent.Bound.Is_Witnessed
                (Certified_Geometry.Frame (Certificate.Geometry),
                 Certified_Geometry.Coefficients (Certificate.Geometry),
                 Certificate.Warp,
                 Certificate.Cache,
                 Certificate.Denominator_Scale,
                 Certificate.Value));

   function Kind (Value : Certificate) return Certificate_Kind
   is (Value.Kind);

   function Geometry (Value : Certificate) return Certified_Geometry.Certificate
   is (Value.Geometry);

   function Warp (Value : Certificate) return Dimensionless
   is (Value.Warp);

   function Cache (Value : Certificate) return Rational_Antiderivative
   is (Value.Cache);

   function Tangent_Error (Value : Certificate) return Dimensionless
   is (Value.Value.Error_Bound);

end Prunt.Motion_Planner.Stereographic_Kernel.Certified_Evaluator;
