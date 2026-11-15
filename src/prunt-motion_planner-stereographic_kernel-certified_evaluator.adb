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

with Prunt.Motion_Planner.Stereographic_Kernel.Sampled_Tangent.Structure;

package body Prunt.Motion_Planner.Stereographic_Kernel.Certified_Evaluator
  with SPARK_Mode => On
is

   function Certify
     (Geometry          : Certified_Geometry.Certificate;
      Warp              : Dimensionless;
      Cache             : Rational_Antiderivative;
      Denominator_Scale : Dimensionless) return Certification
   is
      Value    : constant Sampled_Tangent.Result :=
        Sampled_Tangent.Bound.Error
          (Certified_Geometry.Frame (Geometry),
           Certified_Geometry.Coefficients (Geometry),
           Warp,
           Cache,
           Denominator_Scale);
      Evidence : Certificate;
   begin
      if Value.Outcome = Sampled_Tangent.Certified
        and then Sample_Nodes.Table.Is_Certified
        and then Certified_Geometry.Kind (Geometry) /= Certified_Geometry.Not_Certified
      then
         Evidence :=
           (Kind              => Sampled,
            Geometry          => Geometry,
            Warp              => Warp,
            Cache             => Cache,
            Denominator_Scale => Denominator_Scale,
            Value             => Value);
      end if;
      return (Outcome => Value.Outcome, Error_Bound => Value.Error_Bound, Evidence => Evidence);
   end Certify;

   procedure Lemma_Tangent_Error (Value : Certificate; U : Real) is
   begin
      --  The invariant holds the witness of the sampled tangent certificate, which makes the warp positive.
      pragma Assert (Sample_Nodes.Table.Is_Certified);
      pragma
        Assert
          (Sampled_Tangent.Bound.Is_Witnessed
             (Certified_Geometry.Frame (Value.Geometry),
              Certified_Geometry.Coefficients (Value.Geometry),
              Value.Warp,
              Value.Cache,
              Value.Denominator_Scale,
              Value.Value));
      pragma Assert (Value.Warp > 0.0);
      pragma Assert (R (Value.Warp) > 0.0);
      Lemma_Warped_Is_In_Unit_Interval (U, R (Value.Warp));

      --  The certificate's theorem at the chart coordinate of the point at distance U,
      Sampled_Tangent.Bound.Lemma_Error
        (Certified_Geometry.Frame (Value.Geometry),
         Certified_Geometry.Coefficients (Value.Geometry),
         Value.Warp,
         Value.Cache,
         Value.Denominator_Scale,
         Value.Value,
         Warped (U, R (Value.Warp)));

      --  where the retained tangent is evaluated at the distance of that coordinate, which is U again.
      Sampled_Tangent.Structure.Lemma_Distance_Of_Warped (Value.Warp, U);
      pragma Assert (Sampled_Tangent.Model.Distance (Value.Warp, Warped (U, R (Value.Warp))) = U);
      pragma
        Assert
          (Sampled_Tangent.Model.Tangent_Error
             (Certified_Geometry.Frame (Value.Geometry),
              Certified_Geometry.Coefficients (Value.Geometry),
              Value.Warp,
              Value.Cache,
              X_Axis,
              Warped (U, R (Value.Warp)))
           = Ideal_Tangent (Value, X_Axis, U) - Retained_Value (Value, X_Axis, U));
      pragma
        Assert
          (Sampled_Tangent.Model.Tangent_Error
             (Certified_Geometry.Frame (Value.Geometry),
              Certified_Geometry.Coefficients (Value.Geometry),
              Value.Warp,
              Value.Cache,
              Y_Axis,
              Warped (U, R (Value.Warp)))
           = Ideal_Tangent (Value, Y_Axis, U) - Retained_Value (Value, Y_Axis, U));
      pragma
        Assert
          (Sampled_Tangent.Model.Tangent_Error
             (Certified_Geometry.Frame (Value.Geometry),
              Certified_Geometry.Coefficients (Value.Geometry),
              Value.Warp,
              Value.Cache,
              Z_Axis,
              Warped (U, R (Value.Warp)))
           = Ideal_Tangent (Value, Z_Axis, U) - Retained_Value (Value, Z_Axis, U));
      pragma
        Assert
          (Sampled_Tangent.Model.Tangent_Error
             (Certified_Geometry.Frame (Value.Geometry),
              Certified_Geometry.Coefficients (Value.Geometry),
              Value.Warp,
              Value.Cache,
              E_Axis,
              Warped (U, R (Value.Warp)))
           = Ideal_Tangent (Value, E_Axis, U) - Retained_Value (Value, E_Axis, U));
      pragma
        Assert
          (Squared_Distance (Value, U)
           = Sampled_Tangent.Model.Squared_Tangent_Error
               (Certified_Geometry.Frame (Value.Geometry),
                Certified_Geometry.Coefficients (Value.Geometry),
                Value.Warp,
                Value.Cache,
                Warped (U, R (Value.Warp))));
   end Lemma_Tangent_Error;

end Prunt.Motion_Planner.Stereographic_Kernel.Certified_Evaluator;
