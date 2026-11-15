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

--  The proof that an ideal tangent has the certified geometry, carried as data.
--
--  A Certificate can only be made by Certify, which runs the proved certificates of
--  Stereographic_Kernel.Ideal_Geometry on the frame and chart it is given and records which of them passed. Whatever
--  code handles a certificate afterwards, proved or not, cannot alter it, so a certificate whose Kind is not
--  Not_Certified is evidence that the theorems below hold for the frame and chart inside it. The theorems are stated
--  at one real parameter V at a time, which makes them hold everywhere on the curve.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
use Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry;
with Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed;
with Prunt.Motion_Planner.Reals;
use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Certified_Geometry
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   package Unit_Speed renames Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed;

   Defect_Allowance : constant := Unit_Speed.Defect_Allowance;
   --  Added to a frame's measured defect to cover the rounding of the measurement.

   type Certificate_Kind is
     (Not_Certified,
      --  Nothing is known about the geometry.

      In_Cone,
      --  The frame-plane part of the ideal tangent stays in the cone of the endpoint tangents.

      Chord_Progress
      --  The ideal tangent never travels backwards along the chord direction. Only where bulging was allowed.
     );

   type Certificate is private with Default_Initial_Condition => Kind (Certificate) = Not_Certified;

   function Kind (Value : Certificate) return Certificate_Kind;

   function Frame (Value : Certificate) return Frame_Vector_Array;
   --  The frame which was certified.

   function Coefficients (Value : Certificate) return Bernstein_Chart;
   --  The chart which was certified.

   function Chord_Direction (Value : Certificate) return Position_Scale;
   --  The direction along which Chord_Progress certifies progress.

   function Frame_Defect (Value : Certificate) return Dimensionless;
   --  How far the frame is from orthonormal: Unit_Speed.Frame_Defect of the certified frame.

   function Certify
     (Frame           : Frame_Vector_Array;
      Coefficients    : Bernstein_Chart;
      Bulge_Allowed   : Boolean;
      Chord_Direction : Position_Scale) return Certificate
   with
     Post =>
       (Kind (Certify'Result) /= Not_Certified)
       = Ideal_Geometry_Is_Certified (Frame, Coefficients, Bulge_Allowed, Chord_Direction)
       and then (if not Bulge_Allowed then Kind (Certify'Result) /= Chord_Progress);
   --  Run the certificates. The result holds copies of the frame, chart and direction it was given.

   ---------------------------------------------------------------------------
   --  What a certificate guarantees
   ---------------------------------------------------------------------------

   procedure Lemma_In_Cone (Value : Certificate; V : Real)
   with
     Ghost,
     Pre  => Kind (Value) = In_Cone and then 0.0 <= V and then V <= 1.0,
     Post =>
       Endpoints_Are_In_Frame_Plane (Coefficients (Value))
       and then Is_In_Cone (Coefficients (Value), V)
       and then Start_Weight (Coefficients (Value), V) >= 0.0
       and then Finish_Weight (Coefficients (Value), V) >= 0.0
       and then
         Numerator (Coefficients (Value), 0, V)
         = Start_Weight (Coefficients (Value), V) * Numerator (Coefficients (Value), 0, 0.0)
           + Finish_Weight (Coefficients (Value), V) * Numerator (Coefficients (Value), 0, 1.0)
       and then
         Numerator (Coefficients (Value), 1, V)
         = Start_Weight (Coefficients (Value), V) * Numerator (Coefficients (Value), 1, 0.0)
           + Finish_Weight (Coefficients (Value), V) * Numerator (Coefficients (Value), 1, 1.0)
       and then Numerator (Coefficients (Value), 2, 0.0) = 0.0
       and then Numerator (Coefficients (Value), 3, 0.0) = 0.0
       and then Numerator (Coefficients (Value), 2, 1.0) = 0.0
       and then Numerator (Coefficients (Value), 3, 1.0) = 0.0;
   --  Everywhere on the curve, the part of the tangent in the plane of the first two frame vectors is a
   --  combination of the start and finish tangents with nonnegative weights, and those two tangents lie in that
   --  plane. The components are stated times the positive Denominator.

   procedure Lemma_Chord_Progress (Value : Certificate; V : Real)
   with
     Ghost,
     Pre  => Kind (Value) = Chord_Progress and then 0.0 <= V and then V <= 1.0,
     Post => Projected_Numerator (Frame (Value), Coefficients (Value), Chord_Direction (Value), V) >= 0.0;
   --  Everywhere on the curve, the tangent has a nonnegative component along the chord direction.

   procedure Lemma_Unit_Speed (Value : Certificate; V : Real)
   with
     Ghost,
     Pre  => Kind (Value) /= Not_Certified and then Frame_Defect (Value) < Dimensionless'Last,
     Post =>
       abs (Unit_Speed.Speed_Numerator_Square (Frame (Value), Coefficients (Value), V)
            - Denominator (Coefficients (Value), V) * Denominator (Coefficients (Value), V))
       <= 4.0
          * (R (Frame_Defect (Value)) + Defect_Allowance)
          * (Denominator (Coefficients (Value), V) * Denominator (Coefficients (Value), V));
   --  Everywhere, including outside [0, 1], the squared speed of the ideal curve is within four times the frame's
   --  defect of one.

private

   type Certificate is record
      Kind            : Certificate_Kind := Not_Certified;
      Frame           : Frame_Vector_Array := [others => [others => 0.0]];
      Coefficients    : Bernstein_Chart := [others => [others => 0.0]];
      Chord_Direction : Position_Scale := [others => 0.0];
      Frame_Defect    : Dimensionless := Dimensionless'Last;
   end record
   with
     Type_Invariant =>
       (case Certificate.Kind is
          when Not_Certified  => True,
          when In_Cone        => No_Bulge_Certified (Certificate.Coefficients),
          when Chord_Progress =>
            Nonnegative_Projection_Certified
              (Certificate.Frame, Certificate.Coefficients, Certificate.Chord_Direction))
       and then
         (if Certificate.Frame_Defect < Dimensionless'Last
          then Unit_Speed.Is_Defect_Bound (Certificate.Frame, R (Certificate.Frame_Defect) + Defect_Allowance));

   function Kind (Value : Certificate) return Certificate_Kind
   is (Value.Kind);

   function Frame (Value : Certificate) return Frame_Vector_Array
   is (Value.Frame);

   function Coefficients (Value : Certificate) return Bernstein_Chart
   is (Value.Coefficients);

   function Chord_Direction (Value : Certificate) return Position_Scale
   is (Value.Chord_Direction);

   function Frame_Defect (Value : Certificate) return Dimensionless
   is (Value.Frame_Defect);

end Prunt.Motion_Planner.Stereographic_Kernel.Certified_Geometry;
