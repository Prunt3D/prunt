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

--  The speed of the ideal curve. Its tangent is a unit vector in frame coordinates, exactly, because inverse
--  stereographic projection lands on the unit sphere. On the machine axes it is as close to unit as the frame is
--  to orthonormal, which Frame_Defect measures.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

package Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed
  with SPARK_Mode => On
is

   Largest_Component : constant Dimensionless := 2.0;
   --  No component of a usable frame vector is larger: the vectors are unit to within rounding.

   Defect_Allowance : constant := 2.0**(-45);
   --  More than the rounding error of one dot product of two frame vectors within Largest_Component.

   function Is_Defect_Bound (Frame : Frame_Vector_Array; Bound : Real) return Boolean
   is (for all I in Frame_Component_Index =>
         (for all J in Frame_Component_Index =>
            abs (Dot_Real (Frame (I), Frame (J)) - (if I = J then Real'(1.0) else Real'(0.0))) <= Bound))
   with Ghost;
   --  Whether every exact dot product of two frame vectors is within Bound of the identity matrix's entry.

   function Frame_Defect (Frame : Frame_Vector_Array) return Dimensionless
   with
     Post =>
       Frame_Defect'Result >= 0.0
       and then
         (if Frame_Defect'Result < Dimensionless'Last
          then Is_Defect_Bound (Frame, R (Frame_Defect'Result) + Defect_Allowance));
   --  The largest |Fᵢ·Fⱼ - δᵢⱼ| over the frame, in ordinary rounded arithmetic, or Dimensionless'Last if a
   --  component is larger than Largest_Component or is not a number. Adding Defect_Allowance covers the rounding.

   function Tangent_Numerator
     (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Axis : Axis_Name; V : Real) return Real
   is (Numerator (Coefficients, 0, V) * R (Frame (0) (Axis)) + Numerator (Coefficients, 1, V) * R (Frame (1) (Axis))
       + Numerator (Coefficients, 2, V) * R (Frame (2) (Axis))
       + Numerator (Coefficients, 3, V) * R (Frame (3) (Axis)))
   with Ghost;
   --  One machine-axis component of the tangent at V, times Denominator.

   function Speed_Numerator_Square (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; V : Real) return Real
   is (Tangent_Numerator (Frame, Coefficients, X_Axis, V) * Tangent_Numerator (Frame, Coefficients, X_Axis, V)
       + Tangent_Numerator (Frame, Coefficients, Y_Axis, V) * Tangent_Numerator (Frame, Coefficients, Y_Axis, V)
       + Tangent_Numerator (Frame, Coefficients, Z_Axis, V) * Tangent_Numerator (Frame, Coefficients, Z_Axis, V)
       + Tangent_Numerator (Frame, Coefficients, E_Axis, V) * Tangent_Numerator (Frame, Coefficients, E_Axis, V))
   with Ghost;
   --  The squared speed at V, times the square of Denominator.

   procedure Lemma_Frame_Coordinates_Are_Unit (Coefficients : Bernstein_Chart; V : Real)
   with
     Ghost,
     Post =>
       Numerator (Coefficients, 0, V) * Numerator (Coefficients, 0, V)
       + Numerator (Coefficients, 1, V) * Numerator (Coefficients, 1, V)
       + Numerator (Coefficients, 2, V) * Numerator (Coefficients, 2, V)
       + Numerator (Coefficients, 3, V) * Numerator (Coefficients, 3, V)
       = Denominator (Coefficients, V) * Denominator (Coefficients, V);
   --  In frame coordinates the tangent has unit length at every V, whatever the chart.

   procedure Lemma_Unit_Speed (Frame : Frame_Vector_Array; Coefficients : Bernstein_Chart; Bound, V : Real)
   with
     Ghost,
     Pre  => Is_Defect_Bound (Frame, Bound),
     Post =>
       abs (Speed_Numerator_Square (Frame, Coefficients, V)
            - Denominator (Coefficients, V) * Denominator (Coefficients, V))
       <= 4.0 * Bound * (Denominator (Coefficients, V) * Denominator (Coefficients, V));
   --  On the machine axes the squared speed is within four times the frame's defect of one, at every V: divide
   --  through by the square of Denominator, which is at least one.

end Prunt.Motion_Planner.Stereographic_Kernel.Ideal_Geometry.Unit_Speed;
