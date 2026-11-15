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
pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

package body Prunt.Motion_Planner.Stereographic_Kernel.Certified_Geometry
  with SPARK_Mode => On
is

   function Certify
     (Frame           : Frame_Vector_Array;
      Coefficients    : Bernstein_Chart;
      Bulge_Allowed   : Boolean;
      Chord_Direction : Position_Scale) return Certificate
   is
      Result : Certificate;
   begin
      --  Allowing bulge is a relaxation of the default geometry, not a switch to a disjoint acceptance rule. A
      --  certified no-bulge candidate is retained first; only a candidate which leaves the endpoint tangent cone
      --  needs the chord-monotonicity proof.
      if No_Bulge_Certified (Coefficients) then
         Result.Kind := In_Cone;
      elsif Bulge_Allowed and then Nonnegative_Projection_Certified (Frame, Coefficients, Chord_Direction) then
         Result.Kind := Chord_Progress;
      else
         return Result;
      end if;
      Result.Frame := Frame;
      Result.Coefficients := Coefficients;
      Result.Chord_Direction := Chord_Direction;
      Result.Frame_Defect := Unit_Speed.Frame_Defect (Frame);
      return Result;
   end Certify;

   procedure Lemma_In_Cone (Value : Certificate; V : Real) is
   begin
      Lemma_No_Bulge (Value.Coefficients, V);
      Lemma_Cone_Weights (Value.Coefficients, V);
   end Lemma_In_Cone;

   procedure Lemma_Chord_Progress (Value : Certificate; V : Real) is
   begin
      Lemma_Nonnegative_Projection (Value.Frame, Value.Coefficients, Value.Chord_Direction, V);
   end Lemma_Chord_Progress;

   procedure Lemma_Unit_Speed (Value : Certificate; V : Real) is
   begin
      Unit_Speed.Lemma_Unit_Speed (Value.Frame, Value.Coefficients, R (Value.Frame_Defect) + Defect_Allowance, V);
   end Lemma_Unit_Speed;

end Prunt.Motion_Planner.Stereographic_Kernel.Certified_Geometry;
