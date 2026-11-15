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

--  Real-number views of the floating-point values of Trajectory and Stereographic_Kernel, for specification and
--  proof only.
--
--  The rounding-error bounds are those of SPARKlib for Long_Float, which Dimensioned_Float is derived from. They are
--  restated here for Dimensioned_Float, because instantiating SPARKlib's generic lemmas with a type that carries a
--  dimension system upsets GNATprove's dimension checks in every unit which sees the instance.

with SPARK.Big_Reals;
with SPARK.Conversions.Long_Float_Conversions;

package Prunt.Motion_Planner.Reals
  with SPARK_Mode => On, Ghost
is
   subtype Real is SPARK.Big_Reals.Big_Real;
   use type Real;

   function R (Value : Dimensioned_Float) return Real
   is (SPARK.Conversions.Long_Float_Conversions.To_Big_Real (Long_Float (Value)));
   --  The real number which a floating-point value denotes exactly.

   Epsilon : constant := 2.0**(-53);
   --  Largest relative error of one operation rounded to nearest.

   Eta : constant := 2.0**(-1075);
   --  Largest absolute error of one operation whose result is subnormal.

   Slack_Factor : constant := 5.0 * 2.0**(-54);
   --  Two and a half times Epsilon: enough for two successive rounded operations, and less than the three or so
   --  by which outward padding moves a value.

   Twice_Eta : constant := 2.0**(-1074);

   Square_Root_Of_Last : constant := 2.0**511;

   Cube_Root_Slack : constant := 2.0**(-40);
   --  How far, relatively, the library's cube root is taken to be from the exact one. See
   --  Prunt.Motion_Planner.Trajectory.Elementary.

   package Float_Lemmas is

      procedure Lemma_Rounding_Error_Add (Val1, Val2 : Dimensioned_Float)
      with
        Pre  =>
          Val1 in Dimensioned_Float'First / 2.0 .. Dimensioned_Float'Last / 2.0
          and then Val2 in Dimensioned_Float'First / 2.0 .. Dimensioned_Float'Last / 2.0,
        Post => abs (R (Val1 + Val2) - (R (Val1) + R (Val2))) <= Epsilon * abs (R (Val1) + R (Val2)) + Eta;

      procedure Lemma_Rounding_Error_Sub (Val1, Val2 : Dimensioned_Float)
      with
        Pre  =>
          Val1 in Dimensioned_Float'First / 2.0 .. Dimensioned_Float'Last / 2.0
          and then Val2 in Dimensioned_Float'First / 2.0 .. Dimensioned_Float'Last / 2.0,
        Post => abs (R (Val1 - Val2) - (R (Val1) - R (Val2))) <= Epsilon * abs (R (Val1) - R (Val2)) + Eta;

      procedure Lemma_Rounding_Error_Mul (Val1, Val2 : Dimensioned_Float)
      with
        Pre  =>
          Val1 in -Square_Root_Of_Last .. Square_Root_Of_Last
          and then Val2 in -Square_Root_Of_Last .. Square_Root_Of_Last,
        Post => abs (R (Val1 * Val2) - (R (Val1) * R (Val2))) <= Epsilon * abs (R (Val1) * R (Val2)) + Eta;

      procedure Lemma_Rounding_Error_Div (Val1, Val2 : Dimensioned_Float)
      with
        Pre  =>
          Val1 in -Square_Root_Of_Last .. Square_Root_Of_Last
          and then
            Val2
            in Dimensioned_Float'First .. -1.0 / Square_Root_Of_Last
             | 1.0 / Square_Root_Of_Last .. Dimensioned_Float'Last,
        Post => abs (R (Val1 / Val2) - (R (Val1) / R (Val2))) <= Epsilon * abs (R (Val1) / R (Val2)) + Eta;

   end Float_Lemmas;

end Prunt.Motion_Planner.Reals;
