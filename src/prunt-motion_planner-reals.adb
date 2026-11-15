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
with SPARK.Lemmas.Long_Float_Arithmetic;

package body Prunt.Motion_Planner.Reals
  with SPARK_Mode => On
is

   package body Float_Lemmas is

      procedure Lemma_Rounding_Error_Add (Val1, Val2 : Dimensioned_Float) is
      begin
         SPARK.Lemmas.Long_Float_Arithmetic.Lemma_Rounding_Error_Add (Long_Float (Val1), Long_Float (Val2));
         pragma Assert (Long_Float (Val1 + Val2) = Long_Float (Val1) + Long_Float (Val2));
      end Lemma_Rounding_Error_Add;

      procedure Lemma_Rounding_Error_Sub (Val1, Val2 : Dimensioned_Float) is
      begin
         SPARK.Lemmas.Long_Float_Arithmetic.Lemma_Rounding_Error_Sub (Long_Float (Val1), Long_Float (Val2));
         pragma Assert (Long_Float (Val1 - Val2) = Long_Float (Val1) - Long_Float (Val2));
      end Lemma_Rounding_Error_Sub;

      procedure Lemma_Rounding_Error_Mul (Val1, Val2 : Dimensioned_Float) is
      begin
         SPARK.Lemmas.Long_Float_Arithmetic.Lemma_Rounding_Error_Mul (Long_Float (Val1), Long_Float (Val2));
         pragma Assert (Long_Float (Val1 * Val2) = Long_Float (Val1) * Long_Float (Val2));
      end Lemma_Rounding_Error_Mul;

      procedure Lemma_Rounding_Error_Div (Val1, Val2 : Dimensioned_Float) is
      begin
         SPARK.Lemmas.Long_Float_Arithmetic.Lemma_Rounding_Error_Div (Long_Float (Val1), Long_Float (Val2));
         pragma Assert (Long_Float (Val1 / Val2) = Long_Float (Val1) / Long_Float (Val2));
      end Lemma_Rounding_Error_Div;

   end Float_Lemmas;

end Prunt.Motion_Planner.Reals;
