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
--  No contract or assertion of this unit is executed, whatever the build profile.

--  The elementary functions of the corner blender's construction code, instantiated once, and what the proofs of
--  Prunt.Motion_Planner.Stereographic_Curves take on trust about them.
--
--  The contracts of Ada.Numerics.Generic_Elementary_Functions say nothing about how large a square root, a
--  logarithm, an exponential or an arctangent can be, and GNATprove does not see the library's code. The
--  construction code is proved free of run-time errors by bounding every floating-point operation, which needs the
--  little that is stated below. None of it is proved: these are assumptions about the mathematical library, checked
--  by running the tests of Prunt.Motion_Planner.Stereographic_Curves.Test.

with Ada.Numerics.Generic_Elementary_Functions;

package Prunt.Motion_Planner.Stereographic_Kernel.Elementary
  with SPARK_Mode => On, Always_Terminates
is

   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   Root_Bound_Min : constant := 2.0**(-500);
   Root_Bound_Max : constant := 2.0**500;
   --  A bound in this range has a square which neither overflows nor underflows, so the square is correct to one
   --  rounding and its square root is the bound again.

   procedure Lemma_Sqrt_Is_At_Most (X, Bound : Dimensionless)
   with
     Ghost,
     Global => null,
     Pre    => X >= 0.0 and then Bound >= Root_Bound_Min and then Bound <= Root_Bound_Max and then X <= Bound * Bound,
     Post   => Dimensionless_Math.Sqrt (X) <= Bound;
   --  The square root is correctly rounded, hence monotonic, and the square root of a rounded square is the number
   --  which was squared.

   procedure Lemma_Sqrt_Is_At_Least (X, Bound : Dimensionless)
   with
     Ghost,
     Global => null,
     Pre    => Bound >= Root_Bound_Min and then Bound <= Root_Bound_Max and then X >= Bound * Bound,
     Post   => Dimensionless_Math.Sqrt (X) >= Bound;

   Log_Magnitude_Max : constant := 746.0;
   --  The logarithms of the smallest and of the largest positive number are about -744.4 and 709.8.

   procedure Lemma_Log_Is_Bounded (X : Dimensionless)
   with Ghost, Global => null, Pre => X > 0.0, Post => abs Dimensionless_Math.Log (X) <= Log_Magnitude_Max;

   Arctan_Magnitude_Max : constant := 3.25;
   --  Above π, the largest angle which the two-argument arctangent returns.

   procedure Lemma_Arctan_Is_Bounded (Y, X : Dimensionless)
   with
     Ghost,
     Global => null,
     Pre    => X /= 0.0 or else Y /= 0.0,
     Post   => abs Dimensionless_Math.Arctan (Y, X) <= Arctan_Magnitude_Max;

   Exp_Argument_Max : constant := 64.0;
   Exp_Result_Min   : constant := 2.0**(-93);
   Exp_Result_Max   : constant := 2.0**93;
   --  The exponentials of -64 and of 64 are about 2⁻⁹²·³ and 2⁹²·³.

   function Bounded_Exp (X : Dimensionless) return Dimensionless
   with
     Inline,
     Global => null,
     Pre    => abs X <= Exp_Argument_Max,
     Post   => Bounded_Exp'Result >= Exp_Result_Min and then Bounded_Exp'Result <= Exp_Result_Max;
   --  The exponential of X. GNATprove cannot show that a call of the library's Exp does not overflow, whatever is
   --  known of its argument, so the proved code calls it through this function and the next.

   Warp_Log_Max    : constant := 16.0;
   Warp_Factor_Min : constant := 2.0**(-24);
   Warp_Factor_Max : constant := 2.0**24;
   --  The exponentials of -16 and of 16 are about 2⁻²³·¹ and 2²³·¹. The distance warp's logarithm is searched within
   --  these limits.

   function Warp_Exp (X : Dimensionless) return Dimensionless
   with
     Inline,
     Global => null,
     Pre    => abs X <= Warp_Log_Max,
     Post   => Warp_Exp'Result >= Warp_Factor_Min and then Warp_Exp'Result <= Warp_Factor_Max;
   --  The exponential of the logarithm of a distance warp.

private
   pragma SPARK_Mode (Off);

   procedure Lemma_Sqrt_Is_At_Most (X, Bound : Dimensionless) is null;
   procedure Lemma_Sqrt_Is_At_Least (X, Bound : Dimensionless) is null;
   procedure Lemma_Log_Is_Bounded (X : Dimensionless) is null;
   procedure Lemma_Arctan_Is_Bounded (Y, X : Dimensionless) is null;

   function Bounded_Exp (X : Dimensionless) return Dimensionless
   is (Dimensionless_Math.Exp (X));

   function Warp_Exp (X : Dimensionless) return Dimensionless
   is (Dimensionless_Math.Exp (X));

end Prunt.Motion_Planner.Stereographic_Kernel.Elementary;
