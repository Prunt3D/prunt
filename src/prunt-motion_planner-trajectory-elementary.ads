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

--  The elementary functions of the profile generator, and what the proofs take on trust about them.
--
--  The contracts of Ada.Numerics.Generic_Elementary_Functions say nothing about how accurate a square root or a
--  power is, and GNATprove does not see the library's code. The lemmas below state the little that the proofs
--  need. They are not proved: they are assumptions about the mathematical library, checked by running the tests of
--  Prunt.Motion_Planner.Trajectory.Test.
--
--  Prunt writes a cube root as X ** (1.0 / 3.0), with the "**" of Prunt.Dimensioned_Float_Math. GNATprove knows
--  nothing of its accuracy, so it is wrapped here in a function whose body GNATprove does not see.

with Ada.Numerics.Generic_Elementary_Functions;

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Trajectory.Elementary
  with SPARK_Mode => On, Always_Terminates
is
   use type Prunt.Motion_Planner.Reals.Real;

   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   function Cube_Root (X : Dimensionless) return Dimensionless
   with Inline, Global => null, Pre => X >= 0.0, Post => Cube_Root'Result >= 0.0;
   --  X ** (1.0 / 3.0).

   function Fifth_Root (X : Dimensionless) return Dimensionless
   with Inline, Global => null, Pre => X >= 0.0, Post => Fifth_Root'Result >= 0.0;
   --  X ** 0.2.

   Exp_Argument_Max : constant := 80.0;
   Exp_Result_Max   : constant := 2.0**116;

   function Bounded_Exp (X : Dimensionless) return Dimensionless
   with
     Inline,
     Global => null,
     Pre    => abs X <= Exp_Argument_Max,
     Post   => Bounded_Exp'Result >= 0.0 and then Bounded_Exp'Result <= Exp_Result_Max;
   --  Exp (X). GNATprove knows nothing of the size of an exponential, so the bound is taken on trust here: Exp (80)
   --  is 5.54e34, below 2**116 = 8.31e34, and the exponential is not negative.

   Fifth_Power_Argument_Max : constant := 2.0**100;

   function Fifth_Power (X : Dimensionless) return Dimensionless
   with
     Inline,
     Global => null,
     Pre    => X >= 0.0 and then X <= Fifth_Power_Argument_Max,
     Post   => Fifth_Power'Result >= 0.0 and then Fifth_Power'Result <= 2.0**501;
   --  X**5. GNAT computes a power above the fourth in a wider format and rounds it once, which GNATprove gives no
   --  meaning to, so the bound is taken on trust here: the fifth power of a number up to 2**100 is at most
   --  2**500, and rounding it once cannot double it.

   ------------------------------------------------------------------------------------------------------------------
   --  Assumptions
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Square_Root_Is_Faithful (X, Below, Above : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Pre    =>
       X >= 0.0
       and then Below >= 0.0
       and then Above >= 0.0
       and then R (Below) * R (Below) <= R (X)
       and then R (X) <= R (Above) * R (Above),
     Post   => Dimensionless_Math.Sqrt (X) >= Below and then Dimensionless_Math.Sqrt (X) <= Above;
   --  The square root is never below a machine number whose square is at most X, nor above one whose square is
   --  at least X: it is one of the two machine numbers around the exact root. IEEE 754 requires more, the nearer
   --  of the two.

   procedure Lemma_Cube_Root_Is_Close (X, Below, Above : Dimensioned_Float)
   with
     Ghost,
     Global => null,
     Pre    =>
       X >= 0.0
       and then Below >= 0.0
       and then Above >= 0.0
       and then R (Below) * R (Below) * R (Below) <= R (X)
       and then R (X) <= R (Above) * R (Above) * R (Above),
     Post   =>
       R (Cube_Root (X)) >= R (Below) * (1.0 - Cube_Root_Slack)
       and then R (Cube_Root (X)) <= R (Above) * (1.0 + Cube_Root_Slack);
   --  The cube root is within one part in 2**40 of any machine numbers whose cubes bracket X. It is a power with the
   --  exponent 1/3 rounded to a machine number, which alone can move the result by one part in 2**46 at the ends of
   --  the range, so this is not a claim of faithful rounding.

   Log_Magnitude_Max : constant := 746.0;

   procedure Lemma_Log_Is_Bounded (X : Dimensioned_Float)
   with Ghost, Global => null, Pre => X > 0.0, Post => abs Dimensionless_Math.Log (X) <= Log_Magnitude_Max;
   --  The logarithm of a positive machine number lies between that of the smallest, Log (2**-1074) = -744.44, and
   --  that of the largest, Log (2**1024) = 709.79.

private
   pragma SPARK_Mode (Off);

   function Cube_Root (X : Dimensionless) return Dimensionless
   is (X**(1.0 / 3.0));

   function Fifth_Root (X : Dimensionless) return Dimensionless
   is (Dimensionless_Math."**" (X, 0.2));

   function Bounded_Exp (X : Dimensionless) return Dimensionless
   is (Dimensionless_Math.Exp (X));

   function Fifth_Power (X : Dimensionless) return Dimensionless
   is (X**5);

   procedure Lemma_Square_Root_Is_Faithful (X, Below, Above : Dimensioned_Float) is null;
   procedure Lemma_Cube_Root_Is_Close (X, Below, Above : Dimensioned_Float) is null;
   procedure Lemma_Log_Is_Bounded (X : Dimensioned_Float) is null;

end Prunt.Motion_Planner.Trajectory.Elementary;
