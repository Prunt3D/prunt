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

--  An upper bound on a square root which does not depend on how accurate the library's square root is: the
--  candidate is checked by an outward-rounded division, and moved upward until the check passes.

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Stereographic_Kernel.Square_Roots
  with SPARK_Mode => On
is
   use type Prunt.Motion_Planner.Reals.Real;

   function Certified_Upper_Square_Root (Value : Dimensionless) return Dimensionless
   with
     Post =>
       (if Value = 0.0 then Certified_Upper_Square_Root'Result = 0.0)
       and then
         (Certified_Upper_Square_Root'Result = Dimensionless'Last
          or else
            (Value >= 0.0
             and then Certified_Upper_Square_Root'Result >= 0.0
             and then Certified_Upper_Square_Root'Result < Dimensionless'Last
             and then R (Certified_Upper_Square_Root'Result) * R (Certified_Upper_Square_Root'Result) >= R (Value)));
   --  A number whose square is at least Value, close to its square root, or Dimensionless'Last when Value is
   --  negative or no such number was found.

end Prunt.Motion_Planner.Stereographic_Kernel.Square_Roots;
