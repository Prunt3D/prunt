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

--  An instance of the search with a residual of its own, for the proof of the search not to rest on the solvers.
--
--  GNATprove proves a generic through its instances. The two in Trajectory.Solvers carry the obligations of
--  their residuals as well; this one has the simplest residual there is, with a precondition and a bound so that
--  the instance exercises both of the ways in which an actual can constrain the search. Nothing calls it.

package Prunt.Motion_Planner.Trajectory.Searches.Witness
  with SPARK_Mode => On
is

   function Excess (X : Dimensionless) return Dimensionless
   is (X - 3.0)
   with Pre => X >= 0.0 and then X <= 1.0E6, Post => abs Excess'Result <= 1.0E6;

   function Evaluable (Lower, Upper : Dimensionless) return Boolean
   is (Lower >= 0.0 and then Upper <= 1.0E6);

   function Solve is new Largest_Nonpositive (Excess, Evaluable);

end Prunt.Motion_Planner.Trajectory.Searches.Witness;
