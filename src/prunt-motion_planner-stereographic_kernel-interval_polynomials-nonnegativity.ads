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

--  The proof that a polynomial with interval controls is nonnegative on the unit interval.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Prunt.Motion_Planner.Stereographic_Kernel.Bernstein;

package Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials.Nonnegativity
  with SPARK_Mode => On
is

   Nonnegativity_Depth : constant := 8;
   --  How many times the nonnegativity test may bisect.

   subtype Bisection_Depth is Natural range 0 .. Nonnegativity_Depth;

   function Is_Nonnegative (Value : Polynomial) return Boolean
   with Post => (if Is_Nonnegative'Result then All_Valid (Value));
   --  Whether the polynomial whose controls Value encloses is proved nonnegative on [0, 1], from the controls' lower
   --  bounds and up to Nonnegativity_Depth levels of bisection. False is not a proof of a negative value. An invalid
   --  control gives False.

   procedure Lemma_Is_Nonnegative (Value : Polynomial; Exact : Bernstein.Controls; T : Real)
   with
     Ghost,
     Pre  => Is_Nonnegative (Value) and then Encloses (Value, Exact) and then 0.0 <= T and then T <= 1.0,
     Post => Bernstein.Value (Exact, T) >= 0.0;
   --  A polynomial which passes the test is nonnegative on the unit interval, whichever real polynomial within
   --  the interval controls it is.

end Prunt.Motion_Planner.Stereographic_Kernel.Interval_Polynomials.Nonnegativity;
