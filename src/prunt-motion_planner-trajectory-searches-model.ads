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

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

--  The power-law model which steers Largest_Nonpositive towards its result.
--
--  The estimate only decides where the search evaluates next, and nothing that is proved about the search depends
--  on its value. It is proved to raise no exception: it first decides that the magnitudes are ones for which
--  nothing overflows, and offers no estimate otherwise. See docs/spark_trajectory.md.

package Prunt.Motion_Planner.Trajectory.Searches.Model
  with SPARK_Mode => On
is

   procedure Estimate_Result
     (Anchor, Target                   : Dimensionless;
      Previous_Offset, Previous_Excess : Dimensionless;
      Latest_Offset, Latest_Excess     : Dimensionless;
      Exponent                         : in out Dimensionless;
      Estimate                         : in out Dimensionless;
      Have_Estimate                    : in out Boolean)
   with Inline, Global => null;
   --  Model the excess of the residual over its value at Anchor as a power of the offset from Anchor, refitting
   --  Exponent to the latest two evaluations when both are usable, and set Estimate to where the excess reaches
   --  Target and Have_Estimate to True. Where the model has no estimate to offer, Estimate is left alone,
   --  Have_Estimate is False, and Exponent may already hold the refitted value.

end Prunt.Motion_Planner.Trajectory.Searches.Model;
