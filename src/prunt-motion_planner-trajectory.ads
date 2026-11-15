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

--  The feedrate profile (trajectory) generator: the 15-stage ramps of bounded crackle, their evaluation at a time, the
--  solvers for the fastest ramp and the fastest complete profile, and the search behind them. The planner uses
--  Profiles and Solvers directly.
--
--  These units are in SPARK. Every unit except the trusted Machine_Numbers, Elementary and Checked is proved free of
--  run-time errors within a proved domain of magnitudes, and the ghost units below Profile_Model, Certificates, Reals
--  and Rounding prove what the phase times and the computed results mean in exact arithmetic. prove.sh runs the
--  proofs, and docs/spark_trajectory.md says what they establish and what they assume.

package Prunt.Motion_Planner.Trajectory
  with SPARK_Mode => On
is

   subtype Time_Squared is Dimensioned_Float with Annotate => (Dimension_Check, Dimension, "Second", 2);

end Prunt.Motion_Planner.Trajectory;
