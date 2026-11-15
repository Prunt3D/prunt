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

--  Estimates which steer the solvers' searches towards their results.
--
--  An estimate only decides where a search evaluates first, and nothing that is proved about the solvers depends
--  on its value. Both are proved to raise no exception: they first decide that their arguments are within
--  magnitudes for which nothing overflows, and offer no estimate otherwise. See docs/spark_trajectory.md.

package Prunt.Motion_Planner.Trajectory.Solvers.Estimates
  with SPARK_Mode => On
is

   function Snap_Limited_Guess (Vd : Velocity; Cm : Crackle; T1 : Time) return Dimensionless
   with Global => null;
   --  An estimate, in seconds, of the second phase time T2 for which the ramp with first phase time T1 and no
   --  third or fourth phase gains the speed Vd under crackle Cm. Negative when there is no estimate to offer.

   function Delta_V_Over_Distance (Solver : Delta_V_Solver; Start_Vel : Velocity; Distance : Length) return Velocity
   with Global => null;
   --  An estimate of the velocity change whose optimal ramp from Start_Vel, under Solver's limits, covers exactly
   --  Distance: the speed gained by Optimal_Profile_For_Distance. It is accurate to rounding but carries no
   --  certificate, so a caller confirms it against the ramps it actually builds. Negative when there is no
   --  estimate to offer.

end Prunt.Motion_Planner.Trajectory.Solvers.Estimates;
