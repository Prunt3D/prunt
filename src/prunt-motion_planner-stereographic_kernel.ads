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

--  The proved kernel of the stereographic corner blender (Prunt.Motion_Planner.Stereographic_Curves): interval
--  arithmetic, the geometry certificates of the ideal curve, the sampled certificate of the tangent which the realtime
--  evaluator integrates, and the theorems about the motion which is executed.
--
--  These units are in SPARK and proved. The construction code in Stereographic_Curves is proved free of run-time
--  errors except in the few subprograms marked Skip_Proof, and Create_Blend returns a curve only with certificates
--  issued by the units here. prove.sh runs the proofs, and docs/spark_blender.md says what they establish and what
--  they assume.

package Prunt.Motion_Planner.Stereographic_Kernel
  with SPARK_Mode => On
is
end Prunt.Motion_Planner.Stereographic_Kernel;
