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

pragma Extensions_Allowed (On);

private generic
package Prunt.Motion_Planner.Planner.Extrusion_Density_Normalizer is

   procedure Run (Block : aliased in out Execution_Block; Workspace : not null access Planning_Workspace);
   --  Assign densities once, before corner blending. Each same-direction XYZ/E run uses its total E divided by total
   --  original path length only while every segment's E adjustment fits Extrusion_Rounding_Tolerance.  Travel,
   --  reversals, E-only moves and dwells terminate runs. Zero tolerance preserves the original ratios.  The pass is
   --  linear in the segment count; all corner-sized storage belongs to Block or Workspace.

end Prunt.Motion_Planner.Planner.Extrusion_Density_Normalizer;
