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

pragma Extensions_Allowed (On);

private generic
package Prunt.Motion_Planner.Planner.Kinematic_Limiter is

   pragma Unsuppress (Overflow_Check);
   pragma Unsuppress (Range_Check);
   pragma Unsuppress (Division_Check);

   procedure Run
     (Block           : aliased in out Execution_Block;
      Motor_Map       : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace       : not null access Planning_Workspace;
      Fixed_Ends      : Boolean := False;
      Forced_Junction : Corners_Index'Base := 0);
   --  Fill Block.Corner_Velocity_Limits with velocity limits, that when enforced at corners, will prevent other
   --  limits from being violated.
   --
   --  First assigns timing runs (Assign_Timing_Runs), keeping every corner in Workspace.Timing_Join_Vetoes a junction.
   --  Only run boundaries are junctions: reachability is solved across a whole run, and interior corners keep no
   --  limit of their own.
   --
   --  Fixed_Ends retains the supplied endpoint velocities for a local replacement window. Forced_Junction, if nonzero,
   --  is a corner that must end a timing run.

end Prunt.Motion_Planner.Planner.Kinematic_Limiter;
