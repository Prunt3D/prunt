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
package Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator is

   pragma Unsuppress (Overflow_Check);
   pragma Unsuppress (Range_Check);
   pragma Unsuppress (Division_Check);

   procedure Run
     (Block       : aliased in out Execution_Block;
      Motor_Map   : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace   : not null access constant Planning_Workspace;
      Result      : out Profile_Planning_Result;
      Limit_Scale : Dimensionless := 1.0);
   --  Fills Block.Feedrate_Profiles with profiles based on Block.Corner_Velocity_Limits and Block.Params.
   --
   --  Result identifies the first segment that could not be certified. Only a valid result completes the block.
   --
   --  Limit_Scale reserves additional derivative and velocity margin for the stopped recovery path.

   procedure Select_Feedrate_Profile_Window
     (Block            : not null access Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace        : not null access constant Planning_Workspace;
      Finishing_Corner : Finishing_Corners_Index;
      Result           : out Profile_Planning_Result;
      Limit_Scale      : Dimensionless);
   --  Select and certify a single segment with fixed endpoint velocities and extrusion corrections.
   --  Also used for local corner alternatives; other segments and junctions remain unchanged.

end Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;
