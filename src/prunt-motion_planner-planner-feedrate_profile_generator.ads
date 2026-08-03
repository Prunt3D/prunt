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
package Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator is

   procedure Run
     (Block     : aliased in out Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace : not null access constant Planning_Workspace);
   --  Fills Block.Feedrate_Profiles with profiles based on Block.Corner_Velocity_Limits and Block.Params.

private

   function Profile_Window_Time
     (Start_Vel       : Velocity;
      End_Vel         : Velocity;
      Distance        : Length;
      Max_Vel         : Velocity;
      Limits          : Scalar_Derivative_Limits;
      Prefix_Distance : Length;
      Suffix_Distance : Length) return Time;
   --  Compute the total traversal time for a profile-window candidate with the full profile solver. Returns a large
   --  sentinel value when no feasible profile can be generated.

   procedure Select_Feedrate_Profile_Window
     (Block            : not null access Execution_Block;
      Motor_Map        : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace        : not null access constant Planning_Workspace;
      Finishing_Corner : Finishing_Corners_Index);
   --  Select and store the first feasible fixed profile-window candidate in profile-time order.

end Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;
