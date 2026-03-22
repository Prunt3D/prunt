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

package body Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator is

   pragma Extensions_Allowed (On);

   procedure Run (Block : in out Execution_Block) is
      function Curve_Corner_Distance (Finishing_Corner : Corners_Index) return Length;
      --  Calculates the total path length for a given segment, this is not equivalent to the l² norm of the difference
      --  between the corners. A segment runs from the midpoint of one blended corner along the curve, through a
      --  straight section, to the midpoint of the next blended corner along the curve, this function returns the
      --  length of that path.

      function Curve_Corner_Distance (Finishing_Corner : Corners_Index) return Length is
         Start_Curve_Half_Distance : constant Length :=
           Distance_At_T (Block.Beziers (Finishing_Corner - 1), 1.0)
           - Distance_At_T (Block.Beziers (Finishing_Corner - 1), 0.5);
         End_Curve_Half_Distance   : constant Length := Distance_At_T (Block.Beziers (Finishing_Corner), 0.5);
         Mid_Distance              : constant Length :=
           abs (Point_At_T (Block.Beziers (Finishing_Corner), 0.0)
                - Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0));
      begin
         return Start_Curve_Half_Distance + Mid_Distance + End_Curve_Half_Distance;
      end Curve_Corner_Distance;
   begin
      for I in Block.Feedrate_Profiles'Range loop
         Block.Feedrate_Profiles (I) :=
           Optimal_Full_Profile
             (Start_Vel        => Block.Corner_Velocity_Limits (I - 1),
              Max_Vel          => Block.Limited_Segment_Feedrates (I),
              End_Vel          => Block.Corner_Velocity_Limits (I),
              Distance         => Curve_Corner_Distance (I),
              Acceleration_Max => Block.Params.Acceleration_Max,
              Jerk_Max         => Block.Params.Jerk_Max,
              Snap_Max         => Block.Params.Snap_Max,
              Crackle_Max      => Block.Params.Crackle_Max);
      end loop;
   end Run;

end Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;
