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

package body Prunt.Motion_Planner.Planner.Kinematic_Limiter is

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
      Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'First) := 0.0 * mm / s;
      Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'Last) := 0.0 * mm / s;

      --  Forward pass: Iterate from the second corner to the second-to-last corner. This pass calculates the maximum
      --  corner velocity based on:
      --
      --  1. The acceleration/jerk/etc. constraints imposed by the maximum curvature of the corner blend which
      --     approximately limit the axial acceleration/jerk/etc..
      --
      --  2. The maximum velocity achievable by accelerating from the previous corner's velocity over the length of the
      --     segment.
      --
      --  3. The maximum velocity allowed in the segment leading in to and out of the corner.
      --
      --  In this pass we are only concerned about the maximum reachable velocity under the above constraints.
      --  Deceleration constraints are added in the reverse pass.
      for I in Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
         declare
            Limit             : Velocity :=
              Velocity'Min (Block.Limited_Segment_Feedrates (I), Block.Limited_Segment_Feedrates (I + 1));
            Inverse_Curvature : constant Length := PH_Beziers.Inverse_Curvature (Block.Beziers (I));
         begin
            --  Inverse curvature range is 0..Length'Last. Make sure to avoid overflow here. GCC with optimisation
            --  enabled may transform sqrt(x)*sqrt(y) to sqrt(x*y) etc., but that should be fine in optimised builds
            --  with Ada's checks disabled as the Velocity'Min call will immediately discard the resulting infinity.
            Limit := Velocity'Min (Limit, Block.Params.Acceleration_Max ** (1 / 2) * Inverse_Curvature ** (1 / 2));
            Limit := Velocity'Min (Limit, Block.Params.Jerk_Max ** (1 / 3) * Inverse_Curvature ** (2 / 3));
            Limit := Velocity'Min (Limit, Block.Params.Snap_Max ** (1 / 4) * Inverse_Curvature ** (3 / 4));
            Limit := Velocity'Min (Limit, Block.Params.Crackle_Max ** (1 / 5) * Inverse_Curvature ** (4 / 5));

            --  TODO: Add limit based on interpolation time.
            --  TODO: Snap and crackle limits currently do not match the paper and are likely overly conservative.

            if Block.Corner_Dwell_Times (I) /= 0.0 * s then
               pragma Assert (Limit = 0.0 * mm / s);
            end if;

            Block.Corner_Velocity_Limits (I) :=
              Velocity'Min
                (Limit,
                 Fast_Velocity_At_Max_Time
                   (Optimal_Profile_For_Distance
                      (Block.Corner_Velocity_Limits (I - 1),
                       Curve_Corner_Distance (I),
                       Block.Params.Acceleration_Max,
                       Block.Params.Jerk_Max,
                       Block.Params.Snap_Max,
                       Block.Params.Crackle_Max),
                    0.97 * Block.Params.Crackle_Max,
                    Block.Corner_Velocity_Limits (I - 1)));
            --  The 0.97 here ensures that no feedrate profiles end up with a very small accel/decel part which can
            --  lead to numerical errors that cause kinematic limits to be greatly exceeded for a single interpolation
            --  period. If this is removed, then the sanity check in Feedrate_Profile_Generator also needs to be
            --  removed.
            --
            --  TODO: Check whether this actually matters in practice.
         end;
      end loop;

      --  Reverse pass: Iterate in reverse from the second-to-last corner to the second corner. For each corner, check
      --  if it's possible to decelerate from its current velocity limit to the next corner's velocity limit over the
      --  length of the connecting segment, if it is not then reduce the corner velocity so that it is possible.
      for I in reverse Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
         Block.Corner_Velocity_Limits (I) :=
           Velocity'Min
             (Block.Corner_Velocity_Limits (I),
              Fast_Velocity_At_Max_Time
                (Optimal_Profile_For_Distance
                   (Block.Corner_Velocity_Limits (I + 1),
                    Curve_Corner_Distance (I + 1),
                    Block.Params.Acceleration_Max,
                    Block.Params.Jerk_Max,
                    Block.Params.Snap_Max,
                    Block.Params.Crackle_Max),
                 0.97 * Block.Params.Crackle_Max,
                 Block.Corner_Velocity_Limits (I + 1)));
      end loop;
   end Run;

end Prunt.Motion_Planner.Planner.Kinematic_Limiter;
