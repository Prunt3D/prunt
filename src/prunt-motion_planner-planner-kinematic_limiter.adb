-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

package body Prunt.Motion_Planner.Planner.Kinematic_Limiter is

   procedure Run (Block : in out Execution_Block) is
      function Curve_Corner_Distance (Finishing_Corner : Corners_Index) return Length is
         Start_Curve_Half_Distance : constant Length :=
           Distance_At_T (Block.Beziers (Finishing_Corner - 1), 1.0) -
           Distance_At_T (Block.Beziers (Finishing_Corner - 1), 0.5);
         End_Curve_Half_Distance   : constant Length := Distance_At_T (Block.Beziers (Finishing_Corner), 0.5);
         Mid_Distance              : constant Length :=
           abs
           (Point_At_T (Block.Beziers (Finishing_Corner), 0.0) -
            Point_At_T (Block.Beziers (Finishing_Corner - 1), 1.0));
      begin
         return Mid_Distance;
         --  return Start_Curve_Half_Distance + Mid_Distance + End_Curve_Half_Distance;
      end Curve_Corner_Distance;
   begin
      Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'First) := 0.0 * mm / s;
      Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'Last)  := 0.0 * mm / s;

      for I in Block.Segment_Feedrates'Range loop
         declare
            Offset   : constant Scaled_Position_Offset := Block.Corners (I - 1) - Block.Corners (I);
            Has_XYZ  : constant Boolean                :=
              (Offset with delta E_Axis => 0.0 * mm) /= Scaled_Position_Offset'(others => Length (0.0));

            Feedrate : Velocity := Block.Segment_Feedrates (I);
         begin
            if abs Offset > 0.0 * mm and Feedrate /= Velocity'Last then
               Feedrate := Feedrate * abs (Offset / Block.Params.Axial_Scaler) / abs (Offset);
            end if;

            if Block.Params.Ignore_E_In_XYZE and Has_XYZ and Feedrate /= Velocity'Last then
               Feedrate := Feedrate * (abs Offset / abs [Offset with delta E_Axis => 0.0 * mm]);
            end if;

            Feedrate := Velocity'Min (Feedrate, Block.Params.Tangential_Velocity_Max);

            for A in Axis_Name loop
               if abs Offset (A) > 0.0 * mm then
                  Feedrate :=
                    Velocity'Min
                      (Feedrate,
                       Block.Params.Axial_Velocity_Maxes (A) / Block.Params.Axial_Scaler (A) * abs Offset /
                       abs Offset (A));
               end if;
            end loop;

            Block.Segment_Feedrates (I) := Feedrate;
         end;
      end loop;

      for I in Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
         declare
            Limit           : Velocity := Velocity'Min (Block.Segment_Feedrates (I), Block.Segment_Feedrates (I + 1));
            Optimal_Profile : Feedrate_Profile_Times;

            Inverse_Curvature : constant Length := PH_Beziers.Inverse_Curvature (Block.Beziers (I));
         begin
            --  Inverse curvature range is 0..Length'Last. Make sure to avoid overflow here.  GCC with optimisation
            --  enabled may transform sqrt(x)*sqrt(y) to sqrt(x*y) etc., but that should be fine in optimised builds
            --  with Ada's checks disabled as the Velocity'Min call will immediately discard the resulting infinity.
            Limit := Velocity'Min (Limit, Block.Params.Acceleration_Max**(1 / 2) * Inverse_Curvature**(1 / 2));
            Limit := Velocity'Min (Limit, Block.Params.Jerk_Max**(1 / 3) * Inverse_Curvature**(2 / 3));
            Limit := Velocity'Min (Limit, Block.Params.Snap_Max**(1 / 4) * Inverse_Curvature**(3 / 4));
            Limit := Velocity'Min (Limit, Block.Params.Crackle_Max**(1 / 5) * Inverse_Curvature**(4 / 5));

            --  TODO: Add limit based on interpolation time.
            --  TODO: Snap and crackle limits currently do not match the paper and are likely overly conservative.

            Optimal_Profile :=
              Optimal_Profile_For_Distance
                (Block.Corner_Velocity_Limits (I - 1),
                 Curve_Corner_Distance (I),
                 Block.Params.Acceleration_Max,
                 Block.Params.Jerk_Max,
                 Block.Params.Snap_Max,
                 Block.Params.Crackle_Max);
            Limit           :=
              Velocity'Min
                (Limit,
                 Fast_Velocity_At_Max_Time
                   (Optimal_Profile, 0.97 * Block.Params.Crackle_Max, Block.Corner_Velocity_Limits (I - 1)));
            --  The 0.97 here ensures that no feedrate profiles end up with a very small accel/decel part which can
            --  lead to numerical errors that cause kinematic limits to be greatly exceeded for a single interpolation
            --  period. If this is removed, then the sanity check in Feedrate_Profile_Generator also needs to be
            --  removed.
            --
            --  TODO: Check whether this actually matters in practice.

            Block.Corner_Velocity_Limits (I) := Limit;
         end;
      end loop;

      for I in reverse Block.Corner_Velocity_Limits'First + 1 .. Block.Corner_Velocity_Limits'Last - 1 loop
         declare
            Optimal_Profile : Feedrate_Profile_Times;
         begin
            Optimal_Profile                  :=
              Optimal_Profile_For_Distance
                (Block.Corner_Velocity_Limits (I + 1),
                 Curve_Corner_Distance (I + 1),
                 Block.Params.Acceleration_Max,
                 Block.Params.Jerk_Max,
                 Block.Params.Snap_Max,
                 Block.Params.Crackle_Max);
            Block.Corner_Velocity_Limits (I) :=
              Velocity'Min
                (Block.Corner_Velocity_Limits (I),
                 Fast_Velocity_At_Max_Time
                   (Optimal_Profile, 0.97 * Block.Params.Crackle_Max, Block.Corner_Velocity_Limits (I + 1)));
         end;
      end loop;
   end Run;

end Prunt.Motion_Planner.Planner.Kinematic_Limiter;
