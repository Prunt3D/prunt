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

package body Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator is

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
      for I in Block.Feedrate_Profiles'Range loop
         Block.Feedrate_Profiles (I) :=
           Optimal_Full_Profile
             (Start_Vel            => Block.Corner_Velocity_Limits (I - 1),
              Start_Coast_Distance =>
                Distance_At_T (Block.Beziers (I - 1), 1.0) - Distance_At_T (Block.Beziers (I - 1), 0.5),
              Max_Vel              => Block.Segment_Feedrates (I),
              End_Vel              => Block.Corner_Velocity_Limits (I),
              End_Coast_Distance   => Distance_At_T (Block.Beziers (I), 0.5),
              Mid_Distance         => Curve_Corner_Distance (I),
              Acceleration_Max     => Block.Params.Acceleration_Max,
              Jerk_Max             => Block.Params.Jerk_Max,
              Snap_Max             => Block.Params.Snap_Max,
              Crackle_Max          => Block.Params.Crackle_Max);
      end loop;
   end Run;

end Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;
