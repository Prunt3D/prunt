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

--  This package provides the framework for the 5th-order (bounded crackle) motion planner implemented in the `Planner`
--  child package. Functions to find motion profiles and to find points at given times are implemented in this package.

package Prunt.Motion_Planner is

   type Kinematic_Parameters is record
      Lower_Pos_Limit : Position := [others => 0.0 * mm];
      --  The minimum allowed coordinate for each axis, if any single component of a position is less than the related
      --  component of this array then the position is considered to be out of bounds.

      Upper_Pos_Limit : Position := [others => 0.0 * mm];
      --  The maximum allowed coordinate for each axis, if any single component of a position is greater than the
      --  related component of this array then the position is considered to be out of bounds.

      Ignore_E_In_XYZE : Boolean := True;
      --  When True, tangential velocity limits are based only on the XYZ axes.

      Shift_Blended_Corners : Boolean := False;
      --  When True, the corner blending algorithm will attempt to shift the virtual corner points so that the midpoint
      --  of the generated Bézier curve falls on the original corner's location. This allows for the blended paths to
      --  more closely match the original model that the g-code was generated from in some cases.

      Tangential_Velocity_Max : Velocity := 0.0 * mm / s;
      Axial_Velocity_Maxes    : Axial_Velocities := [others => 0.0 * mm / s];
      Acceleration_Max        : Acceleration := 0.0 * mm / s**2;
      Jerk_Max                : Jerk := 0.0 * mm / s**3;
      Snap_Max                : Snap := 0.0 * mm / s**4;
      Crackle_Max             : Crackle := 0.0 * mm / s**5;
      Chord_Error_Max         : Length := 0.0 * mm;
      Axial_Scaler            : Position_Scale := [others => 1.0];
   end record;

   type Max_Corners_Type is range 2 .. 2**63 - 1;

   type Feedrate_Profile_Times_Index is range 1 .. 4;
   type Feedrate_Profile_Times is array (Feedrate_Profile_Times_Index) of Time;
   --  Represents the timings for segments in a 15-phase motion profile. Note that some times are used for multiple segments.

   type Feedrate_Profile is tagged record
      --  Represents the timings for segments in a 31-phase motion profile.

      Accel : Feedrate_Profile_Times;
      Coast : Time;
      Decel : Feedrate_Profile_Times;
   end record;

   function Fast_Distance_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Length;
   --  Calculates the total distance covered during an acceleration or deceleration phase defined by `Profile` with a
   --  given starting velocity and maximum crackle.
   --
   --  For an acceleration phase `Max_Crackle` should be positive and for a deceleration phase `Max_Crackle` should be
   --  negative.
   --
   --  This function is an optimised version of `Distance_At_Time` where `T` is equal to `Total_Time (Profile)`. While
   --  this function is symbolically identical, it may not be numerically identical to `Distance_At_Time`, but this
   --  does not cause issues with the current design of the motion planner.

   function Fast_Velocity_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity;
   --  Calculates the final velocity after an acceleration or deceleration phase defined by `Profile` with a given
   --  starting velocity and maximum crackle.
   --
   --  For an acceleration phase `Max_Crackle` should be positive and for a deceleration phase `Max_Crackle` should be
   --  negative.
   --
   --  This function is an optimised version of `Velocity_At_Time` where `T` is equal to `Total_Time (Profile)`. While
   --  this function is symbolically identical, it may not be numerically identical to `Velocity_At_Time`, but this
   --  does not cause issues with the current design of the motion planner.

   function Total_Time (Times : Feedrate_Profile_Times) return Time;
   --  Calculates the total duration of a single acceleration or deceleration phase. This is not equivalent to the sum
   --  of components as some components are used multiple times.

   function Crackle_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Crackle;
   --  Returns the crackle at a specific time `T` within a single acceleration or deceleration phase. The crackle will
   --  be either `+Max_Crackle`, `-Max_Crackle`, or zero. For an acceleration phase `Max_Crackle` should be positive
   --  and for a deceleration phase `Max_Crackle` should be negative.
   --
   --  The return value may be negative.

   function Snap_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Snap;
   --  Returns the snap at a specific time `T` within a single acceleration or deceleration phase. For an acceleration
   --  phase `Max_Crackle` should be positive and for a deceleration phase `Max_Crackle` should be negative.
   --
   --  The return value may be negative.

   function Jerk_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Jerk;
   --  Returns the jerk at a specific time `T` within a single acceleration or deceleration phase. For an acceleration
   --  phase `Max_Crackle` should be positive and for a deceleration phase `Max_Crackle` should be negative.
   --
   --  The return value may be negative.

   function Acceleration_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Acceleration;
   --  Returns the acceleration at a specific time `T` within a single acceleration or deceleration phase. For an
   --  acceleration phase `Max_Crackle` should be positive and for a deceleration phase `Max_Crackle` should be
   --  negative.
   --
   --  The return value may be negative.

   function Velocity_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity;
   --  Returns the velocity at a specific time `T` within a single acceleration or deceleration phase. For an
   --  acceleration phase `Max_Crackle` should be positive and for a deceleration phase `Max_Crackle` should be
   --  negative.
   --
   --  The return value may be negative.

   function Distance_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length;
   --  Returns the distance from the start point at a specific time `T` within a single acceleration or deceleration
   --  phase. For an acceleration phase `Max_Crackle` should be positive and for a deceleration phase `Max_Crackle`
   --  should be negative.
   --
   --  The return value may be negative.

   function Total_Time (Profile : Feedrate_Profile) return Time;
   --  Calculates the total duration of a complete feedrate profile. This is not equivalent to the sum of components as
   --  some components are used multiple times.

   function Crackle_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Crackle;
   --  Returns the crackle at a specific time `T` within a feedrate profile. The crackle will be either `+Max_Crackle`,
   --  `-Max_Crackle`, or zero.
   --
   --  The return value may be negative.

   function Snap_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Snap;
   --  Returns the snap at a specific time `T` within a feedrate profile. The return value may be negative.

   function Jerk_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Jerk;
   --  Returns the jerk at a specific time `T` within a feedrate profile. The return value may be negative.

   function Acceleration_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Acceleration;
   --  Returns the acceleration at a specific time `T` within a feedrate profile. The return value may be negative.

   function Velocity_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity;
   --  Returns the velocity at a specific time `T` within a feedrate profile. The return value may be negative.

   function Distance_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length;
   --  Returns the distance from the start point at a specific time `T` within a feedrate profile. The return value may
   --  be negative.

   function Distance_At_Time
     (Profile            : Feedrate_Profile;
      T                  : Time;
      Max_Crackle        : Crackle;
      Start_Vel          : Velocity;
      Is_Past_Accel_Part : out Boolean) return Length;
   --  Returns the distance from the start point at a specific time `T` within a feedrate profile. The return value may
   --  be negative.
   --
   --  `Is_Past_Accel_Part` is set to True if `T` is in the coasting or deceleration part, otherwise it is set to False.

   function Optimal_Profile_For_Distance
     (Start_Vel        : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Feedrate_Profile_Times;
   --  Compute the acceleration part of a feedrate profile that has the lowest total time to travel the given distance
   --  without violating any of the given constraints. Note that there is no velocity limit here.

   function Optimal_Profile_For_Delta_V
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
      return Feedrate_Profile_Times;
   --  Compute the acceleration part of a feedrate profile that achieves the given change in velocity in the lowest
   --  time without violating any of the given constraints. Note that there is no distance limit here.

   function Optimal_Full_Profile
     (Start_Vel        : Velocity;
      Max_Vel          : Velocity;
      End_Vel          : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Feedrate_Profile;
   --  Compute the feedrate profile with the minimal time without violating the given constraints. Raises
   --  `Constraint_Error` if there is no legal feedrate profile which can meet the given constraints, specifically
   --  regarding `End_Vel` being reachable. Also raises `Constraint_Error` if `Start_Vel` or `End_Vel` are higher than
   --  `Max_Vel`.

end Prunt.Motion_Planner;
