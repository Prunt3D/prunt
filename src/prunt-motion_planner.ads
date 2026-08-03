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

--  This package provides the framework for the 5th-order (bounded crackle) motion planner implemented in the Planner
--  child package. Functions to find motion profiles and to find points at given times are implemented in this package.

with Prunt.Input_Shapers;

private with Ada.Numerics.Generic_Elementary_Functions;

package Prunt.Motion_Planner is

   type Axial_Deviation_Limits is array (Axis_Name) of Length range 0.0 * mm .. Length'Last;

   type Cornering_Kind is (Stereographic, Circular, Parabolic, Biarc, Sharp_SCV);

   type Stereographic_Corner_Parameters is record
      Axial_Deviation_Maxes    : Axial_Deviation_Limits := [others => 0.1 * mm];
      Corner_Miss_Distance_Max : Length := 0.1 * mm;
      Shape_Bias               : Dimensionless range -1.0 .. 1.0 := 0.0;
      Circularity              : Dimensionless range 0.0 .. 1.0 := 0.0;
   end record;

   type Circular_Corner_Parameters is record
      Axial_Deviation_Maxes    : Axial_Deviation_Limits := [others => 0.1 * mm];
      Corner_Miss_Distance_Max : Length := 0.1 * mm;
      Radius_Max               : Length := 1.0E100 * mm;
   end record;

   type Parabolic_Corner_Parameters is record
      Axial_Deviation_Maxes    : Axial_Deviation_Limits := [others => 0.1 * mm];
      Corner_Miss_Distance_Max : Length := 0.1 * mm;
      Shape_Bias               : Dimensionless range -1.0 .. 1.0 := 0.0;
      Trim_Max                 : Length := 1.0E100 * mm;
   end record;

   type Biarc_Corner_Parameters is record
      Axial_Deviation_Maxes    : Axial_Deviation_Limits := [others => 0.1 * mm];
      Corner_Miss_Distance_Max : Length := 0.1 * mm;
      Shape_Bias               : Dimensionless range -1.0 .. 1.0 := 0.0;
      Trim_Max                 : Length := 1.0E100 * mm;
   end record;

   type Sharp_SCV_Corner_Parameters is record
      Square_Corner_Velocity : Velocity := 5.0 * mm / s;
   end record;

   type Cornering_Parameters (Kind : Cornering_Kind := Stereographic) is record
      case Kind is
         when Stereographic =>
            Stereographic_Params : Stereographic_Corner_Parameters;

         when Circular =>
            Circular_Params : Circular_Corner_Parameters;

         when Parabolic =>
            Parabolic_Params : Parabolic_Corner_Parameters;

         when Biarc =>
            Biarc_Params : Biarc_Corner_Parameters;

         when Sharp_SCV =>
            Sharp_SCV_Params : Sharp_SCV_Corner_Parameters;
      end case;
   end record;
   --  Selects one corner-transition construction and contains only the parameters meaningful to that construction.
   --  Stereographic supports the existing line/helix combinations and matches position through its fourth distance
   --  derivative at each endpoint. Circular and Parabolic are C1 line-to-line transitions; their endpoint jerk, snap,
   --  and crackle limits are waived. Biarc is C1 for certifiable line/helix corners and has the same waiver at both
   --  endpoints and its internal splice. Sharp_SCV is C0 for primitives with usable tangents, so acceleration and
   --  every higher derivative limit are waived at its junction. Unsupported or uncertifiable geometry becomes a hard
   --  stop.

   type Kinematic_Parameters is record
      Lower_Pos_Limit : Position := [others => 0.0 * mm];
      --  The minimum allowed coordinate for each axis, if any single component of a position is less than the related
      --  component of this array then the position is considered to be out of bounds.

      Upper_Pos_Limit : Position := [others => 0.0 * mm];
      --  The maximum allowed coordinate for each axis, if any single component of a position is greater than the
      --  related component of this array then the position is considered to be out of bounds.

      Ignore_E_In_XYZE : Boolean := True;
      --  When True, tangential velocity limits are based only on the XYZ axes. This is usually what other motion
      --  planners do.

      Tangential_Velocity_Max  : Velocity := 0.0 * mm / s;
      Axial_Velocity_Maxes     : Axial_Velocities := [others => 0.0 * mm / s];
      Axial_Acceleration_Maxes : Axial_Accelerations := [others => 0.0 * mm / s ** 2];
      Axial_Jerk_Maxes         : Axial_Jerks := [others => 0.0 * mm / s ** 3];
      Axial_Snap_Maxes         : Axial_Snaps := [others => 0.0 * mm / s ** 4];
      Axial_Crackle_Maxes      : Axial_Crackles := [others => 0.0 * mm / s ** 5];
      Cornering                : Cornering_Parameters := (others => <>);
      Axial_Shapers            : Input_Shapers.Axial_Shaper_Parameters :=
        [others => (Kind => Input_Shapers.No_Shaper)];
   end record;

   type Unit_Speed_Axial_Velocity_Bounds is array (Axis_Name) of Dimensionless;
   type Unit_Speed_Axial_Acceleration_Bounds is array (Axis_Name) of Curvature;
   type Unit_Speed_Axial_Jerk_Bounds is array (Axis_Name) of Curvature_To_2;
   type Unit_Speed_Axial_Snap_Bounds is array (Axis_Name) of Curvature_To_3;
   type Unit_Speed_Axial_Crackle_Bounds is array (Axis_Name) of Curvature_To_4;

   type Unit_Speed_Axial_Derivative_Bounds is record
      Velocity     : Unit_Speed_Axial_Velocity_Bounds := [others => 0.0];
      Acceleration : Unit_Speed_Axial_Acceleration_Bounds := [others => 0.0 / mm];
      Jerk         : Unit_Speed_Axial_Jerk_Bounds := [others => 0.0 / mm ** 2];
      Snap         : Unit_Speed_Axial_Snap_Bounds := [others => 0.0 / mm ** 3];
      Crackle      : Unit_Speed_Axial_Crackle_Bounds := [others => 0.0 / mm ** 4];
   end record;
   --  Component-wise bounds for |d^n x / ds^n| on a unit-speed path in scaled coordinates.

   type Scalar_Derivative_Limits is record
      Acceleration_Max : Acceleration := 1.0E100 * mm / s ** 2;
      Jerk_Max         : Jerk := 1.0E100 * mm / s ** 3;
      Snap_Max         : Snap := 1.0E100 * mm / s ** 4;
      Crackle_Max      : Crackle := 1.0E100 * mm / s ** 5;
   end record;

   type Mixed_Derivative_Limit_Result is record
      Valid   : Boolean := False;
      Limits  : Scalar_Derivative_Limits;
      Max_Vel : Velocity := 0.0 * mm / s;
   end record;

   function Nth_Root_Ratio (Numerator, Denominator : Dimensionless; Degree : Positive) return Dimensionless;
   --  Return (Numerator / Denominator) raised to 1 / Degree without first forming the potentially overflowing or
   --  underflowing quotient. A nonpositive Numerator returns zero; a nonpositive Denominator returns
   --  Dimensionless'Last.

   function Constant_Speed_Axial_Ceiling
     (Params  : Kinematic_Parameters;
      Bounds  : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel : Velocity;
      Safety  : Dimensionless := 0.999) return Velocity;
   --  Return the maximum constant tangential speed that keeps velocity through crackle within axial limits.

   function Mixed_Derivative_Limits
     (Params  : Kinematic_Parameters;
      Bounds  : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel : Velocity;
      Safety  : Dimensionless := 0.999) return Mixed_Derivative_Limit_Result;
   --  Return scalar tangential limits after reserving axial derivative budget for unit-speed path curvature terms.

   type Max_Corners_Type is range 2 .. 2 ** 63 - 1;

   type Max_Corners_Extra_Data_Type is range 2 .. 2 ** 63 - 1;

   type Feedrate_Profile_Times_Index is range 1 .. 4;
   type Feedrate_Profile_Times is array (Feedrate_Profile_Times_Index) of Time;
   --  Represents the timings for segments in a 15-phase motion profile. Note that some times are used for multiple
   --  segments. The crackle profile represented by this array is as follows, where Tn represents an item of this array
   --  and C is the set crackle value:
   --
   --  Stage:     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
   --  Duration: T₁  T₂  T₁  T₃  T₁  T₂  T₁  T₄  T₁  T₂  T₁  T₃  T₁  T₂  T₁
   --  Crackle:  +C   0  -C   0  -C   0  +C   0  -C   0  +C   0  +C   0  -C
   --
   --  The total time of a profile is therefore:
   --
   --     ΔT = 8 T₁ + 4 T₂ + 2 T₃ + T₄.
   --
   --  For an acceleration profile the crackle is positive and for a deceleration profile the crackle is negative.
   --
   --  Denote the time of any given stage as t and the crackle of that stage as c. If a stage starts with (s, j, a, v,
   --  x) (crackle .. position) then after time t:
   --
   --     s⁺ = s + ct
   --     j⁺ = j + st + ct²/2
   --     a⁺ = a + jt + st²/2 + ct³/6
   --     v⁺ = v + at + jt²/2 + st³/6 + ct⁴/24
   --     x⁺ = x + vt + at²/2 + jt³/6 + st⁴/24 + ct⁵/120
   --
   --  Note that crackle through velocity all start at zero before any motions are executed. By repeatedly applying the
   --  above over a full motion profile it may be observed that snap, jerk, and acceleration will be zero at the end of
   --  a motion profile.
   --
   --  It may also be observed that:
   --
   --     - Maximum snap         = CT₁
   --     - Maximum jerk         = CT₁(T₁ + T₂)
   --     - Maximum acceleration = CT₁(T₁ + T₂)(2T₁ + T₂ + T₃)
   --
   --  Over the entire profile:
   --
   --     ΔV = CT₁(T₁ + T₂)(2T₁ + T₂ + T₃)(4T₁ + 2T₂ + T₃ + T₄)
   --
   --  Thus:
   --
   --     v(ΔT) = Vₛ + ΔV
   --
   --  This is the expression found in Fast_Velocity_At_Max_Time.
   --
   --  Because the acceleration profile is symmetrical around the middle of a profile, velocity has the identity:
   --
   --     v(t) + v(ΔT - t) = 2Vₛ + ΔV
   --
   --  Integrating over [0, ΔT] gives:
   --
   --     ΔX = (Vₛ + ΔV/2)ΔT
   --
   --  Substituting the above definitions of ΔV and ΔX into ΔX gives:
   --
   --     (Vₛ + CT₁(T₁ + T₂)(2T₁ + T₂ + T₃)(4T₁ + 2T₂ + T₃ + T₄) / 2)(8T₁ + 4T₂ + 2T₃ + T₄)
   --
   --  This is the expression found in Fast_Distance_At_Max_Time.

   type Feedrate_Profile is record
      --  Represents the timings for segments in a 31-phase motion profile. Refer to the documentation on
      --  Feedrate_Profile_Times for what the individual parts of this profile are.

      Accel : Feedrate_Profile_Times;
      Coast : Time;
      Decel : Feedrate_Profile_Times;
   end record;

   function Fast_Distance_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Length;
   --  Calculates the total distance covered during an acceleration or deceleration phase defined by Profile with a
   --  given starting velocity and maximum crackle.
   --
   --  For an acceleration phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be
   --  negative.
   --
   --  This function is an optimised version of Distance_At_Time where T is equal to Total_Time (Profile). While
   --  this function is symbolically identical, it may not be numerically identical to Distance_At_Time, but this
   --  does not cause issues with the current design of the motion planner.

   function Fast_Velocity_At_Max_Time
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity;
   --  Calculates the final velocity after an acceleration or deceleration phase defined by Profile with a given
   --  starting velocity and maximum crackle.
   --
   --  For an acceleration phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be
   --  negative.
   --
   --  This function is an optimised version of Velocity_At_Time where T is equal to Total_Time (Profile). While
   --  this function is symbolically identical, it may not be numerically identical to Velocity_At_Time, but this
   --  does not cause issues with the current design of the motion planner.

   function Total_Time (Times : Feedrate_Profile_Times) return Time;
   --  Calculates the total duration of a single acceleration or deceleration phase. This is not equivalent to the sum
   --  of components as some components are used multiple times.

   function Crackle_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Crackle;
   --  Returns the crackle at a specific time T within a single acceleration or deceleration phase. The crackle will
   --  be either +Max_Crackle, -Max_Crackle, or zero. For an acceleration phase Max_Crackle should be positive
   --  and for a deceleration phase Max_Crackle should be negative.
   --
   --  The return value may be negative.

   function Snap_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Snap;
   --  Returns the snap at a specific time T within a single acceleration or deceleration phase. For an acceleration
   --  phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be negative.
   --
   --  The return value may be negative.

   function Jerk_At_Time (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Jerk;
   --  Returns the jerk at a specific time T within a single acceleration or deceleration phase. For an acceleration
   --  phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be negative.
   --
   --  The return value may be negative.

   function Acceleration_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle) return Acceleration;
   --  Returns the acceleration at a specific time T within a single acceleration or deceleration phase. For an
   --  acceleration phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be
   --  negative.
   --
   --  The return value may be negative.

   function Velocity_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity;
   --  Returns the velocity at a specific time T within a single acceleration or deceleration phase. For an
   --  acceleration phase Max_Crackle should be positive and for a deceleration phase Max_Crackle should be
   --  negative.
   --
   --  The return value may be negative.

   function Distance_At_Time
     (Profile : Feedrate_Profile_Times; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length;
   --  Returns the distance from the start point at a specific time T within a single acceleration or deceleration
   --  phase. For an acceleration phase Max_Crackle should be positive and for a deceleration phase Max_Crackle
   --  should be negative.
   --
   --  The return value may be negative.

   function Total_Time (Profile : Feedrate_Profile) return Time;
   --  Calculates the total duration of a complete feedrate profile. This is not equivalent to the sum of components as
   --  some components are used multiple times.

   function Crackle_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Crackle;
   --  Returns the crackle at a specific time T within a feedrate profile. The crackle will be either +Max_Crackle,
   --  -Max_Crackle, or zero.
   --
   --  The return value may be negative.

   function Snap_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Snap;
   --  Returns the snap at a specific time T within a feedrate profile. The return value may be negative.

   function Jerk_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Jerk;
   --  Returns the jerk at a specific time T within a feedrate profile. The return value may be negative.

   function Acceleration_At_Time (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle) return Acceleration;
   --  Returns the acceleration at a specific time T within a feedrate profile. The return value may be negative.

   function Velocity_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Velocity;
   --  Returns the velocity at a specific time T within a feedrate profile. The return value may be negative.

   function Distance_At_Time
     (Profile : Feedrate_Profile; T : Time; Max_Crackle : Crackle; Start_Vel : Velocity) return Length;
   --  Returns the distance from the start point at a specific time T within a feedrate profile. The return value may
   --  be negative.

   function Distance_At_Time
     (Profile            : Feedrate_Profile;
      T                  : Time;
      Max_Crackle        : Crackle;
      Start_Vel          : Velocity;
      Is_Past_Accel_Part : out Boolean) return Length;
   --  Returns the distance from the start point at a specific time T within a feedrate profile. The return value may
   --  be negative.
   --
   --  Is_Past_Accel_Part is set to True if T is in the coasting or deceleration part, otherwise it is set to
   --  False.

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
   --  Constraint_Error if there is no legal feedrate profile which can meet the given constraints, specifically
   --  regarding End_Vel being reachable. Also raises Constraint_Error if Start_Vel or End_Vel are higher than
   --  Max_Vel.

private

   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   type Constraint_Region is (Region_1, Region_2, Region_3, Region_4, Region_5);

   type Internal_Profile_Result is record
      Profile : Feedrate_Profile_Times;
      Region  : Constraint_Region;
      Index   : Integer;
   end record;

   function Optimal_Profile_For_Distance_Internal
     (Start_Vel        : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Internal_Profile_Result;
   --  Solve the minimum-time zero-endpoint-velocity profile for Distance and retain its active constraint region.

   function Optimal_Profile_For_Delta_V_Internal
     (Delta_V : Velocity; Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle)
      return Internal_Profile_Result;
   --  Solve the minimum-time derivative-limited transition for Delta_V and retain its active constraint region.

end Prunt.Motion_Planner;
