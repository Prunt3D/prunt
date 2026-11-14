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
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

pragma Extensions_Allowed (On);

--  This package provides the framework for the 5th-order (bounded crackle) motion planner implemented in the Planner
--  child package. Functions to find motion profiles and to find points at given times are implemented in this package.

with Prunt.Input_Shapers;

private with Ada.Numerics.Generic_Elementary_Functions;

package Prunt.Motion_Planner is

   Homing_Move_Cancelled_Error : exception;
   --  Raised when a planner reset invalidates an in-progress homing-move position handshake.

   type Axial_Deviation_Limits is array (Axis_Name) of Length range 0.0 * mm .. Length'Last;
   --  XYZ entries constrain corner geometry for the purposes of corner blending. In Smooth_Deviation mode the E entry
   --  limits how far E may depart from the path-length-corrected reference while its density changes; zero requires a
   --  stop at every remaining density change. The E entry is unused in Instantaneous_Velocity_Change mode.

   type Cornering_Kind is (Stereographic, Circular, Biarc, Sharp_SCV);

   type Workspace_Bounds_Kind is (Rectangular_Workspace, Circular_Workspace);

   type Workspace_Bounds (Kind : Workspace_Bounds_Kind := Rectangular_Workspace) is record
      Lower_Z : Length := 0.0 * mm;
      Upper_Z : Length := 0.0 * mm;
      Lower_E : Length := 0.0 * mm;
      Upper_E : Length := 0.0 * mm;

      case Kind is
         when Rectangular_Workspace =>
            Lower_X : Length := 0.0 * mm;
            Upper_X : Length := 0.0 * mm;
            Lower_Y : Length := 0.0 * mm;
            Upper_Y : Length := 0.0 * mm;

         when Circular_Workspace =>
            Radius : Length := 0.0 * mm;
      end case;
   end record;

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

         when Biarc =>
            Biarc_Params : Biarc_Corner_Parameters;

         when Sharp_SCV =>
            Sharp_SCV_Params : Sharp_SCV_Corner_Parameters;
      end case;
   end record;
   --  Selects one corner-transition construction and contains only the parameters meaningful to that construction.
   --  Stereographic supports the existing line/helix combinations and matches position through its fourth distance
   --  derivative at each endpoint. Circular transitions are C1 and support line-to-line corners; their endpoint jerk,
   --  snap, and crackle limits are waived. Biarc is C1 for certifiable line/helix corners and has the same waiver at
   --  both endpoints and its internal splice. Sharp_SCV is C0 for primitives with usable tangents, so acceleration and
   --  every higher derivative limit are waived at its junction. Unsupported or uncertifiable geometry becomes a hard
   --  stop. These derivative waivers apply to XYZ; Extrusion_Cornering selects E's junction behavior.

   type Extrusion_Cornering_Kind is (Smooth_Deviation, Instantaneous_Velocity_Change);

   type Extrusion_Cornering_Parameters (Kind : Extrusion_Cornering_Kind := Smooth_Deviation) is record
      Velocity_Change_Max : Velocity range 0.0 * mm / s .. Velocity'Last := 1.0 * mm / s;
      --  Maximum absolute E velocity jump at a moving segment junction whose density change is not smoothed: every
      --  such junction in Instantaneous_Velocity_Change mode, and in Smooth_Deviation mode those the ratio minimums
      --  below exclude or that no transition fits. E follows the path-length-corrected reference exactly there, so
      --  the jump is the density change times the junction speed. Acceleration and higher derivative limits are
      --  waived only at the jump; all E limits still apply between junctions. Zero requires a stop at every such
      --  density change.

      case Kind is
         when Smooth_Deviation =>
            Transition_Distance_Max : Length range 0.0 * mm .. Length'Last := 2.0 * mm;
            --  Where the extrusion density changes at a moving junction, E's density moves between the two segment
            --  densities along a fixed smooth curve over the path either side of the junction. E stays a function of
            --  path position, so every E limit holds through the junction. Each curve is the narrowest whose E limits
            --  do not lower the junction speed that the feedrates and XYZ cornering allow, but it reaches at most this
            --  far along the path on each side, and within the E entry of the cornering model's deviation limits. A
            --  change that fits no curve, including any with zero E deviation or Sharp_SCV, is not smoothed.

            Smoothed_Decrease_Ratio_Min : Dimensionless range -1.0 .. 1.0 := 0.1;
            --  A density change whose magnitude does not grow is smoothed only where the outgoing density divided by
            --  the incoming density is at least this. The default leaves extrusion stops, reversals and drops below a
            --  tenth unsmoothed, so E does not keep extruding into a travel move. Zero smooths stops into travel but
            --  not reversals, -1 smooths every decrease and 1 smooths none.

            Smoothed_Increase_Ratio_Min : Dimensionless range -1.0 .. 1.0 := 0.1;
            --  A density change whose magnitude grows is smoothed only where the incoming density divided by the
            --  outgoing density is at least this. The default leaves extrusion starts, reversals and rises from below
            --  a tenth unsmoothed, so E does not start extruding during a travel move. Zero smooths starts from travel
            --  but not reversals, -1 smooths every increase and 1 smooths none.

         when Instantaneous_Velocity_Change =>
            null;
            --  No density change is smoothed.
      end case;
   end record;

   type Extrusion_Normalization_Parameters is record
      Relative_Adjustment_Max : Dimensionless range 0.0 .. 1.0 := 0.005;
      --  A segment's E displacement may always change by this fraction of itself, so a run treats real density
      --  differences below it as negligible.

      E_Resolution : Length range 0.0 * mm .. Length'Last := 0.000_01 * mm;
      --  The resolution of commanded E coordinates. Rounding both ends of a segment changes its E displacement by at
      --  most this.

      XYZ_Resolution : Length range 0.0 * mm .. Length'Last := 0.001 * mm;
      --  The resolution of commanded X, Y and Z coordinates, including a helix's centre. Rounding them changes a
      --  segment's length, and so the E displacement that a given density gives it.
   end record;
   --  How far Extrusion_Density_Normalizer may change a segment's commanded E displacement when it gives a run of
   --  segments one common density: the larger of Relative_Adjustment_Max of the displacement, and what rounding to
   --  the two resolutions could explain if the segment truly had the run's density. That bound scales with the run's
   --  density, not the segment's own, so a short move that commands far more E than its length explains stays a
   --  boundary. All zero disables density normalization. Runs retain their total extrusion before corner shortening.
   --  This is separate from E's junction deviation and velocity change.

   type Kinematic_Parameters is record
      Bounds : Workspace_Bounds := (Kind => Rectangular_Workspace, others => <>);

      Ignore_E_In_XYZE : Boolean := True;
      --  When True, tangential velocity limits are based only on the XYZ axes. This is usually what other motion
      --  planners do.

      Tangential_Velocity_Max : Velocity := 0.0 * mm / s;

      Tangential_Acceleration_Max : Acceleration := 1.0E100 * mm / s**2;
      Tangential_Jerk_Max         : Jerk := 1.0E100 * mm / s**3;
      Tangential_Snap_Max         : Snap := 1.0E100 * mm / s**4;
      Tangential_Crackle_Max      : Crackle := 1.0E100 * mm / s**5;
      --  Limits on the derivatives of the speed along the path, measured like Tangential_Velocity_Max: in XYZ when
      --  Ignore_E_In_XYZE is True, in XYZE otherwise, and in E for E-only moves. They bound how quickly the speed
      --  changes, not how quickly the path turns, so curvature is limited by the axial limits alone. The defaults
      --  disable them.

      Axial_Velocity_Maxes     : Axial_Velocities := [others => 0.0 * mm / s];
      Axial_Acceleration_Maxes : Axial_Accelerations := [others => 0.0 * mm / s**2];
      Axial_Jerk_Maxes         : Axial_Jerks := [others => 0.0 * mm / s**3];
      Axial_Snap_Maxes         : Axial_Snaps := [others => 0.0 * mm / s**4];
      Axial_Crackle_Maxes      : Axial_Crackles := [others => 0.0 * mm / s**5];
      Cornering                : Cornering_Parameters := (others => <>);
      Extrusion_Cornering      : Extrusion_Cornering_Parameters := (others => <>);
      Extrusion_Normalization  : Extrusion_Normalization_Parameters := (others => <>);
      Axial_Shapers            : Input_Shapers.Axial_Shaper_Parameters :=
        [others => (Kind => Input_Shapers.No_Shaper)];
      Continuous_Timing        : Boolean := False;
      --  Experimental. Plan one feedrate profile across each chain of line segments joined by passthrough corners,
      --  so speed keeps changing through those corners instead of returning to zero acceleration, jerk and snap at
      --  every segment boundary. Non-binding curved corners are also joined where the joined profile, certified piece
      --  by piece, is faster. Feedrate changes, stops, dwells and binding corners remain junctions.
   end record;

   function XY_Position_Is_In_Bounds (Pos : Position; Params : Kinematic_Parameters) return Boolean;
   --  Return whether the X/Y components of Pos are within the workspace.

   function Position_Is_In_Bounds (Pos : Position; Params : Kinematic_Parameters) return Boolean;
   --  Return whether every component is within the workspace.

   function Helix_Is_In_Bounds
     (Start_Pos, Finish_Pos, Center : Position; Clockwise : Boolean; Params : Kinematic_Parameters) return Boolean;
   --  Return whether the complete executed helix is inside the configured position limits. If the supplied geometry
   --  would be converted to a line by the planner, check that line instead.

   type Unit_Speed_Axial_Velocity_Bounds is array (Axis_Name) of Dimensionless;
   type Unit_Speed_Axial_Acceleration_Bounds is array (Axis_Name) of Curvature;
   type Unit_Speed_Axial_Jerk_Bounds is array (Axis_Name) of Curvature_To_2;
   type Unit_Speed_Axial_Snap_Bounds is array (Axis_Name) of Curvature_To_3;
   type Unit_Speed_Axial_Crackle_Bounds is array (Axis_Name) of Curvature_To_4;

   type Unit_Speed_Axial_Derivative_Bounds is record
      Velocity     : Unit_Speed_Axial_Velocity_Bounds := [others => 0.0];
      Acceleration : Unit_Speed_Axial_Acceleration_Bounds := [others => 0.0 / mm];
      Jerk         : Unit_Speed_Axial_Jerk_Bounds := [others => 0.0 / mm**2];
      Snap         : Unit_Speed_Axial_Snap_Bounds := [others => 0.0 / mm**3];
      Crackle      : Unit_Speed_Axial_Crackle_Bounds := [others => 0.0 / mm**4];
   end record;
   --  Component-wise bounds for |d^n x / ds^n| on a unit-speed path in scaled coordinates.

   type Scalar_Derivative_Limits is record
      Acceleration_Max : Acceleration := 1.0E100 * mm / s**2;
      Jerk_Max         : Jerk := 1.0E100 * mm / s**3;
      Snap_Max         : Snap := 1.0E100 * mm / s**4;
      Crackle_Max      : Crackle := 1.0E100 * mm / s**5;
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
     (Params       : Kinematic_Parameters;
      Bounds       : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel      : Velocity;
      Safety       : Dimensionless := 0.999;
      Scalar_Maxes : Scalar_Derivative_Limits := (others => <>)) return Mixed_Derivative_Limit_Result;
   --  Return scalar tangential limits after reserving axial derivative budget for unit-speed path curvature terms.
   --  Scalar_Maxes can restrict the budget to the derivative envelope of a particular generated profile.

   function Mixed_Derivative_Time_Scale
     (Params  : Kinematic_Parameters;
      Bounds  : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel : Velocity;
      Peaks   : Scalar_Derivative_Limits) return Dimensionless;
   --  Return a certified inverse time stretch in [0, 1] for a fixed velocity change. Stretching every ramp
   --  duration by 1 / Scale and multiplying crackle by Scale**4 preserves its velocity change. Its acceleration,
   --  jerk, snap and crackle peaks scale by Scale, Scale**2, Scale**3 and Scale**4 respectively. Max_Vel is fixed;
   --  zero means no positive time scale could be certified. Products are evaluated without intermediate overflow.

   type Max_Corners_Type is range 2 .. 2**63 - 1;

   type Max_Corners_Extra_Data_Type is range 2 .. 2**63 - 1;

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
   --  For a phase whose speeds are not negative the distance never decreases as T increases, to the last bit. A
   --  deceleration phase whose final speed is negative only by rounding is evaluated as ending at rest. One which
   --  really turns back is not, and its return value may be negative.

   type Prepared_Ramp is private;
   --  An acceleration or deceleration phase with everything Distance_At_Time derives from it before it looks at
   --  the time.

   function Prepare
     (Profile : Feedrate_Profile_Times; Max_Crackle : Crackle; Start_Vel : Velocity) return Prepared_Ramp;

   function Distance_At_Time (Ramp : Prepared_Ramp; T : Time) return Length;
   --  Distance_At_Time (Profile, T, Max_Crackle, Start_Vel) for the arguments Ramp was prepared from: the same
   --  value to the last bit, for a fraction of the work. Preparing costs about as much as ten evaluations.

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

   type Full_Profile_Result is record
      Reachable : Boolean := False;
      Profile   : Feedrate_Profile := (Accel => [others => 0.0 * s], Coast => 0.0 * s, Decel => [others => 0.0 * s]);
   end record;

   function Try_Optimal_Full_Profile
     (Start_Vel        : Velocity;
      Max_Vel          : Velocity;
      End_Vel          : Velocity;
      Distance         : Length;
      Acceleration_Max : Acceleration;
      Jerk_Max         : Jerk;
      Snap_Max         : Snap;
      Crackle_Max      : Crackle) return Full_Profile_Result;
   --  As Optimal_Full_Profile, except that when Distance is too short to reach End_Vel from Start_Vel the result has
   --  Reachable set to False and an empty profile instead of Constraint_Error being raised. That outcome is ordinary
   --  for a caller trying candidate limits, and propagating an exception costs far more than returning. Every other
   --  error still raises Constraint_Error.

private

   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   type Feedrate_Profile_Stage_Index is range 1 .. 15;

   type Ramp_Stage is record
      Start, Len : Time := 0.0 * s;
      --  When the stage starts and how long it nominally lasts.
      Vel        : Velocity := 0.0 * mm / s;
      Acc_2      : Acceleration := 0.0 * mm / s**2;
      Jr_6       : Jerk := 0.0 * mm / s**3;
      Sn_24      : Snap := 0.0 * mm / s**4;
      C_120      : Crackle := 0.0 * mm / s**5;
      --  The coefficients of the distance gained within the stage, as a polynomial in the time into it.
      Dist       : Length := 0.0 * mm;
      --  The distance gained before the stage.
      Lo, Hi     : Length := 0.0 * mm;
      --  What the distance gained by a time in the stage is kept between. Hi of a stage is at most Lo of the next.
   end record;
   --  One of the fifteen stages of constant crackle of a ramp which speeds up from rest.

   type Ramp_Stage_Bounds is array (Feedrate_Profile_Stage_Index range 1 .. 14) of Time;
   type Ramp_Stage_Array is array (Feedrate_Profile_Stage_Index) of Ramp_Stage;

   type Ramp_Kind is (Speeds_Up, Slows_Down, Turns_Back);

   type Ramp_Shape is record
      Kind  : Ramp_Kind;
      Vel   : Velocity;
      --  The final speed of a ramp which slows down, and the starting speed of any other.
      Total : Time;
      Whole : Length;
      --  The time of the whole ramp and, for a ramp which slows down, its distance.
   end record;
   --  How the distance of a ramp follows from that of the ramp which speeds up from rest with its phase times.

   type Prepared_Ramp is record
      Shape  : Ramp_Shape;
      Bounds : Ramp_Stage_Bounds;
      Stages : Ramp_Stage_Array;
   end record;

   type Constraint_Region is (Region_1, Region_2, Region_3, Region_4, Region_5);

   type Internal_Profile_Result is record
      Profile : Feedrate_Profile_Times;
      Region  : Constraint_Region;
      Index   : Integer;
   end record;

   type Scaled_Nonnegative is record
      Fraction : Dimensionless := 0.0;
      Exponent : Integer := 0;
   end record;
   --  Fraction * Dimensionless'Machine_Radix ** Exponent. A nonzero Fraction is normalized as by 'Fraction.

   type Mixed_Derivative_Axis_Coefficients is record
      X1, X2, X3, X4, X5   : Scaled_Nonnegative;
      Acceleration_Maximum : Scaled_Nonnegative;
      Jerk_Maximum         : Scaled_Nonnegative;
      Snap_Maximum         : Scaled_Nonnegative;
      Crackle_Maximum      : Scaled_Nonnegative;
   end record;
   --  Decomposed unit-speed path derivative bounds and axial limits for one axis.

   type Mixed_Derivative_Axis_Coefficient_Array is array (Axis_Name) of Mixed_Derivative_Axis_Coefficients;
   type Mixed_Derivative_Axis_Flags is array (Axis_Name) of Boolean;

   type Mixed_Derivative_Plain_Coefficients is record
      X1, X2, X3, X4, X5   : Dimensionless := 0.0;
      Acceleration_Maximum : Dimensionless := 0.0;
      Jerk_Maximum         : Dimensionless := 0.0;
      Snap_Maximum         : Dimensionless := 0.0;
      Crackle_Maximum      : Dimensionless := 0.0;
   end record;
   --  The same bounds and limits for one axis as plain numbers, with negative values replaced by zero.

   type Mixed_Derivative_Plain_Coefficient_Array is array (Axis_Name) of Mixed_Derivative_Plain_Coefficients;

   subtype Time_Squared is Dimensioned_Float with Dimension => (Symbol => "s²", Second => 2, others => 0);

   type Delta_V_Solver is record
      Limits_Valid      : Boolean := False;
      Zero_Limit        : Boolean := False;
      Unclassifiable    : Boolean := False;
      Threshold_Raises  : Boolean := False;
      Region            : Constraint_Region := Region_5;
      Am                : Acceleration := 0.0 * mm / s**2;
      Jm                : Jerk := 0.0 * mm / s**3;
      Cm                : Crackle := 0.0 * mm / s**5;
      T1, T2, T3        : Time := 0.0 * s;
      Accel_Over_Jerk   : Time := 0.0 * s;
      Jerk_Over_Snap    : Time := 0.0 * s;
      Snap_Over_Crackle : Time := 0.0 * s;
      Snap_Root         : Time := 0.0 * s;
      Jerk_Over_Crackle : Time_Squared := 0.0 * s**2;
      Snap_Only_Above   : Velocity := 0.0 * mm / s;
   end record;
   --  Everything in Optimal_Profile_For_Delta_V that depends only on the derivative limits: the constraint region,
   --  the phase times of its fully saturated case (T1 to T3) and the quotients and roots that the partially
   --  saturated cases combine with the velocity change. A search over velocity changes under fixed limits prepares
   --  this once. Every value is formed by the same operations, in the same order, as a direct solve, so a prepared
   --  solve returns identical profiles.
   --
   --  Unclassifiable records that selecting the region raised Constraint_Error, and Threshold_Raises that the
   --  snap-only velocity threshold did. A solve raises the same error at the point where it would need the value.

   type Mixed_Derivative_Cache is record
      Axes               : Mixed_Derivative_Axis_Coefficient_Array;
      Plain_Axes         : Mixed_Derivative_Plain_Coefficient_Array;
      Axes_Filled        : Mixed_Derivative_Axis_Flags := [others => False];
      Axes_Decomposed    : Mixed_Derivative_Axis_Flags := [others => False];
      Dimensionless_Axes : Mixed_Derivative_Axis_Flags := [others => False];
      Plain_Limit_Axes   : Mixed_Derivative_Axis_Flags := [others => False];
      Still_Axes         : Mixed_Derivative_Axis_Flags := [others => False];
      Tangent_Only_Axes  : Mixed_Derivative_Axis_Flags := [others => False];
      Velocity_Ceiling   : Velocity := 0.0 * mm / s;
      Ceiling_Filled     : Boolean := False;
      Ceiling_Usable     : Boolean := False;
   end record;
   --  Values that depend only on the kinematic parameters and path bounds, filled as a solve first needs them.
   --  A cache must only be reused with identical parameters and bounds, such as across a search over one window.
   --
   --  Plain_Axes and the flags are valid for an axis once it is Axes_Filled, which also records that its values
   --  were checked to be finite. Axes holds the same values decomposed for the extended-exponent arithmetic, and
   --  is valid once Axes_Decomposed: only a solve which needs that arithmetic for the axis pays for it.
   --  Dimensionless_Axes marks coefficients whose nonzero values are in [2**-64, 2**64), for a time-scaling solve,
   --  and Plain_Limit_Axes the same for a limit solve. A still axis has no nonzero path bound, so none of its
   --  budgets can bind, and a tangent-only axis has only the velocity bound.

   function Mixed_Derivative_Time_Scale
     (Params  : Kinematic_Parameters;
      Bounds  : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel : Velocity;
      Peaks   : Scalar_Derivative_Limits;
      Cache   : in out Mixed_Derivative_Cache) return Dimensionless;
   --  Identical to the uncached form, but reuses decompositions across calls for the same Params and Bounds.

   function Mixed_Derivative_Limits
     (Params       : Kinematic_Parameters;
      Bounds       : Unit_Speed_Axial_Derivative_Bounds;
      Max_Vel      : Velocity;
      Safety       : Dimensionless;
      Scalar_Maxes : Scalar_Derivative_Limits;
      Cache        : in out Mixed_Derivative_Cache) return Mixed_Derivative_Limit_Result;
   --  Identical to the uncached form, with the same cache rules. A cache serves either this solve or the time
   --  scale, not both.

   function Profile_Is_Valid
     (Profile : Feedrate_Profile; Distance : Length; Start_Vel, End_Vel : Velocity; Max_Crackle : Crackle)
      return Boolean;
   --  Reject nonfinite or negative times, empty motion, and profiles that miss their distance or endpoint velocity.

   function Profile_Is_Valid
     (Profile                        : Feedrate_Profile;
      Distance                       : Length;
      Start_Vel, End_Vel             : Velocity;
      Max_Crackle                    : Crackle;
      Peak                           : Velocity;
      Accel_Distance, Decel_Distance : Length) return Boolean;
   --  Reuse the checked ramp evaluations when the caller already needed them to calculate coast time.

   function Derivative_Limits_Are_Valid (Am : Acceleration; Jm : Jerk; Sm : Snap; Cm : Crackle) return Boolean;
   --  Zero limits permit coasting, but negative or nonfinite limits are never meaningful.

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

   function Prepare_Delta_V
     (Acceleration_Max : Acceleration; Jerk_Max : Jerk; Snap_Max : Snap; Crackle_Max : Crackle) return Delta_V_Solver;
   --  Never raises: invalid limits and arithmetic failures are recorded for the solves that use the result.

   function Optimal_Profile_For_Delta_V (Solver : Delta_V_Solver; Delta_V : Velocity) return Feedrate_Profile_Times;
   --  Identical to Optimal_Profile_For_Delta_V for the limits that Solver was prepared with.

   function Optimal_Profile_For_Delta_V_Internal
     (Solver : Delta_V_Solver; Delta_V : Velocity) return Internal_Profile_Result;

   function Delta_V_Over_Distance (Solver : Delta_V_Solver; Start_Vel : Velocity; Distance : Length) return Velocity;
   --  An estimate of the velocity change whose optimal ramp from Start_Vel, under Solver's limits, covers exactly
   --  Distance: the speed gained by Optimal_Profile_For_Distance. It is accurate to rounding but carries no
   --  certificate, so a caller confirms it against the ramps it actually builds. Negative when there is no
   --  estimate to offer.

   generic
      with function Residual (X : Dimensionless) return Dimensionless;
   function Largest_Nonpositive
     (Lower, Upper                   : Dimensionless;
      Lower_Residual, Upper_Residual : Dimensionless;
      Relative_Tolerance             : Dimensionless := 0.0;
      Guess                          : Dimensionless := -1.0;
      Use_Model                      : Boolean := True) return Dimensionless;
   --  The search behind every monotone velocity and time solve. Residual is treated as nondecreasing over
   --  0 <= Lower < Upper, with Residual (Lower) <= 0 < Residual (Upper); neither end is evaluated. With a zero
   --  tolerance the result X is Lower or a point where Residual was evaluated and found nonpositive, and the next
   --  machine number above X is Upper or a point where Residual was evaluated and found positive. For a Residual
   --  that is monotone in machine arithmetic that is exactly the result of bisecting until the bracket's ends are
   --  adjacent, at a fraction of the evaluations; where rounding makes it locally nonmonotone both results still
   --  carry the same certificate, an evaluated nonpositive Residual.
   --
   --  The search first models Residual - Lower_Residual as a power of X - Lower, which fits ramp distances from a
   --  quarter power near a crackle-limited start through to a square when acceleration-limited, refitting the
   --  exponent to the latest two evaluations. Lower_Residual and Upper_Residual seed that model and need not be
   --  exact. Once the model's estimate stops moving, the bracket is closed around it in steps of machine numbers.
   --  Guess, when inside the bracket, replaces the model with a caller's estimate of the result. Without Use_Model
   --  and a Guess the residual ends are not used; an Upper_Residual that is not positive and finite only costs the
   --  model its first point. A positive Relative_Tolerance also stops at a bracket no wider than that fraction of
   --  its lower end, and then the model stops refining an estimate that moves by less than a quarter of it and the
   --  bracket is closed in steps of half of it.

end Prunt.Motion_Planner;
