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
--  child package. The functions which find motion profiles and the points of a profile at given times are in
--  Prunt.Motion_Planner.Trajectory.

with Prunt.Input_Shapers;

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

   type Motion_Limits is record
      Tangential_Velocity_Max     : Velocity := Velocity'Last;
      Tangential_Acceleration_Max : Acceleration := Acceleration'Last;
      Tangential_Jerk_Max         : Jerk := Jerk'Last;
      Tangential_Snap_Max         : Snap := Snap'Last;
      Tangential_Crackle_Max      : Crackle := Crackle'Last;
      Axial_Velocity_Maxes        : Axial_Velocities := [others => Velocity'Last];
      Axial_Acceleration_Maxes    : Axial_Accelerations := [others => Acceleration'Last];
      Axial_Jerk_Maxes            : Axial_Jerks := [others => Jerk'Last];
      Axial_Snap_Maxes            : Axial_Snaps := [others => Snap'Last];
      Axial_Crackle_Maxes         : Axial_Crackles := [others => Crackle'Last];
   end record;
   --  Caps on the velocity and derivative limits of Kinematic_Parameters for individual moves. A cap only ever lowers
   --  the matching parameter, so the defaults leave every limit as configured. Every other parameter, including the
   --  workspace, cornering, extrusion handling and input shapers, still applies to whole blocks.

   No_Motion_Limits : constant Motion_Limits := (others => <>);

   function Motion_Limits_Are_Valid (Limits : Motion_Limits) return Boolean;
   --  Whether every cap is positive. NaN caps are not.

   function Capped_Parameters (Params : Kinematic_Parameters; Limits : Motion_Limits) return Kinematic_Parameters
   with Inline;
   --  Params with each velocity and derivative limit lowered to its cap in Limits.

   function Parameter_Limits (Params : Kinematic_Parameters) return Motion_Limits;
   --  The velocity and derivative limits of Params, as caps that leave Params unchanged.

   function Lower_Limits (Left, Right : Motion_Limits) return Motion_Limits;
   --  The lower of each pair of caps.

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

private

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

   generic
      with function Residual (X : Dimensionless) return Dimensionless;
   function Largest_Nonpositive
     (Lower, Upper                   : Dimensionless;
      Lower_Residual, Upper_Residual : Dimensionless;
      Relative_Tolerance             : Dimensionless := 0.0;
      Guess                          : Dimensionless := -1.0;
      Use_Model                      : Boolean := True) return Dimensionless
   with Inline;
   --  Trajectory.Searches.Largest_Nonpositive, the search behind every monotone velocity and time solve, which says
   --  what it returns. Its contract is proved for its SPARK callers; an instance of this one ignores it, whatever the
   --  build profile and wherever the instance is.

end Prunt.Motion_Planner;
