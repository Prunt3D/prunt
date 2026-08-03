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

package Prunt.Motion_Planner.Stereographic_Curves.Geometry is
   type Distance_Interval is record
      Lower : Length := 0.0 * mm;
      --  Proven lower bound on the requested distance.

      Upper : Length := Length'Last;
      --  Proven upper bound. Length'Last means that no useful finite upper bound could be established without
      --  violating fixed-work guarantees.
   end record
   with
     Dynamic_Predicate =>
       Distance_Interval.Lower >= 0.0 * mm
       and then Distance_Interval.Lower <= Distance_Interval.Upper
       and then Distance_Interval.Upper <= Length'Last,
     Predicate_Failure => "a geometry interval must be ordered and nonnegative";
   --  A certified enclosure of one nonnegative geometric distance. The interval width reports the remaining
   --  uncertainty after the fixed-work analysis.

   function Maximum_Deviation_From_Line_Corner
     (Curve                  : Stereographic_Curve;
      Start, Corner, Finish : Position;
      Maximum_Interval_Width : Length) return Distance_Interval
   with
     Pre =>
       Maximum_Interval_Width > 0.0 * mm
       and then Maximum_Interval_Width <= Length'Last
       and then
         (for all Axis in Axis_Name =>
            Start (Axis) >= -Length'Last
            and then Start (Axis) <= Length'Last
            and then Corner (Axis) >= -Length'Last
            and then Corner (Axis) <= Length'Last
            and then Finish (Axis) >= -Length'Last
            and then Finish (Axis) <= Length'Last);
   --  Bound the largest distance from the executed curve to the union of the line segment Start--Corner and the line
   --  segment Corner--Finish.
   --
   --  Maximum_Interval_Width is a requested precision, not an unconditional postcondition. The result may be wider
   --  when representation error, floating-point range, or the fixed work limit prevents a tighter certificate.

   function Minimum_Distance_To_Point
     (Curve : Stereographic_Curve; Point : Position; Maximum_Interval_Width : Length) return Distance_Interval
   with
     Pre =>
       Maximum_Interval_Width > 0.0 * mm
       and then Maximum_Interval_Width <= Length'Last
       and then (for all Axis in Axis_Name => Point (Axis) >= -Length'Last and then Point (Axis) <= Length'Last);
   --  Bound the smallest distance from any executed curve point to Point. Maximum_Interval_Width has the same
   --  fixed-work semantics as above.

   function Point_To_Point_Distance (Left, Right : Position) return Distance_Interval;
   --  Certify the Euclidean distance between two represented machine positions.

private

   ---------------------------------------------------------------------------
   --  Fixed-work strategy
   ---------------------------------------------------------------------------

   Fixed_Capsule_Segments : constant Positive := 1_024;
   --  Number of equal parameter ranges in the tight curvature-capsule path.

   Fixed_Lipschitz_Segments : constant Positive := 48;
   --  Cheap fixed sampling count used when it already meets the requested precision.

   Maximum_Lipschitz_Segments : constant Positive := 1_024;
   --  Hard sampling limit for requests between the cheap and tight paths.

   Capsule_Precision_Threshold : constant Length := 1.0E-3 * mm;
   --  Ordinary planner queries use a sampled Lipschitz enclosure. Tighter diagnostic queries use a curvature capsule
   --  for every fixed segment.

   type Metric_Kind is
     (Point_Minimum,
      --  Minimum distance from the curve to one fixed point.

      Line_Corner_Maximum
      --  Maximum distance from the curve to either leg of a line corner.
     );
   --  Selects the geometric quantity accumulated by the shared fixed-work bounding algorithm.

   type Capsule is record
      Lower : Length := 0.0 * mm;
      --  Metric lower bound on one parameter range.

      Upper : Length := Length'Last;
      --  Metric upper bound on the same range.
   end record
   with
     Dynamic_Predicate =>
       Capsule.Lower >= 0.0 * mm and then Capsule.Lower <= Capsule.Upper and then Capsule.Upper <= Length'Last;
   --  A metric enclosure contributed by one closed curve-parameter interval.

   subtype Capsule_Index is Positive range 1 .. Fixed_Capsule_Segments;
   --  Index of one interval in the fixed fine partition used by curvature capsules.

   type Capsule_Array is array (Capsule_Index) of Capsule;
   --  Fixed-capacity storage for all fine-partition capsule results.

   Invalid_Geometry_Interval : constant Interval :=
     (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
   --  Marker returned by private interval arithmetic when a finite enclosure cannot be established.

   ---------------------------------------------------------------------------
   --  Dimensionless interval and length arithmetic
   ---------------------------------------------------------------------------

   function Square_Interval (Value : Interval) return Interval;
   --  Conservatively enclose the square of every value in Value, accounting for an interval that crosses zero.

   function Divide_Intervals (Left, Right : Interval) return Interval;
   --  Conservatively enclose Left divided by Right. The result is invalid when Right may contain zero.

   function Square_Root_Interval (Value : Interval) return Interval;
   --  Enclose the nonnegative square root of Value, rejecting an interval that is wholly negative or invalid.

   function Norm_Interval (Value : Interval_Position_Scale) return Interval;
   --  Enclose the Euclidean norm of a vector whose machine-axis components are intervals.

   function Coordinate_Difference (Left, Right : Length) return Interval;
   --  Enclose the subtraction Left minus Right without assuming the rounded machine result is exact.

   function To_Distance_Interval (Value : Interval) return Distance_Interval;
   --  Convert a valid dimensionless millimetre enclosure into a nonnegative physical-distance enclosure.

   function Unbounded_Distance return Distance_Interval;
   --  Return the conservative distance interval used when no useful finite upper bound can be proved.

   function Add_Upper (Left, Right : Length) return Length
   with Pre => Left >= 0.0 * mm and then Right >= 0.0 * mm;
   --  Add nonnegative upper bounds with upward rounding and saturation at Length'Last.

   function Subtract_Lower_Nonnegative (Left, Right : Length) return Length
   with Pre => Left >= 0.0 * mm and then Right >= 0.0 * mm;
   --  Subtract an error allowance from a lower bound, rounding downward and clamping the result to zero.

   function Round_Down_Nonnegative (Value : Length) return Length;
   --  Round a physical length downward while preserving the nonnegative distance domain.

   function Round_Up (Value : Length) return Length
   with Pre => Value >= 0.0 * mm;
   --  Round a nonnegative physical length upward, saturating if a finite successor cannot be represented.

   function Valid_Error (Value : Length) return Boolean;
   --  Return True when Value is a finite, nonnegative error radius suitable for widening a certificate.

   ---------------------------------------------------------------------------
   --  Geometric primitives and curve enclosures
   ---------------------------------------------------------------------------

   function Point_To_Segment_Distance (Point, Segment_Start, Segment_End : Position) return Distance_Interval;
   --  Certify the shortest distance from Point to the closed line segment, including a zero-length segment.

   function Corner_Distance (Point, Start, Corner, Finish : Position) return Distance_Interval;
   --  Certify the distance from Point to the union of the Start-to-Corner and Corner-to-Finish line segments.

   function Curvature_Norm_Upper (Curve : Stereographic_Curve) return Curvature;
   --  Combine the curve's per-axis derivative certificate into an upper bound on Euclidean curvature magnitude.

   function Curvature_Capsule_Radius
     (Curve                          : Stereographic_Curve;
      Start_Parameter, End_Parameter : Curve_Parameter;
      Curvature_Upper                : Curvature) return Length
   with
     Pre =>
       Start_Parameter <= End_Parameter
       and then Curvature_Upper >= 0.0 / mm
       and then Is_Finite (Dimensionless (Curvature_Upper / (1.0 / mm)));
   --  Bound the maximum departure of a unit-speed, curvature-limited curve segment from its endpoint chord.

   function Frame_Speed_Upper (Curve : Stereographic_Curve) return Dimensionless;
   --  Return the conservative normalized-parameter frame-speed bound computed during construction.

   function Maximum_Half_Parameter_Gap (Segments : Positive) return Dimensionless;
   --  Return the greatest normalized-parameter distance from any point to the nearest equal-partition sample.

   function Lipschitz_Half_Gap_Upper
     (Curve : Stereographic_Curve; Speed_Upper : Dimensionless; Segments : Positive) return Length
   with Pre => Speed_Upper >= 0.0 and then Is_Finite (Speed_Upper);
   --  Convert the maximum parameter sampling gap into a physical position-error allowance using the speed bound.

   function Make_Capsule
     (Curve                          : Stereographic_Curve;
      Kind                           : Metric_Kind;
      Point, Start, Corner, Finish   : Position;
      Start_Parameter, End_Parameter : Curve_Parameter;
      Start_Point, End_Point         : Position;
      Start_Error, End_Error         : Length;
      Curvature_Upper                : Curvature) return Capsule
   with
     Pre =>
       Start_Parameter <= End_Parameter
       and then Valid_Error (Start_Error)
       and then Valid_Error (End_Error)
       and then Curvature_Upper >= 0.0 / mm
       and then Is_Finite (Dimensionless (Curvature_Upper / (1.0 / mm)));
   --  Build a metric enclosure for one curve interval from its endpoint samples, certified sample errors, and
   --  curvature capsule radius.

   function Global_Interval (Kind : Metric_Kind; Capsules : Capsule_Array) return Distance_Interval;
   --  Reduce all capsule enclosures using the minimum or maximum semantics selected by Kind.

   function Widen_Capsule_For_Executed_Position
     (Kind : Metric_Kind; Value : Distance_Interval; Error : Length) return Distance_Interval
   with Pre => Valid_Error (Error);
   --  Widen an ideal-position metric enclosure so that it covers the positions produced by the realtime evaluator.

   function Bound
     (Curve                        : Stereographic_Curve;
      Kind                         : Metric_Kind;
      Point, Start, Corner, Finish : Position;
      Maximum_Interval_Width       : Length) return Distance_Interval
   with Pre => Maximum_Interval_Width > 0.0 * mm and then Maximum_Interval_Width <= Length'Last;
   --  Shared fixed-work driver for the public metrics. It selects Lipschitz sampling or curvature capsules according
   --  to the requested precision, then returns the tightest certified interval available within the hard segment
   --  limits.

end Prunt.Motion_Planner.Stereographic_Curves.Geometry;
