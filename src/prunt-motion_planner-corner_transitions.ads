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

with Ada.Numerics.Generic_Elementary_Functions;
with Prunt.Motion_Planner.Stereographic_Curves;

package Prunt.Motion_Planner.Corner_Transitions is
   type Corner_Transition_Kind is
     (Hard_Stop_Transition,
      Passthrough_Transition,
      Sharp_SCV_Transition,
      Stereographic_Transition,
      Circular_Transition,
      Parabolic_Transition,
      Biarc_Transition);

   type Junction_Policy_Kind is (Hard_Stop, Passthrough, Derivative_Bounded, Square_Corner_Velocity);

   type Continuity_Metadata is record
      Endpoint_Order        : Natural range 0 .. 4 := 0;
      Internal_Splice       : Boolean := False;
      Internal_Splice_Order : Natural range 0 .. 4 := 0;
   end record;
   --  Endpoint_Order is the highest promised derivative of position.  Internal_Splice is True only for a composite
   --  representation; Internal_Splice_Order then describes the join between its pieces.

   type Axis_Position_Envelope is record
      Lower : Length := 0.0 * mm;
      Upper : Length := 0.0 * mm;
   end record;
   type Position_Envelope is array (Axis_Name) of Axis_Position_Envelope;

   type Corner_Transition (Kind_Value : Corner_Transition_Kind := Hard_Stop_Transition) is private;
   type Corner_Transition_Evaluator (Kind_Value : Corner_Transition_Kind := Hard_Stop_Transition) is private;
   subtype Transition_Parameter is Dimensionless range 0.0 .. 1.0;

   type Construction_Status is
     (Construction_Success,
      Invalid_Input,
      Unsupported_Geometry,
      Numerically_Unsafe,
      Radius_Limit_Exceeded,
      Length_Limit_Exceeded,
      Stereographic_Construction_Failed);

   type Construction_Result is record
      Status     : Construction_Status := Invalid_Input;
      Transition : Corner_Transition;
   end record;

   function Stop_At (Point : Position) return Corner_Transition;
   function Passthrough_At (Point : Position) return Corner_Transition;
   function Sharp_At (Point : Position; Velocity_Limit : Velocity) return Corner_Transition;

   function Create_Stereographic (Request : Stereographic_Curves.Blend_Request) return Construction_Result;
   function From_Stereographic (Curve : Stereographic_Curves.Stereographic_Curve) return Corner_Transition;

   function Create_Circular
     (Start_Point, Commanded_Corner, Finish_Point : Position; Maximum_Radius : Length := 1.0E100 * mm)
      return Construction_Result;
   --  Construct the unique equal-trim circular fillet.  Unequal or degenerate trims fail closed.

   function Create_Parabolic
     (Start_Point, Commanded_Corner, Finish_Point : Position; Maximum_Length : Length := 1.0E100 * mm)
      return Construction_Result;
   --  Construct the quadratic Bezier with the commanded corner as its control point.

   function Create_Biarc
     (Start_Point, Finish_Point     : Position;
      Start_Tangent, Finish_Tangent : Position_Scale;
      Maximum_Length                : Length := 1.0E100 * mm;
      Preferred_Trim_Ratio          : Dimensionless := 1.0) return Construction_Result;
   --  Construct a positive, non-bulging two-arc member of the standard biarc family.  Unsupported or uncertifiable
   --  endpoint data is reported as failure; callers must retain the original path and insert a hard stop.

   type SCV_Status is (SCV_Success, SCV_Passthrough, SCV_Reversal_Stop, SCV_Mixed_Pure_E, SCV_Invalid_Input);
   type SCV_Result is record
      Status         : SCV_Status := SCV_Invalid_Input;
      Velocity_Limit : Velocity := 0.0 * mm / s;
   end record;

   function Compute_Sharp_SCV_Limit
     (Incoming_Tangent, Outgoing_Tangent : Position_Scale; Configured_SCV : Velocity; Ignore_E_In_XYZE : Boolean)
      return SCV_Result;

   function Transition_Kind (Transition : Corner_Transition) return Corner_Transition_Kind;
   function Transition_Kind (Evaluator : Corner_Transition_Evaluator) return Corner_Transition_Kind;
   function Policy (Transition : Corner_Transition) return Junction_Policy_Kind;
   function Policy (Evaluator : Corner_Transition_Evaluator) return Junction_Policy_Kind;
   function Continuity (Transition : Corner_Transition) return Continuity_Metadata;
   function Continuity (Evaluator : Corner_Transition_Evaluator) return Continuity_Metadata;
   function Arc_Length (Transition : Corner_Transition) return Length;
   function Arc_Length (Evaluator : Corner_Transition_Evaluator) return Length;
   function Split_Distance (Transition : Corner_Transition) return Length;
   function Split_Distance (Evaluator : Corner_Transition_Evaluator) return Length;
   function Junction_Velocity_Limit (Transition : Corner_Transition) return Velocity;
   function Junction_Velocity_Limit (Evaluator : Corner_Transition_Evaluator) return Velocity;

   function Point_At_Distance (Transition : Corner_Transition; Distance : Length) return Position
   with Pre => Distance >= 0.0 * mm and then Distance <= Arc_Length (Transition);
   function Point_At_Distance (Evaluator : Corner_Transition_Evaluator; Distance : Length) return Position
   with Pre => Distance >= 0.0 * mm and then Distance <= Arc_Length (Evaluator);
   function Point_At_Parameter (Transition : Corner_Transition; Parameter : Transition_Parameter) return Position;
   function Point_At_Parameter
     (Evaluator : Corner_Transition_Evaluator; Parameter : Transition_Parameter) return Position;

   function Derivative_Bounds (Transition : Corner_Transition) return Unit_Speed_Axial_Derivative_Bounds;
   function Derivative_Bounds
     (Transition : Corner_Transition; Start_Distance, End_Distance : Length) return Unit_Speed_Axial_Derivative_Bounds
   with
     Pre =>
       Start_Distance >= 0.0 * mm
       and then Start_Distance <= End_Distance
       and then End_Distance <= Arc_Length (Transition);
   function Derivative_Bounds (Evaluator : Corner_Transition_Evaluator) return Unit_Speed_Axial_Derivative_Bounds;
   function Derivative_Bounds
     (Evaluator : Corner_Transition_Evaluator; Start_Distance, End_Distance : Length)
      return Unit_Speed_Axial_Derivative_Bounds
   with
     Pre =>
       Start_Distance >= 0.0 * mm
       and then Start_Distance <= End_Distance
       and then End_Distance <= Arc_Length (Evaluator);

   function Position_Error_Bound (Transition : Corner_Transition) return Length;
   function Position_Error_Bound (Evaluator : Corner_Transition_Evaluator) return Length;
   function Certified_Position_Envelope (Transition : Corner_Transition) return Position_Envelope;
   function Certified_Position_Envelope (Evaluator : Corner_Transition_Evaluator) return Position_Envelope;
   function Certified_Position_Envelope
     (Transition : Corner_Transition; Start_Distance, End_Distance : Length) return Position_Envelope
   with
     Pre =>
       Start_Distance >= 0.0 * mm
       and then Start_Distance <= End_Distance
       and then End_Distance <= Arc_Length (Transition);
   function Certified_Position_Envelope
     (Evaluator : Corner_Transition_Evaluator; Start_Distance, End_Distance : Length) return Position_Envelope
   with
     Pre =>
       Start_Distance >= 0.0 * mm
       and then Start_Distance <= End_Distance
       and then End_Distance <= Arc_Length (Evaluator);
   function Axis_Is_Structurally_Constant (Transition : Corner_Transition; Axis : Axis_Name) return Boolean;
   function Axis_Is_Structurally_Constant (Evaluator : Corner_Transition_Evaluator; Axis : Axis_Name) return Boolean;

   function To_Evaluator (Transition : Corner_Transition) return Corner_Transition_Evaluator;

private
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   type Structural_Axes is array (Axis_Name) of Boolean;
   type Distance_Table is array (Natural range 0 .. 32) of Length;

   type Arc_Data is record
      Centre        : Position := [others => 0.0 * mm];
      Radial_Start  : Position_Scale := [X_Axis => 1.0, others => 0.0];
      Tangent_Start : Position_Scale := [Y_Axis => 1.0, others => 0.0];
      Radius        : Length := 0.0 * mm;
      Sweep         : Dimensionless := 0.0;
      Length_Value  : Length := 0.0 * mm;
   end record;

   type Parabolic_Data is record
      Start_Point, Control_Point, Finish_Point : Position := [others => 0.0 * mm];
      Table                                    : Distance_Table := [others => 0.0 * mm];
      Length_Value                             : Length := 0.0 * mm;
      Half_Distance                            : Length := 0.0 * mm;
      Length_Error                             : Length := 0.0 * mm;
      Maximum_Speed                            : Length := 0.0 * mm;
      Minimum_Speed                            : Length := 0.0 * mm;
   end record;

   type Biarc_Data is record
      First, Second : Arc_Data;
      Length_Value  : Length := 0.0 * mm;
   end record;

   type Corner_Transition (Kind_Value : Corner_Transition_Kind := Hard_Stop_Transition) is record
      Bounds        : Unit_Speed_Axial_Derivative_Bounds := (others => <>);
      Envelope      : Position_Envelope := [others => <>];
      Error         : Length := 0.0 * mm;
      Constant_Axes : Structural_Axes := [others => True];
      SCV_Limit     : Velocity := 0.0 * mm / s;
      case Kind_Value is
         when Stereographic_Transition =>
            Stereo : Stereographic_Curves.Stereographic_Curve;

         when Circular_Transition =>
            Circle : Arc_Data;

         when Parabolic_Transition =>
            Parabola : Parabolic_Data;

         when Biarc_Transition =>
            Two_Arcs : Biarc_Data;

         when Hard_Stop_Transition | Passthrough_Transition | Sharp_SCV_Transition =>
            Point : Position := [others => 0.0 * mm];
      end case;
   end record;

   type Corner_Transition_Evaluator (Kind_Value : Corner_Transition_Kind := Hard_Stop_Transition) is record
      Bounds        : Unit_Speed_Axial_Derivative_Bounds := (others => <>);
      Envelope      : Position_Envelope := [others => <>];
      Error         : Length := 0.0 * mm;
      Constant_Axes : Structural_Axes := [others => True];
      SCV_Limit     : Velocity := 0.0 * mm / s;
      case Kind_Value is
         when Stereographic_Transition =>
            Stereo : Stereographic_Curves.Stereographic_Curve_Evaluator;

         when Circular_Transition =>
            Circle : Arc_Data;

         when Parabolic_Transition =>
            Parabola : Parabolic_Data;

         when Biarc_Transition =>
            Two_Arcs : Biarc_Data;

         when Hard_Stop_Transition | Passthrough_Transition | Sharp_SCV_Transition =>
            Point : Position := [others => 0.0 * mm];
      end case;
   end record;

   function Finite (X : Dimensionless) return Boolean;
   --  Return True exactly when X is neither an infinity nor a NaN.

   function Finite_Length (X : Length) return Boolean;
   --  Return True exactly when X is neither an infinity nor a NaN.

   function Dot (A, B : Position_Scale) return Dimensionless;
   --  Return the Euclidean dot product over all XYZE axes.

   function Norm (A : Position_Scale) return Dimensionless;
   --  Return the Euclidean norm of A, or Dimensionless'Last if evaluating the norm overflows.

   function Unit (A : Position_Scale; Good : out Boolean) return Position_Scale;
   --  Normalize A.  Good is False, and the returned vector is zero, when A is too small or non-finite.

   function Unit_Offset (A : Position_Offset; Good : out Boolean) return Position_Scale;
   --  Normalize a physical position offset.  Good is False, and the returned vector is zero, when A is too small or
   --  non-finite.

   function Arc_Constant_Axes (Arc : Arc_Data) return Structural_Axes;
   --  Identify axes whose radial and tangent coefficients prove that Arc is structurally constant.

   function Parabolic_Constant_Axes (P : Parabolic_Data) return Structural_Axes;
   --  Identify axes on which all three quadratic Bezier control points are equal.

   function Biarc_Constant_Axes (Data : Biarc_Data) return Structural_Axes;
   --  Identify axes proven constant across both subarcs and their splice.

   function Point_Envelope (P : Position) return Position_Envelope;
   --  Return the degenerate per-axis envelope containing only P.

   function Union (A, B : Position_Envelope) return Position_Envelope;
   --  Return the smallest per-axis envelope containing both input envelopes.

   function Arc_Envelope (Arc : Arc_Data) return Position_Envelope;
   --  Return the analytic per-axis position envelope of the complete arc.

   function Arc_Envelope (Arc : Arc_Data; Start_Distance, End_Distance : Length) return Position_Envelope;
   --  Return the analytic per-axis position envelope of the closed arc-distance interval.

   function Arc_Bounds (Arc : Arc_Data) return Unit_Speed_Axial_Derivative_Bounds;
   --  Return outward-rounded per-axis derivative bounds for the complete unit-speed arc.

   function Arc_Bounds
     (Arc : Arc_Data; Start_Distance, End_Distance : Length) return Unit_Speed_Axial_Derivative_Bounds;
   --  Return outward-rounded per-axis derivative bounds for the closed arc-distance interval.

   function Merge_Bounds (A, B : Unit_Speed_Axial_Derivative_Bounds) return Unit_Speed_Axial_Derivative_Bounds;
   --  Return the component-wise maximum of two nonnegative derivative-bound records.

   function Arc_Point (Arc : Arc_Data; Distance : Length) return Position;
   --  Evaluate Arc at physical arc distance Distance from its start.

   procedure Arc_From_Start
     (Start_Point, Finish_Point : Position; Start_Tangent : Position_Scale; Arc : out Arc_Data; Good : out Boolean);
   --  Construct the minor arc from Start_Point to Finish_Point with the requested initial tangent.  Good is False for
   --  degenerate, backward, greater-than-semicircular, or numerically unsafe geometry.

   function Bezier_Point (P : Parabolic_Data; T : Dimensionless) return Position;
   --  Evaluate the quadratic Bezier at its native parameter T.

   function Bezier_Speed (P : Parabolic_Data; T : Dimensionless) return Length;
   --  Return the norm of the derivative with respect to the native Bezier parameter.

   function Bezier_Cell_Length (P : Parabolic_Data; T0, T1 : Dimensionless) return Length;
   --  Approximate the arc length on a table cell interval using the positive Simpson rule used by table inversion.

   function Parabolic_Parameter (P : Parabolic_Data; Distance : Length) return Dimensionless;
   --  Invert P's monotone arc-length table with a certified bracket and one safeguarded Newton correction.

   function Parabolic_Point (P : Parabolic_Data; Distance : Length) return Position;
   --  Evaluate P at physical arc distance Distance from its start.

   function Parabolic_Envelope (P : Parabolic_Data; Start_Distance, End_Distance : Length) return Position_Envelope;
   --  Bound a closed arc-distance interval using the convex hull of its de Casteljau restricted control polygon.

   function Parabolic_Bounds
     (P : Parabolic_Data; Start_Distance, End_Distance : Length) return Unit_Speed_Axial_Derivative_Bounds;
   --  Return interval-safe chain-rule derivative bounds for a closed arc-distance interval of P.

   function Reverse_Arc (Arc : Arc_Data) return Arc_Data;
   --  Return the same geometric arc with the opposite physical-distance orientation.

   function Arc_Has_Nonnegative_Progress (Arc : Arc_Data; Direction : Position_Scale) return Boolean;
   --  Prove that the arc tangent never has negative projection onto Direction, including at interior extrema.

   function Biarc_Envelope (Data : Biarc_Data; Start_Distance, End_Distance : Length) return Position_Envelope;
   --  Return the analytic envelope of a closed biarc-distance interval, splitting and unioning at the splice when
   --  needed.

   function Biarc_Bounds
     (Data : Biarc_Data; Start_Distance, End_Distance : Length) return Unit_Speed_Axial_Derivative_Bounds;
   --  Return derivative bounds for a closed biarc-distance interval, merging both subarcs when it crosses the splice.

   function Policy_For (Kind : Corner_Transition_Kind) return Junction_Policy_Kind;
   --  Map each stored transition representation to the junction policy enforced by the motion planner.

   function Continuity_For (Kind : Corner_Transition_Kind) return Continuity_Metadata;
   --  Return the endpoint and internal-splice continuity promised by a transition representation.

   function Range_Envelope
     (Start_Point, End_Point : Position; Span, Error : Length; Constant_Axes : Structural_Axes)
      return Position_Envelope;
   --  Conservatively enclose a curve interval from its endpoints and maximum travel Span, then add Error.  Invalid or
   --  overflowing inputs produce the full numeric range so callers fail closed.

   function Stereographic_Envelope
     (Curve : Stereographic_Curves.Stereographic_Curve; Start_Distance, End_Distance : Length)
      return Position_Envelope;
   --  Enclose a Stereographic interval with fixed-size Lipschitz cells. Each cell uses the cached certified
   --  whole-curve velocity bounds and both evaluated endpoints, avoiding both the excessive whole-arc ball used as a
   --  storage fallback and expensive per-cell interval-majorant recomputation.

   function Pad_Envelope
     (Envelope : Position_Envelope; Padding : Length; Constant_Axes : Structural_Axes) return Position_Envelope;
   --  Expand nonconstant axes outward by Padding with saturation.  Invalid inputs produce the full numeric range.

   function Phase_In_Range (Phase, Low, High : Dimensionless) return Boolean;
   --  Return True when an equivalent of Phase modulo 2*pi lies in the closed interval Low .. High.

   function Trig_Absolute_Maximum (Cos_Coefficient, Sin_Coefficient, Low, High : Dimensionless) return Dimensionless;
   --  Return the exact maximum absolute value of A*cos (theta) + B*sin (theta) on Low .. High.
end Prunt.Motion_Planner.Corner_Transitions;
