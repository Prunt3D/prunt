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

with Ada.Numerics;
with Ada.Numerics.Long_Elementary_Functions;
with Prunt.Motion_Planner.Stereographic_Curves;
with Trendy_Test; use Trendy_Test;

package body Prunt.Motion_Planner.Corner_Transitions.Test is

   pragma Extensions_Allowed (On);

   Dense_Samples : constant Positive := 1_024;
   Derivative_Samples : constant Positive := 512;
   Geometry_Tolerance : constant Length := 2.0E-5 * mm;
   Numeric_Tolerance : constant Long_Float := 2.0E-10;

   Origin : constant Position := [others => 0.0 * mm];
   R4_Origin : constant Position :=
     [X_Axis => 2.0 * mm, Y_Axis => -3.0 * mm, Z_Axis => 5.0 * mm, E_Axis => 7.0 * mm];
   R4_First : constant Position_Scale := [others => 0.5];
   R4_Second : constant Position_Scale :=
     [X_Axis => 0.5, Y_Axis => -0.5, Z_Axis => 0.5, E_Axis => -0.5];
   X_Unit : constant Position_Scale := [X_Axis => 1.0, others => 0.0];
   Y_Unit : constant Position_Scale := [Y_Axis => 1.0, others => 0.0];
   E_Unit : constant Position_Scale := [E_Axis => 1.0, others => 0.0];

   subtype Series_Index is Natural range 0 .. 5;
   type Series is array (Series_Index) of Long_Float;
   Factorial : constant array (Series_Index) of Long_Float := [1.0, 1.0, 2.0, 6.0, 24.0, 120.0];

   function Add (Left, Right : Series) return Series;
   function Bezier_Position (Start_Point, Control_Point, Finish_Point : Position; T : Dimensionless) return Position;
   function Distance (Left, Right : Position) return Length;
   function Make_Stereographic_Request return Stereographic_Curves.Blend_Request;
   function Multiply (Left, Right : Series) return Series;
   function Parameter_Series (Data : Parabolic_Data; T : Dimensionless) return Series;
   function Reciprocal (Value : Series) return Series;
   function Scale (Value : Series; Factor : Long_Float) return Series;
   function Square_Root (Value : Series) return Series;

   procedure Assert_Arc_Derivatives_Covered
     (Transition : Corner_Transition;
      Arc        : Arc_Data;
      Name       : String;
      T          : in out Trendy_Test.Operation'Class);

   procedure Assert_Construction_Succeeded
     (Result : Construction_Result;
      Name   : String;
      T      : in out Trendy_Test.Operation'Class);

   procedure Assert_Envelope_Covers_Dense_Samples
     (Transition : Corner_Transition;
      Name       : String;
      T          : in out Trendy_Test.Operation'Class);

   procedure Assert_Evaluator_Matches
     (Transition : Corner_Transition;
      Name       : String;
      T          : in out Trendy_Test.Operation'Class);

   procedure Assert_Parabolic_Derivatives_Covered
     (Transition : Corner_Transition;
      Name       : String;
      T          : in out Trendy_Test.Operation'Class);

   procedure Assert_Point_Close
     (Actual, Expected : Position;
      Tolerance        : Length;
      Name             : String;
      T                : in out Trendy_Test.Operation'Class);

   function Add (Left, Right : Series) return Series is
      Result : Series;
   begin
      for I in Series_Index loop
         Result (I) := Left (I) + Right (I);
      end loop;
      return Result;
   end Add;

   function Bezier_Position
     (Start_Point, Control_Point, Finish_Point : Position;
      T                                         : Dimensionless) return Position is
      Result : Position;
   begin
      for Axis in Axis_Name loop
         Result (Axis) :=
           (1.0 - T) ** 2 * Start_Point (Axis)
           + 2.0 * (1.0 - T) * T * Control_Point (Axis)
           + T ** 2 * Finish_Point (Axis);
      end loop;
      return Result;
   end Bezier_Position;

   function Distance (Left, Right : Position) return Length is
   begin
      return abs (Left - Right);
   end Distance;

   function Make_Stereographic_Request return Stereographic_Curves.Blend_Request is
      Result : Stereographic_Curves.Blend_Request;
   begin
      Result.Start.Point := Origin;
      Result.Start.Jet.Tangent := X_Unit;
      Result.Finish.Point :=
        [X_Axis => 10.0 * mm, Y_Axis => 10.0 * mm, Z_Axis => 0.0 * mm, E_Axis => 0.0 * mm];
      Result.Finish.Jet.Tangent := Y_Unit;
      Result.Maximum_Position_Error := 1.0E-3 * mm;
      Result.Maximum_Arc_Length := 40.0 * mm;
      return Result;
   end Make_Stereographic_Request;

   function Multiply (Left, Right : Series) return Series is
      Result : Series := [others => 0.0];
   begin
      for I in Series_Index loop
         for J in 0 .. Series_Index'Last - I loop
            Result (I + J) := Result (I + J) + Left (I) * Right (J);
         end loop;
      end loop;
      return Result;
   end Multiply;

   function Parameter_Series (Data : Parabolic_Data; T : Dimensionless) return Series is
      Result : Series := [0 => Long_Float (T), others => 0.0];
   begin
      for Order in 0 .. Series_Index'Last - 1 loop
         declare
            Speed_Squared : Series := [others => 0.0];
         begin
            for Axis in Axis_Name loop
               declare
                  Linear : constant Long_Float :=
                    2.0 * Long_Float ((Data.Control_Point (Axis) - Data.Start_Point (Axis)) / mm);
                  Quadratic : constant Long_Float :=
                    Long_Float
                      ((Data.Finish_Point (Axis) - 2.0 * Data.Control_Point (Axis)
                        + Data.Start_Point (Axis))
                       / mm);
                  Tangent : Series := Scale (Result, 2.0 * Quadratic);
               begin
                  Tangent (0) := Tangent (0) + Linear;
                  Speed_Squared := Add (Speed_Squared, Multiply (Tangent, Tangent));
               end;
            end loop;
            Result (Order + 1) := Reciprocal (Square_Root (Speed_Squared)) (Order) / Long_Float (Order + 1);
         end;
      end loop;
      return Result;
   end Parameter_Series;

   function Reciprocal (Value : Series) return Series is
      Result : Series := [others => 0.0];
   begin
      Result (0) := 1.0 / Value (0);
      for I in 1 .. Series_Index'Last loop
         for J in 1 .. I loop
            Result (I) := Result (I) - Value (J) * Result (I - J) / Value (0);
         end loop;
      end loop;
      return Result;
   end Reciprocal;

   function Scale (Value : Series; Factor : Long_Float) return Series is
      Result : Series;
   begin
      for I in Series_Index loop
         Result (I) := Value (I) * Factor;
      end loop;
      return Result;
   end Scale;

   function Square_Root (Value : Series) return Series is
      Result : Series := [others => 0.0];
   begin
      Result (0) := Ada.Numerics.Long_Elementary_Functions.Sqrt (Value (0));
      for I in 1 .. Series_Index'Last loop
         Result (I) := Value (I);
         for J in 1 .. I - 1 loop
            Result (I) := Result (I) - Result (J) * Result (I - J);
         end loop;
         Result (I) := Result (I) / (2.0 * Result (0));
      end loop;
      return Result;
   end Square_Root;

   procedure Assert_Arc_Derivatives_Covered
     (Transition : Corner_Transition;
      Arc        : Arc_Data;
      Name       : String;
      T          : in out Trendy_Test.Operation'Class) is
      Bounds : constant Unit_Speed_Axial_Derivative_Bounds := Derivative_Bounds (Transition);
      In_Bounds : Boolean := True;
      Worst_Excess : Long_Float := 0.0;
      K : constant Long_Float := Long_Float (mm / Arc.Radius);
   begin
      for Sample in 0 .. Derivative_Samples loop
         declare
            Theta : constant Long_Float :=
              Long_Float (Arc.Sweep) * Long_Float (Sample) / Long_Float (Derivative_Samples);
            C : constant Long_Float := Ada.Numerics.Long_Elementary_Functions.Cos (Theta);
            Sine : constant Long_Float := Ada.Numerics.Long_Elementary_Functions.Sin (Theta);
         begin
            for Axis in Axis_Name loop
               declare
                  U : constant Long_Float := Long_Float (Arc.Radial_Start (Axis));
                  V : constant Long_Float := Long_Float (Arc.Tangent_Start (Axis));
                  Values : constant array (Positive range 1 .. 5) of Long_Float :=
                    [abs (-U * Sine + V * C),
                     abs ((-U * C - V * Sine) * K),
                     abs ((U * Sine - V * C) * K ** 2),
                     abs ((U * C + V * Sine) * K ** 3),
                     abs ((-U * Sine + V * C) * K ** 4)];
                  Limits : constant array (Positive range 1 .. 5) of Long_Float :=
                    [Long_Float (Bounds.Velocity (Axis)),
                     Long_Float (Bounds.Acceleration (Axis) * mm),
                     Long_Float (Bounds.Jerk (Axis) * mm ** 2),
                     Long_Float (Bounds.Snap (Axis) * mm ** 3),
                     Long_Float (Bounds.Crackle (Axis) * mm ** 4)];
               begin
                  for Order in Values'Range loop
                     Worst_Excess := Long_Float'Max (Worst_Excess, Values (Order) - Limits (Order));
                     In_Bounds :=
                       In_Bounds
                       and then Values (Order) <= Limits (Order) * (1.0 + Numeric_Tolerance) + 1.0E-12;
                  end loop;
               end;
            end loop;
         end;
      end loop;
      T.Assert
        (In_Bounds,
         Name & " analytic derivatives exceed the public bound by" & Worst_Excess'Image);
   end Assert_Arc_Derivatives_Covered;

   procedure Assert_Construction_Succeeded
     (Result : Construction_Result;
      Name   : String;
      T      : in out Trendy_Test.Operation'Class) is
   begin
      T.Assert
        (Result.Status = Construction_Success,
         Name & " construction status is " & Result.Status'Image);
   end Assert_Construction_Succeeded;

   procedure Assert_Envelope_Covers_Dense_Samples
     (Transition : Corner_Transition;
      Name       : String;
      T          : in out Trendy_Test.Operation'Class) is
      Whole : constant Position_Envelope := Certified_Position_Envelope (Transition);
      Evaluator : constant Corner_Transition_Evaluator := To_Evaluator (Transition);
      Evaluator_Whole : constant Position_Envelope := Certified_Position_Envelope (Evaluator);
      Total : constant Length := Arc_Length (Transition);
      Tolerance : constant Length := Position_Error_Bound (Transition) + 1.0E-9 * mm;
      Whole_Covers : Boolean := True;
      Ranges_Cover : Boolean := True;
   begin
      for Sample in 0 .. Dense_Samples loop
         declare
            D : constant Length := Total * Dimensionless (Sample) / Dimensionless (Dense_Samples);
            Point : constant Position := Point_At_Distance (Transition, D);
         begin
            for Axis in Axis_Name loop
               Whole_Covers :=
                 Whole_Covers
                 and then Point (Axis) >= Whole (Axis).Lower - Tolerance
                 and then Point (Axis) <= Whole (Axis).Upper + Tolerance
                 and then Point (Axis) >= Evaluator_Whole (Axis).Lower - Tolerance
                 and then Point (Axis) <= Evaluator_Whole (Axis).Upper + Tolerance;
            end loop;
         end;
      end loop;

      for Window in 0 .. 7 loop
         declare
            Start_Distance : constant Length := Total * Dimensionless (Window) / 8.0;
            End_Distance : constant Length := Total * Dimensionless (Window + 1) / 8.0;
            Envelope : constant Position_Envelope :=
              Certified_Position_Envelope (Transition, Start_Distance, End_Distance);
            Evaluator_Envelope : constant Position_Envelope :=
              Certified_Position_Envelope (Evaluator, Start_Distance, End_Distance);
         begin
            for Sample in 0 .. 64 loop
               declare
                  D : constant Length :=
                    Start_Distance + (End_Distance - Start_Distance) * Dimensionless (Sample) / 64.0;
                  Point : constant Position := Point_At_Distance (Transition, D);
               begin
                  for Axis in Axis_Name loop
                     Ranges_Cover :=
                       Ranges_Cover
                       and then Point (Axis) >= Envelope (Axis).Lower - Tolerance
                       and then Point (Axis) <= Envelope (Axis).Upper + Tolerance
                       and then Point (Axis) >= Evaluator_Envelope (Axis).Lower - Tolerance
                       and then Point (Axis) <= Evaluator_Envelope (Axis).Upper + Tolerance;
                  end loop;
               end;
            end loop;
         end;
      end loop;
      T.Assert (Whole_Covers, Name & " whole-curve envelope covers dense samples");
      T.Assert (Ranges_Cover, Name & " ranged envelopes cover dense samples");
   end Assert_Envelope_Covers_Dense_Samples;

   procedure Assert_Evaluator_Matches
     (Transition : Corner_Transition;
      Name       : String;
      T          : in out Trendy_Test.Operation'Class) is
      Evaluator : constant Corner_Transition_Evaluator := To_Evaluator (Transition);
      Total : constant Length := Arc_Length (Transition);
      Matches : Boolean := True;
      Metadata : constant Continuity_Metadata := Continuity (Transition);
      Evaluator_Metadata : constant Continuity_Metadata := Continuity (Evaluator);
   begin
      Matches :=
        Transition_Kind (Evaluator) = Transition_Kind (Transition)
        and then Policy (Evaluator) = Policy (Transition)
        and then Evaluator_Metadata = Metadata
        and then Arc_Length (Evaluator) = Total
        and then Split_Distance (Evaluator) = Split_Distance (Transition)
        and then Junction_Velocity_Limit (Evaluator) = Junction_Velocity_Limit (Transition)
        and then Derivative_Bounds (Evaluator) = Derivative_Bounds (Transition)
        and then Position_Error_Bound (Evaluator) = Position_Error_Bound (Transition)
        and then Certified_Position_Envelope (Evaluator) = Certified_Position_Envelope (Transition);
      for Axis in Axis_Name loop
         Matches :=
           Matches
           and then Axis_Is_Structurally_Constant (Evaluator, Axis)
                    = Axis_Is_Structurally_Constant (Transition, Axis);
      end loop;
      for Sample in 0 .. 64 loop
         declare
            D : constant Length := Total * Dimensionless (Sample) / 64.0;
         begin
            Matches :=
              Matches
              and then Distance (Point_At_Distance (Evaluator, D), Point_At_Distance (Transition, D))
                       <= Geometry_Tolerance;
         end;
      end loop;
      T.Assert (Matches, Name & " evaluator preserves all transition metadata and evaluations");
   end Assert_Evaluator_Matches;

   procedure Assert_Parabolic_Derivatives_Covered
     (Transition : Corner_Transition;
      Name       : String;
      T          : in out Trendy_Test.Operation'Class) is
      Data : constant Parabolic_Data := Transition.Parabola;
      Bounds : constant Unit_Speed_Axial_Derivative_Bounds := Derivative_Bounds (Transition);
      In_Bounds : Boolean := True;
      Worst_Excess : Long_Float := 0.0;
   begin
      for Sample in 0 .. Derivative_Samples loop
         declare
            Parameter : constant Dimensionless := Dimensionless (Sample) / Dimensionless (Derivative_Samples);
            Parameter_Jet : constant Series := Parameter_Series (Data, Parameter);
            Parameter_Squared : constant Series := Multiply (Parameter_Jet, Parameter_Jet);
         begin
            for Axis in Axis_Name loop
               declare
                  Start_Value : constant Long_Float := Long_Float (Data.Start_Point (Axis) / mm);
                  Linear : constant Long_Float :=
                    2.0 * Long_Float ((Data.Control_Point (Axis) - Data.Start_Point (Axis)) / mm);
                  Quadratic : constant Long_Float :=
                    Long_Float
                      ((Data.Finish_Point (Axis) - 2.0 * Data.Control_Point (Axis)
                        + Data.Start_Point (Axis))
                       / mm);
                  Position_Jet : Series :=
                    Add (Scale (Parameter_Jet, Linear), Scale (Parameter_Squared, Quadratic));
                  Limits : constant array (Positive range 1 .. 5) of Long_Float :=
                    [Long_Float (Bounds.Velocity (Axis)),
                     Long_Float (Bounds.Acceleration (Axis) * mm),
                     Long_Float (Bounds.Jerk (Axis) * mm ** 2),
                     Long_Float (Bounds.Snap (Axis) * mm ** 3),
                     Long_Float (Bounds.Crackle (Axis) * mm ** 4)];
               begin
                  Position_Jet (0) := Position_Jet (0) + Start_Value;
                  for Order in Limits'Range loop
                     declare
                        Value : constant Long_Float := abs (Position_Jet (Order) * Factorial (Order));
                     begin
                        Worst_Excess := Long_Float'Max (Worst_Excess, Value - Limits (Order));
                        In_Bounds :=
                          In_Bounds
                          and then Value <= Limits (Order) * (1.0 + Numeric_Tolerance) + 2.0E-10;
                     end;
                  end loop;
               end;
            end loop;
         end;
      end loop;
      T.Assert
        (In_Bounds,
         Name & " exact series derivatives exceed the public bound by" & Worst_Excess'Image);
   end Assert_Parabolic_Derivatives_Covered;

   procedure Assert_Point_Close
     (Actual, Expected : Position;
      Tolerance        : Length;
      Name             : String;
      T                : in out Trendy_Test.Operation'Class) is
      Error : constant Length := Distance (Actual, Expected);
   begin
      T.Assert
        (Error <= Tolerance,
         Name & " point error is " & Error'Image & ", tolerance is " & Tolerance'Image);
   end Assert_Point_Close;

   procedure Test_Biarc_Determinism_And_Reversal (T : in out Trendy_Test.Operation'Class) is
      Finish_Point : constant Position := R4_Origin + R4_First * (10.0 * mm) + R4_Second * (10.0 * mm);
      Forward : constant Construction_Result :=
        Create_Biarc
          (R4_Origin, Finish_Point, R4_First, R4_Second,
           Maximum_Length => 100.0 * mm, Preferred_Trim_Ratio => 1.0);
      Repeated : constant Construction_Result :=
        Create_Biarc
          (R4_Origin, Finish_Point, R4_First, R4_Second,
           Maximum_Length => 100.0 * mm, Preferred_Trim_Ratio => 1.0);
      Reversed_Result : constant Construction_Result :=
        Create_Biarc
          (Finish_Point, R4_Origin, R4_Second * (-1.0), R4_First * (-1.0),
           Maximum_Length => 100.0 * mm, Preferred_Trim_Ratio => 1.0);
   begin
      T.Register;
      Assert_Construction_Succeeded (Forward, "Forward R4 biarc", T);
      Assert_Construction_Succeeded (Repeated, "Repeated R4 biarc", T);
      Assert_Construction_Succeeded (Reversed_Result, "Reversed R4 biarc", T);
      if Forward.Status /= Construction_Success
        or else Repeated.Status /= Construction_Success
        or else Reversed_Result.Status /= Construction_Success
      then
         return;
      end if;

      T.Assert
        (Split_Distance (Forward.Transition) = Forward.Transition.Two_Arcs.First.Length_Value
         and then Split_Distance (Forward.Transition) > 0.0 * mm
         and then Split_Distance (Forward.Transition) < Arc_Length (Forward.Transition),
         "Biarc split is the positive first-subarc length");
      Assert_Point_Close
        (Point_At_Distance (Forward.Transition, 0.0 * mm), R4_Origin, Geometry_Tolerance,
         "Biarc start endpoint", T);
      Assert_Point_Close
        (Point_At_Distance (Forward.Transition, Arc_Length (Forward.Transition)), Finish_Point,
         Geometry_Tolerance, "Biarc finish endpoint", T);

      T.Assert
        (Arc_Length (Forward.Transition) = Arc_Length (Repeated.Transition)
         and then Split_Distance (Forward.Transition) = Split_Distance (Repeated.Transition),
         "Repeated biarc selection is bitwise deterministic for lengths");
      for Sample in 0 .. 128 loop
         declare
            Parameter : constant Transition_Parameter := Transition_Parameter (Dimensionless (Sample) / 128.0);
         begin
            Assert_Point_Close
              (Point_At_Parameter (Forward.Transition, Parameter),
               Point_At_Parameter (Repeated.Transition, Parameter), 0.0 * mm,
               "Repeated biarc selection is bitwise deterministic", T);
            Assert_Point_Close
              (Point_At_Parameter (Forward.Transition, Parameter),
               Point_At_Parameter (Reversed_Result.Transition, 1.0 - Parameter), Geometry_Tolerance,
               "Biarc reversal covariance", T);
         end;
      end loop;

      declare
         Split : constant Length := Split_Distance (Forward.Transition);
         Step_Distance : constant Length := 1.0E-4 * mm;
         Incoming : constant Position_Scale :=
           (Point_At_Distance (Forward.Transition, Split)
            - Point_At_Distance (Forward.Transition, Split - Step_Distance))
           / Step_Distance;
         Outgoing : constant Position_Scale :=
           (Point_At_Distance (Forward.Transition, Split + Step_Distance)
            - Point_At_Distance (Forward.Transition, Split))
           / Step_Distance;
      begin
         T.Assert (abs (Incoming - Outgoing) <= 2.0E-5, "Biarc splice is tangent-continuous");
      end;

      Assert_Envelope_Covers_Dense_Samples (Forward.Transition, "R4 biarc", T);
      Assert_Arc_Derivatives_Covered (Forward.Transition, Forward.Transition.Two_Arcs.First, "First biarc arc", T);
      Assert_Arc_Derivatives_Covered (Forward.Transition, Forward.Transition.Two_Arcs.Second, "Second biarc arc", T);
      Assert_Evaluator_Matches (Forward.Transition, "R4 biarc", T);
   end Test_Biarc_Determinism_And_Reversal;

   procedure Test_Biarc_Trim_Ratio_Search (T : in out Trendy_Test.Operation'Class) is
      Finish_Point : constant Position :=
        R4_Origin + R4_First * (18.0 * mm) + R4_Second * (7.0 * mm);
      Ratios : constant array (Positive range 1 .. 3) of Dimensionless := [1.0 / 5.0, 1.0, 5.0];
   begin
      T.Register;
      for Ratio of Ratios loop
         declare
            Result : constant Construction_Result :=
              Create_Biarc
                (R4_Origin, Finish_Point, R4_First, R4_Second,
                 Maximum_Length => 200.0 * mm, Preferred_Trim_Ratio => Ratio);
         begin
            Assert_Construction_Succeeded (Result, "Biarc trim-ratio search " & Ratio'Image, T);
            if Result.Status = Construction_Success then
               Assert_Point_Close
                 (Point_At_Distance (Result.Transition, 0.0 * mm), R4_Origin, Geometry_Tolerance,
                  "Biarc trim-ratio start", T);
               Assert_Point_Close
                 (Point_At_Distance (Result.Transition, Arc_Length (Result.Transition)), Finish_Point,
                  Geometry_Tolerance, "Biarc trim-ratio finish", T);
               Assert_Envelope_Covers_Dense_Samples (Result.Transition, "Biarc trim-ratio", T);
            end if;
         end;
      end loop;
   end Test_Biarc_Trim_Ratio_Search;

   procedure Test_Circular_R4_Geometry_And_Bounds (T : in out Trendy_Test.Operation'Class) is
      Corner : constant Position := R4_Origin;
      Start_Point : constant Position := Corner - R4_First * (8.0 * mm);
      Finish_Point : constant Position := Corner + R4_Second * (8.0 * mm);
      Result : constant Construction_Result :=
        Create_Circular (Start_Point, Corner, Finish_Point, Maximum_Radius => 8.0 * mm);
   begin
      T.Register;
      Assert_Construction_Succeeded (Result, "R4 circular", T);
      if Result.Status /= Construction_Success then
         return;
      end if;

      T.Assert
        (abs (Arc_Length (Result.Transition) - 4.0 * Ada.Numerics.Pi * mm) <= 1.0E-10 * mm,
         "Circular arc has the exact physical quarter-circle length");
      T.Assert
        (Split_Distance (Result.Transition) = Arc_Length (Result.Transition) / 2.0,
         "Circular split is half arc length");
      Assert_Point_Close
        (Point_At_Distance (Result.Transition, 0.0 * mm), Start_Point, Geometry_Tolerance,
         "Circular start endpoint", T);
      Assert_Point_Close
        (Point_At_Distance (Result.Transition, Arc_Length (Result.Transition)), Finish_Point,
         Geometry_Tolerance, "Circular finish endpoint", T);
      Assert_Envelope_Covers_Dense_Samples (Result.Transition, "R4 circular", T);
      Assert_Arc_Derivatives_Covered (Result.Transition, Result.Transition.Circle, "R4 circular", T);
      Assert_Evaluator_Matches (Result.Transition, "R4 circular", T);
   end Test_Circular_R4_Geometry_And_Bounds;

   procedure Test_Fixed_Size_Copying (T : in out Trendy_Test.Operation'Class) is
      type Transition_Buffer is array (Positive range 1 .. 12) of Corner_Transition;
      Corner : constant Position := R4_Origin;
      Circular_Result : constant Construction_Result :=
        Create_Circular
          (Corner - R4_First * (4.0 * mm), Corner, Corner + R4_Second * (4.0 * mm),
           Maximum_Radius => 4.0 * mm);
      Stereographic_Result : constant Construction_Result :=
        Create_Stereographic (Make_Stereographic_Request);
      Seeds : Transition_Buffer := [others => Stop_At (Origin)];
      Copies : Transition_Buffer := [others => Stop_At (Origin)];
   begin
      T.Register;
      Assert_Construction_Succeeded (Circular_Result, "Copy-test circular", T);
      Assert_Construction_Succeeded (Stereographic_Result, "Copy-test stereographic", T);
      if Circular_Result.Status /= Construction_Success
        or else Stereographic_Result.Status /= Construction_Success
      then
         return;
      end if;
      Seeds (1) := Stop_At (R4_Origin);
      Seeds (2) := Passthrough_At (R4_Origin);
      Seeds (3) := Sharp_At (R4_Origin, 5.0 * mm / s);
      Seeds (4) := Circular_Result.Transition;
      Seeds (5) := Stereographic_Result.Transition;
      for I in 6 .. Seeds'Last loop
         Seeds (I) := Seeds (Seeds'First + (I - Seeds'First) mod 5);
      end loop;
      for Iteration in 1 .. 4_096 loop
         declare
            Destination : constant Positive := Copies'First + (Iteration - 1) mod Copies'Length;
            Source : constant Positive := Seeds'First + (Iteration - 1) mod Seeds'Length;
         begin
            Copies (Destination) := Seeds (Source);
         end;
      end loop;
      Copies := Seeds;
      T.Assert
        (Transition_Buffer'Component_Size = Corner_Transition'Size,
         "Transition arrays store each discriminated value inline at fixed component size");
      for I in Seeds'Range loop
         T.Assert
           (Transition_Kind (Copies (I)) = Transition_Kind (Seeds (I))
            and then Arc_Length (Copies (I)) = Arc_Length (Seeds (I))
            and then Point_At_Distance (Copies (I), 0.0 * mm) = Point_At_Distance (Seeds (I), 0.0 * mm),
            "Fixed-size transition assignment preserves variant " & I'Image);
      end loop;
   end Test_Fixed_Size_Copying;

   procedure Test_Parabolic_R4_Geometry_And_Bounds (T : in out Trendy_Test.Operation'Class) is
      Corner : constant Position := R4_Origin;
      Start_Point : constant Position := Corner - R4_First * (12.0 * mm);
      Finish_Point : constant Position := Corner + R4_Second * (5.0 * mm);
      Expected_Midpoint : constant Position := Bezier_Position (Start_Point, Corner, Finish_Point, 0.5);
      Result : constant Construction_Result :=
        Create_Parabolic (Start_Point, Corner, Finish_Point, Maximum_Length => 30.0 * mm);
   begin
      T.Register;
      Assert_Construction_Succeeded (Result, "R4 parabolic", T);
      if Result.Status /= Construction_Success then
         return;
      end if;

      T.Assert
        (abs (Split_Distance (Result.Transition) - Arc_Length (Result.Transition) / 2.0) > 0.1 * mm,
         "Unequal-trim parabolic split is not a hard-coded half length");
      Assert_Point_Close
        (Point_At_Distance (Result.Transition, 0.0 * mm), Start_Point, Geometry_Tolerance,
         "Parabolic start endpoint", T);
      Assert_Point_Close
        (Point_At_Distance (Result.Transition, Arc_Length (Result.Transition)), Finish_Point,
         Geometry_Tolerance, "Parabolic finish endpoint", T);
      Assert_Point_Close
        (Point_At_Distance (Result.Transition, Split_Distance (Result.Transition)), Expected_Midpoint,
         Position_Error_Bound (Result.Transition) + Geometry_Tolerance,
         "Parabolic split evaluates at Bezier parameter 0.5", T);
      declare
         First_Distance : constant Length := Arc_Length (Result.Transition) * 0.371_234;
         Second_Distance : constant Length := First_Distance + Arc_Length (Result.Transition) * 1.0E-9;
      begin
         T.Assert
           (Point_At_Distance (Result.Transition, First_Distance)
              /= Point_At_Distance (Result.Transition, Second_Distance),
            "Parabolic arc-length evaluation is not quantized into constant-distance plateaus");
      end;
      Assert_Envelope_Covers_Dense_Samples (Result.Transition, "R4 parabolic", T);
      Assert_Parabolic_Derivatives_Covered (Result.Transition, "R4 parabolic", T);
      Assert_Evaluator_Matches (Result.Transition, "R4 parabolic", T);
   end Test_Parabolic_R4_Geometry_And_Bounds;

   procedure Test_SCV_Angles_And_Axis_Selection (T : in out Trendy_Test.Operation'Class) is
      SCV : constant Velocity := 5.0 * mm / s;
      Root_Half : constant Dimensionless := 0.707_106_781_186_547_524_4;
      Acute_Tangent : constant Position_Scale :=
        [X_Axis => Root_Half, Y_Axis => Root_Half, others => 0.0];
      Obtuse_Tangent : constant Position_Scale :=
        [X_Axis => -Root_Half, Y_Axis => Root_Half, others => 0.0];
      Straight : constant SCV_Result := Compute_Sharp_SCV_Limit (X_Unit, X_Unit, SCV, True);
      Acute : constant SCV_Result := Compute_Sharp_SCV_Limit (X_Unit, Acute_Tangent, SCV, True);
      Right_Angle : constant SCV_Result := Compute_Sharp_SCV_Limit (X_Unit, Y_Unit, SCV, True);
      Obtuse : constant SCV_Result := Compute_Sharp_SCV_Limit (X_Unit, Obtuse_Tangent, SCV, True);
      Reversal : constant SCV_Result :=
        Compute_Sharp_SCV_Limit (X_Unit, X_Unit * (-1.0), SCV, True);
      Pure_E : constant SCV_Result := Compute_Sharp_SCV_Limit (E_Unit, E_Unit, SCV, True);
      Pure_E_Reversal : constant SCV_Result :=
        Compute_Sharp_SCV_Limit (E_Unit, E_Unit * (-1.0), SCV, True);
      Mixed : constant SCV_Result := Compute_Sharp_SCV_Limit (E_Unit, X_Unit, SCV, True);
      XYZE_Right_Angle : constant SCV_Result := Compute_Sharp_SCV_Limit (E_Unit, X_Unit, SCV, False);
      Spatial_E_Incoming : constant Position_Scale :=
        [X_Axis => Root_Half, E_Axis => Root_Half, others => 0.0];
      Spatial_E_Outgoing : constant Position_Scale :=
        [Y_Axis => Root_Half, E_Axis => Root_Half, others => 0.0];
      Projected_Right_Angle : constant SCV_Result :=
        Compute_Sharp_SCV_Limit (Spatial_E_Incoming, Spatial_E_Outgoing, SCV, True);
   begin
      T.Register;
      T.Assert
        (Straight.Status = SCV_Passthrough and then Straight.Velocity_Limit = Velocity'Last,
         "Straight SCV junction has no angular cap");
      T.Assert
        (Acute.Status = SCV_Success and then Acute.Velocity_Limit > SCV,
         "Acute SCV junction is faster than the configured 90-degree velocity");
      T.Assert
        (Right_Angle.Status = SCV_Success and then abs (Right_Angle.Velocity_Limit - SCV) <= 1.0E-12 * mm / s,
         "A 90-degree corner uses exactly the configured SCV");
      T.Assert
        (Obtuse.Status = SCV_Success and then Obtuse.Velocity_Limit < SCV,
         "Obtuse SCV junction is slower than the configured 90-degree velocity");
      T.Assert
        (Acute.Velocity_Limit > Right_Angle.Velocity_Limit
         and then Right_Angle.Velocity_Limit > Obtuse.Velocity_Limit,
         "SCV angular cap decreases monotonically from acute to obtuse");
      T.Assert
        (Reversal.Status = SCV_Reversal_Stop and then Reversal.Velocity_Limit = 0.0 * mm / s,
         "SCV reversal is a hard stop");
      T.Assert (Pure_E.Status = SCV_Passthrough, "Two pure-E moves use their XYZE tangents");
      T.Assert (Pure_E_Reversal.Status = SCV_Reversal_Stop, "Pure-E reversal stops");
      T.Assert
        (Mixed.Status = SCV_Mixed_Pure_E and then Mixed.Velocity_Limit = 0.0 * mm / s,
         "Mixed pure-E and spatial motion stops when E is ignored");
      T.Assert
        (XYZE_Right_Angle.Status = SCV_Success
         and then abs (XYZE_Right_Angle.Velocity_Limit - SCV) <= 1.0E-12 * mm / s,
         "XYZE selection includes E in the SCV angle");
      T.Assert
        (Projected_Right_Angle.Status = SCV_Success
         and then
           abs (Projected_Right_Angle.Velocity_Limit * Root_Half - SCV)
           <= 1.0E-12 * mm / s,
         "XYZ-projected SCV is converted back to full XYZE path speed");
   end Test_SCV_Angles_And_Axis_Selection;

   procedure Test_Stereographic_Wrapper (T : in out Trendy_Test.Operation'Class) is
      Request : constant Stereographic_Curves.Blend_Request := Make_Stereographic_Request;
      Result : constant Construction_Result := Create_Stereographic (Request);
   begin
      T.Register;
      Assert_Construction_Succeeded (Result, "Stereographic transition", T);
      if Result.Status /= Construction_Success then
         return;
      end if;
      T.Assert
        (Split_Distance (Result.Transition) = Arc_Length (Result.Transition) / 2.0,
         "Stereographic split is half arc length");
      Assert_Point_Close
        (Point_At_Distance (Result.Transition, 0.0 * mm), Origin, Geometry_Tolerance,
         "Stereographic wrapper start endpoint", T);
      Assert_Point_Close
        (Point_At_Distance (Result.Transition, Arc_Length (Result.Transition)),
         Request.Finish.Point, Geometry_Tolerance,
         "Stereographic wrapper finish endpoint", T);
      Assert_Envelope_Covers_Dense_Samples (Result.Transition, "Stereographic wrapper", T);
      Assert_Evaluator_Matches (Result.Transition, "Stereographic wrapper", T);
   end Test_Stereographic_Wrapper;

   procedure Test_Zero_Length_Policy_Separation (T : in out Trendy_Test.Operation'Class) is
      Point : constant Position := R4_Origin;
      Stop : constant Corner_Transition := Stop_At (Point);
      Through : constant Corner_Transition := Passthrough_At (Point);
      Sharp : constant Corner_Transition := Sharp_At (Point, 7.0 * mm / s);
   begin
      T.Register;
      T.Assert
        (Arc_Length (Stop) = 0.0 * mm
         and then Arc_Length (Through) = 0.0 * mm
         and then Arc_Length (Sharp) = 0.0 * mm,
         "All explicit junction-only variants have zero geometric length");
      T.Assert
        (Split_Distance (Stop) = 0.0 * mm
         and then Split_Distance (Through) = 0.0 * mm
         and then Split_Distance (Sharp) = 0.0 * mm,
         "All junction-only variants have a zero split distance");
      T.Assert
        (Junction_Velocity_Limit (Stop) = 0.0 * mm / s
         and then Junction_Velocity_Limit (Through) = Velocity'Last
         and then Junction_Velocity_Limit (Sharp) = 7.0 * mm / s,
         "Each zero-length policy retains its distinct junction velocity semantics");
      Assert_Point_Close (Point_At_Parameter (Stop, 0.75), Point, 0.0 * mm, "Hard-stop point", T);
      Assert_Point_Close (Point_At_Parameter (Through, 0.75), Point, 0.0 * mm, "Passthrough point", T);
      Assert_Point_Close (Point_At_Parameter (Sharp, 0.75), Point, 0.0 * mm, "Sharp point", T);
      Assert_Evaluator_Matches (Stop, "Hard stop", T);
      Assert_Evaluator_Matches (Through, "Passthrough", T);
      Assert_Evaluator_Matches (Sharp, "Sharp SCV", T);
   end Test_Zero_Length_Policy_Separation;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Zero_Length_Policy_Separation'Access,
         Test_SCV_Angles_And_Axis_Selection'Access,
         Test_Circular_R4_Geometry_And_Bounds'Access,
         Test_Parabolic_R4_Geometry_And_Bounds'Access,
         Test_Biarc_Determinism_And_Reversal'Access,
         Test_Biarc_Trim_Ratio_Search'Access,
         Test_Stereographic_Wrapper'Access,
         Test_Fixed_Size_Copying'Access];
   end All_Tests;

end Prunt.Motion_Planner.Corner_Transitions.Test;
