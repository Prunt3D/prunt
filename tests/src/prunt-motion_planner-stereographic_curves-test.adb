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
with Prunt.Motion_Planner.Stereographic_Curves.Geometry;
with Trendy_Test; use Trendy_Test;

package body Prunt.Motion_Planner.Stereographic_Curves.Test is

   pragma Extensions_Allowed (On);

   Geometry_Samples : constant Positive := 2_000;
   Derivative_Samples : constant Positive := 4_096;
   Point_Tolerance  : constant Length := 5.0E-6 * mm;
   Derivative_Tightness : constant Dimensionless := 1.05;
   Normalized_Derivative_Floor : constant Dimensionless := 1.0E-10;

   Origin : constant Position := [others => 0.0 * mm];
   X_Unit : constant Position_Scale :=
     [X_Axis => 1.0, others => 0.0];
   Y_Unit : constant Position_Scale :=
     [Y_Axis => 1.0, others => 0.0];

   function Make_Point
     (X, Y, Z, E : Length) return Position;

   function Make_Request
     (Start, Finish                  : Position;
      Start_Tangent, Finish_Tangent : Position_Scale;
      Maximum_Position_Error        : Length;
      Maximum_Arc_Length            : Length;
      Allow_Bulge                   : Boolean := False) return Blend_Request;

   function Quarter_Turn_Request
     (Scale : Dimensionless := 1.0) return Blend_Request;

   function Circular_Quarter_Turn_Request
     (Scale : Dimensionless := 1.0) return Blend_Request;

   function Point_Distance_To_Segment
     (Point, Segment_Start, Segment_Finish : Position)
      return Length;

   function Sample_Maximum_Deviation
     (Curve : Stereographic_Curve;
      Start, Corner, Finish : Position) return Length;

   function Sample_Minimum_Distance
     (Curve : Stereographic_Curve;
      Point : Position) return Length;

   function Bounds_Are_Zero
     (Bounds : Unit_Speed_Axial_Derivative_Bounds) return Boolean;

   function Cached_Tangent
     (Cache : Rational_Antiderivative;
      U     : Dimensionless) return Position_Scale;

   function Cached_Tangent_Derivative
     (Cache : Rational_Antiderivative;
      U     : Dimensionless;
      Order : Majorant_Order) return Dimensionless_Axis_Vector;

   function Executed_Tangent_Derivative
     (Evaluator : Stereographic_Curve_Evaluator;
      U         : Dimensionless;
      Order     : Majorant_Order) return Dimensionless_Axis_Vector;

   function Normalized_Derivative_Bound
     (Bounds : Unit_Speed_Axial_Derivative_Bounds;
      Length_Value : Length;
      Axis : Axis_Name;
      Order : Majorant_Order) return Dimensionless;

   function Vector_Distance
     (Left, Right : Position_Scale) return Dimensionless;

   procedure Assert_Length_Close
     (Actual, Expected, Tolerance : Length;
      Name                        : String;
      T                           : in out Trendy_Test.Operation'Class);

   procedure Assert_Point_Close
     (Actual, Expected : Position;
      Tolerance        : Length;
      Name             : String;
      T                : in out Trendy_Test.Operation'Class);

   procedure Assert_Result_Kind
     (Request  : Blend_Request;
      Expected : Blend_Result_Kind;
      Name     : String;
      T        : in out Trendy_Test.Operation'Class);

   procedure Assert_Bounds_Nonnegative
     (Bounds : Unit_Speed_Axial_Derivative_Bounds;
      Name   : String;
      T      : in out Trendy_Test.Operation'Class);

   procedure Assert_Executed_Derivative_Bounds
     (Curve                       : Stereographic_Curve;
      Bounds                      : Unit_Speed_Axial_Derivative_Bounds;
      Start_Distance, End_Distance : Length;
      Name                        : String;
      T                           : in out Trendy_Test.Operation'Class);

   --  Require the V7 retained Bernstein certificate to succeed directly, then compare its public derivative bounds
   --  with dense samples of the executed curve. This prevents the pole/Taylor fallback from hiding a broken new path.
   procedure Assert_Bernstein_Derivative_Bounds
     (Curve          : Stereographic_Curve;
      Start_U, End_U : Dimensionless;
      Name           : String;
      T              : in out Trendy_Test.Operation'Class);

   procedure Assert_Certificate_Covers_Dense_Samples
     (Curve : Stereographic_Curve;
      Name  : String;
      T     : in out Trendy_Test.Operation'Class);

   procedure Assert_Geometry_Intervals_Cover_Dense_Samples
     (Curve                       : Stereographic_Curve;
      Start, Corner, Finish, Point : Position;
      Deviation                   : Geometry.Distance_Interval;
      Point_Distance              : Geometry.Distance_Interval;
      Name                        : String;
      T                           : in out Trendy_Test.Operation'Class);

   function Make_Point
     (X, Y, Z, E : Length) return Position
   is
   begin
      return
        [X_Axis => X,
         Y_Axis => Y,
         Z_Axis => Z,
         E_Axis => E];
   end Make_Point;

   function Make_Request
     (Start, Finish                  : Position;
      Start_Tangent, Finish_Tangent : Position_Scale;
      Maximum_Position_Error        : Length;
      Maximum_Arc_Length            : Length;
      Allow_Bulge                   : Boolean := False) return Blend_Request
   is
      Result : Blend_Request;
   begin
      Result.Start.Point := Start;
      Result.Start.Jet.Tangent := Start_Tangent;
      Result.Finish.Point := Finish;
      Result.Finish.Jet.Tangent := Finish_Tangent;
      Result.Maximum_Position_Error := Maximum_Position_Error;
      Result.Maximum_Arc_Length := Maximum_Arc_Length;
      Result.Allow_Bulge := Allow_Bulge;
      return Result;
   end Make_Request;

   function Quarter_Turn_Request
     (Scale : Dimensionless := 1.0) return Blend_Request
   is
   begin
      return
        Make_Request
          (Start                  => Origin,
           Finish                 =>
             Make_Point
               (10.0 * mm * Scale,
                10.0 * mm * Scale,
                0.0 * mm,
                0.0 * mm),
           Start_Tangent          => X_Unit,
           Finish_Tangent         => Y_Unit,
           Maximum_Position_Error => 1.0E-3 * mm * Scale,
           Maximum_Arc_Length     => 40.0 * mm * Scale);
   end Quarter_Turn_Request;

   function Circular_Quarter_Turn_Request
     (Scale : Dimensionless := 1.0) return Blend_Request
   is
      Radius    : constant Length := 10.0 * mm * Scale;
      Turn_Rate : constant Curvature := 1.0 / Radius;
      Result    : Blend_Request := Quarter_Turn_Request (Scale);
   begin
      --  These are the endpoint derivatives of a counter-clockwise circle.
      --  In particular they exercise every derivative order in the request
      --  without relying on any private chart representation.
      Result.Start.Jet.Tangent_Derivative_1 :=
        [Y_Axis => Turn_Rate, others => 0.0 / mm];
      Result.Start.Jet.Tangent_Derivative_2 :=
        [X_Axis => -(Turn_Rate ** 2), others => 0.0 / mm ** 2];
      Result.Start.Jet.Tangent_Derivative_3 :=
        [Y_Axis => -(Turn_Rate ** 3), others => 0.0 / mm ** 3];

      Result.Finish.Jet.Tangent_Derivative_1 :=
        [X_Axis => -Turn_Rate, others => 0.0 / mm];
      Result.Finish.Jet.Tangent_Derivative_2 :=
        [Y_Axis => -(Turn_Rate ** 2), others => 0.0 / mm ** 2];
      Result.Finish.Jet.Tangent_Derivative_3 :=
        [X_Axis => Turn_Rate ** 3, others => 0.0 / mm ** 3];
      return Result;
   end Circular_Quarter_Turn_Request;

   function Point_Distance_To_Segment
     (Point, Segment_Start, Segment_Finish : Position)
      return Length
   is
      Segment        : constant Position_Offset :=
        Segment_Finish - Segment_Start;
      Length_Squared : constant Area := Dot (Segment, Segment);
   begin
      if Length_Squared = 0.0 * mm ** 2 then
         return abs (Point - Segment_Start);
      end if;

      declare
         Fraction : constant Dimensionless :=
           Dimensionless'Max
             (0.0,
              Dimensionless'Min
                (1.0,
                 Dot (Point - Segment_Start, Segment)
                   / Length_Squared));
      begin
         return
           abs
             (Point
              - (Segment_Start + Segment * Fraction));
      end;
   end Point_Distance_To_Segment;

   function Sample_Maximum_Deviation
     (Curve : Stereographic_Curve;
      Start, Corner, Finish : Position) return Length
   is
      Result : Length := 0.0 * mm;
   begin
      for Sample in 0 .. Geometry_Samples loop
         declare
            Parameter : constant Curve_Parameter :=
              Curve_Parameter
                (Dimensionless (Sample)
                 / Dimensionless (Geometry_Samples));
            Curve_Point : constant Position :=
              Point_At_Parameter (Curve, Parameter);
         begin
            Result :=
              Length'Max
                (Result,
                 Length'Min
                   (Point_Distance_To_Segment
                      (Curve_Point, Start, Corner),
                    Point_Distance_To_Segment
                      (Curve_Point, Corner, Finish)));
         end;
      end loop;
      return Result;
   end Sample_Maximum_Deviation;

   function Sample_Minimum_Distance
     (Curve : Stereographic_Curve;
      Point : Position) return Length
   is
      Result : Length := Length'Last;
   begin
      for Sample in 0 .. Geometry_Samples loop
         Result :=
           Length'Min
             (Result,
              abs
                (Point_At_Parameter
                   (Curve,
                    Curve_Parameter
                      (Dimensionless (Sample)
                       / Dimensionless (Geometry_Samples)))
                 - Point));
      end loop;
      return Result;
   end Sample_Minimum_Distance;

   function Bounds_Are_Zero
     (Bounds : Unit_Speed_Axial_Derivative_Bounds) return Boolean
   is
   begin
      for Axis in Axis_Name loop
         if Bounds.Velocity (Axis) /= 0.0
           or else Bounds.Acceleration (Axis) /= 0.0 / mm
           or else Bounds.Jerk (Axis) /= 0.0 / mm ** 2
           or else Bounds.Snap (Axis) /= 0.0 / mm ** 3
           or else Bounds.Crackle (Axis) /= 0.0 / mm ** 4
         then
            return False;
         end if;
      end loop;
      return True;
   end Bounds_Are_Zero;

   function Cached_Tangent
     (Cache : Rational_Antiderivative;
      U     : Dimensionless) return Position_Scale
   is
      Result : Position_Scale :=
        [for Axis in Axis_Name => Cache.Constant_Tangent (Axis)];
   begin
      for Index in 1 .. Cache.Real_Pole_Count loop
         for Axis in Axis_Name loop
            Result (Axis) :=
              Result (Axis)
              + Cache.Pole_Slots (Index).Residue_Component (Axis)
                / (U - Cache.Pole_Slots (Index).Pole_Component);
         end loop;
      end loop;

      for Pair in 1 .. Cache.Pair_Count loop
         declare
            Real_Slot : Rational_Pole_Slot renames
              Cache.Pole_Slots
                (Complex_Pair_Real_Slot
                   (Cache, Rational_Pair_Index (Pair)));
            Imaginary_Slot : Rational_Pole_Slot renames
              Cache.Pole_Slots
                (Complex_Pair_Imaginary_Slot
                   (Cache, Rational_Pair_Index (Pair)));
            Offset : constant Dimensionless :=
              U - Real_Slot.Pole_Component;
            Denominator : constant Dimensionless :=
              Offset * Offset
              + Imaginary_Slot.Pole_Component ** 2;
         begin
            for Axis in Axis_Name loop
               Result (Axis) :=
                 Result (Axis)
                 + 2.0
                   * (Real_Slot.Residue_Component (Axis) * Offset
                      - Imaginary_Slot.Residue_Component (Axis)
                        * Imaginary_Slot.Pole_Component)
                   / Denominator;
            end loop;
         end;
      end loop;
      return Result;
   end Cached_Tangent;

   --  Differentiate the retained partial-fraction tangent analytically. This
   --  is intentionally independent of the production bound implementation:
   --
   --    d^k/dU^k (r / (U - p)) =
   --      (-1)^k k! r / (U - p)^(k + 1).
   --
   --  A conjugate pair is evaluated as twice the real part of the
   --  corresponding complex expression.
   function Cached_Tangent_Derivative
     (Cache : Rational_Antiderivative;
      U     : Dimensionless;
      Order : Majorant_Order) return Dimensionless_Axis_Vector
   is
      type Complex_Scalar is record
         Real_Part      : Dimensionless;
         Imaginary_Part : Dimensionless;
      end record;

      function Multiply
        (Left, Right : Complex_Scalar) return Complex_Scalar
      is
        (Real_Part      =>
           Left.Real_Part * Right.Real_Part
           - Left.Imaginary_Part * Right.Imaginary_Part,
         Imaginary_Part =>
           Left.Real_Part * Right.Imaginary_Part
           + Left.Imaginary_Part * Right.Real_Part);

      Signed_Factor : constant Dimensionless :=
        (if Order mod 2 = 0 then Factorial (Order) else -Factorial (Order));
      Result : Dimensionless_Axis_Vector :=
        (if Order = 0
         then Cache.Constant_Tangent
         else [others => 0.0]);
   begin
      for Index in 1 .. Cache.Real_Pole_Count loop
         declare
            Slot : Rational_Pole_Slot renames Cache.Pole_Slots (Index);
            Denominator : constant Dimensionless :=
              (U - Slot.Pole_Component) ** (Natural (Order) + 1);
         begin
            for Axis in Axis_Name loop
               Result (Axis) :=
                 Result (Axis)
                 + Signed_Factor
                   * Slot.Residue_Component (Axis)
                   / Denominator;
            end loop;
         end;
      end loop;

      for Pair in 1 .. Cache.Pair_Count loop
         declare
            Real_Slot : Rational_Pole_Slot renames
              Cache.Pole_Slots
                (Complex_Pair_Real_Slot
                   (Cache, Rational_Pair_Index (Pair)));
            Imaginary_Slot : Rational_Pole_Slot renames
              Cache.Pole_Slots
                (Complex_Pair_Imaginary_Slot
                   (Cache, Rational_Pair_Index (Pair)));
            Offset : constant Dimensionless :=
              U - Real_Slot.Pole_Component;
            Imaginary_Pole : constant Dimensionless :=
              Imaginary_Slot.Pole_Component;
            Denominator : constant Dimensionless :=
              Offset * Offset + Imaginary_Pole * Imaginary_Pole;
            Reciprocal : constant Complex_Scalar :=
              (Real_Part      => Offset / Denominator,
               Imaginary_Part => Imaginary_Pole / Denominator);
            Reciprocal_Power : Complex_Scalar := (1.0, 0.0);
         begin
            for Exponent in 1 .. Natural (Order) + 1 loop
               Reciprocal_Power :=
                 Multiply (Reciprocal_Power, Reciprocal);
            end loop;
            for Axis in Axis_Name loop
               Result (Axis) :=
                 Result (Axis)
                 + 2.0
                   * Signed_Factor
                   * (Real_Slot.Residue_Component (Axis)
                        * Reciprocal_Power.Real_Part
                      - Imaginary_Slot.Residue_Component (Axis)
                        * Reciprocal_Power.Imaginary_Part);
            end loop;
         end;
      end loop;
      return Result;
   end Cached_Tangent_Derivative;

   --  The public evaluator adds R H(U), where H is the degree-eleven
   --  endpoint smootherstep and R is Finish_Point - Uncorrected_Finish_Point. Its k-th normalized tangent derivative
   --  therefore adds (R / L) H^(k + 1)(U). This dense-sample diagnostic rounds R once; production certificates use
   --  the outward correction interval instead.
   function Executed_Tangent_Derivative
     (Evaluator : Stereographic_Curve_Evaluator;
      U         : Dimensionless;
      Order     : Majorant_Order) return Dimensionless_Axis_Vector
   is
      type Smootherstep_Coefficient_Array is
        array (Natural range 0 .. 11) of Dimensionless;
      Smootherstep_Coefficients : constant Smootherstep_Coefficient_Array :=
        [0 .. 5 => 0.0,
         6      => 462.0,
         7      => -1_980.0,
         8      => 3_465.0,
         9      => -3_080.0,
         10     => 1_386.0,
         11     => -252.0];
      Derivative_Order : constant Positive := Natural (Order) + 1;
      Smootherstep_Derivative : Dimensionless := 0.0;
      Length_Raw : constant Dimensionless :=
        Dimensionless (Evaluator.Length_Value / mm);
      Result : Dimensionless_Axis_Vector :=
        Cached_Tangent_Derivative
          (Evaluator.Antiderivative_Cache, U, Order);
   begin
      --  Horner evaluation of the differentiated power polynomial. At both
      --  endpoints the integer coefficients cancel exactly for orders 1..5.
      for Degree in reverse Derivative_Order .. 11 loop
         Smootherstep_Derivative :=
           Smootherstep_Derivative * U
           + Smootherstep_Coefficients (Degree)
             * Factorial (Degree)
             / Factorial (Degree - Derivative_Order);
      end loop;

      for Axis in Axis_Name loop
         Result (Axis) :=
           Result (Axis)
           + Dimensionless
               ((Evaluator.Finish_Point (Axis) - Evaluator.Uncorrected_Finish_Point (Axis)) / mm)
             / Length_Raw
             * Smootherstep_Derivative;
      end loop;
      return Result;
   end Executed_Tangent_Derivative;

   function Normalized_Derivative_Bound
     (Bounds       : Unit_Speed_Axial_Derivative_Bounds;
      Length_Value : Length;
      Axis         : Axis_Name;
      Order        : Majorant_Order) return Dimensionless
   is
   begin
      case Order is
         when 0 =>
            return Bounds.Velocity (Axis);

         when 1 =>
            return
              Dimensionless
                (Bounds.Acceleration (Axis) * Length_Value);

         when 2 =>
            return
              Dimensionless
                ((Bounds.Jerk (Axis) * Length_Value)
                 * Length_Value);

         when 3 =>
            return
              Dimensionless
                (((Bounds.Snap (Axis) * Length_Value)
                  * Length_Value)
                 * Length_Value);

         when 4 =>
            return
              Dimensionless
                ((((Bounds.Crackle (Axis) * Length_Value)
                   * Length_Value)
                  * Length_Value)
                 * Length_Value);
      end case;
   end Normalized_Derivative_Bound;

   function Vector_Distance
     (Left, Right : Position_Scale) return Dimensionless
   is
      Sum : Dimensionless := 0.0;
   begin
      for Axis in Axis_Name loop
         Sum := Sum + (Left (Axis) - Right (Axis)) ** 2;
      end loop;
      return Dimensionless_Math.Sqrt (Sum);
   end Vector_Distance;

   procedure Assert_Length_Close
     (Actual, Expected, Tolerance : Length;
      Name                        : String;
      T                           : in out Trendy_Test.Operation'Class)
   is
   begin
      T.Assert
        (abs (Actual - Expected) <= Tolerance,
         Name
         & ": actual "
         & Actual'Image
         & ", expected "
         & Expected'Image
         & ", tolerance "
         & Tolerance'Image);
   end Assert_Length_Close;

   procedure Assert_Point_Close
     (Actual, Expected : Position;
      Tolerance        : Length;
      Name             : String;
      T                : in out Trendy_Test.Operation'Class)
   is
      Error : constant Length := abs (Actual - Expected);
   begin
      T.Assert
        (Error <= Tolerance,
         Name
         & ": point error "
         & Error'Image
         & ", tolerance "
         & Tolerance'Image);
   end Assert_Point_Close;

   procedure Assert_Result_Kind
     (Request  : Blend_Request;
      Expected : Blend_Result_Kind;
      Name     : String;
      T        : in out Trendy_Test.Operation'Class)
   is
      Result : constant Blend_Result := Create_Blend (Request);
   begin
      T.Assert
        (Result.Kind = Expected,
         Name
         & ": got "
         & Result.Kind'Image
         & ", expected "
         & Expected'Image);
   end Assert_Result_Kind;

   procedure Assert_Bounds_Nonnegative
     (Bounds : Unit_Speed_Axial_Derivative_Bounds;
      Name   : String;
      T      : in out Trendy_Test.Operation'Class)
   is
   begin
      for Axis in Axis_Name loop
         T.Assert
           (Bounds.Velocity (Axis) >= 0.0
            and then Bounds.Acceleration (Axis) >= 0.0 / mm
            and then Bounds.Jerk (Axis) >= 0.0 / mm ** 2
            and then Bounds.Snap (Axis) >= 0.0 / mm ** 3
            and then Bounds.Crackle (Axis) >= 0.0 / mm ** 4,
            Name & " derivative bounds are nonnegative");
      end loop;
   end Assert_Bounds_Nonnegative;

   procedure Assert_Executed_Derivative_Bounds
     (Curve                        : Stereographic_Curve;
      Bounds                       : Unit_Speed_Axial_Derivative_Bounds;
      Start_Distance, End_Distance : Length;
      Name                         : String;
      T                            : in out Trendy_Test.Operation'Class)
   is
      Evaluator : constant Stereographic_Curve_Evaluator := To_Evaluator (Curve);
      Total : constant Length := Arc_Length (Curve);
      Start_U : constant Dimensionless :=
        Dimensionless (Start_Distance / Total);
      End_U : constant Dimensionless :=
        Dimensionless (End_Distance / Total);
      Last_Sample : constant Natural :=
        (if Start_Distance = End_Distance
         then 0
         else Derivative_Samples);
      Observed : Axis_Majorants := [others => [others => 0.0]];
   begin
      for Sample in 0 .. Last_Sample loop
         declare
            Fraction : constant Dimensionless :=
              Dimensionless (Sample) / Dimensionless (Derivative_Samples);
            U : constant Dimensionless :=
              Start_U + (End_U - Start_U) * Fraction;
         begin
            for Order in Majorant_Order loop
               declare
                  Derivative : constant Dimensionless_Axis_Vector :=
                    Executed_Tangent_Derivative
                      (Evaluator, U, Order);
               begin
                  for Axis in Axis_Name loop
                     Observed (Axis) (Order) :=
                       Dimensionless'Max
                         (Observed (Axis) (Order),
                          abs Derivative (Axis));
                  end loop;
               end;
            end loop;
         end;
      end loop;

      for Axis in Axis_Name loop
         for Order in Majorant_Order loop
            declare
               Actual : constant Dimensionless :=
                 Observed (Axis) (Order);
               Bound : constant Dimensionless :=
                 Normalized_Derivative_Bound
                   (Bounds, Total, Axis, Order);
               Coverage_Allowance : constant Dimensionless :=
                 512.0
                 * Dimensionless'Model_Epsilon
                 * (1.0 + Actual);
               Tight_Upper : constant Dimensionless :=
                 Derivative_Tightness * Actual
                 + Normalized_Derivative_Floor;
               Context : constant String :=
                 Name
                 & ", "
                 & Axis'Image
                 & ", tangent derivative order"
                 & Order'Image;
            begin
               T.Assert
                 (Bound + Coverage_Allowance >= Actual,
                  Context
                  & " sample is covered: observed "
                  & Actual'Image
                  & ", bound "
                  & Bound'Image);
               T.Assert
                 (Bound <= Tight_Upper,
                  Context
                  & " bound is close to the executed curve: observed "
                  & Actual'Image
                  & ", bound "
                  & Bound'Image
                  & ", permitted "
                  & Tight_Upper'Image);
            end;
         end loop;
      end loop;
   end Assert_Executed_Derivative_Bounds;

   procedure Assert_Bernstein_Derivative_Bounds
     (Curve          : Stereographic_Curve;
      Start_U, End_U : Dimensionless;
      Name           : String;
      T              : in out Trendy_Test.Operation'Class)
   is
      Success    : Boolean;
      Majorants  : constant Axis_Majorants :=
        Bernstein_Tangent_Range_Majorants (Curve, Start_U, End_U, Success);
      Bounds     : constant Unit_Speed_Axial_Derivative_Bounds :=
        Bounds_On_Parameter_Range (Curve, Start_U, End_U);
      Total      : constant Length := Arc_Length (Curve);
      Certificate : Retained_Tangent_Bernstein_Certificate renames Curve.Retained_Tangent_Certificate;
   begin
      T.Assert
        (Certificate.Valid
         and then Certificate.Minimum_Denominator > 0.0
         and then Is_Finite (Certificate.Minimum_Denominator),
         Name & " retains a valid positive-denominator Bernstein certificate");
      for Index in 0 .. Certificate.Degree loop
         T.Assert (Certificate.Denominator (Index).Valid, Name & " has valid denominator controls");
         for Axis in Axis_Name loop
            T.Assert
              (Certificate.Axis_Numerators (Axis) (Index).Valid,
               Name & " has valid numerator controls");
         end loop;
      end loop;
      T.Assert (Success, Name & " uses the Bernstein derivative certificate without fallback");
      if not Success then
         return;
      end if;
      for Axis in Axis_Name loop
         for Order in Majorant_Order loop
            T.Assert
              (Majorants (Axis) (Order) >= 0.0
               and then Majorants (Axis) (Order) < Dimensionless'Last
               and then Is_Finite (Majorants (Axis) (Order)),
               Name & " returns finite nonnegative Bernstein majorants");
         end loop;
      end loop;
      Assert_Executed_Derivative_Bounds
        (Curve,
         Bounds,
         Start_U * Total,
         End_U * Total,
         Name,
         T);
   end Assert_Bernstein_Derivative_Bounds;

   procedure Assert_Certificate_Covers_Dense_Samples
     (Curve : Stereographic_Curve;
      Name  : String;
      T     : in out Trendy_Test.Operation'Class)
   is
      Certificate : constant Length :=
        Position_Error_Bound (Curve);
      Reference_Allowance : constant Length :=
        1.0E-9 * mm;
   begin
      T.Assert
        (Certificate >= 0.0 * mm
         and then Certificate < Length'Last,
         Name & " has a finite nonnegative certificate");

      for Sample in 0 .. 256 loop
         declare
            Parameter : constant Curve_Parameter :=
              Curve_Parameter
                (Dimensionless (Sample) / 256.0);
            Executed : constant Position :=
              Point_At_Parameter (Curve, Parameter);
            Ideal_Reference : constant Position :=
              Ideal_Point_At_Parameter
                (Curve, Parameter);
         begin
            T.Assert
              (abs (Executed - Ideal_Reference)
                 <= Certificate + Reference_Allowance,
               Name
               & " dense sample "
               & Sample'Image
               & " lies inside the global certificate");
         end;
      end loop;
   end Assert_Certificate_Covers_Dense_Samples;

   procedure Assert_Geometry_Intervals_Cover_Dense_Samples
     (Curve                       : Stereographic_Curve;
      Start, Corner, Finish, Point : Position;
      Deviation                   : Geometry.Distance_Interval;
      Point_Distance              : Geometry.Distance_Interval;
      Name                        : String;
      T                           : in out Trendy_Test.Operation'Class)
   is
      Sampled_Deviation : constant Length :=
        Sample_Maximum_Deviation
          (Curve, Start, Corner, Finish);
      Sampled_Distance : constant Length :=
        Sample_Minimum_Distance (Curve, Point);
      Sample_Gap : constant Length :=
        Arc_Length (Curve) / Dimensionless (Geometry_Samples);
      Approximation_Allowance : constant Length :=
        Sample_Gap
        + 2.0 * Position_Error_Bound (Curve)
        + Point_Tolerance;
   begin
      T.Assert
        (Deviation.Lower >= 0.0 * mm
         and then Deviation.Lower <= Deviation.Upper,
         Name & " deviation interval is ordered and nonnegative");
      T.Assert
        (Point_Distance.Lower >= 0.0 * mm
         and then Point_Distance.Lower <= Point_Distance.Upper,
         Name & " point-distance interval is ordered and nonnegative");

      T.Assert
        (Deviation.Upper = Length'Last
         or else
           Sampled_Deviation
             <= Deviation.Upper + Point_Tolerance,
         Name & " deviation upper bound contains every dense sample");
      T.Assert
        (Deviation.Lower
           <= Sampled_Deviation + Approximation_Allowance,
         Name & " deviation lower bound is compatible with dense samples");
      T.Assert
        (Point_Distance.Lower
           <= Sampled_Distance + Point_Tolerance,
         Name & " point-distance lower bound contains the sampled minimum");
      T.Assert
        (Point_Distance.Upper = Length'Last
         or else
           Sampled_Distance
             <= Point_Distance.Upper + Approximation_Allowance,
         Name & " point-distance upper bound is compatible with dense samples");
   end Assert_Geometry_Intervals_Cover_Dense_Samples;

   --  Exercise every pairing of negative, nonpositive, sign-spanning, nonnegative, and positive intervals. V7 selects
   --  extremal products by sign, so this compares each optimized branch with the full four-corner definition.
   procedure Test_Interval_Multiplication_Sign_Cases
     (T : in out Trendy_Test.Operation'Class)
   is
      type Interval_Case_Array is array (Positive range <>) of Interval;

      Cases : constant Interval_Case_Array :=
        [(Lower => -5.0, Upper => -2.0, Valid => True),
         (Lower => -4.0, Upper => 0.0, Valid => True),
         (Lower => -3.0, Upper => 7.0, Valid => True),
         (Lower => 0.0, Upper => 4.0, Valid => True),
         (Lower => 2.0, Upper => 11.0, Valid => True)];

      --  Deliberately simple reference implementation against which the sign-specialized production code is checked.
      function Four_Product_Reference (Left, Right : Interval) return Interval is
         P1 : constant Dimensionless := Left.Lower * Right.Lower;
         P2 : constant Dimensionless := Left.Lower * Right.Upper;
         P3 : constant Dimensionless := Left.Upper * Right.Lower;
         P4 : constant Dimensionless := Left.Upper * Right.Upper;
      begin
         return
           Checked_Interval
             (Down (Dimensionless'Min (Dimensionless'Min (P1, P2), Dimensionless'Min (P3, P4))),
              Up (Dimensionless'Max (Dimensionless'Max (P1, P2), Dimensionless'Max (P3, P4))));
      end Four_Product_Reference;

      Invalid : constant Interval :=
        (Lower => -Dimensionless'Last, Upper => Dimensionless'Last, Valid => False);
      Zero : constant Interval := Interval_Exact (0.0);
   begin
      T.Register;

      for Left_Index in Cases'Range loop
         for Right_Index in Cases'Range loop
            T.Assert
              (Interval_Multiply (Cases (Left_Index), Cases (Right_Index))
                 = Four_Product_Reference (Cases (Left_Index), Cases (Right_Index)),
               "Interval product sign case"
               & Left_Index'Image
               & ","
               & Right_Index'Image
               & " matches the four-product reference");
         end loop;
      end loop;

      T.Assert (Interval_Multiply (Zero, Cases (3)) = Zero, "Zero remains an exact product on the left");
      T.Assert (Interval_Multiply (Cases (3), Zero) = Zero, "Zero remains an exact product on the right");
      T.Assert
        (Interval_Multiply (Interval_Exact (1.0), Cases (3)) = Cases (3),
         "Positive one remains an exact multiplier");
      T.Assert
        (Interval_Multiply (Interval_Exact (-1.0), Cases (3)) = Interval_Negate (Cases (3)),
         "Negative one retains exact endpoint reversal");
      T.Assert
        (Interval_Multiply (Cases (3), Interval_Exact (1.0)) = Cases (3),
         "Positive one remains an exact right multiplier");
      T.Assert
        (Interval_Multiply (Cases (3), Interval_Exact (-1.0)) = Interval_Negate (Cases (3)),
         "Negative one retains exact right endpoint reversal");
      T.Assert
        (not Interval_Multiply (Invalid, Cases (3)).Valid
         and then not Interval_Multiply (Cases (3), Invalid).Valid,
         "An invalid operand still invalidates the product");
      T.Assert
        (not Interval_Multiply (Invalid, Zero).Valid
         and then not Interval_Multiply (Zero, Invalid).Valid,
         "Invalidity takes precedence over the exact-zero shortcut");

      declare
         Root : constant Dimensionless := Certified_Upper_Square_Root (Dimensionless'Model_Small);
      begin
         T.Assert
           (Root < Dimensionless'Last and then Root >= Up (Dimensionless'Model_Small / Root),
            "The certified square root remains usable at Model_Small");
      end;
   end Test_Interval_Multiplication_Sign_Cases;

   procedure Test_Complex_Pole_Primitive_Cancellation
     (T : in out Trendy_Test.Operation'Class)
   is
      A                 : constant Dimensionless := 0.5;
      B                 : constant Dimensionless :=
        0.3 * A * Dimensionless_Math.Sqrt (Dimensionless'Model_Epsilon);
      U                 : constant Dimensionless := A;
      Expected          : constant Dimensionless := 2.0 * Dimensionless_Math.Log (B / A);
      Tolerance         : constant Dimensionless :=
        512.0 * Dimensionless'Model_Epsilon * Dimensionless'Max (1.0, abs Expected);
      Cache             : Rational_Antiderivative;
      Huge_Pole_Cache   : Rational_Antiderivative;
      Unsafe_Residue_Cache : Rational_Antiderivative;
      Base_Square       : Dimensionless := A * A + B * B;
      Unstable_Change   : Dimensionless;
      Displacement      : Dimensionless_Axis_Vector;
      Huge_Displacement : Dimensionless_Axis_Vector;
      Huge_B            : constant Dimensionless := 2.0 * Dimensionless_Math.Sqrt (Dimensionless'Last);
      pragma Volatile (Base_Square);
      pragma Volatile (Unstable_Change);
   begin
      T.Register;

      --  Choose B from the active floating-point model so B^2 is lost when added to A^2. The previous primitive
      --  consequently formed an increment of exactly -1 and called Log(0), on both binary64 and extended hosts.
      Unstable_Change := (U * U - 2.0 * A * U) / Base_Square;
      T.Assert
        (Unstable_Change = -1.0,
         "Complex-pole regression recreates the cancelled log1p argument");

      Cache.Pair_Count := 1;
      Cache.Pole_Slots (1).Pole_Component := A;
      Cache.Pole_Slots (1).Residue_Component (X_Axis) := 1.0;
      Cache.Pole_Slots (2).Pole_Component := B;
      T.Assert
        (Rational_Antiderivative_Is_Well_Formed (Cache)
         and then Rational_Antiderivative_Primitives_Are_Safe (Cache),
         "Cancellation-prone complex pole is structurally valid and safe to evaluate");

      Displacement := Evaluate_Rational_Displacement (Cache, U);
      T.Assert
        (Is_Finite (Displacement (X_Axis)),
         "Scaled complex-pole primitive remains finite at its closest point");
      T.Assert
        (abs (Displacement (X_Axis) - Expected) <= Tolerance,
         "Scaled complex-pole primitive matches the analytical logarithm");
      T.Assert
        ((for all Axis in Axis_Name =>
            (if Axis = X_Axis then True else Displacement (Axis) = 0.0)),
         "Zero complex residues leave every other axis unchanged");

      Huge_Pole_Cache.Pair_Count := 1;
      Huge_Pole_Cache.Pole_Slots (1).Pole_Component := 0.0;
      Huge_Pole_Cache.Pole_Slots (2).Pole_Component := Huge_B;
      Huge_Pole_Cache.Pole_Slots (2).Residue_Component (X_Axis) := 1.0;
      T.Assert
        (Rational_Antiderivative_Is_Well_Formed (Huge_Pole_Cache)
         and then Rational_Antiderivative_Primitives_Are_Safe (Huge_Pole_Cache),
         "A complex pole whose unscaled square overflows remains safely evaluable");
      Huge_Displacement := Evaluate_Rational_Displacement (Huge_Pole_Cache, 1.0);
      T.Assert
        ((for all Axis in Axis_Name => Is_Finite (Huge_Displacement (Axis))),
         "Scaled complex angle evaluation remains finite for a huge imaginary pole");

      Unsafe_Residue_Cache.Pair_Count := 1;
      Unsafe_Residue_Cache.Pole_Slots (1).Pole_Component := 0.5;
      Unsafe_Residue_Cache.Pole_Slots (1).Residue_Component (X_Axis) := Dimensionless'Last;
      Unsafe_Residue_Cache.Pole_Slots (2).Pole_Component := 0.25;
      T.Assert
        (Rational_Antiderivative_Is_Well_Formed (Unsafe_Residue_Cache)
         and then not Rational_Antiderivative_Primitives_Are_Safe (Unsafe_Residue_Cache),
         "A structurally valid finite residue whose primitive product can overflow is rejected during construction");
   end Test_Complex_Pole_Primitive_Cancellation;

   procedure Test_Default_And_Zero_State
     (T : in out Trendy_Test.Operation'Class)
   is
      Default_Curve : Stereographic_Curve;
      Point : constant Position :=
        Make_Point
          (4.0 * mm,
           -3.0 * mm,
           2.0 * mm,
           1.0 * mm);
      Curve : constant Stereographic_Curve := Zero_Blend (Point);
      Evaluator : constant Stereographic_Curve_Evaluator := To_Evaluator (Curve);
      Coefficients : constant Projection_Coefficients :=
        [X_Axis => 2.0 / mm,
         Y_Axis => -1.0 / mm,
         Z_Axis => 3.0 / mm,
         E_Axis => 4.0 / mm];
   begin
      T.Register;

      T.Assert
        (Arc_Length (Default_Curve) = 0.0 * mm,
         "Default curve length is zero");
      T.Assert
        (Bounds_Are_Zero (Derivative_Bounds (Default_Curve)),
         "Default curve bounds are zero");
      T.Assert
        (Bounds_Are_Zero
           (Derivative_Bounds
              (Default_Curve, 0.0 * mm, 0.0 * mm)),
         "Default point-range bounds are zero");
      T.Assert
        (Projected_Tangent_Bound
           (Default_Curve, Coefficients) = 0.0 / mm,
         "Default projected tangent bound is zero");
      Assert_Point_Close
        (Point_At_Parameter (Default_Curve, 0.37),
         Origin,
         0.0 * mm,
         "Default curve is located at the origin",
         T);
      T.Assert
        (Arc_Length (Curve) = 0.0 * mm,
         "Explicit zero curve length is zero");
      T.Assert
        (Arc_Length (Evaluator) = 0.0 * mm,
         "Explicit zero evaluator length is zero");
      T.Assert
        (Bounds_Are_Zero (Derivative_Bounds (Curve)),
         "Explicit zero curve bounds are zero");

      for Axis in Axis_Name loop
         T.Assert
           (Axis_Is_Structurally_Constant (Curve, Axis),
            "Every zero-curve axis is structurally constant");
      end loop;

      for Sample in 0 .. 8 loop
         declare
            Parameter : constant Curve_Parameter :=
              Curve_Parameter (Dimensionless (Sample) / 8.0);
         begin
            Assert_Point_Close
              (Point_At_Parameter (Curve, Parameter),
               Point,
               0.0 * mm,
               "Zero curve parameter evaluation",
               T);
            Assert_Point_Close
              (Point_At_Parameter (Evaluator, Parameter),
               Point,
               0.0 * mm,
               "Zero evaluator parameter evaluation",
               T);
         end;
      end loop;

      Assert_Point_Close
        (Point_At_Distance (Evaluator, 0.0 * mm),
         Point,
         0.0 * mm,
         "Zero evaluator distance evaluation",
         T);
   end Test_Default_And_Zero_State;

   procedure Test_Request_Validation
     (T : in out Trendy_Test.Operation'Class)
   is
      Valid : constant Blend_Request :=
        Make_Request
          (Start                  => Origin,
           Finish                 =>
             Make_Point
               (10.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
           Start_Tangent          => X_Unit,
           Finish_Tangent         => X_Unit,
           Maximum_Position_Error => 1.0E-6 * mm,
           Maximum_Arc_Length     => 20.0 * mm);
      Request         : Blend_Request;
      Default_Request : Blend_Request;
   begin
      T.Register;

      T.Assert
        (Satisfies_Unit_Tangent_Identities
           (Valid.Start.Jet.Tangent,
            Valid.Start.Jet.Tangent_Derivative_1,
            Valid.Start.Jet.Tangent_Derivative_2,
            Valid.Start.Jet.Tangent_Derivative_3),
         "A flat unit tangent satisfies the public jet predicate");
      T.Assert
        (not Satisfies_Unit_Tangent_Identities
           ([others => 0.0],
            Valid.Start.Jet.Tangent_Derivative_1,
            Valid.Start.Jet.Tangent_Derivative_2,
            Valid.Start.Jet.Tangent_Derivative_3),
         "A zero tangent fails the public jet predicate");
      T.Assert
        (not Satisfies_Unit_Tangent_Identities
           ([X_Axis => 2.0, others => 0.0],
            Valid.Start.Jet.Tangent_Derivative_1,
            Valid.Start.Jet.Tangent_Derivative_2,
            Valid.Start.Jet.Tangent_Derivative_3),
         "A non-unit tangent fails the public jet predicate");
      T.Assert
        (not Satisfies_Unit_Tangent_Identities
           (X_Unit,
            [X_Axis => 1.0E-3 / mm, others => 0.0 / mm],
            Valid.Start.Jet.Tangent_Derivative_2,
            Valid.Start.Jet.Tangent_Derivative_3),
         "A longitudinal first derivative fails the jet predicate");

      Request := Valid;
      Request.Start.Jet.Tangent_Derivative_1 := [X_Axis => 1.0 / mm, others => 0.0 / mm];
      Assert_Result_Kind
        (Request,
         Blend_Invalid_Start_Jets,
         "An invalid raw start jet is reported instead of raising a predicate assertion",
         T);

      Request := Valid;
      Request.Finish.Jet.Tangent_Derivative_1 := [X_Axis => 1.0 / mm, others => 0.0 / mm];
      Assert_Result_Kind
        (Request,
         Blend_Invalid_Finish_Jets,
         "An invalid raw finish jet is reported instead of raising a predicate assertion",
         T);

      declare
         Invalid_Jet : constant Endpoint_Tangent_Jet :=
           (Tangent              => X_Unit,
            Tangent_Derivative_1 => [X_Axis => 1.0 / mm, others => 0.0 / mm],
            others               => <>);
         Canonical   : Scaled_Tangent_Jet;
         Short_Chord : constant Length := 1_024.0 * Dimensionless'Model_Epsilon * mm;
      begin
         T.Assert
           (not Canonicalize_And_Validate_Jet (Invalid_Jet, Short_Chord, Canonical)
            and then Canonical = Scaled_Tangent_Jet'[others => [others => 0.0]],
            "Physical jet validity is not hidden by a short chord's scaled tolerance floor");
      end;

      Request := Valid;
      Request.Start.Point (X_Axis) := Length'Last;
      Assert_Result_Kind
        (Request,
         Blend_Invalid_Start_Point,
         "Unsafe start point",
         T);

      Request := Valid;
      Request.Finish.Point (X_Axis) := Length'Last;
      Assert_Result_Kind
        (Request,
         Blend_Invalid_Finish_Point,
         "Unsafe finish point",
         T);

      Request := Valid;
      Request.Finish.Point := Request.Start.Point;
      Assert_Result_Kind
        (Request,
         Blend_Endpoints_Too_Close,
         "Coincident endpoints",
         T);

      Request := Valid;
      Request.Maximum_Position_Error := 0.0 * mm;
      Assert_Result_Kind
        (Request,
         Blend_Invalid_Position_Error,
         "Zero position-error budget",
         T);

      Request := Valid;
      Request.Maximum_Position_Error := -1.0 * mm;
      Assert_Result_Kind
        (Request,
         Blend_Invalid_Position_Error,
         "Negative position-error budget",
         T);

      Request := Valid;
      Request.Maximum_Arc_Length := 9.0 * mm;
      Assert_Result_Kind
        (Request,
         Blend_Invalid_Arc_Length_Limit,
         "Arc limit shorter than the chord",
         T);

      Assert_Result_Kind
        (Default_Request,
         Blend_Endpoints_Too_Close,
         "Default request follows validation precedence",
         T);
   end Test_Request_Validation;

   procedure Test_Straight_Line
     (T : in out Trendy_Test.Operation'Class)
   is
      Start : constant Position :=
        Make_Point
          (2.0 * mm,
           -3.0 * mm,
           4.0 * mm,
           5.0 * mm);
      Finish : constant Position :=
        Make_Point
          (14.0 * mm,
           -3.0 * mm,
           4.0 * mm,
           5.0 * mm);
      Request : constant Blend_Request :=
        Make_Request
          (Start                  => Start,
           Finish                 => Finish,
           Start_Tangent          => X_Unit,
           Finish_Tangent         => X_Unit,
           Maximum_Position_Error => 1.0E-9 * mm,
           Maximum_Arc_Length     => 20.0 * mm);
      Result : constant Blend_Result := Create_Blend (Request);
   begin
      T.Register;
      T.Assert
        (Result.Kind = Blend_Success,
         "Straight construction succeeds: " & Result.Kind'Image);
      if Result.Kind /= Blend_Success then
         return;
      end if;

      declare
         Curve     : constant Stereographic_Curve := Result.Curve;
         Evaluator : constant Stereographic_Curve_Evaluator :=
           To_Evaluator (Curve);
         Bounds : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (Curve);
      begin
         Assert_Length_Close
           (Arc_Length (Curve),
            12.0 * mm,
            1.0E-12 * mm,
            "Straight arc length",
            T);
         T.Assert
           (Arc_Length (Evaluator) = Arc_Length (Curve),
            "Straight evaluator retains the exact length");
         T.Assert
           (Evaluator.Antiderivative_Cache.Real_Pole_Count = 0
            and then
              Evaluator.Antiderivative_Cache.Pair_Count = 0,
            "Straight motion uses the general degree-zero rational form");
         T.Assert
           (Evaluator.Antiderivative_Cache.Constant_Tangent
              = Dimensionless_Axis_Vector'
                  [X_Axis => 1.0, others => 0.0],
            "Straight rational cache stores the constant unit tangent");
         T.Assert
           (Position_Error_Bound (Curve) > 0.0 * mm
            and then
              Position_Error_Bound (Curve)
              <= Request.Maximum_Position_Error,
            "Straight evaluator includes public arithmetic roundoff in its certificate");

         declare
            Poisoned_Evaluator : Stereographic_Curve_Evaluator := Evaluator;
            Interior : Position;
         begin
            --  Finish_Point participates through the endpoint-flat correction, rather than through a finish-only
            --  snap. Deliberately changing a constant-axis finish therefore produces a continuous interior change.
            Poisoned_Evaluator.Finish_Point (Y_Axis) := 7.0 * mm;
            Interior :=
              Evaluate_Rational_Point
                (Poisoned_Evaluator, 0.5);
            T.Assert
              (Interior (Y_Axis) > Start (Y_Axis) and then Interior (Y_Axis) < 7.0 * mm,
               "Endpoint-anchor correction acts continuously in the straight interior");
         end;

         for Sample in 0 .. 24 loop
            declare
               Parameter : constant Curve_Parameter :=
                 Curve_Parameter (Dimensionless (Sample) / 24.0);
               Expected : constant Position :=
                 Start + X_Unit * (12.0 * mm * Parameter);
               Curve_Point : constant Position :=
                 Point_At_Parameter (Curve, Parameter);
            begin
               Assert_Point_Close
                 (Curve_Point,
                  Expected,
                  1.0E-12 * mm,
                  "Straight parameter evaluation",
                  T);
               T.Assert
                 (Point_At_Parameter (Evaluator, Parameter)
                    = Curve_Point,
                  "Straight retained evaluator is bit-identical");
               T.Assert
                 (Point_At_Distance
                    (Curve, Arc_Length (Curve) * Parameter)
                    = Curve_Point,
                  "Straight distance and parameter forms agree");
               T.Assert
                 ((for all Axis in Y_Axis .. E_Axis => Curve_Point (Axis) = Start (Axis)),
                  "Straight constant axes remain bit-exact in the barycentric interior");
            end;
         end loop;

         T.Assert
           (not Axis_Is_Structurally_Constant (Curve, X_Axis),
            "Straight travel axis is not structurally constant");
         for Axis in Y_Axis .. E_Axis loop
            T.Assert
              (Axis_Is_Structurally_Constant (Curve, Axis),
               "Straight transverse axis is structurally constant");
         end loop;

         T.Assert
           (abs (Bounds.Velocity (X_Axis) - 1.0) <= 1.0E-12,
            "Straight travel velocity bound is one");
         for Axis in Y_Axis .. E_Axis loop
            T.Assert
              (Bounds.Velocity (Axis) = 0.0,
               "Straight transverse velocity bound is zero");
         end loop;
         for Axis in Axis_Name loop
            T.Assert
              (Bounds.Acceleration (Axis) = 0.0 / mm,
               "Straight acceleration bound is zero");
            T.Assert
              (Bounds.Jerk (Axis) = 0.0 / mm ** 2,
               "Straight jerk bound is zero");
            T.Assert
              (Bounds.Snap (Axis) = 0.0 / mm ** 3,
               "Straight snap bound is zero");
            T.Assert
              (Bounds.Crackle (Axis) = 0.0 / mm ** 4,
               "Straight crackle bound is zero");
         end loop;
      end;

      declare
         Large_Start : constant Position :=
           Make_Point
             (1.0E9 * mm,
              -1.0E9 * mm,
              4.0 * mm,
              5.0 * mm);
         Large_Finish : constant Position :=
           Make_Point
             (1.0E9 * mm + 12.0 * mm,
              -1.0E9 * mm,
              4.0 * mm,
              5.0 * mm);
         Large_Request : constant Blend_Request :=
           Make_Request
             (Start                  => Large_Start,
              Finish                 => Large_Finish,
              Start_Tangent          => X_Unit,
              Finish_Tangent         => X_Unit,
              Maximum_Position_Error => 1.0E-2 * mm,
              Maximum_Arc_Length     => 20.0 * mm);
         Large_Result : constant Blend_Result :=
           Create_Blend (Large_Request);
      begin
         T.Assert
           (Large_Result.Kind = Blend_Success,
            "Large-coordinate straight construction succeeds: "
            & Large_Result.Kind'Image);
         if Large_Result.Kind = Blend_Success then
            T.Assert
              (Position_Error_Bound (Large_Result.Curve)
                 > Position_Error_Bound (Result.Curve),
               "Coordinate translation increases the certified public roundoff");
            T.Assert
              (Position_Error_Bound (Large_Result.Curve)
                 <= Large_Request.Maximum_Position_Error,
               "Large-coordinate roundoff remains inside the requested budget");
            Assert_Certificate_Covers_Dense_Samples
              (Large_Result.Curve,
               "Translated straight curve",
               T);
         end if;
      end;
   end Test_Straight_Line;

   procedure Test_Quarter_Turn_And_Evaluator
     (T : in out Trendy_Test.Operation'Class)
   is
      Request : constant Blend_Request := Quarter_Turn_Request;
      First   : constant Blend_Result := Create_Blend (Request);
      Second  : constant Blend_Result := Create_Blend (Request);
   begin
      T.Register;
      T.Assert
        (First.Kind = Blend_Success,
         "Quarter turn constructs: " & First.Kind'Image);
      T.Assert
        (Second.Kind = First.Kind,
         "Repeated quarter turn returns the same result kind");
      if First.Kind /= Blend_Success
        or else Second.Kind /= Blend_Success
      then
         return;
      end if;

      declare
         Curve     : constant Stereographic_Curve := First.Curve;
         Evaluator : constant Stereographic_Curve_Evaluator :=
           To_Evaluator (Curve);
         Total : constant Length := Arc_Length (Curve);
         Chord : constant Length :=
           abs (Request.Finish.Point - Request.Start.Point);
      begin
         T.Assert
           (Total > Chord,
            "Quarter-turn length exceeds its chord");
         T.Assert
           (Total <= Request.Maximum_Arc_Length,
            "Quarter-turn length respects its limit");
         T.Assert
           (Arc_Length (Evaluator) = Total,
            "Retained evaluator preserves arc length");
         T.Assert
           (Rational_Antiderivative_Is_Well_Formed
              (Evaluator.Antiderivative_Cache),
            "Curved evaluator contains a well-formed rational antiderivative");
         T.Assert
           (Evaluator.Antiderivative_Cache.Real_Pole_Count
              + 2 * Evaluator.Antiderivative_Cache.Pair_Count
              in 1 .. Maximum_Rational_Degree,
            "Rational antiderivative fits its fixed degree capacity");
         T.Assert
           (Stereographic_Curve_Evaluator'Size < Stereographic_Curve'Size,
            "Retained evaluator is smaller than the construction curve");
         T.Assert
           (Position_Error_Bound (Curve) > 0.0 * mm
            and then
              Position_Error_Bound (Curve)
              <= Request.Maximum_Position_Error,
            "Rational evaluator carries an error bound inside the requested budget");
         Assert_Certificate_Covers_Dense_Samples
           (Curve, "Quarter turn", T);

         for Index in 1 ..
           Evaluator.Antiderivative_Cache.Real_Pole_Count
         loop
            T.Assert
              (Evaluator.Antiderivative_Cache.Pole_Slots (Index)
                 .Pole_Component not in 0.0 .. 1.0,
               "Every real pole is outside the evaluation interval");
         end loop;
         for Pair in 1 ..
           Evaluator.Antiderivative_Cache.Pair_Count
         loop
            T.Assert
              (Evaluator.Antiderivative_Cache.Pole_Slots
                 (Complex_Pair_Imaginary_Slot
                    (Evaluator.Antiderivative_Cache,
                     Rational_Pair_Index (Pair)))
                 .Pole_Component > 0.0,
               "Every conjugate pair stores a positive imaginary pole");
         end loop;

         Assert_Point_Close
           (Point_At_Parameter (Curve, 0.0),
            Request.Start.Point,
            0.0 * mm,
            "Exact parameter start",
            T);
         Assert_Point_Close
           (Point_At_Parameter (Curve, 1.0),
            Request.Finish.Point,
            0.0 * mm,
            "Exact parameter finish",
            T);
         Assert_Point_Close
           (Point_At_Distance (Curve, 0.0 * mm),
            Request.Start.Point,
            0.0 * mm,
            "Exact distance start",
            T);
         Assert_Point_Close
           (Point_At_Distance (Evaluator, Total),
            Request.Finish.Point,
            0.0 * mm,
            "Exact evaluator finish",
            T);
         declare
            Raw_Displacement : constant Dimensionless_Axis_Vector :=
              Evaluate_Rational_Displacement
                (Evaluator.Antiderivative_Cache, 1.0);
            Raw_Finish : constant Position :=
              [for Axis in Axis_Name =>
                 Evaluator.Start_Point (Axis)
                 + Evaluator.Length_Value
                   * Raw_Displacement (Axis)];
         begin
            T.Assert
              (Raw_Finish /= Request.Finish.Point,
               "Quarter-turn raw partial fractions retain a nonzero endpoint residual");
            T.Assert
              (Evaluate_Rational_Point (Evaluator, 1.0)
                 = Request.Finish.Point,
               "Endpoint correction makes the continuous evaluator finish exact");
            T.Assert
              (Evaluator.Uncorrected_Finish_Point /= Evaluator.Finish_Point,
               "Quarter-turn evaluator stores the uncorrected endpoint anchor");
         end;

         for Sample in 0 .. 128 loop
            declare
               Parameter : constant Curve_Parameter :=
                 Curve_Parameter (Dimensionless (Sample) / 128.0);
               Distance : constant Length := Total * Parameter;
               Curve_Point : constant Position :=
                 Point_At_Parameter (Curve, Parameter);
               Ideal_Point : constant Position :=
                 Ideal_Point_At_Parameter (Curve, Parameter);
               Ideal_Tangent : constant Position_Scale :=
                 Tangent_At
                   (Curve.Frame,
                    Curve.Coefficients,
                    Curve.Warp_Factor,
                    Dimensionless (Parameter));
               Reconstructed_Tangent : constant Position_Scale :=
                 Cached_Tangent
                   (Evaluator.Antiderivative_Cache,
                    Dimensionless (Parameter));
               Retained_Tangent : constant Position_Scale :=
                 Position_Scale
                   (Executed_Tangent_Derivative
                      (Evaluator, Dimensionless (Parameter), 0));
            begin
               T.Assert
                 (Point_At_Parameter (Evaluator, Parameter)
                    = Curve_Point,
                  "Retained evaluator preserves parameter points");
               T.Assert
                 (Point_At_Distance (Curve, Distance)
                    = Point_At_Distance (Evaluator, Distance),
                  "Curve and retained evaluator preserve distance points");
               Assert_Point_Close
                 (Point_At_Distance (Curve, Distance),
                  Curve_Point,
                  Point_Tolerance,
                  "Distance and parameter evaluation agree",
                  T);
               T.Assert
                 (Point_At_Parameter
                    (Second.Curve, Parameter) = Curve_Point,
                  "Repeated construction gives deterministic points");
               T.Assert
                 (Vector_Distance
                    (Reconstructed_Tangent, Ideal_Tangent)
                    <= 1.0E-7,
                  "Partial fractions reconstruct the ideal tangent");
               T.Assert
                 (Vector_Distance (Retained_Tangent, Ideal_Tangent)
                    <= Retained_Tangent_Error_Bound (Curve) + 128.0 * Dimensionless'Model_Epsilon,
                  "Public retained-tangent certificate covers the executed mathematical tangent");
               T.Assert
                 (abs (Curve_Point - Ideal_Point)
                    <= Position_Error_Bound (Curve) + 1.0E-10 * mm,
                  "Ideal-to-realtime error is inside the global certificate");
            end;
         end loop;

         Assert_Point_Close
           (Point_At_Distance
              (Evaluator, Length'Adjacent (0.0 * mm, Total)),
            Request.Start.Point,
            Request.Maximum_Position_Error + 1.0E-9 * mm,
            "First interior value is continuous with the start",
            T);
         declare
            Delta_U : constant Dimensionless := 2.0 ** (-44);
            Interior_Distance : constant Length :=
              Total * (1.0 - Delta_U);
            Interior_Point : constant Position :=
              Point_At_Distance (Evaluator, Interior_Distance);
            Last_Bounds : constant Unit_Speed_Axial_Derivative_Bounds :=
              Derivative_Bounds
                (Curve, Interior_Distance, Total);
         begin
            for Axis in Axis_Name loop
               declare
                  Rounding_Allowance : constant Length :=
                    128.0
                    * Dimensionless'Model_Epsilon
                    * (abs Request.Finish.Point (Axis)
                       + Total
                       + 1.0 * mm);
                  Derivative_Allowance : constant Length :=
                    1.05
                    * Last_Bounds.Velocity (Axis)
                    * Total
                    * Delta_U;
               begin
                  T.Assert
                    (abs
                       (Request.Finish.Point (Axis)
                        - Interior_Point (Axis))
                       <= Derivative_Allowance
                          + Rounding_Allowance,
                     "Corrected evaluator is continuous at the finish on "
                     & Axis'Image);
               end;
            end loop;
         end;

         T.Assert
           (Axis_Is_Structurally_Constant (Curve, Z_Axis),
            "Unused Z axis remains structurally constant");
         T.Assert
           (Axis_Is_Structurally_Constant (Curve, E_Axis),
            "Unused E axis remains structurally constant");
         declare
            Interior : constant Position := Point_At_Parameter (Evaluator, 0.37);
         begin
            T.Assert
              (Interior (Z_Axis) = Request.Start.Point (Z_Axis)
               and then Interior (E_Axis) = Request.Start.Point (E_Axis),
               "Zero-correction axes remain bit-exact in the barycentric interior");
         end;
      end;
   end Test_Quarter_Turn_And_Evaluator;

   procedure Test_Rounded_Quarter_Turn_Endpoint
     (T : in out Trendy_Test.Operation'Class)
   is
      Finish_X_Residue       : constant Length := 6.123_234E-16 * mm;
      Finish_Tangent_Residue : constant Dimensionless := 6.123_234E-17;

      procedure Check
        (Finish_Tangent : Position_Scale;
         Name           : String);

      procedure Check
        (Finish_Tangent : Position_Scale;
         Name           : String)
      is
         Request : constant Blend_Request :=
           Make_Request
             (Start                  =>
                Make_Point
                  (-10.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
              Finish                 =>
                Make_Point
                  (Finish_X_Residue, 10.0 * mm, 0.0 * mm, 0.0 * mm),
              Start_Tangent          => X_Unit,
              Finish_Tangent         => Finish_Tangent,
              Maximum_Position_Error => 1.0E-8 * mm,
              Maximum_Arc_Length     => 20.0 * mm);
         Result  : constant Blend_Result := Create_Blend (Request);
      begin
         T.Assert
           (Result.Kind = Blend_Success,
            Name & " constructs despite the rounded finish coordinate: " & Result.Kind'Image);
         if Result.Kind /= Blend_Success then
            return;
         end if;

         declare
            Evaluator : constant Stereographic_Curve_Evaluator := To_Evaluator (Result.Curve);
            Total     : constant Length := Arc_Length (Result.Curve);
         begin
            T.Assert
              (Position_Error_Bound (Result.Curve) <= Request.Maximum_Position_Error,
               Name & " remains inside the requested representation-error budget");
            T.Assert
              (Evaluate_Rational_Point (Evaluator, 1.0) = Request.Finish.Point,
               Name & " continuous rational formula reaches the exact finish");
            T.Assert
              (Point_At_Parameter (Result.Curve, 1.0) = Request.Finish.Point,
               Name & " parameter evaluation reaches the exact finish");
            T.Assert
              (Point_At_Distance (Evaluator, Total) = Request.Finish.Point,
               Name & " distance evaluation reaches the exact finish");
         end;
      end Check;
   begin
      T.Register;
      Check (Y_Unit, "Position residue only");
      Check
        ([X_Axis => Finish_Tangent_Residue, Y_Axis => 1.0, others => 0.0],
         "Position and tangent residues");
   end Test_Rounded_Quarter_Turn_Endpoint;

   --  Verify that endpoint-plane residuals attributable to rounding adjacent large stored coordinates are
   --  canonicalized away, while a resolvable physical displacement normal to that plane is still rejected.
   procedure Test_Planar_Roundoff_Canonicalization
     (T : in out Trendy_Test.Operation'Class)
   is
      --  Subtracting adjacent large accumulated E coordinates across very short planar trims leaves a large normalized
      --  normal residue even though its physical magnitude is below one stored-coordinate ULP.
      Trim                 : constant Length := 5.0E-5 * mm;
      Accumulated_E        : constant Length := 1_000.0 * mm;
      Adjacent_E           : constant Length := Length'Adjacent (Accumulated_E, Length'Last);
      Normal_Residue       : constant Length := Adjacent_E - Accumulated_E;
      Request              : constant Blend_Request :=
        Make_Request
          (Start                  => Make_Point (-Trim, 0.0 * mm, 0.0 * mm, Accumulated_E),
           Finish                 => Make_Point (0.0 * mm, Trim, 0.0 * mm, Adjacent_E),
           Start_Tangent          => X_Unit,
           Finish_Tangent         => Y_Unit,
           Maximum_Position_Error => 1.0E-8 * mm,
           Maximum_Arc_Length     => 2.0 * Trim);
      Chord                : constant Position_Offset := Request.Finish.Point - Request.Start.Point;
      Chord_Length         : constant Length := abs Chord;
      Chord_Direction      : constant Position_Scale := Chord / Chord_Length;
      Normal_Fraction      : constant Dimensionless := Dimensionless (abs Normal_Residue / abs Chord);
      Warp                 : constant Warp_Selection :=
        Select_Distance_Warp
          (Request,
           Chord_Direction,
           Request.Start.Jet.Tangent,
           Request.Finish.Jet.Tangent,
           Dimensionless (Request.Maximum_Arc_Length / Chord_Length));
      Result               : constant Blend_Result := Create_Blend (Request);
      Excessive_Residual_Request : constant Blend_Request :=
        Make_Request
          (Start                  => Make_Point (-Trim, 0.0 * mm, 0.0 * mm, Accumulated_E),
           Finish                 => Make_Point (0.0 * mm, Trim, 0.0 * mm, Accumulated_E + 1.0E-5 * mm),
           Start_Tangent          => X_Unit,
           Finish_Tangent         => Y_Unit,
           Maximum_Position_Error => 1.0E-3 * mm,
           Maximum_Arc_Length     => 4.0 * Trim);
      Excessive_Chord           : constant Position_Offset :=
        Excessive_Residual_Request.Finish.Point - Excessive_Residual_Request.Start.Point;
      Excessive_Chord_Length    : constant Length := abs Excessive_Chord;
      Excessive_Warp            : constant Warp_Selection :=
        Select_Distance_Warp
          (Excessive_Residual_Request,
           Excessive_Chord / Excessive_Chord_Length,
           Excessive_Residual_Request.Start.Jet.Tangent,
           Excessive_Residual_Request.Finish.Jet.Tangent,
           Dimensionless (Excessive_Residual_Request.Maximum_Arc_Length / Excessive_Chord_Length));
      Excessive_Residual_Result : constant Blend_Result := Create_Blend (Excessive_Residual_Request);
   begin
      T.Register;
      T.Assert
        (Normal_Fraction > 2.0E-12 and then Normal_Fraction < 1.0E-8,
         "Roundoff-planar fixture exceeds every fixed normalized plane tolerance used previously: "
         & Normal_Fraction'Image);
      T.Assert
        (Warp.Status = Warp_Was_Selected and then Warp.Seed.Valid,
         "Flat seed selection tolerates structural endpoint-plane cancellation");
      T.Assert
        (Excessive_Warp.Status = Warp_Was_Selected and then not Excessive_Warp.Seed.Valid,
         "Resolvable nonplanarity does not receive an exact zero-bubble closure seed");
      T.Assert
        (Result.Kind = Blend_Success,
         "Roundoff-planar flat request constructs: " & Result.Kind'Image);
      T.Assert
        (Excessive_Residual_Result.Kind /= Blend_Success,
         "Resolvable nonplanarity is not absorbed as stored-coordinate cancellation: "
         & Excessive_Residual_Result.Kind'Image);
      if Result.Kind /= Blend_Success then
         return;
      end if;

      declare
         Curve     : constant Stereographic_Curve := Result.Curve;
         Evaluator : constant Stereographic_Curve_Evaluator := To_Evaluator (Curve);
         Total     : constant Length := Arc_Length (Curve);
         Bounds    : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (Curve, 0.0 * mm, Total);
      begin
         T.Assert
           ((for all C in 2 .. 3 =>
               (for all K in Chart_Coefficient_Index => Curve.Coefficients (K, C) = 0.0)),
            "Roundoff-sized normal chart components are canonicalized exactly");
         T.Assert
           (Position_Error_Bound (Curve) <= Request.Maximum_Position_Error,
            "Endpoint correction remains inside the requested representation-error budget");
         T.Assert
           (Point_At_Parameter (Curve, 0.0) = Request.Start.Point
            and then Point_At_Parameter (Curve, 1.0) = Request.Finish.Point,
            "Canonicalized curve preserves exact public endpoints");
         Assert_Certificate_Covers_Dense_Samples
           (Curve, "Roundoff-planar canonicalization", T);
         Assert_Bounds_Nonnegative (Bounds, "Roundoff-planar canonicalization", T);
         T.Assert
           (Evaluator.Uncorrected_Finish_Point (E_Axis) = Accumulated_E
            and then Evaluate_Rational_Point (Evaluator, 1.0) = Request.Finish.Point,
            "The continuous endpoint correction carries the adjacent E coordinate");
         T.Assert
           (not Axis_Is_Structurally_Constant (Curve, E_Axis)
            and then Bounds.Velocity (E_Axis) > 0.0,
            "The adjacent E correction is retained in the executed derivative bounds");
         T.Assert
           ((for all Index in 0 .. Curve.Retained_Tangent_Certificate.Degree =>
               Curve.Retained_Tangent_Certificate.Axis_Numerators (E_Axis) (Index).Lower = 0.0
               and then Curve.Retained_Tangent_Certificate.Axis_Numerators (E_Axis) (Index).Upper = 0.0),
            "The retained rational E tangent is zero before endpoint correction");
         Assert_Bernstein_Derivative_Bounds
           (Curve,
            0.20,
            0.80,
            "Endpoint-correction-only Bernstein range",
            T);
         T.Assert
           (Axis_Is_Structurally_Constant (Curve, Z_Axis)
            and then Bounds.Velocity (Z_Axis) = 0.0
            and then Bounds.Acceleration (Z_Axis) = 0.0 / mm
            and then Bounds.Jerk (Z_Axis) = 0.0 / mm ** 2
            and then Bounds.Snap (Z_Axis) = 0.0 / mm ** 3
            and then Bounds.Crackle (Z_Axis) = 0.0 / mm ** 4,
            "The untouched Z axis retains exact-zero executed derivative bounds");
      end;
   end Test_Planar_Roundoff_Canonicalization;

   procedure Test_Non_Chord_Flat_Tangents
     (T : in out Trendy_Test.Operation'Class)
   is
      Request : constant Blend_Request :=
        Make_Request
          (Start                  => Origin,
           Finish                 =>
             Make_Point
               (10.0 * mm, 1.0 * mm, 0.0 * mm, 0.0 * mm),
           Start_Tangent          => X_Unit,
           Finish_Tangent         => X_Unit,
           Maximum_Position_Error => 1.0E-3 * mm,
           Maximum_Arc_Length     => 20.0 * mm,
           Allow_Bulge            => True);
      Result : constant Blend_Result := Create_Blend (Request);
   begin
      T.Register;
      T.Assert
        (Result.Kind = Blend_Success,
         "Equal flat non-chord tangents use generic closure: "
         & Result.Kind'Image);
      if Result.Kind /= Blend_Success then
         return;
      end if;

      Assert_Point_Close
        (Point_At_Parameter (Result.Curve, 0.0),
         Request.Start.Point,
         0.0 * mm,
         "Non-chord flat start",
         T);
      Assert_Point_Close
        (Point_At_Parameter (Result.Curve, 1.0),
         Request.Finish.Point,
         0.0 * mm,
         "Non-chord flat finish",
         T);
      T.Assert
        (Arc_Length (Result.Curve)
           > abs (Request.Finish.Point - Request.Start.Point),
         "Non-chord flat curve is not replaced by a straight shortcut");
      T.Assert
        (not Axis_Is_Structurally_Constant
           (Result.Curve, Y_Axis),
         "Generic closure changes the transverse axis");
   end Test_Non_Chord_Flat_Tangents;

   procedure Test_Nonzero_Jets_And_Bounds
     (T : in out Trendy_Test.Operation'Class)
   is
      Request : constant Blend_Request :=
        Circular_Quarter_Turn_Request;
      Result : constant Blend_Result := Create_Blend (Request);
   begin
      T.Register;

      T.Assert
        (Satisfies_Unit_Tangent_Identities
           (Request.Start.Jet.Tangent,
            Request.Start.Jet.Tangent_Derivative_1,
            Request.Start.Jet.Tangent_Derivative_2,
            Request.Start.Jet.Tangent_Derivative_3),
         "Circular start jet satisfies the unit-tangent identities");
      T.Assert
        (Satisfies_Unit_Tangent_Identities
           (Request.Finish.Jet.Tangent,
            Request.Finish.Jet.Tangent_Derivative_1,
            Request.Finish.Jet.Tangent_Derivative_2,
            Request.Finish.Jet.Tangent_Derivative_3),
         "Circular finish jet satisfies the unit-tangent identities");

      T.Assert
        (Result.Kind = Blend_Success,
         "Nonzero endpoint jets construct: " & Result.Kind'Image);
      if Result.Kind /= Blend_Success then
         return;
      end if;

      declare
         Curve : constant Stereographic_Curve := Result.Curve;
         Total : constant Length := Arc_Length (Curve);
         Whole : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (Curve);
         At_Start : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (Curve, 0.0 * mm, 0.0 * mm);
         At_Finish : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (Curve, Total, Total);

         function Expected_Normalized_Derivative
           (Jet : Endpoint_Tangent_Jet; Axis : Axis_Name; Order : Natural) return Dimensionless;

         function Expected_Normalized_Derivative
           (Jet : Endpoint_Tangent_Jet; Axis : Axis_Name; Order : Natural) return Dimensionless
         is
         begin
            case Order is
               when 0 =>
                  return Jet.Tangent (Axis);
               when 1 =>
                  return Dimensionless (Jet.Tangent_Derivative_1 (Axis) * Total);
               when 2 =>
                  return Dimensionless ((Jet.Tangent_Derivative_2 (Axis) * Total) * Total);
               when 3 =>
                  return Dimensionless (((Jet.Tangent_Derivative_3 (Axis) * Total) * Total) * Total);
               when others =>
                  raise Program_Error;
            end case;
         end Expected_Normalized_Derivative;
      begin
         T.Assert
           (Total > 15.0 * mm and then Total < 17.0 * mm,
            "Circular endpoint data produces a quarter-circle-sized length");
         Assert_Bounds_Nonnegative (Whole, "Whole curve", T);
         Assert_Bounds_Nonnegative (At_Start, "Start point", T);
         Assert_Bounds_Nonnegative (At_Finish, "Finish point", T);
         Assert_Executed_Derivative_Bounds
           (Curve,
            Whole,
            0.0 * mm,
            Total,
            "Whole circular curve",
            T);
         Assert_Executed_Derivative_Bounds
           (Curve,
            At_Start,
            0.0 * mm,
            0.0 * mm,
            "Circular start point",
            T);
         Assert_Executed_Derivative_Bounds
           (Curve,
            At_Finish,
            Total,
            Total,
            "Circular finish point",
            T);

         for Order in 0 .. 3 loop
            declare
               Start_Actual  : constant Dimensionless_Axis_Vector :=
                 Executed_Tangent_Derivative
                   (Curve.Evaluator_Data, 0.0, Majorant_Order (Order));
               Finish_Actual : constant Dimensionless_Axis_Vector :=
                 Executed_Tangent_Derivative
                   (Curve.Evaluator_Data, 1.0, Majorant_Order (Order));
               Start_Expected : constant Position_Scale :=
                 [for Axis in Axis_Name => Expected_Normalized_Derivative (Request.Start.Jet, Axis, Order)];
               Finish_Expected : constant Position_Scale :=
                 [for Axis in Axis_Name => Expected_Normalized_Derivative (Request.Finish.Jet, Axis, Order)];
               Certified_Error : constant Dimensionless :=
                 Retained_Endpoint_Jet_Error_Bound (Curve, Endpoint_Tangent_Derivative_Order (Order));
            begin
               T.Assert
                 (Vector_Distance (Position_Scale (Start_Actual), Start_Expected) <= Certified_Error,
                  "Retained start jet order" & Natural'Image (Order) & " is covered by its certificate");
               T.Assert
                 (Vector_Distance (Position_Scale (Finish_Actual), Finish_Expected) <= Certified_Error,
                  "Retained finish jet order" & Natural'Image (Order) & " is covered by its certificate");
            end;
         end loop;

         for Axis in Axis_Name loop
            T.Assert
              (At_Start.Velocity (Axis) + 1.0E-10
                 >= abs Request.Start.Jet.Tangent (Axis),
               "Start velocity bound contains requested tangent");
            T.Assert
              (At_Start.Acceleration (Axis) + 1.0E-10 / mm
                 >= abs Request.Start.Jet.Tangent_Derivative_1 (Axis),
               "Start acceleration bound contains requested first derivative");
            T.Assert
              (At_Start.Jerk (Axis) + 1.0E-10 / mm ** 2
                 >= abs Request.Start.Jet.Tangent_Derivative_2 (Axis),
               "Start jerk bound contains requested second derivative");
            T.Assert
              (At_Start.Snap (Axis) + 1.0E-10 / mm ** 3
                 >= abs Request.Start.Jet.Tangent_Derivative_3 (Axis),
               "Start snap bound contains requested third derivative");

            T.Assert
              (At_Finish.Velocity (Axis) + 1.0E-10
                 >= abs Request.Finish.Jet.Tangent (Axis),
               "Finish velocity bound contains requested tangent");
            T.Assert
              (At_Finish.Acceleration (Axis) + 1.0E-10 / mm
                 >= abs Request.Finish.Jet.Tangent_Derivative_1 (Axis),
               "Finish acceleration bound contains requested first derivative");
            T.Assert
              (At_Finish.Jerk (Axis) + 1.0E-10 / mm ** 2
                 >= abs Request.Finish.Jet.Tangent_Derivative_2 (Axis),
               "Finish jerk bound contains requested second derivative");
            T.Assert
              (At_Finish.Snap (Axis) + 1.0E-10 / mm ** 3
                 >= abs Request.Finish.Jet.Tangent_Derivative_3 (Axis),
               "Finish snap bound contains requested third derivative");
         end loop;

         for Axis in Z_Axis .. E_Axis loop
            T.Assert
              (Axis_Is_Structurally_Constant (Curve, Axis),
               "Circular unused axis is structurally constant");
            T.Assert
              (Whole.Velocity (Axis) = 0.0
               and then Whole.Acceleration (Axis) = 0.0 / mm
               and then Whole.Jerk (Axis) = 0.0 / mm ** 2
               and then Whole.Snap (Axis) = 0.0 / mm ** 3
               and then Whole.Crackle (Axis) = 0.0 / mm ** 4,
               "Circular unused axis has exact zero bounds");
         end loop;
         Assert_Certificate_Covers_Dense_Samples
           (Curve, "Nonzero-jet quarter turn", T);
      end;
   end Test_Nonzero_Jets_And_Bounds;

   procedure Test_Saturated_Physical_Bounds
     (T : in out Trendy_Test.Operation'Class)
   is
      Result : constant Blend_Result :=
        Create_Blend (Circular_Quarter_Turn_Request);
   begin
      T.Register;
      T.Assert
        (Result.Kind = Blend_Success,
         "Saturated-bound test curve constructs: "
         & Result.Kind'Image);
      if Result.Kind /= Blend_Success then
         return;
      end if;

      declare
         Curve : Stereographic_Curve := Result.Curve;
         Length_Raw : constant Dimensionless :=
           Dimensionless (Arc_Length (Curve) / mm);
         Half_Last : constant Dimensionless :=
           Dimensionless'Last / 2.0;
         Expected : Dimensionless := Half_Last;
         Sentinel_Bounds, Large_Finite_Bounds :
           Unit_Speed_Axial_Derivative_Bounds;
      begin
         T.Assert
           (Length_Raw > 1.0,
            "Saturated-bound test uses a physical length above one");
         T.Assert
           (Curve.Has_Whole_Curve_Majorants,
            "Saturated-bound test curve stores whole-curve majorants");

         Curve.Whole_Curve_Majorants (X_Axis) (4) :=
           Dimensionless'Last;
         Sentinel_Bounds :=
           Bounds_On_Parameter_Range (Curve, 0.0, 1.0);
         T.Assert
           (Sentinel_Bounds.Crackle (X_Axis)
              = Curvature_To_4'Last,
            "An unbounded stored crackle majorant remains an unbounded physical sentinel");

         Curve.Whole_Curve_Majorants (X_Axis) (4) :=
           Half_Last;
         Large_Finite_Bounds :=
           Bounds_On_Parameter_Range (Curve, 0.0, 1.0);

         --  Compute 4! M / L^4 in the same overflow-safe sequence a
         --  mathematical caller can use when L > 1. The former
         --  multiply-before-divide implementation saturated 24*M first and
         --  then divided that sentinel into a finite underbound.
         for Factor in 1 .. 4 loop
            Expected := Expected / Length_Raw;
            Expected := Expected * Dimensionless (Factor);
         end loop;
         declare
            Actual : constant Dimensionless :=
              Dimensionless
                (Large_Finite_Bounds.Crackle (X_Axis)
                 / (1.0 / mm ** 4));
         begin
            T.Assert
              (Large_Finite_Bounds.Crackle (X_Axis)
                 < Curvature_To_4'Last,
               "A large finite crackle majorant remains finite after physical scaling");
            T.Assert
              (Actual >= Expected,
               "Large finite crackle scaling covers staged 24*M/L^4: actual "
               & Actual'Image
               & ", expected "
               & Expected'Image);
         end;
      end;
   end Test_Saturated_Physical_Bounds;

   procedure Test_Ranged_And_Projected_Bounds
     (T : in out Trendy_Test.Operation'Class)
   is
      Request : constant Blend_Request :=
        Circular_Quarter_Turn_Request;
      Result : constant Blend_Result := Create_Blend (Request);
   begin
      T.Register;
      T.Assert
        (Result.Kind = Blend_Success,
         "Bounds construction succeeds: " & Result.Kind'Image);
      if Result.Kind /= Blend_Success then
         return;
      end if;

      declare
         Curve : constant Stereographic_Curve := Result.Curve;
         Total : constant Length := Arc_Length (Curve);
         Whole : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (Curve);
         Full_Range : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (Curve, 0.0 * mm, Total);
         First_Half : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (Curve, 0.0 * mm, 0.5 * Total);
         Middle : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (Curve, 0.25 * Total, 0.75 * Total);
         XY : constant Projection_Coefficients :=
           [X_Axis => 1.0 / mm,
            Y_Axis => 1.0 / mm,
            others => 0.0 / mm];
         Negative_X : constant Projection_Coefficients :=
           [X_Axis => -1.0 / mm, others => 0.0 / mm];
         Constant_Axes : constant Projection_Coefficients :=
           [Z_Axis => 2.0 / mm,
            E_Axis => -3.0 / mm,
            others => 0.0 / mm];
         Whole_XY : constant Curvature :=
           Projected_Tangent_Bound (Curve, XY);
         Start_XY : constant Curvature :=
           Projected_Tangent_Bound
             (Curve, 0.0 * mm, 0.0 * mm, XY);
         Middle_XY : constant Curvature :=
           Projected_Tangent_Bound
             (Curve, 0.25 * Total, 0.75 * Total, XY);
      begin
         T.Assert
           (Full_Range = Whole,
            "Full ranged bounds equal whole-curve bounds");
         Assert_Bounds_Nonnegative (First_Half, "First half", T);
         Assert_Bounds_Nonnegative (Middle, "Middle range", T);
         Assert_Executed_Derivative_Bounds
           (Curve,
            First_Half,
            0.0 * mm,
            0.5 * Total,
            "Circular first half",
            T);
         Assert_Executed_Derivative_Bounds
           (Curve,
            Middle,
            0.25 * Total,
            0.75 * Total,
            "Circular middle range",
            T);

         for Axis in Axis_Name loop
            T.Assert
              (First_Half.Velocity (Axis)
                 <= Whole.Velocity (Axis) + 1.0E-10,
               "First-half velocity bound is no wider than whole");
            T.Assert
              (Middle.Velocity (Axis)
                 <= Whole.Velocity (Axis) + 1.0E-10,
               "Middle velocity bound is no wider than whole");
            T.Assert
              (First_Half.Acceleration (Axis)
                 <= Whole.Acceleration (Axis) * 1.000_000_001
                    + 1.0E-10 / mm,
               "First-half acceleration bound is no wider than whole");
            T.Assert
              (Middle.Jerk (Axis)
                 <= Whole.Jerk (Axis) * 1.000_000_001
                    + 1.0E-10 / mm ** 2,
               "Middle jerk bound is no wider than whole");
            T.Assert
              (First_Half.Snap (Axis)
                 <= Whole.Snap (Axis) * 1.000_000_001
                    + 1.0E-10 / mm ** 3,
               "First-half snap bound is no wider than whole");
            T.Assert
              (Middle.Crackle (Axis)
                 <= Whole.Crackle (Axis) * 1.000_000_001
                    + 1.0E-10 / mm ** 4,
               "Middle crackle bound is no wider than whole");
         end loop;

         T.Assert
           (Whole_XY >= 1.0 / mm,
            "XY projected bound contains both endpoint tangents");
         T.Assert
           (Whole_XY <= 2.0 / mm,
            "XY projected bound remains useful");
         T.Assert
           (Start_XY >= 1.0 / mm,
            "Singleton start XY projection contains the endpoint tangent: "
            & Start_XY'Image);
         T.Assert
           (Start_XY <= 1.05 / mm,
            "Singleton start XY projection uses the requested range: "
            & Start_XY'Image);
         T.Assert
           (Middle_XY <= Whole_XY + 1.0E-10 / mm,
            "Ranged projection is no wider than whole");
         T.Assert
           (Projected_Tangent_Bound
              (Curve, Negative_X) >= 1.0 / mm,
            "Negative coefficient projection is bounded by magnitude");
         T.Assert
           (Projected_Tangent_Bound
              (Curve, Constant_Axes) = 0.0 / mm,
            "Projection on structurally constant axes is exactly zero");
         T.Assert
           (Projected_Tangent_Bound
              (Curve, [others => 0.0 / mm]) = 0.0 / mm,
            "Zero projection coefficients give an exact zero bound");
         T.Assert
           (Projected_Tangent_Bound
              (Curve,
               0.5 * Total,
               0.5 * Total,
               XY) > 0.0 / mm,
            "An equal-distance range denotes a point, not an empty interval");
         declare
            Fallback_Curve : Stereographic_Curve := Curve;
         begin
            --  Disable both retained certificates so this range query must exercise the older pole/Taylor fallback.
            Fallback_Curve.Retained_Tangent_Certificate.Valid := False;
            Fallback_Curve.Has_Whole_Curve_Majorants := False;
            declare
               Start_U : constant Dimensionless := 0.19;
               End_U   : constant Dimensionless := 0.73;
               Fallback_Bounds : constant Unit_Speed_Axial_Derivative_Bounds :=
                 Bounds_On_Parameter_Range (Fallback_Curve, Start_U, End_U);
            begin
               Assert_Executed_Derivative_Bounds
                 (Fallback_Curve,
                  Fallback_Bounds,
                  Start_U * Total,
                  End_U * Total,
                  "Invalid-certificate pole/Taylor fallback",
                  T);
            end;
         end;
      end;
   end Test_Ranged_And_Projected_Bounds;

   procedure Test_Supported_Asymmetric_Corners
     (T : in out Trendy_Test.Operation'Class)
   is
      Long_In_Request : constant Blend_Request :=
        Make_Request
          (Start                  =>
             Make_Point
               (-20.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
           Finish                 =>
             Make_Point
               (0.0 * mm, 1.0 * mm, 0.0 * mm, 0.0 * mm),
           Start_Tangent          => X_Unit,
           Finish_Tangent         => Y_Unit,
           Maximum_Position_Error => 1.0E-3 * mm,
           Maximum_Arc_Length     => 21.0 * mm);
      Long_Out_Request : constant Blend_Request :=
        Make_Request
          (Start                  =>
             Make_Point
               (-1.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
           Finish                 =>
             Make_Point
               (0.0 * mm, 20.0 * mm, 0.0 * mm, 0.0 * mm),
           Start_Tangent          => X_Unit,
           Finish_Tangent         => Y_Unit,
           Maximum_Position_Error => 1.0E-3 * mm,
           Maximum_Arc_Length     => 21.0 * mm);

      function Mirror
        (Point : Position) return Position
      is
        ([X_Axis => -Point (Y_Axis),
          Y_Axis => -Point (X_Axis),
          Z_Axis => Point (Z_Axis),
          E_Axis => Point (E_Axis)]);

      procedure Assert_Corner_Like
        (Curve   : Stereographic_Curve;
         Request : Blend_Request;
         Name    : String)
      is
         Evaluator : constant Stereographic_Curve_Evaluator :=
           To_Evaluator (Curve);
         Total : constant Length := Arc_Length (Curve);
         Previous : Position := Request.Start.Point;
         Geometry_Tolerance : constant Length :=
           Request.Maximum_Position_Error + 1.0E-6 * mm;
      begin
         Assert_Point_Close
           (Point_At_Distance (Curve, 0.0 * mm),
            Request.Start.Point,
            0.0 * mm,
            Name & " exact start",
            T);
         Assert_Point_Close
           (Point_At_Distance (Evaluator, Total),
            Request.Finish.Point,
            0.0 * mm,
            Name & " exact finish",
            T);

         for Sample in 0 .. 128 loop
            declare
               Parameter : constant Curve_Parameter :=
                 Curve_Parameter (Dimensionless (Sample) / 128.0);
               Point : constant Position :=
                 Point_At_Parameter (Curve, Parameter);
            begin
               T.Assert
                 (Point = Point_At_Parameter (Evaluator, Parameter),
                  Name & " retained evaluator is identical");
               T.Assert
                 (Point (X_Axis)
                    >= Request.Start.Point (X_Axis)
                       - Geometry_Tolerance
                  and then Point (X_Axis)
                    <= Request.Finish.Point (X_Axis)
                       + Geometry_Tolerance
                  and then Point (Y_Axis)
                    >= Request.Start.Point (Y_Axis)
                       - Geometry_Tolerance
                  and then Point (Y_Axis)
                    <= Request.Finish.Point (Y_Axis)
                       + Geometry_Tolerance,
                  Name & " stays inside the replaced corner rectangle");
               T.Assert
                 (Point (X_Axis)
                    >= Previous (X_Axis) - Geometry_Tolerance
                  and then Point (Y_Axis)
                    >= Previous (Y_Axis) - Geometry_Tolerance,
                  Name & " makes nonnegative axial progress");
               Previous := Point;
            end;
         end loop;

         T.Assert
           (Axis_Is_Structurally_Constant (Curve, Z_Axis)
            and then Axis_Is_Structurally_Constant (Curve, E_Axis),
            Name & " preserves unused axes");
      end Assert_Corner_Like;

      procedure Assert_Failed_Warp_Seed_Is_Not_Retried
        (Request          : Blend_Request;
         Successful_Curve : Stereographic_Curve)
      is
         Chord : constant Position_Offset :=
           Request.Finish.Point - Request.Start.Point;
         Chord_Length : constant Length := abs Chord;
         Chord_Direction : constant Position_Scale :=
           Chord / Chord_Length;
         Start_Jet, Finish_Jet : Scaled_Tangent_Jet;
         Candidate_Vector : Position_Scale;
         Candidate_Norm : Dimensionless;
         Frame : Frame_Vector_Array;
         Start_Chart, Finish_Chart : Chart_Jet_Array;
         Norm_OK, Frame_OK, Start_OK, Finish_OK : Boolean;
      begin
         Start_OK :=
           Canonicalize_And_Validate_Jet
             (Request.Start.Jet,
              Chord_Length,
              Start_Jet);
         Finish_OK :=
           Canonicalize_And_Validate_Jet
             (Request.Finish.Jet,
              Chord_Length,
              Finish_Jet);
         T.Assert
           (Start_OK and then Finish_OK,
            "No-retry test canonicalizes the endpoint jets");
         if not Start_OK or else not Finish_OK then
            return;
         end if;

         Candidate_Vector := Start_Jet (0) + Finish_Jet (0);
         Candidate_Norm := Safe_Norm (Candidate_Vector, Norm_OK);
         T.Assert
           (Norm_OK and then Candidate_Norm > 0.0,
            "No-retry test forms the first frame direction");
         if not Norm_OK or else Candidate_Norm <= 0.0 then
            return;
         end if;

         Frame_OK :=
           Complete_Frame
             (Candidate_Vector / Candidate_Norm,
              Start_Jet (0),
              Finish_Jet (0),
              Chord_Direction,
              Frame);
         if Frame_OK then
            Start_OK :=
              Chart_From_Canonical_Jet
                (Frame, Start_Jet, Start_Chart);
            Finish_OK :=
              Chart_From_Canonical_Jet
                (Frame, Finish_Jet, Finish_Chart);
         end if;
         T.Assert
           (Frame_OK and then Start_OK and then Finish_OK,
            "No-retry test constructs the first chart frame");
         if not Frame_OK or else not Start_OK or else not Finish_OK then
            return;
         end if;

         declare
            Unseeded : constant Candidate_Result :=
              Build_Candidate
                (Request,
                 Chord_Length,
                 Chord_Direction,
                 Frame,
                 Start_Chart,
                 Finish_Chart,
                 Successful_Curve.Warp_Factor,
                 (others => <>));
            Bad_Seed : constant Closure_Seed :=
              (Valid  => True,
               Lambda =>
                 Request.Maximum_Arc_Length / Chord_Length,
               C0     => [others => 1.0E100]);
            From_Bad_Seed : constant Candidate_Result :=
              Build_Candidate
                (Request,
                 Chord_Length,
                 Chord_Direction,
                 Frame,
                 Start_Chart,
                 Finish_Chart,
                 Successful_Curve.Warp_Factor,
                 Bad_Seed);
         begin
            T.Assert
              (Unseeded.Status = Candidate_Success,
               "The first asymmetric frame has an ordinary closure solution");
            T.Assert
              (From_Bad_Seed.Status /= Candidate_Success,
               "A failed Mobius proposal is not retried from an unseeded start");
         end;
      end Assert_Failed_Warp_Seed_Is_Not_Retried;

      procedure Assert_Extreme_Corner
        (Angle_Degrees          : Dimensionless;
         Outgoing_Over_Incoming : Dimensionless;
         Name                   : String)
      is
         Turn : constant Dimensionless :=
           Angle_Degrees
           * Dimensionless (Ada.Numerics.Pi)
           / 180.0;
         Finish_Tangent : constant Position_Scale :=
           [X_Axis => Dimensionless_Math.Cos (Turn),
            Y_Axis => Dimensionless_Math.Sin (Turn),
            others => 0.0];
         Request : constant Blend_Request :=
           Make_Request
             (Start =>
                Make_Point
                  (-1.0 * mm,
                   0.0 * mm,
                   0.0 * mm,
                   0.0 * mm),
              Finish =>
                Make_Point
                  (Outgoing_Over_Incoming
                     * Dimensionless_Math.Cos (Turn)
                     * mm,
                   Outgoing_Over_Incoming
                     * Dimensionless_Math.Sin (Turn)
                     * mm,
                   0.0 * mm,
                   0.0 * mm),
              Start_Tangent          => X_Unit,
              Finish_Tangent         => Finish_Tangent,
              Maximum_Position_Error => 1.0E-3 * mm,
              Maximum_Arc_Length     =>
                (1.0 + Outgoing_Over_Incoming) * mm);
         Result : constant Blend_Result :=
           Create_Blend (Request);
      begin
         T.Assert
           (Result.Kind = Blend_Success,
            Name & " construction: " & Result.Kind'Image);
         if Result.Kind /= Blend_Success then
            return;
         end if;

         declare
            Endpoint_Dot : constant Dimensionless :=
              Dot (X_Unit, Finish_Tangent);
            Dual_Denominator : constant Dimensionless :=
              1.0 - Endpoint_Dot * Endpoint_Dot;
            Start_Dual : constant Position_Scale :=
              (X_Unit
               - Finish_Tangent * Endpoint_Dot)
              / Dual_Denominator;
            Finish_Dual : constant Position_Scale :=
              (Finish_Tangent
               - X_Unit * Endpoint_Dot)
              / Dual_Denominator;
         begin
            for Sample in 0 .. 256 loop
               declare
                  Parameter : constant Dimensionless :=
                    Dimensionless (Sample) / 256.0;
                  Tangent : constant Position_Scale :=
                    Tangent_At
                      (Result.Curve.Frame,
                       Result.Curve.Coefficients,
                       Result.Curve.Warp_Factor,
                       Parameter);
               begin
                  T.Assert
                    (Dot (Tangent, Start_Dual)
                       >= -1.0E-10,
                     Name
                     & " stays inside the incoming tangent cone");
                  T.Assert
                    (Dot (Tangent, Finish_Dual)
                       >= -1.0E-10,
                     Name
                     & " stays inside the outgoing tangent cone");
               end;
            end loop;
         end;
         Assert_Certificate_Covers_Dense_Samples
           (Result.Curve, Name, T);
      end Assert_Extreme_Corner;

      Long_In  : constant Blend_Result :=
        Create_Blend (Long_In_Request);
      Long_Out : constant Blend_Result :=
        Create_Blend (Long_Out_Request);
   begin
      T.Register;
      T.Assert
        (Long_In.Kind = Blend_Success,
         "Supported 20:1 corner constructs: " & Long_In.Kind'Image);
      T.Assert
        (Long_Out.Kind = Blend_Success,
         "Supported 1:20 corner constructs: " & Long_Out.Kind'Image);
      if Long_In.Kind /= Blend_Success
        or else Long_Out.Kind /= Blend_Success
      then
         return;
      end if;

      T.Assert
        (Long_In.Curve.Warp_Factor < 0.1,
         "20:1 construction retains a strongly asymmetric Mobius warp");
      T.Assert
        (Long_Out.Curve.Warp_Factor > 10.0,
         "1:20 construction retains the reciprocal Mobius warp");
      T.Assert
        (abs
           (Long_In.Curve.Warp_Factor
            * Long_Out.Curve.Warp_Factor
            - 1.0)
           <= 1.0E-8,
         "Mirrored asymmetric warps are reciprocal");
      Assert_Failed_Warp_Seed_Is_Not_Retried
        (Long_In_Request, Long_In.Curve);

      Assert_Length_Close
        (Arc_Length (Long_In.Curve),
         Arc_Length (Long_Out.Curve),
         1.0E-9 * mm,
         "Mirrored asymmetric arc lengths",
         T);
      Assert_Corner_Like
        (Long_In.Curve, Long_In_Request, "20:1 corner");
      Assert_Corner_Like
        (Long_Out.Curve, Long_Out_Request, "1:20 corner");
      Assert_Certificate_Covers_Dense_Samples
        (Long_In.Curve, "20:1 corner", T);
      Assert_Certificate_Covers_Dense_Samples
        (Long_Out.Curve, "1:20 corner", T);
      Assert_Bernstein_Derivative_Bounds
        (Long_In.Curve,
         0.70,
         0.98,
         "20:1 warped-edge Bernstein range",
         T);
      Assert_Bernstein_Derivative_Bounds
        (Long_Out.Curve,
         0.02,
         0.30,
         "1:20 warped-edge Bernstein range",
         T);

      for Sample in 0 .. 128 loop
         declare
            Parameter : constant Curve_Parameter :=
              Curve_Parameter (Dimensionless (Sample) / 128.0);
            Long_In_Point : constant Position :=
              Point_At_Parameter
                (Long_In.Curve,
                 Curve_Parameter (1.0 - Parameter));
            Long_Out_Point : constant Position :=
              Point_At_Parameter (Long_Out.Curve, Parameter);
         begin
            Assert_Point_Close
              (Long_Out_Point,
               Mirror (Long_In_Point),
               2.1E-3 * mm,
               "20:1 and 1:20 geometry mirrors",
               T);
         end;
      end loop;

      Assert_Extreme_Corner
        (120.0, 20.0, "120 degree 20:1 corner");
      Assert_Extreme_Corner
        (165.0, 1.0, "165 degree symmetric corner");
      Assert_Extreme_Corner
        (175.0, 20.0, "175 degree 20:1 corner");
      Assert_Extreme_Corner
        (175.0, 0.05, "175 degree 1:20 corner");
   end Test_Supported_Asymmetric_Corners;

   procedure Test_Geometry_Zero_And_Degenerate_References
     (T : in out Trendy_Test.Operation'Class)
   is
      Curve_Point : constant Position :=
        Make_Point
          (2.0 * mm,
           2.0 * mm,
           0.0 * mm,
           0.0 * mm);
      Curve : constant Stereographic_Curve := Zero_Blend (Curve_Point);
      Query_Point : constant Position :=
        Make_Point
          (5.0 * mm,
           6.0 * mm,
           12.0 * mm,
           0.0 * mm);
      Line_Start : constant Position := Origin;
      Line_Corner : constant Position :=
        Make_Point
          (1.0 * mm,
           0.0 * mm,
           0.0 * mm,
           0.0 * mm);
      Line_Finish : constant Position :=
        Make_Point
          (1.0 * mm,
           1.0 * mm,
           0.0 * mm,
           0.0 * mm);
      Default_Interval : Geometry.Distance_Interval;
      Point_Distance : constant Geometry.Distance_Interval :=
        Geometry.Minimum_Distance_To_Point
          (Curve, Query_Point, 1.0E-6 * mm);
      Line_Deviation : constant Geometry.Distance_Interval :=
        Geometry.Maximum_Deviation_From_Line_Corner
          (Curve,
           Line_Start,
           Line_Corner,
           Line_Finish,
           1.0E-6 * mm);
      Degenerate_Deviation : constant Geometry.Distance_Interval :=
        Geometry.Maximum_Deviation_From_Line_Corner
          (Curve,
           Origin,
           Origin,
           Origin,
           1.0E-6 * mm);
      Coincident_Distance : constant Geometry.Distance_Interval :=
        Geometry.Minimum_Distance_To_Point
          (Curve, Curve_Point, 1.0E-6 * mm);
      Expected_Point_Distance : constant Length := 13.0 * mm;
      Expected_Line_Deviation : constant Length :=
        Dimensionless_Math.Sqrt (2.0) * mm;
      Expected_Degenerate_Deviation : constant Length :=
        Dimensionless_Math.Sqrt (8.0) * mm;
   begin
      T.Register;

      T.Assert
        (Default_Interval.Lower = 0.0 * mm
         and then Default_Interval.Upper = Length'Last,
         "The default geometry interval cannot accidentally certify a limit");

      T.Assert
        (Point_Distance.Lower <= Expected_Point_Distance
         and then Point_Distance.Upper >= Expected_Point_Distance,
         "Zero-curve point distance encloses the exact four-axis norm");
      T.Assert
        (Line_Deviation.Lower <= Expected_Line_Deviation
         and then Line_Deviation.Upper >= Expected_Line_Deviation,
         "Line-corner distance uses finite segments rather than infinite lines");
      T.Assert
        (Degenerate_Deviation.Lower
           <= Expected_Degenerate_Deviation
         and then Degenerate_Deviation.Upper
           >= Expected_Degenerate_Deviation,
         "Degenerate line-corner segments reduce to a point");
      T.Assert
        (Coincident_Distance.Lower = 0.0 * mm
         and then Coincident_Distance.Upper <= Point_Tolerance,
         "A zero curve tightly encloses zero distance to its stored point: "
         & Coincident_Distance.Lower'Image
         & " .. "
         & Coincident_Distance.Upper'Image);
   end Test_Geometry_Zero_And_Degenerate_References;

   procedure Test_Geometry_Straight_And_Quarter_Turn
     (T : in out Trendy_Test.Operation'Class)
   is
      Straight_Start : constant Position := Origin;
      Straight_Corner : constant Position :=
        Make_Point
          (5.0 * mm,
           0.0 * mm,
           0.0 * mm,
           0.0 * mm);
      Straight_Finish : constant Position :=
        Make_Point
          (10.0 * mm,
           0.0 * mm,
           0.0 * mm,
           0.0 * mm);
      Off_Line : constant Position :=
        Make_Point
          (5.0 * mm,
           2.0 * mm,
           0.0 * mm,
           0.0 * mm);
      Straight_Result : constant Blend_Result :=
        Create_Blend
          (Make_Request
             (Start                  => Straight_Start,
              Finish                 => Straight_Finish,
              Start_Tangent          => X_Unit,
              Finish_Tangent         => X_Unit,
              Maximum_Position_Error => 1.0E-7 * mm,
              Maximum_Arc_Length     => 20.0 * mm));
      Turn_Request : constant Blend_Request := Quarter_Turn_Request;
      Turn_Result : constant Blend_Result :=
        Create_Blend (Turn_Request);
   begin
      T.Register;
      T.Assert
        (Straight_Result.Kind = Blend_Success,
         "Straight geometry construction succeeds");
      T.Assert
        (Turn_Result.Kind = Blend_Success,
         "Quarter-turn geometry construction succeeds");
      if Straight_Result.Kind /= Blend_Success
        or else Turn_Result.Kind /= Blend_Success
      then
         return;
      end if;

      declare
         Straight_Deviation : constant Geometry.Distance_Interval :=
           Geometry.Maximum_Deviation_From_Line_Corner
             (Straight_Result.Curve,
              Straight_Start,
              Straight_Corner,
              Straight_Finish,
              1.0E-6 * mm);
         Straight_Distance : constant Geometry.Distance_Interval :=
           Geometry.Minimum_Distance_To_Point
             (Straight_Result.Curve,
              Off_Line,
              1.0E-6 * mm);
         Turn_Corner : constant Position :=
           Make_Point
             (10.0 * mm,
              0.0 * mm,
              0.0 * mm,
              0.0 * mm);
         Tight_Deviation : constant Geometry.Distance_Interval :=
           Geometry.Maximum_Deviation_From_Line_Corner
             (Turn_Result.Curve,
              Turn_Request.Start.Point,
              Turn_Corner,
              Turn_Request.Finish.Point,
              1.0E-4 * mm);
         Tight_Distance : constant Geometry.Distance_Interval :=
           Geometry.Minimum_Distance_To_Point
             (Turn_Result.Curve,
              Turn_Corner,
              1.0E-4 * mm);
         Coarse_Deviation : constant Geometry.Distance_Interval :=
           Geometry.Maximum_Deviation_From_Line_Corner
             (Turn_Result.Curve,
              Turn_Request.Start.Point,
              Turn_Corner,
              Turn_Request.Finish.Point,
              0.25 * mm);
         Coarse_Distance : constant Geometry.Distance_Interval :=
           Geometry.Minimum_Distance_To_Point
             (Turn_Result.Curve,
              Turn_Corner,
              0.25 * mm);
      begin
         T.Assert
           (Straight_Deviation.Lower >= 0.0 * mm
            and then Straight_Deviation.Lower
              <= Straight_Deviation.Upper,
            "Straight deviation interval is valid");
         T.Assert
           (Straight_Deviation.Upper <= 1.0E-6 * mm,
            "A general-representation straight curve stays on its line: "
            & Straight_Deviation.Upper'Image);
         T.Assert
           (Straight_Distance.Lower <= 2.0 * mm
            and then Straight_Distance.Upper >= 2.0 * mm,
            "Straight point-distance interval contains the exact distance");

         Assert_Geometry_Intervals_Cover_Dense_Samples
           (Turn_Result.Curve,
            Turn_Request.Start.Point,
            Turn_Corner,
            Turn_Request.Finish.Point,
            Turn_Corner,
            Tight_Deviation,
            Tight_Distance,
            "Tight quarter-turn geometry",
            T);
         Assert_Geometry_Intervals_Cover_Dense_Samples
           (Turn_Result.Curve,
            Turn_Request.Start.Point,
            Turn_Corner,
            Turn_Request.Finish.Point,
            Turn_Corner,
            Coarse_Deviation,
            Coarse_Distance,
            "Coarse quarter-turn geometry",
            T);

         T.Assert
           (Tight_Deviation.Upper - Tight_Deviation.Lower
              <= 1.0E-3 * mm,
            "The curvature-capsule deviation interval is useful");
         T.Assert
           (Tight_Distance.Upper - Tight_Distance.Lower
              <= 1.0E-3 * mm,
            "The curvature-capsule point-distance interval is useful");
      end;
   end Test_Geometry_Straight_And_Quarter_Turn;

   procedure Test_Geometry_Asymmetric_Mobius_Warps
     (T : in out Trendy_Test.Operation'Class)
   is
      Long_In_Request : constant Blend_Request :=
        Make_Request
          (Start                  =>
             Make_Point
               (-20.0 * mm,
                0.0 * mm,
                0.0 * mm,
                0.0 * mm),
           Finish                 =>
             Make_Point
               (0.0 * mm,
                1.0 * mm,
                0.0 * mm,
                0.0 * mm),
           Start_Tangent          => X_Unit,
           Finish_Tangent         => Y_Unit,
           Maximum_Position_Error => 1.0E-3 * mm,
           Maximum_Arc_Length     => 21.0 * mm);
      Long_Out_Request : constant Blend_Request :=
        Make_Request
          (Start                  =>
             Make_Point
               (-1.0 * mm,
                0.0 * mm,
                0.0 * mm,
                0.0 * mm),
           Finish                 =>
             Make_Point
               (0.0 * mm,
                20.0 * mm,
                0.0 * mm,
                0.0 * mm),
           Start_Tangent          => X_Unit,
           Finish_Tangent         => Y_Unit,
           Maximum_Position_Error => 1.0E-3 * mm,
           Maximum_Arc_Length     => 21.0 * mm);
      Long_In : constant Blend_Result :=
        Create_Blend (Long_In_Request);
      Long_Out : constant Blend_Result :=
        Create_Blend (Long_Out_Request);

      procedure Assert_Asymmetric_Geometry
        (Request : Blend_Request;
         Curve   : Stereographic_Curve;
         Name    : String);

      procedure Assert_Asymmetric_Geometry
        (Request : Blend_Request;
         Curve   : Stereographic_Curve;
         Name    : String)
      is
         Deviation : constant Geometry.Distance_Interval :=
           Geometry.Maximum_Deviation_From_Line_Corner
             (Curve,
              Request.Start.Point,
              Origin,
              Request.Finish.Point,
              1.0E-4 * mm);
         Point_Distance : constant Geometry.Distance_Interval :=
           Geometry.Minimum_Distance_To_Point
             (Curve,
              Origin,
              1.0E-4 * mm);
      begin
         T.Assert
           (Deviation.Upper < Length'Last,
            Name & " has a finite deviation upper bound");
         T.Assert
           (Point_Distance.Upper < Length'Last,
            Name & " has a finite corner-distance upper bound");
         Assert_Geometry_Intervals_Cover_Dense_Samples
           (Curve,
            Request.Start.Point,
            Origin,
            Request.Finish.Point,
            Origin,
            Deviation,
            Point_Distance,
            Name,
            T);
      end Assert_Asymmetric_Geometry;
   begin
      T.Register;
      T.Assert
        (Long_In.Kind = Blend_Success,
         "20:1 geometry curve constructs: " & Long_In.Kind'Image);
      T.Assert
        (Long_Out.Kind = Blend_Success,
         "1:20 geometry curve constructs: " & Long_Out.Kind'Image);
      if Long_In.Kind /= Blend_Success
        or else Long_Out.Kind /= Blend_Success
      then
         return;
      end if;

      T.Assert
        (Long_In.Curve.Warp_Factor < 0.1,
         "20:1 geometry test uses a strongly asymmetric Mobius warp");
      T.Assert
        (Long_Out.Curve.Warp_Factor > 10.0,
         "1:20 geometry test uses the reciprocal Mobius warp");
      Assert_Asymmetric_Geometry
        (Long_In_Request,
         Long_In.Curve,
         "20:1 Mobius geometry");
      Assert_Asymmetric_Geometry
        (Long_Out_Request,
         Long_Out.Curve,
         "1:20 Mobius geometry");
   end Test_Geometry_Asymmetric_Mobius_Warps;

   procedure Test_Geometry_Widens_For_Position_Certificate
     (T : in out Trendy_Test.Operation'Class)
   is
      Request : constant Blend_Request := Quarter_Turn_Request;
      Result : constant Blend_Result := Create_Blend (Request);
   begin
      T.Register;
      T.Assert
        (Result.Kind = Blend_Success,
         "Certificate-widening geometry curve constructs");
      if Result.Kind /= Blend_Success then
         return;
      end if;

      declare
         Corner : constant Position :=
           Make_Point
             (10.0 * mm,
              0.0 * mm,
              0.0 * mm,
              0.0 * mm);
         Synthetic_Error : constant Length := 0.25E-3 * mm;
         Minimum_Certificate_Effect : constant Length :=
           1.9 * Synthetic_Error;
         Ideal_Certificate_Curve : Stereographic_Curve := Result.Curve;
         Executed_Certificate_Curve : Stereographic_Curve := Result.Curve;
      begin
         --  The geometry stays fixed while only the public ideal-to-executed
         --  certificate changes.  The unattained side of each capsule
         --  consumes one error radius at its evaluator endpoints and another
         --  when the ideal capsule is widened to every executed point.  The
         --  opposite side is an attained evaluator sample and must not move.
         Ideal_Certificate_Curve.Certified_Position_Error :=
           0.0 * mm;
         Executed_Certificate_Curve.Certified_Position_Error :=
           Synthetic_Error;

         declare
            Ideal_Deviation : constant Geometry.Distance_Interval :=
              Geometry.Maximum_Deviation_From_Line_Corner
                (Ideal_Certificate_Curve,
                 Request.Start.Point,
                 Corner,
                 Request.Finish.Point,
                 1.0E-6 * mm);
            Executed_Deviation : constant Geometry.Distance_Interval :=
              Geometry.Maximum_Deviation_From_Line_Corner
                (Executed_Certificate_Curve,
                 Request.Start.Point,
                 Corner,
                 Request.Finish.Point,
                 1.0E-6 * mm);
            Ideal_Distance : constant Geometry.Distance_Interval :=
              Geometry.Minimum_Distance_To_Point
                (Ideal_Certificate_Curve,
                 Corner,
                 1.0E-6 * mm);
            Executed_Distance : constant Geometry.Distance_Interval :=
              Geometry.Minimum_Distance_To_Point
                (Executed_Certificate_Curve,
                 Corner,
                 1.0E-6 * mm);
         begin
            T.Assert
              (Executed_Deviation.Lower = Ideal_Deviation.Lower,
               "Deviation keeps its attained evaluator lower bound");
            T.Assert
              (Executed_Distance.Upper = Ideal_Distance.Upper,
               "Point distance keeps its attained evaluator upper bound");
            T.Assert
              (Executed_Deviation.Upper
                 >= Ideal_Deviation.Upper
                    + Minimum_Certificate_Effect,
               "Deviation upper bound includes both position-error radii");
            T.Assert
              (Executed_Distance.Lower
                 <= Ideal_Distance.Lower
                    - Minimum_Certificate_Effect,
               "Point-distance lower bound includes both position-error radii");
         end;
      end;
   end Test_Geometry_Widens_For_Position_Certificate;

   procedure Test_Geometry_Scale_And_Range
     (T : in out Trendy_Test.Operation'Class)
   is
      Small_Scale : constant Dimensionless := 0.01;
      Large_Scale : constant Dimensionless := 100.0;
      Small_Request : constant Blend_Request :=
        Quarter_Turn_Request (Small_Scale);
      Large_Request : constant Blend_Request :=
        Quarter_Turn_Request (Large_Scale);
      Small : constant Blend_Result := Create_Blend (Small_Request);
      Large : constant Blend_Result := Create_Blend (Large_Request);
      Extreme_Point : constant Position :=
        [X_Axis => Length'Last,
         Y_Axis => Length'Last,
         others => 0.0 * mm];
      Saturated_Distance : constant Geometry.Distance_Interval :=
        Geometry.Minimum_Distance_To_Point
          (Zero_Blend (Origin),
           Extreme_Point,
           1.0 * mm);

      procedure Assert_Scaled_Geometry
        (Request : Blend_Request;
         Curve   : Stereographic_Curve;
         Scale   : Dimensionless;
         Name    : String);

      procedure Assert_Scaled_Geometry
        (Request : Blend_Request;
         Curve   : Stereographic_Curve;
         Scale   : Dimensionless;
         Name    : String)
      is
         Corner : constant Position :=
           Make_Point
             (10.0 * mm * Scale,
              0.0 * mm,
              0.0 * mm,
              0.0 * mm);
         Width : constant Length := 0.25 * mm * Scale;
         Deviation : constant Geometry.Distance_Interval :=
           Geometry.Maximum_Deviation_From_Line_Corner
             (Curve,
              Request.Start.Point,
              Corner,
              Request.Finish.Point,
              Width);
         Point_Distance : constant Geometry.Distance_Interval :=
           Geometry.Minimum_Distance_To_Point
             (Curve,
              Corner,
              Width);
      begin
         T.Assert
           (Deviation.Upper < Length'Last,
            Name & " produces a finite deviation certificate");
         T.Assert
           (Point_Distance.Upper < Length'Last,
            Name & " produces a finite point-distance certificate");
         Assert_Geometry_Intervals_Cover_Dense_Samples
           (Curve,
            Request.Start.Point,
            Corner,
            Request.Finish.Point,
            Corner,
            Deviation,
            Point_Distance,
            Name,
            T);
      end Assert_Scaled_Geometry;
   begin
      T.Register;
      T.Assert
        (Small.Kind = Blend_Success,
         "Small-scale geometry curve constructs: " & Small.Kind'Image);
      T.Assert
        (Large.Kind = Blend_Success,
         "Large-scale geometry curve constructs: " & Large.Kind'Image);
      if Small.Kind = Blend_Success then
         Assert_Scaled_Geometry
           (Small_Request,
            Small.Curve,
            Small_Scale,
            "Small-scale geometry");
      end if;
      if Large.Kind = Blend_Success then
         Assert_Scaled_Geometry
           (Large_Request,
            Large.Curve,
            Large_Scale,
            "Large-scale geometry");
      end if;

      T.Assert
        (Saturated_Distance.Lower = 0.0 * mm
         and then Saturated_Distance.Upper = Length'Last,
         "Out-of-range distance arithmetic safely returns the unbounded interval");
   end Test_Geometry_Scale_And_Range;

   procedure Test_Uniform_Scaling
     (T : in out Trendy_Test.Operation'Class)
   is
      Small_Scale : constant Dimensionless := 0.01;
      Large_Scale : constant Dimensionless := 100.0;
      Base : constant Blend_Result :=
        Create_Blend (Circular_Quarter_Turn_Request);
      Small : constant Blend_Result :=
        Create_Blend
          (Circular_Quarter_Turn_Request (Small_Scale));
      Large : constant Blend_Result :=
        Create_Blend
          (Circular_Quarter_Turn_Request (Large_Scale));
   begin
      T.Register;
      T.Assert
        (Base.Kind = Blend_Success,
         "Base-scale circular construction succeeds");
      T.Assert
        (Small.Kind = Blend_Success,
         "Small-scale circular construction succeeds: "
         & Small.Kind'Image);
      T.Assert
        (Large.Kind = Blend_Success,
         "Large-scale circular construction succeeds: "
         & Large.Kind'Image);
      if Base.Kind /= Blend_Success
        or else Small.Kind /= Blend_Success
        or else Large.Kind /= Blend_Success
      then
         return;
      end if;

      Assert_Length_Close
        (Arc_Length (Small.Curve) / Small_Scale,
         Arc_Length (Base.Curve),
         2.0E-5 * mm,
         "Small-scale normalized arc length",
         T);
      Assert_Length_Close
        (Arc_Length (Large.Curve) / Large_Scale,
         Arc_Length (Base.Curve),
         2.0E-5 * mm,
         "Large-scale normalized arc length",
         T);

      declare
         Base_Bounds : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (Base.Curve);
         Base_Length : constant Length := Arc_Length (Base.Curve);

         procedure Assert_Scaled_Derivatives
           (Curve : Stereographic_Curve;
            Name  : String);

         procedure Assert_Scaled_Derivatives
           (Curve : Stereographic_Curve;
            Name  : String)
         is
            Bounds : constant Unit_Speed_Axial_Derivative_Bounds :=
              Derivative_Bounds (Curve);
            Curve_Length : constant Length := Arc_Length (Curve);
         begin
            Assert_Executed_Derivative_Bounds
              (Curve,
               Bounds,
               0.0 * mm,
               Curve_Length,
               Name,
               T);
            for Axis in Axis_Name loop
               for Order in Majorant_Order loop
                  declare
                     Base_Normalized : constant Dimensionless :=
                       Normalized_Derivative_Bound
                         (Base_Bounds,
                          Base_Length,
                          Axis,
                          Order);
                     Scaled_Normalized : constant Dimensionless :=
                       Normalized_Derivative_Bound
                         (Bounds,
                          Curve_Length,
                          Axis,
                          Order);
                     Allowance : constant Dimensionless :=
                       0.05
                       * Dimensionless'Max
                           (Base_Normalized,
                            Normalized_Derivative_Floor);
                  begin
                     T.Assert
                       (abs
                          (Scaled_Normalized
                           - Base_Normalized)
                          <= Allowance,
                        Name
                        & " preserves normalized "
                        & Axis'Image
                        & " derivative order"
                        & Order'Image
                        & ": base "
                        & Base_Normalized'Image
                        & ", scaled "
                        & Scaled_Normalized'Image);
                  end;
               end loop;
            end loop;
         end Assert_Scaled_Derivatives;
      begin
         Assert_Scaled_Derivatives
           (Base.Curve, "Base-scale circular bounds");
         Assert_Scaled_Derivatives
           (Small.Curve, "Small-scale circular bounds");
         Assert_Scaled_Derivatives
           (Large.Curve, "Large-scale circular bounds");
      end;

      for Sample in 0 .. 40 loop
         declare
            Parameter : constant Curve_Parameter :=
              Curve_Parameter (Dimensionless (Sample) / 40.0);
            Base_Point : constant Position :=
              Point_At_Parameter (Base.Curve, Parameter);
         begin
            Assert_Point_Close
              (Point_At_Parameter (Small.Curve, Parameter)
                 / Small_Scale,
               Base_Point,
               2.1E-3 * mm,
               "Small-scale normalized point",
               T);
            Assert_Point_Close
              (Point_At_Parameter (Large.Curve, Parameter)
                 / Large_Scale,
               Base_Point,
               2.1E-3 * mm,
               "Large-scale normalized point",
               T);
         end;
      end loop;

   end Test_Uniform_Scaling;

   procedure Test_Construction_Failure_Kinds
     (T : in out Trendy_Test.Operation'Class)
   is
      Request : Blend_Request := Quarter_Turn_Request;
   begin
      T.Register;

      Request.Maximum_Arc_Length := 15.0 * mm;
      Assert_Result_Kind
        (Request,
         Blend_Closure_Failed,
         "Quarter turn cannot close inside a valid short limit",
         T);

      Request := Quarter_Turn_Request;
      Request.Maximum_Position_Error := 1.0E-8 * mm;
      Assert_Result_Kind
        (Request,
         Blend_Success,
         "Tight but attainable realtime error budget succeeds",
         T);

      Request := Quarter_Turn_Request;
      Request.Maximum_Position_Error := 1.0E-12 * mm;
      Assert_Result_Kind
        (Request,
         Blend_Representation_Failed,
         "Unattainable realtime error budget is classified",
         T);

      Request :=
        Make_Request
          (Start                  =>
             Make_Point
               (-21.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
           Finish                 =>
             Make_Point
               (0.0 * mm, 1.0 * mm, 0.0 * mm, 0.0 * mm),
           Start_Tangent          => X_Unit,
           Finish_Tangent         => Y_Unit,
           Maximum_Position_Error => 1.0E-3 * mm,
           Maximum_Arc_Length     => 22.0 * mm);
      Assert_Result_Kind
        (Request,
         Blend_Closure_Failed,
         "Trim asymmetry above the advertised 20:1 limit is rejected",
         T);
   end Test_Construction_Failure_Kinds;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Interval_Multiplication_Sign_Cases'Access,
         Test_Complex_Pole_Primitive_Cancellation'Access,
         Test_Default_And_Zero_State'Access,
         Test_Request_Validation'Access,
         Test_Construction_Failure_Kinds'Access,
         Test_Straight_Line'Access,
         Test_Quarter_Turn_And_Evaluator'Access,
         Test_Rounded_Quarter_Turn_Endpoint'Access,
         Test_Planar_Roundoff_Canonicalization'Access,
         Test_Non_Chord_Flat_Tangents'Access,
         Test_Nonzero_Jets_And_Bounds'Access,
         Test_Saturated_Physical_Bounds'Access,
         Test_Ranged_And_Projected_Bounds'Access,
         Test_Supported_Asymmetric_Corners'Access,
         Test_Geometry_Zero_And_Degenerate_References'Access,
         Test_Geometry_Straight_And_Quarter_Turn'Access,
         Test_Geometry_Asymmetric_Mobius_Warps'Access,
         Test_Geometry_Widens_For_Position_Certificate'Access,
         Test_Geometry_Scale_And_Range'Access,
         Test_Uniform_Scaling'Access];
   end All_Tests;

end Prunt.Motion_Planner.Stereographic_Curves.Test;
