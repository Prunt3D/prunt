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

package body Prunt.Motion_Planner.Stereographic_Curves.Geometry is

   pragma
     Compile_Time_Error
       (not Dimensionless'Machine_Rounds, "Stereographic_Curves.Geometry requires rounded floating-point arithmetic");

   ---------------------------------------------------------------------------
   --  Enclosing elementary geometry
   ---------------------------------------------------------------------------
   --
   --  The public answers are only as trustworthy as their smallest distance calculation. These helpers therefore carry
   --  an interval through every add, multiply, divide, and square root. Invalid or overflowing arithmetic becomes an
   --  uninformative interval; it is never converted into a plausible-looking scalar distance.

   function Square_Interval (Value : Interval) return Interval is
   begin
      if not Value.Valid then
         return Invalid_Geometry_Interval;
      elsif Value.Lower = 0.0 and then Value.Upper = 0.0 then
         --  Preserve structural zero. Expanding it to [0, Model_Small] would ask the square-root verifier to prove a
         --  fictitious subnormal upper endpoint.
         return Interval_Exact (0.0);
      elsif Value.Lower <= 0.0 and then Value.Upper >= 0.0 then
         declare
            Maximum : constant Dimensionless :=
              Dimensionless'Max (Value.Lower * Value.Lower, Value.Upper * Value.Upper);
         begin
            return Checked_Interval (0.0, Up (Maximum));
         end;
      else
         return Interval_Multiply (Value, Value);
      end if;
   exception
      when Constraint_Error =>
         return Invalid_Geometry_Interval;
   end Square_Interval;

   function Divide_Intervals (Left, Right : Interval) return Interval is
   begin
      if not Left.Valid or else not Right.Valid or else Right.Lower <= 0.0 then
         return Invalid_Geometry_Interval;
      else
         return Interval_Multiply (Left, Checked_Interval (Down (1.0 / Right.Upper), Up (1.0 / Right.Lower)));
      end if;
   exception
      when Constraint_Error =>
         return Invalid_Geometry_Interval;
   end Divide_Intervals;

   function Square_Root_Interval (Value : Interval) return Interval is
   begin
      if not Value.Valid or else Value.Upper < 0.0 then
         return Invalid_Geometry_Interval;
      else
         declare
            Radicand_Lower : constant Dimensionless := Dimensionless'Max (0.0, Value.Lower);
            Radicand_Upper : constant Dimensionless := Dimensionless'Max (0.0, Value.Upper);

            Lower_Root : Dimensionless :=
              (if Radicand_Lower = 0.0
               then 0.0
               else Dimensionless'Max (0.0, Down (Dimensionless_Math.Sqrt (Radicand_Lower))));
            Upper_Root : Dimensionless :=
              (if Radicand_Upper = 0.0
               then 0.0
               else Dimensionless'Max (0.0, Up (Dimensionless_Math.Sqrt (Radicand_Upper))));

            Lower_Is_Proven : Boolean := Radicand_Lower = 0.0;
            Upper_Is_Proven : Boolean := Radicand_Upper = 0.0;
         begin
            --  Generic_Elementary_Functions does not promise directed rounding. Treat each square root as a proposal
            --  and prove the enclosure by squaring it with our outward-rounded interval arithmetic. Repeated widening
            --  handles a correctly rounded result which happens to land on the wrong side by a few ulps.
            for Attempt in 1 .. 8 loop
               if not Lower_Is_Proven then
                  declare
                     Square : constant Interval := Square_Interval (Interval_Exact (Lower_Root));
                  begin
                     Lower_Is_Proven := Square.Valid and then Square.Upper <= Radicand_Lower;
                     if not Lower_Is_Proven then
                        Lower_Root := Dimensionless'Max (0.0, Down (Lower_Root));
                     end if;
                  end;
               end if;

               if not Upper_Is_Proven then
                  if Upper_Root >= Dimensionless'Last then
                     Upper_Is_Proven := True;
                  else
                     declare
                        Square : constant Interval := Square_Interval (Interval_Exact (Upper_Root));
                     begin
                        Upper_Is_Proven :=
                          Upper_Root >= 0.0 and then Square.Valid and then Square.Lower >= Radicand_Upper;
                        if not Upper_Is_Proven then
                           Upper_Root := Up (Upper_Root);
                        end if;
                     end;
                  end if;
               end if;

               exit when Lower_Is_Proven and Upper_Is_Proven;
            end loop;

            return Checked_Interval (Lower_Root, Upper_Root, Lower_Is_Proven and Upper_Is_Proven);
         end;
      end if;
   exception
      when Constraint_Error =>
         return Invalid_Geometry_Interval;
   end Square_Root_Interval;

   function Norm_Interval (Value : Interval_Position_Scale) return Interval is
      Square_Sum : Interval := Interval_Exact (0.0);
   begin
      for Axis in Axis_Name loop
         Square_Sum := Interval_Add (Square_Sum, Square_Interval (Value (Axis)));
      end loop;
      return Square_Root_Interval (Square_Sum);
   end Norm_Interval;

   function Coordinate_Difference (Left, Right : Length) return Interval is
   begin
      if Left = Right then
         return Interval_Exact (0.0);
      else
         return
           Interval_Subtract (Interval_Exact (Dimensionless (Left / mm)), Interval_Exact (Dimensionless (Right / mm)));
      end if;
   end Coordinate_Difference;

   function Unbounded_Distance return Distance_Interval is
   begin
      return (Lower => 0.0 * mm, Upper => Length'Last);
   end Unbounded_Distance;

   function Round_Down_Nonnegative (Value : Length) return Length is
   begin
      if Value <= 0.0 * mm then
         return 0.0 * mm;
      else
         return Length'Max (0.0 * mm, Length'Adjacent (Value, Length'First));
      end if;
   end Round_Down_Nonnegative;

   function Round_Up (Value : Length) return Length is
   begin
      if Value >= Length'Last then
         return Length'Last;
      else
         return Length'Adjacent (Value, Length'Last);
      end if;
   end Round_Up;

   function To_Distance_Interval (Value : Interval) return Distance_Interval is
      Maximum_Raw : constant Dimensionless := Dimensionless (Length'Last / mm);
      Result      : Distance_Interval;
   begin
      if not Value.Valid or else Value.Upper < 0.0 then
         return Unbounded_Distance;
      end if;

      if Value.Lower <= 0.0 then
         Result.Lower := 0.0 * mm;
      elsif Value.Lower >= Maximum_Raw then
         Result.Lower := Length'Last;
      else
         Result.Lower := Round_Down_Nonnegative (Value.Lower * mm);
      end if;

      if Value.Upper >= Maximum_Raw then
         Result.Upper := Length'Last;
      else
         Result.Upper := Round_Up (Dimensionless'Max (0.0, Value.Upper) * mm);
      end if;

      if not (Result.Lower <= Result.Upper) then
         return Unbounded_Distance;
      end if;
      return Result;
   exception
      when Constraint_Error =>
         return Unbounded_Distance;
   end To_Distance_Interval;

   function Add_Upper (Left, Right : Length) return Length is
      Maximum_Raw : constant Dimensionless := Dimensionless (Length'Last / mm);
   begin
      if Left >= Length'Last or else Right >= Length'Last then
         return Length'Last;
      elsif Left <= 0.0 * mm then
         return Length'Max (0.0 * mm, Right);
      elsif Right <= 0.0 * mm then
         return Length'Max (0.0 * mm, Left);
      else
         declare
            Sum : constant Dimensionless := Dimensionless (Left / mm) + Dimensionless (Right / mm);
         begin
            if not Is_Finite (Sum) or else Sum >= Maximum_Raw then
               return Length'Last;
            else
               return Round_Up (Up (Sum) * mm);
            end if;
         end;
      end if;
   exception
      when Constraint_Error =>
         return Length'Last;
   end Add_Upper;

   function Subtract_Lower_Nonnegative (Left, Right : Length) return Length is
   begin
      if Left <= Right then
         return 0.0 * mm;
      elsif Left >= Length'Last then
         --  No finite lower bound can safely be inferred from the sentinel value.
         return 0.0 * mm;
      else
         declare
            Difference : constant Dimensionless := Dimensionless (Left / mm) - Dimensionless (Right / mm);
         begin
            return Round_Down_Nonnegative (Down (Difference) * mm);
         end;
      end if;
   exception
      when Constraint_Error =>
         return 0.0 * mm;
   end Subtract_Lower_Nonnegative;

   function Valid_Error (Value : Length) return Boolean is
      Raw : constant Dimensionless := Dimensionless (Value / mm);
   begin
      return Is_Finite (Raw) and then Value >= 0.0 * mm and then Value < Length'Last;
   end Valid_Error;

   function Point_To_Point_Distance (Left, Right : Position) return Distance_Interval is
      Difference : Interval_Position_Scale;
   begin
      for Axis in Axis_Name loop
         Difference (Axis) := Coordinate_Difference (Left (Axis), Right (Axis));
      end loop;

      return To_Distance_Interval (Norm_Interval (Difference));
   end Point_To_Point_Distance;

   function Point_To_Segment_Distance (Point, Segment_Start, Segment_End : Position) return Distance_Interval is
      To_Start : constant Distance_Interval := Point_To_Point_Distance (Point, Segment_Start);
      To_End   : constant Distance_Interval := Point_To_Point_Distance (Point, Segment_End);

      function Endpoint_Only_Bound return Distance_Interval;

      function Endpoint_Only_Bound return Distance_Interval is
      begin
         return (Lower => 0.0 * mm, Upper => Length'Min (To_Start.Upper, To_End.Upper));
      end Endpoint_Only_Bound;
   begin
      if Segment_Start = Segment_End then
         return To_Start;
      end if;

      declare
         Offset, From_Start : Interval_Position_Scale;
         Offset_Square      : Interval := Interval_Exact (0.0);
         Projection         : Interval := Interval_Exact (0.0);
      begin
         for Axis in Axis_Name loop
            Offset (Axis) := Coordinate_Difference (Segment_End (Axis), Segment_Start (Axis));
            From_Start (Axis) := Coordinate_Difference (Point (Axis), Segment_Start (Axis));
            Offset_Square := Interval_Add (Offset_Square, Square_Interval (Offset (Axis)));
            Projection := Interval_Add (Projection, Interval_Multiply (From_Start (Axis), Offset (Axis)));
         end loop;

         if not Offset_Square.Valid or else Offset_Square.Lower <= 0.0 then
            return Endpoint_Only_Bound;
         end if;

         declare
            Along : constant Interval := Divide_Intervals (Projection, Offset_Square);
         begin
            if not Along.Valid then
               return Endpoint_Only_Bound;
            end if;

            declare
               Clamped        : constant Interval :=
                 Checked_Interval
                   (Dimensionless'Max (0.0, Dimensionless'Min (1.0, Along.Lower)),
                    Dimensionless'Max (0.0, Dimensionless'Min (1.0, Along.Upper)));
               Residual       : Interval_Position_Scale;
               Result         : Distance_Interval;
               Endpoint_Upper : constant Length := Length'Min (To_Start.Upper, To_End.Upper);
            begin
               if not Clamped.Valid then
                  return Endpoint_Only_Bound;
               end if;

               for Axis in Axis_Name loop
                  Residual (Axis) := Interval_Subtract (From_Start (Axis), Interval_Multiply (Clamped, Offset (Axis)));
               end loop;

               Result := To_Distance_Interval (Norm_Interval (Residual));
               Result.Upper := Length'Min (Result.Upper, Endpoint_Upper);
               if not (Result.Lower <= Result.Upper) then
                  return Endpoint_Only_Bound;
               end if;
               return Result;
            end;
         end;
      end;
   end Point_To_Segment_Distance;

   function Corner_Distance (Point, Start, Corner, Finish : Position) return Distance_Interval is
      Incoming : constant Distance_Interval := Point_To_Segment_Distance (Point, Start, Corner);
      Outgoing : constant Distance_Interval := Point_To_Segment_Distance (Point, Corner, Finish);
   begin
      return
        (Lower => Length'Min (Incoming.Lower, Outgoing.Lower), Upper => Length'Min (Incoming.Upper, Outgoing.Upper));
   end Corner_Distance;

   function Frame_Speed_Upper (Curve : Stereographic_Curve) return Dimensionless is
   begin
      return
        (if Curve.Evaluator_Data.Kind = Zero_Curve_Kind
         then 0.0
         else Curve.Certified_Frame_Speed_Upper);
   end Frame_Speed_Upper;

   function Maximum_Half_Parameter_Gap (Segments : Positive) return Dimensionless is
      Previous : Dimensionless := 0.0;
      Maximum  : Dimensionless := 0.0;
   begin
      for I in 1 .. Segments loop
         declare
            Current  : constant Dimensionless :=
              (if I = Segments then 1.0 else Dimensionless (I) / Dimensionless (Segments));
            Gap      : constant Interval := Interval_Subtract (Interval_Exact (Current), Interval_Exact (Previous));
            Half_Gap : constant Interval := Interval_Multiply (Gap, Interval_Exact (0.5));
         begin
            if not Half_Gap.Valid then
               return Dimensionless'Last;
            end if;
            Maximum := Dimensionless'Max (Maximum, Half_Gap.Upper);
            Previous := Current;
         end;
      end loop;
      return Maximum;
   exception
      when Constraint_Error =>
         return Dimensionless'Last;
   end Maximum_Half_Parameter_Gap;

   function Lipschitz_Half_Gap_Upper
     (Curve : Stereographic_Curve; Speed_Upper : Dimensionless; Segments : Positive) return Length
   is
      Half_Gap : constant Dimensionless := Maximum_Half_Parameter_Gap (Segments);
      Spatial  : constant Interval :=
        Interval_Multiply
          (Interval_Multiply (Interval_Exact (Dimensionless (Arc_Length (Curve) / mm)), Interval_Exact (Speed_Upper)),
           Interval_Exact (Half_Gap));
   begin
      return To_Distance_Interval (Spatial).Upper;
   exception
      when Constraint_Error =>
         return Length'Last;
   end Lipschitz_Half_Gap_Upper;

   function Curvature_Norm_Upper (Curve : Stereographic_Curve) return Curvature is
      Axial  : constant Unit_Speed_Axial_Acceleration_Bounds := Derivative_Bounds (Curve).Acceleration;
      Square : Interval := Interval_Exact (0.0);
   begin
      for Axis in Axis_Name loop
         declare
            Component : constant Dimensionless := abs Dimensionless (Axial (Axis) / (1.0 / mm));
         begin
            if not Is_Finite (Component) then
               return Curvature'Last;
            end if;
            Square := Interval_Add (Square, Square_Interval (Interval_Exact (Component)));
         end;
      end loop;

      declare
         Norm : constant Interval := Square_Root_Interval (Square);
      begin
         return (if Norm.Valid then Norm.Upper / mm else Curvature'Last);
      end;
   exception
      when Constraint_Error =>
         return Curvature'Last;
   end Curvature_Norm_Upper;

   function Curvature_Capsule_Radius
     (Curve                          : Stereographic_Curve;
      Start_Parameter, End_Parameter : Curve_Parameter;
      Curvature_Upper                : Curvature) return Length
   is
      Parameter_Width  : constant Interval :=
        Interval_Subtract
          (Interval_Exact (Dimensionless (End_Parameter)), Interval_Exact (Dimensionless (Start_Parameter)));
      Width_Difference : constant Interval :=
        Interval_Multiply (Interval_Exact (Dimensionless (Arc_Length (Curve) / mm)), Parameter_Width);
   begin
      if not Width_Difference.Valid or else Width_Difference.Upper < 0.0 then
         return Length'Last;
      end if;

      declare
         Width : constant Interval := Checked_Interval (0.0, Dimensionless'Max (0.0, Width_Difference.Upper));
      begin
         declare
            Radius : constant Interval :=
              Divide_Intervals
                (Interval_Multiply
                   (Interval_Exact (Dimensionless (Curvature_Upper / (1.0 / mm))), Square_Interval (Width)),
                 Interval_Exact (8.0));
         begin
            return To_Distance_Interval (Radius).Upper;
         end;
      end;
   exception
      when Constraint_Error =>
         return Length'Last;
   end Curvature_Capsule_Radius;

   function Make_Capsule
     (Curve                          : Stereographic_Curve;
      Kind                           : Metric_Kind;
      Point, Start, Corner, Finish   : Position;
      Start_Parameter, End_Parameter : Curve_Parameter;
      Start_Point, End_Point         : Position;
      Start_Error, End_Error         : Length;
      Curvature_Upper                : Curvature) return Capsule
   is
      Endpoint_Error_Max : constant Length := Length'Max (Start_Error, End_Error);
      Curvature_Radius   : constant Length :=
        Curvature_Capsule_Radius (Curve, Start_Parameter, End_Parameter, Curvature_Upper);
      Radius             : constant Length := Add_Upper (Curvature_Radius, Endpoint_Error_Max);
      Result             : Capsule := (Lower => 0.0 * mm, Upper => Length'Last);
   begin
      case Kind is
         when Point_Minimum       =>
            declare
               Chord_Distance : constant Distance_Interval :=
                 Point_To_Segment_Distance (Point, Start_Point, End_Point);
               Start_Value    : constant Distance_Interval := Point_To_Point_Distance (Start_Point, Point);
               End_Value      : constant Distance_Interval := Point_To_Point_Distance (End_Point, Point);
            begin
               --  Every ideal point is inside the capsule. Conversely, either endpoint supplies an attained sample
               --  for the branch-and-bound upper bound.
               Result.Lower := Subtract_Lower_Nonnegative (Chord_Distance.Lower, Radius);
               Result.Upper := Length'Min (Start_Value.Upper, End_Value.Upper);
            end;

         when Line_Corner_Maximum =>
            declare
               Chord_Subsegments  : constant Positive := 16;
               Start_In           : constant Distance_Interval :=
                 Point_To_Segment_Distance (Start_Point, Start, Corner);
               End_In             : constant Distance_Interval := Point_To_Segment_Distance (End_Point, Start, Corner);
               Start_Out          : constant Distance_Interval :=
                 Point_To_Segment_Distance (Start_Point, Corner, Finish);
               End_Out            : constant Distance_Interval :=
                 Point_To_Segment_Distance (End_Point, Corner, Finish);
               Start_At_Lower     : constant Length := Length'Min (Start_In.Lower, Start_Out.Lower);
               End_At_Lower       : constant Length := Length'Min (End_In.Lower, End_Out.Lower);
               Attained           : Length := Length'Max (Start_At_Lower, End_At_Lower);
               Segment_Envelope   : Length := 0.0 * mm;
               Previous_In_Upper  : Length := Add_Upper (Start_In.Upper, Start_Error);
               Previous_Out_Upper : Length := Add_Upper (Start_Out.Upper, Start_Error);
            begin
               --  Distance to either fixed reference segment is convex along a chord, so its maximum is attained at a
               --  chord endpoint.  Sample a few actual executed points inside this curvature capsule and apply that
               --  fact to each consecutive chord. The parent capsule radius remains a valid (deliberately loose)
               --  deviation bound for every subarc. This sharply encloses the point where the nearer corner leg
               --  switches without asking for thousands of additional derivative-range certificates.
               for J in 1 .. Chord_Subsegments loop
                  declare
                     Sample_Parameter    : constant Curve_Parameter :=
                       (if J = Chord_Subsegments
                        then End_Parameter
                        else
                          Curve_Parameter
                            (Start_Parameter
                             + (End_Parameter - Start_Parameter)
                               * Dimensionless (J)
                               / Dimensionless (Chord_Subsegments)));
                     Sample_Point        : constant Position :=
                       (if J = Chord_Subsegments then End_Point else Point_At_Parameter (Curve, Sample_Parameter));
                     Sample_Error        : constant Length :=
                       (if J = Chord_Subsegments then End_Error else Endpoint_Error_Max);
                     Sample_In           : constant Distance_Interval :=
                       Point_To_Segment_Distance (Sample_Point, Start, Corner);
                     Sample_Out          : constant Distance_Interval :=
                       Point_To_Segment_Distance (Sample_Point, Corner, Finish);
                     Sample_In_Upper     : constant Length := Add_Upper (Sample_In.Upper, Sample_Error);
                     Sample_Out_Upper    : constant Length := Add_Upper (Sample_Out.Upper, Sample_Error);
                     Sample_At_Lower     : constant Length := Length'Min (Sample_In.Lower, Sample_Out.Lower);
                     Subsegment_Envelope : constant Length :=
                       Length'Min
                         (Length'Max (Previous_In_Upper, Sample_In_Upper),
                          Length'Max (Previous_Out_Upper, Sample_Out_Upper));
                  begin
                     Attained := Length'Max (Attained, Sample_At_Lower);
                     Segment_Envelope := Length'Max (Segment_Envelope, Subsegment_Envelope);
                     Previous_In_Upper := Sample_In_Upper;
                     Previous_Out_Upper := Sample_Out_Upper;
                  end;
               end loop;
               Result.Lower := Attained;
               Result.Upper := Add_Upper (Segment_Envelope, Curvature_Radius);
            end;
      end case;

      --  An inversion means that at least one independently computed certificate failed. Do not turn it into a
      --  plausible-looking singleton: discard both bounds and fail closed.
      if not (Result.Lower <= Result.Upper) then
         return (Lower => 0.0 * mm, Upper => Length'Last);
      end if;
      return Result;
   end Make_Capsule;

   function Global_Interval (Kind : Metric_Kind; Capsules : Capsule_Array) return Distance_Interval is
      Result : Distance_Interval :=
        (if Kind = Point_Minimum
         then (Lower => Length'Last, Upper => Length'Last)
         else (Lower => 0.0 * mm, Upper => 0.0 * mm));
   begin
      for I in Capsules'Range loop
         case Kind is
            when Point_Minimum       =>
               Result.Lower := Length'Min (Result.Lower, Capsules (I).Lower);
               Result.Upper := Length'Min (Result.Upper, Capsules (I).Upper);

            when Line_Corner_Maximum =>
               Result.Lower := Length'Max (Result.Lower, Capsules (I).Lower);
               Result.Upper := Length'Max (Result.Upper, Capsules (I).Upper);
         end case;
      end loop;

      if not (Result.Lower <= Result.Upper) then
         return Unbounded_Distance;
      end if;
      return Result;
   end Global_Interval;

   function Widen_Capsule_For_Executed_Position
     (Kind : Metric_Kind; Value : Distance_Interval; Error : Length) return Distance_Interval
   is
      Nonnegative_Error : constant Length := Length'Max (0.0 * mm, Error);
      Result            : Distance_Interval := Value;
   begin
      --  Capsule construction already uses evaluator samples for an attained side of the global extremum. Only the
      --  opposite, unobserved side needs the final ideal-to-executed position allowance.
      case Kind is
         when Point_Minimum       =>
            Result.Lower := Subtract_Lower_Nonnegative (Value.Lower, Nonnegative_Error);

         when Line_Corner_Maximum =>
            Result.Upper := Add_Upper (Value.Upper, Nonnegative_Error);
      end case;

      if not (Result.Lower <= Result.Upper) then
         return Unbounded_Distance;
      end if;
      return Result;
   end Widen_Capsule_For_Executed_Position;

   function Bound
     (Curve                        : Stereographic_Curve;
      Kind                         : Metric_Kind;
      Point, Start, Corner, Finish : Position;
      Maximum_Interval_Width       : Length) return Distance_Interval
   is
      Total                    : constant Length := Arc_Length (Curve);
      Speed_Upper              : Dimensionless := 0.0;
      Fixed_Lipschitz_Half_Gap : Length := 0.0 * mm;
      Use_Fixed_Lipschitz      : Boolean := False;

   begin
      --  Zero blends are genuine constant curves, not failed positive curves. Their geometry is a closed-form case
      --  and Position_Error_Bound is exactly zero by the core-package contract.
      if Curve.Evaluator_Data.Kind = Zero_Curve_Kind or else Total <= 0.0 * mm then
         declare
            Ideal : constant Distance_Interval :=
              (case Kind is
                 when Point_Minimum       => Point_To_Point_Distance (Curve.Evaluator_Data.Start_Point, Point),
                 when Line_Corner_Maximum =>
                   Corner_Distance (Curve.Evaluator_Data.Start_Point, Start, Corner, Finish));
         begin
            --  Even this closed form passes through directed-rounding distance operations; a rounded scalar is not
            --  treated as an exact real result merely because the curve is constant.
            return Widen_Capsule_For_Executed_Position (Kind, Ideal, Position_Error_Bound (Curve));
         end;
      end if;

      Speed_Upper := Frame_Speed_Upper (Curve);
      Fixed_Lipschitz_Half_Gap :=
        Lipschitz_Half_Gap_Upper (Curve, Speed_Upper, Fixed_Lipschitz_Segments);
      Use_Fixed_Lipschitz := Maximum_Interval_Width >= Fixed_Lipschitz_Half_Gap;

      --  At ordinary planner tolerances the frame-certified Lipschitz bound needs only a handful of samples and is
      --  substantially cheaper than computing curvature capsules. Very tight diagnostic requests use the quadratic
      --  capsule enclosure below. Both paths have fixed upper work bounds.
      if Maximum_Interval_Width >= Capsule_Precision_Threshold or else Use_Fixed_Lipschitz then
         declare
            Error       : constant Length := Position_Error_Bound (Curve);
            Twice_Error : constant Length := Add_Upper (Error, Error);
            Segments    : Positive :=
              (if Use_Fixed_Lipschitz then Fixed_Lipschitz_Segments else Maximum_Lipschitz_Segments);
         begin
            if not Valid_Error (Error) then
               return Unbounded_Distance;
            end if;
            if not Use_Fixed_Lipschitz and then Maximum_Interval_Width > Twice_Error then
               declare
                  Available_Length : constant Length := Round_Down_Nonnegative (Maximum_Interval_Width - Twice_Error);
                  Available        : constant Dimensionless := Dimensionless (Available_Length / mm);
                  Needed_Interval  : constant Interval :=
                    Divide_Intervals
                      (Interval_Multiply (Interval_Exact (Dimensionless (Total / mm)), Interval_Exact (Speed_Upper)),
                       Interval_Exact (2.0 * Available));
                  Needed           : constant Dimensionless :=
                    (if Needed_Interval.Valid then Needed_Interval.Upper else Dimensionless'Last);
               begin
                  if Is_Finite (Needed) and then Needed < Dimensionless (Maximum_Lipschitz_Segments) then
                     Segments := Positive'Max (1, Positive (Dimensionless'Ceiling (Needed)));
                  end if;
                  while Segments < Maximum_Lipschitz_Segments
                    and then Lipschitz_Half_Gap_Upper (Curve, Speed_Upper, Segments) > Available_Length
                  loop
                     Segments := Segments + 1;
                  end loop;
               exception
                  when Constraint_Error =>
                     Segments := Maximum_Lipschitz_Segments;
               end;
            end if;

            declare
               Samples : Distance_Interval :=
                 (if Kind = Point_Minimum
                  then (Lower => Length'Last, Upper => Length'Last)
                  else (Lower => 0.0 * mm, Upper => 0.0 * mm));
            begin
               for I in 0 .. Segments loop
                  declare
                     U      : constant Curve_Parameter :=
                       (if I = Segments then 1.0 else Curve_Parameter (Dimensionless (I) / Dimensionless (Segments)));
                     Sample : constant Position := Point_At_Parameter (Curve, U);
                     Value  : constant Distance_Interval :=
                       (case Kind is
                          when Point_Minimum       => Point_To_Point_Distance (Sample, Point),
                          when Line_Corner_Maximum => Corner_Distance (Sample, Start, Corner, Finish));
                  begin
                     case Kind is
                        when Point_Minimum       =>
                           Samples.Lower := Length'Min (Samples.Lower, Value.Lower);
                           Samples.Upper := Length'Min (Samples.Upper, Value.Upper);

                        when Line_Corner_Maximum =>
                           Samples.Lower := Length'Max (Samples.Lower, Value.Lower);
                           Samples.Upper := Length'Max (Samples.Upper, Value.Upper);
                     end case;
                  end;
               end loop;

               declare
                  Allowance : constant Length :=
                    Add_Upper (Lipschitz_Half_Gap_Upper (Curve, Speed_Upper, Segments), Twice_Error);
                  Result    : Distance_Interval;
               begin
                  case Kind is
                     when Point_Minimum       =>
                        Result :=
                          (Lower => Subtract_Lower_Nonnegative (Samples.Lower, Allowance), Upper => Samples.Upper);

                     when Line_Corner_Maximum =>
                        Result := (Lower => Samples.Lower, Upper => Add_Upper (Samples.Upper, Allowance));
                  end case;
                  if not (Result.Lower <= Result.Upper) then
                     return Unbounded_Distance;
                  end if;
                  return Result;
               end;
            end;
         end;
      end if;

      declare
         Error              : constant Length := Position_Error_Bound (Curve);
         Curvature_Upper    : constant Curvature := Curvature_Norm_Upper (Curve);
         Capsules           : Capsule_Array;
         Previous_Parameter : Curve_Parameter := 0.0;
         Previous_Point     : Position := Point_At_Parameter (Curve, 0.0);
      begin
         if not Valid_Error (Error)
           or else not Is_Finite (Dimensionless (Curvature_Upper / (1.0 / mm)))
           or else Curvature_Upper = Curvature'Last
         then
            return Unbounded_Distance;
         end if;

         for I in 1 .. Fixed_Capsule_Segments loop
            declare
               End_Parameter : constant Curve_Parameter :=
                 (if I = Fixed_Capsule_Segments
                  then 1.0
                  else Curve_Parameter (Dimensionless (I) / Dimensionless (Fixed_Capsule_Segments)));
               End_Point     : constant Position := Point_At_Parameter (Curve, End_Parameter);
            begin
               Capsules (I) :=
                 Make_Capsule
                   (Curve,
                    Kind,
                    Point,
                    Start,
                    Corner,
                    Finish,
                    Previous_Parameter,
                    End_Parameter,
                    Previous_Point,
                    End_Point,
                    Error,
                    Error,
                    Curvature_Upper);
               Previous_Parameter := End_Parameter;
               Previous_Point := End_Point;
            end;
         end loop;
         --  Each capsule above bounds the ideal curve: Error accounts for locating the ideal endpoints from the stored
         --  evaluator samples, while K*h**2/8 encloses the ideal subarc between those endpoints. Geometry's public
         --  contract is for executed evaluator positions, so one further Error radius is required here. In particular,
         --  the effective executed-curve capsule allowance is K*h**2/8 + 2*Error.
         return Widen_Capsule_For_Executed_Position (Kind, Global_Interval (Kind, Capsules), Error);
      end;
   end Bound;

   function Maximum_Deviation_From_Line_Corner
     (Curve : Stereographic_Curve; Start, Corner, Finish : Position; Maximum_Interval_Width : Length)
      return Distance_Interval is
   begin
      return
        Bound
          (Curve,
           Line_Corner_Maximum,
           Point                  => Corner,
           Start                  => Start,
           Corner                 => Corner,
           Finish                 => Finish,
           Maximum_Interval_Width => Maximum_Interval_Width);
   end Maximum_Deviation_From_Line_Corner;

   function Minimum_Distance_To_Point
     (Curve : Stereographic_Curve; Point : Position; Maximum_Interval_Width : Length) return Distance_Interval is
   begin
      return
        Bound
          (Curve,
           Point_Minimum,
           Point                  => Point,
           Start                  => Point,
           Corner                 => Point,
           Finish                 => Point,
           Maximum_Interval_Width => Maximum_Interval_Width);
   end Minimum_Distance_To_Point;

end Prunt.Motion_Planner.Stereographic_Curves.Geometry;
