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

with Ada.Numerics.Long_Elementary_Functions;
with Prunt.Motion_Planner.Planner.Corner_Blender;
with Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter;
with Prunt.Motion_Planner.Planner.Preprocessor;
with Trendy_Test; use Trendy_Test;

package body Prunt.Motion_Planner.Planner.Test is

   pragma Extensions_Allowed (On);

   use Ada.Numerics.Long_Elementary_Functions;
   package Tested_Corner_Blender is new Corner_Blender;
   package Tested_Early_Kinematic_Limiter is new Early_Kinematic_Limiter;
   package Tested_Preprocessor is new Preprocessor;

   Identity_Tolerance_Factor : constant Long_Float := 32_768.0;

   type Raw_Vector is array (Axis_Name) of Long_Float;
   type Raw_Axis_Array is array (Positive range <>) of Axis_Name;
   type Sample_Fraction_Array is array (Positive range <>) of Dimensionless;

   Axial_Axes      : constant Raw_Axis_Array := [Z_Axis, E_Axis];
   Sample_Fractions : constant Sample_Fraction_Array := [0.0, 0.13, 0.41, 0.72, 1.0];

   function Bounds_Are_Zero (Bounds : Unit_Speed_Axial_Derivative_Bounds) return Boolean;
   function Dot (Left, Right : Raw_Vector) return Long_Float;
   function Point_Distance (Left, Right : Position) return Length;
   function Raw_Derivative_0 (Jet : Endpoint_Tangent_Jet) return Raw_Vector;
   function Raw_Derivative_1 (Jet : Endpoint_Tangent_Jet) return Raw_Vector;
   function Raw_Derivative_2 (Jet : Endpoint_Tangent_Jet) return Raw_Vector;
   function Raw_Derivative_3 (Jet : Endpoint_Tangent_Jet) return Raw_Vector;
   procedure Reset_Early_Limiter_Block (Block : out Execution_Block);

   procedure Assert_Roundoff_Residual
     (Residual, Magnitude : Long_Float; Name : String; T : in out Trendy_Test.Operation'Class);
   procedure Check_Helix_Direction (Clockwise : Boolean; T : in out Trendy_Test.Operation'Class);
   procedure Check_Unit_Tangent_Identities
     (Jet : Endpoint_Tangent_Jet; Name : String; T : in out Trendy_Test.Operation'Class);

   function Bounds_Are_Zero (Bounds : Unit_Speed_Axial_Derivative_Bounds) return Boolean is
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

   function Dot (Left, Right : Raw_Vector) return Long_Float is
      Result : Long_Float := 0.0;
   begin
      for Axis in Axis_Name loop
         Result := Result + Left (Axis) * Right (Axis);
      end loop;

      return Result;
   end Dot;

   function Point_Distance (Left, Right : Position) return Length is
      Distance_Squared : Dimensionless := 0.0;
   begin
      for Axis in Axis_Name loop
         Distance_Squared := Distance_Squared + ((Left (Axis) - Right (Axis)) / mm) ** 2;
      end loop;

      return Dimensionless (Sqrt (Long_Float (Distance_Squared))) * mm;
   end Point_Distance;

   function Raw_Derivative_0 (Jet : Endpoint_Tangent_Jet) return Raw_Vector is
   begin
      return [for Axis in Axis_Name => Long_Float (Jet.Tangent (Axis))];
   end Raw_Derivative_0;

   function Raw_Derivative_1 (Jet : Endpoint_Tangent_Jet) return Raw_Vector is
   begin
      return [for Axis in Axis_Name => Long_Float (Jet.Tangent_Derivative_1 (Axis) * mm)];
   end Raw_Derivative_1;

   function Raw_Derivative_2 (Jet : Endpoint_Tangent_Jet) return Raw_Vector is
   begin
      return [for Axis in Axis_Name => Long_Float (Jet.Tangent_Derivative_2 (Axis) * mm ** 2)];
   end Raw_Derivative_2;

   function Raw_Derivative_3 (Jet : Endpoint_Tangent_Jet) return Raw_Vector is
   begin
      return [for Axis in Axis_Name => Long_Float (Jet.Tangent_Derivative_3 (Axis) * mm ** 3)];
   end Raw_Derivative_3;

   procedure Reset_Early_Limiter_Block (Block : out Execution_Block) is
   begin
      Block.Kind := Motion_Block_Kind;
      Block.Flush_Resetting_Data := Flush_Resetting_Data_Type_Default;
      Block.Next_Block_Pos := [others => 0.0 * mm];
      Block.Params := (others => <>);
      Block.Params.Lower_Pos_Limit := [others => -1.0E100 * mm];
      Block.Params.Upper_Pos_Limit := [others => 1.0E100 * mm];
      Block.Corners_Extra_Data.Clear;
      Block.Corners_Extra_Data_End_Indices := [others => Block.Corners_Extra_Data.Last_Index];
      Block.Corners := [others => [others => 0.0 * mm]];
      Block.Primitives := [others => Make_Line_Primitive];
      Block.Original_Segment_Feedrates := [others => 0.0 * mm / s];
      Block.First_Corner_ID := 0;
      Block.Associated_Overflow_Block := False;
      Block.Is_Homing_Move := False;
      Block.Limited_Segment_Feedrates := [others => 0.0 * mm / s];
      Block.Corner_Dwell_Times := [others => 0.0 * s];
      for I in Block.Corner_Transitions'Range loop
         Block.Corner_Transitions (I) := To_Evaluator (Stop_At (Block.Corners (I)));
      end loop;
      Block.Corner_Velocity_Limits := [others => 0.0 * mm / s];
      Block.Feedrate_Profiles :=
        [others => (Accel => [others => 0.0 * s], Coast => 0.0 * s, Decel => [others => 0.0 * s])];
      Block.Primitive_Start_Distances := [others => 0.0 * mm];
      Block.Primitive_Distances := [others => 0.0 * mm];
      Block.Profile_Window_Selections := [others => 1];
      Block.Profile_Crackles := [others => 0.0 * mm / s ** 5];
   end Reset_Early_Limiter_Block;

   procedure Assert_Roundoff_Residual
     (Residual, Magnitude : Long_Float; Name : String; T : in out Trendy_Test.Operation'Class)
   is
      Tolerance : constant Long_Float :=
        Identity_Tolerance_Factor * Long_Float'Model_Epsilon * Long_Float'Max (1.0, Magnitude);
   begin
      T.Assert
        (abs Residual <= Tolerance,
         Name & ": residual" & Residual'Image & ", tolerance" & Tolerance'Image);
   end Assert_Roundoff_Residual;

   procedure Check_Helix_Direction (Clockwise : Boolean; T : in out Trendy_Test.Operation'Class) is
      Block : aliased Execution_Block (2);
      Center : constant Position :=
        [X_Axis => 3.0 * mm, Y_Axis => -4.0 * mm, Z_Axis => 2.0 * mm, E_Axis => -1.0 * mm];
      Radius      : constant Length := 12.0 * mm;
      Start_Phase : constant Dimensionless := (if Clockwise then 2.4 else -2.4);
      Phase_Delta : constant Dimensionless := (if Clockwise then -5.1 else 5.1);
      End_Phase   : constant Dimensionless := Start_Phase + Phase_Delta;
      Start_Point : constant Position :=
        [X_Axis => Center (X_Axis) + Dimensionless (Cos (Long_Float (Start_Phase))) * Radius,
         Y_Axis => Center (Y_Axis) + Dimensionless (Sin (Long_Float (Start_Phase))) * Radius,
         Z_Axis => Center (Z_Axis),
         E_Axis => Center (E_Axis)];
      End_Point : constant Position :=
        [X_Axis => Center (X_Axis) + Dimensionless (Cos (Long_Float (End_Phase))) * Radius,
         Y_Axis => Center (Y_Axis) + Dimensionless (Sin (Long_Float (End_Phase))) * Radius,
         Z_Axis => Center (Z_Axis) + 15.0 * mm,
         E_Axis => Center (E_Axis) - 7.0 * mm];
      Primitive : constant Path_Primitive :=
        Make_Helix_Primitive (Start_Point, End_Point, Center, Clockwise);
      Direction_Name : constant String := (if Clockwise then "clockwise" else "counterclockwise");
   begin
      Block.Corners (1) := Start_Point;
      Block.Corners (2) := End_Point;
      Block.Primitives (2) := Primitive;

      T.Assert (Primitive.Kind = Helix_Primitive_Kind, Direction_Name & " test primitive should be a helix");
      if Primitive.Kind /= Helix_Primitive_Kind then
         return;
      end if;

      for Sample_Index in Sample_Fractions'Range loop
         declare
            Fraction       : constant Dimensionless := Sample_Fractions (Sample_Index);
            Distance       : constant Length := Fraction * Primitive_Length (Block'Access, 2);
            Jet             : constant Endpoint_Tangent_Jet :=
              Primitive_Derivative_Jets_At_Distance (Block'Access, 2, Distance);
            Sample_Name : constant String := Direction_Name & " sample" & Sample_Index'Image;
         begin
            T.Assert (abs Jet.Tangent (Z_Axis) > 0.0, Sample_Name & " should have a nonzero axial Z tangent");
            T.Assert (abs Jet.Tangent (E_Axis) > 0.0, Sample_Name & " should have a nonzero axial E tangent");

            Check_Unit_Tangent_Identities (Jet, Sample_Name, T);

            for Axis of Axial_Axes loop
               T.Assert
                 (Jet.Tangent_Derivative_1 (Axis) = 0.0 / mm,
                  Sample_Name & " axial T' should be zero on " & Axis'Image);
               T.Assert
                 (Jet.Tangent_Derivative_2 (Axis) = 0.0 / mm ** 2,
                  Sample_Name & " axial T'' should be zero on " & Axis'Image);
               T.Assert
                 (Jet.Tangent_Derivative_3 (Axis) = 0.0 / mm ** 3,
                  Sample_Name & " axial T''' should be zero on " & Axis'Image);
            end loop;
         end;
      end loop;
   end Check_Helix_Direction;

   procedure Check_Unit_Tangent_Identities
     (Jet : Endpoint_Tangent_Jet; Name : String; T : in out Trendy_Test.Operation'Class)
   is
      D0 : constant Raw_Vector := Raw_Derivative_0 (Jet);
      D1 : constant Raw_Vector := Raw_Derivative_1 (Jet);
      D2 : constant Raw_Vector := Raw_Derivative_2 (Jet);
      D3 : constant Raw_Vector := Raw_Derivative_3 (Jet);

      D0_D0 : constant Long_Float := Dot (D0, D0);
      D0_D1 : constant Long_Float := Dot (D0, D1);
      D0_D2 : constant Long_Float := Dot (D0, D2);
      D0_D3 : constant Long_Float := Dot (D0, D3);
      D1_D1 : constant Long_Float := Dot (D1, D1);
      D1_D2 : constant Long_Float := Dot (D1, D2);
   begin
      Assert_Roundoff_Residual (D0_D0 - 1.0, abs D0_D0 + 1.0, Name & " dot(T,T) = 1", T);
      Assert_Roundoff_Residual (D0_D1, abs D0_D1, Name & " dot(T,T') = 0", T);
      Assert_Roundoff_Residual
        (D0_D2 + D1_D1, abs D0_D2 + abs D1_D1, Name & " dot(T,T'') = -dot(T',T')", T);
      Assert_Roundoff_Residual
        (D0_D3 + 3.0 * D1_D2,
         abs D0_D3 + 3.0 * abs D1_D2,
         Name & " dot(T,T''') = -3 dot(T',T'')",
         T);
   end Check_Unit_Tangent_Identities;

   procedure Test_Early_Limiter_Helix_Ignore_E (T : in out Trendy_Test.Operation'Class) is
      Block     : aliased Execution_Block (2);
      Motor_Map : constant Motor_Position_Map := [others => [others => Length'Last]];
      Radius    : constant Length := 10.0 * mm;
      Start_Pos : constant Position :=
        [X_Axis => Radius, Y_Axis => 0.0 * mm, Z_Axis => 0.0 * mm, E_Axis => 0.0 * mm];
      End_Pos   : constant Position :=
        [X_Axis => 0.0 * mm, Y_Axis => Radius, Z_Axis => 0.0 * mm, E_Axis => 10.0 * mm];
      Center    : constant Position := [others => 0.0 * mm];
      Commanded_XYZ_Feedrate : constant Velocity := 100.0 * mm / s;
      XYZ_Path_Length        : constant Length := Radius * Dimensionless (Ada.Numerics.Pi / 2.0);
   begin
      T.Register;

      Reset_Early_Limiter_Block (Block);
      Block.Params.Ignore_E_In_XYZE := True;
      Block.Params.Tangential_Velocity_Max := 1.0E6 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 1.0E6 * mm / s];
      Block.Corners := [1 => Start_Pos, 2 => End_Pos];
      Block.Primitives (2) := Make_Helix_Primitive (Start_Pos, End_Pos, Center, Clockwise => False);
      Block.Original_Segment_Feedrates (2) := Commanded_XYZ_Feedrate;
      Block.Primitive_Start_Distances (2) := 0.0 * mm;
      Block.Primitive_Distances (2) := Primitive_Length (Block'Access, 2);

      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);

      declare
         Expected : constant Velocity :=
           Commanded_XYZ_Feedrate * Primitive_Length (Block'Access, 2) / XYZ_Path_Length;
      begin
         T.Assert
           (abs (Block.Limited_Segment_Feedrates (2) - Expected) <= 1.0E-9 * mm / s,
            "Ignoring E should preserve the programmed XYZ speed on an extruding helix");
         T.Assert
           (abs (Block.Original_Segment_Feedrates (2) - Expected) <= 1.0E-9 * mm / s,
            "The programmed velocity reference uses the same full-path scalar coordinates");
      end;
   end Test_Early_Limiter_Helix_Ignore_E;

   procedure Test_Early_Limiter_Uses_Executed_Distance (T : in out Trendy_Test.Operation'Class) is
      Block             : aliased Execution_Block (2);
      Motor_Map         : constant Motor_Position_Map := [others => [others => Length'Last]];
      Retained_Distance : constant Length := 0.5 * mm;
      Expected_Limit    : constant Velocity := Retained_Distance / Interpolation_Time;
   begin
      T.Register;

      Reset_Early_Limiter_Block (Block);
      Block.Params.Ignore_E_In_XYZE := True;
      Block.Params.Tangential_Velocity_Max := 10_000.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 1.0E6 * mm / s];
      Block.Corners :=
        [1 => [others => 0.0 * mm],
         2 => [X_Axis => 100.0 * mm, others => 0.0 * mm]];
      Block.Primitives (2) := Make_Line_Primitive;
      Block.Original_Segment_Feedrates (2) := 10_000.0 * mm / s;
      Block.Primitive_Start_Distances (2) := 10.0 * mm;
      Block.Primitive_Distances (2) := Retained_Distance;

      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);

      T.Assert
        (Block.Limited_Segment_Feedrates (2) = Expected_Limit,
         "Minimum segment time should use the post-blend executable distance");
   end Test_Early_Limiter_Uses_Executed_Distance;

   procedure Test_Helix_Primitive_Tangent_Jet_Identities (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Check_Helix_Direction (Clockwise => False, T => T);
      Check_Helix_Direction (Clockwise => True, T => T);
   end Test_Helix_Primitive_Tangent_Jet_Identities;

   procedure Test_Tiny_Helix_And_Scaled_Derivatives (T : in out Trendy_Test.Operation'Class) is
      Tiny_Radius : constant Length := 1.0E6 * mm;
      Tiny_Angle  : constant Dimensionless := 5.0E-13;
      Center      : constant Position := [others => 0.0 * mm];
      Tiny_Start  : constant Position := [X_Axis => Tiny_Radius, others => 0.0 * mm];
      Tiny_End    : constant Position :=
        [X_Axis => Dimensionless (Cos (Long_Float (Tiny_Angle))) * Tiny_Radius,
         Y_Axis => Dimensionless (Sin (Long_Float (Tiny_Angle))) * Tiny_Radius,
         others => 0.0 * mm];
      Tiny_Primitive : constant Path_Primitive :=
        Make_Helix_Primitive (Tiny_Start, Tiny_End, Center, Clockwise => False);

      Small_Radius : constant Length := 1.0E-70 * mm;
      Small_Block  : aliased Execution_Block (2);
      Bounds       : Unit_Speed_Axial_Derivative_Bounds;
   begin
      T.Register;

      T.Assert (Tiny_Primitive.Kind = Helix_Primitive_Kind, "A tiny nonzero sweep remains a helix");
      if Tiny_Primitive.Kind = Helix_Primitive_Kind then
         declare
            Derived : constant Derived_Path_Primitive := Derive_Path_Primitive (Tiny_Primitive, Tiny_Start, Tiny_End);
         begin
            T.Assert
              (Derived.Length > 0.0 * mm and then Derived.Length < 1.0E-5 * mm,
               "A tiny sweep must not be promoted to a complete revolution");
         end;
      end if;

      declare
         Full_Circle : constant Derived_Path_Primitive :=
           Derive_Path_Primitive
             ((Kind => Helix_Primitive_Kind, Center => Center, Clockwise => False), Tiny_Start, Tiny_Start);
      begin
         T.Assert
           (Full_Circle.Kind = Helix_Primitive_Kind
            and then Full_Circle.Length > 6.0 * Tiny_Radius,
            "Exactly coincident XY endpoints retain full-circle semantics");
      end;

      Small_Block.Corners (1) := [X_Axis => Small_Radius, others => 0.0 * mm];
      Small_Block.Corners (2) := [Y_Axis => Small_Radius, others => 0.0 * mm];
      Small_Block.Primitives (2) :=
        Make_Helix_Primitive (Small_Block.Corners (1), Small_Block.Corners (2), Center, Clockwise => False);
      Bounds := Primitive_Derivative_Bounds (Small_Block'Access, 2, 0.0 * mm, Small_Radius);
      T.Assert
        (Bounds.Crackle (X_Axis) > 1.0E279 / mm ** 4,
         "Representable tiny-radius derivative bounds must not underflow their denominator first");
   end Test_Tiny_Helix_And_Scaled_Derivatives;

   procedure Test_Analytical_Shaper_Motor_Bound (T : in out Trendy_Test.Operation'Class) is
      Params       : Kinematic_Parameters := (others => <>);
      Motor_Map    : Motor_Position_Map := [others => [others => Length'Last]];
      ZV_Parameters : constant Input_Shapers.Shaper_Parameters :=
        (Kind                            => Input_Shapers.Zero_Vibration,
         Zero_Vibration_Frequency        => 50.0 * hertz,
         Zero_Vibration_Damping_Ratio    => 0.1,
         Zero_Vibration_Deriviatives     => 0);
      Raw_Ceiling        : Velocity;
      Mismatched_Ceiling : Velocity;
      Matched_Ceiling    : Velocity;
      Block              : aliased Execution_Block (2);
   begin
      T.Register;

      Motor_Map (X_Axis, Motor_Name'First) := 1.0 * mm;
      Motor_Map (Y_Axis, Motor_Name'First) := 1.0 * mm;
      Params.Axial_Shapers := [others => (Kind => Input_Shapers.No_Shaper)];
      Raw_Ceiling := Motor_Delta_Ceiling_For_Projection (Params, Motor_Map, 1.0E6 * mm / s);

      Params.Axial_Shapers (Y_Axis) := ZV_Parameters;
      Mismatched_Ceiling := Motor_Delta_Ceiling_For_Projection (Params, Motor_Map, 1.0E6 * mm / s);
      T.Assert
        (Mismatched_Ceiling > 0.0 * mm / s and then Mismatched_Ceiling < 0.8 * Raw_Ceiling,
         "Different CoreXY axis impulses use their conservative combined motor-space gain");

      Block.Params := Params;
      Block.Corners (1) := [others => 0.0 * mm];
      Block.Corners (2) := [X_Axis => 1.0 * mm, Y_Axis => -1.0 * mm, others => 0.0 * mm];
      Block.Primitives (2) := Make_Line_Primitive;
      T.Assert
        (Primitive_Motor_Delta_Ceiling
           (Block'Access,
            Motor_Map,
            2,
            0.0 * mm,
            Primitive_Length (Block'Access, 2),
            1.0E6 * mm / s)
         <= Mismatched_Ceiling,
         "Independent shaping bounds a primitive whose raw coupled-motor projection cancels");

      Params.Axial_Shapers (X_Axis) := ZV_Parameters;
      Matched_Ceiling := Motor_Delta_Ceiling_For_Projection (Params, Motor_Map, 1.0E6 * mm / s);
      T.Assert
        (Matched_Ceiling = Raw_Ceiling,
         "Identical coupled-axis shapers retain the existing motor projection ceiling");

      Motor_Map := [others => [others => Length'Last]];
      Motor_Map (X_Axis, Motor_Name'First) := 1.0E-200 * mm;
      Params.Axial_Shapers := [others => (Kind => Input_Shapers.No_Shaper)];
      T.Assert
        (Motor_Delta_Ceiling_For_Projection (Params, Motor_Map, 1.0 * mm / s) > 0.0 * mm / s,
         "A representable projection coefficient must not overflow while computing its norm");
   end Test_Analytical_Shaper_Motor_Bound;

   procedure Test_Projection_Cancellation_And_Reachability (T : in out Trendy_Test.Operation'Class) is
      Phase : constant Dimensionless := -Dimensionless (Ada.Numerics.Pi) / 2.0 + 1.0E-9;
      Bound : constant Curvature :=
        Maximum_Absolute_Offset_Sine
          (Phase, Phase, 1.0E6 / mm, 1.0E6 / mm, Phase_Shift => 0.0);
      Limits : constant Scalar_Derivative_Limits :=
        (Acceleration_Max => 1.0 * mm / s ** 2,
         Jerk_Max         => 1.0E6 * mm / s ** 3,
         Snap_Max         => 1.0E12 * mm / s ** 4,
         Crackle_Max      => 1.0E18 * mm / s ** 5);
   begin
      T.Register;

      T.Assert
        (Bound >= 4.0E-13 / mm,
         "Offset-sine bounds include error scaled to operands before catastrophic cancellation");
      T.Assert
        (Reachable_Velocity
           (0.0 * mm / s, 299_792_458_000.0 * mm / s, 1.0 * mm, Limits)
         > 0.0 * mm / s,
         "Reachability search finds a positive feasible speed across a very large absolute range");
   end Test_Projection_Cancellation_And_Reachability;

   procedure Test_Corner_Family_Dispatch_And_Fail_Closed (T : in out Trendy_Test.Operation'Class) is
      type Block_Access is access Execution_Block;
      type Workspace_Access is access Planning_Workspace;

      Block     : constant Block_Access := new Execution_Block (3);
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Motor_Map : Motor_Position_Map := [others => [others => Length'Last]];
      Deviation : constant Axial_Deviation_Limits := [others => 100.0 * mm];

      procedure Assert_Dispatched
        (Expected : Corner_Transition_Kind;
         Name     : String;
         T        : in out Trendy_Test.Operation'Class);

      procedure Assert_Dispatched
        (Expected : Corner_Transition_Kind;
         Name     : String;
         T        : in out Trendy_Test.Operation'Class) is
      begin
         Tested_Corner_Blender.Run (Block.all, Motor_Map, Workspace);
         T.Assert
           (Transition_Kind (Block.Corner_Transitions (2)) = Expected,
            Name & " dispatch produced " & Transition_Kind (Block.Corner_Transitions (2))'Image);
         if Expected in Stereographic_Transition | Circular_Transition | Parabolic_Transition | Biarc_Transition then
            T.Assert
              (Arc_Length (Block.Corner_Transitions (2)) > 0.0 * mm,
               Name & " dispatch retained a positive transition");
         end if;
      end Assert_Dispatched;
   begin
      T.Register;

      Reset_Early_Limiter_Block (Block.all);
      Block.Params.Lower_Pos_Limit := [others => -100.0 * mm];
      Block.Params.Upper_Pos_Limit := [others => 100.0 * mm];
      Block.Corners (1) := [X_Axis => -20.0 * mm, others => 0.0 * mm];
      Block.Corners (2) := [others => 0.0 * mm];
      Block.Corners (3) := [Y_Axis => 20.0 * mm, others => 0.0 * mm];
      Block.Primitives := [others => Make_Line_Primitive];
      Motor_Map (X_Axis, Motor_Name'First) := 1.0 * mm;

      Block.Params.Cornering := (others => <>);
      Assert_Dispatched (Stereographic_Transition, "Default stereographic", T);

      Block.Params.Lower_Pos_Limit := [X_Axis | Y_Axis | Z_Axis => 0.0 * mm, E_Axis => -1.0E100 * mm];
      Block.Params.Upper_Pos_Limit := [X_Axis | Y_Axis | Z_Axis => 300.0 * mm, E_Axis => 1.0E100 * mm];
      Block.Corners :=
        [1 => [X_Axis => 1.0 * mm, Y_Axis => 1.0 * mm, others => 0.0 * mm],
         2 => [X_Axis => 0.0 * mm, Y_Axis => 1.0 * mm, others => 0.0 * mm],
         3 => [X_Axis => 1.0 * mm, Y_Axis => 2.0 * mm, others => 0.0 * mm]];
      Assert_Dispatched (Stereographic_Transition, "Default stereographic tangent to lower bounds", T);

      Block.Corners :=
        [1 => [X_Axis => 122.119 * mm, Y_Axis => 117.893 * mm, Z_Axis => 0.25 * mm, E_Axis => 0.01198 * mm],
         2 => [X_Axis => 122.428 * mm, Y_Axis => 117.608 * mm, Z_Axis => 0.25 * mm, E_Axis => 0.02974 * mm],
         3 => [X_Axis => 122.615 * mm, Y_Axis => 117.549 * mm, Z_Axis => 0.25 * mm, E_Axis => 0.03802 * mm]];
      Assert_Dispatched (Stereographic_Transition, "Default stereographic printed corner", T);

      Block.Params.Lower_Pos_Limit := [others => -100.0 * mm];
      Block.Params.Upper_Pos_Limit := [others => 100.0 * mm];
      Block.Corners (1) := [X_Axis => -20.0 * mm, others => 0.0 * mm];
      Block.Corners (2) := [others => 0.0 * mm];
      Block.Corners (3) := [Y_Axis => 20.0 * mm, others => 0.0 * mm];

      Block.Params.Cornering :=
        (Kind                 => Stereographic,
         Stereographic_Params =>
           (Axial_Deviation_Maxes    => Deviation,
            Corner_Miss_Distance_Max => 100.0 * mm,
            Shape_Bias               => 0.0,
            Circularity              => 0.0));
      Assert_Dispatched (Stereographic_Transition, "Stereographic", T);

      Block.Params.Cornering :=
        (Kind            => Circular,
         Circular_Params =>
           (Axial_Deviation_Maxes    => Deviation,
            Corner_Miss_Distance_Max => 100.0 * mm,
            Radius_Max               => 5.0 * mm));
      Assert_Dispatched (Circular_Transition, "Circular", T);

      Block.Params.Cornering :=
        (Kind             => Parabolic,
         Parabolic_Params =>
           (Axial_Deviation_Maxes    => Deviation,
            Corner_Miss_Distance_Max => 100.0 * mm,
            Shape_Bias               => 0.0,
            Trim_Max                 => 5.0 * mm));
      Assert_Dispatched (Parabolic_Transition, "Parabolic", T);

      Block.Params.Cornering :=
        (Kind        => Biarc,
         Biarc_Params =>
           (Axial_Deviation_Maxes    => Deviation,
            Corner_Miss_Distance_Max => 100.0 * mm,
            Shape_Bias               => 0.0,
            Trim_Max                 => 5.0 * mm));
      Assert_Dispatched (Biarc_Transition, "Biarc", T);

      Block.Params.Cornering :=
        (Kind             => Sharp_SCV,
         Sharp_SCV_Params => (Square_Corner_Velocity => 5.0 * mm / s));
      Assert_Dispatched (Sharp_SCV_Transition, "Sharp SCV", T);
      T.Assert
        (Arc_Length (Block.Corner_Transitions (2)) = 0.0 * mm
         and then Policy (Block.Corner_Transitions (2)) = Square_Corner_Velocity
         and then abs (Junction_Velocity_Limit (Block.Corner_Transitions (2)) - 5.0 * mm / s)
                  <= 1.0E-12 * mm / s,
         "Sharp SCV dispatch stores a zero-length junction policy with its angular cap");
      for I in Block.Primitives'Range loop
         T.Assert
           (Block.Primitive_Start_Distances (I) = 0.0 * mm
            and then Block.Primitive_Distances (I) = Primitive_Length (Block.all'Access, I),
            "Sharp SCV does not geometrically trim segment " & I'Image);
      end loop;

      Block.Params.Cornering :=
        (Kind            => Circular,
         Circular_Params =>
           (Axial_Deviation_Maxes    => Deviation,
            Corner_Miss_Distance_Max => 100.0 * mm,
            Radius_Max               => 5.0 * mm));
      Block.Primitives (2) :=
        Make_Helix_Primitive
          (Block.Corners (1), Block.Corners (2), [X_Axis => -10.0 * mm, others => 0.0 * mm],
           Clockwise => True);
      Assert_Dispatched (Hard_Stop_Transition, "Unsupported circular helix", T);
      T.Assert
        (Policy (Block.Corner_Transitions (2)) = Hard_Stop
         and then Arc_Length (Block.Corner_Transitions (2)) = 0.0 * mm,
         "Unsupported geometry fails closed instead of falling back to another family");
      T.Assert
        (Bounds_Are_Zero (Workspace.Corner_Derivative_Bounds (2)),
         "Fail-closed replacement clears stale workspace derivative bounds");

      Block.Primitives (2) := Make_Line_Primitive;
      Block.Params.Cornering :=
        (Kind             => Parabolic,
         Parabolic_Params =>
           (Axial_Deviation_Maxes    => Deviation,
            Corner_Miss_Distance_Max => 100.0 * mm,
            Shape_Bias               => 0.0,
            Trim_Max                 => 5.0 * mm));
      Assert_Dispatched (Parabolic_Transition, "Post-failure parabolic", T);
      T.Assert
        (not Bounds_Are_Zero (Workspace.Corner_Derivative_Bounds (2)),
         "A successful later family does not retain stale hard-stop workspace state");
   end Test_Corner_Family_Dispatch_And_Fail_Closed;

   procedure Test_Biarc_Helix_Line_Dispatch (T : in out Trendy_Test.Operation'Class) is
      type Block_Access is access Execution_Block;
      type Workspace_Access is access Planning_Workspace;

      Block     : constant Block_Access := new Execution_Block (3);
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Motor_Map : constant Motor_Position_Map := [others => [others => Length'Last]];
      Radius    : constant Length := 20.0 * mm;
      Deviation : constant Axial_Deviation_Limits := [others => 100.0 * mm];

      procedure Configure;
      procedure Assert_Dispatched (Name : String; Helix_Segment : Finishing_Corners_Index);

      procedure Configure is
      begin
         Reset_Early_Limiter_Block (Block.all);
         Block.Params.Lower_Pos_Limit := [others => -100.0 * mm];
         Block.Params.Upper_Pos_Limit := [others => 100.0 * mm];
         Block.Params.Cornering :=
           (Kind        => Biarc,
            Biarc_Params =>
              (Axial_Deviation_Maxes    => Deviation,
               Corner_Miss_Distance_Max => 100.0 * mm,
               Shape_Bias               => 0.0,
               Trim_Max                 => 3.0 * mm));
      end Configure;

      procedure Assert_Dispatched (Name : String; Helix_Segment : Finishing_Corners_Index) is
         Helix_Length : constant Length := Primitive_Length (Block.all'Access, Helix_Segment);
      begin
         T.Assert
           (Block.Primitives (Helix_Segment).Kind = Helix_Primitive_Kind and then Helix_Length > 0.0 * mm,
            Name & " retains a usable helix primitive");
         Tested_Corner_Blender.Run (Block.all, Motor_Map, Workspace);
         T.Assert
           (Transition_Kind (Block.Corner_Transitions (2)) = Biarc_Transition,
            Name & " dispatches a certifiable helix/line junction to Biarc");

         if Transition_Kind (Block.Corner_Transitions (2)) = Biarc_Transition then
            declare
               Transition_Length : constant Length := Arc_Length (Block.Corner_Transitions (2));
               Expected_Start    : constant Position :=
                 Primitive_Point_At_Distance
                   (Block.all'Access,
                    2,
                    Block.Primitive_Start_Distances (2) + Block.Primitive_Distances (2));
               Expected_Finish   : constant Position :=
                 Primitive_Point_At_Distance
                   (Block.all'Access, 3, Block.Primitive_Start_Distances (3));
            begin
               T.Assert (Transition_Length > 0.0 * mm, Name & " retains a positive Biarc transition");
               T.Assert
                 (Point_Distance
                    (Point_At_Distance (Block.Corner_Transitions (2), 0.0 * mm), Expected_Start)
                  <= 1.0E-8 * mm,
                  Name & " Biarc starts on the trimmed incoming primitive");
               T.Assert
                 (Point_Distance
                    (Point_At_Distance (Block.Corner_Transitions (2), Transition_Length), Expected_Finish)
                  <= 1.0E-8 * mm,
                  Name & " Biarc finishes on the trimmed outgoing primitive");
            end;
         end if;
      end Assert_Dispatched;
   begin
      T.Register;

      Configure;
      Block.Corners (1) := [X_Axis => -Radius, Y_Axis => Radius, others => 0.0 * mm];
      Block.Corners (2) := [others => 0.0 * mm];
      Block.Corners (3) := [Y_Axis => Radius, others => 0.0 * mm];
      Block.Primitives (2) :=
        Make_Helix_Primitive
          (Block.Corners (1), Block.Corners (2), [Y_Axis => Radius, others => 0.0 * mm], Clockwise => False);
      Block.Primitives (3) := Make_Line_Primitive;
      Assert_Dispatched ("Incoming helix", 2);

      Configure;
      Block.Corners (1) := [X_Axis => -Radius, others => 0.0 * mm];
      Block.Corners (2) := [others => 0.0 * mm];
      Block.Corners (3) := [X_Axis => Radius, Y_Axis => Radius, others => 0.0 * mm];
      Block.Primitives (2) := Make_Line_Primitive;
      Block.Primitives (3) :=
        Make_Helix_Primitive
          (Block.Corners (2), Block.Corners (3), [X_Axis => Radius, others => 0.0 * mm], Clockwise => True);
      Assert_Dispatched ("Outgoing helix", 3);
   end Test_Biarc_Helix_Line_Dispatch;

   procedure Test_Profile_Window_Transition_Bounds (T : in out Trendy_Test.Operation'Class) is
      type Block_Access is access Execution_Block;
      type Workspace_Access is access Planning_Workspace;

      Block        : constant Block_Access := new Execution_Block (4);
      Workspace    : constant Workspace_Access := new Planning_Workspace;
      Start_Result : constant Construction_Result :=
        Create_Circular
          ([X_Axis => -1.0 * mm, others => 0.0 * mm],
           [others => 0.0 * mm],
           [Y_Axis => 1.0 * mm, others => 0.0 * mm],
           Maximum_Radius => 2.0 * mm);
      End_Result   : constant Construction_Result :=
        Create_Circular
          ([Y_Axis => 9.0 * mm, others => 0.0 * mm],
           [Y_Axis => 10.0 * mm, others => 0.0 * mm],
           [X_Axis => 1.0 * mm, Y_Axis => 10.0 * mm, others => 0.0 * mm],
           Maximum_Radius => 2.0 * mm);

      procedure Merge
        (Target : in out Unit_Speed_Axial_Derivative_Bounds;
         Source : Unit_Speed_Axial_Derivative_Bounds);

      procedure Merge
        (Target : in out Unit_Speed_Axial_Derivative_Bounds;
         Source : Unit_Speed_Axial_Derivative_Bounds) is
      begin
         for Axis in Axis_Name loop
            Target.Velocity (Axis) := Dimensionless'Max (Target.Velocity (Axis), Source.Velocity (Axis));
            Target.Acceleration (Axis) :=
              Curvature'Max (Target.Acceleration (Axis), Source.Acceleration (Axis));
            Target.Jerk (Axis) := Curvature_To_2'Max (Target.Jerk (Axis), Source.Jerk (Axis));
            Target.Snap (Axis) := Curvature_To_3'Max (Target.Snap (Axis), Source.Snap (Axis));
            Target.Crackle (Axis) := Curvature_To_4'Max (Target.Crackle (Axis), Source.Crackle (Axis));
         end loop;
      end Merge;
   begin
      T.Register;
      T.Assert
        (Start_Result.Status = Construction_Success and then End_Result.Status = Construction_Success,
         "Profile-window fixtures construct both circular transitions");
      if Start_Result.Status /= Construction_Success or else End_Result.Status /= Construction_Success then
         return;
      end if;

      Reset_Early_Limiter_Block (Block.all);
      Block.Corners :=
        [1 => [X_Axis => -10.0 * mm, others => 0.0 * mm],
         2 => [others => 0.0 * mm],
         3 => [Y_Axis => 10.0 * mm, others => 0.0 * mm],
         4 => [X_Axis => 10.0 * mm, Y_Axis => 10.0 * mm, others => 0.0 * mm]];
      Block.Primitives := [others => Make_Line_Primitive];
      Block.Corner_Transitions (2) := To_Evaluator (Start_Result.Transition);
      Block.Corner_Transitions (3) := To_Evaluator (End_Result.Transition);
      Block.Primitive_Start_Distances (3) := 1.0 * mm;
      Block.Primitive_Distances (3) := 8.0 * mm;

      declare
         Start_Transition : constant Corner_Transition_Evaluator := Block.Corner_Transitions (2);
         End_Transition   : constant Corner_Transition_Evaluator := Block.Corner_Transitions (3);
         Start_Length     : constant Length := Segment_Start_Transition_Distance (Block.all'Access, 3);
         End_Length       : constant Length := Segment_End_Transition_Distance (Block.all'Access, 3);
         Middle           : constant Length := Segment_Straight_Distance (Block.all'Access, 3);
         End_Start        : constant Length := Start_Length + Middle;
         Start_Window     : constant Profile_Window :=
           (Start_Distance => 0.10 * Start_Length, Distance => 0.25 * Start_Length);
         End_Window       : constant Profile_Window :=
           (Start_Distance => End_Start + 0.10 * End_Length,
            Distance       => 0.25 * End_Length);
         End_Range_Start  : constant Length := End_Window.Start_Distance - End_Start;
         End_Range_Finish : constant Length :=
           End_Window.Start_Distance + End_Window.Distance - End_Start;
         Start_Expected   : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds
             (Start_Transition,
              Split_Distance (Start_Transition) + Start_Window.Start_Distance,
              Split_Distance (Start_Transition) + Start_Window.Start_Distance + Start_Window.Distance);
         End_Expected     : constant Unit_Speed_Axial_Derivative_Bounds :=
           Derivative_Bounds (End_Transition, End_Range_Start, End_Range_Finish);
         Start_Actual     : constant Unit_Speed_Axial_Derivative_Bounds :=
           Window_Axial_Derivative_Bounds (Block.all'Access, Workspace, 3, Start_Window);
         End_Actual       : constant Unit_Speed_Axial_Derivative_Bounds :=
           Window_Axial_Derivative_Bounds (Block.all'Access, Workspace, 3, End_Window);
      begin
         T.Assert
           (Start_Actual = Start_Expected,
            "A start-transition-only profile window uses that exact ranged derivative bound");
         T.Assert
           (End_Actual = End_Expected,
            "An end-transition-only profile window uses that exact ranged derivative bound");
         T.Assert
           (Start_Expected.Velocity (X_Axis) < Derivative_Bounds (Start_Transition).Velocity (X_Axis),
            "The start-transition window does not widen its X-velocity bound to the whole curve");
         T.Assert
           (End_Expected.Velocity (X_Axis) < Derivative_Bounds (End_Transition).Velocity (X_Axis),
            "The end-transition window does not widen its X-velocity bound to the whole curve");

         declare
            Spanning_Window : constant Profile_Window :=
              (Start_Distance => 0.75 * Start_Length,
               Distance       => 0.25 * Start_Length + Middle + 0.25 * End_Length);
            Expected : Unit_Speed_Axial_Derivative_Bounds := (others => <>);
            Actual   : constant Unit_Speed_Axial_Derivative_Bounds :=
              Window_Axial_Derivative_Bounds (Block.all'Access, Workspace, 3, Spanning_Window);
         begin
            Merge
              (Expected,
               Derivative_Bounds
                 (Start_Transition,
                  Split_Distance (Start_Transition) + 0.75 * Start_Length,
                  Arc_Length (Start_Transition)));
            Merge
              (Expected,
               Primitive_Derivative_Bounds
                 (Block.all'Access, 3, Block.Primitive_Start_Distances (3), Middle));
            Merge
              (Expected,
               Derivative_Bounds
                 (End_Transition,
                  0.0 * mm,
                  Spanning_Window.Start_Distance + Spanning_Window.Distance - End_Start));
            T.Assert
              (Actual = Expected,
               "A spanning profile window merges only its two transition portions and retained primitive range");
         end;
      end;
   end Test_Profile_Window_Transition_Bounds;

   procedure Test_Generated_Transition_Motor_Projection (T : in out Trendy_Test.Operation'Class) is
      type Block_Access is access Execution_Block;
      type Workspace_Access is access Planning_Workspace;

      Block     : constant Block_Access := new Execution_Block (3);
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Motor_Map : Motor_Position_Map := [others => [others => Length'Last]];
      Deviation : constant Axial_Deviation_Limits := [others => 100.0 * mm];
      Max_Vel   : constant Velocity := 1.0E6 * mm / s;
   begin
      T.Register;

      Reset_Early_Limiter_Block (Block.all);
      Block.Params.Lower_Pos_Limit := [others => -100.0 * mm];
      Block.Params.Upper_Pos_Limit := [others => 100.0 * mm];
      Block.Params.Cornering :=
        (Kind            => Circular,
         Circular_Params =>
           (Axial_Deviation_Maxes    => Deviation,
            Corner_Miss_Distance_Max => 100.0 * mm,
            Radius_Max               => 5.0 * mm));
      Block.Corners :=
        [1 => [X_Axis => -20.0 * mm, others => 0.0 * mm],
         2 => [others => 0.0 * mm],
         3 => [Y_Axis => 20.0 * mm, others => 0.0 * mm]];
      Block.Primitives := [others => Make_Line_Primitive];
      Motor_Map (X_Axis, Motor_Name'First) := 1.0 * mm;

      Tested_Corner_Blender.Run (Block.all, Motor_Map, Workspace);
      T.Assert
        (Transition_Kind (Block.Corner_Transitions (2)) = Circular_Transition,
         "Motor-projection fixture retains its generated circular transition");

      if Transition_Kind (Block.Corner_Transitions (2)) = Circular_Transition then
         declare
            Start_Length : constant Length := Segment_Start_Transition_Distance (Block.all'Access, 3);
            Middle       : constant Length := Segment_Straight_Distance (Block.all'Access, 3);
            Transition_Window : constant Profile_Window :=
              (Start_Distance => 0.0 * mm, Distance => 0.5 * Start_Length);
            Primitive_Window  : constant Profile_Window :=
              (Start_Distance => Start_Length, Distance => Middle);
            Expected_Transition_Limit : constant Velocity :=
              Motor_Delta_Ceiling_For_Projection (Block.Params, Motor_Map, Max_Vel);
            Transition_Limit : constant Velocity :=
              Motor_Delta_Ceiling_For_Window (Block.all'Access, Motor_Map, 3, Transition_Window, Max_Vel);
            Primitive_Limit : constant Velocity :=
              Motor_Delta_Ceiling_For_Window (Block.all'Access, Motor_Map, 3, Primitive_Window, Max_Vel);
            Spanning_Limit : constant Velocity :=
              Motor_Delta_Ceiling_For_Window
                (Block.all'Access,
                 Motor_Map,
                 3,
                 (Start_Distance => 0.0 * mm, Distance => Start_Length + Middle),
                 Max_Vel);
         begin
            T.Assert (Start_Length > 0.0 * mm and then Middle > 0.0 * mm, "Generated path has both tested portions");
            T.Assert
              (Transition_Limit = Expected_Transition_Limit and then Transition_Limit < Max_Vel,
               "The generated transition projects its changing tangent into the X motor ceiling");
            T.Assert
              (Primitive_Limit = Max_Vel,
               "The retained outgoing Y primitive does not spuriously project into the X-only motor");
            T.Assert
              (Spanning_Limit = Transition_Limit,
               "A window spanning the generated transition and primitive retains the transition motor ceiling");
         end;
      end if;
   end Test_Generated_Transition_Motor_Projection;

   procedure Test_Corner_Transition_Travel_Bounds (T : in out Trendy_Test.Operation'Class) is
      type Block_Access is access Execution_Block;
      type Workspace_Access is access Planning_Workspace;

      Block     : constant Block_Access := new Execution_Block (3);
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Motor_Map : constant Motor_Position_Map := [others => [others => Length'Last]];
      Deviation : constant Axial_Deviation_Limits := [others => 100.0 * mm];
      Full_Incoming_Trim, Bounded_Incoming_Trim : Length;
      Full_Outgoing_Trim, Bounded_Outgoing_Trim : Length;

      function Incoming_Trim return Length;
      function Outgoing_Trim return Length;
      procedure Run_Case (Lower_X, Upper_Y : Length);

      function Incoming_Trim return Length is
      begin
         return
           Point_Distance
             (Block.Corners (2), Point_At_Distance (Block.Corner_Transitions (2), 0.0 * mm));
      end Incoming_Trim;

      function Outgoing_Trim return Length is
         Transition_Length : constant Length := Arc_Length (Block.Corner_Transitions (2));
      begin
         return
           Point_Distance
             (Point_At_Distance (Block.Corner_Transitions (2), Transition_Length), Block.Corners (2));
      end Outgoing_Trim;

      procedure Run_Case (Lower_X, Upper_Y : Length) is
      begin
         Reset_Early_Limiter_Block (Block.all);
         Block.Params.Lower_Pos_Limit := [others => -100.0 * mm];
         Block.Params.Upper_Pos_Limit := [others => 100.0 * mm];
         Block.Params.Lower_Pos_Limit (X_Axis) := Lower_X;
         Block.Params.Upper_Pos_Limit (Y_Axis) := Upper_Y;
         Block.Params.Cornering :=
           (Kind            => Circular,
            Circular_Params =>
              (Axial_Deviation_Maxes    => Deviation,
               Corner_Miss_Distance_Max => 100.0 * mm,
               Radius_Max               => 10.0 * mm));
         Block.Corners (1) := [X_Axis => -10.0 * mm, others => 0.0 * mm];
         Block.Corners (2) := [others => 0.0 * mm];
         Block.Corners (3) := [Y_Axis => 10.0 * mm, others => 0.0 * mm];
         Block.Primitives := [others => Make_Line_Primitive];
         Tested_Corner_Blender.Run (Block.all, Motor_Map, Workspace);
      end Run_Case;
   begin
      T.Register;

      Run_Case (Lower_X => -100.0 * mm, Upper_Y => 100.0 * mm);
      T.Assert
        (Transition_Kind (Block.Corner_Transitions (2)) = Circular_Transition,
         "Wide travel bounds retain a circular transition");
      Full_Incoming_Trim := Incoming_Trim;
      Full_Outgoing_Trim := Outgoing_Trim;
      T.Assert
        (Full_Incoming_Trim > 9.0 * mm and then Full_Outgoing_Trim > 9.0 * mm,
         "Wide travel bounds retain the full requested transition");

      Run_Case (Lower_X => -5.0 * mm, Upper_Y => 100.0 * mm);
      T.Assert
        (Transition_Kind (Block.Corner_Transitions (2)) = Circular_Transition,
         "A finite incoming bound violation is repaired instead of immediately hard-stopping");
      Bounded_Incoming_Trim := Incoming_Trim;
      T.Assert
        (Bounded_Incoming_Trim > 0.0 * mm
         and then Bounded_Incoming_Trim < Full_Incoming_Trim - 0.5 * mm,
         "The incoming travel bound shrinks the generated transition");

      Run_Case (Lower_X => -100.0 * mm, Upper_Y => 5.0 * mm);
      T.Assert
        (Transition_Kind (Block.Corner_Transitions (2)) = Circular_Transition,
         "A finite outgoing bound violation is repaired instead of immediately hard-stopping");
      Bounded_Outgoing_Trim := Outgoing_Trim;
      T.Assert
        (Bounded_Outgoing_Trim > 0.0 * mm
         and then Bounded_Outgoing_Trim < Full_Outgoing_Trim - 0.5 * mm,
         "The outgoing travel bound shrinks the generated transition");

      Run_Case (Lower_X => 0.0 * mm, Upper_Y => 100.0 * mm);
      T.Assert
        (Transition_Kind (Block.Corner_Transitions (2)) = Hard_Stop_Transition
         and then Policy (Block.Corner_Transitions (2)) = Hard_Stop,
         "An unrepairable enabled-side envelope fails closed");
      T.Assert
        (Bounds_Are_Zero (Workspace.Corner_Derivative_Bounds (2)),
         "Fail-closed travel-bound rejection clears transition workspace state");

      Run_Case (Lower_X => -100.0 * mm, Upper_Y => 100.0 * mm);
      T.Assert
        (Transition_Kind (Block.Corner_Transitions (2)) = Circular_Transition
         and then Incoming_Trim > 9.0 * mm
         and then Outgoing_Trim > 9.0 * mm,
         "A later valid run replaces the stale hard stop with the requested family");
   end Test_Corner_Transition_Travel_Bounds;

   procedure Test_Helix_Travel_Bounds_Are_Transactional (T : in out Trendy_Test.Operation'Class) is
      Params       : Kinematic_Parameters := (others => <>);
      Block        : aliased Execution_Block;
      Reset_Called : Boolean;
      Finish       : constant Position := [X_Axis => 1.0 * mm, Y_Axis => 1.0 * mm, others => 0.0 * mm];
      Center       : constant Position := [X_Axis => 1.0 * mm, others => 0.0 * mm];
      Rejected     : Boolean := False;
   begin
      T.Register;

      Params.Lower_Pos_Limit := [others => -10.0 * mm];
      Params.Upper_Pos_Limit := [others => 10.0 * mm];
      Params.Upper_Pos_Limit (X_Axis) := 1.5 * mm;
      Params.Lower_Pos_Limit (Y_Axis) := -0.5 * mm;
      Tested_Preprocessor.Setup (Params);

      begin
         Tested_Preprocessor.Enqueue
           ((Kind             => Helix_Move_Kind,
             Dwell_After      => 0.0 * s,
             Pos              => Finish,
             Center           => Center,
             Clockwise        => False,
             Feedrate         => 1.0 * mm / s));
      exception
         when Out_Of_Bounds_Error =>
            Rejected := True;
      end;
      T.Assert (Rejected, "A major helix with legal endpoints but illegal interior extrema is rejected");

      Tested_Preprocessor.Enqueue
        ((Kind             => Helix_Move_Kind,
          Dwell_After      => 0.0 * s,
          Pos              => Finish,
          Center           => Center,
          Clockwise        => True,
          Feedrate         => 1.0 * mm / s));
      Tested_Preprocessor.Enqueue
        ((Kind                  => Flush_Kind,
          Flush_Resetting_Data => Flush_Resetting_Data_Type_Default,
          Is_Homing_Move       => False));
      Tested_Preprocessor.Run (Block, Reset_Called);

      T.Assert (not Reset_Called, "Accepted helix produces a normal motion block");
      T.Assert
        (Block.N_Corners = 2 and then Block.Corners (1) = Initial_Position and then Block.Corners (2) = Finish,
         "Rejected helix neither enqueues a corner nor advances the queued start position");
   end Test_Helix_Travel_Bounds_Are_Transactional;

   procedure Test_Line_Primitive_Tangent_Jet_Identities (T : in out Trendy_Test.Operation'Class) is
      Block : aliased Execution_Block (2);
   begin
      T.Register;

      Block.Corners (1) :=
        [X_Axis => -2.0 * mm, Y_Axis => 5.0 * mm, Z_Axis => 1.0 * mm, E_Axis => -3.0 * mm];
      Block.Corners (2) :=
        [X_Axis => 7.0 * mm, Y_Axis => -4.0 * mm, Z_Axis => 6.0 * mm, E_Axis => 2.0 * mm];
      Block.Primitives (2) := Make_Line_Primitive;

      for Sample_Index in Sample_Fractions'Range loop
         declare
            Jet : constant Endpoint_Tangent_Jet :=
              Primitive_Derivative_Jets_At_Distance
                (Block'Access,
                 2,
                 Sample_Fractions (Sample_Index) * Primitive_Length (Block'Access, 2));
            Sample_Name : constant String := "line sample" & Sample_Index'Image;
         begin
            Check_Unit_Tangent_Identities (Jet, Sample_Name, T);

            for Axis in Axis_Name loop
               T.Assert (Jet.Tangent_Derivative_1 (Axis) = 0.0 / mm, Sample_Name & " T' should be zero");
               T.Assert (Jet.Tangent_Derivative_2 (Axis) = 0.0 / mm ** 2, Sample_Name & " T'' should be zero");
               T.Assert (Jet.Tangent_Derivative_3 (Axis) = 0.0 / mm ** 3, Sample_Name & " T''' should be zero");
            end loop;
         end;
      end loop;
   end Test_Line_Primitive_Tangent_Jet_Identities;

   procedure Test_Per_Axis_Deviation_Corridor (T : in out Trendy_Test.Operation'Class) is
      type Block_Access is access Execution_Block;
      type Workspace_Access is access Planning_Workspace;

      Block     : constant Block_Access := new Execution_Block (3);
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Motor_Map : Motor_Position_Map := [others => [others => Length'Last]];
      Limits    : constant Axial_Deviation_Limits :=
        [X_Axis => 0.12 * mm,
         Y_Axis => 0.50 * mm,
         Z_Axis => 0.0 * mm,
         E_Axis => 0.0 * mm];
   begin
      T.Register;

      Reset_Early_Limiter_Block (Block.all);
      Block.Params.Cornering :=
        (Kind                 => Stereographic,
         Stereographic_Params =>
           (Axial_Deviation_Maxes    => Limits,
            Corner_Miss_Distance_Max => 100.0 * mm,
            Shape_Bias               => 1.0,
            Circularity              => 0.0));
      Motor_Map (X_Axis, Motor_Name'First) := 1.0 * mm;
      Block.Corners (1) := [X_Axis => -20.0 * mm, others => 0.0 * mm];
      Block.Corners (2) := [others => 0.0 * mm];
      Block.Corners (3) := [Y_Axis => 20.0 * mm, others => 0.0 * mm];
      for I in Block.Primitives'Range loop
         Block.Primitives (I) := Make_Line_Primitive;
         Block.Corner_Dwell_Times (I) := 0.0 * s;
      end loop;

      Tested_Corner_Blender.Run (Block.all, Motor_Map, Workspace);

      declare
         Curve_Length  : constant Length := Arc_Length (Block.Corner_Transitions (2));
         Curve_Start   : constant Position := Point_At_Distance (Block.Corner_Transitions (2), 0.0 * mm);
         Curve_Finish  : constant Position := Point_At_Distance (Block.Corner_Transitions (2), Curve_Length);
         Incoming_Trim : constant Length := Point_Distance (Block.Corners (2), Curve_Start);
         Outgoing_Trim : constant Length := Point_Distance (Curve_Finish, Block.Corners (2));
      begin
         T.Assert (Curve_Length > 0.0 * mm, "Per-axis corridor should retain a nonzero blend");
         T.Assert
           (Outgoing_Trim > Incoming_Trim,
            "Positive shape bias should retain a longer outgoing trim");
         T.Assert
           (Outgoing_Trim <= 20.0 * Incoming_Trim,
            "Allocated trim ratio obeys the 20:1 cap");

         if Curve_Length > 0.0 * mm then
            for I in 0 .. 128 loop
               declare
                  Point : constant Position :=
                    Point_At_Distance
                      (Block.Corner_Transitions (2),
                       Curve_Length * Dimensionless (I) / 128.0);
                  In_Incoming_Corridor : constant Boolean :=
                    Point (Y_Axis) <= Limits (Y_Axis) + 2.0E-3 * mm;
                  In_Outgoing_Corridor : constant Boolean :=
                    -Point (X_Axis) <= Limits (X_Axis) + 2.0E-3 * mm;
               begin
                  T.Assert
                    (Point (X_Axis) <= 2.0E-3 * mm and then Point (Y_Axis) >= -2.0E-3 * mm,
                     "Default no-bulge blend stays inside the line-corner quadrant");
                  T.Assert
                    (In_Incoming_Corridor or else In_Outgoing_Corridor,
                     "Every sample lies in the union of the per-axis line corridors");
                  T.Assert
                    (Point (Z_Axis) = 0.0 * mm and then Point (E_Axis) = 0.0 * mm,
                     "Zero limits on structurally unused axes remain exact");
               end;
            end loop;
         end if;
      end;

      Block.Corner_Dwell_Times (2) := 1.0 * s;
      Tested_Corner_Blender.Run (Block.all, Motor_Map, Workspace);
      declare
         Curve_Length : constant Length := Arc_Length (Block.Corner_Transitions (2));
         Curve_Start  : constant Position := Point_At_Distance (Block.Corner_Transitions (2), 0.0 * mm);
         Curve_Finish : constant Position := Point_At_Distance (Block.Corner_Transitions (2), Curve_Length);
      begin
         T.Assert (Curve_Length = 0.0 * mm, "Dwell should replace the blend with a hard anchor");
         T.Assert
           (Curve_Start = Block.Corners (2) and then Curve_Finish = Block.Corners (2),
            "Hard-anchor evaluator endpoints should equal the original corner");
      end;

      Block.Corner_Dwell_Times (2) := 0.0 * s;
      Tested_Corner_Blender.Run (Block.all, Motor_Map, Workspace);
      T.Assert
        (Arc_Length (Block.Corner_Transitions (2)) > 0.0 * mm,
         "A later blend should not reuse stale hard-anchor workspace state");
   end Test_Per_Axis_Deviation_Corridor;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Early_Limiter_Helix_Ignore_E'Unrestricted_Access,
         Test_Early_Limiter_Uses_Executed_Distance'Unrestricted_Access,
         Test_Helix_Primitive_Tangent_Jet_Identities'Unrestricted_Access,
         Test_Tiny_Helix_And_Scaled_Derivatives'Unrestricted_Access,
         Test_Analytical_Shaper_Motor_Bound'Unrestricted_Access,
         Test_Projection_Cancellation_And_Reachability'Unrestricted_Access,
         Test_Corner_Family_Dispatch_And_Fail_Closed'Unrestricted_Access,
         Test_Biarc_Helix_Line_Dispatch'Unrestricted_Access,
         Test_Profile_Window_Transition_Bounds'Unrestricted_Access,
         Test_Generated_Transition_Motor_Projection'Unrestricted_Access,
         Test_Corner_Transition_Travel_Bounds'Unrestricted_Access,
         Test_Helix_Travel_Bounds_Are_Transactional'Unrestricted_Access,
         Test_Line_Primitive_Tangent_Jet_Identities'Unrestricted_Access,
         Test_Per_Axis_Deviation_Corridor'Unrestricted_Access];
   end All_Tests;

end Prunt.Motion_Planner.Planner.Test;
