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
with Prunt.Motion_Planner.Planner.Kinematic_Limiter;
with Prunt.Motion_Planner.Planner.Extrusion_Density_Normalizer;
with Prunt.Motion_Planner.Planner.Feedrate_Profile_Generator;
with Trendy_Test; use Trendy_Test;

package body Prunt.Motion_Planner.Planner.Test is

   pragma Extensions_Allowed (On);

   function Rectangular_Bounds (Lower, Upper : Position) return Workspace_Bounds;

   function Rectangular_Bounds (Lower, Upper : Position) return Workspace_Bounds is
     (Kind    => Rectangular_Workspace,
      Lower_Z => Lower (Z_Axis),
      Upper_Z => Upper (Z_Axis),
      Lower_E => Lower (E_Axis),
      Upper_E => Upper (E_Axis),
      Lower_X => Lower (X_Axis),
      Upper_X => Upper (X_Axis),
      Lower_Y => Lower (Y_Axis),
      Upper_Y => Upper (Y_Axis));

   use Ada.Numerics.Long_Elementary_Functions;
   package Tested_Corner_Blender is new Corner_Blender;
   package Tested_Density_Normalizer is new Extrusion_Density_Normalizer;
   package Tested_Early_Kinematic_Limiter is new Early_Kinematic_Limiter;
   package Tested_Kinematic_Limiter is new Kinematic_Limiter;
   package Tested_Profile_Generator is new Feedrate_Profile_Generator;
   package Tested_Preprocessor is new Preprocessor;
   package Boundary_Preprocessor is new Preprocessor;

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

   procedure Normalize_And_Blend
     (Block : aliased in out Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace : not null access Planning_Workspace)
   is
   begin
      Tested_Density_Normalizer.Run (Block, Workspace);
      Tested_Corner_Blender.Run (Block, Motor_Map, Workspace);
   end Normalize_And_Blend;

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
      Block.Extrusion_Junction_Corrections := [others => <>];
      Block.Extrusion_Reference_Positions := [others => 0.0 * mm];
      Block.Extrusion_Densities := [others => 0.0];
      Block.Kind := Motion_Block_Kind;
      Block.Flush_Resetting_Data := Flush_Resetting_Data_Type_Default;
      Block.Next_Block_Pos := [others => 0.0 * mm];
      Block.Params := (others => <>);
      --  Existing fixtures exercise literal commanded densities; rounding has dedicated tests below.
      Block.Params.Extrusion_Rounding_Tolerance := 0.0 * mm;
      Block.Params.Bounds := Rectangular_Bounds ([others => -1.0E100 * mm], [others => 1.0E100 * mm]);
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
      Block.Profile_Windows := [others => <>];
      Block.Profile_Ends := [others => <>];
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
            T.Assert (Jet.Tangent (E_Axis) = 0.0, Sample_Name & " spatial geometry must exclude E");

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
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Radius    : constant Length := 10.0 * mm;
      Start_Pos : constant Position :=
        [X_Axis => Radius, Y_Axis => 0.0 * mm, Z_Axis => 0.0 * mm, E_Axis => 0.0 * mm];
      End_Pos   : constant Position :=
        [X_Axis => 0.0 * mm, Y_Axis => Radius, Z_Axis => 0.0 * mm, E_Axis => 10.0 * mm];
      Center    : constant Position := [others => 0.0 * mm];
      Commanded_XYZ_Feedrate : constant Velocity := 100.0 * mm / s;
      XYZ_Path_Length        : constant Length := Radius * Dimensionless (Ada.Numerics.Pi / 2.0);
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
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

      Normalize_And_Blend (Block, Motor_Map, Workspace);
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
      Motor_Map         : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
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
      Motor_Map    : Motor_Position_Map := [others => [others => 0.0 / mm]];
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

      Motor_Map (X_Axis, Motor_Name'First) := 1.0 / mm;
      Motor_Map (Y_Axis, Motor_Name'First) := 1.0 / mm;
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

      Motor_Map := [others => [others => 0.0 / mm]];
      Motor_Map (X_Axis, Motor_Name'First) := 1.0E200 / mm;
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
      Motor_Map : Motor_Position_Map := [others => [others => 0.0 / mm]];
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
         Normalize_And_Blend (Block.all, Motor_Map, Workspace);
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
      Block.Params.Bounds := Rectangular_Bounds ([others => -100.0 * mm], [others => 100.0 * mm]);
      Block.Corners (1) := [X_Axis => -20.0 * mm, others => 0.0 * mm];
      Block.Corners (2) := [others => 0.0 * mm];
      Block.Corners (3) := [Y_Axis => 20.0 * mm, others => 0.0 * mm];
      Block.Primitives := [others => Make_Line_Primitive];
      Motor_Map (X_Axis, Motor_Name'First) := 1.0 / mm;

      Block.Params.Cornering := (others => <>);
      Assert_Dispatched (Stereographic_Transition, "Default stereographic", T);

      Block.Params.Bounds :=
        Rectangular_Bounds
          ([X_Axis | Y_Axis | Z_Axis => 0.0 * mm, E_Axis => -1.0E100 * mm],
           [X_Axis | Y_Axis | Z_Axis => 300.0 * mm, E_Axis => 1.0E100 * mm]);
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

      Block.Params.Bounds := Rectangular_Bounds ([others => -100.0 * mm], [others => 100.0 * mm]);
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
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Radius    : constant Length := 20.0 * mm;
      Deviation : constant Axial_Deviation_Limits := [others => 100.0 * mm];

      procedure Configure;
      procedure Assert_Dispatched (Name : String; Helix_Segment : Finishing_Corners_Index);

      procedure Configure is
      begin
         Reset_Early_Limiter_Block (Block.all);
         Block.Params.Bounds := Rectangular_Bounds ([others => -100.0 * mm], [others => 100.0 * mm]);
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
         Normalize_And_Blend (Block.all, Motor_Map, Workspace);
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

      function With_Stationary_Extrusion
        (Bounds : Unit_Speed_Axial_Derivative_Bounds) return Unit_Speed_Axial_Derivative_Bounds;

      function With_Stationary_Extrusion
        (Bounds : Unit_Speed_Axial_Derivative_Bounds) return Unit_Speed_Axial_Derivative_Bounds is
      begin
         --  The fixture commands no extrusion. E therefore has exact zero bounds from its reference,
         --  independently of any outward rounding in the raw corner geometry's E components.
         return Result : Unit_Speed_Axial_Derivative_Bounds := Bounds do
            Result.Velocity (E_Axis) := 0.0;
            Result.Acceleration (E_Axis) := 0.0 / mm;
            Result.Jerk (E_Axis) := 0.0 / mm ** 2;
            Result.Snap (E_Axis) := 0.0 / mm ** 3;
            Result.Crackle (E_Axis) := 0.0 / mm ** 4;
         end return;
      end With_Stationary_Extrusion;

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
           (Start_Actual = With_Stationary_Extrusion (Start_Expected),
            "A start-transition-only profile window uses that exact ranged derivative bound");
         T.Assert
           (End_Actual = With_Stationary_Extrusion (End_Expected),
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
              (Actual = With_Stationary_Extrusion (Expected),
               "A spanning profile window merges only its two transition portions and retained primitive range");
         end;
      end;
   end Test_Profile_Window_Transition_Bounds;

   procedure Test_Generated_Transition_Motor_Projection (T : in out Trendy_Test.Operation'Class) is
      type Block_Access is access Execution_Block;
      type Workspace_Access is access Planning_Workspace;

      Block     : constant Block_Access := new Execution_Block (3);
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Motor_Map : Motor_Position_Map := [others => [others => 0.0 / mm]];
      Deviation : constant Axial_Deviation_Limits := [others => 100.0 * mm];
      Max_Vel   : constant Velocity := 1.0E6 * mm / s;
   begin
      T.Register;

      Reset_Early_Limiter_Block (Block.all);
      Block.Params.Bounds := Rectangular_Bounds ([others => -100.0 * mm], [others => 100.0 * mm]);
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
      Motor_Map (X_Axis, Motor_Name'First) := 1.0 / mm;

      Normalize_And_Blend (Block.all, Motor_Map, Workspace);
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
              Motor_Delta_Ceiling_For_Window (Block.all'Access, Workspace, Motor_Map, 3, Transition_Window, Max_Vel);
            Primitive_Limit : constant Velocity :=
              Motor_Delta_Ceiling_For_Window (Block.all'Access, Workspace, Motor_Map, 3, Primitive_Window, Max_Vel);
            Spanning_Limit : constant Velocity :=
              Motor_Delta_Ceiling_For_Window
                (Block.all'Access, Workspace,
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
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
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
         Block.Params.Bounds := Rectangular_Bounds ([others => -100.0 * mm], [others => 100.0 * mm]);
         Block.Params.Bounds.Lower_X := Lower_X;
         Block.Params.Bounds.Upper_Y := Upper_Y;
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
         Normalize_And_Blend (Block.all, Motor_Map, Workspace);
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

      Params.Bounds := Rectangular_Bounds ([others => -10.0 * mm], [others => 10.0 * mm]);
      Params.Bounds.Upper_X := 1.5 * mm;
      Params.Bounds.Lower_Y := -0.5 * mm;
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
        ((Kind => Flush_Kind, Flush_Resetting_Data => Flush_Resetting_Data_Type_Default));
      Tested_Preprocessor.Run (Block, Initial_Position, Reset_Called);

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
      Motor_Map : Motor_Position_Map := [others => [others => 0.0 / mm]];
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
      Motor_Map (X_Axis, Motor_Name'First) := 1.0 / mm;
      Block.Corners (1) := [X_Axis => -20.0 * mm, others => 0.0 * mm];
      Block.Corners (2) := [others => 0.0 * mm];
      Block.Corners (3) := [Y_Axis => 20.0 * mm, others => 0.0 * mm];
      for I in Block.Primitives'Range loop
         Block.Primitives (I) := Make_Line_Primitive;
         Block.Corner_Dwell_Times (I) := 0.0 * s;
      end loop;

      Normalize_And_Blend (Block.all, Motor_Map, Workspace);

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
      Normalize_And_Blend (Block.all, Motor_Map, Workspace);
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
      Normalize_And_Blend (Block.all, Motor_Map, Workspace);
      T.Assert
        (Arc_Length (Block.Corner_Transitions (2)) > 0.0 * mm,
         "A later blend should not reuse stale hard-anchor workspace state");
   end Test_Per_Axis_Deviation_Corridor;

   procedure Test_Homing_Unavoidable_Tail_Includes_Complete_Tail (T : in out Trendy_Test.Operation'Class) is
      Block : aliased Execution_Block (2);
   begin
      T.Register;

      Reset_Early_Limiter_Block (Block);
      Block.Is_Homing_Move := True;
      Block.Feedrate_Profiles (2) :=
        (Accel => [others => 0.0 * s], Coast => 100.0 * ms, Decel => [others => 0.0 * s]);
      Block.Corner_Dwell_Times (2) := 10.0 * ms;

      T.Assert
        (Homing_Unavoidable_Tail_Time (Block'Access) = 15.0 * ms,
         "The unavoidable homing tail includes the required coast and dwell but excludes earlier excess coast");
   end Test_Homing_Unavoidable_Tail_Includes_Complete_Tail;

   procedure Test_Homing_Boundary_Uses_Resolved_Position (T : in out Trendy_Test.Operation'Class) is
      Params             : Kinematic_Parameters := (others => <>);
      Homing_Block       : aliased Execution_Block;
      Following_Block    : aliased Execution_Block;
      Reset_Called       : Boolean;
      Observed_Offset    : Position_Offset;
      Resolved_Position  : Position;
      Detector_Hit       : constant Position := [others => 0.0 * mm];
      Tail_Offset        : constant Position_Offset := [X_Axis => 1.0 * mm, others => 0.0 * mm];
      Stopped_Position   : constant Position := Detector_Hit + Tail_Offset;
      Approach_End       : constant Position := [X_Axis => 100.0 * mm, others => 0.0 * mm];
      Following_End      : constant Position := [Y_Axis => 1.0 * mm, others => 0.0 * mm];
      Following_Centre   : constant Position := [others => 0.0 * mm];
   begin
      T.Register;

      Params.Bounds :=
        (Kind    => Circular_Workspace,
         Lower_Z => -10.0 * mm,
         Upper_Z => 10.0 * mm,
         Lower_E => -10.0 * mm,
         Upper_E => 10.0 * mm,
         Radius  => 2.0 * mm);
      Boundary_Preprocessor.Setup (Params);
      Boundary_Preprocessor.Enqueue
        ((Kind        => Move_Kind,
          Dwell_After => 0.0 * s,
          Pos         => Approach_End,
          Feedrate    => 1.0 * mm / s),
         Ignore_Bounds => True);
      Boundary_Preprocessor.Enqueue
        ((Kind => Homing_Flush_Kind, Flush_Resetting_Data => Flush_Resetting_Data_Type_Default));
      Boundary_Preprocessor.Run (Homing_Block, Initial_Position, Reset_Called);

      T.Assert (not Reset_Called and then Homing_Block.Is_Homing_Move, "The boundary produces a homing block");
      Boundary_Preprocessor.Publish_Homing_Tail_Offset (Tail_Offset);
      Boundary_Preprocessor.Wait_For_Homing_Tail_Offset (Observed_Offset, Reset_Called);
      T.Assert
        (not Reset_Called and then Observed_Offset = Tail_Offset,
         "The homing module receives the planner's retained-tail displacement");

      Boundary_Preprocessor.Resolve_Homing_Position (Stopped_Position);
      Boundary_Preprocessor.Wait_For_Resolved_Homing_Position (Resolved_Position, Reset_Called);
      T.Assert
        (not Reset_Called and then Resolved_Position = Stopped_Position,
         "The planner receives the stopped position resolved by the homing module");

      Boundary_Preprocessor.Enqueue
        ((Kind        => Helix_Move_Kind,
          Dwell_After => 0.0 * s,
          Pos         => Following_End,
          Center      => Following_Centre,
          Clockwise   => False,
          Feedrate    => 1.0 * mm / s));
      Boundary_Preprocessor.Enqueue
        ((Kind => Flush_Kind, Flush_Resetting_Data => Flush_Resetting_Data_Type_Default));
      Boundary_Preprocessor.Run (Following_Block, Stopped_Position, Reset_Called);
      T.Assert
        (not Reset_Called
         and then Following_Block.Corners (1) = Stopped_Position
         and then Following_Block.Corners (2) = Following_End,
         "Following preprocessing and helix validation use the resolved stopped position");
   end Test_Homing_Boundary_Uses_Resolved_Position;

   procedure Test_Extrusion_Density_Rounding (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (4);
   begin
      T.Register;
      for Direction in -1 .. 1 when Direction /= 0 loop
         Reset_Early_Limiter_Block (Block);
         Block.Params.Extrusion_Rounding_Tolerance := 0.000_1 * mm;
         Block.Corners :=
           [[others => 0.0 * mm],
            [X_Axis => 1.0 * mm, E_Axis => 0.050_03 * mm, others => 0.0 * mm],
            [X_Axis => 3.0 * mm, E_Axis => 0.149_98 * mm, others => 0.0 * mm],
            [X_Axis => 4.0 * mm, E_Axis => 0.2 * mm, others => 0.0 * mm]];
         for P of Block.Corners loop
            P (E_Axis) := P (E_Axis) * Dimensionless (Direction);
         end loop;
         declare
            Original : constant Block_Plain_Corners := Block.Corners;
         begin
            Tested_Density_Normalizer.Run (Block, Workspace);
            T.Assert (Block.Corners = Original, "Normalization leaves the original command coordinates intact");
            for I in Block.Primitives'Range loop
               T.Assert (Block.Extrusion_Densities (I) = Dimensionless (Direction) * 0.05,
                 "Every segment in the run stores exactly the same total-preserving density");
               T.Assert (abs (Block.Extrusion_Densities (I) * Primitive_Length (Block'Access, I)
                 - (Original (I) (E_Axis) - Original (I - 1) (E_Axis))) <= Block.Params.Extrusion_Rounding_Tolerance,
                 "The actual extrusion adjustment of every segment obeys the local allowance");
            end loop;
            T.Assert (Workspace.Unblended_Extrusion_Positions (4) = Original (4) (E_Axis),
              "The entire run preserves its exact commanded endpoint");
            T.Assert (abs (Workspace.Unblended_Extrusion_Positions (2)
              - Dimensionless (Direction) * 0.05 * mm) < 1.0E-15 * mm,
              "The interior E reference uses the normalized density");
            Block.Params.Extrusion_Rounding_Tolerance := 0.000_01 * mm;
            Tested_Density_Normalizer.Run (Block, Workspace);
            for I in Block.Primitives'Range loop
               T.Assert (Block.Extrusion_Densities (I)
                 = (Original (I) (E_Axis) - Original (I - 1) (E_Axis)) / Primitive_Length (Block'Access, I),
                 "A tighter allowance retains distinct densities when no pair can be normalized");
            end loop;
            Block.Params.Extrusion_Rounding_Tolerance := 0.0 * mm;
            Tested_Density_Normalizer.Run (Block, Workspace);
            T.Assert (Workspace.Unblended_Extrusion_Positions (1 .. 4)
              = Block_Segment_Lengths'[for P of Original => P (E_Axis)],
              "Zero rounding allowance restores the literal unblended reference");
         end;
      end loop;
      declare
         Empty : aliased Execution_Block (1);
      begin
         Reset_Early_Limiter_Block (Empty);
         Tested_Density_Normalizer.Run (Empty, Workspace);
         T.Assert (Workspace.Unblended_Extrusion_Positions (1) = 0.0 * mm,
           "A block with no segments has no density run");
      end;
   end Test_Extrusion_Density_Rounding;

   procedure Test_Extrusion_Density_Rounding_Boundaries (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (4);
   begin
      T.Register;
      for Scenario in 1 .. 6 loop
         Reset_Early_Limiter_Block (Block);
         Block.Params.Extrusion_Rounding_Tolerance := 0.000_1 * mm;
         Block.Corners :=
           [[others => 0.0 * mm],
            [X_Axis => 1.0 * mm, E_Axis => 0.050_03 * mm, others => 0.0 * mm],
            [X_Axis => 3.0 * mm, E_Axis => 0.149_98 * mm, others => 0.0 * mm],
            [X_Axis => 4.0 * mm, E_Axis => 0.2 * mm, others => 0.0 * mm]];
         case Scenario is
            when 1 => Block.Corners (3) (E_Axis) := Block.Corners (2) (E_Axis);
            when 2 => Block.Corners (3) (E_Axis) := -0.05 * mm;
            when 3 => Block.Corners (3) (X_Axis) := Block.Corners (2) (X_Axis);
            when 4 => Block.Corner_Dwell_Times (2) := 0.001 * s;
            when 5 => Block.Corners (3) (E_Axis) := 0.07 * mm;
            when 6 => Block.Corners (3) (X_Axis) := 1.001 * mm;
            when others => raise Program_Error;
         end case;
         Tested_Density_Normalizer.Run (Block, Workspace);
         T.Assert (Block.Extrusion_Densities (2) = 0.050_03,
           "Travel, reversal, E-only, dwell, compensation and E-dominated boundaries end the preceding run");
         if Scenario /= 4 then
            for I in Block.Primitives'Range loop
               T.Assert (Block.Extrusion_Densities (I)
                 = (Block.Corners (I) (E_Axis) - Block.Corners (I - 1) (E_Axis)) / Primitive_Length (Block'Access, I),
                 "A distinct segment retains its original extrusion density");
            end loop;
         end if;
      end loop;
   end Test_Extrusion_Density_Rounding_Boundaries;

   procedure Test_Extrusion_Density_Rounding_Bounds (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Extrusion_Rounding_Tolerance := 0.000_1 * mm;
      Block.Params.Bounds.Lower_E := 0.0 * mm;
      Block.Params.Bounds.Upper_E := 0.1 * mm;
      Block.Corners :=
        [[others => 0.0 * mm],
         [X_Axis => 1.0 * mm, E_Axis => 0.050_03 * mm, others => 0.0 * mm],
         [X_Axis | Y_Axis => 1.0 * mm, E_Axis => 0.1 * mm, others => 0.0 * mm],
         [X_Axis | Y_Axis => 1.0 * mm, others => 0.0 * mm]];
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      T.Assert ((for all C of Block.Corner_Transitions => Policy (C) = Hard_Stop),
        "A shortened print followed by retraction to the lower bound recovers to the unshortened path");
      T.Assert (Block.Extrusion_Densities (2) = 0.05 and then Block.Extrusion_Densities (3) = 0.05,
        "Position-bound recovery retains the exact normalized densities");
      T.Assert (Block.Extrusion_Reference_Positions (2) = 0.05 * mm
        and then Block.Extrusion_Reference_Positions (3) = 0.1 * mm
        and then Block.Extrusion_Reference_Positions (4) = 0.0 * mm,
        "The recovery reference remains normalized and exactly respects the E bounds and retraction total");
   end Test_Extrusion_Density_Rounding_Bounds;

   procedure Test_Extrusion_Density_Rounding_Intersection (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (4);
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Extrusion_Rounding_Tolerance := 0.001 * mm;
      Block.Corners :=
        [[others => 0.0 * mm],
         [X_Axis => 1.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm],
         [X_Axis => 2.0 * mm, E_Axis => 2.0015 * mm, others => 0.0 * mm],
         [X_Axis => 3.0 * mm, E_Axis => 3.0045 * mm, others => 0.0 * mm]];
      Tested_Density_Normalizer.Run (Block, Workspace);
      T.Assert (Block.Extrusion_Densities (2) = Block.Extrusion_Densities (3)
        and then Block.Extrusion_Densities (3) /= Block.Extrusion_Densities (4),
        "A chain of pairwise-small changes cannot exceed an earlier segment's allowance");
      for I in Block.Primitives'Range loop
         T.Assert (abs (Block.Extrusion_Densities (I) * Primitive_Length (Block'Access, I)
           - (Block.Corners (I) (E_Axis) - Block.Corners (I - 1) (E_Axis))) <= 0.001 * mm,
           "The intersection enforces the allowance for every segment of the completed run");
      end loop;
      T.Assert (Workspace.Unblended_Extrusion_Positions (3) = Block.Corners (3) (E_Axis),
        "Closing a run preserves its extrusion total before starting the next run");
   end Test_Extrusion_Density_Rounding_Intersection;

   procedure Test_Extrusion_Density_Rounding_Execution (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Ignore_E_In_XYZE := False;
      Block.Params.Extrusion_Rounding_Tolerance := 0.000_1 * mm;
      Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis) := 0.0 * mm;
      Block.Params.Tangential_Velocity_Max := 100.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
      Block.Params.Axial_Acceleration_Maxes := [others => 1000.0 * mm / s ** 2];
      Block.Params.Axial_Jerk_Maxes := [others => 10000.0 * mm / s ** 3];
      Block.Params.Axial_Snap_Maxes := [others => 100000.0 * mm / s ** 4];
      Block.Params.Axial_Crackle_Maxes := [others => 1000000.0 * mm / s ** 5];
      Block.Original_Segment_Feedrates := [others => 30.0 * mm / s];
      Block.Corners :=
        [[others => 0.0 * mm],
         [X_Axis => 1.0 * mm, E_Axis => 0.050_03 * mm, others => 0.0 * mm],
         [X_Axis => 1.0 * mm, Y_Axis => 2.0 * mm, E_Axis => 0.149_98 * mm, others => 0.0 * mm],
         [X_Axis | Y_Axis => 2.0 * mm, E_Axis => 0.2 * mm, others => 0.0 * mm]];
      Tested_Density_Normalizer.Run (Block, Workspace);
      declare
         Densities : constant Block_Extrusion_Densities := Block.Extrusion_Densities;
         Reference : constant Block_Segment_Lengths := Workspace.Unblended_Extrusion_Positions (1 .. 4);
      begin
         Tested_Corner_Blender.Run (Block, Motor_Map, Workspace);
         for I in Corners_Index range 2 .. 3 loop
            T.Assert (Policy (Block.Corner_Transitions (I)) /= Hard_Stop,
              "Exactly shared normalized densities permit blending with zero E smoothing allowance");
         end loop;
         Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
         for I in Block.Primitives'Range loop
            T.Assert (Block.Original_Segment_Feedrates (I) = Block.Original_Segment_Feedrates (2),
              "XYZE feedrate conversion uses the shared density rather than each original E ratio");
         end loop;
         Plan_Kinematics (Block, Motor_Map, Workspace);
         T.Assert
           (Segment_Pos_At_Time (Block'Access, Block.N_Corners, Segment_Time (Block'Access, Block.N_Corners))
            = [Block.Corners (Block.N_Corners) with delta
                 E_Axis => Block.Extrusion_Reference_Positions (Block.N_Corners)],
            "The stopped endpoint is exact and retains the path-length extrusion correction");
         T.Assert (Block.Extrusion_Densities = Densities,
           "Every planning stage retains the exact stored density values");
         for I in Block.Primitives'Range loop
            T.Assert (Extrusion_Segment_Valid
              (Block'Access, Workspace, I, Block.Feedrate_Profiles (I), Block.Profile_Windows (I),
               Block.Profile_Crackles (I), Block.Limited_Segment_Feedrates (I), Motor_Map, Block.Profile_Ends (I)),
              "The actual normalized extrusion is included in the continuous kinematic certificate");
         end loop;
         --  Exhaust the current timing to exercise the real geometry-recovery path. It must not normalize
         --  again or put the original E coordinates back into the normalized reference.
         Block.Limited_Segment_Feedrates := [others => 0.0 * mm / s];
         Plan_Kinematics (Block, Motor_Map, Workspace);
         T.Assert (Block.Extrusion_Densities = Densities,
           "Geometry recovery preserves all density bits rather than redividing commanded E and length");
         T.Assert (Block.Extrusion_Reference_Positions = Reference,
           "The stopped fallback retains normalized unshortened E positions");
         T.Assert (abs (Segment_Pos_At_Time (Block'Access, 2, Segment_Time (Block'Access, 2)) (E_Axis)
           - 0.05 * mm) < 1.0E-12 * mm, "Execution uses the normalized E amount, not the original 0.05003 mm");
         for I in Corners_Index range 2 .. 3 loop
            T.Assert (abs (Segment_Pos_At_Time (Block'Access, I, Segment_Time (Block'Access, I))
              - Segment_Pos_At_Time (Block'Access, I + 1, 0.0 * s)) < 1.0E-12 * mm,
              "Normalized extrusion remains position-continuous at segment joins");
         end loop;
      end;
   end Test_Extrusion_Density_Rounding_Execution;

   procedure Test_Extrusion_Zero_Allowance_Geometry (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Limits : constant Axial_Deviation_Limits := [E_Axis => 0.0 * mm, others => 0.1 * mm];
   begin
      T.Register;
      for Family in Cornering_Kind loop
         for Mode in Extrusion_Cornering_Kind loop
            Reset_Early_Limiter_Block (Block);
            case Family is
               when Stereographic => Block.Params.Cornering :=
                 (Kind => Stereographic, Stereographic_Params => (Axial_Deviation_Maxes => Limits, others => <>));
               when Circular => Block.Params.Cornering :=
                 (Kind => Circular, Circular_Params => (Axial_Deviation_Maxes => Limits, others => <>));
               when Parabolic => Block.Params.Cornering :=
                 (Kind => Parabolic, Parabolic_Params => (Axial_Deviation_Maxes => Limits, others => <>));
               when Biarc => Block.Params.Cornering :=
                 (Kind => Biarc, Biarc_Params => (Axial_Deviation_Maxes => Limits, others => <>));
               when Sharp_SCV => Block.Params.Cornering := (Kind => Sharp_SCV, others => <>);
            end case;
            Block.Params.Extrusion_Cornering :=
              (if Mode = Smooth_Deviation then (Kind => Smooth_Deviation)
               else (Kind => Instantaneous_Velocity_Change, Velocity_Change_Max => 0.0 * mm / s));
            Block.Corners :=
              [[others => 0.0 * mm],
               [X_Axis => 10.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm],
               [X_Axis | Y_Axis => 10.0 * mm, E_Axis => 3.0 * mm, others => 0.0 * mm],
               [X_Axis => 20.0 * mm, Y_Axis => 10.0 * mm, E_Axis => 5.0 * mm, others => 0.0 * mm]];
            Normalize_And_Blend (Block, Motor_Map, Workspace);
            T.Assert (Policy (Block.Corner_Transitions (2)) = Hard_Stop,
              "A density change with zero E allowance is an unblended hard stop for every corner family");
            T.Assert (Segment_End_Transition_Distance (Block'Access, 2) = 0.0 * mm
              and then Segment_Start_Transition_Distance (Block'Access, 3) = 0.0 * mm,
              "Neither segment retains a curve around the mandatory E stop");
            T.Assert (Policy (Block.Corner_Transitions (3)) /= Hard_Stop,
              "An adjacent corner with unchanged density retains its normal corner policy");
            T.Assert (Block.Extrusion_Reference_Positions (2) = 1.0 * mm,
              "The untrimmed segment preserves its full commanded extrusion");
            if Family /= Sharp_SCV then
               T.Assert (Arc_Length (Block.Corner_Transitions (3)) > 0.0 * mm,
                 "An unchanged-density corner still blends with zero E allowance");
               Block.Params.Extrusion_Cornering :=
                 (Kind => Instantaneous_Velocity_Change, Velocity_Change_Max => 1.0 * mm / s);
               Normalize_And_Blend (Block, Motor_Map, Workspace);
               T.Assert (Arc_Length (Block.Corner_Transitions (2)) > 0.0 * mm,
                 "A positive velocity-jump allowance permits blending despite a zero E deviation entry");
            end if;
         end loop;
      end loop;
   end Test_Extrusion_Zero_Allowance_Geometry;

   procedure Test_Extrusion_Instantaneous_Velocity_Change (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (4);
      Motor_Map : Motor_Position_Map := [others => [others => 0.0 / mm]];
      Maximum : constant Velocity := 0.5 * mm / s;
   begin
      T.Register;
      for Scenario in 1 .. 7 loop
         Reset_Early_Limiter_Block (Block);
         Block.Params.Extrusion_Cornering :=
           (Kind => Instantaneous_Velocity_Change, Velocity_Change_Max => Maximum);
         Block.Params.Tangential_Velocity_Max := 100.0 * mm / s;
         Block.Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
         Block.Params.Axial_Acceleration_Maxes := [others => 1000.0 * mm / s ** 2];
         Block.Params.Axial_Jerk_Maxes := [others => 10000.0 * mm / s ** 3];
         Block.Params.Axial_Snap_Maxes := [others => 100000.0 * mm / s ** 4];
         Block.Params.Axial_Crackle_Maxes := [others => 1000000.0 * mm / s ** 5];
         Block.Original_Segment_Feedrates := [others => 100.0 * mm / s];
         Block.Corners :=
           [[others => 0.0 * mm],
            [X_Axis => 10.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
            [X_Axis => 20.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
            [X_Axis => 30.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm]];
         case Scenario is
            when 2 =>
               Block.Corners (3) (E_Axis) := 1.0 * mm;
               Block.Corners (4) (E_Axis) := 3.0 * mm;
            when 3 =>
               Block.Corners (3) := [X_Axis => 10.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm];
            when 4 =>
               Block.Corners (3) := [X_Axis => 10.01 * mm, E_Axis => 7.0 * mm, others => 0.0 * mm];
            when 5 =>
               Block.Params.Axial_Velocity_Maxes (E_Axis) := 0.1 * mm / s;
            when 6 =>
               Motor_Map (E_Axis, Motor_Name'First) := 10000.0 / mm;
            when 7 =>
               Block.Params.Axial_Acceleration_Maxes (E_Axis) := 0.1 * mm / s ** 2;
               Block.Params.Axial_Jerk_Maxes (E_Axis) := 1.0 * mm / s ** 3;
               Block.Params.Axial_Snap_Maxes (E_Axis) := 10.0 * mm / s ** 4;
               Block.Params.Axial_Crackle_Maxes (E_Axis) := 100.0 * mm / s ** 5;
            when others => null;
         end case;
         Normalize_And_Blend (Block, Motor_Map, Workspace);
         Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
         Plan_Kinematics (Block, Motor_Map, Workspace);
         for Junction in Corners_Index range 2 .. 3 loop
            declare
               Speed : constant Velocity := Block.Corner_Velocity_Limits (Junction);
               Jump : constant Velocity :=
                 abs (Block.Extrusion_Densities (Junction + 1) - Block.Extrusion_Densities (Junction)) * Speed;
               At_End : constant Time := Segment_Time (Block'Access, Junction);
               Step : constant Time := Interpolation_Time / 2.0;
               Before : constant Position := Segment_Pos_At_Time (Block'Access, Junction, At_End - Step);
               After : constant Position := Segment_Pos_At_Time (Block'Access, Junction + 1, Step);
            begin
               T.Assert
                 (Jump <= Maximum, "The actual E jump obeys its limit for stops, reversals and density changes");
               T.Assert (Total_Time (Block.Extrusion_Junction_Corrections (Junction).Times) = 0.0 * s,
                 "Instantaneous mode stores no E smoothing ramp");
               T.Assert (abs (Segment_Pos_At_Time (Block'Access, Junction, At_End)
                 - Segment_Pos_At_Time (Block'Access, Junction + 1, 0.0 * s)) < 1.0E-9 * mm,
                 "E velocity may jump but executed XYZ/E positions remain continuous");
               T.Assert (abs ((After (E_Axis) - Before (E_Axis)) * Motor_Map (E_Axis, Motor_Name'First))
                 <= Maximum_Deltas_Per_Command (Motor_Name'First), "Motor limits hold across an E velocity jump");
               if Scenario in 1 .. 2 then
                  T.Assert (Jump > 0.499 * mm / s, "The instantaneous E limit is reached rather than forcing a stop");
               end if;
            end;
         end loop;
         for I in Block.Primitives'Range loop
            T.Assert (Extrusion_Segment_Valid
              (Block'Access, Workspace, I, Block.Feedrate_Profiles (I), Block.Profile_Windows (I),
               Block.Profile_Crackles (I), Block.Limited_Segment_Feedrates (I), Motor_Map, Block.Profile_Ends (I)),
              "Continuous E derivative and motor bounds still apply on each side of a jump");
            if Scenario /= 3 then
               for Fraction of Sample_Fraction_Array'[0.0, 0.1, 0.5, 0.9, 1.0] loop
                  declare
                     P : constant Position := Segment_Pos_At_Time
                       (Block'Access, I, Fraction * Segment_Time (Block'Access, I));
                  begin
                     T.Assert (abs (P (E_Axis) - Block.Extrusion_Reference_Positions (I - 1)
                       - Block.Extrusion_Densities (I) * (P (X_Axis) - Block.Corners (I - 1) (X_Axis)))
                       < 1.0E-9 * mm, "Executed E follows XYZ at the commanded density without smoothing error");
                  end;
               end loop;
            end if;
         end loop;
         if Scenario = 3 then
            T.Assert (Block.Corner_Velocity_Limits (2) = 0.0 * mm / s
              and then Block.Corner_Velocity_Limits (3) = 0.0 * mm / s,
              "Mixed spatial/E-only junctions retain the required XYZ stops");
            T.Assert (Block.Extrusion_Densities (3) = -1.0, "E-only retraction retains its signed scalar density");
            T.Assert (abs (Segment_Pos_At_Time (Block'Access, 3, Segment_Time (Block'Access, 3)) (E_Axis)
              - Segment_Pos_At_Time (Block'Access, 3, 0.0 * s) (E_Axis) + 1.0 * mm) < 1.0E-10 * mm,
              "The E-only retraction executes its full displacement");
         elsif Scenario in 1 .. 2 then
            Block.Params.Extrusion_Cornering.Velocity_Change_Max := Maximum / 2.0;
            T.Assert (not Extrusion_Segment_Valid
              (Block'Access, Workspace, 2, Block.Feedrate_Profiles (2), Block.Profile_Windows (2),
               Block.Profile_Crackles (2), Block.Limited_Segment_Feedrates (2), Motor_Map, Block.Profile_Ends (2)),
              "Certification rejects a junction whose velocity jump exceeds the configured allowance");
         end if;
         Motor_Map := [others => [others => 0.0 / mm]];
      end loop;
   end Test_Extrusion_Instantaneous_Velocity_Change;

   procedure Test_Extrusion_Geometry_And_Compensation (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Baseline, Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      T.Register;
      for Family in Cornering_Kind loop
         for Incoming in Boolean loop
            Reset_Early_Limiter_Block (Baseline);
            case Family is
               when Stereographic => Baseline.Params.Cornering := (Kind => Stereographic, others => <>);
               when Circular => Baseline.Params.Cornering := (Kind => Circular, others => <>);
               when Parabolic => Baseline.Params.Cornering := (Kind => Parabolic, others => <>);
               when Biarc => Baseline.Params.Cornering := (Kind => Biarc, others => <>);
               when Sharp_SCV => Baseline.Params.Cornering := (Kind => Sharp_SCV, others => <>);
            end case;
            Baseline.Corners :=
              (if Incoming then
                 [1 => [others => 0.0 * mm],
                  2 => [X_Axis => 9.8 * mm, others => 0.0 * mm],
                  3 => [X_Axis => 10.0 * mm, others => 0.0 * mm],
                  4 => [X_Axis | Y_Axis => 10.0 * mm, others => 0.0 * mm]]
               else
                 [1 => [others => 0.0 * mm],
                  2 => [X_Axis => 10.0 * mm, others => 0.0 * mm],
                  3 => [X_Axis => 10.0 * mm, Y_Axis => 0.2 * mm, others => 0.0 * mm],
                  4 => [X_Axis | Y_Axis => 10.0 * mm, others => 0.0 * mm]]);
            Block := Baseline;
            for I in Block.Primitives'Range loop
               Block.Corners (I) (E_Axis) := Block.Corners (I - 1) (E_Axis)
                 + (if I = 3 then 0.02 else 0.1) * Primitive_Length (Block'Access, I);
            end loop;
            Normalize_And_Blend (Baseline, Motor_Map, Workspace);
            Normalize_And_Blend (Block, Motor_Map, Workspace);
            declare
               Expected : Length := 0.0 * mm;
               Full : Length := 0.0 * mm;
               Actual : Length := 0.0 * mm;
            begin
               for I in Block.Primitives'Range loop
                  declare
                     D : constant Length := Segment_Total_Distance (Block'Access, I);
                     Density : constant Dimensionless := (if I = 3 then 0.02 else 0.1);
                  begin
                     Expected := Expected + Density * D;
                     Full := Full + Primitive_Length (Block'Access, I);
                     Actual := Actual + D;
                     T.Assert (D = Segment_Total_Distance (Baseline'Access, I), "E cannot change XYZ trim allocation");
                     for K in 0 .. 100 loop
                        declare
                           S : constant Length := D * (Dimensionless (K) / 100.0);
                           P : constant Position := Point_At_Segment_Distance (Block'Access, I, S);
                           Q : constant Position := Point_At_Segment_Distance (Baseline'Access, I, S);
                           Ref : constant Length := Block.Extrusion_Reference_Positions (I - 1) + Density * S;
                        begin
                           T.Assert ([P with delta E_Axis => 0.0 * mm] = [Q with delta E_Axis => 0.0 * mm],
                             "All spatial samples agree regardless of extrusion density");
                           T.Assert (abs (P (E_Axis) - Ref) <= 0.1 * mm + 1.0E-12 * mm,
                             "Smoothing stays within the E deviation from the shortened-path reference");
                        end;
                     end loop;
                  end;
               end loop;
               T.Assert
                 (abs (Extrusion_Reference_At_Distance (Block'Access, 4, Segment_Total_Distance (Block'Access, 4))
                       - Expected)
                    < 1.0E-12 * mm, "Smoothing preserves integrated extrusion including the compensation section");
               if Family /= Sharp_SCV then
                  T.Assert (Actual < Full, "The compensated corner still blends and shortens XYZ");
                  T.Assert (Expected < Block.Corners (4) (E_Axis), "Shortening the path reduces extrusion");
               end if;
               T.Assert (abs (Block.Extrusion_Densities (3) - 0.02) < 1.0E-12,
                 "The short reduced-density section remains in the extrusion plan");
            end;
         end loop;
      end loop;
   end Test_Extrusion_Geometry_And_Compensation;

   procedure Test_Extrusion_Stops_And_Dominated_Moves (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (4);
      Motor_Map : Motor_Position_Map := [others => [others => 0.0 / mm]];
      type Numbers is array (Positive range <>) of Long_Float;

      procedure Check_Segment (I : Finishing_Corners_Index);
      procedure Check_Segment (I : Finishing_Corners_Index) is
         Window : constant Profile_Window := Selected_Profile_Window (Block'Access, I);
         Profile : constant Feedrate_Profile := Block.Feedrate_Profiles (I);
         C : constant Crackle := Block.Profile_Crackles (I);
         Start_Vel : constant Velocity := Block.Corner_Velocity_Limits (I - 1);
         Prefix : constant Time := Constant_Speed_Time (Window.Start_Distance, Start_Vel);
         Segment_Duration : constant Time := Segment_Time (Block'Access, I);
         Limits : constant Numbers := [5.0, 50.0, 500.0, 5_000.0, 50_000.0];
      begin
         T.Assert
           (Segment_Duration > 0.0 * s and then Segment_Duration < 1.0E6 * s,
            "Every mixed/E-only segment has a finite duration");
         for K in 0 .. 500 loop
            declare
               --  Crackle is discontinuous at a join. Evaluate all components on the same interior side,
               --  rather than mixing the scalar generator's left value with the E generator's right value.
               Epsilon : constant Time := Time'Min (1.0E-9 * s, Segment_Duration / 10000.0);
               At_Time : constant Time :=
                 (if K = 0 then Epsilon elsif K = 500 then Segment_Duration - Epsilon
                  else Segment_Duration * (Dimensionless (K) / 500.0));
               Local : constant Time := Time'Max (0.0 * s, Time'Min (Total_Time (Profile), At_Time - Prefix));
               D : Length;
               V : Long_Float;
               A, J, Snap_Raw, Cr : Long_Float := 0.0;
               Density : constant Long_Float := Long_Float (Block.Extrusion_Densities (I));
               E : Numbers (1 .. 5);
            begin
               if At_Time < Prefix then
                  D := At_Time * Start_Vel;
                  V := Long_Float (Start_Vel / (mm / s));
               elsif At_Time > Prefix + Total_Time (Profile) then
                  V := Long_Float (Block.Corner_Velocity_Limits (I) / (mm / s));
                  D := Window.Start_Distance + Window.Distance
                    + (At_Time - Prefix - Total_Time (Profile)) * Block.Corner_Velocity_Limits (I);
               else
                  D := Window.Start_Distance + Distance_At_Time (Profile, Local, C, Start_Vel);
                  V := Long_Float (Velocity_At_Time (Profile, Local, C, Start_Vel) / (mm / s));
                  A := Long_Float (Acceleration_At_Time (Profile, Local, C) / (mm / s ** 2));
                  J := Long_Float (Jerk_At_Time (Profile, Local, C) / (mm / s ** 3));
                  Snap_Raw := Long_Float (Snap_At_Time (Profile, Local, C) / (mm / s ** 4));
                  Cr := Long_Float (Crackle_At_Time (Profile, Local, C) / (mm / s ** 5));
               end if;
               E := [Density * V, Density * A, Density * J, Density * Snap_Raw, Density * Cr];
               for Junction in I - 1 .. I loop
                  if Junction > 1 and then Junction < Block.N_Corners then
                     declare
                        Ramp : constant Extrusion_Junction_Correction :=
                          Block.Extrusion_Junction_Corrections (Junction);
                        Half_Time : constant Time := Total_Time (Ramp.Times) / 2.0;
                        Offset : constant Time :=
                          (if Junction = I - 1 then At_Time else At_Time - Segment_Duration);
                        U : constant Time := Half_Time - abs Offset;
                        Sign : constant Long_Float := (if Offset <= 0.0 * s then 1.0 else -1.0);
                     begin
                        if Half_Time > 0.0 * s and then abs Offset <= Half_Time then
                           E (1) := E (1) + Sign * Long_Float
                             (Velocity_At_Time (Ramp.Times, U, Ramp.Max_Crackle, 0.0 * mm / s) / (mm / s));
                           E (2) := E (2) + Long_Float
                             (Acceleration_At_Time (Ramp.Times, U, Ramp.Max_Crackle) / (mm / s ** 2));
                           E (3) := E (3) + Sign * Long_Float
                             (Jerk_At_Time (Ramp.Times, U, Ramp.Max_Crackle) / (mm / s ** 3));
                           E (4) := E (4) + Long_Float
                             (Snap_At_Time (Ramp.Times, U, Ramp.Max_Crackle) / (mm / s ** 4));
                           E (5) := E (5) + Sign * Long_Float
                             (Crackle_At_Time (Ramp.Times, U, Ramp.Max_Crackle) / (mm / s ** 5));
                        end if;
                     end;
                  end if;
               end loop;
               T.Assert
                 (abs (Segment_Pos_At_Time (Block'Access, I, At_Time) (E_Axis)
                    - (Block.Extrusion_Reference_Positions (I - 1) + Block.Extrusion_Densities (I) * D))
                  <= Extrusion_Deviation (Block'Access) + 1.0E-10 * mm,
                  "Executed E stays within deviation from the shortened-path reference");
               for Order in E'Range loop
                  T.Assert
                    (abs E (Order) <= Limits (Order) * 1.00001,
                     "E time derivative" & Order'Image & " obeys its limit; value" & E (Order)'Image
                     & ", segment" & I'Image & ", sample" & K'Image);
               end loop;
               if At_Time + Interpolation_Time <= Segment_Duration then
                  declare
                     P0 : constant Position := Segment_Pos_At_Time (Block'Access, I, At_Time);
                     P1 : constant Position := Segment_Pos_At_Time (Block'Access, I, At_Time + Interpolation_Time);
                  begin
                     T.Assert
                       (abs ((P1 (E_Axis) - P0 (E_Axis)) * Motor_Map (E_Axis, Motor_Name'First)
                            + (P1 (X_Axis) - P0 (X_Axis)) * Motor_Map (X_Axis, Motor_Name'First))
                          <= Maximum_Deltas_Per_Command (Motor_Name'First),
                        "Extrusion obeys the motor command delta limit even when E dominates XYZ");
                  end;
               end if;
               if K < 500 then
                  declare
                     DT : constant Time := Time'Min (1.0E-5 * s, Segment_Duration / 10000.0);
                     P0 : constant Position := Segment_Pos_At_Time (Block'Access, I, At_Time);
                     P1 : constant Position := Segment_Pos_At_Time (Block'Access, I, At_Time + DT);
                  begin
                     T.Assert (abs (Long_Float ((P1 (E_Axis) - P0 (E_Axis)) / DT / (mm / s)) - E (1)) < 0.001,
                       "Executed E velocity agrees with the independently differentiated profile");
                  end;
               end if;
            end;
         end loop;
      end Check_Segment;
   begin
      T.Register;
      for Scenario in 1 .. 11 loop
         Reset_Early_Limiter_Block (Block);
         Block.Params.Tangential_Velocity_Max := 100.0 * mm / s;
         Block.Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
         Block.Params.Axial_Acceleration_Maxes := [others => 1000.0 * mm / s ** 2];
         Block.Params.Axial_Jerk_Maxes := [others => 10000.0 * mm / s ** 3];
         Block.Params.Axial_Snap_Maxes := [others => 100000.0 * mm / s ** 4];
         Block.Params.Axial_Crackle_Maxes := [others => 1000000.0 * mm / s ** 5];
         Block.Params.Axial_Velocity_Maxes (E_Axis) := 5.0 * mm / s;
         Block.Params.Axial_Acceleration_Maxes (E_Axis) := 50.0 * mm / s ** 2;
         Block.Params.Axial_Jerk_Maxes (E_Axis) := 500.0 * mm / s ** 3;
         Block.Params.Axial_Snap_Maxes (E_Axis) := 5000.0 * mm / s ** 4;
         Block.Params.Axial_Crackle_Maxes (E_Axis) := 50000.0 * mm / s ** 5;
         Block.Original_Segment_Feedrates := [others => 100.0 * mm / s];
         Block.Corners :=
           [1 => [others => 0.0 * mm],
            2 => [X_Axis => 10.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
            3 => [X_Axis => 20.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
            4 => [X_Axis => 30.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm]];
         if Scenario = 2 then
            Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis) := 0.0 * mm;
         elsif Scenario in 3 .. 4 then
            Block.Corners (3) := [X_Axis => 10.0 * mm, E_Axis => -1.0 * mm, others => 0.0 * mm];
            Block.Corners (4) := [X_Axis => 10.1 * mm, E_Axis => 9.0 * mm, others => 0.0 * mm];
            if Scenario = 4 then
               Block.Primitives (4) := Make_Helix_Primitive
                 (Block.Corners (3), Block.Corners (4),
                  [X_Axis => 10.05 * mm, others => 0.0 * mm], Clockwise => False);
            end if;
         elsif Scenario = 10 then
            Block.Corners :=
              [1 => [others => 0.0 * mm],
               2 => [X_Axis => 9.5 * mm, E_Axis => 0.95 * mm, others => 0.0 * mm],
               3 => [X_Axis => 10.0 * mm, E_Axis => 0.96 * mm, others => 0.0 * mm],
               4 => [X_Axis | Y_Axis => 10.0 * mm, E_Axis => 1.96 * mm, others => 0.0 * mm]];
         elsif Scenario = 11 then
            Block.Corners :=
              [1 => [others => 0.0 * mm],
               2 => [X_Axis => 10.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm],
               3 => [X_Axis => 10.0 * mm, Y_Axis => 0.5 * mm, E_Axis => 1.01 * mm, others => 0.0 * mm],
               4 => [X_Axis | Y_Axis => 10.0 * mm, E_Axis => 1.96 * mm, others => 0.0 * mm]];
         elsif Scenario >= 5 then
            Block.Corners (3) := [X_Axis | Y_Axis => 10.0 * mm, others => 0.0 * mm];
            Block.Corners (4) :=
              [X_Axis => 20.0 * mm, Y_Axis => 10.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm];
            case Scenario is
               when 5 => Block.Params.Cornering := (Kind => Stereographic, others => <>);
               when 6 => Block.Params.Cornering := (Kind => Circular, others => <>);
               when 7 => Block.Params.Cornering := (Kind => Parabolic, others => <>);
               when 8 => Block.Params.Cornering := (Kind => Biarc, others => <>);
               when others => Block.Params.Cornering := (Kind => Sharp_SCV, others => <>);
            end case;
         end if;
         if Scenario <= 2 then
            Block.Params.Bounds.Lower_E := 0.0 * mm;
         end if;
         Motor_Map (E_Axis, Motor_Name'First) := 1000.0 / mm;
         Motor_Map (X_Axis, Motor_Name'First) := (if Scenario in 1 | 3 then 100.0 else 0.0) / mm;
         Normalize_And_Blend (Block, Motor_Map, Workspace);
         Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
         Plan_Kinematics (Block, Motor_Map, Workspace);
         for I in Block.Primitives'Range loop
            Check_Segment (I);
            if I < Block.N_Corners then
               T.Assert (abs (Segment_Pos_At_Time (Block'Access, I, Segment_Time (Block'Access, I))
                 - Segment_Pos_At_Time (Block'Access, I + 1, 0.0 * s)) < 1.0E-9 * mm,
                 "XYZ and E stay continuous across every segment boundary");
            end if;
         end loop;
         if Scenario = 1 then
            T.Assert
              (Block.Corner_Velocity_Limits (2) > 0.0 * mm / s
               and then Block.Corner_Velocity_Limits (3) > 0.0 * mm / s,
              "XYZ keeps moving through extrusion stops and restarts");
            T.Assert
              (Extrusion_Reference_At_Distance (Block'Access, 3, 4.0 * mm)
               = Extrusion_Reference_At_Distance (Block'Access, 3, 6.0 * mm),
              "The travel section contains a complete E stop");
         elsif Scenario = 2 then
            T.Assert
              (Block.Corner_Velocity_Limits (2) = 0.0 * mm / s
               and then Block.Corner_Velocity_Limits (3) = 0.0 * mm / s,
              "Zero E deviation forces safe stops at density discontinuities");
         elsif Scenario in 3 .. 4 then
            T.Assert
              (abs (Block.Extrusion_Reference_Positions (3) - Block.Extrusion_Reference_Positions (2) + 3.0 * mm)
               < 1.0E-12 * mm,
              "E-only retraction retains its complete commanded displacement");
            T.Assert
              (abs (Block.Extrusion_Reference_Positions (4) - Block.Extrusion_Reference_Positions (3) - 10.0 * mm)
               < 1.0E-12 * mm,
              "An E-dominated move retains its extrusion after a retraction");
         end if;
      end loop;
   end Test_Extrusion_Stops_And_Dominated_Moves;

   procedure Test_Extrusion_Transition_Locality (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      T.Register;
      for Scenario in 1 .. 4 loop
         declare
            Block : aliased Execution_Block (4);
            Job_Time : Time := 0.0 * s;
         begin
            Reset_Early_Limiter_Block (Block);
            Block.Params.Tangential_Velocity_Max := 100.0 * mm / s;
            Block.Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
            Block.Params.Axial_Acceleration_Maxes := [others => 1000.0 * mm / s ** 2];
            Block.Params.Axial_Jerk_Maxes := [others => 10000.0 * mm / s ** 3];
            Block.Params.Axial_Snap_Maxes := [others => 100000.0 * mm / s ** 4];
            Block.Params.Axial_Crackle_Maxes := [others => 1000000.0 * mm / s ** 5];
            Block.Params.Axial_Velocity_Maxes (E_Axis) := 5.0 * mm / s;
            Block.Params.Axial_Acceleration_Maxes (E_Axis) := 50.0 * mm / s ** 2;
            Block.Params.Axial_Jerk_Maxes (E_Axis) := 500.0 * mm / s ** 3;
            Block.Params.Axial_Snap_Maxes (E_Axis) := 5000.0 * mm / s ** 4;
            Block.Params.Axial_Crackle_Maxes (E_Axis) := 50000.0 * mm / s ** 5;
            Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => 1.0 * mm];
            Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis) := 0.1 * mm;
            Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 1.0 * mm;
            Block.Original_Segment_Feedrates := [others => 30.0 * mm / s];
            if Scenario = 1 then
               Block.Corners :=
                 [1 => [others => 0.0 * mm],
                  2 => [X_Axis => 9.5 * mm, E_Axis => 0.95 * mm, others => 0.0 * mm],
                  3 => [X_Axis => 10.0 * mm, E_Axis => 0.96 * mm, others => 0.0 * mm],
                  4 => [X_Axis | Y_Axis => 10.0 * mm, E_Axis => 1.96 * mm, others => 0.0 * mm]];
            elsif Scenario = 2 then
               Block.Corners :=
                 [1 => [others => 0.0 * mm],
                  2 => [X_Axis => 10.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm],
                  3 => [X_Axis => 10.0 * mm, Y_Axis => 0.5 * mm, E_Axis => 1.01 * mm, others => 0.0 * mm],
                  4 => [X_Axis | Y_Axis => 10.0 * mm, E_Axis => 1.96 * mm, others => 0.0 * mm]];
            elsif Scenario = 3 then
               Block.Corners :=
                 [1 => [others => 0.0 * mm],
                  2 => [X_Axis => 10.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
                  3 => [X_Axis => 20.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
                  4 => [X_Axis => 30.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm]];
            else
               Block.Corners :=
                 [1 => [others => 0.0 * mm],
                  2 => [X_Axis => 0.0 * mm, E_Axis => -0.4 * mm, others => 0.0 * mm],
                  3 => [X_Axis => 0.2 * mm, E_Axis => 0.6 * mm, others => 0.0 * mm],
                  4 => [X_Axis => 2.2 * mm, E_Axis => 0.8 * mm, others => 0.0 * mm]];
            end if;
            Normalize_And_Blend (Block, Motor_Map, Workspace);
            Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
            Plan_Kinematics (Block, Motor_Map, Workspace);
            for I in Block.Primitives'Range loop
               Job_Time := Job_Time + Segment_Time (Block'Access, I);
            end loop;
            T.Assert (Job_Time < 10.0 * s,
              "Local E changes must not cause tens of seconds of crawling; scenario" & Scenario'Image
              & ", duration" & Job_Time'Image);
            if Scenario <= 2 then
               T.Assert
                 (Block.Corner_Velocity_Limits (2) > 0.0 * mm / s
                  and then Block.Corner_Velocity_Limits (3) > 0.0 * mm / s,
                  "Slicer compensation does not introduce XYZ stops");
            end if;
            if Scenario = 3 then
               declare
                  Window : constant Profile_Window := Selected_Profile_Window (Block'Access, 3);
                  Prefix : constant Time := Constant_Speed_Time
                    (Window.Start_Distance, Block.Corner_Velocity_Limits (2));
                  Half : constant Time := Total_Time (Block.Extrusion_Junction_Corrections (2).Times) / 2.0;
                  Overlap : Boolean := False;
               begin
                  for K in 1 .. 99 loop
                     declare
                        At_Time : constant Time := Half * Dimensionless (K) / 100.0;
                        A_X : constant Acceleration :=
                          (if At_Time > Prefix and then At_Time - Prefix < Total_Time (Block.Feedrate_Profiles (3))
                           then Acceleration_At_Time
                             (Block.Feedrate_Profiles (3), At_Time - Prefix, Block.Profile_Crackles (3))
                           else 0.0 * mm / s ** 2);
                        A_E : constant Acceleration := Acceleration_At_Time
                          (Block.Extrusion_Junction_Corrections (2).Times, Half - At_Time,
                           Block.Extrusion_Junction_Corrections (2).Max_Crackle);
                     begin
                        Overlap := Overlap or else
                          (abs A_X > 1.0E-4 * mm / s ** 2 and then abs A_E > 1.0E-4 * mm / s ** 2);
                     end;
                  end loop;
                  T.Assert (Overlap, "XYZ accelerates while E is still decelerating into its complete stop");
                  T.Assert (Block.Profile_Crackles (3) > 0.99 * Block.Params.Axial_Crackle_Maxes (X_Axis),
                    "Travel retains the XYZ crackle budget during the independent E transition");
               end;
               T.Assert
                 ((Segment_Pos_At_Time (Block'Access, 3, Segment_Time (Block'Access, 3) / 2.0 + 1.0E-5 * s)
                     (X_Axis)
                   - Segment_Pos_At_Time (Block'Access, 3, Segment_Time (Block'Access, 3) / 2.0) (X_Axis))
                    / (1.0E-5 * s) > 10.0 * mm / s,
                  "The travel section accelerates between the E transitions");
               declare
                  Hold_Start : constant Time := Total_Time (Block.Extrusion_Junction_Corrections (2).Times) / 2.0;
                  Hold_End : constant Time := Segment_Time (Block'Access, 3)
                    - Total_Time (Block.Extrusion_Junction_Corrections (3).Times) / 2.0;
               begin
                  T.Assert (Hold_End >= Hold_Start, "Independent E ramps do not overlap each other");
                  T.Assert (abs (Segment_Pos_At_Time (Block'Access, 3, (Hold_Start + Hold_End) / 2.0)
                    (E_Axis) - 2.0 * mm) < 1.0E-10 * mm, "E remains stopped during the travel hold");
               end;
            end if;
         end;
      end loop;
   end Test_Extrusion_Transition_Locality;

   procedure Test_Extrusion_Position_Bounds (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Tangential_Velocity_Max := 100.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
      Block.Params.Axial_Acceleration_Maxes := [others => 1000.0 * mm / s ** 2];
      Block.Params.Axial_Jerk_Maxes := [others => 10000.0 * mm / s ** 3];
      Block.Params.Axial_Snap_Maxes := [others => 100000.0 * mm / s ** 4];
      Block.Params.Axial_Crackle_Maxes := [others => 1000000.0 * mm / s ** 5];
      Block.Original_Segment_Feedrates := [others => 30.0 * mm / s];
      Block.Params.Bounds.Lower_E := 0.0 * mm;
      Block.Params.Bounds.Upper_E := 2.0 * mm;
      Block.Corners :=
        [1 => [others => 0.0 * mm],
         2 => [X_Axis => 10.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm],
         3 => [X_Axis | Y_Axis => 10.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
         4 => [X_Axis | Y_Axis => 10.0 * mm, others => 0.0 * mm]];
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      T.Assert ((for all Transition of Block.Corner_Transitions => Policy (Transition) = Hard_Stop),
        "Infeasible shortened extrusion falls back to the original path with stops");
      for I in Block.Corners'Range loop
         T.Assert (Block.Extrusion_Reference_Positions (I) = Block.Corners (I) (E_Axis),
           "Unblending preserves the original in-bounds extrusion coordinates");
      end loop;
      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
      Plan_Kinematics (Block, Motor_Map, Workspace);
      T.Assert (Block.Corners (3) (E_Axis) = 2.0 * mm and then Block.Params.Bounds.Upper_E = 2.0 * mm,
        "The geometry pass restores commanded coordinates and bounds even when extrusion is infeasible");
      Block.Params.Bounds.Lower_E := -1.0 * mm;
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      for I in Block.Primitives'Range loop
         for K in 0 .. 100 loop
            declare
               P : constant Position := Point_At_Segment_Distance
                 (Block'Access, I, Segment_Total_Distance (Block'Access, I) * (Dimensionless (K) / 100.0));
            begin
               T.Assert (P (E_Axis) in Block.Params.Bounds.Lower_E .. Block.Params.Bounds.Upper_E,
                 "The feasible corrected extrusion stays within E position bounds");
            end;
         end loop;
      end loop;
   end Test_Extrusion_Position_Bounds;

   procedure Test_Extrusion_Generated_Profile (T : in out Trendy_Test.Operation'Class) is
      Block : aliased Execution_Block (4);
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Axial_Velocity_Maxes (E_Axis) := 100.0 * mm / s;
      Workspace.Extrusion_Stops := [others => False];
      Block.Extrusion_Reference_Positions := [others => 0.0 * mm];
      for Scenario in 1 .. 4 loop
         Block.Params.Axial_Acceleration_Maxes (E_Axis) :=
           (if Scenario = 1 then 0.1 else 50.0) * mm / s ** 2;
         Block.Params.Axial_Jerk_Maxes (E_Axis) :=
           (if Scenario = 2 then 0.1 else 500.0) * mm / s ** 3;
         Block.Params.Axial_Snap_Maxes (E_Axis) :=
           (if Scenario = 3 then 0.1 else 5000.0) * mm / s ** 4;
         Block.Params.Axial_Crackle_Maxes (E_Axis) := 50000.0 * mm / s ** 5;
         for Direction in -1 .. 1 loop
            Block.Extrusion_Densities := [2 => 1.0, 3 => Dimensionless (Direction) * 0.5, 4 => 0.0];
            declare
               Speed : constant Velocity := 10.0 * mm / s;
               Ramp : constant Extrusion_Transition_Result := Extrusion_Transition (Block'Access, Workspace, 2, Speed);
               P : constant Feedrate_Profile_Times := Ramp.Profile.Times;
               C : constant Crackle := abs Ramp.Profile.Max_Crackle;
               Peaks : constant array (1 .. 4) of Dimensionless :=
                 [C * P (1) * (P (1) + P (2)) * (2.0 * P (1) + P (2) + P (3))
                    / Block.Params.Axial_Acceleration_Maxes (E_Axis),
                  C * P (1) * (P (1) + P (2)) / Block.Params.Axial_Jerk_Maxes (E_Axis),
                  C * P (1) / Block.Params.Axial_Snap_Maxes (E_Axis),
                  C / Block.Params.Axial_Crackle_Maxes (E_Axis)];
               Durations : constant array (1 .. 15) of Time :=
                 [P (1), P (2), P (1), P (3), P (1), P (2), P (1), P (4),
                  P (1), P (2), P (1), P (3), P (1), P (2), P (1)];
               Signs : constant array (1 .. 15) of Dimensionless :=
                 [1.0, 0.0, -1.0, 0.0, -1.0, 0.0, 1.0, 0.0, -1.0, 0.0, 1.0, 0.0, 1.0, 0.0, -1.0];
               X : Length := -Speed * Total_Time (P) / 2.0;
               V : Velocity := Speed;
               A : Acceleration := 0.0 * mm / s ** 2;
               J : Jerk := 0.0 * mm / s ** 3;
               S4 : Snap := 0.0 * mm / s ** 4;
               Elapsed : Time := 0.0 * s;
            begin
               T.Assert (Ramp.Valid, "Signed E velocity ramps are feasible");
               for Peak of Peaks loop
                  T.Assert (Peak <= 1.0, "Analytical E derivative extrema obey the configured limits");
               end loop;
               T.Assert (Peaks (Scenario) > 0.998, "The generator reaches the active E derivative limit");
               for Phase in Durations'Range loop
                  declare
                     DT : constant Time := Durations (Phase);
                     Cr : constant Crackle := Signs (Phase) * Ramp.Profile.Max_Crackle;
                  begin
                     --  Integrate each constant-crackle polynomial independently of the execution evaluator.
                     X := X + V * DT + A * DT ** 2 / 2.0 + J * DT ** 3 / 6.0
                       + S4 * DT ** 4 / 24.0 + Cr * DT ** 5 / 120.0;
                     V := V + A * DT + J * DT ** 2 / 2.0 + S4 * DT ** 3 / 6.0 + Cr * DT ** 4 / 24.0;
                     A := A + J * DT + S4 * DT ** 2 / 2.0 + Cr * DT ** 3 / 6.0;
                     J := J + S4 * DT + Cr * DT ** 2 / 2.0;
                     S4 := S4 + Cr * DT;
                     Elapsed := Elapsed + DT;
                     declare
                        Offset : constant Time := Elapsed - Total_Time (P) / 2.0;
                        Ref : constant Length :=
                          Block.Extrusion_Densities (if Offset <= 0.0 * s then 2 else 3) * Speed * Offset;
                     begin
                        T.Assert (abs (X - Ref) <= Ramp.Deviation + 1.0E-8 * mm,
                          "Exact phase endpoints lie inside the analytical deviation envelope");
                     end;
                  end;
               end loop;
               T.Assert (abs (X - Block.Extrusion_Densities (3) * Speed * Total_Time (P) / 2.0) < 1.0E-8 * mm,
                 "The signed ramp preserves the reference extrusion integral");
               T.Assert (abs (V - Block.Extrusion_Densities (3) * Speed) < 1.0E-8 * mm / s
                 and then abs A < 1.0E-8 * mm / s ** 2 and then abs J < 1.0E-8 * mm / s ** 3
                 and then abs S4 < 1.0E-8 * mm / s ** 4,
                 "E joins the outgoing velocity with zero acceleration, jerk and snap");
            end;
         end loop;
      end loop;
      Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis) := 0.001 * mm;
      Workspace.Extrusion_Widths := [others => 100.0 * mm];
      Plan_Extrusion_Junction (Block, Workspace, 2, 10.0 * mm / s);
      T.Assert (Workspace.Extrusion_Deviations (2) <= 0.001 * mm
        and then Workspace.Extrusion_Deviations (2) > 0.000999 * mm,
        "Joint speed solving reaches the analytical deviation limit");
   end Test_Extrusion_Generated_Profile;

   procedure Test_Extrusion_Workspace_Reuse (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Printing : aliased Execution_Block (4);
      Retraction : aliased Execution_Block (2);
      Durations : array (Finishing_Corners_Index range 2 .. 4) of Time;
      Positions : array (Finishing_Corners_Index range 2 .. 4, Natural range 0 .. 32) of Position;

      procedure Plan (Block : aliased in out Execution_Block);
      procedure Assert_Printing_Unchanged;

      procedure Plan (Block : aliased in out Execution_Block) is
      begin
         Normalize_And_Blend (Block, Motor_Map, Workspace);
         Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
         Plan_Kinematics (Block, Motor_Map, Workspace);
      end Plan;

      procedure Assert_Printing_Unchanged is
      begin
         for I in Durations'Range loop
            T.Assert (Segment_Time (Printing'Access, I) = Durations (I),
              "Queued block timing is independent of the reused planning workspace");
            for J in Positions'Range (2) loop
               T.Assert
                 (Segment_Pos_At_Time (Printing'Access, I, Durations (I) * Dimensionless (J) / 32.0)
                    = Positions (I, J),
                  "Queued XYZ and E trajectories are independent of the reused planning workspace");
            end loop;
         end loop;
      end Assert_Printing_Unchanged;
   begin
      T.Register;
      Reset_Early_Limiter_Block (Printing);
      Printing.Params.Ignore_E_In_XYZE := True;
      Printing.Params.Tangential_Velocity_Max := 100.0 * mm / s;
      Printing.Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
      Printing.Params.Axial_Acceleration_Maxes := [others => 1000.0 * mm / s ** 2];
      Printing.Params.Axial_Jerk_Maxes := [others => 10000.0 * mm / s ** 3];
      Printing.Params.Axial_Snap_Maxes := [others => 100000.0 * mm / s ** 4];
      Printing.Params.Axial_Crackle_Maxes := [others => 1000000.0 * mm / s ** 5];
      Printing.Params.Axial_Velocity_Maxes (E_Axis) := 5.0 * mm / s;
      Printing.Params.Axial_Acceleration_Maxes (E_Axis) := 50.0 * mm / s ** 2;
      Printing.Params.Axial_Jerk_Maxes (E_Axis) := 500.0 * mm / s ** 3;
      Printing.Params.Axial_Snap_Maxes (E_Axis) := 5000.0 * mm / s ** 4;
      Printing.Params.Axial_Crackle_Maxes (E_Axis) := 50000.0 * mm / s ** 5;
      Printing.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => 1.0 * mm];
      Printing.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis) := 0.1 * mm;
      Printing.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 1.0 * mm;
      Printing.Original_Segment_Feedrates := [others => 30.0 * mm / s];
      Printing.Corners :=
        [1 => [others => 0.0 * mm],
         2 => [X_Axis => 10.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
         3 => [X_Axis => 20.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
         4 => [X_Axis => 30.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm]];
      Plan (Printing);
      T.Assert (Total_Time (Printing.Extrusion_Junction_Corrections (2).Times) > 0.0 * s,
        "Workspace-reuse fixture has an independent E transition");
      for I in Durations'Range loop
         Durations (I) := Segment_Time (Printing'Access, I);
         for J in Positions'Range (2) loop
            Positions (I, J) :=
              Segment_Pos_At_Time (Printing'Access, I, Durations (I) * Dimensionless (J) / 32.0);
         end loop;
      end loop;

      --  Replan without rerunning geometry construction. The limiter must discard stale junction allocations
      --  while retaining the densities and corrected reference built by Corner_Blender.
      declare
         Reference : constant Block_Segment_Lengths := Printing.Extrusion_Reference_Positions;
         Densities : constant Block_Extrusion_Densities := Printing.Extrusion_Densities;
      begin
         Workspace.Extrusion_Widths := [others => Length'Last];
         Workspace.Extrusion_Stops := [others => True];
         Workspace.Extrusion_Ceilings := [others => 0.0 * mm / s];
         Workspace.Extrusion_Deviations := [others => Length'Last];
         Workspace.Static_Corner_Limits := [others => 0.0 * mm / s];
         Workspace.Window_Evaluations := [others => [others => <>]];
         Workspace.Planning_Attempts := [others => Natural'Last];
         Plan_Kinematics (Printing, Motor_Map, Workspace);
         T.Assert
           (Printing.Extrusion_Reference_Positions = Reference and then Printing.Extrusion_Densities = Densities,
            "Replanning preserves the extrusion reference and densities");
         Assert_Printing_Unchanged;
      end;

      Reset_Early_Limiter_Block (Retraction);
      Retraction.Params := Printing.Params;
      Retraction.Original_Segment_Feedrates := [others => 30.0 * mm / s];
      Retraction.Corners := [1 => [others => 0.0 * mm], 2 => [E_Axis => -2.0 * mm, others => 0.0 * mm]];
      Plan (Retraction);
      T.Assert
        (Segment_Pos_At_Time (Retraction'Access, 2, Segment_Time (Retraction'Access, 2)) = Retraction.Corners (2),
         "A smaller E-only block replaces the previous block's planning state");
      Assert_Printing_Unchanged;

      Workspace.Corner_Derivative_Bounds := [others => <>];
      Workspace.Original_Extrusion_Positions := [others => Length'Last];
      Workspace.Static_Corner_Limits := [others => 0.0 * mm / s];
      Workspace.Window_Evaluations := [others => [others => <>]];
      Workspace.Planning_Attempts := [others => Natural'Last];
      Workspace.Extrusion_Widths := [others => Length'Last];
      Workspace.Extrusion_Stops := [others => True];
      Workspace.Extrusion_Ceilings := [others => 0.0 * mm / s];
      Workspace.Extrusion_Deviations := [others => Length'Last];
      Assert_Printing_Unchanged;
      Plan (Printing);
      Assert_Printing_Unchanged;
   end Test_Extrusion_Workspace_Reuse;

   procedure Test_Profile_Local_Curve_Limits (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Curves : Block_Corner_Transitions (Block.Corners'Range);
      I : constant Finishing_Corners_Index := 3;

      procedure Check_Curve_Profile (Part : Profile_End; Window : Profile_Window; Start : Velocity);

      procedure Check_Curve_Profile (Part : Profile_End; Window : Profile_Window; Start : Velocity) is
         Bounds : constant Unit_Speed_Axial_Derivative_Bounds :=
           Window_Axial_Derivative_Bounds (Block'Access, Workspace, I, Window);
         V : constant Velocity := Fast_Velocity_At_Max_Time (Part.Profile.Accel, Part.Max_Crackle, Start);
         A : Acceleration := 0.0 * mm / s ** 2;
         J : Jerk := 0.0 * mm / s ** 3;
         Peak_Snap : Snap := 0.0 * mm / s ** 4;
         C : Crackle := 0.0 * mm / s ** 5;
         type Ramps is array (1 .. 2) of Feedrate_Profile_Times;
      begin
         --  Evaluate the generator at the known extrema, independently of the planner's peak formulas.
         for Ramp of Ramps'[Part.Profile.Accel, Part.Profile.Decel] loop
            A := Acceleration'Max (A, Acceleration_At_Time (Ramp, Total_Time (Ramp) / 2.0, Part.Max_Crackle));
            J := Jerk'Max (J, Jerk_At_Time (Ramp, 2.0 * Ramp (1) + Ramp (2), Part.Max_Crackle));
            Peak_Snap := Snap'Max (Peak_Snap, Snap_At_Time (Ramp, Ramp (1), Part.Max_Crackle));
            C := Crackle'Max (C, Crackle_At_Time (Ramp, Ramp (1) / 2.0, Part.Max_Crackle));
         end loop;
         for Axis in X_Axis .. Z_Axis loop
            --  The geometric interval bounds and scalar extrema enclose the entire curve profile.
            T.Assert (Bounds.Velocity (Axis) * V <= Block.Params.Axial_Velocity_Maxes (Axis),
              "Curve profile respects the axial velocity limit");
            T.Assert (Bounds.Acceleration (Axis) * V ** 2 + Bounds.Velocity (Axis) * A
              <= Block.Params.Axial_Acceleration_Maxes (Axis), "Curve profile respects the axial acceleration limit");
            T.Assert (Bounds.Jerk (Axis) * V ** 3 + 3.0 * Bounds.Acceleration (Axis) * V * A
              + Bounds.Velocity (Axis) * J <= Block.Params.Axial_Jerk_Maxes (Axis),
              "Curve profile respects the axial jerk limit");
            T.Assert (Bounds.Snap (Axis) * V ** 4 + 6.0 * Bounds.Jerk (Axis) * V ** 2 * A
              + 3.0 * Bounds.Acceleration (Axis) * A ** 2 + 4.0 * Bounds.Acceleration (Axis) * V * J
              + Bounds.Velocity (Axis) * Peak_Snap <= Block.Params.Axial_Snap_Maxes (Axis),
              "Curve profile respects the axial snap limit");
            T.Assert (Bounds.Crackle (Axis) * V ** 5 + 10.0 * Bounds.Snap (Axis) * V ** 3 * A
              + 15.0 * Bounds.Jerk (Axis) * V * A ** 2 + 10.0 * Bounds.Jerk (Axis) * V ** 2 * J
              + 10.0 * Bounds.Acceleration (Axis) * A * J + 5.0 * Bounds.Acceleration (Axis) * V * Peak_Snap
              + Bounds.Velocity (Axis) * C <= Block.Params.Axial_Crackle_Maxes (Axis),
              "Curve profile respects the axial crackle limit");
         end loop;
      end Check_Curve_Profile;
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Ignore_E_In_XYZE := True;
      Block.Params.Tangential_Velocity_Max := 250.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
      Block.Params.Axial_Acceleration_Maxes := [others => 5000.0 * mm / s ** 2];
      Block.Params.Axial_Jerk_Maxes := [others => 500000.0 * mm / s ** 3];
      Block.Params.Axial_Snap_Maxes := [others => 500000000.0 * mm / s ** 4];
      Block.Params.Axial_Crackle_Maxes := [others => 500000000000.0 * mm / s ** 5];
      Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => 0.02 * mm];
      Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 0.02 * mm;
      Block.Original_Segment_Feedrates := [others => 250.0 * mm / s];
      --  Benchy travel segment 11749 and its neighbours. A 0.0002 mm curved end previously capped the
      --  entire 34 mm segment at 0.057 mm/s, taking more than ten minutes with zero E deviation.
      Block.Corners :=
        [[117.5 * mm, 103.215 * mm, 9.0 * mm, 0.0 * mm],
         [118.367 * mm, 102.597 * mm, 9.0 * mm, 0.04497 * mm],
         [103.45 * mm, 133.676 * mm, 9.0 * mm, 0.04497 * mm],
         [103.545 * mm, 133.465 * mm, 9.0 * mm, 0.05473 * mm]];
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      Curves := Block.Corner_Transitions;
      --  Tighten E after constructing the curves to exercise timing of a retained curve with a mandatory stop.
      --  The normal pipeline now constructs a sharp corner when the allowance is zero from the outset.
      Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis) := 0.0 * mm;
      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
      Plan_Kinematics (Block, Motor_Map, Workspace);
      T.Assert (Block.Profile_Ends (I).Enabled, "A long primitive gets profiles separate from its tight curved ends");
      T.Assert (Block.Corner_Transitions = Curves, "Local profile limits preserve the blended XYZ geometry");
      T.Assert (Block.Corner_Velocity_Limits (I - 1) = 0.0 * mm / s
        and then Block.Corner_Velocity_Limits (I) = 0.0 * mm / s, "Zero E deviation still stops at density changes");
      T.Assert (Segment_Time (Block'Access, I) < 0.3 * s,
        "Local profiles and actual derivative budgets avoid throttling the travel move");
      T.Assert (Fast_Velocity_At_Max_Time
        (Block.Feedrate_Profiles (I).Accel, Block.Profile_Crackles (I),
         Central_Profile_Start_Velocity (Block'Access, I)) > 10.0 * mm / s,
        "The retained primitive accelerates above the curved end's speed ceiling");
      for J in Block.Primitives'Range loop
         T.Assert (Extrusion_Segment_Valid
           (Block'Access, Workspace, J, Block.Feedrate_Profiles (J), Block.Profile_Windows (J),
            Block.Profile_Crackles (J), 250.0 * mm / s, Motor_Map, Block.Profile_Ends (J)),
           "All phases of the selected local profiles satisfy analytical E limits");
      end loop;
      declare
         Ends : constant Profile_End_Pair := Block.Profile_Ends (I);
         Prefix_Time : constant Time := Segment_Prefix_Time (Block'Access, I);
         Central_End_Time : constant Time := Prefix_Time + Total_Time (Block.Feedrate_Profiles (I));
         Epsilon : constant Time := 1.0E-8 * s;
         type Join_Times is array (1 .. 2) of Time;
      begin
         Check_Curve_Profile
           (Ends.Prefix, (0.0 * mm, Block.Profile_Windows (I).Start_Distance), 0.0 * mm / s);
         Check_Curve_Profile
           (Ends.Suffix,
            (Block.Profile_Windows (I).Start_Distance + Block.Profile_Windows (I).Distance,
             Segment_End_Transition_Distance (Block'Access, I)), Ends.Suffix.Boundary_Velocity);
         for Join of Join_Times'[Prefix_Time, Central_End_Time] loop
            T.Assert (abs (Segment_Pos_At_Time (Block'Access, I, Join + Epsilon)
              - Segment_Pos_At_Time (Block'Access, I, Join - Epsilon)) < 1.0E-5 * mm,
              "Generated profile joins are continuous in executed position");
            T.Assert (abs (Segment_Vel_Ratio_At_Time (Block'Access, I, Join + Epsilon)
              - Segment_Vel_Ratio_At_Time (Block'Access, I, Join - Epsilon)) < 1.0E-8,
              "Generated profile joins are continuous in executed velocity");
         end loop;
         T.Assert (abs (Velocity_At_Time
           (Ends.Prefix.Profile, Prefix_Time, Ends.Prefix.Max_Crackle, 0.0 * mm / s)
           - Ends.Prefix.Boundary_Velocity) < 1.0E-8 * mm / s, "Prefix reaches the central profile's start velocity");
         T.Assert (abs (Velocity_At_Time
           (Block.Feedrate_Profiles (I), Total_Time (Block.Feedrate_Profiles (I)),
            Block.Profile_Crackles (I), Ends.Prefix.Boundary_Velocity) - Ends.Suffix.Boundary_Velocity)
           < 1.0E-8 * mm / s, "Central profile reaches the suffix's start velocity");
      end;
      --  A small positive allowance retains active E corrections. They must be certified against the
      --  generated curved-end profiles too, including when XYZ accelerates during an E correction.
      Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (E_Axis) := 1.0E-8 * mm;
      Plan_Kinematics (Block, Motor_Map, Workspace);
      T.Assert (Block.Profile_Ends (I).Enabled, "Local profiles also support nonzero E junction corrections");
      T.Assert (Total_Time (Block.Extrusion_Junction_Corrections (I - 1).Times) > 0.0 * s,
        "The local-profile fixture includes an active E correction");
      T.Assert (Extrusion_Segment_Valid
        (Block'Access, Workspace, I, Block.Feedrate_Profiles (I), Block.Profile_Windows (I),
         Block.Profile_Crackles (I), 250.0 * mm / s, Motor_Map, Block.Profile_Ends (I)),
        "Overlapping E corrections and local XYZ profiles satisfy the continuous limits");
   end Test_Profile_Local_Curve_Limits;

   procedure Test_Profile_Recovery (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace;
      Block : aliased Execution_Block (3);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Result : Profile_Planning_Result;
      Rejected : Boolean := False;
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Tangential_Velocity_Max := 100.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
      Block.Params.Axial_Acceleration_Maxes := [others => 1000.0 * mm / s ** 2];
      Block.Params.Axial_Jerk_Maxes := [others => 10000.0 * mm / s ** 3];
      Block.Params.Axial_Snap_Maxes := [others => 100000.0 * mm / s ** 4];
      Block.Params.Axial_Crackle_Maxes := [others => 1000000.0 * mm / s ** 5];
      Block.Original_Segment_Feedrates := [others => 30.0 * mm / s];
      Block.Params.Ignore_E_In_XYZE := False;
      Block.Corners :=
        [1 => [others => 0.0 * mm],
         2 => [X_Axis => 10.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
         3 => [X_Axis | Y_Axis => 10.0 * mm, E_Axis => 3.0 * mm, others => 0.0 * mm]];
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
      declare
         Normalized_Feedrates : constant Block_Segment_Feedrates := Block.Original_Segment_Feedrates;
      begin
         --  Model an exhausted conservative bound for the blended path. Profile selection must report the
         --  failed segment; recovery must rebuild a certifiable original path rather than accept stale profiles.
         Block.Limited_Segment_Feedrates := [others => 0.0 * mm / s];
         Tested_Kinematic_Limiter.Run (Block, Motor_Map, Workspace, Result);
         T.Assert (Result.Valid, "Zero junction velocities require no E transition");
         Tested_Profile_Generator.Run (Block, Motor_Map, Workspace, Result);
         T.Assert (not Result.Valid, "Profile exhaustion is reported for retry");
         Plan_Kinematics (Block, Motor_Map, Workspace);
         T.Assert (Block.Original_Segment_Feedrates = Normalized_Feedrates,
           "Geometry recovery does not normalize programmed feedrates twice");
         T.Assert ((for all Transition of Block.Corner_Transitions => Policy (Transition) = Hard_Stop),
           "Exhausted blended profiles recover to the original path with stops");
         T.Assert ((for all Speed of Block.Corner_Velocity_Limits => Speed = 0.0 * mm / s),
           "The conservative fallback stops at every boundary");
         for I in Block.Primitives'Range loop
            T.Assert (Segment_Time (Block'Access, I) > 0.0 * s, "Recovery generates a traversable profile");
            T.Assert (abs (Segment_Pos_At_Time (Block'Access, I, Segment_Time (Block'Access, I))
              - Block.Corners (I)) < 1.0E-10 * mm, "Recovery reaches the commanded endpoint");
            T.Assert (Extrusion_Segment_Valid
              (Block'Access, Workspace, I, Block.Feedrate_Profiles (I), Block.Profile_Windows (I),
               Block.Profile_Crackles (I), Block.Limited_Segment_Feedrates (I), Motor_Map),
              "The recovered trajectory passes continuous E certification");
         end loop;
      end;

      --  A stationary start cannot produce positive E motion with zero permitted E acceleration. Recovery
      --  must terminate with an error rather than silently waive the limit or return a partial block.
      Block.Params.Axial_Acceleration_Maxes (E_Axis) := 0.0 * mm / s ** 2;
      begin
         Plan_Kinematics (Block, Motor_Map, Workspace);
      exception
         when Constraint_Error => Rejected := True;
      end;
      T.Assert (Rejected, "Impossible limits remain an error after bounded recovery");
      T.Assert (Block.Params.Axial_Acceleration_Maxes (E_Axis) = 0.0 * mm / s ** 2,
        "Recovery never weakens the configured E limits");
   end Test_Profile_Recovery;

   procedure Test_Extrusion_Polynomial_Enclosure (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      T.Assert (not Extrusion_Polynomial_In_Range ([0.0, 4.0, -4.0], 0.0, 0.9),
        "An interior limit violation is rejected although both endpoints are zero");
      T.Assert (Extrusion_Polynomial_In_Range ([0.0, 4.0, -4.0], 0.0, 1.001),
        "Subdivision certifies a polynomial whose initial Bernstein hull exceeds the limit");
      T.Assert (not Extrusion_Polynomial_In_Range ([0.0, 0.0, 16.0, -32.0, 16.0], -0.001, 0.99),
        "The quartic velocity polynomial's interior peak is checked analytically");
      T.Assert (Extrusion_Polynomial_In_Range ([0.0, 0.0, 16.0, -32.0, 16.0], -0.001, 1.001),
        "Quartic velocity enclosure converges around a valid interior peak");
   end Test_Extrusion_Polynomial_Enclosure;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Extrusion_Density_Rounding'Unrestricted_Access,
         Test_Extrusion_Density_Rounding_Boundaries'Unrestricted_Access,
         Test_Extrusion_Density_Rounding_Bounds'Unrestricted_Access,
         Test_Extrusion_Density_Rounding_Intersection'Unrestricted_Access,
         Test_Extrusion_Density_Rounding_Execution'Unrestricted_Access,
         Test_Extrusion_Zero_Allowance_Geometry'Unrestricted_Access,
         Test_Extrusion_Instantaneous_Velocity_Change'Unrestricted_Access,
         Test_Profile_Local_Curve_Limits'Unrestricted_Access,
         Test_Profile_Recovery'Unrestricted_Access,
         Test_Extrusion_Workspace_Reuse'Unrestricted_Access,
         Test_Extrusion_Polynomial_Enclosure'Unrestricted_Access,
         Test_Extrusion_Generated_Profile'Unrestricted_Access,
         Test_Extrusion_Transition_Locality'Unrestricted_Access,
         Test_Extrusion_Position_Bounds'Unrestricted_Access,
         Test_Extrusion_Geometry_And_Compensation'Unrestricted_Access,
         Test_Extrusion_Stops_And_Dominated_Moves'Unrestricted_Access,
         Test_Early_Limiter_Helix_Ignore_E'Unrestricted_Access,
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
         Test_Per_Axis_Deviation_Corridor'Unrestricted_Access,
         Test_Homing_Boundary_Uses_Resolved_Position'Unrestricted_Access,
         Test_Homing_Unavoidable_Tail_Includes_Complete_Tail'Unrestricted_Access];
   end All_Tests;

end Prunt.Motion_Planner.Planner.Test;
