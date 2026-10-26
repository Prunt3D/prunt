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
   package Reset_Preprocessor is new Preprocessor;

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
      Block.Extrusion_Reference_Positions := [others => 0.0 * mm];
      Block.Extrusion_Densities := [others => 0.0];
      Block.Kind := Motion_Block_Kind;
      Block.Flush_Resetting_Data := Flush_Resetting_Data_Type_Default;
      Block.Next_Block_Pos := [others => 0.0 * mm];
      Block.Params := (others => <>);
      --  Existing fixtures exercise literal commanded densities; rounding has dedicated tests below.
      Block.Params.Extrusion_Rounding_Tolerance := 0.0 * mm;
      Block.Params.Bounds := Rectangular_Bounds ([others => -1.0E100 * mm], [others => 1.0E100 * mm]);
      Block.Corners_Extra_Data_End_Indices := [others => Corner_Extra_Data_Vectors.Extended_Index'First];
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
      Block.Profile_Decel_Crackles := [others => 0.0 * mm / s ** 5];
      Block.Timing_Run_Back := [others => 0];
      Block.Timing_Run_Ahead := [others => 0];
      Block.Timing_Distance_Offsets := [others => 0.0 * mm];
      Block.Timing_Time_Offsets := [others => 0.0 * s];
      Block.Timing_Segment_Times := [others => 0.0 * s];
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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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

   procedure Test_Small_Sweep_Helix_Axial_Precision (T : in out Trendy_Test.Operation'Class) is
      Block     : aliased Execution_Block (2);
      Center    : constant Position := [others => 0.0 * mm];
      Fractions : constant Sample_Fraction_Array := [-0.25, 0.0, 0.13, 0.5, 0.72, 1.0, 1.25];
      Tolerance : constant Length := 32.0 * Dimensionless'Model_Epsilon * 10.0 * mm;
   begin
      T.Register;

      for Clockwise in Boolean loop
         for Descending in Boolean loop
            declare
               Sign : constant Dimensionless := (if Clockwise then -1.0 else 1.0);
            begin
               Block.Corners (1) :=
                 [X_Axis | Y_Axis => 1.0 * mm,
                  Z_Axis => (if Descending then 10.0 * mm else 0.0 * mm),
                  E_Axis => 3.0 * mm];
               Block.Corners (2) :=
                 [X_Axis => (1.0 - Sign * 1.0E-12) * mm,
                  Y_Axis => (1.0 + Sign * 1.0E-12) * mm,
                  Z_Axis => (if Descending then 0.0 * mm else 10.0 * mm),
                  E_Axis => 9.0 * mm];
               Block.Primitives (2) :=
                 Make_Helix_Primitive (Block.Corners (1), Block.Corners (2), Center, Clockwise);
            end;

            declare
               Primitive : constant Derived_Path_Primitive := Spatial_Primitive (Block'Access, 2);
            begin
               T.Assert
                 (Primitive.Kind = Helix_Primitive_Kind and then abs Primitive.Theta_Delta < 2.0E-12,
                  "Small-sweep axial precision fixture must remain a tiny helix");
               for Fraction of Fractions loop
                  declare
                     Point : constant Position :=
                       Primitive_Point_At_Distance (Block'Access, 2, Fraction * Primitive.Length);
                     Clamped : constant Dimensionless := Dimensionless'Max (0.0, Dimensionless'Min (Fraction, 1.0));
                     Expected_Z : constant Length := (if Descending then 1.0 - Clamped else Clamped) * 10.0 * mm;
                  begin
                     T.Assert
                       (abs (Point (Z_Axis) - Expected_Z) <= Tolerance,
                        "Small-sweep helix Z must follow distance fraction" & Fraction'Image
                        & ": expected" & Expected_Z'Image & ", got" & Point (Z_Axis)'Image);
                     T.Assert
                       (Point (E_Axis) = Block.Corners (1) (E_Axis), "Spatial helix evaluation must preserve E");
                  end;
               end loop;
            end;
         end loop;
      end loop;
   end Test_Small_Sweep_Helix_Axial_Precision;

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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
         if Expected in Stereographic_Transition | Circular_Transition | Biarc_Transition then
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
        (Kind        => Biarc,
         Biarc_Params =>
           (Axial_Deviation_Maxes    => Deviation,
            Corner_Miss_Distance_Max => 100.0 * mm,
            Shape_Bias               => 0.0,
            Trim_Max                 => 5.0 * mm));
      Assert_Dispatched (Biarc_Transition, "Post-failure biarc", T);
      T.Assert
        (not Bounds_Are_Zero (Workspace.Corner_Derivative_Bounds (2)),
         "A successful later family does not retain stale hard-stop workspace state");
   end Test_Corner_Family_Dispatch_And_Fail_Closed;

   procedure Test_Biarc_Helix_Line_Dispatch (T : in out Trendy_Test.Operation'Class) is
      type Block_Access is access Execution_Block;
      type Workspace_Access is access Planning_Workspace;

      Block     : constant Block_Access := new Execution_Block (3);
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Radius    : Length := 20.0 * mm;
      type Radii is array (Positive range <>) of Length;
      Deviation : constant Axial_Deviation_Limits := [E_Axis => 0.0 * mm, others => 100.0 * mm];

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
               Trim_Max                 => 0.15 * Radius));
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
               Incoming_Tangent : constant Position_Scale := Primitive_Direction_At_Distance
                 (Block.all'Access, 2, Block.Primitive_Start_Distances (2) + Block.Primitive_Distances (2));
               Outgoing_Tangent : constant Position_Scale := Primitive_Direction_At_Distance
                 (Block.all'Access, 3, Block.Primitive_Start_Distances (3));
               Step : constant Length := Transition_Length * 1.0E-7;
               Start_Point : constant Position := Point_At_Distance (Block.Corner_Transitions (2), 0.0 * mm);
               After_Start : constant Position := Point_At_Distance (Block.Corner_Transitions (2), Step);
               Before_Finish : constant Position :=
                 Point_At_Distance (Block.Corner_Transitions (2), Transition_Length - Step);
               Finish_Point : constant Position :=
                 Point_At_Distance (Block.Corner_Transitions (2), Transition_Length);
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
               for Axis in Axis_Name loop
                  T.Assert (abs ((After_Start (Axis) - Start_Point (Axis)) / Step - Incoming_Tangent (Axis)) < 1.0E-5,
                    Name & " Biarc matches the tangent at the trimmed incoming endpoint");
                  T.Assert
                    (abs ((Finish_Point (Axis) - Before_Finish (Axis)) / Step - Outgoing_Tangent (Axis)) < 1.0E-5,
                    Name & " Biarc matches the tangent at the trimmed outgoing endpoint");
               end loop;
            end;
         end if;
      end Assert_Dispatched;
   begin
      T.Register;

      for R of Radii'[0.5 * mm, 2.0 * mm, 20.0 * mm] loop
         Radius := R;
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
      end loop;
   end Test_Biarc_Helix_Line_Dispatch;

   procedure Test_Profile_Window_Transition_Bounds (T : in out Trendy_Test.Operation'Class) is
      type Block_Access is access Execution_Block;
      type Workspace_Access is access Planning_Workspace;

      Block        : constant Block_Access := new Execution_Block (4);
      Workspace    : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
           Window_Axial_Derivative_Bounds (Block.all'Access, 3, Start_Window);
         End_Actual       : constant Unit_Speed_Axial_Derivative_Bounds :=
           Window_Axial_Derivative_Bounds (Block.all'Access, 3, End_Window);
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
              Window_Axial_Derivative_Bounds (Block.all'Access, 3, Spanning_Window);
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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
      Extra_Data   : Corner_Extra_Data_Vectors.Vector;
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
      Tested_Preprocessor.Run (Block'Access, Extra_Data, Initial_Position, Reset_Called);

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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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

   procedure Test_Accepted_Corner_Survives_Growth (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (14);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Accepted_Trim : constant Length := 0.75596655525933243 * mm;
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Ignore_E_In_XYZE := True;
      Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => 0.02 * mm];
      Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 0.02 * mm;
      --  A short prefix of the deterministic helix benchmark. Corner 10 first accepts a small allocation;
      --  a neighbour's repair enlarges it beyond what construction can certify.
      Block.Corners :=
        [[X_Axis => 0.0 * mm, Y_Axis => 0.0 * mm, Z_Axis => 0.0 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 150.0 * mm, Y_Axis => 150.0 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 151.7689067924221 * mm, Y_Axis => 152.04625784907958 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 148.1423305556512 * mm, Y_Axis => 153.4064504011324 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 147.8299422910298 * mm, Y_Axis => 152.49869650645897 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 149.69367571512498 * mm, Y_Axis => 151.23178268600722 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 149.7786032118826 * mm, Y_Axis => 150.6464231385447 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 149.62905257455276 * mm, Y_Axis => 114.24428673903903 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 149.22079669569533 * mm, Y_Axis => 112.922871222737 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 149.00032900938973 * mm, Y_Axis => 111.7126424059704 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 155.29126572392974 * mm, Y_Axis => 100.55467106864326 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 147.1647603765454 * mm, Y_Axis => 122.33090394883033 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 131.16923577434093 * mm, Y_Axis => 135.36398844439722 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm],
         [X_Axis => 117.83208917333783 * mm, Y_Axis => 128.77945845718062 * mm, Z_Axis => 0.3 * mm, E_Axis => 0.0 * mm]];
      Block.Primitives (4) := Make_Helix_Primitive
        (Block.Corners (3), Block.Corners (4),
         [X_Axis => 147.9665343403133 * mm, Y_Axis => 147.42301235431944 * mm, others => 0.0 * mm], False);
      Block.Primitives (5) := Make_Helix_Primitive
        (Block.Corners (4), Block.Corners (5),
         [X_Axis => 148.47628593163282 * mm, Y_Axis => 152.78389672853024 * mm, others => 0.0 * mm], False);
      Block.Primitives (6) := Make_Helix_Primitive
        (Block.Corners (5), Block.Corners (6),
         [X_Axis => 147.16004992514678 * mm, Y_Axis => 149.508921524162 * mm, others => 0.0 * mm], True);
      Block.Primitives (7) := Make_Helix_Primitive
        (Block.Corners (6), Block.Corners (7),
         [X_Axis => 150.20375755135146 * mm, Y_Axis => 151.00694776730847 * mm, others => 0.0 * mm], False);
      Block.Primitives (8) := Make_Helix_Primitive
        (Block.Corners (7), Block.Corners (8),
         [X_Axis => 129.1506969821704 * mm, Y_Axis => 132.52979321991737 * mm, others => 0.0 * mm], True);
      Block.Primitives (9) := Make_Helix_Primitive
        (Block.Corners (8), Block.Corners (9),
         [X_Axis => 150.25035827878713 * mm, Y_Axis => 113.32855841129819 * mm, others => 0.0 * mm], False);
      Block.Primitives (11) := Make_Helix_Primitive
        (Block.Corners (10), Block.Corners (11),
         [X_Axis => 159.2350509670333 * mm, Y_Axis => 110.13062396414308 * mm, others => 0.0 * mm], False);
      Block.Primitives (12) := Make_Helix_Primitive
        (Block.Corners (11), Block.Corners (12),
         [X_Axis => 135.90445276157476 * mm, Y_Axis => 105.72430556046167 * mm, others => 0.0 * mm], False);
      Block.Primitives (13) := Make_Helix_Primitive
        (Block.Corners (12), Block.Corners (13),
         [X_Axis => 154.63867031956724 * mm, Y_Axis => 147.8358532849211 * mm, others => 0.0 * mm], True);
      Block.Primitives (14) := Make_Helix_Primitive
        (Block.Corners (13), Block.Corners (14),
         [X_Axis => 138.0316421865766 * mm, Y_Axis => 104.66435186400378 * mm, others => 0.0 * mm], False);
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      T.Assert (Transition_Kind (Block.Corner_Transitions (10)) = Stereographic_Transition,
                "An accepted corner survives a failed enlargement after neighbour repairs");
      T.Assert (abs (Block.Primitive_Start_Distances (11) - Accepted_Trim) < 1.0E-8 * mm,
                "A failed enlargement retains the previously accepted outgoing trim");
      T.Assert (abs (Primitive_Length (Block'Access, 10) - Block.Primitive_Start_Distances (10)
                     - Block.Primitive_Distances (10) - Accepted_Trim) < 1.0E-8 * mm,
                "A failed enlargement retains the previously accepted incoming trim");
      for I in Block.Primitives'Range loop
         T.Assert (Block.Primitive_Distances (I) >= 0.0 * mm, "Saved trims fit the shared primitive");
         declare
            Incoming_End : constant Position := Point_At_Distance
              (Block.Corner_Transitions (I - 1), Arc_Length (Block.Corner_Transitions (I - 1)));
            Outgoing_Start : constant Position := Point_At_Distance (Block.Corner_Transitions (I), 0.0 * mm);
         begin
            T.Assert (Point_Distance
              (Incoming_End, Primitive_Point_At_Distance (Block'Access, I, Block.Primitive_Start_Distances (I)))
              < 1.0E-8 * mm, "Saved outgoing trim matches the retained evaluator");
            T.Assert (Point_Distance
              (Outgoing_Start, Primitive_Point_At_Distance
                (Block'Access, I, Block.Primitive_Start_Distances (I) + Block.Primitive_Distances (I)))
              < 1.0E-8 * mm, "Saved incoming trim matches the retained evaluator");
         end;
      end loop;
   end Test_Accepted_Corner_Survives_Growth;

   procedure Test_Stereographic_Curved_Corners (T : in out Trendy_Test.Operation'Class) is
      type Block_Access is access Execution_Block;
      type Workspace_Access is access Planning_Workspace;

      Block     : constant Block_Access := new Execution_Block (3);
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Radius    : constant Length := 20.0 * mm;

      procedure Assert_Blends (Name : String; Zero_Z_Limit : Boolean := False);

      procedure Assert_Blends (Name : String; Zero_Z_Limit : Boolean := False) is
      begin
         Block.Params.Bounds := Rectangular_Bounds ([others => -100.0 * mm], [others => 100.0 * mm]);
         Block.Params.Cornering :=
           (Kind                 => Stereographic,
            Stereographic_Params =>
              (Axial_Deviation_Maxes    =>
                 [E_Axis => 0.0 * mm, Z_Axis => (if Zero_Z_Limit then 0.0 * mm else 0.1 * mm), others => 0.1 * mm],
               Corner_Miss_Distance_Max => 0.1 * mm,
               Shape_Bias               => 0.0,
               Circularity              => 0.0));
         Normalize_And_Blend (Block.all, Motor_Map, Workspace);
         T.Assert
           (Transition_Kind (Block.Corner_Transitions (2)) = Stereographic_Transition,
            Name & " blends instead of stopping: " & Transition_Kind (Block.Corner_Transitions (2))'Image);
      end Assert_Blends;
   begin
      T.Register;

      --  Matching tangents with a curvature jump: the first width estimate must come from the jump, not from the
      --  whole adjacent segment.
      Reset_Early_Limiter_Block (Block.all);
      Block.Corners (1) := [X_Axis => -Radius, others => 0.0 * mm];
      Block.Corners (3) := [X_Axis => Radius, Y_Axis => Radius, others => 0.0 * mm];
      Block.Primitives (3) :=
        Make_Helix_Primitive
          (Block.Corners (2), Block.Corners (3), [Y_Axis => Radius, others => 0.0 * mm], Clockwise => False);
      Assert_Blends ("A line running into a tangent arc");
      Assert_Blends ("A line into a flat arc with zero Z deviation", Zero_Z_Limit => True);
      for Sample in 0 .. 64 loop
         T.Assert
           (Point_At_Parameter (Block.Corner_Transitions (2), Dimensionless (Sample) / 64.0) (Z_Axis) = 0.0 * mm,
            "The zero-limit axis remains exactly constant along the blend");
      end loop;

      --  A rising helix turning onto a rising line: the endpoint tangents and the chord are not coplanar.
      Reset_Early_Limiter_Block (Block.all);
      Block.Corners (1) := [X_Axis => -Radius, Y_Axis => Radius, others => 0.0 * mm];
      Block.Corners (2) := [Z_Axis => 1.0 * mm, others => 0.0 * mm];
      Block.Corners (3) := [Y_Axis => Radius, Z_Axis => 2.0 * mm, others => 0.0 * mm];
      Block.Primitives (2) :=
        Make_Helix_Primitive
          (Block.Corners (1), Block.Corners (2), [Y_Axis => Radius, others => 0.0 * mm], Clockwise => False);
      Assert_Blends ("A rising helix onto a rising line");
      Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes (Z_Axis) := 0.0 * mm;
      Normalize_And_Blend (Block.all, Motor_Map, Workspace);
      T.Assert (Policy (Block.Corner_Transitions (2)) = Hard_Stop,
                "A zero deviation limit still stops a corner with actual Z motion");

      --  An arc curving left into a right turn: its curvature turns the blend out of the endpoint cone, so no-bulge
      --  is impossible and When_Unavoidable has to allow the bulge.
      Reset_Early_Limiter_Block (Block.all);
      Block.Corners (1) := [X_Axis => -Radius, Y_Axis => Radius, others => 0.0 * mm];
      Block.Corners (3) := [X_Axis => Radius, Y_Axis => -Radius, others => 0.0 * mm];
      Block.Primitives (2) :=
        Make_Helix_Primitive
          (Block.Corners (1), Block.Corners (2), [Y_Axis => Radius, others => 0.0 * mm], Clockwise => False);
      Assert_Blends ("An arc curving away from the turn");
      Assert_Blends ("A flat arc into a line with zero Z deviation", Zero_Z_Limit => True);
   end Test_Stereographic_Curved_Corners;

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
      Extra_Data         : Corner_Extra_Data_Vectors.Vector;
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
      Boundary_Preprocessor.Run (Homing_Block'Access, Extra_Data, Initial_Position, Reset_Called);

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
      Boundary_Preprocessor.Run (Following_Block'Access, Extra_Data, Stopped_Position, Reset_Called);
      T.Assert
        (not Reset_Called
         and then Following_Block.Corners (1) = Stopped_Position
         and then Following_Block.Corners (2) = Following_End,
         "Following preprocessing and helix validation use the resolved stopped position");
   end Test_Homing_Boundary_Uses_Resolved_Position;

   procedure Test_Extrusion_Density_Rounding (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Ignore_E_In_XYZE := False;
      Block.Params.Extrusion_Rounding_Tolerance := 0.000_1 * mm;
      Block.Params.Extrusion_Velocity_Change_Max := 0.0 * mm / s;
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
              "Exactly shared normalized densities permit blending with zero E velocity-change allowance");
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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Limits : constant Axial_Deviation_Limits := [E_Axis => 0.0 * mm, others => 0.1 * mm];
   begin
      T.Register;
      for Family in Cornering_Kind loop
         Reset_Early_Limiter_Block (Block);
         case Family is
            when Stereographic => Block.Params.Cornering :=
              (Kind => Stereographic, Stereographic_Params => (Axial_Deviation_Maxes => Limits, others => <>));
            when Circular => Block.Params.Cornering :=
              (Kind => Circular, Circular_Params => (Axial_Deviation_Maxes => Limits, others => <>));
            when Biarc => Block.Params.Cornering :=
              (Kind => Biarc, Biarc_Params => (Axial_Deviation_Maxes => Limits, others => <>));
            when Sharp_SCV => Block.Params.Cornering := (Kind => Sharp_SCV, others => <>);
         end case;
         Block.Params.Extrusion_Velocity_Change_Max := 0.0 * mm / s;
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
            Block.Params.Extrusion_Velocity_Change_Max := 1.0 * mm / s;
            Normalize_And_Blend (Block, Motor_Map, Workspace);
            T.Assert (Arc_Length (Block.Corner_Transitions (2)) > 0.0 * mm,
              "A positive velocity-jump allowance permits blending despite a zero E deviation entry");
         end if;
      end loop;
   end Test_Extrusion_Zero_Allowance_Geometry;

   procedure Test_Extrusion_Instantaneous_Velocity_Change (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (4);
      Motor_Map : Motor_Position_Map := [others => [others => 0.0 / mm]];
      Maximum : constant Velocity := 0.5 * mm / s;
   begin
      T.Register;
      for Scenario in 1 .. 7 loop
         Reset_Early_Limiter_Block (Block);
         Block.Params.Extrusion_Velocity_Change_Max := Maximum;
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
            Block.Params.Extrusion_Velocity_Change_Max := Maximum / 2.0;
            T.Assert (Extrusion_Junction_Ceiling (Block'Access, 2, Block.Corner_Velocity_Limits (2))
              < Block.Corner_Velocity_Limits (2),
              "The junction ceiling lowers a junction whose velocity jump exceeds the configured allowance");
         end if;
         Motor_Map := [others => [others => 0.0 / mm]];
      end loop;
   end Test_Extrusion_Instantaneous_Velocity_Change;

   procedure Test_Extrusion_Tangential_Limit (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Ignore_E_In_XYZE := False;
      Block.Params.Tangential_Velocity_Max := 10.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 1000.0 * mm / s];
      Block.Params.Axial_Acceleration_Maxes := [E_Axis => 5.0 * mm / s ** 2, others => 1.0E6 * mm / s ** 2];
      Block.Params.Axial_Jerk_Maxes := [E_Axis => 50.0 * mm / s ** 3, others => 1.0E7 * mm / s ** 3];
      Block.Params.Axial_Snap_Maxes := [E_Axis => 500.0 * mm / s ** 4, others => 1.0E8 * mm / s ** 4];
      Block.Params.Axial_Crackle_Maxes := [E_Axis => 5000.0 * mm / s ** 5, others => 1.0E9 * mm / s ** 5];
      Block.Params.Extrusion_Velocity_Change_Max := 5.0 * mm / s;
      Block.Original_Segment_Feedrates := [others => 100.0 * mm / s];
      Block.Corners :=
        [1 => [others => 0.0 * mm],
         2 => [X_Axis => 20.0 * mm, others => 0.0 * mm],
         3 => [X_Axis => 40.0 * mm, E_Axis => 20.0 * mm, others => 0.0 * mm],
         4 => [X_Axis => 60.0 * mm, E_Axis => 20.0 * mm, others => 0.0 * mm]];
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
      Plan_Kinematics (Block, Motor_Map, Workspace);
      T.Assert (Block.Corner_Velocity_Limits (2) > 0.0 * mm / s,
        "The XYZE tangential limit retains a moving density change");
      for I in Block.Primitives'Range loop
         declare
            Segment_Duration : constant Time := Segment_Time (Block'Access, I);
            DT : constant Time := Segment_Duration / 1000.0;
         begin
            for K in 0 .. 999 loop
               declare
                  First : constant Position :=
                    Segment_Pos_At_Time (Block'Access, I, Segment_Duration * (Dimensionless (K) / 1000.0));
                  Last : constant Position :=
                    Segment_Pos_At_Time (Block'Access, I, Segment_Duration * (Dimensionless (K + 1) / 1000.0));
               begin
                  T.Assert (Point_Distance (First, Last) / DT <= 10.00001 * mm / s,
                    "Combined XYZE speed respects the tangential limit across density changes");
               end;
            end loop;
         end;
      end loop;
   end Test_Extrusion_Tangential_Limit;

   procedure Test_Extrusion_Feedrate_Retry (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (2);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      T.Register;
      for Ignore_E in Boolean loop
         Reset_Early_Limiter_Block (Block);
         Block.Params.Ignore_E_In_XYZE := Ignore_E;
         Block.Params.Tangential_Velocity_Max := 10.0 * mm / s;
         Block.Params.Axial_Velocity_Maxes := [others => 1000.0 * mm / s];
         Block.Original_Segment_Feedrates := [others => 100.0 * mm / s];
         Block.Corners :=
           [1 => [others => 0.0 * mm], 2 => [X_Axis | E_Axis => 20.0 * mm, others => 0.0 * mm]];
         Normalize_And_Blend (Block, Motor_Map, Workspace);
         Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
         declare
            Original : constant Velocity := Block.Original_Segment_Feedrates (2);
            Initial_Limit : constant Velocity := Block.Limited_Segment_Feedrates (2);
         begin
            Tested_Corner_Blender.Run (Block, Motor_Map, Workspace, Force_Stops => True);
            Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map, Normalize_Feedrates => False);
            T.Assert (Block.Original_Segment_Feedrates (2) = Original,
              "Geometry retries do not normalize the commanded feedrate twice");
            T.Assert (Block.Limited_Segment_Feedrates (2) = Initial_Limit,
              "Geometry retries retain the tangential velocity cap in the same scalar coordinates");
            T.Assert (Initial_Limit <= (if Ignore_E then 10.0 else 10.0 / Dimensionless (Sqrt (2.0))) * mm / s,
              "The tangential cap uses the configured XYZ or XYZE metric");
         end;
      end loop;
   end Test_Extrusion_Feedrate_Retry;

   procedure Test_Extrusion_Corner_Repair (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (3);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Limits : constant Axial_Deviation_Limits := [E_Axis => 0.0 * mm, others => 0.1 * mm];
   begin
      T.Register;
      for Family in Cornering_Kind loop
         if Family /= Sharp_SCV then
            Reset_Early_Limiter_Block (Block);
            case Family is
               when Stereographic =>
                  Block.Params.Cornering :=
                    (Kind => Stereographic, Stereographic_Params =>
                       (Axial_Deviation_Maxes => Limits, Corner_Miss_Distance_Max => 100.0 * mm, others => <>));
               when Circular =>
                  Block.Params.Cornering :=
                    (Kind => Circular, Circular_Params =>
                       (Axial_Deviation_Maxes => Limits, Corner_Miss_Distance_Max => 100.0 * mm,
                        Radius_Max => 5.0 * mm));
               when Biarc =>
                  Block.Params.Cornering :=
                    (Kind => Biarc, Biarc_Params =>
                       (Axial_Deviation_Maxes => Limits, Corner_Miss_Distance_Max => 100.0 * mm,
                        Trim_Max => 5.0 * mm, others => <>));
               when Sharp_SCV =>
                  null;
            end case;
            Block.Corners :=
              [1 => [X_Axis => -20.0 * mm, E_Axis => 10.0 * mm, others => 0.0 * mm],
               2 => [E_Axis => 12.0 * mm, others => 0.0 * mm],
               3 => [Y_Axis => 20.0 * mm, E_Axis => 14.0 * mm, others => 0.0 * mm]];
            --  Zero is outside the real E bounds, but spatial blending temporarily sets E coordinates to zero.
            Block.Params.Bounds.Lower_E := 10.0 * mm;
            Block.Params.Bounds.Upper_E := 14.0 * mm;
            Normalize_And_Blend (Block, Motor_Map, Workspace);
            T.Assert (Policy (Block.Corner_Transitions (2)) = Derivative_Bounded,
              Family'Image & " repairs an oversized blend without a false E-axis overflow");
            for K in 0 .. 128 loop
               declare
                  P : constant Position := Point_At_Distance
                    (Block.Corner_Transitions (2), Arc_Length (Block.Corner_Transitions (2))
                       * (Dimensionless (K) / 128.0));
               begin
                  T.Assert (Length'Min (abs P (X_Axis), abs P (Y_Axis)) <= 0.1 * mm,
                    Family'Image & " repaired blend obeys the spatial deviation corridor");
               end;
            end loop;
            T.Assert (Block.Params.Bounds.Lower_E = 10.0 * mm and then Block.Corners (2) (E_Axis) = 12.0 * mm,
              "Blending preserves the commanded E coordinates and bounds");
         end if;
      end loop;
   end Test_Extrusion_Corner_Repair;

   procedure Test_Extrusion_Geometry_And_Compensation (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
                           T.Assert (abs (P (E_Axis) - Ref) <= 1.0E-12 * mm,
                             "E follows the shortened-path reference at the commanded density");
                        end;
                     end loop;
                  end;
               end loop;
               T.Assert
                 (abs (Extrusion_Reference_At_Distance (Block'Access, 4, Segment_Total_Distance (Block'Access, 4))
                       - Expected)
                    < 1.0E-12 * mm, "Shortening preserves integrated extrusion including the compensation section");
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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (4);
      Motor_Map : Motor_Position_Map := [others => [others => 0.0 / mm]];
      type Numbers is array (Positive range <>) of Long_Float;

      procedure Check_Segment (I : Finishing_Corners_Index);
      procedure Check_Segment (I : Finishing_Corners_Index) is
         Window : constant Profile_Window := Selected_Profile_Window (Block'Access, I);
         Profile : constant Feedrate_Profile := Block.Feedrate_Profiles (I);
         C : constant Crackle := Block.Profile_Crackles (I);
         Ends : constant Profile_End_Pair := Block.Profile_Ends (I);
         Start_Vel : constant Velocity := Central_Profile_Start_Velocity (Block'Access, I);
         Prefix : constant Time := Segment_Prefix_Time (Block'Access, I);
         Segment_Duration : constant Time := Segment_Time (Block'Access, I);
         Limits : constant Numbers := [5.0, 50.0, 500.0, 5_000.0, 50_000.0];
      begin
         T.Assert
           (Segment_Duration > 0.0 * s and then Segment_Duration < 1.0E6 * s,
            "Every mixed/E-only segment has a finite duration");
         for K in 0 .. 500 loop
            declare
               --  Crackle and E velocity are discontinuous at a join. Evaluate on the segment's interior side.
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

               procedure Sample (Part : Feedrate_Profile; U : Time; Cm : Crackle; V0 : Velocity; Offset : Length);
               procedure Sample (Part : Feedrate_Profile; U : Time; Cm : Crackle; V0 : Velocity; Offset : Length) is
               begin
                  D := Offset + Distance_At_Time (Part, U, Cm, V0);
                  V := Long_Float (Velocity_At_Time (Part, U, Cm, V0) / (mm / s));
                  A := Long_Float (Acceleration_At_Time (Part, U, Cm) / (mm / s ** 2));
                  J := Long_Float (Jerk_At_Time (Part, U, Cm) / (mm / s ** 3));
                  Snap_Raw := Long_Float (Snap_At_Time (Part, U, Cm) / (mm / s ** 4));
                  Cr := Long_Float (Crackle_At_Time (Part, U, Cm) / (mm / s ** 5));
               end Sample;
            begin
               if At_Time < Prefix and then Ends.Enabled then
                  Sample (Ends.Prefix.Profile, At_Time, Ends.Prefix.Max_Crackle,
                    Block.Corner_Velocity_Limits (I - 1), 0.0 * mm);
               elsif At_Time < Prefix then
                  D := At_Time * Start_Vel;
                  V := Long_Float (Start_Vel / (mm / s));
               elsif At_Time > Prefix + Total_Time (Profile) and then Ends.Enabled then
                  Sample (Ends.Suffix.Profile,
                    Time'Min (At_Time - Prefix - Total_Time (Profile), Total_Time (Ends.Suffix.Profile)),
                    Ends.Suffix.Max_Crackle, Ends.Suffix.Boundary_Velocity, Window.Start_Distance + Window.Distance);
               elsif At_Time > Prefix + Total_Time (Profile) then
                  V := Long_Float (Block.Corner_Velocity_Limits (I) / (mm / s));
                  D := Window.Start_Distance + Window.Distance
                    + (At_Time - Prefix - Total_Time (Profile)) * Block.Corner_Velocity_Limits (I);
               else
                  Sample (Profile, Local, C, Start_Vel, Window.Start_Distance);
               end if;
               E := [Density * V, Density * A, Density * J, Density * Snap_Raw, Density * Cr];
               T.Assert
                 (abs (Segment_Pos_At_Time (Block'Access, I, At_Time) (E_Axis)
                    - (Block.Extrusion_Reference_Positions (I - 1) + Block.Extrusion_Densities (I) * D))
                  <= 1.0E-10 * mm,
                  "Executed E follows the shortened-path reference");
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
      for Scenario in 1 .. 10 loop
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
            Block.Params.Extrusion_Velocity_Change_Max := 0.0 * mm / s;
         elsif Scenario in 3 .. 4 then
            Block.Corners (3) := [X_Axis => 10.0 * mm, E_Axis => -1.0 * mm, others => 0.0 * mm];
            Block.Corners (4) := [X_Axis => 10.1 * mm, E_Axis => 9.0 * mm, others => 0.0 * mm];
            if Scenario = 4 then
               Block.Primitives (4) := Make_Helix_Primitive
                 (Block.Corners (3), Block.Corners (4),
                  [X_Axis => 10.05 * mm, others => 0.0 * mm], Clockwise => False);
            end if;
         elsif Scenario = 9 then
            Block.Corners :=
              [1 => [others => 0.0 * mm],
               2 => [X_Axis => 9.5 * mm, E_Axis => 0.95 * mm, others => 0.0 * mm],
               3 => [X_Axis => 10.0 * mm, E_Axis => 0.96 * mm, others => 0.0 * mm],
               4 => [X_Axis | Y_Axis => 10.0 * mm, E_Axis => 1.96 * mm, others => 0.0 * mm]];
         elsif Scenario = 10 then
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
               when 7 => Block.Params.Cornering := (Kind => Biarc, others => <>);
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
              "Zero E velocity-change allowance forces safe stops at density discontinuities");
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
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
            --  Exercise density changes on the retained geometry before allowing the duration optimizer to choose stops.
            Plan_Fixed_Geometry (Block, Motor_Map, Workspace);
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
               T.Assert (Block.Profile_Crackles (3) > 0.99 * Block.Params.Axial_Crackle_Maxes (X_Axis),
                 "Travel retains the XYZ crackle budget next to the E velocity jumps");
               T.Assert
                 ((Segment_Pos_At_Time (Block'Access, 3, Segment_Time (Block'Access, 3) / 2.0 + 1.0E-5 * s)
                     (X_Axis)
                   - Segment_Pos_At_Time (Block'Access, 3, Segment_Time (Block'Access, 3) / 2.0) (X_Axis))
                    / (1.0E-5 * s) > 10.0 * mm / s,
                  "The travel section accelerates between the E velocity jumps");
               T.Assert (abs (Segment_Pos_At_Time (Block'Access, 3, Segment_Time (Block'Access, 3) / 2.0) (E_Axis)
                 - 2.0 * mm) < 1.0E-10 * mm, "E remains stopped during the travel section");
            end if;
            Plan_Kinematics (Block, Motor_Map, Workspace);
            declare
               Selected_Time : Time := 0.0 * s;
            begin
               for I in Block.Primitives'Range loop
                  Selected_Time := @ + Segment_Time (Block'Access, I);
               end loop;
               T.Assert (Selected_Time <= Job_Time,
                 "Duration selection may introduce stops only when the complete trajectory is faster");
            end;
         end;
      end loop;
   end Test_Extrusion_Transition_Locality;

   procedure Test_Fixed_Geometry_Region_Boundary (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (5);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Job_Time : Time := 0.0 * s;
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Tangential_Velocity_Max := 250.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 200.0 * mm / s];
      Block.Params.Axial_Acceleration_Maxes := [others => 3000.0 * mm / s ** 2];
      Block.Params.Axial_Jerk_Maxes := [others => 1.0E5 * mm / s ** 3];
      Block.Params.Axial_Snap_Maxes := [others => 5.0E6 * mm / s ** 4];
      Block.Params.Axial_Crackle_Maxes := [others => 5.0E8 * mm / s ** 5];
      Block.Params.Cornering := (Kind => Sharp_SCV, Sharp_SCV_Params => (Square_Corner_Velocity => 0.0 * mm / s));
      Block.Original_Segment_Feedrates := [others => 250.0 * mm / s];
      Block.Corners :=
        [[others => 0.0 * mm],
         [X_Axis => 40.0 * mm, others => 0.0 * mm],
         [X_Axis | Y_Axis => 40.0 * mm, others => 0.0 * mm],
         [Y_Axis => 40.0 * mm, others => 0.0 * mm],
         [others => 0.0 * mm]];
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
      T.Assert ((for all V of Block.Limited_Segment_Feedrates => abs (V - 199.8 * mm / s) < 1.0E-9 * mm / s),
        "The square starts with the expected axial feedrate limit");
      Plan_Fixed_Geometry (Block, Motor_Map, Workspace);
      T.Assert ((for all V of Block.Limited_Segment_Feedrates => abs (V - 199.8 * mm / s) < 1.0E-9 * mm / s),
        "Boundary roundoff must not halve the square's feedrates; first segment"
        & Block.Limited_Segment_Feedrates (2)'Image);
      for I in Block.Primitives'Range loop
         T.Assert (Workspace.Planning_Attempts (I) = 0, "The boundary profile needs no recovery attempts");
         Job_Time := Job_Time + Segment_Time (Block'Access, I);
         declare
            End_Time : constant Time := Segment_Time (Block'Access, I);
            Before_End : constant Time := Time'Adjacent (End_Time, 0.0 * s);
         begin
            --  At End_Time the executor substitutes the requested endpoint. Inspect the previous representable
            --  time too, otherwise that substitution could conceal an erroneous profile and a position jump.
            T.Assert (abs (Segment_Pos_At_Time (Block'Access, I, Before_End) - Block.Corners (I)) < 1.0E-10 * mm,
              "The executed square approaches each endpoint without a position jump");
            T.Assert (abs (Segment_Pos_At_Time (Block'Access, I, End_Time) - Block.Corners (I)) < 1.0E-10 * mm,
              "The executed square reaches each requested endpoint");
         end;
      end loop;
      T.Assert (Job_Time < 1.4 * s, "The stopped 40 mm square retains its expected duration");
   end Test_Fixed_Geometry_Region_Boundary;

   procedure Test_Extrusion_Position_Bounds (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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

   procedure Test_Extrusion_Workspace_Reuse (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
      Printing.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 1.0 * mm;
      Printing.Original_Segment_Feedrates := [others => 30.0 * mm / s];
      Printing.Corners :=
        [1 => [others => 0.0 * mm],
         2 => [X_Axis => 10.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
         3 => [X_Axis => 20.0 * mm, E_Axis => 2.0 * mm, others => 0.0 * mm],
         4 => [X_Axis => 30.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm]];
      Plan (Printing);
      T.Assert (Printing.Corner_Velocity_Limits (2) > 0.0 * mm / s,
        "Workspace-reuse fixture keeps moving through a density change");
      for I in Durations'Range loop
         Durations (I) := Segment_Time (Printing'Access, I);
         for J in Positions'Range (2) loop
            Positions (I, J) :=
              Segment_Pos_At_Time (Printing'Access, I, Durations (I) * Dimensionless (J) / 32.0);
         end loop;
      end loop;

      --  Replan without rerunning geometry construction. The limiter must discard stale planning state while
      --  retaining the densities and corrected reference built by Corner_Blender.
      declare
         Reference : constant Block_Segment_Lengths := Printing.Extrusion_Reference_Positions;
         Densities : constant Block_Extrusion_Densities := Printing.Extrusion_Densities;
      begin
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
      Assert_Printing_Unchanged;
      Plan (Printing);
      Assert_Printing_Unchanged;
   end Test_Extrusion_Workspace_Reuse;

   procedure Test_Profile_Local_Curve_Limits (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Curves : Block_Corner_Transitions (Block.Corners'Range);
      I : constant Finishing_Corners_Index := 3;

      procedure Check_Curve_Profile (Part : Profile_End; Window : Profile_Window; Start : Velocity);

      procedure Check_Curve_Profile (Part : Profile_End; Window : Profile_Window; Start : Velocity) is
         Bounds : constant Unit_Speed_Axial_Derivative_Bounds :=
           Window_Axial_Derivative_Bounds (Block'Access, I, Window);
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
      Block.Params.Extrusion_Velocity_Change_Max := 0.0 * mm / s;
      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
      --  This fixture checks profiles on retained curves; duration-based geometry selection is tested separately.
      Plan_Fixed_Geometry (Block, Motor_Map, Workspace);
      T.Assert (Block.Profile_Ends (I).Enabled, "A long primitive gets profiles separate from its tight curved ends");
      T.Assert (Block.Corner_Transitions = Curves, "Local profile limits preserve the blended XYZ geometry");
      T.Assert (Block.Corner_Velocity_Limits (I - 1) = 0.0 * mm / s
        and then Block.Corner_Velocity_Limits (I) = 0.0 * mm / s,
        "Zero E velocity-change allowance still stops at density changes");
      T.Assert (Segment_Time (Block'Access, I) < 0.3 * s,
        "Local profiles and actual derivative budgets avoid throttling the travel move");
      T.Assert (Fast_Velocity_At_Max_Time
        (Block.Feedrate_Profiles (I).Accel, Block.Profile_Crackles (I),
         Central_Profile_Start_Velocity (Block'Access, I)) > 10.0 * mm / s,
        "The retained primitive accelerates above the curved end's speed ceiling");
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
      --  A positive allowance lets the curved ends move through their E velocity jumps.
      Block.Params.Extrusion_Velocity_Change_Max := 1.0 * mm / s;
      Plan_Fixed_Geometry (Block, Motor_Map, Workspace);
      T.Assert (Block.Corner_Velocity_Limits (I - 1) > 0.0 * mm / s
        and then Block.Corner_Velocity_Limits (I) > 0.0 * mm / s,
        "The local-profile fixture moves through both density changes");
      T.Assert (Segment_Time (Block'Access, I) < 0.3 * s, "Moving density changes do not throttle the travel move");
   end Test_Profile_Local_Curve_Limits;

   procedure Test_Profile_Recovery (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
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
         Tested_Kinematic_Limiter.Run (Block, Motor_Map, Workspace);
         Tested_Profile_Generator.Run (Block, Motor_Map, Result);
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

   procedure Test_Profile_Numeric_Recovery (T : in out Trendy_Test.Operation'Class) is
      Params : constant Kinematic_Parameters := (others => <>);
      Eval : Profile_Window_Evaluation := (others => <>);
      Result : Window_Profile_Result;
   begin
      T.Register;
      Eval.Valid := True;
      Eval.Window.Distance := 1.0E100 * mm;
      Eval.Max_Vel := 1.0E-300 * mm / s;
      Eval.Limits := (1.0 * mm / s ** 2, 1.0 * mm / s ** 3, 1.0 * mm / s ** 4, 1.0 * mm / s ** 5);
      Result := Profile_At_Peak (Params, Eval, Eval.Max_Vel, Eval.Max_Vel, Eval.Max_Vel);
      T.Assert (not Result.Valid, "An overflowing coast time is rejected within the recovery handler");
      Eval.Window.Distance := 10.0 * mm;
      Eval.Max_Vel := 2.0 * mm / s;
      Eval.Limits := (0.0 * mm / s ** 2, 0.0 * mm / s ** 3, 0.0 * mm / s ** 4, 0.0 * mm / s ** 5);
      Result := Profile_At_Peak (Params, Eval, Eval.Max_Vel, Eval.Max_Vel, Eval.Max_Vel);
      T.Assert (Result.Valid and then Result.Profile.Coast = 5.0 * s,
        "Zero derivative limits still permit a constant-speed coast");
      T.Assert (Endpoint_Delta_V_Distance (0.0 * mm / s, Eval.Max_Vel, Eval.Limits) = Length'Last,
        "An impossible endpoint ramp returns the conservative bound without escaping recovery");
   end Test_Profile_Numeric_Recovery;

   procedure Assert_Window_Profile
     (Params : Kinematic_Parameters; Bounds : Unit_Speed_Axial_Derivative_Bounds;
      Profile : Feedrate_Profile; C : Crackle; Start_Vel, End_Vel : Velocity;
      Distance : Length; T : in out Trendy_Test.Operation'Class)
   is
      V : constant Velocity := Fast_Velocity_At_Max_Time (Profile.Accel, C, Start_Vel);
      A : Acceleration := 0.0 * mm / s ** 2;
      J : Jerk := 0.0 * mm / s ** 3;
      Sn : Snap := 0.0 * mm / s ** 4;
      Cr : Crackle := 0.0 * mm / s ** 5;
      type Ramps is array (1 .. 2) of Feedrate_Profile_Times;
   begin
      --  Evaluate the independent scalar derivative functions at their extrema, then enclose the full path
      --  with the chain rule. These checks cover every instant, including between interpolation samples.
      for Ramp of Ramps'[Profile.Accel, Profile.Decel] loop
         A := Acceleration'Max (A, Acceleration_At_Time (Ramp, Total_Time (Ramp) / 2.0, C));
         J := Jerk'Max (J, Jerk_At_Time (Ramp, 2.0 * Ramp (1) + Ramp (2), C));
         Sn := Snap'Max (Sn, Snap_At_Time (Ramp, Ramp (1), C));
         Cr := Crackle'Max (Cr, Crackle_At_Time (Ramp, Ramp (1) / 2.0, C));
      end loop;
      for Axis in Axis_Name loop
         T.Assert (Bounds.Velocity (Axis) * V <= Params.Axial_Velocity_Maxes (Axis), "Axial velocity is certified");
         T.Assert (Bounds.Acceleration (Axis) * V ** 2 + Bounds.Velocity (Axis) * A
           <= Params.Axial_Acceleration_Maxes (Axis), "Axial acceleration is certified");
         T.Assert (Bounds.Jerk (Axis) * V ** 3 + 3.0 * Bounds.Acceleration (Axis) * V * A
           + Bounds.Velocity (Axis) * J <= Params.Axial_Jerk_Maxes (Axis), "Axial jerk is certified");
         T.Assert (Bounds.Snap (Axis) * V ** 4 + 6.0 * Bounds.Jerk (Axis) * V ** 2 * A
           + 3.0 * Bounds.Acceleration (Axis) * A ** 2 + 4.0 * Bounds.Acceleration (Axis) * V * J
           + Bounds.Velocity (Axis) * Sn <= Params.Axial_Snap_Maxes (Axis), "Axial snap is certified");
         T.Assert (Bounds.Crackle (Axis) * V ** 5 + 10.0 * Bounds.Snap (Axis) * V ** 3 * A
           + 15.0 * Bounds.Jerk (Axis) * V * A ** 2 + 10.0 * Bounds.Jerk (Axis) * V ** 2 * J
           + 10.0 * Bounds.Acceleration (Axis) * A * J + 5.0 * Bounds.Acceleration (Axis) * V * Sn
           + Bounds.Velocity (Axis) * Cr <= Params.Axial_Crackle_Maxes (Axis), "Axial crackle is certified");
      end loop;
      T.Assert (abs (Distance_At_Time (Profile, Total_Time (Profile), C, Start_Vel) - Distance) < 1.0E-8 * mm,
        "The stretched profile closes its distance");
      T.Assert (abs (Velocity_At_Time (Profile, Total_Time (Profile), C, Start_Vel) - End_Vel) < 1.0E-8 * mm / s,
        "The stretched profile preserves its endpoint velocity");
   end Assert_Window_Profile;

   procedure Test_Small_Arc_Headroom (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (2);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      type Radii is array (1 .. 4) of Length;
      Fast_Time : Time := 0.0 * s;
      Eval : Profile_Window_Evaluation;
      Result : Window_Profile_Result;
   begin
      T.Register;
      for Radius of Radii'[0.25 * mm, 0.5 * mm, 1.0 * mm, 2.0 * mm] loop
         for Clockwise in Boolean loop
            for Low_Feedrate in Boolean loop
               Reset_Early_Limiter_Block (Block);
               Block.Params.Ignore_E_In_XYZE := True;
               Block.Params.Tangential_Velocity_Max := 250.0 * mm / s;
               Block.Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
               Block.Params.Axial_Acceleration_Maxes := [others => 5000.0 * mm / s ** 2];
               Block.Params.Axial_Jerk_Maxes := [others => 500000.0 * mm / s ** 3];
               Block.Params.Axial_Snap_Maxes := [others => 500000000.0 * mm / s ** 4];
               Block.Params.Axial_Crackle_Maxes := [others => 500000000000.0 * mm / s ** 5];
               Block.Corners :=
                 [[X_Axis => Radius, others => 0.0 * mm],
                  [Y_Axis => (if Clockwise then -Radius else Radius), others => 0.0 * mm]];
               Block.Primitives (2) :=
                 Make_Helix_Primitive (Block.Corners (1), Block.Corners (2), [others => 0.0 * mm], Clockwise);
               Block.Original_Segment_Feedrates := [others => (if Low_Feedrate then 30.0 else 250.0) * mm / s];
               Normalize_And_Blend (Block, Motor_Map, Workspace);
               Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
               Plan_Kinematics (Block, Motor_Map, Workspace);
               T.Assert (not Block.Profile_Ends (2).Enabled, "A standalone arc gets the general profile solver");
               T.Assert (Segment_Time (Block'Access, 2) < 0.15 * s, "Small arcs retain useful ramp headroom");
               if Low_Feedrate then
                  T.Assert (Fast_Time <= 1.01 * Segment_Time (Block'Access, 2),
                    "Increasing the speed request cannot cause the former small-arc slowdown");
               else
                  Fast_Time := Segment_Time (Block'Access, 2);
               end if;
               Assert_Window_Profile
                 (Block.Params, Window_Axial_Derivative_Bounds (Block'Access, 2, Block.Profile_Windows (2)),
                  Block.Feedrate_Profiles (2), Block.Profile_Crackles (2), 0.0 * mm / s, 0.0 * mm / s,
                  Segment_Total_Distance (Block'Access, 2), T);
               T.Assert (abs (Segment_Pos_At_Time (Block'Access, 2, Segment_Time (Block'Access, 2))
                 - Block.Corners (2)) < 1.0E-8 * mm, "Small arcs reach their commanded endpoint");
            end loop;
         end loop;
      end loop;
      --  Coasting at the curvature ceiling must remain possible; there is no unconditional speed haircut.
      Eval := Evaluate_Profile_Window (Block'Access, Motor_Map, 2,
        (0.0 * mm, Segment_Total_Distance (Block'Access, 2)), 250.0 * mm / s);
      Result := Generate_Window_Profile (Block.Params, Eval, Eval.Max_Vel, Eval.Max_Vel);
      T.Assert (Result.Valid, "Constant speed at the full certified ceiling remains feasible");
      T.Assert (abs (Total_Time (Result.Profile) - Eval.Window.Distance / Eval.Max_Vel) < 1.0E-10 * s,
        "A coasting arc keeps its full speed");
      Assert_Window_Profile (Block.Params, Eval.Bounds, Result.Profile, Result.Max_Crackle,
        Eval.Max_Vel, Eval.Max_Vel, Eval.Window.Distance, T);
   end Test_Small_Arc_Headroom;

   procedure Test_Small_Arc_Lookahead (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (3);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Planning_Result : Profile_Planning_Result;
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
      Block.Corners :=
        [[X_Axis => 1.0 * mm, others => 0.0 * mm],
         [Y_Axis => 1.0 * mm, others => 0.0 * mm],
         [X_Axis => -1.0 * mm, others => 0.0 * mm]];
      Block.Original_Segment_Feedrates := [others => 250.0 * mm / s];
      for I in Block.Primitives'Range loop
         Block.Primitives (I) := Make_Helix_Primitive
           (Block.Corners (I - 1), Block.Corners (I), [others => 0.0 * mm], False);
      end loop;
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
      Tested_Kinematic_Limiter.Run (Block, Motor_Map, Workspace);
      T.Assert (Block.Corner_Velocity_Limits (2) > 30.0 * mm / s,
        "Forward and reverse lookahead use the headroom at the reachable speed");
      declare
         Junction : constant Velocity := Block.Corner_Velocity_Limits (2);
      begin
         Tested_Profile_Generator.Run (Block, Motor_Map, Planning_Result);
         T.Assert (Planning_Result.Valid, "Generation can realize the speeds certified by lookahead without retry");
         T.Assert (Block.Corner_Velocity_Limits (2) = Junction, "Profile generation preserves the lookahead junction");
         for I in Block.Primitives'Range loop
            T.Assert (not Block.Profile_Ends (I).Enabled, "Tangent-continuous arcs need no corner blend");
            Assert_Window_Profile
              (Block.Params, Window_Axial_Derivative_Bounds (Block'Access, I, Block.Profile_Windows (I)),
               Block.Feedrate_Profiles (I), Block.Profile_Crackles (I),
               Block.Corner_Velocity_Limits (I - 1), Block.Corner_Velocity_Limits (I),
               Segment_Total_Distance (Block'Access, I), T);
         end loop;
      end;
   end Test_Small_Arc_Lookahead;

   procedure Test_Small_Arc_Junction_Continuity (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (3);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      type Junction_Case is (Line_To_Arc, Arc_To_Line, Different_Radii, Reversing_Curvature, Same_Circle);
      type Families is array (Positive range <>) of Cornering_Kind;
      Center : constant Position := [Y_Axis => 0.5 * mm, others => 0.0 * mm];
   begin
      T.Register;
      for Family of Families'[Stereographic, Biarc, Sharp_SCV] loop
         for Allow_Deviation in Boolean loop
            for Junction in Junction_Case loop
               Reset_Early_Limiter_Block (Block);
               Block.Params.Ignore_E_In_XYZE := True;
               case Family is
                  when Stereographic => Block.Params.Cornering := (Kind => Stereographic, others => <>);
                  when Biarc => Block.Params.Cornering := (Kind => Biarc, others => <>);
                  when Sharp_SCV => Block.Params.Cornering := (Kind => Sharp_SCV, others => <>);
                  when others => raise Program_Error;
               end case;
               if Family = Stereographic then
                  Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes :=
                    [others => (if Allow_Deviation then 0.02 else 0.0) * mm];
               end if;
               Block.Original_Segment_Feedrates := [others => 50.0 * mm / s];
               Block.Corners :=
                 [[X_Axis => -0.5 * mm, Y_Axis => 0.5 * mm, others => 0.0 * mm],
                  [others => 0.0 * mm],
                  [X_Axis | Y_Axis => 0.5 * mm, others => 0.0 * mm]];
               Block.Primitives (2) := Make_Helix_Primitive (Block.Corners (1), Block.Corners (2), Center, False);
               Block.Primitives (3) := Make_Helix_Primitive (Block.Corners (2), Block.Corners (3), Center, False);
               case Junction is
                  when Line_To_Arc =>
                     Block.Corners (1) := [X_Axis => -1.0 * mm, others => 0.0 * mm];
                     Block.Primitives (2) := Make_Line_Primitive;
                  when Arc_To_Line =>
                     Block.Corners (3) := [X_Axis => 1.0 * mm, others => 0.0 * mm];
                     Block.Primitives (3) := Make_Line_Primitive;
                  when Different_Radii =>
                     Block.Corners (3) := [X_Axis | Y_Axis => 1.0 * mm, others => 0.0 * mm];
                     Block.Primitives (3) := Make_Helix_Primitive
                       (Block.Corners (2), Block.Corners (3), [Y_Axis => 1.0 * mm, others => 0.0 * mm], False);
                  when Reversing_Curvature =>
                     Block.Corners (3) (Y_Axis) := -0.5 * mm;
                     Block.Primitives (3) := Make_Helix_Primitive
                       (Block.Corners (2), Block.Corners (3), [Y_Axis => -0.5 * mm, others => 0.0 * mm], True);
                  when Same_Circle =>
                     null;
               end case;
               Normalize_And_Blend (Block, Motor_Map, Workspace);
               if Family /= Stereographic or else Junction = Same_Circle then
                  T.Assert (Policy (Block.Corner_Transitions (2)) = Passthrough,
                    "Compatible jets and families permitting curvature jumps retain passthroughs");
               elsif Allow_Deviation then
                  T.Assert (Policy (Block.Corner_Transitions (2)) /= Passthrough,
                    "Matching tangents cannot bypass a curvature discontinuity in the bounded-crackle family");
               else
                  T.Assert (Policy (Block.Corner_Transitions (2)) = Hard_Stop,
                    "A curvature discontinuity requires a stop when no geometric deviation is permitted");
               end if;
            end loop;
         end loop;
      end loop;
   end Test_Small_Arc_Junction_Continuity;

   procedure Test_Curved_Window_Consistency (T : in out Trendy_Test.Operation'Class) is
      Params : Kinematic_Parameters := (others => <>);
      Eval : Profile_Window_Evaluation;
      Solution : Window_Profile_Result;
      type Values is array (Positive range <>) of Dimensionless;
      Start, Finish : Velocity;
   begin
      T.Register;
      Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
      Params.Axial_Acceleration_Maxes := [others => 5000.0 * mm / s ** 2];
      Params.Axial_Jerk_Maxes := [others => 500000.0 * mm / s ** 3];
      Params.Axial_Snap_Maxes := [others => 500000000.0 * mm / s ** 4];
      Params.Axial_Crackle_Maxes := [others => 500000000000.0 * mm / s ** 5];
      for Radius of Values'[0.01, 0.25, 1.0, 2.0, 100.0] loop
         Eval := (Valid => True, Curved => True, others => <>);
         Eval.Bounds :=
           (Velocity => [X_Axis | Y_Axis => 1.0, others => 0.0],
            Acceleration => [X_Axis | Y_Axis => 1.0 / (Radius * mm), others => 0.0 / mm],
            Jerk => [X_Axis | Y_Axis => 1.0 / (Radius * mm) ** 2, others => 0.0 / mm ** 2],
            Snap => [X_Axis | Y_Axis => 1.0 / (Radius * mm) ** 3, others => 0.0 / mm ** 3],
            Crackle => [X_Axis | Y_Axis => 1.0 / (Radius * mm) ** 4, others => 0.0 / mm ** 4]);
         Eval.Limits := Mixed_Derivative_Limits
           (Params, (Velocity => Eval.Bounds.Velocity, others => <>), 250.0 * mm / s).Limits;
         Eval.Max_Vel := Constant_Speed_Axial_Ceiling (Params, Eval.Bounds, 250.0 * mm / s);
         for Span of Values'[0.000001, 0.001, 0.1, 1.0, 3.0] loop
            Eval.Window.Distance := Span * Radius * mm;
            for Fraction of Values'[0.0, 0.1, 0.9, 0.999] loop
               Start := Fraction * Eval.Max_Vel;
               Finish := Reachable_Window_Velocity (Params, Eval, Start);
               for Reverse_Direction in Boolean loop
                  Solution := Generate_Window_Profile
                    (Params, Eval, (if Reverse_Direction then Finish else Start),
                     (if Reverse_Direction then Start else Finish));
                  T.Assert (Solution.Valid, "Lookahead must be realizable in either direction");
                  Assert_Window_Profile
                    (Params, Eval.Bounds, Solution.Profile, Solution.Max_Crackle,
                     (if Reverse_Direction then Finish else Start), (if Reverse_Direction then Start else Finish),
                     Eval.Window.Distance, T);
               end loop;
            end loop;
         end loop;
      end loop;
   end Test_Curved_Window_Consistency;

   procedure Test_Small_Arc_Chain (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Result : Profile_Planning_Result;
      type Seed_Type is mod 2 ** 32;
      Seed : Seed_Type := 1234567;
      function Random return Dimensionless is
      begin
         Seed := 1664525 * Seed + 1013904223;
         return Dimensionless (Seed) / Dimensionless (Seed_Type'Last);
      end Random;
      Phase : Dimensionless;
      Radius, Scale : Dimensionless;
   begin
      T.Register;
      for Trial in 1 .. 1000 loop
         Reset_Early_Limiter_Block (Block);
         Radius := 0.25 + 1.75 * Random;
         Scale := (if Trial mod 2 = 0 then 0.0001 else 1.0);
         Block.Params.Ignore_E_In_XYZE := True;
         Block.Params.Tangential_Velocity_Max := 250.0 * Scale * mm / s;
         Block.Params.Axial_Velocity_Maxes := [others => 250.0 * Scale * mm / s];
         Block.Params.Axial_Acceleration_Maxes := [others => 5000.0 * Scale ** 2 * mm / s ** 2];
         Block.Params.Axial_Jerk_Maxes := [others => 500000.0 * Scale ** 3 * mm / s ** 3];
         Block.Params.Axial_Snap_Maxes := [others => 500000000.0 * Scale ** 4 * mm / s ** 4];
         Block.Params.Axial_Crackle_Maxes := [others => 500000000000.0 * Scale ** 5 * mm / s ** 5];
         Phase := 0.0;
         for I in Block.Corners'Range loop
            Block.Corners (I) :=
              [X_Axis => Radius * Dimensionless (Cos (Long_Float (Phase))) * mm,
               Y_Axis => Radius * Dimensionless (Sin (Long_Float (Phase))) * mm, others => 0.0 * mm];
            Phase := Phase + 0.001 + 1.0 * Random ** 3;
            if I > Block.Corners'First then
               Block.Primitives (I) := Make_Helix_Primitive
                 (Block.Corners (I - 1), Block.Corners (I), [others => 0.0 * mm], False);
               Block.Original_Segment_Feedrates (I) := (1.0 + 249.0 * Random) * Scale * mm / s;
            end if;
         end loop;
         Normalize_And_Blend (Block, Motor_Map, Workspace);
         Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
         Tested_Kinematic_Limiter.Run (Block, Motor_Map, Workspace);
         Tested_Profile_Generator.Run (Block, Motor_Map, Result);
         T.Assert (Result.Valid, "Chain generation realizes lookahead" & Trial'Image);
      end loop;
   end Test_Small_Arc_Chain;

   procedure Test_Split_Profile_Endpoint_Continuity (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Start_Speed, End_Speed : Velocity;
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Ignore_E_In_XYZE := True;
      Block.Params.Tangential_Velocity_Max := 500.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 500.0 * mm / s];
      Block.Params.Axial_Acceleration_Maxes :=
        [X_Axis => 2250.0 * mm / s ** 2, Y_Axis => 150.0 * mm / s ** 2, others => 5000.0 * mm / s ** 2];
      Block.Params.Axial_Jerk_Maxes :=
        [X_Axis => 36000.0 * mm / s ** 3, Y_Axis => 1800.0 * mm / s ** 3, others => 500000.0 * mm / s ** 3];
      Block.Params.Axial_Snap_Maxes :=
        [X_Axis => 1200000.0 * mm / s ** 4, Y_Axis => 10000000.0 * mm / s ** 4,
         others => 500000000.0 * mm / s ** 4];
      Block.Params.Axial_Crackle_Maxes :=
        [X_Axis => 18000000000.0 * mm / s ** 5, Y_Axis => 35000000000000.0 * mm / s ** 5,
         others => 500000000000.0 * mm / s ** 5];
      Block.Params.Cornering := (Kind => Biarc, others => <>);
      Block.Params.Cornering.Biarc_Params.Axial_Deviation_Maxes := [others => 0.1 * mm];
      Block.Params.Cornering.Biarc_Params.Corner_Miss_Distance_Max := 0.1 * mm;
      Block.Original_Segment_Feedrates := [300.0 * mm / s, 250.0 * mm / s, 20.0 * mm / s];
      Block.Corners :=
        [[X_Axis => -10.0 * mm, others => 0.0 * mm],
         [others => 0.0 * mm],
         [X_Axis => 0.75 * mm, others => 0.0 * mm],
         [X_Axis => -4.5 * mm, Y_Axis => 8.5 * mm, others => 0.0 * mm]];
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
      Plan_Kinematics (Block, Motor_Map, Workspace);
      for I in Block.Primitives'Range loop
         Start_Speed := Block.Original_Segment_Feedrates (I)
           * Segment_Vel_Ratio_At_Time (Block'Access, I, 0.0 * s);
         T.Assert (abs (Start_Speed - Block.Corner_Velocity_Limits (I - 1)) < 1.0E-7 * mm / s,
           "An absent prefix cannot change the lookahead junction speed" & I'Image
           & Start_Speed'Image & Block.Corner_Velocity_Limits (I - 1)'Image);
         End_Speed := Block.Original_Segment_Feedrates (I)
           * Segment_Vel_Ratio_At_Time (Block'Access, I, Segment_Time (Block'Access, I));
         T.Assert (abs (End_Speed - Block.Corner_Velocity_Limits (I)) < 1.0E-7 * mm / s,
           "An absent suffix cannot change the lookahead junction speed");
      end loop;
   end Test_Split_Profile_Endpoint_Continuity;

   procedure Test_Split_Profile_Time_Roundoff (T : in out Trendy_Test.Operation'Class) is
      Block : aliased Execution_Block (4);
      Workspace : aliased Planning_Workspace (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Duration : Time;
      type Sample_Times is array (Positive range <>) of Time;
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Original_Segment_Feedrates := [others => 1.0 * mm / s];
      Block.Corner_Velocity_Limits := [others => 1.0 * mm / s];
      Block.Corners :=
        [[X_Axis => -1.0 * mm, others => 0.0 * mm], [others => 0.0 * mm],
         [X_Axis => 0.6 * mm, others => 0.0 * mm], [X_Axis => 1.6 * mm, others => 0.0 * mm]];
      Block.Primitive_Distances := [1.0 * mm, 0.6 * mm, 1.0 * mm];
      Block.Profile_Windows (3) := (0.1 * mm, 0.2 * mm);
      Block.Feedrate_Profiles (3) := (Coast => 0.2 * s, others => [others => 0.0 * s]);
      Block.Profile_Ends (3) :=
        (Enabled => True,
         Prefix =>
           ((Coast => 0.1 * s, others => [others => 0.0 * s]), 0.0 * mm / s ** 5, 0.0 * mm / s ** 5, 1.0 * mm / s),
         Suffix =>
           ((Coast => 0.3 * s, others => [others => 0.0 * s]), 0.0 * mm / s ** 5, 0.0 * mm / s ** 5, 1.0 * mm / s));
      Duration := Segment_Time (Block'Access, 3);
      --  The segment is valid throughout; subtracting separately rounded prefix/central times must not
      --  send a local query past the suffix's endpoint (0.1 + 0.2 + 0.3 does not round back to 0.3).
      for Boundary of Sample_Times'[0.1 * s, 0.1 * s + 0.2 * s, Duration] loop
         for Elapsed of Sample_Times'
           [Time'Adjacent (Boundary, 0.0 * s), Boundary,
            Time'Min (Duration, Time'Adjacent (Boundary, Time'Last))]
         loop
            T.Assert (abs (Segment_Vel_Ratio_At_Time (Block'Access, 3, Elapsed) - 1.0) < 1.0E-12,
              "Valid split-profile boundary queries preserve velocity");
            T.Assert (abs (Segment_Pos_At_Time (Block'Access, 3, Elapsed) (X_Axis) - Elapsed * mm / s)
              < 1.0E-12 * mm, "Valid split-profile boundary queries preserve position");
         end loop;
      end loop;
   end Test_Split_Profile_Time_Roundoff;

   procedure Test_Stopped_Duration_Selection (T : in out Trendy_Test.Operation'Class) is
      Workspace : aliased Planning_Workspace (3);
      Reference_Workspace : aliased Planning_Workspace (3);
      Block : aliased Execution_Block (3);
      Stopped : aliased Execution_Block (3);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      type Deviations is array (1 .. 2) of Length;
      Blended_Time, Stopped_Time, Selected_Time : Time;
   begin
      T.Register;
      for Deviation of Deviations'[0.02 * mm, 1.0 * mm] loop
         for Mode in 0 .. 5 loop
            Reset_Early_Limiter_Block (Block);
            Block.Params.Ignore_E_In_XYZE := Mode = 0;
            Block.Params.Tangential_Velocity_Max := 250.0 * mm / s;
            Block.Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
            Block.Params.Axial_Acceleration_Maxes := [others => 5000.0 * mm / s ** 2];
            Block.Params.Axial_Jerk_Maxes := [others => 500000.0 * mm / s ** 3];
            Block.Params.Axial_Snap_Maxes := [others => 500000000.0 * mm / s ** 4];
            Block.Params.Axial_Crackle_Maxes := [others => 500000000000.0 * mm / s ** 5];
            Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => Deviation];
            Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := Deviation;
            Block.Original_Segment_Feedrates := [others => 250.0 * mm / s];
            Block.Corners :=
              [[others => 0.0 * mm],
               [X_Axis => 10.0 * mm, others => 0.0 * mm],
               [X_Axis | Y_Axis => 10.0 * mm, others => 0.0 * mm]];
            if Mode in 1 .. 4 then
               Block.Corners (2) (E_Axis) := (if Mode = 3 then -0.4 else 0.4) * mm;
               Block.Corners (3) (E_Axis) :=
                 (case Mode is when 2 => 1.2, when 3 => -0.8, when others => 0.8) * mm;
               if Mode = 4 then
                  Block.Params.Extrusion_Rounding_Tolerance := 0.0001 * mm;
                  Block.Params.Bounds.Upper_E := 0.8 * mm;
                  Block.Corners (2) (E_Axis) := @ + 0.00001 * mm;
               end if;
            elsif Mode = 5 then
               --  A line into a quarter-circle with a tangent discontinuity; stopped comparison must retain
               --  the commanded helix, including its original centre and untrimmed distance.
               Block.Corners (3) := [X_Axis => 0.0 * mm, Y_Axis => 10.0 * mm, others => 0.0 * mm];
               Block.Primitives (3) :=
                 Make_Helix_Primitive
                   (Block.Corners (2), Block.Corners (3), [others => 0.0 * mm], Clockwise => False);
            end if;
            Stopped := Block;
            Normalize_And_Blend (Block, Motor_Map, Workspace'Access);
            Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
            Plan_Fixed_Geometry (Block, Motor_Map, Workspace'Access);
            Blended_Time := Segment_Time (Block'Access, 2) + Segment_Time (Block'Access, 3);
            Tested_Density_Normalizer.Run (Stopped, Reference_Workspace'Access);
            Tested_Corner_Blender.Run (Stopped, Motor_Map, Reference_Workspace'Access, Force_Stops => True);
            Tested_Early_Kinematic_Limiter.Run (Stopped, Motor_Map);
            Plan_Fixed_Geometry (Stopped, Motor_Map, Reference_Workspace'Access);
            Stopped_Time := Segment_Time (Stopped'Access, 2) + Segment_Time (Stopped'Access, 3);
            declare
               Curves : constant Block_Corner_Transitions := Block.Corner_Transitions;
               References : constant Block_Segment_Lengths := Block.Extrusion_Reference_Positions;
               Densities : constant Block_Extrusion_Densities := Block.Extrusion_Densities;
               Feedrates : constant Block_Segment_Feedrates := Block.Original_Segment_Feedrates;
            begin
               Plan_Kinematics (Block, Motor_Map, Workspace'Access);
               Selected_Time := Segment_Time (Block'Access, 2) + Segment_Time (Block'Access, 3);
               T.Assert (abs (Selected_Time - Time'Min (Blended_Time, Stopped_Time)) < 1.0E-10 * s,
                 "Selection matches the faster independently planned complete trajectory");
               T.Assert (Block.Extrusion_Densities = Densities and then Block.Original_Segment_Feedrates = Feedrates,
                 "Comparing paths preserves normalized densities and normalizes feedrates only once");
               if Workspace.Local_Stops_Selected > 0 then
                  T.Assert (Workspace.Local_Comparison_Corners = 1,
                    "The local pair is certified before committing");
                  T.Assert ((for all C of Block.Corner_Transitions => Policy (C) = Hard_Stop),
                    "A winning stopped candidate restores the original corners");
                  T.Assert (Block.Extrusion_Reference_Positions = Stopped.Extrusion_Reference_Positions
                    and then Block.Primitive_Distances = Stopped.Primitive_Distances
                    and then Block.Primitive_Start_Distances = Stopped.Primitive_Start_Distances,
                    "Stopped selection restores the untrimmed spatial path and its extrusion reference");
                  T.Assert ((for all E of Workspace.Window_Evaluations => (for all W of E => not W.Valid)),
                    "Rejected geometry leaves no cached windows");
               else
                  T.Assert (Block.Corner_Transitions = Curves
                    and then Block.Extrusion_Reference_Positions = References,
                    "Rejected alternatives preserve the selected geometry and extrusion reference");
               end if;
               if Mode = 0 then
                  T.Assert ((Workspace.Local_Stops_Selected > 0) = (Deviation = 0.02 * mm),
                    "Tight corners select stops while profitable wide blends are retained");
                  if Workspace.Local_Stops_Selected > 0 then
                     Plan_Kinematics (Block, Motor_Map, Workspace'Access);
                     T.Assert (Workspace.Local_Comparison_Corners = 0,
                       "Already stopped geometry bypasses candidate planning");
                  end if;
               end if;
            end;
         end loop;
      end loop;
      --  The profitable passthrough case must also bypass stopped comparison.
      Block.Corners (3) := [X_Axis => 20.0 * mm, others => 0.0 * mm];
      Block.Primitives := [others => Make_Line_Primitive];
      Normalize_And_Blend (Block, Motor_Map, Workspace'Access);
      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
      Plan_Kinematics (Block, Motor_Map, Workspace'Access);
      T.Assert (Workspace.Local_Comparison_Corners = 0 and then Block.Corner_Velocity_Limits (2) > 0.0 * mm / s,
        "Passthrough-only paths bypass comparison and retain their through speed");
   end Test_Stopped_Duration_Selection;

   procedure Test_Local_Corner_Selection (T : in out Trendy_Test.Operation'Class) is
      Workspace, Reference_Workspace, Unbounded_Workspace : aliased Planning_Workspace (7);
      Block, Stopped, Unbounded : aliased Execution_Block (7);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Blended_Time, Stopped_Time, Selected_Time : Time;
   begin
      T.Register;
      for Mode in 0 .. 4 loop
         Reset_Early_Limiter_Block (Block);
         Block.Params.Ignore_E_In_XYZE := Mode = 0;
         Block.Params.Tangential_Velocity_Max := 250.0 * mm / s;
         Block.Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
         Block.Params.Axial_Acceleration_Maxes := [others => 5000.0 * mm / s ** 2];
         Block.Params.Axial_Jerk_Maxes := [others => 500000.0 * mm / s ** 3];
         Block.Params.Axial_Snap_Maxes := [others => 500000000.0 * mm / s ** 4];
         Block.Params.Axial_Crackle_Maxes := [others => 500000000000.0 * mm / s ** 5];
         Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => 0.02 * mm];
         Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 0.02 * mm;
         Block.Original_Segment_Feedrates := [others => 250.0 * mm / s];
         Block.Corners :=
           [[others => 0.0 * mm],
            [X_Axis => 10.0 * mm, others => 0.0 * mm],
            [X_Axis => 20.0 * mm, Y_Axis => 0.2 * mm, others => 0.0 * mm],
            [X_Axis => 30.0 * mm, Y_Axis => 0.6 * mm, others => 0.0 * mm],
            [X_Axis => 30.0 * mm, Y_Axis => 10.6 * mm, others => 0.0 * mm],
            [X_Axis => 40.0 * mm, Y_Axis => 10.8 * mm, others => 0.0 * mm],
            [X_Axis => 50.0 * mm, Y_Axis => 10.8 * mm, others => 0.0 * mm]];
         if Mode /= 0 then
            for I in Block.Primitives'Range loop
               Block.Corners (I) (E_Axis) := Block.Corners (I - 1) (E_Axis)
                 + Primitive_Length (Block'Access, I)
                 * (if Mode = 3 then -0.04 elsif Mode = 2 and then I mod 2 = 0 then 0.02 else 0.04);
            end loop;
         end if;
         if Mode = 4 then
            --  Beneficial gentle printing blends precede tight retraction corners and an exact return to E=0.
            --  Untrimming a retraction can move the untouched tail below its lower bound.
            Block.Params.Bounds.Lower_E := 0.0 * mm;
            for I in Corners_Index range 5 .. 7 loop
               Block.Corners (I) (E_Axis) := Block.Corners (4) (E_Axis) * Dimensionless (7 - I) / 3.0;
            end loop;
         end if;
         Stopped := Block;
         Normalize_And_Blend (Block, Motor_Map, Workspace'Access);
         Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
         Plan_Fixed_Geometry (Block, Motor_Map, Workspace'Access);
         Tested_Density_Normalizer.Run (Stopped, Reference_Workspace'Access);
         Tested_Corner_Blender.Run (Stopped, Motor_Map, Reference_Workspace'Access, Force_Stops => True);
         Tested_Early_Kinematic_Limiter.Run (Stopped, Motor_Map);
         Plan_Fixed_Geometry (Stopped, Motor_Map, Reference_Workspace'Access);
         Blended_Time := 0.0 * s;
         Stopped_Time := 0.0 * s;
         for I in Block.Primitives'Range loop
            Blended_Time := @ + Segment_Time (Block'Access, I);
            Stopped_Time := @ + Segment_Time (Stopped'Access, I);
         end loop;
         T.Assert (Mode = 4 or else Stopped_Time > Blended_Time,
           "Mixed fixture defeats whole-block selection: all stops are slower, mode" & Mode'Image);
         declare
            Transitions : constant Block_Corner_Transitions := Block.Corner_Transitions;
            Speeds : constant Block_Corner_Velocity_Limits := Block.Corner_Velocity_Limits;
            Densities : constant Block_Extrusion_Densities := Block.Extrusion_Densities;
            Feedrates : constant Block_Segment_Feedrates := Block.Original_Segment_Feedrates;
            Retained, Removed : Natural := 0;
         begin
            if Mode = 4 then
               Unbounded := Block;
               Unbounded_Workspace := Workspace;
               Unbounded.Params.Bounds.Lower_E := -1000.0 * mm;
               Plan_Kinematics (Unbounded, Motor_Map, Unbounded_Workspace'Access);
            end if;
            Plan_Kinematics (Block, Motor_Map, Workspace'Access);
            if Mode = 4 then
               T.Assert (Workspace.Local_Comparison_Corners > 0
                 and then Workspace.Local_Stops_Selected < Unbounded_Workspace.Local_Stops_Selected,
                 "The downstream E bound rejects otherwise profitable local removals");
            end if;
            Selected_Time := 0.0 * s;
            for I in Block.Primitives'Range loop
               Selected_Time := @ + Segment_Time (Block'Access, I);
               if Mode = 4 then
                  for Sample in 0 .. 16 loop
                     T.Assert (Segment_Pos_At_Time
                       (Block'Access, I, Segment_Time (Block'Access, I) * Dimensionless (Sample) / 16.0) (E_Axis)
                       >= -1.0E-12 * mm, "Local E shifts respect the later retraction bound");
                  end loop;
               end if;
               if I < Block.N_Corners then
                  T.Assert (abs (Segment_Pos_At_Time (Block'Access, I, Segment_Time (Block'Access, I))
                    - Segment_Pos_At_Time (Block'Access, I + 1, 0.0 * s)) < 1.0E-8 * mm,
                    "Local geometry and E references join continuously, mode" & Mode'Image);
               end if;
            end loop;
            for I in 2 .. Block.N_Corners - 1 loop
               if Transition_Kind (Block.Corner_Transitions (I)) = Stereographic_Transition then
                  Retained := @ + 1;
                  T.Assert (Block.Corner_Transitions (I) = Transitions (I)
                    and then Block.Corner_Velocity_Limits (I) = Speeds (I),
                    "Retained neighboring blends keep their exact geometry and junction speed");
               elsif Transition_Kind (Transitions (I)) = Stereographic_Transition then
                  Removed := @ + 1;
                  T.Assert (Policy (Block.Corner_Transitions (I)) = Hard_Stop
                    and then Block.Corner_Velocity_Limits (I) = 0.0 * mm / s,
                    "Removed blends stop at the commanded corner");
               end if;
            end loop;
            T.Assert ((Mode = 4 or else (Removed > 0 and then Retained > 0))
              and then Removed = Workspace.Local_Stops_Selected,
              "A single mixed block keeps useful blends and removes costly blends, mode" & Mode'Image);
            T.Assert (Selected_Time <= Blended_Time
              and then (Mode = 4 or else Selected_Time < Blended_Time - 1.0E-4 * s),
              "Local selection improves a block that whole-block comparison would leave unchanged, mode" & Mode'Image);
            T.Assert (Block.Extrusion_Densities = Densities and then Block.Original_Segment_Feedrates = Feedrates,
              "Local trials preserve authoritative densities and normalized feedrates");
         end;
      end loop;
   end Test_Local_Corner_Selection;

   procedure Test_Local_Pair_Stop_Selection (T : in out Trendy_Test.Operation'Class) is
      Workspace : aliased Planning_Workspace (4);
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Blended_Time, Selected_Time : Time;
   begin
      T.Register;
      for Mode in 0 .. 1 loop
         --  Two right-angle infill turns joined by a short connector. The connector is too short to reach either
         --  blend's speed from rest, so neither corner can stop alone; stopping at both is faster.
         Reset_Early_Limiter_Block (Block);
         Block.Params.Ignore_E_In_XYZE := Mode = 0;
         Block.Params.Tangential_Velocity_Max := 250.0 * mm / s;
         Block.Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
         Block.Params.Axial_Acceleration_Maxes := [others => 5000.0 * mm / s ** 2];
         Block.Params.Axial_Jerk_Maxes := [others => 500000.0 * mm / s ** 3];
         Block.Params.Axial_Snap_Maxes := [others => 500000000.0 * mm / s ** 4];
         Block.Params.Axial_Crackle_Maxes := [others => 500000000000.0 * mm / s ** 5];
         Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => 0.1 * mm];
         Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 0.1 * mm;
         Block.Original_Segment_Feedrates := [others => 250.0 * mm / s];
         Block.Corners :=
           [[others => 0.0 * mm],
            [X_Axis => 10.0 * mm, others => 0.0 * mm],
            [X_Axis => 10.0 * mm, Y_Axis => 0.45 * mm, others => 0.0 * mm],
            [X_Axis => 0.0 * mm, Y_Axis => 0.45 * mm, others => 0.0 * mm]];
         if Mode = 1 then
            for I in Block.Primitives'Range loop
               Block.Corners (I) (E_Axis) := Block.Corners (I - 1) (E_Axis) + Primitive_Length (Block'Access, I) * 0.04;
            end loop;
         end if;
         Normalize_And_Blend (Block, Motor_Map, Workspace'Access);
         Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
         Plan_Fixed_Geometry (Block, Motor_Map, Workspace'Access);
         T.Assert ((for all I in Corners_Index range 2 .. 3 =>
                      Transition_Kind (Block.Corner_Transitions (I)) = Stereographic_Transition),
           "Both turns start as blends, mode" & Mode'Image);
         Blended_Time := 0.0 * s;
         for I in Block.Primitives'Range loop
            Blended_Time := @ + Segment_Time (Block'Access, I);
         end loop;
         Plan_Kinematics (Block, Motor_Map, Workspace'Access);
         Selected_Time := 0.0 * s;
         for I in Block.Primitives'Range loop
            Selected_Time := @ + Segment_Time (Block'Access, I);
            if I < Block.N_Corners then
               T.Assert (abs (Segment_Pos_At_Time (Block'Access, I, Segment_Time (Block'Access, I))
                 - Segment_Pos_At_Time (Block'Access, I + 1, 0.0 * s)) < 1.0E-8 * mm,
                 "Pair geometry and E references join continuously, mode" & Mode'Image);
            end if;
         end loop;
         T.Assert ((for all I in Corners_Index range 2 .. 3 =>
                      Policy (Block.Corner_Transitions (I)) = Hard_Stop
                      and then Block.Corner_Velocity_Limits (I) = 0.0 * mm / s)
           and then Workspace.Local_Stops_Selected = 2,
           "Both turns of the pair stop, mode" & Mode'Image);
         T.Assert (Selected_Time < Blended_Time, "The pair is faster than both blends, mode" & Mode'Image);
         T.Assert (Segment_Pos_At_Time (Block'Access, Block.N_Corners, Segment_Time (Block'Access, Block.N_Corners))
           (E_Axis) - Block.Corners (Block.N_Corners) (E_Axis) in -1.0E-9 * mm .. 1.0E-9 * mm,
           "Stopping at both turns restores the commanded final E, mode" & Mode'Image);
      end loop;
   end Test_Local_Pair_Stop_Selection;

   procedure Test_Preprocessor_Run_After_Reset (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         Block : aliased Execution_Block;
         Extra_Data : Corner_Extra_Data_Vectors.Vector;
         Reset_Called : Boolean;
      begin
         Reset_Preprocessor.Setup ((others => <>));
         Reset_Preprocessor.Reset;
         --  Reproduce reset winning the protected lock after the planner has already acquired its slot.
         Reset_Preprocessor.Run (Block'Access, Extra_Data, Initial_Position, Reset_Called);
         T.Assert (Reset_Called, "A late Run observes reset rather than killing the planner");

         Reset_Preprocessor.Setup ((others => <>));
         Reset_Preprocessor.Enqueue
           ((Kind => Flush_Kind, Flush_Resetting_Data => Flush_Resetting_Data_Type_Default));
         Reset_Preprocessor.Run (Block'Access, Extra_Data, Initial_Position, Reset_Called);
         T.Assert (not Reset_Called and then Block.N_Corners = 1, "Preprocessing works after the next setup");
         Reset_Preprocessor.Reset;
      end;
   end Test_Preprocessor_Run_After_Reset;

   procedure Test_Profile_Extrusion_Limits (T : in out Trendy_Test.Operation'Class) is
      type Workspace_Access is access Planning_Workspace;
      Workspace : constant Workspace_Access := new Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (4);
      Motor_Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Result : Profile_Planning_Result;
      I : constant Finishing_Corners_Index := 3;
   begin
      T.Register;
      Reset_Early_Limiter_Block (Block);
      Block.Params.Ignore_E_In_XYZE := True;
      Block.Params.Tangential_Velocity_Max := 250.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
      Block.Params.Axial_Acceleration_Maxes := [E_Axis => 5.0 * mm / s ** 2, others => 5000.0 * mm / s ** 2];
      Block.Params.Axial_Jerk_Maxes := [E_Axis => 500.0 * mm / s ** 3, others => 500000.0 * mm / s ** 3];
      Block.Params.Axial_Snap_Maxes := [E_Axis => 500000.0 * mm / s ** 4, others => 500000000.0 * mm / s ** 4];
      Block.Params.Axial_Crackle_Maxes :=
        [E_Axis => 500000000.0 * mm / s ** 5, others => 500000000000.0 * mm / s ** 5];
      Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => 0.02 * mm];
      Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 0.02 * mm;
      Block.Original_Segment_Feedrates := [others => 250.0 * mm / s];
      --  Three 0.2 mm legs, with densities 0.2, 0.04 and 0.2, and E limits far below the XYZ limits. E follows each
      --  density exactly, so the window's derivative bounds alone must keep the curved profile within the E limits.
      Block.Corners :=
        [[others => 0.0 * mm],
         [X_Axis => 0.2 * mm, E_Axis => 0.2 * 0.2 * mm, others => 0.0 * mm],
         [X_Axis | Y_Axis => 0.2 * mm, E_Axis => (0.2 + 0.04) * 0.2 * mm, others => 0.0 * mm],
         [Y_Axis => 0.2 * mm, E_Axis => (2.0 * 0.2 + 0.04) * 0.2 * mm, others => 0.0 * mm]];
      Normalize_And_Blend (Block, Motor_Map, Workspace);
      Tested_Early_Kinematic_Limiter.Run (Block, Motor_Map);
      Tested_Kinematic_Limiter.Run (Block, Motor_Map, Workspace);
      T.Assert (Block.Corner_Velocity_Limits (I - 1) > 0.0 * mm / s
        and then Block.Corner_Velocity_Limits (I) > 0.0 * mm / s,
        "Both ends of the curved window move through E velocity jumps");
      declare
         Curves : constant Block_Corner_Transitions := Block.Corner_Transitions;
         Speeds : constant Block_Corner_Velocity_Limits := Block.Corner_Velocity_Limits;
         Feedrates : constant Block_Segment_Feedrates := Block.Limited_Segment_Feedrates;
      begin
         Tested_Profile_Generator.Select_Feedrate_Profile_Window
           (Block'Access, Motor_Map, I, Result, Limit_Scale => 1.0);
         T.Assert (Result.Valid, "The curved window has a certified profile");
         T.Assert (Block.Corner_Transitions = Curves and then Block.Corner_Velocity_Limits = Speeds
           and then Block.Limited_Segment_Feedrates = Feedrates,
           "Profile selection preserves geometry, endpoint velocities and feedrates");
         Assert_Window_Profile
           (Block.Params, Window_Axial_Derivative_Bounds (Block'Access, I, Block.Profile_Windows (I)),
            Block.Feedrate_Profiles (I), Block.Profile_Crackles (I),
            Central_Profile_Start_Velocity (Block'Access, I),
            (if Block.Profile_Ends (I).Enabled
             then Block.Profile_Ends (I).Suffix.Boundary_Velocity else Block.Corner_Velocity_Limits (I)),
            Block.Profile_Windows (I).Distance, T);
      end;
   end Test_Profile_Extrusion_Limits;

   procedure Test_Inter_Block_Blending (T : in out Trendy_Test.Operation'Class) is
      Left, Right : aliased Execution_Block (6);
      Work : aliased Planning_Workspace (Corners_Index'Last);
      Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Blended : Boolean;
      Old_Left_Profile : Feedrate_Profile;
      Before : Time;
      Following_Density : Dimensionless := 0.1;

      procedure Prepare (Block : aliased out Execution_Block; Following : Boolean; Turn : Boolean);
      procedure Prepare (Block : aliased out Execution_Block; Following : Boolean; Turn : Boolean) is
      begin
         Reset_Early_Limiter_Block (Block);
         Block.Blendable_End := True;
         Block.Params.Tangential_Velocity_Max := 100.0 * mm / s;
         Block.Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
         Block.Params.Axial_Acceleration_Maxes := [others => 1_000.0 * mm / s ** 2];
         Block.Params.Axial_Jerk_Maxes := [others => 10_000.0 * mm / s ** 3];
         Block.Params.Axial_Snap_Maxes := [others => 100_000.0 * mm / s ** 4];
         Block.Params.Axial_Crackle_Maxes := [others => 1_000_000.0 * mm / s ** 5];
         Block.Params.Cornering :=
           (Kind => Circular, Circular_Params =>
              (Axial_Deviation_Maxes => [others => 0.5 * mm], Corner_Miss_Distance_Max => 0.5 * mm, others => <>));
         Block.Original_Segment_Feedrates := [others => 100.0 * mm / s];
         for I in Block.Corners'Range loop
            Block.Corners (I) :=
              [X_Axis => (if Following and then Turn then Dimensionless (Block.N_Corners - 1) else
                 Dimensionless (I - 1) + (if Following then Dimensionless (Block.N_Corners - 1) else 0.0)) * mm,
               Y_Axis => (if Following and then Turn then Dimensionless (I - 1) else 0.0) * mm,
               E_Axis => (if Following then
                 Dimensionless (Block.N_Corners - 1) * 0.1 * mm + Dimensionless (I - 1) * Following_Density * mm
                 else Dimensionless (I - 1) * 0.1 * mm),
               others => 0.0 * mm];
         end loop;
         Normalize_And_Blend (Block, Map, Work'Access);
         Tested_Early_Kinematic_Limiter.Run (Block, Map);
         Plan_Fixed_Geometry (Block, Map, Work'Access);
      end Prepare;
   begin
      T.Register;
      for Turn in Boolean loop
         Prepare (Left, False, Turn);
         Prepare (Right, True, Turn);
         Before := 0.0 * s;
         for I in Left.Primitives'Range loop
            Before := @ + Segment_Time (Left'Access, I) + Segment_Time (Right'Access, I);
         end loop;
         Old_Left_Profile := Left.Feedrate_Profiles (4);
         Blend_Blocks (Left, Right, Map, Blended);
         T.Assert (Blended, "A prepared straight or rounded seam should remove the block stop");
         T.Assert (Left.Corner_Velocity_Limits (6) > 0.0 * mm / s, "The seam has nonzero speed");
         T.Assert (Left.Corner_Velocity_Limits (6) = Right.Corner_Velocity_Limits (1), "Seam speed is continuous");
         T.Assert (abs (Segment_Pos_At_Time (Left'Access, 6, Segment_Time (Left'Access, 6))
           - Segment_Pos_At_Time (Right'Access, 2, 0.0 * s)) < 1.0E-8 * mm,
           "XYZ and shortened extrusion are continuous across the seam");
         if not Turn then
            T.Assert (Left.Feedrate_Profiles (4) /= Old_Left_Profile,
              "Removing the slowdown changes profiles more than one segment away");
         end if;
         for I in Left.Primitives'Range loop
            Before := @ - Segment_Time (Left'Access, I) - Segment_Time (Right'Access, I);
         end loop;
         T.Assert (Before > 0.0 * s, "The local replacement is faster than the two stopped plans");
      end loop;
      Following_Density := 0.2;
      Prepare (Left, False, False);
      Prepare (Right, True, False);
      Blend_Blocks (Left, Right, Map, Blended);
      T.Assert (Blended and then Left.Corner_Velocity_Limits (6) > 0.0 * mm / s,
        "A density change keeps moving across the block boundary");
      T.Assert (abs (Right.Extrusion_Densities (2) - Left.Extrusion_Densities (6)) * Left.Corner_Velocity_Limits (6)
        <= Left.Params.Extrusion_Velocity_Change_Max,
        "The seam's E velocity jump obeys its allowance");
      T.Assert (abs (Segment_Pos_At_Time (Left'Access, 6, Segment_Time (Left'Access, 6))
        - Segment_Pos_At_Time (Right'Access, 2, 0.0 * s)) < 1.0E-8 * mm,
        "Both sides evaluate the same boundary extrusion position");
      Following_Density := 0.1;
      declare
         Long_Left, Long_Right : aliased Execution_Block (Corners_Index'Last);
         Prefix, Suffix : Feedrate_Profile;
      begin
         Prepare (Long_Left, False, False);
         Prepare (Long_Right, True, False);
         Prefix := Long_Left.Feedrate_Profiles (2);
         Suffix := Long_Right.Feedrate_Profiles (Corners_Index'Last);
         Blend_Blocks (Long_Left, Long_Right, Map, Blended);
         T.Assert (Blended, "The bounded window also blends blocks larger than that window");
         T.Assert (Long_Left.Feedrate_Profiles (2) = Prefix
           and then Long_Right.Feedrate_Profiles (Corners_Index'Last) = Suffix,
           "Profiles outside the bounded window are retained exactly");
      end;
      declare
         E_Left, E_Right : aliased Execution_Block (2);
      begin
         Reset_Early_Limiter_Block (E_Left);
         Reset_Early_Limiter_Block (E_Right);
         E_Left.Params := Left.Params;
         E_Right.Params := Left.Params;
         E_Left.Blendable_End := True;
         E_Left.Corners (2) (E_Axis) := 1.0 * mm;
         E_Right.Corners (1) (E_Axis) := 1.0 * mm;
         E_Right.Corners (2) (E_Axis) := 2.0 * mm;
         E_Left.Original_Segment_Feedrates := [others => 10.0 * mm / s];
         E_Right.Original_Segment_Feedrates := [others => 10.0 * mm / s];
         Normalize_And_Blend (E_Left, Map, Work'Access);
         Tested_Early_Kinematic_Limiter.Run (E_Left, Map);
         Plan_Fixed_Geometry (E_Left, Map, Work'Access);
         Normalize_And_Blend (E_Right, Map, Work'Access);
         Tested_Early_Kinematic_Limiter.Run (E_Right, Map);
         Plan_Fixed_Geometry (E_Right, Map, Work'Access);
         Blend_Blocks (E_Left, E_Right, Map, Blended);
         T.Assert (Blended and then E_Left.Corner_Velocity_Limits (2) > 0.0 * mm / s,
           "Same-direction extrusion-only blocks have a continuous scalar profile");
      end;
      declare
         --  The rounded seam shortens both sides, translating the following references down. The following block
         --  then retracts to its commanded lower limit entirely inside the replacement window, with no suffix.
         Retract_Right : aliased Execution_Block (3);
         Old_End_E     : Length;
      begin
         for Bounded in Boolean loop
            Prepare (Left, False, False);
            if Bounded then
               Left.Params.Bounds.Lower_E := -1.0E-6 * mm;
            end if;
            Reset_Early_Limiter_Block (Retract_Right);
            Retract_Right.Params := Left.Params;
            Retract_Right.Original_Segment_Feedrates := [others => 100.0 * mm / s];
            Retract_Right.Corners :=
              [Left.Corners (Left.N_Corners),
               [X_Axis => 5.0 * mm, Y_Axis => 5.0 * mm, E_Axis => 1.0 * mm, others => 0.0 * mm],
               [X_Axis => 5.0 * mm, Y_Axis => 10.0 * mm, others => 0.0 * mm]];
            Normalize_And_Blend (Retract_Right, Map, Work'Access);
            Tested_Early_Kinematic_Limiter.Run (Retract_Right, Map);
            Plan_Fixed_Geometry (Retract_Right, Map, Work'Access);
            Old_End_E := Retract_Right.Extrusion_Reference_Positions (3);
            Blend_Blocks (Left, Retract_Right, Map, Blended);
            if Bounded then
               T.Assert (not Blended and then Retract_Right.Extrusion_Reference_Positions (3) = Old_End_E,
                 "A window translation below the E bound retains the stopped plans");
            else
               T.Assert (Blended and then Retract_Right.Extrusion_Reference_Positions (3) < -1.0E-6 * mm,
                 "Without the bound the same seam translates the retraction below it");
            end if;
         end loop;
      end;
      for Barrier in 1 .. 4 loop
         Prepare (Left, False, False);
         Prepare (Right, True, False);
         T.Assert (Blendable_Boundary (Left, Right), "The prepared pair is structurally blendable");
         case Barrier is
            when 1 => Left.Blendable_End := False;
            when 2 => Left.Associated_Overflow_Block := True;
            when 3 => Left.Corner_Dwell_Times (6) := 1.0 * s;
            when 4 => Right.Is_Homing_Move := True;
         end case;
         T.Assert (not Blendable_Boundary (Left, Right),
           "End data, overflow, dwell and homing boundaries are rejected before any window is copied");
      end loop;
   end Test_Inter_Block_Blending;

   procedure Prepare_Continuous_Line
     (Block      : aliased out Execution_Block;
      Work       : not null access Planning_Workspace;
      Continuous : Boolean;
      Slow_From  : Corners_Index'Base := Corners_Index'Base'Last;
      Origin     : Position := [others => 0.0 * mm]);
   --  Plan a 15 mm extruding line along X from Origin, split evenly across Block's segments. Segments from Slow_From on
   --  have half the feedrate. The slow derivative limits make each short segment's own ramp much slower than one long
   --  ramp.

   procedure Prepare_Continuous_Line
     (Block      : aliased out Execution_Block;
      Work       : not null access Planning_Workspace;
      Continuous : Boolean;
      Slow_From  : Corners_Index'Base := Corners_Index'Base'Last;
      Origin     : Position := [others => 0.0 * mm])
   is
      Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      Reset_Early_Limiter_Block (Block);
      Block.Params.Tangential_Velocity_Max := 100.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 100.0 * mm / s];
      Block.Params.Axial_Acceleration_Maxes := [others => 1_000.0 * mm / s ** 2];
      Block.Params.Axial_Jerk_Maxes := [others => 10_000.0 * mm / s ** 3];
      Block.Params.Axial_Snap_Maxes := [others => 100_000.0 * mm / s ** 4];
      Block.Params.Axial_Crackle_Maxes := [others => 1_000_000.0 * mm / s ** 5];
      Block.Params.Continuous_Timing := Continuous;
      Block.Original_Segment_Feedrates :=
        [for I in Block.Original_Segment_Feedrates'Range => (if I >= Slow_From then 50.0 else 100.0) * mm / s];
      for I in Block.Corners'Range loop
         Block.Corners (I) :=
           [Origin with delta
              X_Axis => Origin (X_Axis) + 15.0 * mm * Dimensionless (I - 1) / Dimensionless (Block.N_Corners - 1),
              E_Axis => Origin (E_Axis) + 0.75 * mm * Dimensionless (I - 1) / Dimensionless (Block.N_Corners - 1)];
      end loop;
      Normalize_And_Blend (Block, Map, Work);
      Tested_Early_Kinematic_Limiter.Run (Block, Map);
      Plan_Kinematics (Block, Map, Work);
   end Prepare_Continuous_Line;

   function Planned_Time (Block : aliased Execution_Block) return Time;

   function Planned_Time (Block : aliased Execution_Block) return Time is
      Total : Time := 0.0 * s;
   begin
      for I in Block.Primitives'Range loop
         Total := Total + Segment_Time (Block'Access, I);
      end loop;
      return Total;
   end Planned_Time;

   procedure Test_Continuous_Passthrough_Timing (T : in out Trendy_Test.Operation'Class) is
      Work                      : aliased Planning_Workspace (Corners_Index'Last);
      One                       : aliased Execution_Block (2);
      Split, Continuous, Halved : aliased Execution_Block (Corners_Index'Last);
      Last                      : constant Corners_Index := Corners_Index'Last;
   begin
      T.Register;
      Prepare_Continuous_Line (One, Work'Access, Continuous => False);
      Prepare_Continuous_Line (Split, Work'Access, Continuous => False);
      Prepare_Continuous_Line (Continuous, Work'Access, Continuous => True);

      T.Assert
        ((for all I in Split.Primitives'Range => Split.Timing_Run_Back (I) = 0 and then Split.Timing_Run_Ahead (I) = 0),
         "Without the switch every segment is its own timing run");
      T.Assert
        ((for all I in Continuous.Primitives'Range =>
            Timing_Run_First (Continuous'Access, I) = 2 and then Timing_Run_Last (Continuous'Access, I) = Last),
         "With the switch the collinear segments form one timing run");

      T.Assert
        (Planned_Time (Continuous) < 0.75 * Planned_Time (Split), "One run is much faster than per-segment ramps");
      T.Assert
        (abs (Planned_Time (Continuous) - Planned_Time (One)) <= 1.0E-9 * Planned_Time (One),
         "One run takes as long as one move of the same length");
      T.Assert
        (abs (Planned_Time (Continuous) - Timing_Motion_Time (Continuous'Access, 2)) <= 1.0E-12 * s,
         "Segment durations add up to the run's motion time");

      for I in Continuous.Primitives'First .. Continuous.Primitives'Last - 1 loop
         declare
            End_Time  : constant Time := Segment_Time (Continuous'Access, I);
            End_Pos   : constant Position := Segment_Pos_At_Time (Continuous'Access, I, End_Time);
            Start_Pos : constant Position := Segment_Pos_At_Time (Continuous'Access, I + 1, 0.0 * s);
         begin
            T.Assert (End_Time > 0.0 * s, "Every segment of a run has a positive duration");
            T.Assert
              (abs (End_Pos - Start_Pos) < 1.0E-9 * mm and then abs (End_Pos - Continuous.Corners (I)) < 1.0E-9 * mm,
               "Adjacent segments meet at their shared corner, including E");
            T.Assert
              (abs
                 (Segment_Vel_Ratio_At_Time (Continuous'Access, I, End_Time)
                  - Segment_Vel_Ratio_At_Time (Continuous'Access, I + 1, 0.0 * s))
               < 1.0E-9,
               "Speed is continuous across interior corners");
            T.Assert (Continuous.Corner_Velocity_Limits (I) > 0.0 * mm / s, "Interior corners are passed while moving");
         end;
      end loop;

      declare
         --  Corner 3 is 2 mm in, while the run is still speeding up. A junction would have zero acceleration there.
         At_Corner : constant Time := Continuous.Timing_Time_Offsets (4);
         Step      : constant Time := 0.000_1 * s;
         Before    : constant Velocity := Timing_Velocity_At_Time (Continuous'Access, 2, At_Corner - Step);
         After     : constant Velocity := Timing_Velocity_At_Time (Continuous'Access, 2, At_Corner + Step);
      begin
         T.Assert ((After - Before) / (2.0 * Step) > 10.0 * mm / s ** 2, "The run keeps accelerating through a corner");
      end;

      declare
         --  Step through the block as Block_Executor does, carrying the remainder into each following segment.
         Current  : Time := 0.0 * s;
         Previous : Length := 0.0 * mm;
         Monotone : Boolean := True;
         Final    : Position := [others => 0.0 * mm];
      begin
         for I in Continuous.Primitives'Range loop
            while Current <= Segment_Time (Continuous'Access, I) loop
               Final := Segment_Pos_At_Time (Continuous'Access, I, Current);
               Monotone := Monotone and then Final (X_Axis) >= Previous - 1.0E-12 * mm;
               Previous := Final (X_Axis);
               Current := Current + 0.001 * s;
            end loop;
            Current := Current - Segment_Time (Continuous'Access, I);
         end loop;
         Final := Segment_Pos_At_Time (Continuous'Access, Last, Segment_Time (Continuous'Access, Last));
         T.Assert (Monotone, "Sampled positions never move backwards across segment boundaries");
         T.Assert
           (Final = [Continuous.Corners (Last) with delta E_Axis => Continuous.Extrusion_Reference_Positions (Last)],
            "The run stops exactly at its commanded end");
      end;

      Prepare_Continuous_Line (Halved, Work'Access, Continuous => True, Slow_From => 9);
      T.Assert
        (Timing_Run_Last (Halved'Access, 2) = 8
         and then Timing_Run_First (Halved'Access, Last) = 9
         and then Is_Timing_Junction (Halved, 8)
         and then not Is_Timing_Junction (Halved, 7),
         "A feedrate change ends a run and stays a junction");
   end Test_Continuous_Passthrough_Timing;

   procedure Test_Continuous_Timing_Blend_Seams (T : in out Trendy_Test.Operation'Class) is
      Work                  : aliased Planning_Workspace (Corners_Index'Last);
      Map                   : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
      Left, Right           : aliased Execution_Block (6);
      Long_Left, Long_Right : aliased Execution_Block (Corners_Index'Last);
      Blended               : Boolean;
      Before                : Time;
   begin
      T.Register;
      Prepare_Continuous_Line (Left, Work'Access, Continuous => True);
      Left.Blendable_End := True;
      Prepare_Continuous_Line (Right, Work'Access, Continuous => True, Origin => Left.Corners (Left.N_Corners));
      T.Assert (Blendable_Boundary (Left, Right), "Window ends at run boundaries allow blending");
      Before := Planned_Time (Left) + Planned_Time (Right);

      Blend_Blocks (Left, Right, Map, Blended);
      T.Assert (Blended and then Left.Corner_Velocity_Limits (6) > 0.0 * mm / s, "A straight seam keeps moving");
      T.Assert (Planned_Time (Left) + Planned_Time (Right) < Before, "The blended seam is faster");
      T.Assert
        ((for all I in Left.Primitives'Range =>
            Timing_Run_First (Left'Access, I) = 2 and then Timing_Run_Last (Left'Access, I) = 6)
         and then
           (for all I in Right.Primitives'Range =>
              Timing_Run_First (Right'Access, I) = 2 and then Timing_Run_Last (Right'Access, I) = 6),
         "The seam stays a junction, so each block keeps its own runs");
      T.Assert
        (abs (Segment_Pos_At_Time (Left'Access, 6, Segment_Time (Left'Access, 6))
           - Segment_Pos_At_Time (Right'Access, 2, 0.0 * s)) < 1.0E-8 * mm,
         "Position is continuous across the blended seam");

      Prepare_Continuous_Line (Long_Left, Work'Access, Continuous => True);
      Long_Left.Blendable_End := True;
      Prepare_Continuous_Line
        (Long_Right, Work'Access, Continuous => True, Origin => Long_Left.Corners (Long_Left.N_Corners));
      T.Assert
        (not Blendable_Boundary (Long_Left, Long_Right),
         "A run across the fixed splice corner keeps the stopped seam instead of cutting the run");
   end Test_Continuous_Timing_Blend_Seams;

   procedure Prepare_Gentle_Corners
     (Block : aliased out Execution_Block; Work : not null access Planning_Workspace; Continuous : Boolean);
   --  Plan an extruding path under the Benchy limits: a right angle at corner 4 and gentle stereographic corners
   --  elsewhere. Corners 2 and 5 follow short moves from a stop, so they are passed while still speeding up.

   procedure Prepare_Gentle_Corners
     (Block : aliased out Execution_Block; Work : not null access Planning_Workspace; Continuous : Boolean)
   is
      Map : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      Reset_Early_Limiter_Block (Block);
      Block.Params.Tangential_Velocity_Max := 250.0 * mm / s;
      Block.Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
      Block.Params.Axial_Acceleration_Maxes := [others => 5_000.0 * mm / s ** 2];
      Block.Params.Axial_Jerk_Maxes := [others => 500_000.0 * mm / s ** 3];
      Block.Params.Axial_Snap_Maxes := [others => 500_000_000.0 * mm / s ** 4];
      Block.Params.Axial_Crackle_Maxes := [others => 500_000_000_000.0 * mm / s ** 5];
      Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => 0.02 * mm];
      Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 0.02 * mm;
      Block.Params.Continuous_Timing := Continuous;
      Block.Original_Segment_Feedrates := [others => 250.0 * mm / s];
      Block.Corners :=
        [[others => 0.0 * mm],
         [X_Axis => 3.0 * mm, others => 0.0 * mm],
         [X_Axis => 13.0 * mm, Y_Axis => 0.2 * mm, others => 0.0 * mm],
         [X_Axis => 23.0 * mm, Y_Axis => 0.6 * mm, others => 0.0 * mm],
         [X_Axis => 23.0 * mm, Y_Axis => 3.6 * mm, others => 0.0 * mm],
         [X_Axis => 22.9 * mm, Y_Axis => 13.6 * mm, others => 0.0 * mm],
         [X_Axis => 22.5 * mm, Y_Axis => 23.6 * mm, others => 0.0 * mm]];
      for I in Block.Primitives'Range loop
         Block.Corners (I) (E_Axis) := Block.Corners (I - 1) (E_Axis) + 0.04 * Primitive_Length (Block'Access, I);
      end loop;
      Normalize_And_Blend (Block, Map, Work);
      Tested_Early_Kinematic_Limiter.Run (Block, Map);
      Plan_Kinematics (Block, Map, Work);
   end Prepare_Gentle_Corners;

   procedure Check_Sampled_Axial_Limits
     (Block : aliased Execution_Block; Name : String; T : in out Trendy_Test.Operation'Class);
   --  Sample the block as the executor does and bound each axis's velocity, acceleration, jerk and snap by central
   --  differences. This checks the certified plan without using any planner bound. The tolerances cover the
   --  differences' truncation error at crackle steps.

   procedure Check_Sampled_Axial_Limits
     (Block : aliased Execution_Block; Name : String; T : in out Trendy_Test.Operation'Class)
   is
      Step    : constant Time := 0.000_05 * s;
      Count   : constant Positive := Positive (Planned_Time (Block) / Step) + 1;
      type Samples is array (1 .. Count) of Position;
      Points  : Samples;
      Current : Time := 0.0 * s;
      Segment : Finishing_Corners_Index := Block.Primitives'First;
   begin
      for K in Points'Range loop
         while Current > Segment_Time (Block'Access, Segment) and then Segment < Block.N_Corners loop
            Current := Current - Segment_Time (Block'Access, Segment);
            Segment := Segment + 1;
         end loop;
         Points (K) :=
           Segment_Pos_At_Time (Block'Access, Segment, Time'Min (Current, Segment_Time (Block'Access, Segment)));
         Current := Current + Step;
      end loop;
      for Axis in Axis_Name loop
         declare
            V_Max : Velocity := 0.0 * mm / s;
            A_Max : Acceleration := 0.0 * mm / s ** 2;
            J_Max : Jerk := 0.0 * mm / s ** 3;
            S_Max : Snap := 0.0 * mm / s ** 4;
         begin
            for K in Points'First + 2 .. Points'Last - 2 loop
               declare
                  M2 : constant Length := Points (K - 2) (Axis);
                  M1 : constant Length := Points (K - 1) (Axis);
                  P0 : constant Length := Points (K) (Axis);
                  P1 : constant Length := Points (K + 1) (Axis);
                  P2 : constant Length := Points (K + 2) (Axis);
               begin
                  V_Max := Velocity'Max (V_Max, abs (P1 - M1) / (2.0 * Step));
                  A_Max := Acceleration'Max (A_Max, abs (P1 - 2.0 * P0 + M1) / Step ** 2);
                  J_Max := Jerk'Max (J_Max, abs (P2 - 2.0 * P1 + 2.0 * M1 - M2) / (2.0 * Step ** 3));
                  S_Max := Snap'Max (S_Max, abs (P2 - 4.0 * P1 + 6.0 * P0 - 4.0 * M1 + M2) / Step ** 4);
               end;
            end loop;
            T.Assert
              (V_Max <= 1.001 * Block.Params.Axial_Velocity_Maxes (Axis)
               and then A_Max <= 1.01 * Block.Params.Axial_Acceleration_Maxes (Axis)
               and then J_Max <= 1.05 * Block.Params.Axial_Jerk_Maxes (Axis)
               and then S_Max <= 1.2 * Block.Params.Axial_Snap_Maxes (Axis),
               Name & ": sampled " & Axis'Image & " derivatives stay within their limits");
         end;
      end loop;
   end Check_Sampled_Axial_Limits;

   procedure Test_Continuous_Curved_Merges (T : in out Trendy_Test.Operation'Class) is
      Work       : aliased Planning_Workspace (7);
      Off, On    : aliased Execution_Block (7);
      Merged     : Natural := 0;
   begin
      T.Register;
      Prepare_Gentle_Corners (Off, Work'Access, Continuous => False);
      Prepare_Gentle_Corners (On, Work'Access, Continuous => True);

      T.Assert
        ((for all I in Off.Primitives'Range => Off.Timing_Run_Back (I) = 0 and then Off.Timing_Run_Ahead (I) = 0),
         "Without the switch no corner is merged");
      for I in On.Corners'First + 1 .. On.Corners'Last - 1 loop
         if not Is_Timing_Junction (On, I) then
            Merged := Merged + 1;
            T.Assert
              (Transition_Kind (On.Corner_Transitions (I)) = Stereographic_Transition,
               "Merged corners are curved stereographic blends");
         end if;
      end loop;
      T.Assert (Merged > 0, "At least one gentle curved corner is merged");
      T.Assert (Is_Timing_Junction (On, 4), "The binding right angle stays a junction");
      T.Assert (Planned_Time (On) < Planned_Time (Off), "Merging is kept only where it is faster");

      for I in On.Corners'First + 1 .. On.Corners'Last - 1 loop
         if not Is_Timing_Junction (On, I) then
            declare
               End_Time : constant Time := Segment_Time (On'Access, I);
            begin
               T.Assert
                 (abs (Segment_Pos_At_Time (On'Access, I, End_Time) - Segment_Pos_At_Time (On'Access, I + 1, 0.0 * s))
                  < 1.0E-9 * mm,
                  "Position is continuous across a merged curved corner");
               T.Assert
                 (abs
                    (Segment_Vel_Ratio_At_Time (On'Access, I, End_Time)
                     - Segment_Vel_Ratio_At_Time (On'Access, I + 1, 0.0 * s))
                  < 1.0E-9,
                  "Speed is continuous across a merged curved corner");
            end;
         end if;
      end loop;

      Check_Sampled_Axial_Limits (Off, "Per-segment plan", T);
      Check_Sampled_Axial_Limits (On, "Merged plan", T);
   end Test_Continuous_Curved_Merges;

   procedure Test_Cellular_Certificate (T : in out Trendy_Test.Operation'Class) is
      Params  : Kinematic_Parameters;
      Radius  : constant Length := 5.0 * mm;
      Curved  : constant Unit_Speed_Axial_Derivative_Bounds :=
        (Velocity     => [X_Axis | Y_Axis => 1.0, others => 0.0],
         Acceleration => [X_Axis | Y_Axis => 1.0 / Radius, others => 0.0 / mm],
         Jerk         => [X_Axis | Y_Axis => 1.0 / Radius ** 2, others => 0.0 / mm ** 2],
         Snap         => [X_Axis | Y_Axis => 1.0 / Radius ** 3, others => 0.0 / mm ** 3],
         Crackle      => [X_Axis | Y_Axis => 1.0 / Radius ** 4, others => 0.0 / mm ** 4]);
      Ceiling : Velocity;
      Eval    : Profile_Window_Evaluation;
   begin
      T.Register;
      Params.Tangential_Velocity_Max := 250.0 * mm / s;
      Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
      Params.Axial_Acceleration_Maxes := [others => 5_000.0 * mm / s ** 2];
      Params.Axial_Jerk_Maxes := [others => 500_000.0 * mm / s ** 3];
      Params.Axial_Snap_Maxes := [others => 500_000_000.0 * mm / s ** 4];
      Params.Axial_Crackle_Maxes := [others => 500_000_000_000.0 * mm / s ** 5];
      Ceiling := Constant_Speed_Axial_Ceiling (Params, Curved, 250.0 * mm / s);
      Eval :=
        (Valid         => True,
         Window        => (Start_Distance => 0.0 * mm, Distance => 30.0 * mm),
         Limits        =>
           Mixed_Derivative_Limits (Params, (Velocity => Curved.Velocity, others => <>), 250.0 * mm / s).Limits,
         Max_Vel       => Ceiling,
         Start_Max_Vel => Ceiling,
         End_Max_Vel   => Ceiling,
         Bounds        => Curved,
         Curved        => True,
         Cellular      => True);

      declare
         --  A ramp across one constant-curvature cell: tangential acceleration must make room for curvature.
         Cells  : constant Path_Cells :=
           [(Start => 0.0 * mm, Stop => 30.0 * mm, Bounds => Curved, Ceiling => Ceiling, Curved => True)];
         Caches : Path_Cell_Caches (Cells'Range);
         Result : constant Window_Profile_Result :=
           Cellular_Profile_At_Peak (Params, Cells, Caches, Eval, 0.0 * mm / s, 0.0 * mm / s, 0.8 * Ceiling);
      begin
         T.Assert (Result.Valid, "A ramp across a curved cell converges to a certified stretch");
         T.Assert
           (Result.Max_Crackle < Eval.Limits.Crackle_Max and then Result.Decel_Crackle < Eval.Limits.Crackle_Max,
            "Both ramps are stretched below the tangent-only envelope");
         for Is_Accel in Boolean loop
            declare
               Ramp  : constant Feedrate_Profile_Times :=
                 (if Is_Accel then Result.Profile.Accel else Result.Profile.Decel);
               C     : constant Crackle := (if Is_Accel then Result.Max_Crackle else Result.Decel_Crackle);
               Peaks : constant Scalar_Derivative_Limits :=
                 (Acceleration_Max => C * Ramp (1) * (Ramp (1) + Ramp (2)) * (2.0 * Ramp (1) + Ramp (2) + Ramp (3)),
                  Jerk_Max         => C * Ramp (1) * (Ramp (1) + Ramp (2)),
                  Snap_Max         => C * Ramp (1),
                  Crackle_Max      => C);
            begin
               T.Assert
                 (Mixed_Derivative_Time_Scale (Params, Curved, 0.8 * Ceiling, Peaks) >= 0.999,
                  "Each stretched ramp passes the curved cell's certificate directly");
            end;
         end loop;
      end;

      declare
         --  The same window without curvature needs no stretch.
         Cells  : constant Path_Cells :=
           [(Start   => 0.0 * mm,
             Stop    => 30.0 * mm,
             Bounds  => (Velocity => Curved.Velocity, others => <>),
             Ceiling => 250.0 * mm / s,
             Curved  => False)];
         Caches : Path_Cell_Caches (Cells'Range);
         Result : constant Window_Profile_Result :=
           Cellular_Profile_At_Peak (Params, Cells, Caches, Eval, 0.0 * mm / s, 0.0 * mm / s, 0.8 * Ceiling);
      begin
         T.Assert
           (Result.Valid
            and then Result.Max_Crackle = Eval.Limits.Crackle_Max
            and then Result.Decel_Crackle = Eval.Limits.Crackle_Max,
            "Straight cells keep the tangent-only ramps");
      end;

      declare
         type Fractions is array (Positive range <>) of Dimensionless;
         type Times is array (Positive range <>) of Time;
         Ramp_Up   : constant Feedrate_Profile_Times := Optimal_Profile_For_Delta_V
           (50.0 * mm / s, 5_000.0 * mm / s ** 2, 500_000.0 * mm / s ** 3, 5.0E8 * mm / s ** 4, 5.0E11 * mm / s ** 5);
         Ramp_Down : constant Feedrate_Profile_Times := Optimal_Profile_For_Delta_V
           (30.0 * mm / s, 2_000.0 * mm / s ** 2, 200_000.0 * mm / s ** 3, 2.0E8 * mm / s ** 4, 2.0E11 * mm / s ** 5);
         Profile   : constant Feedrate_Profile := (Accel => Ramp_Up, Coast => 0.01 * s, Decel => Ramp_Down);
         Up        : constant Crackle := 5.0E11 * mm / s ** 5;
         Down      : constant Crackle := 2.0E11 * mm / s ** 5;
         Start     : constant Velocity := 10.0 * mm / s;
         Peak      : constant Velocity := Fast_Velocity_At_Max_Time (Ramp_Up, Up, Start);
         Total     : constant Time := Total_Time (Profile);
         Accel_End : constant Time := Total_Time (Ramp_Up);
         Epsilon   : constant Time := 1.0E-9 * s;
      begin
         for Fraction of Fractions'[0.0, 0.1, 0.3, 0.5, 0.7, 0.9, 1.0] loop
            T.Assert
              (Profile_Distance_At_Time (Profile, Fraction * Total, Up, Up, Start)
               = Distance_At_Time (Profile, Fraction * Total, Up, Start)
               and then Profile_Velocity_At_Time (Profile, Fraction * Total, Up, Up, Start)
                        = Velocity_At_Time (Profile, Fraction * Total, Up, Start),
               "Equal ramp crackles evaluate exactly as one crackle");
         end loop;
         T.Assert
           (abs (Profile_Distance_At_Time (Profile, Total, Up, Down, Start)
              - (Fast_Distance_At_Max_Time (Ramp_Up, Up, Start) + Peak * Profile.Coast
                 + Fast_Distance_At_Max_Time (Ramp_Down, -Down, Peak))) < 1.0E-12 * mm
            and then abs (Profile_Velocity_At_Time (Profile, Total, Up, Down, Start)
                          - Fast_Velocity_At_Max_Time (Ramp_Down, -Down, Peak)) < 1.0E-9 * mm / s,
            "Separate ramp crackles drive each ramp to its own end");
         for Join of Times'[Accel_End, Accel_End + Profile.Coast] loop
            T.Assert
              (abs (Profile_Velocity_At_Time (Profile, Join + Epsilon, Up, Down, Start)
                    - Profile_Velocity_At_Time (Profile, Join - Epsilon, Up, Down, Start)) < 1.0E-6 * mm / s,
               "Speed is continuous where the coast meets each ramp");
         end loop;
      end;
   end Test_Cellular_Certificate;

   procedure Test_Cellular_Endpoint_Ceilings (T : in out Trendy_Test.Operation'Class) is
      Work  : aliased Planning_Workspace (6);
      Block : aliased Execution_Block (6);
      Map   : constant Motor_Position_Map := [others => [others => 0.0 / mm]];

      procedure Prepare (Continuous : Boolean);
      --  A 10 degree corner at corner 2, a collinear passthrough at corner 3 and a 3 degree corner at corner 4, under
      --  the Benchy limits. Segments 3 and 4 form one timing run whose start crosses the sharper corner's transition
      --  and whose end crosses the gentler one's, so its end ceilings differ.

      procedure Prepare (Continuous : Boolean) is
      begin
         Reset_Early_Limiter_Block (Block);
         Block.Params.Tangential_Velocity_Max := 250.0 * mm / s;
         Block.Params.Axial_Velocity_Maxes := [others => 250.0 * mm / s];
         Block.Params.Axial_Acceleration_Maxes := [others => 5_000.0 * mm / s ** 2];
         Block.Params.Axial_Jerk_Maxes := [others => 500_000.0 * mm / s ** 3];
         Block.Params.Axial_Snap_Maxes := [others => 500_000_000.0 * mm / s ** 4];
         Block.Params.Axial_Crackle_Maxes := [others => 500_000_000_000.0 * mm / s ** 5];
         Block.Params.Cornering.Stereographic_Params.Axial_Deviation_Maxes := [others => 0.02 * mm];
         Block.Params.Cornering.Stereographic_Params.Corner_Miss_Distance_Max := 0.02 * mm;
         Block.Params.Continuous_Timing := Continuous;
         Block.Original_Segment_Feedrates := [others => 250.0 * mm / s];
         --  Corners 2, 3 and 4 are exactly collinear in binary: doubling 0.35 is exact.
         Block.Corners :=
           [[others => 0.0 * mm],
            [X_Axis => 10.0 * mm, others => 0.0 * mm],
            [X_Axis => 12.0 * mm, Y_Axis => 0.35 * mm, others => 0.0 * mm],
            [X_Axis => 14.0 * mm, Y_Axis => 0.7 * mm, others => 0.0 * mm],
            [X_Axis => 24.0 * mm, Y_Axis => 3.0 * mm, others => 0.0 * mm],
            [X_Axis => 34.0 * mm, Y_Axis => 5.85 * mm, others => 0.0 * mm]];
         for I in Block.Primitives'Range loop
            Block.Corners (I) (E_Axis) := Block.Corners (I - 1) (E_Axis) + 0.04 * Primitive_Length (Block'Access, I);
         end loop;
         Normalize_And_Blend (Block, Map, Work'Access);
         Tested_Early_Kinematic_Limiter.Run (Block, Map);
      end Prepare;
   begin
      T.Register;
      Prepare (Continuous => True);
      Work.Timing_Join_Vetoes (Block.Corners'Range) := [others => False];
      Tested_Kinematic_Limiter.Run (Block, Map, Work'Access);
      T.Assert
        (Timing_Run_First (Block'Access, 3) = 3 and then Timing_Run_Last (Block'Access, 3) = 4,
         "Segments 3 and 4 form one timing run");

      declare
         Feedrate : constant Velocity := Timing_Feedrate (Block'Access, 3);
         Cells    : constant Path_Cells := Timing_Run_Cells (Block'Access, Map, 3, Feedrate);
         Eval     : constant Profile_Window_Evaluation :=
           Evaluate_Timing_Window
             (Block'Access, Map, 3, (Start_Distance => 0.0 * mm, Distance => Timing_Total_Distance (Block'Access, 3)),
              Feedrate);
         Fixed    : constant Velocity := 0.5 * Eval.Start_Max_Vel;
         Reached  : constant Velocity := Reachable_Cellular_Velocity (Block.Params, Cells, Eval, Fixed, True);
         Back     : constant Velocity :=
           Reachable_Cellular_Velocity (Block.Params, Cells, Eval, Eval.End_Max_Vel, False);
         Caches   : Path_Cell_Caches (Cells'Range);
      begin
         T.Assert (Eval.Valid and then Eval.Cellular, "The whole-run window is cellular");
         T.Assert
           (Eval.Start_Max_Vel = Cells (Cells'First).Ceiling and then Eval.End_Max_Vel = Cells (Cells'Last).Ceiling,
            "Each endpoint ceiling comes from the cell at that end");
         T.Assert
           (Eval.Start_Max_Vel < Eval.End_Max_Vel and then Eval.Max_Vel = Eval.Start_Max_Vel,
            "The sharp corner's ceiling caps only the start, and Max_Vel is the lower ceiling");
         T.Assert
           (Reached > Eval.Start_Max_Vel and then Reached <= Eval.End_Max_Vel,
            "The reached end may exceed the start's ceiling but not its own");
         T.Assert
           (Cellular_Profile_At_Peak (Block.Params, Cells, Caches, Eval, Fixed, Reached, Reached).Valid,
            "The reached end speed is certified");
         T.Assert (Back <= Eval.Start_Max_Vel, "Reaching back from a fast end never exceeds the start's ceiling");
      end;

      --  The limiter and generator must agree on every window, so the block plans without splitting the run.
      Prepare (Continuous => True);
      Plan_Kinematics (Block, Map, Work'Access);
      T.Assert
        ((for all C in Block.Corners'Range => not Work.Timing_Join_Vetoes (C)), "No timing run had to be split");
      T.Assert (not Is_Timing_Junction (Block, 3), "The passthrough stays inside its run");
      Check_Sampled_Axial_Limits (Block, "Asymmetric run", T);
   end Test_Cellular_Endpoint_Ceilings;

   procedure Test_Timing_Join_Vetoes (T : in out Trendy_Test.Operation'Class) is
      Work  : aliased Planning_Workspace (Corners_Index'Last);
      Block : aliased Execution_Block (Corners_Index'Last);
      Map   : constant Motor_Position_Map := [others => [others => 0.0 / mm]];
   begin
      T.Register;
      Prepare_Continuous_Line (Block, Work'Access, Continuous => True);
      T.Assert (not Is_Timing_Junction (Block, 5), "The collinear line is one run");
      Work.Timing_Join_Vetoes (Block.Corners'Range) := [5 => True, others => False];
      Tested_Kinematic_Limiter.Run (Block, Map, Work'Access);
      T.Assert
        (Is_Timing_Junction (Block, 5) and then not Is_Timing_Junction (Block, 4)
         and then not Is_Timing_Junction (Block, 6),
         "A vetoed corner is a junction and only it splits the run");
   end Test_Timing_Join_Vetoes;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Continuous_Passthrough_Timing'Unrestricted_Access,
         Test_Continuous_Timing_Blend_Seams'Unrestricted_Access,
         Test_Continuous_Curved_Merges'Unrestricted_Access,
         Test_Cellular_Certificate'Unrestricted_Access,
         Test_Timing_Join_Vetoes'Unrestricted_Access,
         Test_Cellular_Endpoint_Ceilings'Unrestricted_Access,
         Test_Inter_Block_Blending'Unrestricted_Access,
         Test_Preprocessor_Run_After_Reset'Unrestricted_Access,
         Test_Profile_Extrusion_Limits'Unrestricted_Access,
         Test_Local_Pair_Stop_Selection'Unrestricted_Access,
         Test_Local_Corner_Selection'Unrestricted_Access,
         Test_Stopped_Duration_Selection'Unrestricted_Access,
         Test_Split_Profile_Endpoint_Continuity'Unrestricted_Access,
         Test_Split_Profile_Time_Roundoff'Unrestricted_Access,
         Test_Small_Arc_Chain'Unrestricted_Access,
         Test_Curved_Window_Consistency'Unrestricted_Access,
         Test_Small_Arc_Junction_Continuity'Unrestricted_Access,
         Test_Small_Arc_Headroom'Unrestricted_Access,
         Test_Small_Arc_Lookahead'Unrestricted_Access,
         Test_Extrusion_Density_Rounding'Unrestricted_Access,
         Test_Extrusion_Density_Rounding_Boundaries'Unrestricted_Access,
         Test_Extrusion_Density_Rounding_Bounds'Unrestricted_Access,
         Test_Extrusion_Density_Rounding_Intersection'Unrestricted_Access,
         Test_Extrusion_Density_Rounding_Execution'Unrestricted_Access,
         Test_Extrusion_Zero_Allowance_Geometry'Unrestricted_Access,
         Test_Extrusion_Instantaneous_Velocity_Change'Unrestricted_Access,
         Test_Profile_Local_Curve_Limits'Unrestricted_Access,
         Test_Profile_Recovery'Unrestricted_Access,
         Test_Fixed_Geometry_Region_Boundary'Unrestricted_Access,
         Test_Profile_Numeric_Recovery'Unrestricted_Access,
         Test_Extrusion_Workspace_Reuse'Unrestricted_Access,
         Test_Extrusion_Transition_Locality'Unrestricted_Access,
         Test_Extrusion_Position_Bounds'Unrestricted_Access,
         Test_Extrusion_Tangential_Limit'Unrestricted_Access,
         Test_Extrusion_Feedrate_Retry'Unrestricted_Access,
         Test_Extrusion_Corner_Repair'Unrestricted_Access,
         Test_Extrusion_Geometry_And_Compensation'Unrestricted_Access,
         Test_Extrusion_Stops_And_Dominated_Moves'Unrestricted_Access,
         Test_Early_Limiter_Helix_Ignore_E'Unrestricted_Access,
         Test_Early_Limiter_Uses_Executed_Distance'Unrestricted_Access,
         Test_Helix_Primitive_Tangent_Jet_Identities'Unrestricted_Access,
         Test_Tiny_Helix_And_Scaled_Derivatives'Unrestricted_Access,
         Test_Small_Sweep_Helix_Axial_Precision'Unrestricted_Access,
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
         Test_Stereographic_Curved_Corners'Unrestricted_Access,
         Test_Accepted_Corner_Survives_Growth'Unrestricted_Access,
         Test_Homing_Boundary_Uses_Resolved_Position'Unrestricted_Access,
         Test_Homing_Unavoidable_Tail_Includes_Complete_Tail'Unrestricted_Access];
   end All_Tests;

end Prunt.Motion_Planner.Planner.Test;
