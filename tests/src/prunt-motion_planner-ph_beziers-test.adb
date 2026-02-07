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

with Trendy_Test; use Trendy_Test;

package body Prunt.Motion_Planner.PH_Beziers.Test is

   pragma Extensions_Allowed (On);

   Arc_Length_Subdivisions      : constant Positive := 10_000;
   Curvature_Absolute_Tolerance : constant Curvature := 1.0E-9 / mm;
   Curvature_Relative_Tolerance : constant Dimensionless := 1.0E-3;
   Distance_Tolerance           : constant Length := 1.0E-4 * mm;
   Midpoint_Curvature_Delta     : constant Curve_Parameter := 1.0E-3;
   Monotonicity_Tolerance       : constant Length := 1.0E-12 * mm;
   Point_Tolerance              : constant Length := 1.0E-6 * mm;

   type Curve_Parameter_Samples is array (Positive range <>) of Curve_Parameter;
   type Distance_Fractions is array (Positive range <>) of Dimensionless;

   Sample_Parameters : constant Curve_Parameter_Samples := [for I in 1 .. 100 => Dimensionless (I) / 100.0];
   Sample_Fractions  : constant Distance_Fractions := [for I in 1 .. 100 => Dimensionless (I) / 100.0];

   procedure Assert_Curvature_Close
     (Actual : Curvature; Expected : Curvature; Name : String; T : in out Trendy_Test.Operation'Class);
   procedure Assert_Length_Close
     (Actual : Length; Expected : Length; Name : String; T : in out Trendy_Test.Operation'Class);
   procedure Assert_Point_Close
     (Actual : Scaled_Position; Expected : Scaled_Position; Name : String; T : in out Trendy_Test.Operation'Class);
   procedure Check_Nondegenerate_Case
     (Name            : String;
      Start           : Scaled_Position;
      Corner          : Scaled_Position;
      Finish          : Scaled_Position;
      Deviation_Limit : Length;
      T               : in out Trendy_Test.Operation'Class);
   procedure Check_Zero_Length_Bezier
     (Name            : String;
      Start           : Scaled_Position;
      Corner          : Scaled_Position;
      Finish          : Scaled_Position;
      Deviation_Limit : Length;
      T               : in out Trendy_Test.Operation'Class);
   function Make_Point (X, Y, Z, E : Length) return Scaled_Position;
   function Menger_Curvature (P1, P2, P3 : Scaled_Position) return Curvature;
   function Numerical_Arc_Length (Bez : PH_Bezier; Parameter : Curve_Parameter) return Length;

   procedure Assert_Curvature_Close
     (Actual : Curvature; Expected : Curvature; Name : String; T : in out Trendy_Test.Operation'Class)
   is
      Tolerance : constant Curvature :=
        Curvature'Max (Curvature_Absolute_Tolerance, abs Expected * Curvature_Relative_Tolerance);
   begin
      T.Assert
        (abs (Actual - Expected) <= Tolerance,
         Name & ": " & Actual'Image & " vs " & Expected'Image & " (tol " & Tolerance'Image & ")");
   end Assert_Curvature_Close;

   procedure Assert_Length_Close
     (Actual : Length; Expected : Length; Name : String; T : in out Trendy_Test.Operation'Class) is
   begin
      T.Assert (abs (Actual - Expected) <= Distance_Tolerance, Name & ": " & Actual'Image & " vs " & Expected'Image);
   end Assert_Length_Close;

   procedure Assert_Point_Close
     (Actual : Scaled_Position; Expected : Scaled_Position; Name : String; T : in out Trendy_Test.Operation'Class)
   is
      Point_Delta : constant Length := abs (Actual - Expected);
   begin
      T.Assert (Point_Delta <= Point_Tolerance, Name & ": delta " & Point_Delta'Image);
   end Assert_Point_Close;

   procedure Check_Nondegenerate_Case
     (Name            : String;
      Start           : Scaled_Position;
      Corner          : Scaled_Position;
      Finish          : Scaled_Position;
      Deviation_Limit : Length;
      T               : in out Trendy_Test.Operation'Class)
   is
      Bez : constant PH_Bezier := Create_Bezier (Start, Corner, Finish, Deviation_Limit);

      procedure Check_Curvature is
         Analytical : constant Curvature := 1.0 / Inverse_Curvature (Bez);
         Numerical  : constant Curvature :=
           Menger_Curvature
             (Point_At_T (Bez, 0.5 - Midpoint_Curvature_Delta),
              Point_At_T (Bez, 0.5),
              Point_At_T (Bez, 0.5 + Midpoint_Curvature_Delta));
      begin
         Assert_Curvature_Close (Analytical, Numerical, Name & ": midpoint curvature", T);
      end Check_Curvature;

      procedure Check_Distance_Inversion is
         Total_Distance : constant Length := Distance_At_T (Bez, 1.0);
      begin
         for Fraction of Sample_Fractions loop
            declare
               Target_Distance : constant Length := Total_Distance * Fraction;
               Parameter       : constant Curve_Parameter := T_At_Distance (Bez, Target_Distance);
            begin
               Assert_Length_Close
                 (Distance_At_T (Bez, Parameter),
                  Target_Distance,
                  Name & ": distance inversion at fraction " & Fraction'Image,
                  T);
               Assert_Point_Close
                 (Point_At_Distance (Bez, Target_Distance),
                  Point_At_T (Bez, Parameter),
                  Name & ": point at distance at fraction " & Fraction'Image,
                  T);
            end;
         end loop;
      end Check_Distance_Inversion;

      procedure Check_Distance_Monotonicity is
         Previous : Length := 0.0 * mm;
      begin
         for I in Sample_Parameters'Range loop
            declare
               Parameter           : constant Curve_Parameter := Sample_Parameters (I);
               Analytical_Distance : constant Length := Distance_At_T (Bez, Parameter);
               Numerical_Distance  : constant Length := Numerical_Arc_Length (Bez, Parameter);
               Sample_Name_Suffix  : constant String := " at t=" & Parameter'Image;
            begin
               Assert_Length_Close
                 (Analytical_Distance, Numerical_Distance, Name & ": arc length" & Sample_Name_Suffix, T);

               if I /= Sample_Parameters'First then
                  T.Assert
                    (Analytical_Distance + Monotonicity_Tolerance >= Previous,
                     Name
                     & ": non-monotone distance at t="
                     & Parameter'Image
                     & " ("
                     & Analytical_Distance'Image
                     & " after "
                     & Previous'Image
                     & ")");
               end if;

               Previous := Analytical_Distance;
            end;
         end loop;
      end Check_Distance_Monotonicity;

   begin
      Check_Curvature;
      Check_Distance_Inversion;
      Check_Distance_Monotonicity;
      Assert_Point_Close (Midpoint (Bez), Point_At_T (Bez, 0.5), Name & ": midpoint", T);
   end Check_Nondegenerate_Case;

   procedure Check_Zero_Length_Bezier
     (Name            : String;
      Start           : Scaled_Position;
      Corner          : Scaled_Position;
      Finish          : Scaled_Position;
      Deviation_Limit : Length;
      T               : in out Trendy_Test.Operation'Class)
   is
      Bez : constant PH_Bezier := Create_Bezier (Start, Corner, Finish, Deviation_Limit);
   begin
      T.Assert (Inverse_Curvature (Bez) = 0.0 * mm, Name & ": inverse curvature");

      for Parameter of Sample_Parameters loop
         Assert_Length_Close (Distance_At_T (Bez, Parameter), 0.0 * mm, Name & ": zero distance", T);
         Assert_Point_Close (Point_At_T (Bez, Parameter), Corner, Name & ": point at t", T);
      end loop;

      Assert_Point_Close (Midpoint (Bez), Corner, Name & ": midpoint", T);
      Assert_Point_Close (Point_At_Distance (Bez, 0.0 * mm), Corner, Name & ": point at zero distance", T);
   end Check_Zero_Length_Bezier;

   function Make_Point (X, Y, Z, E : Length) return Scaled_Position is
   begin
      return [X_Axis => X, Y_Axis => Y, Z_Axis => Z, E_Axis => E];
   end Make_Point;

   function Menger_Curvature (P1, P2, P3 : Scaled_Position) return Curvature is
      A : constant Length := abs (P2 - P1);
      B : constant Length := abs (P3 - P2);
      C : constant Length := abs (P3 - P1);
      S : constant Length := (A + B + C) / 2.0;

      Area_Squared : constant Hypervolume := S * (S - A) * (S - B) * (S - C);
   begin
      if A = 0.0 * mm or else B = 0.0 * mm or else C = 0.0 * mm or else Area_Squared <= 0.0 * mm ** 4 then
         return 0.0 / mm;
      else
         return 4.0 * (Area_Squared ** (1 / 2)) / (A * B * C);
      end if;
   end Menger_Curvature;

   function Numerical_Arc_Length (Bez : PH_Bezier; Parameter : Curve_Parameter) return Length is
      Previous_Point : Scaled_Position := Point_At_T (Bez, 0.0);
      Result         : Length := 0.0 * mm;
   begin
      if Parameter = 0.0 then
         return 0.0 * mm;
      end if;

      for I in 1 .. Arc_Length_Subdivisions loop
         declare
            Sample_Parameter : constant Curve_Parameter :=
              Parameter * Dimensionless (I) / Dimensionless (Arc_Length_Subdivisions);
            Current_Point    : constant Scaled_Position := Point_At_T (Bez, Sample_Parameter);
         begin
            Result := Result + abs (Current_Point - Previous_Point);
            Previous_Point := Current_Point;
         end;
      end loop;

      return Result;
   end Numerical_Arc_Length;

   procedure Test_Collinear_XYZE_Corner (T : in out Trendy_Test.Operation'Class) is
      Bez : constant PH_Bezier :=
        Create_Bezier
          (Make_Point (0.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
           Make_Point (10.0 * mm, 10.0 * mm, 5.0 * mm, 2.0 * mm),
           Make_Point (20.0 * mm, 20.0 * mm, 10.0 * mm, 4.0 * mm),
           1.0 * mm);
   begin
      T.Register;
      T.Assert (Inverse_Curvature (Bez) = Length'Last, "Collinear XYZE corner should report infinite radius");
   end Test_Collinear_XYZE_Corner;

   procedure Test_Planar_XY_Non_Right_Angle_Corner (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Nondegenerate_Case
        ("Planar XY non-right-angle corner",
         Make_Point (0.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
         Make_Point (10.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
         Make_Point (18.0 * mm, 6.0 * mm, 0.0 * mm, 0.0 * mm),
         1.0 * mm,
         T);
   end Test_Planar_XY_Non_Right_Angle_Corner;

   procedure Test_Planar_XY_Right_Angle_Corner (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Nondegenerate_Case
        ("Planar XY right-angle corner",
         Make_Point (0.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
         Make_Point (10.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
         Make_Point (10.0 * mm, 10.0 * mm, 0.0 * mm, 0.0 * mm),
         1.0 * mm,
         T);
   end Test_Planar_XY_Right_Angle_Corner;

   procedure Test_Spatial_XYZ_Corner (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Nondegenerate_Case
        ("Spatial XYZ corner",
         Make_Point (0.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
         Make_Point (10.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
         Make_Point (10.0 * mm, 8.0 * mm, 6.0 * mm, 0.0 * mm),
         1.0 * mm,
         T);
   end Test_Spatial_XYZ_Corner;

   procedure Test_XYZE_Corner (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Check_Nondegenerate_Case
        ("XYZE corner",
         Make_Point (0.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
         Make_Point (10.0 * mm, 0.0 * mm, 0.0 * mm, 2.0 * mm),
         Make_Point (10.0 * mm, 10.0 * mm, 5.0 * mm, 6.0 * mm),
         1.0 * mm,
         T);
   end Test_XYZE_Corner;

   procedure Test_Zero_Length_Corner (T : in out Trendy_Test.Operation'Class) is
      Corner : constant Scaled_Position := Make_Point (10.0 * mm, 0.0 * mm, 0.0 * mm, 2.0 * mm);
   begin
      T.Register;

      Check_Zero_Length_Bezier
        ("Zero-length start segment",
         Corner,
         Corner,
         Make_Point (15.0 * mm, 10.0 * mm, 5.0 * mm, 3.0 * mm),
         1.0 * mm,
         T);
      Check_Zero_Length_Bezier
        ("Zero-length finish segment",
         Make_Point (0.0 * mm, 0.0 * mm, 0.0 * mm, 0.0 * mm),
         Corner,
         Corner,
         1.0 * mm,
         T);
   end Test_Zero_Length_Corner;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Collinear_XYZE_Corner'Access,
         Test_Planar_XY_Non_Right_Angle_Corner'Access,
         Test_Planar_XY_Right_Angle_Corner'Access,
         Test_Spatial_XYZ_Corner'Access,
         Test_XYZE_Corner'Access,
         Test_Zero_Length_Corner'Access];
   end All_Tests;

end Prunt.Motion_Planner.PH_Beziers.Test;
