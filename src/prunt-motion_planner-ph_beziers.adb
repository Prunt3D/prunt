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

with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Unchecked_Conversion;

package body Prunt.Motion_Planner.PH_Beziers is

   pragma Extensions_Allowed (On);

   Collinear_Sine_Tolerance : constant Dimensionless := 1.0E-7;

   function Distance_At_T (Bez : PH_Bezier; T : Curve_Parameter) return Length is
      --  Note that this assumes symmetrical curves as it makes the computation significantly faster.
      --
      --  The details of this implementation are here:
      --  https://github.com/Prunt3D/prunt_notebooks/blob/master/Pythagorean-Hodograph%20Splines.ipynb
      L : constant Length := abs (Bez.Control_Points (0) - Bez.Control_Points (1));
      B : constant Length := abs (Bez.Control_Points (4) - Bez.Control_Points (5));
      Z : constant Dimensionless := 2.0 * T - 1.0;
      Y : constant Dimensionless := Z * Z;
      D : constant Length := (if L = 0.0 * mm then 0.0 * mm else (B ** 2 - L ** 2) / L);
      H : constant Dimensionless :=
        (((((((-102_245.0 / 544_768.0) * Y + 70_785.0 / 38_912.0) * Y - 630_201.0 / 77_824.0) * Y
            + 1_329_185.0 / 58_368.0)
           * Y
           - 3_374_085.0 / 77_824.0)
          * Y
          + 2_147_145.0 / 38_912.0)
         * Y
         - 3_578_575.0 / 77_824.0)
        * Y
        + 61_347.0 / 2_128.0;

      Total_Length : constant Length := 15.0 * L + (5_005.0 / 228.0) * D;
   begin
      return Total_Length / 2.0 + Z * (15.0 * L / 2.0 + D * H);
   end Distance_At_T;

   function T_At_Distance (Bez : PH_Bezier; Distance : Length) return Curve_Parameter is
      function Speed_At_T (Bez : PH_Bezier; T : Curve_Parameter) return Length is
         --  Exact derivative of `Distance_At_T`.

         L : constant Length := abs (Bez.Control_Points (0) - Bez.Control_Points (1));
         B : constant Length := abs (Bez.Control_Points (4) - Bez.Control_Points (5));
         D : constant Length := (if L = 0.0 * mm then 0.0 * mm else (B ** 2 - L ** 2) / L);
         X : constant Dimensionless := T * (1.0 - T);
         P : constant Dimensionless := (((400.0 * X + 140.0) * X + 56.0) * X + 35.0);
      begin
         return 15.0 * L + (61_347.0 / 266.0) * D * X ** 4 * P;
      end Speed_At_T;

      Target : Length := Distance;

      Result : Curve_Parameter;
      Lower  : Curve_Parameter := 0.0;
      Upper  : Curve_Parameter := 0.5;

      Total_Length : constant Length := Distance_At_T (Bez, 1.0);

      type Casted_Curve_Parameter is mod 2 ** 64;
      function Cast_Curve_Parameter is new Ada.Unchecked_Conversion (Curve_Parameter, Casted_Curve_Parameter);
      function Cast_Curve_Parameter is new Ada.Unchecked_Conversion (Casted_Curve_Parameter, Curve_Parameter);

      function Binary_Midpoint (Lower, Upper : Curve_Parameter) return Curve_Parameter is
      begin
         return
           Cast_Curve_Parameter
             (Cast_Curve_Parameter (Lower) + (Cast_Curve_Parameter (Upper) - Cast_Curve_Parameter (Lower)) / 2);
      end Binary_Midpoint;
   begin
      --  This probably breaks when not using IEEE 754 floats or on other weird systems, so try to check for
      --  that.
      pragma Assert (Curve_Parameter'Size = 64);
      pragma Assert (Casted_Curve_Parameter'Size = 64);
      pragma Assert (Cast_Curve_Parameter (0.123_45) = 4_593_559_930_647_147_132);

      pragma Assert (Target <= Total_Length);
      pragma Assert (Target >= 0.0 * mm);

      if Target <= 0.0 * mm then
         return 0.0;
      elsif Target >= Total_Length then
         return 1.0;
      end if;

      if Target > Total_Length / 2.0 then
         --  Avoid calling `T_At_Distance` again here as with certain values we might get stuck in a loop.
         Target := Total_Length - Target;
      end if;

      Result := Target / Total_Length;

      if Result < Lower then
         Result := Lower;
      elsif Result > Upper then
         Result := Upper;
      end if;

      for I in 1 .. 5 loop
         declare
            F      : constant Length := Distance_At_T (Bez, Result) - Target;
            Fp     : Length;
            Next_T : Curve_Parameter;
         begin
            if F <= 0.0 then
               Lower := Result;
            else
               Upper := Result;
            end if;

            Fp := Speed_At_T (Bez, Result);
            if Fp > 0.0 then
               Next_T := Result - F / Fp;
            else
               Next_T := Binary_Midpoint (Lower, Upper);
            end if;

            if Next_T <= Lower or else Next_T >= Upper then
               Next_T := Binary_Midpoint (Lower, Upper);
            end if;

            exit when Next_T = Result;

            Result := Next_T;
         end;
      end loop;

      loop
         Result := Binary_Midpoint (Lower, Upper);
         exit when Result = Lower or else Result = Upper;

         if Distance_At_T (Bez, Result) <= Target then
            Lower := Result;
         else
            Upper := Result;
         end if;
      end loop;

      if Distance > Total_Length / 2.0 then
         return 1.0 - Result;
      else
         return Result;
      end if;
   end T_At_Distance;

   function Inverse_Curvature (Bez : PH_Bezier) return Length is
   begin
      return Bez.Inverse_Curvature;
   end Inverse_Curvature;

   function Midpoint (Bez : PH_Bezier) return Scaled_Position is
      --  It is possible to compute the midpoint by multiplying the corner deviation by the unit bisector of the two
      --  vectors from the corner to the start/finish. The corner deviation may be computed by the following equation:
      --  Midpoint = (Sine_Secondary_Angle / 2.0**14) * Base_Length *
      --    ((397.0 / 429.0) + 10_207.0 + (2.0**14 * 1_225.0) / (858.8 * Cosine_Secondary_Angle))
      --
      --  This method may be used if a speed improvement is needed.
   begin
      return Point_At_T (Bez, 0.5);
   end Midpoint;

   function Point_At_T (Bez : PH_Bezier; T : Curve_Parameter) return Scaled_Position is
      --  Uses De Casteljau's algorithm.
      Bez_2 : PH_Control_Points := Bez.Control_Points;
   begin
      for J in reverse Bez_2'First .. Bez_2'Last - 1 loop
         for I in Bez_2'First .. J loop
            Bez_2 (I) := Bez_2 (I) + (Bez_2 (I + 1) - Bez_2 (I)) * T;
         end loop;
      end loop;

      return Bez_2 (Bez_2'First);
   end Point_At_T;

   --  function Tangent_At_T (Bez : PH_Bezier; T : Curve_Parameter) return Scaled_Position_Offset is
   --     --  Uses De Casteljau's algorithm and returns the vector between the two points at the second last iteration.
   --     Bez_2 : PH_Control_Points := Bez.Control_Points;
   --  begin
   --     for J in reverse Bez_2'First + 1 .. Bez_2'Last - 1 loop
   --        for I in Bez_2'First .. J loop
   --           Bez_2 (I) := Bez_2 (I) + (Bez_2 (I + 1) - Bez_2 (I)) * T;
   --        end loop;
   --     end loop;

   --     return Bez_2 (Bez_2'First + 1) - Bez_2 (Bez_2'First);
   --  end Tangent_At_T;

   function Point_At_T_V2 (Bez : PH_Bezier; T : Curve_Parameter) return Scaled_Position is
   begin
      return
        Bez.Control_Points (0) * ((1.0 - T) ** 15)
        + Scaled_Position_Offset (Bez.Control_Points (1)) * (15.0 * T * (1.0 - T) ** 14)
        + Scaled_Position_Offset (Bez.Control_Points (2)) * (105.0 * T ** 2 * (1.0 - T) ** 13)
        + Scaled_Position_Offset (Bez.Control_Points (3)) * (455.0 * T ** 3 * (1.0 - T) ** 12)
        + Scaled_Position_Offset (Bez.Control_Points (4)) * (1_365.0 * T ** 4 * (1.0 - T) ** 11)
        + Scaled_Position_Offset (Bez.Control_Points (5)) * (3_003.0 * T ** 5 * (1.0 - T) ** 10)
        + Scaled_Position_Offset (Bez.Control_Points (6)) * (5_005.0 * T ** 6 * (1.0 - T) ** 9)
        + Scaled_Position_Offset (Bez.Control_Points (7)) * (6_435.0 * T ** 7 * (1.0 - T) ** 8)
        + Scaled_Position_Offset (Bez.Control_Points (8)) * (6_435.0 * T ** 8 * (1.0 - T) ** 7)
        + Scaled_Position_Offset (Bez.Control_Points (9)) * (5_005.0 * T ** 9 * (1.0 - T) ** 6)
        + Scaled_Position_Offset (Bez.Control_Points (10)) * (3_003.0 * T ** 10 * (1.0 - T) ** 5)
        + Scaled_Position_Offset (Bez.Control_Points (11)) * (1_365.0 * T ** 11 * (1.0 - T) ** 4)
        + Scaled_Position_Offset (Bez.Control_Points (12)) * (455.0 * T ** 12 * (1.0 - T) ** 3)
        + Scaled_Position_Offset (Bez.Control_Points (13)) * (105.0 * T ** 13 * (1.0 - T) ** 2)
        + Scaled_Position_Offset (Bez.Control_Points (14)) * (15.0 * T ** 14 * (1.0 - T))
        + Scaled_Position_Offset (Bez.Control_Points (15)) * (T ** 15);
   end Point_At_T_V2;

   function Point_At_Distance (Bez : PH_Bezier; Distance : Length) return Scaled_Position is
   begin
      return Point_At_T (Bez, T_At_Distance (Bez, Distance));
   end Point_At_Distance;

   --  function Tangent_At_Distance (Bez : PH_Bezier; Distance : Length) return Scaled_Position_Offset is
   --  begin
   --     return Tangent_At_T (Bez, T_At_Distance (Bez, Distance));
   --  end Tangent_At_Distance;

   function Create_Bezier (Start, Corner, Finish : Scaled_Position; Deviation_Limit : Length) return PH_Bezier is
      function Real_Create_Bezier return PH_Bezier;

      function Real_Create_Bezier return PH_Bezier is
         function Sine_Secondary_Angle return Dimensionless;
         --  The secondary angle here is pi minus the angle of the corner, or the angles not equal to the corner in an
         --  equilateral triangle at the corner.

         function Cosine_Secondary_Angle return Dimensionless;
         function Base_Length return Length;

         function Sine_Secondary_Angle return Dimensionless is
            V1 : constant Scaled_Position_Offset := Start - Corner;
            V2 : constant Scaled_Position_Offset := Finish - Corner;
            A  : constant Area := Dot (V1, V2);
            B  : constant Area := 2.0 * (abs V1) * (abs V2);
         begin
            if 0.5 + A / B < 0.0 then
               return 0.0;
            elsif (0.5 + A / B) ** (1 / 2) > 1.0 then
               return 1.0;
            else
               return (0.5 + A / B) ** (1 / 2);
            end if;
         end Sine_Secondary_Angle;

         function Cosine_Secondary_Angle return Dimensionless is
            V1 : constant Scaled_Position_Offset := Corner - Start;
            V2 : constant Scaled_Position_Offset := Finish - Corner;
            A  : constant Area := Dot (V1, V2);
            B  : constant Area := 2.0 * (abs V1) * (abs V2);
         begin
            if 0.5 + A / B < 0.0 then
               return 0.0;
            elsif (0.5 + A / B) ** (1 / 2) > 1.0 then
               return 1.0;
            else
               return (0.5 + A / B) ** (1 / 2);
            end if;
         end Cosine_Secondary_Angle;

         function Base_Length return Length is
            Incoming_Length : constant Length := abs (Start - Corner);
            Outgoing_Length : constant Length := abs (Finish - Corner);

            Deviation_Base_Length_Numerator   : constant Length := Deviation_Limit * 2.0 ** 14;
            Deviation_Base_Length_Denominator : constant Dimensionless :=
              Sine_Secondary_Angle
              * (4_072_849.0 / 429.0 + 714.0 + 2.0 ** 14 * 1_225.0 / (858.0 * Cosine_Secondary_Angle));
            Incoming_Limit                    : constant Length :=
              (0.49 * 858.0 * Incoming_Length * Cosine_Secondary_Angle) / (5_210.0 * Cosine_Secondary_Angle + 1_225.0);
            Outgoing_Limit                    : constant Length :=
              (0.49 * 858.0 * Outgoing_Length * Cosine_Secondary_Angle) / (5_210.0 * Cosine_Secondary_Angle + 1_225.0);
         begin
            --  TODO: Do we need a small error margin here?
            if Deviation_Base_Length_Denominator = 0.0 then
               --  Collinear points.
               return Length'Min (Incoming_Limit, Outgoing_Limit);
            else
               return
                 Length'Min
                   (Deviation_Base_Length_Numerator / Deviation_Base_Length_Denominator,
                    Length'Min (Incoming_Limit, Outgoing_Limit));
            end if;
         end Base_Length;

         Incoming_Unit : constant Position_Scale := (Start - Corner) / abs (Start - Corner);
         Outgoing_Unit : constant Position_Scale := (Finish - Corner) / abs (Finish - Corner);
         M             : constant Scaled_Position_Offset :=
           ((Outgoing_Unit - Incoming_Unit) / abs (Outgoing_Unit - Incoming_Unit)) * Base_Length;
         Points        : PH_Control_Points;
      begin
         Points (0) :=
           Corner + Incoming_Unit * ((4.0 + 889.0 / 429.0 + 1_225.0 / (858.0 * Cosine_Secondary_Angle)) * Base_Length);
         Points (1) := Points (0) - Incoming_Unit * Base_Length;
         Points (2) := Points (1) - Incoming_Unit * Base_Length;
         Points (3) := Points (2) - Incoming_Unit * Base_Length;
         Points (4) := Points (3) - Incoming_Unit * Base_Length;
         Points (5) := Points (4) + M * (10.0 / 143.0) - Incoming_Unit * ((133.0 / 143.0) * Base_Length);
         Points (6) := Points (5) + M * (38.0 / 143.0) - Incoming_Unit * ((105.0 / 143.0) * Base_Length);
         Points (7) := Points (6) + M * (254.0 / 429.0) - Incoming_Unit * ((175.0 / 429.0) * Base_Length);
         --  Points (8) := Points (7) + M;
         --  Equivalent to below assignment.

         Points (15) :=
           Corner + Outgoing_Unit * ((4.0 + 889.0 / 429.0 + 1_225.0 / (858.0 * Cosine_Secondary_Angle)) * Base_Length);
         Points (14) := Points (15) - Outgoing_Unit * Base_Length;
         Points (13) := Points (14) - Outgoing_Unit * Base_Length;
         Points (12) := Points (13) - Outgoing_Unit * Base_Length;
         Points (11) := Points (12) - Outgoing_Unit * Base_Length;
         Points (10) := Points (11) - M * (10.0 / 143.0) - Outgoing_Unit * ((133.0 / 143.0) * Base_Length);
         Points (9) := Points (10) - M * (38.0 / 143.0) - Outgoing_Unit * ((105.0 / 143.0) * Base_Length);
         Points (8) := Points (9) - M * (254.0 / 429.0) - Outgoing_Unit * ((175.0 / 429.0) * Base_Length);

         --  Near-collinear inputs can land just above zero because the angle is reconstructed from floating-point dot
         --  products.
         if Sine_Secondary_Angle <= Collinear_Sine_Tolerance then
            --  Collinear points. The curvature is zero, so we return the largest possible inverse curvature value.
            return (Control_Points => Points, Inverse_Curvature => Length'Last);
         else
            return
              (Control_Points    => Points,
               Inverse_Curvature =>
                 (12.0 / 14.0) * Base_Length * (1.0 + Cosine_Secondary_Angle) ** 2 / Sine_Secondary_Angle);
         end if;
      end Real_Create_Bezier;
   begin
      if Start = Corner or else Finish = Corner then
         --  If the start or finish points are the same as the corner, then we have a zero-length segment, so we can't
         --  create a curve. Return a zero-length curve reresenting a sharp corner.
         return (Control_Points => [others => Corner], Inverse_Curvature => 0.0 * mm);
      else
         return Real_Create_Bezier;
      end if;
   end Create_Bezier;

end Prunt.Motion_Planner.PH_Beziers;
