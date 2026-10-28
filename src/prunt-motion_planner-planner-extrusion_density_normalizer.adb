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

pragma Check_Float_Overflow;

package body Prunt.Motion_Planner.Planner.Extrusion_Density_Normalizer is

   pragma Unsuppress (Overflow_Check);
   pragma Unsuppress (Range_Check);
   pragma Unsuppress (Division_Check);

   pragma Extensions_Allowed (On);

   procedure Run (Block : aliased in out Execution_Block; Workspace : not null access Planning_Workspace) is
      Normalization         : Extrusion_Normalization_Parameters renames Block.Params.Extrusion_Normalization;
      Disabled              : constant Boolean :=
        Normalization.Relative_Adjustment_Max = 0.0
        and then Normalization.E_Resolution = 0.0 * mm
        and then Normalization.XYZ_Resolution = 0.0 * mm;
      Sqrt_2                : constant := 1.414_213_562_373_095_05;
      Lengths               : Block_Segment_Lengths renames
        Workspace.Unblended_Segment_Lengths (Block.Primitives'Range);
      Spreads               : Block_Extrusion_Densities renames
        Workspace.Unblended_Length_Spreads (Block.Primitives'Range);
      Reference             : Block_Segment_Lengths renames
        Workspace.Unblended_Extrusion_Positions (Block.Corners'Range);
      First, Last           : Finishing_Corners_Index := Finishing_Corners_Index'First;
      Active                : Boolean := False;
      Run_Length            : Length := 0.0 * mm;
      Length_Roundoff       : Length := 0.0 * mm;
      Lower, Upper, Density : Dimensionless := 0.0;

      function Allowance (Segment : Finishing_Corners_Index; Density : Dimensionless) return Length;
      --  How far giving Segment the common Density may change its commanded E displacement.

      procedure Add_Length (Value : Length; Total, Roundoff : in out Length);
      procedure Finish_Run;

      function Allowance (Segment : Finishing_Corners_Index; Density : Dimensionless) return Length is
         Change : constant Length := Block.Corners (Segment) (E_Axis) - Block.Corners (Segment - 1) (E_Axis);
      begin
         return
           Length'Max
             (Normalization.Relative_Adjustment_Max * abs Change,
              Normalization.E_Resolution + abs Density * Spreads (Segment) * Normalization.XYZ_Resolution);
      end Allowance;

      procedure Add_Length (Value : Length; Total, Roundoff : in out Length) is
         Adjusted : constant Length := Value - Roundoff;
         Sum      : constant Length := Total + Adjusted;
      begin
         Roundoff := (Sum - Total) - Adjusted;
         Total := Sum;
      end Add_Length;

      procedure Finish_Run is
         Distance, Roundoff : Length := 0.0 * mm;
         Start_E, End_E     : Length;
      begin
         if not Active or else First = Last then
            return;
         end if;
         Start_E := Block.Corners (First - 1) (E_Axis);
         End_E := Block.Corners (Last) (E_Axis);
         --  Validate the stored candidate against each complete segment, including arithmetic roundoff. This final
         --  pass visits each segment only once across all runs. An uncertifiable run keeps its original densities and
         --  positions; it does not trigger recursive splitting or repeated fitting.
         for I in First .. Last loop
            declare
               Commanded      : constant Length := Block.Corners (I) (E_Axis) - Block.Corners (I - 1) (E_Axis);
               Replacement    : constant Length := Density * Lengths (I);
               Error          : constant Length := abs (Replacement - Commanded);
               Roundoff_Bound : constant Length :=
                 8.0 * Dimensionless'Model_Epsilon * (abs Replacement + abs Commanded);
            begin
               if Error + Roundoff_Bound > Allowance (I, Density) then
                  return;
               end if;
            end;
         end loop;
         for I in First .. Last loop
            --  Copy this one floating-point value verbatim. Neither E positions nor path distances are used to
            --  reconstruct it later, so exact equality identifies the whole accepted run.
            Block.Extrusion_Densities (I) := Density;
            Add_Length (Lengths (I), Distance, Roundoff);
            Reference (I) :=
              (if I = Last
               then End_E
               else
                 Length'Max
                   (Length'Min (Start_E, End_E),
                    Length'Min (Length'Max (Start_E, End_E), Start_E + Density * Distance)));
         end loop;
      end Finish_Run;
   begin
      for I in Block.Corners'Range loop
         Reference (I) := Block.Corners (I) (E_Axis);
      end loop;
      for I in Block.Primitives'Range loop
         declare
            Spatial : constant Derived_Path_Primitive :=
              Derive_Path_Primitive
                (Block.Primitives (I),
                 [Block.Corners (I - 1) with delta E_Axis => 0.0 * mm],
                 [Block.Corners (I) with delta E_Axis => 0.0 * mm]);
            Change  : constant Length := Block.Corners (I) (E_Axis) - Block.Corners (I - 1) (E_Axis);
         begin
            Lengths (I) := (if Spatial.Length > 0.0 * mm then Spatial.Length else Primitive_Length (Block'Access, I));
            Spreads (I) :=
              (case Spatial.Kind is
                 when Line_Primitive_Kind  =>
                   --  Equal coordinates round identically, so only the axes the line moves along contribute.
                   abs Spatial.Direction (X_Axis) + abs Spatial.Direction (Y_Axis) + abs Spatial.Direction (Z_Axis),
                 when Helix_Primitive_Kind =>
                   --  Rounding the ends and centre moves the radius by at most Sqrt (2) resolutions, the swept angle by
                   --  at most 2 * Sqrt (2) resolutions over the radius, and the axial travel by one resolution.
                   Sqrt_2 * (abs Spatial.Theta_Delta + 2.0) + 1.0);
            Block.Extrusion_Densities (I) := (if Lengths (I) > 0.0 * mm then Change / Lengths (I) else 0.0);
            if Disabled or else Spatial.Length = 0.0 * mm or else Change = 0.0 * mm then
               Finish_Run;
               Active := False;
            else
               declare
                  Magnitude                               : constant Length := abs Change;
                  Relative                                : constant Length :=
                    Normalization.Relative_Adjustment_Max * Magnitude;
                  Slack                                   : constant Length :=
                    Spreads (I) * Normalization.XYZ_Resolution;
                  --  The density magnitudes that either allowance admits. Both ranges contain the segment's own
                  --  density, so together they form one range. The rounding allowance grows with the density, so a
                  --  segment shorter than its length's rounding admits every larger density.
                  Smallest                                : constant Dimensionless :=
                    Dimensionless'Min
                      ((Magnitude - Relative) / Lengths (I),
                       (Magnitude - Normalization.E_Resolution) / (Lengths (I) + Slack));
                  Largest                                 : constant Dimensionless :=
                    (if Lengths (I) > Slack
                     then
                       Dimensionless'Max
                         ((Magnitude + Relative) / Lengths (I),
                          (Magnitude + Normalization.E_Resolution) / (Lengths (I) - Slack))
                     else Dimensionless'Last);
                  Low                                     : constant Dimensionless :=
                    (if Change > 0.0 * mm then Smallest else -Largest);
                  High                                    : constant Dimensionless :=
                    (if Change > 0.0 * mm then Largest else -Smallest);
                  Trial_Length                            : Length := Run_Length;
                  Trial_Roundoff                          : Length := Length_Roundoff;
                  Trial_Lower, Trial_Upper, Trial_Density : Dimensionless;
                  Append                                  : Boolean := False;
               begin
                  if Active
                    and then Block.Corner_Dwell_Times (I - 1) = 0.0 * s
                    and then (Change > 0.0 * mm) = (Density > 0.0)
                  then
                     Add_Length (Lengths (I), Trial_Length, Trial_Roundoff);
                     Trial_Lower := Dimensionless'Max (Lower, Low);
                     Trial_Upper := Dimensionless'Min (Upper, High);
                     Trial_Density := (Block.Corners (I) (E_Axis) - Block.Corners (First - 1) (E_Axis)) / Trial_Length;
                     Append := Trial_Density in Trial_Lower .. Trial_Upper;
                  end if;
                  if Append then
                     Last := I;
                     Run_Length := Trial_Length;
                     Length_Roundoff := Trial_Roundoff;
                     Lower := Trial_Lower;
                     Upper := Trial_Upper;
                     Density := Trial_Density;
                  else
                     Finish_Run;
                     First := I;
                     Last := I;
                     Active := True;
                     Run_Length := Lengths (I);
                     Length_Roundoff := 0.0 * mm;
                     Lower := Low;
                     Upper := High;
                     Density := Block.Extrusion_Densities (I);
                  end if;
               end;
            end if;
         end;
      end loop;
      Finish_Run;
   end Run;

end Prunt.Motion_Planner.Planner.Extrusion_Density_Normalizer;
