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

package body Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter is

   pragma Unsuppress (Overflow_Check);
   pragma Unsuppress (Range_Check);
   pragma Unsuppress (Division_Check);

   pragma Extensions_Allowed (On);

   procedure Run
     (Block               : aliased in out Execution_Block;
      Motor_Map           : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Normalize_Feedrates : Boolean := True) is
   begin
      Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'First) := 0.0 * mm / s;
      Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'Last) := 0.0 * mm / s;

      for I in Block.Original_Segment_Feedrates'Range loop
         --  Clamp the feedrate to the speed of light in a vacuum. This is a safety measure to prevent overflows and
         --  other issues with very large feedrates. If your printer is capable of exceeding the speed of light then
         --  please file a bug report.
         Block.Original_Segment_Feedrates (I) :=
           Velocity'Min (Block.Original_Segment_Feedrates (I), 299_792_458_000.1 * mm / s);

         declare
            Primitive          : constant Derived_Path_Primitive := Spatial_Primitive (Block'Access, I);
            Segment_Distance   : constant Length := Segment_Total_Distance (Block'Access, I);
            Primitive_Distance : constant Length := Block.Primitive_Distances (I);
            Bounds             : constant Unit_Speed_Axial_Derivative_Bounds :=
              Primitive_Derivative_Bounds (Block'Access, I, Block.Primitive_Start_Distances (I), Primitive_Distance);
            Velocity_Safety    : constant Dimensionless := (if Primitive_Distance > 0.0 * mm then 0.999 else 1.0);
            Offset             : constant Position_Offset := Block.Corners (I - 1) - Block.Corners (I);
            XYZ_Path_Length    : Length;

            Tangential_Max : Velocity := Block.Params.Tangential_Velocity_Max;
            Feedrate       : Velocity;
         begin
            case Primitive.Kind is
               when Line_Primitive_Kind  =>
                  XYZ_Path_Length := abs [Offset with delta E_Axis => 0.0 * mm];

               when Helix_Primitive_Kind =>
                  XYZ_Path_Length :=
                    (Primitive.Radius**2 + (abs [Primitive.Axial_Per_Phase with delta E_Axis => 0.0 * mm])**2)**(1 / 2)
                    * abs Primitive.Theta_Delta;
            end case;

            if not Block.Params.Ignore_E_In_XYZE and then XYZ_Path_Length > 0.0 * mm then
               declare
                  --  Convert using the accepted density, not the rounded E coordinates.
                  Spatial_Scale : constant Dimensionless := XYZE_Spatial_Scale (Block.Extrusion_Densities (I));
               begin
                  --  The programmed feedrate is normalized once, but the configured cap remains in XYZE units and must
                  --  be converted on every run, including geometry recovery with stopped corners.
                  Tangential_Max := Tangential_Max * Spatial_Scale;
                  if Normalize_Feedrates then
                     Block.Original_Segment_Feedrates (I) := Block.Original_Segment_Feedrates (I) * Spatial_Scale;
                  end if;
               end;
            end if;
            Feedrate := Velocity'Min (Block.Original_Segment_Feedrates (I), Tangential_Max);

            if Block.Extrusion_Densities (I) /= 0.0 then
               --  Only the constant-density part is common to every candidate profile window. Density-transition
               --  limits belong to the windows and junction coasts that actually overlap them.
               Feedrate :=
                 Velocity'Min
                   (Feedrate, 0.999 * Block.Params.Axial_Velocity_Maxes (E_Axis) / abs Block.Extrusion_Densities (I));
            end if;

            --  Enforce a minimum segment time to prevent any possible issues in the step generator.
            if Segment_Distance > 0.0 * mm then
               Feedrate := Velocity'Min (Feedrate, Segment_Distance / Interpolation_Time);
            end if;

            --  Apply axial velocity limits. The feedrate is scaled down if any single axis exceeds its maximum allowed
            --  velocity.
            for A in Axis_Name loop
               if Bounds.Velocity (A) > 0.0 then
                  Feedrate :=
                    Velocity'Min
                      (Feedrate, Velocity_Safety * Block.Params.Axial_Velocity_Maxes (A) / Bounds.Velocity (A));
               end if;
            end loop;

            if Primitive_Distance > 0.0 * mm then
               Feedrate :=
                 Primitive_Motor_Delta_Ceiling
                   (Block'Access, Motor_Map, I, Block.Primitive_Start_Distances (I), Primitive_Distance, Feedrate);
            end if;

            Block.Limited_Segment_Feedrates (I) := Feedrate;
         end;
      end loop;

      Assign_Extrusion_Transitions (Block, Motor_Map);
   end Run;

end Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter;
