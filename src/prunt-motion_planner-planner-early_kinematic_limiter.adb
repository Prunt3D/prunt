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

package body Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter is

   pragma Extensions_Allowed (On);

   procedure Run
     (Block : aliased in out Execution_Block; Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map) is
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
            Primitive          : constant Derived_Path_Primitive :=
              Derive_Path_Primitive
                (Block.Primitives (I), Block.Corners (I - 1), Block.Corners (I));
            Path_Length        : constant Length := Primitive.Length;
            Segment_Distance   : constant Length := Segment_Total_Distance (Block'Access, I);
            Primitive_Distance : constant Length := Block.Primitive_Distances (I);
            Bounds             : constant Unit_Speed_Axial_Derivative_Bounds :=
              Primitive_Derivative_Bounds
                (Block'Access, I, Block.Primitive_Start_Distances (I), Primitive_Distance);
            Velocity_Safety    : constant Dimensionless := (if Primitive_Distance > 0.0 * mm then 0.999 else 1.0);
            Offset             : constant Position_Offset := Block.Corners (I - 1) - Block.Corners (I);
            XYZ_Path_Length    : Length;

            Feedrate : Velocity :=
              Velocity'Min (Block.Original_Segment_Feedrates (I), Block.Params.Tangential_Velocity_Max);
         begin
            case Primitive.Kind is
               when Line_Primitive_Kind  =>
                  XYZ_Path_Length := abs [Offset with delta E_Axis => 0.0 * mm];

               when Helix_Primitive_Kind =>
                  XYZ_Path_Length :=
                    (Primitive.Radius ** 2
                     + (abs [Primitive.Axial_Per_Phase with delta E_Axis => 0.0 * mm]) ** 2)
                    ** (1 / 2)
                    * abs Primitive.Theta_Delta;
            end case;

            if Block.Params.Ignore_E_In_XYZE and then XYZ_Path_Length > 0.0 * mm then
               declare
                  Full_Path_Scale : constant Dimensionless := Path_Length / XYZ_Path_Length;
               begin
                  Feedrate := Feedrate * Full_Path_Scale;
                  --  Segment_Vel_Ratio_At_Time operates on the planner's full-path scalar velocity. Keep its
                  --  programmed reference in the same coordinates so a move at the requested XYZ speed reports 1.0.
                  Block.Original_Segment_Feedrates (I) :=
                    Block.Original_Segment_Feedrates (I) * Full_Path_Scale;
               end;
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
                      (Feedrate,
                       Velocity_Safety * Block.Params.Axial_Velocity_Maxes (A) / Bounds.Velocity (A));
               end if;
            end loop;

            if Primitive_Distance > 0.0 * mm then
               Feedrate :=
                 Primitive_Motor_Delta_Ceiling
                   (Block'Access,
                    Motor_Map,
                    I,
                    Block.Primitive_Start_Distances (I),
                    Primitive_Distance,
                    Feedrate);
            end if;

            Block.Limited_Segment_Feedrates (I) := Feedrate;
         end;
      end loop;
   end Run;

end Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter;
