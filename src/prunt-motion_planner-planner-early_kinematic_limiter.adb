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

   procedure Run (Block : in out Execution_Block) is
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
            Offset  : constant Scaled_Position_Offset := Block.Corners (I - 1) - Block.Corners (I);
            Has_XYZ : constant Boolean :=
              (Offset with delta E_Axis => 0.0 * mm) /= Scaled_Position_Offset'(others => Length (0.0));

            Feedrate : Velocity :=
              Velocity'Min (Block.Original_Segment_Feedrates (I), Block.Params.Tangential_Velocity_Max);
         begin
            if Block.Params.Ignore_E_In_XYZE and then Has_XYZ then
               Feedrate := Feedrate * (abs Offset / abs [Offset with delta E_Axis => 0.0 * mm]);
               if abs [Offset with delta E_Axis => 0.0 * mm] > 0.0 * mm and then Feedrate /= Velocity'Last then
                  Feedrate :=
                    Feedrate
                    * abs ([Offset with delta E_Axis => 0.0 * mm] / Block.Params.Axial_Scaler)
                    / abs ([Offset with delta E_Axis => 0.0 * mm]);
               end if;

               Block.Original_Segment_Feedrates (I) :=
                 Block.Original_Segment_Feedrates (I)
                 * abs ([Offset with delta E_Axis => 0.0 * mm] / Block.Params.Axial_Scaler)
                 / abs ([Offset with delta E_Axis => 0.0 * mm]);
            else
               if abs Offset > 0.0 * mm then
                  Feedrate := Feedrate * abs (Offset / Block.Params.Axial_Scaler) / abs (Offset);

                  Block.Original_Segment_Feedrates (I) :=
                    Block.Original_Segment_Feedrates (I) * abs (Offset / Block.Params.Axial_Scaler) / abs (Offset);
               end if;
            end if;

            --  Enforce a minimum segment time to prevent any possible issues in the step generator.
            if abs Offset > 0.0 * mm then
               Feedrate := Velocity'Min (Feedrate, abs Offset / Interpolation_Time);
            end if;

            --  Apply axial velocity limits. The feedrate is scaled down if any single axis exceeds its maximum allowed
            --  velocity.
            for A in Axis_Name loop
               if abs Offset (A) > 0.0 * mm then
                  Feedrate :=
                    Velocity'Min
                      (Feedrate,
                       Block.Params.Axial_Velocity_Maxes (A)
                       / Block.Params.Axial_Scaler (A)
                       * abs Offset
                       / abs Offset (A));
               end if;
            end loop;

            Block.Limited_Segment_Feedrates (I) := Feedrate;
         end;
      end loop;
   end Run;

end Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter;
