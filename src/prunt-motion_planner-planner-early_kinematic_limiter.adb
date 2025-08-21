-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

package body Prunt.Motion_Planner.Planner.Early_Kinematic_Limiter is

   procedure Run (Block : in out Execution_Block) is
   begin
      Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'First) := 0.0 * mm / s;
      Block.Corner_Velocity_Limits (Block.Corner_Velocity_Limits'Last) := 0.0 * mm / s;

      for I in Block.Original_Segment_Feedrates'Range loop
         Block.Original_Segment_Feedrates (I) :=
           Velocity'Min (Block.Original_Segment_Feedrates (I), 299_792_458_000.1 * mm / s);

         declare
            Offset  : constant Scaled_Position_Offset := Block.Corners (I - 1) - Block.Corners (I);
            Has_XYZ : constant Boolean :=
              (Offset with delta E_Axis => 0.0 * mm) /= Scaled_Position_Offset'(others => Length (0.0));

            Feedrate : Velocity :=
              Velocity'Min (Block.Original_Segment_Feedrates (I), Block.Params.Tangential_Velocity_Max);
         begin
            if Block.Params.Ignore_E_In_XYZE and Has_XYZ then
               Feedrate := Feedrate * (abs Offset / abs [Offset with delta E_Axis => 0.0 * mm]);
               if abs [Offset with delta E_Axis => 0.0 * mm] > 0.0 * mm and Feedrate /= Velocity'Last then
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

            if abs Offset > 0.0 * mm then
               Feedrate := Velocity'Min (Feedrate, abs Offset / Interpolation_Time);
            --  This ensures that the step generator will not have to skip over many segments in a row, which could
            --  cause the command queue to run dry.

            end if;

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
