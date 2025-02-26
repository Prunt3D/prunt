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

with Ada.Numerics.Generic_Elementary_Functions;

package body Prunt.Motion_Planner.Planner.Corner_Blender is

   package Angle_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Angle);

   procedure Run (Block : in out Execution_Block) is
      Last_Comp_Error : Length := 0.0 * mm;

      function Allow_Corner_Shift (I : Corners_Index) return Boolean is
         Unscaled_Corner : constant Position := Position (Block.Corners (I) * Block.Params.Axial_Scaler);
      begin
         if not Block.Params.Shift_Blended_Corners then
            return False;
         end if;

         --  If all virtual corners are within the allowed volume, then no point on any curve will be outside of the
         --  allowed volume. This assumes that all original points were within the allowed volume.
         --
         --  TODO: More advanced corner shifting that allows for virtual corners to be outside the volume when the
         --  curve is fully inside the volume.
         for A in Axis_Name loop
            if Unscaled_Corner (A) - Block.Params.Chord_Error_Max < Block.Params.Lower_Pos_Limit (A)
              or Unscaled_Corner (A) + Block.Params.Chord_Error_Max > Block.Params.Upper_Pos_Limit (A)
            then
               return False;
            end if;
         end loop;

         return True;
      end Allow_Corner_Shift;
   begin
      for I in Block.Corners'Range loop
         Shifted_Corners (I) := Block.Corners (I);
      end loop;

      for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
         Shifted_Corner_Error_Limits (I) := Block.Params.Chord_Error_Max;
      end loop;

      Shifted_Corner_Error_Limits (Block.Corners'First) := 0.0 * mm;
      Shifted_Corner_Error_Limits (Block.Corners'Last) := 0.0 * mm;

      Block.Beziers (Block.Beziers'First) :=
        Create_Bezier
          (Block.Corners (Block.Beziers'First),
           Block.Corners (Block.Beziers'First),
           Block.Corners (Block.Beziers'First),
           0.0 * mm);
      Block.Beziers (Block.Beziers'Last) :=
        Create_Bezier
          (Block.Corners (Block.Beziers'Last),
           Block.Corners (Block.Beziers'Last),
           Block.Corners (Block.Beziers'Last),
           0.0 * mm);

      loop
         Last_Comp_Error := 0.0 * mm;

         for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
            if Angle_Elementary_Functions.Sin (Corner_Blender_Max_Secondary_Angle_To_Blend)
              < Sine_Secondary_Angle (Block.Corners (I - 1), Block.Corners (I), Block.Corners (I + 1))
            then
               Block.Beziers (I) := Create_Bezier (Block.Corners (I), Block.Corners (I), Block.Corners (I), 0.0 * mm);
            else
               Block.Beziers (I) :=
                 Create_Bezier
                   (Shifted_Corners (I - 1),
                    Shifted_Corners (I),
                    Shifted_Corners (I + 1),
                    Shifted_Corner_Error_Limits (I));
               if Allow_Corner_Shift (I) then
                  Last_Comp_Error :=
                    Length'Max (Last_Comp_Error, abs (Midpoint (Block.Beziers (I)) - Block.Corners (I)));
               end if;
            end if;
         end loop;

         exit when Last_Comp_Error <= Corner_Blender_Max_Computational_Error;

         for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
            if Allow_Corner_Shift (I) then
               Shifted_Corners (I) := @ + (Block.Corners (I) - Midpoint (Block.Beziers (I)));
            end if;
         end loop;

         for I in Block.Corners'First + 1 .. Block.Corners'Last - 1 loop
            if Allow_Corner_Shift (I) then
               declare
                  Start  : constant Scaled_Position := Shifted_Corners (I - 1);
                  Corner : constant Scaled_Position := Shifted_Corners (I);
                  Finish : constant Scaled_Position := Shifted_Corners (I + 1);
               begin
                  Shifted_Corner_Error_Limits (I) :=
                    abs Dot (Block.Corners (I) - Shifted_Corners (I), Unit_Bisector (Start, Corner, Finish));
               end;
            end if;
         end loop;
      end loop;
   end Run;

   function Sine_Secondary_Angle (Start, Corner, Finish : Scaled_Position) return Dimensionless is
      V1 : constant Scaled_Position_Offset := Start - Corner;
      V2 : constant Scaled_Position_Offset := Finish - Corner;
      A  : constant Area := Dot (V1, V2);
      B  : constant Area := 2.0 * (abs V1) * (abs V2);
   begin
      if B = 0.0 then
         return 1.0;
      elsif 0.5 + A / B < 0.0 then
         return 0.0;
      elsif (0.5 + A / B)**(1 / 2) > 1.0 then
         return 1.0;
      else
         return (0.5 + A / B)**(1 / 2);
      end if;
   end Sine_Secondary_Angle;

   function Unit_Bisector (Start, Corner, Finish : Scaled_Position) return Position_Scale is
      A        : constant Scaled_Position_Offset := Start - Corner;
      B        : constant Scaled_Position_Offset := Finish - Corner;
      Bisector : constant Position_Scale :=
        (if abs A = 0.0 or abs B = 0.0 then (others => 0.0) else A / abs A + B / abs B);
   begin
      if abs Bisector = 0.0 then
         return Bisector;
      else
         return Bisector / abs Bisector;
      end if;
   end Unit_Bisector;

end Prunt.Motion_Planner.Planner.Corner_Blender;
