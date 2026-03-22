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

pragma Extensions_Allowed (On);

private with System.Pool_Local;

private generic
package Prunt.Motion_Planner.Planner.Corner_Blender is

   procedure Run (Block : in out Execution_Block);
   --  Replaces sharp corners in `Block.Corners` with C4 continuous PH Bézier curves, storing them in `Block.Beziers`.
   --
   --  The maximum deviation of these curves from the original path is limited by `Chord_Error_Max`.
   --
   --  If `Shift_Blended_Corners` is True then corners that are not within `Chord_Error_Max` of the work area
   --  boundaries and are not outside of the work area will be shifted so that the midpoint of the curve is where the
   --  original corner was. `Block.Corners` will not be modified when the generated curves are shifted. The accuracy
   --  for this shifting is controlled by the generic parameter `Corner_Blender_Max_Computational_Error`.
   --
   --  This procedure uses a number of large allocated arrays and will only allow one call to run at a time while other
   --  calls block. This package may be instantiated multiple times if concurrency is required.

private

   Pool : System.Pool_Local.Unbounded_Reclaim_Pool;

   type Shifted_Corner_Error_Limits_Type is array (Corners_Index) of Length;
   type Shifted_Corners_Type is array (Corners_Index) of Scaled_Position;

   type Shifted_Corner_Error_Limits_Type_Access is access Shifted_Corner_Error_Limits_Type with Storage_Pool => Pool;
   type Shifted_Corners_Type_Access is access Shifted_Corners_Type with Storage_Pool => Pool;

   protected Runner is
      procedure Run (Block : in out Execution_Block);
   private
      Shifted_Corner_Error_Limits : Shifted_Corner_Error_Limits_Type_Access := new Shifted_Corner_Error_Limits_Type;
      Shifted_Corners             : Shifted_Corners_Type_Access := new Shifted_Corners_Type;
      --  When corner shifting is enabled, the original corners are not used directly to create the Bézier curves.
      --  Instead, virtual corners are created by shifting the original corner points. These arrays store the positions
      --  of the virtual corners and the calculated deviation limits for them during the solving process.
   end Runner;

   function Sine_Secondary_Angle (Start, Corner, Finish : Scaled_Position) return Dimensionless;
   --  Calculates the sine of 0.5 × (180° - the angle between the two segments meeting at a corner), or the angles in
   --  an isosceles triangle at the corner where the corner angle is the potentially unique angle. A value of 0.0
   --  corresponds to a 180° angle at the corner (a straight line), while a value of 1.0 corresponds to a 0° angle at
   --  the corner (a complete reversal of direction).
   --
   --  1.0 is returned in the case of any two of the points being equal.

   function Unit_Bisector (Start, Corner, Finish : Scaled_Position) return Position_Scale;
   --  Computes the unit vector that bisects the angle formed by the segments (`Start` -> `Corner`) and (`Corner` ->
   --  `Finish`). The input vectors do not need to be normalised by the caller.
   --
   --  All elements in the return value are set to zero if either segment has zero length.

end Prunt.Motion_Planner.Planner.Corner_Blender;
