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

--  How the rolling pressure advance smoothing algorithm works:
--
--  We begin with the smoothing function from Klipper:
--
--  smooth_pa_position(t) =
--    ∫[t - smooth_time/2, t + smooth_time/2](pa_position(x) * (smooth_time/2 - abs(t - x)))
--    / (smooth_time/2)^2
--
--  If we used this directly it would be very expensive as we would have to recalculate the integral at the step
--  command output rate by summing a large number of values. Since we use fixed time steps in Prunt, what we are
--  looking for here is a simple recurrence relationship on fixed steps of t. We find that relationship as follows:
--
--  Let w = smooth_time / 2
--
--  Let p = pa_position
--
--  S(t) = ∫[t-w, t+w](p(x) * (w - abs(t - x)))
--
--  S(t) =   ∫[t-w, t](p(x) * (w - t - x))
--         + ∫[t, t+w](p(x) * (w + t - x))
--
--  S'(t) =   ∫[t-w, t](-p(x))
--          - ∫[t, t+w]( p(x))
--
--  S''(t) =   (p(t+w) - p(t))
--           - (p(t) - p(t-w))
--
--  S''(t) = p(t+w) + p(t-w) - 2p(t)
--
--  In the discrete world, the second order difference is equivalent to the second derivative, meaning that:
--
--  S(t+1) - 2S(t) + S(t-1) = S''(t)
--
--  S(t+1) - 2S(t) + S(t-1) = p(t+w) + p(t-w) - 2p(t)
--
--  We can rearrange this to find:
--
--  S(t+1) = p(t+w) + p(t-w) - 2p(t) + 2S(t) - S(t-1)
--
--  Offsetting by 1:
--
--  S(t) = p(t+w-1) + p(t-w-1) - 2p(t-1) + 2S(t-1) - S(t-2)

with Prunt.Input_Shapers.Shapers;

package Prunt.Input_Shapers.Pressure_Advance_Shapers is

   type Pressure_Advance_Shaper
     (Input_Offset   : Cycle_Count;
      Extra_End_Time : Cycle_Count;
      Buffer_Size    : Cycle_Count)
   is new Shapers.Shaper with private;

   function Create
     (Parameters : Shaper_Parameters; Interpolation_Time : Time; Start_Position : Length)
      return Pressure_Advance_Shaper
   with Pre => Parameters.Kind in Pressure_Advance;

   overriding
   function Do_Step (This : in out Pressure_Advance_Shaper; Step : Length) return Length;

private

   type Buffer_Member is record
      Pos     : Length;
      PA_Part : Length;
   end record;

   type Buffer_Array is array (Cycle_Count range <>) of Buffer_Member;

   type Previous_Outputs_Array is array (-2 .. -1) of Length;

   type Pressure_Advance_Shaper
     (Input_Offset   : Cycle_Count;
      Extra_End_Time : Cycle_Count;
      Buffer_Size    : Cycle_Count)
   is new Shapers.Shaper (Input_Offset => Input_Offset, Extra_End_Time => Extra_End_Time) with record
      Buffer                 : Buffer_Array (0 .. Buffer_Size);
      --  These buffers are technically 1 larger than Buffer_Size, but that does not matter. Starting at 0 makes the
      --  implementation simpler. We can not subtract from a value that comes from a discriminant to get the correct
      --  size while starting at 0.
      Current_Buffer_Index   : Cycle_Count;
      Previous_Outputs       : Previous_Outputs_Array;
      Previous_Input         : Length;
      Pressure_Advance_Time  : Time;
      Interpolation_Time     : Time;
      Smooth_Added_Part_Only : Boolean;
   end record;

end Prunt.Input_Shapers.Pressure_Advance_Shapers;
