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
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

package body Prunt.Motion_Planner.Trajectory.Checked
  with SPARK_Mode => Off
is

   procedure Divide (Distance : Length; Speed : Velocity; Result : out Time) is
   begin
      Result := Distance / Speed;
   end Divide;

   procedure Total_Time (Profile : Feedrate_Profile; Result : out Time) is
   begin
      Result := Total_Time (Profile);
   end Total_Time;

   procedure Travel
     (Accel_Distance : Length; Peak : Velocity; Coast : Time; Decel_Distance : Length; Result : out Length) is
   begin
      Result := Accel_Distance + Peak * Coast + Decel_Distance;
   end Travel;

end Prunt.Motion_Planner.Trajectory.Checked;
