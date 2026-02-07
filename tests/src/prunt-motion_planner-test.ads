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

with Trendy_Test;

package Prunt.Motion_Planner.Test is

   function All_Tests return Trendy_Test.Test_Group;

private

   N_Kinematic_Check_Steps : constant := 1000;
   N_Boundary_Intervals    : constant := 1000;
   Tolerance_Epsilon       : constant := 1.0E-12;

   procedure Check_Profile_For_Distance
     (Start_Vel   : Velocity;
      Distance    : Length;
      Max_Accel   : Acceleration;
      Max_Jerk    : Jerk;
      Max_Snap    : Snap;
      Max_Crackle : Crackle;
      Region      : Constraint_Region;
      Index       : Integer;
      Name        : String;
      T           : in out Trendy_Test.Operation'Class);
   procedure Check_Profile_For_Delta_V
     (Delta_V     : Velocity;
      Max_Accel   : Acceleration;
      Max_Jerk    : Jerk;
      Max_Snap    : Snap;
      Max_Crackle : Crackle;
      Region      : Constraint_Region;
      Index       : Integer;
      Name        : String;
      T           : in out Trendy_Test.Operation'Class);

end Prunt.Motion_Planner.Test;
