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

with Prunt.Motion_Planner.Planner;
with Prunt.Motion_Planner.Planner.Test;

package body Prunt.Motion_Planner.Planner_Primitive_Jet_Test is

   pragma Extensions_Allowed (On);

   type Test_Motor_Name is range 1 .. 1;
   type Test_Motor_Position_Map is array (Axis_Name, Test_Motor_Name) of Length;
   type Test_Motor_Delta_Limits is array (Test_Motor_Name) of Dimensionless;
   type Test_Flush_Resetting_Data is null record;
   type Test_Corner_Extra_Data is null record;

   package Test_Planner is new
     Prunt.Motion_Planner.Planner
       (Motor_Name                        => Test_Motor_Name,
        Motor_Position_Map                => Test_Motor_Position_Map,
        Motor_Delta_Limits                => Test_Motor_Delta_Limits,
        Maximum_Deltas_Per_Command        => [others => 1.0],
        Flush_Resetting_Data_Type         => Test_Flush_Resetting_Data,
        Flush_Resetting_Data_Type_Default => (null record),
        Corner_Extra_Data_Type            => Test_Corner_Extra_Data,
        Home_Move_Minimum_Coast_Time      => 5.0 * ms,
        Interpolation_Time                => 1.0 * ms,
        Max_Corners                       => 4,
        Max_Corners_Extra_Data_Count      => 2,
        Max_Corners_Extra_Data_Storage    => 1_024,
        Max_Corners_Extra_Data_Per_Corner => 2,
        Input_Queue_Length                => 2);

   package Test_Planner_Test is new Test_Planner.Test;

   function All_Tests return Trendy_Test.Test_Group renames Test_Planner_Test.All_Tests;

begin
   abort Test_Planner.Runner;
end Prunt.Motion_Planner.Planner_Primitive_Jet_Test;
