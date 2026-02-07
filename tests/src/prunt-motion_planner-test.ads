-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
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
