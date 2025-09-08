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

private with System.Pool_Local;

private generic
package Prunt.Motion_Planner.Planner.Step_Rate_Limiter is

   procedure Setup (In_Map : Stepper_Pos_Map);

   procedure Reset;

   procedure Run (Block : in out Execution_Block; Needs_New_Profiles : out Boolean);
   --  Simulate the execution of the given block and check for step rate violations. If any stepper motor would exceed
   --  its maximum step rate, the affected segments' velocities are reduced and `Needs_New_Profiles` is set to True,
   --  indicating that the kinematic limiting stages must be re-run. If no violations are found, `Needs_New_Profiles`
   --  is set to False.

private

   Pool : System.Pool_Local.Unbounded_Reclaim_Pool;

   type Maximum_Overspeed_Type is array (2 .. Corners_Index'Last) of Dimensionless;
   type Maximum_Overspeed_Type_Access is access Maximum_Overspeed_Type with Storage_Pool => Pool;

   protected Runner is
      procedure Setup (In_Map : Stepper_Pos_Map);
      --  TODO: Check that calling Run does not copy Block.
      procedure Run (Block : in out Execution_Block; Needs_New_Profiles : out Boolean);
      procedure Reset;
   private
      Setup_Done        : Boolean := False;
      Pos_Map           : Stepper_Pos_Map;
      Maximum_Overspeed : Maximum_Overspeed_Type_Access := new Maximum_Overspeed_Type;
   end Runner;

end Prunt.Motion_Planner.Planner.Step_Rate_Limiter;
