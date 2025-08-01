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

private generic
package Prunt.Motion_Planner.Planner.Step_Rate_Limiter is

   procedure Setup (In_Map : Stepper_Pos_Map);
   procedure Reset;
   procedure Run (Block : in out Execution_Block; Needs_New_Profiles : out Boolean);

private

   type Maximum_Overspeed_Type is array (2 .. Corners_Index'Last) of Dimensionless;

   protected Runner is
      procedure Setup (In_Map : Stepper_Pos_Map);
      --  TODO: Check that calling Run does not copy Block.
      procedure Run (Block : in out Execution_Block; Needs_New_Profiles : out Boolean);
      procedure Reset;
   private
      Setup_Done        : Boolean := False;
      Pos_Map           : Stepper_Pos_Map;
      pragma Warnings (Off, "use of an anonymous access type allocator");
      Maximum_Overspeed : access Maximum_Overspeed_Type := new Maximum_Overspeed_Type;
      pragma Warnings (On, "use of an anonymous access type allocator");
   end Runner;

end Prunt.Motion_Planner.Planner.Step_Rate_Limiter;
