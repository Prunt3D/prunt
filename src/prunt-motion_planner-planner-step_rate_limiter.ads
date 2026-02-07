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
package Prunt.Motion_Planner.Planner.Step_Rate_Limiter is

   procedure Setup (In_Map : Motor_Pos_Map);

   procedure Reset;

   procedure Run (Block : in out Execution_Block; Needs_New_Profiles : out Boolean);
   --  Simulate the execution of the given block and check for step rate violations. If any motor would exceed its
   --  maximum delta per command, the affected segments' velocities are reduced and Needs_New_Profiles is set to
   --  True, indicating that the kinematic limiting stages must be re-run. If no violations are found,
   --  Needs_New_Profiles is set to False.

private

   function To_Motor_Position (Pos : Position; Map : Motor_Pos_Map) return Motor_Position;

   Pool : System.Pool_Local.Unbounded_Reclaim_Pool;

   type Maximum_Overspeed_Type is array (2 .. Corners_Index'Last) of Dimensionless;
   type Maximum_Overspeed_Type_Access is access Maximum_Overspeed_Type with Storage_Pool => Pool;

   protected Runner is
      procedure Setup (In_Map : Motor_Pos_Map);
      --  TODO: Check that calling Run does not copy Block.
      procedure Run (Block : in out Execution_Block; Needs_New_Profiles : out Boolean);
      procedure Reset;
   private
      Setup_Done        : Boolean := False;
      Pos_Map           : Motor_Pos_Map;
      Maximum_Overspeed : Maximum_Overspeed_Type_Access := new Maximum_Overspeed_Type;
   end Runner;

end Prunt.Motion_Planner.Planner.Step_Rate_Limiter;
