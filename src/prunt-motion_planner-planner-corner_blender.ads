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

private with Ada.Numerics.Generic_Elementary_Functions;
private with System.Pool_Local;

private generic
package Prunt.Motion_Planner.Planner.Corner_Blender is

   procedure Run
     (Block     : aliased in out Execution_Block;
      Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
      Workspace : not null access Planning_Workspace);
   --  Select and certify the configured Stereographic, Circular, Parabolic, Biarc, or Sharp_SCV representation at each
   --  corner, storing compact evaluators and transient planning summaries. Stereographic preserves derivatives through
   --  order four; Circular and Parabolic are C1; Biarc is C1 at its endpoints and internal splice; Sharp_SCV is C0.
   --  Unsupported, out-of-bounds, or uncertifiable geometry retains the commanded path and inserts a hard stop.

private

   package Angle_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Angle);
   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   type Corner_Lengths is array (Corners_Index) of Length;
   type Corner_Flags is array (Corners_Index) of Boolean;

   type Corner_Transition_Attempt is record
      Accepted             : Boolean := False;
      Requires_Hard_Anchor : Boolean := False;
      Trim_In              : Length := 0.0 * mm;
      Trim_Out             : Length := 0.0 * mm;
      Failure_Limit        : Length := 0.0 * mm;
      Failure_Upper        : Length := Length'Last;
   end record;

   type Axial_Deviation_Check is record
      Pass        : Boolean := False;
      Worst_Ratio : Dimensionless := Dimensionless'Last;
   end record;

   type Corner_Transition_Attempts is array (Corners_Index) of Corner_Transition_Attempt;

   Pool : System.Pool_Local.Unbounded_Reclaim_Pool;

   type Corner_Lengths_Access is access Corner_Lengths with Storage_Pool => Pool;
   type Corner_Flags_Access is access Corner_Flags with Storage_Pool => Pool;
   type Corner_Transition_Attempts_Access is access Corner_Transition_Attempts with Storage_Pool => Pool;

   protected Runner is
      procedure Run
        (Block     : aliased in out Execution_Block;
         Motor_Map : Prunt.Motion_Planner.Planner.Motor_Position_Map;
         Workspace : not null access Planning_Workspace);
   private
      Target_Incoming_Trims    : Corner_Lengths_Access := new Corner_Lengths'(others => 0.0 * mm);
      Target_Outgoing_Trims    : Corner_Lengths_Access := new Corner_Lengths'(others => 0.0 * mm);
      Allocated_Incoming_Trims : Corner_Lengths_Access := new Corner_Lengths'(others => 0.0 * mm);
      Allocated_Outgoing_Trims : Corner_Lengths_Access := new Corner_Lengths'(others => 0.0 * mm);
      Hard_Anchors             : Corner_Flags_Access := new Corner_Flags'(others => True);
      Cached_Attempts          : Corner_Transition_Attempts_Access :=
        new Corner_Transition_Attempts'(others => (others => <>));
      Cached_Attempt_Valid     : Corner_Flags_Access := new Corner_Flags'(others => False);
   end Runner;

end Prunt.Motion_Planner.Planner.Corner_Blender;
