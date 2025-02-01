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

private with Ada.Containers.Indefinite_Ordered_Maps;

package Prunt.Input_Shapers.Shapers is

   type Shaper (Input_Offset : Cycle_Count; Extra_End_Time : Cycle_Count) is abstract tagged null record;
   --  New shapers are created for every block. This can be switched to a controlled type if resource deallocation is
   --  required for a future addition, although that should not be required as Axial_Shapers places these on the heap,
   --  meaning that it is fine to have a very large record if required.
   --
   --  The inputs to Do_Step will be delayed by Input_Offset cycles. The applied offset should result in the predicted
   --  movement of the axis matching as closely as possible to the movement of an ideal axis without shaping, this
   --  allows axes with different shaper parameters to stay synchronised with each other. This value can be (and
   --  usually should be) a negative value to receive inputs that are a given number of cycles in to the future. Note
   --  that Input_Offset is relative to offsets requested by other shapers, so requesting an offset of 1000 will not
   --  guarantee 1000 repeats of the initial position if all other shapers also request an offset of 1000.
   --
   --  After all moves are complete, Do_Step will be called Extra_End_Time times with a repeat of the finishing
   --  position to allow for flushing of any remaining outputs that have been buffered.

   function Do_Step (This : in out Shaper; Step : Length) return Length is abstract;
   --  Receive a new input position and return the shaped output. The input position is an absolute value, not a delta.
   --
   --  If part of the output if in the future then Extra_End_Time should be used to guarantee that this function will
   --  be called enough times for the full output to be handled.
   --
   --  If the predicted motion is delayed compared to an ideal axis then Input_Offset should be used to apply a
   --  negative delay to the inputs.
   --
   --  This function should take approximately constant time as it is called in the realtime part of the step
   --  generator. All required setup should be performed during instantiation of the shaper, before this function is
   --  called.

   -----------

   --  Each shaper should implement a function similar to function similar to:
   --
   --  Create
   --  (Parameters : Shaper_Parameters; Interpolation_Time : Time; Initial_Position : Length) return Shaper'Class;
   --
   --  A call to this function should be added to the case statement inside Create (...) return Axial_Shapers.
   --
   --  The Parameters parameter contains the most recent shaper parameters as requested by the user.
   --
   --  The Interpolation_Time parameter is the time between each position passed to Do_Step.
   --
   --  The Initial_Position parameter is the position that the axis is currently stationary at. This value may be used
   --  to prefill internal buffers in the shaper.
   --
   --  If the parameters are for the wrong shaper type then a Constraint_Error should be raised. The code to do this
   --  should use an explicit raise statement and should not just use a precondition or assertion as we do not want
   --  this check to be removed in release builds. A precondition should be used in addition to this to get a
   --  compile-time warning where possible.
   --
   --  This function is only called when the machine is in an idle state so an arbitrarily long setup time is allowed.

   type Axial_Shapers is private;

   function Create
     (Parameters         : Axial_Shaper_Parameters;
      Interpolation_Time : Time;
      Initial_Position   : Position)
      return Axial_Shapers;

   function Do_Step (Shapers : in out Axial_Shapers; Step : Position) return Position;

   function Extra_End_Steps_Required (Shapers : Axial_Shapers) return Cycle_Count;

private

   type Input_Buffer_Array is array (Cycle_Count range <>) of Length;

   type Input_Buffer (Length : Cycle_Count) is record
      Buffer        : Input_Buffer_Array (0 .. Length);
      Current_Index : Cycle_Count;
   end record;

   package Axial_Shaper_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => Axis_Name, Element_Type => Shaper'Class);

   package Axial_Input_Buffer_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => Axis_Name, Element_Type => Input_Buffer);

   type Axial_Shapers is record
      Shapers        : Axial_Shaper_Maps.Map;
      Buffers        : Axial_Input_Buffer_Maps.Map;
      Extra_End_Time : Cycle_Count;
   end record;

end Prunt.Input_Shapers.Shapers;
