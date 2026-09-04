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

generic
   type Motor_Name is (<>);
   type Motor_Position is array (Motor_Name) of Dimensionless;
package Prunt.Kinematic_Transforms is

   type Motor_Position_Map is array (Axis_Name, Motor_Name) of Curvature;
   type Motor_Jacobian_Bounds is array (Motor_Name, Axis_Name) of Curvature;

   type Delta_Tower_Name is (Tower_A, Tower_B, Tower_C);

   type Delta_Tower_Parameters is record
      X          : Length := 0.0 * mm;
      Y          : Length := 0.0 * mm;
      Arm_Length : Length := 1.0 * mm;
   end record;

   type Delta_Motor_Kind is (Unused_Delta_Motor, Delta_Tower_Motor, Delta_Extruder_Motor);

   type Delta_Motor_Parameters (Kind : Delta_Motor_Kind := Unused_Delta_Motor) is record
      Units_Per_Distance : Curvature := 0.0 / mm;
      case Kind is
         when Delta_Tower_Motor =>
            Tower : Delta_Tower_Name := Delta_Tower_Name'First;

         when Unused_Delta_Motor | Delta_Extruder_Motor =>
            null;
      end case;
   end record;

   type Delta_Tower_Parameter_Array is array (Delta_Tower_Name) of Delta_Tower_Parameters;
   type Delta_Motor_Parameter_Array is array (Motor_Name) of Delta_Motor_Parameters;

   type Delta_Parameters is record
      Towers          : Delta_Tower_Parameter_Array := [others => <>];
      Motors          : Delta_Motor_Parameter_Array := [others => <>];
      Jacobian_Bounds : Motor_Jacobian_Bounds := [others => [others => 0.0 / mm]];
   end record;

   type Kinematic_Transform_Kind is (Linear_Transform, Delta_Transform);

   type Kinematic_Transform (Kind : Kinematic_Transform_Kind := Linear_Transform) is record
      case Kind is
         when Linear_Transform =>
            Linear_Map : Motor_Position_Map := [others => [others => 0.0 / mm]];

         when Delta_Transform =>
            Delta_Params : Delta_Parameters;
      end case;
   end record;

   function Default_Transform return Kinematic_Transform;

   function To_Motor_Position (Pos : Position; Transform : Kinematic_Transform) return Motor_Position;
   --  Convert a Cartesian/tool-space position to absolute motor coordinates.

   function To_Cartesian_Position (Motor_Pos : Motor_Position; Transform : Kinematic_Transform) return Position;
   --  Convert absolute motor coordinates to Cartesian/tool-space coordinates. A linear configuration may omit an
   --  axis, most commonly E when no extruder motor is installed. Its motor coordinates then contain no information
   --  about that axis, so the inverse returns the canonical value zero. If represented linear axes are not
   --  independently recoverable, the function raises Constraint_Error instead of choosing an arbitrary solution.
   --
   --  Three delta carriage joints define two mathematical intersections of their arm-length spheres, mirrored across
   --  the plane through those joints. A classic vertical delta places the nozzle below that plane, so the solution
   --  with the lower Cartesian Z coordinate is returned.

   function Axis_Is_Motor_Separable (Transform : Kinematic_Transform; Axis : Axis_Name) return Boolean;
   --  Return whether every motor affected by Axis is unaffected by all other axes.

   function Motor_Affects_Axis (Transform : Kinematic_Transform; Motor : Motor_Name; Axis : Axis_Name) return Boolean;

   function Motor_Is_In_Selective_Move_Group
     (Transform : Kinematic_Transform; Selected_Motor, Candidate_Motor : Motor_Name) return Boolean;
   --  Return whether Candidate_Motor belongs to Selected_Motor's independently movable group. For a linear transform,
   --  motor-selective moves are supported only when every used motor affects at most one Cartesian axis; the group is
   --  then every motor affecting Selected_Motor's axis. This admits Cartesian transforms, including axes driven by
   --  multiple motors. For CoreXY and other coupled linear transforms, return False for every Candidate_Motor,
   --  including Selected_Motor itself. For a delta transform, a tower motor's group contains every motor assigned to
   --  that tower, while an extruder motor's group contains every motor assigned to E. Also return False for every
   --  Candidate_Motor when Selected_Motor is unused.

   function Conservative_Jacobian_Bounds (Transform : Kinematic_Transform) return Motor_Jacobian_Bounds;
   --  Return component-wise upper bounds on the absolute motor-position Jacobian.

   function Transform_Is_Linear (Transform : Kinematic_Transform) return Boolean;
   function Transform_Linear_Map (Transform : Kinematic_Transform) return Motor_Position_Map;
   --  Return the exact map for linear kinematics. For delta kinematics only the independent E mapping is returned.

end Prunt.Kinematic_Transforms;
