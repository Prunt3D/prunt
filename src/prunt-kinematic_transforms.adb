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

with Ada.Numerics.Generic_Elementary_Functions;

package body Prunt.Kinematic_Transforms is

   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   function Default_Transform return Kinematic_Transform
   is (Kind => Linear_Transform, others => <>);

   function To_Motor_Position (Pos : Position; Transform : Kinematic_Transform) return Motor_Position is
      Result : Motor_Position := [others => 0.0];
   begin
      case Transform.Kind is
         when Linear_Transform =>
            for Motor in Motor_Name loop
               for Axis in Axis_Name loop
                  Result (Motor) := Result (Motor) + Pos (Axis) * Transform.Linear_Map (Axis, Motor);
               end loop;
            end loop;

         when Delta_Transform  =>
            for Motor in Motor_Name loop
               declare
                  Motor_Params : Delta_Motor_Parameters renames Transform.Delta_Params.Motors (Motor);
               begin
                  case Motor_Params.Kind is
                     when Unused_Delta_Motor   =>
                        null;

                     when Delta_Tower_Motor    =>
                        declare
                           Params   : Delta_Tower_Parameters renames
                             Transform.Delta_Params.Towers (Motor_Params.Tower);
                           DX       : constant Length := Pos (X_Axis) - Params.X;
                           DY       : constant Length := Pos (Y_Axis) - Params.Y;
                           Radicand : constant Area := Params.Arm_Length ** 2 - DX ** 2 - DY ** 2;
                           Carriage : Length;
                        begin
                           if Radicand <= 0.0 * mm ** 2 then
                              raise Constraint_Error with "Delta position is outside the reachable workspace.";
                           end if;

                           Carriage := Pos (Z_Axis) + Radicand ** (1 / 2);
                           Result (Motor) := Carriage * Motor_Params.Units_Per_Distance;
                        end;

                     when Delta_Extruder_Motor =>
                        Result (Motor) := Pos (E_Axis) * Motor_Params.Units_Per_Distance;
                  end case;
               end;
            end loop;
      end case;

      return Result;
   end To_Motor_Position;

   function To_Cartesian_Position (Motor_Pos : Motor_Position; Transform : Kinematic_Transform) return Position is
      type Axis_Vector is array (Axis_Name) of Dimensionless;
      type Axis_Matrix is array (Axis_Name, Axis_Name) of Dimensionless;
      type Active_Axis_Array is array (Axis_Name) of Boolean;

      function Linear_To_Cartesian return Position;
      function Delta_To_Cartesian return Position;

      function Linear_To_Cartesian return Position is
         Active     : Active_Axis_Array := [others => False];
         Axis_Scale : Axis_Vector := [others => 0.0];
         Matrix     : Axis_Matrix := [others => [others => 0.0]];
         RHS        : Axis_Vector := [others => 0.0];
         Result     : Position := [others => 0.0 * mm];
      begin
         --  Normalize each represented axis before forming the normal equations. This keeps axes with very different
         --  motor-unit scales from making an otherwise well-conditioned transform appear singular.
         for Axis in Axis_Name loop
            for Motor in Motor_Name loop
               Axis_Scale (Axis) :=
                 Dimensionless'Max (Axis_Scale (Axis), abs (Transform.Linear_Map (Axis, Motor) * mm));
            end loop;
            Active (Axis) := Axis_Scale (Axis) /= 0.0;
         end loop;

         for Row in Axis_Name loop
            if Active (Row) then
               for Motor in Motor_Name loop
                  declare
                     Row_Coefficient : constant Dimensionless :=
                       Transform.Linear_Map (Row, Motor) * mm / Axis_Scale (Row);
                  begin
                     RHS (Row) := @ + Row_Coefficient * Motor_Pos (Motor);
                     for Column in Axis_Name loop
                        if Active (Column) then
                           Matrix (Row, Column) :=
                             @ + Row_Coefficient * (Transform.Linear_Map (Column, Motor) * mm / Axis_Scale (Column));
                        end if;
                     end loop;
                  end;
               end loop;
            end if;
         end loop;

         --  Gauss-Jordan elimination solves the normalized least-squares system. Redundant motors are supported, but
         --  a motor map which couples represented axes without enough independent equations has no unique inverse.
         for Pivot_Column in Axis_Name loop
            if Active (Pivot_Column) then
               declare
                  Pivot_Row       : Axis_Name := Pivot_Column;
                  Pivot_Magnitude : Dimensionless := abs Matrix (Pivot_Column, Pivot_Column);
                  Remaining_Scale : Dimensionless := 0.0;
               begin
                  for Candidate_Row in Axis_Name loop
                     if Active (Candidate_Row) and then Axis_Name'Pos (Candidate_Row) >= Axis_Name'Pos (Pivot_Column)
                     then
                        for Column in Axis_Name loop
                           if Active (Column) and then Axis_Name'Pos (Column) >= Axis_Name'Pos (Pivot_Column) then
                              Remaining_Scale :=
                                Dimensionless'Max (Remaining_Scale, abs Matrix (Candidate_Row, Column));
                           end if;
                        end loop;
                        if abs Matrix (Candidate_Row, Pivot_Column) > Pivot_Magnitude then
                           Pivot_Row := Candidate_Row;
                           Pivot_Magnitude := abs Matrix (Candidate_Row, Pivot_Column);
                        end if;
                     end if;
                  end loop;

                  if Pivot_Magnitude <= 128.0 * Dimensionless'Model_Epsilon * Remaining_Scale then
                     raise Constraint_Error with "Linear motor map does not have a unique Cartesian inverse.";
                  end if;

                  if Pivot_Row /= Pivot_Column then
                     for Column in Axis_Name loop
                        declare
                           Temporary : constant Dimensionless := Matrix (Pivot_Column, Column);
                        begin
                           Matrix (Pivot_Column, Column) := Matrix (Pivot_Row, Column);
                           Matrix (Pivot_Row, Column) := Temporary;
                        end;
                     end loop;
                     declare
                        Temporary : constant Dimensionless := RHS (Pivot_Column);
                     begin
                        RHS (Pivot_Column) := RHS (Pivot_Row);
                        RHS (Pivot_Row) := Temporary;
                     end;
                  end if;

                  declare
                     Pivot : constant Dimensionless := Matrix (Pivot_Column, Pivot_Column);
                  begin
                     for Column in Axis_Name loop
                        if Active (Column) then
                           Matrix (Pivot_Column, Column) := @ / Pivot;
                        end if;
                     end loop;
                     RHS (Pivot_Column) := @ / Pivot;
                  end;

                  for Row in Axis_Name loop
                     if Active (Row) and then Row /= Pivot_Column then
                        declare
                           Factor : constant Dimensionless := Matrix (Row, Pivot_Column);
                        begin
                           for Column in Axis_Name loop
                              if Active (Column) then
                                 Matrix (Row, Column) := @ - Factor * Matrix (Pivot_Column, Column);
                              end if;
                           end loop;
                           RHS (Row) := @ - Factor * RHS (Pivot_Column);
                        end;
                     end if;
                  end loop;
               end;
            end if;
         end loop;

         for Axis in Axis_Name loop
            if Active (Axis) then
               Result (Axis) := RHS (Axis) / Axis_Scale (Axis) * mm;
            end if;
         end loop;
         return Result;
      end Linear_To_Cartesian;

      function Delta_To_Cartesian return Position is
         subtype Component is Axis_Name range X_Axis .. Z_Axis;
         type Vector is array (Component) of Dimensionless;

         function "+" (Left, Right : Vector) return Vector
         is [for Axis in Component => Left (Axis) + Right (Axis)];

         function "-" (Left, Right : Vector) return Vector
         is [for Axis in Component => Left (Axis) - Right (Axis)];

         function "*" (Left : Dimensionless; Right : Vector) return Vector
         is [for Axis in Component => Left * Right (Axis)];

         function Dot (Left, Right : Vector) return Dimensionless
         is (Left (X_Axis) * Right (X_Axis) + Left (Y_Axis) * Right (Y_Axis) + Left (Z_Axis) * Right (Z_Axis));

         function Norm (Value : Vector) return Dimensionless
         is (Dimensionless_Math.Sqrt (Dot (Value, Value)));

         function Cross (Left, Right : Vector) return Vector
         is [X_Axis => Left (Y_Axis) * Right (Z_Axis) - Left (Z_Axis) * Right (Y_Axis),
             Y_Axis => Left (Z_Axis) * Right (X_Axis) - Left (X_Axis) * Right (Z_Axis),
             Z_Axis => Left (X_Axis) * Right (Y_Axis) - Left (Y_Axis) * Right (X_Axis)];

         function Centre (Tower : Delta_Tower_Name) return Vector;

         function Centre (Tower : Delta_Tower_Name) return Vector is
         begin
            for Motor in Motor_Name loop
               declare
                  Motor_Params : Delta_Motor_Parameters renames Transform.Delta_Params.Motors (Motor);
                  Params       : Delta_Tower_Parameters renames Transform.Delta_Params.Towers (Tower);
               begin
                  if Motor_Params.Kind = Delta_Tower_Motor and then Motor_Params.Tower = Tower then
                     if Motor_Params.Units_Per_Distance = 0.0 / mm then
                        raise Constraint_Error with "A delta tower motor has zero motor units per distance.";
                     end if;
                     return
                       [X_Axis => Params.X / mm,
                        Y_Axis => Params.Y / mm,
                        Z_Axis => Motor_Pos (Motor) / (Motor_Params.Units_Per_Distance * mm)];
                  end if;
               end;
            end loop;
            raise Constraint_Error with "A delta tower has no assigned motor.";
         end Centre;

         P_1       : constant Vector := Centre (Tower_A);
         P_2       : constant Vector := Centre (Tower_B);
         P_3       : constant Vector := Centre (Tower_C);
         P_2_Minus : constant Vector := P_2 - P_1;
         D         : constant Dimensionless := Norm (P_2_Minus);
         E_X       : Vector;
         P_3_Minus : constant Vector := P_3 - P_1;
         I         : Dimensionless;
         Remainder : Vector;
         J         : Dimensionless;
         E_Y       : Vector;
         E_Z       : Vector;
         X         : Dimensionless;
         Y         : Dimensionless;
         Z_Squared : Dimensionless;
         Base      : Vector;
         First     : Vector;
         Second    : Vector;
         Selected  : Vector;
         Result    : Position := [others => 0.0 * mm];
         R_1       : constant Dimensionless := Transform.Delta_Params.Towers (Tower_A).Arm_Length / mm;
         R_2       : constant Dimensionless := Transform.Delta_Params.Towers (Tower_B).Arm_Length / mm;
         R_3       : constant Dimensionless := Transform.Delta_Params.Towers (Tower_C).Arm_Length / mm;
      begin
         if D = 0.0 then
            raise Constraint_Error with "Delta forward kinematics has coincident carriage centres.";
         end if;
         E_X := (1.0 / D) * P_2_Minus;
         I := Dot (E_X, P_3_Minus);
         Remainder := P_3_Minus - I * E_X;
         J := Norm (Remainder);
         if J = 0.0 then
            raise Constraint_Error with "Delta forward kinematics has collinear carriage centres.";
         end if;
         E_Y := (1.0 / J) * Remainder;
         E_Z := Cross (E_X, E_Y);

         X := (R_1 ** 2 - R_2 ** 2 + D ** 2) / (2.0 * D);
         Y := (R_1 ** 2 - R_3 ** 2 + I ** 2 + J ** 2 - 2.0 * I * X) / (2.0 * J);
         Z_Squared := R_1 ** 2 - X ** 2 - Y ** 2;
         if Z_Squared <= 0.0 then
            raise Constraint_Error with "Delta carriage positions have no nonsingular Cartesian solution.";
         end if;

         Base := P_1 + X * E_X + Y * E_Y;
         First := Base + Dimensionless_Math.Sqrt (Z_Squared) * E_Z;
         Second := Base - Dimensionless_Math.Sqrt (Z_Squared) * E_Z;
         Selected := (if First (Z_Axis) < Second (Z_Axis) then First else Second);
         for Axis in Component loop
            Result (Axis) := Selected (Axis) * mm;
         end loop;
         for Motor in Motor_Name loop
            if Transform.Delta_Params.Motors (Motor).Kind = Delta_Extruder_Motor then
               if Transform.Delta_Params.Motors (Motor).Units_Per_Distance = 0.0 / mm then
                  raise Constraint_Error with "Delta extruder has zero motor units per distance.";
               end if;
               Result (E_Axis) := Motor_Pos (Motor) / Transform.Delta_Params.Motors (Motor).Units_Per_Distance;
               exit;
            end if;
         end loop;
         return Result;
      end Delta_To_Cartesian;
   begin
      case Transform.Kind is
         when Linear_Transform =>
            return Linear_To_Cartesian;

         when Delta_Transform  =>
            return Delta_To_Cartesian;
      end case;
   end To_Cartesian_Position;

   function Transform_Is_Linear (Transform : Kinematic_Transform) return Boolean
   is (Transform.Kind = Linear_Transform);

   function Transform_Linear_Map (Transform : Kinematic_Transform) return Motor_Position_Map
   is (if Transform.Kind = Linear_Transform
       then Transform.Linear_Map
       else
         [for Axis in Axis_Name =>
            [for Motor in Motor_Name =>
               (if Axis = E_Axis and then Transform.Delta_Params.Motors (Motor).Kind = Delta_Extruder_Motor
                then Transform.Delta_Params.Motors (Motor).Units_Per_Distance
                else 0.0 / mm)]]);

   function Motor_Affects_Axis (Transform : Kinematic_Transform; Motor : Motor_Name; Axis : Axis_Name) return Boolean
   is
   begin
      case Transform.Kind is
         when Linear_Transform =>
            return Transform.Linear_Map (Axis, Motor) /= 0.0 / mm;

         when Delta_Transform  =>
            if Axis = E_Axis then
               return Transform.Delta_Params.Motors (Motor).Kind = Delta_Extruder_Motor;
            end if;

            return Transform.Delta_Params.Motors (Motor).Kind = Delta_Tower_Motor;
      end case;
   end Motor_Affects_Axis;

   function Motor_Is_In_Selective_Move_Group
     (Transform : Kinematic_Transform; Selected_Motor, Candidate_Motor : Motor_Name) return Boolean is
   begin
      case Transform.Kind is
         when Linear_Transform =>
            for Motor in Motor_Name loop
               declare
                  Affected_Axis_Count : Natural := 0;
               begin
                  for Axis in Axis_Name loop
                     if Transform.Linear_Map (Axis, Motor) /= 0.0 / mm then
                        Affected_Axis_Count := @ + 1;
                     end if;
                  end loop;
                  if Affected_Axis_Count > 1 then
                     return False;
                  end if;
               end;
            end loop;

            for Axis in Axis_Name loop
               if Transform.Linear_Map (Axis, Selected_Motor) /= 0.0 / mm then
                  return Transform.Linear_Map (Axis, Candidate_Motor) /= 0.0 / mm;
               end if;
            end loop;
            return False;

         when Delta_Transform  =>
            case Transform.Delta_Params.Motors (Selected_Motor).Kind is
               when Unused_Delta_Motor   =>
                  return False;

               when Delta_Tower_Motor    =>
                  return
                    Transform.Delta_Params.Motors (Candidate_Motor).Kind = Delta_Tower_Motor
                    and then
                      Transform.Delta_Params.Motors (Selected_Motor).Tower
                      = Transform.Delta_Params.Motors (Candidate_Motor).Tower;

               when Delta_Extruder_Motor =>
                  return Transform.Delta_Params.Motors (Candidate_Motor).Kind = Delta_Extruder_Motor;
            end case;
      end case;
   end Motor_Is_In_Selective_Move_Group;

   function Axis_Is_Motor_Separable (Transform : Kinematic_Transform; Axis : Axis_Name) return Boolean is
   begin
      for Motor in Motor_Name loop
         if Motor_Affects_Axis (Transform, Motor, Axis) then
            for Other_Axis in Axis_Name loop
               if Other_Axis /= Axis and then Motor_Affects_Axis (Transform, Motor, Other_Axis) then
                  return False;
               end if;
            end loop;
         end if;
      end loop;

      return True;
   end Axis_Is_Motor_Separable;

   function Conservative_Jacobian_Bounds (Transform : Kinematic_Transform) return Motor_Jacobian_Bounds is
      Result : Motor_Jacobian_Bounds := [others => [others => 0.0 / mm]];
   begin
      case Transform.Kind is
         when Linear_Transform =>
            for Motor in Motor_Name loop
               for Axis in Axis_Name loop
                  Result (Motor, Axis) := abs Transform.Linear_Map (Axis, Motor);
               end loop;
            end loop;

         when Delta_Transform  =>
            Result := Transform.Delta_Params.Jacobian_Bounds;
      end case;

      return Result;
   end Conservative_Jacobian_Bounds;

end Prunt.Kinematic_Transforms;
