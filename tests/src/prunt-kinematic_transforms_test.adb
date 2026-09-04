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

with Prunt.Kinematic_Transforms;
with Trendy_Test; use Trendy_Test;

package body Prunt.Kinematic_Transforms_Test is

   pragma Extensions_Allowed (On);

   type Test_Motor is (A_Motor, A_Follower, B_Motor, C_Motor, E_Motor);
   type Test_Motor_Position is array (Test_Motor) of Dimensionless;
   package Transforms is new Prunt.Kinematic_Transforms (Test_Motor, Test_Motor_Position);
   use Transforms;

   function Close (Left, Right : Dimensionless) return Boolean is
     (abs (Left - Right) <= 1.0E-9 * Dimensionless'Max (1.0, abs Right));

   function Standard_Delta return Kinematic_Transform is
     ((Kind         => Delta_Transform,
       Delta_Params =>
         (Towers              =>
            (Tower_A =>
               (X => 100.0 * mm, Y => 0.0 * mm, Arm_Length => 250.0 * mm),
             Tower_B =>
               (X => -50.0 * mm, Y => 86.602_540_378_443_86 * mm, Arm_Length => 251.0 * mm),
             Tower_C =>
               (X => -50.0 * mm, Y => -86.602_540_378_443_86 * mm, Arm_Length => 249.0 * mm)),
          Motors              =>
            (A_Motor => (Kind => Delta_Tower_Motor, Tower => Tower_A, Units_Per_Distance => 1.0 / mm),
             B_Motor => (Kind => Delta_Tower_Motor, Tower => Tower_B, Units_Per_Distance => -2.0 / mm),
             C_Motor => (Kind => Delta_Tower_Motor, Tower => Tower_C, Units_Per_Distance => 0.5 / mm),
             E_Motor => (Kind => Delta_Extruder_Motor, Units_Per_Distance => 4.0 / mm),
             others  => <>),
          Jacobian_Bounds     => [others => [others => 0.0 / mm]])));

   procedure Test_Delta_Centre_And_Reversed_Motor (T : in out Trendy_Test.Operation'Class) is
      Transform : constant Kinematic_Transform := Standard_Delta;
      Result    : constant Test_Motor_Position :=
        To_Motor_Position
          ([X_Axis => 0.0 * mm, Y_Axis => 0.0 * mm, Z_Axis => 10.0 * mm, E_Axis => 3.0 * mm], Transform);
      A_Height  : constant Length := 10.0 * mm + (250.0 ** 2 - 100.0 ** 2) ** (1 / 2) * mm;
      B_Height  : constant Length := 10.0 * mm + (251.0 ** 2 - 100.0 ** 2) ** (1 / 2) * mm;
      C_Height  : constant Length := 10.0 * mm + (249.0 ** 2 - 100.0 ** 2) ** (1 / 2) * mm;
   begin
      T.Register;
      T.Assert (Close (Result (A_Motor), A_Height / mm), "Tower A centre position");
      T.Assert (Close (Result (B_Motor), -2.0 * B_Height / mm), "reversed Tower B");
      T.Assert (Close (Result (C_Motor), 0.5 * C_Height / mm), "Tower C centre position");
      T.Assert (Close (Result (E_Motor), 12.0), "independent extruder");
   end Test_Delta_Centre_And_Reversed_Motor;

   procedure Test_Delta_Multiple_Extruder_Motors (T : in out Trendy_Test.Operation'Class) is
      Transform : Kinematic_Transform := Standard_Delta;
      Pos       : constant Position :=
        [X_Axis => 0.0 * mm, Y_Axis => 0.0 * mm, Z_Axis => 0.0 * mm, E_Axis => 2.0 * mm];
   begin
      T.Register;
      Transform.Delta_Params.Motors (A_Follower) :=
        (Kind => Delta_Extruder_Motor, Units_Per_Distance => -3.0 / mm);

      declare
         Motor_Pos  : constant Test_Motor_Position := To_Motor_Position (Pos, Transform);
         Planner_Map : constant Motor_Position_Map := Transform_Linear_Map (Transform);
         Round_Trip : constant Position := To_Cartesian_Position (Motor_Pos, Transform);
      begin
         T.Assert (Close (Motor_Pos (E_Motor), 8.0), "first extruder motor follows E");
         T.Assert (Close (Motor_Pos (A_Follower), -6.0), "second extruder motor follows E");
         T.Assert
           (Planner_Map (E_Axis, E_Motor) = 4.0 / mm
              and then Planner_Map (E_Axis, A_Follower) = -3.0 / mm,
            "all extruder motors are included in the linear projection");
         T.Assert
           (Motor_Is_In_Selective_Move_Group (Transform, E_Motor, A_Follower),
            "all delta extruder motors form one motion group");
         T.Assert (Close (Round_Trip (E_Axis) / mm, 2.0), "multiple-extruder round trip");
      end;
   end Test_Delta_Multiple_Extruder_Motors;

   procedure Test_Delta_Multiple_Motors_Per_Tower (T : in out Trendy_Test.Operation'Class) is
      Transform : Kinematic_Transform := Standard_Delta;
      Pos       : constant Position :=
        [X_Axis => 20.0 * mm, Y_Axis => -15.0 * mm, Z_Axis => 7.0 * mm, E_Axis => -2.0 * mm];
   begin
      T.Register;
      Transform.Delta_Params.Motors (A_Follower) :=
        (Kind => Delta_Tower_Motor, Tower => Tower_A, Units_Per_Distance => -3.0 / mm);

      declare
         Motor_Pos  : constant Test_Motor_Position := To_Motor_Position (Pos, Transform);
         Round_Trip : constant Position := To_Cartesian_Position (Motor_Pos, Transform);
      begin
         T.Assert
           (Close (Motor_Pos (A_Follower), -3.0 * Motor_Pos (A_Motor)),
            "both Tower A motors use the same carriage height");
         T.Assert
           (Motor_Is_In_Selective_Move_Group (Transform, A_Motor, A_Follower),
            "Tower A motors form one motion group");
         T.Assert
           (not Motor_Is_In_Selective_Move_Group (Transform, A_Motor, B_Motor),
            "different towers remain independent");
         for Axis in Axis_Name loop
            T.Assert (Close (Round_Trip (Axis) / mm, Pos (Axis) / mm), "multi-motor round trip " & Axis'Image);
         end loop;
      end;
   end Test_Delta_Multiple_Motors_Per_Tower;

   procedure Test_Delta_Off_Axis (T : in out Trendy_Test.Operation'Class) is
      Transform : constant Kinematic_Transform := Standard_Delta;
      Planner_Map : constant Motor_Position_Map := Transform_Linear_Map (Transform);
      Pos       : constant Position :=
        [X_Axis => 20.0 * mm, Y_Axis => -15.0 * mm, Z_Axis => 7.0 * mm, E_Axis => -2.0 * mm];
      Result    : constant Test_Motor_Position := To_Motor_Position (Pos, Transform);
      A_Height  : constant Length := 7.0 * mm + (250.0 ** 2 - 80.0 ** 2 - 15.0 ** 2) ** (1 / 2) * mm;
   begin
      T.Register;
      T.Assert (Close (Result (A_Motor), A_Height / mm), "off-axis Tower A");
      T.Assert (Close (Result (E_Motor), -8.0), "negative extrusion");
      T.Assert (not Axis_Is_Motor_Separable (Transform, X_Axis), "delta X is coupled");
      T.Assert (not Axis_Is_Motor_Separable (Transform, Z_Axis), "delta Z is coupled");
      T.Assert (Axis_Is_Motor_Separable (Transform, E_Axis), "delta E is separable");
      T.Assert
         (Planner_Map
           = Motor_Position_Map'
               (E_Axis => (E_Motor => 4.0 / mm, others => 0.0 / mm),
                others => (others => 0.0 / mm)),
         "delta planner projection contains only the independent extruder");
   end Test_Delta_Off_Axis;

   procedure Test_Delta_Round_Trip (T : in out Trendy_Test.Operation'Class) is
      Transform : constant Kinematic_Transform := Standard_Delta;
      Expected  : constant Position :=
        [X_Axis => 20.0 * mm, Y_Axis => -15.0 * mm, Z_Axis => 7.0 * mm, E_Axis => -2.0 * mm];
      Result    : constant Position := To_Cartesian_Position (To_Motor_Position (Expected, Transform), Transform);
   begin
      T.Register;
      for Axis in Axis_Name loop
         T.Assert (Close (Result (Axis) / mm, Expected (Axis) / mm), "delta round trip " & Axis'Image);
      end loop;
   end Test_Delta_Round_Trip;

   procedure Test_Delta_Unreachable (T : in out Trendy_Test.Operation'Class) is
      Raised : Boolean := False;
   begin
      T.Register;
      begin
         declare
            Ignore : constant Test_Motor_Position :=
              To_Motor_Position
                ([X_Axis => 500.0 * mm, Y_Axis => 0.0 * mm, Z_Axis => 0.0 * mm, E_Axis => 0.0 * mm],
                 Standard_Delta);
         begin
            pragma Unreferenced (Ignore);
         end;
      exception
         when Constraint_Error =>
            Raised := True;
      end;
      T.Assert (Raised, "unreachable delta position must be rejected");
   end Test_Delta_Unreachable;

   procedure Test_Linear_Regression (T : in out Trendy_Test.Operation'Class) is
      Transform : constant Kinematic_Transform :=
         (Kind       => Linear_Transform,
         Linear_Map =>
           (X_Axis => (A_Motor => 2.0 / mm, A_Follower => -7.0 / mm, others => 0.0 / mm),
            Y_Axis => (B_Motor => -3.0 / mm, others => 0.0 / mm),
            Z_Axis => (C_Motor => 4.0 / mm, others => 0.0 / mm),
            E_Axis => (E_Motor => 5.0 / mm, others => 0.0 / mm)));
      Result : constant Test_Motor_Position :=
        To_Motor_Position
          ([X_Axis => 1.0 * mm, Y_Axis => 2.0 * mm, Z_Axis => 3.0 * mm, E_Axis => 4.0 * mm], Transform);
      Round_Trip : constant Position := To_Cartesian_Position (Result, Transform);
   begin
      T.Register;
      T.Assert
        (Result = [A_Motor => 2.0, A_Follower => -7.0, B_Motor => -6.0, C_Motor => 12.0, E_Motor => 20.0],
         "linear transform regression");
      T.Assert
        ((for all Axis in Axis_Name => Close (Round_Trip (Axis) / mm, Dimensionless (Axis_Name'Pos (Axis) + 1))),
         "linear transform round trip");
      T.Assert (Transform_Is_Linear (Transform), "linear transform metadata");
      T.Assert
        (Motor_Is_In_Selective_Move_Group (Transform, A_Motor, A_Follower),
         "all Cartesian motors assigned to the selected axis form one group");
      T.Assert
        (not Motor_Is_In_Selective_Move_Group (Transform, A_Motor, B_Motor),
         "motors assigned to other Cartesian axes are excluded");
   end Test_Linear_Regression;

   procedure Test_Linear_Transform_Core_XY_Round_Trip (T : in out Trendy_Test.Operation'Class) is
      Transform : constant Kinematic_Transform :=
        (Kind       => Linear_Transform,
         Linear_Map =>
           (X_Axis => (A_Motor => 2.0 / mm, B_Motor => 3.0 / mm, others => 0.0 / mm),
            Y_Axis => (A_Motor => 2.0 / mm, B_Motor => -3.0 / mm, others => 0.0 / mm),
            Z_Axis => (C_Motor => 4.0 / mm, others => 0.0 / mm),
            E_Axis => (E_Motor => 5.0 / mm, others => 0.0 / mm)));
      Expected : constant Position :=
        [X_Axis => 13.0 * mm, Y_Axis => -7.0 * mm, Z_Axis => 2.0 * mm, E_Axis => 1.5 * mm];
      Result : constant Position := To_Cartesian_Position (To_Motor_Position (Expected, Transform), Transform);
   begin
      T.Register;
      for Axis in Axis_Name loop
         T.Assert (Close (Result (Axis) / mm, Expected (Axis) / mm), "CoreXY round trip " & Axis'Image);
      end loop;
      T.Assert
        (not Motor_Is_In_Selective_Move_Group (Transform, A_Motor, A_Motor),
         "CoreXY does not support motor-selective moves");
      T.Assert
        (not Motor_Is_In_Selective_Move_Group (Transform, C_Motor, C_Motor),
         "a coupled linear transform rejects motor-selective moves as a whole");
   end Test_Linear_Transform_Core_XY_Round_Trip;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Delta_Centre_And_Reversed_Motor'Unrestricted_Access,
         Test_Delta_Multiple_Motors_Per_Tower'Unrestricted_Access,
         Test_Delta_Multiple_Extruder_Motors'Unrestricted_Access,
         Test_Delta_Off_Axis'Unrestricted_Access,
         Test_Delta_Round_Trip'Unrestricted_Access,
         Test_Delta_Unreachable'Unrestricted_Access,
         Test_Linear_Regression'Unrestricted_Access,
         Test_Linear_Transform_Core_XY_Round_Trip'Unrestricted_Access];
   end All_Tests;

end Prunt.Kinematic_Transforms_Test;
