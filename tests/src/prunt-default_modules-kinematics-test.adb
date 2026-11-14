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

with Trendy_Test; use Trendy_Test;
with Prunt.Motion_Planner;
with VSS.Strings.Conversions;

package body Prunt.Default_Modules.Kinematics.Test is

   use type Motion_Planner.Motion_Limits;

   None : constant Gcode_Optional_Float := (Present => False);
   No_JSON : constant Gcode_Optional_String := (Present => False);

   function Given (Value : Dimensionless) return Gcode_Optional_Float
   is ((Present => True, Value => Value));

   function Update
     (Current    : Motion_Planner.Motion_Limits;
      A, J, S, C : Gcode_Optional_Float := None;
      X, Y, Z, E : Gcode_Optional_String := No_JSON) return Motion_Planner.Motion_Limits
   is (Updated_Move_Limits (Current, A, J, S, C, X, Y, Z, E));

   procedure Test_Updated_Move_Limits (T : in out Trendy_Test.Operation'Class);

   procedure Test_Updated_Move_Limits (T : in out Trendy_Test.Operation'Class) is
      function UTF_8 (Item : String) return Virtual_String
      renames VSS.Strings.Conversions.To_Virtual_String;

      --  Each argument is built in place. A Virtual_String in the variant part of a record returned by a function
      --  can come back with a stale pointer to its in-place text.
      X_Caps         : constant Gcode_Optional_String :=
        (Present => True, Value => UTF_8 ("{""Acceleration"": 1500, ""Velocity"": 60.5}"));
      X_No_Accel     : constant Gcode_Optional_String :=
        (Present => True, Value => UTF_8 ("{""Acceleration"": 0}"));
      X_No_Velocity  : constant Gcode_Optional_String :=
        (Present => True, Value => UTF_8 ("{""Velocity"": 0}"));
      E_No_Caps      : constant Gcode_Optional_String :=
        (Present => True, Value => UTF_8 ("{""Jerk"": 0, ""Crackle"": 0}"));
      Unknown_Field  : constant Gcode_Optional_String :=
        (Present => True, Value => UTF_8 ("{""Accel"": 1}"));
      Not_A_Number   : constant Gcode_Optional_String :=
        (Present => True, Value => UTF_8 ("{""Acceleration"": ""fast""}"));
      Not_An_Object  : constant Gcode_Optional_String := (Present => True, Value => UTF_8 ("5"));
      Invalid_JSON   : constant Gcode_Optional_String :=
        (Present => True, Value => UTF_8 ("{""Acceleration"": "));
      Limits         : Motion_Planner.Motion_Limits := Motion_Planner.No_Motion_Limits;

      procedure Expect_Rejection
        (Name : String; A : Gcode_Optional_Float := None; X : Gcode_Optional_String := No_JSON);
      --  Check that these arguments are rejected.

      procedure Expect_Rejection
        (Name : String; A : Gcode_Optional_Float := None; X : Gcode_Optional_String := No_JSON) is
      begin
         Limits := Update (Limits, A => A, X => X);
         T.Assert (False, Name & " should be rejected");
      exception
         when Gcode_Bad_Inputs_Error =>
            null;
      end Expect_Rejection;
   begin
      T.Register;

      Limits := Update (Limits, A => Given (2_000.0), J => Given (100_000.0));
      T.Assert
        (Limits.Tangential_Acceleration_Max = 2_000.0 * mm / s**2
         and then Limits.Tangential_Jerk_Max = 100_000.0 * mm / s**3
         and then Limits.Tangential_Snap_Max = Motion_Planner.No_Motion_Limits.Tangential_Snap_Max
         and then Limits.Axial_Acceleration_Maxes = Motion_Planner.No_Motion_Limits.Axial_Acceleration_Maxes,
         "A and J cap the tangential acceleration and jerk, and nothing else");

      Limits := Update (Limits, X => X_Caps);
      T.Assert
        (Limits.Axial_Acceleration_Maxes (X_Axis) = 1_500.0 * mm / s**2
         and then Limits.Axial_Velocity_Maxes (X_Axis) = 60.5 * mm / s
         and then Limits.Axial_Acceleration_Maxes (Y_Axis)
                  = Motion_Planner.No_Motion_Limits.Axial_Acceleration_Maxes (Y_Axis)
         and then Limits.Tangential_Acceleration_Max = 2_000.0 * mm / s**2,
         "An axis object caps that axis, and caps that are not given keep their values");

      Limits := Update (Limits, J => Given (0.0), X => X_No_Accel);
      T.Assert
        (Limits.Tangential_Jerk_Max = Motion_Planner.No_Motion_Limits.Tangential_Jerk_Max
         and then Limits.Axial_Acceleration_Maxes (X_Axis)
                  = Motion_Planner.No_Motion_Limits.Axial_Acceleration_Maxes (X_Axis)
         and then Limits.Tangential_Acceleration_Max = 2_000.0 * mm / s**2
         and then Limits.Axial_Velocity_Maxes (X_Axis) = 60.5 * mm / s,
         "Zero removes a cap and leaves the others");

      Limits := Update (Limits, A => Given (0.0), X => X_No_Velocity, E => E_No_Caps);
      T.Assert (Limits = Motion_Planner.No_Motion_Limits, "Removing every cap gives no caps at all");

      Expect_Rejection ("No caps at all");
      Expect_Rejection ("A negative cap", A => Given (-1.0));
      Expect_Rejection ("A cap below 1E-6", A => Given (1.0E-7));
      Expect_Rejection ("An unknown field", X => Unknown_Field);
      Expect_Rejection ("A non-numeric cap", X => Not_A_Number);
      Expect_Rejection ("A JSON value that is not an object", X => Not_An_Object);
      Expect_Rejection ("Invalid JSON", X => Invalid_JSON);
      T.Assert (Limits = Motion_Planner.No_Motion_Limits, "Rejected arguments change nothing");
   end Test_Updated_Move_Limits;

   function All_Tests return Trendy_Test.Test_Group
   is ([Test_Updated_Move_Limits'Unrestricted_Access]);

end Prunt.Default_Modules.Kinematics.Test;
