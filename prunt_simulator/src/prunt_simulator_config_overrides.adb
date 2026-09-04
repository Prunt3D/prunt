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

with Prunt;      use Prunt;
with Prunt.JSON; use Prunt.JSON;

package body Prunt_Simulator_Config_Overrides is

   use Prunt.Config;

   procedure Add
     (Result : in out Config_Override_Vectors.Vector;
      Owner  : String;
      Path   : Config_Data_Paths.Vector;
      Value  : JSON_Value);

   procedure Add
     (Result : in out Config_Override_Vectors.Vector;
      Owner  : String;
      Path   : Config_Data_Paths.Vector;
      Value  : JSON_Value) is
   begin
      Result.Append (Config_Override'(Owner => +Owner, Path => Path, Value => Value));
   end Add;

   procedure Add_Homing_Axis (Result : in out Config_Override_Vectors.Vector; Axis : String);

   procedure Add_Homing_Axis (Result : in out Config_Override_Vectors.Vector; Axis : String) is
   begin
      Add
        (Result,
         "Homing",
         Config_Data_Paths.Vector'
           (["Homing", "Axes", +Axis, "Homing_Method", "Kind", "Selected"]),
         Create (+"Set_To_Value"));
   end Add_Homing_Axis;

   procedure Add_Motor (Result : in out Config_Override_Vectors.Vector; Motor : String);

   procedure Add_Motor (Result : in out Config_Override_Vectors.Vector; Motor : String) is
   begin
      Add
        (Result,
         "Motor Drivers",
         Config_Data_Paths.Vector'(["Motors", +Motor, "Enabled"]),
         Create (True));
      Add
        (Result,
         "Motor Drivers",
         Config_Data_Paths.Vector'
           (["Motors",
             +Motor,
             "Motion_Units",
             "Kind",
             "Children",
             "Direct_Entry",
             "Direct_Entry",
             "Distance_Per_Rotation"]),
         Create (Long_Float'(1.0)));
   end Add_Motor;

   procedure Add_Axis_Float
     (Result : in out Config_Override_Vectors.Vector;
      Field  : String;
      Axis   : String;
      Value  : Long_Float);

   procedure Add_Axis_Float
     (Result : in out Config_Override_Vectors.Vector;
      Field  : String;
      Axis   : String;
      Value  : Long_Float) is
   begin
      Add
        (Result,
         "Kinematics",
         Config_Data_Paths.Vector'(["Kinematics", +Field, +Axis]),
         Create (Value));
   end Add_Axis_Float;

   procedure Add_Kinematics_Float
     (Result : in out Config_Override_Vectors.Vector; Field : String; Value : Long_Float);

   procedure Add_Kinematics_Float
     (Result : in out Config_Override_Vectors.Vector; Field : String; Value : Long_Float) is
   begin
      Add
        (Result,
         "Kinematics",
         Config_Data_Paths.Vector'(["Kinematics", +Field]),
         Create (Value));
   end Add_Kinematics_Float;

   procedure Add_Cartesian_Motor
     (Result : in out Config_Override_Vectors.Vector;
      Motor  : String;
      Axis   : String);

   procedure Add_Cartesian_Motor
     (Result : in out Config_Override_Vectors.Vector;
      Motor  : String;
      Axis   : String) is
   begin
      Add
        (Result,
         "Kinematics",
         Config_Data_Paths.Vector'
           (["Kinematics", "Kinematics_Kind", "Kind", "Children", "Cartesian", "Cartesian", +Motor]),
         Create (+Axis));
   end Add_Cartesian_Motor;

   function Overrides return Config_Override_Vectors.Vector is
      Result : Config_Override_Vectors.Vector;
   begin
      --  Add
      --    (Result,
      --     "Basic Config",
      --     Config_Data_Paths.Vector'(["Prunt", "Enabled"]),
      --     Create (True));

      --  Add_Homing_Axis (Result, "X_AXIS");
      --  Add_Homing_Axis (Result, "Y_AXIS");
      --  Add_Homing_Axis (Result, "Z_AXIS");

      --  Add_Motor (Result, "X_MOTOR");
      --  Add_Motor (Result, "Y_MOTOR");
      --  Add_Motor (Result, "Z_MOTOR");
      --  Add_Motor (Result, "E_MOTOR");

      --  Add_Kinematics_Float (Result, "Maximum_Tangential_Velocity", 250.0);
      --  Add_Axis_Float (Result, "Axial_Velocity_Limits", "X_AXIS", 250.0);
      --  Add_Axis_Float (Result, "Axial_Velocity_Limits", "Y_AXIS", 250.0);
      --  Add_Axis_Float (Result, "Axial_Velocity_Limits", "Z_AXIS", 25.0);
      --  Add_Axis_Float (Result, "Axial_Velocity_Limits", "E_AXIS", 80.0);
      --  for Axis in Axis_Name loop
      --     Add_Axis_Float (Result, "Axial_Acceleration_Limits", Axis'Image, 5_000.0);
      --     Add_Axis_Float (Result, "Axial_Jerk_Limits", Axis'Image, 500_000.0);
      --     Add_Axis_Float (Result, "Axial_Snap_Limits", Axis'Image, 500_000_000.0);
      --     Add_Axis_Float (Result, "Axial_Crackle_Limits", Axis'Image, 500_000_000_000.0);
      --  end loop;
      --  Add_Cartesian_Motor (Result, "X_MOTOR", "X_AXIS");
      --  Add_Cartesian_Motor (Result, "Y_MOTOR", "Y_AXIS");
      --  Add_Cartesian_Motor (Result, "Z_MOTOR", "Z_AXIS");
      --  Add_Cartesian_Motor (Result, "E_MOTOR", "E_AXIS");

      return Result;
   end Overrides;

end Prunt_Simulator_Config_Overrides;
