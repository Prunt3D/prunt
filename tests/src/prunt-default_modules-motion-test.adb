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

with Ada.Exceptions;
with Ada.Numerics;
with Ada.Strings.Fixed;

package body Prunt.Default_Modules.Motion.Test is

   procedure Test_Excessive_Extrusion_Arcs (T : in out Trendy_Test.Operation'Class);
   procedure Test_Excessive_Extrusion_Defaults_And_Limits (T : in out Trendy_Test.Operation'Class);
   procedure Test_Excessive_Extrusion_Linear_Moves (T : in out Trendy_Test.Operation'Class);
   procedure Test_Excessive_Extrusion_Recovery_And_Pause (T : in out Trendy_Test.Operation'Class);

   type Mock_Planner_Data is record
      Pos      : Position := [others => 0.0 * mm];
      Count    : Planner_Corner_ID := 0;
      Helices  : Natural := 0;
      Feedrate : Velocity := 0.0 * mm / s;
   end record;

   type Mock_Planner is new Planner_Interface with record
      Data : access Mock_Planner_Data;
   end record;

   overriding
   function Get_Last_Position (This : Mock_Planner) return Position is (This.Data.Pos);
   overriding
   function Get_Last_Kinematic_Parameters (This : Mock_Planner) return Motion_Planner.Kinematic_Parameters
   is ((Bounds =>
          (Kind => Motion_Planner.Rectangular_Workspace,
           Lower_X | Lower_Y | Lower_Z => -1000.0 * mm,
           Upper_X | Upper_Y | Upper_Z => 1000.0 * mm,
           Lower_E => -1.0E100 * mm, Upper_E => 1.0E100 * mm), others => <>));
   overriding
   function Get_State_Anchor_Corner_ID (This : Mock_Planner) return Planner_Corner_ID is (This.Data.Count);
   overriding
   function Get_Last_Executed_Corner_ID (This : Mock_Planner) return Planner_Corner_ID is (This.Data.Count);
   overriding
   procedure Mark_Axis_Homed (This : Mock_Planner; Axis : Axis_Name) is null;
   overriding
   procedure Mark_Axis_Unhomed (This : Mock_Planner; Axis : Axis_Name) is null;
   overriding
   function Axis_Is_Homed (This : Mock_Planner; Axis : Axis_Name) return Boolean is (True);
   overriding
   function Cancellation_Is_Active (This : Mock_Planner) return Boolean is (False);
   overriding
   procedure Add_Corner
     (This : Mock_Planner; Pos : Position; Feedrate : Velocity; Dwell_After : Time := 0.0 * s;
      Require_Homed : Boolean := True);
   overriding
   procedure Add_Helix
     (This : Mock_Planner; Center : Position; Pos : Position; Clockwise : Boolean; Feedrate : Velocity;
      Dwell_After : Time := 0.0 * s; Require_Homed : Boolean := True);
   overriding
   procedure Add_Corner_Data (This : Mock_Planner; Corner_Data : Extra_Corner_Data'Class) is null;
   overriding
   procedure Flush
     (This : Mock_Planner;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record)) is null;
   overriding
   procedure Resolve_Homing_Move (This : Mock_Planner; Stopped_Position : Position) is null;
   overriding
   procedure Flush_And_Change_Kinematic_Parameters
     (This : Mock_Planner; Params : Motion_Planner.Kinematic_Parameters;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record)) is null;
   overriding
   procedure Flush_And_Reset_Position
     (This : Mock_Planner; New_Position : Position;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record)) is null;

   overriding
   procedure Add_Corner
     (This : Mock_Planner; Pos : Position; Feedrate : Velocity; Dwell_After : Time := 0.0 * s;
      Require_Homed : Boolean := True)
   is
      pragma Unreferenced (Dwell_After, Require_Homed);
   begin
      This.Data.Pos := Pos;
      This.Data.Count := @ + 1;
      This.Data.Feedrate := Feedrate;
   end Add_Corner;

   overriding
   procedure Add_Helix
     (This : Mock_Planner; Center : Position; Pos : Position; Clockwise : Boolean; Feedrate : Velocity;
      Dwell_After : Time := 0.0 * s; Require_Homed : Boolean := True)
   is
      pragma Unreferenced (Center, Clockwise);
   begin
      This.Data.Helices := @ + 1;
      This.Add_Corner (Pos, Feedrate, Dwell_After, Require_Homed);
   end Add_Helix;

   type Mock_Pause_Context is new Pause_Context with record
      Pos : Position := [others => 0.0 * mm];
   end record;

   overriding
   function Get_Pause_Position (This : Mock_Pause_Context) return Position is (This.Pos);
   overriding
   function Get_Last_Command_Index (This : Mock_Pause_Context) return Command_Index is (0);

   function Create_Instance
     (Settings : User_Config; Status : Status_Manager.Status_Emitter)
      return My_Modules.Module_Instance_Shared_Pointers.Ref;

   function Create_Instance
     (Settings : User_Config; Status : Status_Manager.Status_Emitter)
      return My_Modules.Module_Instance_Shared_Pointers.Ref
   is
      Data : Config.Config_Data;

      function Create return My_Modules.Module_Instance_Parent'Class;

      function Create return My_Modules.Module_Instance_Parent'Class is
      begin
         return Result : Module_Instance do
            Result.Initialize (Settings, Data, Status);
         end return;
      end Create;
   begin
      return Result : My_Modules.Module_Instance_Shared_Pointers.Ref do
         Result.Set (Create'Access);
      end return;
   end Create_Instance;

   function Linear_Rejected
     (Instance : My_Modules.Module_Instance_Shared_Pointers.Ref; Planner : Mock_Planner; E : Dimensionless;
      X : Gcode_Optional_Float := (Present => False); Y : Gcode_Optional_Float := (Present => False);
      Z : Gcode_Optional_Float := (Present => False); F : Gcode_Optional_Float := (Present => False);
      Rapid : Boolean := False) return Boolean;

   function Linear_Rejected
     (Instance : My_Modules.Module_Instance_Shared_Pointers.Ref; Planner : Mock_Planner; E : Dimensionless;
      X : Gcode_Optional_Float := (Present => False); Y : Gcode_Optional_Float := (Present => False);
      Z : Gcode_Optional_Float := (Present => False); F : Gcode_Optional_Float := (Present => False);
      Rapid : Boolean := False) return Boolean is
   begin
      Module_Instance (Instance.Get.Element.all).Execute_Linear_Move
        (Planner, X, Y, Z, (Present => True, Value => E), F, Rapid);
      return False;
   exception
      when Error : Gcode_Bad_Inputs_Error =>
         return Ada.Strings.Fixed.Index (Ada.Exceptions.Exception_Message (Error), "Excessive extrusion") /= 0;
   end Linear_Rejected;

   procedure Test_Excessive_Extrusion_Arcs (T : in out Trendy_Test.Operation'Class) is
      Device : constant Module := (My_Modules.Module with null record);
      Status : constant Status_Manager.Status_Data_Collection :=
        Status_Manager.Build_Collection (["Motion" => Device.Status_Schema]);
      Settings : User_Config;
      Data : aliased Mock_Planner_Data;
      Planner : constant Mock_Planner := (Data => Data'Unchecked_Access);
      Instance : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Missing : constant Gcode_Optional_Float := (Present => False);
      Start_Pos : constant Position := [X_Axis => 10.0 * mm, others => 0.0 * mm];
      Finish_Pos : constant Position := [Y_Axis => 10.0 * mm, others => 0.0 * mm];
      Center : constant Position := [others => 0.0 * mm];

      function Arc_Rejected
        (E : Dimensionless; Full_Circle : Boolean := False; Clockwise : Boolean := False;
         Radius_Form : Boolean := False; Radius : Dimensionless := 10.0;
         Finish_X : Dimensionless := 0.0; Finish_Y : Dimensionless := 10.0) return Boolean;

      function Arc_Rejected
        (E : Dimensionless; Full_Circle : Boolean := False; Clockwise : Boolean := False;
         Radius_Form : Boolean := False; Radius : Dimensionless := 10.0;
         Finish_X : Dimensionless := 0.0; Finish_Y : Dimensionless := 10.0) return Boolean is
      begin
         Module_Instance (Instance.Get.Element.all).Execute_Arc_Move
           (Planner, (True, (if Full_Circle then 10.0 else Finish_X)),
            (True, (if Full_Circle then 0.0 else Finish_Y)), Missing, (True, E), (True, 600.0),
            Clockwise => Clockwise, Offset_Form => not Radius_Form, I => (True, -10.0), J => Missing, R => Radius);
         return False;
      exception
         when Error : Gcode_Bad_Inputs_Error =>
            return Ada.Strings.Fixed.Index (Ada.Exceptions.Exception_Message (Error), "Excessive extrusion") /= 0;
      end Arc_Rejected;
   begin
      T.Register;
      T.Assert (abs (Linear_XY_Distance (Start_Pos, Finish_Pos) - (200.0 * mm ** 2) ** (1 / 2)) < 1.0E-10 * mm);
      T.Assert (abs (Helix_XY_Distance (Start_Pos, Finish_Pos, Center, False) - 5.0 * Ada.Numerics.Pi * mm)
                  < 1.0E-10 * mm);
      T.Assert (abs (Helix_XY_Distance (Start_Pos, Finish_Pos, Center, True) - 15.0 * Ada.Numerics.Pi * mm)
                  < 1.0E-10 * mm);
      Settings.Excessive_Extrusion_Prevention :=
        (Extrusion_Only => (Kind => Enabled, Maximum_Length => 1.0 * mm),
         Extrusion_To_XY_Ratio => (Kind => Enabled, Maximum_Ratio => 0.25));
      Data.Pos := Start_Pos;
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (Arc_Rejected (4.0));
      T.Assert (Data.Count = 0, "rejected arcs queue no motion");
      T.Assert (not Linear_Rejected (Instance, Planner, 0.0, Z => (True, 1.0)));
      T.Assert (Data.Feedrate = Settings.Motion_Gcode.Default_G1_Feedrate, "rejected arcs preserve feedrate");
      T.Assert (not Arc_Rejected (3.8), "arc length allows extrusion that would exceed the chord ratio");
      T.Assert (Data.Helices = 1);

      Data := (Pos => Start_Pos, others => <>);
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (Arc_Rejected (16.0, Full_Circle => True));
      T.Assert (not Arc_Rejected (15.0, Full_Circle => True), "full circles use circumference, not zero XY travel");
      T.Assert (Data.Helices = 1);

      Data := (Pos => Start_Pos, others => <>);
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (not Arc_Rejected (11.0, Clockwise => True), "clockwise major arcs use the complete sweep");
      Data := (Pos => Start_Pos, others => <>);
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (Arc_Rejected (4.0, Radius_Form => True));
      T.Assert (not Arc_Rejected (3.8, Radius_Form => True), "radius-form arcs use arc length");
      Data := (Pos => Start_Pos, others => <>);
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (not Arc_Rejected (11.0, Radius_Form => True, Radius => -10.0), "negative radius selects a major arc");

      Data := (Pos => Start_Pos, others => <>);
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (Arc_Rejected (4.0, Finish_Y => 20.0), "non-extruding radial corrections cannot dilute the ratio");
      T.Assert (Data.Count = 0, "rejected corrected arcs queue neither the helix nor correction");
      T.Assert (not Arc_Rejected (3.8, Finish_Y => 20.0));
      T.Assert (Data.Count = 2 and then Data.Helices = 1);
      Data := (Pos => Start_Pos, others => <>);
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (Arc_Rejected (2.6, Finish_X => 20.0, Finish_Y => 0.0), "zero-sweep arcs check the fallback line");
      T.Assert (not Arc_Rejected (2.5, Finish_X => 20.0, Finish_Y => 0.0));
      T.Assert (Data.Helices = 0);
   end Test_Excessive_Extrusion_Arcs;

   procedure Test_Excessive_Extrusion_Defaults_And_Limits (T : in out Trendy_Test.Operation'Class) is
      Settings : User_Config_Excessive_Extrusion_Prevention;
      Schema : constant Config.Config_Property_Maps.Map := Build_Schema;
      Guards : constant Config.Config_Property_Parameters_Sequence :=
        Config.Config_Property_Parameters_Sequence (Schema ("Excessive_Extrusion_Prevention"));
      Only_Limit : constant Config.Config_Property_Parameters_Sequence :=
        Config.Config_Property_Parameters_Sequence (Guards.Children ("Extrusion_Only"));
      Ratio_Limit : constant Config.Config_Property_Parameters_Sequence :=
        Config.Config_Property_Parameters_Sequence (Guards.Children ("Extrusion_To_XY_Ratio"));

      function Rejected (E, XY : Length) return Boolean;

      function Rejected (E, XY : Length) return Boolean is
      begin
         Validate_Extrusion_Limits (Settings, E, XY);
         return False;
      exception
         when Gcode_Bad_Inputs_Error =>
            return True;
      end Rejected;
   begin
      T.Register;
      T.Assert (Settings.Extrusion_Only.Kind = Disabled and then Settings.Extrusion_To_XY_Ratio.Kind = Disabled);
      T.Assert
        (Config.Config_Property_Parameters_Variant (Only_Limit.Children ("Kind")).Default = "Disabled");
      T.Assert
        (Config.Config_Property_Parameters_Variant (Ratio_Limit.Children ("Kind")).Default = "Disabled");
      T.Assert (not Rejected (1.0E100 * mm, 0.0 * mm), "extrusion-only guard defaults to disabled");
      T.Assert (not Rejected (1.0E100 * mm, 1.0E-100 * mm), "ratio guard defaults to disabled");

      Settings.Extrusion_Only := (Kind => Enabled, Maximum_Length => 5.0 * mm);
      T.Assert (not Rejected (5.0 * mm, 0.0 * mm), "extrusion-only threshold is inclusive");
      T.Assert (Rejected (5.001 * mm, 0.0 * mm));
      T.Assert (not Rejected (100.0 * mm, 1.0 * mm), "extrusion-only limit does not restrict XY moves");
      Settings.Extrusion_Only := (Kind => Disabled);
      Settings.Extrusion_To_XY_Ratio := (Kind => Enabled, Maximum_Ratio => 0.25);
      T.Assert (not Rejected (100.0 * mm, 0.0 * mm), "ratio guard does not enable the extrusion-only guard");
      T.Assert (not Rejected (2.5 * mm, 10.0 * mm), "ratio threshold is inclusive");
      T.Assert (Rejected (2.501 * mm, 10.0 * mm));
      T.Assert (Rejected (1.0 * mm, 1.0E-300 * mm), "tiny XY moves cannot bypass the guard");

      Settings.Extrusion_Only := (Kind => Enabled, Maximum_Length => 0.0 * mm);
      Settings.Extrusion_To_XY_Ratio := (Kind => Enabled, Maximum_Ratio => 0.0);
      T.Assert (Rejected (0.001 * mm, 0.0 * mm) and then Rejected (0.001 * mm, 10.0 * mm));
      T.Assert (not Rejected (0.0 * mm, 0.0 * mm) and then not Rejected (-100.0 * mm, 0.0 * mm));
      T.Assert (not Rejected (-100.0 * mm, 1.0 * mm), "retractions are unrestricted");
   end Test_Excessive_Extrusion_Defaults_And_Limits;

   procedure Test_Excessive_Extrusion_Linear_Moves (T : in out Trendy_Test.Operation'Class) is
      Device : constant Module := (My_Modules.Module with null record);
      Status : constant Status_Manager.Status_Data_Collection :=
        Status_Manager.Build_Collection (["Motion" => Device.Status_Schema]);
      Settings : User_Config;
      Data : aliased Mock_Planner_Data;
      Planner : constant Mock_Planner := (Data => Data'Unchecked_Access);
      Instance : My_Modules.Module_Instance_Shared_Pointers.Ref;
   begin
      T.Register;
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (not Linear_Rejected (Instance, Planner, 100.0), "disabled extrusion-only limit permits large moves");
      T.Assert
        (not Linear_Rejected (Instance, Planner, 200.0, X => (True, 0.001)), "disabled ratio permits large moves");

      Settings.Excessive_Extrusion_Prevention :=
        (Extrusion_Only => (Kind => Enabled, Maximum_Length => 5.0 * mm),
         Extrusion_To_XY_Ratio => (Kind => Enabled, Maximum_Ratio => 0.25));
      Data := (others => <>);
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (Linear_Rejected (Instance, Planner, 5.1, Z => (True, 10.0), F => (True, 600.0)));
      T.Assert (Data.Count = 0, "rejected Z/E moves queue no motion");
      T.Assert (not Linear_Rejected (Instance, Planner, 5.0));
      T.Assert (Data.Feedrate = Settings.Motion_Gcode.Default_G1_Feedrate, "rejection leaves the feedrate unchanged");
      T.Assert (Linear_Rejected (Instance, Planner, 7.501, X => (True, 6.0), Y => (True, 8.0), Z => (True, 100.0)));
      T.Assert (Data.Count = 1, "Z travel does not dilute the extrusion to XY ratio");
      T.Assert (not Linear_Rejected (Instance, Planner, 7.5, X => (True, 6.0), Y => (True, 8.0)));
      T.Assert (Linear_Rejected (Instance, Planner, 10.1, X => (True, 12.0), Y => (True, 16.0), Rapid => True));
      T.Assert (not Linear_Rejected (Instance, Planner, -100.0), "large retractions remain allowed");

      Data := (others => <>);
      Settings.Motion_Gcode.Default_G92_E_Offset := 100.0 * mm;
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (Linear_Rejected (Instance, Planner, 105.1), "absolute E is checked as a displacement from G92");
      T.Assert (not Linear_Rejected (Instance, Planner, 105.0));
      Module_Instance (Instance.Get.Element.all).Set_Virtual_Position_State
        (Planner, (Present => False), (Present => False), (Present => False), (True, 1000.0));
      T.Assert (Linear_Rejected (Instance, Planner, 1005.1), "changing G92 cannot bypass the guard");
      T.Assert (not Linear_Rejected (Instance, Planner, 1005.0));

      Data := (others => <>);
      Settings.Motion_Gcode.Default_Positioning := Relative_Positioning_Mode;
      Settings.Motion_Gcode.Default_E_Positioning := Relative_E_Positioning_Mode;
      Settings.Motion_Gcode.Default_Flow_Scale := 2.0;
      Settings.Motion_Gcode.Default_G92_E_Offset := 100.0 * mm;
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (Linear_Rejected (Instance, Planner, 3.0), "flow scaling is included in relative E-only limits");
      T.Assert (not Linear_Rejected (Instance, Planner, 2.5));
      T.Assert (Data.Pos (E_Axis) = 5.0 * mm);
      T.Assert
        (Linear_Rejected (Instance, Planner, 1.3, X => (True, 10.0)), "flow scaling is included in ratio limits");
      T.Assert (not Linear_Rejected (Instance, Planner, 1.25, X => (True, 10.0)));

      Data := (others => <>);
      Settings.Motion_Gcode.Default_Units := Inch_Units_Mode;
      Settings.Motion_Gcode.Default_Flow_Scale := 1.0;
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (Linear_Rejected (Instance, Planner, 0.2), "inch E inputs are converted to mm before checking");
      T.Assert (not Linear_Rejected (Instance, Planner, 0.1));
   end Test_Excessive_Extrusion_Linear_Moves;

   procedure Test_Excessive_Extrusion_Recovery_And_Pause (T : in out Trendy_Test.Operation'Class) is
      Device : constant Module := (My_Modules.Module with null record);
      Status : constant Status_Manager.Status_Data_Collection :=
        Status_Manager.Build_Collection (["Motion" => Device.Status_Schema]);
      Settings : User_Config;
      Data : aliased Mock_Planner_Data;
      Planner : constant Mock_Planner := (Data => Data'Unchecked_Access);
      Instance : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Context : Mock_Pause_Context;
      Rejected : Boolean;
   begin
      T.Register;
      Settings.Excessive_Extrusion_Prevention.Extrusion_Only := (Kind => Enabled, Maximum_Length => 5.0 * mm);
      Settings.Motion_Gcode.Firmware_Retract_Length := 6.0 * mm;
      Settings.Motion_Gcode.Firmware_Retract_Z_Lift := 2.0 * mm;
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      Module_Instance (Instance.Get.Element.all).Execute_Retract (Planner, (Present => False));
      Rejected := False;
      begin
         Module_Instance (Instance.Get.Element.all).Execute_Recover (Planner);
      exception
         when Gcode_Bad_Inputs_Error =>
            Rejected := True;
      end;
      T.Assert (Rejected and then Data.Count = 2 and then Data.Pos (Z_Axis) = 2.0 * mm,
                "rejected recovery does not lower Z or queue extrusion");
      Module_Instance (Instance.Get.Element.all).Apply_Retraction_Settings
        (Planner, (Present => False), (True, 4.0), (Present => False));
      Module_Instance (Instance.Get.Element.all).Execute_Recover (Planner);
      T.Assert (Data.Count = 4 and then Data.Pos (Z_Axis) = 0.0 * mm, "rejection preserves the retracted state");

      Data := (others => <>);
      Settings.Motion_Gcode.Default_Auto_Retract_Enabled := True;
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (not Linear_Rejected (Instance, Planner, -2.0));
      T.Assert
        (Linear_Rejected (Instance, Planner, 0.0, F => (True, 600.0)), "auto-recovery checks physical distance");
      T.Assert (Data.Count = 2 and then Data.Pos (Z_Axis) = 2.0 * mm, "auto-recovery rejection queues nothing");

      Data := (others => <>);
      Settings.Pause_Park :=
        (Kind => Relative_Park_Move,
         Relative_Park_Move => (X_Offset => 10.0 * mm, Z_Offset => 2.0 * mm, E_Offset => 6.0 * mm, others => <>));
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      Rejected := False;
      begin
         Module_Instance (Instance.Get.Element.all).Handle_Pause (Planner, Context);
      exception
         when Gcode_Bad_Inputs_Error =>
            Rejected := True;
      end;
      T.Assert (Rejected and then Data.Count = 0, "excessive pause extrusion rejects the complete park move");
      Settings.Pause_Park.Relative_Park_Move.E_Offset := -6.0 * mm;
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      Module_Instance (Instance.Get.Element.all).Handle_Pause (Planner, Context);
      T.Assert (Data.Count = 3, "pause retraction is allowed");
      Rejected := False;
      begin
         Module_Instance (Instance.Get.Element.all).Handle_Resume (Planner, Context);
      exception
         when Gcode_Bad_Inputs_Error =>
            Rejected := True;
      end;
      T.Assert (Rejected and then Data.Count = 3, "excessive resume extrusion rejects before XY or Z return");
   end Test_Excessive_Extrusion_Recovery_And_Pause;

   function All_Tests return Trendy_Test.Test_Group
   is (Trendy_Test.Test_Group'
         [Test_Excessive_Extrusion_Defaults_And_Limits'Unrestricted_Access,
          Test_Excessive_Extrusion_Linear_Moves'Unrestricted_Access,
          Test_Excessive_Extrusion_Arcs'Unrestricted_Access,
          Test_Excessive_Extrusion_Recovery_And_Pause'Unrestricted_Access]);

end Prunt.Default_Modules.Motion.Test;
