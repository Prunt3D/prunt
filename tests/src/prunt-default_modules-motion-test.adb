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
with Prunt.JSON;

package body Prunt.Default_Modules.Motion.Test is

   use type Gcode_Arguments.Argument_Kind;

   type Command_Lines is array (Positive range <>) of Virtual_String;

   procedure Test_Excessive_Extrusion_Arcs (T : in out Trendy_Test.Operation'Class);
   procedure Test_Excessive_Extrusion_Defaults_And_Limits (T : in out Trendy_Test.Operation'Class);
   procedure Test_Excessive_Extrusion_Linear_Moves (T : in out Trendy_Test.Operation'Class);
   procedure Test_Excessive_Extrusion_Recovery_And_Pause (T : in out Trendy_Test.Operation'Class);
   procedure Test_Volumetric_Arcs_And_Guards (T : in out Trendy_Test.Operation'Class);
   procedure Test_Volumetric_Inch_Units_And_Retract (T : in out Trendy_Test.Operation'Class);
   procedure Test_Volumetric_Linear_Moves (T : in out Trendy_Test.Operation'Class);
   procedure Test_Volumetric_Settings (T : in out Trendy_Test.Operation'Class);
   procedure Test_Volumetric_State_And_Config (T : in out Trendy_Test.Operation'Class);

   type Mock_Planner_Data is record
      Pos      : Position := [others => 0.0 * mm];
      Count    : Planner_Corner_ID := 0;
      Helices  : Natural := 0;
      Feedrate : Velocity := 0.0 * mm / s;
      Executed : Planner_Corner_ID := Planner_Corner_ID'Last;
      Report   : Virtual_String;
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
   function Get_Last_Executed_Corner_ID (This : Mock_Planner) return Planner_Corner_ID
   is (Planner_Corner_ID'Min (This.Data.Count, This.Data.Executed));
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
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record));
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
     (Settings : User_Config; Status : Status_Manager.Status_Emitter;
      Settings_Data : access Config.Config_Data := null)
      return My_Modules.Module_Instance_Shared_Pointers.Ref;

   function Create_Instance
     (Settings : User_Config; Status : Status_Manager.Status_Emitter;
      Settings_Data : access Config.Config_Data := null)
      return My_Modules.Module_Instance_Shared_Pointers.Ref
   is
      Data : Config.Config_Data;

      function Create return My_Modules.Module_Instance_Parent'Class;

      function Create return My_Modules.Module_Instance_Parent'Class is
      begin
         return Result : Module_Instance do
            Result.Initialize (Settings, (if Settings_Data = null then Data else Settings_Data.all), Status);
         end return;
      end Create;
   begin
      return Result : My_Modules.Module_Instance_Shared_Pointers.Ref do
         Result.Set (Create'Access);
      end return;
   end Create_Instance;

   procedure Dispatch_Command
     (Instance : My_Modules.Module_Instance_Shared_Pointers.Ref; Planner : Mock_Planner; Line : Virtual_String);

   procedure Dispatch_Command
     (Instance : My_Modules.Module_Instance_Shared_Pointers.Ref; Planner : Mock_Planner; Line : Virtual_String)
   is
      Args : Gcode_Arguments.Arguments := Gcode_Arguments.Parse_Arguments (Line);
      Letter : constant Gcode_Identifier_Argument_Index :=
        (if Args.Kind ('M') = Gcode_Arguments.Non_Existent_Kind then 'G' else 'M');
      Command : constant Gcode_Command_Identifier := (Letter, Args.Consume_Integer (Letter));
   begin
      Gcode_Dispatch (Module_Instance (Instance.Get.Element.all), Instance, Args, Planner, Command);
      Args.Validate_All_Consumed;
   end Dispatch_Command;

   overriding
   procedure Flush
     (This : Mock_Planner;
      Extra_Data : Extra_Block_Resetting_Data'Class := Extra_Block_Resetting_Data'(null record)) is
   begin
      if Extra_Data in Motion_Report_Event then
         This.Data.Report := Motion_Report_Event (Extra_Data).Message;
      end if;
   end Flush;

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

   procedure Test_Volumetric_Arcs_And_Guards (T : in out Trendy_Test.Operation'Class) is
      Device : constant Module := (My_Modules.Module with null record);
      Status : constant Status_Manager.Status_Data_Collection :=
        Status_Manager.Build_Collection (["Motion" => Device.Status_Schema]);
      Settings : User_Config;
      Data : aliased Mock_Planner_Data;
      Planner : constant Mock_Planner := (Data => Data'Unchecked_Access);
      Instance : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Rejected : Boolean := False;
   begin
      T.Register;
      Settings.Motion_Gcode.Default_Volumetric_Enabled := True;
      Settings.Motion_Gcode.Default_Filament_Diameter := 2.0 * mm;
      Settings.Motion_Gcode.Default_E_Positioning := Relative_E_Positioning_Mode;
      Settings.Excessive_Extrusion_Prevention :=
        (Extrusion_Only => (Kind => Enabled, Maximum_Length => 5.0 * mm),
         Extrusion_To_XY_Ratio => (Kind => Enabled, Maximum_Ratio => 0.25));
      Data.Pos (X_Axis) := 10.0 * mm;
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      Dispatch_Command (Instance, Planner, "G3 X0 Y10 I-10 E10 F600");
      T.Assert (Data.Helices = 1 and then abs (Data.Pos (E_Axis) - 10.0 / Ada.Numerics.Pi * mm) < 1.0E-10 * mm,
                "arc extrusion is converted to filament length before checking the XY ratio");
      Dispatch_Command (Instance, Planner, "M221 S200");
      begin
         Dispatch_Command (Instance, Planner, "G2 X10 Y0 J-10 E10");
      exception
         when Gcode_Bad_Inputs_Error =>
            Rejected := True;
      end;
      T.Assert (Rejected and then Data.Count = 1, "flow-scaled volumetric arcs reject before queuing");
      Dispatch_Command (Instance, Planner, "G2 J-10 E2");
      Dispatch_Command (Instance, Planner, "G3 X10 Y0 R10 E4");
      T.Assert (Data.Helices = 3 and then abs (Data.Pos (E_Axis) - 22.0 / Ada.Numerics.Pi * mm) < 1.0E-10 * mm,
                "full-circle and radius-form arcs use the same volumetric conversion");

      Data := (others => <>);
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      T.Assert (Linear_Rejected (Instance, Planner, 16.0), "extrusion-only limits use physical filament length");
      T.Assert (not Linear_Rejected (Instance, Planner, 15.0));
      T.Assert (not Linear_Rejected (Instance, Planner, -100.0), "volumetric retractions remain unrestricted");
      T.Assert (Linear_Rejected (Instance, Planner, 8.0, X => (True, 10.0)), "XY limits use converted E deltas");
   end Test_Volumetric_Arcs_And_Guards;

   procedure Test_Volumetric_Inch_Units_And_Retract (T : in out Trendy_Test.Operation'Class) is
      Device : constant Module := (My_Modules.Module with null record);
      Status : constant Status_Manager.Status_Data_Collection :=
        Status_Manager.Build_Collection (["Motion" => Device.Status_Schema]);
      Settings : User_Config;
      Data : aliased Mock_Planner_Data;
      Planner : constant Mock_Planner := (Data => Data'Unchecked_Access);
      Instance : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Before_Retract : Length;
   begin
      T.Register;
      Settings.Motion_Gcode.Default_Volumetric_Enabled := True;
      Settings.Motion_Gcode.Default_Filament_Diameter := 2.0 * mm;
      Settings.Motion_Gcode.Firmware_Retract_Length := 3.0 * mm;
      Settings.Motion_Gcode.Firmware_Recover_Extra_Length := 1.0 * mm;
      Instance := Create_Instance (Settings, Status.Get_Emitter ("Motion"));
      Dispatch_Command (Instance, Planner, "G20");
      Dispatch_Command (Instance, Planner, "M200 D0.1");
      Dispatch_Command (Instance, Planner, "G92 E0.01");
      Dispatch_Command (Instance, Planner, "G1 X1 E0.02 F60");
      T.Assert (abs (Data.Pos (E_Axis) - 0.01 * 25.4 ** 3 / (Ada.Numerics.Pi * 1.27 ** 2) * mm)
                  < 1.0E-10 * mm, "G20 uses cubic inches for E and linear inches for filament diameter");
      T.Assert (Data.Pos (X_Axis) = 25.4 * mm and then Data.Feedrate = 25.4 * mm / s,
                "XYZ and F retain their linear unit conversion");
      Before_Retract := Data.Pos (E_Axis);
      Dispatch_Command (Instance, Planner, "G10");
      T.Assert (abs (Data.Pos (E_Axis) - Before_Retract + 3.0 * mm) < 1.0E-10 * mm,
                "firmware retract length remains a filament length");
      Dispatch_Command (Instance, Planner, "G11");
      T.Assert (abs (Data.Pos (E_Axis) - Before_Retract - 1.0 * mm) < 1.0E-10 * mm);
      Dispatch_Command (Instance, Planner, "G1 E0.02");
      T.Assert (abs (Data.Pos (E_Axis) - Before_Retract - 1.0 * mm) < 1.0E-10 * mm,
                "firmware retraction and recovery preserve logical volumetric E");
      Dispatch_Command (Instance, Planner, "G21");
      Dispatch_Command (Instance, Planner, "G92 E0");
      Dispatch_Command (Instance, Planner, "M83");
      Dispatch_Command (Instance, Planner, "M200 D2");
      Dispatch_Command (Instance, Planner, "M209 S1");
      Before_Retract := Data.Pos (E_Axis);
      Dispatch_Command (Instance, Planner, "G1 E-20");
      T.Assert (abs (Data.Pos (E_Axis) - Before_Retract + 3.0 * mm) < 1.0E-10 * mm,
                "auto-retract thresholds compare the equivalent filament length");
      Dispatch_Command (Instance, Planner, "G1 E20");
      T.Assert (abs (Data.Pos (E_Axis) - Before_Retract - 1.0 * mm) < 1.0E-10 * mm);
      Dispatch_Command (Instance, Planner, "G60 S0");
      Dispatch_Command (Instance, Planner, "G61 E10");
      Before_Retract := Data.Pos (E_Axis);
      Dispatch_Command (Instance, Planner, "M82");
      T.Assert (not Linear_Rejected (Instance, Planner, Before_Retract / mm + 10.0),
                "saved-position E offsets use volumetric coordinates without moving E");
      T.Assert (abs (Data.Pos (E_Axis) - Before_Retract) < 1.0E-10 * mm);
   end Test_Volumetric_Inch_Units_And_Retract;

   procedure Test_Volumetric_Linear_Moves (T : in out Trendy_Test.Operation'Class) is
      Device : constant Module := (My_Modules.Module with null record);
      Status : constant Status_Manager.Status_Data_Collection :=
        Status_Manager.Build_Collection (["Motion" => Device.Status_Schema]);
      Settings : User_Config;
      Data : aliased Mock_Planner_Data;
      Planner : constant Mock_Planner := (Data => Data'Unchecked_Access);
      Instance : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Create_Instance (Settings, Status.Get_Emitter ("Motion"));
   begin
      T.Register;
      Dispatch_Command (Instance, Planner, "M200 D2");
      Dispatch_Command (Instance, Planner, "G92 E100");
      Dispatch_Command (Instance, Planner, "G1 X10 E110");
      T.Assert (abs (Data.Pos (E_Axis) - 10.0 / Ada.Numerics.Pi * mm) < 1.0E-10 * mm,
                "absolute volumetric E is measured from G92");
      Dispatch_Command (Instance, Planner, "M221 S200");
      Dispatch_Command (Instance, Planner, "G1 E120");
      T.Assert (abs (Data.Pos (E_Axis) - 30.0 / Ada.Numerics.Pi * mm) < 1.0E-10 * mm,
                "M221 scales filament movement without changing logical E");
      Dispatch_Command (Instance, Planner, "M200 D4");
      Dispatch_Command (Instance, Planner, "G1 E130");
      T.Assert (abs (Data.Pos (E_Axis) - 35.0 / Ada.Numerics.Pi * mm) < 1.0E-10 * mm,
                "a diameter change affects only the next E displacement");
      Dispatch_Command (Instance, Planner, "M200 S0");
      Dispatch_Command (Instance, Planner, "G1 E131");
      T.Assert (abs (Data.Pos (E_Axis) - 35.0 / Ada.Numerics.Pi * mm - 2.0 * mm) < 1.0E-10 * mm,
                "disabling volumetric mode preserves the logical E coordinate");
      Dispatch_Command (Instance, Planner, "M200 D2");
      Dispatch_Command (Instance, Planner, "M83");
      Dispatch_Command (Instance, Planner, "G1 E-2");
      Dispatch_Command (Instance, Planner, "G0 E4");
      T.Assert (abs (Data.Pos (E_Axis) - 39.0 / Ada.Numerics.Pi * mm - 2.0 * mm) < 1.0E-10 * mm,
                "relative retractions and rapid extrusion use the same volume and flow scaling");
   end Test_Volumetric_Linear_Moves;

   procedure Test_Volumetric_Settings (T : in out Trendy_Test.Operation'Class) is
      Device : constant Module := (My_Modules.Module with null record);
      Status : constant Status_Manager.Status_Data_Collection :=
        Status_Manager.Build_Collection (["Motion" => Device.Status_Schema]);
      Settings : User_Config;
      Data : aliased Mock_Planner_Data;
      Planner : constant Mock_Planner := (Data => Data'Unchecked_Access);
      Instance : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Create_Instance (Settings, Status.Get_Emitter ("Motion"));

      procedure Assert_Settings (Enabled : Boolean; Diameter : Dimensionless);

      procedure Assert_Settings (Enabled : Boolean; Diameter : Dimensionless) is
      begin
         Dispatch_Command (Instance, Planner, "M200 T0");
         T.Assert (Ada.Strings.Fixed.Index (Conversions.To_UTF_8_String (Data.Report),
                                           "M200: S = " & (if Enabled then "1" else "0")) = 1);
         declare
            Flow : constant Prunt.JSON.JSON_Value := Prunt.JSON.Read (Status.JSON_Data).Get ("Motion").Get ("Flow");
         begin
            T.Assert (Boolean'(Flow.Get ("Volumetric enabled").Get) = Enabled);
            T.Assert (Long_Float'(Flow.Get ("Filament diameter").Get) = Long_Float (Diameter));
         end;
         T.Assert (Data.Count = 0, "setting or reporting M200 never moves the extruder");
      end Assert_Settings;
   begin
      T.Register;
      Dispatch_Command (Instance, Planner, "M200");
      Assert_Settings (False, 1.75);
      Dispatch_Command (Instance, Planner, "M200 D2 T0");
      Assert_Settings (True, 2.0);
      Dispatch_Command (Instance, Planner, "M200 S0 D3");
      Assert_Settings (False, 3.0);
      Dispatch_Command (Instance, Planner, "M200 S1");
      Assert_Settings (True, 3.0);
      Dispatch_Command (Instance, Planner, "M200 D0 S1");
      Assert_Settings (False, 3.0);
      Dispatch_Command (Instance, Planner, "M200 S");
      Assert_Settings (True, 3.0);
      Dispatch_Command (Instance, Planner, "M200 D");
      Assert_Settings (False, 3.0);
      for Line of Command_Lines'["M200 D-2 S1", "M200 D2 T1", "M200 S-1", "M200 X1"] loop
         declare
            Rejected : Boolean := False;
         begin
            begin
               Dispatch_Command (Instance, Planner, Line);
            exception
               when Gcode_Bad_Inputs_Error =>
                  Rejected := True;
            end;
            T.Assert (Rejected, "invalid M200 arguments are rejected");
            Assert_Settings (False, 3.0);
         end;
      end loop;
   end Test_Volumetric_Settings;

   procedure Test_Volumetric_State_And_Config (T : in out Trendy_Test.Operation'Class) is
      Device : constant Module := (My_Modules.Module with null record);
      Status : constant Status_Manager.Status_Data_Collection :=
        Status_Manager.Build_Collection (["Motion" => Device.Status_Schema]);
      File : constant Config.Config_File :=
        Config.Create (Conversions.To_UTF_8_String (Next_Test_Filename), ["Motion" => Device.Config_Schema]);
      Config_Data : aliased Config.Config_Data := File.Get_Data ("Motion");
      Settings : User_Config := Config_Data_To_User_Config (Config_Data);
      Data : aliased Mock_Planner_Data;
      Planner : constant Mock_Planner := (Data => Data'Unchecked_Access);
      Instance : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Create_Instance (Settings, Status.Get_Emitter ("Motion"), Config_Data'Access);
      Before_Cancel : Position;
   begin
      T.Register;
      T.Assert (not Settings.Motion_Gcode.Default_Volumetric_Enabled);
      T.Assert (Settings.Motion_Gcode.Default_Filament_Diameter = 1.75 * mm);
      Dispatch_Command (Instance, Planner, "M200 D2");
      Data.Executed := 0;
      Dispatch_Command (Instance, Planner, "G1 E10");
      Before_Cancel := Data.Pos;
      Dispatch_Command (Instance, Planner, "M200 D4");
      Module_Instance (Instance.Get.Element.all).Prepare_Config_For_Save;
      Settings := Config_Data_To_User_Config (Config_Data);
      T.Assert (Settings.Motion_Gcode.Default_Volumetric_Enabled
                and then Settings.Motion_Gcode.Default_Filament_Diameter = 2.0 * mm,
                "M500 saves the committed volumetric settings");
      Module_Instance (Instance.Get.Element.all).Catch_Up_Planner_State (1);
      declare
         Flow : constant Prunt.JSON.JSON_Value := Prunt.JSON.Read (Status.JSON_Data).Get ("Motion").Get ("Flow");
      begin
         T.Assert (Flow.Get ("Volumetric enabled").Get);
         T.Assert (Long_Float'(Flow.Get ("Filament diameter").Get) = 4.0,
                   "status catches up to the executed settings");
      end;
      Dispatch_Command (Instance, Planner, "G1 E20");
      Dispatch_Command (Instance, Planner, "M200 S0 D3");
      Data.Executed := 1;
      Data.Pos := Before_Cancel;
      Module_Instance (Instance.Get.Element.all).Handle_Cancel (1, 2, Data.Pos);
      Dispatch_Command (Instance, Planner, "M200");
      T.Assert (Ada.Strings.Fixed.Index (Conversions.To_UTF_8_String (Data.Report), "M200: S = 1") = 1,
                "cancellation discards unexecuted volumetric changes");
      Dispatch_Command (Instance, Planner, "G1 E20");
      T.Assert (abs (Data.Pos (E_Axis) - 12.5 / Ada.Numerics.Pi * mm) < 1.0E-10 * mm,
                "subsequent moves use the restored diameter and logical E");
      Module_Instance (Instance.Get.Element.all).Prepare_Config_For_Save;
      Settings := Config_Data_To_User_Config (Config_Data);
      T.Assert (Settings.Motion_Gcode.Default_Volumetric_Enabled
                and then Settings.Motion_Gcode.Default_Filament_Diameter = 4.0 * mm);
   end Test_Volumetric_State_And_Config;

   function All_Tests return Trendy_Test.Test_Group
   is (Trendy_Test.Test_Group'
         [Test_Excessive_Extrusion_Defaults_And_Limits'Unrestricted_Access,
          Test_Excessive_Extrusion_Linear_Moves'Unrestricted_Access,
          Test_Excessive_Extrusion_Arcs'Unrestricted_Access,
          Test_Excessive_Extrusion_Recovery_And_Pause'Unrestricted_Access,
          Test_Volumetric_Arcs_And_Guards'Unrestricted_Access,
          Test_Volumetric_Inch_Units_And_Retract'Unrestricted_Access,
          Test_Volumetric_Linear_Moves'Unrestricted_Access,
          Test_Volumetric_Settings'Unrestricted_Access,
          Test_Volumetric_State_And_Config'Unrestricted_Access]);

end Prunt.Default_Modules.Motion.Test;
