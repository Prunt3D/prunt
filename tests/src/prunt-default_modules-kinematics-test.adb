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

with Ada.Containers;
with Ada.Strings.Fixed;
with VSS.Strings.Conversions;

package body Prunt.Default_Modules.Kinematics.Test is

   pragma Extensions_Allowed (On);

   use type Ada.Containers.Count_Type;
   use type Config.Config_Schema_Version;
   use type Gcode_Arguments.Argument_Integer;

   function Branch_Parameters
     (Variant : Config.Config_Property_Parameters_Variant; Branch_Name, Parameters_Name : Virtual_String)
      return Config.Config_Property_Parameters_Sequence
   is
      Branch : constant Config.Config_Property_Parameters_Sequence :=
        Config.Config_Property_Parameters_Sequence (Variant.Children.Element (Branch_Name));
   begin
      return Config.Config_Property_Parameters_Sequence (Branch.Children.Element (Parameters_Name));
   end Branch_Parameters;

   function Contains_Text (Source : Virtual_String; Fragment : String) return Boolean is
     (Ada.Strings.Fixed.Index (VSS.Strings.Conversions.To_UTF_8_String (Source), Fragment) /= 0);

   function Float_Parameters
     (Properties : Config.Config_Property_Maps.Map; Name : Virtual_String)
      return Config.Config_Property_Parameters_Float
   is (Config.Config_Property_Parameters_Float (Properties.Element (Name)));

   function Sequence_Parameters
     (Properties : Config.Config_Property_Maps.Map; Name : Virtual_String)
      return Config.Config_Property_Parameters_Sequence
   is (Config.Config_Property_Parameters_Sequence (Properties.Element (Name)));

   procedure Assert_Axial_Deviation_Defaults
     (Parameters : Config.Config_Property_Parameters_Sequence;
      Label      : String;
      T          : in out Trendy_Test.Operation'Class)
   is
      Axial_Deviation : constant Config.Config_Property_Parameters_Sequence :=
        Sequence_Parameters (Parameters.Children, "Axial_Deviation_Limits");
   begin
      T.Assert
        (Axial_Deviation.Children.Length = Ada.Containers.Count_Type (Axis_Name'Pos (Axis_Name'Last) + 1),
         Label & " exposes one axial deviation limit per axis");

      for Axis in Axis_Name loop
         T.Assert
           (Float_Parameters (Axial_Deviation.Children, +Axis'Image).Default = 0.1,
            Label & " " & Axis'Image & " axial deviation defaults to 0.1 mm");
      end loop;
   end Assert_Axial_Deviation_Defaults;

   procedure Assert_Branch_Shape
     (Variant         : Config.Config_Property_Parameters_Variant;
      Branch_Name     : Virtual_String;
      Parameters_Name : Virtual_String;
      Field_Count     : Ada.Containers.Count_Type;
      T               : in out Trendy_Test.Operation'Class)
   is
      Branch : constant Config.Config_Property_Parameters_Sequence :=
        Config.Config_Property_Parameters_Sequence (Variant.Children.Element (Branch_Name));
      Parameters : constant Config.Config_Property_Parameters_Sequence :=
        Config.Config_Property_Parameters_Sequence (Branch.Children.Element (Parameters_Name));
      Label : constant String := VSS.Strings.Conversions.To_UTF_8_String (Branch_Name);
   begin
      T.Assert (Branch.Children.Length = 1, Label & " has exactly one visible parameter group");
      T.Assert (Parameters.Children.Length = Field_Count, Label & " exposes the expected fields");
   end Assert_Branch_Shape;

   procedure Test_Cornering_Branch_Round_Trips (T : in out Trendy_Test.Operation'Class) is
      Module_Value : Module;
      Schema       : constant Config.Versioned_Config_Schema := Config_Schema (Module_Value);
      Schemas      : constant Config.Config_Schema_Maps.Map := ["Kinematics" => Schema];
      File         : constant Config.Config_File := Config.Create (Next_Test_Filename, Schemas);
      Data         : Config.Config_Data := File.Get_Data ("Kinematics");

      function Branch_Name (Kind : User_Config_Cornering_Kind) return Virtual_String is
        (case Kind is
            when Stereographic => "Stereographic",
            when Circular      => "Circular",
            when Parabolic     => "Parabolic",
            when Biarc         => "Biarc",
            when Sharp_SCV     => "Sharp_SCV");

      procedure Check (Expected : User_Config_Cornering) is
         Input  : User_Config := (others => <>);
         Output : User_Config;
      begin
         Data.Set
           (["Kinematics", "Cornering", "Kind", "Selected"],
            Branch_Name (Expected.Kind));
         Input.Kinematics.Cornering := Expected;
         User_Config_To_Config_Data (Data, Input);
         Output := Config_Data_To_User_Config (Data);

         T.Assert
           (Output.Kinematics.Cornering = Expected,
            VSS.Strings.Conversions.To_UTF_8_String (Branch_Name (Expected.Kind))
            & " survives config-data conversion in both directions");
      end Check;
   begin
      T.Register;

      T.Assert
        (Config_Data_To_User_Config (Data).Kinematics.Cornering.Kind = Stereographic,
         "Default config data selects Stereographic");

      Check
        ((Kind                 => Stereographic,
          Stereographic_Params =>
            (Axial_Deviation_Limits       =>
               [X_Axis => 1.0 * mm, Y_Axis => 2.0 * mm, Z_Axis => 3.0 * mm, E_Axis => 4.0 * mm],
             Maximum_Corner_Miss_Distance => 5.0 * mm,
             Shape_Bias                   => -0.5,
             Circularity                  => 0.25)));
      Check
        ((Kind            => Circular,
          Circular_Params =>
            (Axial_Deviation_Limits       =>
               [X_Axis => 2.0 * mm, Y_Axis => 3.0 * mm, Z_Axis => 4.0 * mm, E_Axis => 5.0 * mm],
             Maximum_Corner_Miss_Distance => 6.0 * mm,
             Maximum_Radius               => 7.0 * mm)));
      Check
        ((Kind             => Parabolic,
          Parabolic_Params =>
            (Axial_Deviation_Limits       =>
               [X_Axis => 3.0 * mm, Y_Axis => 4.0 * mm, Z_Axis => 5.0 * mm, E_Axis => 6.0 * mm],
             Maximum_Corner_Miss_Distance => 7.0 * mm,
             Shape_Bias                   => 0.5,
             Maximum_Trim                 => 8.0 * mm)));
      Check
        ((Kind         => Biarc,
          Biarc_Params =>
            (Axial_Deviation_Limits       =>
               [X_Axis => 4.0 * mm, Y_Axis => 5.0 * mm, Z_Axis => 6.0 * mm, E_Axis => 7.0 * mm],
             Maximum_Corner_Miss_Distance => 8.0 * mm,
             Shape_Bias                   => -0.25,
             Maximum_Trim                 => 9.0 * mm)));
      Check
        ((Kind             => Sharp_SCV,
          Sharp_SCV_Params => (Square_Corner_Velocity => 12.5 * mm / s)));
   end Test_Cornering_Branch_Round_Trips;

   procedure Test_Cornering_Schema_Defaults_And_Visibility (T : in out Trendy_Test.Operation'Class) is
      Module_Value : Module;
      Schema       : constant Config.Versioned_Config_Schema := Config_Schema (Module_Value);
      Kinematics_Parameters : constant Config.Config_Property_Parameters_Sequence :=
        Sequence_Parameters (Schema.Top_Level_Items, "Kinematics");
      Cornering_Parameters : constant Config.Config_Property_Parameters_Sequence :=
        Sequence_Parameters (Kinematics_Parameters.Children, "Cornering");
      Kind : constant Config.Config_Property_Parameters_Variant :=
        Config.Config_Property_Parameters_Variant (Cornering_Parameters.Children.Element ("Kind"));
      Stereographic_Parameters : constant Config.Config_Property_Parameters_Sequence :=
        Branch_Parameters (Kind, "Stereographic", "Stereographic_Params");
      Circular_Parameters : constant Config.Config_Property_Parameters_Sequence :=
        Branch_Parameters (Kind, "Circular", "Circular_Params");
      Parabolic_Parameters : constant Config.Config_Property_Parameters_Sequence :=
        Branch_Parameters (Kind, "Parabolic", "Parabolic_Params");
      Biarc_Parameters : constant Config.Config_Property_Parameters_Sequence :=
        Branch_Parameters (Kind, "Biarc", "Biarc_Params");
      Sharp_SCV_Parameters : constant Config.Config_Property_Parameters_Sequence :=
        Branch_Parameters (Kind, "Sharp_SCV", "Sharp_SCV_Params");
   begin
      T.Register;

      T.Assert (Schema.Version = 1, "Kinematics schema remains at version 1");
      T.Assert (Kind.Default = "Stereographic", "Stereographic is the default corner family");
      T.Assert (Kind.Children.Length = 5, "Cornering has exactly five selectable branches");
      T.Assert (Kind.Children.Contains ("Stereographic"), "Stereographic branch is visible");
      T.Assert (Kind.Children.Contains ("Circular"), "Circular branch is visible");
      T.Assert (Kind.Children.Contains ("Parabolic"), "Parabolic branch is visible");
      T.Assert (Kind.Children.Contains ("Biarc"), "Biarc branch is visible");
      T.Assert (Kind.Children.Contains ("Sharp_SCV"), "Sharp SCV branch is visible");

      Assert_Branch_Shape (Kind, "Stereographic", "Stereographic_Params", 4, T);
      Assert_Branch_Shape (Kind, "Circular", "Circular_Params", 3, T);
      Assert_Branch_Shape (Kind, "Parabolic", "Parabolic_Params", 4, T);
      Assert_Branch_Shape (Kind, "Biarc", "Biarc_Params", 4, T);
      Assert_Branch_Shape (Kind, "Sharp_SCV", "Sharp_SCV_Params", 1, T);

      Assert_Axial_Deviation_Defaults (Stereographic_Parameters, "Stereographic", T);
      Assert_Axial_Deviation_Defaults (Circular_Parameters, "Circular", T);
      Assert_Axial_Deviation_Defaults (Parabolic_Parameters, "Parabolic", T);
      Assert_Axial_Deviation_Defaults (Biarc_Parameters, "Biarc", T);

      T.Assert
        (Float_Parameters (Stereographic_Parameters.Children, "Maximum_Corner_Miss_Distance").Default = 0.1,
         "Stereographic corner miss defaults to 0.1 mm");
      T.Assert
        (Float_Parameters (Stereographic_Parameters.Children, "Shape_Bias").Default = 0.0,
         "Stereographic shape bias defaults to zero");
      T.Assert
        (Float_Parameters (Stereographic_Parameters.Children, "Circularity").Default = 0.0,
         "Stereographic circularity defaults to zero");

      T.Assert
        (Float_Parameters (Circular_Parameters.Children, "Maximum_Corner_Miss_Distance").Default = 0.1,
         "Circular corner miss defaults to 0.1 mm");
      T.Assert
        (Float_Parameters (Circular_Parameters.Children, "Maximum_Radius").Default = 1.0E100,
         "Circular maximum radius defaults to effectively uncapped");

      T.Assert
        (Float_Parameters (Parabolic_Parameters.Children, "Maximum_Corner_Miss_Distance").Default = 0.1,
         "Parabolic corner miss defaults to 0.1 mm");
      T.Assert
        (Float_Parameters (Parabolic_Parameters.Children, "Shape_Bias").Default = 0.0,
         "Parabolic shape bias defaults to zero");
      T.Assert
        (Float_Parameters (Parabolic_Parameters.Children, "Maximum_Trim").Default = 1.0E100,
         "Parabolic maximum trim defaults to effectively uncapped");

      T.Assert
        (Float_Parameters (Biarc_Parameters.Children, "Maximum_Corner_Miss_Distance").Default = 0.1,
         "Biarc corner miss defaults to 0.1 mm");
      T.Assert
        (Float_Parameters (Biarc_Parameters.Children, "Shape_Bias").Default = 0.0,
         "Biarc shape bias defaults to zero");
      T.Assert
        (Float_Parameters (Biarc_Parameters.Children, "Maximum_Trim").Default = 1.0E100,
         "Biarc maximum trim defaults to effectively uncapped");

      T.Assert
        (Float_Parameters (Sharp_SCV_Parameters.Children, "Square_Corner_Velocity").Default = 5.0,
         "Sharp SCV defaults to 5 mm/s");
   end Test_Cornering_Schema_Defaults_And_Visibility;

   procedure Test_Cornering_Schema_Warnings (T : in out Trendy_Test.Operation'Class) is
      Module_Value : Module;
      Schema       : constant Config.Versioned_Config_Schema := Config_Schema (Module_Value);
      Kinematics_Parameters : constant Config.Config_Property_Parameters_Sequence :=
        Sequence_Parameters (Schema.Top_Level_Items, "Kinematics");
      Cornering_Parameters : constant Config.Config_Property_Parameters_Sequence :=
        Sequence_Parameters (Kinematics_Parameters.Children, "Cornering");
      Kind : constant Config.Config_Property_Parameters_Variant :=
        Config.Config_Property_Parameters_Variant (Cornering_Parameters.Children.Element ("Kind"));
      Stereographic_Description : constant Virtual_String :=
        Branch_Parameters (Kind, "Stereographic", "Stereographic_Params").Description;
      Circular_Description : constant Virtual_String :=
        Branch_Parameters (Kind, "Circular", "Circular_Params").Description;
      Parabolic_Description : constant Virtual_String :=
        Branch_Parameters (Kind, "Parabolic", "Parabolic_Params").Description;
      Biarc_Description : constant Virtual_String :=
        Branch_Parameters (Kind, "Biarc", "Biarc_Params").Description;
      Sharp_SCV_Description : constant Virtual_String :=
        Branch_Parameters (Kind, "Sharp_SCV", "Sharp_SCV_Params").Description;
   begin
      T.Register;

      T.Assert
        (Contains_Text (Stereographic_Description, "first four distance derivatives"),
         "Stereographic documents its higher-order endpoint continuity");
      T.Assert
        (Contains_Text (Circular_Description, "acceleration may jump at the endpoints")
         and then Contains_Text (Circular_Description, "Jerk, snap, and crackle limits do not apply"),
         "Circular documents its endpoint continuity waiver");
      T.Assert
        (Contains_Text (Parabolic_Description, "acceleration may jump at the endpoints")
         and then Contains_Text (Parabolic_Description, "Jerk, snap, and crackle limits do not apply"),
         "Parabolic documents its endpoint continuity waiver");
      T.Assert
        (Contains_Text (Biarc_Description, "internal arc splice")
         and then Contains_Text (Biarc_Description, "jerk, snap, and crackle limits do not apply"),
         "Biarc documents endpoint and splice continuity waivers");
      T.Assert
        (Contains_Text (Sharp_SCV_Description, "Velocity direction is discontinuous")
         and then Contains_Text
           (Sharp_SCV_Description, "acceleration, jerk, snap, and crackle limits intentionally do not apply"),
         "Sharp SCV documents its derivative-limit waiver");
   end Test_Cornering_Schema_Warnings;

   procedure Test_M205_Arguments (T : in out Trendy_Test.Operation'Class) is
      Module_Value : Module;
      Commands     : constant Gcode_Command_Vectors.Vector := Gcode_Commands (Module_Value);
      Found        : Natural := 0;
   begin
      T.Register;

      for Command of Commands loop
         if Command.Identifier.Argument = 'M' and then Command.Identifier.Number = 205 then
            Found := Found + 1;
            T.Assert (Command.Arguments.Length = 5, "M205 exposes exactly five arguments");
            T.Assert (Command.Arguments.Contains ('P'), "M205 exposes P");
            T.Assert (Command.Arguments.Contains ('A'), "M205 exposes A");
            T.Assert (Command.Arguments.Contains ('J'), "M205 exposes J");
            T.Assert (Command.Arguments.Contains ('S'), "M205 exposes S");
            T.Assert (Command.Arguments.Contains ('C'), "M205 exposes C");
            T.Assert (not Command.Arguments.Contains ('D'), "M205 no longer exposes D");
            T.Assert (not Command.Arguments.Contains ('R'), "M205 no longer exposes R");
            T.Assert (not Command.Arguments.Contains ('B'), "M205 no longer exposes B");
            T.Assert (not Command.Arguments.Contains ('Q'), "M205 no longer exposes Q");
         end if;
      end loop;

      T.Assert (Found = 1, "Kinematics defines M205 exactly once");
   end Test_M205_Arguments;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Cornering_Schema_Defaults_And_Visibility'Unrestricted_Access,
         Test_Cornering_Branch_Round_Trips'Unrestricted_Access,
         Test_Cornering_Schema_Warnings'Unrestricted_Access,
         Test_M205_Arguments'Unrestricted_Access];
   end All_Tests;

end Prunt.Default_Modules.Kinematics.Test;
