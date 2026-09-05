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

with Ada.IO_Exceptions;
with Ada.Directories;
with Prunt.Mockable.Faults;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Tags;
with Prunt.Config.Paths;
with Prunt.JSON;            use Prunt.JSON;
with Prunt.Mockable.Directories;
with Prunt.Mockable.Text_IO;

package body Prunt.Config.Test is

   pragma Extensions_Allowed (On);

   package Fault_Test_Lock is new Generic_Lock;

   P : Config_Presentation_Condition renames No_Presentation_Condition;
   --  Short test-local name for the unconditional value required by hand-built schema aggregates.

   type Presentation_Controller_Tag_Source is tagged null record;
   type Missing_Presentation_Controller_Tag_Source is tagged null record;

   function Presentation_Controller_Path return Config_Path
   is (Paths.Append (Paths.Empty_Path, ["Controller"]));

   function Missing_Presentation_Controller_Path return Config_Path
   is (Paths.Append (Paths.Empty_Path, ["Missing"]));

   function Migration_Data (Input : Virtual_String) return Config_Data is
     (For_Migration => (Ada.Finalization.Controlled with Active => True), Module => "Test", Migration_Config => Read (Input), others => <>);

   function Homing_Method_Path return Config_Path is
     (Paths.Append (Paths.Empty_Path, ["Homing", "Axes", "X", "Homing_Method"]));

   function Homing_Switch_Path return Config_Path is
      Detector_Path : constant Config_Path :=
        Paths.Append_Selected
          (Homing_Method_Path,
           ["Kind", "Children", "Home_To_Detector", "Home_To_Detector", "Detector"],
           ["Kind", "Selected"],
           "Home_To_Detector");
   begin
      return
        Paths.Append_Selected
          (Detector_Path,
           ["Kind", "Children", "Input_Switch", "Switch"],
           ["Kind", "Selected"],
           "Input_Switch");
   end Homing_Switch_Path;

   function Motion_Z_Position_Path return Config_Path is
      Pause_Path    : constant Config_Path := Paths.Append (Paths.Empty_Path, [1 => "Pause_Park"]);
      Absolute_Path : constant Config_Path :=
        Paths.Append_Selected
          (Pause_Path,
           ["Kind", "Children", "Absolute_Park_Move", "Absolute_Park_Move"],
           ["Kind", "Selected"],
           "Absolute_Park_Move");
      Z_Target_Path : constant Config_Path := Paths.Append (Absolute_Path, [1 => "Z_Target"]);
   begin
      return
        Paths.Append_Selected
          (Z_Target_Path,
           ["Kind", "Children", "Absolute_Z_Position", "Z_Position"],
           ["Kind", "Selected"],
           "Absolute_Z_Position");
   end Motion_Z_Position_Path;

   function TMC2240_Run_Current_Path return Config_Path is
     (Paths.Append (Paths.Empty_Path, ["Motors", "M", "TMC2240_Parameters", "Run_Current"]));

   procedure Assert_Config_Path_Unreportable
     (T : in out Trendy_Test.Operation'Class; Data : Config_Data; Path : Config_Path)
   is
   begin
      declare
         Resolved : constant Config_Data_Paths.Vector := Resolve_Config_Path (Data, Path);
         pragma Unreferenced (Resolved);
      begin
         T.Fail ("Configuration path should have raised Constraint_Error.");
      end;
   exception
      when Constraint_Error =>
         null;
   end Assert_Config_Path_Unreportable;

   function Reports_Error_Outer_Config (Input : Virtual_String) return Boolean is
      Error_Reported : Boolean := False;

      procedure Report (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
         pragma Unreferenced (Path, Message);
      begin
         Error_Reported := True;
      end Report;
   begin
      Validate_Outer_Config_Structure (Read (Input), Report'Access);
      return Error_Reported;
   end Reports_Error_Outer_Config;

   function Reports_Error_Module_Config (Input : Virtual_String) return Boolean is
      Error_Reported : Boolean := False;

      procedure Report (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
         pragma Unreferenced (Path);
         pragma Unreferenced (Message);
      begin
         Error_Reported := True;
      end Report;
   begin
      Validate_Module_Config_Structure (Read (Input), Report'Access);
      return Error_Reported;
   end Reports_Error_Module_Config;

   function Reports_Error_Module_Config_To_Schema
     (Input : Virtual_String; Schema : Config_Property_Maps.Map; Check_For_Missing_Fields : Boolean := False)
      return Boolean
   is
      Error_Reported : Boolean := False;

      procedure Report (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
         pragma Unreferenced (Path);
         pragma Unreferenced (Message);
      begin
         Error_Reported := True;
      end Report;
   begin
      Validate_Module_Config_To_Schema
        (Read (Input), Schema, Report'Access, Check_For_Missing_Fields => Check_For_Missing_Fields);
      return Error_Reported;
   end Reports_Error_Module_Config_To_Schema;

   function Override_Test_Schemas return Config_Schema_Maps.Map is
   begin
      return
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => Presentation_Controller_Tag_Source'Tag,
              Top_Level_Items =>
                ["s" =>
                   Config_Property_Parameters_Sequence'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Tabbed      => False,
                      Children    =>
                        ["i" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 1),
                         "j" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 2)])])];
   end Override_Test_Schemas;

   function Override_I return Config_Override_Vectors.Vector is
   begin
      return
        Config_Override_Vectors.Vector'
          ([Config_Override'
              (Owner => "M", Path => Config_Data_Paths.Vector'(["s", "i"]), Value => Create (Long_Long_Integer'(7)))]);
   end Override_I;

   function Read_Test_File (Filename : String) return JSON_Value is
      F       : Mockable.Text_IO.File_Type;
      Content : Unbounded_String;
   begin
      Mockable.Text_IO.Open (F, Mockable.Text_IO.In_File, Filename);
      while not Mockable.Text_IO.End_Of_File (F) loop
         Append (Content, Mockable.Text_IO.Get_Line (F));
      end loop;
      Mockable.Text_IO.Close (F);

      return Read (Conversions.To_Virtual_String (Content));
   end Read_Test_File;

   procedure Test_Apply_Patch_Errors (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      File_Name : constant String := Next_Test_Filename;
      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])];
      declare
         File : constant Config_File := Create (File_Name, Schemas);
         Errors : Config_Error_Vectors.Vector;
         Output : Virtual_String;
      begin

         File.Internal.Get.Apply_Untrusted_Patch ("{", Output, Errors);
         T.Assert (not Errors.Is_Empty, "Should report invalid JSON");
         Errors.Clear;

         File.Internal.Get.Apply_Untrusted_Patch ("{""Config"": {""Unknown"": {}}}", Output, Errors);
         T.Assert (not Errors.Is_Empty, "Should report unknown module");
         Errors.Clear;

         File.Internal.Get.Apply_Untrusted_Patch
           ("{""Config"": {""M"": {""Version"": 2, ""Config"": {}}}}", Output, Errors);
         T.Assert (not Errors.Is_Empty, "Should report wrong version");
         Errors.Clear;

         File.Internal.Get.Apply_Untrusted_Patch
           ("{""Config"": {""M"": {""Version"": 1, ""Config"": {}, ""Extra"": 1}}}", Output, Errors);
         T.Assert (not Errors.Is_Empty, "Should report extra field in module wrapper");
         Errors.Clear;

         File.Internal.Get.Apply_Untrusted_Patch
           ("{""Config"": {""M"": {""Version"": 1, ""Config"": {""i"": 5}}}, ""Prunt config version"": 1}",
            Output,
            Errors);
         T.Assert (Errors.Is_Empty, "Should not report error for valid patch");
         T.Assert (File.Get_Data ("M").Get (["i"]) = Long_Long_Integer'(0), "Patch not applied to live config");
      end;
      declare
         Reopened : constant Config_File := Create (File_Name, Schemas);
         Data : constant Config_Data := Reopened.Get_Data ("M");
      begin
         T.Assert (Data.Get (["i"]) = Long_Long_Integer'(5), "Patch applied to stored config");
      end;
   end Test_Apply_Patch_Errors;

   procedure Test_Apply_Untrusted_Patch_Empty (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);
      Output : Virtual_String;
      Errors : Config_Error_Vectors.Vector;

      Apply_Untrusted_Patch (File, "{""Prunt config version"": 1, ""Config"": {}}", Output, Errors);
      T.Assert (Errors.Is_Empty, "Should not report error");
   end Test_Apply_Untrusted_Patch_Empty;

   procedure Test_Apply_Untrusted_Patch_Invalid_Module (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])];
      Output : Virtual_String;
      File : constant Config_File := Create (Next_Test_Filename, Schemas);
      Errors : Config_Error_Vectors.Vector;

      File.Apply_Untrusted_Patch ("{""Config"": {""M"": []}}", Output, Errors);
      T.Assert (not Errors.Is_Empty, "Should report error for invalid module structure");
   end Test_Apply_Untrusted_Patch_Invalid_Module;

   procedure Test_Apply_Untrusted_Patch_Invalid_Module_Structure (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);
      Errors : Config_Error_Vectors.Vector;
      Output : Virtual_String;

      Apply_Untrusted_Patch
        (File, "{""Prunt config version"": 1, ""Config"": {""M"": {""Config"": {}}}}", Output, Errors);
      T.Assert (not Errors.Is_Empty, "Should have reported error for invalid module structure.");
   end Test_Apply_Untrusted_Patch_Invalid_Module_Structure;

   procedure Test_Apply_Untrusted_Patch_No_Config (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);
      Errors : Config_Error_Vectors.Vector;
      Output : Virtual_String;

      File.Apply_Untrusted_Patch ("{""Prunt config version"": 1}", Output, Errors);
      T.Assert (not Errors.Is_Empty, "Should report error for missing Config field");
   end Test_Apply_Untrusted_Patch_No_Config;

   procedure Test_Backup (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 100, Unit => "", Default => 0)])];
      Filename : constant String := Next_Test_Filename;
      File : constant Config_File := Create (Filename, Schemas);

      for I in 1 .. 25 loop
         Data : Config_Data := File.Get_Data ("M");
         Data.Set (Config_Data_Paths.Vector'([1 => "i"]), Long_Long_Integer (I));
         Data.Save;
      end loop;

      for I in 1 .. 20 loop
         Backup_Filename : constant String :=
           Filename & "_backup_" & Ada.Strings.Fixed.Trim (I'Image, Ada.Strings.Both);
         Backup_File : constant Config_File := Create (Backup_Filename, Schemas);
         Data : Config_Data := Backup_File.Get_Data ("M");
         Expected_Value : constant Long_Long_Integer := Long_Long_Integer (25 - I);

         T.Assert (Data.Get ([1 => "i"]) = Expected_Value, "Backup " & I'Image & " has value " & Expected_Value'Image);
      end loop;
   end Test_Backup;

   procedure Test_Config_Data_Not_Saved_Without_Call (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     => ["b" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => False)])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "b"]) = False);
         Data.Set ([1 => "b"], True);
         T.Assert (Data.Get ([1 => "b"]) = True);
      end;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "b"]) = False, "Field should not be saved without Save call.");
      end;
   end Test_Config_Data_Not_Saved_Without_Call;

   procedure Test_Config_Data_Ref_Count (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      Filename : constant String := Next_Test_Filename;
      Data : Config_Data
      with Unreferenced;

      declare
         File       : constant Config_File := Create (Filename, Schemas);
         Inner_Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Inner_Data in Config_Data);
         Data := Config_Data (Inner_Data);
      end; --  File finalizes here, but Data holds a ref.

      T.Fail ("Should have raised Program_Error during File finalization");
   exception
      when Program_Error =>
         null;
   end Test_Config_Data_Ref_Count;

   procedure Test_Config_Data_Set_Get_Boolean (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     => ["b" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => False)])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "b"]) = False);
         Data.Set ([1 => "b"], True);
         T.Assert (Data.Get ([1 => "b"]) = True);
         Data.Save;
      end;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "b"]) = True);
      end;
   end Test_Config_Data_Set_Get_Boolean;

   procedure Test_Config_Data_Set_Get_Dimensionless (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["f" =>
                   Config_Property_Parameters_Float'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0.0, Max => 10.0, Unit => "", Default => 5.5)])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "f"]) = Dimensionless'(5.5));
         Data.Set ([1 => "f"], Dimensionless'(2.5));
         T.Assert (Data.Get ([1 => "f"]) = Dimensionless'(2.5));
         Data.Save;

         begin
            Data.Set ([1 => "f"], Dimensionless'(11.0));
            Data.Save;
            T.Fail ("Should have raised Constraint_Error for out of range");
         exception
            when Constraint_Error =>
               null;
         end;
      end;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "f"]) = Dimensionless'(2.5));
      end;
   end Test_Config_Data_Set_Get_Dimensionless;

   procedure Test_Config_Data_Set_Get_Discrete (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["d" =>
                   Config_Property_Parameters_Discrete'(Experimental => False,
                      Present_When => P, Description => "", Default => "a", Options => ["a", "b"])])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "d"]) = "a");
         Data.Set ([1 => "d"], "b");
         T.Assert (Data.Get ([1 => "d"]) = "b");
         Data.Save;

         begin
            Data.Set ([1 => "d"], "c");
            Data.Save;
            T.Fail ("Should have raised Constraint_Error for invalid option");
         exception
            when Constraint_Error =>
               null;
         end;
      end;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "d"]) = "b");
      end;
   end Test_Config_Data_Set_Get_Discrete;

   procedure Test_Config_Data_Set_Get_Float (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["f" =>
                   Config_Property_Parameters_Float'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0.0, Max => 10.0, Unit => "", Default => 5.5)])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "f"]) = Long_Float'(5.5));
         Data.Set ([1 => "f"], Long_Float'(2.5));
         T.Assert (Data.Get ([1 => "f"]) = Long_Float'(2.5));
         Data.Save;

         begin
            Data.Set ([1 => "f"], Long_Float'(11.0));
            Data.Save;
            T.Fail ("Should have raised Constraint_Error for out of range");
         exception
            when Constraint_Error =>
               null;
         end;
      end;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "f"]) = Long_Float'(2.5));
      end;
   end Test_Config_Data_Set_Get_Float;

   procedure Test_Config_Data_Set_Get_Float_Ratio (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["r" =>
                   Config_Property_Parameters_Float_Ratio'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0.0, Max => 2.0, Default => (Numerator => 1.0, Denominator => 2.0))])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         declare
            Ratio : constant Dimensionless_Ratio := Data.Get ([1 => "r"]);
         begin
            T.Assert (Ratio.Numerator / Ratio.Denominator = 0.5);
         end;

         Data.Set ([1 => "r"], (Numerator => 2.0, Denominator => 2.0));

         declare
            Ratio : constant Dimensionless_Ratio := Data.Get ([1 => "r"]);
         begin
            T.Assert (Ratio.Numerator = 2.0);
            T.Assert (Ratio.Denominator = 2.0);
         end;

         Data.Save;
      end;

      declare
         File  : constant Config_File := Create (Filename, Schemas);
         Data  : Config_Data := File.Get_Data ("M");
         Ratio : constant Dimensionless_Ratio := Data.Get ([1 => "r"]);
      begin
         T.Assert (Ratio.Numerator = 2.0);
         T.Assert (Ratio.Denominator = 2.0);
      end;
   end Test_Config_Data_Set_Get_Float_Ratio;

   procedure Test_Config_Data_Set_Get_Integer (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 5)])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "i"]) = Long_Long_Integer'(5));
         Data.Set ([1 => "i"], Long_Long_Integer'(10));
         T.Assert (Data.Get ([1 => "i"]) = Long_Long_Integer'(10));
         Data.Save;

         begin
            Data.Set ([1 => "i"], Long_Long_Integer'(11));
            Data.Save;
            T.Fail ("Should have raised Constraint_Error for out of range");
         exception
            when Constraint_Error =>
               null;
         end;
      end;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "i"]) = Long_Long_Integer'(10));
      end;
   end Test_Config_Data_Set_Get_Integer;

   procedure Test_Config_Data_Set_Get_Variant (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["v" =>
                   Config_Property_Parameters_Variant'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Default     => "a",
                      Children    =>
                        ["a" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 1)])])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get (["v", "Selected"]) = "a");

         Data.Set (["v", "Children", "a"], Long_Long_Integer'(5));
         T.Assert (Data.Get (["v", "Children", "a"]) = 5);
         Data.Save;
      end;

      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get (["v", "Children", "a"]) = 5);
      end;
   end Test_Config_Data_Set_Get_Variant;

   procedure Test_Create_Default_Module_Config (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map :=
        ["b"  => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => True),
         "d"  => Config_Property_Parameters_Discrete'(Experimental => False,
                      Present_When => P, Description => "", Default => "a", Options => ["a", "b"]),
         "i"  =>
           Config_Property_Parameters_Integer'(Experimental => False,
                      Present_When => P, Description => "", Min => -10, Max => 10, Unit => "", Default => 5),
         "f"  =>
           Config_Property_Parameters_Float'(Experimental => False,
                      Present_When => P, Description => "", Min => -10.0, Max => 10.0, Unit => "", Default => 2.5),
         "fr" =>
           Config_Property_Parameters_Float_Ratio'
             (Experimental => False,
              Present_When => P, Description => "", Min => 0.0, Max => 2.0, Default => (Numerator => 1.0, Denominator => 2.0)),
         "s"  =>
           Config_Property_Parameters_Sequence'
             (Experimental => False,
              Present_When => P, Description => "",
              Tabbed      => False,
              Children    =>
                ["c" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 3)]),
         "v"  =>
           Config_Property_Parameters_Variant'
             (Experimental => False,
              Present_When => P, Description => "",
              Default     => "c",
              Children    =>
                ["c" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 4)])];
      Default : constant JSON_Value := Create_Default_Module_Config (Schema);

      T.Assert (Default.Get ("b").Get = True);
      T.Assert (Default.Get ("d").Get = "a");
      T.Assert (Long_Long_Integer'(Default.Get ("i").Get) = 5);
      T.Assert (Long_Float'(Default.Get ("f").Get) = 2.5);
      T.Assert (Long_Float'(Default.Get ("fr").Get ("Numerator").Get) = 1.0);
      T.Assert (Long_Float'(Default.Get ("fr").Get ("Denominator").Get) = 2.0);
      T.Assert (Long_Long_Integer'(Default.Get ("s").Get ("c").Get) = 3);
      T.Assert (Default.Get ("v").Get ("Selected").Get = "c");
      T.Assert (Long_Long_Integer'(Default.Get ("v").Get ("Children").Get ("c").Get) = 4);
      T.Assert (not Reports_Error_Module_Config_To_Schema (Default.Write, Schema, Check_For_Missing_Fields => True));
   end Test_Create_Default_Module_Config;

   procedure Test_Create_Default_Module_Config_Unhandled_Property (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map := ["u" => Config_Property_Parameters_Unknown'(Experimental => False,
                      Present_When => P, Description => "")];

      declare
         Default : constant JSON_Value := Create_Default_Module_Config (Schema);
         pragma Unreferenced (Default);
      begin
         T.Fail ("Should have raised Constraint_Error");
      end;
   exception
      when Constraint_Error =>
         null;
   end Test_Create_Default_Module_Config_Unhandled_Property;

   procedure Test_Deep_Set_Repeated (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["v" =>
                   Config_Property_Parameters_Variant'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Default     => "a",
                      Children    =>
                        ["a" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => False),
                         "b" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);
      Data : Config_Data := File.Get_Data ("M");

      Data.Set (Config_Data_Paths.Vector'(["v", "Children", "a"]), True);
      --  Same parent, should not recreate parent.
      Data.Set (Config_Data_Paths.Vector'(["v", "Children", "b"]), Long_Long_Integer'(5));
      Data.Set (Config_Data_Paths.Vector'(["v", "Children_2", "a"]), True);
      Data.Set (Config_Data_Paths.Vector'(["v", "Children_2", "b"]), Long_Long_Integer'(5));

      T.Assert (Data.Get (Config_Data_Paths.Vector'(["v", "Children", "a"])) = True);
      T.Assert (Data.Get (Config_Data_Paths.Vector'(["v", "Children", "b"])) = Long_Long_Integer'(5));
      T.Assert (Data.Get (Config_Data_Paths.Vector'(["v", "Children_2", "a"])) = True);
      T.Assert (Data.Get (Config_Data_Paths.Vector'(["v", "Children_2", "b"])) = Long_Long_Integer'(5));
   end Test_Deep_Set_Repeated;

   procedure Test_Finalize_Uninitialized (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      File : Config_File
      with Unreferenced;
      null;
   end Test_Finalize_Uninitialized;

   procedure Test_Generate_Schema_Unknown_Property (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["u" => Config_Property_Parameters_Unknown'(Experimental => False,
                      Present_When => P, Description => "")])];
      File : Config_File := Create (Next_Test_Filename, Schemas)
      with Unreferenced;

      T.Fail ("Should have raised Constraint_Error");
   exception
      when Constraint_Error =>
         null;
   end Test_Generate_Schema_Unknown_Property;

   procedure Test_Generate_Schemas_String_Unhandled_Property (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["u" => Config_Property_Parameters_Unknown'(Experimental => False,
                      Present_When => P, Description => "")])];
      S : constant Virtual_String := Generate_Schemas_String (Schemas)
      with Unreferenced;

      T.Fail ("Should have raised Constraint_Error");
   exception
      when Constraint_Error =>
         null;
   end Test_Generate_Schemas_String_Unhandled_Property;

   procedure Test_Get_Data_String (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "Test Integer", Min => 0, Max => 10, Unit => "", Default => 5)])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);
      Data_JSON : constant JSON_Value := Read (File.Get_Data_String);

      T.Assert (Data_JSON.Has_Field ("Prunt config version"));
      T.Assert (Data_JSON.Has_Field ("Config"));
      T.Assert (Data_JSON.Get ("Config").Has_Field ("M"));
      T.Assert (Long_Long_Integer'(Data_JSON.Get ("Config").Get ("M").Get ("Config").Get ("i").Get) = 5);
   end Test_Get_Data_String;

   procedure Test_Get_Dimensionless (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Val_Float : constant JSON_Value := Create (Long_Float'(1.23));
      Val_Int : constant JSON_Value := Create (Long_Long_Integer'(1));
      Val_Obj : constant JSON_Value := Create_Object;

      Val_Obj.Set_Field ("f", Val_Float);

      T.Assert (Get (Val_Float) = Dimensionless (1.23));
      T.Assert (Get (Val_Int) = Dimensionless (1.0));
      T.Assert (Get (Val_Obj, "f") = Dimensionless (1.23));
   end Test_Get_Dimensionless;

   procedure Test_Get_Empty_Path (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      Filename : constant String := Next_Test_Filename;
      File : constant Config_File := Create (Filename, Schemas);

      begin
         Val : JSON_Value := File.Internal.Get.Get ("M", Config_Data_Paths.Empty_Vector)
         with Unreferenced;
         T.Fail ("Should have raised Constraint_Error for empty path (Get)");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Get_Empty_Path;

   procedure Test_Get_Schemas_String (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => True,
                      Present_When => P, Description => "Test Integer", Min => 0, Max => 10, Unit => "", Default => 5)])];

      File : constant Config_File := Create (Next_Test_Filename, Schemas);

      Schema_JSON : constant JSON_Value := Read (File.Get_Schema_String);

      T.Assert (Schema_JSON.Has_Field ("Prunt config version"));
      T.Assert (Schema_JSON.Has_Field ("Config"));
      T.Assert (Schema_JSON.Get ("Config").Has_Field ("M"));
      T.Assert (Schema_JSON.Get ("Config").Get ("M").Get ("Config").Get ("i").Get ("Kind").Get = "Integer");
      T.Assert
        (Schema_JSON.Get ("Config").Get ("M").Get ("Config").Get ("i").Get ("Description").Get = "Test Integer");
      T.Assert (Schema_JSON.Get ("Config").Get ("M").Get ("Config").Get ("i").Get ("Experimental").Get = True);
   end Test_Get_Schemas_String;

   procedure Test_Get_Dynamically_Presented_Schema_String (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => Presentation_Controller_Tag_Source'Tag,
              Top_Level_Items     =>
                ["Controller" =>
                   Config_Property_Parameters_Variant'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Default     => "A",
                      Children    =>
                        ["A" => Config_Property_Parameters_Sequence'(Experimental => False,
                      Present_When => P, Description => "", Tabbed => False, Children => []),
                         "B" => Config_Property_Parameters_Sequence'(Experimental => False,
                      Present_When => P, Description => "", Tabbed => False, Children => [])]),
                 "s" =>
                   Config_Property_Parameters_Sequence'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Tabbed      => False,
                      Children    =>
                        ["i" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When =>
                                 (Controller_Tag  => Presentation_Controller_Tag_Source'Tag,
                                 Controller_Path => Presentation_Controller_Path,
                                 Values          => ["A", "B"]),
                              Description  => "",
                              Min          => 0,
                              Max          => 10,
                              Unit         => "",
                              Default      => 5)])])];
      Schema_JSON : constant JSON_Value := Read (Generate_Schemas_String (Schemas));
      Condition : constant JSON_Value :=
        Schema_JSON.Get ("Config").Get ("M").Get ("Config").Get ("s").Get ("Children").Get ("i").Get
          ("Present_When");
      Controller_Path : constant JSON_Array := Condition.Get_Array ("Path");
      Values : constant JSON_Array := Condition.Get_Array ("Values");

      T.Assert
        (Schema_JSON.Get ("Config").Get ("M").Get ("Config").Get ("s").Get ("Experimental").Get = False);
      T.Assert (Condition.Get ("Owner") = "M");
      T.Assert (Controller_Path.Length = 2);
      T.Assert (Controller_Path.Get (1).Get = "Controller");
      T.Assert (Controller_Path.Get (2).Get = "Selected");
      T.Assert (Values.Length = 2);
      T.Assert (Values.Get (1).Get = "A");
      T.Assert (Values.Get (2).Get = "B");
   end Test_Get_Dynamically_Presented_Schema_String;

   procedure Test_Dynamic_Presentation_Validation (T : in out Trendy_Test.Operation'Class) is
      function Schemas_For
        (Controller_Path : Config_Path;
         Values          : Discrete_String_Sets.Set;
         Duplicate       : Boolean := False;
         Controller_Tag  : Ada.Tags.Tag := Presentation_Controller_Tag_Source'Tag) return Config_Schema_Maps.Map;

      function Schemas_For
        (Controller_Path : Config_Path;
         Values          : Discrete_String_Sets.Set;
         Duplicate       : Boolean := False;
         Controller_Tag  : Ada.Tags.Tag := Presentation_Controller_Tag_Source'Tag) return Config_Schema_Maps.Map
      is
         Controller : constant Config_Property_Parameters_Discrete :=
           (Experimental => False,
            Present_When => P, Description => "", Default => "A", Options => ["A", "B"]);
         Target : constant Config_Property_Parameters_Integer :=
           (Experimental => False,
            Present_When =>
              (Controller_Tag => Controller_Tag, Controller_Path => Controller_Path, Values => Values),
            Description  => "",
            Min          => 0,
            Max          => 10,
            Unit         => "",
            Default      => 5);
         Result : Config_Schema_Maps.Map :=
           ["M" =>
              Versioned_Config_Schema'
                (Version             => 1,
                 Module_Instance_Tag => Presentation_Controller_Tag_Source'Tag,
                 Top_Level_Items     => ["Controller" => Controller, "Target" => Target])];
      begin
         if Duplicate then
            Result.Insert
              ("N",
               Versioned_Config_Schema'
                 (Version             => 1,
                  Module_Instance_Tag => Presentation_Controller_Tag_Source'Tag,
                  Top_Level_Items     => ["Controller" => Controller]));
         end if;
         return Result;
      end Schemas_For;

      procedure Expect_Error (Schemas : Config_Schema_Maps.Map; Message : String);

      procedure Expect_Error (Schemas : Config_Schema_Maps.Map; Message : String) is
      begin
         declare
            Ignore : constant Virtual_String := Generate_Schemas_String (Schemas);
         begin
            pragma Unreferenced (Ignore);
            T.Fail (Message);
         end;
      exception
         when Constraint_Error =>
            null;
      end Expect_Error;

      Controller_Path : constant Config_Path := Presentation_Controller_Path;
   begin
      T.Register;
      Expect_Error
        (Schemas_For (Missing_Presentation_Controller_Path, ["A"]),
         "missing controller path must be rejected");
      Expect_Error (Schemas_For (Controller_Path, ["Missing"]), "invalid controller value must be rejected");
      Expect_Error
        (Schemas_For (Controller_Path, ["A"], Duplicate => True), "ambiguous controller tag must be rejected");
      Expect_Error (Schemas_For (Controller_Path, []), "empty controller values must be rejected");
      Expect_Error
        (Schemas_For
           (Controller_Path,
            ["A"],
            Controller_Tag => Missing_Presentation_Controller_Tag_Source'Tag),
         "missing controller tag must be rejected");
   end Test_Dynamic_Presentation_Validation;

   procedure Test_Initialize_Corrupt_File (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : Mockable.Text_IO.File_Type;
      begin
         Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
         Mockable.Text_IO.Put_Line (File, "{ invalid json");
         Mockable.Text_IO.Close (File);
      end;

      begin
         File : Config_File := Create (Filename, Schemas)
         with Unreferenced;
         T.Fail ("Should have raised Invalid_JSON_Stream for corrupt file");
      exception
         when Invalid_JSON_Stream =>
            null;
      end;
   end Test_Initialize_Corrupt_File;

   procedure Test_Initialize_Default_Migration (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 2,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 5),
                 "j" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 5)])];

      Filename : constant String := Next_Test_Filename;
      Content : constant String :=
        "{"
        & """Prunt config version"": 1,"
        & """Config"": {"
        & "   ""M"": {"
        & "      ""Version"": 1,"
        & "      ""Config"": {""i"": 2}"
        & "   }"
        & "}"
        & "}";

      declare
         F : Mockable.Text_IO.File_Type;
      begin
         Mockable.Text_IO.Create (F, Mockable.Text_IO.Out_File, Filename);
         Mockable.Text_IO.Put_Line (F, Content);
         Mockable.Text_IO.Close (F);
      end;

      File : constant Config_File := Create (Filename, Schemas);
      pragma Unreferenced (File);

      declare
         F       : Mockable.Text_IO.File_Type;
         Content : Unbounded_String;
      begin
         Mockable.Text_IO.Open (F, Mockable.Text_IO.In_File, Filename);
         while not Mockable.Text_IO.End_Of_File (F) loop
            Append (Content, Mockable.Text_IO.Get_Line (F));
         end loop;
         Mockable.Text_IO.Close (F);

         Val : constant JSON_Value := Read (Conversions.To_Virtual_String (Content));
         T.Assert (Val.Get ("Config").Get ("M").Get ("Version").Get = 2, "Version updated");
         T.Assert (Val.Get ("Config").Get ("M").Get ("Config").Get ("i").Get = 2, "Existing value preserved");
         T.Assert (Val.Get ("Config").Get ("M").Get ("Config").Get ("j").Get = 5, "New field with default");
      end;
   end Test_Initialize_Default_Migration;

   procedure Test_Initialize_Extra_Modules (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : Mockable.Text_IO.File_Type;
      begin
         Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
         Mockable.Text_IO.Put_Line
           (File,
            "{""Prunt config version"": 1, ""Config"": {""M"": {""Version"": 1, ""Config"": {}}, ""EXTRA"": {}}}");
         Mockable.Text_IO.Close (File);
      end;

      begin
         File : Config_File := Create (Filename, Schemas)
         with Unreferenced;
         T.Fail ("Should have raised Constraint_Error for extra module");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Initialize_Extra_Modules;

   procedure Test_Initialize_Newer_Module_Version (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      Filename : constant String := Next_Test_Filename;

      declare
         File : Mockable.Text_IO.File_Type;
      begin
         Mockable.Text_IO.Create (File, Mockable.Text_IO.Out_File, Filename);
         Mockable.Text_IO.Put_Line
           (File, "{""Prunt config version"": 1, ""Config"": {""M"": {""Version"": 2, ""Config"": {}}}}");
         Mockable.Text_IO.Close (File);
      end;

      begin
         File : Config_File := Create (Filename, Schemas)
         with Unreferenced;
         T.Fail ("Should have raised Constraint_Error for newer version");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Initialize_Newer_Module_Version;

   procedure Test_Initialize_Newer_Version (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filename : constant String := Next_Test_Filename;
      Content : constant String :=
        "{"
        & """Prunt config version"": 1,"
        & """Config"": {"
        & "   ""M"": {"
        & "      ""Version"": 2,"
        & "      ""Config"": {}"
        & "   }"
        & "}"
        & "}";
      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];

      begin
         F : Mockable.Text_IO.File_Type;
         Mockable.Text_IO.Create (F, Mockable.Text_IO.Out_File, Filename);
         Mockable.Text_IO.Put_Line (F, Content);
         Mockable.Text_IO.Close (F);
      end;

      begin
         File : Config_File := Create (Filename, Schemas)
         with Unreferenced;
         T.Fail ("Should have raised Constraint_Error for newer version");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Initialize_Newer_Version;

   procedure Test_Initialize_No_Config (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map := [];

      File_Name : constant String := Next_Test_Filename;

      begin
         F : Mockable.Text_IO.File_Type;
         Mockable.Text_IO.Create (F, Mockable.Text_IO.Out_File, File_Name);
         Mockable.Text_IO.Put_Line (F, "{""Prunt config version"": 1}");
         Mockable.Text_IO.Close (F);
      end;

      begin
         File : Config_File := Create (File_Name, Schemas)
         with Unreferenced;
         T.Fail ("Should raise Constraint_Error for missing Config field");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Initialize_No_Config;

   procedure Test_Initialize_Unknown_Module (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map := [];
      Filename : constant String := Next_Test_Filename;
      Content : constant String :=
        "{"
        & """Prunt config version"": 1,"
        & """Config"": {"
        & "   ""Unknown"": {"
        & "      ""Version"": 1,"
        & "      ""Config"": {}"
        & "   }"
        & "}"
        & "}";

      begin
         F : Mockable.Text_IO.File_Type;
         Mockable.Text_IO.Create (F, Mockable.Text_IO.Out_File, Filename);
         Mockable.Text_IO.Put_Line (F, Content);
         Mockable.Text_IO.Close (F);
      end;

      begin
         File : Config_File := Create (Filename, Schemas)
         with Unreferenced;
         T.Fail ("Should have raised Constraint_Error for unknown module");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Initialize_Unknown_Module;

   procedure Test_Initialize_With_Invalid_Module_Config (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])];

      File_Name : constant String := Next_Test_Filename;

      if Mockable.Directories.Exists (File_Name) then
         Mockable.Directories.Delete_File (File_Name);
      end if;
      declare
         F : Mockable.Text_IO.File_Type;
      begin
         Mockable.Text_IO.Create (F, Mockable.Text_IO.Out_File, File_Name);
         Mockable.Text_IO.Put_Line (F, "{""Prunt config version"": 1, ""Config"": {""M"": []}}");
         Mockable.Text_IO.Close (F);
      end;

      begin
         File : Config_File := Create (File_Name, Schemas)
         with Unreferenced;
         T.Fail ("Should have raised Constraint_Error for invalid module config type");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Initialize_With_Invalid_Module_Config;

   procedure Test_Initialize_With_Invalid_Module_Data (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])];
      File_Name : constant String := Next_Test_Filename;

      declare
         F : Mockable.Text_IO.File_Type;
      begin
         Mockable.Text_IO.Create (F, Mockable.Text_IO.Out_File, File_Name);
         Mockable.Text_IO.Put_Line
           (F, "{""Prunt config version"": 1, ""Config"": {""M"": {""Version"": 1, ""Config"": {""i"": 99}}}}");
         Mockable.Text_IO.Close (F);
      end;

      begin
         File : Config_File := Create (File_Name, Schemas)
         with Unreferenced;
         T.Fail ("Should have raised Constraint_Error for invalid module data");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Initialize_With_Invalid_Module_Data;

   procedure Test_Invalid_Module_Calls (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      Filename : constant String := Next_Test_Filename;
      File : constant Config_File := Create (Filename, Schemas);

      begin
         declare
            Val : JSON_Value := File.Internal.Get.Get ("Invalid", Config_Data_Paths.Empty_Vector);
            pragma Unreferenced (Val);
         begin
            T.Fail ("Should have raised Constraint_Error for invalid module (Get)");
         end;
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         File.Internal.Get.Set ("Invalid", Config_Data_Paths.Empty_Vector, Create (True));
         T.Fail ("Should have raised Constraint_Error for invalid module (Set)");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Invalid_Module_Calls;

   procedure Test_Last_Save_Increment (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     => ["b" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => True)])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);
      Data : Config_Data := File.Get_Data ("M");
      S1 : constant Save_Counter := File.Last_Save;
      S2 : Save_Counter;

      Data.Set (Config_Data_Paths.Vector'([1 => "b"]), False);
      Data.Save;
      S2 := File.Last_Save;
      T.Assert (S2 > S1, "Save counter incremented");
   end Test_Last_Save_Increment;

   procedure Test_Merge_Schemas (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M1" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["s" =>
                   Config_Property_Parameters_Sequence'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Tabbed      => False,
                      Children    =>
                        ["x" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 1)])]),
         "M2" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["s" =>
                   Config_Property_Parameters_Sequence'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Tabbed      => False,
                      Children    =>
                        ["y" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 2)])])];
      Filename : constant String := Next_Test_Filename;

      File : Config_File := Create (Filename, Schemas)
      with Unreferenced;
      null;
   end Test_Merge_Schemas;

   procedure Test_Merge_Schemas_Conflict_Diff_Types (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M1" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["x" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)]),
         "M2" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     => ["x" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => False)])];
      Filename : constant String := Next_Test_Filename;

      begin
         File : Config_File := Create (Filename, Schemas)
         with Unreferenced;
         T.Fail ("Should have raised Constraint_Error for type conflict");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Merge_Schemas_Conflict_Diff_Types;

   procedure Test_Merge_Schemas_Conflict_Variant_Diff_Default (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M1" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["v" => Config_Property_Parameters_Variant'(Experimental => False,
                      Present_When => P, Description => "", Default => "a", Children => [])]),
         "M2" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["v" => Config_Property_Parameters_Variant'(Experimental => False,
                      Present_When => P, Description => "", Default => "b", Children => [])])];
      Filename : constant String := Next_Test_Filename;

      begin
         File : Config_File := Create (Filename, Schemas)
         with Unreferenced;
         T.Fail ("Should have raised Constraint_Error for variant default conflict");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Merge_Schemas_Conflict_Variant_Diff_Default;

   procedure Test_Patch_Inner_Errors (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);

      Errors : Config_Error_Vectors.Vector;

      Output : Virtual_String;

      File.Apply_Untrusted_Patch
        ("{""Prunt config version"": 1, ""Config"": {""M"": {""Version"": 1, ""Config"": {""Extra"": 1}}}}",
         Output,
         Errors);
      T.Assert (not Errors.Is_Empty, "Should report error for extra field in inner config");
   end Test_Patch_Inner_Errors;

   procedure Test_Patch_Invalid_Module (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);

      Errors : Config_Error_Vectors.Vector;

      Output : Virtual_String;

      File.Apply_Untrusted_Patch
        ("{""Prunt config version"": 1, ""Config"": {""INVALID"": {""Version"": 1, ""Config"": {}}}}", Output, Errors);
      T.Assert (not Errors.Is_Empty, "Should report error for unknown module");
   end Test_Patch_Invalid_Module;

   procedure Test_Patch_Success (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     => ["b" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => False)])];
      Filename : constant String := Next_Test_Filename;
      declare
         File : constant Config_File := Create (Filename, Schemas);
         Data : Config_Data := File.Get_Data ("M");

         Errors : Config_Error_Vectors.Vector;

         Output : Virtual_String;
      begin

         T.Assert (Data.Get ([1 => "b"]) = False);

         File.Internal.Get.Apply_Untrusted_Patch
           ("{""Prunt config version"": 1, ""Config"": {""M"": {""Version"": 1, ""Config"": {""b"": true}}}}",
            Output,
            Errors);

         T.Assert (Errors.Is_Empty, "Should not report error for valid patch");
      end;
      declare
         Reopened : constant Config_File := Create (Filename, Schemas);
         Data : constant Config_Data := Reopened.Get_Data ("M");
      begin
         T.Assert (Data.Get ([1 => "b"]) = True, "Value updated by patch");
      end;
   end Test_Patch_Success;

   procedure Test_Patch_Wrong_Version (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);

      Errors : Config_Error_Vectors.Vector;

      Output : Virtual_String;

      File.Internal.Get.Apply_Untrusted_Patch
        ("{""Prunt config version"": 1, ""Config"": {""M"": {""Version"": 2, ""Config"": {}}}}", Output, Errors);
      T.Assert (not Errors.Is_Empty, "Should report error for wrong version");
   end Test_Patch_Wrong_Version;

   procedure Test_Path_Errors (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["v"    =>
                   Config_Property_Parameters_Variant'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Default     => "a",
                      Children    => ["a" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => True)]),
                 "r"    =>
                   Config_Property_Parameters_Float_Ratio'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0.0, Max => 1.0, Default => (Numerator => 0.5, Denominator => 1.0)),
                 "s"    =>
                   Config_Property_Parameters_Sequence'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Tabbed      => False,
                      Children    =>
                        ["i" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)]),
                 "leaf" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => True)])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);
      Data : Config_Data := File.Get_Data ("M");

      begin
         declare
            Val : constant Boolean := Data.Get (Config_Data_Paths.Vector'([1 => "nonexistent"]));
            pragma Unreferenced (Val);
         begin
            T.Fail ("Should have raised Constraint_Error for nonexistent field (Get)");
         end;
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         Data.Set (Config_Data_Paths.Vector'([1 => "nonexistent"]), True);
         Data.Save;
         T.Fail ("Should have raised Constraint_Error for nonexistent field (Set)");
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         declare
            Val : constant Long_Long_Integer := Data.Get (Config_Data_Paths.Vector'(["v", "Invalid"]));
            pragma Unreferenced (Val);
         begin
            T.Fail ("Should have raised Constraint_Error for invalid Variant path (Get)");
         end;
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         Data.Set (Config_Data_Paths.Vector'(["v", "Invalid"]), Long_Long_Integer'(1));
         Data.Save;
         T.Fail ("Should have raised Constraint_Error for invalid Variant path (Set)");
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         declare
            Val : constant Long_Float := Data.Get (Config_Data_Paths.Vector'(["r", "Invalid"]));
            pragma Unreferenced (Val);
         begin
            T.Fail ("Should have raised Constraint_Error for invalid Float_Ratio path (Get)");
         end;
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         Data.Set (Config_Data_Paths.Vector'(["r", "Invalid"]), Long_Float'(1.0));
         Data.Save;

         T.Fail ("Should have raised Constraint_Error for invalid Float_Ratio path (Set)");
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         Data.Set (Config_Data_Paths.Vector'(["leaf", "something"]), True);
         Data.Save;
         T.Fail ("Should have raised Constraint_Error for leaf as middle of path");
      exception
         when Constraint_Error =>
            null;
      end;

      Data.Set (Config_Data_Paths.Vector'(["s", "i"]), Long_Long_Integer'(5));
      T.Assert (Data.Get (Config_Data_Paths.Vector'(["s", "i"])) = Long_Long_Integer'(5));

      begin
         Data.Set (Config_Data_Paths.Empty_Vector, True);
         T.Fail ("Should have raised Constraint_Error for empty path in Set");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Path_Errors;

   procedure Test_Recursive_Left_Merge_Bad_Types (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Left : constant JSON_Value := Create (Integer'(1));
      Right : constant JSON_Value := Create_Object;

      Right.Set_Field ("a", Integer'(2));
      begin
         Recursive_Left_Merge (Left, Right);
         T.Fail ("Should have raised Constraint_Error when merging object into integer");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Recursive_Left_Merge_Bad_Types;

   procedure Test_Recursive_Merge (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      L : JSON_Value := Create_Object;
      R : JSON_Value := Create_Object;

      L.Set_Field ("obj", Create_Object);
      L.Get ("obj").Set_Field ("a", Create (Long_Long_Integer'(1)));
      R.Set_Field ("obj", Create_Object);
      R.Get ("obj").Set_Field ("b", Create (Long_Long_Integer'(2)));

      Recursive_Left_Merge (L, R);
      T.Assert (L.Get ("obj").Has_Field ("a") and L.Get ("obj").Has_Field ("b"), "Objects merged");

      L := Create_Object;
      L.Set_Field ("a", Create (Long_Long_Integer'(1)));
      R := Create_Object;
      R.Set_Field ("a", Create (Long_Long_Integer'(2)));
      R.Set_Field ("b", Create (Long_Long_Integer'(3)));

      Recursive_Left_Merge (L, R, Full_Join => False);
      T.Assert (L.Get ("a").Get = Long_Long_Integer'(2), "Existing field updated");
      T.Assert (not L.Has_Field ("b"), "New field should not be added with Full_Join => False");
   end Test_Recursive_Merge;

   procedure Test_Recursive_Merge_Edge_Cases (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Left, Right : JSON_Value;

      Left := Create_Object;
      Left.Set_Field ("a", Integer'(1));

      Right := Create_Object;
      Right.Set_Field ("a", Create_Object);
      Right.Get ("a").Set_Field ("b", Integer'(2));

      Recursive_Left_Merge (Left, Right);
      T.Assert (Left.Get ("a").Kind = JSON_Object_Type, "Leaf integer should be overwritten by Object");
      T.Assert (Left.Get ("a").Get ("b") = Integer'(2), "Merged object content matches");

      Left := Create_Object;
      Left.Set_Field ("a", Create_Object);
      Left.Get ("a").Set_Field ("b", Integer'(2));

      Right := Create_Object;
      Right.Set_Field ("a", Integer'(1));

      Recursive_Left_Merge (Left, Right);
      T.Assert (Left.Get ("a").Kind = JSON_Int_Type, "Object should be overwritten by Leaf");
      T.Assert (Left.Get ("a").Get = Integer'(1), "Merged leaf content matches");
   end Test_Recursive_Merge_Edge_Cases;

   procedure Test_Save_No_Changes (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])];
      Filename : constant String := Next_Test_Filename;
      File : constant Config_File := Create (Filename, Schemas);
      Data : Config_Data := File.Get_Data ("M");

      Data.Save;
   end Test_Save_No_Changes;

   procedure Test_Schema_Conflict_Compatible_Disjoint (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M1" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)]),
         "M2" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["f" =>
                   Config_Property_Parameters_Float'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0.0, Max => 10.0, Unit => "", Default => 0.0)])];

      File : Config_File := Create (Next_Test_Filename, Schemas)
      with Unreferenced;
   end Test_Schema_Conflict_Compatible_Disjoint;

   procedure Test_Schema_Conflict_Compatible_Merged (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M1" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["s" =>
                   Config_Property_Parameters_Sequence'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Tabbed      => False,
                      Children    =>
                        ["i" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])]),
         "M2" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["s" =>
                   Config_Property_Parameters_Sequence'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Tabbed      => False,
                      Children    =>
                        ["f" =>
                           Config_Property_Parameters_Float'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0.0, Max => 10.0, Unit => "", Default => 0.0)])])];

      File : Config_File := Create (Next_Test_Filename, Schemas)
      with Unreferenced;
   end Test_Schema_Conflict_Compatible_Merged;

   procedure Test_Schema_Conflict_Type_Mismatch (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M1" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)]),
         "M2" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Float'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0.0, Max => 10.0, Unit => "", Default => 0.0)])];

      begin
         begin
            File : Config_File := Create (Next_Test_Filename, Schemas)
            with Unreferenced;
            T.Fail ("Conflicting types should raise error.");
         exception
            when Constraint_Error =>
               null;
         end;
      end;
   end Test_Schema_Conflict_Type_Mismatch;

   procedure Test_Schema_Conflict_Variant_Failure_Default (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M1" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["v" => Config_Property_Parameters_Variant'(Experimental => False,
                      Present_When => P, Description => "", Default => "a", Children => [])]),
         "M2" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["v" => Config_Property_Parameters_Variant'(Experimental => False,
                      Present_When => P, Description => "", Default => "b", Children => [])])];

      begin
         File : Config_File := Create (Next_Test_Filename, Schemas)
         with Unreferenced;
         T.Fail ("Conflicting variant defaults should raise error.");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Schema_Conflict_Variant_Failure_Default;

   procedure Test_Schema_Conflict_Variant_Mismatch_Keys (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M1" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["v" =>
                   Config_Property_Parameters_Variant'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Default     => "a",
                      Children    =>
                        ["a" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => False)])]),
         "M2" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["v" =>
                   Config_Property_Parameters_Variant'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Default     => "a",
                      Children    =>
                        ["b" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => False)])])];

      begin
         File : Config_File := Create (Next_Test_Filename, Schemas)
         with Unreferenced;
         T.Fail ("Conflicting variant keys should raise error.");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Schema_Conflict_Variant_Mismatch_Keys;

   procedure Test_Validate_Module_Config_Structure (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      T.Assert (Reports_Error_Module_Config ("{}"), "Empty object.");
      T.Assert (Reports_Error_Module_Config ("[]"), "Type mismatch.");
      T.Assert (Reports_Error_Module_Config ("{""Config"": {}}"), "Missing field.");
      T.Assert (Reports_Error_Module_Config ("{""Version"": 1}"), "Missing field.");
      T.Assert (Reports_Error_Module_Config ("{""Config"": 123, ""Version"": 1}"), "Type mismatch.");
      T.Assert (Reports_Error_Module_Config ("{""Config"": {}, ""Version"": 1.0}"), "Type mismatch.");
      T.Assert (Reports_Error_Module_Config ("{""Config"": {}, ""Version"": 1, ""a"": 1}"), "Extra field.");
      T.Assert
        (not Reports_Error_Module_Config ("{""Config"": {}, ""Version"": 1}"), "Valid object should not cause error.");
   end Test_Validate_Module_Config_Structure;

   procedure Test_Validate_Module_Config_To_Schema_Boolean (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map :=
        ["b" => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => False)];

      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""b"": 1}", Schema, Check_For_Missing_Fields => True),
         "Boolean wrong type.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""b"": true}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
   end Test_Validate_Module_Config_To_Schema_Boolean;

   procedure Test_Validate_Module_Config_To_Schema_Check_Missing_Fields (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map :=
        ["i" =>
           Config_Property_Parameters_Integer'(Experimental => False,
                      Present_When => P, Description => "", Min => -10, Max => 10, Unit => "", Default => 0),
         "v" =>
           Config_Property_Parameters_Variant'
             (Experimental => False,
              Present_When => P, Description => "",
              Default     => "c",
              Children    =>
                ["c" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0),
                 "d" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])];

      T.Assert (not Reports_Error_Module_Config_To_Schema ("{}", Schema, Check_For_Missing_Fields => False));
      T.Assert (Reports_Error_Module_Config_To_Schema ("{}", Schema, Check_For_Missing_Fields => True));
      T.Assert (Reports_Error_Module_Config_To_Schema ("{""i"": 1}", Schema, Check_For_Missing_Fields => True));
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""i"": 1, ""v"": {""Selected"": ""c"", ""Children"": {""c"": 1, ""d"": 1}}}",
                Schema,
                Check_For_Missing_Fields => True),
         "All fields present.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""i"": 1, ""v"": {}}", Schema, Check_For_Missing_Fields => True),
         "Missing Variant Selected/Children.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""i"": 1, ""v"": {""Selected"": ""c""}}", Schema, Check_For_Missing_Fields => True),
         "Missing Variant children.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""i"": 1, ""v"": {""Selected"": ""c"", ""Children"": {""c"": 1}}}",
            Schema,
            Check_For_Missing_Fields => True),
         "Missing unselected Variant child.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""i"": 1, ""v"": {""Selected"": ""d"", ""Children"": {""c"": 1}}}",
            Schema,
            Check_For_Missing_Fields => True),
         "Missing selected Variant child.");
   end Test_Validate_Module_Config_To_Schema_Check_Missing_Fields;

   procedure Test_Validate_Module_Config_To_Schema_Discrete (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map :=
        ["d" => Config_Property_Parameters_Discrete'(Experimental => False,
                      Present_When => P, Description => "", Default => "a", Options => ["a", "b"])];

      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""d"": 1}", Schema, Check_For_Missing_Fields => True),
         "Discrete wrong type.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""d"": ""c""}", Schema, Check_For_Missing_Fields => True),
         "Discrete invalid option.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""d"": ""a""}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
   end Test_Validate_Module_Config_To_Schema_Discrete;

   procedure Test_Validate_Module_Config_To_Schema_Empty (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map := [];

      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""a"": 1}", Schema, Check_For_Missing_Fields => True),
         "Extra field.");
   end Test_Validate_Module_Config_To_Schema_Empty;

   procedure Test_Validate_Module_Config_To_Schema_String (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map :=
        ["s" => Config_Property_Parameters_String'(Experimental => False,
                      Present_When => P, Description => "", Default => "default")];

      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""s"": 1}", Schema, Check_For_Missing_Fields => True),
         "String wrong type.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""s"": ""value""}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
   end Test_Validate_Module_Config_To_Schema_String;

   procedure Test_Validate_Module_Config_To_Schema_Float (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map :=
        ["f" =>
           Config_Property_Parameters_Float'
             (Experimental => False,
              Present_When => P, Description => "", Min => -10.0, Max => 10.0, Unit => "", Default => 0.0)];

      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""f"": ""a""}", Schema, Check_For_Missing_Fields => True),
         "Float wrong type.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""f"": -11.0}", Schema, Check_For_Missing_Fields => True),
         "Float out of range (min).");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""f"": 11.0}", Schema, Check_For_Missing_Fields => True),
         "Float out of range (max).");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""f"": 5.0}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""f"": 10.0}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""f"": -10.0}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""f"": 5}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""f"": 10}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""f"": -10}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
   end Test_Validate_Module_Config_To_Schema_Float;

   procedure Test_Validate_Module_Config_To_Schema_Float_Ratio (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map :=
        ["fr" =>
           Config_Property_Parameters_Float_Ratio'
             (Experimental => False,
              Present_When => P, Description => "", Min => -2.0, Max => 2.0, Default => (Numerator => 1.0, Denominator => 1.0))];

      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""fr"": 1}", Schema, Check_For_Missing_Fields => True),
         "Float_Ratio wrong type.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""fr"": {}}", Schema, Check_For_Missing_Fields => True),
         "Float_Ratio missing fields.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""fr"": {}}", Schema, Check_For_Missing_Fields => True),
         "Float_Ratio missing fields.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""fr"": {""Numerator"": 1}}", Schema, Check_For_Missing_Fields => True),
         "Float_Ratio missing Denominator.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""fr"": {""Denominator"": 1}}", Schema, Check_For_Missing_Fields => True),
         "Float_Ratio missing Numerator.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""fr"": {""Numerator"": ""a"", ""Denominator"": 1}}", Schema, Check_For_Missing_Fields => True),
         "Float_Ratio wrong type for Numerator.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""fr"": {""Numerator"": 1, ""Denominator"": ""a""}}", Schema, Check_For_Missing_Fields => True),
         "Float_Ratio wrong type for Denominator.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""fr"": {""Numerator"": 1, ""Denominator"": 0}}", Schema, Check_For_Missing_Fields => True),
         "Float_Ratio denominator is zero.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""fr"": {""Numerator"": 3, ""Denominator"": 1}}", Schema, Check_For_Missing_Fields => True),
         "Float_Ratio out of range.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""fr"": {""Numerator"": -3, ""Denominator"": 1}}", Schema, Check_For_Missing_Fields => True),
         "Float_Ratio out of range.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 1, ""Denominator"": 1}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 0, ""Denominator"": 1}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 1000, ""Denominator"": 500}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 1.0, ""Denominator"": 1.0}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 0.0, ""Denominator"": 1.0}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 1000.0, ""Denominator"": 500.0}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": -1, ""Denominator"": 1}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": -0, ""Denominator"": 1}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": -1000, ""Denominator"": 500}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": -1.0, ""Denominator"": 1.0}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": -0.0, ""Denominator"": 1.0}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": -1000.0, ""Denominator"": 500.0}}",
                Schema,
                Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 1, ""Denominator"": -1}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 0, ""Denominator"": -1}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 1000, ""Denominator"": -500}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 1.0, ""Denominator"": -1.0}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 0.0, ""Denominator"": -1.0}}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""fr"": {""Numerator"": 1000.0, ""Denominator"": -500.0}}",
                Schema,
                Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""fr"": {""Numerator"": 1, ""Denominator"": 1, ""Extra"": 1}}",
            Schema,
            Check_For_Missing_Fields => True),
         "Float_Ratio with extra field.");
   end Test_Validate_Module_Config_To_Schema_Float_Ratio;

   procedure Test_Validate_Module_Config_To_Schema_Integer (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map :=
        ["i" =>
           Config_Property_Parameters_Integer'(Experimental => False,
                      Present_When => P, Description => "", Min => -10, Max => 10, Unit => "", Default => 0)];

      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""i"": 1.0}", Schema, Check_For_Missing_Fields => True),
         "Integer wrong type.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""i"": -11}", Schema, Check_For_Missing_Fields => True),
         "Integer out of range (min).");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""i"": 11}", Schema, Check_For_Missing_Fields => True),
         "Integer out of range (max).");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""i"": -10}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""i"": 10}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""i"": 5}", Schema, Check_For_Missing_Fields => True),
         "Valid object should not cause error.");
   end Test_Validate_Module_Config_To_Schema_Integer;

   procedure Test_Validate_Module_Config_To_Schema_Sequence (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map :=
        ["s" =>
           Config_Property_Parameters_Sequence'
             (Experimental => False,
              Present_When => P, Description => "",
              Tabbed      => False,
              Children    =>
                ["c" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0),
                 "d" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])];

      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""s"": 1}", Schema, Check_For_Missing_Fields => False),
         "Sequence wrong type.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""s"": { ""c"": 11}}", Schema, Check_For_Missing_Fields => False),
         "Sequence child out of range.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""s"": { ""e"": 1}}", Schema, Check_For_Missing_Fields => False),
         "Sequence extra field.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""s"": {}}", Schema, Check_For_Missing_Fields => False),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""s"": {""c"": 5}}", Schema, Check_For_Missing_Fields => False),
         "Valid object should not cause error.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""s"": {}}", Schema, Check_For_Missing_Fields => True),
         "Sequence missing fields.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""s"": {""c"": 5}}", Schema, Check_For_Missing_Fields => True),
         "Sequence missing fields.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{}", Schema, Check_For_Missing_Fields => True), "Sequence missing.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{}", Schema, Check_For_Missing_Fields => False),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""s"": {""c"": 5, ""d"": 5}}", Schema, Check_For_Missing_Fields => False),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""s"": {""d"": 5}}", Schema, Check_For_Missing_Fields => False),
         "Valid object should not cause error.");
   end Test_Validate_Module_Config_To_Schema_Sequence;

   procedure Test_Validate_Module_Config_To_Schema_Variant (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map :=
        ["v" =>
           Config_Property_Parameters_Variant'
             (Experimental => False,
              Present_When => P, Description => "",
              Default     => "c",
              Children    =>
                ["c" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0),
                 "d" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0)])];

      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""v"": 1}", Schema, Check_For_Missing_Fields => False),
         "Variant wrong type.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""v"": {""Selected"": 1}}", Schema, Check_For_Missing_Fields => False),
         "Variant selected wrong type.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""v"": {""Selected"": ""e""}}", Schema, Check_For_Missing_Fields => False),
         "Variant selected invalid option.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""v"": {""Children"": 1}}", Schema, Check_For_Missing_Fields => False),
         "Variant children wrong type.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""v"": {""Children"": {""c"": 11}}}", Schema, Check_For_Missing_Fields => False),
         "Variant child out of range.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""v"": {""Children"": {""e"": 1}}}", Schema, Check_For_Missing_Fields => False),
         "Variant extra child.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema ("{""v"": {}}", Schema, Check_For_Missing_Fields => True),
         "Variant empty.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""v"": {""Selected"": ""c""}}", Schema, Check_For_Missing_Fields => True),
         "Variant with just selected.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""v"": {""Selected"":""c"", ""Children"": {}}}", Schema, Check_For_Missing_Fields => True),
         "Variant with empty children.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""v"": {""Children"": {""c"": 5, ""d"": 1}}}", Schema, Check_For_Missing_Fields => True),
         "Variant with missing selected.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""v"": {""Selected"":""c"", ""Children"": {""c"": 5}}}", Schema, Check_For_Missing_Fields => True),
         "Variant with selected and missing child.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema ("{""v"": {}}", Schema, Check_For_Missing_Fields => False),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""v"": {""Selected"": ""c""}}", Schema, Check_For_Missing_Fields => False),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""v"": {""Children"": {}}}", Schema, Check_For_Missing_Fields => False),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""v"": {""Children"": {""c"": 5}}}", Schema, Check_For_Missing_Fields => False),
         "Valid object should not cause error.");
      T.Assert
        (not Reports_Error_Module_Config_To_Schema
               ("{""v"": {""Selected"":""c"", ""Children"": {""c"": 5}}}", Schema, Check_For_Missing_Fields => False),
         "Valid object should not cause error.");
      T.Assert
        (Reports_Error_Module_Config_To_Schema
           ("{""v"": {""Selected"": ""c"", ""Extra"": 1}}", Schema, Check_For_Missing_Fields => False),
         "Variant with extra field.");
   end Test_Validate_Module_Config_To_Schema_Variant;

   procedure Test_Validate_Outer_Config_Structure (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      T.Assert (Reports_Error_Outer_Config ("{}"), "Empty object.");
      T.Assert (Reports_Error_Outer_Config ("[]"), "Type mismatch.");
      T.Assert (Reports_Error_Outer_Config ("{""Config"": {}}"), "Missing field.");
      T.Assert (Reports_Error_Outer_Config ("{""Prunt config version"": 1}"), "Missing field.");
      T.Assert (Reports_Error_Outer_Config ("{""Config"": 123, ""Prunt config version"": 1}"), "Type mismatch.");
      T.Assert (Reports_Error_Outer_Config ("{""Config"": {}, ""Prunt config version"": 1.0}"), "Type mismatch.");
      T.Assert
        (Reports_Error_Outer_Config ("{""Config"": {}, ""Prunt config version"": 1, ""a"": 1}"), "Extra field.");
      T.Assert (Reports_Error_Outer_Config ("{""Config"": {}, ""Prunt config version"": 2}"), "Wrong version.");
      T.Assert
        (not Reports_Error_Outer_Config ("{""Config"": {}, ""Prunt config version"": 1}"),
         "Valid object should not cause error.");
   end Test_Validate_Outer_Config_Structure;

   procedure Test_Validate_Unhandled_Property (T : in out Trendy_Test.Operation'Class) is
      procedure Report (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
         pragma Unreferenced (Path);
         pragma Unreferenced (Message);
      begin
         null;
      end Report;
   begin
      T.Register;

      Schema : constant Config_Property_Maps.Map := ["u" => Config_Property_Parameters_Unknown'(Experimental => False,
                      Present_When => P, Description => "")];

      begin
         Validate_Module_Config_To_Schema
           (Read ("{""u"": 1}"), Schema, Report'Access, Check_For_Missing_Fields => True);
         T.Fail ("Should have raised Constraint_Error");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Validate_Unhandled_Property;

   overriding
   procedure Migrate (This : Custom_Schema; Old_Version : Config_Schema_Version; Data : in out Config_Data) is
      Path : Config_Data_Paths.Vector := [1 => "migrated_field"];
   begin
      if Old_Version = 1 then
         Data.Set (Path, Long_Long_Integer'(999));
      end if;
   end Migrate;

   overriding
   procedure Migrate (This : Error_Schema; Old_Version : Config_Schema_Version; Data : in out Config_Data) is
      pragma Unreferenced (This, Old_Version);
   begin
      Prunt.Config.Set (Data, Config_Data_Paths.Empty_Vector, True);
   end Migrate;

   overriding
   procedure Migrate (This : Accessors_Schema; Old_Version : Config_Schema_Version; Data : in out Config_Data) is
      pragma Unreferenced (This, Old_Version);
      use Config_Data_Paths;
   begin
      Data.Set (Vector'([1 => "b"]), not Boolean'(Data.Get (Vector'([1 => "b"]))));
      Data.Set (Vector'([1 => "i"]), Long_Long_Integer'(Data.Get (Vector'([1 => "i"]))) + 1);
      Data.Set (Vector'([1 => "f"]), Long_Float'(Data.Get (Vector'([1 => "f"]))) + 1.0);
      Data.Set (Vector'([1 => "d"]), Virtual_String'("b"));
      if Data.Get (Vector'([1 => "d"])) /= "b" then
         Data.Set (Vector'([1 => "d"]), Virtual_String'("error"));
      end if;

      Data.Set (Vector'([1 => "dim"]), Dimensionless (Long_Float'(Data.Get (Vector'([1 => "dim"])))) + 1.0);

      D : constant Dimensionless := Data.Get (Vector'([1 => "dim"]));
      Data.Set (Vector'([1 => "dim"]), D);

      R : constant Dimensionless_Ratio := Data.Get (Vector'([1 => "r"]));
      Data.Set (Vector'([1 => "r"]), (Numerator => R.Numerator + 1.0, Denominator => R.Denominator));

      Data.Save;
   end Migrate;

   Migration_Schemas : constant Config_Schema_Maps.Map :=
     ["M" =>
        Custom_Schema'
          (Version         => 2,
           Module_Instance_Tag => <>,
           Top_Level_Items =>
             ["i"              =>
                Config_Property_Parameters_Integer'(Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 5),
              "migrated_field" =>
                Config_Property_Parameters_Integer'
                  (Experimental => False,
                   Present_When => P, Description => "", Min => 0, Max => 1000, Unit => "", Default => 0)])];

   Migration_Error_Schemas : constant Config_Schema_Maps.Map :=
     ["M" => Error_Schema'(Version => 2, Module_Instance_Tag => <>, Top_Level_Items => [])];

   Migration_Accessors_Schemas : constant Config_Schema_Maps.Map :=
     ["M" =>
        Accessors_Schema'
          (Version         => 2,
           Module_Instance_Tag => <>,
           Top_Level_Items =>
             ["b"   => Config_Property_Parameters_Boolean'(Experimental => False,
                      Present_When => P, Description => "", Default => True),
              "i"   =>
                Config_Property_Parameters_Integer'(Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 5),
              "f"   =>
                Config_Property_Parameters_Float'
                  (Experimental => False,
                   Present_When => P, Description => "", Min => 0.0, Max => 100.0, Unit => "", Default => 5.0),
              "d"   => Config_Property_Parameters_Discrete'(Experimental => False,
                      Present_When => P, Description => "", Default => "a", Options => ["a", "b"]),
              "dim" =>
                Config_Property_Parameters_Float'
                  (Experimental => False,
                   Present_When => P, Description => "", Min => 0.0, Max => 100.0, Unit => "", Default => 5.0),
              "r"   =>
                Config_Property_Parameters_Float_Ratio'
                  (Experimental => False,
                   Present_When => P, Description => "", Min => 0.0, Max => 10.0, Default => (Numerator => 1.0, Denominator => 1.0))])];

   procedure Test_Initialize_Real_Migration (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filename : constant String := Next_Test_Filename;
      Content : constant String :=
        "{"
        & """Prunt config version"": 1,"
        & """Config"": {"
        & "   ""M"": {"
        & "      ""Version"": 1,"
        & "      ""Config"": {""i"": 2}"
        & "   }"
        & "}"
        & "}";

      declare
         F : Prunt.Mockable.Text_IO.File_Type;
      begin
         Prunt.Mockable.Text_IO.Create (F, Prunt.Mockable.Text_IO.Out_File, Filename);
         Prunt.Mockable.Text_IO.Put_Line (F, Content);
         Prunt.Mockable.Text_IO.Close (F);
      end;

      File : Config_File := Create (Filename, Migration_Schemas);
      pragma Unreferenced (File);

      declare
         F       : Prunt.Mockable.Text_IO.File_Type;
         Content : Unbounded_String;
      begin
         Prunt.Mockable.Text_IO.Open (F, Prunt.Mockable.Text_IO.In_File, Filename);
         while not Prunt.Mockable.Text_IO.End_Of_File (F) loop
            Append (Content, Prunt.Mockable.Text_IO.Get_Line (F));
         end loop;
         Prunt.Mockable.Text_IO.Close (F);

         Val : constant JSON_Value := Read (VSS.Strings.Conversions.To_Virtual_String (To_String (Content)));
         T.Assert (Val.Get ("Config").Get ("M").Get ("Version").Get = 2, "Version updated");
         T.Assert
           (Val.Get ("Config").Get ("M").Get ("Config").Get ("migrated_field").Get = 999,
            "Migration procedure executed");
      end;
   end Test_Initialize_Real_Migration;

   procedure Test_Initialize_With_Empty_Path_In_Migration (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filename : constant String := Next_Test_Filename;
      Content : constant String :=
        "{"
        & """Prunt config version"": 1,"
        & """Config"": {"
        & "   ""M"": {"
        & "      ""Version"": 1,"
        & "      ""Config"": {}"
        & "   }"
        & "}"
        & "}";

      declare
         F : Prunt.Mockable.Text_IO.File_Type;
      begin
         Prunt.Mockable.Text_IO.Create (F, Prunt.Mockable.Text_IO.Out_File, Filename);
         Prunt.Mockable.Text_IO.Put_Line (F, Content);
         Prunt.Mockable.Text_IO.Close (F);
      end;

      File : Config_File := Create (Filename, Migration_Error_Schemas);
      pragma Unreferenced (File);

      T.Fail ("Should have raised Constraint_Error during migration");
   exception
      when Constraint_Error =>
         null;
   end Test_Initialize_With_Empty_Path_In_Migration;

   procedure Test_Migration_Accessors (T : in out Trendy_Test.Operation'Class) is
   begin

      T.Register;

      Filename : constant String := Next_Test_Filename;
      Content : constant String :=
        "{"
        & """Prunt config version"": 1,"
        & """Config"": {"
        & "   ""M"": {"
        & "      ""Version"": 1,"
        & "      ""Config"": {""b"": true, ""i"": 2, ""f"": 10.0, ""d"": ""a"", ""dim"": 5.0,"
        & " ""r"": {""Numerator"": 1.0, ""Denominator"": 1.0}}"
        & "   }"
        & "}"
        & "}";

      declare
         F : Prunt.Mockable.Text_IO.File_Type;
      begin
         Prunt.Mockable.Text_IO.Create (F, Prunt.Mockable.Text_IO.Out_File, Filename);
         Prunt.Mockable.Text_IO.Put_Line (F, Content);
         Prunt.Mockable.Text_IO.Close (F);
      end;

      File : constant Config_File := Create (Filename, Migration_Accessors_Schemas);
      Data : Config_Data := File.Get_Data ("M");

      T.Assert (Data.Get (Config_Data_Paths.Vector'([1 => "b"])) = False, "Boolean migrated");
      T.Assert (Data.Get (Config_Data_Paths.Vector'([1 => "i"])) = Long_Long_Integer'(3), "Integer migrated");
      T.Assert (Data.Get (Config_Data_Paths.Vector'([1 => "f"])) = Long_Float'(11.0), "Float migrated");
      T.Assert (Data.Get (Config_Data_Paths.Vector'([1 => "d"])) = "b", "Discrete migrated");
      T.Assert (Data.Get (Config_Data_Paths.Vector'([1 => "dim"])) = Dimensionless'(6.0), "Dimensionless migrated");

      R : constant Dimensionless_Ratio := Data.Get (Config_Data_Paths.Vector'([1 => "r"]));
      T.Assert (R.Numerator = 2.0 and R.Denominator = 1.0, "Float_Ratio migrated");
   end Test_Migration_Accessors;

   procedure Test_Apply_Untrusted_Patch_Really_Empty (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      File : constant Config_File := Create (Next_Test_Filename, Schemas);
      Output : Virtual_String;

      Errors : Config_Error_Vectors.Vector;

      File.Apply_Untrusted_Patch ("{}", Output, Errors);
      T.Assert (Errors.Is_Empty, "Should not report error for empty patch object");
      T.Assert (Output = File.Get_Data_String, "Output should match existing data");
   end Test_Apply_Untrusted_Patch_Really_Empty;

   procedure Test_Reset_Live_To_Stored_Ref_Count (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      Filename : constant String := Next_Test_Filename;

      File : constant Config_File := Create (Filename, Schemas);
      Data : Config_Data := File.Get_Data ("M");
      pragma Unreferenced (Data);

      begin
         File.Reset_Live_To_Stored;
         T.Fail ("Should have raised Constraint_Error due to ref count");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Reset_Live_To_Stored_Ref_Count;

   procedure Test_Reset_Live_To_Stored_Success (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Schemas : constant Config_Schema_Maps.Map :=
        ["M" => Versioned_Config_Schema'(Version => 1, Module_Instance_Tag => <>, Top_Level_Items => [])];
      Filename : constant String := Next_Test_Filename;

      File : constant Config_File := Create (Filename, Schemas);
      File.Reset_Live_To_Stored;
   end Test_Reset_Live_To_Stored_Success;

   procedure Test_Set_Migration_Config_Not_Object (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Data : Config_Data;

      Data.For_Migration.Active := True;
      Data.Migration_Config := Create (Integer'(123));

      begin
         Data.Set (Config_Data_Paths.Vector'(["Any"]), True);
         T.Fail ("Should have raised Constraint_Error");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Set_Migration_Config_Not_Object;

   procedure Test_Config_Overrides_Hidden_From_Stored_And_Schema (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filename : constant String := Next_Test_Filename;
      Content : constant String :=
        "{"
        & """Prunt config version"": 1,"
        & """Config"": {"
        & "   ""M"": {"
        & "      ""Version"": 1,"
        & "      ""Config"": {""s"": {""i"": 3, ""j"": 4}}"
        & "   }"
        & "}"
        & "}";

      declare
         F : Mockable.Text_IO.File_Type;
      begin
         Mockable.Text_IO.Create (F, Mockable.Text_IO.Out_File, Filename);
         Mockable.Text_IO.Put_Line (F, Content);
         Mockable.Text_IO.Close (F);
      end;

      File : constant Config_File := Create (Filename, Override_Test_Schemas, Override_I);
      Data : Config_Data := File.Get_Data ("M");

      Stored_JSON : constant JSON_Value := Read (File.Get_Data_String);
      Schema_JSON : constant JSON_Value := Read (File.Get_Schema_String);
      Disk_JSON : constant JSON_Value := Read_Test_File (Filename);

      T.Assert (Data.Get (Config_Data_Paths.Vector'(["s", "i"])) = Long_Long_Integer'(7), "Override not live");
      T.Assert (Data.Get (Config_Data_Paths.Vector'(["s", "j"])) = Long_Long_Integer'(4), "Stored value not kept");
      T.Assert
        (not Stored_JSON.Get ("Config").Get ("M").Get ("Config").Get ("s").Has_Field ("i"),
         "Stored config exposes override");
      T.Assert
        (not Schema_JSON.Get ("Config").Get ("M").Get ("Config").Get ("s").Get ("Children").Has_Field ("i"),
         "Schema exposes override");
      T.Assert
        (not Disk_JSON.Get ("Config").Get ("M").Get ("Config").Get ("s").Has_Field ("i"),
         "Config file exposes override");
   end Test_Config_Overrides_Hidden_From_Stored_And_Schema;

   procedure Test_Config_Overrides_Reject_Writes (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filename : constant String := Next_Test_Filename;
      File : constant Config_File := Create (Filename, Override_Test_Schemas, Override_I);
      Data : Config_Data := File.Get_Data ("M");
      Output : Virtual_String;
      Errors : Config_Error_Vectors.Vector;

      begin
         Data.Set (Config_Data_Paths.Vector'(["s", "i"]), Long_Long_Integer'(8));
         T.Fail ("Overridden value should reject Config_Data.Set.");
      exception
         when Constraint_Error =>
            null;
      end;

      Data.Set (Config_Data_Paths.Vector'(["s", "j"]), Long_Long_Integer'(5));
      Data.Save;
      T.Assert (Data.Get (Config_Data_Paths.Vector'(["s", "i"])) = Long_Long_Integer'(7), "Override changed");
      T.Assert (Data.Get (Config_Data_Paths.Vector'(["s", "j"])) = Long_Long_Integer'(5), "Normal value not saved");

      File.Apply_Untrusted_Patch
        ("{""Prunt config version"": 1, ""Config"": {""M"": {""Version"": 1, ""Config"": {""s"": {""i"": 9}}}}}",
         Output,
         Errors);
      T.Assert (not Errors.Is_Empty, "Overridden value should reject web patch.");
      T.Assert (Data.Get (Config_Data_Paths.Vector'(["s", "i"])) = Long_Long_Integer'(7), "Patch changed override");
   end Test_Config_Overrides_Reject_Writes;

   procedure Test_Config_Overrides_Reset_Reapplies_Overrides (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filename : constant String := Next_Test_Filename;
      File : constant Config_File := Create (Filename, Override_Test_Schemas, Override_I);

      declare
         Data : Config_Data := File.Get_Data ("M");
      begin
         Data.Set (Config_Data_Paths.Vector'(["s", "j"]), Long_Long_Integer'(6));
         T.Assert (Data.Get (Config_Data_Paths.Vector'(["s", "j"])) = Long_Long_Integer'(6));
      end;

      File.Reset_Live_To_Stored;

      declare
         Data : Config_Data := File.Get_Data ("M");
      begin
         T.Assert (Data.Get (Config_Data_Paths.Vector'(["s", "i"])) = Long_Long_Integer'(7), "Override not reapplied");
         T.Assert
           (Data.Get (Config_Data_Paths.Vector'(["s", "j"])) = Long_Long_Integer'(2),
            "Reset did not restore stored value");
      end;
   end Test_Config_Overrides_Reset_Reapplies_Overrides;

   procedure Test_Config_Overrides_Variant_Selected_Rehydrates_Defaults (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filename : constant String := Next_Test_Filename;
      Content : constant String :=
        "{"
        & """Prunt config version"": 1,"
        & """Config"": {"
        & "   ""M"": {"
        & "      ""Version"": 1,"
        & "      ""Config"": {}"
        & "   }"
        & "}"
        & "}";
      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["v" =>
                   Config_Property_Parameters_Variant'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Default     => "a",
                      Children    =>
                        ["a" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 1),
                         "b" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 2)])])];
      Overrides : constant Config_Override_Vectors.Vector :=
        [Config_Override'
           (Owner => "M",
            Path  => Config_Data_Paths.Vector'(["v", "Selected"]),
            Value => Create (Conversions.To_Virtual_String ("b"))),
         Config_Override'
           (Owner => "M",
            Path  => Config_Data_Paths.Vector'(["v", "Children", "b"]),
            Value => Create (Long_Long_Integer'(6)))];

      declare
         F : Mockable.Text_IO.File_Type;
      begin
         Mockable.Text_IO.Create (F, Mockable.Text_IO.Out_File, Filename);
         Mockable.Text_IO.Put_Line (F, Content);
         Mockable.Text_IO.Close (F);
      end;

      declare
         File : constant Config_File := Create (Filename, Schemas, Overrides);
         Data : Config_Data := File.Get_Data ("M");

         Stored_JSON : constant JSON_Value := Read (File.Get_Data_String);
         Schema_JSON : constant JSON_Value := Read (File.Get_Schema_String);
      begin
         T.Assert (Data.Get (Config_Data_Paths.Vector'(["v", "Selected"])) = "b", "Variant selection not live");
         T.Assert
           (Data.Get (Config_Data_Paths.Vector'(["v", "Children", "a"])) = Long_Long_Integer'(1),
            "Variant defaults not rehydrated");
         T.Assert
           (Data.Get (Config_Data_Paths.Vector'(["v", "Children", "b"])) = Long_Long_Integer'(6),
            "Variant child override not live");
         T.Assert
           (not Stored_JSON.Get ("Config").Get ("M").Get ("Config").Has_Field ("v"),
            "Stored config exposes override");
         T.Assert
           (not Schema_JSON.Get ("Config").Get ("M").Get ("Config").Has_Field ("v"),
            "Schema exposes variant selected override");
      end;
   end Test_Config_Overrides_Variant_Selected_Rehydrates_Defaults;

   procedure Test_Config_Overrides_Invalid (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      begin
         File : Config_File :=
           Create
             (Next_Test_Filename,
              Override_Test_Schemas,
              [Config_Override'
                 (Owner => "Unknown",
                  Path  => Config_Data_Paths.Vector'(["s", "i"]),
                  Value => Create (Long_Long_Integer'(7)))])
         with Unreferenced;
         T.Fail ("Unknown override module should fail.");
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         File : Config_File :=
           Create
             (Next_Test_Filename,
              Override_Test_Schemas,
              [Config_Override'
                 (Owner => "M",
                  Path  => Config_Data_Paths.Vector'(["s", "missing"]),
                  Value => Create (Long_Long_Integer'(7)))])
         with Unreferenced;
         T.Fail ("Unknown override path should fail.");
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         File : Config_File :=
           Create
             (Next_Test_Filename,
              Override_Test_Schemas,
              [Config_Override'
                 (Owner => "M",
                  Path  => Config_Data_Paths.Vector'(["s", "i"]),
                  Value => Create (Long_Long_Integer'(99)))])
         with Unreferenced;
         T.Fail ("Out-of-range override should fail.");
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         File : Config_File :=
           Create
             (Next_Test_Filename,
              Override_Test_Schemas,
              [Config_Override'(Owner => "M", Path => Config_Data_Paths.Vector'(["s"]), Value => Create_Object),
               Config_Override'
                 (Owner => "M",
                  Path  => Config_Data_Paths.Vector'(["s", "i"]),
                  Value => Create (Long_Long_Integer'(7)))])
         with Unreferenced;
         T.Fail ("Overlapping overrides should fail.");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Config_Overrides_Invalid;

   procedure Test_Config_Overrides_Prune_During_Migration (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Base_Schemas : constant Config_Schema_Maps.Map := Override_Test_Schemas;
      Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 2,
              Module_Instance_Tag => <>,
              Top_Level_Items     => Base_Schemas.Element ("M").Top_Level_Items)];
      Filename : constant String := Next_Test_Filename;
      Content : constant String :=
        "{"
        & """Prunt config version"": 1,"
        & """Config"": {"
        & "   ""M"": {"
        & "      ""Version"": 1,"
        & "      ""Config"": {""s"": {""i"": 3, ""j"": 4}}"
        & "   }"
        & "}"
        & "}";

      declare
         F : Mockable.Text_IO.File_Type;
      begin
         Mockable.Text_IO.Create (F, Mockable.Text_IO.Out_File, Filename);
         Mockable.Text_IO.Put_Line (F, Content);
         Mockable.Text_IO.Close (F);
      end;

      File : constant Config_File := Create (Filename, Schemas, Override_I);
      Data : Config_Data := File.Get_Data ("M");
      Disk_JSON : constant JSON_Value := Read_Test_File (Filename);

      T.Assert (Data.Get (Config_Data_Paths.Vector'(["s", "i"])) = Long_Long_Integer'(7), "Override not live");
      T.Assert (Data.Get (Config_Data_Paths.Vector'(["s", "j"])) = Long_Long_Integer'(4), "Stored value lost");
      T.Assert (Disk_JSON.Get ("Config").Get ("M").Get ("Version").Get = Long_Long_Integer'(2), "Version not migrated");
      T.Assert
        (not Disk_JSON.Get ("Config").Get ("M").Get ("Config").Get ("s").Has_Field ("i"),
         "Migrated file exposes override");
   end Test_Config_Overrides_Prune_During_Migration;

   procedure Test_Config_Override_Helper_Edges (T : in out Trendy_Test.Operation'Class) is
      Helper_Schemas : constant Config_Schema_Maps.Map :=
        ["M" =>
           Versioned_Config_Schema'
             (Version             => 1,
              Module_Instance_Tag => <>,
              Top_Level_Items     =>
                ["i" =>
                   Config_Property_Parameters_Integer'
                     (Experimental => False,
                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 0),
                 "s" =>
                   Config_Property_Parameters_Sequence'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Tabbed      => False,
                      Children    =>
                        ["v" =>
                           Config_Property_Parameters_Variant'
                             (Experimental => False,
                              Present_When => P, Description => "",
                              Default     => "a",
                              Children    =>
                                ["a" =>
                                   Config_Property_Parameters_Integer'
                                     (Experimental => False,
                                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 1)])]),
                 "v" =>
                   Config_Property_Parameters_Variant'
                     (Experimental => False,
                      Present_When => P, Description => "",
                      Default     => "a",
                      Children    =>
                        ["a" =>
                           Config_Property_Parameters_Integer'
                             (Experimental => False,
                              Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 1),
                         "child" =>
                           Config_Property_Parameters_Variant'
                             (Experimental => False,
                              Present_When => P, Description => "",
                              Default     => "leaf",
                              Children    =>
                                ["leaf" =>
                                   Config_Property_Parameters_Integer'
                                     (Experimental => False,
                                      Present_When => P, Description => "", Min => 0, Max => 10, Unit => "", Default => 2)])])])];
      Helper_Schema : constant Config_Property_Maps.Map := Helper_Schemas.Element ("M").Top_Level_Items;
      Default_Value : JSON_Value;
      Reported      : Boolean := False;
      Reported_Path : Config_Data_Paths.Vector;
      use type Config_Data_Paths.Vector;

      procedure Report (Path : Config_Data_Paths.Vector; Message : Virtual_String) is
         pragma Unreferenced (Message);
      begin
         Reported := True;
         Reported_Path := Path;
      end Report;
   begin
      T.Register;

      T.Assert (Is_Path_Prefix (Config_Data_Paths.Empty_Vector, Config_Data_Paths.Vector'([1 => "a"])));
      T.Assert (Paths_Overlap (Config_Data_Paths.Vector'(["a", "b"]), Config_Data_Paths.Vector'([1 => "a"])));
      T.Assert (not Paths_Overlap (Config_Data_Paths.Vector'([1 => "a"]), Config_Data_Paths.Vector'([1 => "b"])));
      T.Assert (Path_Equals_Override ("M", Config_Data_Paths.Vector'(["s", "i"]), Override_I));
      T.Assert (not Path_Equals_Override ("N", Config_Data_Paths.Vector'(["s", "i"]), Override_I));

      declare
         Root : JSON_Value := Read ("{""s"": {""i"": 1}, ""n"": 1}");
      begin
         T.Assert (not Unset_JSON_Node (Root, Config_Data_Paths.Empty_Vector));
         T.Assert (not Unset_JSON_Node (Root.Get ("n"), Config_Data_Paths.Vector'([1 => "x"])));
         T.Assert (not Unset_JSON_Node (Root, Config_Data_Paths.Vector'(["n", "x"])));
         T.Assert (Unset_JSON_Node (Root, Config_Data_Paths.Vector'(["s", "i"])));
         T.Assert (not Root.Get ("s").Has_Field ("i"));
      end;

      T.Assert (Path_Without_Last (Config_Data_Paths.Vector'([1 => "x"])).Is_Empty);
      T.Assert
        (Path_Without_Last (Config_Data_Paths.Vector'(["x", "y"])) = Config_Data_Paths.Vector'([1 => "x"]));

      T.Assert (not Selected_Variant_Default (Helper_Schema, Config_Data_Paths.Vector'([1 => "Selected"]), Default_Value));
      T.Assert (not Selected_Variant_Default (Helper_Schema, Config_Data_Paths.Vector'(["v", "Wrong"]), Default_Value));
      T.Assert
        (not Selected_Variant_Default (Helper_Schema, Config_Data_Paths.Vector'(["missing", "Selected"]), Default_Value));
      T.Assert (not Selected_Variant_Default (Helper_Schema, Config_Data_Paths.Vector'(["s", "Selected"]), Default_Value));
      T.Assert
        (not Selected_Variant_Default (Helper_Schema, Config_Data_Paths.Vector'(["i", "child", "Selected"]), Default_Value));
      T.Assert
        (not Selected_Variant_Default
               (Helper_Schema, Config_Data_Paths.Vector'(["v", "Wrong", "child", "Selected"]), Default_Value));
      T.Assert
        (not Selected_Variant_Default (Helper_Schema, Config_Data_Paths.Vector'(["v", "Children", "Selected"]), Default_Value));
      T.Assert
        (not Selected_Variant_Default
               (Helper_Schema, Config_Data_Paths.Vector'(["v", "Children", "missing", "Selected"]), Default_Value));
      T.Assert (Selected_Variant_Default (Helper_Schema, Config_Data_Paths.Vector'(["v", "Selected"]), Default_Value));
      T.Assert (Default_Value.Get ("Selected").Get = "a");
      T.Assert (Selected_Variant_Default (Helper_Schema, Config_Data_Paths.Vector'(["s", "v", "Selected"]), Default_Value));
      T.Assert (Default_Value.Get ("Selected").Get = "a");
      T.Assert
        (Selected_Variant_Default
           (Helper_Schema, Config_Data_Paths.Vector'(["v", "Children", "child", "Selected"]), Default_Value));
      T.Assert (Default_Value.Get ("Selected").Get = "leaf");

      declare
         Root   : JSON_Value := Read ("{""v"": {""Selected"": ""b"", ""Children"": {""b"": 6}}, ""leaf"": 1}");
         Result : JSON_Value;
      begin
         T.Assert (not Try_Get_JSON_Node (Root, Config_Data_Paths.Empty_Vector, Result));
         T.Assert (not Try_Get_JSON_Node (Root.Get ("leaf"), Config_Data_Paths.Vector'([1 => "x"]), Result));
         T.Assert (Try_Get_JSON_Node (Root, Config_Data_Paths.Vector'([1 => "v"]), Result));
         T.Assert (Result.Get ("Selected").Get = "b");

         Merge_Default_JSON_Node
           (Root,
            Config_Data_Paths.Vector'([1 => "v"]),
            Read ("{""Selected"": ""a"", ""Children"": {""a"": 1, ""b"": 2}}"));
         T.Assert (Root.Get ("v").Get ("Selected").Get = "b");
         T.Assert (Root.Get ("v").Get ("Children").Get ("a").Get = Long_Long_Integer'(1));
         T.Assert (Root.Get ("v").Get ("Children").Get ("b").Get = Long_Long_Integer'(6));
      end;

      declare
         Root : JSON_Value := Read ("{""v"": 1}");
      begin
         Merge_Default_JSON_Node (Root, Config_Data_Paths.Vector'([1 => "v"]), Create (Long_Long_Integer'(2)));
         T.Assert (Root.Get ("v").Get = Long_Long_Integer'(1));

         Merge_Default_JSON_Node (Root, Config_Data_Paths.Vector'([1 => "v"]), Read ("{""x"": 2}"));
         T.Assert (Root.Get ("v").Get = Long_Long_Integer'(1));
      end;

      declare
         Config : JSON_Value := Read ("{""Config"": {""U"": {""Version"": 1, ""Config"": {}}}}");
      begin
         Apply_Overrides_To_Config
           (Config,
            Helper_Schemas,
            [Config_Override'
               (Owner => "U", Path => Config_Data_Paths.Vector'([1 => "x"]), Value => Create (Long_Long_Integer'(1)))]);
         T.Assert (Config.Get ("Config").Get ("U").Get ("Config").Get ("x").Get = Long_Long_Integer'(1));
      end;

      declare
         Config : JSON_Value := Read ("{""Config"": {}}");
      begin
         T.Assert (not Prune_Overrides_From_Config (Config, Helper_Schemas, Override_I));
      end;

      declare
         Config : JSON_Value := Read ("{""Config"": {""M"": {""Version"": 1, ""Config"": {""s"": {}}}}}");
         Empty_Schemas : constant Config_Schema_Maps.Map := [];
      begin
         T.Assert (not Prune_Overrides_From_Config (Config, Empty_Schemas, Override_I));
      end;

      declare
         Config : JSON_Value := Read ("{""Config"": {""M"": {""Version"": 1, ""Config"": {""s"": {}}}}}");
      begin
         T.Assert (not Prune_Overrides_From_Config (Config, Override_Test_Schemas, Override_I));
      end;

      declare
         Result : Config_Schema_Maps.Map :=
           Prune_Overrides_From_Schemas
             (Helper_Schemas,
              [Config_Override'
                 (Owner => "Unknown",
                  Path  => Config_Data_Paths.Vector'([1 => "x"]),
                  Value => Create (Long_Long_Integer'(1)))]);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      declare
         Result : Config_Schema_Maps.Map :=
           Prune_Overrides_From_Schemas
             (Helper_Schemas,
              [Config_Override'
                 (Owner => "M", Path => Config_Data_Paths.Vector'(["i", "x"]), Value => Create_Object)]);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      declare
         Result : Config_Schema_Maps.Map :=
           Prune_Overrides_From_Schemas
             (Helper_Schemas,
              [Config_Override'(Owner => "M", Path => Config_Data_Paths.Empty_Vector, Value => Create_Object)]);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      declare
         Result : Config_Schema_Maps.Map :=
           Prune_Overrides_From_Schemas
             (Helper_Schemas,
              [Config_Override'
                 (Owner => "M", Path => Config_Data_Paths.Vector'(["v", "Children"]), Value => Create_Object)]);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      declare
         Result : Config_Schema_Maps.Map :=
           Prune_Overrides_From_Schemas
             (Helper_Schemas,
              [Config_Override'(Owner => "M", Path => Config_Data_Paths.Vector'(["v", "Bad"]), Value => Create_Object)]);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      declare
         Result : Config_Schema_Maps.Map :=
           Prune_Overrides_From_Schemas
             (Helper_Schemas,
              [Config_Override'
                 (Owner => "M",
                  Path  => Config_Data_Paths.Vector'(["v", "Children", "missing"]),
                  Value => Create_Object)]);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      declare
         Result : Config_Schema_Maps.Map :=
           Prune_Overrides_From_Schemas
             (Helper_Schemas,
              [Config_Override'
                 (Owner => "M",
                  Path  => Config_Data_Paths.Vector'(["v", "Children", "child"]),
                  Value => Create_Object)]);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      declare
         Result : Config_Schema_Maps.Map :=
           Prune_Overrides_From_Schemas
             (Helper_Schemas,
              [Config_Override'
                 (Owner => "M",
                  Path  => Config_Data_Paths.Vector'(["v", "Children", "child", "Children"]),
                  Value => Create_Object)]);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      declare
         Result : Config_Schema_Maps.Map :=
           Prune_Overrides_From_Schemas
             (Helper_Schemas,
              [Config_Override'
                 (Owner => "M",
                  Path  => Config_Data_Paths.Vector'(["v", "Children", "child", "Selected"]),
                  Value => Create_Object)]);
         pragma Unreferenced (Result);
      begin
         null;
      end;

      begin
         Validate_Overrides
           (Helper_Schemas,
            [Config_Override'(Owner => "M", Path => Config_Data_Paths.Empty_Vector, Value => Create_Object)]);
         T.Fail ("Empty override path should fail validation.");
      exception
         when Constraint_Error =>
            null;
      end;

      Validate_No_Overrides_In_Patch
        ("M",
         Read ("{""s"": {""i"": 1}}"),
         [Config_Override'(Owner => "M", Path => Config_Data_Paths.Vector'([1 => "s"]), Value => Create_Object)],
         Report'Access);
      T.Assert (Reported);
      T.Assert (Reported_Path = Config_Data_Paths.Vector'([1 => "s"]));
   end Test_Config_Override_Helper_Edges;

   procedure Test_Config_Path_Selected_Variant (T : in out Trendy_Test.Operation'Class) is
      use type Config_Data_Paths.Vector;

      Data : constant Config_Data :=
        Migration_Data
          ("{""Homing"":{""Axes"":{""X"":{""Homing_Method"":{""Kind"":{"
           & """Selected"":""Home_To_Detector"",""Children"":{""Home_To_Detector"":{"
           & """Home_To_Detector"":{""Detector"":{""Kind"":{""Selected"":""Input_Switch"","
           & """Children"":{""Input_Switch"":{""Switch"":""Probe""}}}}}}}}}}}}}");
      Resolved : constant Config_Data_Paths.Vector :=
        Resolve_Config_Path (Data, Homing_Switch_Path);
   begin
      T.Register;
      T.Assert
        (Resolved
         = Config_Data_Paths.Vector'
             (["Homing",
               "Axes",
               "X",
               "Homing_Method",
               "Kind",
               "Children",
               "Home_To_Detector",
               "Home_To_Detector",
               "Detector",
               "Kind",
               "Children",
               "Input_Switch",
               "Switch"]),
         "Typed homing detector path did not resolve to the selected input switch.");
   end Test_Config_Path_Selected_Variant;

   procedure Test_Config_Path_Unselected_Variant_Does_Not_Report (T : in out Trendy_Test.Operation'Class) is
      Data : constant Config_Data :=
        Migration_Data
          ("{""Homing"":{""Axes"":{""X"":{""Homing_Method"":{""Kind"":{"
           & """Selected"":""Disabled"",""Children"":{""Home_To_Detector"":{"
           & """Home_To_Detector"":{""Detector"":{""Kind"":{""Selected"":""Input_Switch"","
           & """Children"":{""Input_Switch"":{""Switch"":""Probe""}}}}}}}}}}}}}");
      Reported : Boolean := False;

      procedure Report (Path : Config_Path) is
         Resolved : constant Config_Data_Paths.Vector := Resolve_Config_Path (Data, Path);
         pragma Unreferenced (Resolved);
      begin
         Reported := True;
      end Report;
   begin
      T.Register;

      begin
         Report (Homing_Switch_Path);
         T.Fail ("Unselected variant path should have raised Constraint_Error.");
      exception
         when Constraint_Error =>
            null;
      end;

      T.Assert (not Reported, "An unselected typed path reached the reporting callback.");
   end Test_Config_Path_Unselected_Variant_Does_Not_Report;

   procedure Test_Config_Path_Nested_Variant_Selections (T : in out Trendy_Test.Operation'Class) is
      Inner_Unselected : constant Config_Data :=
        Migration_Data
          ("{""Pause_Park"":{""Kind"":{""Selected"":""Absolute_Park_Move"",""Children"":{"
           & """Absolute_Park_Move"":{""Absolute_Park_Move"":{""Z_Target"":{""Kind"":{"
           & """Selected"":""Relative_Z_Offset"",""Children"":{""Absolute_Z_Position"":{"
           & """Z_Position"":1}}}}}}}}}}");
      Outer_Unselected : constant Config_Data :=
        Migration_Data
          ("{""Pause_Park"":{""Kind"":{""Selected"":""Relative_Park_Move"",""Children"":{"
           & """Absolute_Park_Move"":{""Absolute_Park_Move"":{""Z_Target"":{""Kind"":{"
           & """Selected"":""Absolute_Z_Position"",""Children"":{""Absolute_Z_Position"":{"
           & """Z_Position"":1}}}}}}}}}}");
      Target : constant Config_Path := Motion_Z_Position_Path;
   begin
      T.Register;
      Assert_Config_Path_Unreportable (T, Inner_Unselected, Target);
      Assert_Config_Path_Unreportable (T, Outer_Unselected, Target);
   end Test_Config_Path_Nested_Variant_Selections;

   procedure Test_Config_Path_Discriminator_Always_Reportable (T : in out Trendy_Test.Operation'Class) is
      use type Config_Data_Paths.Vector;

      Data : constant Config_Data :=
        Migration_Data
          ("{""Homing"":{""Axes"":{""X"":{""Homing_Method"":{""Kind"":{"
           & """Selected"":""Disabled"",""Children"":{}}}}}}}");
      Resolved : constant Config_Data_Paths.Vector :=
        Resolve_Config_Path (Data, Paths.Append (Homing_Method_Path, [1 => "Kind"]));
   begin
      T.Register;
      T.Assert
        (Resolved
         = Config_Data_Paths.Vector'
             (["Homing", "Axes", "X", "Homing_Method", "Kind"]),
         "A discriminator path should not require a particular selection.");
   end Test_Config_Path_Discriminator_Always_Reportable;

   procedure Test_Config_Path_Missing_Conditional_Targets (T : in out Trendy_Test.Operation'Class) is
      Missing_Present_When : constant Config_Data := Migration_Data ("{""Homing"":{}}");
      Missing_Fixed_Kind   : constant Config_Data := Migration_Data ("{""Motors"":{""M"":{}}}");
   begin
      T.Register;
      Assert_Config_Path_Unreportable
        (T, Missing_Present_When, Paths.Append (Homing_Method_Path, [1 => "Kind"]));
      Assert_Config_Path_Unreportable
        (T, Missing_Fixed_Kind, TMC2240_Run_Current_Path);
   end Test_Config_Path_Missing_Conditional_Targets;

   function Persistence_Schemas return Config_Schema_Maps.Map is
     (["M" => Versioned_Config_Schema'
         (Version => 1, Module_Instance_Tag => <>,
          Top_Level_Items =>
            ["i" => Config_Property_Parameters_Integer'
                (Experimental => False, Present_When => P, Description => "",
                 Min => 0, Max => 10, Unit => "", Default => 0)])]);

   procedure Test_Migration_Copy_Rejected (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      declare
         Data : Config_Data :=
           (For_Migration => (Ada.Finalization.Controlled with Active => True),
            Module => "M", Migration_Config => Create_Object, others => <>);
      begin
         begin
            declare
               Copied : Config_Data := Data;
               pragma Unreferenced (Copied);
            begin
               T.Fail ("Migration data copy must raise Program_Error.");
            end;
         exception
            when Program_Error => null;
         end;
         begin
            declare
               Assigned : Config_Data;
            begin
               Assigned := Data;
               T.Fail ("Migration data assignment must raise Program_Error.");
            end;
         exception
            when Program_Error => null;
         end;
      end;
   end Test_Migration_Copy_Rejected;

   procedure Test_Exclusive_Config_Writer (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Filename : constant String := Next_Test_Filename;
      declare
         File : constant Config_File := Create (Filename, Persistence_Schemas);
      begin
         for Alias in Boolean loop
            begin
               declare
                  Duplicate : constant Config_File :=
                    Create
                      ((if Alias then Ada.Directories.Containing_Directory (Filename) & "/./"
                         & Ada.Directories.Simple_Name (Filename) else Filename), Persistence_Schemas);
                  pragma Unreferenced (Duplicate);
               begin
                  T.Fail ("A second configuration writer must be rejected.");
               end;
            exception
               when Ada.IO_Exceptions.Use_Error => null;
            end;
         end loop;
         T.Assert (File.Last_Save = 1);
      end;
      declare
         Reopened : constant Config_File := Create (Filename, Persistence_Schemas);
      begin
         T.Assert (Reopened.Last_Save = 0, "Finalization must release the writer lease.");
      end;
      --  Failed initialization must release the lease too.
      for Attempt in 1 .. 2 loop
         begin
            declare
               Invalid : constant Config_File := Create (Filename, []);
               pragma Unreferenced (Invalid);
            begin
               T.Fail ("Unknown module should reject initialization.");
            end;
         exception
            when Constraint_Error => null;
         end;
      end loop;
   end Test_Exclusive_Config_Writer;

   procedure Test_Config_Write_Interruptions (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Lock : Fault_Test_Lock.Lock_Holder := Fault_Test_Lock.Lock with Unreferenced;
      for Power_Loss in Boolean loop
         for Step in 0 .. 150 loop
            declare
               Filename : constant String := Next_Test_Filename;
               Failed : Boolean := False;
            begin
               declare
                  File : constant Config_File := Create (Filename, Persistence_Schemas);
                  Data : Config_Data := File.Get_Data ("M");
               begin
                  --  Fill the backup ring so deletion and every rotation are exercised.
                  for I in 1 .. 22 loop
                     Data.Set ([1 => "i"], Long_Long_Integer'(3));
                     Data.Save;
                  end loop;
                  declare
                     Before : constant Virtual_String := File.Get_Data_String;
                     Count : constant Save_Counter := File.Last_Save;
                  begin
                     Data.Set ([1 => "i"], Long_Long_Integer'(7));
                     Mockable.Faults.Fail_After (Filename, Step, Power_Loss);
                     begin
                        Data.Save;
                     exception
                        when Ada.IO_Exceptions.Use_Error => Failed := True;
                     end;
                     Mockable.Faults.Disable_Failure;
                     if Failed then
                        T.Assert (File.Get_Data_String = Before, "Failed write must not publish stored data.");
                        T.Assert (File.Last_Save = Count, "Failed write must not advance save counter.");
                        if not Power_Loss then
                           Data.Save;
                           T.Assert (File.Last_Save = Count + 1, "Failed write must retain retryable deltas.");
                        end if;
                     end if;
                  end;
               end;
               if Power_Loss then
                  Mockable.Faults.Crash (Filename);
               end if;
               T.Assert (Mockable.Directories.Exists (Filename), "Primary must survive every interruption.");
               declare
                  File : constant Config_File := Create (Filename, Persistence_Schemas);
                  Data : constant Config_Data := File.Get_Data ("M");
                  Value : constant Long_Long_Integer := Data.Get ([1 => "i"]);
               begin
                  T.Assert (Value = 3 or else Value = 7, "Recovery must contain a complete old or new value.");
                  if not Failed or else not Power_Loss then
                     T.Assert (Value = 7, "Successful saves must survive power loss.");
                  end if;
               end;
               if not Failed then
                  T.Assert (Step > 50, "Fault sweep must cover backup rotation and commit boundaries.");
                  exit;
               end if;
               T.Assert (Step < 150, "Fault sweep must reach a successful write.");
            end;
         end loop;
      end loop;
   exception
      when others =>
         Mockable.Faults.Disable_Failure;
         raise;
   end Test_Config_Write_Interruptions;

   procedure Test_Reset_Discards_Deltas (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      File : constant Config_File := Create (Next_Test_Filename, Persistence_Schemas);
      declare
         Data : Config_Data := File.Get_Data ("M");
         Copy : Config_Data := Data;
      begin
         Data.Set ([1 => "i"], Long_Long_Integer'(3));
         T.Assert (Copy.Get ([1 => "i"]) = Long_Long_Integer'(3), "Copies share live updates.");
      end;
      File.Reset_Live_To_Stored;
      declare
         Data : Config_Data := File.Get_Data ("M");
         Count : constant Save_Counter := File.Last_Save;
      begin
         Data.Save;
         T.Assert (File.Last_Save = Count, "Reset must discard pending deltas.");
         T.Assert (Data.Get ([1 => "i"]) = Long_Long_Integer'(0));
      end;
   end Test_Reset_Discards_Deltas;

   procedure Test_Config_Initial_Write_Interruptions (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Lock : Fault_Test_Lock.Lock_Holder := Fault_Test_Lock.Lock with Unreferenced;
      for Power_Loss in Boolean loop
         for Step in 0 .. 12 loop
            declare
               Filename : constant String := Next_Test_Filename;
            begin
               Mockable.Faults.Fail_After (Filename, Step, Power_Loss);
               begin
                  declare
                     File : constant Config_File := Create (Filename, Persistence_Schemas);
                     pragma Unreferenced (File);
                  begin
                     null;
                  end;
               exception
                  when Ada.IO_Exceptions.Use_Error => null;
               end;
               Mockable.Faults.Disable_Failure;
               if Power_Loss then
                  Mockable.Faults.Crash (Filename);
               end if;
               declare
                  File : constant Config_File := Create (Filename, Persistence_Schemas);
                  Data : constant Config_Data := File.Get_Data ("M");
               begin
                  T.Assert (Data.Get ([1 => "i"]) = Long_Long_Integer'(0));
               end;
            end;
         end loop;
      end loop;
   exception
      when others =>
         Mockable.Faults.Disable_Failure;
         raise;
   end Test_Config_Initial_Write_Interruptions;

   procedure Test_Config_Failed_Patch_And_Validation (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Lock : Fault_Test_Lock.Lock_Holder := Fault_Test_Lock.Lock with Unreferenced;
      Filename : constant String := Next_Test_Filename;
      File : constant Config_File := Create (Filename, Persistence_Schemas);
      Data : Config_Data := File.Get_Data ("M");
      Before : constant Virtual_String := File.Get_Data_String;
      Result : Virtual_String;
      Errors : Config_Error_Vectors.Vector;
      Patch : constant Virtual_String :=
        "{""Prunt config version"":1,""Config"":{""M"":{""Version"":1,""Config"":{""i"":7}}}}";

      Data.Set ([1 => "i"], Long_Long_Integer'(11));
      for Attempt in 1 .. 2 loop
         begin
            Data.Save;
            T.Fail ("Invalid deltas must remain pending until corrected or reset.");
         exception
            when Constraint_Error => null;
         end;
      end loop;
      T.Assert (File.Get_Data_String = Before);
      Mockable.Faults.Fail_After (Filename, 3, False);
      begin
         File.Apply_Untrusted_Patch (Patch, Result, Errors);
         T.Fail ("Patch write must propagate IO failure.");
      exception
         when Ada.IO_Exceptions.Use_Error => null;
      end;
      Mockable.Faults.Disable_Failure;
      T.Assert (File.Get_Data_String = Before, "A failed patch must not update stored state.");
      File.Apply_Untrusted_Patch (Patch, Result, Errors);
      T.Assert (Errors.Is_Empty);
      T.Assert (File.Last_Save = 2);
      Data.Set ([1 => "i"], Long_Long_Integer'(4));
      Data.Save;
      T.Assert (File.Last_Save = 3, "Corrected deltas must be saveable after validation failure.");
   exception
      when others =>
         Mockable.Faults.Disable_Failure;
         raise;
   end Test_Config_Failed_Patch_And_Validation;

   procedure Test_Stored_Snapshot_Isolation (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      Filename : constant String := Next_Test_Filename;
      File : constant Config_File := Create (Filename, Persistence_Schemas);
      Data : Config_Data := File.Get_Data ("M");
      Result : Virtual_String;
      Errors : Config_Error_Vectors.Vector;
      Patch : constant Virtual_String :=
        "{""Prunt config version"":1,""Config"":{""M"":{""Version"":1,""Config"":{""i"":7}}}}";

      Data.Set ([1 => "i"], Long_Long_Integer'(3));
      Data.Save;
      Data.Set ([1 => "i"], Long_Long_Integer'(9));
      declare
         Snapshot : constant Config_File := File.Stored_Snapshot;
         Snapshot_Data : Config_Data := Snapshot.Get_Data ("M");
         Before : constant Virtual_String := Snapshot.Get_Data_String;
         Count : constant Save_Counter := File.Last_Save;
      begin
         T.Assert (Snapshot_Data.Get ([1 => "i"]) = Long_Long_Integer'(3), "Snapshot excludes unsaved edits.");
         T.Assert (Snapshot.Get_Schema_String = File.Get_Schema_String);
         T.Assert (Snapshot.Last_Save = Count);
         for Operation in 1 .. 3 loop
            begin
               case Operation is
                  when 1 => Snapshot_Data.Set ([1 => "i"], Long_Long_Integer'(5));
                  when 2 => Snapshot_Data.Save;
                  when 3 => Snapshot.Apply_Untrusted_Patch (Patch, Result, Errors);
               end case;
               T.Fail ("Snapshot must reject mutation.");
            exception
               when Ada.IO_Exceptions.Use_Error => null;
            end;
         end loop;
         T.Assert (Snapshot.Get_Data_String = Before);
         T.Assert (File.Last_Save = Count, "Snapshots must not write the source file.");
         T.Assert (Data.Get ([1 => "i"]) = Long_Long_Integer'(9), "Snapshot must not reset active live edits.");
         File.Apply_Untrusted_Patch (Patch, Result, Errors);
         T.Assert (Errors.Is_Empty);
         declare
            Reloaded : constant Config_File := File.Stored_Snapshot;
            Reloaded_Data : constant Config_Data := Reloaded.Get_Data ("M");
         begin
            T.Assert (Reloaded_Data.Get ([1 => "i"]) = Long_Long_Integer'(7), "Reload sees newly stored settings.");
            T.Assert (Snapshot_Data.Get ([1 => "i"]) = Long_Long_Integer'(3), "Existing snapshots are isolated.");
         end;
         Data.Save;
         T.Assert (Snapshot_Data.Get ([1 => "i"]) = Long_Long_Integer'(3), "Active writer remains usable.");
      end;
   end Test_Stored_Snapshot_Isolation;

   procedure Test_Stored_Snapshot_Overrides (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      File : constant Config_File :=
        Create (Next_Test_Filename, Persistence_Schemas,
                [Config_Override'(Owner => "M", Path => [1 => "i"], Value => Create (Long_Long_Integer'(4)))]);
      Snapshot : constant Config_File := File.Stored_Snapshot;
      Data : constant Config_Data := Snapshot.Get_Data ("M");
      Stored : constant JSON_Value := Read (Snapshot.Get_Data_String);

      T.Assert (Data.Get ([1 => "i"]) = Long_Long_Integer'(4), "Validation must see startup overrides.");
      T.Assert (not Stored.Get ("Config").Get ("M").Get ("Config").Has_Field ("i"));
      T.Assert (Snapshot.Get_Schema_String = File.Get_Schema_String);
   end Test_Stored_Snapshot_Overrides;

   procedure Test_Stored_Snapshot_Ownership (T : in out Trendy_Test.Operation'Class) is
      Filename : constant String := Next_Test_Filename;

      function Capture return Config_File;

      function Capture return Config_File is
         File : constant Config_File := Create (Filename, Persistence_Schemas);
         Data : Config_Data := File.Get_Data ("M");
      begin
         Data.Set ([1 => "i"], Long_Long_Integer'(3));
         Data.Save;
         return File.Stored_Snapshot;
      end Capture;
   begin
      T.Register;
      Snapshot : constant Config_File := Capture;
      Data : constant Config_Data := Snapshot.Get_Data ("M");
      Reopened : constant Config_File := Create (Filename, Persistence_Schemas);

      T.Assert (Data.Get ([1 => "i"]) = Long_Long_Integer'(3), "Snapshot outlives its source.");
      T.Assert (Reopened.Last_Save = 0, "Snapshot must not retain the source writer lease.");
   end Test_Stored_Snapshot_Ownership;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Stored_Snapshot_Isolation'Access,
         Test_Stored_Snapshot_Overrides'Access,
         Test_Stored_Snapshot_Ownership'Access,
         Test_Config_Initial_Write_Interruptions'Access,
         Test_Config_Failed_Patch_And_Validation'Access,
         Test_Migration_Copy_Rejected'Access,
         Test_Exclusive_Config_Writer'Access,
         Test_Config_Write_Interruptions'Access,
         Test_Reset_Discards_Deltas'Access,
         Test_Apply_Patch_Errors'Access,
         Test_Apply_Untrusted_Patch_Empty'Access,
         Test_Apply_Untrusted_Patch_Invalid_Module'Access,
         Test_Apply_Untrusted_Patch_Invalid_Module_Structure'Access,
         Test_Apply_Untrusted_Patch_No_Config'Access,
         Test_Backup'Access,
         Test_Config_Data_Not_Saved_Without_Call'Access,
         Test_Config_Data_Ref_Count'Access,
         Test_Config_Data_Set_Get_Boolean'Access,
         Test_Config_Data_Set_Get_Discrete'Access,
         Test_Config_Data_Set_Get_Dimensionless'Access,
         Test_Config_Data_Set_Get_Float'Access,
         Test_Config_Data_Set_Get_Float_Ratio'Access,
         Test_Config_Data_Set_Get_Integer'Access,
         Test_Config_Data_Set_Get_Variant'Access,
         Test_Create_Default_Module_Config'Access,
         Test_Create_Default_Module_Config_Unhandled_Property'Access,
         Test_Deep_Set_Repeated'Access,
         Test_Finalize_Uninitialized'Access,
         Test_Generate_Schema_Unknown_Property'Access,
         Test_Generate_Schemas_String_Unhandled_Property'Access,
         Test_Get_Data_String'Access,
         Test_Get_Dimensionless'Access,
         Test_Dynamic_Presentation_Validation'Access,
         Test_Get_Dynamically_Presented_Schema_String'Access,
         Test_Get_Empty_Path'Access,
         Test_Get_Schemas_String'Access,
         Test_Initialize_Corrupt_File'Access,
         Test_Initialize_Default_Migration'Access,
         Test_Initialize_Extra_Modules'Access,
         Test_Initialize_Newer_Module_Version'Access,
         Test_Initialize_Newer_Version'Access,
         Test_Initialize_No_Config'Access,
         Test_Initialize_Unknown_Module'Access,
         Test_Initialize_With_Invalid_Module_Config'Access,
         Test_Initialize_With_Invalid_Module_Data'Access,
         Test_Invalid_Module_Calls'Access,
         Test_Last_Save_Increment'Access,
         Test_Merge_Schemas'Access,
         Test_Merge_Schemas_Conflict_Diff_Types'Access,
         Test_Merge_Schemas_Conflict_Variant_Diff_Default'Access,
         Test_Patch_Inner_Errors'Access,
         Test_Patch_Invalid_Module'Access,
         Test_Patch_Success'Access,
         Test_Patch_Wrong_Version'Access,
         Test_Path_Errors'Access,
         Test_Recursive_Left_Merge_Bad_Types'Access,
         Test_Recursive_Merge'Access,
         Test_Recursive_Merge_Edge_Cases'Access,
         Test_Save_No_Changes'Access,
         Test_Schema_Conflict_Compatible_Disjoint'Access,
         Test_Schema_Conflict_Compatible_Merged'Access,
         Test_Schema_Conflict_Type_Mismatch'Access,
         Test_Schema_Conflict_Variant_Failure_Default'Access,
         Test_Schema_Conflict_Variant_Mismatch_Keys'Access,
         Test_Validate_Module_Config_Structure'Access,
         Test_Validate_Module_Config_To_Schema_Boolean'Access,
         Test_Validate_Module_Config_To_Schema_Check_Missing_Fields'Access,
         Test_Validate_Module_Config_To_Schema_Discrete'Access,
         Test_Validate_Module_Config_To_Schema_Empty'Access,
         Test_Validate_Module_Config_To_Schema_String'Access,
         Test_Validate_Module_Config_To_Schema_Float'Access,
         Test_Validate_Module_Config_To_Schema_Float_Ratio'Access,
         Test_Validate_Module_Config_To_Schema_Integer'Access,
         Test_Validate_Module_Config_To_Schema_Sequence'Access,
         Test_Validate_Module_Config_To_Schema_Variant'Access,
         Test_Validate_Outer_Config_Structure'Access,
         Test_Validate_Unhandled_Property'Access,
         Test_Initialize_Real_Migration'Access,
         Test_Initialize_With_Empty_Path_In_Migration'Access,
         Test_Migration_Accessors'Access,
         Test_Apply_Untrusted_Patch_Really_Empty'Access,
         Test_Reset_Live_To_Stored_Ref_Count'Access,
         Test_Reset_Live_To_Stored_Success'Access,
         Test_Set_Migration_Config_Not_Object'Access,
         Test_Config_Overrides_Hidden_From_Stored_And_Schema'Access,
         Test_Config_Overrides_Reject_Writes'Access,
         Test_Config_Overrides_Reset_Reapplies_Overrides'Access,
         Test_Config_Overrides_Variant_Selected_Rehydrates_Defaults'Access,
         Test_Config_Overrides_Invalid'Access,
         Test_Config_Overrides_Prune_During_Migration'Access,
         Test_Config_Override_Helper_Edges'Access,
         Test_Config_Path_Selected_Variant'Access,
         Test_Config_Path_Unselected_Variant_Does_Not_Report'Access,
         Test_Config_Path_Nested_Variant_Selections'Access,
         Test_Config_Path_Discriminator_Always_Reportable'Access,
         Test_Config_Path_Missing_Conditional_Targets'Access];
   end All_Tests;

end Prunt.Config.Test;
