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
with Prunt.JSON;

package body Prunt.Default_Modules.Input_Shapers is

   pragma Extensions_Allowed (On);

   use type JSON.JSON_Value_Type;

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   function Build_Shaper_Parameters
     (Method : User_Config_Input_Shaping_Method) return Prunt.Input_Shapers.Shaper_Parameters is
   begin
      case Method.Kind is
         when No_Shaper        =>
            return (Kind => Prunt.Input_Shapers.No_Shaper);

         when ZV               =>
            return
              (Kind                         => Prunt.Input_Shapers.Zero_Vibration,
               Zero_Vibration_Frequency     => Method.ZV.Shaper_Frequency,
               Zero_Vibration_Damping_Ratio => Method.ZV.Damping_Ratio,
               Zero_Vibration_Deriviatives  => Method.ZV.Number_Of_Derivatives);

         when EI               =>
            return
              (Kind                                 => Prunt.Input_Shapers.Extra_Insensitive,
               Extra_Insensitive_Frequency          => Method.EI.Shaper_Frequency,
               Extra_Insensitive_Damping_Ratio      => Method.EI.Damping_Ratio,
               Extra_Insensitive_Humps              => Method.EI.Number_Of_Humps,
               Extra_Insensitive_Residual_Vibration => Method.EI.Residual_Vibration_Level);

         when Pressure_Advance =>
            return
              (Kind                                    => Prunt.Input_Shapers.Pressure_Advance,
               Pressure_Advance_Time                   => Method.Pressure_Advance.Pressure_Advance_Time,
               Pressure_Advance_Smooth_Time            => Method.Pressure_Advance.Pressure_Advance_Smooth_Time,
               Pressure_Advance_Smooth_Added_Part_Only => Method.Pressure_Advance.Smooth_Added_Part_Only,
               Pressure_Advance_Smooth_Levels          => Positive (Method.Pressure_Advance.Smoothing_Levels));
      end case;
   end Build_Shaper_Parameters;

   overriding
   procedure Process_After_Block (This : Input_Shaping_Config_Update; Context : Block_End_Context'Class) is
      pragma Unreferenced (Context);
   begin
      Module_Instance (This.Module_Instance_Ref.Get.Element.all).Apply_Runtime_Config (This.Updated_Configs);
   end Process_After_Block;

   function Parse_Axial_Shaper_Config (Value : Virtual_String) return User_Config_Input_Shaping_Method is
      pragma Unsuppress (All_Checks);
      --  We use this to make sure that components are within range in the returned value.

      type Field_Name_Array is array (Positive range <>) of Virtual_String;

      procedure Validate_Object_Fields (Value : JSON.JSON_Value; Allowed : Field_Name_Array);

      function Get_Dimensionless
        (Value : JSON.JSON_Value; Name : Virtual_String; Minimum : Dimensionless; Maximum : Dimensionless)
         return Dimensionless;

      function Get_Integer
        (Value : JSON.JSON_Value; Name : Virtual_String; Minimum : Long_Long_Integer; Maximum : Long_Long_Integer)
         return Long_Long_Integer;

      function Get_Boolean (Value : JSON.JSON_Value; Name : Virtual_String) return Boolean;

      procedure Validate_Object_Fields (Value : JSON.JSON_Value; Allowed : Field_Name_Array) is
         procedure Check_Field (Name : Virtual_String; Field_Value : JSON.JSON_Value);

         procedure Check_Field (Name : Virtual_String; Field_Value : JSON.JSON_Value) is
            pragma Unreferenced (Field_Value);
         begin
            for Allowed_Name of Allowed loop
               if Name = Allowed_Name then
                  return;
               end if;
            end loop;

            raise Gcode_Bad_Inputs_Error
              with "JSON contains unknown field """ & Conversions.To_UTF_8_String (Name) & """.";
         end Check_Field;
      begin
         Value.Map_JSON_Object (Check_Field'Access);
      end Validate_Object_Fields;

      function Get_Dimensionless
        (Value : JSON.JSON_Value; Name : Virtual_String; Minimum : Dimensionless; Maximum : Dimensionless)
         return Dimensionless
      is
         Result : Dimensionless;
      begin
         if not Value.Has_Field (Name) then
            raise Gcode_Bad_Inputs_Error
              with "JSON is missing required field """ & Conversions.To_UTF_8_String (Name) & """.";
         elsif Value.Get (Name).Kind not in JSON.JSON_Int_Type | JSON.JSON_Float_Type then
            raise Gcode_Bad_Inputs_Error with Conversions.To_UTF_8_String (Name) & " must be a number.";
         end if;

         Result := Value.Get (Name);

         if Result not in Minimum .. Maximum then
            raise Gcode_Bad_Inputs_Error with Conversions.To_UTF_8_String (Name) & " is outside the allowed range.";
         end if;

         return Result;
      end Get_Dimensionless;

      function Get_Integer
        (Value : JSON.JSON_Value; Name : Virtual_String; Minimum : Long_Long_Integer; Maximum : Long_Long_Integer)
         return Long_Long_Integer
      is
         Result : Long_Long_Integer;
      begin
         if not Value.Has_Field (Name) then
            raise Gcode_Bad_Inputs_Error
              with "JSON is missing required field """ & Conversions.To_UTF_8_String (Name) & """.";
         elsif Value.Get (Name).Kind /= JSON.JSON_Int_Type then
            raise Gcode_Bad_Inputs_Error with Conversions.To_UTF_8_String (Name) & " must be an integer.";
         end if;

         Result := Long_Long_Integer'(Value.Get (Name).Get);

         if Result not in Minimum .. Maximum then
            raise Gcode_Bad_Inputs_Error with Conversions.To_UTF_8_String (Name) & " is outside the allowed range.";
         end if;

         return Result;
      end Get_Integer;

      function Get_Boolean (Value : JSON.JSON_Value; Name : Virtual_String) return Boolean is
      begin
         if not Value.Has_Field (Name) then
            raise Gcode_Bad_Inputs_Error
              with "JSON is missing required field """ & Conversions.To_UTF_8_String (Name) & """.";
         elsif Value.Get (Name).Kind /= JSON.JSON_Boolean_Type then
            raise Gcode_Bad_Inputs_Error with Conversions.To_UTF_8_String (Name) & " must be a boolean.";
         end if;

         return Value.Get (Name);
      end Get_Boolean;

      JSON_Result : constant JSON.Read_Result := JSON.Read (Value);
      Payload     : JSON.JSON_Value;
   begin
      if not JSON_Result.Success then
         raise Gcode_Bad_Inputs_Error
           with
             "Invalid JSON stream at "
             & JSON_Result.Error.Line'Image
             & ":"
             & JSON_Result.Error.Column'Image
             & ": "
             & Conversions.To_UTF_8_String (JSON_Result.Error.Message);
      end if;

      Payload := JSON_Result.Value;

      if not Payload.Has_Field ("Kind") then
         raise Gcode_Bad_Inputs_Error with "Kind field is missing from JSON.";
      end if;

      if Payload.Get ("Kind").Kind /= JSON.JSON_String_Type then
         raise Gcode_Bad_Inputs_Error with "Kind field is not string in JSON.";
      end if;

      --  TODO: Does Ada provide some way to get the range of an anonymous component subtype instead of hardcoding
      --  constants below?

      if Payload.Get ("Kind") = "No_Shaper" then
         Validate_Object_Fields (Payload, ["Kind"]);
         return (Kind => No_Shaper, No_Shaper => (null record));
      elsif Payload.Get ("Kind") = "Zero_Vibration" then
         Validate_Object_Fields
           (Payload, ["Kind", "Selected", "Shaper_Frequency", "Damping_Ratio", "Number_Of_Derivatives"]);

         return
           (Kind => ZV,
            ZV   =>
              (Shaper_Frequency      => Get_Dimensionless (Payload, "Shaper_Frequency", 1.0E-10, 1.0E100) * hertz,
               Damping_Ratio         =>
                 Prunt.Input_Shapers.Shaper_Damping_Ratio (Get_Dimensionless (Payload, "Damping_Ratio", 0.001, 0.999)),
               Number_Of_Derivatives =>
                 Prunt.Input_Shapers.Zero_Vibration_Deriviatives_Count
                   (Get_Integer (Payload, "Number_Of_Derivatives", 0, 3))));
      elsif Payload.Get ("Kind") = "Extra_Insensitive" then
         Validate_Object_Fields
           (Payload,
            ["Kind", "Selected", "Shaper_Frequency", "Damping_Ratio", "Residual_Vibration_Level", "Number_Of_Humps"]);

         return
           (Kind => EI,
            EI   =>
              (Shaper_Frequency         => Get_Dimensionless (Payload, "Shaper_Frequency", 1.0E-10, 1.0E100) * hertz,
               Damping_Ratio            =>
                 Prunt.Input_Shapers.Shaper_Damping_Ratio (Get_Dimensionless (Payload, "Damping_Ratio", 0.001, 0.999)),
               Residual_Vibration_Level =>
                 Prunt.Input_Shapers.Residual_Vibration_Level
                   (Get_Dimensionless (Payload, "Residual_Vibration_Level", 0.001, 0.999)),
               Number_Of_Humps          =>
                 Prunt.Input_Shapers.Extra_Insensitive_Humps_Count (Get_Integer (Payload, "Number_Of_Humps", 1, 3))));
      elsif Payload.Get ("Kind") = "Pressure_Advance" then
         Validate_Object_Fields
           (Payload,
            ["Kind",
             "Selected",
             "Pressure_Advance_Time",
             "Pressure_Advance_Smooth_Time",
             "Smooth_Added_Part_Only",
             "Smoothing_Levels"]);

         return
           (Kind             => Pressure_Advance,
            Pressure_Advance =>
              (Pressure_Advance_Time        =>
                 Get_Dimensionless (Payload, "Pressure_Advance_Time", -1.0E100, 1.0E100) * s,
               Pressure_Advance_Smooth_Time =>
                 Get_Dimensionless (Payload, "Pressure_Advance_Smooth_Time", 0.0, 0.2) * s,
               Smooth_Added_Part_Only       => Get_Boolean (Payload, "Smooth_Added_Part_Only"),
               Smoothing_Levels             =>
                 User_Config_Integer (Get_Integer (Payload, "Smoothing_Levels", 1, 10))));
      else
         raise Gcode_Bad_Inputs_Error with "Unknown shaper kind in JSON.";
      end if;
   exception
      when Constraint_Error =>
         raise Gcode_Bad_Inputs_Error
           with
             "JSON contains a value outside the allowed range, this is a bug in Prunt, the out of range value should"
             & " have been caught earlier.";
   end Parse_Axial_Shaper_Config;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema is
   begin
      return (Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Report_Config_Error, Status_Emitter);

      Parsed_Config                     : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Config_Saving_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Config_Saving_Module.Module_Instance'Tag);
      Config_Saver                      : Config_Saving_Module.Config_Saver'Class renames
        Config_Saving_Module.Config_Saver'Class (Config_Saving_Module_Instance_Ref.Get.Element.all);
   begin
      return Result : Module_Instance do
         Config_Saver.Register_For_Saving (Config_Data);
         Result.Initialize (Parsed_Config, Config_Data);
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config; Config_Data_In : Prunt.Config.Config_Data) is
      begin
         Config := Config_In;
         Config_Data := Config_Data_In;
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Planner);
      begin
         Self_Ref := Self_Ref_In;
      end Start;

      procedure Apply_Runtime_Config (Updates : Input_Shaping_Update_Maps.Map) is
      begin
         for Update in Updates.Iterate loop
            Config.Input_Shaping (Update.Key) := Update.Element;
         end loop;

         User_Config_To_Config_Data (Config_Data, Config);
      end Apply_Runtime_Config;

      function Get_Current_Axial_Shapers return Prunt.Input_Shapers.Axial_Shaper_Parameters is
      begin
         return [for Axis in Axis_Name => Build_Shaper_Parameters (Config.Input_Shaping (Axis))];
      end Get_Current_Axial_Shapers;
   end Module_Instance;

   procedure Configure_Input_Shaping
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      P        : Virtual_String;
      X        : Gcode_Optional_String;
      Y        : Gcode_Optional_String;
      Z        : Gcode_Optional_String;
      E        : Gcode_Optional_String)
   is
      Updated_Configs : Input_Shaping_Update_Maps.Map := [];
      Updated         : Boolean := False;
      New_Shapers     : Prunt.Input_Shapers.Axial_Shaper_Parameters :=
        Planner.Get_Last_Kinematic_Parameters.Axial_Shapers;

      procedure Handle_Axis (Axis : Axis_Name; Value : Gcode_Optional_String);

      procedure Handle_Axis (Axis : Axis_Name; Value : Gcode_Optional_String) is
         Method : User_Config_Input_Shaping_Method;
      begin
         if Value.Present then
            Method := Parse_Axial_Shaper_Config (Value.Value);
            Updated_Configs.Insert (Axis, Method);
            New_Shapers (Axis) := Build_Shaper_Parameters (Method);
            Updated := True;
         end if;
      exception
         when Error : Gcode_Bad_Inputs_Error =>
            raise Gcode_Bad_Inputs_Error
              with "Invalid " & Axis'Image & " payload: " & Ada.Exceptions.Exception_Message (Error);
      end Handle_Axis;
   begin
      if P /= "Prunt" then
         raise Gcode_Bad_Inputs_Error with "The P parameter must be set to ""Prunt"".";
      end if;

      Handle_Axis (X_Axis, X);
      Handle_Axis (Y_Axis, Y);
      Handle_Axis (Z_Axis, Z);
      Handle_Axis (E_Axis, E);

      if Updated then
         Planner.Flush_And_Change_Kinematic_Parameters
           (Params     => (Planner.Get_Last_Kinematic_Parameters with delta Axial_Shapers => New_Shapers),
            Extra_Data =>
              Input_Shaping_Config_Update'(Module_Instance_Ref => Self_Ref, Updated_Configs => Updated_Configs));
      end if;
   end Configure_Input_Shaping;

end Prunt.Default_Modules.Input_Shapers;
