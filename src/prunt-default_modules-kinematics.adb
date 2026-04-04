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

package body Prunt.Default_Modules.Kinematics is

   pragma Extensions_Allowed (On);

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

   overriding
   procedure Process_After_Block (This : Kinematics_Config_Update; Context : Block_End_Context'Class) is
      pragma Unreferenced (Context);

      Instance : Module_Instance renames Module_Instance (This.Module_Instance_Ref.Get.Element.all);
   begin
      Instance.Apply_Runtime_Config (This.Updates);
   end Process_After_Block;

   function Build_Motion_Planner_Configuration
     (Config                        : User_Config;
      Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance_Interface'Class;
      Input_Shapers_Module_Instance : Input_Shapers_Module.Module_Instance_Interface'Class)
      return Motion_Planner_Configuration
   is
      Result : Motion_Planner_Configuration :=
        (Parameters         =>
           Motion_Planner.Kinematic_Parameters'
             (Lower_Pos_Limit         => [for X of Config.Kinematics.Lower_Position_Limit => X],
              Upper_Pos_Limit         => [for X of Config.Kinematics.Upper_Position_Limit => X],
              Ignore_E_In_XYZE        => Config.Kinematics.Ignore_E_In_XYZE,
              Shift_Blended_Corners   => Config.Kinematics.Shift_Blended_Corners,
              Tangential_Velocity_Max => Config.Kinematics.Maximum_Tangential_Velocity,
              Axial_Velocity_Maxes    => [for X of Config.Kinematics.Axial_Velocity_Limits => X],
              Acceleration_Max        => Config.Kinematics.Maximum_Acceleration,
              Jerk_Max                => Config.Kinematics.Maximum_Jerk,
              Snap_Max                => Config.Kinematics.Maximum_Snap,
              Crackle_Max             => Config.Kinematics.Maximum_Crackle,
              Chord_Error_Max         => Config.Kinematics.Maximum_Chord_Error,
              Axial_Scaler            => [for X of Config.Kinematics.Axial_Scaler => X],
              Axial_Shapers           => Input_Shapers_Module_Instance.Get_Current_Axial_Shapers),
         Motors_To_Position => [others => [others => Length'Last]]);
   begin
      for M in Motor_Name loop
         declare
            Distance_Per_Unit : constant Length := Motor_Drivers_Module_Instance.Distance_Per_Unit (M);
         begin
            case Config.Kinematics.Kinematics_Kind.Kind is
               when Cartesian =>
                  case Config.Kinematics.Kinematics_Kind.Cartesian (M) is
                     when None   =>
                        null;

                     when X_Axis =>
                        Result.Motors_To_Position (X_Axis, M) := Distance_Per_Unit;

                     when Y_Axis =>
                        Result.Motors_To_Position (Y_Axis, M) := Distance_Per_Unit;

                     when Z_Axis =>
                        Result.Motors_To_Position (Z_Axis, M) := Distance_Per_Unit;

                     when E_Axis =>
                        Result.Motors_To_Position (E_Axis, M) := Distance_Per_Unit;
                  end case;

               when Core_XY   =>
                  case Config.Kinematics.Kinematics_Kind.Core_XY (M) is
                     when None   =>
                        null;

                     when A_Axis =>
                        Result.Motors_To_Position (X_Axis, M) := Distance_Per_Unit;
                        Result.Motors_To_Position (Y_Axis, M) := Distance_Per_Unit;

                     when B_Axis =>
                        Result.Motors_To_Position (X_Axis, M) := Distance_Per_Unit;
                        Result.Motors_To_Position (Y_Axis, M) := -Distance_Per_Unit;

                     when Z_Axis =>
                        Result.Motors_To_Position (Z_Axis, M) := Distance_Per_Unit;

                     when E_Axis =>
                        Result.Motors_To_Position (E_Axis, M) := Distance_Per_Unit;
                  end case;
            end case;
         end;
      end loop;

      return Result;
   end Build_Motion_Planner_Configuration;

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
      pragma Unreferenced (This, Status_Emitter);

      Parsed_Config                     : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Config_Saving_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Config_Saving_Module.Module_Instance'Tag);
      Motor_Drivers_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Motor_Drivers_Module.Module_Instance'Tag);
      Input_Shapers_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Input_Shapers_Module.Module_Instance'Tag);
   begin
      return Result : Module_Instance do
         declare
            Config_Saver                  : Config_Saving_Module.Config_Saver'Class renames
              Config_Saving_Module.Config_Saver'Class (Config_Saving_Module_Instance_Ref.Get.Element.all);
            Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance_Interface'Class renames
              Motor_Drivers_Module.Module_Instance_Interface'Class (Motor_Drivers_Module_Instance_Ref.Get.Element.all);
         begin
            Config_Saver.Register_For_Saving (Config_Data);
            Result.Initialize
              (Config_In                            => Parsed_Config,
               Config_Data_In                       => Config_Data,
               Motor_Drivers_Module_Instance_Ref_In => Motor_Drivers_Module_Instance_Ref,
               Input_Shapers_Module_Instance_Ref_In => Input_Shapers_Module_Instance_Ref);

            case Parsed_Config.Kinematics.Kinematics_Kind.Kind is
               when Cartesian =>
                  for M in Motor_Name loop
                     if Parsed_Config.Kinematics.Kinematics_Kind.Cartesian (M) /= None
                       and then not Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M)
                     then
                        Report_Config_Error
                          (["Kinematics", "Kinematics_Kind", "Kind", "Children", "Cartesian", "Cartesian", +M'Image],
                           "This motor is assigned to an axis but the motor is disabled in its motor configuration.");
                     end if;
                  end loop;

               when Core_XY   =>
                  for M in Motor_Name loop
                     if Parsed_Config.Kinematics.Kinematics_Kind.Core_XY (M) /= None
                       and then not Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M)
                     then
                        Report_Config_Error
                          (["Kinematics", "Kinematics_Kind", "Kind", "Children", "Core_XY", "Core_XY", +M'Image],
                           "This motor is assigned to an axis but the motor is disabled in its motor configuration.");
                     end if;
                  end loop;
            end case;
         end;
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize
        (Config_In                            : User_Config;
         Config_Data_In                       : Prunt.Config.Config_Data;
         Motor_Drivers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Input_Shapers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref) is
      begin
         Config := Config_In;
         Config_Data := Config_Data_In;
         Motor_Drivers_Module_Instance_Ref := Motor_Drivers_Module_Instance_Ref_In;
         Input_Shapers_Module_Instance_Ref := Input_Shapers_Module_Instance_Ref_In;
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         null;
      end Start;

      procedure Apply_Runtime_Config (Updates : Runtime_Kinematics_Updates) is
      begin
         for Update in Updates.Axial_Velocity_Limits.Iterate loop
            Config.Kinematics.Axial_Velocity_Limits (Update.Key) := Update.Element;
         end loop;

         if Updates.Has_Maximum_Acceleration then
            Config.Kinematics.Maximum_Acceleration := Updates.Maximum_Acceleration;
         end if;

         if Updates.Has_Maximum_Jerk then
            Config.Kinematics.Maximum_Jerk := Updates.Maximum_Jerk;
         end if;

         if Updates.Has_Maximum_Snap then
            Config.Kinematics.Maximum_Snap := Updates.Maximum_Snap;
         end if;

         if Updates.Has_Maximum_Crackle then
            Config.Kinematics.Maximum_Crackle := Updates.Maximum_Crackle;
         end if;

         if Updates.Has_Maximum_Chord_Error then
            Config.Kinematics.Maximum_Chord_Error := Updates.Maximum_Chord_Error;
         end if;

         User_Config_To_Config_Data (Config_Data, Config);
      end Apply_Runtime_Config;

      function Get_Default_Motion_Planner_Configuration return Motion_Planner_Configuration is
         Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance_Interface'Class renames
           Motor_Drivers_Module.Module_Instance_Interface'Class (Motor_Drivers_Module_Instance_Ref.Get.Element.all);
         Input_Shapers_Module_Instance : Input_Shapers_Module.Module_Instance_Interface'Class renames
           Input_Shapers_Module.Module_Instance_Interface'Class (Input_Shapers_Module_Instance_Ref.Get.Element.all);
      begin
         return
           Build_Motion_Planner_Configuration (Config, Motor_Drivers_Module_Instance, Input_Shapers_Module_Instance);
      end Get_Default_Motion_Planner_Configuration;

      function Get_Config return User_Config is
      begin
         return Config;
      end Get_Config;
   end Module_Instance;

   procedure Set_Max_Feedrate
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_Float;
      Y        : Gcode_Optional_Float;
      Z        : Gcode_Optional_Float;
      E        : Gcode_Optional_Float)
   is
      pragma Unsupress (All_Checks);
      --  Required so we get a `Constraint_Error` when we try to set an invalid value.
      pragma Unreferenced (This);

      Updates    : Runtime_Kinematics_Updates;
      New_Params : Motion_Planner.Kinematic_Parameters := Planner.Get_Last_Kinematic_Parameters;
      Updated    : Boolean := False;

      procedure Handle_Axis (Axis : Axis_Name; Value : Gcode_Optional_Float) is
         Limit : Velocity;
      begin
         if Value.Present then
            begin
               Limit := Value.Value * mm / s;
            exception
               when Constraint_Error =>
                  raise Gcode_Bad_Inputs_Error with Axis'Image & " feedrate must be between 1.0E-6 and 1.0E100 mm/s.";
            end;

            Updates.Axial_Velocity_Limits.Insert (Axis, Limit);
            New_Params.Axial_Velocity_Maxes (Axis) := Limit;
            Updated := True;
         end if;
      end Handle_Axis;
   begin
      Handle_Axis (X_Axis, X);
      Handle_Axis (Y_Axis, Y);
      Handle_Axis (Z_Axis, Z);
      Handle_Axis (E_Axis, E);

      if not Updated then
         raise Gcode_Bad_Inputs_Error with "Provide at least one axis feedrate.";
      end if;

      Planner.Flush_And_Change_Kinematic_Parameters
        (Params     => New_Params,
         Extra_Data => Kinematics_Config_Update'(Module_Instance_Ref => Self_Ref, Updates => Updates));
   end Set_Max_Feedrate;

   procedure Set_Dynamic_Kinematic_Limits
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      P        : Virtual_String;
      A        : Gcode_Optional_Float;
      J        : Gcode_Optional_Float;
      S        : Gcode_Optional_Float;
      C        : Gcode_Optional_Float;
      D        : Gcode_Optional_Float)
   is
      pragma Unsupress (All_Checks);
      --  Required so we get a `Constraint_Error` when we try to set an invalid value.
      pragma Unreferenced (This);

      Updates    : Runtime_Kinematics_Updates;
      New_Params : Motion_Planner.Kinematic_Parameters := Planner.Get_Last_Kinematic_Parameters;
      Updated    : Boolean := False;
   begin
      if P /= "Prunt" then
         raise Gcode_Bad_Inputs_Error with "The P parameter must be set to ""Prunt"".";
      end if;

      if A.Present then
         begin
            Updates.Maximum_Acceleration := A.Value * mm / Prunt.s ** 2;
         exception
            when Constraint_Error =>
               raise Gcode_Bad_Inputs_Error with "Acceleration limit must be between 1.0E-6 and 1.0E100 mm/s^2.";
         end;

         Updates.Has_Maximum_Acceleration := True;
         New_Params.Acceleration_Max := Updates.Maximum_Acceleration;
         Updated := True;
      end if;

      if J.Present then
         begin
            Updates.Maximum_Jerk := J.Value * mm / Prunt.s ** 3;
         exception
            when Constraint_Error =>
               raise Gcode_Bad_Inputs_Error with "Jerk limit must be between 1.0E-6 and 1.0E100 mm/s^3.";
         end;

         Updates.Has_Maximum_Jerk := True;
         New_Params.Jerk_Max := Updates.Maximum_Jerk;
         Updated := True;
      end if;

      if S.Present then
         begin
            Updates.Maximum_Snap := S.Value * mm / Prunt.s ** 4;
         exception
            when Constraint_Error =>
               raise Gcode_Bad_Inputs_Error with "Snap limit must be between 1.0E-6 and 1.0E100 mm/s^4.";
         end;

         Updates.Has_Maximum_Snap := True;
         New_Params.Snap_Max := Updates.Maximum_Snap;
         Updated := True;
      end if;

      if C.Present then
         begin
            Updates.Maximum_Crackle := C.Value * mm / Prunt.s ** 5;
         exception
            when Constraint_Error =>
               raise Gcode_Bad_Inputs_Error with "Crackle limit must be between 1.0E-6 and 1.0E100 mm/s^5.";
         end;

         Updates.Has_Maximum_Crackle := True;
         New_Params.Crackle_Max := Updates.Maximum_Crackle;
         Updated := True;
      end if;

      if D.Present then
         begin
            Updates.Maximum_Chord_Error := D.Value * mm;
         exception
            when Constraint_Error =>
               raise Gcode_Bad_Inputs_Error with "Path deviation must be between 0 and 1.0E100 mm.";
         end;

         Updates.Has_Maximum_Chord_Error := True;
         New_Params.Chord_Error_Max := Updates.Maximum_Chord_Error;
         Updated := True;
      end if;

      if not Updated then
         raise Gcode_Bad_Inputs_Error with "Provide at least one dynamic kinematic limit.";
      end if;

      Planner.Flush_And_Change_Kinematic_Parameters
        (Params     => New_Params,
         Extra_Data => Kinematics_Config_Update'(Module_Instance_Ref => Self_Ref, Updates => Updates));
   end Set_Dynamic_Kinematic_Limits;

end Prunt.Default_Modules.Kinematics;
