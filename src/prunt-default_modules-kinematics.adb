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

with Prunt.Default_Modules.Kinematics.Config_Paths;

package body Prunt.Default_Modules.Kinematics is

   pragma Extensions_Allowed (On);

   package My_Config_Paths is new Config_Paths;

   function Map_Axis_Is_Motor_Separable (Map : Motor_Position_Map; Axis : Axis_Name) return Boolean is
   begin
      for Motor in Motor_Name loop
         if Map (Axis, Motor) /= 0.0 / mm then
            for Other_Axis in Axis_Name loop
               if Other_Axis /= Axis and then Map (Other_Axis, Motor) /= 0.0 / mm then
                  return False;
               end if;
            end loop;
         end if;
      end loop;

      return True;
   end Map_Axis_Is_Motor_Separable;

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

   function Build_Cornering_Parameters (Cornering : User_Config_Cornering) return Motion_Planner.Cornering_Parameters
   is
   begin
      case Cornering.Kind is
         when Stereographic =>
            return
              (Kind                 => Motion_Planner.Stereographic,
               Stereographic_Params =>
                 (Axial_Deviation_Maxes    => [for X of Cornering.Stereographic_Params.Axial_Deviation_Limits => X],
                  Corner_Miss_Distance_Max => Cornering.Stereographic_Params.Maximum_Corner_Miss_Distance,
                  Shape_Bias               => Cornering.Stereographic_Params.Shape_Bias,
                  Circularity              => Cornering.Stereographic_Params.Circularity));

         when Circular      =>
            return
              (Kind            => Motion_Planner.Circular,
               Circular_Params =>
                 (Axial_Deviation_Maxes    => [for X of Cornering.Circular_Params.Axial_Deviation_Limits => X],
                  Corner_Miss_Distance_Max => Cornering.Circular_Params.Maximum_Corner_Miss_Distance,
                  Radius_Max               => Cornering.Circular_Params.Maximum_Radius));

         when Parabolic     =>
            return
              (Kind             => Motion_Planner.Parabolic,
               Parabolic_Params =>
                 (Axial_Deviation_Maxes    => [for X of Cornering.Parabolic_Params.Axial_Deviation_Limits => X],
                  Corner_Miss_Distance_Max => Cornering.Parabolic_Params.Maximum_Corner_Miss_Distance,
                  Shape_Bias               => Cornering.Parabolic_Params.Shape_Bias,
                  Trim_Max                 => Cornering.Parabolic_Params.Maximum_Trim));

         when Biarc         =>
            return
              (Kind         => Motion_Planner.Biarc,
               Biarc_Params =>
                 (Axial_Deviation_Maxes    => [for X of Cornering.Biarc_Params.Axial_Deviation_Limits => X],
                  Corner_Miss_Distance_Max => Cornering.Biarc_Params.Maximum_Corner_Miss_Distance,
                  Shape_Bias               => Cornering.Biarc_Params.Shape_Bias,
                  Trim_Max                 => Cornering.Biarc_Params.Maximum_Trim));

         when Sharp_SCV     =>
            return
              (Kind             => Motion_Planner.Sharp_SCV,
               Sharp_SCV_Params => (Square_Corner_Velocity => Cornering.Sharp_SCV_Params.Square_Corner_Velocity));
      end case;
   end Build_Cornering_Parameters;

   function Build_Motion_Planner_Configuration
     (Config : User_Config; Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance_Interface'Class)
      return Motion_Planner_Configuration
   is
      Result : Motion_Planner_Configuration :=
        (Parameters         =>
           Motion_Planner.Kinematic_Parameters'
             (Lower_Pos_Limit          => [for X of Config.Kinematics.Lower_Position_Limit => X],
              Upper_Pos_Limit          => [for X of Config.Kinematics.Upper_Position_Limit => X],
              Ignore_E_In_XYZE         => Config.Kinematics.Ignore_E_In_XYZE,
              Tangential_Velocity_Max  => Config.Kinematics.Maximum_Tangential_Velocity,
              Axial_Velocity_Maxes     => [for X of Config.Kinematics.Axial_Velocity_Limits => X],
              Axial_Acceleration_Maxes => [for X of Config.Kinematics.Axial_Acceleration_Limits => X],
              Axial_Jerk_Maxes         => [for X of Config.Kinematics.Axial_Jerk_Limits => X],
              Axial_Snap_Maxes         => [for X of Config.Kinematics.Axial_Snap_Limits => X],
              Axial_Crackle_Maxes      => [for X of Config.Kinematics.Axial_Crackle_Limits => X],
              Cornering                => Build_Cornering_Parameters (Config.Kinematics.Cornering),
              Axial_Shapers            => <>),
         Motors_To_Position => [others => [others => 0.0 / mm]]);
   begin
      for M in Motor_Name loop
         declare
            Distance_Per_Unit  : constant Length := Motor_Drivers_Module_Instance.Distance_Per_Unit (M);
            Units_Per_Distance : constant Curvature := 1.0 / Distance_Per_Unit;
         begin
            case Config.Kinematics.Kinematics_Kind.Kind is
               when Cartesian =>
                  case Config.Kinematics.Kinematics_Kind.Cartesian (M) is
                     when None   =>
                        null;

                     when X_Axis =>
                        Result.Motors_To_Position (X_Axis, M) := Units_Per_Distance;

                     when Y_Axis =>
                        Result.Motors_To_Position (Y_Axis, M) := Units_Per_Distance;

                     when Z_Axis =>
                        Result.Motors_To_Position (Z_Axis, M) := Units_Per_Distance;

                     when E_Axis =>
                        Result.Motors_To_Position (E_Axis, M) := Units_Per_Distance;
                  end case;

               when Core_XY   =>
                  case Config.Kinematics.Kinematics_Kind.Core_XY (M) is
                     when None   =>
                        null;

                     when A_Axis =>
                        Result.Motors_To_Position (X_Axis, M) := Units_Per_Distance;
                        Result.Motors_To_Position (Y_Axis, M) := Units_Per_Distance;

                     when B_Axis =>
                        Result.Motors_To_Position (X_Axis, M) := Units_Per_Distance;
                        Result.Motors_To_Position (Y_Axis, M) := -Units_Per_Distance;

                     when Z_Axis =>
                        Result.Motors_To_Position (Z_Axis, M) := Units_Per_Distance;

                     when E_Axis =>
                        Result.Motors_To_Position (E_Axis, M) := Units_Per_Distance;
                  end case;
            end case;
         end;
      end loop;

      return Result;
   end Build_Motion_Planner_Configuration;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class is
   begin
      return Config.Versioned_Config_Schema'(Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path'Class; Message : Virtual_String);
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
               Motor_Drivers_Module_Instance_Ref_In => Motor_Drivers_Module_Instance_Ref);

            declare
               Planner_Config : constant Motion_Planner_Configuration :=
                 Build_Motion_Planner_Configuration (Parsed_Config, Motor_Drivers_Module_Instance);
               Zero_Curvature : constant Curvature := 0.0 / mm;
            begin
               Motor_Drivers_Module_Instance.Set_Motor_Axis_Map
                 ([for Axis in Axis_Name =>
                     [for Motor in Motor_Name => Planner_Config.Motors_To_Position (Axis, Motor) /= Zero_Curvature]]);
            end;

            case Parsed_Config.Kinematics.Kinematics_Kind.Kind is
               when Cartesian =>
                  for M in Motor_Name loop
                     if Parsed_Config.Kinematics.Kinematics_Kind.Cartesian (M) /= None
                       and then not Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M)
                     then
                        Report_Config_Error
                          (My_Config_Paths.Root.Kinematics.Kinematics_Kind.Cartesian (M),
                           "This motor is assigned to an axis but the motor is disabled in its motor configuration.");
                     end if;
                  end loop;

               when Core_XY   =>
                  for M in Motor_Name loop
                     if Parsed_Config.Kinematics.Kinematics_Kind.Core_XY (M) /= None
                       and then not Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M)
                     then
                        Report_Config_Error
                          (My_Config_Paths.Root.Kinematics.Kinematics_Kind.Core_XY (M),
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
         Motor_Drivers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref) is
      begin
         Config := Config_In;
         Config_Data := Config_Data_In;
         Motor_Drivers_Module_Instance_Ref := Motor_Drivers_Module_Instance_Ref_In;
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         null;
      end Start;

      procedure Apply_Runtime_Config (Updates : Runtime_Kinematics_Updates) is
      begin
         for Axis in Axis_Name loop
            if Updates.Has_Axial_Velocity_Limit (Axis) then
               Config.Kinematics.Axial_Velocity_Limits (Axis) := Updates.Axial_Velocity_Limits (Axis);
            end if;

            if Updates.Has_Axial_Acceleration_Limit (Axis) then
               Config.Kinematics.Axial_Acceleration_Limits (Axis) := Updates.Axial_Acceleration_Limits (Axis);
            end if;

            if Updates.Has_Axial_Jerk_Limit (Axis) then
               Config.Kinematics.Axial_Jerk_Limits (Axis) := Updates.Axial_Jerk_Limits (Axis);
            end if;

            if Updates.Has_Axial_Snap_Limit (Axis) then
               Config.Kinematics.Axial_Snap_Limits (Axis) := Updates.Axial_Snap_Limits (Axis);
            end if;

            if Updates.Has_Axial_Crackle_Limit (Axis) then
               Config.Kinematics.Axial_Crackle_Limits (Axis) := Updates.Axial_Crackle_Limits (Axis);
            end if;
         end loop;

         User_Config_To_Config_Data (Config_Data, Config);
      end Apply_Runtime_Config;

      function Get_Default_Motion_Planner_Configuration return Motion_Planner_Configuration is
         Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance_Interface'Class renames
           Motor_Drivers_Module.Module_Instance_Interface'Class (Motor_Drivers_Module_Instance_Ref.Get.Element.all);
      begin
         return Build_Motion_Planner_Configuration (Config, Motor_Drivers_Module_Instance);
      end Get_Default_Motion_Planner_Configuration;

      function Axis_Is_Motor_Separable (Axis : Axis_Name) return Boolean is
      begin
         return Map_Axis_Is_Motor_Separable (Get_Default_Motion_Planner_Configuration.Motors_To_Position, Axis);
      end Axis_Is_Motor_Separable;

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
      pragma Unsuppress (All_Checks);
      --  Required so we get a Constraint_Error when we try to set an invalid value.
      pragma Unreferenced (This);

      Updates    : Runtime_Kinematics_Updates;
      New_Params : Motion_Planner.Kinematic_Parameters := Planner.Get_Last_Kinematic_Parameters;
      Updated    : Boolean := False;

      procedure Handle_Axis (Axis : Axis_Name; Value : Gcode_Optional_Float);

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

            Updates.Has_Axial_Velocity_Limit (Axis) := True;
            Updates.Axial_Velocity_Limits (Axis) := Limit;
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
      C        : Gcode_Optional_Float)
   is
      pragma Unsuppress (All_Checks);
      --  Required so we get a Constraint_Error when we try to set an invalid value.
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
            Updates.Axial_Acceleration_Limits := [others => A.Value * mm / Prunt.s ** 2];
         exception
            when Constraint_Error =>
               raise Gcode_Bad_Inputs_Error with "Acceleration limit must be between 1.0E-6 and 1.0E100 mm/s^2.";
         end;

         Updates.Has_Axial_Acceleration_Limit := [others => True];
         New_Params.Axial_Acceleration_Maxes := Updates.Axial_Acceleration_Limits;
         Updated := True;
      end if;

      if J.Present then
         begin
            Updates.Axial_Jerk_Limits := [others => J.Value * mm / Prunt.s ** 3];
         exception
            when Constraint_Error =>
               raise Gcode_Bad_Inputs_Error with "Jerk limit must be between 1.0E-6 and 1.0E100 mm/s^3.";
         end;

         Updates.Has_Axial_Jerk_Limit := [others => True];
         New_Params.Axial_Jerk_Maxes := Updates.Axial_Jerk_Limits;
         Updated := True;
      end if;

      if S.Present then
         begin
            Updates.Axial_Snap_Limits := [others => S.Value * mm / Prunt.s ** 4];
         exception
            when Constraint_Error =>
               raise Gcode_Bad_Inputs_Error with "Snap limit must be between 1.0E-6 and 1.0E100 mm/s^4.";
         end;

         Updates.Has_Axial_Snap_Limit := [others => True];
         New_Params.Axial_Snap_Maxes := Updates.Axial_Snap_Limits;
         Updated := True;
      end if;

      if C.Present then
         begin
            Updates.Axial_Crackle_Limits := [others => C.Value * mm / Prunt.s ** 5];
         exception
            when Constraint_Error =>
               raise Gcode_Bad_Inputs_Error with "Crackle limit must be between 1.0E-6 and 1.0E100 mm/s^5.";
         end;

         Updates.Has_Axial_Crackle_Limit := [others => True];
         New_Params.Axial_Crackle_Maxes := Updates.Axial_Crackle_Limits;
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
