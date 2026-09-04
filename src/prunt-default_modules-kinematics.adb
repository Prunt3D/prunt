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

with Ada.Numerics.Generic_Elementary_Functions;
with Prunt.Default_Modules.Kinematics.Config_Paths;

package body Prunt.Default_Modules.Kinematics is

   pragma Extensions_Allowed (On);

   package My_Config_Paths is new Config_Paths;
   package Angle_Math is new Ada.Numerics.Generic_Elementary_Functions (Angle);
   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);

   function Kinematics_Kind_Config_Path return Config.Config_Path
   is (My_Config_Paths.Root.Kinematics.Kinematics_Kind.Kind);

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

   function Planner_Workspace_Bounds (Config : User_Config_Workspace_Bounds) return Motion_Planner.Workspace_Bounds is
   begin
      case Config.Kind is
         when Rectangular =>
            return
              (Kind    => Motion_Planner.Rectangular_Workspace,
               Lower_Z => Config.Lower_Z,
               Upper_Z => Config.Upper_Z,
               Lower_E => Config.Lower_E,
               Upper_E => Config.Upper_E,
               Lower_X => Config.Lower_X,
               Upper_X => Config.Upper_X,
               Lower_Y => Config.Lower_Y,
               Upper_Y => Config.Upper_Y);

         when Circular    =>
            return
              (Kind    => Motion_Planner.Circular_Workspace,
               Lower_Z => Config.Lower_Z,
               Upper_Z => Config.Upper_Z,
               Lower_E => Config.Lower_E,
               Upper_E => Config.Upper_E,
               Radius  => Config.Radius);
      end case;
   end Planner_Workspace_Bounds;

   function Hypot (X, Y : Length) return Length is
      DX    : constant Dimensionless := X / mm;
      DY    : constant Dimensionless := Y / mm;
      Scale : constant Dimensionless := Dimensionless'Max (abs DX, abs DY);
   begin
      if Scale = 0.0 then
         return 0.0 * mm;
      end if;
      return Scale * Dimensionless_Math.Sqrt ((DX / Scale) ** 2 + (DY / Scale) ** 2) * mm;
   end Hypot;

   procedure Workspace_Tower_Extents
     (Workspace      : User_Config_Workspace_Bounds;
      Tower_X        : Length;
      Tower_Y        : Length;
      Maximum_DX     : out Length;
      Maximum_DY     : out Length;
      Maximum_Radius : out Length) is
   begin
      case Workspace.Kind is
         when Rectangular =>
            Maximum_DX := Length'Max (abs (Workspace.Lower_X - Tower_X), abs (Workspace.Upper_X - Tower_X));
            Maximum_DY := Length'Max (abs (Workspace.Lower_Y - Tower_Y), abs (Workspace.Upper_Y - Tower_Y));
            Maximum_Radius := Hypot (Maximum_DX, Maximum_DY);

         when Circular    =>
            Maximum_DX := abs Tower_X + Workspace.Radius;
            Maximum_DY := abs Tower_Y + Workspace.Radius;
            Maximum_Radius := Hypot (Tower_X, Tower_Y) + Workspace.Radius;
      end case;
   end Workspace_Tower_Extents;

   function Build_Motion_Planner_Configuration
     (Config : User_Config; Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance_Interface'Class)
      return Motion_Planner_Configuration
   is
      Result : Motion_Planner_Configuration :=
        (Parameters =>
           Motion_Planner.Kinematic_Parameters'
             (Bounds                   => Planner_Workspace_Bounds (Config.Kinematics.Workspace_Bounds),
              Ignore_E_In_XYZE         => Config.Kinematics.Ignore_E_In_XYZE,
              Tangential_Velocity_Max  => Config.Kinematics.Maximum_Tangential_Velocity,
              Axial_Velocity_Maxes     => [for X of Config.Kinematics.Axial_Velocity_Limits => X],
              Axial_Acceleration_Maxes => [for X of Config.Kinematics.Axial_Acceleration_Limits => X],
              Axial_Jerk_Maxes         => [for X of Config.Kinematics.Axial_Jerk_Limits => X],
              Axial_Snap_Maxes         => [for X of Config.Kinematics.Axial_Snap_Limits => X],
              Axial_Crackle_Maxes      => [for X of Config.Kinematics.Axial_Crackle_Limits => X],
              Cornering                => Build_Cornering_Parameters (Config.Kinematics.Cornering),
              Axial_Shapers            => <>),
         Transform  => (Kind => Linear_Transform, others => <>));
   begin
      for M in Motor_Name loop
         declare
            Distance_Per_Unit  : constant Length := Motor_Drivers_Module_Instance.Distance_Per_Unit (M);
            Units_Per_Distance : constant Curvature := 1.0 / Distance_Per_Unit;
         begin
            case Config.Kinematics.Kinematics_Kind.Kind is
               when Cartesian    =>
                  case Config.Kinematics.Kinematics_Kind.Cartesian (M) is
                     when None   =>
                        null;

                     when X_Axis =>
                        Result.Transform.Linear_Map (X_Axis, M) := Units_Per_Distance;

                     when Y_Axis =>
                        Result.Transform.Linear_Map (Y_Axis, M) := Units_Per_Distance;

                     when Z_Axis =>
                        Result.Transform.Linear_Map (Z_Axis, M) := Units_Per_Distance;

                     when E_Axis =>
                        Result.Transform.Linear_Map (E_Axis, M) := Units_Per_Distance;
                  end case;

               when Core_XY      =>
                  case Config.Kinematics.Kinematics_Kind.Core_XY (M) is
                     when None   =>
                        null;

                     when A_Axis =>
                        Result.Transform.Linear_Map (X_Axis, M) := Units_Per_Distance;
                        Result.Transform.Linear_Map (Y_Axis, M) := Units_Per_Distance;

                     when B_Axis =>
                        Result.Transform.Linear_Map (X_Axis, M) := Units_Per_Distance;
                        Result.Transform.Linear_Map (Y_Axis, M) := -Units_Per_Distance;

                     when Z_Axis =>
                        Result.Transform.Linear_Map (Z_Axis, M) := Units_Per_Distance;

                     when E_Axis =>
                        Result.Transform.Linear_Map (E_Axis, M) := Units_Per_Distance;
                  end case;

               when Linear_Delta =>
                  null;
            end case;
         end;
      end loop;

      if Config.Kinematics.Kinematics_Kind.Kind = Linear_Delta then
         declare
            Delta_Config : User_Config_Kinematics_Delta renames Config.Kinematics.Kinematics_Kind.Linear_Delta;

            function Tower_Config (Tower : Delta_Tower_Name) return User_Config_Kinematics_Delta_Tower
            is (case Tower is
                  when Transforms.Tower_A => Delta_Config.Tower_A,
                  when Transforms.Tower_B => Delta_Config.Tower_B,
                  when Transforms.Tower_C => Delta_Config.Tower_C);

            Delta_Params : Delta_Parameters := (others => <>);
         begin
            for Tower in Delta_Tower_Name loop
               declare
                  Parameters : constant User_Config_Kinematics_Delta_Tower := Tower_Config (Tower);
               begin
                  Delta_Params.Towers (Tower) :=
                    (X          => Delta_Config.Delta_Radius * Angle_Math.Cos (Parameters.Angle),
                     Y          => Delta_Config.Delta_Radius * Angle_Math.Sin (Parameters.Angle),
                     Arm_Length => Parameters.Arm_Length);
               end;
            end loop;

            for Motor in Motor_Name loop
               case Delta_Config.Motors (Motor) is
                  when Tower_A | Tower_B | Tower_C =>
                     Delta_Params.Motors (Motor) :=
                       (Kind               => Delta_Tower_Motor,
                        Tower              =>
                          (case Delta_Config.Motors (Motor) is
                             when Tower_A => Transforms.Tower_A,
                             when Tower_B => Transforms.Tower_B,
                             when Tower_C => Transforms.Tower_C,
                             when others  => raise Program_Error),
                        Units_Per_Distance => 1.0 / Motor_Drivers_Module_Instance.Distance_Per_Unit (Motor));

                  when E_Axis                      =>
                     Delta_Params.Motors (Motor) :=
                       (Kind               => Delta_Extruder_Motor,
                        Units_Per_Distance => 1.0 / Motor_Drivers_Module_Instance.Distance_Per_Unit (Motor));

                  when None                        =>
                     null;
               end case;
            end loop;

            for Motor in Motor_Name loop
               declare
                  Motor_Params : Delta_Motor_Parameters renames Delta_Params.Motors (Motor);
               begin
                  case Motor_Params.Kind is
                     when Unused_Delta_Motor   =>
                        null;

                     when Delta_Extruder_Motor =>
                        Delta_Params.Jacobian_Bounds (Motor, E_Axis) := abs Motor_Params.Units_Per_Distance;

                     when Delta_Tower_Motor    =>
                        declare
                           Params       : Delta_Tower_Parameters renames Delta_Params.Towers (Motor_Params.Tower);
                           Max_DX       : Length;
                           Max_DY       : Length;
                           Max_Radius   : Length;
                           Min_Radicand : Area;
                           Min_Height   : Length;
                        begin
                           Workspace_Tower_Extents
                             (Config.Kinematics.Workspace_Bounds, Params.X, Params.Y, Max_DX, Max_DY, Max_Radius);
                           Min_Radicand := Params.Arm_Length ** 2 - Max_Radius ** 2;
                           if Min_Radicand <= 0.0 * mm ** 2 then
                              Min_Height := Length'Small;
                           else
                              Min_Height := Min_Radicand ** (1 / 2);
                           end if;

                           Delta_Params.Jacobian_Bounds (Motor, X_Axis) :=
                             abs Motor_Params.Units_Per_Distance * Max_DX / Min_Height;
                           Delta_Params.Jacobian_Bounds (Motor, Y_Axis) :=
                             abs Motor_Params.Units_Per_Distance * Max_DY / Min_Height;
                           Delta_Params.Jacobian_Bounds (Motor, Z_Axis) := abs Motor_Params.Units_Per_Distance;
                        end;
                  end case;
               end;
            end loop;

            Result.Transform := (Kind => Delta_Transform, Delta_Params => Delta_Params);
         end;
      end if;

      return Result;
   end Build_Motion_Planner_Configuration;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class is
   begin
      return
        Config.Versioned_Config_Schema'
          (Version => 1, Module_Instance_Tag => Module_Instance'Tag, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
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
            begin
               Motor_Drivers_Module_Instance.Set_Motor_Axis_Map
                 ([for Axis in Axis_Name =>
                     [for Motor in Motor_Name => Motor_Affects_Axis (Planner_Config.Transform, Motor, Axis)]]);
            end;

            case Parsed_Config.Kinematics.Workspace_Bounds.Kind is
               when Rectangular =>
                  if Parsed_Config.Kinematics.Workspace_Bounds.Lower_X
                    > Parsed_Config.Kinematics.Workspace_Bounds.Upper_X
                  then
                     Report_Config_Error
                       (My_Config_Paths.Root.Kinematics.Workspace_Bounds.Lower_X,
                        "The lower X limit must not be greater than the upper X limit.");
                  end if;
                  if Parsed_Config.Kinematics.Workspace_Bounds.Lower_Y
                    > Parsed_Config.Kinematics.Workspace_Bounds.Upper_Y
                  then
                     Report_Config_Error
                       (My_Config_Paths.Root.Kinematics.Workspace_Bounds.Lower_Y,
                        "The lower Y limit must not be greater than the upper Y limit.");
                  end if;

               when Circular    =>
                  if Parsed_Config.Kinematics.Workspace_Bounds.Radius <= 0.0 * mm then
                     Report_Config_Error
                       (My_Config_Paths.Root.Kinematics.Workspace_Bounds.Radius,
                        "The circular XY workspace radius must be greater than zero.");
                  end if;
            end case;

            if Parsed_Config.Kinematics.Workspace_Bounds.Lower_Z > Parsed_Config.Kinematics.Workspace_Bounds.Upper_Z
            then
               Report_Config_Error
                 (My_Config_Paths.Root.Kinematics.Workspace_Bounds.Lower_Z,
                  "The lower Z limit must not be greater than the upper Z limit.");
            end if;
            if Parsed_Config.Kinematics.Workspace_Bounds.Lower_E > Parsed_Config.Kinematics.Workspace_Bounds.Upper_E
            then
               Report_Config_Error
                 (My_Config_Paths.Root.Kinematics.Workspace_Bounds.Lower_E,
                  "The lower E limit must not be greater than the upper E limit.");
            end if;

            case Parsed_Config.Kinematics.Kinematics_Kind.Kind is
               when Cartesian    =>
                  for M in Motor_Name loop
                     if Parsed_Config.Kinematics.Kinematics_Kind.Cartesian (M) /= None
                       and then not Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M)
                     then
                        Report_Config_Error
                          (My_Config_Paths.Root.Kinematics.Kinematics_Kind.Cartesian (M),
                           "This motor is assigned to an axis but the motor is disabled in its motor configuration.");
                     end if;
                  end loop;

               when Core_XY      =>
                  for M in Motor_Name loop
                     if Parsed_Config.Kinematics.Kinematics_Kind.Core_XY (M) /= None
                       and then not Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M)
                     then
                        Report_Config_Error
                          (My_Config_Paths.Root.Kinematics.Kinematics_Kind.Core_XY (M),
                           "This motor is assigned to an axis but the motor is disabled in its motor configuration.");
                     end if;
                  end loop;

               when Linear_Delta =>
                  declare
                     Delta_Config      : User_Config_Kinematics_Delta renames
                       Parsed_Config.Kinematics.Kinematics_Kind.Linear_Delta;
                     Assignment_Counts : array (User_Config_Delta_Axis_Name) of Natural := [others => 0];
                  begin
                     for Motor in Motor_Name loop
                        Assignment_Counts (Delta_Config.Motors (Motor)) := @ + 1;
                        if Delta_Config.Motors (Motor) /= None
                          and then not Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (Motor)
                        then
                           Report_Config_Error
                             (My_Config_Paths.Root.Kinematics.Kinematics_Kind.Linear_Delta.Motors (Motor),
                              "This motor is assigned to delta kinematics but is disabled in its motor "
                              & "configuration.");
                        end if;
                     end loop;

                     for Assignment in User_Config_Delta_Axis_Name range Tower_A .. Tower_C loop
                        if Assignment_Counts (Assignment) = 0 then
                           Report_Config_Error
                             (My_Config_Paths.Root.Kinematics.Kinematics_Kind.Linear_Delta.Motors (Motor_Name'First),
                              "Each of Tower_A, Tower_B, and Tower_C must be assigned to at least one motor.");
                        end if;
                     end loop;

                     declare
                        Delta_Transform_Config : constant Motion_Planner_Configuration :=
                          Build_Motion_Planner_Configuration (Parsed_Config, Motor_Drivers_Module_Instance);
                     begin
                        for Tower in Delta_Tower_Name loop
                           declare
                              Params                     : Delta_Tower_Parameters renames
                                Delta_Transform_Config.Transform.Delta_Params.Towers (Tower);
                              Max_DX, Max_DY, Max_Radius : Length;
                           begin
                              Workspace_Tower_Extents
                                (Parsed_Config.Kinematics.Workspace_Bounds,
                                 Params.X,
                                 Params.Y,
                                 Max_DX,
                                 Max_DY,
                                 Max_Radius);
                              if Max_Radius >= Params.Arm_Length then
                                 case Tower is
                                    when Transforms.Tower_A =>
                                       Report_Config_Error
                                         (My_Config_Paths
                                            .Root
                                            .Kinematics
                                            .Kinematics_Kind
                                            .Linear_Delta
                                            .Tower_A
                                            .Arm_Length,
                                          "The configured XY workspace is not reachable by tower A.");

                                    when Transforms.Tower_B =>
                                       Report_Config_Error
                                         (My_Config_Paths
                                            .Root
                                            .Kinematics
                                            .Kinematics_Kind
                                            .Linear_Delta
                                            .Tower_B
                                            .Arm_Length,
                                          "The configured XY workspace is not reachable by tower B.");

                                    when Transforms.Tower_C =>
                                       Report_Config_Error
                                         (My_Config_Paths
                                            .Root
                                            .Kinematics
                                            .Kinematics_Kind
                                            .Linear_Delta
                                            .Tower_C
                                            .Arm_Length,
                                          "The configured XY workspace is not reachable by tower C.");
                                 end case;
                              end if;
                           end;
                        end loop;
                     end;
                  end;
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
         return Transforms.Axis_Is_Motor_Separable (Get_Default_Motion_Planner_Configuration.Transform, Axis);
      end Axis_Is_Motor_Separable;

      function Motor_Affects_Axis (Motor : Motor_Name; Axis : Axis_Name) return Boolean is
      begin
         return Transforms.Motor_Affects_Axis (Get_Default_Motion_Planner_Configuration.Transform, Motor, Axis);
      end Motor_Affects_Axis;

      function Get_Homing_Configuration return Kinematics_Homing_Configuration is
      begin
         case Config.Kinematics.Kinematics_Kind.Kind is
            when Cartesian | Core_XY =>
               return (Kind => Non_Delta_Kinematics);

            when Linear_Delta        =>
               return
                 (Kind           => Linear_Delta_Kinematics,
                  Tower_Motors   =>
                    [Transforms.Tower_A =>
                       [for Motor in Motor_Name =>
                          Config.Kinematics.Kinematics_Kind.Linear_Delta.Motors (Motor) = Tower_A],
                     Transforms.Tower_B =>
                       [for Motor in Motor_Name =>
                          Config.Kinematics.Kinematics_Kind.Linear_Delta.Motors (Motor) = Tower_B],
                     Transforms.Tower_C =>
                       [for Motor in Motor_Name =>
                          Config.Kinematics.Kinematics_Kind.Linear_Delta.Motors (Motor) = Tower_C]],
                  Planner_Config => Get_Default_Motion_Planner_Configuration);
         end case;
      end Get_Homing_Configuration;

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
