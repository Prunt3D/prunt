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
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   function Cartesian_Path (Motor : Motor_Name) return Config.Config_Data_Paths.Vector
   is (["Kinematics", "Kinematics_Kind", "Kind", "Children", "Cartesian", "Cartesian", +Motor'Image]);

   function Core_XY_Path (Motor : Motor_Name) return Config.Config_Data_Paths.Vector
   is (["Kinematics", "Kinematics_Kind", "Kind", "Children", "Core_XY", "Core_XY", +Motor'Image]);

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
              Axial_Shapers           => Input_Shapers_Module_Instance.Get_Default_Axial_Shapers),
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
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Status_Emitter);

      Parsed_Config                     : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Motor_Drivers_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Motor_Drivers_Module.Module_Instance'Tag);
      Input_Shapers_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Input_Shapers_Module.Module_Instance'Tag);
   begin
      return Result : Module_Instance do
         declare
            Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance_Interface'Class renames
              Motor_Drivers_Module.Module_Instance_Interface'Class (Motor_Drivers_Module_Instance_Ref.Get.Element.all);
         begin
            Result.Initialize
              (Config_In                            => Parsed_Config,
               Motor_Drivers_Module_Instance_Ref_In => Motor_Drivers_Module_Instance_Ref,
               Input_Shapers_Module_Instance_Ref_In => Input_Shapers_Module_Instance_Ref);

            case Parsed_Config.Kinematics.Kinematics_Kind.Kind is
               when Cartesian =>
                  for M in Motor_Name loop
                     if Parsed_Config.Kinematics.Kinematics_Kind.Cartesian (M) /= None
                       and then not Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M)
                     then
                        Report_Config_Error
                          (Cartesian_Path (M), "This motor is assigned to an axis but disabled in Motor Drivers.");
                     end if;
                  end loop;

               when Core_XY   =>
                  for M in Motor_Name loop
                     if Parsed_Config.Kinematics.Kinematics_Kind.Core_XY (M) /= None
                       and then not Motor_Drivers_Module_Instance.Motor_Is_Enabled_In_Config (M)
                     then
                        Report_Config_Error
                          (Core_XY_Path (M), "This motor is assigned to an axis but disabled in Motor Drivers.");
                     end if;
                  end loop;
            end case;
         end;
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize
        (Config_In                            : User_Config;
         Motor_Drivers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Input_Shapers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref) is
      begin
         Config := Config_In;
         Motor_Drivers_Module_Instance_Ref := Motor_Drivers_Module_Instance_Ref_In;
         Input_Shapers_Module_Instance_Ref := Input_Shapers_Module_Instance_Ref_In;
      end Initialize;

      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         Self_Ref := Self_Ref_In;
      end Start;

      procedure Set_Print_And_Travel_Move_Limits
        (Planner : Planner_Interface'Class;
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         T       : Gcode_Optional_Integer;
         F       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, X, Y, Z, E, T, F, S);
         raise Constraint_Error with "M201 is not implemented yet.";
      end Set_Print_And_Travel_Move_Limits;

      procedure Set_Max_Feedrate
        (Planner : Planner_Interface'Class;
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         T       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, X, Y, Z, E, T);
         raise Constraint_Error with "M203 is not implemented yet.";
      end Set_Max_Feedrate;

      procedure Set_Starting_Acceleration
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_Float;
         R       : Gcode_Optional_Float;
         T       : Gcode_Optional_Float;
         S       : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, P, R, T, S);
         raise Constraint_Error with "M204 is not implemented yet.";
      end Set_Starting_Acceleration;

      procedure Set_Advanced_Motion_Settings
        (Planner : Planner_Interface'Class;
         P       : Gcode_Optional_String;
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         B       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Float;
         T       : Gcode_Optional_Float;
         J       : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, P, X, Y, Z, E, B, S, T, J);
         raise Constraint_Error with "M205 is not implemented yet.";
      end Set_Advanced_Motion_Settings;

      function Get_Default_Motion_Planner_Configuration return Motion_Planner_Configuration is
         Motor_Drivers_Module_Instance : Motor_Drivers_Module.Module_Instance_Interface'Class renames
           Motor_Drivers_Module.Module_Instance_Interface'Class (Motor_Drivers_Module_Instance_Ref.Get.Element.all);
         Input_Shapers_Module_Instance : Input_Shapers_Module.Module_Instance_Interface'Class renames
           Input_Shapers_Module.Module_Instance_Interface'Class (Input_Shapers_Module_Instance_Ref.Get.Element.all);
      begin
         return
           Build_Motion_Planner_Configuration
             (Config, Motor_Drivers_Module_Instance, Input_Shapers_Module_Instance);
      end Get_Default_Motion_Planner_Configuration;
   end Module_Instance;

end Prunt.Default_Modules.Kinematics;
