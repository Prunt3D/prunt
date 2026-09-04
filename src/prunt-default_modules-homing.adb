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

with Prunt.Default_Modules.Homing.Config_Paths;
with Prunt.Motion_Planner;

package body Prunt.Default_Modules.Homing is

   pragma Extensions_Allowed (On);

   use type Kinematics_Module.Kinematics_Homing_Kind;
   use type TMC2240_Drivers_Module.Homing_Detector_Kind;
   use Kinematics_Module.Transforms;

   package My_Config_Paths is new Config_Paths;

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   function Required_Loop_Count (Maximum_Travel : Length; Approach_Velocity : Velocity) return Dimensionless
   is (Dimensionless'Max
         (1.0, Dimensionless'Ceiling (Maximum_Travel / (Approach_Velocity * Interpolation_Time) + 2.0)));

   function Loop_Count_For (Maximum_Travel : Length; Approach_Velocity : Velocity) return Loop_Move_Count is
      Count : constant Dimensionless := Required_Loop_Count (Maximum_Travel, Approach_Velocity);
   begin
      if Count > Dimensionless (Loop_Move_Count'Last) then
         raise Constraint_Error with "The configured homing move exceeds the hardware loop-count range.";
      end if;
      return Loop_Move_Count (Count);
   end Loop_Count_For;

   function First_StallGuard_Motor return Motor_Name is
   begin
      for Motor in Motor_Name loop
         if TMC2240_Drivers_Module.Motor_Hardware (Motor).Kind = TMC2240_UART_Kind then
            return Motor;
         end if;
      end loop;

      --  The StallGuard variants are hidden when there are no capable motors, but their schemas still need a valid
      --  value of Motor_Name for the inactive default.
      return Motor_Name'First;
   end First_StallGuard_Motor;

   function Homing_Driver_Parameters_For
     (This : Module_Instance; Axis : Axis_Name; Motor : Motor_Name)
      return TMC2240_Drivers_Module.Homing_Driver_Parameters
   is
      Parameters : constant Axis_Homing_Parameters := This.Get_Homing_Parameters (Axis, Motor);
   begin
      case Parameters.Kind is
         when No_Axis_Homing_Parameters_Kind | Use_Input_Switch_Kind =>
            return (Kind => TMC2240_Drivers_Module.No_Homing_Detector, others => <>);

         when Use_StallGuard2_Kind                                   =>
            return
              (Kind          => TMC2240_Drivers_Module.StallGuard2_Homing_Detector,
               Motor         => Motor,
               Enable_Filter => Parameters.Use_StallGuard2.Enable_Filter,
               SG2_Threshold => Integer (Parameters.Use_StallGuard2.Threshold));

         when Use_StallGuard4_Kind                                   =>
            return
              (Kind          => TMC2240_Drivers_Module.StallGuard4_Homing_Detector,
               Motor         => Motor,
               Enable_Filter => Parameters.Use_StallGuard4.Enable_Filter,
               SG4_Threshold => Natural (Parameters.Use_StallGuard4.Threshold));
      end case;
   end Homing_Driver_Parameters_For;

   procedure Configure_Homing_Driver (This : Module_Instance; Axis : Axis_Name) is
      TMC_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
   begin
      TMC_Instance_Ref := This.Get_TMC2240_Drivers_Module_Instance_Ref;
      for Motor in Motor_Name loop
         declare
            Parameters : constant TMC2240_Drivers_Module.Homing_Driver_Parameters :=
              Homing_Driver_Parameters_For (This, Axis, Motor);
         begin
            if Parameters.Kind /= TMC2240_Drivers_Module.No_Homing_Detector then
               if My_Modules.Module_Instance_Shared_Pointers.Is_Null (TMC_Instance_Ref) then
                  raise Constraint_Error with "TMC2240 homing was requested without an active TMC2240 driver module.";
               end if;
               TMC2240_Drivers_Module.Module_Instance_Interface'Class (TMC_Instance_Ref.Get.Element.all)
                 .Configure_Homing (Parameters);
            end if;
         end;
      end loop;
   end Configure_Homing_Driver;

   procedure Restore_Homing_Driver (This : Module_Instance; Axis : Axis_Name) is
      TMC_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
   begin
      TMC_Instance_Ref := This.Get_TMC2240_Drivers_Module_Instance_Ref;
      for Motor in Motor_Name loop
         declare
            Parameters : constant TMC2240_Drivers_Module.Homing_Driver_Parameters :=
              Homing_Driver_Parameters_For (This, Axis, Motor);
         begin
            if Parameters.Kind /= TMC2240_Drivers_Module.No_Homing_Detector then
               if My_Modules.Module_Instance_Shared_Pointers.Is_Null (TMC_Instance_Ref) then
                  raise Constraint_Error with "TMC2240 homing was requested without an active TMC2240 driver module.";
               end if;
               TMC2240_Drivers_Module.Module_Instance_Interface'Class (TMC_Instance_Ref.Get.Element.all)
                 .Restore_After_Homing (Parameters);
            end if;
         end;
      end loop;
   end Restore_Homing_Driver;

   procedure Restore_All_Homing_Drivers (This : Module_Instance) is
   begin
      for Axis in X_Axis .. Z_Axis loop
         Restore_Homing_Driver (This, Axis);
      end loop;
   end Restore_All_Homing_Drivers;

   function First_User_Visible_Input_Switch return Input_Switch_Name is
   begin
      for S in Input_Switch_Name loop
         if Input_Switches_Module.Input_Switch_Hardware (S).Visible_To_User then
            return S;
         end if;
      end loop;

      raise Constraint_Error with "No user-visible input switches are available.";
   end First_User_Visible_Input_Switch;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class is
   begin
      return
        Config.Versioned_Config_Schema'
          (Version => 1, Module_Instance_Tag => Module_Instance'Tag, Top_Level_Items => Build_Schema);
   end Config_Schema;

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
   procedure Process_After_Block (This : Axis_Start_Event; Context : Block_End_Context'Class) is
      Instance : Module_Instance renames Module_Instance (This.Module_Instance_Ref.Get.Element.all);
   begin
      Context.Wait_For_Idle;
      Configure_Homing_Driver (Instance, This.Axis);
      Instance.Notify_Homing_Axis_Start (This.Axis);
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Axis_Loop_Move_Event; Context : Block_End_Context'Class) is
      pragma Unreferenced (This);
   begin
      Context.Wait_For_Idle;
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Axis_Finish_Event; Context : Block_End_Context'Class) is
      Instance : Module_Instance renames Module_Instance (This.Module_Instance_Ref.Get.Element.all);
   begin
      Context.Wait_For_Idle;
      Instance.Notify_Homing_Axis_Finish (This.Axis);
      Restore_Homing_Driver (Instance, This.Axis);
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Delta_Setup_Event; Context : Block_End_Context'Class) is
      Instance : Module_Instance renames Module_Instance (This.Module_Instance_Ref.Get.Element.all);
   begin
      Context.Wait_For_Idle;
      for Axis in X_Axis .. Z_Axis loop
         Configure_Homing_Driver (Instance, Axis);
         Instance.Notify_Homing_Axis_Start (Axis);
      end loop;
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Delta_Loop_Move_Event; Context : Block_End_Context'Class) is
      pragma Unreferenced (This);
   begin
      Context.Wait_For_Idle;
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Check_Released_Event; Context : Block_End_Context'Class) is
      Instance : Module_Instance renames Module_Instance (This.Module_Instance_Ref.Get.Element.all);
   begin
      Context.Wait_For_Idle;
      for Motor in Motor_Name loop
         if Instance.Motor_Affects_Axis (Motor, This.Axis)
           and then
             Input_Switches_Module.Input_Switch_Hardware (This.Loop_Setup.Stop_Conditions (Motor).Input_Switch)
               .Get_State (This.Loop_Setup.Stop_Conditions (Motor).Input_Switch)
             = This.Loop_Setup.Stop_Conditions (Motor).Stop_State
         then
            raise Constraint_Error with "A homing detector did not release during backoff.";
         end if;
      end loop;
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Check_Motor_Released_Event; Context : Block_End_Context'Class) is
      Condition : Stop_Condition renames This.Loop_Setup.Stop_Conditions (This.Motor);
   begin
      Context.Wait_For_Idle;
      if Input_Switches_Module.Input_Switch_Hardware (Condition.Input_Switch).Get_State (Condition.Input_Switch)
        = Condition.Stop_State
      then
         raise Constraint_Error with "A homing detector did not release during backoff.";
      end if;
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Delta_Clear_Event; Context : Block_End_Context'Class) is
      Instance : Module_Instance renames Module_Instance (This.Module_Instance_Ref.Get.Element.all);
   begin
      Context.Wait_For_Idle;
      for Axis in X_Axis .. Z_Axis loop
         Instance.Notify_Homing_Axis_Finish (Axis);
         Restore_Homing_Driver (Instance, Axis);
      end loop;
   end Process_After_Block;

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

      Parsed_Config                       : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Input_Switches_Module_Instance_Ref  : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Input_Switches_Module.Module_Instance'Tag);
      Kinematics_Module_Instance_Ref      : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Kinematics_Module.Module_Instance'Tag);
      Kinematics_Instance                 : Kinematics_Module.Module_Instance_Interface'Class renames
        Kinematics_Module.Module_Instance_Interface'Class (Kinematics_Module_Instance_Ref.Get.Element.all);
      Kinematics_Config                   : constant Kinematics_Module.Kinematics_Homing_Configuration :=
        Kinematics_Instance.Get_Homing_Configuration;
      TMC2240_Drivers_Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref :=
        My_Modules.Module_Instance_Shared_Pointers.Null_Ref;

      procedure Validate_Loop_Count (Maximum_Travel : Length; Approach_Velocity : Velocity; Path : Config.Config_Path);

      procedure Validate_Loop_Count (Maximum_Travel : Length; Approach_Velocity : Velocity; Path : Config.Config_Path)
      is
      begin
         if Required_Loop_Count (Maximum_Travel, Approach_Velocity) > Dimensionless (Loop_Move_Count'Last) then
            Report_Config_Error
              (Path, "This homing move requires more repeated commands than the hardware loop-count limit permits.");
         end if;
      end Validate_Loop_Count;
   begin
      if (for some Motor in Motor_Name => TMC2240_Drivers_Module.Motor_Hardware (Motor).Kind = TMC2240_UART_Kind) then
         TMC2240_Drivers_Module_Instance_Ref := Get_Other_Instance (TMC2240_Drivers_Module.Module_Instance'Tag);
      end if;

      return Result : Module_Instance do
         Result.Initialize
           (Parsed_Config,
            Input_Switches_Module_Instance_Ref,
            Kinematics_Module_Instance_Ref,
            TMC2240_Drivers_Module_Instance_Ref);

         if Kinematics_Config.Kind = Kinematics_Module.Linear_Delta_Kinematics then
            declare
               Delta_Config            : User_Config_Linear_Delta_Homing renames
                 Parsed_Config.Homing.Linear_Delta_Homing;
               Input_Switches_Instance : Input_Switches_Module.Module_Instance_Interface'Class renames
                 Input_Switches_Module.Module_Instance_Interface'Class
                   (Input_Switches_Module_Instance_Ref.Get.Element.all);
               function Detector (Tower : Delta_Tower_Name) return User_Config_Homing_Detector
               is (case Tower is
                     when Tower_A => Delta_Config.Tower_A,
                     when Tower_B => Delta_Config.Tower_B,
                     when Tower_C => Delta_Config.Tower_C);

               procedure Report_Detector_Error (Tower : Delta_Tower_Name; Message : Virtual_String);

               procedure Report_Detector_Error (Tower : Delta_Tower_Name; Message : Virtual_String) is
               begin
                  case Tower is
                     when Tower_A =>
                        Report_Config_Error (My_Config_Paths.Root.Homing.Linear_Delta_Homing.Tower_A.Kind, Message);

                     when Tower_B =>
                        Report_Config_Error (My_Config_Paths.Root.Homing.Linear_Delta_Homing.Tower_B.Kind, Message);

                     when Tower_C =>
                        Report_Config_Error (My_Config_Paths.Root.Homing.Linear_Delta_Homing.Tower_C.Kind, Message);
                  end case;
               end Report_Detector_Error;

            begin
               for Tower in Delta_Tower_Name loop
                  declare
                     Configured_Detector : constant User_Config_Homing_Detector := Detector (Tower);
                     Switch              : Input_Switch_Name;
                     Has_Detector        : Boolean := True;
                  begin
                     case Configured_Detector.Kind is
                        when Disabled                  =>
                           Has_Detector := False;
                           Switch := Input_Switch_Name'First;
                           Report_Detector_Error (Tower, "A homing detector must be selected for this tower.");

                        when Input_Switch              =>
                           Switch := Configured_Detector.Switch;
                           if not Input_Switches_Module.Input_Switch_Hardware (Switch).Visible_To_User then
                              Report_Detector_Error (Tower, "This input is not exposed as a physical user switch.");
                           end if;

                        when StallGuard2 | StallGuard4 =>
                           declare
                              Detector_Motor : constant Motor_Name :=
                                (if Configured_Detector.Kind = StallGuard2
                                 then Configured_Detector.StallGuard2_Parameters.Motor
                                 else Configured_Detector.StallGuard4_Parameters.Motor);
                           begin
                              if not Kinematics_Config.Tower_Motors (Tower, Detector_Motor) then
                                 Report_Detector_Error (Tower, "The selected StallGuard motor must drive this tower.");
                              end if;
                              if TMC2240_Drivers_Module.Motor_Hardware (Detector_Motor).Kind /= TMC2240_UART_Kind then
                                 Report_Detector_Error (Tower, "The selected motor does not support StallGuard.");
                                 Switch := Input_Switch_Name'First;
                              else
                                 Switch := TMC2240_Drivers_Module.Motor_Hardware (Detector_Motor).TMC2240_Diag_0;
                              end if;
                           end;
                     end case;

                     if Has_Detector then
                        if not Input_Switches_Instance.Switch_Is_Enabled_In_Config (Switch) then
                           Report_Detector_Error (Tower, "This detector input is disabled.");
                        end if;
                     end if;
                  end;
               end loop;

               if Delta_Config.Motion.Fast_Maximum_Travel < Delta_Config.Motion.Maximum_Overtravel then
                  Report_Config_Error
                    (My_Config_Paths.Root.Homing.Linear_Delta_Homing.Motion.Fast_Maximum_Travel,
                     "Fast maximum travel must be at least the maximum overtravel.");
               end if;
               Validate_Loop_Count
                 (Delta_Config.Motion.Fast_Maximum_Travel,
                  Delta_Config.Motion.Fast_Approach_Velocity,
                  My_Config_Paths.Root.Homing.Linear_Delta_Homing.Motion.Fast_Maximum_Travel);
               Validate_Loop_Count
                 (2.0 * Delta_Config.Motion.Backoff_Distance,
                  Delta_Config.Motion.Slow_Approach_Velocity,
                  My_Config_Paths.Root.Homing.Linear_Delta_Homing.Motion.Backoff_Distance);
               if Delta_Config.Motion.Backoff_Distance <= Delta_Config.Motion.Maximum_Overtravel then
                  Report_Config_Error
                    (My_Config_Paths.Root.Homing.Linear_Delta_Homing.Motion.Backoff_Distance,
                     "Backoff distance must be greater than the maximum overtravel.");
               end if;
               if Delta_Config.Move_To_After < Kinematics_Config.Planner_Config.Parameters.Bounds.Lower_Z
                 or else Delta_Config.Move_To_After > Kinematics_Config.Planner_Config.Parameters.Bounds.Upper_Z
               then
                  Report_Config_Error
                    (My_Config_Paths.Root.Homing.Linear_Delta_Homing.Move_To_After,
                     "Post-home Z is outside the configured delta Z position limits.");
               end if;
               if Delta_Config.Home_Z < Kinematics_Config.Planner_Config.Parameters.Bounds.Lower_Z
                 or else Delta_Config.Home_Z > Kinematics_Config.Planner_Config.Parameters.Bounds.Upper_Z
               then
                  Report_Config_Error
                    (My_Config_Paths.Root.Homing.Linear_Delta_Homing.Home_Z,
                     "The delta home Z coordinate is outside the configured delta Z position limits.");
               end if;
               if not Motion_Planner.XY_Position_Is_In_Bounds
                        ([X_Axis => Delta_Config.Endstop_Hit_Offset_X,
                          Y_Axis => Delta_Config.Endstop_Hit_Offset_Y,
                          others => 0.0 * mm],
                         Kinematics_Instance.Get_Default_Motion_Planner_Configuration.Parameters)
               then
                  Report_Config_Error
                    (My_Config_Paths.Root.Homing.Linear_Delta_Homing.Endstop_Hit_Offset_X,
                     "The delta endstop-hit XY offset is outside the configured workspace.");
                  Report_Config_Error
                    (My_Config_Paths.Root.Homing.Linear_Delta_Homing.Endstop_Hit_Offset_Y,
                     "The delta endstop-hit XY offset is outside the configured workspace.");
               end if;
            end;
         else
            for Axis in Axis_Name when Axis /= E_Axis loop
               if Parsed_Config.Homing.Axes (Axis).Homing_Method.Kind = Disabled then
                  Report_Config_Error
                    (My_Config_Paths.Root.Homing.Axes (Axis).Homing_Method.Kind,
                     "Homing is not configured for this axis.");
               elsif Parsed_Config.Homing.Axes (Axis).Homing_Method.Kind = Home_To_Detector then
                  declare
                     Method                         : User_Config_Homing_To_Detector renames
                       Parsed_Config.Homing.Axes (Axis).Homing_Method.Home_To_Detector;
                     Input_Switches_Module_Instance : Input_Switches_Module.Module_Instance_Interface'Class renames
                       Input_Switches_Module.Module_Instance_Interface'Class
                         (Input_Switches_Module_Instance_Ref.Get.Element.all);
                     Has_Participating_Motor        : constant Boolean :=
                       (for some Motor in Motor_Name => Kinematics_Instance.Motor_Affects_Axis (Motor, Axis));

                     procedure Validate_Switch (Switch : Input_Switch_Name);

                     procedure Validate_Switch (Switch : Input_Switch_Name) is
                     begin
                        if not Input_Switches_Module_Instance.Switch_Is_Enabled_In_Config (Switch) then
                           Report_Config_Error
                             (My_Config_Paths.Root.Homing.Axes (Axis).Homing_Method.Home_To_Detector.Detector.Kind,
                              "This detector input is disabled.");
                        end if;
                     end Validate_Switch;
                  begin
                     if Parsed_Config.Homing.Axes (Axis).Motion.Fast_Maximum_Travel
                       < Parsed_Config.Homing.Axes (Axis).Motion.Maximum_Overtravel
                     then
                        Report_Config_Error
                          (My_Config_Paths.Root.Homing.Axes (Axis).Motion.Fast_Maximum_Travel,
                           "Fast maximum travel must be at least the maximum overtravel.");
                     end if;
                     Validate_Loop_Count
                       (Parsed_Config.Homing.Axes (Axis).Motion.Fast_Maximum_Travel,
                        Parsed_Config.Homing.Axes (Axis).Motion.Fast_Approach_Velocity,
                        My_Config_Paths.Root.Homing.Axes (Axis).Motion.Fast_Maximum_Travel);
                     Validate_Loop_Count
                       (2.0 * Parsed_Config.Homing.Axes (Axis).Motion.Backoff_Distance,
                        Parsed_Config.Homing.Axes (Axis).Motion.Slow_Approach_Velocity,
                        My_Config_Paths.Root.Homing.Axes (Axis).Motion.Backoff_Distance);
                     if 2.0 * Parsed_Config.Homing.Axes (Axis).Motion.Backoff_Distance
                       < Parsed_Config.Homing.Axes (Axis).Motion.Maximum_Overtravel
                     then
                        Report_Config_Error
                          (My_Config_Paths.Root.Homing.Axes (Axis).Motion.Backoff_Distance,
                           "Twice the backoff distance must be at least the maximum overtravel.");
                     end if;

                     case Method.Detector.Kind is
                        when Disabled                  =>
                           Report_Config_Error
                             (My_Config_Paths.Root.Homing.Axes (Axis).Homing_Method.Home_To_Detector.Detector.Kind,
                              "A homing detector must be selected.");

                        when Input_Switch              =>
                           if not Input_Switches_Module.Input_Switch_Hardware (Method.Detector.Switch).Visible_To_User
                           then
                              Report_Config_Error
                                (My_Config_Paths.Root.Homing.Axes (Axis).Homing_Method.Home_To_Detector.Detector.Kind,
                                 "This input is not exposed as a physical user switch.");
                           end if;
                           Validate_Switch (Method.Detector.Switch);

                        when StallGuard2 | StallGuard4 =>
                           declare
                              Motor : constant Motor_Name :=
                                (if Method.Detector.Kind = StallGuard2
                                 then Method.Detector.StallGuard2_Parameters.Motor
                                 else Method.Detector.StallGuard4_Parameters.Motor);
                           begin
                              if not Kinematics_Instance.Motor_Affects_Axis (Motor, Axis) then
                                 Report_Config_Error
                                   (My_Config_Paths.Root.Homing.Axes (Axis)
                                      .Homing_Method
                                      .Home_To_Detector
                                      .Detector
                                      .Kind,
                                    "The selected StallGuard motor does not drive this axis.");
                              end if;
                              if TMC2240_Drivers_Module.Motor_Hardware (Motor).Kind /= TMC2240_UART_Kind then
                                 Report_Config_Error
                                   (My_Config_Paths.Root.Homing.Axes (Axis)
                                      .Homing_Method
                                      .Home_To_Detector
                                      .Detector
                                      .Kind,
                                    "The selected motor does not support StallGuard.");
                              else
                                 Validate_Switch (TMC2240_Drivers_Module.Motor_Hardware (Motor).TMC2240_Diag_0);
                              end if;
                           end;
                     end case;

                     if Method.Detector.Kind /= Disabled and then not Has_Participating_Motor then
                        Report_Config_Error
                          (My_Config_Paths.Root.Homing.Axes (Axis).Homing_Method.Home_To_Detector.Detector.Kind,
                           "The homed axis does not affect any motor.");
                     end if;
                  end;
               end if;
            end loop;

            declare
               type Axis_Homing_State is (Unvisited, Visiting, Done);

               Axis_States : array (Axis_Name) of Axis_Homing_State := [others => Unvisited];

               procedure Check_Axis (Axis : Axis_Name);

               procedure Check_Axis (Axis : Axis_Name) is
               begin
                  if Axis_States (Axis) /= Unvisited then
                     return;
                  end if;

                  Axis_States (Axis) := Visiting;

                  for Prereq_Axis in Axis_Name when Axis /= Prereq_Axis and then Prereq_Axis /= E_Axis loop
                     --  Direct self-references are prevented by the schema.

                     if Parsed_Config.Homing.Axes (Axis).Prerequisites (Prereq_Axis).Kind /= No_Requirement then
                        case Axis_States (Prereq_Axis) is
                           when Unvisited =>
                              Check_Axis (Prereq_Axis);

                           when Visiting  =>
                              --  TODO: We should show the cycle in the log.
                              Report_Config_Error
                                (My_Config_Paths.Root.Homing.Axes (Axis).Prerequisites (Prereq_Axis).Kind,
                                 "This prerequisite introduces a cycle in the homing prerequisites.");

                           when Done      =>
                              null;
                        end case;
                     end if;
                  end loop;

                  Axis_States (Axis) := Done;
               end Check_Axis;
            begin
               for Axis in Axis_Name when Axis /= E_Axis loop
                  Axis_States := [others => Unvisited];
                  Check_Axis (Axis);
               end loop;
            end;
         end if;
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize
        (Config_In                              : User_Config;
         Input_Switches_Module_Instance_Ref_In  : My_Modules.Module_Instance_Shared_Pointers.Ref;
         Kinematics_Module_Instance_Ref_In      : My_Modules.Module_Instance_Shared_Pointers.Ref;
         TMC2240_Drivers_Module_Instance_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Ref) is
      begin
         Config := Config_In;
         Input_Switches_Module_Instance_Ref := Input_Switches_Module_Instance_Ref_In;
         Kinematics_Module_Instance_Ref := Kinematics_Module_Instance_Ref_In;
         TMC2240_Drivers_Module_Instance_Ref := TMC2240_Drivers_Module_Instance_Ref_In;
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Self_Ref_In);
      begin
         --  The extruder coordinate is relative to an arbitrary origin and has no physical homing procedure.
         Planner.Mark_Axis_Homed (E_Axis);
      end Start;

      procedure Notify_Homing_Axis_Start (Axis : Axis_Name) is
      begin
         for Subscriber of Subscribers loop
            Homing_Event_Subscriber'Class (Subscriber.Get.Element.all).On_Homing_Axis_Start (Axis);
         end loop;
      end Notify_Homing_Axis_Start;

      procedure Notify_Homing_Axis_Finish (Axis : Axis_Name) is
      begin
         for Subscriber of Subscribers loop
            Homing_Event_Subscriber'Class (Subscriber.Get.Element.all).On_Homing_Axis_Finish (Axis);
         end loop;
      end Notify_Homing_Axis_Finish;

      procedure Subscribe_To_Homing (Subscriber : not null access function return Homing_Event_Subscriber'Class) is
         Subscriber_Ref : Homing_Event_Subscriber_Shared_Pointers.Ref;
      begin
         Subscriber_Ref.Set (Subscriber);
         Subscribers.Append (Subscriber_Ref);
      end Subscribe_To_Homing;

      function Get_Homing_Parameters (Axis : Axis_Name; Motor : Motor_Name) return Axis_Homing_Parameters is
         Detector : User_Config_Homing_Detector;
      begin
         if Axis = E_Axis then
            return (Kind => No_Axis_Homing_Parameters_Kind);
         end if;

         if Get_Kinematics_Homing_Configuration.Kind = Kinematics_Module.Linear_Delta_Kinematics then
            declare
               Tower             : constant Delta_Tower_Name :=
                 (case Axis is
                    when X_Axis => Tower_A,
                    when Y_Axis => Tower_B,
                    when Z_Axis => Tower_C,
                    when E_Axis => raise Program_Error);
               Kinematics_Config : constant Kinematics_Module.Kinematics_Homing_Configuration :=
                 Get_Kinematics_Homing_Configuration;
            begin
               if not Kinematics_Config.Tower_Motors (Tower, Motor) then
                  return (Kind => No_Axis_Homing_Parameters_Kind);
               end if;

               Detector :=
                 (case Tower is
                    when Tower_A => Config.Homing.Linear_Delta_Homing.Tower_A,
                    when Tower_B => Config.Homing.Linear_Delta_Homing.Tower_B,
                    when Tower_C => Config.Homing.Linear_Delta_Homing.Tower_C);
            end;
         else
            if not Motor_Affects_Axis (Motor, Axis)
              or else Config.Homing.Axes (Axis).Homing_Method.Kind /= Home_To_Detector
            then
               return (Kind => No_Axis_Homing_Parameters_Kind);
            end if;
            Detector := Config.Homing.Axes (Axis).Homing_Method.Home_To_Detector.Detector;
         end if;

         case Detector.Kind is
            when Disabled     =>
               return (Kind => No_Axis_Homing_Parameters_Kind);

            when Input_Switch =>
               return (Kind => Use_Input_Switch_Kind, Switch => Detector.Switch);

            when StallGuard2  =>
               if Motor /= Detector.StallGuard2_Parameters.Motor then
                  return (Kind => No_Axis_Homing_Parameters_Kind);
               end if;
               return
                 (Kind            => Use_StallGuard2_Kind,
                  Use_StallGuard2 =>
                    (Motor         => Detector.StallGuard2_Parameters.Motor,
                     Threshold     => Detector.StallGuard2_Parameters.Threshold,
                     Enable_Filter => Detector.StallGuard2_Parameters.Enable_Filter));

            when StallGuard4  =>
               if Motor /= Detector.StallGuard4_Parameters.Motor then
                  return (Kind => No_Axis_Homing_Parameters_Kind);
               end if;
               return
                 (Kind            => Use_StallGuard4_Kind,
                  Use_StallGuard4 =>
                    (Motor         => Detector.StallGuard4_Parameters.Motor,
                     Threshold     => Detector.StallGuard4_Parameters.Threshold,
                     Enable_Filter => Detector.StallGuard4_Parameters.Enable_Filter));
         end case;
      end Get_Homing_Parameters;

      function Get_Config return User_Config is
      begin
         return Config;
      end Get_Config;

      function Switch_Is_Normally_Closed (Switch : Input_Switch_Name) return Boolean is
         Input_Switches_Instance : Input_Switches_Module.Module_Instance_Interface'Class renames
           Input_Switches_Module.Module_Instance_Interface'Class (Input_Switches_Module_Instance_Ref.Get.Element.all);
      begin
         return Input_Switches_Instance.Switch_Is_Normally_Closed (Switch);
      end Switch_Is_Normally_Closed;

      function Switch_Is_Enabled (Switch : Input_Switch_Name) return Boolean is
         Input_Switches_Instance : Input_Switches_Module.Module_Instance_Interface'Class renames
           Input_Switches_Module.Module_Instance_Interface'Class (Input_Switches_Module_Instance_Ref.Get.Element.all);
      begin
         return Input_Switches_Instance.Switch_Is_Enabled_In_Config (Switch);
      end Switch_Is_Enabled;

      function Motor_Affects_Axis (Motor : Motor_Name; Axis : Axis_Name) return Boolean is
         Kinematics_Instance : Kinematics_Module.Module_Instance_Interface'Class renames
           Kinematics_Module.Module_Instance_Interface'Class (Kinematics_Module_Instance_Ref.Get.Element.all);
      begin
         return Kinematics_Instance.Motor_Affects_Axis (Motor, Axis);
      end Motor_Affects_Axis;

      function Get_Kinematics_Homing_Configuration return Kinematics_Module.Kinematics_Homing_Configuration is
         Kinematics_Instance : Kinematics_Module.Module_Instance_Interface'Class renames
           Kinematics_Module.Module_Instance_Interface'Class (Kinematics_Module_Instance_Ref.Get.Element.all);
      begin
         return Kinematics_Instance.Get_Homing_Configuration;
      end Get_Kinematics_Homing_Configuration;

      function Get_TMC2240_Drivers_Module_Instance_Ref return My_Modules.Module_Instance_Shared_Pointers.Ref is
      begin
         return TMC2240_Drivers_Module_Instance_Ref;
      end Get_TMC2240_Drivers_Module_Instance_Ref;

   end Module_Instance;

   procedure Auto_Home
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_No_Value;
      Y        : Gcode_Optional_No_Value;
      Z        : Gcode_Optional_No_Value)
   is
      Config            : constant User_Config := This.Get_Config;
      Kinematics_Config : constant Kinematics_Module.Kinematics_Homing_Configuration :=
        This.Get_Kinematics_Homing_Configuration;
      Loop_Move_Planner : My_Modules.Loop_Move_Planner_Interface'Class renames
        My_Modules.Loop_Move_Planner_Interface'Class (Planner);

      type Axis_Homing_State is (Pending, Visiting, Done);

      Requested_Axes : constant array (Axis_Name) of Boolean :=
        (if not (X.Present or else Y.Present or else Z.Present)
         then [X_Axis | Y_Axis | Z_Axis => True, E_Axis => False]
         else [X_Axis => X.Present, Y_Axis => Y.Present, Z_Axis => Z.Present, E_Axis => False]);

      Axis_States : array (Axis_Name) of Axis_Homing_State :=
        [for A in Axis_Name => (if Requested_Axes (A) or else not Planner.Axis_Is_Homed (A) then Pending else Done)];

      function Directed_Length (Move_Towards_Negative_Infinity : Boolean; Distance : Length) return Length
      is (if Move_Towards_Negative_Infinity then -Distance else Distance);

      function Flush_Configured_Loop_Move
        (Setup : Loop_Move_Setup; Extra_Data : Extra_Block_Resetting_Data'Class) return Position_Offset;

      function Flush_Configured_Motor_Loop_Move
        (Motor : Motor_Name; Setup : Loop_Move_Setup; Extra_Data : Extra_Block_Resetting_Data'Class)
         return Position_Offset;

      procedure Flush_And_Resolve_Homing_Move
        (Detector_Hit_Position : Position; Setup : Loop_Move_Setup; Extra_Data : Extra_Block_Resetting_Data'Class);

      function Flush_Configured_Loop_Move
        (Setup : Loop_Move_Setup; Extra_Data : Extra_Block_Resetting_Data'Class) return Position_Offset
      is (Loop_Move_Planner.Flush_Loop_Move
            (Stop_Conditions    => Setup.Stop_Conditions,
             Maximum_Loop_Count => Setup.Maximum_Loop_Count,
             Extra_Data         => Extra_Data));

      function Flush_Configured_Motor_Loop_Move
        (Motor : Motor_Name; Setup : Loop_Move_Setup; Extra_Data : Extra_Block_Resetting_Data'Class)
         return Position_Offset
      is (Loop_Move_Planner.Flush_Motor_Loop_Move
            (Motor              => Motor,
             Stop_Condition     => Setup.Stop_Conditions (Motor),
             Maximum_Loop_Count => Setup.Maximum_Loop_Count,
             Extra_Data         => Extra_Data));

      procedure Flush_And_Resolve_Homing_Move
        (Detector_Hit_Position : Position; Setup : Loop_Move_Setup; Extra_Data : Extra_Block_Resetting_Data'Class)
      is
         Tail_Offset : constant Position_Offset := Flush_Configured_Loop_Move (Setup, Extra_Data);
      begin
         Planner.Resolve_Homing_Move (Detector_Hit_Position + Tail_Offset);
      end Flush_And_Resolve_Homing_Move;

      procedure Home_Axis (Axis : Axis_Name);
      procedure Home_Delta_Group;

      procedure Home_Axis (Axis : Axis_Name) is
         Axis_Config : constant User_Config_Axis_Homing := Config.Homing.Axes (Axis);

         function Setup_For
           (Detector : User_Config_Homing_Detector; Maximum_Travel : Length; Approach_Velocity : Velocity)
            return Loop_Move_Setup;

         function Setup_For
           (Detector : User_Config_Homing_Detector; Maximum_Travel : Length; Approach_Velocity : Velocity)
            return Loop_Move_Setup
         is
            Maximum_Cycles : constant Loop_Move_Count := Loop_Count_For (Maximum_Travel, Approach_Velocity);
            Result         : Loop_Move_Setup :=
              (Stop_Conditions => [others => <>], Maximum_Loop_Count => Maximum_Cycles);

            procedure Add_Stop_Condition (Switch : Input_Switch_Name);

            procedure Add_Stop_Condition (Switch : Input_Switch_Name) is
            begin
               if not This.Switch_Is_Enabled (Switch) then
                  raise Constraint_Error with "A homing detector input is disabled.";
               end if;

               for Motor in Motor_Name loop
                  if This.Motor_Affects_Axis (Motor, Axis) then
                     Result.Stop_Conditions (Motor) :=
                       (Input_Switch => Switch, Stop_State => not This.Switch_Is_Normally_Closed (Switch));
                  end if;
               end loop;
            end Add_Stop_Condition;
         begin
            case Detector.Kind is
               when Disabled                  =>
                  raise Constraint_Error with "Detector-based homing requires a configured detector.";

               when Input_Switch              =>
                  Add_Stop_Condition (Detector.Switch);

               when StallGuard2 | StallGuard4 =>
                  declare
                     Motor : constant Motor_Name :=
                       (if Detector.Kind = StallGuard2
                        then Detector.StallGuard2_Parameters.Motor
                        else Detector.StallGuard4_Parameters.Motor);
                  begin
                     if not This.Motor_Affects_Axis (Motor, Axis) then
                        raise Constraint_Error with "The selected StallGuard motor does not drive this axis.";
                     end if;
                     if TMC2240_Drivers_Module.Motor_Hardware (Motor).Kind /= TMC2240_UART_Kind then
                        raise Constraint_Error with "The selected motor does not support StallGuard.";
                     end if;
                     Add_Stop_Condition (TMC2240_Drivers_Module.Motor_Hardware (Motor).TMC2240_Diag_0);
                  end;
            end case;

            if not (for some Motor in Motor_Name => This.Motor_Affects_Axis (Motor, Axis)) then
               raise Constraint_Error with "A homed axis must affect at least one motor.";
            end if;
            return Result;
         end Setup_For;
      begin
         case Axis_States (Axis) is
            when Done     =>
               return;

            when Visiting =>
               raise Program_Error with "Homing cycle should have been caught earlier.";

            when Pending  =>
               null;
         end case;

         Axis_States (Axis) := Visiting;

         for Prereq_Axis in Axis_Name when Prereq_Axis /= E_Axis loop
            declare
               Prereq : User_Config_Homing_Prereq renames Axis_Config.Prerequisites (Prereq_Axis);
            begin
               case Prereq.Kind is
                  when No_Requirement      =>
                     null;

                  when Must_Be_Homed       =>
                     Home_Axis (Prereq_Axis);

                  when Must_Be_At_Position =>
                     Home_Axis (Prereq_Axis);

                     Planner.Add_Corner
                       ([Planner.Get_Last_Position with delta Prereq_Axis => Prereq.Must_Be_At_Position.Position],
                        Feedrate => Config.Homing.Axes (Prereq_Axis).Motion.Slow_Approach_Velocity);
                     Planner.Flush;
               end case;
            end;
         end loop;

         Planner.Mark_Axis_Unhomed (Axis);

         case Axis_Config.Homing_Method.Kind is
            when Set_To_Value     =>
               Planner.Flush_And_Reset_Position
                 (New_Position =>
                    [Planner.Get_Last_Position with delta Axis => Axis_Config.Homing_Method.Set_To_Value.Position],
                  Extra_Data   => Axis_Finish_Event'(Module_Instance_Ref => Self_Ref, Axis => Axis));

            when Home_To_Detector =>
               declare
                  Method               : User_Config_Homing_To_Detector renames
                    Axis_Config.Homing_Method.Home_To_Detector;
                  Motion               : User_Config_Homing_Motion renames Axis_Config.Motion;
                  Fast_Setup           : constant Loop_Move_Setup :=
                    Setup_For (Method.Detector, Motion.Fast_Maximum_Travel, Motion.Fast_Approach_Velocity);
                  Slow_Setup           : constant Loop_Move_Setup :=
                    Setup_For (Method.Detector, 2.0 * Motion.Backoff_Distance, Motion.Slow_Approach_Velocity);
                  Home_Position        : constant Position :=
                    [Planner.Get_Last_Position with delta Axis => Method.Home_Position];
                  Initial_Position     : constant Position := [Home_Position with delta Axis => 0.0 * mm];
                  Any_Initially_Active : Boolean := False;
               begin
                  for Motor in Motor_Name loop
                     if This.Motor_Affects_Axis (Motor, Axis)
                       and then
                         Input_Switches_Module.Input_Switch_Hardware (Fast_Setup.Stop_Conditions (Motor).Input_Switch)
                           .Get_State (Fast_Setup.Stop_Conditions (Motor).Input_Switch)
                         = Fast_Setup.Stop_Conditions (Motor).Stop_State
                     then
                        Any_Initially_Active := True;
                     end if;
                  end loop;

                  Planner.Flush_And_Reset_Position
                    (New_Position => Initial_Position,
                     Extra_Data   => Axis_Start_Event'(Module_Instance_Ref => Self_Ref, Axis => Axis));

                  if Any_Initially_Active then
                     Planner.Add_Corner
                       ([Initial_Position with delta
                           Axis => Directed_Length (Method.Move_Towards_Negative_Infinity, -Motion.Backoff_Distance)],
                        Feedrate      => Motion.Slow_Approach_Velocity,
                        Require_Homed => False);
                     Planner.Flush
                       (Check_Released_Event'
                          (Module_Instance_Ref => Self_Ref, Axis => Axis, Loop_Setup => Fast_Setup));
                  end if;

                  Planner.Add_Corner
                    ([Planner.Get_Last_Position with delta
                        Axis =>
                          Planner.Get_Last_Position (Axis)
                          + Directed_Length (Method.Move_Towards_Negative_Infinity, Motion.Maximum_Overtravel)],
                     Feedrate      => Motion.Fast_Approach_Velocity,
                     Require_Homed => False);

                  Flush_And_Resolve_Homing_Move (Home_Position, Fast_Setup, Axis_Loop_Move_Event'(null record));

                  Planner.Add_Corner
                    ([Home_Position with delta
                        Axis =>
                          Method.Home_Position
                          + Directed_Length (Method.Move_Towards_Negative_Infinity, -Motion.Backoff_Distance)],
                     Feedrate      => Motion.Slow_Approach_Velocity,
                     Require_Homed => False);
                  Planner.Flush
                    (Check_Released_Event'(Module_Instance_Ref => Self_Ref, Axis => Axis, Loop_Setup => Fast_Setup));

                  Planner.Add_Corner
                    ([Planner.Get_Last_Position with delta
                        Axis =>
                          Planner.Get_Last_Position (Axis)
                          + Directed_Length (Method.Move_Towards_Negative_Infinity, Motion.Maximum_Overtravel)],
                     Feedrate      => Motion.Slow_Approach_Velocity,
                     Require_Homed => False);

                  Flush_And_Resolve_Homing_Move
                    (Home_Position, Slow_Setup, Axis_Finish_Event'(Module_Instance_Ref => Self_Ref, Axis => Axis));
               end;

            when Disabled         =>
               raise Program_Error with "Disabled homing method should have been caught earlier.";
         end case;

         Planner.Mark_Axis_Homed (Axis);

         if Axis_Config.Homing_Method.Kind = Home_To_Detector
           or else Planner.Get_Last_Position (Axis) /= Axis_Config.Move_To_After
         then
            Planner.Add_Corner
              ([Planner.Get_Last_Position with delta Axis => Axis_Config.Move_To_After],
               Feedrate => Axis_Config.Motion.Slow_Approach_Velocity);
            Planner.Flush;
         end if;

         Axis_States (Axis) := Done;
      end Home_Axis;

      procedure Home_Delta_Group is
         Delta_Config : User_Config_Linear_Delta_Homing renames Config.Homing.Linear_Delta_Homing;

         function Setup_For (Maximum_Travel : Length; Approach_Velocity : Velocity) return Loop_Move_Setup;

         function Detector (Tower : Delta_Tower_Name) return User_Config_Homing_Detector
         is (case Tower is
               when Tower_A => Delta_Config.Tower_A,
               when Tower_B => Delta_Config.Tower_B,
               when Tower_C => Delta_Config.Tower_C);

         function Detector_Switch (Tower : Delta_Tower_Name) return Input_Switch_Name;
         function Switch_Is_Shared (Tower : Delta_Tower_Name) return Boolean;
         function Representative_Motor (Tower : Delta_Tower_Name) return Motor_Name;

         function Representative_Motor (Tower : Delta_Tower_Name) return Motor_Name is
         begin
            for Motor in Motor_Name loop
               if Kinematics_Config.Tower_Motors (Tower, Motor) then
                  return Motor;
               end if;
            end loop;
            raise Constraint_Error with "Delta homing requires at least one motor for every tower.";
         end Representative_Motor;

         function Detector_Switch (Tower : Delta_Tower_Name) return Input_Switch_Name is
            Configured_Detector : constant User_Config_Homing_Detector := Detector (Tower);
         begin
            case Configured_Detector.Kind is
               when Disabled                  =>
                  raise Constraint_Error with "Delta homing requires a detector for every tower.";

               when Input_Switch              =>
                  return Configured_Detector.Switch;

               when StallGuard2 | StallGuard4 =>
                  declare
                     Detector_Motor : constant Motor_Name :=
                       (if Configured_Detector.Kind = StallGuard2
                        then Configured_Detector.StallGuard2_Parameters.Motor
                        else Configured_Detector.StallGuard4_Parameters.Motor);
                  begin
                     if not Kinematics_Config.Tower_Motors (Tower, Detector_Motor) then
                        raise Constraint_Error with "The selected StallGuard motor does not drive this tower.";
                     end if;
                     if TMC2240_Drivers_Module.Motor_Hardware (Detector_Motor).Kind /= TMC2240_UART_Kind then
                        raise Constraint_Error with "The selected motor does not support StallGuard.";
                     end if;
                     return TMC2240_Drivers_Module.Motor_Hardware (Detector_Motor).TMC2240_Diag_0;
                  end;
            end case;
         end Detector_Switch;

         function Switch_Is_Shared (Tower : Delta_Tower_Name) return Boolean is
            Switch : constant Input_Switch_Name := Detector_Switch (Tower);
         begin
            return
              (for some Other_Tower in Delta_Tower_Name =>
                 Other_Tower /= Tower and then Detector_Switch (Other_Tower) = Switch);
         end Switch_Is_Shared;

         function Setup_For (Maximum_Travel : Length; Approach_Velocity : Velocity) return Loop_Move_Setup is
            Maximum_Cycles : constant Loop_Move_Count := Loop_Count_For (Maximum_Travel, Approach_Velocity);
            Result         : Loop_Move_Setup :=
              (Stop_Conditions => [others => <>], Maximum_Loop_Count => Maximum_Cycles);
         begin
            for Tower in Delta_Tower_Name loop
               declare
                  Switch : constant Input_Switch_Name := Detector_Switch (Tower);
               begin
                  if not This.Switch_Is_Enabled (Switch) then
                     raise Constraint_Error with "A delta homing detector input is disabled.";
                  end if;

                  for Motor in Motor_Name loop
                     if Kinematics_Config.Tower_Motors (Tower, Motor) then
                        Result.Stop_Conditions (Motor) :=
                          (Input_Switch => Switch, Stop_State => not This.Switch_Is_Normally_Closed (Switch));
                     end if;
                  end loop;
               end;
            end loop;
            return Result;
         end Setup_For;

         Fast_Setup           : constant Loop_Move_Setup :=
           Setup_For (Delta_Config.Motion.Fast_Maximum_Travel, Delta_Config.Motion.Fast_Approach_Velocity);
         Slow_Maximum_Travel  : constant Length := 2.0 * Delta_Config.Motion.Backoff_Distance;
         Slow_Setup           : constant Loop_Move_Setup :=
           Setup_For (Slow_Maximum_Travel, Delta_Config.Motion.Slow_Approach_Velocity);
         Home_Z               : constant Length := Delta_Config.Home_Z;
         Home_Position        : constant Position :=
           [Planner.Get_Last_Position with delta
              X_Axis => Delta_Config.Endstop_Hit_Offset_X,
              Y_Axis => Delta_Config.Endstop_Hit_Offset_Y,
              Z_Axis => Home_Z];
         Initial_Position     : constant Position := [Home_Position with delta Z_Axis => 0.0 * mm];
         Any_Initially_Active : Boolean := False;

         procedure Back_Off_And_Check_Released (Setup : Loop_Move_Setup);
         procedure Back_Off_Tower_And_Check_Released (Tower : Delta_Tower_Name; Setup : Loop_Move_Setup);
         procedure Perform_Pass (Setup : Loop_Move_Setup; Approach_Velocity : Velocity);
         procedure Perform_Motor_Pass
           (Tower : Delta_Tower_Name; Setup : Loop_Move_Setup; Approach_Velocity : Velocity);

         procedure Back_Off_And_Check_Released (Setup : Loop_Move_Setup) is
         begin
            Planner.Add_Corner
              ([Planner.Get_Last_Position with delta
                  Z_Axis => Planner.Get_Last_Position (Z_Axis) - Delta_Config.Motion.Backoff_Distance],
               Feedrate      => Delta_Config.Motion.Slow_Approach_Velocity,
               Require_Homed => False);
            Planner.Flush
              (Check_Released_Event'(Module_Instance_Ref => Self_Ref, Axis => Z_Axis, Loop_Setup => Setup));
         end Back_Off_And_Check_Released;

         procedure Back_Off_Tower_And_Check_Released (Tower : Delta_Tower_Name; Setup : Loop_Move_Setup) is
         begin
            Planner.Add_Corner
              ([Planner.Get_Last_Position with delta
                  Z_Axis => Planner.Get_Last_Position (Z_Axis) - Delta_Config.Motion.Backoff_Distance],
               Feedrate      => Delta_Config.Motion.Slow_Approach_Velocity,
               Require_Homed => False);
            Loop_Move_Planner.Flush_Motor_Move
              (Motor      => Representative_Motor (Tower),
               Extra_Data =>
                 Check_Motor_Released_Event'
                   (Module_Instance_Ref => Self_Ref, Loop_Setup => Setup, Motor => Representative_Motor (Tower)));
         end Back_Off_Tower_And_Check_Released;

         procedure Perform_Pass (Setup : Loop_Move_Setup; Approach_Velocity : Velocity) is
            Tail_Offset : Position_Offset;
         begin
            Planner.Add_Corner
              ([Planner.Get_Last_Position with delta
                  Z_Axis => Planner.Get_Last_Position (Z_Axis) + Delta_Config.Motion.Maximum_Overtravel],
               Feedrate      => Approach_Velocity,
               Require_Homed => False);
            Tail_Offset := Flush_Configured_Loop_Move (Setup, Delta_Loop_Move_Event'(null record));
            Planner.Resolve_Homing_Move (Home_Position + Tail_Offset);
         end Perform_Pass;

         procedure Perform_Motor_Pass (Tower : Delta_Tower_Name; Setup : Loop_Move_Setup; Approach_Velocity : Velocity)
         is
            Motor       : constant Motor_Name := Representative_Motor (Tower);
            Tail_Offset : Position_Offset;
         begin
            Planner.Add_Corner
              ([Planner.Get_Last_Position with delta
                  Z_Axis => Planner.Get_Last_Position (Z_Axis) + Delta_Config.Motion.Maximum_Overtravel],
               Feedrate      => Approach_Velocity,
               Require_Homed => False);
            Tail_Offset := Flush_Configured_Motor_Loop_Move (Motor, Setup, Delta_Loop_Move_Event'(null record));
            Planner.Resolve_Homing_Move (Home_Position + Tail_Offset);
         end Perform_Motor_Pass;

      begin
         if Delta_Config.Motion.Fast_Maximum_Travel < Delta_Config.Motion.Maximum_Overtravel
           or else Delta_Config.Motion.Backoff_Distance <= Delta_Config.Motion.Maximum_Overtravel
         then
            raise Constraint_Error with "Delta homing travel must cover the configured maximum overtravel.";
         end if;

         for Axis in X_Axis .. Z_Axis loop
            Planner.Mark_Axis_Unhomed (Axis);
         end loop;

         for Tower in Delta_Tower_Name loop
            declare
               Condition : Stop_Condition renames Fast_Setup.Stop_Conditions (Representative_Motor (Tower));
            begin
               if Input_Switches_Module.Input_Switch_Hardware (Condition.Input_Switch).Get_State
                    (Condition.Input_Switch)
                 = Condition.Stop_State
               then
                  Any_Initially_Active := True;
               end if;
            end;
         end loop;

         Planner.Flush_And_Reset_Position (Initial_Position, Delta_Setup_Event'(Module_Instance_Ref => Self_Ref));

         if Any_Initially_Active then
            Back_Off_And_Check_Released (Fast_Setup);
         end if;

         Perform_Pass (Fast_Setup, Delta_Config.Motion.Fast_Approach_Velocity);
         Back_Off_And_Check_Released (Fast_Setup);

         declare
            Has_Shared_Switch : constant Boolean := (for some Tower in Delta_Tower_Name => Switch_Is_Shared (Tower));
         begin
            Perform_Pass (Slow_Setup, Delta_Config.Motion.Slow_Approach_Velocity);

            if Has_Shared_Switch then
               Back_Off_And_Check_Released (Slow_Setup);

               for Tower in Delta_Tower_Name loop
                  if Switch_Is_Shared (Tower) then
                     declare
                        Individual_Setup : constant Loop_Move_Setup :=
                          Setup_For
                            (Delta_Config.Motion.Fast_Maximum_Travel, Delta_Config.Motion.Slow_Approach_Velocity);
                     begin
                        Perform_Motor_Pass (Tower, Individual_Setup, Delta_Config.Motion.Slow_Approach_Velocity);
                        Back_Off_Tower_And_Check_Released (Tower, Individual_Setup);
                     end;
                  end if;
               end loop;
            end if;
         end;
         Planner.Flush (Delta_Clear_Event'(Module_Instance_Ref => Self_Ref));

         for Axis in X_Axis .. Z_Axis loop
            Planner.Mark_Axis_Homed (Axis);
         end loop;

         --  Always plan this move: the slow-pass tail stops beyond the detector-hit point. Shared-input towers are
         --  subsequently backed off, so even a Move_To_After equal to Home_Z requires a physical move.
         Planner.Add_Corner
           ([Home_Position with delta Z_Axis => Delta_Config.Move_To_After],
            Feedrate => Delta_Config.Motion.Slow_Approach_Velocity);
         Planner.Flush;
      end Home_Delta_Group;
   begin
      if Kinematics_Config.Kind = Kinematics_Module.Linear_Delta_Kinematics then
         Home_Delta_Group;
         return;
      end if;

      for Axis in Axis_Name when Axis /= E_Axis and then Requested_Axes (Axis) loop
         Home_Axis (Axis);
      end loop;
   exception
      when Prunt.Motion_Planner.Homing_Move_Cancelled_Error =>
         Restore_All_Homing_Drivers (This);
         for Axis in Axis_Name when Axis /= E_Axis loop
            Planner.Mark_Axis_Unhomed (Axis);
         end loop;

      when others =>
         Restore_All_Homing_Drivers (This);
         --  A partially completed homing sequence cannot leave cartesian axes trusted.
         for Axis in Axis_Name when Axis /= E_Axis loop
            Planner.Mark_Axis_Unhomed (Axis);
         end loop;
         raise;
   end Auto_Home;

end Prunt.Default_Modules.Homing;
