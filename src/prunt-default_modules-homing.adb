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

package body Prunt.Default_Modules.Homing is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   function First_StallGuard2_Motor return Motor_Name is
   begin
      for M in Motor_Name loop
         if Motor_Hardware (M).Kind = TMC2240_UART_Kind then
            return M;
         end if;
      end loop;

      raise Constraint_Error with "No StallGuard2-capable motors are available.";
   end First_StallGuard2_Motor;

   function First_StallGuard4_Motor return Motor_Name is
   begin
      for M in Motor_Name loop
         if Motor_Hardware (M).Kind = TMC2240_UART_Kind then
            return M;
         end if;
      end loop;

      raise Constraint_Error with "No StallGuard4-capable motors are available.";
   end First_StallGuard4_Motor;

   function First_User_Visible_Input_Switch return Input_Switch_Name is
   begin
      for S in Input_Switch_Name loop
         if Input_Switch_Hardware (S).Visible_To_User then
            return S;
         end if;
      end loop;

      raise Constraint_Error with "No user-visible input switches are available.";
   end First_User_Visible_Input_Switch;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class is
   begin
      return Config.Versioned_Config_Schema'(Version => 1, Top_Level_Items => Build_Schema);
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
   procedure Process_After_Block (This : Homing_Event; Context : Block_End_Context'Class) is
      pragma Unreferenced (Context);

      Instance : Module_Instance renames Module_Instance (This.Module_Instance_Ref.Get.Element.all);
   begin
      case This.Kind is
         when Axis_Start_Event  =>
            Instance.Notify_Homing_Axis_Start (This.Axis);

         when Axis_Finish_Event =>
            Instance.Notify_Homing_Axis_Finish (This.Axis);
      end case;
   end Process_After_Block;

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

      Parsed_Config                      : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Input_Switches_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Input_Switches_Module.Module_Instance'Tag);
      Input_Switches_Module_Instance     : Input_Switches_Module.Module_Instance_Interface'Class renames
        Input_Switches_Module.Module_Instance_Interface'Class (Input_Switches_Module_Instance_Ref.Get.Element.all);
   begin
      return Result : Module_Instance do
         Result.Initialize (Parsed_Config);

         for Axis in Axis_Name when Axis /= E_Axis loop
            if Parsed_Config.Homing (Axis).Homing_Method.Kind = Disabled then
               Report_Config_Error
                 (["Homing", +Axis'Image, "Homing_Method", "Kind"], "Homing is not configured for this axis.");
            elsif Parsed_Config.Homing (Axis).Homing_Method.Kind = Use_Input_Switch
              and then
                not Input_Switches_Module_Instance.Switch_Is_Enabled_In_Config
                      (Parsed_Config.Homing (Axis).Homing_Method.Use_Input_Switch.Switch)
            then
               Report_Config_Error
                 (["Homing",
                   +Axis'Image,
                   "Homing_Method",
                   "Kind",
                   "Children",
                   "Use_Input_Switch",
                   "Use_Input_Switch",
                   "Switch"],
                  "This switch is disabled.");
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

                  if Parsed_Config.Homing (Axis).Prerequisites (Prereq_Axis).Kind /= No_Requirement then
                     case Axis_States (Prereq_Axis) is
                        when Unvisited =>
                           Check_Axis (Prereq_Axis);

                        when Visiting  =>
                           --  TODO: We should show the cycle in the log.
                           Report_Config_Error
                             (["Homing", +Axis'Image, "Prerequisites", +Prereq_Axis'Image, "Kind"],
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
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config) is
      begin
         Config := Config_In;
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Self_Ref_In, Planner);
      begin
         null;
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

      function Get_Homing_Parameters (Axis : Axis_Name) return Axis_Homing_Parameters is
      begin
         if Axis = E_Axis then
            return (Kind => No_Axis_Homing_Parameters_Kind);
         end if;

         declare
            Method : constant User_Config_Homing_Method := Config.Homing (Axis).Homing_Method;
         begin
            case Method.Kind is
               when Disabled | Set_To_Value =>
                  return (Kind => No_Axis_Homing_Parameters_Kind);

               when Use_Input_Switch        =>
                  return (Kind => Use_Input_Switch_Kind, Switch => Method.Use_Input_Switch.Switch);

               when Use_StallGuard2         =>
                  return
                    (Kind            => Use_StallGuard2_Kind,
                     Use_StallGuard2 =>
                       (Motor         => Method.Use_StallGuard2.Motor,
                        Threshold     => Method.Use_StallGuard2.Threshold,
                        Enable_Filter => Method.Use_StallGuard2.Enable_Filter));

               when Use_StallGuard4         =>
                  return
                    (Kind            => Use_StallGuard4_Kind,
                     Use_StallGuard4 =>
                       (Motor         => Method.Use_StallGuard4.Motor,
                        Threshold     => Method.Use_StallGuard4.Threshold,
                        Enable_Filter => Method.Use_StallGuard4.Enable_Filter));
            end case;
         end;
      end Get_Homing_Parameters;

      function Get_Config return User_Config is
      begin
         return Config;
      end Get_Config;
   end Module_Instance;

   procedure Auto_Home
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_No_Value;
      Y        : Gcode_Optional_No_Value;
      Z        : Gcode_Optional_No_Value)
   is
      Config : constant User_Config := This.Get_Config;

      type Axis_Homing_State is (Pending, Visiting, Done);

      Requested_Axes : constant array (Axis_Name) of Boolean :=
        (if not (X.Present or else Y.Present or else Z.Present)
         then [X_Axis | Y_Axis | Z_Axis => True, E_Axis => False]
         else [X_Axis => X.Present, Y_Axis => Y.Present, Z_Axis => Z.Present, E_Axis => False]);

      Axis_States : array (Axis_Name) of Axis_Homing_State :=
        [for A in Axis_Name => (if Requested_Axes (A) or else not Planner.Axis_Is_Homed (A) then Pending else Done)];

      function Directed_Length (Move_Towards_Negative_Infinity : Boolean; Distance : Length) return Length
      is (if Move_Towards_Negative_Infinity then -Distance else Distance);

      function Create_Homing_Event (Axis : Axis_Name; Kind : Homing_Event_Kind) return Homing_Event;

      function Create_Homing_Event (Axis : Axis_Name; Kind : Homing_Event_Kind) return Homing_Event is
      begin
         return Homing_Event'(Module_Instance_Ref => Self_Ref, Axis => Axis, Kind => Kind);
      end Create_Homing_Event;

      procedure Home_Axis (Axis : Axis_Name);

      procedure Home_Axis (Axis : Axis_Name) is
         Axis_Config : constant User_Config_Axis_Homing := Config.Homing (Axis);
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
                        Feedrate => Config.Homing (Prereq_Axis).Velocity_Limit);
                     Planner.Flush;
               end case;
            end;
         end loop;

         --  TODO: Every homing move here needs a maximum move length.

         case Axis_Config.Homing_Method.Kind is
            when Set_To_Value     =>
               Planner.Flush_And_Reset_Position
                 (New_Position =>
                    [Planner.Get_Last_Position with delta Axis => Axis_Config.Homing_Method.Set_To_Value.Position],
                  Extra_Data   => Create_Homing_Event (Axis, Axis_Finish_Event));

            when Use_Input_Switch =>
               declare
                  Method        : constant User_Config_Homing_Use_Input_Switch :=
                    Axis_Config.Homing_Method.Use_Input_Switch;
                  Home_Position : constant Position :=
                    [Planner.Get_Last_Position with delta Axis => Method.Switch_Position];
               begin
                  --  TODO: Add validation that the first move approximately matches the second point.

                  Planner.Flush_And_Reset_Position
                    (New_Position => [Home_Position with delta Axis => 0.0 * mm],
                     Extra_Data   => Create_Homing_Event (Axis, Axis_Start_Event));

                  --  TODO: Back off if switch is already hit.
                  --  TODO: We need before_block events for this to work.

                  --  TODO: Event to check switch state.

                  Planner.Add_Corner
                    ([Home_Position with delta
                        Axis => Directed_Length (Method.Move_Towards_Negative_Infinity, Method.First_Move_Distance)],
                     Feedrate      => Axis_Config.Velocity_Limit,
                     Require_Homed => False);

                  Planner.Flush_And_Reset_Position
                    (New_Position => [Home_Position with delta Axis => 0.0 * mm], Is_Homing_Move => True);

                  --  TODO: Event to check switch state.

                  Planner.Add_Corner
                    ([Home_Position with delta
                        Axis =>
                          Directed_Length (Method.Move_Towards_Negative_Infinity, -Method.Back_Off_Move_Distance)],
                     Feedrate      => Axis_Config.Velocity_Limit,
                     Require_Homed => False);

                  Planner.Flush_And_Reset_Position (New_Position => [Home_Position with delta Axis => 0.0 * mm]);

                  --  TODO: Event to check switch state.

                  Planner.Add_Corner
                    ([Home_Position with delta
                        Axis => Directed_Length (Method.Move_Towards_Negative_Infinity, Method.Second_Move_Distance)],
                     Feedrate      => Axis_Config.Velocity_Limit,
                     Require_Homed => False);

                  Planner.Flush_And_Reset_Position
                    (New_Position   => Home_Position,
                     Extra_Data     => Create_Homing_Event (Axis, Axis_Finish_Event),
                     Is_Homing_Move => True);

                  --  TODO: Event to check switch state.
               end;

            when Use_StallGuard2  =>
               declare
                  Method        : constant User_Config_Homing_Use_StallGuard2 :=
                    Axis_Config.Homing_Method.Use_StallGuard2;
                  Home_Position : constant Position :=
                    [Planner.Get_Last_Position with delta Axis => Method.Stop_Position];
               begin
                  --  TODO: TMC module needs to do setup and validation here.

                  Planner.Flush_And_Reset_Position
                    (New_Position => [Home_Position with delta Axis => 0.0 * mm],
                     Extra_Data   => Create_Homing_Event (Axis, Axis_Start_Event));

                  Planner.Add_Corner
                    ([Home_Position with delta
                        Axis => Directed_Length (Method.Move_Towards_Negative_Infinity, 1.0 * mm)],
                     Feedrate      => Axis_Config.Velocity_Limit,
                     Require_Homed => False);
                  --  TODO: We probably do not want to hardcode 1 mm here, what should we do instead?

                  Planner.Flush_And_Reset_Position
                    (New_Position   => Home_Position,
                     Extra_Data     => Create_Homing_Event (Axis, Axis_Finish_Event),
                     Is_Homing_Move => True);
               end;

            when Use_StallGuard4  =>
               declare
                  Method        : constant User_Config_Homing_Use_StallGuard4 :=
                    Axis_Config.Homing_Method.Use_StallGuard4;
                  Home_Position : constant Position :=
                    [Planner.Get_Last_Position with delta Axis => Method.Stop_Position];
               begin
                  --  TODO: TMC module needs to do setup and validation here.

                  Planner.Flush_And_Reset_Position
                    (New_Position => [Home_Position with delta Axis => 0.0 * mm],
                     Extra_Data   => Create_Homing_Event (Axis, Axis_Start_Event));

                  Planner.Add_Corner
                    ([Home_Position with delta
                        Axis => Directed_Length (Method.Move_Towards_Negative_Infinity, 1.0 * mm)],
                     Feedrate      => Axis_Config.Velocity_Limit,
                     Require_Homed => False);
                  --  TODO: We probably do not want to hardcode 1 mm here, what should we do instead?

                  Planner.Flush_And_Reset_Position
                    (New_Position   => Home_Position,
                     Extra_Data     => Create_Homing_Event (Axis, Axis_Finish_Event),
                     Is_Homing_Move => True);
               end;

            when Disabled         =>
               raise Program_Error with "Disabled homing method should have been caught earlier.";
         end case;

         Planner.Mark_Axis_Homed (Axis);

         if Planner.Get_Last_Position (Axis) /= Axis_Config.Move_To_After then
            Planner.Add_Corner
              ([Planner.Get_Last_Position with delta Axis => Axis_Config.Move_To_After],
               Feedrate => Axis_Config.Velocity_Limit);
            Planner.Flush;
         end if;

         Axis_States (Axis) := Done;
      end Home_Axis;
   begin
      for Axis in Axis_Name when Axis /= E_Axis and then Requested_Axes (Axis) loop
         Home_Axis (Axis);
      end loop;
   end Auto_Home;

end Prunt.Default_Modules.Homing;
