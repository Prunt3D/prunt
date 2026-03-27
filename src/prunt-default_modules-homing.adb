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

   function Input_Switch_Path (Axis : Axis_Name) return Config.Config_Data_Paths.Vector
   is (["Homing", +Axis'Image, "Homing_Method", "Kind", "Children", "Use_Input_Switch", "Use_Input_Switch", "Switch"]);

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema is
   begin
      return (Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

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
   begin
      return Result : Module_Instance do
         declare
            Input_Switches_Module_Instance : Input_Switches_Module.Module_Instance_Interface'Class renames
              Input_Switches_Module.Module_Instance_Interface'Class
                (Input_Switches_Module_Instance_Ref.Get.Element.all);
         begin
            Result.Initialize (Parsed_Config);

            for Axis in Axis_Name loop
               if Parsed_Config.Homing (Axis).Homing_Method.Kind = Disabled then
                  Report_Config_Error
                    (["Homing", +Axis'Image, "Homing_Method", "Kind"], "Homing is not configured for this axis.");
               elsif Parsed_Config.Homing (Axis).Homing_Method.Kind = Use_Input_Switch
                 and then
                   not Input_Switches_Module_Instance.Switch_Is_Enabled_In_Config
                         (Parsed_Config.Homing (Axis).Homing_Method.Use_Input_Switch.Switch)
               then
                  Report_Config_Error (Input_Switch_Path (Axis), "This switch is disabled in Input Switches.");
               end if;
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
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         Self_Ref := Self_Ref_In;
      end Start;

      procedure Auto_Home
        (Planner : Planner_Interface'Class;
         X       : Gcode_Optional_No_Value;
         Y       : Gcode_Optional_No_Value;
         Z       : Gcode_Optional_No_Value;
         E       : Gcode_Optional_No_Value) is
      begin
         --  TODO: Implement this.
      end Auto_Home;

      procedure Subscribe_To_Homing (Subscriber : not null access function return Homing_Event_Subscriber'Class) is
         Subscriber_Ref : Homing_Event_Subscriber_Shared_Pointers.Ref;
      begin
         Subscriber_Ref.Set (Subscriber);
         Subscribers.Append (Subscriber_Ref);
      end Subscribe_To_Homing;

      function Get_Homing_Parameters (Axis : Axis_Name) return Axis_Homing_Parameters is
         Method : User_Config_Homing_Method := Config.Homing (Axis).Homing_Method;
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
      end Get_Homing_Parameters;
   end Module_Instance;

end Prunt.Default_Modules.Homing;
