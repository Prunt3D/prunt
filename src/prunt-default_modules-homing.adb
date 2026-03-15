-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
--                                                                         --
--  This program is free software: you can redistribute it and/or modify   --
--  it under the terms of the GNU General Public License as published by   --
--  the Free Software Foundation, either version 3 of the License, or      --
--  (at your option) any later version.                                    --
--                                                                         --
--  This program is distributed in the hope that it will be useful,        --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          --
--  GNU General Public License for more details.                           --
--                                                                         --
--  You should have received a copy of the GNU General Public License      --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.  --
--                                                                         --
-----------------------------------------------------------------------------

package body Prunt.Default_Modules.Homing is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : Config.Config_Data; Config : User_Config) is separate;

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
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema is
   begin
      return (Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   --  overriding
   --  function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Status_Emitter, Get_Other_Instance);

      Parsed_Config : constant User_Config := Config_Data_To_User_Config (Config_Data.Get);
   begin
      return Result : Module_Instance do
         Result.Initialize (Parsed_Config);

         for Axis in Axis_Name loop
            if Parsed_Config.Homing (Axis).Homing_Method.Kind = Disabled then
               Report_Config_Error
                 (["Homing", +Axis'Image, "Homing_Method", "Kind"], "Homing is not configured for this axis.");
            end if;
         end loop;
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config) is
      begin
         Config := Config_In;
      end Initialize;

      procedure Start is null;

      procedure Gcode_Dispatch
        (Args               : in out Gcode_Arguments.Arguments;
         Planner            : Planner_Interface'Class;
         Command_Identifier : Gcode_Command_Identifier) is
      begin
         null;
         --  TODO
      end Gcode_Dispatch;

      procedure Subscribe_To_Homing (Subscriber : not null access function return Homing_Event_Subscriber'Class) is
         Subscriber_Ref : Homing_Event_Subscriber_Shared_Pointers.Ref;
      begin
         Subscriber_Ref.Set (Subscriber);
         Subscribers.Append (Subscriber_Ref);
      end Subscribe_To_Homing;

      function Get_Homing_Parameters (Axis : Axis_Name) return Axis_Homing_Parameters is
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
