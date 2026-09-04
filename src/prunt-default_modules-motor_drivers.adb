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

package body Prunt.Default_Modules.Motor_Drivers is

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
      pragma Unreferenced (This, Report_Config_Error, Status_Emitter, Get_Other_Instance);
   begin
      return Result : Module_Instance do
         Result.Initialize (Config_Data_To_User_Config (Config_Data));
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
         pragma Unreferenced (Planner);
      begin
         Self_Ref := Self_Ref_In;

         for M in Motor_Name loop
            if not Motor_Configs_Provided (M) then
               raise Program_Error with "Motor configuration not provided for " & M'Image;
            end if;
         end loop;
      end Start;

      procedure Provide_Motor_Configuration
        (Motor : Motor_Name; Configuration : Motor_Configuration; Handler : Motor_Handler'Class) is
      begin
         if Motor_Configs_Provided (Motor) then
            raise Program_Error with "Motor configuration already provided for " & Motor'Image;
         end if;

         Motor_Configs (Motor) := Configuration;
         Motor_Handlers.Insert (Motor, Handler);
         Motor_Configs_Provided (Motor) := True;
      end Provide_Motor_Configuration;

      procedure Set_Motor_Axis_Map (Map : Motor_Axis_Map) is
      begin
         Motor_Axes := Map;
      end Set_Motor_Axis_Map;

      function Motor_Is_Enabled_In_Config (Motor : Motor_Name) return Boolean is
      begin
         return Config.Motors (Motor).Enabled;
      end Motor_Is_Enabled_In_Config;

      function Distance_Per_Rotation (Motor : Motor_Name) return Length is
         Motor_Config         : constant User_Config_Motion_Units := Config.Motors (Motor).Motion_Units;
         Direction_Multiplier : constant Dimensionless := (if Motor_Config.Reverse_Direction then -1.0 else 1.0);
      begin
         case Motor_Config.Kind is
            when Direct_Entry                    =>
               return Direction_Multiplier * Motor_Config.Direct_Entry.Distance_Per_Rotation;

            when Lead_Screw                      =>
               return
                 Direction_Multiplier
                 * Motor_Config.Lead_Screw.Lead
                 / (Motor_Config.Lead_Screw.Gear_Ratio.Numerator / Motor_Config.Lead_Screw.Gear_Ratio.Denominator);

            when Gear_With_Circumference         =>
               return
                 Direction_Multiplier
                 * Motor_Config.Gear_With_Circumference.Circumference
                 / (Motor_Config.Gear_With_Circumference.Gear_Ratio.Numerator
                    / Motor_Config.Gear_With_Circumference.Gear_Ratio.Denominator);

            when Gear_With_Tooth_Count_And_Pitch =>
               return
                 Direction_Multiplier
                 * (Motor_Config.Gear_With_Tooth_Count_And_Pitch.Tooth_Count
                    * Motor_Config.Gear_With_Tooth_Count_And_Pitch.Tooth_Pitch)
                 / (Motor_Config.Gear_With_Tooth_Count_And_Pitch.Gear_Ratio.Numerator
                    / Motor_Config.Gear_With_Tooth_Count_And_Pitch.Gear_Ratio.Denominator);
         end case;
      end Distance_Per_Rotation;

      function Distance_Per_Unit (Motor : Motor_Name; Microsteps : Dimensionless) return Length is
      begin
         return (Distance_Per_Rotation (Motor) / Config.Motors (Motor).Motion_Units.Units_Per_Rotation) / Microsteps;
      end Distance_Per_Unit;

      function Distance_Per_Unit (Motor : Motor_Name) return Length is
      begin
         if not Motor_Configs_Provided (Motor) then
            raise Program_Error with "Motor configuration not yet provided for " & Motor'Image;
         end if;

         return Distance_Per_Unit (Motor, Motor_Configs (Motor).Microsteps);
      end Distance_Per_Unit;

      procedure Enable_Selected (Axes : Axis_Selection) is
         Enable_All : constant Boolean := (for all Selected of Axes => not Selected);
      begin
         for Motor in Motor_Name loop
            if Config.Motors (Motor).Enabled
              and then
                (Enable_All or else (for some Axis in Axis_Name => Axes (Axis) and then Motor_Axes (Axis, Motor)))
            then
               declare
                  Handler : Motor_Handler'Class renames Motor_Handlers.Reference (Motor);
               begin
                  Handler.Enable_Motor;
               end;
            end if;
         end loop;
      end Enable_Selected;

      function Affected_Axes (Requested_Axes : Axis_Selection) return Axis_Selection is
         Affect_All : constant Boolean := (for all Selected of Requested_Axes => not Selected);
      begin
         return
           [for Axis in Axis_Name =>
              (for some Motor in Motor_Name =>
                 Config.Motors (Motor).Enabled
                 and then Motor_Axes (Axis, Motor)
                 and then
                   (Affect_All
                    or else
                      (for some Requested_Axis in Requested_Axes'Range =>
                         Requested_Axes (Requested_Axis) and then Motor_Axes (Requested_Axis, Motor))))];
      end Affected_Axes;

      procedure Disable_Selected (Requested_Axes : Axis_Selection; Invalidated_Axes : out Axis_Selection) is
         Disable_All    : constant Boolean := (for all Selected of Requested_Axes => not Selected);
         Disabled_Motor : array (Motor_Name) of Boolean := [others => False];
      begin
         for Motor in Motor_Name loop
            if Config.Motors (Motor).Enabled
              and then
                (Disable_All
                 or else (for some Axis in Axis_Name => Requested_Axes (Axis) and then Motor_Axes (Axis, Motor)))
            then
               declare
                  Handler : Motor_Handler'Class renames Motor_Handlers.Reference (Motor);
               begin
                  Handler.Disable_Motor;
               end;
               Disabled_Motor (Motor) := True;
            end if;
         end loop;

         Invalidated_Axes :=
           [for Axis in Axis_Name =>
              (for some Motor in Motor_Name => Disabled_Motor (Motor) and then Motor_Axes (Axis, Motor))];
      end Disable_Selected;

   end Module_Instance;

   procedure Enable_Steppers
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_No_Value;
      Y        : Gcode_Optional_No_Value;
      Z        : Gcode_Optional_No_Value;
      E        : Gcode_Optional_No_Value)
   is
      Instance : Module_Instance renames Module_Instance (Self_Ref.Get.Element.all);
   begin
      pragma Unreferenced (This, Planner);
      Instance.Enable_Selected ([X_Axis => X.Present, Y_Axis => Y.Present, Z_Axis => Z.Present, E_Axis => E.Present]);
   end Enable_Steppers;

   procedure Disable_Steppers
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_No_Value;
      Y        : Gcode_Optional_No_Value;
      Z        : Gcode_Optional_No_Value;
      E        : Gcode_Optional_No_Value)
   is
      Instance         : Module_Instance renames Module_Instance (Self_Ref.Get.Element.all);
      Requested_Axes   : constant Axis_Selection :=
        [X_Axis => X.Present, Y_Axis => Y.Present, Z_Axis => Z.Present, E_Axis => E.Present];
      Invalidated_Axes : Axis_Selection;
   begin
      pragma Unreferenced (This);

      --  Marking the axes unhomed flushes and waits for all preceding motion. Motors are only released after every
      --  affected axis has reached that synchronization point.
      Invalidated_Axes := Instance.Affected_Axes (Requested_Axes);
      for Axis in Axis_Name when Invalidated_Axes (Axis) loop
         Planner.Mark_Axis_Unhomed (Axis);
      end loop;
      Instance.Disable_Selected (Requested_Axes, Invalidated_Axes);
   end Disable_Steppers;

   procedure Disable_Steppers_M84
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_No_Value;
      Y        : Gcode_Optional_No_Value;
      Z        : Gcode_Optional_No_Value;
      E        : Gcode_Optional_No_Value) is
   begin
      Disable_Steppers (This, Self_Ref, Planner, X, Y, Z, E);
   end Disable_Steppers_M84;

end Prunt.Default_Modules.Motor_Drivers;
