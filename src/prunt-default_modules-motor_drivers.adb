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
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

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
      return My_Modules.Module_Instance'Class is
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

      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         Self_Ref := Self_Ref_In;

         for M in Motor_Name loop
            if not Motor_Configs_Provided (M) then
               raise Program_Error with "Motor configuration not provided for " & M'Image;
            end if;
         end loop;
      end Start;

      procedure Enable_Steppers
        (Planner : Planner_Interface'Class;
         X       : Gcode_Optional_No_Value;
         Y       : Gcode_Optional_No_Value;
         Z       : Gcode_Optional_No_Value;
         E       : Gcode_Optional_No_Value;
         A       : Gcode_Optional_No_Value;
         B       : Gcode_Optional_No_Value;
         C       : Gcode_Optional_No_Value;
         U       : Gcode_Optional_No_Value;
         V       : Gcode_Optional_No_Value;
         W       : Gcode_Optional_No_Value) is
      begin
         pragma Unreferenced (Planner, X, Y, Z, E, A, B, C, U, V, W);
         raise Constraint_Error with "M17 is not implemented yet.";
      end Enable_Steppers;

      procedure Disable_Steppers
        (Planner : Planner_Interface'Class;
         S       : Gcode_Optional_Integer;
         X       : Gcode_Optional_No_Value;
         Y       : Gcode_Optional_No_Value;
         Z       : Gcode_Optional_No_Value;
         E       : Gcode_Optional_No_Value;
         A       : Gcode_Optional_No_Value;
         B       : Gcode_Optional_No_Value;
         C       : Gcode_Optional_No_Value;
         U       : Gcode_Optional_No_Value;
         V       : Gcode_Optional_No_Value;
         W       : Gcode_Optional_No_Value) is
      begin
         pragma Unreferenced (Planner, S, X, Y, Z, E, A, B, C, U, V, W);
         raise Constraint_Error with "M18 is not implemented yet.";
      end Disable_Steppers;

      procedure Disable_Steppers_M84
        (Planner : Planner_Interface'Class;
         S       : Gcode_Optional_Integer;
         X       : Gcode_Optional_No_Value;
         Y       : Gcode_Optional_No_Value;
         Z       : Gcode_Optional_No_Value;
         E       : Gcode_Optional_No_Value;
         A       : Gcode_Optional_No_Value;
         B       : Gcode_Optional_No_Value;
         C       : Gcode_Optional_No_Value;
         U       : Gcode_Optional_No_Value;
         V       : Gcode_Optional_No_Value;
         W       : Gcode_Optional_No_Value) is
      begin
         Disable_Steppers (Planner, S, X, Y, Z, E, A, B, C, U, V, W);
      end Disable_Steppers_M84;

      procedure Set_Microstepping
        (Planner : Planner_Interface'Class;
         B       : Gcode_Optional_Integer;
         S       : Gcode_Optional_Integer;
         X       : Gcode_Optional_Integer;
         Y       : Gcode_Optional_Integer;
         Z       : Gcode_Optional_Integer;
         A       : Gcode_Optional_Integer;
         C       : Gcode_Optional_Integer;
         U       : Gcode_Optional_Integer;
         V       : Gcode_Optional_Integer;
         W       : Gcode_Optional_Integer;
         E       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, B, S, X, Y, Z, A, C, U, V, W, E);
         raise Constraint_Error with "M350 is not implemented yet.";
      end Set_Microstepping;

      procedure Set_Microstep_Pins
        (Planner : Planner_Interface'Class;
         S       : Gcode_Arguments.Argument_Integer;
         B       : Gcode_Optional_Integer;
         X       : Gcode_Optional_Integer;
         Y       : Gcode_Optional_Integer;
         Z       : Gcode_Optional_Integer;
         E       : Gcode_Optional_Integer) is
      begin
         pragma Unreferenced (Planner, S, B, X, Y, Z, E);
         raise Constraint_Error with "M351 is not implemented yet.";
      end Set_Microstep_Pins;

      procedure Set_Trimpot_Current
        (Planner : Planner_Interface'Class;
         B       : Gcode_Optional_Float;
         C       : Gcode_Optional_Float;
         D       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         S       : Gcode_Optional_Float;
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         I       : Gcode_Optional_Float;
         J       : Gcode_Optional_Float;
         K       : Gcode_Optional_Float;
         U       : Gcode_Optional_Float;
         V       : Gcode_Optional_Float;
         W       : Gcode_Optional_Float) is
      begin
         pragma Unreferenced (Planner, B, C, D, E, S, X, Y, Z, I, J, K, U, V, W);
         raise Constraint_Error with "M907 is not implemented yet.";
      end Set_Trimpot_Current;

      procedure Set_Trimpot_Pin
        (Planner : Planner_Interface'Class;
         P       : Gcode_Arguments.Argument_Integer;
         S       : Gcode_Arguments.Argument_Integer) is
      begin
         pragma Unreferenced (Planner, P, S);
         raise Constraint_Error with "M908 is not implemented yet.";
      end Set_Trimpot_Pin;

      procedure Report_DAC_Current (Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Planner);
         My_Logger.Log ("M909 reporting is not implemented yet.");
      end Report_DAC_Current;

      procedure Commit_DAC_To_EEPROM (Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Planner);
         raise Constraint_Error with "M910 is not implemented yet.";
      end Commit_DAC_To_EEPROM;

      procedure Provide_Motor_Configuration
        (Motor : Motor_Name; Configuration : Motor_Configuration; Handler : Motor_Handler'Class) is
      begin
         if Motor_Configs_Provided (Motor) then
            raise Program_Error with "Motor configuration already provided for " & Motor'Image;
         end if;

         Motor_Configs (Motor) := Configuration;
         Motor_Configs_Provided (Motor) := True;
      end Provide_Motor_Configuration;

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
   end Module_Instance;

end Prunt.Default_Modules.Motor_Drivers;
