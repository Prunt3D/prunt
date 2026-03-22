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

package body Prunt.Default_Modules.Motion is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema is
   begin
      return (Version => 1, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
   begin
      return Result : Module_Instance do
         Result.Initialize (Config_Data_To_User_Config (Config_Data), Status_Emitter);
      end return;
   end Initialize;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map is
   begin
      return
        ["G92 offset" =>
           [for A in Axis_Name use Conversions.To_Virtual_String (A'Image) =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm",
               Description => "G92 offset of axis " & Conversions.To_Virtual_String (A'Image),
               Condition   => "")]];
   end Status_Schema;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config; Status_Emitter_In : Status_Manager.Status_Emitter)
      is
      begin
         Config := Config_In;
         Status_Emitter := Status_Emitter_In;
         Feedrate := Config.Motion_Gcode.Default_G1_Feedrate;
      end Initialize;

      procedure Start (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         Self_Ref := Self_Ref_In;
      end Start;

      procedure Rapid_Linear_Move (Planner : Planner_Interface'Class; X, Y, Z, E, F : Gcode_Optional_Float) is
      begin
         --  TODO: Relative mode and handle retraction/G92 offsets.
         if Config.Motion_Gcode.Replace_G0_With_G1 then
            Linear_Move (Planner, X => X, Y => Y, Z => Z, E => E, F => F);
         else
            declare
               Last_Pos : constant Position := Planner.Get_Last_Position;
            begin
               Planner.Add_Corner
                 (Pos      =>
                    [X_Axis => (if X.Present then X.Value * mm else Last_Pos (X_Axis)),
                     Y_Axis => (if Y.Present then Y.Value * mm else Last_Pos (Y_Axis)),
                     Z_Axis => (if Z.Present then Z.Value * mm else Last_Pos (Z_Axis)),
                     E_Axis => (if E.Present then E.Value * mm else Last_Pos (E_Axis))],
                  Feedrate => (if F.Present then F.Value * mm / s else Velocity'Last));
            end;
         end if;
      end Rapid_Linear_Move;

      procedure Linear_Move (Planner : Planner_Interface'Class; X, Y, Z, E, F : Gcode_Optional_Float) is
         Last_Pos : constant Position := Planner.Get_Last_Position;
      begin
         --  TODO: Relative mode and handle retraction/G92 offsets.
         if F.Present then
            Feedrate := F.Value * mm / min;
         end if;

         Planner.Add_Corner
           (Pos      =>
              [X_Axis => (if X.Present then X.Value * mm else Last_Pos (X_Axis)),
               Y_Axis => (if Y.Present then Y.Value * mm else Last_Pos (Y_Axis)),
               Z_Axis => (if Z.Present then Z.Value * mm else Last_Pos (Z_Axis)),
               E_Axis => (if E.Present then E.Value * mm else Last_Pos (E_Axis))],
            Feedrate => Feedrate);
      end Linear_Move;

      procedure Clockwise_Arc_Move_Offset_Form
        (Planner : Planner_Interface'Class; X, Y, Z, E, F : Gcode_Optional_Float; I, J : Dimensionless) is
      begin
         null;
         --  TODO
      end Clockwise_Arc_Move_Offset_Form;

      procedure Clockwise_Arc_Move_Radius_Form
        (Planner : Planner_Interface'Class; X, Y, Z, E, F : Gcode_Optional_Float; R : Dimensionless) is
      begin
         null;
         --  TODO
      end Clockwise_Arc_Move_Radius_Form;

      procedure Counter_Clockwise_Arc_Move_Offset_Form
        (Planner : Planner_Interface'Class; X, Y, Z, E, F : Gcode_Optional_Float; I, J : Dimensionless) is
      begin
         null;
         --  TODO
      end Counter_Clockwise_Arc_Move_Offset_Form;

      procedure Counter_Clockwise_Arc_Move_Radius_Form
        (Planner : Planner_Interface'Class; X, Y, Z, E, F : Gcode_Optional_Float; R : Dimensionless) is
      begin
         null;
         --  TODO
      end Counter_Clockwise_Arc_Move_Radius_Form;

      procedure Retract (Planner : Planner_Interface'Class) is
      begin
         null;
         --  TODO
      end Retract;

      procedure Recover (Planner : Planner_Interface'Class) is
      begin
         null;
         --  TODO
      end Recover;

      procedure Millimeter_Units (Planner : Planner_Interface'Class) is
      begin
         null;
         --  TODO
      end Millimeter_Units;

      procedure Report_Stored_Positions (Planner : Planner_Interface'Class) is
      begin
         null;
         --  TODO
      end Report_Stored_Positions;

      procedure Save_Current_Position (Planner : Planner_Interface'Class; S : Gcode_Arguments.Argument_Integer) is
      begin
         null;
         --  TODO
      end Save_Current_Position;

      procedure Delete_Stored_Position (Planner : Planner_Interface'Class; D : Gcode_Arguments.Argument_Integer) is
      begin
         null;
         --  TODO
      end Delete_Stored_Position;

      procedure Delete_All_Stored_Positions (Planner : Planner_Interface'Class; D : Gcode_No_Value) is
      begin
         null;
         --  TODO
      end Delete_All_Stored_Positions;

      procedure Restore_Saved_Position_G60
        (Planner : Planner_Interface'Class; Q : Gcode_Arguments.Argument_Integer; F, X, Y, Z, E : Gcode_Optional_Float)
      is
      begin
         null;
         --  TODO
      end Restore_Saved_Position_G60;

      procedure Return_To_Saved_Position
        (Planner    : Planner_Interface'Class;
         F          : Gcode_Optional_Float;
         S          : Gcode_Arguments.Argument_Integer;
         X, Y, Z, E : Gcode_Optional_Float) is
      begin
         null;
         --  TODO
      end Return_To_Saved_Position;

      procedure Absolute_Positioning (Planner : Planner_Interface'Class) is
      begin
         null;
         --  TODO
      end Absolute_Positioning;

      procedure Relative_Positioning (Planner : Planner_Interface'Class) is
      begin
         null;
         --  TODO
      end Relative_Positioning;

      procedure Set_Virtual_Position (Planner : Planner_Interface'Class; X, Y, Z, E : Gcode_Optional_Float) is
      begin
         null;
         --  TODO
      end Set_Virtual_Position;

      procedure E_Axis_Absolute (Planner : Planner_Interface'Class) is
      begin
         null;
         --  TODO
      end E_Axis_Absolute;

      procedure E_Axis_Relative (Planner : Planner_Interface'Class) is
      begin
         null;
         --  TODO
      end E_Axis_Relative;

      procedure Retraction_Settings (Planner : Planner_Interface'Class; F, E, Z : Gcode_Optional_Float) is
      begin
         null;
         --  TODO
      end Retraction_Settings;

      procedure Recover_Settings (Planner : Planner_Interface'Class; F, S : Gcode_Optional_Float) is
      begin
         null;
         --  TODO
      end Recover_Settings;

      procedure Set_Auto_Retract (Planner : Planner_Interface'Class; S : Gcode_Optional_Float) is
      begin
         null;
         --  TODO
      end Set_Auto_Retract;

      procedure Set_Feedrate_Percentage (Planner : Planner_Interface'Class; S : Gcode_Optional_Float) is
      begin
         null;
         --  TODO
      end Set_Feedrate_Percentage;

      procedure Set_Flow_Percentage (Planner : Planner_Interface'Class; S : Gcode_Optional_Float) is
      begin
         null;
         --  TODO
      end Set_Flow_Percentage;
   end Module_Instance;

end Prunt.Default_Modules.Motion;
