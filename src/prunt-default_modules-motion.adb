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
      return My_Modules.Module_Instance'Class
   is
      Parsed_Config                  : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Kinematics_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Kinematics_Module.Module_Instance'Tag);
      Kinematics_Module_Instance     : Kinematics_Module.Module_Instance_Interface'Class renames
        Kinematics_Module.Module_Instance_Interface'Class (Kinematics_Module_Instance_Ref.Get.Element.all);

      procedure Report_If_Absolute_Park_Position_Out_Of_Bounds;

      procedure Report_If_Absolute_Park_Position_Out_Of_Bounds is
         use type Config.Config_Data_Paths.Vector;

         Params : constant Motion_Planner.Kinematic_Parameters :=
           Kinematics_Module_Instance.Get_Default_Motion_Planner_Configuration.Parameters;

         procedure Check_Axis (Axis : Axis_Name; Value : Length; Path : Config.Config_Data_Paths.Vector) is
         begin
            if Value < Params.Lower_Pos_Limit (Axis) then
               Report_Config_Error (Path, "This absolute position is below the configured lower position limit.");
            end if;

            if Value > Params.Upper_Pos_Limit (Axis) then
               Report_Config_Error (Path, "This absolute position is above the configured upper position limit.");
            end if;
         end Check_Axis;
      begin
         if Parsed_Config.Pause_Park.Kind = Absolute_Park_Move then
            declare
               Move      : constant User_Config_Pause_Park_Absolute_Park_Move :=
                 Parsed_Config.Pause_Park.Absolute_Park_Move;
               Base_Path : constant Config.Config_Data_Paths.Vector :=
                 ["Pause_Park", "Kind", "Children", "Absolute_Park_Move", "Absolute_Park_Move"];
            begin
               Check_Axis (X_Axis, Move.X_Position, Base_Path & ["X_Position"]);
               Check_Axis (Y_Axis, Move.Y_Position, Base_Path & ["Y_Position"]);

               if Move.Z_Target.Kind = Absolute_Z_Position then
                  Check_Axis
                    (Z_Axis,
                     Move.Z_Target.Z_Position,
                     Base_Path & ["Z_Target", "Kind", "Children", "Absolute_Z_Position", "Z_Position"]);
               end if;
            end;
         end if;
      end Report_If_Absolute_Park_Position_Out_Of_Bounds;
   begin
      return Result : Module_Instance do
         Report_If_Absolute_Park_Position_Out_Of_Bounds;
         Result.Initialize (Parsed_Config, Status_Emitter);
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
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   procedure Add_Corner_If_Moved
     (Planner : Planner_Interface'Class; Current : in out Position; Target : Position; Feedrate : Velocity) is
   begin
      if Target /= Current then
         Planner.Add_Corner (Pos => Target, Feedrate => Feedrate);
         Current := Target;
      end if;
   end Add_Corner_If_Moved;

   function Bounds_Checked_Position
     (Target             : Position;
      Behavior           : User_Config_Pause_Park_Out_Of_Bounds_Behavior;
      Target_Description : String;
      Params             : Motion_Planner.Kinematic_Parameters) return Position
   is
      Result : Position := Target;
   begin
      for Axis in Axis_Name loop
         if Result (Axis) < Params.Lower_Pos_Limit (Axis) then
            case Behavior is
               when Error_If_Out_Of_Bounds =>
                  raise Constraint_Error
                    with Target_Description & " is out of bounds (" & Axis'Image & " = " & Result (Axis)'Image & ").";

               when Clip_To_Bounds         =>
                  Result (Axis) := Params.Lower_Pos_Limit (Axis);
            end case;
         elsif Result (Axis) > Params.Upper_Pos_Limit (Axis) then
            case Behavior is
               when Error_If_Out_Of_Bounds =>
                  raise Constraint_Error
                    with Target_Description & " is out of bounds (" & Axis'Image & " = " & Result (Axis)'Image & ").";

               when Clip_To_Bounds         =>
                  Result (Axis) := Params.Upper_Pos_Limit (Axis);
            end case;
         end if;
      end loop;

      return Result;
   end Bounds_Checked_Position;

   function Park_Position
     (Config : User_Config_Pause_Park; Pause_Position : Position; Params : Motion_Planner.Kinematic_Parameters)
      return Position is
   begin
      case Config.Kind is
         when Relative_Park_Move =>
            declare
               Move : constant User_Config_Pause_Park_Relative_Park_Move := Config.Relative_Park_Move;
            begin
               return
                 Bounds_Checked_Position
                   (Target             =>
                      Pause_Position
                      + Position_Offset'
                          (X_Axis => Move.X_Offset,
                           Y_Axis => Move.Y_Offset,
                           Z_Axis => Move.Z_Offset,
                           E_Axis => Move.E_Offset),
                    Behavior           => Move.Out_Of_Bounds_Behavior,
                    Target_Description => "Relative pause park target",
                    Params             => Params);
            end;

         when Absolute_Park_Move =>
            declare
               Move   : constant User_Config_Pause_Park_Absolute_Park_Move := Config.Absolute_Park_Move;
               Target : Position :=
                 (X_Axis => Move.X_Position,
                  Y_Axis => Move.Y_Position,
                  Z_Axis => Pause_Position (Z_Axis),
                  E_Axis => Pause_Position (E_Axis) + Move.E_Offset);
            begin
               case Move.Z_Target.Kind is
                  when Absolute_Z_Position =>
                     Target (Z_Axis) := Move.Z_Target.Z_Position;

                     if Move.Z_Target.Avoid_Lowering_Z and then Target (Z_Axis) < Pause_Position (Z_Axis) then
                        Target (Z_Axis) := Pause_Position (Z_Axis);
                     end if;

                  when Relative_Z_Offset   =>
                     Target (Z_Axis) := Pause_Position (Z_Axis) + Move.Z_Target.Z_Offset;
               end case;

               return
                 Bounds_Checked_Position
                   (Target             => Target,
                    Behavior           => Move.Out_Of_Bounds_Behavior,
                    Target_Description => "Absolute pause park target",
                    Params             => Params);
            end;

         when No_Park_Move       =>
            raise Program_Error with "Park_Position called without a configured park move.";
      end case;
   end Park_Position;

   function Park_Feedrate (Config : User_Config_Pause_Park) return Velocity is
   begin
      case Config.Kind is
         when Relative_Park_Move =>
            return Config.Relative_Park_Move.Feedrate;

         when Absolute_Park_Move =>
            return Config.Absolute_Park_Move.Feedrate;

         when No_Park_Move       =>
            raise Program_Error with "Park_Feedrate called without a configured park move.";
      end case;
   end Park_Feedrate;

   function Park_Return_Feedrate (Config : User_Config_Pause_Park) return Velocity is
   begin
      case Config.Kind is
         when Relative_Park_Move =>
            return Config.Relative_Park_Move.Return_Feedrate;

         when Absolute_Park_Move =>
            return Config.Absolute_Park_Move.Return_Feedrate;

         when No_Park_Move       =>
            raise Program_Error with "Park_Return_Feedrate called without a configured park move.";
      end case;
   end Park_Return_Feedrate;

   protected body Module_Instance is
      procedure Initialize (Config_In : User_Config; Status_Emitter_In : Status_Manager.Status_Emitter) is
      begin
         Config := Config_In;
         Status_Emitter := Status_Emitter_In;
         Feedrate := Config.Motion_Gcode.Default_G1_Feedrate;
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Self_Ref_In, Planner);
      begin
         null;
      end Start;

      function Get_Config return User_Config is
      begin
         return Config;
      end Get_Config;

      function Get_Feedrate return Velocity is
      begin
         return Feedrate;
      end Get_Feedrate;

      procedure Set_Feedrate (Value : Velocity) is
      begin
         Feedrate := Value;
      end Set_Feedrate;

      procedure Handle_Pause (Planner : Planner_Interface'Class; Context : Pause_Context'Class) is
         Pause_Position : constant Position := Context.Get_Pause_Position;
      begin
         if Config.Pause_Park.Kind in Relative_Park_Move | Absolute_Park_Move then
            declare
               Target  : constant Position :=
                 Park_Position (Config.Pause_Park, Pause_Position, Planner.Get_Last_Kinematic_Parameters);
               Feed    : constant Velocity := Park_Feedrate (Config.Pause_Park);
               Current : Position := Pause_Position;
               Next    : Position := Current;
            begin
               Next (E_Axis) := Target (E_Axis);
               Add_Corner_If_Moved (Planner, Current, Next, Feed);

               Next := Current;
               Next (Z_Axis) := Target (Z_Axis);
               Add_Corner_If_Moved (Planner, Current, Next, Feed);

               Next := Target;
               Add_Corner_If_Moved (Planner, Current, Next, Feed);
            end;
         end if;
      end Handle_Pause;

      procedure Handle_Resume (Planner : Planner_Interface'Class; Context : Pause_Context'Class) is
         Pause_Position : constant Position := Context.Get_Pause_Position;
      begin
         if Config.Pause_Park.Kind in Relative_Park_Move | Absolute_Park_Move then
            declare
               Feed    : constant Velocity := Park_Return_Feedrate (Config.Pause_Park);
               Current : Position := Planner.Get_Last_Position;
               Next    : Position := Current;
            begin
               Next (X_Axis) := Pause_Position (X_Axis);
               Next (Y_Axis) := Pause_Position (Y_Axis);
               Add_Corner_If_Moved (Planner, Current, Next, Feed);

               Next := Current;
               Next (Z_Axis) := Pause_Position (Z_Axis);
               Add_Corner_If_Moved (Planner, Current, Next, Feed);

               Next := Current;
               Next (E_Axis) := Pause_Position (E_Axis);
               Add_Corner_If_Moved (Planner, Current, Next, Feed);
            end;
         end if;
      end Handle_Resume;
   end Module_Instance;

   procedure Rapid_Linear_Move
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float)
   is
      Config : constant User_Config := This.Get_Config;
   begin
      --  TODO: Relative mode and handle retraction/G92 offsets.
      if Config.Motion_Gcode.Replace_G0_With_G1 then
         Linear_Move (This, Planner, X => X, Y => Y, Z => Z, E => E, F => F);
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

   procedure Linear_Move
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float)
   is
      Last_Pos     : constant Position := Planner.Get_Last_Position;
      Current_Rate : Velocity := This.Get_Feedrate;
   begin
      --  TODO: Relative mode and handle retraction/G92 offsets.
      if F.Present then
         Current_Rate := F.Value * mm / min;
      --  This.Set_Feedrate (Current_Rate);
      --  TODO: Needs to be event.

      end if;

      Planner.Add_Corner
        (Pos      =>
           [X_Axis => (if X.Present then X.Value * mm else Last_Pos (X_Axis)),
            Y_Axis => (if Y.Present then Y.Value * mm else Last_Pos (Y_Axis)),
            Z_Axis => (if Z.Present then Z.Value * mm else Last_Pos (Z_Axis)),
            E_Axis => (if E.Present then E.Value * mm else Last_Pos (E_Axis))],
         Feedrate => Current_Rate);
   end Linear_Move;

   procedure Clockwise_Arc_Move_Offset_Form
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float;
      I       : Dimensionless;
      J       : Dimensionless) is
   begin
      pragma Unreferenced (This, Planner, X, Y, Z, E, F, I, J);
      null;
      --  TODO
   end Clockwise_Arc_Move_Offset_Form;

   procedure Clockwise_Arc_Move_Radius_Form
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float;
      R       : Dimensionless) is
   begin
      pragma Unreferenced (This, Planner, X, Y, Z, E, F, R);
      null;
      --  TODO
   end Clockwise_Arc_Move_Radius_Form;

   procedure Counter_Clockwise_Arc_Move_Offset_Form
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float;
      I       : Dimensionless;
      J       : Dimensionless) is
   begin
      pragma Unreferenced (This, Planner, X, Y, Z, E, F, I, J);
      null;
      --  TODO
   end Counter_Clockwise_Arc_Move_Offset_Form;

   procedure Counter_Clockwise_Arc_Move_Radius_Form
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float;
      R       : Dimensionless) is
   begin
      pragma Unreferenced (This, Planner, X, Y, Z, E, F, R);
      null;
      --  TODO
   end Counter_Clockwise_Arc_Move_Radius_Form;

   procedure Retract (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      pragma Unreferenced (This, Planner);
      null;
      --  TODO
   end Retract;

   procedure Recover (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      pragma Unreferenced (This, Planner);
      null;
      --  TODO
   end Recover;

   procedure Millimeter_Units (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      pragma Unreferenced (This, Planner);
      null;
      --  TODO
   end Millimeter_Units;

   procedure Report_Stored_Positions (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      pragma Unreferenced (This, Planner);
      null;
      --  TODO
   end Report_Stored_Positions;

   procedure Save_Current_Position
     (This : Module_Instance; Planner : Planner_Interface'Class; S : Gcode_Arguments.Argument_Integer) is
   begin
      pragma Unreferenced (This, Planner, S);
      null;
      --  TODO
   end Save_Current_Position;

   procedure Delete_Stored_Position
     (This : Module_Instance; Planner : Planner_Interface'Class; D : Gcode_Arguments.Argument_Integer) is
   begin
      pragma Unreferenced (This, Planner, D);
      null;
      --  TODO
   end Delete_Stored_Position;

   procedure Delete_All_Stored_Positions
     (This : Module_Instance; Planner : Planner_Interface'Class; D : Gcode_No_Value) is
   begin
      pragma Unreferenced (This, Planner, D);
      null;
      --  TODO
   end Delete_All_Stored_Positions;

   procedure Restore_Saved_Position_G60
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      Q       : Gcode_Arguments.Argument_Integer;
      F       : Gcode_Optional_Float;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float) is
   begin
      pragma Unreferenced (This, Planner, Q, F, X, Y, Z, E);
      null;
      --  TODO
   end Restore_Saved_Position_G60;

   procedure Return_To_Saved_Position
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      F       : Gcode_Optional_Float;
      S       : Gcode_Arguments.Argument_Integer;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float) is
   begin
      pragma Unreferenced (This, Planner, F, S, X, Y, Z, E);
      null;
      --  TODO
   end Return_To_Saved_Position;

   procedure Absolute_Positioning (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      pragma Unreferenced (This, Planner);
      null;
      --  TODO
   end Absolute_Positioning;

   procedure Relative_Positioning (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      pragma Unreferenced (This, Planner);
      null;
      --  TODO
   end Relative_Positioning;

   procedure Set_Virtual_Position
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float) is
   begin
      pragma Unreferenced (This, Planner, X, Y, Z, E);
      null;
      --  TODO
   end Set_Virtual_Position;

   procedure E_Axis_Absolute (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      pragma Unreferenced (This, Planner);
      null;
      --  TODO
   end E_Axis_Absolute;

   procedure E_Axis_Relative (This : Module_Instance; Planner : Planner_Interface'Class) is
   begin
      pragma Unreferenced (This, Planner);
      null;
      --  TODO
   end E_Axis_Relative;

   procedure Retraction_Settings
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      F       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float) is
   begin
      pragma Unreferenced (This, Planner, F, E, Z);
      null;
      --  TODO
   end Retraction_Settings;

   procedure Recover_Settings
     (This : Module_Instance; Planner : Planner_Interface'Class; F : Gcode_Optional_Float; S : Gcode_Optional_Float) is
   begin
      pragma Unreferenced (This, Planner, F, S);
      null;
      --  TODO
   end Recover_Settings;

   procedure Set_Auto_Retract (This : Module_Instance; Planner : Planner_Interface'Class; S : Gcode_Optional_Float) is
   begin
      pragma Unreferenced (This, Planner, S);
      null;
      --  TODO
   end Set_Auto_Retract;

   procedure Set_Feedrate_Percentage
     (This : Module_Instance; Planner : Planner_Interface'Class; S : Gcode_Optional_Float) is
   begin
      pragma Unreferenced (This, Planner, S);
      null;
      --  TODO
   end Set_Feedrate_Percentage;

   procedure Set_Flow_Percentage (This : Module_Instance; Planner : Planner_Interface'Class; S : Gcode_Optional_Float)
   is
   begin
      pragma Unreferenced (This, Planner, S);
      null;
      --  TODO
   end Set_Flow_Percentage;

end Prunt.Default_Modules.Motion;
