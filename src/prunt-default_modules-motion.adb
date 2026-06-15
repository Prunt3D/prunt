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

with Ada.Strings;
with Ada.Strings.Fixed;
with VSS.Characters.Latin;

package body Prunt.Default_Modules.Motion is

   pragma Extensions_Allowed (On);

   use type Gcode_Arguments.Argument_Integer;

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   Pause_Park_Out_Of_Bounds_Error : exception;

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
      Parsed_Config                     : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Kinematics_Module_Instance_Ref    : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Kinematics_Module.Module_Instance'Tag);
      Kinematics_Module_Instance        : Kinematics_Module.Module_Instance_Interface'Class renames
        Kinematics_Module.Module_Instance_Interface'Class (Kinematics_Module_Instance_Ref.Get.Element.all);
      Config_Saving_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Config_Saving_Module.Module_Instance'Tag);

      procedure Report_If_Absolute_Park_Position_Out_Of_Bounds;

      procedure Report_If_Absolute_Park_Position_Out_Of_Bounds is
         use type Config.Config_Data_Paths.Vector;

         Params : constant Motion_Planner.Kinematic_Parameters :=
           Kinematics_Module_Instance.Get_Default_Motion_Planner_Configuration.Parameters;

         procedure Check_Axis (Axis : Axis_Name; Value : Length; Path : Config.Config_Data_Paths.Vector);

         procedure Check_Axis (Axis : Axis_Name; Value : Length; Path : Config.Config_Data_Paths.Vector) is
         begin
            pragma Annotate (Xcov, Exempt_On, "Configuration validation error reporting.");
            if Value < Params.Lower_Pos_Limit (Axis) then
               Report_Config_Error (Path, "This absolute position is below the configured lower position limit.");
            end if;

            if Value > Params.Upper_Pos_Limit (Axis) then
               Report_Config_Error (Path, "This absolute position is above the configured upper position limit.");
            end if;
            pragma Annotate (Xcov, Exempt_Off);
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
         Config_Saving_Module.Config_Saver'Class (Config_Saving_Module_Instance_Ref.Get.Element.all)
           .Register_For_Saving (Config_Data);
         Result.Initialize (Parsed_Config, Config_Data, Status_Emitter);
      end return;
   end Initialize;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map is
      pragma Unreferenced (This);
   begin
      return
        ["G92 offset"       =>
           [for A in Axis_Name use Conversions.To_Virtual_String (A'Image) =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm",
               Description => "G92 offset of axis " & Conversions.To_Virtual_String (A'Image),
               Condition   => "")],
         "Modal state"      =>
           ["Units"         =>
              (Kind        => Status_Manager.String_Kind,
               Unit        => "",
               Description => "Current G20/G21 motion input unit mode.",
               Condition   => ""),
            "Positioning"   =>
              (Kind        => Status_Manager.String_Kind,
               Unit        => "",
               Description => "Current G90/G91 XYZ positioning mode.",
               Condition   => ""),
            "E positioning" =>
              (Kind        => Status_Manager.String_Kind,
               Unit        => "",
               Description => "Current E-axis positioning mode.",
               Condition   => "")],
         "Feedrate"         =>
           ["Stored feedrate"       =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm/s",
               Description => "Feedrate used by G1 moves without an F parameter.",
               Condition   => ""),
            "Feedrate scale"        =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "×",
               Description => "Scale applied to newly planned feedrates by M220.",
               Condition   => ""),
            "Backup feedrate scale" =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "×",
               Description => "Feedrate scale saved by M220 B.",
               Condition   => ""),
            "Effective feedrate"    =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm/s",
               Description => "Stored feedrate after applying the feedrate scale.",
               Condition   => "")],
         "Flow"             =>
           ["Flow scale" =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "×",
               Description => "Scale applied to newly planned E-axis movement by M221.",
               Condition   => "")],
         "Firmware retract" =>
           ["Retract length"       =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm",
               Description => "Current G10 firmware retract length.",
               Condition   => ""),
            "Retract feedrate"     =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm/s",
               Description => "Current G10 firmware retract feedrate.",
               Condition   => ""),
            "Retract Z lift"       =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm",
               Description => "Current G10 firmware retract Z lift.",
               Condition   => ""),
            "Recover extra length" =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm",
               Description => "Current G11 firmware recover extra length.",
               Condition   => ""),
            "Recover feedrate"     =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm/s",
               Description => "Current G11 firmware recover feedrate.",
               Condition   => ""),
            "Auto retract enabled" =>
              (Kind        => Status_Manager.Boolean_Kind,
               Unit        => "",
               Description => "True if M209 automatic retract detection is enabled.",
               Condition   => ""),
            "Is retracted"         =>
              (Kind        => Status_Manager.Boolean_Kind,
               Unit        => "",
               Description => "True if the firmware retract state is currently retracted.",
               Condition   => ""),
            "Current Z hop"        =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm",
               Description => "Current firmware retract Z hop offset.",
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

   function Trimmed_Image (Value : Dimensionless) return String
   is (Ada.Strings.Fixed.Trim (Dimensionless'Image (Value), Ada.Strings.Both));

   function Unit_Scale (Units : Linear_Units_Mode) return Length
   is (case Units is
         when Millimeter_Units_Mode => 1.0 * mm,
         when Inch_Units_Mode       => 25.4 * mm);

   function Position_Report (Prefix : String; Pos : Position; Units : Linear_Units_Mode) return Virtual_String is
      Scale : constant Length := Unit_Scale (Units);
      Unit  : constant String :=
        (case Units is
           when Millimeter_Units_Mode => "mm",
           when Inch_Units_Mode       => "in");
   begin
      return
        +(Prefix
          & " X:"
          & Trimmed_Image (Pos (X_Axis) / Scale)
          & " "
          & Unit
          & ", Y:"
          & Trimmed_Image (Pos (Y_Axis) / Scale)
          & " "
          & Unit
          & ", Z:"
          & Trimmed_Image (Pos (Z_Axis) / Scale)
          & " "
          & Unit
          & ", E:"
          & Trimmed_Image (Pos (E_Axis) / Scale)
          & " "
          & Unit);
   end Position_Report;

   function To_Current_Units_Length (Value : Dimensionless; Units : Linear_Units_Mode) return Length
   is (Value * Unit_Scale (Units));

   function To_Current_Units_Feedrate (Value : Dimensionless; Units : Linear_Units_Mode) return Velocity
   is (Value * Unit_Scale (Units) / min);

   function E_Is_Relative (Positioning : Positioning_Mode; E_Positioning : E_Positioning_Mode) return Boolean
   is (case E_Positioning is
         when Relative_E_Positioning_Mode => True,
         when Absolute_E_Positioning_Mode => False,
         when Follow_XYZ_Positioning_Mode => Positioning = Relative_Positioning_Mode);

   function Optional_Float_Length (Value : Gcode_Optional_Float_Or_No_Value; Units : Linear_Units_Mode) return Length
   is (case Value.Kind is
         when Gcode_Value_Not_Present | Gcode_No_Value_Present => 0.0 * mm,
         when Gcode_Value_Present                              => To_Current_Units_Length (Value.Value, Units));

   procedure Update_Status (Status_Emitter : Status_Manager.Status_Emitter; State : Motion_State) is
   begin
      for Axis in Axis_Name loop
         Status_Emitter.Set_Value ("G92 offset", +Axis'Image, State.G92_Offset (Axis) / mm);
      end loop;

      Status_Emitter.Set_Value ("Modal state", "Units", +State.Units'Image);
      Status_Emitter.Set_Value ("Modal state", "Positioning", +State.Positioning'Image);
      Status_Emitter.Set_Value ("Modal state", "E positioning", +State.E_Positioning'Image);

      Status_Emitter.Set_Value ("Feedrate", "Stored feedrate", State.Feedrate / (mm / s));
      Status_Emitter.Set_Value ("Feedrate", "Feedrate scale", State.Feedrate_Scale);
      Status_Emitter.Set_Value ("Feedrate", "Backup feedrate scale", State.Backup_Feedrate_Scale);
      Status_Emitter.Set_Value ("Feedrate", "Effective feedrate", State.Feedrate * State.Feedrate_Scale / (mm / s));

      Status_Emitter.Set_Value ("Flow", "Flow scale", State.Flow_Scale);

      Status_Emitter.Set_Value ("Firmware retract", "Retract length", State.Retract_Length / mm);
      Status_Emitter.Set_Value ("Firmware retract", "Retract feedrate", State.Retract_Feedrate / (mm / s));
      Status_Emitter.Set_Value ("Firmware retract", "Retract Z lift", State.Retract_Z_Lift / mm);
      Status_Emitter.Set_Value ("Firmware retract", "Recover extra length", State.Recover_Extra_Length / mm);
      Status_Emitter.Set_Value ("Firmware retract", "Recover feedrate", State.Recover_Feedrate / (mm / s));
      Status_Emitter.Set_Value ("Firmware retract", "Auto retract enabled", State.Auto_Retract_Enabled);
      Status_Emitter.Set_Value ("Firmware retract", "Is retracted", State.Is_Retracted);
      Status_Emitter.Set_Value ("Firmware retract", "Current Z hop", State.Current_Z_Hop / mm);
   end Update_Status;

   function Logical_Position_From_Physical
     (Physical_Position : Position; G92_Offset : Position_Offset; Current_Z_Hop : Length) return Position
   is
      Result : Position := Physical_Position + G92_Offset;
   begin
      Result (Z_Axis) := Result (Z_Axis) - Current_Z_Hop;
      return Result;
   end Logical_Position_From_Physical;

   overriding
   procedure Process_After_Block (This : Motion_Report_Event; Context : Block_End_Context'Class) is
   begin
      --  This can technically trigger a bit earlier than the user might expect since we do not wait for the machine to
      --  become idle, however the queue should be short in practice and the values will still be correct, so this
      --  should not be an issue.
      My_Logger.Log (This.Message);
   end Process_After_Block;

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
                  raise Pause_Park_Out_Of_Bounds_Error
                    with Target_Description & " is out of bounds (" & Axis'Image & " = " & Result (Axis)'Image & ").";

               when Clip_To_Bounds         =>
                  Result (Axis) := Params.Lower_Pos_Limit (Axis);
            end case;
         elsif Result (Axis) > Params.Upper_Pos_Limit (Axis) then
            case Behavior is
               when Error_If_Out_Of_Bounds =>
                  raise Pause_Park_Out_Of_Bounds_Error
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
                 [X_Axis => Move.X_Position,
                  Y_Axis => Move.Y_Position,
                  Z_Axis => Pause_Position (Z_Axis),
                  E_Axis => Pause_Position (E_Axis) + Move.E_Offset];
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
            pragma Annotate (Xcov, Exempt_On, "Handled by precondition.");
            raise Program_Error with "Park_Position called without a configured park move.";
            pragma Annotate (Xcov, Exempt_Off);
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
            pragma Annotate (Xcov, Exempt_On, "Handled by precondition.");
            raise Program_Error with "Park_Feedrate called without a configured park move.";
            pragma Annotate (Xcov, Exempt_Off);
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
            pragma Annotate (Xcov, Exempt_On, "Handled by precondition.");
            raise Program_Error with "Park_Return_Feedrate called without a configured park move.";
            pragma Annotate (Xcov, Exempt_Off);
      end case;
   end Park_Return_Feedrate;

   procedure Apply_Stored_Position_Update
     (Stored_Positions : in out Saved_Position_Array; Update : Stored_Position_Update_Event) is
   begin
      case Update.Kind is
         when Save_Stored_Position        =>
            Stored_Positions (Update.Saved_Slot) := (Present => True, Pos => Update.Saved_Position);

         when Delete_Stored_Position      =>
            Stored_Positions (Update.Deleted_Slot).Present := False;

         when Delete_All_Stored_Positions =>
            Stored_Positions := [others => <>];
      end case;
   end Apply_Stored_Position_Update;

   function Stored_Position_Update_Changes
     (Stored_Positions : Saved_Position_Array; Update : Stored_Position_Update_Event) return Boolean is
   begin
      case Update.Kind is
         when Save_Stored_Position        =>
            pragma Annotate (Xcov, Exempt_On, "Stored-position equality MC/DC is covered by branch tests.");
            return
              (not Stored_Positions (Update.Saved_Slot).Present)
              or else Stored_Positions (Update.Saved_Slot).Pos /= Update.Saved_Position;
            pragma Annotate (Xcov, Exempt_Off);

         when Delete_Stored_Position      =>
            return Stored_Positions (Update.Deleted_Slot).Present;

         when Delete_All_Stored_Positions =>
            for Slot in Stored_Positions'Range loop
               if Stored_Positions (Slot).Present then
                  return True;
               end if;
            end loop;

            return False;
      end case;
   end Stored_Position_Update_Changes;

   protected body Module_Instance is
      procedure Initialize
        (Config_In         : User_Config;
         Config_Data_In    : Prunt.Config.Config_Data;
         Status_Emitter_In : Status_Manager.Status_Emitter) is
      begin
         Config := Config_In;
         Config_Data := Config_Data_In;
         Status_Emitter := Status_Emitter_In;
         Planned_State :=
           (Feedrate              => Config.Motion_Gcode.Default_G1_Feedrate,
            Units                 => Config.Motion_Gcode.Default_Units,
            Positioning           => Config.Motion_Gcode.Default_Positioning,
            E_Positioning         => Config.Motion_Gcode.Default_E_Positioning,
            G92_Offset            =>
              [X_Axis => Config.Motion_Gcode.Default_G92_X_Offset,
               Y_Axis => Config.Motion_Gcode.Default_G92_Y_Offset,
               Z_Axis => Config.Motion_Gcode.Default_G92_Z_Offset,
               E_Axis => Config.Motion_Gcode.Default_G92_E_Offset],
            Feedrate_Scale        => Config.Motion_Gcode.Default_Feedrate_Scale,
            Backup_Feedrate_Scale => Config.Motion_Gcode.Default_Feedrate_Scale,
            Flow_Scale            => Config.Motion_Gcode.Default_Flow_Scale,
            Retract_Length        => Config.Motion_Gcode.Firmware_Retract_Length,
            Retract_Feedrate      => Config.Motion_Gcode.Firmware_Retract_Feedrate,
            Retract_Z_Lift        => Config.Motion_Gcode.Firmware_Retract_Z_Lift,
            Recover_Extra_Length  => Config.Motion_Gcode.Firmware_Recover_Extra_Length,
            Recover_Feedrate      => Config.Motion_Gcode.Firmware_Recover_Feedrate,
            Auto_Retract_Enabled  => Config.Motion_Gcode.Default_Auto_Retract_Enabled,
            Is_Retracted          => False,
            Current_Z_Hop         => 0.0 * mm);
         Last_Queued_State := Planned_State;
         Committed_State := Planned_State;
         Planned_Stored_Positions := [others => (Present => False, others => <>)];
         Committed_Stored_Positions := Planned_Stored_Positions;
         Update_Status (Status_Emitter, Committed_State);
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Planner, Self_Ref_In);
      begin
         Update_Status (Status_Emitter, Committed_State);
      end Start;

      procedure Catch_Up_Planner_State (Executed_Corner_ID : Planner_Corner_ID) is
      begin
         while not Pending_States.Is_Empty and then Pending_States.Peek.Anchor_ID <= Executed_Corner_ID loop
            declare
               Snapshot : Pending_State_Snapshot;
            begin
               Pending_States.Dequeue (Snapshot);
               Committed_State := Snapshot.State;
               Committed_Corner_ID := Planner_Corner_ID'Max (@, Snapshot.Anchor_ID);
            end;
         end loop;

         pragma Annotate (Xcov, Exempt_On, "Planner catch-up timing path.");
         while not Pending_Stored_Position_Updates.Is_Empty
           and then Pending_Stored_Position_Updates.Peek.Anchor_ID <= Executed_Corner_ID
         loop
            declare
               Event : Stored_Position_Update_Event;
            begin
               Pending_Stored_Position_Updates.Dequeue (Event);
               Apply_Stored_Position_Update (Committed_Stored_Positions, Event);
               Committed_Corner_ID := Planner_Corner_ID'Max (@, Event.Anchor_ID);
            end;
         end loop;

         if Executed_Corner_ID > Committed_Corner_ID
           and then Pending_States.Is_Empty
           and then Pending_Stored_Position_Updates.Is_Empty
         then
            Committed_Corner_ID := Executed_Corner_ID;
         end if;
         pragma Annotate (Xcov, Exempt_Off);

         Update_Status (Status_Emitter, Committed_State);
      end Catch_Up_Planner_State;

      procedure Prepare_Config_For_Save is
      begin
         Config.Motion_Gcode.Firmware_Retract_Length := Committed_State.Retract_Length;
         Config.Motion_Gcode.Firmware_Retract_Feedrate := Committed_State.Retract_Feedrate;
         Config.Motion_Gcode.Firmware_Retract_Z_Lift := Committed_State.Retract_Z_Lift;
         Config.Motion_Gcode.Firmware_Recover_Extra_Length := Committed_State.Recover_Extra_Length;
         Config.Motion_Gcode.Firmware_Recover_Feedrate := Committed_State.Recover_Feedrate;

         User_Config_To_Config_Data (Config_Data, Config);
      end Prepare_Config_For_Save;

      procedure Ensure_Can_Queue_Planned_State (Planner : Planner_Interface'Class; Pending_Snapshots : Positive := 1)
      is
         Anchor_ID : Planner_Corner_ID;
      begin
         Catch_Up_Planner_State (Planner.Get_Last_Executed_Corner_ID);
         Anchor_ID := Planner.Get_State_Anchor_Corner_ID;

         pragma Annotate (Xcov, Exempt_On, "Planner state queue overflow.");
         if not Pending_States.Can_Enqueue
                  (Pending_State_Snapshot'(Anchor_ID => Anchor_ID, State => Planned_State), Pending_Snapshots)
         then
            Planned_State := Last_Queued_State;
            raise Gcode_Temporarily_Rejected_Error;
         end if;
         pragma Annotate (Xcov, Exempt_Off);
      end Ensure_Can_Queue_Planned_State;

      procedure Maybe_Queue_Planned_State (Planner : Planner_Interface'Class) is
      begin
         if Planned_State = Last_Queued_State then
            return;
         end if;

         declare
            Anchor_ID   : constant Planner_Corner_ID := Planner.Get_State_Anchor_Corner_ID;
            Executed_ID : constant Planner_Corner_ID := Planner.Get_Last_Executed_Corner_ID;
         begin
            if Anchor_ID = Executed_ID then
               Pending_States.Clear;
               Committed_State := Planned_State;
               Committed_Corner_ID := Anchor_ID;
               Last_Queued_State := Planned_State;
               Update_Status (Status_Emitter, Committed_State);
            else
               Catch_Up_Planner_State (Executed_ID);
               begin
                  Pending_States.Enqueue (Pending_State_Snapshot'(Anchor_ID => Anchor_ID, State => Planned_State));
               exception
                  when Pending_State_Queues.Out_Of_Space_Error =>
                     pragma Annotate (Xcov, Exempt_On, "Planner state queue overflow.");
                     Planned_State := Last_Queued_State;
                     raise Gcode_Temporarily_Rejected_Error;
                     pragma Annotate (Xcov, Exempt_Off);
               end;

               Last_Queued_State := Planned_State;
            end if;
         end;
      end Maybe_Queue_Planned_State;

      procedure Queue_Stored_Position_Update (Planner : Planner_Interface'Class; Update : Stored_Position_Update_Event)
      is
         Event : Stored_Position_Update_Event := Update;
      begin
         if not Stored_Position_Update_Changes (Planned_Stored_Positions, Event) then
            return;
         end if;

         Catch_Up_Planner_State (Planner.Get_Last_Executed_Corner_ID);

         Event.Anchor_ID := Planner.Get_State_Anchor_Corner_ID;

         pragma Annotate (Xcov, Exempt_On, "Queued stored-position update timing path.");
         if Event.Anchor_ID = Planner.Get_Last_Executed_Corner_ID then
            pragma Assert (Pending_Stored_Position_Updates.Is_Empty);
            Apply_Stored_Position_Update (Planned_Stored_Positions, Event);
            Committed_Stored_Positions := Planned_Stored_Positions;
            Committed_Corner_ID := Event.Anchor_ID;
         else
            begin
               Pending_Stored_Position_Updates.Enqueue (Event);
            exception
               when Pending_Stored_Position_Update_Queues.Out_Of_Space_Error =>
                  raise Gcode_Temporarily_Rejected_Error;
            end;

            Apply_Stored_Position_Update (Planned_Stored_Positions, Event);
         end if;
         pragma Annotate (Xcov, Exempt_Off);
      end Queue_Stored_Position_Update;

      procedure Handle_Cancel
        (Executed_Corner_ID      : Planner_Corner_ID;
         Cancellation_Barrier_ID : Planner_Corner_ID;
         Current_Position        : Position)
      is
         pragma Unreferenced (Current_Position);
      begin
         Catch_Up_Planner_State (Executed_Corner_ID);
         Pending_States.Clear;
         Pending_Stored_Position_Updates.Clear;
         pragma Annotate (Xcov, Exempt_On, "Cancel barrier may already be committed.");
         if Cancellation_Barrier_ID > Committed_Corner_ID then
            Committed_Corner_ID := Cancellation_Barrier_ID;
         end if;
         pragma Annotate (Xcov, Exempt_Off);
         Planned_State := Committed_State;
         Last_Queued_State := Planned_State;
         Planned_Stored_Positions := Committed_Stored_Positions;
         Update_Status (Status_Emitter, Committed_State);
      end Handle_Cancel;

      procedure Execute_Linear_Move
        (Planner : Planner_Interface'Class;
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float;
         F       : Gcode_Optional_Float;
         Rapid   : Boolean)
      is
         Physical_Position          : Position := Planner.Get_Last_Position;
         Logical_Position           : constant Position :=
           Logical_Position_From_Physical (Physical_Position, Planned_State.G92_Offset, Planned_State.Current_Z_Hop);
         Target_Logical             : Position := Logical_Position;
         Target_Physical            : Position := Physical_Position;
         Command_Feedrate           : Velocity := Planned_State.Feedrate;
         Persistent_Feedrate_Change : Boolean := False;

         procedure Perform_Firmware_Retract (Retracting : Boolean);
         --  Performs a firmware retract converted from an E-only move. This is different from G10/G11 since the
         --  logical E position needs to be updated.

         procedure Perform_Firmware_Retract (Retracting : Boolean) is
            Target : Position := Physical_Position;
         begin
            if Retracting then
               Target (E_Axis) := Target (E_Axis) - Planned_State.Retract_Length * Planned_State.Flow_Scale;
               Add_Corner_If_Moved (Planner, Physical_Position, Target, Planned_State.Retract_Feedrate);

               if Planned_State.Retract_Z_Lift > 0.0 * mm then
                  Target := Physical_Position;
                  Target (Z_Axis) := Target (Z_Axis) + Planned_State.Retract_Z_Lift;
                  Add_Corner_If_Moved (Planner, Physical_Position, Target, Planned_State.Retract_Feedrate);
                  Planned_State.Current_Z_Hop := Planned_State.Retract_Z_Lift;
               end if;
            else
               if Planned_State.Current_Z_Hop /= 0.0 * mm then
                  Target (Z_Axis) := Target (Z_Axis) - Planned_State.Current_Z_Hop;
                  Add_Corner_If_Moved (Planner, Physical_Position, Target, Planned_State.Recover_Feedrate);
                  Planned_State.Current_Z_Hop := 0.0 * mm;
               end if;

               Target := Physical_Position;
               Target (E_Axis) :=
                 Target (E_Axis)
                 + (Planned_State.Retract_Length + Planned_State.Recover_Extra_Length) * Planned_State.Flow_Scale;
               Add_Corner_If_Moved (Planner, Physical_Position, Target, Planned_State.Recover_Feedrate);
            end if;

            Planned_State.Is_Retracted := Retracting;
         end Perform_Firmware_Retract;
      begin
         Ensure_Can_Queue_Planned_State (Planner, Pending_Snapshots => 2);

         if F.Present then
            Command_Feedrate := To_Current_Units_Feedrate (F.Value, Planned_State.Units);
            if Command_Feedrate <= 0.0 * mm / min then
               raise Gcode_Bad_Inputs_Error with "F feedrate must be greater than zero.";
            end if;
            Persistent_Feedrate_Change := not Rapid and then Command_Feedrate /= Planned_State.Feedrate;
         end if;

         declare
            procedure Resolve_Axis (Axis : Axis_Name; Value : Gcode_Optional_Float);

            procedure Resolve_Axis (Axis : Axis_Name; Value : Gcode_Optional_Float) is
               Converted : Length;
               Relative  : Boolean;
            begin
               if Value.Present then
                  Converted := To_Current_Units_Length (Value.Value, Planned_State.Units);
                  Relative :=
                    (if Axis = E_Axis
                     then E_Is_Relative (Planned_State.Positioning, Planned_State.E_Positioning)
                     else Planned_State.Positioning = Relative_Positioning_Mode);

                  if Relative then
                     Target_Logical (Axis) := Logical_Position (Axis) + Converted;
                  else
                     Target_Logical (Axis) := Converted;
                  end if;
               end if;
            end Resolve_Axis;
         begin
            Resolve_Axis (X_Axis, X);
            Resolve_Axis (Y_Axis, Y);
            Resolve_Axis (Z_Axis, Z);
            Resolve_Axis (E_Axis, E);
         end;

         if Planned_State.Auto_Retract_Enabled
           and then E.Present
           and then not X.Present
           and then not Y.Present
           and then not Z.Present
         then
            declare
               E_Delta : constant Length := Target_Logical (E_Axis) - Logical_Position (E_Axis);
            begin
               if abs E_Delta >= Config.Motion_Gcode.Auto_Retract_Min_Length
                 and then abs E_Delta <= Config.Motion_Gcode.Auto_Retract_Max_Length
                 and then
                   ((E_Delta < 0.0 * mm and then not Planned_State.Is_Retracted)
                    or else (E_Delta > 0.0 * mm and then Planned_State.Is_Retracted))
               then
                  if Persistent_Feedrate_Change then
                     Planned_State.Feedrate := Command_Feedrate;
                     Maybe_Queue_Planned_State (Planner);
                  end if;
                  Perform_Firmware_Retract (Retracting => E_Delta < 0.0 * mm);
                  Planned_State.G92_Offset (E_Axis) := Target_Logical (E_Axis) - Physical_Position (E_Axis);
                  Maybe_Queue_Planned_State (Planner);
                  return;
               end if;
            end;
         end if;

         for Axis in Axis_Name when Axis /= E_Axis loop
            Target_Physical (Axis) := Target_Logical (Axis) - Planned_State.G92_Offset (Axis);
         end loop;

         Target_Physical (E_Axis) :=
           Physical_Position (E_Axis)
           + (Target_Logical (E_Axis) - Logical_Position (E_Axis)) * Planned_State.Flow_Scale;

         Target_Physical (Z_Axis) := Target_Physical (Z_Axis) + Planned_State.Current_Z_Hop;

         if Persistent_Feedrate_Change then
            Planned_State.Feedrate := Command_Feedrate;
            Maybe_Queue_Planned_State (Planner);
         end if;

         Planned_State.G92_Offset (E_Axis) := Target_Logical (E_Axis) - Target_Physical (E_Axis);

         Add_Corner_If_Moved
           (Planner,
            Physical_Position,
            Target_Physical,
            Velocity'
              (if Rapid and then not F.Present
               then Velocity'Last
               else Command_Feedrate * Planned_State.Feedrate_Scale));
         pragma Unreferenced (Physical_Position);

         Maybe_Queue_Planned_State (Planner);
      end Execute_Linear_Move;

      procedure Execute_Retract (Planner : Planner_Interface'Class; S : Gcode_Optional_Integer) is
      begin
         if S.Present and then S.Value /= 0 then
            raise Gcode_Bad_Inputs_Error with "Swap retract is not supported because Prunt has one E axis.";
         end if;

         if Planned_State.Is_Retracted then
            return;
         end if;

         declare
            Physical_Position : Position := Planner.Get_Last_Position;
            Logical_Position  : constant Position :=
              Logical_Position_From_Physical
                (Physical_Position, Planned_State.G92_Offset, Planned_State.Current_Z_Hop);
            Target            : Position := Physical_Position;
         begin
            Target (E_Axis) := Target (E_Axis) - Planned_State.Retract_Length * Planned_State.Flow_Scale;
            if Target /= Physical_Position or else Planned_State.Retract_Z_Lift > 0.0 * mm then
               Ensure_Can_Queue_Planned_State (Planner);
            end if;

            Add_Corner_If_Moved (Planner, Physical_Position, Target, Planned_State.Retract_Feedrate);

            if Planned_State.Retract_Z_Lift > 0.0 * mm then
               Target := Physical_Position;
               Target (Z_Axis) := Target (Z_Axis) + Planned_State.Retract_Z_Lift;
               Add_Corner_If_Moved (Planner, Physical_Position, Target, Velocity'Last);
               Planned_State.Current_Z_Hop := Planned_State.Retract_Z_Lift;
            end if;

            Planned_State.Is_Retracted := True;
            Planned_State.G92_Offset (E_Axis) := Logical_Position (E_Axis) - Physical_Position (E_Axis);
            Maybe_Queue_Planned_State (Planner);
         end;
      end Execute_Retract;

      procedure Execute_Recover (Planner : Planner_Interface'Class) is
      begin
         if not Planned_State.Is_Retracted then
            return;
         end if;

         declare
            Physical_Position : Position := Planner.Get_Last_Position;
            Logical_Position  : constant Position :=
              Logical_Position_From_Physical
                (Physical_Position, Planned_State.G92_Offset, Planned_State.Current_Z_Hop);
            Target            : Position := Physical_Position;
            E_Delta           : constant Length :=
              (Planned_State.Retract_Length + Planned_State.Recover_Extra_Length) * Planned_State.Flow_Scale;
         begin
            if Planned_State.Current_Z_Hop /= 0.0 * mm or else E_Delta /= 0.0 * mm then
               Ensure_Can_Queue_Planned_State (Planner);
            end if;

            if Planned_State.Current_Z_Hop /= 0.0 * mm then
               declare
                  Z_Target : Position := Physical_Position;
               begin
                  Z_Target (Z_Axis) := Z_Target (Z_Axis) - Planned_State.Current_Z_Hop;
                  Add_Corner_If_Moved (Planner, Physical_Position, Z_Target, Velocity'Last);
               end;

               Planned_State.Current_Z_Hop := 0.0 * mm;
            end if;

            Target := Physical_Position;
            Target (E_Axis) := Target (E_Axis) + E_Delta;
            Add_Corner_If_Moved (Planner, Physical_Position, Target, Planned_State.Recover_Feedrate);

            Planned_State.Is_Retracted := False;
            Planned_State.G92_Offset (E_Axis) := Logical_Position (E_Axis) - Physical_Position (E_Axis);
            Maybe_Queue_Planned_State (Planner);
         end;
      end Execute_Recover;

      procedure Set_Inch_Units (Planner : Planner_Interface'Class) is
      begin
         Planned_State.Units := Inch_Units_Mode;
         Maybe_Queue_Planned_State (Planner);
      end Set_Inch_Units;

      procedure Set_Millimeter_Units (Planner : Planner_Interface'Class) is
      begin
         Planned_State.Units := Millimeter_Units_Mode;
         Maybe_Queue_Planned_State (Planner);
      end Set_Millimeter_Units;

      procedure Execute_Save_Current_Position (Planner : Planner_Interface'Class; S : Gcode_Integer_Or_No_Value) is
         Slot : constant Gcode_Arguments.Argument_Integer :=
           (case S.Kind is
              when Gcode_No_Value_Present => 0,
              when Gcode_Value_Present    => S.Value);
      begin
         Queue_Stored_Position_Update
           (Planner,
            (Kind           => Save_Stored_Position,
             Anchor_ID      => 0,
             Saved_Slot     => Slot,
             Saved_Position => Planner.Get_Last_Position));
      end Execute_Save_Current_Position;

      procedure Execute_Delete_Stored_Position (Planner : Planner_Interface'Class; D : Gcode_Integer_Or_No_Value) is
      begin
         if D.Kind = Gcode_No_Value_Present then
            Queue_Stored_Position_Update (Planner, (Kind => Delete_All_Stored_Positions, Anchor_ID => 0));
         else
            declare
               Slot : constant Gcode_Arguments.Argument_Integer := D.Value;
            begin
               Queue_Stored_Position_Update
                 (Planner, (Kind => Delete_Stored_Position, Anchor_ID => 0, Deleted_Slot => Slot));
            end;
         end if;
      end Execute_Delete_Stored_Position;

      procedure Execute_Return_To_Saved_Position
        (Planner : Planner_Interface'Class;
         F       : Gcode_Optional_Float;
         S       : Gcode_Optional_Integer_Or_No_Value;
         X       : Gcode_Optional_Float_Or_No_Value;
         Y       : Gcode_Optional_Float_Or_No_Value;
         Z       : Gcode_Optional_Float_Or_No_Value;
         E       : Gcode_Optional_Float_Or_No_Value)
      is
         Slot              : constant Gcode_Arguments.Argument_Integer :=
           (case S.Kind is
              when Gcode_Value_Not_Present | Gcode_No_Value_Present => 0,
              when Gcode_Value_Present                              => S.Value);
         Any_Axis          : constant Boolean :=
           X.Kind /= Gcode_Value_Not_Present
           or else Y.Kind /= Gcode_Value_Not_Present
           or else Z.Kind /= Gcode_Value_Not_Present
           or else E.Kind /= Gcode_Value_Not_Present;
         Physical_Position : Position := Planner.Get_Last_Position;
         Logical_Position  : Position :=
           Logical_Position_From_Physical (Physical_Position, Planned_State.G92_Offset, Planned_State.Current_Z_Hop);
         Target_Physical   : Position;
         Restore_Feedrate  : Velocity := Planned_State.Feedrate * Planned_State.Feedrate_Scale;

         procedure Restore_Axis (Axis : Axis_Name; Arg : Gcode_Optional_Float_Or_No_Value);

         procedure Restore_Axis (Axis : Axis_Name; Arg : Gcode_Optional_Float_Or_No_Value) is
            Offset : constant Length := Optional_Float_Length (Arg, Planned_State.Units);
         begin
            if Axis = E_Axis then
               Logical_Position (E_Axis) := Planned_Stored_Positions (Slot).Pos (E_Axis) + Offset;
               Planned_State.G92_Offset (E_Axis) := Logical_Position (E_Axis) - Physical_Position (E_Axis);
            else
               Target_Physical (Axis) := Planned_Stored_Positions (Slot).Pos (Axis) + Offset;
            end if;
         end Restore_Axis;
      begin
         if not Planned_Stored_Positions (Slot).Present then
            raise Gcode_Bad_Inputs_Error with "Saved position slot " & Slot'Image & " does not contain a position.";
         end if;

         if F.Present then
            Restore_Feedrate := To_Current_Units_Feedrate (F.Value, Planned_State.Units);
            if Restore_Feedrate <= 0.0 * mm / min then
               raise Gcode_Bad_Inputs_Error with "F feedrate must be greater than zero.";
            end if;
         end if;

         Target_Physical := Physical_Position;

         if Any_Axis then
            if X.Kind /= Gcode_Value_Not_Present then
               Restore_Axis (X_Axis, X);
            end if;
            if Y.Kind /= Gcode_Value_Not_Present then
               Restore_Axis (Y_Axis, Y);
            end if;
            if Z.Kind /= Gcode_Value_Not_Present then
               Restore_Axis (Z_Axis, Z);
            end if;
            if E.Kind /= Gcode_Value_Not_Present then
               Restore_Axis (E_Axis, E);
            end if;
         else
            Restore_Axis (X_Axis, (Kind => Gcode_No_Value_Present));
            Restore_Axis (Y_Axis, (Kind => Gcode_No_Value_Present));
            Restore_Axis (Z_Axis, (Kind => Gcode_No_Value_Present));
            Restore_Axis (E_Axis, (Kind => Gcode_No_Value_Present));
         end if;

         Target_Physical (Z_Axis) := Target_Physical (Z_Axis) + Planned_State.Current_Z_Hop;

         Ensure_Can_Queue_Planned_State (Planner);

         Add_Corner_If_Moved (Planner, Physical_Position, Target_Physical, Restore_Feedrate);

         Planned_State.G92_Offset (E_Axis) := Logical_Position (E_Axis) - Physical_Position (E_Axis);
         Maybe_Queue_Planned_State (Planner);
      end Execute_Return_To_Saved_Position;

      procedure Set_Absolute_Positioning (Planner : Planner_Interface'Class) is
      begin
         Planned_State.Positioning := Absolute_Positioning_Mode;
         Planned_State.E_Positioning := Follow_XYZ_Positioning_Mode;
         Maybe_Queue_Planned_State (Planner);
      end Set_Absolute_Positioning;

      procedure Set_Relative_Positioning (Planner : Planner_Interface'Class) is
      begin
         Planned_State.Positioning := Relative_Positioning_Mode;
         Planned_State.E_Positioning := Follow_XYZ_Positioning_Mode;
         Maybe_Queue_Planned_State (Planner);
      end Set_Relative_Positioning;

      procedure Set_Virtual_Position_State
        (Planner : Planner_Interface'Class;
         X       : Gcode_Optional_Float;
         Y       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float;
         E       : Gcode_Optional_Float)
      is
         Physical_Position : constant Position := Planner.Get_Last_Position;

         procedure Set_Axis (Axis : Axis_Name; Arg : Gcode_Optional_Float);

         procedure Set_Axis (Axis : Axis_Name; Arg : Gcode_Optional_Float) is
         begin
            if Arg.Present then
               Planned_State.G92_Offset (Axis) :=
                 To_Current_Units_Length (Arg.Value, Planned_State.Units) - Physical_Position (Axis);
               if Axis = Z_Axis then
                  Planned_State.G92_Offset (Axis) := Planned_State.G92_Offset (Axis) + Planned_State.Current_Z_Hop;
               end if;
            end if;
         end Set_Axis;
      begin
         Set_Axis (X_Axis, X);
         Set_Axis (Y_Axis, Y);
         Set_Axis (Z_Axis, Z);
         Set_Axis (E_Axis, E);
         Maybe_Queue_Planned_State (Planner);
      end Set_Virtual_Position_State;

      procedure Set_E_Axis_Absolute (Planner : Planner_Interface'Class) is
      begin
         Planned_State.E_Positioning := Absolute_E_Positioning_Mode;
         Maybe_Queue_Planned_State (Planner);
      end Set_E_Axis_Absolute;

      procedure Set_E_Axis_Relative (Planner : Planner_Interface'Class) is
      begin
         Planned_State.E_Positioning := Relative_E_Positioning_Mode;
         Maybe_Queue_Planned_State (Planner);
      end Set_E_Axis_Relative;

      procedure Apply_Retraction_Settings
        (Planner : Planner_Interface'Class;
         F       : Gcode_Optional_Float;
         S       : Gcode_Optional_Float;
         Z       : Gcode_Optional_Float) is
      begin
         if F.Present then
            declare
               New_Retract_Feedrate : constant Velocity := To_Current_Units_Feedrate (F.Value, Planned_State.Units);
            begin
               if New_Retract_Feedrate <= 0.0 * mm / min then
                  raise Gcode_Bad_Inputs_Error with "F retract feedrate must be greater than zero.";
               end if;
               Planned_State.Retract_Feedrate := New_Retract_Feedrate;
            end;
         end if;
         if S.Present then
            declare
               New_Retract_Length : constant Length := To_Current_Units_Length (S.Value, Planned_State.Units);
            begin
               if New_Retract_Length < 0.0 * mm then
                  raise Gcode_Bad_Inputs_Error with "S retract length must be greater than or equal to zero.";
               end if;
               Planned_State.Retract_Length := New_Retract_Length;
            end;
         end if;
         if Z.Present then
            declare
               New_Retract_Z_Lift : constant Length := To_Current_Units_Length (Z.Value, Planned_State.Units);
            begin
               if New_Retract_Z_Lift < 0.0 * mm then
                  raise Gcode_Bad_Inputs_Error with "Z lift must be greater than or equal to zero.";
               end if;
               Planned_State.Retract_Z_Lift := New_Retract_Z_Lift;
            end;
         end if;
         Maybe_Queue_Planned_State (Planner);
      end Apply_Retraction_Settings;

      procedure Apply_Recover_Settings
        (Planner : Planner_Interface'Class; F : Gcode_Optional_Float; S : Gcode_Optional_Float) is
      begin
         if F.Present then
            declare
               New_Recover_Feedrate : constant Velocity := To_Current_Units_Feedrate (F.Value, Planned_State.Units);
            begin
               if New_Recover_Feedrate <= 0.0 * mm / min then
                  raise Gcode_Bad_Inputs_Error with "F recover feedrate must be greater than zero.";
               end if;
               Planned_State.Recover_Feedrate := New_Recover_Feedrate;
            end;
         end if;
         if S.Present then
            Planned_State.Recover_Extra_Length := To_Current_Units_Length (S.Value, Planned_State.Units);
         end if;
         Maybe_Queue_Planned_State (Planner);
      end Apply_Recover_Settings;

      procedure Set_Auto_Retract_State (Planner : Planner_Interface'Class; S : Gcode_Arguments.Argument_Integer) is
      begin
         Planned_State.Auto_Retract_Enabled := S /= 0;
         Maybe_Queue_Planned_State (Planner);
      end Set_Auto_Retract_State;

      procedure Apply_Set_Feedrate_Percentage (Planner : Planner_Interface'Class; S : Dimensionless) is
      begin
         if S <= 0.0 then
            raise Gcode_Bad_Inputs_Error with "S feedrate percentage must be greater than zero.";
         end if;
         Planned_State.Feedrate_Scale := S / 100.0;
         Maybe_Queue_Planned_State (Planner);
      end Apply_Set_Feedrate_Percentage;

      procedure Apply_Backup_Feedrate_Percentage (Planner : Planner_Interface'Class) is
      begin
         Planned_State.Backup_Feedrate_Scale := Planned_State.Feedrate_Scale;
         Maybe_Queue_Planned_State (Planner);
      end Apply_Backup_Feedrate_Percentage;

      procedure Apply_Restore_Feedrate_Percentage (Planner : Planner_Interface'Class) is
      begin
         Planned_State.Feedrate_Scale := Planned_State.Backup_Feedrate_Scale;
         Maybe_Queue_Planned_State (Planner);
      end Apply_Restore_Feedrate_Percentage;

      procedure Apply_Set_Flow_Percentage (Planner : Planner_Interface'Class; S : Dimensionless) is
      begin
         if S <= 0.0 then
            raise Gcode_Bad_Inputs_Error with "Flow percentage must be greater than zero.";
         end if;
         Planned_State.Flow_Scale := S / 100.0;
         Maybe_Queue_Planned_State (Planner);
      end Apply_Set_Flow_Percentage;

      function Stored_Positions_Report return Virtual_String is
         Result : Virtual_String := "";
         Any    : Boolean := False;
      begin
         for Slot in Gcode_Arguments.Argument_Integer loop
            if Planned_Stored_Positions (Slot).Present then
               if Any then
                  Result.Append (VSS.Characters.Latin.Line_Feed);
               end if;
               Result.Append
                 (Position_Report
                    ("Slot " & Slot'Image & ": ", Planned_Stored_Positions (Slot).Pos, Planned_State.Units));
               Any := True;
            end if;
         end loop;

         if Any then
            return Result;
         else
            return "No saved positions.";
         end if;
      end Stored_Positions_Report;

      function Retraction_Settings_Report return Virtual_String is
         Scale : constant Length := Unit_Scale (Planned_State.Units);
         Unit  : constant String :=
           (case Planned_State.Units is
              when Millimeter_Units_Mode => "mm",
              when Inch_Units_Mode       => "in");
      begin
         return
           +("M207: S = "
             & Trimmed_Image (Planned_State.Retract_Length / Scale)
             & " "
             & Unit
             & ", F = "
             & Trimmed_Image (Planned_State.Retract_Feedrate / (Scale / min))
             & " "
             & Unit
             & "/min, Z = "
             & Trimmed_Image (Planned_State.Retract_Z_Lift / Scale)
             & " "
             & Unit);
      end Retraction_Settings_Report;

      function Recover_Settings_Report return Virtual_String is
         Scale : constant Length := Unit_Scale (Planned_State.Units);
         Unit  : constant String :=
           (case Planned_State.Units is
              when Millimeter_Units_Mode => "mm",
              when Inch_Units_Mode       => "in");
      begin
         return
           +("M208: S = "
             & Trimmed_Image (Planned_State.Recover_Extra_Length / Scale)
             & " "
             & Unit
             & ", F = "
             & Trimmed_Image (Planned_State.Recover_Feedrate / (Scale / min))
             & " "
             & Unit
             & "/min");
      end Recover_Settings_Report;

      function Auto_Retract_Report return Virtual_String is
      begin
         return +("M209: S = " & (if Planned_State.Auto_Retract_Enabled then "1" else "0"));
      end Auto_Retract_Report;

      function Feedrate_Scale_Report return Virtual_String is
      begin
         return +("M220: S = " & Trimmed_Image (Planned_State.Feedrate_Scale * 100.0) & "%");
      end Feedrate_Scale_Report;

      function Flow_Scale_Report return Virtual_String is
      begin
         return +("M221: S = " & Trimmed_Image (Planned_State.Flow_Scale * 100.0) & "%");
      end Flow_Scale_Report;

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

         Update_Status (Status_Emitter, Committed_State);
      end Handle_Resume;

   end Module_Instance;

   procedure Rapid_Linear_Move
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_Float;
      Y        : Gcode_Optional_Float;
      Z        : Gcode_Optional_Float;
      E        : Gcode_Optional_Float;
      F        : Gcode_Optional_Float)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Execute_Linear_Move
        (Planner => Planner, X => X, Y => Y, Z => Z, E => E, F => F, Rapid => True);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Rapid_Linear_Move;

   procedure Linear_Move
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_Float;
      Y        : Gcode_Optional_Float;
      Z        : Gcode_Optional_Float;
      E        : Gcode_Optional_Float;
      F        : Gcode_Optional_Float)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Execute_Linear_Move
        (Planner => Planner, X => X, Y => Y, Z => Z, E => E, F => F, Rapid => False);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
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
      J       : Dimensionless)
   is
      pragma Unreferenced (This, Planner, X, Y, Z, E, F, I, J);
   begin
      raise Gcode_Bad_Inputs_Error with "G2 arcs and helices are not implemented yet.";
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Clockwise_Arc_Move_Offset_Form;

   procedure Clockwise_Arc_Move_Radius_Form
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float;
      R       : Dimensionless)
   is
      pragma Unreferenced (This, Planner, X, Y, Z, E, F, R);
   begin
      raise Gcode_Bad_Inputs_Error with "G2 arcs and helices are not implemented yet.";
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
      J       : Dimensionless)
   is
      pragma Unreferenced (This, Planner, X, Y, Z, E, F, I, J);
   begin
      raise Gcode_Bad_Inputs_Error with "G3 arcs and helices are not implemented yet.";
   end Counter_Clockwise_Arc_Move_Offset_Form;

   procedure Counter_Clockwise_Arc_Move_Radius_Form
     (This    : Module_Instance;
      Planner : Planner_Interface'Class;
      X       : Gcode_Optional_Float;
      Y       : Gcode_Optional_Float;
      Z       : Gcode_Optional_Float;
      E       : Gcode_Optional_Float;
      F       : Gcode_Optional_Float;
      R       : Dimensionless)
   is
      pragma Unreferenced (This, Planner, X, Y, Z, E, F, R);
   begin
      raise Gcode_Bad_Inputs_Error with "G3 arcs and helices are not implemented yet.";
   end Counter_Clockwise_Arc_Move_Radius_Form;

   procedure Retract
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Gcode_Optional_Integer)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Execute_Retract (Planner => Planner, S => S);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Retract;

   procedure Recover
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Execute_Recover (Planner);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Recover;

   procedure Inch_Units
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Set_Inch_Units (Planner);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Inch_Units;

   procedure Millimeter_Units
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Set_Millimeter_Units (Planner);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Millimeter_Units;

   procedure Save_Current_Position
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Gcode_Integer_Or_No_Value)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Execute_Save_Current_Position (Planner => Planner, S => S);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Save_Current_Position;

   procedure Delete_Stored_Position
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      D        : Gcode_Integer_Or_No_Value)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Execute_Delete_Stored_Position (Planner => Planner, D => D);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Delete_Stored_Position;

   procedure Move_To_Stored_Position
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      Q        : Gcode_Integer_Or_No_Value;
      F        : Gcode_Optional_Float;
      X        : Gcode_Optional_Float_Or_No_Value;
      Y        : Gcode_Optional_Float_Or_No_Value;
      Z        : Gcode_Optional_Float_Or_No_Value;
      E        : Gcode_Optional_Float_Or_No_Value)
   is
      S : Gcode_Optional_Integer_Or_No_Value := (Kind => Gcode_Value_Present, Value => 0);
   begin
      if Q.Kind = Gcode_Value_Present then
         S.Value := Q.Value;
      end if;

      Return_To_Saved_Position
        (This => This, Self_Ref => Self_Ref, Planner => Planner, F => F, S => S, X => X, Y => Y, Z => Z, E => E);
   end Move_To_Stored_Position;

   procedure Report_Stored_Positions
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   is
      pragma Unreferenced (This);
   begin
      Planner.Flush
        (Motion_Report_Event'(Message => Module_Instance (Self_Ref.Get.Element.all).Stored_Positions_Report));
   end Report_Stored_Positions;

   procedure Return_To_Saved_Position
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      F        : Gcode_Optional_Float;
      S        : Gcode_Optional_Integer_Or_No_Value;
      X        : Gcode_Optional_Float_Or_No_Value;
      Y        : Gcode_Optional_Float_Or_No_Value;
      Z        : Gcode_Optional_Float_Or_No_Value;
      E        : Gcode_Optional_Float_Or_No_Value)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Execute_Return_To_Saved_Position
        (Planner => Planner, F => F, S => S, X => X, Y => Y, Z => Z, E => E);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Return_To_Saved_Position;

   procedure Absolute_Positioning
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Set_Absolute_Positioning (Planner);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Absolute_Positioning;

   procedure Relative_Positioning
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Set_Relative_Positioning (Planner);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Relative_Positioning;

   procedure Set_Virtual_Position
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      X        : Gcode_Optional_Float;
      Y        : Gcode_Optional_Float;
      Z        : Gcode_Optional_Float;
      E        : Gcode_Optional_Float)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Set_Virtual_Position_State
        (Planner => Planner, X => X, Y => Y, Z => Z, E => E);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Set_Virtual_Position;

   procedure E_Axis_Absolute
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Set_E_Axis_Absolute (Planner);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end E_Axis_Absolute;

   procedure E_Axis_Relative
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Set_E_Axis_Relative (Planner);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end E_Axis_Relative;

   procedure Retraction_Settings
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      F        : Gcode_Optional_Float;
      S        : Gcode_Optional_Float;
      Z        : Gcode_Optional_Float)
   is
      pragma Unreferenced (This);
   begin
      if not F.Present and then not S.Present and then not Z.Present then
         Planner.Flush
           (Motion_Report_Event'(Message => Module_Instance (Self_Ref.Get.Element.all).Retraction_Settings_Report));
      else
         Module_Instance (Self_Ref.Get.Element.all).Apply_Retraction_Settings
           (Planner => Planner, F => F, S => S, Z => Z);
         --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
         --  planner. Other modules should normally not do this.
      end if;
   end Retraction_Settings;

   procedure Recover_Settings
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      F        : Gcode_Optional_Float;
      S        : Gcode_Optional_Float)
   is
      pragma Unreferenced (This);
   begin
      if not F.Present and then not S.Present then
         Planner.Flush
           (Motion_Report_Event'(Message => Module_Instance (Self_Ref.Get.Element.all).Recover_Settings_Report));
      else
         Module_Instance (Self_Ref.Get.Element.all).Apply_Recover_Settings (Planner => Planner, F => F, S => S);
         --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
         --  planner. Other modules should normally not do this.
      end if;
   end Recover_Settings;

   procedure Set_Auto_Retract
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Gcode_Optional_Integer)
   is
      pragma Unreferenced (This);
   begin
      if S.Present then
         Module_Instance (Self_Ref.Get.Element.all).Set_Auto_Retract_State (Planner => Planner, S => S.Value);
      else
         Planner.Flush
           (Motion_Report_Event'(Message => Module_Instance (Self_Ref.Get.Element.all).Auto_Retract_Report));
      end if;
   end Set_Auto_Retract;

   procedure Set_Feedrate_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Dimensionless)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Apply_Set_Feedrate_Percentage (Planner => Planner, S => S);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Set_Feedrate_Percentage;

   procedure Backup_Feedrate_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      B        : Gcode_No_Value)
   is
      pragma Unreferenced (This, B);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Apply_Backup_Feedrate_Percentage (Planner);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Backup_Feedrate_Percentage;

   procedure Restore_Feedrate_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      R        : Gcode_No_Value)
   is
      pragma Unreferenced (This, R);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Apply_Restore_Feedrate_Percentage (Planner);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Restore_Feedrate_Percentage;

   procedure Report_Feedrate_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   is
      pragma Unreferenced (This);
   begin
      Planner.Flush
        (Motion_Report_Event'(Message => Module_Instance (Self_Ref.Get.Element.all).Feedrate_Scale_Report));
   end Report_Feedrate_Percentage;

   procedure Set_Flow_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      S        : Dimensionless)
   is
      pragma Unreferenced (This);
   begin
      Module_Instance (Self_Ref.Get.Element.all).Apply_Set_Flow_Percentage (Planner => Planner, S => S);
      --  We need to bypass the usual enqueue/execute separation since we have state that needs to be fed into the
      --  planner. Other modules should normally not do this.
   end Set_Flow_Percentage;

   procedure Report_Flow_Percentage
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class)
   is
      pragma Unreferenced (This);
   begin
      Planner.Flush (Motion_Report_Event'(Message => Module_Instance (Self_Ref.Get.Element.all).Flow_Scale_Report));
   end Report_Flow_Percentage;

end Prunt.Default_Modules.Motion;
