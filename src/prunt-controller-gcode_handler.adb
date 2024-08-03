-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
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

with Prunt.Gcode_Parser; use Prunt.Gcode_Parser;
with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Exceptions;
with Prunt.TMC_Types.TMC2240;
with Prunt.Heaters;      use Prunt.Heaters;
with Prunt.Logger;
with Ada.IO_Exceptions;

use type Prunt.TMC_Types.TMC2240.UART_CRC;

package body Prunt.Controller.Gcode_Handler is

   procedure Try_Set_File (Path : String; Succeeded : out Boolean) is
   begin
      Gcode_Queue.Try_Set_File (Path, Succeeded);
   end Try_Set_File;

   procedure Try_Queue_Command (Command : String; Succeeded : out Boolean) is
   begin
      Gcode_Queue.Try_Set_Command (Command, Succeeded);
   end Try_Queue_Command;

   task body Runner is
      Corner_Data : Corner_Extra_Data := (Fans => (others => 0.0), Heaters => (others => Temperature (0.0)));

      --  TODO: Track down GNAT bug that stops [others => 0.0 * mm] from working specifically in this file.
      Zero_Pos : constant Position := [X_Axis => 0.0 * mm, Y_Axis => 0.0 * mm, Z_Axis => 0.0 * mm, E_Axis => 0.0 * mm];
      Zero_Pos_Offset : constant Position_Offset :=
        [X_Axis => 0.0 * mm, Y_Axis => 0.0 * mm, Z_Axis => 0.0 * mm, E_Axis => 0.0 * mm];

      Parser_Context : Gcode_Parser.Context := Make_Context (Zero_Pos, 100.0 * mm / s);

      Is_Homed : array (Axis_Name) of Boolean := [others => False];

      Kinematics_Params        : My_Config.Kinematics_Parameters;
      Axial_Homing_Params      : array (Axis_Name) of My_Config.Homing_Parameters;
      Switchwise_Switch_Params : array (Generic_Types.Input_Switch_Name) of My_Config.Input_Switch_Parameters;

      G_Code_Assignment_Params : My_Config.G_Code_Assignment_Parameters;

      Command_Constraint_Error : exception;

      procedure Double_Tap_Home_Axis (Axis : Axis_Name; Pos_After : in out Position) is
         Switch          : constant Generic_Types.Input_Switch_Name := Axial_Homing_Params (Axis).Switch;
         Hit_State       : constant Pin_State :=
           (if Switchwise_Switch_Params (Axial_Homing_Params (Axis).Switch).Hit_On_High then High_State
            else Low_State);
         First_Offset    : constant Position_Offset :=
           [Zero_Pos_Offset with delta Axis => Axial_Homing_Params (Axis).First_Move_Distance];
         Second_Offset   : constant Position_Offset :=
           [Zero_Pos_Offset with delta Axis => Axial_Homing_Params (Axis).Second_Move_Distance];
         Back_Off_Offset : constant Position_Offset :=
           [Zero_Pos_Offset with delta Axis => Axial_Homing_Params (Axis).Back_Off_Move_Distance];
      begin
         My_Planner.Enqueue
           ((Kind             => My_Planner.Flush_And_Reset_Position_Kind,
             Flush_Extra_Data => (others => <>),
             Reset_Pos        => Zero_Pos),
            Ignore_Bounds => True);

         My_Planner.Enqueue
           ((Kind              => My_Planner.Move_Kind,
             Pos               => Zero_Pos + Back_Off_Offset,
             Feedrate          => Velocity'Last,
             Corner_Extra_Data => Corner_Data),
            Ignore_Bounds => True);
         My_Planner.Enqueue
           ((Kind             => My_Planner.Flush_And_Reset_Position_Kind,
             Flush_Extra_Data =>
               (Is_Conditional_Move      => True,
                Conditional_Switch       => Switch,
                Conditional_Hit_On_State => (if Hit_State = High_State then Low_State else High_State),
                others                   => <>),
             Reset_Pos        => Zero_Pos),
            Ignore_Bounds => True);

         My_Planner.Enqueue
           ((Kind              => My_Planner.Move_Kind,
             Pos               => Zero_Pos + First_Offset,
             Feedrate          => Velocity'Last,
             Corner_Extra_Data => Corner_Data),
            Ignore_Bounds => True);
         My_Planner.Enqueue
           ((Kind             => My_Planner.Flush_And_Reset_Position_Kind,
             Flush_Extra_Data =>
               (Is_Homing_Move => True, Home_Switch => Switch, Home_Hit_On_State => Hit_State, others => <>),
             Reset_Pos        => Zero_Pos),
            Ignore_Bounds => True);
         My_Planner.Enqueue
           ((Kind              => My_Planner.Move_Kind,
             Pos               => Zero_Pos + Back_Off_Offset,
             Feedrate          => Velocity'Last,
             Corner_Extra_Data => Corner_Data),
            Ignore_Bounds => True);
         My_Planner.Enqueue
           ((Kind             => My_Planner.Flush_And_Reset_Position_Kind,
             Flush_Extra_Data => (others => <>),
             Reset_Pos        => Zero_Pos),
            Ignore_Bounds => True);

         declare
            Data            : Flush_Extra_Data;
            First_Seg_Accel : Length;
         begin
            Finished_Block_Queue.Pop (Data, First_Seg_Accel);
            --  We do not care what about happened during the homing move at this point as long as
            --  the switch is not hit.
         end;

         My_Planner.Enqueue
           ((Kind              => My_Planner.Move_Kind,
             Pos               => Zero_Pos + Second_Offset,
             Feedrate          => Velocity'Last,
             Corner_Extra_Data => Corner_Data),
            Ignore_Bounds => True);
         My_Planner.Enqueue
           ((Kind             => My_Planner.Flush_And_Reset_Position_Kind,
             Flush_Extra_Data =>
               (Is_Homing_Move => True, Home_Switch => Switch, Home_Hit_On_State => Hit_State, others => <>),
             Reset_Pos        => Zero_Pos),
            Ignore_Bounds => True);

         declare
            Data            : Flush_Extra_Data;
            First_Seg_Accel : Length;
         begin
            Finished_Block_Queue.Pop (Data, First_Seg_Accel);

            if Axial_Homing_Params (Axis).Second_Move_Distance < 0.0 * mm then
               First_Seg_Accel := -First_Seg_Accel;
            end if;

            Pos_After (Axis) :=
              Axial_Homing_Params (Axis).Switch_Position + Axial_Homing_Params (Axis).Second_Move_Distance -
              First_Seg_Accel;
         end;

         My_Planner.Enqueue
           ((Kind             => My_Planner.Flush_And_Reset_Position_Kind,
             Flush_Extra_Data => (others => <>),
             Reset_Pos        => Pos_After),
            Ignore_Bounds => True);

         Pos_After := Pos_After + Back_Off_Offset;

         if Pos_After (Axis) < Kinematics_Params.Planner_Parameters.Lower_Pos_Limit (Axis) then
            Pos_After (Axis) := Kinematics_Params.Planner_Parameters.Lower_Pos_Limit (Axis);
         elsif Pos_After (Axis) > Kinematics_Params.Planner_Parameters.Upper_Pos_Limit (Axis) then
            Pos_After (Axis) := Kinematics_Params.Planner_Parameters.Upper_Pos_Limit (Axis);
         end if;

         My_Planner.Enqueue
           ((Kind              => My_Planner.Move_Kind,
             Pos               => Pos_After,
             Feedrate          => Velocity'Last,
             Corner_Extra_Data => Corner_Data),
            Ignore_Bounds => True);
         My_Planner.Enqueue
           ((Kind             => My_Planner.Flush_And_Reset_Position_Kind,
             Flush_Extra_Data => (others => <>),
             Reset_Pos        => Pos_After),
            Ignore_Bounds => True);
         Gcode_Parser.Reset_Position (Parser_Context, Pos_After);
      end Double_Tap_Home_Axis;

      procedure Run_Command (Command : Gcode_Parser.Command) is
      begin
         case Command.Kind is
            when None_Kind =>
               null;
            when Pause_Kind =>
               My_Planner.Enqueue
                 ((Kind             => My_Planner.Flush_Kind,
                   Flush_Extra_Data => (Pause_After => True, others => <>)));
            when Move_Kind =>
               if Command.Pos /= Command.Old_Pos then
                  for I in Axis_Name loop
                     if not Is_Homed (I) then
                        raise Command_Constraint_Error
                          with "Must home all axes before moving. Axis " & I'Image & " is not homed.";
                     end if;
                  end loop;

                  My_Planner.Enqueue
                    ((Kind              => My_Planner.Move_Kind,
                      Pos               => Command.Pos,
                      Feedrate          => Command.Feedrate,
                      Corner_Extra_Data => Corner_Data));
               end if;
            when Dwell_Kind =>
               My_Planner.Enqueue
                 ((Kind             => My_Planner.Flush_Kind,
                   Flush_Extra_Data => (Dwell_Time => Command.Dwell_Time, others => <>)));
            when Home_Kind =>
               declare
                  Pos_After    : Position                                := Command.Pos_Before;
                  Homing_Order : constant array (Axis_Name) of Axis_Name := [E_Axis, Z_Axis, X_Axis, Y_Axis];
               begin
                  for Axis of Homing_Order loop
                     if Command.Axes (Axis) then
                        case Axial_Homing_Params (Axis).Kind is
                           when My_Config.Set_To_Value_Kind =>
                              Pos_After (Axis) := Axial_Homing_Params (Axis).Value;
                              My_Planner.Enqueue
                                ((Kind             => My_Planner.Flush_And_Reset_Position_Kind,
                                  Reset_Pos        => Pos_After,
                                  Flush_Extra_Data => (others => <>)));
                              Gcode_Parser.Reset_Position (Parser_Context, Pos_After);
                           when My_Config.Double_Tap_Kind =>
                              Double_Tap_Home_Axis (Axis, Pos_After);
                              if Axis = Z_Axis then
                                 Pos_After (Z_Axis) := 5.0 * mm; --  TODO: Replace with a config option.
                                 My_Planner.Enqueue
                                   ((Kind              => My_Planner.Move_Kind,
                                     Pos               => Pos_After,
                                     Feedrate          => 10.0 * mm / s,
                                     Corner_Extra_Data => Corner_Data));
                                 My_Planner.Enqueue
                                   ((Kind => My_Planner.Flush_Kind, Flush_Extra_Data => (others => <>)));
                                 Gcode_Parser.Reset_Position (Parser_Context, Pos_After);
                              end if;
                        end case;
                        Is_Homed (Axis) := True;
                     end if;
                  end loop;
               exception
                  when others =>
                     --  If homing fails for any reason then all axes become unhomed.
                     Is_Homed := [others => False];
                     raise;
               end;
            when Enable_Steppers_Kind =>
               for S in Generic_Types.Stepper_Name loop
                  case Kinematics_Params.Kind is
                     when My_Config.Core_XY_Kind =>
                        if (Command.Axes (X_Axis) or Command.Axes (Y_Axis)) and
                          (Kinematics_Params.A_Steppers (S) or Kinematics_Params.B_Steppers (S))
                        then
                           Stepper_Hardware (S).Enable_Stepper (S);
                        end if;
                     when My_Config.Cartesian_Kind =>
                        if Command.Axes (X_Axis) and Kinematics_Params.X_Steppers (S) then
                           Stepper_Hardware (S).Enable_Stepper (S);
                        end if;
                        if Command.Axes (Y_Axis) and Kinematics_Params.Y_Steppers (S) then
                           Stepper_Hardware (S).Enable_Stepper (S);
                        end if;
                  end case;
                  if Command.Axes (E_Axis) and Kinematics_Params.E_Steppers (S) then
                     Stepper_Hardware (S).Enable_Stepper (S);
                  end if;
                  if Command.Axes (Z_Axis) and Kinematics_Params.Z_Steppers (S) then
                     Stepper_Hardware (S).Enable_Stepper (S);
                  end if;
               end loop;
            when Disable_Steppers_Kind =>
               for A in Axis_Name loop
                  if Command.Axes (A) then
                     Is_Homed (A) := False;
                  end if;
               end loop;

               for S in Generic_Types.Stepper_Name loop
                  case Kinematics_Params.Kind is
                     when My_Config.Core_XY_Kind =>
                        if (Command.Axes (X_Axis) or Command.Axes (Y_Axis)) and
                          (Kinematics_Params.A_Steppers (S) or Kinematics_Params.B_Steppers (S))
                        then
                           Stepper_Hardware (S).Disable_Stepper (S);
                        end if;
                     when My_Config.Cartesian_Kind =>
                        if Command.Axes (X_Axis) and Kinematics_Params.X_Steppers (S) then
                           Stepper_Hardware (S).Disable_Stepper (S);
                        end if;
                        if Command.Axes (Y_Axis) and Kinematics_Params.Y_Steppers (S) then
                           Stepper_Hardware (S).Disable_Stepper (S);
                        end if;
                  end case;
                  if Command.Axes (E_Axis) and Kinematics_Params.E_Steppers (S) then
                     Stepper_Hardware (S).Disable_Stepper (S);
                  end if;
                  if Command.Axes (Z_Axis) and Kinematics_Params.Z_Steppers (S) then
                     Stepper_Hardware (S).Disable_Stepper (S);
                  end if;
               end loop;
            when Set_Hotend_Temperature_Kind =>
               Corner_Data.Heaters (G_Code_Assignment_Params.Hotend_Heater) := Command.Target_Temperature;
               My_Planner.Enqueue
                 ((Kind              => My_Planner.Move_Kind,
                   Pos               => Command.Pos,
                   Feedrate          => 0.000_1 * mm / s,
                   Corner_Extra_Data => Corner_Data));
               My_Planner.Enqueue ((Kind => My_Planner.Flush_Kind, Flush_Extra_Data => (others => <>)));
            when Wait_Hotend_Temperature_Kind =>
               Corner_Data.Heaters (G_Code_Assignment_Params.Hotend_Heater) := Command.Target_Temperature;
               My_Planner.Enqueue
                 ((Kind              => My_Planner.Move_Kind,
                   Pos               => Command.Pos,
                   Feedrate          => 0.000_1 * mm / s,
                   Corner_Extra_Data => Corner_Data));
               My_Planner.Enqueue
                 ((Kind             => My_Planner.Flush_Kind,
                   Flush_Extra_Data =>
                     (Wait_For_Heater      => True,
                      Wait_For_Heater_Name => G_Code_Assignment_Params.Hotend_Heater,
                      others               => <>)));
            when Set_Bed_Temperature_Kind =>
               Corner_Data.Heaters (G_Code_Assignment_Params.Bed_Heater) := Command.Target_Temperature;
               My_Planner.Enqueue
                 ((Kind              => My_Planner.Move_Kind,
                   Pos               => Command.Pos,
                   Feedrate          => 0.000_1 * mm / s,
                   Corner_Extra_Data => Corner_Data));
               My_Planner.Enqueue ((Kind => My_Planner.Flush_Kind, Flush_Extra_Data => (others => <>)));
            when Wait_Bed_Temperature_Kind =>
               Corner_Data.Heaters (G_Code_Assignment_Params.Bed_Heater) := Command.Target_Temperature;
               My_Planner.Enqueue
                 ((Kind              => My_Planner.Move_Kind,
                   Pos               => Command.Pos,
                   Feedrate          => 0.000_1 * mm / s,
                   Corner_Extra_Data => Corner_Data));
               My_Planner.Enqueue
                 ((Kind             => My_Planner.Flush_Kind,
                   Flush_Extra_Data =>
                     (Wait_For_Heater      => True,
                      Wait_For_Heater_Name => G_Code_Assignment_Params.Bed_Heater,
                      others               => <>)));
            when TMC_Dump_Kind =>
               for S in Generic_Types.Stepper_Name loop
                  if Stepper_Hardware (S).Kind = TMC2240_UART_Kind then
                     Logger.Log ("TMC dump for " & S'Image & ":");
                     for R in TMC_Types.TMC2240.UART_Register_Address loop
                        declare
                           Query          : TMC_Types.TMC2240.UART_Query_Message :=
                             (Bytes_Mode => False,
                              Content    =>
                                (Node     => Stepper_Hardware (S).TMC2240_UART_Address,
                                 Register => R,
                                 others   => <>));
                           Reply          : TMC_Types.TMC2240.UART_Data_Message;
                           Receive_Failed : Boolean;
                        begin
                           Logger.Log (R'Image);
                           Query.Content.CRC := TMC_Types.TMC2240.Compute_CRC (Query);
                           Stepper_Hardware (S).TMC2240_UART_Read (Query.Bytes, Receive_Failed, Reply.Bytes);
                           if Receive_Failed then
                              Logger.Log ("No response.");
                           elsif Reply.Content.CRC /= TMC_Types.TMC2240.Compute_CRC (Reply) then
                              Logger.Log ("Bad CRC.");
                           else
                              Logger.Log (Reply.Content'Image);
                           end if;
                        end;
                     end loop;
                  end if;
               end loop;
            when Heater_Autotune_Kind =>
               Autotune_Heater
                 (G_Code_Assignment_Params.Hotend_Heater,
                  (Kind                   => PID_Autotune_Kind,
                   PID_Tuning_Temperature => Command.Tuning_Temperature,
                   others                 => <>));
               --  TODO: Take parameters.
            when others =>
               raise Constraint_Error with "Command not implemented.";
         end case;
      end Run_Command;
   begin
      accept Start (Initial_Data : Corner_Extra_Data) do
         Corner_Data := Initial_Data;

         My_Config.Config_File.Read (Kinematics_Params);
         My_Config.Config_File.Read (G_Code_Assignment_Params);

         for I in Generic_Types.Input_Switch_Name loop
            My_Config.Config_File.Read (Switchwise_Switch_Params (I), I);
         end loop;

         for I in Axis_Name loop
            My_Config.Config_File.Read (Axial_Homing_Params (I), I);
            case Axial_Homing_Params (I).Kind is
               when My_Config.Double_Tap_Kind =>
                  if Axial_Homing_Params (I).First_Move_Distance = 0.0 * mm or
                    Axial_Homing_Params (I).Second_Move_Distance = 0.0 * mm or
                    Axial_Homing_Params (I).Back_Off_Move_Distance = 0.0 * mm
                  then
                     raise Config_Constraint_Error
                       with "One or more homing distances on axis " & I'Image &
                       " are set to 0. This would cause homing to never complete.";
                  end if;

                  if Axial_Homing_Params (I).First_Move_Distance / Axial_Homing_Params (I).Second_Move_Distance < 0.0
                  then
                     raise Config_Constraint_Error
                       with "First and second homing distance on axis " & I'Image & " have different signs. " &
                       "This would cause the axis to move away from the switch forever after the first hit.";
                  end if;

                  if Axial_Homing_Params (I).First_Move_Distance / Axial_Homing_Params (I).Back_Off_Move_Distance > 0.0
                  then
                     raise Config_Constraint_Error
                       with "First homing distance and back-off distance on axis " & I'Image & " have same sign. " &
                       "This would cause axis to move further in to the switch after hitting it.";
                  end if;

                  if abs Axial_Homing_Params (I).First_Move_Distance < abs Axial_Homing_Params (I).Second_Move_Distance
                  then
                     raise Config_Constraint_Error
                       with "First homing distance on axis " & I'Image & " is smaller than second homing distance. " &
                       "The second distance should be smaller for more accurate homing.";
                  end if;

                  if not Switchwise_Switch_Params (Axial_Homing_Params (I).Switch).Enabled then
                     raise Config_Constraint_Error
                       with "Axis " & I'Image & " is configured to use switch " &
                       Axial_Homing_Params (I).Switch'Image & " for homing but the switch is not enabled.";
                  end if;
               when My_Config.Set_To_Value_Kind =>
                  null;
            end case;
         end loop;
      end Start;

      My_Planner.Enqueue
        ((Kind             => My_Planner.Flush_And_Reset_Position_Kind,
          Flush_Extra_Data => (others => <>),
          Reset_Pos        => Zero_Pos));
      My_Planner.Enqueue
        ((Kind              => My_Planner.Move_Kind,
          Pos               => Zero_Pos,
          Feedrate          => Velocity'Last,
          Corner_Extra_Data => Corner_Data));
      My_Planner.Enqueue
        ((Kind             => My_Planner.Flush_And_Reset_Position_Kind,
          Flush_Extra_Data => (others => <>),
          Reset_Pos        => Zero_Pos));

      loop
         delay 0.1;

         if Gcode_Queue.Get_Command /= "" then
            declare
               Line    : constant String := Gcode_Queue.Get_Command;
            begin
               Parse_Line (Parser_Context, Line, Run_Command'Unrestricted_Access);
               My_Planner.Enqueue ((Kind => My_Planner.Flush_Kind, Flush_Extra_Data => (others => <>)));
            exception
               when E : Command_Constraint_Error =>
                  Logger.Log
                    ("Error running manual command (" & Line & "): " & Ada.Exceptions.Exception_Information (E));
               when E : Bad_Line =>
                  Logger.Log
                    ("Error parsing manual command (" & Line & "): " & Ada.Exceptions.Exception_Information (E));
            end;

            Gcode_Queue.Clear_Command;
         elsif Gcode_Queue.Get_File /= "" then
            declare
               File : File_Type;
            begin
               Open (File, In_File, Gcode_Queue.Get_File);

               declare
                  type File_Line_Count is range 1 .. 2**63 - 1;
                  Current_Line      : constant File_Line_Count := 1;
                  Command_Succeeded : Boolean                  := True;
               begin
                  while Command_Succeeded and not End_Of_File (File) loop
                     declare
                        Line : constant String := Get_Line (File);
                     begin
                        Parse_Line (Parser_Context, Line, Run_Command'Unrestricted_Access);
                     exception
                        when E : Command_Constraint_Error =>
                           Logger.Log
                             ("Error running line in file " & Gcode_Queue.Get_File & " on line " &
                              Current_Line'Image & " (" & Line & "): " &
                              Ada.Exceptions.Exception_Information (E));
                           Command_Succeeded := False;
                        when E : Bad_Line                 =>
                           Logger.Log
                             ("Error parsing line in file " & Gcode_Queue.Get_File & " on line " &
                              Current_Line'Image & " (" & Line & "): " &
                              Ada.Exceptions.Exception_Information (E));
                           Command_Succeeded := False;
                     end;
                  end loop;
               end;

               My_Planner.Enqueue ((Kind => My_Planner.Flush_Kind, Flush_Extra_Data => (others => <>)));

               Close (File);
            exception
               when E : Ada.IO_Exceptions.Status_Error
                 | Ada.IO_Exceptions.Mode_Error
                 | Ada.IO_Exceptions.Name_Error
                 | Ada.IO_Exceptions.Use_Error
                 | Ada.IO_Exceptions.Device_Error
                 | Ada.IO_Exceptions.End_Error
                 | Ada.IO_Exceptions.Data_Error
                 | Ada.IO_Exceptions.Layout_Error =>
                  Logger.Log
                    ("IO error when processing file " & Gcode_Queue.Get_File & ": " &
                     Ada.Exceptions.Exception_Information (E));
            end;

            Gcode_Queue.Clear_File;
         end if;
      end loop;
   end Runner;

   protected body Gcode_Queue is
      procedure Try_Set_File (In_File : String; Succeeded : out Boolean) is
      begin
         if File /= "" then
            Succeeded := False;
            --  Other file already running.
         else
            Set_Unbounded_String (File, In_File);
            Succeeded := True;
         end if;
      end Try_Set_File;

      procedure Clear_File is
      begin
         Set_Unbounded_String (File, "");
      end Clear_File;

      function Get_File return String is
      begin
         return To_String (File);
      end Get_File;

      procedure Try_Set_Command (In_Command : String; Succeeded : out Boolean) is
      begin
         if Command /= "" then
            Succeeded := False;
            --  Other command already running.
         elsif File /= "" then
            Succeeded := False;
            --  File already running. Commands can not be run at same time.
         else
            Set_Unbounded_String (Command, In_Command);
            Succeeded := True;
         end if;
      end Try_Set_Command;

      procedure Clear_Command is
      begin
         Set_Unbounded_String (Command, "");
      end Clear_Command;

      function Get_Command return String is
      begin
         return To_String (Command);
      end Get_Command;
   end Gcode_Queue;

   procedure Finished_Block (Data : Flush_Extra_Data; First_Segment_Accel_Distance : Length) is
   begin
      if Data.Is_Homing_Move then
         Finished_Block_Queue.Push (Data, First_Segment_Accel_Distance);
      end if;
   end Finished_Block;

   protected body Finished_Block_Queue is
      procedure Push (In_Data : Flush_Extra_Data; In_First_Segment_Accel_Distance : Length) is
      begin
         if Has_Item then
            raise Constraint_Error with "There should not be more than one homing move in the pipeline.";
         end if;

         Data                         := In_Data;
         First_Segment_Accel_Distance := In_First_Segment_Accel_Distance;
         Has_Item                     := True;
      end Push;

      entry Pop (Out_Data : out Flush_Extra_Data; Out_First_Segment_Accel_Distance : out Length) when Has_Item is
      begin
         Out_Data                         := Data;
         Out_First_Segment_Accel_Distance := First_Segment_Accel_Distance;
         Has_Item                         := False;
      end Pop;
   end Finished_Block_Queue;

end Prunt.Controller.Gcode_Handler;
