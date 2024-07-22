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

with Ada.Directories;
with Ada.Text_IO;
with TOML.File_IO; use TOML;

package body Prunt.Config is

   pragma Unsuppress (All_Checks);

   function To_TOML (V : Dimensionless) return TOML_Value is
     (Create_Float ((Kind => Regular, Value => Valid_Float (V))));
   function From_TOML (V : TOML_Value) return Dimensionless is (Dimensionless (V.As_Float.Value));

   generic
      type T is (<>);
   package Discrete_TOML_Conversion is
      function To_TOML (V : T) return TOML_Value is (Create_String (V'Image));
      function From_TOML (V : TOML_Value) return T is (T'Value (V.As_String));
   end Discrete_TOML_Conversion;

   --  package Stepper_Name_TOML_Conversion is new Discrete_TOML_Conversion (Stepper_Name);
   --  use Stepper_Name_TOML_Conversion;

   package Heater_Name_TOML_Conversion is new Discrete_TOML_Conversion (Heater_Name);
   use Heater_Name_TOML_Conversion;

   package Thermistor_Name_TOML_Conversion is new Discrete_TOML_Conversion (Thermistor_Name);
   use Thermistor_Name_TOML_Conversion;

   --  package Fan_Name_TOML_Conversion is new Discrete_TOML_Conversion (Fan_Name);
   --  use Fan_Name_TOML_Conversion;

   package Kinematics_Kind_TOML_Conversion is new Discrete_TOML_Conversion (Kinematics_Kind);
   use Kinematics_Kind_TOML_Conversion;

   package Homing_Kind_TOML_Conversion is new Discrete_TOML_Conversion (Homing_Kind);
   use Homing_Kind_TOML_Conversion;

   package Heater_Kind_TOML_Conversion is new Discrete_TOML_Conversion (Heater_Kind);
   use Heater_Kind_TOML_Conversion;

   package Bed_Mesh_Kind_TOML_Conversion is new Discrete_TOML_Conversion (Bed_Mesh_Kind);
   use Bed_Mesh_Kind_TOML_Conversion;

   package Fan_Kind_TOML_Conversion is new Discrete_TOML_Conversion (Fan_Kind);
   use Fan_Kind_TOML_Conversion;

   --  package Axis_Name_TOML_Conversion is new Discrete_TOML_Conversion (Axis_Name);
   --  use Axis_Name_TOML_Conversion;

   package Input_Switch_Name_TOML_Conversion is new Discrete_TOML_Conversion (Input_Switch_Name);
   use Input_Switch_Name_TOML_Conversion;

   package Thermistor_Kind_TOML_Conversion is new Discrete_TOML_Conversion (Thermistor_Kind);
   use Thermistor_Kind_TOML_Conversion;

   function To_TOML (V : Boolean) return TOML_Value is (Create_Boolean (V));
   function From_TOML (V : TOML_Value) return Boolean is (V.As_Boolean);

   function To_TOML (V : Path_Strings.Bounded_String) return TOML_Value is
     (Create_String (Path_Strings.To_String (V)));
   function From_TOML (V : TOML_Value) return Path_Strings.Bounded_String is
     (Path_Strings.To_Bounded_String (V.As_String));

   function To_TOML (Pos : Position) return TOML_Value is
      Table : constant TOML_Value := Create_Table;
   begin
      for I in Pos'Range loop
         Table.Set (I'Image, To_TOML (Pos (I) / mm));
      end loop;
      return Table;
   end To_TOML;

   function From_TOML (Table : TOML_Value) return Position is
      Pos : Position;
   begin
      for I in Pos'Range loop
         Pos (I) := From_TOML (Table.Get (I'Image)) * mm;
      end loop;
      return Pos;
   end From_TOML;

   function To_TOML (Steppers : Attached_Steppers) return TOML_Value is
      Value : constant TOML_Value := Create_Array;
   begin
      for I in Steppers'Range loop
         if Steppers (I) then
            Value.Append (Create_String (I'Image));
         end if;
      end loop;
      return Value;
   end To_TOML;

   function From_TOML (Table : TOML_Value) return Attached_Steppers is
      Steppers : Attached_Steppers := [others => False];
   begin
      for I in 1 .. Table.Length loop
         Steppers (Stepper_Name'Value (Table.Item (I).As_String)) := True;
      end loop;
      return Steppers;
   end From_TOML;

   function To_TOML (Vels : Axial_Velocities) return TOML_Value is
      Table : constant TOML_Value := Create_Table;
   begin
      for I in Vels'Range loop
         Table.Set (I'Image, To_TOML (Vels (I) / (mm / s)));
      end loop;
      return Table;
   end To_TOML;

   function From_TOML (Table : TOML_Value) return Axial_Velocities is
      Vels : Axial_Velocities;
   begin
      for I in Vels'Range loop
         Vels (I) := From_TOML (Table.Get (I'Image)) * mm / s;
      end loop;
      return Vels;
   end From_TOML;

   function To_TOML (Scale : Position_Scale) return TOML_Value is
      Table : constant TOML_Value := Create_Table;
   begin
      for I in Scale'Range loop
         Table.Set (I'Image, To_TOML (Scale (I)));
      end loop;
      return Table;
   end To_TOML;

   function From_TOML (Table : TOML_Value) return Position_Scale is
      Scale : Position_Scale;
   begin
      for I in Scale'Range loop
         Scale (I) := From_TOML (Table.Get (I'Image));
      end loop;
      return Scale;
   end From_TOML;

   function To_TOML (Params : Motion_Planner.Kinematic_Parameters) return TOML_Value is
      Table : constant TOML_Value := Create_Table;
   begin
      Table.Set ("Lower_Pos_Limit", To_TOML (Params.Lower_Pos_Limit));
      Table.Set ("Upper_Pos_Limit", To_TOML (Params.Upper_Pos_Limit));
      Table.Set ("Ignore_E_In_XYZE", To_TOML (Params.Ignore_E_In_XYZE));
      Table.Set ("Shift_Blended_Corners", To_TOML (Params.Shift_Blended_Corners));
      Table.Set ("Tangential_Velocity_Max", To_TOML (Params.Tangential_Velocity_Max / (mm / s)));
      Table.Set ("Axial_Velocity_Maxes", To_TOML (Params.Axial_Velocity_Maxes));
      Table.Set ("Pressure_Advance_Time", To_TOML (Params.Pressure_Advance_Time / s));
      Table.Set ("Acceleration_Max", To_TOML (Params.Acceleration_Max / (mm / s**2)));
      Table.Set ("Jerk_Max", To_TOML (Params.Jerk_Max / (mm / s**3)));
      Table.Set ("Snap_Max", To_TOML (Params.Snap_Max / (mm / s**4)));
      Table.Set ("Crackle_Max", To_TOML (Params.Crackle_Max / (mm / s**5)));
      Table.Set ("Chord_Error_Max", To_TOML (Params.Chord_Error_Max / mm));
      Table.Set ("Axial_Scaler", To_TOML (Params.Axial_Scaler));
      return Table;
   end To_TOML;

   function From_TOML (Table : TOML_Value) return Motion_Planner.Kinematic_Parameters is
   begin
      return
        (Lower_Pos_Limit         => From_TOML (Table.Get ("Lower_Pos_Limit")),
         Upper_Pos_Limit         => From_TOML (Table.Get ("Upper_Pos_Limit")),
         Ignore_E_In_XYZE        => From_TOML (Table.Get ("Ignore_E_In_XYZE")),
         Shift_Blended_Corners   => From_TOML (Table.Get ("Shift_Blended_Corners")),
         Tangential_Velocity_Max => From_TOML (Table.Get ("Tangential_Velocity_Max")) * mm / s,
         Axial_Velocity_Maxes    => From_TOML (Table.Get ("Axial_Velocity_Maxes")),
         Pressure_Advance_Time   => From_TOML (Table.Get ("Pressure_Advance_Time")) * s,
         Acceleration_Max        => From_TOML (Table.Get ("Acceleration_Max")) * mm / s**2,
         Jerk_Max                => From_TOML (Table.Get ("Jerk_Max")) * mm / s**3,
         Snap_Max                => From_TOML (Table.Get ("Snap_Max")) * mm / s**4,
         Crackle_Max             => From_TOML (Table.Get ("Crackle_Max")) * mm / s**5,
         Chord_Error_Max         => From_TOML (Table.Get ("Chord_Error_Max")) * mm,
         Axial_Scaler            => From_TOML (Table.Get ("Axial_Scaler")));
   end From_TOML;

   function To_TOML (V : TMC_Types.Unsigned_1) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_1 is (TMC_Types.Unsigned_1 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_2) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_2 is (TMC_Types.Unsigned_2 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_3) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_3 is (TMC_Types.Unsigned_3 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_4) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_4 is (TMC_Types.Unsigned_4 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_5) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_5 is (TMC_Types.Unsigned_5 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_6) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_6 is (TMC_Types.Unsigned_6 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_7) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_7 is (TMC_Types.Unsigned_7 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_8) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_8 is (TMC_Types.Unsigned_8 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_9) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_9 is (TMC_Types.Unsigned_9 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_10) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_10 is (TMC_Types.Unsigned_10 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_11) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_11 is (TMC_Types.Unsigned_11 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_12) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_12 is (TMC_Types.Unsigned_12 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_13) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_13 is (TMC_Types.Unsigned_13 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_14) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_14 is (TMC_Types.Unsigned_14 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_15) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_15 is (TMC_Types.Unsigned_15 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_16) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_16 is (TMC_Types.Unsigned_16 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_17) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_17 is (TMC_Types.Unsigned_17 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_18) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_18 is (TMC_Types.Unsigned_18 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_19) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_19 is (TMC_Types.Unsigned_19 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_20) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_20 is (TMC_Types.Unsigned_20 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_21) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_21 is (TMC_Types.Unsigned_21 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_22) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_22 is (TMC_Types.Unsigned_22 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_23) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_23 is (TMC_Types.Unsigned_23 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_24) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_24 is (TMC_Types.Unsigned_24 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_25) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_25 is (TMC_Types.Unsigned_25 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_26) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_26 is (TMC_Types.Unsigned_26 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_27) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_27 is (TMC_Types.Unsigned_27 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_28) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_28 is (TMC_Types.Unsigned_28 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_29) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_29 is (TMC_Types.Unsigned_29 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_30) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_30 is (TMC_Types.Unsigned_30 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_31) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_31 is (TMC_Types.Unsigned_31 (V.As_Integer));

   function To_TOML (V : TMC_Types.Unsigned_32) return TOML_Value is (Create_Integer (Any_Integer (V)));
   function From_TOML (V : TOML_Value) return TMC_Types.Unsigned_32 is (TMC_Types.Unsigned_32 (V.As_Integer));

   package TMC2240_Slope_Control_Type_TOML_Conversion is new Discrete_TOML_Conversion
     (TMC_Types.TMC2240.Slope_Control_Type);
   use TMC2240_Slope_Control_Type_TOML_Conversion;

   package TMC2240_Microstep_Resolution_Type_TOML_Conversion is new Discrete_TOML_Conversion
     (TMC_Types.TMC2240.Microstep_Resolution_Type);
   use TMC2240_Microstep_Resolution_Type_TOML_Conversion;

   package TMC2240_Freewheel_Type_TOML_Conversion is new Discrete_TOML_Conversion (TMC_Types.TMC2240.Freewheel_Type);
   use TMC2240_Freewheel_Type_TOML_Conversion;

   --  TODO: It might make sense to make these recursively merge tables.
   function Left (Key : Unbounded_UTF8_String; L, R : TOML_Value) return TOML_Value is
      pragma Unreferenced (Key, R);
   begin
      return L;
   end Left;

   function Right (Key : Unbounded_UTF8_String; L, R : TOML_Value) return TOML_Value is
      pragma Unreferenced (Key, L);
   begin
      return R;
   end Right;

   protected body Config_File is

      procedure Read (Data : out Prunt_Parameters) is
         Table : TOML_Value;
      begin
         Data := (others => <>);
         Write (Data, Append_Only => True);
         Table := TOML_Data.Get ("Prunt");

         Data.Enabled := From_TOML (Table.Get ("Enabled"));
      end Read;

      procedure Write (Data : Prunt_Parameters; Append_Only : Boolean := False) is
         Table : constant TOML_Value := Create_Table;
      begin
         Table.Set ("Enabled", To_TOML (Data.Enabled));

         Maybe_Read_File;
         TOML_Data.Set_Default ("Prunt", Create_Table);
         TOML_Data.Set
           ("Prunt", Merge (TOML_Data.Get ("Prunt"), Table, (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Read (Data : out Stepper_Parameters; Stepper : Stepper_Name) is
         Table : TOML_Value;
      begin
         case Stepper_Kinds (Stepper) is
            when Basic_Kind =>
               Data := (Kind => Basic_Kind, others => <>);
               Write (Data, Stepper, Append_Only => True);
               Table := TOML_Data.Get ("Stepper").Get (Stepper'Image);
            when TMC2240_UART_Kind =>
               Data := (Kind => TMC2240_UART_Kind, others => <>);
               Write (Data, Stepper, Append_Only => True);
               Table                     := TOML_Data.Get ("Stepper").Get (Stepper'Image);
               Data.Output_Current       := From_TOML (Table.Get ("Output_Current")) * amp;
               Data.Slope_Control        := From_TOML (Table.Get ("Slope_Control"));
               Data.I_Hold               := From_TOML (Table.Get ("I_Hold"));
               Data.I_Run                := From_TOML (Table.Get ("I_Run"));
               Data.I_Hold_Delay         := From_TOML (Table.Get ("I_Hold_Delay"));
               Data.I_Run_Delay          := From_TOML (Table.Get ("I_Run_Delay"));
               Data.T_Power_Down         := From_TOML (Table.Get ("T_Power_Down"));
               Data.T_PWM_Thrs           := From_TOML (Table.Get ("T_PWM_Thrs"));
               Data.T_Cool_Thrs          := From_TOML (Table.Get ("T_Cool_Thrs"));
               Data.T_High               := From_TOML (Table.Get ("T_High"));
               Data.TOFF                 := From_TOML (Table.Get ("TOFF"));
               Data.HSTRT_TFD210         := From_TOML (Table.Get ("HSTRT_TFD210"));
               Data.HEND_OFFSET          := From_TOML (Table.Get ("HEND_OFFSET"));
               Data.FD3                  := From_TOML (Table.Get ("FD3"));
               Data.DISFDCC              := From_TOML (Table.Get ("DISFDCC"));
               Data.CHM                  := From_TOML (Table.Get ("CHM"));
               Data.VHIGHFS              := From_TOML (Table.Get ("VHIGHFS"));
               Data.VHIGHCHM             := From_TOML (Table.Get ("VHIGHCHM"));
               Data.TPFD                 := From_TOML (Table.Get ("TPFD"));
               Data.Microstep_Resolution := From_TOML (Table.Get ("Microstep_Resolution"));
               Data.PWM_OFS              := From_TOML (Table.Get ("PWM_OFS"));
               Data.PWM_Grad             := From_TOML (Table.Get ("PWM_Grad"));
               Data.PWM_Freq             := From_TOML (Table.Get ("PWM_Freq"));
               Data.PWM_Auto_Scale       := From_TOML (Table.Get ("PWM_Auto_Scale"));
               Data.PWM_Auto_Grad        := From_TOML (Table.Get ("PWM_Auto_Grad"));
               Data.Freewheel            := From_TOML (Table.Get ("Freewheel"));
               Data.PWM_Meas_SD_Enable   := From_TOML (Table.Get ("PWM_Meas_SD_Enable"));
               Data.PWM_Dis_Reg_Stst     := From_TOML (Table.Get ("PWM_Dis_Reg_Stst"));
               Data.PWM_Reg              := From_TOML (Table.Get ("PWM_Reg"));
               Data.PWM_Lim              := From_TOML (Table.Get ("PWM_Lim"));
         end case;
         Data.Enabled     := From_TOML (Table.Get ("Enabled"));
         Data.Mm_Per_Step := From_TOML (Table.Get ("Mm_Per_Step")) * mm;

         if Data.Kind /= Stepper_Kinds (Stepper) then
            raise Constraint_Error
              with "Provided Stepper_Parameters has wrong discriminant. Expected " & Stepper_Kinds (Stepper)'Image &
              " but got " & Data.Kind'Image & " for stepper " & Stepper'Image & ".";
         end if;
      end Read;

      procedure Write (Data : Stepper_Parameters; Stepper : Stepper_Name; Append_Only : Boolean := False) is
         Table : constant TOML_Value := Create_Table;
      begin
         if Data.Kind /= Stepper_Kinds (Stepper) then
            raise Constraint_Error
              with "Provided Stepper_Parameters has wrong discriminant. Expected " & Stepper_Kinds (Stepper)'Image &
              " but got " & Data.Kind'Image & " for stepper " & Stepper'Image & ".";
         end if;

         case Data.Kind is
            when Basic_Kind =>
               null;
            when TMC2240_UART_Kind =>
               Table.Set ("Output_Current", To_TOML (Data.Output_Current / amp));
               Table.Set ("Slope_Control", To_TOML (Data.Slope_Control));
               Table.Set ("I_Hold", To_TOML (Data.I_Hold));
               Table.Set ("I_Run", To_TOML (Data.I_Run));
               Table.Set ("I_Hold_Delay", To_TOML (Data.I_Hold_Delay));
               Table.Set ("I_Run_Delay", To_TOML (Data.I_Run_Delay));
               Table.Set ("T_Power_Down", To_TOML (Data.T_Power_Down));
               Table.Set ("T_PWM_Thrs", To_TOML (Data.T_PWM_Thrs));
               Table.Set ("T_Cool_Thrs", To_TOML (Data.T_Cool_Thrs));
               Table.Set ("T_High", To_TOML (Data.T_High));
               Table.Set ("TOFF", To_TOML (Data.TOFF));
               Table.Set ("HSTRT_TFD210", To_TOML (Data.HSTRT_TFD210));
               Table.Set ("HEND_OFFSET", To_TOML (Data.HEND_OFFSET));
               Table.Set ("FD3", To_TOML (Data.FD3));
               Table.Set ("DISFDCC", To_TOML (Data.DISFDCC));
               Table.Set ("CHM", To_TOML (Data.CHM));
               Table.Set ("VHIGHFS", To_TOML (Data.VHIGHFS));
               Table.Set ("VHIGHCHM", To_TOML (Data.VHIGHCHM));
               Table.Set ("TPFD", To_TOML (Data.TPFD));
               Table.Set ("Microstep_Resolution", To_TOML (Data.Microstep_Resolution));
               Table.Set ("PWM_OFS", To_TOML (Data.PWM_OFS));
               Table.Set ("PWM_Grad", To_TOML (Data.PWM_Grad));
               Table.Set ("PWM_Freq", To_TOML (Data.PWM_Freq));
               Table.Set ("PWM_Auto_Scale", To_TOML (Data.PWM_Auto_Scale));
               Table.Set ("PWM_Auto_Grad", To_TOML (Data.PWM_Auto_Grad));
               Table.Set ("Freewheel", To_TOML (Data.Freewheel));
               Table.Set ("PWM_Meas_SD_Enable", To_TOML (Data.PWM_Meas_SD_Enable));
               Table.Set ("PWM_Dis_Reg_Stst", To_TOML (Data.PWM_Dis_Reg_Stst));
               Table.Set ("PWM_Reg", To_TOML (Data.PWM_Reg));
               Table.Set ("PWM_Lim", To_TOML (Data.PWM_Lim));
         end case;

         Table.Set ("Enabled", To_TOML (Data.Enabled));
         Table.Set ("Mm_Per_Step", To_TOML (Data.Mm_Per_Step / mm));

         Maybe_Read_File;
         TOML_Data.Set_Default ("Stepper", Create_Table);
         TOML_Data.Get ("Stepper").Set_Default (Stepper'Image, Create_Table);
         TOML_Data.Get ("Stepper").Set
           (Stepper'Image,
            Merge
              (TOML_Data.Get ("Stepper").Get (Stepper'Image),
               Table,
               (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Read (Data : out Kinematics_Parameters) is
         Table : TOML_Value;
      begin
         Data := (others => <>);
         Write (Data, Append_Only => True);
         Table := TOML_Data.Get ("Kinematics");

         case Kinematics_Kind'Value (Table.Get ("Kind").As_String) is
            when Cartesian_Kind =>
               Data := (Kind => Cartesian_Kind, others => <>);
               Write (Data, Append_Only => True);
               Table           := TOML_Data.Get ("Kinematics");
               Data.X_Steppers := From_TOML (Table.Get ("X_Steppers"));
               Data.Y_Steppers := From_TOML (Table.Get ("Y_Steppers"));
            when Core_XY_Kind =>
               Data := (Kind => Core_XY_Kind, others => <>);
               Write (Data, Append_Only => True);
               Table           := TOML_Data.Get ("Kinematics");
               Data.A_Steppers := From_TOML (Table.Get ("A_Steppers"));
               Data.B_Steppers := From_TOML (Table.Get ("B_Steppers"));
         end case;
         Data.Planner_Parameters := From_TOML (Table.Get ("Planner_Parameters"));
         Data.Z_Steppers         := From_TOML (Table.Get ("Z_Steppers"));
         Data.E_Steppers         := From_TOML (Table.Get ("E_Steppers"));
      end Read;

      procedure Write (Data : Kinematics_Parameters; Append_Only : Boolean := False) is
         Table : constant TOML_Value := Create_Table;
      begin
         Table.Set ("Kind", To_TOML (Data.Kind));
         case Data.Kind is
            when Cartesian_Kind =>
               Table.Set ("X_Steppers", To_TOML (Data.X_Steppers));
               Table.Set ("Y_Steppers", To_TOML (Data.Y_Steppers));
            when Core_XY_Kind =>
               Table.Set ("A_Steppers", To_TOML (Data.A_Steppers));
               Table.Set ("B_Steppers", To_TOML (Data.B_Steppers));
         end case;
         Table.Set ("Planner_Parameters", To_TOML (Data.Planner_Parameters));
         Table.Set ("Z_Steppers", To_TOML (Data.Z_Steppers));
         Table.Set ("E_Steppers", To_TOML (Data.E_Steppers));

         Maybe_Read_File;
         TOML_Data.Set_Default ("Kinematics", Create_Table);
         TOML_Data.Set
           ("Kinematics",
            Merge (TOML_Data.Get ("Kinematics"), Table, (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Read (Data : out Input_Switch_Parameters; Input_Switch : Input_Switch_Name) is
         Table : TOML_Value;
      begin
         Data := (others => <>);
         Write (Data, Input_Switch, Append_Only => True);
         Table := TOML_Data.Get ("Input_Switch").Get (Input_Switch'Image);

         Data.Enabled     := From_TOML (Table.Get ("Enabled"));
         Data.Hit_On_High := From_TOML (Table.Get ("Hit_On_High"));
      end Read;

      procedure Write
        (Data : Input_Switch_Parameters; Input_Switch : Input_Switch_Name; Append_Only : Boolean := False)
      is
         Table : constant TOML_Value := Create_Table;
      begin
         Table.Set ("Enabled", To_TOML (Data.Enabled));
         Table.Set ("Hit_On_High", To_TOML (Data.Hit_On_High));

         Maybe_Read_File;
         TOML_Data.Set_Default ("Input_Switch", Create_Table);
         TOML_Data.Get ("Input_Switch").Set_Default (Input_Switch'Image, Create_Table);
         TOML_Data.Get ("Input_Switch").Set
           (Input_Switch'Image,
            Merge
              (TOML_Data.Get ("Input_Switch").Get (Input_Switch'Image),
               Table,
               (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Read (Data : out Homing_Parameters; Axis : Axis_Name) is
         Table : TOML_Value;
      begin
         Data := (others => <>);
         Write (Data, Axis, Append_Only => True);
         Table := TOML_Data.Get ("Homing").Get (Axis'Image);

         case Homing_Kind'Value (Table.Get ("Kind").As_String) is
            when Double_Tap_Kind =>
               Data := (Kind => Double_Tap_Kind, others => <>);
               Write (Data, Axis, Append_Only => True);
               Table                       := TOML_Data.Get ("Homing").Get (Axis'Image);
               Data.Switch                 := From_TOML (Table.Get ("Switch"));
               Data.First_Move_Distance    := From_TOML (Table.Get ("First_Move_Distance")) * mm;
               Data.Back_Off_Move_Distance := From_TOML (Table.Get ("Back_Off_Move_Distance")) * mm;
               Data.Second_Move_Distance   := From_TOML (Table.Get ("Second_Move_Distance")) * mm;
               Data.Switch_Position        := From_TOML (Table.Get ("Switch_Position")) * mm;
            when Set_To_Value_Kind =>
               Data := (Kind => Set_To_Value_Kind, others => <>);
               Write (Data, Axis, Append_Only => True);
               Table      := TOML_Data.Get ("Homing").Get (Axis'Image);
               Data.Value := From_TOML (Table.Get ("Value")) * mm;
         end case;
      end Read;

      procedure Write (Data : Homing_Parameters; Axis : Axis_Name; Append_Only : Boolean := False) is
         Table : constant TOML_Value := Create_Table;
      begin
         Table.Set ("Kind", To_TOML (Data.Kind));
         case Data.Kind is
            when Double_Tap_Kind =>
               Table.Set ("Switch", To_TOML (Data.Switch));
               Table.Set ("First_Move_Distance", To_TOML (Data.First_Move_Distance / mm));
               Table.Set ("Back_Off_Move_Distance", To_TOML (Data.Back_Off_Move_Distance / mm));
               Table.Set ("Second_Move_Distance", To_TOML (Data.Second_Move_Distance / mm));
               Table.Set ("Switch_Position", To_TOML (Data.Switch_Position / mm));
            when Set_To_Value_Kind =>
               Table.Set ("Value", To_TOML (Data.Value / mm));
         end case;

         Maybe_Read_File;
         TOML_Data.Set_Default ("Homing", Create_Table);
         TOML_Data.Get ("Homing").Set_Default (Axis'Image, Create_Table);
         TOML_Data.Get ("Homing").Set
           (Axis'Image,
            Merge
              (TOML_Data.Get ("Homing").Get (Axis'Image), Table, (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Read (Data : out Extruder_Parameters) is
         Table : TOML_Value;
      begin
         Data := (others => <>);
         Write (Data, Append_Only => True);
         Table := TOML_Data.Get ("Extruder");

         Data.Nozzle_Diameter   := From_TOML (Table.Get ("Nozzle_Diameter")) * mm;
         Data.Filament_Diameter := From_TOML (Table.Get ("Filament_Diameter")) * mm;
      end Read;

      procedure Write (Data : Extruder_Parameters; Append_Only : Boolean := False) is
         Table : constant TOML_Value := Create_Table;
      begin
         Table.Set ("Nozzle_Diameter", To_TOML (Data.Nozzle_Diameter / mm));
         Table.Set ("Filament_Diameter", To_TOML (Data.Filament_Diameter / mm));

         Maybe_Read_File;
         TOML_Data.Set_Default ("Extruder", Create_Table);
         TOML_Data.Set
           ("Extruder",
            Merge (TOML_Data.Get ("Extruder"), Table, (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Read (Data : out Thermistor_Parameters; Thermistor : Thermistor_Name) is
         Table : TOML_Value;
      begin
         Data := (others => <>);
         Write (Data, Thermistor, Append_Only => True);
         Table := TOML_Data.Get ("Thermistor").Get (Thermistor'Image);

         case Thermistor_Kind'Value (Table.Get ("Kind").As_String) is
            when Disabled_Kind =>
               Data := (Kind => Disabled_Kind, others => <>);
               Write (Data, Thermistor, Append_Only => True);
               Table := TOML_Data.Get ("Thermistor").Get (Thermistor'Image);
            when Steinhart_Hart_Kind =>
               Data := (Kind => Steinhart_Hart_Kind, others => <>);
               Write (Data, Thermistor, Append_Only => True);
               Table     := TOML_Data.Get ("Thermistor").Get (Thermistor'Image);
               Data.SH_A := From_TOML (Table.Get ("SH_A"));
               Data.SH_B := From_TOML (Table.Get ("SH_B"));
               Data.SH_C := From_TOML (Table.Get ("SH_C"));
            when Callendar_Van_Dusen_Kind =>
               Data := (Kind => Callendar_Van_Dusen_Kind, others => <>);
               Write (Data, Thermistor, Append_Only => True);
               Table       := TOML_Data.Get ("Thermistor").Get (Thermistor'Image);
               Data.CVD_R0 := From_TOML (Table.Get ("CVD_R0")) * ohm;
               Data.CVD_A  := From_TOML (Table.Get ("CVD_A"));
               Data.CVD_B  := From_TOML (Table.Get ("CVD_B"));
         end case;
         Data.Minimum_Temperature := From_TOML (Table.Get ("Minimum_Temperature")) * celcius;
         Data.Maximum_Temperature := From_TOML (Table.Get ("Maximum_Temperature")) * celcius;

         if Data.Minimum_Temperature > Data.Maximum_Temperature then
            raise Constraint_Error with "Maximum temperature may not be less that minimum temperature.";
         end if;
      end Read;

      procedure Write (Data : Thermistor_Parameters; Thermistor : Thermistor_Name; Append_Only : Boolean := False) is
         Table : constant TOML_Value := Create_Table;
      begin
         if Data.Minimum_Temperature > Data.Maximum_Temperature then
            raise Constraint_Error with "Maximum temperature may not be less that minimum temperature.";
         end if;

         Table.Set ("Kind", To_TOML (Data.Kind));
         case Data.Kind is
            when Disabled_Kind =>
               null;
            when Steinhart_Hart_Kind =>
               Table.Set ("SH_A", To_TOML (Data.SH_A));
               Table.Set ("SH_B", To_TOML (Data.SH_B));
               Table.Set ("SH_C", To_TOML (Data.SH_C));
            when Callendar_Van_Dusen_Kind =>
               Table.Set ("CVD_R0", To_TOML (Data.CVD_R0 / ohm));
               Table.Set ("CVD_A", To_TOML (Data.CVD_A));
               Table.Set ("CVD_B", To_TOML (Data.CVD_B));
         end case;
         Table.Set ("Minimum_Temperature", To_TOML (Data.Minimum_Temperature / celcius));
         Table.Set ("Maximum_Temperature", To_TOML (Data.Maximum_Temperature / celcius));

         Maybe_Read_File;
         TOML_Data.Set_Default ("Thermistor", Create_Table);
         TOML_Data.Get ("Thermistor").Set_Default (Thermistor'Image, Create_Table);
         TOML_Data.Get ("Thermistor").Set
           (Thermistor'Image,
            Merge
              (TOML_Data.Get ("Thermistor").Get (Thermistor'Image),
               Table,
               (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Read (Data : out Heater_Full_Parameters; Heater : Heater_Name) is
         Table : TOML_Value;
      begin
         Data := (others => <>);
         Write (Data, Heater, Append_Only => True);
         Table := TOML_Data.Get ("Heater").Get (Heater'Image);

         case Heater_Kind'Value (Table.Get ("Kind").As_String) is
            when Disabled_Kind =>
               Data := (Params => (Kind => Disabled_Kind, others => <>), others => <>);
               Write (Data, Heater, Append_Only => True);
               Table := TOML_Data.Get ("Heater").Get (Heater'Image);
            when PID_Kind =>
               Data := (Params => (Kind => PID_Kind, others => <>), others => <>);
               Write (Data, Heater, Append_Only => True);
               Table                          := TOML_Data.Get ("Heater").Get (Heater'Image);
               Data.Params.Proportional_Scale := From_TOML (Table.Get ("Proportional_Scale"));
               Data.Params.Integral_Scale     := From_TOML (Table.Get ("Integral_Scale"));
               Data.Params.Derivative_Scale   := From_TOML (Table.Get ("Derivative_Scale"));
            when Bang_Bang_Kind =>
               Data := (Params => (Kind => Bang_Bang_Kind, others => <>), others => <>);
               Write (Data, Heater, Append_Only => True);
               Table := TOML_Data.Get ("Heater").Get (Heater'Image);
               Data.Params.Bang_Bang_Hysteresis := From_TOML (Table.Get ("Bang_Bang_Hysteresis")) * celcius;
            when PID_Autotune_Kind =>
               raise Constraint_Error with "PID_Autotune_Kind should not exist in config file.";
         end case;
         Data.Thermistor                        := From_TOML (Table.Get ("Thermistor"));
         Data.Params.Check_Max_Cumulative_Error := From_TOML (Table.Get ("Check_Max_Cumulative_Error")) * celcius;
         Data.Params.Check_Gain_Time            := From_TOML (Table.Get ("Check_Gain_Time")) * s;
         Data.Params.Check_Minimum_Gain         := From_TOML (Table.Get ("Check_Minimum_Gain")) * celcius;
         Data.Params.Check_Hysteresis           := From_TOML (Table.Get ("Check_Hysteresis")) * celcius;
      end Read;

      procedure Write (Data : Heater_Full_Parameters; Heater : Heater_Name; Append_Only : Boolean := False) is
         Table : constant TOML_Value := Create_Table;
      begin
         Table.Set ("Kind", To_TOML (Data.Params.Kind));
         case Data.Params.Kind is
            when Disabled_Kind =>
               null;
            when PID_Kind =>
               Table.Set ("Proportional_Scale", To_TOML (Data.Params.Proportional_Scale));
               Table.Set ("Integral_Scale", To_TOML (Data.Params.Integral_Scale));
               Table.Set ("Derivative_Scale", To_TOML (Data.Params.Derivative_Scale));
            when Bang_Bang_Kind =>
               Table.Set ("Bang_Bang_Hysteresis", To_TOML (Data.Params.Bang_Bang_Hysteresis / celcius));
            when PID_Autotune_Kind =>
               raise Constraint_Error with "PID_Autotune_Kind can not be written to config file.";
         end case;
         Table.Set ("Thermistor", To_TOML (Data.Thermistor));
         Table.Set ("Check_Max_Cumulative_Error", To_TOML (Data.Params.Check_Max_Cumulative_Error / celcius));
         Table.Set ("Check_Gain_Time", To_TOML (Data.Params.Check_Gain_Time / s));
         Table.Set ("Check_Minimum_Gain", To_TOML (Data.Params.Check_Minimum_Gain / celcius));
         Table.Set ("Check_Hysteresis", To_TOML (Data.Params.Check_Hysteresis / celcius));

         Maybe_Read_File;
         TOML_Data.Set_Default ("Heater", Create_Table);
         TOML_Data.Get ("Heater").Set_Default (Heater'Image, Create_Table);
         TOML_Data.Get ("Heater").Set
           (Heater'Image,
            Merge
              (TOML_Data.Get ("Heater").Get (Heater'Image),
               Table,
               (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Read (Data : out Bed_Mesh_Parameters) is
         Table : TOML_Value;
      begin
         Data := (others => <>);
         Write (Data, Append_Only => True);
         Table := TOML_Data.Get ("Bed_Mesh");

         case Bed_Mesh_Kind'Value (Table.Get ("Kind").As_String) is
            when No_Mesh_Kind =>
               Data := (Kind => No_Mesh_Kind);
               Write (Data, Append_Only => True);
               Table := TOML_Data.Get ("Bed_Mesh");
            when Beacon_Kind =>
               Data := (Kind => Beacon_Kind, others => <>);
               Write (Data, Append_Only => True);
               Table                     := TOML_Data.Get ("Bed_Mesh");
               Data.Serial_Port_Path     := From_TOML (Table.Get ("Serial_Port_Path"));
               Data.X_Offset             := From_TOML (Table.Get ("X_Offset")) * mm;
               Data.Y_Offset             := From_TOML (Table.Get ("Y_Offset")) * mm;
               Data.Calibration_Floor    := From_TOML (Table.Get ("Calibration_Floor")) * mm;
               Data.Calibration_Ceiling  := From_TOML (Table.Get ("Calibration_Ceiling")) * mm;
               Data.Calibration_Feedrate := From_TOML (Table.Get ("Calibration_Feedrate")) * mm / s;
         end case;
      end Read;

      procedure Write (Data : Bed_Mesh_Parameters; Append_Only : Boolean := False) is
         Table : constant TOML_Value := Create_Table;
      begin
         Table.Set ("Kind", To_TOML (Data.Kind));
         case Data.Kind is
            when No_Mesh_Kind =>
               null;
            when Beacon_Kind =>
               Table.Set ("Serial_Port_Path", To_TOML (Data.Serial_Port_Path));
               Table.Set ("X_Offset", To_TOML (Data.X_Offset / mm));
               Table.Set ("Y_Offset", To_TOML (Data.Y_Offset / mm));
               Table.Set ("Calibration_Floor", To_TOML (Data.Calibration_Floor / mm));
               Table.Set ("Calibration_Ceiling", To_TOML (Data.Calibration_Ceiling / mm));
               Table.Set ("Calibration_Feedrate", To_TOML (Data.Calibration_Feedrate / (mm / s)));
         end case;

         Maybe_Read_File;
         TOML_Data.Set_Default ("Bed_Mesh", Create_Table);
         TOML_Data.Set
           ("Bed_Mesh",
            Merge (TOML_Data.Get ("Bed_Mesh"), Table, (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Read (Data : out Fan_Parameters; Fan : Fan_Name) is
         Table : TOML_Value;
      begin
         Data := (others => <>);
         Write (Data, Fan, Append_Only => True);
         Table := TOML_Data.Get ("Fan").Get (Fan'Image);

         case Fan_Kind'Value (Table.Get ("Kind").As_String) is
            when Disabled_Kind =>
               Data := (Kind => Disabled_Kind);
               Write (Data, Fan, Append_Only => True);
               Table := TOML_Data.Get ("Fan").Get (Fan'Image);
            when Dynamic_PWM_Kind =>
               Data := (Kind => Dynamic_PWM_Kind, others => <>);
               Write (Data, Fan, Append_Only => True);
               Table                  := TOML_Data.Get ("Fan").Get (Fan'Image);
               Data.Disable_Below_PWM := From_TOML (Table.Get ("Disable_Below_PWM"));
               Data.Max_PWM           := From_TOML (Table.Get ("Max_PWM"));
            when Always_On_Kind =>
               Data := (Kind => Always_On_Kind, others => <>);
               Write (Data, Fan, Append_Only => True);
               Table              := TOML_Data.Get ("Fan").Get (Fan'Image);
               Data.Always_On_PWM := From_TOML (Table.Get ("Always_On_PWM"));
         end case;
      end Read;

      procedure Write (Data : Fan_Parameters; Fan : Fan_Name; Append_Only : Boolean := False) is
         Table : constant TOML_Value := Create_Table;
      begin
         Table.Set ("Kind", To_TOML (Data.Kind));
         case Data.Kind is
            when Disabled_Kind =>
               null;
            when Dynamic_PWM_Kind =>
               Table.Set ("Disable_Below_PWM", To_TOML (Data.Disable_Below_PWM));
               Table.Set ("Max_PWM", To_TOML (Data.Max_PWM));
            when Always_On_Kind =>
               Table.Set ("Always_On_PWM", To_TOML (Data.Always_On_PWM));
         end case;

         Maybe_Read_File;
         TOML_Data.Set_Default ("Fan", Create_Table);
         TOML_Data.Get ("Fan").Set_Default (Fan'Image, Create_Table);
         TOML_Data.Get ("Fan").Set
           (Fan'Image,
            Merge (TOML_Data.Get ("Fan").Get (Fan'Image), Table, (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Read (Data : out G_Code_Assignment_Parameters) is
         Table : TOML_Value;
      begin
         Data := (others => <>);
         Write (Data, Append_Only => True);
         Table := TOML_Data.Get ("G_Code_Assignment");

         Data.Bed_Heater    := From_TOML (Table.Get ("Bed_Heater"));
         --  Data.Chamber_Heater := From_TOML (Table.Get ("Chamber_Heater"));
         Data.Hotend_Heater := From_TOML (Table.Get ("Hotend_Heater"));
      end Read;

      procedure Write (Data : G_Code_Assignment_Parameters; Append_Only : Boolean := False) is
         Table : constant TOML_Value := Create_Table;
      begin
         Table.Set ("Bed_Heater", To_TOML (Data.Bed_Heater));
         --  Table.Set ("Chamber_Heater", To_TOML (Data.Chamber_Heater));
         Table.Set ("Hotend_Heater", To_TOML (Data.Hotend_Heater));

         Maybe_Read_File;
         TOML_Data.Set_Default ("G_Code_Assignment", Create_Table);
         TOML_Data.Set
           ("G_Code_Assignment",
            Merge (TOML_Data.Get ("G_Code_Assignment"), Table, (if Append_Only then Left'Access else Right'Access)));
         Write_File;
      end Write;

      procedure Maybe_Read_File is
      begin
         if File_Read then
            return;
         end if;

         if not Ada.Directories.Exists (Config_Path) then
            declare
               F : Ada.Text_IO.File_Type;
            begin
               Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Config_Path);
               Ada.Text_IO.Close (F);
            end;
         end if;

         declare
            Result : constant TOML.Read_Result := TOML.File_IO.Load_File (Config_Path);
         begin
            if Result.Success then
               TOML_Data := Result.Value;
               File_Read := True;
            else
               raise IO_Error with Result'Image;
            end if;
         end;
      end Maybe_Read_File;

      procedure Write_File is
         F : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Open (F, Ada.Text_IO.Out_File, Config_Path);
         TOML.File_IO.Dump_To_File (TOML_Data, F);
         Ada.Text_IO.Close (F);
      end Write_File;
   end Config_File;

end Prunt.Config;
