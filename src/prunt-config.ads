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

with Prunt.Thermistors;     use Prunt.Thermistors;
with Prunt.Heaters;         use Prunt.Heaters;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.JSON;         use GNATCOLL.JSON;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Prunt.Motion_Planner;
with Prunt.TMC_Types;
with Prunt.TMC_Types.TMC2240;

generic
   type Stepper_Name is (<>);
   type Stepper_Kinds_Type is array (Stepper_Name) of Stepper_Kind;
   Stepper_Kinds : Stepper_Kinds_Type;
   type Heater_Name is (<>);
   type Thermistor_Name is (<>);
   type Fan_Name is (<>);
   type Input_Switch_Name is (<>);
   Config_Path : String;
package Prunt.Config is

   --  function Schema return String;

   type Attached_Steppers is array (Stepper_Name) of Boolean;

   IO_Error                 : exception;
   Config_File_Format_Error : exception;

   type Prunt_Parameters is record
      Enabled            : Boolean := False;
      Replace_G0_With_G1 : Boolean := False;
   end record;

   type Stepper_Parameters (Kind : Stepper_Kind := Basic_Kind) is record
      Enabled     : Boolean := False;
      Mm_Per_Step : Length  := Length'Last / 2.0;
      case Kind is
         when Basic_Kind =>
            null;
         when TMC2240_UART_Kind =>
            GCONF         : TMC_Types.TMC2240.GCONF;
            DRV_CONF      : TMC_Types.TMC2240.DRV_CONF;
            GLOBAL_SCALER : TMC_Types.TMC2240.GLOBAL_SCALER;
            IHOLD_IRUN    : TMC_Types.TMC2240.IHOLD_IRUN;
            TPOWERDOWN    : TMC_Types.TMC2240.TPOWERDOWN;
            TPWMTHRS      : TMC_Types.TMC2240.TPWMTHRS;
            TCOOLTHRS     : TMC_Types.TMC2240.TCOOLTHRS;
            THIGH         : TMC_Types.TMC2240.THIGH;
            CHOPCONF      : TMC_Types.TMC2240.CHOPCONF;
            PWMCONF       : TMC_Types.TMC2240.PWMCONF;
            PWM_AUTO      : TMC_Types.TMC2240.PWM_AUTO;
      end case;
   end record;

   type Kinematics_Kind is (Cartesian_Kind, Core_XY_Kind);

   type Kinematics_Parameters (Kind : Kinematics_Kind := Cartesian_Kind) is record
      Planner_Parameters : Motion_Planner.Kinematic_Parameters := (others => <>);
      Z_Steppers         : Attached_Steppers                   := [others => False];
      E_Steppers         : Attached_Steppers                   := [others => False];
      case Kind is
         when Cartesian_Kind =>
            X_Steppers : Attached_Steppers := [others => False];
            Y_Steppers : Attached_Steppers := [others => False];
         when Core_XY_Kind =>
            A_Steppers : Attached_Steppers := [others => False];
            B_Steppers : Attached_Steppers := [others => False];
      end case;
   end record;

   type Input_Switch_Parameters is record
      Enabled     : Boolean := False;
      Hit_On_High : Boolean := False;
   end record;

   type Homing_Kind is (Double_Tap_Kind, Set_To_Value_Kind);

   type Homing_Parameters (Kind : Homing_Kind := Double_Tap_Kind) is record
      case Kind is
         when Double_Tap_Kind =>
            Switch                 : Input_Switch_Name := Input_Switch_Name'First;
            First_Move_Distance    : Length            := 0.0 * mm;
            Back_Off_Move_Distance : Length            := 0.0 * mm;
            Second_Move_Distance   : Length            := 0.0 * mm;
            Switch_Position        : Length            := 0.0 * mm;
            Move_To_After          : Length            := 5.0 * mm;
         when Set_To_Value_Kind =>
            Value : Length := 0.0 * mm;
      end case;
   end record;

   --  Thermistor_Parameters in Prunt.Thermistors.

   type Heater_Full_Parameters is record
      Thermistor : Thermistor_Name := Thermistor_Name'First;
      Params     : Heaters.Heater_Parameters;
   end record;

   type Fan_Kind is (Dynamic_PWM_Kind, Always_On_Kind);

   type Fan_Parameters (Kind : Fan_Kind := Always_On_Kind) is record
      Invert_Output : Boolean           := False;
      PWM_Frequency : Fan_PWM_Frequency := 30.0 * hertz;
      case Kind is
         when Dynamic_PWM_Kind =>
            Disable_Below_PWM : PWM_Scale := 0.5;
            Max_PWM           : PWM_Scale := 1.0;
         when Always_On_Kind =>
            Always_On_PWM : PWM_Scale := 1.0;
      end case;
   end record;

   type G_Code_Assignment_Parameters is record
      Bed_Heater    : Heater_Name := Heater_Name'First;
      Hotend_Heater : Heater_Name := Heater_Name'First;
   end record;

   --  procedure Disable_Prunt;
   --  procedure Read (Data : out Prunt_Parameters);
   --  procedure Read (Data : out Stepper_Parameters; Stepper : Stepper_Name) with
   --    Post => Data.Kind = Stepper_Kinds (Stepper);
   --  procedure Read (Data : out Kinematics_Parameters);
   --  procedure Read (Data : out Input_Switch_Parameters; Input_Switch : Input_Switch_Name);
   --  procedure Read (Data : out Homing_Parameters; Axis : Axis_Name);
   --  procedure Read (Data : out Thermistor_Parameters; Thermistor : Thermistor_Name);
   --  procedure Read (Data : out Heater_Full_Parameters; Heater : Heater_Name) with
   --    Post => Data.Params.Kind not in PID_Autotune_Kind;
   --  procedure Read (Data : out Fan_Parameters; Fan : Fan_Name);
   --  procedure Read (Data : out G_Code_Assignment_Parameters);
   --  procedure Patch (Data : String);
   --  procedure Validate_Config (Report : access procedure (Message : String));

private

   package Discrete_String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   type Property_Kind is (Boolean_Kind, Discrete_Kind, Integer_Kind, Float_Kind, Sequence_Kind, Variant_Kind);

   type Property_Parameters (Kind : Property_Kind);
   type Property_Parameters_Access is not null access constant Property_Parameters;

   package Property_Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, Property_Parameters_Access);

   type Property_Parameters (Kind : Property_Kind) is record
      Description : Unbounded_String;
      case Kind is
         when Boolean_Kind =>
            Boolean_Default : Boolean;
         when Discrete_Kind =>
            Discrete_Options : Discrete_String_Sets.Set;
            Discrete_Default : Unbounded_String;
         when Integer_Kind =>
            Integer_Min     : Long_Long_Integer;
            Integer_Max     : Long_Long_Integer;
            Integer_Unit    : Unbounded_String;
            Integer_Default : Long_Long_Integer;
         when Float_Kind =>
            Float_Min     : Long_Float;
            Float_Max     : Long_Float;
            Float_Unit    : Unbounded_String;
            Float_Default : Long_Float;
         when Sequence_Kind =>
            Sequence_Children : Property_Maps.Map;
         when Variant_Kind =>
            Variant_Children : Property_Maps.Map;
            Variant_Default  : Unbounded_String;
      end case;
   end record;

   package Flat_Schemas is new Ada.Containers.Indefinite_Ordered_Maps (String, Property_Parameters_Access);

   function Build_Schema return Property_Maps.Map;
   --  This function leaks memory as it is only meant to be called once over the lifetime of the program.

   function Schema_To_JSON (Schema : Property_Maps.Map) return Unbounded_String;

   function Build_Flat_Schema (Schema : Property_Maps.Map) return Flat_Schemas.Map with
     Post =>
      (for all P of Build_Flat_Schema'Result => P.Kind in Boolean_Kind | Discrete_Kind | Integer_Kind | Float_Kind);
   --  This function leaks memory as it is only meant to be called once over the lifetime of the program.

   type Stepper_Parameters_Array is array (Stepper_Name) of Stepper_Parameters;

   type Input_Switch_Parameters_Array is array (Input_Switch_Name) of Input_Switch_Parameters;

   type Homing_Parameters_Array is array (Axis_Name) of Homing_Parameters;

   type Thermistor_Parameters_Array is array (Thermistor_Name) of Thermistor_Parameters;

   type Heater_Full_Parameters_Array is array (Heater_Name) of Heater_Full_Parameters;

   type Fan_Parameters_Array is array (Fan_Name) of Fan_Parameters;

   type Full_Config is record
      Prunt              : Prunt_Parameters;
      Kinematics         : Kinematics_Parameters;
      G_Code_Assignments : G_Code_Assignment_Parameters;
      Steppers           : Stepper_Parameters_Array;
      Switches           : Input_Switch_Parameters_Array;
      Homing             : Homing_Parameters_Array;
      Thermistors        : Thermistor_Parameters_Array;
      Heaters            : Heater_Full_Parameters_Array;
      Fans               : Fan_Parameters_Array;
   end record;

   protected Config_File is
      procedure Disable_Prunt;
      procedure Read (Data : out Prunt_Parameters);
      procedure Read (Data : out Stepper_Parameters; Stepper : Stepper_Name) with
        Post => Data.Kind = Stepper_Kinds (Stepper);
      procedure Read (Data : out Kinematics_Parameters);
      procedure Read (Data : out Input_Switch_Parameters; Input_Switch : Input_Switch_Name);
      procedure Read (Data : out Homing_Parameters; Axis : Axis_Name);
      procedure Read (Data : out Thermistor_Parameters; Thermistor : Thermistor_Name);
      procedure Read (Data : out Heater_Full_Parameters; Heater : Heater_Name) with
        Post => Data.Params.Kind not in PID_Autotune_Kind;
      procedure Read (Data : out Fan_Parameters; Fan : Fan_Name);
      procedure Read (Data : out G_Code_Assignment_Parameters);
      procedure Patch (Data : String);
      procedure Validate_Config (Report : access procedure (Message : String));
   private
      procedure Maybe_Do_Init;
      procedure Write_File;
      Init_Done      : Boolean := False;
      JSON_Data      : JSON_Value;
      Schema         : Property_Maps.Map;
      Schema_JSON    : Unbounded_String;
      Flat_Schema    : Flat_Schemas.Map;
      Initial_Config : Full_Config;
   end Config_File;

end Prunt.Config;
