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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Prunt.TMC2240_Test_UART;
with VSS.Strings.Conversions;

package body Prunt.Default_Modules.TMC2240_Drivers.Test is

   procedure Ignore_Config_Error (Path : Prunt.Config.Config_Path; Message : Virtual_String) is null;

   procedure Test_Excessive_Hysteresis_Reports_Config_Error (T : in out Trendy_Test.Operation'Class) is
      use Ada.Strings.Unbounded;

      Realistic_Motor : constant User_Config_SpreadCycle_Derived :=
        (Input_Voltage => 24.0 * volt, Phase_Inductance => 1.2 * millihenry, Phase_Resistance => 1.2 * ohm);
      --  Optimize_Spreadcycle gives this motor a hysteresis sum of 20 at 1.5 A and IRUN 1.0.

      procedure Check (Name : String; Config : User_Config_TMC2240; Expected_Error : String);
      --  Expected_Error is a fragment of the hysteresis error, or empty if no error may be reported.

      procedure Check (Name : String; Config : User_Config_TMC2240; Expected_Error : String) is
         Errors : Unbounded_String;

         procedure Record_Config_Error (Path : Prunt.Config.Config_Path; Message : Virtual_String) is
            pragma Unreferenced (Path);
         begin
            Append (Errors, VSS.Strings.Conversions.To_UTF_8_String (Message) & ASCII.LF);
         end Record_Config_Error;
      begin
         declare
            Registers : constant TMC2240_Registers :=
              Generate_Default_Registers
                (Config              => Config,
                 Motor_Enabled       => True,
                 Report_Config_Error => Record_Config_Error'Access,
                 Motor               => Motor_Name'First,
                 Distance_Per_Step   => 0.01 * mm);
            pragma Unreferenced (Registers);
         begin
            null;
         end;

         if Expected_Error = "" then
            T.Assert (Errors = "", Name & ": no error expected, got: " & To_String (Errors));
         else
            T.Assert
              (Ada.Strings.Fixed.Index (To_String (Errors), Expected_Error) /= 0,
               Name & ": expected an error containing '" & Expected_Error & "', got: " & To_String (Errors));
         end if;
      exception
         when E : Constraint_Error =>
            T.Fail (Name & ": the reported config error must not be followed by an exception: "
                    & Ada.Exceptions.Exception_Message (E));
      end Check;
   begin
      T.Register;

      Check
        ("default Derived parameters",
         (IRUN => 1.0, others => <>),
         "Automatically computed hysteresis sum is too high. Check that motor parameters are correct. If parameters "
         & "are correct then decrease TBL, decrease IRUN, or use manual tuning." & ASCII.LF);
      Check
        ("realistic Derived motor at full scale",
         (Run_Current => 1.5 * amp,
          IRUN        => 1.0,
          CHM         => (Kind => SpreadCycle, SpreadCycle => (Kind => Derived, Derived => Realistic_Motor)),
          others      => <>),
         "A very small reduction of IRUN to 0.97");
      Check
        ("realistic Derived motor below full scale",
         (Run_Current        => 1.5 * amp,
          IRUN               => 0.97,
          IRUN_During_Homing => 0.97,
          CHM                => (Kind => SpreadCycle, SpreadCycle => (Kind => Derived, Derived => Realistic_Motor)),
          others             => <>),
         "");
      Check
        ("Manual HSTRT + HEND above 14",
         (IRUN   => 1.0,
          CHM    =>
            (Kind => SpreadCycle, SpreadCycle => (Kind => Manual, Manual => (HSTRT => 8, HEND => 12))),
          others => <>),
         "HSTRT + HEND must be less than 15");
   end Test_Excessive_Hysteresis_Reports_Config_Error;

   procedure Test_Disable_After_Manager_Termination (T : in out Trendy_Test.Operation'Class) is
      Motor        : constant Motor_Name := Motor_Name'First;
      Registers    : constant TMC2240_Registers :=
        Generate_Default_Registers
          (Config              => (IRUN => 0.5, others => <>),
           Motor_Enabled       => True,
           Report_Config_Error => Ignore_Config_Error'Access,
           Motor               => Motor,
           Distance_Per_Step   => 0.01 * mm);
      Emitter      : Status_Manager.Status_Emitter;
      Setup_Failed : Boolean := False;

      function Create_UART_Manager return UART_Motor_Manager;

      function Create_UART_Manager return UART_Motor_Manager is
      begin
         return Result : UART_Motor_Manager;
      end Create_UART_Manager;
   begin
      T.Register;
      TMC2240_Test_UART.Reset;
      T.Assert (Registers.CHOPCONF.TOFF /= Disable_Driver, "the default configuration enables the driver");

      begin
         declare
            Manager : TMC_Motor_Manager;
         begin
            Manager.Kind := TMC2240_UART_Kind;
            Manager.Motor := Motor;
            Manager.UART.Set (Create_UART_Manager'Access);
            Manager.Started := True;

            --  The enabling CHOPCONF write reaches the driver, but its readback is lost, so the error escapes the
            --  Setup entry body and terminates the manager.
            TMC2240_Test_UART.Drop_Next_Readback (CHOPCONF_Address);
            begin
               Manager.UART.Get.Setup (Registers, Motor, Emitter);
            exception
               when TMC_UART_Error =>
                  Setup_Failed := True;
            end;

            T.Assert (Setup_Failed, "a missing CHOPCONF readback fails Setup");
            T.Assert
              (TMC2240_Test_UART.CHOPCONF.TOFF /= Disable_Driver, "the unconfirmed write left the driver enabled");
            while not Manager.UART.Get.Element.all'Terminated loop
               delay 0.001;
            end loop;
         end;
      exception
         when E : Program_Error =>
            --  An exception propagated by Finalize is re-raised as Program_Error after leaving the block.
            T.Fail
              ("stopping a terminated manager must not fail cleanup: " & Ada.Exceptions.Exception_Information (E));
      end;

      T.Assert
        (TMC2240_Test_UART.CHOPCONF.TOFF = Disable_Driver, "cleanup disables the driver after the manager terminates");
   end Test_Disable_After_Manager_Termination;

   procedure Test_Supply_Voltage_Check (T : in out Trendy_Test.Operation'Class) is
      Motor : constant Motor_Name := Motor_Name'First;

      Derived_24V : constant User_Config_TMC2240 :=
        (CHM    =>
           (Kind        => SpreadCycle,
            SpreadCycle => (Kind => Derived, Derived => (Input_Voltage => 24.0 * volt, others => <>))),
         others => <>);

      function Reading (Volts : Dimensionless) return ADC_V_Supply_Volts_Type
      is (ADC_V_Supply_Volts_Type (Volts));

      procedure Check
        (Name           : String;
         Config         : User_Config_TMC2240;
         Enabled        : Boolean;
         Supply         : Dimensionless;
         Expected_Error : String);
      --  A negative Supply leaves ADC_VSUPPLY_AIN without a reply, so the check must not read it. Expected_Error is
      --  a fragment of the mismatch error, or empty if the check must pass.

      procedure Check
        (Name           : String;
         Config         : User_Config_TMC2240;
         Enabled        : Boolean;
         Supply         : Dimensionless;
         Expected_Error : String) is
      begin
         TMC2240_Test_UART.Reset;
         if Supply >= 0.0 then
            TMC2240_Test_UART.Set_Supply_Voltage (Reading (Supply));
         end if;

         Check_Supply_Voltages
           ((Motors => [others => (Fixed_Kind => TMC2240_UART_Kind, TMC2240_Parameters => Config)]),
            [others => Enabled]);
         T.Assert (Expected_Error = "", Name & ": expected an error containing '" & Expected_Error & "'");
      exception
         when E : Hardware_Config_Mismatch_Error =>
            T.Assert
              (Expected_Error /= ""
               and then Ada.Strings.Fixed.Index (Ada.Exceptions.Exception_Message (E), Expected_Error) /= 0,
               Name & ": unexpected error: " & Ada.Exceptions.Exception_Message (E));
         when E : TMC_UART_Error =>
            T.Fail (Name & ": unexpected UART failure: " & Ada.Exceptions.Exception_Message (E));
      end Check;
   begin
      T.Register (Parallelize => False);

      Check ("matching supply", Derived_24V, True, 24.1, "");
      Check ("supply 9% low", Derived_24V, True, 21.84, "");
      Check ("supply 9% high", Derived_24V, True, 26.16, "");
      Check ("supply 11% low", Derived_24V, True, 21.36, "Input_Voltage is 24.00 V");
      Check ("supply 11% high", Derived_24V, True, 26.64, "Input_Voltage is 24.00 V");
      Check ("12 V supply", Derived_24V, True, 12.0, "Motor " & Motor'Image & " supply voltage is 12.0");
      Check ("disabled motor", Derived_24V, False, -1.0, "");
      Check
        ("manual SpreadCycle",
         (CHM => (Kind => SpreadCycle, SpreadCycle => (Kind => Manual, Manual => <>)), others => <>),
         True,
         -1.0,
         "");
      Check
        ("constant off-time",
         (CHM => (Kind => Constant_Off_Time, Constant_Off_Time => <>), others => <>),
         True,
         -1.0,
         "");

      TMC2240_Test_UART.Reset;
      begin
         Check_Supply_Voltages
           ((Motors => [others => (Fixed_Kind => TMC2240_UART_Kind, TMC2240_Parameters => Derived_24V)]),
            [others => True]);
         T.Fail ("a missing supply reading must not pass the check");
      exception
         when TMC_UART_Error =>
            null;
      end;
   end Test_Supply_Voltage_Check;

   function All_Tests return Trendy_Test.Test_Group
   is (Trendy_Test.Test_Group'
         [Test_Disable_After_Manager_Termination'Unrestricted_Access,
          Test_Excessive_Hysteresis_Reports_Config_Error'Unrestricted_Access,
          Test_Supply_Voltage_Check'Unrestricted_Access]);

end Prunt.Default_Modules.TMC2240_Drivers.Test;
