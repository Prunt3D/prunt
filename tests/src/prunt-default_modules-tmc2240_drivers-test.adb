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
with Prunt.TMC2240_Test_UART;

package body Prunt.Default_Modules.TMC2240_Drivers.Test is

   procedure Ignore_Config_Error (Path : Prunt.Config.Config_Path; Message : Virtual_String) is null;

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

   function All_Tests return Trendy_Test.Test_Group
   is (Trendy_Test.Test_Group'[Test_Disable_After_Manager_Termination'Unrestricted_Access]);

end Prunt.Default_Modules.TMC2240_Drivers.Test;
