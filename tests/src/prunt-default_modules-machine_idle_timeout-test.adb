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

package body Prunt.Default_Modules.Machine_Idle_Timeout.Test is

   pragma Extensions_Allowed (On);

   procedure Test_Activity_Restarts_Timeout (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         Disabled_Did_Not_Expire : Boolean;
         Expired_Once            : Boolean;
         Motion_Prevented_Expiry : Boolean;
         Restarted_Interval_Held : Boolean;
         Stayed_Disarmed         : Boolean;
         Sustained_Idle_Expired  : Boolean;
         Transient_Activity_Held : Boolean;
         Transient_Idle_Expired  : Boolean;
         Watchdog                : Inactivity_Watchdog;
      begin
         Reset_Report_Count;
         Watchdog.Start;
         Watchdog.Idle_Start;

         Watchdog.Set_Timeout (0.10);
         Watchdog.Set_Timeout (0.0);
         delay 0.18;
         Disabled_Did_Not_Expire := Get_Report_Count = 0;

         Watchdog.Set_Timeout (0.10);
         delay 0.18;
         Expired_Once := Get_Report_Count = 1;
         delay 0.12;
         Stayed_Disarmed := Get_Report_Count = 1;

         Reset_Report_Count;
         Watchdog.Set_Timeout (0.25);

         delay 0.12;
         Watchdog.Idle_End;
         delay 0.25;
         Motion_Prevented_Expiry := Get_Report_Count = 0;

         Watchdog.Idle_Start;
         delay 0.12;
         Restarted_Interval_Held := Get_Report_Count = 0;
         delay 0.22;
         Sustained_Idle_Expired := Get_Report_Count = 1;

         Reset_Report_Count;
         Watchdog.Set_Timeout (0.25);
         delay 0.15;
         Watchdog.Idle_End;
         Watchdog.Idle_Start;
         delay 0.15;
         Transient_Activity_Held := Get_Report_Count = 0;
         delay 0.15;
         Transient_Idle_Expired := Get_Report_Count = 1;
         Watchdog.Stop;

         T.Assert (Disabled_Did_Not_Expire, "a zero timeout disables the watchdog");
         T.Assert (Expired_Once, "an armed watchdog reports sustained inactivity exactly once");
         T.Assert (Motion_Prevented_Expiry, "motion prevents the armed timeout from expiring");
         T.Assert (Restarted_Interval_Held, "becoming idle restarts the complete timeout interval");
         T.Assert (Stayed_Disarmed, "an expired watchdog disarms itself");
         T.Assert (Sustained_Idle_Expired, "the restarted timeout expires once after sustained inactivity");
         T.Assert
           (Transient_Activity_Held,
            "back-to-back idle-end and idle-start events restart the complete timeout interval");
         T.Assert (Transient_Idle_Expired, "the timeout expires after transient motion followed by sustained idle");
      end;
   end Test_Activity_Restarts_Timeout;

   function All_Tests return Trendy_Test.Test_Group is
     (Trendy_Test.Test_Group'[1 => Test_Activity_Restarts_Timeout'Unrestricted_Access]);

end Prunt.Default_Modules.Machine_Idle_Timeout.Test;
