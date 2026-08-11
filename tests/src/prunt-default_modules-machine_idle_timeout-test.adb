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

with Ada.Containers;
with Ada.Strings.Fixed;
with VSS.Strings.Conversions;

package body Prunt.Default_Modules.Machine_Idle_Timeout.Test is

   pragma Extensions_Allowed (On);

   use type Ada.Containers.Count_Type;
   use type Config.Config_Schema_Version;

   function Contains_Text (Source : Virtual_String; Fragment : String) return Boolean is
     (Ada.Strings.Fixed.Index (VSS.Strings.Conversions.To_UTF_8_String (Source), Fragment) /= 0);

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

   procedure Test_Command_Metadata (T : in out Trendy_Test.Operation'Class) is
      Commands        : constant Gcode_Command_Vectors.Vector :=
        Gcode_Commands (Module'(My_Modules.Module with null record));
      Setter_Command   : Gcode_Command renames Commands (1);
      Reporter_Command : Gcode_Command renames Commands (2);
   begin
      T.Register;
      T.Assert (Commands.Length = 2, "the module exposes separate setter and reporter forms of M85");
      T.Assert (Setter_Command.Identifier = Gcode_Command_Identifier'('M', 85), "the setter identifier is M85");
      T.Assert (Setter_Command.Name = "Set_Inactivity_Shutdown", "the first M85 form is the setter");
      T.Assert (Setter_Command.Arguments.Length = 1, "the M85 setter only accepts S");
      T.Assert
        (Setter_Command.Arguments ('S').Allowed_Kinds (Gcode_Arguments.Integer_Kind),
         "the M85 setter accepts an integer S");
      T.Assert
        (not Setter_Command.Arguments ('S').Allowed_Kinds (Gcode_Arguments.Non_Existent_Kind),
         "the M85 setter requires S");
      T.Assert
        (Contains_Text (Setter_Command.Description, "S0"),
         "the generated setter documentation explains disabling");
      T.Assert
        (Contains_Text (Setter_Command.Description, "fatal error"),
         "the generated command documentation explains timeout shutdown");
      T.Assert
        (Contains_Text (Setter_Command.Description, "Saved by M500"),
         "the generated setter documentation explains config saving");

      T.Assert (Reporter_Command.Identifier = Gcode_Command_Identifier'('M', 85), "the reporter identifier is M85");
      T.Assert (Reporter_Command.Name = "Report_Inactivity_Shutdown", "the second M85 form is the reporter");
      T.Assert (Reporter_Command.Arguments.Is_Empty, "the M85 reporter accepts no arguments");
      T.Assert
        (Contains_Text (Reporter_Command.Description, "Report the current"),
         "the generated reporter documentation describes its result");
   end Test_Command_Metadata;

   procedure Test_Config_Schema (T : in out Trendy_Test.Operation'Class) is
      Schema : constant Config.Versioned_Config_Schema := Config_Schema (Module'(My_Modules.Module with null record));
      Section : constant Config.Config_Property_Parameters_Sequence :=
        Config.Config_Property_Parameters_Sequence (Schema.Top_Level_Items ("Machine_Idle_Timeout"));
      Timeout : constant Config.Config_Property_Parameters_Float :=
        Config.Config_Property_Parameters_Float (Section.Children ("Timeout"));
      Schemas : constant Config.Config_Schema_Maps.Map := ["Machine_Idle_Timeout" => Schema];
      File    : constant Config.Config_File := Config.Create (Next_Test_Filename, Schemas);
      Data    : Config.Config_Data := File.Get_Data ("Machine_Idle_Timeout");
      Updated : User_Config := (others => <>);
   begin
      T.Register;
      T.Assert (Schema.Version = 1, "the inactivity-timeout user config has version one");
      T.Assert (Timeout.Min = 0.0, "the configured timeout cannot be negative");
      T.Assert (Timeout.Default = 0.0, "inactivity shutdown is disabled by default");
      T.Assert (Timeout.Unit = "s", "the configured timeout is expressed in seconds");
      T.Assert
        (Contains_Text (Timeout.Description, "M85 S"),
         "the user-config documentation explains the runtime override");

      Updated.Machine_Idle_Timeout.Timeout := 42.0 * s;
      User_Config_To_Config_Data (Data, Updated);
      T.Assert
        (Duration (Config_Data_To_User_Config (Data).Machine_Idle_Timeout.Timeout / s) = 42.0,
         "the M85 timeout survives conversion into the config data registered for M500");
   end Test_Config_Schema;

   function All_Tests return Trendy_Test.Test_Group is
     (Trendy_Test.Test_Group'
        [Test_Activity_Restarts_Timeout'Unrestricted_Access,
         Test_Command_Metadata'Unrestricted_Access,
         Test_Config_Schema'Unrestricted_Access]);

end Prunt.Default_Modules.Machine_Idle_Timeout.Test;
