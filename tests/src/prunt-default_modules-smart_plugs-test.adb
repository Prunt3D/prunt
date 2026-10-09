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

with Trendy_Test; use Trendy_Test;

package body Prunt.Default_Modules.Smart_Plugs.Test is

   procedure Test_Saved_Changes_Wait_For_Restart (T : in out Operation'Class);

   function All_Tests return Test_Group is
   begin
      return [Test_Saved_Changes_Wait_For_Restart'Unrestricted_Access];
   end All_Tests;

   procedure Test_Saved_Changes_Wait_For_Restart (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      declare
         Plug_Module : Module;
         File : constant Config.Config_File :=
           Config.Create ("smart_plug_saved_changes.json", ["Smart Plug" => Plug_Module.Config_Schema]);
         Result : Virtual_String;
         Errors : Config.Config_Error_Vectors.Vector;
         pragma Unreferenced (Result);
      begin
         T.Assert
           (not Read_Configuration (File.Get_Data ("Smart Plug")).Enabled,
            "startup loads the disabled default");

         File.Apply_Untrusted_Patch
           ("{""Prunt config version"":1,""Config"":{""Smart Plug"":{""Version"":1,""Config"":{""Smart_Plug"":{"
            & """Kind"":{""Selected"":""Enabled"",""Children"":{""Enabled"":{""Host"":""plug.local""}}}}}}}}",
            Result, Errors);
         T.Assert (Errors.Is_Empty, "saved enable patch is valid");
         T.Assert
           (not Read_Configuration (File.Get_Data ("Smart Plug")).Enabled,
            "saving Enabled does not activate the plug before restart");

         File.Reset_Live_To_Stored;
         T.Assert
           (Read_Configuration (File.Get_Data ("Smart Plug")).Enabled
            and then Read_Configuration (File.Get_Data ("Smart Plug")).Host = "plug.local",
            "restart applies saved settings even before the board connects");

         File.Apply_Untrusted_Patch
           ("{""Prunt config version"":1,""Config"":{""Smart Plug"":{""Version"":1,""Config"":{""Smart_Plug"":{"
            & """Kind"":{""Children"":{""Enabled"":{""Host"":""replacement.local"",""Watchdog_Seconds"":120}}}}}}}}",
            Result, Errors);
         T.Assert (Errors.Is_Empty, "saved host and timeout patch is valid");
         T.Assert
           (Read_Configuration (File.Get_Data ("Smart Plug")).Host = "plug.local"
            and then Read_Configuration (File.Get_Data ("Smart Plug")).Watchdog_Seconds = 60,
            "saving host and watchdog changes preserves the active settings until restart");

         File.Reset_Live_To_Stored;
         T.Assert
           (Read_Configuration (File.Get_Data ("Smart Plug")).Host = "replacement.local"
            and then Read_Configuration (File.Get_Data ("Smart Plug")).Watchdog_Seconds = 120,
            "restart applies host and watchdog changes together");

         File.Apply_Untrusted_Patch
           ("{""Prunt config version"":1,""Config"":{""Smart Plug"":{""Version"":1,""Config"":{""Smart_Plug"":{"
            & """Kind"":{""Selected"":""Disabled""}}}}}}",
            Result, Errors);
         T.Assert (Errors.Is_Empty, "saved disable patch is valid");
         T.Assert
           (Read_Configuration (File.Get_Data ("Smart Plug")).Enabled,
            "saving Disabled leaves the banner and worker enabled until restart");

         File.Reset_Live_To_Stored;
         T.Assert
           (not Read_Configuration (File.Get_Data ("Smart Plug")).Enabled,
            "restart applies saved disabling through the normal live configuration reset");
      end;
   end Test_Saved_Changes_Wait_For_Restart;

end Prunt.Default_Modules.Smart_Plugs.Test;
