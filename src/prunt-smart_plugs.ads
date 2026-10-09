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

pragma Extensions_Allowed (On);

private with Ada.Real_Time;
private with GNATCOLL.JSON;

package Prunt.Smart_Plugs is

   type Provider_Kind is (Tasmota) with Annotate => (Prunt_Config, User_Config);

   subtype Watchdog_Timeout is Positive range 30 .. 3600;

   type Plug_Config is record
      Enabled          : Boolean := False;
      Host             : Virtual_String;
      Provider         : Provider_Kind := Tasmota;
      Watchdog_Seconds : Watchdog_Timeout := 60;
   end record;

   type Controller is tagged limited private;

   task type Worker (Control : not null access Controller) is
      entry Stop;
   end Worker;

   procedure Configure (This : in out Controller; Value : Plug_Config);

   function Status_JSON (This : Controller) return Virtual_String;

   procedure Switch (This : in out Controller; Enabled : Boolean; Accepted : out Boolean);

   function Valid_Host (Host : Virtual_String) return Boolean;
   --  Accept a DNS name or IPv4 address, optionally followed by a port. No URL paths or credentials.

private

   type Power_State is (Unknown, Off, On);
   type Power_Request is (No_Request, Turn_Off, Turn_On);

   type Plug_Status is record
      Config          : Plug_Config;
      Power           : Power_State := Unknown;
      Watchdog_Active : Boolean := False;
      Busy            : Boolean := False;
      Error           : Virtual_String;
   end record;

   type Runtime_State is record
      Power     : Power_State := Unknown;
      Armed     : Boolean := False;
      Last_Feed : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
      Retry_Off : Boolean := False;
      Error     : Virtual_String;
   end record;

   generic
      with function Request (Host : Virtual_String; Path : String) return GNATCOLL.JSON.JSON_Value;
   procedure Poll (Config : Plug_Config; State : in out Runtime_State; Action : Power_Request);
   --  Run one power/watchdog cycle. The injected Request is to keep this testable.

   protected type Shared_State is
      procedure Configure (Value : Plug_Config);
      procedure Submit (Action : Power_Request; Accepted : out Boolean);
      procedure Take (Config_Out : out Plug_Config; Action : out Power_Request);
      procedure Publish (Value : Plug_Status);
      function Snapshot return Plug_Status;
   private
      Config          : Plug_Config;
      Status          : Plug_Status;
      Pending_Request : Power_Request := No_Request;
   end Shared_State;

   type Controller is tagged limited record
      State : Shared_State;
   end record;

end Prunt.Smart_Plugs;
