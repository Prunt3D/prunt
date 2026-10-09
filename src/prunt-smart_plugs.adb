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
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Fixed;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Prunt.Curl;
with Util.Http.Clients;
with VSS.Strings.Conversions;

package body Prunt.Smart_Plugs is

   pragma Extensions_Allowed (On);

   package Conversions renames VSS.Strings.Conversions;

   procedure Configure (This : in out Controller; Value : Plug_Config) is
   begin
      This.State.Configure (Value);
   end Configure;

   function Valid_Host (Host : Virtual_String) return Boolean is
      Text  : constant String := Conversions.To_UTF_8_String (Host);
      Colon : constant Natural := Ada.Strings.Fixed.Index (Text, ":");
      Last  : constant Integer := (if Colon = 0 then Text'Last else Colon - 1);
   begin
      if Text'Length = 0 or else Text'Length > 259 or else Last < Text'First then
         return False;
      end if;
      if Text (Text'First) not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
        or else Text (Last) not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
      then
         return False;
      end if;
      for C of Text (Text'First .. Last) loop
         if C not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' then
            return False;
         end if;
      end loop;
      if Colon /= 0 then
         if Colon = Text'Last then
            return False;
         end if;
         for C of Text (Colon + 1 .. Text'Last) loop
            if C not in '0' .. '9' then
               return False;
            end if;
         end loop;
         return Integer'Value (Text (Colon + 1 .. Text'Last)) in 1 .. 65535;
      end if;
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Valid_Host;

   procedure Poll (Config : Plug_Config; State : in out Runtime_State; Action : Power_Request) is
      function Tasmota_Request (Host : Virtual_String; Command : String) return JSON_Value;
      function Read_Power (Config : Plug_Config) return Power_State;
      procedure Set_Power (Config : Plug_Config; Enabled : Boolean);
      procedure Prepare_Watchdog (Config : Plug_Config);
      procedure Refresh_Watchdog (Config : Plug_Config);

      function Tasmota_Request (Host : Virtual_String; Command : String) return JSON_Value is
         Encoded : Virtual_String;
      begin
         for C of Command loop
            if C = ' ' then
               Encoded.Append ("%20");
            else
               Encoded.Append (+String'(1 => C));
            end if;
         end loop;
         return Request (Host, "/cm?cmnd=" & Conversions.To_UTF_8_String (Encoded));
      end Tasmota_Request;

      function Read_Power (Config : Plug_Config) return Power_State is
      begin
         case Config.Provider is
            when Tasmota =>
               declare
                  Reply : constant JSON_Value := Tasmota_Request (Config.Host, "Power1");
                  Value : constant String :=
                    (if Reply.Has_Field ("POWER1") then Reply.Get ("POWER1") else Reply.Get ("POWER"));
               begin
                  if Value = "ON" then
                     return On;
                  elsif Value = "OFF" then
                     return Off;
                  end if;
                  raise Constraint_Error with "Unexpected Tasmota power response";
               end;
         end case;
      end Read_Power;

      procedure Set_Power (Config : Plug_Config; Enabled : Boolean) is
      begin
         case Config.Provider is
            when Tasmota =>
               declare
                  Reply : constant JSON_Value :=
                    Tasmota_Request (Config.Host, (if Enabled then "Power1 ON" else "Power1 OFF"));
                  Value : constant String :=
                    (if Reply.Has_Field ("POWER1") then Reply.Get ("POWER1") else Reply.Get ("POWER"));
               begin
                  if Value /= (if Enabled then "ON" else "OFF") then
                     raise Constraint_Error with "Tasmota did not confirm requested power state";
                  end if;
               end;
         end case;
      end Set_Power;

      procedure Prepare_Watchdog (Config : Plug_Config) is
      begin
         case Config.Provider is
            when Tasmota =>
               declare
                  Reply : constant JSON_Value := Tasmota_Request (Config.Host, "PowerOnState 0");
               begin
                  if Integer'(Reply.Get ("PowerOnState")) /= 0 then
                     raise Constraint_Error with "Tasmota did not confirm power-off on boot";
                  end if;
               end;
         end case;
         Refresh_Watchdog (Config);
      end Prepare_Watchdog;

      procedure Refresh_Watchdog (Config : Plug_Config) is
      begin
         case Config.Provider is
            when Tasmota =>
               declare
                  Pulse : constant Integer := Config.Watchdog_Seconds + 100;
                  Reply : constant JSON_Value :=
                    Tasmota_Request
                      (Config.Host, "PulseTime1 " & Ada.Strings.Fixed.Trim (Pulse'Image, Ada.Strings.Both));
                  Timer : constant JSON_Value := Reply.Get ("PulseTime1");
               begin
                  if Integer'(Timer.Get ("Set")) /= Pulse then
                     raise Constraint_Error with "Tasmota did not confirm watchdog timeout";
                  end if;
               end;
         end case;
      end Refresh_Watchdog;

      Feed_Started : Ada.Real_Time.Time;
   begin
      if Action /= No_Request then
         State.Error := "";
      end if;
      if not Config.Enabled or else not Valid_Host (Config.Host) then
         raise Constraint_Error with "Smart plug is disabled or its host is invalid";
      end if;
      if Action = Turn_Off or else (Action = No_Request and then State.Retry_Off) then
         State.Armed := False;
         State.Retry_Off := True;
         Set_Power (Config, False);
         State.Power := Off;
         State.Retry_Off := False;
      elsif Action = Turn_On then
         --  Arm and verify the timer before any command which can energise the relay.
         State.Armed := False;
         State.Retry_Off := True;
         Prepare_Watchdog (Config);
         Feed_Started := Clock;
         Set_Power (Config, True);
         State.Last_Feed := Feed_Started;
         State.Power := On;
         State.Armed := True;
         State.Retry_Off := False;
      elsif State.Armed and then Clock - State.Last_Feed >= Seconds (Config.Watchdog_Seconds / 2) then
         --  A stalled worker must not restart power after its last lease may have expired.
         State.Armed := False;
         State.Retry_Off := True;
         Set_Power (Config, False);
         State.Power := Off;
         State.Retry_Off := False;
         State.Error := "Watchdog refresh was late. Switch on again to resume.";
         return;
      else
         State.Power := Read_Power (Config);
         if State.Power = Off then
            State.Armed := False;
         elsif State.Armed then
            Feed_Started := Clock;
            --  Reset only the timer. Unlike Power ON, this cannot re-energise a relay which was switched off
            --  between the status query and the refresh (or while an HTTP request was delayed).
            Refresh_Watchdog (Config);
            State.Last_Feed := Feed_Started;
         end if;
      end if;
   exception
      when E : others =>
         State.Retry_Off := State.Retry_Off or else State.Armed;
         State.Armed := False;
         State.Power := Unknown;
         State.Error := +Ada.Exceptions.Exception_Message (E);
   end Poll;

   function Request (Host : Virtual_String; Path : String) return JSON_Value;

   function Request (Host : Virtual_String; Path : String) return JSON_Value is
      Client   : Util.Http.Clients.Client;
      Response : Util.Http.Clients.Response;
   begin
      if not Valid_Host (Host) then
         raise Constraint_Error with "Invalid smart plug host";
      end if;
      Client.Set_Timeout (2.0);
      Client.Add_Header ("User-Agent", "Prunt3D");
      Client.Get ("http://" & Conversions.To_UTF_8_String (Host) & Path, Response);
      if Response.Get_Status /= 200 then
         raise Constraint_Error with "HTTP request failed with status" & Response.Get_Status'Image;
      end if;
      return Read (Response.Get_Body);
   end Request;

   procedure Poll_Plug is new Poll (Request);

   protected body Shared_State is
      procedure Configure (Value : Plug_Config) is
      begin
         if Config /= Value then
            Config := Value;
            Pending_Request := No_Request;
         end if;
      end Configure;

      procedure Submit (Action : Power_Request; Accepted : out Boolean) is
      begin
         Accepted := Config = Status.Config and then Config.Enabled and then Valid_Host (Config.Host);
         if Accepted then
            Pending_Request := Action;
            Status.Busy := True;
         end if;
      end Submit;

      procedure Take (Config_Out : out Plug_Config; Action : out Power_Request) is
      begin
         Config_Out := Config;
         Action := Pending_Request;
         Pending_Request := No_Request;
      end Take;

      procedure Publish (Value : Plug_Status) is
      begin
         Status := Value;
         Status.Busy := Pending_Request /= No_Request;
      end Publish;

      function Snapshot return Plug_Status is
      begin
         if Config /= Status.Config then
            return (Config => Config, Busy => Config.Enabled, others => <>);
         end if;
         return Status;
      end Snapshot;
   end Shared_State;

   procedure Switch (This : in out Controller; Enabled : Boolean; Accepted : out Boolean) is
   begin
      This.State.Submit ((if Enabled then Turn_On else Turn_Off), Accepted);
   end Switch;

   function Status_JSON (This : Controller) return Virtual_String is
      Item  : constant Plug_Status := This.State.Snapshot;
      Value : constant JSON_Value := Create_Object;
   begin
      Value.Set_Field ("Enabled", Item.Config.Enabled);
      Value.Set_Field ("Host", Conversions.To_UTF_8_String (Item.Config.Host));
      Value.Set_Field ("Provider", Item.Config.Provider'Image);
      Value.Set_Field ("Power", Item.Power'Image);
      Value.Set_Field ("Watchdog_Active", Item.Watchdog_Active);
      Value.Set_Field ("Watchdog_Seconds", Item.Config.Watchdog_Seconds);
      Value.Set_Field ("Busy", Item.Busy);
      Value.Set_Field ("Error", Conversions.To_UTF_8_String (Item.Error));
      return +Write (Value);
   end Status_JSON;

   task body Worker is
      State     : Shared_State renames Control.State;
      Config    : Plug_Config;
      Runtime   : Runtime_State;
      Action    : Power_Request;
      Next_Poll : Ada.Real_Time.Time := Time_First;
      Stopping  : Boolean := False;
   begin
      Curl.Initializer.Wait_For_Initialization;
      loop
         begin
            declare
               New_Config : Plug_Config;
            begin
               State.Take (New_Config, Action);
               if Config /= New_Config then
                  if Config.Enabled and then (Runtime.Armed or else Runtime.Retry_Off) then
                     Poll_Plug (Config, Runtime, Turn_Off);
                  end if;
                  Config := New_Config;
                  Runtime := (others => <>);
                  Action := No_Request;
                  Next_Poll := Time_First;
               end if;
            end;
            if Config.Enabled and then (Action /= No_Request or else Clock >= Next_Poll) then
               Poll_Plug (Config, Runtime, Action);
               Next_Poll := Clock + Seconds (5);
            end if;
            State.Publish
              ((Config          => Config,
                Power           => Runtime.Power,
                Watchdog_Active => Runtime.Armed,
                Busy            => False,
                Error           => Runtime.Error));
         exception
            when E : others =>
               --  Configuration errors must leave the worker available for a corrected configuration.
               Runtime.Retry_Off := Runtime.Retry_Off or else Runtime.Armed;
               Runtime.Armed := False;
               Runtime.Power := Unknown;
               Runtime.Error := +Ada.Exceptions.Exception_Message (E);
               State.Publish
                 ((Config          => Config,
                   Power           => Unknown,
                   Watchdog_Active => False,
                   Busy            => False,
                   Error           => Runtime.Error));
         end;
         select
            accept Stop do
               if Config.Enabled and then (Runtime.Armed or else Runtime.Retry_Off) then
                  Poll_Plug (Config, Runtime, Turn_Off);
               end if;
               Stopping := True;
            end Stop;
         or
            delay 0.2;
         end select;
         exit when Stopping;
      end loop;
   end Worker;
end Prunt.Smart_Plugs;
