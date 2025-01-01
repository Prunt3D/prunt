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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with EWS.Dynamic;
with EWS.HTTP;
with EWS.Server;
with EWS.Types;
with EWS_Htdocs;
with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

package body Prunt.Web_Server is

   function Trim (S : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Side => Ada.Strings.Both);
   end Trim;

   function Response_Config_Schema (Request : EWS.HTTP.Request_P) return EWS.HTTP.Response'Class is
   begin
      if EWS.HTTP.Get_Method (Request.all) = "GET" then
         return Result : EWS.Dynamic.Dynamic_Response (Request) do
            Result.Set_Content_Type (EWS.Types.JSON);
            Result.Set_Content (My_Config.Get_Schema);
         end return;
      else
         return EWS.HTTP.Not_Implemented (Request);
      end if;
   end Response_Config_Schema;

   function Response_Config_Values (Request : EWS.HTTP.Request_P) return EWS.HTTP.Response'Class is
      Errors : Unbounded_String := To_Unbounded_String ("[");
      Values : Unbounded_String;

      procedure Append_Error (Key, Message : String) is
      begin
         if Errors /= "[" then
            Append (Errors, ",");
         end if;
         Append (Errors, "{""Key"":""" & JSON_Escape (Key) & """,""Message"":""" & JSON_Escape (Message) & """}");
      end Append_Error;
   begin
      if EWS.HTTP.Get_Method (Request.all) = "GET" then
         Values := My_Config.Get_Values_And_Validate (Append_Error'Unrestricted_Access);
         return Result : EWS.Dynamic.Dynamic_Response (Request) do
            Result.Set_Content_Type (EWS.Types.JSON);
            Result.Set_Content ("{""Values"":" & Values & ",""Errors"":" & Errors & "]}");
         end return;
      elsif EWS.HTTP.Get_Method (Request.all) = "POST" then
         Values := EWS.HTTP.Get_Body (Request.all);
         My_Config.Patch (Values, Append_Error'Access);
         return Result : EWS.Dynamic.Dynamic_Response (Request) do
            Result.Set_Content_Type (EWS.Types.JSON);
            Result.Set_Content ("{""Values"":" & Values & ",""Errors"":" & Errors & "]}");
         end return;
      else
         return EWS.HTTP.Not_Implemented (Request);
      end if;
   end Response_Config_Values;

   function Response_Status_Values (Request : EWS.HTTP.Request_P) return EWS.HTTP.Response'Class is
      Result : Unbounded_String := To_Unbounded_String ("{");
      Pos    : Position         := Get_Position;

      use type My_Config.Thermistor_Name;
      use type My_Config.Fan_Name;
      use type My_Config.Heater_Name;
      use type My_Config.Stepper_Name;
      use type My_Config.Input_Switch_Name;
   begin
      if EWS.HTTP.Get_Method (Request.all) /= "GET" then
         return EWS.HTTP.Not_Implemented (Request);
      end if;

      Append (Result, """Position"":{");
      for A in Axis_Name loop
         Append (Result, """" & Trim (A'Image) & """:" & Pos (A)'Image);
         if A /= Axis_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Thermistor Temperatures"":{");
      for T in My_Config.Thermistor_Name loop
         Append (Result, """" & Trim (T'Image) & """:" & Get_Thermistor_Temperature (T)'Image);
         if T /= My_Config.Thermistor_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Stepper Temperatures"":{");
      for S in My_Config.Stepper_Name loop
         Append (Result, """" & Trim (S'Image) & """:" & Get_Stepper_Temperature (S)'Image);
         if S /= My_Config.Stepper_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Board Probe Temperatures"":{");
      for P in Board_Temperature_Probe_Name loop
         Append (Result, """" & Trim (P'Image) & """:" & Get_Board_Temperature (P)'Image);
         if P /= Board_Temperature_Probe_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Heater Powers"":{");
      for H in My_Config.Heater_Name loop
         Append (Result, """" & Trim (H'Image) & """:" & Get_Heater_Power (H)'Image);
         if H /= My_Config.Heater_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Switch Is High State"":{");
      for I in My_Config.Input_Switch_Name loop
         Append
           (Result,
            """" & Trim (I'Image) & """:" & (if Get_Input_Switch_State (I) = High_State then "true" else "false"));
         if I /= My_Config.Input_Switch_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Tachometer Frequencies"":{");
      for F in My_Config.Fan_Name loop
         Append (Result, """" & Trim (F'Image) & """:" & Get_Tachometer_Frequency (F)'Image);
         if F /= My_Config.Fan_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Stepgen Is Paused"":" & (if Is_Stepgen_Paused then "true" else "false"));

      Append (Result, "}");

      return Response : EWS.Dynamic.Dynamic_Response (Request) do
         Response.Set_Content_Type (EWS.Types.JSON);
         Response.Set_Content (Result);
      end return;
   end Response_Status_Values;

   task body Server is
   begin
      EWS.Dynamic.Register (Response_Config_Schema'Unrestricted_Access, "/config/schema");
      EWS.Dynamic.Register (Response_Config_Values'Unrestricted_Access, "/config/values");
      EWS.Dynamic.Register (Response_Status_Values'Unrestricted_Access, "/status/values");

      EWS.Server.Serve (Using_Port => 8_080, With_Stack => 4_000_000);

      loop
         delay 100.0;
      end loop;
   end Server;

end Prunt.Web_Server;
