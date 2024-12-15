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

package body Prunt.Web_Server is

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

   task body Server is
   begin
      EWS.Dynamic.Register (Response_Config_Schema'Unrestricted_Access, "/config/schema");
      EWS.Dynamic.Register (Response_Config_Values'Unrestricted_Access, "/config/values");

      EWS.Server.Serve (Using_Port => 8_080, With_Stack => 4_000_000);

      loop
         delay 100.0;
      end loop;
   end Server;

end Prunt.Web_Server;
