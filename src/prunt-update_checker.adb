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

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Util.Http.Clients;
with Util.Http.Clients.Curl;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Ada.Real_Time; use Ada.Real_Time;

package body Prunt.Update_Checker is

   task body Checker is
   begin
      Util.Http.Clients.Curl.Register;

      case Details.Method is
         when None =>
            loop
               accept Get_Update_URL (Update_Available : out Boolean; Update_URL : out Unbounded_String) do
                  Update_Available := False;
                  Update_URL := To_Unbounded_String ("");
               end Get_Update_URL;
            end loop;

         when Github =>
            loop
               declare
                  Next_Check_Time : constant Ada.Real_Time.Time := Clock + Minutes (24 * 60);
                  Client          : Util.Http.Clients.Client;
                  Response        : Util.Http.Clients.Response;
               begin
                  Client.Add_Header ("User-Agent", "Prunt3D-Update-Checker");
                  Client.Get
                    (To_String ("https://api.github.com/repos/" & Details.Repository & "/releases/latest"), Response);

                  declare
                     Response_Body       : constant String := Response.Get_Body;
                     Response_JSON       : constant JSON_Value := Read (Response_Body);
                     Current_Release_URL : constant String := Get (Response_JSON, "html_url");
                     Current_Release_Tag : constant String := Get (Response_JSON, "tag_name");
                  begin
                     while Clock < Next_Check_Time loop
                        select
                           accept Get_Update_URL (Update_Available : out Boolean; Update_URL : out Unbounded_String) do
                              Update_Available := Details.Expected_Tag /= Current_Release_Tag;
                              Update_URL := To_Unbounded_String (Current_Release_URL);
                           end Get_Update_URL;
                        or
                           delay until Next_Check_Time;
                        end select;
                     end loop;
                  end;
               exception
                  when E : others =>
                     My_Logger.Log
                       ("Exception in update checker, retrying in 60 minutes:"
                        & Ada.Characters.Latin_1.CR
                        & Ada.Exceptions.Exception_Information (E));
                     delay 3600.0;
               end;
            end loop;
      end case;
   end Checker;

end Prunt.Update_Checker;
