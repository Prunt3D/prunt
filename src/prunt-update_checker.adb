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

with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with Util.Http.Clients.Curl;
with Util.Http.Clients;
with VSS.Strings.Conversions;

package body Prunt.Update_Checker is

   pragma Extensions_Allowed (On);

   task body Checker is
   begin
      Util.Http.Clients.Curl.Register;

      case Details.Method is
         when None   =>
            loop
               accept Get_Update_URL (Update_Available : out Boolean; Update_URL : out Virtual_String) do
                  Update_Available := False;
                  Update_URL := "";
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
                    ("https://api.github.com/repos/"
                     & Conversions.To_UTF_8_String (Details.Repository)
                     & "/releases/latest",
                     Response);

                  declare
                     Response_Body       : constant String := Response.Get_Body;
                     Response_JSON       : constant JSON_Value := Read (Response_Body);
                     Current_Release_URL : constant String := Get (Response_JSON, "html_url");
                     Current_Release_Tag : constant String := Get (Response_JSON, "tag_name");
                  begin
                     while Clock < Next_Check_Time loop
                        select
                           accept Get_Update_URL (Update_Available : out Boolean; Update_URL : out Virtual_String) do
                              Update_Available :=
                                Conversions.To_UTF_8_String (Details.Expected_Tag) /= Current_Release_Tag;
                              Update_URL := Conversions.To_Virtual_String (Current_Release_URL);
                           end Get_Update_URL;
                        or
                           delay until Next_Check_Time;
                        end select;
                     end loop;
                  end;
               exception
                  when E : others =>
                     My_Logger.Log
                       (Conversions.To_Virtual_String
                          ("Exception in update checker, retrying in 60 minutes:"
                           & Ada.Characters.Latin_1.CR
                           & Ada.Characters.Latin_1.LF
                           & Ada.Exceptions.Exception_Information (E)));
                     delay 3600.0;
               end;
            end loop;
      end case;
   end Checker;

end Prunt.Update_Checker;
