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
with Ada.Strings;
with Ada.Strings.Fixed;
with Prunt.Web_Server_Resources;

package body Prunt.Web_Server is

   function Trim (S : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Side => Ada.Strings.Both);
   end Trim;

   function Ends_With (Source, Pattern : String) return Boolean is
   begin
      return Source'Length >= Pattern'Length and then Ada.Strings.Fixed.Tail (Source, Pattern'Length) = Pattern;
   end Ends_With;

   overriding procedure Initialize (Client : in out Prunt_Client) is
   begin
      Initialize (HTTP_Client (Client));
   end Initialize;

   overriding procedure Commit (Destination : in out Post_Body_Destination) is
   begin
      null;
   end Commit;

   overriding procedure Put (Destination : in out Post_Body_Destination; Data : String) is
   begin
      Post_Bodies.Append (Destination.Content, Data);
   end Put;

   procedure Write (Stream : access Root_Stream_Type'Class; Item : Post_Body_Destination) is
   begin
      null;
   end Write;

   function Patch_Config_Values (Patch : String) return Unbounded_String is
      Errors : Unbounded_String := To_Unbounded_String ("[");
      Values : Unbounded_String := To_Unbounded_String (Patch);

      procedure Append_Error (Key, Message : String) is
      begin
         if Errors /= "[" then
            Append (Errors, ",");
         end if;
         Append (Errors, "{""Key"":""" & JSON_Escape (Key) & """,""Message"":""" & JSON_Escape (Message) & """}");
      end Append_Error;
   begin
      My_Config.Patch (Values, Append_Error'Access);
      return "{""Values"":" & Values & ",""Errors"":" & Errors & "]}";
   end Patch_Config_Values;

   overriding procedure Reply_HTML
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True)
   is
   begin
      Send_Status_Line (Client, Code, Reason);
      Send_Date (Client);
      Send_Content_Type (Client, "text/html");
      Send_Connection (Client, False);
      Send_Body (Client, Message, Get);
   end Reply_HTML;

   overriding procedure Reply_Text
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True)
   is
   begin
      Send_Status_Line (Client, Code, Reason);
      Send_Date (Client);
      Send_Content_Type (Client, "text/plain");
      Send_Connection (Client, False);
      Send_Body (Client, Message, Get);
   end Reply_Text;

   procedure Reply_JSON
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True)
   is
   begin
      Send_Status_Line (Client, Code, Reason);
      Send_Date (Client);
      Send_Content_Type (Client, "application/json");
      Send_Connection (Client, False);
      Send_Body (Client, Message, Get);
   end Reply_JSON;

   overriding procedure Body_Error
     (Client : in out Prunt_Client; Content : in out Content_Destination'Class; Error : Exception_Occurrence)
   is
   begin
      Save_Occurrence (Client, Error);
      Client.Post_Content.Failed := True;
   end Body_Error;

   overriding procedure Body_Received (Client : in out Prunt_Client; Stream : in out Root_Stream_Type'Class) is
   begin
      null;
   end Body_Received;

   overriding procedure Body_Sent (Client : in out Prunt_Client; Stream : in out Root_Stream_Type'Class; Get : Boolean)
   is
   begin
      null;
   end Body_Sent;

   overriding function Create
     (Factory  : access Prunt_HTTP_Factory;
      Listener : access Connections_Server'Class;
      From     : Sock_Addr_Type)
      return Connection_Ptr
   is
      Result : Prunt_Client_Access;
   begin
      if Get_Clients_Count (Listener.all) < Factory.Max_Connections then
         Result :=
           new Prunt_Client
             (Listener       => Listener.all'Unchecked_Access,
              Request_Length => Factory.Request_Length,
              Input_Size     => Factory.Input_Size,
              Output_Size    => Factory.Output_Size);
         Receive_Body_Tracing (Prunt_Client (Result.all), True);
         Receive_Header_Tracing (Prunt_Client (Result.all), True);
         return Result.all'Unrestricted_Access;
      else
         return null;
      end if;
   end Create;

   procedure Do_Get_Head (Client : in out Prunt_Client; Get : Boolean) is
      Status : Status_Line renames Get_Status_Line (Client);

      use type Web_Server_Resources.Content_Access;
   begin
      if Status.Kind = File then
         if Status.File = "config/schema" then
            Reply_JSON (Client, 200, "OK", To_String (My_Config.Get_Schema), Get);
         elsif Status.File = "config/values" then
            Reply_JSON (Client, 200, "OK", To_String (Patch_Config_Values ("{}")), Get);
         elsif Web_Server_Resources.Get_Content ((if Status.File = "" then "index.html" else Status.File)) /= null then
            Send_Status_Line (Client, 200, "OK");
            Send_Date (Client);
            if Status.File = "" or else Ends_With (Status.File, ".html") then
               Send_Content_Type (Client, "text/html");
            elsif Ends_With (Status.File, ".js") then
               Send_Content_Type (Client, "text/javascript");
            else
               Send_Content_Type (Client, "text/plain");
            end if;
            Send_Connection (Client, False);
            Send_Body
              (Client,
               Web_Server_Resources.Get_Content ((if Status.File = "" then "index.html" else Status.File)).all,
               Get);
         else
            Reply_Text (Client, 404, "Not Found", "File not found.", Get);
         end if;
      else
         Reply_Text (Client, 404, "Not Found", "File not found.", Get);
      end if;
   end Do_Get_Head;

   overriding procedure Do_Get (Client : in out Prunt_Client) is
   begin
      Do_Get_Head (Client, True);
   end Do_Get;

   overriding procedure Do_Head (Client : in out Prunt_Client) is
   begin
      Do_Get_Head (Client, False);
   end Do_Head;

   overriding procedure Do_Post (Client : in out Prunt_Client) is
      Status : Status_Line renames Get_Status_Line (Client);
   begin
      if Status.Kind = File and then Status.File = "config/values" then
         if Client.Post_Content.Failed then
            declare
               Error : Exception_Occurrence;
            begin
               Client.Post_Content.Failed := False;
               Get_Occurrence (Client, Error);
               Reply_Text (Client, 413, "Content Too Large", Exception_Information (Error));
            end;
         else
            Reply_JSON
              (Client,
               200,
               "OK",
               To_String (Patch_Config_Values (Post_Bodies.To_String (Client.Post_Content.Content))),
               True);
         end if;
      else
         Reply_Text (Client, 404, "Not Found", "File not found.", True);
      end if;
   end Do_Post;

   overriding procedure Do_Body (Client : in out Prunt_Client) is
      Status : Status_Line renames Get_Status_Line (Client);
   begin
      Receive_Body (Client, Client.Post_Content'Access);
   end Do_Body;

   function Build_Status_Schema return Unbounded_String is
      Result : Unbounded_String := To_Unbounded_String ("{");

      use type My_Config.Thermistor_Name;
      use type My_Config.Fan_Name;
      use type My_Config.Heater_Name;
      use type My_Config.Stepper_Name;
      use type My_Config.Input_Switch_Name;
   begin
      Append (Result, """Position"":[");
      for A in Axis_Name loop
         Append (Result, """" & Trim (A'Image) & """");
         if A /= Axis_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Thermistor Temperatures"":[");
      for T in My_Config.Thermistor_Name loop
         Append (Result, """" & Trim (T'Image) & """");
         if T /= My_Config.Thermistor_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Stepper Temperatures"":[");
      for S in My_Config.Stepper_Name loop
         Append (Result, """" & Trim (S'Image) & """");
         if S /= My_Config.Stepper_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Board Probe Temperatures"":[");
      for P in Board_Temperature_Probe_Name loop
         Append (Result, """" & Trim (P'Image) & """");
         if P /= Board_Temperature_Probe_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Heater Powers"":[");
      for H in My_Config.Heater_Name loop
         Append (Result, """" & Trim (H'Image) & """");
         if H /= My_Config.Heater_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Switch Is High State"":[");
      for I in My_Config.Input_Switch_Name loop
         Append (Result, """" & Trim (I'Image) & """");
         if I /= My_Config.Input_Switch_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Tachometer Frequencies"":[");
      for F in My_Config.Fan_Name loop
         Append (Result, """" & Trim (F'Image) & """");
         if F /= My_Config.Fan_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "]");

      Append (Result, "}");

      return Result;
   end Build_Status_Schema;

   function Build_Status_Values return Unbounded_String is
      Result : Unbounded_String := To_Unbounded_String ("{");
      Pos    : Position         := Get_Position;

      use type My_Config.Thermistor_Name;
      use type My_Config.Fan_Name;
      use type My_Config.Heater_Name;
      use type My_Config.Stepper_Name;
      use type My_Config.Input_Switch_Name;
   begin
      if Fatal_Exception_Occurrence_Holder.Is_Set then
         declare
            Occurrence : Ada.Exceptions.Exception_Occurrence;
         begin
            Fatal_Exception_Occurrence_Holder.Get (Occurrence);
            return
              To_Unbounded_String ("{""Fatal Exception"":""") &
              JSON_Escape (Ada.Exceptions.Exception_Information (Occurrence)) & """}";
         end;
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

      return Result;
   end Build_Status_Values;

   --  function Response_Pause_Pause (Request : EWS.HTTP.Request_P) return EWS.HTTP.Response'Class is
   --  begin
   --     if EWS.HTTP.Get_Method (Request.all) = "POST" then
   --        Pause_Stepgen;
   --        return Result : EWS.Dynamic.Dynamic_Response (Request) do
   --           Result.Set_Content_Type (EWS.Types.Plain);
   --           Result.Set_Content ("");
   --        end return;
   --     else
   --        return EWS.HTTP.Not_Implemented (Request);
   --     end if;
   --  end Response_Pause_Pause;

   --  function Response_Pause_Resume (Request : EWS.HTTP.Request_P) return EWS.HTTP.Response'Class is
   --  begin
   --     if EWS.HTTP.Get_Method (Request.all) = "POST" then
   --        Resume_Stepgen;
   --        return Result : EWS.Dynamic.Dynamic_Response (Request) do
   --           Result.Set_Content_Type (EWS.Types.Plain);
   --           Result.Set_Content ("");
   --        end return;
   --     else
   --        return EWS.HTTP.Not_Implemented (Request);
   --     end if;
   --  end Response_Pause_Resume;

   task body Server is
      Factory :
        aliased Prunt_HTTP_Factory
          (Request_Length  => Buffer_Size,
           Input_Size      => Buffer_Size,
           Output_Size     => Buffer_Size,
           Max_Connections => 1000);

      Server : GNAT.Sockets.Server.Connections_Server (Factory'Access, Port);
   begin
      --  EWS.Dynamic.Register (Response_Status_Schema'Unrestricted_Access, "/status/schema");
      --  EWS.Dynamic.Register (Response_Status_Values'Unrestricted_Access, "/status/values");
      --  EWS.Dynamic.Register (Response_Pause_Pause'Unrestricted_Access, "/pause/pause");
      --  EWS.Dynamic.Register (Response_Pause_Resume'Unrestricted_Access, "/pause/resume");

      --  EWS.Server.Serve (Using_Port => Port, With_Stack => 4_000_000);

      --  Trace_On (Factory, Received => GNAT.Sockets.Server.Trace_Decoded, Sent => GNAT.Sockets.Server.Trace_Decoded);

      loop
         delay 100.0;
      end loop;
   end Server;

end Prunt.Web_Server;
