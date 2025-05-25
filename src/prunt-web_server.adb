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

with Ada.Directories; use Ada.Directories;
with Ada.Real_Time;   use Ada.Real_Time;
with Ada.Strings;
with Ada.Strings.Fixed;
with System;
with GNAT.Sockets;
with Ada.Containers.Ordered_Sets;

package body Prunt.Web_Server is

   procedure Wait_For_User_To_Allow_Update is
   begin
      Startup_Manager.Set_Update_Required;
      Startup_Manager.Wait_For_Update_Allowed;
   end Wait_For_User_To_Allow_Update;

   procedure Notify_Startup_Done is
   begin
      Startup_Manager.Set_Startup_Done;
   end Notify_Startup_Done;

   protected body Startup_Manager is
      entry Wait_For_Update_Allowed when Update_Allowed is
      begin
         null;
      end Wait_For_Update_Allowed;

      procedure Set_Update_Required is
      begin
         Update_Required := True;
      end Set_Update_Required;

      function Get_Update_Required return Boolean is
      begin
         return Update_Required;
      end Get_Update_Required;

      procedure Set_Startup_Done is
      begin
         Startup_Done := True;
      end Set_Startup_Done;

      procedure Clear_Startup_Done is
      begin
         Startup_Done := False;
      end Clear_Startup_Done;

      function Get_Startup_Done return Boolean is
      begin
         return Startup_Done;
      end Get_Startup_Done;

      procedure Set_Update_Allowed is
      begin
         Update_Allowed := True;
      end Set_Update_Allowed;

      function Get_Update_Allowed return Boolean is
      begin
         return Update_Allowed;
      end Get_Update_Allowed;
   end Startup_Manager;

   function Trim (S : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (S, Side => Ada.Strings.Both);
   end Trim;

   function Ends_With (Source, Pattern : String) return Boolean is
   begin
      return Source'Length >= Pattern'Length and then Ada.Strings.Fixed.Tail (Source, Pattern'Length) = Pattern;
   end Ends_With;

   function Starts_With (Source, Pattern : String) return Boolean is
   begin
      return Source'Length >= Pattern'Length and then Ada.Strings.Fixed.Head (Source, Pattern'Length) = Pattern;
   end Starts_With;

   overriding
   procedure Read
     (Stream : in out Array_Stream_Type; Item : out Stream_Element_Array; Last : out Stream_Element_Offset) is
   begin
      if Stream.Position > Stream.Content'Last then
         Stream.Done := True;
      end if;

      if Stream.Done then
         Last := Item'First - 1;
      else
         for I in Item'Range loop
            Last := I;
            Item (I) := Stream.Content (Stream.Position);

            if Stream.Position = Stream.Content'Last then
               Stream.Done := True;
               exit;
            else
               Stream.Position := Stream.Position + 1;
            end if;
         end loop;
      end if;
   end Read;

   overriding
   procedure Write (Stream : in out Array_Stream_Type; Item : Stream_Element_Array) is
   begin
      raise Constraint_Error with "Writing not supported.";
   end Write;

   overriding
   function Get (Source : access Directory_Content) return String is
      Dir : Directory_Entry_Type;
   begin
      case Source.Step is
         when Starting =>
            Source.Step := First_Entry;
            return "[";

         when First_Entry | Continuing_Entries =>
            if More_Entries (Source.Search) then
               Get_Next_Entry (Source.Search, Dir);
               if Source.Step = First_Entry then
                  Source.Step := Continuing_Entries;
                  return """" & JSON_Escape (Simple_Name (Dir)) & """";
               else
                  return ",""" & Simple_Name (Dir) & """";
               end if;
            else
               Source.Step := Finished;
               return "]";
            end if;

         when Finished =>
            return "";
      end case;
   end Get;

   overriding
   procedure Finalize (Source : in out Directory_Content) is
   begin
      Finalize (Content_Source (Source));
   end Finalize;

   overriding
   function Get (Source : access Unbounded_String_Source) return String is
      New_Next_Start : constant Positive :=
        Positive'Min (Source.Next_Start + Buffer_Size - 100, Ada.Strings.Unbounded.Length (Source.Content) + 1);
      Result         : constant String := Slice (Source.Content, Source.Next_Start, New_Next_Start - 1);
   begin
      Source.Next_Start := New_Next_Start;
      return Result;
   end Get;

   overriding
   procedure Initialize (Client : in out Prunt_Client) is
   begin
      Initialize (HTTP_Client (Client));
   end Initialize;

   overriding
   procedure Connected (Client : in out Prunt_Client) is
   begin
      GNAT.Sockets.Set_Socket_Option (Client.Get_Socket, IP_Protocol_For_TCP_Level, (Keep_Alive_Idle, 30));
      GNAT.Sockets.Set_Socket_Option (Client.Get_Socket, IP_Protocol_For_TCP_Level, (Keep_Alive_Interval, 10));
      GNAT.Sockets.Set_Socket_Option (Client.Get_Socket, IP_Protocol_For_TCP_Level, (Keep_Alive_Count, 3));
      GNAT.Sockets.Set_Socket_Option (Client.Get_Socket, Socket_Level, (Keep_Alive, True));

      Connected (HTTP_Client (Client));
   end Connected;

   overriding
   procedure Commit (Destination : in out Post_Body_Destination) is
   begin
      null;
   end Commit;

   overriding
   procedure Put (Destination : in out Post_Body_Destination; Data : String) is
   begin
      Post_Bodies.Append (Destination.Content, Data);
   end Put;

   procedure Write (Stream : access Root_Stream_Type'Class; Item : Extra_Client_Content) is
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

   overriding
   procedure Reply_HTML
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True) is
   begin
      Send_Status_Line (Client, Code, Reason);
      Send_Date (Client);
      Send_Content_Type (Client, "text/html");
      Send_Connection (Client, Persistent => False);
      Send (Client, "Cache-Control: no-cache, no-store, must-revalidate" & CRLF);
      Send (Client, "Pragma: no-cache" & CRLF);
      Send (Client, "Expires: 0" & CRLF);
      Send_Body (Client, Message, Get);
   end Reply_HTML;

   overriding
   procedure Reply_Text
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True) is
   begin
      Send_Status_Line (Client, Code, Reason);
      Send_Date (Client);
      Send_Content_Type (Client, "text/plain");
      Send_Connection (Client, Persistent => False);
      Send (Client, "Cache-Control: no-cache, no-store, must-revalidate" & CRLF);
      Send (Client, "Pragma: no-cache" & CRLF);
      Send (Client, "Expires: 0" & CRLF);
      Send_Body (Client, Message, Get);
   end Reply_Text;

   procedure Reply_JSON
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True) is
   begin
      Send_Status_Line (Client, Code, Reason);
      Send_Date (Client);
      Send_Content_Type (Client, "application/json");
      Send_Connection (Client, Persistent => False);
      Send (Client, "Cache-Control: no-cache, no-store, must-revalidate" & CRLF);
      Send (Client, "Pragma: no-cache" & CRLF);
      Send (Client, "Expires: 0" & CRLF);
      Send_Body (Client, Message, Get);
   end Reply_JSON;

   procedure Reply_JSON
     (Client  : in out Prunt_Client;
      Code    : Positive;
      Reason  : String;
      Message : Unbounded_String;
      Get     : Boolean := True) is
   begin
      Send_Status_Line (Client, Code, Reason);
      Send_Date (Client);
      Send_Content_Type (Client, "application/json");
      Send_Connection (Client, Persistent => False);
      Send (Client, "Cache-Control: no-cache, no-store, must-revalidate" & CRLF);
      Send (Client, "Pragma: no-cache" & CRLF);
      Send (Client, "Expires: 0" & CRLF);

      Client.Content.Big_String_Content.Content := Message;
      Client.Content.Big_String_Content.Next_Start := 1;

      Send_Body (Client, Client.Content.Big_String_Content'Access, Get);
   end Reply_JSON;

   overriding
   procedure Body_Error
     (Client : in out Prunt_Client; Content : in out Content_Destination'Class; Error : Exception_Occurrence) is
   begin
      Save_Occurrence (Client, Error);
      --  It doesn't matter if we set both fields here as Do_Put or Do_Post will be called later and will use the
      --  correct field.
      Client.Content.Post_Content.Failed := True;
      Client.Content.Put_Fail_Reason := Unhandled_Exception_Kind;
   end Body_Error;

   overriding
   procedure Body_Received (Client : in out Prunt_Client; Stream : in out Root_Stream_Type'Class) is
   begin
      if Is_Open (Client.Content.File) then
         Close (Client.Content.File);
      end if;
   exception
      when E : others =>
         Save_Occurrence (Client, E);
         Client.Content.Put_Fail_Reason := Unhandled_Exception_Kind;
   end Body_Received;

   overriding
   procedure Body_Sent (Client : in out Prunt_Client; Stream : in out Root_Stream_Type'Class; Get : Boolean) is
   begin
      if Is_Open (Client.Content.File) then
         Close (Client.Content.File);
      end if;
   end Body_Sent;

   overriding
   function Create
     (Factory : access Prunt_HTTP_Factory; Listener : access Connections_Server'Class; From : Sock_Addr_Type)
      return Connection_Ptr
   is
      Result : Prunt_Client_Access;
   begin
      if Get_Clients_Count (Listener.all) < Factory.Max_Connections then
         if Get_Clients_Count (Listener.all) > Positive (Float (Factory.Max_Connections) * 0.7) then
            --  TODO: Should we force the printer to pause and the heaters to cool down if the maximum is reached?
            My_Logger.Log
              ("Warning: More than 70% of available HTTP connections used (Maximum = "
               & Factory.Max_Connections'Image
               & ")");
         end if;

         Result :=
           new Prunt_Client
                 (Listener => Listener.all'Unchecked_Access,
                  Request_Length => Factory.Request_Length,
                  Input_Size => Factory.Input_Size,
                  Output_Size => Factory.Output_Size);

         --  Receive_Body_Tracing (Prunt_Client (Result.all), True);
         --  Receive_Header_Tracing (Prunt_Client (Result.all), True);
         Result.Content.Self_Access := Result;

         return Result.all'Unrestricted_Access;
      else
         My_Logger.Log
           ("Warning: All available HTTP connections used (Maximum = " & Factory.Max_Connections'Image & ")");
         return null;
      end if;
   end Create;

   procedure Do_Get_Head (Client : in out Prunt_Client; Get : Boolean) is
      Status : Status_Line renames Get_Status_Line (Client);

      use type Web_Server_Resources.Content_Access;
   begin
      --  TODO: Status.Kind may be None. Are there any browsers that send a request with no request target, and if so
      --  should we handle it as if the target was "/"?
      if Status.Kind = File then
         if Status.File = "config/schema" then
            Reply_JSON (Client, 200, "OK", My_Config.Get_Schema, Get);
         elsif Status.File = "config/values" then
            Reply_JSON (Client, 200, "OK", Patch_Config_Values ("{}"), Get);
         elsif Status.File = "status/schema" then
            Reply_JSON (Client, 200, "OK", Build_Status_Schema, Get);
         elsif Status.File = "status/values" then
            Reply_JSON (Client, 200, "OK", Build_Status_Values, Get);
         elsif Status.File = "update_check" then
            declare
               Update_Available : Boolean;
               Update_URL       : Unbounded_String;
            begin
               select
                  My_Update_Checker.Checker.Get_Update_URL (Update_Available, Update_URL);
                  Reply_JSON
                    (Client,
                     200,
                     "OK",
                     "{""Available"":"
                     & (if Update_Available then "true" else "false")
                     & ",""URL"":"""
                     & JSON_Escape (Update_URL)
                     & """}",
                     Get);
               or
                  delay 30.0;
                  Reply_JSON (Client, 500, "Internal Server Error", "Update check timed out.", Get);
               end select;
            end;
         elsif Web_Server_Resources.Get_Content ((if Status.File = "" then "index.html" else Status.File)) /= null then
            Send_Status_Line (Client, 200, "OK");
            Send_Date (Client);
            if Status.File = "" or else Ends_With (Status.File, ".html") then
               Send_Content_Type (Client, "text/html");
            elsif Ends_With (Status.File, ".js") then
               Send_Content_Type (Client, "text/javascript");
            elsif Ends_With (Status.File, ".css") then
               Send_Content_Type (Client, "text/css");
            elsif Ends_With (Status.File, ".ico") then
               Send_Content_Type (Client, "image/vnd.microsoft.icon");
            elsif Ends_With (Status.File, ".ico") then
               Send_Content_Type (Client, "image/png");
            elsif Ends_With (Status.File, ".svg") then
               Send_Content_Type (Client, "image/svg+xml");
            elsif Ends_With (Status.File, ".webmanifest") then
               Send_Content_Type (Client, "application/manifest+json");
            elsif Ends_With (Status.File, ".xml") then
               Send_Content_Type (Client, "text/xml");
            else
               Send_Content_Type (Client, "text/plain");
            end if;
            Send_Connection (Client, Persistent => False);
            Send (Client, "Cache-Control: no-cache, no-store, must-revalidate" & CRLF);
            Send (Client, "Pragma: no-cache" & CRLF);
            Send (Client, "Expires: 0" & CRLF);
            Client.Content.Array_Stream.Content :=
              Web_Server_Resources.Get_Content ((if Status.File = "" then "index.html" else Status.File));
            Client.Content.Array_Stream.Position := Client.Content.Array_Stream.Content.all'First;
            Client.Content.Array_Stream.Done := False;
            Send_Body (Client, Client.Content.Array_Stream'Access, Get);
         elsif Status.File = "prunt-is-enabled" then
            if My_Config.Prunt_Is_Enabled then
               Reply_JSON (Client, 200, "OK", "true", Get);
            else
               Reply_JSON (Client, 200, "OK", "false", Get);
            end if;
         elsif Status.File = "uploads" or else Status.File = "uploads/" then
            --  uploads/ is provided for users manually entering the URL.
            if Kind ("uploads") /= Directory then
               Reply_Text
                 (Client,
                  500,
                  "Internal Server Error",
                  """uploads"" is not a directory. Delete or rename the file named uploads and restart Prunt.",
                  Get);
            else
               begin
                  Client.Content.Uploads_Directory_Content.Step := Starting;
                  Start_Search
                    (Search    => Client.Content.Uploads_Directory_Content.Search,
                     Directory => "uploads",
                     Pattern   => "*",
                     Filter    => (Ordinary_File => True, others => False));

                  Send_Status_Line (Client, 200, "OK");
                  Send_Date (Client);
                  Send_Server (Client);
                  Send_Content_Type (Client, "application/json");
                  Send_Connection (Client, Persistent => False);
                  Send (Client, "Cache-Control: no-cache, no-store, must-revalidate" & CRLF);
                  Send (Client, "Pragma: no-cache" & CRLF);
                  Send (Client, "Expires: 0" & CRLF);
                  Send_Body (Client, Client.Content.Uploads_Directory_Content'Access, Get);
               exception
                  when Ada.Directories.Use_Error =>
                     Reply_Text (Client, 500, "Internal Server Error", """uploads"" directory is not readable.", Get);
               end;
            end if;
         elsif Starts_With (Status.File, "uploads/") then
            if Kind ("uploads") /= Directory then
               Reply_Text
                 (Client,
                  500,
                  "Internal Server Error",
                  """uploads"" is not a directory. Delete or rename the file named uploads and restart Prunt.",
                  Get);
            else
               begin
                  declare
                     File_Name : constant String :=
                       Status.File (Status.File'First + String'("uploads/")'Length .. Status.File'Last);
                     File_Path : constant String := Compose (Containing_Directory => "uploads", Name => File_Name);
                  begin
                     if not Exists (File_Path) then
                        Reply_Text (Client, 404, "Not Found", "File not found.", Get);
                     elsif Kind (File_Path) /= Ordinary_File then
                        Reply_Text
                          (Client,
                           400,
                           "Bad Request",
                           "File is not regular. Only regular files directly in uploads directory may be accessed.",
                           Get);
                     else
                        if Is_Open (Client.Content.File) then
                           Close (Client.Content.File);
                        end if;
                        Open (Client.Content.File, In_File, File_Path);
                        Send_Status_Line (Client, 200, "OK");
                        Send_Date (Client);
                        Send_Server (Client);
                        Send_Content_Type (Client, "application/octet-stream");
                        Send_Connection (Client, Persistent => False);
                        Send (Client, "Cache-Control: no-cache, no-store, must-revalidate" & CRLF);
                        Send (Client, "Pragma: no-cache" & CRLF);
                        Send (Client, "Expires: 0" & CRLF);
                        Send_Body
                          (Client,
                           Stream (Client.Content.File),
                           Stream_Element_Count (Size (Client.Content.File)),
                           Get);
                     end if;
                  end;
               exception
                  when Ada.Directories.Name_Error =>
                     Reply_Text
                       (Client,
                        400,
                        "Bad Request",
                        "File name is malformed. Only regular files directly in uploads directory may be accessed.",
                        Get);
               end;
            end if;
         else
            Reply_Text (Client, 404, "Not Found", "File not found.", Get);
         end if;
      else
         Reply_Text (Client, 404, "Not Found", "File not found.", Get);
      end if;
   exception
      when E : others =>
         Reply_Text (Client, 500, "Internal Server Error", Exception_Information (E), Get);
         My_Logger.Log ("Unhandled exception in Web_Server.Do_Get_Head: " & Exception_Information (E));
         Save_Occurrence (Client, E);
   end Do_Get_Head;

   overriding
   procedure Do_Get (Client : in out Prunt_Client) is
   begin
      Do_Get_Head (Client, True);
   end Do_Get;

   overriding
   procedure Do_Head (Client : in out Prunt_Client) is
   begin
      Do_Get_Head (Client, False);
   end Do_Head;

   overriding
   procedure Do_Post (Client : in out Prunt_Client) is
      Status : Status_Line renames Get_Status_Line (Client);
   begin
      if Client.Content.Post_Content.Failed then
         declare
            Error : Exception_Occurrence;
         begin
            Client.Content.Post_Content.Failed := False;
            Get_Occurrence (Client, Error);
            Reply_Text (Client, 413, "Content Too Large", Exception_Information (Error), True);
         end;
      elsif Status.Kind = File then
         if Status.File = "pause/pause" then
            Pause_Stepgen;
            Reply_Text (Client, 204, "No Content", "", True);
         elsif Status.File = "pause/resume" then
            Resume_Stepgen;
            Reply_Text (Client, 204, "No Content", "", True);
         elsif Status.File = "config/values" then
            Reply_JSON
              (Client,
               200,
               "OK",
               Patch_Config_Values (Post_Bodies.To_String (Client.Content.Post_Content.Content)),
               True);
         elsif Status.File = "run-file" then
            if Kind ("uploads") /= Directory then
               Reply_Text
                 (Client,
                  500,
                  "Internal Server Error",
                  """uploads"" is not a directory. Delete or rename the file named uploads and restart Prunt.",
                  True);
            else
               declare
                  File_Path : constant String :=
                    Compose
                      (Containing_Directory => "uploads",
                       Name                 => Post_Bodies.To_String (Client.Content.Post_Content.Content));
                  Succeeded : Boolean;
               begin
                  if Exists (File_Path) and then Kind (File_Path) /= Ordinary_File then
                     Reply_Text (Client, 500, "Internal Server Error", "File exists but is not a regular file.", True);
                  else
                     Submit_Gcode_File (File_Path, Succeeded);
                     if Succeeded then
                        Reply_Text (Client, 204, "No Content", "", True);
                     else
                        Reply_Text
                          (Client,
                           500,
                           "Internal Server Error",
                           "Can not run file while another file is running.",
                           True);
                     end if;
                  end if;
               end;
            end if;
         elsif Status.File = "run-command" then
            declare
               Succeeded : Boolean;
            begin
               Submit_Gcode_Command (Post_Bodies.To_String (Client.Content.Post_Content.Content), Succeeded);
               if Succeeded then
                  Reply_Text (Client, 204, "No Content", "", True);
               else
                  Reply_Text
                    (Client,
                     500,
                     "Internal Server Error",
                     "Can not run command while another command or file is running.",
                     True);
               end if;
            end;
         elsif Status.File = "allow-firmware-update" then
            Startup_Manager.Set_Update_Allowed;
            Reply_Text (Client, 204, "No Content", "", True);
         elsif Status.File = "reload-server" then
            Reload_Server;
            Reply_Text (Client, 204, "No Content", "", True);
         else
            Reply_Text (Client, 404, "Not Found", "File not found.", True);
         end if;
      else
         Reply_Text (Client, 404, "Not Found", "File not found.", True);
      end if;
   exception
      when E : others =>
         Reply_Text (Client, 500, "Internal Server Error", Exception_Information (E), True);
         My_Logger.Log ("Unhandled exception in Web_Server.Do_Post: " & Exception_Information (E));
         Save_Occurrence (Client, E);
   end Do_Post;

   overriding
   procedure Do_Put (Client : in out Prunt_Client) is
   begin
      case Client.Content.Put_Fail_Reason is
         when No_Failure_Kind =>
            Reply_Text (Client, 204, "No Content", "", True);
            --  TODO: Currently we send a 204 when the user attempts to upload an empty file, but the file is not
            --  actually created or replaced. We should either send a reply that indicated that or write the file.

         when Uploads_Not_Dir_Kind =>
            Reply_Text
              (Client,
               500,
               "Internal Server Error",
               """uploads"" is not a directory. Delete or rename the file named uploads and restart Prunt.",
               True);

         when File_Not_Regular_Kind =>
            Reply_Text
              (Client,
               500,
               "Internal Server Error",
               "File exists and is not a regular file. Only regular files may be replaced.",
               True);

         when File_Name_Malformed_Kind =>
            Reply_Text
              (Client,
               400,
               "Bad Request",
               "File name is malformed. Only regular files directly in the uploads directory may be accessed.",
               True);

         when Wrong_Directory_Kind =>
            Reply_Text (Client, 400, "Bad Request", "Files may only be uploaded to the uploads directory.", True);

         when Wrong_Request_Target_Kind =>
            Reply_Text (Client, 400, "Bad Request", "Request target must be a file.", True);

         when Unhandled_Exception_Kind =>
            declare
               Error : Exception_Occurrence;
            begin
               Get_Occurrence (Client, Error);
               Reply_Text (Client, 500, "Internal Server Error", Exception_Information (Error));
            end;
      end case;
   exception
      when E : others =>
         Reply_Text (Client, 500, "Internal Server Error", Exception_Information (E), True);
         My_Logger.Log ("Unhandled exception in Web_Server.Do_Put: " & Exception_Information (E));
         Save_Occurrence (Client, E);
   end Do_Put;

   overriding
   procedure Do_Body (Client : in out Prunt_Client) is
      Status : Status_Line renames Get_Status_Line (Client);
   begin
      if Client.Get_Method = HTTP_POST then
         Receive_Body (Client, Client.Content.Post_Content'Access);
      elsif Client.Get_Method = HTTP_PUT then
         begin
            Client.Content.Put_Fail_Reason := No_Failure_Kind;
            if Status.Kind = File then
               if Starts_With (Status.File, "uploads/") then
                  if Kind ("uploads") /= Directory then
                     Client.Content.Put_Fail_Reason := Uploads_Not_Dir_Kind;
                  end if;

                  declare
                     File_Name : constant String :=
                       Status.File (Status.File'First + String'("uploads/")'Length .. Status.File'Last);
                     File_Path : constant String := Compose (Containing_Directory => "uploads", Name => File_Name);
                  begin
                     if Exists (File_Path) and then Kind (File_Path) /= Ordinary_File then
                        Client.Content.Put_Fail_Reason := File_Not_Regular_Kind;
                     else
                        if Is_Open (Client.Content.File) then
                           Close (Client.Content.File);
                        end if;
                        if Exists (File_Path) then
                           Open (Client.Content.File, Out_File, File_Path);
                        else
                           Create (Client.Content.File, Out_File, File_Path);
                        end if;

                        Receive_Body (Client, Stream (Client.Content.File));
                     end if;
                  end;
               else
                  Client.Content.Put_Fail_Reason := Wrong_Directory_Kind;
               end if;
            else
               Client.Content.Put_Fail_Reason := Wrong_Request_Target_Kind;
            end if;
         exception
            when Ada.Directories.Name_Error =>
               Client.Content.Put_Fail_Reason := File_Name_Malformed_Kind;
            when E : others =>
               Client.Content.Put_Fail_Reason := Unhandled_Exception_Kind;
               Save_Occurrence (Client, E);
         end;
      end if;
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

      Append (Result, """Thermistor_Temperatures"":[");
      for T in My_Config.Thermistor_Name loop
         Append (Result, """" & Trim (T'Image) & """");
         if T /= My_Config.Thermistor_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Stepper_Temperatures"":[");
      for S in My_Config.Stepper_Name loop
         Append (Result, """" & Trim (S'Image) & """");
         if S /= My_Config.Stepper_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Board_Probe_Temperatures"":[");
      for P in Board_Temperature_Probe_Name loop
         Append (Result, """" & Trim (P'Image) & """");
         if P /= Board_Temperature_Probe_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Heater_Powers"":[");
      for H in My_Config.Heater_Name loop
         Append (Result, """" & Trim (H'Image) & """");
         if H /= My_Config.Heater_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Switch_Is_High_State"":[");
      for I in My_Config.Input_Switch_Name loop
         Append (Result, """" & Trim (I'Image) & """");
         if I /= My_Config.Input_Switch_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "],");

      Append (Result, """Tachometer_Frequencies"":[");
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
      Result : Unbounded_String := To_Unbounded_String ("{""Status"":{");
      Pos    : constant Position := Get_Position;

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
              To_Unbounded_String ("{""Fatal_Error"":""")
              & JSON_Escape (Ada.Exceptions.Exception_Information (Occurrence))
              & """}";
         end;
      end if;

      Append (Result, """Time"":" & Clock'Image & ",");

      Append (Result, """Position"":{");
      for A in Axis_Name loop
         Append (Result, """" & Trim (A'Image) & """:" & Pos (A)'Image);
         if A /= Axis_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Thermistor_Temperatures"":{");
      for T in My_Config.Thermistor_Name loop
         Append (Result, """" & Trim (T'Image) & """:" & Get_Thermistor_Temperature (T)'Image);
         if T /= My_Config.Thermistor_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Stepper_Temperatures"":{");
      for S in My_Config.Stepper_Name loop
         Append (Result, """" & Trim (S'Image) & """:" & Get_Stepper_Temperature (S)'Image);
         if S /= My_Config.Stepper_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Board_Probe_Temperatures"":{");
      for P in Board_Temperature_Probe_Name loop
         Append (Result, """" & Trim (P'Image) & """:" & Get_Board_Temperature (P)'Image);
         if P /= Board_Temperature_Probe_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Heater_Powers"":{");
      for H in My_Config.Heater_Name loop
         Append (Result, """" & Trim (H'Image) & """:" & Get_Heater_Power (H)'Image);
         if H /= My_Config.Heater_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Switch_Is_High_State"":{");
      for I in My_Config.Input_Switch_Name loop
         Append
           (Result,
            """" & Trim (I'Image) & """:" & (if Get_Input_Switch_State (I) = High_State then "true" else "false"));
         if I /= My_Config.Input_Switch_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Tachometer_Frequencies"":{");
      for F in My_Config.Fan_Name loop
         Append (Result, """" & Trim (F'Image) & """:" & Get_Tachometer_Frequency (F)'Image);
         if F /= My_Config.Fan_Name'Last then
            Append (Result, ",");
         end if;
      end loop;
      Append (Result, "},");

      Append (Result, """Stepgen_Is_Paused"":" & (if Is_Stepgen_Paused then "true," else "false,"));

      Append (Result, """Current_File_Name"":""" & JSON_Escape (Get_File_Name) & """,");

      Append (Result, """Current_File_Line"":" & Get_Line'Image & ",");

      if Startup_Manager.Get_Startup_Done then
         Append (Result, """Startup"":""Done""");
      elsif Startup_Manager.Get_Update_Allowed then
         Append (Result, """Startup"":""Update running""");
      elsif Startup_Manager.Get_Update_Required then
         Append (Result, """Startup"":""Update required""");
      else
         Append (Result, """Startup"":""Waiting""");
      end if;

      Append (Result, "}}");

      return Result;
   end Build_Status_Values;

   overriding
   procedure WebSocket_Finalize (Client : in out Prunt_Client) is
   begin
      Server.Remove_WebSocket_Receiver (Client);
   end WebSocket_Finalize;

   overriding
   procedure WebSocket_Initialize (Client : in out Prunt_Client) is
   begin
      GNAT.Sockets.Set_Socket_Option (Client.Get_Socket, Socket_Level, (Send_Timeout, 0.000_001));
      if GNAT.Sockets.Get_Socket_Option (Client.Get_Socket, Socket_Level, Send_Buffer).Size < 20_000 then
         GNAT.Sockets.Set_Socket_Option (Client.Get_Socket, Socket_Level, (Send_Buffer, 20_000));
      end if;
      --  Sending a WebSocket message from a different task bypasses the regular handling of unresponsive clients,
      --  therefore we need to set our own timeout here. The timeout is set to an extremely low value here as the
      --  timeout counter only starts when the syscall is blocked.
      --
      --  TODO: Move TMC register dump to a HTTP request so in will not generate a large log message which overruns
      --  the buffer.

      Server.Register_WebSocket_Receiver (Client);
   end WebSocket_Initialize;

   overriding
   function WebSocket_Open (Client : access Prunt_Client) return WebSocket_Accept is
      Status : Status_Line renames Get_Status_Line (Client.all);
   begin
      if Status.File = "websocket/everything" then
         return (Accepted => True, Length => 0, Size => 5_000, Duplex => True, Chunked => False, Protocols => "");
      else
         return (Accepted => False, Length => 9, Code => 404, Reason => "Not Found");
      end if;
   end WebSocket_Open;

   overriding
   procedure WebSocket_Received (Client : in out Prunt_Client; Message : String) is
   begin
      null;
   end WebSocket_Received;

   overriding
   procedure Finalize (Client : in out Prunt_Client) is
   begin
      if Is_Open (Client.Content.File) then
         Close (Client.Content.File);
      end if;

      Client.Content.Big_String_Content.Content := Null_Unbounded_String;
      Client.Content.Post_Content.Content := Post_Bodies.Null_Bounded_String;
      End_Search (Client.Content.Uploads_Directory_Content.Search);

      Finalize (HTTP_Client (Client));
   end Finalize;

   function "<" (Left, Right : Prunt_Client_Access) return Boolean is
      use System;
   begin
      if Left = Right then
         return False;
      elsif Left = null then
         return True;
      elsif Right = null then
         return False;
      else
         return Left.all'Address < Right.all'Address;
      end if;
   end "<";

   procedure Task_Termination_Set_Specific_Handler (Handler : Ada.Task_Termination.Termination_Handler) is
   begin
      Ada.Task_Termination.Set_Specific_Handler (Server'Identity, Handler);
   end Task_Termination_Set_Specific_Handler;

   procedure Reset is
   begin
      Startup_Manager.Clear_Startup_Done;
      Server.Reset_Server_Start_Time;
   end Reset;

   task body Server is
      Factory :
        aliased Prunt_HTTP_Factory
                  (Request_Length => Buffer_Size,
                   Input_Size => Buffer_Size,
                   Output_Size => Buffer_Size,
                   Max_Connections => 300);

      Next_Status_Send : Ada.Real_Time.Time := Clock;

      package WebSocket_Receiver_Sets is new Ada.Containers.Ordered_Sets (Prunt_Client_Access);
      WebSocket_Receivers : WebSocket_Receiver_Sets.Set;
      Sockets_Server      : GNAT.Sockets.Server.Connections_Server (Factory'Access, Port);

      procedure Send_To_All_WebSocket_Receivers (Message : String) is
      begin
         for C of WebSocket_Receivers loop
            begin
               WebSocket_Send (C.all, Message);
            exception
               when E : others =>
                  Trace_Error (Factory, "Send_To_All_WebSocket_Receivers", E);
                  C.Shutdown;
                  --  Force the socket to close after the send syscall blocks due to buffer overrun.
                  --
                  --  TODO: Is there a better way to do this?
            end;
         end loop;
      end Send_To_All_WebSocket_Receivers;

      Log_Handle : My_Logger.Handle;

      procedure Logger_Receiver (Message : String) is
      begin
         Server.Log_To_WebSocket_Receivers (Message);
      end Logger_Receiver;

      Server_Start_Time : Ada.Real_Time.Time := Clock;
      --  Used by client to force a reload if the server has been restarted.

      New_Client : Prunt_Client_Access;
   begin
      --  Trace_On (Factory, Received => GNAT.Sockets.Server.Trace_Decoded, Sent => GNAT.Sockets.Server.Trace_Decoded);
      --  Trace_On (Factory, Received => GNAT.Sockets.Server.Trace_None, Sent => GNAT.Sockets.Server.Trace_None);
      --  TODO: Make the above only log errors instead of producing lots of noise.

      begin
         if not Exists ("uploads") then
            Create_Directory ("uploads");
         elsif Kind ("uploads") /= Directory then
            raise Constraint_Error
              with """uploads"" is not a directory. Delete or rename the file named uploads and restart Prunt.";
         end if;
      exception
         when E : others =>
            Fatal_Exception_Occurrence_Holder.Set (Ada.Task_Termination.Unhandled_Exception, Server'Identity, E);
      end;

      Log_Handle.Set_Receiver (Logger_Receiver'Unrestricted_Access);

      loop
         select
            accept Register_WebSocket_Receiver (Client : in out Prunt_Client) do
               if Client.Content.Self_Access /= Client'Unrestricted_Access then
                  raise Constraint_Error
                    with "Client record was copied at some point. Unrestricted_Access may be unsafe.";
               end if;
               --  It seems like this never occurs, but it's better to have it in case the library changes. I
               --  would prefer to avoid Unrestricted_Access completely, but that is not possible with how the
               --  library is designed.

               New_Client := Client'Unrestricted_Access;
               WebSocket_Receivers.Insert (New_Client);
            end Register_WebSocket_Receiver;

            WebSocket_Send (New_Client.all, "{""Server_Start_Time"":""" & Server_Start_Time'Image & """}");
         or
            accept Remove_WebSocket_Receiver (Client : in out Prunt_Client) do
               if Client.Content.Self_Access /= Client'Unrestricted_Access then
                  raise Constraint_Error
                    with "Client record was copied at some point. Unrestricted_Access may be unsafe.";
               end if;
               --  It seems like this never occurs, but it's better to have it in case the library changes. I
               --  would prefer to avoid Unrestricted_Access completely, but that is not possible with how the
               --  library is designed.

               WebSocket_Receivers.Delete (Client'Unrestricted_Access);
            end Remove_WebSocket_Receiver;
         or
            accept Log_To_WebSocket_Receivers (Message : String) do
               Send_To_All_WebSocket_Receivers ("{""Log"":""" & JSON_Escape (Message) & """}");
            end Log_To_WebSocket_Receivers;
         or
            accept Reset_Server_Start_Time;
            Server_Start_Time := Clock;
            Send_To_All_WebSocket_Receivers ("{""Server_Start_Time"":""" & Server_Start_Time'Image & """}");
         or
            delay until Next_Status_Send;

            Send_To_All_WebSocket_Receivers (To_String (Build_Status_Values));

            Next_Status_Send := Next_Status_Send + Seconds (1);
            --  TODO: Change the interval to 125ms and allow the client to specify a divisor to skip some updates.
            if Clock > Next_Status_Send then
               Next_Status_Send := Clock;
            end if;
            --  Try to keep to a 1 second interval, but if we can not keep up then avoid building up a backlog.
         end select;
      end loop;
   end Server;

end Prunt.Web_Server;
