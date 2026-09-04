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

with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.Sockets;          use GNAT.Sockets;
with Interfaces;
with Prunt_Simulator_Samples;

package body Prunt_Simulator_Telemetry_Server is

   Environment_Name             : constant String := "PRUNT_SIM_TELEMETRY_PORT";
   Maximum_Request_Bytes         : constant Positive := 4_096;
   Request_Chunk_Bytes           : constant Positive := 1_024;
   Response_Chunk_Bytes          : constant Positive := 16_384;
   Client_Timeout                : constant Timeval_Duration := 1.0;
   Listener_Poll_Interval        : constant Selector_Duration := 0.1;
   Execution_Path_Prefix         : constant String := "/execution_samples_after_";
   JSON_Suffix                   : constant String := ".json";
   Position_Samples_Path         : constant String := "/position_samples.json";

   Listener : Socket_Type := No_Socket;
   Enabled  : Boolean := False;
   Started  : Boolean := False;

   task Telemetry_Task is
      entry Activate;
      entry Stop;
   end Telemetry_Task;

   function Has_Complete_Header (Request : String; Length : Natural) return Boolean;
   procedure Handle_Client (Client : Socket_Type);
   function Is_Signed_Decimal (Value : String) return Boolean;
   procedure Send_All (Client : Socket_Type; Data : String);
   procedure Send_Response
     (Client        : Socket_Type;
      Status_Code   : Positive;
      Reason        : String;
      Content_Type  : String;
      Payload       : String;
      Extra_Headers : String := "");

   function Has_Complete_Header (Request : String; Length : Natural) return Boolean is
   begin
      if Length < 4 then
         return False;
      end if;

      for I in 1 .. Length - 3 loop
         if Request (I .. I + 3) = ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF then
            return True;
         end if;
      end loop;
      return False;
   end Has_Complete_Header;

   function Is_Signed_Decimal (Value : String) return Boolean is
      First_Digit : Positive := Value'First;
   begin
      if Value'Length = 0 then
         return False;
      elsif Value (Value'First) = '-' then
         if Value'Length = 1 then
            return False;
         end if;
         First_Digit := @ + 1;
      end if;

      for C of Value (First_Digit .. Value'Last) loop
         if C not in '0' .. '9' then
            return False;
         end if;
      end loop;
      return True;
   end Is_Signed_Decimal;

   procedure Send_All (Client : Socket_Type; Data : String) is
      Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (Response_Chunk_Bytes));
      Last   : Stream_Element_Offset;
      Offset : Natural := 0;
      Count  : Natural;
   begin
      while Offset < Data'Length loop
         Count := Natural'Min (Response_Chunk_Bytes, Data'Length - Offset);
         for I in 1 .. Count loop
            Buffer (Stream_Element_Offset (I)) :=
              Stream_Element (Character'Pos (Data (Data'First + Offset + I - 1)));
         end loop;

         Send_Socket (Client, Buffer (1 .. Stream_Element_Offset (Count)), Last);
         if Last < Buffer'First then
            raise Socket_Error with "telemetry client closed while a response was being sent";
         end if;
         Offset := @ + Natural (Last - Buffer'First + 1);
      end loop;
   end Send_All;

   procedure Send_Response
     (Client        : Socket_Type;
      Status_Code   : Positive;
      Reason        : String;
      Content_Type  : String;
      Payload       : String;
      Extra_Headers : String := "")
   is
      Header : constant String :=
        "HTTP/1.1"
        & Positive'Image (Status_Code)
        & " "
        & Reason
        & ASCII.CR
        & ASCII.LF
        & "Content-Type: "
        & Content_Type
        & ASCII.CR
        & ASCII.LF
        & "Content-Length: "
        & Trim (Natural'Image (Payload'Length), Both)
        & ASCII.CR
        & ASCII.LF
        & "Connection: close"
        & ASCII.CR
        & ASCII.LF
        & "Cache-Control: no-store"
        & ASCII.CR
        & ASCII.LF
        & Extra_Headers
        & ASCII.CR
        & ASCII.LF;
   begin
      Send_All (Client, Header);
      Send_All (Client, Payload);
   end Send_Response;

   procedure Handle_Client (Client : Socket_Type) is
      Request        : String (1 .. Maximum_Request_Bytes);
      Request_Length : Natural := 0;
      Buffer         : Stream_Element_Array (1 .. Stream_Element_Offset (Request_Chunk_Bytes));
      Last           : Stream_Element_Offset;
      Line_End       : Natural := 0;
   begin
      Set_Socket_Option (Client, Socket_Level, (Name => Receive_Timeout, Timeout => Client_Timeout));
      Set_Socket_Option (Client, Socket_Level, (Name => Send_Timeout, Timeout => Client_Timeout));

      loop
         Receive_Socket (Client, Buffer, Last);
         exit when Last < Buffer'First;

         for I in Buffer'First .. Last loop
            if Request_Length = Maximum_Request_Bytes then
               Send_Response
                 (Client,
                  431,
                  "Request Header Fields Too Large",
                  "text/plain; charset=utf-8",
                  "Request headers exceed the simulator telemetry limit." & ASCII.LF);
               return;
            end if;
            Request_Length := @ + 1;
            Request (Request_Length) := Character'Val (Buffer (I));
         end loop;
         exit when Has_Complete_Header (Request, Request_Length);
      end loop;

      if not Has_Complete_Header (Request, Request_Length) then
         Send_Response
           (Client, 400, "Bad Request", "text/plain; charset=utf-8", "Incomplete HTTP request." & ASCII.LF);
         return;
      end if;

      for I in 1 .. Request_Length - 1 loop
         if Request (I .. I + 1) = ASCII.CR & ASCII.LF then
            Line_End := I - 1;
            exit;
         end if;
      end loop;
      if Line_End = 0 then
         Send_Response (Client, 400, "Bad Request", "text/plain; charset=utf-8", "Malformed request line." & ASCII.LF);
         return;
      end if;

      declare
         Request_Line : constant String := Request (1 .. Line_End);
         First_Space  : constant Natural := Index (Request_Line, " ");
         Second_Space : constant Natural :=
           (if First_Space = 0 or else First_Space = Request_Line'Last
            then 0
            else Index (Request_Line (First_Space + 1 .. Request_Line'Last), " "));
      begin
         if First_Space = 0 or else Second_Space = 0 or else Second_Space = Request_Line'Last then
            Send_Response
              (Client, 400, "Bad Request", "text/plain; charset=utf-8", "Malformed request line." & ASCII.LF);
            return;
         end if;

         declare
            Method  : constant String := Request_Line (Request_Line'First .. First_Space - 1);
            Target  : constant String := Request_Line (First_Space + 1 .. Second_Space - 1);
            Version : constant String := Request_Line (Second_Space + 1 .. Request_Line'Last);
         begin
            if Method /= "GET" then
               Send_Response
                 (Client,
                  405,
                  "Method Not Allowed",
                  "text/plain; charset=utf-8",
                  "Only GET is supported." & ASCII.LF,
                  "Allow: GET" & ASCII.CR & ASCII.LF);
               return;
            elsif Version /= "HTTP/1.0" and then Version /= "HTTP/1.1" then
               Send_Response
                 (Client, 400, "Bad Request", "text/plain; charset=utf-8", "Unsupported HTTP version." & ASCII.LF);
               return;
            end if;

            if Target = Position_Samples_Path then
               declare
                  Payload : constant String := Prunt_Simulator_Samples.JSON_String_Content;
               begin
                  Send_Response (Client, 200, "OK", "application/json; charset=utf-8", Payload);
               end;
            elsif Target'Length > Execution_Path_Prefix'Length + JSON_Suffix'Length
              and then Target (Target'First .. Target'First + Execution_Path_Prefix'Length - 1)
                = Execution_Path_Prefix
              and then Target (Target'Last - JSON_Suffix'Length + 1 .. Target'Last) = JSON_Suffix
            then
               declare
                  Sequence_Image : constant String :=
                    Target (Target'First + Execution_Path_Prefix'Length .. Target'Last - JSON_Suffix'Length);
               begin
                  if not Is_Signed_Decimal (Sequence_Image) then
                     Send_Response
                       (Client,
                        404,
                        "Not Found",
                        "text/plain; charset=utf-8",
                        "Telemetry endpoint not found." & ASCII.LF);
                     return;
                  end if;

                  declare
                     Payload : constant String :=
                       Prunt_Simulator_Samples.Execution_JSON_Content (Interfaces.Integer_64'Value (Sequence_Image));
                  begin
                     Send_Response (Client, 200, "OK", "application/json; charset=utf-8", Payload);
                  end;
               exception
                  when Constraint_Error =>
                     Send_Response
                       (Client,
                        404,
                        "Not Found",
                        "text/plain; charset=utf-8",
                        "Telemetry endpoint not found." & ASCII.LF);
               end;
            else
               Send_Response
                 (Client,
                  404,
                  "Not Found",
                  "text/plain; charset=utf-8",
                  "Telemetry endpoint not found." & ASCII.LF);
            end if;
         end;
      end;
   end Handle_Client;

   task body Telemetry_Task is
      Client       : Socket_Type := No_Socket;
      Peer_Address : Sock_Addr_Type;
      Status       : Selector_Status;
      pragma Unreferenced (Peer_Address);
   begin
      accept Activate;
      if Listener /= No_Socket then
         loop
            select
               accept Stop;
               exit;
            else
               Accept_Socket
                 (Server  => Listener,
                  Socket  => Client,
                  Address => Peer_Address,
                  Timeout => Listener_Poll_Interval,
                  Status  => Status);
               if Status = Completed then
                  begin
                     Handle_Client (Client);
                  exception
                     when Socket_Error =>
                        null;
                     when E : others =>
                        Ada.Text_IO.Put_Line
                          (Ada.Text_IO.Standard_Error,
                           "Simulator telemetry request failed: " & Ada.Exceptions.Exception_Information (E));
                  end;
                  Close_Socket (Client);
                  Client := No_Socket;
               end if;
            end select;
         end loop;
      end if;

      if Client /= No_Socket then
         Close_Socket (Client);
      end if;
      if Listener /= No_Socket then
         Close_Socket (Listener);
         Listener := No_Socket;
      end if;
   exception
      when E : others =>
         if Client /= No_Socket then
            Close_Socket (Client);
         end if;
         if Listener /= No_Socket then
            Close_Socket (Listener);
            Listener := No_Socket;
         end if;
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Simulator telemetry server stopped: " & Ada.Exceptions.Exception_Information (E));
   end Telemetry_Task;

   procedure Start is
   begin
      if Started then
         return;
      end if;

      if not Ada.Environment_Variables.Exists (Environment_Name) then
         Started := True;
         Telemetry_Task.Activate;
         return;
      end if;

      declare
         Port_Image : constant String := Trim (Ada.Environment_Variables.Value (Environment_Name), Both);
         Port       : constant Port_Type := Port_Type'Value (Port_Image);
      begin
         if Port = 0 then
            raise Constraint_Error with Environment_Name & " must be in the range 1 .. 65535.";
         end if;

         Create_Socket (Listener, Family_Inet, Socket_Stream);
         Set_Socket_Option (Listener, Socket_Level, (Name => Reuse_Address, Enabled => True));
         Bind_Socket (Listener, (Family => Family_Inet, Addr => Inet_Addr ("127.0.0.1"), Port => Port));
         Listen_Socket (Listener, 4);
         Enabled := True;
         Started := True;
         Telemetry_Task.Activate;
      end;
   exception
      when others =>
         if Listener /= No_Socket then
            Close_Socket (Listener);
            Listener := No_Socket;
         end if;
         Started := True;
         Telemetry_Task.Activate;
         raise;
   end Start;

   procedure Stop is
   begin
      if Enabled then
         begin
            Telemetry_Task.Stop;
         exception
            when Tasking_Error =>
               null;
         end;
         Enabled := False;
      end if;
   end Stop;

end Prunt_Simulator_Telemetry_Server;
