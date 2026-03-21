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

pragma Extensions_Allowed (On);

with Ada.Directories;
with Ada.Exceptions;                                    use Ada.Exceptions;
with Ada.Real_Time;
with Ada.Streams;                                       use Ada.Streams;
with Ada.Streams.Stream_IO;                             use Ada.Streams.Stream_IO;
with Ada.Strings.Bounded;
with Ada.Task_Termination;
with GNAT.Sockets;                                      use GNAT.Sockets;
with GNAT.Sockets.Connection_State_Machine.HTTP_Server; use GNAT.Sockets.Connection_State_Machine.HTTP_Server;
with GNAT.Sockets.Server;                               use GNAT.Sockets.Server;
with Prunt.Config;
with Prunt.Exception_Occurrence_Holders;
with Prunt.Logger;
with Prunt.Update_Checker;

generic
   with
     procedure Apply_Config_Patch
       (Value : Virtual_String; Result : out Virtual_String; Errors : out Config.Config_Error_Vectors.Vector);
   with package My_Logger is new Prunt.Logger (<>);
   with package My_Update_Checker is new Prunt.Update_Checker (<>);
   with procedure Submit_Gcode_Command (Command : Virtual_String; Succeeded : out Boolean);
   with procedure Submit_Gcode_File (Path : Virtual_String; Succeeded : out Boolean);
   with procedure Pause_Stepgen;
   with procedure Resume_Stepgen;
   with procedure Reload_Server;
   with
     function Get_Extra_HTTP_Content (Name : Virtual_String) return access constant Ada.Streams.Stream_Element_Array;
   Exception_Occurrence_Holder : in out Exception_Occurrence_Holders.Exception_Occurrence_Holder_Type;
   Config_Schema_String : Virtual_String;
   Status_Schema_String : Virtual_String;
   Gcode_JSON_String : Virtual_String;
   with function Get_Status_Values_String return Virtual_String;
   Port : GNAT.Sockets.Port_Type;
package Prunt.Web_Server is

   procedure Wait_For_User_To_Allow_Update;
   procedure Notify_Startup_Done;
   procedure Task_Termination_Set_Specific_Handler (Handler : Ada.Task_Termination.Termination_Handler);
   procedure Reset;

private

   function Trim (S : String) return String;
   function Ends_With (Source, Pattern : String) return Boolean;
   function Starts_With (Source, Pattern : String) return Boolean;

   protected Startup_Manager is
      entry Wait_For_Update_Allowed;
      procedure Set_Update_Required;
      function Get_Update_Required return Boolean;
      procedure Set_Startup_Done;
      procedure Clear_Startup_Done;
      function Get_Startup_Done return Boolean;
      procedure Set_Update_Allowed;
      function Get_Update_Allowed return Boolean;
   private
      Update_Required : Boolean := False;
      Update_Allowed  : Boolean := False;
      Startup_Done    : Boolean := False;
   end Startup_Manager;

   Buffer_Size      : constant := 5_000;
   Post_Buffer_Size : constant := 100_000;

   package Post_Bodies is new Ada.Strings.Bounded.Generic_Bounded_Length (Post_Buffer_Size);

   type Post_Body_Destination is new Content_Destination with record
      Content : Post_Bodies.Bounded_String := Post_Bodies.Null_Bounded_String;
      Failed  : Boolean := False;
   end record;

   overriding
   procedure Commit (Destination : in out Post_Body_Destination);
   overriding
   procedure Put (Destination : in out Post_Body_Destination; Data : String);

   type Virtual_String_Source is new Content_Source with record
      Content    : Virtual_String;
      Next_Start : Positive := Positive'Last;
      --  `Next_Start` needs to be set manually when we use it, but we can't use a raise expression here as we need to
      --  default initialise this when it's not used, so instead we use a large value where it will be obvious that we
      --  forgot to set it.
   end record;

   overriding
   function Get (Source : access Virtual_String_Source) return String;

   type Array_Stream_Type is new Root_Stream_Type with record
      Content  : access constant Ada.Streams.Stream_Element_Array;
      Position : Stream_Element_Offset;
      Done     : Boolean;
   end record;

   overriding
   procedure Read
     (Stream : in out Array_Stream_Type; Item : out Stream_Element_Array; Last : out Stream_Element_Offset);

   overriding
   procedure Write (Stream : in out Array_Stream_Type; Item : Stream_Element_Array);

   type Prunt_HTTP_Factory
     (Request_Length  : Positive;
      Input_Size      : Buffer_Length;
      Output_Size     : Buffer_Length;
      Max_Connections : Positive)
   is new Connections_Factory with null record;

   type Prunt_Client;

   type Prunt_Client_Access is access Prunt_Client;

   function "<" (Left, Right : Prunt_Client_Access) return Boolean;

   type Put_Fail_Reason_Kind is
     (No_Failure_Kind,
      Uploads_Not_Dir_Kind,
      File_Not_Regular_Kind,
      File_Name_Malformed_Kind,
      Wrong_Directory_Kind,
      Wrong_Request_Target_Kind,
      Unhandled_Exception_Kind);

   type Directory_Content_Step is (Starting, First_Entry, Continuing_Entries, Finished);

   type Directory_Content is new Content_Source with record
      Step   : Directory_Content_Step := Starting;
      Search : Ada.Directories.Search_Type;
   end record;

   overriding
   function Get (Source : access Directory_Content) return String;
   overriding
   procedure Finalize (Source : in out Directory_Content);

   type Extra_Client_Content is record
      Self_Access               : Prunt_Client_Access := null;
      --  Embedded file GET requests:
      Array_Stream              : aliased Array_Stream_Type;
      --  Virtual_String GET requests:
      Big_String_Content        : aliased Virtual_String_Source;
      --  POST requests:
      Post_Content              : aliased Post_Body_Destination;
      --  File GET and PUT requests:
      File                      : File_Type;
      --  File PUT requests:
      Put_Fail_Reason           : Put_Fail_Reason_Kind := No_Failure_Kind;
      --  GET /uploads requests:
      Uploads_Directory_Content : aliased Directory_Content;
   end record;

   procedure Write (Stream : access Root_Stream_Type'Class; Item : Extra_Client_Content);
   for Extra_Client_Content'Write use Write;

   type WebSocket_Message_Index_Type is mod 2 ** 32;

   type Prunt_Client
     (Listener       : access Connections_Server'Class;
      Request_Length : Positive;
      Input_Size     : Buffer_Length;
      Output_Size    : Buffer_Length)
   is
     new HTTP_Client
          (Listener => Listener,
           Request_Length => Request_Length,
           Input_Size => Input_Size,
           Output_Size => Output_Size)
   with record
      Request_Start_Time      : Ada.Real_Time.Time;
      Content                 : Extra_Client_Content;
      WebSocket_Speed_Divisor : WebSocket_Message_Index_Type := 20;
   end record;

   task Server is
      entry Register_WebSocket_Receiver (Client : in out Prunt_Client);
      entry Remove_WebSocket_Receiver (Client : in out Prunt_Client);
      entry Log_To_WebSocket_Receivers (Message : Virtual_String);
      entry Reset_Server_Start_Time;
   end Server;

   overriding
   procedure Reply_HTML
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True);
   --  Identical to overridden procedure aside from sending the Content-Length header when Get = False. This procedure
   --  does whereas the original does not.

   overriding
   procedure Reply_Text
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True);
   --  Identical to overridden procedure aside from sending the Content-Length header when Get = False. This procedure
   --  does whereas the original does not.

   procedure Reply_JSON
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True);

   procedure Reply_JSON
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : Virtual_String; Get : Boolean := True);

   overriding
   procedure Body_Received (Client : in out Prunt_Client; Stream : in out Root_Stream_Type'Class);
   overriding
   procedure Body_Sent (Client : in out Prunt_Client; Stream : in out Root_Stream_Type'Class; Get : Boolean);
   overriding
   procedure Body_Error
     (Client : in out Prunt_Client; Content : in out Content_Destination'Class; Error : Exception_Occurrence);
   overriding
   procedure Do_Get (Client : in out Prunt_Client);
   overriding
   procedure Do_Head (Client : in out Prunt_Client);
   overriding
   procedure Do_Post (Client : in out Prunt_Client);
   overriding
   procedure Do_Put (Client : in out Prunt_Client);
   overriding
   procedure Do_Body (Client : in out Prunt_Client);
   overriding
   procedure Initialize (Client : in out Prunt_Client);
   overriding
   procedure Finalize (Client : in out Prunt_Client);
   overriding
   procedure Connected (Client : in out Prunt_Client);
   overriding
   function Create
     (Factory : access Prunt_HTTP_Factory; Listener : access Connections_Server'Class; From : Sock_Addr_Type)
      return Connection_Ptr;
   overriding
   function WebSocket_Open (Client : access Prunt_Client) return WebSocket_Accept;
   overriding
   procedure WebSocket_Received (Client : in out Prunt_Client; Message : String);
   overriding
   procedure WebSocket_Initialize (Client : in out Prunt_Client);
   overriding
   procedure WebSocket_Finalize (Client : in out Prunt_Client);

end Prunt.Web_Server;
