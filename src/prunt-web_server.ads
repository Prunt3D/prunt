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

with Prunt.Logger;
with Prunt.Config;
with Prunt.Update_Checker;
with Ada.Strings.Bounded;
with Ada.Containers.Ordered_Sets;
with Ada.Task_Termination;
with Ada.Directories;
with Prunt.Web_Server_Resources;
with Ada.Exceptions;                                    use Ada.Exceptions;
with Ada.Streams;                                       use Ada.Streams;
with Ada.Streams.Stream_IO;                             use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;                             use Ada.Strings.Unbounded;
with GNAT.Sockets;                                      use GNAT.Sockets;
with GNAT.Sockets.Server;                               use GNAT.Sockets.Server;
with GNAT.Sockets.Connection_State_Machine.HTTP_Server; use GNAT.Sockets.Connection_State_Machine.HTTP_Server;

generic
   with package My_Logger is new Prunt.Logger (<>);
   with package My_Config is new Prunt.Config (<>);
   with package My_Update_Checker is new Prunt.Update_Checker (<>);
   with function Get_Position return Prunt.Position;
   with function Get_Thermistor_Temperature (Thermistor : My_Config.Thermistor_Name) return Prunt.Temperature;
   with function Get_Stepper_Temperature (Thermistor : My_Config.Stepper_Name) return Prunt.Temperature;
   type Board_Temperature_Probe_Name is (<>);
   with function Get_Board_Temperature (Thermistor : Board_Temperature_Probe_Name) return Prunt.Temperature;
   with function Get_Heater_Power (Heater : My_Config.Heater_Name) return Prunt.PWM_Scale;
   with function Get_Input_Switch_State (Switch : My_Config.Input_Switch_Name) return Prunt.Pin_State;
   with function Get_Tachometer_Frequency (Fan : My_Config.Fan_Name) return Frequency;
   with function Get_File_Name return String;
   with function Get_Line return File_Line_Count;
   with procedure Submit_Gcode_Command (Command : String; Succeeded : out Boolean);
   with procedure Submit_Gcode_File (Path : String; Succeeded : out Boolean);
   with function Is_Stepgen_Paused return Boolean;
   with procedure Pause_Stepgen;
   with procedure Resume_Stepgen;
   with procedure Reload_Server;
   Fatal_Exception_Occurrence_Holder : in out Fatal_Exception_Occurrence_Holder_Type;
   Port : GNAT.Sockets.Port_Type;
package Prunt.Web_Server is

   procedure Wait_For_User_To_Allow_Update;
   procedure Notify_Startup_Done;
   procedure Task_Termination_Set_Specific_Handler (Handler : Ada.Task_Termination.Termination_Handler);
   procedure Reset;

private

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

   type Unbounded_String_Source is new Content_Source with record
      Content    : Unbounded_String;
      Next_Start : Positive := 1;
      --  Using the Slice function to replace the Unbounded_String would be a bit cleaner here, but the GCC Slice
      --  implementation copies the entire string in to a new allocation, so we do this instead to avoid some copies.
   end record;

   overriding
   function Get (Source : access Unbounded_String_Source) return String;

   type Array_Stream_Type is new Root_Stream_Type with record
      Content  : Web_Server_Resources.Content_Access;
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
      --  Unbounded_String GET requests:
      Big_String_Content        : aliased Unbounded_String_Source;
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
      Content : Extra_Client_Content;
   end record;

   task Server is
      entry Register_WebSocket_Receiver (Client : in out Prunt_Client);
      entry Remove_WebSocket_Receiver (Client : in out Prunt_Client);
      entry Log_To_WebSocket_Receivers (Message : String);
      entry Reset_Server_Start_Time;
   end Server;

   overriding
   procedure Reply_HTML
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True);
   --  Identical to overridden procedure aside from sending the Content-Length header when Get = False. This procedure
   --  does whereas the original does not.
   --
   --  TODO: Take an Unbounded_String here to avoid some copies.

   overriding
   procedure Reply_Text
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True);
   --  Identical to overridden procedure aside from sending the Content-Length header when Get = False. This procedure
   --  does whereas the original does not.
   --
   --  TODO: Take an Unbounded_String here to avoid some copies.

   procedure Reply_JSON
     (Client : in out Prunt_Client; Code : Positive; Reason : String; Message : String; Get : Boolean := True);

   procedure Reply_JSON
     (Client  : in out Prunt_Client;
      Code    : Positive;
      Reason  : String;
      Message : Unbounded_String;
      Get     : Boolean := True);

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

   function Build_Status_Schema return Unbounded_String;
   function Build_Status_Values return Unbounded_String;

end Prunt.Web_Server;
