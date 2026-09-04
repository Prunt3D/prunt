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
with Ada.Streams;
with Prunt; use Prunt;
with Prunt.Controller;
with Prunt_Simulator_Config_Overrides;
with Prunt_Simulator_Extra_Resources;
with Prunt_Simulator_Hardware;
with Prunt_Simulator_Machine;
with Prunt_Simulator_Samples;
with Prunt_Simulator_Types;
with VSS.Strings;
with VSS.Strings.Conversions;

package body Prunt_Simulator_Controller is

   package Generic_Types renames Prunt_Simulator_Types.Generic_Types;

   Config_Path                : constant String := "prunt_simulator_config.json";
   Interpolation_Time         : constant Time := 0.000_05 * s;
   Executed_Position_Capacity : constant Positive := 65_536;

   function Get_Board_Specific_Documentation
     (Key : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   function Get_Extra_HTTP_Content
     (Name : VSS.Strings.Virtual_String) return access constant Ada.Streams.Stream_Element_Array;
   procedure Report_Executor_Error (Occurrence : Ada.Exceptions.Exception_Occurrence; Is_Fatal : Boolean);

   function Get_Board_Specific_Documentation
     (Key : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String is
   begin
      pragma Unreferenced (Key);
      return +"";
   end Get_Board_Specific_Documentation;

   function Get_Extra_HTTP_Content
     (Name : VSS.Strings.Virtual_String) return access constant Ada.Streams.Stream_Element_Array
   is
      Name_String : constant String := VSS.Strings.Conversions.To_UTF_8_String (Name);
   begin
      if Name_String = "position_samples.json" then
         return Prunt_Simulator_Samples.JSON_Content;
      else
         return Prunt_Simulator_Extra_Resources.Get_Content (Name_String);
      end if;
   end Get_Extra_HTTP_Content;

   package Controller is new
     Prunt.Controller
       (Generic_Types                           => Generic_Types,
        Hardware                                => Prunt_Simulator_Hardware.Hardware,
        Interpolation_Time                      => Interpolation_Time,
        Maximum_Loop_Move_Tail_Length           => Prunt_Simulator_Types.Maximum_Loop_Move_Tail_Length,
        Enqueue_Command                         => Prunt_Simulator_Machine.Enqueue_Command,
        Setup_For_Loop_Move                     => Prunt_Simulator_Machine.Setup_For_Loop_Move,
        Reset_Position                          => Prunt_Simulator_Machine.Reset_Position,
        Wait_Until_Idle                         => Prunt_Simulator_Machine.Wait_Until_Idle,
        Reset_Hardware                          => Prunt_Simulator_Machine.Reset_Hardware,
        Config_Path                             => Config_Path,
        Config_Overrides                        => Prunt_Simulator_Config_Overrides.Overrides,
        Get_Extra_HTTP_Content                  => Get_Extra_HTTP_Content,
        Get_Board_Specific_Documentation        => Get_Board_Specific_Documentation,
        Executed_Command_Position_Ring_Capacity => Executed_Position_Capacity);

   procedure Report_Executor_Error (Occurrence : Ada.Exceptions.Exception_Occurrence; Is_Fatal : Boolean) is
   begin
      Controller.Report_External_Error (Occurrence, Is_Fatal);
   end Report_Executor_Error;

   procedure Run is
   begin
      Controller.Run;
   end Run;

begin
   Prunt_Simulator_Machine.Set_Reporters
     (Last_Command  => Controller.Report_Last_Command_Executed'Access,
      Error         => Report_Executor_Error'Access);
end Prunt_Simulator_Controller;
