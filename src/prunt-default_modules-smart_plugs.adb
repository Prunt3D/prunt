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

with Prunt.Default_Modules.Smart_Plugs.Config_Paths;

package body Prunt.Default_Modules.Smart_Plugs is

   pragma Extensions_Allowed (On);

   package My_Config_Paths is new Config_Paths;

   function Build_Schema return Config.Config_Property_Maps.Map is separate;
   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;
   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class is
      pragma Unreferenced (This);
   begin
      return
        Config.Versioned_Config_Schema'
          (Version => 1, Module_Instance_Tag => Module_Instance'Tag, Top_Level_Items => Build_Schema);
   end Config_Schema;

   function Read_Configuration (Data : Config.Config_Data) return Prunt.Smart_Plugs.Plug_Config is
      Parsed : constant User_Config := Config_Data_To_User_Config (Data);
   begin
      if Parsed.Smart_Plug.Kind = Disabled then
         return (others => <>);
      end if;
      return
        (Enabled          => True,
         Host             => Parsed.Smart_Plug.Host,
         Provider         => Parsed.Smart_Plug.Provider,
         Watchdog_Seconds => Prunt.Smart_Plugs.Watchdog_Timeout (Parsed.Smart_Plug.Watchdog_Seconds));
   end Read_Configuration;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Status_Emitter, Get_Other_Instance);
      Parsed : constant Prunt.Smart_Plugs.Plug_Config := Read_Configuration (Config_Data);
   begin
      if Parsed.Enabled and then not Prunt.Smart_Plugs.Valid_Host (Parsed.Host) then
         Report_Config_Error
           (My_Config_Paths.Root.Smart_Plug.Host, "Enter a valid smart plug hostname or IPv4 address.");
      end if;
      return Result : Module_Instance;
   end Initialize;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier)
   is
      pragma Unreferenced (This, Self_Ref, Args, Planner, Command_Identifier);
   begin
      raise Constraint_Error;
   end Gcode_Dispatch;

   protected body Module_Instance is
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Self_Ref_In, Planner);
      begin
         null;
      end Start;
   end Module_Instance;

end Prunt.Default_Modules.Smart_Plugs;
