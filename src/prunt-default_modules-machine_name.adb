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

package body Prunt.Default_Modules.Machine_Name is

   pragma Extensions_Allowed (On);

   function Build_Schema return Config.Config_Property_Maps.Map is separate;

   function Config_Data_To_User_Config (Data : Config.Config_Data) return User_Config is separate;

   procedure User_Config_To_Config_Data (Data : in out Config.Config_Data; Config : User_Config) is separate;

   overriding
   function Config_Schema (This : Module) return Config.Versioned_Config_Schema'Class is
   begin
      return
        Config.Versioned_Config_Schema'
          (Version => 1, Module_Instance_Tag => Module_Instance'Tag, Top_Level_Items => Build_Schema);
   end Config_Schema;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map is
      pragma Unreferenced (This);
   begin
      return
        ["Identity" =>
           ["Machine name" =>
              (Kind        => Status_Manager.String_Kind,
               Unit        => "",
               Description => "Current machine name.",
               Condition   => "")]];
   end Status_Schema;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   overriding
   procedure Process_After_Block (This : Machine_Name_Update; Context : Block_End_Context'Class) is
      pragma Unreferenced (Context);
   begin
      Module_Instance (This.Module_Instance_Ref.Get.Element.all).Apply_Runtime_Name (This.Name);
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Machine_Name_Report_Event; Context : Block_End_Context'Class) is
   begin
      Context.Log ("Machine name: " & Module_Instance (This.Module_Instance_Ref.Get.Element.all).Get_Current_Name);
   end Process_After_Block;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      pragma Unreferenced (This, Report_Config_Error);

      Parsed_Config                     : constant User_Config := Config_Data_To_User_Config (Config_Data);
      Config_Saving_Module_Instance_Ref : constant My_Modules.Module_Instance_Shared_Pointers.Ref :=
        Get_Other_Instance (Config_Saving_Module.Module_Instance'Tag);
      Config_Saver                      : Config_Saving_Module.Config_Saver'Class renames
        Config_Saving_Module.Config_Saver'Class (Config_Saving_Module_Instance_Ref.Get.Element.all);
   begin
      return Result : Module_Instance do
         Config_Saver.Register_For_Saving (Config_Data);
         Result.Initialize (Parsed_Config, Config_Data, Status_Emitter);
      end return;
   end Initialize;

   protected body Module_Instance is
      procedure Initialize
        (Config_In         : User_Config;
         Config_Data_In    : Prunt.Config.Config_Data;
         Status_Emitter_In : Status_Manager.Status_Emitter) is
      begin
         Config := Config_In;
         Config_Data := Config_Data_In;
         Status_Emitter := Status_Emitter_In;
         Status_Emitter.Set_Value ("Identity", "Machine name", Config.Machine_Name.Name);
      end Initialize;

      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class) is
      begin
         pragma Unreferenced (Planner);
         Self_Ref := Self_Ref_In;
      end Start;

      procedure Apply_Runtime_Name (Value : Virtual_String) is
      begin
         Config.Machine_Name.Name := Value;
         User_Config_To_Config_Data (Config_Data, Config);
         Status_Emitter.Set_Value ("Identity", "Machine name", Config.Machine_Name.Name);
      end Apply_Runtime_Name;

      function Get_Current_Name return Virtual_String is
      begin
         return Config.Machine_Name.Name;
      end Get_Current_Name;
   end Module_Instance;

   procedure Expected_Printer_Check (This : Module_Instance; Planner : Planner_Interface'Class; P : Virtual_String) is
      Current_Name : constant Virtual_String := This.Get_Current_Name;
   begin
      pragma Unreferenced (Planner);
      if P /= Current_Name then
         raise Constraint_Error
           with
             Conversions.To_UTF_8_String
               ("Expected machine name """ & P & """ but current machine name is """ & Current_Name & """.");
      end if;
   end Expected_Printer_Check;

   procedure Set_Machine_Name
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      P        : Virtual_String) is
   begin
      pragma Unreferenced (This);
      Planner.Flush (Machine_Name_Update'(Module_Instance_Ref => Self_Ref, Name => P));
   end Set_Machine_Name;

   procedure Report_Machine_Name
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class) is
   begin
      pragma Unreferenced (This);
      Planner.Flush (Machine_Name_Report_Event'(Module_Instance_Ref => Self_Ref));
   end Report_Machine_Name;

end Prunt.Default_Modules.Machine_Name;
