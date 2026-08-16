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

package body Prunt.Default_Modules.Config_Saving is

   pragma Extensions_Allowed (On);

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector is separate;

   overriding
   procedure Gcode_Dispatch
     (This               : Module_Instance;
      Self_Ref           : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier) is separate;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Path'Class; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class is
   begin
      return Result : Module_Instance;
   end Initialize;

   overriding
   procedure Process_After_Block (This : Config_Save_Event; Context : Block_End_Context'Class) is
      Instance : Module_Instance renames Module_Instance (This.Module_Instance_Ref.Get.Element.all);
   begin
      Context.Wait_For_Idle;
      Context.Catch_Up_Planner_State;
      Context.Prepare_Config_For_Save;

      case This.Save_All is
         when False =>
            Instance.Process_Save_Settings (This.Config_To_Save);

         when True  =>
            Instance.Process_Save_All_Settings;
      end case;
   end Process_After_Block;

   overriding
   procedure Process_After_Block (This : Config_List_Event; Context : Block_End_Context'Class) is
   begin
      Context.Log (Module_Instance (This.Module_Instance_Ref.Get.Element.all).Config_List);
   end Process_After_Block;

   protected body Module_Instance is
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class)
      is
         pragma Unreferenced (Self_Ref_In, Planner);
      begin
         Started := True;
      end Start;

      procedure Register_For_Saving (Config_Data : Config.Config_Data) is
      begin
         if Started then
            raise Constraint_Error with "Configs must be registered before Start is called.";
         end if;

         Configs_To_Save.Insert (Config_Data.Module_Name, Config_Data);
      end Register_For_Saving;

      procedure Process_Save_All_Settings is
      begin
         for C of Configs_To_Save loop
            C.Save;
         end loop;
      end Process_Save_All_Settings;

      procedure Process_Save_Settings (I : Virtual_String) is
         Config_To_Save : constant Config_Data_Maps.Reference_Type := Configs_To_Save.Reference (I);
      begin
         Config.Save (Config_To_Save.Element.all);
      end Process_Save_Settings;

      function Contains_Config_To_Save (I : Virtual_String) return Boolean is
      begin
         return Configs_To_Save.Contains (I);
      end Contains_Config_To_Save;

      function Config_List return Virtual_String is
         Module_List : Virtual_String := "Modules with savable settings: ";
      begin
         for C in Configs_To_Save.Iterate loop
            Module_List := @ & C.Key & (if C.Key = Configs_To_Save.Last_Key then +"" else +", ");
         end loop;

         return Module_List;
      end Config_List;
   end Module_Instance;

   procedure Save_Settings
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref; Planner : Planner_Interface'Class) is
   begin
      Planner.Flush (Config_Save_Event'(Save_All => True, Module_Instance_Ref => Self_Ref));
   end Save_Settings;

   procedure Save_Settings
     (This     : Module_Instance;
      Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;
      Planner  : Planner_Interface'Class;
      I        : Virtual_String) is
   begin
      if not This.Contains_Config_To_Save (I) then
         raise Gcode_Bad_Inputs_Error
           with "Module """ & Conversions.To_UTF_8_String (I) & """ not known or does not have savable settings.";
      end if;

      Planner.Flush (Config_Save_Event'(Save_All => False, Module_Instance_Ref => Self_Ref, Config_To_Save => I));
   end Save_Settings;

   procedure List_Savable_Settings
     (Self_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref; Planner : Planner_Interface'Class; I : Gcode_No_Value)
   is
      pragma Unreferenced (I);
   begin
      Planner.Flush (Config_List_Event'(Module_Instance_Ref => Self_Ref));
   end List_Savable_Settings;

end Prunt.Default_Modules.Config_Saving;
