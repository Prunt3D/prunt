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

pragma Extensions_Allowed (On);

with Ada.Tags;
with Prunt.Config;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;

private with Ada.Containers.Ordered_Maps;

generic
package Prunt.Default_Modules.Config_Saving is

   type Module is new My_Modules.Module with null record;

   overriding
   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector;

   type Config_Saver is synchronized interface;

   procedure Register_For_Saving (This : in out Config_Saver; Config_Data : Config.Config_Data) is abstract;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance and Config_Saver with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : Config.Config_Data;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Manager.Status_Emitter;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

private

   function Return_False (Left, Right : Config.Config_Data) return Boolean
   is (False);

   package Config_Data_Maps is new
     Ada.Containers.Ordered_Maps (Virtual_String, Config.Config_Data, "=" => Return_False);

   type Config_Save_Event (Save_All : Boolean) is new Extra_Block_Resetting_Data with record
      Module_Instance_Ref : My_Modules.Module_Instance_Shared_Pointers.Ref;

      case Save_All is
         when False =>
            Config_To_Save : Virtual_String;

         when True =>
            null;
      end case;
   end record;

   overriding
   procedure Process_After_Block
     (This                 : Config_Save_Event;
      First_Accel_Distance : Length;
      Last_Command_Index   : Command_Index;
      Loop_Move_Offset     : Position_Offset);

   type Config_List_Event is new Extra_Block_Resetting_Data with record
      Config_List : Virtual_String;
   end record;

   protected type Module_Instance is new My_Modules.Module_Instance and Config_Saver with
      overriding
      procedure Start
        (Self_Ref_In : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref; Planner : Planner_Interface'Class);

      --  overriding
      --  TODO: GCC bug above: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=124596
      procedure Register_For_Saving (Config_Data : Config.Config_Data);

      procedure Save_Settings (Planner : Planner_Interface'Class)
      with Annotate => (Prunt_Config, Gcode_Command, "M500");
      --  Save all configurable settings for all modules that have been temporarily set as a result of g-code commands.
      --  Settings and g-code commands which use this functionality make a note of this in their own descriptions.
      --
      --  This command differs from Marlin in that the exact settings that are available to be saved may not be the
      --  same.

      procedure Save_Settings
        (Planner : Planner_Interface'Class;
         I       : Virtual_String
         --  The name of the module to save.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M500");
      --  Save all configurable settings for a specific module that have been temporarily set as a result of g-code
      --  commands. Settings and g-code commands which use this functionality make a note of this in their own
      --  descriptions.
      --
      --  This command is not present in Marlin.

      procedure Save_Settings
        (Planner : Planner_Interface'Class;
         I       : Gcode_No_Value
         --  When providing no value a listing of modules with savable settings will be emitted.
         )
      with Annotate => (Prunt_Config, Gcode_Command, "M500");
      --  List modules with savable settings.
      --
      --  This command is not present in Marlin.

      procedure Process_Save_All_Settings;

      procedure Process_Save_Settings (I : Virtual_String);

   private

      Self_Ref        : My_Modules.Module_Instance_Shared_Pointers.Weak_Ref;
      Configs_To_Save : Config_Data_Maps.Map;
   end Module_Instance;

end Prunt.Default_Modules.Config_Saving;
