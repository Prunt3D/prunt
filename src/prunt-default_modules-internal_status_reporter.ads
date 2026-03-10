-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2026 Liam Powell (liam@prunt3d.com)            --
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

pragma Extensions_Allowed (On);

with Ada.Tags;
with Prunt.Config;
with Prunt.Gcode_Arguments;
with Prunt.Module_Types; use Prunt.Module_Types;
with Prunt.Status_Manager;

private with Ada.Finalization;
private with Prunt.Limited_Shared_Pointers;

generic
   with function Get_Position return Prunt.Position;
   with function Get_File_Name return Virtual_String;
   with function Get_Line return File_Line_Count;
   with function Stepgen_Paused return Boolean;
package Prunt.Default_Modules.Internal_Status_Reporter is

   type Module is new My_Modules.Module with null record;

   type Module_Instance (<>) is synchronized new My_Modules.Module_Instance with private;

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map;

private

   task type Status_Updater is
      entry Start (Status_Emitter : My_Modules.Status_Emitter_Shared_Pointers.Ref);
      entry Stop;
   end Status_Updater;

   type Status_Updater_Wrapper is new Ada.Finalization.Limited_Controlled with record
      Updater : Status_Updater;
   end record;

   overriding
   procedure Finalize (Object : in out Status_Updater_Wrapper);

   package Status_Updater_Wrapper_Pointers is new Limited_Shared_Pointers (Status_Updater_Wrapper);
   --  TODO: GCC bug in comment below?
   --
   --  The task sometimes does not start if we do not wrap it in a shared pointer. I have no idea why. I do not just
   --  mean that it does not start during the BIP (which it should not do), I mean that it does not start after the
   --  assignment is complete.

   protected type Module_Instance is new My_Modules.Module_Instance with
      procedure Initialize (Status_Emitter_In : My_Modules.Status_Emitter_Shared_Pointers.Ref);

      overriding
      procedure Start;

      overriding
      procedure Gcode_Dispatch
        (Args               : in out Gcode_Arguments.Arguments;
         Planner            : Planner_Interface'Class;
         Command_Identifier : Gcode_Command_Identifier);
   private
      Status_Emitter : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Updater        : Status_Updater_Wrapper_Pointers.Ref;
   end Module_Instance;

end Prunt.Default_Modules.Internal_Status_Reporter;
