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

with VSS.Strings.Conversions;
use VSS.Strings;

package body Prunt.Default_Modules.Internal_Status_Reporter is

   pragma Extensions_Allowed (On);

   overriding
   function Initialize
     (This                : Module;
      Config_Data         : My_Modules.Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return My_Modules.Module_Instance_Shared_Pointers.Ref)
      return My_Modules.Module_Instance'Class
   is
      function Create_Updater_Task return Status_Updater is
      begin
         return Result : Status_Updater;
      end Create_Updater_Task;
   begin
      return Result : Module_Instance := (My_Modules.Module_Instance with others => <>) do
         Result.Updater.Set (Create_Updater_Task'Access);
         Result.Updater.Get.Prepare (Status_Emitter);
      end return;
   end Initialize;

   overriding
   procedure Start (This : in out Module_Instance) is
   begin
      This.Updater.Get.Start;
   end Start;

   overriding
   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map is
   begin
      return
        ["Position"     =>
           [for A in Axis_Name use Conversions.To_Virtual_String (A'Image) =>
              (Kind        => Status_Manager.Real_Kind,
               Unit        => "mm",
               Description => "Position of axis " & Conversions.To_Virtual_String (A'Image),
               Condition   => "")],
         "Print status" =>
           ["File name"    =>
              (Kind        => Status_Manager.String_Kind,
               Unit        => "",
               Description => "Name of current g-code file.",
               Condition   => ""),
            "Current line" =>
              (Kind        => Status_Manager.Integer_Kind,
               Unit        => "",
               Description => "Current line in g-code file.",
               Condition   => ""),
            "Paused"       =>
              (Kind        => Status_Manager.Boolean_Kind,
               Unit        => "",
               Description => "True if printer is currently paused.",
               Condition   => "")]];
   end Status_Schema;

   task body Status_Updater is
      Status_Ref    : My_Modules.Status_Emitter_Shared_Pointers.Ref;
      Stop_Received : Boolean := False;
   begin
      accept Prepare (Status_Emitter : My_Modules.Status_Emitter_Shared_Pointers.Ref) do
         Status_Ref := Status_Emitter;
      end Prepare;

      select
         accept Stop;
         Stop_Received := True;
      or
         accept Start;
      end select;

      while not Stop_Received loop
         select
            accept Stop;
            Stop_Received := True;
         or
            delay 0.5;
            declare
               Pos : constant Position := Get_Position;
            begin
               for A in Axis_Name loop
                  Status_Ref.Get.Set_Value ("Position", Conversions.To_Virtual_String (A'Image), Pos (A) / mm);
               end loop;
               Status_Ref.Get.Set_Value ("Print status", "File name", Get_File_Name);
               Status_Ref.Get.Set_Value
                 ("Print status", "Current line", Long_Long_Integer (File_Line_Count'(Get_Line)));
               Status_Ref.Get.Set_Value ("Print status", "Paused", Stepgen_Paused);
            end;
         end select;
      end loop;
   end Status_Updater;

   overriding
   procedure Finalize (Object : in out Module_Instance) is
   begin
      Object.Updater.Get.Stop;
   end Finalize;

end Prunt.Default_Modules.Internal_Status_Reporter;
