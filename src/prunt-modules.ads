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

with Ada.Finalization;
with Ada.Tags;
with GNATCOLL.Refcount;
with Prunt.Config;
with Prunt.Gcode_Arguments;
with Prunt.Limited_Shared_Pointers;
with Prunt.Module_Types; use Prunt.Module_Types;
with Prunt.Status_Manager;

generic
   --  This package is generic to allow it to have the same accessibility level as generic modules. This is required as
   --  it contains shared pointers to class-wide `Module_Instance` types.
package Prunt.Modules is

   type Module is abstract tagged private;

   function Config_Schema (This : Module) return Config.Versioned_Config_Schema
   is (Version => 1, Top_Level_Items => []);

   function Gcode_Commands (This : Module) return Gcode_Command_Vectors.Vector
   is ([]);

   function Status_Schema (This : Module) return Status_Manager.Status_Group_Maps.Map
   is ([]);

   type Module_Instance is abstract new Ada.Finalization.Limited_Controlled with null record;
   --  Children of `Module_Instance` should be declared with an unknown discriminant part to prevent accidental
   --  instantiation without a constructor.
   --
   --  Finalization should reset motors/heaters/etc. to their initial power-on state iff the module instance has been
   --  started.

   procedure Gcode_Dispatch
     (This               : in out Module_Instance;
      Args               : in out Gcode_Arguments.Arguments;
      Planner            : Planner_Interface'Class;
      Command_Identifier : Gcode_Command_Identifier);

   procedure Start (This : in out Module_Instance) is null;
   --  Modules should not start in the initialize procedure as the initialize procedure can be used to check for config
   --  errors without actually starting.

   package Module_Instance_Shared_Pointers is new Limited_Shared_Pointers (Module_Instance'Class);

   package Config_Data_Shared_Pointers is new GNATCOLL.Refcount.Shared_Pointers (Config.Config_Data);

   package Status_Emitter_Shared_Pointers is new GNATCOLL.Refcount.Shared_Pointers (Status_Manager.Status_Emitter);

   function Initialize
     (This                : Module;
      Config_Data         : Config_Data_Shared_Pointers.Ref;
      Report_Config_Error : access procedure (Path : Config.Config_Data_Paths.Vector; Message : Virtual_String);
      Status_Emitter      : Status_Emitter_Shared_Pointers.Ref;
      Get_Other_Instance  : access function (Tag : Ada.Tags.Tag) return Module_Instance_Shared_Pointers.Ref)
      return Module_Instance'Class
   is abstract;
   --  `Get_Other_Instance` attempts to initialize all other modules and then returns the instance with a tag equal to
   --  `Tag`. `Null_Ref` will be returned if the given module instance does not exist, this allows for probing of
   --  optional dependencies. Circular dependencies will not be detected. All instances returned from
   --  `Get_Other_Instance` will not be started at this point.
   --
   --  A shared pointer is used so that an instance reference may be kept after this function returns to be used in
   --  g-code commands.

private

   type Module is abstract tagged null record;

end Prunt.Modules;
