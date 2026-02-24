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

--  This package just contains a few functions for `Prunt.Controller` that are required to be called in the
--  specification but are difficult to implement with expression functions, and things that those functions rely
--  on.
--
--  Specifically, this is currently just `Build_Gcode_Dispatch_Map`, which we want to call in the specification so
--  we can generate a string to pass to the web server generic.

pragma Extensions_Allowed (On);

with Ada.Containers.Ordered_Maps;
with Prunt.Controller_Generic_Types;
with Prunt.Gcode_Arguments;
with Prunt.JSON;
with Prunt.Module_Types;

generic
   with package Generic_Types is new Controller_Generic_Types (<>);
package Prunt.Controller_Helpers is

   use Generic_Types;

   type Gcode_Dispatch_Argument_Kinds is array (Gcode_Arguments.Arguments_Index) of Gcode_Arguments.Argument_Kind;

   type Gcode_Dispatch_Key is record
      Identifier     : Module_Types.Gcode_Command_Identifier;
      Argument_Kinds : Gcode_Dispatch_Argument_Kinds;
   end record;

   function "<" (Left, Right : Gcode_Dispatch_Key) return Boolean;

   package Gcode_Dispatch_Maps is new Ada.Containers.Ordered_Maps (Gcode_Dispatch_Key, Virtual_String);

   function Build_Gcode_Dispatch_Map (Active_Modules : Module_Maps.Map) return Gcode_Dispatch_Maps.Map;

   function Build_Gcode_JSON (Active_Modules : Module_Maps.Map) return JSON.JSON_Value;

end Prunt.Controller_Helpers;
