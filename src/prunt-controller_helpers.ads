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
with Ada.Containers.Vectors;
with Prunt.Controller_Generic_Types;
with Prunt.Gcode_Arguments;
with Prunt.JSON;
with Prunt.Module_Types;

generic
   with package Generic_Types is new Controller_Generic_Types (<>);
package Prunt.Controller_Helpers is

   use Generic_Types;

   type Gcode_Dispatch_Argument_Kinds is
     array (Gcode_Arguments.Arguments_Index) of Module_Types.Gcode_Argument_Allowed_Kinds;

   type Gcode_Dispatch_Entry is record
      Module_Name    : Virtual_String;
      Argument_Kinds : Gcode_Dispatch_Argument_Kinds;
   end record;

   package Gcode_Dispatch_Entry_Vectors is new Ada.Containers.Vectors (Positive, Gcode_Dispatch_Entry);

   function Return_False (Left, Right : Gcode_Dispatch_Entry_Vectors.Vector) return Boolean
   is (False);

   package Gcode_Dispatch_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Module_Types.Gcode_Command_Identifier,
        Element_Type => Gcode_Dispatch_Entry_Vectors.Vector,
        "<"          => Module_Types."<",
        "="          => Return_False);

   function Build_Gcode_Dispatch_Map (Active_Modules : Module_Maps.Map) return Gcode_Dispatch_Maps.Map;
   --  Builds symbolic dispatch buckets keyed by command identifier and raises if any command signatures overlap.

   function Matches (Dispatch_Entry : Gcode_Dispatch_Entry; Args : Gcode_Arguments.Arguments) return Boolean;
   --  Returns True when the parsed argument kinds match a symbolic dispatch signature.

   function Find_Module_Name
     (Dispatch_Map : Gcode_Dispatch_Maps.Map;
      Identifier   : Module_Types.Gcode_Command_Identifier;
      Args         : Gcode_Arguments.Arguments) return Virtual_String;
   --  Returns the owning module name for a command or an empty string if no entry matches.

   function Build_Gcode_JSON (Active_Modules : Module_Maps.Map) return JSON.JSON_Value;

end Prunt.Controller_Helpers;
