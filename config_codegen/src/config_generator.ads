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

with Ada.Containers.Vectors;
with VSS.Strings; use VSS.Strings;

with Config_Types; use Config_Types;

package Config_Generator is

   procedure Generate_Files (Filename : String; Data : Module_Data; Global_Config : Config_Maps.Map);

private

   package String_Vectors is new Ada.Containers.Vectors (Positive, Virtual_String);
   use type String_Vectors.Vector;

   procedure Write_File (Filename : String; Content : Virtual_String);
   procedure Generate_Gcode_Dispatch (Filename : String; Data : Module_Data);

end Config_Generator;
