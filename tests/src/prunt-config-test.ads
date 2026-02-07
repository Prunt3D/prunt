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

with Trendy_Test;

package Prunt.Config.Test is

   function All_Tests return Trendy_Test.Test_Group;

   function Reports_Error_Outer_Config (Input : Virtual_String) return Boolean;

   function Reports_Error_Module_Config (Input : Virtual_String) return Boolean;

   function Reports_Error_Module_Config_To_Schema
     (Input : Virtual_String; Schema : Config_Property_Maps.Map; Check_For_Missing_Fields : Boolean := False)
      return Boolean;

private

   type Config_Property_Parameters_Unknown is new Config_Property_Parameters with null record;

   type Custom_Schema is new Versioned_Config_Schema with null record;

   overriding
   procedure Migrate (This : Custom_Schema; Old_Version : Config_Schema_Version; Data : in out Config_Data);

   type Error_Schema is new Versioned_Config_Schema with null record;

   overriding
   procedure Migrate (This : Error_Schema; Old_Version : Config_Schema_Version; Data : in out Config_Data);

   type Accessors_Schema is new Versioned_Config_Schema with null record;

   overriding
   procedure Migrate (This : Accessors_Schema; Old_Version : Config_Schema_Version; Data : in out Config_Data);

end Prunt.Config.Test;
