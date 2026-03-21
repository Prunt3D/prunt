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
