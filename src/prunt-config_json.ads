-----------------------------------------------------------------------------
--                                                                         --
--                   Part of the Prunt Motion Controller                   --
--                                                                         --
--            Copyright (C) 2024 Liam Powell (liam@prunt3d.com)            --
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

with Prunt.Thermistors;     use Prunt.Thermistors;
with Prunt.Heaters;         use Prunt.Heaters;
with Interfaces;            use Interfaces;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Prunt.Motion_Planner;
with TOML;
with Prunt.TMC_Types;
with Prunt.TMC_Types.TMC2240;

generic
   type Stepper_Name is (<>);
   type Stepper_Kinds_Type is array (Stepper_Name) of Stepper_Kind;
   Stepper_Kinds : Stepper_Kinds_Type;
   type Heater_Name is (<>);
   type Thermistor_Name is (<>);
   type Fan_Name is (<>);
   type Input_Switch_Name is (<>);
   Config_Path : String;
package Prunt.Config_JSON is

   Schema : constant String;

private

   package Discrete_String_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   type Property_Kind is (Boolean_Kind, Discrete_Kind, Integer_Kind, Float_Kind, Sequence_Kind, Variant_Kind);

   type Property_Parameters (Kind : Property_Kind);
   type Property_Parameters_Access is not null access constant Property_Parameters;

   package Property_Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, Property_Parameters_Access);

   type Property_Parameters (Kind : Property_Kind) is record
      Description : Unbounded_String;
      case Kind is
         when Boolean_Kind =>
            null;
         when Discrete_Kind =>
            Discrete_Options : Discrete_String_Sets.Set;
         when Integer_Kind =>
            Integer_Min  : Long_Long_Integer;
            Integer_Max  : Long_Long_Integer;
            Integer_Unit : Unbounded_String;
         when Float_Kind =>
            Float_Min  : Long_Float;
            Float_Max  : Long_Float;
            Float_Unit : Unbounded_String;
         when Sequence_Kind | Variant_Kind =>
            Children : Property_Maps.Map;
      end case;
   end record;

   function Build_Schema return Property_Maps.Map;
   Internal_Schema : constant Property_Maps.Map := Build_Schema;

   function Schema_To_JSON (Schema : Property_Maps.Map) return String;
   Schema : constant String := Schema_To_JSON (Internal_Schema);

   package Flat_Schemas is new Ada.Containers.Indefinite_Ordered_Maps (String, Property_Parameters);

   function Build_Flat_Schema return Flat_Schemas.Map with
     Post =>
      (for all P of Build_Flat_Schema'Result =>
         P.Kind in Boolean_Kind | Discrete_Kind | Integer_Kind | Float_Kind);
   Flat_Schema : constant Flat_Schemas.Map := Build_Flat_Schema;

end Prunt.Config_JSON;
