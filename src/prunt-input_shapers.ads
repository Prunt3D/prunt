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

package Prunt.Input_Shapers is

   type Cycle_Count is range -2**63 + 1 .. 2**63 - 1;

   subtype Shaper_Damping_Ratio is Dimensionless range 0.0 .. 1.0;
   subtype Residual_Vibration_Level is Dimensionless range 0.0 .. 1.0;

   type Zero_Vibration_Deriviatives_Count is range 0 .. 3;
   type Extra_Insensitive_Humps_Count is range 1 .. 3;
   type Modified_Zero_Vibration_Impulses_Count is range 2 .. 3;

   type Shaper_Kind is (No_Shaper, Zero_Vibration, Extra_Insensitive, Pressure_Advance);

   type Shaper_Parameters (Kind : Shaper_Kind := No_Shaper) is record
      case Kind is
         when No_Shaper =>
            null;

         when Zero_Vibration =>
            Zero_Vibration_Frequency     : Frequency;
            Zero_Vibration_Damping_Ratio : Shaper_Damping_Ratio;
            Zero_Vibration_Deriviatives  : Zero_Vibration_Deriviatives_Count;

         when Extra_Insensitive =>
            Extra_Insensitive_Frequency          : Frequency;
            Extra_Insensitive_Damping_Ratio      : Shaper_Damping_Ratio;
            Extra_Insensitive_Humps              : Extra_Insensitive_Humps_Count;
            Extra_Insensitive_Residual_Vibration : Residual_Vibration_Level;

         when Pressure_Advance =>
            Pressure_Advance_Time                   : Time;
            Pressure_Advance_Smooth_Time            : Time;
            Pressure_Advance_Smooth_Added_Part_Only : Boolean;
            Pressure_Advance_Smooth_Levels          : Positive;
      end case;
   end record;

   type Axial_Shaper_Parameters is array (Axis_Name) of Shaper_Parameters;

end Prunt.Input_Shapers;
