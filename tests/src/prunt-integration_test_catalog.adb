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
--------------------------------------------------

pragma Extensions_Allowed (On);

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Prunt.Integration_Test_Catalog is

   type Scenario_Definition is record
      Name : Unbounded_String;
      Path : Unbounded_String;
   end record;

   Scenarios : constant array (Positive range <>) of Scenario_Definition :=
     [(To_Unbounded_String ("integration.simple_linear_move"),
       To_Unbounded_String ("integration_scenarios/simple_linear_move.json")),
      (To_Unbounded_String ("integration.cancel_mid_move"),
       To_Unbounded_String ("integration_scenarios/cancel_mid_move.json")),
      (To_Unbounded_String ("integration.heater_event"),
       To_Unbounded_String ("integration_scenarios/heater_event.json")),
      (To_Unbounded_String ("integration.fans"),
       To_Unbounded_String ("integration_scenarios/fans.json")),
      (To_Unbounded_String ("integration.fans.always_on"),
       To_Unbounded_String ("integration_scenarios/fans_always_on.json")),
      (To_Unbounded_String ("integration.dwell"),
       To_Unbounded_String ("integration_scenarios/dwell.json")),
      (To_Unbounded_String ("integration.motion_with_heater_event"),
       To_Unbounded_String ("integration_scenarios/motion_with_heater_event.json")),
      (To_Unbounded_String ("integration.motion_rapid_move"),
       To_Unbounded_String ("integration_scenarios/motion_rapid_move.json")),
      (To_Unbounded_String ("integration.motion_modal_state"),
       To_Unbounded_String ("integration_scenarios/motion_modal_state.json")),
      (To_Unbounded_String ("integration.motion_saved_positions"),
       To_Unbounded_String ("integration_scenarios/motion_saved_positions.json")),
      (To_Unbounded_String ("integration.motion_retraction_scaling"),
       To_Unbounded_String ("integration_scenarios/motion_retraction_scaling.json")),
      (To_Unbounded_String ("integration.motion_auto_retract"),
       To_Unbounded_String ("integration_scenarios/motion_auto_retract.json")),
      (To_Unbounded_String ("integration.motion_retract_recover"),
       To_Unbounded_String ("integration_scenarios/motion_retract_recover.json")),
      (To_Unbounded_String ("integration.motion_zero_distance_retract"),
       To_Unbounded_String ("integration_scenarios/motion_zero_distance_retract.json")),
      (To_Unbounded_String ("integration.motion_z_lift_only_retract"),
       To_Unbounded_String ("integration_scenarios/motion_z_lift_only_retract.json")),
      (To_Unbounded_String ("integration.motion_e_only_retract"),
       To_Unbounded_String ("integration_scenarios/motion_e_only_retract.json")),
      (To_Unbounded_String ("integration.motion_bad_direct_commands"),
       To_Unbounded_String ("integration_scenarios/motion_bad_direct_commands.json")),
      (To_Unbounded_String ("integration.motion_pause_no_park"),
       To_Unbounded_String ("integration_scenarios/motion_pause_no_park.json")),
      (To_Unbounded_String ("integration.motion_pause_relative_park"),
       To_Unbounded_String ("integration_scenarios/motion_pause_relative_park.json")),
      (To_Unbounded_String ("integration.motion_pause_relative_lower_error"),
       To_Unbounded_String ("integration_scenarios/motion_pause_relative_lower_error.json")),
      (To_Unbounded_String ("integration.motion_pause_relative_upper_error"),
       To_Unbounded_String ("integration_scenarios/motion_pause_relative_upper_error.json")),
      (To_Unbounded_String ("integration.motion_pause_absolute_park"),
       To_Unbounded_String ("integration_scenarios/motion_pause_absolute_park.json")),
      (To_Unbounded_String ("integration.motion_pause_absolute_lowering_park"),
       To_Unbounded_String ("integration_scenarios/motion_pause_absolute_lowering_park.json")),
      (To_Unbounded_String ("integration.motion_pause_absolute_raise_park"),
       To_Unbounded_String ("integration_scenarios/motion_pause_absolute_raise_park.json")),
      (To_Unbounded_String ("integration.motion_pause_absolute_relative_z_park"),
       To_Unbounded_String ("integration_scenarios/motion_pause_absolute_relative_z_park.json"))];

   function Find_Scenario_Path (Name : String) return String is
   begin
      for Scenario of Scenarios loop
         if To_String (Scenario.Name) = Name then
            return To_String (Scenario.Path);
         end if;
      end loop;

      raise Constraint_Error with "Unknown integration scenario: " & Name;
   end Find_Scenario_Path;

   function Matches_Filter (Name, Filter : String) return Boolean is
      use Ada.Characters.Handling;
   begin
      return Filter = "" or else Ada.Strings.Fixed.Index (To_Lower (Name), To_Lower (Filter)) /= 0;
   end Matches_Filter;

   function Scenario_Count return Natural is
   begin
      return Scenarios'Length;
   end Scenario_Count;

   function Scenario_Name (Index : Positive) return String is
   begin
      return To_String (Scenarios (Index).Name);
   end Scenario_Name;

end Prunt.Integration_Test_Catalog;
