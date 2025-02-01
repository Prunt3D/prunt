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

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics; use Ada.Numerics;

package body Prunt.Input_Shapers.Basic_Shapers is

   package Dimensionless_Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensionless);
   use Dimensionless_Math;

   function Create
     (Parameters : Shaper_Parameters; Interpolation_Time : Time; Start_Position : Length) return Basic_Shaper
   is
      Impulses : constant Impulses_Array := Compute_Impulses (Parameters, Interpolation_Time);
   begin
      return
        (Input_Offset         => Compute_Input_Offset (Impulses),
         Extra_End_Time       => Impulses (Impulses'Last).Output_Delay + 1,
         Impulse_Count        => Impulses'Length,
         Buffer_Size          => Impulses (Impulses'Last).Output_Delay + 1,
         Impulses             => Impulses,
         Buffer               => (others => Start_Position),
         Current_Buffer_Index => 0);
   end Create;

   overriding function Do_Step (This : in out Basic_Shaper; Step : Length) return Length is
      Result : Length := 0.0 * mm;
   begin
      This.Current_Buffer_Index               := (@ + 1) mod This.Buffer_Size;
      This.Buffer (This.Current_Buffer_Index) := Step;

      for I of This.Impulses loop
         Result :=
           Result + This.Buffer ((This.Current_Buffer_Index - I.Output_Delay) mod This.Buffer_Size) * I.Output_Ratio;
      end loop;

      return Result;
   end Do_Step;

   function Compute_Impulses (Parameters : Shaper_Parameters; Interpolation_Time : Time) return Impulses_Array is
   begin
      case Parameters.Kind is
         when No_Shaper =>
            return (1 => (Output_Delay => 0, Output_Ratio => 1.0));
         when Zero_Vibration =>
            --  https://doi.org/10.1115/1.2894142
            declare
               K  : constant Dimensionless :=
                 Exp
                   (-Parameters.Zero_Vibration_Damping_Ratio * Pi /
                    (1.0 - Parameters.Zero_Vibration_Damping_Ratio**2)**(1 / 2));
               TD : constant Dimensionless :=
                 1.0 /
                 (Parameters.Zero_Vibration_Frequency * (1.0 - Parameters.Zero_Vibration_Damping_Ratio**2)**(1 / 2) *
                  Interpolation_Time);
               --  Note that the original paper uses rad/s, but we use hertz here so the 2 * pi term has been removed.
            begin
               case Parameters.Zero_Vibration_Deriviatives is
                  when 0 =>
                     return
                       ((Output_Delay => 0, Output_Ratio => 1.0 / (1.0 + K)),
                        (Output_Delay => Cycle_Count (0.5 * TD), Output_Ratio => K / (1.0 + K)));
                  when 1 =>
                     declare
                        A1         : constant Dimensionless := 1.0;
                        A2         : constant Dimensionless := 2.0 * K;
                        A3         : constant Dimensionless := K**2;
                        Output_Sum : constant Dimensionless := A1 + A2 + A3;
                     begin
                        return
                          ((Output_Delay => 0, Output_Ratio => A1 / Output_Sum),
                           (Output_Delay => Cycle_Count (0.5 * TD), Output_Ratio => A2 / Output_Sum),
                           (Output_Delay => Cycle_Count (TD), Output_Ratio => A3 / Output_Sum));
                     end;
                  when 2 =>
                     declare
                        A1         : constant Dimensionless := 1.0;
                        A2         : constant Dimensionless := 3.0 * K;
                        A3         : constant Dimensionless := 3.0 * K**2;
                        A4         : constant Dimensionless := K**3;
                        Output_Sum : constant Dimensionless := A1 + A2 + A3 + A4;
                     begin
                        return
                          ((Output_Delay => 0, Output_Ratio => A1 / Output_Sum),
                           (Output_Delay => Cycle_Count (0.5 * TD), Output_Ratio => A2 / Output_Sum),
                           (Output_Delay => Cycle_Count (TD), Output_Ratio => A3 / Output_Sum),
                           (Output_Delay => Cycle_Count (1.5 * TD), Output_Ratio => A4 / Output_Sum));
                     end;
                  when 3 =>
                     declare
                        A1         : constant Dimensionless := 1.0;
                        A2         : constant Dimensionless := 4.0 * K;
                        A3         : constant Dimensionless := 6.0 * K**2;
                        A4         : constant Dimensionless := 4.0 * K**3;
                        A5         : constant Dimensionless := K**4;
                        Output_Sum : constant Dimensionless := A1 + A2 + A3 + A4 + A5;
                     begin
                        return
                          ((Output_Delay => 0, Output_Ratio => A1 / Output_Sum),
                           (Output_Delay => Cycle_Count (0.5 * TD), Output_Ratio => A2 / Output_Sum),
                           (Output_Delay => Cycle_Count (TD), Output_Ratio => A3 / Output_Sum),
                           (Output_Delay => Cycle_Count (1.5 * TD), Output_Ratio => A4 / Output_Sum),
                           (Output_Delay => Cycle_Count (2.0 * TD), Output_Ratio => A5 / Output_Sum));
                     end;
               end case;
            end;
         when Extra_Insensitive =>
            --  https://doi.org/10.1115/1.2801257
            declare
               K  : constant Dimensionless :=
                 Exp
                   (-Parameters.Extra_Insensitive_Damping_Ratio * Pi /
                    (1.0 - Parameters.Extra_Insensitive_Damping_Ratio**2)**(1 / 2));
               TD : constant Dimensionless :=
                 1.0 /
                 (Parameters.Zero_Vibration_Frequency * (1.0 - Parameters.Zero_Vibration_Damping_Ratio**2)**(1 / 2) *
                  Interpolation_Time);
               --  Note that the original paper uses rad/s, but we use hertz here so the 2 * pi term has been removed.

               V : constant Residual_Vibration_Level := Parameters.Extra_Insensitive_Residual_Vibration;
            begin
               case Parameters.Extra_Insensitive_Humps is
                  when 1 =>
                     declare
                        A1         : constant Dimensionless := (1.0 + V) / 4.0;
                        A2         : constant Dimensionless := K * (1.0 - V) / 2.0;
                        A3         : constant Dimensionless := K**2 * (1.0 + V) / 4.0;
                        Output_Sum : constant Dimensionless := A1 + A2 + A3;
                     begin
                        return
                          ((Output_Delay => 0, Output_Ratio => A1 / Output_Sum),
                           (Output_Delay => Cycle_Count (0.5 * TD), Output_Ratio => A2 / Output_Sum),
                           (Output_Delay => Cycle_Count (TD), Output_Ratio => A3 / Output_Sum));
                     end;
                  when 2 =>
                     declare
                        X          : constant Dimensionless := (V**2 * ((1.0 - V**2)**(1 / 2) + 1.0))**(1 / 3);
                        A1         : constant Dimensionless := (3.0 * X**2 + 2.0 * X + 3.0 * V**2) / (16.0 * X);
                        A2         : constant Dimensionless := (0.5 - A1) * K;
                        A3         : constant Dimensionless := (0.5 - A1) * K**2;
                        A4         : constant Dimensionless := A1 * K**3;
                        Output_Sum : constant Dimensionless := A1 + A2 + A3 + A4;
                     begin
                        return
                          ((Output_Delay => 0, Output_Ratio => A1 / Output_Sum),
                           (Output_Delay => Cycle_Count (0.5 * TD), Output_Ratio => A2 / Output_Sum),
                           (Output_Delay => Cycle_Count (TD), Output_Ratio => A3 / Output_Sum),
                           (Output_Delay => Cycle_Count (1.5 * TD), Output_Ratio => A4 / Output_Sum));
                     end;
                  when 3 =>
                     declare
                        A1         : constant Dimensionless := (1.0 + 3.0 * V + 2.0 * (2.0 * (V**2 + V))**(1 / 2));
                        A2         : constant Dimensionless := K * (1.0 - V) / 4.0;
                        A3         : constant Dimensionless := K**2 * (1.0 - 2.0 * (A1 + (1.0 - V) / 4.0));
                        A4         : constant Dimensionless := K**3 * (1.0 - V) / 4.0;
                        A5         : constant Dimensionless := K**4 * A1;
                        Output_Sum : constant Dimensionless := A1 + A2 + A3 + A4 + A5;
                     begin
                        return
                          ((Output_Delay => 0, Output_Ratio => A1 / Output_Sum),
                           (Output_Delay => Cycle_Count (0.5 * TD), Output_Ratio => A2 / Output_Sum),
                           (Output_Delay => Cycle_Count (TD), Output_Ratio => A3 / Output_Sum),
                           (Output_Delay => Cycle_Count (1.5 * TD), Output_Ratio => A4 / Output_Sum),
                           (Output_Delay => Cycle_Count (2.0 * TD), Output_Ratio => A5 / Output_Sum));
                     end;
               end case;
            end;
         when others =>
            raise Constraint_Error with "Wrong Create function called for given shaper parameters.";
      end case;
   end Compute_Impulses;

   function Compute_Input_Offset (Impulses : Impulses_Array) return Cycle_Count is
      Sum : Dimensionless := 0.0;
   begin
      for I of Impulses loop
         Sum := @ + I.Output_Ratio * Dimensionless (I.Output_Delay);
      end loop;

      return Cycle_Count (-Sum);
   end Compute_Input_Offset;

end Prunt.Input_Shapers.Basic_Shapers;
