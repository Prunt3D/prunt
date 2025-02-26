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

with Prunt.Input_Shapers.Basic_Shapers;

package body Prunt.Input_Shapers.Shapers is

   function Create
     (Parameters : Axial_Shaper_Parameters; Interpolation_Time : Time; Initial_Position : Position)
      return Axial_Shapers
   is
      Result : Axial_Shapers :=
        (Shapers        => Axial_Shaper_Maps.Empty,
         Buffers        => Axial_Input_Buffer_Maps.Empty,
         Extra_End_Time => Cycle_Count'Last);
   begin
      for A in Axis_Name loop
         case Parameters (A).Kind is
            when No_Shaper | Zero_Vibration | Extra_Insensitive =>
               Result.Shapers.Insert
                 (A, Basic_Shapers.Create (Parameters (A), Interpolation_Time, Initial_Position (A)));
         end case;
      end loop;

      declare
         Minimum_Input_Offset   : Cycle_Count := Result.Shapers (Axis_Name'First).Input_Offset;
         Maximum_Input_Offset   : Cycle_Count := Result.Shapers (Axis_Name'First).Input_Offset;
         Maximum_Extra_End_Time : Cycle_Count := Result.Shapers (Axis_Name'First).Extra_End_Time;
      begin
         for A in Axis_Name loop
            Minimum_Input_Offset := Cycle_Count'Min (@, Result.Shapers (A).Input_Offset);
            Maximum_Input_Offset := Cycle_Count'Max (@, Result.Shapers (A).Input_Offset);
            Maximum_Extra_End_Time := Cycle_Count'Max (@, Result.Shapers (A).Extra_End_Time);
         end loop;

         for A in Axis_Name loop
            Result.Buffers.Insert
              (A,
               (Length        => Result.Shapers (A).Input_Offset - Minimum_Input_Offset,
                Buffer        => (others => Initial_Position (A)),
                Current_Index => 0));
         end loop;

         Result.Extra_End_Time := Maximum_Input_Offset - Minimum_Input_Offset + Maximum_Extra_End_Time;
      end;

      return Result;
   end Create;

   function Do_Step (Shapers : in out Axial_Shapers; Step : Position) return Position is
      Result : Position;
   begin
      for A in Axis_Name loop
         declare
            Buffer : access Input_Buffer := Shapers.Buffers.Reference (A).Element;
         begin
            Buffer.Buffer (Buffer.Current_Index) := Step (A);
            Buffer.Current_Index := (Buffer.Current_Index + 1) mod (Buffer.Length + 1);
            Result (A) := Shapers.Shapers (A).Do_Step (Buffer.Buffer (Buffer.Current_Index));
         end;
      end loop;

      return Result;
   end Do_Step;

   function Extra_End_Steps_Required (Shapers : Axial_Shapers) return Cycle_Count is
   begin
      return Shapers.Extra_End_Time;
   end Extra_End_Steps_Required;

end Prunt.Input_Shapers.Shapers;
