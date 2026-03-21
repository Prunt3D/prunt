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

with Prunt.Input_Shapers.Basic_Shapers;
with Prunt.Input_Shapers.Pressure_Advance_Shapers;

package body Prunt.Input_Shapers.Shapers is

   pragma Extensions_Allowed (On);

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

            when Pressure_Advance                               =>
               Result.Shapers.Insert
                 (A, Pressure_Advance_Shapers.Create (Parameters (A), Interpolation_Time, Initial_Position (A)));
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
                Buffer        => [others => Initial_Position (A)],
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
            Buffer : constant access Input_Buffer := Shapers.Buffers.Reference (A).Element;
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
