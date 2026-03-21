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

with Trendy_Test; use Trendy_Test;

package body Prunt.Moving_Averages.Test is

   pragma Extensions_Allowed (On);

   procedure Run_Test
     (CMA              : in out Cascading_Moving_Average;
      Inputs           : Number_Array;
      Expected_Outputs : Number_Array;
      T                : in out Trendy_Test.Operation'Class)
   is
      Output_Value : Number;
   begin
      for I in Inputs'Range loop
         Output_Value := Do_Step (CMA, Input => Inputs (I));
         T.Assert (abs (Output_Value - Expected_Outputs (I)) <= abs Expected_Outputs (I) * 1.0E-5, "Step " & I'Image);
      end loop;
   end Run_Test;

   procedure Test_Create (T : in out Trendy_Test.Operation'Class) is
      procedure Check_Create
        (N_Levels          : Positive;
         Max_Total_Width   : Natural;
         Expected_WPL      : Natural;
         Expected_N_Levels : Positive;
         Expected_Delay    : Natural)
      is
         CMA : constant Cascading_Moving_Average :=
           Create (N_Levels => N_Levels, Max_Total_Width => Max_Total_Width, Initial_Value => 0.0);
      begin
         T.Assert (CMA.N_Levels = Expected_N_Levels, "N_Levels");
         T.Assert (CMA.Width_Per_Level = Expected_WPL, "Width_Per_Level");
         T.Assert (Total_Delay (CMA) = Expected_Delay, "Total_Delay");
      end Check_Create;
   begin
      T.Register;

      Check_Create
        (N_Levels => 1, Max_Total_Width => 5, Expected_WPL => 5, Expected_N_Levels => 1, Expected_Delay => 2);
      Check_Create
        (N_Levels => 2, Max_Total_Width => 5, Expected_WPL => 2, Expected_N_Levels => 2, Expected_Delay => 1);
      Check_Create
        (N_Levels => 3, Max_Total_Width => 9, Expected_WPL => 3, Expected_N_Levels => 3, Expected_Delay => 3);
      Check_Create
        (N_Levels => 5, Max_Total_Width => 5, Expected_WPL => 1, Expected_N_Levels => 5, Expected_Delay => 0);
   end Test_Create;

   procedure Test_Do_Step_Multi_Level_Constant_Input (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filter           : Cascading_Moving_Average :=
        Create (N_Levels => 3, Max_Total_Width => 9, Initial_Value => 0.0);
      Inputs           : constant Number_Array := (1 .. 100 => 20.0);
      Expected_Outputs : constant Number_Array :=
        (1        => 20.0 / 27.0,
         2        => 80.0 / 27.0,
         3        => 200.0 / 27.0,
         4        => 340.0 / 27.0,
         5        => 460.0 / 27.0,
         6        => 520.0 / 27.0,
         7 .. 100 => 20.0);

      Run_Test (CMA => Filter, Inputs => Inputs, Expected_Outputs => Expected_Outputs, T => T);
   end Test_Do_Step_Multi_Level_Constant_Input;

   procedure Test_Do_Step_Multi_Level_Impulse_Input (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filter           : Cascading_Moving_Average :=
        Create (N_Levels => 2, Max_Total_Width => 4, Initial_Value => 0.0);
      Inputs           : constant Number_Array := (0.0, 0.0, 1.0, 0.0, 0.0, 0.0);
      Expected_Outputs : constant Number_Array := (0.0, 0.0, 0.25, 0.5, 0.25, 0.0);

      Run_Test (CMA => Filter, Inputs => Inputs, Expected_Outputs => Expected_Outputs, T => T);
   end Test_Do_Step_Multi_Level_Impulse_Input;

   procedure Test_Do_Step_Multi_Level_Step_Input (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filter           : Cascading_Moving_Average :=
        Create (N_Levels => 2, Max_Total_Width => 4, Initial_Value => 0.0);
      Inputs           : constant Number_Array := (1 .. 10 => 0.0, 11 .. 100 => 1.0);
      Expected_Outputs : constant Number_Array := (1 .. 10 => 0.0, 11 => 0.25, 12 => 0.75, 13 .. 100 => 1.0);

      Run_Test (CMA => Filter, Inputs => Inputs, Expected_Outputs => Expected_Outputs, T => T);
   end Test_Do_Step_Multi_Level_Step_Input;

   procedure Test_Do_Step_No_Filtering (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filter           : Cascading_Moving_Average :=
        Create (N_Levels => 1, Max_Total_Width => 1, Initial_Value => 0.0);
      Inputs           : constant Number_Array := (10.0, -5.0, 0.0);
      Expected_Outputs : constant Number_Array := (10.0, -5.0, 0.0);

      Run_Test (CMA => Filter, Inputs => Inputs, Expected_Outputs => Expected_Outputs, T => T);
   end Test_Do_Step_No_Filtering;

   procedure Test_Do_Step_Single_Level_Constant_Input (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filter           : Cascading_Moving_Average :=
        Create (N_Levels => 1, Max_Total_Width => 5, Initial_Value => 0.0);
      Inputs           : constant Number_Array := (1 .. 100 => 10.0);
      Expected_Outputs : constant Number_Array := (1 => 2.0, 2 => 4.0, 3 => 6.0, 4 => 8.0, 5 .. 100 => 10.0);

      Run_Test (CMA => Filter, Inputs => Inputs, Expected_Outputs => Expected_Outputs, T => T);
   end Test_Do_Step_Single_Level_Constant_Input;

   procedure Test_Do_Step_Single_Level_Impulse_Input (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filter           : Cascading_Moving_Average :=
        Create (N_Levels => 1, Max_Total_Width => 3, Initial_Value => 0.0);
      Inputs           : constant Number_Array := (0.0, 1.0, 0.0, 0.0, 0.0);
      Expected_Outputs : constant Number_Array := (0.0, 1.0 / 3.0, 1.0 / 3.0, 1.0 / 3.0, 0.0);

      Run_Test (CMA => Filter, Inputs => Inputs, Expected_Outputs => Expected_Outputs, T => T);
   end Test_Do_Step_Single_Level_Impulse_Input;

   procedure Test_Do_Step_Single_Level_Step_Input (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      Filter           : Cascading_Moving_Average :=
        Create (N_Levels => 1, Max_Total_Width => 5, Initial_Value => 0.0);
      Inputs           : constant Number_Array := (1 .. 5 => 0.0, 6 .. 100 => 1.0);
      Expected_Outputs : constant Number_Array :=
        (1 .. 5 => 0.0, 6 => 0.2, 7 => 0.4, 8 => 0.6, 9 => 0.8, 10 .. 100 => 1.0);

      Run_Test (CMA => Filter, Inputs => Inputs, Expected_Outputs => Expected_Outputs, T => T);
   end Test_Do_Step_Single_Level_Step_Input;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_Create'Unrestricted_Access,
         Test_Do_Step_Multi_Level_Constant_Input'Unrestricted_Access,
         Test_Do_Step_Multi_Level_Impulse_Input'Unrestricted_Access,
         Test_Do_Step_Multi_Level_Step_Input'Unrestricted_Access,
         Test_Do_Step_No_Filtering'Unrestricted_Access,
         Test_Do_Step_Single_Level_Constant_Input'Unrestricted_Access,
         Test_Do_Step_Single_Level_Impulse_Input'Unrestricted_Access,
         Test_Do_Step_Single_Level_Step_Input'Unrestricted_Access];
   end All_Tests;

end Prunt.Moving_Averages.Test;
