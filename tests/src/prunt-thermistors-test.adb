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

with Ada.Numerics.Generic_Elementary_Functions;
with System.Assertions;
with Trendy_Test; use Trendy_Test;

package body Prunt.Thermistors.Test is

   pragma Extensions_Allowed (On);

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Dimensioned_Float);

   procedure Test_Thermistor (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;
      if Params.Kind not in Steinhart_Hart_Kind | Callendar_Van_Dusen_Kind then
         raise Constraint_Error;
      end if;

      for I in -250 .. 2000 loop
         Temp : constant Temperature := Dimensionless (I) * celsius;
         Calculated_Resistance : constant Resistance :=
           Temperature_To_Resistance
             ((Params with delta Minimum_Temperature => -250.0 * celsius, Maximum_Temperature => 2000.0 * celsius),
              Temp);
         Expected_Resistance : constant Resistance :=
           (if Params.Kind = Steinhart_Hart_Kind
            then Newton_Inverse_Solve_Steinhart_Hart (Params, Temp)
            else Solve_Callendar_Van_Dusen (Params, Temp));
         Difference : constant Resistance := abs (Calculated_Resistance - Expected_Resistance);
         Tolerance : constant Resistance := abs (Expected_Resistance * 1.0E-12);

         T.Assert
           (Difference < Tolerance,
            "Failed at "
            & Dimensionless'Image (Temp / celsius)
            & " degC. Expected "
            & Dimensionless'Image (Expected_Resistance / ohm)
            & " ohm, got"
            & Dimensionless'Image (Calculated_Resistance / ohm)
            & " ohm.");
      end loop;
   end Test_Thermistor;

   function Newton_Inverse_Solve_Steinhart_Hart (Params : Thermistor_Parameters; Temp : Temperature) return Resistance
   is
      Temp_K  : constant Dimensionless := Temp / celsius + 273.15;
      R_Guess : Dimensionless := 10.0;
   begin
      for I in 1 .. 100 loop
         Body_Val : constant Dimensionless :=
           Params.SH_A + Params.SH_B * Math.Log (R_Guess) + Params.SH_C * Math.Log (R_Guess) ** 3 - 1.0 / Temp_K;
         Derivative_Val : constant Dimensionless :=
           (Params.SH_B + 3.0 * Params.SH_C * Math.Log (R_Guess) ** 2) / R_Guess;

         R_Guess := Dimensionless'Max (1.0E-9, R_Guess - Body_Val / Derivative_Val);
      end loop;
      return Resistance (R_Guess * ohm);
   end Newton_Inverse_Solve_Steinhart_Hart;

   function Solve_Callendar_Van_Dusen (Params : Thermistor_Parameters; Temp : Temperature) return Resistance is
   begin
      return Params.CVD_R0 * (1.0 + Params.CVD_A * Temp / celsius + Params.CVD_B * (Temp / celsius) ** 2);
   end Solve_Callendar_Van_Dusen;

   procedure Test_ATC_Semitec_104GT_2 is new
     Test_Thermistor
       ((Kind                => Steinhart_Hart_Kind,
         SH_A                => 8.0965E-4,
         SH_B                => 2.1163E-4,
         SH_C                => 7.0742E-8,
         Minimum_Temperature => <>,
         Maximum_Temperature => <>));

   procedure Test_ATC_Semitec_104NT_4_R025H42G is new
     Test_Thermistor
       ((Kind                => Steinhart_Hart_Kind,
         SH_A                => 7.9582E-4,
         SH_B                => 2.1360E-4,
         SH_C                => 6.4830E-8,
         Minimum_Temperature => <>,
         Maximum_Temperature => <>));

   procedure Test_EPCOS_100K_B57560G104F is new
     Test_Thermistor
       ((Kind                => Steinhart_Hart_Kind,
         SH_A                => 7.2213E-4,
         SH_B                => 2.1676E-4,
         SH_C                => 8.9293E-8,
         Minimum_Temperature => <>,
         Maximum_Temperature => <>));

   procedure Test_Generic_3950 is new
     Test_Thermistor
       ((Kind                => Steinhart_Hart_Kind,
         SH_A                => 7.9347E-4,
         SH_B                => 2.0076E-4,
         SH_C                => 1.6328E-7,
         Minimum_Temperature => <>,
         Maximum_Temperature => <>));

   procedure Test_SliceEngineering_450 is new
     Test_Thermistor
       ((Kind                => Steinhart_Hart_Kind,
         SH_A                => 3.0553E-4,
         SH_B                => 2.1171E-4,
         SH_C                => 1.1962E-7,
         Minimum_Temperature => <>,
         Maximum_Temperature => <>));

   procedure Test_TDK_NTCG104LH104JT1 is new
     Test_Thermistor
       ((Kind                => Steinhart_Hart_Kind,
         SH_A                => 9.7639E-4,
         SH_B                => 1.9688E-4,
         SH_C                => 7.2671E-8,
         Minimum_Temperature => <>,
         Maximum_Temperature => <>));

   procedure Test_Honeywell_100K_135_104LAG_J01 is new
     Test_Thermistor
       ((Kind                => Steinhart_Hart_Kind,
         SH_A                => 4.5695E-4,
         SH_B                => 2.5163E-4,
         SH_C                => 0.0,
         Minimum_Temperature => <>,
         Maximum_Temperature => <>));

   procedure Test_NTC_100K_MGB18_104F39050L32 is new
     Test_Thermistor
       ((Kind                => Steinhart_Hart_Kind,
         SH_A                => 5.4598E-4,
         SH_B                => 2.4390E-4,
         SH_C                => 0.0,
         Minimum_Temperature => <>,
         Maximum_Temperature => <>));

   procedure Test_PT_1000_PT_385 is new
     Test_Thermistor
       ((Kind                => Callendar_Van_Dusen_Kind,
         CVD_R0              => 1_000.0 * ohm,
         CVD_A               => 3.9083E-3,
         CVD_B               => -5.775E-7,
         Minimum_Temperature => <>,
         Maximum_Temperature => <>));

   procedure Test_PT_1000_PT_392 is new
     Test_Thermistor
       ((Kind                => Callendar_Van_Dusen_Kind,
         CVD_R0              => 1_000.0 * ohm,
         CVD_A               => 3.9827E-3,
         CVD_B               => -5.875E-7,
         Minimum_Temperature => <>,
         Maximum_Temperature => <>));

   procedure Test_Disabled (T : in out Trendy_Test.Operation'Class) is
   begin
      T.Register;

      R : constant Resistance :=
        Temperature_To_Resistance
          ((Kind => Disabled_Kind, Minimum_Temperature => 0.0 * celsius, Maximum_Temperature => 100.0 * celsius),
           50.0 * celsius)
      with Unreferenced;

      T.Fail ("Should have raised Constraint_Error");
   exception
      when Constraint_Error | System.Assertions.Assert_Failure =>
         null;
   end Test_Disabled;

   function All_Tests return Trendy_Test.Test_Group is
   begin
      return
        [Test_ATC_Semitec_104GT_2'Access,
         Test_ATC_Semitec_104NT_4_R025H42G'Access,
         Test_EPCOS_100K_B57560G104F'Access,
         Test_Generic_3950'Access,
         Test_SliceEngineering_450'Access,
         Test_TDK_NTCG104LH104JT1'Access,
         Test_Honeywell_100K_135_104LAG_J01'Access,
         Test_NTC_100K_MGB18_104F39050L32'Access,
         Test_PT_1000_PT_385'Access,
         Test_PT_1000_PT_392'Access,
         Test_Disabled'Access];
   end All_Tests;

end Prunt.Thermistors.Test;
