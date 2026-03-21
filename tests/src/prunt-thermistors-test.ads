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

package Prunt.Thermistors.Test is

   function All_Tests return Trendy_Test.Test_Group;

private

   generic
      Params : Thermistor_Parameters;
   procedure Test_Thermistor (T : in out Trendy_Test.Operation'Class);

   function Newton_Inverse_Solve_Steinhart_Hart (Params : Thermistor_Parameters; Temp : Temperature) return Resistance
   with Pre => Params.Kind = Steinhart_Hart_Kind;

   function Solve_Callendar_Van_Dusen (Params : Thermistor_Parameters; Temp : Temperature) return Resistance
   with Pre => Params.Kind = Callendar_Van_Dusen_Kind;

end Prunt.Thermistors.Test;
