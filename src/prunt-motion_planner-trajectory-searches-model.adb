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

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

with Prunt.Motion_Planner.Trajectory.Elementary; use Prunt.Motion_Planner.Trajectory.Elementary;

package body Prunt.Motion_Planner.Trajectory.Searches.Model
  with SPARK_Mode => On
is

   Magnitude_Min : constant Dimensionless := 2.0**(-500);
   Magnitude_Max : constant Dimensionless := 2.0**500;

   function Is_Moderate (X : Dimensionless) return Boolean
   is (X >= Magnitude_Min and then X <= Magnitude_Max);
   --  The quotient of two moderate numbers neither overflows nor underflows to zero, so it has a logarithm.

   Exponent_Min : constant Dimensionless := 0.05;
   Exponent_Max : constant Dimensionless := 8.0;
   --  The exponents which the model accepts from a fit.

   Offset_Log_Min : constant Dimensionless := 2.0**(-60);
   --  The logarithm of a machine number other than one is at least 2**-53 in magnitude, so a correct logarithm of
   --  the quotient of two different offsets passes this test, which keeps the quotient of the logarithms
   --  representable.

   procedure Estimate_Result
     (Anchor, Target                   : Dimensionless;
      Previous_Offset, Previous_Excess : Dimensionless;
      Latest_Offset, Latest_Excess     : Dimensionless;
      Exponent                         : in out Dimensionless;
      Estimate                         : in out Dimensionless;
      Have_Estimate                    : in out Boolean) is
   begin
      --  The model offers no estimate for magnitudes which no search of a motion profile sees. Deciding that here
      --  is what lets every operation below be proved not to overflow.
      if not (Is_Moderate (Target)
              and then Is_Moderate (Latest_Excess)
              and then Is_Moderate (Latest_Offset)
              and then Anchor >= 0.0
              and then Anchor <= Bracket_Max
              and then Exponent >= Exponent_Min
              and then Exponent <= Exponent_Max)
      then
         Have_Estimate := False;
         return;
      end if;

      if Previous_Excess > 0.0
        and then Previous_Offset > 0.0
        and then Previous_Excess /= Latest_Excess
        and then Previous_Offset /= Latest_Offset
      then
         if not (Is_Moderate (Previous_Excess) and then Is_Moderate (Previous_Offset)) then
            Have_Estimate := False;
            return;
         end if;
         declare
            Excess_Ratio : constant Dimensionless := Latest_Excess / Previous_Excess;
            Offset_Ratio : constant Dimensionless := Latest_Offset / Previous_Offset;
            Excess_Log   : constant Dimensionless := Dimensionless_Math.Log (Excess_Ratio);
            Offset_Log   : constant Dimensionless := Dimensionless_Math.Log (Offset_Ratio);
         begin
            Lemma_Log_Is_Bounded (Excess_Ratio);
            if not (abs Offset_Log >= Offset_Log_Min) then
               Have_Estimate := False;
               return;
            end if;
            declare
               Fitted : constant Dimensionless := Excess_Log / Offset_Log;
            begin
               if Fitted >= Exponent_Min and then Fitted <= Exponent_Max then
                  Exponent := Fitted;
               end if;
            end;
         end;
      end if;
      declare
         Target_Ratio : constant Dimensionless := Target / Latest_Excess;
         Target_Log   : constant Dimensionless := Dimensionless_Math.Log (Target_Ratio);
      begin
         Lemma_Log_Is_Bounded (Target_Ratio);
         declare
            Log_Step : constant Dimensionless := Target_Log / Exponent;
         begin
            if abs Log_Step <= 80.0 then
               Estimate := Anchor + Latest_Offset * Bounded_Exp (Log_Step);
               Have_Estimate := True;
            end if;
         end;
      end;
   end Estimate_Result;

end Prunt.Motion_Planner.Trajectory.Searches.Model;
