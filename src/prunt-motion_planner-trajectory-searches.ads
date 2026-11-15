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

--  The search behind every monotone velocity and time solve of the profile generator.

with Prunt.Motion_Planner.Trajectory.Machine_Numbers; use Prunt.Motion_Planner.Trajectory.Machine_Numbers;

package Prunt.Motion_Planner.Trajectory.Searches
  with SPARK_Mode => On
is

   Bracket_Max : constant := 2.0**1022;
   --  The largest upper end of a bracket in the proved domain: half of the largest machine number.

   Residual_Max : constant := 2.0**1000;
   --  The largest magnitude of a residual in the proved domain. A search is proved for a Residual whose results
   --  are within it: the model adds two residual magnitudes, and its own guard against that sum overflowing
   --  fails for magnitudes within a few units in the last place of the largest machine number.

   pragma Warnings (Off, "formal parameter * is not referenced");
   function Any_Bracket (Lower, Upper : Dimensionless) return Boolean
   is (True);
   pragma Warnings (On, "formal parameter * is not referenced");
   --  The default for Evaluable below: a Residual without a precondition may be evaluated anywhere.

   function Is_Close (High, Low, Tolerance : Dimensionless) return Boolean
   is (Tolerance > 0.0 and then High - Low <= Tolerance * Low)
   with Ghost, Pre => Tolerance <= 1.0 and then Low >= 0.0 and then Low <= High and then High <= Bracket_Max;
   --  High is no further above Low than the fraction Tolerance of Low: how a search with a positive tolerance
   --  may end.

   generic
      with function Residual (X : Dimensionless) return Dimensionless;
      with function Evaluable (Lower, Upper : Dimensionless) return Boolean is Any_Bracket;
      --  Whether Residual may be evaluated at every point from Lower to Upper. This only appears in the
      --  precondition, which is how a Residual with a precondition of its own is admitted: an instance is proved
      --  for the brackets of which this holds. It is never called when the program runs.
   function Largest_Nonpositive
     (Lower, Upper                   : Dimensionless;
      Lower_Residual, Upper_Residual : Dimensionless;
      Relative_Tolerance             : Dimensionless := 0.0;
      Guess                          : Dimensionless := -1.0;
      Use_Model                      : Boolean := True) return Dimensionless
   with
     Pre  =>
       Upper <= Bracket_Max
       and then Relative_Tolerance <= 1.0
       and then Lower_Residual >= -Residual_Max
       and then Upper_Residual <= Residual_Max
       and then Evaluable (Lower, Upper),
     Post =>
       (if Lower >= 0.0 and then Lower < Upper
        then
          Largest_Nonpositive'Result >= Lower
          and then Largest_Nonpositive'Result < Upper
          and then (Largest_Nonpositive'Result = Lower or else Residual (Largest_Nonpositive'Result) <= 0.0)
          and then To_Bits (Largest_Nonpositive'Result) < Last_Pattern
          and then (if Largest_Nonpositive'Result = 0.0 then To_Bits (Largest_Nonpositive'Result) = 0)
          and then
            (if not (Relative_Tolerance > 0.0)
             then
               To_Real (To_Bits (Largest_Nonpositive'Result) + 1) <= Upper
               and then
                 (To_Real (To_Bits (Largest_Nonpositive'Result) + 1) = Upper
                  or else Residual (To_Real (To_Bits (Largest_Nonpositive'Result) + 1)) > 0.0))
          and then
            (for some Pattern in Bits range To_Bits (Largest_Nonpositive'Result) + 1 .. Last_Pattern =>
               To_Real (Pattern) > Largest_Nonpositive'Result
               and then To_Real (Pattern) <= Upper
               and then (To_Real (Pattern) = Upper or else Residual (To_Real (Pattern)) > 0.0)
               and then
                 (Pattern = To_Bits (Largest_Nonpositive'Result) + 1
                  or else Is_Close (To_Real (Pattern), Largest_Nonpositive'Result, Relative_Tolerance)))
        else Largest_Nonpositive'Result = Lower);
   --  Proved for every instance in SPARK code, for a bracket within the precondition: the search terminates
   --  without a run-time error, and the result carries the certificate described below. A result of zero is
   --  positive zero. The quantified part is the upper half of the certificate for any tolerance: there is a
   --  machine number above the result which is the upper end or a point at which the residual was found positive,
   --  and it is the next machine number or, with a positive tolerance, no further from the result than that
   --  fraction of it.
   --
   --  Residual is treated as nondecreasing over 0 <= Lower < Upper, with Residual (Lower) <= 0 < Residual (Upper);
   --  neither end is evaluated. With a zero tolerance the result X is Lower or a point where Residual was evaluated
   --  and found nonpositive, and the next machine number above X is Upper or a point where Residual was evaluated
   --  and found positive. For a Residual that is monotone in machine arithmetic that is exactly the result of
   --  bisecting until the bracket's ends are adjacent, at a fraction of the evaluations; where rounding makes it
   --  locally nonmonotone both results still carry the same certificate, an evaluated nonpositive Residual.
   --
   --  The search first models Residual - Lower_Residual as a power of X - Lower, which fits ramp distances from a
   --  quarter power near a crackle-limited start through to a square when acceleration-limited, refitting the
   --  exponent to the latest two evaluations. Lower_Residual and Upper_Residual seed that model and need not be
   --  exact. Once the model's estimate stops moving, the bracket is closed around it in steps of machine numbers.
   --  Guess, when inside the bracket, replaces the model with a caller's estimate of the result. Without Use_Model
   --  and a Guess the residual ends are not used; an Upper_Residual that is not positive and finite only costs the
   --  model its first point. A positive Relative_Tolerance also stops at a bracket no wider than that fraction of
   --  its lower end, and then the model stops refining an estimate that moves by less than a quarter of it and the
   --  bracket is closed in steps of half of it.

end Prunt.Motion_Planner.Trajectory.Searches;
