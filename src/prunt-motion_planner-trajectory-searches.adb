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

pragma Check_Float_Overflow;
pragma Unsuppress (Overflow_Check);
pragma Unsuppress (Range_Check);
pragma Unsuppress (Division_Check);

pragma Assertion_Policy (Ignore);
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile.

with Prunt.Motion_Planner.Trajectory.Searches.Model;

package body Prunt.Motion_Planner.Trajectory.Searches
  with SPARK_Mode => On
is

   procedure Lemma_Difference (A, B : Dimensionless)
   with
     Ghost,
     Always_Terminates,
     Pre  => B >= 0.0 and then A >= B and then A <= Bracket_Max,
     Post => A - B >= 0.0 and then A - B <= A and then A - B <= Bracket_Max;

   procedure Lemma_Difference (A, B : Dimensionless) is null;

   procedure Lemma_Midpoint (Low, High : Dimensionless)
   with
     Ghost,
     Always_Terminates,
     Pre  => Low >= 0.0 and then Low < High and then High <= Bracket_Max,
     Post =>
       High - Low >= 0.0
       and then 0.5 * (High - Low) >= 0.0
       and then 0.5 * (High - Low) <= Bracket_Max
       and then Low + 0.5 * (High - Low) >= 0.0
       and then Low + 0.5 * (High - Low) <= 2.0 * Bracket_Max;

   procedure Lemma_Midpoint (Low, High : Dimensionless) is null;

   procedure Lemma_Scaled (Factor, X : Dimensionless)
   with
     Ghost,
     Always_Terminates,
     Pre  => Factor >= 0.0 and then Factor <= 0.5 and then X >= 0.0 and then X <= Bracket_Max,
     Post => Factor * X >= 0.0 and then Factor * X <= Bracket_Max;

   procedure Lemma_Scaled (Factor, X : Dimensionless) is null;

   function Largest_Nonpositive
     (Lower, Upper                   : Dimensionless;
      Lower_Residual, Upper_Residual : Dimensionless;
      Relative_Tolerance             : Dimensionless := 0.0;
      Guess                          : Dimensionless := -1.0;
      Use_Model                      : Boolean := True) return Dimensionless
   is
      --  Nonnegative machine numbers are ordered like their bit patterns, so pattern differences count the machine
      --  numbers between two bracket ends. This relies on IEEE 754 doubles; see Machine_Numbers.

      Settled_Gap         : constant Bits := 4;
      --  A model estimate within this many machine numbers of a bracket end has converged onto that end.
      Maximum_Model_Steps : constant Positive := 48;
      First_Closing_Step  : constant Bits := 2;
      Closing_Growth      : constant Bits := 8;

      Lo      : Dimensionless := (if Lower = 0.0 then 0.0 else Lower);
      Hi      : Dimensionless := Upper;
      Near_Lo : Boolean := True;
      --  Whether the latest estimate of the result is the lower end of the bracket rather than the upper one.

      function Bracketed return Boolean
      is (Lo >= 0.0
          and then Lo < Hi
          and then Hi <= Upper
          and then Lower <= Lo
          and then To_Bits (Lo) < To_Bits (Hi)
          and then To_Bits (Hi) <= Last_Pattern
          and then (Lo = Lower or else Residual (Lo) <= 0.0)
          and then (Hi = Upper or else Residual (Hi) > 0.0)
          and then (if Lo = 0.0 then To_Bits (Lo) = 0))
      with Ghost, Pre => Upper <= Bracket_Max and then Evaluable (Lower, Upper);
      --  What the search maintains: a bracket of nonnegative machine numbers within the original one, whose lower
      --  end is the original or has a nonpositive residual and whose upper end is the original or has a positive
      --  residual. A lower end of zero is the positive zero which the search starts from.

      function Closed return Boolean
      is (To_Bits (Hi) - To_Bits (Lo) <= 1
          or else (Relative_Tolerance > 0.0 and then Hi - Lo <= Relative_Tolerance * Lo))
      with
        Pre =>
          Upper <= Bracket_Max
          and then Relative_Tolerance <= 1.0
          and then Lo >= 0.0
          and then Lo < Hi
          and then Hi <= Upper;

      procedure Evaluate (X : Dimensionless; Value : out Dimensionless)
      with
        Pre  =>
          Upper <= Bracket_Max and then Evaluable (Lower, Upper) and then Bracketed and then X > Lo and then X < Hi,
        Post =>
          Bracketed
          and then Lo >= Lo'Old
          and then Hi <= Hi'Old
          and then To_Bits (Hi) - To_Bits (Lo) < To_Bits (Hi'Old) - To_Bits (Lo'Old)
          and then (if Near_Lo then Lo = X and then Hi = Hi'Old else Hi = X and then Lo = Lo'Old)
          and then Value = Residual (X)
          and then abs Value <= Residual_Max,
        Always_Terminates;
      --  Evaluate Residual at X, strictly inside the bracket, and move the matching end there.

      procedure Evaluate (X : Dimensionless; Value : out Dimensionless) is
      begin
         Lemma_Positive_Pattern (X);
         Lemma_Order (Lo, X);
         Lemma_Order (X, Hi);
         Value := Residual (X);
         if Value <= 0.0 then
            Lo := X;
            Near_Lo := True;
         else
            Hi := X;
            Near_Lo := False;
         end if;
      end Evaluate;

      procedure Lemma_Inside (Pattern : Bits)
      with
        Ghost,
        Always_Terminates,
        Pre  =>
          Upper <= Bracket_Max
          and then Evaluable (Lower, Upper)
          and then Bracketed
          and then Pattern > To_Bits (Lo)
          and then Pattern < To_Bits (Hi),
        Post => Pattern <= Last_Pattern and then To_Real (Pattern) > Lo and then To_Real (Pattern) < Hi;
      --  A pattern strictly between those of the bracket's ends is that of a number strictly inside the bracket.

      procedure Lemma_Inside (Pattern : Bits) is
      begin
         Lemma_Order (Lo, To_Real (Pattern));
         Lemma_Order (To_Real (Pattern), Hi);
      end Lemma_Inside;

      procedure Lemma_Bounds
      with
        Ghost,
        Always_Terminates,
        Pre  => Upper <= Bracket_Max and then Evaluable (Lower, Upper) and then Bracketed,
        Post => Lo >= 0.0 and then Lo <= Bracket_Max and then Hi >= 0.0 and then Hi <= Bracket_Max and then Lo < Hi;

      procedure Lemma_Bounds is null;

      procedure Lemma_Adjacent
      with
        Ghost,
        Always_Terminates,
        Pre  =>
          Upper <= Bracket_Max
          and then Evaluable (Lower, Upper)
          and then Bracketed
          and then To_Bits (Hi) - To_Bits (Lo) <= 1,
        Post =>
          To_Bits (Lo) < Last_Pattern
          and then To_Real (To_Bits (Lo) + 1) = Hi
          and then To_Real (To_Bits (Lo) + 1) <= Upper
          and then (To_Real (To_Bits (Lo) + 1) = Upper or else Residual (To_Real (To_Bits (Lo) + 1)) > 0.0);
      --  The ends of a bracket which no machine number separates are consecutive machine numbers, so what is known
      --  of the residual at the upper end is known at the machine number after the lower end.

      procedure Lemma_Same_Residual (X, Y : Dimensionless)
      with
        Ghost,
        Always_Terminates,
        Pre  =>
          Upper <= Bracket_Max
          and then Evaluable (Lower, Upper)
          and then X = Y
          and then X > 0.0
          and then X >= Lower
          and then X <= Upper,
        Post => Residual (X) = Residual (Y);
      --  Two positive numbers which compare equal are the same number, so the residual is the same at both.

      procedure Lemma_Same_Residual (X, Y : Dimensionless) is null;

      procedure Lemma_Adjacent is
      begin
         Lemma_Round_Trip (Hi);
         pragma Assert (To_Bits (Hi) = To_Bits (Lo) + 1);
         Lemma_Same_Residual (Hi, To_Real (To_Bits (Lo) + 1));
      end Lemma_Adjacent;

      procedure Lemma_Closed
      with
        Ghost,
        Always_Terminates,
        Pre  =>
          Upper <= Bracket_Max
          and then Relative_Tolerance <= 1.0
          and then Evaluable (Lower, Upper)
          and then Bracketed
          and then Closed,
        Post =>
          To_Bits (Lo) < Last_Pattern
          and then
            (for some Pattern in Bits range To_Bits (Lo) + 1 .. Last_Pattern =>
               To_Real (Pattern) > Lo
               and then To_Real (Pattern) <= Upper
               and then (To_Real (Pattern) = Upper or else Residual (To_Real (Pattern)) > 0.0)
               and then (Pattern = To_Bits (Lo) + 1 or else Is_Close (To_Real (Pattern), Lo, Relative_Tolerance)));
      --  The upper end of a closed bracket is the machine number which the upper half of the certificate speaks
      --  of: its pattern is the witness. The pattern of the lower end is below it, which the postcondition of
      --  the search states too.

      procedure Lemma_Closed is
         Pattern : constant Bits := To_Bits (Hi);
      begin
         Lemma_Round_Trip (Hi);
         Lemma_Same_Residual (Hi, To_Real (Pattern));
         pragma Assert (To_Real (Pattern) = Hi);
         pragma Assert (To_Real (Pattern) > Lo and then To_Real (Pattern) <= Upper);
         pragma Assert (To_Real (Pattern) = Upper or else Residual (To_Real (Pattern)) > 0.0);
         pragma Assert (Pattern = To_Bits (Lo) + 1 or else Is_Close (To_Real (Pattern), Lo, Relative_Tolerance));
         pragma Assert (Pattern in To_Bits (Lo) + 1 .. Last_Pattern);
         pragma Assert (To_Bits (Lo) < Last_Pattern);
      end Lemma_Closed;

      Unused : Dimensionless;
   begin
      if not (Lo >= 0.0 and then Lo < Hi and then Hi <= Dimensionless'Last) then
         return Lower;
      end if;

      Lemma_Zero_Pattern;
      if Lo > 0.0 then
         Lemma_Positive_Pattern (Lo);
      end if;
      Lemma_Positive_Pattern (Hi);
      pragma Assert (To_Bits (Lo) <= Last_Pattern);
      Lemma_Order (Lo, Hi);
      pragma Assert (Bracketed);

      if Guess > Lo and then Guess < Hi then
         Evaluate (Guess, Unused);
      elsif Guess >= Hi then
         --  The caller's estimate is at or beyond the upper end, so the result is just below that end.
         Near_Lo := False;
      elsif Use_Model and then Lower_Residual < 0.0 then
         declare
            Target           : constant Dimensionless := -Lower_Residual;
            --  The model is anchored at the original lower end: the excess over Lower_Residual is zero there and
            --  must reach Target at the result.
            Anchor           : constant Dimensionless := Lo;
            Previous_Offset  : Dimensionless := 0.0;
            Previous_Excess  : Dimensionless := 0.0;
            Latest_Offset    : Dimensionless := Hi - Lo;
            Latest_Excess    : Dimensionless :=
              (if Upper_Residual > 0.0 and then Upper_Residual <= Dimensionless'Last - Target
               then Upper_Residual + Target
               else 0.0);
            --  Zero leaves the model without a point to fit, as for an upper residual of unknown size.
            Last_Magnitude   : Dimensionless := Dimensionless'Last;
            Exponent         : Dimensionless := 1.0;
            Poor_Steps       : Natural := 0;
            --  Consecutive model steps that failed to halve the residual; two of them force a bisection step.
            Estimate         : Dimensionless := 0.0;
            Value            : Dimensionless;
            Have_Estimate    : Boolean;
            Within_Tolerance : Boolean;
         begin
            Near_Lo := False;
            for Step in 1 .. Maximum_Model_Steps loop
               pragma Loop_Invariant (Bracketed);
               pragma Loop_Invariant (Anchor >= 0.0 and then Anchor <= Lo);
               pragma Loop_Invariant (Latest_Offset >= 0.0 and then Latest_Offset <= Bracket_Max);
               pragma Loop_Invariant (Target > 0.0);
               pragma Loop_Invariant (Poor_Steps < Step);
               exit when Closed or else To_Bits (Hi) - To_Bits (Lo) <= 2 * Settled_Gap;
               Have_Estimate := False;
               if Poor_Steps < 2 and then Latest_Excess > 0.0 and then Latest_Offset > 0.0 then
                  Model.Estimate_Result
                    (Anchor          => Anchor,
                     Target          => Target,
                     Previous_Offset => Previous_Offset,
                     Previous_Excess => Previous_Excess,
                     Latest_Offset   => Latest_Offset,
                     Latest_Excess   => Latest_Excess,
                     Exponent        => Exponent,
                     Estimate        => Estimate,
                     Have_Estimate   => Have_Estimate);
               end if;
               Within_Tolerance := False;
               if Have_Estimate and then Estimate > Lo and then Estimate < Hi then
                  Lemma_Positive_Pattern (Estimate);
                  if To_Bits (Estimate) - To_Bits (Lo) <= Settled_Gap then
                     Near_Lo := True;
                     exit;
                  elsif To_Bits (Hi) - To_Bits (Estimate) <= Settled_Gap then
                     Near_Lo := False;
                     exit;
                  end if;
                  --  An estimate that has stopped moving by more than a fraction of the tolerance only needs the
                  --  bracket closed around it.
                  Within_Tolerance :=
                    Relative_Tolerance > 0.0
                    and then abs (Estimate - (Anchor + Latest_Offset)) <= 0.25 * Relative_Tolerance * Estimate;
                  pragma Assert (Estimate > Anchor and then Estimate < Upper);
                  Evaluate (Estimate, Value);
                  pragma Assert (abs Value <= Residual_Max);
                  Poor_Steps := (if abs Value > 0.5 * Last_Magnitude then Poor_Steps + 1 else 0);
               elsif Have_Estimate and then Estimate <= Lo then
                  Near_Lo := True;
                  exit;
               elsif Have_Estimate and then Estimate >= Hi then
                  Near_Lo := False;
                  exit;
               else
                  --  Without a usable model, halve the bracket. Its ends may be many orders of magnitude apart
                  --  only when one is zero, and the closing search below handles that by machine numbers.
                  Lemma_Bounds;
                  Lemma_Midpoint (Lo, Hi);
                  pragma Assert (To_Bits (Hi) - To_Bits (Lo) > 2 * Settled_Gap);
                  pragma
                    Assert
                      ((To_Bits (Hi) - To_Bits (Lo)) / 2 >= 1
                       and then (To_Bits (Hi) - To_Bits (Lo)) / 2 < To_Bits (Hi) - To_Bits (Lo));
                  Estimate := Lo + 0.5 * (Hi - Lo);
                  if not (Estimate > Lo and then Estimate < Hi) then
                     Lemma_Inside (To_Bits (Lo) + (To_Bits (Hi) - To_Bits (Lo)) / 2);
                     Estimate := To_Real (To_Bits (Lo) + (To_Bits (Hi) - To_Bits (Lo)) / 2);
                  end if;
                  pragma Assert (Estimate > Lo and then Estimate < Hi);
                  pragma Assert (Estimate > Anchor and then Estimate < Upper);
                  Evaluate (Estimate, Value);
                  pragma Assert (abs Value <= Residual_Max);
                  Poor_Steps := 0;
               end if;
               pragma Assert (Bracketed);
               pragma Assert (abs Value <= Residual_Max);
               pragma Assert (Estimate > Anchor and then Estimate < Upper);
               Lemma_Difference (Estimate, Anchor);
               pragma Assert (Estimate - Anchor >= 0.0 and then Estimate - Anchor <= Bracket_Max);
               Last_Magnitude := abs Value;
               Previous_Offset := Latest_Offset;
               Previous_Excess := Latest_Excess;
               Latest_Offset := Estimate - Anchor;
               --  Target is finite, so only a residual already at the top of the range could overflow here, and the
               --  model then simply stops proposing estimates.
               Latest_Excess := (if Value <= Dimensionless'Last - Target then Value + Target else 0.0);
               exit when Within_Tolerance;
            end loop;
            pragma Assert (Bracketed);
         end;
      end if;
      pragma Assert (Bracketed);

      --  Close the bracket around the estimate: step away from it by a growing count of machine numbers until the
      --  residual changes sign, then bisect the machine numbers in between.
      declare
         Step : Bits := First_Closing_Step;
         Gap  : Bits;
      begin
         if Relative_Tolerance > 0.0 and then not Closed then
            --  A first step of half the tolerance closes the bracket at once when the estimate is that good.
            pragma Assert (0.5 * Relative_Tolerance >= 0.0 and then 0.5 * Relative_Tolerance <= 0.5);
            Lemma_Bounds;
            Lemma_Scaled (0.5 * Relative_Tolerance, Lo);
            Lemma_Scaled (0.5 * Relative_Tolerance, Hi);
            declare
               Probe : constant Dimensionless :=
                 (if Near_Lo then Lo + 0.5 * Relative_Tolerance * Lo else Hi - 0.5 * Relative_Tolerance * Hi);
            begin
               if Probe > Lo and then Probe < Hi then
                  Step :=
                    Bits'Max
                      (First_Closing_Step,
                       (if Near_Lo then To_Bits (Probe) - To_Bits (Lo) else To_Bits (Hi) - To_Bits (Probe)));
               end if;
            end;
         end if;
         pragma Assert (Bracketed);
         loop
            pragma Loop_Invariant (Bracketed);
            pragma Loop_Invariant (Step >= First_Closing_Step);
            pragma Loop_Variant (Decreases => To_Bits (Hi) - To_Bits (Lo));
            exit when Closed;
            Gap := To_Bits (Hi) - To_Bits (Lo);
            exit when Step >= Gap;
            if Near_Lo then
               Lemma_Inside (To_Bits (Lo) + Step);
               Evaluate (To_Real (To_Bits (Lo) + Step), Unused);
               exit when not Near_Lo;
            else
               Lemma_Inside (To_Bits (Hi) - Step);
               Evaluate (To_Real (To_Bits (Hi) - Step), Unused);
               exit when Near_Lo;
            end if;
            Step := (if Step > Bits'Last / Closing_Growth then Bits'Last else Step * Closing_Growth);
         end loop;
         loop
            pragma Loop_Invariant (Bracketed);
            pragma Loop_Variant (Decreases => To_Bits (Hi) - To_Bits (Lo));
            exit when Closed;
            pragma Assert (To_Bits (Hi) - To_Bits (Lo) >= 2);
            pragma
              Assert
                ((To_Bits (Hi) - To_Bits (Lo)) / 2 >= 1
                 and then (To_Bits (Hi) - To_Bits (Lo)) / 2 < To_Bits (Hi) - To_Bits (Lo));
            Lemma_Inside (To_Bits (Lo) + (To_Bits (Hi) - To_Bits (Lo)) / 2);
            Evaluate (To_Real (To_Bits (Lo) + (To_Bits (Hi) - To_Bits (Lo)) / 2), Unused);
         end loop;
         if not (Relative_Tolerance > 0.0) then
            Lemma_Adjacent;
         end if;
         Lemma_Closed;
      end;
      return Lo;
   end Largest_Nonpositive;

end Prunt.Motion_Planner.Trajectory.Searches;
