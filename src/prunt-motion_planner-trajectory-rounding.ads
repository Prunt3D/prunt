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
--  Every contract and assertion of this unit is proved, so none is executed, whatever the build profile. The
--  ghost code among them computes with exact rationals and would be far too slow to run.

--  How far a floating-point value computed by a chain of rounded operations is from the exact real which the
--  chain computes, for specification and proof only.
--
--  Close (X, F, M, U, A) says that the floating-point value F is within U * M + A of the exact value X, whose
--  magnitude is at most M. X and F may have either sign. M is the magnitude of the terms which X is made of: for
--  a sum it is the sum of the magnitudes of the terms, so that a bound relative to M survives cancellation. U is
--  the relative part of the error and A the absolute part, which only a result too small to be normal gives rise
--  to.
--
--  There is one lemma for each kind of operation. It takes what is known about the operands to what is known about
--  the result: each operation adds Step to the relative part, and a product carries the absolute part of each
--  factor through a bound of the magnitude of the other factor. The caller chooses the relative and absolute
--  parts of the result, as named numbers, and the lemma's precondition checks that they are large enough.
--
--  The bounds of SPARKlib allow every rounded operation an absolute error of Eta. For a product whose result is
--  not normal that is what happens. For a sum it never is: a sum which is not normal is exact. A chain which
--  multiplied a sum's Eta by a large factor would have an absolute part far larger than the true one, so
--  Lemma_Sum_Nonnegative and the doubling lemmas, which are for sums of values which are not negative, such as
--  times, do not allow that Eta: see the body for how.

with Prunt.Motion_Planner.Reals; use Prunt.Motion_Planner.Reals;

package Prunt.Motion_Planner.Trajectory.Rounding
  with SPARK_Mode => On, Ghost, Always_Terminates
is
   use type Prunt.Motion_Planner.Reals.Real;

   function Close (X : Real; F : Dimensioned_Float; M, U, A : Real) return Boolean
   is (abs (R (F) - X) <= U * M + A and then abs X <= M);
   --  The floating-point value F is within U * M + A of the exact value X, whose magnitude is at most M.

   ------------------------------------------------------------------------------------------------------------------
   --  The limits within which the lemmas apply
   ------------------------------------------------------------------------------------------------------------------

   Step : constant := 2.0**(-49);
   --  What one rounded operation adds to the relative part of a bound: sixteen times Epsilon. A sum of values
   --  which are not negative needs ten, the others less than two.

   Slack_Max : constant := 2.0**(-40);
   --  The largest relative part which the lemmas take.

   Residue_Max : constant := 1.0;
   --  The largest absolute part which the lemmas take.

   Magnitude_Max : constant := 2.0**509;
   --  The largest magnitude which the lemmas take: a value within these limits is within Root.

   Root : constant Dimensioned_Float := 2#1.0#E511;
   --  The rounding lemmas of Prunt.Motion_Planner.Reals apply to operands within this.

   function Within_Limits (M, B, U, A : Real) return Boolean
   is (M <= B
       and then B <= Magnitude_Max
       and then U >= 0.0
       and then U <= Slack_Max
       and then A >= 0.0
       and then A <= Residue_Max);
   --  The magnitude M is at most B, and B, the relative part U and the absolute part A are within the limits
   --  above. B is a named number: it is what carries the absolute part of one factor of a product through the
   --  other.

   function In_Range (F : Dimensioned_Float) return Boolean
   is (F >= -Root and then F <= Root);

   Scaled_Magnitude_Max : constant := 2.0**506;
   --  The largest magnitude which the doubling lemmas take: an eighth of Magnitude_Max.

   Sum_Residue : constant := 2.0**(-1572);
   --  What a sum of values which are not negative adds to the absolute part of a bound.

   Scale_Residue : constant := 2.0**(-1566);
   --  The absolute part of the bound of two, four or eight times a floating-point value.

   Slack_1  : constant := 1.0 * Step;
   Slack_2  : constant := 2.0 * Step;
   Slack_3  : constant := 3.0 * Step;
   Slack_4  : constant := 4.0 * Step;
   Slack_5  : constant := 5.0 * Step;
   Slack_6  : constant := 6.0 * Step;
   Slack_7  : constant := 7.0 * Step;
   Slack_8  : constant := 8.0 * Step;
   Slack_9  : constant := 9.0 * Step;
   Slack_10 : constant := 10.0 * Step;
   Slack_11 : constant := 11.0 * Step;
   Slack_12 : constant := 12.0 * Step;
   Slack_13 : constant := 13.0 * Step;
   Slack_14 : constant := 14.0 * Step;
   Slack_15 : constant := 15.0 * Step;
   Slack_16 : constant := 16.0 * Step;
   Slack_17 : constant := 17.0 * Step;
   Slack_18 : constant := 18.0 * Step;
   Slack_19 : constant := 19.0 * Step;
   Slack_20 : constant := 20.0 * Step;
   Slack_21 : constant := 21.0 * Step;
   Slack_22 : constant := 22.0 * Step;
   Slack_23 : constant := 23.0 * Step;
   Slack_24 : constant := 24.0 * Step;
   --  The relative part of a bound after so many rounded operations.

   ------------------------------------------------------------------------------------------------------------------
   --  Floating-point values and the reals which they denote
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Order (F1, F2 : Dimensioned_Float)
   with Post => (if F1 <= F2 then R (F1) <= R (F2)) and then (if R (F1) <= R (F2) then F1 <= F2);

   procedure Lemma_Nonnegative (F : Dimensioned_Float)
   with Pre => F >= 0.0, Post => R (F) >= 0.0;

   procedure Lemma_Magnitude (F : Dimensioned_Float)
   with Post => R (abs F) = abs R (F);

   procedure Lemma_Bounded (F, Limit : Dimensioned_Float)
   with Pre => abs F <= Limit, Post => abs R (F) <= R (Limit);
   --  A bound of a floating-point value is a bound of the real which it denotes.

   procedure Lemma_Same (F1, F2 : Dimensioned_Float)
   with Pre => F1 = F2, Post => R (F1) = R (F2);
   --  Equal floating-point values denote the same real, whatever the sign of a zero.

   ------------------------------------------------------------------------------------------------------------------
   --  One rounded sum of values which are not negative, without the Eta of SPARKlib's bound
   ------------------------------------------------------------------------------------------------------------------

   Sharp : constant := 5.0 * 2.0**(-52);
   --  Ten times Epsilon.

   procedure Lemma_Sharp_Sum (F1, F2 : Dimensioned_Float)
   with
     Pre  => F1 >= 0.0 and then F1 <= Root and then F2 >= 0.0 and then F2 <= Root,
     Post => F1 + F2 >= 0.0 and then abs (R (F1 + F2) - (R (F1) + R (F2))) <= Sharp * (R (F1) + R (F2)) + Sum_Residue;
   --  The rounding of a sum of two values which are not negative, relative to the sum but for a residue which is
   --  far below Eta: ten times Epsilon is more than the relative part of the rounding.

   ------------------------------------------------------------------------------------------------------------------
   --  From the operands of an operation to its result
   ------------------------------------------------------------------------------------------------------------------

   procedure Lemma_Exact (F : Dimensioned_Float; M : Real)
   with Pre => abs R (F) <= M, Post => Close (R (F), F, M, 0.0, 0.0);
   --  A floating-point value is the real which it denotes.

   procedure Lemma_In_Range (X : Real; F : Dimensioned_Float; M, B, U, A : Real)
   with Pre => Close (X, F, M, U, A) and then Within_Limits (M, B, U, A), Post => In_Range (F);
   --  A value within the limits is within Root, so that it can be an operand of a further operation.

   procedure Lemma_Sum (X1, X2 : Real; F1, F2 : Dimensioned_Float; M1, M2, B1, B2, U1, U2, A1, A2, U, A : Real)
   with
     Pre  =>
       Close (X1, F1, M1, U1, A1)
       and then Within_Limits (M1, B1, U1, A1)
       and then Close (X2, F2, M2, U2, A2)
       and then Within_Limits (M2, B2, U2, A2)
       and then U >= U1 + Step
       and then U >= U2 + Step
       and then A >= 2.0 * (A1 + A2) + Eta,
     Post =>
       Close (X1 + X2, F1 + F2, M1 + M2, U, A)
       and then M1 + M2 <= B1 + B2
       and then (if Within_Limits (M1 + M2, B1 + B2, U, A) then In_Range (F1 + F2));
   --  A rounded sum of two values of either sign. The magnitude of the sum is the sum of the magnitudes.

   procedure Lemma_Sum_Nonnegative
     (X1, X2 : Real; F1, F2 : Dimensioned_Float; M1, M2, B1, B2, U1, U2, A1, A2, U, A : Real)
   with
     Pre  =>
       F1 >= 0.0
       and then F2 >= 0.0
       and then Close (X1, F1, M1, U1, A1)
       and then Within_Limits (M1, B1, U1, A1)
       and then Close (X2, F2, M2, U2, A2)
       and then Within_Limits (M2, B2, U2, A2)
       and then U >= U1 + Step
       and then U >= U2 + Step
       and then A >= 2.0 * (A1 + A2) + Sum_Residue,
     Post =>
       F1 + F2 >= 0.0
       and then Close (X1 + X2, F1 + F2, M1 + M2, U, A)
       and then M1 + M2 <= B1 + B2
       and then (if Within_Limits (M1 + M2, B1 + B2, U, A) then In_Range (F1 + F2));
   --  A rounded sum of two floating-point values which are not negative. Its absolute part does not include Eta.

   procedure Lemma_Double (F : Dimensioned_Float; B : Real)
   with
     Pre  => F >= 0.0 and then R (F) <= B and then B <= Scaled_Magnitude_Max,
     Post =>
       2.0 * F >= 0.0
       and then Close (2.0 * R (F), 2.0 * F, 2.0 * R (F), Slack_1, Scale_Residue)
       and then 2.0 * R (F) <= 2.0 * B
       and then In_Range (2.0 * F);

   procedure Lemma_Quadruple (F : Dimensioned_Float; B : Real)
   with
     Pre  => F >= 0.0 and then R (F) <= B and then B <= Scaled_Magnitude_Max,
     Post =>
       4.0 * F >= 0.0
       and then Close (4.0 * R (F), 4.0 * F, 4.0 * R (F), Slack_2, Scale_Residue)
       and then 4.0 * R (F) <= 4.0 * B
       and then In_Range (4.0 * F);

   procedure Lemma_Octuple (F : Dimensioned_Float; B : Real)
   with
     Pre  => F >= 0.0 and then R (F) <= B and then B <= Scaled_Magnitude_Max,
     Post =>
       8.0 * F >= 0.0
       and then Close (8.0 * R (F), 8.0 * F, 8.0 * R (F), Slack_3, Scale_Residue)
       and then 8.0 * R (F) <= 8.0 * B
       and then In_Range (8.0 * F);
   --  Two, four and eight times a floating-point value which is not negative. They are exact, but all that is
   --  proved of them is what is proved of the sums which they stand for.

   procedure Lemma_Product (X1, X2 : Real; F1, F2 : Dimensioned_Float; M1, M2, B1, B2, U1, U2, A1, A2, U, A : Real)
   with
     Pre  =>
       Close (X1, F1, M1, U1, A1)
       and then Within_Limits (M1, B1, U1, A1)
       and then Close (X2, F2, M2, U2, A2)
       and then Within_Limits (M2, B2, U2, A2)
       and then U >= U1 + U2 + Step
       and then A >= 2.0 * (A1 * B2 + A2 * B1 + A1 * A2) + Eta,
     Post =>
       Close (X1 * X2, F1 * F2, M1 * M2, U, A)
       and then M1 * M2 <= B1 * B2
       and then (if Within_Limits (M1 * M2, B1 * B2, U, A) then In_Range (F1 * F2));
   --  A rounded product of two values of either sign. B1 and B2 carry the absolute part of each factor through
   --  the other factor.

   procedure Lemma_Half (X : Real; F : Dimensioned_Float; M, B, U, A, UH, AH : Real)
   with
     Pre  => Close (X, F, M, U, A) and then Within_Limits (M, B, U, A) and then UH >= U + Step and then AH >= A + Eta,
     Post =>
       Close (X / 2.0, F / 2.0, M / 2.0, UH, AH)
       and then M / 2.0 <= B / 2.0
       and then (if Within_Limits (M / 2.0, B / 2.0, UH, AH) then In_Range (F / 2.0));
   --  Half a value of either sign. It is exact unless the result is not normal, but is taken as rounded.

   procedure Lemma_Meets (X : Real; F : Dimensioned_Float; M, U, A, Slack, Residue : Real)
   with
     Pre  => Close (X, F, M, U, A) and then U <= Slack and then A <= Residue,
     Post => abs (R (F) - X) <= Slack * M + Residue;
   --  What a bound which is within a stated relative and absolute part says.

end Prunt.Motion_Planner.Trajectory.Rounding;
