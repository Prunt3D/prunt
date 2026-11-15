--  Part of the Prunt Motion Controller
--
--  Derived from the reference implementation of LAPACK (https://www.netlib.org/lapack/).
--
--  Copyright (c) 1992-2025 The University of Tennessee and The University
--                          of Tennessee Research Foundation.  All rights
--                          reserved.
--  Copyright (c) 2000-2025 The University of California Berkeley. All
--                          rights reserved.
--  Copyright (c) 2006-2025 The University of Colorado Denver.  All rights
--                          reserved.
--
--  $COPYRIGHT$
--
--  Additional copyrights may follow
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  $HEADER$
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are
--  met:
--
--  - Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
--  - Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer listed
--    in this license in the documentation and/or other materials
--    provided with the distribution.
--
--  - Neither the name of the copyright holders nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
--  The copyright holders provide no reassurances that the source code
--  provided does not infringe any patent, copyright, or any other
--  intellectual property rights of third parties.  The copyright holders
--  disclaim any liability to any recipient for claims brought against
--  recipient by any third party for infringement of that parties
--  intellectual property rights.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------

with Prunt.LAPACK.Bounds;
with Prunt.LAPACK.Elementary; use Prunt.LAPACK.Elementary;

package body Prunt.LAPACK.Norms
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);

   Small_Scale : constant Real := 2#1.0#E537;
   Big_Scale   : constant Real := 2#1.0#E-538;
   --  What the small and the big elements are multiplied by before they are squared.

   Zero_Sums : constant Scaled_Sums := (Big => 0.0, Medium => 0.0, Small => 0.0, Not_Big => True);
   --  The accumulators before an element is added.

   ----------------------------------
   --  Bounds of the accumulators  --
   ----------------------------------

   Big_Term    : constant Real := 2#1.0#E884 with Ghost;
   Medium_Term : constant Real := 2#1.0#E972 with Ghost;
   Small_Term  : constant Real := 2#1.0#E52 with Ghost;
   --  The scaled square of an element within Entry_Limit is within these, each in its size class.

   function Is_Tracked (Sums : Scaled_Sums) return Boolean
   is (Is_Valid (Sums) and then (Sums.Not_Big or else Sums.Big > 0.0))
   with Ghost;
   --  The accumulators are within their limits, and Not_Big is only False once a big element was added.

   function Is_Counted (Sums : Scaled_Sums; N : Natural) return Boolean
   is (Sums.Big >= 0.0
       and then Sums.Big <= Real (N) * Big_Term
       and then Sums.Medium >= 0.0
       and then Sums.Medium <= Real (N) * Medium_Term
       and then Sums.Small >= 0.0
       and then Sums.Small <= Real (N) * Small_Term)
   with Ghost, Pre => N <= Maximum_Order;
   --  The accumulators are what at most N elements within Entry_Limit sum up to.

   function Is_Step (Before, After : Scaled_Sums) return Boolean
   is (After.Big >= Before.Big
       and then After.Big <= Before.Big + Big_Term
       and then After.Medium >= Before.Medium
       and then After.Medium <= Before.Medium + Medium_Term
       and then After.Small >= Before.Small
       and then After.Small <= Before.Small + Small_Term)
   with Ghost, Pre => Is_Valid (Before);
   --  No accumulator shrinks, or grows by more than one term, from Before to After.

   procedure Lemma_Count_Big (N : Natural; S : Real)
   with
     Ghost,
     Pre  => N < Maximum_Order and then 0.0 <= S and then S <= Real (N) * Big_Term,
     Post => S + Big_Term <= Real (N + 1) * Big_Term and then Real (N + 1) * Big_Term <= Big_Sum_Limit;

   procedure Lemma_Count_Medium (N : Natural; S : Real)
   with
     Ghost,
     Pre  => N < Maximum_Order and then 0.0 <= S and then S <= Real (N) * Medium_Term,
     Post => S + Medium_Term <= Real (N + 1) * Medium_Term and then Real (N + 1) * Medium_Term <= Medium_Sum_Limit;

   procedure Lemma_Count_Small (N : Natural; S : Real)
   with
     Ghost,
     Pre  => N < Maximum_Order and then 0.0 <= S and then S <= Real (N) * Small_Term,
     Post => S + Small_Term <= Real (N + 1) * Small_Term and then Real (N + 1) * Small_Term <= Small_Sum_Limit;
   --  One more term keeps a sum of N terms within N + 1 times the bound of a term. The provers settle this for
   --  one value of N at a time.

   procedure Lemma_Count (N : Natural; Before, After : Scaled_Sums)
   with
     Ghost,
     Pre  =>
       N < Maximum_Order
       and then Is_Valid (Before)
       and then Is_Counted (Before, N)
       and then Is_Step (Before, After)
       and then (After.Not_Big or else After.Big > 0.0),
     Post => Is_Counted (After, N + 1) and then Is_Tracked (After);

   procedure Lemma_Sum_Step (S, T, Term : Real)
   with
     Ghost,
     Pre  =>
       0.0 <= S
       and then S <= Bounds.Large
       and then 0.0 <= T
       and then T <= Term
       and then Term <= Bounds.Large,
     Post => S + T >= S and then S + T <= S + Term;

   procedure Lemma_Grows (S, Term : Real)
   with
     Ghost,
     Pre  => 0.0 <= S and then S <= Bounds.Large and then 0.0 <= Term and then Term <= Bounds.Large,
     Post => S <= S + Term;
   --  Adding T to a sum does not shrink it, or enlarge it by more than a bound of T.

   procedure Lemma_Root_Within (X, Bound : Real)
   with
     Ghost,
     Pre  =>
       Bound >= Exact_Square_Minimum
       and then Bound <= Exact_Square_Maximum
       and then X >= 0.0
       and then X <= Bound * Bound,
     Post => Sqrt (X) <= Bound;
   --  The root of a number within the square of Bound is within Bound.

   procedure Lemma_Count_Big (N : Natural; S : Real) is
      pragma Unreferenced (S);
   begin
      case N is
         when 0 => null; when 1 => null; when 2 => null; when 3 => null; when 4 => null; when 5 => null;
         when 6 => null; when 7 => null; when 8 => null; when 9 => null; when 10 => null; when 11 => null;
         when 12 => null; when 13 => null; when 14 => null; when 15 => null; when 16 => null; when 17 => null;
         when 18 => null; when 19 => null; when 20 => null; when 21 => null; when 22 => null; when 23 => null;
         when 24 => null; when 25 => null; when 26 => null; when 27 => null; when 28 => null; when 29 => null;
         when 30 => null; when others => null;
      end case;
   end Lemma_Count_Big;

   procedure Lemma_Count_Medium (N : Natural; S : Real) is
      pragma Unreferenced (S);
   begin
      case N is
         when 0 => null; when 1 => null; when 2 => null; when 3 => null; when 4 => null; when 5 => null;
         when 6 => null; when 7 => null; when 8 => null; when 9 => null; when 10 => null; when 11 => null;
         when 12 => null; when 13 => null; when 14 => null; when 15 => null; when 16 => null; when 17 => null;
         when 18 => null; when 19 => null; when 20 => null; when 21 => null; when 22 => null; when 23 => null;
         when 24 => null; when 25 => null; when 26 => null; when 27 => null; when 28 => null; when 29 => null;
         when 30 => null; when others => null;
      end case;
   end Lemma_Count_Medium;

   procedure Lemma_Count_Small (N : Natural; S : Real) is
      pragma Unreferenced (S);
   begin
      case N is
         when 0 => null; when 1 => null; when 2 => null; when 3 => null; when 4 => null; when 5 => null;
         when 6 => null; when 7 => null; when 8 => null; when 9 => null; when 10 => null; when 11 => null;
         when 12 => null; when 13 => null; when 14 => null; when 15 => null; when 16 => null; when 17 => null;
         when 18 => null; when 19 => null; when 20 => null; when 21 => null; when 22 => null; when 23 => null;
         when 24 => null; when 25 => null; when 26 => null; when 27 => null; when 28 => null; when 29 => null;
         when 30 => null; when others => null;
      end case;
   end Lemma_Count_Small;

   procedure Lemma_Count (N : Natural; Before, After : Scaled_Sums) is
      pragma Unreferenced (After);
   begin
      Lemma_Count_Big (N, Before.Big);
      Lemma_Count_Medium (N, Before.Medium);
      Lemma_Count_Small (N, Before.Small);
   end Lemma_Count;

   procedure Lemma_Sum_Step (S, T, Term : Real) is null;

   procedure Lemma_Grows (S, Term : Real) is null;

   procedure Lemma_Root_Within (X, Bound : Real) is
   begin
      Lemma_Sqrt_Is_Monotonic (X, Bound * Bound);
      Lemma_Sqrt_Of_Square (Bound);
   end Lemma_Root_Within;

   -----------
   --  Add  --
   -----------

   procedure Add (Sums : in out Scaled_Sums; X : Real)
   with
     Inline,
     Pre  => abs X <= Entry_Limit and then Is_Tracked (Sums),
     Post =>
       Is_Step (Sums'Old, Sums)
       and then (Sums.Not_Big or else Sums.Big > 0.0)
       and then
         (if Is_Plain (Sums)
          then
            Is_Plain (Sums'Old)
            and then Is_Medium_Or_Zero (X)
            and then Sums.Medium >= X * X
            and then Sums.Medium = Sums.Medium'Old + X * X);
   --  Add the square of X to the accumulator of its size class.

   procedure Add (Sums : in out Scaled_Sums; X : Real) is
      Ax : constant Real := abs X;

      Before : constant Scaled_Sums := Sums
      with Ghost;
   begin
      if Ax > Big_Threshold then
         Lemma_Sum_Step (Sums.Big, (Ax * Big_Scale)**2, Big_Term);
         Sums.Big := Sums.Big + (Ax * Big_Scale)**2;
         Sums.Not_Big := False;
         pragma Assert (Sums.Big > 0.0);
      elsif Ax < Small_Threshold then
         if Sums.Not_Big then
            Lemma_Sum_Step (Sums.Small, (Ax * Small_Scale)**2, Small_Term);
            Sums.Small := Sums.Small + (Ax * Small_Scale)**2;
            pragma Assert (if X /= 0.0 then Sums.Small > 0.0);
         end if;
         pragma Assert (if X = 0.0 then Sums.Medium = Before.Medium + X * X);
      else
         Lemma_Sum_Step (Sums.Medium, Ax**2, Medium_Term);
         Sums.Medium := Sums.Medium + Ax**2;
         pragma Assert (Ax * Ax = X * X);
         pragma Assert (Sums.Medium >= X * X);
      end if;
      Lemma_Grows (Before.Big, Big_Term);
      Lemma_Grows (Before.Medium, Medium_Term);
      Lemma_Grows (Before.Small, Small_Term);
      pragma Assert (Sums.Big >= Before.Big);
      pragma Assert (Sums.Big <= Before.Big + Big_Term);
      pragma Assert (Sums.Medium >= Before.Medium);
      pragma Assert (Sums.Medium <= Before.Medium + Medium_Term);
      pragma Assert (Sums.Small >= Before.Small);
      pragma Assert (Sums.Small <= Before.Small + Small_Term);
   end Add;

   ------------
   --  Root  --
   ------------

   function Root (Sums : Scaled_Sums) return Real is
      Scale, Sum_Of_Squares : Real;
   begin
      if Sums.Big > 0.0 then
         --  Combine the big and medium accumulators.
         if Sums.Medium > 0.0 then
            Sum_Of_Squares := Sums.Big + (Sums.Medium * Big_Scale) * Big_Scale;
         else
            Sum_Of_Squares := Sums.Big;
         end if;
         Scale := 1.0 / Big_Scale;
         pragma Assert (Sum_Of_Squares >= 0.0 and then Sum_Of_Squares <= 2#1.0#E890);
         Lemma_Root_Within (Sum_Of_Squares, 2#1.0#E445);
         pragma Assert (Scale >= 0.0 and then Scale <= 2#1.0#E538);
      elsif Sums.Small > 0.0 then
         --  Combine the medium and small accumulators.
         if Sums.Medium > 0.0 then
            declare
               Medium     : constant Real := Sqrt (Sums.Medium);
               Small      : constant Real := Sqrt (Sums.Small) / Small_Scale;
               Ymin, Ymax : Real;
            begin
               Lemma_Root_Within (Sums.Medium, 2#1.0#E489);
               Lemma_Root_Within (Sums.Small, 2#1.0#E29);
               pragma Assert (Medium > 0.0 and then Medium <= 2#1.0#E489);
               pragma Assert (Small >= 0.0 and then Small <= 2#1.0#E489);
               if Small > Medium then
                  Ymin := Medium;
                  Ymax := Small;
               else
                  Ymin := Small;
                  Ymax := Medium;
               end if;
               Scale := 1.0;
               Bounds.Lemma_Quotient_Within_One (Ymin, Ymax);
               pragma Assert (abs (Ymin / Ymax) <= 1.0);
               Sum_Of_Squares := Ymax**2 * (1.0 + (Ymin / Ymax)**2);
               pragma Assert (Sum_Of_Squares >= 0.0 and then Sum_Of_Squares <= 2#1.0#E980);
               Lemma_Root_Within (Sum_Of_Squares, 2#1.0#E490);
            end;
         else
            Scale := 1.0 / Small_Scale;
            Sum_Of_Squares := Sums.Small;
            Lemma_Root_Within (Sum_Of_Squares, 2#1.0#E29);
            pragma Assert (Scale >= 0.0 and then Scale <= 1.0);
         end if;
      else
         --  Every element is of medium size, or zero.
         Scale := 1.0;
         Sum_Of_Squares := Sums.Medium;
         Lemma_Root_Within (Sum_Of_Squares, 2#1.0#E489);
      end if;
      pragma Assert (Sum_Of_Squares >= 0.0);
      pragma Assert (Scale >= 0.0 and then Scale <= 2#1.0#E538);
      pragma Assert (Sqrt (Sum_Of_Squares) <= 2#1.0#E490);
      pragma Assert (if Scale > 1.0 then Sqrt (Sum_Of_Squares) <= 2#1.0#E445);
      return Scale * Sqrt (Sum_Of_Squares);
   end Root;

   -----------------
   --  Block_Max  --
   -----------------

   function Block_Max (A : Real_Matrix; First : Positive; Last : Natural) return Real is
      Value : Real := 0.0;
   begin
      for I in First .. Last loop
         pragma Loop_Invariant (Value >= 0.0);
         pragma Loop_Invariant (for all R in First .. I - 1 => (for all C in First .. Last => abs A (R, C) <= Value));
         for J in First .. Last loop
            pragma Loop_Invariant (Value >= 0.0);
            pragma
              Loop_Invariant (for all R in First .. I - 1 => (for all C in First .. Last => abs A (R, C) <= Value));
            pragma Loop_Invariant (for all C in First .. J - 1 => abs A (I, C) <= Value);
            if Value < abs A (I, J) then
               Value := abs A (I, J);
            end if;
         end loop;
      end loop;
      return Value;
   end Block_Max;

   ------------------
   --  Hypotenuse  --
   ------------------

   function Hypotenuse (X, Y : Real) return Real is
      W : constant Real := Real'Max (abs X, abs Y);
      Z : constant Real := Real'Min (abs X, abs Y);

      Ratio, Sum, Factor, Result : Real;
   begin
      if Z = 0.0 then
         Result := W;
      else
         Bounds.Lemma_Quotient_Within_One (Z, W);
         Ratio := Z / W;
         pragma Assert (Ratio >= 0.0 and then Ratio <= 1.0);
         Sum := 1.0 + Ratio**2;
         pragma Assert (Sum >= 1.0 and then Sum <= 2.0);

         --  The root of a number between one and four is between one and two.
         Lemma_Sqrt_Of_Square (1.0);
         Lemma_Sqrt_Is_Monotonic (1.0 * 1.0, Sum);
         Lemma_Sqrt_Of_Square (2.0);
         Lemma_Sqrt_Is_Monotonic (Sum, 2.0 * 2.0);
         Factor := Sqrt (Sum);
         pragma Assert (Factor >= 1.0 and then Factor <= 2.0);

         Bounds.Lemma_Mult_Is_Monotonic (1.0, Factor, W);
         Bounds.Lemma_Mult_Is_Monotonic (Factor, 2.0, W);
         Result := W * Factor;
         pragma Assert (Result >= W and then Result <= 2.0 * W);
      end if;
      return Result;
   end Hypotenuse;

   ----------------------
   --  Sum_Of_Squares  --
   ----------------------

   function Sum_Of_Squares (X : Real_Vector; Count : Natural) return Scaled_Sums is
      Sums : Scaled_Sums := Zero_Sums;
   begin
      for I in 1 .. Count loop
         pragma Loop_Invariant (Is_Counted (Sums, I - 1) and then Is_Tracked (Sums));
         pragma Loop_Invariant (if I = 1 then Sums.Medium = 0.0);
         pragma
           Loop_Invariant
             (if Is_Plain (Sums)
                then
                  (for all J in 1 .. I - 1 => Is_Medium_Or_Zero (X (J)) and then Sums.Medium >= X (J) * X (J))
                  and then (if I = 2 then Sums.Medium <= X (1) * X (1)));
         declare
            Before : constant Scaled_Sums := Sums
            with Ghost;
         begin
            Add (Sums, X (I));
            Lemma_Count (I - 1, Before, Sums);
            pragma
              Assert
                (if Is_Plain (Sums)
                   then
                     (for all J in 1 .. I => Is_Medium_Or_Zero (X (J)) and then Sums.Medium >= X (J) * X (J)));
            pragma Assert (if Is_Plain (Sums) and then I = 1 then Sums.Medium <= X (1) * X (1));
            pragma Assert (if Is_Plain (Sums) and then I = 2 then Sums.Medium <= X (1) * X (1) + X (2) * X (2));
         end;
      end loop;
      return Sums;
   end Sum_Of_Squares;

   -------------------
   --  Column_Norm  --
   -------------------

   function Column_Norm (A : Real_Matrix; Column : Positive; First : Positive; Last : Natural) return Real is
      Sums : Scaled_Sums := Zero_Sums;
   begin
      for I in First .. Last loop
         pragma Loop_Invariant (Is_Counted (Sums, I - First) and then Is_Tracked (Sums));
         declare
            Before : constant Scaled_Sums := Sums
            with Ghost;
         begin
            Add (Sums, A (I, Column));
            Lemma_Count (I - First, Before, Sums);
         end;
      end loop;
      return Root (Sums);
   end Column_Norm;

   ----------------
   --  Row_Norm  --
   ----------------

   function Row_Norm (A : Real_Matrix; Row : Positive; First : Positive; Last : Natural) return Real is
      Sums : Scaled_Sums := Zero_Sums;
   begin
      for J in First .. Last loop
         pragma Loop_Invariant (Is_Counted (Sums, J - First) and then Is_Tracked (Sums));
         declare
            Before : constant Scaled_Sums := Sums
            with Ghost;
         begin
            Add (Sums, A (Row, J));
            Lemma_Count (J - First, Before, Sums);
         end;
      end loop;
      return Root (Sums);
   end Row_Norm;

end Prunt.LAPACK.Norms;
