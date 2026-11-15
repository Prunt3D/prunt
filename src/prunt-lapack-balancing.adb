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
with Prunt.LAPACK.Norms; use Prunt.LAPACK.Norms;

package body Prunt.LAPACK.Balancing
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);

   Radix : constant Real := 2.0;
   --  The factor rows and columns are scaled by, SCLFAC.

   Required_Gain : constant Real := 0.95;
   --  A row and column are rescaled only when that reduces the sum of their norms below this fraction, FACTOR.

   Factor_Minimum : constant Real := 2#1.0#E-970;
   Factor_Maximum : constant Real := 2#1.0#E970;
   --  SFMIN1 = DLAMCH ('S') / DLAMCH ('P') and SFMAX1, its reciprocal: the bounds of an accumulated scale factor.

   Step_Minimum : constant Real := 2#1.0#E-969;
   Step_Maximum : constant Real := 2#1.0#E969;
   --  SFMIN2 = SFMIN1 * SCLFAC and SFMAX2, its reciprocal.

   Scaled_Limit : constant Real := 2#1.0#E971;
   --  Half of Balanced_Limit. The reference implementation stops enlarging a row or a column before its largest
   --  element reaches Step_Maximum times two, which is below this.

   Accumulated_Limit : constant Real := 2#1.0#E971;
   --  Twice Factor_Maximum. No accumulated scale factor is larger: one is only enlarged while it is below
   --  Factor_Maximum divided by what it is multiplied by, and that quotient is rounded.

   Maximum_Steps : constant := 2048;
   --  More than the number of times a scale factor can be doubled or halved between Factor_Minimum and
   --  Factor_Maximum.

   Maximum_Passes : constant := 1024;
   --  The passes of norm reduction allowed. The reference implementation repeats them until one changes nothing,
   --  without a limit. The largest number seen in 2.6 million generated matrices was 32.

   ---------------------------
   --  What the proofs use  --
   ---------------------------

   function Row_Within (A : Real_Matrix; Row, First : Positive; Last : Natural; Limit : Real) return Boolean
   is (for all Column in First .. Last => abs A (Row, Column) <= Limit)
   with Ghost, Pre => A'First (1) = 1 and then A'First (2) = 1 and then Row <= A'Last (1) and then Last <= A'Last (2);
   --  No element of columns First .. Last of a row of A is larger than Limit.

   function Column_Within (A : Real_Matrix; Column, First : Positive; Last : Natural; Limit : Real) return Boolean
   is (for all Row in First .. Last => abs A (Row, Column) <= Limit)
   with
     Ghost,
     Pre => A'First (1) = 1 and then A'First (2) = 1 and then Column <= A'Last (2) and then Last <= A'Last (1);
   --  No element of rows First .. Last of a column of A is larger than Limit.

   function Is_Factor (F : Real) return Boolean
   is (F >= Factor_Minimum and then F <= Factor_Maximum)
   with Ghost;
   --  F is in the range of what a row or a column is scaled by in one go.

   function Is_Accumulated (Factor : Real) return Boolean
   is (Factor >= 0.0 and then Factor <= Accumulated_Limit)
   with Ghost;
   --  Factor is in the range of an accumulated scale factor.

   procedure Lemma_Reciprocal (F : Real)
   with
     Ghost,
     Pre  => Is_Factor (F),
     Post => Is_Factor (1.0 / F) and then (if F > 1.0 then 1.0 / F <= 1.0);
   --  The reciprocal of a factor is in the same range, and at most one when the factor is above one.

   procedure Lemma_Guarded_Element (F, X : Real)
   with
     Ghost,
     Pre  => F > 1.0 and then F <= Factor_Maximum and then abs X <= Scaled_Limit / F,
     Post => abs (F * X) <= Balanced_Limit;
   --  An element which is within Scaled_Limit divided by what it is enlarged by stays within Balanced_Limit: the
   --  quotient and the product are each rounded, which is why the limit is twice Scaled_Limit.

   procedure Lemma_Scaled_Element (F, X : Real)
   with
     Ghost,
     Pre  =>
       F >= 0.0
       and then F <= Factor_Maximum
       and then abs X <= Balanced_Limit
       and then (if F > 1.0 then abs X <= Scaled_Limit / F),
     Post => abs (F * X) <= Balanced_Limit and then (if F <= 1.0 then abs (F * X) <= abs X);
   --  A scaled element stays within Balanced_Limit, and does not grow when the factor is at most one.

   procedure Lemma_Guarded_Factor (Factor, F : Real)
   with
     Ghost,
     Pre  => F > 1.0 and then F <= Factor_Maximum and then Factor >= 0.0 and then Factor <= Factor_Maximum / F,
     Post => Factor * F <= Accumulated_Limit;
   --  The same for an accumulated scale factor and Factor_Maximum.

   procedure Lemma_Accumulated (Factor, F : Real)
   with
     Ghost,
     Pre  =>
       Is_Accumulated (Factor)
       and then Is_Factor (F)
       and then (if F > 1.0 and then Factor > 1.0 then Factor <= Factor_Maximum / F),
     Post => Is_Accumulated (Factor * F);
   --  An accumulated scale factor stays in its range when it is only enlarged while it is below Factor_Maximum
   --  divided by what it is multiplied by.

   procedure Lemma_Guarded_Column (A : Real_Matrix; I : Index; L : Natural; C_Max, F : Real)
   with
     Ghost,
     Pre  =>
       A'First (1) = 1
       and then A'First (2) = 1
       and then I <= A'Last (2)
       and then L <= A'Last (1)
       and then Column_Within (A, I, 1, L, C_Max)
       and then (if F > 1.0 then C_Max <= Scaled_Limit / F),
     Post => (if F > 1.0 then Column_Within (A, I, 1, L, Scaled_Limit / F));
   --  What bounds the largest magnitude of a column bounds its elements.

   procedure Lemma_Guarded_Row (A : Real_Matrix; I : Index; K : Positive; N : Order; R_Max, G : Real)
   with
     Ghost,
     Pre  =>
       A'First (1) = 1
       and then A'First (2) = 1
       and then I <= A'Last (1)
       and then N <= A'Last (2)
       and then Row_Within (A, I, K, N, R_Max)
       and then (if G > 1.0 then R_Max <= Scaled_Limit / G),
     Post => (if G > 1.0 then Row_Within (A, I, K, N, Scaled_Limit / G));
   --  What bounds the largest magnitude of a row bounds its elements.

   procedure Lemma_Reciprocal (F : Real) is null;

   procedure Lemma_Guarded_Element (F, X : Real) is
      Ax : constant Real := abs X;
   begin
      pragma Assert (Ax * F <= Balanced_Limit);
      pragma Assert (abs (F * X) = Ax * F);
   end Lemma_Guarded_Element;

   procedure Lemma_Scaled_Element (F, X : Real) is
   begin
      if F > 1.0 then
         Lemma_Guarded_Element (F, X);
      else
         Bounds.Lemma_Shrink (X, F);
         pragma Assert (F * X = X * F);
      end if;
   end Lemma_Scaled_Element;

   procedure Lemma_Guarded_Factor (Factor, F : Real) is null;

   procedure Lemma_Accumulated (Factor, F : Real) is
   begin
      if F > 1.0 and then Factor > 1.0 then
         Lemma_Guarded_Factor (Factor, F);
      elsif F <= 1.0 then
         Bounds.Lemma_Shrink (Factor, F);
      else
         Bounds.Lemma_Shrink (F, Factor);
         pragma Assert (Factor * F = F * Factor);
      end if;
   end Lemma_Accumulated;

   procedure Lemma_Guarded_Column (A : Real_Matrix; I : Index; L : Natural; C_Max, F : Real) is null;

   procedure Lemma_Guarded_Row (A : Real_Matrix; I : Index; K : Positive; N : Order; R_Max, G : Real) is null;

   --------------------
   --  Permutation  --
   --------------------

   procedure Swap (N : Order; A : in out Real_Matrix; K : Positive; L : Natural; I, J : Index)
   with
     Pre  =>
       Holds (N, A)
       and then L <= N
       and then I <= L
       and then J <= L
       and then Block_Within (A, 1, N, Balanced_Limit),
     Post => Block_Within (A, 1, N, Balanced_Limit);
   --  Exchange rows and columns I and J of the part of A which is not yet isolated: rows 1 .. L of the columns
   --  and columns K .. N of the rows.

   procedure Swap (N : Order; A : in out Real_Matrix; K : Positive; L : Natural; I, J : Index) is
      Temp : Real;
   begin
      for Row in 1 .. L loop
         pragma Loop_Invariant (Block_Within (A, 1, N, Balanced_Limit));
         Temp := A (Row, I);
         A (Row, I) := A (Row, J);
         A (Row, J) := Temp;
      end loop;
      for Column in K .. N loop
         pragma Loop_Invariant (Block_Within (A, 1, N, Balanced_Limit));
         Temp := A (I, Column);
         A (I, Column) := A (J, Column);
         A (J, Column) := Temp;
      end loop;
   end Swap;

   ---------------
   --  Scaling  --
   ---------------

   function Column_Maximum (A : Real_Matrix; I : Index; L : Natural) return Real
   with
     Pre  =>
       A'First (1) = 1
       and then A'First (2) = 1
       and then I <= A'Last (2)
       and then L <= A'Last (1)
       and then Column_Within (A, I, 1, L, Balanced_Limit),
     Post =>
       Column_Maximum'Result >= 0.0
       and then Column_Maximum'Result <= Balanced_Limit
       and then Column_Within (A, I, 1, L, Column_Maximum'Result);
   --  The largest magnitude in rows 1 .. L of column I.

   function Row_Maximum (A : Real_Matrix; I : Index; K : Positive; N : Order) return Real
   with
     Pre  =>
       A'First (1) = 1
       and then A'First (2) = 1
       and then I <= A'Last (1)
       and then N <= A'Last (2)
       and then Row_Within (A, I, K, N, Balanced_Limit),
     Post =>
       Row_Maximum'Result >= 0.0
       and then Row_Maximum'Result <= Balanced_Limit
       and then Row_Within (A, I, K, N, Row_Maximum'Result);
   --  The largest magnitude in columns K .. N of row I.

   procedure Find_Factor (C_Norm, R_Norm, C_Max, R_Max : Real; F : out Real; Worthwhile : out Boolean)
   with
     Pre  =>
       C_Norm >= 0.0
       and then C_Norm <= Norm_Limit
       and then R_Norm >= 0.0
       and then R_Norm <= Norm_Limit
       and then C_Max >= 0.0
       and then C_Max <= Balanced_Limit
       and then R_Max >= 0.0
       and then R_Max <= Balanced_Limit,
     Post => Is_Factor (F);
   --  The power of two F for which the norm of a column times F and the norm of its row divided by F are
   --  closest, from the norms C_Norm and R_Norm and the largest magnitudes C_Max and R_Max of the column and of
   --  the row. Worthwhile is whether scaling by F reduces the sum of the norms by enough.

   function Accumulates (Factor, F : Real) return Boolean
   with
     Inline,
     Pre  => Is_Accumulated (Factor) and then Is_Factor (F),
     Post => (if Accumulates'Result then Is_Accumulated (Factor * F));
   --  Whether the accumulated scale factor Factor can take another factor F: not when the product could be
   --  below Factor_Minimum or beyond Factor_Maximum.

   function Stays_Bounded (F, G, C_Max, R_Max : Real) return Boolean
   is (not ((F > 1.0 and then C_Max > Scaled_Limit / F) or else (G > 1.0 and then R_Max > Scaled_Limit / G)))
   with Inline;
   --  Whether a column whose largest magnitude is C_Max can be multiplied by F, and a row whose largest magnitude
   --  is R_Max by G, with nothing passing Balanced_Limit.
   --
   --  This always holds for what Find_Factor returns and its reciprocal. C_Max times F is what Find_Factor makes
   --  of its copy of C_Max, and R_Max times G what it makes of its copy of R_Max: it doubles and halves them
   --  along with F, exactly, since it only halves what is above Step_Minimum. It only doubles what is below
   --  Step_Maximum, so the one of the two which is enlarged ends below Step_Maximum times two, which is half of
   --  Scaled_Limit. The proofs do not follow that the doublings and the halvings are exact, so Rescale tests it.

   procedure Scale_Row_And_Column
     (N : Order; A : in out Real_Matrix; K : Positive; L : Natural; I : Index; F, G : Real)
   with
     Pre  =>
       Holds (N, A)
       and then L <= N
       and then I in K .. L
       and then Is_Factor (F)
       and then Is_Factor (G)
       and then (if F > 1.0 then G <= 1.0)
       and then Block_Within (A, 1, N, Balanced_Limit)
       and then (if F > 1.0 then Column_Within (A, I, 1, L, Scaled_Limit / F))
       and then (if G > 1.0 then Row_Within (A, I, K, N, Scaled_Limit / G)),
     Post => Block_Within (A, 1, N, Balanced_Limit);
   --  Multiply columns K .. N of row I by G, then rows 1 .. L of column I by F.

   procedure Rescale
     (N       : Order;
      A       : in out Real_Matrix;
      K       : Positive;
      L       : Natural;
      I       : Index;
      Factor  : in out Real;
      Changed : in out Boolean;
      Ok      : in out Boolean)
   with
     Pre  =>
       Holds (N, A)
       and then L <= N
       and then I in K .. L
       and then Is_Accumulated (Factor)
       and then Block_Within (A, 1, N, Balanced_Limit),
     Post => Is_Accumulated (Factor) and then Block_Within (A, 1, N, Balanced_Limit);
   --  Scale row and column I of rows and columns K .. L by a power of two if that brings their norms closer, and
   --  multiply Factor, their accumulated scale factor, by it. Clear Ok instead if that could take an element
   --  beyond Balanced_Limit.

   function Column_Maximum (A : Real_Matrix; I : Index; L : Natural) return Real is
      Ca : Real := 0.0;
   begin
      for Row in 1 .. L loop
         Ca := Real'Max (Ca, abs A (Row, I));
         pragma Loop_Invariant (Ca >= 0.0 and then Ca <= Balanced_Limit);
         pragma Loop_Invariant (for all Earlier in 1 .. Row => abs A (Earlier, I) <= Ca);
      end loop;
      return Ca;
   end Column_Maximum;

   function Row_Maximum (A : Real_Matrix; I : Index; K : Positive; N : Order) return Real is
      Ra : Real := 0.0;
   begin
      for Column in K .. N loop
         Ra := Real'Max (Ra, abs A (I, Column));
         pragma Loop_Invariant (Ra >= 0.0 and then Ra <= Balanced_Limit);
         pragma Loop_Invariant (for all Earlier in K .. Column => abs A (I, Earlier) <= Ra);
      end loop;
      return Ra;
   end Row_Maximum;

   procedure Find_Factor (C_Norm, R_Norm, C_Max, R_Max : Real; F : out Real; Worthwhile : out Boolean) is
      C  : Real := C_Norm;
      R  : Real := R_Norm;
      Ca : Real := C_Max;
      Ra : Real := R_Max;
      G  : Real := R / Radix;
      S  : constant Real := C + R;
   begin
      F := 1.0;

      for Step in 1 .. Maximum_Steps loop
         pragma Loop_Invariant (F >= 1.0 and then F <= Factor_Maximum);
         pragma Loop_Invariant (C >= 0.0 and then C <= Norm_Limit);
         pragma Loop_Invariant (Ca >= 0.0 and then Ca <= Balanced_Limit);
         pragma Loop_Invariant (R >= 0.0 and then R <= Norm_Limit);
         pragma Loop_Invariant (G >= 0.0 and then G <= Norm_Limit);
         pragma Loop_Invariant (Ra >= 0.0 and then Ra <= Balanced_Limit);
         exit when
           not (C < G
                and then Real'Max (F, Real'Max (C, Ca)) < Step_Maximum
                and then Real'Min (R, Real'Min (G, Ra)) > Step_Minimum);
         F := F * Radix;
         C := C * Radix;
         Ca := Ca * Radix;
         R := R / Radix;
         G := G / Radix;
         Ra := Ra / Radix;
      end loop;

      --  What the second loop starts from, without how the first one arrived there.
      pragma
        Assert_And_Cut
          (F >= 1.0
           and then F <= Factor_Maximum
           and then C >= 0.0
           and then C <= Norm_Limit
           and then Ca >= 0.0
           and then Ca <= Balanced_Limit
           and then R >= 0.0
           and then R <= Norm_Limit
           and then Ra >= 0.0
           and then Ra <= Balanced_Limit
           and then S >= 0.0
           and then S <= 2.0 * Norm_Limit);

      G := C / Radix;

      for Step in 1 .. Maximum_Steps loop
         pragma Loop_Invariant (Is_Factor (F));
         pragma Loop_Invariant (C >= 0.0 and then C <= Norm_Limit);
         pragma Loop_Invariant (Ca >= 0.0 and then Ca <= Balanced_Limit);
         pragma Loop_Invariant (R >= 0.0 and then R <= Norm_Limit);
         pragma Loop_Invariant (G >= 0.0 and then G <= Norm_Limit);
         pragma Loop_Invariant (Ra >= 0.0 and then Ra <= Balanced_Limit);
         exit when
           not (G >= R
                and then Real'Max (R, Ra) < Step_Maximum
                and then Real'Min (Real'Min (F, C), Real'Min (G, Ca)) > Step_Minimum);
         F := F / Radix;
         C := C / Radix;
         G := G / Radix;
         Ca := Ca / Radix;
         R := R * Radix;
         Ra := Ra * Radix;
      end loop;

      --  What the comparison of the sums of the norms needs.
      pragma
        Assert_And_Cut
          (Is_Factor (F)
           and then C >= 0.0
           and then C <= Norm_Limit
           and then R >= 0.0
           and then R <= Norm_Limit
           and then S >= 0.0
           and then S <= 2.0 * Norm_Limit);

      Worthwhile := not (C + R >= Required_Gain * S);
   end Find_Factor;

   function Accumulates (Factor, F : Real) return Boolean is
   begin
      if F < 1.0 and then Factor < 1.0 then
         if F * Factor <= Factor_Minimum then
            return False;
         end if;
      end if;
      if F > 1.0 and then Factor > 1.0 then
         if Factor >= Factor_Maximum / F then
            return False;
         end if;
      end if;
      Lemma_Accumulated (Factor, F);
      return True;
   end Accumulates;

   procedure Scale_Row_And_Column
     (N : Order; A : in out Real_Matrix; K : Positive; L : Natural; I : Index; F, G : Real)
   is
      X, Y : Real;

      Row_Bound    : constant Real := (if G > 1.0 then Scaled_Limit / G else Balanced_Limit)
      with Ghost;
      Column_Bound : constant Real := (if F > 1.0 then Scaled_Limit / F else Balanced_Limit)
      with Ghost;
      --  What the elements of the row and of the column which are still to be scaled are within.
   begin
      --  The row and the column share the diagonal element, which is scaled twice. If the column is to be
      --  enlarged, G is at most one, so the first loop leaves that element within the bound of the column.
      for Column in K .. N loop
         pragma Loop_Invariant (Block_Within (A, 1, N, Balanced_Limit));
         pragma Loop_Invariant (for all Later in Column .. N => abs A (I, Later) <= Row_Bound);
         pragma Loop_Invariant (for all Row in 1 .. L => abs A (Row, I) <= Column_Bound);
         X := A (I, Column);
         Lemma_Scaled_Element (G, X);
         Y := G * X;

         --  The bounds of the new element, without the arithmetic which gives them, before it is stored.
         pragma Assert (if Column = I and then F > 1.0 then abs X <= Column_Bound);
         pragma Assert (if Column = I and then F > 1.0 then abs Y <= abs X);
         pragma Assert (if Column = I then abs Y <= Column_Bound);
         pragma
           Assert_And_Cut
             (abs Y <= Balanced_Limit
              and then (if Column = I then abs Y <= Column_Bound)
              and then Block_Within (A, 1, N, Balanced_Limit)
              and then (for all Later in Column .. N => abs A (I, Later) <= Row_Bound)
              and then (for all Row in 1 .. L => abs A (Row, I) <= Column_Bound));
         A (I, Column) := Y;
      end loop;

      --  What the second loop starts from.
      pragma
        Assert_And_Cut
          (Block_Within (A, 1, N, Balanced_Limit)
           and then (for all Row in 1 .. L => abs A (Row, I) <= Column_Bound));

      for Row in 1 .. L loop
         pragma Loop_Invariant (Block_Within (A, 1, N, Balanced_Limit));
         pragma Loop_Invariant (for all Later in Row .. L => abs A (Later, I) <= Column_Bound);
         X := A (Row, I);
         Lemma_Scaled_Element (F, X);
         Y := F * X;
         pragma
           Assert_And_Cut
             (abs Y <= Balanced_Limit
              and then Block_Within (A, 1, N, Balanced_Limit)
              and then (for all Later in Row .. L => abs A (Later, I) <= Column_Bound));
         A (Row, I) := Y;
      end loop;
   end Scale_Row_And_Column;

   procedure Rescale
     (N       : Order;
      A       : in out Real_Matrix;
      K       : Positive;
      L       : Natural;
      I       : Index;
      Factor  : in out Real;
      Changed : in out Boolean;
      Ok      : in out Boolean)
   is
      C  : constant Real := Column_Norm (A, I, K, L);
      R  : constant Real := Row_Norm (A, I, K, L);
      Ca : constant Real := Column_Maximum (A, I, L);
      Ra : constant Real := Row_Maximum (A, I, K, N);
      --  The norms of the column and of the row within the block to balance, and the largest magnitudes of all
      --  their elements which are scaled.

      F, G       : Real;
      Worthwhile : Boolean;
   begin
      --  Guard against zero C or R due to underflow.
      if C = 0.0 or else R = 0.0 then
         return;
      end if;

      Find_Factor (C, R, Ca, Ra, F, Worthwhile);

      --  Now balance.
      if not Worthwhile then
         return;
      end if;
      if not Accumulates (Factor, F) then
         return;
      end if;
      G := 1.0 / F;
      Lemma_Reciprocal (F);

      if not Stays_Bounded (F, G, Ca, Ra) then
         Ok := False;
         return;
      end if;
      Lemma_Guarded_Column (A, I, L, Ca, F);
      Lemma_Guarded_Row (A, I, K, N, Ra, G);
      Factor := Factor * F;
      Changed := True;

      Scale_Row_And_Column (N, A, K, L, I, F, G);
   end Rescale;

   ---------------
   --  Balance  --
   ---------------

   procedure Balance (N : Order; A : in out Real_Matrix; Ilo : out Positive; Ihi : out Natural; Ok : out Boolean) is

      K : Positive := 1;
      L : Natural := N;
      --  The rows and columns still to be balanced.

      Scale : Real_Vector (1 .. Maximum_Order) := [others => 1.0];
      --  The accumulated scale factor of each row and column.

      Changed  : Boolean;
      Isolated : Boolean;

   begin
      Ok := True;
      if N = 0 then
         Ilo := 1;
         Ihi := 0;
         return;
      end if;

      --  Permute to isolate eigenvalues. First search for rows which isolate an eigenvalue and push them down.
      --  Every pass but the last isolates one, so L decreases.
      Changed := True;
      while Changed loop
         pragma Loop_Invariant (L in 1 .. N and then Block_Within (A, 1, N, Balanced_Limit));
         pragma Loop_Variant (Decreases => L);
         Changed := False;
         for I in reverse 1 .. L loop
            pragma Loop_Invariant (I <= L and then L <= L'Loop_Entry and then (if Changed then L < L'Loop_Entry));
            pragma Loop_Invariant (Block_Within (A, 1, N, Balanced_Limit));
            Isolated := True;
            for J in 1 .. L loop
               pragma Loop_Invariant (Isolated);
               if I /= J and then A (I, J) /= 0.0 then
                  Isolated := False;
                  exit;
               end if;
            end loop;

            if Isolated then
               if I /= L then
                  Swap (N, A, K, L, I, L);
               end if;
               Changed := True;
               if L = 1 then
                  Ilo := 1;
                  Ihi := 1;
                  return;
               end if;
               L := L - 1;
            end if;
         end loop;
      end loop;

      --  Then search for columns which isolate an eigenvalue and push them left. Every pass but the last isolates
      --  one, so K increases.
      Changed := True;
      while Changed loop
         pragma Loop_Invariant (K <= L + 1 and then Block_Within (A, 1, N, Balanced_Limit));
         pragma Loop_Variant (Increases => K);
         Changed := False;
         for J in K .. L loop
            pragma Loop_Invariant (K <= J and then K >= K'Loop_Entry and then (if Changed then K > K'Loop_Entry));
            pragma Loop_Invariant (Block_Within (A, 1, N, Balanced_Limit));
            Isolated := True;
            for I in K .. L loop
               pragma Loop_Invariant (Isolated);
               if I /= J and then A (I, J) /= 0.0 then
                  Isolated := False;
                  exit;
               end if;
            end loop;

            if Isolated then
               if J /= K then
                  Swap (N, A, K, L, J, K);
               end if;
               Changed := True;
               K := K + 1;
            end if;
         end loop;
      end loop;

      --  Balance the submatrix in rows K to L by an iterative loop for norm reduction.
      for Pass in 1 .. Maximum_Passes loop
         pragma Loop_Invariant (Ok and then Block_Within (A, 1, N, Balanced_Limit));
         pragma Loop_Invariant (for all J in Scale'Range => Is_Accumulated (Scale (J)));
         Changed := False;
         for I in K .. L loop
            pragma Loop_Invariant (Ok and then Block_Within (A, 1, N, Balanced_Limit));
            pragma Loop_Invariant (for all J in Scale'Range => Is_Accumulated (Scale (J)));
            Rescale (N, A, K, L, I, Scale (I), Changed, Ok);
            if not Ok then
               Ilo := K;
               Ihi := L;
               return;
            end if;
         end loop;
         exit when not Changed;
      end loop;

      Ilo := K;
      Ihi := L;
   end Balance;

end Prunt.LAPACK.Balancing;
