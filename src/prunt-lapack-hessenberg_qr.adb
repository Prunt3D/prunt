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

pragma Assertion_Policy (Ignore);

with Prunt.LAPACK.Bounds;
with Prunt.LAPACK.Elementary;   use Prunt.LAPACK.Elementary;
with Prunt.LAPACK.Norms;
with Prunt.LAPACK.Reflectors;
with Prunt.LAPACK.Schur_Blocks;

package body Prunt.LAPACK.Hessenberg_QR
  with SPARK_Mode
is

   --  The proof that nothing overflows follows one bound, Limit, on the magnitude of every element of rows and
   --  columns Ilo .. Ihi of H. A scan of the block sets it, each reflector multiplies it by Growth, and the block is
   --  scanned again before it can pass Top_Limit. Everything else which is computed is bounded from it:
   --
   --  * the deflation tests, the shifts and the search for the start of a QR step only read H, and their operands
   --    are within constants times Top_Limit; where they divide, it is by a sum of magnitudes which includes that
   --    of the numerator;
   --  * a reflector comes from a vector within one (the start of a QR step) or from elements of H, and
   --    Reflectors.Generate bounds its scalar factor and its vector by two and what it makes of the first element
   --    by four times the largest element;
   --  * applying it from one side takes elements within B to within 64 * B.
   --
   --  The subprograms which compute with floating-point numbers are small, and those which update H do little
   --  else, because every product or quotient in the context of a verification condition slows the provers down.

   Ulp : constant Real := Precision;
   --  ULP, the relative machine precision.

   Exceptional_Shift_1 : constant Real := 0.75;
   Exceptional_Shift_2 : constant Real := -0.4375;
   --  DAT1 and DAT2, the constants of the exceptional shifts.

   Exceptional_Period : constant := 10;
   --  KEXSH: an exceptional shift is tried after every this many iterations which deflate nothing.

   Scan_Limit : constant Real := 2#1.0#E800;
   --  The largest element which a scan of the active block may find. The matrix has been scaled and balanced, and
   --  orthogonal transformations do not enlarge it by more than its order, so it is in fact below 2**470.

   Step_Limit : constant Real := 2#1.0#E880;
   --  The block is scanned again when the bound on its elements passes this.

   Growth : constant Real := 4096.0;
   --  What one reflector, applied from both sides, can multiply the bound on the elements by, as far as the proof
   --  follows it: each of the two applications by 64.

   Top_Limit : constant Real := 2#1.0#E892;
   --  Step_Limit * Growth: the bound on the elements of the block is never larger than this.

   Shift_Entry_Limit : constant Real := 2#1.0#E894;
   --  The elements of the two by two matrix whose eigenvalues are the shifts are within this.

   Shift_Sum_Limit : constant Real := 2#1.0#E896;
   --  The sum of the magnitudes of those four elements is within this.

   Shift_Limit : constant Real := 2#1.0#E898;
   --  The real and imaginary parts of the shifts are within this.

   Shift_Reach : constant Real := 2#1.0#E899;
   --  The difference of an element of the block and the real part of a shift is within this.

   Start_Limit : constant Real := 2#1.0#E902;
   --  The elements of the vector which starts a QR step are within this before they are scaled.

   Maximum_Iteration_Limit : constant := 30 * Maximum_Order;
   --  The largest value of ITMAX.

   function Fits (H : Real_Matrix; Ilo, Ihi : Index) return Boolean
   is (H'First (1) = 1
       and then H'First (2) = 1
       and then Ihi <= H'Last (1)
       and then Ihi <= H'Last (2)
       and then Ilo <= Ihi)
   with Ghost;
   --  H has the rows and columns Ilo .. Ihi.

   --------------
   --  Lemmas  --
   --------------

   procedure Lemma_Widen (H : Real_Matrix; First : Positive; Last : Natural; Narrow, Wide : Real)
   with
     Ghost,
     Pre  =>
       H'First (1) = 1
       and then H'First (2) = 1
       and then Last <= H'Last (1)
       and then Last <= H'Last (2)
       and then Narrow <= Wide
       and then Block_Within (H, First, Last, Narrow),
     Post => Block_Within (H, First, Last, Wide);
   --  A block within a bound is within a larger one.

   procedure Lemma_Sum_3 (X, Y, Z, V2, V3, B : Real)
   with
     Ghost,
     Pre  =>
       B <= Top_Limit
       and then abs X <= B
       and then abs Y <= B
       and then abs Z <= B
       and then abs V2 <= 2.0
       and then abs V3 <= 2.0,
     Post => abs (X + V2 * Y + V3 * Z) <= 8.0 * B;
   --  The product of a row or a column of three elements with the vector of a reflector.

   procedure Lemma_Sum_2 (X, Y, V2, B : Real)
   with
     Ghost,
     Pre  => B <= Top_Limit and then abs X <= B and then abs Y <= B and then abs V2 <= 2.0,
     Post => abs (X + V2 * Y) <= 8.0 * B;
   --  The product of a row or a column of two elements with the vector of a reflector.

   procedure Lemma_Apply (W, S, T, B : Real)
   with
     Ghost,
     Pre  => B <= Top_Limit and then abs W <= B and then abs S <= 8.0 * B and then abs T <= 4.0,
     Post => abs (W - S * T) <= 64.0 * B;
   --  An element once a reflector is applied to it.

   procedure Lemma_Growth (B : Real)
   with Ghost, Pre => B >= 0.0 and then B <= Step_Limit, Post => 64.0 * (64.0 * B) <= B * Growth;
   --  Two applications which each multiply a bound by 64 multiply it by Growth.

   procedure Lemma_Scaled (M, X, B : Real)
   with
     Ghost,
     Pre  => abs M <= 1.0 and then abs X <= B and then B <= Bounds.Large,
     Post => abs (M * X) <= B and then abs (X * M) <= B;
   --  A factor of at most one keeps a product within the bound of the other factor.

   procedure Lemma_Start_1 (A, B, C, D, E, F : Real)
   with
     Ghost,
     Pre  =>
       abs A <= 1.0
       and then abs B <= Top_Limit
       and then abs C <= Shift_Reach
       and then abs D <= 1.0
       and then abs E <= Shift_Limit
       and then abs F <= 1.0,
     Post =>
       abs (A * B) <= Top_Limit
       and then abs (C * D) <= Shift_Reach
       and then abs (A * B + C * D) <= 2#1.0#E900
       and then abs (E * F) <= Shift_Limit
       and then abs (A * B + C * D - E * F) <= Start_Limit;
   --  The first element of the vector which starts a QR step, and what it is computed from.

   procedure Lemma_Start_2 (A, H11, H22, Rt1r, Rt2r : Real)
   with
     Ghost,
     Pre  =>
       abs A <= 1.0
       and then abs H11 <= Top_Limit
       and then abs H22 <= Top_Limit
       and then abs Rt1r <= Shift_Limit
       and then abs Rt2r <= Shift_Limit,
     Post =>
       abs (H11 + H22) <= 2#1.0#E893
       and then abs (H11 + H22 - Rt1r) <= Shift_Reach
       and then abs (H11 + H22 - Rt1r - Rt2r) <= 2#1.0#E900
       and then abs (A * (H11 + H22 - Rt1r - Rt2r)) <= 2#1.0#E900;
   --  The second element of the vector which starts a QR step, and what it is computed from.

   procedure Lemma_Widen (H : Real_Matrix; First : Positive; Last : Natural; Narrow, Wide : Real) is null;

   procedure Lemma_Sum_3 (X, Y, Z, V2, V3, B : Real) is
      P2 : constant Real := V2 * Y;
      P3 : constant Real := V3 * Z;
   begin
      Bounds.Lemma_Product (V2, Y, 2.0, B);
      Bounds.Lemma_Product (V3, Z, 2.0, B);
      pragma Assert (abs P2 <= 2.0 * B);
      pragma Assert (abs P3 <= 2.0 * B);
      pragma Assert (abs (X + P2) <= 4.0 * B);
      pragma Assert (abs (X + P2 + P3) <= 8.0 * B);
   end Lemma_Sum_3;

   procedure Lemma_Sum_2 (X, Y, V2, B : Real) is
      P2 : constant Real := V2 * Y;
   begin
      Bounds.Lemma_Product (V2, Y, 2.0, B);
      pragma Assert (abs P2 <= 2.0 * B);
      pragma Assert (abs (X + P2) <= 4.0 * B);
   end Lemma_Sum_2;

   procedure Lemma_Apply (W, S, T, B : Real) is
      P : constant Real := S * T;
   begin
      Bounds.Lemma_Product (T, S, 4.0, 8.0 * B);
      pragma Assert (abs P <= 32.0 * B);
      pragma Assert (abs (W - P) <= 64.0 * B);
   end Lemma_Apply;

   procedure Lemma_Growth (B : Real) is null;

   procedure Lemma_Scaled (M, X, B : Real) is
   begin
      Bounds.Lemma_Shrink (X, M);
      pragma Assert (abs (X * M) <= B);
      pragma Assert (M * X = X * M);
   end Lemma_Scaled;

   procedure Lemma_Start_1 (A, B, C, D, E, F : Real) is
      P1 : constant Real := A * B;
      P2 : constant Real := C * D;
      P3 : constant Real := E * F;
   begin
      Lemma_Scaled (A, B, Top_Limit);
      Lemma_Scaled (D, C, Shift_Reach);
      Lemma_Scaled (F, E, Shift_Limit);
      pragma Assert (abs P1 <= Top_Limit and then abs P2 <= Shift_Reach and then abs P3 <= Shift_Limit);
      pragma Assert (abs (P1 + P2) <= 2#1.0#E900);
      pragma Assert (abs (P1 + P2 - P3) <= Start_Limit);
   end Lemma_Start_1;

   procedure Lemma_Start_2 (A, H11, H22, Rt1r, Rt2r : Real) is
      S1 : constant Real := H11 + H22;
      S2 : constant Real := S1 - Rt1r;
      S3 : constant Real := S2 - Rt2r;
   begin
      pragma Assert (abs S1 <= 2#1.0#E893);
      pragma Assert (abs S2 <= Shift_Reach);
      pragma Assert (abs S3 <= 2#1.0#E900);
      Lemma_Scaled (A, S3, 2#1.0#E900);
   end Lemma_Start_2;

   -----------------
   --  Deflation  --
   -----------------

   function Small_Subdiagonal (Nh : Positive) return Real
   is (Safe_Minimum * (Real (Nh) / Ulp))
   with Inline, Pre => Nh <= Maximum_Order, Post => Small_Subdiagonal'Result > 0.0;
   --  SMLNUM: a subdiagonal element this small is negligible whatever its neighbours.

   function Is_Small_Beside (Sub, Super, Above, Below, Smlnum : Real) return Boolean
   with
     Inline,
     Pre =>
       abs Sub <= Top_Limit
       and then abs Super <= Top_Limit
       and then abs Above <= Top_Limit
       and then abs Below <= Top_Limit
       and then Smlnum > 0.0
       and then abs Sub > Smlnum;
   --  The conservative small subdiagonal deflation criterion due to Ahues and Tisseur (LAWN 122, 1997), for the
   --  subdiagonal element Sub = H (K, K - 1), with Super = H (K - 1, K), Above = H (K - 1, K - 1) and
   --  Below = H (K, K).

   function Is_Negligible (H : Real_Matrix; Ilo, Ihi, K : Index; Smlnum : Real) return Boolean
   with
     Inline,
     Pre =>
       Fits (H, Ilo, Ihi)
       and then K > Ilo
       and then K <= Ihi
       and then Block_Within (H, Ilo, Ihi, Top_Limit)
       and then Smlnum > 0.0
       and then abs H (K, K - 1) > Smlnum;
   --  The subdiagonal element H (K, K - 1), which is larger than Smlnum, is negligible next to its neighbours.

   procedure Find_Split (H : Real_Matrix; Ilo, Ihi, I : Index; Smlnum : Real; L : in out Index)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= L
       and then L <= I
       and then I <= Ihi
       and then Block_Within (H, Ilo, Ihi, Top_Limit)
       and then Smlnum > 0.0,
     Post => L >= L'Old and then L <= I and then (for all K in L + 1 .. I => abs H (K, K - 1) > Smlnum);
   --  Look for a single small subdiagonal element, from row I up to row L + 1: on return L is the row of the
   --  first one found, or is unchanged when there is none.

   function Is_Small_Beside (Sub, Super, Above, Below, Smlnum : Real) return Boolean is
      Ab : constant Real := Real'Max (abs Sub, abs Super);
      Ba : constant Real := Real'Min (abs Sub, abs Super);
      Aa : constant Real := Real'Max (abs Below, abs (Above - Below));
      Bb : constant Real := Real'Min (abs Below, abs (Above - Below));
      S  : constant Real := Aa + Ab;
   begin
      pragma Assert (Ab > 0.0 and then S >= Ab and then S >= Aa);
      Bounds.Lemma_Quotient_Within_One (Ab, S);
      Bounds.Lemma_Quotient_Within_One (Aa, S);
      Bounds.Lemma_Shrink (Ba, Ab / S);
      Bounds.Lemma_Shrink (Bb, Aa / S);
      return Ba * (Ab / S) <= Real'Max (Smlnum, Ulp * (Bb * (Aa / S)));
   end Is_Small_Beside;

   function Is_Negligible (H : Real_Matrix; Ilo, Ihi, K : Index; Smlnum : Real) return Boolean is
      Tst : Real;
   begin
      pragma
        Assert
          (abs H (K - 1, K - 1) <= Top_Limit
           and then abs H (K, K) <= Top_Limit
           and then abs H (K, K - 1) <= Top_Limit
           and then abs H (K - 1, K) <= Top_Limit);
      Tst := abs H (K - 1, K - 1) + abs H (K, K);
      if Tst = 0.0 then
         if K - 2 >= Ilo then
            Tst := Tst + abs H (K - 1, K - 2);
         end if;
         if K + 1 <= Ihi then
            Tst := Tst + abs H (K + 1, K);
         end if;
      end if;

      if abs H (K, K - 1) <= Ulp * Tst then
         return Is_Small_Beside (H (K, K - 1), H (K - 1, K), H (K - 1, K - 1), H (K, K), Smlnum);
      end if;
      return False;
   end Is_Negligible;

   procedure Find_Split (H : Real_Matrix; Ilo, Ihi, I : Index; Smlnum : Real; L : in out Index) is
      K : Index := I;
   begin
      while K > L loop
         pragma Loop_Invariant (K <= I);
         pragma Loop_Invariant (for all J in K + 1 .. I => abs H (J, J - 1) > Smlnum);
         pragma Loop_Variant (Decreases => K);
         exit when abs H (K, K - 1) <= Smlnum;
         exit when Is_Negligible (H, Ilo, Ihi, K, Smlnum);
         K := K - 1;
      end loop;
      L := K;
   end Find_Split;

   --------------
   --  Shifts  --
   --------------

   procedure Shift_Block
     (H : Real_Matrix; Ilo, Ihi, L, I : Index; Stalled : Positive; H11, H12, H21, H22 : out Real)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= L
       and then L <= I - 2
       and then I <= Ihi
       and then Block_Within (H, Ilo, Ihi, Top_Limit),
     Post =>
       abs H11 <= Shift_Entry_Limit
       and then abs H12 <= Shift_Entry_Limit
       and then abs H21 <= Shift_Entry_Limit
       and then abs H22 <= Shift_Entry_Limit;
   --  The two by two matrix whose eigenvalues are the shifts: the trailing block of the active submatrix, or an
   --  exceptional one after every Exceptional_Period iterations without a deflation.

   procedure Shifts_Of_Block (A11, A12, A21, A22 : Real; Rt1r, Rt1i, Rt2r, Rt2i : out Real)
   with
     Inline,
     Pre  =>
       abs A11 <= Shift_Entry_Limit
       and then abs A12 <= Shift_Entry_Limit
       and then abs A21 <= Shift_Entry_Limit
       and then abs A22 <= Shift_Entry_Limit,
     Post =>
       abs Rt1r <= Shift_Limit
       and then abs Rt1i <= Shift_Limit
       and then abs Rt2r <= Shift_Limit
       and then abs Rt2i <= Shift_Limit;
   --  The shifts (Rt1r, Rt1i) and (Rt2r, Rt2i): the eigenvalues of the two by two matrix or, when they are real,
   --  the one of them which is nearer its last element, taken twice.

   procedure Shift_Block
     (H : Real_Matrix; Ilo, Ihi, L, I : Index; Stalled : Positive; H11, H12, H21, H22 : out Real)
   is
      pragma Unreferenced (Ilo, Ihi);
      S : Real;
   begin
      pragma
        Assert
          (abs H (I, I) <= Top_Limit
           and then abs H (I - 1, I - 1) <= Top_Limit
           and then abs H (I, I - 1) <= Top_Limit
           and then abs H (I - 1, I) <= Top_Limit
           and then abs H (I - 1, I - 2) <= Top_Limit
           and then abs H (L, L) <= Top_Limit
           and then abs H (L + 1, L) <= Top_Limit
           and then abs H (L + 2, L + 1) <= Top_Limit);
      if Stalled mod (2 * Exceptional_Period) = 0 then
         --  Exceptional shift.
         S := abs H (I, I - 1) + abs H (I - 1, I - 2);
         H11 := Exceptional_Shift_1 * S + H (I, I);
         H12 := Exceptional_Shift_2 * S;
         H21 := S;
         H22 := H11;
      elsif Stalled mod Exceptional_Period = 0 then
         --  Exceptional shift.
         S := abs H (L + 1, L) + abs H (L + 2, L + 1);
         H11 := Exceptional_Shift_1 * S + H (L, L);
         H12 := Exceptional_Shift_2 * S;
         H21 := S;
         H22 := H11;
      else
         --  Prepare to use Francis' double shift (i.e. second degree generalized Rayleigh quotient).
         H11 := H (I - 1, I - 1);
         H21 := H (I, I - 1);
         H12 := H (I - 1, I);
         H22 := H (I, I);
      end if;
   end Shift_Block;

   procedure Shifts_Of_Block (A11, A12, A21, A22 : Real; Rt1r, Rt1i, Rt2r, Rt2i : out Real) is
      S : constant Real := abs A11 + abs A12 + abs A21 + abs A22;
      Det, H11, H12, H21, H22, Rtdisc, Tr : Real;

      Three : constant Real := 3.0
      with Ghost;
   begin
      if S = 0.0 then
         Rt1r := 0.0;
         Rt1i := 0.0;
         Rt2r := 0.0;
         Rt2i := 0.0;
      else
         pragma Assert (abs A11 <= S and then abs A12 <= S and then abs A21 <= S and then abs A22 <= S);
         pragma Assert (S <= Shift_Sum_Limit);
         Bounds.Lemma_Quotient_Within_One (A11, S);
         H11 := A11 / S;
         Bounds.Lemma_Quotient_Within_One (A21, S);
         H21 := A21 / S;
         Bounds.Lemma_Quotient_Within_One (A12, S);
         H12 := A12 / S;
         Bounds.Lemma_Quotient_Within_One (A22, S);
         H22 := A22 / S;
         pragma
           Assert_And_Cut
             (abs H11 <= 1.0 and then abs H12 <= 1.0 and then abs H21 <= 1.0 and then abs H22 <= 1.0);
         Tr := (H11 + H22) / 2.0;
         Det := (H11 - Tr) * (H22 - Tr) - H12 * H21;
         pragma Assert (abs Tr <= 1.0 and then abs Det <= Three * Three);
         Lemma_Sqrt_Is_Monotonic (abs Det, Three * Three);
         Lemma_Sqrt_Of_Square (Three);
         Rtdisc := Sqrt (abs Det);
         pragma Assert (Rtdisc >= 0.0 and then Rtdisc <= 3.0);
         pragma
           Assert_And_Cut
             (S >= 0.0
              and then S <= Shift_Sum_Limit
              and then abs Tr <= 1.0
              and then abs H22 <= 1.0
              and then Rtdisc >= 0.0
              and then Rtdisc <= 3.0);
         if Det >= 0.0 then
            --  Complex conjugate shifts.
            Bounds.Lemma_Product (Tr, S, 1.0, Shift_Sum_Limit);
            Rt1r := Tr * S;
            Rt2r := Rt1r;
            Bounds.Lemma_Product (Rtdisc, S, 3.0, Shift_Sum_Limit);
            Rt1i := Rtdisc * S;
            Rt2i := -Rt1i;
         else
            --  Real shifts (use only one of them).
            Rt1r := Tr + Rtdisc;
            Rt2r := Tr - Rtdisc;
            pragma Assert (abs Rt1r <= 4.0 and then abs Rt2r <= 4.0);
            if abs (Rt1r - H22) <= abs (Rt2r - H22) then
               Bounds.Lemma_Product (Rt1r, S, 4.0, Shift_Sum_Limit);
               Rt1r := Rt1r * S;
               Rt2r := Rt1r;
            else
               Bounds.Lemma_Product (Rt2r, S, 4.0, Shift_Sum_Limit);
               Rt2r := Rt2r * S;
               Rt1r := Rt2r;
            end if;
            Rt1i := 0.0;
            Rt2i := 0.0;
         end if;
      end if;
   end Shifts_Of_Block;

   --------------------------------
   --  The start of the QR step  --
   --------------------------------

   procedure Unscaled_Start
     (H11, H21, H12, H22, H32 : Real; Rt1r, Rt1i, Rt2r, Rt2i : Real; V1, V2, V3 : out Real)
   with
     Inline,
     Pre  =>
       abs H11 <= Top_Limit
       and then abs H21 <= Top_Limit
       and then abs H12 <= Top_Limit
       and then abs H22 <= Top_Limit
       and then abs H32 <= Top_Limit
       and then abs H21 > 0.0
       and then abs Rt1r <= Shift_Limit
       and then abs Rt1i <= Shift_Limit
       and then abs Rt2r <= Shift_Limit
       and then abs Rt2i <= Shift_Limit,
     Post => abs V1 <= Start_Limit and then abs V2 <= Start_Limit and then abs V3 <= Start_Limit;
   --  The first column of (H - Rt1) * (H - Rt2), divided by a scale, for the submatrix whose leading elements are
   --
   --     [ H11  H12 ]
   --     [ H21  H22 ]
   --     [  0   H32 ]

   procedure Scale_Start (V1, V2, V3 : in out Real; Nonzero : out Boolean)
   with
     Inline,
     Pre  => abs V1 <= Start_Limit and then abs V2 <= Start_Limit and then abs V3 <= Start_Limit,
     Post => (if Nonzero then abs V1 <= 1.0 and then abs V2 <= 1.0 and then abs V3 <= 1.0);
   --  Divide the vector by the sum of the magnitudes of its elements, unless it is zero.

   procedure Start_Vector
     (H                      : Real_Matrix;
      Ilo, Ihi, M            : Index;
      Rt1r, Rt1i, Rt2r, Rt2i : Real;
      V1, V2, V3             : out Real;
      Nonzero                : out Boolean)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= M
       and then M <= Ihi - 2
       and then Block_Within (H, Ilo, Ihi, Top_Limit)
       and then abs H (M + 1, M) > 0.0
       and then abs Rt1r <= Shift_Limit
       and then abs Rt1i <= Shift_Limit
       and then abs Rt2r <= Shift_Limit
       and then abs Rt2i <= Shift_Limit,
     Post => (if Nonzero then abs V1 <= 1.0 and then abs V2 <= 1.0 and then abs V3 <= 1.0);
   --  The first column of (H - Rt1) * (H - Rt2) in rows M .. M + 2 of the submatrix which starts at row M, scaled
   --  so that the magnitudes of its elements add up to one. (The following uses scaling to avoid overflows and
   --  most underflows.) Nonzero is False, and the vector is not scaled, when it is zero.

   function Starts_At (H : Real_Matrix; Ilo, Ihi, M : Index; V1, V2, V3 : Real) return Boolean
   with
     Inline,
     Pre =>
       Fits (H, Ilo, Ihi)
       and then Ilo < M
       and then M < Ihi
       and then Block_Within (H, Ilo, Ihi, Top_Limit)
       and then abs V1 <= 1.0
       and then abs V2 <= 1.0
       and then abs V3 <= 1.0;
   --  Starting the double-shift QR iteration at row M, with the vector (V1, V2, V3), makes H (M, M - 1) negligible.

   procedure Find_Start
     (H                      : Real_Matrix;
      Ilo, Ihi, L, I         : Index;
      Rt1r, Rt1i, Rt2r, Rt2i : Real;
      M                      : out Index;
      V1, V2, V3             : out Real;
      Ok                     : out Boolean)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= L
       and then L <= I - 2
       and then I <= Ihi
       and then Block_Within (H, Ilo, Ihi, Top_Limit)
       and then (for all K in L + 1 .. I => abs H (K, K - 1) > 0.0)
       and then abs Rt1r <= Shift_Limit
       and then abs Rt1i <= Shift_Limit
       and then abs Rt2r <= Shift_Limit
       and then abs Rt2i <= Shift_Limit,
     Post =>
       (if Ok
        then
          M >= L and then M <= I - 2 and then abs V1 <= 1.0 and then abs V2 <= 1.0 and then abs V3 <= 1.0);
   --  Look for two consecutive small subdiagonal elements: M is the row to start the double-shift QR step at and
   --  (V1, V2, V3) the vector which defines its first reflector. Ok is False where the reference implementation
   --  starts the step with a vector which is not a number.

   procedure Unscaled_Start
     (H11, H21, H12, H22, H32 : Real; Rt1r, Rt1i, Rt2r, Rt2i : Real; V1, V2, V3 : out Real)
   is
      H21s, S : Real;

      Q1, Q2 : Real;
      --  The two quotients in the first element of the vector.
   begin
      H21s := H21;
      S := abs (H11 - Rt2r) + abs Rt2i + abs H21s;
      pragma Assert (S >= abs (H11 - Rt2r) and then S >= abs Rt2i and then S >= abs H21 and then S > 0.0);
      Bounds.Lemma_Quotient_Within_One (H21, S);
      H21s := H21 / S;
      Bounds.Lemma_Quotient_Within_One (H11 - Rt2r, S);
      Q1 := (H11 - Rt2r) / S;
      Bounds.Lemma_Quotient_Within_One (Rt2i, S);
      Q2 := Rt2i / S;
      pragma Assert_And_Cut (abs H21s <= 1.0 and then abs Q1 <= 1.0 and then abs Q2 <= 1.0);
      Lemma_Start_1 (H21s, H12, H11 - Rt1r, Q1, Rt1i, Q2);
      V1 := H21s * H12 + (H11 - Rt1r) * Q1 - Rt1i * Q2;
      Lemma_Start_2 (H21s, H11, H22, Rt1r, Rt2r);
      V2 := H21s * (H11 + H22 - Rt1r - Rt2r);
      Lemma_Scaled (H21s, H32, Top_Limit);
      V3 := H21s * H32;
   end Unscaled_Start;

   procedure Scale_Start (V1, V2, V3 : in out Real; Nonzero : out Boolean) is
      S : constant Real := abs V1 + abs V2 + abs V3;
   begin
      if S = 0.0 then
         Nonzero := False;
      else
         pragma Assert (abs V1 <= S and then abs V2 <= S and then abs V3 <= S);
         Bounds.Lemma_Quotient_Within_One (V1, S);
         V1 := V1 / S;
         Bounds.Lemma_Quotient_Within_One (V2, S);
         V2 := V2 / S;
         Bounds.Lemma_Quotient_Within_One (V3, S);
         V3 := V3 / S;
         Nonzero := True;
      end if;
   end Scale_Start;

   procedure Start_Vector
     (H                      : Real_Matrix;
      Ilo, Ihi, M            : Index;
      Rt1r, Rt1i, Rt2r, Rt2i : Real;
      V1, V2, V3             : out Real;
      Nonzero                : out Boolean)
   is
      pragma Unreferenced (Ilo, Ihi);
   begin
      pragma
        Assert
          (abs H (M, M) <= Top_Limit
           and then abs H (M + 1, M) <= Top_Limit
           and then abs H (M, M + 1) <= Top_Limit
           and then abs H (M + 1, M + 1) <= Top_Limit
           and then abs H (M + 2, M + 1) <= Top_Limit);
      Unscaled_Start
        (H (M, M), H (M + 1, M), H (M, M + 1), H (M + 1, M + 1), H (M + 2, M + 1), Rt1r, Rt1i, Rt2r, Rt2i, V1, V2, V3);
      Scale_Start (V1, V2, V3, Nonzero);
   end Start_Vector;

   function Starts_At (H : Real_Matrix; Ilo, Ihi, M : Index; V1, V2, V3 : Real) return Boolean is
      pragma Unreferenced (Ilo, Ihi);
   begin
      pragma
        Assert
          (abs H (M, M - 1) <= Top_Limit
           and then abs H (M - 1, M - 1) <= Top_Limit
           and then abs H (M, M) <= Top_Limit
           and then abs H (M + 1, M + 1) <= Top_Limit);
      return
        abs H (M, M - 1) * (abs V2 + abs V3)
        <= Ulp * abs V1 * (abs H (M - 1, M - 1) + abs H (M, M) + abs H (M + 1, M + 1));
   end Starts_At;

   procedure Find_Start
     (H                      : Real_Matrix;
      Ilo, Ihi, L, I         : Index;
      Rt1r, Rt1i, Rt2r, Rt2i : Real;
      M                      : out Index;
      V1, V2, V3             : out Real;
      Ok                     : out Boolean)
   is
      Nonzero : Boolean;
   begin
      M := I - 2;
      loop
         pragma Loop_Invariant (M >= L and then M <= I - 2);
         pragma Loop_Variant (Decreases => M);

         --  Determine the effect of starting the double-shift QR iteration at row M, and see if this would make
         --  H (M, M - 1) negligible.
         Start_Vector (H, Ilo, Ihi, M, Rt1r, Rt1i, Rt2r, Rt2i, V1, V2, V3, Nonzero);
         if not Nonzero then
            --  The reference implementation divides zero by zero here. Its test below then fails, so that it goes
            --  on to the row above, unless this is the last row, which it starts the iteration at with a vector
            --  which is not a number.
            if M = L then
               Ok := False;
               return;
            end if;
         else
            exit when M = L;
            exit when Starts_At (H, Ilo, Ihi, M, V1, V2, V3);
         end if;
         M := M - 1;
      end loop;
      Ok := True;
   end Find_Start;

   -------------------
   --  The QR step  --
   -------------------

   procedure Make_Reflector
     (H        : Real_Matrix;
      Ilo, Ihi : Index;
      I, M, K  : Index;
      Nr       : Positive;
      V1       : in out Real;
      Tail     : in out Real_Vector;
      T1       : out Real;
      B        : Real;
      Bounded  : out Boolean)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= M
       and then M <= K
       and then K < I
       and then M <= I - 2
       and then I <= Ihi
       and then Nr = Integer'Min (3, I - K + 1)
       and then Tail'First = 1
       and then Tail'Last = 2
       and then B >= 0.0
       and then B <= Step_Limit
       and then Block_Within (H, Ilo, Ihi, B)
       and then (if K = M then abs V1 <= 1.0 and then abs Tail (1) <= 1.0 and then abs Tail (2) <= 1.0),
     Post =>
       (if Bounded
        then
          (T1 = 0.0 or else (T1 >= 1.0 and then T1 <= 2.0))
          and then abs Tail (1) <= 2.0
          and then (if Nr = 3 then abs Tail (2) <= 2.0)
          and then (if K > M then abs V1 <= 4.0 * B));
   --  Determine the reflection G of order Nr of step K of the sweep which starts at row M. The first one is that
   --  of the vector (V1, Tail), and creates a nonzero bulge below the subdiagonal. Each subsequent one is that of
   --  column K - 1, to restore the Hessenberg form in it, and thus chases the bulge one step toward the bottom of
   --  the active submatrix. On return G = I - T1 * v * v' with v = (1, Tail), and V1 is the first element of what
   --  G makes of the vector.
   --
   --  B bounds the elements of the block. Bounded is False when the reflector is not to be used.

   function Reflected (X, T1 : Real) return Real
   with
     Inline,
     Pre  => abs X <= Top_Limit and then (T1 = 0.0 or else (T1 >= 1.0 and then T1 <= 2.0)),
     Post => abs Reflected'Result <= abs X;
   --  What a reflection whose scalar factor is T1 makes of the first element X of a vector whose other elements
   --  are zero.

   procedure Apply_To_Previous_Column
     (H : in out Real_Matrix; Ilo, Ihi : Index; L, I, M, K : Index; V1, T1, B : Real)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= L
       and then L <= M
       and then M <= K
       and then K < I
       and then M <= I - 2
       and then I <= Ihi
       and then B >= 0.0
       and then B <= Step_Limit
       and then Block_Within (H, Ilo, Ihi, B)
       and then (T1 = 0.0 or else (T1 >= 1.0 and then T1 <= 2.0))
       and then (if K > M then abs V1 <= 4.0 * B),
     Post => Block_Within (H, K, I, B) and then Block_Within (H, Ilo, Ihi, 64.0 * B);
   --  Apply the reflection of step K, whose scalar factor is T1, from the left to column K - 1 of the active
   --  submatrix, which is in rows and columns L to I: after the first step it is the column the reflection is
   --  that of, and becomes (V1, 0, 0).

   procedure Apply_3_Rows (H : in out Real_Matrix; Ilo, Ihi, K, I : Index; V2, V3, T1, T2, T3, B : Real)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= K
       and then K <= I - 2
       and then I <= Ihi
       and then B >= 0.0
       and then B <= Step_Limit
       and then Block_Within (H, K, I, B)
       and then Block_Within (H, Ilo, Ihi, 64.0 * B)
       and then abs V2 <= 2.0
       and then abs V3 <= 2.0
       and then abs T1 <= 2.0
       and then abs T2 <= 4.0
       and then abs T3 <= 4.0,
     Post => Block_Within (H, Ilo, Ihi, 64.0 * B);
   --  Apply G = I - T1 * (1, V2, V3) * (1, V2, V3)' from the left to transform rows K .. K + 2 of the matrix in
   --  columns K to I.

   procedure Apply_3_Columns (H : in out Real_Matrix; Ilo, Ihi, L, K, I : Index; V2, V3, T1, T2, T3, B : Real)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= L
       and then L <= K
       and then K <= I - 2
       and then I <= Ihi
       and then B >= 0.0
       and then B <= Step_Limit
       and then Block_Within (H, Ilo, Ihi, 64.0 * B)
       and then abs V2 <= 2.0
       and then abs V3 <= 2.0
       and then abs T1 <= 2.0
       and then abs T2 <= 4.0
       and then abs T3 <= 4.0,
     Post => Block_Within (H, Ilo, Ihi, 64.0 * (64.0 * B));
   --  Apply G from the right to transform columns K .. K + 2 of the matrix in rows L to min (K + 3, I).

   procedure Apply_2_Rows (H : in out Real_Matrix; Ilo, Ihi, K, I : Index; V2, T1, T2, B : Real)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= K
       and then K = I - 1
       and then I <= Ihi
       and then B >= 0.0
       and then B <= Step_Limit
       and then Block_Within (H, K, I, B)
       and then Block_Within (H, Ilo, Ihi, 64.0 * B)
       and then abs V2 <= 2.0
       and then abs T1 <= 2.0
       and then abs T2 <= 4.0,
     Post => Block_Within (H, Ilo, Ihi, 64.0 * B);
   --  Apply G = I - T1 * (1, V2) * (1, V2)' from the left to transform rows K .. K + 1 of the matrix in columns K
   --  to I.

   procedure Apply_2_Columns (H : in out Real_Matrix; Ilo, Ihi, L, K, I : Index; V2, T1, T2, B : Real)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= L
       and then L <= K
       and then K = I - 1
       and then I <= Ihi
       and then B >= 0.0
       and then B <= Step_Limit
       and then Block_Within (H, Ilo, Ihi, 64.0 * B)
       and then abs V2 <= 2.0
       and then abs T1 <= 2.0
       and then abs T2 <= 4.0,
     Post => Block_Within (H, Ilo, Ihi, 64.0 * (64.0 * B));
   --  Apply G from the right to transform columns K .. K + 1 of the matrix in rows L to I.

   procedure QR_Step
     (H          : in out Real_Matrix;
      Ilo, Ihi   : Index;
      L, I, M, K : Index;
      V1         : in out Real;
      Tail       : in out Real_Vector;
      B          : Real;
      Bounded    : out Boolean)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= L
       and then L <= M
       and then M <= K
       and then K < I
       and then M <= I - 2
       and then I <= Ihi
       and then Tail'First = 1
       and then Tail'Last = 2
       and then B >= 0.0
       and then B <= Step_Limit
       and then Block_Within (H, Ilo, Ihi, B)
       and then (if K = M then abs V1 <= 1.0 and then abs Tail (1) <= 1.0 and then abs Tail (2) <= 1.0),
     Post => (if Bounded then Block_Within (H, Ilo, Ihi, 64.0 * (64.0 * B)));
   --  Step K of the double-shift QR sweep on rows and columns L .. I which starts at row M: determine a reflection
   --  and apply it from left and right to H.

   procedure QR_Sweep
     (H          : in out Real_Matrix;
      Ilo, Ihi   : Index;
      L, I, M    : Index;
      V1, V2, V3 : Real;
      Limit      : in out Real;
      Ok         : out Boolean)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= L
       and then L <= M
       and then M <= I - 2
       and then I <= Ihi
       and then Limit >= 0.0
       and then Limit <= Top_Limit
       and then Block_Within (H, Ilo, Ihi, Limit)
       and then abs V1 <= 1.0
       and then abs V2 <= 1.0
       and then abs V3 <= 1.0,
     Post => (if Ok then Limit >= 0.0 and then Limit <= Top_Limit and then Block_Within (H, Ilo, Ihi, Limit));
   --  The double-shift QR sweep on rows and columns L .. I which starts at row M with the vector (V1, V2, V3).
   --  Limit bounds the elements of rows and columns Ilo .. Ihi. Ok is False when a magnitude failed its test.

   procedure Make_Reflector
     (H        : Real_Matrix;
      Ilo, Ihi : Index;
      I, M, K  : Index;
      Nr       : Positive;
      V1       : in out Real;
      Tail     : in out Real_Vector;
      T1       : out Real;
      B        : Real;
      Bounded  : out Boolean)
   is
      pragma Unreferenced (Ilo, Ihi, I);
      Alpha_Before, X1_Before, X2_Before : Real
      with Ghost;
   begin
      if K > M then
         V1 := H (K, K - 1);
         Tail (1) := H (K + 1, K - 1);
         if Nr = 3 then
            Tail (2) := H (K + 2, K - 1);
         end if;
         pragma Assert (abs V1 <= B and then abs Tail (1) <= B and then (if Nr = 3 then abs Tail (2) <= B));
      end if;
      Alpha_Before := V1;
      X1_Before := Tail (1);
      X2_Before := Tail (2);
      pragma
        Assert
          (abs V1 <= Entry_Limit
           and then abs Tail (1) <= Entry_Limit
           and then (if Nr = 3 then abs Tail (2) <= Entry_Limit));
      Reflectors.Generate (Nr, V1, Tail, T1, Bounded);
      pragma
        Assert
          (if Bounded and then K > M and then Nr = 2 then Real'Max (abs Alpha_Before, abs X1_Before) <= B);
      pragma
        Assert
          (if Bounded and then K > M and then Nr = 3
             then Real'Max (abs Alpha_Before, Real'Max (abs X1_Before, abs X2_Before)) <= B);
      pragma Assert (if Bounded and then K > M then abs V1 <= 4.0 * B);
   end Make_Reflector;

   function Reflected (X, T1 : Real) return Real is
   begin
      Bounds.Lemma_Shrink (X, 1.0 - T1);
      return X * (1.0 - T1);
   end Reflected;

   procedure Apply_To_Previous_Column
     (H : in out Real_Matrix; Ilo, Ihi : Index; L, I, M, K : Index; V1, T1, B : Real)
   is
      Grown : constant Real := 64.0 * B
      with Ghost;
   begin
      pragma Assert (B <= Grown);
      pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
      pragma Assert (for all R in K .. I => (for all C in K .. I => abs H (R, C) <= B));
      if K > M then
         pragma Assert (abs V1 <= Grown);
         H (K, K - 1) := V1;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all R in K .. I => (for all C in K .. I => abs H (R, C) <= B));
         H (K + 1, K - 1) := 0.0;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all R in K .. I => (for all C in K .. I => abs H (R, C) <= B));
         if K < I - 1 then
            H (K + 2, K - 1) := 0.0;
            pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
            pragma Assert (for all R in K .. I => (for all C in K .. I => abs H (R, C) <= B));
         end if;
      elsif M > L then
         --  Use the following instead of H (K, K - 1) := -H (K, K - 1) to avoid a bug when V (2) and V (3)
         --  underflow.
         H (K, K - 1) := Reflected (H (K, K - 1), T1);
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all R in K .. I => (for all C in K .. I => abs H (R, C) <= B));
      end if;
      pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
      pragma Assert (for all R in K .. I => (for all C in K .. I => abs H (R, C) <= B));
   end Apply_To_Previous_Column;

   procedure Apply_3_Rows (H : in out Real_Matrix; Ilo, Ihi, K, I : Index; V2, V3, T1, T2, T3, B : Real) is
      Grown : constant Real := 64.0 * B
      with Ghost;
      Sum, X1, X2, X3, Y1, Y2, Y3 : Real;
   begin
      for J in K .. I loop
         pragma Loop_Invariant (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Loop_Invariant (for all C in J .. I => (for all R in K .. K + 2 => abs H (R, C) <= B));
         X1 := H (K, J);
         X2 := H (K + 1, J);
         X3 := H (K + 2, J);
         Lemma_Sum_3 (X1, X2, X3, V2, V3, B);
         Sum := X1 + V2 * X2 + V3 * X3;
         Lemma_Apply (X1, Sum, T1, B);
         Lemma_Apply (X2, Sum, T2, B);
         Lemma_Apply (X3, Sum, T3, B);
         Y1 := X1 - Sum * T1;
         Y2 := X2 - Sum * T2;
         Y3 := X3 - Sum * T3;
         pragma Assert_And_Cut (abs Y1 <= Grown and then abs Y2 <= Grown and then abs Y3 <= Grown);
         H (K, J) := Y1;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all C in J + 1 .. I => (for all R in K .. K + 2 => abs H (R, C) <= B));
         H (K + 1, J) := Y2;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all C in J + 1 .. I => (for all R in K .. K + 2 => abs H (R, C) <= B));
         H (K + 2, J) := Y3;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all C in J + 1 .. I => (for all R in K .. K + 2 => abs H (R, C) <= B));
      end loop;
      pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
   end Apply_3_Rows;

   procedure Apply_3_Columns (H : in out Real_Matrix; Ilo, Ihi, L, K, I : Index; V2, V3, T1, T2, T3, B : Real)
   is
      Entered : constant Real := 64.0 * B
      with Ghost;
      Grown   : constant Real := 64.0 * Entered
      with Ghost;
      Sum, X1, X2, X3, Y1, Y2, Y3 : Real;
   begin
      Lemma_Widen (H, Ilo, Ihi, Entered, Grown);
      for J in L .. Integer'Min (K + 3, I) loop
         pragma Loop_Invariant (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Loop_Invariant (for all R in J .. I => (for all C in K .. K + 2 => abs H (R, C) <= Entered));
         X1 := H (J, K);
         X2 := H (J, K + 1);
         X3 := H (J, K + 2);
         Lemma_Sum_3 (X1, X2, X3, V2, V3, Entered);
         Sum := X1 + V2 * X2 + V3 * X3;
         Lemma_Apply (X1, Sum, T1, Entered);
         Lemma_Apply (X2, Sum, T2, Entered);
         Lemma_Apply (X3, Sum, T3, Entered);
         Y1 := X1 - Sum * T1;
         Y2 := X2 - Sum * T2;
         Y3 := X3 - Sum * T3;
         pragma Assert_And_Cut (abs Y1 <= Grown and then abs Y2 <= Grown and then abs Y3 <= Grown);
         H (J, K) := Y1;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all R in J + 1 .. I => (for all C in K .. K + 2 => abs H (R, C) <= Entered));
         H (J, K + 1) := Y2;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all R in J + 1 .. I => (for all C in K .. K + 2 => abs H (R, C) <= Entered));
         H (J, K + 2) := Y3;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all R in J + 1 .. I => (for all C in K .. K + 2 => abs H (R, C) <= Entered));
      end loop;
      pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
   end Apply_3_Columns;

   procedure Apply_2_Rows (H : in out Real_Matrix; Ilo, Ihi, K, I : Index; V2, T1, T2, B : Real) is
      Grown : constant Real := 64.0 * B
      with Ghost;
      Sum, X1, X2, Y1, Y2 : Real;
   begin
      for J in K .. I loop
         pragma Loop_Invariant (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Loop_Invariant (for all C in J .. I => (for all R in K .. K + 1 => abs H (R, C) <= B));
         X1 := H (K, J);
         X2 := H (K + 1, J);
         Lemma_Sum_2 (X1, X2, V2, B);
         Sum := X1 + V2 * X2;
         Lemma_Apply (X1, Sum, T1, B);
         Lemma_Apply (X2, Sum, T2, B);
         Y1 := X1 - Sum * T1;
         Y2 := X2 - Sum * T2;
         pragma Assert_And_Cut (abs Y1 <= Grown and then abs Y2 <= Grown);
         H (K, J) := Y1;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all C in J + 1 .. I => (for all R in K .. K + 1 => abs H (R, C) <= B));
         H (K + 1, J) := Y2;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all C in J + 1 .. I => (for all R in K .. K + 1 => abs H (R, C) <= B));
      end loop;
      pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
   end Apply_2_Rows;

   procedure Apply_2_Columns (H : in out Real_Matrix; Ilo, Ihi, L, K, I : Index; V2, T1, T2, B : Real) is
      Entered : constant Real := 64.0 * B
      with Ghost;
      Grown   : constant Real := 64.0 * Entered
      with Ghost;
      Sum, X1, X2, Y1, Y2 : Real;
   begin
      Lemma_Widen (H, Ilo, Ihi, Entered, Grown);
      for J in L .. I loop
         pragma Loop_Invariant (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Loop_Invariant (for all R in J .. I => (for all C in K .. K + 1 => abs H (R, C) <= Entered));
         X1 := H (J, K);
         X2 := H (J, K + 1);
         Lemma_Sum_2 (X1, X2, V2, Entered);
         Sum := X1 + V2 * X2;
         Lemma_Apply (X1, Sum, T1, Entered);
         Lemma_Apply (X2, Sum, T2, Entered);
         Y1 := X1 - Sum * T1;
         Y2 := X2 - Sum * T2;
         pragma Assert_And_Cut (abs Y1 <= Grown and then abs Y2 <= Grown);
         H (J, K) := Y1;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all R in J + 1 .. I => (for all C in K .. K + 1 => abs H (R, C) <= Entered));
         H (J, K + 1) := Y2;
         pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
         pragma Assert (for all R in J + 1 .. I => (for all C in K .. K + 1 => abs H (R, C) <= Entered));
      end loop;
      pragma Assert (for all R in Ilo .. Ihi => (for all C in Ilo .. Ihi => abs H (R, C) <= Grown));
   end Apply_2_Columns;

   procedure QR_Step
     (H          : in out Real_Matrix;
      Ilo, Ihi   : Index;
      L, I, M, K : Index;
      V1         : in out Real;
      Tail       : in out Real_Vector;
      B          : Real;
      Bounded    : out Boolean)
   is
      Nr : constant Positive := Integer'Min (3, I - K + 1);
      --  The order of the reflection.

      T1, T2, T3, V2, V3 : Real;
   begin
      Make_Reflector (H, Ilo, Ihi, I, M, K, Nr, V1, Tail, T1, B, Bounded);
      if not Bounded then
         return;
      end if;
      Apply_To_Previous_Column (H, Ilo, Ihi, L, I, M, K, V1, T1, B);
      V2 := Tail (1);
      Bounds.Lemma_Product (T1, V2, 2.0, 2.0);
      T2 := T1 * V2;
      if Nr = 3 then
         V3 := Tail (2);
         Bounds.Lemma_Product (T1, V3, 2.0, 2.0);
         T3 := T1 * V3;
         Apply_3_Rows (H, Ilo, Ihi, K, I, V2, V3, T1, T2, T3, B);
         Apply_3_Columns (H, Ilo, Ihi, L, K, I, V2, V3, T1, T2, T3, B);
         pragma Assert (Block_Within (H, Ilo, Ihi, 64.0 * (64.0 * B)));
      else
         Apply_2_Rows (H, Ilo, Ihi, K, I, V2, T1, T2, B);
         Apply_2_Columns (H, Ilo, Ihi, L, K, I, V2, T1, T2, B);
         pragma Assert (Block_Within (H, Ilo, Ihi, 64.0 * (64.0 * B)));
      end if;
   end QR_Step;

   procedure QR_Sweep
     (H          : in out Real_Matrix;
      Ilo, Ihi   : Index;
      L, I, M    : Index;
      V1, V2, V3 : Real;
      Limit      : in out Real;
      Ok         : out Boolean)
   is
      Alpha : Real := V1;
      Tail  : Real_Vector (1 .. 2) := [V2, V3];
      --  The vector which defines a reflector: Alpha is V (1) and Tail is V (2 .. 3).

      Bounded : Boolean;
   begin
      Ok := True;
      for K in M .. I - 1 loop
         pragma Loop_Invariant (Limit >= 0.0 and then Limit <= Top_Limit and then Block_Within (H, Ilo, Ihi, Limit));
         pragma
           Loop_Invariant (if K = M then abs Alpha <= 1.0 and then abs Tail (1) <= 1.0 and then abs Tail (2) <= 1.0);
         pragma Loop_Invariant (Ok);
         if Limit > Step_Limit then
            Limit := Norms.Block_Max (H, Ilo, Ihi);
            if Limit > Scan_Limit then
               Ok := False;
               return;
            end if;
         end if;

         QR_Step (H, Ilo, Ihi, L, I, M, K, Alpha, Tail, Limit, Bounded);
         if not Bounded then
            Ok := False;
            return;
         end if;

         --  The bound on the elements which the proof follows through the two applications.
         Lemma_Growth (Limit);
         Lemma_Widen (H, Ilo, Ihi, 64.0 * (64.0 * Limit), Limit * Growth);
         Limit := Limit * Growth;
      end loop;
   end QR_Sweep;

   ----------------------
   --  The iterations  --
   ----------------------

   procedure QR_Iteration
     (H        : in out Real_Matrix;
      Ilo, Ihi : Index;
      L, I     : Index;
      Stalled  : Positive;
      Limit    : in out Real;
      Ok       : out Boolean)
   with
     Inline,
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= L
       and then L <= I - 2
       and then I <= Ihi
       and then Limit >= 0.0
       and then Limit <= Top_Limit
       and then Block_Within (H, Ilo, Ihi, Limit)
       and then (for all K in L + 1 .. I => abs H (K, K - 1) > 0.0),
     Post => (if Ok then Limit >= 0.0 and then Limit <= Top_Limit and then Block_Within (H, Ilo, Ihi, Limit));
   --  One QR iteration on the active submatrix, which is in rows and columns L to I. Only it is transformed, since
   --  the Schur form is not wanted. Stalled is the number of iterations since the last deflation, this one
   --  included.

   procedure QR_Iteration
     (H        : in out Real_Matrix;
      Ilo, Ihi : Index;
      L, I     : Index;
      Stalled  : Positive;
      Limit    : in out Real;
      Ok       : out Boolean)
   is
      H11, H12, H21, H22, Rt1i, Rt1r, Rt2i, Rt2r, V1, V2, V3 : Real;
      M                                                      : Index;
   begin
      Lemma_Widen (H, Ilo, Ihi, Limit, Top_Limit);
      Shift_Block (H, Ilo, Ihi, L, I, Stalled, H11, H12, H21, H22);
      Shifts_Of_Block (H11, H12, H21, H22, Rt1r, Rt1i, Rt2r, Rt2i);
      Find_Start (H, Ilo, Ihi, L, I, Rt1r, Rt1i, Rt2r, Rt2i, M, V1, V2, V3, Ok);
      if not Ok then
         return;
      end if;
      QR_Sweep (H, Ilo, Ihi, L, I, M, V1, V2, V3, Limit, Ok);
   end QR_Iteration;

   procedure Split_Off
     (H                  : in out Real_Matrix;
      Ilo, Ihi, I        : Index;
      Smlnum             : Real;
      Maximum_Iterations : Positive;
      Limit              : in out Real;
      L                  : out Index;
      Converged          : out Boolean;
      Ok                 : out Boolean)
   with
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Ilo <= I
       and then I <= Ihi
       and then Smlnum > 0.0
       and then Maximum_Iterations <= Maximum_Iteration_Limit
       and then Limit >= 0.0
       and then Limit <= Top_Limit
       and then Block_Within (H, Ilo, Ihi, Limit),
     Post =>
       (if Ok
        then
          Limit >= 0.0
          and then Limit <= Top_Limit
          and then Block_Within (H, Ilo, Ihi, Limit)
          and then L >= Ilo
          and then L <= I
          and then (if Converged then L >= I - 1));
   --  Perform QR iterations on rows and columns Ilo to I until a submatrix of order one or two splits off at the
   --  bottom: Converged, and it is in rows and columns L to I. Either L = Ilo or H (L, L - 1) is negligible so
   --  that the matrix splits. Ok is False when a magnitude failed its test.

   procedure Split_Off
     (H                  : in out Real_Matrix;
      Ilo, Ihi, I        : Index;
      Smlnum             : Real;
      Maximum_Iterations : Positive;
      Limit              : in out Real;
      L                  : out Index;
      Converged          : out Boolean;
      Ok                 : out Boolean)
   is
      Stalled : Natural := 0;
      --  KDEFL: the iterations since the last deflation.
   begin
      L := Ilo;
      Converged := False;
      Ok := True;
      for Iteration in 0 .. Maximum_Iterations loop
         pragma Loop_Invariant (Ok and then not Converged and then Stalled = Iteration);
         pragma Loop_Invariant (L >= Ilo and then L <= I);
         pragma Loop_Invariant (Limit >= 0.0 and then Limit <= Top_Limit and then Block_Within (H, Ilo, Ihi, Limit));

         --  Look for a single small subdiagonal element.
         Lemma_Widen (H, Ilo, Ihi, Limit, Top_Limit);
         Find_Split (H, Ilo, Ihi, I, Smlnum, L);

         if L > Ilo then
            --  H (L, L - 1) is negligible.
            H (L, L - 1) := 0.0;
         end if;

         --  Exit from the loop if a submatrix of order one or two has split off.
         if L >= I - 1 then
            Converged := True;
            exit;
         end if;
         Stalled := Stalled + 1;

         QR_Iteration (H, Ilo, Ihi, L, I, Stalled, Limit, Ok);
         if not Ok then
            return;
         end if;
      end loop;
   end Split_Off;

   -------------------
   --  Eigenvalues  --
   -------------------

   procedure Clear_Trash (H : in out Real_Matrix; Ilo, Ihi : Index)
   with Pre => Fits (H, Ilo, Ihi);
   --  Clear out the trash: set to zero the elements of rows and columns Ilo .. Ihi which are two or three rows
   --  below the diagonal.

   procedure Store_Converged
     (H : Real_Matrix; Ilo, Ihi, L, I : Index; Wr, Wi : in out Real_Vector; Ok : out Boolean)
   with
     Pre  =>
       Fits (H, Ilo, Ihi)
       and then Wr'First = 1
       and then Wi'First = 1
       and then Ihi <= Wr'Last
       and then Ihi <= Wi'Last
       and then Ilo <= L
       and then L <= I
       and then L >= I - 1
       and then I <= Ihi
       and then Block_Within (H, Ilo, Ihi, Top_Limit)
       and then Is_Paired (Wr, Wi, I + 1, Ihi),
     Post =>
       (if Ok then Is_Paired (Wr, Wi, L, Ihi))
       and then (for all J in Wr'Range => (if J < L or else J > I then Wr (J) = Wr'Old (J)))
       and then (for all J in Wi'Range => (if J < L or else J > I then Wi (J) = Wi'Old (J)));
   --  Store the eigenvalues of the submatrix of order one or two in rows and columns L to I, which has split off.
   --  Ok is False when a magnitude failed its test.

   procedure Clear_Trash (H : in out Real_Matrix; Ilo, Ihi : Index) is
   begin
      for J in Ilo .. Ihi - 3 loop
         pragma Loop_Invariant (True);
         H (J + 2, J) := 0.0;
         H (J + 3, J) := 0.0;
      end loop;
      if Ilo <= Ihi - 2 then
         H (Ihi, Ihi - 2) := 0.0;
      end if;
   end Clear_Trash;

   procedure Store_Converged
     (H : Real_Matrix; Ilo, Ihi, L, I : Index; Wr, Wi : in out Real_Vector; Ok : out Boolean)
   is
      pragma Unreferenced (Ilo, Ihi);
      Rt1i, Rt1r, Rt2i, Rt2r : Real;
   begin
      if L = I then
         --  H (I, I - 1) is negligible: one eigenvalue has converged.
         Wr (I) := H (I, I);
         Wi (I) := 0.0;
         Ok := True;
      else
         --  H (I - 1, I - 2) is negligible: a pair of eigenvalues have converged.
         pragma
           Assert
             (abs H (I - 1, I - 1) <= Top_Limit
              and then abs H (I - 1, I) <= Top_Limit
              and then abs H (I, I - 1) <= Top_Limit
              and then abs H (I, I) <= Top_Limit);
         Schur_Blocks.Eigenvalues_Of_Block
           (H (I - 1, I - 1), H (I - 1, I), H (I, I - 1), H (I, I), Rt1r, Rt1i, Rt2r, Rt2i, Ok);
         if not Ok then
            return;
         end if;
         Wr (I - 1) := Rt1r;
         Wi (I - 1) := Rt1i;
         Wr (I) := Rt2r;
         Wi (I) := Rt2i;
      end if;
   end Store_Converged;

   procedure Eigenvalues
     (N      : Order;
      Ilo    : Positive;
      Ihi    : Natural;
      H      : in out Real_Matrix;
      Wr, Wi : in out Real_Vector;
      Info   : out Natural;
      Ok     : out Boolean)
   is
      pragma Unreferenced (N);
      Nh : constant Positive := Ihi - Ilo + 1;

      Smlnum : constant Real := Small_Subdiagonal (Nh);

      Maximum_Iterations : constant Positive := 30 * Integer'Max (10, Nh);
      --  ITMAX: the iterations allowed for each eigenvalue.

      Limit : Real;
      --  No element of rows and columns Ilo .. Ihi of H is larger than this.

      I                  : Natural;
      L                  : Index;
      Converged, Step_Ok : Boolean;
   begin
      Info := 0;
      Ok := True;
      if Ilo = Ihi then
         Wr (Ilo) := H (Ilo, Ilo);
         Wi (Ilo) := 0.0;
         return;
      end if;

      Clear_Trash (H, Ilo, Ihi);

      Limit := Norms.Block_Max (H, Ilo, Ihi);
      if Limit > Scan_Limit then
         Ok := False;
         return;
      end if;

      --  The main loop. Eigenvalues I + 1 .. Ihi have already converged.
      I := Ihi;
      while I >= Ilo loop
         pragma Loop_Invariant (I <= Ihi and then Info = 0 and then Ok);
         pragma Loop_Invariant (Limit >= 0.0 and then Limit <= Top_Limit and then Block_Within (H, Ilo, Ihi, Limit));
         pragma Loop_Invariant (Is_Paired (Wr, Wi, I + 1, Ihi));
         pragma Loop_Invariant (for all J in Wr'Range => (if J < Ilo or else J > Ihi then Wr (J) = Wr'Loop_Entry (J)));
         pragma Loop_Invariant (for all J in Wi'Range => (if J < Ilo or else J > Ihi then Wi (J) = Wi'Loop_Entry (J)));
         pragma Loop_Variant (Decreases => I);
         Split_Off (H, Ilo, Ihi, I, Smlnum, Maximum_Iterations, Limit, L, Converged, Step_Ok);
         if not Step_Ok then
            Ok := False;
            return;
         end if;

         if not Converged then
            --  Failure to converge in the remaining number of iterations.
            Info := I;
            return;
         end if;

         Lemma_Widen (H, Ilo, Ihi, Limit, Top_Limit);
         Store_Converged (H, Ilo, Ihi, L, I, Wr, Wi, Step_Ok);
         if not Step_Ok then
            Ok := False;
            return;
         end if;

         --  Return to the start of the main loop with the new value of I.
         I := L - 1;
      end loop;
   end Eigenvalues;

end Prunt.LAPACK.Hessenberg_QR;
