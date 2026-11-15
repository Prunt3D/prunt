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

with Prunt.LAPACK.Balancing;
with Prunt.LAPACK.Hessenberg_QR;
with Prunt.LAPACK.Hessenberg_Reduction;
with Prunt.LAPACK.Norms;

package body Prunt.LAPACK.Eigenvalues
  with SPARK_Mode
is

   Smlnum : constant Real := 2#1.0#E-459;
   Bignum : constant Real := 2#1.0#E459;
   --  sqrt (DLAMCH ('S')) / DLAMCH ('P') and its reciprocal. A matrix whose largest element is outside this range
   --  is scaled into it.

   Unscaled_Limit : constant Real := 2#1.0#E1022;
   --  An eigenvalue is scaled back up only when the result is within twice this.

   Scaled_Up_Limit : constant Real := 2#1.0#E1000;
   --  An eigenvalue of a matrix which was scaled up is far below this.

   function Is_Scaling (Anrm, Cscale : Real) return Boolean
   is ((Anrm > 0.0 and then Anrm < Smlnum and then Cscale = Smlnum) or else (Anrm > Bignum and then Cscale = Bignum));
   --  Cscale is what DGEEV scales a matrix whose largest element is Anrm to.

   procedure Lemma_Scaled_Element (X, Anrm, Cscale : Real)
   with
     Ghost,
     Pre  => Is_Scaling (Anrm, Cscale) and then abs X <= Anrm,
     Post => abs (X * (Cscale / Anrm)) <= 2.0 * Cscale;
   --  Scaling an element of a matrix by the quotient of the new largest element and the old one leaves it within
   --  twice the new one.

   procedure Scale_Matrix (N : Order; A : in out Real_Matrix; Anrm, Cscale : Real)
   with
     Pre  => N >= 1 and then Holds (N, A) and then Is_Scaling (Anrm, Cscale) and then Block_Within (A, 1, N, Anrm),
     Post => Block_Within (A, 1, N, Balanced_Limit);
   --  DLASCL ('G', 0, 0, Anrm, Cscale, N, N, A): multiply A by Cscale / Anrm.

   Largest_Factor : constant Real := 2#1.0#E570;
   --  More than the largest element of a matrix can be divided by Bignum.

   function Unscaling_Bound (Mul : Real) return Real
   is (if Mul > 1.0 then Unscaled_Limit / Mul else Scaled_Up_Limit)
   with Pre => Mul > 0.0 and then Mul <= Largest_Factor;
   --  An eigenvalue within this can be multiplied by Mul.

   Unscaled_Maximum : constant Real := 2#1.0#E1023;

   procedure Lemma_Unscaled (X, Mul : Real)
   with
     Ghost,
     Pre  => Mul > 0.0 and then Mul <= Largest_Factor and then abs X <= Unscaling_Bound (Mul),
     Post => abs (X * Mul) <= Unscaled_Maximum;

   procedure Lemma_Scaled_Pair (R1, I1, R2, I2, Mul : Real)
   with
     Ghost,
     Pre  =>
       Mul > 0.0
       and then Mul <= Largest_Factor
       and then abs R1 <= Unscaling_Bound (Mul)
       and then abs I1 <= Unscaling_Bound (Mul)
       and then I1 >= 0.0
       and then R2 = R1
       and then I2 = -I1,
     Post => I1 * Mul >= 0.0 and then R2 * Mul = R1 * Mul and then I2 * Mul = -(I1 * Mul);
   --  A conjugate pair scaled by a positive factor is a conjugate pair, or two equal real numbers when the
   --  imaginary part underflows.

   function Scaled (X, Mul : Real) return Real
   with
     Ghost,
     Pre  => Mul > 0.0 and then Mul <= Largest_Factor and then abs X <= Unscaling_Bound (Mul),
     Post => Scaled'Result = X * Mul and then abs Scaled'Result <= Unscaled_Maximum;
   --  X times Mul, as a function, so that facts about every element of a vector need no product.

   function Is_Scaled (Old, Now : Real_Vector; N : Order; Mul : Real) return Boolean
   is (for all J in 1 .. N => Now (J) = Scaled (Old (J), Mul))
   with
     Ghost,
     Pre =>
       Holds (N, Old)
       and then Holds (N, Now)
       and then Mul > 0.0
       and then Mul <= Largest_Factor
       and then (for all J in 1 .. N => abs Old (J) <= Unscaling_Bound (Mul));

   function Is_Paired_At (Wr, Wi : Real_Vector; N : Order; J : Positive) return Boolean
   is (Wi (J) = 0.0
       or else (Wi (J) > 0.0 and then J < N and then Wr (J + 1) = Wr (J) and then Wi (J + 1) = -Wi (J))
       or else (Wi (J) < 0.0 and then J > 1 and then Wr (J - 1) = Wr (J) and then Wi (J - 1) = -Wi (J)))
   with Ghost, Pre => Holds (N, Wr) and then Holds (N, Wi) and then J <= N;
   --  What Is_Paired (Wr, Wi, 1, N) says of eigenvalue J.

   procedure Lemma_Scaled_Pair_At (Wr_Old, Wi_Old, Wr, Wi : Real_Vector; N : Order; Mul : Real; J : Positive)
   with
     Ghost,
     Pre  =>
       Holds (N, Wr_Old)
       and then Holds (N, Wi_Old)
       and then Holds (N, Wr)
       and then Holds (N, Wi)
       and then J <= N
       and then Mul > 0.0
       and then Mul <= Largest_Factor
       and then (for all K in 1 .. N => abs Wr_Old (K) <= Unscaling_Bound (Mul))
       and then (for all K in 1 .. N => abs Wi_Old (K) <= Unscaling_Bound (Mul))
       and then Is_Paired_At (Wr_Old, Wi_Old, N, J)
       and then Is_Scaled (Wr_Old, Wr, N, Mul)
       and then Is_Scaled (Wi_Old, Wi, N, Mul),
     Post => Is_Paired_At (Wr, Wi, N, J);

   procedure Lemma_Scaled_Pairs (Wr_Old, Wi_Old, Wr, Wi : Real_Vector; N : Order; Mul : Real)
   with
     Ghost,
     Pre  =>
       Holds (N, Wr_Old)
       and then Holds (N, Wi_Old)
       and then Holds (N, Wr)
       and then Holds (N, Wi)
       and then Mul > 0.0
       and then Mul <= Largest_Factor
       and then (for all J in 1 .. N => abs Wr_Old (J) <= Unscaling_Bound (Mul))
       and then (for all J in 1 .. N => abs Wi_Old (J) <= Unscaling_Bound (Mul))
       and then Is_Paired (Wr_Old, Wi_Old, 1, N)
       and then Is_Scaled (Wr_Old, Wr, N, Mul)
       and then Is_Scaled (Wi_Old, Wi, N, Mul),
     Post => Is_Paired (Wr, Wi, 1, N);
   --  Scaling keeps eigenvalues paired.

   procedure Unscale
     (N : Order; First : Positive; Wr, Wi : in out Real_Vector; Mul : Real; Paired : Boolean; Ok : out Boolean)
   with
     Pre  =>
       Holds (N, Wr)
       and then Holds (N, Wi)
       and then First <= N + 1
       and then Mul > 0.0
       and then Mul <= Largest_Factor
       and then (if Paired then First = 1 and then Is_Paired (Wr, Wi, 1, N)),
     Post => (if Ok and then Paired then Is_Paired (Wr, Wi, 1, N));
   --  DLASCL ('G', 0, 0, Cscale, Anrm, N - First + 1, 1, W (First)) for Wr and Wi: multiply eigenvalues First .. N
   --  by Mul = Anrm / Cscale. Ok is False, and nothing is changed, when a product could overflow.

   procedure Lemma_Scaled_Element (X, Anrm, Cscale : Real) is
      Q  : constant Real := Cscale / Anrm;
      Ax : constant Real := abs X;
   begin
      pragma Assert (Q > 0.0);
      pragma Assert (Ax * Q <= 2.0 * Cscale);
      pragma Assert (abs (X * Q) = Ax * Q);
   end Lemma_Scaled_Element;

   procedure Scale_Matrix (N : Order; A : in out Real_Matrix; Anrm, Cscale : Real) is
      Mul    : constant Real := Cscale / Anrm;
      Target : constant Real := 2.0 * Cscale;
      X, Y   : Real;
   begin
      for I in 1 .. N loop
         pragma Loop_Invariant (for all R in 1 .. I - 1 => (for all C in 1 .. N => abs A (R, C) <= Target));
         pragma Loop_Invariant (for all R in I .. N => (for all C in 1 .. N => abs A (R, C) <= Anrm));
         for J in 1 .. N loop
            pragma Loop_Invariant (for all R in 1 .. I - 1 => (for all C in 1 .. N => abs A (R, C) <= Target));
            pragma Loop_Invariant (for all C in 1 .. J - 1 => abs A (I, C) <= Target);
            pragma Loop_Invariant (for all C in J .. N => abs A (I, C) <= Anrm);
            pragma Loop_Invariant (for all R in I + 1 .. N => (for all C in 1 .. N => abs A (R, C) <= Anrm));
            X := A (I, J);
            Lemma_Scaled_Element (X, Anrm, Cscale);
            Y := X * Mul;
            pragma Assert_And_Cut
              (abs Y <= Target
               and then (for all R in 1 .. I - 1 => (for all C in 1 .. N => abs A (R, C) <= Target))
               and then (for all C in 1 .. J - 1 => abs A (I, C) <= Target)
               and then (for all C in J .. N => abs A (I, C) <= Anrm)
               and then (for all R in I + 1 .. N => (for all C in 1 .. N => abs A (R, C) <= Anrm)));
            A (I, J) := Y;
         end loop;
      end loop;
      pragma Assert (Target <= Balanced_Limit);
      pragma Assert (for all R in 1 .. N => (for all C in 1 .. N => abs A (R, C) <= Target));
   end Scale_Matrix;

   procedure Lemma_Unscaled (X, Mul : Real) is
      Ax : constant Real := abs X;
   begin
      if Mul > 1.0 then
         pragma Assert (Ax <= Unscaled_Limit / Mul);
         pragma Assert (Ax * Mul <= 2.0 * Unscaled_Limit);
      else
         pragma Assert (Ax <= Scaled_Up_Limit);
         pragma Assert (Ax * Mul <= Scaled_Up_Limit);
      end if;
      pragma Assert (Ax * Mul <= Unscaled_Maximum);
      pragma Assert (abs (X * Mul) = Ax * Mul);
   end Lemma_Unscaled;

   function Scaled (X, Mul : Real) return Real is
   begin
      Lemma_Unscaled (X, Mul);
      return X * Mul;
   end Scaled;

   procedure Lemma_Scaled_Pair (R1, I1, R2, I2, Mul : Real) is
   begin
      Lemma_Unscaled (R1, Mul);
      Lemma_Unscaled (I1, Mul);
      Lemma_Unscaled (R2, Mul);
      Lemma_Unscaled (I2, Mul);
      pragma Assert (I1 * Mul >= 0.0);
      pragma Assert (R2 * Mul = R1 * Mul);
      pragma Assert ((-I1) * Mul = -(I1 * Mul));
   end Lemma_Scaled_Pair;

   procedure Lemma_Scaled_Pair_At (Wr_Old, Wi_Old, Wr, Wi : Real_Vector; N : Order; Mul : Real; J : Positive) is
   begin
      pragma Assert (Wr (J) = Scaled (Wr_Old (J), Mul) and then Wi (J) = Scaled (Wi_Old (J), Mul));
      if Wi_Old (J) > 0.0 then
         pragma Assert (J < N and then Wr_Old (J + 1) = Wr_Old (J) and then Wi_Old (J + 1) = -Wi_Old (J));
         pragma
           Assert (Wr (J + 1) = Scaled (Wr_Old (J + 1), Mul) and then Wi (J + 1) = Scaled (Wi_Old (J + 1), Mul));
         Lemma_Scaled_Pair (Wr_Old (J), Wi_Old (J), Wr_Old (J + 1), Wi_Old (J + 1), Mul);
         pragma Assert (Wi (J) >= 0.0);
         pragma Assert (Wr (J + 1) = Wr (J));
         pragma Assert (Wi (J + 1) = -Wi (J));
      elsif Wi_Old (J) < 0.0 then
         pragma Assert (J > 1 and then Wr_Old (J - 1) = Wr_Old (J) and then Wi_Old (J - 1) = -Wi_Old (J));
         pragma
           Assert (Wr (J - 1) = Scaled (Wr_Old (J - 1), Mul) and then Wi (J - 1) = Scaled (Wi_Old (J - 1), Mul));
         pragma Assert (Wi_Old (J - 1) >= 0.0);
         pragma Assert (Wr_Old (J) = Wr_Old (J - 1));
         pragma Assert (Wi_Old (J) = -Wi_Old (J - 1));
         Lemma_Scaled_Pair (Wr_Old (J - 1), Wi_Old (J - 1), Wr_Old (J), Wi_Old (J), Mul);
         pragma Assert (Wi (J) <= 0.0);
         pragma Assert (Wr (J - 1) = Wr (J));
         pragma Assert (Wi (J - 1) = -Wi (J));
      else
         pragma Assert (Wi (J) = 0.0);
      end if;
   end Lemma_Scaled_Pair_At;

   procedure Lemma_Scaled_Pairs (Wr_Old, Wi_Old, Wr, Wi : Real_Vector; N : Order; Mul : Real) is
   begin
      for J in 1 .. N loop
         pragma Assert (Is_Paired_At (Wr_Old, Wi_Old, N, J));
         Lemma_Scaled_Pair_At (Wr_Old, Wi_Old, Wr, Wi, N, Mul, J);
         pragma Loop_Invariant (for all K in 1 .. J => Is_Paired_At (Wr, Wi, N, K));
      end loop;
   end Lemma_Scaled_Pairs;

   procedure Unscale
     (N : Order; First : Positive; Wr, Wi : in out Real_Vector; Mul : Real; Paired : Boolean; Ok : out Boolean)
   is
      Bound : constant Real := Unscaling_Bound (Mul);

      Wr_Old : constant Real_Vector := Wr with Ghost;
      Wi_Old : constant Real_Vector := Wi with Ghost;
   begin
      --  When the matrix was scaled down, an eigenvalue of the matrix itself can be too large for the type.
      for I in First .. N loop
         pragma Loop_Invariant (for all J in First .. I - 1 => abs Wr (J) <= Bound and then abs Wi (J) <= Bound);
         if abs Wr (I) > Bound or else abs Wi (I) > Bound then
            Ok := False;
            return;
         end if;
      end loop;
      Ok := True;

      for I in First .. N loop
         pragma Loop_Invariant (for all J in First .. N => abs Wr_Old (J) <= Bound and then abs Wi_Old (J) <= Bound);
         pragma Loop_Invariant (for all J in I .. N => Wr (J) = Wr_Old (J) and then Wi (J) = Wi_Old (J));
         pragma Loop_Invariant
           (for all J in First .. I - 1 =>
              Wr (J) = Scaled (Wr_Old (J), Mul) and then Wi (J) = Scaled (Wi_Old (J), Mul));
         Lemma_Unscaled (Wr (I), Mul);
         Lemma_Unscaled (Wi (I), Mul);
         Wr (I) := Wr (I) * Mul;
         Wi (I) := Wi (I) * Mul;
         pragma Assert (Wr (I) = Scaled (Wr_Old (I), Mul) and then Wi (I) = Scaled (Wi_Old (I), Mul));
      end loop;

      if Paired then
         Lemma_Scaled_Pairs (Wr_Old, Wi_Old, Wr, Wi, N, Mul);
      end if;
   end Unscale;

   procedure Solve (N : Order; A : in out Real_Matrix; Wr, Wi : out Real_Vector; Info : out Integer) is
      Anrm     : Real;
      Cscale   : Real := 1.0;
      Scaled   : Boolean := False;
      Ilo      : Positive;
      Ihi      : Natural;
      Qr_Info  : Natural := 0;
      Ok       : Boolean;
   begin
      Info := 0;
      Wr := [others => 0.0];
      Wi := [others => 0.0];
      if N = 0 then
         return;
      end if;

      --  Scale A if its largest element is outside the range Smlnum .. Bignum.
      Anrm := Norms.Block_Max (A, 1, N);
      if Anrm > 0.0 and then Anrm < Smlnum then
         Scaled := True;
         Cscale := Smlnum;
      elsif Anrm > Bignum then
         Scaled := True;
         Cscale := Bignum;
      end if;
      if Scaled then
         --  DLASCL multiplies in several steps when the quotient of its two scales, or their product with the
         --  largest or smallest machine number, could overflow or underflow. Between the scales used here it
         --  always takes a single step.
         Scale_Matrix (N, A, Anrm, Cscale);
      end if;

      --  Balance the matrix and reduce it to upper Hessenberg form.
      Balancing.Balance (N, A, Ilo, Ihi, Ok);
      if not Ok then
         Info := Out_Of_Range;
         return;
      end if;
      Hessenberg_Reduction.Reduce (N, Ilo, Ihi, A, Ok);
      if not Ok then
         Info := Out_Of_Range;
         return;
      end if;

      --  DHSEQR: the eigenvalues which balancing isolated are on the diagonal.
      for I in 1 .. Ilo - 1 loop
         Wr (I) := A (I, I);
         pragma Loop_Invariant (for all J in 1 .. N => Wi (J) = 0.0);
      end loop;
      for I in Ihi + 1 .. N loop
         Wr (I) := A (I, I);
         pragma Loop_Invariant (for all J in 1 .. N => Wi (J) = 0.0);
      end loop;

      --  The QR algorithm finds the others.
      if Ilo <= Ihi then
         Hessenberg_QR.Eigenvalues (N, Ilo, Ihi, A, Wr, Wi, Qr_Info, Ok);
         if not Ok then
            Info := Out_Of_Range;
            return;
         end if;
         Info := Qr_Info;
      end if;
      pragma Assert (if Qr_Info = 0 then Is_Paired (Wr, Wi, 1, N));

      --  Undo the scaling.
      if Scaled then
         Unscale (N, Qr_Info + 1, Wr, Wi, Anrm / Cscale, Qr_Info = 0, Ok);
         if Ok and then Qr_Info > 0 then
            --  The eigenvalues which balancing isolated are valid as well.
            Unscale (Ilo - 1, 1, Wr, Wi, Anrm / Cscale, False, Ok);
         end if;
         if not Ok then
            Info := Out_Of_Range;
         end if;
      end if;
   end Solve;

end Prunt.LAPACK.Eigenvalues;
