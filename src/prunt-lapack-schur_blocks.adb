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
with Prunt.LAPACK.Elementary; use Prunt.LAPACK.Elementary;
with Prunt.LAPACK.Norms;      use Prunt.LAPACK.Norms;

package body Prunt.LAPACK.Schur_Blocks
  with SPARK_Mode
is

   Multiple : constant Real := 4.0;
   --  MULTPL: with Z below this many units of rounding the eigenvalues are treated as complex or equal.

   Product_Limit : constant Real := 2#1.0#E960;
   --  A product which the proof does not bound is tested against this.

   Scale_Minimum : constant Real := 2#1.0#E-485;
   Scale_Maximum : constant Real := 2#1.0#E485;
   --  SAFMN2 = base ** int (log (DLAMCH ('S') / DLAMCH ('P')) / log (base) / 2) and SAFMX2, its reciprocal.

   --  The magnitudes the proof follows. DLANV2 is split below into its stages, each a subprogram whose contract
   --  bounds what it reads and what it leaves, so that the provers see a few operations at a time.

   Twice_Limit : constant Real := 2#1.0#E921;
   --  Twice Block_Entry_Limit: a sum or a difference of two elements of the block is within this.

   Rotated_Limit : constant Real := 2#1.0#E922;
   --  Four times Block_Entry_Limit: an element of the block is within this once the rotation is applied to both
   --  sides of the block.

   Root_Limit : constant Real := 2#1.0#E461
   with Ghost;
   --  The root of Rotated_Limit.

   Real_Scale_Minimum : constant Real := 2#1.0#E-51
   with Ghost;
   --  Half of Multiple * Precision: Scale is at least this when Z is at least Multiple * Precision.

   Root_Product_Minimum : constant Real := 2#1.0#E-51
   with Ghost;
   --  The product of the roots of Scale and Z in the real branch is at least this.

   Shift_Limit : constant Real := 2#1.0#E923
   with Ghost;
   --  Twice Rotated_Limit.

   Quotient_Limit : constant Real := 2#1.0#E971
   with Ghost;
   --  Block_Entry_Limit / Root_Product_Minimum.

   --------------
   --  Lemmas  --
   --------------

   procedure Lemma_Roots (X, Y : Real)
   with
     Ghost,
     Pre  => abs X <= Rotated_Limit and then abs Y <= Rotated_Limit,
     Post => Sqrt (abs X) * Sqrt (abs Y) >= 0.0 and then Sqrt (abs X) * Sqrt (abs Y) <= Rotated_Limit;
   --  The product of the roots of two magnitudes within Rotated_Limit is within it too.

   procedure Lemma_Roots (X, Y : Real) is
      Rx : constant Real := Sqrt (abs X);
      Ry : constant Real := Sqrt (abs Y);
   begin
      Lemma_Sqrt_Is_Monotonic (abs X, Root_Limit * Root_Limit);
      Lemma_Sqrt_Is_Monotonic (abs Y, Root_Limit * Root_Limit);
      Lemma_Sqrt_Of_Square (Root_Limit);
      pragma Assert (Rx >= 0.0 and then Rx <= Root_Limit);
      pragma Assert (Ry >= 0.0 and then Ry <= Root_Limit);
      pragma Assert (Rx * Ry >= 0.0 and then Rx * Ry <= Rotated_Limit);
   end Lemma_Roots;

   procedure Lemma_Term (N, X, Scale : Real)
   with
     Ghost,
     Pre  => Scale > 0.0 and then Scale <= Block_Entry_Limit and then abs N <= Scale and then abs X <= Scale,
     Post => abs (N / Scale) <= 1.0 and then abs ((N / Scale) * X) <= Scale;
   --  A term of Z: a number within Scale multiplied by a quotient within one stays within Scale.

   procedure Lemma_Term (N, X, Scale : Real) is
   begin
      Bounds.Lemma_Quotient_Within_One (N, Scale);
      Bounds.Lemma_Shrink (X, N / Scale);
      pragma Assert (abs (X * (N / Scale)) <= Scale);
      pragma Assert ((N / Scale) * X = X * (N / Scale));
   end Lemma_Term;

   procedure Lemma_Sum_Of_Terms (X, Y, Scale : Real)
   with
     Ghost,
     Pre  => Scale > 0.0 and then Scale <= Block_Entry_Limit and then abs X <= Scale and then abs Y <= Scale,
     Post =>
       abs (X + Y) <= Twice_Limit and then (if X + Y >= Multiple * Precision then Scale >= Real_Scale_Minimum);
   --  Z is the sum of two terms within Scale, so it is not large, and Scale is not small when Z is not. The
   --  bounds are constants because the provers are slow with a sum bounded by a variable.

   procedure Lemma_Sum_Of_Terms (X, Y, Scale : Real) is null;

   procedure Lemma_Root_Of_Discriminant (Scale, Z : Real)
   with
     Ghost,
     Pre  =>
       Scale >= Real_Scale_Minimum
       and then Scale <= Block_Entry_Limit
       and then Z >= Multiple * Precision
       and then Z <= Twice_Limit,
     Post => Sqrt (Scale) * Sqrt (Z) >= Root_Product_Minimum and then Sqrt (Scale) * Sqrt (Z) <= Rotated_Limit;
   --  Neither Scale nor Z is small or large, so neither root is.

   procedure Lemma_Root_Of_Discriminant (Scale, Z : Real) is
      Scale_Root_Minimum : constant Real := 2#1.0#E-26;
      Z_Root_Minimum     : constant Real := 2#1.0#E-25;
      Rs                 : constant Real := Sqrt (Scale);
      Rz                 : constant Real := Sqrt (Z);
   begin
      pragma Assert (Scale >= Scale_Root_Minimum * Scale_Root_Minimum);
      pragma Assert (Z >= Z_Root_Minimum * Z_Root_Minimum);
      Lemma_Sqrt_Is_Monotonic (Scale_Root_Minimum * Scale_Root_Minimum, Scale);
      Lemma_Sqrt_Of_Square (Scale_Root_Minimum);
      Lemma_Sqrt_Is_Monotonic (Z_Root_Minimum * Z_Root_Minimum, Z);
      Lemma_Sqrt_Of_Square (Z_Root_Minimum);
      Lemma_Sqrt_Is_Monotonic (Scale, Root_Limit * Root_Limit);
      Lemma_Sqrt_Is_Monotonic (Z, Root_Limit * Root_Limit);
      Lemma_Sqrt_Of_Square (Root_Limit);
      pragma Assert (Rs >= Scale_Root_Minimum and then Rs <= Root_Limit);
      pragma Assert (Rz >= Z_Root_Minimum and then Rz <= Root_Limit);
      pragma Assert (Rs * Rz >= Root_Product_Minimum and then Rs * Rz <= Rotated_Limit);
   end Lemma_Root_Of_Discriminant;

   procedure Lemma_Away_From_Zero (P, S : Real)
   with
     Ghost,
     Pre  => abs P <= Block_Entry_Limit and then S >= Root_Product_Minimum and then S <= Rotated_Limit,
     Post => abs (P + Sign (S, P)) >= S and then abs (P + Sign (S, P)) <= Shift_Limit;
   --  Adding to P a number of magnitude S with the sign bit of P moves it away from zero by S at least. When P
   --  is a zero the sum is that number, whichever sign it has.

   procedure Lemma_Away_From_Zero (P, S : Real) is
      T : constant Real := Sign (S, P);
   begin
      pragma Assert (abs T = S);
      if P > 0.0 then
         pragma Assert (T = S);
         pragma Assert (P + T >= S);
      elsif P < 0.0 then
         pragma Assert (T = -S);
         pragma Assert (P + T <= -S);
      else
         pragma Assert (T = S or else T = -S);
         pragma Assert (P + T = T);
      end if;
   end Lemma_Away_From_Zero;

   procedure Lemma_Guarded_Product (Q, X : Real)
   with
     Ghost,
     Pre  =>
       abs Q <= Quotient_Limit
       and then abs X <= Block_Entry_Limit
       and then (abs Q <= 1.0 or else abs X <= Product_Limit / abs Q),
     Post => abs (Q * X) <= 2.0 * Product_Limit;
   --  A product which passed the test against Product_Limit is within twice Product_Limit.

   procedure Lemma_Guarded_Product (Q, X : Real) is
      F : constant Real := abs Q;
      Y : constant Real := abs X;
   begin
      if F <= 1.0 then
         Bounds.Lemma_Shrink (X, Q);
         pragma Assert (Q * X = X * Q);
      else
         pragma Assert (Y * F <= 2.0 * Product_Limit);
         pragma Assert (abs (Q * X) = Y * F);
      end if;
   end Lemma_Guarded_Product;

   procedure Lemma_Cosine (Ratio : Real)
   with
     Ghost,
     Pre  => Ratio >= 0.0 and then Ratio <= 1.0,
     Post => Sqrt (0.5 * (1.0 + Ratio)) >= 0.5 and then Sqrt (0.5 * (1.0 + Ratio)) <= 1.0;
   --  The cosine of the rotation is the root of a number between a half and one.

   procedure Lemma_Cosine (Ratio : Real) is
      Half : constant Real := 0.5;
      One  : constant Real := 1.0;
      H    : constant Real := 0.5 * (1.0 + Ratio);
   begin
      pragma Assert (H >= Half * Half and then H <= One * One);
      Lemma_Sqrt_Is_Monotonic (Half * Half, H);
      Lemma_Sqrt_Of_Square (Half);
      Lemma_Sqrt_Is_Monotonic (H, One * One);
      Lemma_Sqrt_Of_Square (One);
   end Lemma_Cosine;

   procedure Lemma_Sine (P, Tau, Cs, S : Real)
   with
     Ghost,
     Pre  =>
       Tau >= Scale_Minimum
       and then Tau <= Rotated_Limit
       and then abs P <= 0.5 * Tau
       and then Cs >= 0.5
       and then Cs <= 1.0
       and then abs S <= 1.0,
     Post =>
       Tau * Cs > 0.0
       and then Tau * Cs <= Rotated_Limit
       and then abs (P / (Tau * Cs)) <= 1.0
       and then abs (-(P / (Tau * Cs)) * S) <= 1.0;
   --  P is within half of Tau and the cosine is at least a half, so the sine is a quotient within one.

   procedure Lemma_Sine (P, Tau, Cs, S : Real) is
      D : constant Real := Tau * Cs;
      Q : constant Real := P / D;
   begin
      Bounds.Lemma_Mult_Is_Monotonic (0.5, Cs, Tau);
      pragma Assert (D = Cs * Tau);
      pragma Assert (D >= 0.5 * Tau);
      pragma Assert (D > 0.0);
      pragma Assert (D <= Rotated_Limit);
      pragma Assert (abs P <= D);
      Bounds.Lemma_Quotient_Within_One (P, D);
      Bounds.Lemma_Shrink (Q, S);
      pragma Assert (abs (Q * S) <= 1.0);
   end Lemma_Sine;

   procedure Lemma_Rotated_Once (X, Y, U, V : Real)
   with
     Ghost,
     Pre  =>
       abs X <= Block_Entry_Limit
       and then abs Y <= Block_Entry_Limit
       and then abs U <= 1.0
       and then abs V <= 1.0,
     Post =>
       abs (X * U) <= Block_Entry_Limit
       and then abs (Y * V) <= Block_Entry_Limit
       and then abs (X * U + Y * V) <= Twice_Limit
       and then abs (-(X * U) + Y * V) <= Twice_Limit;
   --  An element of the product of the block by the rotation is within twice the bound of the block.

   procedure Lemma_Rotated_Once (X, Y, U, V : Real) is
   begin
      Bounds.Lemma_Shrink (X, U);
      Bounds.Lemma_Shrink (Y, V);
   end Lemma_Rotated_Once;

   procedure Lemma_Rotated_Twice (X, Y, U, V : Real)
   with
     Ghost,
     Pre  => abs X <= Twice_Limit and then abs Y <= Twice_Limit and then abs U <= 1.0 and then abs V <= 1.0,
     Post =>
       abs (X * U) <= Twice_Limit
       and then abs (Y * V) <= Twice_Limit
       and then abs (X * U + Y * V) <= Rotated_Limit
       and then abs (-(X * U) + Y * V) <= Rotated_Limit;
   --  The same for the product of the rotation by that product. The two lemmas have constant bounds because the
   --  provers are slow with a sum bounded by a variable.

   procedure Lemma_Rotated_Twice (X, Y, U, V : Real) is
   begin
      Bounds.Lemma_Shrink (X, U);
      Bounds.Lemma_Shrink (Y, V);
   end Lemma_Rotated_Twice;

   --------------
   --  Stages  --
   --------------

   procedure Form_Discriminant (Aa, Bb, Cc, Dd : Real; Temp, P, Bcmax, Bcmis, Scale, Z : out Real)
   with
     Pre  =>
       abs Aa <= Block_Entry_Limit
       and then abs Bb <= Block_Entry_Limit
       and then abs Cc <= Block_Entry_Limit
       and then abs Dd <= Block_Entry_Limit
       and then Bb /= 0.0
       and then Cc /= 0.0,
     Post =>
       abs Temp <= Twice_Limit
       and then abs P <= Block_Entry_Limit
       and then Bcmax > 0.0
       and then Bcmax <= Block_Entry_Limit
       and then abs Bcmis <= Block_Entry_Limit
       and then Scale > 0.0
       and then Scale <= Block_Entry_Limit
       and then abs Z <= Twice_Limit
       and then (if Z >= Multiple * Precision then Scale >= Real_Scale_Minimum);
   --  Z, whose sign is that of the discriminant of the characteristic polynomial, scaled by Scale so that it
   --  cannot overflow, and what it is formed from.

   procedure Form_Discriminant (Aa, Bb, Cc, Dd : Real; Temp, P, Bcmax, Bcmis, Scale, Z : out Real) is
      Smaller : constant Real := Real'Min (abs Bb, abs Cc)
      with Ghost;
      Sb      : constant Real := Sign (1.0, Bb)
      with Ghost;
      Sc      : constant Real := Sign (1.0, Cc)
      with Ghost;
   begin
      Temp := Aa - Dd;
      P := 0.5 * Temp;
      pragma Assert (abs P <= Block_Entry_Limit);
      Bcmax := Real'Max (abs Bb, abs Cc);
      Bounds.Lemma_Shrink (Smaller, Sb);
      Bounds.Lemma_Shrink (Smaller * Sb, Sc);
      Bcmis := Real'Min (abs Bb, abs Cc) * Sign (1.0, Bb) * Sign (1.0, Cc);
      pragma Assert (abs Bcmis <= Bcmax);
      Scale := Real'Max (abs P, Bcmax);
      pragma
        Assert_And_Cut
          (abs Temp <= Twice_Limit
             and then abs P <= Block_Entry_Limit
             and then Bcmax > 0.0
             and then Bcmax <= Block_Entry_Limit
             and then abs Bcmis <= Block_Entry_Limit
             and then Scale > 0.0
             and then Scale <= Block_Entry_Limit
             and then abs P <= Scale
             and then Bcmax <= Scale
             and then abs Bcmis <= Scale);
      Lemma_Term (P, P, Scale);
      Lemma_Term (Bcmax, Bcmis, Scale);
      Lemma_Sum_Of_Terms ((P / Scale) * P, (Bcmax / Scale) * Bcmis, Scale);
      Z := (P / Scale) * P + (Bcmax / Scale) * Bcmis;
   end Form_Discriminant;

   procedure Split_Real
     (P, Scale, Z, Bcmax, Bcmis : Real; Aa : out Real; Bb, Cc, Dd : in out Real; Ok : out Boolean)
   with
     Pre  =>
       abs P <= Block_Entry_Limit
       and then Scale >= Real_Scale_Minimum
       and then Scale <= Block_Entry_Limit
       and then Z >= Multiple * Precision
       and then Z <= Twice_Limit
       and then Bcmax >= 0.0
       and then Bcmax <= Block_Entry_Limit
       and then abs Bcmis <= Block_Entry_Limit
       and then abs Bb <= Block_Entry_Limit
       and then abs Cc <= Block_Entry_Limit
       and then abs Dd <= Block_Entry_Limit,
     Post => (if Ok then Cc = 0.0);
   --  The real eigenvalues of a block whose discriminant is positive: the block is made upper triangular.

   procedure Split_Real
     (P, Scale, Z, Bcmax, Bcmis : Real; Aa : out Real; Bb, Cc, Dd : in out Real; Ok : out Boolean)
   is
      Shift, Quotient : Real;
   begin
      Ok := True;
      Lemma_Root_Of_Discriminant (Scale, Z);
      Lemma_Away_From_Zero (P, Sqrt (Scale) * Sqrt (Z));
      Shift := P + Sign (Sqrt (Scale) * Sqrt (Z), P);
      pragma Assert_And_Cut (abs Shift >= Root_Product_Minimum and then abs Shift <= Shift_Limit);
      Aa := Dd + Shift;
      if Shift = 0.0 then
         --  Shift is P moved away from zero by the product of two roots which are not small, so this does not
         --  happen.
         Ok := False;
         return;
      end if;
      Quotient := Bcmax / Shift;
      pragma Assert (abs Quotient <= Quotient_Limit);
      --  The product is a quotient of the product of B and C by Shift, and so at most the larger of P and Bcmax.
      if abs Quotient > 1.0 and then abs Bcmis > Product_Limit / abs Quotient then
         Ok := False;
         return;
      end if;
      Lemma_Guarded_Product (Quotient, Bcmis);
      Dd := Dd - Quotient * Bcmis;
      Bb := Bb - Cc;
      Cc := 0.0;
   end Split_Real;

   procedure Scale_Pair (Sigma, Temp : in out Real)
   with
     Pre  => abs Sigma <= Twice_Limit and then abs Temp <= Twice_Limit,
     Post => abs Sigma <= Twice_Limit and then abs Temp <= Twice_Limit;
   --  Scale Sigma and Temp by the same power of two so that the rotation can be formed from them.

   procedure Scale_Pair (Sigma, Temp : in out Real) is
      Count : Natural := 0;
      Scale : Real;
   begin
      loop
         pragma Loop_Invariant (Count <= 20 and then abs Sigma <= Twice_Limit and then abs Temp <= Twice_Limit);
         pragma Loop_Variant (Increases => Count);
         Count := Count + 1;
         Scale := Real'Max (abs Temp, abs Sigma);
         if Scale >= Scale_Maximum then
            Bounds.Lemma_Shrink (Sigma, Scale_Minimum);
            Bounds.Lemma_Shrink (Temp, Scale_Minimum);
            Sigma := Sigma * Scale_Minimum;
            Temp := Temp * Scale_Minimum;
            exit when Count > 20;
         elsif Scale <= Scale_Minimum then
            Sigma := Sigma * Scale_Maximum;
            Temp := Temp * Scale_Maximum;
            pragma Assert (abs Sigma <= 1.0 and then abs Temp <= 1.0);
            exit when Count > 20;
         else
            exit;
         end if;
      end loop;
   end Scale_Pair;

   procedure Find_Rotation (Sigma, Temp : Real; Cs, Sn : out Real; Ok : out Boolean)
   with
     Pre  => abs Sigma <= Twice_Limit and then abs Temp <= Twice_Limit,
     Post => (if Ok then abs Cs <= 1.0 and then abs Sn <= 1.0);
   --  The cosine and sine of the rotation which makes the diagonal elements equal.

   procedure Find_Rotation (Sigma, Temp : Real; Cs, Sn : out Real; Ok : out Boolean) is
      P, Tau : Real;
   begin
      Ok := True;
      Cs := 0.0;
      Sn := 0.0;
      P := 0.5 * Temp;
      Tau := Hypotenuse (Sigma, Temp);
      if Tau < Scale_Minimum then
         --  The scaling left the larger of Sigma and Temp, which Tau is no smaller than, above this.
         Ok := False;
         return;
      end if;
      pragma Assert (Tau <= Rotated_Limit);
      Bounds.Lemma_Quotient_Within_One (abs Sigma, Tau);
      pragma Assert (abs Sigma / Tau >= 0.0 and then abs Sigma / Tau <= 1.0);
      Lemma_Cosine (abs Sigma / Tau);
      Cs := Sqrt (0.5 * (1.0 + abs Sigma / Tau));
      pragma Assert (abs P <= 0.5 * Tau);
      pragma
        Assert_And_Cut
          (Tau >= Scale_Minimum
             and then Tau <= Rotated_Limit
             and then abs P <= 0.5 * Tau
             and then Cs >= 0.5
             and then Cs <= 1.0);
      Lemma_Sine (P, Tau, Cs, Sign (1.0, Sigma));
      Sn := -(P / (Tau * Cs)) * Sign (1.0, Sigma);
   end Find_Rotation;

   procedure Rotate_Columns (Cs, Sn, Aa, Bb, Cc, Dd : Real; A1, B1, C1, D1 : out Real)
   with
     Pre  =>
       abs Cs <= 1.0
       and then abs Sn <= 1.0
       and then abs Aa <= Block_Entry_Limit
       and then abs Bb <= Block_Entry_Limit
       and then abs Cc <= Block_Entry_Limit
       and then abs Dd <= Block_Entry_Limit,
     Post =>
       abs A1 <= Twice_Limit
       and then abs B1 <= Twice_Limit
       and then abs C1 <= Twice_Limit
       and then abs D1 <= Twice_Limit;
   --  Compute [ A1  B1 ] = [ Aa  Bb ] [ Cs  -Sn ]
   --          [ C1  D1 ]   [ Cc  Dd ] [ Sn   Cs ]

   procedure Rotate_Columns (Cs, Sn, Aa, Bb, Cc, Dd : Real; A1, B1, C1, D1 : out Real) is
   begin
      Lemma_Rotated_Once (Aa, Bb, Cs, Sn);
      Lemma_Rotated_Once (Aa, Bb, Sn, Cs);
      A1 := Aa * Cs + Bb * Sn;
      B1 := -Aa * Sn + Bb * Cs;
      pragma Assert_And_Cut (abs A1 <= Twice_Limit and then abs B1 <= Twice_Limit);
      Lemma_Rotated_Once (Cc, Dd, Cs, Sn);
      Lemma_Rotated_Once (Cc, Dd, Sn, Cs);
      C1 := Cc * Cs + Dd * Sn;
      D1 := -Cc * Sn + Dd * Cs;
   end Rotate_Columns;

   procedure Rotate_Rows (Cs, Sn, A1, B1, C1, D1 : Real; Aa, Bb, Cc, Dd : out Real)
   with
     Pre  =>
       abs Cs <= 1.0
       and then abs Sn <= 1.0
       and then abs A1 <= Twice_Limit
       and then abs B1 <= Twice_Limit
       and then abs C1 <= Twice_Limit
       and then abs D1 <= Twice_Limit,
     Post =>
       abs Aa <= Rotated_Limit
       and then abs Bb <= Rotated_Limit
       and then abs Cc <= Rotated_Limit
       and then abs Dd <= Rotated_Limit;
   --  Compute [ Aa  Bb ] = [  Cs  Sn ] [ A1  B1 ]
   --          [ Cc  Dd ]   [ -Sn  Cs ] [ C1  D1 ]

   procedure Rotate_Rows (Cs, Sn, A1, B1, C1, D1 : Real; Aa, Bb, Cc, Dd : out Real) is
   begin
      Lemma_Rotated_Twice (A1, C1, Cs, Sn);
      Lemma_Rotated_Twice (B1, D1, Cs, Sn);
      Aa := A1 * Cs + C1 * Sn;
      Bb := B1 * Cs + D1 * Sn;
      pragma Assert_And_Cut (abs Aa <= Rotated_Limit and then abs Bb <= Rotated_Limit);
      Lemma_Rotated_Twice (A1, C1, Sn, Cs);
      Lemma_Rotated_Twice (B1, D1, Sn, Cs);
      Cc := -(A1 * Sn) + C1 * Cs;
      Dd := -B1 * Sn + D1 * Cs;
   end Rotate_Rows;

   procedure Standardize (Aa, Bb, Cc, Dd : in out Real)
   with
     Pre  =>
       abs Aa <= Rotated_Limit
       and then abs Bb <= Rotated_Limit
       and then abs Cc <= Rotated_Limit
       and then abs Dd <= Rotated_Limit,
     Post => Cc = 0.0 or else (Aa = Dd and then abs Bb <= Rotated_Limit and then abs Cc <= Rotated_Limit);
   --  Make the diagonal elements equal, and the block upper triangular when its eigenvalues are real after all.

   procedure Standardize (Aa, Bb, Cc, Dd : in out Real) is
      P, Temp : Real;
   begin
      Temp := 0.5 * (Aa + Dd);
      pragma Assert (abs Temp <= Rotated_Limit);
      Aa := Temp;
      Dd := Temp;

      if Cc /= 0.0 then
         if Bb /= 0.0 then
            if Sign (1.0, Bb) = Sign (1.0, Cc) then
               --  Real eigenvalues: reduce to upper triangular form.
               Lemma_Roots (Bb, Cc);
               P := Sign (Sqrt (abs Bb) * Sqrt (abs Cc), Cc);
               pragma Assert (abs P <= Rotated_Limit);
               Aa := Temp + P;
               Dd := Temp - P;
               Bb := Bb - Cc;
               Cc := 0.0;
            end if;
         else
            --  Copy -Cc to Bb and set Cc to zero.
            Bb := -Cc;
            Cc := 0.0;
         end if;
      end if;
   end Standardize;

   --------------------------
   -- Eigenvalues_Of_Block --
   --------------------------

   procedure Eigenvalues_Of_Block
     (A, B, C, D : Real; Rt1r, Rt1i, Rt2r, Rt2i : out Real; Ok : out Boolean)
   is
      Aa, Bb, Cc, Dd : Real;
      --  The block as it is transformed.

      A1, B1, C1, D1                                 : Real;
      Bcmax, Bcmis, Cs, P, Scale, Sigma, Sn, Temp, Z : Real;
   begin
      Ok := True;
      Rt1r := 0.0;
      Rt1i := 0.0;
      Rt2r := 0.0;
      Rt2i := 0.0;
      Aa := A;
      Bb := B;
      Cc := C;
      Dd := D;

      if Cc = 0.0 then
         null;

      elsif Bb = 0.0 then
         --  Swap rows and columns.
         Temp := Dd;
         Dd := Aa;
         Aa := Temp;
         Bb := -Cc;
         Cc := 0.0;

      elsif Aa - Dd = 0.0 and then Sign (1.0, Bb) /= Sign (1.0, Cc) then
         pragma Assert (Aa = Dd);

      else
         Form_Discriminant (Aa, Bb, Cc, Dd, Temp, P, Bcmax, Bcmis, Scale, Z);

         --  If Z is of the order of the machine accuracy, postpone the decision on the nature of the eigenvalues.
         if Z >= Multiple * Precision then
            --  Real eigenvalues.
            Split_Real (P, Scale, Z, Bcmax, Bcmis, Aa, Bb, Cc, Dd, Ok);
            if not Ok then
               return;
            end if;

         else
            --  Complex eigenvalues, or real (almost) equal eigenvalues: make the diagonal elements equal.
            Sigma := Bb + Cc;
            Scale_Pair (Sigma, Temp);
            Find_Rotation (Sigma, Temp, Cs, Sn, Ok);
            if not Ok then
               return;
            end if;
            Rotate_Columns (Cs, Sn, Aa, Bb, Cc, Dd, A1, B1, C1, D1);
            Rotate_Rows (Cs, Sn, A1, B1, C1, D1, Aa, Bb, Cc, Dd);
            Standardize (Aa, Bb, Cc, Dd);
         end if;
      end if;

      --  A block which is left with a subdiagonal element has equal diagonal elements.
      pragma Assert (Cc = 0.0 or else (Aa = Dd and then abs Bb <= Rotated_Limit and then abs Cc <= Rotated_Limit));

      --  Store the eigenvalues.
      Rt1r := Aa;
      Rt2r := Dd;
      if Cc = 0.0 then
         Rt1i := 0.0;
         Rt2i := 0.0;
      else
         Lemma_Roots (Bb, Cc);
         Rt1i := Sqrt (abs Bb) * Sqrt (abs Cc);
         Rt2i := -Rt1i;
      end if;
   end Eigenvalues_Of_Block;

end Prunt.LAPACK.Schur_Blocks;
