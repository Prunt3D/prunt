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
with Prunt.LAPACK.Norms;      use Prunt.LAPACK.Norms;

package body Prunt.LAPACK.Reflectors
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);

   Rescaling_Threshold : constant Real := 2#1.0#E-969;
   --  DLAMCH ('S') / DLAMCH ('E'): below this Beta may be inaccurate, so the vector is scaled up first.

   Rescaling_Factor : constant Real := 2#1.0#E969;

   Maximum_Rescalings : constant := 20;

   Beta_Limit : constant Real := 2#1.0#E985
   with Ghost;
   --  Twice Norm_Limit, which Beta is within.

   function Is_Opposed (Alpha, Beta : Real) return Boolean
   is ((Alpha >= 0.0 and then Beta < 0.0) or else (Alpha <= 0.0 and then Beta > 0.0))
   with Ghost;
   --  Beta is not zero and does not have the sign of a non-zero Alpha.

   function Is_Largest (Count : Positive; Alpha : Real; X : Real_Vector; Largest : Real) return Boolean
   is (Largest >= 0.0
       and then Largest <= Entry_Limit
       and then (if Count = 2 then Largest = Real'Max (abs Alpha, abs X (1)))
       and then (if Count = 3 then Largest = Real'Max (abs Alpha, Real'Max (abs X (1), abs X (2)))))
   with Ghost, Pre => X'First = 1 and then Count - 1 <= X'Last;
   --  Largest is the largest magnitude among Alpha and X (1 .. Count - 1), as far as the contract of Generate
   --  refers to it.

   function Is_Close (Count : Positive; Alpha : Real; X : Real_Vector; Beta : Real) return Boolean
   is ((if Count = 2 then abs Beta <= 4.0 * Real'Max (abs Alpha, abs X (1)))
       and then (if Count = 3 then abs Beta <= 4.0 * Real'Max (abs Alpha, Real'Max (abs X (1), abs X (2)))))
   with
     Ghost,
     Pre =>
       X'First = 1
       and then Count - 1 <= X'Last
       and then abs Alpha <= Entry_Limit
       and then (for all I in 1 .. Count - 1 => abs X (I) <= Entry_Limit);
   --  Beta is within four times the largest magnitude it is formed from, for the orders the contract of Generate
   --  says so.

   function Is_Kept (Count : Natural; Before, After : Real_Vector) return Boolean
   is (for all I in After'Range => (if I >= Count then After (I) = Before (I)))
   with Ghost, Pre => Before'First = After'First and then Before'Last = After'Last;
   --  The elements from Count on are those of Before: a reflector of order Count leaves them alone.

   function Is_Reflector
     (Count        : Natural;
      Alpha_Before : Real;
      X_Before     : Real_Vector;
      Alpha        : Real;
      X            : Real_Vector;
      Tau          : Real;
      Bounded      : Boolean) return Boolean
   is (Is_Kept (Count, X_Before, X)
       and then
         (if Bounded
          then
            (Tau = 0.0 or else (Tau >= 1.0 and then Tau <= 2.0))
            and then abs Alpha <= Reflector_Limit
            and then (if Tau = 0.0 then Alpha = Alpha_Before and then (for all I in 1 .. Count - 1 => X (I) = 0.0))
            and then (for all I in 1 .. Count - 1 => abs X (I) <= 2.0)
            and then (if Count = 2 then abs Alpha <= 4.0 * Real'Max (abs Alpha_Before, abs X_Before (1)))
            and then
              (if Count = 3
               then
                 abs Alpha <= 4.0 * Real'Max (abs Alpha_Before, Real'Max (abs X_Before (1), abs X_Before (2))))))
   with
     Ghost,
     Pre =>
       X'First = 1
       and then X_Before'First = 1
       and then X_Before'Last = X'Last
       and then Count - 1 <= X'Last
       and then abs Alpha_Before <= Entry_Limit
       and then (for all I in 1 .. Count - 1 => abs X_Before (I) <= Entry_Limit);
   --  The postcondition of Generate, of the values Alpha_Before and X_Before it is called with and those it
   --  returns.

   --------------
   --  Lemmas  --
   --------------

   procedure Lemma_Element_Is_Zero (V : Real)
   with Ghost, Pre => Is_Medium_Or_Zero (V) and then V * V <= 0.0, Post => V = 0.0;
   --  The square of an element of medium size is not zero.

   procedure Lemma_Element_Within_Norm (V, Medium : Real)
   with
     Ghost,
     Pre  => Is_Medium_Or_Zero (V) and then Medium >= 0.0 and then Medium >= V * V,
     Post => abs V <= Sqrt (Medium);
   --  An element is within the root of a sum which holds its square.

   procedure Lemma_Norm_Of_One (X1, Medium : Real)
   with
     Ghost,
     Pre  => Is_Medium_Or_Zero (X1) and then Medium > 0.0 and then Medium <= X1 * X1,
     Post => Sqrt (Medium) <= abs X1;

   procedure Lemma_Double_Square (M : Real)
   with
     Ghost,
     Pre  => M >= Small_Threshold and then M <= Big_Threshold,
     Post => M * M + M * M <= (2.0 * M) * (2.0 * M);

   procedure Lemma_Sum_Of_Two (A, B, P : Real)
   with
     Ghost,
     Pre  => 0.0 <= A and then A <= P and then 0.0 <= B and then B <= P and then P <= Bounds.Large,
     Post => A + B <= P + P;

   procedure Lemma_Close (Applies : Boolean; Count : Positive; Alpha : Real; X : Real_Vector; Largest, Beta : Real)
   with
     Ghost,
     Pre  =>
       X'First = 1
       and then Count - 1 <= X'Last
       and then abs Alpha <= Entry_Limit
       and then (for all I in 1 .. Count - 1 => abs X (I) <= Entry_Limit)
       and then
         (if Applies
          then Is_Largest (Count, Alpha, X, Largest) and then (Count > 3 or else abs Beta <= 4.0 * Largest)),
     Post => (if Applies then Is_Close (Count, Alpha, X, Beta));
   --  What the test of Beta against the largest magnitude establishes.

   procedure Lemma_Double_Squares (X1, X2, M : Real)
   with
     Ghost,
     Pre  => abs X1 <= M and then abs X2 <= M and then M >= Small_Threshold and then M <= Big_Threshold,
     Post => X1 * X1 + X2 * X2 <= (2.0 * M) * (2.0 * M);

   procedure Lemma_Norm_Of_Two (X1, X2, Medium : Real)
   with
     Ghost,
     Pre  =>
       Is_Medium_Or_Zero (X1)
       and then Is_Medium_Or_Zero (X2)
       and then Medium > 0.0
       and then Medium <= X1 * X1 + X2 * X2,
     Post => Sqrt (Medium) <= 2.0 * Real'Max (abs X1, abs X2);
   --  The norm of two elements is within twice the larger.

   procedure Lemma_Beta_Within (Alpha, Xnorm, Beta, Tail : Real)
   with
     Ghost,
     Pre  =>
       abs Alpha <= Entry_Limit
       and then Tail >= 0.0
       and then Tail <= Entry_Limit
       and then Xnorm >= 0.0
       and then Xnorm <= 2.0 * Tail
       and then abs Beta <= 2.0 * Real'Max (abs Alpha, Xnorm),
     Post => abs Beta <= 4.0 * Real'Max (abs Alpha, Tail);

   procedure Lemma_Zero_Vector (Plain : Boolean; X : Real_Vector; Last : Natural; Medium : Real)
   with
     Ghost,
     Pre  =>
       X'First = 1
       and then Last <= X'Last
       and then
         (if Plain
          then
            Medium >= 0.0
            and then Sqrt (Medium) = 0.0
            and then (for all I in 1 .. Last => Is_Medium_Or_Zero (X (I)) and then Medium >= X (I) * X (I))),
     Post => (if Plain then (for all I in 1 .. Last => X (I) = 0.0));
   --  When no element of X needed scaling for its square, so that the norm of X is the root of the sum Medium of
   --  the squares, and the norm is zero, every element is zero.

   procedure Lemma_Plain
     (Plain : Boolean; Count : Positive; Alpha : Real; X : Real_Vector; Medium, Xnorm, Beta : Real)
   with
     Ghost,
     Pre  =>
       X'First = 1
       and then Count - 1 <= X'Last
       and then abs Alpha <= Entry_Limit
       and then (for all I in 1 .. Count - 1 => abs X (I) <= Entry_Limit)
       and then Xnorm >= 0.0
       and then Xnorm <= Norm_Limit
       and then abs Beta <= 2.0 * Real'Max (abs Alpha, Xnorm)
       and then
         (if Plain
          then
            Medium > 0.0
            and then Xnorm = Sqrt (Medium)
            and then (for all I in 1 .. Count - 1 => Is_Medium_Or_Zero (X (I)) and then Medium >= X (I) * X (I))
            and then (if Count = 2 then Medium <= X (1) * X (1))
            and then (if Count = 3 then Medium <= X (1) * X (1) + X (2) * X (2))),
     Post =>
       (if Plain then (for all I in 1 .. Count - 1 => abs X (I) <= Xnorm) and then Is_Close (Count, Alpha, X, Beta));
   --  What follows when no element of X needed scaling for its square, so that the norm Xnorm of X is the root of
   --  the sum Medium of the squares: no element is larger than the norm, and Beta, the hypotenuse of Alpha and the
   --  norm, is close to the largest magnitude.

   procedure Lemma_Divisor (Alpha, Beta : Real)
   with
     Ghost,
     Pre  =>
       abs Alpha <= abs Beta
       and then abs Beta <= Beta_Limit
       and then abs Beta >= Rescaling_Threshold
       and then Is_Opposed (Alpha, Beta),
     Post =>
       abs (Alpha - Beta) >= abs Beta
       and then abs (Alpha - Beta) <= 2.0 * Beta_Limit
       and then abs (1.0 / (Alpha - Beta)) <= Rescaling_Factor;
   --  Alpha and Beta do not cancel, so the reciprocal of their difference is no larger than that of Beta.

   procedure Lemma_Element_Is_Zero (V : Real) is null;

   procedure Lemma_Element_Within_Norm (V, Medium : Real) is
      A : constant Real := abs V;
   begin
      if V /= 0.0 then
         pragma Assert (A * A = V * V);
         Lemma_Sqrt_Is_Monotonic (A * A, Medium);
         Lemma_Sqrt_Of_Square (A);
      end if;
   end Lemma_Element_Within_Norm;

   procedure Lemma_Norm_Of_One (X1, Medium : Real) is
      A : constant Real := abs X1;
   begin
      pragma Assert (X1 /= 0.0);
      pragma Assert (A * A = X1 * X1);
      Lemma_Sqrt_Is_Monotonic (Medium, A * A);
      Lemma_Sqrt_Of_Square (A);
   end Lemma_Norm_Of_One;

   procedure Lemma_Double_Square (M : Real) is null;

   procedure Lemma_Sum_Of_Two (A, B, P : Real) is null;

   procedure Lemma_Close (Applies : Boolean; Count : Positive; Alpha : Real; X : Real_Vector; Largest, Beta : Real)
   is null;

   procedure Lemma_Double_Squares (X1, X2, M : Real) is
      P : constant Real := M * M;
      Q : constant Real := (2.0 * M) * (2.0 * M);
   begin
      pragma Assert (X1 * X1 <= P);
      pragma Assert (X2 * X2 <= P);
      Lemma_Sum_Of_Two (X1 * X1, X2 * X2, P);
      pragma Assert (X1 * X1 + X2 * X2 <= P + P);
      Lemma_Double_Square (M);
      pragma Assert (P + P <= Q);
   end Lemma_Double_Squares;

   procedure Lemma_Norm_Of_Two (X1, X2, Medium : Real) is
      M : constant Real := Real'Max (abs X1, abs X2);
      D : constant Real := 2.0 * M;
   begin
      pragma Assert (M /= 0.0);
      pragma Assert (M >= Small_Threshold and then M <= Big_Threshold);
      Lemma_Double_Squares (X1, X2, M);
      Lemma_Sqrt_Is_Monotonic (Medium, D * D);
      Lemma_Sqrt_Of_Square (D);
   end Lemma_Norm_Of_Two;

   procedure Lemma_Beta_Within (Alpha, Xnorm, Beta, Tail : Real) is null;

   procedure Lemma_Zero_Vector (Plain : Boolean; X : Real_Vector; Last : Natural; Medium : Real) is
   begin
      if Plain then
         pragma Assert (Medium = 0.0);
         for I in 1 .. Last loop
            Lemma_Element_Is_Zero (X (I));
            pragma Loop_Invariant (for all J in 1 .. I => X (J) = 0.0);
         end loop;
      end if;
   end Lemma_Zero_Vector;

   procedure Lemma_Plain
     (Plain : Boolean; Count : Positive; Alpha : Real; X : Real_Vector; Medium, Xnorm, Beta : Real) is
   begin
      if Plain then
         for I in 1 .. Count - 1 loop
            Lemma_Element_Within_Norm (X (I), Medium);
            pragma Loop_Invariant (for all J in 1 .. I => abs X (J) <= Xnorm);
         end loop;
         if Count = 2 then
            Lemma_Norm_Of_One (X (1), Medium);
            Lemma_Beta_Within (Alpha, Xnorm, Beta, abs X (1));
         elsif Count = 3 then
            Lemma_Norm_Of_Two (X (1), X (2), Medium);
            Lemma_Beta_Within (Alpha, Xnorm, Beta, Real'Max (abs X (1), abs X (2)));
         end if;
      end if;
   end Lemma_Plain;

   procedure Lemma_Divisor (Alpha, Beta : Real) is null;

   -------------------------
   --  Largest_Magnitude  --
   -------------------------

   function Largest_Magnitude (Count : Positive; Alpha : Real; X : Real_Vector) return Real
   with
     Pre  =>
       X'First = 1
       and then Count - 1 <= X'Last
       and then abs Alpha <= Entry_Limit
       and then (for all I in 1 .. Count - 1 => abs X (I) <= Entry_Limit),
     Post => Is_Largest (Count, Alpha, X, Largest_Magnitude'Result);
   --  The largest magnitude among Alpha and X (1 .. Count - 1).

   function Largest_Magnitude (Count : Positive; Alpha : Real; X : Real_Vector) return Real is
      Largest : Real := abs Alpha;
   begin
      for I in 1 .. Count - 1 loop
         pragma Loop_Invariant (Largest >= 0.0 and then Largest <= Entry_Limit);
         pragma Loop_Invariant (if I = 1 then Largest = abs Alpha);
         pragma Loop_Invariant (if I = 2 then Largest = Real'Max (abs Alpha, abs X (1)));
         Largest := Real'Max (Largest, abs X (I));
      end loop;
      return Largest;
   end Largest_Magnitude;

   ----------------
   --  Scale_Up  --
   ----------------

   procedure Scale_Up
     (Count      : Positive;
      Alpha      : in out Real;
      Beta       : Real;
      X          : in out Real_Vector;
      Rescalings : out Natural;
      Bounded    : out Boolean)
   with
     Pre  =>
       X'First = 1
       and then Count - 1 <= X'Last
       and then abs Alpha <= abs Beta
       and then abs Beta < Rescaling_Threshold,
     Post =>
       (for all I in Count .. X'Last => X (I) = X'Old (I))
       and then Rescalings <= Maximum_Rescalings
       and then
         (if Bounded
          then abs Alpha <= 1.0 and then (for all I in 1 .. Count - 1 => abs X (I) <= Rescaling_Factor));
   --  Scale Alpha, Beta and X (1 .. Count - 1) up by Rescaling_Factor until Beta reaches Rescaling_Threshold, and
   --  count how many times in Rescalings.
   --
   --  Bounded is False when an element of X is too large to scale: X is then partly scaled.

   procedure Scale_Up
     (Count      : Positive;
      Alpha      : in out Real;
      Beta       : Real;
      X          : in out Real_Vector;
      Rescalings : out Natural;
      Bounded    : out Boolean)
   is
      Scaled_Beta : Real := Beta;
   begin
      Rescalings := 0;
      Bounded := True;
      loop
         pragma Loop_Invariant (Rescalings < Maximum_Rescalings);
         pragma Loop_Invariant (Bounded);
         pragma Loop_Invariant (abs Alpha <= abs Scaled_Beta and then abs Scaled_Beta < Rescaling_Threshold);
         pragma Loop_Invariant (for all J in Count .. X'Last => X (J) = X'Loop_Entry (J));
         pragma Loop_Variant (Increases => Rescalings);
         Rescalings := Rescalings + 1;
         for I in 1 .. Count - 1 loop
            pragma Loop_Invariant (for all J in 1 .. I - 1 => abs X (J) <= Rescaling_Factor);
            pragma Loop_Invariant (for all J in Count .. X'Last => X (J) = X'Loop_Entry (J));
            --  No element is larger than the norm, which is at most Beta.
            if abs X (I) > 1.0 then
               Bounded := False;
               return;
            end if;
            X (I) := Rescaling_Factor * X (I);
         end loop;
         Bounds.Lemma_Abs_Mult (Alpha, Rescaling_Factor);
         Bounds.Lemma_Abs_Mult (Scaled_Beta, Rescaling_Factor);
         Bounds.Lemma_Mult_Is_Monotonic (abs Alpha, abs Scaled_Beta, Rescaling_Factor);
         Scaled_Beta := Scaled_Beta * Rescaling_Factor;
         Alpha := Alpha * Rescaling_Factor;
         pragma Assert (abs Alpha <= abs Scaled_Beta);
         pragma Assert (abs Alpha <= 1.0);
         exit when not (abs Scaled_Beta < Rescaling_Threshold and then Rescalings < Maximum_Rescalings);
      end loop;
   end Scale_Up;

   ----------------------
   --  Form_Reflector  --
   ----------------------

   procedure Form_Reflector
     (Count : Natural; Alpha : in out Real; X : in out Real_Vector; Tau : out Real; Bounded : out Boolean)
   with
     Inline,
     Pre  =>
       X'First = 1
       and then Count - 1 <= X'Last
       and then Count <= Maximum_Order
       and then abs Alpha <= Entry_Limit
       and then (for all I in 1 .. Count - 1 => abs X (I) <= Entry_Limit),
     Post => Is_Reflector (Count, Alpha'Old, X'Old, Alpha, X, Tau, Bounded);
   --  Generate, with its postcondition stated as Is_Reflector: the proof shows that at each return on its own,
   --  away from the arithmetic of the other returns.

   procedure Form_Reflector
     (Count : Natural; Alpha : in out Real; X : in out Real_Vector; Tau : out Real; Bounded : out Boolean)
   is
      Sums                         : Scaled_Sums;
      Beta, Xnorm, Divisor, Factor : Real;
      Scaled                       : Real;
      Largest                      : Real := 0.0;
      Rescalings                   : Natural := 0;
      Plain                        : Boolean;

      Alpha_Before : constant Real := Alpha
      with Ghost;
      X_Before     : constant Real_Vector := X
      with Ghost;
   begin
      Bounded := True;
      Tau := 0.0;
      if Count <= 1 then
         pragma Assert (Is_Kept (Count, X_Before, X));
         pragma Assert (Is_Reflector (Count, Alpha_Before, X_Before, Alpha, X, Tau, Bounded));
         return;
      end if;

      Sums := Sum_Of_Squares (X, Count - 1);
      Xnorm := Root (Sums);
      if Xnorm = 0.0 then
         --  H is the identity. A norm of zero means that every element is zero, which the proof follows only when
         --  no element needed scaling for its square: the scaled sums are not zero otherwise.
         Bounded := Is_Plain (Sums);
         Lemma_Zero_Vector (Bounded, X, Count - 1, Sums.Medium);
         pragma Assert (if Bounded then (for all I in 1 .. Count - 1 => abs X (I) <= 2.0));
         pragma Assert (Is_Close (Count, Alpha_Before, X_Before, Alpha));
         pragma Assert (Is_Kept (Count, X_Before, X));
         pragma Assert (Is_Reflector (Count, Alpha_Before, X_Before, Alpha, X, Tau, Bounded));
         return;
      end if;

      --  When no element needed scaling for its square, the norm is the plain root of the sum of the squares, and
      --  the proof bounds the reflector from that. Otherwise the bounds are tested below.
      Plain := Is_Plain (Sums);
      pragma Assert (if Plain then Sums.Medium > 0.0);

      Beta := -Sign (Hypotenuse (Alpha, Xnorm), Alpha);
      pragma Assert (abs Beta >= abs Alpha and then abs Beta >= Xnorm);
      pragma Assert (abs Beta <= 2.0 * Real'Max (abs Alpha, Xnorm));
      pragma Assert (abs Beta <= Beta_Limit);
      pragma Assert (Beta = 0.0 or else Is_Opposed (Alpha, Beta));
      Lemma_Plain (Plain, Count, Alpha, X, Sums.Medium, Xnorm, Beta);

      if abs Beta < Rescaling_Threshold or else not Plain then
         Largest := Largest_Magnitude (Count, Alpha, X);
      end if;

      if abs Beta < Rescaling_Threshold then
         Plain := False;
         Scale_Up (Count, Alpha, Beta, X, Rescalings, Bounded);
         if not Bounded then
            pragma Assert (Is_Kept (Count, X_Before, X));
            pragma Assert (Is_Reflector (Count, Alpha_Before, X_Before, Alpha, X, Tau, Bounded));
            return;
         end if;
         Xnorm := Root (Sum_Of_Squares (X, Count - 1));
         Beta := -Sign (Hypotenuse (Alpha, Xnorm), Alpha);
         pragma Assert (abs Beta >= abs Alpha);
         pragma Assert (abs Beta <= Beta_Limit);
         pragma Assert (Beta = 0.0 or else Is_Opposed (Alpha, Beta));

         --  The vector is not zero, so Beta was at least the smallest positive number, 2**-1074, and one scaling
         --  brings it to 2**-105: this Beta, which is the scaled one but for rounding, is far above the threshold.
         --  The proof does not follow the norm that closely, and needs the bound for the reciprocal of
         --  Alpha - Beta, which is at least Beta in magnitude.
         if abs Beta < Rescaling_Threshold then
            Bounded := False;
            pragma Assert (Is_Kept (Count, X_Before, X));
            pragma Assert (Is_Reflector (Count, Alpha_Before, X_Before, Alpha, X, Tau, Bounded));
            return;
         end if;
      end if;

      pragma Assert (abs Alpha <= abs Beta and then abs Beta <= Beta_Limit and then abs Beta >= Rescaling_Threshold);
      pragma Assert (Is_Opposed (Alpha, Beta));
      pragma Assert (for all J in Count .. X'Last => X (J) = X_Before (J));
      pragma Assert (Largest >= 0.0 and then Largest <= Entry_Limit);
      pragma Assert (if not Plain then Is_Largest (Count, Alpha_Before, X_Before, Largest));
      pragma
        Assert
          (if Plain
             then
               Rescalings = 0
               and then Xnorm <= abs Beta
               and then (for all J in 1 .. Count - 1 => abs X (J) <= Xnorm)
               and then Is_Close (Count, Alpha_Before, X_Before, Beta));

      Bounds.Lemma_Tau (Alpha, Beta);
      Lemma_Divisor (Alpha, Beta);
      Tau := (Beta - Alpha) / Beta;
      Divisor := Alpha - Beta;
      Factor := 1.0 / Divisor;
      if Plain then
         for I in 1 .. Count - 1 loop
            Bounds.Lemma_Reciprocal_Product (X (I), Divisor);
            X (I) := Factor * X (I);
            pragma Loop_Invariant (for all J in 1 .. I => abs X (J) <= 2.0);
            pragma Loop_Invariant (for all J in I + 1 .. Count - 1 => abs X (J) <= Xnorm);
            pragma Loop_Invariant (for all J in Count .. X'Last => X (J) = X_Before (J));
         end loop;
         pragma Assert (for all J in 1 .. Count - 1 => abs X (J) <= 2.0);
         pragma Assert (for all J in Count .. X'Last => X (J) = X_Before (J));
      else
         for I in 1 .. Count - 1 loop
            pragma Loop_Invariant (Bounded and then Tau >= 1.0 and then Tau <= 2.0);
            pragma Loop_Invariant (for all J in 1 .. I - 1 => abs X (J) <= 2.0);
            pragma Loop_Invariant (for all J in Count .. X'Last => X (J) = X_Before (J));

            --  No element is larger than the norm, which is at most the divisor.
            if abs X (I) > abs Divisor then
               Bounded := False;
               Tau := 0.0;
               pragma Assert (Is_Kept (Count, X_Before, X));
               pragma Assert (Is_Reflector (Count, Alpha_Before, X_Before, Alpha, X, Tau, Bounded));
               return;
            end if;
            Bounds.Lemma_Reciprocal_Product (X (I), Divisor);
            Scaled := Factor * X (I);
            pragma
              Assert_And_Cut
                (abs Scaled <= 2.0
                 and then Bounded
                 and then Tau >= 1.0
                 and then Tau <= 2.0
                 and then (for all J in 1 .. I - 1 => abs X (J) <= 2.0)
                 and then (for all J in Count .. X'Last => X (J) = X_Before (J)));
            X (I) := Scaled;
            pragma Assert (for all J in 1 .. I => abs X (J) <= 2.0);
            pragma Assert (for all J in Count .. X'Last => X (J) = X_Before (J));
         end loop;
         pragma Assert (for all J in 1 .. Count - 1 => abs X (J) <= 2.0);
         pragma Assert (for all J in Count .. X'Last => X (J) = X_Before (J));
      end if;

      pragma Assert (Bounded and then Tau >= 1.0 and then Tau <= 2.0);
      pragma Assert (for all J in 1 .. Count - 1 => abs X (J) <= 2.0);
      pragma Assert (for all J in Count .. X'Last => X (J) = X_Before (J));
      pragma Assert (Is_Kept (Count, X_Before, X));

      for J in 1 .. Rescalings loop
         pragma Loop_Invariant (abs Beta <= Beta_Limit and then not Plain);
         Bounds.Lemma_Shrink (Beta, Rescaling_Threshold);
         Beta := Beta * Rescaling_Threshold;
         pragma Assert (abs Beta <= Beta_Limit);
      end loop;
      pragma Assert (if Plain then Is_Close (Count, Alpha_Before, X_Before, Beta));
      if not Plain and then Count <= 3 and then abs Beta > 4.0 * Largest then
         --  Beta is the norm of at most three numbers, none larger than Largest.
         Bounded := False;
         Tau := 0.0;
         pragma Assert (Is_Kept (Count, X_Before, X));
         pragma Assert (Is_Reflector (Count, Alpha_Before, X_Before, Alpha, X, Tau, Bounded));
         return;
      end if;
      pragma Assert (abs Beta <= Beta_Limit);
      Lemma_Close (not Plain, Count, Alpha_Before, X_Before, Largest, Beta);
      pragma Assert (Is_Close (Count, Alpha_Before, X_Before, Beta));
      Alpha := Beta;
      pragma Assert (Is_Kept (Count, X_Before, X));
      pragma Assert (Is_Reflector (Count, Alpha_Before, X_Before, Alpha, X, Tau, Bounded));
   end Form_Reflector;

   ----------------
   --  Generate  --
   ----------------

   procedure Generate
     (Count : Natural; Alpha : in out Real; X : in out Real_Vector; Tau : out Real; Bounded : out Boolean) is
   begin
      Form_Reflector (Count, Alpha, X, Tau, Bounded);
   end Generate;

end Prunt.LAPACK.Reflectors;
