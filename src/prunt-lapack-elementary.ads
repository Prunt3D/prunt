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

--  The two operations Prunt.LAPACK needs which GNATprove does not model: the square root and Fortran's SIGN.
--
--  Nothing here is proved. The contracts and the two lemmas state what IEEE 754 guarantees of a correctly rounded
--  square root in binary64 and of copying a sign bit. They are the trusted base of Prunt.LAPACK, which the tests of
--  Prunt.LAPACK.Test check on the machine they run on.

package Prunt.LAPACK.Elementary
  with SPARK_Mode, Always_Terminates
is

   function Sqrt (X : Real) return Real
   with
     Global => null,
     Pre    => X >= 0.0,
     Post   =>
       Sqrt'Result >= 0.0
       and then (if X > 0.0 then Sqrt'Result > 0.0)
       and then (if X = 0.0 then Sqrt'Result = 0.0);
   --  The correctly rounded square root of X, which is zero only for zero.

   Exact_Square_Minimum : constant Real := 2#1.0#E-511;
   Exact_Square_Maximum : constant Real := 2#1.0#E511;
   --  The square of a number in this range neither overflows nor is denormalized.

   procedure Lemma_Sqrt_Is_Monotonic (X, Y : Real)
   with Ghost, Global => null, Pre => 0.0 <= X and then X <= Y, Post => Sqrt (X) <= Sqrt (Y);
   --  Rounding is monotonic, so the rounded root of a larger number is not smaller.

   procedure Lemma_Sqrt_Of_Square (X : Real)
   with
     Ghost,
     Global => null,
     Pre    => X >= Exact_Square_Minimum and then X <= Exact_Square_Maximum,
     Post   => Sqrt (X * X) = X;
   --  In radix two with rounding to nearest, the rounded root of the rounded square of X is X. (S. Boldo, "Stupid
   --  is as stupid does: taking the square root of the square of a floating-point number", 2015.)

   function Sign (Magnitude, Source : Real) return Real
   with
     Global => null,
     Post   =>
       abs Sign'Result = abs Magnitude
       and then (if Source > 0.0 then Sign'Result >= 0.0)
       and then (if Source < 0.0 then Sign'Result <= 0.0);
   --  Fortran's SIGN (Magnitude, Source): the magnitude of the first argument with the sign bit of the second, so
   --  that a negative zero Source gives a negative result.

private
   pragma SPARK_Mode (Off);

   function Builtin_Sqrt (X : Real) return Real
   with Import, Convention => Intrinsic, External_Name => "__builtin_sqrt";

   function Sqrt (X : Real) return Real
   is (Builtin_Sqrt (X));

   procedure Lemma_Sqrt_Is_Monotonic (X, Y : Real) is null;

   procedure Lemma_Sqrt_Of_Square (X : Real) is null;

   function Sign (Magnitude, Source : Real) return Real
   is (Real'Copy_Sign (Magnitude, Source));

end Prunt.LAPACK.Elementary;
