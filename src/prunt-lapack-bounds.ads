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

--  Lemmas which bound the result of one or two floating-point operations.
--
--  The provers settle a bound on a single rounded operation, but rarely a chain of them, so the proofs of the
--  other units call these one step at a time. Every lemma here is proved: none is an assumption.

package Prunt.LAPACK.Bounds
  with SPARK_Mode, Ghost, Always_Terminates
is

   Large : constant Real := 2#1.0#E1000;
   --  Operands up to this size leave room for the small factors below.

   Small_Factor : constant Real := 1024.0;

   procedure Lemma_Mult_Is_Monotonic (A, B, C : Real)
   with
     Pre  => 0.0 <= A and then A <= B and then B <= Small_Factor and then 0.0 <= C and then C <= Large,
     Post => A * C <= B * C;

   procedure Lemma_Mult_Is_Monotonic_Right (A, B, C : Real)
   with
     Pre  => 0.0 <= A and then A <= B and then B <= Large and then 0.0 <= C and then C <= Small_Factor,
     Post => C * A <= C * B;

   procedure Lemma_Abs_Mult (X, Y : Real)
   with Pre => abs X <= Small_Factor and then abs Y <= Large, Post => abs (X * Y) = abs X * abs Y;

   procedure Lemma_Product (X, Y, Bx, By : Real)
   with
     Pre  => abs X <= Bx and then abs Y <= By and then Bx <= Small_Factor and then By <= Large,
     Post => abs (X * Y) <= Bx * By and then abs (Y * X) <= Bx * By;
   --  A product is within the product of the bounds of its factors.

   procedure Lemma_Shrink (X, M : Real)
   with Pre => abs M <= 1.0 and then abs X <= Large, Post => abs (X * M) <= abs X;
   --  A factor of at most one does not enlarge.

   procedure Lemma_Quotient_Within_One (X, S : Real)
   with Pre => S > 0.0 and then abs X <= S, Post => abs (X / S) <= 1.0;

   procedure Lemma_Reciprocal_Product (X, D : Real)
   with
     Pre  => abs X <= abs D and then abs D >= 2#1.0#E-1000,
     Post => abs ((1.0 / D) * X) <= 2.0 and then abs (X * (1.0 / D)) <= 2.0;
   --  Scaling by a reciprocal, as DLARFG scales a reflector vector, gives two at most.

   procedure Lemma_Tau (Alpha, Beta : Real)
   with
     Pre  =>
       abs Alpha <= abs Beta
       and then abs Beta <= Large
       and then Beta /= 0.0
       and then ((Alpha >= 0.0 and then Beta < 0.0) or else (Alpha <= 0.0 and then Beta > 0.0)),
     Post => (Beta - Alpha) / Beta >= 1.0 and then (Beta - Alpha) / Beta <= 2.0;
   --  The scalar factor of a reflector is between one and two.

end Prunt.LAPACK.Bounds;
