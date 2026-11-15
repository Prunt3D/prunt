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

--  SPARK port of the LAPACK routines Prunt uses: the eigenvalues of a general real matrix, as computed by DGEEV
--  with JOBVL = JOBVR = 'N'.
--
--  The port performs the floating-point operations of the reference implementation (LAPACK 3.12.1) in the same
--  order, so on a target which rounds every operation on its own it returns the same bits. See
--  docs/spark_lapack.md for what is proved about it and where it differs.

package Prunt.LAPACK
  with SPARK_Mode, Preelaborate
is

   subtype Real is Long_Float;
   --  IEEE 754 binary64, the type LAPACK calls DOUBLE PRECISION.

   Maximum_Order : constant := 32;
   --  Up to this order the reference implementation reduces a matrix to Hessenberg form with its unblocked code
   --  and finds the eigenvalues with the small-bulge QR algorithm, which are the two algorithms ported here.
   --  Beyond it the reference implementation switches to blocked and multishift code which rounds differently.

   subtype Order is Natural range 0 .. Maximum_Order;
   subtype Index is Positive range 1 .. Maximum_Order;

   type Real_Vector is array (Positive range <>) of Real;
   type Real_Matrix is array (Positive range <>, Positive range <>) of Real;

   --  The machine constants of DLAMCH.

   Epsilon : constant Real := 2#1.0#E-53;
   --  DLAMCH ('E'), the relative rounding error.

   Precision : constant Real := 2#1.0#E-52;
   --  DLAMCH ('P'), Epsilon times the base.

   Safe_Minimum : constant Real := 2#1.0#E-1022;
   --  DLAMCH ('S'), the smallest number whose reciprocal does not overflow.

   function Holds (N : Order; A : Real_Matrix) return Boolean
   is (A'First (1) = 1 and then A'First (2) = 1 and then A'Last (1) >= N and then A'Last (2) >= N);
   --  A has a leading block of order N, indexed from one as in LAPACK.

   function Holds (N : Order; X : Real_Vector) return Boolean
   is (X'First = 1 and then X'Last >= N);
   --  X has N leading elements, indexed from one.

   --  Magnitudes. LAPACK relies on arithmetic which carries on after an overflow. SPARK has no infinities, so the
   --  port shows that nothing overflows, from bounds on the magnitude of the elements it computes with. Where it
   --  cannot derive a bound it tests one at run time, and gives up when the test fails: see docs/spark_lapack.md.

   Balanced_Limit : constant Real := 2#1.0#E972;
   --  No element of a matrix is larger than this once it is scaled and balanced. Balancing itself can bring an
   --  element outside the balanced block close to this.

   Entry_Limit : constant Real := 2#1.0#E980;
   --  The largest magnitude a norm or a reflector is formed from.

   function Block_Within (A : Real_Matrix; First : Positive; Last : Natural; Limit : Real) return Boolean
   is (for all I in First .. Last => (for all J in First .. Last => abs A (I, J) <= Limit))
   with Ghost, Pre => A'First (1) = 1 and then A'First (2) = 1 and then Last <= A'Last (1) and then Last <= A'Last (2);
   --  No element of rows and columns First .. Last of A is larger than Limit.

   function Is_Paired (Wr, Wi : Real_Vector; First : Positive; Last : Natural) return Boolean
   is (for all J in First .. Last =>
         Wi (J) = 0.0
         or else (Wi (J) > 0.0 and then J < Last and then Wr (J + 1) = Wr (J) and then Wi (J + 1) = -Wi (J))
         or else (Wi (J) < 0.0 and then J > First and then Wr (J - 1) = Wr (J) and then Wi (J - 1) = -Wi (J)))
   with Ghost, Pre => Wr'First = 1 and then Wi'First = 1 and then Last <= Wr'Last and then Last <= Wi'Last;
   --  The eigenvalues Wr (J) + i * Wi (J), J in First .. Last, which are not real come in pairs of exact complex
   --  conjugates, stored next to one another with the one in the upper half plane first.

end Prunt.LAPACK;
