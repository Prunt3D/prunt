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

--  Reduction to Hessenberg form: DGEHD2, which is what DGEHRD runs for a matrix of order Maximum_Order or less.

package Prunt.LAPACK.Hessenberg_Reduction
  with SPARK_Mode, Always_Terminates
is

   procedure Reduce (N : Order; Ilo : Positive; Ihi : Natural; A : in out Real_Matrix; Ok : out Boolean)
   with
     Pre =>
       Holds (N, A) and then Ihi <= N and then Ilo <= Ihi + 1 and then Block_Within (A, Ilo, Ihi, Balanced_Limit);
   --  DGEHD2 (N, Ilo, Ihi, A): reduce rows and columns Ilo .. Ihi of A to upper Hessenberg form by an orthogonal
   --  similarity transformation. On return the elements below the first subdiagonal hold the reflector vectors,
   --  not zeros, as in the reference implementation.
   --
   --  Only the block of rows and columns Ilo .. Ihi is transformed. The reference implementation also transforms
   --  rows 1 .. Ilo - 1 and columns Ihi + 1 .. N of the rest of the matrix, which nothing reads again when only
   --  eigenvalues are wanted.
   --
   --  Ok is False when a magnitude which the proof does not bound failed its test at run time: A is then not to
   --  be used. See docs/spark_lapack.md.

end Prunt.LAPACK.Hessenberg_Reduction;
