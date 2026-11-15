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

--  The eigenvalues of a general real matrix: DGEEV with JOBVL = JOBVR = 'N'.

package Prunt.LAPACK.Eigenvalues
  with SPARK_Mode, Always_Terminates
is

   Out_Of_Range : constant Integer := -1;
   --  The value of Info when the port gave up: see below.

   procedure Solve (N : Order; A : in out Real_Matrix; Wr, Wi : out Real_Vector; Info : out Integer)
   with
     Pre  =>
       Holds (N, A) and then Wr'First = 1 and then Wr'Last >= N and then Wi'First = 1 and then Wi'Last >= N,
     Post => Info in Out_Of_Range .. N and then (if Info = 0 then Is_Paired (Wr, Wi, 1, N));
   --  DGEEV ('N', 'N', N, A, Wr, Wi): the eigenvalues of the leading block of order N of A. Eigenvalue J is
   --  Wr (J) + i * Wi (J). A complex conjugate pair is stored in consecutive elements, the one with the positive
   --  imaginary part first. A is overwritten.
   --
   --  Info is zero when every eigenvalue was found. A positive Info means the QR algorithm did not converge:
   --  eigenvalues Info + 1 .. N have converged and the others are not valid. Where the reference implementation
   --  then tries a second algorithm, the port does not: see docs/spark_lapack.md.
   --
   --  Info is Out_Of_Range, and no eigenvalue is valid, when an eigenvalue is too large for the type, where the
   --  reference implementation returns an infinity, or when a magnitude which the proof does not bound failed its
   --  test at run time, which no matrix is known to cause: see docs/spark_lapack.md.

end Prunt.LAPACK.Eigenvalues;
