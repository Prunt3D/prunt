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

--  Balancing: DGEBAL with JOB = 'B'.

package Prunt.LAPACK.Balancing
  with SPARK_Mode, Always_Terminates
is

   procedure Balance (N : Order; A : in out Real_Matrix; Ilo : out Positive; Ihi : out Natural; Ok : out Boolean)
   with
     Pre  => Holds (N, A) and then Block_Within (A, 1, N, Balanced_Limit),
     Post => Ilo <= Ihi + 1 and then Ihi <= N and then (if Ok then Block_Within (A, 1, N, Balanced_Limit));
   --  DGEBAL ('B', N, A): permute A by a similarity transformation so that rows Ihi + 1 .. N and columns
   --  1 .. Ilo - 1 are upper triangular, which isolates the eigenvalues on their diagonal, then scale rows and
   --  columns Ilo .. Ihi by powers of two to make their norms as close as possible. The permutation and the scale
   --  factors are not returned: only eigenvectors need them.
   --
   --  Ilo = Ihi + 1 when every eigenvalue was isolated.
   --
   --  Ok is False when a magnitude which the proof does not bound failed its test at run time: A is then not to
   --  be used. See docs/spark_lapack.md.

end Prunt.LAPACK.Balancing;
