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

--  The eigenvalues of an upper Hessenberg matrix: DLAHQR with WANTT = WANTZ = .FALSE., which is what DHSEQR runs
--  for a matrix of order Maximum_Order or less.

package Prunt.LAPACK.Hessenberg_QR
  with SPARK_Mode, Always_Terminates
is

   procedure Eigenvalues
     (N      : Order;
      Ilo    : Positive;
      Ihi    : Natural;
      H      : in out Real_Matrix;
      Wr, Wi : in out Real_Vector;
      Info   : out Natural;
      Ok     : out Boolean)
   with
     Pre  => Holds (N, H) and then Holds (N, Wr) and then Holds (N, Wi) and then Ilo <= Ihi and then Ihi <= N,
     Post =>
       (Info = 0 or else Info in Ilo .. Ihi)
       and then (for all J in Wr'Range => (if J < Ilo or else J > Ihi then Wr (J) = Wr'Old (J)))
       and then (for all J in Wi'Range => (if J < Ilo or else J > Ihi then Wi (J) = Wi'Old (J)))
       and then (if Ok and then Info = 0 then Is_Paired (Wr, Wi, Ilo, Ihi))
       and then (if Ok and then Info > 0 then Is_Paired (Wr, Wi, Info + 1, Ihi));
   --  DLAHQR (.FALSE., .FALSE., N, Ilo, Ihi, H, Wr, Wi): the eigenvalues of rows and columns Ilo .. Ihi of the
   --  upper Hessenberg matrix H, by the double-shift QR algorithm. Elements of H below the first subdiagonal are
   --  not read. Info is zero when every eigenvalue was found. Otherwise the iteration did not converge:
   --  eigenvalues Info + 1 .. Ihi are in Wr and Wi and those of Ilo .. Info are missing.
   --
   --  Ok is False when a magnitude which the proof does not bound failed its test at run time, or where the
   --  reference implementation divides zero by zero: the eigenvalues are then not valid. See docs/spark_lapack.md.

end Prunt.LAPACK.Hessenberg_QR;
