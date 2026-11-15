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

--  The eigenvalues of a two by two block: DLANV2 without the rotation it also returns.

package Prunt.LAPACK.Schur_Blocks
  with SPARK_Mode, Always_Terminates
is

   Block_Entry_Limit : constant Real := 2#1.0#E920;

   procedure Eigenvalues_Of_Block
     (A, B, C, D : Real; Rt1r, Rt1i, Rt2r, Rt2i : out Real; Ok : out Boolean)
   with
     Pre  =>
       abs A <= Block_Entry_Limit
       and then abs B <= Block_Entry_Limit
       and then abs C <= Block_Entry_Limit
       and then abs D <= Block_Entry_Limit,
     Post => (if Ok then Rt1i >= 0.0 and then Rt2i = -Rt1i and then (if Rt1i /= 0.0 then Rt1r = Rt2r));
   --  DLANV2: the eigenvalues (Rt1r, Rt1i) and (Rt2r, Rt2i) of the matrix
   --
   --     [ A  B ]
   --     [ C  D ]
   --
   --  computed, as in the reference implementation, by reducing it to its standardized Schur form. That form and
   --  the rotation which gives it are not returned: only the Schur factorization and eigenvectors need them.
   --
   --  The eigenvalues are either both real or an exact complex conjugate pair with the first in the upper half
   --  plane.
   --
   --  Ok is False when a magnitude which the proof does not bound failed its test at run time: the eigenvalues
   --  are then not valid. See docs/spark_lapack.md.

end Prunt.LAPACK.Schur_Blocks;
