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

--  Elementary reflectors: DLARFG.

package Prunt.LAPACK.Reflectors
  with SPARK_Mode, Always_Terminates
is

   Reflector_Limit : constant Real := 2#1.0#E986;
   --  The first element of a reflected vector whose elements were within Entry_Limit is within this.

   procedure Generate
     (Count : Natural; Alpha : in out Real; X : in out Real_Vector; Tau : out Real; Bounded : out Boolean)
   with
     Pre  =>
       X'First = 1
       and then Count - 1 <= X'Last
       and then Count <= Maximum_Order
       and then abs Alpha <= Entry_Limit
       and then (for all I in 1 .. Count - 1 => abs X (I) <= Entry_Limit),
     Post =>
       (for all I in X'Range => (if I >= Count then X (I) = X'Old (I)))
       and then
         (if Bounded
          then
            (Tau = 0.0 or else (Tau >= 1.0 and then Tau <= 2.0))
            and then abs Alpha <= Reflector_Limit
            and then (if Tau = 0.0 then Alpha = Alpha'Old and then (for all I in 1 .. Count - 1 => X (I) = 0.0))
            and then (for all I in 1 .. Count - 1 => abs X (I) <= 2.0)
            and then (if Count = 2 then abs Alpha <= 4.0 * Real'Max (abs Alpha'Old, abs X'Old (1)))
            and then
              (if Count = 3
               then abs Alpha <= 4.0 * Real'Max (abs Alpha'Old, Real'Max (abs X'Old (1), abs X'Old (2)))));
   --  DLARFG (Count, Alpha, X, 1, Tau): generate the reflector H = I - Tau * v * v' of order Count which maps
   --  (Alpha, X (1 .. Count - 1)) to (Beta, 0, ..., 0). On return Alpha is Beta and X holds v (2 .. Count); v (1)
   --  is one. Tau is zero, and H the identity, when X is zero.
   --
   --  Bounded is False when a magnitude which the proof does not bound failed its test at run time: the reflector
   --  is then not to be used. See docs/spark_lapack.md.

end Prunt.LAPACK.Reflectors;
