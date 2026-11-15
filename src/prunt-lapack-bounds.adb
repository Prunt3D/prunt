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

package body Prunt.LAPACK.Bounds
  with SPARK_Mode
is

   procedure Lemma_Mult_Is_Monotonic (A, B, C : Real) is null;

   procedure Lemma_Mult_Is_Monotonic_Right (A, B, C : Real) is null;

   procedure Lemma_Abs_Mult (X, Y : Real) is null;

   procedure Lemma_Product (X, Y, Bx, By : Real) is
   begin
      Lemma_Abs_Mult (X, Y);
      Lemma_Mult_Is_Monotonic (abs X, Bx, abs Y);
      Lemma_Mult_Is_Monotonic_Right (abs Y, By, Bx);
      pragma Assert (abs (X * Y) <= Bx * By);
      pragma Assert (Y * X = X * Y);
   end Lemma_Product;

   procedure Lemma_Shrink (X, M : Real) is
      Ax : constant Real := abs X;
      Am : constant Real := abs M;
   begin
      Lemma_Mult_Is_Monotonic (Am, 1.0, Ax);
      pragma Assert (Am * Ax <= Ax);
      pragma Assert (abs (X * M) = Am * Ax);
   end Lemma_Shrink;

   procedure Lemma_Quotient_Within_One (X, S : Real) is null;

   procedure Lemma_Reciprocal_Product (X, D : Real) is
      R : constant Real := 1.0 / D;
   begin
      pragma Assert (abs X * abs R <= 2.0);
      pragma Assert (abs (X * R) = abs X * abs R);
      pragma Assert (R * X = X * R);
   end Lemma_Reciprocal_Product;

   procedure Lemma_Tau (Alpha, Beta : Real) is
      Q : constant Real := (Beta - Alpha) / Beta;
   begin
      pragma Assert (abs (Beta - Alpha) <= 2.0 * abs Beta);
      pragma Assert (abs Q <= 2.0);
      pragma Assert (Q <= 2.0);
      pragma Assert (Q >= 1.0);
   end Lemma_Tau;

end Prunt.LAPACK.Bounds;
