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

--  Norms: DLANGE with NORM = 'M', DNRM2 and DLAPY2.

with Prunt.LAPACK.Elementary;

package Prunt.LAPACK.Norms
  with SPARK_Mode, Always_Terminates
is

   function Block_Max (A : Real_Matrix; First : Positive; Last : Natural) return Real
   with
     Pre  => A'First (1) = 1 and then A'First (2) = 1 and then Last <= A'Last (1) and then Last <= A'Last (2),
     Post => Block_Max'Result >= 0.0 and then Block_Within (A, First, Last, Block_Max'Result);
   --  DLANGE ('M') of rows and columns First .. Last of A: the largest magnitude of an element.

   Hypotenuse_Limit : constant Real := 2#1.0#E1000;

   function Hypotenuse (X, Y : Real) return Real
   with
     Pre  => abs X <= Hypotenuse_Limit and then abs Y <= Hypotenuse_Limit,
     Post =>
       Hypotenuse'Result >= abs X
       and then Hypotenuse'Result >= abs Y
       and then Hypotenuse'Result <= 2.0 * Real'Max (abs X, abs Y);
   --  DLAPY2: sqrt (X**2 + Y**2), formed so that the squares cannot overflow.

   ------------
   --  DNRM2 --
   ------------

   --  DNRM2 is Blue's algorithm: the squares of large elements are accumulated scaled down and those of small
   --  ones scaled up, so that no square overflows or loses its value to underflow. It is split in two here, the
   --  accumulation and the combination of the accumulators, so that a caller can tell when no scaling took place:
   --  the norm is then the plain root of the sum of the squares, which the proofs can follow.

   Small_Threshold : constant Real := 2#1.0#E-511;
   Big_Threshold   : constant Real := 2#1.0#E486;
   --  An element whose magnitude is between these is of medium size: its square is accumulated unscaled.

   type Scaled_Sums is record
      Big, Medium, Small : Real;
      --  The sums of the squares of the big elements, scaled down, of the medium ones, and of the small ones,
      --  scaled up.

      Not_Big : Boolean;
      --  No element was big.
   end record;

   Big_Sum_Limit    : constant Real := 2#1.0#E889;
   Medium_Sum_Limit : constant Real := 2#1.0#E977;
   Small_Sum_Limit  : constant Real := 2#1.0#E57;
   --  The accumulators of at most Maximum_Order elements within Entry_Limit are within these.

   function Is_Valid (Sums : Scaled_Sums) return Boolean
   is (Sums.Big >= 0.0
       and then Sums.Big <= Big_Sum_Limit
       and then Sums.Medium >= 0.0
       and then Sums.Medium <= Medium_Sum_Limit
       and then Sums.Small >= 0.0
       and then Sums.Small <= Small_Sum_Limit);

   function Is_Plain (Sums : Scaled_Sums) return Boolean
   is (Sums.Big = 0.0 and then Sums.Small = 0.0);
   --  Every element was of medium size or zero, so that Medium is the sum of all the squares.

   function Is_Medium_Or_Zero (X : Real) return Boolean
   is (X = 0.0 or else (abs X >= Small_Threshold and then abs X <= Big_Threshold));

   function Sum_Of_Squares (X : Real_Vector; Count : Natural) return Scaled_Sums
   with
     Pre  =>
       X'First = 1
       and then Count <= X'Last
       and then Count <= Maximum_Order
       and then (for all I in 1 .. Count => abs X (I) <= Entry_Limit),
     Post =>
       Is_Valid (Sum_Of_Squares'Result)
       and then
         (if Is_Plain (Sum_Of_Squares'Result)
          then
            (for all I in 1 .. Count =>
               Is_Medium_Or_Zero (X (I)) and then Sum_Of_Squares'Result.Medium >= X (I) * X (I)))
       and then
         (if Is_Plain (Sum_Of_Squares'Result) and then Count = 1
          then Sum_Of_Squares'Result.Medium <= X (1) * X (1))
       and then
         (if Is_Plain (Sum_Of_Squares'Result) and then Count = 2
          then Sum_Of_Squares'Result.Medium <= X (1) * X (1) + X (2) * X (2));
   --  The first half of DNRM2 (Count, X, 1): the scaled sums of the squares of X (1 .. Count).

   Norm_Limit : constant Real := 2#1.0#E984;

   function Root (Sums : Scaled_Sums) return Real
   with
     Pre  => Is_Valid (Sums),
     Post =>
       Root'Result >= 0.0
       and then Root'Result <= Norm_Limit
       and then (if Is_Plain (Sums) then Root'Result = Elementary.Sqrt (Sums.Medium));
   --  The second half of DNRM2: the norm from the scaled sums.

   function Column_Norm (A : Real_Matrix; Column : Positive; First : Positive; Last : Natural) return Real
   with
     Pre  =>
       A'First (1) = 1
       and then A'First (2) = 1
       and then Column <= A'Last (2)
       and then Last <= A'Last (1)
       and then Last <= Maximum_Order
       and then (for all I in First .. Last => abs A (I, Column) <= Entry_Limit),
     Post => Column_Norm'Result >= 0.0 and then Column_Norm'Result <= Norm_Limit;
   --  DNRM2 of A (First .. Last, Column).

   function Row_Norm (A : Real_Matrix; Row : Positive; First : Positive; Last : Natural) return Real
   with
     Pre  =>
       A'First (1) = 1
       and then A'First (2) = 1
       and then Row <= A'Last (1)
       and then Last <= A'Last (2)
       and then Last <= Maximum_Order
       and then (for all J in First .. Last => abs A (Row, J) <= Entry_Limit),
     Post => Row_Norm'Result >= 0.0 and then Row_Norm'Result <= Norm_Limit;
   --  DNRM2 of A (Row, First .. Last).

end Prunt.LAPACK.Norms;
