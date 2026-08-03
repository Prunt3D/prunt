--  Part of the Prunt Motion Controller
--
--  Copyright (C) 2026 Liam Powell (liam@prunt3d.com)
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
--  documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
--  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
--  permit persons to whom the Software is furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice (including the next paragraph) shall be included in all
--  copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
--  THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
--  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
--  SOFTWARE.
--------------------------------------------------

with Interfaces.Fortran;

package Prunt.LAPACK is
   --  GPRBuild selects the Fortran driver when these routines are linked. The
   --  embedded sources do not use its runtime, so discard the implicit but
   --  otherwise unused libgfortran dependency.
   pragma Linker_Options ("-Wl,--as-needed");

   subtype Fortran_Integer is Interfaces.Fortran.Fortran_Integer;
   subtype Double_Precision is Interfaces.Fortran.Double_Precision;

   type Double_Precision_Vector is array (Fortran_Integer range <>) of Double_Precision;
   pragma Convention (Fortran, Double_Precision_Vector);

   type Fortran_Integer_Vector is array (Fortran_Integer range <>) of Fortran_Integer;
   pragma Convention (Fortran, Fortran_Integer_Vector);

   type Double_Precision_Matrix is array (Fortran_Integer range <>, Fortran_Integer range <>) of Double_Precision;
   pragma Convention (Fortran, Double_Precision_Matrix);

   procedure DGEEV
     (Jobvl : Character;
      Jobvr : Character;
      N     : Fortran_Integer;
      A     : in out Double_Precision_Matrix;
      Lda   : Fortran_Integer;
      Wr    : out Double_Precision_Vector;
      Wi    : out Double_Precision_Vector;
      Vl    : out Double_Precision_Matrix;
      Ldvl  : Fortran_Integer;
      Vr    : out Double_Precision_Matrix;
      Ldvr  : Fortran_Integer;
      Work  : in out Double_Precision_Vector;
      Lwork : Fortran_Integer;
      Info  : out Fortran_Integer);
   --  Call LAPACK DGEEV to compute the eigenvalues and optional left and right eigenvectors of square matrix A.

   pragma Import (Fortran, DGEEV, "dgeev");

   procedure DGELSY
     (M     : Fortran_Integer;
      N     : Fortran_Integer;
      Nrhs  : Fortran_Integer;
      A     : in out Double_Precision_Matrix;
      Lda   : Fortran_Integer;
      B     : in out Double_Precision_Matrix;
      Ldb   : Fortran_Integer;
      Jpvt  : in out Fortran_Integer_Vector;
      Rcond : Double_Precision;
      Rank  : out Fortran_Integer;
      Work  : in out Double_Precision_Vector;
      Lwork : Fortran_Integer;
      Info  : out Fortran_Integer);
   --  Call LAPACK DGELSY to solve a rank-deficient linear least-squares system using a complete orthogonal
   --  factorization.

   pragma Import (Fortran, DGELSY, "dgelsy");

end Prunt.LAPACK;
