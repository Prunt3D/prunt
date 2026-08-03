*> \brief <b> DGELSY solves overdetermined or underdetermined systems for GE matrices</b>
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DGELSY + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dgelsy.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dgelsy.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dgelsy.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
*                          WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
*       DOUBLE PRECISION   RCOND
*       ..
*       .. Array Arguments ..
*       INTEGER            JPVT( * )
*       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DGELSY computes the minimum-norm solution to a real linear least
*> squares problem:
*>     minimize || A * X - B ||
*> using a complete orthogonal factorization of A.  A is an M-by-N
*> matrix which may be rank-deficient.
*>
*> Several right hand side vectors b and solution vectors x can be
*> handled in a single call; they are stored as the columns of the
*> M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*> matrix X.
*>
*> The routine first computes a QR factorization with column pivoting:
*>     A * P = Q * [ R11 R12 ]
*>                 [  0  R22 ]
*> with R11 defined as the largest leading submatrix whose estimated
*> condition number is less than 1/RCOND.  The order of R11, RANK,
*> is the effective rank of A.
*>
*> Then, R22 is considered to be negligible, and R12 is annihilated
*> by orthogonal transformations from the right, arriving at the
*> complete orthogonal factorization:
*>    A * P = Q * [ T11 0 ] * Z
*>                [  0  0 ]
*> The minimum-norm solution is then
*>    X = P * Z**T [ inv(T11)*Q1**T*B ]
*>                 [        0         ]
*> where Q1 consists of the first RANK columns of Q.
*>
*> This routine is basically identical to the original xGELSX except
*> three differences:
*>   o The call to the subroutine xGEQPF has been substituted by the
*>     the call to the subroutine xGEQP3. This subroutine is a Blas-3
*>     version of the QR factorization with column pivoting.
*>   o Matrix B (the right hand side) is updated with Blas-3.
*>   o The permutation of matrix B (the right hand side) is faster and
*>     more simple.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.  M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of right hand sides, i.e., the number of
*>          columns of matrices B and X. NRHS >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          On entry, the M-by-N matrix A.
*>          On exit, A has been overwritten by details of its
*>          complete orthogonal factorization.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,M).
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is DOUBLE PRECISION array, dimension (LDB,NRHS)
*>          On entry, the M-by-NRHS right hand side matrix B.
*>          On exit, the N-by-NRHS solution matrix X.
*>          If M = 0 or N = 0, B is not referenced.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B. LDB >= max(1,M,N).
*> \endverbatim
*>
*> \param[in,out] JPVT
*> \verbatim
*>          JPVT is INTEGER array, dimension (N)
*>          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
*>          to the front of AP, otherwise column i is a free column.
*>          On exit, if JPVT(i) = k, then the i-th column of AP
*>          was the k-th column of A.
*> \endverbatim
*>
*> \param[in] RCOND
*> \verbatim
*>          RCOND is DOUBLE PRECISION
*>          RCOND is used to determine the effective rank of A, which
*>          is defined as the order of the largest leading triangular
*>          submatrix R11 in the QR factorization with pivoting of A,
*>          whose estimated condition number < 1/RCOND.
*> \endverbatim
*>
*> \param[out] RANK
*> \verbatim
*>          RANK is INTEGER
*>          The effective rank of A, i.e., the order of the submatrix
*>          R11.  This is the same as the order of the submatrix T11
*>          in the complete orthogonal factorization of A.
*>          If NRHS = 0, RANK = 0 on output.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.
*>          The unblocked strategy requires that:
*>             LWORK >= MAX( MN+3*N+1, 2*MN+NRHS ),
*>          where MN = min( M, N ).
*>          The block algorithm requires that:
*>             LWORK >= MAX( MN+2*N+NB*(N+1), 2*MN+NB*NRHS ),
*>          where NB is an upper bound on the blocksize returned
*>          by ILAENV for the routines DGEQP3, DTZRZF, STZRQF, DORMQR,
*>          and DORMRZ.
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0: successful exit
*>          < 0: If INFO = -i, the i-th argument had an illegal value.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup gelsy
*
*> \par Contributors:
*  ==================
*>
*>    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA \n
*>    E. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain \n
*>    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain \n
*>
*  =====================================================================
      SUBROUTINE DGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND,
     $                   RANK,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK driver routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            IMAX, IMIN
      PARAMETER          ( IMAX = 1, IMIN = 2 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IASCL, IBSCL, ISMAX, ISMIN, J, LWKMIN,
     $                   LWKOPT, MN, NB, NB1, NB2, NB3, NB4
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, C1, C2, S1, S2, SMAX,
     $                   SMAXPR, SMIN, SMINPR, SMLNUM, WSIZE
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           ILAENV, DLAMCH, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEQP3, DLAIC1, DLASCL,
     $                   DLASET,
     $                   DORMQR, DORMRZ, DTRSM, DTZRZF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M, N )
      ISMIN = MN + 1
      ISMAX = 2*MN + 1
*
*     Test the input arguments.
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, M, N ) ) THEN
         INFO = -7
      END IF
*
*     Figure out optimal block size
*
      IF( INFO.EQ.0 ) THEN
         IF( MN.EQ.0 .OR. NRHS.EQ.0 ) THEN
            LWKMIN = 1
            LWKOPT = 1
         ELSE
            NB1 = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
            NB2 = ILAENV( 1, 'DGERQF', ' ', M, N, -1, -1 )
            NB3 = ILAENV( 1, 'DORMQR', ' ', M, N, NRHS, -1 )
            NB4 = ILAENV( 1, 'DORMRQ', ' ', M, N, NRHS, -1 )
            NB = MAX( NB1, NB2, NB3, NB4 )
            LWKMIN = MN + MAX( 2*MN, N + 1, MN + NRHS )
            LWKOPT = MAX( LWKMIN,
     $                    MN + 2*N + NB*( N + 1 ), 2*MN + NB*NRHS )
         END IF
         WORK( 1 ) = LWKOPT
*
         IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY ) THEN
            INFO = -12
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELSY', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( MN.EQ.0 .OR. NRHS.EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters
*
      SMLNUM = DLAMCH( 'S' ) / DLAMCH( 'P' )
      BIGNUM = ONE / SMLNUM
*
*     Scale A, B if max entries outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         RANK = 0
         GO TO 70
      END IF
*
      BNRM = DLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB,
     $                INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB,
     $                INFO )
         IBSCL = 2
      END IF
*
*     Compute QR factorization with column pivoting of A:
*        A * P = Q * R
*
      CALL DGEQP3( M, N, A, LDA, JPVT, WORK( 1 ), WORK( MN+1 ),
     $             LWORK-MN, INFO )
      WSIZE = MN + WORK( MN+1 )
*
*     workspace: MN+2*N+NB*(N+1).
*     Details of Householder rotations stored in WORK(1:MN).
*
*     Determine RANK using incremental condition estimation
*
      WORK( ISMIN ) = ONE
      WORK( ISMAX ) = ONE
      SMAX = ABS( A( 1, 1 ) )
      SMIN = SMAX
      IF( ABS( A( 1, 1 ) ).EQ.ZERO ) THEN
         RANK = 0
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         GO TO 70
      ELSE
         RANK = 1
      END IF
*
   10 CONTINUE
      IF( RANK.LT.MN ) THEN
         I = RANK + 1
         CALL DLAIC1( IMIN, RANK, WORK( ISMIN ), SMIN, A( 1, I ),
     $                A( I, I ), SMINPR, S1, C1 )
         CALL DLAIC1( IMAX, RANK, WORK( ISMAX ), SMAX, A( 1, I ),
     $                A( I, I ), SMAXPR, S2, C2 )
*
         IF( SMAXPR*RCOND.LE.SMINPR ) THEN
            DO 20 I = 1, RANK
               WORK( ISMIN+I-1 ) = S1*WORK( ISMIN+I-1 )
               WORK( ISMAX+I-1 ) = S2*WORK( ISMAX+I-1 )
   20       CONTINUE
            WORK( ISMIN+RANK ) = C1
            WORK( ISMAX+RANK ) = C2
            SMIN = SMINPR
            SMAX = SMAXPR
            RANK = RANK + 1
            GO TO 10
         END IF
      END IF
*
*     workspace: 3*MN.
*
*     Logically partition R = [ R11 R12 ]
*                             [  0  R22 ]
*     where R11 = R(1:RANK,1:RANK)
*
*     [R11,R12] = [ T11, 0 ] * Y
*
      IF( RANK.LT.N )
     $   CALL DTZRZF( RANK, N, A, LDA, WORK( MN+1 ), WORK( 2*MN+1 ),
     $                LWORK-2*MN, INFO )
*
*     workspace: 2*MN.
*     Details of Householder rotations stored in WORK(MN+1:2*MN)
*
*     B(1:M,1:NRHS) := Q**T * B(1:M,1:NRHS)
*
      CALL DORMQR( 'Left', 'Transpose', M, NRHS, MN, A, LDA,
     $             WORK( 1 ),
     $             B, LDB, WORK( 2*MN+1 ), LWORK-2*MN, INFO )
      WSIZE = MAX( WSIZE, 2*MN+WORK( 2*MN+1 ) )
*
*     workspace: 2*MN+NB*NRHS.
*
*     B(1:RANK,1:NRHS) := inv(T11) * B(1:RANK,1:NRHS)
*
      CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', RANK,
     $            NRHS, ONE, A, LDA, B, LDB )
*
      DO 40 J = 1, NRHS
         DO 30 I = RANK + 1, N
            B( I, J ) = ZERO
   30    CONTINUE
   40 CONTINUE
*
*     B(1:N,1:NRHS) := Y**T * B(1:N,1:NRHS)
*
      IF( RANK.LT.N ) THEN
         CALL DORMRZ( 'Left', 'Transpose', N, NRHS, RANK, N-RANK, A,
     $                LDA, WORK( MN+1 ), B, LDB, WORK( 2*MN+1 ),
     $                LWORK-2*MN, INFO )
      END IF
*
*     workspace: 2*MN+NRHS.
*
*     B(1:N,1:NRHS) := P * B(1:N,1:NRHS)
*
      DO 60 J = 1, NRHS
         DO 50 I = 1, N
            WORK( JPVT( I ) ) = B( I, J )
   50    CONTINUE
         CALL DCOPY( N, WORK( 1 ), 1, B( 1, J ), 1 )
   60 CONTINUE
*
*     workspace: N.
*
*     Undo scaling
*
      IF( IASCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB,
     $                INFO )
         CALL DLASCL( 'U', 0, 0, SMLNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB,
     $                INFO )
         CALL DLASCL( 'U', 0, 0, BIGNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB,
     $                INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB,
     $                INFO )
      END IF
*
   70 CONTINUE
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DGELSY
*
      END
*> \brief \b DGEQP3
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DGEQP3 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dgeqp3.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dgeqp3.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dgeqp3.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       INTEGER            INFO, LDA, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       INTEGER            JPVT( * )
*       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DGEQP3 computes a QR factorization with column pivoting of a
*> matrix A:  A*P = Q*R  using Level 3 BLAS.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A. M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          On entry, the M-by-N matrix A.
*>          On exit, the upper triangle of the array contains the
*>          min(M,N)-by-N upper trapezoidal matrix R; the elements below
*>          the diagonal, together with the array TAU, represent the
*>          orthogonal matrix Q as a product of min(M,N) elementary
*>          reflectors.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A. LDA >= max(1,M).
*> \endverbatim
*>
*> \param[in,out] JPVT
*> \verbatim
*>          JPVT is INTEGER array, dimension (N)
*>          On entry, if JPVT(J).ne.0, the J-th column of A is permuted
*>          to the front of A*P (a leading column); if JPVT(J)=0,
*>          the J-th column of A is a free column.
*>          On exit, if JPVT(J)=K, then the J-th column of A*P was the
*>          the K-th column of A.
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (min(M,N))
*>          The scalar factors of the elementary reflectors.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*>          On exit, if INFO=0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK. LWORK >= 3*N+1.
*>          For optimal performance LWORK >= 2*N+( N+1 )*NB, where NB
*>          is the optimal blocksize.
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0: successful exit.
*>          < 0: if INFO = -i, the i-th argument had an illegal value.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup geqp3
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  The matrix Q is represented as a product of elementary reflectors
*>
*>     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*>
*>  Each H(i) has the form
*>
*>     H(i) = I - tau * v * v**T
*>
*>  where tau is a real scalar, and v is a real/complex vector
*>  with v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in
*>  A(i+1:m,i), and tau in TAU(i).
*> \endverbatim
*
*> \par Contributors:
*  ==================
*>
*>    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*>    X. Sun, Computer Science Dept., Duke University, USA
*>
*  =====================================================================
      SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            INB, INBMIN, IXOVER
      PARAMETER          ( INB = 1, INBMIN = 2, IXOVER = 3 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            FJB, IWS, J, JB, LWKOPT, MINMN, MINWS, NA, NB,
     $                   NBMIN, NFXD, NX, SM, SMINMN, SN, TOPBMN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQRF, DLAQP2, DLAQPS, DORMQR, DSWAP,
     $                   XERBLA
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DNRM2
      EXTERNAL           ILAENV, DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test input arguments
*  ====================
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
*
      IF( INFO.EQ.0 ) THEN
         MINMN = MIN( M, N )
         IF( MINMN.EQ.0 ) THEN
            IWS = 1
            LWKOPT = 1
         ELSE
            IWS = 3*N + 1
            NB = ILAENV( INB, 'DGEQRF', ' ', M, N, -1, -1 )
            LWKOPT = 2*N + ( N + 1 )*NB
         END IF
         WORK( 1 ) = LWKOPT
*
         IF( ( LWORK.LT.IWS ) .AND. .NOT.LQUERY ) THEN
            INFO = -8
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEQP3', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Move initial columns up front.
*
      NFXD = 1
      DO 10 J = 1, N
         IF( JPVT( J ).NE.0 ) THEN
            IF( J.NE.NFXD ) THEN
               CALL DSWAP( M, A( 1, J ), 1, A( 1, NFXD ), 1 )
               JPVT( J ) = JPVT( NFXD )
               JPVT( NFXD ) = J
            ELSE
               JPVT( J ) = J
            END IF
            NFXD = NFXD + 1
         ELSE
            JPVT( J ) = J
         END IF
   10 CONTINUE
      NFXD = NFXD - 1
*
*     Factorize fixed columns
*  =======================
*
*     Compute the QR factorization of fixed columns and update
*     remaining columns.
*
      IF( NFXD.GT.0 ) THEN
         NA = MIN( M, NFXD )
*CC      CALL DGEQR2( M, NA, A, LDA, TAU, WORK, INFO )
         CALL DGEQRF( M, NA, A, LDA, TAU, WORK, LWORK, INFO )
         IWS = MAX( IWS, INT( WORK( 1 ) ) )
         IF( NA.LT.N ) THEN
*CC         CALL DORM2R( 'Left', 'Transpose', M, N-NA, NA, A, LDA,
*CC  $                   TAU, A( 1, NA+1 ), LDA, WORK, INFO )
            CALL DORMQR( 'Left', 'Transpose', M, N-NA, NA, A, LDA,
     $                   TAU,
     $                   A( 1, NA+1 ), LDA, WORK, LWORK, INFO )
            IWS = MAX( IWS, INT( WORK( 1 ) ) )
         END IF
      END IF
*
*     Factorize free columns
*  ======================
*
      IF( NFXD.LT.MINMN ) THEN
*
         SM = M - NFXD
         SN = N - NFXD
         SMINMN = MINMN - NFXD
*
*        Determine the block size.
*
         NB = ILAENV( INB, 'DGEQRF', ' ', SM, SN, -1, -1 )
         NBMIN = 2
         NX = 0
*
         IF( ( NB.GT.1 ) .AND. ( NB.LT.SMINMN ) ) THEN
*
*           Determine when to cross over from blocked to unblocked code.
*
            NX = MAX( 0, ILAENV( IXOVER, 'DGEQRF', ' ', SM, SN, -1,
     $           -1 ) )
*
*
            IF( NX.LT.SMINMN ) THEN
*
*              Determine if workspace is large enough for blocked code.
*
               MINWS = 2*SN + ( SN+1 )*NB
               IWS = MAX( IWS, MINWS )
               IF( LWORK.LT.MINWS ) THEN
*
*                 Not enough workspace to use optimal NB: Reduce NB and
*                 determine the minimum value of NB.
*
                  NB = ( LWORK-2*SN ) / ( SN+1 )
                  NBMIN = MAX( 2, ILAENV( INBMIN, 'DGEQRF', ' ', SM,
     $                         SN,
     $                    -1, -1 ) )
*
*
               END IF
            END IF
         END IF
*
*        Initialize partial column norms. The first N elements of work
*        store the exact column norms.
*
         DO 20 J = NFXD + 1, N
            WORK( J ) = DNRM2( SM, A( NFXD+1, J ), 1 )
            WORK( N+J ) = WORK( J )
   20    CONTINUE
*
         IF( ( NB.GE.NBMIN ) .AND. ( NB.LT.SMINMN ) .AND.
     $       ( NX.LT.SMINMN ) ) THEN
*
*           Use blocked code initially.
*
            J = NFXD + 1
*
*           Compute factorization: while loop.
*
*
            TOPBMN = MINMN - NX
   30       CONTINUE
            IF( J.LE.TOPBMN ) THEN
               JB = MIN( NB, TOPBMN-J+1 )
*
*              Factorize JB columns among columns J:N.
*
               CALL DLAQPS( M, N-J+1, J-1, JB, FJB, A( 1, J ), LDA,
     $                      JPVT( J ), TAU( J ), WORK( J ), WORK( N+J ),
     $                      WORK( 2*N+1 ), WORK( 2*N+JB+1 ), N-J+1 )
*
               J = J + FJB
               GO TO 30
            END IF
         ELSE
            J = NFXD + 1
         END IF
*
*        Use unblocked code to factor the last or only block.
*
*
         IF( J.LE.MINMN )
     $      CALL DLAQP2( M, N-J+1, J-1, A( 1, J ), LDA, JPVT( J ),
     $                   TAU( J ), WORK( J ), WORK( N+J ),
     $                   WORK( 2*N+1 ) )
*
      END IF
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of DGEQP3
*
      END
*> \brief \b DGEQR2 computes the QR factorization of a general rectangular matrix using an unblocked algorithm.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DGEQR2 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dgeqr2.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dgeqr2.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dgeqr2.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DGEQR2( M, N, A, LDA, TAU, WORK, INFO )
*
*       .. Scalar Arguments ..
*       INTEGER            INFO, LDA, M, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DGEQR2 computes a QR factorization of a real m-by-n matrix A:
*>
*>    A = Q * ( R ),
*>            ( 0 )
*>
*> where:
*>
*>    Q is a m-by-m orthogonal matrix;
*>    R is an upper-triangular n-by-n matrix;
*>    0 is a (m-n)-by-n zero matrix, if m > n.
*>
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.  M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          On entry, the m by n matrix A.
*>          On exit, the elements on and above the diagonal of the array
*>          contain the min(m,n) by n upper trapezoidal matrix R (R is
*>          upper triangular if m >= n); the elements below the diagonal,
*>          with the array TAU, represent the orthogonal matrix Q as a
*>          product of elementary reflectors (see Further Details).
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,M).
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (min(M,N))
*>          The scalar factors of the elementary reflectors (see Further
*>          Details).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (N)
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0: successful exit
*>          < 0: if INFO = -i, the i-th argument had an illegal value
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup geqr2
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  The matrix Q is represented as a product of elementary reflectors
*>
*>     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*>
*>  Each H(i) has the form
*>
*>     H(i) = I - tau * v * v**T
*>
*>  where tau is a real scalar, and v is a real vector with
*>  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*>  and tau in TAU(i).
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DGEQR2( M, N, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, K
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF1F, DLARFG, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEQR2', -INFO )
         RETURN
      END IF
*
      K = MIN( M, N )
*
      DO 10 I = 1, K
*
*        Generate elementary reflector H(i) to annihilate A(i+1:m,i)
*
         CALL DLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                TAU( I ) )
         IF( I.LT.N ) THEN
*
*           Apply H(i) to A(i:m,i+1:n) from the left
*
            CALL DLARF1F( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
         END IF
   10 CONTINUE
      RETURN
*
*     End of DGEQR2
*
      END
*> \brief \b DGEQRF
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DGEQRF + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dgeqrf.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dgeqrf.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dgeqrf.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       INTEGER            INFO, LDA, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DGEQRF computes a QR factorization of a real M-by-N matrix A:
*>
*>    A = Q * ( R ),
*>            ( 0 )
*>
*> where:
*>
*>    Q is a M-by-M orthogonal matrix;
*>    R is an upper-triangular N-by-N matrix;
*>    0 is a (M-N)-by-N zero matrix, if M > N.
*>
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.  M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          On entry, the M-by-N matrix A.
*>          On exit, the elements on and above the diagonal of the array
*>          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
*>          upper triangular if m >= n); the elements below the diagonal,
*>          with the array TAU, represent the orthogonal matrix Q as a
*>          product of min(m,n) elementary reflectors (see Further
*>          Details).
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,M).
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (min(M,N))
*>          The scalar factors of the elementary reflectors (see Further
*>          Details).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.
*>          LWORK >= 1, if MIN(M,N) = 0, and LWORK >= N, otherwise.
*>          For optimum performance LWORK >= N*NB, where NB is
*>          the optimal blocksize.
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup geqrf
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  The matrix Q is represented as a product of elementary reflectors
*>
*>     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*>
*>  Each H(i) has the form
*>
*>     H(i) = I - tau * v * v**T
*>
*>  where tau is a real scalar, and v is a real vector with
*>  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*>  and tau in TAU(i).
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, LWKOPT, NB,
     $                   NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQR2, DLARFB, DLARFT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      K = MIN( M, N )
      INFO = 0
      NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( .NOT.LQUERY ) THEN
         IF( LWORK.LE.0 .OR. ( M.GT.0 .AND. LWORK.LT.MAX( 1, N ) ) )
     $      INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEQRF', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         IF( K.EQ.0 ) THEN
            LWKOPT = 1
         ELSE
            LWKOPT = N*NB
         END IF
         WORK( 1 ) = LWKOPT
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'DGEQRF', ' ', M, N, -1, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'DGEQRF', ' ', M, N, -1,
     $                 -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code initially
*
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
*
*           Compute the QR factorization of the current block
*           A(i:m,i:i+ib-1)
*
            CALL DGEQR2( M-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.N ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL DLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H**T to A(i:m,i+ib:n) from the left
*
               CALL DLARFB( 'Left', 'Transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
*
*     Use unblocked code to factor the last or only block.
*
      IF( I.LE.K )
     $   CALL DGEQR2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of DGEQRF
*
      END
*> \brief \b DLAIC1 applies one step of incremental condition estimation.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DLAIC1 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlaic1.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlaic1.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlaic1.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DLAIC1( JOB, J, X, SEST, W, GAMMA, SESTPR, S, C )
*
*       .. Scalar Arguments ..
*       INTEGER            J, JOB
*       DOUBLE PRECISION   C, GAMMA, S, SEST, SESTPR
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   W( J ), X( J )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DLAIC1 applies one step of incremental condition estimation in
*> its simplest version:
*>
*> Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
*> lower triangular matrix L, such that
*>          twonorm(L*x) = sest
*> Then DLAIC1 computes sestpr, s, c such that
*> the vector
*>                 [ s*x ]
*>          xhat = [  c  ]
*> is an approximate singular vector of
*>                 [ L       0  ]
*>          Lhat = [ w**T gamma ]
*> in the sense that
*>          twonorm(Lhat*xhat) = sestpr.
*>
*> Depending on JOB, an estimate for the largest or smallest singular
*> value is computed.
*>
*> Note that [s c]**T and sestpr**2 is an eigenpair of the system
*>
*>     diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]
*>                                           [ gamma ]
*>
*> where  alpha =  x**T*w.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] JOB
*> \verbatim
*>          JOB is INTEGER
*>          = 1: an estimate for the largest singular value is computed.
*>          = 2: an estimate for the smallest singular value is computed.
*> \endverbatim
*>
*> \param[in] J
*> \verbatim
*>          J is INTEGER
*>          Length of X and W
*> \endverbatim
*>
*> \param[in] X
*> \verbatim
*>          X is DOUBLE PRECISION array, dimension (J)
*>          The j-vector x.
*> \endverbatim
*>
*> \param[in] SEST
*> \verbatim
*>          SEST is DOUBLE PRECISION
*>          Estimated singular value of j by j matrix L
*> \endverbatim
*>
*> \param[in] W
*> \verbatim
*>          W is DOUBLE PRECISION array, dimension (J)
*>          The j-vector w.
*> \endverbatim
*>
*> \param[in] GAMMA
*> \verbatim
*>          GAMMA is DOUBLE PRECISION
*>          The diagonal element gamma.
*> \endverbatim
*>
*> \param[out] SESTPR
*> \verbatim
*>          SESTPR is DOUBLE PRECISION
*>          Estimated singular value of (j+1) by (j+1) matrix Lhat.
*> \endverbatim
*>
*> \param[out] S
*> \verbatim
*>          S is DOUBLE PRECISION
*>          Sine needed in forming xhat.
*> \endverbatim
*>
*> \param[out] C
*> \verbatim
*>          C is DOUBLE PRECISION
*>          Cosine needed in forming xhat.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup laic1
*
*  =====================================================================
      SUBROUTINE DLAIC1( JOB, J, X, SEST, W, GAMMA, SESTPR, S, C )
*
*  -- LAPACK auxiliary routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            J, JOB
      DOUBLE PRECISION   C, GAMMA, S, SEST, SESTPR
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   W( J ), X( J )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
      DOUBLE PRECISION   HALF, FOUR
      PARAMETER          ( HALF = 0.5D0, FOUR = 4.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   ABSALP, ABSEST, ABSGAM, ALPHA, B, COSINE, EPS,
     $                   NORMA, S1, S2, SINE, T, TEST, TMP, ZETA1, ZETA2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN, SQRT
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DDOT, DLAMCH
      EXTERNAL           DDOT, DLAMCH
*     ..
*     .. Executable Statements ..
*
      EPS = DLAMCH( 'Epsilon' )
      ALPHA = DDOT( J, X, 1, W, 1 )
*
      ABSALP = ABS( ALPHA )
      ABSGAM = ABS( GAMMA )
      ABSEST = ABS( SEST )
*
      IF( JOB.EQ.1 ) THEN
*
*        Estimating largest singular value
*
*        special cases
*
         IF( SEST.EQ.ZERO ) THEN
            S1 = MAX( ABSGAM, ABSALP )
            IF( S1.EQ.ZERO ) THEN
               S = ZERO
               C = ONE
               SESTPR = ZERO
            ELSE
               S = ALPHA / S1
               C = GAMMA / S1
               TMP = SQRT( S*S+C*C )
               S = S / TMP
               C = C / TMP
               SESTPR = S1*TMP
            END IF
            RETURN
         ELSE IF( ABSGAM.LE.EPS*ABSEST ) THEN
            S = ONE
            C = ZERO
            TMP = MAX( ABSEST, ABSALP )
            S1 = ABSEST / TMP
            S2 = ABSALP / TMP
            SESTPR = TMP*SQRT( S1*S1+S2*S2 )
            RETURN
         ELSE IF( ABSALP.LE.EPS*ABSEST ) THEN
            S1 = ABSGAM
            S2 = ABSEST
            IF( S1.LE.S2 ) THEN
               S = ONE
               C = ZERO
               SESTPR = S2
            ELSE
               S = ZERO
               C = ONE
               SESTPR = S1
            END IF
            RETURN
         ELSE IF( ABSEST.LE.EPS*ABSALP .OR. ABSEST.LE.EPS*ABSGAM ) THEN
            S1 = ABSGAM
            S2 = ABSALP
            IF( S1.LE.S2 ) THEN
               TMP = S1 / S2
               S = SQRT( ONE+TMP*TMP )
               SESTPR = S2*S
               C = ( GAMMA / S2 ) / S
               S = SIGN( ONE, ALPHA ) / S
            ELSE
               TMP = S2 / S1
               C = SQRT( ONE+TMP*TMP )
               SESTPR = S1*C
               S = ( ALPHA / S1 ) / C
               C = SIGN( ONE, GAMMA ) / C
            END IF
            RETURN
         ELSE
*
*           normal case
*
            ZETA1 = ALPHA / ABSEST
            ZETA2 = GAMMA / ABSEST
*
            B = ( ONE-ZETA1*ZETA1-ZETA2*ZETA2 )*HALF
            C = ZETA1*ZETA1
            IF( B.GT.ZERO ) THEN
               T = C / ( B+SQRT( B*B+C ) )
            ELSE
               T = SQRT( B*B+C ) - B
            END IF
*
            SINE = -ZETA1 / T
            COSINE = -ZETA2 / ( ONE+T )
            TMP = SQRT( SINE*SINE+COSINE*COSINE )
            S = SINE / TMP
            C = COSINE / TMP
            SESTPR = SQRT( T+ONE )*ABSEST
            RETURN
         END IF
*
      ELSE IF( JOB.EQ.2 ) THEN
*
*        Estimating smallest singular value
*
*        special cases
*
         IF( SEST.EQ.ZERO ) THEN
            SESTPR = ZERO
            IF( MAX( ABSGAM, ABSALP ).EQ.ZERO ) THEN
               SINE = ONE
               COSINE = ZERO
            ELSE
               SINE = -GAMMA
               COSINE = ALPHA
            END IF
            S1 = MAX( ABS( SINE ), ABS( COSINE ) )
            S = SINE / S1
            C = COSINE / S1
            TMP = SQRT( S*S+C*C )
            S = S / TMP
            C = C / TMP
            RETURN
         ELSE IF( ABSGAM.LE.EPS*ABSEST ) THEN
            S = ZERO
            C = ONE
            SESTPR = ABSGAM
            RETURN
         ELSE IF( ABSALP.LE.EPS*ABSEST ) THEN
            S1 = ABSGAM
            S2 = ABSEST
            IF( S1.LE.S2 ) THEN
               S = ZERO
               C = ONE
               SESTPR = S1
            ELSE
               S = ONE
               C = ZERO
               SESTPR = S2
            END IF
            RETURN
         ELSE IF( ABSEST.LE.EPS*ABSALP .OR. ABSEST.LE.EPS*ABSGAM ) THEN
            S1 = ABSGAM
            S2 = ABSALP
            IF( S1.LE.S2 ) THEN
               TMP = S1 / S2
               C = SQRT( ONE+TMP*TMP )
               SESTPR = ABSEST*( TMP / C )
               S = -( GAMMA / S2 ) / C
               C = SIGN( ONE, ALPHA ) / C
            ELSE
               TMP = S2 / S1
               S = SQRT( ONE+TMP*TMP )
               SESTPR = ABSEST / S
               C = ( ALPHA / S1 ) / S
               S = -SIGN( ONE, GAMMA ) / S
            END IF
            RETURN
         ELSE
*
*           normal case
*
            ZETA1 = ALPHA / ABSEST
            ZETA2 = GAMMA / ABSEST
*
            NORMA = MAX( ONE+ZETA1*ZETA1+ABS( ZETA1*ZETA2 ),
     $              ABS( ZETA1*ZETA2 )+ZETA2*ZETA2 )
*
*           See if root is closer to zero or to ONE
*
            TEST = ONE + TWO*( ZETA1-ZETA2 )*( ZETA1+ZETA2 )
            IF( TEST.GE.ZERO ) THEN
*
*              root is close to zero, compute directly
*
               B = ( ZETA1*ZETA1+ZETA2*ZETA2+ONE )*HALF
               C = ZETA2*ZETA2
               T = C / ( B+SQRT( ABS( B*B-C ) ) )
               SINE = ZETA1 / ( ONE-T )
               COSINE = -ZETA2 / T
               SESTPR = SQRT( T+FOUR*EPS*EPS*NORMA )*ABSEST
            ELSE
*
*              root is closer to ONE, shift by that amount
*
               B = ( ZETA2*ZETA2+ZETA1*ZETA1-ONE )*HALF
               C = ZETA1*ZETA1
               IF( B.GE.ZERO ) THEN
                  T = -C / ( B+SQRT( B*B+C ) )
               ELSE
                  T = B - SQRT( B*B+C )
               END IF
               SINE = -ZETA1 / T
               COSINE = -ZETA2 / ( ONE+T )
               SESTPR = SQRT( ONE+T+FOUR*EPS*EPS*NORMA )*ABSEST
            END IF
            TMP = SQRT( SINE*SINE+COSINE*COSINE )
            S = SINE / TMP
            C = COSINE / TMP
            RETURN
*
         END IF
      END IF
      RETURN
*
*     End of DLAIC1
*
      END
*> \brief \b DLAQP2 computes a QR factorization with column pivoting of the matrix block.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DLAQP2 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlaqp2.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlaqp2.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlaqp2.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DLAQP2( M, N, OFFSET, A, LDA, JPVT, TAU, VN1, VN2,
*                          WORK )
*
*       .. Scalar Arguments ..
*       INTEGER            LDA, M, N, OFFSET
*       ..
*       .. Array Arguments ..
*       INTEGER            JPVT( * )
*       DOUBLE PRECISION   A( LDA, * ), TAU( * ), VN1( * ), VN2( * ),
*      $                   WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DLAQP2 computes a QR factorization with column pivoting of
*> the block A(OFFSET+1:M,1:N).
*> The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A. M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A. N >= 0.
*> \endverbatim
*>
*> \param[in] OFFSET
*> \verbatim
*>          OFFSET is INTEGER
*>          The number of rows of the matrix A that must be pivoted
*>          but no factorized. OFFSET >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          On entry, the M-by-N matrix A.
*>          On exit, the upper triangle of block A(OFFSET+1:M,1:N) is
*>          the triangular factor obtained; the elements in block
*>          A(OFFSET+1:M,1:N) below the diagonal, together with the
*>          array TAU, represent the orthogonal matrix Q as a product of
*>          elementary reflectors. Block A(1:OFFSET,1:N) has been
*>          accordingly pivoted, but no factorized.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A. LDA >= max(1,M).
*> \endverbatim
*>
*> \param[in,out] JPVT
*> \verbatim
*>          JPVT is INTEGER array, dimension (N)
*>          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
*>          to the front of A*P (a leading column); if JPVT(i) = 0,
*>          the i-th column of A is a free column.
*>          On exit, if JPVT(i) = k, then the i-th column of A*P
*>          was the k-th column of A.
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (min(M,N))
*>          The scalar factors of the elementary reflectors.
*> \endverbatim
*>
*> \param[in,out] VN1
*> \verbatim
*>          VN1 is DOUBLE PRECISION array, dimension (N)
*>          The vector with the partial column norms.
*> \endverbatim
*>
*> \param[in,out] VN2
*> \verbatim
*>          VN2 is DOUBLE PRECISION array, dimension (N)
*>          The vector with the exact column norms.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (N)
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup laqp2
*
*> \par Contributors:
*  ==================
*>
*>    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*>    X. Sun, Computer Science Dept., Duke University, USA
*> \n
*>  Partial column norm updating strategy modified on April 2011
*>    Z. Drmac and Z. Bujanovic, Dept. of Mathematics,
*>    University of Zagreb, Croatia.
*
*> \par References:
*  ================
*>
*> LAPACK Working Note 176
*
*> \htmlonly
*> <a href="http://www.netlib.org/lapack/lawnspdf/lawn176.pdf">[PDF]</a>
*> \endhtmlonly
*
*  =====================================================================
      SUBROUTINE DLAQP2( M, N, OFFSET, A, LDA, JPVT, TAU, VN1, VN2,
     $                   WORK )
*
*  -- LAPACK auxiliary routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            LDA, M, N, OFFSET
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), VN1( * ), VN2( * ),
     $                   WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, ITEMP, J, MN, OFFPI, PVT
      DOUBLE PRECISION   TEMP, TEMP2, TOL3Z
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DLARFG, DSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH, DNRM2
      EXTERNAL           IDAMAX, DLAMCH, DNRM2
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M-OFFSET, N )
      TOL3Z = SQRT(DLAMCH('Epsilon'))
*
*     Compute factorization.
*
      DO 20 I = 1, MN
*
         OFFPI = OFFSET + I
*
*        Determine ith pivot column and swap if necessary.
*
         PVT = ( I-1 ) + IDAMAX( N-I+1, VN1( I ), 1 )
*
         IF( PVT.NE.I ) THEN
            CALL DSWAP( M, A( 1, PVT ), 1, A( 1, I ), 1 )
            ITEMP = JPVT( PVT )
            JPVT( PVT ) = JPVT( I )
            JPVT( I ) = ITEMP
            VN1( PVT ) = VN1( I )
            VN2( PVT ) = VN2( I )
         END IF
*
*        Generate elementary reflector H(i).
*
         IF( OFFPI.LT.M ) THEN
            CALL DLARFG( M-OFFPI+1, A( OFFPI, I ), A( OFFPI+1, I ),
     $                   1,
     $                   TAU( I ) )
         ELSE
            CALL DLARFG( 1, A( M, I ), A( M, I ), 1, TAU( I ) )
         END IF
*
         IF( I.LT.N ) THEN
*
*           Apply H(i)**T to A(offset+i:m,i+1:n) from the left.
*
            CALL DLARF1F( 'Left', M-OFFPI+1, N-I, A( OFFPI, I ), 1,
     $                  TAU( I ), A( OFFPI, I+1 ), LDA, WORK( 1 ) )
         END IF
*
*        Update partial column norms.
*
         DO 10 J = I + 1, N
            IF( VN1( J ).NE.ZERO ) THEN
*
*              NOTE: The following 4 lines follow from the analysis in
*              Lapack Working Note 176.
*
               TEMP = ONE - ( ABS( A( OFFPI, J ) ) / VN1( J ) )**2
               TEMP = MAX( TEMP, ZERO )
               TEMP2 = TEMP*( VN1( J ) / VN2( J ) )**2
               IF( TEMP2 .LE. TOL3Z ) THEN
                  IF( OFFPI.LT.M ) THEN
                     VN1( J ) = DNRM2( M-OFFPI, A( OFFPI+1, J ), 1 )
                     VN2( J ) = VN1( J )
                  ELSE
                     VN1( J ) = ZERO
                     VN2( J ) = ZERO
                  END IF
               ELSE
                  VN1( J ) = VN1( J )*SQRT( TEMP )
               END IF
            END IF
   10    CONTINUE
*
   20 CONTINUE
*
      RETURN
*
*     End of DLAQP2
*
      END
*> \brief \b DLAQPS computes a step of QR factorization with column pivoting of a real m-by-n matrix A by using BLAS level 3.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DLAQPS + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlaqps.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlaqps.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlaqps.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DLAQPS( M, N, OFFSET, NB, KB, A, LDA, JPVT, TAU, VN1,
*                          VN2, AUXV, F, LDF )
*
*       .. Scalar Arguments ..
*       INTEGER            KB, LDA, LDF, M, N, NB, OFFSET
*       ..
*       .. Array Arguments ..
*       INTEGER            JPVT( * )
*       DOUBLE PRECISION   A( LDA, * ), AUXV( * ), F( LDF, * ), TAU( * ),
*      $                   VN1( * ), VN2( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DLAQPS computes a step of QR factorization with column pivoting
*> of a real M-by-N matrix A by using Blas-3.  It tries to factorize
*> NB columns from A starting from the row OFFSET+1, and updates all
*> of the matrix with Blas-3 xGEMM.
*>
*> In some cases, due to catastrophic cancellations, it cannot
*> factorize NB columns.  Hence, the actual number of factorized
*> columns is returned in KB.
*>
*> Block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A. M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A. N >= 0
*> \endverbatim
*>
*> \param[in] OFFSET
*> \verbatim
*>          OFFSET is INTEGER
*>          The number of rows of A that have been factorized in
*>          previous steps.
*> \endverbatim
*>
*> \param[in] NB
*> \verbatim
*>          NB is INTEGER
*>          The number of columns to factorize.
*> \endverbatim
*>
*> \param[out] KB
*> \verbatim
*>          KB is INTEGER
*>          The number of columns actually factorized.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          On entry, the M-by-N matrix A.
*>          On exit, block A(OFFSET+1:M,1:KB) is the triangular
*>          factor obtained and block A(1:OFFSET,1:N) has been
*>          accordingly pivoted, but no factorized.
*>          The rest of the matrix, block A(OFFSET+1:M,KB+1:N) has
*>          been updated.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A. LDA >= max(1,M).
*> \endverbatim
*>
*> \param[in,out] JPVT
*> \verbatim
*>          JPVT is INTEGER array, dimension (N)
*>          JPVT(I) = K <==> Column K of the full matrix A has been
*>          permuted into position I in AP.
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (KB)
*>          The scalar factors of the elementary reflectors.
*> \endverbatim
*>
*> \param[in,out] VN1
*> \verbatim
*>          VN1 is DOUBLE PRECISION array, dimension (N)
*>          The vector with the partial column norms.
*> \endverbatim
*>
*> \param[in,out] VN2
*> \verbatim
*>          VN2 is DOUBLE PRECISION array, dimension (N)
*>          The vector with the exact column norms.
*> \endverbatim
*>
*> \param[in,out] AUXV
*> \verbatim
*>          AUXV is DOUBLE PRECISION array, dimension (NB)
*>          Auxiliary vector.
*> \endverbatim
*>
*> \param[in,out] F
*> \verbatim
*>          F is DOUBLE PRECISION array, dimension (LDF,NB)
*>          Matrix F**T = L*Y**T*A.
*> \endverbatim
*>
*> \param[in] LDF
*> \verbatim
*>          LDF is INTEGER
*>          The leading dimension of the array F. LDF >= max(1,N).
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup laqps
*
*> \par Contributors:
*  ==================
*>
*>    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*>    X. Sun, Computer Science Dept., Duke University, USA
*> \n
*>  Partial column norm updating strategy modified on April 2011
*>    Z. Drmac and Z. Bujanovic, Dept. of Mathematics,
*>    University of Zagreb, Croatia.
*
*> \par References:
*  ================
*>
*> LAPACK Working Note 176
*
*> \htmlonly
*> <a href="http://www.netlib.org/lapack/lawnspdf/lawn176.pdf">[PDF]</a>
*> \endhtmlonly
*
*  =====================================================================
      SUBROUTINE DLAQPS( M, N, OFFSET, NB, KB, A, LDA, JPVT, TAU,
     $                   VN1,
     $                   VN2, AUXV, F, LDF )
*
*  -- LAPACK auxiliary routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            KB, LDA, LDF, M, N, NB, OFFSET
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), AUXV( * ), F( LDF, * ), TAU( * ),
     $                   VN1( * ), VN2( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            ITEMP, J, K, LASTRK, LSTICC, PVT, RK
      DOUBLE PRECISION   AKK, TEMP, TEMP2, TOL3Z
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DGEMV, DLARFG, DSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, NINT, SQRT
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH, DNRM2
      EXTERNAL           IDAMAX, DLAMCH, DNRM2
*     ..
*     .. Executable Statements ..
*
      LASTRK = MIN( M, N+OFFSET )
      LSTICC = 0
      K = 0
      TOL3Z = SQRT(DLAMCH('Epsilon'))
*
*     Beginning of while loop.
*
   10 CONTINUE
      IF( ( K.LT.NB ) .AND. ( LSTICC.EQ.0 ) ) THEN
         K = K + 1
         RK = OFFSET + K
*
*        Determine ith pivot column and swap if necessary
*
         PVT = ( K-1 ) + IDAMAX( N-K+1, VN1( K ), 1 )
         IF( PVT.NE.K ) THEN
            CALL DSWAP( M, A( 1, PVT ), 1, A( 1, K ), 1 )
            CALL DSWAP( K-1, F( PVT, 1 ), LDF, F( K, 1 ), LDF )
            ITEMP = JPVT( PVT )
            JPVT( PVT ) = JPVT( K )
            JPVT( K ) = ITEMP
            VN1( PVT ) = VN1( K )
            VN2( PVT ) = VN2( K )
         END IF
*
*        Apply previous Householder reflectors to column K:
*        A(RK:M,K) := A(RK:M,K) - A(RK:M,1:K-1)*F(K,1:K-1)**T.
*
         IF( K.GT.1 ) THEN
            CALL DGEMV( 'No transpose', M-RK+1, K-1, -ONE, A( RK,
     $                  1 ),
     $                  LDA, F( K, 1 ), LDF, ONE, A( RK, K ), 1 )
         END IF
*
*        Generate elementary reflector H(k).
*
         IF( RK.LT.M ) THEN
            CALL DLARFG( M-RK+1, A( RK, K ), A( RK+1, K ), 1,
     $                   TAU( K ) )
         ELSE
            CALL DLARFG( 1, A( RK, K ), A( RK, K ), 1, TAU( K ) )
         END IF
*
         AKK = A( RK, K )
         A( RK, K ) = ONE
*
*        Compute Kth column of F:
*
*        Compute  F(K+1:N,K) := tau(K)*A(RK:M,K+1:N)**T*A(RK:M,K).
*
         IF( K.LT.N ) THEN
            CALL DGEMV( 'Transpose', M-RK+1, N-K, TAU( K ),
     $                  A( RK, K+1 ), LDA, A( RK, K ), 1, ZERO,
     $                  F( K+1, K ), 1 )
         END IF
*
*        Padding F(1:K,K) with zeros.
*
         DO 20 J = 1, K
            F( J, K ) = ZERO
   20    CONTINUE
*
*        Incremental updating of F:
*        F(1:N,K) := F(1:N,K) - tau(K)*F(1:N,1:K-1)*A(RK:M,1:K-1)**T
*                    *A(RK:M,K).
*
         IF( K.GT.1 ) THEN
            CALL DGEMV( 'Transpose', M-RK+1, K-1, -TAU( K ), A( RK,
     $                  1 ),
     $                  LDA, A( RK, K ), 1, ZERO, AUXV( 1 ), 1 )
*
            CALL DGEMV( 'No transpose', N, K-1, ONE, F( 1, 1 ), LDF,
     $                  AUXV( 1 ), 1, ONE, F( 1, K ), 1 )
         END IF
*
*        Update the current row of A:
*        A(RK,K+1:N) := A(RK,K+1:N) - A(RK,1:K)*F(K+1:N,1:K)**T.
*
         IF( K.LT.N ) THEN
            CALL DGEMV( 'No transpose', N-K, K, -ONE, F( K+1, 1 ),
     $                  LDF,
     $                  A( RK, 1 ), LDA, ONE, A( RK, K+1 ), LDA )
         END IF
*
*        Update partial column norms.
*
         IF( RK.LT.LASTRK ) THEN
            DO 30 J = K + 1, N
               IF( VN1( J ).NE.ZERO ) THEN
*
*                 NOTE: The following 4 lines follow from the analysis in
*                 Lapack Working Note 176.
*
                  TEMP = ABS( A( RK, J ) ) / VN1( J )
                  TEMP = MAX( ZERO, ( ONE+TEMP )*( ONE-TEMP ) )
                  TEMP2 = TEMP*( VN1( J ) / VN2( J ) )**2
                  IF( TEMP2 .LE. TOL3Z ) THEN
                     VN2( J ) = DBLE( LSTICC )
                     LSTICC = J
                  ELSE
                     VN1( J ) = VN1( J )*SQRT( TEMP )
                  END IF
               END IF
   30       CONTINUE
         END IF
*
         A( RK, K ) = AKK
*
*        End of while loop.
*
         GO TO 10
      END IF
      KB = K
      RK = OFFSET + KB
*
*     Apply the block reflector to the rest of the matrix:
*     A(OFFSET+KB+1:M,KB+1:N) := A(OFFSET+KB+1:M,KB+1:N) -
*                         A(OFFSET+KB+1:M,1:KB)*F(KB+1:N,1:KB)**T.
*
      IF( KB.LT.MIN( N, M-OFFSET ) ) THEN
         CALL DGEMM( 'No transpose', 'Transpose', M-RK, N-KB, KB,
     $               -ONE,
     $               A( RK+1, 1 ), LDA, F( KB+1, 1 ), LDF, ONE,
     $               A( RK+1, KB+1 ), LDA )
      END IF
*
*     Recomputation of difficult columns.
*
   40 CONTINUE
      IF( LSTICC.GT.0 ) THEN
         ITEMP = NINT( VN2( LSTICC ) )
         VN1( LSTICC ) = DNRM2( M-RK, A( RK+1, LSTICC ), 1 )
*
*        NOTE: The computation of VN1( LSTICC ) relies on the fact that
*        SNRM2 does not fail on vectors with norm below the value of
*        SQRT(DLAMCH('S'))
*
         VN2( LSTICC ) = VN1( LSTICC )
         LSTICC = ITEMP
         GO TO 40
      END IF
*
      RETURN
*
*     End of DLAQPS
*
      END
*> \brief \b DLARZ applies an elementary reflector (as returned by stzrzf) to a general matrix.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DLARZ + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlarz.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlarz.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlarz.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DLARZ( SIDE, M, N, L, V, INCV, TAU, C, LDC, WORK )
*
*       .. Scalar Arguments ..
*       CHARACTER          SIDE
*       INTEGER            INCV, L, LDC, M, N
*       DOUBLE PRECISION   TAU
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   C( LDC, * ), V( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DLARZ applies a real elementary reflector H to a real M-by-N
*> matrix C, from either the left or the right. H is represented in the
*> form
*>
*>       H = I - tau * v * v**T
*>
*> where tau is a real scalar and v is a real vector.
*>
*> If tau = 0, then H is taken to be the unit matrix.
*>
*>
*> H is a product of k elementary reflectors as returned by DTZRZF.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] SIDE
*> \verbatim
*>          SIDE is CHARACTER*1
*>          = 'L': form  H * C
*>          = 'R': form  C * H
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix C.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix C.
*> \endverbatim
*>
*> \param[in] L
*> \verbatim
*>          L is INTEGER
*>          The number of entries of the vector V containing
*>          the meaningful part of the Householder vectors.
*>          If SIDE = 'L', M >= L >= 0, if SIDE = 'R', N >= L >= 0.
*> \endverbatim
*>
*> \param[in] V
*> \verbatim
*>          V is DOUBLE PRECISION array, dimension (1+(L-1)*abs(INCV))
*>          The vector v in the representation of H as returned by
*>          DTZRZF. V is not used if TAU = 0.
*> \endverbatim
*>
*> \param[in] INCV
*> \verbatim
*>          INCV is INTEGER
*>          The increment between elements of v. INCV <> 0.
*> \endverbatim
*>
*> \param[in] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION
*>          The value tau in the representation of H.
*> \endverbatim
*>
*> \param[in,out] C
*> \verbatim
*>          C is DOUBLE PRECISION array, dimension (LDC,N)
*>          On entry, the M-by-N matrix C.
*>          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
*>          or C * H if SIDE = 'R'.
*> \endverbatim
*>
*> \param[in] LDC
*> \verbatim
*>          LDC is INTEGER
*>          The leading dimension of the array C. LDC >= max(1,M).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension
*>                         (N) if SIDE = 'L'
*>                      or (M) if SIDE = 'R'
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup larz
*
*> \par Contributors:
*  ==================
*>
*>    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DLARZ( SIDE, M, N, L, V, INCV, TAU, C, LDC, WORK )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            INCV, L, LDC, M, N
      DOUBLE PRECISION   TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), V( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMV, DGER
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        Form  H * C
*
         IF( TAU.NE.ZERO ) THEN
*
*           w( 1:n ) = C( 1, 1:n )
*
            CALL DCOPY( N, C, LDC, WORK, 1 )
*
*           w( 1:n ) = w( 1:n ) + C( m-l+1:m, 1:n )**T * v( 1:l )
*
            CALL DGEMV( 'Transpose', L, N, ONE, C( M-L+1, 1 ), LDC,
     $                  V,
     $                  INCV, ONE, WORK, 1 )
*
*           C( 1, 1:n ) = C( 1, 1:n ) - tau * w( 1:n )
*
            CALL DAXPY( N, -TAU, WORK, 1, C, LDC )
*
*           C( m-l+1:m, 1:n ) = C( m-l+1:m, 1:n ) - ...
*                               tau * v( 1:l ) * w( 1:n )**T
*
            CALL DGER( L, N, -TAU, V, INCV, WORK, 1, C( M-L+1, 1 ),
     $                 LDC )
         END IF
*
      ELSE
*
*        Form  C * H
*
         IF( TAU.NE.ZERO ) THEN
*
*           w( 1:m ) = C( 1:m, 1 )
*
            CALL DCOPY( M, C, 1, WORK, 1 )
*
*           w( 1:m ) = w( 1:m ) + C( 1:m, n-l+1:n, 1:n ) * v( 1:l )
*
            CALL DGEMV( 'No transpose', M, L, ONE, C( 1, N-L+1 ),
     $                  LDC,
     $                  V, INCV, ONE, WORK, 1 )
*
*           C( 1:m, 1 ) = C( 1:m, 1 ) - tau * w( 1:m )
*
            CALL DAXPY( M, -TAU, WORK, 1, C, 1 )
*
*           C( 1:m, n-l+1:n ) = C( 1:m, n-l+1:n ) - ...
*                               tau * w( 1:m ) * v( 1:l )**T
*
            CALL DGER( M, L, -TAU, WORK, 1, V, INCV, C( 1, N-L+1 ),
     $                 LDC )
*
         END IF
*
      END IF
*
      RETURN
*
*     End of DLARZ
*
      END
*> \brief \b DLARZB applies a block reflector or its transpose to a general matrix.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DLARZB + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlarzb.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlarzb.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlarzb.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DLARZB( SIDE, TRANS, DIRECT, STOREV, M, N, K, L, V,
*                          LDV, T, LDT, C, LDC, WORK, LDWORK )
*
*       .. Scalar Arguments ..
*       CHARACTER          DIRECT, SIDE, STOREV, TRANS
*       INTEGER            K, L, LDC, LDT, LDV, LDWORK, M, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   C( LDC, * ), T( LDT, * ), V( LDV, * ),
*      $                   WORK( LDWORK, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DLARZB applies a real block reflector H or its transpose H**T to
*> a real distributed M-by-N  C from the left or the right.
*>
*> Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] SIDE
*> \verbatim
*>          SIDE is CHARACTER*1
*>          = 'L': apply H or H**T from the Left
*>          = 'R': apply H or H**T from the Right
*> \endverbatim
*>
*> \param[in] TRANS
*> \verbatim
*>          TRANS is CHARACTER*1
*>          = 'N': apply H (No transpose)
*>          = 'C': apply H**T (Transpose)
*> \endverbatim
*>
*> \param[in] DIRECT
*> \verbatim
*>          DIRECT is CHARACTER*1
*>          Indicates how H is formed from a product of elementary
*>          reflectors
*>          = 'F': H = H(1) H(2) . . . H(k) (Forward, not supported yet)
*>          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*> \endverbatim
*>
*> \param[in] STOREV
*> \verbatim
*>          STOREV is CHARACTER*1
*>          Indicates how the vectors which define the elementary
*>          reflectors are stored:
*>          = 'C': Columnwise                        (not supported yet)
*>          = 'R': Rowwise
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix C.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix C.
*> \endverbatim
*>
*> \param[in] K
*> \verbatim
*>          K is INTEGER
*>          The order of the matrix T (= the number of elementary
*>          reflectors whose product defines the block reflector).
*> \endverbatim
*>
*> \param[in] L
*> \verbatim
*>          L is INTEGER
*>          The number of columns of the matrix V containing the
*>          meaningful part of the Householder reflectors.
*>          If SIDE = 'L', M >= L >= 0, if SIDE = 'R', N >= L >= 0.
*> \endverbatim
*>
*> \param[in] V
*> \verbatim
*>          V is DOUBLE PRECISION array, dimension (LDV,NV).
*>          If STOREV = 'C', NV = K; if STOREV = 'R', NV = L.
*> \endverbatim
*>
*> \param[in] LDV
*> \verbatim
*>          LDV is INTEGER
*>          The leading dimension of the array V.
*>          If STOREV = 'C', LDV >= L; if STOREV = 'R', LDV >= K.
*> \endverbatim
*>
*> \param[in] T
*> \verbatim
*>          T is DOUBLE PRECISION array, dimension (LDT,K)
*>          The triangular K-by-K matrix T in the representation of the
*>          block reflector.
*> \endverbatim
*>
*> \param[in] LDT
*> \verbatim
*>          LDT is INTEGER
*>          The leading dimension of the array T. LDT >= K.
*> \endverbatim
*>
*> \param[in,out] C
*> \verbatim
*>          C is DOUBLE PRECISION array, dimension (LDC,N)
*>          On entry, the M-by-N matrix C.
*>          On exit, C is overwritten by H*C or H**T*C or C*H or C*H**T.
*> \endverbatim
*>
*> \param[in] LDC
*> \verbatim
*>          LDC is INTEGER
*>          The leading dimension of the array C. LDC >= max(1,M).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (LDWORK,K)
*> \endverbatim
*>
*> \param[in] LDWORK
*> \verbatim
*>          LDWORK is INTEGER
*>          The leading dimension of the array WORK.
*>          If SIDE = 'L', LDWORK >= max(1,N);
*>          if SIDE = 'R', LDWORK >= max(1,M).
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup larzb
*
*> \par Contributors:
*  ==================
*>
*>    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DLARZB( SIDE, TRANS, DIRECT, STOREV, M, N, K, L, V,
     $                   LDV, T, LDT, C, LDC, WORK, LDWORK )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, SIDE, STOREV, TRANS
      INTEGER            K, L, LDC, LDT, LDV, LDWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), T( LDT, * ), V( LDV, * ),
     $                   WORK( LDWORK, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANST
      INTEGER            I, INFO, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DTRMM, XERBLA
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
*     Check for currently supported options
*
      INFO = 0
      IF( .NOT.LSAME( DIRECT, 'B' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.LSAME( STOREV, 'R' ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLARZB', -INFO )
         RETURN
      END IF
*
      IF( LSAME( TRANS, 'N' ) ) THEN
         TRANST = 'T'
      ELSE
         TRANST = 'N'
      END IF
*
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        Form  H * C  or  H**T * C
*
*        W( 1:n, 1:k ) = C( 1:k, 1:n )**T
*
         DO 10 J = 1, K
            CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
   10    CONTINUE
*
*        W( 1:n, 1:k ) = W( 1:n, 1:k ) + ...
*                        C( m-l+1:m, 1:n )**T * V( 1:k, 1:l )**T
*
         IF( L.GT.0 )
     $      CALL DGEMM( 'Transpose', 'Transpose', N, K, L, ONE,
     $                  C( M-L+1, 1 ), LDC, V, LDV, ONE, WORK, LDWORK )
*
*        W( 1:n, 1:k ) = W( 1:n, 1:k ) * T**T  or  W( 1:m, 1:k ) * T
*
         CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K, ONE,
     $               T,
     $               LDT, WORK, LDWORK )
*
*        C( 1:k, 1:n ) = C( 1:k, 1:n ) - W( 1:n, 1:k )**T
*
         DO 30 J = 1, N
            DO 20 I = 1, K
               C( I, J ) = C( I, J ) - WORK( J, I )
   20       CONTINUE
   30    CONTINUE
*
*        C( m-l+1:m, 1:n ) = C( m-l+1:m, 1:n ) - ...
*                            V( 1:k, 1:l )**T * W( 1:n, 1:k )**T
*
         IF( L.GT.0 )
     $      CALL DGEMM( 'Transpose', 'Transpose', L, N, K, -ONE, V,
     $                  LDV,
     $                  WORK, LDWORK, ONE, C( M-L+1, 1 ), LDC )
*
      ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*        Form  C * H  or  C * H**T
*
*        W( 1:m, 1:k ) = C( 1:m, 1:k )
*
         DO 40 J = 1, K
            CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
   40    CONTINUE
*
*        W( 1:m, 1:k ) = W( 1:m, 1:k ) + ...
*                        C( 1:m, n-l+1:n ) * V( 1:k, 1:l )**T
*
         IF( L.GT.0 )
     $      CALL DGEMM( 'No transpose', 'Transpose', M, K, L, ONE,
     $                  C( 1, N-L+1 ), LDC, V, LDV, ONE, WORK, LDWORK )
*
*        W( 1:m, 1:k ) = W( 1:m, 1:k ) * T  or  W( 1:m, 1:k ) * T**T
*
         CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K, ONE,
     $               T,
     $               LDT, WORK, LDWORK )
*
*        C( 1:m, 1:k ) = C( 1:m, 1:k ) - W( 1:m, 1:k )
*
         DO 60 J = 1, K
            DO 50 I = 1, M
               C( I, J ) = C( I, J ) - WORK( I, J )
   50       CONTINUE
   60    CONTINUE
*
*        C( 1:m, n-l+1:n ) = C( 1:m, n-l+1:n ) - ...
*                            W( 1:m, 1:k ) * V( 1:k, 1:l )
*
         IF( L.GT.0 )
     $      CALL DGEMM( 'No transpose', 'No transpose', M, L, K,
     $                  -ONE,
     $                  WORK, LDWORK, V, LDV, ONE, C( 1, N-L+1 ), LDC )
*
      END IF
*
      RETURN
*
*     End of DLARZB
*
      END
*> \brief \b DLARZT forms the triangular factor T of a block reflector H = I - vtvH.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DLARZT + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlarzt.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlarzt.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlarzt.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DLARZT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
*
*       .. Scalar Arguments ..
*       CHARACTER          DIRECT, STOREV
*       INTEGER            K, LDT, LDV, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   T( LDT, * ), TAU( * ), V( LDV, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DLARZT forms the triangular factor T of a real block reflector
*> H of order > n, which is defined as a product of k elementary
*> reflectors.
*>
*> If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
*>
*> If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
*>
*> If STOREV = 'C', the vector which defines the elementary reflector
*> H(i) is stored in the i-th column of the array V, and
*>
*>    H  =  I - V * T * V**T
*>
*> If STOREV = 'R', the vector which defines the elementary reflector
*> H(i) is stored in the i-th row of the array V, and
*>
*>    H  =  I - V**T * T * V
*>
*> Currently, only STOREV = 'R' and DIRECT = 'B' are supported.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] DIRECT
*> \verbatim
*>          DIRECT is CHARACTER*1
*>          Specifies the order in which the elementary reflectors are
*>          multiplied to form the block reflector:
*>          = 'F': H = H(1) H(2) . . . H(k) (Forward, not supported yet)
*>          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*> \endverbatim
*>
*> \param[in] STOREV
*> \verbatim
*>          STOREV is CHARACTER*1
*>          Specifies how the vectors which define the elementary
*>          reflectors are stored (see also Further Details):
*>          = 'C': columnwise                        (not supported yet)
*>          = 'R': rowwise
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the block reflector H. N >= 0.
*> \endverbatim
*>
*> \param[in] K
*> \verbatim
*>          K is INTEGER
*>          The order of the triangular factor T (= the number of
*>          elementary reflectors). K >= 1.
*> \endverbatim
*>
*> \param[in,out] V
*> \verbatim
*>          V is DOUBLE PRECISION array, dimension
*>                               (LDV,K) if STOREV = 'C'
*>                               (LDV,N) if STOREV = 'R'
*>          The matrix V. See further details.
*> \endverbatim
*>
*> \param[in] LDV
*> \verbatim
*>          LDV is INTEGER
*>          The leading dimension of the array V.
*>          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
*> \endverbatim
*>
*> \param[in] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (K)
*>          TAU(i) must contain the scalar factor of the elementary
*>          reflector H(i).
*> \endverbatim
*>
*> \param[out] T
*> \verbatim
*>          T is DOUBLE PRECISION array, dimension (LDT,K)
*>          The k by k triangular factor T of the block reflector.
*>          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
*>          lower triangular. The rest of the array is not used.
*> \endverbatim
*>
*> \param[in] LDT
*> \verbatim
*>          LDT is INTEGER
*>          The leading dimension of the array T. LDT >= K.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup larzt
*
*> \par Contributors:
*  ==================
*>
*>    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  The shape of the matrix V and the storage of the vectors which define
*>  the H(i) is best illustrated by the following example with n = 5 and
*>  k = 3. The elements equal to 1 are not stored; the corresponding
*>  array elements are modified but restored on exit. The rest of the
*>  array is not used.
*>
*>  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
*>
*>                                              ______V_____
*>         ( v1 v2 v3 )                        /            \
*>         ( v1 v2 v3 )                      ( v1 v1 v1 v1 v1 . . . . 1 )
*>     V = ( v1 v2 v3 )                      ( v2 v2 v2 v2 v2 . . . 1   )
*>         ( v1 v2 v3 )                      ( v3 v3 v3 v3 v3 . . 1     )
*>         ( v1 v2 v3 )
*>            .  .  .
*>            .  .  .
*>            1  .  .
*>               1  .
*>                  1
*>
*>  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
*>
*>                                                        ______V_____
*>            1                                          /            \
*>            .  1                           ( 1 . . . . v1 v1 v1 v1 v1 )
*>            .  .  1                        ( . 1 . . . v2 v2 v2 v2 v2 )
*>            .  .  .                        ( . . 1 . . v3 v3 v3 v3 v3 )
*>            .  .  .
*>         ( v1 v2 v3 )
*>         ( v1 v2 v3 )
*>     V = ( v1 v2 v3 )
*>         ( v1 v2 v3 )
*>         ( v1 v2 v3 )
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DLARZT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, STOREV
      INTEGER            K, LDT, LDV, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   T( LDT, * ), TAU( * ), V( LDV, * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DTRMV, XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
*     Check for currently supported options
*
      INFO = 0
      IF( .NOT.LSAME( DIRECT, 'B' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( STOREV, 'R' ) ) THEN
         INFO = -2
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLARZT', -INFO )
         RETURN
      END IF
*
      DO 20 I = K, 1, -1
         IF( TAU( I ).EQ.ZERO ) THEN
*
*           H(i)  =  I
*
            DO 10 J = I, K
               T( J, I ) = ZERO
   10       CONTINUE
         ELSE
*
*           general case
*
            IF( I.LT.K ) THEN
*
*              T(i+1:k,i) = - tau(i) * V(i+1:k,1:n) * V(i,1:n)**T
*
               CALL DGEMV( 'No transpose', K-I, N, -TAU( I ),
     $                     V( I+1, 1 ), LDV, V( I, 1 ), LDV, ZERO,
     $                     T( I+1, I ), 1 )
*
*              T(i+1:k,i) = T(i+1:k,i+1:k) * T(i+1:k,i)
*
               CALL DTRMV( 'Lower', 'No transpose', 'Non-unit', K-I,
     $                     T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
            END IF
            T( I, I ) = TAU( I )
         END IF
   20 CONTINUE
      RETURN
*
*     End of DLARZT
*
      END
*> \brief \b DLATRZ factors an upper trapezoidal matrix by means of orthogonal transformations.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DLATRZ + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlatrz.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlatrz.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlatrz.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DLATRZ( M, N, L, A, LDA, TAU, WORK )
*
*       .. Scalar Arguments ..
*       INTEGER            L, LDA, M, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DLATRZ factors the M-by-(M+L) real upper trapezoidal matrix
*> [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z, by means
*> of orthogonal transformations.  Z is an (M+L)-by-(M+L) orthogonal
*> matrix and, R and A1 are M-by-M upper triangular matrices.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.  M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] L
*> \verbatim
*>          L is INTEGER
*>          The number of columns of the matrix A containing the
*>          meaningful part of the Householder vectors. N-M >= L >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          On entry, the leading M-by-N upper trapezoidal part of the
*>          array A must contain the matrix to be factorized.
*>          On exit, the leading M-by-M upper triangular part of A
*>          contains the upper triangular matrix R, and elements N-L+1 to
*>          N of the first M rows of A, with the array TAU, represent the
*>          orthogonal matrix Z as a product of M elementary reflectors.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,M).
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (M)
*>          The scalar factors of the elementary reflectors.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (M)
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup latrz
*
*> \par Contributors:
*  ==================
*>
*>    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  The factorization is obtained by Householder's method.  The kth
*>  transformation matrix, Z( k ), which is used to introduce zeros into
*>  the ( m - k + 1 )th row of A, is given in the form
*>
*>     Z( k ) = ( I     0   ),
*>              ( 0  T( k ) )
*>
*>  where
*>
*>     T( k ) = I - tau*u( k )*u( k )**T,   u( k ) = (   1    ),
*>                                                 (   0    )
*>                                                 ( z( k ) )
*>
*>  tau is a scalar and z( k ) is an l element vector. tau and z( k )
*>  are chosen to annihilate the elements of the kth row of A2.
*>
*>  The scalar tau is returned in the kth element of TAU and the vector
*>  u( k ) in the kth row of A2, such that the elements of z( k ) are
*>  in  a( k, l + 1 ), ..., a( k, n ). The elements of R are returned in
*>  the upper triangular part of A1.
*>
*>  Z is given by
*>
*>     Z =  Z( 1 ) * Z( 2 ) * ... * Z( m ).
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DLATRZ( M, N, L, A, LDA, TAU, WORK )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            L, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARFG, DLARZ
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
*     Quick return if possible
*
      IF( M.EQ.0 ) THEN
         RETURN
      ELSE IF( M.EQ.N ) THEN
         DO 10 I = 1, N
            TAU( I ) = ZERO
   10    CONTINUE
         RETURN
      END IF
*
      DO 20 I = M, 1, -1
*
*        Generate elementary reflector H(i) to annihilate
*        [ A(i,i) A(i,n-l+1:n) ]
*
         CALL DLARFG( L+1, A( I, I ), A( I, N-L+1 ), LDA, TAU( I ) )
*
*        Apply H(i) to A(1:i-1,i:n) from the right
*
         CALL DLARZ( 'Right', I-1, N-I+1, L, A( I, N-L+1 ), LDA,
     $               TAU( I ), A( 1, I ), LDA, WORK )
*
   20 CONTINUE
*
      RETURN
*
*     End of DLATRZ
*
      END
*> \brief \b DORMR3 multiplies a general matrix by the orthogonal matrix from a RZ factorization determined by stzrzf (unblocked algorithm).
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DORMR3 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dormr3.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dormr3.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dormr3.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DORMR3( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C, LDC,
*                          WORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          SIDE, TRANS
*       INTEGER            INFO, K, L, LDA, LDC, M, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DORMR3 overwrites the general real m by n matrix C with
*>
*>       Q * C  if SIDE = 'L' and TRANS = 'N', or
*>
*>       Q**T* C  if SIDE = 'L' and TRANS = 'C', or
*>
*>       C * Q  if SIDE = 'R' and TRANS = 'N', or
*>
*>       C * Q**T if SIDE = 'R' and TRANS = 'C',
*>
*> where Q is a real orthogonal matrix defined as the product of k
*> elementary reflectors
*>
*>       Q = H(1) H(2) . . . H(k)
*>
*> as returned by DTZRZF. Q is of order m if SIDE = 'L' and of order n
*> if SIDE = 'R'.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] SIDE
*> \verbatim
*>          SIDE is CHARACTER*1
*>          = 'L': apply Q or Q**T from the Left
*>          = 'R': apply Q or Q**T from the Right
*> \endverbatim
*>
*> \param[in] TRANS
*> \verbatim
*>          TRANS is CHARACTER*1
*>          = 'N': apply Q  (No transpose)
*>          = 'T': apply Q**T (Transpose)
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix C. M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix C. N >= 0.
*> \endverbatim
*>
*> \param[in] K
*> \verbatim
*>          K is INTEGER
*>          The number of elementary reflectors whose product defines
*>          the matrix Q.
*>          If SIDE = 'L', M >= K >= 0;
*>          if SIDE = 'R', N >= K >= 0.
*> \endverbatim
*>
*> \param[in] L
*> \verbatim
*>          L is INTEGER
*>          The number of columns of the matrix A containing
*>          the meaningful part of the Householder reflectors.
*>          If SIDE = 'L', M >= L >= 0, if SIDE = 'R', N >= L >= 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension
*>                               (LDA,M) if SIDE = 'L',
*>                               (LDA,N) if SIDE = 'R'
*>          The i-th row must contain the vector which defines the
*>          elementary reflector H(i), for i = 1,2,...,k, as returned by
*>          DTZRZF in the last k rows of its array argument A.
*>          A is modified by the routine but restored on exit.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A. LDA >= max(1,K).
*> \endverbatim
*>
*> \param[in] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (K)
*>          TAU(i) must contain the scalar factor of the elementary
*>          reflector H(i), as returned by DTZRZF.
*> \endverbatim
*>
*> \param[in,out] C
*> \verbatim
*>          C is DOUBLE PRECISION array, dimension (LDC,N)
*>          On entry, the m-by-n matrix C.
*>          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
*> \endverbatim
*>
*> \param[in] LDC
*> \verbatim
*>          LDC is INTEGER
*>          The leading dimension of the array C. LDC >= max(1,M).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension
*>                                   (N) if SIDE = 'L',
*>                                   (M) if SIDE = 'R'
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0: successful exit
*>          < 0: if INFO = -i, the i-th argument had an illegal value
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup unmr3
*
*> \par Contributors:
*  ==================
*>
*>    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DORMR3( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C,
     $                   LDC,
     $                   WORK, INFO )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, L, LDA, LDC, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JA, JC, MI, NI, NQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARZ, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q
*
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( L.LT.0 .OR. ( LEFT .AND. ( L.GT.M ) ) .OR.
     $         ( .NOT.LEFT .AND. ( L.GT.N ) ) ) THEN
         INFO = -6
      ELSE IF( LDA.LT.MAX( 1, K ) ) THEN
         INFO = -8
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -11
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMR3', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
*
      IF( ( LEFT .AND. .NOT.NOTRAN .OR. .NOT.LEFT .AND. NOTRAN ) ) THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
*
      IF( LEFT ) THEN
         NI = N
         JA = M - L + 1
         JC = 1
      ELSE
         MI = M
         JA = N - L + 1
         IC = 1
      END IF
*
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
*
*           H(i) or H(i)**T is applied to C(i:m,1:n)
*
            MI = M - I + 1
            IC = I
         ELSE
*
*           H(i) or H(i)**T is applied to C(1:m,i:n)
*
            NI = N - I + 1
            JC = I
         END IF
*
*        Apply H(i) or H(i)**T
*
         CALL DLARZ( SIDE, MI, NI, L, A( I, JA ), LDA, TAU( I ),
     $               C( IC, JC ), LDC, WORK )
*
   10 CONTINUE
*
      RETURN
*
*     End of DORMR3
*
      END
*> \brief \b DORMRZ
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DORMRZ + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dormrz.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dormrz.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dormrz.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DORMRZ( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C, LDC,
*                          WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          SIDE, TRANS
*       INTEGER            INFO, K, L, LDA, LDC, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DORMRZ overwrites the general real M-by-N matrix C with
*>
*>                 SIDE = 'L'     SIDE = 'R'
*> TRANS = 'N':      Q * C          C * Q
*> TRANS = 'T':      Q**T * C       C * Q**T
*>
*> where Q is a real orthogonal matrix defined as the product of k
*> elementary reflectors
*>
*>       Q = H(1) H(2) . . . H(k)
*>
*> as returned by DTZRZF. Q is of order M if SIDE = 'L' and of order N
*> if SIDE = 'R'.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] SIDE
*> \verbatim
*>          SIDE is CHARACTER*1
*>          = 'L': apply Q or Q**T from the Left;
*>          = 'R': apply Q or Q**T from the Right.
*> \endverbatim
*>
*> \param[in] TRANS
*> \verbatim
*>          TRANS is CHARACTER*1
*>          = 'N':  No transpose, apply Q;
*>          = 'T':  Transpose, apply Q**T.
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix C. M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix C. N >= 0.
*> \endverbatim
*>
*> \param[in] K
*> \verbatim
*>          K is INTEGER
*>          The number of elementary reflectors whose product defines
*>          the matrix Q.
*>          If SIDE = 'L', M >= K >= 0;
*>          if SIDE = 'R', N >= K >= 0.
*> \endverbatim
*>
*> \param[in] L
*> \verbatim
*>          L is INTEGER
*>          The number of columns of the matrix A containing
*>          the meaningful part of the Householder reflectors.
*>          If SIDE = 'L', M >= L >= 0, if SIDE = 'R', N >= L >= 0.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension
*>                               (LDA,M) if SIDE = 'L',
*>                               (LDA,N) if SIDE = 'R'
*>          The i-th row must contain the vector which defines the
*>          elementary reflector H(i), for i = 1,2,...,k, as returned by
*>          DTZRZF in the last k rows of its array argument A.
*>          A is modified by the routine but restored on exit.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A. LDA >= max(1,K).
*> \endverbatim
*>
*> \param[in] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (K)
*>          TAU(i) must contain the scalar factor of the elementary
*>          reflector H(i), as returned by DTZRZF.
*> \endverbatim
*>
*> \param[in,out] C
*> \verbatim
*>          C is DOUBLE PRECISION array, dimension (LDC,N)
*>          On entry, the M-by-N matrix C.
*>          On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
*> \endverbatim
*>
*> \param[in] LDC
*> \verbatim
*>          LDC is INTEGER
*>          The leading dimension of the array C. LDC >= max(1,M).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.
*>          If SIDE = 'L', LWORK >= max(1,N);
*>          if SIDE = 'R', LWORK >= max(1,M).
*>          For good performance, LWORK should generally be larger.
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup unmrz
*
*> \par Contributors:
*  ==================
*>
*>    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DORMRZ( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C,
     $                   LDC,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, L, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NBMAX, LDT, TSIZE
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1,
     $                     TSIZE = LDT*NBMAX )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      CHARACTER          OPTS*2, TRANST
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWT, JA, JC,
     $                   LDWORK, LWKOPT, MI, NB, NBMIN, NI, NQ, NW
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARZB, DLARZT, DORMR3, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
      OPTS( 1: 1 ) = SIDE
      OPTS( 2: 2 ) = TRANS
*
*     NQ is the order of Q and NW is the minimum dimension of WORK
*
      IF( LEFT ) THEN
         NQ = M
         NW = MAX( 1, N )
      ELSE
         NQ = N
         NW = MAX( 1, M )
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( L.LT.0 .OR. ( LEFT .AND. ( L.GT.M ) ) .OR.
     $         ( .NOT.LEFT .AND. ( L.GT.N ) ) ) THEN
         INFO = -6
      ELSE IF( LDA.LT.MAX( 1, K ) ) THEN
         INFO = -8
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -11
      ELSE IF( LWORK.LT.NW .AND. .NOT.LQUERY ) THEN
         INFO = -13
      END IF
*
      IF( INFO.EQ.0 ) THEN
*
*        Compute the workspace requirements
*
         IF( M.EQ.0 .OR. N.EQ.0 ) THEN
            LWKOPT = 1
         ELSE
            NB = MIN( NBMAX, ILAENV( 1, 'DORMRQ', OPTS, M,
     $                N,
     $                               K, -1 ) )
            LWKOPT = NW*NB + TSIZE
         END IF
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMRZ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IF( LWORK.LT.LWKOPT ) THEN
            NB = (LWORK-TSIZE) / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'DORMRQ', OPTS, M, N,
     $                   K,
     $              -1 ) )
         END IF
      END IF
*
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
*
*        Use unblocked code
*
         CALL DORMR3( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C, LDC,
     $                WORK, IINFO )
      ELSE
*
*        Use blocked code
*
         IWT = 1 + NW*NB
         IF( ( LEFT .AND. .NOT.NOTRAN ) .OR.
     $       ( .NOT.LEFT .AND. NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         END IF
*
         IF( LEFT ) THEN
            NI = N
            JC = 1
            JA = M - L + 1
         ELSE
            MI = M
            IC = 1
            JA = N - L + 1
         END IF
*
         IF( NOTRAN ) THEN
            TRANST = 'T'
         ELSE
            TRANST = 'N'
         END IF
*
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
*
*           Form the triangular factor of the block reflector
*           H = H(i+ib-1) . . . H(i+1) H(i)
*
            CALL DLARZT( 'Backward', 'Rowwise', L, IB, A( I, JA ),
     $                   LDA,
     $                   TAU( I ), WORK( IWT ), LDT )
*
            IF( LEFT ) THEN
*
*              H or H**T is applied to C(i:m,1:n)
*
               MI = M - I + 1
               IC = I
            ELSE
*
*              H or H**T is applied to C(1:m,i:n)
*
               NI = N - I + 1
               JC = I
            END IF
*
*           Apply H or H**T
*
            CALL DLARZB( SIDE, TRANST, 'Backward', 'Rowwise', MI, NI,
     $                   IB, L, A( I, JA ), LDA, WORK( IWT ), LDT,
     $                   C( IC, JC ), LDC, WORK, LDWORK )
   10    CONTINUE
*
      END IF
*
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DORMRZ
*
      END
*> \brief \b DTRSM
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE DTRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
*
*       .. Scalar Arguments ..
*       DOUBLE PRECISION ALPHA
*       INTEGER LDA,LDB,M,N
*       CHARACTER DIAG,SIDE,TRANSA,UPLO
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION A(LDA,*),B(LDB,*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DTRSM  solves one of the matrix equations
*>
*>    op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
*>
*> where alpha is a scalar, X and B are m by n matrices, A is a unit, or
*> non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
*>
*>    op( A ) = A   or   op( A ) = A**T.
*>
*> The matrix X is overwritten on B.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] SIDE
*> \verbatim
*>          SIDE is CHARACTER*1
*>           On entry, SIDE specifies whether op( A ) appears on the left
*>           or right of X as follows:
*>
*>              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
*>
*>              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
*> \endverbatim
*>
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>           On entry, UPLO specifies whether the matrix A is an upper or
*>           lower triangular matrix as follows:
*>
*>              UPLO = 'U' or 'u'   A is an upper triangular matrix.
*>
*>              UPLO = 'L' or 'l'   A is a lower triangular matrix.
*> \endverbatim
*>
*> \param[in] TRANSA
*> \verbatim
*>          TRANSA is CHARACTER*1
*>           On entry, TRANSA specifies the form of op( A ) to be used in
*>           the matrix multiplication as follows:
*>
*>              TRANSA = 'N' or 'n'   op( A ) = A.
*>
*>              TRANSA = 'T' or 't'   op( A ) = A**T.
*>
*>              TRANSA = 'C' or 'c'   op( A ) = A**T.
*> \endverbatim
*>
*> \param[in] DIAG
*> \verbatim
*>          DIAG is CHARACTER*1
*>           On entry, DIAG specifies whether or not A is unit triangular
*>           as follows:
*>
*>              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
*>
*>              DIAG = 'N' or 'n'   A is not assumed to be unit
*>                                  triangular.
*> \endverbatim
*>
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>           On entry, M specifies the number of rows of B. M must be at
*>           least zero.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>           On entry, N specifies the number of columns of B.  N must be
*>           at least zero.
*> \endverbatim
*>
*> \param[in] ALPHA
*> \verbatim
*>          ALPHA is DOUBLE PRECISION.
*>           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
*>           zero then  A is not referenced and  B need not be set before
*>           entry.
*> \endverbatim
*>
*> \param[in] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension ( LDA, k ),
*>           where k is m when SIDE = 'L' or 'l'
*>             and k is n when SIDE = 'R' or 'r'.
*>           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
*>           upper triangular part of the array  A must contain the upper
*>           triangular matrix  and the strictly lower triangular part of
*>           A is not referenced.
*>           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
*>           lower triangular part of the array  A must contain the lower
*>           triangular matrix  and the strictly upper triangular part of
*>           A is not referenced.
*>           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
*>           A  are not referenced either,  but are assumed to be  unity.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>           On entry, LDA specifies the first dimension of A as declared
*>           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
*>           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
*>           then LDA must be at least max( 1, n ).
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is DOUBLE PRECISION array, dimension ( LDB, N )
*>           Before entry,  the leading  m by n part of the array  B must
*>           contain  the  right-hand  side  matrix  B,  and  on exit  is
*>           overwritten by the solution matrix  X.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>           On entry, LDB specifies the first dimension of B as declared
*>           in  the  calling  (sub)  program.   LDB  must  be  at  least
*>           max( 1, m ).
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup trsm
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  Level 3 Blas routine.
*>
*>
*>  -- Written on 8-February-1989.
*>     Jack Dongarra, Argonne National Laboratory.
*>     Iain Duff, AERE Harwell.
*>     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*>     Sven Hammarling, Numerical Algorithms Group Ltd.
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DTRSM(SIDE,UPLO,TRANSA,DIAG,M,N,ALPHA,A,LDA,B,LDB)
*
*  -- Reference BLAS level3 routine --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION ALPHA
      INTEGER LDA,LDB,M,N
      CHARACTER DIAG,SIDE,TRANSA,UPLO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(LDA,*),B(LDB,*)
*     ..
*
*  =====================================================================
*
*     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC MAX
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION TEMP
      INTEGER I,INFO,J,K,NROWA
      LOGICAL LSIDE,NOUNIT,UPPER
*     ..
*     .. Parameters ..
      DOUBLE PRECISION ONE,ZERO
      PARAMETER (ONE=1.0D+0,ZERO=0.0D+0)
*     ..
*
*     Test the input parameters.
*
      LSIDE = LSAME(SIDE,'L')
      IF (LSIDE) THEN
          NROWA = M
      ELSE
          NROWA = N
      END IF
      NOUNIT = LSAME(DIAG,'N')
      UPPER = LSAME(UPLO,'U')
*
      INFO = 0
      IF ((.NOT.LSIDE) .AND. (.NOT.LSAME(SIDE,'R'))) THEN
          INFO = 1
      ELSE IF ((.NOT.UPPER) .AND. (.NOT.LSAME(UPLO,'L'))) THEN
          INFO = 2
      ELSE IF ((.NOT.LSAME(TRANSA,'N')) .AND.
     +         (.NOT.LSAME(TRANSA,'T')) .AND.
     +         (.NOT.LSAME(TRANSA,'C'))) THEN
          INFO = 3
      ELSE IF ((.NOT.LSAME(DIAG,'U')) .AND.
     +         (.NOT.LSAME(DIAG,'N'))) THEN
          INFO = 4
      ELSE IF (M.LT.0) THEN
          INFO = 5
      ELSE IF (N.LT.0) THEN
          INFO = 6
      ELSE IF (LDA.LT.MAX(1,NROWA)) THEN
          INFO = 9
      ELSE IF (LDB.LT.MAX(1,M)) THEN
          INFO = 11
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('DTRSM ',INFO)
          RETURN
      END IF
*
*     Quick return if possible.
*
      IF (M.EQ.0 .OR. N.EQ.0) RETURN
*
*     And when  alpha.eq.zero.
*
      IF (ALPHA.EQ.ZERO) THEN
          DO 20 J = 1,N
              DO 10 I = 1,M
                  B(I,J) = ZERO
   10         CONTINUE
   20     CONTINUE
          RETURN
      END IF
*
*     Start the operations.
*
      IF (LSIDE) THEN
          IF (LSAME(TRANSA,'N')) THEN
*
*           Form  B := alpha*inv( A )*B.
*
              IF (UPPER) THEN
                  DO 60 J = 1,N
                      IF (ALPHA.NE.ONE) THEN
                          DO 30 I = 1,M
                              B(I,J) = ALPHA*B(I,J)
   30                     CONTINUE
                      END IF
                      DO 50 K = M,1,-1
                          IF (B(K,J).NE.ZERO) THEN
                              IF (NOUNIT) B(K,J) = B(K,J)/A(K,K)
                              DO 40 I = 1,K - 1
                                  B(I,J) = B(I,J) - B(K,J)*A(I,K)
   40                         CONTINUE
                          END IF
   50                 CONTINUE
   60             CONTINUE
              ELSE
                  DO 100 J = 1,N
                      IF (ALPHA.NE.ONE) THEN
                          DO 70 I = 1,M
                              B(I,J) = ALPHA*B(I,J)
   70                     CONTINUE
                      END IF
                      DO 90 K = 1,M
                          IF (B(K,J).NE.ZERO) THEN
                              IF (NOUNIT) B(K,J) = B(K,J)/A(K,K)
                              DO 80 I = K + 1,M
                                  B(I,J) = B(I,J) - B(K,J)*A(I,K)
   80                         CONTINUE
                          END IF
   90                 CONTINUE
  100             CONTINUE
              END IF
          ELSE
*
*           Form  B := alpha*inv( A**T )*B.
*
              IF (UPPER) THEN
                  DO 130 J = 1,N
                      DO 120 I = 1,M
                          TEMP = ALPHA*B(I,J)
                          DO 110 K = 1,I - 1
                              TEMP = TEMP - A(K,I)*B(K,J)
  110                     CONTINUE
                          IF (NOUNIT) TEMP = TEMP/A(I,I)
                          B(I,J) = TEMP
  120                 CONTINUE
  130             CONTINUE
              ELSE
                  DO 160 J = 1,N
                      DO 150 I = M,1,-1
                          TEMP = ALPHA*B(I,J)
                          DO 140 K = I + 1,M
                              TEMP = TEMP - A(K,I)*B(K,J)
  140                     CONTINUE
                          IF (NOUNIT) TEMP = TEMP/A(I,I)
                          B(I,J) = TEMP
  150                 CONTINUE
  160             CONTINUE
              END IF
          END IF
      ELSE
          IF (LSAME(TRANSA,'N')) THEN
*
*           Form  B := alpha*B*inv( A ).
*
              IF (UPPER) THEN
                  DO 210 J = 1,N
                      IF (ALPHA.NE.ONE) THEN
                          DO 170 I = 1,M
                              B(I,J) = ALPHA*B(I,J)
  170                     CONTINUE
                      END IF
                      DO 190 K = 1,J - 1
                          IF (A(K,J).NE.ZERO) THEN
                              DO 180 I = 1,M
                                  B(I,J) = B(I,J) - A(K,J)*B(I,K)
  180                         CONTINUE
                          END IF
  190                 CONTINUE
                      IF (NOUNIT) THEN
                          TEMP = ONE/A(J,J)
                          DO 200 I = 1,M
                              B(I,J) = TEMP*B(I,J)
  200                     CONTINUE
                      END IF
  210             CONTINUE
              ELSE
                  DO 260 J = N,1,-1
                      IF (ALPHA.NE.ONE) THEN
                          DO 220 I = 1,M
                              B(I,J) = ALPHA*B(I,J)
  220                     CONTINUE
                      END IF
                      DO 240 K = J + 1,N
                          IF (A(K,J).NE.ZERO) THEN
                              DO 230 I = 1,M
                                  B(I,J) = B(I,J) - A(K,J)*B(I,K)
  230                         CONTINUE
                          END IF
  240                 CONTINUE
                      IF (NOUNIT) THEN
                          TEMP = ONE/A(J,J)
                          DO 250 I = 1,M
                              B(I,J) = TEMP*B(I,J)
  250                     CONTINUE
                      END IF
  260             CONTINUE
              END IF
          ELSE
*
*           Form  B := alpha*B*inv( A**T ).
*
              IF (UPPER) THEN
                  DO 310 K = N,1,-1
                      IF (NOUNIT) THEN
                          TEMP = ONE/A(K,K)
                          DO 270 I = 1,M
                              B(I,K) = TEMP*B(I,K)
  270                     CONTINUE
                      END IF
                      DO 290 J = 1,K - 1
                          IF (A(J,K).NE.ZERO) THEN
                              TEMP = A(J,K)
                              DO 280 I = 1,M
                                  B(I,J) = B(I,J) - TEMP*B(I,K)
  280                         CONTINUE
                          END IF
  290                 CONTINUE
                      IF (ALPHA.NE.ONE) THEN
                          DO 300 I = 1,M
                              B(I,K) = ALPHA*B(I,K)
  300                     CONTINUE
                      END IF
  310             CONTINUE
              ELSE
                  DO 360 K = 1,N
                      IF (NOUNIT) THEN
                          TEMP = ONE/A(K,K)
                          DO 320 I = 1,M
                              B(I,K) = TEMP*B(I,K)
  320                     CONTINUE
                      END IF
                      DO 340 J = K + 1,N
                          IF (A(J,K).NE.ZERO) THEN
                              TEMP = A(J,K)
                              DO 330 I = 1,M
                                  B(I,J) = B(I,J) - TEMP*B(I,K)
  330                         CONTINUE
                          END IF
  340                 CONTINUE
                      IF (ALPHA.NE.ONE) THEN
                          DO 350 I = 1,M
                              B(I,K) = ALPHA*B(I,K)
  350                     CONTINUE
                      END IF
  360             CONTINUE
              END IF
          END IF
      END IF
*
      RETURN
*
*     End of DTRSM
*
      END
*> \brief \b DTZRZF
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download DTZRZF + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dtzrzf.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dtzrzf.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dtzrzf.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE DTZRZF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*       .. Scalar Arguments ..
*       INTEGER            INFO, LDA, LWORK, M, N
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DTZRZF reduces the M-by-N ( M<=N ) real upper trapezoidal matrix A
*> to upper triangular form by means of orthogonal transformations.
*>
*> The upper trapezoidal matrix A is factored as
*>
*>    A = ( R  0 ) * Z,
*>
*> where Z is an N-by-N orthogonal matrix and R is an M-by-M upper
*> triangular matrix.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          The number of rows of the matrix A.  M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The number of columns of the matrix A.  N >= M.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is DOUBLE PRECISION array, dimension (LDA,N)
*>          On entry, the leading M-by-N upper trapezoidal part of the
*>          array A must contain the matrix to be factorized.
*>          On exit, the leading M-by-M upper triangular part of A
*>          contains the upper triangular matrix R, and elements M+1 to
*>          N of the first M rows of A, with the array TAU, represent the
*>          orthogonal matrix Z as a product of M elementary reflectors.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,M).
*> \endverbatim
*>
*> \param[out] TAU
*> \verbatim
*>          TAU is DOUBLE PRECISION array, dimension (M)
*>          The scalar factors of the elementary reflectors.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.  LWORK >= max(1,M).
*>          For optimum performance LWORK >= M*NB, where NB is
*>          the optimal blocksize.
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup tzrzf
*
*> \par Contributors:
*  ==================
*>
*>    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  The N-by-N matrix Z can be computed by
*>
*>     Z =  Z(1)*Z(2)* ... *Z(M)
*>
*>  where each N-by-N Z(k) is given by
*>
*>     Z(k) = I - tau(k)*v(k)*v(k)**T
*>
*>  with v(k) is the kth row vector of the M-by-N matrix
*>
*>     V = ( I   A(:,M+1:N) )
*>
*>  I is the M-by-M identity matrix, A(:,M+1:N)
*>  is the output stored in A on exit from DTZRZF,
*>  and tau(k) is the kth element of the array TAU.
*>
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE DTZRZF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, IWS, KI, KK, LDWORK, LWKMIN, LWKOPT,
     $                   M1, MU, NB, NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, DLARZB, DLARZT, DLATRZ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.M ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( M.EQ.0 .OR. M.EQ.N ) THEN
            LWKOPT = 1
            LWKMIN = 1
         ELSE
*
*           Determine the block size.
*
            NB = ILAENV( 1, 'DGERQF', ' ', M, N, -1, -1 )
            LWKOPT = M*NB
            LWKMIN = MAX( 1, M )
         END IF
         WORK( 1 ) = LWKOPT
*
         IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY ) THEN
            INFO = -7
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTZRZF', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 ) THEN
         RETURN
      ELSE IF( M.EQ.N ) THEN
         DO 10 I = 1, N
            TAU( I ) = ZERO
   10    CONTINUE
         RETURN
      END IF
*
      NBMIN = 2
      NX = 1
      IWS = M
      IF( NB.GT.1 .AND. NB.LT.M ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'DGERQF', ' ', M, N, -1, -1 ) )
         IF( NX.LT.M ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = M
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'DGERQF', ' ', M, N, -1,
     $                 -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.M .AND. NX.LT.M ) THEN
*
*        Use blocked code initially.
*        The last kk rows are handled by the block method.
*
         M1 = MIN( M+1, N )
         KI = ( ( M-NX-1 ) / NB )*NB
         KK = MIN( M, KI+NB )
*
         DO 20 I = M - KK + KI + 1, M - KK + 1, -NB
            IB = MIN( M-I+1, NB )
*
*           Compute the TZ factorization of the current block
*           A(i:i+ib-1,i:n)
*
            CALL DLATRZ( IB, N-I+1, N-M, A( I, I ), LDA, TAU( I ),
     $                   WORK )
            IF( I.GT.1 ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i+ib-1) . . . H(i+1) H(i)
*
               CALL DLARZT( 'Backward', 'Rowwise', N-M, IB, A( I,
     $                      M1 ),
     $                      LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H to A(1:i-1,i:n) from the right
*
               CALL DLARZB( 'Right', 'No transpose', 'Backward',
     $                      'Rowwise', I-1, N-I+1, IB, N-M, A( I, M1 ),
     $                      LDA, WORK, LDWORK, A( 1, I ), LDA,
     $                      WORK( IB+1 ), LDWORK )
            END IF
   20    CONTINUE
         MU = I + NB - 1
      ELSE
         MU = M
      END IF
*
*     Use unblocked code to factor the last or only block
*
      IF( MU.GT.0 )
     $   CALL DLATRZ( MU, N, N-M, A, LDA, TAU, WORK )
*
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DTZRZF
*
      END
