!> \brief \b DLARTG generates a plane rotation with real cosine and real sine.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       SUBROUTINE DLARTG( F, G, C, S, R )
!
!       .. Scalar Arguments ..
!       REAL(wp)          C, F, G, R, S
!       ..
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> DLARTG generates a plane rotation so that
!>
!>    [  C  S  ]  .  [ F ]  =  [ R ]
!>    [ -S  C  ]     [ G ]     [ 0 ]
!>
!> where C**2 + S**2 = 1.
!>
!> The mathematical formulas used for C and S are
!>    R = sign(F) * sqrt(F**2 + G**2)
!>    C = F / R
!>    S = G / R
!> Hence C >= 0. The algorithm used to compute these quantities
!> incorporates scaling to avoid overflow or underflow in computing the
!> square root of the sum of squares.
!>
!> This version is discontinuous in R at F = 0 but it returns the same
!> C and S as ZLARTG for complex inputs (F,0) and (G,0).
!>
!> This is a more accurate version of the BLAS1 routine DROTG,
!> with the following other differences:
!>    F and G are unchanged on return.
!>    If G=0, then C=1 and S=0.
!>    If F=0 and (G .ne. 0), then C=0 and S=sign(1,G) without doing any
!>       floating point operations (saves work in DBDSQR when
!>       there are zeros on the diagonal).
!>
!> Below, wp=>dp stands for double precision from LA_CONSTANTS module.
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] F
!> \verbatim
!>          F is REAL(wp)
!>          The first component of vector to be rotated.
!> \endverbatim
!>
!> \param[in] G
!> \verbatim
!>          G is REAL(wp)
!>          The second component of vector to be rotated.
!> \endverbatim
!>
!> \param[out] C
!> \verbatim
!>          C is REAL(wp)
!>          The cosine of the rotation.
!> \endverbatim
!>
!> \param[out] S
!> \verbatim
!>          S is REAL(wp)
!>          The sine of the rotation.
!> \endverbatim
!>
!> \param[out] R
!> \verbatim
!>          R is REAL(wp)
!>          The nonzero component of the rotated vector.
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Edward Anderson, Lockheed Martin
!
!> \date July 2016
!
!> \ingroup lartg
!
!> \par Contributors:
!  ==================
!>
!> Weslley Pereira, University of Colorado Denver, USA
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  Anderson E. (2017)
!>  Algorithm 978: Safe Scaling in the Level 1 BLAS
!>  ACM Trans Math Softw 44:1--28
!>  https://doi.org/10.1145/3061665
!>
!> \endverbatim
!
subroutine DLARTG( f, g, c, s, r )
   integer, parameter :: wp = kind(1.d0)
   real(wp), parameter :: zero = 0.0_wp
   real(wp), parameter :: one = 1.0_wp
   real(wp), parameter :: safmin = real(radix(0._wp), wp)**max( &
      minexponent(0._wp)-1, 1-maxexponent(0._wp))
   real(wp), parameter :: safmax = one / safmin
!
!  -- LAPACK auxiliary routine --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     February 2021
!
!  .. Scalar Arguments ..
   real(wp) :: c, f, g, r, s
!  ..
!  .. Local Scalars ..
   real(wp) :: d, f1, fs, g1, gs, u, rtmin, rtmax
!  ..
!  .. Intrinsic Functions ..
   intrinsic :: abs, sign, sqrt
!  ..
!  .. Constants ..
   rtmin = sqrt( safmin )
   rtmax = sqrt( safmax/2 )
!  ..
!  .. Executable Statements ..
!
   f1 = abs( f )
   g1 = abs( g )
   if( g == zero ) then
      c = one
      s = zero
      r = f
   else if( f == zero ) then
      c = zero
      s = sign( one, g )
      r = g1
   else if( f1 > rtmin .and. f1 < rtmax .and. &
            g1 > rtmin .and. g1 < rtmax ) then
      d = sqrt( f*f + g*g )
      c = f1 / d
      r = sign( d, f )
      s = g / r
   else
      u = min( safmax, max( safmin, f1, g1 ) )
      fs = f / u
      gs = g / u
      d = sqrt( fs*fs + gs*gs )
      c = abs( fs ) / d
      r = sign( d, f )
      s = gs / r
      r = r*u
   end if
   return
end subroutine
!> \brief \b DLASSQ updates a sum of squares represented in scaled form.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!> \htmlonly
!> Download DLASSQ + dependencies
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlassq.f90">
!> [TGZ]</a>
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlassq.f90">
!> [ZIP]</a>
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlassq.f90">
!> [TXT]</a>
!> \endhtmlonly
!
!  Definition:
!  ===========
!
!       SUBROUTINE DLASSQ( N, X, INCX, SCALE, SUMSQ )
!
!       .. Scalar Arguments ..
!       INTEGER            INCX, N
!       DOUBLE PRECISION   SCALE, SUMSQ
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION   X( * )
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> DLASSQ returns the values scale_out and sumsq_out such that
!>
!>    (scale_out**2)*sumsq_out = x( 1 )**2 +...+ x( n )**2 + (scale**2)*sumsq,
!>
!> where x( i ) = X( 1 + ( i - 1 )*INCX ). The value of sumsq is
!> assumed to be non-negative.
!>
!> scale and sumsq must be supplied in SCALE and SUMSQ and
!> scale_out and sumsq_out are overwritten on SCALE and SUMSQ respectively.
!>
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>          The number of elements to be used from the vector x.
!> \endverbatim
!>
!> \param[in] X
!> \verbatim
!>          X is DOUBLE PRECISION array, dimension (1+(N-1)*abs(INCX))
!>          The vector for which a scaled sum of squares is computed.
!>             x( i ) = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER
!>          The increment between successive values of the vector x.
!>          If INCX > 0, X(1+(i-1)*INCX) = x(i) for 1 <= i <= n
!>          If INCX < 0, X(1-(n-i)*INCX) = x(i) for 1 <= i <= n
!>          If INCX = 0, x isn't a vector so there is no need to call
!>          this subroutine. If you call it anyway, it will count x(1)
!>          in the vector norm N times.
!> \endverbatim
!>
!> \param[in,out] SCALE
!> \verbatim
!>          SCALE is DOUBLE PRECISION
!>          On entry, the value scale in the equation above.
!>          On exit, SCALE is overwritten by scale_out, the scaling factor
!>          for the sum of squares.
!> \endverbatim
!>
!> \param[in,out] SUMSQ
!> \verbatim
!>          SUMSQ is DOUBLE PRECISION
!>          On entry, the value sumsq in the equation above.
!>          On exit, SUMSQ is overwritten by sumsq_out, the basic sum of
!>          squares from which scale_out has been factored out.
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Edward Anderson, Lockheed Martin
!
!> \par Contributors:
!  ==================
!>
!> Weslley Pereira, University of Colorado Denver, USA
!> Nick Papior, Technical University of Denmark, DK
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  Anderson E. (2017)
!>  Algorithm 978: Safe Scaling in the Level 1 BLAS
!>  ACM Trans Math Softw 44:1--28
!>  https://doi.org/10.1145/3061665
!>
!>  Blue, James L. (1978)
!>  A Portable Fortran Program to Find the Euclidean Norm of a Vector
!>  ACM Trans Math Softw 4:15--23
!>  https://doi.org/10.1145/355769.355771
!>
!> \endverbatim
!
!> \ingroup lassq
!
!  =====================================================================
subroutine DLASSQ( n, x, incx, scale, sumsq )
   integer, parameter :: wp = kind(1.d0)
   real(wp), parameter :: zero = 0.0_wp
   real(wp), parameter :: one = 1.0_wp
   real(wp), parameter :: tsml = real(radix(0._wp), wp)**ceiling( &
      (minexponent(0._wp) - 1) * 0.5_wp)
   real(wp), parameter :: tbig = real(radix(0._wp), wp)**floor( &
      (maxexponent(0._wp) - digits(0._wp) + 1) * 0.5_wp)
   real(wp), parameter :: ssml = real(radix(0._wp), wp)**( - floor( &
      (minexponent(0._wp) - digits(0._wp)) * 0.5_wp))
   real(wp), parameter :: sbig = real(radix(0._wp), wp)**( - ceiling( &
      (maxexponent(0._wp) + digits(0._wp) - 1) * 0.5_wp))
!
!  -- LAPACK auxiliary routine --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!
!  .. Scalar Arguments ..
   integer :: incx, n
   real(wp) :: scale, sumsq
!  ..
!  .. Array Arguments ..
   real(wp) :: x(*)
!  ..
!  .. Local Scalars ..
   integer :: i, ix
   logical :: notbig
   real(wp) :: abig, amed, asml, ax, ymax, ymin
!  ..
!
!  Quick return if possible
!
   if( scale /= scale .or. sumsq /= sumsq ) return
   if( sumsq == zero ) scale = one
   if( scale == zero ) then
      scale = one
      sumsq = zero
   end if
   if (n <= 0) then
      return
   end if
!
!  Compute the sum of squares in 3 accumulators:
!     abig -- sums of squares scaled down to avoid overflow
!     asml -- sums of squares scaled up to avoid underflow
!     amed -- sums of squares that do not require scaling
!  The thresholds and multipliers are
!     tbig -- values bigger than this are scaled down by sbig
!     tsml -- values smaller than this are scaled up by ssml
!
   notbig = .true.
   asml = zero
   amed = zero
   abig = zero
   ix = 1
   if( incx < 0 ) ix = 1 - (n-1)*incx
   do i = 1, n
      ax = abs(x(ix))
      if (ax > tbig) then
         abig = abig + (ax*sbig)**2
         notbig = .false.
      else if (ax < tsml) then
         if (notbig) asml = asml + (ax*ssml)**2
      else
         amed = amed + ax**2
      end if
      ix = ix + incx
   end do
!
!  Put the existing sum of squares into one of the accumulators
!
   if( sumsq > zero ) then
      ax = scale*sqrt( sumsq )
      if (ax > tbig) then
         if (scale > one) then
            scale = scale * sbig
            abig = abig + scale * (scale * sumsq)
         else
            ! sumsq > tbig^2 => (sbig * (sbig * sumsq)) is representable
            abig = abig + scale * (scale * (sbig * (sbig * sumsq)))
         end if
      else if (ax < tsml) then
         if (notbig) then
            if (scale < one) then
               scale = scale * ssml
               asml = asml + scale * (scale * sumsq)
            else
               ! sumsq < tsml^2 => (ssml * (ssml * sumsq)) is representable
               asml = asml + scale * (scale * (ssml * (ssml * sumsq)))
            end if
         end if
      else
         amed = amed + scale * (scale * sumsq)
      end if
   end if
!
!  Combine abig and amed or amed and asml if more than one
!  accumulator was used.
!
   if (abig > zero) then
!
!     Combine abig and amed if abig > 0.
!
      if (amed > zero .or. amed /= amed) then
         abig = abig + (amed*sbig)*sbig
      end if
      scale = one / sbig
      sumsq = abig
   else if (asml > zero) then
!
!     Combine amed and asml if asml > 0.
!
      if (amed > zero .or. amed /= amed) then
         amed = sqrt(amed)
         asml = sqrt(asml) / ssml
         if (asml > amed) then
            ymin = amed
            ymax = asml
         else
            ymin = asml
            ymax = amed
         end if
         scale = one
         sumsq = ymax**2*( one + (ymin/ymax)**2 )
      else
         scale = one / ssml
         sumsq = asml
      end if
   else
!
!     Otherwise all values are mid-range or zero
!
      scale = one
      sumsq = amed
   end if
   return
end subroutine
!> \brief \b DNRM2
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
!  Definition:
!  ===========
!
!       DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)
!
!       .. Scalar Arguments ..
!       INTEGER INCX,N
!       ..
!       .. Array Arguments ..
!       DOUBLE PRECISION X(*)
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> DNRM2 returns the euclidean norm of a vector via the function
!> name, so that
!>
!>    DNRM2 := sqrt( x'*x )
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] N
!> \verbatim
!>          N is INTEGER
!>         number of elements in input vector(s)
!> \endverbatim
!>
!> \param[in] X
!> \verbatim
!>          X is DOUBLE PRECISION array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
!> \endverbatim
!>
!> \param[in] INCX
!> \verbatim
!>          INCX is INTEGER, storage spacing between elements of X
!>          If INCX > 0, X(1+(i-1)*INCX) = x(i) for 1 <= i <= n
!>          If INCX < 0, X(1-(n-i)*INCX) = x(i) for 1 <= i <= n
!>          If INCX = 0, x isn't a vector so there is no need to call
!>          this subroutine.  If you call it anyway, it will count x(1)
!>          in the vector norm N times.
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Edward Anderson, Lockheed Martin
!
!> \date August 2016
!
!> \ingroup nrm2
!
!> \par Contributors:
!  ==================
!>
!> Weslley Pereira, University of Colorado Denver, USA
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  Anderson E. (2017)
!>  Algorithm 978: Safe Scaling in the Level 1 BLAS
!>  ACM Trans Math Softw 44:1--28
!>  https://doi.org/10.1145/3061665
!>
!>  Blue, James L. (1978)
!>  A Portable Fortran Program to Find the Euclidean Norm of a Vector
!>  ACM Trans Math Softw 4:15--23
!>  https://doi.org/10.1145/355769.355771
!>
!> \endverbatim
!>
!  =====================================================================
function DNRM2( n, x, incx ) 
   integer, parameter :: wp = kind(1.d0)
   real(wp) :: DNRM2
!
!  -- Reference BLAS level1 routine (version 3.9.1) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     March 2021
!
!  .. Constants ..
   real(wp), parameter :: zero = 0.0_wp
   real(wp), parameter :: one  = 1.0_wp
   real(wp), parameter :: maxN = huge(0.0_wp)
!  ..
!  .. Blue's scaling constants ..
   real(wp), parameter :: tsml = real(radix(0._wp), wp)**ceiling( &
       (minexponent(0._wp) - 1) * 0.5_wp)
   real(wp), parameter :: tbig = real(radix(0._wp), wp)**floor( &
       (maxexponent(0._wp) - digits(0._wp) + 1) * 0.5_wp)
   real(wp), parameter :: ssml = real(radix(0._wp), wp)**( - floor( &
       (minexponent(0._wp) - digits(0._wp)) * 0.5_wp))
   real(wp), parameter :: sbig = real(radix(0._wp), wp)**( - ceiling( &
       (maxexponent(0._wp) + digits(0._wp) - 1) * 0.5_wp))
!  ..
!  .. Scalar Arguments ..
   integer :: incx, n
!  ..
!  .. Array Arguments ..
   real(wp) :: x(*)
!  ..
!  .. Local Scalars ..
   integer :: i, ix
   logical :: notbig
   real(wp) :: abig, amed, asml, ax, scl, sumsq, ymax, ymin
!
!  Quick return if possible
!
   DNRM2 = zero
   if( n <= 0 ) return
!
   scl = one
   sumsq = zero
!
!  Compute the sum of squares in 3 accumulators:
!     abig -- sums of squares scaled down to avoid overflow
!     asml -- sums of squares scaled up to avoid underflow
!     amed -- sums of squares that do not require scaling
!  The thresholds and multipliers are
!     tbig -- values bigger than this are scaled down by sbig
!     tsml -- values smaller than this are scaled up by ssml
!
   notbig = .true.
   asml = zero
   amed = zero
   abig = zero
   ix = 1
   if( incx < 0 ) ix = 1 - (n-1)*incx
   do i = 1, n
      ax = abs(x(ix))
      if (ax > tbig) then
         abig = abig + (ax*sbig)**2
         notbig = .false.
      else if (ax < tsml) then
         if (notbig) asml = asml + (ax*ssml)**2
      else
         amed = amed + ax**2
      end if
      ix = ix + incx
   end do
!
!  Combine abig and amed or amed and asml if more than one
!  accumulator was used.
!
   if (abig > zero) then
!
!     Combine abig and amed if abig > 0.
!
      if ( (amed > zero) .or. (amed > maxN) .or. (amed /= amed) ) then
         abig = abig + (amed*sbig)*sbig
      end if
      scl = one / sbig
      sumsq = abig
   else if (asml > zero) then
!
!     Combine amed and asml if asml > 0.
!
      if ( (amed > zero) .or. (amed > maxN) .or. (amed /= amed) ) then
         amed = sqrt(amed)
         asml = sqrt(asml) / ssml
         if (asml > amed) then
            ymin = amed
            ymax = asml
         else
            ymin = asml
            ymax = amed
         end if
         scl = one
         sumsq = ymax**2*( one + (ymin/ymax)**2 )
      else
         scl = one / ssml
         sumsq = asml
      end if
   else
!
!     Otherwise all values are mid-range
!
      scl = one
      sumsq = amed
   end if
   DNRM2 = scl*sqrt( sumsq )
   return
end function
