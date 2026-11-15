# The SPARK LAPACK eigenvalue solver

`Prunt.LAPACK` and its children are a SPARK port of the part of LAPACK which Prunt uses: the eigenvalues of a
general real matrix, as `DGEEV` computes them when no eigenvectors are wanted (`JOBVL = JOBVR = 'N'`).
`Find_Roots` in `Prunt.Motion_Planner.Stereographic_Curves` calls it on the companion matrix of a polynomial of
degree at most 16 to find the poles of a corner blend, when the simultaneous iteration of `Iterate_Conjugate_Roots`
fails, which it does for about 1.3% of the blends of a real print.

The port performs the floating-point operations of the reference implementation (LAPACK 3.12.1, the latest
release) in the same order, so on a target which rounds every operation on its own it returns the same bits. The
Reference-LAPACK master branch at 902162d (2026-11-15) changes the ported path in two places, and neither can fire
on input the port accepts: `DGEEV` now returns `INFO = -4` for a NaN or an infinity in A, and `DLARFG` scales down
when |BETA| exceeds half the overflow threshold, which the port's proved bound on BETA (2**986) rules out. The
other changes are to non-finite input, negative increments, and routines which `DGEEV` does not run without
eigenvectors.

`./prove.sh` proves every check of these units at level 2, with no `pragma Assume` and no justified check.

## What is ported

| Unit | Reference routine | Notes |
|---|---|---|
| `Prunt.LAPACK.Eigenvalues.Solve` | `DGEEV`, `DHSEQR`, `DLASCL` | The driver: scale, balance, reduce, iterate, scale back. |
| `Prunt.LAPACK.Balancing.Balance` | `DGEBAL` with `JOB = 'B'` | |
| `Prunt.LAPACK.Hessenberg_Reduction.Reduce` | `DGEHRD` = `DGEHD2`, `DLARF1F`, `ILADLC`, `ILADLR`, `DGEMV`, `DGER`, `DAXPY`, `DSCAL` | |
| `Prunt.LAPACK.Hessenberg_QR.Eigenvalues` | `DLAHQR` with `WANTT = WANTZ = .FALSE.` | The double-shift QR iteration. |
| `Prunt.LAPACK.Schur_Blocks.Eigenvalues_Of_Block` | `DLANV2` | Without the rotation it also returns. |
| `Prunt.LAPACK.Reflectors.Generate` | `DLARFG` | |
| `Prunt.LAPACK.Norms` | `DLANGE` with `NORM = 'M'`, `DNRM2`, `DLAPY2`, `IDAMAX` | |
| `Prunt.LAPACK` | `DLAMCH` | The machine constants, as named numbers. |
| `Prunt.LAPACK.Elementary` | `SQRT`, `SIGN` | Not in SPARK: the trusted base. |
| `Prunt.LAPACK.Bounds` | | Proved lemmas about one or two rounded operations. |

`Maximum_Order` is 32. Up to that order the reference implementation reduces a matrix to Hessenberg form with its
unblocked code and finds the eigenvalues with `DLAHQR`, the two algorithms ported here; beyond it `DGEHRD` switches
to blocked code, and beyond 75 `DHSEQR` to the multishift algorithm, which round differently.

## What is proved

For every call of `Prunt.LAPACK.Eigenvalues.Solve` with a matrix of order at most 32 whose leading block and
result vectors are indexed from one (a SPARK value of a floating-point type is always a number, so the elements
need no condition):

1. **No run-time error.** No index is out of range, no integer overflows, nothing is divided by zero, and no
   floating-point operation overflows: every intermediate and every result is a number. In particular no
   exception is raised and no infinity or NaN is produced.
2. **Termination.** Every loop ends. The QR iteration is bounded by LAPACK's own count; balancing by the two
   limits of difference 4 below.
3. **Data flow.** Every result is assigned before it is read, and nothing but the parameters is read or written.
4. **Pairing** (`Is_Paired`). When `Info` is zero, every eigenvalue is real (`Wi (J) = 0`), or `Wi (J) > 0` and
   eigenvalue `J + 1` is its exact complex conjugate, or `Wi (J) < 0` and eigenvalue `J - 1` is: complex
   eigenvalues come in pairs of exact conjugates stored together, the one in the upper half plane first. This is
   what Prunt's root refinement and validation rely on to treat a pair once, and what `DGEEV` documents.
5. **`Info`** is zero, or the row at which the QR iteration gave up, or `Out_Of_Range`.

The units are proved against one another's contracts, which state more than `Solve` passes on:

- `Reflectors.Generate`: the scalar factor of a reflector is zero or between one and two; no element of its
  vector is larger than two; when the factor is zero the vector is zero and the first element is unchanged; for
  the orders two and three which the QR iteration uses, the new first element is at most four times the largest
  input.
- `Norms`: the result of `Hypotenuse` is at least each argument's magnitude and at most twice the larger; `DNRM2`
  of numbers of medium size is the root of the plain sum of their squares, which is at least each square.
- `Schur_Blocks.Eigenvalues_Of_Block`: the two eigenvalues are real, or an exact conjugate pair with the first
  in the upper half plane.
- `Hessenberg_QR.Eigenvalues`: the pairing of the eigenvalues it found, also when it gave up, and that it
  leaves the eigenvalues outside `Ilo .. Ihi` alone.
- `Balancing.Balance`: `Ilo <= Ihi + 1 <= N + 1`, and no element larger than 2**972.

Not proved: that the results are close to the eigenvalues of the matrix. That is the backward stability of the
QR algorithm, which the port inherits by performing the reference's operations, and Prunt checks every root it
uses against the polynomial itself. Nor that the run-time tests of "Magnitudes" below never fail.

### The trusted base

`Prunt.LAPACK.Elementary` is the only code which is not in SPARK. Its contracts state, without proof, that
`Sqrt` is zero for zero and positive otherwise, that it is monotonic, that `Sqrt (X * X) = X` for `X` between
2**-511 and 2**511 (true of a correctly rounded square root in radix two: S. Boldo, 2015), and that `Sign` returns
the magnitude of its first argument with the sign of its second. The unit tests of `Prunt.LAPACK.Test` check all
four on the machine they run on. Nothing else is assumed.

## Where the port differs from the reference

1. **No second QR algorithm.** When `DLAHQR` has not found every eigenvalue after `30 * max (10, n)` iterations
   per eigenvalue, `DHSEQR` runs the multishift algorithm `DLAQR0` on what is left, which brings in most of
   LAPACK's eigenvalue code. The port returns the positive `Info` of `DLAHQR` instead, with the eigenvalues it had
   found, as `DGEEV` itself does when `DLAQR0` fails too. This is rare, and in practice needs denormalized
   elements.
2. **No infinities.** A SPARK value of a floating-point type is a number. Where the reference implementation
   returns an infinity or a NaN (an eigenvalue of a matrix with elements near the largest number which does not
   fit the type; a zero divided by zero in the search for where to start a QR step), the port returns
   `Info = Out_Of_Range`. It also does so when scaling an eigenvalue back would bring it within a factor of four of
   the largest number, although the reference implementation still returns a number there.
3. **Bounds tested at run time.** See "Magnitudes" below: the port returns `Info = Out_Of_Range` if a magnitude
   which mathematics bounds, but the proof does not, fails a test. No matrix is known which makes it do so.
4. **Balancing is bounded.** `DGEBAL` repeats its passes of norm reduction until one changes nothing. The port
   stops after 1,024 passes, and its two loops which double and halve a scale factor after 2,048 steps (more than
   the factor's range allows). A matrix balanced less than the reference would balance it has the same
   eigenvalues.
5. **Only what eigenvalues need is computed.** The Hessenberg reduction transforms rows and columns `Ilo .. Ihi`
   only, not the rest of the rows and columns of the matrix, which nothing reads again; `DLANV2`'s rotation is
   not formed; the permutation and the scale factors of balancing and the reflector factors `TAU` are not
   returned; `DLASCL`'s loop is the single multiplication it always performs between the scales `DGEEV` uses.
   The matrix on return is therefore not the reference's, whose contents `DGEEV` leaves unspecified.
6. **`Wr` and `Wi` are set to zero first.** The reference leaves the elements it does not compute as they were.
7. **Input.** The matrix and the vectors are indexed from one, the matrix in the Ada order, and every element is
   a number. There are no workspace arguments.

## Magnitudes: how overflow is excluded

LAPACK relies on arithmetic which carries on after an overflow. GNATprove requires every operation to stay within
the type, so the port carries bounds:

- `DGEEV` scales the matrix so that its largest element is within 2**459. After balancing no element is larger
  than `Balanced_Limit`, 2**972; the reference implementation itself stops enlarging a row or a column before
  its largest element reaches 2**970.
- A reflector is generated from numbers within `Entry_Limit`, 2**980. Its scalar factor is zero or between one and
  two, and no element of its vector is larger than two.
- The QR iteration keeps a number, `Limit`, which no element of the active block exceeds. One reflector, applied
  from both sides, can multiply it by 4,096 as far as the proof follows; when it passes 2**880 the block is
  scanned for its largest element, which becomes the new `Limit`.

What the proof does not follow is that a reflector is orthogonal, so that in fact it enlarges no element by more
than the square root of its order, and that a norm is at least as large as each element and at most their sum when
`DNRM2` had to scale the squares. Where the port relies on such a fact it tests it:

| Where | Test | Why it holds |
|---|---|---|
| `Hessenberg_QR` | the scanned largest element of the block is within 2**800 | the matrix was scaled to 2**459 and balanced, and orthogonal transformations preserve its norm |
| `Hessenberg_Reduction` | after a reflector which is not the identity, the largest element of the block is within 2**972 | as above |
| `Balancing` | the largest element of a row or column times its new factor is within 2**971 | the reference's own loop conditions keep the product below 2**970 |
| `Reflectors`, only when an element of the vector is outside 2**-511 .. 2**486 and not zero, or the vector is rescaled | the norm is not zero; no element is larger than the divisor (or than one, before it is rescaled); the rescaled first element is at least 2**-969; for the orders two and three, the new first element is within four times the largest input | a norm is at least each element and at most their sum; one rescaling by 2**969 lifts any number above the threshold |
| `Schur_Blocks` | the shifted root is not zero; a quotient times `Bcmis` is within 2**961; the hypotenuse is at least 2**-485 | the first is proved and kept; the product is at most the larger of two elements; the scaling loop leaves its operands above 2**-485 |
| `Eigenvalues` | an eigenvalue scaled back up is within 2**1023 (within 2**1000 before it is scaled back down) | the first is not always so: see difference 2 |

The tests are on paths which run rarely, or once per reflector, except the scans, which cost one comparison per
element of the block each time. For a dense matrix the scan after each reflector of the Hessenberg reduction costs
about what inlining saves; a companion matrix is already of Hessenberg form, so it pays for no such scan.

## Reproducing

```sh
./prove.sh                                  # flow analysis and proof of every SPARK unit of Prunt, these included
./gnatprove.sh --output=oneline prunt-lapack-eigenvalues.adb    # one unit
PRUNT_SKIP_VALIDATION=true PRUNT_XCOV_DUMP=false PRUNT_SKIP_INTEGRATION=true ./run_tests.sh lapack
```

`prove.sh` and `gnatprove.sh` are described in [spark_trajectory.md](spark_trajectory.md). The units are proved
with COLIBRI besides the provers of level 2 (the `Prove` package of `prunt.gpr`); without it some of the lemmas
of `Bounds` are not proved. Do not pass `-f`: an unchanged goal is taken from the session of the last run. Level 2
gives each prover five seconds of wall-clock time, which the slowest goal of these units needs most of on a lightly
loaded machine, so on a busy machine add `--timeout=20` or more.

## How Prunt uses it

`Find_Roots` fills the leading block of order `Degree` of a 16 by 16 `Real_Matrix` with the companion matrix of
the monic polynomial (`Matrix (Row, Row - 1) := 1.0`, `Matrix (Row, Degree) := -Coefficient`) and calls
`Prunt.LAPACK.Eigenvalues.Solve`. It gives up on the roots when `Info` is not zero. `Refine_Roots` and
`Validate_Roots` rely on a conjugate pair being stored together with the root in the upper half plane first, which
is the postcondition `Is_Paired` of `Solve`, and every root is checked against the polynomial before it is used.

## Licence

The units are derived from the reference implementation of LAPACK and are distributed under its licence (SPDX
`BSD-3-Clause-Open-MPI`), which is at the top of each of their source files. The rest of Prunt is under the MIT
licence in `COPYING`.
