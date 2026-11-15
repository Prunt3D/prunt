# The SPARK stereographic corner blender

Prunt's stereographic corner blender, `Prunt.Motion_Planner.Stereographic_Curves` and its `Geometry` child, is
written in SPARK, with machine-checked proofs of the properties its certificates are meant to establish, for the
ideal curve and for the motion which is executed. The proved certificate kernel is
`Prunt.Motion_Planner.Stereographic_Kernel` and its children.

`./prove.sh` proves every check of the kernel and the postcondition of `Create_Blend`, with no `pragma Assume` and
no justified check. `Stereographic_Curves` and `Geometry` are proved free of run-time errors, except nine
subprograms of `Stereographic_Curves` which are only flow analysed (see "What is not proved").

## What is proved

Everything below holds for every curve which `Create_Blend` returns with `Blend_Success`, at **every** point of
the curve. The statements are theorems about exact real-number functions which the stored floating-point data
define, not about samples of them.

### The ideal curve

1. **Cone containment** (`Certified_Geometry.Lemma_In_Cone`). When the certificate's kind is `In_Cone`, the
   part of the ideal tangent in the plane of the endpoint tangents is a combination of the start tangent and
   the finish tangent with nonnegative weights (`Start_Weight`, `Finish_Weight`), and both endpoint tangents lie
   in that plane. This is the only kind a request with `Bulge => Never` can return.
2. **Chord progress** (`Certified_Geometry.Lemma_Chord_Progress`). When the kind is `Chord_Progress`, which
   only a request that allows bulging can return, the ideal tangent never has a negative component along the
   chord direction.
3. **Unit speed** (`Certified_Geometry.Lemma_Unit_Speed`). In frame coordinates the ideal tangent is exactly a
   unit vector. On the machine axes its squared length is within `4 * (Frame_Defect + 2**-45)` of one, where
   `Frame_Defect` is the measured distance of the frame from orthonormal (at most 2e-12 for any frame the
   construction accepts, so the speed is within about 4e-12 of one).
4. **Arc length**: positive and at most `Request.Maximum_Arc_Length`.

### The executed motion

The realtime evaluator does not evaluate the ideal curve. At normalized distance `U` along a curve of length `L`
it returns

    Start + L·∫₀ᵁ R + H(U)·(Finish - Uncorrected),

where `R` is the *retained tangent*, a rational function compiled from the ideal one and stored as a constant,
poles and residues, and the last term is the endpoint correction which makes the curve end on `Finish` exactly.
The tangent of the executed motion is therefore

    T(U) = R(U) + H′(U)·(Finish - Uncorrected)/L,

which is `Certified_Evaluator.Motion.Executed_Tangent`, for the real numbers which the curve's stored cache,
finish points and length denote. With `Total = Executed_Tangent_Error_Bound (Curve)`:

5. **The retained tangent is the ideal tangent, to within a certified bound**
   (`Certified_Evaluator.Lemma_Tangent_Error`). At every normalized distance the Euclidean distance between the
   ideal tangent and the retained tangent is at most `Certified_Evaluator.Tangent_Error`. This is the theorem of
   the *sampled tangent certificate* (`Sampled_Tangent.Bound.Lemma_Error`): the bound which construction computes
   from 33 samples of two error polynomials holds between the samples as well.
6. **The executed tangent is within `Total` of the ideal tangent** (`Motion.Lemma_Executed_Tangent`), along
   every direction. `Total` adds to the bound of 5 the largest rate of the endpoint correction,
   `693/256·‖Finish - Uncorrected‖/L`, rounded outwards (`Finish_Correction.Total_Tangent_Error_Bound`).
7. **Executed cone containment** (`Motion.Lemma_Executed_In_Cone`). When the kind is `In_Cone`, the executed
   tangent's component along every inward normal of the cone of the endpoint tangents, of length at most one, is
   at least `-Total`.
8. **Executed chord progress** (`Motion.Lemma_Executed_Chord_Progress`). When the kind is `Chord_Progress`, the
   executed tangent's component along the chord direction is at least `-Total` times that direction's length.
9. **Executed unit speed** (`Motion.Lemma_Executed_Speed`). The squared length of the executed tangent is within
   `4·δ + 2·(1 + 2·δ)·Total + Total²` of one, with `δ = Frame_Defect + 2**-45`. The length of the executed path
   is the integral of that speed over the planner's distance, so it is the planner's arc length to within the
   same fraction.

`Total` is largest for the shortest curves, and almost all of a large `Total` is the endpoint correction, which
spreads the construction's closure error over the curve.

### How the postcondition carries this

`Create_Blend`'s postcondition states that a successful result carries

- a geometry certificate of kind `In_Cone` or `Chord_Progress` for its own frame and chart
  (`Certificate_Matches`), and
- an evaluator certificate for that geometry certificate, its own distance warp and its own evaluator cache
  (`Evaluator_Matches`), with a finite `Executed_Tangent_Error_Bound` computed from the curve's own finish
  points and length (`Motion.Is_Evaluator`).

`Certified_Geometry.Certificate` and `Certified_Evaluator.Certificate` are private types whose invariants are
that the proved certificate functions accepted their contents; only `Certify` can make one. `Create_Blend` checks
the certificates of the curve it is about to return, so its postcondition is proved without depending on the
construction code being proved.

Each theorem takes the parameter (`V` in the chart, `U` along the curve) as an argument.
`Ideal_Geometry.Lemma_Warped_Is_In_Unit_Interval` shows that every normalized distance maps to a chart parameter
in [0, 1] under the curve's distance warp.

The sampled certificate bounds a polynomial of at most 33 coefficients by 3.5 times its largest magnitude at the
33 sample nodes. That constant is not assumed: `Sample_Nodes.Certify` checks, once at start-up and in about
3 ms, that the Lebesgue function of the nodes actually in the table is at most 3.5 on [0, 1], and
`Sample_Nodes.Lemma_Lebesgue` proves that a passed check means exactly that. No curve is certified if the check
fails.

The proofs of the kernel rest only on SPARKlib's floating-point rounding lemmas for `Long_Float`. The proofs that
the construction code is free of run-time errors rest in addition on a few stated bounds on the library's
elementary functions, in `Stereographic_Kernel.Elementary` (see "The construction code").

## What is not proved

- **That the evaluator's closed form is that integral, and how accurately it is computed.** The evaluator
  integrates the retained tangent in closed form, with logarithms and arctangents, and `H` is a fixed polynomial
  whose derivative is `Finish_Correction.Rate`. That the closed form is the antiderivative of `R`, and that
  `Rate` is `H′`, are calculus, which SPARK cannot state; the accuracy of the library's `Log` and `Arctan` and
  the rounding of the evaluator's own arithmetic are not analysed either. So theorems 5 to 9 are about the exact
  tangent which the stored numbers denote, and `Position_Error_Bound`, which budgets for the rest, is computed but
  not proved. That the evaluator returns `Start` and `Finish` exactly at the two ends is tested on every curve at
  construction, not proved.
- **The Bernstein reconstruction of the retained tangent** (`Certified_Realtime_Tangent_Error`), a slower second
  certificate. It does not decide whether a curve is accepted (see "The certificates"), but where it returns a
  smaller bound than the sampled certificate, `Retained_Tangent_Error_Bound` and `Position_Error_Bound` use it.
  `Executed_Tangent_Error_Bound` is always the proved one.
- **Absence of run-time errors in the compilation of the evaluator and in the derivative enclosures.** These
  recover from floating-point overflow by handling `Constraint_Error`, and no proof can show such a check never
  fails, because it is meant to fail. They carry the annotation `Skip_Proof` and keep their handlers and run-time
  checks; they pass flow analysis (initialization, aliasing, termination) only. None of them has a postcondition,
  so the proved code which calls them assumes only that they return. They are:
  - the compilation of the rational evaluator (`Build_Rational_Representation`);
  - the two enclosures of the retained tangent's derivatives (`Pole_Taylor_Tangent_Range_Majorants`,
    `Bernstein_Tangent_Range_Majorants`);
  - the Bernstein reconstruction (`Certified_Realtime_Tangent_Error`, both `Multiply_Bernstein`,
    `Elevate_Bernstein`);
  - `Strict_Bernstein_Nonnegative` and `Ideal_Point_At_Parameter`, which only the unit tests call.

## Layout

| Unit | Status | Contents |
|---|---|---|
| `Prunt.Motion_Planner.Stereographic_Kernel` | proved | The root of the kernel; the numeric and vector types are Prunt's own |
| `Prunt.Motion_Planner.Reals` | proved | Real-number model of the floats; rounding lemmas restated from SPARKlib; shared with `Trajectory` |
| `.Intervals`, `.Square_Roots` | proved | Outward-rounded interval arithmetic and square root, with enclosure contracts |
| `.Bernstein`, `.Bernstein.Binomial_Form` | proved (ghost) | Real polynomials in Bernstein form: convex hull, subdivision, products, endpoint roots |
| `.Interval_Polynomials`, `.Nonnegativity` | proved | Interval polynomials: product, nonnegativity by bisection |
| `.Ideal_Geometry` and children | proved | The geometry certificates and what each establishes |
| `.Certified_Geometry` | proved | The geometry certificate type and theorems 1 to 3 |
| `.Polynomials`, `.Polynomials.Interpolation` | proved (ghost) | Real polynomials in the power basis; the Lagrange interpolation bound |
| `.Sample_Nodes`, `.Sample_Nodes.Table` | proved | The 33 sample nodes and the start-up check of their Lebesgue function |
| `.Retained_Tangent` | proved (ghost model) | The evaluator cache's types and the real retained tangent they denote |
| `.Sampled_Tangent` and children | proved | The sampled tangent certificate: `Poles`, `Ideal_Side`, `Retained_Side`, `Nodes` compute it stage by stage, each with its rounding-error analysis; `Model` and `Structure` are the real functions and their polynomial structure; `Bound` holds the theorem |
| `.Finish_Correction` | proved | The endpoint correction's rate and the total tangent error bound |
| `.Certified_Evaluator`, `.Motion` | proved | The evaluator certificate type and theorems 5 to 9 |
| `.Elementary` | trusted, tested | What the construction's proofs assume of the library's square root, logarithm, arctangent and exponential |
| `Prunt.Motion_Planner.Stereographic_Curves` | proved free of run-time errors, except the subprograms marked `Skip_Proof` (flow only); `Create_Blend`'s postcondition proved | Construction, compilation, evaluation, bounds |
| `Prunt.Motion_Planner.Stereographic_Curves.Geometry` | proved free of run-time errors | Distances from a curve to a point and to a line corner |

The kernel and the construction units are children of `Prunt.Motion_Planner`. The construction units name the
kernel's units as `Stereographic_Kernel.Intervals` and the like; the private part of `Stereographic_Curves` uses
`Ideal_Geometry`, `Intervals` and `Retained_Tangent`, whose types it is built on, and `Elementary`. The
elementary functions of the kernel and the construction code are the one instance in `Elementary`,
`Dimensionless_Math`, which the trusted lemmas are about.

`Create_Blend` is declared in a nested package, `Certified_Construction`, and renamed under its own name. Its
postcondition speaks of ghost functions, which are never compiled, so it must be ignored even where assertions are
enabled; the nested package keeps that policy from the other contracts of `Stereographic_Curves`, which are executed
in Prunt's development builds.

The proved units begin with `pragma Assertion_Policy (Ignore)`: their contracts are proved, and the ghost code in
them computes with exact rationals, which is far too slow to execute in an assertion-enabled build.

## Where the proved code fails closed

The proved units decide beforehand whether an operation could overflow instead of handling the exception. They
fail closed (invalid interval, certificate refused, no bound) in these cases, none of which a real blend
approaches:

- an interval endpoint above `Dimensionless'Last / 4` in an addition, or above `2**511` in a product;
- a binomially scaled polynomial control above `2**500`;
- `Up` and `Down` saturate within `2**-50` of `Dimensionless'Last` instead of a few units in the last place;
- in the sampled certificate, a running product of the common denominator outside `1e-100 .. 1e100` at a sample
  node, or a distance warp outside `2**-24 .. 2**24`, where the rounding allowances need not hold;
- a total tangent error above a quarter of `Dimensionless'Last`.

`Geometry` is proved in the same way, and fails closed or saturates in these cases:

- the square of an interval which contains zero and has an endpoint above `2**511`, where `Interval_Multiply`
  already gives up, is invalid;
- `Add_Upper` returns `Length'Last` when either bound is at least half of it, and `Round_Curvature_Up` returns
  `Curvature'Last` above half of it;
- `Point_To_Point_Distance_Upper` returns `Length'Last` for a coordinate above `2**400` mm;
- `Bound` returns the unbounded interval for a position error bound which is not a valid error and for a frame
  speed bound which is negative, and does not raise for a requested width above half of `Length'Last`;
- a `Distance_Interval` or a `Capsule` is ordered wherever it exists, as its predicate says, so bounds computed
  separately are held in a plain record (`Unordered_Bounds`) until they are compared.

### The construction code

`Stereographic_Curves` is proved in the same way, except the subprograms listed under "What is not proved". A
proved stage has no handler: it tests the numbers it is given, or the numbers it has produced, against a limit
before the next stage computes with them, and fails closed beyond it. The limits are far above anything a corner
of a real machine produces. The proved stages are request validation and the canonical tangent jets, the frames,
the chart jets and the chart, the closure solve with its quadrature and linear algebra, the tests for a planar
chord and for an impossible no-bulge blend, the ideal candidate, the choice of the distance warp
(`Select_Distance_Warp`), `Build_Candidate`, `Prepare_Blend` and `Construct_Blend`, `Flat_Line_Blend_Miss_Ratio`,
the root iteration of the compilation (`Iterate_Conjugate_Roots`), the drivers of both tangent certificates
(`Sampled_Realtime_Tangent_Error`, `Retain_Bernstein_Tangent_Certificate`), the realtime evaluator from its cache
to `Point_At_Distance` with the bound on its rounding (`Realtime_Point_Roundoff_Bound`), the endpoint diagnostics
(`Measure_Evaluator_Endpoint_Jet_Error`), the bound queries (`Derivative_Bounds`, `Projected_Tangent_Bound`,
`Bounds_On_Parameter_Range`, `Projected_Bound_On_Parameter_Range`, `Realtime_Tangent_Range_Majorants`) down to
the two enclosures they call, the binomial tables and weights of the Bernstein products, and the tables of the
finish correction's derivatives, which are computed once at elaboration. The limits are in the private part of the
specification, or beside the subprogram in the body where only it uses them:

- **A request.** A coordinate of an endpoint is within `Coordinate_Limit`, 2⁹⁹ mm. A component of a tangent is
  within two and of a tangent derivative within 2¹⁰⁰ in millimetres (`Satisfies_Unit_Tangent_Identities` rejects
  anything else, as it rejects a NaN). The position error bound and the arc length limit are at most
  `Arc_Length_Limit`, 2¹⁰⁴ mm. A request outside these limits fails with the status which names the offending
  field.
- **Between stages.** The canonical jets, the chart jets, the chart's coefficients, the terms of the closure
  solve, its closure coefficients, its quadrature sums and its residuals are within `Tame_Limit`, 2¹⁰⁰; where a
  stage finds one beyond it the construction fails closed (the jet is invalid, the frame is unusable, or the
  candidate is numerically unsafe).
- **The closure solve.** It considers no curve longer than `Lambda_Limit`, 2⁴⁰, times its chord, whatever the
  request allows; at most 64 quadrature panels; a Newton step which takes a closure coefficient beyond the common
  limit is not accepted; the 4×4 systems are solved for entries within the common limit, and a pivot at most
  10⁻²⁰ stops the solve.
- **The evaluator.** A cache is well formed (`Rational_Antiderivative_Is_Well_Formed`, part of the evaluator
  type's predicate, so it is established once at construction and assumed by every evaluation) only if every
  pole and residue is within 2¹⁰⁰ in magnitude and every pole is clear of the unit interval: a real pole, or the
  real part of a pair, at most -2⁻¹⁰⁰ or above one, or a pair's imaginary part at least 2⁻¹⁰⁰. A positive-length
  evaluator has its length between 2⁻¹⁰⁰ and 2¹⁰⁴ mm. With that no logarithm, arctangent, product or sum of an
  evaluation can fail, and `Evaluate_Rational_Point` is proved to return the start point exactly at distance
  zero.
- **Interval quotients.** `Interval_Divide` is invalid for a numerator endpoint above 2⁵⁰⁰ or a denominator
  endpoint below 2⁻⁵⁰⁰ in magnitude, and `Lower_Power` returns zero once a partial product exceeds 2⁴⁰⁰.
- **The distance warp.** `Select_Distance_Warp` keeps the identity warp, as for ill-conditioned tangents,
  where the scalar product of the endpoint tangents is one or more in magnitude. Its quadrature cannot overflow:
  with the warp between 2⁻²⁴ and 2²⁴ and the cosine and sine of half the angle at least 2⁻²⁰, every term is below
  2¹⁷⁰. A Newton step is not taken where the residual's derivative is below 2⁻²⁰⁰ in magnitude, which ends the
  first solve and bisects in the second, and no length ratio is proposed from an integral below 2⁻²⁰⁰.
- **The root iteration.** Each division of an Aberth step of `Iterate_Conjugate_Roots` has its own test, and the
  iteration gives up, leaving the polynomial to the eigenvalue solve, where: the leading coefficient is below
  2⁻³⁰⁰ in magnitude; the starting radius is not within e^±64; an iterate has an imaginary part below 2⁻³⁰⁰; two
  iterates, or an iterate and a conjugate, are within 2⁻¹⁵⁰ of each other; the polynomial or its derivative
  exceeds 2³⁰⁰ at an iterate, or the derivative's squared modulus is below 2⁻⁴⁰⁰; a Newton correction exceeds
  2¹⁵⁰; or the Aberth denominator's squared modulus is below 2⁻³⁰⁰. The Horner sums need no test: ghost code
  bounds them by 2³³³⁺²⁴ᵏ after k steps.
- **Bounds in physical units.** `Bounds_On_Parameter_Range` divides a derivative majorant by the length once per
  order. For a length of at least a millimetre a partial result above an eighth of the largest number is
  reported as no bound, and for a shorter one a partial result above 2⁹⁰⁰. `Projected_Bound_On_Parameter_Range`
  reports no bound where a term of one of its sums exceeds 2⁴⁰⁰; where only the sum of the per-axis terms does,
  it returns the global bound.
- **A candidate.** `Build_Candidate` adds the position error of the compiled evaluator, the bound on the
  rounding of an evaluation and the bound on the finish correction. A term above 2¹⁰⁰ mm is taken as no bound
  at all, as `Length'Last` is, so the candidate fails as one over its budget does. The bound on the rounding of an
  evaluation is of a positive curve only, whose cache the type's predicate keeps well formed.

A few values are tested which the mathematics already bounds, to state the bound for the proof where rounding
makes it long to argue: a binomial coefficient and a factorial are taken no larger than their tables' limits, a
Bernstein basis value no larger than one, a norm no smaller than its largest component, a normalized quadrature
node between zero and one, a Gaussian elimination factor at most one, an exact binomial coefficient at least
one, the warp's denominator W·(1 - V) + V no smaller than 2⁻²⁵, a trim ratio which passed the 20:1 test within
64. None of these changes a result. Two tests of the same kind have a fallback instead of a bound:
`Derivative_Bounds` over a range returns the bounds of the whole curve should the two ends of the range, after
division by the length and outward rounding, not be in order, which they always are.

GNATprove assumes nothing about an object which a function with side effects has updated, not even the predicate
of its type, though it does after a procedure call. `Build_Candidate` therefore tests, after
`Build_Rational_Representation` and after `Retain_Bernstein_Tangent_Certificate`, that the curve is still a
positive one with the nonnegative certificates its type requires (`Is_Sound_Positive_Curve`), and reports a
numerically unsafe candidate otherwise. Neither test can fail. For the same reason a retained certificate is
tested to be valid before it is used, and a certified sampled bound to be a finite number which is not negative.

The unit is compiled with floating-point overflow checks. Its innermost loops (the closure and warp quadratures,
the root iteration) are proved not to overflow, and are divided into small subprograms which suppress the check.

Prunt's vector operators (`"*"`, `"-"`, `"/"` and `Dot` on `Position_Scale`) are outside SPARK, so GNATprove
would take their results on trust. The proved stages use local functions which do the same operations in the
same order with stated bounds.

What the proofs take on trust is in `Stereographic_Kernel.Elementary`: that the square root of a rounded square is
the number which was squared and that the square root is monotonic, that the logarithm of a positive number is
within 746, that the two-argument arctangent is within 3.25, and bounds on the exponential over two ranges. The
exponential is called through two functions of that unit which state those bounds, because GNATprove cannot show
that a call of the library's `Exp` does not overflow, whatever is known of its argument.
`Test_Elementary_Bounds` checks each of them. The distance warp's limits are 2⁻²⁴ and 2²⁴, the powers of two just
outside the exponentials of ±16.

### The certificates

A cache is accepted only if the sampled certificate certifies it, because that is the certificate whose bound is
proved. The Bernstein reconstruction runs when the sampled bound leaves less than a sixteenth of the position
budget, and then only tightens the budget.

`Stereographic_Curve` holds the two certificates, and `Create_Blend` is a small proved wrapper around
`Construct_Blend`. Functions with `out` parameters are marked `Side_Effects` and called in assignments.

## Reproducing

```sh
./prove.sh                # flow analysis of every unit and proof of all that is not marked Skip_Proof
PRUNT_SKIP_VALIDATION=true PRUNT_XCOV_DUMP=false PRUNT_SKIP_INTEGRATION=true ./run_tests.sh stereographic
```

`prove.sh` proves the units of `Prunt.Motion_Planner.Trajectory` and `Prunt.LAPACK` too;
[spark_trajectory.md](spark_trajectory.md) describes it and `gnatprove.sh`, through which it runs every step. A run of
the kernel which reuses nothing takes over half an hour on a 16-processor machine. Level 2 gives each prover a few
seconds of wall-clock time, and a starved prover times out on a proof it would find, so on a busy machine lower
`PROVE_PROVERS` rather than accept the timeouts. `prunt.gpr` gives `Stereographic_Curves` 20 seconds instead: the
postconditions of `Create_Blend` and `Build_Ideal_Candidate` take about ten on a busy machine.
