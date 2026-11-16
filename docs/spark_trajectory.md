# The SPARK feedrate profile generator

`Prunt.Motion_Planner.Trajectory` is Prunt's feedrate profile (trajectory) generator, written in SPARK: the 15-phase
bounded-crackle ramp, the solvers for the fastest ramp and the fastest complete profile under acceleration, jerk,
snap and crackle limits, and the evaluation of a profile at a time. The planner uses `Trajectory.Profiles` and
`Trajectory.Solvers` directly. The workspace bounds and the mixed-derivative limit solver of `Prunt.Motion_Planner`,
and the rest of `Prunt.Motion_Planner.Planner`, are not in SPARK.

`./prove.sh` proves every check of these units at level 2, with no `pragma Assume` and no justified check.

## What is proved

### The exact ramp (`Profile_Model` and its children; ghost code over exact reals)

`Profile_Model` defines a ramp the way the comment on `Feedrate_Profile_Times` describes it: fifteen stages of
constant crackle, each quantity the exact integral of the one before. For every ramp with nonnegative phase times,
every stage and every time within the stage:

1. **Limits** (`Derivatives.Lemma_Limits`). Snap never exceeds `C * T1` in magnitude, jerk never
   `C * T1 * (T1 + T2)` and acceleration never `C * T1 * (T1 + T2) * (2 * T1 + T2 + T3)`, and acceleration never
   opposes the crackle of the first stage. These are the three "maximum" formulas of the comment, proved for every
   instant rather than observed.
2. **Rest at the end** (`Derivatives.Lemma_Ends_At_Rest`). Snap, jerk and acceleration are zero at the end of a
   ramp, so ramps and coasting join without a discontinuity.
3. **End formulas** (`Motion.Lemma_End`). The velocity changes by
   `C * T1 * (T1 + T2) * (2 * T1 + T2 + T3) * (4 * T1 + 2 * T2 + T3 + T4)` and the distance is the duration times the
   mean of the starting and final velocities: what `Fast_Velocity_At_Max_Time` and `Fast_Distance_At_Max_Time`
   compute.
4. **Monotone motion** (`Motion.Lemma_Velocity_Range`, `Lemma_Distance_Range`). The velocity is between its starting
   and final values at every instant, and between two nonnegative speeds the distance is between zero and the travel:
   a ramp never overshoots and never moves backwards.
5. **The formulas of the code** (`Formulas`, `Motion.Lemma_Checkpoint_4/8/12`). Each of the 60 stage polynomials of
   `Snap_At_Time`, `Jerk_At_Time`, `Acceleration_At_Time` and `Velocity_At_Time`, written in the spec with the
   grouping and coefficients of the code, is exactly the integral of the stage, and the three closed-form states
   from which `Distance_At_Time` starts are exactly the state of the ramp there.
6. **No turning within a stage** (`Derivatives.Lemma_Stage_Signs`, `Lemma_Stage_Monotone`, `Lemma_Stage_Range`).
   Snap and jerk keep one sign through each stage, and snap, jerk and acceleration each move one way within a
   stage, so each lies between its values at the two ends of the stage.
7. **The fastest ramp of the family** (`Optimality`). With the spans `a = T1`, `b = T1 + T2`,
   `c = 2 T1 + T2 + T3`, `d = 4 T1 + 2 T2 + T3 + T4`, a ramp has the peaks `C a`, `C a b`, `C a b c`, the change
   of speed `C a b c d` and the duration `a + b + c + d`, and its phase times are nonnegative exactly when
   `b >= a`, `c >= a + b` and `d >= a + b + c`. A ramp is *saturating* when each span is as long as its limit or
   the ordering allows, which is how the solvers build one. No ramp of the family within the same limits reaches
   the same change of speed in less time (`Lemma_Saturating_Is_Fastest`), or covers the same distance from the
   same starting speed in less time (`Lemma_Saturating_Is_Fastest_Over_Distance`); two saturating ramps for the
   same change of speed have the same phase times; and the thirteen cases of the speed-change solver and the
   candidates of the distance solver are saturating (`Lemma_Delta_V_Case_Is_Fastest`,
   `Lemma_Distance_Case_Is_Fastest`, `Lemma_Region_1_Is_Saturating` to `Lemma_Region_5_Is_Saturating`). This is
   about the fifteen-stage family and exact phase times: it does not say that no other motion with bounded
   crackle is faster. A ramp of four accelerating arcs, which the family does not contain, can be faster.
   [The optimality proof](spark_trajectory_optimality.md) gives the argument on paper.

These are theorems of exact arithmetic. They say what the phase times mean, not how the floating-point evaluation
rounds.

### The executable code

Each subprogram has a precondition describing a *proved domain* of magnitudes, far beyond any that a machine can
have: phase and coast times up to 2**165 s, crackle up to 2**100 mm/s^5 and a starting speed up to 2**100 mm/s for
the evaluation functions, and derivative limits, speeds and distances which are zero or within 2**-80 to 2**80 of
their units for the solvers (the constants are in the specs). For arguments within it GNATprove proves:

- **No run-time errors.** No overflow, division by zero or range violation in the evaluation functions, the ramp end
  formulas, the solvers and the search; every `raise` in a function is unreachable; every loop and recursion
  terminates. `Try_Optimal_Full_Profile` raises nothing but `Constraint_Error`.
- **Bounds of the whole-profile evaluators** (postconditions of the `Feedrate_Profile` overloads of the
  `*_At_Time` functions). As for a single ramp: the crackle is within `abs Max_Crackle`; snap, jerk and
  acceleration are within `Evaluated_Snap_Max`, `Evaluated_Jerk_Max` and `Evaluated_Acceleration_Max`; the
  velocity is within `Evaluated_Profile_Gain_Max` of the starting speed; and the distance is within
  `Evaluated_Profile_Travel_Max` of the travel at the starting speed for `Profile_Time_Max`. These bound
  magnitudes over the proved domain, not errors.
- **The search certificate** (`Searches.Largest_Nonpositive`). The result is the lower end of the bracket or a point
  at which the residual was found nonpositive, and with a zero tolerance the next machine number above it is the
  upper end or a point at which the residual was found positive. For any tolerance there is a machine number above
  the result which is the upper end or a point at which the residual was found positive, and which is the next
  machine number or no further from the result than the tolerance, as a fraction of it.
- **The two bisections of a phase time** (`Distance_Is_Bisected`, `Gain_Is_Bisected`). The distance solver and the
  snap-limited case of the speed-change solver return a phase time for which the ramp, as the end formula computes
  it, does not exceed what was asked, while with the next machine number in its place it does, unless that number
  is the cap of 24 hours. The end formulas are functions of the four phase times for this
  (`Profiles.Distance_Formula`, `Velocity_Formula`).
- **Solver results.** Every phase time returned by a ramp solver is nonnegative and bounded, a prepared solver holds
  no recorded failure, and a complete profile which `Try_Optimal_Full_Profile` returns as reachable has passed
  `Profile_Is_Valid`. When it reports that the end speed cannot be reached, the limits are positive and the
  distance is less than the direct ramp from the start speed to the end speed covers (`Direct_Ramp_Distance`).
- **The speed-change solver respects the limits and gains the speed asked for** (postconditions of
  `Prepare_Delta_V` and of every `Optimal_Profile_For_Delta_V`, with `Solvers.Accuracy` and `Rounding.Relative`).
  For positive limits anywhere in the proved domain and a change of speed up to 2**80: at every instant of the
  exact ramp of the phase times returned, crackle is within its limit and snap, jerk and acceleration are within
  theirs to one part in 2**29 (`Ramp_Is_Within_Limits`); and the exact ramp changes speed by what was asked, to
  one part in 2**32 and 2**-700 mm/s (`Gain_Is_At_Most`, `Gain_Is_Reached`). It never gains more. It gains less
  only where its second phase time is the machine number below 24 hours, which is where the bisection of the
  snap-limited case stops when 24 hours are not enough. The proof is a rounding analysis of each of the five
  regions and thirteen cases: the saturated ramp which `Prepare_Delta_V` forms has its peaks within the limits
  (`Solver_Is_Consistent`, `Snap_Is_Within`), and a solve returns a ramp whose spans are within those of the
  saturated ramp (`Solve_Is_Sound`).
- **The distance solver respects the limits** (postcondition of `Optimal_Profile_For_Distance`). The same
  statement about the limits, for *moderate* limits: within the proved domain and with each quotient of two
  successive limits, as computed, at most 2**99 s (`Limits_Are_Moderate`). The saturated ramp then has no phase
  longer than 2**100 s, which the bound of the rounding of a ramp's travel needs. The solver bisects one phase
  time of the longest of its candidates which is too short; the candidate after it was not too short, so the
  bisected ramp travels no further, and its spans are within the saturated ones.
- **A complete profile respects the limits and the highest speed** (postcondition of
  `Try_Optimal_Full_Profile`, with `Certificates.Validity.Lemma_Limits_At_Every_Instant_Of_Profile` and
  `Lemma_Speed_At_Every_Instant`). For a reachable result with positive limits in the proved domain, both ramps of
  the exact profile are within the limits at every instant, to one part in 2**29. For any reachable result the
  exact profile never moves faster than it coasts, and it coasts at no more than `Max_Vel`, to one part in 2**31
  and 2**-699 mm/s: the acceleration ramp is solved for a speed change which is rounded once, and gains that to
  one part in 2**32.
- **The end formulas against the exact ramp** (postconditions of `Fast_Velocity_At_Max_Time` and
  `Fast_Distance_At_Max_Time`, with `Rounding`). For phase times up to 2**100 s, crackle up to 2**80 and a starting
  speed up to 2**500, the floating-point end speed and end distance of a ramp are within 2**-44 of the size of
  their terms from those of the exact ramp, plus 2**-700 mm/s and 2**-600 mm where a product underflows.
- **The prepared distance is the distance** (`Lemma_Prepared_Distance`). `Distance_At_Time (Prepare (Profile,
  Max_Crackle, Start_Vel), T)` equals `Distance_At_Time (Profile, T, Max_Crackle, Start_Vel)` for every ramp of
  the proved domain and every time within it. Inside the package this is the refined postcondition of the
  unprepared function. `Prepare` returns a table for which the prepared function is free of run-time errors
  (`Is_Prepared`).
- **The table of a prepared ramp is in order** (`Lemma_Prepared_Is_Ordered`, sections 3.1 to 3.4 of
  [the monotonicity proof](spark_trajectory_monotonicity.md)). As rounded: no stage ends before an earlier one does,
  the stage which `Stage_At` selects starts at or before the time and is not an earlier stage for a later time, the
  closed form at the start of stage 4 is not negative and that at the start of stage 8 is not below it, the two
  limits which the gain of a stage is kept between are in order, and the upper limit of a stage is at most the
  lower limit of the next.
- **The distance does not decrease if the stage polynomial does not** (`Lemma_Distance_Is_Monotone`, sections
  3.5 and 3.6). For a prepared ramp which speeds up from a speed which is not negative or slows down to one, whose
  table is in order, and two times of it in order: if both are evaluated in the same stage and the compensated
  polynomial of that stage is no smaller for the later of the two times into the stage, the distance at the
  later time is no smaller. That condition (`Stages_Are_Monotone`) is a hypothesis which nothing proves or
  assumes. It is stated for the two times, not for the whole stage, because a quantified expression cannot range
  over floating-point numbers.
- **What a valid profile is** (`Certificates.Validity.Lemma_Valid_Profile_Is_Exact`). For a profile which
  `Profile_Is_Valid` accepts, with times up to 2**100 s and crackle, speeds and distance up to 2**80: the exact
  profile which its phase times denote (the acceleration ramp, a coast at the speed that ramp ends at, and the
  deceleration ramp from there) covers the distance asked for to within 1.01 parts in 10**9, and ends at the end
  speed asked for to within 1.01 parts in 10**9 of its highest speed, each plus 2**-500 where a product
  underflows; and it speeds up to its coasting speed and slows down from it. Since every reachable profile which
  `Try_Optimal_Full_Profile` returns has passed `Profile_Is_Valid`, this holds for all of them in that domain.
- **Limits of any ramp** (`Certificates.Ramp_Respects_Limits`, `Lemma_Limits_At_Every_Instant`). A proved
  floating-point check which, when it accepts phase times, guarantees that the exact ramp they denote stays within
  the given acceleration, jerk, snap and crackle limits at every instant, to one part in 2**29. Nothing in Prunt
  calls it. With the solvers proved to respect the limits it is a second, independent statement of the same
  property, and the only one for the distance solver outside moderate limits.

The preconditions are not checked when the program runs. Outside them nothing is proved, and an operation which
overflows raises `Constraint_Error`.

## What is trusted

The proofs rest on SPARKlib's rounding lemmas and on these statements, which GNATprove cannot check and the unit
tests of `Prunt.Motion_Planner.Trajectory.Test` check by running:

- **Bit patterns** (`Machine_Numbers`). Five facts about how IEEE 754 binary64 lays out nonnegative finite
  numbers: their patterns are ordered as they are, and converting to a pattern and back is the identity. GNATprove
  has no model of the bit pattern of a floating-point value.
- **Square root** (`Elementary.Lemma_Square_Root_Is_Faithful`). The library's square root is one of the two machine
  numbers around the exact root. IEEE 754 requires the nearer of the two.
- **Cube root** (`Elementary.Lemma_Cube_Root_Is_Close`). `X**(1/3)` is within one part in 2**40 of the exact cube
  root.
- **Logarithm, exponential, fifth power and fifth root** (`Elementary.Lemma_Log_Is_Bounded`, `Bounded_Exp`,
  `Fifth_Power`, `Fifth_Root`). The logarithm of a positive machine number is at most 746 in magnitude, the
  exponential of a number up to 80 is at most 2**116, the fifth power of a number up to 2**100 is at most 2**501,
  and a fifth root, like the cube root, is a machine number which is not negative. Only the estimates which steer
  the searches (`Searches.Model`, `Solvers.Estimates`) use these, to be proved free of run-time errors. Nothing
  that is proved depends on the value of an estimate.
- **Operations meant to overflow** (`Checked`). Where raising `Constraint_Error` on overflow is the intended
  outcome, the operation is a one-line procedure specified to raise, whose body GNATprove does not see: the two
  coast-time divisions of `Try_Optimal_Full_Profile`, and the total time and the travel in `Profile_Is_Valid`,
  which a coast time of any size can overflow. Two of them state that they do not raise for arguments within the
  proved domain, where the same expression is proved not to overflow. An overflow in one of them is reported at a
  line of `prunt-motion_planner-trajectory-checked.adb`.

## Not proved

- **Completeness of `Try_Optimal_Full_Profile`**: that its last `raise`, for a generated profile which
  `Profile_Is_Valid` rejects, is never reached. The speed-change solver is proved to gain the speed asked for to
  one part in 2**32, which is the larger half of it, but three things are missing. The 24-hour cap has to be
  excluded by a condition on the request: it is excluded so far by a condition on the result (`Is_Capped`). The
  arithmetic of `Profile_Is_Valid` has to be bounded the other way round: the validity theorem shows that an
  accepted profile is accurate, not that an accurate profile is accepted. And where the peak is searched for, the
  coast which makes up the distance divides by the peak found, which nothing bounds from below: that needs a
  bound of how the ramp distances grow with the peak.
- **An end-to-end theorem on the request alone.** For a reachable result the limits and the highest speed are
  proved from the request alone, as above. Distance and end speed are proved through `Profile_Is_Valid` by the
  validity theorem, which asks that the times of the profile returned be at most 2**100 s
  (`Certificates.Validity.Is_Certifiable`). That is a condition on the result, which a caller can test. Deriving
  it from the request needs bounds of the phase times in terms of the limits, which the solvers state only up to
  2**165 s, and the same lower bound of the peak as above for the coast.
- **The distance solver outside moderate limits**, and that the ramp which it returns covers the distance in
  exact arithmetic: its certificate is about the end formula as computed.
- **Global optimality.** The fastest ramp of the family is proved for exact phase times; that the floating-point
  phase times are near the saturating ones is proved as bounds of spans and gain, not as a bound of the duration.
- **That the compensated polynomial of a stage does not decrease** (section 4 of
  [the monotonicity proof](spark_trajectory_monotonicity.md)), which is the hypothesis of
  `Lemma_Distance_Is_Monotone`: it rests on the exactness of the error terms of the compensated Horner scheme, which
  is a statement about the bits of a product, and on analysis over the reals. Nothing is proved either about the
  composition of two ramps and a coast into a complete profile, which the document argues in 3.6.
- **Error bounds between the `*_At_Time` evaluation functions and the exact ramp.** Only the two end formulas
  are bounded, as postconditions. Near a stage boundary such a bound for `Distance_At_Time` would be against the
  polynomial of the stage selected, which can be the stage before or after that of the exact time.

## Known limits

- `Largest_Nonpositive` guards two sums with `X <= Dimensionless'Last - Target`. The guard does not prevent the
  overflow it is there for: with `Target` = 4.4e306 and `X` = `Last - Target`, `X + Target` rounds to infinity.
  Residuals of that size are not physical, and the proved domain is limited to residuals below 2**1000.
- Both bisections of a phase time search from zero to 24 hours. Within the proved domain the phase time wanted
  can be longer: with a snap limit of 1 mm/s^4, jerk and crackle limits of 1e6 and an acceleration limit of 2e12,
  a change of speed of 1e18 mm/s needs a second phase of about 7.9e5 s. The solve then returns the ramp of 24
  hours, which gains less than was asked, and says nothing. A complete profile built from it is rejected by
  `Profile_Is_Valid`, so `Try_Optimal_Full_Profile` raises. No machine has such limits.
- A speed change below 2**-870 mm/s makes the quotient under the fourth root of the crackle-limited case a
  subnormal number, and the phase time is then not relatively accurate. The bound of the gain carries an absolute
  term of 2**-700 mm/s for this.

## Notes on the code

The proofs need some of the code in a particular form:

- `Prepare` is an expression function in which the loop over the stages is written out: one constant for the
  state at the start of each stage and one for each stage. The definition of an expression function is known to
  the proofs about what it returns, which the result of a function with a body is not.
- `Rising_Stage` keeps the closed-form distance of stage 12 in `Lo` and passes that to `Checkpoint_12` and to the
  maximum, and `Distance_At_Time` of a ramp names the stage which `Rising_Stage` returns in a constant before it
  evaluates it.
- `Largest_Nonpositive` has a second generic formal, `Evaluable`, with a default, used only in its precondition.
  It must be instantiated where every assertion policy is `Ignore`, since its postcondition and loop invariants
  speak of ghost functions which are never compiled, and its precondition bounds residuals more tightly than the
  planner's are. `Prunt.Motion_Planner.Largest_Nonpositive` is a generic without a contract whose body
  instantiates it so; the planner instantiates that one. A generic instance takes its check suppression from the
  unit which instantiates it, so the model which steers the search does not depend on a run-time check.
- The units are children of `Prunt.Motion_Planner`, so its declarations are directly visible in them and hide any
  of the same name which a use clause would make visible. `Prunt.Motion_Planner` therefore declares no elementary
  functions in its specification: the instance it uses is in its body, and every unit here calls the one of
  `Elementary`, about which the trusted lemmas speak.
- The model's estimate in `Largest_Nonpositive`, the Newton estimate in the snap-limited solve and
  `Delta_V_Over_Distance` are in their own units, `Searches.Model` and `Solvers.Estimates`. Each first decides
  that its arguments are within magnitudes for which nothing overflows, and offers no estimate otherwise, so that
  it is proved to raise nothing and has no handler. The magnitudes are far beyond any machine: residuals, offsets
  and targets of the model within 2**-500 .. 2**500; for the snap-limited estimate a speed up to 2**100 mm/s, a
  crackle limit within 2**-300 .. 2**300 mm/s**5 and a phase time within 2**-150 .. 2**150 s; for
  `Delta_V_Over_Distance` a speed, a distance and a crackle limit up to 2**200 in mm and s, phase times up to
  2**100 s, and divisors of at least 2**-300. `X**5`, `X ** 0.2`, the cube root and `Exp` are calls of
  `Elementary.Fifth_Power`, `Fifth_Root`, `Cube_Root` and `Bounded_Exp`, whose bodies are those expressions.
- `Try_Optimal_Full_Profile` and `Optimal_Full_Profile` are functions with `Side_Effects`, since they raise
  `Constraint_Error` by design: SPARK code calls them in an assignment statement.
- The cube root `X ** (1.0 / 3.0)` is a call of `Elementary.Cube_Root`, whose body is that expression, on the
  quotient divided by `s**3`, multiplied by `s` again: both exact. GNATprove knows nothing of the accuracy of
  `"**"`, so the root is taken on trust through that function (see "What is trusted").
- Two fourth powers, `Sm**4` and `DT**4`, are written `(Sm**2)**2` and `(DT**2)**2`, which GNATprove models as two
  multiplications. GNAT computes both forms with the same two multiplications.
- The conversions between numbers and bit patterns come from `Machine_Numbers`, since GNATprove rejects an unchecked
  conversion to a floating-point type.
- The solvers contain ghost code: calls of lemmas after the statements whose values they describe, ghost
  variables and constants, and `Assert_And_Cut` pragmas. None of it is compiled. The distance solver keeps, in a
  ghost variable, the saturated ramp as `Prepare_Delta_V` would return it for the same limits, and its loop over
  the candidates has a loop invariant.

### The monotone `Distance_At_Time`

The distance of a ramp whose speeds are not negative never decreases as the time increases, to the last bit. The
comment headed "Distance along a ramp" in `src/prunt-motion_planner-trajectory-profiles.adb` describes the evaluator:

- A ramp which slows down is evaluated backwards, as the distance of the whole ramp less the distance of the ramp
  which speeds up from its final speed, so nothing large is subtracted from `Start_Vel * T`.
- The gain over travel at the starting speed is kept between the gains at the two ends of its stage, and each
  group of stages between the closed forms from which stages 4, 8 and 12 start is kept between its own closed form
  and the next. A stage far shorter than the spacing of the times around it could otherwise put a closed form
  below the end of the stage before it.
- The polynomial of a stage is evaluated by a compensated Horner scheme: the result of twice the working precision,
  rounded once. The error of each product is taken from the halves of its factors, so no target needs a fused
  multiply-add. Every target computes the same value only if the compiler fuses nothing itself, so Prunt is
  compiled with `-ffp-contract=off`: with fused operations the error terms are wrong and the distance
  decreases between some adjacent times ([the monotonicity proof](spark_trajectory_monotonicity.md), section 0).
- `Prepare` computes all fifteen stages of a ramp once, and `Distance_At_Time` on the `Prepared_Ramp` gives the
  same value bit for bit. The planner prepares the two ramps of a run's central profile for its bisections, and
  execution uses the unprepared form.

[The monotonicity proof](spark_trajectory_monotonicity.md) proves on paper that the distance does not decrease,
from two published theorems on floating-point arithmetic and one hypothesis on the rounded state of a ramp which it
states but does not prove.

## Layout

| Unit | Status | Contents |
|---|---|---|
| `Prunt.Motion_Planner.Trajectory` | proved | The one numeric subtype the units add to Prunt's, `Time_Squared` |
| `.Profiles` | proved | Phase times, ramp end formulas, evaluation at a time |
| `.Searches`, `.Searches.Witness` | proved | `Largest_Nonpositive`, and an instance of it through which GNATprove analyses the generic |
| `.Searches.Model`, `.Solvers.Estimates` | proved | Estimates which steer the searches |
| `.Solvers` | proved | Ramp solvers, `Profile_Is_Valid`, `Try_Optimal_Full_Profile` |
| `.Solvers.Accuracy` | proved (ghost) | What each value which the solvers compute means in exact arithmetic: the rounding analysis of the regions and cases |
| `.Certificates` | proved | The limit check and its theorem |
| `.Certificates.Validity` | proved (ghost) | What `Profile_Is_Valid` establishes in exact arithmetic |
| `.Profile_Model`, `.Derivatives`, `.Motion`, `.Formulas`, `.Optimality` | proved (ghost) | The exact ramp and its theorems |
| `Prunt.Motion_Planner.Reals` | proved (ghost) | Real-number view of the floats and rounding lemmas restated from SPARKlib, shared with `Stereographic_Kernel` |
| `.Rounding` | proved (ghost) | Error tracking through sums and products |
| `.Rounding.Relative` | proved (ghost) | Relative error bounds through products, quotients, sums and roots, without absolute terms; density of machine numbers |
| `.Machine_Numbers`, `.Elementary`, `.Checked` | trusted | Bit patterns, roots, logarithm and exponential, operations meant to overflow |

Every unit of `Prunt.Motion_Planner.Trajectory` except `Checked` and `Elementary` starts with `pragma Assertion_Policy
(Ignore)`: its contracts are proved, and the ghost code among them computes with exact rationals, which is far too
slow to run. The bodies with lemmas declared only by their bodies turn off the style check for separate declarations
(`pragma Style_Checks ("-s")`), and those whose lemmas take parameters only for their contracts turn off the warning
about unreferenced formals (`pragma Warnings ("F")`).

## Reproducing

```sh
./prove.sh                # flow analysis and proof of the SPARK units; add -f for a run which reuses nothing
PRUNT_SKIP_VALIDATION=true PRUNT_XCOV_DUMP=false PRUNT_SKIP_INTEGRATION=true ./run_tests.sh trajectory
```

`prove.sh` proves the blender's and the eigenvalue solver's units too (see [spark_blender.md](spark_blender.md) and
[spark_lapack.md](spark_lapack.md)). It runs `alr build` first, which configures Prunt without its test mocks, and
then every step through `./gnatprove.sh`, which takes the arguments of GNATprove except `-P` and `-j`.
`gnatprove -jN` runs up to N gnatwhy3 processes (each reads an entity's Why3 input, splits and transforms its
goals) and N provers, and the two limits want different values: provers must leave headroom for their wall-clock
timeouts, gnatwhy3 processes cost memory (more than a gigabyte each for the largest units). The script starts the
prover server itself and sets them separately (`PROVE_LANES` gnatwhy3 processes, `PROVE_PROVERS` provers, both
three quarters of the processors by default). Use it for single units too:

```sh
./gnatprove.sh --output=oneline prunt-motion_planner-trajectory-solvers.adb
```

GNATprove learns the layout of types by compiling with `-gnatR`. `gnatprove.sh` sets `GNSA_ROOT` so that it does
this with Prunt's own compiler rather than the one it comes with: gnatprove 16.2.1001 comes with the compiler of
gnat_native 16.2.1008, whose `-gnatR` fails an assertion on two ghost types of
`Prunt.Motion_Planner.Stereographic_Kernel`, and 16.2.1009 fixed that.

GNATprove has to be given the files of the units to analyse: most of Prunt is not in SPARK. `gnatprove.sh` passes
`--no-subprojects`, which keeps it out of the dependencies, some of which it cannot analyse (gnat2why crashes on the
XML units of `vss_extra`). That holds only without `-u`: with `-u`, GNATprove's generation of Global contracts
compiles every unit of the dependencies too. The GNATprove of Prunt's Alire index (`gnatprove` 16.2.1001) is
needed: its front end is that of Prunt's compiler, and AdaCore's 16.1 rejects Prunt's sources.

A run which reuses nothing takes up to about an hour for these units on a 16-processor machine. Level 2 gives each
prover a few seconds of wall-clock time, and a starved prover times out on a proof it would find, so on a busy machine
lower `PROVE_PROVERS` rather than accept the timeouts.

Without `-f` a run is incremental, and usually much faster: unchanged units are skipped, and in a changed unit
every goal an earlier run settled is taken from its session in `obj/development/gnatprove`, also after an edit
which only moves code to other lines. Only new or changed goals go to the provers. A goal which failed is retried
only with more provers or a longer timeout than before (`--level`, `--timeout`). After a spurious timeout on a
loaded machine, retry just that check with a longer timeout, as in `--timeout=30 --limit-line=FILE:LINE`, and
without `-f`: `-f` starts the entity from an empty session, which then holds only the proofs of that line, and the
next run proves the rest of the entity again.

Proofs are shared between checkouts (worktrees) in `~/.cache/prunt/gnatprove` (`PROVE_CACHE` overrides the
directory, or shares nothing when set to "off"), so a new worktree does not prove again what another has proved. It
holds GNATprove's file cache (`--memcached-server=file:DIR`), which keeps the output of each gnatwhy3 run and each
prover call under a hash of its exact input, and the latest session of each entity (subprogram or package). An entity
whose Why3 input is unchanged is taken from the cache without running gnatwhy3. An edit which moves code to other
lines changes the input of every entity after it, and then gnatwhy3 takes the proved goals from the shared session,
which `gnatprove.sh` gives to every entity that has none in the checkout. Outputs which leave a check unproved are not
kept, so a failure is retried as above. `gnatprove.sh` describes the details; `-f` neither uses nor fills either
store.

The unit tests of `Prunt.Motion_Planner.Trajectory.Test` check the trusted statements by running them: the bit-pattern
facts on random and extreme patterns, the bounds of the logarithm, the exponential, the fifth power and the fifth
root, and the square and cube root statements in exact rational arithmetic. The tests of `Prunt.Motion_Planner`
exercise the solvers and evaluators.
