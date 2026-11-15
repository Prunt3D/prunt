# Monotonicity of `Distance_At_Time`

This is a proof on paper, not in SPARK, that the floating-point distance of a ramp never decreases as the time
increases, bit for bit. It covers `Distance_At_Time` for a ramp, the prepared form, and the composition into a
complete profile. Sections 2 and 3 use nothing but the monotonicity of rounding and are written to become SPARK
lemmas. Section 4 rests on two published theorems about floating-point arithmetic and on one hypothesis about the
rounded state of a ramp, which section 5 states and does not prove.

The code is the block headed "Distance along a ramp" in `src/prunt-motion_planner-trajectory-profiles.adb`, part of
`Prunt.Motion_Planner.Trajectory.Profiles` (see [spark_trajectory.md](spark_trajectory.md)).

## 0. What is proved

$\mathbb{F}$ is the set of finite binary64 numbers, $\mathrm{RN}$ rounds a real to the nearest of them (ties to
even), and $u = 2^{-53}$. Every operation of the code returns $\mathrm{RN}$ of its exact result. No operation
overflows, which `Prunt.Motion_Planner.Trajectory` proves separately for its proved domain (section 6).

That every operation is rounded on its own is a condition on the compiler. GCC by default contracts a product
and a sum into one fused multiply-add wherever the target has that instruction, across statements too, and the
error terms of section 4.1 are then wrong. Prunt is therefore compiled with `-ffp-contract=off`.
A copy of the stage polynomial built with Prunt's switches and `-mfma`, without that switch, contained 49 fused
operations, gave a value other than the exact polynomial rounded in a quarter of 729,513 evaluations (by up to
three units in the last place), and decreased in 913 of 14.3 million adjacent pairs. With the switch it gave the
values of a build without the instruction, bit for bit.

* **Theorem 1 (ramps).** Take phase times $T_1..T_4 \ge 0$ and a ramp which speeds up from a speed which is not
  negative, or which slows down to one. Assume that every stage of the ramp is *regular* (hypothesis S, section
  4.4). Then for all times $t \le t'$ in $\mathbb{F} \cap [0, \text{Total}]$ at which no product in the stage
  polynomial underflows (hypothesis N, section 4.1) the results satisfy
  $\text{Distance\_At\_Time}(t) \le \text{Distance\_At\_Time}(t')$.
* **Theorem 2 (prepared ramps).** `Distance_At_Time (Prepare (P, C, V), T)` and `Distance_At_Time (P, T, C, V)`
  are the same number. No hypothesis is needed: both call the same subprograms with the same arguments (3.7).
* **Corollary 3 (profiles).** Under the hypotheses of Theorem 1 for both ramps, the distance of a complete
  profile from a starting speed which is not negative does not decrease either, across the acceleration ramp, the
  coast and the deceleration ramp (3.6).

Not covered: ramps which turn back (their distance is not monotone in exact arithmetic either), the joins between
the profiles of a run in the planner (5.4), and times so small that the stage polynomial underflows (5.2).
Hypothesis S holds in exact arithmetic and for stages 1 to 4 and 8 in floating point; for the other stages it is
argued, not proved (5.1).

## 1. The evaluator

For a ramp which speeds up from rest with crackle $C_m \ge 0$ the code computes a **gain** $G(t)$: how much
further the ramp has travelled than at its starting speed. Everything else is built on it.

* **Bounds.** $B_1 .. B_{14}$ are the times at which the first fourteen stages end, as the sums which the other
  evaluators write (`Ramp_Bounds`). The stage of $t$ is $k(t) = 1 + \#\{j : t \ge B_j\}$ (`Stage_At`), and stage
  $k$ starts at $\text{Start}_k = B_{k-1}$ (zero for $k = 1$) and nominally lasts $\text{Len}_k$.
* **States.** $S_k = (\text{Sn}, \text{Jr}, \text{Acc}, \text{Vel}, \text{Dist})_k$ is the state at the start
  of stage $k$. $S_1$ is zero. $S_4$, $S_8$ and $S_{12}$ are closed forms (`Checkpoint_4/8/12`), whose distances
  are $D_4$, $D_8$ and $D_{12}$. Every other state is `Advance` of the one before, and the distance of `Advance`
  is $E_k = \text{Dist}_k + \max(0, \text{Local}_k)$ rounded (`Stage_End_Distance`), where $\text{Local}_k$ is the
  plain Horner evaluation of the stage polynomial at $\text{Len}_k$.
* **Groups.** Stages 1 to 3, 4 to 7, 8 to 11 and 12 to 15 are the four groups. Group $g$ has the range
  $[A_g, N_g]$ with $A_1 = 0$, $N_1 = A_2 = D_4$, $N_2 = A_3 = D_8$, $N_3 = A_4 = \max(D_{12}, D_8)$ and
  $N_4$ the largest number.
* **Stages.** `Make_Stage` gives stage $k$ of group $g$ the coefficients
  $c_1 = \text{Vel}$, $c_2 = \text{Acc}/2$, $c_3 = \mathrm{RN}(\text{Jr}/6)$, $c_4 = \mathrm{RN}(\text{Sn}/24)$,
  $c_5 = \mathrm{RN}(\pm C_m/120)$ and the limits
  $\text{Lo}_k = \min(\max(\text{Dist}_k, A_g), N_g)$ and $\text{Hi}_k = \min(\max(E_k, A_g), N_g)$.
* **Gain.** With $x = \min(\mathrm{RN}(t - \text{Start}_k), \text{Len}_k)$ and $\hat{C}_k(x)$ the result of
  `Compensated_Local`,
  $G(t) = \min(\max(\mathrm{RN}(\text{Dist}_k + \hat{C}_k(x)), \text{Lo}_k), \text{Hi}_k)$ for $k = k(t)$
  (`Stage_Gain`).
* **Distance.** A ramp which speeds up from $V$ has $D(t) = \mathrm{RN}(\mathrm{RN}(V t) + G(t))$. A ramp which
  slows down from $V$ to a final speed $F \ge 0$ (a final speed which is negative by less than $2^{-40} V$ is
  taken to be zero) has $D(0) = 0$ and, for $t > 0$,
  $D(t) = \max(0, \mathrm{RN}(W - R(\ell)))$ with $\ell = \max(0, \mathrm{RN}(\text{Total} - t))$,
  $R(\ell) = \mathrm{RN}(\mathrm{RN}(F \ell) + G(\ell))$ and $W$ the closed-form distance of the whole ramp
  (`Shape_Of`, `Ramp_Distance`). A ramp which turns back has $D(t) = \mathrm{RN}(\mathrm{RN}(V t) - G(t))$ and is
  outside this proof.

## 2. Monotone operations

$\mathrm{RN}$ is nondecreasing and leaves the members of $\mathbb{F}$ unchanged. Hence, for $a \le a'$ and $b$ in
$\mathbb{F}$:

* **(M1)** $\mathrm{RN}(a + b) \le \mathrm{RN}(a' + b)$, $\mathrm{RN}(a - b) \le \mathrm{RN}(a' - b)$ and
  $\mathrm{RN}(b - a) \ge \mathrm{RN}(b - a')$. If $b \ge 0$ then $\mathrm{RN}(a b) \le \mathrm{RN}(a' b)$, and
  for a constant $c > 0$, $\mathrm{RN}(a / c) \le \mathrm{RN}(a' / c)$. Minimum and maximum are exact and
  nondecreasing in each argument.
* **(M2)** If $b \ge 0$ then $\mathrm{RN}(a + b) \ge a$.
* **(M3)** If $a, b \ge 0$ then $\mathrm{RN}(a b) \ge 0$ and $\mathrm{RN}(a + b) \ge 0$.

## 3. From the stages to the ramp

**3.1 The bounds do not decrease.** Each $B_{j+1}$ differs from $B_j$ by a larger multiple of one phase time in
the same position of the same sum, or by one more term at the end: $T_1 \to T_1 + T_2 \to 2T_1 + T_2 \to
2T_1 + T_2 + T_3 \to 3T_1 + T_2 + T_3 \to 3T_1 + 2T_2 + T_3 \to \dots$. With phase times which are not negative,
(M1) gives $B_j \le B_{j+1}$ in each case. So the count of bounds which $t$ has reached is the index of the last
one, the bisection in `Stage_At` finds it, $k(t)$ is the first stage which $t$ is before the end of (the stage
which the other evaluators select with their chain of comparisons), $k$ is nondecreasing in $t$, and
$t \ge \text{Start}_{k(t)}$.

**3.2 A stage is an interval.** If $A_g \le N_g$ then $\text{Lo}_k \le \text{Hi}_k$: by (M2),
$E_k \ge \text{Dist}_k$, and $\min(\max(\cdot, A_g), N_g)$ is nondecreasing.

**3.3 The groups are ordered.** $0 \le D_4 \le D_8 \le \max(D_{12}, D_8)$, so $A_g \le N_g$ and
$N_g = A_{g+1}$ for every group.

*Proof.* $D_4 \ge 0$ by (M3). For $D_4 \le D_8$ write $Q = \mathrm{RN}(\mathrm{RN}(C_m T_1)\,\mathrm{RN}(T_1 +
T_2)) \ge 0$, which both closed forms start with. Then $D_4$ is $Q$ times $2T_1 + T_2$ times
$P_4 = 3T_1^2 + 3T_1T_2 + T_2^2$ over 24, and $D_8$ is $Q$ times $2T_1 + T_2 + T_3$ times
$P_8 = 27T_1^2 + 27T_1T_2 + 14T_1T_3 + 7T_2^2 + 7T_2T_3 + 2T_3^2$ over 12, each evaluated from the left. The
second factor of $D_8$ is that of $D_4$ plus $T_3 \ge 0$ (M2). In $P_8$, dropping the three terms with $T_3$
does not increase the rounded sum (M2 and M1), and what remains exceeds $P_4$ term for term: $27 T_1^2 \ge 3
T_1^2$, $(27T_1)T_2 \ge (3T_1)T_2$ and $7T_2^2 \ge T_2^2$ as rounded (M1). So the rounded product before the
division is at least as large for $D_8$ (M1), and dividing by 12 gives at least as much as dividing by 24 (M1).
$\square$

$D_{12} \ge D_8$ is false in floating point: the two closed forms are different expressions, and when stages 8 to
11 are shorter than the spacing of the times around them they differ by rounding alone, in either direction. That
is what the maximum in $A_4$ is for.

**3.4 The stages join.** $\text{Hi}_k \le \text{Lo}_{k+1}$ for $k = 1..14$.

*Proof.* Inside a group, $\text{Dist}_{k+1}$ is the distance of `Advance`, which is the value $E_k$ of the same
function on the same arguments, and the range is the same, so $\text{Hi}_k = \text{Lo}_{k+1}$. Across groups
($k = 3, 7, 11$), $\text{Hi}_k \le N_g = A_{g+1} \le \text{Lo}_{k+1}$, the last step because
$N_{g+1} \ge A_{g+1}$ (3.3). $\square$

**3.5 The gain.** *Let $\mathbb{T} \subseteq \mathbb{F} \cap [0, \infty)$ contain zero. If every
$\hat{C}_k$ is nondecreasing on the times into stage $k$ which the members of $\mathbb{T}$ give, then $G$ is
nondecreasing on $\mathbb{T}$, $G \ge 0$ and $G(0) = 0$.*

*Proof.* Let $t \le t'$, $k = k(t) \le k' = k(t')$ (3.1). If $k = k'$: $x$ is nondecreasing in $t$ by (M1) and
lies in $[0, \text{Len}_k]$ because $t \ge \text{Start}_k$; then $\hat{C}_k(x)$, the sum with
$\text{Dist}_k$ (M1) and the two limits are nondecreasing. If $k < k'$:
$G(t) \le \text{Hi}_k \le \text{Lo}_{k+1} \le \text{Hi}_{k+1} \le \dots \le \text{Lo}_{k'} \le G(t')$ by 3.2 and
3.4. $G \ge \text{Lo}_k \ge A_g \ge 0$. At $t = 0$ every stage before $k(0)$ has length zero, so its state is
that of a ramp which has not started: the distances of `Advance` and of the closed forms are zero, $x = 0$,
$\hat{C}_k(0) = 0$, and $A_g = 0$. $\square$

**3.6 The distance.** Assume 3.5's hypothesis, with $\mathbb{T}$ the times and, for a ramp which slows down, the
times which are left.

*Speeding up from $V \ge 0$.* $\mathrm{RN}(V t)$ and $G(t)$ are nondecreasing, so their rounded sum is (M1).

*Slowing down to $F \ge 0$.* $\ell$ is nonincreasing in $t$ (M1), $R$ is nondecreasing in $\ell$ as in the first
case, so $\mathrm{RN}(W - R(\ell))$ is nondecreasing in $t$ (M1), and so is its maximum with zero. For $t > 0$
that maximum is at least $0 = D(0)$.

*A complete profile.* With $A$ the time of the acceleration ramp, $K$ the coast, $V_m \ge 0$ the speed between
the ramps and $D_A$ the distance of the acceleration ramp at $A$, the code returns the distance of the
acceleration ramp for $t \le A$; $\mathrm{RN}(D_A + \mathrm{RN}(V_m \mathrm{RN}(t - A)))$ for
$A < t < \mathrm{RN}(A + K)$; and
$\mathrm{RN}(\mathrm{RN}(D_A + \mathrm{RN}(V_m K)) + D_{dec}(t'))$ with
$t' = \min(\mathrm{RN}(t - \mathrm{RN}(A + K)), \text{Total}_{dec})$ after that. Each piece is nondecreasing by
(M1). The first ends at $D_A$ and the second starts at or above it (M2). In the second, $t < \mathrm{RN}(A + K)$
implies $t - A < K$ exactly: if the sum was rounded up, its predecessor in $\mathbb{F}$ is below $A + K$. So
$\mathrm{RN}(t - A) \le K$ and the second piece stays at or below $\mathrm{RN}(D_A + \mathrm{RN}(V_m K))$ (M1),
where the third starts or above which it lies (M2, as $D_{dec} \ge 0$). The planner's
`Profile_Distance_At_Time`, with a crackle of its own for each ramp, is the same composition. $\square$

**3.7 Prepared ramps.** `Prepare` makes each stage with `Make_Stage` from a state which it obtains by the calls
`Rising_Stage` makes: the closed form of the group, then `Advance` with the lengths and crackles of the stages
between, in the same order. The ranges are the same expressions of $D_4$, $D_8$, $D_{12}$. The bounds come from
the one `Ramp_Bounds`, the stage from the one `Stage_At`, the gain from the one `Stage_Gain` and the distance from
the one `Ramp_Distance`, on the same `Ramp_Shape`. Functions return equal results for equal arguments, so the two
forms agree. The subprograms which round are not inlined, so that this holds also for a compiler which contracts a
product and a sum into one fused operation (Theorem 1 does not hold for such a compiler, see section 0). SPARK
proves this theorem for `Prunt.Motion_Planner.Trajectory.Profiles` (section 6).

## 4. Within a stage

Fix a stage with coefficients $c_1..c_5 \in \mathbb{F}$ and length $L \in \mathbb{F}$, $L \ge 0$. Write
$p(t) = c_1 t + c_2 t^2 + c_3 t^3 + c_4 t^4 + c_5 t^5$ for the *exact* polynomial with these rounded
coefficients, and $\tilde{p}(t) = \sum |c_i| t^i$. What has to be shown is that $\hat{C}$, the result of
`Compensated_Local`, is nondecreasing on $\mathbb{F} \cap [0, L]$.

**4.1 Exact errors.** For $a, b \in \mathbb{F}$ with $s = \mathrm{RN}(a + b)$, `Sum_Error (a, b, s)` is
$a + b - s$ exactly (Knuth's two-sum; Knuth, *The Art of Computer Programming* vol. 2, 4.2.2, Theorem B). For
$a, x \in \mathbb{F}$ with $\pi = \mathrm{RN}(a x)$, `Product_Error (a, High_Half (a), x, High_Half (x), π)` is
$a x - \pi$ exactly, provided the product does not underflow (Dekker's product with Veltkamp's splitting; Dekker,
"A floating-point technique for extending the available precision", Numer. Math. 18, 1971, Theorems 4.6 and 5.7).

**Hypothesis N.** Every product formed in `Compensated_Local` is zero or at least $2^{-900}$ in magnitude.

For a machine this excludes nothing: at $t = 10^{-12}$ s into a ramp with $C_m = 10^4$ mm/s⁵ the smallest product
is above $10^{-100}$. Section 5.2 says what happens below.

**4.2 The compensated Horner scheme.** `Compensated_Local` is the compensated Horner scheme of Graillat, Langlois
and Louvet ("Compensated Horner scheme", 2005) for the polynomial $p$ of degree 5 with constant term zero: `P` and
`S` are the plain scheme, each product and sum hands its exact error to `R`, which is itself a Horner scheme, and
the result is $\mathrm{RN}(P_0 + R)$. By their Theorem 4 the plain result plus the exact value of the error
polynomial is $p(x)$, and by their Lemma 3 the computed $R$ is within
$\gamma_9 \gamma_{10}\, \tilde{p}(x)$ of that exact value, with $\gamma_n = nu/(1 - nu)$. So, under N,

$$\hat{C}(x) = \mathrm{RN}(p(x) + \eta_x), \qquad |\eta_x| \le \gamma_{10}^2\, \tilde{p}(x) < 2^{-99.3}\,
\tilde{p}(x).$$

**4.3 Lemma C.** *Let $\rho \ge 2^{-41}$ and suppose $t\, p'(t) \ge \rho\, \tilde{p}(t)$ for all real
$t \in [0, L]$. Then $\hat{C}(x) \le \hat{C}(y)$ for all $x < y$ in $\mathbb{F} \cap [0, L]$ at which N
holds, and $\hat{C} \ge 0$ there.*

*Proof.* $p' \ge 0$ and $\tilde{p}(t)/t$ is nondecreasing. At $x = 0$ every product and every error is zero, so
$\hat{C}(0) = 0 = p(0)$ and $\eta_0 = 0$.

If $y \ge 2x$: $p(y) - p(x) \ge \int_{y/2}^{y} p' \ge \rho \int_{y/2}^{y} \tilde{p}(t)/t\, dt \ge \rho\,
\tilde{p}(y/2) \ge 2^{-5} \rho\, \tilde{p}(y)$, while $|\eta_x| + |\eta_y| \le 2 \gamma_{10}^2\,
\tilde{p}(y) < 2^{-98}\, \tilde{p}(y)$.

If $x < y < 2x$: two members of $\mathbb{F}$ are at least a unit in the last place apart, so $y - x > 2^{-53} x$,
and $p(y) - p(x) = \int_x^y p' \ge \rho\, (y - x)\, \tilde{p}(x)/x > 2^{-53} \rho\, \tilde{p}(x)$, while
$\tilde{p}(y) \le 2^5\, \tilde{p}(x)$ gives $|\eta_x| + |\eta_y| \le 33 \gamma_{10}^2\, \tilde{p}(x) <
2^{-94}\, \tilde{p}(x)$.

In both cases $p(x) + \eta_x \le p(y) + \eta_y$, and $\mathrm{RN}$ keeps the order. With $x = 0$ this is
$\hat{C}(y) \ge 0$. $\square$

The margin is the point of the compensation. The exact polynomial moves by about half a unit in the last place of
its value between adjacent times, so a result with an error of a few units in the last place, as the plain Horner
scheme gives, can decrease. The compensated result is wrong by $2^{-99}$ of $\tilde{p}$ before its single
rounding.

**4.4 Lemma R and regular stages.** *Let $K = 2^{14}$. If $c_1 \ge 0$ and $p'' \ge 0$ on $[0, L]$, then
$\tilde{p}(t) \le K\, t\, p'(t)$ on $[0, L]$. If instead $c_1 > 0$ and $p'' \ge -\varepsilon$ on $[0, L]$ for
some $\varepsilon \ge 0$ with $\varepsilon L \le c_1 / 2$, then $\tilde{p}(t) \le 4K\, t\, p'(t)$.*

A stage which satisfies one of the two conditions is **regular**. A regular stage satisfies the hypothesis of
Lemma C with $\rho = 2^{-16}$.

*Proof.* First a fact about cubics. Let $q(s) = q_0 + q_1 s + q_2 s^2 + q_3 s^3 \ge 0$ on $[0, 1]$ with maximum
$M$ there. Interpolating at $0, 1/3, 2/3, 1$ bounds the coefficients by the values: $|q_0| \le M$,
$|q_1| \le 20M$, $|q_2| \le 54M$, $|q_3| \le 36M$. So $|q'| \le 236M$ on $[0, 1]$, and next to a point where $q
= M$ there is an interval of length $1/472$ inside $[0, 1]$ on which $q \ge M/2$; hence $\int_0^1 q \ge M/944$.
Together,

$$\frac{|q_0|}{2} + \frac{|q_1|}{6} + \frac{|q_2|}{12} + \frac{|q_3|}{20} \le 10.2\, M \le K \int_0^1 q.$$

Now fix $t \in (0, L]$ and take $q(s) = p''(s t)$, a cubic in $s$ with $q_0 = 2c_2$, $q_1 = 6c_3 t$,
$q_2 = 12c_4 t^2$, $q_3 = 20c_5 t^3$, which is not negative on $[0, 1]$ in the first case. The left side above is
$|c_2| + |c_3| t + |c_4| t^2 + |c_5| t^3 = (\tilde{p}(t) - c_1 t)/t^2$, and $\int_0^1 q = (p'(t) - c_1)/t$. So
$\tilde{p}(t) - c_1 t \le K\, t\, (p'(t) - c_1)$, and since $c_1 \ge 0$ and $K \ge 1$,
$\tilde{p}(t) \le K\, t\, p'(t)$.

In the second case apply the fact to $q + \varepsilon$: the left side grows by at most $\varepsilon / 2$ and the
integral by $\varepsilon$, so $\tilde{p}(t) - c_1 t \le K\, t\, (p'(t) - c_1) + (K + \tfrac12)\, \varepsilon t^2$.
As $p'' \ge -\varepsilon$, $p'(t) \ge c_1 - \varepsilon t \ge c_1/2$, so $\varepsilon t^2 \le t\, p'(t)$ and
$\tilde{p}(t) \le (2K + \tfrac12)\, t\, p'(t)$. $\square$

In words: a quintic which starts at zero, does not start downhill and is convex cannot hide a small slope behind
large terms which cancel. The best constant for the first case is about 14.7 (found by linear programming), not
$2^{14}$, but any constant below $2^{39}$ would do.

**Hypothesis S.** Every stage of the ramp is regular.

$p'$ is the speed which the stage has gained since the ramp started and $p''$ its acceleration, as the rounded
state has them. Section 5.1 discusses why S holds.

**4.5 Proof of Theorem 1.** By S and Lemma R every stage satisfies the hypothesis of Lemma C, so every
$\hat{C}_k$ is nondecreasing on the times into the stage at which N holds. Then 3.5 and 3.6 give the theorem.
$\square$

## 5. What the proof leaves open

**5.1 Hypothesis S.** In exact arithmetic it holds with $\varepsilon = 0$: the speed of a ramp which speeds up
never falls below its starting speed (`Motion.Lemma_Velocity_Range`) and its acceleration lies between zero and
its peak (`Derivatives.Lemma_Stage_Range`, `Lemma_Limits`).

For the rounded state it is immediate in five stages. In stages 1, 2, 4 and 8 every coefficient is a rounded sum
or product of numbers which are not negative, so $p'' \ge 0$ by (M3). In stage 3 the only negative coefficient is
$c_5$, and $12 c_4 t^2 + 20 c_5 t^3 \ge 0$ for $t \le T_1$ because $c_4$ is $C_m T_1 / 24$ and $c_5$ is
$-C_m / 120$, each to one or two roundings.

In the other ten stages the jerk or the acceleration has fallen from a peak, and the rounded state has it with
the absolute error of the peak, so the rounded acceleration polynomial $p''$ can dip below zero where the exact
acceleration touches zero. The outline of a proof that the dip is within the slack is this.

* *The errors are small against the peaks.* Write $S_{max} = C_m T_1$, $J_{max} = S_{max} (T_1 + T_2)$ and
  $A_{max} = J_{max} (2T_1 + T_2 + T_3)$ for the peaks of snap, jerk and acceleration. Each term which `Advance`
  adds to the snap, the jerk or the acceleration is at most the peak of that quantity. That is so although $T_3$
  and $T_4$ may be far longer than $T_1$ and $T_2$, because stages 4, 8 and 12 start from closed forms whose snap
  is exactly zero, and stage 8 from one whose jerk is exactly zero as well. So after at most three calls the
  rounded snap, jerk and acceleration are within a small multiple of $u$ times their peaks of the exact ones, and
  on a stage of
  length $T_1$, $T_2$ or $T_3$ the rounded $p''$ is within a small multiple of $u A_{max}$ of the exact
  acceleration, which is not negative. (On stage 8, of length $T_4$, $p''$ is the constant $2c_2 \ge 0$.)
* *The speed is large against that.* From stage 9 on, the speed gained is at least that at the start of stage 8,
  $A_{max} (4T_1 + 2T_2 + T_3)/2$, which is at least $A_{max} L / 2$ for every later stage length $L$. In stages 5
  to 7 the exact acceleration is rising from its value at the start of stage 5 and the same comparison holds with
  the speed at the start of stage 4.

So $\varepsilon L$ is a small multiple of $u$ times $c_1$, where $1/2$ is allowed. What is missing for a proof is
the count of roundings which turns "a small multiple" into a number for each of the ten stages.

**5.2 Underflow.** Below hypothesis N the error of a product is no longer exact, and the result can be off by a
few multiples of $2^{-1074}$. In every stage but the first, $\text{Dist}_k$ is then so much larger that the sum
with it absorbs this. In the first stage of a ramp from rest the distance itself is of that size, at times below
about $10^{-60}$ s, and there nothing is proved.

**5.3 Ramps which turn back.** A ramp which slows down past rest is evaluated forwards, with the cancellation the
mirrored form avoids. Its exact distance decreases too.

**5.4 Runs.** The planner evaluates a run as a prefix, a central profile and a suffix, each on its own, and each
ends within rounding of where the next begins, on either side. In the plans of a real print (47,924 runs in the
default mode, 36,555 in continuous mode) the distance of the pieces taken on their own fell across 0.8% of the
joins of a prefix with the central profile, across 3% to 4% of the joins of the central profile with a suffix, and
at the end of 9% to 11% of the runs, by at most 15 units in the last place ($1.1 \cdot 10^{-14}$ mm). A segment's
distance is limited to the segment before it is used, so such a fall would move nothing, but the bisection which
places the members of a run assumes there is none.

`Timing_Distance_At_Time` therefore keeps each piece between the run's distances at its two ends: zero, the
start of the window, the end of the window and the whole distance, put in order with $\min$ and $\max$. By (M1)
that preserves the order within a piece, and it makes the last value of a piece at most the first of the next, so
the run's distance does not decrease if the distance of none of its three profiles does, which is Corollary 3.
With it no join of those plans falls, and the plans are the same to the last bit as without it.

**5.5 Final speeds below zero.** A deceleration ramp whose final speed is negative by less than $2^{-40}$ of its
starting speed is evaluated as ending at rest. That keeps the theorem's condition $F \ge 0$ and lengthens the
distance at $t$ by the final speed times $t$, which is at most $2^{-40}$ of the starting speed times $t$. A ramp
which ends further below zero is evaluated forwards, as one which turns back (5.3). In the same plan 965 of
14,757 deceleration ramps of central profiles ended below zero, all by less than $1.2 \cdot 10^{-15}$ of the
starting speed, which is about $2^{-50}$, so the change was at most $2 \cdot 10^{-14}$ mm.

## 6. What SPARK proves of this

GNATprove proves the following about `Prunt.Motion_Planner.Trajectory.Profiles`, with no assumption and no justified
check. The names are those of the package.

* **No run-time error.** No operation of the evaluator overflows for a ramp of the proved domain, and none of its
  `raise` statements is reached.
* **Theorem 2** (`Lemma_Prepared_Distance`): the prepared and the unprepared distance are equal. An equality of
  floating-point numbers or of records in a postcondition is not the identity which makes two calls the same
  call, because a zero has two signs, so the proof cannot go through the stages as values. It is made where both
  evaluations can be seen through: `Prepare` is an expression function with the loop over the stages written
  out, and `Rising_Stage` has no contract, so that it is analysed inside `Distance_At_Time`.
* **3.1** (`Ramp_Bounds`, `Stage_At`, `Lemma_Stage_Order`): the bounds are in order, the selected stage contains
  the time, and a later time is not in an earlier stage.
* **3.2 to 3.4** (`Lemma_Group_Order`, `Is_Ordered`, `Lemma_Prepared_Is_Ordered`): $0 \le D_4 \le D_8$ as rounded,
  the end of a stage is not below its start, and in the table of a prepared ramp $Lo_k \le Hi_k \le Lo_{k+1}$.
* **3.5 and 3.6 for a ramp** (`Lemma_Distance_Is_Monotone`): for a prepared ramp whose table is in order and
  which speeds up from a speed which is not negative or slows down to one, and two times in order, the distance
  at the later time is not smaller, provided that `Stages_Are_Monotone` holds for the two times.

`Stages_Are_Monotone` is the hypothesis which section 4 argues: if the two times are evaluated in the same
stage, the compensated polynomial of that stage is no smaller for the later of the two times into it. It is
stated for the two times because a quantified expression cannot range over floating-point numbers, which also
makes it weaker than monotonicity over the whole stage. Nothing in the package proves or assumes it.

Not proved in SPARK: section 4, of which 4.1 is a statement about the bits of a product and 4.3 and 4.4 are
analysis over the reals; the composition of two ramps and a coast into a complete profile (Corollary 3); and that
the gain is zero at the start of a ramp and never negative. The proofs model every operation as rounded on its
own, which is the condition on the compiler in section 0.

The order of products needed more than the provers do directly: $a b \le a' b$ in rounded arithmetic, with three
unknowns, is proved through the reals (the real product, then monotone rounding), because SPARKlib's lemma is
limited to magnitudes below $2^{511}$.
