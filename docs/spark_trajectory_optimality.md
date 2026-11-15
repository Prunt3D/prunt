# Optimality of the selected feedrate profile

This is a proof on paper that the profile `Try_Optimal_Full_Profile` selects is the fastest member of the
bounded-crackle profile family, and that each ramp `Optimal_Profile_For_Delta_V` returns is the fastest ramp of
that family for its velocity change. The results about a single ramp are also proved in SPARK, over exact phase
times, by `Prunt.Motion_Planner.Trajectory.Profile_Model.Optimality` (`Lemma_Saturating_Is_Fastest`,
`Lemma_Delta_V_Case_Is_Fastest` and the lemmas they rest on); the argument about the full profile is on paper only.

The subprograms named here are those of `Prunt.Motion_Planner.Trajectory.Solvers`
(`src/prunt-motion_planner-trajectory-solvers.adb`) unless another unit is named, and the units are those of
`Prunt.Motion_Planner.Trajectory`, which [spark_trajectory.md](spark_trajectory.md) describes.

## 0. What is proved

Fix the limits $A_m, J_m, S_m, C > 0$ (acceleration, jerk, snap, crackle), the speeds $V_s, V_e \ge 0$, the speed
limit $V_{max} \ge \max(V_s, V_e)$ with $V_{max} > 0$, and the distance $D > 0$.

* **Theorem 1 (ramps).** For each velocity change $g > 0$ exactly one ramp of the family (15 stages, crackle $\pm C$,
  phase times $T_1..T_4 \ge 0$) changes the velocity by $g$ within the limits in the least time, and the formulas
  of `Prepare_Delta_V` and `Optimal_Profile_For_Delta_V_Internal` give exactly that ramp in every region and case.
* **Theorem 2 (full profiles).** A member of the 31-phase family (an accelerating ramp, a coast, a decelerating
  ramp) that goes from $V_s$ to $V_e$ over exactly $D$ within the limits and under $V_{max}$ exists exactly when the
  ramp straight from $V_s$ to $V_e$ fits in $D$, which is the test `Try_Optimal_Full_Profile` makes. When one
  exists, the fastest member has the highest peak whose two fastest ramps fit, or $V_{max}$ if those fit, and those
  two ramps; any other peak is strictly slower. That is the profile the function builds.
* **Proposition 3 (search tolerance).** A peak a relative $\varepsilon$ below the best one costs at most a factor
  $1 + \varepsilon$ in duration. The peak search stops at $\varepsilon = 2^{-40}$. Section 6 shows that this loss
  is first order, not second order.
* **Remarks.** A ramp with a smaller crackle never does better (7.1). `Optimal_Profile_For_Distance` is optimal in
  its own sense too (7.2). The family is **not** time-optimal among *all* motions with bounded crackle: a 4-arc
  bang-bang ramp makes the same velocity change about 7% faster (7.3). So "optimal" here means optimal within the
  family.

Everything here is exact real arithmetic, with the one-dimensional equations the code solves by bisection solved
exactly. Section 7.4 lists what changes in floating point.

## 1. The family

A **ramp** is $T = (T_1, T_2, T_3, T_4) \in \mathbb{R}^4_{\ge 0}$ run with crackle $\pm C$ through the 15 stages
of the comment on `Feedrate_Profile_Times`. It starts from snap, jerk and acceleration zero at some speed $V$. Its
crackle is $+C$ when it accelerates and $-C$ when it decelerates (as in `Velocity_At_Time` on a `Feedrate_Profile`
in `Profiles`). The proof uses these facts about one ramp. All are proved in SPARK over exact reals, by the lemmas
of `Profile_Model` named with them.

* **(F1) Peaks.** Over the whole ramp, $|\text{snap}| \le C T_1$, $|\text{jerk}| \le C T_1 (T_1 + T_2)$ and
  $|\text{acceleration}| \le C T_1 (T_1+T_2)(2T_1+T_2+T_3)$ (`Derivatives.Lemma_Limits`). The bounds are
  *attained*: at the end of stage 1, 3 and 7 respectively. So the ramp respects the limits exactly when
  $CT_1 \le S_m$, $CT_1(T_1+T_2) \le J_m$ and $CT_1(T_1+T_2)(2T_1+T_2+T_3) \le A_m$.
* **(F2) End state.** Snap, jerk and acceleration are zero at the end. The velocity changes by
  $G(T) = C T_1(T_1+T_2)(2T_1+T_2+T_3)(4T_1+2T_2+T_3+T_4)$ (the gain), the duration is
  $8T_1 + 4T_2 + 2T_3 + T_4$, and the distance is the duration times the mean of the start and end speeds
  (`Derivatives.Lemma_Ends_At_Rest`, `Motion.Lemma_End`).
* **(F3) Monotone.** The velocity stays between its start and end values (`Motion.Lemma_Velocity_Range`).

A **profile** $(A, k, B)$ is a ramp $A$ with crackle $+C$ from $V_s$, then a coast of $k \ge 0$ at the speed
$V_p = V_s + G(A)$ that $A$ reaches (the *peak*), then a ramp $B$ with crackle $-C$ from $V_p$. It is a
**member** of the family $\mathcal{F}$ when both ramps respect the limits, it ends at $V_e$
($V_p - G(B) = V_e$), it covers exactly $D$, and its speed never exceeds $V_{max}$. By (F3) the speed runs from
$V_s$ up to $V_p$ and down to $V_e$, so the last condition is just $V_p \le V_{max}$. Its duration is the two
ramp durations plus $k$.

## 2. Coordinates in which the ramp problem is simple

Put

$$x_1 = T_1,\quad x_2 = T_1 + T_2,\quad x_3 = 2T_1 + T_2 + T_3,\quad x_4 = 4T_1 + 2T_2 + T_3 + T_4,$$

and write $p_k = x_1 + \dots + x_k$ for the partial sums. Then:

* the duration is $p_4 = x_1 + x_2 + x_3 + x_4$;
* the gain is $C\, x_1 x_2 x_3 x_4$;
* the three peaks are $C x_1$, $C x_1 x_2$ and $C x_1 x_2 x_3$;
* the map is a bijection between $T \ge 0$ and the cone $x_1 \ge 0$, $x_k \ge p_{k-1}$ ($k = 2, 3, 4$), with
  inverse $T_1 = x_1$ and $T_k = x_k - p_{k-1}$.

With $\sigma = S_m/C$, $\iota = J_m/C$, $\kappa = A_m/C$ (units s, s², s³), let
$L_1 = \sigma$, $L_2 = \iota$, $L_3 = \kappa$, $L_4 = \infty$. Let

$$F_k = \{x \in \mathbb{R}^k : x_1 \ge 0;\ x_i \ge p_{i-1}\ (2 \le i \le k);\ x_1 \cdots x_i \le L_i\ (1 \le i \le k)\}.$$

The ramps that respect the limits are exactly the points of $F_4$, by (F1). The **ramp problem** for a gain
$g > 0$ is to minimise $p_4$ over $x \in F_4$ with $C x_1x_2x_3x_4 = g$.

Two properties of $F_k$ are used. Every coordinate of a point of $F_k$ is nonnegative. And $F_k$ is closed under
scaling by $\lambda \in [0, 1]$: the ordering constraints are homogeneous, and every product only shrinks.

## 3. The fastest ramp

Let $M_k(p) = \max\{x_1 \cdots x_k : x \in F_k,\ p_k \le p\}$: the largest product, which is the largest gain over
$C$, for a total of at most $p$. The maximum exists because the set is compact: it is closed, and its
coordinates are nonnegative with sum at most $p$.

**Class $\mathcal{M}$.** A function $m : [0, \infty) \to [0, \infty)$ is in $\mathcal{M}$ when it is continuous and
strictly increasing with $m(0) = 0$, and $m(p)/p$ is nondecreasing on $(0, \infty)$. Such an $m$ is unbounded,
since $m(p) \ge m(1)\, p$ for $p \ge 1$. So for $L \in (0, \infty]$ the threshold $Q = m^{-1}(L)$ exists, with
$Q = \infty$ when $L = \infty$.

**Lemma A (one level).** Let $m \in \mathcal{M}$, $L \in (0, \infty]$, $Q = m^{-1}(L)$ and $M = \min(L, m)$. For
$p > 0$ the function $\varphi(q) = M(q)(p - q)$ on $[0, p/2]$ has exactly one maximiser, $q^* = \min(p/2, Q)$.
The maximum value $m^+(p) = \varphi(q^*)$ is

$$m^+(p) = \begin{cases} \tfrac{p}{2}\, m(\tfrac{p}{2}) & p \le 2Q \\ L\,(p - Q) & p \ge 2Q, \end{cases}$$

and $m^+$ is in $\mathcal{M}$.

*Proof.* On $[0, \min(p/2, Q)]$ we have $M = m$, because $m(q) \le m(Q) = L$ there. For
$0 < q_1 < q_2 \le \min(p/2, Q)$,

$$\frac{\varphi(q_2)}{\varphi(q_1)} = \frac{m(q_2)}{m(q_1)} \cdot \frac{p - q_2}{p - q_1} \ \ge\ \frac{q_2}{q_1} \cdot \frac{p - q_2}{p - q_1} = \frac{q_2(p - q_2)}{q_1(p - q_1)} \ >\ 1.$$

The first step holds because $m(q)/q$ is nondecreasing, and the last because $q(p - q)$ is strictly increasing on
$[0, p/2]$. Also $\varphi(0) = 0 < \varphi(q)$ for $q > 0$. So $\varphi$ is strictly increasing on
$[0, \min(p/2, Q)]$. If $Q < p/2$, then on $[Q, p/2]$ we have $\varphi = L(p - q)$, which is strictly decreasing.
Hence the maximiser is unique and equals $\min(p/2, Q)$, and the formula for $m^+$ follows.

Now check that $m^+ \in \mathcal{M}$. The two pieces agree at $p = 2Q$, where both equal $QL$. On $[0, 2Q]$, $m^+$
is the product of the strictly increasing $p/2$ and the nondecreasing $m(p/2)$, which is positive for $p > 0$; on
$[2Q, \infty)$ it is linear with slope $L > 0$. So $m^+$ is continuous and strictly increasing with $m^+(0) = 0$.
Its ratio $m^+(p)/p$ is $m(p/2)/2$ on the first piece and $L(1 - Q/p)$ on the second. Both are nondecreasing and
they agree at $2Q$, so the ratio is nondecreasing. $\square$

**Lemma B (recursion).** Let $m_1(p) = p$, and for $k = 2, 3, 4$ let $m_k = m_{k-1}^+$, formed with
$L = L_{k-1}$. Let $Q_k = m_k^{-1}(L_k)$ for $k = 1, 2, 3$. Then every $m_k$ is in $\mathcal{M}$, and
$M_k = \min(L_k, m_k)$. In particular $M_4 = m_4$.

*Proof.* By induction on $k$. The function $m_1$ is in $\mathcal{M}$ and $M_1(p) = \max\{x_1 : 0 \le x_1 \le
\min(p, \sigma)\} = \min(L_1, m_1(p))$. For $k \ge 2$, suppose $M_{k-1} = \min(L_{k-1}, m_{k-1})$.

*Upper bound.* Take $x \in F_k$ with $p_k \le p$, and let $q = p_{k-1}$. Ordering gives $x_k \ge q$, and the sum
gives $x_k \le p - q$, so $q \le p/2$. The point $(x_1, \dots, x_{k-1})$ lies in $F_{k-1}$ with sum $q$, so
$x_1 \cdots x_k \le M_{k-1}(q)(p - q) \le m_k(p)$ by Lemma A. The limit gives $x_1 \cdots x_k \le L_k$ as well.

*Attained.* Let $q^*$ be the maximiser from Lemma A, and let $y \in F_{k-1}$ attain $M_{k-1}(q^*)$ with sum at most
$q^*$. Then $(y, p - q^*)$ satisfies the ordering, because $p - q^* \ge q^* \ge \sum y$, and its product is
$m_k(p)$. If $m_k(p) > L_k$, scale it by $\lambda = (L_k/m_k(p))^{1/k} < 1$. That keeps it in $F_{k-1} \times
\mathbb{R}$, with the ordering intact and the sum at most $p$, and makes the product exactly $L_k$. $\square$

**Lemma C (the maximiser is unique).** For $p > 0$ define

$$r_4 = p,\qquad r_3 = \min(r_4/2,\ Q_3),\qquad r_2 = \min(r_3/2,\ Q_2),\qquad r_1 = \min(r_2/2,\ Q_1),$$

where $Q_1 = \sigma$. The only $x \in F_4$ with $p_4 \le p$ and $x_1x_2x_3x_4 = m_4(p)$ is the one with partial
sums $p_k = r_k$, that is $x_1 = r_1$ and $x_k = r_k - r_{k-1}$.

*Proof.* Let $x$ be any maximiser. We show by downward induction that $p_k = r_k$ and
$x_1 \cdots x_k = m_k(r_k)$, with $m_k(r_k) \le L_k$.

* $k = 4$: $m_4(p) = x_1 \cdots x_4 \le M_4(p_4) = m_4(p_4)$. Since $m_4$ is strictly increasing, $p_4 = p$.
* From $k$ to $k - 1$: let $q = p_{k-1}$. Then $x_k = r_k - q$, and $q \in [0, r_k/2]$ by ordering. So
  $m_k(r_k) = x_1 \cdots x_k \le M_{k-1}(q)(r_k - q) \le m_k(r_k)$, and both inequalities are equalities.
  So $q$ maximises $\varphi$ for $p = r_k$, which means $q = r_{k-1}$ by Lemma A, and
  $x_1 \cdots x_{k-1} = M_{k-1}(r_{k-1})$, using $r_k - q > 0$. Since $r_{k-1} \le Q_{k-1}$, this equals
  $m_{k-1}(r_{k-1}) \le L_{k-1}$.

So the maximiser, which exists by Lemma B, has these partial sums. $\square$

**Theorem 1.** Let $g > 0$ and $S = m_4^{-1}(g/C)$. Every ramp that respects the limits and has gain $g$ lasts at
least $S$. Exactly one lasts $S$: the point of Lemma C for $p = S$, whose phase times are

$$T_1 = r_1,\quad T_2 = r_2 - 2r_1,\quad T_3 = r_3 - 2r_2,\quad T_4 = S - 2r_3.$$

*Proof.* Such a ramp is a point of $F_4$ with $g = C x_1 \cdots x_4 \le C\, m_4(p_4)$. Since $m_4$ is strictly
increasing, $p_4 \ge S$. If $p_4 = S$, then the product is $m_4(S)$, and Lemma C says there is only one such
point. The formula for $T$ is the inverse map of section 2. $\square$

Each $T_k$ with $k \ge 2$ is positive exactly when the minimum that defines $r_{k-1}$ takes its second argument
with room to spare. Equivalently, the $(k-1)$-th limit (snap, jerk, acceleration) binds and the phase that holds
it lasts a positive time. This is the "reachable" structure the code's comments describe.

**Corollary 1.** Let $\tau(g)$ be the least duration for gain $g \ge 0$, with $\tau(0) = 0$, so that
$\tau = m_4^{-1}(\cdot / C)$. Then

1. $\tau$ is continuous and strictly increasing;
2. $\tau(g)/g$ is nonincreasing on $(0, \infty)$: $\tau(\lambda g) \le \lambda \tau(g)$ for $\lambda \ge 1$;
3. $\tau(g)/g^{1/4}$ is nondecreasing.

*Proof.* (1) holds because $m_4 \in \mathcal{M}$. (2): let $p = \tau(g)$. Since $m_4(p)/p$ is nondecreasing,
$m_4(\lambda p) \ge \lambda m_4(p) = \lambda g/C$, so $\tau(\lambda g) \le \lambda p$. (3): for $\lambda \le 1$,
scaling the fastest ramp for $g$ by $\lambda$ stays in $F_4$ and gives gain $\lambda^4 g$, so
$\tau(\lambda^4 g) \le \lambda \tau(g)$. $\square$

## 4. The ramp solver computes it

The thresholds, worked out from Lemma A:

* $m_2(p) = p^2/4$ for $p \le 2\sigma$, and $\sigma(p - \sigma)$ for $p \ge 2\sigma$.
* $Q_2 = m_2^{-1}(\iota)$: $Q_2 = 2\sqrt{\iota}$ if $\iota \le \sigma^2$, and $Q_2 = \sigma + \iota/\sigma > 2\sigma$
  if $\iota > \sigma^2$.
* $m_3(p) = \tfrac{p}{2} m_2(\tfrac{p}{2})$ for $p \le 2Q_2$, which is $p^3/32$ up to $p = 4\sigma$ and then
  $\tfrac{p}{2}\sigma(\tfrac{p}{2} - \sigma)$. For $p \ge 2Q_2$, $m_3(p) = \iota(p - Q_2)$. Two values are used
  below: $m_3(2Q_2) = Q_2\,\iota$, and $m_3(4\sigma) = 2\sigma^3$ when $4\sigma \le 2Q_2$.
* $Q_3 = m_3^{-1}(\kappa)$, and $m_4(p) = \tfrac{p}{2} m_3(\tfrac{p}{2})$ for $p \le 2Q_3$, $\kappa(p - Q_3)$
  for $p \ge 2Q_3$.

**Regions.** The tests in `Prepare_Delta_V` decide where $Q_3$ falls among the breakpoints of $m_3$. That fixes
$Q_3$ in closed form. ($\mathrm{JoS} = J_m/S_m = \iota/\sigma$, $\mathrm{SoC} = S_m/C = \sigma$,
$\mathrm{AoJ} = A_m/J_m = \kappa/\iota$.)

| Region | Tests in `Prepare_Delta_V` | Meaning | $Q_3$ |
|---|---|---|---|
| 1 | $\mathrm{JoS} - \mathrm{SoC} > 0$, $\mathrm{AoJ} - \mathrm{JoS} - \mathrm{SoC} \ge 0$ | $\iota > \sigma^2$, $\kappa \ge \iota Q_2 = m_3(2Q_2)$, so $Q_3 \ge 2Q_2$ | $\kappa/\iota + \iota/\sigma + \sigma$ |
| 2 | $\iota > \sigma^2$, not 1, $\sqrt{\sigma^2/4 + \kappa/\sigma} - 3\sigma/2 \ge 0$ | $2\sigma^3 \le \kappa < \iota Q_2$, so $4\sigma \le Q_3 < 2Q_2$ | $\sigma + 2\sqrt{\sigma^2/4 + \kappa/\sigma}$ |
| 3 | $\iota > \sigma^2$, $\kappa < 2\sigma^3$ | $Q_3 < 4\sigma < 2Q_2$ | $(32\kappa)^{1/3} = 4(\kappa/2)^{1/3}$ |
| 4 | $\mathrm{JoS} - \mathrm{SoC} \le 0$, $\mathrm{AoJ} - 2\sqrt{J_m/C} > 0$ | $\iota \le \sigma^2$, $\kappa > 2\iota^{3/2} = m_3(2Q_2)$, so $Q_3 > 2Q_2$ | $\kappa/\iota + 2\sqrt\iota$ |
| 5 | $\iota \le \sigma^2$, $\kappa \le 2\iota^{3/2}$ | $Q_3 \le 2Q_2 = 4\sqrt\iota \le 4\sigma$ | $4(\kappa/2)^{1/3}$ |

Each $Q_3$ solves $m_3(Q_3) = \kappa$ on the piece of $m_3$ that the region selects. In region 2, for example,
$Q_3/2 = y$ solves $\sigma y (y - \sigma) = \kappa$. At a boundary between regions the two formulas agree, so it
does not matter which side the code's choice of strict or non-strict test puts the boundary on.

**Cases.** Let $S = \tau(g)$ and follow the minima of Lemma C. Each step below says which test the code makes and
why it is the right one. Each test compares $g$ with $C\,m_4$ at a breakpoint, and $m_4$ is strictly increasing,
so "$g$ above the threshold" is the same as "$S$ above the breakpoint".

**(Acceleration-limited) $S > 2Q_3$.** Here $r_3 = Q_3$, so $T_1..T_3$ depend only on the limits, and
$g = C\,m_4(S) = C\kappa(S - Q_3)$. So $T_4 = S - 2Q_3 = V_d/A_m - Q_3$. The condition is
$g > C\,m_4(2Q_3) = C\kappa Q_3$, that is $V_d/A_m - Q_3 > 0$. That is the `T4 > 0` test of every region in
`Optimal_Profile_For_Delta_V_Internal`, whose $Q_3$ terms are those in the table. The fixed times are
$r_2 = \min(Q_3/2, Q_2)$ and $r_1 = \min(r_2/2, \sigma)$:

* Region 1: $r_2 = Q_2$ and $r_1 = \sigma$, so $T = (\sigma,\ \iota/\sigma - \sigma,\ \kappa/\iota - Q_2,\ \cdot)$,
  which is `Solver.T1/T2/T3`.
* Region 2: $r_2 = Q_3/2$ and $r_1 = \sigma$, so $T_2 = \sqrt{\sigma^2/4 + \kappa/\sigma} - 3\sigma/2$ and
  $T_3 = 0$.
* Regions 3 and 5: $r_1 = Q_3/4 = (\kappa/2)^{1/3}$ and $T_2 = T_3 = 0$.
* Region 4: $r_2 = Q_2 = 2\sqrt\iota$ and $r_1 = \sqrt\iota$, so $T_2 = 0$ and $T_3 = \kappa/\iota - 2\sqrt\iota$.

**Otherwise** $S \le 2Q_3$. Then $T_4 = 0$, and $g = C\,u\,m_3(u)$ with $u = S/2 = r_3 \le Q_3$.

**(Jerk-limited) $u > 2Q_2$.** This needs $2Q_2 < Q_3$, so it can happen only in regions 1 and 4, and the code
tests it only there. Here $r_2 = Q_2$ and $g/C = \iota u (u - Q_2)$. So
$u = Q_2/2 + \sqrt{Q_2^2/4 + g/(C\iota)}$ and $T_3 = u - 2Q_2 = \sqrt{Q_2^2/4 + V_d/J_m} - 3Q_2/2$. The case
holds exactly when this is positive. If $u \le 2Q_2$, then $g/C \le 2Q_2\, m_3(2Q_2) = 2\iota Q_2^2$, and the
expression is at most $\sqrt{9Q_2^2/4} - 3Q_2/2 = 0$. With $Q_2 = \mathrm{JoS} + \mathrm{SoC}$ (region 1) and
$Q_2 = 2T_1$ (region 4), this is the `T3` which `Optimal_Profile_For_Delta_V_Internal` computes in those two
regions. $T_1$ and $T_2$ are the region's saturated values.

**Otherwise** $u \le 2Q_2$. Then $T_3 = 0$, and $g = 2C w^2 m_2(w)$ with $w = u/2 = r_2 \le Q_2$.

**(Snap-limited) $w > 2\sigma$.** This needs $Q_2 > 2\sigma$ and $Q_3 > 4\sigma$, so it can happen only in regions
1 and 2. Here $r_1 = \sigma$ and $g = 2C\sigma w^2 (w - \sigma)$. With $T_1 = \sigma$ and $T_2 = w - 2\sigma$
this is $g = C T_1(T_1+T_2)(2T_1+T_2)(4T_1+2T_2)$, the equation `Snap_Limited` solves. Its left side is strictly
increasing in $T_2$, so the root is unique. The case holds when $S > 8\sigma$, that is
$g > C\,m_4(8\sigma) = C \cdot 4\sigma \cdot m_3(4\sigma) = 8C\sigma^4 = 8 S_m^4/C^3$, which is `Snap_Only_Above`
as `Prepare_Delta_V` sets it.

**(Crackle-limited) Otherwise** $w \le 2\sigma$. Then $r_1 = w/2$, $T_2 = 0$ and $g = C w^4/2 = 8 C T_1^4$, so
$T_1 = (g/(8C))^{1/4}$. That is `Crackle_Limited`, inside `Optimal_Profile_For_Delta_V_Internal`.

The code makes these tests in this order, nested the same way: region 1 tests acceleration, jerk, snap, then
falls back to crackle; region 2 tests acceleration, snap, crackle; region 4 tests acceleration, jerk, crackle;
regions 3 and 5 test acceleration, crackle. Every test that is skipped is one shown impossible above. So in exact
arithmetic `Optimal_Profile_For_Delta_V` returns the ramp of Theorem 1 for every $g > 0$, and the empty ramp,
which is trivially optimal, for $g = 0$. $\square$

*Degenerate limits.* If any limit is zero, then $x_1 x_2 x_3 = 0$ or $C = 0$, so every ramp has gain zero: a ramp
with $T_1 = 0$ never applies crackle, so it is a coast. Only $g = 0$ is possible, which is what the `Zero_Limit`
branches allow.

## 5. The fastest full profile

For a peak $V \ge V_0 = \max(V_s, V_e)$ with $V > 0$, write $g_s = V - V_s$, $g_e = V - V_e$, and

$$R(V) = \tfrac{V_s + V}{2}\,\tau(g_s) + \tfrac{V_e + V}{2}\,\tau(g_e)$$

for the distance of the two fastest ramps (F2). Let

$$\Theta(V) = \tau(g_s) + \tau(g_e) + \frac{D - R(V)}{V} = \frac{D + \tfrac12\big(g_s\tau(g_s) + g_e\tau(g_e)\big)}{V}.$$

**Lemma D (fixed peak).** Any profile with peak $V$ that ramps from $V_s$ and to $V_e$ within the limits covers
$D$ for some $k \ge 0$ exactly when its ramps' distances total at most $D$. Its duration is

$$\frac{D}{V} + \frac{g_s\,\tau_A + g_e\,\tau_B}{2V},$$

where $\tau_A$ and $\tau_B$ are its ramp durations. So the members with peak $V$ exist exactly when
$R(V) \le D$, and the shortest of them lasts $\Theta(V)$. When $g_s > 0$ or $g_e > 0$, only the fastest ramp for
that gain attains it.

*Proof.* By (F2) the ramp distances are $\tfrac{V_s+V}{2}\tau_A$ and $\tfrac{V_e+V}{2}\tau_B$, and the coast is
$k = (D - \text{those})/V$. Adding the durations gives the formula. Its coefficients $g_s/(2V)$ and $g_e/(2V)$ are
nonnegative, so the duration is smallest when $\tau_A = \tau(g_s)$ and $\tau_B = \tau(g_e)$. The fastest ramps also
cover the least distance, so they fit whenever any ramps fit. Uniqueness is Theorem 1. A ramp of gain zero has
$T_1 = 0$: it is a coast at the speed it starts from, and only how it shares the time with $k$ changes, not the
motion. $\square$

**Lemma E (higher peaks are faster).** $R$ is continuous, strictly increasing and unbounded on $[V_0, \infty)$.
If $V_0 \le V < V'$ with $V > 0$ and $R(V') \le D$, then $\Theta(V) > \Theta(V')$.

*Proof.* The first sentence holds because $\tau$ is continuous and strictly increasing, and the factors
$V_i + V$ are increasing and positive. For the second, write $\tau_i = \tau(g_i)$, $\tau_i' = \tau(g_i')$ with
$g_i' = V' - V_i$, and $X_i$, $X_i'$ for the ramp distances. Then

$$\Theta(V) - \Theta(V') = \sum_{i \in \{s, e\}} \Big[\tau_i - \tau_i' + \frac{X_i' - X_i}{V}\Big] + \big(D - R(V')\big)\Big(\frac1V - \frac1{V'}\Big).$$

The last term is at least zero. Multiplying the bracket for one $i$ by $2V$ and expanding $X_i$ and $X_i'$ gives
$g\tau + (g' - 2g)\tau'$, writing $g = g_i < g' = g_i'$. This is positive:

* if $g = 0$, it is $g'\tau' > 0$;
* if $g' \ge 2g > 0$, both terms are nonnegative and $g\tau > 0$;
* if $g < g' < 2g$, Corollary 1.2 gives $\tau' \le (g'/g)\tau$, so
  $g\tau + (g' - 2g)\tau' \ge \tfrac{\tau}{g}\,\big(g^2 - 2gg' + g'^2\big) = \tfrac{\tau}{g}(g' - g)^2 > 0$.

$\square$

**Theorem 2.** Let $V_0 = \max(V_s, V_e)$.

1. $\mathcal{F}$ is nonempty exactly when $R(V_0) \le D$. $R(V_0) = \tfrac{V_s + V_e}{2}\tau(|V_s - V_e|)$ is the
   distance of the ramp straight from $V_s$ to $V_e$: `Direct_Distance` in `Try_Optimal_Full_Profile`.
2. If it is nonempty, let $V^* = V_{max}$ if $R(V_{max}) \le D$. Otherwise let $V^*$ be the unique
   $V \in [V_0, V_{max})$ with $R(V) = D$, which is also the largest $V \le V_{max}$ with $R(V) \le D$. The least
   duration in $\mathcal{F}$ is $\Theta(V^*)$. It is attained by the fastest ramps for gains $V^* - V_s$ and
   $V^* - V_e$, with coast $(D - R(V^*))/V^*$. Every member with a different peak is strictly slower, and every
   member with peak $V^*$ attains it only with those ramps, up to coasting moved into a gain-zero ramp.

*Proof.* Every member has $V_p \ge V_0$, since its ramps' gains are nonnegative, and $R(V_p) \le D$ by Lemma D.
$R$ is increasing, so $R(V_0) \le D$ is necessary. It is sufficient: if $V_0 > 0$, peak $V_0$ is feasible; if
$V_0 = 0$, then $R(V) \to 0$ as $V \to 0$, so a small enough peak is. The feasible peaks are therefore
$[V_0, V^*] \cap (0, \infty)$, and $V^*$ is well defined by continuity and strict monotonicity of $R$. A member
with peak $V$ lasts at least $\Theta(V)$ (Lemma D), and $\Theta(V) > \Theta(V^*)$ whenever $V < V^*$
(Lemma E). $\square$

**What `Try_Optimal_Full_Profile` does.** After rejecting invalid and degenerate inputs, it

* returns "unreachable" when $D <$ `Direct_Distance` (Theorem 2.1);
* builds the fastest ramps for peak $V_{max}$ (`Set_Peak`, which through `Ramps_To` calls
  `Optimal_Profile_For_Delta_V` on $|V_s - V|$ and $|V_e - V|$, Theorem 1), and coasts the rest if they fit
  (Theorem 2.2, first case);
* otherwise searches $[V_0, V_{max}]$ for the largest peak whose excess $R(V) - D$ is nonpositive, which is $V^*$
  in the second case, and coasts the remainder, which is zero for an exact root.

So in exact arithmetic, with exact roots, it returns the optimum of Theorem 2.

## 6. The peak search tolerance

**Proposition 3.** If $V$ is a feasible peak, with $V_0 \le V \le V^*$ and $V > 0$, then
$\Theta(V) \le (V^*/V)\,\Theta(V^*)$.

*Proof.* $g\,\tau(g)$ is nondecreasing and $g_i \le g_i^*$, so the numerator of $\Theta(V)$ is at most that of
$\Theta(V^*)$. Only the denominators $V \le V^*$ differ. $\square$

`Highest_Peak` stops with a peak $V$ that has a nonpositive excess, and a point at most $2^{-40}V$ above it that
has a positive excess or is the upper end of the bracket. (That is the postcondition of `Largest_Nonpositive`, in
`Searches`, with a positive tolerance.) Then $V^* < V(1 + 2^{-40})$, and the selected profile lasts at most
$(1 + 2^{-40})$ times the optimum.

**The duration is not stationary at $V^*$.** Where $R(V^*) = D$ and $\tau$ is differentiable,

$$\Theta'(V^*) = \sum_i \frac{g_i\tau'(g_i) - \tau(g_i)}{2V^*},$$

and $g\tau'(g) - \tau(g) < 0$. For example, it equals $-\tfrac34\tau$ where only crackle binds and $-K$ where
acceleration binds ($\tau = g/A_m + K$). Corollary 1.3 gives $g\tau' \ge \tau/4$, which bounds the relative loss
by about $\tfrac38\varepsilon$, with equality for a symmetric crackle-limited move from rest. At
$\varepsilon = 2^{-40}$ the loss is up to about $3.4 \times 10^{-13}$ of the duration: around twelve significant
digits, not sixteen. In practice this is nothing (a third of a picosecond per second of motion). The comment on
`Peak_Tolerance` in `Try_Optimal_Full_Profile` states this bound.

## 7. Remarks

### 7.1 A smaller crackle never helps

Suppose each ramp may use its own constant crackle $c \in (0, C]$. Take a ramp $x$ run with crackle $c$, and let
$\lambda = c/C$. The point $(\lambda x_1, x_2, x_3, x_4)$ still satisfies the ordering, since $x_2 \ge x_1 \ge
\lambda x_1$ and so on. Run with crackle $C$, it has the same gain and the same three peaks, and a duration shorter
by $(1 - \lambda)x_1$. By Lemma D, a shorter ramp of the same gain never lengthens a profile. So Theorems 1 and 2
hold over this larger family too, and the selected profile is still optimal.

### 7.2 `Optimal_Profile_For_Distance`

Nothing in Prunt calls it, but it is optimal in its own sense. Among ramps from $V_s$ that cover exactly $D$, the
duration is $D/(V_s + g/2)$, so the fastest one has the largest gain. Any ramp of gain $g$ is at least as long as
the fastest one, so it covers at least $X^*(g) = (V_s + g/2)\tau(g)$. $X^*$ is continuous and strictly increasing,
so the largest gain is the $g$ with $X^*(g) = D$, reached only by the fastest ramp for that $g$. The cases of
`Optimal_Profile_For_Distance_Internal` are the saturation points $r_k = Q_k$ of section 4 in order, and it frees
the next phase time from the last saturation point that $D$ exceeds. That walks along exactly the curve
$g \mapsto$ (fastest ramp for $g$), so the bisection's root is that ramp. The "impossible" entries repeat the
previous point and are skipped by the strict test.

### 7.3 The family is not optimal among all bounded-crackle motions

Run crackle $+C$ for $a$, $-C$ for $b$, $+C$ for $b$, then $-C$ for $a$, with $b = (1 + \sqrt2)a$. This ends
with snap, jerk and acceleration zero, gains $(17/6 + 2\sqrt2)\,C a^4$ with the velocity monotone, and takes
$(4 + 2\sqrt2)a$. Scaled to the same gain as the family's crackle-limited ramp ($8CT_1^4$ in $8T_1$), it takes
$0.9306$ times as long. With limits that do not bind, the fastest family ramp is about 7% slower than this
motion. This is what Feldbaum's theorem predicts: for a chain of four integrators, a time-optimal control is
bang-bang with at most four arcs, and the family's crackle-only ramp has six. So the theorems above are about the
family. A claim of optimality over every motion with these derivative bounds would be false.

### 7.4 What this does not cover

* **Floating point.** The theorems describe the exact-arithmetic profile. The executed code rounds each formula,
  and bisects the snap-limited $T_2$ and the distance solver's free time to adjacent machine numbers.
* **The 24-hour cap.** Both bisections search $[0, 86\,400\text{ s}]$. Where the true time is longer, the code
  silently returns a shorter time, a different ramp, and the theorems do not describe it. No physical move comes
  near this.
* **The rest of the planner.** Window selection, split and cellular profiles, mixed-derivative limits and time
  scaling choose *which* limits and speeds `Try_Optimal_Full_Profile` is called with. The theorems say that, given
  those, the profile is the fastest of the family. They say nothing about whether the planner's choice of window
  or limits is the best one.
